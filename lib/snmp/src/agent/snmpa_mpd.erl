%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(snmpa_mpd).

-export([init/1, reset/0, inc/1, counters/0, 
	 discarded_pdu/1,
	 process_packet/6,
	 generate_response_msg/5, generate_msg/5, 
	 generate_req_id/0]).

-define(SNMP_USE_V3, true).
-include("snmp_types.hrl").
-include("SNMP-MPD-MIB.hrl").
-include("SNMPv2-TM.hrl").

-define(VMODULE,"MPD").
-include("snmp_verbosity.hrl").

-define(empty_msg_size, 24).

-record(state, {v1 = false, v2c = false, v3 = false}).

					
%%%-----------------------------------------------------------------
%%% This module implemets the Message Processing and Dispatch part of
%%% the multi-lingual SNMP agent.
%%%
%%% The MPD is responsible for:
%%%   *) call the security module (auth/priv).
%%%   *) decoding the message into a PDU.
%%%   *) decide a suitable Access Control Model, and provide it with
%%%      the data it needs.
%%%   *) maintaining SNMP counters.
%%%
%%% In order to take care of the different versions of counters, it
%%% implements and maintains the union of all SNMP counters (i.e. from
%%% rfc1213 and from rfc1907).  It is up to the administrator of the
%%% agent to load the correct MIB.  Note that this module implements
%%% the counters only, it does not provide instrumentation functions
%%% for the counters.
%%%
%%% With the terms defined in rfc2271, this module implememts part
%%% of the Dispatcher and the Message Processing functionality.
%%%-----------------------------------------------------------------
init(Vsns) ->
    ?vlog("init -> entry with"
	"~n   Vsns: ~p", [Vsns]),
    {A,B,C} = erlang:now(),
    random:seed(A,B,C),
    ets:insert(snmp_agent_table, {msg_id, random:uniform(2147483647)}),
    ets:insert(snmp_agent_table, {req_id, random:uniform(2147483647)}),
    init_counters(),
    init_versions(Vsns, #state{}).


reset() ->
    reset_counters(),
    ok.


%%-----------------------------------------------------------------
%% Purpose: We must calculate the length of a
%%          message with an empty Pdu, and zero-length community
%%          string.  This length is used to calculate the max
%%          pdu size allowed for each request. This size is 
%%          dependent on two dynamic fields, the community string
%%          and the pdu (varbinds actually). It is calculated
%%          as EmptySize + length(CommunityString) + 4.
%%          We assume that the length of the CommunityString is
%%          less than 128 (thus requiring just one octet for the
%%          length field (the same as the zero-length community
%%          string)). 4 comes from the fact that the maximum pdu
%%          size needs 31 bits which needs 5 * 7 bits to be
%%          expressed. One 7bit octet is already present in the
%%          empty msg, leaving 4 more 7bit octets.
%% Actually, this function is not used, we use a constant instead.
%%-----------------------------------------------------------------
%% Ret: 24
%empty_msg() ->
%    M = #message{version = 'version-1', community = "", data = 
%		 #pdu{type = 'get-response', request_id = 1,
%		      error_status = noError, error_index = 0, varbinds = []}},
%    length(snmp_pdus:enc_message(M)) + 4.

%%-----------------------------------------------------------------
%% Func: process_packet(Packet, TDomain, TAddress, State, Log) ->
%%       {ok, SnmpVsn, Pdu, PduMS, ACMData} | {discarded, Reason}
%% Types: Packet = binary()
%%        TDomain = snmpUDPDomain | atom()
%%        TAddress = {Ip, Udp}
%%        State = #state
%% Purpose: This is the main Message Dispatching function. (see
%%          section 4.2.1 in rfc2272)
%%-----------------------------------------------------------------
process_packet(Packet, TDomain, TAddress, State, NoteStore, Log) ->
    inc(snmpInPkts),
    case catch snmp_pdus:dec_message_only(binary_to_list(Packet)) of
	#message{version = 'version-1', vsn_hdr = Community, data = Data} 
	  when State#state.v1 == true ->
	    ?vlog("~n   v1, community: ~s", [Community]),
	    HS = ?empty_msg_size + length(Community),
	    v1_v2c_proc('version-1', NoteStore, Community, TDomain, TAddress, 
			Data, HS, Log, Packet);
	#message{version = 'version-2', vsn_hdr = Community, data = Data}
	  when State#state.v2c == true ->
	    ?vlog("~n   v2c, community: ~s", [Community]),
	    HS = ?empty_msg_size + length(Community),
	    v1_v2c_proc('version-2', NoteStore, Community, TDomain, TAddress, 
			Data, HS, Log, Packet);
	#message{version = 'version-3', vsn_hdr = V3Hdr, data = Data}
	  when State#state.v3 == true ->
	    ?vlog("~n   v3, msgID: ~p, msgFlags: ~p, msgSecModel: ~p",
		  [V3Hdr#v3_hdr.msgID,V3Hdr#v3_hdr.msgFlags,
		   V3Hdr#v3_hdr.msgSecurityModel]),
	    validate_catch(catch v3_proc(NoteStore, Packet, TDomain, TAddress,
					 V3Hdr, Data, Log));
	{'EXIT', {bad_version, Vsn}} ->
	    ?vtrace("exit: bad version: ~p",[Vsn]),
	    inc(snmpInBadVersions),
	    {discarded, snmpInBadVersions};
	{'EXIT', Reason} ->
	    ?vtrace("exit: ~p",[Reason]),
	    inc(snmpInASNParseErrs),
	    {discarded, Reason};
	UnknownMessage ->
	    ?vtrace("Unknown message: ~n   ~p"
		"~nwhen"
		"~n   State: ~p",[UnknownMessage, State]),
	    inc(snmpInBadVersions),
	    {discarded, snmpInBadVersions}
    end.

discarded_pdu(false) -> ok;
discarded_pdu(Variable) -> inc(Variable).

validate_catch({'EXIT', Error}) -> 
    ?vlog("exit signal reason: ~p",[Error]),
    exit(Error);
validate_catch(Res) -> Res.
    

%%-----------------------------------------------------------------
%% Handles a Community based message (v1 or v2c).
%%-----------------------------------------------------------------
v1_v2c_proc(Vsn, NoteStore, Community, snmpUDPDomain, {Ip, Udp}, 
	    Data, HS, Log, Packet) ->
    TAddress = tuple_to_list(Ip) ++ [Udp div 256, Udp rem 256],
    AgentMS = snmp_framework_mib:get_engine_max_message_size(),
    MgrMS = snmp_community_mib:get_target_addr_ext_mms(?snmpUDPDomain,
						       TAddress),
    PduMS = case MgrMS of
		{ok, MMS} when MMS < AgentMS -> MMS - HS;
		_ -> AgentMS - HS
	    end,
    case (catch snmp_pdus:dec_pdu(Data)) of
	Pdu when record(Pdu, pdu) ->
	    Log(Pdu#pdu.type, Packet),
	    inc_snmp_in_vars(Pdu),
	    #pdu{request_id = ReqId} = Pdu,
	    OkRes = {ok, Vsn, Pdu, PduMS,
		     {community, sec_model(Vsn), Community, TAddress}},
	    %% Make sure that we don't process duplicate SET request
	    %% twice.  We don't know what could happen in that case.
	    %% The mgr does, so he has to generate a new SET request.
	    ?vdebug("PDU type: ~p",[Pdu#pdu.type]),
	    case Pdu#pdu.type of
		'set-request' ->
		    Key = {agent, Ip, ReqId},
		    case snmp_note_store:get_note(NoteStore, Key) of
			undefined -> 
			    %% Set the note _after_ pdu processing. This makes
			    %% duplicated requests be ignored even if pdu
			    %% processing took long time.
			    snmp_note_store:set_note(NoteStore, 
						     100, Key, true),
			    %% Uses ACMData that snmpa_acm knows of.
			    %% snmpUDPDomain is implicit, since that's the only
			    %% one we handle.
			    OkRes;
			true ->
			    {discarded, duplicate_pdu}
		    end;
		_ ->
		    OkRes
	    end;
	{'EXIT', Reason} ->
	    ?vtrace("PDU decode exit: ~p",[Reason]),
	    inc(snmpInASNParseErrs),
	    {discarded, Reason};
	_TrapPdu ->
	    {discarded, trap_pdu}
    end;
v1_v2c_proc(_Vsn, _NoteStore, _Community, snmpUDPDomain, TAddress, 
	    _Data, _HS, _Log, _Packet) ->
    {discarded, {badarg, TAddress}};
v1_v2c_proc(_Vsn, _NoteStore, _Community, TDomain, _TAddress, 
	    _Data, _HS, _Log, _Packet) ->
    {discarded, {badarg, TDomain}}.

sec_model('version-1') -> ?SEC_V1;
sec_model('version-2') -> ?SEC_V2C.

%%-----------------------------------------------------------------
%% Handles a SNMPv3 Message, following the procedures in rfc2272,
%% section 4.2 and 7.2
%%-----------------------------------------------------------------
v3_proc(NoteStore, Packet, _TDomain, _TAddress, V3Hdr, Data, Log) ->
    %% 7.2.3
    #v3_hdr{msgID = MsgID, msgMaxSize = MMS, msgFlags = MsgFlags,
	    msgSecurityModel = MsgSecurityModel,
	    msgSecurityParameters = SecParams, hdr_size = HdrSize} = V3Hdr,
    ?vdebug("version 3 message header:"
	    "~n   msgID                 = ~p"
	    "~n   msgMaxSize            = ~p"
	    "~n   msgFlags              = ~p"
	    "~n   msgSecurityModel      = ~p"
	    "~n   msgSecurityParameters = ~w",
	    [MsgID,MMS,MsgFlags,MsgSecurityModel,SecParams]),
    %% 7.2.4
    SecModule = get_security_module(MsgSecurityModel),
    %% 7.2.5
    SecLevel = check_sec_level(MsgFlags),
    IsReportable = snmp_misc:is_reportable(MsgFlags),
    %% 7.2.6
    ?vtrace("~n   SecModule    = ~p"
	    "~n   SecLevel     = ~p"
	    "~n   IsReportable = ~p",
	    [SecModule,SecLevel,IsReportable]),
    SecRes = (catch SecModule:process_incoming_msg(Packet, Data,
						   SecParams, SecLevel)),
    ?vtrace("message processing result: ~n\t~p",[SecRes]),
    {SecEngineID, SecName, ScopedPDUBytes, SecData} =
	check_sec_module_result(SecRes, V3Hdr, Data, IsReportable, Log),
    ?vtrace("SecEngineID = ~p, SecName = ~p",[SecEngineID,SecName]),
    %% 7.2.7
    #scopedPdu{contextEngineID = ContextEngineID,
	       contextName = ContextName,
	       data = PDU} =
	case catch snmp_pdus:dec_scoped_pdu(ScopedPDUBytes) of
	    ScopedPDU when record(ScopedPDU, scopedPdu) -> 
		ScopedPDU;
	    {'EXIT', Reason} ->
		inc(snmpInASNParseErrs),
		throw({discarded, Reason})
	end,
    %% We'll have to take care of the unlikely case that we receive an
    %% v1 trappdu in a v3 message explicitly...
    if
	record(PDU, trappdu) ->
	    inc(snmpUnknownPDUHandlers),
	    throw({discarded, received_v1_trap});
	true ->
	    ok
    end,
    ?vlog("~n   contextEngineID: \"~s\", context: \"~s\"",
	  [ContextEngineID, ContextName]),
    if
	SecLevel == 3 -> % encrypted message - log decrypted pdu
	    Log(PDU#pdu.type, {V3Hdr, ScopedPDUBytes});
	true -> % otherwise, log binary
	    Log(PDU#pdu.type, Packet)
    end,
    %% Make sure a get_bulk doesn't get too big.
    AgentMS = snmp_framework_mib:get_engine_max_message_size(),
    %% PduMMS is supposed to be the maximum total length of the response
    %% PDU we can send.  From the MMS, we need to subtract everything before
    %% the PDU, i.e. Message and ScopedPDU.
    %%   Message: [48, TotalLen, Vsn, [Tag, LH, Hdr], [Tag, LM, MsgSec], Data]
    %%             1              3   <----------- HdrSize ----------->
    %%   HdrSize = everything up to and including msgSecurityParameters.
    %% ScopedPduData follows.  This is
    %%   [Tag, Len, [Tag, L1, CtxName], [Tag, L2, CtxEID]]
    %%   i.e. 6 + length(CtxName) + length(CtxEID)
    %% 
    %% Total: 1 + TotalLenOctets + 3 + ScopedPduDataLen
    TotMMS = if AgentMS > MMS -> MMS;
		true -> AgentMS
	     end,
    TotalLenOctets = snmp_pdus:get_encoded_length(TotMMS - 1),
    PduMMS = TotMMS - TotalLenOctets - 10 - HdrSize - 
	length(ContextName) - length(ContextEngineID),
    ?vdebug("PDU type: ~p",[PDU#pdu.type]),
    case PDU#pdu.type of
	report ->
	    %% 7.2.11
	    throw({discarded, report});
	'get-response' -> %% As a result of a sent inform-request?
	    %% 7.2.12
	    case snmp_note_store:get_note(NoteStore, {agent, MsgID}) of
		{SecEngineID, MsgSecurityModel, SecName, SecLevel,
		 ContextEngineID, ContextName} ->
		    {ok, 'version-3', PDU, PduMMS, undefined};
		_ ->
		    inc(snmpUnknownPDUHandlers),
		    throw({discarded, {no_outstanding_req, MsgID}})
	    end;
	'snmpv2-trap' ->
	    inc(snmpUnknownPDUHandlers),
	    throw({discarded, received_v2_trap});
	Type ->
	    %% 7.2.13
	    SnmpEngineID = snmp_framework_mib:get_engine_id(),
	    ?vtrace("SnmpEngineID = ~p",[SnmpEngineID]),
	    case SecEngineID of
		SnmpEngineID ->
		    %% 4.2.2.1.1 - we don't handle proxys yet => we only 
		    %% handle ContextEngineID to ourselves
		    case ContextEngineID of
			SnmpEngineID ->
			    %% Uses ACMData that snmpa_acm knows of.
			    {ok, 'version-3', PDU, PduMMS, 
			     {v3, MsgID, MsgSecurityModel, SecName, SecLevel,
			      ContextEngineID, ContextName, SecData}};
			_ ->
			    %% 4.2.2.1.2
			    NIsReportable = snmp_misc:is_reportable_pdu(Type),
			    Val = inc(snmpUnknownPDUHandlers),
			    ErrorInfo = {#varbind{oid = ?snmpUnknownPDUHandlers,
						  variabletype = 'Counter32',
						  value = Val},
					 SecName,
					 [{securityLevel, SecLevel},
					  {contextEngineID, ContextEngineID},
					  {contextName, ContextName}]},
			    case generate_v3_report_msg(MsgID, MsgSecurityModel,
							Data, ErrorInfo,Log) of
				{ok, Report} when NIsReportable == true ->
				    {discarded, snmpUnknownPDUHandlers, Report};
				_ ->
				    {discarded, snmpUnknownPDUHandlers}
			    end
		    end;
		_ ->
		    {discarded, {badSecurityEngineID, SecEngineID}}
	    end
    end.


get_security_module(?SEC_USM) ->
    snmpa_usm;
get_security_module(_) ->
    inc(snmpUnknownSecurityModels),
    throw({discarded, snmpUnknownSecurityModels}).
    
check_sec_level([MsgFlag]) ->
    SecLevel = MsgFlag band 3,
    if 
	SecLevel == 2 -> 
	    inc(snmpInvalidMsgs),
	    throw({discarded, snmpInvalidMsgs});
	true ->
	    SecLevel
    end;
check_sec_level(Unknown) ->
    ?vlog("invalid msgFlags: ~p",[Unknown]), 
    inc(snmpInvalidMsgs),
    throw({discarded, snmpInvalidMsgs}).

check_sec_module_result(Res, V3Hdr, Data, IsReportable, Log) ->
    case Res of
	{ok, X} -> 
	    X;
	{error, Reason, []} ->         % case 7.2.6 b
	    ?vdebug("security module result [7.2.6-b]:"
		    "~n   Reason: ~p", [Reason]),
	    throw({discarded, {securityError, Reason}});
	{error, Reason, ErrorInfo} when IsReportable == true -> % case 7.2.6 a
	    ?vdebug("security module result when reportable [7.2.6-a]:"
		    "~n   Reason:    ~p"
		    "~n   ErrorInfo: ~p", [Reason, ErrorInfo]),
	    #v3_hdr{msgID = MsgID, msgSecurityModel = MsgSecModel} = V3Hdr,
	    Pdu = get_scoped_pdu(Data),
	    case generate_v3_report_msg(MsgID, MsgSecModel, Pdu,
					ErrorInfo, Log) of
		{ok, Report} ->
		    throw({discarded, {securityError, Reason}, Report});
		{discarded, _SomeOtherReason} ->
		    throw({discarded, {securityError, Reason}})
	    end;
	{error, Reason, ErrorInfo} ->
	    ?vdebug("security module result when not reportable:"
		    "~n   Reason:    ~p"
		    "~n   ErrorInfo: ~p", [Reason, ErrorInfo]),
	    throw({discarded, {securityError, Reason}});
	Else ->
	    ?vdebug("security module result:"
		    "~n   Else: ~p", [Else]),
	    throw({discarded, {securityError, Else}})
    end.

get_scoped_pdu(D) when list(D) ->
    (catch snmp_pdus:dec_scoped_pdu(D));
get_scoped_pdu(D) ->
    D.


%%-----------------------------------------------------------------
%% Executed when a response or report message is generated.
%%-----------------------------------------------------------------
generate_response_msg(Vsn, RePdu, Type, ACMData, Log) ->
    generate_response_msg(Vsn, RePdu, Type, ACMData, Log, 1).

generate_response_msg(Vsn, RePdu, Type, 
		      {community, _SecModel, Community, _IpUdp},
		      Log, _) ->
	case catch snmp_pdus:enc_pdu(RePdu) of
	    {'EXIT', Reason} ->
		user_err("failed encoding pdu: "
			 "(pdu: ~w, community: ~w): ~n~w",
			 [RePdu, Community, Reason]),
		{discarded, Reason};
	    PduBytes ->
		Message = #message{version = Vsn, vsn_hdr = Community, 
				   data = PduBytes},
		case catch list_to_binary(
			     snmp_pdus:enc_message_only(Message)) of
		    {'EXIT', Reason} ->
			user_err("failed encoding message only "
				 "(pdu: ~w, community: ~w): ~n~w",
				 [RePdu, Community, Reason]),
			{discarded, Reason};
		    Packet ->
			MMS = snmp_framework_mib:get_engine_max_message_size(),
			case size(Packet) of
			    Len when Len =< MMS ->
				Log(Type, Packet),
				inc_snmp_cnt_vars(Type, RePdu),
				inc_snmp_out_vars(RePdu),
				{ok, Packet};
			    Len ->
				?vlog("pdu to big:"
				      "~n   Max message size:     ~p"
				      "~n   Encoded message size: ~p",
				      [MMS,Len]),
				too_big(Vsn, RePdu, Community, Log, MMS, Len)
			end
		end
	end;
generate_response_msg(Vsn, RePdu, Type, 
		      {v3, MsgID, MsgSecurityModel, SecName, SecLevel,
		       ContextEngineID, ContextName, SecData},
		      Log, N) ->
    %% rfc2272: 7.1 steps 6-8
    ScopedPDU = #scopedPdu{contextEngineID = ContextEngineID,
			   contextName = ContextName,
			   data = RePdu},
    case catch snmp_pdus:enc_scoped_pdu(ScopedPDU) of
	{'EXIT', Reason} ->
	    user_err("failed encoded scoped pdu "
		     "(pdu: ~w, contextName: ~w): ~n~w",
		     [RePdu, ContextName, Reason]),
	    {discarded, Reason};
	ScopedPDUBytes -> 
	    AgentMS = snmp_framework_mib:get_engine_max_message_size(),
	    V3Hdr = #v3_hdr{msgID = MsgID,
			    msgMaxSize = AgentMS,
			    msgFlags = snmp_misc:mk_msg_flags(Type, 
							      SecLevel),
			    msgSecurityModel = MsgSecurityModel},
	    Message = #message{version = Vsn, vsn_hdr = V3Hdr, 
			       data = ScopedPDUBytes},
	    %% We know that the security model is valid when we
	    %% generate a response.
	    SecModule = 
		case MsgSecurityModel of
		    ?SEC_USM ->
			snmpa_usm
		end,
	    SecEngineID = snmp_framework_mib:get_engine_id(),
	    ?vtrace("generate_response_msg -> SecEngineID: ~p", [SecEngineID]),
	    case (catch SecModule:generate_outgoing_msg(Message, 
							SecEngineID,
							SecName, SecData, 
							SecLevel)) of
		{'EXIT', Reason} ->
		    config_err("~p (message: ~p)", [Reason, Message]),
		    {discarded, Reason};
		{error, Reason} ->
		    config_err("~p (message: ~p)", [Reason, Message]),
		    {discarded, Reason};
		OutMsg when list(OutMsg) ->
		    %% Check the packet size.  Send the msg even
		    %% if it's larger than the mgr can handle - it
		    %% will be dropped.  Just check against the
		    %% internal size.  For GET-BULk responses: we
		    %% *know* that we're within the right limits,
		    %% because of the calculation we do when we
		    %% receive the bulk-request.
		    Packet = list_to_binary(OutMsg),
		    case size(Packet) of
			Len when Len =< AgentMS ->
			    if
				SecLevel == 3 -> 
				    %% encrypted - log decrypted pdu
				    Log(Type, {V3Hdr, ScopedPDUBytes});
				true -> 
				    %% otherwise log the entire msg
				    Log(Type, Packet)
			    end,
			    inc_snmp_cnt_vars(Type, RePdu),
			    inc_snmp_out_vars(RePdu),
			    {ok, Packet};
			Len when N == 2 ->
			    ?vlog("packet max size exceeded: "
				  "~n   Max: ~p"
				  "~n   Len: ~p",
				  [AgentMS,Len]),
			    inc(snmpSilentDrops),
			    {discarded, tooBig};
			Len ->
			    ?vlog("packet max size exceeded: "
				  "~n   N:   ~p"
				  "~n   Max: ~p"
				  "~n   Len: ~p",
				  [N,AgentMS,Len]),
			    TooBigPdu = RePdu#pdu{error_status = tooBig,
						  error_index = 0, 
						  varbinds = []},
			    generate_response_msg(Vsn, TooBigPdu, Type, 
						  {v3, MsgID, 
						   MsgSecurityModel,
						   SecName, SecLevel,
						   ContextEngineID, 
						   ContextName,
						   SecData}, Log, N+1)
		    end
	    end
    end.

generate_v3_report_msg(MsgID, MsgSecurityModel, Data, ErrorInfo, Log) ->
    {Varbind, SecName, Opts} = ErrorInfo,
    ReqId =
	if record(Data, scopedPdu) -> 
		(Data#scopedPdu.data)#pdu.request_id;
	   true -> 
		0 %% RFC2572, 7.1.3.c.4
	end,
    ?vtrace("Report ReqId: ~p",[ReqId]),
    Pdu = #pdu{type = report, request_id = ReqId,
	       error_status = noError, error_index = 0,
	       varbinds = [Varbind]},
    SecLevel = snmp_misc:get_option(securityLevel, Opts, 0),
    SnmpEngineID = snmp_framework_mib:get_engine_id(),
    ContextEngineID = 
	snmp_misc:get_option(contextEngineID, Opts, SnmpEngineID),
    ContextName = snmp_misc:get_option(contextName, Opts, ""),
    SecData = snmp_misc:get_option(sec_data, Opts, []),

    generate_response_msg('version-3', Pdu, report,
			  {v3, MsgID, MsgSecurityModel, SecName, SecLevel,
			   ContextEngineID, ContextName, SecData}, Log).


too_big(Vsn, Pdu, Community, Log, _MMS, _Len) 
  when Pdu#pdu.type == 'get-response' ->
    ErrPdu =
	if 
	    Vsn == 'version-1' ->
		%% In v1, the varbinds should be identical to the incoming
		%% request.  It isn't identical now!
		%% Make acceptable (?) approximation.
		V = set_vb_null(Pdu#pdu.varbinds),
		Pdu#pdu{error_status = tooBig, error_index = 0, varbinds = V};
	    true ->
		%% In v2, varbinds should be empty (reasonable!)
		Pdu#pdu{error_status = tooBig, error_index = 0, varbinds = []}
	end,

    case catch snmp_pdus:enc_pdu(ErrPdu) of
	{'EXIT', Reason} ->
	    user_err("failed encoding pdu (pdu: ~w, community: ~w): ~n~w", 
		     [ErrPdu, Community, Reason]),
	    {discarded, Reason};
	PduBytes -> 
	    Message = #message{version = Vsn, vsn_hdr = Community, 
			       data = PduBytes},
	    case catch snmp_pdus:enc_message_only(Message) of
		{'EXIT', Reason} ->
		    user_err("failed encoding message only"
			     "(pdu: ~w, community: ~w): ~n~w", 
			     [ErrPdu, Community, Reason]),
		    {discarded, Reason};
		Packet -> 
		    Bin = list_to_binary(Packet),
		    Log(Pdu#pdu.type, Bin),
		    inc_snmp_out_vars(ErrPdu),
		    {ok, Bin}
	    end
    end;
too_big(_Vsn, Pdu, _Community, _Log, MMS, Len) ->
    user_err("encoded pdu, ~p bytes, exceeded "
	     "max message size of ~p bytes. Pdu: ~n~w", 
	     [Len, MMS, Pdu]),
    {discarded, tooBig}.

set_vb_null([Vb | Vbs]) ->
    [Vb#varbind{variabletype = 'NULL', value = 'NULL'} | set_vb_null(Vbs)];
set_vb_null([]) ->
    [].

%%-----------------------------------------------------------------
%% Executed when a message that isn't a response is generated, i.e.
%% a trap or an inform.
%%-----------------------------------------------------------------
generate_msg(Vsn, _NoteStore, Pdu, {community, Community}, To) ->
    Message = #message{version = Vsn, vsn_hdr = Community, data = Pdu},
    case catch list_to_binary(snmp_pdus:enc_message(Message)) of
	{'EXIT', Reason} ->
	    user_err("failed encoding message "
		     "(pdu: ~w, community: ~w): ~n~w",
		     [Pdu, Community, Reason]),
	    {discarded, Reason};
	Packet ->
	    AgentMax = snmp_framework_mib:get_engine_max_message_size(),
	    case size(Packet) of
		Len when Len =< AgentMax ->
		    {ok, mk_v1_v2_packet_list(To, Packet, Len, Pdu)};
		Len ->
		    ?vlog("packet max size exceeded: "
			  "~n   Max: ~p"
			  "~n   Len: ~p",
			  [AgentMax,Len]),
		    {discarded, tooBig}
	    end
    end;
generate_msg('version-3', NoteStore, Pdu, 
	     {v3, ContextEngineID, ContextName}, To) ->
    %% rfc2272: 7.1.6
    ScopedPDU = #scopedPdu{contextEngineID = ContextEngineID,
			   contextName = ContextName,
			   data = Pdu},
    case (catch snmp_pdus:enc_scoped_pdu(ScopedPDU)) of
	{'EXIT', Reason} ->
	    user_err("failed encoding scoped pdu "
		     "(pdu: ~w, contextName: ~w): ~n~w",
		     [Pdu, ContextName, Reason]),
	    {discarded, Reason};
	ScopedPDUBytes -> 
	    {ok, mk_v3_packet_list(NoteStore, To, ScopedPDUBytes, Pdu, 
			   ContextEngineID, ContextName)}
    end.

mk_v1_v2_packet_list([{?snmpUDPDomain, [A,B,C,D,U1,U2]} | T],
		     Packet, Len, Pdu) ->
    %% Sending from default UDP port
    inc_snmp_out_vars(Pdu),
    [{snmpUDPDomain, {{A,B,C,D}, U1 bsl 8 + U2}, Packet} |
     mk_v1_v2_packet_list(T, Packet, Len, Pdu)];
mk_v1_v2_packet_list([{TDomain, TAddr} | T], Packet, Len, Pdu) ->
    user_err("Bad TDomain/TAddr: ~w/~w", [TDomain, TAddr]),
    mk_v1_v2_packet_list(T, Packet, Len, Pdu);
mk_v1_v2_packet_list([], _Packet, _Len, _Pdu) ->
    [].

mk_v3_packet_list(NoteStore, [{{?snmpUDPDomain, [A,B,C,D,U1,U2]},
			       {SecModel, SecName, SecLevel, TargetAddrName}} | T], 
		  ScopedPDUBytes, Pdu, ContextEngineID, ContextName) ->
    %% 7.1.7
    PduType = Pdu#pdu.type,
    MsgID = generate_msg_id(),
    V3Hdr = #v3_hdr{msgID = MsgID,
		    msgMaxSize =
		       snmp_framework_mib:get_engine_max_message_size(),
		    msgFlags = snmp_misc:mk_msg_flags(PduType, SecLevel),
		    msgSecurityModel = SecModel},
    Message = #message{version = 'version-3', vsn_hdr = V3Hdr,
		       data = ScopedPDUBytes},
    SecModule = 
	case SecModel of
	    ?SEC_USM ->
		snmpa_usm
	end,
    %% 7.1.9a
    ?vtrace("mk_v3_packet_list -> sec engine id - 7.1.9a", []),
    SecEngineID =
	case PduType of
	    'snmpv2-trap' ->
		snmp_framework_mib:get_engine_id();
	    _ ->
		%% This is the implementation dependent target engine id
		%% procedure.
		case snmp_target_mib:get_target_engine_id(TargetAddrName) of
		    {ok, TargetEngineId} ->
			?vtrace("TargetEngineId: ~p", [TargetEngineId]),
			TargetEngineId;
		    undefined ->
			config_err("Can't find engineID for "
				   "snmpTargetAddrName ~p \n",
				   [TargetAddrName]),
			"" % this will trigger error in secmodule
		end
	end,
    ?vdebug("secEngineID: ~p", [SecEngineID]),
    %% 7.1.9b
    case catch SecModule:generate_outgoing_msg(Message, SecEngineID,
					       SecName, [], SecLevel) of
	{'EXIT', Reason} ->
	    config_err("~p (message: ~p)", [Reason, Message]),
	    mk_v3_packet_list(NoteStore, T, ScopedPDUBytes, Pdu, 
			      ContextEngineID, ContextName);
	{error, Reason} ->
	    ?vlog("~n   ~w error ~p\n", [SecModule, Reason]),
	    mk_v3_packet_list(NoteStore, T, ScopedPDUBytes, Pdu, 
			      ContextEngineID, ContextName);
	Packet ->
	    %% 7.1.9c
	    %% Store in cache for 150 sec.
	    CacheVal = {SecEngineID, SecModel, SecName, SecLevel,
			ContextEngineID, ContextName},
	    snmp_note_store:set_note(NoteStore, 1500, {agent,MsgID}, CacheVal),
	    inc_snmp_out_vars(Pdu),
	    [{snmpUDPDomain, {{A,B,C,D}, U1 bsl 8 + U2}, Packet} |
	     mk_v3_packet_list(NoteStore, T, ScopedPDUBytes, Pdu,
			       ContextEngineID, ContextName)]
    end;
mk_v3_packet_list(NoteStore, [{{TDomain, TAddr}, _} | T], 
		  ScopedPDUBytes, Pdu, ContextEngineID, ContextName) ->
    user_err("Bad TDomain/TAddr: ~w/~w", [TDomain, TAddr]),
    mk_v3_packet_list(NoteStore, T, ScopedPDUBytes, Pdu, ContextEngineID, 
		      ContextName);
mk_v3_packet_list(_, [], _ScopedPDUBytes, _Pdu, _ContextEngineID, 
		  _ContextName) ->
    [].

generate_msg_id() ->
    gen(msg_id).

generate_req_id() ->
    gen(req_id).

gen(Id) ->
    case ets:update_counter(snmp_agent_table, Id, 1) of
	N when N =< 2147483647 ->
	    N;
	_N ->
	    ets:insert(snmp_agent_table, {Id, 0}),
	    0
    end.


%%-----------------------------------------------------------------
%% Version(s) functions
%%-----------------------------------------------------------------
init_versions([], S) ->
    S;
init_versions([v1|Vsns], S) ->
    init_versions(Vsns, S#state{v1 = true});
init_versions([v2|Vsns], S) ->
    init_versions(Vsns, S#state{v2c = true});
init_versions([v3|Vsns], S) ->
    init_versions(Vsns, S#state{v3 = true}).


%%-----------------------------------------------------------------
%% Counter functions
%%-----------------------------------------------------------------
init_counters() -> 
    F = fun(Counter) -> maybe_create_counter(Counter) end,
    lists:map(F, counters()).

reset_counters() -> 
    F = fun(Counter) -> init_counter(Counter) end,
    lists:map(F, counters()).

maybe_create_counter(Counter) ->
    case ets:lookup(snmp_agent_table, Counter) of
	[_] -> ok;
	_ -> init_counter(Counter)
    end.

init_counter(Counter) -> 
    ets:insert(snmp_agent_table, {Counter, 0}).

counters() ->
    [snmpInPkts,
     snmpOutPkts,
     snmpInBadVersions,
     snmpInBadCommunityNames,
     snmpInBadCommunityUses,
     snmpInASNParseErrs,
     snmpInTooBigs,
     snmpInNoSuchNames,
     snmpInBadValues,
     snmpInReadOnlys,
     snmpInGenErrs,
     snmpInTotalReqVars,
     snmpInTotalSetVars,
     snmpInGetRequests,
     snmpInGetNexts,
     snmpInSetRequests,
     snmpInGetResponses,
     snmpInTraps,
     snmpOutTooBigs,
     snmpOutNoSuchNames,
     snmpOutBadValues,
     snmpOutGenErrs,
     snmpOutGetRequests,
     snmpOutGetNexts,
     snmpOutSetRequests,
     snmpOutGetResponses,
     snmpOutTraps,
     snmpSilentDrops,
     snmpProxyDrops,
     %% From SNMP-MPD-MIB
     snmpUnknownSecurityModels,
     snmpInvalidMsgs,
     snmpUnknownPDUHandlers
    ].
    
%%-----------------------------------------------------------------
%%  inc(VariableName) increments the variable (Counter) in
%%  the local mib. (e.g. snmpInPkts)
%%-----------------------------------------------------------------
inc(Name)    -> ets:update_counter(snmp_agent_table, Name, 1).
inc(Name, N) -> ets:update_counter(snmp_agent_table, Name, N).

inc_snmp_in_vars(#pdu{type = Type}) ->
    inc_in_type(Type).

inc_snmp_cnt_vars(_, #pdu{error_status = ErrStat}) when ErrStat /= noError ->
    ok;
inc_snmp_cnt_vars('get-request', #pdu{varbinds = Vbs}) ->
    inc(snmpInTotalReqVars, length(Vbs));
inc_snmp_cnt_vars('get-next-request', #pdu{varbinds = Vbs}) ->
    inc(snmpInTotalReqVars, length(Vbs));
inc_snmp_cnt_vars('set-request', #pdu{varbinds = Vbs}) ->
    inc(snmpInTotalSetVars, length(Vbs));
inc_snmp_cnt_vars(_, _) ->
    ok.

inc_snmp_out_vars(#pdu{type         = Type, 
		       error_status = ErrorStatus}) ->
    inc(snmpOutPkts),
    inc_out_err(ErrorStatus),
    inc_out_vars_2(Type);
inc_snmp_out_vars(TrapPdu) when record(TrapPdu, trappdu) ->
    inc(snmpOutPkts),
    inc(snmpOutTraps).

inc_out_vars_2('get-response')     -> inc(snmpOutGetResponses);
inc_out_vars_2('get-request')      -> inc(snmpOutGetRequests);
inc_out_vars_2('get-next-request') -> inc(snmpOutGetNexts);
inc_out_vars_2('set-request')      -> inc(snmpOutSetRequests);
inc_out_vars_2(_)                  -> ok.

inc_out_err(genErr)     -> inc(snmpOutGenErrs);
inc_out_err(tooBig)     -> inc(snmpOutTooBigs);
inc_out_err(noSuchName) -> inc(snmpOutNoSuchNames);
inc_out_err(badValue)   -> inc(snmpOutBadValues);
% snmpOutReadOnlys is not used any more (rfc1213)
%inc_out_err(readOnly) -> inc(snmpOutReadOnlys);
inc_out_err(_)          -> ok.

inc_in_type('get-request')      -> inc(snmpInGetRequests);
inc_in_type('get-next-request') -> inc(snmpInGetNexts);
inc_in_type('set-request')      -> inc(snmpInSetRequests);
inc_in_type(_)                  -> ok.


user_err(F, A) ->
    snmpa_error:user_err(F, A).

config_err(F, A) ->
    snmpa_error:config_err(F, A).
