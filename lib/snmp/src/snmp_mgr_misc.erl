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
-module(snmp_mgr_misc).
%% c(snmp_mgr_misc).

%% API
-export([start_link_packet/7, stop/1, send_pdu/2, send_msg/4, error/2,
	 send_bytes/2,
	 get_pdu/1, set_pdu/2, format_hdr/1]).

%% internal exports
-export([init_packet/8]).

-define(SNMP_USE_V3, true).
-include("snmp_types.hrl").
-record(mini_mib_elem,{aliasname, oid, type}).

%%----------------------------------------------------------------------
%% The InHandler process will receive messages on the form {snmp_pdu, Pdu}.
%%----------------------------------------------------------------------
start_link_packet(InHandler, AgentIp, UdpPort, TrapUdp, VsnHdr, Version, Dir) 
  when integer(UdpPort) ->
    proc_lib:start_link(snmp_mgr_misc, init_packet,
		[self(),InHandler,AgentIp,UdpPort,TrapUdp, VsnHdr, Version,
		 Dir]).

stop(Pid) ->
    Pid ! {stop, self()},
    receive
	{Pid, stopped} -> ok
    end.
	

send_pdu(Pdu, PacketPid) when record(Pdu, pdu) ->
    PacketPid ! {send_pdu, Pdu}.

send_msg(Msg, PacketPid, Ip, Udp) when record(Msg, message) ->
    PacketPid ! {send_msg, Msg, Ip, Udp}.

send_bytes(Bytes, PacketPid) ->
    PacketPid ! {send_bytes, Bytes}.

%%--------------------------------------------------
%% The SNMP encode/decode process
%%--------------------------------------------------
init_packet(Parent, SnmpMgr, AgentIp, UdpPort, TrapUdp, VsnHdr, Version, Dir) ->
    {ok, UdpId} = gen_udp:open(TrapUdp, [{reuseaddr, true}]),
    put(msg_id, 1),
    proc_lib:init_ack(Parent, self()),
    init_usm(Version, Dir),
    packet_loop(SnmpMgr, UdpId, AgentIp, UdpPort, VsnHdr, Version, []).

packet_loop(SnmpMgr, UdpId, AgentIp, UdpPort, VsnHdr, Version, MsgData) ->
    receive
	{send_pdu, Pdu} ->
	    case mk_msg(Version, Pdu, VsnHdr, MsgData) of
		error ->
		    ok;
		B when list(B) -> 
		    gen_udp:send(UdpId, AgentIp, UdpPort, B)
	    end,
	    packet_loop(SnmpMgr, UdpId, AgentIp, UdpPort, VsnHdr, Version, []);
	{send_msg, Msg, Ip, Udp} ->
	    case catch snmp_pdus:enc_message(Msg) of
		{'EXIT', Reason} ->
		    error("Encoding error. Msg: ~w. Reason: ~w",[Msg, Reason]);
		B when list(B) -> 
		    gen_udp:send(UdpId, Ip, Udp, B)
	    end,
	    packet_loop(SnmpMgr, UdpId, AgentIp, UdpPort, VsnHdr, Version, []);
	{udp, UdpId, Ip, UdpPort, Bytes} ->
	    MsgData3 =
		case catch snmp_pdus:dec_message_only(Bytes) of
		    Message when Message#message.version == 'version-3' ->
			case catch handle_v3_msg(Bytes, Message) of
			    {ok, NewData, MsgData2} ->
				Msg = Message#message{data = NewData},
				case SnmpMgr of
				    {pdu, Pid} ->
					Pid ! {snmp_pdu, get_pdu(Msg)};
				    {msg, Pid} ->
					Pid ! {snmp_msg, Msg, Ip, UdpPort}
				end,
				MsgData2;
			    {error, Reason, B} ->
				gen_udp:send(UdpId, AgentIp, UdpPort, B),
				error("Decoding error. Auto-sending Report.\n"
				      "Reason: ~w "
				      "(UDPport: ~w, Ip: ~w)",
				      [Reason, UdpPort, Ip]),
				[];
			    {error, Reason} ->
				error("Decoding error. Bytes: ~w ~n Reason: ~w "
				      "(UDPport: ~w, Ip: ~w)",
				      [Bytes, Reason, UdpPort, Ip]),
				[]
			end;
		    Message when record(Message, message) ->
			%% v1 or v2c
			case catch snmp_pdus:dec_pdu(Message#message.data) of
			    Pdu when record(Pdu, pdu) ->
				case SnmpMgr of
				    {pdu, Pid} ->
					Pid ! {snmp_pdu, Pdu};
				    {msg, Pid} ->
					Msg = Message#message{data = Pdu},
					Pid ! {snmp_msg, Msg, Ip, UdpPort}
				end;
			    Pdu when record(Pdu, trappdu) ->
				case SnmpMgr of
				    {pdu, Pid} ->
					Pid ! {snmp_pdu, Pdu};
				    {msg, Pid} ->
					Msg = Message#message{data = Pdu},
					Pid ! {snmp_msg, Msg, Ip, UdpPort}
				end;
			    Reason ->
				error("Decoding error. Bytes: ~w ~n Reason: ~w "
				      "(UDPport: ~w, Ip: ~w)",
				      [Bytes, Reason, UdpPort, Ip])
			end,
			[];
		    Reason ->
			error("Decoding error. Bytes: ~w ~n Reason: ~w "
			      "(UDPport: ~w, Ip: ~w)",
			      [Bytes, Reason, UdpPort, Ip]),
			[]
		end,
	    packet_loop(SnmpMgr, UdpId, AgentIp, UdpPort, VsnHdr, Version,
			MsgData3);
	{send_bytes, B} ->
	    gen_udp:send(UdpId, AgentIp, UdpPort, B),
	    packet_loop(SnmpMgr, UdpId, AgentIp, UdpPort, VsnHdr, Version, []);
	{stop, Pid} ->
	    gen_udp:close(UdpId),
	    Pid ! {self(), stopped},
	    exit(normal);
	Other ->
	    exit({snmp_packet_got_other, Other})
    end.

handle_v3_msg(Packet, #message{vsn_hdr = V3Hdr, data = Data}) ->
    %% Code copied from snmp_mpd.erl
    #v3_hdr{msgID = MsgId, msgFlags = MsgFlags,
	    msgSecurityModel = MsgSecurityModel,
	    msgSecurityParameters = SecParams} = V3Hdr,
    SecModule = get_security_module(MsgSecurityModel),
    SecLevel = hd(MsgFlags) band 3,
    IsReportable = snmp_misc:is_reportable(MsgFlags),
    SecRes = (catch SecModule:process_incoming_msg(list_to_binary(Packet), Data,
						   SecParams, SecLevel)),
    {SecEngineID, SecName, ScopedPDUBytes, SecData} =
	check_sec_module_result(SecRes, V3Hdr, Data, IsReportable),
    case catch snmp_pdus:dec_scoped_pdu(ScopedPDUBytes) of
	ScopedPDU when record(ScopedPDU, scopedPdu) -> 
	    {ok, ScopedPDU, {MsgId, SecName, SecData}};
	{'EXIT', Reason} ->
	    throw({error, Reason})
    end.

get_security_module(?SEC_USM) ->
    snmp_usm;
get_security_module(SecModel) ->
    throw({error, {unknown_sec_model, SecModel}}).

check_sec_module_result(Res, V3Hdr, Data, IsReportable) ->
    case Res of
	{ok, X} -> 
	    X;
	{error, Reason, []} ->
	    throw({error, {securityError, Reason}});
	{error, Reason, ErrorInfo} when IsReportable == true ->
	    #v3_hdr{msgID = MsgID, msgSecurityModel = MsgSecModel} = V3Hdr,
	    case generate_v3_report_msg(MsgID, MsgSecModel, Data, ErrorInfo) of
		error ->
		    throw({error, {securityError, Reason}});
		Packet ->
		    throw({error, {securityError, Reason}, Packet})
	    end;
	{error, Reason, _} ->
	    throw({error, {securityError, Reason}});
	Else ->
	    throw({error, {securityError, Else}})
    end.

generate_v3_report_msg(MsgID, MsgSecurityModel, Data, ErrorInfo) ->
    {Varbind, SecName, Opts} = ErrorInfo,
    ReqId =
	if record(Data, scopedPdu) -> (Data#scopedPdu.data)#pdu.request_id;
	   true -> 0
	end,
    Pdu = #pdu{type = report, request_id = ReqId,
	       error_status = noError, error_index = 0,
	       varbinds = [Varbind]},
    SecLevel = snmp_misc:get_option(securityLevel, Opts, 0),
    SnmpEngineID = snmp_framework_mib:get_engine_id(),
    ContextEngineID = 
	snmp_misc:get_option(contextEngineID, Opts, SnmpEngineID),
    ContextName = snmp_misc:get_option(contextName, Opts, ""),
    mk_msg('version-3', Pdu, {ContextName, SecName, SnmpEngineID, 
			      ContextEngineID, SecLevel},
	   undefined).


error(Format, Data) ->
    io:format("* Error: "),
    ok = io:format(Format, Data),
    io:format("~n").


mk_msg('version-3', Pdu, {Context, User, EngineID, CtxEngineId, SecLevel}, 
       MsgData) ->
    %% Code copied from snmp_mpd.erl
    {MsgId, SecName, SecData} =
	if
	    tuple(MsgData), Pdu#pdu.type == 'get-response' ->
		MsgData;
	    true -> 
		Md = get(msg_id),
		put(msg_id, Md + 1),
		{Md, User, []}
	end,
    ScopedPDU = #scopedPdu{contextEngineID = CtxEngineId,
			   contextName = Context,
			   data = Pdu},
    ScopedPDUBytes = snmp_pdus:enc_scoped_pdu(ScopedPDU),

    PduType = Pdu#pdu.type,
    V3Hdr = #v3_hdr{msgID = MsgId,
		    msgMaxSize = 1000,
		    msgFlags = snmp_misc:mk_msg_flags(PduType, SecLevel),
		    msgSecurityModel = ?SEC_USM},
    Message = #message{version = 'version-3', vsn_hdr = V3Hdr,
		       data = ScopedPDUBytes},
    SecEngineID = case PduType of
		      'get-response' -> snmp_framework_mib:get_engine_id();
		      _ -> EngineID
		  end,
    case catch snmp_usm:generate_outgoing_msg(Message, SecEngineID,
					      SecName, SecData, SecLevel) of
	{'EXIT', Reason} ->
	    error("Encoding error. Pdu: ~w. Reason: ~w",[Pdu, Reason]),
	    error;
	{error, Reason} ->
	    error("Encoding error. Pdu: ~w. Reason: ~w",[Pdu, Reason]),
	    error;
	Packet ->
	    Packet
    end;
mk_msg(Version, Pdu, {Com, _User, _EngineID, _Ctx, _SecLevel}, _SecData) ->
    Msg = #message{version = Version, vsn_hdr = Com, data = Pdu},
    case catch snmp_pdus:enc_message(Msg) of
	{'EXIT', Reason} ->
	    error("Encoding error. Pdu: ~w. Reason: ~w",[Pdu, Reason]),
	    error;
	B when list(B) -> 
	    B
    end.

format_hdr(#message{version = 'version-3', 
		    vsn_hdr = #v3_hdr{msgID = MsgId},
		    data = #scopedPdu{contextName = CName}}) ->
    io_lib:format("v3, ContextName = \"~s\"  Message ID = ~w\n",
		  [CName, MsgId]);
format_hdr(#message{version = Vsn, vsn_hdr = Com, data = X}) ->
    io_lib:format("~w, CommunityName = \"~s\"\n", [vsn(Vsn), Com]).

vsn('version-1') -> v1;
vsn('version-2') -> v2c.


get_pdu(#message{version = 'version-3', data = #scopedPdu{data = Pdu}}) ->
    Pdu;
get_pdu(#message{data = Pdu}) ->
    Pdu.

set_pdu(Msg, RePdu) when Msg#message.version == 'version-3' ->
    SP = (Msg#message.data)#scopedPdu{data = RePdu}, 
    Msg#message{data = SP};
set_pdu(Msg, RePdu) ->
    Msg#message{data = RePdu}.


init_usm('version-3', Dir) ->
    ets:new(snmp_agent_table, [set, public, named_table]),
    snmp_local_db:start_link(Dir, normal),
    snmp_generic:variable_set(snmpEngineID, "mgrEngine"),
    snmp_framework_mib:set_engine_boots(1),
    snmp_framework_mib:set_engine_time(1),
    snmp_user_based_sm_mib:reconfigure(Dir);
init_usm(_Vsn, _Dir) ->
    ok.

