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
-module(snmpa_usm).

-export([process_incoming_msg/4, generate_outgoing_msg/5]).

-define(SNMP_USE_V3, true).
-include("snmp_types.hrl").
-include("SNMP-USER-BASED-SM-MIB.hrl").
-include("SNMPv2-TC.hrl").

-define(VMODULE,"A-USM").
-include("snmp_verbosity.hrl").


%%-----------------------------------------------------------------
%% This module implements the User Based Security Model for SNMP,
%% as defined in rfc2274.
%%-----------------------------------------------------------------

%% Columns not accessible via SNMP
-define(usmUserAuthKey, 14).
-define(usmUserPrivKey, 15).

-define(i32(Int), (Int bsr 24) band 255, (Int bsr 16) band 255, (Int bsr 8) band 255, Int band 255).


%%-----------------------------------------------------------------
%% Func: process_incoming_msg(Packet, Data, SecParams, SecLevel) ->
%%       {ok, {SecEngineID, SecName, ScopedPDUBytes, SecData}} |
%%       {error, Reason} | {error, Reason, ErrorInfo}
%%       Return value may be throwed.
%% Types: Reason -> term()
%% Purpose: 
%%-----------------------------------------------------------------
process_incoming_msg(Packet, Data, SecParams, SecLevel) ->
    %% 3.2.1
    ?vtrace("process_incoming_msg -> check security parms: 3.2.1",[]),
    UsmSecParams =
	case catch snmp_pdus:dec_usm_security_parameters(SecParams) of
	    {'EXIT', Reason} ->
		inc(snmpInASNParseErrs),
		error({parseError, Reason}, []);
	    Res ->
		Res
	end,
    #usmSecurityParameters{msgAuthoritativeEngineID = MsgAuthEngineID,
			   msgUserName = MsgUserName} = UsmSecParams,
    ?vlog("~n   authEngineID: \"~s\", userName: \"~s\"",
	  [MsgAuthEngineID, MsgUserName]),
    %% 3.2.3
    ?vtrace("process_incoming_msg -> check engine id: 3.2.3",[]),
    case snmp_user_based_sm_mib:is_engine_id_known(MsgAuthEngineID) of
	true ->
	    ok;
	false ->
	    SecData1 = [MsgUserName],
	    error(usmStatsUnknownEngineIDs, 
		  ?usmStatsUnknownEngineIDs_instance, %% OTP-3542
		  undefined, [{sec_data, SecData1}])
    end,
    %% 3.2.4
    ?vtrace("process_incoming_msg -> retrieve usm user: 3.2.4",[]),
    UsmUser =
	case snmp_user_based_sm_mib:get_user(MsgAuthEngineID, MsgUserName) of
	    User when element(?usmUserStatus, User) == ?'RowStatus_active' ->
		User;
	    {_, Name,_,_,_,_,_,_,_,_,_,_,_, RowStatus,_,_} ->
		?vdebug("process_incoming_msg -> "
			"found user ~p with wrong row status: ~p", 
			[Name, RowStatus]),
		SecData2 = [MsgUserName],
		error(usmStatsUnknownUserNames, 
		      ?usmStatsUnknownUserNames_instance, %% OTP-3542
		      undefined, [{sec_data, SecData2}]);
	    _ -> % undefined or not active user
		SecData2 = [MsgUserName],
		error(usmStatsUnknownUserNames, 
		      ?usmStatsUnknownUserNames_instance, %% OTP-3542
		      undefined, [{sec_data, SecData2}])
	end,
    SecName = element(?usmUserSecurityName, UsmUser),
    ?vtrace("process_incoming_msg -> securityName: ~p",[SecName]),
    %% 3.2.5 - implicit in following checks
    %% 3.2.6 - 3.2.7
    ?vtrace("process_incoming_msg -> authenticate incoming: 3.2.5 - 3.2.7"
	    "~n   ~p",[UsmUser]),
    authenticate_incoming(Packet, UsmSecParams, UsmUser, SecLevel),
    %% 3.2.8
    ?vtrace("process_incoming_msg -> decrypt scoped data: 3.2.8",[]),
    ScopedPDUBytes = decrypt(Data, UsmUser, UsmSecParams, SecLevel),
    %% 3.2.9
    %% Means that if AuthKey/PrivKey are changed; the old values
    %% will be used.
    ?vtrace("process_incoming_msg -> "
	    "AuthKey/PrivKey are changed - use old values: 3.2.9",[]),
    CachedSecData = {MsgUserName,
		     element(?usmUserAuthProtocol, UsmUser),
		     element(?usmUserPrivProtocol, UsmUser),
		     element(?usmUserAuthKey, UsmUser),
		     element(?usmUserPrivKey, UsmUser)},
    {ok, {MsgAuthEngineID, SecName, ScopedPDUBytes, CachedSecData}}.
    

authenticate_incoming(Packet, UsmSecParams, UsmUser, SecLevel) ->
    %% 3.2.6
    ?vtrace("authenticate_incoming -> 3.2.6",[]),
    AuthProtocol = element(?usmUserAuthProtocol, UsmUser),
    #usmSecurityParameters{msgAuthoritativeEngineID    = MsgAuthEngineID,
			   msgAuthoritativeEngineBoots = MsgAuthEngineBoots,
			   msgAuthoritativeEngineTime  = MsgAuthEngineTime,
			   msgAuthenticationParameters = MsgAuthParams} =
	UsmSecParams,
    case snmp_misc:is_auth(SecLevel) of
	true ->
	    SecName = element(?usmUserSecurityName, UsmUser),
	    case is_auth(AuthProtocol,
			 element(?usmUserAuthKey, UsmUser),
			 MsgAuthParams,
			 Packet,
			 SecName,
			 MsgAuthEngineID,
			 MsgAuthEngineBoots, 
			 MsgAuthEngineTime) of
		true -> ok;
		false -> error(usmStatsWrongDigests,
			       ?usmStatsWrongDigests, SecName)
	    end;
	false ->  % noAuth
	    ok
    end.
	    
is_auth(?usmNoAuthProtocol, _, _, _, SecName, _, _, _) -> % 3.2.5
    error(usmStatsUnsupportedSecLevels,
	  ?usmStatsUnsupportedSecLevels, SecName);
is_auth(AuthProtocol, AuthKey, AuthParams, Packet, SecName,
	MsgAuthEngineID, MsgAuthEngineBoots, MsgAuthEngineTime) ->
    IsAuth = auth_in(AuthProtocol, AuthKey, AuthParams, Packet),
    case IsAuth of
	true ->
	    %% 3.2.7
	    ?vtrace("is_auth -> "
		    "retrieve EngineBoots and EngineTime: 3.2.7",[]),
	    SnmpEngineID = snmp_framework_mib:get_engine_id(),
	    ?vtrace("is_auth -> SnmpEngineID: ~p",[SnmpEngineID]),
	    case MsgAuthEngineID of
		SnmpEngineID -> %% 3.2.7a
		    ?vtrace("is_auth -> we are authoritative: 3.2.7a",[]),
		    SnmpEngineBoots = snmp_framework_mib:get_engine_boots(),
		    ?vtrace("is_auth -> SnmpEngineBoots: ~p",
			    [SnmpEngineBoots]),
		    SnmpEngineTime = snmp_framework_mib:get_engine_time(),
		    InTimeWindow =
			if
			    SnmpEngineBoots == 2147483647 -> false;
			    MsgAuthEngineBoots /= SnmpEngineBoots -> false;
			    MsgAuthEngineTime + 150 < SnmpEngineTime -> false;
			    MsgAuthEngineTime - 150 > SnmpEngineTime -> false;
			    true -> true
			end,
		    case InTimeWindow of
			true -> 
			    true;
			false -> 
			    %% OTP-4090 (OTP-3542)
			    ?vinfo("NOT in time window: "
				   "~n   SecName:            ~p"
				   "~n   SnmpEngineBoots:    ~p"
				   "~n   MsgAuthEngineBoots: ~p"
				   "~n   SnmpEngineTime:     ~p"
				   "~n   MsgAuthEngineTime:  ~p",
				   [SecName,
				    SnmpEngineBoots, MsgAuthEngineBoots,
				    SnmpEngineTime, MsgAuthEngineTime]),
			    error(usmStatsNotInTimeWindows,
				  ?usmStatsNotInTimeWindows_instance,
				  SecName,
				  [{securityLevel, 1}]) % authNoPriv
		    end;
		_ -> %% 3.2.7b - we're non-authoritative
		    ?vtrace("is_auth -> we are non-authoritative: 3.2.7b",[]),
		    SnmpEngineBoots = get_engine_boots(MsgAuthEngineID),
		    ?vtrace("is_auth -> SnmpEngineBoots: ~p",
			    [SnmpEngineBoots]),
		    SnmpEngineTime = get_engine_time(MsgAuthEngineID),
		    LatestRecvTime = get_engine_latest_time(MsgAuthEngineID),
		    UpdateLCD =
			if
			    MsgAuthEngineBoots > SnmpEngineBoots -> true;
			    MsgAuthEngineBoots == SnmpEngineBoots,
			    MsgAuthEngineTime > LatestRecvTime -> true;
			    true -> false
			end,
		    case UpdateLCD of
			true -> %% 3.2.7b1
			    ?vtrace("is_auth -> "
				    "update msgAuthoritativeEngineID: 3.2.7b1",
				    []),
			    set_engine_boots(MsgAuthEngineID,
					     MsgAuthEngineBoots),
			    set_engine_time(MsgAuthEngineID,
					    MsgAuthEngineTime),
			    set_engine_latest_time(MsgAuthEngineID,
						   MsgAuthEngineTime);
			false ->
			    ok
		    end,
		    %% 3.2.7.b2
		    ?vtrace("is_auth -> "
			    "check if message is outside time window: 3.2.7b2",
			    []),
		    InTimeWindow =
			if
			    SnmpEngineBoots == 2147483647 ->
				false;
			    MsgAuthEngineBoots < SnmpEngineBoots ->
				false;
			    MsgAuthEngineBoots == SnmpEngineBoots,
			    MsgAuthEngineTime < (SnmpEngineTime - 150) ->
				false;
			    true -> true
			end,
		    case InTimeWindow of
			false ->
			    ?vinfo("NOT in time window: "
				   "~n   SecName:            ~p"
				   "~n   SnmpEngineBoots:    ~p"
				   "~n   MsgAuthEngineBoots: ~p"
				   "~n   SnmpEngineTime:     ~p"
				   "~n   MsgAuthEngineTime:  ~p",
				   [SecName,
				    SnmpEngineBoots, MsgAuthEngineBoots,
				    SnmpEngineTime, MsgAuthEngineTime]),
			    error(notInTimeWindow, []);
			true ->
			    ok
		    end,
		    true
	    end;
	false -> 
	    false
    end.
				
			    
decrypt(Data, UsmUser, UsmSecParams, SecLevel) ->
    case snmp_misc:is_priv(SecLevel) of
	true ->
	    do_decrypt(Data, UsmUser, UsmSecParams);
	false ->
	    Data
    end.

do_decrypt(Data, UsmUser, 
	   #usmSecurityParameters{msgPrivacyParameters = PrivParms}) ->
	    EncryptedPDU = snmp_pdus:dec_scoped_pdu_data(Data),
	    SecName      = element(?usmUserSecurityName, UsmUser),
	    PrivP        = element(?usmUserPrivProtocol, UsmUser),
	    PrivKey      = element(?usmUserPrivKey,      UsmUser), 
    try_decrypt(PrivP, PrivKey, PrivParms, EncryptedPDU, SecName).

try_decrypt(?usmNoPrivProtocol, _, _, _, SecName) -> % 3.2.5
    error(usmStatsUnsupportedSecLevels, 
	  ?usmStatsUnsupportedSecLevels, SecName);
try_decrypt(?usmDESPrivProtocol, 
	    PrivKey, MsgPrivParams, EncryptedPDU, SecName) ->
    case (catch des_decrypt(PrivKey, MsgPrivParams, EncryptedPDU)) of
	{ok, DecryptedData} ->
	    DecryptedData;
	_ ->
	    error(usmStatsDecryptionErrors, 
		  ?usmStatsDecryptionErrors, SecName)
    end.


generate_outgoing_msg(Message, SecEngineID, SecName, SecData, SecLevel) ->
    %% 3.1.1
    ?vtrace("generate_outgoing_msg -> entry [3.1.1]",[]),
    {UserName, AuthProtocol, PrivProtocol, AuthKey, PrivKey} =
	case SecData of
	    [] -> % 3.1.1b
		%% Not a response - read from LCD
		case snmp_user_based_sm_mib:get_user_from_security_name(
		       SecEngineID, SecName) of
		    User when element(?usmUserStatus, User) ==
			      ?'RowStatus_active' ->
			{element(?usmUserName, User),
			 element(?usmUserAuthProtocol, User),
			 element(?usmUserPrivProtocol, User),
			 element(?usmUserAuthKey, User),
			 element(?usmUserPrivKey, User)};
		    {_, Name,_,_,_,_,_,_,_,_,_,_,_, RowStatus,_,_} ->
			?vdebug("generate_outgoing_msg -> "
				"found user ~p with wrong row status: ~p", 
				[Name, RowStatus]),
			error(unknownSecurityName);
		    _ ->
			error(unknownSecurityName)
		end;
	    [MsgUserName] ->
		%% This means the user at the engine is unknown
		{MsgUserName, ?usmNoAuthProtocol, ?usmNoPrivProtocol, "", ""};
	    _ -> % 3.1.1a
		SecData
	end,
    %% 3.1.4
    ?vtrace("generate_outgoing_msg -> [3.1.4]",[]),
    ScopedPduBytes = Message#message.data,
    {ScopedPduData, MsgPrivParams} =
	encrypt(ScopedPduBytes, PrivProtocol, PrivKey, SecLevel),
    SnmpEngineID = snmp_framework_mib:get_engine_id(),
    ?vtrace("generate_outgoing_msg -> SnmpEngineID: ~p [3.1.6]",
	    [SnmpEngineID]),
    %% 3.1.6
    {MsgAuthEngineBoots, MsgAuthEngineTime} =
	case snmp_misc:is_auth(SecLevel) of
	    false when SecData == [] -> % not a response
		{0, 0}; 
	    true when SecEngineID /= SnmpEngineID ->
		{get_engine_boots(SecEngineID),
		 get_engine_time(SecEngineID)};
	    _ ->
		{snmp_framework_mib:get_engine_boots(),
		 snmp_framework_mib:get_engine_time()}
	end,
    %% 3.1.5 - 3.1.7
    ?vtrace("generate_outgoing_msg -> [3.1.5 - 3.1.7]",[]),
    UsmSecParams =
	#usmSecurityParameters{msgAuthoritativeEngineID = SecEngineID,
			       msgAuthoritativeEngineBoots = MsgAuthEngineBoots,
			       msgAuthoritativeEngineTime = MsgAuthEngineTime,
			       msgUserName = UserName,
			       msgPrivacyParameters = MsgPrivParams},
    Message2 = Message#message{data = ScopedPduData},
    %% 3.1.8
    ?vtrace("generate_outgoing_msg -> [3.1.8]",[]),
    authenticate_outgoing(Message2, UsmSecParams,
			  AuthKey, AuthProtocol, SecLevel).


%% Ret: {ScopedPDU, MsgPrivParams} - both are already encoded as OCTET STRINGs
encrypt(Data, PrivProtocol, PrivKey, SecLevel) ->
    case snmp_misc:is_priv(SecLevel) of
	false -> % 3.1.4b
	    ?vtrace("encrypt -> 3.1.4b",[]),
	    {Data, []};
	true -> % 3.1.4a
	    ?vtrace("encrypt -> 3.1.4a",[]),
	    case try_encrypt(PrivProtocol, PrivKey, Data) of
		{ok, ScopedPduData, MsgPrivParams} ->
		    ?vtrace("encrypt -> encode tag",[]),
		    {snmp_pdus:enc_oct_str_tag(ScopedPduData), MsgPrivParams};
		_ ->
		    error(encryptionError)
	    end
    end.

try_encrypt(?usmNoPrivProtocol, _PrivKey, _Data) -> % 3.1.2
    error(unsupportedSecurityLevel);
try_encrypt(?usmDESPrivProtocol, PrivKey, Data) ->
    des_encrypt(PrivKey, Data).


authenticate_outgoing(Message, UsmSecParams, 
		      AuthKey, AuthProtocol, SecLevel) ->
    Message2 = 
	case snmp_misc:is_auth(SecLevel) of
	    true ->
		auth_out(AuthProtocol, AuthKey, Message, UsmSecParams);
	    false ->
		set_msg_auth_params(Message, UsmSecParams)
	end,
    ?vtrace("authenticate_outgoing -> encode message only",[]),
    snmp_pdus:enc_message_only(Message2).
    
	    

%%-----------------------------------------------------------------
%% Auth and priv algorithms
%%-----------------------------------------------------------------
auth_in(AuthProtocol, AuthKey, AuthParams, Packet) ->
    snmp_usm:auth_in(AuthProtocol, AuthKey, AuthParams, Packet).

auth_out(AuthProtocol, AuthKey, Message, UsmSecParams) ->
    snmp_usm:auth_out(AuthProtocol, AuthKey, Message, UsmSecParams).

set_msg_auth_params(Message, UsmSecParams) ->
    snmp_usm:set_msg_auth_params(Message, UsmSecParams, []).

des_encrypt(PrivKey, Data) ->
    snmp_usm:des_encrypt(PrivKey, Data, fun get_salt/0).

des_decrypt(PrivKey, MsgPrivParams, EncData) ->
    snmp_usm:des_decrypt(PrivKey, MsgPrivParams, EncData).

get_salt() ->
    SaltInt = 
	case catch ets:update_counter(snmp_agent_table, usm_salt_int, 1) of
	    N when N =< 4294967295 ->
		N;
	    _ -> % it doesn't exist, or it's time to wrap
		ets:insert(snmp_agent_table, {usm_salt_int, 0}),
		0
	end,
    EngineBoots = snmp_framework_mib:get_engine_boots(),
    [?i32(EngineBoots), ?i32(SaltInt)].


%%-----------------------------------------------------------------
%% We cache the local values of all non-auth engines we know.
%% Keep the values in the snmp_agent_table.
%% See section 2.3 of the RFC.
%%-----------------------------------------------------------------
get_engine_boots(SnmpEngineID) ->
    case ets:lookup(snmp_agent_table, {usm_eboots, SnmpEngineID}) of
	[{_Key, Boots}] -> Boots;
	_ -> 0
    end.

get_engine_time(SnmpEngineID) ->
    case ets:lookup(snmp_agent_table, {usm_etime, SnmpEngineID}) of
	[{_Key, Diff}] -> snmp_misc:now(sec) - Diff;
	_ -> 0
    end.
	    
get_engine_latest_time(SnmpEngineID) ->
    case ets:lookup(snmp_agent_table, {usm_eltime, SnmpEngineID}) of
	[{_Key, Time}] -> Time;
	_ -> 0
    end.
	    

set_engine_boots(SnmpEngineID, EngineBoots) ->
    ets:insert(snmp_agent_table, {{usm_eboots, SnmpEngineID}, EngineBoots}).

set_engine_time(SnmpEngineID, EngineTime) ->
    Diff = snmp_misc:now(sec) - EngineTime,
    ets:insert(snmp_agent_table, {{usm_etime, SnmpEngineID}, Diff}).

set_engine_latest_time(SnmpEngineID, EngineTime) ->
    ets:insert(snmp_agent_table, {{usm_eltime, SnmpEngineID}, EngineTime}).


%%-----------------------------------------------------------------
%% Utility functions
%%-----------------------------------------------------------------
error(Reason) ->
    throw({error, Reason}).

error(Reason, ErrorInfo) ->
    throw({error, Reason, ErrorInfo}).

error(Variable, Oid, SecName) ->
    error(Variable, Oid, SecName, []).
error(Variable, Oid, SecName, Opts) ->
    Val = inc(Variable),
    ErrorInfo = {#varbind{oid = Oid,
			  variabletype = 'Counter32',
			  value = Val},
		 SecName,
		 Opts},
    throw({error, Variable, ErrorInfo}).

inc(Name) -> ets:update_counter(snmp_agent_table, Name, 1).



