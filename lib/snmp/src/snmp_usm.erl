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
-module(snmp_usm).

-export([process_incoming_msg/4, generate_outgoing_msg/5]).
-export([passwd2localized_key/3, localize_key/3]).

-define(SNMP_USE_V3, true).
-include("snmp_types.hrl").
-include("SNMP-USER-BASED-SM-MIB.hrl").
-include("SNMPv2-TC.hrl").

-define(VMODULE,"USM").
-include("snmp_verbosity.hrl").


%%%-----------------------------------------------------------------
%%% This module implements the User Based Security Model for SNMP,
%%% as defined in rfc2274.
%%%-----------------------------------------------------------------

%% Columns not accessible via SNMP
-define(usmUserAuthKey, 14).
-define(usmUserPrivKey, 15).

-define(twelwe_zeros, [0,0,0,0,0,0,0,0,0,0,0,0]).

-define(i32(Int), (Int bsr 24) band 255, (Int bsr 16) band 255, (Int bsr 8) band 255, Int band 255).


%%-----------------------------------------------------------------
%% Func: passwd2localized_key/3
%% Types: Alg      = md5 | sha
%%        Passwd   = string()
%%        EngineID = string()
%% Purpose: Generates a key that can be used as an authentication
%%          or privacy key using MD5 och SHA.  The key is
%%          localized for EngineID.
%%          The algorithm is described in appendix A.1 2) of
%%          rfc2274.
%%-----------------------------------------------------------------
passwd2localized_key(Alg, Passwd, EngineID) when length(Passwd) > 0 ->
    Key = mk_digest(Alg, Passwd),
    localize_key(Alg, Key, EngineID).
    
%%-----------------------------------------------------------------
%% Func: localize_key/3
%% Types: Alg      = md5 | sha
%%        Passwd   = string()
%%        EngineID = string()
%% Purpose: Localizes an unlocalized key for EngineID.  See rfc2274
%%          section 2.6 for a definition of localized keys.
%%-----------------------------------------------------------------
localize_key(Alg, Key, EngineID) ->
    Str = [Key, EngineID, Key],
    Hash = {crypto, Alg},
    binary_to_list(Hash(Str)).

mk_digest(md5, Passwd) ->
    mk_md5_digest(Passwd);
mk_digest(sha, Passwd) ->
    mk_sha_digest(Passwd).

mk_md5_digest(Passwd) ->
    Ctx = crypto:md5_init(),
    Ctx2 = md5_loop(0, [], Ctx, Passwd, length(Passwd)),
    crypto:md5_final(Ctx2).

md5_loop(Count, Buf, Ctx, Passwd, PasswdLen) when Count < 1048576 ->
    {Buf64, NBuf} = mk_buf64(length(Buf), Buf, Passwd, PasswdLen),
    NCtx = crypto:md5_update(Ctx, Buf64),
    md5_loop(Count+64, NBuf, NCtx, Passwd, PasswdLen);
md5_loop(_Count, _Buf, Ctx, _Passwd, _PasswdLen) ->
    Ctx.

mk_sha_digest(Passwd) ->
    Ctx = crypto:sha_init(),
    Ctx2 = sha_loop(0, [], Ctx, Passwd, length(Passwd)),
    crypto:sha_final(Ctx2).

sha_loop(Count, Buf, Ctx, Passwd, PasswdLen) when Count < 1048576 ->
    {Buf64, NBuf} = mk_buf64(length(Buf), Buf, Passwd, PasswdLen),
    NCtx = crypto:sha_update(Ctx, Buf64),
    sha_loop(Count+64, NBuf, NCtx, Passwd, PasswdLen);
sha_loop(_Count, _Buf, Ctx, _Passwd, _PasswdLen) ->
    Ctx.

%% Create a 64 bytes long string, by repeating Passwd as many times
%% necessary.  Output is the 64 byte string, and the rest of the last
%% repetition of the Passwd.  This is used as input in the next
%% invocation.
mk_buf64(BufLen, Buf, Passwd, PasswdLen) ->
    case BufLen + PasswdLen of
	TotLen when TotLen > 64 ->
	    {[Buf, lists:sublist(Passwd, 64-BufLen)],
	     lists:sublist(Passwd, 65-BufLen, PasswdLen)};
	TotLen ->
	    mk_buf64(TotLen, [Buf, Passwd], Passwd, PasswdLen)
    end.


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
    ?vtrace("check security parms: 3.2.1",[]),
    UsmSecParams =
	case catch snmp_pdus:dec_usm_security_parameters(SecParams) of
	    {'EXIT', Reason} ->
		inc(snmpInASNParseErrs),
		throw({error, {parseError, Reason}, []});
	    Res ->
		Res
	end,
    #usmSecurityParameters{msgAuthoritativeEngineID = MsgAuthEngineID,
			   msgUserName = MsgUserName} = UsmSecParams,
    ?vlog("~n   authEngineID: \"~s\", userName: \"~s\"",
	  [MsgAuthEngineID, MsgUserName]),
    %% 3.2.3
    ?vtrace("check engine id: 3.2.3",[]),
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
    ?vtrace("retrieve usm user: 3.2.4",[]),
    UsmUser =
	case snmp_user_based_sm_mib:get_user(MsgAuthEngineID, MsgUserName) of
	    User when element(?usmUserStatus, User) == ?'RowStatus_active' ->
		User;
	    {_, Name,_,_,_,_,_,_,_,_,_,_,_, RowStatus,_,_} ->
		?vdebug("found user ~p with wrong row status: ~p", 
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
    ?vtrace("securityName: ~p",[SecName]),
    %% 3.2.5 - implicit in following checks
    %% 3.2.6 - 3.2.7
    ?vtrace("authenticate incoming: 3.2.5 - 3.2.7",[]),
    authenticate_incoming(Packet, UsmSecParams, UsmUser, SecLevel),
    %% 3.2.8
    ?vtrace("decrypt scoped data: 3.2.8",[]),
    ScopedPDUBytes = decrypt(Data, UsmUser, UsmSecParams, SecLevel),
    %% 3.2.9
    %% Means that if AuthKey/PrivKey are changed; the old values
    %% will be used.
    CachedSecData = {MsgUserName,
		     element(?usmUserAuthProtocol, UsmUser),
		     element(?usmUserPrivProtocol, UsmUser),
		     element(?usmUserAuthKey, UsmUser),
		     element(?usmUserPrivKey, UsmUser)},
    {ok, {MsgAuthEngineID, SecName, ScopedPDUBytes, CachedSecData}}.
    

authenticate_incoming(Packet, UsmSecParams, UsmUser, SecLevel) ->
    SecName = element(?usmUserSecurityName, UsmUser),
    %% 3.2.6
    ?vtrace("authenticate incoming: 3.2.6",[]),
    AuthProtocol = element(?usmUserAuthProtocol, UsmUser),
    #usmSecurityParameters{msgAuthoritativeEngineID    = MsgAuthEngineID,
			   msgAuthoritativeEngineBoots = MsgAuthEngineBoots,
			   msgAuthoritativeEngineTime  = MsgAuthEngineTime,
			   msgAuthenticationParameters = MsgAuthParams} =
	UsmSecParams,
    case snmp_misc:is_auth(SecLevel) of
	true ->
	    case AuthProtocol of
		?usmNoAuthProtocol -> % 3.2.5
		    error(usmStatsUnsupportedSecLevels, 
			  ?usmStatsUnsupportedSecLevels, SecName);
		_ ->
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
		    end
	    end;
	false ->  % noAuth
	    ok
    end.
	    
is_auth(AuthProtocol, AuthKey, AuthParams, Packet, SecName,
	MsgAuthEngineID, MsgAuthEngineBoots, MsgAuthEngineTime) ->
    IsAuth =
	case AuthProtocol of
	    ?usmHMACMD5AuthProtocol ->
		md5_auth_in(AuthKey, AuthParams, Packet);
	    ?usmHMACSHAAuthProtocol ->
		sha_auth_in(AuthKey, AuthParams, Packet)
	end,
    case IsAuth of
	true ->
	    %% 3.2.7
	    ?vtrace("retrieve EngineBoots and EngineTime: 3.2.7",[]),
	    SnmpEngineID = snmp_framework_mib:get_engine_id(),
	    ?vtrace("SnmpEngineID: ~p",[SnmpEngineID]),
	    case MsgAuthEngineID of
		SnmpEngineID -> %% 3.2.7a
		    ?vtrace("we are authoritative: 3.2.7a",[]),
		    SnmpEngineBoots = snmp_framework_mib:get_engine_boots(),
		    ?vtrace("SnmpEngineBoots: ~p",[SnmpEngineBoots]),
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
			true -> true;
			%% OTP-4090 (OTP-3542)
			false -> error(usmStatsNotInTimeWindows,
				       ?usmStatsNotInTimeWindows_instance,
				       SecName,
				       [{securityLevel, 1}]) % authNoPriv
		    end;
		_ -> %% 3.2.7b - we're non-authoritative
		    ?vtrace("we are non-authoritative: 3.2.7b",[]),
		    SnmpEngineBoots = get_engine_boots(MsgAuthEngineID),
		    ?vtrace("SnmpEngineBoots: ~p",[SnmpEngineBoots]),
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
			    ?vtrace("update msgAuthoritativeEngineID: 3.2.7b1",
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
		    ?vtrace("check if message is outside time window: 3.2.7b2",
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
			    throw({error, notInTimeWindow, []});
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
	false ->
	    Data;
	true ->
	    EncryptedPDU = snmp_pdus:dec_scoped_pdu_data(Data),
	    SecName = element(?usmUserSecurityName, UsmUser),
	    PrivProtocol = element(?usmUserPrivProtocol, UsmUser),
	    MsgPrivParams =
		UsmSecParams#usmSecurityParameters.msgPrivacyParameters,
	    TryDecrypt=
		case PrivProtocol of
		    ?usmNoPrivProtocol -> % 3.2.5
			error(usmStatsUnsupportedSecLevels,
			      ?usmStatsUnsupportedSecLevels, SecName);
		    ?usmDESPrivProtocol ->
			des_decrypt(element(?usmUserPrivKey, UsmUser),
				    MsgPrivParams,
				    EncryptedPDU)
		end,
	    case TryDecrypt of
		error ->
		    error(usmStatsDecryptionErrors, 
			  ?usmStatsDecryptionErrors, SecName);
		DecryptedData ->
		    DecryptedData
	    end
    end.


generate_outgoing_msg(Message, SecEngineID, SecName, SecData, SecLevel) ->
    %% 3.1.1
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
			?vdebug("found user ~p with wrong row status: ~p", 
				[Name, RowStatus]),
			throw({error, unknownSecurityName});
		    _ ->
			throw({error, unknownSecurityName})
		end;
	    [MsgUserName] ->
		%% This means the user at the engine is unknown
		{MsgUserName, ?usmNoAuthProtocol, ?usmNoPrivProtocol, "", ""};
	    _ -> % 3.1.1a
		SecData
	end,
    %% 3.1.4
    ScopedPduBytes = Message#message.data,
    {ScopedPduData, MsgPrivParams} =
	encrypt(ScopedPduBytes, PrivProtocol, PrivKey, SecLevel),
    SnmpEngineID = snmp_framework_mib:get_engine_id(),
    ?vtrace("SnmpEngineID: ~p",[SnmpEngineID]),
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
    UsmSecParams =
	#usmSecurityParameters{msgAuthoritativeEngineID = SecEngineID,
			       msgAuthoritativeEngineBoots = MsgAuthEngineBoots,
			       msgAuthoritativeEngineTime = MsgAuthEngineTime,
			       msgUserName = UserName,
			       msgPrivacyParameters = MsgPrivParams},
    Message2 = Message#message{data = ScopedPduData},
    %% 3.1.8
    authenticate_outgoing(Message2, UsmSecParams,
			  AuthKey, AuthProtocol, SecLevel).


%% Ret: {ScopedPDU, MsgPrivParams} - both are already encoded as OCTET STRINGs
encrypt(Data, PrivProtocol, PrivKey, SecLevel) ->
    case snmp_misc:is_priv(SecLevel) of
	false -> % 3.1.4b
	    {Data, []};
	true -> % 3.1.4a
	    TryEncrypt=
		case PrivProtocol of
		    ?usmNoPrivProtocol -> % 3.1.2
			throw({error, unsupportedSecurityLevel});
		    ?usmDESPrivProtocol ->
			des_encrypt(PrivKey, Data)
		end,
	    case TryEncrypt of
		error ->
		    throw({error, encryptionError});
		{ScopedPduData, MsgPrivParams} ->
		    {snmp_pdus:enc_oct_str_tag(ScopedPduData),
		     MsgPrivParams}
	    end
    end.


authenticate_outgoing(Message, UsmSecParams, 
		      AuthKey, AuthProtocol, SecLevel) ->
    Message2 = 
	case snmp_misc:is_auth(SecLevel) of
	    true ->
		case AuthProtocol of
		    ?usmNoAuthProtocol -> % 3.1.3
			throw({error, unSupportedSecurityLevel});
		    ?usmHMACMD5AuthProtocol ->
			md5_auth_out(AuthKey, Message, UsmSecParams);
		    ?usmHMACSHAAuthProtocol ->
			sha_auth_out(AuthKey, Message, UsmSecParams)
		end;
	    false ->
		set_msg_auth_params(Message, UsmSecParams, [])
	end,
    snmp_pdus:enc_message_only(Message2).
    
	    

%%-----------------------------------------------------------------
%% Auth and priv algorithms
%%-----------------------------------------------------------------
md5_auth_out(AuthKey, Message, UsmSecParams) ->
    %% 6.3.1.1
    Message2 = set_msg_auth_params(Message, UsmSecParams, ?twelwe_zeros),
    Packet = snmp_pdus:enc_message_only(Message2),
    %% 6.3.1.2-4 is done by the crypto function
    %% 6.3.1.4
    MAC = binary_to_list(crypto:md5_mac_96(AuthKey, Packet)),
    %% 6.3.1.5
    set_msg_auth_params(Message, UsmSecParams, MAC).

md5_auth_in(AuthKey, AuthParams, Packet) when length(AuthParams) == 12 ->
    %% 6.3.2.3
    Packet2 = patch_packet(binary_to_list(Packet)),
    %% 6.3.2.5
    MAC = binary_to_list(crypto:md5_mac_96(AuthKey, Packet2)),
    %% 6.3.2.6
    MAC == AuthParams;
md5_auth_in(_AuthKey, _AuthParams, _Packet) ->
    %% 6.3.2.1
    false.

sha_auth_out(AuthKey, Message, UsmSecParams) ->
    %% 7.3.1.1
    Message2 = set_msg_auth_params(Message, UsmSecParams, ?twelwe_zeros),
    Packet = snmp_pdus:enc_message_only(Message2),
    %% 7.3.1.2-4 is done by the crypto function
    %% 7.3.1.4
    MAC = binary_to_list(crypto:sha_mac_96(AuthKey, Packet)),
    %% 7.3.1.5
    set_msg_auth_params(Message, UsmSecParams, MAC).

sha_auth_in(AuthKey, AuthParams, Packet) when length(AuthParams) == 12 ->
    %% 7.3.2.3
    Packet2 = patch_packet(binary_to_list(Packet)),
    %% 7.3.2.5
    MAC = binary_to_list(crypto:sha_mac_96(AuthKey, Packet2)),
    %% 7.3.2.6
    MAC == AuthParams;
sha_auth_in(_AuthKey, _AuthParams, _Packet) ->
    %% 7.3.2.1
    false.

des_encrypt(PrivKey, Data) ->
    [A,B,C,D,E,F,G,H | PreIV] = PrivKey,
    DesKey = [A,B,C,D,E,F,G,H],
    Salt = get_salt(),
    IV = snmp_misc:str_xor(PreIV, Salt),
    TailLen = (8 - (length(Data) rem 8)) rem 8,
    Tail = mk_tail(TailLen),
    EncData = crypto:des_cbc_encrypt(DesKey, IV, [Data,Tail]),
    {binary_to_list(EncData), Salt}.

des_decrypt(PrivKey, MsgPrivParams, EncData) when length(MsgPrivParams) == 8 ->
    [A,B,C,D,E,F,G,H | PreIV] = PrivKey,
    DesKey = [A,B,C,D,E,F,G,H],
    Salt = MsgPrivParams,
    IV = snmp_misc:str_xor(PreIV, Salt),
    %% Whatabout errors here???  E.g. not a mulitple of 8!
    Data = binary_to_list(crypto:des_cbc_decrypt(DesKey, IV, EncData)),
    Data2 = snmp_pdus:strip_encrypted_scoped_pdu_data(Data),
    Data2.

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
mk_tail(N) when N > 0 ->
    [0 | mk_tail(N-1)];
mk_tail(0) ->
    [].

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

set_msg_auth_params(Message, UsmSecParams, AuthParams) ->
    NUsmSecParams = 
	UsmSecParams#usmSecurityParameters{msgAuthenticationParameters =
					   AuthParams},
    SecBytes = snmp_pdus:enc_usm_security_parameters(NUsmSecParams),
    VsnHdr   = Message#message.vsn_hdr,
    NVsnHdr  = VsnHdr#v3_hdr{msgSecurityParameters = SecBytes},
    Message#message{vsn_hdr = NVsnHdr}.


%% Not very nice...
%% This function patches the asn.1 encoded message. It changes the
%% AuthenticationParameters to 12 zeros.
%% NOTE: returns a deep list of bytes
patch_packet([48 | T]) ->
    %% Length for whole packet - 2 is tag for version
    {Len1, [2 | T1]} = split_len(T),
    %% Length for version - 48 is tag for header data
    {Len2, [Vsn,48|T2]} = split_len(T1),
    %% Length for header data
    {Len3, T3} = split_len(T2),
    [48,Len1,2,Len2,Vsn,48,Len3|pp2(dec_len(Len3),T3)].

%% Skip HeaderData - 4 is tag for SecurityParameters
pp2(0,[4|T]) ->
    %% 48 is tag for UsmSecParams
    {Len1,[48|T1]} = split_len(T),
    %% 4 is tag for EngineID
    {Len2,[4|T2]} = split_len(T1),
    %% Len 3 is length for EngineID
    {Len3,T3} = split_len(T2),
    [4,Len1,48,Len2,4,Len3|pp3(dec_len(Len3),T3)];
pp2(N,[H|T]) ->
    [H|pp2(N-1,T)].

%% Skip EngineID - 2 is tag for EngineBoots
pp3(0,[2|T]) ->
    {Len1,T1} = split_len(T),
    [2,Len1|pp4(dec_len(Len1),T1)];
pp3(N,[H|T]) ->
    [H|pp3(N-1,T)].

%% Skip EngineBoots - 2 is tag for EngineTime
pp4(0,[2|T]) ->
    {Len1,T1} = split_len(T),
    [2,Len1|pp5(dec_len(Len1),T1)];
pp4(N,[H|T]) ->
    [H|pp4(N-1,T)].

%% Skip EngineTime - 4 is tag for UserName 
pp5(0,[4|T]) ->
    {Len1,T1} = split_len(T),
    [4,Len1|pp6(dec_len(Len1),T1)];
pp5(N,[H|T]) ->
    [H|pp5(N-1,T)].

%% Skip UserName - 4 is tag for AuthenticationParameters
%% This is what we're looking for!
pp6(0,[4|T]) ->
    {Len1,[_,_,_,_,_,_,_,_,_,_,_,_|T1]} = split_len(T),
    12 == dec_len(Len1),
    [4,Len1,?twelwe_zeros|T1];
pp6(N,[H|T]) ->
    [H|pp6(N-1,T)].

%% Code copied from snmp_asn1.erl.  Modifed a bit.

%% Returns {LengthOctets, Rest}
split_len([Hd|Tl]) ->
    %% definite form
    case is8set(Hd) of
	0 -> % Short form
	    {Hd,Tl};
	1 -> % Long form - at least one more octet
	    No = clear(Hd,8),
	    {DigList,Rest} = head(No,Tl),
	    {[Hd | DigList], Rest}
    end.

dec_len(D) when integer(D) ->
    D;
dec_len([_LongOctet|T]) ->
    dl(T).
dl([D]) ->
    D;
dl([A,B]) ->
    (A bsl 8) bor B;
dl([A,B,C]) ->
    (A bsl 16) bor (B bsl 8) bor C;
dl([0 | T]) ->
    dl(T).

head(L,List) when length(List) == L -> {List,[]};
head(L,List) ->
    head(L,List,[]).

head(0,L,Res) ->
    {lists:reverse(Res),L};

head(Int,[H|Tail],Res) ->
    head(Int-1,Tail,[H|Res]).

clear(Byte,8) -> 
    Byte band 127;
clear(Byte,Pos) when Pos < 9 ->
    Mask = bnot set(0,Pos),
    Mask band Byte.

is8set(Byte) ->
    if
	Byte > 127 -> 1;
	true -> 0
    end.

set(Byte,8) -> 
    Byte bor 2#10000000;
set(Byte,Pos)  when Pos < 9 ->
    Mask = 1  bsl (Pos-1),
    Byte bor Mask.

