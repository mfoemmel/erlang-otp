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
-module(snmp_conf).

-include("SNMPv2-TC.hrl").
-include("SNMPv2-TM.hrl").
-include("SNMPv2-MIB.hrl").
-include("OTP-SNMPEA-MIB.hrl").
-include("SNMP-USER-BASED-SM-MIB.hrl").
-include("SNMP-VIEW-BASED-ACM-MIB.hrl").
-include("SNMP-COMMUNITY-MIB.hrl").
-include("snmp_types.hrl").

-define(VMODULE,"CONF").
-include("snmp_verbosity.hrl").


%% Enum definitions from intCommunityAccess
-define(intCommunityAccess_readWrite, 2).
-define(intCommunityAccess_read, 1).

%%%-----------------------------------------------------------------
%%% Handles the configuration files.
%%%-----------------------------------------------------------------

%% External exports
-export([read_internal_config_files/1, read_agent/1, read_standard/1,
	 read_target_config_files/1, read_notify_config_files/1,
	 read_vacm_config_files/1, read_usm_config_files/1,
	 read_community_config_files/1]).
-export([check_mandatory/2]).

%% Internal exports
-export([check_old_community/1, check_trap/1, check_view/1, check_address/1]).
-export([check_target_addr/1, check_target_params/1]).
-export([check_context/1, check_notify/1]).
-export([check_usm/1, check_vacm/1, check_community/1]).
-export([check_agent/1, check_standard/1, check_old_params/1]).

%%-----------------------------------------------------------------
%% Func: read_internal_config_files/1
%% Args: Dir is the directory with trailing dir_separator where
%%       the configuration files can be found.
%% Purpose: Reads the configuration files.
%% Returns: A list of contextnames.
%% Fails: If an error occurs, the process will die with Reason
%%        configuration_error.
%%-----------------------------------------------------------------
read_internal_config_files(Dir) ->
    ?vdebug("check context config file",[]),
    CtxFile = filename:join(Dir, "context.conf"),
    case file:read_file_info(CtxFile) of
	{ok, _} -> ok;
	{error, _} -> convert_context(Dir)
    end,
    ?vdebug("read contexts config file",[]),
    Contexts = snmp_misc:read(CtxFile, {snmp_conf, check_context}),
    Contexts.

read_target_config_files(Dir) ->
    ?vdebug("check target address config file",[]),
    TAFile = filename:join(Dir, "target_addr.conf"),
    case file:read_file_info(TAFile) of
	{ok, _} -> ok;
	{error, _} -> convert_trap(Dir)
    end,
    ?vdebug("check vacm config file",[]),
    VACMFile = filename:join(Dir, "vacm.conf"),
    case file:read_file_info(VACMFile) of
	{ok, _} -> ok;
	{error, _} -> convert_target_params(Dir)
    end,
    ?vdebug("read addresses from target config file",[]),
    Addrs = snmp_misc:read(TAFile,
			   {snmp_conf, check_target_addr}),
    ?vdebug("read target params config file",[]),
    Params = snmp_misc:read(filename:join(Dir, "target_params.conf"), 
			    {snmp_conf, check_target_params}),
    {Addrs, Params}.
    
read_notify_config_files(Dir) ->
    ?vdebug("read notify config file",[]),
    Notifs = snmp_misc:read(filename:join(Dir, "notify.conf"), 
			    {snmp_conf, check_notify}),
    Notifs.

read_vacm_config_files(Dir) ->
    ?vdebug("check vacm config file",[]),
    VACMFile = filename:join(Dir, "vacm.conf"),
    case file:read_file_info(VACMFile) of
	{ok, _} -> ok;
	{error, _} -> convert_view(Dir)
    end,
    ?vdebug("read vacm config file",[]),
    Vacms = snmp_misc:read(VACMFile, {snmp_conf, check_vacm}),
    Sec2Group = [X || {vacmSecurityToGroup, X} <- Vacms],
    Access = [X || {vacmAccess, X} <- Vacms],
    View = [X || {vacmViewTreeFamily, X} <- Vacms],
    {Sec2Group, Access, View}.

read_usm_config_files(Dir) ->
    ?vdebug("check usm config file",[]),
    USMFile = filename:join(Dir, "usm.conf"),
    case file:read_file_info(USMFile) of
	{ok, _} -> ok;
	{error, _} -> generate_usm(Dir)
    end,
    ?vdebug("read usm config file",[]),
    Usms = snmp_misc:read(USMFile, {snmp_conf, check_usm}),
    Usms.

read_community_config_files(Dir) ->
    ?vdebug("read community config file",[]),
    Comms = snmp_misc:read(filename:join(Dir, "community.conf"), 
			   {snmp_conf, check_community}),
    Comms.

%%-----------------------------------------------------------------
%% Func: read_agent/1
%% Args: Dir is the directory with trailing dir_separator where
%%       the configuration files can be found.
%% Purpose: Reads the agent configuration files.
%% Returns: A list of {Variable, Value}.
%%-----------------------------------------------------------------
read_agent(Dir) ->
    ?vdebug("check agent config file",[]),
    File = filename:join(Dir, "agent.conf"), 
    ?vdebug("read agent config file",[]),
    Agent = snmp_misc:read(File, {snmp_conf, check_agent}),
    Agent2 =
	case lists:keymember(snmpEngineMaxMessageSize, 1, Agent) of
	    true ->
		Agent;
	    false ->
		convert_agent(File),
		snmp_misc:read(File, {snmp_conf, check_agent})
	end,
    sort_agent(Agent2).

%%-----------------------------------------------------------------
%% Make sure that each mandatory agent attribute is present, and
%% provide default values for the other non-present attributes.
%%-----------------------------------------------------------------
sort_agent(L) ->
    L2 = check_mandatory(L, [{intAgentIpAddress, mandatory},
			     {intAgentUDPPort, mandatory},
			     {snmpEngineMaxMessageSize, mandatory},
			     {snmpEngineID, mandatory}]),
    lists:keysort(1, L2).

%%-----------------------------------------------------------------
%% Func: read_standard/1
%% Args: Dir is the directory with trailing dir_separator where
%%       the configuration files can be found.
%% Purpose: Reads th standard configuration file.
%% Returns: A list of standard variables
%% Fails: If an error occurs, the process will die with Reason
%%        configuration_error.
%%-----------------------------------------------------------------
read_standard(Dir) ->
    Standard = snmp_misc:read(filename:join(Dir, "standard.conf"), 
			      {snmp_conf, check_standard}),
    sort_standard(Standard).

%%-----------------------------------------------------------------
%% Make sure that each mandatory standard attribute is present, and
%% provide default values for the other non-present attributes.
%%-----------------------------------------------------------------
sort_standard(L) ->
    L2 = check_mandatory(L, [{sysContact, {value, ""}},
			     {sysDescr, {value, ""}},
			     {sysLocation, {value, ""}},
			     {sysName, {value, ""}},
			     {sysObjectID, mandatory},
			     {sysServices, mandatory},
			     {snmpEnableAuthenTraps, mandatory}]),
    lists:keysort(1, L2).

check_mandatory(L, [{Key, Value}|T]) ->
    case lists:keymember(Key, 1, L) of
	true -> check_mandatory(L, T);
	false when Value == mandatory -> 
	    error("No value specified for mandatory attribute ~w", [Key]);
	false ->
	    {value, V} = Value,
	    check_mandatory([{Key, V} | L], T)
    end;
check_mandatory(L, []) -> L.

%%-----------------------------------------------------------------
%%  Agent
%%  {Name, Value}.
%%-----------------------------------------------------------------
check_agent({intAgentIpAddress, Value}) -> check_ip(Value);
check_agent({intAgentUDPPort, Value}) -> check_integer(Value);
%% This one is kept for backwards compatibility
check_agent({intAgentMaxPacketSize, Value}) -> check_packet_size(Value);
check_agent({snmpEngineMaxMessageSize, Value}) -> check_packet_size(Value);
check_agent({snmpEngineID, Value}) -> check_string(Value);
check_agent(X) -> {invalid_agent_attribute, X}.
    
%%-----------------------------------------------------------------
%%  Context
%%  Context.
%%-----------------------------------------------------------------
check_context(Context) ->
    check_string(Context),
    {ok, {Context}};
check_context(X) ->
    {invalid_context, X}.

%%-----------------------------------------------------------------
%%  TargetAddr
%%  {Name, Ip, Udp, Timeout, RetryCount, TagList, Params, EngineId,
%%   TMask, MMS}
%%-----------------------------------------------------------------
check_target_addr({Name, Ip, Udp, Timeout, RetryCount, TagList,
		   Params, EngineId, TMask, MMS}) ->
    ?vtrace("check target address with:"
	    "~n   Name:       ~s"
	    "~n   Ip:         ~p"
	    "~n   Udp:        ~p"
	    "~n   Timeout:    ~p"
	    "~n   RetryCount: ~p"
	    "~n   TagList:    ~p"
	    "~n   Params:     ~p"
	    "~n   EngineId:   ~p"
	    "~n   TMask:      ~p"
	    "~n   MMS:        ~p",
	    [Name,Ip,Udp,Timeout,RetryCount,
	     TagList,Params,EngineId,TMask,MMS]),
    check_string(Name,{gt,0}),
    check_ip(Ip),
    check_integer(Udp),
    check_integer(Timeout),
    if
	Timeout < 0 -> throw({invalid_timeout, Timeout});
	true -> ok
    end,
    check_integer(RetryCount),
    if
	RetryCount < 0 -> throw({invalid_retry_count, RetryCount});
	true -> ok
    end,
    check_string(TagList),
    check_string(Params),
    check_string(EngineId),
    TAddr = Ip ++ [Udp div 256, Udp rem 256],
    check_mask(TMask, TAddr, fun check_ip_udp/1),
    check_packet_size(MMS),
    ?vtrace("check target address done",[]),
    {ok, {Name, ?snmpUDPDomain, TAddr, Timeout,
	  RetryCount, TagList, Params,
	  ?'StorageType_nonVolatile', ?'RowStatus_active', EngineId,
	 TMask, MMS}}; % Values for Augmenting table in SNMP-COMMUNITY-MIB
%% Use dummy engine id if the old style is found
check_target_addr({Name, Ip, Udp, Timeout, RetryCount, TagList, Params}) ->
    check_target_addr({Name, Ip, Udp, Timeout, RetryCount, TagList,
		       Params, "dummy", [], 2048});
check_target_addr(X) ->
    {invalid_target_addr, X}.

%%-----------------------------------------------------------------
%%  TargetParams
%%  {Name, MPModel, SecurityModel, SecurityName, SecurityLevel}
%%-----------------------------------------------------------------
check_target_params({Name, MPModel, SecModel, SecName, SecLevel}) ->
    check_string(Name,{gt,0}),
    {ok, MP} = check_atom(MPModel, [{v1, ?MP_V1},
				    {v2c, ?MP_V2C},
				    {v3, ?MP_V3}]),
    SecM = check_sec_model(SecModel, [any]),
    check_string(SecName),
    SecL = check_sec_level(SecLevel),
    {ok, {Name, MP, SecM, SecName, SecL, 
	  ?'StorageType_nonVolatile', ?'RowStatus_active'}};
check_target_params(X) ->
    {invalid_target_params, X}.

%%-----------------------------------------------------------------
%%  Notify
%%  {Name, Tag, Type}
%%-----------------------------------------------------------------
check_notify({Name, Tag, Type}) ->
    check_string(Name,{gt,0}),
    check_string(Tag),
    {ok, Val} = check_atom(Type, [{trap, 1},
				  {inform, 2}]),
    {ok, {Name, Tag, Val, %% OTP-4329
	  ?'StorageType_nonVolatile', ?'RowStatus_active'}};
check_notify(X) ->
    {invalid_notify, X}.

%%-----------------------------------------------------------------
%% VACM tables
%%-----------------------------------------------------------------
check_vacm({vacmSecurityToGroup, SecModel, SecName, GroupName}) ->
    SecM = check_sec_model(SecModel, []),
    check_string(SecName),
    check_string(GroupName),
    {ok, {vacmSecurityToGroup, {SecM, SecName, GroupName,
				?'StorageType_nonVolatile',
				?'RowStatus_active'}}};
check_vacm({vacmAccess, GroupName, Prefix, SecModel, SecLevel,
	    Match, RV, WV, NV}) ->
    check_string(GroupName),
    check_string(Prefix),
    SecM = check_sec_model(SecModel, []),
    SecL = check_sec_level(SecLevel),
    {ok, M} = check_atom(Match, [{exact, ?vacmAccessContextMatch_exact},
				 {prefix, ?vacmAccessContextMatch_prefix}]),
    check_string(RV),
    check_string(WV),
    check_string(NV),
    {ok, {vacmAccess, {GroupName, Prefix, SecM, SecL, 
		       {M, RV, WV, NV,
			?'StorageType_nonVolatile', ?'RowStatus_active'}}}};
check_vacm({vacmViewTreeFamily, ViewName, Tree, Type, Mask}) ->
    check_string(ViewName),
    check_oid(Tree),
    {ok, TypeVal} =
	check_atom(Type, [{included, ?view_included},
			  {excluded, ?view_excluded}]),
    MaskVal = 
	case catch check_atom(Mask, [{null, []}]) of
	    {ok, X} ->
		X;
	    _  -> 
		check_oid(Mask),
		Mask
    end,
    {ok, {vacmViewTreeFamily, 
	  {ViewName, Tree, MaskVal,
	   TypeVal, ?'StorageType_nonVolatile', ?'RowStatus_active'}}};
check_vacm(X) ->
    {invalid_vacm, X}.

%%-----------------------------------------------------------------
%% Usm User
%%-----------------------------------------------------------------
check_usm({EngineID, Name, SecName, Clone, AuthP, AuthKeyC, OwnAuthKeyC,
	   PrivP, PrivKeyC, OwnPrivKeyC, Public, AuthKey, PrivKey}) ->
    check_string(EngineID),
    check_string(Name),
    check_string(SecName),
    CloneVal =
	case catch check_atom(Clone, [{zeroDotZero, ?zeroDotZero}]) of
	    {ok, X} ->
		X;
	    _ ->
		check_oid(Clone),
		Clone
	end,
    {ok, AuthProto} = 
	check_atom(AuthP,
		   [{usmNoAuthProtocol, ?usmNoAuthProtocol},
		    {usmHMACSHAAuthProtocol, ?usmHMACSHAAuthProtocol},
		    {usmHMACMD5AuthProtocol, ?usmHMACMD5AuthProtocol}]),
    check_string(AuthKeyC),
    check_string(OwnAuthKeyC),
    {ok, PrivProto} = 
	check_atom(PrivP,
		   [{usmNoPrivProtocol, ?usmNoPrivProtocol},
		    {usmDESPrivProtocol, ?usmDESPrivProtocol}]),
    check_string(PrivKeyC),
    check_string(OwnPrivKeyC),
    check_string(Public),
    check_string(AuthKey, alen(AuthP)),
    check_string(PrivKey, plen(PrivP)),

    {ok, {EngineID, Name, SecName, CloneVal, AuthProto, AuthKeyC, OwnAuthKeyC,
	  PrivProto, PrivKeyC, OwnPrivKeyC, Public,
	  ?'StorageType_nonVolatile', ?'RowStatus_active', AuthKey, PrivKey}};
check_usm(X) ->
    {invalid_user, X}.

alen(usmNoAuthProtocol) -> any;
alen(usmHMACMD5AuthProtocol) -> 16;
alen(usmHMACSHAAuthProtocol) -> 20.

plen(usmNoPrivProtocol) -> any;
plen(usmDESPrivProtocol) -> 16.
    

%%-----------------------------------------------------------------
%%  Community
%%  {Index, CommunityName, SecurityName, ContextName, TransportTag}.
%%-----------------------------------------------------------------
check_community({Index, CommunityName, SecurityName, ContextName,
		 TransportTag}) ->
    check_string(Index,{gt,0}),
    check_string(CommunityName),
    check_string(SecurityName),
    check_string(ContextName),
    check_string(TransportTag),
    EngineID = snmp_framework_mib:get_engine_id(),
    {ok, {Index, CommunityName, SecurityName, EngineID, ContextName,
	  TransportTag,
	  ?'StorageType_nonVolatile', ?'RowStatus_active'}};
check_community(X) ->
    {invalid_community, X}.

%%-----------------------------------------------------------------
%%  Standard
%%  {Name, Value}.
%%-----------------------------------------------------------------
check_standard({sysDescr, Value}) -> check_string(Value);
check_standard({sysObjectID, Value}) -> check_oid(Value);
check_standard({sysContact, Value}) -> check_string(Value);
check_standard({sysName, Value}) -> check_string(Value);
check_standard({sysLocation, Value}) -> check_string(Value);
check_standard({sysServices, Value}) -> check_integer(Value);
check_standard({snmpEnableAuthenTraps, Value}) -> 
    {ok, Val} = check_atom(Value, [{enabled, ?snmpEnableAuthenTraps_enabled},
				   {disabled, ?snmpEnableAuthenTraps_disabled}]),
    {ok, {snmpEnableAuthenTraps, Val}};
check_standard({Attrib, _Value}) -> {unknown_attribute, Attrib};
check_standard(X) -> {invalid_standard_specification, X}.
    
check_sec_model(SecModel, Exclude) ->
    All = [{any, ?SEC_ANY},
	   {v1, ?SEC_V1},
	   {v2c, ?SEC_V2C},
	   {usm, ?SEC_USM}],
    {ok, SecM} =
	check_atom(SecModel, [{X, Y} || {X, Y} <- All,
					not lists:member(X, Exclude)]),
    SecM.

check_sec_level(SecLevel) ->
    {ok, SecL} = check_atom(SecLevel, [{noAuthNoPriv, 1},
				       {authNoPriv, 2},
				       {authPriv, 3}]),
    SecL.

check_integer(X) when integer(X) -> true;
check_integer(X) -> throw({invalid_integer, X}).

check_string(X) when list(X) -> true;
check_string(X) -> throw({invalid_string, X}).

check_string(X, any)       when list(X) -> true;
check_string(X,{gt,Len})   when list(X), length(X) > Len -> true;
check_string(X,{gt,_Len})  when list(X) -> throw({invalid_length, X});
check_string(X,{gte,Len})  when list(X), length(X) >= Len -> true;
check_string(X,{gte,_Len}) when list(X) -> throw({invalid_length, X});
check_string(X,{lt,Len})   when list(X), length(X) < Len -> true;
check_string(X,{lt,_Len})  when list(X) -> throw({invalid_length, X});
check_string(X,{lte,Len})  when list(X), length(X) =< Len -> true;
check_string(X,{lte,_Len}) when list(X) -> throw({invalid_length, X});
check_string(X, Len)       when list(X), length(X) == Len -> true;
check_string(X, _Len)      when list(X) -> throw({invalid_length, X});
check_string(X, _Len) -> throw({invalid_string, X}).

check_oid([E1,E2|R]) when E1 * 40 + E2 =< 255 ->
    X = [E1,E2|R],
    case all_integer(X) of
	true -> true;
	_ -> throw({invalid_object_identifier, X})
    end;
check_oid(X) -> throw({invalid_object_identifier, X}).

all_integer([H|T]) when integer(H) -> all_integer(T);
all_integer([_H|_T]) -> false;
all_integer([]) -> true.

check_ip(X) ->
    case catch all_integer(X) of
	true when length(X) == 4 -> true;
	_ -> throw({invalid_ip_address, X})
    end.

check_ip_udp(X) ->
    case catch all_integer(X) of
	true when length(X) == 6 -> true;
	_ -> throw({invalid_taddress, X})
    end.

check_mask([], _TAddr, _F) -> ok;
check_mask(TMask, TAddr, F) when length(TMask) == length(TAddr) ->
    F(TMask);
check_mask(TMask, _TAddr, _F) -> throw({bad_tmask_length, TMask}).

check_packet_size(X) when integer(X), X >= 484, X =< 2147483647 -> true;
check_packet_size(X) -> throw({invalid_packet_size, X}).

check_atom(X, Atoms) ->
    case lists:keysearch(X, 1, Atoms) of
	{value, {X, Val}} -> {ok, Val};
	_ -> throw({"Expected", Atoms, "got", X})
    end.

error(Format, Error) ->
    config_err(Format, Error),
    exit(configuration_error).
%% error(File, Line, Error) ->
%%     config_err("~p:~w: ~p", [File, Line, Error]),
%%     exit(configuration_error).

%%%-----------------------------------------------------------------
%%% Converting functions - converts old style config files to
%%% semantically (almost) equivalent new files.
%%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Convert old style intAgentMaxPacketSize to new
%% snmpEngineMaxMessageSize.  Adds a snmpEngineID also.
%%-----------------------------------------------------------------
convert_agent(File) ->
    error_logger:info_msg("snmp: Converting old style agent.conf file "
			  "to new file with snmpEngine support.\n", []),
    case catch c_agent(File) of
	ok ->
	    ok;
	{'EXIT', Reason} ->
	    error_logger:error_msg("snmp: conversion failed. ~w\n", [Reason]),
	    exit({conversion_failed, Reason})
    end.

c_agent(File) ->
    {ok, Bin} = file:read_file(File),
    Str = binary_to_list(Bin),
    Str2 = sub(Str),
    EngineStr = ["\n", "{snmpEngineID, \"dummyEngine\"}.\n"],
    HdrStr = agent_header(),
    ok = file:write_file(File, list_to_binary([HdrStr, Str2, EngineStr])).

sub(Str) ->
    case regexp:sub(Str, "intAgentMaxPacketSize", "snmpEngineMaxMessageSize") of
	{ok, NewStr, 1} ->
	    sub(NewStr);
	{ok, _NewStr, 0} ->
	    Str
    end.

agent_header() ->
    {Y,Mo,D} = date(),
    {H,Mi,S} = time(),
    io_lib:format("%% This file was automatically changed by "
		  "snmp_config v~s  ~w-~2.2.0w-~2.2.0w "
		  "~2.2.0w:~2.2.0w:~2.2.0w\n",
		  [?version,Y,Mo,D,H,Mi,S]).

%%-----------------------------------------------------------------
%% Converts old trap_dest.conf to the new target*.conf
%%-----------------------------------------------------------------
convert_trap(Dir) ->
    error_logger:info_msg("snmp: Converting old style trap_dest.conf file "
			  "to new target and notify files.\n", []),
    case catch c_trap(Dir) of
	ok ->
	    ok;
	Else ->
	    file:delete(filename:join(Dir, "target_addr.conf")),
	    file:delete(filename:join(Dir, "target_params.conf")),
	    file:delete(filename:join(Dir, "notify.conf")),
	    {Reason, Str} = case Else of
				{error, R, S} -> {R, S};
				_ -> {Else, io_lib:format("~p", [Else])}
			    end,
	    error_logger:error_msg("snmp: conversion failed. ~s\n", [Str]),
	    exit({conversion_failed, Reason})
    end.

c_trap(Dir) ->
    Traps = snmp_misc:read(filename:join(Dir, "trap_dest.conf"), 
			   {snmp_conf, check_trap}),
    Addresses = snmp_misc:read(filename:join(Dir, "address.conf"), 
			       {snmp_conf, check_address}),
    
    %% Trap = {CommunityString, Ip, ?'RowStatus_active', VsnVal}}
    %% Address = {Ip, UDP, PacketSize, ?'RowStatus_active'}}

    SortedTraps = sort_traps(Traps, []),
    Communities = get_communities(Traps, []),
    
    wr_addr(Dir, SortedTraps, Addresses),
    wr_params(Dir, [1,2]),  % always write both of them
    wr_notify(Dir, Communities).

%%-----------------------------------------------------------------
%%  Trap
%%  {CommunityString, ManagerIpAddress}.
%%-----------------------------------------------------------------
check_trap({CommunityString, Ip, Vsn}) ->
    check_ip(Ip),
    check_string(CommunityString),
    {ok, VsnVal} =
	check_atom(Vsn, [{v1, 1},
			 {v2, 2}]),
    {ok, {CommunityString, Ip, ?'RowStatus_active', VsnVal}};
check_trap({CommunityString, Ip}) ->
    % Old format
    check_ip(Ip),
    check_string(CommunityString),
    {ok, {CommunityString, Ip, ?'RowStatus_active', 1}};
check_trap(X) ->
    {invalid_trap, X}.

%% Ret: [{ {Ip,Vsn}, [Community] }]
sort_traps([{Comm, Ip, _, Vsn} | T], Res) ->
    case lists:keymember({Ip, Vsn}, 1, Res) of
	false ->
	    Comms = [Comm | [C1 || {C1, I, _, V} <- T,
				   I == Ip,
				   V == Vsn]],
	    sort_traps(T, [{{Ip, Vsn},  Comms} | Res]);
	true ->
	    sort_traps(T, Res)
    end;
sort_traps([], Res) ->
    Res.
    
get_communities([{Comm, _, _, _} | T], Res) ->
    case lists:member(Comm, Res) of
	true ->
	    get_communities(T, Res);
	false ->
	    get_communities(T, [Comm | Res])
    end;
get_communities([], Res) ->
    Res.

%% {Name, Ip, Udp, Timeout, RetryCount, TagList, ParamsName}
wr_addr(Dir, SortedTraps, Addresses) ->
    {ok, Fid} = file:open(filename:join(Dir,"target_addr.conf"),write),
    ok = io:format(Fid, "~s\n", [trap_header()]),
    lists:foreach(fun({{Ip, Vsn}, Communities}) ->
			  {UDP, MMS} = get_udp_mms(Ip, Addresses),
			  AA = mk_ip(Ip, Vsn),
			  BB = mk_tag_list(Communities, []),
			  ok = io:format(Fid, 
					 "{\"~s\", ~w, ~w, 1500, 3, "
					 "\"~s\", \"~s\", \"dummy\", "
					 "[],~w}.\n",
					 [AA, Ip, UDP, BB, 
					  mk_param(Vsn), MMS])
		  end,
		  SortedTraps),
    file:close(Fid).

get_udp_mms(Ip, Addresses) ->
    case lists:keysearch(Ip, 1, Addresses) of
	{value, {_, UDP, MMS, _}} ->
	    {UDP, MMS};
	false ->
	    Str = io_lib:format("No UDP port found for ~w in address.conf",
				[Ip]),
	    throw({error, {no_udp_port, Ip}, Str})
    end.

mk_ip([A,B,C,D], Ver) ->
    io_lib:format("~w.~w.~w.~w v~w", [A,B,C,D,Ver]).

mk_param(1) -> "otp_v1";
mk_param(2) -> "otp_v2".
     
mk_tag_list([Community | T], Res) ->
    mk_tag_list(T, [32 | mk_tag(Community)] ++  Res);
mk_tag_list([], []) ->
    [];
mk_tag_list([], Res) ->
    tl(Res).

mk_tag([32 | T]) -> [$_ | mk_tag(T)];
mk_tag([9 | T]) -> [$_ | mk_tag(T)];
mk_tag([13 | T]) -> [$_ | mk_tag(T)];
mk_tag([11 | T]) -> [$_ | mk_tag(T)];
mk_tag([Char | T]) -> [Char | mk_tag(T)];
mk_tag([]) -> [].

wr_params(Dir, Vers) ->
    {ok, Fid} = file:open(filename:join(Dir,"target_params.conf"),write),
    ok = io:format(Fid, "~s\n", [trap_header()]),
    lists:foreach(fun(Ver) ->
			  {MM,SM} = if Ver == 1 -> {v1,v1};
				       Ver == 2 -> {v2c,v2c};
				       Ver == 3 -> {v3,usm}
			       end,
			  ok = io:format(Fid, "{\"otp_v~w\", ~w, ~w, "
					 "\"\", noAuthNoPriv}.\n",
					 [Ver, MM, SM])
		  end,
		  Vers),
    file:close(Fid).

wr_notify(Dir, Communities) ->
    {ok, Fid} = file:open(filename:join(Dir,"notify.conf"),write),
    ok = io:format(Fid, "~s\n", [trap_header()]),
    lists:foreach(fun(Community) ->
			  ok = io:format(Fid, "{\"~s\", \"~s\", trap}.\n",
					 [Community, mk_tag(Community)])
		  end,
		  Communities),
    file:close(Fid).

trap_header() ->
    {Y,Mo,D} = date(),
    {H,Mi,S} = time(),
    io_lib:format("%% This file was automatically converted from old style\n"
		  "%% trap_dest.conf by snmp_config v~s ~w-~2.2.0w-~2.2.0w "
		  "~2.2.0w:~2.2.0w:~2.2.0w\n",
		  [?version,Y,Mo,D,H,Mi,S]).

%%-----------------------------------------------------------------
%% Convert old target_params.conf to new target_params.conf
%% Just chnage otp_sec security model, leave the rest intact.
%%-----------------------------------------------------------------
convert_target_params(Dir) ->
    error_logger:info_msg("snmp: Converting target_params.conf file\n"
			  "Saving old file in target_params.old.\n", []),
    case catch c_target_params(Dir) of
	ok ->
	    ok;
	Else ->
	    FName1 = filename:join(Dir, "target_params.conf"),
	    FName2 = filename:join(Dir, "target_params.old"),
	    file:rename(FName2, FName1),
	    {Reason, Str} = case Else of
				{error, R, S} -> {R, S};
				_ -> {Else, io_lib:format("~p", [Else])}
			    end,
	    error_logger:error_msg("snmp: conversion failed. ~s\n", [Str]),
	    exit({conversion_failed, Reason})
    end.

c_target_params(Dir) ->
    FName1 = filename:join(Dir, "target_params.conf"),
    FName2 = filename:join(Dir, "target_params.old"),
    Params = snmp_misc:read(FName1, {snmp_conf, check_old_params}),
    file:rename(FName1, FName2),

    {ok, Fid} = file:open(FName1, write),
    ok = io:format(Fid, "~s\n", [params_header()]),

    lists:foreach(fun({Name, MPModel, SecModel, SecName, SecLevel}) ->
			  SecM = tr_sec_model(SecModel, MPModel),
			  ok = io:format(Fid, "{\"~s\", ~w, ~w, \"~s\", ~w}.\n",
					 [Name,MPModel,SecM,SecName,SecLevel])
		  end, Params),
    ok.

tr_sec_model(otp_sec, v1) -> v1;
tr_sec_model(otp_sec, v2c) -> v2c;
tr_sec_model(Any, _) -> Any.

%%-----------------------------------------------------------------
%%  TargetParams
%%  {Name, MPModel, SecurityModel, SecurityName, SecurityLevel}
%%-----------------------------------------------------------------
check_old_params({Name, MPModel, SecModel, SecName, SecLevel}) ->
    check_string(Name),
    {ok, _MP} = check_atom(MPModel, [{v1, 0},
				     {v2c, 1}]),
    {ok, _SecM} = check_atom(SecModel, [{v1, 1},
					{v2c, 2},
					{otp_sec, undefined}]),
    check_string(SecName),
    {ok, _SecL} = check_atom(SecLevel, [{noAuthNoPriv, 1},
					{authNoPriv, 2},
					{authPriv, 3}]),
    {ok, {Name, MPModel, SecModel, SecName, SecLevel}};
check_old_params(X) ->
    {invalid_target_params, X}.

params_header() ->
    {Y,Mo,D} = date(),
    {H,Mi,S} = time(),
    io_lib:format("%% This file was automatically converted from old style\n"
		  "%% target_params.conf by snmp_config v~s ~w-~2.2.0w-~2.2.0w "
		  "~2.2.0w:~2.2.0w:~2.2.0w\n",
		  [?version,Y,Mo,D,H,Mi,S]).

%%-----------------------------------------------------------------
%% Generate usm.conf
%%-----------------------------------------------------------------
generate_usm(Dir) ->
    error_logger:info_msg("snmp: Incomplete configuration. Generating "
			  "empty usm.conf.\n", []),
    USMFile = filename:join(Dir, "usm.conf"),
    ok = file:write_file(USMFile, list_to_binary([])).


%%-----------------------------------------------------------------
%% Convert old view.conf to new vacm.conf
%%-----------------------------------------------------------------
convert_view(Dir) ->
    error_logger:info_msg("snmp: Converting old style view.conf file "
			  "to new vacm file.\nSaving old community.conf "
			  "in community.old\n", []),
    case catch c_view(Dir) of
	ok ->
	    ok;
	Else ->
	    Cf = filename:join(Dir, "community.conf"),
	    Cf2 = filename:join(Dir, "community.old"),
	    file:delete(filename:join(Dir, "vacm.conf")),
	    file:rename(Cf2, Cf),
	    {Reason, Str} = case Else of
				{error, R, S} -> {R, S};
				_ -> {Else, io_lib:format("~p", [Else])}
			    end,
	    error_logger:error_msg("snmp: conversion failed. ~s\n"
				   "Restoring community.conf.\n", [Str]),
	    exit({conversion_failed, Reason})
    end.

c_view(Dir) ->
    Views = snmp_misc:read(filename:join(Dir, "view.conf"), 
			   {snmp_conf, check_view}),
    Cf = filename:join(Dir, "community.conf"),
    Cf2 = filename:join(Dir, "community.old"),
    Communities = snmp_misc:read(Cf, {snmp_conf, check_old_community}),
    file:rename(Cf, Cf2),
    wr_vacm(Dir, Views, Communities).
	

wr_vacm(Dir, Views, Communities) ->
    {ok, Fid} = file:open(filename:join(Dir,"vacm.conf"),write),
    {ok, Fid2} = file:open(filename:join(Dir,"community.conf"),write),
    ok = io:format(Fid2, "~s\n", [community_header()]),
    Groups = mk_groups(Communities),
    lists:foreach(fun({Community, _VA}) ->
			  ok =
			      io:format(Fid2,
				"{\"~s\", \"~s\", \"~s\", \"\", \"\"}.\n",
					[Community, Community, Community])
		  end, Groups),
    ok = io:format(Fid, "~s\n", [vacm_header()]),
    lists:foreach(fun({Community, _VA}) ->
			  ok = io:format(Fid, 
					 "{vacmSecurityToGroup, "
					 "v1, \"~s\", \"~s\"}.\n",
					 [Community, Community]),
			  ok = io:format(Fid, 
					 "{vacmSecurityToGroup, "
					 "v2c, \"~s\", \"~s\"}.\n",
					 [Community, Community])
		  end, Groups),
    ok = io:format(Fid, "\n", []),
    lists:foreach(fun({Community, VA}) ->
			  ok = io:format(Fid, 
					 "{vacmAccess, \"~s\", \"\", any,"
					 "noAuthNoPriv, prefix, "
					 "\"~s\", \"~s\", \"~s\"}.\n",
					 [Community,va_r(VA),va_w(VA),va_r(VA)])
		  end, Groups),
    ok = io:format(Fid, "\n", []),
    ok = io:format(Fid, "{vacmViewTreeFamily, \"internet\", [1,3,6,1], "
		   "included, null}.\n", []),
    lists:foreach(fun({Index, Subtree, Mask, Type, _Storage, _Status}) ->
			  ok = io:format(Fid,
					 "{vacmViewTreeFamily, \"view~w\", "
					 "~w, ~w, ~w}.\n",
					 [Index, Subtree, mk_type(Type),
					  mk_mask(Mask)])
		  end,
		  Views),
    file:close(Fid),
    file:close(Fid2).
		   
mk_type(?view_included) -> included;
mk_type(?view_excluded) -> excluded.

mk_mask([]) -> null;
mk_mask(X) -> X.

va_r({global, _}) -> "internet";
va_r({Idx, _}) -> io_lib:format("view~w", [Idx]).

va_w({_, ?intCommunityAccess_read}) -> "";
va_w({global, _}) -> "internet";
va_w({Idx, _}) -> io_lib:format("view~w", [Idx]).

%% In: [{ManagerIpAddress, CommunityString, ViewIndex, Access}]
%% Ret: [{CommunityStr, {ViewIndex, Access}}]
mk_groups([]) -> [];
mk_groups([{_Ip, Community, ViewIndex, Access, _} | T]) ->
    %% Sort and collect the list into [{Comm, [{View, Access}]}]
    Cs = sort_comms(T, Community, [{ViewIndex, Access}], []),
    %% If there are more than one View for a Community, print a warning,
    %% and extend the view to the global view.
    lists:map(fun({C, VAList}) -> {C, check_views(VAList, C)} end, Cs).

sort_comms([{_Ip, Comm, ViewIndex, Access, _} | T], Comm, Cs, Res) ->
    sort_comms(T, Comm, [{ViewIndex, Access} | Cs], Res);
sort_comms([{_Ip, Comm, ViewIndex, Access, _} | T], Cur, Cs, Res) ->
    sort_comms(T, Comm, [{ViewIndex, Access}], [{Cur, Cs} | Res]);
sort_comms([], Cur, Cs, Res) ->
    [{Cur, Cs} | Res].

check_views([{ViewIndex, Access} | T], Comm) ->
    check_views(T, ViewIndex, Access, Comm).
check_views([{ViewIndex, Access} | T], CurView, MaxAccess, Comm) ->
    if
	ViewIndex == CurView ->
	    check_views(T, CurView, max_acc(Access, MaxAccess), Comm);
	CurView == global ->
	    check_views(T, CurView, max_acc(Access, MaxAccess), Comm);
	true ->
	    error_logger:error_msg("snmp: view conversion found conflicting "
				   "views for community ~p.\nWARNING: using "
				   "internet view for this community!  Check "
				   "the configuration!\n", [Comm]),
	    check_views(T, global, max_acc(Access, MaxAccess), Comm)
    end;
check_views([], CurView, MaxAccess, _Comm) ->
    {CurView, MaxAccess}.

max_acc(?intCommunityAccess_readWrite, _) -> ?intCommunityAccess_readWrite;
max_acc(_, Max) -> Max.
    

%%-----------------------------------------------------------------
%%  View
%%  {viewIndex, viewSubtree, viewType, viewMask}.
%%-----------------------------------------------------------------
check_view({ViewIndex, Subtree, Type, Mask}) ->
    check_integer(ViewIndex),
    check_oid(Subtree),
    {ok, TypeVal} =
	check_atom(Type, [{included, ?view_included},
			  {excluded, ?view_excluded}]),
    MaskVal =
	case catch check_atom(Mask, [{null, []}]) of
	    {ok, X} ->
		X;
	    _  -> 
		check_oid(Mask),
		Mask
	end,
    {ok, {ViewIndex, Subtree, MaskVal,
	  TypeVal, ?'StorageType_nonVolatile', ?'RowStatus_active'}};
check_view(X) ->
    {invalid_view, X}.

%%-----------------------------------------------------------------
%%  Community
%%  {ManagerIpAddress, CommunityString, ViewIndex, Access}.
%%-----------------------------------------------------------------
check_old_community({Ip, CommunityString, ViewIndex, Access}) ->
    check_ip(Ip),
    check_string(CommunityString),
    check_integer(ViewIndex),
    {ok, AccessVal} =
	check_atom(Access, [{read, ?intCommunityAccess_read},
			    {readWrite, ?intCommunityAccess_readWrite}]),
    {ok, {Ip, CommunityString, ViewIndex, AccessVal, ?'RowStatus_active'}};
check_old_community(X) ->
    {invalid_community, X}.

%%-----------------------------------------------------------------
%%  Address
%%  {ManagerIpAddress, UDPPort, MaxPacketSize}.
%%-----------------------------------------------------------------
check_address({Ip, UDP, PacketSize}) ->
    check_ip(Ip),
    check_integer(UDP),
    check_packet_size(PacketSize),
    {ok, {Ip, UDP, PacketSize, ?'RowStatus_active'}};
check_address(X) ->
    {invalid_address, X}.


vacm_header() ->
    {Y,Mo,D} = date(),
    {H,Mi,S} = time(),
    io_lib:format("%% This file was automatically converted from old style\n"
		  "%% view.conf and community.conf by "
		  "snmp_config v~s ~w-~2.2.0w-~2.2.0w "
		  "~2.2.0w:~2.2.0w:~2.2.0w\n",
		  [?version,Y,Mo,D,H,Mi,S]).

community_header() ->
    {Y,Mo,D} = date(),
    {H,Mi,S} = time(),
    io_lib:format("%% This file was automatically converted from old style\n"
		  "%% community.conf by snmp_config v~s ~w-~2.2.0w-~2.2.0w "
		  "~2.2.0w:~2.2.0w:~2.2.0w\n",
		  [?version,Y,Mo,D,H,Mi,S]).

%%-----------------------------------------------------------------
%% Generate a context.conf file.
%%-----------------------------------------------------------------
convert_context(Dir) ->
    error_logger:info_msg("snmp: Missing context.conf file. Generating a "
			  "default file.\n", []),
    case catch c_context(Dir) of
	ok ->
	    ok;
	Else ->
	    file:delete(filename:join(Dir, "context.conf")),
	    {Reason, Str} = case Else of
				{error, R, S} -> {R, S};
				_ -> {Else, io_lib:format("~p", [Else])}
			    end,
	    error_logger:error_msg("snmp: conversion failed. ~s\n", [Str]),
	    exit({conversion_failed, Reason})
    end.

c_context(Dir) ->
    {ok, Fid} = file:open(filename:join(Dir,"context.conf"),write),
    ok = io:format(Fid, "~s\n", [context_header()]),
    ok = io:format(Fid, "%% The default context\n\"\".\n", []),
    file:close(Fid).


context_header() ->
    {Y,Mo,D} = date(),
    {H,Mi,S} = time(),
    io_lib:format("%% This file was automatically generated by "
		  "snmp_config v~s  ~w-~2.2.0w-~2.2.0w "
		  "~2.2.0w:~2.2.0w:~2.2.0w\n",
		  [?version,Y,Mo,D,H,Mi,S]).


config_err(F, A) ->
    snmp_error_report:config_err(F, A).
