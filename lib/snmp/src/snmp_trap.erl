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
-module(snmp_trap).

%%%-----------------------------------------------------------------
%%% This module takes care of all trap handling.
%%%-----------------------------------------------------------------
%% External exports
-export([construct_trap/2, try_initialise_vars/2, send_trap/6]).

%% Internal exports
-export([alias_to_oid/1, make_varbind/1, init_v2_inform/9, init_v3_inform/9]).

-include("snmp_types.hrl").
-include("SNMPv2-MIB.hrl").
-include("SNMP-FRAMEWORK-MIB.hrl").
-define(enterpriseSpecific, 6).


-define(VMODULE,"TRAP").
-include("snmp_verbosity.hrl").


%%-----------------------------------------------------------------
%% Trap mechanism
%% ==============
%% Distributed subagent (dSA) case
%%   The MIB with the TRAP-TYPE macro is loaded in dSA.  This means
%%   that dSA has info on all variables defined in the TRAP-TYPE,
%%   even though some variables may be located in other SA:s (or
%%   in the MA). Other variables that may be sent in the trap, 
%%   must be known by either the dSA, or some of its parent agents
%%   (e.g. the master agent), if the variable should be referred
%%   to by symbolic name. It is however possible to send other
%%   variables as well, but then the entire OID must be provided.
%%   The dSA locates the asn1 type, oid and value for as many
%%   variables as possible. This information, together with the
%%   variables for which the type, value or oid isn't known, is
%%   sent to the dSA's parent. This agent performs the same
%%   operation, and so on, until eventually the MA will receive the
%%   info. The MA then fills in the gaps, and at this point all
%%   oids and types must be known, otherwise an error is signalled,
%%   and the opertaion is aborted. For the unknown values for some 
%%   oids, a get-operation is performed by the MA. This will
%%   retreive the missing values.
%%   At this point, all oid, types and values are known, so the MA
%%   can distribute the traps according to the information in the
%%   internal tables.
%% 
%% Local subagent (lSA) case
%%   This case is similar to the case above.
%%
%% Master agent (MA) case
%%   This case is similar to the case above.
%%
%% NOTE: All trap forwarding between agents is made asynchronously.
%%
%% dSA: Distributed SA  (the #trap is loaded here)
%% nSA: [many] SAs between dSA and MA
%% MA:  Master Agent. (all trap info (destiniations is here))
%% 1) application decides to send a trap.
%% 2) dSA calls send_trap which initialises vars
%% 3) dSA sends all to nSA
%% 4) nSA tries to map symbolic names to oids and find the types
%%    of all variableoids with a value (and no type).
%% 5) nSA sends all to (n-1)SA
%% 6) MA tries to initialise vars
%% 7) MA makes a trappdu, and sends it to all destination.
%%
%% Problems with this implementation
%% =================================
%% It's ok to send {Oid, Value} but not just Oid. (it should be for
%%   any Oid)
%% It's ok to send {Name, Value} but not just Name. (it should be
%%   for Names in the hierarchy)
%% This approach might be too flexible; will people use it?
%% *NOTE*
%% Therefore, in this version we *do not* allow extra variables
%% in traps.
%% *YES* In _this_ version we do.
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: construct_trap/2
%% Args: Trap is an atom
%%       Varbinds is a list of {Variable, Value},
%%         where Variable is an atom or an OID. |
%%         {SymbolicTableCol, RowIndex, Value},
%%         where RowIndex is the indexes for the row.
%%         We don't check the RowIndex.
%% Purpose: This is the initially-called function. It is called
%%          by the agent that found out that a trap should be
%%          sent.
%%          Initialize as many variables as possible.
%% Returns: {ok, TrapRecord, <list of Var>} | error
%%          where Var is returned from initiate_vars.
%% NOTE: Executed at the inital SA
%%-----------------------------------------------------------------
construct_trap(Trap, Varbinds) ->
    case snmp_symbolic_store:get_notification(Trap) of
	undefined -> 
	    snmp_error:user_err("send_trap got undef Trap: ~p" , [Trap]),
	    error;
	{value, TRec} when record(TRec, trap) ->
	    ListOfVars = TRec#trap.oidobjects,
	    OidVbs = lists:map({snmp_trap, alias_to_oid}, [], Varbinds),
	    LV = initiate_vars(ListOfVars, OidVbs),
	    {ok, TRec, try_initialise_vars(get(mibserver), LV)};
	{value, NRec} when record(NRec, notification) ->
	    ListOfVars = NRec#notification.oidobjects,
	    OidVbs = lists:map({snmp_trap, alias_to_oid}, [], Varbinds),
	    LV = initiate_vars(ListOfVars, OidVbs),
	    {ok, NRec, try_initialise_vars(get(mibserver), LV)}
    end.

alias_to_oid({Alias, Val}) when atom(Alias) ->
    case snmp_symbolic_store:aliasname_to_oid(Alias) of
	{value, Oid} -> {lists:append(Oid, [0]), {value, Val}};
	_ ->   	     {Alias, {value, Val}}
    end;
alias_to_oid({Alias, RowIndex, Val}) when atom(Alias) ->
    case snmp_symbolic_store:aliasname_to_oid(Alias) of
	{value, Oid} -> {lists:append(Oid, RowIndex), {value, Val}};
	_ ->   	     {Alias, RowIndex, {value, Val}}
    end;
alias_to_oid({Oid, Val}) -> {Oid, {value, Val}}.

%%-----------------------------------------------------------------
%% Func: initiate_vars/2
%% Args: ListOfVars is a list of {Oid, #asn1_type}
%%       Varbinds is a list of 
%%          {VariableOid, Value} | 
%%          {VariableAtom, Value} |
%%          {TableColAtom, RowIndex, Value}
%% Purpose: For each variable in specified in the TRAP-TYPE macro
%%          (each in ListOfVars), check if it's got a value given
%%          in the Varbinds list.
%%          For each Oid:
%%            1) It has corresponding VariableOid. Use Value.
%%            2) No corresponding VariableOid. No value.
%% Returns: A list of
%%            {VariableOid, #asn1_type, Value} |
%%            {VariableOid, #asn1_type} |
%%            {VariableOid, Value} |
%%            {VariableAtom, Value} |
%%            {TableColAtom, RowIndex, Value}
%% NOTE: Executed at the inital SA
%%-----------------------------------------------------------------
initiate_vars([{Oid, Asn1Type} | T], Varbinds) ->
    case delete_oid_from_varbinds(Oid, Varbinds) of
	{undefined, _, _} ->
	    [{Oid, Asn1Type} | initiate_vars(T, Varbinds)];
	{Value, VarOid, RestOfVarbinds} ->
	    [{VarOid, Asn1Type, Value} | initiate_vars(T, RestOfVarbinds)]
    end;
initiate_vars([], Varbinds) ->
    Varbinds.
    
delete_oid_from_varbinds(Oid, [{VarOid, Value} | T]) ->
    case lists:prefix(Oid, VarOid) of
	true -> 
	    {Value, VarOid, T};
	_ -> 
	    {Value2, VarOid2, T2} = delete_oid_from_varbinds(Oid, T),
	    {Value2, VarOid2, [{VarOid, Value} | T2]}
    end;
delete_oid_from_varbinds(Oid, [H | T]) ->
    {Value, VarOid, T2} = delete_oid_from_varbinds(Oid, T),
    {Value, VarOid, [H | T2]};
delete_oid_from_varbinds(_Oid, []) -> {undefined, undefined, []}.

%%-----------------------------------------------------------------
%% Func: try_initialise_vars(Mib, Varbinds)
%% Args: Mib is the local mib process
%%       Varbinds is a list returned from initiate_vars.
%% Purpose: Try to initialise uninitialised vars.
%% Returns: see initiate_vars
%% NOTE: Executed at the intermediate SAs
%%-----------------------------------------------------------------
try_initialise_vars(Mib, Varbinds) ->
    V = try_map_symbolic(Varbinds),
    try_find_type(V, Mib).

%%-----------------------------------------------------------------
%% Func: try_map_symbolic/1
%% Args: Varbinds is a list returned from initiate_vars.
%% Purpose: Try to map symbolic name to oid for the 
%%          symbolic names left in the Varbinds list.
%% Returns: see initiate_vars.
%% NOTE: Executed at the intermediate SAs
%%-----------------------------------------------------------------
try_map_symbolic([Varbind | Varbinds]) ->
    [localise_oid(Varbind) | try_map_symbolic(Varbinds)];
try_map_symbolic([]) -> [].

localise_oid({VariableName, Value}) when atom(VariableName) ->
    alias_to_oid({VariableName, Value});
localise_oid({VariableName, RowIndex, Value}) when atom(VariableName) ->
    alias_to_oid({VariableName, RowIndex, Value});
localise_oid(X) -> X.

%%-----------------------------------------------------------------
%% Func: try_find_type/2
%% Args: Varbinds is a list returned from initiate_vars.
%%       Mib is a ref to the Mib process corresponding to
%%         this agent.
%% Purpose: Try to find the type for each variableoid with a value
%%          but no type.
%% Returns: see initiate_vars.
%% NOTE: Executed at the intermediate SAs
%%-----------------------------------------------------------------
try_find_type([Varbind | Varbinds], Mib) ->
    [localise_type(Varbind, Mib) | try_find_type(Varbinds, Mib)];
try_find_type([], _) -> [].

localise_type({VariableOid, Type}, Mib) 
  when list(VariableOid), record(Type, asn1_type) ->
    {VariableOid, Type};
localise_type({VariableOid, Value}, Mib) when list(VariableOid) ->
    case snmp_mib:lookup(Mib, VariableOid) of
	{variable, ME} ->
	    {VariableOid, ME#me.asn1_type, Value};
	{table_column, ME, _} ->
	    {VariableOid, ME#me.asn1_type, Value};
	_ ->
	    {VariableOid, Value}
    end;
localise_type(X, _) -> X.

%%-----------------------------------------------------------------
%% Func: make_v1_trap_pdu/4
%% Args: Enterprise = oid()
%%       Specific = integer()
%%       Varbinds is as returned from initiate_vars
%%         (but only {Oid, Type[, Value} permitted)
%%       SysUpTime = integer()
%% Purpose: Make a #trappdu
%%          Checks the Varbinds to see that no symbolic names are
%%          present, and that each var has a type. Performs a get
%%          to find any missing value.
%% Returns: {#trappdu, [byte()] | error
%% Fails: yes
%% NOTE: Executed at the MA
%%-----------------------------------------------------------------
make_v1_trap_pdu(Enterprise, Specific, VarbindList, SysUpTime) ->
    case Enterprise of
	?snmp ->
	    Enterp = sys_object_id(),
	    Generic = Specific,
	    Spec = 0;
	_ ->
	    Enterp = Enterprise,
	    Generic = ?enterpriseSpecific,
	    Spec = Specific
    end,
    {value, AgentIp} = snmp_framework_mib:intAgentIpAddress(get),
    #trappdu{enterprise = Enterp,
	     agent_addr = AgentIp,
	     generic_trap = Generic,
	     specific_trap = Spec,
	     time_stamp = SysUpTime,
	     varbinds = VarbindList}.

make_v2_notif_pdu(Vbs, Type) ->
    #pdu{type = Type,
	 request_id = snmp_mpd:generate_req_id(),
	 error_status = noError,
	 error_index = 0,
	 varbinds = Vbs}.

make_varbind_list(Varbinds) ->
    {VariablesWithValueAndType, VariablesWithType} =
	split_variables(order(Varbinds)),
    V = get_all(VariablesWithType),
    Vars = lists:append([V, VariablesWithValueAndType]),
    lists:map({snmp_trap, make_varbind}, [], unorder(lists:keysort(1, Vars))).

%%-----------------------------------------------------------------
%% Func: send_trap/6
%% Args: TrapRec = #trap | #notification
%%       NotifyName = string()
%%       ContextName = string()
%%       Recv = no_receiver | {Ref, Receiver}
%%       Receiver = pid() | atom() | {M,F,A}
%%       Vbs = [varbind()]
%%       NetIf = pid()
%% Purpose: Default trap sending function.
%%          Sends the trap to the targets pointed out by NotifyName.
%%          If NotifyName is ""; the normal procedure defined in 
%%          SNMP-NOTIFICATION-MIB is used, i.e. the trap is sent to
%%          all managers.
%%          Otherwise, the NotifyName is used to find an entry in the
%%          SnmpNotifyTable which define how to send the notification
%%          (as an Inform or a Trap), and to select targets from
%%          SnmpTargetAddrTable (using the Tag).
%%-----------------------------------------------------------------
send_trap(TrapRec, NotifyName, ContextName, Recv, Vbs, NetIf) ->
    VarbindList = make_varbind_list(Vbs),
    Dests = find_dests(NotifyName),
    send_trap_pdus(Dests, ContextName, {TrapRec, VarbindList}, [], [], [],
		   Recv, NetIf).
	    
get_all(VariablesWithType) ->
    {Order, Varbinds} = extract_order(VariablesWithType, 1),
    case snmp_agent:do_get(snmp_acm:get_root_mib_view(), Varbinds, true) of
	{noError, _, NewVarbinds} ->
	    contract_order(Order, NewVarbinds);
	{ErrorStatus, ErrorIndex, _} ->
	    snmp_error:user_err("snmp_trap: get operation failed "
				"{~p, ~p}~n  in ~p",
				[ErrorStatus, ErrorIndex, Varbinds]),
	    throw(error)
    end.
    
make_varbind(Varbind) when record(Varbind, varbind) ->
    Varbind;
make_varbind({VarOid, ASN1Type, Value}) ->
    case snmp_agent:make_value_a_correct_value(Value, ASN1Type, undef) of
	{value, Type, Val} ->
	    #varbind{oid = VarOid, variabletype = Type, value = Val};
	{error, Reason} -> 
	    snmp_error:user_err("snmp_trap: Invalid value: Oid ~p, Val:~p, ~p",
				[VarOid, Value, Reason]),
	    throw(error)
    end.

order(Varbinds) -> order(Varbinds, 1).
order([H | T], No) -> [{No, H} | order(T, No + 1)];
order([], _) -> [].

unorder([{No, H} | T]) -> [H | unorder(T)];
unorder([]) -> [].

extract_order([{No, {VarOid, _Type}} | T], Index) ->
    {Order, V} = extract_order(T, Index+1),
    {[No | Order], [#varbind{oid = VarOid, org_index = Index} | V]};
extract_order([], _) -> {[], []}.

contract_order([No | Order], [Varbind | T]) ->
    [{No, Varbind} | contract_order(Order, T)];
contract_order([], []) -> [].

split_variables([{No, {VarOid, Type, Val}} | T]) when list(VarOid) ->
    {A, B} = split_variables(T),
    {[{No, {VarOid, Type, Val}} | A], B};
split_variables([{No, {VarOid, Type}} | T]) 
  when list(VarOid), record(Type, asn1_type) ->
    {A, B} = split_variables(T),
    {A, [{No, {VarOid, Type}} | B]};
split_variables([{No, {VarName, Value}} | T]) ->
    snmp_error:user_err("snmp_trap: Undefined variable ~p (~p)",
			[VarName, Value]),
    throw(error);
split_variables([{No, {VarName, RowIndex, Value}} | T]) ->
    snmp_error:user_err("snmp_trap: Undefined variable ~p ~w (~p)",
			[VarName, RowIndex, Value]),
    throw(error);
split_variables([]) -> {[], []}.

%%-----------------------------------------------------------------
%% Func: find_dests(NotifyName) -> 
%%          [{DestAddr, TargetName, TargetParams, NotifyType}]
%% Types: NotifyType = string()
%%        DestAddr = {TDomain, TAddr}
%%        TargetName = string()
%%        TargetParams = {MpModel, SecModel, SecName, SecLevel}
%%        NotifyType = trap | {inform, Timeout, Retry}
%% Returns: A list of all Destination addresses for this community.
%% NOTE: This function is executed in the master agent's context
%%-----------------------------------------------------------------
find_dests("") ->
    snmp_notification_mib:get_targets();
find_dests(NotifyName) ->
    case snmp_notification_mib:get_targets(NotifyName) of
	[] ->
	    ?vlog("No dests found for snmpNotifyName: ~p",[NotifyName]),
	    [];
	Dests ->
	    Dests
    end.

%%-----------------------------------------------------------------
%% NOTE: This function is executed in the master agent's context
%% For each target, check if it has access to the objects in the
%% notification, determine which message version (v1, v2c or v3)
%% should be used for the target, and determine the message
%% specific parameters to be used.
%%-----------------------------------------------------------------
send_trap_pdus([{DestAddr, TargetName, {MpModel, SecModel, SecName, SecLevel},
		 Type} | T],
	       ContextName,{TrapRec, Vbs}, V1Res, V2Res, V3Res, Recv, NetIf) ->
    ?vdebug("send trap pdus: "
	    "~n   Destination address: ~p~n"
	    "~n   Target name:         ~p~n"
	    "~n   MP model:            ~p~n"
	    "~n   Type:                ~p",
	    [DestAddr,TargetName,MpModel,Type]),
    case snmp_vacm:get_mib_view(notify, SecModel, SecName, SecLevel,
				ContextName) of
	{ok, MibView} ->
	    case check_all_varbinds(TrapRec, Vbs, MibView) of
		true when MpModel == ?MP_V1 ->
		    ContextEngineId = snmp_framework_mib:get_engine_id(),
		    case snmp_community_mib:vacm2community({SecName,
							    ContextEngineId,
							    ContextName},
							   DestAddr) of
			{ok, Community} ->
			    send_trap_pdus(T, ContextName, {TrapRec, Vbs},
					   [{DestAddr, Community} | V1Res],
					   V2Res, V3Res, Recv, NetIf);
			undefined ->
			    ?vlog("No community found for v1 dest: ~p", 
				  [element(2, DestAddr)]),
			    send_trap_pdus(T, ContextName, {TrapRec, Vbs},
					   V1Res, V2Res, V3Res, Recv, NetIf)
		    end;
		true when MpModel == ?MP_V2C ->
		    ContextEngineId = snmp_framework_mib:get_engine_id(),
		    case snmp_community_mib:vacm2community({SecName,
							    ContextEngineId,
							    ContextName},
							   DestAddr) of
			{ok, Community} ->
			    send_trap_pdus(T, ContextName, {TrapRec, Vbs},
					   V1Res,
					   [{DestAddr, Community, Type}|V2Res],
					   V3Res, Recv, NetIf);
			undefined ->
			    ?vlog("No community found for v2c dest: ~p\n", 
				  [element(2, DestAddr)]),
			    send_trap_pdus(T, ContextName, {TrapRec, Vbs},
					   V1Res, V2Res, V3Res, Recv, NetIf)
		    end;
		true when MpModel == ?MP_V3 ->
		    SecLevelF = mk_flag(SecLevel),
		    MsgData = {SecModel, SecName, SecLevelF, TargetName},
		    send_trap_pdus(T, ContextName, {TrapRec, Vbs},
				   V1Res, V2Res,
				   [{DestAddr, MsgData, Type} | V3Res],
				   Recv, NetIf);
		true ->
		    ?vlog("bad MpModel ~p for dest: ~p.~n",
			  [MpModel, element(2, DestAddr)]),
		    send_trap_pdus(T, ContextName, {TrapRec, Vbs},
				   V1Res, V2Res, V3Res, Recv, NetIf);
		_ ->
		    ?vlog("no access for dest: ~p in target ~p.~n",
			  [element(2, DestAddr), TargetName]),
		    send_trap_pdus(T, ContextName, {TrapRec, Vbs},
				   V1Res, V2Res, V3Res, Recv, NetIf)
	    end;
	{discarded, Reason} ->
	    ?vlog("mib view error ~p for"
		  "~n   dest:    ~p"
		  "~n   secName: ~w", 
		  [Reason, element(2, DestAddr), SecName]),
	    send_trap_pdus(T, ContextName, {TrapRec, Vbs},
			   V1Res, V2Res, V3Res, Recv, NetIf)
    end;
send_trap_pdus([], ContextName, {TrapRec, Vbs}, V1Res, V2Res, V3Res,
	       Recv, NetIf) ->
    SysUpTime = snmp_standard_mib:sys_up_time(),
    ?vdebug("send trap pdus with sysUpTime ~p",[SysUpTime]),
    send_v1_trap(TrapRec, V1Res, Vbs, NetIf, SysUpTime),
    send_v2_trap(TrapRec, V2Res, Vbs, Recv, NetIf, SysUpTime),
    send_v3_trap(TrapRec, V3Res, Vbs, Recv, NetIf, SysUpTime, ContextName).

send_v1_trap(_TrapRec, [], _Vbs, _NetIf, _SysUpTime) ->
    ok;
send_v1_trap(#trap{enterpriseoid = Enter, specificcode = Spec},
	     V1Res, Vbs, NetIf, SysUpTime) ->
    ?vdebug("prepare to send v1 trap "
	    "~n   '~p'"
	    "~n   with ~p",[Enter,Spec]),
    TrapPdu = make_v1_trap_pdu(Enter, Spec, Vbs, SysUpTime),
    AddrCommunities = mk_addr_communities(V1Res),
    lists:foreach(fun({Community, Addrs}) ->
			  ?vtrace("send v1 trap pdu to ~p",[Addrs]),
			  NetIf ! {send_pdu, 'version-1', TrapPdu,
				   {community, Community}, Addrs}
		  end, AddrCommunities);
send_v1_trap(#notification{oid = Oid}, V1Res, Vbs, NetIf, SysUpTime) ->
    %% Use alg. in rfc2089 to map a v2 trap to a v1 trap
    % delete Counter64 objects from vbs
    ?vdebug("prepare to send v1 trap '~p'",[Oid]),
    NVbs = lists:filter(fun(Vb) when Vb#varbind.variabletype =/= 'Counter64' ->
				true;
			   (_) -> false
			end, Vbs),
    case Oid of
	[1,3,6,1,6,3,1,1,5,Specific] ->
	    Enter = ?snmp,
	    Spec = Specific - 1;
	_ ->
	    case lists:reverse(Oid) of
		[Last, 0 | First] ->
		    Enter = lists:reverse(First),
		    Spec = Last;
		[Last | First] ->
		    Enter = lists:reverse(First),
		    Spec = Last
	    end
    end,
    TrapPdu = make_v1_trap_pdu(Enter, Spec, NVbs, SysUpTime),
    AddrCommunities = mk_addr_communities(V1Res),
    lists:foreach(fun({Community, Addrs}) ->
			  ?vtrace("send v1 trap to ~p",[Addrs]),
			  NetIf ! {send_pdu, 'version-1', TrapPdu,
				   {community, Community}, Addrs}
		  end, AddrCommunities).
    
send_v2_trap(_TrapRec, [], _Vbs, _Recv, _NetIf, _SysUpTime) ->
    ok;
send_v2_trap(TrapRec, V2Res, Vbs, Recv, NetIf, SysUpTime) ->
    ?vdebug("prepare to send v2 trap",[]),
    {Oid, IVbs} = mk_v2_trap(TrapRec, Vbs, SysUpTime),
    TrapRecvs = get_trap_recvs(V2Res),
    InformRecvs = get_inform_recvs(V2Res),
    do_send_v2_trap(TrapRecvs, IVbs, NetIf),
    do_send_v2_inform(InformRecvs, IVbs, Recv, NetIf).
    
send_v3_trap(_TrapRec, [], _Vbs, _Recv, _NetIf, _SysUpTime, _ContextName) ->
    ok;
send_v3_trap(TrapRec, V3Res, Vbs, Recv, NetIf, SysUpTime, ContextName) ->
    ?vdebug("prepare to send v3 trap",[]),
    {Oid, IVbs} = mk_v2_trap(TrapRec, Vbs, SysUpTime), % v2 refers to SMIv2;
    TrapRecvs = get_trap_recvs(V3Res),                 % same SMI for v3
    InformRecvs = get_inform_recvs(V3Res),
    do_send_v3_trap(TrapRecvs, ContextName, IVbs, NetIf),
    do_send_v3_inform(InformRecvs, ContextName, IVbs, Recv, NetIf).
    

mk_v2_trap(#notification{oid = Oid}, Vbs, SysUpTime) ->
    ?vtrace("make v2 notification '~p'",[Oid]),
    mk_v2_notif(Oid, Vbs, SysUpTime);
mk_v2_trap(#trap{enterpriseoid = Enter, specificcode = Spec}, Vbs, SysUpTime) ->
    %% Use alg. in rfc1908 to map a v1 trap to a v2 trap
    ?vtrace("make v2 trap for '~p' with ~p",[Enter,Spec]),
    case Enter of
	?snmp ->
	    Oid = ?snmpTraps ++ [Spec + 1],
	    Enterp = sys_object_id();
	_ ->
	    Oid = Enter ++ [0, Spec],
	    Enterp = Enter
    end,
    ExtraVb = #varbind{oid = ?snmpTrapEnterprise_instance,
		       variabletype = 'OBJECT IDENTIFIER',
		       value = Enterp},
    mk_v2_notif(Oid, Vbs ++ [ExtraVb], SysUpTime).
    
mk_v2_notif(Oid, Vbs, SysUpTime) ->
    IVbs = [#varbind{oid = ?sysUpTime_instance,
		     variabletype = 'TimeTicks',
		     value = SysUpTime},
	    #varbind{oid = ?snmpTrapOID_instance,
		     variabletype = 'OBJECT IDENTIFIER',
		     value = Oid} | Vbs],
    {Oid, IVbs}.

%% Addr = {Domain, DomainAddr} ; e.g. {snmpUDPDomain, {IPasList, Udp}}
%% MsgData = CommunityString (v1, v2c) |
%%           {SecModel, SecName, SecLevel, TargetAddrName} (v3)
get_trap_recvs([{Addr, MsgData, trap} | T]) ->
    [{Addr, MsgData} | get_trap_recvs(T)];
get_trap_recvs([_ | T]) ->
    get_trap_recvs(T);
get_trap_recvs([]) ->
    [].

get_inform_recvs([{Addr, MsgData, {inform, Timeout, Retry}} | T]) ->
    [{Addr, MsgData, Timeout, Retry} | get_inform_recvs(T)];
get_inform_recvs([_ | T]) ->
    get_inform_recvs(T);
get_inform_recvs([]) ->
    [].

do_send_v2_trap([], _Vbs, _NetIf) ->
    ok;
do_send_v2_trap(Recvs, Vbs, NetIf) ->
    TrapPdu = make_v2_notif_pdu(Vbs, 'snmpv2-trap'),
    AddrCommunities = mk_addr_communities(Recvs),
    lists:foreach(fun({Community, Addrs}) ->
			  ?vtrace("~n   send v2 trap to ~p",[Addrs]),
			  NetIf ! {send_pdu, 'version-2', TrapPdu,
				   {community, Community}, Addrs}
		  end, AddrCommunities),
    ok.

do_send_v2_inform([], _Vbs, Recv, _NetIf) ->
    deliver_recv(Recv, snmp_targets, []);
do_send_v2_inform(Recvs, Vbs, Recv, NetIf) ->
    Targets = lists:map(fun({Addr, _Community, _Timeout, _Retry}) ->
				Addr
			end, Recvs),
    deliver_recv(Recv, snmp_targets, Targets),
    lists:foreach(
      fun({Addr, Community, Timeout, Retry}) ->
	      ?vtrace("~n   start inform sender to send v2 inform to ~p",
		      [Addr]),
	      proc_lib:spawn_link(?MODULE, init_v2_inform,
				  [Addr, Timeout, Retry, Vbs,
				   Recv, NetIf, Community,
				   get(verbosity),get(sname)])
      end, 
      Recvs).

do_send_v3_trap([], _ContextName, _Vbs, _NetIf) ->
    ok;
do_send_v3_trap(Recvs, ContextName, Vbs, NetIf) ->
    TrapPdu = make_v2_notif_pdu(Vbs, 'snmpv2-trap'), % Yes, v2
    ContextEngineId = snmp_framework_mib:get_engine_id(),
    lists:foreach(fun(Recv) ->
			  ?vtrace("~n   send v3 notif to ~p",[Recv]),
			  NetIf ! {send_pdu, 'version-3', TrapPdu,
				   {v3, ContextEngineId, ContextName}, [Recv]}
		  end, Recvs),
    ok.

do_send_v3_inform([], _ContextName, _Vbs, Recv, _NetIf) ->
    deliver_recv(Recv, snmp_targets, []);
do_send_v3_inform(Recvs, ContextName, Vbs, Recv, NetIf) ->
    Targets = lists:map(fun({Addr, _, _, _}) -> Addr end, Recvs),
    deliver_recv(Recv, snmp_targets, Targets),
    lists:foreach(
      fun({Addr, MsgData, Timeout, Retry}) ->
	      ?vtrace("~n   start inform sender to send v3 inform to ~p",
		      [Addr]),
	      proc_lib:spawn_link(?MODULE, init_v3_inform,
				  [{Addr, MsgData}, Timeout, Retry, Vbs,
				   Recv, NetIf, ContextName,
				   get(verbosity),get(sname)])
      end, 
      Recvs).

%% New process
init_v2_inform(Addr, Timeout, Retry, Vbs, Recv, NetIf, Community,V,S) ->
    %% Make a new Inform for each recipient; they need unique
    %% request-ids!
    put(verbosity,V),
    put(sname,inform_sender_short_name(S)),
    ?vdebug("~n   starting with timeout = ~p and retry = ~p",
	    [Timeout,Retry]),
    InformPdu = make_v2_notif_pdu(Vbs, 'inform-request'),
    Msg = {send_pdu_req, 'version-2', InformPdu, {community, Community},
	   [Addr], self()},
    send_inform(Addr, Timeout*10, Retry, Msg, Recv, NetIf).
    

send_inform(Addr, Timeout, -1, Msg,  Recv, NetIf) ->
    ?vinfo("~n   Delivery of send-pdu-request to net-if failed: reply timeout",
	   []),
    deliver_recv(Recv, snmp_notification, {no_response, Addr});
send_inform(Addr, Timeout, Retry, Msg, Recv, NetIf) ->
    ?vtrace("~n   deliver send-pdu-request to net-if when Retry = ~p",[Retry]),
    NetIf ! Msg,
    receive
	{snmp_response_received, _Vsn, _Pdu, _From} ->
	    ?vtrace("~n   send request for ~p acknowledged (~p)",[Recv,Retry]),
	    deliver_recv(Recv, snmp_notification, {got_response, Addr})
    after
	Timeout ->
	    send_inform(Addr, Timeout*2, Retry-1, Msg, Recv, NetIf)
    end.

%% New process
init_v3_inform(Addr, Timeout, Retry, Vbs, Recv, NetIf, ContextName,V,S) ->
    %% Make a new Inform for each recipient; they need unique
    %% request-ids!
    put(verbosity,V),
    put(sname,inform_sender_short_name(S)),
    ?vdebug("~n   starting with timeout = ~p and retry = ~p",
	    [Timeout,Retry]),
    InformPdu = make_v2_notif_pdu(Vbs, 'inform-request'), % Yes, v2
    ContextEngineId = snmp_framework_mib:get_engine_id(),
    Msg = {send_pdu_req, 'version-3', InformPdu,
	   {v3, ContextEngineId, ContextName}, [Addr], self()},
    send_inform(Addr, Timeout*10, Retry, Msg, Recv, NetIf).

% A nasty bit of verbosity setup...    
inform_sender_short_name(ma)   -> mais;
inform_sender_short_name(maw)  -> mais;
inform_sender_short_name(mats) -> mais;
inform_sender_short_name(_)    -> sais.

deliver_recv(no_receiver, MsgId, _Result) ->
    ok;
deliver_recv({Tag, Receiver}, MsgId, Result) ->
    Msg = {MsgId, Tag, Result},
    case Receiver of
	Pid when pid(Pid) ->
	    Pid ! Msg;
	Name when atom(Name) ->
	    catch Name ! Msg;
	{M, F, A} ->
	    catch M:F([Msg | A]);
	Else ->
	    ?vinfo("~n   Cannot deliver acknowledgment: bad receiver = '~p'",
		   [Else]),
	    snmp_error:user_err("snmp: bad receiver, ~w\n", [Else])
    end;
deliver_recv(Else, _MsgId, _Result) ->
    ?vinfo("~n   Cannot deliver acknowledgment: bad receiver = '~p'",
	   [Else]),
    snmp_error:user_err("snmp: bad receiver, ~w\n", [Else]).

check_all_varbinds(#notification{oid = Oid}, Vbs, MibView) ->
    case snmp_acm:validate_mib_view(Oid, MibView) of
	true -> check_all_varbinds(Vbs, MibView);
	false -> false
    end;
check_all_varbinds(#trap{enterpriseoid = Enter, specificcode = Spec},
		   Vbs, MibView) ->
    %% Use alg. in rfc1908 to map a v1 trap to a v2 trap
    Oid = case Enter of
	      ?snmp -> ?snmpTraps ++ [Spec + 1];
	      _ -> Enter ++ [0, Spec]
	  end,
    case snmp_acm:validate_mib_view(Oid, MibView) of
	true -> check_all_varbinds(Vbs, MibView);
	false -> false
    end.

check_all_varbinds([#varbind{oid = Oid} | Vbs], MibView) ->
    case snmp_acm:validate_mib_view(Oid, MibView) of
	true -> check_all_varbinds(Vbs, MibView);
	false -> false
    end;
check_all_varbinds([], _MibView) -> true.

%%--------------------------------------------------
%% Functions to access the local mib.
%%--------------------------------------------------
sys_object_id() ->
    case snmp_agent:do_get(snmp_acm:get_root_mib_view(),
			   [#varbind{oid = ?sysObjectID_instance}],
			   true) of
	{noError, _, [#varbind{value = Value}]} ->
	    Value;
	X ->
	    snmp_error:user_err("sysObjectID bad return value ~p", [X])
    end.

%% Collect all ADDRs for each community together.
%% In: [{Addr, Community}]
%% Out: [{Community, [Addr]}]
mk_addr_communities(Recvs) ->
    [{Addr, Comm} | T] = lists:keysort(2, Recvs),
    mic(T, Comm, [Addr], []).

mic([{Addr, Comm} | T], CurComm, AddrList, Res) when Comm == CurComm ->
    mic(T, CurComm, [Addr | AddrList], Res);
mic([{Addr, Comm} | T], CurComm, AddrList, Res) ->
    mic(T, Comm, [Addr], [{CurComm, AddrList} | Res]);
mic([], CurComm, AddrList, Res) ->
    [{CurComm, AddrList} | Res].

%%-----------------------------------------------------------------
%% Convert the SecurityLevel into a flag value used by snmp_mpd
%%-----------------------------------------------------------------
mk_flag(?'SnmpSecurityLevel_noAuthNoPriv') -> 0;
mk_flag(?'SnmpSecurityLevel_authNoPriv') -> 1;
mk_flag(?'SnmpSecurityLevel_authPriv') -> 3.
     


