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
-module(snmp_target_mib).

-export([configure/1, reconfigure/1,
	 snmpTargetSpinLock/1, snmpTargetSpinLock/2,
	 snmpTargetAddrTable/1, snmpTargetAddrTable/3,
	 snmpTargetParamsTable/1, snmpTargetParamsTable/3,
	 table_next/2, get_target_addrs/0, 
	 get_target_engine_id/1, set_target_engine_id/2,
	 is_valid_tag/3, get/3, table_next/2]).

-include("SNMP-TARGET-MIB.hrl").
-include("SNMPv2-TC.hrl").
-include("SNMPv2-TM.hrl").

%% Column not accessible via SNMP - needed when the agent sends informs
-define(snmpTargetAddrEngineId, 10).
%% Extra comlumns for the augmented table snmpTargetAddrExtTable
-define(snmpTargetAddrTMask, 11).
-define(snmpTargetAddrMMS, 12).

%%-----------------------------------------------------------------
%% Func: configure/1
%% Args: Dir is the directory where the configuration files are found.
%% Purpose: If the tables doesn't exist, this function reads
%%          the config-files for the target tables, and
%%          inserts the data.  This means that the data in the tables
%%          survive a reboot.  However, the StorageType column is
%%          checked for each row.  If volatile, the row is deleted.
%% Returns: ok
%% Fails: exit(configuration_error)
%%-----------------------------------------------------------------
configure(Dir) ->
    case snmp_local_db:table_exists(db(snmpTargetParamsTable)) of
	true ->
	    init_vars(),
	    gc_tabs();
	false ->
	    reconfigure(Dir)
    end.

%%-----------------------------------------------------------------
%% Func: reconfigure/1
%% Args: Dir is the directory where the configuration files are found.
%% Purpose: Reads the config-files for the target tables, and
%%          inserts the data.  Makes sure that all old data in
%%          the tables are deleted, and the new data inserted.
%%          This function makes sure that all (and only) 
%%          config-file-data are in the tables. 
%% Returns: ok
%% Fails: exit(configuration_error)
%%-----------------------------------------------------------------
reconfigure(Dir) ->
    {Addrs, Params} =
	snmp_conf:read_target_config_files(Dir),
    init_tabs(Addrs, Params),
    init_vars(),
    snmp_notification_mib:invalidate_cache(),
    ok.

maybe_create_table(Name) ->
    case snmp_local_db:table_exists(db(Name)) of
	true -> ok;
	_ -> snmp_local_db:table_create(db(Name))
    end.

init_tabs(Addrs, Params) ->
    snmp_local_db:table_delete(db(snmpTargetAddrTable)),
    snmp_local_db:table_create(db(snmpTargetAddrTable)),
    init_addr_table(Addrs),
    snmp_local_db:table_delete(db(snmpTargetParamsTable)),
    snmp_local_db:table_create(db(snmpTargetParamsTable)),
    init_params_table(Params).
    
init_addr_table([Row | T]) ->
    Key = element(1, Row),
    snmp_local_db:table_create_row(db(snmpTargetAddrTable), Key, Row),
    init_addr_table(T);
init_addr_table([]) -> true.

init_params_table([Row | T]) ->
    Key = element(1, Row),
    snmp_local_db:table_create_row(db(snmpTargetParamsTable), Key, Row),
    init_params_table(T);
init_params_table([]) -> true.

gc_tabs() ->
    gc_tab(snmpTargetAddrTable),
    gc_tab(snmpTargetParamsTable),
    ok.

gc_tab(Tab) ->
    STC = stc(Tab),
    F = fun(Oid, Row) ->
		case element(STC, Row) of
		    ?'StorageType_volatile' ->
			snmp_local_db:table_delete_row(db(Tab), Oid);
		    _ ->
			ok
		end
	end,
    snmp_generic:table_foreach(db(Tab), F).

%%-----------------------------------------------------------------
%% Counter functions
%%-----------------------------------------------------------------
init_vars() -> lists:map(fun maybe_create_var/1, vars()).

maybe_create_var(Var) ->
    case ets:lookup(snmp_agent_table, Var) of
	[_] -> ok;
	_ -> init_var(Var)
    end.

init_var(Var) -> ets:insert(snmp_agent_table, {Var, 0}).

vars() ->
    [snmpUnavailableContexts,
     snmpUnknownContexts].

%%-----------------------------------------------------------------
%% API functions
%%-----------------------------------------------------------------
is_valid_tag("", TDomain, TAddress) ->
    true;
is_valid_tag(Tag, TDomain, TAddress) ->
    is_valid_tag(TDomain, TAddress, Tag, []).

is_valid_tag(TDomain, TAddress, Tag, Key) ->
    case table_next(snmpTargetAddrTable, Key) of
	endOfTable -> 
	    false;
	NextKey -> 
	    case get(snmpTargetAddrTable, NextKey, [?snmpTargetAddrTDomain,
						    ?snmpTargetAddrTAddress,
						    ?snmpTargetAddrTagList,
						    ?snmpTargetAddrTMask]) of
		[{value, TDomain}, % must match exactly
		 {value, TAddress2},
		 {value, TagList}, 
		 {value, TMask}] ->
		    case snmp_misc:is_tmask_match(TAddress, TAddress2, TMask) of
			true ->
			    case snmp_misc:is_tag_member(Tag, TagList) of
				true ->
				    true;
				false ->
				    is_valid_tag(TDomain, TAddress,
							    Tag, NextKey)
			    end;
			false ->
			    is_valid_tag(TDomain, TAddress,
						    Tag, NextKey)
		    end;
		_ ->
		    is_valid_tag(TDomain, TAddress, Tag, NextKey)
	    end
    end.
    

%% TargAddrs =
%%     [{TagList, TargetAddr, TargetAddrName, TargetParams, Timeout, Retry}]
%%   TargetAddr = {TDomain, TAddr}; e.g. {?snmpUDPDomain, IpAndUdpPortAsList}
get_target_addrs() ->
    get_target_addrs([], []).

get_target_addrs(Key, Res) ->
    case table_next(snmpTargetAddrTable, Key) of
	endOfTable -> 
	    Res;
	NextKey -> 
	    case snmp_local_db:table_get_row(db(snmpTargetAddrTable),NextKey) of
		{_Key, TDomain, TAddress, Timeout, RetryCount, TagList, Params,
		 _Storage, ?'RowStatus_active', _TargetEngineId,_TMask,_MMS} ->
		    TargetParams = get_target_params(Params),
		    Targ = {TagList, {TDomain, TAddress}, NextKey,
			    TargetParams, Timeout, RetryCount},
		    get_target_addrs(NextKey, [Targ | Res]);
		_ ->
		    get_target_addrs(NextKey, Res)
	    end
    end.

get_target_params(Params) ->
    case snmpTargetParamsTable(get, Params, [?snmpTargetParamsMPModel,
					     ?snmpTargetParamsSecurityModel,
					     ?snmpTargetParamsSecurityName,
					     ?snmpTargetParamsSecurityLevel,
					     ?snmpTargetParamsRowStatus]) of
	[{value, MpModel}, 
	 {value, SecModel}, {value, SecName}, {value, SecLevel},
	 {value, ?'RowStatus_active'}] ->
	    {MpModel, SecModel, SecName, SecLevel};
	_ ->
	    undefined
    end.

get_target_engine_id(TargetAddrName) ->
    case snmp_local_db:table_get_element(db(snmpTargetAddrTable),
					 TargetAddrName,
					 ?snmpTargetAddrEngineId) of
	{value, Val} -> {ok, Val};
	_ ->
	    undefined
    end.
				    
set_target_engine_id(TargetAddrName, EngineId) ->
    snmp_local_db:table_set_elements(db(snmpTargetAddrTable),
				     TargetAddrName,
				     [{?snmpTargetAddrEngineId, EngineId}]).

%%-----------------------------------------------------------------
%% Instrumentation Functions
%%-----------------------------------------------------------------
snmpTargetSpinLock(new) ->
    snmp_generic:variable_func(new, {snmpTargetSpinLock, volatile}),
    {A1,A2,A3} = erlang:now(),
    random:seed(A1,A2,A3),
    Val = random:uniform(2147483648) - 1,
    snmp_generic:variable_func(set, Val, {snmpTargetSpinLock, volatile});

snmpTargetSpinLock(delete) ->
    ok;

snmpTargetSpinLock(get) ->
    snmp_generic:variable_func(get, {snmpTargetSpinLock, volatile}).

snmpTargetSpinLock(is_set_ok, NewVal) ->
    case snmp_generic:variable_func(get, {snmpTargetSpinLock, volatile}) of
	{value, NewVal} -> noError;
	_ -> inconsistentValue
    end;
snmpTargetSpinLock(set, NewVal) ->
    snmp_generic:variable_func(set, (NewVal + 1) rem 2147483648,
			       {snmpTargetSpinLock, volatile}).


%% Op == new | delete
snmpTargetAddrTable(Op) ->
    snmp_generic:table_func(Op, db(snmpTargetAddrTable)).

%% Op == get | is_set_ok | set | get_next
snmpTargetAddrTable(get, RowIndex, Cols) ->
    get(snmpTargetAddrTable, RowIndex, Cols);
snmpTargetAddrTable(get_next, RowIndex, Cols) ->
    next(snmpTargetAddrTable, RowIndex, Cols);
snmpTargetAddrTable(set, RowIndex, Cols) ->
    snmp_notification_mib:invalidate_cache(),
    %% Add columns for augmenting table snmpTargetAddrExtTable and for
    %% target engine ID.  Target engine ID is set to "".  The function
    %% get_target_engine_id will return "" unless a value is set using
    %% set_target_engine_id.  If it is "" Informs can't be sent to the
    %% target.
    NCols = Cols ++ [{?snmpTargetAddrEngineId, ""},
		     {?snmpTargetAddrTMask, []},
		     {?snmpTargetAddrMMS, 2048}],
    snmp_generic:table_func(set, RowIndex, NCols, db(snmpTargetAddrTable));
snmpTargetAddrTable(Op, Arg1, Arg2) ->
    snmp_generic:table_func(Op, Arg1, Arg2, db(snmpTargetAddrTable)).

%% Op == new | delete
snmpTargetParamsTable(Op) ->
    snmp_generic:table_func(Op, db(snmpTargetParamsTable)).

%% Op == get | is_set_ok | set | get_next
snmpTargetParamsTable(get, RowIndex, Cols) ->
    get(snmpTargetParamsTable, RowIndex, Cols);
snmpTargetParamsTable(get_next, RowIndex, Cols) ->
    next(snmpTargetParamsTable, RowIndex, Cols);
snmpTargetParamsTable(set, RowIndex, Cols) ->
    snmp_notification_mib:invalidate_cache(),
    snmp_generic:table_func(set, RowIndex, Cols, db(snmpTargetParamsTable));
snmpTargetParamsTable(Op, Arg1, Arg2) ->
    snmp_generic:table_func(Op, Arg1, Arg2, db(snmpTargetParamsTable)).


db(X) -> {X, persistent}.

fa(snmpTargetAddrTable) -> ?snmpTargetAddrTDomain;
fa(snmpTargetParamsTable) -> ?snmpTargetParamsMPModel.
 
foi(snmpTargetAddrTable) -> ?snmpTargetAddrName;
foi(snmpTargetParamsTable) -> ?snmpTargetParamsName.
 
noc(snmpTargetAddrTable) -> 9;
noc(snmpTargetParamsTable) -> 7.

stc(snmpTargetAddrTable) -> ?snmpTargetAddrStorageType;
stc(snmpTargetParamsTable) -> ?snmpTargetParamsStorageType.
 
next(Name, RowIndex, Cols) ->
    snmp_generic:handle_table_next(db(Name), RowIndex, Cols,
                                   fa(Name), foi(Name), noc(Name)).

table_next(Name, RestOid) ->
    snmp_generic:table_next(db(Name), RestOid).

 
get(Name, RowIndex, Cols) ->
    snmp_generic:handle_table_get(db(Name), RowIndex, Cols, foi(Name)).

