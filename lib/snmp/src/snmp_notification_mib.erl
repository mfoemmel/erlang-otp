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
-module(snmp_notification_mib).

-export([configure/1, reconfigure/1, invalidate_cache/0,
	 snmpNotifyTable/1, snmpNotifyTable/3,
	 snmpNotifyFilterTable/3, snmpNotifyFilterProfileTable/3,
	 get_targets/0, get_targets/1]).

-include("SNMP-NOTIFICATION-MIB.hrl").
-include("SNMPv2-TC.hrl").


%%-----------------------------------------------------------------
%% Func: configure/1
%% Args: Dir is the directory where the configuration files are found.
%% Purpose: If the tables doesn't exist, this function reads
%%          the config-files for the notify tables, and
%%          inserts the data.  This means that the data in the tables
%%          survive a reboot.  However, the StorageType column is
%%          checked for each row.  If volatile, the row is deleted.
%% Returns: ok
%% Fails: exit(configuration_error)
%%-----------------------------------------------------------------
configure(Dir) ->
    case snmp_local_db:table_exists(db(snmpNotifyTable)) of
	true ->
	    gc_tabs();
	false ->
	    reconfigure(Dir)
    end.

%%-----------------------------------------------------------------
%% Func: reconfigure/1
%% Args: Dir is the directory where the configuration files are found.
%% Purpose: Reads the config-files for the notify tables, and
%%          inserts the data.  Makes sure that all old data in
%%          the tables are deleted, and the new data inserted.
%%          This function makes sure that all (and only) 
%%          config-file-data are in the tables. 
%% Returns: ok
%% Fails: exit(configuration_error)
%%-----------------------------------------------------------------
reconfigure(Dir) ->
    Notifs =
	snmp_conf:read_notify_config_files(Dir),
    init_tabs(Notifs),
    invalidate_cache(),
    ok.

maybe_create_table(Name) ->
    case snmp_local_db:table_exists(db(Name)) of
	true -> ok;
	_ -> snmp_local_db:table_create(db(Name))
    end.

init_tabs(Notifs) ->
    snmp_local_db:table_delete(db(snmpNotifyTable)),
    snmp_local_db:table_create(db(snmpNotifyTable)),
    init_notify_table(Notifs).
    
init_notify_table([Row | T]) ->
    Key = element(1, Row),
    snmp_local_db:table_create_row(db(snmpNotifyTable), Key, Row),
    init_notify_table(T);
init_notify_table([]) -> true.

gc_tabs() ->
    gc_tab(snmpNotifyTable),
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
%% Func: get_targets()
%%       get_targets(NotifyName) -> [Target]
%% Types: Target = {DestAddr, TargetName, TargetParams, NotifyType}
%%        NotifyName = string()  - the INDEX
%%        DestAddr = {TDomain, TAddr}
%%        TagrgetName = string()
%%        TargetParams = {MpModel, SecModel, SecName, SecLevel}
%%        NotifyType = trap | {inform, Timeout, Retry}
%% Purpose: Returns a list of all targets.  Called by snmp_trap
%%          when a trap should be sent.
%%          If a NotifyName is specified, the targets for that
%%          name is returned.
%%-----------------------------------------------------------------
get_targets() ->
    [Target || {_NotifyName, Target} <- get_targets1()].

get_targets1() ->
    case ets:lookup(snmp_agent_table, snmp_targets_cache) of
	[{_, Targets}] ->
	    Targets;
	 _ ->
	    Targets = find_targets(),
	    update_cache(Targets),
	    Targets
    end.

get_targets(NotifyName) ->
    [Target || {N, Target} <- get_targets1(),
	       N == NotifyName].

%%-----------------------------------------------------------------
%% We use a cache of targets to avoid searching the tables each
%% time a trap is sent.  When some of the 3 tables (notify,
%% targetAddr, targetParams) is modified, the cache is invalidated.
%%-----------------------------------------------------------------
update_cache(Targets) ->
    ets:insert(snmp_agent_table, {snmp_targets_cache, Targets}).

invalidate_cache() ->
    ets:delete(snmp_agent_table, snmp_targets_cache).
    

%% Ret: [{NotifyName, {DestAddr, TargetName, TargetParams, NotifyType}}]
%%   NotifyType = trap | {inform, Timeout. Retry}
%%   DestAddr = {Domain, Addr} ; e.g. {snmpUDPDomain, {IPasList, UdpPort}}

find_targets() ->
    TargAddrs = snmp_target_mib:get_target_addrs(),
    %% TargAddrs = [{TagList,DestAddr,TargetName,TargetParams,Timeout,Retry}]
    find_targets([], TargAddrs , []).
find_targets(Key, TargAddrs, Res) ->
    case table_next(snmpNotifyTable, Key) of
	endOfTable -> 
	    Res;
	NextKey -> 
	    Elements = [?snmpNotifyTag, ?snmpNotifyType, ?snmpNotifyRowStatus],
	    case snmpNotifyTable(get, NextKey, Elements) of
		[{value, Tag}, {value, Type}, {value, ?'RowStatus_active'}] ->
		    Targs = get_targets(TargAddrs, Tag, Type, NextKey),
		    find_targets(NextKey, TargAddrs, Targs ++ Res);
		_ ->
		    find_targets(NextKey, TargAddrs, Res)
	    end
    end.

get_targets([{TagList, Addr, TargetName, Params, Timeout, Retry}|T],
	    Tag, Type, Name) ->
    case snmp_misc:is_tag_member(Tag, TagList) of
	true -> [{Name, {Addr, TargetName, Params, type(Type, Timeout, Retry)}}|
		 get_targets(T, Tag, Type, Name)];
	false ->
	    get_targets(T, Tag, Type, Name)
    end;
get_targets([], Tag, Type, Name) ->
    [].

type(trap, _, _) -> trap;
type(inform, Timeout, Retry) -> {inform, Timeout, Retry}.


%%-----------------------------------------------------------------
%% Instrumentation Functions
%%-----------------------------------------------------------------
%% Op == new | delete
snmpNotifyTable(Op) ->
    snmp_generic:table_func(Op, db(snmpNotifyTable)).

%% Op == get | is_set_ok | set | get_next
snmpNotifyTable(get, RowIndex, Cols) ->
    get(snmpNotifyTable, RowIndex, Cols);
snmpNotifyTable(get_next, RowIndex, Cols) ->
    next(snmpNotifyTable, RowIndex, Cols);
snmpNotifyTable(set, RowIndex, Cols) ->
    invalidate_cache(),
    snmp_generic:table_func(set, RowIndex, Cols, db(snmpNotifyTable));
snmpNotifyTable(Op, Arg1, Arg2) ->
    snmp_generic:table_func(Op, Arg1, Arg2, db(snmpNotifyTable)).


%%-----------------------------------------------------------------
%% In this version of the agent, we don't support notification
%% filters.
%%-----------------------------------------------------------------
snmpNotifyFilterTable(get, RowIndex, Cols) ->
    lists:map(fun(Col) -> {noValue, noSuchObject} end, Cols);
snmpNotifyFilterTable(get_next, RowIndex, Cols) ->
    lists:map(fun(Col) -> endOfTable end, Cols);
snmpNotifyFilterTable(is_set_ok, RowIndex, Cols) ->
    {notWritable, element(1, hd(Cols))}.

snmpNotifyFilterProfileTable(get, RowIndex, Cols) ->
    lists:map(fun(Col) -> {noValue, noSuchObject} end, Cols);
snmpNotifyFilterProfileTable(get_next, RowIndex, Cols) ->
    lists:map(fun(Col) -> endOfTable end, Cols);
snmpNotifyFilterProfileTable(is_set_ok, RowIndex, Cols) ->
    {notWritable, element(1, hd(Cols))}.


db(X) -> {X, persistent}.

fa(snmpNotifyTable) -> ?snmpNotifyTag.
 
foi(snmpNotifyTable) -> ?snmpNotifyName.
 
noc(snmpNotifyTable) -> 5.

stc(snmpNotifyTable) -> ?snmpNotifyStorageType.
 
next(Name, RowIndex, Cols) ->
    snmp_generic:handle_table_next(db(Name), RowIndex, Cols,
                                   fa(Name), foi(Name), noc(Name)).

table_next(Name, RestOid) ->
    snmp_generic:table_next(db(Name), RestOid).

 
get(Name, RowIndex, Cols) ->
    snmp_generic:handle_table_get(db(Name), RowIndex, Cols, foi(Name)).

