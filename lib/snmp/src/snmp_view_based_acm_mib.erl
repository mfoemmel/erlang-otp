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
-module(snmp_view_based_acm_mib).

-export([configure/1, reconfigure/1, table_next/2, get/3]).

-export([vacmAccessTable/1, vacmAccessTable/3,
	 vacmContextTable/1, vacmContextTable/3,
	 vacmSecurityToGroupTable/1, vacmSecurityToGroupTable/3,
	 vacmViewSpinLock/1, vacmViewSpinLock/2,
	 vacmViewTreeFamilyTable/1, vacmViewTreeFamilyTable/3]).

-include("SNMPv2-TC.hrl").
-include("SNMP-VIEW-BASED-ACM-MIB.hrl").
-include("snmp_vacm.hrl").


-define(VMODULE,"VACM-MIB").
-include("snmp_verbosity.hrl").

-ifndef(default_verbosity).
-define(default_verbosity,silence).
-endif.


%%-----------------------------------------------------------------
%% Func: configure/1
%% Args: Dir is the directory where the configuration files are found.
%% Purpose: If the tables doesn't exist, this function reads
%%          the config-files for the VACM tables, and
%%          inserts the data.  This means that the data in the tables
%%          survive a reboot.  However, the StorageType column is
%%          checked for each row.  If volatile, the row is deleted.
%% Returns: ok
%% Fails: exit(configuration_error)
%%-----------------------------------------------------------------
configure(Dir) ->
    set_sname(),
    case snmp_local_db:table_exists(db(vacmSecurityToGroupTable)) of
	true ->
	    ?vdebug("vacm security-to-group table already exist: cleanup",[]),
	    gc_tabs();
	false ->
	    ?vdebug("vacm security-to-group table does not exist: "
		    "reconfigure",[]),
	    reconfigure(Dir)
    end.

%%-----------------------------------------------------------------
%% Func: reconfigure/1
%% Args: Dir is the directory where the configuration files are found.
%% Purpose: Reads the config-files for the VACM tables, and
%%          inserts the data.  Makes sure that all old data in
%%          the tables are deleted, and the new data inserted.
%%          This function makes sure that all (and only) 
%%          config-file-data are in the tables. 
%% Returns: ok
%% Fails: exit(configuration_error)
%%-----------------------------------------------------------------
reconfigure(Dir) ->
    set_sname(),
    ?vdebug("read vacm configuration files",[]),
    {Sec2Group, Access, View} =
	snmp_conf:read_vacm_config_files(Dir),
    ?vdebug("initiate tables",[]),
    init_tabs(Sec2Group, Access, View),
    ok.

maybe_create_table(Name) ->
    case snmp_local_db:table_exists(db(Name)) of
	true -> ok;
	_ -> snmp_local_db:table_create(db(Name))
    end.

init_tabs(Sec2Group, Access, View) ->
    ?vdebug("create vacm security-to-group table",[]),
    snmp_local_db:table_delete(db(vacmSecurityToGroupTable)),
    snmp_local_db:table_create(db(vacmSecurityToGroupTable)),
    init_sec2group_table(Sec2Group),
    init_access_table(Access),
    ?vdebug("create vacm view-tree-family table",[]),
    snmp_local_db:table_delete(db(vacmViewTreeFamilyTable)),
    snmp_local_db:table_create(db(vacmViewTreeFamilyTable)),
    init_view_table(View).
    
init_sec2group_table([Row | T]) ->
    Key1 = element(1, Row),
    Key2 = element(2, Row),
    Key = [Key1, length(Key2) | Key2],
    snmp_local_db:table_create_row(db(vacmSecurityToGroupTable), Key, Row),
    init_sec2group_table(T);
init_sec2group_table([]) -> true.

init_access_table([{GN, Prefix, Model, Level, Row} | T]) ->
    Key = [length(GN) | GN] ++ [length(Prefix) | Prefix] ++ [Model, Level],
    snmp_vacm:insert([{Key, Row}], false),
    init_access_table(T);
init_access_table([]) ->
    snmp_vacm:dump_table().

init_view_table([Row | T]) ->
    Key1 = element(1, Row),
    Key2 = element(2, Row),
    Key = [length(Key1) | Key1] ++ [length(Key2) | Key2],
    snmp_local_db:table_create_row(db(vacmViewTreeFamilyTable), Key, Row),
    init_view_table(T);
init_view_table([]) -> true.

gc_tabs() ->
    gc_tab(vacmSecurityToGroupTable),
    gc_tab(vacmViewTreeFamilyTable),
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
%% The context table is actually implemented in an internal,
%% non-snmp visible table intContextTable.
%%-----------------------------------------------------------------
vacmContextTable(Op) ->
    ok.
vacmContextTable(Op, Arg1, Arg2) ->
    snmp_framework_mib:intContextTable(Op, Arg1, Arg2).


vacmSecurityToGroupTable(Op) ->
    snmp_generic:table_func(Op, db(vacmSecurityToGroupTable)).
vacmSecurityToGroupTable(get_next, RowIndex, Cols) ->
    next(vacmSecurityToGroupTable, RowIndex, Cols);
vacmSecurityToGroupTable(get, RowIndex, Cols) ->
    get(vacmSecurityToGroupTable, RowIndex, Cols);
vacmSecurityToGroupTable(Op, Arg1, Arg2) ->
    snmp_generic:table_func(Op, Arg1, Arg2, db(vacmSecurityToGroupTable)).

%%-----------------------------------------------------------------
%% The vacmAccesTable is implemented as a bplus_tree in the
%% snmp_vacm_access_server process.  That means that we'll have
%% to implement everything by ourselves, most notably is_set_ok
%% and set.
%% Each row is stored in the bplus_tree as
%%    {RowIndex, {Col4, Col5, ..., Col9}}
%%
%%-----------------------------------------------------------------
vacmAccessTable(Op) ->
    ok.
vacmAccessTable(get, RowIndex, Cols) ->
    %% For GET, Cols are guaranteed to be accessible columns.
    case snmp_vacm:get_row(RowIndex) of
	{ok, Row} ->
	    lists:map(fun(Col) -> {value, element(Col-3, Row)} end, Cols);
	false ->
	    {noValue, noSuchInstance}
    end;
vacmAccessTable(get_next, RowIndex, Cols) ->
    %% For GET-NEXT, Cols can be anything, but they are sorted.
    %% Thus, we translate each 
    %% Example: GET-NEXT  -1.3.4.5
    %%                     4.3.4.5
    %%                    10.3.4.5
    %% Table: Idx= 1.2.3 Col4= 1
    %%        Idx= 4.5.6. Col4= 2
    %% Returns:  4.1.2.3 = 1, 4.4.5.6 = 2, endOfTable
    {PreCols, ValidCols} = split_cols(Cols, []),
    do_get_next([], PreCols) ++ do_get_next(RowIndex, ValidCols);
%% vacmAccessContextMatch does not have a default value => we'll have
%% to treat that col specially
vacmAccessTable(is_set_ok, RowIndex, Cols) ->
    IsValidKey = is_valid_key(RowIndex),
    case lists:keysearch(?vacmAccessStatus, 1, Cols) of
	{value, {Col, ?'RowStatus_active'}} -> % Ok, if contextMatch is init
	    {ok, Row} = snmp_vacm:get_row(RowIndex),
	    case element(?vacmAContextMatch, Row) of
		noinit -> {inconsistentValue, Col};
		_ -> {noError, 0}
	    end;
	{value, {Col, ?'RowStatus_notInService'}} -> % Ok, if not notReady
	    {ok, Row} = snmp_vacm:get_row(RowIndex),
	    case element(?vacmAStatus, Row) of
		?'RowStatus_notReady' -> {inconsistentValue, Col};
		_ -> {noError, 0}
	    end;
	{value, {Col, ?'RowStatus_notReady'}} -> % never ok!
	    {inconsistentValue, Col};
	{value, {Col, ?'RowStatus_createAndGo'}} -> % ok, if it doesn't exist
	    Res = lists:keysearch(?vacmAccessContextMatch, 1, Cols),
	    case snmp_vacm:get_row(RowIndex) of
		false when IsValidKey == true, tuple(Res) -> {noError, 0};
		false -> {noCreation, Col}; % Bad RowIndex
		_ -> {inconsistentValue, Col}
	    end;
	{value, {Col, ?'RowStatus_createAndWait'}} -> % ok, if it doesn't exist
	    case snmp_vacm:get_row(RowIndex) of
		false when IsValidKey == true -> {noError, 0};
		false -> {noCreation, Col}; % Bad RowIndex
		_ -> {inconsistentValue, Col}
	    end;
	{value, {Col, ?'RowStatus_destroy'}} -> % always ok!
	    {noError, 0};
	_ -> % otherwise, it's a change; it must exist
	    case snmp_vacm:get_row(RowIndex) of
		{ok, _} ->
		    {noError, 0};
		false ->
		    {inconsistentName, element(1, hd(Cols))}
	    end
    end;
vacmAccessTable(set, RowIndex, Cols) ->
    case lists:keysearch(?vacmAccessStatus, 1, Cols) of
	{value, {Col, ?'RowStatus_createAndGo'}} ->
	    Row = mk_row(Cols),
	    Row2 = setelement(?vacmAStatus, Row, ?'RowStatus_active'),
	    snmp_vacm:insert([{RowIndex, Row2}]),
	    {noError, 0};
	{value, {Col, ?'RowStatus_createAndWait'}} ->
	    Row = mk_row(Cols),
	    Row2 = case element(?vacmAContextMatch, Row) of
		       noinit -> setelement(?vacmAStatus, Row,
					    ?'RowStatus_notReady');
		       _      -> setelement(?vacmAStatus, Row,
					    ?'RowStatus_notInService')
		   end,
	    snmp_vacm:insert([{RowIndex, Row2}]),
	    {noError, 0};
	{value, {Col, ?'RowStatus_destroy'}} ->
	    snmp_vacm:delete(RowIndex),
	    {noError, 0};
	_ ->
	    {ok, Row} = snmp_vacm:get_row(RowIndex),
	    NRow = ch_row(Cols, Row),
	    NRow2 =
		case element(?vacmAContextMatch, NRow) of
		    noinit -> setelement(?vacmAStatus, NRow,
					 ?'RowStatus_notReady');
		    _      -> setelement(?vacmAStatus, NRow,
					 ?'RowStatus_notInService')
		end,
	    snmp_vacm:insert([{RowIndex, NRow2}]),
	    {noError, 0}
    end.
	    
	    
%% Cols are sorted, and all columns are > 3.
do_get_next(RowIndex, Cols) ->
    case snmp_vacm:get_next_row(RowIndex) of
	{NextIndex, Row} ->
	    lists:map(fun(Col) when Col < ?vacmAccessStatus -> 
			      {[Col | NextIndex], element(Col-3, Row)};
			 (_) -> endOfTable
		      end, Cols);
	false ->
	    case snmp_vacm:get_next_row([]) of
		{NextIndex, Row} ->
		    lists:map(fun(Col) when Col < ?vacmAccessStatus -> 
				      {[Col+1 | RowIndex], element(Col-2, Row)};
				 (_) ->
				      endOfTable
			      end, Cols);
		false ->
		    lists:map(fun(Col) -> endOfTable end, Cols)
	    end
    end.

%%-----------------------------------------------------------------
%% Functions to manipulate vacmAccessRows.
%%-----------------------------------------------------------------
is_valid_key(RowIndex) ->
    case catch mk_key(RowIndex) of
	true -> true;
	_ -> false
    end.

mk_key([L1 | T1]) ->
    [L2 | T2] = spx(L1, T1),
    [SM, SL] = spx(L2, T2),
    true.

spx(N, L) -> spx(N, [], L).
spx(0, L1, L2) -> L2;
spx(N, L1, [H | L2]) -> spx(N-1, [H | L1], L2).

mk_row(Cols) -> 
    ch_row(Cols, {noinit, "", "", "", ?'StorageType_volatile', noinit}).

ch_row([], Row) -> Row;
ch_row([{Col, Val} | T], Row) -> ch_row(T, setelement(Col-3, Row, Val)).
   

%% Split a list of columns in 2 lists - the first is all columns
%% that are < 3.  For these, use the first accessible column number: 4.
split_cols([Col | Cols], PreCols) when Col < 3 ->
    split_cols(Cols, [4 | PreCols]);
split_cols(Cols, PreCols) ->
    {PreCols, Cols}.

vacmViewSpinLock(new) ->
    snmp_generic:variable_func(new, {vacmViewSpinLock, volatile}),
    {A1,A2,A3} = erlang:now(),
    random:seed(A1,A2,A3),
    Val = random:uniform(2147483648) - 1,
    snmp_generic:variable_func(set, Val, {vacmViewSpinLock, volatile});

vacmViewSpinLock(delete) ->
    ok;

vacmViewSpinLock(get) ->
    snmp_generic:variable_func(get, {vacmViewSpinLock, volatile}).

vacmViewSpinLock(is_set_ok, NewVal) ->
    case snmp_generic:variable_func(get, {vacmViewSpinLock, volatile}) of
	{value, NewVal} -> noError;
	_ -> inconsistentValue
    end;
vacmViewSpinLock(set, NewVal) ->
    snmp_generic:variable_func(set, (NewVal + 1) rem 2147483648,
			       {vacmViewSpinLock, volatile}).


vacmViewTreeFamilyTable(Op) ->
    snmp_generic:table_func(Op, db(vacmViewTreeFamilyTable)).
vacmViewTreeFamilyTable(get_next, RowIndex, Cols) ->
    next(vacmViewTreeFamilyTable, RowIndex, Cols);
vacmViewTreeFamilyTable(get, RowIndex, Cols) ->
    get(vacmViewTreeFamilyTable, RowIndex, Cols);
vacmViewTreeFamilyTable(Op, Arg1, Arg2) ->
    snmp_generic:table_func(Op, Arg1, Arg2, db(vacmViewTreeFamilyTable)).


table_next(Name, RestOid) ->
    snmp_generic:table_next(db(Name), RestOid).


db(X) -> {X, persistent}.

fa(vacmSecurityToGroupTable) -> ?vacmGroupName;
fa(vacmViewTreeFamilyTable) -> ?vacmViewTreeFamilyMask.
 
foi(vacmSecurityToGroupTable) -> ?vacmSecurityModel;
foi(vacmViewTreeFamilyTable) -> ?vacmViewTreeFamilyViewName.

noc(vacmSecurityToGroupTable) -> 5;
noc(vacmViewTreeFamilyTable) -> 6.
 
stc(vacmSecurityToGroupTable) -> ?vacmSecurityToGroupStorageType;
stc(vacmViewTreeFamilyTable) -> ?vacmViewTreeFamilyStorageType.
 
next(Name, RowIndex, Cols) ->
    snmp_generic:handle_table_next(db(Name), RowIndex, Cols,
                                   fa(Name), foi(Name), noc(Name)).
 
get(Name, RowIndex, Cols) ->
    snmp_generic:handle_table_get(db(Name), RowIndex, Cols, foi(Name)).

set_sname() ->
    set_sname(get(sname)).

set_sname(undefined) ->
    put(sname,conf);
set_sname(_) -> %% Keep it, if already set.
    ok.
