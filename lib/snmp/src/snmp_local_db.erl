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
-module(snmp_local_db).

-include("snmp_types.hrl").
-include("snmp_generic.hrl").
-include("STANDARD-MIB.hrl").

%% External exports
-export([start_link/2, stop/0, dump/0]).
-export([table_func/2, table_func/4,
	 variable_get/1, variable_set/2, variable_delete/1, variable_inc/2,
	 table_create/1, table_exists/1, table_delete/1,
         table_create_row/3, table_create_row/4, table_construct_row/4,
	 table_delete_row/2,
	 table_get_row/2,
         table_get_element/3, table_get_elements/4,
	 table_set_elements/3, table_set_status/7,
         table_next/2,
	 table_max_col/2]).

-export([match/2]).

%% Debug exports
-export([print/0, print/1, print/2]).

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%%%-----------------------------------------------------------------
%%% Implements a general database in which its possible
%%% to store variables and tables. Provide functions for
%%% tableaccess by row or by element, and for next.
%%%-----------------------------------------------------------------
start_link(Dir, Prio) ->
    gen_server:start_link({local, snmp_local_db}, snmp_local_db, [Dir,Prio],[]).

stop() ->
    gen_server:call(snmp_local_db, stop, infinity).

dump() ->
    gen_server:call(snmp_local_db, dump, infinity).

init([Dir, Prio]) ->
    process_flag(priority, Prio),
    FileName = filename:join(Dir, "snmp_local_db"),
    PETS = 
	case catch snmp_pets:open(FileName) of
	    {'EXIT', _Reason} ->
		case catch snmp_pets:new(FileName, snmp_local_db1,
					 [set, private]) of
		    {ok, Pets} -> 
			Pets;
		    {'EXIT', _} ->
			snmp_error:user_err("Error opening database ~p", 
					    [FileName]),
			exit(normal)
		end;
	    {ok, Pets} -> 
		Pets;
	    {error, Reason} ->
		snmp_error:user_err("Error opening database ~p ~p", 
				    [FileName, Reason]),
		exit(normal)
	end,
    Ets = ets:new(snmp_local_db2, [set, private]),
    {ok, {PETS, Ets}}.

%%-----------------------------------------------------------------
%% Interface functions.
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Functions for debugging.
%%-----------------------------------------------------------------
print() -> gen_server:call(snmp_local_db, print, infinity).
print(Table) -> gen_server:call(snmp_local_db, {print,Table,volatile},
				infinity).
print(Table, Db) -> gen_server:call(snmp_local_db, {print,Table,Db}, infinity).

variable_get({Name, Db}) ->
    gen_server:call(snmp_local_db, {variable_get, Name, Db}, infinity);
variable_get(Name) ->
    gen_server:call(snmp_local_db, {variable_get, Name, volatile}, infinity).
variable_set({Name, Db}, Val) ->
    gen_server:call(snmp_local_db, {variable_set, Name, Db, Val}, infinity);
variable_set(Name, Val) ->
    gen_server:call(snmp_local_db, {variable_set, Name, volatile, Val},
		    infinity).
variable_inc({Name, Db}, N) ->
    gen_server:cast(snmp_local_db, {variable_inc, Name, Db, N});
variable_inc(Name, N) ->
    gen_server:cast(snmp_local_db, {variable_inc, Name, volatile, N}).
variable_delete({Name, Db}) ->
    gen_server:call(snmp_local_db, {variable_delete, Name, Db}, infinity);
variable_delete(Name) ->
    gen_server:call(snmp_local_db, {variable_delete, Name, volatile},infinity).

table_create({Name, Db}) ->
    gen_server:call(snmp_local_db, {table_create, Name, Db}, infinity);
table_create(Name) ->
    gen_server:call(snmp_local_db, {table_create, Name, volatile}, infinity).
table_exists({Name, Db}) ->
    gen_server:call(snmp_local_db, {table_exists, Name, Db}, infinity);
table_exists(Name) ->
    gen_server:call(snmp_local_db, {table_exists, Name, volatile}, infinity).
table_delete({Name, Db}) ->
    gen_server:call(snmp_local_db, {table_delete, Name, Db}, infinity);
table_delete(Name) ->
    gen_server:call(snmp_local_db, {table_delete, Name, volatile}, infinity).
table_delete_row({Name, Db}, RowIndex) ->
    gen_server:call(snmp_local_db, {table_delete_row, Name, Db, RowIndex},
		    infinity);
table_delete_row(Name, RowIndex) ->
    gen_server:call(snmp_local_db, {table_delete_row, Name, volatile,RowIndex},
		    infinity).
table_get_row({Name, Db}, RowIndex) ->
    gen_server:call(snmp_local_db, {table_get_row, Name, Db, RowIndex},
		    infinity);
table_get_row(Name, RowIndex) ->
    gen_server:call(snmp_local_db, {table_get_row, Name, volatile, RowIndex},
		    infinity).
table_get_element({Name, Db}, RowIndex, Col) ->
    gen_server:call(snmp_local_db, {table_get_element, Name, Db,
				    RowIndex, Col}, infinity);
table_get_element(Name, RowIndex, Col) ->
    gen_server:call(snmp_local_db, {table_get_element, Name, volatile,
				    RowIndex,Col}, infinity).
table_set_elements({Name, Db}, RowIndex, Cols) ->
    gen_server:call(snmp_local_db, {table_set_elements, Name, Db,
				    RowIndex, Cols}, infinity);
table_set_elements(Name, RowIndex, Cols) ->
    gen_server:call(snmp_local_db, {table_set_elements, Name, volatile,
				    RowIndex, Cols}, infinity).
table_next({Name, Db}, RestOid) ->
    gen_server:call(snmp_local_db, {table_next, Name, Db, RestOid}, infinity);
table_next(Name, RestOid) ->
    gen_server:call(snmp_local_db, {table_next, Name, volatile, RestOid},
		    infinity).
table_max_col({Name, Db}, Col) ->
    gen_server:call(snmp_local_db, {table_max_col, Name, Db, Col}, infinity);
table_max_col(Name, Col) ->
    gen_server:call(snmp_local_db, {table_max_col, Name, volatile, Col},
		    infinity).
table_create_row({Name, Db}, RowIndex, Row) ->
    gen_server:call(snmp_local_db, {table_create_row, Name, Db,RowIndex, Row},
		    infinity);
table_create_row(Name, RowIndex, Row) ->
    gen_server:call(snmp_local_db, {table_create_row, Name, volatile,
				    RowIndex, Row}, infinity).

table_create_row(NameDb, RowIndex, Status, Cols) ->
    Row = table_construct_row(NameDb, RowIndex, Status, Cols),
    table_create_row(NameDb, RowIndex, Row).

match({Name, Db}, Pattern) ->
    gen_server:call(snmp_local_db, {match, Name, Db, Pattern}, infinity);    
match(Name, Pattern) ->
    gen_server:call(snmp_local_db, {match, Name, volatile, Pattern}, infinity).

%%-----------------------------------------------------------------
%% Implements the variable functions.
%%-----------------------------------------------------------------
handle_call({variable_get, Name, Db}, _From, Dbs) -> 
    {reply, lookup(Db, Name, Dbs), Dbs};

handle_call({variable_set, Name, Db, Val}, _From, Dbs) -> 
    {reply, insert(Db, Name, Val, Dbs), Dbs};

handle_call({variable_delete, Name, Db}, _From, Dbs) -> 
    {reply, delete(Db, Name, Dbs), Dbs};

%%-----------------------------------------------------------------
%% Implements the table functions.
%%-----------------------------------------------------------------
%% Entry in ets for a tablerow:
%% Key = {<tableName>, <(flat) list of indexes>}
%% Val = {{Row}, <Prev>, <Next>}
%% Where Prev and Next = <list of indexes>; "pointer to prev/next"
%% Each table is a double linked list, with a head-element, with
%% direct access to each individual element.
%% Head-el: Key = {<tableName>, first}
%% Operations:
%% table_create_row(<tableName>, <list of indexes>, <row>)   O(n)
%% table_delete_row(<tableName>, <list of indexes>)          O(1)
%% get(<tableName>, <list of indexes>, Col)            O(1)
%% set(<tableName>, <list of indexes>, Col, Val)       O(1)
%% next(<tableName>, <list of indexes>)   if Row exist O(1), else O(n)
%%-----------------------------------------------------------------
handle_call({table_create, Name, Db}, _From, Dbs) ->
    catch handle_delete(Db, Name, Dbs),
    {reply, insert(Db, {Name, first}, {undef, first, first}, Dbs), Dbs};

handle_call({table_exists, Name, Db}, _From, Dbs) ->
    Res =
	case lookup(Db, {Name, first}, Dbs) of
	    {value, _} -> true;
	    undefined -> false
	end,
    {reply, Res, Dbs};

handle_call({table_delete, Name, Db}, _From, Dbs) ->
    catch handle_delete(Db, Name, Dbs),
    {reply, true, Dbs};

handle_call({table_create_row, Name, Db, Indexes, Row}, _From, Dbs) ->
    Res = 
	case catch handle_create_row(Db, Name, Indexes, Row, Dbs) of
	    {'EXIT', _} -> false;
	    _ -> true
	end,
    {reply, Res, Dbs};

handle_call({table_delete_row, Name, Db, Indexes}, _From, Dbs) ->
    Res = 
	case catch handle_delete_row(Db, Name, Indexes, Dbs) of
	    {'EXIT', _} -> false;
	    _ -> true
	end,
    {reply, Res, Dbs};

handle_call({table_get_row, Name, Db, Indexes}, _From, Dbs) ->
    case lookup(Db, {Name, Indexes}, Dbs) of
	undefined -> Res = undefined;
	{value, {Row, Prev, Next}} -> Res = Row
    end,
    {reply, Res, Dbs};

handle_call({table_get_element, Name, Db, Indexes, Col}, _From, Dbs) ->
    case lookup(Db, {Name, Indexes}, Dbs) of
	undefined -> Res = undefined;
	{value, {Row, Prev, Next}} -> Res = {value, element(Col, Row)}
    end,
    {reply, Res, Dbs};

handle_call({table_set_elements, Name, Db, Indexes, Cols}, _From, Dbs) ->
    Res = 
	case catch handle_set_elements(Db, Name, Indexes, Cols, Dbs) of
	    {'EXIT', _} -> false;
	    _ -> true
	end,
    {reply, Res, Dbs};

handle_call({table_next, Name, Db, []}, From, Dbs) ->
    handle_call({table_next, Name, Db, first}, From, Dbs);
    
handle_call({table_next, Name, Db, Indexes}, _From, Dbs) ->
    case lookup(Db, {Name, Indexes}, Dbs) of
	{value, {Row, Prev, Next}} -> 
	    if 
		Next == first -> Res = endOfTable;
		true -> Res= Next
	    end;
	undefined -> 
	    Res = table_search_next(Db, Name, Indexes, Dbs)
    end,
    {reply, Res, Dbs};

handle_call({table_max_col, Name, Db, Col}, _From, Dbs) ->
    Res = table_max_col(Db, Name, Col, 0, first, Dbs),
    {reply, Res, Dbs};

handle_call({match, Name, Db, Pattern}, _From, {Pets, Ets}) ->
    REts =
	case Db of
	    volatile -> Ets;
	    _ -> 
		{_,_,Ets1} = Pets,
		Ets1
	end,
    L1 = ets:match(REts, {{Name,'_'},{Pattern,'_','_'}}),
    {reply, lists:delete([undef], L1), {Pets, Ets}};

handle_call(dump, _From, Dbs) ->
    Reply = dump(Dbs),
    {reply, Reply, Dbs};

handle_call(print, _From, {Pets, Ets}) ->
    {_,_,Ets1} = Pets,
    L1 = ets:tab2list(Ets1),
    L2 = ets:tab2list(Ets),
    {reply, {{pets, L1}, {ets, L2}}, {Pets, Ets}};

handle_call({print, Table, Db}, _From, {Pets, Ets}) ->
    REts =
	case Db of
	    volatile -> Ets;
	    _ -> 
		{_,_,Ets1} = Pets,
		Ets1
	end,
    L1 = ets:match(REts, {{Table,'_'},{'$1','_','_'}}),
    {reply, lists:delete([undef], L1), {Pets, Ets}};

handle_call(stop, _From, Dbs) ->
    {stop, normal, stopped, Dbs}.

handle_cast({variable_inc, Name, Db, N}, Dbs) ->
    M = case lookup(Db, Name, Dbs) of
	    {value, Val} -> Val;
	    _ -> 0 
	end,
    insert(Db, Name, M+N rem 4294967296, Dbs),
    {noreply, Dbs}.
    
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, Dbs) ->
    close(Dbs).


%%-----------------------------------------------------------------
%% All handle_ functions exists so we can catch the call to
%% them, because we don't want to crash the server if a user
%% forgets to call e.g. table_create.
%%-----------------------------------------------------------------
%% Find larger element and insert before.
handle_create_row(Db, Name, Indexes, Row, Dbs) ->		
    case table_find_first_after_maybe_same(Db, Name, Indexes, Dbs) of
	{{Name, Next}, {NRow, NPrev, NNext}} ->
	    {value, {PRow, PPrev, PNext}} = lookup(Db, {Name, NPrev}, Dbs),
	    if 
		Next == NPrev ->
		    % Insert before first
		    insert(Db, {Name, NPrev}, {PRow, Indexes, Indexes}, Dbs);
		true ->
		    insert(Db, {Name, NPrev}, {PRow, PPrev, Indexes}, Dbs),
		    insert(Db, {Name, Next}, {NRow, Indexes, NNext}, Dbs)
	    end,
	    insert(Db, {Name, Indexes}, {Row, NPrev, Next}, Dbs);
	{same_row, {Prev, Next}} ->
	    insert(Db, {Name, Indexes}, {Row, Prev, Next}, Dbs)
    end.

handle_delete_row(Db, Name, Indexes, Dbs) ->
    {value, {_, Prev, Next}} = lookup(Db, {Name, Indexes}, Dbs),
    {value, {PRow, PPrev, Indexes}} = lookup(Db, {Name, Prev}, Dbs),
    insert(Db, {Name, Prev}, {PRow, PPrev, Next}, Dbs),
    {value, {NRow, Indexes, NNext}} = lookup(Db, {Name, Next}, Dbs),
    insert(Db, {Name, Next}, {NRow, Prev, NNext}, Dbs),
    delete(Db, {Name, Indexes}, Dbs).

handle_set_elements(Db, Name, Indexes, Cols, Dbs) ->
    {value, {Row, Prev, Next}} = lookup(Db, {Name, Indexes}, Dbs),
    NewRow = set_new_row(Cols, Row),
    insert(Db, {Name, Indexes}, {NewRow, Prev, Next}, Dbs).

set_new_row([{Col, Val} | Cols], Row) ->
    set_new_row(Cols, setelement(Col, Row, Val));
set_new_row([], Row) ->
    Row.

handle_delete(Db, Name, Dbs) ->
    {value, {_, _, Next}} = lookup(Db, {Name, first}, Dbs),
    delete(Db, {Name, first}, Dbs),
    handle_delete(Db, Name, Next, Dbs).
handle_delete(Db, Name, first, Dbs) -> true;
handle_delete(Db, Name, Indexes, Dbs) ->
    {value, {_, _, Next}} = lookup(Db, {Name, Indexes}, Dbs),
    delete(Db, {Name, Indexes}, Dbs),
    handle_delete(Db, Name, Next, Dbs).

%%-----------------------------------------------------------------
%% Implementation of next.
%%-----------------------------------------------------------------
table_search_next(Db, Name, Indexes, Dbs) ->
    case catch table_find_first_after(Db, Name, Indexes, Dbs) of
	{{Name, Key}, {_, _, Next}} ->
	    case Key of
		first -> endOfTable;
		_ -> Key
	    end;
	{'EXIT', _} -> endOfTable
    end.

table_find_first_after(Db, Name, Indexes, Dbs) ->
    {value, {Row, Prev, Next}} = lookup(Db, {Name, first}, Dbs),
    table_loop(Db, Name, Indexes, Next, Dbs).

table_loop(Db, Name, Indexes, first, Dbs) -> 
    {value, FirstVal} = lookup(Db, {Name, first}, Dbs),
    {{Name, first}, FirstVal};

table_loop(Db, Name, Indexes, Cur, Dbs) -> 
    {value, {Row, Prev, Next}} = lookup(Db, {Name, Cur}, Dbs),
    if
	Cur > Indexes ->
	    {{Name, Cur}, {Row, Prev, Next}};
	true ->
	    table_loop(Db, Name, Indexes, Next, Dbs)
    end.
    
table_find_first_after_maybe_same(Db, Name, Indexes, Dbs) ->
    {value, {Row, Prev, Next}} = lookup(Db, {Name, first}, Dbs),
    table_loop2(Db, Name, Indexes, Next, Dbs).

table_loop2(Db, Name, Indexes, first, Dbs) -> 
    {value, FirstVal} = lookup(Db, {Name, first}, Dbs),
    {{Name, first}, FirstVal};

table_loop2(Db, Name, Indexes, Cur, Dbs) -> 
    {value, {Row, Prev, Next}} = lookup(Db, {Name, Cur}, Dbs),
    if
	Cur > Indexes ->
	    {{Name, Cur}, {Row, Prev, Next}};
	Cur == Indexes ->
	    {same_row, {Prev, Next}};
	true ->
	    table_loop2(Db, Name, Indexes, Next, Dbs)
    end.
    
%%-----------------------------------------------------------------
%% Implementation of max.
%% The value in a column could be noinit or undefined,
%% so we must check only those with an integer.
%%-----------------------------------------------------------------
table_max_col(Db, Name, Col, Max, Indexes, Dbs) ->
    case lookup(Db, {Name, Indexes}, Dbs) of
	{value, {Row, Prev, Next}} -> 
	    if 
		Next == first -> 
		    if 
			integer(element(Col, Row)),
			element(Col, Row) > Max -> 
			    element(Col, Row);
			true ->
			    Max
		    end;
		integer(element(Col, Row)),
		element(Col, Row) > Max -> 
		    table_max_col(Db, Name, Col, element(Col, Row), Next, Dbs);
		true -> 
		    table_max_col(Db, Name, Col, Max, Next, Dbs)
	    end;
	undefined -> table_search_next(Db, Name, Indexes, Dbs)
    end.
    
%%-----------------------------------------------------------------
%% Interface to Pets.
%%-----------------------------------------------------------------
insert(volatile, Key, Val, {_Pets, Ets}) -> 
    ets:insert(Ets, {Key, Val}),
    true;
insert(persistent, Key, Val, {Pets, _Ets}) -> 
    snmp_pets:insert(Pets, {Key, Val}),
    true;
insert(permanent, Key, Val, {Pets, _Ets}) -> 
    snmp_pets:insert(Pets, {Key, Val}),
    true;
insert(UnknownDb, Key, Val, _) ->
    snmp_error:user_err("Tried to insert ~w = ~w into unknown db ~w", 
			[Key, Val, UnknownDb]),
    false.

delete(volatile, Key, {_Pets, Ets}) -> 
    ets:delete(Ets, Key),
    true;
delete(persistent, Key, {Pets, _Ets}) -> 
    snmp_pets:delete(Pets, Key),
    true;
delete(permanent, Key, {Pets, _Ets}) -> 
    snmp_pets:delete(Pets, Key),
    true;
delete(UnknownDb, Key, _) ->
    snmp_error:user_err("Tried to delete ~w from unknown db ~w", 
			[Key, UnknownDb]),
    false.

lookup(volatile, Key, {_Pets, Ets}) ->
    lookup(Ets, Key);
lookup(persistent, Key, {{_,_,Pets}, _Ets}) ->
    lookup(Pets, Key);
lookup(permanent, Key, {{_,_,Pets}, _Ets}) ->
    lookup(Pets, Key);
lookup(UnknownDb, Key, _) ->
    snmp_error:user_err("Tried to lookup ~w in unknown db ~w", 
			[Key, UnknownDb]),
    false.

lookup(Ets, Key) ->
    case ets:lookup(Ets, Key) of
	[{_, Val}] -> {value, Val};
	[] -> undefined
    end.

close({Pets, Ets}) -> 
    ets:delete(Ets),
    catch snmp_pets:dump_db(Pets),
    snmp_pets:stop(Pets).

%%-----------------------------------------------------------------
%% Returns: ok | {error, Error}
%%-----------------------------------------------------------------
dump({Pets, Ets}) ->
    case catch snmp_pets:dump_db(Pets) of
	ok -> ok;
	Error -> {error, Error}
    end.

%%------------------------------------------------------------------
%%  Constructs a row with first elements the own part of RowIndex,
%%  and last element RowStatus. All values are stored "as is", i.e.
%%  dynamic key values are stored without length first.
%%  RowIndex is a list of the
%%  first elements. RowStatus is needed, because the
%%  provided value may not be stored, e.g. createAndGo
%%  should be active.
%%  Returns a tuple of values for the row. If a value
%%  isn't specified in the Col list, then the
%%  corresponding value will be noinit.
%%------------------------------------------------------------------
table_construct_row(Name, RowIndex, Status, Cols) ->
    #table_info{nbr_of_cols = LastCol, index_types = Indexes,
		defvals = Defs, status_col = StatusCol,
		first_own_index = FirstOwnIndex, not_accessible = NoAccs} =
	snmp_generic:table_info(Name),
    Keys = snmp_generic:split_index_to_keys(Indexes, RowIndex),
    OwnKeys = snmp_generic:get_own_indexes(FirstOwnIndex, Keys),
    Row = OwnKeys ++ snmp_generic:table_create_rest(length(OwnKeys) + 1,
						    LastCol, StatusCol,
						    Status, Cols, NoAccs),
    L = snmp_generic:init_defaults(Defs, Row),
    list_to_tuple(L).


table_get_elements(NameDb, RowIndex, Cols, _FirstOwnIndex) ->
    get_elements(Cols, table_get_row(NameDb, RowIndex)).

get_elements(Cols, undefined) -> undefined;
get_elements([Col | Cols], Row) ->
    [element(Col, Row) | get_elements(Cols, Row)];
get_elements([], Row) -> [].

%%----------------------------------------------------------------------
%% This should/could be a generic function, but since Mnesia implements
%% its own and this version still is local_db dependent, it's not generic yet.
%%----------------------------------------------------------------------
%% createAndGo
table_set_status(NameDb, RowIndex, ?'RowStatus_createAndGo', StatusCol, Cols, 
		 ChangedStatusFunc, ConsFunc) ->
    case table_create_row(NameDb, RowIndex, ?'RowStatus_active', Cols) of
	true -> snmp_generic:try_apply(ChangedStatusFunc,
				       [NameDb, ?'RowStatus_createAndGo',
					RowIndex, Cols]);
	_ -> {commitFailed, StatusCol}
    end;

%%------------------------------------------------------------------
%% createAndWait - set status to notReady, and try to 
%% make row consistent.
%%------------------------------------------------------------------
table_set_status(NameDb, RowIndex, ?'RowStatus_createAndWait', StatusCol, Cols, 
		 ChangedStatusFunc, ConsFunc) ->
    case table_create_row(NameDb, RowIndex, ?'RowStatus_notReady', Cols) of
	true -> 
	    case snmp_generic:try_apply(ConsFunc, [NameDb, RowIndex, Cols]) of
		{noError, 0} ->
		    snmp_generic:try_apply(ChangedStatusFunc, 
					   [NameDb, ?'RowStatus_createAndWait',
					    RowIndex, Cols]);
		Error -> Error
	    end;
	_ -> {commitFailed, StatusCol}
    end;
    
%% destroy
table_set_status(NameDb, RowIndex, ?'RowStatus_destroy', StatusCol, Cols,
		 ChangedStatusFunc, ConsFunc) ->
    case snmp_generic:try_apply(ChangedStatusFunc,
				[NameDb, ?'RowStatus_destroy',
				 RowIndex, Cols]) of
	{noError, 0} ->
	    table_delete_row(NameDb, RowIndex),
	    {noError, 0};
	Error -> Error
    end;

%% Otherwise, active or notInService
table_set_status(NameDb, RowIndex, Val, StatusCol, Cols,
		 ChangedStatusFunc, ConsFunc) ->
    snmp_generic:table_set_cols(NameDb, RowIndex, Cols, ConsFunc),
    snmp_generic:try_apply(ChangedStatusFunc, [NameDb, Val, RowIndex, Cols]).

table_func(new, NameDb) ->
    case table_exists(NameDb) of
	true -> ok;
	_ -> table_create(NameDb)
    end;

table_func(delete, NameDb) ->
    ok.

table_func(get, RowIndex, Cols, NameDb) ->
    TableInfo = snmp_generic:table_info(NameDb),
    snmp_generic:handle_table_get(NameDb, RowIndex, Cols,
				  TableInfo#table_info.first_own_index);

%%------------------------------------------------------------------
%% Returns: List of endOfTable | {NextOid, Value}.
%% Implements the next operation, with the function
%% handle_table_next. Next should return the next accessible
%% instance, which cannot be a key.
%%------------------------------------------------------------------
table_func(get_next, RowIndex, Cols, NameDb) ->
    #table_info{first_accessible = FirstCol, first_own_index = FOI,
		nbr_of_cols = LastCol} = snmp_generic:table_info(NameDb),
    snmp_generic:handle_table_next(NameDb, RowIndex, Cols,
				   FirstCol, FOI, LastCol);

%%-----------------------------------------------------------------
%% This function must only be used by tables with a RowStatus col!
%% Other tables must check if row exist themselves.
%%-----------------------------------------------------------------
table_func(is_set_ok, RowIndex, Cols, NameDb) ->
    snmp_generic:table_try_row(NameDb, nofunc, RowIndex, Cols);

%%------------------------------------------------------------------
%% Cols is here a list of {ColumnNumber, NewValue}
%% This function must only be used by tables with a RowStatus col!
%% Other tables should use table_set_cols/3,4.
%%------------------------------------------------------------------
table_func(set, RowIndex, Cols, NameDb) ->
    snmp_generic:table_set_row(NameDb,
			       nofunc,
			       {snmp_generic, table_try_make_consistent},
			       RowIndex,
			       Cols);

table_func(undo, RowIndex, Cols, NameDb) ->
    {noError, 0}.
