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

-include("snmp_verbosity.hrl").


%% External exports
-export([start_link/2, start_link/3, stop/0, verbosity/1]).
-export([dump/0, register_notify_client/2, unregister_notify_client/1]).
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
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
	 code_change/3]).


-define(DEFAULT_AUTO_REPAIR,true).
-define(DEFAULT_VERBOSITY,silence).

-record(state,{dets, ets, notify_clients = []}).


%% Debugging (develop)
%% -define(snmp_debug,true).
-include("snmp_debug.hrl").


%%%-----------------------------------------------------------------
%%% Implements a general database in which its possible
%%% to store variables and tables. Provide functions for
%%% tableaccess by row or by element, and for next.
%%%
%%% Opts = [Opt]
%%% Opt = {auto_repair, false | true | true_verbose} |
%%%       {verbosity,silence | log | debug}
%%%-----------------------------------------------------------------
start_link(Dir, Prio) ->
    start_link(Dir,Prio,[]).

start_link(Dir, Prio, Opts) when list(Opts) ->
    gen_server:start_link({local, snmp_local_db}, 
			  snmp_local_db, [Dir,Prio,Opts],[]).

stop() ->
    call(stop).

register_notify_client(Client,Module) ->
    call({register_notify_client,Client,Module}).


unregister_notify_client(Client) ->
    call({unregister_notify_client,Client}).

dump() ->
    call(dump).

verbosity(Verbosity) ->
    cast({verbosity,Verbosity}).


%%%-----------------------------------------------------------------

init([Dir, Prio, Opts]) ->
    process_flag(priority, Prio),
    put(sname,ldb),
    put(verbosity,get_verbosity(Opts)),
    ?vlog("starting",[]),
    Dets = do_dets_open(snmp_local_db1, Dir, get_auto_repair(Opts)),
    Ets  = ets:new(snmp_local_db2, [set, private]),
    ?vdebug("started",[]),
    {ok, #state{dets = Dets, ets = Ets}}.

do_dets_open(Name, Dir, Repair) ->
    case dets_open_file(Name, Dir, Repair) of
	{ok, Dets} ->
	    ?vdebug("dets open done",[]),
	    Dets;
	{error, Reason} ->
	    error_msg("Error opening database ~w: ~w", [Name, Reason]),
	    exit(normal)
    end.
    
dets_open_file(Name, Dir, Repair) ->
    Filename = dets_filename(Name, Dir),
    case dets:info(Filename, size) of
	undefined ->
	    Args = [{file, Filename}, {repair, Repair}],
	    dets:open_file(Name, Args);
	Else ->
	    Else
    end.
	    
dets_filename(Name, Dir) ->
    filename:join(dets_filename1(Dir), Name).
    
dets_filename1([])  -> ".";
dets_filename1(Dir) -> Dir.


%%-----------------------------------------------------------------
%% Interface functions.
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Functions for debugging.
%%-----------------------------------------------------------------
print()          -> call(print).
print(Table)     -> call({print,Table,volatile}).
print(Table, Db) -> call({print,Table,Db}).

variable_get({Name, Db}) ->
    call({variable_get, Name, Db});
variable_get(Name) ->
    call({variable_get, Name, volatile}).

variable_set({Name, Db}, Val) ->
    call({variable_set, Name, Db, Val});
variable_set(Name, Val) ->
    call({variable_set, Name, volatile, Val}).

variable_inc({Name, Db}, N) ->
    cast({variable_inc, Name, Db, N});
variable_inc(Name, N) ->
    cast({variable_inc, Name, volatile, N}).

variable_delete({Name, Db}) ->
    call({variable_delete, Name, Db});
variable_delete(Name) ->
    call({variable_delete, Name, volatile}).


table_create({Name, Db}) ->
    call({table_create, Name, Db});
table_create(Name) ->
    call({table_create, Name, volatile}).

table_exists({Name, Db}) ->
    call({table_exists, Name, Db});
table_exists(Name) ->
    call({table_exists, Name, volatile}).

table_delete({Name, Db}) ->
    call({table_delete, Name, Db});
table_delete(Name) ->
    call({table_delete, Name, volatile}).

table_delete_row({Name, Db}, RowIndex) ->
    call({table_delete_row, Name, Db, RowIndex});
table_delete_row(Name, RowIndex) ->
    call({table_delete_row, Name, volatile, RowIndex}).

table_get_row({Name, Db}, RowIndex) ->
    call({table_get_row, Name, Db, RowIndex});
table_get_row(Name, RowIndex) ->
    call({table_get_row, Name, volatile, RowIndex}).

table_get_element({Name, Db}, RowIndex, Col) ->
    call({table_get_element, Name, Db, RowIndex, Col});
table_get_element(Name, RowIndex, Col) ->
    call({table_get_element, Name, volatile, RowIndex, Col}).

table_set_elements({Name, Db}, RowIndex, Cols) ->
    call({table_set_elements, Name, Db, RowIndex, Cols});
table_set_elements(Name, RowIndex, Cols) ->
    call({table_set_elements, Name, volatile, RowIndex, Cols}).

table_next({Name, Db}, RestOid) ->
    call({table_next, Name, Db, RestOid});
table_next(Name, RestOid) ->
    call({table_next, Name, volatile, RestOid}).

table_max_col({Name, Db}, Col) ->
    call({table_max_col, Name, Db, Col});
table_max_col(Name, Col) ->
    call({table_max_col, Name, volatile, Col}).

table_create_row({Name, Db}, RowIndex, Row) ->
    call({table_create_row, Name, Db,RowIndex, Row});
table_create_row(Name, RowIndex, Row) ->
    call({table_create_row, Name, volatile, RowIndex, Row}).
table_create_row(NameDb, RowIndex, Status, Cols) ->
    Row = table_construct_row(NameDb, RowIndex, Status, Cols),
    table_create_row(NameDb, RowIndex, Row).

match({Name, Db}, Pattern) ->
    call({match, Name, Db, Pattern});    
match(Name, Pattern) ->
    call({match, Name, volatile, Pattern}).


%%-----------------------------------------------------------------
%% Implements the variable functions.
%%-----------------------------------------------------------------
handle_call({variable_get, Name, Db}, _From, State) -> 
    ?vlog("variable get: ~p",[Name]),
    {reply, lookup(Db, Name, State), State};

handle_call({variable_set, Name, Db, Val}, _From, State) -> 
    ?vlog("variable ~p set: "
	  "~n   Val:  ~p",[Name, Val]),
    {reply, insert(Db, Name, Val, State), State};

handle_call({variable_delete, Name, Db}, _From, State) -> 
    ?vlog("variable delete: ~p",[Name]),
    {reply, delete(Db, Name, State), State};


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
handle_call({table_create, Name, Db}, _From, State) ->
    ?vlog("table create: ~p",[Name]),
    catch handle_delete(Db, Name, State),
    {reply, insert(Db, {Name, first}, {undef, first, first}, State), State};

handle_call({table_exists, Name, Db}, _From, State) ->
    ?vlog("table exist: ~p",[Name]),
    Res =
	case lookup(Db, {Name, first}, State) of
	    {value, _} -> true;
	    undefined -> false
	end,
    ?vdebug("table exist result: "
	    "~n   ~p",[Res]),
    {reply, Res, State};

handle_call({table_delete, Name, Db}, _From, State) ->
    ?vlog("table delete: ~p",[Name]),
    catch handle_delete(Db, Name, State),
    {reply, true, State};

handle_call({table_create_row, Name, Db, Indexes, Row}, _From, State) ->
    ?vlog("table create row: "
	  "~n   Name:    ~p"
	  "~n   Indexes: ~p"
	  "~n   Row:     ~p",[Name, Indexes, Row]),
    Res = 
	case catch handle_create_row(Db, Name, Indexes, Row, State) of
	    {'EXIT', _} -> false;
	    _ -> true
	end,
    ?vdebug("table create row result: "
	    "~n   ~p",[Res]),
    {reply, Res, State};

handle_call({table_delete_row, Name, Db, Indexes}, _From, State) ->
    ?vlog("table delete row: "
	  "~n   Name:    ~p"
	  "~n   Indexes: ~p", [Name, Indexes]),
    Res = 
	case catch handle_delete_row(Db, Name, Indexes, State) of
	    {'EXIT', _} -> false;
	    _ -> true
	end,
    ?vdebug("table delete row result: "
	    "~n   ~p",[Res]),
    {reply, Res, State};

handle_call({table_get_row, Name, Db, Indexes}, _From, State) -> 
    ?vlog("table get row: "
	  "~n   Name:    ~p"
	  "~n   Indexes: ~p",[Name, Indexes]),
    Res = case lookup(Db, {Name, Indexes}, State) of
	      undefined -> undefined;
	      {value, {Row, _Prev, _Next}} -> Row
	  end,
    ?vdebug("table get row result: "
	    "~n   ~p",[Res]),
    {reply, Res, State};

handle_call({table_get_element, Name, Db, Indexes, Col}, _From, State) ->
    ?vlog("table ~p get element: "
	  "~n   Indexes: ~p"
	  "~n   Col:     ~p", [Name, Indexes, Col]),
    Res = case lookup(Db, {Name, Indexes}, State) of
	      undefined -> undefined;
	      {value, {Row, _Prev, _Next}} -> {value, element(Col, Row)}
	  end,
    ?vdebug("table get element result: "
	    "~n   ~p",[Res]),
    {reply, Res, State};

handle_call({table_set_elements, Name, Db, Indexes, Cols}, _From, State) ->
    ?vlog("table ~p set element: "
	  "~n   Indexes: ~p"
	  "~n   Cols:    ~p", [Name, Indexes, Cols]),
    Res = 
	case catch handle_set_elements(Db, Name, Indexes, Cols, State) of
	    {'EXIT', _} -> false;
	    _ -> true
	end,
    ?vdebug("table set element result: "
	    "~n   ~p",[Res]),
    {reply, Res, State};

handle_call({table_next, Name, Db, []}, From, State) ->
    ?vlog("table next: ~p",[Name]),
    handle_call({table_next, Name, Db, first}, From, State);
    
handle_call({table_next, Name, Db, Indexes}, _From, State) ->
    ?vlog("table ~p next: "
	  "~n   Indexes: ~p",[Name,Indexes]),
    Res = case lookup(Db, {Name, Indexes}, State) of
	      {value, {_Row, _Prev, Next}} -> 
		  if 
		      Next == first -> endOfTable;
		      true -> Next
		  end;
	      undefined -> 
		  table_search_next(Db, Name, Indexes, State)
	  end,
    ?vdebug("table next result: "
	    "~n   ~p",[Res]),
    {reply, Res, State};

handle_call({table_max_col, Name, Db, Col}, _From, State) ->
    ?vlog("table ~p max col: "
	  "~n   Col: ~p",[Name,Col]),
    Res = table_max_col(Db, Name, Col, 0, first, State),
    ?vdebug("table max col result: "
	    "~n   ~p",[Res]),
    {reply, Res, State};

handle_call({match, Name, Db, Pattern}, _From, State) ->
    ?vlog("~p match(~p):"
	"~n   Pat: ~p", [Name, Db, Pattern]),
    L1 = match(Db, Name, Pattern, State),
    {reply, lists:delete([undef], L1), State};

handle_call(dump, _From, #state{dets = Dets} = State) ->
    ?vlog("dump",[]),
    dets:sync(Dets),
    {reply, ok, State};

handle_call(print, _From, #state{dets = Dets, ets = Ets} = State) ->
    ?vlog("print",[]),
    L1 = ets:tab2list(Ets),
    L2 = dets:match_object(Dets, '_'),
    {reply, {{ets, L1}, {dets, L2}}, State};

handle_call({print, Table, Db}, _From, State) ->
    ?vlog("print: ~p",[Table]),
    L = match(Db, Table, '$1', State),
    {reply, lists:delete([undef], L), State};

handle_call({register_notify_client, Client, Module}, _From, State) ->
    ?vlog("register_notify_client: "
	"~n   Client: ~p"
	"~n   Module: ~p", [Client, Module]),
    Nc = State#state.notify_clients,
    case lists:keysearch(Client,1,Nc) of
	{value,{Client,Mod}} ->
	    ?vlog("register_notify_client: already registered to: ~p",
		  [Module]),
	    {reply, {error,{already_registered,Mod}}, State};
	false ->
	    {reply, ok, State#state{notify_clients = [{Client,Module}|Nc]}}
    end;

handle_call({unregister_notify_client, Client}, _From, State) ->
    ?vlog("unregister_notify_client: ~p",[Client]),
    Nc = State#state.notify_clients,
    case lists:keysearch(Client,1,Nc) of
	{value,{Client,_Module}} ->
	    Nc1 = lists:keydelete(Client,1,Nc),
	    {reply, ok, State#state{notify_clients = Nc1}};
	false ->
	    ?vlog("unregister_notify_client: not registered",[]),
	    {reply, {error,not_registered}, State}
    end;

handle_call(stop, _From, State) ->
    ?vlog("stop",[]),
    {stop, normal, stopped, State}.


handle_cast({variable_inc, Name, Db, N}, State) ->
    ?vlog("variable ~p inc"
	  "~n   N: ~p", [Name, N]),
    M = case lookup(Db, Name, State) of
	    {value, Val} -> Val;
	    _ -> 0 
	end,
    insert(Db, Name, M+N rem 4294967296, State),
    {noreply, State};
    
handle_cast({verbosity,Verbosity}, State) ->
    ?vlog("verbosity: ~p -> ~p",[get(verbosity),Verbosity]),
    put(verbosity,validate(verbosity,Verbosity)),
    {noreply, State}.
    

handle_info(Info, State) ->
    ?vlog("Unknown message: ~p",[Info]),
    {noreply, State}.


terminate(Reason, State) ->
    ?vlog("terminate: ~p",[Reason]),
    close(State).


%%----------------------------------------------------------
%% Code change
%%----------------------------------------------------------

%% downgrade to 3.4
code_change({down, _Vsn}, State, _Extra) ->
%     ?debug("code_change(down) -> entry with~n"
%            "  Vsn:   ~p~n"
%            "  State: ~p~n"
%            "  Extra: ~p",
%            [Vsn,State,Extra]),
    {ok, State};

%% upgrade from 3.4
code_change(_Vsn, State, _Extra) ->
%     ?debug("code_change(up) -> entry with~n"
%            "  Vsn:   ~p~n"
%            "  State: ~p~n"
%            "  Extra: ~p",
%            [Vsn,State,Extra]),
    {ok, State}.


%%-----------------------------------------------------------------
%% All handle_ functions exists so we can catch the call to
%% them, because we don't want to crash the server if a user
%% forgets to call e.g. table_create.
%%-----------------------------------------------------------------
%% Find larger element and insert before.
handle_create_row(Db, Name, Indexes, Row, State) ->		
    case table_find_first_after_maybe_same(Db, Name, Indexes, State) of
	{{Name, Next}, {NRow, NPrev, NNext}} ->
	    {value, {PRow, PPrev, _PNext}} = lookup(Db, {Name, NPrev}, State),
	    if 
		Next == NPrev ->
		    % Insert before first
		    insert(Db, {Name, NPrev}, {PRow, Indexes, Indexes}, State);
		true ->
		    insert(Db, {Name, NPrev}, {PRow, PPrev, Indexes}, State),
		    insert(Db, {Name, Next}, {NRow, Indexes, NNext}, State)
	    end,
	    insert(Db, {Name, Indexes}, {Row, NPrev, Next}, State);
	{same_row, {Prev, Next}} ->
	    insert(Db, {Name, Indexes}, {Row, Prev, Next}, State)
    end.

handle_delete_row(Db, Name, Indexes, State) ->
    {value, {_, Prev, Next}} = lookup(Db, {Name, Indexes}, State),
    {value, {PRow, PPrev, Indexes}} = lookup(Db, {Name, Prev}, State),
    insert(Db, {Name, Prev}, {PRow, PPrev, Next}, State),
    {value, {NRow, Indexes, NNext}} = lookup(Db, {Name, Next}, State),
    insert(Db, {Name, Next}, {NRow, Prev, NNext}, State),
    delete(Db, {Name, Indexes}, State).

handle_set_elements(Db, Name, Indexes, Cols, State) ->
    {value, {Row, Prev, Next}} = lookup(Db, {Name, Indexes}, State),
    NewRow = set_new_row(Cols, Row),
    insert(Db, {Name, Indexes}, {NewRow, Prev, Next}, State).

set_new_row([{Col, Val} | Cols], Row) ->
    set_new_row(Cols, setelement(Col, Row, Val));
set_new_row([], Row) ->
    Row.

handle_delete(Db, Name, State) ->
    {value, {_, _, Next}} = lookup(Db, {Name, first}, State),
    delete(Db, {Name, first}, State),
    handle_delete(Db, Name, Next, State).
handle_delete(_Db, _Name, first, _State) -> true;
handle_delete(Db, Name, Indexes, State) ->
    {value, {_, _, Next}} = lookup(Db, {Name, Indexes}, State),
    delete(Db, {Name, Indexes}, State),
    handle_delete(Db, Name, Next, State).

%%-----------------------------------------------------------------
%% Implementation of next.
%%-----------------------------------------------------------------
table_search_next(Db, Name, Indexes, State) ->
    case catch table_find_first_after(Db, Name, Indexes, State) of
	{{Name, Key}, {_, _, _Next}} ->
	    case Key of
		first -> endOfTable;
		_ -> Key
	    end;
	{'EXIT', _} -> endOfTable
    end.

table_find_first_after(Db, Name, Indexes, State) ->
    {value, {_Row, _Prev, Next}} = lookup(Db, {Name, first}, State),
    table_loop(Db, Name, Indexes, Next, State).

table_loop(Db, Name, _Indexes, first, State) -> 
    {value, FirstVal} = lookup(Db, {Name, first}, State),
    {{Name, first}, FirstVal};

table_loop(Db, Name, Indexes, Cur, State) -> 
    {value, {Row, Prev, Next}} = lookup(Db, {Name, Cur}, State),
    if
	Cur > Indexes ->
	    {{Name, Cur}, {Row, Prev, Next}};
	true ->
	    table_loop(Db, Name, Indexes, Next, State)
    end.
    
table_find_first_after_maybe_same(Db, Name, Indexes, State) ->
    {value, {_Row, _Prev, Next}} = lookup(Db, {Name, first}, State),
    table_loop2(Db, Name, Indexes, Next, State).

table_loop2(Db, Name, _Indexes, first, State) -> 
    {value, FirstVal} = lookup(Db, {Name, first}, State),
    {{Name, first}, FirstVal};

table_loop2(Db, Name, Indexes, Cur, State) -> 
    {value, {Row, Prev, Next}} = lookup(Db, {Name, Cur}, State),
    if
	Cur > Indexes ->
	    {{Name, Cur}, {Row, Prev, Next}};
	Cur == Indexes ->
	    {same_row, {Prev, Next}};
	true ->
	    table_loop2(Db, Name, Indexes, Next, State)
    end.
    
%%-----------------------------------------------------------------
%% Implementation of max.
%% The value in a column could be noinit or undefined,
%% so we must check only those with an integer.
%%-----------------------------------------------------------------
table_max_col(Db, Name, Col, Max, Indexes, State) ->
    case lookup(Db, {Name, Indexes}, State) of
	{value, {Row, _Prev, Next}} -> 
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
		    table_max_col(Db,Name, Col,element(Col, Row),Next, State);
		true -> 
		    table_max_col(Db, Name, Col, Max, Next, State)
	    end;
	undefined -> table_search_next(Db, Name, Indexes, State)
    end.
    
%%-----------------------------------------------------------------
%% Interface to Pets.
%%-----------------------------------------------------------------
insert(volatile, Key, Val, #state{ets = Ets}) -> 
    ?vtrace("insert(volatile) -> ~n"
	    "      Key: ~p~n"
	    "      Val: ~p",
	    [Key,Val]),
    ets:insert(Ets, {Key, Val}),
    true;
insert(persistent, Key, Val, #state{dets = Dets, notify_clients = NC}) -> 
    ?vtrace("insert(persistent) -> ~n"
	    "      Key:  ~p~n"
	    "      Val:  ~p",
	    [Key,Val]),
    dets:insert(Dets, {Key, Val}),
    notify_clients(insert,NC),
    true;
insert(permanent, Key, Val, #state{dets = Dets, notify_clients = NC}) -> 
    ?vtrace("insert(permanent) -> ~n"
	    "      Key:  ~p~n"
	    "      Val:  ~p",
	    [Key,Val]),
    dets:insert(Dets, {Key, Val}),
    notify_clients(insert,NC),
    true;
insert(UnknownDb, Key, Val, _) ->
    error_msg("Tried to insert ~w = ~w into unknown db ~w", 
	      [Key, Val, UnknownDb]),
    false.

delete(volatile, Key, State) -> 
    ets:delete(State#state.ets, Key),
    true;
delete(persistent, Key, #state{dets = Dets, notify_clients = NC}) -> 
    dets:delete(Dets, Key),
    notify_clients(delete,NC),
    true;
delete(permanent, Key, #state{dets = Dets, notify_clients = NC}) -> 
    dets:delete(Dets, Key),
    notify_clients(delete,NC),
    true;
delete(UnknownDb, Key, _) ->
    error_msg("Tried to delete ~w from unknown db ~w", 
		      [Key, UnknownDb]),
    false.


match(volatile, Name, Pattern, #state{ets = Ets}) ->
    ets:match(Ets, {{Name,'_'},{Pattern,'_','_'}});
match(persistent, Name, Pattern, #state{dets = Dets}) ->
    dets:match(Dets, {{Name,'_'},{Pattern,'_','_'}});
match(permanent, Name, Pattern, #state{dets = Dets}) ->
    dets:match(Dets, {{Name,'_'},{Pattern,'_','_'}});
match(UnknownDb, Name, Pattern, _) ->
    error_msg("Tried to match [~p,~p] from unknown db ~w", 
	      [Name, Pattern, UnknownDb]),
    [].

lookup(volatile, Key, #state{ets = Ets}) ->
    case ets:lookup(Ets, Key) of
	[{_, Val}] -> 
	    {value, Val};
	[] -> 
	    undefined
    end;
lookup(persistent, Key, #state{dets = Dets}) ->
    case dets:lookup(Dets, Key) of
	[{_, Val}] ->  
	    {value, Val};
	[] -> 
	    undefined
    end;
lookup(permanent, Key, #state{dets = Dets}) ->
    case dets:lookup(Dets, Key) of
	[{_, Val}] -> 
	    {value, Val};
	[] -> 
	    undefined
    end;
lookup(UnknownDb, Key, _) ->
    error_msg("Tried to lookup ~w in unknown db ~w", [Key, UnknownDb]),
    false.

close(#state{dets = Dets, ets = Ets}) -> 
    ets:delete(Ets),
    dets:close(Dets).


%%-----------------------------------------------------------------
%% Notify clients interface
%%-----------------------------------------------------------------
notify_clients(Event,Clients) ->
    [notify_client(Client,Event) || Client <- Clients].

notify_client({Client,Module},Event) ->
    catch Module:notify(Client,Event).


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

get_elements(_Cols, undefined) -> undefined;
get_elements([Col | Cols], Row) ->
    [element(Col, Row) | get_elements(Cols, Row)];
get_elements([], _Row) -> [].

%%----------------------------------------------------------------------
%% This should/could be a generic function, but since Mnesia implements
%% its own and this version still is local_db dependent, it's not generic yet.
%%----------------------------------------------------------------------
%% createAndGo
table_set_status(NameDb, RowIndex, ?'RowStatus_createAndGo', StatusCol, Cols, 
		 ChangedStatusFunc, _ConsFunc) ->
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
table_set_status(NameDb, RowIndex, ?'RowStatus_destroy', _StatusCol, Cols,
		 ChangedStatusFunc, _ConsFunc) ->
    case snmp_generic:try_apply(ChangedStatusFunc,
				[NameDb, ?'RowStatus_destroy',
				 RowIndex, Cols]) of
	{noError, 0} ->
	    table_delete_row(NameDb, RowIndex),
	    {noError, 0};
	Error -> Error
    end;

%% Otherwise, active or notInService
table_set_status(NameDb, RowIndex, Val, _StatusCol, Cols,
		 ChangedStatusFunc, ConsFunc) ->
    snmp_generic:table_set_cols(NameDb, RowIndex, Cols, ConsFunc),
    snmp_generic:try_apply(ChangedStatusFunc, [NameDb, Val, RowIndex, Cols]).

table_func(new, NameDb) ->
    case table_exists(NameDb) of
	true -> ok;
	_ -> table_create(NameDb)
    end;

table_func(delete, _NameDb) ->
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

table_func(undo, _RowIndex, _Cols, _NameDb) ->
    {noError, 0}.



%%------------------------------------------------------------------
%% These functions retrieves option values from the Options list.
%%------------------------------------------------------------------
get_verbosity([]) ->
    ?DEFAULT_VERBOSITY;
get_verbosity(L) ->
    case lists:keysearch(verbosity,1,L) of
	{value,{verbosity,Verbosity}} ->
	    validate(verbosity,Verbosity);
	_ ->
	    ?DEFAULT_VERBOSITY
    end.

get_auto_repair([]) ->
    ?DEFAULT_AUTO_REPAIR;
get_auto_repair(L) ->
    case lists:keysearch(auto_repair,1,L) of
	{value,{auto_repair,AutoRepair}} ->
	    validate(auto_repair,AutoRepair);
	_ ->
	    ?DEFAULT_AUTO_REPAIR
    end.

validate(auto_repair,true)         -> true;
validate(auto_repair,true_verbose) -> true;
validate(auto_repair,_)            -> false;

validate(verbosity,Verbosity) -> snmp_verbosity:validate(Verbosity).

error_msg(Format, X) ->
    Form = lists:concat(["** Database error: ", Format, "\n"]),
    catch error_logger:error_msg(Form, X).

%% ----------------------------------------------------------------

call(Req) ->
    gen_server:call(snmp_local_db, Req, infinity).

cast(Msg) ->
    gen_server:cast(snmp_local_db, Msg).

