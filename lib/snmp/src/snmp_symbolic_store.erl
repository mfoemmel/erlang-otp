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
-module(snmp_symbolic_store).

%%----------------------------------------------------------------------
%% This module implements a multipurpose symbolic store.
%% 1) For internal and use from application: aliasname_to_value/1.
%%    If this was stored in the mib, deadlock would occur.
%% 2 table_info/1. Getting information about a table. Used by default 
%%    implementation of tables.
%% 3) variable_info/1. Used by default implementation of variables.
%% 4) notification storage. Used by snmp_trap.
%% There is one symbolic store per node and it uses the ets table
%% snmp_agent_table, owned by the snmp_supervisor.
%%
%%----------------------------------------------------------------------
-include("snmp_types.hrl").
-include("snmp_verbosity.hrl").
-include("snmp_debug.hrl").


%% API
-export([aliasname_to_oid/1, oid_to_aliasname/1, enum_to_int/2, 
	 int_to_enum/2, add_aliasnames/2, delete_aliasnames/1,
	 table_info/1, add_table_infos/2, delete_table_infos/1,
	 variable_info/1, add_variable_infos/2, delete_variable_infos/1,
	 get_notification/1, set_notification/2, delete_notifications/1,
	 start_link/1, start_link/2, add_types/2, delete_types/1]).

%% API (for quick access to the db, note that this is only reads).
-export([get_db/0,
	 aliasname_to_oid/2, oid_to_aliasname/2, 
	 enum_to_int/3, int_to_enum/3]).


%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	code_change/3]).

-export([verbosity/1]).

-ifndef(default_verbosity).
-define(default_verbosity,silence).
-endif.

-record(state, {db}).
-record(symbol,{key,mib_name,info}).

start_link(Prio) ->
    start_link(Prio,[]).

start_link(Prio,Opts) ->
    gen_server:start_link({local, snmp_symbolic_store}, snmp_symbolic_store,
			  [Prio,Opts], []).

%%----------------------------------------------------------------------
%% Returns: Db
%%----------------------------------------------------------------------
get_db() ->
    gen_server:call(snmp_symbolic_store, get_db, infinity).

%%----------------------------------------------------------------------
%% Returns: {value, Oid} | false
%%----------------------------------------------------------------------
aliasname_to_oid(Aliasname) ->
    gen_server:call(snmp_symbolic_store, {aliasname_to_oid, Aliasname},
		    infinity).

oid_to_aliasname(OID) ->
    gen_server:call(snmp_symbolic_store, {oid_to_aliasname, OID}, infinity).

int_to_enum(TypeOrObjName, Int) ->
    gen_server:call(snmp_symbolic_store,{int_to_enum,TypeOrObjName,Int},
		    infinity).

enum_to_int(TypeOrObjName, Enum) ->
    gen_server:call(snmp_symbolic_store,{enum_to_int,TypeOrObjName,Enum},
		    infinity).

add_types(MibName, Types) ->
    snmp_symbolic_store ! {add_types, MibName, Types}.

add_aliasnames(MibName, MEs) ->
    snmp_symbolic_store ! {add_aliasnames, MibName, MEs}.

delete_aliasnames(MibName) ->
    snmp_symbolic_store ! {delete_aliasname_ets, MibName}.

delete_types(MibName) ->
    snmp_symbolic_store ! {delete_types, MibName}.

%%----------------------------------------------------------------------
%% Returns: false|{value, Info}
%%----------------------------------------------------------------------
table_info(TableName) ->
    gen_server:call(snmp_symbolic_store, {table_info, TableName}, infinity).

%%----------------------------------------------------------------------
%% Returns: false|{value, Info}
%%----------------------------------------------------------------------
variable_info(VariableName) ->
    gen_server:call(snmp_symbolic_store, {variable_info, VariableName}, 
		    infinity).

add_table_infos(MibName, TableInfos) ->
    snmp_symbolic_store ! {add_table_infos, MibName, TableInfos}.

delete_table_infos(MibName) ->
    snmp_symbolic_store ! {delete_table_infos, MibName}.

add_variable_infos(MibName, VariableInfos) ->
    snmp_symbolic_store ! {add_variable_infos, MibName, VariableInfos}.

delete_variable_infos(MibName) ->
    snmp_symbolic_store ! {delete_variable_infos, MibName}.

%%-----------------------------------------------------------------
%% Store traps
%%-----------------------------------------------------------------
%% A notification is stored as {Key, Value}, where
%% Key is the symbolic trap name, and Value is 
%% a #trap record.
%%-----------------------------------------------------------------
%% Returns: {value, Val} | undefined
%%-----------------------------------------------------------------
get_notification(Key) ->
    gen_server:call(snmp_symbolic_store, {get_notification, Key}, infinity).
set_notification(Trap, MibName) ->
    gen_server:call(snmp_symbolic_store, {set_notification, MibName, Trap},
		    infinity).
delete_notifications(MibName) ->
    gen_server:call(snmp_symbolic_store, {delete_notifications, MibName},
		    infinity).

verbosity(Verbosity) -> 
    gen_server:cast(snmp_symbolic_store,{verbosity,Verbosity}).


%%----------------------------------------------------------------------
%% DB access (read) functions: Returns: {value, Oid} | false
%%----------------------------------------------------------------------
aliasname_to_oid(Db,Aliasname) ->
    ?debug("aliasname_to_oid -> entry with~n"
	   "  Db:        ~p~n"
	   "  Aliasname: ~p",
	   [Db,Aliasname]),
    case snmp_general_db:read(Db, {alias, Aliasname}) of
	{value,#symbol{info = {Oid, _Enums}}} -> {value, Oid};
	false -> false
    end.

oid_to_aliasname(Db,Oid) ->
    ?debug("oid_to_aliasname -> entry with~n"
	   "  Db:  ~p~n"
	   "  Oid: ~p",
	   [Db,Oid]),
    case snmp_general_db:read(Db, {alias, Oid}) of
	{value,#symbol{info = Aliasname}} -> {value, Aliasname};
	_ -> false
    end.

int_to_enum(Db,TypeOrObjName,Int) ->
    ?debug("int_to_enum -> entry with~n"
	   "  Db:            ~p~n"
	   "  TypeOrObjName: ~p~n"
	   "  Int:           ~p",
	   [Db,TypeOrObjName,Int]),
    case snmp_general_db:read(Db, {alias, TypeOrObjName}) of
	{value,#symbol{info = {_Oid, Enums}}} ->
	    case lists:keysearch(Int, 2, Enums) of
		{value, {Enum, _Int}} -> {value, Enum};
		false -> false
	    end;
	false -> % Not an Aliasname ->
	    ?debug("int_to_enum -> not alias, try type",[]),
	    case snmp_general_db:read(Db, {type, TypeOrObjName}) of
		{value,#symbol{info = Enums}} ->
		    case lists:keysearch(Int, 2, Enums) of
			{value, {Enum, _Int}} -> {value, Enum};
			false -> false
		    end;
		false ->
		    false
	    end
    end.

enum_to_int(Db, TypeOrObjName, Enum) ->
    ?debug("int_to_enum -> entry with~n"
	   "  Db:            ~p~n"
	   "  TypeOrObjName: ~p~n"
	   "  Enum:          ~p",
	   [Db,TypeOrObjName,Enum]),
    case snmp_general_db:read(Db, {alias, TypeOrObjName}) of
	{value,#symbol{info = {_Oid, Enums}}} ->
	    case lists:keysearch(Enum, 1, Enums) of
		{value, {_Enum, Int}} -> {value, Int};
		false -> false
	    end;
	false -> % Not an Aliasname
	    ?debug("enum_to_int -> not alias, try type",[]),
	    case snmp_general_db:read(Db, {type, TypeOrObjName}) of
		{value,#symbol{info = Enums}} ->
		    case lists:keysearch(Enum, 1, Enums) of
			{value, {_Enum, Int}} -> {value, Int};
			false -> false
		    end;
		false ->
		    false
	    end
    end.


%%----------------------------------------------------------------------
%% DB access (read) functions: Returns: false|{value, Info}
%%----------------------------------------------------------------------
table_info(Db,TableName) ->
    case snmp_general_db:read(Db, {table_info, TableName}) of
	{value,#symbol{info = Info}} -> {value, Info};
	false -> false
    end.


%%----------------------------------------------------------------------
%% DB access (read) functions: Returns: false|{value, Info}
%%----------------------------------------------------------------------
variable_info(Db,VariableName) ->
    case snmp_general_db:read(Db, {variable_info, VariableName}) of
	{value,#symbol{info = Info}} -> {value, Info};
	false -> false
    end.


%%----------------------------------------------------------------------
%% Implementation
%%----------------------------------------------------------------------

init([Prio,Opts]) ->
    process_flag(priority, Prio),
    put(sname,ss),
    put(verbosity,get_verbosity(Opts)),
    ?vlog("starting",[]),
    Storage = get_mib_storage(Opts),
    %% type = bag solves the problem with import and multiple
    %% object/type definitions.
    Db = snmp_general_db:open(Storage,snmp_symbolic_store,
			      symbol,record_info(fields,symbol),bag),
    S  = #state{db = Db},
    ?vdebug("started",[]),
    {ok, S}.


handle_call(get_db, _From, S) ->
    ?vlog("get db",[]),
    {reply, S#state.db, S};

handle_call({table_info, TableName}, _From, S) ->
    ?vlog("table info: ~p",[TableName]),
    Res = table_info(S#state.db, TableName),
    ?vdebug("table info result: ~p",[Res]),
    {reply, Res, S};

handle_call({variable_info, VariableName}, _From, S) ->
    ?vlog("variable info: ~p",[VariableName]),
    Res = variable_info(S#state.db, VariableName),
    ?vdebug("variable info result: ~p",[Res]),
    {reply, Res, S};

handle_call({aliasname_to_oid, Aliasname}, _From, S) ->
    ?vlog("aliasname to oid: ~p",[Aliasname]),
    Res = aliasname_to_oid(S#state.db,Aliasname),
    ?vdebug("aliasname to oid result: ~p",[Res]),
    {reply, Res, S};

handle_call({oid_to_aliasname, Oid}, _From, S) ->
    ?vlog("oid to aliasname: ~p",[Oid]),
    Res = oid_to_aliasname(S#state.db, Oid),
    ?vdebug("oid to aliasname result: ~p",[Res]),
    {reply, Res, S};

handle_call({enum_to_int, TypeOrObjName, Enum}, _From, S) ->
    ?vlog("enum to int: ~p, ~p",[TypeOrObjName,Enum]),
    Res = enum_to_int(S#state.db, TypeOrObjName, Enum),
    ?vdebug("enum to int result: ~p",[Res]),
    {reply, Res, S};

handle_call({int_to_enum, TypeOrObjName, Int}, _From, S) ->
    ?vlog("int to enum: ~p, ~p",[TypeOrObjName,Int]),
    Res = int_to_enum(S#state.db, TypeOrObjName, Int),
    ?vdebug("int to enum result: ~p",[Res]),
    {reply, Res, S};

handle_call({set_notification, MibName, Trap}, _From, S) ->
    ?vlog("set notification:"
	  "~n   ~p~n   ~p",[MibName,Trap]),
    set_notif(S#state.db, MibName, Trap),
    {reply, true, S};

handle_call({delete_notifications, MibName}, _From, S) ->
    ?vlog("delete notification: ~p",[MibName]),
    delete_notif(S#state.db, MibName),
    {reply, true, S};

handle_call({get_notification, Key}, _From, S) ->
    ?vlog("get notification: ~p",[Key]),
    Res = get_notif(S#state.db, Key),
    ?vdebug("get notification result: ~p",[Res]),
    {reply, Res, S};

handle_call(stop, _From, S) -> 
    ?vlog("stop",[]),
    {stop, normal, ok, S}.

handle_cast({verbosity,Verbosity}, State) ->
    ?vlog("verbosity: ~p -> ~p",[get(verbosity),Verbosity]),
    put(verbosity,snmp_verbosity:validate(Verbosity)),
    {noreply, State};
    
handle_cast(_, S) ->
    {noreply, S}.
    
handle_info({add_aliasnames, MibName, MEs}, S) ->
    ?vlog("add aliasnames for ~p:",[MibName]),
    lists:foreach(
      fun(#me{aliasname = AN, oid = Oid, asn1_type = AT}) ->
	      Enums =
		  case AT of
		      #asn1_type{assocList = Alist} -> 
			  case lists:keysearch(enums, 1, Alist) of
			      {value, {enums, Es}} -> Es;
			      _ -> []
			  end;
		      _ -> []
		  end,
	      ?vlog("add alias~n   ~p -> {~p,~p}",[AN,Oid,Enums]),
	      Rec1 = #symbol{key      = {alias, AN}, 
			     mib_name = MibName, 
			     info     = {Oid,Enums}},
	      snmp_general_db:write(S#state.db, Rec1),
	      ?vlog("add alias~n   ~p -> ~p",[Oid,AN]),
	      Rec2 = #symbol{key      = {alias, Oid}, 
			     mib_name = MibName, 
			     info     = AN},
	      snmp_general_db:write(S#state.db, Rec2)
      end, MEs),
    {noreply, S};

handle_info({add_types, MibName, Types}, S) ->
    ?vlog("add types for ~p:",[MibName]),
    Db = S#state.db,
    lists:foreach(
      fun(#asn1_type{assocList = Alist, aliasname = Name}) ->
	      case snmp_misc:assq(enums, Alist) of
		  {value, Es} ->
		      ?vlog("add type~n   ~p -> ~p",[Name,Es]),
		      Rec = #symbol{key      = {type, Name}, 
				    mib_name = MibName, 
				    info     = Es},
		      snmp_general_db:write(Db, Rec);
		  false -> done
	      end
      end, Types),
    {noreply, S};

handle_info({delete_aliasname_ets, MibName}, S) ->
    ?vlog("delete aliasname ets: ~p",[MibName]),
    Pattern = #symbol{key = {alias, '_'}, mib_name = MibName, info = '_'},
    snmp_general_db:match_delete(S#state.db, Pattern),
    {noreply, S};

handle_info({delete_types, MibName}, S) ->
    ?vlog("delete types: ~p",[MibName]),
    Pattern = #symbol{key = {type, '_'}, mib_name = MibName, info = '_'},
    snmp_general_db:match_delete(S#state.db, Pattern),
    {noreply, S};


handle_info({add_table_infos, MibName, TableInfos}, S) ->
    ?vlog("add table infos for ~p:",[MibName]),
    lists:foreach(fun({Name, TableInfo}) ->
			  ?vlog("add table info~n   ~p -> ~p",
				[Name,TableInfo]),
			  Rec = #symbol{key      = {table_info, Name}, 
					mib_name = MibName, 
					info     = TableInfo},
			  snmp_general_db:write(S#state.db, Rec)
		  end, TableInfos),
    {noreply, S};

handle_info({delete_table_infos, MibName}, S) ->
    ?vlog("delete table infos: ~p",[MibName]),
    Pattern = #symbol{key = {table_info, '_'}, mib_name = MibName, info = '_'},
    snmp_general_db:match_delete(S#state.db, Pattern),
    {noreply, S};

handle_info({add_variable_infos, MibName, VariableInfos}, S) ->
    ?vlog("add variable infos for ~p:",[MibName]),
    lists:foreach(fun({Name, VariableInfo}) ->
			  ?vlog("add variable info~n   ~p -> ~p",
				[Name,VariableInfo]),
			  Rec = #symbol{key      = {variable_info, Name},
					mib_name = MibName,
					info     = VariableInfo},
			  snmp_general_db:write(S#state.db, Rec)
		  end, VariableInfos),
    {noreply, S};

handle_info({delete_variable_infos, MibName}, S) ->
    ?vlog("delete variable infos: ~p",[MibName]),
    Pattern = #symbol{key={variable_info,'_'}, mib_name=MibName, info='_'},
    snmp_general_db:match_delete(S#state.db, Pattern),
    {noreply, S}.


terminate(Reason, S) ->
    ?vlog("terminate: ~p",[Reason]),
    snmp_general_db:close(S#state.db).


%%----------------------------------------------------------
%% Code change
%%----------------------------------------------------------

% downgrade
code_change({down, Vsn}, State, downgrade_to_pre_3_3_0) ->
    ?debug("code_change(down) -> entry with~n"
	   "  Vsn:   ~p~n"
	   "  State: ~p~n"
	   "  Extra: ~p",
      [Vsn,State,downgrade_to_pre_3_3_0]),
    case State#state.db of
	{ets,Tab} ->
	    ?debug("code_change(down) -> Tab = ~p",[Tab]),
	    NTab = ets_downgrade(Tab),
	    ?debug("code_change(down) -> NTab = ~p",[NTab]),
	    {ok, {state,NTab}};
	Other ->
	    exit({unsupported_database,Other})
    end;
code_change({down, Vsn}, State, Extra) ->
    ?debug("code_change(down) -> entry with~n"
	   "  Vsn:   ~p~n"
	   "  State: ~p~n"
	   "  Extra: ~p",
      [Vsn,State,Extra]),
    {ok, State};

% upgrade
code_change(Vsn, State, upgrade_from_pre_3_3_0) ->
    ?debug("code_change(up) -> entry with~n"
	   "  Vsn:   ~p~n"
	   "  State: ~p~n"
	   "  Extra: ~p",
      [Vsn,State,upgrade_from_pre_3_3_0]),
    %% Prior to 3.3.0 there where only ets storage
    {state,Tab} = State,
    ?debug("code_change(up) -> Tab = ~p",[Tab]),
    NTab = ets_upgrade(Tab),
    ?debug("code_change(up) -> NTab = ~p",[NTab]),
    {ok, #state{db = {ets,NTab}}};
code_change(Vsn, State, Extra) ->
    ?debug("code_change(up) -> entry with~n"
	   "  Vsn:   ~p~n"
	   "  State: ~p~n"
	   "  Extra: ~p",
      [Vsn,State,Extra]),
    {ok, State}.



ets_upgrade(Tab) ->
    ets_update(Tab,up).

ets_downgrade(Tab) ->
    ets_update(Tab,down).

ets_update(Tab,How) ->
    ?debug("ets_update -> entry when Tab = ~p",[Tab]),
    Recs = ets:tab2list(Tab),
    ?debug("ets_update -> ~p records to be ~pgraded",[length(Recs),How]),
    ets:rename(Tab,snmp_symbolic_store_tmp),
    {ets,NTab} = snmp_general_db:open(ets,snmp_symbolic_store,
				      symbol,record_info(fields,symbol),bag),
    ?debug("ets_update -> NTab = ~p",[NTab]),
    ets_update(NTab,Tab,Recs,How).

ets_update(Tab,OldTab,[],_How) ->
    ets:delete(OldTab),
    Tab;
ets_update(Tab,OldTab,[Rec|Recs],How) ->
    ?debug("ets_update -> ~pgrade record ~p",[How,Rec]),
    record_update(Tab,Rec,How),
    ets_update(Tab,OldTab,Recs,How).

record_update(Tab,{Key, MibName, Info},up) ->
    ?debug("record_update -> upgrade record with"
	   "~n     Tab:     ~p"
	   "~n     Key:     ~p"
	   "~n     MibName: ~p"
	   "~n     Info:    ~p",[Tab, Key, MibName, Info]),
    NRec = #symbol{key = Key, mib_name = MibName, info = Info},
    ?debug("record_update(up) -> "
	   "~n     NRec: ~p",[NRec]),
    ets:insert(Tab,NRec);
record_update(Tab,Rec,down) when record(Rec,symbol) ->
    ?debug("record_update -> downgrade record"
	   "~n     Rec: ~p",[Rec]),
    ORec = {Rec#symbol.key, Rec#symbol.mib_name, Rec#symbol.info},
    ?debug("record_update -> ORec: ~p",[ORec]),
    ets:insert(Tab,ORec).

    
%%-----------------------------------------------------------------
%% Store traps
%%-----------------------------------------------------------------
%% A notification is stored as {Key, Value}, where
%% Key is the symbolic trap name, and Value is 
%% a #trap or a #notification record.
%%-----------------------------------------------------------------
%% Returns: {value, Value} | undefined
%%-----------------------------------------------------------------
get_notif(Db, Key) ->
    case snmp_general_db:read(Db, {trap, Key}) of
	{value,#symbol{info = Value}} -> {value, Value};
	false -> undefined
    end.

set_notif(Db, MibName, Trap) when record(Trap, trap) ->
    #trap{trapname = Key} = Trap,
    Rec = #symbol{key = {trap, Key}, mib_name = MibName, info = Trap},
    snmp_general_db:write(Db, Rec);
set_notif(Db, MibName, Trap) ->
    #notification{trapname = Key} = Trap,
    Rec = #symbol{key = {trap, Key}, mib_name = MibName, info = Trap},
    snmp_general_db:write(Db, Rec).

delete_notif(Db, MibName) ->
    Rec = #symbol{key = {trap, '_'}, mib_name = MibName, info = '_'},
    snmp_general_db:match_delete(Db, Rec).


%% -------------------------------------

get_verbosity(L) -> 
    snmp_misc:get_option(verbosity,L,?default_verbosity).

get_mib_storage(L) -> 
    snmp_misc:get_option(mib_storage,L,ets).

