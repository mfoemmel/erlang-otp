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

-record(state, {tab}).

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
    case ets:lookup(Db, {alias, Aliasname}) of
	[{_Key, _MibName, {Oid, _Enums}}|_] -> {value, Oid};
	_ -> false
    end.

oid_to_aliasname(Db,Oid) ->
    ?debug("oid_to_aliasname -> entry with~n"
	   "  Db:  ~p~n"
	   "  Oid: ~p",
	   [Db,Oid]),
    case ets:lookup(Db, {alias, Oid}) of
	[{_Key, _MibName, Aliasname}|_] -> {value, Aliasname};
	_ -> false
    end.

int_to_enum(Db,TypeOrObjName,Int) ->
    ?debug("int_to_enum -> entry with~n"
	   "  Db:            ~p~n"
	   "  TypeOrObjName: ~p~n"
	   "  Int:           ~p",
	   [Db,TypeOrObjName,Int]),
    case ets:lookup(Db, {alias, TypeOrObjName}) of
	[{_Key, _MibName, {_Oid, Enums}}|_] ->
	    case lists:keysearch(Int, 2, Enums) of
		{value, {Enum, _Int}} -> {value, Enum};
		false -> false
	    end;
	_NotAnAliasname ->
	    ?debug("int_to_enum -> not alias, try type",[]),
	    case ets:lookup(Db, {type, TypeOrObjName}) of
		[{_Key, _MibName, Enums}|_] ->
		    case lists:keysearch(Int, 2, Enums) of
			{value, {Enum, _Int}} -> {value, Enum};
			false -> false
		    end;
		_ ->
		    false
	    end
    end.

enum_to_int(Db, TypeOrObjName, Enum) ->
    ?debug("int_to_enum -> entry with~n"
	   "  Db:            ~p~n"
	   "  TypeOrObjName: ~p~n"
	   "  Enum:          ~p",
	   [Db,TypeOrObjName,Enum]),
    case ets:lookup(Db, {alias, TypeOrObjName}) of
	[{_Key, _MibName, {_Oid, Enums}}|_] ->
	    case lists:keysearch(Enum, 1, Enums) of
		{value, {_Enum, Int}} -> {value, Int};
		false -> false
	    end;
	_NotAnAliasname -> 
	    ?debug("enum_to_int -> not alias, try type",[]),
	    case ets:lookup(Db, {type, TypeOrObjName}) of
		[{_Key, _MibName, Enums}|_] ->
		    case lists:keysearch(Enum, 1, Enums) of
			{value, {_Enum, Int}} -> {value, Int};
			false -> false
		    end;
		_ ->
		    false
	    end
    end.


%%----------------------------------------------------------------------
%% DB access (read) functions: Returns: false|{value, Info}
%%----------------------------------------------------------------------
table_info(Db,TableName) ->
    case ets:lookup(Db, {table_info, TableName}) of
	[{_Key, _MibName, Info}] -> {value, Info};
	_ -> false
    end.


%%----------------------------------------------------------------------
%% DB access (read) functions: Returns: false|{value, Info}
%%----------------------------------------------------------------------
variable_info(Db,VariableName) ->
    case ets:lookup(Db, {variable_info, VariableName}) of
	[{_Key, _MibName, Info}] -> {value, Info};
	_ -> false
    end.


%%----------------------------------------------------------------------
%% Implementation
%%----------------------------------------------------------------------

init([Prio,Opts]) ->
    process_flag(priority, Prio),
    put(sname,ss),
    put(verbosity,get_verbosity(Opts)),
    ?vlog("starting",[]),
    %% type = bag solves the problem with import and multiple
    %% object/type definitions.
    S = #state{tab = ets:new(snmp_symbolic_ets, [bag, protected])},
    ?vdebug("started",[]),
    {ok, S}.

handle_call(get_db, _From, S) ->
    ?vlog("get db",[]),
    {reply, S#state.tab, S};

handle_call({table_info, TableName}, _From, S) ->
    ?vlog("table info: ~p",[TableName]),
    Res = table_info(S#state.tab, TableName),
    ?vdebug("table info result: ~p",[Res]),
    {reply, Res, S};

handle_call({variable_info, VariableName}, _From, S) ->
    ?vlog("variable info: ~p",[VariableName]),
    Res = variable_info(S#state.tab, VariableName),
    ?vdebug("variable info result: ~p",[Res]),
    {reply, Res, S};

handle_call({aliasname_to_oid, Aliasname}, _From, S) ->
    ?vlog("aliasname to oid: ~p",[Aliasname]),
    Res = aliasname_to_oid(S#state.tab,Aliasname),
    ?vdebug("aliasname to oid result: ~p",[Res]),
    {reply, Res, S};

handle_call({oid_to_aliasname, Oid}, _From, S) ->
    ?vlog("oid to aliasname: ~p",[Oid]),
    Res = oid_to_aliasname(S#state.tab, Oid),
    ?vdebug("oid to aliasname result: ~p",[Res]),
    {reply, Res, S};

handle_call({enum_to_int, TypeOrObjName, Enum}, _From, S) ->
    ?vlog("enum to int: ~p, ~p",[TypeOrObjName,Enum]),
    Res = enum_to_int(S#state.tab, TypeOrObjName, Enum),
    ?vdebug("enum to int result: ~p",[Res]),
    {reply, Res, S};

handle_call({int_to_enum, TypeOrObjName, Int}, _From, S) ->
    ?vlog("int to enum: ~p, ~p",[TypeOrObjName,Int]),
    Res = int_to_enum(S#state.tab, TypeOrObjName, Int),
    ?vdebug("int to enum result: ~p",[Res]),
    {reply, Res, S};

handle_call({set_notification, MibName, Trap}, _From, S) ->
    ?vlog("set notification:"
	  "~n   ~p~n   ~p",[MibName,Trap]),
    set_notif(S#state.tab, MibName, Trap),
    {reply, true, S};

handle_call({delete_notifications, MibName}, _From, S) ->
    ?vlog("delete notification: ~p",[MibName]),
    delete_notif(S#state.tab, MibName),
    {reply, true, S};

handle_call({get_notification, Key}, _From, S) ->
    ?vlog("get notification: ~p",[Key]),
    Res = get_notif(S#state.tab, Key),
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
	      ets:insert(S#state.tab, {{alias, AN}, MibName, {Oid,Enums}}),
	      ?vlog("add alias~n   ~p -> ~p",[Oid,AN]),
	      ets:insert(S#state.tab, {{alias, Oid}, MibName, AN})
      end, MEs),
    {noreply, S};

handle_info({add_types, MibName, Types}, S) ->
    ?vlog("add types for ~p:",[MibName]),
    Ets = S#state.tab,
    lists:foreach(
      fun(#asn1_type{assocList = Alist, aliasname = Name}) ->
	      case snmp_misc:assq(enums, Alist) of
		  {value, Es} ->
		      ?vlog("add type~n   ~p -> ~p",[Name,Es]),
		      ets:insert(Ets, {{type, Name}, MibName, Es});
		  false -> done
	      end
      end, Types),
    {noreply, S};

handle_info({delete_aliasname_ets, MibName}, S) ->
    ?vlog("delete aliasname ets: ~p",[MibName]),
    ets:match_delete(S#state.tab, {{alias, '_'}, MibName, '_'}),
    {noreply, S};

handle_info({delete_types, MibName}, S) ->
    ?vlog("delete types: ~p",[MibName]),
    ets:match_delete(S#state.tab, {{type, '_'}, MibName, '_'}),
    {noreply, S};


handle_info({add_table_infos, MibName, TableInfos}, S) ->
    ?vlog("add table infos for ~p:",[MibName]),
    lists:foreach(fun({Name, TableInfo}) ->
			  Key = {table_info, Name},
			  ?vlog("add table info~n   ~p -> ~p",
				[Name,TableInfo]),
			  ets:insert(S#state.tab, {Key, MibName, TableInfo})
		  end, TableInfos),
    {noreply, S};

handle_info({delete_table_infos, MibName}, S) ->
    ?vlog("delete table infos: ~p",[MibName]),
    ets:match_delete(S#state.tab, {{table_info, '_'}, MibName, '_'}),
    {noreply, S};

handle_info({add_variable_infos, MibName, VariableInfos}, S) ->
    ?vlog("add variable infos for ~p:",[MibName]),
    lists:foreach(fun({Name, VariableInfo}) ->
			  Key = {variable_info, Name},
			  ?vlog("add variable info~n   ~p -> ~p",
				[Name,VariableInfo]),
			  ets:insert(S#state.tab, {Key,MibName,VariableInfo})
		  end, VariableInfos),
    {noreply, S};

handle_info({delete_variable_infos, MibName}, S) ->
    ?vlog("delete variable infos: ~p",[MibName]),
    ets:match_delete(S#state.tab, {{variable_info, '_'}, MibName, '_'}),
    {noreply, S}.


terminate(Reason, S) ->
    ?vlog("terminate: ~p",[Reason]),
    ets:delete(S#state.tab).


%%----------------------------------------------------------
%% Code change
%%----------------------------------------------------------

% downgrade
code_change({down, Vsn}, State, downgrade_to_pre_3_2_0) ->
    ?debug("code_change(down) -> entry with~n"
	   "  Vsn:   ~p~n"
	   "  State: ~p~n"
	   "  Extra: ~p",
      [Vsn,State,downgrade_to_pre_3_2_0]),
    ets_downgrade(State#state.tab),
    ?debug("downgrade done",[]),
    {ok, State};
code_change({down, Vsn}, State, Extra) ->
    ?debug("code_change(down) -> entry with~n"
	   "  Vsn:   ~p~n"
	   "  State: ~p~n"
	   "  Extra: ~p",
      [Vsn,State,Extra]),
    {ok, State};

% upgrade
code_change(Vsn, State, upgrade_from_pre_3_2_0) ->
    ?debug("code_change(up) -> entry with~n"
	   "  Vsn:   ~p~n"
	   "  State: ~p~n"
	   "  Extra: ~p",
      [Vsn,State,upgrade_from_pre_3_2_0]),
    ets_upgrade(State#state.tab),
    ?debug("upgrade done",[]),
    {ok, State};
code_change(Vsn, State, Extra) ->
    ?debug("code_change(up) -> entry with~n"
	   "  Vsn:   ~p~n"
	   "  State: ~p~n"
	   "  Extra: ~p",
      [Vsn,State,Extra]),
    {ok, State}.


%% Upgrade the ets table, i.e. upgrade all trap- and notification-records
ets_upgrade(Tab) -> 
    ?debug("upgrade ets-table",[]),
    ets_update(Tab,up).

%% Downgrade the ets table, i.e. downgrade all trap- and notification-records
ets_downgrade(Tab) ->
    ?debug("downgrade ets-table",[]),
    ets_update(Tab,down).

ets_update(Tab,How) ->
    ?debug("~pgrade ets-table",[How]),
    Traps = ets:match_object(Tab,{{trap,'_'},'_','_'}),
    ?debug("~p elements to ~pgrade",[length(Traps),How]),
    ets_update(Tab,Traps,How).

ets_update(_Tab,[],How) ->
    ?debug("ets table ~pgraded",[How]),
    ok;
ets_update(Tab,[{{trap,Key},MibName,Trap}|Traps],How) ->
    trap_update(Tab,Key,MibName,Trap,How),
    ets_update(Tab,Traps,How).


trap_update(Tab,Key,MibName,Trap,How) ->
    ?debug("update trap-record with key = ~p",[Key]),
    NTrap = trap_update(How,Trap),  % Create the new trap record
    ets:delete(Tab,{trap,Key}),     % Delete current record from table
    ets:insert(Tab,{{trap,Key},MibName,NTrap}). % Insert new record into table
    
trap_update(up,Trap)   -> trap_upgrade(Trap);
trap_update(down,Trap) -> trap_downgrade(Trap).
    
trap_upgrade({trap,TrapName,EnterpriseOid,SpecificCode,OidObjects}) ->
    ?debug("upgrade trap-record with name = ~p",[TrapName]),
    #trap{trapname      = TrapName, 
	  enterpriseoid = EnterpriseOid,
	  specificcode  = SpecificCode, 
	  oidobjects    = OidObjects};
trap_upgrade({notification,TrapName,Oid,OidObjects}) ->
    ?debug("upgrade notification-record with name = ~p",[TrapName]),
    #notification{trapname   = TrapName, 
		  oid        = Oid, 
		  oidobjects = OidObjects};
trap_upgrade(Any) ->
    ?debug("trap upgrade: ignoring ~p",[Any]),
    Any.
    

trap_downgrade(Trap) when record(Trap,trap) ->
    #trap{trapname      = TrapName, 
	  enterpriseoid = EnterpriseOid,
	  specificcode  = SpecificCode, 
	  oidobjects    = OidObjects} = Trap,
    ?debug("downgrade trap-record with name = ~p",[TrapName]),
    {trap,TrapName,EnterpriseOid,SpecificCode,OidObjects};
trap_downgrade(Trap) when record(Trap,notification) ->
    #notification{trapname   = TrapName, 
		  oid        = Oid, 
		  oidobjects = OidObjects} = Trap,
    ?debug("downgrade notification-record with name = ~p",[TrapName]),
    {notification,TrapName,Oid,OidObjects};
trap_downgrade(Any) ->
    ?debug("trap downgrade: ignoring ~p",[Any]),
    Any.
    

%%-----------------------------------------------------------------
%% Store traps
%%-----------------------------------------------------------------
%% A notification is stored as {Key, Value}, where
%% Key is the symbolic trap name, and Value is 
%% a #trap or a #notification record.
%%-----------------------------------------------------------------
%% Returns: {value, Value} | undefined
%%-----------------------------------------------------------------
get_notif(Tab, Key) ->
    case ets:lookup(Tab, {trap, Key}) of
	[{_Key, _MibName, Value}] -> {value, Value};
	_ -> undefined
    end.

set_notif(Tab, MibName, Trap) when record(Trap, trap) ->
    #trap{trapname = Key} = Trap,
    ets:insert(Tab, {{trap, Key}, MibName, Trap});
set_notif(Tab, MibName, Trap) ->
    #notification{trapname = Key} = Trap,
    ets:insert(Tab, {{trap, Key}, MibName, Trap}).

delete_notif(Tab, MibName) ->
    ets:match_delete(Tab, {{trap, '_'}, MibName, '_'}).


%% -------------------------------------

get_verbosity(L) -> snmp_misc:get_option(verbosity,L,?default_verbosity).

