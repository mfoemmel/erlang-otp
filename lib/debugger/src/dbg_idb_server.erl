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
-module(dbg_idb_server).

%%-compile(export_all).
-export([start/0]).


%%% -------------------------------------------------------
%%% The 'int_db' server's main loop.
%%% The server holds a database table (called global) with
%%% accesible entries: trace, breakpoints, interpret,
%%%                    stack_trace,
%%%                    Break == {Mod,Line}.
%%% It also holds the protected entries: 
%%%                    {Mod,refs},  (Database tables for Mod)
%%%                    DbRef        (Pids using DbRef)
%%%
%%% -------------------------------------------------------

start() ->
    DbT = dbg_ets:new(int_db,[set,protected]),
    dbg_ets:insert(DbT,{trace,false}),
    dbg_ets:insert(DbT,{stack_trace,all}),
    dbg_ets:insert(DbT,{breakpoints,[]}),
    dbg_ets:insert(DbT,{interpret,[]}),
    main_db_loop(DbT).

main_db_loop(DbT) ->
    receive
	{From,insert,Key,Value} ->
	    insert1(Key,Value,DbT),
	    main_db_loop(DbT);
	{From,delete,Key} ->
	    delete(Key,DbT),
	    main_db_loop(DbT);
	{From,lookup,Key} ->
	    lookup(Key,From,DbT),
	    main_db_loop(DbT);
	{From,lookup,Mod,Key} ->
	    lookup(Mod,Key,From,DbT),
	    main_db_loop(DbT);
	{From,lookup,Mod,Name,Arity} ->
	    lookup(Mod,Name,Arity,From,DbT),
	    main_db_loop(DbT);
	{From,clear,UsePid} ->       % Remove usage in DB.
	    rm_usage(UsePid,DbT),
	    main_db_loop(DbT);
	{From,cr_new_module,Module} ->
	    new_mod(From,Module,DbT),
	    main_db_loop(DbT);
	{From,delete_all_breaks} ->
	    del_all_breaks(DbT),
	    main_db_loop(DbT);
	{From,delete_all_breaks,Module} ->
	    delete_all_breaks(DbT,Module),
	    main_db_loop(DbT);
	{From,get_all_breaks} ->
	    Breaks = get_all_breaks(DbT,get_db(DbT,breakpoints)),
	    From ! {self(),all_breaks,Breaks},
	    main_db_loop(DbT);
	{From,get_all_breaks,Module} ->
	    Breaks = get_all_breaks(DbT,Module,
				    get_db(DbT,breakpoints)),
	    From ! {self(),all_breaks,Module,Breaks},
	    main_db_loop(DbT);
	{From,get_all_funcs,Module} ->
	    Funcs = get_all_funcs(DbT,Module),
	    From ! {self(),all_funcs,Funcs},
	    main_db_loop(DbT);
	{From,del_mod,Module} ->
	    del_mod(DbT,Module),
	    main_db_loop(DbT);
	{From,get_mod_db,Module,UsePid} ->
	    get_mod_db(DbT,From,Module,UsePid),
	    main_db_loop(DbT);
	{From,which_db,Module,UsePid} ->
	    which_db(From,Module,UsePid,DbT),
	    main_db_loop(DbT);
	{From, stop} ->
	    exit(stopped)
    end.


%%% -------------------------------------------------------
%%% Insert a value to specified database entry.
%%% -------------------------------------------------------
insert1(trace,Value,DbT) ->
    dbg_ets:insert(DbT,{trace,Value});
insert1(stack_trace,Value,DbT) ->
    dbg_ets:insert(DbT,{stack_trace,Value});
insert1(interpret,Value,DbT) ->
    dbg_ets:insert(DbT,{interpret,Value});
insert1(Break,Options,DbT) when tuple(Break),
                                size(Break) == 2,
                                integer(element(2,Break)) ->
    Breaks = get_db(DbT,breakpoints),
    case lists:member(Break,Breaks) of
	true ->
	    dbg_ets:insert(DbT,{Break,Options});
	_ ->
	    dbg_ets:insert(DbT,{Break,Options}),
	    dbg_ets:insert(DbT,{breakpoints,[Break|Breaks]})
    end;
insert1(Key,Value,DbT) -> % Unknown Key.
    dbg_ets:insert(DbT,{Key,Value}).


%%% -------------------------------------------------------
%%% Delete a break point from the global database.
%%% No other key is possible to delete.
%%% -------------------------------------------------------

delete(Break,DbT) when tuple(Break),
                       size(Break) == 2,
                       integer(element(2,Break)) ->
    Breaks = get_db(DbT,breakpoints),
    case lists:member(Break,Breaks) of
	true ->
	    dbg_ets:delete(DbT,Break),
	    dbg_ets:insert(DbT,{breakpoints,
			    lists:delete(Break,Breaks)});
	_ ->
	    false
    end;
delete(_,DbT) ->
    false.


%%% -------------------------------------------------------
%%% Lookup a key in the global database.
%%% -------------------------------------------------------

lookup(trace,From,DbT) ->
    From ! {self(),look_resp,{ok,get_db(DbT,trace)}};
lookup(stack_trace,From,DbT) ->
    From ! {self(),look_resp,{ok,get_db(DbT,stack_trace)}};
lookup(interpret,From,DbT) ->
    From ! {self(),look_resp,{ok,get_db(DbT,interpret)}};
lookup(breakpoints,From,DbT) ->
    From ! {self(),look_resp,{ok,get_db(DbT,breakpoints)}};
lookup(Break,From,DbT) ->
    case get_db(DbT,Break) of
	undefined ->
	    From ! {self(),look_resp,not_found};
	Options ->
	    From ! {self(),look_resp,{ok,Options}}
    end;
lookup(_,From,DbT) ->
    From ! {self(),look_resp,not_found}.

%%% -------------------------------------------------------
%%% Lookup a key in a module table (not specified).
%%% The first found DbRef is used.
%%% -------------------------------------------------------

lookup(Mod,Key,From,DbT) ->
    case get_db(DbT,{Mod,refs}) of
	undefined ->
	    From ! {self(),look_resp,not_found};
	[DbRef|_] ->
	    case get_db(DbRef,Key) of
		undefined ->
		    From ! {self(),look_resp,not_found};
		Value ->
		    From ! {self(),look_resp,{ok,Value}}
	    end
    end.

lookup(Mod, Name, Arity, From, DbT) ->
    case get_db(DbT,{Mod,refs}) of
	undefined ->
	    From ! {self(),look_resp,not_found};
	[DbRef|_] ->
	    case dbg_ets:match_object(DbRef, {{Mod,Name,Arity,'_'},'_'}) of
		[{_,Cs}] ->
		    From ! {self(),look_resp,{ok,Cs}};
		[] ->
		    From ! {self(),look_resp,not_found}
	    end
    end.

%%% -------------------------------------------------------
%%% Delete all references to Pid from the usage entries
%%% of Module tables.
%%% If Pid is the last user of an old module version, 
%%% delete that module from the database.
%%% -------------------------------------------------------

rm_usage(Pid,DbT) ->
    Modules = get_db(DbT,interpret),
    rm_usage(Pid,DbT,Modules).

rm_usage(Pid,DbT,[Mod|Mods]) ->
    DbRefs = get_db(DbT,{Mod,refs}),
    rm_usage(Pid,DbT,Mod,DbRefs),
    rm_usage(Pid,DbT,Mods);
rm_usage(_,_,[]) ->
    true.

rm_usage(Pid,DbT,Mod,[ModDb|ModDbs]) ->
    UsePids = get_db(DbT,ModDb),
    case lists:member(Pid,UsePids) of
	true ->
	    dbg_ets:insert(DbT,{ModDb,lists:delete(Pid,UsePids)});
	_ ->
	    rm_usage_and_kill(Pid,DbT,Mod,ModDbs)
    end;
rm_usage(_,_,_,_) ->
    true.
	    
rm_usage_and_kill(Pid,DbT,Mod,[ModDb|ModDbs]) ->
    UsePids = get_db(DbT,ModDb),
    case lists:member(Pid,UsePids) of
	true when length(UsePids) == 1 ->
	    dbg_ets:insert(DbT,{{Mod,refs},
			    lists:delete(ModDb,get_db(DbT,{Mod,refs}))}),
	    dbg_ets:delete(DbT,ModDb),
	    dbg_ets:delete(ModDb),
	    rm_usage_and_kill(Pid,DbT,Mod,ModDbs);
	true ->
	    dbg_ets:insert(DbT,{ModDb,lists:delete(Pid,UsePids)}),
	    rm_usage_and_kill(Pid,DbT,Mod,ModDbs);
	_ ->
	    rm_usage_and_kill(Pid,DbT,Mod,ModDbs)
    end;
rm_usage_and_kill(_,_,_,[]) ->
    true.


%%% -------------------------------------------------------
%%% Create a new database table for a new module.
%%% Insert a reference to the new table into the global 
%%% table.
%%% If an old version of Module exists without usage,
%%% delete the old table (and it's references).
%%% -------------------------------------------------------

new_mod(From,Module,DbT) ->
    ModDb = dbg_ets:new(Module,[ordered_set,public]),
    From ! {self(),created,Module,ModDb},
    add_new_mod(ModDb,Module,DbT).

add_new_mod(ModDb,Module,DbT) ->
    case get_db(DbT,{Module,refs}) of
	undefined ->
	    dbg_ets:insert(DbT,{{Module,refs},[ModDb]}),
	    dbg_ets:insert(DbT,{ModDb,[]});
	Dbs ->
	    Dbs1 = del_old_versions(Dbs,DbT),
	    dbg_ets:insert(DbT,{{Module,refs},[ModDb|Dbs1]}),
	    dbg_ets:insert(DbT,{ModDb,[]})
    end.

%%
%% Delete old versions of interpreted modules if not used.
%%

del_old_versions(Dbs,DbT) ->
    del_old_versions(Dbs,DbT,Dbs).

del_old_versions([DbRef|Dbs],DbT,Dbs00) ->
    case get_db(DbT,DbRef) of
	[] ->
	    dbg_ets:delete(DbRef),
	    dbg_ets:delete(DbT,DbRef),
	    del_old_versions(Dbs,DbT,lists:delete(DbRef,Dbs00));
	_ ->
	    del_old_versions(Dbs,DbT,Dbs00)
    end;
del_old_versions([],_,Dbs00) ->
    Dbs00.

%%% ---------------------------------------------------------
%%% Delete all break points from the database.
%%% ---------------------------------------------------------

del_all_breaks(DbT) ->
    Breaks = get_db(DbT,breakpoints),
    dbg_ets:insert(DbT,{breakpoints,[]}),
    del_all_breaks(DbT,Breaks).

del_all_breaks(DbT,[Break|Breaks]) ->
    dbg_ets:delete(DbT,Break),
    del_all_breaks(DbT,Breaks);
del_all_breaks(_,[]) ->
    true.

%%% ---------------------------------------------------------
%%% Delete all break points associated to module Mod.
%%% ---------------------------------------------------------

delete_all_breaks(DbT,Mod) ->
    NewBreaks = delete_all_breaks1(DbT,Mod,get_db(DbT,breakpoints)),
    dbg_ets:insert(DbT,{breakpoints,NewBreaks}).

delete_all_breaks1(DbT,Mod,[{Mod,Line}|Breaks]) ->
    dbg_ets:delete(DbT,{Mod,Line}),
    delete_all_breaks1(DbT,Mod,Breaks);
delete_all_breaks1(DbT,Mod,[Br|Breaks]) ->
    [Br|delete_all_breaks1(DbT,Mod,Breaks)];
delete_all_breaks1(_,_,[]) ->
    [].


    
%%% ---------------------------------------------------------
%%% Get all break points in the database.
%%% Combine the break point location {Mod,Line} with the 
%%% Options associated to the break point.
%%% ---------------------------------------------------------

get_all_breaks(DbT,[Break|Breaks]) ->
    Options = get_db(DbT,Break),
    [{Break,Options}|get_all_breaks(DbT,Breaks)];
get_all_breaks(_,[]) ->
    [].


%%% ---------------------------------------------------------
%%% Get all break points in the database assocaited to
%%% module Mod.
%%% Combine the break point location {Mod,Line} with the 
%%% Options associated to the break point.
%%% ---------------------------------------------------------

get_all_breaks(DbT,Mod,[{Mod,Line}|Breaks]) ->
    Options = get_db(DbT,{Mod,Line}),
    [{{Mod,Line},Options}|get_all_breaks(DbT,Mod,Breaks)];
get_all_breaks(DbT,Mod,[_|Breaks]) ->
    get_all_breaks(DbT,Mod,Breaks);
get_all_breaks(_,_,[]) ->
    [].


%%% ---------------------------------------------------------
%%% Get all functions in Module.
%%% Return: [{Name,Arity}] or [].
%%% ---------------------------------------------------------

get_all_funcs(DbT,Module) ->
    case get_db(DbT,{Module,refs}) of
	undefined ->
	    [];
	[DbRef|_] ->
	    Fs = lists:map(fun(Fun) ->
				   list_to_tuple(Fun)
			   end,
			   dbg_ets:match(DbRef,{{'$1','$2'},'_'})),
	    lists:sort(Fs)
    end.



%%% -------------------------------------------------------
%%% The Module is not interpreted any longer, delete
%%% existing database tables and entries.
%%% If there are processes marked as using this Module,
%%% notify them, so that they can terminate if necessary.

%%% FIXME: Don't unnecessarily kill module users.
%%% FIXME: And perhaps, above all, don't do it from here (the DB).
%%% -------------------------------------------------------

del_mod(DbT,Module) ->
    case get_db(DbT,{Module,refs}) of
	undefined ->
	    true;
	Dbs ->
	    dbg_ets:delete(DbT,{Module,refs}),
	    notify_mod_users(Dbs,DbT, Module)
    end.

notify_mod_users(DbRefs,DbT, Module) ->
    NotifyAndCleanup =
	fun(DbRef) ->
		UsePids = get_db(DbT,DbRef),
		notify_pids(UsePids, Module),
		dbg_ets:delete(DbT,DbRef),
		dbg_ets:delete(DbRef)
	end,
    lists:foreach(NotifyAndCleanup, DbRefs).

notify_pids(Pids, Module) ->
    OldCodeInModule =
	fun(Pid) ->
		dbg_iserver_api:old_code(Pid, Module)
	end,
    lists:foreach(OldCodeInModule, Pids).


%%% -------------------------------------------------------
%%% A new process wants a database table for usage of the
%%% module Module.
%%% If the module exists, mark UsePid as using the module
%%% and return the database table reference.
%%% -------------------------------------------------------

get_mod_db(DbT,From,Module,UsePid) ->
    case get_db(DbT,{Module,refs}) of
	undefined ->
	    From ! {self(),get_mod_db_resp,not_found};
	[DbRef|_] ->
	    From ! {self(),get_mod_db_resp,DbRef},
	    Usage = get_db(DbT,DbRef),
	    dbg_ets:insert(DbT,{DbRef,[UsePid|Usage]})
    end.




%%% -------------------------------------------------------
%%% Get the database table used by the UsePid process
%%% for the module Module.
%%%
%%% For every interpreted module there exists an entry in the
%%% database containing information about which processes
%%% using this version of Module.
%%% -------------------------------------------------------

which_db(From,Module,UsePid,DbT) ->
    case which_db1(DbT,Module,UsePid,get_db(DbT,{Module,refs})) of
	{ok,Db} ->
	    From ! {self(),which_db_resp,Db};
	_ ->
	    From ! {self(),which_db_resp,not_found}
    end.

which_db1(DbT,Module,UsePid,undefined) ->
    not_found;
which_db1(DbT,Module,UsePid,[]) ->
	    not_found;
which_db1(DbT,Module,UsePid,Dbs) ->
    case which_db(DbT,UsePid,Dbs) of
	not_found ->
	    {ok,hd(Dbs)};
	Res ->
	    Res
    end.

which_db(DbT,Pid,[Db|Dbs]) ->
    UsePids = get_db(DbT,Db),
    case lists:member(Pid,UsePids) of
	true ->
	    {ok,Db};
	_ ->
	    which_db(DbT,Pid,Dbs)
    end;
which_db(_,_,[]) ->
    not_found.




%%% -------------------------------------------------------
%%% Get the associated value with Key from the database
%%% table Db.
%%% -------------------------------------------------------

get_db(Db,Key) ->
    case dbg_ets:lookup(Db,Key) of
	[{Key,Value}] ->
	    Value;
	_ ->
	    undefined
    end.
