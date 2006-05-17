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
-module(mnesia_session_lib).

%%%----------------------------------------------------------------------
%%% Purpose : Main module for the mnesia_session_lib application
%%%----------------------------------------------------------------------

-export([start/0, start/1, stop/0, get_env/1]).
-export([lookup_connector/0,
	 lookup_corba_connector/0,
	 get_initial_debug/0]).

%% debug
-export([ms/0, nc/0, nc/1, ni/0, ni/1, kill/0, lkill/0]).
-export([verbose/3, debug/3]).

-define(APPLICATION, mnesia_session).
-define(SUPERVISOR, mnesia_session_top_sup).


%% Returns the registered name of the connector
lookup_connector() ->
    Name = mnesia_connector,
    case whereis(Name) of
	Pid when pid(Pid) ->
	    Pid;
	_ ->
	    exit({not_started, Name})
    end.

%% Returns the object key of the corba connector
lookup_corba_connector() ->
    case get_env(enable_corba) of
	true ->
	    Name = get_env(corba_connector_name),
	    NS = corba:resolve_initial_references("NameService"),
	    Key = 'CosNaming_NamingContext':resolve(NS, Name),
	    case catch corba:get_pid(Key) of
		Pid when pid(Pid) ->
		    % Return object key for corba connector
		    Key; 
		_ ->
		    exit({not_started, Name})
	    end;
	false ->
	    exit(corba_not_enabled)
    end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handle configuration parameters

get_env(E) ->
    case application:get_env(?APPLICATION, E) of
	{ok, Val} ->
	    check_env(E, Val);
	undefined ->
	    check_env(E, default_env(E))
    end.

check_env(Env, Val) ->
    case catch do_check_env(Env, Val) of
	{'EXIT', _Reason} ->
	    exit({bad_config_env, Env, Val});
	NewVal ->
	    NewVal
    end.

default_env(debug) -> none;
default_env(gen_server_module) -> gen_server;
default_env(enable_corba) -> false;
default_env(corba_connector_name) ->
    DefaultName = "mnesia_corba_connector",
    InitialId = lname_component:create(),
    NC = lname_component:set_id(InitialId, DefaultName),
    InitialName = lname:create(),
    lname:insert_component(InitialName, 1, NC).

do_check_env(enable_corba, B) -> is_bool(B);
do_check_env(gen_server_module, M) when atom(M) -> M;
do_check_env(debug, D) -> is_debug(D);
do_check_env(corba_connector_name, N) -> is_corba_name(N).

is_bool(true) -> true;
is_bool(false) -> false.

is_debug(none) -> none;
is_debug(verbose) -> verbose;
is_debug(debug) -> debug;
is_debug(trace) -> trace.
    
is_corba_name(N) -> true == lname:check_name(N), N.

get_initial_debug() ->
    Debug = mnesia_session_lib:get_env(debug),
    verbose("Env debug: ~p~n", [Debug], Debug),
    Debug.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% debug functions

stop() ->
    application:stop(?APPLICATION).

start() ->
    application:start(?APPLICATION).

start(ExtraEnv) when list(ExtraEnv) ->
    case application:load(?APPLICATION) of
	ok ->
	    patched_start(ExtraEnv);
	{error,{already_loaded, ?APPLICATION}} ->
	    patched_start(ExtraEnv);
	Error ->
	    Error
    end;
start(ExtraEnv) ->
    {error, {badarg, ExtraEnv}}.

patched_start([{Env, Val} | Tail]) when atom(Env) ->
    case patch_env(Env, Val) of
	{error, Reason} ->
	    {error, Reason};
	_NewVal ->
	    patched_start(Tail)
    end;
patched_start([Head | _]) ->
    {error, {bad_type, Head}};
patched_start([]) ->
    start().

patch_env(Env, Val) ->
    case catch do_check_env(Env, Val) of
	{'EXIT', _Reason} ->
	    {error, {bad_type, Env, Val}};
	NewVal ->
	    application:set_env(?APPLICATION, Env, NewVal),
	    NewVal
    end.
   
kill() -> 
    rpc:multicall(?SUPERVISOR, kill, []).
lkill() -> 
    ?SUPERVISOR:kill().

ms() ->
    [
     mnesia_SystemInfo,
     mnesia_TableDef,
     mnesia_TableInfo,
     mnesia_connector,
     mnesia_connector_impl,
     mnesia_corba_connector,
     mnesia_corba_connector_impl,
     mnesia_corba_session,
     mnesia_registry,
     mnesia_session,
     mnesia_session_impl,
     mnesia_session_lib,
     mnesia_session_top_sup,
     oe_mnesia_corba_session,
     oe_mnesia_session
    ].

nc() ->
    Mods = ms(),
    nc(Mods).
nc([Mod | Tail]) ->
    format( " ~w ", [Mod]),
    c:nc(Mod, [{d, debug}]),
    nc(Tail);
nc([]) ->
    format( " DONE~n",[]).

ni() ->
    Mods = ms(),
    ni(Mods).
ni([Mod | Tail]) ->
    format( " ~w ", [Mod]),
    int:ni(Mod, [{d, debug}]),
    ni(Tail);
ni([]) ->
    format( " DONE~n",[]).

verbose(_Format, _Args, none) ->  ok;
verbose(Format, Args, _Debug) ->  format(Format, Args).
 
debug(_Format, _Args, none) ->    ok;
debug(_Format, _Args, verbose) -> ok;
debug(Format, Args, _Debug) ->    format(Format, Args).
 
%% trace(_Format, _Args, none) ->    ok;
%% trace(_Format, _Args, verbose) -> ok;
%% trace(_Format, _Args, debug) ->   ok;
%% trace(Format, Args, _Debug) ->    io:format(Format, Args).
  
format(Format, Args) ->
    io:format("~p: " ++ Format, [?APPLICATION | Args]).
