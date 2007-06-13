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
-module(mnesia_session_top_sup).

%%%----------------------------------------------------------------------
%%% Purpose : The top supervisor for mnesia_session
%%%----------------------------------------------------------------------

-behaviour(application).
-behaviour(supervisor).

-export([start/0, start/2, stop/1, init/1, 
	 start_link_connector/1]).

%% debug
-export([kill/0, supervisor_timeout/1]).

-define(SERVER_NAME, ?MODULE).
-define(APPLICATION, mnesia_session).
-define(VERBOSE(F, A),
	mnesia_session_lib:verbose(F, A, mnesia_session_lib:get_env(debug))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% application and supervisor callback functions

start(normal, Args) ->
    SupName = {local,?MODULE},
    case supervisor:start_link(SupName, ?MODULE, [Args]) of
	{ok, Pid} ->
	    {ok, Pid, {normal, Args}};
	Error -> 
	    Error
    end;
start(_, _) ->
    {error, badarg}.

start() ->
    SupName = {local,?MODULE},
    supervisor:start_link(SupName, ?MODULE, []).

stop(_StartArgs) ->
    ok.

init([]) -> % Supervisor
    init();
init([[]]) -> % Application
    init();
init(BadArg) ->
    {error, {badarg, BadArg}}.

init() ->
    mnesia_session_lib:get_initial_debug(),
    Flags = {one_for_one, 4, 3600},
    KillAfter = timer:seconds(1),
    KA = supervisor_timeout(KillAfter),
    GenServerProcs = gen_server_procs(KA),
    CorbaProcs = corba_procs(KA),
    Workers = GenServerProcs ++ CorbaProcs,
    {ok, {Flags, Workers}}.

session_sup_spec(Name) ->
    Mods = [mnesia_session_sup, supervisor],
    MFA = {mnesia_session_sup, start, [Name]},
    {Name, MFA, permanent, infinity, supervisor, Mods}.
    
gen_server_procs(KA) ->
    Name = mnesia_connector,
    GenMod = mnesia_session_lib:get_env(gen_server_module),
    ?VERBOSE("Env gen_server_module: ~p~n", [GenMod]),
    Mods = [Name, GenMod, mnesia],
    MFA = {?MODULE, start_link_connector, [Name]},
    Conn = {Name, MFA, permanent, KA, worker, Mods},
    SessionSup = session_sup_spec(mnesia_session_sup),    
    [Conn, SessionSup].

corba_procs(KA) ->
    case corba_enabled() of
	false ->
	    [];
	true ->
	    Name = mnesia_corba_connector,
	    Mods = [Name, corba, mnesia],
	    MFA = {?MODULE, start_link_connector, [Name]},
	    Conn = {Name, MFA, permanent, KA, worker, Mods},
	    [Conn]
    end.

corba_enabled() ->
    EnabledCorba = mnesia_session_lib:get_env(enable_corba),
    ?VERBOSE("Env enable_corba: ~p~n", [EnabledCorba]),
    case EnabledCorba of
	false -> 
	    false;
	true ->
	    Appls = application:which_applications(),
	    OrberStarted = lists:keymember(orber, 1, Appls),
	    ?VERBOSE("orber application started: ~p~n",
		 [OrberStarted]),
	    case OrberStarted of
		true -> true;
		false -> exit(orber_not_started)
	    end
    end.

start_link_connector(mnesia_connector) ->
    Env = [],
    Name = {local, mnesia_connector},
    case mnesia_connector:oe_create_link(Env, Name) of
	{ok, Pid} ->
	    {ok, Pid};
	Other ->
	    Error = {connector_start_error, Other},
	    ?VERBOSE("~p~n", [Error]),
	    Other
    end;

%% Start a corba connector and  bind it to a name
start_link_connector(mnesia_corba_connector) ->
    CorbaName = mnesia_session_lib:get_env(corba_connector_name),
    ?VERBOSE("Env corba_connector_name: ~p~n", [CorbaName]),
    catch
	begin
	    register_types(),
	    Env = [],
	    Opts = [{regname, {local, mnesia_corba_connector}},{sup_child,true}],
	    CreateRes = (catch mnesia_corba_connector:oe_create_link(Env, Opts)),
	    {ok, Pid, Key} = exit_to_error(CreateRes, corba_connector_create),
	    NS = corba:resolve_initial_references("NameService"),
	    BindRes = (catch 'CosNaming_NamingContext':bind(NS, CorbaName, Key)),
	    ok = exit_to_error(BindRes, corba_bind_error),
	    {ok, Pid}
	end.

exit_to_error(Result, Why) ->
    case Result of
	{'EXCEPTION', Reason} ->
	    Error = {Why, {'EXCEPTION', Reason}},
	    ?VERBOSE("~p~n", [Error]),
	    throw({error, Error});
	{'EXIT', Reason} ->
	    Error = {Why, {'EXIT', Reason}},
	    ?VERBOSE("~p~n", [Error]),
	    throw({error, Error});
	Other ->
	    Other
    end.
    
register_types() ->
    TypeId = mnesia_corba_session:typeID(),
    OE_IFR = orber_ifr:find_repository(),
    case orber_ifr:'Repository_lookup_id'(OE_IFR, TypeId) of
	[] ->
	    RegRes = (catch oe_mnesia_corba_session:oe_register()),
	    case exit_to_error(RegRes, corba_register_error) of
		{error, Reason} ->
		    throw({error, Reason});
		ok ->
		    ok
	    end;
	_ ->
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% debug functions

kill() ->
    Modules = mnesia_session_lib:ms(),
    Kill = fun(Name) -> catch exit(whereis(Name), kill) end,
    lists:foreach(Kill, Modules),
    lists:foreach(fun ensure_dead/1, Modules),
    timer:sleep(10),
    Applications = application:which_applications(),
    case lists:keymember(?APPLICATION, 1, Applications) of
	true -> kill();
	false -> ok
    end.

ensure_dead(Name) ->
    case whereis(Name) of
	undefined ->
	    ok;
	Pid when is_pid(Pid) ->
	    exit(Pid, kill),
	    timer:sleep(10),
	    ensure_dead(Name)
    end.

-ifdef(debug_shutdown).
supervisor_timeout(_KillAfter) -> timer:hours(500).
-else.
supervisor_timeout(KillAfter) -> KillAfter.
-endif.    
