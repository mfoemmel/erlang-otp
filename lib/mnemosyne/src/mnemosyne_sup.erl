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
-module(mnemosyne_sup).
%%% adapted from mnesia_kernel_sup

-behaviour(application).
-behaviour(supervisor).

-export([start/0, start/2, stop/1, init/1]).

%% debug
-export([kill/0, supervisor_timeout/1]).

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

stop(StartArgs) ->
    ok.

start() ->
    SupName = {local,?MODULE},
    supervisor:start_link(SupName, ?MODULE, []).

init([]) -> % Supervisor
    init();
init([[]]) -> % Application
    init();
init(BadArg) ->
    {error, {badarg, BadArg}}.

init() ->
    Flags = {one_for_all, 3, 3600}, % Trust the top supervisor
    SupervisedProcs = 
	[worker_spec(mnemosyne_catalog, timer:seconds(1), 
		     [gen_server, mnesia])
	],
    {ok, {Flags, SupervisedProcs}}.

worker_spec(Name, KillAfter, Modules) ->
    KA = supervisor_timeout(KillAfter),
    {Name, {Name, start, []}, permanent, KA, worker, [Name] ++ Modules}.

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% debug functions

kill() ->
    Appl = get_application(),
    Modules = mnemosyne:ms(),
    Kill = fun(Name) -> catch exit(whereis(Name), kill) end,
    lists:foreach(Kill, Modules),
    lists:foreach(fun ensure_dead/1, Modules),
    timer:sleep(10),
    case lists:keymember(Appl, 1, application:which_applications()) of
	true -> kill();
	false -> ok
    end.

get_application() ->
    case whereis(mnemosyne_catalog) of
	undefined ->
	    mnemosyne;
	Pid ->
	    case application:get_application(Pid) of
		{ok, Appl} ->
		    Appl;
		undefined ->
		    mnemosyne
	    end
    end.

ensure_dead(Name) ->
    case whereis(Name) of
	undefined ->
	    ok;
	Pid when pid(Pid) ->
	    exit(Pid, kill),
	    timer:sleep(10),
	    ensure_dead(Name)
    end.

-ifdef(debug_shutdown).
supervisor_timeout(_KillAfter) -> timer:hours(500).
-else.
supervisor_timeout(KillAfter) -> KillAfter.
-endif.    
