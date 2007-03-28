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

-ifdef(debug_shutdown).
supervisor_timeout(_KillAfter) -> timer:hours(500).
-else.
supervisor_timeout(KillAfter) -> KillAfter.
-endif.    
