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
%%%----------------------------------------------------------------------
%%% Purpose   : Application top supervisor
%%%----------------------------------------------------------------------

-module(jive_sup).

-behaviour(supervisor).

-export([start/0,
	 init/1]).

start() ->
    supervisor:start_link({local, jive_sup}, jive_sup, []).


init([]) ->

    % If one goes down we are in trouble, restart all.

    SupFlags = {one_for_all,10,3600},

    % This is the named process 'jive_server' that take care of all
    % administrative data, like permissions.

    ChildSpec1 = {jive_server,
		  {jive_server,start_server,[]},
		  permanent,
		  brutal_kill,
		  worker,
		  [jive_app,jive,jive_server]},

    % This process is waiting for connections and spawns new jive workers
    % when there is a new connection from the Java side.

    ChildSpec2 = {jive_broker,
		  {jive_broker,start,[]},
		  permanent,
		  brutal_kill,
		  worker,
		  [jive_broker]},

    % Last child is supervisor for the remporary processes worker processes

    ChildSpec3 = {jive_worker_sup,
		  {jive_worker_sup,start,[]},
		  permanent,
		  infinity,
		  supervisor,
		  [jive_worker_sup]},
    {ok, {SupFlags, [ChildSpec1, ChildSpec2, ChildSpec3]}}.

