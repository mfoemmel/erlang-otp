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
%%% Purpose   : Worker supervisor
%%%----------------------------------------------------------------------

-module(jive_worker_sup).

-behaviour(supervisor).

-export([start/0,init/1]).

start() ->
    supervisor:start_link({local,jive_worker_sup},jive_worker_sup,[]).


init([]) ->

    % If one goes down just let that child die, no restart.

    SupFlags = {simple_one_for_one,10,3600},

    % This is a template for the values a child created with
    % supervisor:start_child() will have.

    ChildSpec1 = {jive_worker,
		  {jive_worker,start,[]},
		  temporary,
		  brutal_kill,
		  worker,
		  [jive_worker]},
    {ok, {SupFlags, [ChildSpec1]}}.
