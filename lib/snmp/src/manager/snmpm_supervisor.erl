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
-module(snmpm_supervisor).

-behaviour(supervisor).


%% External exports
-export([start_link/2, stop/0]).

%% supervisor callbacks
-export([init/1]).


-define(SERVER, ?MODULE).

-include("snmp_debug.hrl").


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
start_link(_Type, Opts) ->
    ?d("start_link -> entry with"
       "~n   Opts: ~p", [Opts]),
    SupName = {local, ?MODULE}, 
    supervisor:start_link(SupName, ?MODULE, [Opts]).

stop() ->
    ?d("stop -> entry", []),
    case whereis(?SERVER) of
	Pid when pid(Pid) ->
	    ?d("stop -> Pid: ~p", [Pid]),
	    exit(Pid, shutdown),
	    ?d("stop -> stopped", []),
	    ok;
	_ ->
	    ?d("stop -> not running", []),
	    not_running
    end.


%%%-------------------------------------------------------------------
%%% Callback functions from supervisor
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%--------------------------------------------------------------------
init([Opts]) ->
    ?d("init -> entry with"
       "~n   Opts: ~p", [Opts]),
    Flags   = {one_for_all, 2, 500},
    Config  = worker_spec(snmpm_config, [Opts], [gen_server]),
    MiscSup = sup_spec(snmpm_misc_sup, []),
    Server  = worker_spec(snmpm_server, [], [gen_server]),
    Sups    = [Config, MiscSup, Server],
    {ok, {Flags, Sups}}.


%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

sup_spec(Name, Args) ->
    {Name, 
     {Name, start_link, Args}, 
     transient, 2000, supervisor, [Name,supervisor]}.

worker_spec(Name, Args, Modules) ->
    {Name, 
     {Name, start_link, Args}, 
     transient, 2000, worker, [Name] ++ Modules}.



