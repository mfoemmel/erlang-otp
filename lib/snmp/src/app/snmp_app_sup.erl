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
-module(snmp_app_sup).

-include("snmp_debug.hrl").

-behaviour(supervisor).


%% External exports
-export([start_link/0, start_agent/2, start_manager/2]).

%% Internal exports
-export([stop/0]).


%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
start_link() ->
    ?d("start_link -> entry",[]),
    SupName = {local, ?MODULE},
    supervisor:start_link(SupName, ?MODULE, []).

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
    

start_agent(Type, Opts) ->
    ?d("start_agent -> entry with"
	"~n   Type: ~p"
	"~n   Opts: ~p", [Type, Opts]),
    start_sup_child(snmpa_supervisor, permanent, [Type, Opts]).


start_manager(Type, Opts) ->
    ?d("start_manager -> entry with"
	"~n   Type: ~p"
	"~n   Opts: ~p", [Type, Opts]),
    start_sup_child(snmpm_supervisor, transient, [Type, Opts]).


%%%-------------------------------------------------------------------
%%% Callback functions from supervisor
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}   
%%--------------------------------------------------------------------
init(_Args) ->
    ?d("init -> entry", []),
    Flags = {one_for_one, 0, 1},
    Sups  = [],
    {ok, {Flags, Sups}}.


%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------


start_sup_child(Mod, Type, Args) ->
    Spec = sup_spec(Mod, Type, Args), 
    supervisor:start_child(?MODULE, Spec).

sup_spec(Name, Type, Args) ->
    {Name, 
     {Name, start_link, Args}, 
     Type, 2000, supervisor, [Name, supervisor]}.


% i(F) ->
%     i(F, []).

% i(F, A) ->
%     io:format("~p: " ++ F ++ "~n", [?MODULE|A]).

