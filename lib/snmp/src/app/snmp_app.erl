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
-module(snmp_app).

-behaviour(application).

-include("snmp_debug.hrl").


%%%-----------------------------------------------------------------
%%%  This module implements the SNMP application.
%%%-----------------------------------------------------------------
-export([start/2, stop/1, config_change/3]).

start(Type, []) ->
    ?d("start -> entry with"
      "~n   Type. ~p", [Type]),
    %% This is the new snmp application config format
    %% First start the (new) central supervisor,
    {ok, Pid} = snmp_app_sup:start_link(),
    case entities() of
	{ok, Entities} ->
	    %% and then the entities
	    ok = start_entities(Type, Entities),
	    {ok, Pid};
	_ ->
	    %% Assume old style snmp (agent) application config format
	    old_start(Type)
    end.

entities() ->
    entities([agent, manager], []).

entities([], []) ->
    error;
entities([], E) ->
    {ok, lists:reverse(E)};
entities([ET|ETs], E) ->
    case application:get_env(snmp, ET) of
	{ok, Conf} ->
	    entities(ETs, [{ET, Conf}|E]);
	_ ->
	    entities(ETs, E)
    end.
	
start_entities(_Type, []) ->
    ok;
start_entities(Type, [{agent, Opts}|Entities]) ->
    case snmp_app_sup:start_agent(Type, Opts) of
	{ok, _} ->
	    start_entities(Type, Entities);
	Error ->
	    Error
    end;
start_entities(Type, [{manager, Opts}|Entities]) ->
    case snmp_app_sup:start_manager(Type, Opts) of
	{ok, _} ->
	    start_entities(Type, Entities);
	Error ->
	    Error
    end;
start_entities(Type, [BadEntitie|Entities]) ->
    error_msg("Bad snmp configuration: ~n: ~p", [BadEntitie]), 
    start_entities(Type, Entities).


old_start(Type) ->
    snmpa_app:start(Type).


stop(_) ->
    ok.

%%-----------------------------------------------------------------
%% The presence of this function means that we will accept changes
%% in the configuration parameters.  However, we won't react upon
%% those changes until the app is restarted.  So we just return
%% ok.
%%-----------------------------------------------------------------
config_change(_Changed, _New, _Removed) ->
    ok.

%% ---------------------------------------------------------------------
                        
error_msg(F, A) ->
    error_logger:error_msg("~w: " ++ F ++ "~n", [?MODULE|A]).

