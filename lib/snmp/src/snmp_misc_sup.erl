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
-module(snmp_misc_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/1, 
	 start_mib/4, start_mib/5, stop_mib/2,
	 start_net_if/4, start_net_if/5, stop_net_if/2]).

%% Internal exports
-export([init/1]).

%%%-----------------------------------------------------------------
%%% This is a supervisor for the mib and net_ifprocesses.
%%% Each agent has one mib process.
%%%-----------------------------------------------------------------

start_link(SupName) ->
    supervisor:start_link({local, SupName}, snmp_misc_sup, []).

%%-----------------------------------------------------------------
%% When the agent starts, it calls this function. If there already
%% exist a mib process for the agent, this one is used. Otherwise
%% a new one is started.
%%-----------------------------------------------------------------
start_mib(SupName, Ref, Mibs, Prio) ->
    start_mibserver(SupName, Ref, [Mibs,Prio]).

start_mib(SupName, Ref, Mibs, Prio, Opts) ->
    start_mibserver(SupName, Ref, [Mibs,Prio,Opts]).

start_mibserver(SupName, Ref, Args) ->
    Children = supervisor:which_children(SupName),
    case lists:keysearch({mib, Ref}, 1, Children) of
	{value, {_, Pid, _, _}} -> {ok, Pid};
	_ ->
	    Mib = {{mib, Ref}, 
		   {snmp_mib, start_link, Args},
		   transient, 10000, worker, [snmp_mib]},
	    supervisor:start_child(SupName, Mib)
    end.

stop_mib(SupName, Ref) ->
    case whereis(SupName) of
	undefined ->
	    ok;
	_ ->
	    supervisor:terminate_child(SupName, {mib, Ref}),
	    supervisor:delete_child(SupName, {mib, Ref})
    end.

start_net_if(SupName, Ref, Master, NetIfModule) ->
    start_netif(SupName, Ref, NetIfModule, [Master]).

start_net_if(SupName, Ref, Master, NetIfModule, Opts) ->
    start_netif(SupName, Ref, NetIfModule, [Master,Opts]).

start_netif(SupName, Ref, NetIfModule, Args) ->
    %% make sure we start from scratch...
    Children = supervisor:which_children(SupName),
    case lists:keysearch({net_if, Ref}, 1, Children) of
	{value, {_, _Pid, _, _}} ->
	    stop_net_if(SupName, Ref);
	_ ->
	    ok
    end,
    NetIf = {{net_if, Ref}, 
	     {NetIfModule, start_link, Args},
	     permanent, 2000, worker, [NetIfModule]},
    supervisor:start_child(SupName, NetIf).

stop_net_if(SupName, Ref) ->
    case whereis(SupName) of
	undefined ->
	    ok;
	_ ->
	    supervisor:terminate_child(SupName, {net_if, Ref}),
	    supervisor:delete_child(SupName, {net_if, Ref})
    end.


init([]) ->
    SupFlags = {one_for_all, 0, 3600},
    {ok, {SupFlags, []}}.
