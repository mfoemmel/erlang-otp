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
-module(snmp_agent_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/1, start_link/2, start_subagent/3, stop_subagent/1]).

%% Internal exports
-export([init/1]).

%%%-----------------------------------------------------------------
%%% This is a supervisor for the mib processes.  Each agent has one
%%% mib process.
%%%-----------------------------------------------------------------
start_link(Prio) ->
    supervisor:start_link({local, snmp_agent_sup}, snmp_agent_sup,
			  [Prio, []]).
start_link(Prio, AgentSpec) ->
    supervisor:start_link({local, snmp_agent_sup}, snmp_agent_sup,
			  [Prio, [AgentSpec]]).

start_subagent(ParentAgent, Subtree, Mibs) ->
    Max = find_max(supervisor:which_children(snmp_agent_sup), 1),
    [{_, Prio}] = ets:lookup(snmp_agent_table, priority),
    Ref = make_ref(),
    Agent = {{sub_agent, Max},
	     {snmp_agent, start_link,
	      [ParentAgent, Ref, [{mibs,Mibs}, {misc_sup, snmp_misc_sup}]]},
	     permanent, 2000, worker, [snmp_agent]},
    case supervisor:start_child(snmp_agent_sup, Agent) of
	{ok, SA} -> 
	    snmp_agent:register_subagent(ParentAgent, Subtree, SA),
	    {ok, SA};
	Error ->
	    Error
    end.

stop_subagent(SubAgentPid) ->
    case find_name(supervisor:which_children(snmp_agent_sup), SubAgentPid) of
	undefined -> no_such_child;
	Name ->
	    supervisor:terminate_child(snmp_agent_sup, Name),
	    supervisor:delete_child(snmp_agent_sup, Name),
	    ok
    end.

init([Prio, Children]) ->
    %% 20 restarts in ten minutes.  If the agent crashes and restarts,
    %% it may very well crash again, because the management application
    %% tries to resend the very same request.  This depends on the resend
    %% strategy used by the management application.
    SupFlags = {one_for_one, 20, 600},
    {ok, {SupFlags, Children}}.


find_max([{{sub_agent, N}, _, _, _} | T], M) when N >= M -> find_max(T, N+1);
find_max([_|T], M) -> find_max(T, M);
find_max([], M) -> M.

find_name([{Name, Pid, _, _} | T], Pid)-> Name;
find_name([_|T], Pid) -> find_name(T, Pid);
find_name([], Pid) -> undefined.
