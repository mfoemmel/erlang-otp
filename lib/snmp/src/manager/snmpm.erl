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
-module(snmpm).

%%----------------------------------------------------------------------
%% This module implements a simple SNMP manager for Erlang.
%%----------------------------------------------------------------------

%% User interface
-export([
	 %% Management API
	 start/1, 
	 start_link/1, 
	 stop/0, 

	 load_mib/1, unload_mib/1, 
	 which_mibs/0, 
	 name_to_oid/1, oid_to_name/1, 

	 register_user/3, register_user_monitor/3, unregister_user/1, 
	 which_users/0, 

	 register_agent/2, register_agent/3, register_agent/4, 
	 unregister_agent/2, unregister_agent/3,
	 which_agents/0, which_agents/1, 
	 agent_info/3, update_agent_info/5, 
	 
	 register_usm_user/3, 
	 which_usm_users/0, which_usm_users/1, 
	 usm_user_info/3, update_usm_user_info/4, 
	 
	 %% 
	 %% Basic SNMP API
	 g/3, g/4, g/5, g/6, ag/3, ag/4, ag/5, ag/6, 
	 gn/3, gn/4, gn/5, gn/6, agn/3, agn/4, agn/5, agn/6, 
	 gb/5, gb/6, gb/7, gb/8, 
	 s/3, s/4, s/5, s/6, as/3, as/4, as/5, as/6, 
	 cancel_async_request/2, 
	 
	 %% Extended SNMP API
	 discovery/2, discovery/3, discovery/4, discovery/5, 

	 %% Logging
	 log_to_txt/2, log_to_txt/3, log_to_txt/4,
	 log_to_txt/5, log_to_txt/6, log_to_txt/7, 
	 change_log_size/1,

	 reconfigure/0,

	 system_start_time/0,
	 sys_up_time/0,

	 verbosity/2 
	]).

%% Application internal export
-export([start_link/3]).


-include("snmpm_atl.hrl").

-define(DEFAULT_AGENT_PORT, 161).
-define(DEFAULT_CONTEXT,    "").


%% This function is called when the snmp application
%% starts. 
start_link(Opts, normal, []) ->
    start_link(Opts).


%% This function is normally not used. Instead the manager is
%% started as a consequence of a call to application:start(snmp)
%% when {snmp, [{manager, Options}]} is present in the
%% node config file.
start_link(Opts) ->
    %% This start the manager top supervisor, which in turn
    %% starts the other processes.
    {ok, _} = snmpm_supervisor:start_link(normal, Opts),
    ok.

start(Opts) ->
    %% This start the manager top supervisor, which in turn
    %% starts the other processes.
    {ok, Pid} = snmpm_supervisor:start_link(normal, Opts),
    unlink(Pid),
    ok.

stop() ->
    snmpm_supervisor:stop().


%% -- Mibs --

%% Load a mib into the manager
load_mib(MibFile) ->
    snmpm_server:load_mib(MibFile).

%% Unload a mib from the manager
unload_mib(Mib) ->
    snmpm_server:unload_mib(Mib).

%% Which mib's are loaded
which_mibs() ->
    snmpm_config:which_mibs().

%% Get all the possible oid's for the aliasname
name_to_oid(Name) ->
    snmpm_config:name_to_oid(Name).

%% Get the aliasname for an oid
oid_to_name(Oid) ->
    snmpm_config:oid_to_name(Oid).


%% -- Verbosity -- 

%% Change the verbosity of a process in the manager
verbosity(config, V) ->
    snmpm_config:verbosity(V);
verbosity(server, V) ->
    snmpm_server:verbosity(V);
verbosity(net_if, V) ->
    snmpm_server:verbosity(net_if, V);
verbosity(note_store, V) ->
    snmpm_server:verbosity(note_store, V);
verbosity(all, V) ->
    snmpm_config:verbosity(V),
    snmpm_server:verbosity(V),
    snmpm_server:verbosity(net_if, V),
    snmpm_server:verbosity(note_store, V).


%% -- Users --

%% Register the 'user'. 
%% The manager entity responsible for a specific agent. 
%% Module is the callback module (snmpm_user behaviour) which 
%% will be called whenever something happens (detected 
%% agent, incomming reply or incomming trap/notification).
%% Note that this could have already been done as a 
%% consequence of the node config.
register_user(Id, Module, Data) ->
    snmpm_server:register_user(Id, Module, Data).

register_user_monitor(Id, Module, Data) ->
    snmpm_server:register_user_monitor(Id, Module, Data).

unregister_user(Id) ->
    snmpm_server:unregister_user(Id).

which_users() ->
    snmpm_config:which_users().


%% -- Agents --

%% Explicitly instruct the manager to handle this agent.
%% Called to instruct the manager that this agent 
%% shall be handled. These functions is used when
%% the user know's in advance which agents the
%% manager shall handle.
%% Note that there is an alternate way to do the same thing:
%% Add the agent to the manager config files.
%% 
%% UserId -> Id of the user responsible for this agent
%% Addr   -> Address of the agent: term()
%% Port   -> Port number of the agent: integer()
%% Config -> Agent configuration: [config()]
register_agent(UserId, Addr) ->
    register_agent(UserId, Addr, ?DEFAULT_AGENT_PORT, []).

register_agent(UserId, Addr, Port) when integer(Port) ->
    register_agent(UserId, Addr, Port, []);
register_agent(UserId, Addr, Config) when list(Config) ->
    register_agent(UserId, Addr, ?DEFAULT_AGENT_PORT, Config).

register_agent(UserId, Addr, Port, Config) ->
    snmpm_config:register_agent(UserId, Addr, Port, Config).

unregister_agent(UserId, Addr) ->
    snmpm_config:unregister_agent(UserId, Addr, ?DEFAULT_AGENT_PORT).

unregister_agent(UserId, Addr, Port) ->
    snmpm_config:unregister_agent(UserId, Addr, Port).

agent_info(Addr, Port, Item) ->
    snmpm_config:agent_info(Addr, Port, Item).

update_agent_info(UserId, Addr, Port, Item, Val) ->
    snmpm_config:update_agent_info(UserId, Addr, Port, Item, Val).

which_agents() ->
    snmpm_config:which_agents().

which_agents(UserId) ->
    snmpm_config:which_agents(UserId).


%% -- USM users --

register_usm_user(EngineID, UserName, Conf) 
  when list(EngineID), list(UserName), list(Conf) ->
    snmpm_config:register_usm_user(EngineID, UserName, Conf).

usm_user_info(EngineID, UserName, Item) 
  when list(EngineID), list(UserName), atom(Item) ->
    snmpm_config:usm_user_info(EngineID, UserName, Item).

update_usm_user_info(EngineID, UserName, Item, Val) 
  when list(EngineID), list(UserName), atom(Item) ->
    snmpm_config:update_usm_user_info(EngineID, UserName, Item, Val).

which_usm_users() ->
    snmpm_config:which_usm_users().

which_usm_users(EngineID) when list(EngineID) ->
    snmpm_config:which_usm_users(EngineID).


%% -- Discovery --

%% Start a discovery process
discovery(UserId, BAddr) ->
    snmpm_server:discovery(UserId, BAddr).

discovery(UserId, BAddr, ExpireOrConfig) ->
    snmpm_server:discovery(UserId, BAddr, ExpireOrConfig).

discovery(UserId, BAddr, Config, Expire) ->
    snmpm_server:discovery(UserId, BAddr, Config, Expire).

discovery(UserId, BAddr, Port, Config, Expire) ->
    snmpm_server:discovery(UserId, BAddr, Port, Config, Expire).


%% -- Requests --

%% synchroneous get-request
%% 
g(UserId, Addr, Oids) ->
    snmpm_server:sync_get(UserId, Addr, ?DEFAULT_AGENT_PORT, 
			  ?DEFAULT_CONTEXT, Oids).

g(UserId, Addr, CtxName, Oids) when list(CtxName), list(Oids) ->
    snmpm_server:sync_get(UserId, Addr, ?DEFAULT_AGENT_PORT, CtxName, Oids);
g(UserId, Addr, Port, Oids) when integer(Port), list(Oids) ->
    snmpm_server:sync_get(UserId, Addr, Port, ?DEFAULT_CONTEXT, Oids);
g(UserId, Addr, Oids, Timeout) when list(Oids), integer(Timeout) ->
    g(UserId, Addr, ?DEFAULT_AGENT_PORT, ?DEFAULT_CONTEXT, Oids, Timeout).

g(UserId, Addr, Port, CtxName, Oids) 
  when integer(Port), list(CtxName), list(Oids) ->
    snmpm_server:sync_get(UserId, Addr, CtxName, Port, Oids);
g(UserId, Addr, Port, Oids, Timeout) 
  when integer(Port), list(Oids), integer(Timeout) ->
    g(UserId, Addr, Port, ?DEFAULT_CONTEXT, Oids, Timeout);
g(UserId, Addr, CtxName, Oids, Timeout) 
  when list(CtxName), list(Oids), integer(Timeout) ->
    g(UserId, Addr, ?DEFAULT_AGENT_PORT, CtxName, Oids, Timeout).

g(UserId, Addr, Port, CtxName, Oids, Timeout) ->
    snmpm_server:sync_get(UserId, Addr, Port, CtxName, Oids, Timeout).


%% asynchroneous get-request
%% 
%% The reply will be delivered to the user
%% through a call to handle_pdu/5
%% 
ag(UserId, Addr, Oids) ->
    ag(UserId, Addr, ?DEFAULT_AGENT_PORT, Oids).

ag(UserId, Addr, Port, Oids) when integer(Port), list(Oids) ->
    snmpm_server:async_get(UserId, Addr, Port, ?DEFAULT_CONTEXT, Oids);
ag(UserId, Addr, CtxName, Oids) when list(CtxName), list(Oids) ->
    snmpm_server:async_get(UserId, Addr, ?DEFAULT_AGENT_PORT, CtxName, Oids);
ag(UserId, Addr, Oids, Expire) when list(Oids), integer(Expire) ->
    ag(UserId, Addr, ?DEFAULT_AGENT_PORT, ?DEFAULT_CONTEXT, Oids, Expire).

ag(UserId, Addr, Port, CtxName, Oids) 
  when integer(Port), list(CtxName), list(Oids) ->
    snmpm_server:async_get(UserId, Addr, Port, CtxName, Oids);
ag(UserId, Addr, Port, Oids, Expire) 
  when integer(Port), list(Oids), integer(Expire) ->
    ag(UserId, Addr, Port, ?DEFAULT_CONTEXT, Oids, Expire);
ag(UserId, Addr, CtxName, Oids, Expire) 
  when list(CtxName), list(Oids), integer(Expire) ->
    ag(UserId, Addr, ?DEFAULT_AGENT_PORT, CtxName, Oids, Expire).

ag(UserId, Addr, Port, CtxName, Oids, Expire) ->
    snmpm_server:async_get(UserId, Addr, Port, CtxName, Oids, Expire).


%% synchroneous get_next-request
%% 
gn(UserId, Addr, Oids) ->
    snmpm_server:sync_get_next(UserId, Addr, ?DEFAULT_AGENT_PORT, 
			       ?DEFAULT_CONTEXT, Oids).

gn(UserId, Addr, Port, Oids) when integer(Port), list(Oids) ->
    snmpm_server:sync_get_next(UserId, Addr, Port, ?DEFAULT_CONTEXT, Oids);
gn(UserId, Addr, CtxName, Oids) when list(CtxName), list(Oids) ->
    snmpm_server:sync_get_next(UserId, Addr, ?DEFAULT_AGENT_PORT, 
			       CtxName, Oids);
gn(UserId, Addr, Oids, Timeout) when list(Oids), integer(Timeout) ->
    gn(UserId, Addr, ?DEFAULT_AGENT_PORT, ?DEFAULT_CONTEXT, Oids, Timeout).

gn(UserId, Addr, Port, CtxName, Oids) 
  when integer(Port), list(CtxName), list(Oids) ->
    snmpm_server:sync_get_next(UserId, Addr, Port, CtxName, Oids);
gn(UserId, Addr, Port, Oids, Timeout) 
  when integer(Port), list(Oids), integer(Timeout) ->
    gn(UserId, Addr, Port, ?DEFAULT_CONTEXT, Oids, Timeout);
gn(UserId, Addr, CtxName, Oids, Timeout) 
  when list(CtxName), list(Oids), integer(Timeout) ->
    gn(UserId, Addr, ?DEFAULT_AGENT_PORT, CtxName, Oids, Timeout).

gn(UserId, Addr, Port, CtxName, Oids, Timeout) ->
    snmpm_server:sync_get_next(UserId, Addr, Port, CtxName, Oids, Timeout).


%% asynchroneous get_next-request
%% 
agn(UserId, Addr, Oids) ->
    snmpm_server:async_get_next(UserId, Addr, ?DEFAULT_AGENT_PORT, 
				?DEFAULT_CONTEXT, Oids).

agn(UserId, Addr, Port, Oids) when integer(Port), list(Oids) ->
    snmpm_server:async_get_next(UserId, Addr, Port, 
				?DEFAULT_CONTEXT, Oids);
agn(UserId, Addr, CtxName, Oids) when list(CtxName), list(Oids) ->
    snmpm_server:async_get_next(UserId, Addr, ?DEFAULT_AGENT_PORT, 
				CtxName, Oids);
agn(UserId, Addr, Oids, Expire) when list(Oids), integer(Expire) ->
    agn(UserId, Addr, ?DEFAULT_AGENT_PORT, ?DEFAULT_CONTEXT, Oids, Expire).

agn(UserId, Addr, Port, CtxName, Oids) 
  when integer(Port), list(CtxName), list(Oids) ->
    snmpm_server:async_get_next(UserId, Addr, Port, CtxName, Oids);
agn(UserId, Addr, Port, Oids, Expire) 
  when integer(Port), list(Oids), integer(Expire) ->
    agn(UserId, Addr, Port, ?DEFAULT_CONTEXT, Oids, Expire);
agn(UserId, Addr, CtxName, Oids, Expire) 
  when list(Expire), list(CtxName), integer(Expire) ->
    agn(UserId, Addr, ?DEFAULT_AGENT_PORT, CtxName, Oids, Expire).

agn(UserId, Addr, Port, CtxName, Oids, Expire) ->
    snmpm_server:async_get_next(UserId, Addr, Port, CtxName, Oids, Expire).


%% synchroneous set-request
%% 
s(UserId, Addr, VarsAndVals) ->
    snmpm_server:sync_set(UserId, Addr, ?DEFAULT_AGENT_PORT, 
			  ?DEFAULT_CONTEXT, VarsAndVals).

s(UserId, Addr, Port, VarsAndVals) when integer(Port), list(VarsAndVals) ->
    snmpm_server:sync_set(UserId, Addr, Port, 
			  ?DEFAULT_CONTEXT, VarsAndVals);
s(UserId, Addr, CtxName, VarsAndVals) when list(CtxName), list(VarsAndVals) ->
    snmpm_server:sync_set(UserId, Addr, ?DEFAULT_AGENT_PORT, 
			  CtxName, VarsAndVals);
s(UserId, Addr, VarsAndVals, Timeout) 
  when list(VarsAndVals), integer(Timeout) ->
    s(UserId, Addr, ?DEFAULT_AGENT_PORT, 
      ?DEFAULT_CONTEXT, VarsAndVals, Timeout).

s(UserId, Addr, Port, CtxName, VarsAndVals) 
  when integer(Port), list(CtxName), list(VarsAndVals) ->
    snmpm_server:sync_set(UserId, Addr, Port, CtxName, VarsAndVals);
s(UserId, Addr, Port, VarsAndVals, Timeout) 
  when integer(Port), list(VarsAndVals), integer(Timeout) ->
    s(UserId, Addr, Port, ?DEFAULT_CONTEXT, VarsAndVals, Timeout);
s(UserId, Addr, CtxName, VarsAndVals, Timeout) 
  when list(CtxName), list(VarsAndVals), integer(Timeout) ->
    s(UserId, Addr, ?DEFAULT_AGENT_PORT, CtxName, VarsAndVals, Timeout).

s(UserId, Addr, Port, CtxName, VarsAndVals, Timeout) ->
    snmpm_server:sync_set(UserId, Addr, Port, CtxName, VarsAndVals, Timeout).


%% asynchroneous set-request
%% 
as(UserId, Addr, VarsAndVals) ->
    snmpm_server:async_set(UserId, Addr, ?DEFAULT_AGENT_PORT, 
			   ?DEFAULT_CONTEXT, VarsAndVals).

as(UserId, Addr, Port, VarsAndVals) when integer(Port), list(VarsAndVals) ->
    snmpm_server:async_set(UserId, Addr, Port, 
			   ?DEFAULT_CONTEXT, VarsAndVals);
as(UserId, Addr, CtxName, VarsAndVals) when list(CtxName), list(VarsAndVals) ->
    snmpm_server:async_set(UserId, Addr, ?DEFAULT_AGENT_PORT, 
			   CtxName, VarsAndVals);
as(UserId, Addr, VarsAndVals, Expire) 
  when list(VarsAndVals), integer(Expire) ->
    as(UserId, Addr, ?DEFAULT_AGENT_PORT, 
       ?DEFAULT_CONTEXT, VarsAndVals, Expire).

as(UserId, Addr, Port, CtxName, VarsAndVals) 
  when integer(Port), list(CtxName), list(VarsAndVals) ->
    snmpm_server:async_set(UserId, Addr, Port, CtxName, VarsAndVals);
as(UserId, Addr, Port, VarsAndVals, Expire) 
  when integer(Port), list(VarsAndVals), integer(Expire) ->
    as(UserId, Addr, Port, ?DEFAULT_CONTEXT, VarsAndVals, Expire);
as(UserId, Addr, CtxName, VarsAndVals, Expire) 
  when list(CtxName), list(VarsAndVals), integer(Expire) ->
    as(UserId, Addr, ?DEFAULT_AGENT_PORT, CtxName, VarsAndVals, Expire).

as(UserId, Addr, Port, CtxName, VarsAndVals, Expire) ->
    snmpm_server:async_set(UserId, Addr, Port, CtxName, VarsAndVals, Expire).


%% synchroneous get-bulk
%% 
gb(UserId, Addr, NonRep, MaxRep, Oids) ->
    snmpm_server:sync_get_bulk(UserId, Addr, ?DEFAULT_AGENT_PORT, 
			       NonRep, MaxRep, 
			       ?DEFAULT_CONTEXT, Oids).

gb(UserId, Addr, Port, NonRep, MaxRep, Oids) 
  when integer(Port), integer(NonRep), integer(MaxRep), list(Oids) ->
    snmpm_server:sync_get_bulk(UserId, Addr, Port, 
			       NonRep, MaxRep, ?DEFAULT_CONTEXT, Oids);
gb(UserId, Addr, NonRep, MaxRep, CtxName, Oids) 
  when integer(NonRep), integer(MaxRep), list(CtxName), list(Oids) ->
    snmpm_server:sync_get_bulk(UserId, Addr, ?DEFAULT_AGENT_PORT, 
			       NonRep, MaxRep, CtxName, Oids);
gb(UserId, Addr, NonRep, MaxRep, Oids, Timeout) 
  when integer(NonRep), integer(MaxRep), list(Oids), integer(Timeout) ->
    gb(UserId, Addr, ?DEFAULT_AGENT_PORT, 
       NonRep, MaxRep, ?DEFAULT_CONTEXT, Oids, Timeout).

gb(UserId, Addr, Port, NonRep, MaxRep, CtxName, Oids) 
  when integer(Port), integer(NonRep), integer(MaxRep), 
       list(CtxName), list(Oids) ->
    snmpm_server:sync_get_bulk(UserId, Addr, Port, 
			       NonRep, MaxRep, CtxName, Oids);
gb(UserId, Addr, Port, NonRep, MaxRep, Oids, Timeout) 
  when integer(Port), integer(NonRep), integer(MaxRep), 
       list(Oids), integer(Timeout) ->
    gb(UserId, Addr, Port, 
       NonRep, MaxRep, ?DEFAULT_CONTEXT, Oids, Timeout);
gb(UserId, Addr, NonRep, MaxRep, CtxName, Oids, Timeout) 
  when integer(NonRep), integer(MaxRep), 
       list(CtxName), list(Oids), integer(Timeout) ->
    gb(UserId, Addr, ?DEFAULT_AGENT_PORT, 
       NonRep, MaxRep, CtxName, Oids).

gb(UserId, Addr, Port, NonRep, MaxRep, CtxName, Oids, Timeout) ->
    snmpm_server:sync_get_bulk(UserId, Addr, Port, 
			       NonRep, MaxRep, CtxName, Oids, Timeout).


cancel_async_request(UserId, ReqId) ->
    snmpm_server:cancel_async_request(UserId, ReqId).


%%%-----------------------------------------------------------------
%%% Audit Trail Log functions (for backward compatibillity)
%%%-----------------------------------------------------------------
log_to_txt(LogDir, Mibs) ->
    OutFile = "snmpm_log.txt",       
    LogName = ?audit_trail_log_name, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile).
log_to_txt(LogDir, Mibs, OutFile) ->
    LogName = ?audit_trail_log_name, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile).
log_to_txt(LogDir, Mibs, OutFile, LogName) ->
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile).
log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile) -> 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile).
log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Start) -> 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Start).
log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Start, Stop) -> 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Start, Stop).


change_log_size(NewSize) ->
    LogName = ?audit_trail_log_name, 
    snmp:change_log_size(LogName, NewSize).

reconfigure() ->
    snmpm_server:reconfigure().


%%%-----------------------------------------------------------------

system_start_time() ->
    {ok, Time} = snmpm_config:system_start_time(),
    Time.

sys_up_time() ->
    % time in 0.01 seconds.
    StartTime = system_start_time(),
    (snmp_misc:now(cs) - StartTime) rem (2 bsl 31).


