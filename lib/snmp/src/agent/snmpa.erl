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
-module(snmpa).


%%----------------------------------------------------------------------
%% This module contains the user interface to the snmp agent toolkit.
%%----------------------------------------------------------------------

-export([verbosity/2, 
	 
	 current_request_id/0, current_community/0, current_address/0,
	 current_context/0, current_net_if_data/0, 
	 
	 get_symbolic_store_db/0,
	 which_aliasnames/0, 
	 which_tables/0, 
	 which_variables/0, 
	 name_to_oid/1, name_to_oid/2, 
	 oid_to_name/1, oid_to_name/2,
	 int_to_enum/2, int_to_enum/3, 
	 enum_to_int/2, enum_to_int/3,

	 info/0, info/1, old_info_format/1, 
	 load_mibs/1, load_mibs/2, 
	 unload_mibs/1, unload_mibs/2, 
	 which_mibs/0, which_mibs/1, 
	 whereis_mib/1, whereis_mib/2, 
	 dump_mibs/0, dump_mibs/1,
	 mib_of/1, mib_of/2, 
	 me_of/1,  me_of/2, 

	 get/2, get/3, get_next/2, get_next/3,

	 register_subagent/3, unregister_subagent/2, 

	 send_notification/3, send_notification/4, send_notification/5,
	 send_notification/6,
	 send_trap/3, send_trap/4,

         register_notification_filter/3,
         register_notification_filter/4,
         register_notification_filter/5,
         unregister_notification_filter/1,
         unregister_notification_filter/2,
         which_notification_filter/0,
         which_notification_filter/1,
 
 	 sys_up_time/0, system_start_time/0,

	 backup/1, backup/2, 

	 convert_config/1]).

%% USM functions:
-export([passwd2localized_key/3, localize_key/3]).

%% Agent Capabilities functions
-export([add_agent_caps/2, del_agent_caps/1, get_agent_caps/0]).

%% Audit Trail Log functions
-export([log_to_txt/2, log_to_txt/3, log_to_txt/4, 
	 log_to_txt/5, log_to_txt/6, log_to_txt/7, 
	 change_log_size/1]).


-include("snmpa_atl.hrl").


%%-----------------------------------------------------------------
%% This utility function is used to convert an old SNMP application
%% config (prior to snmp-4.0) to a SNMP agent config (as of 
%% snmp-4.0).
%% This is the config structure of the SNMP application as of 
%% snmp-4.0:
%% {snmp, snmp_config()}
%% snmp_config() -> [snmp_config_item()]
%% snmp_config_item() -> {agent,   agent_config()} | 
%%                       {manager, manager_config()}
%%-----------------------------------------------------------------

convert_config(Opts) ->
    snmpa_app:convert_config(Opts).


%%-----------------------------------------------------------------
%% Note that verbosity for the agents is actually only implemented 
%% (properly) for the master agent.
%%-----------------------------------------------------------------

verbosity(all,Verbosity) -> 
    catch snmpa_agent:verbosity(sub_agents,Verbosity),
    catch snmpa_agent:verbosity(master_agent,Verbosity),
    catch snmpa_agent:verbosity(net_if,Verbosity),
    catch snmpa_agent:verbosity(mib_server,Verbosity),
    catch snmpa_agent:verbosity(note_store,Verbosity),
    catch snmpa_symbolic_store:verbosity(Verbosity),
    catch snmpa_local_db:verbosity(Verbosity);
verbosity(master_agent,Verbosity) -> 
    catch snmpa_agent:verbosity(master_agent,Verbosity);
verbosity(net_if,Verbosity) -> 
    catch snmpa_agent:verbosity(net_if,Verbosity);
verbosity(note_store,Verbosity) -> 
    catch snmpa_agent:verbosity(note_store, Verbosity);
verbosity(mib_server,Verbosity) -> 
    catch snmpa_agent:verbosity(mib_server,Verbosity);
verbosity(symbolic_store,Verbosity) -> 
    catch snmpa_symbolic_store:verbosity(Verbosity);
verbosity(local_db,Verbosity) -> 
    catch snmpa_local_db:verbosity(Verbosity);
verbosity(Agent,{subagents,Verbosity}) -> 
    catch snmpa_agent:verbosity(Agent,{sub_agents,Verbosity});
verbosity(Agent,Verbosity) -> 
    catch snmpa_agent:verbosity(Agent,Verbosity).


%%-----------------------------------------------------------------
%% 
%% Some symbolic store (internal database) utility functions
%% 
%%-----------------------------------------------------------------

get_symbolic_store_db() ->
    snmpa_symbolic_store:get_db().


which_aliasnames() ->
    snmpa_symbolic_store:which_aliasnames().

which_tables() ->
    snmpa_symbolic_store:which_tables().

which_variables() ->
    snmpa_symbolic_store:which_variables().


%%-----------------------------------------------------------------
%% These 8 functions returns {value, Val} | false
%%-----------------------------------------------------------------
name_to_oid(Name) ->
    snmpa_symbolic_store:aliasname_to_oid(Name).

name_to_oid(Db, Name) ->
    snmpa_symbolic_store:aliasname_to_oid(Db, Name).

oid_to_name(OID) ->
    snmpa_symbolic_store:oid_to_aliasname(OID).

oid_to_name(Db, OID) ->
    snmpa_symbolic_store:oid_to_aliasname(Db, OID).

enum_to_int(Name, Enum) ->
    snmpa_symbolic_store:enum_to_int(Name, Enum).

enum_to_int(Db, Name, Enum) ->
    snmpa_symbolic_store:enum_to_int(Db, Name, Enum).

int_to_enum(Name, Int) ->
    snmpa_symbolic_store:int_to_enum(Name, Int).

int_to_enum(Db, Name, Int) ->
    snmpa_symbolic_store:int_to_enum(Db, Name, Int).


%%-----------------------------------------------------------------
%% These functions must only be called in the process context
%% where the instrumentation functions are called!
%%-----------------------------------------------------------------
current_request_id()  -> current_get(snmp_request_id).
current_context()     -> current_get(snmp_context).
current_community()   -> current_get(snmp_community).
current_address()     -> current_get(snmp_address).
current_net_if_data() -> current_get(net_if_data).

current_get(Tag) ->
    case get(Tag) of
	undefined -> false;
	X -> {value, X}
    end.
    

%% -

get(Agent, Vars) -> snmpa_agent:get(Agent, Vars).
get(Agent, Vars, Context) -> snmpa_agent:get(Agent, Vars, Context).

get_next(Agent, Vars) -> snmpa_agent:get_next(Agent, Vars).
get_next(Agent, Vars, Context) -> snmpa_agent:get_next(Agent, Vars, Context).


info()      -> info(snmp_master_agent).
info(Agent) -> snmpa_agent:info(Agent).

old_info_format(Info) when list(Info) ->
    {value, Vsns}         = lists:keysearch(vsns,            1, Info),
    {value, {_, MibInfo}} = lists:keysearch(mib_server,      1, Info),
    {value, SAa}          = lists:keysearch(subagents,       1, MibInfo),
    {value, LoadedMibs}   = lists:keysearch(loaded_mibs,     1, MibInfo),
    {value, TreeSz}       = lists:keysearch(tree_size_bytes, 1, MibInfo),
    {value, ProcMem}      = lists:keysearch(process_memory,  1, MibInfo),
    {value, DbMem}        = lists:keysearch(db_memory,       1, MibInfo),
    [Vsns, SAa, LoadedMibs, TreeSz, ProcMem, DbMem].
    

%% -

backup(BackupDir) ->
    backup(snmp_master_agent, BackupDir).

backup(Agent, BackupDir) ->
    snmpa_agent:backup(Agent, BackupDir).


%% -

dump_mibs()     -> snmpa_agent:dump_mibs(snmp_master_agent).
dump_mibs(File) -> snmpa_agent:dump_mibs(snmp_master_agent, File).

load_mibs(Mibs) ->
    load_mibs(snmp_master_agent, Mibs).
load_mibs(Agent, Mibs) when list(Mibs) -> 
    snmpa_agent:load_mibs(Agent, Mibs).

unload_mibs(Mibs) ->
    unload_mibs(snmp_master_agent, Mibs).
unload_mibs(Agent, Mibs) when list(Mibs) -> 
    snmpa_agent:unload_mibs(Agent, Mibs).

which_mibs()      -> which_mibs(snmp_master_agent).
which_mibs(Agent) -> snmpa_agent:which_mibs(Agent).

whereis_mib(Mib) ->
    whereis_mib(snmp_master_agent, Mib).
whereis_mib(Agent, Mib) when atom(Mib) ->
    snmpa_agent:whereis_mib(Agent, Mib).


%% -

mib_of(Oid) ->
    snmpa_agent:mib_of(Oid).

mib_of(Agent, Oid) ->
    snmpa_agent:mib_of(Agent, Oid).

me_of(Oid) ->
    snmpa_agent:me_of(Oid).

me_of(Agent, Oid) ->
    snmpa_agent:me_of(Agent, Oid).


%% -

register_notification_filter(Id, Mod, Data) when atom(Mod) ->
    register_notification_filter(snmp_master_agent, Id, Mod, Data, last).
 
register_notification_filter(Agent, Id, Mod, Data)
  when atom(Agent), atom(Mod) ->
    register_notification_filter(Agent, Id, Mod, Data, last);
register_notification_filter(Agent, Id, Mod, Data)
  when pid(Agent), atom(Mod) ->
    register_notification_filter(Agent, Id, Mod, Data, last);
register_notification_filter(Id, Mod, Data, Where) when atom(Mod) ->
    register_notification_filter(snmp_master_agent, Id, Mod, Data, Where).
 
register_notification_filter(Agent, Id, Mod, Data, Where) ->
    snmpa_agent:register_notification_filter(Agent, Id, Mod, Data, Where).
 
unregister_notification_filter(Id) ->
    unregister_notification_filter(snmp_master_agent, Id).
 
unregister_notification_filter(Agent, Id) ->
    snmpa_agent:unregister_notification_filter(Agent, Id).
 
which_notification_filter() ->
    which_notification_filter(snmp_master_agent).
 
which_notification_filter(Agent) ->
    snmpa_agent:which_notification_filter(Agent).
 
 
%% -

send_notification(Agent, Notification, Recv) ->
    send_notification(Agent, Notification, Recv, "", "", []).

send_notification(Agent, Notification, Recv, Varbinds) ->
    send_notification(Agent, Notification, Recv, "", "", Varbinds).

send_notification(Agent, Notification, Recv, NotifyName, Varbinds) ->
    send_notification(Agent, Notification, Recv, NotifyName, "", Varbinds).

send_notification(Agent, Notification, Recv,NotifyName,ContextName,Varbinds) 
  when list(NotifyName), list(ContextName), list(Varbinds) ->
    snmpa_agent:send_trap(Agent, Notification, NotifyName, 
			  ContextName, Recv, Varbinds).

%% Kept for backwards compatibility
send_trap(Agent, Trap, Community) ->
    send_notification(Agent, Trap, no_receiver, Community, "", []).

send_trap(Agent, Trap, Community, Varbinds) ->
    send_notification(Agent, Trap, no_receiver, Community, "", Varbinds).

register_subagent(Agent, SubTree, SubAgent) ->
    snmpa_agent:register_subagent(Agent, SubTree, SubAgent).

unregister_subagent(Agent, SubOidOrPid) ->
    snmpa_agent:unregister_subagent(Agent, SubOidOrPid).

system_start_time() ->
    [{_, Time}] = ets:lookup(snmp_agent_table, system_start_time),
    Time.

sys_up_time() ->
    % time in 0.01 seconds.
    StartTime = system_start_time(),
    (snmp_misc:now(cs) - StartTime) rem (2 bsl 31).


%%%-----------------------------------------------------------------
%%% USM functions
%%%-----------------------------------------------------------------
passwd2localized_key(Alg, Passwd, EngineID) ->
    snmp_usm:passwd2localized_key(Alg, Passwd, EngineID).

localize_key(Alg, Key, EngineID) ->
    snmp_usm:localize_key(Alg, Key, EngineID).


%%%-----------------------------------------------------------------
%%% Agent Capabilities functions
%%%-----------------------------------------------------------------
add_agent_caps(Oid, Descr) ->
    snmp_standard_mib:add_agent_caps(Oid, Descr).

del_agent_caps(Index) ->
    snmp_standard_mib:del_agent_caps(Index).

get_agent_caps() ->
    snmp_standard_mib:get_agent_caps().


%%%-----------------------------------------------------------------
%%% Audit Trail Log functions 
%%%-----------------------------------------------------------------
log_to_txt(LogDir, Mibs) -> 
    OutFile = "snmpa_log.txt",       
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
    LogName = ?audit_trail_log_name, % The old (agent) default
    snmp:change_log_size(LogName, NewSize).


