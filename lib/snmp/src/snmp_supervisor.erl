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
-module(snmp_supervisor).

-behaviour(supervisor).

%% External exports
-export([start_sub/1, start_sub/2, start_master/2, start_master/3]).
-export([start_subagent/3, stop_subagent/1]).

%% Internal exports
-export([init/1, snmp_config/3]).

-include("snmp_verbosity.hrl").
-include("snmp_debug.hrl").


-ifndef(default_verbosity).
-define(default_verbosity,silence).
-endif.


%%-----------------------------------------------------------------
%% Process structure
%% =================
%%
%%          ____________ supervisor _________________
%%         /             |        \         \        \
%%   misc_sup  symbolic_store  local_db   agent_sup  note_store
%%     |   |                                |   | 
%%    mib net_if                           MA - SA
%%
%%  The supervisor (one at each node) starts:
%%    snmp_symbolic_store (one at each node)
%%    snmp_local_db (one at each node)
%%    MA    which starts
%%             own mib, hangs it under misc_sup
%%             net_if, hangs it under misc_sup
%%    SAs   which starts
%%             own mib, hangs it under misc_sup
%%    snmp_note_store (only one at the master agent's node)
%%
%% This structure is intended to make the agent fault tolerant. The
%% agent processes (by far most code is in these processes) can be
%% restarted in case of a failure, as all data needed by these procs
%% are stored in the other procs. Any other process crash leads to
%% that the entire snmp_supervisor crashes. If it is restarted, all
%% dynamically loaded mibs must be reloaded.
%%
%% It is up to the supervisor to configure the internal and
%% external mibs. Note that the 
%% agent group (internal MIB) and sysObjectID *must* be configured
%% in some way.
%%-----------------------------------------------------------------
start_sub(DbDir) -> start_sub(DbDir, []).
start_sub(DbDir, Opts) ->
    ?debug("start_sub -> ~n"
	   "\tDbDir: ~p~n"
	   "\tOpts:  ~p",[DbDir,Opts]),
    supervisor:start_link({local, snmp_supervisor}, snmp_supervisor,
			  [sub_agent, DbDir, undefined, Opts]).
start_master(DbDir, ConfDir) ->
    start_master(DbDir, ConfDir, [{name, {local, snmp_master_agent}}]).

start_master(DbDir, ConfDir, Opts) ->
    ?debug("start_master -> ~n"
	   "\tDbDir:   ~p~n"
	   "\tConfDir: ~p~n"
	   "\tOpts:    ~p",[DbDir,ConfDir,Opts]),
    supervisor:start_link({local, snmp_supervisor}, snmp_supervisor,
			  [master_agent, DbDir, ConfDir, Opts]).

start_subagent(ParentAgent, Subtree, Mibs) when pid(ParentAgent),list(Mibs) ->
    ?debug("start_subagent -> ~n"
	   "\tParentAgent: ~p~n"
	   "\tSubtree:     ~p~n"
	   "\tMibs:        ~p",[ParentAgent,Subtree,Mibs]),
    snmp_agent_sup:start_subagent(ParentAgent, Subtree, Mibs).

stop_subagent(SubAgentPid) ->
    ?debug("stop_subagent -> ~n"
	   "\tSubAgentPid: ~p",[SubAgentPid]),
    snmp_agent_sup:stop_subagent(SubAgentPid).


init([Type, DbD, ConfD, Opts]) ->
    ?debug("init -> entry with"
	   "~n    Type:  ~p"
	   "~n    DbD:   ~p"
	   "~n    ConfD: ~p"
	   "~n    Opts:  ~p",[Type, DbD, ConfD, Opts]),
    put(sname,sup),
    put(verbosity,get_verbosity(Opts)),
    ?vlog("starting",[]),
    DbDir = filename:join([DbD]),
    SupFlags = {one_for_all, 0, 3600},
    Prio = snmp_misc:get_option(priority, Opts, normal),
    Vsns = snmp_misc:get_option(snmp_vsn, Opts, [v1,v2,v3]),
    MiscSup = {snmp_misc_sup,
	      {snmp_misc_sup, start_link, [snmp_misc_sup]},
	      permanent, infinity, supervisor, [snmp_misc_sup]},
    MibStorage = {mib_storage,snmp_misc:get_option(mib_storage,Opts,ets)},
    SymStoreVerbosity = 
	{verbosity,
	 snmp_misc:get_option(symbolic_store_verbosity,Opts,silence)},
    SymStoreArgs = [Prio,[SymStoreVerbosity,MibStorage]],
    SymbolicStore = {snmp_symbolic_store,
		     {snmp_symbolic_store, start_link, SymStoreArgs},
		     permanent, 2000, worker, [snmp_symbolic_store]},
    LdbAutoRepair = snmp_misc:get_option(local_db_auto_repair, Opts, true),
    LdbVerbosity = snmp_misc:get_option(local_db_verbosity, Opts, silence),
    LdbOpts = [{auto_repair,LdbAutoRepair},{verbosity,LdbVerbosity}],
    LocalDb = {snmp_local_db,
	       {snmp_local_db, start_link, [DbDir, Prio, LdbOpts]},
	       permanent, 5000, worker, [snmp_local_db]},    
    ErrorReportMod = snmp_misc:get_option(error_report_mod, Opts, snmp_error),
    ?vdebug("create agent table",[]),
    ets:new(snmp_agent_table, [set, public, named_table]),
    ?vdebug("create community cache",[]),
    ets:new(snmp_community_cache, [bag, public, named_table]),
    ?vdebug("store priority",[]),
    ets:insert(snmp_agent_table, {priority, Prio}),
    ?vdebug("store system start time",[]),
    ets:insert(snmp_agent_table, {system_start_time, snmp_misc:now(cs)}),
    ?vdebug("store error report module: ~w",[ErrorReportMod]),
    ets:insert(snmp_agent_table, {error_report_mod, ErrorReportMod}),
    ?vdebug("init VACM",[]),
    snmp_vacm:init(DbDir),
    Rest =
	case Type of
	    master_agent ->
		% If we're starting the master, we must also
		% configure the tables.
		?vdebug("starting as master agent -> configure the tables",[]),
		ConfDir = filename:join([ConfD]),
		ForceLoad = snmp_misc:get_option(force_load, Opts, false),
		Config = {snmp_config,
			  {snmp_supervisor, snmp_config,
			   [ConfDir, Vsns, ForceLoad]},
			  transient, 2000, worker, []},
		NoteStoreVerbosity = 
		    {verbosity,
		     snmp_misc:get_option(note_store_verbosity,Opts,silence)},
		NoteStoreArgs = [Prio,[NoteStoreVerbosity]],
		NoteStore =  {snmp_note_store,
			      {snmp_note_store, start_link, NoteStoreArgs},
			      permanent, 2000, worker, [snmp_note_store]},
		Mibs = snmp_misc:get_option(mibs, Opts, []),
		MibDir = filename:join(code:priv_dir(snmp), "mibs"),
		StdMib = 
		    case (lists:member(v2, Vsns) or lists:member(v3, Vsns)) of
			true -> filename:join([MibDir, "SNMPv2-MIB"]);
			false -> filename:join([MibDir, "STANDARD-MIB"])
		    end,
		?vdebug("add standard and v2 mibs",[]),
		NMibs = add_mib(StdMib, Mibs, ["SNMPv2-MIB", "STANDARD-MIB"]),
		NMibs2 = case lists:member(v3, Vsns) of
			     true -> 
				 ?vdebug("add v3 mibs",[]),
				 add_v3_mibs(MibDir, NMibs);
			     false -> NMibs
			 end,
		?vdebug("set mibs options",[]),
		Opts2 = snmp_misc:set_option(mibs, NMibs2, Opts),
		?vdebug("set misc_sup options",[]),
		Opts3 = snmp_misc:set_option(misc_sup, snmp_misc_sup, Opts2),
		Ref = make_ref(),
		Agent =
		    case snmp_misc:assq(name, Opts) of
			{value, Name} ->
			    {master_agent,
			     {snmp_agent, start_link, [Name,none,Ref,Opts3]},
			     permanent, 15000, worker, [snmp_agent]};
			false ->
			    {master_agent,
			     {snmp_agent, start_link, [none,Ref,Opts3]},
			     permanent, 15000, worker, [snmp_agent]}
		    end,
		AgentSup = {snmp_agent_sup,
			    {snmp_agent_sup, start_link, [Prio,Agent]},
			    permanent, infinity, supervisor, [snmp_agent_sup]},
		[Config, NoteStore, AgentSup];
	    _ ->
		AgentSup = {snmp_agent_sup,
			    {snmp_agent_sup, start_link, [Prio]},
			    permanent, infinity, supervisor, [snmp_agent_sup]},
		[AgentSup]
	end,
    ?vdebug("init done with:~n~p",[[MiscSup, SymbolicStore, LocalDb | Rest]]),
    {ok, {SupFlags, [MiscSup, SymbolicStore, LocalDb | Rest]}}.


add_v3_mibs(MibDir, Mibs) ->
    NMibs = add_mib(filename:join(MibDir, "SNMP-FRAMEWORK-MIB"),
		    Mibs, ["SNMP-FRAMEWORK-MIB"]),
    add_mib(filename:join(MibDir, "SNMP-MPD-MIB"),
	    NMibs, ["SNMP-MPD-MIB"]).

add_mib(DefaultMib, [], _BaseNames) -> [DefaultMib];
add_mib(DefaultMib, [Mib | T], BaseNames) ->
    case lists:member(filename:basename(Mib), BaseNames) of
	true -> [Mib | T]; % The user defined his own version of the mib
	false -> [Mib | add_mib(DefaultMib, T, BaseNames)]
    end.

snmp_config(ConfDir, Vsns, ForceLoad) ->
    case catch conf(ConfDir, Vsns, ForceLoad) of
	ok -> 
	    ignore;
	{'EXIT', Reason} -> 
	    ?vlog("configure failed (exit) with reason: ~p",[Reason]),
	    {error, {config_error, Reason}}
    end.

conf(Dir, Vsns, ForceLoad) ->
    ?vlog("start configure (~p)",[ForceLoad]),
    Conf = case ForceLoad of
	       false -> configure;
	       true -> reconfigure
	   end,
    ?vdebug("~p snmp_standard_mib",[Conf]),
    snmp_standard_mib:Conf(Dir),
    ?vdebug("init snmp_framework_mib",[]),
    snmp_framework_mib:init(),
    ?vdebug("configure snmp_framework_mib",[]),
    snmp_framework_mib:configure(Dir),
    ?vdebug("~p snmp_target_mib",[Conf]),
    snmp_target_mib:Conf(Dir),
    ?vdebug("~p snmp_notification_mib",[Conf]),
    snmp_notification_mib:Conf(Dir),
    ?vdebug("~p snmp_view_based_acm_mib",[Conf]),
    snmp_view_based_acm_mib:Conf(Dir),
    case lists:member(v1, Vsns) or lists:member(v2, Vsns) of
	true ->
	    ?vdebug("we need to handle v1 and/or v2 =>~n"
		    "   ~p snmp_community_mib",[Conf]),
	    snmp_community_mib:Conf(Dir);
	false ->
	    ?vdebug("no need to handle v1 or v2",[]),
	    ok
    end,
    case lists:member(v3, Vsns) of
	true ->
	    ?vdebug("we need to handle v3 =>~n"
		    "   ~p snmp_user_based_sm_mib",[Conf]),
	    snmp_user_based_sm_mib:Conf(Dir);
	false ->
	    ?vdebug("no need to handle v3",[]),
	    ok
    end,
    ?vdebug("conf done",[]),
    ok.


%% -------------------------------------

get_verbosity(L) -> 
    snmp_misc:get_option(supervisor_verbosity,L,?default_verbosity).

