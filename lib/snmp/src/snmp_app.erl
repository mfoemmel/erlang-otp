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

%%%-----------------------------------------------------------------
%%%  This module implements the SNMP application.
%%%-----------------------------------------------------------------
-export([start/2, stop/1, config_change/3]).

start(Type, []) ->
    LdbAutoRepair =
	case application:get_env(snmp, snmp_local_db_auto_repair) of
	    {ok, AutoRepair} -> AutoRepair;
	    _ -> true
	end,
    LdbVerbosity =
	case application:get_env(snmp, snmp_local_db_verbosity) of
	    {ok, V1} -> V1;
	    _ -> silence
	end,
    SymbolicStoreVerbosity =
	case application:get_env(snmp, snmp_symbolic_store_verbosity) of
	    {ok, V2} -> V2;
	    _ -> silence
	end,
    DbDir =
	case application:get_env(snmp, snmp_db_dir) of
	    {ok, Dir2} when list(Dir2) -> Dir2;
	    {ok, Bad} -> exit({bad_config, {snmp, {snmp_db_dir, Bad}}});
	    _ -> exit({undefined_config, {snmp, snmp_db_dir}})
    end,
    Prio =
	case application:get_env(snmp, snmp_priority) of
	    {ok, Pr} when atom(Pr) -> Pr;
	    _ -> normal
    end,
    MibsVerbosity =
	case application:get_env(snmp, snmp_mibserver_verbosity) of
	    {ok, V3} -> V3;
	    _ -> silence
	end,
    MibStorage =
	case application:get_env(snmp, snmp_mib_storage) of
	    {ok, Storage} -> Storage;
	    _ -> ets
	end,
    MeOverride =
	case application:get_env(snmp, snmp_mibentry_override) of
	    {ok, true} -> true;
	    _ -> false
    end,
    TeOverride =
	case application:get_env(snmp, snmp_trapentry_override) of
	    {ok, true} -> true;
	    _ -> false
    end,
    MultiT =
	case application:get_env(snmp, snmp_multi_threaded) of
	    {ok, true} -> true;
	    {ok, false} -> false;
	    {ok, Bad2} -> exit({bad_config,{snmp,{snmp_multi_threaded,Bad2}}});
	    _ -> false
    end,
    Vsns = snmp_misc:get_vsns(),
    SupVerbosity =
	case application:get_env(snmp, snmp_supervisor_verbosity) of
	    {ok, V4} -> V4;
	    _ -> silence
	end,
    ErrorReportMod =
	case application:get_env(snmp, snmp_error_report_mod) of
	    {ok, Mod} -> Mod;
	    _ -> snmp_error
	end,
    case application:get_env(snmp, snmp_agent_type) of
	{ok, sub} ->
	    SubVerbosity =
		case application:get_env(snmp, snmp_subagent_verbosity) of
		    {ok, V5} -> V5;
		    _ -> silence
		end,
	    Opts = [{supervisor_verbosity, SupVerbosity},
		    {priority, Prio},
		    {snmp_vsn, Vsns},
		    {multi_threaded, MultiT},
		    {mibserver_verbosity, MibsVerbosity},
		    {mib_storage, MibStorage},
		    {mibentry_override, MeOverride},
		    {trapentry_override, TeOverride},
		    {local_db_auto_repair,LdbAutoRepair},
		    {local_db_verbosity,LdbVerbosity},
		    {symbolic_store_verbosity,SymbolicStoreVerbosity},
		    {subagent_verbosity,SubVerbosity},
		    {error_report_mod, ErrorReportMod}],
	    case snmp_supervisor:start_sub(DbDir, Opts) of
		{ok, Pid} ->
		    {ok, Pid, []};
		Error ->
		    Error
	    end;
	_ ->
	    MasterVerbosity =
		case application:get_env(snmp, snmp_master_agent_verbosity) of
		    {ok, V6} -> V6;
		    _ -> silence
		end,
	    NoteStoreVerbosity =
		case application:get_env(snmp, snmp_note_store_verbosity) of
		    {ok, V7} -> V7;
		    _ -> silence
		end,
	    NetIfVerbosity =
		case application:get_env(snmp, snmp_net_if_verbosity) of
		    {ok, V8} -> V8;
		    _ -> silence
		end,
	    NetIfRecBuf =
		case application:get_env(snmp, snmp_net_if_recbuf) of
		    {ok, V9} -> V9;
		    _ -> use_default
		end,
	    ConfDir =
		case application:get_env(snmp, snmp_config_dir) of
		    {ok, Dir1} when list(Dir1) -> Dir1;
		    {ok, Bad3} ->
			exit({bad_config, {snmp, {snmp_config_dir, Bad3}}});
		    _ -> exit({undefined_config, {snmp, snmp_config_dir}})
		end,
	    Mibs =
		case application:get_env(snmp, snmp_master_agent_mibs) of
		    {ok, Ms} when list(Ms) -> 
			lists:map(fun(Mib) -> ConfDir ++ Mib end, Ms);
		    _ -> []
		end,
	    ForceLoad =
		case application:get_env(snmp, force_config_load) of
		    {ok, true} -> true;
		    {ok, false} -> false;
		    {ok, Bad4} ->
			exit({bad_config,{snmp,{force_config_load,Bad4}}});
		    _ -> false
		end,
	    Opts = [{supervisor_verbosity, SupVerbosity},
		    {priority, Prio},
		    {snmp_vsn, Vsns},
		    {multi_threaded, MultiT},
		    {mibs, Mibs},
		    {mibserver_verbosity, MibsVerbosity},
		    {mib_storage, MibStorage},
		    {mibentry_override, MeOverride},
		    {trapentry_override, TeOverride},
		    {force_load, ForceLoad},
		    {name, {local, snmp_master_agent}},
		    {local_db_auto_repair,LdbAutoRepair},
		    {local_db_verbosity,LdbVerbosity},
		    {master_agent_verbosity,MasterVerbosity},
		    {symbolic_store_verbosity,SymbolicStoreVerbosity},
		    {note_store_verbosity,NoteStoreVerbosity},
		    {net_if_recbuf,NetIfRecBuf},
		    {net_if_verbosity,NetIfVerbosity},
		    {error_report_mod, ErrorReportMod}],
	    case snmp_supervisor:start_master(DbDir, ConfDir, Opts) of
		{ok, Pid} when Type == normal ->
		    {ok, Pid};
		{ok, Pid} ->
		    {takeover, Node} = Type,
		    OwnInfoList = snmp:info(snmp_master_agent),
		    {value, {_, OwnLoadedMibs}} =
			lists:keysearch(loaded_mibs, 1, OwnInfoList),
		    OwnMibNames = [ Name || {Name, _, _} <- OwnLoadedMibs ],
		    case rpc:call(Node, snmp, info, [snmp_master_agent]) of
			{badrpc, R} ->
			    error_logger:info_msg("snmp: could not takeover "
						  "loaded mibs: ~p~n", [R]);
			InfoList ->
			    {value, {_, LoadedMibs}} =
				lists:keysearch(loaded_mibs, 1, InfoList),
			    MibsToLoad = [{MibName, Symbolic, FileName} || 
					     {MibName, Symbolic, FileName} <-
						 LoadedMibs,
					      not lists:member(MibName, 
							       OwnMibNames)],
			    lists:foreach(fun takeover_mib/1, MibsToLoad)
		    end,
		    {ok, Pid};
		Else ->
		    Else
	    end
    end.

stop(_) ->
    ok.

%%-----------------------------------------------------------------
%% The presence of this function means that we will accept changes
%% in the configuration parameters.  However, we won't react upon
%% those changes until the agent is restarted.  So we just return
%% ok.
%%-----------------------------------------------------------------
config_change(Changed, New, Removed) ->
    ok.



takeover_mib({MibName, _Symbolic, FileName}) ->
    case MibName of
	'STANDARD-MIB' -> ok;
	'SNMPv2-MIB' -> ok;
	_ -> 
	    case snmp:load_mibs(snmp_master_agent, [FileName]) of
		ok -> ok;
		{error, R} ->
		    error_logger:info_msg("snmp: could not reload mib ~p: ~p~n",
					  [FileName, R])
	    end
    end.

