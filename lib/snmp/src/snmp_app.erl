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
    MultiT =
	case application:get_env(snmp, snmp_multi_threaded) of
	    {ok, true} -> true;
	    {ok, false} -> false;
	    {ok, Bad2} -> exit({bad_config,{snmp,{snmp_multi_threaded,Bad2}}});
	    _ -> false
    end,
    Vsns = snmp_misc:get_vsns(),
    case application:get_env(snmp, snmp_agent_type) of
	{ok, sub} ->
	    Opts = [{priority, Prio},
		    {snmp_vsn, Vsns},
		    {multi_threaded, MultiT}],
	    case snmp_supervisor:start_sub(DbDir, Opts) of
		{ok, Pid} ->
		    {ok, Pid, []};
		Error ->
		    Error
	    end;
	_ ->
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
	    Opts = [{priority, Prio},
		    {snmp_vsn, Vsns},
		    {multi_threaded, MultiT},
		    {mibs, Mibs},
		    {force_load, ForceLoad},
		    {name, {local, snmp_master_agent}}],
	    case snmp_supervisor:start_master(DbDir, ConfDir, Opts) of
		{ok, Pid} when Type == normal ->
		    {ok, Pid};
		{ok, Pid} ->
		    {takeover, Node} = Type,
		    case rpc:call(Node, snmp, info, [snmp_master_agent]) of
			{badrpc, R} ->
			    error_logger:info_msg("snmp: could not takeover "
						  "loaded mibs: ~p~n", [R]);
			List ->
			    {value, {_, LMibs}} =
				lists:keysearch(loaded_mibs, 1, List),
			    lists:foreach(fun takeover_mib/1, LMibs)
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

