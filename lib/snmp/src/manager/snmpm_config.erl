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
%% -------------------------------------------------------------------------
%%
%% Some of the stuff stored here should really be persistent!!
%% (e.g. snmp-engine-boot)
%%
%% -------------------------------------------------------------------------

-module(snmpm_config).

-behaviour(gen_server).

%% External exports
-export([start_link/1, stop/0]).
-export([register_user/3, unregister_user/1, 
	 which_users/0, 
	 user_info/0, user_info/1, 

	 register_agent/4, unregister_agent/3, 
	 agent_info/3, update_agent_info/5, 
	 which_agents/0, which_agents/1, 

	 get_agent_engine_id/1, get_agent_engine_id/2, 
	 get_agent_engine_max_message_size/2, 
	 get_agent_version/2, 
	 get_agent_mp_model/2, 
	 get_agent_user_id/2, 
	 
	 system_info/1, 
	 get_engine_id/0, get_engine_max_message_size/0,

	 register_usm_user/3, usm_user_info/3, update_usm_user_info/4, 
	 which_usm_users/0, which_usm_users/1, 
	 get_usm_user/2, get_usm_user_from_sec_name/2, 
	 is_usm_engine_id_known/1,
	 get_engine_boots/0, get_engine_time/0, 
	 set_engine_boots/1, set_engine_time/1, 
	 get_usm_eboots/1, get_usm_etime/1, get_usm_eltime/1, 
	 set_usm_eboots/2, set_usm_etime/2, set_usm_eltime/2, 
	 set_engine_time/1, 
	 

	 cre_counter/2,
	 incr_counter/2,

	 cre_stats_counter/2,
	 maybe_cre_stats_counter/2,
	 incr_stats_counter/2,
	 reset_stats_counter/1,

	 load_mib/1, unload_mib/1, make_mini_mib/0,
	 name_to_oid/1, oid_to_name/1,

	 system_start_time/0,

	 verbosity/1 

	]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 code_change/3, terminate/2]).


%% Includes:
-include_lib("kernel/include/file.hrl").
-include("snmp_debug.hrl").
-include("snmp_types.hrl").
-include("snmp_verbosity.hrl").
-include("snmpm_usm.hrl").


%% Types:
-record(user, {id, mod, data}).

-record(state, {}).


%% Macros and Constants:
-define(SERVER, ?MODULE).

-define(USER_MOD_DEFAULT,  snmpm_user_default).
-define(USER_DATA_DEFAULT, undefined).

-ifdef(snmp_debug).
-define(GS_START_LINK(Opts),
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Opts], 
			      [{debug,[trace]}])).
-else.
-define(GS_START_LINK(Opts),
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Opts], [])).
-endif.



%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------
start_link(Opts) -> 
    ?d("start_link -> entry with"
       "~n   Opts: ~p", [Opts]),
    ?GS_START_LINK(Opts).

stop() ->
    call(stop).

register_user(UserId, UserMod, UserData) when UserId =/= default_user ->
    case (catch verify_user_behaviour(UserMod)) of
	ok ->
	    call({register_user, UserId, UserMod, UserData});
	Error ->
	    Error
    end;
register_user(UserId, _, _) ->
    {error, {bad_user_id, UserId}}.

verify_user_behaviour(UserMod) ->
    case snmp_misc:verify_behaviour(snmpm_user, UserMod) of
	ok ->
	    ok;
	Error ->
	    throw(Error)
    end.


unregister_user(UserId) when UserId =/= default_user ->
    call({unregister_user, UserId});
unregister_user(BadUserId) ->
    {error, {bad_user_id, BadUserId}}.

which_users() ->
    Pattern = #user{id = '$1', _ = '_'},
    Match   = ets:match(snmpm_user_table, Pattern),
    [UserId || [UserId] <- Match, UserId =/= default_user].


user_info() ->
    UserId = default_user,
    case user_info(UserId) of
	{ok, Mod, Data} ->
	    {ok, UserId, Mod, Data};
	Error ->
	    Error
    end.

user_info(UserId) ->
    case ets:lookup(snmpm_user_table, UserId) of
	[#user{mod = UserMod, data = UserData}] ->
	    {ok, UserMod, UserData};
	_ ->
	    {error, not_found}
    end.


register_agent(UserId, Addr0, Port, Config) ->
    %% Check: 
    %%   1) That the mandatory configs (none at the moment) are present
    %%   2) That the illegal configs user_id (used internally) is 
    %%      not present
    %%   3) Check that there are no invalid or erroneous configs
    %%   4) Chack that the manager is capable to use the selected version
    case verify_agent_config(Config) of
	ok ->
	    Addr = normalize_address(Addr0),
	    call({register_agent, UserId, Addr, Port, Config});
	Error ->
	    Error
    end.

verify_agent_config(Conf) ->
    case verify_mandatory(Conf, []) of
	ok ->
	    case verify_invalid(Conf, [user_id]) of
		ok ->
		    case verify_agent_config2(Conf) of
			ok ->
			    {ok, Vsns} = system_info(versions),
			    Vsn = 
				case lists:keysearch(version, 1, Conf) of
				    {value, {version, V}} ->
					V;
				    false ->
					v1
				end,
			    case lists:member(Vsn, Vsns) of
				true ->
				    ok;
				false ->
				    {error, {version_not_supported_by_manager, Vsn, Vsns}}
			    end
		    end;
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

verify_agent_config2(Conf) ->
    verify_agent2(Conf).


unregister_agent(UserId, Addr0, Port) ->
    Addr = normalize_address(Addr0),
    call({unregister_agent, UserId, Addr, Port}).

agent_info(Addr0, Port, all) ->
    Addr = normalize_address(Addr0),
    case ets:match_object(snmpm_agent_table, {{Addr, Port, '_'}, '_'}) of
	[] ->
	    {error, not_found};
	All ->
	    {ok, [{Item, Val} || {{_, _, Item}, Val} <- All,
				Item /= address, 
				     Item /= port, 
				     Item /= user_id]}
    end;
agent_info(Addr0, Port, Item) 
  when Item /= address; Item /= port; Item /= user_id ->
    Addr = normalize_address(Addr0),
    case ets:lookup(snmpm_agent_table, {Addr, Port, Item}) of
	[{_, Val}] ->
	    {ok, Val};
	[] ->
	    {error, not_found}
    end.


which_agents() ->
    which_agents('_').

which_agents(UserId) ->
    Pat = {{'$1', '$2', user_id}, UserId},
    Agents = ets:match(snmpm_agent_table, Pat),
    [{Addr, Port} || [Addr, Port] <- Agents].

    
update_agent_info(UserId, Addr0, Port, Item, Val0)  
  when Item /= address; Item /= port; Item /= user_id ->
    case (catch verify_val(Item, Val0)) of
	{ok, Val} ->
	    Addr = normalize_address(Addr0),
	    call({update_agent_info, UserId, Addr, Port, Item, Val});
	Error ->
	    Error
    end.

get_agent_engine_id(Name) ->
    Pat = {{'$1', '$2', name}, Name},
    case ets:match(snmpm_agent_table, Pat) of
	[[Addr, Port]|_] ->
	    %% If the user has been dum enough to give several agents
	    %% the same name, we pick the first we find...
	    agent_info(Addr, Port, engine_id);
	_ ->
	    {error, not_found}
    end.

get_agent_engine_id(Addr, Port) ->
    agent_info(Addr, Port, engine_id).

get_agent_engine_max_message_size(Addr, Port) ->
    agent_info(Addr, Port, max_message_size).

get_agent_version(Addr, Port) ->
    agent_info(Addr, Port, version). %% MP-model

get_agent_mp_model(Addr, Port) ->
    case agent_info(Addr, Port, version) of
	{ok, v2} ->
	    {ok, v2c};
	{ok, V} ->
	    {ok, V};
	Err ->
	    Err
    end.

get_agent_user_id(Addr, Port) ->
    agent_info(Addr, Port, user_id).


system_info(Key) when atom(Key) ->
    case ets:lookup(snmpm_config_table, Key) of
	[{_, Val}] ->
	    {ok, Val};
	_ ->
	    {error, not_found}
    end.

system_start_time() ->
    system_info(system_start_time).

get_engine_id() ->
    system_info(engine_id).

get_engine_max_message_size() ->
    system_info(max_message_size).

get_engine_boots() ->
    case dets:lookup(snmpm_config_db, snmp_engine_boots) of
	[{_, Boots}] ->
	    {ok, Boots};
	_ ->
	    {error, not_found}
    end.

set_engine_boots(Boots) ->
    case (whereis(?SERVER) == self()) of
	false ->
	    call({set_engine_boots, Boots});
	true ->
	    dets:insert(snmpm_config_db, {snmp_engine_boots, Boots}),
	    ok
    end.
    

get_engine_time() ->
    case system_info(snmp_engine_base) of
	{ok, EngineBase} ->
	    {ok, snmp_misc:now(sec) - EngineBase};
	Error ->
	    Error
    end.

get_usm_eboots(SnmpEngineID) ->
    Key = {eboots, SnmpEngineID},
    case get_usm_cache(Key) of
	{ok, Boots} ->
	    {ok, Boots};
	_ ->
	    {ok, 0}
    end.

get_usm_etime(SnmpEngineID) ->
    Key = {etime, SnmpEngineID},
    case get_usm_cache(Key) of
	{ok, Diff} ->
	    {ok, snmp_misc:now(sec) - Diff};
	_ ->
	    {ok, 0}
    end.

get_usm_eltime(SnmpEngineID) ->
    Key = {eltime, SnmpEngineID},
    case get_usm_cache(Key) of
	{ok, Time} ->
	    {ok, Time};
	_ ->
	    {ok, 0}
    end.

get_usm_cache(Key) ->
    case ets:lookup(snmpm_usm_table, {usm_cache, Key}) of
	[{_, Val}] ->
	    {ok, Val};
	_ ->
	    {error, not_found}
    end.
    
set_usm_eboots(SnmpEngineID, EngineBoots) ->
    set_usm_cache({eboots, SnmpEngineID}, EngineBoots).

set_usm_etime(SnmpEngineID, Diff) ->
    set_usm_cache({etime, SnmpEngineID}, Diff).

set_usm_eltime(SnmpEngineID, Time) ->
    set_usm_cache({eltime, SnmpEngineID}, Time).

set_usm_cache(Key, Val) ->
    call({set_usm_cache, Key, Val}).
    
set_engine_time(Time) ->
    call({set_engine_time, Time}).

register_usm_user(EngineID, Name, Config) when list(EngineID), list(Name) ->
    case verify_usm_user_config(EngineID, Name, Config) of
	{ok, User} ->
	    call({register_usm_user, User});
	Error ->
	    Error
    end.

verify_usm_user_config(EngineID, Name, Config) ->
    case verify_mandatory(Config, []) of
	ok ->
	    case verify_invalid(Config, [engine_id, name]) of
		ok ->
		    verify_usm_user_config2(EngineID, Name, Config);
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end.

verify_usm_user_config2(EngineID, Name, Config) ->
    SecName = verify_usm_user_get(sec_name, Name,              Config),
    Auth    = verify_usm_user_get(auth,     usmNoAuthProtocol, Config),
    AuthKey = verify_usm_user_get(auth_key, [],                Config),
    Priv    = verify_usm_user_get(priv,     usmNoPrivProtocol, Config),
    PrivKey = verify_usm_user_get(priv_key, [],                Config),
    User = {EngineID, Name, SecName, Auth, AuthKey, Priv, PrivKey},
    verify_usm_user(User).
	
verify_usm_user_get(Item, Default, Config) ->	
    case lists:keysearch(Item, 1, Config) of
	{value, {_, Val}} ->
	    Val;
	false ->
	    Default
    end.

which_usm_users() ->
    Pattern = {usm_key('$1', '$2'), '_'},
    Match   = ets:match(snmpm_usm_table, Pattern),
    [{EngineID, UserName} || [EngineID, UserName] <- Match].

which_usm_users(EngineID) ->
    Pattern = {usm_key(EngineID, '$1'), '_'},
    Match   = ets:match(snmpm_usm_table, Pattern),
    [UserName || [UserName] <- Match].

usm_user_info(EngineID, UserName, Item) ->
    case ets:lookup(snmpm_usm_table, usm_key(EngineID, UserName)) of
	[] ->
	    {error, not_found};
	[{_Key, UsmUser}] ->
	    do_usm_user_info(UsmUser, Item)
    end.

do_usm_user_info(#usm_user{sec_name = SecName}, sec_name) ->
    {ok, SecName};
do_usm_user_info(#usm_user{auth = AuthP}, auth) ->
    {ok, AuthP};
do_usm_user_info(#usm_user{auth_key = AuthKey}, auth_key) ->
    {ok, AuthKey};
do_usm_user_info(#usm_user{priv = PrivP}, priv) ->
    {ok, PrivP};
do_usm_user_info(#usm_user{priv_key = PrivKey}, priv_key) ->
    {ok, PrivKey};
do_usm_user_info(#usm_user{engine_id = EngineID}, engine_id) ->
    {ok, EngineID};
do_usm_user_info(#usm_user{name = Name}, name) ->
    {ok, Name};
do_usm_user_info(_, Item) ->
    {error, {bad_iten, Item}}.

update_usm_user_info(EngineID, UserName, Item, Val) 
  when Item =/= engine_id, Item =/= name ->
    call({update_usm_user_info, EngineID, UserName, Item, Val}).

get_usm_user(EngineID, UserName) ->
    Key = {EngineID, UserName},
    case ets:lookup(snmpm_usm_table, Key) of
	[{_, User}] ->
	    {ok, User};
	_ ->
	    {error, not_found}
    end.

is_usm_engine_id_known(EngineID) ->
    Pattern = {usm_key(EngineID, '$1'), '_'},
    case ets:match(snmpm_usm_table, Pattern) of
	[] ->
	    false;
	_ ->
	    true
    end.

get_usm_user_from_sec_name(EngineID, SecName) ->
    %% Since the normal mapping between UserName and SecName is the
    %% identity-function, we first try to use the SecName as UserName,
    %% and check the resulting row.  If it doesn't match, we'll have to
    %% loop through the entire table.
    Key = {EngineID, SecName},
    case ets:lookup(snmpm_usm_table, Key) of
	[{Key, #usm_user{sec_name = SecName} = User}] ->
	    {ok, User};
	_ ->
	    %% That did not work, so we have to search
	    Pattern = {usm_key(EngineID, '_'), 
		       #usm_user{sec_name = SecName, _ = '_'}},
	    case ets:match_object(snmpm_usm_table, Pattern) of
		[User|_] ->
		    {ok, User};
		_ ->
		    {error, not_found}
	    end
    end.


%% Wrap-counters (wrapping at 2147483647 or 4294967295)
cre_counter(Counter, Initial) ->
    case (whereis(?SERVER) == self()) of
	false ->
	    call({cre_counter, Counter, Initial});
	true ->
	    ets:insert(snmpm_counter_table, {Counter, Initial}),
	    Initial
    end.

incr_counter(usm_salt, Incr) ->
    incr_counter(usm_salt, Incr, 4294967295);
incr_counter(Counter, Incr) ->
    incr_counter(Counter, Incr, 2147483647).

incr_counter(Counter, Incr, Wrap) ->
    case (catch ets:update_counter(snmpm_counter_table, Counter, Incr)) of
	{'EXIT', _} ->
	    cre_counter(Counter, Incr);
	NewVal when NewVal =< Wrap ->
	    NewVal;
	N ->
	    cre_counter(Counter, N - Wrap)
    end.


maybe_cre_stats_counter(Counter, Initial) ->
    case ets:lookup(snmpm_stats_table, {Counter, Initial}) of
	[_] ->
	    ok;
	_ ->
	    cre_stats_counter(Counter, Initial)
    end.
			      
cre_stats_counter(Counter, Initial) ->
    case (whereis(?SERVER) == self()) of
	false ->
	    call({cre_stats_counter, Counter, Initial});
	true ->
	    ets:insert(snmpm_stats_table, {Counter, Initial}),
	    Initial
    end.

incr_stats_counter(Counter, Incr) ->
    case (catch ets:update_counter(snmpm_stats_table, Counter, Incr)) of
	{'EXIT', _} ->
	    cre_counter(Counter, Incr);
	NewVal ->
	    NewVal
    end.

reset_stats_counter(Counter) ->
    case (whereis(?SERVER) == self()) of
	false ->
	    call({reset_stats_counter, Counter});
	true ->
	    ets:insert(snmpm_stats_table, {Counter, 0})
    end,
    ok.
    
    
    
load_mib(Mib) when list(Mib) ->
    call({load_mib, Mib}).

unload_mib(Mib) when list(Mib) ->
    call({unload_mib, Mib}).

make_mini_mib() ->
    Pat = {{mini_mib, '$1'}, '$2', '$3', '_'},
    MiniElems = ets:match(snmpm_mib_table, Pat),
    [list_to_tuple(MiniElem) || MiniElem <- MiniElems].


name_to_oid(Name) ->
    Pat = {{mini_mib, '$1'}, Name, '_', '_'},
    case ets:match(snmpm_mib_table, Pat) of
	[] ->
	    {error, not_found};
	Oids ->
	    {ok, Oids}
    end.

oid_to_name(Oid) ->
    case ets:lookup(snmpm_mib_table, {mini_mib, Oid}) of
	[{_, Name, _, _}] ->
	    {ok, Name};
	[] ->
	    {error, not_found}
    end.


verbosity(Verbosity) ->
    case ?vvalidate(Verbosity) of
	Verbosity ->
	    call({verbosity, Verbosity});
	_ ->
	    {error, {invalid_verbosity, Verbosity}}
    end.


%%%-------------------------------------------------------------------
%%% Callback functions from gen_server
%%%-------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%--------------------------------------------------------------------
init([Opts]) ->
%     put(sname, mconf),
%     put(verbosity, trace),
    ?d("init -> entry with"
       "~n   Opts: ~p", [Opts]),
    case (catch do_init(Opts)) of
	ok ->
	    {ok, #state{}};
	{error, Reason} ->
	    error_msg("init error: ~p", [Reason]),
	    {stop, Reason};
	{'EXIT', Reason} ->
	    error_msg("init exit: ~p", [Reason]),
	    {stop, Reason};
	Error ->
	    error_msg("init failed: ~p", [Error]),
	    {stop, Error}
    end.

do_init(Opts) ->
    %% Mandatory = [versions, {config, [dir]}],
    Mandatory = [{config, [dir, db_dir]}],
    verify_options(Opts, Mandatory),

    ets:new(snmpm_counter_table, [set, public,    named_table, {keypos, 1}]),
    ets:new(snmpm_stats_table,   [set, public,    named_table, {keypos, 1}]),
    ets:new(snmpm_mib_table,     [set, protected, named_table, {keypos, 1}]),
    ets:new(snmpm_config_table,  [set, protected, named_table, {keypos, 1}]),
    ets:new(snmpm_agent_table,   [set, protected, named_table, {keypos, 1}]),
    ets:new(snmpm_user_table,    [set, protected, named_table, {keypos, 2}]),
    ets:new(snmpm_usm_table,     [set, protected, named_table, {keypos, 1}]),

    %% -- System start time --
    ets:insert(snmpm_config_table, {system_start_time, snmp_misc:now(cs)}),
    
    %% --- Own options (dir and db_dir mandatory) ---
    ConfOpts  = get_opt(config,    Opts,      []),
    ConfVerb  = get_opt(verbosity, ConfOpts, silence),
    ConfDir   = get_opt(dir,       ConfOpts),
    ConfDbDir = get_opt(db_dir,    ConfOpts),
    ConfRep   = get_opt(repair,    ConfOpts, true),
    ConfAs    = get_opt(auto_save, ConfOpts, 5000),
    ets:insert(snmpm_config_table, {config_verbosity, ConfVerb}),
    ets:insert(snmpm_config_table, {config_dir,       ConfDir}),
    ets:insert(snmpm_config_table, {config_db_dir,    ConfDbDir}),
    ets:insert(snmpm_config_table, {config_repair,    ConfRep}),
    ets:insert(snmpm_config_table, {config_auto_save, ConfAs}),
    put(sname, mconf),
    put(verbosity, ConfVerb),
    ?vlog("starting", []),

    %% -- Create dets file used for storing persistent data --
    dets_open(ConfDbDir, ConfRep, ConfAs),
    
    %% -- Prio (optional) --
    Prio = get_opt(priority, Opts, normal),
    ets:insert(snmpm_config_table, {prio, Prio}),
    process_flag(priority, Prio),

    %% -- Server (optional) --
    ServerOpts = get_opt(server,    Opts,      []),
    ServerVerb = get_opt(verbosity, ServerOpts, silence),
    ServerGct  = get_opt(timeout,   ServerOpts, 30000),
    ets:insert(snmpm_config_table, {server_verbosity, ServerVerb}),
    ets:insert(snmpm_config_table, {server_timeout,   ServerGct}),
   
    %% -- Mibs (optional) --
    ?vdebug("initiate mini mib", []),
    Mibs = get_opt(mibs, Opts, []),
    ets:insert(snmpm_config_table, {mibs, Mibs}),
    init_mini_mib(Mibs),

    %% -- Net-if (optional) --
    ?vdebug("net_if options", []),
    NetIfOpts    = get_opt(net_if,    Opts,      []),
    NetIfMod     = get_opt(module,    NetIfOpts, snmpm_net_if),
    NetIfVerb    = get_opt(verbosity, NetIfOpts, silence),
    NetIfOptions = get_opt(options,   NetIfOpts, []),
    ets:insert(snmpm_config_table, {net_if_module,    NetIfMod}),
    ets:insert(snmpm_config_table, {net_if_verbosity, NetIfVerb}),
    ets:insert(snmpm_config_table, {net_if_options,   NetIfOptions}),

    %% -- Versions (optional) --
    %% -- Versions (mandatory) ???????????? --
    ?vdebug("versions", []),
    Vsns = get_opt(versions, Opts, [v1, v2, v3]),
    ets:insert(snmpm_config_table, {versions, Vsns}),

    %% -- Audit trail log (optional) --
    ?vdebug("audit trail log", []),
    case get_opt(audit_trail_log, Opts, []) of
	[] ->
	    ?vtrace("no ATL", []),
	    ets:insert(snmpm_config_table, {audit_trail_log, false});
	AuditTrailLogOpts ->
	    ?vtrace("ATL options: ~p", [AuditTrailLogOpts]),
	    ets:insert(snmpm_config_table, {audit_trail_log, true}),
	    LogDir  = get_atl_dir(AuditTrailLogOpts),
	    LogType = get_atl_type(AuditTrailLogOpts),
	    LogSize = get_atl_size(AuditTrailLogOpts),
	    LogRep  = get_atl_repair(AuditTrailLogOpts),
	    ets:insert(snmpm_config_table, {audit_trail_log_dir,    LogDir}),
	    ets:insert(snmpm_config_table, {audit_trail_log_type,   LogType}),
	    ets:insert(snmpm_config_table, {audit_trail_log_size,   LogSize}),
	    ets:insert(snmpm_config_table, {audit_trail_log_repair, LogRep})
    end,

    %% -- User (optional) --
    ?vdebug("default user", []),
    DefUserMod  = get_opt(def_user_mod,  Opts, ?USER_MOD_DEFAULT),
    DefUserData = get_opt(def_user_data, Opts, ?USER_DATA_DEFAULT),
    ets:insert(snmpm_config_table, {def_user_mod,  DefUserMod}),
    ets:insert(snmpm_config_table, {def_user_data, DefUserData}),
    DefUser = #user{id = default_user, mod = DefUserMod, data = DefUserData},
    ok = handle_register_user(DefUser),
    
    %% -- Note store --
    ?vdebug("note store", []),
    NoteStoreOpts    = get_opt(note_store, Opts, []),
    NoteStoreVerb    = get_opt(verbosity,  NoteStoreOpts, silence),
    NoteStoreTimeout = get_opt(timeout,    NoteStoreOpts, 30000),
    ets:insert(snmpm_config_table, {note_store_verbosity, NoteStoreVerb}),
    ets:insert(snmpm_config_table, {note_store_timeout,   NoteStoreTimeout}),
    
    %% -- Manager SNMP config --
    ?vdebug("manager snmp config", []),
    MgrConf = read_manager_config_file(ConfDir),
    init_manager_config(MgrConf),

    %% -- User config --
    ?vdebug("users config", []),
    Users = read_users_config_file(ConfDir),
    init_users_config(Users),

    %% -- Agents config --
    ?vdebug("agents config", []),
    init_agent_default(),
    Agents = read_agents_config_file(ConfDir),
    init_agents_config(Agents),

    %% -- USM config --
    UsmUsers = read_usm_config_file(ConfDir),
    init_usm_users_config(UsmUsers),

    %% -- snmp engine init --
    init_engine(),

    ?vlog("started", []),
    ok.


dets_open(Dir, Repair, AutoSave) ->
    Name     = snmpm_config_db,
    Filename = dets_filename(Name, Dir),
    Opts     = [{repair, Repair}, {auto_save, AutoSave}, {file, Filename}],
    case dets:open_file(Name, Opts) of
	{ok, _Dets} ->
	    ok;
	{error, Reason} ->
	    error({failed_open_dets, Reason})
    end.

dets_filename(Name, Dir) ->
    filename:join(dets_filename1(Dir), Name).
                                                                                                 
dets_filename1([])  -> ".";
dets_filename1(Dir) -> Dir.

%% ------------------------------------------------------------------------

init_engine() ->
    case get_engine_boots() of
	{ok, Val} when Val < 2147483647 ->
	    set_engine_boots(Val + 1);
	{ok, _} ->
	    ok;
	_ ->
	    set_engine_boots(1)
    end,
    reset_engine_base().

reset_engine_base() ->
    ets:insert(snmpm_config_table, {snmp_engine_base, snmp_misc:now(sec)}).

%% ------------------------------------------------------------------------

verify_options(Opts, Mandatory) ->
    ?d("verify_options -> entry with"
       "~n   Opts:      ~p"
       "~n   Mandatory: ~p", [Opts, Mandatory]),
    verify_mandatory_options(Opts, Mandatory),
    verify_options(Opts).

%% mandatory() -> [mand()]
%% mand() -> atom() | {atom, [atom()]}
verify_mandatory_options(_Opts, []) ->
    ok;
verify_mandatory_options(Opts, [Mand|Mands]) ->
    verify_mandatory_option(Opts, Mand),
    verify_mandatory_options(Opts, Mands).

verify_mandatory_option(Opts, {Mand, MandSubOpts}) ->
    ?d("verify_mandatory_option -> entry with"
       "~n   Mand:        ~p"
       "~n   MandSubObjs: ~p", [Mand, MandSubOpts]),
    case lists:keysearch(Mand, 1, Opts) of
	{value, {Mand, SubOpts}} ->
	    verify_mandatory_options(SubOpts, MandSubOpts);
	false ->
	    ?d("missing mandatory option: ~w [~p]", [Mand, MandSubOpts]),
	    error({missing_mandatory, Mand, MandSubOpts})
    end;
verify_mandatory_option(Opts, Mand) ->
    ?d("verify_mandatory_option -> entry with"
       "~n   Mand:        ~p", [Mand]),
    case lists:keymember(Mand, 1, Opts) of
	true ->
	    ok;
	false ->
	    ?d("missing mandatory option: ~w", [Mand]),
	    error({missing_mandatory, Mand})
    end.
	
verify_options([]) ->
    ?d("verify_options -> done", []),
    ok;
verify_options([Opt|Opts]) ->
    ?d("verify_options -> entry with"
       "~n   Opt: ~p", [Opt]),
    verify_option(Opt),
    verify_options(Opts).

verify_option({prio, Prio}) ->
    verify_prio(Prio);
verify_option({mibs, Mibs}) ->
    verify_mibs(Mibs);
verify_option({net_if, NetIfOpts}) ->
    verify_net_if_opts(NetIfOpts);
verify_option({server, ServerOpts}) ->
    verify_server_opts(ServerOpts);
verify_option({note_store, NoteStoreOpts}) ->
    verify_note_store_opts(NoteStoreOpts);
verify_option({config, ConfOpts}) ->
    verify_config_opts(ConfOpts);
verify_option({versions, Vsns}) ->
    verify_versions(Vsns);
verify_option({audit_trail_log, LogOpts}) ->
    Mandatory = [dir, size],
    case (catch verify_mandatory_options(LogOpts, Mandatory)) of
	ok ->
	    verify_audit_trail_log_opts(LogOpts);
	{error, {missing_mandatory, LogOpt}} ->
	    error({missing_mandatory, audit_trail_log, LogOpt})
    end;
verify_option({def_user_mod, Mod}) ->
    verify_module(def_user_mod, Mod);
verify_option({def_user_data, _Data}) ->
    ok;
verify_option(Opt) ->
    {error, {invalid_option, Opt}}.

verify_prio(Prio) when atom(Prio) ->
    ok;
verify_prio(Prio) ->
    error({invalid_prio, Prio}).

verify_mibs([]) ->
    ok;
verify_mibs([Mib|Mibs]) when list(Mib) ->
    verify_mibs(Mibs);
verify_mibs(Mibs) ->
    error({invalid_mibs, Mibs}).

verify_config_opts([]) ->
    ok;
verify_config_opts([{verbosity, Verbosity}|Opts]) ->
    verify_verbosity(Verbosity),
    verify_config_opts(Opts);
verify_config_opts([{dir, Dir}|Opts]) ->
    verify_conf_dir(Dir),
    verify_config_opts(Opts);
verify_config_opts([{db_dir, Dir}|Opts]) ->
    verify_conf_db_dir(Dir),
    verify_config_opts(Opts);
verify_config_opts([{repair, Repair}|Opts]) ->
    verify_conf_repair(Repair),
    verify_config_opts(Opts);
verify_config_opts([{auto_save, AutoSave}|Opts]) ->
    verify_conf_auto_save(AutoSave),
    verify_config_opts(Opts);
verify_config_opts([Opt|_]) ->
    error({invalid_config_option, Opt}).

verify_server_opts([]) ->
    ok;
verify_server_opts([{verbosity, Verbosity}|Opts]) ->
    verify_verbosity(Verbosity),
    verify_server_opts(Opts);
verify_server_opts([{timeout, Timeout}|Opts]) ->
    verify_server_timeout(Timeout),
    verify_server_opts(Opts);
verify_server_opts([Opt|_]) ->
    error({invalid_server_option, Opt}).

verify_server_timeout(T) when integer(T), T > 0 ->
    ok;
verify_server_timeout(T) ->
    error({invalid_server_timeout, T}).

verify_net_if_opts([]) ->
    ok;
verify_net_if_opts([{module, Mod}|Opts]) ->
    verify_network_interface_behaviour(Mod),
    verify_net_if_opts(Opts);
verify_net_if_opts([{verbosity, Verbosity}|Opts]) ->
    verify_verbosity(Verbosity),
    verify_net_if_opts(Opts);
verify_net_if_opts([{options, Options}|Opts]) when list(Options) ->
    verify_net_if_opts(Opts);
verify_net_if_opts([Opt|_]) ->
    error({invalid_net_if_option, Opt}).

verify_network_interface_behaviour(Mod) ->
    case snmp_misc:verify_behaviour(snmpm_network_interface, Mod) of
	ok ->
	    ok;
	Error ->
	    throw(Error)
    end.
    
    
verify_note_store_opts([]) ->
    ok;
verify_note_store_opts([{verbosity, Verbosity}|Opts]) ->
    verify_verbosity(Verbosity),
    verify_note_store_opts(Opts);
verify_note_store_opts([{timeout, Timeout}|Opts]) ->
    verify_note_store_timeout(Timeout),
    verify_note_store_opts(Opts);
verify_note_store_opts([Opt|_]) ->
    error({invalid_note_store_option, Opt}).

verify_note_store_timeout(T) when integer(T), T > 0 ->
    ok;
verify_note_store_timeout(T) ->
    error({invalid_note_store_timeout, T}).

verify_conf_dir(Dir) ->
    case (catch verify_dir(Dir)) of
	ok ->
	    ok;
	{error, Reason} ->
	    error({invalid_conf_dir, Dir, Reason});
	_ ->
	    error({invalid_conf_dir, Dir})
    end.

verify_conf_db_dir(Dir) ->
    case (catch verify_dir(Dir)) of
	ok ->
	    ok;
	{error, Reason} ->
	    error({invalid_conf_db_dir, Dir, Reason});
	_ ->
	    error({invalid_conf_db_dir, Dir})
    end.


verify_conf_repair(true) ->
    ok;
verify_conf_repair(false) ->
    ok;
verify_conf_repair(force) ->
    ok;
verify_conf_repair(InvalidRepair) ->
    error({invalid_conf_db_repair, InvalidRepair}).


verify_conf_auto_save(infinity) ->
    ok;
verify_conf_auto_save(AutoSave) when integer(AutoSave), AutoSave > 0 ->
    ok;
verify_conf_auto_save(InvalidAutoSave) ->
    error({invalid_conf_db_auto_save, InvalidAutoSave}).


verify_versions([]) ->
    ok;
verify_versions([Vsn|Vsns]) ->
    verify_version(Vsn),
    verify_versions(Vsns).

verify_version(v1) ->
    ok;
verify_version(v2) ->
    ok;
verify_version(v3) ->
    ok;
verify_version(Vsn) ->
    error({invalid_version, Vsn}).

verify_audit_trail_log_opts([]) ->
    ok;
verify_audit_trail_log_opts([{dir, Dir}|Opts]) ->
    verify_log_dir(Dir),
    verify_audit_trail_log_opts(Opts);
verify_audit_trail_log_opts([{type, Type}|Opts]) ->
    verify_log_type(Type),
    verify_audit_trail_log_opts(Opts);
verify_audit_trail_log_opts([{size, Size}|Opts]) ->
    verify_log_size(Size),
    verify_audit_trail_log_opts(Opts);
verify_audit_trail_log_opts([{repair, Repair}|Opts]) ->
    verify_log_repair(Repair),
    verify_audit_trail_log_opts(Opts);
verify_audit_trail_log_opts([Opt|_Opts]) ->
    error({invalid_audit_trail_log_option, Opt}).

verify_log_type(read) ->
    ok;
verify_log_type(write) ->
    ok;
verify_log_type(read_write) ->
    ok;
verify_log_type(Type) ->
    error({invalid_audit_trail_log_type, Type}).

verify_log_dir(Dir) ->
    case (catch verify_dir(Dir)) of
	ok ->
	    ok;
	{error, Reason} ->
	    error({invalid_audit_trail_log_dir, Dir, Reason});
	_ ->
	    error({invalid_audit_trail_log_dir, Dir})
    end.

verify_log_size(Sz) when integer(Sz), Sz > 0 ->
    ok;
verify_log_size(infinity) ->
    ok;
verify_log_size({MaxNoBytes, MaxNoFiles}) 
  when integer(MaxNoBytes), MaxNoBytes > 0,
       integer(MaxNoFiles), MaxNoFiles > 0, MaxNoFiles < 65000 ->
    ok;
verify_log_size(Sz) ->
    error({invalid_audit_trail_log_size, Sz}).

verify_log_repair(true) -> ok;
verify_log_repair(false) -> ok;
verify_log_repair(truncate) -> ok;
verify_log_repair(Repair) ->
    error({invalid_audit_trail_log_repair, Repair}).


verify_module(_, Mod) when atom(Mod) ->
    ok;
verify_module(ReasonTag, Mod) ->
    error({invalid_module, ReasonTag, Mod}).

% verify_bool(_, true) ->
%     ok;
% verify_bool(_, false) ->
%     ok;
% verify_bool(ReasonTag, Bool) ->
%     error({invalid_bool, ReasonTag, Bool}).

verify_dir(Dir) when list(Dir) ->
    case file:read_file_info(Dir) of
	{ok, #file_info{type = directory}} ->
	    ok;
	{ok, _} ->
	    {error, not_directory};
	{error, _Reason} ->
	    {error, not_found}
    end;
verify_dir(Dir) ->
    {error, {invalid_log_dir, Dir}}.


verify_verbosity(Verbosity) ->
    case snmp_verbosity:validate(Verbosity) of
	Verbosity ->
	    ok;
	_ ->
	    error({invalid_verbosity, Verbosity})
    end.
    
%% ------------------------------------------------------------------------

init_manager_config([]) ->
    ok;
init_manager_config([{Key, Val}|Confs]) ->
    ets:insert(snmpm_config_table, {Key, Val}),
    init_manager_config(Confs).


init_agent_default() ->
    %% Name
    init_agent_default(target_name, "agent-default"),

    %% EngineId
    init_agent_default(engine_id, "agentEngine-default"),

    %% Timeout
    init_agent_default(timeout, 10000),

    %% Max message (packet) size
    init_agent_default(max_message_size, 484),

    %% MPModel
    init_agent_default(version, v2),

    %% SecModel
    init_agent_default(sec_model, v2c),

    %% SecName
    init_agent_default(sec_name, "initial"),

    %% SecLevel
    init_agent_default(sec_level, noAuthNoPriv),

    %% Community
    init_agent_default(community, "all-rights"),

    ok.

init_agent_default(Item, Val) 
  when Item /= address; Item /= port; Item /= user_id ->
    case do_update_agent_info(default, default, Item, Val) of
	ok ->
	    ok;
	{error, Reason} ->
	    error(Reason)
    end.


read_agents_config_file(Dir) ->
    Check = fun(C) -> check_agent_config(C) end,
    case read_file(Dir, "agents.conf", Check, []) of
	{ok, Conf} ->
	    Conf;
	Error ->
	    ?vlog("agent config error: ~p", [Error]),
	    throw(Error)
    end.

check_agent_config({UserId, 
		    TargetName, 
		    Community, 
		    Ip, Port, 
		    EngineId, 
		    Timeout, MaxMessageSize, 
		    Version, SecModel, SecName, SecLevel}) ->
    ?vtrace("check_agent_config -> entry with"
	    "~n   UserId:         ~p"
	    "~n   TargetName:     ~p"
	    "~n   Community:      ~p"
	    "~n   Ip:             ~p"
	    "~n   Port:           ~p"
	    "~n   EngineId:       ~p"
	    "~n   Timeout:        ~p"
	    "~n   MaxMessageSize: ~p"
	    "~n   Version:        ~p"
	    "~n   SecModel:       ~p"
	    "~n   SecName:        ~p"
	    "~n   SecLevel:       ~p", 
	    [UserId, TargetName, Community, Ip, Port, 
	     EngineId, Timeout, MaxMessageSize, 
	     Version, SecModel, SecName, SecLevel]),
    Addr = normalize_address(Ip),
    ?vtrace("check_agent_config -> Addr: ~p", [Addr]),
    Agent = {UserId, 
	     TargetName, 
	     Community, 
	     Addr, Port, 
	     EngineId, 
	     Timeout, MaxMessageSize, 
	     Version, SecModel, SecName, SecLevel},
    {ok, verify_agent(Agent)};
check_agent_config(Agent) ->
    error({bad_agent_config, Agent}).


init_agents_config([]) ->
    ok;
init_agents_config([Agent|Agents]) ->
    init_agent_config(Agent),
    init_agents_config(Agents).

init_agent_config({UserId, Addr, Port, Config}) ->
    case handle_register_agent(UserId, Addr, Port, Config) of
	ok ->
	    ok;
	Error ->
	    throw(Error)
    end.


verify_agent({UserId, 
	      TargetName, 
	      Comm, 
	      Ip, Port, 
	      EngineId, 
	      Timeout, MMS, 
	      Version, SecModel, SecName, SecLevel}) ->
    ?vtrace("verify_agent -> entry with"
	    "~n   UserId:     ~p"
	    "~n   TargetName: ~p", [UserId, TargetName]),
    case verify_val(address, Ip) of
	{ok, Addr} ->
	    snmp_conf:check_integer(Port, {gt, 0}),
	    Conf = 
		[{target_name,      TargetName},
		 {community,        Comm}, 
		 {engine_id,        EngineId},
		 {timeout,          Timeout},
		 {max_message_size, MMS},
		 {version,          Version},
		 {sec_model,        SecModel},
		 {sec_name,         SecName},
		 {sec_level,        SecLevel}
		],
	    case verify_agent2(Conf) of
		ok ->
		    {ok, Vsns} = system_info(versions),
		    case lists:member(Version, Vsns) of
			true ->
			    {UserId, Addr, Port, Conf};
			false ->
			    error({version_not_supported_by_manager, 
				   Version, Vsns})
		    end;
		Err ->
		    throw(Err)
	    end;
	
	Error ->
	    ?vlog("verify_agent -> failed: ~n   ~p", [Error]),
	    throw(Error)
    end.

verify_agent2([]) ->
    ok;
verify_agent2([{Item, Val}|Items]) ->
    case verify_val(Item, Val) of
	{ok, _Val} ->
	    verify_agent2(Items);
	Err ->
	    Err
    end.


read_users_config_file(Dir) ->
    Check = fun(C) -> check_user_config(C) end,
    case read_file(Dir, "users.conf", Check, []) of
	{ok, Conf} ->
	    Conf;
	Error ->
	    ?vlog("failure reading users config file: ~n   ~p", [Error]),
	    throw(Error)
    end.

check_user_config({Id, Mod, _Data} = User) when Id =/= default_user ->
    case (catch verify_user_behaviour(Mod)) of
	ok ->
	    {ok, User};
	Error ->
	    throw(Error)
    end;
check_user_config({Id, _Mod, _Data}) ->
    error({bad_user_id, Id});
check_user_config(User) ->
    error({bad_user_config, User}).

init_users_config([]) ->
    ok;
init_users_config([User|Users]) ->
    init_user_config(User),
    init_users_config(Users).

init_user_config(User) ->
    case (catch verify_user(User)) of
	{ok, UserRec} ->
	    case handle_register_user(UserRec) of
		ok ->
		    ok;
		{error, Reason} ->
		    error_msg("failed register user: "
			      "~n~w~n~w", [User, Reason])
	    end;
	{error, Reason} ->
	    error_msg("user config check failed: "
		      "~n~w~n~w", [User, Reason])
    end.
   
verify_user({Id, UserMod, UserData}) ->
    ?d("verify_user -> entry with"
       "~n   Id:       ~p"
       "~n   UserMod:  ~p"
       "~n   UserData: ~p", [Id, UserMod, UserData]),
    case (catch verify_user_behaviour(UserMod)) of
	ok ->
	    {ok, #user{id = Id, mod = UserMod, data = UserData}};
	Error ->
	    throw(Error)
    end.


read_usm_config_file(Dir) ->
    Check = fun(C) -> check_usm_user_config(C) end,
    case read_file(Dir, "usm.conf", Check, []) of
	{ok, Conf} ->
	    Conf;
	Error ->
	    throw(Error)
    end.

%% Identity-function
check_usm_user_config({EngineId, Name, 
		       AuthP, AuthKey, 
		       PrivP, PrivKey}) ->
    User = {EngineId, Name, Name, AuthP, AuthKey, PrivP, PrivKey},
    {ok, verify_usm_user(User)};
check_usm_user_config({_EngineId, _Name, _SecName, 
		       _AuthP, _AuthKey, 
		       _PrivP, _PrivKey} = User) ->
    {ok, verify_usm_user(User)};
check_usm_user_config(User) ->
    error({bad_usm_config, User}).

init_usm_users_config([]) ->
    ok;
init_usm_users_config([User|Users]) ->
    init_usm_user_config(User),
    init_usm_users_config(Users).

init_usm_user_config(User) when record(User, usm_user) ->
    case handle_register_usm_user(User) of
	ok ->
	    ok;
	Error ->
	    throw(Error)
    end.

verify_usm_user({EngineID, Name, SecName, AuthP, AuthKey, PrivP, PrivKey}) ->
    ?d("verify_usm_user -> entry with"
       "~n   EngineID: ~p"
       "~n   Name:     ~p"
       "~n   SecName:  ~p"
       "~n   AuthP:    ~p"
       "~n   AuthKey:  ~p"
       "~n   PrivP:    ~p"
       "~n   PrivKey:  ~p", 
       [EngineID, Name, SecName, AuthP, AuthKey, PrivP, PrivKey]),
    verify_usm_user_engine_id(EngineID),
    verify_usm_user_name(Name),
    verify_usm_user_sec_name(SecName),
    verify_usm_user(AuthP, AuthKey, PrivP, PrivKey),
    User = #usm_user{engine_id = EngineID,
		     name      = Name,
		     sec_name  = SecName,
		     auth      = AuthP, 
		     auth_key  = AuthKey,
		     priv      = PrivP, 
		     priv_key  = PrivKey},
    
    {ok, User}.

verify_usm_user_engine_id(EngineID) ->
    case (snmp_conf:check_string(EngineID, {gt, 0})) of
	ok ->
	    ok;
	_ ->
	    error({bad_usm_engine_id, EngineID})
    end.

verify_usm_user_name(Name) ->
    case (snmp_conf:check_string(Name, {gt, 0})) of
	ok ->
	    ok;
	_ ->
	    error({bad_usm_user_name, Name})
    end.

verify_usm_user_sec_name(Name) ->
    case (snmp_conf:check_string(Name, {gt, 0})) of
	ok ->
	    ok;
	_ ->
	    error({bad_usm_sec_name, Name})
    end.

verify_usm_user(AuthP, AuthKey, PrivP, PrivKey) ->
    verify_usm_user_auth(AuthP, AuthKey),
    verify_usm_user_priv(PrivP, PrivKey),
    ok.

verify_usm_user_auth(usmNoAuthProtocol, AuthKey) ->
    case (catch snmp_conf:check_string(AuthKey, any)) of
	ok ->
	    ok;
	_ ->
	    error({invalid_auth_key, usmNoAuthProtocol})
    end;
verify_usm_user_auth(usmHMACMD5AuthProtocol, AuthKey) 
  when list(AuthKey), length(AuthKey) == 16 ->
    case is_crypto_supported(md5_mac_96) of
	true -> 
	    case snmp_conf:all_integer(AuthKey) of
		true ->
		    ok;
		_ ->
		    error({invalid_auth_key, usmHMACMD5AuthProtocol})
	    end;
	false -> 
	    error({unsupported_crypto, md5_mac_96})
    end;    
verify_usm_user_auth(usmHMACMD5AuthProtocol, AuthKey) when list(AuthKey) ->
    Len = length(AuthKey),
    error({invalid_auth_key, usmHMACMD5AuthProtocol, Len});
verify_usm_user_auth(usmHMACMD5AuthProtocol, _AuthKey) ->
    error({invalid_auth_key, usmHMACMD5AuthProtocol});
verify_usm_user_auth(usmHMACSHAAuthProtocol, AuthKey) 
  when list(AuthKey), length(AuthKey) == 20 ->
    case is_crypto_supported(sha_mac_96) of
	true -> 
	    case snmp_conf:all_integer(AuthKey) of
		true ->
		    ok;
		_ ->
		    error({invalid_auth_key, usmHMACSHAAuthProtocol})
	    end;
	false -> 
	    error({unsupported_crypto, sha_mac_96})
    end;
verify_usm_user_auth(usmHMACSHAAuthProtocol, AuthKey) when list(AuthKey) ->
    Len = length(AuthKey),
    error({invalid_auth_key, usmHMACSHAAuthProtocol, Len});
verify_usm_user_auth(usmHMACSHAAuthProtocol, _AuthKey) ->
    error({invalid_auth_key, usmHMACSHAAuthProtocol});
verify_usm_user_auth(AuthP, _AuthKey) ->
    error({invalid_auth_protocol, AuthP}).
    
verify_usm_user_priv(usmNoPrivProtocol, PrivKey) ->
    case (catch snmp_conf:check_string(PrivKey, any)) of
	ok ->
	    ok;
	_ ->
	    error({invalid_priv_key, usmNoPrivProtocol})
    end;
verify_usm_user_priv(usmDESPrivProtocol, PrivKey) 
  when length(PrivKey) == 16 ->
    case is_crypto_supported(des_cbc_decrypt) of
	true -> 
	    case snmp_conf:all_integer(PrivKey) of
		true ->
		    ok;
		_ ->
		    error({invalid_priv_key, usmDESPrivProtocol})
	    end;
	false -> 
	    error({unsupported_crypto, des_cbc_decrypt})
    end;
verify_usm_user_priv(usmDESPrivProtocol, PrivKey) when list(PrivKey) ->
    Len = length(PrivKey),
    error({invalid_priv_key, usmDESPrivProtocol, Len});
verify_usm_user_priv(usmDESPrivProtocol, _PrivKey) ->
    error({invalid_priv_key, usmDESPrivProtocol});
verify_usm_user_priv(PrivP, _PrivKey) ->
    error({invalid_priv_protocol, PrivP}).
    
is_crypto_supported(Func) ->
    %% The 'catch' handles the case when 'crypto' is
    %% not present in the system (or not started).
    case (catch lists:member(Func, crypto:info())) of
        true -> true;
        _ -> false
    end.
 

read_manager_config_file(Dir) ->
    Check = fun(Conf) -> check_manager_config(Conf) end,
    case read_file(Dir, "manager.conf", Check) of
	{ok, Conf} ->
	    ?d("read_manager_config_file -> ok: "
	       "~n   Conf: ~p", [Conf]),
	    %% If the address is not specified, then we assume
	    %% it should be the local host.
	    %% If the address is not possible to determine
	    %% that way, then we give up...
	    check_mandatory_manager_config(Conf),
	    ensure_manager_config(Conf);
	Error ->
	    throw(Error)
    end.

default_manager_config() ->
    Addr = 
	case inet:gethostname() of
	    {ok, HostName} ->
		case inet:getaddr(HostName, inet) of
		    {ok, A} ->
			[{address, tuple_to_list(A)}];
		    {error, _Reason} ->
			?d("default_manager_config -> failed getting address: "
			   "~n   _Reason: ~p", [_Reason]),
			[]
		end;
	    {error, _Reason} ->
		?d("default_manager_config -> failed getting hostname: "
		   "~n   Reason: ~p", [_Reason]),
		[]
	end,
    Addr.
    
check_manager_config({address, Addr}) ->
    snmp_conf:check_ip(Addr);
check_manager_config({port, Port}) ->
    snmp_conf:check_integer(Port, {gt, 0});
check_manager_config({engine_id, EngineID}) ->
    snmp_conf:check_string(EngineID);
check_manager_config({max_message_size, Max}) ->
    snmp_conf:check_integer(Max, {gte, 484});
check_manager_config(Conf) ->
    {error, {unknown_config, Conf}}.


check_mandatory_manager_config(Conf) ->
    Mand  = [port, engine_id, max_message_size],
    check_mandatory_manager_config(Mand, Conf).

check_mandatory_manager_config([], _Conf) ->
    ok;
check_mandatory_manager_config([Item|Mand], Conf) ->
    case lists:keysearch(Item, 1, Conf) of
	false ->
	    error({missing_mandatory_manager_config, Item});
	_ ->
	    check_mandatory_manager_config(Mand, Conf)
    end.
    

ensure_manager_config(Confs) ->
    ensure_manager_config(Confs, default_manager_config()).

ensure_manager_config(Confs, []) ->
    Confs;
ensure_manager_config(Confs, [{Key,_} = DefKeyVal|Defs]) ->
    case lists:keysearch(Key, 1, Confs) of
	false ->
	    ensure_manager_config([DefKeyVal|Confs], Defs);
	{value, _Conf} ->
	    ensure_manager_config(Confs, Defs)
    end.

% ensure_manager_config([], Defs, Confs) ->
%     Confs ++ Defs;
% ensure_manager_config(Confs0, [{Key, DefVal}|Defs], Acc) ->
%     case lists:keysearch(Key, 1, Confs0) of
% 	false ->
% 	    ensure_manager_config(Confs0, Defs, [{Key, DefVal}|Acc]);
% 	{value, Conf} ->
% 	    Confs = lists:keydelete(Key, 1, Confs0),
% 	    ensure_manager_config(Confs, Defs, [Conf|Acc])
%     end.
			      


read_file(Dir, FileName, Check, Default) ->
    File = filename:join(Dir, FileName),
    case file:read_file_info(File) of
        {ok, _} ->
            case (catch do_read(File, Check)) of
		{ok, Conf} ->
		    {ok, Conf};
		Error ->
		    ?vtrace("read_file -> read failed:"
			    "~n   Error: ~p", [Error]),
		    Error
	    end;
        {error, Reason} ->
	    ?vinfo("failed reading config from ~s: ~p", [FileName, Reason]),
	    {ok, Default}
    end.

read_file(Dir, FileName, Check) ->
    File = filename:join(Dir, FileName),
    case file:read_file_info(File) of
        {ok, _} ->
            case (catch do_read(File, Check)) of
		{ok, Conf} ->
		    ?vtrace("read_file -> read ok"
			    "~n   Conf: ~p", [Conf]),
		    {ok, Conf};
		Error ->
		    ?vtrace("read_file -> read failed:"
			    "~n   Error: ~p", [Error]),
		    Error
	    end;
        {error, Reason} ->
	    error_msg("failed reading config from ~s: ~p", [FileName, Reason]),
	    {error, {failed_reading, FileName, Reason}}
    end.

do_read(File, Check) ->
    {ok, snmp_conf:read(File, Check)}.


%%--------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call({register_user, UserId, UserMod, UserData}, _From, State) ->
    User  = #user{id = UserId, mod = UserMod, data = UserData},
    Reply = handle_register_user(User),
    {reply, Reply, State};

handle_call({unregister_user, UserId}, _From, State) ->
    Reply = handle_unregister_user(UserId),
    {reply, Reply, State};

handle_call({register_agent, UserId, Addr, Port, Config}, _From, State) ->
    Reply = handle_register_agent(UserId, Addr, Port, Config),
    {reply, Reply, State};

handle_call({unregister_agent, UserId, Addr, Port}, _From, State) ->
    Reply = handle_unregister_agent(UserId, Addr, Port),
    {reply, Reply, State};

handle_call({update_agent_info, UserId, Addr, Port, Item, Val}, 
	    _From, State) ->
    Reply = handle_update_agent_info(UserId, Addr, Port, Item, Val),
    {reply, Reply, State};

handle_call({register_usm_user, User}, _From, State) ->
    Reply = handle_register_usm_user(User),
    {reply, Reply, State};

handle_call({update_usm_user_info, EngineID, UserName, Item, Val}, 
	    _From, State) ->
    Reply = handle_update_usm_user_info(EngineID, UserName, Item, Val),
    {reply, Reply, State};

handle_call({cre_counter, Counter, Initial}, _From, State) ->
    ?vlog("received cre_counter ~p -> ~w", [Counter, Initial]),
    Reply = cre_counter(Counter, Initial),
    {reply, Reply, State};

handle_call({cre_stats_counter, Counter, Initial}, _From, State) ->
    ?vlog("received cre_stats_counter ~p -> ~w", [Counter, Initial]),
    Reply = cre_stats_counter(Counter, Initial),
    {reply, Reply, State};

handle_call({reset_stats_counter, Counter}, _From, State) ->
    ?vlog("received reset_stats_counter ~p", [Counter]),
    Reply = reset_stats_counter(Counter),
    {reply, Reply, State};

handle_call({load_mib, Mib}, _From, State) ->
    ?vlog("received load_mib ~p", [Mib]),
    case handle_load_mib(Mib) of
	ok ->
	    {reply, ok, State};
	Error ->
	    {reply, Error, State}
    end;


handle_call({unload_mib, Mib}, _From, State) ->
    ?vlog("received unload_mib ~p", [Mib]),
    case handle_unload_mib(Mib) of
	ok ->
	    {reply, ok, State};
	Error ->
	    {reply, Error, State}
    end;


handle_call({set_engine_boots, Boots}, _From, State) ->
    ?vlog("received set_engine_boots ~p", [Boots]),
    set_engine_boots(Boots),
    {reply, ok, State};

handle_call({set_engine_time, Time}, _From, State) ->
    ?vlog("received set_engine_time ~p", [Time]),
    Base = snmp_misc:now(sec) - Time,
    ets:insert(snmpm_config_table, {snmp_engine_base, Base}),
    {reply, ok, State};

handle_call({set_usm_cache, Key, Val}, _From, State) ->
    ?vlog("received set_usm_cache: ~w -> ~p", [Key, Val]),
    ets:insert(snmpm_usm_table, {{usm_cache, Key}, Val}),
    {reply, ok, State};


handle_call({verbosity, Verbosity}, _From, State) ->
    ?vlog("received verbosity request", []),
    put(verbosity, Verbosity),
    {reply, ok, State};


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};


handle_call(Req, _From, State) ->
    info_msg("received unknown request: ~n~p", [Req]),
    {reply, {error, unknown_request}, State}.


%%--------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    info_msg("received unknown message: ~n~p", [Msg]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info(Info, State) ->
    info_msg("received unknown info: ~n~p", [Info]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------

code_change(_Vsn, S, _Extra) ->
    {ok, S}.


%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

handle_register_user(User) ->
    ?vdebug("handle_register_user -> entry with"
	    "~n   User: ~p", [User]),
    ets:insert(snmpm_user_table, User),
    ok.

handle_unregister_user(UserId) ->
    ?vdebug("handle_unregister_user -> entry with"
	    "~n   UserId: ~p", [UserId]),
    ets:delete(snmpm_user_table, UserId),
    ok.


handle_register_agent(UserId, Addr, Port, Config) ->
    ?vdebug("handle_register_agent -> entry with"
	    "~n   UserId: ~p"
	    "~n   Addr:   ~p"
	    "~n   Port:   ~p"
	    "~n   Config: ~p", [UserId, Addr, Port, Config]),
    case (catch agent_info(Addr, Port, address)) of
	{error, _} ->
	    {ok, DefConfig} = agent_info(default, default, all),
	    do_handle_register_agent(Addr, Port, DefConfig),
	    do_handle_register_agent(Addr, Port, [{user_id, UserId}|Config]);
	_ ->
	    {error, {already_registered, Addr, Port}}
    end.

do_handle_register_agent(Addr, Port, []) ->
    do_update_agent_info(Addr, Port, address, Addr),
    do_update_agent_info(Addr, Port, port,    Port),
    ok;
do_handle_register_agent(Addr, Port, [{Item, Val}|Rest]) ->
    case (catch do_update_agent_info(Addr, Port, Item, Val)) of
	ok ->
	    do_handle_register_agent(Addr, Port, Rest);
	{error, Reason} ->
	    ets:match_delete(snmpm_agent_table, {Addr, Port, '_'}),
	    {error, Reason}
    end;
do_handle_register_agent(Addr, Port, BadConfig) ->
    error_msg("error during agent registration - bab config: ~n~p", 
	      [BadConfig]),
    ets:match_delete(snmpm_agent_table, {Addr, Port, '_'}),
    {error, {bad_agent_config, Addr, Port, BadConfig}}.


handle_unregister_agent(UserId, Addr, Port) ->
    ?vdebug("handle_unregister_agent -> entry with"
	    "~n   UserId: ~p"
	    "~n   Addr:   ~p"
	    "~n   Port:   ~p", [UserId, Addr, Port]),
    case (catch agent_info(Addr, Port, user_id)) of
	{ok, UserId} ->
	    ets:match_delete(snmpm_agent_table, {{Addr, Port, '_'}, '_'}),
	    ok;
	{ok, OtherUserId} ->
	    {error, {not_owner, OtherUserId}};
	Error ->
	    Error
    end.


handle_update_agent_info(UserId, Addr, Port, Item, Val) ->
    ?vdebug("handle_update_agent_info -> entry with"
	    "~n   UserId: ~p"
	    "~n   Addr:   ~p"
	    "~n   Port:   ~p"
	    "~n   Item:   ~p"
	    "~n   Val:    ~p", [UserId, Addr, Port, Item, Val]),
    case (catch agent_info(Addr, Port, user_id)) of
	{ok, UserId} ->
	    do_update_agent_info(Addr, Port, Item, Val);
	{ok, OtherUserId} ->
	    {error, {not_owner, OtherUserId}};
	Error ->
	    Error
    end.

do_update_agent_info(Addr, Port, Item, Val0) ->
    case verify_val(Item, Val0) of
	{ok, Val} ->
	    ets:insert(snmpm_agent_table, {{Addr, Port, Item}, Val}),
	    ok;
	Error ->
	    ?vlog("do_update_agent_info -> verify value failed: "
		  "~n   Addr:  ~p"
		  "~n   Port:  ~p"
		  "~n   Item:  ~p"
		  "~n   Val0:  ~p"
		  "~n   Error: ~p", [Addr, Port, Item, Val0, Error]),
	    {error, {bad_agent_val, Addr, Port, Item, Val0}}
    end.


handle_register_usm_user(User) ->
    ?vdebug("handle_register_usm_user -> entry with"
	    "~n   User: ~p", [User]),
    #usm_user{engine_id = EngineID, name = Name} = User,
    Key = usm_key(EngineID, Name),
    do_update_usm_user_info(Key, User).

handle_update_usm_user_info(EngineID, Name, Item, Val) ->
    ?vdebug("handle_update_usm_user_info -> entry with"
	    "~n   EngineID: ~p"
	    "~n   Name:     ~p"
	    "~n   Item:     ~p"
	    "~n   Val:      ~p", [EngineID, Name, Item, Val]),
    Key = usm_key(EngineID, Name),
    case ets:lookup(snmpm_usm_table, Key) of
	[] ->
	    {error, not_found};
	[{_Key, User}] ->
	    do_update_usm_user_info(Key, User, Item, Val)
    end.

do_update_usm_user_info(Key, User, sec_name, Val) ->
    case verify_usm_user_sec_name(Val) of
	ok ->
	    do_update_usm_user_info(Key, User#usm_user{sec_name = Val});
	_ ->
	    {error, {invalid_usm_sec_name, Val}}
    end;
do_update_usm_user_info(Key, User, auth, Val) 
  when Val == usmNoAuthProtocol; 
       Val == usmHMACMD5AuthProtocol;
       Val == usmHMACSHAAuthProtocol ->
    do_update_usm_user_info(Key, User#usm_user{auth = Val});
do_update_usm_user_info(_Key, _User, auth, Val) ->
    {error, {invalid_auth_protocol, Val}};
do_update_usm_user_info(Key, 
			#usm_user{auth = usmNoAuthProtocol} = User, 
			auth_key, Val) ->
    case (catch snmp_conf:check_string(Val, any)) of
	ok ->
	    do_update_usm_user_info(Key, User#usm_user{auth_key = Val});
	_ ->
	    {error, {invalid_auth_key, Val}}
    end;
do_update_usm_user_info(Key, 
			#usm_user{auth = usmHMACMD5AuthProtocol} = User, 
			auth_key, Val) 
  when length(Val) == 16 ->
    case is_crypto_supported(md5_mac_96) of
	true -> 
	    do_update_usm_user_info(Key, User#usm_user{auth_key = Val});
	false -> 
	    {error, {unsupported_crypto, md5_mac_96}}
    end;    
do_update_usm_user_info(_Key, 
			#usm_user{auth = usmHMACMD5AuthProtocol}, 
			auth_key, Val) when list(Val) ->
    Len = length(Val),
    {error, {invalid_auth_key_length, usmHMACMD5AuthProtocol, Len}};
do_update_usm_user_info(_Key, 
			#usm_user{auth = usmHMACMD5AuthProtocol}, 
			auth_key, Val) ->
    {error, {invalid_auth_key, usmHMACMD5AuthProtocol, Val}};
do_update_usm_user_info(Key, 
			#usm_user{auth = usmHMACSHAAuthProtocol} = User, 
			auth_key, Val) 
  when length(Val) == 16 ->
    case is_crypto_supported(sha_mac_96) of
	true -> 
	    do_update_usm_user_info(Key, User#usm_user{auth_key = Val});
	false -> 
	    {error, {unsupported_crypto, sha_mac_96}}
    end;    
do_update_usm_user_info(_Key, 
			#usm_user{auth = usmHMACSHAAuthProtocol}, 
			auth_key, Val) when list(Val) ->
    Len = length(Val),
    {error, {invalid_auth_key_length, usmHMACSHAAuthProtocol, Len}};
do_update_usm_user_info(_Key, 
			#usm_user{auth = usmHMACSHAAuthProtocol}, 
			auth_key, Val) ->
    {error, {invalid_auth_key, usmHMACSHAAuthProtocol, Val}};
do_update_usm_user_info(Key, User, priv, Val) 
  when Val == usmNoPrivProtocol; 
       Val == usmDESPrivProtocol ->
    do_update_usm_user_info(Key, User#usm_user{priv = Val});
do_update_usm_user_info(_Key, _User, priv, Val) ->
    {error, {invalid_priv_protocol, Val}};
do_update_usm_user_info(Key, 
			#usm_user{priv = usmNoPrivProtocol} = User, 
			priv_key, Val) ->
    case (catch snmp_conf:check_string(Val, any)) of
	ok ->
	    do_update_usm_user_info(Key, User#usm_user{priv_key = Val});
	_ ->
	    {error, {invalid_priv_key, Val}}
    end;
do_update_usm_user_info(Key, 
			#usm_user{priv = usmDESPrivProtocol} = User, 
			priv_key, Val) 
  when length(Val) == 16 ->
    case is_crypto_supported(des_cbc_decrypt) of
	true -> 
	    do_update_usm_user_info(Key, User#usm_user{priv_key = Val});
	false -> 
	    {error, {unsupported_crypto, des_cbc_decrypt}}
    end;    
do_update_usm_user_info(_Key, 
			#usm_user{auth = usmHMACSHAAuthProtocol}, 
			priv_key, Val) when list(Val) ->
    Len = length(Val),
    {error, {invalid_priv_key_length, usmHMACSHAAuthProtocol, Len}};
do_update_usm_user_info(_Key, 
			#usm_user{auth = usmHMACSHAAuthProtocol}, 
			priv_key, Val) ->
    {error, {invalid_priv_key, usmHMACSHAAuthProtocol, Val}};
do_update_usm_user_info(_Key, _User, Item, Val) ->
    {error, {bad_item, Item, Val}}.

do_update_usm_user_info(Key, User) ->
    ets:insert(snmpm_usm_table, {Key, User}),
    ok.


usm_key(EngineId, Name) ->
    {usmUserTable, EngineId, Name}.


%% ---------------------------------------------------------------------

verify_mandatory(_, []) ->
    ok;
verify_mandatory(Conf, [Mand|Mands]) ->
    case lists:member(Mand, Conf) of
	true ->
	    verify_mandatory(Conf, Mands);
	false ->
	    {error, {missing_mandatory_config, Mand}}
    end.

verify_invalid(_, []) ->
    ok;
verify_invalid(Conf, [Inv|Invs]) ->
    case lists:member(Inv, Conf) of
	false ->
	    verify_mandatory(Conf, Invs);
	true ->
	    {error, {illegal_config, Inv}}
    end.


verify_val(user_id, UserId) ->
    {ok, UserId};
verify_val(address, Ip) when list(Ip) ->
    case (catch snmp_conf:check_ip(Ip)) of
	ok ->
	    {ok, list_to_tuple(Ip)};
	Err ->
	    Err
    end;
verify_val(address, {A1, A2, A3, A4} = Addr) 
  when integer(A1), integer(A2), integer(A3), integer(A4) ->
    {ok, Addr};
verify_val(address, InvalidAddr) ->
    error({bad_address, InvalidAddr});
verify_val(port, Port) ->
    case (catch snmp_conf:check_integer(Port, {gt, 0})) of
	ok ->
	    {ok, Port};
	Err ->
	    Err
    end;
verify_val(target_name, Name) ->
    case (catch snmp_conf:check_string(Name,{gt,0})) of
	ok ->
	    {ok, Name};
	Err ->
	    Err
    end;
verify_val(community, Comm) ->
    case (catch snmp_conf:check_string(Comm)) of
	ok ->
	    {ok, Comm};
	Err ->
	    Err
    end;
verify_val(engine_id, EngineId) ->
    case (catch snmp_conf:check_string(EngineId)) of
	ok ->
	    {ok, EngineId};
	Err ->
	    Err
    end;
verify_val(timeout, Timeout) ->
    (catch snmp_conf:check_timer(Timeout));
verify_val(max_message_size, MMS) ->
    case (catch snmp_conf:check_packet_size(MMS)) of
	ok ->
	    {ok, MMS};
	Err ->
	    Err
    end;
verify_val(version, V) 
  when V == v1; V == v2; V == v3 ->
    {ok, V};
verify_val(version, BadVersion) ->
    error({bad_version, BadVersion});
verify_val(sec_model, Model) ->
    (catch snmp_conf:check_sec_model(Model));
verify_val(sec_name, Name) when list(Name) ->
    case (catch snmp_conf:check_string(Name)) of
	ok ->
	    {ok, Name};
	Err ->
	    Err
    end;
verify_val(sec_name, BadName) ->
    error({bad_sec_name, BadName});
verify_val(sec_level, Level) ->
    (catch snmp_conf:check_sec_level(Level));
verify_val(Item, _) ->
    {error, {no_such_item, Item}}.


%%%-------------------------------------------------------------------
%%%
%%% Mini MIB stuff
%%%
%%%-------------------------------------------------------------------

init_mini_mib(MibFiles) ->
    MiniMibs = lists:flatten([do_load_mib(MibFile) || MibFile <- MibFiles]),
    MiniMIB  = remove_duplicates(lists:keysort(1, MiniMibs), []),
    init_mini_mib2(MiniMIB).

remove_duplicates([], Res) -> 
    Res;
remove_duplicates([X,X|T], Res) -> 
    remove_duplicates([X|T], Res);
remove_duplicates([{Oid, Name, Type, _} = X, {Oid, Name, Type, _}|T], Res) -> 
    remove_duplicates([X|T], Res);
remove_duplicates([X|T], Res) -> 
    remove_duplicates(T, [X|Res]).

init_mini_mib2([]) ->
    ok;
init_mini_mib2([{Oid, Name, Type, MibName}|MiniMib]) ->
    ?vtrace("init mini mib -> ~w: ~w [~w] from ~s", 
	    [Name, Oid, Type,MibName ]),    
    ets:insert(snmpm_mib_table, {{mini_mib, Oid}, Name, Type, MibName}),
    init_mini_mib2(MiniMib).


handle_load_mib(Mib) ->
    [{mibs, Mibs0}] = ets:lookup(snmpm_config_table, mibs),
    case lists:member(Mib, Mibs0) of
	true ->
	    {error, already_loaded};
	false ->
	    Mibs = [Mib|Mibs0],
	    case (catch do_load_mib(Mib)) of
		MiniElems when list(MiniElems) ->
		    ets:insert(snmpm_config_table, {mibs, Mibs}),
		    update_mini_mib(MiniElems),
		    ok;
		Error ->
		    Error
	    end
    end.

update_mini_mib([]) ->
    ok;
update_mini_mib([{Oid, N, Type, MibName}|Elems]) ->
    Key = {mini_mib, Oid},
    case ets:lookup(snmpm_mib_table, Key) of
	[{Key, _N, _Type, _AnotherMibName}] ->
	    %% Already loaded from another mib
	    update_mini_mib(Elems);
	[] ->
	    %% Not yet loaded
	    ets:insert(snmpm_mib_table, {Key, N, Type, MibName}),
	    update_mini_mib(Elems)
    end.


handle_unload_mib(Mib) ->
    Key = {mib, Mib},
    case ets:lookup(snmpm_mib_table, Key) of
	[{Key, MibName}] ->
	    do_unload_mib(MibName),
	    [{mibs, Mibs0}] = ets:lookup(snmpm_config_table, mibs),
	    Mibs = lists:delete(Mib, Mibs0),
	    ets:insert(snmpm_config_table, {mibs, Mibs}),
	    ok;
	_ ->
	    {error, not_loaded}
    end.

do_unload_mib(MibName) ->
    Pat  = {{mini_mib, '$1'}, '_', '_', MibName},
    Oids = ets:match(snmpm_mib_table, Pat),
    F    = fun([Oid]) -> ets:delete(snmpm_mib_table, {mini_mib, Oid}) end,
    lists:foreach(F, Oids).
    

do_load_mib(MibFile) ->
    ?vtrace("load mib ~s", [MibFile]),
    F1 = snmp_misc:strip_extension_from_filename(MibFile, ".bin"),
    ActualFileName = lists:append(F1, ".bin"),
    case snmp_misc:read_mib(ActualFileName) of
        {ok, #mib{name = Name, mes = MEs, traps = Traps}} -> 
	    %% Check that the mib was not loaded or loaded
	    %% with a different filename: 
	    %% e.g. /tmp/MYMIB.bin and /tmp/mibs/MYMIB.bin
	    Pattern = {{mib, '$1'}, Name},
	    case ets:match(snmpm_mib_table, Pattern) of
		[] ->
		    Rec = {{mib, MibFile}, Name}, 
		    ets:insert(snmpm_mib_table, Rec),
		    init_mini_mib_elems(Name, MEs++Traps, []);

		%% This means that the mib has already been loaded
		[[MibFile]] ->
		    [];

		%% This means that the mib was loaded before,
		%% but under another filename
		[[OtherMibFile]] ->
		    error({already_loaded, MibFile, OtherMibFile})
	    end;
		
        {error, Reason} -> 
	    error({failed_reading_mib, MibFile, Reason})
    end.


init_mini_mib_elems(_, [], Res) -> 
    Res;
init_mini_mib_elems(MibName, 
		    [#me{aliasname = N, 
			 oid       = Oid, 
			 entrytype = variable,
			 asn1_type = #asn1_type{bertype = Type}} | T], Res) ->
    init_mini_mib_elems(MibName, T, [{Oid, N, Type, MibName}|Res]);

init_mini_mib_elems(MibName, 
		    [#me{aliasname = N, 
			 oid       = Oid, 
			 entrytype = table_column,
			 asn1_type = #asn1_type{bertype = Type}}|T], Res) ->
    init_mini_mib_elems(MibName, T, [{Oid, N, Type, MibName}|Res]);

init_mini_mib_elems(MibName, 
		    [#me{aliasname = N, 
			 oid       = Oid,
			 asn1_type = undefined}|T], Res) ->
    init_mini_mib_elems(MibName, T, [{Oid, N, undefined, MibName}|Res]);

init_mini_mib_elems(MibName, 
		    [#notification{trapname = N, 
				   oid      = Oid}|T], Res) ->
    init_mini_mib_elems(MibName, T, [{Oid, N, undefined, MibName}|Res]);

init_mini_mib_elems(MibName, [_|T], Res) ->
    init_mini_mib_elems(MibName, T, Res).



%%----------------------------------------------------------------------

normalize_address(Addr) ->
    case inet:getaddr(Addr, inet) of
        {ok, Addr2} ->
            Addr2;
        _ when list(Addr) ->
	    case (catch snmp_conf:check_ip(Addr)) of
		ok ->
		    list_to_tuple(Addr);
		_ ->
		    Addr
	    end;
	_ ->
            Addr
    end.


%%----------------------------------------------------------------------

call(Req) ->
    gen_server:call(?SERVER, Req, infinity).

% cast(Msg) ->
%     gen_server:cast(snmpm_server, Msg).


%%-------------------------------------------------------------------

get_atl_dir(Opts) ->
    get_opt(dir, Opts).

get_atl_type(Opts) ->
    case get_opt(type, Opts, read_write) of
	read_write ->
	    [read,write];
	read ->
	    [read];
	write ->
	    [write]
    end.

get_atl_size(Opts) ->
    get_opt(size, Opts).

get_atl_repair(Opts) ->
    get_opt(repair, Opts, truncate).


%%----------------------------------------------------------------------

get_opt(Key, Opts) ->
    ?d("get option ~w from ~p", [Key, Opts]),
    snmp_misc:get_option(Key, Opts).

get_opt(Key, Opts, Def) ->
    ?d("get option ~w with default ~p from ~p", [Key, Def, Opts]),
    snmp_misc:get_option(Key, Opts, Def).


%%----------------------------------------------------------------------

error(Reason) ->
    throw({error, Reason}).


%%----------------------------------------------------------------------

info_msg(F, A) ->
    (catch error_logger:info_msg("[~p] " ++ F ++ "~n", [?MODULE|A])).

error_msg(F, A) ->
    (catch error_logger:error_msg("[~p] " ++ F ++ "~n", [?MODULE|A])).
