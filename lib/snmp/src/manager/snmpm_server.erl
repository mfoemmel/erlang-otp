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
-module(snmpm_server).

%%----------------------------------------------------------------------
%% This module implements a simple SNMP manager for Erlang.
%%
%% Discovery: broadcast a request for: 
%% 
%%          sysObjectID, sysDescr and sysUpTime
%%
%%----------------------------------------------------------------------

%% User interface
-export([start_link/0, stop/0, 

	 load_mib/1, unload_mib/1, 

	 register_user/3, register_user_monitor/3, unregister_user/1, 

	 sync_get/5, sync_get/6, sync_get/7, 
	 async_get/5, async_get/6, async_get/7, 
	 sync_get_next/5, sync_get_next/6, sync_get_next/7, 
	 async_get_next/5, async_get_next/6, async_get_next/7, 
	 sync_get_bulk/7, sync_get_bulk/8, sync_get_bulk/9, 
	 sync_set/5, sync_set/6, sync_set/7, 
	 async_set/5, async_set/6, async_set/7, 
	 cancel_async_request/2,

	 discovery/2, discovery/3, discovery/4, discovery/5, discovery/6, 

	 reconfigure/0,

	 verbosity/1, verbosity/2 

	]).


%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 code_change/3, terminate/2]).

%% GCT exports
-export([gct_init/1, gct/2]).


-include("snmp_debug.hrl").
-include("snmp_types.hrl").
-include("STANDARD-MIB.hrl").
-include("SNMP-FRAMEWORK-MIB.hrl").
-include("snmp_verbosity.hrl").


%%----------------------------------------------------------------------

-define(SERVER, ?MODULE).

-define(SYNC_GET_TIMEOUT,     5000).
-define(SYNC_SET_TIMEOUT,     5000).
-define(DEFAULT_ASYNC_EXPIRE, 5000).
-define(EXTRA_INFO,           undefined).

-define(SNMP_AGENT_PORT,      161).


-ifdef(snmp_debug).
-define(GS_START_LINK(Args),
	gen_server:start_link({local, ?SERVER}, ?MODULE, Args, 
			      [{debug,[trace]}])).
-else.
-define(GS_START_LINK(Args),
	gen_server:start_link({local, ?SERVER}, ?MODULE, Args, [])).
-endif.



%%----------------------------------------------------------------------

-record(state,
	{parent,
	 gct,
	 note_store,
	 note_store_ref,
	 net_if,
	 net_if_mod,
	 net_if_ref,
	 req,  %%  ???? Last request
	 oid,  %%  ???? Last requested oid
	 mini_mib
	}
       ).

-record(gct, {parent, state = idle, timeout}).

-record(request, 
	{id, 
	 user_id,
	 addr, 
	 port, 
	 type, 
	 data, 
	 ref, 
	 mon, 
	 from,
	 discovery = false, 
	 expire = infinity % When shall the request expire (time in ms)
	}
       ). 

-record(monitor,
	{id, 
	 mon,
	 proc
	}
       ).


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start_link() ->
    ?d("start_link -> entry", []),
    Args = [],
    ?GS_START_LINK(Args).

stop() ->
    call(stop).


load_mib(MibFile) when list(MibFile) ->
    call({load_mib, MibFile}).

unload_mib(Mib) when list(Mib) ->
    call({unload_mib, Mib}).


register_user(UserId, UserMod, UserData) ->
    snmpm_config:register_user(UserId, UserMod, UserData).

register_user_monitor(Id, Module, Data) ->
    case register_user(Id, Module, Data) of
	ok ->
	    case call({monitor_user, Id, self()}) of
		ok ->
		    ok;
		Error ->
		    unregister_user(Id),
		    Error
	    end;
	Error ->
	    Error
    end.

unregister_user(UserId) ->
    call({unregister_user, UserId}).


%% -- [sync] get --

sync_get(UserId, Addr, Port, CtxName, Oids) ->
    sync_get(UserId, Addr, Port, CtxName, Oids, 
	     ?SYNC_GET_TIMEOUT, ?EXTRA_INFO).

sync_get(UserId, Addr, Port, CtxName, Oids, Timeout) ->
    sync_get(UserId, Addr, Port, CtxName, Oids, Timeout, ?EXTRA_INFO).

sync_get(UserId, Addr0, Port, CtxName, Oids, Timeout, ExtraInfo) 
  when integer(Port), list(CtxName), list(Oids), integer(Timeout) ->
    {ok, Addr} = inet:getaddr(Addr0, inet),
    call({sync_get, self(), UserId, Addr, Port, CtxName, Oids, Timeout, 
	  ExtraInfo}).

%% -- [async] get --

async_get(UserId, Addr, Port, CtxName, Oids) ->
    async_get(UserId, Addr, Port, CtxName, Oids, 
	      ?DEFAULT_ASYNC_EXPIRE, ?EXTRA_INFO).

async_get(UserId, Addr, Port, CtxName, Oids, Expire) ->
    async_get(UserId, Addr, Port, CtxName, Oids, Expire, ?EXTRA_INFO).

async_get(UserId, Addr0, Port, CtxName, Oids, Expire, ExtraInfo) 
  when integer(Port), 
       list(CtxName), 
       list(Oids), 
       integer(Expire), Expire >= 0 ->
    {ok, Addr} = inet:getaddr(Addr0, inet),
    call({async_get, self(), UserId, Addr, Port, CtxName, Oids, Expire, 
	  ExtraInfo}).

%% -- [sync] get-next --

sync_get_next(UserId, Addr0, Port, CtxName, Oids) ->
    sync_get_next(UserId, Addr0, Port, CtxName, Oids, ?SYNC_GET_TIMEOUT, 
		  ?EXTRA_INFO).

sync_get_next(UserId, Addr0, Port, CtxName, Oids, Timeout) ->
    sync_get_next(UserId, Addr0, Port, CtxName, Oids, Timeout, ?EXTRA_INFO).

sync_get_next(UserId, Addr0, Port, CtxName, Oids, Timeout, ExtraInfo) 
  when integer(Port), list(CtxName), list(Oids), integer(Timeout) ->
    {ok, Addr} = inet:getaddr(Addr0, inet),
    call({sync_get_next, self(), UserId, Addr, Port, CtxName, Oids, Timeout, 
	  ExtraInfo}).

%% -- [async] get-next --

async_get_next(UserId, Addr, Port, CtxName, Oids) ->
    async_get_next(UserId, Addr, Port, CtxName, Oids, 
		   ?DEFAULT_ASYNC_EXPIRE, ?EXTRA_INFO).

async_get_next(UserId, Addr, Port, CtxName, Oids, Expire) ->
    async_get_next(UserId, Addr, Port, CtxName, Oids, Expire, ?EXTRA_INFO).

async_get_next(UserId, Addr0, Port, CtxName, Oids, Expire, ExtraInfo) 
  when integer(Port), 
       list(CtxName), 
       list(Oids), 
       integer(Expire), Expire >= 0 ->
    {ok, Addr} = inet:getaddr(Addr0, inet),
    call({async_get_next, self(), UserId, Addr, Port, CtxName, Oids, 
	  Expire, ExtraInfo}).

%% -- [sync] get-bulk --

sync_get_bulk(UserId, Addr, Port, NonRep, MaxRep, CtxName, Oids) ->
    sync_get_bulk(UserId, Addr, Port, 
		  NonRep, MaxRep, CtxName, Oids, 
		  ?SYNC_GET_TIMEOUT, ?EXTRA_INFO).

sync_get_bulk(UserId, Addr, Port, NonRep, MaxRep, CtxName, Oids, Timeout) ->
    sync_get_bulk(UserId, Addr, Port, 
		  NonRep, MaxRep, CtxName, Oids, 
		  Timeout, ?EXTRA_INFO).

sync_get_bulk(UserId, Addr0, Port, NonRep, MaxRep, CtxName, Oids, Timeout, ExtraInfo) 
  when integer(Port), 
       integer(NonRep), integer(MaxRep), 
       list(CtxName), list(Oids), integer(Timeout) ->
    {ok, Addr} = inet:getaddr(Addr0, inet),
    call({sync_get_bulk, self(), UserId, Addr, Port, 
	  NonRep, MaxRep, CtxName, Oids, Timeout, ExtraInfo}).

%% -- [sync] set --

%% VarsAndValues is: {PlainOid, o|s|i, Value} (unknown mibs) | {Oid, Value} 
sync_set(UserId, Addr0, Port, CtxName, VarsAndVals) ->
    sync_set(UserId, Addr0, Port, CtxName, VarsAndVals, 
	     ?SYNC_SET_TIMEOUT, ?EXTRA_INFO).

sync_set(UserId, Addr0, Port, CtxName, VarsAndVals, Timeout) ->
    sync_set(UserId, Addr0, Port, CtxName, VarsAndVals, 
	     Timeout, ?EXTRA_INFO).

sync_set(UserId, Addr0, Port, CtxName, VarsAndVals, Timeout, ExtraInfo) 
  when integer(Port), list(CtxName), list(VarsAndVals), integer(Timeout) ->
    {ok, Addr} = inet:getaddr(Addr0, inet),
    call({sync_set, self(), UserId, Addr, Port, 
	  CtxName, VarsAndVals, Timeout, ExtraInfo}).

%% -- [async] set --

async_set(UserId, Addr, Port, CtxName, VarsAndVals) ->
    async_set(UserId, Addr, Port, CtxName, VarsAndVals, 
	      ?DEFAULT_ASYNC_EXPIRE, ?EXTRA_INFO).

async_set(UserId, Addr, Port, CtxName, VarsAndVals, Expire) ->
    async_set(UserId, Addr, Port, CtxName, VarsAndVals, 
	      Expire, ?EXTRA_INFO).

async_set(UserId, Addr0, Port, CtxName, VarsAndVals, Expire, ExtraInfo) 
  when integer(Port), 
       list(CtxName), 
       list(VarsAndVals), 
       integer(Expire), Expire >= 0 ->
    {ok, Addr} = inet:getaddr(Addr0, inet),
    call({async_set, self(), UserId, Addr, Port, 
	  CtxName, VarsAndVals, Expire, ExtraInfo}).


cancel_async_request(UserId, ReqId) ->
    call({cancel_async_request, UserId, ReqId}).


discovery(UserId, BAddr) ->
    discovery(UserId, BAddr, ?SNMP_AGENT_PORT, [], 
	      ?DEFAULT_ASYNC_EXPIRE, ?EXTRA_INFO).

discovery(UserId, BAddr, Config) when is_list(Config) ->
    discovery(UserId, BAddr, ?SNMP_AGENT_PORT, Config, 
	      ?DEFAULT_ASYNC_EXPIRE, ?EXTRA_INFO);

discovery(UserId, BAddr, Expire) when is_integer(Expire) ->
    discovery(UserId, BAddr, ?SNMP_AGENT_PORT, [], Expire, ?EXTRA_INFO).

discovery(UserId, BAddr, Config, Expire) ->
    discovery(UserId, BAddr, ?SNMP_AGENT_PORT, Config, Expire, ?EXTRA_INFO).

discovery(UserId, BAddr, Port, Config, Expire) ->
    discovery(UserId, BAddr, Port, Config, Expire, ?EXTRA_INFO).

discovery(UserId, BAddr, Port, Config, Expire, ExtraInfo) ->
    call({discovery, self(), UserId, BAddr, Port, Config, Expire, ExtraInfo}).

    
verbosity(Verbosity) ->
    case ?vvalidate(Verbosity) of
	Verbosity ->
	    call({verbosity, Verbosity});
	_ ->
	    {error, {invalid_verbosity, Verbosity}}
    end.

verbosity(net_if = Ref, Verbosity) ->
    verbosity2(Ref, Verbosity);
verbosity(note_store = Ref, Verbosity) ->
    verbosity2(Ref, Verbosity).

verbosity2(Ref, Verbosity) ->
    case ?vvalidate(Verbosity) of
	Verbosity ->
	    call({verbosity, Ref, Verbosity});
	_ ->
	    {error, {invalid_verbosity, Verbosity}}
    end.

reconfigure() ->
    call(reconfigure).


%%----------------------------------------------------------------------
%% Options: List of
%%  {community, String ("public" is default} 
%%  {mibs, List of Filenames}
%%  {trap_udp, integer() (default 5000)}
%%  {conf_dir, string()}
%%  {log_dir,  string()}
%%  {db_dir,   string()}
%%  {db_repair, true | false}
%%----------------------------------------------------------------------
init(_) ->
    ?d("init -> entry", []),
    case (catch do_init()) of
	{ok, State} ->
	    {ok, State};
	{error, Reason} ->
	    {stop, Reason}
    end.


%% Put all config stuff in a snmpm_config module/process.
%% Tables should be protected so that it is cheap to 
%% read. Writing has to go through the interface...

do_init() ->
    process_flag(trap_exit, true),
    {ok, Prio} = snmpm_config:system_info(prio),
    process_flag(priority, Prio),

    {ok, Verbosity} = snmpm_config:system_info(server_verbosity),
    put(sname, mse),
    put(verbosity, Verbosity),
    ?vlog("starting", []),

    %% Start the garbage collector timer process
    {ok, Timeout} = snmpm_config:system_info(server_timeout),
    {ok, GCT} = gct_start(Timeout),

    %% -- Create request table --
    ets:new(snmpm_request_table, 
	    [set, protected, named_table, {keypos, #request.id}]),

    %% -- Create monitor table --
    ets:new(snmpm_monitor_table, 
	    [set, protected, named_table, {keypos, #monitor.id}]),

    %% -- Start the note-store and net-if processes --
    {NoteStore, NoteStoreRef} = do_init_note_store(Prio),
    {NetIf, NetIfModule, NetIfRef} = do_init_net_if(NoteStore),

    State = #state{gct            = GCT,
		   note_store     = NoteStore,
		   note_store_ref = NoteStoreRef,
		   net_if         = NetIf,
		   net_if_mod     = NetIfModule,
		   net_if_ref     = NetIfRef},
    ?vlog("started", []),
    {ok, State}.


do_init_note_store(Prio) ->
    ?vdebug("try start note store", []),
    {ok, Verbosity} = snmpm_config:system_info(note_store_verbosity),
    {ok, Timeout}   = snmpm_config:system_info(note_store_timeout),
    Opts = [{sname,     mns}, 
	    {verbosity, Verbosity}, 
	    {timeout,   Timeout}],
    case snmpm_misc_sup:start_note_store(Prio, Opts) of
	{ok, Pid} ->
	    ?vtrace("do_init_note_store -> Pid: ~p", [Pid]),
	    Ref = erlang:monitor(process, Pid),
	    {Pid, Ref};
	{error, Reason} ->
	    ?vlog("failed starting note-store - Reason: "
		  "~n", [Reason]),
	    throw({error, {failed_starting_note_store, Reason}})
    end.

do_init_net_if(NoteStore) ->
    ?vdebug("try start net if", []),
    {ok, NetIfModule} = snmpm_config:system_info(net_if_module),
    case snmpm_misc_sup:start_net_if(NetIfModule, NoteStore) of
	{ok, Pid} ->
	    ?vtrace("do_init_net_if -> Pid: ~p", [Pid]),
	    Ref = erlang:monitor(process, Pid),
	    {Pid, NetIfModule, Ref};
	{error, Reason} ->
	    ?vlog("failed starting net-if - Reason: "
		  "~n", [Reason]),
	    throw({error, {failed_starting_net_if, Reason}})
    end.

%% ---------------------------------------------------------------------
%% ---------------------------------------------------------------------

handle_call({monitor_user, Id, Pid}, _From, State) when pid(Pid) ->
    ?vlog("received monitor_user request for ~w [~w]", [Id, Pid]),
    Reply = 
	case ets:lookup(snmpm_monitor_table, Id) of
	    [#monitor{proc = Pid}] ->
		?vdebug("already monitored", []),
		ok;

	    [#monitor{proc = OtherPid}] ->
		?vinfo("already registered to ~w", [OtherPid]),
		{error, {already_monitored, OtherPid}};

	    [] ->
		Ref = erlang:monitor(process, Pid),
		?vtrace("monitor ref: ~w", [Ref]),
		Mon = #monitor{id = Id, mon = Ref, proc = Pid},
		ets:insert(snmpm_monitor_table, Mon),
		ok
	end,
    {reply, Reply, State};

handle_call({unregister_user, UserId}, _From, State) ->
    ?vlog("received request to unregister user ~p", [UserId]),

    %% 1) If this user is monitored, then demonitor
    case ets:lookup(snmpm_monitor_table, UserId) of
	[] ->
	    ok;
	[#monitor{mon = M}] ->
	    maybe_demonitor(M), % This is really overkill (meybe_), but...
	    ok
    end,

    %% 2) Delete all outstanding requests from this user
    Pat = #request{user_id = UserId, 
		   id = '$1', ref = '$2', mon = '$3', _ = '_'},
    Match = ets:match(snmpm_request_table, Pat),
    F1 = fun([ReqId, Ref, MonRef]) -> 
		 ets:delete(snmpm_request_table, ReqId),
		 cancel_timer(Ref),
		 maybe_demonitor(MonRef),
		 ok
	end,
    lists:foreach(F1, Match),
    
    %% 3) Unregister all agents registered by this user
    Agents = snmpm_config:which_agents(UserId),
    F2 = fun({Addr, Port}) ->
		 snmpm_config:unregister_agent(UserId, Addr, Port)
	 end,
    lists:foreach(F2, Agents),

    %% 4) Unregister the user
    Reply = snmpm_config:unregister_user(UserId),
    {reply, Reply, State};


%% We will reply to this request later, when the reply comes in from the
%% agent, or when the timeout hits (unless we get an error now).
handle_call({sync_get, Pid, UserId, Addr, Port, CtxName, Oids, Timeout, ExtraInfo}, From, State) ->
    ?vlog("received sync_get [~p] request", [CtxName]),
    case (catch handle_sync_get(Pid, 
				UserId, Addr, Port, CtxName, Oids, 
				Timeout, ExtraInfo, From, State)) of
	ok ->
	    {noreply, State};
	Error ->
	    {reply, Error, State}
    end;


handle_call({sync_get_next, Pid, UserId, Addr, Port, CtxName, Oids, Timeout, ExtraInfo}, From, State) ->
    ?vlog("received sync_get_next [~p] request", [CtxName]),
    case (catch handle_sync_get_next(Pid, 
				     UserId, Addr, Port, CtxName, Oids, 
				     Timeout, ExtraInfo, From, State)) of
	ok ->
	    {noreply, State};
	Error ->
	    {reply, Error, State}
    end;


%% Check agent version? This op not in v1
handle_call({sync_get_bulk, _Pid, _UserId, _Addr, _Port, 
	     _NonRep, _MaxRep, _CtxName, _Oids, _Timeout, _ExtraInfo}, 
	    _From, State) ->
    ?vlog("received sync_get_bulk [~p] request", [_CtxName]),
    Reply = {error, {not_supported, sync_get_bulk}},
    {reply, Reply, State};
%     case (catch handle_sync_get_bulk(Pid, 
% 				     UserId, Addr, Port, NonRep, MaxRep, Oids, 
% 				     Timeout, ExtraInfo, From, State)) of
% 	ok ->
% 	    {noreply, State};
% 	Error ->
% 	    {reply, Error, State}
%     end;


handle_call({sync_set, Pid, UserId, Addr, Port, CtxName, VarsAndVals, Timeout, ExtraInfo}, From, State) ->
    ?vlog("received sync_set [~p] request", [CtxName]),
    case (catch handle_sync_set(Pid, 
				UserId, Addr, Port, CtxName, VarsAndVals, 
				Timeout, ExtraInfo, From, State)) of
	ok ->
	    {noreply, State};
	Error ->
	    {reply, Error, State}
    end;


handle_call({async_get, Pid, UserId, Addr, Port, CtxName, Oids, Expire, ExtraInfo}, 
	    _From, State) ->
    ?vlog("received async_get [~p] request", [CtxName]),
    Reply = (catch handle_async_get(Pid, UserId, Addr, Port, CtxName, Oids, 
				    Expire, ExtraInfo, State)),
    {reply, Reply, State};


handle_call({async_get_next, Pid, UserId, Addr, Port, CtxName, Oids, Expire, ExtraInfo}, 
	    _From, State) ->
    ?vlog("received async_get_next [~p] request", [CtxName]),
    Reply = (catch handle_async_get_next(Pid, UserId, Addr, Port, CtxName, 
					 Oids, Expire, ExtraInfo, State)),
    {reply, Reply, State};


handle_call({async_set, Pid, UserId, Addr, Port, CtxName, VarsAndVals, Expire, ExtraInfo}, 
	    _From, State) ->
    ?vlog("received async_set [~p] request", [CtxName]),
    Reply = (catch handle_async_set(Pid, UserId, Addr, Port, CtxName, 
				    VarsAndVals, Expire, ExtraInfo, State)),
    {reply, Reply, State};


handle_call({cancel_async_request, UserId, ReqId}, _From, State) ->
    ?vlog("received cancel_async_request request", []),
    Reply = (catch handle_cancel_async_request(UserId, ReqId, State)),
    {reply, Reply, State};


handle_call({discovery, Pid, UserId, BAddr, Port, Config, Expire, ExtraInfo}, 
	    _From, State) ->
    ?vlog("received discovery request", []),
    Reply = (catch handle_discovery(Pid, UserId, BAddr, Port, Config, 
				    Expire, ExtraInfo, State)),
    {reply, Reply, State};


handle_call({load_mib, Mib}, _From, State) ->
    ?vlog("received load_mib request", []),
    case snmpm_config:load_mib(Mib) of
	ok ->
	    MiniMIB = snmpm_config:make_mini_mib(),
	    {reply, ok, State#state{mini_mib = MiniMIB}};
	Error ->
	    {reply, Error, State}
    end;


handle_call({unload_mib, Mib}, _From, State) ->
    ?vlog("received unload_mib request", []),
    case snmpm_config:unload_mib(Mib) of
	ok ->
	    MiniMIB = snmpm_config:make_mini_mib(),
	    {reply, ok, State#state{mini_mib = MiniMIB}};
	Error ->
	    {reply, Error, State}
    end;

handle_call({verbosity, Verbosity}, _From, State) ->
    ?vlog("received verbosity request", []),
    put(verbosity, Verbosity),
    {reply, ok, State};

handle_call({verbosity, net_if, Verbosity}, _From, 
	    #state{net_if = Pid, net_if_mod = Mod} = State) ->
    ?vlog("received net_if verbosity request", []),
    Mod:verbosity(Pid, Verbosity),
    {reply, ok, State};

handle_call({verbosity, note_store, Verbosity}, _From, 
	    #state{note_store = Pid} = State) ->
    ?vlog("received note_store verbosity request", []),
    snmp_note_store:verbosity(Pid, Verbosity),
    {reply, ok, State};

handle_call(reconfigure, _From, State) ->
    ?vlog("received reconfigure request", []),
    Reply = {error, not_implemented},
    {reply, Reply, State};

handle_call(stop, _From, State) ->
    ?vlog("received stop request", []),
    {stop, normal, ok, State};


handle_call(Req, _From, State) ->
    info_msg("received unknown request: ~n~p", [Req]),
    {reply, {error, unknown_request}, State}.


handle_cast(Msg, State) ->
    info_msg("received unknown message: ~n~p", [Msg]),
    {noreply, State}.


handle_info({sync_timeout, ReqId, From}, State) ->
    ?vlog("received sync_timeout [~w] message", [ReqId]),
    handle_sync_timeout(ReqId, From, State),
    {noreply, State};


handle_info({snmp_error, Pdu, Reason}, State) ->
    ?vlog("received snmp_error message", []),
    handle_snmp_error(Pdu, Reason, State),
    {noreply, State};

handle_info({snmp_error, Reason, Addr, Port}, State) ->
    ?vlog("received snmp_error message", []),
    handle_snmp_error(Addr, Port, -1, Reason, State),
    {noreply, State};

handle_info({snmp_error, ReqId, Reason, Addr, Port}, State) ->
    ?vlog("received snmp_error message", []),
    handle_snmp_error(Addr, Port, ReqId, Reason, State),
    {noreply, State};

%% handle_info({snmp_error, ReqId, Pdu, Reason, Addr, Port}, State) ->
%%     ?vlog("received snmp_error message", []),
%%     handle_snmp_error(Pdu, ReqId, Reason, Addr, Port, State),
%%     {noreply, State};


handle_info({snmp_pdu, Pdu, Addr, Port}, State) ->
    ?vlog("received snmp_pdu message", []),
    handle_snmp_pdu(Pdu, Addr, Port, State),
    {noreply, State};


handle_info({snmp_trap, Trap, Addr, Port}, State) ->
    ?vlog("received snmp_trap message", []),
    handle_snmp_trap(Trap, Addr, Port, State),
    {noreply, State};


handle_info({snmp_inform, Pdu, Addr, Port}, State) ->
    ?vlog("received snmp_inform message", []),
    handle_snmp_inform(Pdu, Addr, Port, State),
    {noreply, State};


handle_info({snmp_report, {ok, Pdu}, Addr, Port}, State) ->
    handle_snmp_report(Pdu, Addr, Port, State),
    {noreply, State};

handle_info({snmp_report, {error, ReqId, Info, Pdu}, Addr, Port}, State) ->
    handle_snmp_report(ReqId, Pdu, Info, Addr, Port, State),
    {noreply, State};


handle_info(gc_timeout, #state{gct = GCT} = State) ->
    ?vlog("received gc_timeout message", []),
    handle_gc(GCT),
    {noreply, State};


handle_info({'DOWN', _MonRef, process, Pid, _Reason}, 
	    #state{note_store = NoteStore, 
		   net_if     = Pid} = State) ->
    ?vlog("received 'DOWN' message regarding net_if", []),
    {NetIf, _, Ref} = do_init_net_if(NoteStore),
    {noreply, State#state{net_if = NetIf, net_if_ref = Ref}};


handle_info({'DOWN', _MonRef, process, Pid, _Reason}, 
	    #state{note_store = Pid, 
		   net_if     = NetIf,
		   net_if_mod = Mod} = State) ->
    ?vlog("received 'DOWN' message regarding note_store", []),
    {ok, Prio} = snmpm_config:system_info(prio),
    {NoteStore, Ref} = do_init_note_store(Prio),
    Mod:note_store(NetIf, NoteStore),
    {noreply, State#state{note_store = NoteStore, note_store_ref = Ref}};


handle_info({'DOWN', MonRef, process, Pid, Reason}, State) ->
    ?vlog("received 'DOWN' message (~w) from ~w "
	  "~n   Reason: ~p", [MonRef, Pid, Reason]),
    handle_down(MonRef),
    {noreply, State};


handle_info({'EXIT', Pid, Reason}, #state{gct = Pid} = State) ->
    ?vlog("received 'EXIT' message from the GCT (~w) process: "
	  "~n   ~p", [Pid, Reason]),
    {ok, Timeout} = snmpm_config:system_info(server_timeout),
    {ok, GCT} = gct_start(Timeout),
    {noreply, State#state{gct = GCT}};


handle_info(Info, State) ->
    info_msg("received unknown info: ~n~p", [Info]),
    {noreply, State}.


%%----------------------------------------------------------
%% Code change
%%----------------------------------------------------------
                                                                              
% downgrade
code_change({down, _Vsn}, #state{gct = GCT} = State, downgrade_to_404) ->
    ?d("code_change(down) -> entry (~w)", [GCT]),
    gct_code_change(GCT),
    handle_downgrade(),
    ?d("code_change(down) -> done", []),
    {ok, State};
 
% upgrade
code_change(_Vsn, #state{gct = GCT} = State, upgrade_from_404) ->
    ?d("code_change(up) -> entry (~w)", [GCT]),
    gct_code_change(GCT),
    handle_upgrade(),
    ?d("code_change(up) -> done", []),
    {ok, State};

code_change(_Vsn, #state{gct = GCT} = State, _Extra) ->
    ?d("code_change -> entry [just give GCT a kick]", []),
    gct_code_change(GCT),
    {ok, State}.
 
 
handle_downgrade() ->
    First = ets:first(snmpm_monitor_table),
    handle_downgrade(First).

handle_downgrade('$end_of_table') ->
    %% All monitored users demonitored, now delete the table:
    ets:delete(snmpm_monitor_table);
handle_downgrade(Id) ->
    Next = 
	case ets:lookup(snmpm_monitor_table, Id) of
	    [#monitor{mon = Ref}] ->
		maybe_demonitor(Ref),% This is really overkill (meybe_), but...
		ets:next(snmpm_monitor_table, Id);
	    _ ->
		%% Ouch, somebody else messing with the table...
		%% fake a complete, and let the bug bang...
		'$end_of_table'
	end,
    handle_downgrade(Next).

handle_upgrade() ->
    ets:new(snmpm_monitor_table, 
	    [set, protected, named_table, {keypos, #monitor.id}]).

	    
%%----------------------------------------------------------
%% Terminate
%%----------------------------------------------------------
                                                                              
terminate(Reason, #state{gct = GCT}) ->
    ?vdebug("terminate: ~p",[Reason]),
    gct_stop(GCT),
    snmpm_misc_sup:stop_note_store(),
    snmpm_misc_sup:stop_net_if(),
    ok.


%%----------------------------------------------------------------------
%% 
%%----------------------------------------------------------------------

handle_sync_get(Pid, UserId, Addr, Port, CtxName, Oids, Timeout, ExtraInfo, 
		From, State) ->    
    ?vtrace("handle_sync_get -> entry with"
	    "~n   Pid:     ~p"
	    "~n   UserId:  ~p"
	    "~n   Addr:    ~p"
	    "~n   Port:    ~p"
	    "~n   CtxName: ~p"
	    "~n   Oids:    ~p"
	    "~n   Timeout: ~p"
	    "~n   From:    ~p", 
	    [Pid, UserId, Addr, Port, CtxName, Oids, Timeout, From]),
    case agent_data(Addr, Port, CtxName) of
	{ok, Vsn, MsgData} ->
	    ?vtrace("handle_sync_get -> send a ~p message", [Vsn]),
	    ReqId  = send_get_request(Oids, Vsn, MsgData, Addr, Port, 
				      ExtraInfo, State),
	    ?vdebug("handle_sync_get -> ReqId: ~p", [ReqId]),
	    Msg    = {sync_timeout, ReqId, From},
	    Ref    = erlang:send_after(Timeout, self(), Msg),
	    MonRef = erlang:monitor(process, Pid),
	    ?vtrace("handle_sync_get -> MonRef: ~p", [MonRef]),
	    Req    = #request{id      = ReqId,
			      user_id = UserId, 
			      addr    = Addr,
			      port    = Port,
			      type    = get, 
			      data    = MsgData, 
			      ref     = Ref, 
			      mon     = MonRef, 
			      from    = From},
	    ets:insert(snmpm_request_table, Req),
	    ok;
	Error ->
	    ?vinfo("failed retrieving agent data for get:"
		   "~n   Addr:  ~p"
		   "~n   Port:  ~p"
		   "~n   Error: ~p", [Addr, Port, Error]),
	    Error
    end.
    

handle_sync_get_next(Pid, UserId, Addr, Port, CtxName, Oids, Timeout, 
		     ExtraInfo, From, State) ->
    ?vtrace("handle_sync_get_next -> entry with"
	    "~n   Pid:     ~p"
	    "~n   UserId:  ~p"
	    "~n   Addr:    ~p"
	    "~n   Port:    ~p"
	    "~n   CtxName: ~p"
	    "~n   Oids:    ~p"
	    "~n   Timeout: ~p"
	    "~n   From:    ~p", 
	    [Pid, UserId, Addr, Port, CtxName, Oids, Timeout, From]),
    case agent_data(Addr, Port, CtxName) of
	{ok, Vsn, MsgData} ->
	    ?vtrace("handle_sync_get_next -> send a ~p message", [Vsn]),
	    ReqId  = send_get_next_request(Oids, Vsn, MsgData, 
					   Addr, Port, ExtraInfo, State),
	    ?vdebug("handle_sync_get_next -> ReqId: ~p", [ReqId]),
	    Msg    = {sync_timeout, ReqId, From},
	    Ref    = erlang:send_after(Timeout, self(), Msg),
	    MonRef = erlang:monitor(process, Pid),
	    ?vtrace("handle_sync_get_next -> MonRef: ~p", [MonRef]),
	    Req    = #request{id      = ReqId,
			      user_id = UserId, 
			      addr    = Addr,
			      port    = Port,
			      type    = get_next, 
			      data    = MsgData, 
			      ref     = Ref, 
			      mon     = MonRef, 
			      from    = From},
	    ets:insert(snmpm_request_table, Req),
	    ok;

	Error ->
	    ?vinfo("failed retrieving agent data for get-next:"
		   "~n   Addr:  ~p"
		   "~n   Port:  ~p"
		   "~n   Error: ~p", [Addr, Port, Error]),
	    Error
    end.
    

handle_sync_set(Pid, UserId, Addr, Port, CtxName, VarsAndVals, Timeout, 
		ExtraInfo, From, State) ->
    ?vtrace("handle_sync_set -> entry with"
	    "~n   Pid:         ~p"
	    "~n   UserId:      ~p"
	    "~n   Addr:        ~p"
	    "~n   Port:        ~p"
	    "~n   CtxName:     ~p"
	    "~n   VarsAndVals: ~p"
	    "~n   Timeout:     ~p"
	    "~n   From:        ~p", 
	    [Pid, UserId, Addr, Port, CtxName, VarsAndVals, Timeout, From]),
    case agent_data(Addr, Port, CtxName) of
	{ok, Vsn, MsgData} ->
	    ?vtrace("handle_sync_set -> send a ~p message", [Vsn]),
	    ReqId  = send_set_request(VarsAndVals, Vsn, MsgData, 
				      Addr, Port, ExtraInfo, State),
	    ?vdebug("handle_sync_set -> ReqId: ~p", [ReqId]),
	    Msg    = {sync_timeout, ReqId, From},
	    Ref    = erlang:send_after(Timeout, self(), Msg),
            MonRef = erlang:monitor(process, Pid),
	    ?vtrace("handle_sync_set -> MonRef: ~p", [MonRef]),
	    Req    = #request{id      = ReqId,
			      user_id = UserId, 
			      addr    = Addr,
			      port    = Port,
			      type    = set, 
			      data    = MsgData, 
			      ref     = Ref, 
			      mon     = MonRef, 
			      from    = From},
	    ets:insert(snmpm_request_table, Req),
	    ok;

	Error ->
	    ?vinfo("failed retrieving agent data for set:"
		   "~n   Addr:  ~p"
		   "~n   Port:  ~p"
		   "~n   Error: ~p", [Addr, Port, Error]),
	    Error
    end.

 
handle_async_get(Pid, UserId, Addr, Port, CtxName, Oids, Expire, ExtraInfo, 
		 State) ->
    ?vtrace("handle_async_get -> entry with"
	    "~n   Pid:     ~p"
	    "~n   UserId:  ~p"
	    "~n   Addr:    ~p"
	    "~n   Port:    ~p"
	    "~n   CtxName: ~p"
	    "~n   Oids:    ~p"
	    "~n   Expire:  ~p",
	    [Pid, UserId, Addr, Port, CtxName, Oids, Expire]),
    case agent_data(Addr, Port, CtxName) of
	{ok, Vsn, MsgData} ->
	    ?vtrace("handle_async_get -> send a ~p message", [Vsn]),
	    ReqId  = send_get_request(Oids, Vsn, MsgData, Addr, Port, 
				      ExtraInfo, State),
	    ?vdebug("handle_async_get -> ReqId: ~p", [ReqId]),
	    Req    = #request{id      = ReqId,
			      user_id = UserId, 
			      addr    = Addr,
			      port    = Port,
			      type    = get, 
			      data    = MsgData, 
			      expire  = t() + Expire},

	    ets:insert(snmpm_request_table, Req),
	    gct_activate(State#state.gct),
	    {ok, ReqId};

	Error ->
	    ?vinfo("failed retrieving agent data for get:"
		   "~n   Addr:  ~p"
		   "~n   Port:  ~p"
		   "~n   Error: ~p", [Addr, Port, Error]),
	    Error
    end.


handle_async_get_next(Pid, UserId, Addr, Port, CtxName, Oids, Expire, 
		      ExtraInfo, State) ->
    ?vtrace("handle_async_get_next -> entry with"
	    "~n   Pid:     ~p"
	    "~n   UserId:  ~p"
	    "~n   Addr:    ~p"
	    "~n   Port:    ~p"
	    "~n   CtxName: ~p"
	    "~n   Oids:    ~p"
	    "~n   Expire:  ~p",
	    [Pid, UserId, Addr, Port, CtxName, Oids, Expire]),
    case agent_data(Addr, Port, CtxName) of
	{ok, Vsn, MsgData} ->
	    ?vtrace("handle_async_get_next -> send a ~p message", [Vsn]),
	    ReqId  = send_get_next_request(Oids, Vsn, MsgData, 
					   Addr, Port, ExtraInfo, State),
	    ?vdebug("handle_async_get_next -> ReqId: ~p", [ReqId]),
	    Req    = #request{id      = ReqId,
			      user_id = UserId, 
			      addr    = Addr,
			      port    = Port,
			      type    = get_next, 
			      data    = MsgData, 
			      expire  = t() + Expire},

	    ets:insert(snmpm_request_table, Req),
	    gct_activate(State#state.gct),
	    {ok, ReqId};

	Error ->
	    ?vinfo("failed retrieving agent data for get-next:"
		   "~n   Addr:  ~p"
		   "~n   Port:  ~p"
		   "~n   Error: ~p", [Addr, Port, Error]),
	    Error
    end.


handle_async_set(Pid, UserId, Addr, Port, CtxName, VarsAndVals, Expire, 
		 ExtraInfo, State) ->
    ?vtrace("handle_async_set -> entry with"
	    "~n   Pid:         ~p"
	    "~n   UserId:      ~p"
	    "~n   Addr:        ~p"
	    "~n   Port:        ~p"
	    "~n   CtxName:     ~p"
	    "~n   VarsAndVals: ~p"
	    "~n   Expire:      ~p",
	    [Pid, UserId, Addr, Port, CtxName, VarsAndVals, Expire]),
    case agent_data(Addr, Port, CtxName) of
	{ok, Vsn, MsgData} ->
	    ?vtrace("handle_async_set -> send a ~p message", [Vsn]),
	    ReqId  = send_set_request(VarsAndVals, Vsn, MsgData, 
				      Addr, Port, ExtraInfo, State),
	    ?vdebug("handle_async_set -> ReqId: ~p", [ReqId]),
	    Req    = #request{id      = ReqId,
			      user_id = UserId, 
			      addr    = Addr,
			      port    = Port,
			      type    = set, 
			      data    = MsgData, 
			      expire  = t() + Expire},

	    ets:insert(snmpm_request_table, Req),
	    gct_activate(State#state.gct),
	    {ok, ReqId};

	Error ->
	    ?vinfo("failed retrieving agent data for set:"
		   "~n   Addr:  ~p"
		   "~n   Port:  ~p"
		   "~n   Error: ~p", [Addr, Port, Error]),
	    Error
    end.


handle_cancel_async_request(UserId, ReqId, _State) ->
    ?vtrace("handle_cancel_async_request -> entry with"
	    "~n   UserId: ~p"
	    "~n   ReqId:  ~p", [UserId, ReqId]),
    case ets:lookup(snmpm_request_table, ReqId) of
	[#request{user_id = UserId,
		  ref     = Ref}] ->
	    ?vdebug("handle_cancel_async_request -> demonitor and cancel timer"
		    "~n   Ref: ~p", [Ref]),
	    cancel_timer(Ref),
	    ets:delete(snmpm_request_table, ReqId),
	    ok;
	
	[#request{user_id = OtherUserId}] ->
	    ?vinfo("handle_cancel_async_request -> Not request owner"
		    "~n   OtherUserId: ~p", [OtherUserId]),
	    {error, {not_owner, OtherUserId}};
	
	[] ->
	    ?vlog("handle_cancel_async_request -> not found", []),
	    {error, not_found}
    end.
    

handle_discovery(Pid, UserId, BAddr, Port, Config, Expire, ExtraInfo, State) ->
    ?vtrace("handle_discovery -> entry with"
	    "~n   Pid:         ~p"
	    "~n   UserId:      ~p"
	    "~n   BAddr:       ~p"
	    "~n   Port:        ~p"
	    "~n   Config:      ~p"
	    "~n   Expire:      ~p",
	    [Pid, UserId, BAddr, Port, Config, Expire]),
    case agent_data(default, default, "", Config) of
	{ok, Vsn, MsgData} ->
	    ?vtrace("handle_discovery -> send a ~p disco message", [Vsn]),
	    ReqId  = send_discovery(Vsn, MsgData, BAddr, Port, ExtraInfo, 
				    State),
	    ?vdebug("handle_discovery -> ReqId: ~p", [ReqId]),
	    MonRef = erlang:monitor(process, Pid),
	    ?vtrace("handle_discovery -> MonRef: ~p", [MonRef]),
	    Req    = #request{id        = ReqId,
			      user_id   = UserId, 
			      addr      = BAddr, 
			      port      = Port,
			      type      = get, 
			      data      = MsgData, 
			      mon       = MonRef,
			      discovery = true, 
			      expire    = t() + Expire},
	    ets:insert(snmpm_request_table, Req),
	    gct_activate(State#state.gct),
	    {ok, ReqId};

	Error ->
	    ?vinfo("failed retrieving agent data for discovery (get):"
		   "~n   BAddr: ~p"
		   "~n   Port:  ~p"
		   "~n   Error: ~p", [BAddr, Port, Error]),
	    Error
    end.


handle_sync_timeout(ReqId, From, State) ->
    ?vtrace("handle_sync_timeout -> entry with"
	    "~n   ReqId: ~p"
	    "~n   From:  ~p", [ReqId, From]),
    case ets:lookup(snmpm_request_table, ReqId) of
	[#request{mon = MonRef, from = From} = Req0] ->
	    ?vdebug("handle_sync_timeout -> "
		    "deliver reply (timeout) and demonitor: "
		    "~n   Monref: ~p"
		    "~n   From:   ~p", [MonRef, From]),
	    gen_server:reply(From, {error, {timeout, ReqId}}),
	    maybe_demonitor(MonRef),
	    
	    %% 
	    %% Instead of deleting the request record now,
	    %% we leave it to the gc. But for that to work 
	    %% we must update the expire value (which for
	    %% sync requests is infinity).
	    %% 

	    Req = Req0#request{ref    = undefined, 
			       mon    = undefined, 
			       from   = undefined, 
			       expire = t()},
	    ets:insert(snmpm_request_table, Req),
	    gct_activate(State#state.gct),
	    ok;
	_ ->
	    ok
    end.

    
handle_snmp_error(#pdu{request_id = ReqId} = Pdu, Reason, _State) ->

    ?vtrace("handle_snmp_error -> entry with"
	    "~n   Reason: ~p"
	    "~n   Pdu:    ~p", [Reason, Pdu]),

    case ets:lookup(snmpm_request_table, ReqId) of

	%% Failed async request
	[#request{user_id   = UserId, 
		  from      = undefined, 
		  ref       = undefined, 
		  mon       = MonRef,
		  discovery = Disco}] ->

	    ?vdebug("handle_snmp_error -> "
		    "found corresponding request: "
		    "~n   failed async request"
		    "~n   UserId: ~p"
		    "~n   ModRef: ~p"
		    "~n   Disco:  ~p", [UserId, MonRef, Disco]),

	    maybe_demonitor(MonRef),
	    case snmpm_config:user_info(UserId) of
		{ok, UserMod, UserData} ->
		    handle_error(UserId, UserMod, Reason, ReqId, UserData),
		    maybe_delete(Disco, ReqId);
		_ ->
		    %% reply to outstanding request, for which there is no
		    %% longer any owner (the user has unregistered).
		    %% Therefor send it to the default user
		    case snmpm_config:user_info() of
			{ok, DefUserId, DefMod, DefData} ->
			    handle_error(DefUserId, DefMod, Reason, ReqId, 
					 DefData),
			    maybe_delete(Disco, ReqId);
			_ ->
			    error_msg("failed retreiving the default user "
				      "info handling error [~w]: "
				      "~n~w", [ReqId, Reason])
		    end
	    end;


	%% Failed sync request
	%%
	[#request{ref = Ref, mon = MonRef, from = From}] -> 

	    ?vdebug("handle_snmp_error -> "
		    "found corresponding request: "
		    "~n   failed sync request"
		    "~n   Ref:    ~p"
		    "~n   ModRef: ~p"
		    "~n   From:   ~p", [Ref, MonRef, From]),

	    Remaining = 
		case (catch cancel_timer(Ref)) of
		    R when integer(R) ->
			R;
		    _ ->
			0
		end,

	    ?vtrace("handle_snmp_error -> Remaining: ~p", [Remaining]),

	    maybe_demonitor(MonRef),
	    Reply = {error, {send_failed, ReqId, Reason}},
	    ?vtrace("handle_snmp_error -> deliver (error-) reply",[]), 
	    gen_server:reply(From, Reply),
	    ets:delete(snmpm_request_table, ReqId),
	    ok;


	%% A very old reply, see if this agent is handled by
	%% a user. In that case send it there, else to the 
	%% default user.
	_ ->

	    ?vdebug("handle_snmp_error -> no request?", []), 

	    case snmpm_config:user_info() of
		{ok, DefUserId, DefMod, DefData} ->
		    handle_error(DefUserId, DefMod, Reason, 
				 ReqId, DefData);
		_ ->
		    error_msg("failed retreiving the default "
			      "user info handling error [~w]: "
			      "~n~w",[ReqId, Reason])
	    end
    end;

handle_snmp_error(CrapError, Reason, _State) ->
    error_msg("received crap (snmp) error =>"
	      "~n~p~n~p", [CrapError, Reason]),
    ok.

handle_snmp_error(Addr, Port, ReqId, Reason, _State) ->

    ?vtrace("handle_snmp_error -> entry with"
	    "~n   Addr:   ~p"
	    "~n   Port:   ~p"
	    "~n   ReqId:  ~p"
	    "~n   Reason: ~p", [Addr, Port, ReqId, Reason]),

    case snmpm_config:get_agent_user_id(Addr, Port) of
	{ok, UserId} ->
	    case snmpm_config:user_info(UserId) of
		{ok, UserMod, UserData} ->
		    handle_error(UserId, UserMod, Reason, ReqId, UserData);
		_Error ->
		    case snmpm_config:user_info() of
			{ok, DefUserId, DefMod, DefData} ->
			    handle_error(DefUserId, DefMod, ReqId, 
					 Reason, DefData);
			_Error ->
			    error_msg("failed retreiving the default user "
				      "info handling snmp error "
				      "<~p,~p>: ~n~w~n~w",
				      [Addr, Port, ReqId, Reason])
		    end
	    end;
	_Error ->
	    case snmpm_config:user_info() of
		{ok, DefUserId, DefMod, DefData} ->
		    handle_error(DefUserId, DefMod, ReqId, 
				 Reason, DefData);
		_Error ->
		    error_msg("failed retreiving the default user "
			      "info handling snmp error "
			      "<~p,~p>: ~n~w~n~w",
			      [Addr, Port, ReqId, Reason])
	    end
    end.


%% handle_snmp_error(#pdu{error_status = EStatus, 
%% 		       error_index  = EIndex, 
%% 		       varbinds     = Varbinds} = Pdu, 
%% 		  ReqId, {ReportReason, Info}, Addr, Port, State) 
%%   when integer(ReqId) ->

%%     ?vtrace("handle_snmp_error -> entry with"
%% 	    "~n   Addr:         ~p"
%% 	    "~n   Port:         ~p"
%% 	    "~n   ReqId:        ~p"
%% 	    "~n   Pdu:          ~p"
%% 	    "~n   ReportReason: ~p"
%% 	    "~n   Info:         ~p", 
%% 	    [Addr, Port, ReqId, Pdu, ReportReason, Info]),

%%     SnmpReport = {EStatus, EIndex, Varbinds},
%%     Reason     = {ReportReason, Info, SnmpReport},
    
%%     %% Check if there is someone waiting for this request
%%     case ets:lookup(snmpm_request_table, ReqId) of

%% 	[#request{from = From, 
%% 		  ref  = Ref, 
%% 		  mon  = MonRef}] when From /= undefined, 
%% 				       Ref  /= undefined ->

%% 	    ?vdebug("handle_snmp_error -> "
%% 		    "found corresponding request: "
%% 		    "~n   reply to sync request"
%% 		    "~n   Ref:    ~p"
%% 		    "~n   ModRef: ~p"
%% 		    "~n   From:   ~p", [Ref, MonRef, From]),

%% 	    Remaining = 
%% 		case (catch cancel_timer(Ref)) of
%% 		    R when integer(R) ->
%% 			R;
%% 		    _ ->
%% 			0
%% 		end,

%% 	    ?vtrace("handle_snmp_pdu(get-response) -> Remaining: ~p", 
%% 		    [Remaining]),

%% 	    maybe_demonitor(MonRef),

%% 	    Reply = {error, Reason},
%% 	    ?vtrace("handle_snmp_error -> deliver reply",[]), 
%% 	    gen_server:reply(From, Reply),
%% 	    ets:delete(snmpm_request_table, ReqId),
%% 	    ok;
	
%% 	_ ->
%% 	    %% Either not a sync request or no such request. Either
%% 	    %% way, this is error info, so handle it as such.

%% 	    case snmpm_config:get_agent_user_id(Addr, Port) of
%% 		{ok, UserId} ->
%% 		    case snmpm_config:user_info(UserId) of
%% 			{ok, Mod, Data} ->
%% 			    ?vdebug("[report] callback handle_error with: "
%% 				    "~n   ~p"
%% 				    "~n   ~p"
%% 				    "~n   ~p", [UserId, Mod, Reason]),
%% 			    handle_error(UserId, Mod, Reason, ReqId, Data);
%% 			Error ->
%% 			    %% Oh crap, use the default user
%% 			    ?vlog("[report] failed retreiving user info for "
%% 				  "user ~p:"
%% 				  " ~n   ~p", [UserId, Error]),
%% 			    case snmpm_config:user_info() of
%% 				{ok, DefUserId, DefMod, DefData} ->
%% 				    handle_error(DefUserId, DefMod, 
%% 						 Reason, ReqId, DefData);
%% 				Error ->
%% 				    error_msg("failed retreiving the "
%% 					      "default user "
%% 					      "info handling report from "
%% 					      "<~p,~p>: ~n~w~n~w~n~w",
%% 					      [Addr, Port, Error, 
%% 					       ReqId, Reason])
%% 			    end
%% 		    end;
%% 		Error ->
%% 		    %% Unknown agent, pass it on to the default user
%% 		    ?vlog("[report] failed retreiving user id for "
%% 			  "agent <~p,~p>: "
%% 			  "~n   ~p", [Addr, Port, Error]),
%% 		    case snmpm_config:user_info() of
%% 			{ok, DefUserId, DefMod, DefData} ->
%% 			    handle_error(DefUserId, DefMod, Reason, ReqId, 
%% 					 DefData);
%% 			Error ->
%% 			    error_msg("failed retreiving "
%% 				      "the default user info handling "
%% 				      "error from "
%% 				      "<~p,~p>: ~n~w~n~w~n~w",
%% 				      [Addr, Port, Error, ReqId, Reason])
%% 		    end
%% 	    end
%%     end,
%%     ok;

%% handle_snmp_error(CrapError, CrapReqId, CrapInfo, Addr, Port, _State) ->
%%     error_msg("received crap (snmp) error from ~w:~w =>"
%% 	      "~n~p~n~p~n~p", [Addr, Port, CrapError, CrapReqId, CrapInfo]),
%%     ok.

handle_error(_UserId, Mod, Reason, ReqId, Data) ->
    ?vtrace("handle_error -> entry when"
	    "~n   Mod: ~p", [Mod]),
    (catch Mod:handle_error(ReqId, Reason, Data)),
    ok.


handle_snmp_pdu(#pdu{type = 'get-response', request_id = ReqId} = Pdu, 
		Addr, Port, _State) ->

    ?vtrace("handle_snmp_pdu(get-response) -> entry with"
	    "~n   Addr: ~p"
	    "~n   Port: ~p"
	    "~n   Pdu:  ~p", [Addr, Port, Pdu]),

    case ets:lookup(snmpm_request_table, ReqId) of

	%% Reply to a async request or 
	%% possibly a late reply to a sync request
	%% (ref is also undefined)
	[#request{user_id   = UserId, 
		  from      = undefined, 
		  ref       = undefined, 
		  mon       = MonRef,
		  discovery = Disco}] ->

	    ?vdebug("handle_snmp_pdu(get-response) -> "
		    "found corresponding request: "
		    "~n   reply to async request or late reply to sync request"
		    "~n   UserId: ~p"
		    "~n   ModRef: ~p"
		    "~n   Disco:  ~p", [UserId, MonRef, Disco]),

	    maybe_demonitor(MonRef),
	    #pdu{error_status = EStatus, 
		 error_index  = EIndex, 
		 varbinds     = Varbinds} = Pdu,
	    SnmpResponse = {EStatus, EIndex, Varbinds},
	    case snmpm_config:user_info(UserId) of
		{ok, UserMod, UserData} ->
		    handle_pdu(UserId, UserMod, Addr, Port, ReqId, 
			       SnmpResponse, UserData),
		    maybe_delete(Disco, ReqId);
		_Error ->
		    %% reply to outstanding request, for which there is no
		    %% longer any owner (the user has unregistered).
		    %% Therefor send it to the default user
		    case snmpm_config:user_info() of
			{ok, DefUserId, DefMod, DefData} ->
			    handle_pdu(DefUserId, DefMod, Addr, Port, ReqId, 
				       SnmpResponse, DefData),
			    maybe_delete(Disco, ReqId);
			Error ->
			    error_msg("failed retreiving the default user "
				      "info handling pdu from "
				      "<~p,~p>: ~n~w~n~w",
				      [Addr, Port, Error, Pdu])
		    end
	    end;


	%% Reply to a sync request
	%%
	[#request{ref = Ref, mon = MonRef, from = From}] -> 

	    ?vdebug("handle_snmp_pdu(get-response) -> "
		    "found corresponding request: "
		    "~n   reply to sync request"
		    "~n   Ref:    ~p"
		    "~n   ModRef: ~p"
		    "~n   From:   ~p", [Ref, MonRef, From]),

	    Remaining = 
		case (catch cancel_timer(Ref)) of
		    R when integer(R) ->
			R;
		    _ ->
			0
		end,

	    ?vtrace("handle_snmp_pdu(get-response) -> Remaining: ~p", 
		    [Remaining]),

	    maybe_demonitor(MonRef),
	    #pdu{error_status = EStatus, 
		 error_index  = EIndex, 
		 varbinds     = Varbinds} = Pdu,
	    SnmpReply = {EStatus, EIndex, Varbinds},
	    Reply = {ok, SnmpReply, Remaining},
	    ?vtrace("handle_snmp_pdu(get-response) -> deliver reply",[]), 
	    gen_server:reply(From, Reply),
	    ets:delete(snmpm_request_table, ReqId),
	    ok;
		

	%% A very old reply, see if this agent is handled by
	%% a user. In that case send it there, else to the 
	%% default user.
	_ ->

	    ?vdebug("handle_snmp_pdu(get-response) -> "
		    "no corresponding request: "
		    "~n   a very old reply", []),

	    #pdu{error_status = EStatus, 
		 error_index  = EIndex, 
		 varbinds     = Varbinds} = Pdu,
	    SnmpInfo = {EStatus, EIndex, Varbinds},
	    case snmpm_config:get_agent_user_id(Addr, Port) of
		{ok, UserId} ->
		    %% A very late reply or a reply to a request
		    %% that has been cancelled.
		    %% 
		    ?vtrace("handle_snmp_pdu(get-response) -> "
			    "a very late reply:"
			    "~n   UserId: ~p",[UserId]), 
		    case snmpm_config:user_info(UserId) of
			{ok, UserMod, UserData} ->
			    Reason = {unexpected_pdu, SnmpInfo},
			    handle_error(UserId, UserMod, Reason, ReqId, 
					 UserData);
			_Error ->
			    %% Ouch, found an agent but not it's user!!
			    case snmpm_config:user_info() of
				{ok, DefUserId, DefMod, DefData} ->
				    Reason = {unexpected_pdu, SnmpInfo}, 
				    handle_error(DefUserId, DefMod, 
						 Reason, ReqId, DefData);
				Error ->
				    error_msg("failed retreiving the default "
					      "user info handling (old) "
					      "pdu from "
					      "<~p,~p>: ~n~w~n~w",
					      [Addr, Port, Error, Pdu])
			    end
		    end;

		{error, _} -> 
		    %% No agent, so either this agent has been 
		    %% unregistered, or this is a very late reply 
		    %% to a request (possibly a discovery), which 
		    %% has since been cancelled (either because of
		    %% a timeout or that the user has unregistered 
		    %% itself (and with it all it's requests)). 
		    %% No way to know which.
		    %% 
		    ?vtrace("handle_snmp_pdu(get-response) -> "
			    "no agent info found", []),
		    case snmpm_config:user_info() of
			{ok, DefUserId, DefMod, DefData} ->
			    handle_agent(DefUserId, DefMod, Addr, Port, 
					 SnmpInfo, DefData);
			Error ->
			    error_msg("failed retreiving the default user "
				      "info handling (old) pdu when no user "
				      "found from "
				      "<~p,~p>: ~n~w~n~w",
				      [Addr, Port, Error, Pdu])
		    end
	    end
    end;

handle_snmp_pdu(CrapPdu, Addr, Port, _State) ->
    error_msg("received crap (snmp) Pdu from ~w:~w =>"
	      "~p", [Addr, Port, CrapPdu]),
    ok.

handle_pdu(_UserId, Mod, Addr, Port, ReqId, SnmpResponse, Data) ->
    ?vtrace("handle_pdu -> entry when"
	    "~n   Mod: ~p", [Mod]),
    (catch Mod:handle_pdu(Addr, Port, ReqId, SnmpResponse, Data)),
    ok.

handle_agent(UserId, Mod, Addr, Port, SnmpInfo, Data) ->
    ?vtrace("handle_agent -> entry when"
	    "~n   UserId: ~p"
	    "~n   Mod:    ~p", [UserId, Mod]),
    case (catch Mod:handle_agent(Addr, Port, SnmpInfo, Data)) of
	{register, UserId2, Config} ->  
	    ?vlog("handle_agent -> register"
		  "~n   UserId2: ~p"
		  "~n   Config:  ~p", [UserId2, Config]),
	    case snmpm_config:register_agent(UserId2, Addr, Port, Config) of
		ok ->
		    ok;
		{error, Reason} ->
		    error_msg("failed registering agent handling agent "
			      "<~p,~p>: ~n~w", 
			      [Addr, Port, Reason]),
		    ok
	    end;
	_Ignore ->
	    ok
    end.
    

%% Retrieve user info for this agent.
%% If this is an unknown agent, then use the default user
handle_snmp_trap(#trappdu{enterprise    = Enteprise, 
			  generic_trap  = Generic, 
			  specific_trap = Spec,
			  time_stamp    = Timestamp, 
			  varbinds      = Varbinds} = Trap, 
		 Addr, Port, State) ->

    ?vtrace("handle_snmp_trap [trappdu] -> entry with"
	    "~n   Addr: ~p"
	    "~n   Port: ~p"
	    "~n   Trap: ~p", [Addr, Port, Trap]),

    SnmpTrapInfo = {Enteprise, Generic, Spec, Timestamp, Varbinds},
    do_handle_snmp_trap(SnmpTrapInfo, Addr, Port, State);

handle_snmp_trap(#pdu{error_status = EStatus, 
		      error_index  = EIndex, 
		      varbinds     = Varbinds} = Trap, 
		 Addr, Port, State) ->

    ?vtrace("handle_snmp_trap [pdu] -> entry with"
	    "~n   Addr: ~p"
	    "~n   Port: ~p"
	    "~n   Trap: ~p", [Addr, Port, Trap]),

    SnmpTrapInfo = {EStatus, EIndex, Varbinds},
    do_handle_snmp_trap(SnmpTrapInfo, Addr, Port, State);

handle_snmp_trap(CrapTrap, Addr, Port, _State) ->
    error_msg("received crap (snmp) trap from ~w:~w =>"
	      "~p", [Addr, Port, CrapTrap]),
    ok.

do_handle_snmp_trap(SnmpTrapInfo, Addr, Port, _State) ->
    case snmpm_config:get_agent_user_id(Addr, Port) of
	{ok, UserId} ->
	    ?vtrace("handle_snmp_trap -> found user: ~p",[UserId]), 
	    case snmpm_config:user_info(UserId) of
		{ok, Mod, Data} ->
		    handle_trap(UserId, Mod, Addr, Port, SnmpTrapInfo, Data);

		Error ->
		    %% Oh crap, use the default user
		    ?vlog("[trap] failed retreiving user info for user ~p: "
			  "~n   ~p", [UserId, Error]),
		    case snmpm_config:user_info() of
			{ok, DefUserId, DefMod, DefData} ->
			    handle_trap(DefUserId, DefMod, Addr, Port, 
					SnmpTrapInfo, DefData);
			Error ->
			    error_msg("failed retreiving the default user "
				      "info handling report from "
				      "<~p,~p>: ~n~w~n~w",
				      [Addr, Port, Error, SnmpTrapInfo])
		    end
	    end;
	Error ->
	    %% Unknown agent, pass it on to the default user
	    ?vlog("[trap] failed retreiving user id for agent <~p,~p>: "
		  "~n   ~p", [Addr, Port, Error]),
	    case snmpm_config:user_info() of
		{ok, DefUserId, DefMod, DefData} ->
		    handle_trap(DefUserId, DefMod, Addr, Port, 
				SnmpTrapInfo, DefData);
		Error ->
		    error_msg("failed retreiving "
			      "the default user info handling trap from "
			      "<~p,~p>: ~n~w~n~w",
			      [Addr, Port, Error, SnmpTrapInfo])
	    end
    end,
    ok.

%% handle_snmp_trap(Trap, Addr, Port, _State) ->

%%     ?vtrace("handle_snmp_trap -> entry with"
%% 	    "~n   Addr: ~p"
%% 	    "~n   Port: ~p"
%% 	    "~n   Trap: ~p", [Addr, Port, Trap]),

%%     SnmpTrapInfo = 
%% 	case Trap of
%% 	    #trappdu{enterprise    = Enteprise, 
%% 		     generic_trap  = Generic, 
%% 		     specific_trap = Spec,
%% 		     time_stamp    = Timestamp, 
%% 		     varbinds      = Varbinds} ->
%% 		{Enteprise, Generic, Spec, Timestamp, Varbinds};
	    
%% 	    #pdu{error_status = EStatus, 
%% 		 error_index  = EIndex, 
%% 		 varbinds     = Varbinds} ->
%% 		{EStatus, EIndex, Varbinds}
%% 	end,
%%     case snmpm_config:get_agent_user_id(Addr, Port) of
%% 	{ok, UserId} ->
%% 	    ?vtrace("handle_snmp_trap -> found user: ~p",[UserId]), 
%% 	    case snmpm_config:user_info(UserId) of
%% 		{ok, Mod, Data} ->
%% 		    handle_trap(UserId, Mod, Addr, Port, SnmpTrapInfo, Data);

%% 		Error ->
%% 		    %% Oh crap, use the default user
%% 		    ?vlog("[trap] failed retreiving user info for user ~p: "
%% 			  "~n   ~p", [UserId, Error]),
%% 		    case snmpm_config:user_info() of
%% 			{ok, DefUserId, DefMod, DefData} ->
%% 			    handle_trap(DefUserId, DefMod, Addr, Port, 
%% 					SnmpTrapInfo, DefData);
%% 			Error ->
%% 			    error_msg("failed retreiving the default user "
%% 				      "info handling report from "
%% 				      "<~p,~p>: ~n~w~n~w",
%% 				      [Addr, Port, Error, Trap])
%% 		    end
%% 	    end;
%% 	Error ->
%% 	    %% Unknown agent, pass it on to the default user
%% 	    ?vlog("[trap] failed retreiving user id for agent <~p,~p>: "
%% 		  "~n   ~p", [Addr, Port, Error]),
%% 	    case snmpm_config:user_info() of
%% 		{ok, DefUserId, DefMod, DefData} ->
%% 		    handle_trap(DefUserId, DefMod, Addr, Port, 
%% 				SnmpTrapInfo, DefData);
%% 		Error ->
%% 		    error_msg("failed retreiving "
%% 			      "the default user info handling trap from "
%% 			      "<~p,~p>: ~n~w~n~w",
%% 			      [Addr, Port, Error, Trap])
%% 	    end
%%     end,
%%     ok.

handle_trap(UserId, Mod, Addr, Port, SnmpTrapInfo, Data) ->
    ?vtrace("handle_trap -> entry with"
	    "~n   UserId: ~p"
	    "~n   Mod:    ~p", [UserId, Mod]),
    case (catch Mod:handle_trap(Addr, Port, SnmpTrapInfo, Data)) of
	{register, UserId2, Config} -> 
	    ?vlog("handle_trap -> register"
		  "~n   UserId2: ~p"
		  "~n   Config:  ~p", [UserId2, Config]),
	    case snmpm_config:register_agent(UserId2, Addr, Port, Config) of
		ok ->
		    ok;
		{error, Reason} ->
		    error_msg("failed registering agent handling trap "
			      "<~p,~p>: ~n~w", 
			      [Addr, Port, Reason]),
		    ok
	    end;
	unregister ->
 	    case snmpm_config:unregister_agent(UserId, Addr, Port) of
 		ok ->
 		    ok;
 		{error, Reason} ->
 		    error_msg("failed unregistering agent handling trap "
			      "<~p,~p>: ~n~w", 
			      [Addr, Port, Reason]),
 		    ok
 	    end;	    
	_Ignore ->
	    ok
    end.
    
   
handle_snmp_inform(#pdu{error_status = EStatus, 
			error_index  = EIndex, 
			varbinds     = Varbinds} = Pdu, Addr, Port, _State) ->
 
    ?vtrace("handle_snmp_inform -> entry with"
	    "~n   Addr: ~p"
	    "~n   Port: ~p"
	    "~n   Pdu:  ~p", [Addr, Port, Pdu]),

    SnmpInform = {EStatus, EIndex, Varbinds},
    case snmpm_config:get_agent_user_id(Addr, Port) of
	{ok, UserId} ->
	    case snmpm_config:user_info(UserId) of
		{ok, Mod, Data} ->
		    ?vdebug("[inform] callback handle_inform with: "
			    "~n   ~p"
			    "~n   ~p"
			    "~n   ~p", [UserId, Mod, SnmpInform]),
		    handle_inform(UserId, Mod, Addr, Port, SnmpInform, Data);
		Error ->
		    %% Oh crap, use the default user
		    ?vlog("[inform] failed retreiving user info for user ~p:"
			  "~n   ~p", [UserId, Error]),
		    case snmpm_config:user_info() of
			{ok, DefUserId, DefMod, DefData} ->
			    handle_inform(DefUserId, DefMod, Addr, Port, 
					  SnmpInform, DefData);
			Error ->
			    error_msg("failed retreiving the default user "
				      "info handling inform from "
				      "<~p,~p>: ~n~w~n~w",
				      [Addr, Port, Error, Pdu])
		    end
	    end;
	Error ->
	    %% Unknown agent, pass it on to the default user
	    ?vlog("[inform] failed retreiving user id for agent <~p,~p>: "
		  "~n   ~p", [Addr, Port, Error]),
	    case snmpm_config:user_info() of
		{ok, DefUserId, DefMod, DefData} ->
		    handle_inform(DefUserId, DefMod, Addr, Port, 
				  SnmpInform, DefData);
		Error ->
		    error_msg("failed retreiving "
			      "the default user info handling inform from "
			      "<~p,~p>: ~n~w~n~w",
			      [Addr, Port, Error, Pdu])
	    end
    end,
    ok;

handle_snmp_inform(CrapInform, Addr, Port, _State) ->
    error_msg("received crap (snmp) inform from ~w:~w =>"
	      "~p", [Addr, Port, CrapInform]),
    ok.

handle_inform(UserId, Mod, Addr, Port, SnmpInform, Data) ->
    case (catch Mod:handle_inform(Addr, Port, SnmpInform, Data)) of
	{register, UserId2, Config} -> 
	    %% The only user which would do this is the default user
	    case snmpm_config:register_agent(UserId2, Addr, Port, Config) of
		ok ->
		    ok;
		{error, Reason} ->
		    error_msg("failed registering agent handling inform "
			      "<~p,~p>: ~n~w", 
			      [Addr, Port, Reason]),
		    ok
	    end;
	unregister ->
 	    case snmpm_config:unregister_agent(UserId, Addr, Port) of
 		ok ->
 		    ok;
 		{error, Reason} ->
 		    error_msg("failed unregistering agent handling inform "
			      "<~p,~p>: ~n~w", 
			      [Addr, Port, Reason]),
 		    ok
 	    end;	    
	_Ignore ->
	    ok
    end.


    
handle_snmp_report(#pdu{error_status = EStatus, 
			error_index  = EIndex, 
			varbinds     = Varbinds} = Pdu, Addr, Port, _State) ->

    ?vtrace("handle_snmp_report -> entry with"
	    "~n   Addr: ~p"
	    "~n   Port: ~p"
	    "~n   Pdu:  ~p", [Addr, Port, Pdu]),

    SnmpReport = {EStatus, EIndex, Varbinds},
    case snmpm_config:get_agent_user_id(Addr, Port) of
 	{ok, UserId} ->
 	    case snmpm_config:user_info(UserId) of
 		{ok, Mod, Data} ->
 		    ?vdebug("[report] callback handle_report with: "
 			    "~n   ~p"
 			    "~n   ~p"
 			    "~n   ~p", [UserId, Mod, SnmpReport]),
 		    handle_report(UserId, Mod, Addr, Port, SnmpReport, Data);
 		Error ->
 		    %% Oh crap, use the default user
 		    ?vlog("[report] failed retreiving user info for user ~p:"
 			  " ~n   ~p", [UserId, Error]),
		    case snmpm_config:user_info() of
			{ok, DefUserId, DefMod, DefData} ->
			    handle_report(DefUserId, DefMod, Addr, Port, 
					  SnmpReport, DefData);
			Error ->
			    error_msg("failed retreiving the default user "
				      "info handling report from "
				      "<~p,~p>: ~n~w~n~w",
				      [Addr, Port, Error, Pdu])
		    end
 	    end;
 	Error ->
 	    %% Unknown agent, pass it on to the default user
 	    ?vlog("[report] failed retreiving user id for agent <~p,~p>: "
 		  "~n   ~p", [Addr, Port, Error]),
	    case snmpm_config:user_info() of
		{ok, DefUserId, DefMod, DefData} ->
		    handle_report(DefUserId, DefMod, Addr, Port, 
				  SnmpReport, DefData);
		Error ->
		    error_msg("failed retreiving "
			      "the default user info handling report from "
			      "<~p,~p>: ~n~w~n~w",
			      [Addr, Port, Error, Pdu])
	    end
    end,
    ok;

handle_snmp_report(CrapReport, Addr, Port, _State) ->
    error_msg("received crap (snmp) report from ~w:~w =>"
	      "~p", [Addr, Port, CrapReport]),
    ok.

%% This could be from a failed get-request, so we might have a user
%% waiting for a reply here. If there is, we handle this as an failed
%% get-response (except for tha data which is different). Otherwise,
%% we handle it as an error (reported via the handle_error callback
%% function).
handle_snmp_report(ReqId, 
		   #pdu{error_status = EStatus, 
			error_index  = EIndex, 
			varbinds     = Varbinds} = Pdu, 
		   {ReportReason, Info} = R, 
		   Addr, Port, _State) 
  when integer(ReqId) ->

    ?vtrace("handle_snmp_report -> entry with"
	    "~n   Addr:   ~p"
	    "~n   Port:   ~p"
	    "~n   ReqId:  ~p"
	    "~n   R:      ~p"
	    "~n   Pdu:    ~p", [Addr, Port, ReqId, R, Pdu]),

    SnmpReport = {EStatus, EIndex, Varbinds},
    Reason     = {ReportReason, Info, SnmpReport},
    
    %% Check if there is someone waiting for this request

    case ets:lookup(snmpm_request_table, ReqId) of

	[#request{from = From, 
		  ref  = Ref, 
		  mon  = MonRef}] when From /= undefined, 
				       Ref  /= undefined ->

	    ?vdebug("handle_snmp_report -> "
		    "found corresponding request: "
		    "~n   reply to sync request"
		    "~n   Ref:    ~p"
		    "~n   ModRef: ~p"
		    "~n   From:   ~p", [Ref, MonRef, From]),

	    Remaining = 
		case (catch cancel_timer(Ref)) of
		    R when integer(R) ->
			R;
		    _ ->
			0
		end,

	    ?vtrace("handle_snmp_pdu(get-response) -> Remaining: ~p", 
		    [Remaining]),

	    maybe_demonitor(MonRef),

	    Reply = {error, Reason},
	    ?vtrace("handle_snmp_report -> deliver reply",[]), 
	    gen_server:reply(From, Reply),
	    ets:delete(snmpm_request_table, ReqId),
	    ok;
	
	_ ->
	    %% Either not a sync request or no such request. Either
	    %% way, this is error info, so handle it as such.

	    case snmpm_config:get_agent_user_id(Addr, Port) of
		{ok, UserId} ->
		    case snmpm_config:user_info(UserId) of
			{ok, Mod, Data} ->
			    ?vdebug("[report] callback handle_error with: "
				    "~n   ~p"
				    "~n   ~p"
				    "~n   ~p", [UserId, Mod, Reason]),
			    handle_error(UserId, Mod, Reason, ReqId, Data);
			Error ->
			    %% Oh crap, use the default user
			    ?vlog("[report] failed retreiving user info for "
				  "user ~p:"
				  " ~n   ~p", [UserId, Error]),
			    case snmpm_config:user_info() of
				{ok, DefUserId, DefMod, DefData} ->
				    handle_error(DefUserId, DefMod, 
						 Reason, ReqId, DefData);
				Error ->
				    error_msg("failed retreiving the "
					      "default user "
					      "info handling report from "
					      "<~p,~p>: ~n~w~n~w~n~w",
					      [Addr, Port, Error, 
					       ReqId, Reason])
			    end
		    end;
		Error ->
		    %% Unknown agent, pass it on to the default user
		    ?vlog("[report] failed retreiving user id for "
			  "agent <~p,~p>: "
			  "~n   ~p", [Addr, Port, Error]),
		    case snmpm_config:user_info() of
			{ok, DefUserId, DefMod, DefData} ->
			    handle_error(DefUserId, DefMod, Reason, ReqId, 
					 DefData);
			Error ->
			    error_msg("failed retreiving "
				      "the default user info handling "
				      "report from "
				      "<~p,~p>: ~n~w~n~w~n~w",
				      [Addr, Port, Error, ReqId, Reason])
		    end
	    end
    end,
    ok;

handle_snmp_report(CrapReqId, CrapReport, CrapInfo, Addr, Port, _State) ->
    error_msg("received crap (snmp) report from ~w:~w =>"
	      "~n~p~n~p~n~p", [Addr, Port, CrapReqId, CrapReport, CrapInfo]),
    ok.
   

handle_report(UserId, Mod, Addr, Port, SnmpReport, Data) ->
    case (catch Mod:handle_report(Addr, Port, SnmpReport, Data)) of
 	{register, UserId2, Config} -> 
	    %% The only user which would do this is the default user
 	    case snmpm_config:register_agent(UserId2, Addr, Port, Config) of
 		ok ->
 		    ok;
 		{error, Reason} ->
 		    error_msg("failed registering agent handling report "
			      "<~p,~p>: ~n~w", 
			      [Addr, Port, Reason]),
 		    ok
 	    end;
	unregister ->
 	    case snmpm_config:unregister_agent(UserId, Addr, Port) of
 		ok ->
 		    ok;
 		{error, Reason} ->
 		    error_msg("failed unregistering agent handling report "
			      "<~p,~p>: ~n~w", 
			      [Addr, Port, Reason]),
 		    ok
 	    end;	    
	_Ignore ->
	    ok
    end.


    
handle_down(MonRef) ->
    %% Clear out all requests from this client
    handle_down_requests_cleanup(MonRef),

    %% 
    %% Check also if this was a monitored user, and if so
    %% unregister all agents registered by this user, and
    %% finally unregister the user itself
    %% 
    handle_down_user_cleanup(MonRef),
    
    ok.


handle_down_requests_cleanup(MonRef) ->
    Pat = #request{id = '$1', ref = '$2', mon = MonRef, _ = '_'},
    Match = ets:match(snmpm_request_table, Pat),
    Fun = fun([Id, Ref]) -> 
		  ?vtrace("delete request: ~p", [Id]),
		  ets:delete(snmpm_request_table, Id),
		  cancel_timer(Ref),
		  ok
	  end,
    lists:foreach(Fun, Match).

handle_down_user_cleanup(MonRef) ->
    Pat = #monitor{id = '$1', mon = MonRef, _ = '_'},
    Match = ets:match(snmpm_monitor_table, Pat),
    Fun = fun([Id]) -> 
		  Agents = snmpm_config:which_agents(Id),
		  lists:foreach(
		    fun({A,P}) -> 
			    ?vtrace("unregister agent of monitored user "
				    "~w: <~w,~w>", [Id,A,P]),
			    snmpm_config:unregister_agent(Id, A, P)
		    end,
		    Agents),
		  ?vtrace("unregister monitored user: ~w", [Id]),
		  ets:delete(snmpm_monitor_table, Id),
		  snmpm_config:unregister_user(Id),
		  ok
	     end,
    lists:foreach(Fun, Match).
    
cancel_timer(undefined) ->
    ok;
cancel_timer(Ref) ->
    (catch erlang:cancel_timer(Ref)).

handle_gc(GCT) ->
    ets:safe_fixtable(snmpm_request_table, true),
    case do_gc(ets:first(snmpm_request_table), t()) of
	0 ->
	    %% BMK BMK BMK
	    %% When do we activate?
	    gct_deactivate(GCT);
	_ ->
	    ok
    end,
    ets:safe_fixtable(snmpm_request_table, false).



%% We are deleting at the same time as we are traversing the table!!!
do_gc('$end_of_table', _) ->
    ets:info(snmpm_request_table, size);
do_gc(Key, Now) ->
    Next = ets:next(snmpm_request_table, Key),
    case ets:lookup(snmpm_request_table, Key) of
	[#request{expire = BestBefore}] when BestBefore < Now ->
	    ets:delete(snmpm_request_table, Key);
	_ ->
	    ok
    end,
    do_gc(Next, Now).
	    


%%----------------------------------------------------------------------
%% 
%%----------------------------------------------------------------------

send_get_request(Oids, Vsn, MsgData, Addr, Port, ExtraInfo, 
		 #state{net_if     = NetIf, 
			net_if_mod = Mod,
			mini_mib   = MiniMIB}) ->
    Pdu = make_pdu(get, Oids, MiniMIB),
    ?vtrace("send_get_request -> send get-request:"
	    "~n   Mod:     ~p"
	    "~n   NetIf:   ~p"
	    "~n   Pdu:     ~p"
	    "~n   Vsn:     ~p"
	    "~n   MsgData: ~p"
	    "~n   Addr:    ~p"
	    "~n   Port:    ~p", [Mod, NetIf, Pdu, Vsn, MsgData, Addr, Port]),
    (catch Mod:send_pdu(NetIf, Pdu, Vsn, MsgData, Addr, Port, ExtraInfo)),
    Pdu#pdu.request_id.

send_get_next_request(Oids, Vsn, MsgData, Addr, Port, ExtraInfo, 
		      #state{mini_mib   = MiniMIB, 
			     net_if     = NetIf, 
			     net_if_mod = Mod}) ->
    Pdu = make_pdu(get_next, Oids, MiniMIB),
    Mod:send_pdu(NetIf, Pdu, Vsn, MsgData, Addr, Port, ExtraInfo),
    Pdu#pdu.request_id.

% send_get_bulk_request(Oids, Vsn, MsgData, Addr, Port, 
% 		      NonRep, MaxRep, 
% 		      #state{mini_mib   = MiniMIB, 
% 			     net_if     = NetIf, 
% 			     net_if_mod = Mod}) ->
%     Pdu = make_pdu(bulk, {NonRep, MaxRep, Oids}, MiniMIB),
%     Mod:send_pdu(NetIf, Pdu, Vsn, MsgData, Addr, Port),
%     Pdu#pdu.request_id.

send_set_request(VarsAndVals, Vsn, MsgData, Addr, Port, ExtraInfo, 
		 #state{mini_mib   = MiniMIB,
			net_if     = NetIf, 
			net_if_mod = Mod}) ->
    Pdu = make_pdu(set, VarsAndVals, MiniMIB),
    Mod:send_pdu(NetIf, Pdu, Vsn, MsgData, Addr, Port, ExtraInfo),
    Pdu#pdu.request_id.

send_discovery(Vsn, MsgData, Addr, Port, ExtraInfo, 
	       #state{net_if     = NetIf, 
		      net_if_mod = Mod}) ->
    Pdu = make_discovery_pdu(),
    Mod:send_pdu(NetIf, Pdu, Vsn, MsgData, Addr, Port, ExtraInfo),
    Pdu#pdu.request_id.
							  


%%----------------------------------------------------------------------
%% 
%%----------------------------------------------------------------------

make_discovery_pdu() ->
    Oids = [?sysObjectID_instance, ?sysDescr_instance, ?sysUpTime_instance],
    make_pdu_impl(get, Oids).

make_pdu(set, VarsAndVals, MiniMIB) ->
    VBs = [var_and_value_to_varbind(VAV, MiniMIB) || VAV <- VarsAndVals],
    make_pdu_impl(set, VBs);

% make_pdu(bulk, {NonRepeaters, MaxRepetitions, Oids}, MiniMIB) ->
%     Foids = [flatten_oid(Oid, MiniMIB) || Oid <- Oids],
%     #pdu{type         = 'get-bulk-request', 
% 	 request_id   = request_id(),
% 	 error_status = NonRepeaters, 
% 	 error_index  = MaxRepetitions,
% 	 varbinds     = [make_vb(Foid) || Foid <- Foids]};

make_pdu(Op, Oids, MiniMIB) ->
    Foids = [flatten_oid(Oid, MiniMIB) || Oid <- Oids],
    make_pdu_impl(Op, Foids).


make_pdu_impl(get, Oids) ->
    #pdu{type         = 'get-request',
	 request_id   = request_id(),
	 error_status = noError, 
	 error_index  = 0,
	 varbinds     = [make_vb(Oid) || Oid <- Oids]};

make_pdu_impl(get_next, Oids) ->
    #pdu{type         = 'get-next-request', 
	 request_id   = request_id(), 
	 error_status = noError, 
	 error_index  = 0,
	 varbinds     = [make_vb(Oid) || Oid <- Oids]};

make_pdu_impl(set, Varbinds) ->
    #pdu{type         = 'set-request', 
	 request_id   = request_id(),
	 error_status = noError, 
	 error_index  = 0, 
	 varbinds     = Varbinds}.


%%----------------------------------------------------------------------
%% Purpose: Unnesting of oids like [myTable, 3, 4, "hej", 45] to
%%          [1,2,3,3,4,104,101,106,45]
%%----------------------------------------------------------------------

flatten_oid([A|T], MiniMIB) when atom(A) ->
    Oid = [alias2oid(A, MiniMIB)|T],
    check_is_pure_oid(lists:flatten(Oid));
flatten_oid(Oid, _) when list(Oid) ->
    check_is_pure_oid(lists:flatten(Oid));
flatten_oid(Shit, _) ->
    throw({error, {invalid_oid, Shit}}).
	       
check_is_pure_oid([]) -> [];
check_is_pure_oid([X | T]) when integer(X), X >= 0 ->
    [X | check_is_pure_oid(T)];
check_is_pure_oid([X | _T]) ->
    throw({error, {invalid_oid, X}}).


var_and_value_to_varbind({Oid, Type, Value}, MiniMIB) ->
    Oid2 = flatten_oid(Oid, MiniMIB), 
    #varbind{oid          = Oid2, 
	     variabletype = char_to_type(Type), 
	     value        = Value};
var_and_value_to_varbind({Oid, Value}, MiniMIB) ->
    Oid2 = flatten_oid(Oid, MiniMIB), 
    #varbind{oid          = Oid2, 
	     variabletype = oid2type(Oid2, MiniMIB),
	     value        = Value}.

char_to_type(i) ->
    'INTEGER';
char_to_type(u) ->
    'Unsigned32';
char_to_type(g) -> % Gauge, Gauge32
    'Unsigned32';
char_to_type(b) -> 
    'BITS';
char_to_type(ia) -> 
    'IpAddress';
char_to_type(op) -> 
    'Opaque';
char_to_type(c32) -> 
    'Counter32';
char_to_type(c64) -> 
    'Counter64';
char_to_type(tt) -> 
    'TimeTicks';
char_to_type(o) ->
    'OBJECT IDENTIFIER';
char_to_type(s) ->
    'OCTET STRING';
char_to_type(C) ->
    throw({error, {invalid_value_type, C}}).


alias2oid(AliasName, MiniMIB) when atom(AliasName) ->
    case lists:keysearch(AliasName, 2, MiniMIB) of
	{value, {Oid, _Aliasname, _Type}} -> 
 	    Oid;
	false -> 
 	    throw({error, {unknown_aliasname, AliasName}})
    end.

oid2type(Oid, MiniMIB) ->
    Oid2 = case lists:reverse(Oid) of
	       [0|T] ->
		   lists:reverse(T);
	       _ ->
		   Oid
	   end,
    oid2type(Oid2, MiniMIB, utter_nonsense).

oid2type(_Oid, [], utter_nonsense) ->
    throw({error, no_type});
oid2type(_Oid, [], Type) ->
    Type;
oid2type(Oid, [{Oid2, _, Type}|MiniMIB], Res) when Oid2 =< Oid ->
    case lists:prefix(Oid2, Oid) of
        true ->
            oid2type(Oid, MiniMIB, Type); % A better guess
        false ->
            oid2type(Oid, MiniMIB, Res)
    end;
oid2type(_Oid, _MiniMIB, utter_nonsense) ->
    throw({error, no_type});
oid2type(_Oid, _MiniMIB, Type) ->
    Type.
    

make_vb(Oid) ->
    #varbind{oid = Oid, variabletype = 'NULL', value = 'NULL'}.


%%----------------------------------------------------------------------

request_id() ->
    snmpm_mpd:next_req_id().


%%----------------------------------------------------------------------

agent_data(Addr, Port, CtxName) ->
    agent_data(Addr, Port, CtxName, []).

agent_data(Addr, Port, CtxName, Config) ->
    case snmpm_config:agent_info(Addr, Port, all) of
	{ok, Info} ->
	    {value, {_, Version}} = lists:keysearch(version, 1, Info),
	    MsgData = 
		case Version of
		    v3 ->
			DefTargetName = agent_data_item(target_name, Info),
			DefEngineId   = agent_data_item(engine_id,   Info),
			DefSecModel   = agent_data_item(sec_model,   Info),
			DefSecName    = agent_data_item(sec_name,    Info),
			DefSecLevel   = agent_data_item(sec_level,   Info),

			TargetName    = agent_data_item(target_name, 
							Config, 
							DefTargetName),
			EngineId      = agent_data_item(engine_id,   
							Config, 
							DefEngineId),
			SecModel      = agent_data_item(sec_model,   
							Config, 
							DefSecModel),
			SecName       = agent_data_item(sec_name,    
							Config, 
							DefSecName),
			SecLevel      = agent_data_item(sec_level,   
							Config, 
							DefSecLevel),
			
			{SecModel, SecName, mk_sec_level_flag(SecLevel), 
			 EngineId, CtxName, TargetName};
		    _ ->
			DefComm     = agent_data_item(community, Info),
			DefSecModel = agent_data_item(sec_model, Info),

			Comm        = agent_data_item(community, 
						      Config, 
						      DefComm),
			SecModel    = agent_data_item(sec_model, 
						      Config, 
						      DefSecModel),
			
			{Comm, SecModel}
		end,
	    {ok, version(Version), MsgData};
	Error ->
	    Error
    end.

agent_data_item(Item, Info) ->
    {value, {_, Val}} = lists:keysearch(Item, 1, Info),
    Val.

agent_data_item(Item, Info, Default) ->
    case lists:keysearch(Item, 1, Info) of
	{value, {_, Val}} ->
	    Val;
	false ->
	    Default
    end.


version(v1) ->
    'version-1';
version(v2) ->
    'version-2';
version(v3) ->
    'version-3'.


%%-----------------------------------------------------------------
%% Convert the SecurityLevel into a flag value used by snmpm_mpd
%%-----------------------------------------------------------------
mk_sec_level_flag(?'SnmpSecurityLevel_noAuthNoPriv') -> 0;
mk_sec_level_flag(?'SnmpSecurityLevel_authNoPriv') -> 1;
mk_sec_level_flag(?'SnmpSecurityLevel_authPriv') -> 3.


%%----------------------------------------------------------------------
%% Request Garbage Collector timer
%%----------------------------------------------------------------------

gct_start(Timeout) ->
    ?vdebug("start gc timer process (~p)", [Timeout]),    
    State = #gct{parent = self(), timeout = Timeout},
    proc_lib:start_link(?MODULE, gct_init, [State]).

gct_stop(GCT) ->
    GCT ! {stop, self()}.

gct_activate(GCT) ->
    GCT ! {activate, self()}.

gct_deactivate(GCT) ->
    GCT ! {deactivate, self()}.

gct_code_change(GCT) ->
    GCT ! {code_change, self()}.

gct_init(#gct{parent = Parent, timeout = Timeout} = State) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    gct(State, Timeout).

gct(#gct{parent = Parent, state = active} = State, Timeout) ->
    T = t(),
    receive
	{stop, Parent} ->
	    ok;

	%% This happens when a new request is received.
	{activate, Parent}  ->
	    ?MODULE:gct(State, new_timeout(Timeout, T)); 

	{deactivate, Parent} ->
	    %% Timeout is of no consequence in the idle state, 
	    %% but just to be sure
	    NewTimeout = State#gct.timeout,
	    ?MODULE:gct(State#gct{state = idle}, NewTimeout);

	{code_change, Parent} ->
	    %% Let the server take care of this
	    exit(normal);

	{'EXIT', Parent, _Reason} ->
	    ok;

	_ -> % Crap
	    ?MODULE:gct(State, Timeout)

    after Timeout ->
	    Parent ! gc_timeout,
	    NewTimeout = State#gct.timeout,
	    ?MODULE:gct(State, NewTimeout)
    end;

gct(#gct{parent = Parent, state = idle} = State, Timeout) ->
    receive
	{stop, Parent} ->
	    ok;

	{deactivate, Parent} ->
	    ?MODULE:gct(State, Timeout);

	{activate, Parent} ->
	    NewTimeout = State#gct.timeout,
	    ?MODULE:gct(State#gct{state = active}, NewTimeout);

	{code_change, Parent} ->
	    %% Let the server take care of this
	    exit(normal);

	{'EXIT', Parent, _Reason} ->
	    ok;

	_ -> % Crap
	    ?MODULE:gct(State, Timeout)

    after Timeout ->
	    ?MODULE:gct(State, Timeout)
    end.

new_timeout(T1, T2) ->
    case T1 - (t() - T2) of
	T when T > 0 ->
	    T;
	_ ->
	    0
    end.


%%----------------------------------------------------------------------

maybe_delete(false, ReqId) ->
    ets:delete(snmpm_request_table, ReqId);
maybe_delete(true, _) ->
    ok.
    
maybe_demonitor(undefined) ->
    ok;
maybe_demonitor(MonRef) ->
    erlang:demonitor(MonRef).

%% Time in milli seconds
t() ->
    {A,B,C} = erlang:now(),
    A*1000000000+B*1000+(C div 1000).
    

%%----------------------------------------------------------------------
	
call(Req) ->
    gen_server:call(?SERVER, Req, infinity).

% cast(Msg) ->
%     gen_server:cast(?SERVER, Msg).

info_msg(F, A) ->
    catch error_logger:info_msg("*** SNMPM: " ++ F ++ "~n", A).
 
error_msg(F, A) ->
    catch error_logger:error_msg("*** SNMPM: " ++ F ++ "~n", A).
 

%%----------------------------------------------------------------------
%% Debug
%%----------------------------------------------------------------------

% sz(L) when list(L) ->
%     length(lists:flatten(L));
% sz(B) when binary(B) ->
%     size(B).

%% i(F) ->
%%     i(F, []).

%% i(F, A) ->
%%     io:format(F ++ "~n", A).

