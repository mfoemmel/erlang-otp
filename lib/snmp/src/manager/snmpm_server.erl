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

	 register_user/3, unregister_user/1, 

	 sync_get/5, sync_get/6, async_get/5, async_get/6, 
	 sync_get_next/5, sync_get_next/6, async_get_next/5, async_get_next/6, 
	 sync_get_bulk/7, sync_get_bulk/8, 
	 sync_set/5, sync_set/6, async_set/5, async_set/6, 
	 cancel_async_request/2,

	 discovery/1, discovery/2, discovery/3, discovery/4, 

	 reconfigure/0,

	 verbosity/1, verbosity/2 

	]).

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 code_change/3, terminate/2]).

%% GCT exports
-export([gct_init/2, gct/3]).


-include("snmp_debug.hrl").
-include("snmp_types.hrl").
-include("STANDARD-MIB.hrl").
-include("snmp_verbosity.hrl").


%%----------------------------------------------------------------------

-define(SERVER, ?MODULE).

-define(SYNC_GET_TIMEOUT,     5000).
-define(SYNC_SET_TIMEOUT,     5000).
-define(DEFAULT_ASYNC_EXPIRE, 5000).

-define(SNMP_AGENT_PORT, 4000).


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


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start_link() ->
    ?d("start_link -> entry", []),
    Args = [],
    ?GS_START_LINK(Args).

stop() ->
    call(stop).


register_user(UserId, UserMod, UserData) ->
    snmpm_config:register_user(UserId, UserMod, UserData).

unregister_user(UserId) ->
    call({unregister_user, UserId}).


sync_get(UserId, Addr, Port, CtxName, Oids) ->
    sync_get(UserId, Addr, Port, CtxName, Oids, ?SYNC_GET_TIMEOUT).

sync_get(UserId, Addr0, Port, CtxName, Oids, Timeout) 
  when integer(Port), list(CtxName), list(Oids), integer(Timeout)->
    {ok, Addr} = inet:getaddr(Addr0, inet),
    call({sync_get, self(), UserId, Addr, Port, CtxName, Oids, Timeout}).

async_get(UserId, Addr, Port, CtxName, Oids) ->
    async_get(UserId, Addr, Port, CtxName, Oids, ?DEFAULT_ASYNC_EXPIRE).

async_get(UserId, Addr0, Port, CtxName, Oids, Expire) 
  when integer(Port), 
       list(CtxName), 
       list(Oids), 
       integer(Expire), Expire >= 0 ->
    {ok, Addr} = inet:getaddr(Addr0, inet),
    call({async_get, self(), UserId, Addr, Port, CtxName, Oids, Expire}).

sync_get_next(UserId, Addr0, Port, CtxName, Oids) ->
    sync_get_next(UserId, Addr0, Port, CtxName, Oids, ?SYNC_GET_TIMEOUT).

sync_get_next(UserId, Addr0, Port, CtxName, Oids, Timeout) 
  when integer(Port), list(CtxName), list(Oids), integer(Timeout) ->
    {ok, Addr} = inet:getaddr(Addr0, inet),
    call({sync_get_next, self(), UserId, Addr, Port, CtxName, Oids, Timeout}).

async_get_next(UserId, Addr, Port, CtxName, Oids) ->
    async_get_next(UserId, Addr, Port, CtxName, Oids, ?DEFAULT_ASYNC_EXPIRE).

async_get_next(UserId, Addr0, Port, CtxName, Oids, Expire) 
  when integer(Port), 
       list(CtxName), 
       list(Oids), 
       integer(Expire), Expire >= 0 ->
    {ok, Addr} = inet:getaddr(Addr0, inet),
    call({async_get_next, self(), UserId, Addr, Port, CtxName, Oids, Expire}).

sync_get_bulk(UserId, Addr, Port, NonRep, MaxRep, CtxName, Oids) ->
    sync_get_bulk(UserId, Addr, Port, 
		  NonRep, MaxRep, CtxName, Oids, ?SYNC_GET_TIMEOUT).

sync_get_bulk(UserId, Addr0, Port, NonRep, MaxRep, CtxName, Oids, Timeout) 
  when integer(Port), 
       integer(NonRep), integer(MaxRep), 
       list(CtxName), list(Oids), integer(Timeout) ->
    {ok, Addr} = inet:getaddr(Addr0, inet),
    call({sync_get_bulk, self(), UserId, Addr, Port, 
	  NonRep, MaxRep, CtxName, Oids, Timeout}).

%% VarsAndValues is: {PlainOid, o|s|i, Value} (unknown mibs) | {Oid, Value} 
sync_set(UserId, Addr0, Port, CtxName, VarsAndVals) ->
    sync_set(UserId, Addr0, Port, CtxName, VarsAndVals, ?SYNC_SET_TIMEOUT).

sync_set(UserId, Addr0, Port, CtxName, VarsAndVals, Timeout) 
  when integer(Port), list(CtxName), list(VarsAndVals), integer(Timeout) ->
    {ok, Addr} = inet:getaddr(Addr0, inet),
    call({sync_set, self(), UserId, Addr, Port, 
	  CtxName, VarsAndVals, Timeout}).

async_set(UserId, Addr, Port, CtxName, VarsAndVals) ->
    async_set(UserId, Addr, Port, CtxName, VarsAndVals, ?DEFAULT_ASYNC_EXPIRE).

async_set(UserId, Addr0, Port, CtxName, VarsAndVals, Expire) 
  when integer(Port), 
       list(CtxName), 
       list(VarsAndVals), 
       integer(Expire), Expire >= 0 ->
    {ok, Addr} = inet:getaddr(Addr0, inet),
    call({async_set, self(), UserId, Addr, Port, 
	  CtxName, VarsAndVals, Expire}).


cancel_async_request(UserId, ReqId) ->
    call({cancel_async_request, UserId, ReqId}).


%% Discovery on our subnet using default port
discovery(UserId) ->
    {ok, Host} = inet:gethostname(),
    {ok, {A1, A2, A3, _A4}} = inet:getaddr(Host, inet),
    Addr = {A1, A2, A3, 255},
    discovery(UserId, Addr, ?SNMP_AGENT_PORT, ?DEFAULT_ASYNC_EXPIRE).

%% Discovery on our subnet
discovery(UserId, Port) when integer(Port) ->
    {ok, Host} = inet:gethostname(),
    {ok, {A1, A2, A3, _A4}} = inet:getaddr(Host, inet),
    Addr = {A1, A2, A3, 255},
    discovery(UserId, Addr, Port, ?DEFAULT_ASYNC_EXPIRE);

%% Discovery using default port
discovery(UserId, Addr) ->
    discovery(UserId, Addr, ?SNMP_AGENT_PORT, ?DEFAULT_ASYNC_EXPIRE).

%% Discovery 
discovery(UserId, Addr, Port) ->
    discovery(UserId, Addr, Port, ?DEFAULT_ASYNC_EXPIRE).

discovery(UserId, {A1, A2, A3, A4} = Addr, Port, Expire) 
  when integer(A1), A1 >= 0, A1 =< 255,
       integer(A2), A2 >= 0, A2 =< 255,
       integer(A3), A3 >= 0, A3 =< 255,
       integer(A4), A4 == 255,
       integer(Port) ->
    call({discovery, self(), UserId, Addr, Port, Expire}).

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

handle_call({unregister_user, UserId}, _From, State) ->
    ?vlog("received request to unregister user ~p", [UserId]),
    %% 1) Delete all outstanding requests from this user
    Pat = #request{user_id = UserId, 
		   id = '$1', ref = '$2', mon = '$3', _ = '_'},
    Match = ets:match(snmpm_request_table, Pat),
    F1 = fun([ReqId, Ref, MonRef]) -> 
		 ets:delete(snmpm_request_table, ReqId),
		 erlang:cancel_timer(Ref),
		 erlang:demonitor(MonRef),
		 ok
	end,
    lists:foreach(F1, Match),
    
    %% 2) Unregister all agents registered by this user
    Agents = snmpm_config:which_agents(UserId),
    F2 = fun({Addr, Port}) ->
		 snmpm_config:unregister_agent(UserId, Addr, Port)
	 end,
    lists:foreach(F2, Agents),

    %% 3) Unregister the user
    Reply = snmpm_config:unregister_user(UserId),
    {reply, Reply, State};


%% We will reply to this request later, when the reply comes in from the
%% agent, or when the timeout hits (unless we get an error now).
handle_call({sync_get, Pid, UserId, Addr, Port, CtxName, Oids, Timeout}, From, State) ->
    ?vlog("received sync_get [~p] request", [CtxName]),
    case (catch handle_sync_get(Pid, 
				UserId, Addr, Port, CtxName, Oids, 
				Timeout, From, State)) of
	ok ->
	    {noreply, State};
	Error ->
	    {reply, Error, State}
    end;


handle_call({sync_get_next, Pid, UserId, Addr, Port, CtxName, Oids, Timeout}, From, State) ->
    ?vlog("received sync_get_next [~p] request", [CtxName]),
    case (catch handle_sync_get_next(Pid, 
				     UserId, Addr, Port, CtxName, Oids, 
				     Timeout, From, State)) of
	ok ->
	    {noreply, State};
	Error ->
	    {reply, Error, State}
    end;


%% Check agent version? This op not in v1
handle_call({sync_get_bulk, _Pid, _UserId, _Addr, _Port, 
	     _NonRep, _MaxRep, _CtxName, _Oids, _Timeout}, 
	    _From, State) ->
    ?vlog("received sync_get_bulk [~p] request", [_CtxName]),
    Reply = {error, {not_supported, sync_get_bulk}},
    {reply, Reply, State};
%     case (catch handle_sync_get_bulk(Pid, 
% 				     UserId, Addr, Port, NonRep, MaxRep, Oids, 
% 				     Timeout, From, State)) of
% 	ok ->
% 	    {noreply, State};
% 	Error ->
% 	    {reply, Error, State}
%     end;


handle_call({sync_set, Pid, UserId, Addr, Port, CtxName, VarsAndVals, Timeout}, From, State) ->
    ?vlog("received sync_set [~p] request", [CtxName]),
    case (catch handle_sync_set(Pid, 
				UserId, Addr, Port, CtxName, VarsAndVals, 
				Timeout, From, State)) of
	ok ->
	    {noreply, State};
	Error ->
	    {reply, Error, State}
    end;


handle_call({async_get, Pid, UserId, Addr, Port, CtxName, Oids, Expire}, 
	    _From, State) ->
    ?vlog("received async_get [~p] request", []),
    Reply = (catch handle_async_get(Pid, UserId, Addr, Port, CtxName, Oids, 
				    Expire, State)),
    {reply, Reply, State};


handle_call({async_get_next, Pid, UserId, Addr, Port, CtxName, Oids, Expire}, 
	    _From, State) ->
    ?vlog("received async_get_next [~p] request", [CtxName]),
    Reply = (catch handle_async_get_next(Pid, UserId, Addr, Port, CtxName, 
					 Oids, Expire, State)),
    {reply, Reply, State};


handle_call({async_set, Pid, UserId, Addr, Port, CtxName, VarsAndVals, Expire}, 
	    _From, State) ->
    ?vlog("received async_set [~p] request", [CtxName]),
    Reply = (catch handle_async_set(Pid, UserId, Addr, Port, CtxName, 
				    VarsAndVals, Expire, State)),
    {reply, Reply, State};


handle_call({cancel_async_request, UserId, ReqId}, _From, State) ->
    ?vlog("received cancel_async_request request", []),
    Reply = (catch handle_cancel_async_request(UserId, ReqId, State)),
    {reply, Reply, State};


handle_call({discovery, Pid, UserId, Addr, Port, Expire}, _From, State) ->
    ?vlog("received discovery request", []),
    Reply = (catch handle_discovery(Pid, UserId, Addr, Port, Expire, State)),
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
    handle_sync_timeout(ReqId, From),
    {noreply, State};


handle_info({snmp_error, Pdu, Reason}, State) ->
    ?vlog("received snmp_error message", []),
    handle_snmp_error(Pdu, Reason, State),
    {noreply, State};


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


handle_info({snmp_report, Pdu, Addr, Port}, State) ->
    handle_snmp_report(Pdu, Addr, Port, State),
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
    ?vlog("received 'DOWN' message regarding net_if", []),
    {ok, Prio} = snmpm_config:system_info(prio),
    {NoteStore, Ref} = do_init_note_store(Prio),
    Mod:note_store(NetIf, NoteStore),
    {noreply, State#state{note_store = NoteStore, note_store_ref = Ref}};


handle_info({'DOWN', MonRef, process, _Pid, _Reason}, State) ->
    ?vlog("received 'DOWN' message", []),
    handle_down(MonRef),
    {noreply, State};


handle_info(Info, State) ->
    info_msg("received unknown info: ~n~p", [Info]),
    {noreply, State}.


%%----------------------------------------------------------
%% Code change
%%----------------------------------------------------------
                                                                              
% downgrade
code_change({down, _Vsn}, #state{gct = GCT} = State, _Extra) ->
    gct_code_change(GCT),
    {ok, State};
 
% upgrade
code_change(_Vsn, #state{gct = GCT} = State, _Extra) ->
    gct_code_change(GCT),
    {ok, State}.
 
 
%%----------------------------------------------------------
%% Terminate
%%----------------------------------------------------------
                                                                              
terminate(_Reason, #state{gct = GCT}) ->
    gct_stop(GCT),
    snmpm_misc_sup:stop_note_store(),
    snmpm_misc_sup:stop_net_if(),
    ok.


%%----------------------------------------------------------------------
%% 
%%----------------------------------------------------------------------

handle_sync_get(Pid, UserId, Addr, Port, CtxName, Oids, Timeout, From, State) ->    
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
	    ReqId  = send_get_request(Oids, Vsn, MsgData, Addr, Port, State),
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
    

handle_sync_get_next(Pid, UserId, Addr, Port, CtxName, Oids, Timeout, From, State) ->
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
					   Addr, Port, State),
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
    

handle_sync_set(Pid, UserId, Addr, Port, CtxName, VarsAndVals, Timeout, From, State) ->
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
				      Addr, Port, State),
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

 
handle_async_get(Pid, UserId, Addr, Port, CtxName, Oids, Expire, State) ->
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
	    ReqId  = send_get_request(Oids, Vsn, MsgData, Addr, Port, State),
	    ?vdebug("handle_async_get -> ReqId: ~p", [ReqId]),
            MonRef = erlang:monitor(process, Pid),
	    ?vtrace("handle_async_get -> MonRef: ~p", [MonRef]),
	    Req    = #request{id      = ReqId,
			      user_id = UserId, 
			      addr    = Addr,
			      port    = Port,
			      type    = get, 
			      data    = MsgData, 
			      mon     = MonRef,
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


handle_async_get_next(Pid, UserId, Addr, Port, CtxName, Oids, Expire, State) ->
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
					   Addr, Port, State),
	    ?vdebug("handle_async_get_next -> ReqId: ~p", [ReqId]),
            MonRef = erlang:monitor(process, Pid),
	    ?vtrace("handle_async_get_next -> MonRef: ~p", [MonRef]),
	    Req    = #request{id      = ReqId,
			      user_id = UserId, 
			      addr    = Addr,
			      port    = Port,
			      type    = get_next, 
			      data    = MsgData, 
			      mon     = MonRef,
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


handle_async_set(Pid, UserId, Addr, Port, CtxName, VarsAndVals, Expire, State) ->
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
				      Addr, Port, State),
	    ?vdebug("handle_async_set -> ReqId: ~p", [ReqId]),
            MonRef = erlang:monitor(process, Pid),
	    ?vtrace("handle_async_set -> MonRef: ~p", [MonRef]),
	    Req    = #request{id      = ReqId,
			      user_id = UserId, 
			      addr    = Addr,
			      port    = Port,
			      type    = set, 
			      data    = MsgData, 
			      mon     = MonRef,
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
		  ref     = Ref, 
		  mon     = MonRef}] ->
	    ?vdebug("handle_cancel_async_request -> demonitor and cancel timer"
		    "~n   Monref: ~p"
		    "~n   Ref:    ~p", [MonRef, Ref]),
	    erlang:demonitor(MonRef),
	    erlang:cancel_timer(Ref),
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
    

handle_discovery(Pid, UserId, Addr, Port, Expire, State) ->
    ?vtrace("handle_discovery -> entry with"
	    "~n   Pid:         ~p"
	    "~n   UserId:      ~p"
	    "~n   Addr:        ~p"
	    "~n   Port:        ~p"
	    "~n   Expire:      ~p",
	    [Pid, UserId, Addr, Port, Expire]),
    case agent_data(default, default, "") of
	{ok, Vsn, MsgData} ->
	    ?vtrace("handle_discovery -> send a ~p disco message", [Vsn]),
	    ReqId  = send_discovery(Vsn, MsgData, Addr, Port, State),
	    ?vdebug("handle_discovery -> ReqId: ~p", [ReqId]),
	    MonRef = erlang:monitor(process, Pid),
	    ?vtrace("handle_discovery -> MonRef: ~p", [MonRef]),
	    Req    = #request{id        = ReqId,
			      user_id   = UserId, 
			      addr      = Addr, 
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
		   "~n   Addr:  ~p"
		   "~n   Port:  ~p"
		   "~n   Error: ~p", [Addr, Port, Error]),
	    Error
    end.


handle_sync_timeout(ReqId, From) ->
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
	    erlang:demonitor(MonRef),
	    
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
		case (catch erlang:cancel_timer(Ref)) of
		    R when integer(R) ->
			R;
		    _ ->
			0
		end,

	    ?vtrace("handle_snmp_error -> Remaining: ~p", [Remaining]),

	    erlang:demonitor(MonRef),
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
    end.


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
		case (catch erlang:cancel_timer(Ref)) of
		    R when integer(R) ->
			R;
		    _ ->
			0
		end,

	    ?vtrace("handle_snmp_pdu(get-response) -> Remaining: ~p", 
		    [Remaining]),

	    erlang:demonitor(MonRef),
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
		    "found corresponding request: "
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
			    handle_pdu(UserId, UserMod, Addr, Port, ReqId, 
				       SnmpInfo, UserData);
			_Error ->
			    %% Ouch, found an agent but not it's user!!
			    case snmpm_config:user_info() of
				{ok, DefUserId, DefMod, DefData} ->
				    handle_pdu(DefUserId, DefMod, Addr, Port, 
					       ReqId, SnmpInfo, DefData);
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
    end.

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
handle_snmp_trap(Trap, Addr, Port, _State) ->

    ?vtrace("handle_snmp_trap -> entry with"
	    "~n   Addr: ~p"
	    "~n   Port: ~p"
	    "~n   Trap: ~p", [Addr, Port, Trap]),

    SnmpTrapInfo = 
	case Trap of
	    #trappdu{enterprise    = Enteprise, 
		     generic_trap  = Generic, 
		     specific_trap = Spec,
		     time_stamp    = Timestamp, 
		     varbinds      = Varbinds} ->
		{Enteprise, Generic, Spec, Timestamp, Varbinds};
	    
	    #pdu{error_status = EStatus, 
		 error_index  = EIndex, 
		 varbinds     = Varbinds} ->
		{EStatus, EIndex, Varbinds}
	end,
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
				      [Addr, Port, Error, Trap])
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
			      [Addr, Port, Error, Trap])
	    end
    end,
    ok.

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
    
   
handle_snmp_inform(Pdu, Addr, Port, _State) ->
 
    ?vtrace("handle_snmp_inform -> entry with"
	    "~n   Addr: ~p"
	    "~n   Port: ~p"
	    "~n   Pdu:  ~p", [Addr, Port, Pdu]),

   #pdu{error_status = EStatus, 
	error_index  = EIndex, 
	varbinds     = Varbinds} = Pdu,
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
			  " ~n   ~p", [UserId, Error]),
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


    
handle_snmp_report(Pdu, Addr, Port, _State) ->
    #pdu{error_status = EStatus, 
 	 error_index  = EIndex, 
 	 varbinds     = Varbinds} = Pdu,
    SnmpReport = {EStatus, EIndex, Varbinds},
    case snmpm_config:get_agent_user_id(Addr, Port) of
 	{ok, UserId} ->
 	    case snmpm_config:user_info(UserId) of
 		{ok, Mod, Data} ->
 		    ?vdebug("[report] callback handle_inform with: "
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
    Pat = #request{id = '$1', ref = '$2', mon = MonRef, _ = '_'},
    Match = ets:match(snmpm_request_table, Pat),
    F = fun([ReqId, Ref]) -> 
		ets:delete(snmpm_request_table, ReqId),
		erlang:cancel_timer(Ref),
		ok
	end,
    lists:foreach(F, Match),
    ok.


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

send_get_request(Oids, Vsn, MsgData, Addr, Port, 
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
    (catch Mod:send_pdu(NetIf, Pdu, Vsn, MsgData, Addr, Port)),
    Pdu#pdu.request_id.

send_get_next_request(Oids, Vsn, MsgData, Addr, Port, 
		      #state{mini_mib   = MiniMIB, 
			     net_if     = NetIf, 
			     net_if_mod = Mod}) ->
    Pdu = make_pdu(get_next, Oids, MiniMIB),
    Mod:send_pdu(NetIf, Pdu, Vsn, MsgData, Addr, Port),
    Pdu#pdu.request_id.

% send_get_bulk_request(Oids, Vsn, MsgData, Addr, Port, 
% 		      NonRep, MaxRep, 
% 		      #state{mini_mib   = MiniMIB, 
% 			     net_if     = NetIf, 
% 			     net_if_mod = Mod}) ->
%     Pdu = make_pdu(bulk, {NonRep, MaxRep, Oids}, MiniMIB),
%     Mod:send_pdu(NetIf, Pdu, Vsn, MsgData, Addr, Port),
%     Pdu#pdu.request_id.

send_set_request(VarsAndVals, Vsn, MsgData, Addr, Port, 
		 #state{mini_mib   = MiniMIB,
			net_if     = NetIf, 
			net_if_mod = Mod}) ->
    Pdu = make_pdu(set, VarsAndVals, MiniMIB),
    Mod:send_pdu(NetIf, Pdu, Vsn, MsgData, Addr, Port),
    Pdu#pdu.request_id.

send_discovery(Vsn, MsgData, Addr, Port, 
	       #state{net_if     = NetIf, 
		      net_if_mod = Mod}) ->
    Pdu = make_discovery_pdu(),
    Mod:send_pdu(NetIf, Pdu, Vsn, MsgData, Addr, Port),
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

make_pdu(bulk, {NonRepeaters, MaxRepetitions, Oids}, MiniMIB) ->
    Foids = [flatten_oid(Oid, MiniMIB) || Oid <- Oids],
    #pdu{type         = 'get-bulk-request', 
	 request_id   = request_id(),
	 error_status = NonRepeaters, 
	 error_index  = MaxRepetitions,
	 varbinds     = [make_vb(Foid) || Foid <- Foids]};

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
	     variabletype = oid2type(Oid, MiniMIB),
	     value        = Value}.

char_to_type(o) ->
    'OBJECT IDENTIFIER';
char_to_type(i) ->
    'INTEGER';
char_to_type(u) ->
    'Unsigned32';
char_to_type(g) -> % Gauge, Gauge32
    'Unsigned32';
char_to_type(s) ->
    'OCTET STRING'.


alias2oid(AliasName, MiniMIB) when atom(AliasName) ->
    case lists:keysearch(AliasName, 2, MiniMIB) of
	{value, {Oid, _Aliasname, _Type}} -> 
 	    Oid;
	false -> 
 	    throw({error, {unknown_aliasname, AliasName}})
    end.

oid2type(Oid, MiniMIB) ->
    oid2type(Oid, MiniMIB, dummy).

oid2type(_Oid, [], Type) ->
    Type;
oid2type(Oid, [{Oid2, _, Type}|MiniMIB], Res) when Oid2 =< Oid ->
    case lists:prefix(Oid2, Oid) of
        true ->
            oid2type(Oid, MiniMIB, Type); % A better guess
        false ->
            oid2type(Oid, MiniMIB, Res)
    end;
oid2type(_Oid, _MiniMIB, Type) ->
    Type.
    

make_vb(Oid) ->
    #varbind{oid = Oid, variabletype = 'NULL', value = 'NULL'}.


%%----------------------------------------------------------------------

request_id() ->
    snmpm_mpd:next_req_id().


%%----------------------------------------------------------------------

agent_data(Addr, Port, CtxName) ->
    case snmpm_config:agent_info(Addr, Port, all) of
	{ok, Info} ->
	    {value, {_, Version}} = lists:keysearch(version, 1, Info),
	    MsgData = 
		case Version of
		    v3 ->
			TargetName = agent_data_item(target_name,  Info),
			EngineId   = agent_data_item(engine_id,    Info),
			SecModel   = agent_data_item(sec_model,    Info),
			SecName    = agent_data_item(sec_name,     Info),
			SecLevel   = agent_data_item(sec_level,    Info),
			{SecModel, SecName, SecLevel, 
			 EngineId, CtxName, TargetName};
		    _ ->
			Comm     = agent_data_item(community, Info),
			SecModel = agent_data_item(sec_model, Info),
			{Comm, SecModel}
		end,
	    {ok, version(Version), MsgData};
	Error ->
	    Error
    end.

agent_data_item(Item, Info) ->
    {value, {_, Val}} = lists:keysearch(Item, 1, Info),
    Val.

version(v1) ->
    'version-1';
version(v2) ->
    'version-2';
version(v3) ->
    'version-3'.


%%----------------------------------------------------------------------
%% Request Garbage Collector timer
%%----------------------------------------------------------------------

gct_start(Timeout) ->
    ?vdebug("start gc timer process (~p)", [Timeout]),    
    proc_lib:start_link(?MODULE, gct_init, [self(), Timeout]).

gct_stop(GCT) ->
    GCT ! {stop, self()}.

gct_activate(GCT) ->
    GCT ! {activate, self()}.

gct_deactivate(GCT) ->
    GCT ! {deactivate, self()}.

gct_code_change(GCT) ->
    GCT ! {code_change, self()}.

gct_init(Parent, Timeout) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    gct(Parent, idle, Timeout).

gct(Parent, active, Timeout) ->
    T = t(),
    receive
	{stop, Parent} ->
	    ok;

	%% This happens when a new request is received.
	{activate, Parent}  ->
	    NewTimeout = 
		case Timeout - (t() - T) of
		    NewT when NewT > 0 ->
			NewT;
		    _ ->
			0
		end,
	    ?MODULE:gct(Parent, active, NewTimeout); 
	{deactivate, Parent} ->
	    ?MODULE:gct(Parent, idle, Timeout);
	{code_change, Parent} ->
	    ?MODULE:gct(Parent, active, Timeout);
	{'EXIT', Parent, _Reason} ->
	    ok
    after Timeout ->
	    Parent ! gc_timeout,
	    ?MODULE:gct(Parent, active, Timeout)
    end;

gct(Parent, idle, Timeout) ->
    receive
	{stop, Parent} ->
	    ok;
	{activate, Parent} ->
	    ?MODULE:gct(Parent, active, Timeout);
	{code_change, Parent} ->
	    ?MODULE:gct(Parent, idle, Timeout);
	{'EXIT', Parent, _Reason} ->
	    ok
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

