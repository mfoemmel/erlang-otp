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
-module(snmp_agent).

-include("snmp_types.hrl").
-include("snmp_debug.hrl").
-include("snmp_verbosity.hrl").

%% External exports
-export([start_link/3, start_link/4, stop/1]).
-export([subagent_set/2, load_mibs/2, unload_mibs/2, info/1,
	 register_subagent/3, unregister_subagent/2,
	 send_trap/6, get_net_if/1]).
-export([debug/2, verbosity/2, dump_mibs/1, dump_mibs/2]).
-export([validate_err/3, make_value_a_correct_value/3, do_get/3, get/2]).

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, tr_var/2, tr_varbind/1,
	 handle_pdu/8, worker/2, worker_loop/1, do_send_trap/6]).

-ifndef(default_verbosity).
-define(default_verbosity,silence).
-endif.

-define(empty_pdu_size, 21).


%%-----------------------------------------------------------------
%% The agent is multi-threaded, i.e. each request is handled
%% by a separate process.  However, in the normal case, there
%% is just one request handled at the time.  In order to improve
%% performance, there is always two worker processes alive.  They are
%% created at initialization time.  There is always one worker
%% dedicated to SET-handling.  When a get*-request is received,
%% it is sent to the worker, and the worker is marked as busy.
%% If a request is received when the worker is busy, a new temporary
%% worker is spawned.
%% Code change
%% ===========
%% Note that the worker(s) execute the same module as the master
%% agent. For code change we have two options - ignore the workers,
%% or send them a code change message.
%%-----------------------------------------------------------------
-record(state, {type, parent, worker, worker_state = ready,
		set_worker, multi_threaded, ref, misc_sup, vsns}).

%%%-----------------------------------------------------------------
%%% This module implements the agent machinery; both for the master
%%% agent and the subagents.
%%%-----------------------------------------------------------------
%%% Table of contents
%%% =================
%%% 1. Interface
%%% 2. Main loop
%%% 3. GET REQUEST
%%% 4. GET-NEXT REQUEST
%%% 5. GET-BULK REQUEST
%%% 6. SET REQUEST
%%% 7. Misc functions
%%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Parent is a Pid (of master_agent) or none
%% Options is a list of Option, where Option is
%%   {mibs, Mibs}
%%   {net_if, NetIfModule}
%%   {priority, Prio}
%%   {verbosity, Verbosity}
%%   {multi_threaded, Bool} true means that SETs are serialized,
%%      while GETs are concurrent, even with a SET.
%%   {set_mechanism, SetModule}           % undocumented feature
%%   {verbosity, verbosity()}             % undocumented feature
%%
%% The following options are now removed - they are not needed
%% anymore when VACM is standard for authentication, and works
%% with all versions, and trap sending is standardized too.
%%   {authentication_service, AuthModule} % undocumented feature
%%   {trap_mechanism, TrapModule}         % undocumented feature
%% Note: authentication_service is reintroduced for AXD301 (OTP-3324).
%%-----------------------------------------------------------------
start_link(Parent, Ref, Options) ->
    ?debug("start_link -> ~n"
	   "Parent:  ~p~n"
	   "Ref:     ~p~n"
	   "Options: ~p",
	   [Parent, Ref, Options]),
    gen_server:start_link(?MODULE, [Parent, Ref, Options], []).
start_link(Name, Parent, Ref, Options) ->
    ?debug("start_link -> ~n"
	   "Name:    ~p~n"
	   "Parent:  ~p~n"
	   "Ref:     ~p~n"
	   "Options: ~p",
	   [Name, Parent, Ref, Options]),
    gen_server:start_link(Name, ?MODULE, [Parent, Ref, Options], []).

stop(Agent) -> gen_server:call(Agent, stop, infinity).

init([Parent, Ref, Options]) ->
    ?debug("init -> ~n"
	   "Parent:  ~p~n"
	   "Ref:     ~p~n"
	   "Options: ~p",
	   [Parent, Ref, Options]),
    put(sname,short_name(Parent)),
    put(verbosity,get_verbosity(Parent,Options)),
    ?vlog("starting",[]),
    Mibs = get_option(mibs, Options, []),
    MeOverride = get_option(mibentry_override, Options, false),
    TeOverride = get_option(trapentry_override, Options, false),
    MibsVerbosity = get_option(mibserver_verbosity, Options, silence),
    MibStorage = get_option(mib_storage, Options, ets),
    SetModule = get_option(set_mechanism, Options, snmp_set),
    put(set_module, SetModule),
    %% XXX OTP-3324. For AXD301.
    AuthModule = get_option(authentication_service, Options, snmp_acm),
    put(auth_module, AuthModule),
    Prio = get_option(priority, Options, normal),
    process_flag(priority, Prio),
    MultiT = get_option(multi_threaded, Options, false),
    MiscSup = get_option(misc_sup, Options, undefined),
    Vsns = snmp_misc:get_option(snmp_vsn, Options, [v1,v2,v3]),
    {Type, ParentPid, NetIfPid} =
	case Parent of
	    none -> 
		NetIf = get_option(net_if, Options, snmp_net_if),
		NiVerbosity = get_option(net_if_verbosity, Options, silence),
		NiRecBuf = get_option(net_if_recbuf, Options, use_default),
		NiOpts = [{net_if_verbosity,NiVerbosity},
			  {net_if_recbuf,NiRecBuf}],
		?debug("init -> start net if",[]),
		?vdebug("start net if",[]),
		case snmp_misc_sup:start_net_if(MiscSup,Ref,self(),
						NetIf,NiOpts) of
		    {ok, Pid} -> {master_agent, undefined, Pid};
		    {error, Reason} -> exit(Reason)
		end;
	    Pid when pid(Pid) -> 
		{subagent, Pid, undefined}
	end,
    ?debug("init -> start mib server",[]),
    ?vdebug("start mib server (~p,~p,~p,~p)",
	    [MeOverride,TeOverride,MibsVerbosity,MibStorage]),
    MibsOpts = [{mibentry_override,MeOverride},
		{trapentry_override,TeOverride},
		{mibserver_verbosity,MibsVerbosity},
		{mib_storage,MibStorage}],
    case snmp_misc_sup:start_mib(MiscSup, Ref, Mibs, Prio, MibsOpts) of
	{ok, MibPid} ->
	    put(mibserver, MibPid),
	    process_flag(trap_exit, true),
	    put(net_if, NetIfPid),
	    {Worker, SetWorker} =
		case MultiT of
		    true ->
			?debug("start worker and set-worker",[]),
			?vdebug("start worker and set-worker",[]),
			{proc_lib:spawn_link(?MODULE,worker,[self(),get()]),
			 proc_lib:spawn_link(?MODULE,worker,[self(),get()])};
		    _ -> 
			{undefined, undefined}
		end,
	    ?debug("init -> started",[]),
	    ?vdebug("started",[]),
	    {ok, #state{type = Type, parent = Parent, worker = Worker,
			set_worker = SetWorker,
			multi_threaded = MultiT, ref = Ref,
			misc_sup = MiscSup, vsns = Vsns}};
	{error, Reason2} ->
	    ?debug("init -> error: ~p",[Reason2]),
	    ?vlog("~n   failed starting mib: ~p",[Reason2]),
	    {stop, Reason2}
    end.

%%-----------------------------------------------------------------
%% Purpose: We must calculate the length of an empty Pdu.  This
%%          length is used to calculate the max pdu size allowed
%%          for each get-bulk-request. This size is 
%%          dependent on the varbinds. It is calculated
%%          as EmptySize + 8.  8 comes from the fact that the
%%          maximum pdu size needs 31 bits which needs 5 * 7 bits to be
%%          expressed. One 7bit octet is already present in the
%%          empty pdu, leaving 4 more 7bit octets. The length is
%%          repeated twice, once for the varbinds, and once for the
%%          entire pdu; 2 * 4 = 8.
%% Actually, this function is not used, we use a constant instead.
%%-----------------------------------------------------------------
%% Ret: 21
%empty_pdu() ->
%    Pdu = #pdu{type = 'get-response', request_id = 1,
%	       error_status = noError, error_index = 0, varbinds = []},
%    length(snmp_pdus:enc_pdu(Pdu)) + 8.


%%%--------------------------------------------------
%%% 1. Interface
%%%--------------------------------------------------
%% Called by administrator (not subagent; deadlock could occur)
register_subagent(Agent, SubTreeOid, SubagentPid) ->
    gen_server:call(Agent, {register_subagent, SubTreeOid, SubagentPid},
		    infinity).

%% Called by administrator (not subagent; deadlock could occur)
unregister_subagent(Agent, SubagentOidOrPid) ->
    gen_server:call(Agent, {unregister_subagent, SubagentOidOrPid}, infinity).

%%-----------------------------------------------------------------
%% These subagent_ functions either return a value, or exits
%% with {nodedown, Node} | Reason.
%%-----------------------------------------------------------------
subagent_get(SubAgent, Varbinds, IsNotification) ->
    PduData = get_pdu_data(),
    gen_server:call(SubAgent, {subagent_get, Varbinds, PduData, IsNotification},
		    infinity).

subagent_get_next(SubAgent, MibView, Varbinds) ->
    PduData = get_pdu_data(),
    gen_server:call(SubAgent, {subagent_get_next, MibView, Varbinds, PduData},
		   infinity).

subagent_set(SubAgent, Arguments) ->
    PduData = get_pdu_data(),
    gen_server:call(SubAgent, {subagent_set, Arguments, PduData}, infinity).

%% Called by administrator (not agent; deadlock would occur)
load_mibs(Agent, Mibs) ->
    gen_server:call(Agent, {load_mibs, Mibs}, infinity).

%% Called by administrator (not agent; deadlock would occur)
unload_mibs(Agent, Mibs) ->
    gen_server:call(Agent, {unload_mibs, Mibs}, infinity).

info(Agent) ->
    gen_server:call(Agent, info, infinity).

get_net_if(Agent) ->
    gen_server:call(Agent, get_net_if, infinity).

send_trap(Agent, Trap, NotifyName, ContextName, Recv, Varbinds) ->
    Agent ! {send_trap, Trap, NotifyName, ContextName, Recv, Varbinds}.

forward_trap(Agent, TrapRecord, NotifyName, ContextName, Recv, Varbinds) ->
    Agent ! {forward_trap, TrapRecord, NotifyName, ContextName, Recv, Varbinds}.

%%-----------------------------------------------------------------
%% Args: Vars = [Oid]
%% Returns: [Value]
%% Called from a program to get variables.  Don't call this from
%% an instrumentation function; deadlock can occur!
%%-----------------------------------------------------------------
get(Agent, Vars) -> gen_server:call(Agent, {get, Vars}, infinity).

%%-----------------------------------------------------------------
%% Runtime debug support.  When Flag is true, the agent prints info
%% when a packet is receive/sent, and when a user defined function
%% is called.
%%
%% This is kept for backward compatibillity reasons, see verbosity
%%-----------------------------------------------------------------
debug(Agent, Flag) -> gen_server:call(Agent, {debug, Flag}, infinity).

dump_mibs(Agent) -> 
    gen_server:call(Agent, dump_mibs, infinity).
dump_mibs(Agent,File) when list(File) -> 
    gen_server:call(Agent,{dump_mibs,File}, infinity).


%%-----------------------------------------------------------------
%% Runtime debug (verbosity) support.
%%-----------------------------------------------------------------
verbosity(snmp_net_if,Verbosity) -> 
    gen_server:cast(snmp_master_agent,{snmp_net_if_verbosity,Verbosity});
verbosity(snmp_mib,Verbosity) -> 
    gen_server:cast(snmp_master_agent,{snmp_mib_verbosity,Verbosity});
verbosity(snmp_sub_agents,Verbosity) -> 
    gen_server:cast(snmp_master_agent,{snmp_sub_agents_verbosity,Verbosity});
verbosity(Agent,{snmp_sub_agents,Verbosity}) -> 
    gen_server:cast(Agent,{snmp_sub_agents_verbosity,Verbosity});
verbosity(Agent,Verbosity) -> 
    gen_server:cast(Agent,{verbosity,Verbosity}).

%%%--------------------------------------------------
%%% 2. Main loop
%%%--------------------------------------------------
handle_info({snmp_pdu, Vsn, Pdu, PduMS, ACMData, Address, Extra}, S) ->
    ?vdebug("~n   Received PDU ~p"
	    "~n   from ~p", [Pdu,Address]),
    %% XXX OTP-3324
    AuthMod = get(auth_module),
    case AuthMod:init_check_access(Pdu, ACMData) of
	{ok, MibView, ContextName} ->
	    AgentData = cheat(ACMData, Address, ContextName),
	    case valid_pdu_type(Pdu#pdu.type) of
		true when S#state.multi_threaded == false ->
		    % Execute in same process
		    ?vtrace("execute in the same process",[]),
		    handle_pdu(MibView, Vsn, Pdu, PduMS, 
			       ACMData, AgentData, Extra),
		    {noreply, S};
		true when Pdu#pdu.type == 'set-request' ->
		    % Always send to main worker, in order to serialize
		    % the SETs
		    ?vtrace("send set-request to main worker",[]),
		    S#state.set_worker !
			{MibView, Vsn, Pdu, PduMS, ACMData, AgentData, Extra},
		    {noreply, S#state{worker_state = busy}};
		true when S#state.worker_state == busy ->
		    % Main worker busy => create new worker
		    ?vtrace("main worker busy -> crete new worker",[]),
		    spawn_thread(MibView, Vsn, Pdu, PduMS,
				 ACMData, AgentData, Extra),
		    {noreply, S};
		true ->
		    % Send to main worker
		    ?vtrace("send to main worker",[]),
		    S#state.worker !
			{MibView, Vsn, Pdu, PduMS, ACMData, AgentData, Extra},
		    {noreply, S#state{worker_state = busy}};
		_ -> 
		    {noreply, S}
	    end;
	{error, Reason} ->
	    ?vlog("~n   auth init check failed: ~p", [Reason]),
	    handle_acm_error(Vsn, Reason, Pdu, ACMData, Address, Extra),
	    {noreply, S};
	{discarded, Variable, Reason} ->
	    ?vlog("~n   PDU discarded for reason: ~p", [Reason]),
	    get(net_if) ! {discarded_pdu, Vsn, Pdu#pdu.request_id,
			   ACMData, Variable, Extra},
	    {noreply, S}
    end;

handle_info(worker_available, S) ->
    ?vdebug("worker available",[]),
    {noreply, S#state{worker_state = ready}};

handle_info({send_trap, Trap, NotifyName, ContextName, Recv, Varbinds}, S) ->
    ?vlog("send trap request:"
	  "~n   Trap:        ~p"
	  "~n   NotifyName:  ~p"
	  "~n   ContextName: ~p"
	  "~n   Recv:        ~p" 
	  "~n   Varbinds:    ~p", 
	  [Trap,NotifyName,ContextName,Recv,Varbinds]),
    case catch handle_send_trap(S, Trap, NotifyName, ContextName,
				Recv, Varbinds) of
	{ok, NewS} ->
	    {noreply, NewS};
	{'EXIT', R} ->
	    ?vinfo("Trap not sent:~n   ~p", [R]),
	    {noreply, S};
	_ ->
	    {noreply, S}
    end;

handle_info({forward_trap, TrapRecord, NotifyName, ContextName,
	     Recv, Varbinds},S) ->
    ?vlog("forward trap request:"
	  "~n   TrapRecord:  ~p"
	  "~n   NotifyName:  ~p"
	  "~n   ContextName: ~p"
	  "~n   Recv:        ~p"
	  "~n   Varbinds:    ~p", 
	  [TrapRecord,NotifyName,ContextName,Recv,Varbinds]),
    case catch handle_forward_trap(S, TrapRecord, NotifyName, ContextName,
				   Recv, Varbinds) of
	{ok, NewS} ->
	    {noreply, NewS};
	{'EXIT', R} ->
	    ?vinfo("Trap not sent:~n   ~p", [R]),
	    {noreply, S};
	_ ->
	    {noreply, S}
    end;

%%-----------------------------------------------------------------
%% If a process crashes, we first check to see if it was the mib.
%% Otherwise, we check to see if it was a subagent. In this case
%% we unregister the sa, and unlink us from the sa.
%%-----------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, S) ->
    ?vlog("~p exited for reason ~p", [Pid,Reason]),
    Mib = get(mibserver),
    NetIf = get(net_if),
    case Pid of
	Mib ->
	    exit(Reason);
	NetIf ->
	    exit(Reason);
	Worker when S#state.worker == Worker -> 
	    ?vtrace("was a worker -> create new", []),
	    NewWorker =
		proc_lib:spawn_link(?MODULE, worker, [self(), get()]),
	    {noreply, S#state{worker = NewWorker, worker_state = ready}};
	Worker when S#state.set_worker == Worker -> 
	    ?vtrace("was a set-worker -> create new", []),
	    NewWorker =
		proc_lib:spawn_link(?MODULE, worker, [self(), get()]),
	    {noreply, S#state{set_worker = NewWorker}};
	Parent when Parent == S#state.parent ->
	    ?vlog("parent died", []),
	    {stop, {parent_died, Reason}, S};
	_ ->
	    SAs = snmp_mib:info(Mib, subagents),
	    case lists:keysearch(Pid, 1, SAs) of
		{value, _} ->
		    ?vlog("subagent", []),
		    snmp_mib:unregister_subagent(Mib, Pid),
		    unlink(Pid);
		_ -> 
		    %% Otherwise it was a probably a worker thread - ignore
		    ok
	    end,
	    {noreply, S}
    end;
handle_info(_, S) ->
    {noreply, S}.

handle_call({subagent_get, Varbinds, PduData, IsNotification}, _From, S) ->
    ?vlog("subagent get:"
	  "~n   Varbinds: ~p"
	  "~n   PduData:  ~p", 
	  [Varbinds,PduData]),
    put_pdu_data(PduData),
    {reply, do_get(Varbinds, IsNotification), S};
handle_call({subagent_get_next, MibView, Varbinds, PduData}, _From, S) ->
    ?vlog("subagent get-next:"
	  "~n   MibView:  ~p"
	  "~n   Varbinds: ~p"
	  "~n   PduData:  ~p", 
	  [MibView,Varbinds,PduData]),
    put_pdu_data(PduData),
    {reply, do_get_next(MibView, Varbinds), S};
handle_call({subagent_set, Arguments, PduData}, _From, S) ->
    ?vlog("subagent set:"
	  "~n   Arguments: ~p"
	  "~n   PduData:   ~p", 
	  [Arguments,PduData]),
    put_pdu_data(PduData),
    {reply, do_subagent_set(Arguments), S};

handle_call({get, Vars}, _From, S) ->
    ?vlog("get:"
	  "~n   Vars: ~p",[Vars]),
    case catch mapfoldl({?MODULE, tr_var}, [], 1, Vars) of
	{error, Reason} -> {reply, {error, Reason}, S};
	{_, Varbinds} ->
	    ?vdebug("Varbinds: ~p",[Varbinds]),
	    Reply =
		case do_get(Varbinds, false) of
		    {noError, 0, NewVarbinds} ->
			ResVarbinds = lists:keysort(#varbind.org_index,
						    NewVarbinds),
			snmp_misc:map({?MODULE, tr_varbind}, [], ResVarbinds);
		    {ErrorStatus, ErrIndex, _} ->
			N = lists:nth(ErrIndex, Vars),
			{error, {ErrorStatus, N}}
		end,
	    {reply, Reply, S}
    end;

handle_call({register_subagent, SubTreeOid, SubagentPid}, _From, S) ->
    Reply = 
	case snmp_mib:register_subagent(get(mibserver),
					SubTreeOid, SubagentPid) of
	    ok -> link(SubagentPid), ok;
	    Error -> Error
	end,
    {reply, Reply, S};

handle_call({unregister_subagent, SubagentPid}, _From, S) 
  when pid(SubagentPid) ->
    ?vlog("unregister subagent ~p", [SubagentPid]),
    Reply = snmp_mib:unregister_subagent(get(mibserver), SubagentPid),
    unlink(SubagentPid),
    {reply, Reply, S};

handle_call({unregister_subagent, SubTreeOid}, _From, S) ->
    ?vlog("unregister subagent ~p", [SubTreeOid]),
    Reply = 
	case snmp_mib:unregister_subagent(get(mibserver), SubTreeOid) of
	    {ok, DeletedSubagentPid} ->
		SAs = snmp_mib:info(get(mibserver), subagents),
		case lists:keysearch(DeletedSubagentPid, 1, SAs) of
		    {value, _} -> ok;
		    _ -> unlink(DeletedSubagentPid)
		end,
		ok;
	    Error ->
		Error
	end,
    {reply, Reply, S};

handle_call({load_mibs, Mibs}, _From, S) ->
    ?vlog("load mibs ~p", [Mibs]),
    {reply, snmp_mib:load_mibs(get(mibserver), Mibs), S};

handle_call({unload_mibs, Mibs}, _From, S) ->
    ?vlog("unload mibs ~p", [Mibs]),
    {reply, snmp_mib:unload_mibs(get(mibserver), Mibs), S};

handle_call(info, _From, S) ->
    {reply, [{vsns, S#state.vsns} | snmp_mib:info(get(mibserver))], S};

handle_call(get_net_if, _From, S) ->
    {reply, get(net_if), S};

handle_call({debug, Flag}, _From, S) ->
    V = d2v(Flag),
    put(verbosity,V),
    net_if_verbosity(get(net_if),V),
    case S#state.worker of
	Pid when pid(Pid) -> Pid ! {verbosity,V};
	_ -> ok
    end,
    case S#state.set_worker of
	Pid2 when pid(Pid2) -> Pid2 ! {verbosity,V};
	_ -> ok
    end,
    {reply, ok, S};

handle_call(dump_mibs, _From, S) ->
    Reply = snmp_mib:dump(get(mibserver)),
    {reply, Reply, S};
    
handle_call({dump_mibs,File}, _From, S) ->
    Reply = snmp_mib:dump(get(mibserver),File),
    {reply, Reply, S};
    
handle_call(stop, _From, S) ->
    {stop, normal, ok, S}.

handle_cast({verbosity,Verbosity}, S) ->
    ?vlog("verbosity: ~p -> ~p",[get(verbosity),Verbosity]),
    put(verbosity,snmp_verbosity:validate(Verbosity)),
    case S#state.worker of
	Pid when pid(Pid) -> Pid ! {verbosity,Verbosity};
	_ -> ok
    end,
    case S#state.set_worker of
	Pid2 when pid(Pid2) -> Pid2 ! {verbosity,Verbosity};
	_ -> ok
    end,
    {noreply, S};
    
handle_cast({snmp_sub_agents_verbosity,Verbosity}, S) ->
    ?vlog("subagent verbosity: ~p",[Verbosity]),
    subagents_verbosity(Verbosity),
    {noreply, S};
    
%% This should only happen if we are a master_agent
handle_cast({snmp_net_if_verbosity,Verbosity}, S) ->
    net_if_verbosity(get(net_if),Verbosity),
    {noreply, S};
    
handle_cast({snmp_mib_verbosity,Verbosity}, S) ->
    mib_verbosity(get(mibserver),Verbosity),
    {noreply, S};
    
handle_cast(_, S) ->
    {noreply, S}.
    
terminate(shutdown, #state{ref = Ref, misc_sup = MiscSup}) ->
    %% Ordered shutdown - stop mib and net_if.
    snmp_misc_sup:stop_mib(MiscSup, Ref),
    snmp_misc_sup:stop_net_if(MiscSup, Ref);
terminate(_Reason, _S) ->
    %% We crashed!  We will reuse net_if and mib if we get restarted.
    ok.

%%-----------------------------------------------------------------
%% Code replacement
%% 
%%-----------------------------------------------------------------

%% Upgrade
%%
code_change(_Vsn, S, _Extra) ->
    NS = worker_restart(S),
    {ok, NS};

%% Downgrade
%%
code_change({down, _Vsn}, S, _Extra) ->
    NS = worker_restart(S),
    {ok, NS}.


worker_restart(S) ->
    Worker    = restart_worker(S#state.worker),
    SetWorker = restart_worker(S#state.set_worker),
    S#state{worker = Worker, set_worker = SetWorker}.

restart_worker(Pid) when pid(Pid) -> 
    Pid ! terminate, 
    receive 
	{'EXIT', Pid, normal} ->
	    ok
    end,
    proc_lib:spawn_link(?MODULE,worker,[self(),get()]);
restart_worker(Any) ->
    Any.

%%-----------------------------------------------------------------
%% We must cheat to get the community string out of the ACM data,
%% because we (for some reason) support the function
%% snmp:current_community().
%%-----------------------------------------------------------------
cheat({community, _SecModel, Community, _IpUdp}, Address, ContextName) ->
    {Community, Address, ContextName};
cheat(_, Address, ContextName) ->
    {"", Address, ContextName}.

%%-----------------------------------------------------------------
%% Threads and workers
%% 
%%-----------------------------------------------------------------

spawn_thread(MibView, Vsn, Pdu, PduMS, ACMData, AgentData, Extra) ->
    Dict = get(),
    proc_lib:spawn_link(?MODULE, handle_pdu,
			[MibView, Vsn, Pdu, PduMS, ACMData,
			 AgentData, Extra, Dict]).

spawn_trap_thread(TrapRec, NotifyName, ContextName, Recv, V) ->
    Dict = get(),
    proc_lib:spawn_link(?MODULE, do_send_trap,
			[TrapRec, NotifyName, ContextName, Recv, V, Dict]).

do_send_trap(TrapRec, NotifyName, ContextName, Recv, V, Dict) ->
    lists:foreach(fun({Key, Val}) -> put(Key, Val) end, Dict),
    put(sname,trap_sender_short_name(get(sname))),
    ?vlog("starting",[]),
    snmp_trap:send_trap(TrapRec, NotifyName, ContextName, Recv, V, get(net_if)).


worker(Master, Dict) ->
    lists:foreach(fun({Key, Val}) -> put(Key, Val) end, Dict),
    put(sname,worker_short_name(get(sname))),
    ?vlog("starting",[]),
    worker_loop(Master).

worker_loop(Master) ->
    receive
	{MibView, Vsn, Pdu, PduMS, ACMData, AgentData, Extra} ->
	    handle_pdu(MibView, Vsn, Pdu, PduMS, ACMData, AgentData, Extra),
	    Master ! worker_available;
	{TrapRec, NotifyName, ContextName, Recv, V} -> % We don't trap exits!
	    ?vtrace("send trap:~n   ~p",[TrapRec]),
	    snmp_trap:send_trap(TrapRec, NotifyName, 
				ContextName, Recv, V, get(net_if)),
	    Master ! worker_available;
	{verbosity, Verbosity} ->
	    put(verbosity,snmp_verbosity:validate(Verbosity));
	terminate ->
	    exit(normal);
	_X ->
	    %% ignore
	    ok
    after 30000 ->
	    %% This is to assure that the worker process leaves a
	    %% possibly old version of this module.
	    ok
    end,
    ?MODULE:worker_loop(Master).


handle_pdu(MibView, Vsn, Pdu, PduMS, ACMData, AgentData, Extra, Dict) ->
    lists:foreach(fun({Key, Val}) -> put(Key, Val) end, Dict),
    put(sname,pdu_handler_short_name(get(sname))),
    ?vlog("starting",[]),
    handle_pdu(MibView, Vsn, Pdu, PduMS, ACMData, AgentData, Extra).

handle_pdu(MibView, Vsn, Pdu, PduMS, ACMData,
	   {Community, Address, ContextName}, Extra) ->
    put(snmp_net_if_data, Extra),
    RePdu = process_msg(MibView, Vsn, Pdu, PduMS, Community, 
			Address, ContextName),
    ?vtrace("reply PDU:~n   ~p",[RePdu]),
    get(net_if) ! {snmp_response, Vsn, RePdu, 
		   RePdu#pdu.type, ACMData, Address, Extra}.


handle_acm_error(Vsn, Reason, Pdu, ACMData, Address, Extra) ->
    #pdu{type = Type, request_id = ReqId, varbinds = Vbs} = Pdu,
    RawErrStatus = snmp_acm:error2status(Reason),
    case valid_pdu_type(Type) of
	true ->
	    %% RawErrStatus can be authorizationError or genErr.  If it is
	    %% authorizationError, we'll have to do different things, 
	    %% depending on which SNMP version is used.
	    %% v1 - noSuchName error
	    %% v2 - GET: all variables 'noSuchObject'
	    %%      NEXT/BULK: all variables 'endOfMibView'
	    %%      SET: noAccess error
	    %% v3 - authorizationError error
	    %%
	    %% NOTE: this procedure is not yet defined in the coex document!
	    ?vdebug("~n   Raw error status: ~w",[RawErrStatus]),
	    Idx = case Vbs of
		      [] -> 0;
		      _ -> 1
		  end,
	    RePdu =
		if
		    Vsn == 'version-1' ->
			ErrStatus = v2err_to_v1err(RawErrStatus),
			make_response_pdu(ReqId, ErrStatus, Idx, Vbs, Vbs);
		    Vsn == 'version-3' ->
			make_response_pdu(ReqId, RawErrStatus, Idx, Vbs, Vbs);
		    Type == 'get-request' ->  % this is v2
			ReVbs = lists:map(
				  fun(Vb) -> Vb#varbind{value=noSuchObject} end,
				  Vbs),
			make_response_pdu(ReqId, noError, 0, Vbs, ReVbs);
		    Type == 'set-request' ->
			make_response_pdu(ReqId, noAccess, Idx, Vbs, Vbs);
		    true -> % next or bulk
			ReVbs = lists:map(
				  fun(Vb) -> Vb#varbind{value=endOfMibView} end,
				  Vbs),
			make_response_pdu(ReqId, noError, 0, Vbs, ReVbs)
		end,
	    get(net_if) ! {snmp_response, Vsn, RePdu, 
			   'get-response', ACMData, Address, Extra};
	false ->
	    ?vdebug("~n   Raw error status: ~w"
		    "~n   invalid pdu type: ~w", 
		    [RawErrStatus,Type]),
	    ok
    end.


handle_send_trap(S, TrapName, NotifyName, ContextName, Recv, Varbinds) ->
    case snmp_trap:construct_trap(TrapName, Varbinds) of
	{ok, TrapRecord, VarList} ->
	    case S#state.type of
		subagent ->
		    forward_trap(S#state.parent, TrapRecord, NotifyName,
				 ContextName, Recv, VarList),
		    {ok, S};
		master_agent ->
		    handle_forward_trap(S, TrapRecord, NotifyName,
					ContextName, Recv, VarList)
	    end;
	error ->
	    error
    end.
				
handle_forward_trap(S, TrapRec, NotifyName, ContextName, Recv, Varbinds) ->
    V = snmp_trap:try_initialise_vars(get(mibserver), Varbinds),
    case S#state.type of
	subagent ->
	    forward_trap(S#state.parent, TrapRec, NotifyName, ContextName,
			 Recv, V),
	    {ok, S};
	master_agent when S#state.multi_threaded == false ->
	    ?vtrace("send trap:~n   ~p",[TrapRec]),
	    snmp_trap:send_trap(TrapRec, NotifyName, ContextName,
				Recv, V, get(net_if)),
	    {ok, S};
	master_agent when S#state.worker_state == busy ->
	    %% Main worker busy => create new worker
	    ?vtrace("~n   main worker busy -> spawn a trap sender",[]),
	    spawn_trap_thread(TrapRec, NotifyName, ContextName, Recv, V),
	    {ok, S};
	master_agent ->
	    %% Send to main worker
	    ?vtrace("~n   send to main worker",[]),
	    S#state.worker ! {TrapRec, NotifyName, ContextName, Recv, V},
	    {ok, S#state{worker_state = busy}}
    end.
    
%%-----------------------------------------------------------------
%% Func: process_msg/7
%% Returns: RePdu
%%-----------------------------------------------------------------
process_msg(MibView, Vsn, Pdu, PduMS, Community, {Ip, Udp}, ContextName) ->
    #pdu{request_id = ReqId} = Pdu,
    put(snmp_address, {tuple_to_list(Ip), Udp}),
    put(snmp_request_id, ReqId),
    put(snmp_community, Community),
    put(snmp_context, ContextName),
    ?vtrace("process ~p",[Pdu#pdu.type]),
    process_pdu(Pdu, PduMS, Vsn, MibView).

process_pdu(#pdu{type='get-request', request_id = ReqId, varbinds=Vbs},
	    _PduMS, Vsn, MibView) ->
    ?vtrace("get ~p",[ReqId]),
    Res = get_err(do_get(MibView, Vbs, false)),
    ?vtrace("get result: "
	    "~n   ~p",[Res]),
    {ErrStatus, ErrIndex, ResVarbinds} =
	if
	    Vsn == 'version-1' -> validate_get_v1(Res);
	    true -> Res
	end,
    ?vtrace("get final result: "
	    "~n   Error status: ~p"
	    "~n   Error index:  ~p"
	    "~n   Varbinds:     ~p",
	    [ErrStatus,ErrIndex,ResVarbinds]),
    ResponseVarbinds = lists:keysort(#varbind.org_index, ResVarbinds),
    ?vtrace("response varbinds: "
	    "~n   ~p",[ResponseVarbinds]),
    make_response_pdu(ReqId, ErrStatus, ErrIndex, Vbs, ResponseVarbinds);

process_pdu(#pdu{type = 'get-next-request',request_id = ReqId,varbinds = Vbs},
	    _PduMS, Vsn, MibView) ->
    Res = get_err(do_get_next(MibView, Vbs)),
    ?vtrace("get-next result: "
	    "~n   ~p",[Res]),
    {ErrStatus, ErrIndex, ResVarbinds} = 
	if
	    Vsn == 'version-1' -> validate_next_v1(Res, MibView);
	    true -> Res
	end,
    ?vtrace("get-next final result: "
	    "~n   Error status: ~p"
	    "~n   Error index:  ~p"
	    "~n   Varbinds:     ~p",[ErrStatus,ErrIndex,ResVarbinds]),
    ResponseVarbinds = lists:keysort(#varbind.org_index, ResVarbinds),
    ?vtrace("response varbinds: "
	    "~n   ~p",[ResponseVarbinds]),
    make_response_pdu(ReqId, ErrStatus, ErrIndex, Vbs, ResponseVarbinds);

process_pdu(#pdu{type = 'get-bulk-request',request_id = ReqId,varbinds = Vbs,
		 error_status = NonRepeaters, error_index = MaxRepetitions},
	    PduMS, _Vsn, MibView)->
    {ErrStatus, ErrIndex, ResponseVarbinds} = 
	get_err(do_get_bulk(MibView,NonRepeaters,MaxRepetitions,PduMS,Vbs)),
    ?vtrace("get-bulk final result: "
	    "~n   Error status:     ~p"
	    "~n   Error index:      ~p"
	    "~n   Respons varbinds: ~p",
	    [ErrStatus,ErrIndex,ResponseVarbinds]),
    make_response_pdu(ReqId, ErrStatus, ErrIndex, Vbs, ResponseVarbinds);

process_pdu(#pdu{type = 'set-request', request_id = ReqId, varbinds = Vbs},
	    _PduMS, Vsn, MibView)->
    Res = do_set(MibView, Vbs),
    ?vtrace("set result: "
	    "~n   ~p",[Res]),
    {ErrStatus, ErrIndex} =
	if 
	    Vsn == 'version-1' -> validate_err(v2_to_v1, Res);
	    true -> Res
	end,
    ?vtrace("set final result: "
	    "~n   Error status: ~p"
	    "~n   Error index:  ~p",[ErrStatus,ErrIndex]),
    make_response_pdu(ReqId, ErrStatus, ErrIndex, Vbs, Vbs).

%%-----------------------------------------------------------------
%% Transform a value == noSuchInstance | noSuchObject or a 
%% Counter64 type to a noSuchName error for the whole pdu.
%% Args: {Error, Index, Vbs}
%% Returns: {NewError, NewIndex, NewVbs}
%%-----------------------------------------------------------------
validate_get_v1({noError, _, ResponseVarbinds}) ->
    case validate_get_v1_2(ResponseVarbinds) of
	true -> {noError, 0, ResponseVarbinds};
	{Error, Index} -> {Error, Index, []} % dummy vbs
    end;
validate_get_v1({ErrStatus, ErrIndex, ResponseVarbinds}) ->
    {v2err_to_v1err(ErrStatus), ErrIndex, ResponseVarbinds}.

validate_get_v1_2([Vb | Vbs]) when Vb#varbind.value /= noSuchInstance,
				   Vb#varbind.value /= noSuchObject,
				   Vb#varbind.variabletype /= 'Counter64' ->
    validate_get_v1_2(Vbs);
validate_get_v1_2([Vb | Vbs]) ->
    {noSuchName, Vb#varbind.org_index};
validate_get_v1_2([]) ->
    true.

%%-----------------------------------------------------------------
%% Transform a value == endOfMibView to a noSuchName for the
%% whole pdu, and do another get-next for any Counter64 value.
%% Args: {Error, Index, Vbs}
%% Returns: {NewError, NewIndex, NewVbs}
%%-----------------------------------------------------------------
validate_next_v1({noError, _, ResponseVarbinds}, MibView) ->
    case validate_next_v1_2(ResponseVarbinds, MibView, []) of
	{true, NVbs} -> {noError, 0, NVbs};
	{Error, Index} -> {Error, Index, []} % dummy vbs
    end;
validate_next_v1({ErrStatus, ErrIndex, ResponseVarbinds}, _MibView) ->
    {v2err_to_v1err(ErrStatus), ErrIndex, ResponseVarbinds}.

validate_next_v1_2([Vb | Vbs], MibView, _Res)
  when Vb#varbind.value == endOfMibView ->
    {noSuchName, Vb#varbind.org_index};
validate_next_v1_2([Vb | Vbs], MibView, Res)
  when Vb#varbind.variabletype == 'Counter64' ->
    case validate_next_v1(do_get_next(MibView, [mk_next_oid(Vb)]), MibView) of
	{noError, 0, [NVb]} ->
	    validate_next_v1_2(Vbs, MibView, [NVb | Res]);
	{Error, Index, _OrgVb} ->
	    {Error, Index}
    end;
validate_next_v1_2([Vb | Vbs], MibView, Res) ->
    validate_next_v1_2(Vbs, MibView, [Vb | Res]);
validate_next_v1_2([], _MibView, Res) ->
    {true, Res}.

%%-----------------------------------------------------------------
%% Optimization. When we get to a Counter64 object that is a table
%% column, we'll try to find the next instance. This will be the
%% next row in the table, which is a Counter64 value as well. This
%% means that we will loop through the entire table, until we find
%% a column that isn't a Counter64 column. We can optimze this by
%% adding 1 to the column-no in the oid of this instance.
%% If the table is implemented by a subagent this does not help,
%% we'll call that subagent many times. But it shouldn't be any
%% problems.
%%-----------------------------------------------------------------
mk_next_oid(Vb) ->
    case snmp_mib:lookup(get(mibserver), Oid = Vb#varbind.oid) of
	{table_column, MibEntry, TableEntryOid} ->
	    [Col | _] = Oid -- TableEntryOid,
	    Vb#varbind{oid = TableEntryOid ++ [Col+1]};
	_ ->
	    Vb
    end.

%%%-----------------------------------------------------------------
%%% 3. GET REQUEST
%%% --------------
%%%   According to RFC1157, section 4.1.2 and RFC1905, section 4.2.1.
%%%   In rfc1157:4.1.2 it isn't specified if noSuchName should be
%%%   returned even if some other varbind generates a genErr.
%%%   In rfc1905:4.2.1 this is not a problem since exceptions are
%%%   used, and thus a genErr will be returned anyway.
%%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: do_get/3
%% Purpose: do_get handles "getRequests".
%% Pre: incoming varbinds have type == 'NULL', value == unSpecified
%% Returns: {noError, 0, ListOfNewVarbinds} |
%%          {ErrorStatus, ErrorIndex, []}
%%-----------------------------------------------------------------
do_get(MibView, UnsortedVarbinds, IsNotification) ->
    {OutSideView, InSideView} = split_vbs_view(UnsortedVarbinds, MibView),
    {Error, Index, NewVbs} = do_get(InSideView, IsNotification),
    {Error, Index, NewVbs ++ OutSideView}.

split_vbs_view(Vbs, MibView) ->
    split_vbs_view(Vbs, MibView, [], []).

split_vbs_view([Vb | Vbs], MibView, Out, In) ->
    case snmp_acm:validate_mib_view(Vb#varbind.oid, MibView) of
	true -> split_vbs_view(Vbs, MibView, Out, [Vb | In]);
	false -> split_vbs_view(Vbs, MibView,
				[Vb#varbind{value = noSuchObject} | Out], In)
    end;
split_vbs_view([], MibView, Out, In) ->
    {Out, In}.
	    
do_get(UnsortedVarbinds, IsNotification) ->
    {MyVarbinds, SubagentVarbinds} = sort_varbindlist(UnsortedVarbinds),
    case do_get_local(MyVarbinds, [], IsNotification) of
	{noError, 0, NewMyVarbinds} ->
	    case do_get_subagents(SubagentVarbinds, IsNotification) of
		{noError, 0, NewSubagentVarbinds} ->
		    {noError, 0, NewMyVarbinds ++ NewSubagentVarbinds};
		{ErrorStatus, ErrorIndex, _} ->
		    {ErrorStatus, ErrorIndex, []}
	    end;
	{ErrorStatus, ErrorIndex, _} -> 
	    {ErrorStatus, ErrorIndex, []}
    end.

%%-----------------------------------------------------------------
%% Func: do_get_local/3
%% Purpose: Loop the variablebindings list. We know that each varbind
%%          in that list belongs to us.
%% Returns: {noError, 0, ListOfNewVarbinds} |
%%          {ErrorStatus, ErrorIndex, []}
%%-----------------------------------------------------------------
do_get_local([Vb | Vbs], Res, IsNotification) ->
    case try_get(Vb, IsNotification) of
	NewVb when record(NewVb, varbind) ->
	    do_get_local(Vbs, [NewVb | Res], IsNotification);
	ListOfNewVb when list(ListOfNewVb) ->
	    do_get_local(Vbs, lists:append(ListOfNewVb, Res), IsNotification);
	{error, Error, OrgIndex} ->
	    {Error, OrgIndex, []}
    end;
do_get_local([], Res, _IsNotification) -> 
    {noError, 0, Res}.

%%-----------------------------------------------------------------
%% Func: do_get_subagents/2
%% Purpose: Loop the list of varbinds for different subagents.
%%          For each of them, call sub_agent_get to retreive
%%          the values for them.
%% Returns: {noError, 0, ListOfNewVarbinds} |
%%          {ErrorStatus, ErrorIndex, []}
%%-----------------------------------------------------------------
do_get_subagents(SubagentVarbinds, IsNotification) ->
    do_get_subagents(SubagentVarbinds, [], IsNotification).
do_get_subagents([{SubAgentPid, SAVbs} | Tail], Res, IsNotification) ->
    {_SAOids, Vbs} = sa_split(SAVbs),
    case catch subagent_get(SubAgentPid, Vbs, IsNotification) of
	{noError, 0, NewVbs} ->
	    do_get_subagents(Tail, lists:append(NewVbs, Res), IsNotification);
	{ErrorStatus, ErrorIndex, _} ->
	    {ErrorStatus, ErrorIndex, []};
	{'EXIT', Reason} ->
	    user_err("Lost contact with subagent (get) ~w. Using genErr", 
		     [Reason]),
	    {genErr, 0, []} 
    end;
do_get_subagents([], Res, _IsNotification) ->
    {noError, 0, Res}.


%%-----------------------------------------------------------------
%% Func: try_get/2
%% Returns: {error, ErrorStatus, OrgIndex} |
%%          #varbind |
%%          List of #varbind
%%-----------------------------------------------------------------
try_get(IVb, IsNotification) when record(IVb, ivarbind) ->
    #ivarbind{varbind = Vb} = IVb,
    get_var_value_from_ivb(IVb, IsNotification);
try_get({TableOid, TableVbs}, IsNotification) ->
    [#ivarbind{mibentry = MibEntry}|_] = TableVbs,
    {NoAccessVbs, AccessVbs} =
	check_all_table_vbs(TableVbs, IsNotification, [], []),
    case get_tab_value_from_mib(MibEntry, TableOid, AccessVbs) of
	{error, ErrorStatus, OrgIndex} ->
	    {error, ErrorStatus, OrgIndex};
	NVbs ->
	    NVbs ++ NoAccessVbs
    end.

%%-----------------------------------------------------------------
%% Make sure all requested columns are accessible.
%%-----------------------------------------------------------------
check_all_table_vbs([IVb| IVbs], IsNotification, NoA, A) ->
    #ivarbind{mibentry = Me, varbind = Vb} = IVb,
    #varbind{oid = Oid, org_index = OrgIndex} = Vb,
    case Me#me.access of
	'not-accessible' -> 
	    NNoA = [Vb#varbind{value = noSuchInstance} | NoA],
	    check_all_table_vbs(IVbs, IsNotification, NNoA, A);
	'accessible-for-notify' when IsNotification == false -> 
	    NNoA = [Vb#varbind{value = noSuchInstance} | NoA],
	    check_all_table_vbs(IVbs, IsNotification, NNoA, A);
	'write-only' -> 
	    NNoA = [Vb#varbind{value = noSuchInstance} | NoA],
	    check_all_table_vbs(IVbs, IsNotification, NNoA, A);
	_ ->
	    check_all_table_vbs(IVbs, IsNotification, NoA, [IVb | A])
    end;
check_all_table_vbs([], _IsNotification, NoA, A) -> {NoA, A}.

%%-----------------------------------------------------------------
%% Returns: {error, ErrorStatus, OrgIndex} |
%%          #varbind
%%-----------------------------------------------------------------
get_var_value_from_ivb(IVb, IsNotification)
  when IVb#ivarbind.status == noError ->
    #ivarbind{mibentry = Me, varbind = Vb} = IVb,
    #varbind{org_index = OrgIndex, oid = Oid} = Vb,
    case Me#me.access of
	'not-accessible' -> 
	    Vb#varbind{value = noSuchInstance};
	'accessible-for-notify' when IsNotification == false -> 
	    Vb#varbind{value = noSuchInstance};
	'write-only' -> 
	    Vb#varbind{value = noSuchInstance};
	_ -> 
	    case get_var_value_from_mib(Me, Oid) of
		{value, Type, Value} ->
		    Vb#varbind{variabletype = Type, value = Value};
		{error, ErrorStatus} ->
		    {error, ErrorStatus, OrgIndex}
	    end
    end;
get_var_value_from_ivb(#ivarbind{status = Status, varbind = Vb}, _) ->
    Vb#varbind{value = Status}.

%%-----------------------------------------------------------------
%% Func: get_var_value_from_mib/1
%% Purpose: 
%% Returns: {error, ErrorStatus} |
%%          {value, Type, Value}
%%-----------------------------------------------------------------
%% Pre: Oid is a correct instance Oid (lookup checked that).
%% Returns: A correct return value (see make_value_a_correct_value)
get_var_value_from_mib(#me{entrytype = variable,
			   asn1_type = ASN1Type,
			   mfa = {Module, Func, Args}},
		       _Oid) ->
    Result = (catch dbg_apply(Module, Func, [get | Args])),
    % mib shall return {value, <a-nice-value-within-range>} |
    % {noValue, noSuchName} (v1) | 
    % {noValue, noSuchObject | noSuchInstance} (v2, v1)
    % everything else (including 'genErr') will generate 'genErr'.
    make_value_a_correct_value(Result, ASN1Type, {Module, Func, Args});

get_var_value_from_mib(#me{entrytype = table_column,
			   oid = MeOid,
			   asn1_type = ASN1Type,
			   mfa = {Module, Func, Args}},
		       Oid) ->
    Col = lists:last(MeOid),
    Indexes = snmp_misc:diff(Oid, MeOid),
    [Result] = (catch dbg_apply(Module, Func, [get, Indexes, [Col] | Args])),
    make_value_a_correct_value(Result, ASN1Type, {Module, Func, Args, 
						  Indexes, Col}).


%% For table operations we need to pass RestOid down to the table-function.
%% Its up to the table-function to check for noSuchInstance (ex: a 
%% non-existing row).
%% Returns: {error, ErrorStatus, OrgIndex} |
%%          {value, Type, Value}
get_tab_value_from_mib(#me{mfa = {Module,Func,Args}}, TableOid, TableVbs) ->
    TableOpsWithShortOids = deletePrefixes(TableOid, TableVbs),
    case get_value_all_rows(snmp_svbl:sort_varbinds_rows(TableOpsWithShortOids),
			    Module, Func, Args, []) of
	{Error, Index} ->
	    #ivarbind{varbind = Vb} = lists:nth(Index, TableVbs),
	    {error, Error, Vb#varbind.org_index};
	ListOfValues -> 
	    merge_varbinds_and_value(TableVbs, ListOfValues)
    end.

%%-----------------------------------------------------------------
%% Values is a scrambled list of {CorrectValue, Index}, where Index
%% is index into the #ivarbind list. So for each Value, we must
%% find the corresponding #ivarbind, and merge them into a new
%% #varbind.
%% The Values list comes from validate_tab_res.
%%-----------------------------------------------------------------
merge_varbinds_and_value(IVbs, [{{value, Type, Value}, Index} | Values]) ->
    #ivarbind{varbind = Vb} = lists:nth(Index, IVbs),
    [Vb#varbind{variabletype = Type, value = Value} |
     merge_varbinds_and_value(IVbs, Values)];
merge_varbinds_and_value(_, []) -> [].
    
get_value_all_rows([{RowIndex, OrgCols} | Rows], Module, Func, Args, Res) 
  when RowIndex == [] ->
    Cols = lists:map(fun({_Col, _ASN1Type, Index}) ->
			     {{value, noValue, noSuchInstance}, Index}
		     end, OrgCols),
    NewRes = lists:append(Cols, Res),
    get_value_all_rows(Rows, Module, Func, Args, NewRes);
get_value_all_rows([Row | Rows], Module, Func, Args, Res) ->
    {RowIndex, OrgCols} = Row,
    {DOrgCols, Dup} = remove_duplicates(OrgCols),
    Cols = delete_index(DOrgCols),
    Result = (catch dbg_apply(Module, Func, [get, RowIndex, Cols | Args])),
    case validate_tab_res(Result, DOrgCols, {Module, Func, Args}) of
	Values when list(Values) ->
	    NVals = restore_duplicates(Dup, Values),
	    NewRes = lists:append(NVals, Res),
	    get_value_all_rows(Rows, Module, Func, Args, NewRes);
	{error, ErrorStatus, Index} ->
	    validate_err(row_set, {ErrorStatus, Index}, {Module, Func, Args})
%% Remove these lines!
%	{ErrorStatus, ColNumber} ->
%	    Index = col_to_index(ColNumber, OrgCols),
%	    validate_err(row_set, {ErrorStatus, Index}, {Module,Func,Args})
    end;
get_value_all_rows([], Module, Func, Args, Res) -> Res.

%%-----------------------------------------------------------------
%% Returns: list of {ShortOid, ASN1TYpe}
%%-----------------------------------------------------------------
deletePrefixes(Prefix, [#ivarbind{varbind = Varbind, mibentry = ME} | Vbs]) ->
    #varbind{oid = Oid} = Varbind,
    [{snmp_misc:diff(Oid, Prefix), ME#me.asn1_type} |
     deletePrefixes(Prefix, Vbs)];
deletePrefixes(Prefix, []) -> [].

%%-----------------------------------------------------------------
%% Args: {RowIndex, list of {ShortOid, ASN1Type}}
%% Returns: list of Col
%%-----------------------------------------------------------------
delete_index([{Col, Val, OrgIndex} | T]) ->
    [Col | delete_index(T)];
delete_index([]) -> [].

%%-----------------------------------------------------------------
%% This function is called before 'get' on a table, and removes
%% any duplicate columns.  It returns {Cols, DupInfo}.  The Cols
%% are the unique columns.  The instrumentation function is
%% called to get the values.  These values, together with the
%% DupInfo, is later passed to restore_duplicates, which uses
%% the retrieved values to reconstruct the original column list,
%% but with the retrieved value for each column.
%%-----------------------------------------------------------------
remove_duplicates(Cols) ->
    remove_duplicates(Cols, [], []).


remove_duplicates([{Col, V1, OrgIdx1}, {Col, V2, OrgIdx2} | T], NCols, Dup) ->
    remove_duplicates([{Col, V1, OrgIdx1} | T], NCols, 
		      [{Col, V2, OrgIdx2} | Dup]);
remove_duplicates([Col | T], NCols, Dup) ->
    remove_duplicates(T, [Col | NCols], Dup);
remove_duplicates([], NCols, Dup) ->
    {lists:reverse(NCols), lists:reverse(Dup)}.

restore_duplicates([], Cols) ->
    lists:map(fun({Col, Val, OrgIndex}) -> {Val, OrgIndex} end, Cols);
restore_duplicates([{Col, Val2, OrgIndex2} | Dup],
		   [{Col, NVal, OrgIndex1} | Cols]) ->
    [{NVal, OrgIndex2} |
     restore_duplicates(Dup, [{Col, NVal, OrgIndex1} | Cols])];
restore_duplicates(Dup, [{Col, Val, OrgIndex} | T]) ->
    [{Val, OrgIndex} | restore_duplicates(Dup, T)].

%% Maps the column number to Index.
% col_to_index(0, _) -> 0;
% col_to_index(Col, [{Col, _, Index}|_]) ->
%     Index;
% col_to_index(Col, [_|Cols]) ->
%     col_to_index(Col, Cols).

%%-----------------------------------------------------------------
%% Three cases:
%%   1) All values ok
%%   2) table_func returned {Error, ...}
%%   3) Some value in Values list is erroneous.
%% Args: Value is a list of values from table_func(get..)
%%       OrgCols is a list with {Col, ASN1Type, OrgIndex} 
%%         each element in Values and OrgCols correspond to each
%%         other.
%%-----------------------------------------------------------------
validate_tab_res(Values, OrgCols, Mfa) when list(Values) ->
    {_Col, ASN1Type, OneIdx} = hd(OrgCols),
    validate_tab_res(Values, OrgCols, Mfa, [], OneIdx);
validate_tab_res({noValue, Error}, OrgCols, Mfa) ->
    Values = lists:duplicate(length(OrgCols), {noValue, Error}),
    validate_tab_res(Values, OrgCols, Mfa);
validate_tab_res({genErr, Col}, OrgCols, Mfa) ->
    case lists:keysearch(Col, 1, OrgCols) of
	{value, {_Col, _ASN1Type, Index}} ->
	    {error, genErr, Index};
	_ ->
	    user_err("Invalid column in {genErr, ~w} from ~w (get)",
		     [Col, Mfa]),
	    [{_Col, _ASN1Type, Index} | _] = OrgCols,
	    {error, genErr, Index}
    end;
validate_tab_res(genErr, [{_Col, _ASN1Type, Index} | _OrgCols], Mfa) ->
    {error, genErr, Index};
validate_tab_res(Error, [{_Col, _ASN1Type, Index} | _OrgCols], Mfa) ->
    user_err("Invalid return value ~w from ~w (get)",[Error, Mfa]),
    {error, genErr, Index}.

validate_tab_res([Value | Values], [{Col, ASN1Type, Index} | OrgCols],
		 Mfa, Res, I) ->
    %% This one makes it possible to return a list of genErr, which
    %% is not allowed according to the manual.  But that's ok, as
    %% everything else will generate a genErr! (the only problem is
    %% that it won't generate a user_error).
    case make_value_a_correct_value(Value, ASN1Type, Mfa) of
	{error, ErrorStatus} ->
	    {error, ErrorStatus, Index};
	CorrectValue ->
	    NewRes = [{Col, CorrectValue, Index} | Res],
	    validate_tab_res(Values, OrgCols, Mfa, NewRes, I)
    end;
validate_tab_res([], [], Mfa, Res, _I) -> Res;
validate_tab_res([], [{_Col, _ASN1Type, Index}|_], Mfa, Res, I) ->
    user_err("Too few values returned from ~w (get)", [Mfa]),
    {error, genErr, Index};
validate_tab_res(TooMany, [], Mfa, Res, I) ->
    user_err("Too many values returned from ~w (get)", [Mfa]),
    {error, genErr, I}.


%%%-----------------------------------------------------------------
%%% 4. GET-NEXT REQUEST
%%% --------------
%%%   According to RFC1157, section 4.1.3 and RFC1905, section 4.2.2.
%%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: do_get_next/2
%% Purpose: do_get_next handles "getNextRequests".
%% Note: Even if it is SNMPv1, a varbind's value can be
%%       endOfMibView. This is converted to noSuchName in process_pdu.
%% Returns: {noError, 0, ListOfNewVarbinds} |
%%          {ErrorStatus, ErrorIndex, []}
%% Note2: ListOfNewVarbinds is not sorted in any order!!!
%% Alg: First, the variables are sorted in OID order.
%%
%%      Second, next in the MIB is performed for each OID, and
%%      the result is collected as: if next oid is a variable,
%%      perform a get to retrieve its value; if next oid is in a
%%      table, save this value and continue until we get an oid
%%      outside this table. Then perform get_next on the table,
%%      and continue with all endOfTables and the oid outside the
%%      table; if next oid is an subagent, save this value and
%%      continue as in the table case.
%%
%%      Third, each response is checked for endOfMibView, or (for
%%      subagents) that the Oid returned has the correct prefix.
%%      (This is necessary since an SA can be registered under many
%%      separated subtrees, and if the last variable in the first
%%      subtree is requested in a next, the SA will return the first
%%      variable in the second subtree. This might be working, since
%%      there may be a variable in between these subtrees.) For each
%%      of these, a new get-next is performed, one at a time.
%%      This alg. might be optimised in several ways. The most 
%%      striking one is that the same SA might be called several
%%      times, when one time should be enough. But it isn't clear
%%      that this really matters, since many nexts across the same
%%      subagent must be considered to be very rare.
%%-----------------------------------------------------------------
do_get_next(MibView, UnsortedVarbinds) ->
    SortedVarbinds = oid_sort_varbindlist(UnsortedVarbinds),
    next_loop_varbinds([], SortedVarbinds, MibView, [], []).

oid_sort_varbindlist(Vbs) ->
    lists:keysort(#varbind.oid, Vbs).

%% LAVb is Last Accessible Vb
next_loop_varbinds([], [Vb | Vbs], MibView, Res, LAVb) ->
    case varbind_next(Vb, MibView) of
	endOfMibView ->
	    RVb = if LAVb == [] -> Vb;
		     true -> LAVb
		  end,
	    NewVb = RVb#varbind{variabletype = 'NULL', value = endOfMibView},
	    next_loop_varbinds([], Vbs, MibView, [NewVb | Res], []);
	{variable, ME, VarOid} when ME#me.access /= 'not-accessible',
	                            ME#me.access /= 'write-only',
				    ME#me.access /= 'accessible-for-notify' -> 
	    case try_get_instance(Vb, ME) of
		{value, noValue, _NoSuchSomething} ->
		    %% Try next one
		    NewVb = Vb#varbind{oid = VarOid, value = 'NULL'},
		    next_loop_varbinds([], [NewVb | Vbs], MibView, Res, []);
		{value, Type, Value} ->
		    NewVb = Vb#varbind{oid = VarOid, variabletype = Type,
				       value = Value},
		    next_loop_varbinds([], Vbs, MibView, [NewVb | Res], []);
		{error, ErrorStatus} ->
		    ?vdebug("next loop varbinds:"
			    "~n   ErrorStatus: ~p",[ErrorStatus]),
		    {ErrorStatus, Vb#varbind.org_index, []}
	    end;
	{variable, ME, VarOid} -> 
	    RVb = if LAVb == [] -> Vb;
		     true -> LAVb
		  end,
	    NewVb = Vb#varbind{oid = VarOid, value = 'NULL'},
	    next_loop_varbinds([], [NewVb | Vbs], MibView, Res, RVb);
	{table, TableOid, TableRestOid, ME} ->
	    next_loop_varbinds({table, TableOid, ME,
				[{tab_oid(TableRestOid), Vb}]},
			       Vbs, MibView, Res, []);
	{subagent, SubAgentPid, SAOid} ->
	    next_loop_varbinds({subagent, SubAgentPid, SAOid, [Vb]},
			       Vbs, MibView, Res, [])
    end;
next_loop_varbinds({table, TableOid, ME, TabOids},
		   [Vb | Vbs], MibView, Res, LAVb) ->
    case varbind_next(Vb, MibView) of
	{table, TableOid, TableRestOid, _ME} ->
	    next_loop_varbinds({table, TableOid, ME,
				[{tab_oid(TableRestOid), Vb} | TabOids]},
			       Vbs, MibView, Res, []);
	_ ->
	    case get_next_table(ME, TableOid, TabOids, MibView) of
		{ok, TabRes, TabEndOfTabVbs} ->
		    NewVbs = lists:append(TabEndOfTabVbs, [Vb | Vbs]),
		    NewRes = lists:append(TabRes, Res),
		    next_loop_varbinds([], NewVbs, MibView, NewRes, []);
		{ErrorStatus, OrgIndex} ->
		    ?vdebug("next loop varbinds: next varbind"
			    "~n   ErrorStatus: ~p"
			    "~n   OrgIndex:    ~p",
			    [ErrorStatus,OrgIndex]),
		    {ErrorStatus, OrgIndex, []}
	    end
    end;
next_loop_varbinds({table, TableOid, ME, TabOids},
		   [], MibView, Res, LAVb) ->
    case get_next_table(ME, TableOid, TabOids, MibView) of
	{ok, TabRes, TabEndOfTabVbs} ->
	    NewRes = lists:append(TabRes, Res),
	    next_loop_varbinds([], TabEndOfTabVbs, MibView, NewRes, []);
	{ErrorStatus, OrgIndex} ->
	    ?vdebug("next loop varbinds: next table"
		    "~n   ErrorStatus: ~p"
		    "~n   OrgIndex:    ~p",
		    [ErrorStatus,OrgIndex]),
	    {ErrorStatus, OrgIndex, []}
    end;
next_loop_varbinds({subagent, SAPid, SAOid, SAVbs},
		   [Vb | Vbs], MibView, Res, LAVb) ->
    case varbind_next(Vb, MibView) of
	{subagent, SubAgentPid, SAOid} ->
	    next_loop_varbinds({subagent, SAPid, SAOid,
				[Vb | SAVbs]},
			       Vbs, MibView, Res, []);
	_ ->
	    case get_next_sa(SAPid, SAOid, SAVbs, MibView) of
		{ok, SARes, SAEndOfMibViewVbs} ->
		    NewVbs = lists:append(SAEndOfMibViewVbs, [Vb | Vbs]),
		    NewRes = lists:append(SARes, Res),
		    next_loop_varbinds([], NewVbs, MibView, NewRes, []);
		{ErrorStatus, OrgIndex} ->
		    ?vdebug("next loop varbinds: next subagent"
			    "~n   Vb:          ~p"
			    "~n   ErrorStatus: ~p"
			    "~n   OrgIndex:    ~p",
			    [Vb,ErrorStatus,OrgIndex]),
		    {ErrorStatus, OrgIndex, []}
	    end
    end;
next_loop_varbinds({subagent, SAPid, SAOid, SAVbs},
		   [], MibView, Res, LAVb) ->
    case get_next_sa(SAPid, SAOid, SAVbs, MibView) of
	{ok, SARes, SAEndOfMibViewVbs} ->
	    NewRes = lists:append(SARes, Res),
	    next_loop_varbinds([], SAEndOfMibViewVbs, MibView, NewRes, []);
	{ErrorStatus, OrgIndex} ->
	    ?vdebug("next loop varbinds: next subagent"
		    "~n   ErrorStatus: ~p"
		    "~n   OrgIndex:    ~p",
		    [ErrorStatus,OrgIndex]),
	    {ErrorStatus, OrgIndex, []}
    end;
next_loop_varbinds([], [], _MibView, Res, _LAVb) ->
    {noError, 0, Res}.

try_get_instance(Vb, #me{mfa = {M, F, A}, asn1_type = ASN1Type}) ->
    ?vtrace("try get instance from <~p,~p,~p>",[M,F,A]),
    Result = (catch dbg_apply(M, F, [get | A])),
    % mib shall return {value, <a-nice-value-within-range>} |
    % {noValue, noSuchName} (v1) | 
    % {noValue, noSuchObject | noSuchInstance} (v2, v1)
    % everything else (including 'genErr') will generate 'genErr'.
    make_value_a_correct_value(Result, ASN1Type, {M, F, A}).

tab_oid([]) -> [0];
tab_oid(X) -> X.

%%-----------------------------------------------------------------
%% Perform a next, using the varbinds Oid if value is simple
%% value. If value is {endOf<something>, NextOid}, use NextOid.
%% This case happens when a table has returned endOfTable, or
%% a subagent has returned endOfMibView.
%%-----------------------------------------------------------------
varbind_next(#varbind{value = Value, oid = Oid}, MibView) ->
    case Value of
	{endOfTable, NextOid} ->
	    snmp_mib:next(get(mibserver), NextOid, MibView);
	{endOfMibView, NextOid} ->
	    snmp_mib:next(get(mibserver), NextOid, MibView);
	_ ->
	    snmp_mib:next(get(mibserver), Oid, MibView)
    end.

get_next_table(#me{mfa = {M, F, A}}, TableOid, TableOids, MibView) ->
    % We know that all TableOids have at least a column number as oid
    Sorted = snmp_svbl:sort_varbinds_rows(TableOids),
    case get_next_values_all_rows(Sorted, M,F,A, [], TableOid) of
	NewVbs when list(NewVbs) ->
	    % We must now check each Vb for endOfTable and that it is
	    % in the MibView. If not, it becomes a endOfTable. We 
	    % collect all of these together.
	    transform_tab_next_result(NewVbs, {[], []}, MibView);
	{ErrorStatus, OrgIndex} ->
	    {ErrorStatus, OrgIndex}
    end.

get_next_values_all_rows([Row | Rows], M, F, A, Res, TabOid) ->
    {RowIndex, TableOids} = Row,
    Cols = delete_index(TableOids),
    Result = (catch dbg_apply(M, F, [get_next, RowIndex, Cols | A])),
    case validate_tab_next_res(Result, TableOids, {M, F, A}, TabOid) of
	Values when list(Values) -> 
	    NewRes = lists:append(Values, Res),
	    get_next_values_all_rows(Rows, M, F, A, NewRes, TabOid);
	{ErrorStatus, OrgIndex} ->
	    {ErrorStatus, OrgIndex}
    end;
get_next_values_all_rows([], M, F, A, Res, TabOid) ->
    Res.

transform_tab_next_result([Vb | Vbs], {Res, EndOfs}, MibView) ->
    case Vb#varbind.value of
	{endOfTable, _} ->
	    split_varbinds(Vbs, Res, [Vb | EndOfs]);
	_ ->
	    case snmp_acm:validate_mib_view(Vb#varbind.oid, MibView) of
		true ->
		    transform_tab_next_result(Vbs, {[Vb|Res], EndOfs},MibView);
		_ ->
		    Oid = Vb#varbind.oid,
		    NewEndOf = Vb#varbind{value = {endOfTable, Oid}},
		    transform_tab_next_result(Vbs, {Res, [NewEndOf | EndOfs]},
					      MibView)
	    end
    end;
transform_tab_next_result([], {Res, EndOfs}, _MibView) ->
    {ok, Res, EndOfs}.

%%-----------------------------------------------------------------
%% Three cases:
%%   1) All values ok
%%   2) table_func returned {Error, ...}
%%   3) Some value in Values list is erroneous.
%% Args: Value is a list of values from table_func(get_next, ...)
%%       TableOids is a list of {TabRestOid, OrgVb} 
%%         each element in Values and TableOids correspond to each
%%         other.
%% Returns: List of NewVarbinds |
%%          {ErrorStatus, OrgIndex}
%%          (In the NewVarbinds list, the value may be endOfTable)
%%-----------------------------------------------------------------
validate_tab_next_res(Values, TableOids, Mfa, TabOid) ->
    {_Col, ASN1Type, OneIdx} = hd(TableOids),
    validate_tab_next_res(Values, TableOids, Mfa, [], TabOid,
			  next_oid(TabOid), OneIdx).
validate_tab_next_res([{NextOid, Value} | Values],
		      [{_ColNo, OrgVb, _Index} | TableOids],
		      Mfa, Res, TabOid, TabNextOid, I) ->
    #varbind{org_index = OrgIndex} = OrgVb,
    NextCompleteOid = lists:append(TabOid, NextOid),
    case snmp_mib:lookup(get(mibserver), NextCompleteOid) of
	{table_column, #me{asn1_type = ASN1Type}, _TableEntryOid} ->
	    case make_value_a_correct_value({value, Value}, ASN1Type, Mfa) of
		{error, ErrorStatus} ->
		    {ErrorStatus, OrgIndex};
		{value, Type, NValue} ->
		    NewVb = OrgVb#varbind{oid = NextCompleteOid,
					  variabletype = Type, value = NValue},
		    validate_tab_next_res(Values, TableOids, Mfa,
					  [NewVb | Res], TabOid, TabNextOid, I)
	    end;
	_ ->
	    user_err("Invalid oid ~w from ~w (get_next). Using genErr",
		     [NextOid, Mfa]),
	    {genErr, OrgIndex}
    end;
validate_tab_next_res([endOfTable | Values],
		      [{_ColNo, OrgVb, _Index} | TableOids],
		      Mfa, Res, TabOid, TabNextOid, I) ->
    NewVb = OrgVb#varbind{value = {endOfTable, TabNextOid}},
    validate_tab_next_res(Values, TableOids, Mfa, [NewVb | Res],
			  TabOid, TabNextOid, I);
validate_tab_next_res([], [], _Mfa, Res, _TabOid, _TabNextOid, I) ->
    Res;
validate_tab_next_res([], [{_Col, _OrgVb, Index}|_], Mfa, Res, _, _, _I) ->
    user_err("Too few values returned from ~w (get_next)", [Mfa]),
    {genErr, Index};
validate_tab_next_res({genErr, ColNumber}, OrgCols,
		      Mfa, _Res, _TabOid, _TabNextOid, I) ->
    OrgIndex = snmp_svbl:col_to_orgindex(ColNumber, OrgCols),
    validate_err(table_next, {genErr, OrgIndex}, Mfa);
validate_tab_next_res(Error, [{_ColNo, OrgVb, _Index} | _TableOids],
		      Mfa, _Res, _TabOid, _TabNextOid, I) ->
    #varbind{org_index = OrgIndex} = OrgVb,
    user_err("Invalid return value ~w from ~w (get_next)",
	     [Error, Mfa]),
    {genErr, OrgIndex};
validate_tab_next_res(TooMany, [], Mfa, Res, _, _, I) ->
    user_err("Too many values ~w returned from ~w (get_next)",
	     [TooMany, Mfa]),
    {genErr, I}.

%%-----------------------------------------------------------------
%% Func: get_next_sa/4
%% Purpose: Loop the list of varbinds for the subagent.
%%          Call subagent_get_next to retreive
%%          the next varbinds.
%% Returns: {ok, ListOfNewVbs, ListOfEndOfMibViewsVbs} |
%%          {ErrorStatus, ErrorIndex}
%%-----------------------------------------------------------------
get_next_sa(SAPid, SAOid, SAVbs, MibView) ->
    case catch subagent_get_next(SAPid, MibView, SAVbs) of
	{noError, 0, NewVbs} ->
	    NewerVbs = transform_sa_next_result(NewVbs,SAOid,next_oid(SAOid)),
	    split_varbinds(NewerVbs, [], []);
	{ErrorStatus, ErrorIndex, _} ->
	    {ErrorStatus, ErrorIndex};
	{'EXIT', Reason} ->
	    user_err("Lost contact with subagent (next) ~w. Using genErr",
		     [Reason]),
	    {genErr, 0}
    end.

%%-----------------------------------------------------------------
%% Check for wrong prefix returned or endOfMibView, and convert
%% into {endOfMibView, SANextOid}.
%%-----------------------------------------------------------------
transform_sa_next_result([Vb | Vbs], SAOid, SANextOid)
  when Vb#varbind.value == endOfMibView ->
    [Vb#varbind{value = {endOfMibView, SANextOid}} |
     transform_sa_next_result(Vbs, SAOid, SANextOid)];
transform_sa_next_result([Vb | Vbs], SAOid, SANextOid) ->
    case lists:prefix(SAOid, Vb#varbind.oid) of
	true ->
	    [Vb | transform_sa_next_result(Vbs, SAOid, SANextOid)];
	_ ->
	    [Vb#varbind{value = {endOfMibView, SANextOid}} |
	     transform_sa_next_result(Vbs, SAOid, SANextOid)]
    end;
transform_sa_next_result([], _SAOid, _SANextOid) ->
    [].

split_varbinds([Vb | Vbs], Res, EndOfs) ->
    case Vb#varbind.value of
	{endOfMibView, _} -> split_varbinds(Vbs, Res, [Vb | EndOfs]);
	{endOfTable, _} -> split_varbinds(Vbs, Res, [Vb | EndOfs]);
	_ -> split_varbinds(Vbs, [Vb | Res], EndOfs)
    end;
split_varbinds([], Res, EndOfs) -> {ok, Res, EndOfs}.

next_oid(Oid) ->
    case lists:reverse(Oid) of
	[H | T] -> lists:reverse([H+1 | T]);
	[] -> []
    end.

%%%-----------------------------------------------------------------
%%% 5. GET-BULK REQUEST
%%%-----------------------------------------------------------------
do_get_bulk(MibView, NonRepeaters, MaxRepetitions, PduMS, Varbinds) ->
    ?vtrace("do get bulk: start with"
	    "~n   MibView:        ~p"
	    "~n   NonRepeaters:   ~p"
	    "~n   MaxRepetitions: ~p"
	    "~n   PduMS:          ~p"
	    "~n   Varbinds:       ~p",
	    [MibView, NonRepeaters, MaxRepetitions, PduMS, Varbinds]),
    {NonRepVbs, RestVbs} = split_vbs(NonRepeaters, Varbinds, []),
    case do_get_next(MibView, NonRepVbs) of
	{noError, 0, UResNonRepVbs} -> 
	    ResNonRepVbs = lists:keysort(#varbind.org_index, UResNonRepVbs),
	    % Decode the first varbinds, produce a reversed list of
	    % listOfBytes.
	    case enc_vbs(PduMS - ?empty_pdu_size, ResNonRepVbs) of
		{SizeLeft, Res} ->
		    do_get_rep(SizeLeft, MibView, MaxRepetitions,
			       RestVbs, Res);
		Res ->
		    {noError, 0, conv_res(Res)}
	    end;
	{ErrorStatus, Index, _} ->
	    ?vdebug("do get bulk: "
		    "~n   ErrorStatus: ~p"
		    "~n   Index:       ~p",[ErrorStatus, Index]),
	    {ErrorStatus, Index, []}
    end.

% sz(L) when list(L) -> length(L);
% sz(B) when binary(B) -> size(B);
% sz(_) -> unknown.

split_vbs(N, Varbinds, Res) when N =< 0 -> {Res, Varbinds};
split_vbs(N, [H | T], Res) -> split_vbs(N-1, T, [H | Res]);
split_vbs(N, [], Res) -> {Res, []}.
     
enc_vbs(SizeLeft, Vbs) ->
    catch lists:foldl(fun(Vb, {Sz, Res}) when Sz > 0 ->
			      X = snmp_pdus:enc_varbind(Vb),
			      Lx = length(X),
			      if
				  Lx < Sz ->
				      {Sz - length(X), [X | Res]};
				  true ->
				      throw(Res)
			      end;
			 (Vb, {Sz, [H | T]}) ->
			      throw(T);
			 (Vb, {Sz, []}) ->
			      throw([])
		      end,
		      {SizeLeft, []},
		      Vbs).

do_get_rep(Sz, MibView, MaxRepetitions, Varbinds, Res) when MaxRepetitions>=0 ->
    do_get_rep(Sz, MibView, 0, MaxRepetitions, Varbinds, Res);
do_get_rep(Sz, MibView, _MaxRepetitions, Varbinds, Res) ->
    do_get_rep(Sz, MibView, 0, 0, Varbinds, Res).

conv_res(ResVarbinds) ->
    conv_res(ResVarbinds, []).
conv_res([VbListOfBytes | T], Bytes) ->
    conv_res(T, VbListOfBytes ++ Bytes);
conv_res([], Bytes) ->
    Bytes.

do_get_rep(_Sz, MibView, Max, Max, _, Res) ->
    {noError, 0, conv_res(Res)};
do_get_rep(Sz, MibView, Count, Max, Varbinds, Res) -> 
    case try_get_bulk(Sz, MibView, Varbinds) of
	{noError, NextVarbinds, SizeLeft, Res2} -> 
	    do_get_rep(SizeLeft, MibView, Count+1, Max, NextVarbinds,
		       Res2 ++ Res);
	{endOfMibView, _NextVarbinds, _SizeLeft, Res2} -> 
	    {noError, 0, conv_res(Res2 ++ Res)};
	{ErrorStatus, Index} ->
	    {ErrorStatus, Index, []}
    end.

try_get_bulk(Sz, MibView, Varbinds) -> 
    case do_get_next(MibView, Varbinds) of
	{noError, 0, UNextVarbinds} -> 
	    NextVarbinds = lists:keysort(#varbind.org_index, UNextVarbinds),
	    case enc_vbs(Sz, NextVarbinds) of
		{SizeLeft, Res} when list(Res) ->
		    {check_end_of_mibview(NextVarbinds),
		     NextVarbinds, SizeLeft, Res};
		Res when list(Res) ->
		    {endOfMibView, [], 0, Res};
		Else ->
		    exit(Else)
	    end;
	{ErrorStatus, Index, _} ->
	    {ErrorStatus, Index}
    end.

%% If all variables in this pass are endOfMibView,
%% there is no reason to continue.
check_end_of_mibview([#varbind{value = endOfMibView} | T]) ->
    check_end_of_mibview(T);
check_end_of_mibview([]) -> endOfMibView;
check_end_of_mibview(_) -> noError.


%%%--------------------------------------------------
%%% 6. SET REQUEST
%%%--------------------------------------------------
%% return:  {ErrStatus, ErrIndex}
%% where ErrIndex is an index in Varbinds list (not org_index (user-functions
%% doesn't see org_index)).
do_set(MibView, UnsortedVarbinds) ->
    SetModule = get(set_module),
    ?vtrace("set module: ~p",[SetModule]),
    apply(SetModule, do_set, [MibView, UnsortedVarbinds]).

do_subagent_set(Arguments) ->
    SetModule = get(set_module),
    apply(SetModule, do_subagent_set, [Arguments]).

%%%-----------------------------------------------------------------
%%% 7. Misc functions
%%%-----------------------------------------------------------------
sort_varbindlist(Varbinds) ->
    snmp_svbl:sort_varbindlist(get(mibserver), Varbinds).

sa_split(SubagentVarbinds) ->
    snmp_svbl:sa_split(SubagentVarbinds).

make_response_pdu(ReqId, ErrStatus, ErrIndex, OrgVarbinds, ResponseVarbinds)
  when ErrIndex /= 0 ->
    #pdu{type = 'get-response', request_id = ReqId, error_status = ErrStatus,
	 error_index = ErrIndex, varbinds = OrgVarbinds};
make_response_pdu(ReqId, ErrStatus, ErrIndex, OrgVarbinds, ResponseVarbinds) ->
    #pdu{type = 'get-response', request_id = ReqId, error_status = ErrStatus,
	 error_index = ErrIndex, varbinds = ResponseVarbinds}.

%% Valid errormsgs for different operations.
validate_err(consistency_check, {'EXIT', Reason}, _) ->
    {genErr, 0};
validate_err(consistency_check, X, _) ->
    X;

validate_err(is_set_ok, noError, _) -> noError;
validate_err(is_set_ok, noCreation, _) -> noCreation;
validate_err(is_set_ok, inconsistentValue, _) -> inconsistentValue;
validate_err(is_set_ok, resourceUnavailable, _) -> resourceUnavailable;
validate_err(is_set_ok, inconsistentName, _) -> inconsistentName;
validate_err(is_set_ok, badValue, _) -> badValue;
validate_err(is_set_ok, wrongValue, _) -> wrongValue;
validate_err(is_set_ok, noSuchName, _) -> noSuchName;
validate_err(is_set_ok, noAccess, _) -> noAccess;
validate_err(is_set_ok, notWritable, _) -> notWritable;
validate_err(is_set_ok, genErr, _) -> genErr;
validate_err(is_set_ok, X, Mfa) -> 
    user_err("~w with is_set_ok, returned: ~w. Using genErr.",
	     [Mfa, X]),
    genErr;

validate_err(set, commitFailed, _) -> commitFailed;
validate_err(set, undoFailed, _) -> undoFailed;
validate_err(set, noError, _) -> noError;
validate_err(set, genErr, _) -> genErr;
validate_err(set, X, Mfa) -> 
    user_err("~w with set, returned: ~w. Using genErr.",
	     [Mfa, X]),
    genErr;

validate_err(undo, undoFailed, _) -> undoFailed;
validate_err(undo, noError, _) -> noError;
validate_err(undo, genErr, _) -> genErr;
validate_err(undo, X, Mfa) -> 
    user_err("~w with undo, returned: ~w. Using genErr.",
	     [Mfa, X]),
    genErr;

validate_err(table_is_set_ok, {Err, Idx}, Mfa) when integer(Idx) ->
    {validate_err(is_set_ok, Err, Mfa), Idx};
validate_err(table_is_set_ok, X, Mfa) ->
    user_err("~w with is_set_ok (table), returned: ~w. Using genErr.",
	     [Mfa, X]),
    {genErr, 0};

validate_err(row_is_set_ok, {Err, Idx}, _) when integer(Idx) ->
    {Err, Idx};
validate_err(row_is_set_ok, {Err, {false, BadCol}}, Mfa) ->
    user_err("~w with is_set_ok (table), returned bad column: "
	     "~w. Using genErr.", [Mfa, BadCol]),
    {genErr, 0};

validate_err(table_undo, {Err, Idx}, Mfa) when integer(Idx) ->
    {validate_err(undo, Err, Mfa), Idx};
validate_err(table_undo, X, Mfa) ->
    user_err("~w with undo (table), returned: ~w. Using genErr.",
	     [Mfa, X]),
    {genErr, 0};

validate_err(row_undo, {Err, Idx}, _) when integer(Idx) ->
    {Err, Idx};
validate_err(row_undo, {Err, {false, BadCol}}, Mfa) ->
    user_err("~w with undo (table), returned bad column: "
	     "~w. Using genErr.", [Mfa, BadCol]),
    {genErr, 0};

validate_err(table_set, {Err, Idx}, Mfa) when integer(Idx) ->
    {validate_err(set, Err, Mfa), Idx};
validate_err(table_set, X, Mfa) ->
    user_err("~w with set (table), returned: ~w. Using genErr.",
	     [Mfa, X]),
    {genErr, 0};

validate_err(row_set, {Err, Idx}, _) when integer(Idx) ->
    {Err, Idx};
validate_err(row_set, {Err, {false, BadCol}}, Mfa) ->
    user_err("~w with set (table), returned bad column: "
	     "~w. Using genErr.", [Mfa, BadCol]),
    {genErr, 0};

validate_err(table_next, {Err, Idx}, Mfa) when integer(Idx) ->
    {Err, Idx};
validate_err(table_next, {Err, {false, BadCol}}, Mfa) ->
    user_err("~w with get_next, returned bad column: "
	     "~w. Using genErr.", [Mfa, BadCol]),
    {genErr, 0}.

validate_err(v2_to_v1, {V2Err, Index}) ->
    {v2err_to_v1err(V2Err), Index};
validate_err(v2_to_v1, _) ->
    {genErr, 0}.

get_err({ErrC, ErrI, Vbs}) ->
    {get_err_i(ErrC), ErrI, Vbs}.

get_err_i(noError) -> noError;
get_err_i(S) -> 
    ?vtrace("convert '~p' to 'getErr'",[S]),
    genErr.

v2err_to_v1err(noError) ->            noError;
v2err_to_v1err(noAccess) ->           noSuchName;
v2err_to_v1err(noCreation) ->         noSuchName;
v2err_to_v1err(notWritable) ->        noSuchName;
v2err_to_v1err(wrongLength) ->        badValue;
v2err_to_v1err(wrongEncoding) ->      badValue;
v2err_to_v1err(wrongType) ->          badValue;
v2err_to_v1err(wrongValue) ->         badValue;
v2err_to_v1err(inconsistentValue) ->  badValue;
v2err_to_v1err(inconsistentName) ->   noSuchName;
v2err_to_v1err(noSuchName) ->         noSuchName;
v2err_to_v1err(badValue) ->           badValue;
v2err_to_v1err(authorizationError) -> noSuchName;
%% genErr | resourceUnavailable | undoFailed | commitFailed -> genErr
v2err_to_v1err(_Error) ->             genErr.

%%-----------------------------------------------------------------
%% transforms a (hopefully correct) return value ((perhaps) from a 
%% mib-function) to a typed and guaranteed correct return value.
%% An incorrect return value is transformed to {error, genErr}.
%% A correct return value is on the form: 
%% {error, <error-msg>} | {value, <variable-type>, <value>}
%%-----------------------------------------------------------------
make_value_a_correct_value({value, Val}, Asn1, Mfa)
  when Asn1#asn1_type.bertype == 'INTEGER' ->
    check_integer(Val, Asn1, Mfa);

make_value_a_correct_value({value, Val}, Asn1, Mfa)
  when Asn1#asn1_type.bertype == 'Counter32' ->
    check_integer(Val, Asn1, Mfa);

make_value_a_correct_value({value, Val}, Asn1, Mfa)
  when Asn1#asn1_type.bertype == 'Unsigned32' ->
    check_integer(Val, Asn1, Mfa);

make_value_a_correct_value({value, Val}, Asn1, Mfa)
  when Asn1#asn1_type.bertype == 'TimeTicks' ->
    check_integer(Val, Asn1, Mfa);

make_value_a_correct_value({value, Val}, Asn1, Mfa)
  when Asn1#asn1_type.bertype == 'Counter64' ->
    check_integer(Val, Asn1, Mfa);

make_value_a_correct_value({value, Val}, Asn1, Mfa)
  when Asn1#asn1_type.bertype == 'BITS', list(Val) ->
    {value,Kibbles} = snmp_misc:assq(kibbles,Asn1#asn1_type.assocList),
    case snmp_misc:bits_to_int(Val,Kibbles) of
	error ->
	    report_err(Val, Mfa, wrongValue);
	Int ->
	    make_value_a_correct_value({value,Int},Asn1,Mfa)
    end;

make_value_a_correct_value({value, Val}, Asn1, Mfa)
  when Asn1#asn1_type.bertype == 'BITS', integer(Val) ->
    {value,Kibbles} = snmp_misc:assq(kibbles,Asn1#asn1_type.assocList),
    {_Kibble,BitNo} = lists:last(Kibbles),
    case round(math:pow(2,BitNo+1)) of
	X when Val < X ->
	    {value,'BITS',Val};
	Big ->
	    report_err(Val,Mfa,wrongValue)
    end;

make_value_a_correct_value({value, String},
			   #asn1_type{bertype = 'OCTET STRING',
				      hi = Hi, lo = Lo}, Mfa) ->
    check_octet_string(String, Hi, Lo, Mfa, 'OCTET STRING');

make_value_a_correct_value({value, String},
			   #asn1_type{bertype = 'IpAddress',
				      hi = Hi, lo = Lo}, Mfa) ->
    check_octet_string(String, Hi, Lo, Mfa, 'IpAddress');

make_value_a_correct_value({value, Oid},
			   #asn1_type{bertype = 'OBJECT IDENTIFIER'},
			   Mfa) ->
    case snmp_misc:is_oid(Oid) of
	true -> {value, 'OBJECT IDENTIFIER', Oid};
	Else -> {error, wrongType}
    end;

make_value_a_correct_value({value, Val}, Asn1, Mfa)
  when Asn1#asn1_type.bertype == 'Opaque' ->
    if list(Val) -> {value, 'Opaque', Val};
       true -> {error, wrongType}
    end;

make_value_a_correct_value({noValue, noSuchObject}, ASN1Type, Mfa) ->
    {value, noValue, noSuchObject};
make_value_a_correct_value({noValue, noSuchInstance}, ASN1Type, Mfa) ->
    {value, noValue, noSuchInstance};
make_value_a_correct_value({noValue, noSuchName}, ASN1Type, Mfa) ->
    %% Transform this into a v2 value.  It is converted to noSuchName
    %% later if it was v1.  If it was v2, we use noSuchInstance.
    {value, noValue, noSuchInstance};
%% For backwards compatibility only - we really shouldn't allow this;
%% it makes no sense to return unSpecified for a variable! But we did
%% allow it previously. -- We transform unSpecified to noSuchInstance
%% (OTP-3303).
make_value_a_correct_value({noValue, unSpecified}, ASN1Type, Mfa) ->
    {value, noValue, noSuchInstance};
make_value_a_correct_value(genErr, _ASN1Type, _MFA) ->
    {error, genErr};

make_value_a_correct_value(WrongVal, ASN1Type, undef) ->
    {error, genErr};

make_value_a_correct_value(WrongVal, ASN1Type, Mfa) ->
    user_err("Got ~w from ~w. (~w) Using genErr",
	     [WrongVal, Mfa, ASN1Type]),
    {error, genErr}.

check_integer(Val, Asn1, Mfa) ->
    case Asn1#asn1_type.assocList of
	undefined -> check_size(Val, Asn1, Mfa);
	Alist ->
	    case snmp_misc:assq(enums, Alist) of
		{value, Enums} -> check_enums(Val, Asn1, Enums, Mfa);
		false -> check_size(Val, Asn1, Mfa)
	    end
    end.

check_octet_string(String, Hi, Lo, Mfa, Type) ->
    Len = (catch length(String)), % it might not be a list
    case snmp_misc:is_string(String) of
	true when Lo == undefined -> {value, Type, String};
	true when Len =< Hi, Len >= Lo ->
	    {value, Type, String};
	true ->
	    report_err(String, Mfa, wrongLength);
	Else ->
	    report_err(String, Mfa, wrongType)
    end.

check_size(Val, #asn1_type{lo = Lo, hi = Hi, bertype = Type}, Mfa) 
  when integer(Val) ->
    ?vtrace("check size of integer: "
	    "~n   Value:       ~p"
	    "~n   Upper limit: ~p"
	    "~n   Lower limit: ~p"
	    "~n   BER-type:    ~p",
	    [Val,Hi,Lo,Type]),
    if
	Lo == undefined, Hi == undefined -> {value, Type, Val};
	Lo == undefined, integer(Hi), Val =< Hi ->
	    {value, Type, Val};
	integer(Lo), Val >= Lo, Hi == undefined ->
	    {value, Type, Val};
	integer(Lo), integer(Hi), Val >= Lo, Val =< Hi ->
	    {value, Type, Val};
	true ->
	    report_err(Val, Mfa, wrongValue)
    end;
check_size(Val, _, Mfa) ->
    report_err(Val, Mfa, wrongType).

check_enums(Val, Asn1, Enums, Mfa) ->
    Association = 
	if
	    integer(Val) -> lists:keysearch(Val, 2, Enums);
	    atom(Val)    -> lists:keysearch(Val, 1, Enums);
	    true         -> {error, wrongType}
    end,
    case Association of
	{value, {AliasIntName, Val2}} -> {value, Asn1#asn1_type.bertype, Val2};
	false                         -> report_err(Val, Mfa, wrongValue);
	{error, wrongType}            -> report_err(Val, Mfa, wrongType)
    end.

report_err(Val, undef, Err) ->
    {error, Err};
report_err(Val, Mfa, Err) ->
    user_err("Got ~p from ~w. Using ~w", [Val, Mfa, Err]),
    {error, Err}.

get_option(Key, Options, Default) ->
    case lists:keysearch(Key, 1, Options) of
	{value, {_Key, Value}} -> Value;
	_ -> Default
    end.

valid_pdu_type('get-request') -> true;
valid_pdu_type('get-next-request') -> true;
valid_pdu_type('get-bulk-request') -> true;
valid_pdu_type('set-request') -> true;
valid_pdu_type(_) -> false.

get_pdu_data() ->
    {get(snmp_net_if_data), get(snmp_request_id),
     get(snmp_address), get(snmp_community), get(snmp_context)}.

put_pdu_data({Extra, ReqId, Address, Community, ContextName}) -> 
    put(snmp_net_if_data, Extra),
    put(snmp_address, Address),
    put(snmp_request_id, ReqId),
    put(snmp_community, Community),
    put(snmp_context, ContextName).

tr_var(Oid, Idx) ->
    case snmp_misc:is_oid(Oid) of
	true ->
	    {#varbind{oid = Oid, value = unSpecified, org_index = Idx},
	     Idx+1};
	false -> throw({error, {bad_oid, Oid}})
    end.

tr_varbind(#varbind{value = Value}) -> Value.

mapfoldl(F, Eas, Accu0, [Hd|Tail]) ->
    {R,Accu1} = apply(F, [Hd,Accu0|Eas]),
    {Accu2,Rs} = mapfoldl(F, Eas, Accu1, Tail),
    {Accu2,[R|Rs]};
mapfoldl(F, Eas, Accu, []) -> {Accu,[]}.

%%-----------------------------------------------------------------
%% Runtime debugging of the agent.
%%-----------------------------------------------------------------

dbg_apply(M,F,A) ->
    case get(verbosity) of
	silence -> apply(M,F,A);
	_ ->
	    ?vlog("~n   apply: ~w,~w,~p~n", [M,F,A]),
	    Res = (catch apply(M,F,A)),
	    ?vlog("~n   returned: ~p", [Res]),
	    Res
    end.


short_name(none) -> ma;
short_name(Pid)  -> sa.

worker_short_name(ma) -> maw;
worker_short_name(_)  -> saw.

trap_sender_short_name(ma) -> mats;
trap_sender_short_name(_)  -> sats.

pdu_handler_short_name(ma) -> maph;
pdu_handler_short_name(_)  -> saph.

get_verbosity(_,[]) ->
    ?default_verbosity;
get_verbosity(none,L) ->
    snmp_misc:get_option(master_agent_verbosity,L,?default_verbosity);
get_verbosity(_,L) ->
    snmp_misc:get_option(subagent_verbosity,L,?default_verbosity).


net_if_verbosity(Pid,Verbosity) when pid(Pid) ->
    Pid ! {verbosity,Verbosity};
net_if_verbosity(_Pid,_Verbosity) ->
    ok.


mib_verbosity(Pid,Verbosity) when pid(Pid) ->
    snmp_mib:verbosity(Pid,Verbosity);
mib_verbosity(_Pid,_Verbosity) ->
    ok.

d2v(true) -> log;
d2v(_)    -> silence.


subagents_verbosity(V) ->
    subagents_verbosity(catch snmp_mib:info(get(mibserver),subagents),V).

subagents_verbosity([],_V) ->
    ok;
subagents_verbosity([{Pid,_Oid}|T],V) ->
    catch snmp_agent:verbosity(Pid,V),             %% On the agent
    catch snmp_agent:verbosity(Pid,{subagents,V}), %% and it's subagents
    subagents_verbosity(T,V);
subagents_verbosity(_,_V) ->
    ok.


user_err(F, A) ->
    snmp_error_report:user_err(F, A).
