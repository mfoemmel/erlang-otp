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
%%----------------------------------------------------------------------
%% This module implements an SNMP manager used in the test suite
%%----------------------------------------------------------------------
%%

-module(snmp_test_manager).

-behaviour(gen_server).
-behaviour(snmpm_user).


%% External exports
-export([
	 start_link/0, start_link/1,
	 stop/0, 

	 sync_get/1,      sync_get/2, 
	 sync_get_next/1, sync_get_next/2, 
	 sync_get_bulk/3, 
	 sync_set/1,      sync_set/2
	]).


%% Manager callback API:
-export([handle_error/3,
         handle_agent/4,
         handle_pdu/5,
         handle_trap/4,
         handle_inform/4,
         handle_report/4]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 code_change/3, terminate/2]).

-record(state, {mgr, parent, req, addr, port}).

-define(SERVER, ?MODULE).
-define(USER,   ?MODULE).


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start_link() ->
    start_link([]).

start_link(Opts) -> 
    gen_server:start_link({local, ?SERVER}, ?MODULE, [self(), Opts], []).

stop() ->
    call(stop).


sync_get(Oids) ->
    sync_get(Oids, fun(X) -> {ok, X} end).

sync_get(Oids, Verify) when is_list(Oids) and is_function(Verify) ->
    Verify(call({sync_get, Oids})).


sync_get_next(Oids) ->
    sync_get_next(Oids, fun(X) -> {ok, X} end).

sync_get_next(Oids, Verify) when is_list(Oids) and is_function(Verify) ->
    Verify(call({sync_get_next, Oids})).


sync_get_bulk(NR, MR, Oids) ->
    sync_get_bulk(NR, MR, Oids, fun(X) -> {ok, X} end).

sync_get_bulk(NR, MR, Oids, Verify) 
  when is_integer(NR) and is_integer(MR) and 
       is_list(Oids) and is_function(Verify) ->
    Verify(call({sync_get_bulk, NR, MR, Oids})).


sync_set(VarsAndVals) ->
    sync_set(VarsAndVals, fun(X) -> {ok, X} end).

sync_set(VarsAndVals, Verify) 
  when is_list(VarsAndVals) and is_function(Verify) ->
    Verify(call({sync_set, VarsAndVals})).


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
init([Parent, Opts]) ->
    process_flag(trap_exit, true),
    case (catch do_init(Opts)) of
	{ok, State} ->
	    {ok, State#state{parent = Parent}};
	{error, Reason} ->
	    {stop, Reason}
    end.

do_init(Opts) ->
    {MgrDir, MgrConf, MgrOpts, Addr, Port, AgentConf} = parse_opts(Opts),
    ok = snmp_config:write_manager_config(MgrDir, "", MgrConf),
    {ok, Pid} = snmpm:start_link(MgrOpts),
    ok = snmpm:register_user(?USER, ?MODULE, self()),
    ok = snmpm:register_agent(?USER, Addr, Port, AgentConf),
    {ok, #state{mgr = Pid, addr = Addr, port = Port}}.


parse_opts(Opts) ->
    %% Manager config (written to the manager.conf file)
    %% Addr     = get_opt(addr,      Opts, ?HOSTNAME()),
    Port     = get_opt(port,      Opts, 5000),
    EngineId = get_opt(engine_id, Opts, "mgrEngine"),
    MMS      = get_opt(max_message_size, Opts, 484),

    MgrConf = [%% {address,          Addr},
	       {port,             Port},
	       {engine_id,        EngineId},
	       {max_message_size, MMS}],

    
    %% Manager options
    MgrOpts = get_opt(options, Opts),
    MgrDir  = get_opt(dir, get_opt(config,  MgrOpts, [])),
    
    
    %% Retreive the agent configuration
    AgentConf = get_opt(agent_config, Opts),
    AgentAddr = get_opt(agent_addr, Opts),
    AgentPort = get_opt(agent_port, Opts),
%     AgentTargetName = get_opt(agent_target_name, Opts, "agent"),
%     AgentMMS        = get_opt(agent_mms,  Opts, 484),
%     AgentEngineId   = get_opt(agent_engine_id, Opts, "agentEngine"),
%     AgentSecName    = get_opt(agent_sec_name, Opts, "initial"),
%     AgentSecLevel   = get_opt(agent_sec_level, Opts, noAuthNoPriv),
%     AgentSecModel   = get_opt(agent_sec_model, Opts, v1),
%     AgentVersion    = get_opt(agent_version, Opts, v1),
%     AgentCtx        = get_opt(agent_context, Opts, ""),
%     AgentComm       = get_opt(agent_community, Opts, "all-rights"),

%     AgentConf = [{target_name,      AgentTargetName},
% 		 {community,        AgentComm},
% 		 {engine_id,        AgentEngineId},
% 		 {max_message_size, AgentMMS},
% 		 {version,          AgentVersion},
% 		 {sec_model,        AgentSecModel},
% 		 {sec_name,         AgentSecName},
% 		 {sec_level,        AgentSecLevel}],

    {MgrDir, MgrConf, MgrOpts, AgentAddr, AgentPort, AgentConf}.


get_opt(Key, Opts) ->
    case lists:keysearch(Key, 1, Opts) of
	{value, {Key, Val}} ->
	    Val;
	false ->
	    throw({error, {missing_mandatory, Key}})
    end.

get_opt(Key, Opts, Def) ->
    case lists:keysearch(Key, 1, Opts) of
	{value, {Key, Val}} ->
	    Val;
	false ->
	    Def
    end.


%%--------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_call(stop, _From, S) ->
    (catch snmpm:stop()),
    {stop, normal, S};

handle_call({sync_get, Oids}, _From, 
	    #state{addr = Addr, port = Port} = S) ->
    Reply = (catch snmpm:g(?USER, Addr, Port, Oids)),
    {reply, Reply, S};

handle_call({sync_get_next, Oids}, _From, 
	    #state{addr = Addr, port = Port} = S) ->
    Reply = (catch snmpm:gn(?USER, Addr, Port, Oids)),
    {reply, Reply, S};

handle_call({sync_get_bulk, NR, MR, Oids}, _From, 
	    #state{addr = Addr, port = Port} = S) ->
    Reply = (catch snmpm:gb(?USER, Addr, Port, NR, MR, Oids)),
    {reply, Reply, S};

handle_call({sync_set, VarsAndVals}, _From, 
	    #state{addr = Addr, port = Port} = S) ->
    Reply = (catch snmpm:s(?USER, Addr, Port,VarsAndVals)),
    {reply, Reply, S};

handle_call(Req, From, State) ->
    error_msg("received unknown request ~n~p~nFrom ~p", [Req, From]),
    {reply, {error, unknown_request}, State}.


%%--------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    error_msg("received unknown message ~n~p", [Msg]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({snmp_error, ReqId, Reason}, 
	    #state{parent = P} = State) ->
    info_msg("received snmp error: "
	      "~n   ReqId:  ~w"
	      "~n   Reason: ~p", [ReqId, Reason]),
    P ! {snmp_error, ReqId, Reason}, 
    {noreply, State};

handle_info({snmp_agent, Addr, Port, Info, Pid}, 
	    #state{parent = P} = State) ->
    error_msg("detected new agent: "
	      "~n   Addr: ~w"
	      "~n   Port: ~p"
	      "~n   Info: ~p", [Addr, Port, Info]),
    Pid ! {snmp_agent_reply, ignore, self()},
    P ! {snmp_agent, Addr, Port, Info},
    {noreply, State};

handle_info({snmp_pdu, Addr, Port, ReqId, Resp}, 
	    #state{parent = P} = State) ->
    info_msg("received snmp pdu: "
	      "~n   Addr:  ~w"
	      "~n   Port:  ~w"
	      "~n   ReqId: ~w"
	      "~n   Resp:  ~p", [Addr, Port, ReqId, Resp]),
    P ! {snmp_pdu, Addr, Port, ReqId, Resp}, 
    {noreply, State};

handle_info({snmp_trap, Addr, Port, Info, Pid}, 
	    #state{parent = P} = State) ->
    info_msg("received snmp trap: "
	      "~n   Addr: ~w"
	      "~n   Port: ~w"
	      "~n   Info: ~p", [Addr, Port, Info]),
    Pid ! {snmp_trap_reply, ignore, self()},
    P ! {snmp_trap, Addr, Port, Info}, 
    {noreply, State};

handle_info({snmp_inform, Addr, Port, Info, Pid}, 
	    #state{parent = P} = State) ->
    info_msg("received snmp inform: "
	      "~n   Addr: ~w"
	      "~n   Port: ~w"
	      "~n   Info: ~p", [Addr, Port, Info]),
    Pid ! {snmp_inform_reply, ignore, self()},
    P ! {snmp_inform, Addr, Port, Info}, 
    {noreply, State};

handle_info({snmp_report, Addr, Port, Info, Pid}, 
	    #state{parent = P} = State) ->
    info_msg("received snmp report: "
	      "~n   Addr: ~w"
	      "~n   Port: ~w"
	      "~n   Info: ~p", [Addr, Port, Info]),
    Pid ! {snmp_report_reply, ignore, self()},
    P ! {snmp_report, Addr, Port, Info}, 
    {noreply, State};

handle_info(Info, State) ->
    error_msg("received unknown info: "
	      "~n   Info: ~p", [Info]),
    {noreply, State}.
    

%%--------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.


code_change({down, _Vsn}, State, _Extra) ->
    {ok, State};
  
% upgrade
code_change(_Vsn, State, _Extra) ->
    {ok, State}.


%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------




%% --------------------------------------------------------------------------
%% 
%%                   SNMP manager callback functions 	   
%% 
%% --------------------------------------------------------------------------

handle_error(ReqId, Reason, Pid) ->
    Pid ! {snmp_error, ReqId, Reason},
    ignore.


handle_agent(Addr, Port, SnmpInfo, Pid) ->
    Pid ! {snmp_agent, Addr, Port, SnmpInfo, self()},
    receive
	{snmp_agent_reply, Reply, Pid} ->
	    Reply
    after 10000 ->
	    ignore
    end.


handle_pdu(Addr, Port, ReqId, SnmpResponse, Pid) ->
    Pid ! {snmp_pdu, Addr, Port, ReqId, SnmpResponse},
    ignore.


handle_trap(Addr, Port, SnmpTrapInfo, Pid) ->
    Pid ! {snmp_trap, Addr, Port, SnmpTrapInfo, self()},
    receive
	{snmp_trap_reply, Reply, Pid} ->
	    Reply
    after 10000 ->
	    ignore
    end.


handle_inform(Addr, Port, SnmpInfo, Pid) ->
    Pid ! {snmp_inform, Addr, Port, SnmpInfo, self()},
    receive
	{snmp_inform_reply, Reply, Pid} ->
	    Reply
    after 10000 ->
	    ignore
    end.


handle_report(Addr, Port, SnmpInfo, Pid) ->
    Pid ! {snmp_report, Addr, Port, SnmpInfo, self()},
    receive
	{snmp_report_reply, Reply, Pid} ->
	    Reply
    after 10000 ->
	    ignore
    end.
    

%%----------------------------------------------------------------------
         
call(Req) ->
    gen_server:call(?SERVER, Req, infinity).
 
% cast(Msg) ->
%     gen_server:cast(?SERVER, Msg).
 
info_msg(F, A) ->
    catch error_logger:info_msg("*** TEST-MANAGER: " ++ F ++ "~n", A).
  
error_msg(F, A) ->
    catch error_logger:error_msg("*** TEST-MANAGER: " ++ F ++ "~n", A).
  
 
