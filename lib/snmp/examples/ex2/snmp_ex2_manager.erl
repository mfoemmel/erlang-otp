%%<copyright>
%% <year>2006-2007</year>
%% <holder>Ericsson AB, All Rights Reserved</holder>
%%</copyright>
%%<legalnotice>
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson AB.
%%</legalnotice>
%%
%%----------------------------------------------------------------------
%% This module examplifies how to write test suites for your SNMP agent.
%%----------------------------------------------------------------------

-module(snmp_ex2_manager).

-behaviour(gen_server).
-behaviour(snmpm_user).

-export([start_link/0, start_link/1, stop/0,
	 agent/2, agent/3,
         sync_get/2,      sync_get/3,
         sync_get_next/2, sync_get_next/3,
         sync_get_bulk/4, sync_get_bulk/5,
         sync_set/2,      sync_set/3,

	 oid_to_name/1
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

-include_lib("snmp/include/snmp_types.hrl").


-define(SERVER,   ?MODULE).
-define(USER,     ?MODULE).
-define(USER_MOD, ?MODULE).

-record(state, {parent}).


%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

start_link() ->
    start_link([]).

start_link(Opts) when is_list(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [self(), Opts], []).

stop() ->
    cast(stop).


%% --- Instruct manager to handle an agent ---

agent(Addr, Conf) ->
    call({agent, Addr, Conf}).

agent(Addr, Port, Conf) ->
    call({agent, Addr, Port, Conf}).


%% --- Misc utility functions ---

oid_to_name(Oid) ->
    call({oid_to_name, Oid}).


%% --- Various SNMP operations ----

sync_get(Addr, Oids) ->
    call({sync_get, Addr, Oids}).

sync_get(Addr, Port, Oids) ->
    call({sync_get, Addr, Port, Oids}).


sync_get_next(Addr, Oids) ->
    call({sync_get_next, Addr, Oids}).

sync_get_next(Addr, Port, Oids) ->
    call({sync_get_next, Addr, Port, Oids}).


sync_get_bulk(Addr, NR, MR, Oids) ->
    call({sync_get_bulk, Addr, NR, MR, Oids}).

sync_get_bulk(Addr, Port, NR, MR, Oids) ->
    call({sync_get_bulk, Addr, Port, NR, MR, Oids}).


sync_set(Addr, VarsAndVals) ->
    call({sync_set, Addr, VarsAndVals}).

sync_set(Addr, Port, VarsAndVals) ->
    call({sync_set, Addr, Port, VarsAndVals}).


%%%-------------------------------------------------------------------
%%% Callback functions from gen_server
%%%-------------------------------------------------------------------

init([Parent, Opts]) ->
    process_flag(trap_exit, true),
    case (catch do_init(Opts)) of
        {ok, State} ->
            {ok, State#state{parent = Parent}};
        {error, Reason} ->
            {stop, Reason};
	Crap ->
	    {stop, Crap}
    end.

do_init(Opts) ->
    {Dir, MgrConf, MgrOpts} = parse_opts(Opts),
    write_config(Dir, MgrConf),
    start_manager(MgrOpts),
    register_user(),
    {ok, #state{}}.

write_config(Dir, Conf) ->
    case snmp_config:write_manager_config(Dir, "", Conf) of
	ok ->
	    ok;
	Error ->
	    error({failed_writing_config, Error})
    end.

start_manager(Opts) ->
    case snmpm:start_link(Opts) of
	ok ->
	    ok; 
	Error ->
	    error({failed_starting_manager, Error})
    end.

register_user() ->
    case snmpm:register_user(?USER, ?USER_MOD, self()) of
	ok ->
	    ok;
	Error ->
	    error({failed_register_user, Error})
    end.

parse_opts(Opts) ->
    Port     = get_opt(port,             Opts, 5000),
    EngineId = get_opt(engine_id,        Opts, "mgrEngine"),
    MMS      = get_opt(max_message_size, Opts, 484),

    MgrConf = [{port,             Port},
               {engine_id,        EngineId},
               {max_message_size, MMS}],

    %% Manager options
    Mibs      = get_opt(mibs,     Opts, []),
    Vsns      = get_opt(versions, Opts, [v1, v2, v3]),
    {ok, Cwd} = file:get_cwd(),
    Dir       = get_opt(dir, Opts, Cwd),
    MgrOpts   = [{mibs,     Mibs},
		 {versions, Vsns}, 
		 %% {server,   [{verbosity, trace}]}, 
		 {config,   [% {verbosity, trace}, 
			     {dir, Dir}, {db_dir, Dir}]}],
    
    {Dir, MgrConf, MgrOpts}.


%%--------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------

handle_call({agent, Addr, Conf}, _From, S) ->
    Reply = (catch snmpm:register_agent(?USER, Addr, Conf)),
    {reply, Reply, S};

handle_call({agent, Addr, Port, Conf}, _From, S) ->
    Reply = (catch snmpm:register_agent(?USER, Addr, Port, Conf)),
    {reply, Reply, S};

handle_call({oid_to_name, Oid}, _From, S) ->
    Reply = (catch snmpm:oid_to_name(Oid)),
    {reply, Reply, S};

handle_call({sync_get, Addr, Oids}, _From, S) ->
    Reply = (catch snmpm:g(?USER, Addr, Oids)),
    {reply, Reply, S};

handle_call({sync_get, Addr, Port, Oids}, _From, S) ->
    Reply = (catch snmpm:g(?USER, Addr, Port, Oids)),
    {reply, Reply, S};

handle_call({sync_get_next, Addr, Oids}, _From, S) ->
    Reply = (catch snmpm:gn(?USER, Addr, Oids)),
    {reply, Reply, S};

handle_call({sync_get_next, Addr, Port, Oids}, _From, S) ->
    Reply = (catch snmpm:gn(?USER, Addr, Port, Oids)),
    {reply, Reply, S};

handle_call({sync_get_bulk, Addr, NR, MR, Oids}, _From, S) ->
    Reply = (catch snmpm:gb(?USER, Addr, NR, MR, Oids)),
    {reply, Reply, S};

handle_call({sync_get_bulk, Addr, Port, NR, MR, Oids}, _From, S) ->
    Reply = (catch snmpm:gb(?USER, Addr, Port, NR, MR, Oids)),
    {reply, Reply, S};

handle_call({sync_set, Addr, VarsAndVals}, _From, S) ->
    Reply = (catch snmpm:s(?USER, Addr, VarsAndVals)),
    {reply, Reply, S};

handle_call({sync_set, Addr, Port, VarsAndVals}, _From, S) ->
    Reply = (catch snmpm:s(?USER, Addr, Port, VarsAndVals)),
    {reply, Reply, S};

handle_call(Req, From, State) ->
    error_msg("received unknown request ~n~p~nFrom ~p", [Req, From]),
    {reply, {error, {unknown_request, Req}}, State}.


%%--------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast(stop, S) ->
    (catch snmpm:stop()),
    {stop, normal, S};

handle_cast(Msg, State) ->
    error_msg("received unknown message ~n~p", [Msg]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_info({snmp_callback, Tag, Info}, State) ->
    handle_snmp_callback(Tag, Info),
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


%% ========================================================================
%% ========================================================================

handle_snmp_callback(handle_error, {ReqId, Reason}) ->
    io:format("*** FAILURE ***"
	      "~n   Request Id: ~p"
	      "~n   Reason:     ~p"
	      "~n", [ReqId, Reason]),
    ok;
handle_snmp_callback(handle_agent, {Addr, Port, SnmpInfo}) ->
    {ES, EI, VBs} = SnmpInfo, 
    io:format("*** UNKNOWN AGENT ***"
	      "~n   Address:   ~p"
	      "~n   Port:      ~p"
	      "~n   SNMP Info: "
	      "~n     Error Status: ~w"
	      "~n     Error Index:  ~w"
	      "~n     Varbinds:     ~p"
	      "~n", [Addr, Port, ES, EI, VBs]),
    ok;
handle_snmp_callback(handle_pdu, {Addr, Port, ReqId, SnmpResponse}) ->
    {ES, EI, VBs} = SnmpResponse, 
    io:format("*** Received PDU ***"
	      "~n   Address:       ~p"
	      "~n   Port:          ~p"
	      "~n   Request Id:    ~p"
	      "~n   SNMP response:"
	      "~n     Error Status: ~w"
	      "~n     Error Index:  ~w"
	      "~n     Varbinds:     ~p"
	      "~n", [Addr, Port, ReqId, ES, EI, VBs]),
    ok;
handle_snmp_callback(handle_trap, {Addr, Port, SnmpTrap}) ->
    TrapStr = 
	case SnmpTrap of
	    {Enteprise, Generic, Spec, Timestamp, Varbinds} ->
		io_lib:format("~n     Generic:    ~w"
			      "~n     Exterprise: ~w"
			      "~n     Specific:   ~w"
			      "~n     Timestamp:  ~w"
			      "~n     Varbinds:   ~p", 
			      [Generic, Enteprise, Spec, Timestamp, Varbinds]);
	    {ErrorStatus, ErrorIndex, Varbinds} ->
		io_lib:format("~n     Error Status: ~w"
			      "~n     Error Index:  ~w"
			      "~n     Varbinds:     ~p"
			      "~n", [ErrorStatus, ErrorIndex, Varbinds])
	end,
    io:format("*** Received TRAP ***"
	      "~n   Address:   ~p"
	      "~n   Port:      ~p"
	      "~n   SNMP trap: ~s"
	      "~n", [Addr, Port, lists:flatten(TrapStr)]),
    ok;
handle_snmp_callback(handle_inform, {Addr, Port, SnmpInform}) ->
    {ES, EI, VBs} = SnmpInform, 
    io:format("*** Received INFORM ***"
	      "~n   Address:     ~p"
	      "~n   Port:        ~p"
	      "~n   SNMP inform: "
	      "~n     Error Status: ~w"
	      "~n     Error Index:  ~w"
	      "~n     Varbinds:     ~p"
	      "~n", [Addr, Port, ES, EI, VBs]),
    ok;
handle_snmp_callback(handle_report, {Addr, Port, SnmpReport}) ->
    {ES, EI, VBs} = SnmpReport, 
    io:format("*** Received REPORT ***"
	      "~n   Address:   ~p"
	      "~n   Port:      ~p"
	      "~n   SNMP report: "
	      "~n     Error Status: ~w"
	      "~n     Error Index:  ~w"
	      "~n     Varbinds:     ~p"
	      "~n", [Addr, Port, ES, EI, VBs]),
    ok;
handle_snmp_callback(BadTag, Crap) ->
    io:format("*** Received crap ***"
	      "~n   ~p"
	      "~n   ~p"
	      "~n", [BadTag, Crap]),
    ok.
    


error(Reason) ->
    throw({error, Reason}).


error_msg(F, A) ->
    catch error_logger:error_msg("*** TEST-MANAGER: " ++ F ++ "~n", A).


call(Req) ->
    gen_server:call(?SERVER, Req, infinity).

cast(Msg) ->
    gen_server:cast(?SERVER, Msg).


%% ========================================================================
%% Misc internal utility functions
%% ========================================================================

%% get_opt(Key, Opts) ->
%%     case lists:keysearch(Key, 1, Opts) of
%%         {value, {Key, Val}} ->
%%             Val;
%%         false ->
%%             throw({error, {missing_mandatory, Key}})
%%     end.

get_opt(Key, Opts, Def) ->
    case lists:keysearch(Key, 1, Opts) of
        {value, {Key, Val}} ->
            Val;
        false ->
            Def
    end.


%% ========================================================================
%% SNMPM user callback functions
%% ========================================================================

handle_error(ReqId, Reason, Server) when is_pid(Server) ->
    report_callback(Server, handle_error, {ReqId, Reason}),
    ignore.


handle_agent(Addr, Port, SnmpInfo, Server) when is_pid(Server) ->
    report_callback(Server, handle_agent, {Addr, Port, SnmpInfo}),
    ignore.


handle_pdu(Addr, Port, ReqId, SnmpResponse, Server) when is_pid(Server) ->
    report_callback(Server, handle_pdu, {Addr, Port, ReqId, SnmpResponse}),
    ignore.


handle_trap(Addr, Port, SnmpTrap, Server) when is_pid(Server) ->
    report_callback(Server, handle_trap, {Addr, Port, SnmpTrap}),
    ok.

handle_inform(Addr, Port, SnmpInform, Server) when is_pid(Server) ->
    report_callback(Server, handle_inform, {Addr, Port, SnmpInform}),
    ok.


handle_report(Addr, Port, SnmpReport, Server) when is_pid(Server) ->
    report_callback(Server, handle_inform, {Addr, Port, SnmpReport}),
    ok.

report_callback(Pid, Tag, Info) ->
    Pid ! {snmp_callback, Tag, Info}.
