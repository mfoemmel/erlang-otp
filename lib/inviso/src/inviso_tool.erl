-module(inviso_tool).


%% This is the inviso tool, which is a tool using the inviso trace application.
%% It is developed to make tracing using trace cases possible in an environment
%% of distributed Erlang nodes.
%% A current restriction is that the Erlang nodes are supposed to have the same
%% code. This since inviso tool can at this point not handle subsets of nodes.
%% Instead all participating Erlang nodes are treated the same.
%%
%% The main functionality of the inviso tool are:
%%
%% (1) Handles start and stop of tracing at participating nodes.
%% (2) Interprets trace-case files at a distributed network level.
%%     (The inviso runtime component is responsible for interpreting
%%      trace cases at a local level, if run in an autostart).
%% (3) Keeps a command history log from which:
%%     (a) Sequences easily can be repeated.
%%     (b) Autostart configuration files can be created (understood by the
%%         default inviso autostart mechanism.
%% (4) Performs reactivation in case tracing is suspended (manually or by
%%     an overload mechanism.
%% (5) Uses the reactivation mechanism to start tracing at restarted nodes.

%% Distributed Erlang
%% ------------------
%% Inviso is built to run in a distributed environment. At this moment the
%% inviso tool must run at the same node as the inviso control component

%% The history mechanism
%% ---------------------
%% The built in history mechanism serves the following purposes:
%% a) Make it possible to reactivate suspended nodes. Including making a
%%    restarted node to "catch-up".
%% b) Generate autostart-scripts for use with runtime component autostart.
%% For this to work the history mechanism must not only keep a chronological
%% log over issued commands. The commands must also be classified as such
%% that shall be redone in case of a reactivation, and such that shall only
%% be repeated when a node is restarted. This history mechanism will cancel
%% out commands that acts as each other opposites.

%% List of all inviso commands that are supposed to be possible to do through
%% the inviso inviso tool API.

%% -----------------------------------------------------------------------------
%% API exports.
%% -----------------------------------------------------------------------------

-export([start/0,start/1,stop/0]).
-export([init_tracing/0,init_tracing/1,stop_tracing/0,tc/3,sync_tc/3,sync_tc/4]).
-export([sync_stop_tc/2,sync_stop_tc/3,stop_tc/2,inviso/2]).
-export([save_history/1,reactivate/0,reactivate/1,get_autostart_data/1,get_autostart_data/2]).
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% OTP exports and call backs.
%% -----------------------------------------------------------------------------

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2]).
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Internal exports.
%% -----------------------------------------------------------------------------

-export([tc_executer/4,reactivator_executer/6]).
-export([std_options_generator/1]).
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Constants.
%% -----------------------------------------------------------------------------

%% Defines the inviso function calls that shall be possible to do through the
%% inviso API in this tool.
-define(INVISO_CMDS,
	[{tp,5},{tp,4},{tp,1},{tpl,5},{tpl,4},{tpl,1},
	 {ctp,1},{ctp,2},{ctp,3},{ctpl,1},{ctpl,2},{ctpl,3},
	 {tf,2},{tf,1},{ctf,2},{ctf,1},{ctf_all,0},
	 {init_tpm,4},{init_tpm,7},
	 {tpm,4},{tpm,5},{tpm,8},
	 {tpm_tracer,4},{tpm_tracer,5},{init_tpm,8},
	 {tpm_ms,5},{tpm_ms_tracer,5},
	 {ctpm_ms,4},{ctpm,3},
	 {tpm_localnames,0},{ctpm_localnames,0},
	 {tpm_globalnames,0},{ctpm_globalnames,0},
	 {ctp_all,0},
	 {suspend,1},{cancel_suspension,0}]).
%% -----------------------------------------------------------------------------

%% These inviso functions shall be included in the command history log. Others
%% are not relevant to be redone during a recactivation nor an autostart.
-define(INVISO_CMD_HISTORY,
	[{tp,5},{tp,4},{tp,1},{tpl,5},{tpl,4},{tpl,1},
	 {ctp,1},{ctp,2},{ctp,3},{ctpl,1},{ctpl,2},{ctpl,3},
	 {tf,2},{tf,1},{ctf,2},{ctf,1},{ctf_all,0},
	 {init_tpm,4},{init_tpm,7},
	 {tpm,4},{tpm,5},{tpm,8},
	 {tpm_tracer,4},{tpm_tracer,5},{init_tpm,8},
	 {tpm_ms,5},{tpm_ms_tracer,5},
	 {ctpm_ms,4},{ctpm,3},
	 {tpm_localnames,0},{ctpm_localnames,0},
	 {tpm_globalnames,0},{ctpm_globalnames,0},
	 {ctp_all,0}]).
%% -----------------------------------------------------------------------------

%% Default max time to wait for a trace case called synchronously to return.
-define(SYNC_TC_TIMEOUT,10000).

%% Runtime components shall terminate when the tool terminates.
-define(DEFAULT_DEPENDENCY,{dependency,0}).
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Record definitions.
%% -----------------------------------------------------------------------------

%% The loopdata record.
-record(ld,{
	  dir=".",                          % Working dir of the tool.
	  nodes=down,                       % The nodesD database, defaults to non-distr.
	  c_node,                           % Location of inviso_c.
	  c_pid,                            % The inviso control component.
	  regexp_node,                      % Node for regexp expansions.
	  tc_dict,                          % Trace case definition db.
	  chl,                              % Command history log.
	  trace_state=new,                  % new | tracing | idle.
	  tdg={inviso_tool_lib,std_tdg,[]}, % Tracer data generator func.
	  tracer_data,                      % #td
	  reactivators=[],                  % Pids of now running reactivators.
	  tc_def_file,                      % Trace case definition file.
	  optg={?MODULE,std_options_generator,[]}, % Generates options to add_nodes/3.
	  initial_tcs=[],                   % Initial trace cases.
	  started_initial_tcs=[],           % Cases that must be stopped when stop_tracing.
	  history_prefix="",                % File path for history file.
	  keep_nodes=[],                    % Nodes that shall be left running.
	  keep_pattern=false,               % To keep trace pattern after stop.
	  debug=false                       % Internal debug mode
	  }).
%% -----------------------------------------------------------------------------


%% =============================================================================
%% API
%% =============================================================================

%% start()={ok,Pid} | {error,{already_started,pid()}}
%% start(Config)
%%   Config=[{Opt,Value},...], list of tuple options.
%%     Opt=dir|nodes|c_node|regexp_node|tdg|tc_def_file|optg|initial_tcs|
%%           history_prefix
%% Starts the inviso_tool process. Options in Config are the same as those
%% which are kept in the #ld structure.
start() ->
    start([]).
start(Config) ->
    gen_server:start({local,?MODULE},?MODULE,Config,[]).
%% -----------------------------------------------------------------------------

%% stop()={ok,NodeResults} | NodeResult | {error,Reason}
%%   NodeResults=[{Node,NodeResult},...]
%%   NodeResult=ok | {error,Reason} | patterns_untouched
%% Stops the inviso tool and the inviso control component. Runtime components are
%% stopped by them selves depending on their dependcy of the control component.
%% All runtime components that are not marked as to be kept will have their
%% trace patterns cleared before the inviso control component is shutdown.
%% The NodeResults indicates which nodes were successfullt handled.
stop() ->
    gen_server:call(?MODULE,stop).
%% -----------------------------------------------------------------------------

%% init_tracing()={ok,{SessionNr,InvisoReturn}} | {error,Reason}
%% init_tracing(MoreTDGargs)=
%%   MoreTDGargs=list(), prepended to the fixed list of args used when calling the
%%     tracer data generator function.
%%   SessionNr=integer(), trace sessions are numbered by the tool.
%%   InvisoReturn=If successful inviso call, the returnvalue from inviso.
%%     Note that individual nodes may be unsuccessful. See inviso:init_tracing/1
%% Initiates tracing at all participating nodes.
init_tracing() ->
    init_tracing([]).
init_tracing(MoreTDGargs) ->
    gen_server:call(?MODULE,{init_tracing,MoreTDGargs}).
%% -----------------------------------------------------------------------------

%% stop_tracing()={ok,SessionNr} | {error,Reason}
%%   SessionNr=integer()
%% Stops inviso tracing at all participating nodes. The inviso runtime components
%% will go to state idle. It is now time to fetch the logfiles. Will most often
%% succeed. Will only return an error if the entire inviso call returned an
%% error. Not if an individual node failed stop tracing successfully.
%% Any running trace case, including reactivator processes will be terminated.
stop_tracing() ->
    gen_server:call(?MODULE,stop_tracing).
%% -----------------------------------------------------------------------------

%% save_history(FileName)={ok,AbsFileName} | {error,Reason}
%% Saves the currently collected command history log to a file. The file will
%% be a binary-file named to #ld.history_prefix++FileName. (Note really ++).
save_history(FileName) ->
    gen_server:call(?MODULE,{save_history,FileName}).
%% -----------------------------------------------------------------------------

%% tc(TC,Id,Vars)=ok | {error,Reason}
%%   TC=atom(), name of the trace case.
%%   Id=term(), given name of this usage of TC.
%%   Vars=list(), list of variable bindings [{Var,Value},...], Var=atom(),Value=term().
%% Function activating a trace case. The trace case must be defined in the
%% trace case dictionary. The 'ok' return value is only a signal that the
%% trace case has started successfully. It may then run for as long as it is
%% programmed to run. An erroneous return value does not necessarily mean that
%% the trace case has not been executed. It rather means that is undetermined
%% what happend.
tc(TC,Id,Vars) ->
    gen_server:call(?MODULE,{tc,{TC,Id,Vars}}).
%% -----------------------------------------------------------------------------

%% sync_tc(TC,Id,Vars)=Result | {error,Reason}
%% sync_tc(TC,Id,Vars,TimeOut)=
%%   Result=term(), what ever is returned be the last expression in the trace case.
%%   TimeOut=interger() | infinity, the max wait time for the trace case to finnish.
%% As tc/3 but waits for the trace case to finish.
sync_tc(TC,Id,Vars) ->
    gen_server:call(?MODULE,{sync_tc,{TC,Id,Vars,?SYNC_TC_TIMEOUT}}).
sync_tc(TC,Id,Vars,TimeOut) ->
    gen_server:call(?MODULE,{sync_tc,{TC,Id,Vars,TimeOut}}).
%% -----------------------------------------------------------------------------

%% stop_tc(TC,Id)=ok | {error,Reason}
%% Deactivates a previosly activated trace case. This function can only be used
%% on trace cases that has a deactivation defined in the trace case dictionary.
%% There is of course really no difference between a file containing an activation
%% compared to a deactivation. But to be able cancelling activations out from the
%% history log, a defined deactivation is essential.
%% As with activation, the returned 'ok' simply indicates the start of the trace
%% case.
stop_tc(TC,Id) ->
    gen_server:call(?MODULE,{tc_off,{TC,Id}}).
%% -----------------------------------------------------------------------------


sync_stop_tc(TC,Id) ->
    gen_server:call(?MODULE,{sync_tc_off,{TC,Id,?SYNC_TC_TIMEOUT}}).
sync_stop_tc(TC,Id,TimeOut) ->
    gen_server:call(?MODULE,{sync_tc_off,{TC,Id,TimeOut}}).
%% -----------------------------------------------------------------------------

%% inviso(Cmd,Args)=Value
%%   Cmd=atom(), the (inviso) function name that shall be called.
%%   Args=list(), the arguments to Cmd.
%% This function executes a Cmd in the inviso tool context. The inviso call will
%% be logged in history log and thereby repeated in case of a reactivation.
%% Note that this function is intended for use with inviso function API without
%% specifying any nodes, since the function call is supposed to be carried out on
%% all nodes.
%% When these functions are written to an autostart config file by the tool there
%% is supposed to be a translation to inviso_rt functions.
inviso(Cmd,Args) ->
    gen_server:call(?MODULE,{inviso,{Cmd,Args}}).
%% -----------------------------------------------------------------------------

%% reactivate()=ok | {error,Reason}
%% reactivate(Node)=ok | {error,Reason}
%% Moves a runtime component from suspended to the state running. This can be
%% done for both tracing and inactive nodes. The later is necessary since you
%% may have stopped tracing with a node suspended.
%% In case the node is tracing, commands in the command history log are redone at
%% the node in questions.
%% Note that this function returns 'ok' before the node is running. This because the
%% the reactivated history is done by a separate process and there is no guarantee
%% when it will be ready. The reactivated node will not be marked as running in
%% the tool until done reactivating.
reactivate() ->                             % Non-distributed API.
    reactivate(node()).
reactivate(Node) ->
    gen_server:call(?MODULE,{reactivate,Node}).
%% -----------------------------------------------------------------------------

%% get_autostart_data(Nodes,Dependency)={ok,{AutoStartData,NodeResults} |
%%     {ok,{AutoStartData,NodeResult}} | {error,Reason}
%%   Dependency=inviso dependency parameter which will be used for every
%%     autostarted runtime component (included in Options).
%%   NodeResults=[{Node,NodeResult},...]
%%     NodeResult={ok,{Options,TracerData}} | {error,Reason}
%%       Options=add_nodes options to the inviso runtime component.
%%       TracerData=init tracing parameter to the runtime component.
%%       AutostartData=[CaseSpec,...]
%%         CaseSpec={file,{FileName,Bindings}} | {bin,FileContent}
%%           FileName=string(), pointing out the trace case file. Note that this
%%             is the same as the path used by the tool.
%%           Bindings=Var bindings used according to the history for the
%%             invocation.
%%           FileContent=binary(), to be written to a text file. This is for
%%             direct inviso commands found in the history. Such must be written
%%             to a self-made trace case file in order for the script execution
%%             mechanism to perform them.
%% Function returning information on how to autostart a node to make it trace
%% according to the current history. The inviso_tool does not know how to write
%% the necessary files at the nodes in question. That must be done by the user
%% of the tool, guided by the return value from this function.
%% Note that there will be two types of trace case files. Regular trace case
%% files and binaries returned from this function. The latter contains the
%% inviso commands which have been executed. Note that the order amongst the
%% trace cases and binaries is of importance (otherwise they will be redone in
%% an incorrect order).
get_autostart_data(Dependency) ->
    gen_server:call(?MODULE,{get_autostart_data,Dependency}).
get_autostart_data(Nodes,Dependency) ->
    gen_server:call(?MODULE,{get_autostart_data,{Nodes,Dependency}}).
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Internal APIs.
%% -----------------------------------------------------------------------------

%% tc_executer_reply(To,Reply)=nothing significant
%%   To=pid()
%%   Reply=term()
%% Internal API used by a trace case executer process to signal its completion.
tc_executer_reply(To,Reply) ->
    gen_server:cast(To,{tc_executer_reply,Reply}).
%% -----------------------------------------------------------------------------

%% Internal API used by a reactivator process indicating it is done with the
%% history log it has got so far.
reactivator_reply(TPid,Counter) ->
    gen_server:call(TPid,{reactivator_reply,{Counter,self()}}).
%% -----------------------------------------------------------------------------



%% =============================================================================
%% gen_server implementation.
%% =============================================================================

init(Config) ->
    case fetch_configuration(Config) of     % From conf-file and Config.
	{ok,LD} when record(LD,ld) ->
	    case start_inviso_at_c_node(LD) of
		{ok,CPid} ->
		    LD2=start_runtime_components(LD),
		    LD3=read_trace_case_definitions(LD2),
		    process_flag(trap_exit,true),
		    start_subscribe_inviso_events(LD3#ld.c_node),
		    {ok,LD3#ld{c_pid=CPid}};
		{error,Reason} ->           % Most likely already running.
		    {stop,{error,Reason}}
	    end;
	{error,Reason} ->
	    {stop,{error,{start_up,Reason}}}
    end.
%% -----------------------------------------------------------------------------

%% Help function starting the inviso control component at node c_node, or "here"
%% if it is not a distributed network.
start_inviso_at_c_node(#ld{c_node=undefined}) -> % Non distributed case.
    case inviso:start() of
	{ok,Pid} ->
	    {ok,Pid};
	{error,Reason} ->
	    {error,Reason}
    end;
start_inviso_at_c_node(#ld{c_node=CNode}) ->
    case rpc:call(CNode,inviso,start,[]) of
	{ok,Pid} ->
	    {ok,Pid};
	{error,Reason} ->
	    {error,Reason};
	{badrpc,Reason} ->
	    {error,{inviso_control_node_error,Reason}}
    end.
%% -----------------------------------------------------------------------------

%% Help function starting the runtime components at all particapting nodes.
%% It also updates the nodes structure in the #ld to indicate which nodes where
%% successfully started. Returns a new #ld.
start_runtime_components(LD=#ld{c_node=undefined,optg=OptG}) ->
    Opts=start_runtime_components_mk_opts(local_runtime,OptG),
    case inviso:add_node(mk_rt_tag(),Opts) of
	{ok,NAnsw} ->                       % Should be more clever really!
	    NewNodesD=update_added_nodes({ok,NAnsw},LD#ld.nodes),
	    LD#ld{nodes=NewNodesD};
	{error,_Reason} ->
	    LD
    end;
start_runtime_components(LD=#ld{c_node=CNode,nodes=NodesD}) ->
    start_runtime_components_2(get_all_nodenames_nodes(NodesD),CNode,LD).

start_runtime_components_2([Node|Rest],CNode,LD=#ld{optg=OptG}) ->
    Opts=start_runtime_components_mk_opts(Node,OptG),
    case rpc:call(CNode,inviso,add_nodes,[[Node],mk_rt_tag(),Opts]) of
	{ok,NodeResults} ->
	    NewNodesD=update_added_nodes(NodeResults,LD#ld.nodes),
	    start_runtime_components_2(Rest,CNode,LD#ld{nodes=NewNodesD});
	{error,_Reason} ->
	    start_runtime_components_2(Rest,CNode,LD);
	{badrpc,_Reason} ->
	    start_runtime_components_2(Rest,CNode,LD)
    end;
start_runtime_components_2([],_,LD) ->
    LD.

start_runtime_components_mk_opts(Node,{M,F,Args}) ->
    case catch apply(M,F,[Node|Args]) of
	{ok,Opts} when list(Opts) ->
	    start_runtime_component_mk_opts_add_dependency(Opts);
	_ ->
	    [?DEFAULT_DEPENDENCY]
    end.

%% The options generator is not supposed to generate the dependency. Hence this
%% function adds and if necessary removes an incorrectly added dependency tag.
start_runtime_component_mk_opts_add_dependency(Opts) ->
    case lists:keysearch(dependency,1,Opts) of
	{value,_} ->                           % Not allowed!!!
	    [?DEFAULT_DEPENDENCY|lists:keydelete(dependecy,1,Opts)];
	false ->
	    [?DEFAULT_DEPENDENCY|Opts]
    end.
%% -----------------------------------------------------------------------------

%% Help function subscribing to inviso events from the inviso controller. This
%% will make it possible to follow runtime components going down.
start_subscribe_inviso_events(undefined) ->
    inviso:subscribe();
start_subscribe_inviso_events(CNode) ->
    rpc:call(CNode,inviso,subscribe,[self()]). % Don't want the rpc-proc to subscribe!
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% gen_server handle call back functions.
%% -----------------------------------------------------------------------------


handle_call({init_tracing,MoreTDGargs},_From,LD=#ld{trace_state=TState}) ->
    case is_tracing(TState) of
	false ->                               % No session running.
	    DateTime=calendar:local_time(),
	    {M,F,Args}=LD#ld.tdg,
	    TDGargs=[DateTime]++MoreTDGargs++Args,
	    case h_init_tracing(M,F,TDGargs,LD) of
		{ok,{SessionNr,ReturnVal,NewLD}} ->
		    NewLD2=do_initial_tcs(NewLD#ld.initial_tcs,NewLD),
		    {reply,
		     {ok,{SessionNr,ReturnVal}},
		     NewLD2#ld{trace_state=tracing_tracing()}};
		{error,Reason} ->
		    {reply,{error,Reason},LD}
	    end;
	true ->
	    {reply,{error,already_tracing},LD}
    end;

handle_call(stop,_From,LD=#ld{nodes=Nodes,c_node=CNode,keep_pattern=KP,keep_nodes=KNs}) ->
    {stop,stop,remove_all_trace_patterns(KP,CNode,KNs,Nodes),LD};

%% To stop tracing means stop_tracing through the inviso API. But we must also
%% remove any help processes executing inviso commands (trace case executers
%% and reactivators).
%% Note that to be really sure we should actually wait for EXIT-signals from those
%% processes before returning a successful returnvalue to the caller. In theory
%% those processes could issue an inviso call effecting a new trace session started
%% with init_tracing shortly after the call to stop_tracing. But too complicated! :-)
%% Further, stop-tracing is done on all nodes in our nodes structure. Regardless
%% if the node is tracing or not
handle_call(stop_tracing,_From,LD=#ld{trace_state=TState,chl=CHL,reactivators=ReAct}) ->
    case is_tracing(TState) of
	true ->
	    NewCHL=stop_all_tc_executer_chl(CHL), % Stop any running trace case proc.
	    NewReAct=stop_all_reactivators(ReAct),  % Stop any running reactivators.
	    case h_stop_tracing(LD) of
		{ok,{SessionNr,InactiveNodes}} ->
		    NewNodesD=set_inactive_nodes(InactiveNodes,LD#ld.nodes),
		    {reply,
		     {ok,SessionNr},
		     LD#ld{trace_state=idle_tracing(),
			   nodes=NewNodesD,
			   chl=NewCHL,
			   reactivators=NewReAct,
			   started_initial_tcs=[]}};
		{error,Reason} ->           % Now we're really in deep shit :-)
		    {reply,{error,{unrecoverable,Reason}},LD}
	    end;
	false ->
	    {reply,{error,not_tracing},LD}
    end;

handle_call({save_history,FileName},_From,LD=#ld{chl=CHL,history_prefix=Prefix}) ->
    case get_loghandler_chl(CHL) of
	undefined ->                        % We have not init_tracing any time.
	    {reply,{error,no_history},LD};
	_TId ->
	    SortedLog=lists:keysort(2,get_loglist_chl(CHL)),
	    case catch Prefix++FileName of
		FullFileName when list(FullFileName) ->
		    case file:write_file(FullFileName,term_to_binary(SortedLog)) of
			ok ->
			    {reply,{ok,FullFileName},LD};
			{error,Reason} ->
			    {reply,{error,{write_file,Reason}},LD}
		    end;
		{'EXIT',_Reason} ->         % Faulty FileName
		    {reply,{error,{badarg,FileName}},LD}
	    end
    end;

%% Calling a trace-case, or "turning it on".
handle_call({tc,{TC,Id,Vars}},_From,LD=#ld{trace_state=TState}) ->
    case is_tracing(TState) of              % Check that we are tracing now.
	true ->
	    case h_tc_on(TC,Id,Vars,LD) of
		{ok,NewLD} ->               % Trace case executed.
		    {reply,ok,NewLD};
		{error,Reason} ->
		    {reply,{error,Reason},LD}
	    end;
	false ->                            % Can't activate if not tracing.
	    {reply,{error,not_tracing},LD}
    end;

handle_call({sync_tc,{TC,Id,Vars,TimeOut}},_From,LD=#ld{trace_state=TState}) ->
    case is_tracing(TState) of
	true ->
	    case h_sync_tc_on(TC,Id,Vars,TimeOut,LD) of
		{ok,NewLD,Result} ->
		    {reply,Result,NewLD};
		{error,Reason} ->
		    {reply,{error,Reason},LD}
	    end;
	false ->
	    {reply,{error,not_tracing},LD}
    end;

handle_call({tc_off,{TC,Id}},_From,LD=#ld{trace_state=TState}) ->
    case is_tracing(TState) of              % Check that we are tracing now.
	true ->
	    case h_tc_off(TC,Id,LD) of
		{ok,NewLD} ->
		    {reply,ok,NewLD};
		{error,Reason} ->
		    {reply,{error,Reason},LD}
	    end;
	false ->                            % Can't activate if not tracing.
	    {reply,{error,not_tracing},LD}
    end;

handle_call({sync_tc_off,{TC,Id,TimeOut}},_From,LD=#ld{trace_state=TState}) ->
    case is_tracing(TState) of              % Check that we are tracing now.
	true ->
	    case h_sync_tc_off(TC,Id,TimeOut,LD) of
		{ok,NewLD,Result} ->
		    {reply,Result,NewLD};
		{error,Reason} ->
		    {reply,{error,Reason},LD}
	    end;
	false ->                            % Can't activate if not tracing.
	    {reply,{error,not_tracing},LD}
    end;

handle_call({inviso,{Cmd,Args}},_From,LD=#ld{trace_state=TRstate}) ->
    case is_tracing(TRstate) of
	true ->
	    case h_inviso(Cmd,Args,LD) of
		{ok,{Reply,NewLD}} ->
		    {reply,Reply,NewLD};
		{error,Reason} ->
		    {reply,{error,Reason},LD}
	    end;
	false ->                            % Can't do if not tracing.
	    {reply,{error,not_tracing},LD}
    end;

handle_call({reactivate,Node},_From,LD=#ld{nodes=NodesD,c_node=CNode}) ->
    case get_state_nodes(Node,NodesD) of
	{State,suspended} ->                % The node is infact suspended.
	    case h_reactivate(Node,CNode) of
		ok ->
		    case {State,is_tracing(LD#ld.trace_state)} of
			{tracing,true} ->   % Only then shall we redo cmds.
			    TCdict=LD#ld.tc_dict,
			    CHL=LD#ld.chl,
			    P=start_reactivator(Node,CNode,TCdict,CHL),
			    {reply,
			     ok,
			     LD#ld{nodes=set_reactivating_nodes(Node,NodesD),
				   reactivators=
				     add_reactivators(Node,P,LD#ld.reactivators)}};

			_ ->                % All other just no longer suspended.
			    {reply,ok,LD#ld{nodes=set_running_nodes(Node,NodesD)}}
		    end;
		{error,Reason} ->
		    {reply,{error,Reason},LD}
	    end;
	reactivating ->
	    {reply,{error,reactivating},LD};
	running ->
	    {reply,{error,already_running},LD};
	down ->
	    {reply,{error,not_available},LD};
	false ->
	    {reply,{error,unknown_node},LD}
    end;

handle_call({get_autostart_data,{Nodes,Dependency}},_From,LD=#ld{chl=CHL}) ->
    case build_autostart_data(lists:keysort(2,get_loglist_chl(CHL)),LD#ld.tc_dict) of
	{ok,ASD} ->
	    TDGargs=get_latest_tdgargs_tracer_data(LD#ld.tracer_data),
	    {M,F,_}=LD#ld.tdg,
	    OptsG=LD#ld.optg,               % Addnodes options generator.
	    {reply,
	     h_get_autostart_data(Nodes,LD#ld.c_node,Dependency,ASD,M,F,TDGargs,OptsG),
	     LD};
	{error,Reason} ->                   % Bad datatypes in command args.
	    {reply,{error,Reason},LD}
    end;
handle_call({get_autostart_data,Dependency},From,LD=#ld{c_node=undefined}) ->
    handle_call({get_autostart_data,{local_runtime,Dependency}},From,LD);
handle_call({get_autostart_data,Dependency},From,LD=#ld{nodes=NodesD}) ->
    Nodes=get_all_nodenames_nodes(NodesD),
    handle_call({get_autostart_data,{local_runtime,{Nodes,Dependency}}},From,LD);

handle_call({reactivator_reply,{Counter,RPid}},_From,LD=#ld{chl=CHL}) ->
    HighestUsedCounter=get_highest_used_counter_chl(CHL),
    if
	HighestUsedCounter>Counter ->       % There are now more log entries.
	    NewUnsortedLog=get_loglist_chl(CHL),
	    {reply,{more,NewUnsortedLog},LD};
	true ->                             % No Counter is youngest log entry.
	    NodesD=LD#ld.nodes,
	    Node=get_node_reactivators(RPid,LD#ld.reactivators),
	    {reply,
	     done,
	     LD#ld{nodes=set_running_nodes(Node,NodesD),
		   reactivators=del_reactivators(RPid,LD#ld.reactivators)}}
    end.
%% -----------------------------------------------------------------------------

%% Handling a notification from a trace case execution process. Receiving this
%% indicated that this phase of the trace case is finnished.
handle_cast({tc_executer_reply,{Phase,ProcH,Result}},LD) ->
    case Phase of
	activating ->                       % The trace case is running now.
	    {ok,NewLD}=h_tc_activation_done(ProcH,Result,LD),
	    {noreply,NewLD};
	stopping ->
	    {ok,NewLD}=h_tc_stopping_done(ProcH,Result,LD),
	    {noreply,NewLD};
	_ ->
	    {noreply,LD}
    end;
handle_cast(_,LD) ->
    {noreply,LD}.
%% -----------------------------------------------------------------------------

%% This is the case when a runtime component goes down.
handle_info({inviso_event,_CNode,_Time,{disconnected,Node,_}},LD) ->
    {noreply,LD#ld{nodes=set_down_nodes(Node,LD#ld.nodes)}};

%% This is the case when a runtime component gets suspended.
handle_info({inviso_event,_CNode,_Time,{state_change,Node,{_,{suspended,_}}}},LD) ->
    case set_suspended_nodes(Node,LD#ld.nodes) of
	{ok,{reactivating,NewNodesD}} ->    % Must kill reactivator process.
	    {noreply,LD#ld{nodes=NewNodesD,
			   reactivators=stop_node_reactivators(Node,LD#ld.reactivators)}};
	{ok,{_,NewNodesD}} ->
	    {noreply,LD#ld{nodes=NewNodesD}};
	{error,_Reason} ->
	    {noreply,LD}
    end;
handle_info(_,LD) ->
    {noreply,LD}.
%% -----------------------------------------------------------------------------

%% Called when the tool server stops. First clause, termination is initiated by
%% out self and therefore controlled another way. In the second case we are
%% stopping for some external reason, and we must then do more here in terminate/2.
terminate(stop,#ld{c_node=CNode}) ->        % This is when we are stopping our self.
    stop_inviso_at_c_node(CNode);
terminate(_,#ld{c_node=CNode,nodes=Nodes,keep_nodes=KeepNodes,keep_pattern=KP}) ->
    remove_all_trace_patterns(KP,CNode,KeepNodes,Nodes),
    stop_inviso_at_c_node(CNode).
%% -----------------------------------------------------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% =============================================================================
%% Handler first level help functions.
%% =============================================================================

%% -----------------------------------------------------------------------------
%% init_tracing
%% -----------------------------------------------------------------------------

%% Help function starting the tracing at all nodes. Note that the tracer data
%% is calculated using a user defined function. This is how for instance the
%% file names (of the log files) are determined.
h_init_tracing(M,F,TDGargs,LD=#ld{c_node=CNode,nodes=NodesD,tracer_data=TDs}) ->
    case get_inactive_running_nodes(NodesD) of
	[] ->                               % There are no nodes to initiate!
	    {SessionNr,NewTDs}=insert_td_tracer_data(TDGargs,TDs),
	    {ok,{SessionNr,NewTDs},LD#ld{tracer_data=NewTDs,chl=mk_chl(LD#ld.chl)}};
	Nodes ->                            % List of nodes or 'local_runtime'.
	    case call_tracer_data_generator(CNode,M,F,TDGargs,Nodes) of
		{ok,TracerList} ->          % Generated our tracerdata.
		    case h_init_tracing_2(CNode,TracerList) of
			{ok,ReturnValue} -> % Some nodes are initialized now.
			    NewNodesD=set_tracing_running_nodes(ReturnValue,NodesD),
			    {SessionNr,NewTDs}=insert_td_tracer_data(TDGargs,TDs),
			    {ok,{SessionNr,
				 ReturnValue,
				 LD#ld{nodes=NewNodesD,
				       tracer_data=NewTDs,
				       chl=mk_chl(LD#ld.chl)}}};
			{error,Reason} ->
			    {error,Reason}
		    end;
		{error,Reason} ->           % Faulty tracer data generator func.
		    {error,{bad_tdg,Reason}}
	    end
    end.

h_init_tracing_2(undefined,TracerData) ->   % Non distributed case.
    case inviso:init_tracing(TracerData) of
	{ok,LogResult} when list(LogResult) ->
	    {ok,{ok,LogResult}};
	{error,already_initated} ->         % Perhaps adopted!?
	    {ok,{error,already_initiated}}; % Not necessarily wrong.
	{error,Reason} ->
	    {error,Reason}
    end;
h_init_tracing_2(CNode,TracerList) ->
    case rpc:call(CNode,inviso,init_tracing,[TracerList]) of
	{ok,NodeResults} ->
	    {ok,{ok,NodeResults}};
	{error,Reason} ->
	    {error,Reason};
	{badrpc,Reason} ->
	    {error,{inviso_control_node_error,Reason}}
    end.
%% -----------------------------------------------------------------------------

%% Help function starting all initial trace cases. They are actually handled
%% the same way as user started trace cases.
do_initial_tcs([{TC,Vars}|Rest],LD) ->
    Id=make_ref(),                          % Trace case ID.
    case h_tc_on(TC,Id,Vars,LD) of          % Start using regular start methods.
	{ok,NewLD} ->                       % Trace case was successfully started.
	    NewInitialTcs=add_initial_tcs(TC,Id,NewLD#ld.started_initial_tcs),
	    do_initial_tcs(Rest,NewLD#ld{started_initial_tcs=NewInitialTcs});
	{error,_Reason} ->
	    do_initial_tcs(Rest,LD)
    end;
do_initial_tcs([_|Rest],LD) ->
    do_initial_tcs(Rest,LD);
do_initial_tcs([],LD) ->
    LD.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% stop_tracing
%% -----------------------------------------------------------------------------

%% Help function stopping tracing at tracing nodes.
h_stop_tracing(#ld{c_node=CNode,nodes=NodesD,tracer_data=TDs}) ->
    case h_stop_tracing_2(CNode,NodesD) of
	{ok,InactiveNodes} ->
	    {ok,{get_latest_session_nr_tracer_data(TDs),InactiveNodes}};
	{error,Reason} ->
	    {error,Reason}
    end.

h_stop_tracing_2(undefined,NodesD) ->  % The non distributed case.
    case get_tracing_nodes(NodesD) of
	{up,{inactive,_}} ->           % Already not tracing!
	    {ok,[]};
	{up,_} ->
	    case inviso:stop_tracing() of
		{ok,_State} ->
		    {ok,local_runtime};
		{error,Reason} ->
		    {error,Reason}
	    end;
	down ->
	    {ok,[]}
    end;
h_stop_tracing_2(CNode,NodesD) ->
    Nodes=get_tracing_nodes(NodesD),
    case rpc:call(CNode,inviso,stop_tracing,[Nodes]) of
	{ok,_NodeResults} ->
	    {ok,Nodes};
	{error,Reason} ->
	    {error,Reason};
	{badrpc,Reason} ->
	    {error,{inviso_control_node_error,Reason}}
    end.
%% -----------------------------------------------------------------------------

%% Help function building the structures used when exporting autostart information
%% from the tool.
h_get_autostart_data(local_runtime,_,Dependency,ASD,M,F,TDGargs,OptsG) ->
    case call_tracer_data_generator(undefined,M,F,TDGargs,local_runtime) of
	{ok,TracerData} ->
	    Opts=[Dependency|start_runtime_components_mk_opts(local_runtime,OptsG)],
	    {ok,{ASD,{ok,{Opts,TracerData}}}};
	{error,Reason} ->
	    {error,{bad_tdg,Reason}}
    end;
h_get_autostart_data(Nodes,CNode,Dependency,ASD,M,F,TDGargs,OptsG) ->
    {ok,{ASD,h_get_autostart_data_2(Nodes,CNode,Dependency,M,F,TDGargs,OptsG)}}.

h_get_autostart_data_2([Node|Rest],CNode,Dependency,M,F,TDGargs,OptsG) ->
    case call_tracer_data_generator(CNode,M,F,TDGargs,[Node]) of
	{ok,[{_,TracerData}]} ->
	    Opts=[Dependency|start_runtime_components_mk_opts(Node,OptsG)],
	    [{Node,{Opts,TracerData}}|
	     h_get_autostart_data_2(Rest,CNode,Dependency,M,F,TDGargs,OptsG)];
	{error,Reason} ->
	    [{Node,{error,{bad_tdg,Reason}}}|
	     h_get_autostart_data_2(Rest,CNode,Dependency,M,F,TDGargs,OptsG)]
    end;
h_get_autostart_data_2([],_CNode,_Dependency,_M,_F,_TDGargs,_OptsG) ->
    [].
%% -----------------------------------------------------------------------------

%% Function handling ativating a trace case. Trace cases that do not have a
%% particular on/off handling (but just on in some scense) are handled here too.
%% The trace case is entered into the Command History Log.
%% Note that the trace case can not be executed at this node but must be
%% executed where the inviso control component is.
%%  TC=tracecase_name(),
%%  Id=term(), identifiying this usage so we can turn it off later.
%%  Vars=list(), list of variable-value bindnings.
h_tc_on(TC,Id,Vars,LD=#ld{c_node=CNode,tc_dict=TCdict,chl=CHL}) ->
    case find_id_chl(TC,Id,CHL) of
	activating ->                       % Already started.
	    {error,activating};
	stopping ->                         % Not yet stopped.
	    {error,not_yet_stopped};
	false ->
	    case get_tracecase_tc_dict(TC,TCdict) of
		{ok,TraceCase} ->           % Such a trace case exists.
		    case check_bindings(Vars,TraceCase) of
			{ok,Bindings} ->    % Necessary vars exists in Vars.
			    Nodes=get_nodenames_running_nodes(LD#ld.nodes),
			    case exec_trace_case_on(CNode,TraceCase,Bindings,Nodes) of
				{ok,ProcH} -> % Trace cases have no return values.
				    NewCHL=set_activating_chl(TC,Id,CHL,Bindings,ProcH),
				    {ok,LD#ld{chl=NewCHL}};
				{error,Reason} ->
				    {error,Reason}
			    end;
			{error,Reason} ->   % Variable def missing.
			    {error,Reason}
		    end;
		false ->
		    {error,unknown_tracecase}
	    end;
	{ok,_Bindings} ->                   % Already activated and running.
	    {error,already_started}
    end.
%% -----------------------------------------------------------------------------

h_sync_tc_on(TC,Id,Vars,TimeOut,LD=#ld{c_node=CNode,tc_dict=TCdict,chl=CHL}) ->
    case find_id_chl(TC,Id,CHL) of
	activating ->                       % Already started.
	    {error,activating};
	stopping ->                         % Not yet stopped.
	    {error,not_yet_stopped};
	false ->
	    case get_tracecase_tc_dict(TC,TCdict) of
		{ok,TraceCase} ->           % Such a trace case exists.
		    case check_bindings(Vars,TraceCase) of
			{ok,Bindings} ->    % Necessary vars exists in Vars.
			    {ok,TcFName}=get_tc_activate_fname(TraceCase),
			    Nodes=get_nodenames_running_nodes(LD#ld.nodes),
			    Bindings2=erl_eval:add_binding('Nodes',Nodes,Bindings),
			    RpcNode=get_rpc_nodename(CNode),
			    case rpc:call(RpcNode,file,script,[TcFName,Bindings2],TimeOut) of
				{ok,Value} ->
				    FakeProcH=make_ref(),
				    NewCHL1=set_activating_chl(TC,Id,CHL,Bindings2,FakeProcH),
				    NewCHL2=set_running_chl(FakeProcH,TC,Id,Value,NewCHL1),
				    {ok,LD#ld{chl=NewCHL2},Value};
				{error,Reason} ->
				    {error,{faulty_tracecase,{TcFName,Reason}}};
				{badrpc,Reason} ->
				    {error,{badrpc,Reason}}
			    end;
			{error,Reason} ->   % Variable def missing.
			    {error,Reason}
		    end;
		false ->
		    {error,unknown_tracecase}
	    end;
	{ok,_Bindings} ->                   % Already activated and running.
	    {error,already_started}
    end.
%% -----------------------------------------------------------------------------

%% Function handling turning a trace case off. The trace case must be registered
%% as having an off mechanism. If it has an off mechanism and was previously entered
%% into the Command History Log and is done with its activation phase, it will be
%% executed and removed from the CHL.
h_tc_off(TC,Id,LD=#ld{c_node=CNode,tc_dict=TCdict,chl=CHL}) ->
    case find_id_chl(TC,Id,CHL) of
	{ok,Bindings} ->                    % Yes, we have turned it on before.
	    case get_tracecase_tc_dict(TC,TCdict) of
		{ok,TraceCase} ->
		    Nodes=get_nodenames_running_nodes(LD#ld.nodes),
		    case exec_trace_case_off(CNode,TraceCase,Bindings,Nodes) of
			{ok,ProcH} ->
			    NewCHL=set_stopping_chl(TC,Id,CHL,ProcH),
			    {ok,LD#ld{chl=NewCHL}};
			{error,Reason} ->
			    {error,Reason}
		    end;
		false ->                    % Strange, Id ok but no such trace case.
		    {error,unknown_tracecase}
	    end;
	false ->                            % Not previously turned on.
	    {error,unknown_id};
	activating ->
	    {error,activating};
	stopping ->
	    {error,already_stopping}
    end.
%% -----------------------------------------------------------------------------


h_sync_tc_off(TC,Id,TimeOut,LD=#ld{c_node=CNode,tc_dict=TCdict,chl=CHL}) ->
    case find_id_chl(TC,Id,CHL) of
	{ok,Bindings} ->                    % Yes, we have turned it on before.
	    case get_tracecase_tc_dict(TC,TCdict) of
		{ok,TraceCase} ->
		    case get_tc_deactivate_fname(TraceCase) of
			{ok,TcFName} ->
			    Nodes=get_nodenames_running_nodes(LD#ld.nodes),
			    Bindings2=erl_eval:add_binding('Nodes',Nodes,Bindings),
			    RpcNode=get_rpc_nodename(CNode),
			    case rpc:call(RpcNode,file,script,[TcFName,Bindings2],TimeOut) of
				{ok,Value} ->
				    FakeProcH=make_ref(),
				    NewCHL1=set_stopping_chl(TC,Id,CHL,FakeProcH),
				    NewCHL2=nullify_chl(FakeProcH,TC,Id,NewCHL1),
				    {ok,LD#ld{chl=NewCHL2},Value};
				{error,Reason} -> % Script fault.
				    {error,{faulty_tracecase,{TcFName,Reason}}};
				{badrpc,Reason} ->
				    {error,{badrpc,Reason}}
			    end;
			false ->
			    {error,no_deactivation}
		    end;
		false ->                    % Strange, Id ok but no such trace case.
		    {error,unknown_tracecase}
	    end;
	false ->                            % Not previously turned on.
	    {error,unknown_id};
	activating ->
	    {error,activating};
	stopping ->
	    {error,already_stopping}
    end.
%% -----------------------------------------------------------------------------

%% Function handling that a trace case has completed its activation phase and
%% shall now be marked in the Command History Log as running.
h_tc_activation_done(ProcH,Result,LD=#ld{chl=CHL}) ->
    case find_tc_executer_chl(ProcH,CHL) of
	{activating,{TC,Id}} ->
	    case Result of
		{ok,Value} ->               % The trace case is successful activated.
		    {ok,LD#ld{chl=set_running_chl(ProcH,TC,Id,Value,CHL)}};
		{error,_} ->                % Then pretend it never happend :-)
		    {ok,LD#ld{chl=del_tc_chl(ProcH,TC,Id,CHL)}} % Remove it.
	    end;
	_ ->                                % Where did this come from?
	    {ok,LD}                         % Well just ignore it then.
    end.
%% -----------------------------------------------------------------------------

%% Function handling that a trace case has completed its stopping phase and
%% shall now be nulled in the Command History Log (meaning that it will not
%% be repeated in the event of a reactivation).
h_tc_stopping_done(ProcH,Result,LD=#ld{chl=CHL}) ->
    case find_tc_executer_chl(ProcH,CHL) of
	{stopping,{TC,Id}} ->
	    case Result of
		ok ->
		    {ok,LD#ld{chl=nullify_chl(ProcH,TC,Id,CHL)}};
		{error,_} ->                % This is difficult, is it still active?
		    {ok,LD#ld{chl=nullify_chl(ProcH,TC,Id,CHL)}}
	    end;
	_ ->                                % Strange.
	    {ok,LD}
    end.
%% -----------------------------------------------------------------------------

%% Function executing one inviso command. The returnvalue from the inviso
%% function call will be the return value to the client. The command is
%% entered into the history command log.
%% Note that the inviso call may have to be done at another node, dictated
%% by the c_node field. Further, if the module name is not an atom it is
%% most likely a regexp, which must be expanded at the regexp_node. Note
%% this is only relevant for tp and tpl.
h_inviso(Cmd,Args,LD=#ld{c_node=CNode,regexp_node=RegExpNode,chl=CHL}) ->
    Arity=length(Args),
    case check_proper_inviso_call(Cmd,Arity) of
	{true,RegExpFlag} ->                % Yes it is an inviso call.
	    Nodes=get_nodenames_running_nodes(LD#ld.nodes),
	    case h_inviso_2(Cmd,Args,CNode,RegExpNode,RegExpFlag,Nodes) of
		{ok,Result} ->
		    case check_inviso_call_to_history(Cmd,Arity) of
			true ->             % This function shall be added to chl.
			    {ok,{Result,LD#ld{chl=add_inviso_call_chl(Cmd,Args,CHL)}}};
			false ->            % Do not add it.
			    {ok,{Result,LD}}
		    end;
		{error,Reason} ->
		    {error,Reason}
	    end;
	false ->                            % Not an inviso function.
	    {error,invalid_function_name}
    end.

h_inviso_2(Cmd,Args,undefined,_,_,_) ->     % A non distributed system.
    case catch apply(inviso,Cmd,Args) of
	{'EXIT',Reason} ->
	    {error,{'EXIT',Reason}};
	Result ->
	    {ok,Result}
    end;
h_inviso_2(Cmd,Args,CNode,RegExpNode,RegExpFlag,Nodes) ->
    case expand_module_regexps(Args,RegExpNode,Nodes,RegExpFlag) of
	{ok,NewArgs} ->
	    case catch inviso_tool_lib:inviso_cmd(CNode,Cmd,[Nodes|NewArgs]) of
		{'EXIT',Reason} ->
		    {error,{'EXIT',Reason}};
		{error,{badrpc,Reason}} ->  % Includes runtime failure.
		    {error,{badrpc,Reason}};
		Result ->
		    {ok,Result}
	    end;
	{error,Reason} ->
	    {error,Reason}
    end.
%% -----------------------------------------------------------------------------


h_reactivate(_Node,undefined) ->            % The non-distributed case.
    case inviso:cancel_suspension() of
	ok ->
	    ok;
	{error,Reason} ->
	    {error,Reason}
    end;
h_reactivate(Node,CNode) ->
    case inviso_tool_lib:inviso_cmd(CNode,cancel_suspension,[[Node]]) of
	{ok,[{Node,ok}]} ->
	    ok;
	{ok,[{Node,{error,Reason}}]} ->
	    {error,Reason};
	{error,Reason} ->
	    {error,Reason}
    end.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% Terminate.
%% -----------------------------------------------------------------------------

%% Help function stopping the inviso control component. Does not return
%% anything significant.
stop_inviso_at_c_node(undefined) ->         % Non distributed case.
    inviso:stop();
stop_inviso_at_c_node(CNode) ->
    rpc:call(CNode,inviso,stop,[]).
%% -----------------------------------------------------------------------------

%% Help function that removes all trace patterns from the nodes that are not
%% marked as such were patterns shall be left after stopping of inviso.
%% Returns {ok,NodeResult} or {error,Reason}. In the non-distributed case
%% 'ok' is returned incase of success, ot 'patterns_untouched'.
remove_all_trace_patterns(true,_CNode,_KeepNodes,_Nodes) ->
    patterns_untouched;
remove_all_trace_patterns(_KP,undefined,KeepNodes,_Nodes) ->
    case KeepNodes of
	undefined ->                        % No, remove patterns from localruntime.
	    inviso:ctp_all();
	_ ->
	    patterns_untouched
    end;
remove_all_trace_patterns(_KP,CNode,KeepNodes,Nodes) ->
    Nodes2=lists:filter(fun(N)->not(lists:member(N,KeepNodes)) end,Nodes),
    case rpc:call(CNode,inviso,ctp_all,[Nodes2]) of
	{ok,NodeResults} ->
	    F=fun(N) ->
		      case lists:member(N,KeepNodes) of
			  true ->
			      {N,pattern_untouched};
			  false ->
			      case lists:keysearch(N,1,NodeResults) of
				  {value,Result} ->
				      Result; % {Node,ok}
				  false ->  % Extremely strange.
				      {N,{error,general_error}}
			      end
		      end
	      end,
	    {ok,lists:map(F,Nodes)};
	{error,Reason} ->
	    {error,Reason};
	{badrpc,Reason} ->
	    {error,{inviso_control_node_error,Reason}}
    end.
%% -----------------------------------------------------------------------------

%% =============================================================================
%% Second level help functions.
%% =============================================================================

%% This help function builds the AutoStartData structure which is returned from
%% get_austostart_data. An AutoStartData structure is a list of trace-files and
%% inviso commands. The order is significant since it is the idea that doing
%% the trace case files and inviso commands in that order will bring a node to
%% a certain state in a trace perspective.
%% Returns {ok,AutoStartData} or {error,Reason}
build_autostart_data(SortedLog,TCdict) ->
    build_autostart_data_2(SortedLog,TCdict,[]).

build_autostart_data_2([{_,_C,Stop,_B}|Rest],TCdict,Accum) when Stop==stopped;Stop==stopping->
    build_autostart_data_2(Rest,TCdict,Accum); % Simply skip deactivated/deativating.
build_autostart_data_2([{{TCname,_},_C,activating,Bindings}|Rest],TCdict,Accum) ->
    build_autostart_data_tc(TCname,Bindings,TCdict,Rest,Accum);
build_autostart_data_2([{{TCname,_},_C,running,Bindings,_R}|Rest],TCdict,Accum) ->
    build_autostart_data_tc(TCname,Bindings,TCdict,Rest,Accum);
build_autostart_data_2(SortedLog,TCdict,Accum) when SortedLog/=[] ->
    case build_autostart_data_cmd(SortedLog,[]) of
	{ok,{Cmds,Rest}} ->
	    build_autostart_data_2(Rest,TCdict,[{binary,list_to_binary(Cmds)}|Accum]);
	{error,Reason} ->                    % Non printable datatypes.
	    {error,Reason}
    end;
build_autostart_data_2([],_TCdict,Accum) ->
    {ok,lists:reverse(Accum)}.

build_autostart_data_cmd([{{M,F,Args,_Ref},_C,_R}|Rest],Accum) ->
    build_autostart_data_cmd(Rest,[{M,F,Args}|Accum]);
build_autostart_data_cmd(Rest,Accum) ->      % Either end of list of end of commands.
    case build_autostart_data_cmd_2(Accum,"") of
	{ok,String} ->
	    {ok,{String,Rest}};
	{error,Reason} ->
	    {error,Reason}
    end.

build_autostart_data_cmd_2([{M,F,Args}|Rest],String) ->
    case build_autostart_data_cmd_2_mk_args(Args,"") of
	{ok,ArgString} ->
	    build_autostart_data_cmd_2(Rest,
				       atom_to_list(M)++":"++
				       atom_to_list(F)++"("++
				       ArgString++").\n"++String);
	{error,Reason} ->                    % An incorrect argument found.
	    {error,Reason}
    end;
build_autostart_data_cmd_2([],String) ->
    {ok,String}.

%% Help function building a string of all argument or returns an error if
%% the arguments contains a non-printable datatype (pids are of no use in a
%% script-file).
build_autostart_data_cmd_2_mk_args([Arg|Rest],ArgString) ->
    case build_autostart_data_cmd_2_mk_args_check_type(Arg) of
	ok ->
	    NewArgString=
		if
		    ArgString=="" ->
			lists:flatten(io_lib:write(Arg));
		    true ->
			lists:flatten(io_lib:write(Arg))++","++ArgString
		end,
	    build_autostart_data_cmd_2_mk_args(Rest,NewArgString);
	error ->
	    {error,{datatype,Arg}}
    end;
build_autostart_data_cmd_2_mk_args([],ArgString) ->
    {ok,ArgString}.

build_autostart_data_cmd_2_mk_args_check_type(A)
  when pid(A);port(A);reference(A);function(A) ->
    error;                                   % Can not be written to file.
build_autostart_data_cmd_2_mk_args_check_type([E|Rest]) ->
    case build_autostart_data_cmd_2_mk_args_check_type(E) of
	ok ->
	    build_autostart_data_cmd_2_mk_args_check_type(Rest);
	error ->
	    error
    end;
build_autostart_data_cmd_2_mk_args_check_type(T) when tuple(T),size(T)>0 ->
    build_autostart_data_cmd_2_mk_args_check_type_tuple(T,1);
build_autostart_data_cmd_2_mk_args_check_type(_) ->
    ok.

build_autostart_data_cmd_2_mk_args_check_type_tuple(T,I) when size(T)>=I ->
    case build_autostart_data_cmd_2_mk_args_check_type(element(I,T)) of
	ok ->
	    build_autostart_data_cmd_2_mk_args_check_type_tuple(T,I+1);
	error ->
	    error
    end;
build_autostart_data_cmd_2_mk_args_check_type_tuple(_T,_) ->
    ok.

%% Help function placing the filename in the AutoStartData structure.    
build_autostart_data_tc(TCname,Bindings,TCdict,Rest,Accum) ->
    {ok,TC}=get_tracecase_tc_dict(TCname,TCdict),
    {ok,FName}=get_tc_activate_fname(TC),
    build_autostart_data_2(Rest,TCdict,[{file,{FName,Bindings}}|Accum]).
%% -----------------------------------------------------------------------------

%% Help function generating tracerdata to init inviso tracing. The generation
%% is done by the TracerDataGenerator, TDG, function.
%% Individual tracerdata is generated for each node in Nodes.
%% Returns {ok,TracerData} or {error,Reason}.
call_tracer_data_generator(undefined,M,F,TDGargs,_Nodes) -> % Non distributed.
    case catch call_tracer_data_generator_3(M,F,TDGargs,local_runtime) of
	{'EXIT',Reason} ->
	    {error,{'EXIT',Reason}};
	TracerData ->
	    {ok,TracerData}
    end;
call_tracer_data_generator(_CNode,M,F,TDGargs,Nodes) ->
    case catch call_tracer_data_generator_2(M,F,TDGargs,Nodes) of
	{'EXIT',Reason} ->
	    {error,{'EXIT',Reason}};
	TracerList ->
	    {ok,TracerList}
    end.

call_tracer_data_generator_2(M,F,TDGargs,[Node|Rest]) ->
    [{Node,call_tracer_data_generator_3(M,F,TDGargs,Node)}|
     call_tracer_data_generator_2(M,F,TDGargs,Rest)];
call_tracer_data_generator_2(_,_,_,[]) ->
    [].

call_tracer_data_generator_3(M,F,TDGargs,Node) ->
    apply(M,F,[Node|TDGargs]).
%% -----------------------------------------------------------------------------

%% This function acts as standard options generator function. That is returning
%% the options argument to inviso:add_node/3. Note that this function must not
%% return the dependency part of that option.
std_options_generator(_Node) ->
    [].                                     % No particular options(!)
%% -----------------------------------------------------------------------------


%% Help function checking that Vars contains a binding for every variable
%% listed in the VarNames field in TraceCase. Note that the special variable 'Nodes'
%% is disregarded, since it is always added by the inviso_tool.
%% Returns {ok,Bindings} or 'false'. Where Bindings is a bindngs structure
%% according to file:eval functionality.
check_bindings(Vars,TraceCase) ->
    case catch check_bindings_2(Vars,
				get_tc_varnames(TraceCase),
				erl_eval:new_bindings()) of
	{'EXIT',_Reason} ->
	    false;
	false ->                            % Missing a bindning.
	    false;
	Bindings ->
	    {ok,Bindings}
    end.

check_bindings_2(Vars,['Nodes'|Rest],Bindings) ->
    check_bindings_2(Vars,Rest,Bindings);   % Disregard Nodes since it is automatic.
check_bindings_2(Vars,[VarName|Rest],Bindings) ->
    case lists:keysearch(VarName,1,Vars) of
	{value,{_,Val}} ->
	    check_bindings_2(Vars,Rest,erl_eval:add_binding(VarName,Val,Bindings));
	false ->                            % Mandatory variable missing.
	    false                           % Quite here then.
    end;
check_bindings_2(_,[],Bindings) ->
    Bindings.
%% -----------------------------------------------------------------------------

%% This help function checks that the command the user tries to do is amongst
%% the inviso API. It at the same time returns what kind of command it is.
%% {true,RegExpFlag} or 'false' where RegExpFlag indicates if this command
%% needs to have its argument modified by module regexp expansion or not.
check_proper_inviso_call(Cmd,Arity) ->
    case lists:member({Cmd,Arity},?INVISO_CMDS) of
	true ->                             % It is part of inviso API.
	    {true,check_proper_inviso_call_regexp(Cmd,Arity)};
	false ->
	    false
    end.

%% Returns {Type,Arity,PlaceOfModuleSpec} or 'false'.
check_proper_inviso_call_regexp(tp,5) -> {tp,5,1};
check_proper_inviso_call_regexp(tp,4) -> {tp,4,1};
check_proper_inviso_call_regexp(tp,1) -> {tp,1,1};
check_proper_inviso_call_regexp(tpl,5) -> {tp,5,1};
check_proper_inviso_call_regexp(tpl,4) -> {tp,4,1};
check_proper_inviso_call_regexp(tpl,1) -> {tp,1,1};
check_proper_inviso_call_regexp(ctp,3) -> {ctp,3,1};
check_proper_inviso_call_regexp(ctp,1) -> {ctp,1,1};
check_proper_inviso_call_regexp(ctpl,3) -> {ctp,3,1};
check_proper_inviso_call_regexp(ctpl,1) -> {ctp,1,1};
check_proper_inviso_call_regexp(_,_) ->      % No regexp expansion.
    false.
%% -----------------------------------------------------------------------------

%% Help function checking if this inviso command shall be added to the command
%% history log. Returns true or false.
check_inviso_call_to_history(Cmd,Arity) ->
    case lists:member({Cmd,Arity},?INVISO_CMD_HISTORY) of
	true ->
	    true;
	false ->
	    false
    end.
%% -----------------------------------------------------------------------------

%% Help function traversing the arguments and expanding module names stated
%% as regular expressions. This means that the resulting arguments may be longer
%% than the orginal ones.
%% When we run this function it has been determined that we are a distributed
%% system.
%% Also note that if there are no regexps in Args, no regexpansion will be
%% made and RegExpNode may be 'undefined' (as it is if not set at start-up).
%% If RegExpNode is unavailable the nodes found in Nodes will be used until
%% one that works is found.
expand_module_regexps(Args,_RegExpNode,_Nodes,false) ->
    {ok,Args};
expand_module_regexps([PatternList],RegExpNode,Nodes,{tp,1,1}) ->
    case catch expand_module_regexps_tp(PatternList,RegExpNode,Nodes) of
	NewPatternList when list(NewPatternList) ->
	    {ok,[NewPatternList]};
	{error,Reason} ->
	    {error,Reason}
    end;
expand_module_regexps([PatternList],RegExpNode,Nodes,{ctp,1,1}) ->
    case catch expand_module_regexps_ctp(PatternList,RegExpNode,Nodes) of
	NewPatternList when list(NewPatternList) ->
	    {ok,[NewPatternList]};
	{error,Reason} ->
	    {error,Reason}
    end;
expand_module_regexps([M,F,Arity,MS,Opts],RegExpNode,Nodes,{tp,5,1}) ->
    expand_module_regexps([[{M,F,Arity,MS,Opts}]],RegExpNode,Nodes,{tp,1,1});
expand_module_regexps([M,F,Arity,MS],RegExpNode,Nodes,{tp,4,1}) ->
    expand_module_regexps([[{M,F,Arity,MS,[]}]],RegExpNode,Nodes,{tp,1,1});
expand_module_regexps([M,F,Arity],RegExpNode,Nodes,{ctp,3,1}) ->
    expand_module_regexps([[{M,F,Arity}]],RegExpNode,Nodes,{ctp,1,1}).


expand_module_regexps_tp([E={M,_,_,_,_}|Rest],RegExpNode,Nodes) when atom(M) ->
    [E|expand_module_regexps_tp(Rest,RegExpNode,Nodes)];
expand_module_regexps_tp([{M,F,Arity,MS,Opts}|Rest],RegExpNode,Nodes) when list(M);tuple(M) ->
    case inviso_tool_lib:expand_module_names([RegExpNode],
					     M,
					     [{expand_only_at,RegExpNode}]) of
	{singlenode_expansion,badrpc} ->       % RegExpNode probably down.
	    case Nodes of
		[NewRegExpNode|RestNodes] ->   % Ok, just choose a node.
		    expand_module_regexps_tp([{M,F,Arity,MS,Opts}|Rest],NewRegExpNode,RestNodes);
		[] ->                          % No more nodes to choose from.
		    throw({error,no_available_regexpnode})
	    end;
	{singlenode_expansion,Modules} ->
	    expand_module_regexps_tp_2(Modules,F,Arity,MS,Opts,Rest,RegExpNode,Nodes);
	{error,_Reason} ->
	    expand_module_regexps_tp(Rest,RegExpNode,Nodes)
    end;
expand_module_regexps_tp([_|Rest],RegExpNode,Nodes) ->
    expand_module_regexps_tp(Rest,RegExpNode,Nodes); % Skip faulty module specification.
expand_module_regexps_tp([],_RegExpNodes,_Nodes) ->
    [].

expand_module_regexps_tp_2([M|MRest],F,Arity,MS,Opts,Rest,RegExpNode,Nodes) ->
    [{M,F,Arity,MS,Opts}|
     expand_module_regexps_tp_2(MRest,F,Arity,MS,Opts,Rest,RegExpNode,Nodes)];
expand_module_regexps_tp_2([],_,_,_,_,Rest,RegExpNode,Nodes) ->
    expand_module_regexps_tp(Rest,RegExpNode,Nodes).

expand_module_regexps_ctp([E={M,_,_}|Rest],RegExpNode,Nodes) when atom(M) ->
    [E|expand_module_regexps_ctp(Rest,RegExpNode,Nodes)];
expand_module_regexps_ctp([{M,F,Arity}|Rest],RegExpNode,Nodes) when list(M);tuple(M) ->
    case inviso_tool_lib:expand_module_names([RegExpNode],
					     M,
					     [{expand_only_at,RegExpNode}]) of
	{singlenode_expansion,badrpc} ->       % RegExpNode probably down.
	    case Nodes of
		[NewRegExpNode|RestNodes] ->   % Ok, just choose a node.
		    expand_module_regexps_ctp([{M,F,Arity}|Rest],NewRegExpNode,RestNodes);
		[] ->                          % No more nodes to choose from.
		    throw({error,no_available_regexpnode})
	    end;
	{singlenode_expansion,Modules} ->
	    expand_module_regexps_ctp_2(Modules,F,Arity,Rest,RegExpNode,Nodes);
	{error,_Reason} ->
	    expand_module_regexps_ctp(Rest,RegExpNode,Nodes)
    end;
expand_module_regexps_ctp([_|Rest],RegExpNode,Nodes) ->
    expand_module_regexps_tp(Rest,RegExpNode,Nodes); % Skip faulty module specification.
expand_module_regexps_ctp([],_RegExpNodes,_Nodes) ->
    [].

expand_module_regexps_ctp_2([M|MRest],F,Arity,Rest,RegExpNode,Nodes) ->
    [{M,F,Arity}|expand_module_regexps_ctp_2(MRest,F,Arity,Rest,RegExpNode,Nodes)];
expand_module_regexps_ctp_2([],_,_,Rest,RegExpNode,Nodes) ->
    expand_module_regexps_ctp(Rest,RegExpNode,Nodes).
%% -----------------------------------------------------------------------------



%% Help function running the activation of a trace case. Note that this must
%% be done at the inviso control component's Erlang node *and* that it must be
%% done in its own process since there is no telling for how long a trace case
%% may run.
%% Returns {ok,ActivationHandler}.
exec_trace_case_on(CNode,TraceCase,Bindings,Nodes) ->
    {ok,TcFName}=get_tc_activate_fname(TraceCase),
    {ok,exec_trace_case_2(CNode,
			  TcFName,
			  erl_eval:add_binding('Nodes',Nodes,Bindings),
			  activating)}.

%% Help function running the deactivation of a trace case.
exec_trace_case_off(CNode,TraceCase,Bindings,Nodes) ->
    case get_tc_deactivate_fname(TraceCase) of
	{ok,TcFName} ->                     % There is a deactivation.
	    {ok,exec_trace_case_2(CNode,
				  TcFName,
				  erl_eval:add_binding('Nodes',Nodes,Bindings),
				  stopping)};
	false ->
	    {error,no_deactivation}
    end.

exec_trace_case_2(CNode,TcFName,Bindings,Phase) ->
    if
	CNode==undefined ->                 % The non distributed case.
	    spawn_link(?MODULE,tc_executer,[TcFName,Bindings,Phase,self()]);
	true ->
	    spawn_link(CNode,?MODULE,tc_executer,[TcFName,Bindings,Phase,self()])
    end.

%% This function is run in its own process and is responsible for executing
%% the trace case.
tc_executer(TcFName,Bindings,Phase,Parent) ->
    case catch file:script(TcFName,Bindings) of
	{ok,Value} ->
	    tc_executer_reply(Parent,{Phase,self(),{ok,Value}});
	{'EXIT',Reason} ->
	    tc_executer_reply(Parent,{Phase,self(),{error,{'EXIT',Reason}}});
	Error ->
	    tc_executer_reply(Parent,{Phase,self(),Error})
    end.
%% -----------------------------------------------------------------------------

%% Help function starting a reactivator process replaying the command history log.
%% Returns a pid of the reactivator process.
start_reactivator(Node,CNode,TCdict,CHL) ->
    UnsortedLog=get_loglist_chl(CHL),       % Must fetch here, later on wrong node.
    if
	CNode==undefined ->                 % The non-distributed case.
	    spawn_link(?MODULE,
		       reactivator_executer,
		       [Node,TCdict,UnsortedLog,self(),0,false]);
	true ->
	    spawn_link(CNode,
		       ?MODULE,
		       reactivator_executer,
		       [Node,TCdict,UnsortedLog,self(),0,false])
    end.

%% The strategy is to traverse the CHL ETS table in Counter order, redoing the
%% commands one by one. We wait until one command is finished until we do the
%% next. Commands marked as nullified are not performed. In fact when a command
%% is nullified only the stop will be found in the CHL. Its activation will be
%% removed.
reactivator_executer(Node,TCdict,UnsortedLog,TPid,StartCounter,SFlag) ->
    SortedLog=lists:keysort(2,UnsortedLog), % Sort on Counter, oldest first.
    Log=reactivator_skip_log_entries(SortedLog,StartCounter),
    case reactivator_executer_2(Node,TCdict,TPid,SFlag,StartCounter,Log) of
	done ->
	    true;                           % Simply terminate the reactivator then.
	{more,{NewStartCounter,NewUnsortedLog}} ->
	    reactivator_executer(Node,TCdict,NewUnsortedLog,TPid,NewStartCounter,true)
    end.

%% This clause is that we shall activate a trace-case.
reactivator_executer_2(Node,TCdict,TPid,SFlag,_,[{{TCname,_Id},NextC,_,Bindings}|Rest]) ->
    case get_tracecase_tc_dict(TCname,TCdict) of
	{ok,{_,_,_,FNameOn}} ->             % A case with just on functionality.
	    reactivator_executer_tc(Node,Bindings,FNameOn),
	    reactivator_executer_2(Node,TCdict,TPid,SFlag,NextC,Rest);
	{ok,{_,_,_,FNameOn,_}} ->
	    reactivator_executer_tc(Node,Bindings,FNameOn),
	    reactivator_executer_2(Node,TCdict,TPid,SFlag,NextC,Rest);
	false ->                            % Strange, does not exist anylonger!?
	    reactivator_executer_2(Node,TCdict,TPid,SFlag,NextC,Rest)
    end;
%% This clause is stopping a trace-case and shall NOT be done.
reactivator_executer_2(Node,TCdict,TPid,false,_,[{{_TCname,_Id,_Ref},NextC,stopped,_}|Rest]) ->
    reactivator_executer_2(Node,TCdict,TPid,false,NextC,Rest);
%% This clause is stopping a trace-case and it shall be done since the SFlag is true.
reactivator_executer_2(Node,TCdict,TPid,true,_,
		       [{{TCname,_Id,_Ref},NextC,stopped,Bindings}|Rest]) ->
    case get_tracecase_tc_dict(TCname,TCdict) of
	{ok,{_,_,_,_,FNameOff}} ->
	    reactivator_executer_tc(Node,Bindings,FNameOff),
	    reactivator_executer_2(Node,TCdict,TPid,true,NextC,Rest);
	{ok,_} ->                           % No stop-filename, strange!
	    reactivator_executer_2(Node,TCdict,TPid,true,NextC,Rest);
	false ->                            % Even stranger, does not exist!?
	    reactivator_executer_2(Node,TCdict,TPid,true,NextC,Rest)
    end;	    
%% An inviso command that shall be executed.
reactivator_executer_2(Node,TCdict,TPid,SFlag,_,[{{M,F,Args,_Ref},NextC}|Rest]) ->
    reactivator_executer_cmd(Node,M,F,Args),
    reactivator_executer_2(Node,TCdict,TPid,SFlag,NextC,Rest);
%% Done all log entries found this lap. See if there are more entries by now.
reactivator_executer_2(_Node,_TCdict,TPid,_,Counter,[]) ->
    case reactivator_reply(TPid,Counter) of % Ask the tool process for more entries.
	done ->                             % No more entries in the CHL.
	    done;
	{more,NewUnsortedLog} ->            % Repeat the procedure
	    {more,{{Counter+1},NewUnsortedLog}} % with log entries from Counter+1.
    end.

%% Help function executing a trace case in the reactivators context. Does not
%% return anything significant.
reactivator_executer_tc(Node,Bindings,FileName) ->
    catch file:eval(FileName,erl_eval:add_binding('Nodes',[Node],Bindings)).

reactivator_executer_cmd(nonode@nohost,M,F,Args) ->
    catch apply(M,F,Args);                  % Non-distributed.
reactivator_executer_cmd(Node,M,F,Args) ->
    catch apply(M,F,[[Node]|Args]).

%% Help function returning a list of log entries missing the first entries
%% having a counter less or equal to C1.
reactivator_skip_log_entries([{_,C,_,_}|Rest],C1) when C1>=C ->
    reactivator_skip_log_entries(Rest,C1);
reactivator_skip_log_entries([{_,C}|Rest],C1) when C1>=C ->
    reactivator_skip_log_entries(Rest,C1);
reactivator_skip_log_entries(Log,_) ->
    Log.
%% -----------------------------------------------------------------------------

%% Help function returning the node name to use in an rpc call.
get_rpc_nodename(undefined) ->
    node();
get_rpc_nodename(CNode) ->
    CNode.
%% -----------------------------------------------------------------------------

mk_rt_tag() ->
    inviso_tool.



	    
%% -----------------------------------------------------------------------------
%% Functions for handling the configuration file.
%% -----------------------------------------------------------------------------

%% The inviso tool is configured via start arguments and/or a configuration file.
%% Start arguments will override any definitions in a configuration file.
%% The configuration file is pointed out by either a start argument or the
%% inviso application parameter 'inviso_tool_config_file'.

%% Help function building the internal configuration structure. Configurations
%% in the start argument will override parameters found in a configuration file.
fetch_configuration(Config) ->
    case fetch_config_filename(Config) of
	{ok,FName} ->                       % We are supposed to use a conf-file.
	    case read_config_file(FName) of
		{ok,LD} ->                  % Managed to open a file.
		    NewLD=read_config_list(LD,Config),
		    {ok,NewLD};
		{error,_Reason} ->          % Problem finding/opening file.
		    LD=read_config_list(#ld{},Config),
		    {ok,LD}
	    end;
	false ->                            % No filename specified.
	    LD=read_config_list(#ld{},Config),
	    {ok,LD}
    end.

%% Help function determining the name of the file which shall be consulted as
%% the main configuration file.
%% Returns {ok,FileName} or 'false'. The latter if no name could be determined.
fetch_config_filename(Config) ->
    case catch lists:keysearch(config_file,1,Config) of
	{value,{_,FName}} when list(FName) ->
	    {ok,FName};
	_ ->                                % No filename in the start argument.
	    fetch_config_filename_2()
    end.

fetch_config_filename_2() ->
    case application:get_env(inviso_tool_config_file) of
	{ok,FName} when list(FName) ->
	    {ok,FName};
	_ ->                                % Application parameter not specified.
	    false                           % Means no config file will be used.
    end.

%% Help function reading the configuration file. Returns a #conf or {error,Reason}.
read_config_file(FName) ->
    case catch file:consult(FName) of
	{ok,Terms} ->
	    {ok,read_config_list(#ld{},Terms)};
	{error,Reason} ->
	    {error,{file_consult,Reason}};
	{'EXIT',Reason} ->
	    {error,{failure,Reason}}
    end.

%% Help function traversing the Terms list entering known tag-values into #ld.
read_config_list(LD,Terms) ->
    LD1=read_config_list_2(LD,Terms,nodes),
    LD2=read_config_list_2(LD1,Terms,c_node),
    LD3=read_config_list_2(LD2,Terms,regexp_node),
    LD4=read_config_list_2(LD3,Terms,tc_def_file),
    LD6=read_config_list_2(LD4,Terms,tdg),
    LD8=read_config_list_2(LD6,Terms,debug),
    LD10=read_config_list_2(LD8,Terms,initial_tcs),
    LD11=read_config_list_2(LD10,Terms,dir),
    _LD12=read_config_list_2(LD11,Terms,optg).

read_config_list_2(LD,Terms,Tag) ->
    case catch lists:keysearch(Tag,1,Terms) of
	{value,{_,Value}} ->
	    update_ld_record(LD,Tag,Value);
	_ ->
	    LD                              % Tag not found in Terms (or error!)
    end.
%% -----------------------------------------------------------------------------

%% Function updating a named field in a record. Returns a new record. Note that
%% this function must be maintained due the fact that field names are removed
%% at compile time.
update_ld_record(LD,nodes,Value) when record(LD,ld) ->
    case mk_nodes(Value) of
	{ok,NodesD} ->
	    LD#ld{nodes=NodesD};
	error ->
	    LD
    end;
update_ld_record(LD,Tag,Value) when record(LD,ld) ->
    Index=
	case Tag of
	    c_node ->                       % atom()
		#ld.c_node;
	    regexp_node ->                  % atom()
		#ld.regexp_node;
	    tc_def_file ->                  % string()
		#ld.tc_def_file;
	    initial_tcs ->                  % [{TCname,VarList},...]
		#ld.initial_tcs;
	    history_prefix ->               % string()
		#ld.history_prefix;
	    debug ->                        % true | false
		#ld.debug;
	    dir ->                          % string()
		#ld.dir;
	    optg ->                         % {Mod,Func,Args}
		#ld.optg;
	    tdg ->                          % {Mod,Func,Args}
		#ld.tdg
	end,
    setelement(Index,LD,Value).             % Cheeting!
%% -----------------------------------------------------------------------------


%% Help function which, if it exists, consults the trace definition file. The
%% idea behind the trace definition file is to point out which trace cases there
%% are, where to find them and how to turn them on and off.
%% Trace case definitions are:
%% {TCname,Type,VariableNameList,ActivatioFileName} |
%%     {TCname,Type,VariableNameList,ActivationFileName,DeactivationFileName}
%%   TCname=atom()
%%   Type=on | on_off
%%   VariableNameList=[atom(),...]
%%   ActivationFileName=DeactivationFileName=string()
read_trace_case_definitions(LD) ->
    case LD#ld.tc_def_file of
	TCfileName when list(TCfileName) ->
	    case catch file:consult(TCfileName) of
		{ok,Terms} ->
		    TCdict=read_trace_case_definitions_2(Terms,mk_tc_dict()),
		    LD#ld{tc_dict=TCdict};
		_ ->
		    LD
	    end;
	_ ->
	    LD
    end.

read_trace_case_definitions_2([{TCname,on,VarNames,FName}|Rest],TCdict) ->
    read_trace_case_definitions_2(Rest,
				  insert_tracecase_tc_dict(TCname,
							   on,
							   VarNames,
							   FName,
							   TCdict));
read_trace_case_definitions_2([{TCname,on_off,VarNames,FNameOn,FNameOff}|Rest],TCdict) ->
    read_trace_case_definitions_2(Rest,
				  insert_tracecase_tc_dict(TCname,
							   on_off,
							   VarNames,
							   FNameOn,
							   FNameOff,
							   TCdict));
read_trace_case_definitions_2([_|Rest],TCdict) ->
    read_trace_case_definitions_2(Rest,TCdict);
read_trace_case_definitions_2([],TCdict) ->
    TCdict.
%% -----------------------------------------------------------------------------


%% =============================================================================
%% Internal data structure functions.
%% =============================================================================

%% -----------------------------------------------------------------------------
%% The nodes database structure.
%% -----------------------------------------------------------------------------

%% The purpose of the nodes database structure is to keep track of what runtime
%% nodes we have, and their current status.
%% Implementation:
%% [{NodeName,AvailableStatus},...] or AvailableStatus in the
%%     non-distributed case.
%%   AvailableStatus={up,Status1} | down
%%     Status1={State,Status} | reactivating
%%       State=tracing | inactive
%%       Status=running | suspended
%%         reactivating=the node is now being brought up to date.
%%         inactive=not tracing, can be initiated and then reactivated.
%% The following states can occure.
%%   {inactive,running}
%%     Mainly when we start the tool, before a session has been started.
%%   {tracing,running}
%%     When a trace session is on-going.
%%   {tracing,suspended}
%%   reactivating
%%     The node is tracing (has always been) but was suspended. It is now
%%     no longer suspended and the tool is redong commands.
%%   {inactive,suspended}
%%     We can end up here if a session is stopped with this node suspended.

%% Returns a nodes database structure filled with the nodes Nodes.
mk_nodes(Nodes) when list(Nodes) ->
    {ok,lists:map(fun(N) when atom(N)->{N,down} end,Nodes)};
mk_nodes(local_runtime) ->                  % The non-distributed case.
    down;
mk_nodes(_Nodes) ->
    error.
%% -----------------------------------------------------------------------------

%% Updated the nodes database structure for each node that has been added.
%% Returns a new nodes database structure.
update_added_nodes([{Node,NodeResult}|Rest],NodesD) ->
    case update_added_nodes_2(NodeResult) of
	ignore ->                           % Added same node twice, ignore second.
	    update_added_nodes(Rest,NodesD);
	R ->
	    case lists:keysearch(Node,1,NodesD) of
		{value,_} ->                % Node already exists, replace!
		    update_added_nodes(Rest,lists:keyreplace(Node,1,NodesD,{Node,R}));
		false ->                    % Strange, unknown node!
		    update_added_nodes(Rest,NodesD)
	    end
    end;
update_added_nodes([],NodesD) ->
    NodesD;
update_added_nodes(NodeResult,_NodesD) ->   % Non distributed case.
    case update_added_nodes_2(NodeResult) of
	ignore ->                           % Hmm, strange and can not happend.
	    down;                           % You have added this node twice.
	Result ->
	    Result                          % Simply replace NodesD.
    end.

update_added_nodes_2({ok,{adopted,tracing,running,_Tag}}) ->
    {up,{tracing,running}};
update_added_nodes_2({ok,{adopted,tracing,{suspended,_SReason},_Tag}}) ->
    {up,{tracing,suspended}};
update_added_nodes_2({ok,{adopted,_,running,_Tag}}) ->
    {up,{inactive,running}};
update_added_nodes_2({ok,{adopted,_,{suspended,_SReason},_Tag}}) ->
    {up,{inactive,suspended}};
update_added_nodes_2({ok,new}) ->
    {up,{inactive,running}};
update_added_nodes_2({ok,already_added}) -> % Attempt to add twice!
    ignore;                                % This is an error value!
update_added_nodes_2({error,_Reason}) ->
    down.
%% -----------------------------------------------------------------------------

%% Function marking all nodes that, according to the returnvalue from init_tracing,
%% now are successfullt initiated as tracing and running.
set_tracing_running_nodes([{Node,{ok,LogResults}}|Rest],NodesD) ->
    case set_tracing_running_nodes_2(LogResults) of
	ok ->                              % The result is good.
	    case lists:keysearch(Node,1,NodesD) of
		{value,_} ->
		    NewNodesD=lists:keyreplace(Node,1,NodesD,{Node,{up,{tracing,running}}}),
		    set_tracing_running_nodes(Rest,NewNodesD);
		false ->                   % Strange.
		    set_tracing_running_nodes(Rest,NodesD)
	    end;
	error ->                           % This node is not tracing.
	    set_tracing_running_nodes(Rest,NodesD)
    end;
set_tracing_running_nodes([{_Node,{error,_Reason}}|Rest],NodesD) ->
    set_tracing_running_nodes(Rest,NodesD);
set_tracing_running_nodes([],NodesD) ->
    NodesD;
set_tracing_running_nodes({ok,LogResults},AvailableStatus) -> % Non-distributed case.
    case set_tracing_running_nodes_2(LogResults) of
	ok ->
	    {up,{tracing,running}};
	error ->
	    AvailableStatus
    end.

set_tracing_running_nodes_2(_LogResults) -> ok. % Should really be better!
%% -----------------------------------------------------------------------------

%% Function updating Node in the NodesD structure and sets it to 'down'.
%% Returns a new nodes structure.
set_down_nodes(Node,[{Node,_}|Rest]) ->
    [{Node,down}|Rest];
set_down_nodes(Node,[NodeStruct|Rest]) ->
    [NodeStruct|set_down_nodes(Node,Rest)];
set_down_nodes(_,[]) ->
    [];
set_down_nodes(_,_) ->                     % Non-distributed case.
    down.                                  % One can argue if this can happend.
%% -----------------------------------------------------------------------------

%% Function updating Node in NodesD to now be suspended. It also returns the
%% current status of the node.
set_suspended_nodes(Node,NodesD) when list(NodesD) ->
    case lists:keysearch(Node,1,NodesD) of
	{value,{_,{up,reactivating}}} ->
	    {ok,{reactivating,
		 lists:keyreplace(Node,1,NodesD,{Node,{up,{tracing,suspended}}})}};
	{value,{_,{up,{State,_}}}} ->
	    {ok,{State,lists:keyreplace(Node,1,NodesD,{Node,{up,{State,suspended}}})}};
	_ ->                               % Strange, the node is down.
	    {error,NodesD}
    end;
set_suspended_nodes(_,{up,reactivating}) -> % Non-distributed case.
    {ok,{reactivating,{up,{tracing,suspended}}}};
set_suspended_nodes(_,{up,{State,_}}) ->
    {ok,{State,{up,{State,suspended}}}}.
%% -----------------------------------------------------------------------------

%% This function is called when reactivation is completed. Hence it moves the
%% node to no longer suspended. Note this can mean that the node is either
%% tracing or inactive.
set_running_nodes(Node,NodesD) when list(NodesD) ->
    case lists:keysearch(Node,1,NodesD) of
	{value,{_,AvailableStatus}} ->
	    lists:keyreplace(Node,1,NodesD,{Node,set_running_nodes_2(AvailableStatus)});
	false ->                           % Very strange!
	    NodesD
    end;
set_running_nodes(_,NodesD) ->             % The non-distributed case.
    set_running_nodes_2(NodesD).

set_running_nodes_2({up,reactivating}) ->
    {up,{tracing,running}};
set_running_nodes_2({up,{State,suspended}}) ->
    {up,{State,running}}.
%% -----------------------------------------------------------------------------

%% Function marking a node as now reactivating. That means it is not suspended
%% any longer (and tracing), but still not part of the set of nodes which shall
%% get all commands. Returns a new NodesD.
set_reactivating_nodes(Node,[{Node,_}|Rest]) ->
    [{Node,{up,reactivating}}|Rest];
set_reactivating_nodes(Node,[NodeStruct|Rest]) ->
    [NodeStruct|set_reactivating_nodes(Node,Rest)];
set_reactivating_nodes(_,[]) ->
    [];
set_reactivating_nodes(_,{up,_}) ->        % The non-distributed case.
    {up,reactivating}.
%% -----------------------------------------------------------------------------

%% Function called when stop-tracing is done. That is all nodes in Nodes shall
%% be inactive now. Note that an inactive node can still be suspended.
%% Returns a new NodesD.
set_inactive_nodes(_,{up,reactivating}) -> % Non-distributed case.
    {up,{inactive,running}};
set_inactive_nodes(_,{up,{tracing,Status}}) ->
    {up,{inactive,Status}};

set_inactive_nodes(Nodes,[{Node,{up,Status1}}|Rest]) ->
    case lists:member(Node,Nodes) of
	true ->
	    case Status1 of
		reactivating ->            % Node is tracing and running.
		    [{Node,{up,{inactive,running}}}|set_inactive_nodes(Nodes,Rest)];
		{tracing,Status} ->
		    [{Node,{up,{inactive,Status}}}|set_inactive_nodes(Nodes,Rest)]
	    end;
	false ->
	    [{Node,{up,Status1}}|set_inactive_nodes(Nodes,Rest)]
    end;
set_inactive_nodes(Nodes,[{Node,down}|Rest]) ->
    [{Node,down}|set_inactive_nodes(Nodes,Rest)];
set_inactive_nodes(_Nodes,[]) ->
    [].
%% -----------------------------------------------------------------------------

%% Returns a list of all node names. Note that it can only be used in the
%% distributed case.
get_all_nodenames_nodes(NodesD) ->
    lists:map(fun({Node,_})->Node end,NodesD).
%% -----------------------------------------------------------------------------

%% Returns a list of all nodes that are up, tracing and running (not suspended),
%% or 'void' in the non-distributed case. This is the list of nodes that shall get
%% inviso commands.
get_nodenames_running_nodes([{Node,{up,{tracing,running}}}|Rest]) ->
    [Node|get_nodenames_running_nodes(Rest)];
get_nodenames_running_nodes([{_Node,_}|Rest]) ->
    get_nodenames_running_nodes(Rest);
get_nodenames_running_nodes([]) ->
    [];
get_nodenames_running_nodes(_) ->
    void.                                   % When non distributed, N/A.
%% -----------------------------------------------------------------------------

get_inactive_running_nodes({up,{inactive,running}}) ->
    local_runtime;
get_inactive_running_nodes(NonDistributed) when not(is_list(NonDistributed)) ->
    [];
get_inactive_running_nodes([{Node,{up,{inactive,running}}}|Rest]) ->
    [Node|get_inactive_running_nodes(Rest)];
get_inactive_running_nodes([{_Node,_}|Rest]) ->
    get_inactive_running_nodes(Rest);
get_inactive_running_nodes([]) ->
    [].
%% -----------------------------------------------------------------------------

%% Returns a list of nodes that are currently tracing (not necessarily running).
%% In the non-distributed case the status of the runtime component will be
%% returned.
get_tracing_nodes([{Node,{up,{tracing,_}}}|Rest]) ->
    [Node|get_tracing_nodes(Rest)];
get_tracing_nodes([_|Rest]) ->
    get_tracing_nodes(Rest);
get_tracing_nodes([]) ->
    [];
get_tracing_nodes(AvailableStatus) ->
    AvailableStatus.
%% -----------------------------------------------------------------------------

%% Function returning the "state" of Node. Mainly used to check if the node is
%% suspended or not.
get_state_nodes(Node,NodesD) when list(NodesD) ->
    case lists:keysearch(Node,NodesD,1) of
	{value,{_,AvailableStatus}} ->
	    get_state_nodes_2(AvailableStatus);
	false ->
	    false
    end;
get_state_nodes(_,NodesD) ->                % Non distributed case.
    get_state_nodes_2(NodesD).

get_state_nodes_2({up,{State,suspended}}) -> % {tracing|inactive,suspended}
    {State,suspended};
get_state_nodes_2({up,reactivating}) ->
    reactivating;
get_state_nodes_2({up,{_,running}}) ->
    running;
get_state_nodes_2(down) ->
    down.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% The trace_state.
%% -----------------------------------------------------------------------------

%% The trace state reflects if the inviso_tool is tracing or not. It means for
%% instance that if a node is reconnected and the tool is tracing, the newly
%% reconnected node will be (re)initiated to trace (unless it already is tracing
%% of course, which may be the case if it autostarted)tracing is initiated or not.

%% Returns the correct value indicating that we are tracing now.
tracing_tracing() ->
    tracing.
%% -----------------------------------------------------------------------------

%% Returns true or false depending on if we are tracing now or not.
is_tracing(tracing) ->
    true;
is_tracing(_) ->
    false.
%% -----------------------------------------------------------------------------

%% Returns the correct value indicating that the tool is not tracing.
idle_tracing() ->
    idle.
%% -----------------------------------------------------------------------------

%% -----------------------------------------------------------------------------
%% The tracer_data datastructure.
%% -----------------------------------------------------------------------------

%% The tracer_data structure collects the tracer data arguments used to init tracing
%% by this inviso tool. The args are saved per session. Each session has
%% a number.
%% Implementation:
%%     Sessions=[{SessionNr,TDGargs},...]
%%     SessionNr=integer()
%%     TDGargs=list(), args given to the tracer data generator
%%       minus the first argument which is the Node name.

%% Function taking tracerdata args structure inserting yet another session.
%% Returns {SessionNr,NewTDs}.
insert_td_tracer_data(TDGargs,TDs=[{SNr,_}|_]) ->
    {SNr+1,[{SNr+1,TDGargs}|TDs]};
insert_td_tracer_data(TDGargs,undefined) ->
    {1,[{1,TDGargs}]}.
%% -----------------------------------------------------------------------------

%% Returns the latest session nr.
get_latest_session_nr_tracer_data([{SessionNr,_}|_]) ->
    SessionNr.
%% -----------------------------------------------------------------------------

%% Returns the tracer data arguments used when creating the trace data for the
%% latest session.
get_latest_tdgargs_tracer_data([{_,TDGargs}|_]) ->
    TDGargs.
%% -----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% The tc_dict or trace case dictionary datastructure.
%% -----------------------------------------------------------------------------

%% The tc_dict stores information about all available trace cases.
%% Implementation:
%%   [{TCname,Type,VarNames,FNameOn [,FNameOff]},...]
%%     TCname=atom()
%%     Type=on | on_off
%%     VarNames=[atom(),...]
%%     FNameOn=FNameOff=string()

%% Returns the empty trace case dictionary.
mk_tc_dict() ->
    [].
%% -----------------------------------------------------------------------------

%% Function inserting a new trace case into the trace case dictionary.
insert_tracecase_tc_dict(TCname,on,VarNames,FNameOn,TCdict) ->
    [{TCname,on,VarNames,FNameOn}|TCdict].
insert_tracecase_tc_dict(TCname,on_off,VarNames,FNameOn,FNameOff,TCdict) ->
    [{TCname,on_off,VarNames,FNameOn,FNameOff}|TCdict].
%% -----------------------------------------------------------------------------

%% Function finding a trace case definition in the tc_dict structure.
%% Returns {ok,{TCname,Type,VarNAmes,FNameOn [,FNameOff]}} or 'false'.
get_tracecase_tc_dict(TCname,[Tuple|_]) when element(1,Tuple)==TCname ->
    {ok,Tuple};
get_tracecase_tc_dict(TCname,[_|Rest]) ->
    get_tracecase_tc_dict(TCname,Rest);
get_tracecase_tc_dict(_,[]) ->
    false.
%% -----------------------------------------------------------------------------

%% Function working on the trace case definition returned by get_tracecase_tc_dict/2
%% function.
%% Returning {ok,ActivationFileName}.
get_tc_activate_fname({_TCname,_Type,_VarNames,FNameOn}) ->
    {ok,FNameOn};
get_tc_activate_fname({_TCname,_Type,_VarNames,FNameOn,_FNameOff}) ->
    {ok,FNameOn}.

get_tc_deactivate_fname({_TCname,_Type,_VarNames,_FNameOn,FNameOff}) ->
    {ok,FNameOff};
get_tc_deactivate_fname(_) ->               % Not a case with off function.
    false.

get_tc_varnames({_TCname,_Type,VarNames,_FNameOn}) ->
    VarNames;
get_tc_varnames({_TCname,_Type,VarNames,_FNameOn,_FNameOff}) ->
    VarNames.

%% -----------------------------------------------------------------------------


%% The Command History Log (CHL) stores commands to make it possible to
%% reactivate suspended nodes, reconnect restarted nodes, and to make
%% autostart files.
%% Each time tracing is initiated (that is started) the CHL is cleared since
%% it would not make scense to repeat commands from an earlier tracing at
%% reactivation for instance.

%% Implementation: {NextCounter,OnGoingList,ETStable}
%%   NextCounter=integer(), next command number - to be able to sort them in order.
%%   OnGoingList=[{ProcH,{TCname,ID}},...]
%%   ID=term(), instance id for this execution of this trace case.
%%   ETStable=tid() -> {{TCname,Id},Counter,running,Bindings,Result} |
%%                     {{TCname,Id,#Ref},Counter,State1,Bindings} |
%%                     {{M,F,Args,#Ref},Counter,Result}
%%     Counter=integer(), the order-counter for this logged entry.
%%     State1=activating | stopping
%%     Result=term(), the result returned from the tr-case or inviso call.


%% Returning an initial empty CHL.
mk_chl(undefined) ->
    {1,[],ets:new(inviso_tool_chl,[set,protected])};
mk_chl({_,_,TId}) ->
    ets:delete(TId),
    mk_chl(undefined).

%% Function looking up the state of this trace case.    
find_id_chl(TCname,Id,{_NextCounter,_OnGoingList,TId}) ->
    case ets:lookup(TId,{TCname,Id}) of
	[{_,_,running,Bindings,_Result}] -> % The trace case is tracing.
	    {ok,Bindings};
	[{_,_,State,_}] ->                  % activating or stopping.
	    State;
	[] ->
	    false
    end.

%% Function finding the Trace case associated with a process handle
%% doing this trace case's activation or stopping.
find_tc_executer_chl(ProcH,{_,OnGoingList,TId}) ->
    case lists:keysearch(ProcH,1,OnGoingList) of
	{value,{_,{TCname,Id}}} ->
	    [{_,_,State,_}]=ets:lookup(TId,{TCname,Id}),
	    {State,{TCname,Id}};            % Should be activating or stopping.
	false ->
	    false
    end.

%% Adds a Trace case to the CHL. This is done when it is turned on. Or when it
%% is called for trace cases that do not have on/off functionality.
set_activating_chl(TCname,Id,{Counter,OnGoingList,TId},Bindings,ProcH) ->
    ets:insert(TId,{{TCname,Id},Counter,activating,Bindings}),
    {Counter+1,[{ProcH,{TCname,Id}}|OnGoingList],TId}.

%% Function marking a trace case as now running. That is the activation
%% phase is completed. It is normaly completed when the process executing
%% the trace case signals that it is done.
set_running_chl(ProcH,TCname,Id,Result,{NextCounter,OnGoingList,TId}) ->
    [{_,Counter,_,Bindings}]=ets:lookup(TId,{TCname,Id}),
    ets:insert(TId,{{TCname,Id},Counter,running,Bindings,Result}),
    NewOnGoingList=lists:keydelete(ProcH,1,OnGoingList),
    {NextCounter,NewOnGoingList,TId}.

%% Function marking trace case TCname with identifier Id as now in its stopping
%% state. Where ProcH is the handler to the process running the stopping
%% trace case.
set_stopping_chl(TCname,Id,{NextCounter,OnGoingList,TId},ProcH)->
    [{_,Counter,_,Bindings,_}]=ets:lookup(TId,{TCname,Id}),
    ets:insert(TId,{{TCname,Id},Counter,stopping,Bindings}),
    {NextCounter,[{ProcH,{TCname,Id}}|OnGoingList],TId}.

%% Function removing a TCname-Id from the CHL. This is mostly used
%% if activating the trace case failed for some reason. We do not then
%% expect the user to stop the trace case. Hence it must be removed now.
%% A reactivation process may have noticed the activating-entry and started
%% to activate it. But since the general state reached after an unsuccessful
%% activation can not easily be determined, we don't try to do much about it.
del_tc_chl(ProcH,TCname,Id,{NextCounter,OnGoingList,TId}) ->
   ets:delete(TId,{TCname,Id}),
   NewOnGoingList=lists:keydelete(ProcH,1,OnGoingList),
   {NextCounter,NewOnGoingList,TId}.

%% Function nullifying the trace case making it not become reactivated
%% and similar.
%% This is tricky: We remove the "start entry" since any reactivation process
%% started after this point shall not do the start nor the stop. But we must
%% insert the stop since there may be on-going reactivation processes that will
%% consult the CHL to see what has happend since it started to be completely
%% upto date.
nullify_chl(ProcH,TCname,Id,{NextCounter,OnGoingList,TId}) ->
    [{_,_Counter,_,Bindings}]=ets:lookup(TId,{TCname,Id}),
    ets:delete(TId,{TCname,Id}),
    ets:insert(TId,{{TCname,Id,make_ref()},NextCounter,stopped,Bindings}),
    NewOnGoingList=lists:keydelete(ProcH,1,OnGoingList),
    {NextCounter+1,NewOnGoingList,TId}.

%% Function stopping all processes saved as being now running tc executers.
%% This is useful as cleanup during stop tracing for instance.
%% Returns a new CHL which is not in all parts correct. Entries in the
%% ETS table are for instance not properly state-changed. But the CHL will
%% from now on only be used to create command files and similar.
stop_all_tc_executer_chl({NextCounter,[{ProcH,_}|Rest],TId}) ->
    exit(ProcH,kill),
    stop_all_tc_executer_chl({NextCounter,Rest,TId});
stop_all_tc_executer_chl({NextCounter,[],TId}) ->
    {NextCounter,[],TId}.

%% Function adding a "plain" inviso call to the CHL.
add_inviso_call_chl(Cmd,Args,{NextCounter,OnGoingList,TId}) ->
    ets:insert(TId,{{inviso,Cmd,Args,make_ref()},NextCounter}),
    {NextCounter+1,OnGoingList,TId}.

%% Returns the highest used counter number in the command history log.
get_highest_used_counter_chl({NextCounter,_,_}) ->    
    NextCounter-1.

%% Returns a handler to the log entries. In this case the ETS table Id.
get_loghandler_chl({_,_,TId}) ->
    TId;
get_loghandler_chl(_) ->
    undefined.

%% Function returning a list of log entries. Note that the list is unsorted
%% in respect to Counter.
get_loglist_chl({_,_,TId}) ->
    L=ets:tab2list(TId),
    lists:map(fun({{{TC,Id},C,S,B,_Result}}) -> {{TC,Id},C,S,B};
		 (Tuple={{_TC,_Id,_Ref},_C,_S,_B}) -> Tuple;
		 (Tuple={{_M,_F,_Args,_Ref},_C}) -> Tuple
	      end,
	      L);
get_loglist_chl(_) ->                       % The history is not initiated, ever!
    [].
%% -----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% Reactivators data structure.
%% -----------------------------------------------------------------------------

%% Function adding a new node-reactivatorpid pair to the reactivators structure.
add_reactivators(Node,Pid,Reactivators) ->
    [{Node,Pid}|Reactivators].

%% Function removing a reactivator entry from the reactivators structure.
del_reactivators(RPid,[{_Node,RPid}|Rest]) ->
    Rest;
del_reactivators(RPid,[Element|Rest]) ->
    [Element|del_reactivators(RPid,Rest)];
del_reactivators(_,[]) ->                   % This should not happend.
    [].

get_node_reactivators(RPid,Reactivators) ->
    case lists:keysearch(RPid,2,Reactivators) of
	{value,{Node,_}} ->
	    Node;
	false ->                            % This should not happend.
	    false
    end.

%% Function stopping all running reactivator processes. Returns a new empty
%% reactivators structure.
stop_all_reactivators([{_Node,Pid}|Rest]) ->
    exit(Pid,kill),
    stop_all_reactivators(Rest);
stop_all_reactivators([]) ->
    [].                                     % Returns an empty reactivators.

%% Help function stopping the reactivator (if any) that reactivates Node.
%% Returns a new list of reactivators structure.
stop_node_reactivators(Node,[{Node,Pid}|Rest]) ->
    exit(Pid,kill),
    Rest;
stop_node_reactivators(Node,[E|Rest]) ->
    [E|stop_node_reactivators(Node,Rest)];
stop_node_reactivators(_,_) ->
    true.
%% -----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% Started initial trace cases data structure.
%% -----------------------------------------------------------------------------

%% This datastructure keeps information about ongoing trace cases started
%% automatically at init_tracing. These must be automatically stopped when calling
%% stop_tracing.

add_initial_tcs(TCname,Id,StartedInitialTcs) ->
    [{TCname,Id}|StartedInitialTcs].
%% -----------------------------------------------------------------------------


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
