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
-module(dbg).
-export([p/1,p/2,c/3,c/4,i/0,i/1,stop/0,tracer/0,tracer/1,tracer/2,
	 get_tracer/0, tp/2, tp/3, tp/4, ctp/1, ctp/2, ctp/3,
	 ltp/0, wtp/1, rtp/1, dtp/0, dtp/1, n/1, cn/1, ln/0]).
%-export([ptp/0]).

-export([trace_port/2, trace_client/2, trace_client/3, stop_trace_client/1]).

%% Local exports
-export([init/2,do_relay/1,tracer_init/2,tracer_loop/2, start_tc_loop/2]).

%%% Client functions.

%%
%% n(Node) -> {ok, Node} | {error, Reason}
%% Adds Node to the list of traced nodes.
%%
n(Node) ->
    case (catch net_adm:ping(Node)) of
	{'EXIT',_} ->
	    {error, {bad_node, Node}};
	pang ->
	    {error, {nodedown, Node}};
	pong ->
	    req({add_node, Node});
	Other ->
	    {error, Other}
    end.
%%
%% cn(Node) -> ok
%% Remove Node from the list of traced nodes.
%%    
cn(Node) ->
    req({remove_node, Node}).

%%
%% ln() -> ok
%% List traced nodes (except local node)
%%
ln() ->
    lists:foreach(fun(X) ->
			  io:format("~p~n",[X])
		  end,
		  req(get_nodes)),
    ok.


%%
%% tp(Module, Pattern) | tp(Module,Function,Pattern) |
%% tp(Module,Function,Arity,Pattern) | tp({M,F,A},Pattern) 
%% -> {ok, [{matched, N}]} | {ok, [{matched,N}, {saved, M}]} | {error, Reason}
%% Set trace pattern for function or group of functions.
%%
tp(Module, Function, Pattern) ->
  tp({Module, Function, '_'}, Pattern).
tp(Module, Function, Arity, Pattern) ->
  tp({Module, Function, Arity}, Pattern).
tp(Module, Pattern) when atom(Module) ->
    tp({Module, '_', '_'}, Pattern);
tp({Module, Function, Arity} = X, Pattern) when integer(Pattern) ->
    case ets:lookup(get_pattern_table(), Pattern) of
	[{_,NPattern}] ->
	    tp(X, NPattern);
	_ ->
	    {error, unknown_pattern}
    end;
tp({Module, Function, Arity} = X, Pattern) when list(Pattern) ->
    RemoteNodes = req(get_nodes),
    case Module of
	'_' -> 
	    ok;
	M when atom(M) ->
	    (catch M:module_info()), % Make it load
	    lists:foreach(fun(Node) ->
				  (catch rpc:call(Node, M, module_info, []))
			  end,
			  RemoteNodes)
    end,
    case (catch erlang:trace_pattern(X, Pattern)) of
	{'EXIT', Reason} ->
	    case lint_tp(Pattern) of
		{ok,_} ->
		    {error, {badarg, X}};
		Other ->
		    Other
	    end;
	Matched ->
	    SaveInfo = case save_pattern(Pattern) of
			   N when integer(N), N > 0 ->
			       [{saved, N}];
			   _ ->
			       []
		       end,
	    {ok, [{matched, Matched} | remote_tp(RemoteNodes, X, Pattern)] 
	     ++ SaveInfo}
    end.

remote_tp(RN, MFA, P) ->
    lists:map(fun(Node) ->
			  case (catch rpc:call(
					Node,
					erlang,
					trace_pattern,
					[MFA,
					P])) of
			      N when integer(N) ->
				  {matched, Node, N};
			      Else ->
				  {matched, Node, 0, Else}
			  end
	      end,
	      RN).

%%
%% ctp(Module) | ctp(Module,Function) | ctp(Module,Function,Arity) |
%% ctp({M,F,A}) ->
%% {ok, [{matched, N}]} | {error, Reason}
%% Clears trace pattern for function or group of functions.
%%
ctp(Module, Function) ->
    ctp({Module, Function, '_'}).
ctp(Module, Function, Arity) ->
    ctp({Module, Function, Arity}).
ctp(Module) when atom(Module) ->
    ctp({Module, '_', '_'});
ctp({Module, Function, Arity}) ->
    case (catch erlang:trace_pattern({Module, Function, Arity}, false)) of
	{'EXIT',_} ->
	    {error, badarg};
	Else ->
	    {ok, [{matched, Else} | remote_tp(req(get_nodes),
					      {Module, Function, Arity},
					      false)]}
    end.

%%
%% ltp() -> ok
%% List saved trace patterns.
%%
ltp() ->
    pt_doforall(fun({X, El},_Ignore) -> io:format("~p: ~p~n", [X,El]) end,[]).

%%
%% dtp() | dtp(N) -> ok
%% Delete saved pattern with number N or all saved patterns
%%
dtp() ->
    pt_doforall(fun({Key, _}, _) ->
			dtp(Key)
		end,
		[]).
dtp(N) ->
    ets:delete(get_pattern_table(), N),
    ok.

%%
%% wtp(FileName) -> ok | {error, Reason}
%% Writes all current saved trace patterns to a file.
%%
wtp(FileName) ->
    case file:open(FileName,[write]) of
	{error, Reason} ->
	    {error, Reason};
	{ok, File} ->
	    pt_doforall(fun({_, Val}, _) ->
				io:format(File, "~p.~n", [Val])
			end,
			[]),
	    file:close(File),
	    ok
    end.

%%
%% rtp(FileName) -> ok | {error, Reason}
%% Reads in previously saved pattern file and merges the contents
%% with what's there now.
%%
rtp(FileName) ->
    T = get_pattern_table(),
    case file:consult(FileName) of
	{error, Reason1} ->
	    {error, {read_error, Reason1}};
	{ok, Data} ->
	    case check_list(Data) of
		ok ->
		    lists:foreach(fun(X) ->
					  save_pattern(X,T)
				  end, Data),
		    ok;
		{error, Reason2} ->
		    {error, {file_format_error, Reason2}}
	    end
    end.

tracer() ->
    tracer(process).

tracer(process) ->
    tracer(process, {fun dhandler/2,false});
tracer(Type) ->
    tracer(Type, false).

tracer(port, Fun) when function(Fun) ->
    start(Fun);

tracer(port, Port) when port(Port) ->
    start(fun() -> Port end);

tracer(process, {Handler,HandlerData}) ->
    start(fun() -> start_tracer_process(Handler, HandlerData) end).

trace_port(file, Filename) when list(Filename) ->
    fun() ->
	    (catch erl_ddll:load_driver(
		     filename:join(
		       code:priv_dir(runtime_tools), 
		       "lib"), 
		     "trace_file_drv")),
	    open_port({spawn, "trace_file_drv " ++ Filename}, 
		      [eof])
    end;

trace_port(ip, Portno) when integer(Portno) -> 
    trace_port(ip,{Portno,50});

trace_port(ip, {Portno, Qsiz}) when integer(Portno), integer(Qsiz) -> 
    fun() ->
	    (catch erl_ddll:load_driver(
		     filename:join(
		       code:priv_dir(runtime_tools), 
		       "lib"), 
		     "trace_ip_drv")),
	    L = lists:flatten(
		  io_lib:format("trace_ip_drv ~p ~p 2",
				[Portno, Qsiz])),
	    open_port({spawn, L}, [eof])
    end.

trace_client(file, Filename) when list(Filename) ->
    trace_client1(file, Filename, {fun dhandler/2,false});
trace_client(ip, Portno) when integer(Portno) ->
    trace_client1(ip, {"localhost", Portno}, {fun dhandler/2,false});
trace_client(ip, {Host, Portno}) when integer(Portno) ->
    trace_client1(ip, {Host, Portno}, {fun dhandler/2,false}).

trace_client(file, Filename, {Fun,Data} ) when list(Filename), function(Fun) ->
    trace_client1(file, Filename, {Fun,Data});
trace_client(ip, Portno, {Fun,Data}) when integer(Portno), function(Fun) ->
    trace_client1(ip, {"localhost", Portno}, {Fun,Data});
trace_client(ip, {Host, Portno}, {Fun,Data}) when integer(Portno), 
						  function(Fun) ->
    trace_client1(ip, {Host, Portno}, {Fun,Data}).

trace_client1(Type, OpenData, HandlerData) ->
    case req({link_to, spawn(?MODULE, start_tc_loop, 
			     [fun() -> 
				      gen_reader(Type, OpenData) 
			      end, 
			      HandlerData])}) of
	{ok, Pid} ->
	    Pid;
	Other ->
	    Other
    end.

stop_trace_client(Pid)->
    process_flag(trap_exit,true),
    link(Pid),
    exit(to_pid(Pid),abnormal),
    Res = receive 
	      {'EXIT', Pid, _} ->
		  ok
	  after 5000 ->
		  {error, timeout}
	  end,
    process_flag(trap_exit,false),
    Res.

p(Pid) ->
    p(Pid, [m]).

p(Pid, Flags) when atom(Flags) ->
    p(Pid, [Flags]);
p(Pid, Flags) ->
    req({p,Pid,Flags}).

i() -> req(i).
i(Pid) -> req({i,Pid}).
	
c(M, F, A) ->
    c(M, F, A, all).
c(M, F, A, Flags) ->
    p(self(), Flags),
    Res = apply(M, F, A),
    p(self(), clear),
    Res.

stop() ->
    req(stop).


%%% Calling the server.

req(R) ->
    ensure() ! {self(),R},
    receive
	{dbg,Reply} -> Reply
    end.

ensure() ->
    case whereis(dbg) of
	undefined -> 
	    case tracer() of
		{ok, P} ->
		    P;
		{error, already_started} ->
		    whereis(dbg);
		Else ->
		    Else
	    end;
	Pid -> 
	    Pid
    end.


%%% Server implementation.

start(TracerFun) ->
    Tracer = spawn(?MODULE, init, [self(),TracerFun]),
    receive
	{Tracer,started} ->
	    {ok, Tracer};
	{Tracer, Error} ->
	    Error
    end.

init(Parent, StartTracer) ->
    process_flag(trap_exit, true),
    case (catch register(dbg, self())) of
	{'EXIT', _} ->
	    Parent ! {self(), {error, already_started}};
	_ ->
	    Tracer = StartTracer(),
	    Parent ! {self(),started},
	    loop(Tracer,[], [])
    end.

%
% SurviveLinks = Processes we should take with us while falling, 
%                but not get killed by if they die (i. e. trace clients.)
%
loop(Tracer,SurviveLinks, Table) ->
    receive
	{From,i} ->
	    reply(From, display_info(processes())),
	    loop(Tracer, SurviveLinks, Table);
	{From,{i,Pid}} ->
	    reply(From, display_info(to_pid(Pid))),
	    loop(Tracer, SurviveLinks, Table);
	{From,{p,Pid,Flags}} ->
	    reply(From, trace_process(Tracer, to_pid(Pid), Flags)),
	    loop(Tracer, SurviveLinks, Table);
	{From,get_tracer} ->
	    reply(From, {ok,Tracer}),
	    loop(Tracer, SurviveLinks, Table);
	{From, get_table} ->
	    Tab = case Table of
		      [] ->
			  ets:new(dbg_tab, [ordered_set, public]);
		      T ->
			  T
		  end,
	    reply(From, {ok, Tab}),
	    loop(Tracer, SurviveLinks, Tab);
	{From,stop} ->
	    reply(From, ok),
	    exit(normal);
	{From, {link_to, Pid}} -> 	    
	    case (catch link(Pid)) of
		{'EXIT', Reason} ->
		    reply(From, {error, Reason}),
		    loop(Tracer, SurviveLinks, Table);
		_ ->
		    reply(From, {ok, Pid}),
		    loop(Tracer, [Pid | SurviveLinks], Table)
	    end;
	{From, {add_node, Node}} ->
	    case (catch relay(Node, Tracer)) of
		{'EXIT', Something} ->
		    reply(From,
			  {error, cant_trace_remote_pid_to_local_port});
		Pid when pid(Pid) ->
		    reply(From, {ok, Node});
		SomethingElse ->
		    reply(From, {error, SomethingElse})
	    end,
	    loop(Tracer, SurviveLinks, Table);
	{From, {remove_node, Node}} ->
	    erase(Node),
	    reply(From, ok),
	    loop(Tracer, SurviveLinks, Table);
	{From, get_nodes} ->
	    reply(From, lists:map(fun({N,_}) -> N end, get())),
	    loop(Tracer, SurviveLinks, Table);
	{'EXIT', Pid, _} ->
	    case lists:delete(Pid, SurviveLinks) of
		SurviveLinks ->
		    exit(normal);
		NewSLinks ->
		    loop(Tracer, NewSLinks, Table)
	    end;
	Other ->
	    io:format(user,"** dbg got garbage: ~p~n", [Other]),
	    loop(Tracer, SurviveLinks, Table)
    end.

reply(Pid, Reply) ->
    Pid ! {dbg,Reply}.


%%% A process-based tracer.

start_tracer_process(Handler, HandlerData) ->
    spawn_link(?MODULE, tracer_init, [Handler,HandlerData]).

tracer_init(Handler, HandlerData) ->
    process_flag(trap_exit, true),
    process_flag(priority, max),
    tracer_loop(Handler, HandlerData).

tracer_loop(Handler, Hdata) ->
    receive
	{From,stop} ->
	    From ! {stop,Hdata};
	{'EXIT',_,Reason} ->
	    ok;
	Trace ->
	    NewData = recv_all_traces(Trace, Handler, Hdata),
	    tracer_loop(Handler, NewData)
    end.
    
recv_all_traces(Trace, Handler, Hdata) ->
    Suspended = suspend(Trace, ordsets:new_set()),
    recv_all_traces(Suspended, Handler, Hdata, [Trace]).

recv_all_traces(Suspended0, Handler, Hdata, Traces) ->
    receive
	Trace when tuple(Trace), element(1, Trace) == trace ->
	    Suspended = suspend(Trace, Suspended0),
	    recv_all_traces(Suspended, Handler, Hdata, [Trace|Traces]);
	Trace when tuple(Trace), element(1, Trace) == trace_ts ->
	    Suspended = suspend(Trace, Suspended0),
	    recv_all_traces(Suspended, Handler, Hdata, [Trace|Traces]);
	Trace when tuple(Trace), element(1, Trace) == seq_trace ->
	    Suspended = suspend(Trace, Suspended0),
	    recv_all_traces(Suspended, Handler, Hdata, [Trace|Traces]);
	Trace when tuple(Trace), element(1, Trace) == drop ->
	    Suspended = suspend(Trace, Suspended0),
	    recv_all_traces(Suspended, Handler, Hdata, [Trace|Traces]);
	Other ->
	    %%% Is this really a good idea?
	    io:format(user,"** tracer received garbage: ~p~n", [Other]),
	    recv_all_traces(Suspended0, Handler, Hdata, Traces)
    after 0 ->
	    NewHdata = invoke_handler(Traces, Handler, Hdata),
	    resume(ordsets:set_to_list(Suspended0)),
	    NewHdata
    end.

invoke_handler([Tr|Traces], Handler, Hdata0) ->
    Hdata = invoke_handler(Traces, Handler, Hdata0),
    Handler(Tr, Hdata);
invoke_handler([], Handler, Hdata) ->
    Hdata.

suspend({trace,From,call,Func}, Suspended) when node(From) == node() ->
    case ordsets:is_element(From, Suspended) of
	true -> Suspended;
	false ->
	    case (catch erlang:suspend_process(From)) of
		true ->
		    ordsets:add_element(From, Suspended);
		_ ->
		    Suspended
	    end
    end;
suspend(Other, Suspended) -> Suspended.

resume([Pid|Pids]) when node(Pid) == node() ->
    (catch erlang:resume_process(Pid)),
    resume(Pids);
resume([]) -> ok.



%%% Utilities.

trac(Pid, How, Flags) when atom(Pid) ->
    case catch erlang:trace(Pid, How, Flags) of
	{'EXIT',Reason} -> 
	    case erlang:is_process_alive(Pid) of
		false ->
		    {error, {dead_process, Pid}};
		_ ->
		    {error,Reason}
	    end;
	N -> 
	    {ok,[{matched, N} | trac_remote(Pid, How, Flags)]}
    end;
trac(Pid, How, Flags) when node(Pid) == node() ->
    case catch erlang:trace(Pid, How, Flags) of
	{'EXIT',Reason} -> 
	    {error,Reason};
	N -> 
	    {ok,[{matched,N}]}
    end;

%%
%% Remote tracing...
%%
trac(Pid, How, Flags) ->
    case lists:keysearch(tracer,1,Flags) of
	{value, {tracer, Port}} when port(Port) ->
	    {error, {cant_trace_remote_pid_to_local_port, Pid}};
	{value, {tracer, Pid1}} when pid(Pid1) ->
	    Node = node(Pid), 
	    case get(Node) of
		Relay when pid(Relay) ->
		    case (catch rpc:call(
				  Node, 
				  erlang,
				  trace,
				  [Pid,
				   How,
				   lists:keyreplace(
				     tracer,
				     1,
				     Flags,
				     {tracer, Relay})])) of
			N when integer(N) -> 
			    {ok,[{matched,N}]};
			{'EXIT',ExitCode} ->
			    case (catch rpc:call(Node, 
						 erlang,
						 is_process_alive,
						  [Pid])) of
				false ->
				    {error, {dead_process, Pid}};
				_ ->
				    {error, ExitCode}
			    end;
			Other ->
			    {error, Other}
		    end;
		Else ->
		    {error, {node_not_activated, Node}}
	    end;
	Strange ->
	    {error, unsupported_tracer}
    end.

trac_remote(AtomPid, How, Flags) ->
    lists:map(
      fun({Node, Relay}) ->
	      case rpc:call(Node, 
			    erlang, 
			    trace,
			    [AtomPid, 
			     How,
			     lists:keyreplace(tracer,1,
					      Flags,
					      {tracer, Relay})]) of
		  N when integer(N) ->
		      {matched, Node, N};
		  Else ->
		      {matched, Node, 0, Else}
	      end
      end,
      get()).

%% Since we are not allowed to do erlang:trace/3 on a remote
%% process, we create a relay process at the remote node.

relay(Node,To) when Node /= node() ->
    case get(Node) of
	undefined -> 
	    Pid = spawn_link(Node, ?MODULE, do_relay, [To]),
	    put(Node, Pid),
	    Pid;
	Pid -> 
	    Pid
    end.

do_relay(RelP) ->
    process_flag(trap_exit, true),
    do_relay_1(RelP).

do_relay_1(RelP) ->
    receive
	{'EXIT', _P, _} ->
	    exit(normal);
	Other ->             %% Here is the normal case for trace i/o
	    RelP ! Other, 
	    do_relay_1(RelP)
    end.

dhandler(Trace, Data) when element(1, Trace) == trace, size(Trace) >= 3 ->
    dhandler1(Trace, size(Trace));
dhandler(Trace, Data) when element(1, Trace) == trace_ts, size(Trace) >= 4 ->
    dhandler1(Trace, size(Trace)-1);
dhandler(Trace, Data) when element(1, Trace) == drop, size(Trace) == 2 ->
    io:format(user, "*** Dropped ~p messages.~n", [element(2,Trace)]),
    ok;
dhandler(Trace, Data) when element(1, Trace) == seq_trace, size(Trace) >= 3 ->
    SeqTraceInfo = case Trace of
		       {seq_trace, Lbl, STI, TS} ->
			   io:format(user, "SeqTrace ~p [~p]: ",
				     [TS, Lbl]),
			   STI;
		       {seq_trace, Lbl, STI} ->
			  io:format(user, "SeqTrace [~p]: ",
				     [Lbl]),
			   STI 
		   end,
    case SeqTraceInfo of
	{send, Ser, Fr, To, Mes} ->
	    io:format(user, "(~p) ~p ! ~p [Serial: ~p]~n",
		      [Fr, To, Mes, Ser]);
	{'receive', Ser, Fr, To, Mes} ->
	    io:format(user, "(~p) << ~p [Serial: ~p, From: ~p]~n",
		      [To, Mes, Ser, Fr]);
	{print, Ser, Fr, _, Info} ->
	    io:format(user, "-> ~p [Serial: ~p, From: ~p]~n",
		      [Info, Ser, Fr]);
	Else ->
	    io:format(user, "~p~n", [Else])
    end,
    ok;

dhandler(Trace, Data) ->
    ok.

dhandler1(Trace, Size) ->
    Self = self(),
    From = element(2, Trace),
    case element(3, Trace) of
	'receive' ->
	    case element(4, Trace) of
		{dbg,ok} -> ok;
		Message -> io:format(user, "(~p) << ~p~n", [From,Message])
	    end;
	'send' ->
	    Message = element(4, Trace),
	    case element(5, Trace) of
		Self -> ok;
		To -> io:format(user, "(~p) ~p ! ~p~n", [From,To,Message])
	    end;
	call ->
	    case element(4, Trace) of
		MFA when Size == 5 ->
		    Message = element(5, Trace),
		    io:format(user, "(~p) call ~s (~p)~n", [From,ffunc(MFA),Message]);
		MFA ->
		    io:format(user, "(~p) call ~s~n", [From,ffunc(MFA)])
	    end;
	return ->
	    case element(4, Trace) of
		MFA when Size == 5 ->
		    Ret = element(5, Trace),
		    io:format(user, "(~p) ret ~s -> ~p~n", [From,ffunc(MFA),Ret]);
		MFA ->
		    io:format(user, "(~p) ret ~s~n", [From,ffunc(MFA)])
	    end;
	Op ->
	    Data = element(4, Trace),
	    io:format(user, "(~p) ~p ~p~n", [From,Op,Data])
    end.

ffunc({M,F,Arity}) when integer(Arity) ->
    io_lib:format("~p:~p/~p", [M,F,Arity]);
ffunc({M,F, Argl}) ->
    io_lib:format("~p:~p(~s)", [M, F, fargs(Argl)]);
ffunc(X) -> io_lib:format("~p", [X]).

fargs(Arity) when integer(Arity) -> integer_to_list(Arity);
fargs([]) -> [];
fargs([A]) -> io_lib:format("~p", [A]);  %% last arg
fargs([A|Args]) -> [io_lib:format("~p,", [A]) | fargs(Args)].

trace_process(Tracer, {badpid,_}=Bad, Flags) ->
    {error,Bad};
trace_process(Tracer, Pid, [clear]) ->
    trac(Pid, false, [{tracer,Tracer}|all()]);
trace_process(Tracer, Pid, Flags0) ->
    case transform_flags(Flags0) of
	{error,_}=Error -> Error;
	Flags -> trac(Pid, true, [{tracer,Tracer}|Flags])
    end.
	    
transform_flags([]) -> [];
transform_flags([m|Tail]) -> [send,'receive'|transform_flags(Tail)];
transform_flags([s|Tail]) -> [send|transform_flags(Tail)];
transform_flags([r|Tail]) -> ['receive'|transform_flags(Tail)];
transform_flags([c|Tail]) -> [call|transform_flags(Tail)];
transform_flags([call|Tail]) -> [call|transform_flags(Tail)];
transform_flags([p|Tail]) -> [procs|transform_flags(Tail)];
transform_flags([sos|Tail]) -> [set_on_spawn|transform_flags(Tail)];
transform_flags([sol|Tail]) -> [set_on_link|transform_flags(Tail)];
transform_flags([sofs|Tail]) -> [set_on_first_spawn|transform_flags(Tail)];
transform_flags([sofl|Tail]) -> [set_on_first_link|transform_flags(Tail)];
transform_flags([all|_]) -> all();
transform_flags([F|Tail]=List) when atom(F) ->
    case lists:member(F, all()) of
	true -> [F|transform_flags(Tail)];
	false -> {error,{bad_flags,List}}
    end;
transform_flags(Bad) -> {error,{bad_flags,Bad}}.

all() ->
    [send,'receive',call,old_call_trace,procs,garbage_collection,running,
     set_on_spawn,set_on_first_spawn,set_on_link,set_on_first_link,timestamp,arity].

display_info(List) ->
    io:format("~-12s ~-21s Trace ~n", ["Pid", "Initial call"]),
    display_info1(List).

display_info1([Pid|T]) ->
    case pinfo(Pid, initial_call) of
        undefined ->
            display_info1(T);
        {initial_call, Call} ->
	    case tinfo(Pid, flags) of
		{flags,[]} ->
		    display_info1(T);
		{flags,Flags} ->
		    io:format("~-12s ~-21s ~s~n",
			      [io_lib:format("~w",[Pid]),
			       io_lib:format("~p", [Call]),
			       format_trace(Flags)]),
		    display_info1(T)
	    end
    end;
display_info1([]) -> ok.

format_trace([]) -> [];
format_trace([Item]) -> [ts(Item)];
format_trace([Item|T]) -> [ts(Item) ," | ", format_trace(T)].

ts(send) -> "s";
ts('receive') -> "r";
ts(call) -> "c";
ts(procs) -> "p";
ts(set_on_spawn) -> "sos";
ts(set_on_first_spawn) -> "sofs";
ts(set_on_link) -> "sol";
ts(set_on_first_link) -> "sofl";
ts(Other) -> Other.

%%
%% Turn something into a pid, return 'nopid' on failure 
%%

to_pid(new) -> new;
to_pid(all) -> all;
to_pid(existing) -> existing;
to_pid(X) when pid(X) -> X;
to_pid(X) when atom(X) ->
    case whereis(X) of
	undefined -> {badpid,X};
	Pid -> Pid
    end;
to_pid(X) when integer(X) -> to_pid({0,X,1});
to_pid({X,Y,Z}) ->
    list_to_pid(lists:concat(["<",integer_to_list(X),".",
			      integer_to_list(Y),".",
			      integer_to_list(Z),">"]));
to_pid(X) -> {badpid,X}.


pinfo(P, X) when node(P) == node() -> erlang:process_info(P, X);
pinfo(P, X) -> check(rpc:call(node(P), erlang, process_info, [P, X])).

tinfo(P, X) when node(P) == node() -> erlang:trace_info(P, X);
tinfo(P, X) -> check(rpc:call(node(P), erlang, trace_info, [P, X])).

check({badrpc, _}) -> undefined;
check(X) -> X.

start_tc_loop(GenReader, Handler) ->
    tc_loop(GenReader(), Handler).
    
tc_loop(Reader,{Handler, HData}) ->
    case (catch Reader()) of
	{'EXIT', eof} ->
	    exit(normal);
	{'EXIT', Other} ->
	    exit({client_cannot_open, Other});
	Data ->
	    NewHData = Handler(Data,HData),
	    tc_loop(Reader,{Handler, NewHData})
    end.

gen_reader(ip, {Host, Portno}) ->
    case gen_tcp:connect(Host, Portno, [{active, false}]) of
        {ok, Sock} ->    
	    fun() ->
		     [Op | BESiz] = my_ip_read(Sock, 5),
		     Siz = get_be(BESiz),
		     case Op of
			 0 ->
			     B = list_to_binary(my_ip_read(Sock, Siz)),
			     binary_to_term(B);
			 1 ->
			     {drop, Siz};
			 Else ->
			     exit({'bad trace tag', Else})
		     end
	     end;
	Error ->
	    exit(Error)
    end;

gen_reader(file, Filename) ->
    case file:open(Filename, [read, raw, binary]) of
	{ok, File} ->
	    fun() ->
		     [Op | BESiz] = binary_to_list(my_file_read(File, 5)),
		     Siz = get_be(BESiz),
		     case Op of
			 0 ->
			     binary_to_term(my_file_read(File,Siz));
			 Else ->
			     exit({'bad trace tag', Else})
		     end
	     end;
	Error ->
	    exit(Error)
    end.

		     
		     
get_be([A,B,C,D]) ->
    A * 16777216 + B * 65536 + C * 256 + D.

my_file_read(File,N) ->
    case file:read(File, N) of
	{ok, Bin} when size(Bin) =:= N -> 
	    Bin;
	_ ->
	    exit(eof)
    end.

my_ip_read(Sock,N) ->
    case gen_tcp:recv(Sock, N) of
        {ok, Data} ->
	    case length(Data) of
		N ->
		    Data;
		X ->
		    Data ++ my_ip_read(Sock, N - X)
	    end;
	Else ->
	    exit(eof)
    end.


get_tracer() ->
    req(get_tracer).

save_pattern([]) ->
    false;

save_pattern(P) ->
    (catch save_pattern(P, get_pattern_table())).

save_pattern(Pattern, PT) ->
    Last = case ets:last(PT) of
	       Int when integer(Int) ->
		   Int;
	       '$end_of_table' ->
		   0;
	       Else ->
		   throw({error, badtable})
	   end,
    case ets:match_object(PT, {'_', Pattern}) of
	[] ->
	    ets:insert(PT, {Last + 1, Pattern}),
	    Last + 1;
	[{N, Pattern}] ->
	    N
    end.
	    

get_pattern_table() ->
    {ok, Ret} = req(get_table),
    Ret.
		   

pt_doforall(Fun, Ld) ->
    T = get_pattern_table(),
    pt_doforall(T, Fun, ets:first(T), Ld).

pt_doforall(_, _, '$end_of_table', Ld) -> 
    ok;
pt_doforall(T, Fun, Key, Ld) ->
    [Obj] = ets:lookup(T,Key),
    NLd = Fun(Obj,Ld),
    pt_doforall(T,Fun,ets:next(T,Key),NLd).

lint_tp(Pattern) ->
    case erlang:match_spec_test([],Pattern,trace) of
	{ok,_Res,Warnings,_Flags} ->
	    {ok, Warnings};
	{error, Reasons} ->
	    {error, Reasons}
    end.

check_list(T) ->
    case (catch lists:foldl(fun(Val,_) ->
				    {ok,_,_,_} = 
					erlang:match_spec_test([],Val,trace),
				    ok
			    end,
			    ok, T)) of
	{'EXIT',_} ->
	    {error, bad_match_spec};
	ok ->
	    ok;
	Else ->
	    {error, badfile}
    end.

