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
	 tpl/2, tpl/3, tpl/4, ctpl/1, ctpl/2, ctpl/3,
	 ctpg/1, ctpg/2, ctpg/3,
	 ltp/0, wtp/1, rtp/1, dtp/0, dtp/1, n/1, cn/1, ln/0, h/0, h/1]).
%-export([ptp/0]).

-export([trace_port/2, flush_trace_port/0, trace_port_control/1,
	 trace_client/2, trace_client/3, stop_trace_client/1]).

%% Local exports
-export([init/2,do_relay/1,tracer_init/2,tracer_loop/2]).

%% Debug exports
-export([wrap_presort/2, wrap_sort/2, wrap_postsort/1, wrap_sortfix/2,
	 match_front/2, match_rear/2,
	 match_0_9/1]).

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
%% tp/tpl(Module, Pattern) | tp/tpl(Module,Function,Pattern) |
%% tp/tpl(Module,Function,Arity,Pattern) | tp/tpl({M,F,A},Pattern) 
%% -> {ok, [{matched, N}]} | {ok, [{matched,N}, {saved, M}]} | {error, Reason}
%% Set trace pattern for function or group of functions.
%%
tp(Module, Function, Pattern) ->
    do_tp({Module, Function, '_'}, Pattern, []).
tp(Module, Function, Arity, Pattern) ->
    do_tp({Module, Function, Arity}, Pattern, []).
tp(Module, Pattern) when atom(Module) ->
    do_tp({Module, '_', '_'}, Pattern, []);
tp({Module, Function, Arity} = X, Pattern) ->
    do_tp(X,Pattern,[]).
tpl(Module, Function, Pattern) ->
    do_tp({Module, Function, '_'}, Pattern, [local]).
tpl(Module, Function, Arity, Pattern) ->
    do_tp({Module, Function, Arity}, Pattern, [local]).
tpl(Module, Pattern) when atom(Module) ->
    do_tp({Module, '_', '_'}, Pattern, [local]);
tpl({Module, Function, Arity} = X, Pattern) ->
    do_tp(X,Pattern,[local]).
do_tp({Module, Function, Arity} = X, Pattern, Flags) when integer(Pattern) ->
    case ets:lookup(get_pattern_table(), Pattern) of
	[{_,NPattern}] ->
	    do_tp(X, binary_to_term(NPattern), Flags);
	_ ->
	    {error, unknown_pattern}
    end;
do_tp({Module, Function, Arity} = X, Pattern, Flags) when list(Pattern) ->
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
    case (catch erlang:trace_pattern(X, Pattern, Flags)) of
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
	    {ok, [{matched, Matched} | remote_tp(RemoteNodes, X, Pattern, Flags)] 
	     ++ SaveInfo}
    end.

remote_tp(RN, MFA, P, Flags) ->
    lists:map(fun(Node) ->
			  case (catch rpc:call(
					Node,
					erlang,
					trace_pattern,
					[MFA,
					P, Flags])) of
			      N when integer(N) ->
				  {matched, Node, N};
			      Else ->
				  {matched, Node, 0, Else}
			  end
	      end,
	      RN).

%%
%% ctp/ctpl(Module) | ctp/ctpl(Module,Function) | ctp/ctpl(Module,Function,Arity) |
%% ctp/ctpl({M,F,A}) ->
%% {ok, [{matched, N}]} | {error, Reason}
%% Clears trace pattern for function or group of functions.
%%
ctp(Module, Function) ->
    do_ctp({Module, Function, '_'}, []).
ctp(Module, Function, Arity) ->
    do_ctp({Module, Function, Arity}, []).
ctp(Module) when atom(Module) ->
    do_ctp({Module, '_', '_'}, []);
ctp({Module, Function, Arity} = X) ->
    do_ctp(X,[]).
ctpl(Module, Function) ->
    do_ctp({Module, Function, '_'}, [local]).
ctpl(Module, Function, Arity) ->
    do_ctp({Module, Function, Arity}, [local]).
ctpl(Module) when atom(Module) ->
    do_ctp({Module, '_', '_'}, [local]);
ctpl({Module, Function, Arity} = X) ->
    do_ctp(X,[local]).
ctpg(Module, Function) ->
    do_ctp({Module, Function, '_'}, [global]).
ctpg(Module, Function, Arity) ->
    do_ctp({Module, Function, Arity}, [global]).
ctpg(Module) when atom(Module) ->
    do_ctp({Module, '_', '_'}, [global]);
ctpg({Module, Function, Arity} = X) ->
    do_ctp(X,[global]).
do_ctp({Module, Function, Arity},[]) ->
    do_ctp({Module, Function, Arity},[global]),
    do_ctp({Module, Function, Arity},[local]);
do_ctp({Module, Function, Arity},Flags) ->
    case (catch erlang:trace_pattern({Module, Function, Arity}, false, Flags)) of
	{'EXIT',_} ->
	    {error, badarg};
	Else ->
	    {ok, [{matched, Else} | remote_tp(req(get_nodes),
					      {Module, Function, Arity},
					      false, Flags)]}
    end.

%%
%% ltp() -> ok
%% List saved trace patterns.
%%
ltp() ->
    pt_doforall(fun({X, El},_Ignore) -> 
			io:format("~p: ~p~n", [X,El]) 
		end,[]).

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



flush_trace_port() ->
    trace_port_control(flush).

trace_port_control(flush) ->
    case trace_port_control($f, "") of
	{ok, [0]} ->
	    ok;
	{ok, _} ->
	    {error, not_supported_by_trace_driver};
	Other ->
	    Other
    end;
trace_port_control(get_listen_port) ->
    case trace_port_control($p, "") of
	{ok, <<0, IpPort:16>>} ->
	    {ok, IpPort};
	{ok, Other} ->
	    {error, not_supported_by_trace_driver};
	Other ->
	    Other
    end.

trace_port_control(Command, Arg) ->
    case get_tracer() of
	{ok, Port} when port(Port) ->
	    {ok, catch erlang:port_control(Port, Command, Arg)};
	_ ->
	    {error, no_trace_driver}
    end.

					   

trace_port(file, {Filename, wrap, Tail}) ->
    trace_port(file, {Filename, wrap, Tail, 128*1024});
trace_port(file, {Filename, wrap, Tail, WrapSize}) ->
    trace_port(file, {Filename, wrap, Tail, WrapSize, 8});
trace_port(file, {Filename, wrap, Tail, WrapSize, WrapCnt})
  when list(Tail), 
       integer(WrapSize), WrapSize >= 0, WrapSize < (1 bsl 32),
       integer(WrapCnt), WrapCnt >= 1, WrapCnt < (1 bsl 32) ->
    trace_port1(file, Filename, {wrap, Tail, WrapSize, WrapCnt, 0});
trace_port(file, {Filename, wrap, Tail, {time, WrapTime}, WrapCnt})
  when list(Tail), 
       integer(WrapTime), WrapTime >= 1, WrapTime < (1 bsl 32),
       integer(WrapCnt), WrapCnt >= 1, WrapCnt < (1 bsl 32) ->
    trace_port1(file, Filename, {wrap, Tail, 0, WrapCnt, WrapTime});
trace_port(file, Filename) ->
    trace_port1(file, Filename, nowrap);

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

trace_port1(file, Filename, Options) ->
    Driver = "trace_file_drv",
    Name = filename:absname(Filename), 
    %% Absname is needed since the driver uses 
    %% the supplied name without further investigations, 
    %% and if the name is relative the resulting path 
    %% might be too long which can cause a bus error
    %% on vxworks instead of a nice error code return.
    {Wrap, Tail} =
	case Options of
	    {wrap, T, WrapSize, WrapCnt, WrapTime} ->
		{lists:flatten(
		   io_lib:format("w ~p ~p ~p ~p ", 
				 [WrapSize, WrapCnt, WrapTime, length(Name)])),
		 T};
	    nowrap ->
		{"", ""}
	end,
    Command = Driver ++ " " ++ Wrap ++ "n " ++ Name ++ Tail,
    fun() ->
	    (catch erl_ddll:load_driver(
		     filename:join(
		       code:priv_dir(runtime_tools), 
		       "lib"), 
		     Driver)),
	    case Options of
		{wrap, _, _, _} ->
		    %% Delete old files
		    Files = wrap_postsort(wrap_presort(Name, Tail)),
		    lists:foreach(
		      fun(N) -> file:delete(N) end,
		      Files);
		Other ->
		    ok
	    end,
	    open_port({spawn, Command}, [eof])
    end.


trace_client(file, Filename) ->
    trace_client(file, Filename, {fun dhandler/2,false});
trace_client(follow_file, Filename) ->
    trace_client(follow_file, Filename, {fun dhandler/2,false});
trace_client(ip, Portno) when integer(Portno) ->
    trace_client1(ip, {"localhost", Portno}, {fun dhandler/2,false});
trace_client(ip, {Host, Portno}) when integer(Portno) ->
    trace_client1(ip, {Host, Portno}, {fun dhandler/2,false}).

trace_client(file, {Filename, wrap, Tail}, FD) ->
    trace_client(file, {Filename, wrap, Tail, 128*1024}, FD);
trace_client(file, {Filename, wrap, Tail, WrapSize}, FD) ->
    trace_client(file, {Filename, wrap, Tail, WrapSize, 8}, FD);
trace_client(file, 
	     {Filename, wrap, Tail, _, WrapCnt} = WrapSpec, 
	     {Fun, Data} = FD)
  when list(Tail), function(Fun), integer(WrapCnt), WrapCnt >= 1 ->
    trace_client1(file, WrapSpec, FD);
trace_client(file, Filename, {Fun, Data} ) when function(Fun) ->
    trace_client1(file, Filename, {Fun, Data});
trace_client(follow_file, Filename, {Fun, Data} ) when function(Fun) ->
    trace_client1(follow_file, Filename, {Fun, Data});
trace_client(ip, Portno, {Fun, Data}) when integer(Portno), function(Fun) ->
    trace_client1(ip, {"localhost", Portno}, {Fun, Data});
trace_client(ip, {Host, Portno}, {Fun, Data}) when integer(Portno), 
						   function(Fun) ->
    trace_client1(ip, {Host, Portno}, {Fun, Data}).

trace_client1(Type, OpenData, HandlerData) ->
    case req({link_to, 
	      spawn(
		fun() ->
			tc_loop(gen_reader(Type, OpenData), HandlerData)
		end)}) of
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
    P = ensure(), % The pid or perhaps the name of the server
    Mref = erlang:monitor(process, P),
    catch P ! {self(), R}, % May crash if P = atom() and server died
    receive
	{'DOWN', Mref, _, _, _} -> % If server died
	    exit(dbg_server_crash);
	{dbg, Reply} ->
	    erlang:demonitor(Mref),
	    receive {'DOWN', Mref, _, _, _} -> ok after 0 -> ok end,
	    Reply
    end.

%% Returns the pid of the dbg server, or in worst case the name.
%% Starts a new server if necessary.
ensure() ->
    case whereis(dbg) of
	undefined -> 
	    case tracer() of
		{ok, P} ->
		    P;
		{error, already_started} ->
		    dbg
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
	    case (catch StartTracer()) of
		{'EXIT', Reason} ->
		    Parent ! {self(), {error, Reason}};
		Tracer ->
		    Parent ! {self(),started},
		    loop(Tracer,[], [])
	    end
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
	    exit(done);
	{From, {link_to, Pid}} -> 	    
	    case (catch link(Pid)) of
		{'EXIT', Reason} ->
		    reply(From, {error, Reason}),
		    loop(Tracer, SurviveLinks, Table);
		_ ->
		    reply(From, {ok, Pid}),
		    loop(Tracer, [Pid | SurviveLinks], Table)
	    end;
	{From, {add_node, Node}} when Node =:= node() ->
	    reply(From,{ok, Node}),
	    loop(Tracer, SurviveLinks, Table);
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
		    exit(done);
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

dhandler(end_of_trace, _Data) ->
    ok;
dhandler(Trace, _Data) when element(1, Trace) == trace, size(Trace) >= 3 ->
    dhandler1(Trace, size(Trace));
dhandler(Trace, _Data) when element(1, Trace) == trace_ts, size(Trace) >= 4 ->
    dhandler1(Trace, size(Trace)-1);
dhandler(Trace, _Data) when element(1, Trace) == drop, size(Trace) == 2 ->
    io:format(user, "*** Dropped ~p messages.~n", [element(2,Trace)]),
    ok;
dhandler(Trace, _Data) when element(1, Trace) == seq_trace, size(Trace) >= 3 ->
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
dhandler(Trace, _Data) ->
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
	return -> %% To be deleted...
	    case element(4, Trace) of
		MFA when Size == 5 ->
		    Ret = element(5, Trace),
		    io:format(user, "(~p) old_ret ~s -> ~p~n", [From,ffunc(MFA),Ret]);
		MFA ->
		    io:format(user, "(~p) old_ret ~s~n", [From,ffunc(MFA)])
	    end;
	return_from ->
	    MFA = element(4, Trace),
	    Ret = element(5, Trace),
	    io:format(user, "(~p) returned from ~s -> ~p~n", [From,ffunc(MFA),Ret]);
	return_to ->
	    MFA = element(4, Trace),
	    io:format(user, "(~p) returning to ~s~n", [From,ffunc(MFA)]);
	spawn when Size == 5 ->
	    Pid = element(4, Trace),
	    MFA = element(5, Trace),
	    io:format(user, "(~p) spawn ~p as ~s~n", [From,Pid,ffunc(MFA)]);
	Op ->
	    io:format(user, "(~p) ~p ~s~n", [From,Op,ftup(Trace,4,Size)])
    end.



%%% These f* functions returns non-flat strings

%% {M,F,[A1, A2, ..., AN]} -> "M:F(A1, A2, ..., AN)"
%% {M,F,A}                 -> "M:F/A"
ffunc({M,F,Argl}) when list(Argl) ->
    io_lib:format("~p:~p(~s)", [M, F, fargs(Argl)]);
ffunc({M,F,Arity}) ->
    io_lib:format("~p:~p/~p", [M,F,Arity]);
ffunc(X) -> io_lib:format("~p", [X]).

%% Integer           -> "Integer"
%% [A1, A2, ..., AN] -> "A1, A2, ..., AN"
fargs(Arity) when integer(Arity) -> integer_to_list(Arity);
fargs([]) -> [];
fargs([A]) -> io_lib:format("~p", [A]);  %% last arg
fargs([A|Args]) -> [io_lib:format("~p,", [A]) | fargs(Args)];
fargs(A) -> io_lib:format("~p", [A]). % last or only arg

%% {A_1, A_2, ..., A_N} -> "A_Index A_Index+1 ... A_Size"
ftup(Trace, Index, Index) -> 
    io_lib:format("~p", [element(Index, Trace)]);
ftup(Trace, Index, Size) -> 
    [io_lib:format("~p ", [element(Index, Trace)]) 
     | ftup(Trace, Index+1, Size)].



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
    [send,'receive',call,procs,garbage_collection,running,
     set_on_spawn,set_on_first_spawn,set_on_link,set_on_first_link,
     timestamp,arity,return_to].

display_info(List) ->
    io:format("~-12s ~-21s Trace ~n", ["Pid", "Initial call"]),
    display_info1(List).

display_info1([Pid|T]) ->
    case pinfo(Pid, initial_call) of
        undefined ->
            display_info1(T);
        {initial_call, Call} ->
	    case tinfo(Pid, flags) of
		undefined ->
		    display_info1(T);
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

%% Process loop that processes a trace. Reads the trace with 
%% the reader Reader, and feeds the trace terms 
%% to handler Handler, keeping a state variable for the 
%% handler.
%%
%% Exits 'normal' at end of trace, other exits due to errors.
%%
%% Reader is a lazy list, i.e either a list or a fun/0. 
%% If it is a fun, it is evaluated for rest of the lazy list.
%% A list head is considered to be a trace term. End of list 
%% is interpreted as end of trace.
tc_loop(Reader, {Handler, HData}) when function(Reader) ->
    tc_loop(Reader(), {Handler, HData});
tc_loop([], {Handler, HData}) ->
    Handler(end_of_trace, HData),
    exit(normal);
tc_loop([Term | Tail], {Handler, HData}) ->
    NewHData = Handler(Term, HData),
    tc_loop(Tail, {Handler, NewHData});
tc_loop(Other, {_Handler, _HData}) ->
    io:format("~p:tc_loop ~p~n", [?MODULE, Other]),
    exit({unknown_term_from_reader, Other}).



%% Returns a reader (lazy list of trace terms) for tc_loop/2.
gen_reader(ip, {Host, Portno}) ->
    case gen_tcp:connect(Host, Portno, [{active, false}, binary]) of
        {ok, Sock} ->    
	    mk_reader(fun ip_read/2, Sock);
	Error ->
	    exit(Error)
    end;
gen_reader(file, {Filename, wrap, Tail, _, WrapCnt}) ->
    mk_reader_wrap(wrap_sort(wrap_presort(Filename, Tail), WrapCnt));
gen_reader(file, Filename) ->
    gen_reader_file(fun file_read/2, Filename);
gen_reader(follow_file, Filename) ->
    gen_reader_file(fun follow_read/2, Filename).

%% Opens a file and returns a reader (lazy list).
gen_reader_file(ReadFun, Filename) ->
    case file:open(Filename, [read, raw, binary]) of
	{ok, File} ->
	    mk_reader(ReadFun, File);
	Error ->
	    exit({client_cannot_open, Error})
    end.

%% Creates and returns a reader (lazy list).
mk_reader(ReadFun, Source) ->
    fun() ->
	    case read_term(ReadFun, Source) of
		{ok, Term} ->
		    [Term | mk_reader(ReadFun, Source)];
		eof ->
		    [] % end_of_trace
	    end
    end.

%% Creates and returns a reader (lazy list) for a wrap log.
%% The argument is a sorted list of sort converted 
%% wrap log file names, see wrap_presort/2.

mk_reader_wrap([]) ->
    [];
mk_reader_wrap([Hd | _] = WrapFiles) ->
    case file:open(wrap_name(Hd), [read, raw, binary]) of
	{ok, File} ->
	    mk_reader_wrap(WrapFiles, File);
	Error ->
	    exit({client_cannot_open, Error})
    end.

mk_reader_wrap([Hd | Tail] = WrapFiles, File) ->
    fun() ->
	    case read_term(fun file_read/2, File) of
		{ok, Term} ->
		    [Term | mk_reader_wrap(WrapFiles, File)];
		eof ->
		    file:close(File),
		    case Tail of
			[_|_] ->
			    mk_reader_wrap(Tail);
			[] ->
			    [] % end_of_trace
		    end
	    end
    end.



%% Generic read term function. 
%% Returns {ok, Term} | 'eof'. Exits on errors.

read_term(ReadFun, Source) ->
    case ReadFun(Source, 5) of
	Bin when binary(Bin) ->
	    read_term(ReadFun, Source, Bin);
	List when list(List) ->
	    read_term(ReadFun, Source, list_to_binary(List));
	eof ->
	    eof
    end.

read_term(ReadFun, Source, <<Op, Size:32>> = Tag) ->
    case Op of
	0 ->
	    case ReadFun(Source, Size) of
		eof ->
		    exit({'trace term missing', 
			  binary_to_list(Tag)});
		Bin when binary(Bin) ->
		    {ok, binary_to_term(Bin)};
		List when list(List) ->
		    {ok, binary_to_term(list_to_binary(List))}
	    end;
	1 ->
	    {ok, {drop, Size}};
	Junk ->
	    exit({'bad trace tag', Junk})
    end.
    


%% Read functions for different source types, for read_term/2.
%%
%% Returns a binary of length N, an I/O-list of 
%% effective length N or 'eof'. Exits on errors.

file_read(File, N) ->
    case file:read(File, N) of
	{ok, Bin} when binary(Bin), size(Bin) =:= N -> 
	    Bin;
	{ok, Bin} when binary(Bin) ->
	    exit({'truncated file', binary_to_list(Bin)});
	eof ->
	    eof;
	{error, Reason} ->
	    exit({'file read error', Reason})
    end.

follow_read(File, N) ->
    follow_read(File, N, cur).

follow_read(File, N, Pos) ->
    case file:position(File, Pos) of
	{ok, Offset} ->
	    case file:read(File, N) of
		{ok, Bin} when binary(Bin), size(Bin) =:= N -> 
		    Bin;
		{ok, Bin} when binary(Bin) ->
		    follow_read(File, N, Offset);
		eof ->
		    follow_read(File, N, Offset);
		{error, Reason} ->
		    exit({'file read error', Reason})
	    end;
	{error, Reason} ->
	    exit({'file position error', Reason})
    end.

ip_read(Socket, N) ->
    case gen_tcp:recv(Socket, N) of
	{ok, Bin} when binary(Bin), size(Bin) < N ->
	    [Bin | ip_read(Socket, N-size(Bin))];
	{ok, Bin} when binary(Bin), size(Bin) == N ->
	    [Bin];
	{ok, Bin} when binary(Bin) ->
	    exit({'socket read too much data', Bin});
	{error, closed} ->
	    eof;
	{error, Reason} = Error ->
	    exit({'socket read error', Error})
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
    BPattern = term_to_binary(Pattern),
    case ets:match_object(PT, {'_', BPattern}) of
	[] ->
	    ets:insert(PT, {Last + 1, BPattern}),
	    Last + 1;
	[{N, BPattern}] ->
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
    [{A,B}] = ets:lookup(T,Key),
    NLd = Fun({A,binary_to_term(B)},Ld),
    pt_doforall(T,Fun,ets:next(T,Key),NLd).

lint_tp(Pattern) ->
    case erlang:match_spec_test([],Pattern,trace) of
	{ok,_Res,Warnings,_Flags} ->
	    {ok, Warnings};
	{error, Reasons} ->
	    {error, Reasons}
    end.

check_list(T) ->
    case (catch lists:foldl(
		  fun(Val,_) ->
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



%% Find all possible wrap log files.
%% Returns: a list of sort converted filenames.
%%
%% The sort conversion is done by extracting the wrap sequence counter
%% from the filename, and calling wrap_encode/2.
wrap_presort(Filename, Tail) ->
    Name = filename:basename(Filename),
    Dirname = filename:dirname(Filename),
    case file:list_dir(Dirname) of
	{ok, Files} ->
	    lists:zf(
	      fun(N) ->
		      case match_front(N, Name) of
			  false ->
			      false;
			  X ->
			      case match_rear(X, Tail) of
				  false ->
				      false;
				  C -> % Counter
				      case match_0_9(C) of
					  true ->
					      {true, 
					       wrap_encode(
						 filename:join(Dirname, N),
						 C)};
					  false ->
					      false
				      end
			      end
		      end
	      end,
	      Files);
	_ ->
	    []
    end.



%% Sorts a list of sort converted files
wrap_sort(Files, N) ->
    wrap_sortfix(lists:sort(Files), N).

%% Finish the sorting, since the lists:sort order is not the correct order.
%% Cut the list of files at the gap (at least one file is supposed
%% to be 'missing') and create a new list by cons'ing the two parts
%% in the right order.
wrap_sortfix([], N) when N >= 1 ->
    [];
wrap_sortfix([], _N) ->
    exit(inconsistent_wrap_file_trace_set);
%% file 0, gap 1..N
wrap_sortfix([{0, _}] = Files, N) when N >= 1 ->
    Files;
wrap_sortfix([{0, _}] = Files, _N) ->
    exit(inconsistent_wrap_file_trace_set);
%% files 0, ...
wrap_sortfix([{0, _} | _] = Files, N) when N >= 1->
    wrap_sortfix_1(Files, N, [], Files);
%% gap 0, files 1, ...
wrap_sortfix([{1, _} | _] = Files, N) when N >= 1 ->
    wrap_sortfix_2(Files, N, [], Files);
wrap_sortfix([{_C, _} | _], _N) ->
    exit(inconsistent_wrap_file_trace_set).

%% files 0..C, gap C+1..N
wrap_sortfix_1([{C, _}], N, _R, Files) 
  when C < N ->
    Files;
%% files 0..C1, C1+1==C2, ...
wrap_sortfix_1([{C1, _} = F1 | [{C2, _} | _] = Tail], N, R, Files) 
  when C1+1 == C2, C2 < N ->
    wrap_sortfix_1(Tail, N, [F1 | R], Files);
%% files 0..C1, gap C1+1, files C1+2==C2, ...
wrap_sortfix_1([{C1, _} = F1 | [{C2, _} | _] = Tail], N, R, _Files) 
  when C1+2 == C2, C2 =< N ->
    wrap_sortfix_2(Tail, N, lists:reverse([F1 | R]), Tail);
wrap_sortfix_1([_F1 | [_F2 | _]], _N, _R, _Files) ->
    exit(inconsistent_wrap_file_trace_set).

%% M == length(R); files 0..M-1, gap M, files M+1..N
wrap_sortfix_2([{N, _}], N, R, Files) ->
    Files ++ R;
wrap_sortfix_2([{_C, _}], _N, _R, _Files) ->
    exit(inconsistent_wrap_file_trace_set);
%% M == length(R); files 0..M-1, gap M, files M+1..C1, C1+1==C2, ...
wrap_sortfix_2([{C1, _} | [{C2, _} | _] = Tail], N, R, Files)
  when C1+1 == C2, C2 =< N ->
    wrap_sortfix_2(Tail, N, R, Files);
wrap_sortfix_2([{_C1, _} | [{_C2, _} | _]], _N, _R, _Files) ->
    exit(inconsistent_wrap_file_trace_set).



%% Extract the filenames from a list of sort converted ones.
wrap_postsort(Files) ->    
    lists:map(fun wrap_name/1, Files).

wrap_encode(N, C) ->
    {list_to_integer(C), N}.

wrap_name({_C, N}) ->
    N.

%% Returns what is left of ListA when removing all matching
%% elements from ListB, or false if some element did not match,
%% or if ListA runs out of elements before ListB.
match_front(ListA, []) when list(ListA) ->
    ListA;
match_front([], ListB) when list(ListB) ->
    false;
match_front([Hd|TlA], [Hd|TlB]) ->
    match_front(TlA,TlB);
match_front([HdA|_], [HdB|_]) ->
    false.

%% Reversed version of match_front/2
match_rear(ListA, ListB) when list(ListA), list(ListB) ->
    case match_front(lists:reverse(ListA), lists:reverse(ListB)) of
	false ->
	    false;
	List ->
	    lists:reverse(List)
    end.

%% Returns true if the non-empty list arguments contains all
%% characters $0 .. $9.
match_0_9([]) ->
    false;
match_0_9([H]) when integer(H), $0 =< H, H =< $9 ->
    true;
match_0_9([H|T] = L) when integer(H), $0 =< H, H =< $9 ->
    match_0_9(T);
match_0_9(L) when list(L) ->
    false.

%%%%%%%%%%%%%%%%%%
%% Help...
%%%%%%%%%%%%%%%%%%

help_display([]) ->
    io:format("~n",[]),
    ok;
help_display([H|T]) ->
    io:format("~s~n",[H]),
    help_display(T).

h() ->
    help_display(
      [
       "The following help items are available:",
       "   p, c",
       "       - Set trace flags for processes",
       "   tp, tpl, ctp, ctpl, ctpg, ltp, dtp, wtp, rtp",
       "       - Manipulate trace patterns for functions",
       "   n, cn, ln",
       "       - Add/remove traced nodes.",
       "   tracer, trace_port, trace_client, get_tracer, stop", 
       "       - Manipulate tracer process/port",
       "   i",
       "       - Info", 
       "",
       "call dbg:h(Item) for brief help a brief description",
       "of one of the items above."]).
h(p) ->
    help_display(["p(Item) -> {ok, MatchDesc} | {error, term()}",
		  " - Traces messages to and from Item.",
		  "p(Item, Flags) -> {ok, MatchDesc} | {error, term()}",
		  " - Traces Item according to Flags.",
		  "   Flags can be one of s,r,m,c,p,sos,sol,sofs,",
		  "   sofl,all,clear or any flag accepted by erlang:trace/3"]);
h(c) ->
    help_display(["c(Mod, Fun, Args)",
		  " - Evaluates apply(M,F,Args) with all trace flags set.",
		  "c(Mod, Fun, Args, Flags)",
		  " - Evaluates apply(M,F,Args) with Flags trace flags set."]);
h(i) ->
    help_display(["i() -> ok",
		  " - Displays information about all traced processes."]);
h(tp) ->
    help_display(
      ["tp(Module,MatchSpec)",
       " - Same as tp({Module, '_', '_'}, MatchSpec)",
       "tp(Module,Function,MatchSpec)",
       " - Same as tp({Module, Function, '_'}, MatchSpec)",
       "tp(Module, Function, Arity, MatchSpec)",
       " - Same as tp({Module, Function, Arity}, MatchSpec)",
       "tp({Module, Function, Arity}, MatchSpec) -> {ok, MatchDesc} "
       "| {error, term()}",
       " - Set pattern for traced global function calls."]);
h(tpl) ->
    help_display(
      ["tpl(Module,MatchSpec)",
       " - Same as tpl({Module, '_', '_'}, MatchSpec)",
       "tpl(Module,Function,MatchSpec)",
       " - Same as tpl({Module, Function, '_'}, MatchSpec)",
       "tpl(Module, Function, Arity, MatchSpec)",
       " - Same as tpl({Module, Function, Arity}, MatchSpec)",
       "tpl({Module, Function, Arity}, MatchSpec) -> {ok, MatchDesc} "
       "| {error, term()}",
       " - Set pattern for traced local (as well as global) function calls."]);
h(ctp) ->
    help_display(
      ["ctp(Module)",
       " - Same as ctp({Module, '_', '_'})",
       "ctp(Module, Function)",
       " - Same as ctp({Module, Function, '_'})",
       "ctp(Module, Function, Arity)",
       " - Same as ctp({Module, Function, Arity})",
       "ctp({Module, Function, Arity}) -> {ok, MatchDesc} | {error, term()}",
       " - Clear call trace pattern for the specified functions"]);
h(ctpl) ->
    help_display(
      ["ctpl(Module)",
       " - Same as ctpl({Module, '_', '_'})",
       "ctpl(Module, Function)",
       " - Same as ctpl({Module, Function, '_'})",
       "ctpl(Module, Function, Arity)",
       " - Same as ctpl({Module, Function, Arity})",
       "ctpl({Module, Function, Arity}) -> {ok, MatchDesc} | {error, term()}",
       " - Clear call trace pattern for the specified functions"]);
h(ctpg) ->
    help_display(
      ["ctpg(Module)",
       " - Same as ctpg({Module, '_', '_'})",
       "ctpg(Module, Function)",
       " - Same as ctpg({Module, Function, '_'})",
       "ctpg(Module, Function, Arity)",
       " - Same as ctpg({Module, Function, Arity})",
       "ctpg({Module, Function, Arity}) -> {ok, MatchDesc} | {error, term()}",
       " - Clear call trace pattern for the specified functions"]);
h(ltp) ->
    help_display(["ltp() -> ok",
		  " - Lists saved match_spec's on the console."]);
h(dtp) ->
    help_display(["dtp() -> ok",
		  " - Deletes all saved match_spec's.",
		  "dtp(N) -> ok",
		  " - Deletes a specific saved match_spec."]);
h(wtp) ->
    help_display(["wtp(Name) -> ok | {error, IOError}",
		  " - Writes all saved match_spec's to a file"]);
h(rtp) ->
    help_display(["rtp(Name) -> ok | {error, Error}",
		  " - Read saved match specifications from file."]);
h(n) ->
    help_display(["n(Nodename) -> {ok, Nodename} | {error, Reason}",
		  " - Adds a node to the list of traced nodes"]);
h(cn) ->
    help_display(["cn(Nodename) -> ok",
		  " - Clears a node from the list of traced nodes."]);
h(ln) ->
    help_display(["ln() -> ok",
		  " - Shows the list of traced nodes on the console."]);
h(tracer) ->
    help_display(["tracer() -> {ok, pid()} | {error, already_started}",
		  " - Starts a tracer server that handles trace messages.",
		  "tracer(Type, Data) -> {ok, pid()} | {error, Error}",
		  " - Starts a tracer server with additional parameters"]);
h(trace_port) ->
    help_display(["trace_port(Type, Parameters) -> fun()",
		  " - Creates and returns a trace port generating fun"]);
h(trace_client) ->
    help_display(["trace_client(Type, Parameters) -> pid()",
		  " - Starts a trace client that reads messages created by "
		  "a trace port driver",
		  "trace_client(Type, Parameters, HandlerSpec) -> pid()",
		  " - Starts a trace client that reads messages created by a",
		  "   trace port driver, with a user defined handler"]);
h(get_tracer) ->
    help_display(
      ["get_tracer() -> {ok, Tracer}",
       " - Returns the process or port to which all trace messages are "
       "sent."]);
h(stop) ->
    help_display(
      ["stop() -> stopped",
       " - Stops the dbg server and the tracing of all processes."]).
