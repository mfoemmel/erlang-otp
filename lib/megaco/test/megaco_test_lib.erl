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
%% Purpose: Lightweight test server
%%----------------------------------------------------------------------

-module(megaco_test_lib).

-compile(export_all).

-include("megaco_test_lib.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Evaluates a test case or test suite
%% Returns a list of failing test cases:
%% 
%%     {Mod, Fun, ExpectedRes, ActualRes}
%%----------------------------------------------------------------------

tickets(Case) ->
    Res = lists:flatten(tickets(Case, default_config())),
    %% io:format("Res: ~p~n", [Res]),
    display_result(Res),
    Res.

tickets(Cases, Config) when list(Cases) ->     
    [tickets(Case, Config) || Case <- Cases];
tickets(Mod, Config) when atom(Mod) ->
    Res = tickets(Mod, tickets, Config),
    Res;
tickets(Bad, _Config) ->
    [{badarg, Bad, ok}].

tickets(Mod, Func, Config) ->
    case (catch Mod:Func(suite)) of
	[] ->
	    io:format("Eval:   ~p:", [{Mod, Func}]),
	    Res = eval(Mod, Func, Config),
	    {R, _, _} = Res,
	    io:format(" ~p~n", [R]),
	    Res;
% 	    io:format("Eval:   ~p:~p~n", [Mod, Func]),
% 	    eval(Mod, Func, Config);
	
	Cases when list(Cases) ->
	    io:format("Expand: ~p:~p ... ~n"
		      "        ~p~n", [Mod, Func, Cases]),
	    Map = fun({M,_}) when atom(M) -> tickets(M, tickets, Config);
		     (F)     when atom(F) -> tickets(Mod, F, Config);
		     (Case) -> Case
		  end,
	    lists:map(Map, Cases);

        {req, _, {conf, Init, Cases, Finish}} ->
	    case (catch Mod:Init(Config)) of
		Conf when list(Conf) ->
		    io:format("Expand: ~p:~p ...~n", [Mod, Func]),
		    Map = fun({M,_}) when atom(M) -> tickets(M, tickets, Config);
			     (F)     when atom(F) -> tickets(Mod, F, Config);
			     (Case) -> Case
			  end,
		    Res = lists:map(Map, Cases),
		    (catch Mod:Finish(Conf)),
		    Res;
		    
		{'EXIT', {skipped, Reason}} ->
		    io:format(" => skipping: ~p~n", [Reason]),
		    [{skipped, {Mod, Func}, Reason}];
		    
		Error ->
		    io:format(" => init failed: ~p~n", [Error]),
		    [{failed, {Mod, Func}, Error}]
	    end;
		    
        {'EXIT', {undef, _}} ->
	    io:format("Undefined:   ~p~n", [{Mod, Func}]),
	    [{nyi, {Mod, Func}, ok}];
		    
        Error ->
	    io:format("Ignoring:   ~p:~p: ~p~n", [Mod, Func, Error]),
	    [{failed, {Mod, Func}, Error}]
    end.
	

t(Case) ->
    process_flag(trap_exit, true),
    Res = lists:flatten(t(Case, default_config())),
    io:format("Res: ~p~n", [Res]),
    display_result(Res),
    Res.

t({Mod, Fun}, Config) when atom(Mod), atom(Fun) ->
    case catch apply(Mod, Fun, [suite]) of
	[] ->
	    io:format("Eval:   ~p:", [{Mod, Fun}]),
	    Res = eval(Mod, Fun, Config),
	    {R, _, _} = Res,
	    io:format(" ~p~n", [R]),
	    Res;

	Cases when list(Cases) ->
	    io:format("Expand: ~p ...~n", [{Mod, Fun}]),
	    Map = fun(Case) when atom(Case)-> {Mod, Case};
		     (Case) -> Case
		  end,
	    t(lists:map(Map, Cases), Config);

        {req, _, {conf, Init, Cases, Finish}} ->
	    case (catch apply(Mod, Init, [Config])) of
		Conf when list(Conf) ->
		    io:format("Expand: ~p ...~n", [{Mod, Fun}]),
		    Map = fun(Case) when atom(Case)-> {Mod, Case};
			     (Case) -> Case
			  end,
		    Res = t(lists:map(Map, Cases), Conf),
		    (catch apply(Mod, Finish, [Conf])),
		    Res;
		    
		{'EXIT', {skipped, Reason}} ->
		    io:format(" => skipping: ~p~n", [Reason]),
		    [{skipped, {Mod, Fun}, Reason}];
		    
		Error ->
		    io:format(" => failed: ~p~n", [Error]),
		    [{failed, {Mod, Fun}, Error}]
	    end;
		    
        {'EXIT', {undef, _}} ->
	    io:format("Undefined:   ~p~n", [{Mod, Fun}]),
	    [{nyi, {Mod, Fun}, ok}];
		    
        Error ->
	    io:format("Ignoring:   ~p: ~p~n", [{Mod, Fun}, Error]),
	    [{failed, {Mod, Fun}, Error}]
    end;
t(Mod, Config) when atom(Mod) ->
    Res = t({Mod, all}, Config),
    Res;
t(Cases, Config) when list(Cases) ->
    [t(Case, Config) || Case <- Cases];
t(Bad, _Config) ->
    [{badarg, Bad, ok}].

eval(Mod, Fun, Config) ->
    TestCase = {?MODULE, Mod, Fun},
    Label = lists:concat(["TEST CASE: ", Fun]),
    megaco:report_event(40, ?MODULE, Mod, Label ++ " started",
			[TestCase, Config]),
    global:register_name(megaco_test_case_sup, self()),
    Flag = process_flag(trap_exit, true),
    put(megaco_test_server, true),
    Config2 = Mod:init_per_testcase(Fun, Config),
    Pid = spawn_link(?MODULE, do_eval, [self(), Mod, Fun, Config2]),
    R = wait_for_evaluator(Pid, Mod, Fun, Config2, []),
    Mod:fin_per_testcase(Fun, Config2),
    erase(megaco_test_server),    
    global:unregister_name(megaco_test_case_sup),
    process_flag(trap_exit, Flag),
    R.

-record('REASON', {mod, line, desc}).

wait_for_evaluator(Pid, Mod, Fun, Config, Errors) ->
    TestCase = {?MODULE, Mod, Fun},
    Label = lists:concat(["TEST CASE: ", Fun]),
    receive
	{done, Pid, ok} when Errors == [] ->
	    megaco:report_event(40, Mod, ?MODULE, Label ++ " ok",
				[TestCase, Config]),
	    {ok, {Mod, Fun}, Errors};
	{done, Pid, ok} ->
	    megaco:report_event(40, Mod, ?MODULE, Label ++ " failed",
				[TestCase, Config]),
	    {failed, {Mod, Fun}, Errors};
	{done, Pid, {ok, _}} when Errors == [] ->
	    megaco:report_event(40, Mod, ?MODULE, Label ++ " ok",
				[TestCase, Config]),
	    {ok, {Mod, Fun}, Errors};
	{done, Pid, {ok, _}} ->
	    megaco:report_event(40, Mod, ?MODULE, Label ++ " failed",
				[TestCase, Config]),
	    {failed, {Mod, Fun}, Errors};
	{done, Pid, Fail} ->
	    megaco:report_event(20, Mod, ?MODULE, Label ++ " failed",
				[TestCase, Config, {return, Fail}, Errors]),
	    {failed, {Mod,Fun}, Fail};
	{'EXIT', Pid, {skipped, Reason}} -> 
	    megaco:report_event(20, Mod, ?MODULE, Label ++ " skipped",
				[TestCase, Config, {skipped, Reason}]),
	    {skipped, {Mod, Fun}, Errors};
	{'EXIT', Pid, Reason} -> 
	    megaco:report_event(20, Mod, ?MODULE, Label ++ " crashed",
				[TestCase, Config, {'EXIT', Reason}]),
	    {crashed, {Mod, Fun}, [{'EXIT', Reason} | Errors]};
	{fail, Pid, Reason} ->
	    wait_for_evaluator(Pid, Mod, Fun, Config, Errors ++ [Reason])
    end.

do_eval(ReplyTo, Mod, Fun, Config) ->
    case (catch apply(Mod, Fun, [Config])) of
	{'EXIT', {skipped, Reason}} ->
	    ReplyTo ! {'EXIT', self(), {skipped, Reason}};
	Other ->
	    ReplyTo ! {done, self(), Other}
    end,
    unlink(ReplyTo),
    exit(shutdown).


display_result([]) ->    
    io:format("OK~n", []);
display_result(Res) when list(Res) ->
    Ok      = [MF || {ok, MF, _}  <- Res],
    Nyi     = [MF || {nyi, MF, _} <- Res],
    Skipped = [{MF, Reason} || {skipped, MF, Reason} <- Res],
    Failed  = [{MF, Reason} || {failed, MF, Reason} <- Res],
    Crashed = [{MF, Reason} || {crashed, MF, Reason} <- Res],
    display_summery(Ok, Nyi, Skipped, Failed, Crashed),
    display_skipped(Skipped),
    display_failed(Failed),
    display_crashed(Crashed).

display_summery(Ok, Nyi, Skipped, Failed, Crashed) ->
    io:format("~nTest case summery:~n", []),
    display_summery(Ok,      "successfull"),
    display_summery(Nyi,     "not yet implemented"),
    display_summery(Skipped, "skipped"),
    display_summery(Failed,  "failed"),
    display_summery(Crashed, "crashed"),
    io:format("~n", []).
   
display_summery(Res, Info) ->
    io:format("  ~w test cases ~s~n", [length(Res), Info]).
    
display_skipped([]) ->
    ok;
display_skipped(Skipped) ->
    io:format("Skipped test cases:~n", []),
    F = fun({MF, Reason}) -> io:format("  ~p => ~p~n", [MF, Reason]) end,
    lists:foreach(F, Skipped),
    io:format("~n", []).
    

display_failed([]) ->
    ok;
display_failed(Failed) ->
    io:format("Failed test cases:~n", []),
    F = fun({MF, Reason}) -> io:format("  ~p => ~p~n", [MF, Reason]) end,
    lists:foreach(F, Failed),
    io:format("~n", []).

display_crashed([]) ->
    ok;
display_crashed(Crashed) ->
    io:format("Crashed test cases:~n", []),
    F = fun({MF, Reason}) -> io:format("  ~p => ~p~n", [MF, Reason]) end,
    lists:foreach(F, Crashed),
    io:format("~n", []).
        
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Verify that the actual result of a test case matches the exected one
%% Returns the actual result
%% Stores the result in the process dictionary if mismatch

error(Actual, Mod, Line) ->
    global:send(megaco_global_logger, {failed, Mod, Line}),
    log("<ERROR> Bad result: ~p~n", [Actual], Mod, Line),
    Label = lists:concat([Mod, "(", Line, ") unexpected result"]),
    megaco:report_event(60, Mod, Mod, Label,
			[{line, Mod, Line}, {error, Actual}]),
    case global:whereis_name(megaco_test_case_sup) of
	undefined -> 
	    ignore;
	Pid -> 
	    Fail = #'REASON'{mod = Mod, line = Line, desc = Actual},
	    Pid ! {fail, self(), Fail}
    end,
    Actual.

log(Format, Args, Mod, Line) ->
    case global:whereis_name(megaco_global_logger) of
	undefined ->
	    io:format(user, "~p~p(~p): " ++ Format, 
		      [self(), Mod, Line] ++ Args);
	Pid ->
	    io:format(Pid, "~p~p(~p): " ++ Format, 
		      [self(), Mod, Line] ++ Args)
    end.

skip(Actual, File, Line) ->
    log("Skipping test case~n", [], File, Line),
    String = lists:flatten(io_lib:format("Skipping test case ~p(~p): ~p~n",
					 [File, Line, Actual])),
    exit({skipped, String}).

fatal_skip(Actual, File, Line) ->
    error(Actual, File, Line),
    exit(shutdown).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Flush the message queue and return its messages
flush() ->
    receive
	Msg ->
	    [Msg | flush()]
    after 1000 ->
	    []
    end.
	    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Check if process is alive and kicking
still_alive(Pid) ->   
    case catch erlang:is_process_alive(Pid) of % New BIF in Erlang/OTP R5
	true -> 
	    true;
	false -> 
	    false;
	{'EXIT', _} -> % Pre R5 backward compatibility 
	    case process_info(Pid, message_queue_len) of
		undefined -> false;
		_ -> true
	    end 
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The proxy process

proxy_start(ProxyId) ->
    spawn_link(?MODULE, proxy_init, [ProxyId, self()]).

proxy_start(Node, ProxyId) ->
    spawn_link(Node, ?MODULE, proxy_init, [ProxyId, self()]).

proxy_init(ProxyId, Controller) ->
    process_flag(trap_exit, true),
    ?LOG("[~p] proxy started by ~p~n",[ProxyId, Controller]),
    proxy_loop(ProxyId, Controller).

proxy_loop(OwnId, Controller) ->
    receive
	{'EXIT', Controller, Reason} ->
	    p("proxy_loop -> received exit from controller"
	      "~n   Reason: ~p"
	      "~n", [Reason]),
	    exit(Reason);
	{apply, Fun} ->
	    p("proxy_loop -> received apply request~n", []),
	    Res = Fun(),
	    p("proxy_loop -> apply result: "
	      "~n   ~p"
	      "~n", [Res]),
	    Controller ! {res, OwnId, Res},
	    proxy_loop(OwnId, Controller);
	OtherMsg ->
	    p("proxy_loop -> received unknown message: "
	      "~n  OtherMsg: ~p"
	      "~n", [OtherMsg]),
	    Controller ! {msg, OwnId, OtherMsg},
	    proxy_loop(OwnId, Controller)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test server callbacks
init_per_testcase(_Case, Config) ->
    Pid = group_leader(),
    Name = megaco_global_logger,
    case global:whereis_name(Name) of
	undefined ->
	    global:register_name(megaco_global_logger, Pid);
	Pid ->
	    io:format("init_per_testcase -> "
		      "already registered to ~p~n", [Pid]),
	    ok;
	OtherPid when pid(OtherPid) ->
	    io:format("init_per_testcase -> "
		      "already registered to other ~p (~p)~n", [OtherPid,Pid]),
	    exit({already_registered, {megaco_global_logger, OtherPid, Pid}})
    end,
    set_kill_timer(Config).

fin_per_testcase(_Case, Config) ->
    Name = megaco_global_logger,
    case global:whereis_name(Name) of
	undefined ->
	    io:format("fin_per_testcase -> already un-registered~n", []),
	    ok;
	Pid when pid(Pid) ->
	    global:unregister_name(megaco_global_logger),
	    ok
    end,
    reset_kill_timer(Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Set kill timer

set_kill_timer(Config) ->
    case init:get_argument(megaco_test_timeout) of
	{ok, _} -> 
	    Config;
	_ ->
	    Time = 
		case lookup_config(tc_timeout, Config) of
		    [] ->
			timer:minutes(5);
		    ConfigTime when integer(ConfigTime) ->
			ConfigTime
		end,
	    Dog = 
		case get(megaco_test_server) of
		    true ->
			spawn_link(?MODULE, watchdog, [self(), Time]);
		    _ ->
			test_server:timetrap(Time)
		end,
	    [{kill_timer, Dog}|Config]
		    
	    
    end.

reset_kill_timer(Config) ->
    DogKiller = 
	case get(megaco_test_server) of
	    true ->
		fun(P) when is_pid(P) -> P ! stop;
		   (_) -> ok 
		end;
	    _ ->
		fun(Ref) -> test_server:timetrap_cancel(Ref) end
	end,
    case lists:keysearch(kill_timer, 1, Config) of
	{value, {kill_timer, Dog}} ->
	    DogKiller(Dog), 
	    lists:keydelete(kill_timer, 1, Config);
	_ ->
	    Config
    end.

watchdog(Pid, Time) ->
    erlang:now(),
    receive
	stop ->
	    ok
    after Time ->
	    case process_info(Pid) of
		undefined ->
		    ok;
		_ ->
		    ?LOG("<ERROR> Watchdog in test case timed out "
			"for ~p after ~p min~n",
		    [Pid, Time div (1000*60)]),
		    exit(Pid, kill)
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prepare_test_case(Actions, N, Config, File, Line) ->
    OrigNodes = lookup_config(nodes, Config),
    TestNodes = lookup_config(nodenames, Config), %% For testserver
    This = node(),
    SomeNodes = OrigNodes ++ (TestNodes -- OrigNodes),
    AllNodes = [This | (SomeNodes -- [This])],
    Nodes = pick_n_nodes(N, AllNodes, File, Line),
    start_nodes(Nodes, File, Line),
    do_prepare_test_case(Actions, Nodes, Config, File, Line).

do_prepare_test_case([init | Actions], Nodes, Config, File, Line) ->
    process_flag(trap_exit, true),
    megaco_test_lib:flush(),
    do_prepare_test_case(Actions, Nodes, Config, File, Line);
do_prepare_test_case([{stop_app, App} | Actions], Nodes, Config, File, Line) ->
    _Res = rpc:multicall(Nodes, application, stop, [App]),
    do_prepare_test_case(Actions, Nodes, Config, File, Line);
do_prepare_test_case([], Nodes, _Config, _File, _Line) ->
    Nodes.

pick_n_nodes(all, AllNodes, _File, _Line) ->
    AllNodes;
pick_n_nodes(N, AllNodes, _File, _Line) 
  when integer(N), length(AllNodes) >= N ->
    AllNodes -- lists:nthtail(N, AllNodes);
pick_n_nodes(N, AllNodes, File, Line) ->
    fatal_skip({too_few_nodes, N, AllNodes}, File, Line).
   
lookup_config(Key,Config) ->
    case lists:keysearch(Key, 1, Config) of
	{value,{Key,Val}} ->
	    Val;
	_ ->
	    []
    end.

default_config() ->
    [{nodes, default_nodes()}].

default_nodes() ->    
    mk_nodes(2, []).

mk_nodes(0, Nodes) ->
    Nodes;
mk_nodes(N, []) ->
    mk_nodes(N - 1, [node()]);
mk_nodes(N, Nodes) when N > 0 ->
    Head = hd(Nodes),
    [Name, Host] = node_to_name_and_host(Head),
    Nodes ++ [mk_node(I, Name, Host) || I <- lists:seq(1, N)].

mk_node(N, Name, Host) ->
    list_to_atom(lists:concat([Name ++ integer_to_list(N) ++ "@" ++ Host])).
    
%% Returns [Name, Host]    
node_to_name_and_host(Node) ->
    string:tokens(atom_to_list(Node), [$@]).

start_nodes([Node | Nodes], File, Line) ->
    case net_adm:ping(Node) of
	pong ->
	    start_nodes(Nodes, File, Line);
	pang ->
	    [Name, Host] = node_to_name_and_host(Node),
	    case slave:start_link(Host, Name) of
		{ok, NewNode} when NewNode == Node ->
		    Path = code:get_path(),
		    {ok, Cwd} = file:get_cwd(),
		    true = rpc:call(Node, code, set_path, [Path]),
		    ok = rpc:call(Node, file, set_cwd, [Cwd]),
		    true = rpc:call(Node, code, set_path, [Path]),
		    {_, []} = rpc:multicall(global, sync, []),
		    start_nodes(Nodes, File, Line);
		Other ->
		    fatal_skip({cannot_start_node, Node, Other}, File, Line)
	    end
    end;
start_nodes([], _File, _Line) ->
    ok.

p(F,A) ->
    io:format("~p" ++ F ++ "~n", [self()|A]).
