%%<copyright>
%% <year>2003-2007</year>
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
%% Purpose: Lightweight test server
%%----------------------------------------------------------------------

-module(snmp_test_server).

-compile(export_all).

-include("snmp_test_lib.hrl").

-define(d(F,A),d(F,A,?LINE)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Evaluates a test case or test suite
%% Returns a list of failing test cases:
%% 
%%     {Mod, Fun, ExpectedRes, ActualRes}
%%----------------------------------------------------------------------

t(Case) ->
    io:format("~n", []),
    ?d("t(~p) -> entry", [Case]),
    Res = t(Case, default_config()),
    display_result(Res),
    Res;
t([Mod, Fun]) when atom(Mod), atom(Fun) ->
    ?d("t([~w,~w]:1) -> entry", [Mod, Fun]),
    Res = t({Mod, Fun}, default_config()),
    display_result(Res),
    ?d("t(~p,~p) -> Res:~n~p", [Mod, Fun, Res]),
    Res.
    

t({Mod, Fun}, Config) when atom(Mod), atom(Fun), list(Config) ->
    ?d("t(~p,~p) -> entry", [Mod, Fun]),
    case (catch apply(Mod, Fun, [suite])) of
	[] ->
	    io:format("~n~n*** Eval: ~p ***************~n", 
		      [{Mod, Fun}]),
	    case eval(Mod, Fun, Config) of
		{ok, _, _} ->
		    [];
		Other ->
		    [Other]
	    end;

	Cases when list(Cases) ->
	    io:format("~n*** Expand: ~p ...~n", [{Mod, Fun}]),
	    Map = fun(Case) when atom(Case)-> {Mod, Case};
		     (Case) -> Case
		  end,
	    t(lists:map(Map, Cases), Config);

        {req, _, SubCases} when list(SubCases) ->
	    do_subcases(Mod, Fun, SubCases, Config, []);
                    
        {req, _, Conf} ->
	    do_subcases(Mod, Fun, [Conf], Config, []);
                    
        {'EXIT', {undef, _}} ->
            io:format("~n*** Undefined:   ~p~n", [{Mod, Fun}]),
            [{nyi, {Mod, Fun}, ok}];
                    
        Error ->
            io:format("~n*** Ignoring:   ~p: ~p~n", [{Mod, Fun}, Error]),
            [{failed, {Mod, Fun}, Error}]
    end;
t(Mod, Config) when atom(Mod), list(Config) ->
    ?d("t(~p) -> entry whith"
	"~n   Config: ~p", [Mod, Config]),
    t({Mod, all}, Config);
t(Cases, Config) when list(Cases), list(Config) ->
    ?d("t -> entry whith"
	"~n   Cases:  ~p"
	"~n   Config: ~p", [Cases, Config]),
    Errors = [t(Case, Config) || Case <- Cases],
    ?d("t -> Errors: ~n~p", [Errors]),
    lists:append(Errors);
t(Bad, Config) ->
    ?d("t -> entry with"
	"~n   Bad:    ~p"
	"~n   Config: ~p", [Bad, Config]),
    [{badarg, Bad, ok}].


do_case(M,F,C) ->
    io:format("~n~n*** Eval: ~p ***************~n", [{M, F}]),
    case eval(M, F, C) of
	{ok, _, _} ->
	    [];
	Other ->
	    [Other]
    end.
    

do_subcases(_Mod, _Fun, [], _Config, Acc) ->
    ?d("t -> do_subcases([]) -> entry with"
	"~n   Acc: ~p", [Acc]),
    lists:flatten(lists:reverse(Acc));
do_subcases(Mod, Fun, [{conf, Init, Cases, Finish}|SubCases], Config, Acc) ->
    ?d("t -> do_subcases(conf) -> entry with"
	"~n   Init:   ~p"
	"~n   Cases:  ~p"
	"~n   Finish: ~p", [Init, Cases, Finish]),
    R = case (catch apply(Mod, Init, [Config])) of
	    Conf when list(Conf) ->
		io:format("~n*** Expand: ~p ...~n", [{Mod, Fun}]),
		Map = fun(Case) when atom(Case) -> {Mod, Case};
			 (Case) -> Case
		      end,
		Res = t(lists:map(Map, Cases), Conf),
		(catch apply(Mod, Finish, [Conf])),
		Res;
	    
	    {'EXIT', {skipped, Reason}} ->
		io:format(" => skipping: ~p~n", [Reason]),
		[{skipped, {Mod, Fun}, Reason}];
	    
	    Error ->
		io:format(" => init (~p) failed: ~n~p~n", [Init, Error]),
		(catch apply(Mod, Finish, [Config])),
		[{failed, {Mod, Fun}, Error}]
	end,
    ?d("t -> do_subcases(conf):"
	"~n   R: ~p", [R]),
    do_subcases(Mod, Fun, SubCases, Config, [R|Acc]);
do_subcases(Mod, Fun, [SubCase|SubCases], Config, Acc) when atom(SubCase) ->
    ?d("t -> do_subcases(~p)", [SubCase]),
    R = do_case(Mod, SubCase, Config),
    do_subcases(Mod, Fun, SubCases,Config, [R|Acc]).



eval(Mod, Fun, Config) ->
    ?d("eval -> entry with"
	"~n   Mod: ~p"
	"~n   Fun: ~p", [Mod, Fun]),
    global:register_name(inets_test_case_sup, self()),
    Flag = process_flag(trap_exit, true),
    Config2 = Mod:init_per_testcase(Fun, Config),
    Pid = spawn_link(?MODULE, do_eval, [self(), Mod, Fun, Config2]),
    R = wait_for_evaluator(Pid, Mod, Fun, Config2, []),
    Mod:fin_per_testcase(Fun, Config2),
    global:unregister_name(inets_test_case_sup),
    process_flag(trap_exit, Flag),
    ?d("eval -> exit with:"
	"~n   R: ~p", [R]),
    R.

wait_for_evaluator(Pid, Mod, Fun, Config, Errors) ->
    Pre = lists:concat(["TEST CASE: ", Fun]),
    receive
	{'EXIT', _Watchdog, watchdog_timeout} ->
	    io:format("*** ~s WATCHDOG TIMEOUT~n", [Pre]), 
	    exit(Pid, kill),
	    {failed, {Mod,Fun}, watchdog_timeout};
	{done, Pid, ok} when Errors == [] ->
	    io:format("*** ~s OK~n", [Pre]),
	    {ok, {Mod, Fun}, Errors};
	{done, Pid, {ok, _}} when Errors == [] ->
	    io:format("*** ~s OK~n", [Pre]),
	    {ok, {Mod, Fun}, Errors};
	{done, Pid, Fail} ->
	    io:format("*** ~s FAILED~n~p~n", [Pre, Fail]),
	    {failed, {Mod,Fun}, Fail};
	{'EXIT', Pid, {skipped, Reason}} -> 
	    io:format("*** ~s SKIPPED~n~p~n", [Pre, Reason]),
	    {skipped, {Mod, Fun}, Errors};
	{'EXIT', Pid, Reason} -> 
	    io:format("*** ~s CRASHED~n~p~n", [Pre, Reason]),
	    {crashed, {Mod, Fun}, [{'EXIT', Reason} | Errors]};
	{fail, Pid, Reason} ->
	    io:format("*** ~s FAILED~n~p~n", [Pre, Reason]),
	    wait_for_evaluator(Pid, Mod, Fun, Config, Errors ++ [Reason])
   end.

do_eval(ReplyTo, Mod, Fun, Config) ->
    ?d("do_eval -> entry with"
	"~n   ReplyTo: ~p"
	"~n   Mod:     ~p"
	"~n   Fun:     ~p"
	"~n   Config:  ~p"
	"~nat"
	"~n   ~p", [ReplyTo, Mod, Fun, Config, erlang:now()]),
    case (catch apply(Mod, Fun, [Config])) of
	{'EXIT', {skipped, Reason}} ->
	    ReplyTo ! {'EXIT', self(), {skipped, Reason}};
	Other ->
 	    ?d("do_eval -> entry with"
		"~n   Other: ~p", [Other]),
	    ReplyTo ! {done, self(), Other}
    end,
    ?d("do_eval -> case ~p completed at ~p", [Fun, erlang:now()]),
    unlink(ReplyTo),
    exit(shutdown).


display_result([]) ->    
    io:format("TEST OK~n", []);

display_result(Errors) when list(Errors) ->
    Nyi     = [MF || {nyi, MF, _} <- Errors],
    Skipped = [{MF, Reason} || {skipped, MF, Reason} <- Errors],
    Crashed = [{MF, Reason} || {crashed, MF, Reason} <- Errors],
    Failed  = [{MF, Reason} || {failed,  MF, Reason} <- Errors],
    display_skipped(Skipped),
    display_crashed(Crashed),
    display_failed(Failed),
    display_summery(Nyi, Skipped, Crashed, Failed).

display_summery(Nyi, Skipped, Crashed, Failed) ->
    io:format("~nTest case summery:~n", []),
    display_summery(Nyi, "not yet implemented"),
    display_summery(Skipped, "skipped"),
    display_summery(Crashed, "crashed"),
    display_summery(Failed, "failed"),
    io:format("~n", []).
   
display_summery([], _) ->
    ok;
display_summery(Res, Info) ->
    io:format("  ~w test cases ~s~n", [length(Res), Info]).
    
display_skipped([]) ->
    io:format("Skipped test cases: -~n", []);
display_skipped(Skipped) ->
    io:format("Skipped test cases:~n", []),
    [io:format("  ~p => ~p~n", [MF, Reason]) || {MF, Reason} <- Skipped],
    io:format("~n", []).
    
display_crashed([]) ->
    io:format("Crashed test cases: -~n", []);
display_crashed(Crashed) ->
    io:format("Crashed test cases:~n", []),
    [io:format("  ~p => ~p~n", [MF, Reason]) || {MF, Reason} <- Crashed],
    io:format("~n", []).
    
display_failed([]) ->
    io:format("Failed test cases: -~n", []);
display_failed(Failed) ->
    io:format("Failed test cases:~n", []),
    [io:format("  ~p => ~p~n", [MF, Reason]) || {MF, Reason} <- Failed],
    io:format("~n", []).
        
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Verify that the actual result of a test case matches the exected one
%% Returns the actual result
%% Stores the result in the process dictionary if mismatch

error(Actual, Mod, Line) ->
    global:send(inets_global_logger, {failed, Mod, Line}),
    log("<ERROR> Bad result: ~p~n", [Actual], Mod, Line),
    case global:whereis_name(inets_test_case_sup) of
	undefined -> 
	    ignore;
	Pid -> 
	    Pid ! {fail, self(), {Actual, Mod, Line}}
    end,
    Actual.

skip(Actual, Mod, Line) ->
    log("Skipping test case~n", [], Mod, Line),
    exit({skipped, {Actual, Mod, Line}}).

fatal_skip(Actual, Mod, Line) ->
    ?d("fatal_skip -> entry with"
      "~n   Actual: ~p"
      "~n   Mod:    ~p"
      "~n   Line:  ~p", [Actual, Mod, Line]),
    error(Actual, Mod, Line),
    
    exit(shutdown).


log(Format, Args, Mod, Line) ->
    case global:whereis_name(inets_global_logger) of
	undefined ->
	    io:format(user, "~p(~p): " ++ Format, [Mod, Line] ++ Args);
	Pid ->
	    io:format(Pid, "~p(~p): " ++ Format, [Mod, Line] ++ Args)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test server callbacks

init_per_testcase(Case, Config) ->
    ?d("init_per_testcase(~p) -> entry", [Case]),
    global:register_name(megaco_global_logger, group_leader()),
    Config.

fin_per_testcase(Case, Config) ->
    ?d("fin_per_testcase(~p) -> entry", [Case]),
    global:unregister_name(megaco_global_logger),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Internal utility functions

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
	    Args = [],
	    
	    case ?STARTL_NODE(Host, Name, Args) of
		{ok, NewNode} when NewNode == Node ->
		    start_nodes(Nodes, File, Line);
		Other ->
		    fatal_skip({cannot_start_node, Node, Other}, File, Line)
	    end
    end;
start_nodes([], File, Line) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

d(F, A, L) ->
    %% d(true, F, A, L).
    d(get(dbg), F, A, L).

d(true, F, A, L) ->
    io:format("STS:~p:~p " ++ F ++ "~n", [self(),L|A]);
d(_, _, _, _) ->
    ok.

