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
%% Purpose: Verify the application specifics of the Megaco application
%%----------------------------------------------------------------------
-module(megaco_load_test).

-compile(export_all).

-include("megaco_test_lib.hrl").
-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").

-define(TEST_VERBOSITY, debug).
-define(MGC_VERBOSITY,  silence).
-define(MG_VERBOSITY,   silence).

-define(SINGLE_USER_LOAD_NUM_REQUESTS, 1000).
-define(MULTI_USER_LOAD_NUM_REQUESTS,  1000).

-define(MGC_START(Pid, Mid, ET, Verb), 
        megaco_test_mgc:start(Pid, Mid, ET, Verb)).
-define(MGC_STOP(Pid), megaco_test_mgc:stop(Pid)).

-define(MG_START(Pid, Mid, Enc, Transp, Conf, Verb), 
        megaco_test_mg:start(Pid, Mid, Enc, Transp, Conf, Verb)).
-define(MG_STOP(Pid), megaco_test_mg:stop(Pid)).
-define(MG_USER_INFO(Pid,Tag), megaco_test_mg:user_info(Pid,Tag)).
-define(MG_CONN_INFO(Pid,Tag), megaco_test_mg:conn_info(Pid,Tag)).
-define(MG_SERV_CHANGE(Pid), megaco_test_mg:service_change(Pid)).
-define(MG_MLOAD(Pid, NL, NR), 
	timer:tc(megaco_test_mg, apply_multi_load, [Pid, NL, NR])).
-define(MG_LOAD(Pid, NL, NR), megaco_test_mg:apply_multi_load(Pid, NL, NR)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


t()     -> megaco_test_lib:t(?MODULE).
t(Case) -> megaco_test_lib:t({?MODULE, Case}).


%% Test server callbacks
init_per_testcase(single_user_heavy_load = Case, Config) ->
    process_flag(trap_exit, true),
    C = lists:keydelete(tc_timeout, 1, Config),
    megaco_test_lib:init_per_testcase(Case, [{tc_timeout,timer:minutes(10)}|C]);
init_per_testcase(single_user_extreme_load = Case, Config) ->
    process_flag(trap_exit, true),
    C = lists:keydelete(tc_timeout, 1, Config),
    megaco_test_lib:init_per_testcase(Case, [{tc_timeout,timer:minutes(20)}|C]);
init_per_testcase(Case, Config) ->
    process_flag(trap_exit, true),
    megaco_test_lib:init_per_testcase(Case, Config).

fin_per_testcase(Case, Config) ->
    process_flag(trap_exit, false),
    megaco_test_lib:fin_per_testcase(Case, Config).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all(suite) ->
    Cases = 
	[
	 single_user_light_load,
	 single_user_medium_load,
	 single_user_heavy_load,
	 single_user_extreme_load,
	 multi_user_light_load,
	 multi_user_medium_load,
	 multi_user_heavy_load,
	 multi_user_extreme_load
	].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_user_light_load(suite) ->
    [];
single_user_light_load(doc) ->
    [];
single_user_light_load(Config) when list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(tc,        single_user_light_load),
    put(sname,     "TEST"),
    i("starting"),

    single_user_load(5),

    i("done", []),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_user_medium_load(suite) ->
    [];
single_user_medium_load(doc) ->
    [];
single_user_medium_load(Config) when list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(tc,        single_user_medium_load),
    put(sname,     "TEST"),
    i("starting"),

    single_user_load(15),

    i("done", []),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_user_heavy_load(suite) ->
    [];
single_user_heavy_load(doc) ->
    [];
single_user_heavy_load(Config) when list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(tc,        single_user_heavy_load),
    put(sname,     "TEST"),
    i("starting"),

    single_user_load(25),

    i("done", []),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_user_extreme_load(suite) ->
    [];
single_user_extreme_load(doc) ->
    [];
single_user_extreme_load(Config) when list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(tc,        single_user_extreme_load),
    put(sname,     "TEST"),
    i("starting"),

    single_user_load(100),

    i("done", []),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_user_light_load(suite) ->
    [];
multi_user_light_load(doc) ->
    [];
multi_user_light_load(Config) when list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(tc,        multi_user_light_load),
    put(sname,     "TEST"),
    i("starting"),

    multi_user_load(3,1),

    i("done", []),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_user_medium_load(suite) ->
    [];
multi_user_medium_load(doc) ->
    [];
multi_user_medium_load(Config) when list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(tc,        multi_user_medium_load),
    put(sname,     "TEST"),
    i("starting"),

    multi_user_load(3,5),

    i("done", []),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_user_heavy_load(suite) ->
    [];
multi_user_heavy_load(doc) ->
    [];
multi_user_heavy_load(Config) when list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(tc,        multi_user_heavy_load),
    put(sname,     "TEST"),
    i("starting"),

    multi_user_load(3,10),

    i("done", []),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_user_extreme_load(suite) ->
    [];
multi_user_extreme_load(doc) ->
    [];
multi_user_extreme_load(Config) when list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(tc,        multi_user_extreme_load),
    put(sname,     "TEST"),
    i("starting"),

    multi_user_load(3,15),

    i("done", []),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_user_load(NumLoaders) ->
    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("Nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p", [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    %% Start the MGC and MGs
    i("[MGC] start"),
    MgcMid = {deviceName, "ctrl"},
    ET     = [{text,tcp}],
    {ok, Mgc} = ?MGC_START(MgcNode, MgcMid, ET, ?MGC_VERBOSITY),

    i("[MG] start"),
    MgMid = {deviceName, "mg"},
    {ok, Mg} = ?MG_START(MgNode, MgMid, text, tcp, [], ?MG_VERBOSITY),

    d("MG user info: ~p", [?MG_USER_INFO(Mg, all)]),

    i("[MG] connect to the MGC (service change)"),    
    ServChRes = ?MG_SERV_CHANGE(Mg),
    d("service change result: ~p", [ServChRes]),

    d("MG conn info: ~p", [?MG_CONN_INFO(Mg, all)]),

    d("apply the load"),
    Res = ?MG_MLOAD(Mg, NumLoaders, ?SINGLE_USER_LOAD_NUM_REQUESTS),
    case Res of
	{Time, {ok, Ok, Err}} ->
	    Sec = Time / 1000000,
	    io:format("~nmultiple loaders result: ~n"
		      "   Number of successfull: ~w~n"
		      "   Number of failure:     ~w~n"
		      "   Time:                  ~w seconds~n"
		      "   Calls / seconds        ~w~n~n", 
		      [Ok, Err, Sec, (NumLoaders * ?SINGLE_USER_LOAD_NUM_REQUESTS)/Sec]);
	{Time, Error} ->
	    io:format("SUL: multiple loaders failed: ~p after ~w~n", 
		      [Error, Time])
    end,

    %% Tell MG to stop
    i("[MG] stop"),
    ?MG_STOP(Mg),

    %% Tell Mgc to stop
    i("[MGC] stop"),
    ?MGC_STOP(Mgc),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_user_load(NumUsers, NumLoaders) 
  when integer(NumUsers), NumUsers > 1, 
       integer(NumLoaders), NumLoaders >= 1 ->
    MgcNode = make_node_name(mgc),
    MgNodes = make_node_names(mg, NumUsers),
    d("Nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNodes: ~p", [MgcNode, MgNodes]),
    ok = megaco_test_lib:start_nodes([MgcNode| MgNodes], ?FILE, ?LINE),

    %% Start the MGC and MGs
    i("[MGC] start"),
    MgcMid = {deviceName, "ctrl"},
    ET     = [{text,tcp}],
    {ok, Mgc} = ?MGC_START(MgcNode, MgcMid, ET, ?MGC_VERBOSITY),

    MgUsers = make_mids(MgNodes),

    d("start MGs, apply the load and stop MGs"),
    ok = multi_load(MgUsers, NumLoaders, ?MULTI_USER_LOAD_NUM_REQUESTS),

    %% Tell Mgc to stop
    i("[MGC] stop"),
    ?MGC_STOP(Mgc),
    ok.


multi_load(MGs, NumLoaders, NumReqs) ->
    d("multi_load -> entry with"
      "~n   MGs:        ~p"
      "~n   NumLoaders: ~p"
      "~n   NumReqs:    ~p", [MGs, NumLoaders, NumReqs]),

    Pids = multi_load_collector_start(MGs, NumLoaders, NumReqs, []),
    case timer:tc(?MODULE, do_multi_load, [Pids, NumLoaders, NumReqs]) of
	{Time, {ok, OKs, []}} ->
	    Sec = Time / 1000000,
	    multi_load_collector_calc(Sec, OKs);
	{Time, Error} ->
	    Sec = Time/1000000,
	    io:format("~nmulti load failed after ~.1f:~n~p~n~n", [Sec,Error]),
	    {error, Error}
    end.

do_multi_load(Pids, NumLoaders, NumReqs) ->
    Fun = fun(P) -> P ! {apply_multi_load, self()} end,
    lists:foreach(Fun, Pids),
    await_multi_load_collectors(Pids, [], []).

multi_load_collector_start([], _NumLoaders, _NumReqs, Pids) ->
    Pids;
multi_load_collector_start([{Mid, Node}|MGs], NumLoaders, NumReqs, Pids) ->
    Pid = spawn_link(?MODULE, multi_load_collector, 
		     [self(), Node, Mid, NumLoaders, NumReqs]),
    multi_load_collector_start(MGs, NumLoaders, NumReqs, [Pid|Pids]).


multi_load_collector(Parent, Node, Mid, NumLoaders, NumReqs) ->
    case ?MG_START(Node, Mid, text, tcp, [], ?MG_VERBOSITY) of
	{ok, Pid} ->
	    d("MG ~p user info: ~n~p", [Mid, ?MG_USER_INFO(Pid,all)]),
	    ServChRes = ?MG_SERV_CHANGE(Pid),
	    d("service change result: ~p", [ServChRes]),
	    d("MG ~p conn info: ~p", [Mid, ?MG_CONN_INFO(Pid,all)]),
	    multi_load_collector_loop(Parent, Pid, Mid, NumLoaders, NumReqs);
	Else ->
	    Parent ! {load_start_failed, self(), Mid, Else}
    end.

multi_load_collector_loop(Perent, Pid, Mid, NumLoaders, NumReqs) ->
    receive
	{apply_multi_load, Parent} ->
	    Res = ?MG_LOAD(Pid, NumLoaders, NumReqs),
	    Parent ! {load_complete, self(), Mid, Res},
	    ?MG_STOP(Pid),
	    exit(normal)
    end.    
    

await_multi_load_collectors([], Oks, Errs) ->
    {ok, Oks, Errs};
await_multi_load_collectors(Pids, Oks, Errs) ->
    receive
	{load_complete, Pid, Mg, {ok, Ok, Err}} ->
	    Pids2 = lists:delete(Pid, Pids),
	    Oks2  = [{Mg, Ok, Err}|Oks],
	    await_multi_load_collectors(Pids2, Oks2, Errs);
	{load_complete, Pid, Mg, Error} ->
	    Pids2 = lists:delete(Pid, Pids),
	    Errs2 = [{Mg, Error}|Errs],
	    await_multi_load_collectors(Pids2, Oks, Errs2);

	Else ->
	    i("await_multi_load_collectors -> received unexpected message:"
	      "~n~p", [Else]),
	    await_multi_load_collectors(Pids, Oks, Errs)
    after 
	300000 ->
	    %% Cleanup?
	    {error, {timeout, Pids, Oks, Errs}}
    end.
	    
		
%% Note that this is an approximation...we run all the
%% MGs in parrallel, so it should be "accurate"...
multi_load_collector_calc(Sec, Oks) ->
    Succs = lists:sum([Ok   || {_, Ok,   _} <- Oks]),
    Fails = lists:sum([Err  || {_,  _, Err} <- Oks]),
    io:format("~ntotal multiple loaders result: ~n"
	      "   Number of successfull: ~w~n"
	      "   Number of failure:     ~w~n"
	      "   Total Calls / seconds: ~.2f~n~n", 
	      [Succs, Fails, Sec]),
    ok.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_node_names(Name, Num) ->
    make_node_names(Name, Num, []).

make_node_names(_, 0, Names) ->
    Names;
make_node_names(BaseName, N, Names) ->
    Name = lists:flatten(io_lib:format("~p~w", [BaseName,N])),
    make_node_names(BaseName, N-1, [make_node_name(Name)|Names]).

make_node_name(Name) when atom(Name) ->
    make_node_name(atom_to_list(Name));
make_node_name(Name) when list(Name) ->
    case string:tokens(atom_to_list(node()), [$@]) of
	[_,Host] ->
	    list_to_atom(lists:concat([Name ++ "@" ++ Host]));
	_ ->
	    exit("Test node must be started with '-sname'")
     end.

make_mids(MgNodes) when list(MgNodes), length(MgNodes) > 0 ->
    make_mids(MgNodes, []).

make_mids([], Mids) ->
    lists:reverse(Mids);
make_mids([MgNode|MgNodes], Mids) ->
    case string:tokens(atom_to_list(MgNode), [$@]) of
	[Name, _] ->
	    Mid = {deviceName, Name},
	    make_mids(MgNodes, [{Mid, MgNode}|Mids]);
	Else ->
	    exit("Test node must be started with '-sname'")
    end.

tim() ->
    {A,B,C} = erlang:now(),
    A*1000000000+B*1000+(C div 1000).

sleep(X) -> receive after X -> ok end.

error_msg(F,A) -> error_logger:error_msg(F ++ "~n",A).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

i(F) ->
    i(F, []).

i(F, A) ->
    print(info, get(verbosity), now(), get(tc), "INF", F, A).

d(F) ->
    d(F, []).

d(F, A) ->
    print(debug, get(verbosity), now(), get(tc), "DBG", F, A).

printable(_, debug)   -> true;
printable(info, info) -> true;
printable(_,_)        -> false.

print(Severity, Verbosity, Ts, Tc, P, F, A) ->
    print(printable(Severity,Verbosity), Ts, Tc, P, F, A).

print(true, Ts, Tc, P, F, A) ->
    io:format("*** [~s] ~s ~p ~s:~w ***"
              "~n   " ++ F ++ "~n", 
              [format_timestamp(Ts), P, self(), get(sname), Tc | A]);
print(_, _, _, _, _, _) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

random_init() ->
    {A,B,C} = now(),
    random:seed(A,B,C).

random() ->
    10 * random:uniform(50).

apply_load_timer() ->
    erlang:send_after(random(), self(), apply_load_timeout).

format_timestamp(Now) ->
    {N1, N2, N3} = Now,
    {Date, Time}   = calendar:now_to_datetime(Now),
    {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    FormatDate = 
        io_lib:format("~.4w:~.2.0w:~.2.0w ~.2.0w:~.2.0w:~.2.0w 4~w",
                      [YYYY,MM,DD,Hour,Min,Sec,round(N3/1000)]),  
    lists:flatten(FormatDate).
