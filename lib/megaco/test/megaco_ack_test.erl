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
-module(megaco_ack_test).

-compile(export_all).

-include("megaco_test_lib.hrl").
-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").

-define(TEST_VERBOSITY, debug).
-define(MGC_VERBOSITY,  debug).
-define(MG_VERBOSITY,   debug).

-define(LOAD_COUNTER_START, 10).
-define(A4444, ["11111111", "00000000", "00000000"]).
-define(A4445, ["11111111", "00000000", "11111111"]).
-define(A5555, ["11111111", "11111111", "00000000"]).
-define(A5556, ["11111111", "11111111", "11111111"]).

-define(MGC_START(Pid, Mid, ET, Verb), 
	megaco_test_mgc:start(Pid, Mid, ET, Verb)).
-define(MGC_STOP(Pid), megaco_test_mgc:stop(Pid)).
-define(MGC_GET_STATS(Pid, No), megaco_test_mgc:get_stats(Pid, No)).
-define(MGC_RESET_STATS(Pid), megaco_test_mgc:reset_stats(Pid)).
-define(MGC_REQ_DISC(Pid,To), megaco_test_mgc:request_discard(Pid,To)).
-define(MGC_REQ_PEND(Pid,To), megaco_test_mgc:request_pending(Pid,To)).
-define(MGC_REQ_HAND(Pid),    megaco_test_mgc:request_handle(Pid)).
-define(MGC_REQ_HANDS(Pid),   megaco_test_mgc:request_handle_sloppy(Pid)).
-define(MGC_UPDATE_UI(Pid,Tag,Val), 
	megaco_test_mgc:update_user_info(Pid,Tag,Val)).
-define(MGC_UPDATE_CI(Pid,Tag,Val), 
	megaco_test_mgc:update_conn_info(Pid,Tag,Val)).
-define(MGC_USER_INFO(Pid,Tag), megaco_test_mgc:user_info(Pid,Tag)).
-define(MGC_CONN_INFO(Pid,Tag), megaco_test_mgc:conn_info(Pid,Tag)).
-define(MGC_ACK_INFO(Pid,To),   megaco_test_mgc:ack_info(Pid,To)).

-define(MG_START(Pid, Mid, Enc, Transp, Conf, Verb), 
	megaco_test_mg:start(Pid, Mid, Enc, Transp, Conf, Verb)).
-define(MG_STOP(Pid), megaco_test_mg:stop(Pid)).
-define(MG_GET_STATS(Pid, No), megaco_test_mg:get_stats(Pid, No)).
-define(MG_RESET_STATS(Pid), megaco_test_mg:reset_stats(Pid)).
-define(MG_SERV_CHANGE(Pid), megaco_test_mg:service_change(Pid)).
-define(MG_NOTIF_RAR(Pid), megaco_test_mg:notify_request_and_reply(Pid)).
-define(MG_NOTIF_REQ(Pid), megaco_test_mg:notify_request(Pid)).
-define(MG_NOTIF_AR(Pid),  megaco_test_mg:await_notify_reply(Pid)).
-define(MG_CANCEL(Pid,R),  megaco_test_mg:cancel_request(Pid,R)).
-define(MG_APPLY_LOAD(Pid,CntStart), megaco_test_mg:apply_load(Pid,CntStart)).
-define(MG_UPDATE_UI(Pid,Tag,Val), 
	megaco_test_mg:update_user_info(Pid,Tag,Val)).
-define(MG_UPDATE_CI(Pid,Tag,Val), 
	megaco_test_mg:update_conn_info(Pid,Tag,Val)).
-define(MG_USER_INFO(Pid,Tag), megaco_test_mg:user_info(Pid,Tag)).
-define(MG_CONN_INFO(Pid,Tag), megaco_test_mg:conn_info(Pid,Tag)).
-define(MG_GRP_REQ(Pid,N),     megaco_test_mg:group_requests(Pid,N)).
-define(MG_ACK_INFO(Pid,To),   megaco_test_mg:ack_info(Pid,To)).

t()     -> megaco_test_lib:t(?MODULE).
t(Case) -> megaco_test_lib:t({?MODULE, Case}).


%% Test server callbacks
init_per_testcase(multi_ack_maxcount = Case, Config) ->
    process_flag(trap_exit, true),
    C = lists:keydelete(tc_timeout, 1, Config),
    megaco_test_lib:init_per_testcase(Case, [{tc_timeout,timer:minutes(10)}|C]);
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
	 single_ack,
	 multi_ack_timeout,
	 multi_ack_maxcount
	].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_ack(suite) ->
    [];
single_ack(doc) ->
    [];
single_ack(Config) when list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        single_ack),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p", 
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    %% Start the MGC and MGs
    i("[MGC] start"),    
    ET = [{text,tcp}, {text,udp}, {binary,tcp}, {binary,udp}],
    {ok, Mgc} = 
	?MGC_START(MgcNode, {deviceName, "ctrl"}, ET, ?MGC_VERBOSITY),

    i("[MG] start"),    
    %% MgConf0 = [{MgNode, "mg", text, tcp, ?MG_VERBOSITY}],
    MgMid = {deviceName, "mg"},
    MgConfig = [{auto_ack, true}, {accu_ack_timer, 5000}],
    {ok, Mg} = ?MG_START(MgNode, MgMid, text, tcp, MgConfig, ?MG_VERBOSITY),

    d("MG user info: ~p", [?MG_USER_INFO(Mg, all)]),

    i("[MG] connect to the MGC (service change)"),    
    ServChRes = ?MG_SERV_CHANGE(Mg),
    d("service change result: ~p", [ServChRes]),

    d("MG conn info: ~p", [?MG_CONN_INFO(Mg, all)]),

    d("ensure the megaco stack calls the handle_trans_ack callback"),
    ?MGC_REQ_HANDS(Mgc),

    d("tell the MGC to send the ack's to us"),
    ?MGC_ACK_INFO(Mgc, self()),

    d("send the notify"),
    ?MG_GRP_REQ(Mg, 1),

    d("send the notify"),
    ?MG_NOTIF_REQ(Mg),

    d("await the ack"),
    await_ack(Mgc, 1, infinity, ok),

    %% Tell MG to stop
    i("[MG] stop"),
    ?MG_STOP(Mg),

    %% Tell Mgc to stop
    i("[MGC] stop"),
    ?MGC_STOP(Mgc),

    i("done", []),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_ack_timeout(suite) ->
    [];
multi_ack_timeout(doc) ->
    [];
multi_ack_timeout(Config) when list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        multi_ack_timeout),
    i("starting"),


    MaxCount = 20,
    MgcNode  = make_node_name(mgc),
    MgNode   = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p", 
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    %% Start the MGC and MGs
    i("[MGC] start"),    
    ET = [{text,tcp}, {text,udp}, {binary,tcp}, {binary,udp}],
    {ok, Mgc} = 
	?MGC_START(MgcNode, {deviceName, "ctrl"}, ET, ?MGC_VERBOSITY),

    i("[MG] start"),    
    %% MgConf0 = [{MgNode, "mg", text, tcp, ?MG_VERBOSITY}],
    MgMid = {deviceName, "mg"},
    MgConfig = [{auto_ack,          true}, 
		{accu_ack_timer,    10000}, 
		{accu_ack_maxcount, MaxCount + 10}],
    {ok, Mg} = ?MG_START(MgNode, MgMid, text, tcp, MgConfig, ?MG_VERBOSITY),

    d("MG user info: ~p", [?MG_USER_INFO(Mg, all)]),

    i("[MG] connect the MGC (service change)"),    
    ServChRes = ?MG_SERV_CHANGE(Mg),
    d("service change result: ~p", [ServChRes]),

    i("wait some time"),
    sleep(1000),

    d("MG conn info: ~p", [?MG_CONN_INFO(Mg, all)]),

    {ok, _OldAction} = ?MGC_REQ_HANDS(Mgc),

    d("tell the MGC to send the ack's to us"),
    ?MGC_ACK_INFO(Mgc, self()),

    d("set group size to ~p", [MaxCount]),
    ?MG_GRP_REQ(Mg, MaxCount),

    d("[MG] send a group of requests (and await the replies)"),
    ?MG_NOTIF_RAR(Mg),

    d("await the ack(s)"),
    await_ack(Mgc, MaxCount, 60000, ok),

    i("wait some time before closing down"),
    sleep(5000),

    %% Tell MG to stop
    i("[MG] stop"),
    ?MG_STOP(Mg),

    %% Tell Mgc to stop
    i("[MGC] stop"),
    ?MGC_STOP(Mgc),

    i("done"),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

multi_ack_maxcount(suite) ->
    [];
multi_ack_maxcount(doc) ->
    [];
multi_ack_maxcount(Config) when list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        multi_ack_maxcount),
    i("starting"),

    MaxCount = 10,
    MgcNode  = make_node_name(mgc),
    MgNode   = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p", 
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    %% Start the MGC and MGs
    i("[MGC] start"),    
    ET = [{text,tcp}, {text,udp}, {binary,tcp}, {binary,udp}],
    {ok, Mgc} = 
	?MGC_START(MgcNode, {deviceName, "ctrl"}, ET, ?MGC_VERBOSITY),

    i("[MG] start"),    
    %% MgConf0 = [{MgNode, "mg", text, tcp, ?MG_VERBOSITY}],
    MgMid = {deviceName, "mg"},
    MgConfig = [%% {auto_ack,          true}, 
		%% {accu_ack_timer,    120000}, 
		%% {accu_ack_maxcount, MaxCount}
	       ],
    {ok, Mg} = ?MG_START(MgNode, MgMid, text, tcp, MgConfig, ?MG_VERBOSITY),

    d("MG user info: ~p", [?MG_USER_INFO(Mg, all)]),

    i("[MG] connect the MGC (service change)"),    
    ServChRes = ?MG_SERV_CHANGE(Mg),
    d("service change result: ~p", [ServChRes]),

    i("wait some time"),
    sleep(1000),

    d("MG conn info: ~p", [?MG_CONN_INFO(Mg, all)]),

    ?MG_UPDATE_CI(Mg,auto_ack,true), 
    ?MG_UPDATE_CI(Mg,accu_ack_timer,120000), 
    ?MG_UPDATE_CI(Mg,accu_ack_maxcount,MaxCount), 

    d("MG conn info: ~p", [?MG_CONN_INFO(Mg, all)]),

    {ok, _OldAction} = ?MGC_REQ_HANDS(Mgc),

    d("tell the MGC to send the ack's to us"),
    ?MGC_ACK_INFO(Mgc, self()),

    d("set group size to ~p", [MaxCount]),
    ?MG_GRP_REQ(Mg, MaxCount),

    d("[MG] send a group of requests (and await the replies)"),
    ?MG_NOTIF_RAR(Mg),

    d("await the ack"),
    await_ack(Mgc, MaxCount, 60000, ok),

    i("wait some time before closing down"),
    sleep(5000),


    %% Tell MG to stop
    i("[MG] stop"),
    ?MG_STOP(Mg),

    %% Tell Mgc to stop
    i("[MGC] stop"),
    ?MGC_STOP(Mgc),

    i("done"),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

await_ack(_User, 0, Timeout, _Expected) ->
    d("await_ack -> done when Timeout = ~p", [Timeout]),
    ok;
await_ack(User, N, Timeout, Expected) when N > 0, integer(Timeout) ->
    d("await_ack -> entry with N: ~p, Timeout: ~p", [N,Timeout]),
    T = tim(),
    receive
	{ack_received, User, Expected} ->
	    d("await_ack -> received another ack"),
	    await_ack(User, N-1, Timeout - (tim() - T), Expected);
	{ack_received, User, UnExpected} ->
	    d("await_ack -> unexpected ack result: ~p", [UnExpected]),
	    exit({unexpected_ack_result, UnExpected, Expected})
    after Timeout ->
	    exit({await_ack_timeout, N})
    end;
await_ack(User, N, infinity, Expected) when N > 0 ->
    d("await_ack -> entry with N: ~p", [N]),
    receive
	{ack_received, User, Expected} ->
	    d("await_ack -> received another ack"),
	    await_ack(User, N-1, infinity, Expected);
	{ack_received, User, UnExpected} ->
	    d("await_ack -> unexpected ack result: ~p", [UnExpected]),
	    exit({unexpected_ack_result, UnExpected, Expected})
    end.

tim() ->
    {A,B,C} = erlang:now(),
    A*1000000000+B*1000+(C div 1000).


make_node_name(Name) ->
    case string:tokens(atom_to_list(node()), [$@]) of
	[_,Host] ->
	    list_to_atom(lists:concat([atom_to_list(Name) ++ "@" ++ Host]));
	_ ->
	    exit("Test node must be started with '-sname'")
     end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

