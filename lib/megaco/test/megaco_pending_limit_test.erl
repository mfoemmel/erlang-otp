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
%%          Testing the xxxOriginatingPendingLimit property of the 
%%          root package
%%----------------------------------------------------------------------
-module(megaco_pending_limit_test).

-compile(export_all).

-include("megaco_test_lib.hrl").
-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").

-define(TEST_VERBOSITY, debug).
-define(MGC_VERBOSITY,  debug).
-define(MG_VERBOSITY,   debug).

-define(VERSION, 1).

-define(A4444, ["11111111", "00000000", "00000000"]).
-define(A4445, ["11111111", "00000000", "11111111"]).
-define(A5555, ["11111111", "11111111", "00000000"]).
-define(A5556, ["11111111", "11111111", "11111111"]).

-define(MGC_START(Pid, Mid, ET, Verb), 
	megaco_test_mgc:start(Pid, Mid, ET, Verb)).
-define(MGC_STOP(Pid), megaco_test_mgc:stop(Pid)).
-define(MGC_GET_STATS(Pid, No), megaco_test_mgc:get_stats(Pid, No)).
-define(MGC_RESET_STATS(Pid),   megaco_test_mgc:reset_stats(Pid)).
-define(MGC_REQ_IGNORE(Pid),    megaco_test_mgc:request_ignore(Pid)).
-define(MGC_REQ_PIGNORE(Pid),   megaco_test_mgc:request_pending_ignore(Pid)).
-define(MGC_REQ_DISC(Pid,To),   megaco_test_mgc:request_discard(Pid,To)).
-define(MGC_REQ_PEND(Pid,To),   megaco_test_mgc:request_pending(Pid,To)).
-define(MGC_REQ_HAND(Pid, To),  megaco_test_mgc:request_handle(Pid, To)).
-define(MGC_REQ_HANDS(Pid),     megaco_test_mgc:request_handle_sloppy(Pid)).
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
-define(MG_ECC(Pid, M, T, F),  megaco_test_mg:enable_test_code(Pid,M,T,F)).

t()     -> megaco_test_lib:t(?MODULE).
t(Case) -> megaco_test_lib:t({?MODULE, Case}).


%% Test server callbacks
init_per_testcase(Case, Config) ->
    process_flag(trap_exit, true),
    megaco_test_lib:init_per_testcase(Case, Config).

fin_per_testcase(Case, Config) ->
    process_flag(trap_exit, false),
    megaco_test_lib:fin_per_testcase(Case, Config).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all(suite) ->
    [
     timer_late_reply,
     timer_exceeded,
     timer_exceeded_long,
     resend_late_reply,
     resend_exceeded,
     resend_exceeded_long,
     
     %% Tickets last 
     tickets
    ].

tickets(suite) ->
    [
     otp_4956
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

timer_late_reply(suite) ->
    [];
timer_late_reply(doc) ->
    "...";
timer_late_reply(Config) when list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        timer_late_reply),
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
    MgMid = {deviceName, "mg"},
    MgConfig = [],
    {ok, Mg} = ?MG_START(MgNode, MgMid, text, tcp, MgConfig, ?MG_VERBOSITY),

    d("MG user info: ~p", [?MG_USER_INFO(Mg, all)]),

    i("[MG] connect to the MGC (service change)"),    
    ServChRes = ?MG_SERV_CHANGE(Mg),
    d("service change result: ~p", [ServChRes]),

    d("MG conn info: ~p", [?MG_CONN_INFO(Mg, all)]),

    d("[MGC] update connection info pending timer"),
    PendingTimer = #megaco_incr_timer{wait_for = timer:seconds(5),
				      factor   = 1},
    ?MGC_UPDATE_CI(Mgc, pending_timer, PendingTimer),

    d("[MGC] update connection info originating pending limit"),
    PendingLimit = 5,
    ?MGC_UPDATE_CI(Mgc, orig_pending_limit, PendingLimit),

    d("[MGC] late reply to requests "
      "(simulate that the request takes a long time)"),
    ?MGC_REQ_DISC(Mgc, 11000),

    d("[MG] send the notify"),
    {ok, Reply} = ?MG_NOTIF_RAR(Mg),
    d("[MG] Reply: ~p", [Reply]),
    {_Version, {ok, [_ActionReply]}} = Reply,

    %% Tell MG to stop
    i("[MG] stop"),
    ?MG_STOP(Mg),

    %% Tell Mgc to stop
    i("[MGC] stop"),
    ?MGC_STOP(Mgc),

    i("done", []),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

timer_exceeded(suite) ->
    [];
timer_exceeded(doc) ->
    "...";
timer_exceeded(Config) when list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        timer_exceeded),
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
    MgMid = {deviceName, "mg"},
    MgConfig = [],
    {ok, Mg} = ?MG_START(MgNode, MgMid, text, tcp, MgConfig, ?MG_VERBOSITY),

    d("MG user info: ~p", [?MG_USER_INFO(Mg, all)]),

    i("[MG] connect to the MGC (service change)"),    
    ServChRes = ?MG_SERV_CHANGE(Mg),
    d("service change result: ~p", [ServChRes]),

    d("MG conn info: ~p", [?MG_CONN_INFO(Mg, all)]),

    d("[MGC] update connection info pending timer"),
    PendingTimer = #megaco_incr_timer{wait_for = timer:seconds(5),
				      factor    = 1},
    ?MGC_UPDATE_CI(Mgc, pending_timer, PendingTimer),

    d("[MGC] update connection info originating pending limit"),
    PendingLimit = 5,
    ?MGC_UPDATE_CI(Mgc, orig_pending_limit, PendingLimit),

    d("[MGC] no reply to requests "
      "(simulate that the request takes a __long__ time)"),
    ?MGC_REQ_IGNORE(Mgc),

    d("[MG] send the notify"),
    ED = ?MG_NOTIF_RAR(Mg),
    d("[MG] ED: ~p", [ED]),
    ErrorCode = ?megaco_number_of_transactionpending_exceeded,
    #'ErrorDescriptor'{errorCode = ErrorCode} = ED,

    %% Tell MG to stop
    i("[MG] stop"),
    ?MG_STOP(Mg),

    %% Tell Mgc to stop
    i("[MGC] stop"),
    ?MGC_STOP(Mgc),

    i("done", []),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

timer_exceeded_long(suite) ->
    [];
timer_exceeded_long(doc) ->
    "...";
timer_exceeded_long(Config) when list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        timer_exceeded_long),
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
    MgMid = {deviceName, "mg"},
    MgConfig = [],
    {ok, Mg} = ?MG_START(MgNode, MgMid, text, tcp, MgConfig, ?MG_VERBOSITY),

    d("MG user info: ~p", [?MG_USER_INFO(Mg, all)]),

    i("[MG] connect to the MGC (service change)"),    
    ServChRes = ?MG_SERV_CHANGE(Mg),
    d("service change result: ~p", [ServChRes]),

    d("MG conn info: ~p", [?MG_CONN_INFO(Mg, all)]),

    d("[MGC] update connection info pending timer"),
    PendingTimer = #megaco_incr_timer{wait_for = timer:seconds(5),
				      factor    = 1},
    ?MGC_UPDATE_CI(Mgc, pending_timer, PendingTimer),

    d("[MGC] update connection info originating pending limit"),
    PendingLimit = 5,
    ?MGC_UPDATE_CI(Mgc, orig_pending_limit, PendingLimit),

    d("[MGC] long request with no reply ~n"
      "   (simulate that we know that this will "
      "take a while, but takes even longer...)"),
    ?MGC_REQ_PIGNORE(Mgc),

    d("[MG] send the notify"),
    ED = ?MG_NOTIF_RAR(Mg),
    d("[MG] ED: ~p", [ED]),
    ErrorCode = ?megaco_number_of_transactionpending_exceeded,
    #'ErrorDescriptor'{errorCode = ErrorCode} = ED,

    %% Tell MG to stop
    i("[MG] stop"),
    ?MG_STOP(Mg),

    %% Tell Mgc to stop
    i("[MGC] stop"),
    ?MGC_STOP(Mgc),

    i("done", []),
    ok.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case can only be run with the stack compiled with
%% the MEGACO_TEST_CODE flag. Therefor there is no point in 
%% including this test case in the usual test suite
-ifdef(MEGACO_TEST_CODE).
resend_late_reply(suite) ->
    [];
resend_late_reply(doc) ->
    "...";
resend_late_reply(Config) when list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        resend_late_reply),
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
    MgMid = {deviceName, "mg"},
    MgConfig = [],
    {ok, Mg} = ?MG_START(MgNode, MgMid, text, tcp, MgConfig, ?MG_VERBOSITY),

    d("MG user info: ~p", [?MG_USER_INFO(Mg, all)]),

    i("[MG] connect to the MGC (service change)"),    
    ServChRes = ?MG_SERV_CHANGE(Mg),
    d("service change result: ~p", [ServChRes]),

    d("MG conn info: ~p", [?MG_CONN_INFO(Mg, all)]),

    d("[MGC] update connection info pending timer"),
    PendingTimer = infinity,
    %%     PendingTimer = #megaco_incr_timer{wait_for = timer:seconds(5),
    %% 				      factor    = 1},
    ?MGC_UPDATE_CI(Mgc, pending_timer, PendingTimer),

    d("[MGC] update connection info originating pending limit"),
    PendingLimit = 5,
    ?MGC_UPDATE_CI(Mgc, orig_pending_limit, PendingLimit),

    d("[MG] update connection info request timer"),
    RequestTimer = #megaco_incr_timer{wait_for = timer:seconds(5),
				      factor   = 1},
    ?MG_UPDATE_CI(Mg, request_timer, RequestTimer),

    d("[MGC] no reply to requests "
      "(simulate that the request takes a __long__ time)"),
    ?MGC_REQ_IGNORE(Mgc),

    d("[MG] set the 'init_request_timer' tag"),
    EccRes = (catch ?MG_ECC(Mg, megaco_messenger, 
			    init_request_timer, fun init_request_timer/1)),
    d("[MG] EccRes: ~p", [EccRes]),

    d("[MGC] late reply to requests "
      "(simulate that the request takes a long time)"),
    ?MGC_REQ_DISC(Mgc, 11000),

    d("[MG] send the notify"),
    {ok, Reply} = (catch ?MG_NOTIF_RAR(Mg)),
    d("[MG] Reply: ~p", [Reply]),
    {_Version, {ok, [_ActionReply]}} = Reply,

    %% Tell MG to stop
    i("[MG] stop"),
    ?MG_STOP(Mg),

    %% Tell Mgc to stop
    i("[MGC] stop"),
    ?MGC_STOP(Mgc),

    i("done", []),
    ok.

-else.

resend_late_reply(suite) ->
    [];
resend_late_reply(doc) ->
    "...";
resend_late_reply(Config) when list(Config) ->
    ?SKIP("included only if compiled with USE_MEGACO_TEST_CODE=true").

-endif.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case can only be run with the stack compiled with
%% the MEGACO_TEST_CODE flag. Therefor there is no point in 
%% including this test case in the usual test suite
-ifdef(MEGACO_TEST_CODE).
resend_exceeded(suite) ->
    [];
resend_exceeded(doc) ->
    "...";
resend_exceeded(Config) when list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        resend_exceeded),
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
    MgMid = {deviceName, "mg"},
    MgConfig = [],
    {ok, Mg} = ?MG_START(MgNode, MgMid, text, tcp, MgConfig, ?MG_VERBOSITY),

    d("MG user info: ~p", [?MG_USER_INFO(Mg, all)]),

    i("[MG] connect to the MGC (service change)"),    
    ServChRes = ?MG_SERV_CHANGE(Mg),
    d("service change result: ~p", [ServChRes]),

    d("MG conn info: ~p", [?MG_CONN_INFO(Mg, all)]),

    d("[MGC] update connection info pending timer"),
    PendingTimer = infinity,
    ?MGC_UPDATE_CI(Mgc, pending_timer, PendingTimer),

    d("[MGC] update connection info originating pending limit"),
    PendingLimit = 5,
    ?MGC_UPDATE_CI(Mgc, orig_pending_limit, PendingLimit),

    d("[MG] update connection info request timer"),
    RequestTimer = #megaco_incr_timer{wait_for = timer:seconds(5),
				      factor   = 1},
    ?MG_UPDATE_CI(Mg, request_timer, RequestTimer),

    d("[MGC] no reply to requests "
      "(simulate that the request takes a __long__ time)"),
    ?MGC_REQ_IGNORE(Mgc),

    d("[MG] set the 'init_request_timer' tag"),
    EccRes = (catch ?MG_ECC(Mg, megaco_messenger, 
			    init_request_timer, fun init_request_timer/1)),
    d("[MG] EccRes: ~p", [EccRes]),

    d("[MG] send the notify"),
    ED = (catch ?MG_NOTIF_RAR(Mg)),
    d("[MG] ED: ~p", [ED]),
    ErrorCode = ?megaco_number_of_transactionpending_exceeded,
    #'ErrorDescriptor'{errorCode = ErrorCode} = ED,

    %% Tell MG to stop
    i("[MG] stop"),
    ?MG_STOP(Mg),

    %% Tell Mgc to stop
    i("[MGC] stop"),
    ?MGC_STOP(Mgc),

    i("done", []),
    ok.


-else.

resend_exceeded(suite) ->
    [];
resend_exceeded(doc) ->
    "...";
resend_exceeded(Config) when list(Config) ->
    ?SKIP("included only if compiled with USE_MEGACO_TEST_CODE=true").

-endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% This test case can only be run with the stack compiled with
%% the MEGACO_TEST_CODE flag. Therefor there is no point in 
%% including this test case in the usual test suite
-ifdef(MEGACO_TEST_CODE).
resend_exceeded_long(suite) ->
    [];
resend_exceeded_long(doc) ->
    "...";
resend_exceeded_long(Config) when list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        resend_exceeded_long),
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
    MgMid = {deviceName, "mg"},
    MgConfig = [],
    {ok, Mg} = ?MG_START(MgNode, MgMid, text, tcp, MgConfig, ?MG_VERBOSITY),

    d("MG user info: ~p", [?MG_USER_INFO(Mg, all)]),

    i("[MG] connect to the MGC (service change)"),    
    ServChRes = ?MG_SERV_CHANGE(Mg),
    d("service change result: ~p", [ServChRes]),

    d("MG conn info: ~p", [?MG_CONN_INFO(Mg, all)]),

    d("[MGC] update connection info pending timer"),
    PendingTimer = infinity,
    ?MGC_UPDATE_CI(Mgc, pending_timer, PendingTimer),

    d("[MGC] update connection info originating pending limit"),
    PendingLimit = 5,
    ?MGC_UPDATE_CI(Mgc, orig_pending_limit, PendingLimit),

    d("[MG] update connection info request timer"),
    RequestTimer = #megaco_incr_timer{wait_for = timer:seconds(5),
				      factor   = 1},
    ?MG_UPDATE_CI(Mg, request_timer, RequestTimer),

    d("[MGC] long request with no reply ~n"
      "   (simulate that we know that this will "
      "take a while, but takes even longer...)"),
    ?MGC_REQ_PIGNORE(Mgc),

    d("[MG] set the 'init_request_timer' tag"),
    EccRes = (catch ?MG_ECC(Mg, megaco_messenger, 
			    init_request_timer, fun init_request_timer/1)),
    d("[MG] EccRes: ~p", [EccRes]),

    d("[MG] send the notify"),
    ED = (catch ?MG_NOTIF_RAR(Mg)),
    d("[MG] ED: ~p", [ED]),
    ErrorCode = ?megaco_number_of_transactionpending_exceeded,
    #'ErrorDescriptor'{errorCode = ErrorCode} = ED,

    %% Tell MG to stop
    i("[MG] stop"),
    ?MG_STOP(Mg),

    %% Tell Mgc to stop
    i("[MGC] stop"),
    ?MGC_STOP(Mgc),

    i("done", []),
    ok.


-else.

resend_exceeded_long(suite) ->
    [];
resend_exceeded_long(doc) ->
    "...";
resend_exceeded_long(Config) when list(Config) ->
    ?SKIP("included only if compiled with USE_MEGACO_TEST_CODE=true").

-endif.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_4956(suite) ->
    [];
otp_4956(Config) when list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        otp_4956),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p", 
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    d("[MGC] start the simulator "),
    {ok, Mgc} = megaco_test_generator:start_link("MGC", MgcNode),

    d("[MGC] create the event sequence"),
    MgcEvSeq = otp_4956_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    megaco_test_generator:megaco(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = otp_4956_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    megaco_test_generator:tcp(Mg, MgEvSeq),

    d("[MGC] await the generator reply"),
    case megaco_test_generator:megaco_await_reply(Mgc) of
        {ok, MgcReply} ->
            d("[MGC] OK => MgcReply: ~n~p", [MgcReply]),
            ok;
        {error, MgcReply} ->
            d("[MGC] ERROR => MgcReply: ~n~p", [MgcReply]),
            ?ERROR(mgc_failed)
    end,

    d("[MG] await the generator reply"),
    case megaco_test_generator:tcp_await_reply(Mg) of
        {ok, MgReply} ->
            d("[MG] OK => MgReply: ~n~p", [MgReply]),
            ok;
        {error, MgReply} ->
            d("[MG] ERROR => MgReply: ~n~p", [MgReply]),
            ?ERROR(mg_failed)
    end,

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_generator:stop(Mgc),

    %% Tell Mg to stop
    i("[MG] stop generator"),
    megaco_test_generator:stop(Mg),

    i("done", []),
    ok.


%%
%% MGC generator stuff
%% 
otp_4956_mgc_event_sequence(text, tcp) ->
    Mid = {deviceName,"ctrl"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    ConnectVerify = fun otp_4956_mgc_verify_handle_connect/1,
    ServiceChangeReqVerify = otp_4956_mgc_verify_service_change_req_fun(Mid),
    NotifyReqVerify = otp_4956_mgc_verify_notify_request_fun(),
    ReqAbortVerify = fun otp_4956_mgc_verify_handle_trans_request_abort/1, 
    DiscoVerify = fun otp_4956_mgc_verify_handle_disconnect/1,
    EvSeq = [
             {debug, true},
             {megaco_trace, disable},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
	     {megaco_update_user_info, orig_pending_limit, 4},
             start_transport,
             listen,
             {megaco_callback, handle_connect, ConnectVerify},
	     {megaco_conn_info, all},
             {megaco_callback, handle_trans_request, ServiceChangeReqVerify},
             {megaco_callback, handle_trans_request, NotifyReqVerify},
             {megaco_callback, handle_trans_request_abort, ReqAbortVerify},
             {megaco_callback, nocall, 1000},
             {megaco_callback, handle_disconnect, DiscoVerify},
             megaco_stop_user,
             megaco_stop
            ],
    EvSeq.

otp_4956_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    {ok, CH, ok};
otp_4956_mgc_verify_handle_connect(Else) ->
    {error, Else, ok}.

otp_4956_mgc_verify_service_change_req_fun(Mid) ->
    fun({handle_trans_request, _, ?VERSION, [AR]}) ->
%             io:format("otp_4956_mgc_verify_service_change_req -> ok"
%                       "~n   AR: ~p~n", [AR]),
            case AR of
                #'ActionRequest'{commandRequests = [CR]} ->
                    case CR of
                        #'CommandRequest'{command = Cmd} ->
                            case Cmd of
                                {serviceChangeReq, 
                                 #'ServiceChangeRequest'{terminationID = [Tid],
                                                         serviceChangeParms = Parms}} ->
                                    case Tid of
                                        #megaco_term_id{contains_wildcards = false, 
                                                        id = ["root"]} ->
                                            case Parms of
                                                #'ServiceChangeParm'{
                                                         serviceChangeMethod = restart,
                                                         serviceChangeReason = [[$9,$0,$1|_]]} ->
                                                    Reply = 
                                                        {discard_ack, 
                                                         [otp_4956_mgc_service_change_reply_ar(Mid, 1)]},
                                                    {ok, AR, Reply};
                                                _ ->
                                                    Err = {invalid_SCP, Parms},
                                                    ED = otp_4956_err_desc(Parms),
                                                    ErrReply = {discard_ack, 
                                                                ED},
                                                    {error, Err, ErrReply}
                                            end;
                                        _ ->
                                            Err = {invalid_termination_id, Tid},
                                            ED = otp_4956_err_desc(Tid),
                                            ErrReply = {discard_ack, ED},
                                            {error, Err, ErrReply}
                                    end;
                                _ ->
                                    Err = {invalid_command, Cmd},
                                    ED = otp_4956_err_desc(Cmd),
                                    ErrReply = {discard_ack, ED},
                                    {error, Err, ErrReply}
                            end;
                        _ ->
                            Err = {invalid_command_request, CR},
                            ED = otp_4956_err_desc(CR),
                            ErrReply = {discard_ack, ED},
                            {error, Err, ErrReply}
                    end;
                _ ->
                    Err = {invalid_action_request, AR},
                    ED = otp_4956_err_desc(AR),
                    ErrReply = {discard_ack, ED},
                    {error, Err, ErrReply}
            end;
       (Else) ->
%             io:format("otp_4956_mgc_verify_service_change_req -> unknown"
%                       "~n   Else: ~p~n", [Else]),
            ED = otp_4956_err_desc(Else),
            ErrReply = {discard_ack, ED},
            {error, Else, ErrReply}
    end.

otp_4956_mgc_verify_notify_request_fun() ->
    fun({handle_trans_request, _, ?VERSION, [AR]}) ->
%             io:format("otp_4956_mgc_verify_notify_request:fun -> ok"
%                       "~n   AR: ~p~n", [AR]),
            case AR of
                #'ActionRequest'{contextId = Cid, 
                                 commandRequests = [CR]} ->
                    #'CommandRequest'{command = Cmd} = CR,
                    {notifyReq, NR} = Cmd,
                    #'NotifyRequest'{terminationID = [Tid],
                                     observedEventsDescriptor = OED,
                                     errorDescriptor = asn1_NOVALUE} = NR,
                    #'ObservedEventsDescriptor'{observedEventLst = [OE]} = OED,
                    #'ObservedEvent'{eventName = "al/of"} = OE,
                    Reply = {discard_ack, [otp_4956_mgc_notify_reply_ar(Cid, Tid)]},
                    {ok, 3000, AR, Reply};
                _ ->
                    ED = otp_4956_err_desc(AR),
                    ErrReply = {discard_ack, ED},
                    {error, AR, ErrReply}
            end;
       (Else) ->
%             io:format("otp_4956_mgc_verify_notify_request:fun -> unknown"
%                       "~n   Else: ~p~n", [Else]),
            ED = otp_4956_err_desc(Else),
            ErrReply = {discard_ack, ED},
            {error, Else, ErrReply}
    end.

otp_4956_mgc_verify_handle_trans_request_abort({handle_trans_request_abort, 
						CH, ?VERSION, 2, Pid}) -> 
    {ok, {CH, Pid}, ok};
otp_4956_mgc_verify_handle_trans_request_abort(Else) ->
    {error, Else, ok}.

otp_4956_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, R}) -> 
%     io:format("otp_4956_mgc_verify_handle_disconnect -> ok"
%               "~n   CH: ~p"
%               "~n   R:  ~p"
%               "~n", [CH, R]),
    {ok, CH, ok};
otp_4956_mgc_verify_handle_disconnect(Else) ->
%     io:format("otp_4956_mgc_verify_handle_disconnect -> unknown"
%               "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

otp_4956_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    AR    = cre_actionReply(Cid, [CR]).

otp_4956_mgc_service_change_reply_msg(Mid, TransId, Cid) ->
    AR    = otp_4956_mgc_service_change_reply_ar(Mid, Cid),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

otp_4956_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

otp_4956_mgc_notify_reply(Mid, TransId, Cid, TermId) ->
    AR    = otp_4956_mgc_notify_reply_ar(Cid, TermId),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%% 
otp_4956_mg_event_sequence(text, tcp) ->
    DecodeFun = otp_4956_mg_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = otp_4956_mg_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid = {deviceName,"mg"},
    ServiceChangeReq = otp_4956_mg_service_change_request_msg(Mid, 1, 0),
    ServiceChangeReplyVerifyFun = 
	otp_4956_mg_verify_service_change_rep_msg_fun(),
    TermId = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NotifyReq = otp_4956_mg_notify_request_msg(Mid, 2, 1, TermId, 1),
    PendingVerify = otp_4956_mg_verify_pending_msg_fun(),
    PendingLimitVerify = otp_4956_mg_verify_pending_limit_msg_fun(),
    MgcEvSeq = [{debug,  true},
                {decode, DecodeFun},
                {encode, EncodeFun},
                {connect, 2944},
                {send, "service-change-request", ServiceChangeReq}, 
                {expect_receive, "service-change-reply", {ServiceChangeReplyVerifyFun, 10000}}, 
                {send, "notify request (first send)", NotifyReq}, 
                {sleep, 100},
                {send, "notify request (resend 1)", NotifyReq}, 
                {expect_receive, "pending 1", {PendingVerify, 1000}},
                {send, "notify request (resend 2)", NotifyReq}, 
                {expect_receive, "pending 2", {PendingVerify, 1000}},
                {send, "notify request (resend 3)", NotifyReq}, 
                {expect_receive, "pending 3", {PendingVerify, 1000}},
                {send, "notify request (resend 4)", NotifyReq}, 
                {expect_receive, "pending 4", {PendingVerify, 1000}},
                {send, "notify request (resend 5)", NotifyReq}, 
                {expect_receive, "pending limit exceeded", 
		 {PendingLimitVerify, 1000}},
                {send, "notify request (resend 6)", NotifyReq}, 
                {expect_nothing, 1000},
		disconnect
               ],
    MgcEvSeq.

otp_4956_mg_encode_msg_fun(Mod, Conf) ->
    fun(M) -> 
            Mod:encode_message(Conf, M) 
    end.
otp_4956_mg_encode_msg_fun(Mod, Conf, Ver) ->
    fun(M) -> 
            Mod:encode_message(Conf, Ver, M) 
    end.

otp_4956_mg_decode_msg_fun(Mod, Conf) ->
    fun(M) -> 
            Mod:decode_message(Conf, M) 
    end.
otp_4956_mg_decode_msg_fun(Mod, Conf, Ver) ->
    fun(M) -> 
            Mod:decode_message(Conf, Ver, M) 
    end.

otp_4956_mg_verify_service_change_rep_msg_fun() ->
    fun(#'MegacoMessage'{mess = Mess} = M) -> 
            io:format("otp_4956_mg_verify_service_change_rep:fun -> "
		      "ok so far~n",[]),
            #'Message'{version     = _V,
                       mId         = _MgMid,
                       messageBody = Body} = Mess,
            {transactions, [Trans]} = Body,
            {transactionReply, TR} = Trans,
            #'TransactionReply'{transactionId = _Tid,
				immAckRequired = asn1_NOVALUE,
				transactionResult = Res} = TR,
	    {actionReplies, [AR]} = Res,
            #'ActionReply'{contextId = _Cid,
			   errorDescriptor = asn1_NOVALUE,
			   contextReply = _CtxReq,
			   commandReply = [CR]} = AR,
	    {serviceChangeReply, SCR} = CR,
            #'ServiceChangeReply'{terminationID = _TermID,
				  serviceChangeResult = SCRes} = SCR,
	    {serviceChangeResParms, SCRP} = SCRes,
            #'ServiceChangeResParm'{serviceChangeMgcId = _MgcMid} = SCRP,
            {ok, M};
       (M) ->
            {error, {invalid_message, M}}
    end.

otp_4956_mg_verify_pending_msg_fun() ->
    fun(#'MegacoMessage'{mess = Mess} = M) -> 
            #'Message'{messageBody = Body} = Mess,
            {transactions, [Trans]} = Body,
            {transactionPending, TP} = Trans,
            #'TransactionPending'{transactionId = _Id} = TP,
            {ok, M};
       (M) ->
            {error, {invalid_message, M}}
    end.

otp_4956_mg_verify_pending_limit_msg_fun() ->
    fun(#'MegacoMessage'{mess = Mess} = M) -> 
            #'Message'{messageBody = Body} = Mess,
            {messageError, ED} = Body,
            #'ErrorDescriptor'{errorCode = ?megaco_number_of_transactionpending_exceeded} = ED,
            {ok, M};
       (M) ->
            {error, {invalid_message, M}}
    end.

otp_4956_mg_service_change_request_ar(Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    AR    = cre_actionReq(Cid, [CR]).

otp_4956_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = otp_4956_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

otp_4956_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

otp_4956_mg_notify_request_msg(Mid, TransId, Rid, TermId, Cid) ->
    AR      = otp_4956_mg_notify_request_ar(Rid, TermId, Cid),
    TR      = cre_transReq(TransId, [AR]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


otp_4956_err_desc(T) ->
    EC = ?megaco_internal_gateway_error,
    ET = lists:flatten(io_lib:format("~w",[T])),
    #'ErrorDescriptor'{errorCode = EC, errorText = ET}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Common message creation functions
%%

cre_serviceChangeParm(M,R,P) ->
    #'ServiceChangeParm'{serviceChangeMethod  = M, 
                         serviceChangeReason  = R, 
                         serviceChangeProfile = P}.

cre_serviceChangeReq(Tid, Parms) ->
    #'ServiceChangeRequest'{terminationID      = Tid, 
                            serviceChangeParms = Parms}.

cre_timeNotation(D,T) ->
    #'TimeNotation'{date = D, time = T}.

cre_obsEvent(Name, Not) ->
    #'ObservedEvent'{eventName    = Name, 
                     timeNotation = Not}.
cre_obsEvent(Name, Not, Par) ->
    #'ObservedEvent'{eventName    = Name, 
                     timeNotation = Not, 
                     eventParList = Par}.

cre_obsEvsDesc(Id, EvList) ->
    #'ObservedEventsDescriptor'{requestId        = Id, 
                                observedEventLst = EvList}.

cre_notifyReq(Tid, EvsDesc) ->
    #'NotifyRequest'{terminationID            = Tid, 
                     observedEventsDescriptor = EvsDesc}.

cre_command(R) when record(R, 'NotifyRequest') ->
    {notifyReq, R};
cre_command(R) when record(R, 'ServiceChangeRequest') ->
    {serviceChangeReq, R}.

cre_cmdReq(Cmd) ->
    #'CommandRequest'{command = Cmd}.

cre_actionReq(CtxId, CmdReqs) when list(CmdReqs) ->
    #'ActionRequest'{contextId       = CtxId,
                     commandRequests = CmdReqs}.

cre_transReq(TransId, ARs) when list(ARs) ->
    TR = #'TransactionRequest'{transactionId = TransId,
                               actions       = ARs}.

%% --

cre_serviceChangeResParm(Mid) ->
    cre_serviceChangeResParm(Mid, ?VERSION).

cre_serviceChangeResParm(Mid, V) ->
    #'ServiceChangeResParm'{serviceChangeMgcId   = Mid, 
			    serviceChangeVersion = V}.

cre_serviceChangeResult(SCRP) when record(SCRP, 'ServiceChangeResParm') ->
    {serviceChangeResParms, SCRP};
cre_serviceChangeResult(ED) when record(ED, 'ErrorDescriptor') ->
    {errorDescriptor, ED}.

cre_serviceChangeReply(Tid, Res) ->
    #'ServiceChangeReply'{terminationID       = Tid, 
                          serviceChangeResult = Res}.

cre_cmdReply(R) when record(R, 'NotifyReply') ->
    {notifyReply, R};
cre_cmdReply(R) when record(R, 'ServiceChangeReply') ->
    {serviceChangeReply, R}.

cre_notifyReply(Tid) ->
    #'NotifyReply'{terminationID = Tid}.

cre_actionReply(CtxId, CmdRep) ->
    #'ActionReply'{contextId    = CtxId,
                   commandReply = CmdRep}.

cre_transResult(ED) when record(ED, 'ErrorDescriptor') ->
    {transactionError, ED};
cre_transResult([AR|_] = ARs) when record(AR, 'ActionReply') ->
    {actionReplies, ARs}.

cre_transReply(TransId, Res) ->
    #'TransactionReply'{transactionId     = TransId,
                        transactionResult = Res}.

%% --

cre_serviceChangeProf(Name, Ver) when list(Name), integer(Ver) ->
    #'ServiceChangeProfile'{profileName = Name, 
                            version     = Ver}.

cre_transaction(Trans) when record(Trans, 'TransactionRequest') ->
    {transactionRequest, Trans};
cre_transaction(Trans) when record(Trans, 'TransactionPending') ->
    {transactionPending, Trans};
cre_transaction(Trans) when record(Trans, 'TransactionReply') ->
    {transactionReply, Trans};
cre_transaction(Trans) when record(Trans, 'TransactionAck') ->
    {transactionResponseAck, Trans}.

cre_transactions(Trans) when list(Trans) ->
    {transactions, Trans}.

cre_message(Version, Mid, Body) ->
    #'Message'{version     = Version,
               mId         = Mid,
               messageBody = Body}.

cre_megacoMessage(Mess) ->
    #'MegacoMessage'{mess = Mess}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Transform a short timer to a long one.
%% The purpose of this is to trick the stack
%% to keep re-sending the request, even after 
%% having received the first pending (which
%% indicates that the other side _IS_ 
%% working on the request).
-ifdef(MEGACO_TEST_CODE).

init_request_timer({short, Ref}) ->
    {long, Ref};  
init_request_timer(O) ->
    O.

-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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

