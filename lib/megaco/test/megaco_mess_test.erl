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
%% Purpose: Verify the implementation of the ITU-T protocol H.248
%%----------------------------------------------------------------------
%% Run the entire test suite with:
%% 
%%    megaco_test_lib:t(megaco_test).
%%    megaco_test_lib:t({megaco_test, all}).
%%    
%% Or parts of it:
%% 
%%    megaco_test_lib:t({megaco_test, accept}).
%%----------------------------------------------------------------------
-module(megaco_mess_test).

% -export([all/1, init_per_testcase/2, fin_per_testcase/2]).
% -export([connect/1,
% 	 request_and_reply/1,
% 	 request_and_no_reply/1,
% 	 pending_ack/1,
% 	 dist/1,

% 	 tickets/1,
% 	 otp_4359/1
% 	]).
-compile(export_all).

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").
-include("megaco_test_lib.hrl").

-define(TEST_VERBOSITY, debug).
-define(MGC_VERBOSITY,  debug).
-define(MG_VERBOSITY,   debug).

-define(MGC_START(Pid, Mid, ET, Verb), 
	megaco_test_mgc:start(Pid, Mid, ET, Verb)).
-define(MGC_STOP(Pid), megaco_test_mgc:stop(Pid)).
-define(MGC_REQ_PEND(Pid,To), megaco_test_mgc:request_pending(Pid,To)).

-define(MG_START(Pid, Mid, Enc, Transp, Conf, Verb), 
	megaco_test_mg:start(Pid, Mid, Enc, Transp, Conf, Verb)).
-define(MG_STOP(Pid), megaco_test_mg:stop(Pid)).
-define(MG_SERV_CHANGE(Pid), megaco_test_mg:service_change(Pid)).
-define(MG_NOTIF_REQ(Pid), megaco_test_mg:notify_request(Pid)).
-define(MG_AWAIT_NOTIF_REP(Pid), megaco_test_mg:await_notify_reply(Pid)).
%% -define(MG_CONN_INFO(Pid,Tag), megaco_test_mg:conn_info(Pid,Tag)).
-define(MG_USER_INFO(Pid,Tag), megaco_test_mg:user_info(Pid,Tag)).

-define(SEND(Expr), 
	?VERIFY(ok, megaco_mess_user_test:apply_proxy(fun() -> Expr end))).

-define(USER(Expected, Reply),
	megaco_mess_user_test:reply(?MODULE,
				    ?LINE,
				    fun(Actual) ->
				       case ?VERIFY(Expected, Actual) of
					   Expected   -> {ok, Reply};
					   UnExpected -> {error, {reply_verify,
								  ?MODULE,
								  ?LINE,
								  UnExpected}}
				       end
				    end)).
	
t()     -> megaco_test_lib:t(?MODULE).
t(Case) -> megaco_test_lib:t({?MODULE, Case}).

%% Test server callbacks
% init_per_testcase(pending_ack = Case, Config) ->
%     put(dbg,true),
%     megaco_test_lib:init_per_testcase(Case, Config);
init_per_testcase(Case, Config) ->
    megaco_test_lib:init_per_testcase(Case, Config).

% fin_per_testcase(pending_ack = Case, Config) ->
%     erase(dbg),
%     megaco_test_lib:fin_per_testcase(Case, Config);
fin_per_testcase(Case, Config) ->
    megaco_test_lib:fin_per_testcase(Case, Config).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all(suite) ->
    [
     connect ,
     request_and_reply,
     request_and_no_reply,
     pending_ack,
     dist,

     %% Tickets last
     tickets
    ].

tickets(suite) ->
    [
     otp_4359
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connect(suite) ->
    [];
connect(doc) ->
    [];
connect(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    PrelMid = preliminary_mid,
    MgMid   = ipv4_mid(4711),

    ?VERIFY(ok, application:start(megaco)),
    ?VERIFY(ok,	megaco:start_user(MgMid, [{send_mod, bad_send_mod},
	                                  {request_timer, infinity},
	                                  {reply_timer, infinity}])),

    MgRH = ?VERIFY(_, megaco:user_info(MgMid, receive_handle)),
    {ok, PrelCH} = ?VERIFY({ok, _}, megaco:connect(MgRH, PrelMid, sh, self())),

    ?VERIFY([PrelCH], megaco:system_info(connections)),
    ?VERIFY([PrelCH], megaco:user_info(MgMid, connections)),
    
    ?VERIFY(bad_send_mod, megaco:user_info(MgMid, send_mod)),
    ?VERIFY(bad_send_mod, megaco:conn_info(PrelCH, send_mod)),
    SC = service_change_request(),
    ?VERIFY({1, {error, {send_message_failed, {'EXIT',
                  {undef, [{bad_send_mod, send_message, [sh, _]} | _]}}}}},
	     megaco:call(PrelCH, [SC], [])),

    ?VERIFY(ok, megaco:disconnect(PrelCH, shutdown)),

    ?VERIFY(ok,	megaco:stop_user(MgMid)),
    ?VERIFY(ok, application:stop(megaco)),
    ?RECEIVE([]),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

request_and_reply(suite) ->
    [];
request_and_reply(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    d("request_and_reply -> start proxy",[]),
    megaco_mess_user_test:start_proxy(),

    PrelMid = preliminary_mid,
    MgMid   = ipv4_mid(4711),
    MgcMid  = ipv4_mid(),
    UserMod = megaco_mess_user_test,
    d("request_and_reply -> start megaco app",[]),
    ?VERIFY(ok, application:start(megaco)),
    UserConfig = [{user_mod, UserMod}, {send_mod, UserMod},
		  {request_timer, infinity}, {reply_timer, infinity}],
    d("request_and_reply -> start (MG) user ~p",[MgMid]),
    ?VERIFY(ok,	megaco:start_user(MgMid, UserConfig)),

    d("request_and_reply -> start (MGC) user ~p",[MgcMid]),
    ?VERIFY(ok,	megaco:start_user(MgcMid, UserConfig)),

    d("request_and_reply -> get receive info for ~p",[MgMid]),
    MgRH = ?VERIFY(_, megaco:user_info(MgMid, receive_handle)),
    d("request_and_reply -> get receive info for ~p",[MgcMid]),
    MgcRH = ?VERIFY(_, megaco:user_info(MgcMid, receive_handle)), 
    d("request_and_reply -> start transport",[]),
    {ok, MgPid, MgSH} =
	?VERIFY({ok, _, _}, UserMod:start_transport(MgRH, MgcRH)),
    PrelMgCH = #megaco_conn_handle{local_mid = MgMid,
				   remote_mid = preliminary_mid},
    MgCH  = #megaco_conn_handle{local_mid = MgMid,
				remote_mid = MgcMid},
    MgcCH = #megaco_conn_handle{local_mid = MgcMid,
				remote_mid = MgMid},
    d("request_and_reply -> (MG) try connect to MGC",[]),
    ?SEND(megaco:connect(MgRH, PrelMid, MgSH, MgPid)), % Mg prel
    d("request_and_reply -> (MGC) await connect from MG",[]),
    ?USER({connect, PrelMgCH, V, []}, ok),
    ?RECEIVE([{res, _, {ok, PrelMgCH}}]),

    d("request_and_reply -> (MG) send service change request",[]),
    Req = service_change_request(),
    ?SEND(megaco:call(PrelMgCH, [Req], [])),

    d("request_and_reply -> (MGC) send service change reply",[]),
    ?USER({connect, MgcCH, V, []}, ok), % Mgc auto
    Rep = service_change_reply(MgcMid),
    ?USER({request, MgcCH, V, [[Req]]}, {discard_ack, [Rep]}),
    ?USER({connect, MgCH, V, []}, ok), % Mg confirm
    ?RECEIVE([{res, _, {1, {ok, [Rep]}}}]),

    d("request_and_reply -> get (system info) connections",[]),
    ?VERIFYL([MgCH, MgcCH], megaco:system_info(connections)),
    d("request_and_reply -> get (~p) connections",[MgMid]),
    ?VERIFY([MgCH], megaco:user_info(MgMid, connections)),
    d("request_and_reply -> get (~p) connections",[MgcMid]),
    ?VERIFY([MgcCH], megaco:user_info(MgcMid, connections)),

    Reason = shutdown,
    d("request_and_reply -> (MG) disconnect",[]),
    ?SEND(megaco:disconnect(MgCH, Reason)),
    ?USER({disconnect, MgCH, V, [{user_disconnect, Reason}]}, ok),
    ?RECEIVE([{res, _, ok}]),
    ?VERIFY(ok,	megaco:stop_user(MgMid)),

    d("request_and_reply -> (MGC) disconnect",[]),
    ?SEND(megaco:disconnect(MgcCH, Reason)),
    ?USER({disconnect, MgcCH, V, [{user_disconnect, Reason}]}, ok),
    ?RECEIVE([{res, _, ok}]),
    ?VERIFY(ok,	megaco:stop_user(MgcMid)),

    d("request_and_reply -> stop megaco app",[]),
    ?VERIFY(ok, application:stop(megaco)),
    ?RECEIVE([]),
    d("request_and_reply -> done",[]),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% OTP-4760
request_and_no_reply(suite) ->
    [];
request_and_no_reply(doc) ->
    [];
request_and_no_reply(Config) when list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        request_and_no_reply),
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
    ReqTmr = #megaco_incr_timer{wait_for    = 3000,
				factor      = 1, 
				incr        = 0,
				max_retries = 2
			       },
    LongReqTmr = #megaco_incr_timer{wait_for    = 10000,
				    factor      = 1, 
				    incr        = 0,
				    max_retries = 3
				   },
    PendingTmr = 10000,
    ReplyTmr = 16000,
    MgConfig = [{request_timer,      ReqTmr}, 
		{long_request_timer, LongReqTmr}, 
		{pending_timer,      PendingTmr},
		{reply_timer,        ReplyTmr}],
    {ok, Mg} = ?MG_START(MgNode, MgMid, text, tcp, MgConfig, ?MG_VERBOSITY),

    d("MG user info: ~p", [?MG_USER_INFO(Mg, all)]),

    i("[MG] connect to the MGC (service change)"),    
    ServChRes = ?MG_SERV_CHANGE(Mg),
    d("service change result: ~p", [ServChRes]),

    d("tell the MGC to ignore requests"),
    ?MGC_REQ_PEND(Mgc, infinity),

    d("[MG] send the notify"),
    ?MG_NOTIF_REQ(Mg),

    d("[MG] await notify reply"),
    {ok, {_Vsn, {error, timeout}}} = ?MG_AWAIT_NOTIF_REP(Mg),

    d("[MG] received expected reply"),

    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pending_ack(suite) ->
    [];
pending_ack(Config) when list(Config) ->

%     MgcNode = make_node_name(mgc),
%     Mg1Node = make_node_name(mg1),

%     d("pending_ack -> create nodes",[]),
%     ok = megaco_test_lib:start_nodes([MgcNode, Mg1Node], ?FILE, ?LINE),

%     d("pending_ack -> start MGC",[]),
%     MgcMid = ipv4_mid(),
%     ET = [{text,tcp}, {text,udp}, {binary,tcp}, {binary,udp}],
%     {ok, Mgc} = ?MGC_START(MgcNode, MgcMid, ET, ?MGC_VERBOSITY),

%     d("pending_ack -> start MG",[]),
%     MgMid  = ipv4_mid(4711),
%     {ok, Mg1} = ?MG_START(Mg1Node, MgMid, text, tcp, ?MG_VERBOSITY),
    
%     d("pending_ack -> (MGC) request action changed to pending",[]),
%     {ok, _} = ?MGC_REQ_PEND(Mgc,3500),

%     d("pending_ack -> (MG) perform service change",[]),
%     ok = ?MG_SERV_CHANGE(Mg1),
    
%     d("pending_ack -> wait some",[]),
%     sleep(1000),

    %% ----------------------------------------------

    ?ACQUIRE_NODES(1, Config),
    d("pending_ack -> start proxy",[]),
    megaco_mess_user_test:start_proxy(),
    
    PrelMid = preliminary_mid,
    MgMid   = ipv4_mid(4711),
    MgcMid  = ipv4_mid(),
    UserMod = megaco_mess_user_test,
    d("pending_ack -> start megaco app",[]),
    ?VERIFY(ok, application:start(megaco)),
    UserData = user_data,
    UserConfig = [{user_mod, UserMod},
		  {send_mod, UserMod},
		  {request_timer, infinity},
		  {long_request_timer, infinity},
		  {reply_timer, infinity}],
    
    d("pending_ack -> start megaco user MG (~p)",[MgMid]),
    ?VERIFY(ok,	megaco:start_user(MgMid, UserConfig)),
    
    d("pending_ack -> start megaco user MGC (~p)",[MgcMid]),
    ?VERIFY(ok,	megaco:start_user(MgcMid, UserConfig)),
    
    d("pending_ack -> retrieve (user info) receive_handle for MG",[]),
    MgRH = ?VERIFY(_, megaco:user_info(MgMid, receive_handle)),
    
    d("pending_ack -> retrieve (user info) receive_handle for MGC",[]),
    MgcRH = ?VERIFY(_, megaco:user_info(MgcMid, receive_handle)), 
    
    d("pending_ack -> start transport",[]),
    {ok, MgPid, MgSH} =
 	?VERIFY({ok, _, _}, UserMod:start_transport(MgRH, MgcRH)),
    PrelMgCH = #megaco_conn_handle{local_mid = MgMid,
				    remote_mid = preliminary_mid},
    MgCH  = #megaco_conn_handle{local_mid = MgMid,
				 remote_mid = MgcMid},
    MgcCH = #megaco_conn_handle{local_mid = MgcMid,
				 remote_mid = MgMid},
    
    d("pending_ack -> (MG) connect",[]),
    ?SEND(megaco:connect(MgRH, PrelMid, MgSH, MgPid)), % Mg prel
    
    d("pending_ack -> (MG) await connect",[]),
    ?USER({connect, PrelMgCH, V, []}, ok),
    
    ?RECEIVE([{res, _, {ok, PrelMgCH}}]),
    
    d("pending_ack -> (MG) send service change request",[]),
    Req = service_change_request(),
    ?VERIFY(ok, megaco:cast(PrelMgCH, [Req], [{reply_data, UserData}])),
    
    d("pending_ack -> (MGC) await connect",[]),
    ?USER({connect, MgcCH, V, []}, ok), % Mgc auto
    
    d("pending_ack -> (MGC) "
       "await service change request (reply with pending)",[]),
    RequestData = Req,
    ?USER({request, MgcCH, V, [[Req]]}, {pending, RequestData}),
    Rep = service_change_reply(MgcMid),
    AckData = ack_data,
    %% BUGBUG: How do we verify that the MG rally gets a pending trans?
    d("pending_ack -> (MGC) await long request (result of pending)",[]),
    ?USER({long_request, MgcCH, V, [RequestData]}, {{handle_ack, AckData}, [Rep]}),
    d("pending_ack -> (MG) await connect",[]),
    ?USER({connect, MgCH, V, []}, ok), % Mg confirm

    %% **************************************************
    %% The following 2 USER calls should really happen
    %% in the order reply - ack, but due to timing, could
    %% happen in ack - reply.
    %% 
    %% START
    %% 
    d("pending_ack -> (MG) await service change reply",[]),
    ?USER({reply, MgCH, V, [{ok, [Rep]}, UserData]}, ok),

    d("pending_ack -> (MGC) await service change ack",[]),
    ?USER({ack, MgcCH, V, [ok, AckData]}, ok),

    %% 
    %% END
    %% 
    %% **************************************************


    ?VERIFYL([MgCH, MgcCH], megaco:system_info(connections)),
    ?VERIFY([MgCH], megaco:user_info(MgMid, connections)),
    ?VERIFY([MgcCH], megaco:user_info(MgcMid, connections)),
    
    Reason = shutdown,
    ?SEND(application:stop(megaco)),
    ?RECEIVE([{res, _, ok}]),
    d("pending_ack -> done",[]),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dist(suite) ->
    [];
dist(Config) when list(Config) ->
    [Local, Dist] = ?ACQUIRE_NODES(2, Config),
    d("dist -> start proxy",[]),
    megaco_mess_user_test:start_proxy(),

    PrelMid = preliminary_mid,
    MgMid   = ipv4_mid(4711),
    MgcMid  = ipv4_mid(),
    UserMod = megaco_mess_user_test,
    d("dist -> start megaco app",[]),
    ?VERIFY(ok, application:start(megaco)),
    UserConfig = [{user_mod, UserMod}, {send_mod, UserMod},
		  {request_timer, infinity}, {reply_timer, infinity}],

    d("dist -> start megaco user MG (~p)",[MgMid]),
    ?VERIFY(ok,	megaco:start_user(MgMid, UserConfig)),

    d("dist -> start megaco user MGC (~p)",[MgcMid]),
    ?VERIFY(ok,	megaco:start_user(MgcMid, UserConfig)),

    d("dist -> retrieve (user info) receive_handle for MG",[]),
    MgRH = ?VERIFY(_, megaco:user_info(MgMid, receive_handle)),

    d("dist -> retrieve (user info) receive_handle for MGC",[]),
    MgcRH = ?VERIFY(_, megaco:user_info(MgcMid, receive_handle)), 

    d("dist -> start transport",[]),
    {ok, MgPid, MgSH} =
	?VERIFY({ok, _, _}, UserMod:start_transport(MgRH, MgcRH)),
    PrelMgCH = #megaco_conn_handle{local_mid = MgMid,
				   remote_mid = preliminary_mid},
    MgCH  = #megaco_conn_handle{local_mid = MgMid,
				remote_mid = MgcMid},
    MgcCH = #megaco_conn_handle{local_mid = MgcMid,
				remote_mid = MgMid},

    d("dist -> (MG) connect",[]),
    ?SEND(megaco:connect(MgRH, PrelMid, MgSH, MgPid)), % Mg prel

    d("dist -> (MG) await connect",[]),
    ?USER({connect, PrelMgCH, V, []}, ok),
    ?RECEIVE([{res, _, {ok, PrelMgCH}}]),

    d("dist -> (MG) send service change request",[]),
    Req = service_change_request(),
    ?SEND(megaco:call(PrelMgCH, [Req], [])),

    d("dist -> (MGC) await auto-connect",[]),
    ?USER({connect, MgcCH, V, []}, ok), % Mgc auto


    Rep = service_change_reply(MgcMid),
    d("dist -> (MGC) "
      "await service change request and send reply when received",[]),
    ?USER({request, MgcCH, V, [[Req]]}, {discard_ack, [Rep]}),

    d("dist -> (MG) await connect",[]),
    ?USER({connect, MgCH, V, []}, ok), % Mg confirm

    d("dist -> (MG) await service change reply",[]),
    ?RECEIVE([{res, _, {1, {ok, [Rep]}}}]),

    %% Dist
    d("dist -> start megaco on ~p", [Dist]),
    ?VERIFY(ok,	rpc:call(Dist, megaco, start, [])),

    d("dist -> start megaco user on ~p", [Dist]),
    ?VERIFY(ok,	rpc:call(Dist, megaco, start_user, [MgcMid, UserConfig])),

    d("dist -> (MG) connect to MGC", []),
    MgcPid = self(),
    MgcSH = {element(2, MgSH), element(1, MgSH)},
    ?SEND(rpc:call(Dist, megaco, connect, [MgcRH, MgMid, MgcSH, MgcPid])), % Mgc dist

    d("dist -> (MGC) await auto-connect (from MG on ~p)", [Dist]),
    ?USER({connect, MgcCH, V, []}, ok), % Mgc dist auto
    ?RECEIVE([{res, _, {ok, MgcCH}}]),

    d("dist -> (~p:MG) send service change request",[Dist]),
    ?SEND(rpc:call(Dist, megaco, call, [MgcCH, [Req], []])),

    d("dist -> (MG????????) "
      "await service change request and send reply when received",[]),
    ?USER({request, MgCH, V, [[Req]]}, {discard_ack, [Rep]}),
    ?RECEIVE([{res, _, {1, {ok, [Rep]}}}]),

    d("dist -> retreive some info",[]),
    ?VERIFYL([MgCH, MgcCH], megaco:system_info(connections)),
    ?VERIFY([MgCH], megaco:user_info(MgMid, connections)),
    ?VERIFY([MgcCH], megaco:user_info(MgcMid, connections)),

    ?VERIFY([MgcCH], rpc:call(Dist, megaco, system_info, [connections])),
    ?VERIFY([], rpc:call(Dist, megaco, user_info, [MgMid, connections])),
    ?VERIFY([MgcCH], rpc:call(Dist, megaco, user_info, [MgcMid, connections])),

    %% Shutdown

    d("dist -> close down the shop...",[]),
    Reason = shutdown,
    ?SEND(megaco:disconnect(MgCH, Reason)),
    ?USER({disconnect, MgCH, V, [{user_disconnect, Reason}]}, ok),
    ?RECEIVE([{res, _, ok}]),
    ?VERIFY(ok,	megaco:stop_user(MgMid)),

    ?SEND(megaco:disconnect(MgcCH, Reason)),
    ?USER({disconnect, MgcCH, V, [{user_disconnect, Reason}]}, ok),
    ?USER({disconnect, MgcCH, V, [{user_disconnect, Reason}]}, ok),
    ?RECEIVE([{res, _, ok}]),
    ?VERIFY(ok,	megaco:stop_user(MgcMid)),

    ?VERIFY(ok, application:stop(megaco)),
    ?RECEIVE([]),
    d("dist -> done",[]),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_4359(suite) ->
    [];
otp_4359(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Mid = {deviceName, "dummy_mid"},

    io:format("otp_4359 -> start megaco application~n", []),
    ?VERIFY(ok, application:start(megaco)),

    io:format("otp_4359 -> start and configure megaco user~n", []),
    ?VERIFY(ok,	megaco:start_user(Mid, [{send_mod, ?MODULE},
					{request_timer, infinity},
					{reply_timer, infinity}])),

    ?VERIFY(ok, megaco:update_user_info(Mid, user_mod,  ?MODULE)),
    ?VERIFY(ok, megaco:update_user_info(Mid, user_args, [self()])),
    RH0 = ?VERIFY(_,  megaco:user_info(Mid, receive_handle)),
    RH1 = RH0#megaco_receive_handle{send_mod        = ?MODULE,
				    encoding_mod    = megaco_compact_text_encoder,
				    encoding_config = []},

    %% First an erroneous transaction (missing the transaction id number)
    %% then an valid transaction.
    M = "!/1 ml2 "
	"T={C=${A=${M{O {MO=SR,RG=OFF,RV=OFF}}}}}"
	"T=1{C=${A=${M{O {MO=SR,RG=OFF,RV=OFF}}}}}",

    %% Simulate incomming message
    %% Will result in an (auto) connect first
    io:format("otp_4359 -> simulate receive message~n", []),
    megaco:receive_message(RH1, self(), self(), list_to_binary(M)),
    io:format("otp_4359 -> await actions~n", []),
    Actions = otp_4359_await_actions([{handle_connect, ignore}, 
				      {send_message, ?megaco_bad_request}, 
				      {handle_trans_request, ignore}, 
				      {send_message, ?megaco_not_implemented}]), 
    io:format("otp_4359 -> analyze actions~n", []),
    otp_4359_analyze_result(RH1, Actions),

    Conns = megaco:system_info(connections),
    OKs   = lists:duplicate(length(Conns),ok),
    ?VERIFY(OKs, [megaco:disconnect(CH, test_complete) || CH <- Conns]),
    ?VERIFY(_,	megaco:stop_user(Mid)),
    ?VERIFY(ok, application:stop(megaco)),
    ?RECEIVE([]),
    ok.


otp_4359_await_actions(Exp) ->
    otp_4359_await_actions(Exp, []).

otp_4359_await_actions([], Rep) ->
    lists:reverse(Rep);
otp_4359_await_actions([{M,I}|R] = All, Rep) ->
    receive
	{M, Info} ->
	    otp_4359_await_actions(R, [{M, I, Info}|Rep]);
	Else ->
	    exit({received_unexpected_message, M, Else})
	    %% io:format("received unexpected: ~p~n", [Else]),
	    %% otp_4359_await_actions(All, Rep)
    after 10000 ->
	    exit(timeout)
    end.

otp_4359_analyze_result(RH, []) ->
    ok;
otp_4359_analyze_result(RH, [{send_message, ExpErrorCode, EncodedMessage}|L]) ->
    io:format("otp_4359_analyze_result -> send_message: ", []),
    otp_4359_analyze_encoded_message(RH, ExpErrorCode, EncodedMessage),
    otp_4359_analyze_result(RH,L);
otp_4359_analyze_result(RH, [{M,ignore,_}|T]) ->
    io:format("otp_4359_analyze_result -> ignoring ~p~n", [M]),
    otp_4359_analyze_result(RH,T).

otp_4359_analyze_encoded_message(RH, ExpErrorCode, M) 
  when record(RH, megaco_receive_handle), binary(M) ->
    #megaco_receive_handle{encoding_mod    = Mod,
			   encoding_config = Conf} = RH,
    case (catch Mod:decode_message(Conf, M)) of
	{ok, #'MegacoMessage'{mess = #'Message'{messageBody = Body}}} ->
	    case Body of
		{transactions, [{transactionReply,Reply}]} ->
		    case Reply of
			#'TransactionReply'{transactionResult = Result} ->
			    case Result of
				{transactionError,ED} when record(ED, 'ErrorDescriptor') ->
				    case ED#'ErrorDescriptor'.errorCode of
					ExpErrorCode ->
					    io:format("error code ~p ok~n", [ExpErrorCode]),
					    ok;
					Code ->
					    io:format("error code ~p erroneous~n", [Code]),
					    exit({unexpected_error_code, ExpErrorCode, Code})
				    end;
				_ ->
				    io:format("unexpected trans result~n", []),
				    exit({unexpected_trans_result, Result})
			    end;
			_ ->
			    io:format("unexpected trans reply~n", []),
			    exit({unexpected_trans_reply, Reply})
		    end;
		_ ->
		    io:format("unexpected body~n", []),
		    exit({unexpected_body, Body})
	    end;

	Else ->
	    io:format("unexpected decode result~n", []),
	    exit({unexpected_decode_result, Else})
    end.
	    
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

service_change_request() ->
    Parm = #'ServiceChangeParm'{serviceChangeMethod = restart,
				serviceChangeReason = [?megaco_cold_boot]},
    SCR = #'ServiceChangeRequest'{terminationID = [?megaco_root_termination_id],
				  serviceChangeParms = Parm},
    CR = #'CommandRequest'{command = {serviceChangeReq, SCR}},
    #'ActionRequest'{contextId = ?megaco_null_context_id,
		     commandRequests = [CR]}.

service_change_reply(MgcMid) ->
    Res = {serviceChangeResParms, #'ServiceChangeResParm'{serviceChangeMgcId = MgcMid}},
    SCR = #'ServiceChangeReply'{terminationID = [?megaco_root_termination_id],
				serviceChangeResult = Res},
    #'ActionReply'{contextId = ?megaco_null_context_id,
		   commandReply = [{serviceChangeReply, SCR}]}.

local_ip_address() ->
    {ok, Hostname} = inet:gethostname(),
    {ok, {A1, A2, A3, A4}} = inet:getaddr(Hostname, inet),
    {A1, A2, A3, A4}.

ipv4_mid() ->
    ipv4_mid(asn1_NOVALUE).

ipv4_mid(Port) ->
    IpAddr = local_ip_address(),
    Ip = tuple_to_list(IpAddr),
    {ip4Address, #'IP4Address'{address = Ip, portNumber = Port}}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% -------------------------------------------------------------------------------
%%% Megaco user callback interface
%%% -------------------------------------------------------------------------------

handle_connect(ConnHandle, ProtocolVersion, Pid) ->
    Pid ! {handle_connect, {ConnHandle, ProtocolVersion}},
    ok.

handle_disconnect(_, _, {user_disconnect, test_complete}, _) ->
    ok;
handle_disconnect(ConnHandle, ProtocolVersion, Reason, Pid) ->
    Pid ! {handle_disconnect, {ConnHandle, ProtocolVersion, Reason}},
    ok.

handle_syntax_error(ConnHandle, ProtocolVersion, ErrorDescriptor, Pid) ->
    Pid ! {handle_syntax_error,{ConnHandle, ProtocolVersion, ErrorDescriptor}},
    reply.
         
handle_message_error(ConnHandle, ProtocolVersion, ErrorDescriptor, Pid) ->
    Pid ! {handle_message_error,{ConnHandle, ProtocolVersion, ErrorDescriptor}},
    reply.

handle_trans_request(ConnHandle, ProtocolVersion, ActionRequests, Pid) ->
    Pid ! {handle_trans_request,{ConnHandle, ProtocolVersion, ActionRequests}},
    ED = #'ErrorDescriptor'{errorCode = ?megaco_not_implemented,
			    errorText = "not implemented yet"},
    {discard_ack, ED}.

handle_trans_long_request(ConnHandle, ProtocolVersion, Data, Pid) ->
    Pid ! {handle_trans_long_request,{ConnHandle, ProtocolVersion, Data}},
    ED = #'ErrorDescriptor'{errorCode = ?megaco_not_implemented,
			    errorText = "not implemented yet"},
    {discard_ack, ED}.

handle_trans_reply(ConnHandle, ProtocolVersion, ActualReply, Data, Pid) ->
    Pid ! {handle_trans_reply,{ConnHandle, ProtocolVersion, ActualReply, Data}},
    ok.

handle_trans_ack(ConnHandle, ProtocolVersion, Status, Data, Pid) ->
    Pid ! {handle_trans_ack,{ConnHandle, ProtocolVersion, Status, Data}},
    ok.





%%% -------------------------------------------------------------------------------
%%% Megaco transport module interface
%%% -------------------------------------------------------------------------------

send_message(Pid, Data) ->
    Pid ! {send_message, Data},
    ok.

% block(Pid) ->
%     Pid ! {block, dummy},
%     ok.

unblock(Pid) ->
    Pid ! {unblock, dummy},
    ok.

% close(Pid) ->
%     Pid ! {close, dummy},
%     ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_node_name(Name) ->
    case string:tokens(atom_to_list(node()), [$@]) of
	[_,Host] ->
	    list_to_atom(lists:concat([atom_to_list(Name) ++ "@" ++ Host]));
	_ ->
	    exit("Test node must be started with '-sname'")
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sleep(X) -> receive after X -> ok end.

% error_msg(F,A) -> error_logger:error_msg(F ++ "~n",A).


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

format_timestamp(Now) ->
    {N1, N2, N3}   = Now,
    {Date, Time}   = calendar:now_to_datetime(Now),
    {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    FormatDate = 
        io_lib:format("~.4w:~.2.0w:~.2.0w ~.2.0w:~.2.0w:~.2.0w 4~w",
                      [YYYY,MM,DD,Hour,Min,Sec,round(N3/1000)]),  
    lists:flatten(FormatDate).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

random_init() ->
    {A,B,C} = now(),
    random:seed(A,B,C).

random() ->
    10 * random:uniform(50).

