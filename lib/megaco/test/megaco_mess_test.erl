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

%% -export([all/1, init_per_testcase/2, fin_per_testcase/2]).
%% -export([connect/1,
	 
%% 	 request_and_reply/1,
%% 	 request_and_reply_plain/1,
%% 	 request_and_no_reply/1,
%% 	 request_and_reply_pending_ack_no_pending/1,
%% 	 request_and_reply_pending_ack_one_pending/1,
%% 	 single_trans_req_and_reply/1,
%% 	 single_trans_req_and_reply_sendopts/1,
%% 	 request_and_reply_and_ack/1,
%% 	 request_and_reply_and_no_ack/1,
%% 	 request_and_reply_and_late_ack/1,

%% 	 pending_ack/1,

%% 	 dist/1,

%% 	 tickets/1,
%% 	 otp_4359/1,
%% 	 otp_4836/1,
%% 	 otp_5805/1,
%% 	 otp_5881/1,
%% 	 otp_5887/1
%% 	]).
-compile(export_all).

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").
-include("megaco_test_lib.hrl").

-define(VERSION, 1).

-define(TEST_VERBOSITY, debug).
-define(MGC_VERBOSITY,  debug).
-define(MG_VERBOSITY,   debug).

-define(MGC_START(Pid, Mid, ET, Conf, Verb), 
	megaco_test_mgc:start(Pid, Mid, ET, Conf, Verb)).
-define(MGC_STOP(Pid),        megaco_test_mgc:stop(Pid)).
-define(MGC_REQ_PEND(Pid,To), megaco_test_mgc:request_pending(Pid,To)).
-define(MGC_REQ_HP(Pid,To),   megaco_test_mgc:request_handle_pending(Pid,To)).
-define(MGC_ACK_INFO(Pid),    megaco_test_mgc:ack_info(Pid,self())).

-define(MG_START(Pid, Mid, Enc, Transp, Conf, Verb), 
	megaco_test_mg:start(Pid, Mid, Enc, Transp, Conf, Verb)).
-define(MG_STOP(Pid), megaco_test_mg:stop(Pid)).
-define(MG_SERV_CHANGE(Pid), megaco_test_mg:service_change(Pid)).
-define(MG_NOTIF_REQ(Pid), megaco_test_mg:notify_request(Pid)).
-define(MG_AWAIT_NOTIF_REP(Pid), megaco_test_mg:await_notify_reply(Pid)).
-define(MG_CONN_INFO(Pid,Tag), megaco_test_mg:conn_info(Pid,Tag)).
-define(MG_USER_INFO(Pid,Tag), megaco_test_mg:user_info(Pid,Tag)).
-define(MG_NOTIF_RAR(Pid), megaco_test_mg:notify_request_and_reply(Pid)).

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


min(M) -> timer:minutes(M).

%% Test server callbacks
% init_per_testcase(pending_ack = Case, Config) ->
%     put(dbg,true),
%     megaco_test_lib:init_per_testcase(Case, Config);
init_per_testcase(request_and_no_reply = Case, Config) ->
    C = lists:keydelete(tc_timeout, 1, Config),
    megaco_test_lib:init_per_testcase(Case, [{tc_timeout, min(2)} |C]);
init_per_testcase(Case, Config) ->
    C = lists:keydelete(tc_timeout, 1, Config),
    megaco_test_lib:init_per_testcase(Case, [{tc_timeout, min(1)} |C]).

% fin_per_testcase(pending_ack = Case, Config) ->
%     erase(dbg),
%     megaco_test_lib:fin_per_testcase(Case, Config);
fin_per_testcase(Case, Config) ->
    megaco_test_lib:fin_per_testcase(Case, Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all(suite) ->
    [
     connect,
     request_and_reply,
     pending_ack,
     dist,

     %% Tickets last
     tickets
    ].

request_and_reply(suite) ->
    [
     request_and_reply_plain,
     request_and_no_reply,
     request_and_reply_pending_ack_no_pending,
     request_and_reply_pending_ack_one_pending,
     single_trans_req_and_reply,
     single_trans_req_and_reply_sendopts,
     request_and_reply_and_ack,
     request_and_reply_and_no_ack,
     request_and_reply_and_late_ack
    ].

pending_ack(suite) ->
    [
     pending_ack_plain,
     request_and_pending_and_late_reply
    ].

tickets(suite) ->
    [
     otp_4359,
     otp_4836,
     otp_5805,
     otp_5881,
     otp_5887,
     otp_6253
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connect(suite) ->
    [];
connect(doc) ->
    [];
connect(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    PrelMid = preliminary_mid,
    MgMid   = ipv4_mid(4711),

    ?VERIFY(ok, application:start(megaco)),
    ?VERIFY(ok,	megaco:start_user(MgMid, [{send_mod, bad_send_mod},
	                                  {request_timer, infinity},
	                                  {reply_timer, infinity}])),

    MgRH = user_info(MgMid, receive_handle),
    {ok, PrelCH} = ?VERIFY({ok, _}, megaco:connect(MgRH, PrelMid, sh, self())),

    connections([PrelCH]),
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

request_and_reply_plain(suite) ->
    [];
request_and_reply_plain(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    d("request_and_reply_plain -> start proxy",[]),
    megaco_mess_user_test:start_proxy(),

    PrelMid = preliminary_mid,
    MgMid   = ipv4_mid(4711),
    MgcMid  = ipv4_mid(),
    UserMod = megaco_mess_user_test,
    d("request_and_reply_plain -> start megaco app",[]),
    ?VERIFY(ok, application:start(megaco)),
    UserConfig = [{user_mod, UserMod}, {send_mod, UserMod},
		  {request_timer, infinity}, {reply_timer, infinity}],
    d("request_and_reply_plain -> start (MG) user ~p",[MgMid]),
    ?VERIFY(ok,	megaco:start_user(MgMid, UserConfig)),

    d("request_and_reply_plain -> start (MGC) user ~p",[MgcMid]),
    ?VERIFY(ok,	megaco:start_user(MgcMid, UserConfig)),

    d("request_and_reply_plain -> get receive info for ~p",[MgMid]),
    MgRH = user_info(MgMid, receive_handle),
    d("request_and_reply_plain -> get receive info for ~p",[MgcMid]),
    MgcRH = user_info(MgcMid, receive_handle), 
    d("request_and_reply_plain -> start transport",[]),
    {ok, MgPid, MgSH} =
	?VERIFY({ok, _, _}, UserMod:start_transport(MgRH, MgcRH)),
    PrelMgCH = #megaco_conn_handle{local_mid = MgMid,
				   remote_mid = preliminary_mid},
    MgCH  = #megaco_conn_handle{local_mid = MgMid,
				remote_mid = MgcMid},
    MgcCH = #megaco_conn_handle{local_mid = MgcMid,
				remote_mid = MgMid},
    d("request_and_reply_plain -> (MG) try connect to MGC",[]),
    ?SEND(megaco:connect(MgRH, PrelMid, MgSH, MgPid)), % Mg prel
    d("request_and_reply_plain -> (MGC) await connect from MG",[]),
    ?USER({connect, PrelMgCH, _V, []}, ok),
    ?RECEIVE([{res, _, {ok, PrelMgCH}}]),

    d("request_and_reply_plain -> (MG) send service change request",[]),
    Req = service_change_request(),
    ?SEND(megaco:call(PrelMgCH, [Req], [])),

    d("request_and_reply_plain -> (MGC) send service change reply",[]),
    ?USER({connect, MgcCH, _V, []}, ok), % Mgc auto
    Rep = service_change_reply(MgcMid),
    ?USER({request, MgcCH, _V, [[Req]]}, {discard_ack, [Rep]}),
    ?USER({connect, MgCH, _V, []}, ok), % Mg confirm
    ?RECEIVE([{res, _, {1, {ok, [Rep]}}}]),

    d("request_and_reply_plain -> get (system info) connections",[]),
    connections([MgCH, MgcCH]),
    d("request_and_reply_plain -> get (~p) connections",[MgMid]),
    ?VERIFY([MgCH], megaco:user_info(MgMid, connections)),
    d("request_and_reply_plain -> get (~p) connections",[MgcMid]),
    ?VERIFY([MgcCH], megaco:user_info(MgcMid, connections)),

    Reason = shutdown,
    d("request_and_reply_plain -> (MG) disconnect",[]),
    ?SEND(megaco:disconnect(MgCH, Reason)),
    ?USER({disconnect, MgCH, _V, [{user_disconnect, Reason}]}, ok),
    ?RECEIVE([{res, _, ok}]),
    ?VERIFY(ok,	megaco:stop_user(MgMid)),

    d("request_and_reply_plain -> (MGC) disconnect",[]),
    ?SEND(megaco:disconnect(MgcCH, Reason)),
    ?USER({disconnect, MgcCH, _V, [{user_disconnect, Reason}]}, ok),
    ?RECEIVE([{res, _, ok}]),
    ?VERIFY(ok,	megaco:stop_user(MgcMid)),

    d("request_and_reply_plain -> stop megaco app",[]),
    ?VERIFY(ok, application:stop(megaco)),
    ?RECEIVE([]),
    d("request_and_reply_plain -> done",[]),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% OTP-4760
request_and_no_reply(suite) ->
    [];
request_and_no_reply(doc) ->
    [];
request_and_no_reply(Config) when is_list(Config) ->
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

    %% Start the MGC
    i("[MGC] start"),    
    ET = [{text,tcp}, {text,udp}, {binary,tcp}, {binary,udp}],
    {ok, Mgc} = 
	?MGC_START(MgcNode, {deviceName, "ctrl"}, ET, [], ?MGC_VERBOSITY),

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
    %% Start the MGs
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

request_and_reply_pending_ack_no_pending(suite) ->
    [];
request_and_reply_pending_ack_no_pending(doc) ->
    ["This test case tests that megaco correctly handles the return "
     "value handle_pending_ack from handle_trans_request when NO "
     "pending message has been sent"];
request_and_reply_pending_ack_no_pending(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        rar_panp),
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
    MgcEvSeq = rarpanp_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    megaco_test_generator:megaco(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = rarpanp_mg_event_sequence(text, tcp),

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

rarpanp_mgc_event_sequence(text, tcp) ->
    Mid = {deviceName,"ctrl"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    ConnectVerify = fun rarpanp_mgc_verify_handle_connect/1,
    ScrVerify = rarpanp_mgc_verify_service_change_req_fun(Mid),
    NrVerify = rarpanp_mgc_verify_notify_request_fun(),
    DiscoVerify = fun rarpanp_mgc_verify_handle_disconnect/1,
    EvSeq = [
             {debug, true},
             {megaco_trace, disable},
             {megaco_trace, max},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
             {megaco_update_user_info, sent_pending_limit, 100},
             start_transport,
             listen,
             {megaco_callback, handle_connect,       ConnectVerify},
             {megaco_conn_info, all},
             {megaco_callback, handle_trans_request, ScrVerify},
	     {megaco_callback, handle_trans_request, NrVerify},
             {megaco_callback, nocall, 10000},
             {megaco_callback, handle_disconnect,    DiscoVerify},
             megaco_stop_user,
             megaco_stop
            ],
    EvSeq.

%% Connect verification
rarpanp_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) ->
    {ok, CH, ok};
rarpanp_mgc_verify_handle_connect(Else) ->
    {error, Else, ok}.

%% Service Change verification
rarpanp_mgc_verify_service_change_req_fun(Mid) ->
    fun(Req) -> rarpanp_mgc_verify_service_change_req(Mid, Req) end.

rarpanp_mgc_verify_service_change_req(
  Mid, 
  {handle_trans_request, _, ?VERSION, [AR]}) ->
    (catch rarpanp_mgc_do_verify_service_change_req(Mid, AR));
rarpanp_mgc_verify_service_change_req(_Mid, Crap) ->
    ED = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

rarpanp_mgc_do_verify_service_change_req(Mid, AR) ->
    io:format("rarpanp_mgc_do_verify_service_change_req -> entry with"
	      "~n   Mid: ~p"
	      "~n   AR:  ~p"
	      "~n", [Mid, AR]),
    CR = 
	case AR of
	    #'ActionRequest'{commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		Err1      = {invalid_action_request, AR},
		ED1       = cre_ErrDesc(AR),
		ErrReply1 = {discard_ack, ED1},
		throw({error, Err1, ErrReply1})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		Err2      = {invalid_command_request, CR},
		ED2       = cre_ErrDesc(CR),
		ErrReply2 = {discard_ack, ED2},
		throw({error, Err2, ErrReply2})
	end,
    {Tid, Parms} = 
	case Cmd of
	    {serviceChangeReq, 
	     #'ServiceChangeRequest'{terminationID = [TermID],
				     serviceChangeParms = ServChParms}} ->
		{TermID, ServChParms};
	    _ ->
		Err3      = {invalid_command, Cmd},
		ED3       = cre_ErrDesc(Cmd),
		ErrReply3 = {discard_ack, ED3},
		throw({error, Err3, ErrReply3})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Err4      = {invalid_termination_id, Tid},
	    ED4       = cre_ErrDesc(Tid),
	    ErrReply4 = {discard_ack, ED4},
	    throw({error, Err4, ErrReply4})
    end,
    case Parms of
	#'ServiceChangeParm'{serviceChangeMethod = restart,
			     serviceChangeReason = [[$9,$0,$1|_]]} ->
	    AckData = [rarpanp_mgc_service_change_reply_ar(Mid, 1)], 
	    Reply   = {discard_ack, AckData},
	    {ok, AR, Reply};
	_ ->
	    Err5      = {invalid_SCP, Parms},
	    ED5       = cre_ErrDesc(Parms),
	    ErrReply5 = {discard_ack, ED5},
	    {error, Err5, ErrReply5}
    end.


%% Notify Request verification
rarpanp_mgc_verify_notify_request_fun() ->
    fun(Req) -> rarpanp_mgc_verify_notify_request(Req) end.

rarpanp_mgc_verify_notify_request({handle_trans_request, _, ?VERSION, [AR]}) ->
    (catch rarpanp_mgc_do_verify_notify_request(AR));
rarpanp_mgc_verify_notify_request(Crap) ->
    ED = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

rarpanp_mgc_do_verify_notify_request(AR) ->
    io:format("rarpanp_mgc_do_verify_notify_request -> entry with"
	      "~n   AR: ~p"
	      "~n", [AR]),
    {Cid, CR} = 
	case AR of
	    #'ActionRequest'{contextId       = CtxID,
			     commandRequests = [CmdReq]} ->
		{CtxID, CmdReq};
	    _ ->
		Err1      = {invalid_action_request, AR},
		ED1       = cre_ErrDesc(AR),
		ErrReply1 = {discard_ack, ED1},
		throw({error, Err1, ErrReply1})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command;
	    _ ->
		Err2      = {invalid_command_request, CR},
		ED2       = cre_ErrDesc(CR),
		ErrReply2 = {discard_ack, ED2},
		throw({error, Err2, ErrReply2})
	end,
    NR = 
	case Cmd of
	    {notifyReq, NotifReq} ->
		NotifReq;
	    _ ->
		Err3      = {invalid_command, Cmd},
		ED3       = cre_ErrDesc(Cmd),
		ErrReply3 = {discard_ack, ED3},
		throw({error, Err3, ErrReply3})
	end,
    {Tid, OED} = 
	case NR of
	    #'NotifyRequest'{terminationID            = [TermID],
			     observedEventsDescriptor = ObsEvsDesc,
			     errorDescriptor          = asn1_NOVALUE} ->
		{TermID, ObsEvsDesc};
	    _ ->
		Err4      = {invalid_NR, NR},
		ED4       = cre_ErrDesc(NR),
		ErrReply4 = {discard_ack, ED4},
		throw({error, Err4, ErrReply4})
	end,
    OE = 
	case OED of 
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEvLst]} ->
		ObsEvLst;
	    _ ->
		Err5      = {invalid_OED, OED},
		ED5       = cre_ErrDesc(NR),
		ErrReply5 = {discard_ack, ED5},
		throw({error, Err5, ErrReply5})
	end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
	    AckData = notify_request_verified, 
	    Replies = [rarpanp_mgc_notify_reply_ar(Cid, Tid)],
	    Reply   = {{handle_pending_ack, AckData}, Replies},
	    {ok, AR, Reply};
	_ ->
	    Err6      = {invalid_OE, OE},
	    ED6       = cre_ErrDesc(OE),
	    ErrReply6 = {discard_ack, ED6},
	    throw({error, Err6, ErrReply6})
    end.


%% Disconnect verification
rarpanp_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, _R}) ->
    {ok, CH, ok};
rarpanp_mgc_verify_handle_disconnect(Else) ->
    {error, Else, ok}.

rarpanp_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    AR    = cre_actionReply(Cid, [CR]),
    AR.

rarpanp_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).


%%
%% MG generator stuff
%%
rarpanp_mg_event_sequence(text, tcp) ->
    DecodeFun = rarpanp_mg_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = rarpanp_mg_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid       = {deviceName,"mg"},
    ServiceChangeReq = rarpanp_mg_service_change_request_msg(Mid, 1, 0),
    ScrVerifyFun = rarpanp_mg_verify_service_change_rep_msg_fun(),
    TransId = 2, 
    TermId = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NotifyReq = rarpanp_mg_notify_request_msg(Mid, TransId, 1, TermId, 1),
    NrVerifyFun = rarpanp_mg_verify_notify_rep_msg_fun(TransId, TermId),
    EvSeq = [{debug,  true},
             {decode, DecodeFun},
             {encode, EncodeFun},
             {connect, 2944},
             {send, "service-change-request", ServiceChangeReq},
             {expect_receive, "service-change-reply", {ScrVerifyFun, 10000}},
             {send, "notify request", NotifyReq},
             {expect_receive, "notify-reply", {NrVerifyFun, 10000}},
             {expect_nothing, 11000},
             disconnect
            ],
    EvSeq.

rarpanp_mg_encode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:encode_message(Conf, M)
    end.

rarpanp_mg_decode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:decode_message(Conf, M)
    end.

rarpanp_mg_verify_service_change_rep_msg_fun() ->
    fun(Msg) -> (catch rarpanp_mg_verify_service_change_rep(Msg)) end.

rarpanp_mg_verify_service_change_rep(#'MegacoMessage'{mess = Mess} = M) ->
    io:format("rarpanp_mg_verify_service_change_rep -> entry with"
	      "~n   Mess:  ~p"
	      "~n", [Mess]),
    Body = 
	case Mess of 
	    #'Message'{version     = _V,
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId = _Tid,
                                immAckRequired = asn1_NOVALUE,
                                transactionResult = TransRes} ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
            {actionReplies, [ActRes]} ->
		ActRes;
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
            #'ActionReply'{contextId       = _Cid,
                           errorDescriptor = asn1_NOVALUE,
                           contextReply    = _CtxReq,
                           commandReply    = [CmdRep]} ->
		CmdRep;
	    _ ->
		throw({error, {invalid_actionReplies, AR}})
	end,
    SCR = 
	case CR of
            {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    SCRes = 
	case SCR of
            #'ServiceChangeReply'{terminationID       = _TermID,
                                  serviceChangeResult = ServChRes} ->
		ServChRes;
	    _ ->
		throw({error, {invalid_serviceChangeReply, SCR}})
	end,
    SCRP = 
	case SCRes of
            {serviceChangeResParms, Parms} ->
		Parms;
	    _ ->
		throw({error, {invalid_serviceChangeResult, SCRes}})
	end,
    case SCRP of
	#'ServiceChangeResParm'{serviceChangeMgcId = _MgcMid} ->
            {ok, M};
	_ ->
	    {error, {invalid_serviceChangeResParms, SCRP}}
    end;
rarpanp_mg_verify_service_change_rep(Crap) ->
    {error, {invalid_message, Crap}}.

rarpanp_mg_verify_notify_rep_msg_fun(TransId, TermId) ->
    fun(Msg) -> (catch rarpanp_mg_verify_notify_rep(TransId, TermId, Msg)) end.

rarpanp_mg_verify_notify_rep(TransId, TermId, 
			     #'MegacoMessage'{mess = Mess} = M) ->
    io:format("rarpanp_mg_verify_notify_rep -> entry with"
	      "~n   TransId: ~p"
	      "~n   TermId:  ~p"
	      "~n   Mess:    ~p"
	      "~n", [TransId, TermId, Mess]),
    Body = 
	case Mess of 
	    #'Message'{version     = _V,
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId     = TransId,
                                immAckRequired    = asn1_NOVALUE, % No ack
                                transactionResult = TransRes} ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
            {actionReplies, [ActRes]} ->
		ActRes;
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
            #'ActionReply'{contextId       = _Cid,
                           errorDescriptor = asn1_NOVALUE,
                           contextReply    = _CtxReq,
                           commandReply    = [CmdRep]} ->
		CmdRep;
	    _ ->
		throw({error, {invalid_actionReplies, AR}})
	end,
    NR = 
	case CR of
            {notifyReply, NotifyRep} ->
		NotifyRep;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,

    case NR of
	#'NotifyReply'{terminationID   = [TermId],
		       errorDescriptor = asn1_NOVALUE} ->
            {ok, M};
	#'NotifyReply'{terminationID   = A,
		       errorDescriptor = B} ->
	    throw({error, {invalid_notifyReply, 
			   {A, TermId}, 
			   {B, asn1_NOVALUE}}});
	_ ->
	    throw({error, {invalid_notifyReply, NR}})
    end;
rarpanp_mg_verify_notify_rep(_TransId, _TermId, Crap) ->
    {error, {invalid_message, Crap}}.

rarpanp_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

rarpanp_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = rarpanp_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

rarpanp_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

rarpanp_mg_notify_request_msg(Mid, TransId, Rid, TermId, Cid) ->
    AR      = rarpanp_mg_notify_request_ar(Rid, TermId, Cid),
    TR      = cre_transReq(TransId, [AR]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

request_and_reply_pending_ack_one_pending(suite) ->
    [];
request_and_reply_pending_ack_one_pending(doc) ->
    ["This test case tests that megaco correctly handles the return "
     "value handle_pending_ack from handle_trans_request when ONE "
     "pending message has been sent"];
request_and_reply_pending_ack_one_pending(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        rar_paop),
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
    MgcEvSeq = rarpaop_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    megaco_test_generator:megaco(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = rarpaop_mg_event_sequence(text, tcp),

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

rarpaop_mgc_event_sequence(text, tcp) ->
    Mid = {deviceName,"ctrl"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    ConnectVerify = fun rarpaop_mgc_verify_handle_connect/1,
    ScrVerify = rarpaop_mgc_verify_service_change_req_fun(Mid),
    NrVerify = rarpaop_mgc_verify_notify_request_fun(),
    AckVerify = rarpaop_mgc_verify_reply_ack_fun(),
    DiscoVerify = fun rarpaop_mgc_verify_handle_disconnect/1,
    EvSeq = [
             {debug, true},
             {megaco_trace, disable},
             {megaco_trace, max},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
             {megaco_update_user_info, sent_pending_limit, 100},
             start_transport,
             listen,
             {megaco_callback, handle_connect,       ConnectVerify},
             {megaco_conn_info, all},
             {megaco_callback, handle_trans_request, ScrVerify},
	     {megaco_callback, handle_trans_request, NrVerify},
	     {megaco_callback, handle_trans_ack,     AckVerify},
             {megaco_callback, nocall, 10000},
             {megaco_callback, handle_disconnect,    DiscoVerify},
             megaco_stop_user,
             megaco_stop
            ],
    EvSeq.

%% Connect verification
rarpaop_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) ->
    {ok, CH, ok};
rarpaop_mgc_verify_handle_connect(Else) ->
    {error, Else, ok}.

%% Service Change verification
rarpaop_mgc_verify_service_change_req_fun(Mid) ->
    fun(Req) -> rarpaop_mgc_verify_service_change_req(Mid, Req) end.

rarpaop_mgc_verify_service_change_req(
  Mid, 
  {handle_trans_request, _, ?VERSION, [AR]}) ->
    (catch rarpaop_do_verify_service_change_req(Mid, AR));
rarpaop_mgc_verify_service_change_req(_Mid, Crap) ->
    ED = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

rarpaop_do_verify_service_change_req(Mid, AR) ->
    CR = 
	case AR of
	    #'ActionRequest'{commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		Err1      = {invalid_action_request, AR},
		ED1       = cre_ErrDesc(AR),
		ErrReply1 = {discard_ack, ED1},
		throw({error, Err1, ErrReply1})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		Err2      = {invalid_command_request, CR},
		ED2       = cre_ErrDesc(CR),
		ErrReply2 = {discard_ack, ED2},
		throw({error, Err2, ErrReply2})
	end,
    {Tid, Parms} = 
	case Cmd of
	    {serviceChangeReq, 
	     #'ServiceChangeRequest'{terminationID = [TermID],
				     serviceChangeParms = ServChParms}} ->
		{TermID, ServChParms};
	    _ ->
		Err3      = {invalid_command, Cmd},
		ED3       = cre_ErrDesc(Cmd),
		ErrReply3 = {discard_ack, ED3},
		throw({error, Err3, ErrReply3})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Err4      = {invalid_termination_id, Tid},
	    ED4       = cre_ErrDesc(Tid),
	    ErrReply4 = {discard_ack, ED4},
	    throw({error, Err4, ErrReply4})
    end,
    case Parms of
	#'ServiceChangeParm'{serviceChangeMethod = restart,
			     serviceChangeReason = [[$9,$0,$1|_]]} ->
	    AckData = [rarpaop_mgc_service_change_reply_ar(Mid, 1)], 
	    Reply   = {discard_ack, AckData},
	    {ok, AR, Reply};
	_ ->
	    Err5      = {invalid_SCP, Parms},
	    ED5       = cre_ErrDesc(Parms),
	    ErrReply5 = {discard_ack, ED5},
	    {error, Err5, ErrReply5}
    end.


%% Notify Request verification
rarpaop_mgc_verify_notify_request_fun() ->
    fun(Req) -> rarpaop_mgc_verify_notify_request(Req) end.

rarpaop_mgc_verify_notify_request({handle_trans_request, _, ?VERSION, [AR]}) ->
    (catch rarpaop_mgc_do_verify_notify_request(AR));
rarpaop_mgc_verify_notify_request(Crap) ->
    ED = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

rarpaop_mgc_do_verify_notify_request(AR) ->
    {Cid, CR} = 
	case AR of
	    #'ActionRequest'{contextId       = CtxID,
			     commandRequests = [CmdReq]} ->
		{CtxID, CmdReq};
	    _ ->
		Err1      = {invalid_action_request, AR},
		ED1       = cre_ErrDesc(AR),
		ErrReply1 = {discard_ack, ED1},
		throw({error, Err1, ErrReply1})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command;
	    _ ->
		Err2      = {invalid_command_request, CR},
		ED2       = cre_ErrDesc(CR),
		ErrReply2 = {discard_ack, ED2},
		throw({error, Err2, ErrReply2})
	end,
    NR = 
	case Cmd of
	    {notifyReq, NotifReq} ->
		NotifReq;
	    _ ->
		Err3      = {invalid_command, Cmd},
		ED3       = cre_ErrDesc(Cmd),
		ErrReply3 = {discard_ack, ED3},
		throw({error, Err3, ErrReply3})
	end,
    {Tid, OED} = 
	case NR of
	    #'NotifyRequest'{terminationID            = [TermID],
			     observedEventsDescriptor = ObsEvsDesc,
			     errorDescriptor          = asn1_NOVALUE} ->
		{TermID, ObsEvsDesc};
	    _ ->
		Err4      = {invalid_NR, NR},
		ED4       = cre_ErrDesc(NR),
		ErrReply4 = {discard_ack, ED4},
		throw({error, Err4, ErrReply4})
	end,
    OE = 
	case OED of 
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEvLst]} ->
		ObsEvLst;
	    _ ->
		Err5      = {invalid_OED, OED},
		ED5       = cre_ErrDesc(NR),
		ErrReply5 = {discard_ack, ED5},
		throw({error, Err5, ErrReply5})
	end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
	    AckData = notify_request_verified, 
	    Replies = [rarpaop_mgc_notify_reply_ar(Cid, Tid)],
	    Reply   = {{handle_pending_ack, AckData}, Replies},
	    {ok, 5000, AR, Reply};
	_ ->
	    Err6      = {invalid_OE, OE},
	    ED6       = cre_ErrDesc(OE),
	    ErrReply6 = {discard_ack, ED6},
	    throw({error, Err6, ErrReply6})
    end.


%% Ack verification
rarpaop_mgc_verify_reply_ack_fun() ->
    fun(M) -> rarpaop_mgc_verify_reply_ack(M) end.

rarpaop_mgc_verify_reply_ack({handle_trans_ack, _, ?VERSION, ok, _}) ->
    io:format("rarpaop_mgc_verify_reply_ack -> ok~n", []),
    {ok, ok, ok};
rarpaop_mgc_verify_reply_ack({handle_trans_ack, _, ?VERSION, AS, AD} = Crap) ->
    io:format("rarpaop_mgc_verify_reply_ack -> incorrect ack-status:"
	      "~n   AS: ~p"
	      "~n   AD: ~p"
	      "~n", [AS, AD]),
    ED = cre_ErrDesc({invalid_ack_status, {AS, AD}}),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply};
rarpaop_mgc_verify_reply_ack(Crap) ->
    io:format("rarpaop_mgc_verify_reply_ack -> invalid ack:"
	      "~n   Crap: ~p"
	      "~n", [Crap]),
    ED = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.


%% Disconnect verification
rarpaop_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, _R}) ->
    {ok, CH, ok};
rarpaop_mgc_verify_handle_disconnect(Else) ->
    {error, Else, ok}.

rarpaop_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    AR    = cre_actionReply(Cid, [CR]),
    AR.

rarpaop_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).


%%
%% MG generator stuff
%%
rarpaop_mg_event_sequence(text, tcp) ->
    DecodeFun = rarpaop_mg_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = rarpaop_mg_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid       = {deviceName,"mg"},
    TransId = 2, 
    TermId = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    ServiceChangeReq = rarpaop_mg_service_change_request_msg(Mid, 1, 0),
    ScrVerifyFun = rarpaop_mg_verify_service_change_rep_msg_fun(),
    NotifyReq = rarpaop_mg_notify_request_msg(Mid, TransId, 1, TermId, 1),
    PendVerifyFun = rarpaop_mg_verify_pending_msg_fun(TransId),
    NrVerifyFun = rarpaop_mg_verify_notify_rep_msg_fun(TransId, TermId),
    Ack = rarpaop_mg_ack_msg(Mid, TransId),
    EvSeq = [{debug,  true},
             {decode, DecodeFun},
             {encode, EncodeFun},
             {connect, 2944},
             {send, "service-change-request", ServiceChangeReq},
             {expect_receive, "service-change-reply", {ScrVerifyFun, 10000}},
             {send, "notify request", NotifyReq},
             {sleep, 2000},
             {send, "notify request", NotifyReq},
             {expect_receive, "pending", {PendVerifyFun, 5000}},
             {expect_receive, "notify-reply", {NrVerifyFun, 5000}},
             {send, "reply ack", Ack},
             {expect_nothing, 11000},
             disconnect
            ],
    EvSeq.

rarpaop_mg_encode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:encode_message(Conf, M)
    end.

rarpaop_mg_decode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:decode_message(Conf, M)
    end.

rarpaop_mg_verify_service_change_rep_msg_fun() ->
    fun(Msg) -> (catch rarpaop_mg_verify_service_change_rep(Msg)) end.

rarpaop_mg_verify_service_change_rep(#'MegacoMessage'{mess = Mess} = M) ->
    Body = 
	case Mess of 
	    #'Message'{version     = _V,
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId = _Tid,
                                immAckRequired = asn1_NOVALUE,
                                transactionResult = TransRes} ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
            {actionReplies, [ActRes]} ->
		ActRes;
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
            #'ActionReply'{contextId       = _Cid,
                           errorDescriptor = asn1_NOVALUE,
                           contextReply    = _CtxReq,
                           commandReply    = [CmdRep]} ->
		CmdRep;
	    _ ->
		throw({error, {invalid_actionReplies, AR}})
	end,
    SCR = 
	case CR of
            {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    SCRes = 
	case SCR of
            #'ServiceChangeReply'{terminationID       = _TermID,
                                  serviceChangeResult = ServChRes} ->
		ServChRes;
	    _ ->
		throw({error, {invalid_serviceChangeReply, SCR}})
	end,
    SCRP = 
	case SCRes of
            {serviceChangeResParms, Parms} ->
		Parms;
	    _ ->
		throw({error, {invalid_serviceChangeResult, SCRes}})
	end,
    case SCRP of
	#'ServiceChangeResParm'{serviceChangeMgcId = _MgcMid} ->
            {ok, M};
	_ ->
	    {error, {invalid_serviceChangeResParms, SCRP}}
    end;
rarpaop_mg_verify_service_change_rep(Crap) ->
    {error, {invalid_message, Crap}}.

rarpaop_mg_verify_pending_msg_fun(TransId) ->
    fun(Msg) -> (catch rarpaop_mg_verify_pending(TransId, Msg)) end.

rarpaop_mg_verify_pending(TransId, #'MegacoMessage'{mess = Mess} = M) ->
    Body = 
	case Mess of 
	    #'Message'{version     = _V,
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TP = 
	case Trans of
            {transactionPending, TransPending} ->
		TransPending;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    case TP of
	#'TransactionPending'{transactionId = TransId} ->
	    {ok, M};
	_ ->
	    throw({error, {invalid_transactionPending, TP}})
    end;
rarpaop_mg_verify_pending(_TransId, Crap) ->
    {error, {invalid_message, Crap}}.

rarpaop_mg_verify_notify_rep_msg_fun(TransId, TermId) ->
    fun(Msg) -> (catch rarpaop_mg_verify_notify_rep(TransId, TermId, Msg)) end.

rarpaop_mg_verify_notify_rep(TransId, TermId, 
			     #'MegacoMessage'{mess = Mess} = M) ->
    Body = 
	case Mess of 
	    #'Message'{version     = _V,
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId     = TransId,
                                immAckRequired    = 'NULL', % Ack
                                transactionResult = TransRes} ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
            {actionReplies, [ActRes]} ->
		ActRes;
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
            #'ActionReply'{contextId       = _Cid,
                           errorDescriptor = asn1_NOVALUE,
                           contextReply    = _CtxReq,
                           commandReply    = [CmdRep]} ->
		CmdRep;
	    _ ->
		throw({error, {invalid_actionReplies, AR}})
	end,
    NR = 
	case CR of
            {notifyReply, NotifyRep} ->
		NotifyRep;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,

    case NR of
	#'NotifyReply'{terminationID   = [TermId],
		       errorDescriptor = asn1_NOVALUE} ->
            {ok, M};
	#'NotifyReply'{terminationID   = A,
		       errorDescriptor = B} ->
	    throw({error, {invalid_notifyReply, 
			   {A, TermId}, 
			   {B, asn1_NOVALUE}}});
	_ ->
	    throw({error, {invalid_notifyReply, NR}})
    end;
rarpaop_mg_verify_notify_rep(_TransId, _TermId, Crap) ->
    {error, {invalid_message, Crap}}.

rarpaop_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

rarpaop_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = rarpaop_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

rarpaop_mg_ack_msg(Mid, TransId) ->
    TR    = cre_transRespAck(cre_transAck(TransId)),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

rarpaop_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

rarpaop_mg_notify_request_msg(Mid, TransId, Rid, TermId, Cid) ->
    AR      = rarpaop_mg_notify_request_ar(Rid, TermId, Cid),
    TR      = cre_transReq(TransId, [AR]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_trans_req_and_reply(suite) ->
    [];
single_trans_req_and_reply(doc) ->
    ["Receive a (single) transaction request and then send a "
     "reply (discard ack). "
     "The MGC is a megaco instance (megaco event sequence) and the "
     "MG is emulated (tcp event sequence)"];
single_trans_req_and_reply(Config) when list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        strar),
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
    MgcEvSeq = strar_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    megaco_test_generator:megaco(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = strar_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    megaco_test_generator:megaco(Mg, MgEvSeq),

    d("[MGC] await the generator reply"),
    case megaco_test_generator:megaco_await_reply(Mgc, 30000) of
	{ok, MgcReply} ->
	    d("[MGC] OK => MgcReply: ~n~p", [MgcReply]),
	    ok;
	{error, MgcReply} ->
	    d("[MGC] ERROR => MgcReply: ~n~p", [MgcReply]),
	    ?ERROR(mgc_failed)
		end,

    d("[MG] await the generator reply"),
    case megaco_test_generator:megaco_await_reply(Mg, 30000) of
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
strar_mgc_event_sequence(text, tcp) ->
    Mid = {deviceName,"ctrl"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ConnectVerify = fun strar_mgc_verify_handle_connect/1,
    ServiceChangeReqVerify = strar_mgc_verify_service_change_req_fun(Mid),
    %% Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NotifyReqVerify = strar_mgc_verify_notify_request_fun(),
    DiscoVerify = fun strar_mgc_verify_handle_disconnect/1,
%%     ReqTmr = #megaco_incr_timer{wait_for    = 500,
%% 				factor      = 1,
%% 				max_retries = 1},
    EvSeq = [
	     {debug, true},
	     {megaco_trace, disable},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     listen,
	     {megaco_callback, handle_connect,       ConnectVerify},
	     {megaco_callback, handle_trans_request, ServiceChangeReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_disconnect,    DiscoVerify},
	     {sleep, 1000},
	     megaco_stop_user,
	     megaco_stop
	    ],
    EvSeq.


strar_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("strar_mgc_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
strar_mgc_verify_handle_connect(Else) ->
    io:format("strar_mgc_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

strar_mgc_verify_service_change_req_fun(Mid) ->
    fun(Req) -> strar_mgc_verify_scr(Mid, Req) end.

strar_mgc_verify_scr(Mid, {handle_trans_request, _, ?VERSION, [AR]}) ->
    (catch strar_mgc_do_verify_scr(Mid, AR));
strar_mgc_verify_scr(_Mid, Crap) ->
    ED = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

strar_mgc_do_verify_scr(Mid, AR) ->
    io:format("strar_mgc_verify_service_change_req -> ok"
	      "~n   AR: ~p~n", [AR]),
    CR = 
	case AR of
	    #'ActionRequest'{commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
                Err1      = {invalid_action_request, AR},
                ED1       = cre_ErrDesc(AR),
                ErrReply1 = {discard_ack, ED1},
                throw({error, Err1, ErrReply1})
	end,
    Cmd =
        case CR of
            #'CommandRequest'{command = Command} ->
                Command;
            _ ->
                Err2      = {invalid_command_request, CR},
                ED2       = cre_ErrDesc(CR),
                ErrReply2 = {discard_ack, ED2},
                throw({error, Err2, ErrReply2})
        end,
    {Tid, Parms} =
        case Cmd of
            {serviceChangeReq,
             #'ServiceChangeRequest'{terminationID = [TermID],
                                     serviceChangeParms = ServChParms}} ->
                {TermID, ServChParms};
            _ ->
                Err3      = {invalid_command, Cmd},
                ED3       = cre_ErrDesc(Cmd),
                ErrReply3 = {discard_ack, ED3},
                throw({error, Err3, ErrReply3})
        end,
    case Tid of
        #megaco_term_id{contains_wildcards = false, id = ["root"]} ->
            ok;
        _ ->
            Err4      = {invalid_termination_id, Tid},
            ED4       = cre_ErrDesc(Tid),
            ErrReply4 = {discard_ack, ED4},
            throw({error, Err4, ErrReply4})
    end,
    case Parms of
        #'ServiceChangeParm'{serviceChangeMethod = restart,
                             serviceChangeReason = [[$9,$0,$1|_]]} ->
            AckData = [strar_mgc_service_change_reply_ar(Mid, 1)],
            Reply   = {discard_ack, AckData},
            {ok, AR, Reply};
        _ ->
            Err5      = {invalid_SCP, Parms},
            ED5       = cre_ErrDesc(Parms),
            ErrReply5 = {discard_ack, ED5},
            {error, Err5, ErrReply5}
    end.

strar_mgc_verify_notify_request_fun() ->
    fun(Req) -> strar_mgc_verify_nr(Req) end.

strar_mgc_verify_nr({handle_trans_request, _, ?VERSION, [AR]}) ->
    (catch strar_mgc_do_verify_nr(AR));
strar_mgc_verify_nr(Crap) ->
    ED = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.
    
strar_mgc_do_verify_nr(AR) ->
    io:format("strar_mgc_do_verify_nr -> ok"
	      "~n   AR: ~p~n", [AR]),
    {Cid, CR} =
	case AR of
	    #'ActionRequest'{contextId       = CtxID, 
			     commandRequests = [CmdReq]} when (CtxID == 1) or
							      (CtxID == 2) ->
		{CtxID, CmdReq};
	    _ ->
                Err1      = {invalid_action_request, AR},
                ED1       = cre_ErrDesc(AR),
                ErrReply1 = {discard_ack, ED1},
                throw({error, Err1, ErrReply1})
        end,
    Cmd =
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command;
	    _ ->
                Err2      = {invalid_command_request, CR},
                ED2       = cre_ErrDesc(CR),
                ErrReply2 = {discard_ack, ED2},
                throw({error, Err2, ErrReply2})
        end,
    NR =
        case Cmd of
	    {notifyReq, NotifReq} ->
		NotifReq;
	    _ ->
                Err3      = {invalid_command, Cmd},
                ED3       = cre_ErrDesc(Cmd),
                ErrReply3 = {discard_ack, ED3},
                throw({error, Err3, ErrReply3})
        end,
    {Tid, OED} =
        case NR of
            #'NotifyRequest'{terminationID            = [TermID],
                             observedEventsDescriptor = ObsEvsDesc,
                             errorDescriptor          = asn1_NOVALUE} ->
                {TermID, ObsEvsDesc};
            _ ->
                Err4      = {invalid_NR, NR},
                ED4       = cre_ErrDesc(NR),
                ErrReply4 = {discard_ack, ED4},
                throw({error, Err4, ErrReply4})
        end,
    OE =
	case OED of
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEvLst]} ->
		ObsEvLst;
            _ ->
                Err5      = {invalid_OED, OED},
                ED5       = cre_ErrDesc(NR),
                ErrReply5 = {discard_ack, ED5},
                throw({error, Err5, ErrReply5})
        end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
            Replies = [strar_mgc_notify_reply_ar(Cid, Tid)],
            Reply   = {discard_ack, Replies},
            {ok, AR, Reply};
        _ ->
            Err6      = {invalid_OE, OE},
            ED6       = cre_ErrDesc(OE),
            ErrReply6 = {discard_ack, ED6},
            {error, Err6, ErrReply6}
    end.

strar_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, R}) -> 
    io:format("strar_mgc_verify_handle_disconnect -> ok"
	      "~n   CH: ~p"
	      "~n   R:  ~p"
	      "~n", [CH, R]),
    {ok, CH, ok};
strar_mgc_verify_handle_disconnect(Else) ->
    io:format("strar_mgc_verify_handle_disconnect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


strar_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    cre_actionReply(Cid, [CR]).

strar_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

strar_mgc_notify_reply(Mid, TransId, Cid, TermId) ->
    AR    = strar_mgc_notify_reply_ar(Cid, TermId),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%% 
strar_mg_event_sequence(text, tcp) ->
    Mid = {deviceName, "mg"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ServiceChangeReq = [strar_mg_service_change_request_ar(Mid, 1)],
    ConnectVerify = strar_mg_verify_handle_connect_fun(), 
    ServiceChangeReplyVerify = strar_mg_verify_service_change_reply_fun(), 
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NR = fun(Cid, Rid) ->
		 [strar_mg_notify_request_ar(Rid, Tid, Cid)]
	 end,
    NotifyReplyVerify = fun strar_mg_verify_notify_reply/1, 
    EvSeq = [
	     {debug, true},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     {megaco_trace, disable},
	     {megaco_system_info, users},
	     {megaco_system_info, connections},
	     connect,
	     {megaco_callback, handle_connect, ConnectVerify},
	     megaco_connect,
	     {megaco_cast, ServiceChangeReq, []},
	     {megaco_callback, handle_connect, ConnectVerify}, 
	     {megaco_callback, handle_trans_reply, ServiceChangeReplyVerify},
	     {sleep, 1000},
	     {megaco_system_info, users},
	     {megaco_system_info, connections},
	     {sleep, 1000},
	     {megaco_conn_info, all},
	     {megaco_cast, NR(1,1), []},
	     {megaco_callback, handle_trans_reply, NotifyReplyVerify},
	     {sleep, 3000},
	     megaco_stop_user,
	     megaco_stop,
	     {sleep, 1000}
	    ],
    EvSeq.

strar_mg_verify_handle_connect_fun() ->
    fun(Ev) -> strar_mg_verify_handle_connect(Ev) end.

strar_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("strar_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
strar_mg_verify_handle_connect(Else) ->
    io:format("strar_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

strar_mg_verify_service_change_reply_fun() ->
    fun(Rep) -> strar_mg_verify_scr(Rep) end.

strar_mg_verify_scr({handle_trans_reply, _CH, ?VERSION, {ok, [AR]}, _}) ->
    (catch strar_mg_do_verify_scr(AR));
strar_mg_verify_scr(Crap) ->
    {error, Crap, ok}.

strar_mg_do_verify_scr(AR) ->
    io:format("strar_mg_verify_scr -> ok"
	      "~n   AR: ~p~n", [AR]),
    CR = 
	case AR of
	    #'ActionReply'{commandReply = [CmdRep]} ->
		CmdRep;
	    _ ->
		Reason1 = {invalid_action_reply, AR},
		throw({error, Reason1, ok})
	end,
    SCR = 
	case CR of
	    {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		Reason2 = {invalid_command_reply, CR},
		throw({error, Reason2, ok})
	end,
    {Tid, SCRes} = 
	case SCR of
	    #'ServiceChangeReply'{terminationID       = [TermID],
				  serviceChangeResult = Res} ->
		{TermID, Res};
	    _ ->
		Reason3 = {invalid_service_change_reply, SCR},
		throw({error, Reason3, ok})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Reason4 = {invalid_termination_id, Tid},
	    throw({error, Reason4, ok})
    end,
    SCRParm = 
	case SCRes of
	    {serviceChangeResParms, ServChResParms} ->
		ServChResParms;
	    _ ->
		Reason5 = {invalid_serviceChangeResult, SCRes},
		throw({error, Reason5, ok})
	end,
    case SCRParm of
	#'ServiceChangeResParm'{serviceChangeMgcId = _RemoteMid} ->
	    {ok, AR, ok};
	_ ->
	    Reason6 = {invalid_service_change_result, SCRParm},
	    {error, Reason6, ok}
    end.

strar_mg_verify_notify_reply_fun() ->
    fun(Rep) -> strar_mg_verify_notify_reply(Rep) end.
	     
strar_mg_verify_notify_reply({handle_trans_reply, _CH, ?VERSION, 
			      {ok, [AR]}, _}) ->
    io:format("strar_mg_verify_notify_reply -> ok"
	      "~n   AR: ~p~n", [AR]),
    {ok, AR, ok};
strar_mg_verify_notify_reply(Else) ->
    io:format("strar_mg_verify_notify_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

strar_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

strar_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_trans_req_and_reply_sendopts(suite) ->
    [];
single_trans_req_and_reply_sendopts(doc) ->
    ["Receive a (single) transaction request and then send a "
     "reply with handle_ack and a nre reply_timer in sendoptions. "
     "The MGC is a megaco instance (megaco event sequence) and the "
     "MG is emulated (tcp event sequence)"];
single_trans_req_and_reply_sendopts(Config) when list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        straro),
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
    MgcEvSeq = straro_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    megaco_test_generator:megaco(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = straro_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    megaco_test_generator:megaco(Mg, MgEvSeq),

    d("[MGC] await the generator reply"),
    case megaco_test_generator:megaco_await_reply(Mgc, 30000) of
	{ok, MgcReply} ->
	    d("[MGC] OK => MgcReply: ~n~p", [MgcReply]),
	    ok;
	{error, MgcReply} ->
	    d("[MGC] ERROR => MgcReply: ~n~p", [MgcReply]),
	    ?ERROR(mgc_failed)
    end,

    d("[MG] await the generator reply"),
    case megaco_test_generator:megaco_await_reply(Mg, 30000) of
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
straro_mgc_event_sequence(text, tcp) ->
    Mid = {deviceName,"ctrl"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ConnectVerify = fun straro_mgc_verify_handle_connect/1,
    ServiceChangeReqVerify = straro_mgc_verify_service_change_req_fun(Mid),
    NotifyReqVerify = straro_mgc_verify_notify_request_fun(),
    TransAckVerify = straro_mgc_verify_handle_trans_ack_fun(), 
    EvSeq = [
	     {debug, true},
	     {megaco_trace, disable},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     listen,
	     {megaco_callback, handle_connect,       ConnectVerify},
	     {megaco_callback, handle_trans_request, ServiceChangeReqVerify},
	     {megaco_callback, handle_trans_request, NotifyReqVerify},
	     {megaco_callback, handle_trans_ack,     TransAckVerify},
	     megaco_stop_user,
	     megaco_stop
	    ],
    EvSeq.


straro_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("straro_mgc_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
straro_mgc_verify_handle_connect(Else) ->
    io:format("straro_mgc_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

straro_mgc_verify_service_change_req_fun(Mid) ->
    fun(Req) -> straro_mgc_verify_scr(Mid, Req) end.

straro_mgc_verify_scr(Mid, {handle_trans_request, _, ?VERSION, [AR]}) ->
    (catch straro_mgc_do_verify_scr(Mid, AR));
straro_mgc_verify_scr(_Mid, Crap) ->
    ED = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

straro_mgc_do_verify_scr(Mid, AR) ->
    io:format("straro_mgc_verify_service_change_req -> ok"
	      "~n   AR: ~p~n", [AR]),
    CR = 
	case AR of
	    #'ActionRequest'{commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
                Err1      = {invalid_action_request, AR},
                ED1       = cre_ErrDesc(AR),
                ErrReply1 = {discard_ack, ED1},
                throw({error, Err1, ErrReply1})
	end,
    Cmd =
        case CR of
            #'CommandRequest'{command = Command} ->
                Command;
            _ ->
                Err2      = {invalid_command_request, CR},
                ED2       = cre_ErrDesc(CR),
                ErrReply2 = {discard_ack, ED2},
                throw({error, Err2, ErrReply2})
        end,
    {Tid, Parms} =
        case Cmd of
            {serviceChangeReq,
             #'ServiceChangeRequest'{terminationID = [TermID],
                                     serviceChangeParms = ServChParms}} ->
                {TermID, ServChParms};
            _ ->
                Err3      = {invalid_command, Cmd},
                ED3       = cre_ErrDesc(Cmd),
                ErrReply3 = {discard_ack, ED3},
                throw({error, Err3, ErrReply3})
        end,
    case Tid of
        #megaco_term_id{contains_wildcards = false, id = ["root"]} ->
            ok;
        _ ->
            Err4      = {invalid_termination_id, Tid},
            ED4       = cre_ErrDesc(Tid),
            ErrReply4 = {discard_ack, ED4},
            throw({error, Err4, ErrReply4})
    end,
    case Parms of
        #'ServiceChangeParm'{serviceChangeMethod = restart,
                             serviceChangeReason = [[$9,$0,$1|_]]} ->
            AckData = [straro_mgc_service_change_reply_ar(Mid, 1)],
            Reply   = {discard_ack, AckData},
            {ok, AR, Reply};
        _ ->
            Err5      = {invalid_SCP, Parms},
            ED5       = cre_ErrDesc(Parms),
            ErrReply5 = {discard_ack, ED5},
            {error, Err5, ErrReply5}
    end.

straro_mgc_verify_notify_request_fun() ->
    fun(Req) -> straro_mgc_verify_nr(Req) end.

straro_mgc_verify_nr({handle_trans_request, _, ?VERSION, [AR]}) ->
    (catch straro_mgc_do_verify_nr(AR));
straro_mgc_verify_nr(Crap) ->
    ED = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.
    
straro_mgc_do_verify_nr(AR) ->
    io:format("straro_mgc_do_verify_nr -> ok"
	      "~n   AR: ~p~n", [AR]),
    {Cid, CR} =
	case AR of
	    #'ActionRequest'{contextId       = CtxID, 
			     commandRequests = [CmdReq]} when (CtxID == 1) or
							      (CtxID == 2) ->
		{CtxID, CmdReq};
	    _ ->
                Err1      = {invalid_action_request, AR},
                ED1       = cre_ErrDesc(AR),
                ErrReply1 = {discard_ack, ED1},
                throw({error, Err1, ErrReply1})
        end,
    Cmd =
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command;
	    _ ->
                Err2      = {invalid_command_request, CR},
                ED2       = cre_ErrDesc(CR),
                ErrReply2 = {discard_ack, ED2},
                throw({error, Err2, ErrReply2})
        end,
    NR =
        case Cmd of
	    {notifyReq, NotifReq} ->
		NotifReq;
	    _ ->
                Err3      = {invalid_command, Cmd},
                ED3       = cre_ErrDesc(Cmd),
                ErrReply3 = {discard_ack, ED3},
                throw({error, Err3, ErrReply3})
        end,
    {Tid, OED} =
        case NR of
            #'NotifyRequest'{terminationID            = [TermID],
                             observedEventsDescriptor = ObsEvsDesc,
                             errorDescriptor          = asn1_NOVALUE} ->
                {TermID, ObsEvsDesc};
            _ ->
                Err4      = {invalid_NR, NR},
                ED4       = cre_ErrDesc(NR),
                ErrReply4 = {discard_ack, ED4},
                throw({error, Err4, ErrReply4})
        end,
    OE =
	case OED of
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEvLst]} ->
		ObsEvLst;
            _ ->
                Err5      = {invalid_OED, OED},
                ED5       = cre_ErrDesc(NR),
                ErrReply5 = {discard_ack, ED5},
                throw({error, Err5, ErrReply5})
        end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
            Replies = [straro_mgc_notify_reply_ar(Cid, Tid)],
	    SendOpts = [{protocol_version, 99}],
            Reply   = {{handle_ack, get(tc)}, Replies, SendOpts},
            {ok, AR, Reply};
        _ ->
            Err6      = {invalid_OE, OE},
            ED6       = cre_ErrDesc(OE),
            ErrReply6 = {discard_ack, ED6},
            {error, Err6, ErrReply6}
    end.

straro_mgc_verify_handle_trans_ack_fun() ->
    fun(Ack) -> straro_mgc_verify_ta(Ack) end.

straro_mgc_verify_ta({handle_trans_ack, _CH, ?VERSION, AS, _AD}) -> 
    (catch straro_mgc_do_verify_ta(AS));
straro_mgc_verify_ta(Crap) ->
    {error, Crap, ok}.

straro_mgc_do_verify_ta({error, {EM, [EC, Msg], Reason}}) ->
    io:format("straro_mgc_do_verify_handle_ta -> entry with"
	      "~n   EM:     ~p"
	      "~n   EC:     ~p"
	      "~n   Msg:    ~p"
	      "~n   Reason: ~p"
	      "~n", [EM, EC, Msg, Reason]),
    case Reason of
	{bad_version, 99} ->
	    {ok, Reason, ok};
	_ ->
	    {error, {unexpected_reason, Reason}, ok}
    end;
straro_mgc_do_verify_ta(Else) ->
    io:format("straro_mgc_verify_handle_ta -> unknown"
	      "~n   Else: ~p"
	      "~n", [Else]),
    {error, Else, ok}.

straro_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, R}) -> 
    io:format("straro_mgc_verify_handle_disconnect -> ok"
	      "~n   CH: ~p"
	      "~n   R:  ~p"
	      "~n", [CH, R]),
    {ok, CH, ok};
straro_mgc_verify_handle_disconnect(Else) ->
    io:format("straro_mgc_verify_handle_disconnect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


straro_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    cre_actionReply(Cid, [CR]).

straro_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

straro_mgc_notify_reply(Mid, TransId, Cid, TermId) ->
    AR    = straro_mgc_notify_reply_ar(Cid, TermId),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%
%% MG generator stuff
%% 
straro_mg_event_sequence(text, tcp) ->
    Mid = {deviceName, "mg"},
    RI = [
	  {port,             2944},
	  {encoding_module,  megaco_pretty_text_encoder},
	  {encoding_config,  []},
	  {transport_module, megaco_tcp}
	 ],
    ServiceChangeReq = [straro_mg_service_change_request_ar(Mid, 1)],
    ConnectVerify = straro_mg_verify_handle_connect_fun(), 
    DiscoVerify = fun straro_mg_verify_handle_disconnect/1,
    ServiceChangeReplyVerify = straro_mg_verify_service_change_reply_fun(), 
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NR = fun(Cid, Rid) ->
		 [straro_mg_notify_request_ar(Rid, Tid, Cid)]
	 end,
    EvSeq = [
	     {debug, true},
	     megaco_start,
	     {megaco_start_user, Mid, RI, []},
	     start_transport,
	     {megaco_trace, disable},
	     {megaco_system_info, users},
	     {megaco_system_info, connections},
	     connect,
	     {megaco_callback, handle_connect, ConnectVerify},
	     megaco_connect,
	     {megaco_cast, ServiceChangeReq, []},
	     {megaco_callback, handle_connect, ConnectVerify}, 
	     {megaco_callback, handle_trans_reply, ServiceChangeReplyVerify},
	     {sleep, 1000},
	     {megaco_system_info, users},
	     {megaco_system_info, connections},
	     {sleep, 1000},
	     {megaco_conn_info, all},
	     {megaco_cast, NR(1,1), []},
	     {megaco_callback, handle_disconnect, DiscoVerify}, 
	     megaco_stop_user,
	     megaco_stop,
	     {sleep, 1000}
	    ],
    EvSeq.

straro_mg_verify_handle_connect_fun() ->
    fun(Ev) -> straro_mg_verify_handle_connect(Ev) end.

straro_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("straro_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
straro_mg_verify_handle_connect(Else) ->
    io:format("straro_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

straro_mg_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, R}) -> 
    io:format("straro_mg_verify_handle_disconnect -> ok"
	      "~n   CH: ~p"
	      "~n   R:  ~p"
	      "~n", [CH, R]),
    {ok, CH, ok};
straro_mg_verify_handle_disconnect(Else) ->
    io:format("straro_mg_verify_handle_disconnect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


straro_mg_verify_service_change_reply_fun() ->
    fun(Rep) -> straro_mg_verify_scr(Rep) end.

straro_mg_verify_scr({handle_trans_reply, _CH, ?VERSION, {ok, [AR]}, _}) ->
    (catch straro_mg_do_verify_scr(AR));
straro_mg_verify_scr(Crap) ->
    {error, Crap, ok}.

straro_mg_do_verify_scr(AR) ->
    io:format("straro_mg_verify_scr -> ok"
	      "~n   AR: ~p~n", [AR]),
    CR = 
	case AR of
	    #'ActionReply'{commandReply = [CmdRep]} ->
		CmdRep;
	    _ ->
		Reason1 = {invalid_action_reply, AR},
		throw({error, Reason1, ok})
	end,
    SCR = 
	case CR of
	    {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		Reason2 = {invalid_command_reply, CR},
		throw({error, Reason2, ok})
	end,
    {Tid, SCRes} = 
	case SCR of
	    #'ServiceChangeReply'{terminationID       = [TermID],
				  serviceChangeResult = Res} ->
		{TermID, Res};
	    _ ->
		Reason3 = {invalid_service_change_reply, SCR},
		throw({error, Reason3, ok})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Reason4 = {invalid_termination_id, Tid},
	    throw({error, Reason4, ok})
    end,
    SCRParm = 
	case SCRes of
	    {serviceChangeResParms, ServChResParms} ->
		ServChResParms;
	    _ ->
		Reason5 = {invalid_serviceChangeResult, SCRes},
		throw({error, Reason5, ok})
	end,
    case SCRParm of
	#'ServiceChangeResParm'{serviceChangeMgcId = _RemoteMid} ->
	    {ok, AR, ok};
	_ ->
	    Reason6 = {invalid_service_change_result, SCRParm},
	    {error, Reason6, ok}
    end.

straro_mg_verify_notify_reply_fun() ->
    fun(Rep) -> straro_mg_verify_notify_reply(Rep) end.
	     
straro_mg_verify_notify_reply({handle_trans_reply, _CH, ?VERSION, 
			      {ok, [AR]}, _}) ->
    io:format("straro_mg_verify_notify_reply -> ok"
	      "~n   AR: ~p~n", [AR]),
    {ok, AR, ok};
straro_mg_verify_notify_reply(Else) ->
    io:format("straro_mg_verify_notify_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

straro_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

straro_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

request_and_reply_and_ack(suite) ->
    [];
request_and_reply_and_ack(doc) ->
    ["This test case tests that megaco correctly handles three-way-handshake"];
request_and_reply_and_ack(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        raraa),
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
    MgcEvSeq = raraa_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    megaco_test_generator:megaco(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = raraa_mg_event_sequence(text, tcp),

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

raraa_mgc_event_sequence(text, tcp) ->
    Mid = {deviceName,"ctrl"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    ConnectVerify = fun raraa_mgc_verify_handle_connect/1,
    ScrVerify = raraa_mgc_verify_service_change_req_fun(Mid),
    NrVerify = raraa_mgc_verify_notify_request_fun(),
    AckVerify = raraa_mgc_verify_trans_ack_fun(),
    DiscoVerify = fun raraa_mgc_verify_handle_disconnect/1,
    EvSeq = [
             {debug, true},
             {megaco_trace, disable},
             {megaco_trace, max},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
             {megaco_update_user_info, sent_pending_limit, 100},
             start_transport,
             listen,
             {megaco_callback, handle_connect,           ConnectVerify},
             {megaco_conn_info, all},
             {megaco_callback, handle_trans_request,     ScrVerify},
	     {megaco_callback, handle_trans_request,     NrVerify},
	     {megaco_callback, handle_trans_ack,         AckVerify},
             {megaco_callback, handle_disconnect,        DiscoVerify},
             megaco_stop_user,
             megaco_stop
            ],
    EvSeq.

%% Connect verification
raraa_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) ->
    {ok, CH, ok};
raraa_mgc_verify_handle_connect(Else) ->
    {error, Else, ok}.

%% Service Change verification
raraa_mgc_verify_service_change_req_fun(Mid) ->
    fun(Req) -> raraa_mgc_verify_service_change_req(Mid, Req) end.

raraa_mgc_verify_service_change_req(
  Mid, 
  {handle_trans_request, _, ?VERSION, [AR]}) ->
    (catch raraa_do_verify_service_change_req(Mid, AR));
raraa_mgc_verify_service_change_req(_Mid, Crap) ->
    ED = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

raraa_do_verify_service_change_req(Mid, AR) ->
    CR = 
	case AR of
	    #'ActionRequest'{commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		Err1      = {invalid_action_request, AR},
		ED1       = cre_ErrDesc(AR),
		ErrReply1 = {discard_ack, ED1},
		throw({error, Err1, ErrReply1})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		Err2      = {invalid_command_request, CR},
		ED2       = cre_ErrDesc(CR),
		ErrReply2 = {discard_ack, ED2},
		throw({error, Err2, ErrReply2})
	end,
    {Tid, Parms} = 
	case Cmd of
	    {serviceChangeReq, 
	     #'ServiceChangeRequest'{terminationID = [TermID],
				     serviceChangeParms = ServChParms}} ->
		{TermID, ServChParms};
	    _ ->
		Err3      = {invalid_command, Cmd},
		ED3       = cre_ErrDesc(Cmd),
		ErrReply3 = {discard_ack, ED3},
		throw({error, Err3, ErrReply3})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Err4      = {invalid_termination_id, Tid},
	    ED4       = cre_ErrDesc(Tid),
	    ErrReply4 = {discard_ack, ED4},
	    throw({error, Err4, ErrReply4})
    end,
    case Parms of
	#'ServiceChangeParm'{serviceChangeMethod = restart,
			     serviceChangeReason = [[$9,$0,$1|_]]} ->
	    AckData = [raraa_mgc_service_change_reply_ar(Mid, 1)], 
	    Reply   = {discard_ack, AckData},
	    {ok, AR, Reply};
	_ ->
	    Err5      = {invalid_SCP, Parms},
	    ED5       = cre_ErrDesc(Parms),
	    ErrReply5 = {discard_ack, ED5},
	    {error, Err5, ErrReply5}
    end.


%% Notify Request verification
raraa_mgc_verify_notify_request_fun() ->
    fun(Req) -> raraa_mgc_verify_notify_request(Req) end.

raraa_mgc_verify_notify_request({handle_trans_request, _, ?VERSION, [AR]}) ->
    (catch raraa_mgc_do_verify_notify_request(AR));
raraa_mgc_verify_notify_request(Crap) ->
    ED = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

raraa_mgc_do_verify_notify_request(AR) ->
    {Cid, CR} = 
	case AR of
	    #'ActionRequest'{contextId       = CtxID,
			     commandRequests = [CmdReq]} ->
		{CtxID, CmdReq};
	    _ ->
		Err1      = {invalid_action_request, AR},
		ED1       = cre_ErrDesc(AR),
		ErrReply1 = {discard_ack, ED1},
		throw({error, Err1, ErrReply1})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command;
	    _ ->
		Err2      = {invalid_command_request, CR},
		ED2       = cre_ErrDesc(CR),
		ErrReply2 = {discard_ack, ED2},
		throw({error, Err2, ErrReply2})
	end,
    NR = 
	case Cmd of
	    {notifyReq, NotifReq} ->
		NotifReq;
	    _ ->
		Err3      = {invalid_command, Cmd},
		ED3       = cre_ErrDesc(Cmd),
		ErrReply3 = {discard_ack, ED3},
		throw({error, Err3, ErrReply3})
	end,
    {Tid, OED} = 
	case NR of
	    #'NotifyRequest'{terminationID            = [TermID],
			     observedEventsDescriptor = ObsEvsDesc,
			     errorDescriptor          = asn1_NOVALUE} ->
		{TermID, ObsEvsDesc};
	    _ ->
		Err4      = {invalid_NR, NR},
		ED4       = cre_ErrDesc(NR),
		ErrReply4 = {discard_ack, ED4},
		throw({error, Err4, ErrReply4})
	end,
    OE = 
	case OED of 
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEvLst]} ->
		ObsEvLst;
	    _ ->
		Err5      = {invalid_OED, OED},
		ED5       = cre_ErrDesc(NR),
		ErrReply5 = {discard_ack, ED5},
		throw({error, Err5, ErrReply5})
	end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
	    AckData = raraa, 
	    Replies = [raraa_mgc_notify_reply_ar(Cid, Tid)],
	    Reply   = {{handle_ack, AckData}, Replies},
	    {ok, AR, Reply};
	_ ->
	    Err6      = {invalid_OE, OE},
	    ED6       = cre_ErrDesc(OE),
	    ErrReply6 = {discard_ack, ED6},
	    throw({error, Err6, ErrReply6})
    end.


raraa_mgc_verify_trans_ack_fun() ->
    fun(Ack) -> raraa_mgc_verify_trans_ack(Ack) end.

raraa_mgc_verify_trans_ack({handle_trans_ack, CH, ?VERSION, ok, raraa}) ->
    io:format("raraa_mgc_verify_trans_ack -> ok"
              "~n   CH: ~p"
              "~n", [CH]),
    {ok, CH, ok};
raraa_mgc_verify_trans_ack(Crap) ->
    io:format("raraa_mgc_verify_trans_ack -> unknown"
              "~n   Crap: ~p~n", [Crap]),
    {error, Crap, ok}.


%% Disconnect verification
raraa_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, _R}) ->
    {ok, CH, ok};
raraa_mgc_verify_handle_disconnect(Else) ->
    {error, Else, ok}.

raraa_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    AR    = cre_actionReply(Cid, [CR]),
    AR.

raraa_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).


%%
%% MG generator stuff
%%
raraa_mg_event_sequence(text, tcp) ->
    DecodeFun = raraa_mg_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = raraa_mg_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid       = {deviceName,"mg"},
    ServiceChangeReq = raraa_mg_service_change_request_msg(Mid, 1, 0),
    ScrVerifyFun = raraa_mg_verify_service_change_rep_msg_fun(),
    TermId  = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    TransId = 2,
    ReqId   = 1,
    CtxId   = 1, 
    NotifyReq = raraa_mg_notify_request_msg(Mid, TermId, 
					    TransId, ReqId, CtxId),
    NrVerifyFun = raraa_mg_verify_notify_rep_msg_fun(TermId, 
						     TransId, ReqId, CtxId),
    TransAck = raraa_mg_trans_ack_msg(Mid, TransId),
    EvSeq = [{debug,  true},
             {decode, DecodeFun},
             {encode, EncodeFun},
             {connect, 2944},
             {send, "service-change-request", ServiceChangeReq},
             {expect_receive, "service-change-reply", {ScrVerifyFun, 10000}},
             {send, "notify request", NotifyReq},
             {expect_receive, "notify-reply", {NrVerifyFun, 10000}},
             {send, "transaction-ack", TransAck},
             {expect_nothing, 11000},
             disconnect
            ],
    EvSeq.

raraa_mg_encode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:encode_message(Conf, M)
    end.

raraa_mg_decode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:decode_message(Conf, M)
    end.

raraa_mg_verify_service_change_rep_msg_fun() ->
    fun(Msg) -> (catch raraa_mg_verify_service_change_rep(Msg)) end.

raraa_mg_verify_service_change_rep(#'MegacoMessage'{mess = Mess} = M) ->
    Body = 
	case Mess of 
	    #'Message'{version     = _V,
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId = _Tid,
                                immAckRequired = asn1_NOVALUE,
                                transactionResult = TransRes} ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
            {actionReplies, [ActRes]} ->
		ActRes;
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
            #'ActionReply'{contextId       = _Cid,
                           errorDescriptor = asn1_NOVALUE,
                           contextReply    = _CtxReq,
                           commandReply    = [CmdRep]} ->
		CmdRep;
	    _ ->
		throw({error, {invalid_actionReplies, AR}})
	end,
    SCR = 
	case CR of
            {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    SCRes = 
	case SCR of
            #'ServiceChangeReply'{terminationID       = _TermID,
                                  serviceChangeResult = ServChRes} ->
		ServChRes;
	    _ ->
		throw({error, {invalid_serviceChangeReply, SCR}})
	end,
    SCRP = 
	case SCRes of
            {serviceChangeResParms, Parms} ->
		Parms;
	    _ ->
		throw({error, {invalid_serviceChangeResult, SCRes}})
	end,
    case SCRP of
	#'ServiceChangeResParm'{serviceChangeMgcId = _MgcMid} ->
            {ok, M};
	_ ->
	    {error, {invalid_serviceChangeResParms, SCRP}}
    end;
raraa_mg_verify_service_change_rep(Crap) ->
    {error, {invalid_message, Crap}}.

raraa_mg_verify_notify_rep_msg_fun(TermId, TransId, Rid, Cid) ->
    fun(Msg) -> (catch raraa_mg_verify_notify_rep(Msg, 
						  TermId, 
						  TransId, Rid, Cid)) end.

raraa_mg_verify_notify_rep(#'MegacoMessage'{mess = Mess} = M,
			   TermId, TransId, _Rid, Cid) ->
%%     io:format("raraa_mg_verify_notify_rep -> entry with"
%% 	      "~n   M:       ~p"
%% 	      "~n   TermId:  ~p"
%% 	      "~n   TransId: ~p"
%% 	      "~n   Rid:     ~p"
%% 	      "~n   Cid:     ~p"
%% 	      "~n", [M, TermId, TransId, Rid, Cid]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId     = TransId,
                                immAckRequired    = 'NULL',
                                transactionResult = TransRes} ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
            {actionReplies, [ActRes]} ->
		ActRes;
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
            #'ActionReply'{contextId       = Cid,
                           errorDescriptor = asn1_NOVALUE,
                           contextReply    = _CtxReq,
                           commandReply    = [CmdRep]} ->
		CmdRep;
	    _ ->
		throw({error, {invalid_actionReplies, AR}})
	end,
    NR = 
	case CR of
            {notifyReply, NotifyReply} ->
		NotifyReply;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    case NR of
	#'NotifyReply'{terminationID   = [TermId],
		       errorDescriptor = asn1_NOVALUE} ->
	    {ok, M};
	_ ->
	    {error, {invalid_notifyReply, NR}}
    end;
raraa_mg_verify_notify_rep(Crap, _TermId, _TransId, _Rid, _Cid) ->
    {error, {invalid_message, Crap}}.

raraa_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

raraa_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = raraa_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

raraa_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

raraa_mg_notify_request_msg(Mid, TermId, TransId, Rid, Cid) ->
    AR      = raraa_mg_notify_request_ar(Rid, TermId, Cid),
    TR      = cre_transReq(TransId, [AR]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

raraa_mg_trans_ack_msg(Mid, TransId) ->
    TR    = cre_transRespAck(cre_transAck(TransId)),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

request_and_reply_and_no_ack(suite) ->
    [];
request_and_reply_and_no_ack(doc) ->
    ["This test case tests that megaco handles a failed three-way-handshake,"
     " i.e. when the ack never arrives"];
request_and_reply_and_no_ack(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        rarana),
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
    MgcEvSeq = rarana_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    megaco_test_generator:megaco(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = rarana_mg_event_sequence(text, tcp),

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

rarana_mgc_event_sequence(text, tcp) ->
    Mid = {deviceName,"ctrl"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    ConnectVerify = fun rarana_mgc_verify_handle_connect/1,
    ScrVerify     = rarana_mgc_verify_service_change_req_fun(Mid),
    NrVerify      = rarana_mgc_verify_notify_request_fun(),
    AckVerify     = rarana_mgc_verify_trans_ack_fun(),
    DiscoVerify   = fun rarana_mgc_verify_handle_disconnect/1,
    EvSeq = [
             {debug, true},
             {megaco_trace, disable},
             {megaco_trace, max},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
             {megaco_update_user_info, sent_pending_limit, 100},
             {megaco_update_user_info, reply_timer,        9000},
             start_transport,
             listen,
             {megaco_callback, handle_connect,           ConnectVerify},
             {megaco_conn_info, all},
             {megaco_callback, handle_trans_request,     ScrVerify},
	     {megaco_callback, handle_trans_request,     NrVerify},
	     {megaco_callback, handle_trans_ack,         AckVerify},
	     %% {megaco_callback, nocall,                   8000},
             {megaco_callback, handle_disconnect,        DiscoVerify},
             megaco_stop_user,
             megaco_stop
            ],
    EvSeq.

%% Connect verification
rarana_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) ->
    {ok, CH, ok};
rarana_mgc_verify_handle_connect(Else) ->
    {error, Else, ok}.

%% Service Change verification
rarana_mgc_verify_service_change_req_fun(Mid) ->
    fun(Req) -> rarana_mgc_verify_service_change_req(Mid, Req) end.

rarana_mgc_verify_service_change_req(
  Mid, 
  {handle_trans_request, _, ?VERSION, [AR]}) ->
    (catch rarana_do_verify_service_change_req(Mid, AR));
rarana_mgc_verify_service_change_req(_Mid, Crap) ->
    ED = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

rarana_do_verify_service_change_req(Mid, AR) ->
    CR = 
	case AR of
	    #'ActionRequest'{commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		Err1      = {invalid_action_request, AR},
		ED1       = cre_ErrDesc(AR),
		ErrReply1 = {discard_ack, ED1},
		throw({error, Err1, ErrReply1})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		Err2      = {invalid_command_request, CR},
		ED2       = cre_ErrDesc(CR),
		ErrReply2 = {discard_ack, ED2},
		throw({error, Err2, ErrReply2})
	end,
    {Tid, Parms} = 
	case Cmd of
	    {serviceChangeReq, 
	     #'ServiceChangeRequest'{terminationID = [TermID],
				     serviceChangeParms = ServChParms}} ->
		{TermID, ServChParms};
	    _ ->
		Err3      = {invalid_command, Cmd},
		ED3       = cre_ErrDesc(Cmd),
		ErrReply3 = {discard_ack, ED3},
		throw({error, Err3, ErrReply3})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Err4      = {invalid_termination_id, Tid},
	    ED4       = cre_ErrDesc(Tid),
	    ErrReply4 = {discard_ack, ED4},
	    throw({error, Err4, ErrReply4})
    end,
    case Parms of
	#'ServiceChangeParm'{serviceChangeMethod = restart,
			     serviceChangeReason = [[$9,$0,$1|_]]} ->
	    AckData = [rarana_mgc_service_change_reply_ar(Mid, 1)], 
	    Reply   = {discard_ack, AckData},
	    {ok, AR, Reply};
	_ ->
	    Err5      = {invalid_SCP, Parms},
	    ED5       = cre_ErrDesc(Parms),
	    ErrReply5 = {discard_ack, ED5},
	    {error, Err5, ErrReply5}
    end.


%% Notify Request verification
rarana_mgc_verify_notify_request_fun() ->
    fun(Req) -> rarana_mgc_verify_notify_request(Req) end.

rarana_mgc_verify_notify_request({handle_trans_request, _, ?VERSION, [AR]}) ->
    (catch rarana_mgc_do_verify_notify_request(AR));
rarana_mgc_verify_notify_request(Crap) ->
    ED = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

rarana_mgc_do_verify_notify_request(AR) ->
    {Cid, CR} = 
	case AR of
	    #'ActionRequest'{contextId       = CtxID,
			     commandRequests = [CmdReq]} ->
		{CtxID, CmdReq};
	    _ ->
		Err1      = {invalid_action_request, AR},
		ED1       = cre_ErrDesc(AR),
		ErrReply1 = {discard_ack, ED1},
		throw({error, Err1, ErrReply1})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command;
	    _ ->
		Err2      = {invalid_command_request, CR},
		ED2       = cre_ErrDesc(CR),
		ErrReply2 = {discard_ack, ED2},
		throw({error, Err2, ErrReply2})
	end,
    NR = 
	case Cmd of
	    {notifyReq, NotifReq} ->
		NotifReq;
	    _ ->
		Err3      = {invalid_command, Cmd},
		ED3       = cre_ErrDesc(Cmd),
		ErrReply3 = {discard_ack, ED3},
		throw({error, Err3, ErrReply3})
	end,
    {Tid, OED} = 
	case NR of
	    #'NotifyRequest'{terminationID            = [TermID],
			     observedEventsDescriptor = ObsEvsDesc,
			     errorDescriptor          = asn1_NOVALUE} ->
		{TermID, ObsEvsDesc};
	    _ ->
		Err4      = {invalid_NR, NR},
		ED4       = cre_ErrDesc(NR),
		ErrReply4 = {discard_ack, ED4},
		throw({error, Err4, ErrReply4})
	end,
    OE = 
	case OED of 
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEvLst]} ->
		ObsEvLst;
	    _ ->
		Err5      = {invalid_OED, OED},
		ED5       = cre_ErrDesc(NR),
		ErrReply5 = {discard_ack, ED5},
		throw({error, Err5, ErrReply5})
	end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
	    AckData = rarana, 
	    Replies = [rarana_mgc_notify_reply_ar(Cid, Tid)],
	    Reply   = {{handle_ack, AckData}, Replies},
	    {ok, AR, Reply};
	_ ->
	    Err6      = {invalid_OE, OE},
	    ED6       = cre_ErrDesc(OE),
	    ErrReply6 = {discard_ack, ED6},
	    throw({error, Err6, ErrReply6})
    end.


rarana_mgc_verify_trans_ack_fun() ->
    fun(Ack) -> rarana_mgc_verify_trans_ack(Ack) end.

rarana_mgc_verify_trans_ack({handle_trans_ack, CH, ?VERSION, 
			    {error, timeout}, rarana}) ->
    io:format("rarana_mgc_verify_trans_ack -> expected error: ok"
              "~n   CH: ~p"
              "~n", [CH]),
    {ok, CH, ok};
rarana_mgc_verify_trans_ack(Crap) ->
    io:format("rarana_mgc_verify_trans_ack -> unknown"
              "~n   Crap: ~p~n", [Crap]),
    {error, Crap, ok}.


%% Disconnect verification
rarana_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, _R}) ->
    {ok, CH, ok};
rarana_mgc_verify_handle_disconnect(Else) ->
    {error, Else, ok}.

rarana_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    AR    = cre_actionReply(Cid, [CR]),
    AR.

rarana_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).


%%
%% MG generator stuff
%%
rarana_mg_event_sequence(text, tcp) ->
    DecodeFun = rarana_mg_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = rarana_mg_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid       = {deviceName,"mg"},
    ServiceChangeReq = rarana_mg_service_change_request_msg(Mid, 1, 0),
    ScrVerifyFun = rarana_mg_verify_service_change_rep_msg_fun(),
    TermId  = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    TransId = 2,
    ReqId   = 1,
    CtxId   = 1, 
    NotifyReq = rarana_mg_notify_request_msg(Mid, TermId, 
					    TransId, ReqId, CtxId),
    NrVerifyFun = rarana_mg_verify_notify_rep_msg_fun(TermId, 
						     TransId, ReqId, CtxId),
    EvSeq = [{debug,  true},
             {decode, DecodeFun},
             {encode, EncodeFun},
             {connect, 2944},
             {send, "service-change-request", ServiceChangeReq},
             {expect_receive, "service-change-reply", {ScrVerifyFun, 10000}},
             {send, "notify request", NotifyReq},
             {expect_receive, "notify-reply", {NrVerifyFun, 10000}},
             {expect_nothing, 11000},
             disconnect
            ],
    EvSeq.

rarana_mg_encode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:encode_message(Conf, M)
    end.

rarana_mg_decode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:decode_message(Conf, M)
    end.

rarana_mg_verify_service_change_rep_msg_fun() ->
    fun(Msg) -> (catch rarana_mg_verify_service_change_rep(Msg)) end.

rarana_mg_verify_service_change_rep(#'MegacoMessage'{mess = Mess} = M) ->
    Body = 
	case Mess of 
	    #'Message'{version     = _V,
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId = _Tid,
                                immAckRequired = asn1_NOVALUE,
                                transactionResult = TransRes} ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
            {actionReplies, [ActRes]} ->
		ActRes;
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
            #'ActionReply'{contextId       = _Cid,
                           errorDescriptor = asn1_NOVALUE,
                           contextReply    = _CtxReq,
                           commandReply    = [CmdRep]} ->
		CmdRep;
	    _ ->
		throw({error, {invalid_actionReplies, AR}})
	end,
    SCR = 
	case CR of
            {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    SCRes = 
	case SCR of
            #'ServiceChangeReply'{terminationID       = _TermID,
                                  serviceChangeResult = ServChRes} ->
		ServChRes;
	    _ ->
		throw({error, {invalid_serviceChangeReply, SCR}})
	end,
    SCRP = 
	case SCRes of
            {serviceChangeResParms, Parms} ->
		Parms;
	    _ ->
		throw({error, {invalid_serviceChangeResult, SCRes}})
	end,
    case SCRP of
	#'ServiceChangeResParm'{serviceChangeMgcId = _MgcMid} ->
            {ok, M};
	_ ->
	    {error, {invalid_serviceChangeResParms, SCRP}}
    end;
rarana_mg_verify_service_change_rep(Crap) ->
    {error, {invalid_message, Crap}}.

rarana_mg_verify_notify_rep_msg_fun(TermId, TransId, Rid, Cid) ->
    fun(Msg) -> (catch rarana_mg_verify_notify_rep(Msg, 
						  TermId, 
						  TransId, Rid, Cid)) end.

rarana_mg_verify_notify_rep(#'MegacoMessage'{mess = Mess} = M,
			   TermId, TransId, _Rid, Cid) ->
%%     io:format("rarana_mg_verify_notify_rep -> entry with"
%% 	      "~n   M:       ~p"
%% 	      "~n   TermId:  ~p"
%% 	      "~n   TransId: ~p"
%% 	      "~n   Rid:     ~p"
%% 	      "~n   Cid:     ~p"
%% 	      "~n", [M, TermId, TransId, Rid, Cid]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId     = TransId,
                                immAckRequired    = 'NULL',
                                transactionResult = TransRes} ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
            {actionReplies, [ActRes]} ->
		ActRes;
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
            #'ActionReply'{contextId       = Cid,
                           errorDescriptor = asn1_NOVALUE,
                           contextReply    = _CtxReq,
                           commandReply    = [CmdRep]} ->
		CmdRep;
	    _ ->
		throw({error, {invalid_actionReplies, AR}})
	end,
    NR = 
	case CR of
            {notifyReply, NotifyReply} ->
		NotifyReply;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    case NR of
	#'NotifyReply'{terminationID   = [TermId],
		       errorDescriptor = asn1_NOVALUE} ->
	    {ok, M};
	_ ->
	    {error, {invalid_notifyReply, NR}}
    end;
rarana_mg_verify_notify_rep(Crap, _TermId, _TransId, _Rid, _Cid) ->
    {error, {invalid_message, Crap}}.

rarana_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

rarana_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = rarana_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

rarana_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

rarana_mg_notify_request_msg(Mid, TermId, TransId, Rid, Cid) ->
    AR      = rarana_mg_notify_request_ar(Rid, TermId, Cid),
    TR      = cre_transReq(TransId, [AR]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

request_and_reply_and_late_ack(suite) ->
    [];
request_and_reply_and_late_ack(doc) ->
    ["This test case tests that megaco handles three-way-handshake "
     "when the ack is late (and requeire a retransmission)"];
request_and_reply_and_late_ack(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        rarala),
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
    MgcEvSeq = rarala_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    megaco_test_generator:megaco(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = rarala_mg_event_sequence(text, tcp),

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

rarala_mgc_event_sequence(text, tcp) ->
    Mid = {deviceName,"ctrl"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    RepTmr = #megaco_incr_timer{wait_for    = 3000,
				factor      = 1, 
				incr        = 0,
				max_retries = 2
			       },    
    ConnectVerify = fun rarala_mgc_verify_handle_connect/1,
    ScrVerify = rarala_mgc_verify_service_change_req_fun(Mid),
    NrVerify = rarala_mgc_verify_notify_request_fun(),
    AckVerify = rarala_mgc_verify_trans_ack_fun(),
    DiscoVerify = fun rarala_mgc_verify_handle_disconnect/1,
    EvSeq = [
             {debug, true},
             {megaco_trace, disable},
             {megaco_trace, max},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
             {megaco_update_user_info, sent_pending_limit, 100},
             {megaco_update_user_info, reply_timer,        RepTmr},
             start_transport,
             listen,
             {megaco_callback, handle_connect,           ConnectVerify},
             {megaco_conn_info, all},
             {megaco_callback, handle_trans_request,     ScrVerify},
	     {megaco_callback, handle_trans_request,     NrVerify},
	     {megaco_callback, handle_trans_ack,         AckVerify},
             {megaco_callback, handle_disconnect,        DiscoVerify},
             megaco_stop_user,
             megaco_stop
            ],
    EvSeq.

%% Connect verification
rarala_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) ->
    {ok, CH, ok};
rarala_mgc_verify_handle_connect(Else) ->
    {error, Else, ok}.

%% Service Change verification
rarala_mgc_verify_service_change_req_fun(Mid) ->
    fun(Req) -> rarala_mgc_verify_service_change_req(Mid, Req) end.

rarala_mgc_verify_service_change_req(
  Mid, 
  {handle_trans_request, _, ?VERSION, [AR]}) ->
    (catch rarala_do_verify_service_change_req(Mid, AR));
rarala_mgc_verify_service_change_req(_Mid, Crap) ->
    ED = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

rarala_do_verify_service_change_req(Mid, AR) ->
    CR = 
	case AR of
	    #'ActionRequest'{commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		Err1      = {invalid_action_request, AR},
		ED1       = cre_ErrDesc(AR),
		ErrReply1 = {discard_ack, ED1},
		throw({error, Err1, ErrReply1})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		Err2      = {invalid_command_request, CR},
		ED2       = cre_ErrDesc(CR),
		ErrReply2 = {discard_ack, ED2},
		throw({error, Err2, ErrReply2})
	end,
    {Tid, Parms} = 
	case Cmd of
	    {serviceChangeReq, 
	     #'ServiceChangeRequest'{terminationID = [TermID],
				     serviceChangeParms = ServChParms}} ->
		{TermID, ServChParms};
	    _ ->
		Err3      = {invalid_command, Cmd},
		ED3       = cre_ErrDesc(Cmd),
		ErrReply3 = {discard_ack, ED3},
		throw({error, Err3, ErrReply3})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Err4      = {invalid_termination_id, Tid},
	    ED4       = cre_ErrDesc(Tid),
	    ErrReply4 = {discard_ack, ED4},
	    throw({error, Err4, ErrReply4})
    end,
    case Parms of
	#'ServiceChangeParm'{serviceChangeMethod = restart,
			     serviceChangeReason = [[$9,$0,$1|_]]} ->
	    AckData = [rarala_mgc_service_change_reply_ar(Mid, 1)], 
	    Reply   = {discard_ack, AckData},
	    {ok, AR, Reply};
	_ ->
	    Err5      = {invalid_SCP, Parms},
	    ED5       = cre_ErrDesc(Parms),
	    ErrReply5 = {discard_ack, ED5},
	    {error, Err5, ErrReply5}
    end.


%% Notify Request verification
rarala_mgc_verify_notify_request_fun() ->
    fun(Req) -> rarala_mgc_verify_notify_request(Req) end.

rarala_mgc_verify_notify_request({handle_trans_request, _, ?VERSION, [AR]}) ->
    (catch rarala_mgc_do_verify_notify_request(AR));
rarala_mgc_verify_notify_request(Crap) ->
    ED = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

rarala_mgc_do_verify_notify_request(AR) ->
    io:format("rarala_mgc_do_verify_notify_request -> entry with"
	      "~n   AR: ~p"
	      "~n", [AR]),
    {Cid, CR} = 
	case AR of
	    #'ActionRequest'{contextId       = CtxID,
			     commandRequests = [CmdReq]} ->
		{CtxID, CmdReq};
	    _ ->
		Err1      = {invalid_action_request, AR},
		ED1       = cre_ErrDesc(AR),
		ErrReply1 = {discard_ack, ED1},
		throw({error, Err1, ErrReply1})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command;
	    _ ->
		Err2      = {invalid_command_request, CR},
		ED2       = cre_ErrDesc(CR),
		ErrReply2 = {discard_ack, ED2},
		throw({error, Err2, ErrReply2})
	end,
    NR = 
	case Cmd of
	    {notifyReq, NotifReq} ->
		NotifReq;
	    _ ->
		Err3      = {invalid_command, Cmd},
		ED3       = cre_ErrDesc(Cmd),
		ErrReply3 = {discard_ack, ED3},
		throw({error, Err3, ErrReply3})
	end,
    {Tid, OED} = 
	case NR of
	    #'NotifyRequest'{terminationID            = [TermID],
			     observedEventsDescriptor = ObsEvsDesc,
			     errorDescriptor          = asn1_NOVALUE} ->
		{TermID, ObsEvsDesc};
	    _ ->
		Err4      = {invalid_NR, NR},
		ED4       = cre_ErrDesc(NR),
		ErrReply4 = {discard_ack, ED4},
		throw({error, Err4, ErrReply4})
	end,
    OE = 
	case OED of 
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEvLst]} ->
		ObsEvLst;
	    _ ->
		Err5      = {invalid_OED, OED},
		ED5       = cre_ErrDesc(NR),
		ErrReply5 = {discard_ack, ED5},
		throw({error, Err5, ErrReply5})
	end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
	    AckData = rarala, 
	    Replies = [rarala_mgc_notify_reply_ar(Cid, Tid)],
	    Reply   = {{handle_ack, AckData}, Replies},
	    {ok, AR, Reply};
	_ ->
	    Err6      = {invalid_OE, OE},
	    ED6       = cre_ErrDesc(OE),
	    ErrReply6 = {discard_ack, ED6},
	    throw({error, Err6, ErrReply6})
    end.


rarala_mgc_verify_trans_ack_fun() ->
    fun(Ack) -> rarala_mgc_verify_trans_ack(Ack) end.

rarala_mgc_verify_trans_ack({handle_trans_ack, CH, ?VERSION, ok, rarala}) ->
    io:format("rarala_mgc_verify_trans_ack -> ok"
              "~n   CH: ~p"
              "~n", [CH]),
    {ok, CH, ok};
rarala_mgc_verify_trans_ack(Crap) ->
    io:format("rarala_mgc_verify_trans_ack -> unknown"
              "~n   Crap: ~p~n", [Crap]),
    {error, Crap, ok}.


%% Disconnect verification
rarala_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, _R}) ->
    {ok, CH, ok};
rarala_mgc_verify_handle_disconnect(Else) ->
    {error, Else, ok}.

rarala_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    AR    = cre_actionReply(Cid, [CR]),
    AR.

rarala_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).


%%
%% MG generator stuff
%%
rarala_mg_event_sequence(text, tcp) ->
    DecodeFun = rarala_mg_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = rarala_mg_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid       = {deviceName,"mg"},
    ServiceChangeReq = rarala_mg_service_change_request_msg(Mid, 1, 0),
    ScrVerifyFun = rarala_mg_verify_service_change_rep_msg_fun(),
    TermId  = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    TransId = 2,
    ReqId   = 1,
    CtxId   = 1, 
    NotifyReq = rarala_mg_notify_request_msg(Mid, TermId, 
					    TransId, ReqId, CtxId),
    NrVerifyFun = rarala_mg_verify_notify_rep_msg_fun(TermId, 
						     TransId, ReqId, CtxId),
    TransAck = rarala_mg_trans_ack_msg(Mid, TransId),
    EvSeq = [{debug,  true},
             {decode, DecodeFun},
             {encode, EncodeFun},
             {connect, 2944},
             {send, "service-change-request", ServiceChangeReq},
             {expect_receive, "service-change-reply", {ScrVerifyFun, 10000}},
             {send, "notify request", NotifyReq},
             {expect_receive, "notify-reply", {NrVerifyFun, 4000}},
             {expect_receive, "notify-reply", {NrVerifyFun, 4000}},
             {expect_receive, "notify-reply", {NrVerifyFun, 4000}},
             {send, "transaction-ack", TransAck},
             {expect_nothing, 11000},
             disconnect
            ],
    EvSeq.

rarala_mg_encode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:encode_message(Conf, M)
    end.

rarala_mg_decode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:decode_message(Conf, M)
    end.

rarala_mg_verify_service_change_rep_msg_fun() ->
    fun(Msg) -> (catch rarala_mg_verify_service_change_rep(Msg)) end.

rarala_mg_verify_service_change_rep(#'MegacoMessage'{mess = Mess} = M) ->
    Body = 
	case Mess of 
	    #'Message'{version     = _V,
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId = _Tid,
                                immAckRequired = asn1_NOVALUE,
                                transactionResult = TransRes} ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
            {actionReplies, [ActRes]} ->
		ActRes;
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
            #'ActionReply'{contextId       = _Cid,
                           errorDescriptor = asn1_NOVALUE,
                           contextReply    = _CtxReq,
                           commandReply    = [CmdRep]} ->
		CmdRep;
	    _ ->
		throw({error, {invalid_actionReplies, AR}})
	end,
    SCR = 
	case CR of
            {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    SCRes = 
	case SCR of
            #'ServiceChangeReply'{terminationID       = _TermID,
                                  serviceChangeResult = ServChRes} ->
		ServChRes;
	    _ ->
		throw({error, {invalid_serviceChangeReply, SCR}})
	end,
    SCRP = 
	case SCRes of
            {serviceChangeResParms, Parms} ->
		Parms;
	    _ ->
		throw({error, {invalid_serviceChangeResult, SCRes}})
	end,
    case SCRP of
	#'ServiceChangeResParm'{serviceChangeMgcId = _MgcMid} ->
            {ok, M};
	_ ->
	    {error, {invalid_serviceChangeResParms, SCRP}}
    end;
rarala_mg_verify_service_change_rep(Crap) ->
    {error, {invalid_message, Crap}}.

rarala_mg_verify_notify_rep_msg_fun(TermId, TransId, Rid, Cid) ->
    fun(Msg) -> (catch rarala_mg_verify_notify_rep(Msg, 
						  TermId, 
						  TransId, Rid, Cid)) end.

rarala_mg_verify_notify_rep(#'MegacoMessage'{mess = Mess} = M,
			   TermId, TransId, Rid, Cid) ->
    io:format("rarala_mg_verify_notify_rep -> entry with"
	      "~n   M:       ~p"
	      "~n   TermId:  ~p"
	      "~n   TransId: ~p"
	      "~n   Rid:     ~p"
	      "~n   Cid:     ~p"
	      "~n", [M, TermId, TransId, Rid, Cid]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId     = TransId,
                                immAckRequired    = 'NULL',
                                transactionResult = TransRes} ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
            {actionReplies, [ActRes]} ->
		ActRes;
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
            #'ActionReply'{contextId       = Cid,
                           errorDescriptor = asn1_NOVALUE,
                           contextReply    = _CtxReq,
                           commandReply    = [CmdRep]} ->
		CmdRep;
	    _ ->
		throw({error, {invalid_actionReplies, AR}})
	end,
    NR = 
	case CR of
            {notifyReply, NotifyReply} ->
		NotifyReply;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    case NR of
	#'NotifyReply'{terminationID   = [TermId],
		       errorDescriptor = asn1_NOVALUE} ->
	    {ok, M};
	_ ->
	    {error, {invalid_notifyReply, NR}}
    end;
rarala_mg_verify_notify_rep(Crap, _TermId, _TransId, _Rid, _Cid) ->
    {error, {invalid_message, Crap}}.

rarala_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

rarala_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = rarala_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

rarala_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

rarala_mg_notify_request_msg(Mid, TermId, TransId, Rid, Cid) ->
    AR      = rarala_mg_notify_request_ar(Rid, TermId, Cid),
    TR      = cre_transReq(TransId, [AR]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

rarala_mg_trans_ack_msg(Mid, TransId) ->
    TR    = cre_transRespAck(cre_transAck(TransId)),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pending_ack_plain(suite) ->
    [];
pending_ack_plain(doc) ->
    ["Receive a request and handle it as a long request, "
     "i.e. return with {pending, _} and expect a call to the "
     "long request function"];
pending_ack_plain(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        pap),
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
    MgcEvSeq = pap_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    megaco_test_generator:megaco(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = pap_mg_event_sequence(text, tcp),

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

pap_mgc_event_sequence(text, tcp) ->
    Mid = {deviceName,"ctrl"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    ConnectVerify = fun pap_mgc_verify_handle_connect/1,
    ScrVerify = pap_mgc_verify_service_change_req_fun(Mid),
    NrVerify1 = pap_mgc_verify_notify_request_fun(),
    NrVerify2 = pap_mgc_verify_notify_request_long_fun(),
    AckVerify = pap_mgc_verify_trans_ack_fun(),
    DiscoVerify = fun pap_mgc_verify_handle_disconnect/1,
    EvSeq = [
             {debug, true},
             {megaco_trace, disable},
             {megaco_trace, max},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
             {megaco_update_user_info, sent_pending_limit, 100},
             start_transport,
             listen,
             {megaco_callback, handle_connect,            ConnectVerify},
             {megaco_conn_info, all},
             {megaco_callback, handle_trans_request,      ScrVerify},
	     {megaco_callback, handle_trans_request,      NrVerify1},
	     {megaco_callback, handle_trans_long_request, NrVerify2},
	     {megaco_callback, handle_trans_ack,          AckVerify},
             {megaco_callback, handle_disconnect,         DiscoVerify},
             megaco_stop_user,
             megaco_stop
            ],
    EvSeq.

%% Connect verification
pap_mgc_verify_handle_connect({handle_connect, CH, ?VERSION}) ->
    {ok, CH, ok};
pap_mgc_verify_handle_connect(Else) ->
    {error, Else, ok}.

%% Service Change verification
pap_mgc_verify_service_change_req_fun(Mid) ->
    fun(Req) -> pap_mgc_verify_service_change_req(Mid, Req) end.

pap_mgc_verify_service_change_req(
  Mid, 
  {handle_trans_request, _, ?VERSION, [AR]}) ->
    (catch pap_do_verify_service_change_req(Mid, AR));
pap_mgc_verify_service_change_req(_Mid, Crap) ->
    ED = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

pap_do_verify_service_change_req(Mid, AR) ->
    CR = 
	case AR of
	    #'ActionRequest'{commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		Err1      = {invalid_action_request, AR},
		ED1       = cre_ErrDesc(AR),
		ErrReply1 = {discard_ack, ED1},
		throw({error, Err1, ErrReply1})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		Err2      = {invalid_command_request, CR},
		ED2       = cre_ErrDesc(CR),
		ErrReply2 = {discard_ack, ED2},
		throw({error, Err2, ErrReply2})
	end,
    {Tid, Parms} = 
	case Cmd of
	    {serviceChangeReq, 
	     #'ServiceChangeRequest'{terminationID = [TermID],
				     serviceChangeParms = ServChParms}} ->
		{TermID, ServChParms};
	    _ ->
		Err3      = {invalid_command, Cmd},
		ED3       = cre_ErrDesc(Cmd),
		ErrReply3 = {discard_ack, ED3},
		throw({error, Err3, ErrReply3})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Err4      = {invalid_termination_id, Tid},
	    ED4       = cre_ErrDesc(Tid),
	    ErrReply4 = {discard_ack, ED4},
	    throw({error, Err4, ErrReply4})
    end,
    case Parms of
	#'ServiceChangeParm'{serviceChangeMethod = restart,
			     serviceChangeReason = [[$9,$0,$1|_]]} ->
	    AckData = [pap_mgc_service_change_reply_ar(Mid, 1)], 
	    Reply   = {discard_ack, AckData},
	    {ok, AR, Reply};
	_ ->
	    Err5      = {invalid_SCP, Parms},
	    ED5       = cre_ErrDesc(Parms),
	    ErrReply5 = {discard_ack, ED5},
	    {error, Err5, ErrReply5}
    end.


%% Notify Request verification
pap_mgc_verify_notify_request_fun() ->
    fun(Req) -> pap_mgc_verify_notify_request(Req) end.

pap_mgc_verify_notify_request({handle_trans_request, _, ?VERSION, [AR]}) ->
    io:format("pap_mgc_do_verify_notify_request -> entry with"
	      "~n   AR: ~p"
	      "~n", [AR]),
    Reply = {pending, AR}, 
    {ok, AR, Reply};
pap_mgc_verify_notify_request(Crap) ->
    ED = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

%% Notify Request verification
pap_mgc_verify_notify_request_long_fun() ->
    fun(Req) -> pap_mgc_verify_notify_request_long(Req) end.

pap_mgc_verify_notify_request_long({handle_trans_long_request, _, 
				    ?VERSION, AR}) ->
    (catch pap_mgc_do_verify_notify_request_long(AR));
pap_mgc_verify_notify_request_long(Crap) ->
    ED = cre_ErrDesc(Crap),
    ErrReply = {discard_ack, ED},
    {error, Crap, ErrReply}.

pap_mgc_do_verify_notify_request_long(AR) ->
    io:format("pap_mgc_do_verify_notify_request_long -> entry with"
	      "~n   AR: ~p"
	      "~n", [AR]),
    {Cid, CR} = 
	case AR of
	    #'ActionRequest'{contextId       = CtxID,
			     commandRequests = [CmdReq]} ->
		{CtxID, CmdReq};
	    _ ->
		Err1      = {invalid_action_request, AR},
		ED1       = cre_ErrDesc(AR),
		ErrReply1 = {discard_ack, ED1},
		throw({error, Err1, ErrReply1})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command;
	    _ ->
		Err2      = {invalid_command_request, CR},
		ED2       = cre_ErrDesc(CR),
		ErrReply2 = {discard_ack, ED2},
		throw({error, Err2, ErrReply2})
	end,
    NR = 
	case Cmd of
	    {notifyReq, NotifReq} ->
		NotifReq;
	    _ ->
		Err3      = {invalid_command, Cmd},
		ED3       = cre_ErrDesc(Cmd),
		ErrReply3 = {discard_ack, ED3},
		throw({error, Err3, ErrReply3})
	end,
    {Tid, OED} = 
	case NR of
	    #'NotifyRequest'{terminationID            = [TermID],
			     observedEventsDescriptor = ObsEvsDesc,
			     errorDescriptor          = asn1_NOVALUE} ->
		{TermID, ObsEvsDesc};
	    _ ->
		Err4      = {invalid_NR, NR},
		ED4       = cre_ErrDesc(NR),
		ErrReply4 = {discard_ack, ED4},
		throw({error, Err4, ErrReply4})
	end,
    OE = 
	case OED of 
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEvLst]} ->
		ObsEvLst;
	    _ ->
		Err5      = {invalid_OED, OED},
		ED5       = cre_ErrDesc(NR),
		ErrReply5 = {discard_ack, ED5},
		throw({error, Err5, ErrReply5})
	end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
	    AckData = pap, 
	    Replies = [pap_mgc_notify_reply_ar(Cid, Tid)],
	    Reply   = {{handle_ack, AckData}, Replies},
	    {ok, AR, Reply};
	_ ->
	    Err6      = {invalid_OE, OE},
	    ED6       = cre_ErrDesc(OE),
	    ErrReply6 = {discard_ack, ED6},
	    throw({error, Err6, ErrReply6})
    end.


pap_mgc_verify_trans_ack_fun() ->
    fun(Ack) -> pap_mgc_verify_trans_ack(Ack) end.

pap_mgc_verify_trans_ack({handle_trans_ack, CH, ?VERSION, ok, pap}) ->
    io:format("pap_mgc_verify_trans_ack -> ok"
              "~n   CH: ~p"
              "~n", [CH]),
    {ok, CH, ok};
pap_mgc_verify_trans_ack(Crap) ->
    io:format("pap_mgc_verify_trans_ack -> unknown"
              "~n   Crap: ~p~n", [Crap]),
    {error, Crap, ok}.


%% Disconnect verification
pap_mgc_verify_handle_disconnect({handle_disconnect, CH, ?VERSION, _R}) ->
    {ok, CH, ok};
pap_mgc_verify_handle_disconnect(Else) ->
    {error, Else, ok}.

pap_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    AR    = cre_actionReply(Cid, [CR]),
    AR.

pap_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).


%%
%% MG generator stuff
%%
pap_mg_event_sequence(text, tcp) ->
    DecodeFun = pap_mg_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = pap_mg_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid       = {deviceName,"mg"},
    ServiceChangeReq = pap_mg_service_change_request_msg(Mid, 1, 0),
    ScrVerifyFun = pap_mg_verify_service_change_rep_msg_fun(),
    TermId  = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    TransId = 2,
    ReqId   = 1,
    CtxId   = 1, 
    NotifyReq = 
	pap_mg_notify_request_msg(Mid, TermId, TransId, ReqId, CtxId),
    PendingVerifyFun = 
	pap_mg_verify_pending_msg_fun(TransId),
    NrVerifyFun = 
	pap_mg_verify_notify_rep_msg_fun(TermId, TransId, ReqId, CtxId),
    TransAck = pap_mg_trans_ack_msg(Mid, TransId),
    EvSeq = [{debug,  true},
             {decode, DecodeFun},
             {encode, EncodeFun},
             {connect, 2944},
             {send, "service-change-request", ServiceChangeReq},
             {expect_receive, "service-change-reply", {ScrVerifyFun, 10000}},
             {send, "notify request", NotifyReq},
             {expect_receive, "pending",      {PendingVerifyFun, 4000}},
             {expect_receive, "notify-reply", {NrVerifyFun, 4000}},
             {send, "transaction-ack", TransAck},
             {expect_nothing, 11000},
             disconnect
            ],
    EvSeq.

pap_mg_encode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:encode_message(Conf, M)
    end.

pap_mg_decode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:decode_message(Conf, M)
    end.

pap_mg_verify_service_change_rep_msg_fun() ->
    fun(Msg) -> (catch pap_mg_verify_service_change_rep(Msg)) end.

pap_mg_verify_service_change_rep(#'MegacoMessage'{mess = Mess} = M) ->
    Body = 
	case Mess of 
	    #'Message'{version     = _V,
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId = _Tid,
                                immAckRequired = asn1_NOVALUE,
                                transactionResult = TransRes} ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
            {actionReplies, [ActRes]} ->
		ActRes;
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
            #'ActionReply'{contextId       = _Cid,
                           errorDescriptor = asn1_NOVALUE,
                           contextReply    = _CtxReq,
                           commandReply    = [CmdRep]} ->
		CmdRep;
	    _ ->
		throw({error, {invalid_actionReplies, AR}})
	end,
    SCR = 
	case CR of
            {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    SCRes = 
	case SCR of
            #'ServiceChangeReply'{terminationID       = _TermID,
                                  serviceChangeResult = ServChRes} ->
		ServChRes;
	    _ ->
		throw({error, {invalid_serviceChangeReply, SCR}})
	end,
    SCRP = 
	case SCRes of
            {serviceChangeResParms, Parms} ->
		Parms;
	    _ ->
		throw({error, {invalid_serviceChangeResult, SCRes}})
	end,
    case SCRP of
	#'ServiceChangeResParm'{serviceChangeMgcId = _MgcMid} ->
            {ok, M};
	_ ->
	    {error, {invalid_serviceChangeResParms, SCRP}}
    end;
pap_mg_verify_service_change_rep(Crap) ->
    {error, {invalid_message, Crap}}.

pap_mg_verify_pending_msg_fun(TransId) ->
    fun(Msg) -> (catch pap_mg_verify_pending(Msg, TransId)) end.

pap_mg_verify_pending(#'MegacoMessage'{mess = Mess} = M, TransId) ->
    io:format("pap_mg_verify_pending -> entry with"
	      "~n   M:       ~p"
	      "~n   TransId: ~p"
	      "~n", [M, TransId]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TP = 
	case Trans of
            {transactionPending, TransPending} ->
		TransPending;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    case TP of
	#'TransactionPending'{transactionId = TransId} ->
	    {ok, M};
	_ ->
	    {error, {invalid_transactionPending, TP}}
    end;
pap_mg_verify_pending(Crap, _TransId) ->
    {error, {invalid_message, Crap}}.

pap_mg_verify_notify_rep_msg_fun(TermId, TransId, Rid, Cid) ->
    fun(Msg) -> (catch pap_mg_verify_notify_rep(Msg, 
						  TermId, 
						  TransId, Rid, Cid)) end.

pap_mg_verify_notify_rep(#'MegacoMessage'{mess = Mess} = M,
			   TermId, TransId, Rid, Cid) ->
    io:format("pap_mg_verify_notify_rep -> entry with"
	      "~n   M:       ~p"
	      "~n   TermId:  ~p"
	      "~n   TransId: ~p"
	      "~n   Rid:     ~p"
	      "~n   Cid:     ~p"
	      "~n", [M, TermId, TransId, Rid, Cid]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionReply, TransReply} ->
		TransReply;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    TRes = 
	case TR of
            #'TransactionReply'{transactionId     = TransId,
                                immAckRequired    = 'NULL',
                                transactionResult = TransRes} ->
		TransRes;
	    _ ->
		throw({error, {invalid_transactionReply, TR}})
	end,
    AR = 
	case TRes of
            {actionReplies, [ActRes]} ->
		ActRes;
	    _ ->
		throw({error, {invalid_transactionResult, TRes}})
	end,
    CR = 
	case AR of
            #'ActionReply'{contextId       = Cid,
                           errorDescriptor = asn1_NOVALUE,
                           contextReply    = _CtxReq,
                           commandReply    = [CmdRep]} ->
		CmdRep;
	    _ ->
		throw({error, {invalid_actionReplies, AR}})
	end,
    NR = 
	case CR of
            {notifyReply, NotifyReply} ->
		NotifyReply;
	    _ ->
		throw({error, {invalid_commandReply, CR}})
	end,
    case NR of
	#'NotifyReply'{terminationID   = [TermId],
		       errorDescriptor = asn1_NOVALUE} ->
	    {ok, M};
	_ ->
	    {error, {invalid_notifyReply, NR}}
    end;
pap_mg_verify_notify_rep(Crap, _TermId, _TransId, _Rid, _Cid) ->
    {error, {invalid_message, Crap}}.

pap_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

pap_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = pap_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

pap_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

pap_mg_notify_request_msg(Mid, TermId, TransId, Rid, Cid) ->
    AR      = pap_mg_notify_request_ar(Rid, TermId, Cid),
    TR      = cre_transReq(TransId, [AR]),
    Trans   = cre_transaction(TR),
    Mess    = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

pap_mg_trans_ack_msg(Mid, TransId) ->
    TR    = cre_transRespAck(cre_transAck(TransId)),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

request_and_pending_and_late_reply(suite) ->
    [];
request_and_pending_and_late_reply(doc) ->
    ["Receive a request and handle it as a long request, "
     "i.e. return with {pending, _}. Then, expect the sender "
     "to keep re-sending the request until the reply is sent."];
request_and_pending_and_late_reply(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        rapalr),
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
    MgcEvSeq = rapalr_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    MgcTag = megaco_test_generator:tcp(Mgc, MgcEvSeq),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("[MG] start the simulator (generator)"),
    {ok, Mg} = megaco_test_generator:start_link("MG", MgNode),

    d("[MG] create the event sequence"),
    MgEvSeq = rapalr_mg_event_sequence(text, tcp),

    i("wait some time before starting the MG simulation"),
    sleep(1000),

    d("[MG] start the simulation"),
    _MgTag = megaco_test_generator:megaco(Mg, MgEvSeq),

    d("[MGC] await the generator reply"),
    case megaco_test_generator:await_reply(MgcTag, Mgc) of
        {ok, MgcReply} ->
            d("[MGC] OK => MgcReply: ~n~p", [MgcReply]),
            ok;
        {error, MgcReply} ->
            d("[MGC] ERROR => MgcReply: ~n~p", [MgcReply]),
            ?ERROR(mgc_failed)
    end,

    d("[MG] await the generator reply"),
    case megaco_test_generator:megaco_await_reply(Mg) of
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

rapalr_mgc_event_sequence(text, tcp) ->
    DecodeFun = rapalr_mgc_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = rapalr_mgc_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid       = {deviceName,"mgc"},
    ScrVerifyFun     = rapalr_mgc_verify_service_change_req_msg_fun(),
    ServiceChangeRep = rapalr_mgc_service_change_reply_msg(Mid, 1),
    TermId           = 
	#megaco_term_id{id = ["00000000","00000000","01101101"]},
    TransId   = 2,
    ReqId     = 1,
    CtxId     = 1, 
    NrVerifyFun  = 
	rapalr_mgc_verify_notify_req_msg_fun(TermId, TransId, ReqId, CtxId),
    Pending      = rapalr_mgc_trans_pending_msg(Mid, TransId),
    NotifyRep    = rapalr_mgc_notify_reply_msg(Mid, TransId, 
					       CtxId, TermId),
    AckVerifyFun = rapalr_mgc_verify_trans_ack_msg_fun(TransId),
    EvSeq = [{debug,  false},
             {decode, DecodeFun},
             {encode, EncodeFun},
             {listen, 2944},
	     {expect_accept, any},
             {expect_receive, "service-change-request", {ScrVerifyFun, 5000}},
             {send, "service-change-reply", ServiceChangeRep},
             {expect_receive, "notify-request(1)", {NrVerifyFun, 4000}},
             {send, "pending", Pending},
             {expect_receive, "notify-request(2)", {NrVerifyFun, 4000}},
             {expect_receive, "notify-request(3)", {NrVerifyFun, 4000}},
             {send, "notify reply", NotifyRep},
             {expect_receive, "ack", {AckVerifyFun, 4000}},
             {sleep, 1000},
             disconnect
            ],
    EvSeq.

rapalr_mgc_encode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:encode_message(Conf, M)
    end.

rapalr_mgc_decode_msg_fun(Mod, Conf) ->
    fun(M) ->
            Mod:decode_message(Conf, M)
    end.

rapalr_mgc_verify_service_change_req_msg_fun() ->
    fun(Msg) -> 
	    (catch rapalr_mgc_verify_service_change_req(Msg)) 
    end.

rapalr_mgc_verify_service_change_req(#'MegacoMessage'{mess = Mess} = M) ->
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _MgMid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionRequest, TransRequest} ->
		TransRequest;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    AR = 
	case TR of
            #'TransactionRequest'{transactionId = _TransId,
				  actions       = [ActionReq]} ->
		ActionReq;
	    _ ->
		throw({error, {invalid_transactionRequest, TR}})
	end,
    CR = 
	case AR of
	    #'ActionRequest'{contextId       = _Cid, 
			     commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		throw({error, {invalid_action, AR}})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command; 
	    _ ->
		throw({error, {invalid_commandRequest, CR}})
	end,
    {Tid, Parms} = 
	case Cmd of
	    {serviceChangeReq, 
	     #'ServiceChangeRequest'{terminationID      = [TermID],
				     serviceChangeParms = ServChParms}} ->
		{TermID, ServChParms};
	    _ ->
		throw({error, {invalid_command, Cmd}})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    throw({error, {invalid_terminationID, Tid}})
    end,
    case Parms of
	#'ServiceChangeParm'{serviceChangeMethod = restart,
			     serviceChangeReason = [[$9,$0,$1|_]]} ->
	    {ok, M};
	_ ->
	    {error, {invalid_serviceChangeParms, Parms}}
    end.

rapalr_mgc_verify_notify_req_msg_fun(TermId, TransId, Rid, Cid) ->
    fun(Msg) -> 
	    (catch rapalr_mgc_verify_notify_req(Msg, 
						TermId, TransId, Rid, Cid)) 
    end.

rapalr_mgc_verify_notify_req(#'MegacoMessage'{mess = Mess} = M,
			     TermId, TransId, Rid, Cid) ->
    io:format("rapalr_mgc_verify_notify_req -> entry with"
	      "~n   M:       ~p"
	      "~n   TermId:  ~p"
	      "~n   TransId: ~p"
	      "~n   Rid:     ~p"
	      "~n   Cid:     ~p"
	      "~n", [M, TermId, TransId, Rid, Cid]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TR = 
	case Trans of
            {transactionRequest, TransRequest} ->
		TransRequest;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    AR = 
	case TR of
            #'TransactionRequest'{transactionId = TransId,
				  actions       = [ActReq]} ->
		ActReq;
	    _ ->
		throw({error, {invalid_transactionRequest, TR}})
	end,
    CR = 
	case AR of
	    #'ActionRequest'{contextId       = Cid,
			     commandRequests = [CmdReq]} ->
		CmdReq;
	    _ ->
		throw({error, {invalid_actions, AR}})
	end,
    Cmd = 
	case CR of
	    #'CommandRequest'{command = Command} ->
		Command;
	    _ ->
		throw({error, {invalid_commandRequests, CR}})
	end,
    NR = 
	case Cmd of
	    {notifyReq, NotifReq} ->
		NotifReq;
	    _ ->
		throw({error, {invalid_command, Cmd}})
	end,
    OED = 
	case NR of
	    #'NotifyRequest'{terminationID            = [TermId],
			     observedEventsDescriptor = ObsEvsDesc,
			     errorDescriptor          = asn1_NOVALUE} ->
		ObsEvsDesc;
	    _ ->
		throw({error, {invalid_notifyReq, NR}})
	end,
    OE = 
	case OED of 
	    #'ObservedEventsDescriptor'{observedEventLst = [ObsEvLst]} ->
		ObsEvLst;
	    _ ->
		throw({error, {invalid_observedEventsDescriptor, OED}})
	end,
    case OE of
	#'ObservedEvent'{eventName = "al/of"} ->
	    {ok, M};
	_ ->
	    throw({error, {invalid_observedEventLst, OE}})
    end;
rapalr_mgc_verify_notify_req(Crap, _TermId, _TransId, _Rid, _Cid) ->
    {error, {invalid_MegacoMessage, Crap}}.

rapalr_mgc_verify_trans_ack_msg_fun(TransId) ->
    fun(Msg) -> 
	    (catch rapalr_mgc_verify_trans_ack(Msg, TransId)) 
    end.

rapalr_mgc_verify_trans_ack(#'MegacoMessage'{mess = Mess} = M, TransId) ->
    io:format("rapalr_mgc_verify_trans_ack -> entry with"
	      "~n   M:       ~p"
	      "~n   TransId: ~p"
	      "~n", [M, TransId]),
    Body = 
	case Mess of 
	    #'Message'{version     = ?VERSION,
                       mId         = _Mid,
                       messageBody = MsgBody} ->
		MsgBody;
	    _ ->
		throw({error, {invalid_Message, Mess}})
	end,
    Trans = 
	case Body of
            {transactions, [Transactions]} ->
		Transactions;
	    _ ->
		throw({error, {invalid_messageBody, Body}})
	end,
    TA = 
	case Trans of
            {transactionResponseAck, [TransAck]} ->
		TransAck;
	    _ ->
		throw({error, {invalid_transactions, Trans}})
	end,
    case TA of
            #'TransactionAck'{firstAck = TransId,
			      lastAck  = asn1_NOVALUE} ->
		{ok, M};
	    _ ->
		throw({error, {invalid_transactionResponseAck, TA}})
    end;
rapalr_mgc_verify_trans_ack(Crap, _TransId) ->
    {error, {invalid_MegacoMessage, Crap}}.

rapalr_mgc_service_change_reply_msg(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    AR    = cre_actionReply(Cid, [CR]),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(1, TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

rapalr_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).

rapalr_mgc_notify_reply_msg(Mid, TransId, Cid, TermId) ->
    AR    = rapalr_mgc_notify_reply_ar(Cid, TermId),
    TRes  = cre_transResult([AR]),
    TR    = cre_transReply(TransId, 'NULL', TRes),
    Trans = cre_transaction(TR),
    Mess  = cre_message(?VERSION, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

rapalr_mgc_trans_pending_msg(Mid, TransId) ->
    TP   = #'TransactionPending'{transactionId = TransId},
    Body = {transactions, [{transactionPending, TP}]},
    Mess = #'Message'{version     = 1,
		      mId         = Mid,
		      messageBody = Body},
    #'MegacoMessage'{mess = Mess}.



%%
%% MG generator stuff
%%
rapalr_mg_event_sequence(text, tcp) ->
    Mid = {deviceName,"mg"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    LReqTmr = #megaco_incr_timer{wait_for    = 3000,
				 factor      = 1, 
				 incr        = 0,
				 max_retries = 2
				},    
    ServiceChangeReq = rapalr_mg_service_change_request_ar(Mid, 1),
    ConnectVerify = rapalr_mg_verify_handle_connect_fun(),
    ServiceChangeReplyVerify = rapalr_mg_verify_service_change_reply_fun(),
    Tid = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NotifyReq = rapalr_mg_notify_request_ar(1, Tid, 1),
    NotifyReplyVerify = rapalr_mg_verify_notify_reply_fun(),
    EvSeq = [
             {debug, false},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
             start_transport,
             {megaco_trace, disable},
             {megaco_system_info, users},
             {megaco_system_info, connections},
             {megaco_update_user_info, long_request_resend, true},
	     {megaco_update_user_info, long_request_timer,  LReqTmr}, 
             connect,
             {megaco_callback, handle_connect, ConnectVerify},
             megaco_connect,
             {megaco_cast,     [ServiceChangeReq], []},
             {megaco_callback, handle_connect,     ConnectVerify},
             {megaco_callback, handle_trans_reply, ServiceChangeReplyVerify},
             {sleep, 1000},
             {megaco_cast,     [NotifyReq],        []},
             {megaco_callback, handle_trans_reply, NotifyReplyVerify},
             {sleep, 1000},
             megaco_stop_user,
             megaco_stop,
             {sleep, 1000}
            ],
    EvSeq.


rapalr_mg_verify_handle_connect_fun() ->
    fun(Ev) -> rapalr_mg_verify_handle_connect(Ev) end.

rapalr_mg_verify_handle_connect({handle_connect, CH, ?VERSION}) -> 
    io:format("rapalr_mg_verify_handle_connect -> ok"
	      "~n   CH: ~p~n", [CH]),
    {ok, CH, ok};
rapalr_mg_verify_handle_connect(Else) ->
    io:format("rapalr_mg_verify_handle_connect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


rapalr_mg_verify_service_change_reply_fun() ->
    fun(Rep) -> rapalr_mg_verify_scr(Rep) end.

rapalr_mg_verify_scr({handle_trans_reply, _CH, ?VERSION, {ok, [AR]}, _}) ->
    (catch rapalr_mg_do_verify_scr(AR));
rapalr_mg_verify_scr(Crap) ->
    {error, Crap, ok}.

rapalr_mg_do_verify_scr(AR) ->
    io:format("rapalr_mg_verify_scr -> ok"
	      "~n   AR: ~p~n", [AR]),
    CR = 
	case AR of
	    #'ActionReply'{commandReply = [CmdRep]} ->
		CmdRep;
	    _ ->
		Reason1 = {invalid_action_reply, AR},
		throw({error, Reason1, ok})
	end,
    SCR = 
	case CR of
	    {serviceChangeReply, ServChRep} ->
		ServChRep;
	    _ ->
		Reason2 = {invalid_command_reply, CR},
		throw({error, Reason2, ok})
	end,
    {Tid, SCRes} = 
	case SCR of
	    #'ServiceChangeReply'{terminationID       = [TermID],
				  serviceChangeResult = Res} ->
		{TermID, Res};
	    _ ->
		Reason3 = {invalid_service_change_reply, SCR},
		throw({error, Reason3, ok})
	end,
    case Tid of
	#megaco_term_id{contains_wildcards = false, id = ["root"]} ->
	    ok;
	_ ->
	    Reason4 = {invalid_termination_id, Tid},
	    throw({error, Reason4, ok})
    end,
    SCRParm = 
	case SCRes of
	    {serviceChangeResParms, ServChResParms} ->
		ServChResParms;
	    _ ->
		Reason5 = {invalid_serviceChangeResult, SCRes},
		throw({error, Reason5, ok})
	end,
    case SCRParm of
	#'ServiceChangeResParm'{serviceChangeMgcId = _RemoteMid} ->
	    {ok, AR, ok};
	_ ->
	    Reason6 = {invalid_service_change_result, SCRParm},
	    {error, Reason6, ok}
    end.

rapalr_mg_verify_notify_reply_fun() ->
    fun(Rep) -> rapalr_mg_verify_notify_reply(Rep) end.
	     
rapalr_mg_verify_notify_reply({handle_trans_reply, _CH, ?VERSION, 
			      {ok, [AR]}, _}) ->
    io:format("rapalr_mg_verify_notify_reply -> ok"
	      "~n   AR: ~p~n", [AR]),
    {ok, AR, ok};
rapalr_mg_verify_notify_reply(Else) ->
    io:format("rapalr_mg_verify_notify_reply -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.


rapalr_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

rapalr_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dist(suite) ->
    [];
dist(Config) when is_list(Config) ->
    [_Local, Dist] = ?ACQUIRE_NODES(2, Config),
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
    MgRH = user_info(MgMid, receive_handle),

    d("dist -> retrieve (user info) receive_handle for MGC",[]),
    MgcRH = user_info(MgcMid, receive_handle), 

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
    ?USER({connect, PrelMgCH, _V, []}, ok),
    ?RECEIVE([{res, _, {ok, PrelMgCH}}]),

    d("dist -> (MG) send service change request",[]),
    Req = service_change_request(),
    ?SEND(megaco:call(PrelMgCH, [Req], [])),

    d("dist -> (MGC) await auto-connect",[]),
    ?USER({connect, MgcCH, _V, []}, ok), % Mgc auto


    Rep = service_change_reply(MgcMid),
    d("dist -> (MGC) "
      "await service change request and send reply when received",[]),
    ?USER({request, MgcCH, _V, [[Req]]}, {discard_ack, [Rep]}),

    d("dist -> (MG) await connect",[]),
    ?USER({connect, MgCH, _V, []}, ok), % Mg confirm

    d("dist -> (MG) await service change reply",[]),
    ?RECEIVE([{res, _, {1, {ok, [Rep]}}}]),

    %% Dist
    d("dist -> start megaco on ~p", [Dist]),
    ?VERIFY(ok,	rpc:call(Dist, megaco, start, [])),

    d("dist -> start megaco user on ~p", [Dist]),
    ?VERIFY(ok,	rpc:call(Dist, megaco, start_user, [MgcMid, UserConfig])),

    d("dist -> (MG) connect to MGC", []),
    MgcPid = self(),
    MgcSH  = {element(2, MgSH), element(1, MgSH)},
    ?SEND(rpc:call(Dist, megaco, connect, [MgcRH, MgMid, MgcSH, MgcPid])), % Mgc dist

    d("dist -> (MGC) await auto-connect (from MG on ~p)", [Dist]),
    ?USER({connect, MgcCH, _V, []}, ok), % Mgc dist auto
    ?RECEIVE([{res, _, {ok, MgcCH}}]),

    d("dist -> (~p:MG) send service change request",[Dist]),
    ?SEND(rpc:call(Dist, megaco, call, [MgcCH, [Req], []])),

    d("dist -> (MG????????) "
      "await service change request and send reply when received",[]),
    ?USER({request, MgCH, _V, [[Req]]}, {discard_ack, [Rep]}),
    ?RECEIVE([{res, _, {1, {ok, [Rep]}}}]),

    d("dist -> retreive some info",[]),
    connections([MgCH, MgcCH]),
    ?VERIFY([MgCH], megaco:user_info(MgMid, connections)),
    ?VERIFY([MgcCH], megaco:user_info(MgcMid, connections)),

    ?VERIFY([MgcCH], rpc:call(Dist, megaco, system_info, [connections])),
    ?VERIFY([], rpc:call(Dist, megaco, user_info, [MgMid, connections])),
    ?VERIFY([MgcCH], rpc:call(Dist, megaco, user_info, [MgcMid, connections])),

    %% Shutdown

    d("dist -> close down the shop...",[]),
    Reason = shutdown,
    ?SEND(megaco:disconnect(MgCH, Reason)),
    ?USER({disconnect, MgCH, _V, [{user_disconnect, Reason}]}, ok),
    ?RECEIVE([{res, _, ok}]),
    ?VERIFY(ok,	megaco:stop_user(MgMid)),

    ?SEND(megaco:disconnect(MgcCH, Reason)),
    ?USER({disconnect, MgcCH, _V, [{user_disconnect, Reason}]}, ok),
    ?USER({disconnect, MgcCH, _V, [{user_disconnect, Reason}]}, ok),
    ?RECEIVE([{res, _, ok}]),
    ?VERIFY(ok,	megaco:stop_user(MgcMid)),

    ?VERIFY(ok, application:stop(megaco)),
    ?RECEIVE([]),
    d("dist -> done",[]),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_4359(suite) ->
    [];
otp_4359(Config) when is_list(Config) ->
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
    RH0 = user_info(Mid, receive_handle),
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
    stop_user(Mid),
    ?VERIFY(ok, application:stop(megaco)),
    ?RECEIVE([]),
    ok.


otp_4359_await_actions(Exp) ->
    otp_4359_await_actions(Exp, []).

otp_4359_await_actions([], Rep) ->
    lists:reverse(Rep);
otp_4359_await_actions([{M,I}|R] = _All, Rep) ->
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

otp_4359_analyze_result(_RH, []) ->
    ok;
otp_4359_analyze_result(RH, 
			[{send_message, ExpErrorCode, EncodedMessage}|L]) ->
    io:format("otp_4359_analyze_result -> send_message: ", []),
    otp_4359_analyze_encoded_message(RH, ExpErrorCode, EncodedMessage),
    otp_4359_analyze_result(RH,L);
otp_4359_analyze_result(RH, [{M,ignore,_}|T]) ->
    io:format("otp_4359_analyze_result -> ignoring ~p~n", [M]),
    otp_4359_analyze_result(RH,T).

otp_4359_analyze_encoded_message(RH, ExpErrorCode, M) 
  when is_record(RH, megaco_receive_handle), binary(M) ->
    #megaco_receive_handle{encoding_mod    = Mod,
			   encoding_config = Conf} = RH,
    case (catch Mod:decode_message(Conf, M)) of
	{ok, #'MegacoMessage'{mess = #'Message'{messageBody = Body}}} ->
	    case Body of
		{transactions, [{transactionReply,Reply}]} ->
		    case Reply of
			#'TransactionReply'{transactionResult = Result} ->
			    case Result of
				{transactionError,ED} when is_record(ED, 'ErrorDescriptor') ->
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

otp_4836(suite) ->
    [];
otp_4836(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        otp_4836),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p", 
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    d("start the MGC simulator (generator)"),
    {ok, Mgc} = megaco_test_generator:start_link("MGC", MgcNode),

    d("create the MGC event sequence"),
    MgcEvSeq = otp_4836_mgc_event_sequence(text, tcp),

    d("start the MGC simulation"),
    megaco_test_generator:tcp(Mgc, MgcEvSeq),

    i("[MG] start"),    
    MgMid = {deviceName, "mg"},
    ReqTmr = #megaco_incr_timer{wait_for    = 3000,
                                factor      = 1, 
                                incr        = 0,
                                max_retries = 3},
    %% 5000,  %% Does not matter since we will not use this anyway...
    LongReqTmr = #megaco_incr_timer{wait_for    = 3000,
                                    factor      = 1, 
                                    incr        = 0,
                                    max_retries = 3},
    PendingTmr = 10000,
    ReplyTmr = 16000,
    MgConfig = [{request_timer,      ReqTmr}, 
                {long_request_timer, LongReqTmr}, 
                {pending_timer,      PendingTmr},
                {reply_timer,        ReplyTmr}],
    {ok, Mg} = ?MG_START(MgNode, MgMid, text, tcp, MgConfig, ?MG_VERBOSITY),

    i("[MG] connect to the MGC (service change)"),    
    ServChRes = ?MG_SERV_CHANGE(Mg),
    d("service change result: ~p", [ServChRes]),

    d("[MG] send the notify"),
    {ok, Reply} = (catch ?MG_NOTIF_RAR(Mg)),
    {1, {ok, [AR]}} = Reply,
    d("[MG] ActionReply: ~p", [AR]),

    d("[MGC] await the tcp reply"),
    {ok, TcpReply} = megaco_test_generator:tcp_await_reply(Mgc),
    d("[MGC] TcpReply: ~p", [TcpReply]),

    %% Tell MG to stop
    i("[MG] stop"),
    ?MG_STOP(Mg),

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_generator:stop(Mgc),

    i("done", []),
    ok.


otp_4836_mgc_event_sequence(text, tcp) ->
    DecodeFun = otp_4836_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = otp_4836_encode_msg_fun(megaco_pretty_text_encoder, []),
    ServiceChangeVerifyFun = otp_4836_verify_service_change_req_msg_fun(),
    Mid = {deviceName,"ctrl"},
    ServiceChangeReply = otp_4836_service_change_reply_msg(Mid, 1, 0),
    NotifyReqVerifyFun = otp_4836_verify_notify_request_fun(),
    TermId = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NotifyReply = otp_4836_notify_reply_msg(Mid, 2, 0, TermId),
    Pending = otp_4836_pending_msg(Mid,2),
    MgcEvSeq = [{debug,  true},
		{decode, DecodeFun},
		{encode, EncodeFun},
		{listen, 2944},
		{expect_accept, any},
		{expect_receive, "service-change-request", {ServiceChangeVerifyFun, 10000}}, 
		{send, "service-change-reply", ServiceChangeReply}, 
		{expect_receive, "notify-request", {NotifyReqVerifyFun, 10000}},
		{send, "pending 1", Pending}, 
		{sleep, 100},
		{send, "pending 2", Pending}, 
		{sleep, 500},
  		{send, "notify-reply", NotifyReply}, 
		{sleep, 2000}
	       ],
    MgcEvSeq.

otp_4836_service_change_reply_msg(Mid, TransId, Cid) ->
    SCRP  = #'ServiceChangeResParm'{serviceChangeMgcId = Mid},
    SCRPs = {serviceChangeResParms,SCRP},
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = #'ServiceChangeReply'{terminationID       = [Root],
				  serviceChangeResult = SCRPs},
    CR    = {serviceChangeReply, SCR},
    otp_4836_msg(Mid, TransId, CR, Cid).

otp_4836_notify_reply_msg(Mid, TransId, Cid, TermId) ->
    NR  = #'NotifyReply'{terminationID = [TermId]},
    CR  = {notifyReply, NR},
    otp_4836_msg(Mid, TransId, CR, Cid).

otp_4836_msg(Mid, TransId, CR, Cid) ->
    AR  = #'ActionReply'{contextId    = Cid,
			 commandReply = [CR]},
    ARs  = {actionReplies, [AR]},
    TR   = #'TransactionReply'{transactionId     = TransId,
			       transactionResult = ARs},
    Body = {transactions, [{transactionReply, TR}]},
    Mess = #'Message'{version     = 1,
		      mId         = Mid,
		      messageBody = Body},
    #'MegacoMessage'{mess = Mess}.


otp_4836_pending_msg(Mid, TransId) ->
    TP   = #'TransactionPending'{transactionId = TransId},
    Body = {transactions, [{transactionPending, TP}]},
    Mess = #'Message'{version     = 1,
		      mId         = Mid,
		      messageBody = Body},
    #'MegacoMessage'{mess = Mess}.


otp_4836_encode_msg_fun(Mod, Conf) ->
    fun(M) -> 
	    Mod:encode_message(Conf, M) 
    end.
otp_4836_encode_msg_fun(Mod, Conf, Ver) ->
    fun(M) -> 
	    Mod:encode_message(Conf, Ver, M) 
    end.

otp_4836_decode_msg_fun(Mod, Conf) ->
    fun(M) -> 
	    Mod:decode_message(Conf, M) 
    end.
otp_4836_decode_msg_fun(Mod, Conf, Ver) ->
    fun(M) -> 
	    Mod:decode_message(Conf, Ver, M) 
    end.

otp_4836_verify_msg_fun() ->
    fun(M) -> {ok, M} end.

otp_4836_verify_service_change_req_msg_fun() ->
    fun(#'MegacoMessage'{mess = Mess} = M) -> 
	    #'Message'{version     = _V,
		       mId         = _Mid,
		       messageBody = Body} = Mess,
	    {transactions, [Trans]} = Body,
	    {transactionRequest, TR} = Trans,
	    #'TransactionRequest'{transactionId = _Tid,
				  actions = [AR]} = TR,
	    #'ActionRequest'{contextId = _Cid,
			     contextRequest = _CtxReq,
			     contextAttrAuditReq = _CtxAar,
			     commandRequests = [CR]} = AR,
	    #'CommandRequest'{command = Cmd,
			      optional = _Opt,
			      wildcardReturn = _WR} = CR,
	    {serviceChangeReq, SCR} = Cmd,
	    #'ServiceChangeRequest'{terminationID = _TermID,
				    serviceChangeParms = SCP} = SCR,
	    #'ServiceChangeParm'{serviceChangeMethod = restart,
				 serviceChangeReason = [[$9,$0,$1|_]]} = SCP,
	    {ok, M};
       (M) ->
	    {error, {invalid_message, M}}
    end.

otp_4836_verify_notify_request_fun() ->
    fun(#'MegacoMessage'{mess = Mess} = M) -> 
	    #'Message'{messageBody = Body} = Mess,
	    {transactions, [Trans]} = Body,
	    {transactionRequest, TR} = Trans,
	    #'TransactionRequest'{actions = [AR]} = TR,
	    #'ActionRequest'{commandRequests = [CR1,CR2]} = AR,
	    #'CommandRequest'{command = Cmd1} = CR1,
	    {notifyReq, NR1} = Cmd1,
	    #'NotifyRequest'{observedEventsDescriptor = OED1} = NR1,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE1]} = OED1,
	    #'ObservedEvent'{eventName = "al/of"} = OE1,
	    #'CommandRequest'{command = Cmd2} = CR2,
	    {notifyReq, NR2} = Cmd2,
	    #'NotifyRequest'{observedEventsDescriptor = OED2} = NR2,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE2]} = OED2,
	    #'ObservedEvent'{eventName = "al/of"} = OE2,
	    {ok, M};
       (M) ->
	    {error, {invalid_message, M}}
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_5805(suite) ->
    [];
otp_5805(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        otp_5805),
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
    MgcEvSeq = otp_5805_mgc_event_sequence(text, tcp),

    i("wait some time before starting the MGC simulation"),
    sleep(1000),

    d("[MGC] start the simulation"),
    megaco_test_generator:megaco(Mgc, MgcEvSeq, timer:minutes(1)),

    i("wait some time before starting the MG simulator"),
    sleep(1000),

    d("start the MG simulator (generator)"),
    {ok, Mg} = megaco_test_generator:start_link("MG", MgNode),

    d("create the MG event sequence"),
    MgEvSeq = otp_5805_mg_event_sequence(text, tcp),

    d("start the MG simulation"),
    megaco_test_generator:tcp(Mg, MgEvSeq, timer:minutes(1)),

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


otp_5805_mg_event_sequence(text, tcp) ->
    DecodeFun = otp_5805_mg_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = otp_5805_mg_encode_msg_fun(megaco_pretty_text_encoder, []),
    Mid = {deviceName,"mg"},
    ServiceChangeReq = otp_5805_mg_service_change_request_msg(Mid, 1, 0),
    ServiceChangeReplyVerifyFun = 
	otp_5805_mg_verify_service_change_rep_msg_fun(),
    NotifyReqNNV = otp_5805_mg_notify_request_msg("1"),
    NotifyReqUV = otp_5805_mg_notify_request_msg("4"),
    EDVerify = otp_5805_mg_verify_error_descriptor_msg_fun(),
    MgEvSeq = [{debug,  true},
	       {decode, DecodeFun},
	       {encode, EncodeFun},
	       {connect, 2944},

	       {send, "service-change-request", ServiceChangeReq}, 
	       {expect_receive, "service-change-reply", {ServiceChangeReplyVerifyFun, 5000}}, 
	       {sleep, 1000},
	       {send, "notify request (not negotiated version)", NotifyReqNNV}, 
	       {expect_receive, "error-descriptor", EDVerify}, 
	       {sleep, 1000},
 	       {send, "notify request (unsupported version)", NotifyReqUV}, 
	       {expect_receive, "error-descriptor", EDVerify}, 

	       {expect_nothing, 4000},
	       disconnect
	      ],
    MgEvSeq.

otp_5805_mg_encode_msg_fun(Mod, Conf) ->
    fun(M) -> 
            Mod:encode_message(Conf, M) 
    end.

otp_5805_mg_decode_msg_fun(Mod, Conf) ->
    fun(M) -> 
            Mod:decode_message(Conf, M) 
    end.

otp_5805_mg_verify_service_change_rep_msg_fun() ->
    fun(#'MegacoMessage'{mess = Mess} = M) -> 
            case Mess of
		#'Message'{version     = 1,
			   mId         = _MgMid,
			   messageBody = Body} ->
		    case Body of
			{transactions, [Trans]} ->
			    case Trans of
				{transactionReply, TR} ->
				    case TR of
					#'TransactionReply'{transactionId = _Tid,
							    immAckRequired = asn1_NOVALUE,
							    transactionResult = Res} ->
					    case Res of
						{actionReplies, [AR]} ->
						    case AR of
							#'ActionReply'{contextId = _Cid,
								       errorDescriptor = asn1_NOVALUE,
								       contextReply    = _CtxReq,
								       commandReply    = [CR]} ->
							    case CR of
								{serviceChangeReply, SCR} ->
								    case SCR of
									#'ServiceChangeReply'{
									       terminationID       = _TermID,
									       serviceChangeResult = SCRes} ->
									    case SCRes of
										{serviceChangeResParms, SCRP} ->
										    case SCRP of
											#'ServiceChangeResParm'{
												serviceChangeMgcId   = _MgcMid,
												serviceChangeVersion = 2} ->
											    {ok, M};
											_ ->
											    {error, {invalid_scrp, SCRP}}
										    end;
										_ ->
										    {error, {invalid_scres, SCRes}}
									    end;
									_ ->
									    {error, {invalid_scr, SCR}}
								    end;
								_ ->
								    {error, {invalid_cr, CR}}
							    end;
							_ ->
							    {error, {invalid_ar, AR}}
						    end;
						_ ->
						    {error, {invalid_tres, Res}}
					    end;
					_ ->
					    {error, {invalid_tr, TR}}
				    end;
				_ ->
				    {error, {invalid_trans, Trans}}
			    end;
			_ ->
			    {error, {invalid_body, Body}}
		    end;
		_ ->
		    {error, {invalid_mess, Mess}}
	    end;
       (M) ->
            {error, {invalid_message, M}}
    end.

otp_5805_mg_verify_error_descriptor_msg_fun() ->
    fun(#'MegacoMessage'{mess = Mess} = M) -> 
            case Mess of
		#'Message'{version     = 2,
			   mId         = _MgMid,
			   messageBody = Body} ->
		    io:format("otp_5805_mg_verify_error_descriptor_msg_fun -> ok"
			      "~n   Body:  ~p"
			      "~n", [Body]),
		    case Body of
			{messageError, ED} ->
			    case ED of
				#'ErrorDescriptor'{
				      errorCode = ?megaco_version_not_supported} ->
				    {ok, M};
				_ ->
				    {error, {invalid_ed, ED}}
			    end;
			_ ->
			    {error, {invalid_body, Body}}
		    end;
		_ ->
		    {error, {invalid_mess, Mess}}
	    end;
       (M) ->
            {error, {invalid_message, M}}
    end.

otp_5805_mg_service_change_request_ar(_Mid, Cid) ->
    Prof  = cre_serviceChangeProf("resgw", 1),
    SCP   = cre_serviceChangeParm(restart, 2, ["901 mg col boot"], Prof),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReq([Root], SCP),
    CMD   = cre_command(SCR),
    CR    = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

otp_5805_mg_service_change_request_msg(Mid, TransId, Cid) ->
    AR    = otp_5805_mg_service_change_request_ar(Mid, Cid),
    TR    = cre_transReq(TransId, [AR]),
    Trans = cre_transaction(TR),
    Mess  = cre_message(1, Mid, cre_transactions([Trans])),
    cre_megacoMessage(Mess).

otp_5805_mg_notify_request_ar(Rid, Tid, Cid) ->
    TT      = cre_timeNotation("19990729", "22000000"),
    Ev      = cre_obsEvent("al/of", TT),
    EvsDesc = cre_obsEvsDesc(Rid, [Ev]),
    NR      = cre_notifyReq([Tid], EvsDesc),
    CMD     = cre_command(NR),
    CR      = cre_cmdReq(CMD),
    cre_actionReq(Cid, [CR]).

otp_5805_mg_notify_request_msg(V) ->
    M = 
"MEGACO/" ++ V ++ " mg
Transaction = 2 {
	Context = 1 {
		Notify = 00000000/00000000/01101101 {
			ObservedEvents = 1 {
				19990729T22000000:al/of
			}
		}
	}
}",
    list_to_binary(M).


%%
%% MGC generator stuff
%% 
otp_5805_mgc_event_sequence(text, tcp) ->
    Mid = {deviceName,"ctrl"},
    RI = [
          {port,             2944},
          {encoding_module,  megaco_pretty_text_encoder},
          {encoding_config,  []},
          {transport_module, megaco_tcp}
         ],
    ConnectVerify = fun otp_5805_mgc_verify_handle_connect/1,
    ServiceChangeReqVerify = otp_5805_mgc_verify_service_change_req_fun(Mid),
    SyntaxErrorVerify1 = fun otp_5805_mgc_verify_handle_syntax_error/1,
    SyntaxErrorVerify2 = fun otp_5805_mgc_verify_handle_syntax_error/1,
    DiscoVerify = fun otp_5805_mgc_verify_handle_disconnect/1,
    EvSeq = [
             {debug, true},
	     {megaco_trace, disable},
	     {megaco_trace, max},
             megaco_start,
             {megaco_start_user, Mid, RI, []},
             start_transport,
             listen,
             {megaco_callback, handle_connect, ConnectVerify},
	     {megaco_conn_info, all},
             {megaco_callback, handle_trans_request_sc, ServiceChangeReqVerify},
	     {megaco_update_conn_info, protocol_version, 2}, 
             {megaco_callback, handle_syntax_error, SyntaxErrorVerify1},
             {megaco_callback, handle_syntax_error, SyntaxErrorVerify2},
             {megaco_callback, handle_disconnect, DiscoVerify},
             megaco_stop_user,
             megaco_stop
            ],
    EvSeq.

otp_5805_mgc_verify_handle_connect({handle_connect, CH, 1}) -> 
    io:format("otp_5805_mgc_verify_handle_connect -> ok"
	      "~n   CH:  ~p"
	      "~n", [CH]),
    {ok, CH, ok};
otp_5805_mgc_verify_handle_connect({handle_connect, CH, V}) -> 
    io:format("otp_5805_mgc_verify_handle_connect -> unexpected version"
	      "~n   CH:  ~p"
	      "~n   V:   ~p"
	      "~n", [CH, V]),
    {error, {unexpected_version, V}, ok};
otp_5805_mgc_verify_handle_connect(Else) ->
    {error, Else, ok}.


otp_5805_mgc_verify_service_change_req_fun(Mid) ->
    fun({handle_trans_request, _, V, [AR]}) ->
	    io:format("otp_5805_mgc_verify_service_change_req_fun -> ok so far"
		      "~n   V:   ~p"
		      "~n", [V]),
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
							 serviceChangeVersion = 2, 
                                                         serviceChangeReason = [[$9,$0,$1|_]]} ->
                                                    Reply = 
                                                        {discard_ack, 
                                                         [otp_5805_mgc_service_change_reply_ar(Mid, 1)]},
                                                    {ok, AR, Reply};
                                                _ ->
                                                    Err = {invalid_SCP, Parms},
                                                    ED = otp_5805_err_desc(Parms),
                                                    ErrReply = 
							{discard_ack, ED},
                                                    {error, Err, ErrReply}
                                            end;
                                        _ ->
                                            Err = {invalid_termination_id, Tid},
                                            ED = otp_5805_err_desc(Tid),
                                            ErrReply = {discard_ack, ED},
                                            {error, Err, ErrReply}
                                    end;
                                _ ->
                                    Err = {invalid_command, Cmd},
                                    ED = otp_5805_err_desc(Cmd),
                                    ErrReply = {discard_ack, ED},
                                    {error, Err, ErrReply}
                            end;
                        _ ->
                            Err = {invalid_command_request, CR},
                            ED = otp_5805_err_desc(CR),
                            ErrReply = {discard_ack, ED},
                            {error, Err, ErrReply}
                    end;
                _ ->
                    Err = {invalid_action_request, AR},
                    ED = otp_5805_err_desc(AR),
                    ErrReply = {discard_ack, ED},
                    {error, Err, ErrReply}
            end;
       (Else) ->
            ED = otp_5805_err_desc(Else),
            ErrReply = {discard_ack, ED},
            {error, Else, ErrReply}
    end.

otp_5805_mgc_verify_handle_syntax_error({handle_syntax_error, CH, _, ED}) 
  when is_record(ED, 'ErrorDescriptor') -> 
    io:format("otp_5805_mgc_verify_handle_syntax_error -> ok so far"
	      "~n   CH: ~p"
	      "~n   ED:  ~p"
	      "~n", [CH, ED]),
    case ED of
	#'ErrorDescriptor'{errorCode = ?megaco_version_not_supported} ->
	    {ok, CH, reply};
	#'ErrorDescriptor'{errorCode = Code} ->
	    {error, {invalid_errorCode, Code}, ok}
    end;
otp_5805_mgc_verify_handle_syntax_error(Else) ->
    io:format("otp_5805_mgc_verify_handle_syntax_error -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

otp_5805_mgc_verify_handle_disconnect({handle_disconnect, CH, V, _R}) -> 
    io:format("otp_5805_mgc_verify_handle_disconnect -> ok"
	      "~n   CH: ~p"
	      "~n   V:  ~p"
	      "~n   _R:  ~p"
	      "~n", [CH, V, _R]),
    {ok, CH, ok};
otp_5805_mgc_verify_handle_disconnect(Else) ->
    io:format("otp_5805_mgc_verify_handle_disconnect -> unknown"
	      "~n   Else: ~p~n", [Else]),
    {error, Else, ok}.

otp_5805_mgc_service_change_reply_ar(Mid, Cid) ->
    SCRP  = cre_serviceChangeResParm(Mid, 2),
    SCRes = cre_serviceChangeResult(SCRP),
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = cre_serviceChangeReply([Root], SCRes),
    CR    = cre_cmdReply(SCR),
    cre_actionReply(Cid, [CR]).

otp_5805_mgc_notify_reply_ar(Cid, TermId) ->
    NR    = cre_notifyReply([TermId]),
    CR    = cre_cmdReply(NR),
    cre_actionReply(Cid, [CR]).


otp_5805_err_desc(T) ->
    EC = ?megaco_internal_gateway_error,
    ET = lists:flatten(io_lib:format("~w",[T])),
    #'ErrorDescriptor'{errorCode = EC, errorText = ET}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_5881(suite) ->
    [];
otp_5881(Config) when is_list(Config) ->
    ?SKIP("deprecated by OTP-5887"),
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        otp_5881),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p", 
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    d("start the MGC simulator (generator)"),
    {ok, Mgc} = megaco_test_generator:start_link("MGC", MgcNode),

    d("create the MGC event sequence"),
    MgcEvSeq = otp_5881_mgc_event_sequence(text, tcp),

    d("start the MGC simulation"),
    megaco_test_generator:tcp(Mgc, MgcEvSeq),

    i("[MG] start"),    
    MgMid = {deviceName, "mg"},
    ReqTmr = #megaco_incr_timer{wait_for    = 3000,
                                factor      = 1, 
                                incr        = 0,
                                max_retries = 3},
    %% 5000,  %% Does not matter since we will not use this anyway...
    LongReqTmr = #megaco_incr_timer{wait_for    = 3000,
                                    factor      = 1, 
                                    incr        = 0,
                                    max_retries = 3},
    PendingTmr = 10000,
    ReplyTmr = 16000,
    MgConfig = [{request_timer,      ReqTmr}, 
                {long_request_timer, LongReqTmr}, 
                {pending_timer,      PendingTmr},
                {reply_timer,        ReplyTmr}],
    {ok, Mg} = ?MG_START(MgNode, MgMid, text, tcp, MgConfig, ?MG_VERBOSITY),

    i("[MG] verify transaction-id: undefined_serial"),    
    otp_5881_verify_trans_id(Mg, undefined_serial),
    
    i("[MG] connect to the MGC (service change)"),    
    ServChRes = ?MG_SERV_CHANGE(Mg),
    d("service change result: ~p", [ServChRes]),

    i("[MG] verify transaction-id: 1"),    
    otp_5881_verify_trans_id(Mg, 1),
    
    d("[MG] send the notify"),
    {ok, Reply} = (catch ?MG_NOTIF_RAR(Mg)),
    {1, {ok, [AR]}} = Reply,
    d("[MG] ActionReply: ~p", [AR]),

    i("[MG] verify transaction-id: 2"),    
    otp_5881_verify_trans_id(Mg, 2),
    
    d("[MGC] await the tcp reply"),
    {ok, TcpReply} = megaco_test_generator:tcp_await_reply(Mgc),
    d("[MGC] TcpReply: ~p", [TcpReply]),

    %% Tell MG to stop
    i("[MG] stop"),
    ?MG_STOP(Mg),

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_generator:stop(Mgc),

    i("done", []),
    ok.

otp_5881_verify_trans_id(Mg, Expected) ->
    case ?MG_CONN_INFO(Mg, trans_id) of
	Expected -> 
	    ok;
	ErroneousValue ->
	    throw({unexpected_transaction_id_value, ErroneousValue, Expected})
    end.
    

otp_5881_mgc_event_sequence(text, tcp) ->
    DecodeFun = otp_5881_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = otp_5881_encode_msg_fun(megaco_pretty_text_encoder, []),
    ServiceChangeVerifyFun = otp_5881_verify_service_change_req_msg_fun(),
    Mid = {deviceName,"ctrl"},
    ServiceChangeReply = otp_5881_service_change_reply_msg(Mid, 1, 0),
    NotifyReqVerifyFun = otp_5881_verify_notify_request_fun(),
    TermId = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NotifyReply = otp_5881_notify_reply_msg(Mid, 2, 0, TermId),
    %% Pending = otp_5881_pending_msg(Mid,2),
    MgcEvSeq = [{debug,  true},
		{decode, DecodeFun},
		{encode, EncodeFun},
		{listen, 2944},
		{expect_accept, any},
		{expect_receive, "service-change-request", {ServiceChangeVerifyFun, 10000}}, 
		{send, "service-change-reply", ServiceChangeReply}, 
		{expect_receive, "notify-request", {NotifyReqVerifyFun, 10000}},
  		{send, "notify-reply", NotifyReply},
		{sleep, 2000}
	       ],
    MgcEvSeq.

otp_5881_service_change_reply_msg(Mid, TransId, Cid) ->
    SCRP  = #'ServiceChangeResParm'{serviceChangeMgcId = Mid},
    SCRPs = {serviceChangeResParms,SCRP},
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = #'ServiceChangeReply'{terminationID       = [Root],
				  serviceChangeResult = SCRPs},
    CR    = {serviceChangeReply, SCR},
    otp_5881_msg(Mid, TransId, CR, Cid).

otp_5881_notify_reply_msg(Mid, TransId, Cid, TermId) ->
    NR  = #'NotifyReply'{terminationID = [TermId]},
    CR  = {notifyReply, NR},
    otp_5881_msg(Mid, TransId, CR, Cid).

otp_5881_msg(Mid, TransId, CR, Cid) ->
    AR  = #'ActionReply'{contextId    = Cid,
			 commandReply = [CR]},
    ARs  = {actionReplies, [AR]},
    TR   = #'TransactionReply'{transactionId     = TransId,
			       transactionResult = ARs},
    Body = {transactions, [{transactionReply, TR}]},
    Mess = #'Message'{version     = 1,
		      mId         = Mid,
		      messageBody = Body},
    #'MegacoMessage'{mess = Mess}.


otp_5881_pending_msg(Mid, TransId) ->
    TP   = #'TransactionPending'{transactionId = TransId},
    Body = {transactions, [{transactionPending, TP}]},
    Mess = #'Message'{version     = 1,
		      mId         = Mid,
		      messageBody = Body},
    #'MegacoMessage'{mess = Mess}.


otp_5881_encode_msg_fun(Mod, Conf) ->
    fun(M) -> 
	    Mod:encode_message(Conf, M) 
    end.
otp_5881_encode_msg_fun(Mod, Conf, Ver) ->
    fun(M) -> 
	    Mod:encode_message(Conf, Ver, M) 
    end.

otp_5881_decode_msg_fun(Mod, Conf) ->
    fun(M) -> 
	    Mod:decode_message(Conf, M) 
    end.
otp_5881_decode_msg_fun(Mod, Conf, Ver) ->
    fun(M) -> 
	    Mod:decode_message(Conf, Ver, M) 
    end.

otp_5881_verify_msg_fun() ->
    fun(M) -> {ok, M} end.

otp_5881_verify_service_change_req_msg_fun() ->
    fun(#'MegacoMessage'{mess = Mess} = M) -> 
	    #'Message'{version     = _V,
		       mId         = _Mid,
		       messageBody = Body} = Mess,
	    {transactions, [Trans]} = Body,
	    {transactionRequest, TR} = Trans,
	    #'TransactionRequest'{transactionId = _Tid,
				  actions = [AR]} = TR,
	    #'ActionRequest'{contextId = _Cid,
			     contextRequest = _CtxReq,
			     contextAttrAuditReq = _CtxAar,
			     commandRequests = [CR]} = AR,
	    #'CommandRequest'{command = Cmd,
			      optional = _Opt,
			      wildcardReturn = _WR} = CR,
	    {serviceChangeReq, SCR} = Cmd,
	    #'ServiceChangeRequest'{terminationID = _TermID,
				    serviceChangeParms = SCP} = SCR,
	    #'ServiceChangeParm'{serviceChangeMethod = restart,
				 serviceChangeReason = [[$9,$0,$1|_]]} = SCP,
	    {ok, M};
       (M) ->
	    {error, {invalid_message, M}}
    end.

otp_5881_verify_notify_request_fun() ->
    fun(#'MegacoMessage'{mess = Mess} = M) -> 
	    #'Message'{messageBody = Body} = Mess,
	    {transactions, [Trans]} = Body,
	    {transactionRequest, TR} = Trans,
	    #'TransactionRequest'{actions = [AR]} = TR,
	    #'ActionRequest'{commandRequests = [CR1,CR2]} = AR,
	    #'CommandRequest'{command = Cmd1} = CR1,
	    {notifyReq, NR1} = Cmd1,
	    #'NotifyRequest'{observedEventsDescriptor = OED1} = NR1,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE1]} = OED1,
	    #'ObservedEvent'{eventName = "al/of"} = OE1,
	    #'CommandRequest'{command = Cmd2} = CR2,
	    {notifyReq, NR2} = Cmd2,
	    #'NotifyRequest'{observedEventsDescriptor = OED2} = NR2,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE2]} = OED2,
	    #'ObservedEvent'{eventName = "al/of"} = OE2,
	    {ok, M};
       (M) ->
	    {error, {invalid_message, M}}
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_5887(suite) ->
    [];
otp_5887(Config) when is_list(Config) ->
    put(verbosity, ?TEST_VERBOSITY),
    put(sname,     "TEST"),
    put(tc,        otp_5887),
    i("starting"),

    MgcNode = make_node_name(mgc),
    MgNode  = make_node_name(mg),
    d("start nodes: "
      "~n   MgcNode: ~p"
      "~n   MgNode:  ~p", 
      [MgcNode, MgNode]),
    ok = megaco_test_lib:start_nodes([MgcNode, MgNode], ?FILE, ?LINE),

    d("start the MGC simulator (generator)"),
    {ok, Mgc} = megaco_test_generator:start_link("MGC", MgcNode),

    d("create the MGC event sequence"),
    MgcEvSeq = otp_5887_mgc_event_sequence(text, tcp),

    d("start the MGC simulation"),
    megaco_test_generator:tcp(Mgc, MgcEvSeq),

    i("[MG] start"),    
    MgMid = {deviceName, "mg"},
    ReqTmr = #megaco_incr_timer{wait_for    = 3000,
                                factor      = 1, 
                                incr        = 0,
                                max_retries = 3},
    %% 5000,  %% Does not matter since we will not use this anyway...
    LongReqTmr = #megaco_incr_timer{wait_for    = 3000,
                                    factor      = 1, 
                                    incr        = 0,
                                    max_retries = 3},
    PendingTmr = 10000,
    ReplyTmr = 16000,
    MgConfig = [{request_timer,      ReqTmr}, 
                {long_request_timer, LongReqTmr}, 
                {pending_timer,      PendingTmr},
                {reply_timer,        ReplyTmr}],
    {ok, Mg} = ?MG_START(MgNode, MgMid, text, tcp, MgConfig, ?MG_VERBOSITY),

    i("[MG] conn info: ~n~p", [?MG_CONN_INFO(Mg, all)]), 

    i("[MG] verify conn transaction-id: 1"),    
    otp_5887_verify_conn_trans_id(Mg, 1),

    i("[MG] user info: ~n~p", [?MG_USER_INFO(Mg, all)]), 

    i("[MG] verify user transaction-id: undefined_serial"),    
    otp_5887_verify_user_trans_id(Mg, undefined_serial),
    
    i("[MG] connect to the MGC (service change)"),    
    ServChRes = ?MG_SERV_CHANGE(Mg),
    d("service change result: ~p", [ServChRes]),

    i("[MG] conn info: ~n~p", [?MG_CONN_INFO(Mg, all)]), 

    i("[MG] verify conn transaction-id: 2"),    
    otp_5887_verify_conn_trans_id(Mg, 2),

    i("[MG] user info: ~n~p", [?MG_USER_INFO(Mg, all)]), 

    i("[MG] verify user transaction-id: 1"),    
    otp_5887_verify_user_trans_id(Mg, 1),
    
    d("[MG] send the notify"),
    {ok, Reply} = (catch ?MG_NOTIF_RAR(Mg)),
    {1, {ok, [AR]}} = Reply,
    d("[MG] ActionReply: ~p", [AR]),

    i("[MG] conn info: ~n~p", [?MG_CONN_INFO(Mg, all)]), 

    i("[MG] verify conn transaction-id: 3"),    
    otp_5887_verify_conn_trans_id(Mg, 3),

    i("[MG] user info: ~n~p", [?MG_USER_INFO(Mg, all)]), 

    i("[MG] verify user transaction-id: 2"),    
    otp_5887_verify_user_trans_id(Mg, 2),
    
    d("[MGC] await the tcp reply"),
    {ok, TcpReply} = megaco_test_generator:tcp_await_reply(Mgc),
    d("[MGC] TcpReply: ~p", [TcpReply]),

    %% Tell MG to stop
    i("[MG] stop"),
    ?MG_STOP(Mg),

    %% Tell Mgc to stop
    i("[MGC] stop generator"),
    megaco_test_generator:stop(Mgc),

    i("done", []),
    ok.


otp_5887_verify_conn_trans_id(Mg, Expected) ->
    F = fun() -> (catch ?MG_CONN_INFO(Mg, trans_id)) end,
    otp_5887_verify_trans_id(F, Expected).

otp_5887_verify_user_trans_id(Mg, Expected) ->
    F = fun() -> (catch ?MG_USER_INFO(Mg, trans_id)) end,
    otp_5887_verify_trans_id(F, Expected).

otp_5887_verify_trans_id(F, Expected) ->
    case F() of
	Expected -> 
	    ok;
	ErroneousValue ->
	    throw({unexpected_transaction_id_value, ErroneousValue, Expected})
    end.
    

otp_5887_mgc_event_sequence(text, tcp) ->
    DecodeFun = otp_5887_decode_msg_fun(megaco_pretty_text_encoder, []),
    EncodeFun = otp_5887_encode_msg_fun(megaco_pretty_text_encoder, []),
    ServiceChangeVerifyFun = otp_5887_verify_service_change_req_msg_fun(),
    Mid = {deviceName,"ctrl"},
    ServiceChangeReply = otp_5887_service_change_reply_msg(Mid, 1, 0),
    NotifyReqVerifyFun = otp_5887_verify_notify_request_fun(),
    TermId = #megaco_term_id{id = ["00000000","00000000","01101101"]},
    NotifyReply = otp_5887_notify_reply_msg(Mid, 2, 0, TermId),
    MgcEvSeq = [{debug,  true},
		{decode, DecodeFun},
		{encode, EncodeFun},
		{listen, 2944},
		{expect_accept, any},
		{expect_receive, "service-change-request", {ServiceChangeVerifyFun, 10000}}, 
		{send, "service-change-reply", ServiceChangeReply}, 
		{expect_receive, "notify-request", {NotifyReqVerifyFun, 10000}},
  		{send, "notify-reply", NotifyReply},
		{sleep, 2000}
	       ],
    MgcEvSeq.

otp_5887_service_change_reply_msg(Mid, TransId, Cid) ->
    SCRP  = #'ServiceChangeResParm'{serviceChangeMgcId = Mid},
    SCRPs = {serviceChangeResParms,SCRP},
    Root  = #megaco_term_id{id = ["root"]},
    SCR   = #'ServiceChangeReply'{terminationID       = [Root],
				  serviceChangeResult = SCRPs},
    CR    = {serviceChangeReply, SCR},
    otp_5887_msg(Mid, TransId, CR, Cid).

otp_5887_notify_reply_msg(Mid, TransId, Cid, TermId) ->
    NR  = #'NotifyReply'{terminationID = [TermId]},
    CR  = {notifyReply, NR},
    otp_5887_msg(Mid, TransId, CR, Cid).

otp_5887_msg(Mid, TransId, CR, Cid) ->
    AR  = #'ActionReply'{contextId    = Cid,
			 commandReply = [CR]},
    ARs  = {actionReplies, [AR]},
    TR   = #'TransactionReply'{transactionId     = TransId,
			       transactionResult = ARs},
    Body = {transactions, [{transactionReply, TR}]},
    Mess = #'Message'{version     = 1,
		      mId         = Mid,
		      messageBody = Body},
    #'MegacoMessage'{mess = Mess}.


otp_5887_pending_msg(Mid, TransId) ->
    TP   = #'TransactionPending'{transactionId = TransId},
    Body = {transactions, [{transactionPending, TP}]},
    Mess = #'Message'{version     = 1,
		      mId         = Mid,
		      messageBody = Body},
    #'MegacoMessage'{mess = Mess}.


otp_5887_encode_msg_fun(Mod, Conf) ->
    fun(M) -> 
	    Mod:encode_message(Conf, M) 
    end.
otp_5887_encode_msg_fun(Mod, Conf, Ver) ->
    fun(M) -> 
	    Mod:encode_message(Conf, Ver, M) 
    end.

otp_5887_decode_msg_fun(Mod, Conf) ->
    fun(M) -> 
	    Mod:decode_message(Conf, M) 
    end.
otp_5887_decode_msg_fun(Mod, Conf, Ver) ->
    fun(M) -> 
	    Mod:decode_message(Conf, Ver, M) 
    end.

otp_5887_verify_msg_fun() ->
    fun(M) -> {ok, M} end.

otp_5887_verify_service_change_req_msg_fun() ->
    fun(#'MegacoMessage'{mess = Mess} = M) -> 
	    #'Message'{version     = _V,
		       mId         = _Mid,
		       messageBody = Body} = Mess,
	    {transactions, [Trans]} = Body,
	    {transactionRequest, TR} = Trans,
	    #'TransactionRequest'{transactionId = _Tid,
				  actions = [AR]} = TR,
	    #'ActionRequest'{contextId = _Cid,
			     contextRequest = _CtxReq,
			     contextAttrAuditReq = _CtxAar,
			     commandRequests = [CR]} = AR,
	    #'CommandRequest'{command = Cmd,
			      optional = _Opt,
			      wildcardReturn = _WR} = CR,
	    {serviceChangeReq, SCR} = Cmd,
	    #'ServiceChangeRequest'{terminationID = _TermID,
				    serviceChangeParms = SCP} = SCR,
	    #'ServiceChangeParm'{serviceChangeMethod = restart,
				 serviceChangeReason = [[$9,$0,$1|_]]} = SCP,
	    {ok, M};
       (M) ->
	    {error, {invalid_message, M}}
    end.

otp_5887_verify_notify_request_fun() ->
    fun(#'MegacoMessage'{mess = Mess} = M) -> 
	    #'Message'{messageBody = Body} = Mess,
	    {transactions, [Trans]} = Body,
	    {transactionRequest, TR} = Trans,
	    #'TransactionRequest'{actions = [AR]} = TR,
	    #'ActionRequest'{commandRequests = [CR1,CR2]} = AR,
	    #'CommandRequest'{command = Cmd1} = CR1,
	    {notifyReq, NR1} = Cmd1,
	    #'NotifyRequest'{observedEventsDescriptor = OED1} = NR1,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE1]} = OED1,
	    #'ObservedEvent'{eventName = "al/of"} = OE1,
	    #'CommandRequest'{command = Cmd2} = CR2,
	    {notifyReq, NR2} = Cmd2,
	    #'NotifyRequest'{observedEventsDescriptor = OED2} = NR2,
	    #'ObservedEventsDescriptor'{observedEventLst = [OE2]} = OED2,
	    #'ObservedEvent'{eventName = "al/of"} = OE2,
	    {ok, M};
       (M) ->
	    {error, {invalid_message, M}}
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_6253(suite) ->
    [];
otp_6253(Config) when is_list(Config) ->
    ?ACQUIRE_NODES(1, Config),

    put(verbosity, debug),
    put(tc, otp_6253),

    d("otp_6253 -> start test case controller",[]),
    ok = megaco_tc_controller:start_link(),

    PrelMid = preliminary_mid,
    MgMid   = ipv4_mid(4711),

    ?VERIFY(ok, application:start(megaco)),
    ?VERIFY(ok,	megaco:start_user(MgMid, [{send_mod, megaco_mess_user_test},
	                                  {request_timer, infinity},
	                                  {reply_timer, infinity}])),

    MgRH = user_info(MgMid, receive_handle),
    {ok, PrelCH} = ?VERIFY({ok, _}, megaco:connect(MgRH, PrelMid, sh, self())),

    connections([PrelCH]),
    ?VERIFY([PrelCH], megaco:user_info(MgMid, connections)),
    
    SC = service_change_request(),

    %% Instruct the transport module to fail all send_message
    d("otp_6253 -> instruct transport module to fail message send",[]),
    ok = megaco_tc_controller:insert(allow_send_message, {fail, otp_6253}),

    ?VERIFY({1, {error, {send_message_failed, otp_6253}}},
	    megaco:call(PrelCH, [SC], [])),

    sleep(1000),

    %% Instruct the transport module to cancel all send_message
    d("otp_6253 -> instruct transport module to cancel message send",[]),
    ok = megaco_tc_controller:insert(allow_send_message, {cancel, otp_6253}),

    ?VERIFY({1, {error, {send_message_cancelled, otp_6253}}},
	    megaco:call(PrelCH, [SC], [])),

    ?VERIFY(ok, megaco:disconnect(PrelCH, shutdown)),

    ?VERIFY(ok,	megaco:stop_user(MgMid)),
    ?VERIFY(ok, application:stop(megaco)),
    ?RECEIVE([]),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cre_ErrDesc(T) ->
    EC = ?megaco_internal_gateway_error,
    ET = lists:flatten(io_lib:format("~w",[T])),
    #'ErrorDescriptor'{errorCode = EC, errorText = ET}.
	    

cre_serviceChangeParm(M,R,P) ->
    #'ServiceChangeParm'{serviceChangeMethod  = M,
                         serviceChangeReason  = R,
                         serviceChangeProfile = P}.

cre_serviceChangeParm(M, V, R, P) ->
    #'ServiceChangeParm'{serviceChangeMethod  = M, 
			 serviceChangeVersion = V,
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

cre_obsEvsDesc(Id, EvList) ->
    #'ObservedEventsDescriptor'{requestId        = Id, 
                                observedEventLst = EvList}.

cre_notifyReq(Tid, EvsDesc) ->
    #'NotifyRequest'{terminationID            = Tid, 
                     observedEventsDescriptor = EvsDesc}.

cre_command(R) when is_record(R, 'NotifyRequest') ->
    {notifyReq, R};
cre_command(R) when is_record(R, 'ServiceChangeRequest') ->
    {serviceChangeReq, R}.

cre_cmdReq(Cmd) ->
    #'CommandRequest'{command = Cmd}.

cre_actionReq(CtxId, CmdReqs) when is_list(CmdReqs) ->
    #'ActionRequest'{contextId       = CtxId,
                     commandRequests = CmdReqs}.

cre_transReq(TransId, ARs) when is_list(ARs) ->
    #'TransactionRequest'{transactionId = TransId,
			  actions       = ARs}.

cre_transResult(ED) when record(ED, 'ErrorDescriptor') ->
    {transactionError, ED};
cre_transResult([AR|_] = ARs) when record(AR, 'ActionReply') ->
    {actionReplies, ARs}.

cre_transReply(TransId, Res) ->
    #'TransactionReply'{transactionId     = TransId,
			transactionResult = Res}.

cre_transReply(TransId, IAR, Res) ->
    #'TransactionReply'{transactionId     = TransId,
			immAckRequired    = IAR, 
			transactionResult = Res}.


%% --

cre_serviceChangeResParm(Mid) ->
    cre_serviceChangeResParm(Mid, ?VERSION).

cre_serviceChangeResParm(Mid, V) ->
    #'ServiceChangeResParm'{serviceChangeMgcId   = Mid, 
			    serviceChangeVersion = V}.

cre_serviceChangeResult(SCRP) when is_record(SCRP, 'ServiceChangeResParm') ->
    {serviceChangeResParms, SCRP};
cre_serviceChangeResult(ED) when is_record(ED, 'ErrorDescriptor') ->
    {errorDescriptor, ED}.

cre_serviceChangeReply(Tid, Res) ->
    #'ServiceChangeReply'{terminationID       = Tid, 
                          serviceChangeResult = Res}.

cre_cmdReply(R) when is_record(R, 'NotifyReply') ->
    {notifyReply, R};
cre_cmdReply(R) when is_record(R, 'ServiceChangeReply') ->
    {serviceChangeReply, R}.

cre_transRespAck(TransAck) when is_record(TransAck, 'TransactionAck') ->
    [TransAck];
cre_transRespAck(TRA) when is_list(TRA) ->
    TRA.

cre_transAck(TransId) ->
    #'TransactionAck'{firstAck = TransId}.

cre_notifyReply(Tid) ->
    #'NotifyReply'{terminationID = Tid}.

cre_actionReply(CtxId, CmdRep) ->
    #'ActionReply'{contextId    = CtxId,
                   commandReply = CmdRep}.

cre_serviceChangeProf(Name, Ver) when is_list(Name), integer(Ver) ->
    #'ServiceChangeProfile'{profileName = Name, 
                            version     = Ver}.

cre_transaction(Trans) when is_record(Trans, 'TransactionRequest') ->
    {transactionRequest, Trans};
cre_transaction(Trans) when is_record(Trans, 'TransactionPending') ->
    {transactionPending, Trans};
cre_transaction(Trans) when is_record(Trans, 'TransactionReply') ->
    {transactionReply, Trans};
cre_transaction(Trans) when is_list(Trans) ->
    {transactionResponseAck, Trans}.

cre_transactions(Trans) when is_list(Trans) ->
    {transactions, Trans}.

cre_message(Version, Mid, Body) ->
    #'Message'{version     = Version,
               mId         = Mid,
               messageBody = Body}.

cre_megacoMessage(Mess) ->
    #'MegacoMessage'{mess = Mess}.

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

handle_unexpected_trans(ReceiveHandle, ProtocolVersion, Trans, Pid) ->
    Pid ! {handle_unexpected_trans, 
	   {ReceiveHandle, ProtocolVersion, Trans, Pid}},
    ok.

handle_trans_request_abort(ReceiveHandle, ProtocolVersion, TransNo, HandlerPid, Pid) ->
    Pid ! {handle_trans_request_abort, 
	   {ReceiveHandle, ProtocolVersion, TransNo, HandlerPid}},
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

user_info(Mid, Key) ->
    case (catch megaco:user_info(Mid, Key)) of
	{'EXIT', _} = Error ->
	    ?ERROR(Error);
	Val ->
	    ?LOG("user_info -> ok: "
		 "~n   ~p"
		 "~n", [Val]),
	    Val
    end.


stop_user(Mid) ->
    case (catch megaco:stop_user(Mid)) of
	{'EXIT', _} = Error ->
	    ?ERROR(Error);
	Val ->
	    ?LOG("stop_user -> ok:"
		 "~n   ~p"
		 "~n", [Val]),
	    Val
    end.

connections() ->
    system_info(connections).

connections(Conns0) ->
    Conns1 = lists:sort(Conns0),
    case lists:sort(connections()) of
	Conns1 ->
	    ?LOG("connections -> ok:"
		 "~n   ~p"
		 "~n", [Conns1]),
	    Conns1;
	Conns2 ->
	    ?ERROR({Conns1, Conns2}),
	    Conns2
    end.

system_info(Key) ->
    megaco:system_info(Key).


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

format_timestamp({_N1, _N2, N3} = Now) ->
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

