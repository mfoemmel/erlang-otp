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

-compile(export_all).
-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").
-include("megaco_test_lib.hrl").

-define(SEND(Expr), ?VERIFY(ok, megaco_mess_user_test:apply_proxy(fun() -> Expr end))).

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
% init_per_testcase(request_and_reply = Case, Config) ->
%     put(dbg,true),
%     megaco_test_lib:init_per_testcase(Case, Config);
init_per_testcase(Case, Config) ->
    megaco_test_lib:init_per_testcase(Case, Config).

% fin_per_testcase(request_and_reply = Case, Config) ->
%     erase(dbg),
%     megaco_test_lib:fin_per_testcase(Case, Config)M
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

tickets(suite) ->
    [
     otp_4359
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connect(suite) ->
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
    d("request_and_reply -> MG try connect to MGC",[]),
    ?SEND(megaco:connect(MgRH, PrelMid, MgSH, MgPid)), % Mg prel
    d("request_and_reply -> MGC await connect from MG",[]),
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

pending_ack(suite) ->
    [];
pending_ack(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    megaco_mess_user_test:start_proxy(),

    PrelMid = preliminary_mid,
    MgMid   = ipv4_mid(4711),
    MgcMid  = ipv4_mid(),
    UserMod = megaco_mess_user_test,
    ?VERIFY(ok, application:start(megaco)),
    UserData = user_data,
    UserConfig = [{user_mod, UserMod},
		  {send_mod, UserMod},
		  {request_timer, infinity},
		  {long_request_timer, infinity},
		  {reply_timer, infinity}],
    ?VERIFY(ok,	megaco:start_user(MgMid, UserConfig)),
    ?VERIFY(ok,	megaco:start_user(MgcMid, UserConfig)),

    MgRH = ?VERIFY(_, megaco:user_info(MgMid, receive_handle)),
    MgcRH = ?VERIFY(_, megaco:user_info(MgcMid, receive_handle)), 
    {ok, MgPid, MgSH} =
	?VERIFY({ok, _, _}, UserMod:start_transport(MgRH, MgcRH)),
    PrelMgCH = #megaco_conn_handle{local_mid = MgMid,
				   remote_mid = preliminary_mid},
    MgCH  = #megaco_conn_handle{local_mid = MgMid,
				remote_mid = MgcMid},
    MgcCH = #megaco_conn_handle{local_mid = MgcMid,
				remote_mid = MgMid},
    ?SEND(megaco:connect(MgRH, PrelMid, MgSH, MgPid)), % Mg prel
    ?USER({connect, PrelMgCH, V, []}, ok),
    ?RECEIVE([{res, _, {ok, PrelMgCH}}]),

    Req = service_change_request(),
    ?VERIFY(ok, megaco:cast(PrelMgCH, [Req], [{reply_data, UserData}])),

    ?USER({connect, MgcCH, V, []}, ok), % Mgc auto

    RequestData = Req,
    ?USER({request, MgcCH, V, [[Req]]}, {pending, RequestData}),
    Rep = service_change_reply(MgcMid),
    AckData = ack_data,
    %% BUGBUG: How do we verify that the MG rally gets a pending trans?
    ?USER({long_request, MgcCH, V, [RequestData]}, {{handle_ack, AckData}, [Rep]}),
    ?USER({connect, MgCH, V, []}, ok), % Mg confirm
    ?USER({reply, MgCH, V, [{ok, [Rep]}, UserData]}, ok),
    ?USER({ack, MgcCH, V, [ok, AckData]}, ok),

    ?VERIFYL([MgCH, MgcCH], megaco:system_info(connections)),
    ?VERIFY([MgCH], megaco:user_info(MgMid, connections)),
    ?VERIFY([MgcCH], megaco:user_info(MgcMid, connections)),

    Reason = shutdown,
    ?SEND(application:stop(megaco)),
    ?RECEIVE([{res, _, ok}]),
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dist(suite) ->
    [];
dist(Config) when list(Config) ->
    [Local, Dist] = ?ACQUIRE_NODES(2, Config),
    megaco_mess_user_test:start_proxy(),

    PrelMid = preliminary_mid,
    MgMid   = ipv4_mid(4711),
    MgcMid  = ipv4_mid(),
    UserMod = megaco_mess_user_test,
    ?VERIFY(ok, application:start(megaco)),
    UserConfig = [{user_mod, UserMod}, {send_mod, UserMod},
		  {request_timer, infinity}, {reply_timer, infinity}],
    ?VERIFY(ok,	megaco:start_user(MgMid, UserConfig)),
    ?VERIFY(ok,	megaco:start_user(MgcMid, UserConfig)),

    MgRH = ?VERIFY(_, megaco:user_info(MgMid, receive_handle)),
    MgcRH = ?VERIFY(_, megaco:user_info(MgcMid, receive_handle)), 
    {ok, MgPid, MgSH} =
	?VERIFY({ok, _, _}, UserMod:start_transport(MgRH, MgcRH)),
    PrelMgCH = #megaco_conn_handle{local_mid = MgMid,
				   remote_mid = preliminary_mid},
    MgCH  = #megaco_conn_handle{local_mid = MgMid,
				remote_mid = MgcMid},
    MgcCH = #megaco_conn_handle{local_mid = MgcMid,
				remote_mid = MgMid},
    ?SEND(megaco:connect(MgRH, PrelMid, MgSH, MgPid)), % Mg prel
    ?USER({connect, PrelMgCH, V, []}, ok),
    ?RECEIVE([{res, _, {ok, PrelMgCH}}]),

    Req = service_change_request(),
    ?SEND(megaco:call(PrelMgCH, [Req], [])),

    ?USER({connect, MgcCH, V, []}, ok), % Mgc auto
    Rep = service_change_reply(MgcMid),
    ?USER({request, MgcCH, V, [[Req]]}, {discard_ack, [Rep]}),
    ?USER({connect, MgCH, V, []}, ok), % Mg confirm
    ?RECEIVE([{res, _, {1, {ok, [Rep]}}}]),

    %% Dist
    ?VERIFY(ok,	rpc:call(Dist, megaco, start, [])),
    ?VERIFY(ok,	rpc:call(Dist, megaco, start_user, [MgcMid, UserConfig])),
    MgcPid = self(),
    MgcSH = {element(2, MgSH), element(1, MgSH)},
    ?SEND(rpc:call(Dist, megaco, connect, [MgcRH, MgMid, MgcSH, MgcPid])), % Mgc dist
    ?USER({connect, MgcCH, V, []}, ok), % Mgc dist auto
    ?RECEIVE([{res, _, {ok, MgcCH}}]),

    ?SEND(rpc:call(Dist, megaco, call, [MgcCH, [Req], []])),
    ?USER({request, MgCH, V, [[Req]]}, {discard_ack, [Rep]}),
    ?RECEIVE([{res, _, {1, {ok, [Rep]}}}]),

    ?VERIFYL([MgCH, MgcCH], megaco:system_info(connections)),
    ?VERIFY([MgCH], megaco:user_info(MgMid, connections)),
    ?VERIFY([MgcCH], megaco:user_info(MgcMid, connections)),

    ?VERIFY([MgcCH], rpc:call(Dist, megaco, system_info, [connections])),
    ?VERIFY([], rpc:call(Dist, megaco, user_info, [MgMid, connections])),
    ?VERIFY([MgcCH], rpc:call(Dist, megaco, user_info, [MgcMid, connections])),

    %% Shutdown

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

block(Pid) ->
    Pid ! {block, dummy},
    ok.

unblock(Pid) ->
    Pid ! {unblock, dummy},
    ok.

close(Pid) ->
    Pid ! {close, dummy},
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



d(F,A) ->
    d(get(dbg),F,A).

d(true,F,A) ->
    io:format("DBG: " ++ F ++ "~n",A);
d(_, _F, _A) ->
    ok.
