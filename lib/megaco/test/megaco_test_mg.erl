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
%% Purpose: Implements an "MG" used by the test suite
%%----------------------------------------------------------------------
-module(megaco_test_mg).

-export([start/5, start/6, stop/1, 
	 get_stats/2, reset_stats/1,
	 service_change/1, 
	 user_info/1, user_info/2, conn_info/1, conn_info/2, 
	 update_user_info/3, update_conn_info/3, 
	 notify_request/1, await_notify_reply/1, notify_request_and_reply/1,
	 cancel_request/2, 
	 group_requests/2, 
	 ack_info/2, 
	 apply_load/2, 
	 verbosity/2]).

-export([mg/3, notify_request_handler_main/4]).

%% Megaco callback api
-export([
	 handle_connect/3,
	 handle_disconnect/4,
	 handle_syntax_error/4,
	 handle_message_error/4,
	 handle_trans_request/4,
	 handle_trans_long_request/4,
	 handle_trans_reply/5,
	 handle_trans_ack/5
	]).

-include("megaco_test_lib.hrl").
-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").

-define(A4444, tid(255*256*256)                 ).
-define(A4445, tid(255*256*256 + 255)           ).
-define(A5555, tid(255*256*256 + 255*256)       ).
-define(A5556, tid(255*256*256 + 255*256 + 255) ).

-record(mg, {parent       = undefined,
	     mid          = undefined,
	     conn_handle  = undefined,
	     state        = initiated,
	     req_handler  = undefined,
	     call_mode    = async,
	     group_size   = 1,
	     ack_info     = undefined,
	     load_counter  = 0,
	     reply_counter = 0}).


%%% --------------------------------------------------------------------

start(Node, Mid, Encoding, Transport, Verbosity) ->
    start(Node, Mid, Encoding, Transport, [], Verbosity).

start(Node, Mid, Encoding, Transport, Conf, Verbosity) ->
    d("start mg[~p]: ~p", [Node, Mid]),
    RI1 = encoding_config(Encoding),
    RI2 = transport_config(Transport),
    RI = {receive_info, RI1 ++ RI2},
    Config = [{local_mid, Mid}, RI] ++ Conf,
    Pid = spawn_link(Node, ?MODULE, mg, [self(), Verbosity, Config]),
    await_started(Pid).

encoding_config(text) ->
    [{encoding_module, megaco_pretty_text_encoder},
     {encoding_config, []},
     {port,2944}];
encoding_config(binary) ->
    [{encoding_module, megaco_ber_bin_encoder},
     {encoding_config, []},
     {port,2945}].

transport_config(tcp) ->
    [{transport_module, megaco_tcp}];
transport_config(udp) ->
    [{transport_module, megaco_udp}].

await_started(Pid) ->
    receive
	{started, Pid} ->
	    d("await_started ~p: ok", [Pid]),
	    {ok, Pid};
	{'EXIT', Pid, Reason} ->
	    i("await_started ~p: received exit signal: ~p", [Pid, Reason]),
	    exit({failed_starting, Pid, Reason})
    after 10000 ->
	    i("await_started ~p: timeout", [Pid]),
	    exit({error, timeout})
    end.


stop(Pid) ->
    server_request(Pid, stop, stopped).


get_stats(Pid, No) ->
    server_request(Pid, {statistics, No}, {statistics_reply, No}).


reset_stats(Pid) ->
    server_request(Pid, reset_stats, reset_stats_ack).


user_info(Pid) ->
    server_request(Pid, {user_info, all}, user_info_ack).

user_info(Pid, Tag) ->
    server_request(Pid, {user_info, Tag}, user_info_ack).


conn_info(Pid) ->
    server_request(Pid, {conn_info, all}, conn_info_ack).

conn_info(Pid, Tag) ->
    server_request(Pid, {conn_info, Tag}, conn_info_ack).


update_user_info(Pid, Tag, Val) ->
    server_request(Pid, {update_user_info, Tag, Val}, update_user_info_ack).


update_conn_info(Pid, Tag, Val) ->
    server_request(Pid, {update_conn_info, Tag, Val}, update_conn_info_ack).


service_change(Pid) ->
    server_request(Pid, service_change, service_change_reply).


ack_info(Pid, InfoPid) ->
    Pid ! {ack_info, InfoPid, self()}.

verbosity(Pid, V) ->
    Pid ! {verbosity, V, self()}.


group_requests(Pid, N) ->
    server_request(Pid, {group_requests, N}, group_requests_reply).


notify_request(Pid) ->
    Pid ! {notify_request, self()}.

await_notify_reply(Pid) ->
    await_reply(Pid, notify_request_reply).

notify_request_and_reply(Pid) ->
    notify_request(Pid),
    await_notify_reply(Pid).


cancel_request(Pid, Reason) ->
    server_request(Pid, cancel_request, Reason, cancel_request_reply).


apply_load(Pid, CounterStart) ->
    server_request(Pid, apply_load, CounterStart, apply_load_ack).
    

server_request(Pid, Req, ReplyTag) ->
    Pid ! {Req, self()},
    await_reply(Pid, ReplyTag).

server_request(Pid, Req, ReqData, ReplyTag) ->
    Pid ! {Req, ReqData, self()},
    await_reply(Pid, ReplyTag).

await_reply(Pid, ReplyTag) ->
    await_reply(Pid, ReplyTag, infinity).

await_reply(Pid, ReplyTag, Timeout) ->
    receive
	{ReplyTag, Reply, Pid} ->
	    Reply;
	{'EXIT', Pid, Reason} ->
	    exit({failed, ReplyTag, Pid, Reason})
    after Timeout ->
	    exit({timeout, ReplyTag, Pid})
    end.


server_reply(Pid, ReplyTag, Reply) ->
    Pid ! {ReplyTag, Reply, self()}.


%%% --------------------------------------------------------------------


mg(Parent, Verbosity, Config) ->
    process_flag(trap_exit, true),
    put(verbosity, Verbosity),
    put(sname,   "MG"),
    i("mg -> starting"),
    {Mid, ConnHandle} = init(Config),
    notify_started(Parent),
    S = #mg{parent = Parent, mid = Mid, conn_handle = ConnHandle},
    i("mg -> started"),
    loop(S).

init(Config) ->
    d("init -> entry with"
      "~n   Config: ~p", [Config]),
    random_init(),
    Mid = get_conf(local_mid, Config),
    RI  = get_conf(receive_info, Config),
    d("init -> start megaco"),
    application:start(megaco),

    d("init -> start megaco user"),
    Conf0 = lists:keydelete(local_mid,    1, Config),
    Conf1 = lists:keydelete(receive_info, 1, Conf0),
    ok = megaco:start_user(Mid, Conf1),
    d("init -> update user info (user_mod)"),
    ok = megaco:update_user_info(Mid, user_mod,  ?MODULE),
    d("init -> update user info (user_args)"),
    ok = megaco:update_user_info(Mid, user_args, [self()]),

%     d("init -> start megaco user"),
%     megaco:start_user(Mid, []),
%     d("init -> update user info (user_mod)"),
%     megaco:update_user_info(Mid, user_mod,  ?MODULE),
%     d("init -> update user info (user_args)"),
%     megaco:update_user_info(Mid, user_args, [self()]),

    d("init -> get user info (receive_handle)"),
    RH = megaco:user_info(Mid,receive_handle),
    d("init -> parse receive info"),
    {MgcPort,RH1} = parse_receive_info(RI, RH),
    d("init -> start transport with"),
    ConnHandle = start_transport(MgcPort, RH1),
    {Mid, ConnHandle}.


loop(#mg{state = State} = S) ->
    d("loop(~p) -> await request", [State]),
    receive
	{stop, Parent} when S#mg.parent == Parent ->
	    i("loop(~p) -> stopping", [State]),
	    close_conn(S#mg.conn_handle),
	    megaco:stop_user(S#mg.mid),
	    application:stop(megaco),
	    i("loop(~p) -> stopped", [State]),
	    server_reply(Parent, stopped, ok),
	    exit(normal);


	{reset_stats, Parent} when S#mg.parent == Parent ->
	    i("loop(~p) -> got request to reset stats counters", [State]),
	    %% reset_stats(S#mgc.conn_handle),
	    do_reset_stats(S#mg.conn_handle),
	    server_reply(Parent, reset_stats_ack, ok),
	    loop(S);

	{{update_user_info, Tag, Val}, Parent} when S#mg.parent == Parent ->
	    i("loop(~p) -> got update_user_info: ~w -> ~p", [State, Tag, Val]),
	    Res = (catch megaco:update_user_info(S#mg.mid, Tag, Val)),
	    d("loop(~p) -> Res: ~p", [Parent, Res]),
	    server_reply(Parent, update_user_info_ack, Res),
	    loop(S);

	{{user_info, Tag}, Parent} when S#mg.parent == Parent ->
	    i("loop(~p) -> got user_info request for ~w", [State, Tag]),
	    Res = (catch megaco:user_info(S#mg.mid, Tag)),
	    d("loop(~p) -> Res: ~p", [Parent, Res]),
	    server_reply(Parent, user_info_ack, Res),
	    loop(S);

	{{update_conn_info, Tag, Val}, Parent} when S#mg.parent == Parent ->
	    i("loop(~p) -> got update_conn_info: ~w -> ~p", [State, Tag, Val]),
	    %% [CH] = megaco:user_info(S#mg.mid, connections), % We only got one
	    case megaco:user_info(S#mg.mid, connections) of
		[CH] ->
		    Res = (catch megaco:update_conn_info(CH, Tag, Val)),
		    d("loop(~p) -> Res: ~p", [Parent, Res]),
		    server_reply(Parent, update_conn_info_ack, Res),
		    loop(S);
		Else ->
		    exit({error, Else, (catch megaco:user_info(S#mg.mid, all))})
	    end;

	{{conn_info, Tag}, Parent} when S#mg.parent == Parent ->
	    i("loop(~p) -> got conn_info request for ~w", [State, Tag]),
	    [CH] = megaco:user_info(S#mg.mid, connections), % We only got one
	    Res = (catch megaco:conn_info(CH, Tag)),
	    d("loop(~p) -> Res: ~p", [Parent, Res]),
	    server_reply(Parent, conn_info_ack, Res),
	    loop(S);

	%% Give me statistics
	{{statistics, 1}, Parent} when S#mg.parent == Parent ->
	    i("loop(~p) -> got request for statistics 1", [State]),
	    {ok, Gen} = megaco:get_stats(),
	    CH = S#mg.conn_handle,
	    Reason = {statistics, CH}, 
	    Pid = megaco:conn_info(CH, control_pid),
	    SendMod    = megaco:conn_info(CH, send_mod),
	    SendHandle = megaco:conn_info(CH, send_handle),
	    {ok, Trans} = 
		case SendMod of
		    megaco_tcp -> megaco_tcp:get_stats(SendHandle);
		    megaco_udp -> megaco_udp:get_stats(SendHandle);
		    SendMod    -> exit(Pid, Reason)
		end,
	    Reply = {ok, [{gen, Gen}, {trans, Trans}]},
	    server_reply(Parent, {statistics_reply, 1}, Reply),
	    loop(S); 


	%% Do a service change
	%% No server-reply here. Since the service change is 
	%% async, the reply (from the MGC) will come later.
	{service_change, Parent} when S#mg.parent == Parent, 
				      State == initiated ->
	    i("loop(~p) -> received request to perform service change", 
	      [State]),
	    Res = do_service_change(S#mg.conn_handle),
	    d("loop(~p) -> service change result: ~p", [State, Res]),
	    loop(S#mg{state = connecting}); 


	{{group_requests, N}, Parent} when S#mg.parent == Parent, N > 0 ->
	    i("loop(~p) -> received group_requests ~p", [State, N]),
	    OldN = S#mg.group_size,
	    server_reply(Parent, group_requests_reply, {ok, OldN}),
	    loop(S#mg{group_size = N}); 


	{ack_info, To, Parent} when S#mg.parent == Parent ->
	    i("loop(~p) -> received request to inform about received ack's ", 
	      [State]),
	    loop(S#mg{ack_info = To});


	{verbosity, V, Parent} when S#mg.parent == Parent ->
	    i("loop -> received new verbosity: ~p", [V]),
	    put(verbosity,V),
	    loop(S);


	%% Make a sync-call
	{notify_request, Parent} when S#mg.parent == Parent, 
				      State == connected ->
	    i("loop(~p) -> received request to send notify request ", 
	      [State]),
	    Pid = start_notify_request_handler(S),
	    d("loop(~p) -> created notify request handler: ~p", [State, Pid]),
	    loop(S#mg{req_handler = Pid});


	%% Make a sync-call
	{notify_request_complete, Reply, Pid} when S#mg.req_handler == Pid ->
	    i("loop(~p) -> received notify request complete from "
	      "~n   ~p with"
	      "~n   Reply: ~p", 
	      [State, Pid, Reply]),
	    server_reply(S#mg.parent, notify_request_reply, {ok, Reply}),
	    loop(S#mg{req_handler = undefined});


	%% Make a sync-call
	{cancel_request, Reason, Parent} when S#mg.parent == Parent, 
					      State == connected ->
	    i("loop(~p) -> received request to cancel (all) megaco requests ", 
	      [State]),
	    Res = megaco:cancel(S#mg.conn_handle, Reason),
	    server_reply(Parent, cancel_request_reply, Res),
	    loop(S);


	%% Apply some load
	{apply_load, Times, Parent} when S#mg.parent == Parent ->
	    i("loop(~p) -> received apply_load request", [State]),
	    apply_load_timer(),
	    server_reply(Parent, apply_load_ack, ok),
	    loop(S#mg{load_counter = Times});


	apply_load_timeout ->
	    d("loop(~p) -> received apply_load timeout [~p]", 
	      [State, S#mg.load_counter]),
	    S1 = do_apply_load(S),
	    loop(S1);


	%% Megaco callback messages
	{request, Request, From} ->
	    d("loop(~p) -> received megaco request: ~n~p~n   From: ~p", 
	      [State, Request, From]),
	    {Reply, NewS} = handle_megaco_request(Request, S),
	    d("loop(~p) -> send (megaco callback) request reply: ~n~p", 
	      [NewS#mg.state, Reply]),
	    From ! {reply, Reply, self()},
	    loop(NewS);


	{'EXIT', Pid, Reason} when S#mg.req_handler == Pid ->
	    error_msg("MG ~p "
		      "received unexpected exit from the request handler:"
		      "~n   ~p", 
		      [S#mg.mid, Reason]),
	    server_reply(S#mg.parent, notify_request_reply, 
			 {error, {request_handler_exit, Reason}}),
	    loop(S#mg{req_handler = undefined});


	{'EXIT', Pid, Reason} ->
	    error_msg("MG ~p received unexpected exit signal from ~p:~n~p", 
		      [S#mg.mid, Pid, Reason]),
	    loop(S);


	Invalid ->
	    i("loop(~p) -> received invalid request: ~p", [State, Invalid]),
	    loop(S)

    end.


do_reset_stats(CH) ->
    megaco:reset_stats(),
    case (catch megaco:conn_info(CH, send_mod)) of
	{error, Reason} ->
	    error_msg("unexpected result when retrieving send module for "
		      "own connection ~p: ~p. "
		      "~nexiting...", [CH, Reason]),
	    exit({invalid_connection, CH, Reason});
	{'EXIT', Reason} ->
	    error_msg("exit signal when retrieving send module for "
		      "own connection ~p: ~p. "
		      "~nexiting...", [CH, Reason]),
	    exit({invalid_connection, CH, Reason});
	SendMod when atom(SendMod) ->
	    SendMod:reset_stats()
    end.



close_conn(CH) ->
    Reason = {self(), ignore},
    Pid        = megaco:conn_info(CH, control_pid),
    SendMod    = megaco:conn_info(CH, send_mod),
    SendHandle = megaco:conn_info(CH, send_handle),
    megaco:disconnect(CH, Reason),
    case SendMod of
	megaco_tcp -> megaco_tcp:close(SendHandle);
	megaco_udp -> megaco_udp:close(SendHandle);
	SendMod    -> exit(Pid, Reason)
    end.


parse_receive_info(RI, RH) ->
    d("parse_receive_info -> get encoding module"),
    EM = get_encoding_module(RI),
    d("parse_receive_info -> get encoding config"),
    EC = get_encoding_config(RI, EM),
    d("parse_receive_info -> get transport module"),
    TM = get_transport_module(RI),
    d("parse_receive_info -> get transport port"),
    TP = get_transport_port(RI),
    RH1 = RH#megaco_receive_handle{send_mod        = TM,
				   encoding_mod    = EM,
				   encoding_config = EC},
    {TP, RH1}.


start_transport(MgcPort,
		   #megaco_receive_handle{send_mod = megaco_tcp} = RH) ->
    start_tcp(MgcPort,RH);
start_transport(MgcPort,
		   #megaco_receive_handle{send_mod = megaco_udp} = RH) ->
    start_udp(MgcPort,RH);
start_transport(_, #megaco_receive_handle{send_mod = Mod}) ->
    throw({error, {bad_send_mod, Mod}}).


start_tcp(MgcPort, RH) ->
    d("start tcp transport"),
    case megaco_tcp:start_transport() of
	{ok, Sup} ->
	    {ok, LocalHost} = inet:gethostname(),
	    Opts = [{host, LocalHost},{port, MgcPort}, {receive_handle, RH}],
	    case megaco_tcp:connect(Sup, Opts) of
		{ok, SendHandle, ControlPid} ->
% 		    {ok, LocalHost} = inet:gethostname(),
%                     PrelMgcMid = preliminary_mid,
% 		    {ok, ConnHandle} = 
% 			megaco:connect(RH, PrelMgcMid, 
% 				       SendHandle, ControlPid),
% 		    ConnHandle;
		    megaco_tcp_connect(RH, LocalHost, SendHandle, ControlPid);
		{error, Reason} ->
		    {error, {megaco_tcp_connect, Reason}}
	    end;
        {error, Reason} ->
            {error, {megaco_tcp_start_transport, Reason}}
    end.

megaco_tcp_connect(RH, LocalHost, SendHandle, ControlPid) ->
    PrelMgcMid = preliminary_mid,
    {ok, CH} = megaco:connect(RH, PrelMgcMid, SendHandle, ControlPid),
    CH.

start_udp(MgcPort, RH) ->
    d("start udp transport"),
    case megaco_udp:start_transport() of
	{ok, Sup} ->
	    Opts = [{port, 0}, {receive_handle, RH}],
	    case megaco_udp:open(Sup, Opts) of
		{ok, Handle, ControlPid} ->
% 		    {ok, LocalHost} = inet:gethostname(),
%                     MgcMid = preliminary_mid,
%                     SendHandle = megaco_udp:create_send_handle(Handle, 
% 							       LocalHost, 
% 							       MgcPort),
% 		    {ok, ConnHandle} = 
% 			megaco:connect(RH, MgcMid, 
% 				       SendHandle, ControlPid),
% 		    ConnHandle;
		    megaco_udp_connect(MgcPort, RH, Handle, ControlPid);
		{error, Reason} ->
                    {error, {megaco_udp_open, Reason}}
	    end;
        {error, Reason} ->
            {error, {megaco_udp_start_transport, Reason}}
    end.

megaco_udp_connect(MgcPort, RH, Handle, ControlPid) ->
    {ok, LocalHost} = inet:gethostname(),
    MgcMid     = preliminary_mid,
    SendHandle = megaco_udp:create_send_handle(Handle, LocalHost, MgcPort),
    {ok, CH}   = megaco:connect(RH, MgcMid, SendHandle, ControlPid),
    CH.

do_service_change(ConnHandle) ->
    do_service_change(ConnHandle, restart, ?megaco_cold_boot).

do_service_change(ConnHandle, Method, Reason) ->
    SCP    = cre_serviceChangeParm(Method, [Reason]),
    TermId = [?megaco_root_termination_id],
    SCR    = cre_serviceChangeReq(TermId, SCP),
    CR     = cre_commandReq({serviceChangeReq, SCR}),
    AR     = cre_actionReq(?megaco_null_context_id,[CR]),
    megaco:cast(ConnHandle, [AR], []).


do_apply_load(#mg{conn_handle  = CH, 
		  call_mode    = Mode, 
		  group_size   = Sz,
		  load_counter = N0} = S) ->
    d("do_apply_load -> entry with"
      "~n   Mode: ~p"
      "~n   Sz:   ~p"
      "~n   N0:   ~p", [Mode, Sz, N0]),
    {NofSent, Actions, ReplyData} = make_notify_request(N0, Sz),
    d("do_apply_load -> notifications constructed:"
      "~n   NofSent:   ~p"
      "~n   Actions:   ~p"
      "~n   ReplyData: ~p", [NofSent, Actions, ReplyData]),
    N = N0 - NofSent,
    case Mode of
	sync ->
	    Result = megaco:call(CH, Actions, []),
	    d("do_apply_load -> call result:~n   ~p", [Result]),
	    case N of
		0 ->
		    Pid = S#mg.parent,
		    Pid ! {load_complete, self()},
		    S#mg{call_mode = async, load_counter = 0};
		_ ->
		    apply_load_timer(),
		    S#mg{call_mode = async, load_counter = N}
	    end;
	async ->
	    Result = megaco:cast(CH, Actions, [{reply_data, ReplyData}]),
	    d("do_apply_load -> cast result:~n   ~p", [Result]),
	    S#mg{call_mode     = sync, 
		 load_counter  = N, 
		 reply_counter = NofSent} % Outstanding replies
    end.


start_notify_request_handler(#mg{conn_handle = CH, group_size = N}) ->
    Env = get(),
    spawn_link(?MODULE, notify_request_handler_main, [self(), Env, CH, N]).

notify_request_handler_main(Parent, Env, CH, N) ->
    [put(Tag, Val) || {Tag, Val} <- Env],
    d("notify_request_handler_main -> entry with"
      "~n   Parent: ~p"
      "~n   CH:     ~p"
      "~n   N:      ~p", [Parent, CH, N]),
    Res = do_notify_request(CH, N),
    d("notify_request_handler_main -> notify complete:"
      "~n   Res: ~p", [Res]),
    Parent ! {notify_request_complete, Res, self()},
    unlink(Parent),
    exit(normal).

do_notify_request(CH,N) when N =< 0 ->
    d("do_notify_request(~p) -> ignoring", [N]),
    ignore;
do_notify_request(CH,1) ->
    d("do_notify_request(1) -> entry with"),
    {Action, _} = make_notify_request(),
    megaco:call(CH, Action, []);
do_notify_request(CH,N) ->
    d("do_notify_request(~p) -> entry with", [N]),
    {N, Actions, _} = make_notify_request(N,N),
    megaco:call(CH, Actions, []).

make_notify_request(N, Sz) when N >= Sz, Sz > 0 ->
    {Req, ReplyData} = make_notify_request(N, Sz, [], []),
    {Sz, Req, ReplyData};
make_notify_request(N, _Sz) when N > 0 ->
    {Req, ReplyData} = make_notify_request(N, N, [], []),
    {N, Req, ReplyData}.


    
make_notify_request(_Offset, 0, Actions, ReplyDatas) ->
    {lists:reverse(Actions), lists:reverse(ReplyDatas)};
make_notify_request(Offset, N, Actions, ReplyDatas) when N > 0 ->
    TimeStamp = cre_timeNotation(),
    Event     = cre_observedEvent("al/of", TimeStamp),
    Desc      = cre_observedEventsDesc(2220 + N, [Event]),
    NotifyReq = cre_notifyReq([#megaco_term_id{id = tid(100+Offset-N)}],Desc),
    CmdReq    = cre_commandReq({notifyReq, NotifyReq}),
    ActReq    = cre_actionReq(?megaco_null_context_id, [CmdReq]),
    make_notify_request(Offset, N-1, [[ActReq]|Actions], [Desc|ReplyDatas]).
    
make_notify_request() ->
    TimeStamp  = cre_timeNotation("19990729", "22000000"),
    Event      = cre_observedEvent("al/of",   TimeStamp),
    Desc1      = cre_observedEventsDesc(2221, [Event]),
    Desc2      = cre_observedEventsDesc(2222, [Event]),
    Desc3      = cre_observedEventsDesc(2223, [Event]),
    Desc4      = cre_observedEventsDesc(2224, [Event]),
    NotifyReq1 = cre_notifyReq([#megaco_term_id{id = ?A4444}], Desc1),
    NotifyReq2 = cre_notifyReq([#megaco_term_id{id = ?A4445}], Desc2),
    CmdReq1    = cre_commandReq({notifyReq, NotifyReq1}),
    CmdReq2    = cre_commandReq({notifyReq, NotifyReq2}),
    ActReq     = cre_actionReq(?megaco_null_context_id, [CmdReq1,CmdReq2]),
    {[ActReq], [Desc1,Desc2]}.


cre_actionReq(Cid, Cmds) ->
    #'ActionRequest'{contextId       = Cid,
		     commandRequests = Cmds}.

cre_commandReq(Cmd) ->
    #'CommandRequest'{command = Cmd}.

cre_serviceChangeReq(TermId, Parms) ->
    #'ServiceChangeRequest'{terminationID      = TermId,
			    serviceChangeParms = Parms}.

cre_serviceChangeParm(Method, Reason) ->
    #'ServiceChangeParm'{serviceChangeMethod = Method,
			 serviceChangeReason = Reason}.

cre_notifyReq(Tid, EvsDesc) ->
    #'NotifyRequest'{terminationID = Tid, observedEventsDescriptor = EvsDesc}.

% cre_notifyRep(Tid) ->
%     #'NotifyReply'{terminationID = [Tid]}.

% cre_notifyRep(Tid,Err) ->
%     #'NotifyReply'{terminationID = [Tid], errorDescriptor = Err}.

cre_observedEventsDesc(Id, EvList) ->
    #'ObservedEventsDescriptor'{requestId = Id, observedEventLst = EvList}.

cre_observedEvent(Name, Not) ->
    #'ObservedEvent'{eventName = Name, timeNotation = Not}.

cre_timeNotation() ->
    {{Year,Month,Day},{Hour,Min,Sec}} = calendar:universal_time(),
    D = lists:flatten(io_lib:format("~4..0w~2..0w~2..0w", [Year,Month,Day])),
    T = lists:flatten(io_lib:format("~2..0w~2..0w~4..0w", [Hour,Min,Sec])),
    cre_timeNotation(D, T).
    
cre_timeNotation(D,T) ->
    #'TimeNotation'{date = D, time = T}.

cre_error_descr(Code,Text) ->
    #'ErrorDescriptor'{errorCode = Code, errorText = Text}.

% cre_error_descr(Code,FormatString,Args) ->
%     Text = lists:flatten(io_lib:format(FormatString,Args)),
%     cre_error_descr(Code,Text).


%% -----------------------
%% Handle megaco callbacks
%%

handle_megaco_request({handle_connect, CH, _PV}, 
		  #mg{state = connecting} = S) ->
    {ok, S#mg{conn_handle = CH}};

handle_megaco_request({handle_disconnect, CH, _PV, _R}, S) ->
    {ok, S#mg{conn_handle = CH}};

handle_megaco_request({handle_syntax_error, _RH, _PV, _ED}, S) ->
    {reply, S};

handle_megaco_request({handle_message_error, CH, _PV, _ED}, S) ->
    {no_reply, S#mg{conn_handle = CH}};

handle_megaco_request({handle_trans_request, CH, _PV, _AR}, S) ->
    ED =  cre_error_descr(?megaco_not_implemented,
			  "Transaction requests not handled"),
    {{discard_ack, ED}, S#mg{conn_handle = CH}};

handle_megaco_request({handle_trans_long_request, CH, _PV, _RD}, S) ->
    ED = cre_error_descr(?megaco_not_implemented,
			 "Long transaction requests not handled"),
    {{discard_ack,  ED}, S#mg{conn_handle = CH}};

handle_megaco_request({handle_trans_reply, CH, PV, AR, RD}, S) ->
    S1 = do_handle_trans_reply(CH, PV, AR, RD, S),
    {ok, S1};

handle_megaco_request({handle_trans_ack, _CH, _PV, _AS, _AD}, S) ->
    {ok, S}.

do_handle_trans_reply(CH, _PV, {ok, Rep}, _RD, 
		      #mg{parent = Pid, state = connecting} = S) ->
    %% Should really check this...
    d("do_handle_trans_reply(connecting) -> received "
      "~n   Rep: ~p", [Rep]),
    server_reply(Pid, service_change_reply, ok),
    S#mg{conn_handle = CH, state = connected};
do_handle_trans_reply(_CH, _PV, {ok, Reps}, _RD, 
		      #mg{parent = Pid, load_counter = 0} = S) ->
    d("do_handle_trans_reply(load_counter = 0) -> reply received ", []),
    handle_trans_reply_verify_act(Reps),
    server_reply(Pid, load_complete, ok),
    S#mg{reply_counter = 0}; % Just in case
do_handle_trans_reply(_CH, _PV, {ok, Reps}, _, 
		      #mg{reply_counter = 0} = S) ->
    d("do_handle_trans_reply(reply_counter = 0) -> reply received", []),
    handle_trans_reply_verify_act(Reps),
    apply_load_timer(),
    S;
do_handle_trans_reply(_CH, _PV, {ok, Reps}, _, 
		      #mg{reply_counter = N} = S) ->
    d("do_handle_trans_reply(reply_counter = ~p) -> reply received ", [N]),
    handle_trans_reply_verify_act(Reps),
    apply_load_timer(),
    S#mg{reply_counter = N-1};
do_handle_trans_reply(_CH, _PV, {error, ED}, _RD, S) ->
    i("unexpected error transaction: ~p", [ED]),
    S.


handle_trans_reply_verify_act([]) ->
    ok;
handle_trans_reply_verify_act([#'ActionReply'{commandReply = Rep}|Reps]) ->
    handle_trans_reply_verify_cmd(Rep),
    handle_trans_reply_verify_act(Reps);
handle_trans_reply_verify_act([Rep|Reps]) ->
    i("received 'propably' unexpected reply: ~n~p", [Rep]),
    handle_trans_reply_verify_act(Reps).

handle_trans_reply_verify_cmd([]) ->
    ok;
handle_trans_reply_verify_cmd([Cmd|Cmds]) ->
    case Cmd of
	{notifyReply, #'NotifyReply'{terminationID = [Tid]}} ->
	    d("received expected notification reply from ~n   ~p", [Tid]);
	Else ->
	    i("received unexpected notification reply ~n~p", [Else])
    end,
    handle_trans_reply_verify_cmd(Cmds).
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

notify_started(Parent) ->
    Parent ! {started, self()}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The megaco user callback interface 

handle_connect(CH, PV, Pid) ->
    case CH#megaco_conn_handle.remote_mid of
        preliminary_mid ->
	    %% Avoids deadlock
	    ok;
	_ ->
	    Reply = request(Pid, {handle_connect, CH, PV}),
	    Reply
    end.

handle_disconnect(CH, PV, 
		  {user_disconnect, {Pid, ignore}}, 
		  Pid) ->
    ok;
handle_disconnect(CH, PV, R, Pid) ->
    request(Pid, {handle_disconnect, CH, PV, R}).

handle_syntax_error(ReceiveHandle, ProtocolVersion, ErrorDescriptor, Pid) ->
    Req = {handle_syntax_error, ReceiveHandle, ProtocolVersion, 
	   ErrorDescriptor},
    request(Pid, Req).
    
handle_message_error(ConnHandle, ProtocolVersion, ErrorDescriptor, Pid) ->
    Req = {handle_message_error, ConnHandle, ProtocolVersion, ErrorDescriptor},
    request(Pid, Req).

handle_trans_request(CH, PV, AR, Pid) ->
    Reply = request(Pid, {handle_trans_request, CH, PV, AR}),
    Reply.

handle_trans_long_request(ConnHandle, ProtocolVersion, ReqData, Pid) ->
    Req = {handle_trans_long_request, ConnHandle, ProtocolVersion, ReqData},
    request(Pid, Req).

handle_trans_reply(ConnHandle, ProtocolVersion, ActualReply, ReplyData, Pid) ->
    Req = {handle_trans_reply, ConnHandle, ProtocolVersion, 
	   ActualReply, ReplyData},
    request(Pid, Req).

handle_trans_ack(ConnHandle, ProtocolVersion, AckStatus, AckData, Pid) ->
    Req = {handle_trans_ack, ConnHandle, ProtocolVersion, AckStatus, AckData},
    request(Pid, Req).


request(Pid, Request) ->
    Pid ! {request, Request, self()},
    receive
	{reply, {delay, To, ED}, Pid} ->
	    sleep(To),
	    {discard_ack, ED};
	{reply, Reply, Pid} ->
	    Reply
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sleep(X) ->
    receive after X -> ok end.


error_msg(F,A) -> error_logger:error_msg(F ++ "~n",A).


get_encoding_module(RI) ->
    case (catch get_conf(encoding_module, RI)) of
	{error, _} ->
	    undefined;
	Val ->
	    Val
    end.

get_encoding_config(RI, EM) ->
    case text_codec(EM) of
	true ->
	    case megaco:system_info(text_config) of
		[Conf] when list(Conf) ->
		    Conf;
		_ ->
		    []
	    end;

	false ->
	    get_conf(encoding_config, RI)
    end.

text_codec(megaco_compact_text_encoder) ->
    true;
text_codec(megaco_pretty_text_encoder) ->
    true;
text_codec(_) ->
    false.


get_transport_module(RI) ->
    get_conf(transport_module, RI).

get_transport_port(RI) ->
    get_conf(port, RI).


get_conf(Key, Config) ->
    case lists:keysearch(Key, 1, Config) of
	{value, {Key, Val}} ->
	    Val;
	_ ->
	    exit({error, {not_found, Key, Config}})
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tid(N) when N >= 0 ->
    {Rem1, Val1} = num2str(N),
    {Rem2, Val2} = num2str(Rem1),
    {0,    Val3} = num2str(Rem2),
    [Val3, Val2, Val1].

num2str(N) when N >= 0 ->
    num2str(N, []).

num2str(Rem, Val) when length(Val) == 8 ->
    {Rem, Val};
num2str(N, Val) ->
    D = N div 2,
    case N rem 2 of
	1 ->
	    num2str(D, [$1|Val]);
	0 -> 
	    num2str(D, [$0|Val])
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

i(F) ->
    i(F, []).

i(F, A) ->
    print(info, get(verbosity), "", F, A).


d(F) ->
    d(F, []).

d(F, A) ->
    print(debug, get(verbosity), "DBG: ", F, A).


printable(_, debug)   -> true;
printable(info, info) -> true;
printable(_,_)        -> false.

print(Severity, Verbosity, P, F, A) ->
    print(printable(Severity,Verbosity), P, F, A).

print(true, P, F, A) ->
    io:format("~s~p:~s: " ++ F ++ "~n", [P, self(), get(sname) | A]);
print(_, _, _, _) ->
    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

random_init() ->
    {A,B,C} = now(),
    random:seed(A,B,C).

random() ->
    10 * random:uniform(50).

apply_load_timer() ->
    erlang:send_after(random(), self(), apply_load_timeout).

