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
%% Purpose: Implements an "MGC" used by the test suite
%%----------------------------------------------------------------------
-module(megaco_test_mgc).

-export([start/4, start/5, stop/1,
	 get_stats/2, reset_stats/1,
	 user_info/1, user_info/2, conn_info/1, conn_info/2, 
	 update_user_info/3, update_conn_info/3, 
	 request_ignore/1, 
	 request_discard/1, request_discard/2, 
	 request_pending/1, request_pending/2, request_pending_ignore/1, 
	 request_handle/1, request_handle/2, 
	 request_handle_sloppy/1, 
	 ack_info/2, req_info/2, 
	 verbosity/2]).
-export([mgc/3]).

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

-define(A4444, ["11111111", "00000000", "00000000"]).
-define(A4445, ["11111111", "00000000", "11111111"]).
-define(A5555, ["11111111", "11111111", "00000000"]).
-define(A5556, ["11111111", "11111111", "11111111"]).

-define(valid_actions, 
	[ignore, pending, pending_ignore, discard_ack, handle_ack, handle_sloppy_ack]).

-record(mgc, {parent      = undefined,
	      tcp_sup     = undefined,
	      udp_sup     = undefined,
	      req_action  = discard_ack,
	      req_timeout = 0,
	      mid         = undefined,
	      ack_info    = undefined,
	      req_info    = undefined,
	      mg          = []}).


%%% ------------------------------------------------------------------

start(Node, Mid, ET, Verbosity) ->
    Conf = [{megaco_trace, false}],
    start(Node, Mid, ET, Conf, Verbosity).

start(Node, Mid, ET, Conf, Verbosity) ->
    d("start mgc[~p]: ~p", [Node, Mid]),
    RI = {receive_info, mk_recv_info(ET)},
    Config = [{local_mid, Mid}, RI] ++ Conf,
    Pid = spawn_link(Node, ?MODULE, mgc, [self(), Verbosity, Config]),
    await_started(Pid).

mk_recv_info(ET) ->
    mk_recv_info(ET, []).

mk_recv_info([], Acc) ->
    Acc;
mk_recv_info([{Encoding, Transport}|ET], Acc) ->
    {EMod, Port} = select_encoding(Encoding),
    TMod         = select_transport(Transport),
    RI = [{encoding_module,  EMod},
	  {encoding_config,  []},
	  {transport_module, TMod},
	  {port,             Port}],
    mk_recv_info(ET, [RI|Acc]);
mk_recv_info([{Encoding, EC, Transport}|ET], Acc) ->
    {EMod, Port} = select_encoding(Encoding),
    TMod         = select_transport(Transport),
    RI = [{encoding_module,  EMod},
	  {encoding_config,  EC},
	  {transport_module, TMod},
	  {port,             Port}],
    mk_recv_info(ET, [RI|Acc]);
mk_recv_info([ET|_], _) ->
    throw({error, {invalid_encoding_transport, ET}}).

select_encoding(text) ->
    {megaco_pretty_text_encoder, 2944};
select_encoding(pretty_text) ->
    {megaco_pretty_text_encoder, 2944};
select_encoding(compact_text) ->
    {megaco_compact_text_encoder, 2944};
select_encoding(binary) ->
    {megaco_ber_bin_encoder, 2945};
select_encoding(erl_dist) ->
    {megaco_erl_dist_encoder, 2946};
select_encoding(Encoding) ->
    throw({error, {invalid_encoding, Encoding}}).

select_transport(tcp) ->
    megaco_tcp;
select_transport(udp) ->
    megaco_udp;
select_transport(Transport) ->
    throw({error, {invalid_transport, Transport}}).


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

ack_info(Pid, InfoPid) ->
    Pid ! {ack_info, InfoPid, self()}.

req_info(Pid, InfoPid) ->
    Pid ! {req_info, InfoPid, self()}.

verbosity(Pid, V) ->
    Pid ! {verbosity, V, self()}.

request_ignore(Pid) ->
    request_action(Pid, {ignore, infinity}).

request_pending_ignore(Pid) ->
    request_action(Pid, {pending_ignore, infinity}).

request_discard(Pid) ->
    request_discard(Pid,0).

request_discard(Pid, To) ->
    request_action(Pid, {discard_ack, To}).

request_pending(Pid) ->
    request_pending(Pid, 5000).

request_pending(Pid, To) ->
    request_action(Pid, {pending, To}).

request_handle(Pid) ->
    request_handle(Pid, 0).

request_handle(Pid, To) ->
    request_action(Pid, {handle_ack, To}).

request_handle_sloppy(Pid) ->
    request_action(Pid, {handle_sloppy_ack, 0}).

request_action(Pid, Action) ->
    server_request(Pid, request_action, Action, request_action_ack).


server_request(Pid, Req, ReplyTag) ->
    Pid ! {Req, self()},
    receive
	{ReplyTag, Reply, Pid} ->
	    Reply;
	{'EXIT', Pid, Reason} ->
	    exit({failed, Req, Pid, Reason})
    after 10000 ->
	    exit({timeout, Req, Pid})
    end.

server_request(Pid, Req, ReqData, ReplyTag) ->
    Pid ! {Req, ReqData, self()},
    receive
	{ReplyTag, Reply, Pid} ->
	    Reply;
	{'EXIT', Pid, Reason} ->
	    exit({failed, Req, Pid, Reason})
    after 10000 ->
	    exit({timeout, Req, Pid})
    end.


server_reply(Pid, ReplyTag, Reply) ->
    Pid ! {ReplyTag, Reply, self()}.


%%% ------------------------------------------------------------------


mgc(Parent, Verbosity, Config) ->
    process_flag(trap_exit, true),
    put(verbosity, Verbosity),
    put(sname,   "MGC"),
    i("mgc -> starting"),
    {Mid, TcpSup, UdpSup} = init(Config),
    notify_started(Parent),
    S = #mgc{parent = Parent, 
	     tcp_sup = TcpSup, udp_sup = UdpSup, mid = Mid},
    i("mgc -> started"),
    loop(S).

init(Config) ->
    d("init -> entry"),
    Mid = get_conf(local_mid, Config),
    RI  = get_conf(receive_info, Config),

    d("init -> start megaco"),
    application:start(megaco),

    d("init -> possibly enable megaco trace"),
    case lists:keysearch(megaco_trace, 1, Config) of
	{value, {megaco_trace, true}} ->
	    megaco:enable_trace(max, io);
	_ ->
	    ok
    end,
    Conf0 = lists:keydelete(megaco_trace,    1, Config),

    d("init -> start megaco user"),
    Conf1 = lists:keydelete(local_mid,    1, Conf0),
    Conf2 = lists:keydelete(receive_info, 1, Conf1),
    ok = megaco:start_user(Mid, Conf2),

    d("init -> update user info (user_mod)"),
    ok = megaco:update_user_info(Mid, user_mod,  ?MODULE),

    d("init -> update user info (user_args)"),
    ok = megaco:update_user_info(Mid, user_args, [self()]),

    d("init -> get user info (receive_handle)"),
    RH = megaco:user_info(Mid,receive_handle),
    d("init -> parse receive info"),
    ListenTo = parse_receive_info(RI, RH),

    d("init -> start transports"),
    {Tcp, Udp} = start_transports(ListenTo),
    {Mid, Tcp, Udp}.
    

loop(S) ->
    d("loop -> await request"),
    receive
	{stop, Parent} when S#mgc.parent == Parent ->
	    i("loop -> stopping", []),
  	    Mid = S#mgc.mid,
	    (catch close_conns(Mid)),
	    megaco:stop_user(Mid),
	    application:stop(megaco),
	    i("loop -> stopped", []),
	    server_reply(Parent, stopped, ok),
	    exit(normal);


	{{update_user_info, Tag, Val}, Parent} when S#mgc.parent == Parent ->
	    i("loop -> got update_user_info: ~w -> ~p", [Tag, Val]),
	    Res = (catch megaco:update_user_info(S#mgc.mid, Tag, Val)),
	    d("loop -> Res: ~p", [Res]),
	    server_reply(Parent, update_user_info_ack, Res),
	    loop(S);

	{{user_info, Tag}, Parent} when S#mgc.parent == Parent ->
	    i("loop -> got user_info request for ~w", [Tag]),
	    Res = (catch megaco:user_info(S#mgc.mid, Tag)),
	    d("loop -> Res: ~p", [Res]),
	    server_reply(Parent, user_info_ack, Res),
	    loop(S);

	{{update_conn_info, Tag, Val}, Parent} when S#mgc.parent == Parent ->
	    i("loop -> got update_conn_info: ~w -> ~p", [Tag, Val]),
	    Conns = megaco:user_info(S#mgc.mid, connections), 
	    Fun = fun(CH) ->
			  (catch megaco:update_conn_info(CH, Tag, Val))
		  end,
	    Res = lists:map(Fun, Conns),
	    d("loop -> Res: ~p", [Res]),
	    server_reply(Parent, update_conn_info_ack, Res),
	    loop(S);

	{{conn_info, Tag}, Parent} when S#mgc.parent == Parent ->
	    i("loop -> got conn_info request for ~w", [Tag]),
	    Conns = megaco:user_info(S#mgc.mid, connections), 
	    Fun = fun(CH) ->
			  (catch megaco:conn_info(CH, Tag))
		  end,
	    Res = lists:map(Fun, Conns),
	    d("loop -> Res: ~p", [Res]),
	    server_reply(Parent, conn_info_ack, Res),
	    loop(S);


	%% 
	{request_action, {Action, To}, Parent} when S#mgc.parent == Parent ->
	    i("loop -> got new request_action: ~p:~w", [Action,To]),
	    {Reply, S1} = 
		case lists:member(Action, ?valid_actions) of
		    true when To >= 0 ->
			{{ok, S#mgc.req_action}, 
			 S#mgc{req_action = Action, req_timeout = To}};
		    true ->
			{{error, {invalid_action_timeout, To}}, S};
		    false ->
			{{error, {invalid_action, Action}}, S}
		end,
	    server_reply(Parent, request_action_ack, Reply),
	    loop(S1);


	%% Reset stats
	{reset_stats, Parent} when S#mgc.parent == Parent ->
	    i("loop -> got request to reset stats counters"),
	    do_reset_stats(S#mgc.mid),
	    server_reply(Parent, reset_stats_ack, ok),
	    loop(S);


	%% Give me statistics
	{{statistics, 1}, Parent} when S#mgc.parent == Parent ->
	    i("loop -> got request for statistics 1"),
	    {ok, Gen} = megaco:get_stats(),
	    GetTrans = 
		fun(CH) ->
			Reason = {statistics, CH}, 
			Pid = megaco:conn_info(CH, control_pid),
			SendMod = megaco:conn_info(CH, send_mod),
			SendHandle = megaco:conn_info(CH, send_handle),
			{ok, Stats} = 
			    case SendMod of
				megaco_tcp -> megaco_tcp:get_stats(SendHandle);
				megaco_udp -> megaco_udp:get_stats(SendHandle);
				SendMod    -> exit(Pid, Reason)
			    end,
			{SendHandle, Stats}
		end,
	    Mid = S#mgc.mid,
	    Trans = 
		lists:map(GetTrans, megaco:user_info(Mid, connections)),
	    Reply = {ok, [{gen, Gen}, {trans, Trans}]},
	    server_reply(Parent, {statistics_reply, 1}, Reply),
	    loop(S);


	{{statistics, 2}, Parent} when S#mgc.parent == Parent ->
	    i("loop -> got request for statistics 2"),
	    {ok, Gen} = megaco:get_stats(),
	    #mgc{tcp_sup = TcpSup, udp_sup = UdpSup} = S,
	    TcpStats = get_trans_stats(TcpSup, megaco_tcp),
	    UdpStats = get_trans_stats(UdpSup, megaco_udp),
	    Reply = {ok, [{gen, Gen}, {trans, [TcpStats, UdpStats]}]},
	    server_reply(Parent, {statistics_reply, 2}, Reply),
	    loop(S);


	%% Megaco callback messages
	{request, Request, From} ->
	    d("loop -> received megaco request from ~p:~n~p", 
	      [From, Request]),
	    {Reply, S1} = handle_megaco_request(Request, S),
	    d("loop -> send request reply: ~n~p", [Reply]),
	    reply(From, Reply),
	    loop(S1);


	{ack_info, To, Parent} when S#mgc.parent == Parent ->
	    i("loop -> received request to inform about received ack's ", []),
	    loop(S#mgc{ack_info = To});


	{req_info, To, Parent} when S#mgc.parent == Parent ->
	    i("loop -> received request to inform about received ack's ", []),
	    loop(S#mgc{req_info = To});


	{verbosity, V, Parent} when S#mgc.parent == Parent ->
	    i("loop -> received new verbosity: ~p", [V]),
	    put(verbosity,V),
	    loop(S);


	{'EXIT', Pid, Reason} ->
	    error_msg("MGC received unexpected exit signal from ~p:~n~p", 
		      [Pid, Reason]),
	    loop(S);


	Invalid ->
	    i("loop -> received invalid request: ~p", [Invalid]),
	    loop(S)
    end.


do_reset_stats(Mid) ->
    megaco:reset_stats(),
    do_reset_trans_stats(megaco:user_info(Mid, connections), []).

do_reset_trans_stats([], Reset) ->
    ok;
do_reset_trans_stats([CH|CHs], Reset) ->
    SendMod = megaco:conn_info(CH, send_mod),
    case lists:member(SendMod, Reset) of
	true ->
	    do_reset_trans_stats(CHs, Reset);
	false ->
	    SendMod:reset_stats(),
	    do_reset_trans_stats(CHs, [SendMod|Reset])
    end.
	    

close_conns(Mid) ->
    Reason = {self(), ignore},
    Disco  = fun(CH) ->
		     (catch do_close_conn(CH, Reason))
	     end,
    lists:map(Disco, megaco:user_info(Mid, connections)).

do_close_conn(CH, Reason) ->
    d("close connection to ~p", [CH#megaco_conn_handle.remote_mid]),
    Pid        = megaco:conn_info(CH, control_pid),
    SendMod    = megaco:conn_info(CH, send_mod),
    SendHandle = megaco:conn_info(CH, send_handle),
    megaco:disconnect(CH, Reason),
    case SendMod of
	megaco_tcp -> megaco_tcp:close(SendHandle);
	megaco_udp -> megaco_udp:close(SendHandle);
	SendMod    -> exit(Pid, Reason)
    end.
    
get_trans_stats(P, SendMod) when pid(P) ->
    case (catch SendMod:get_stats()) of
	{ok, Stats} ->
	    {SendMod, Stats};
	Else ->
	    {SendMod, Else}
    end;
get_trans_stats(P, SendMod) ->
    {SendMod, undefined}.

parse_receive_info([], RH) ->
    throw({error, no_receive_info});
parse_receive_info(RI, RH) ->
    parse_receive_info(RI, RH, []).

parse_receive_info([], RH, ListenTo) ->
    ListenTo;
parse_receive_info([RI|RIs], RH, ListenTo) ->
    d("parse_receive_info -> parse receive info"),
    case (catch parse_receive_info1(RI, RH)) of
	{error, Reason} ->
	    i("failed parsing receive info: ~p~n~p", [RI, Reason]),
	    exit({failed_parsing_recv_info, RI, Reason});
	RH1 ->
	    parse_receive_info(RIs, RH, [RH1|ListenTo])
    end.
    
parse_receive_info1(RI, RH) ->
    d("parse_receive_info1 -> get encoding module"),
    EM = get_encoding_module(RI),
    d("parse_receive_info1 -> get encoding config"),
    EC = get_encoding_config(RI, EM),
    d("parse_receive_info1 -> get transport module"),
    TM = get_transport_module(RI),
    d("parse_receive_info1 -> get transport port"),
    TP = get_transport_port(RI),
    RH1 = RH#megaco_receive_handle{send_mod        = TM,
				   encoding_mod    = EM,
				   encoding_config = EC},
    {TP, RH1}.


start_transports([]) ->
    throw({error, no_transport});
start_transports(ListenTo) ->
    start_transports(ListenTo, undefined, undefined).
    

start_transports([], TcpSup, UdpSup) ->
    {TcpSup, UdpSup};
start_transports([{Port, RH}|ListenTo], TcpSup, UdpSup) 
  when RH#megaco_receive_handle.send_mod == megaco_tcp ->
    TcpSup1 = start_tcp(RH, Port, TcpSup),
    start_transports(ListenTo, TcpSup1, UdpSup);
start_transports([{Port, RH}|ListenTo], TcpSup, UdpSup) 
  when RH#megaco_receive_handle.send_mod == megaco_udp ->
    UdpSup1 = start_udp(RH, Port, UdpSup),
    start_transports(ListenTo, TcpSup, UdpSup1);
start_transports([{Port, RH}|ListenTo], _TcpSup, _UdpSup) ->
    throw({error, {bad_send_mod, RH#megaco_receive_handle.send_mod}}).


start_tcp(RH, Port, undefined) ->
    d("start tcp transport"),
    case megaco_tcp:start_transport() of
	{ok, Sup} ->
	    start_tcp(RH, Port, Sup);
	Else ->
	    throw({error, {failed_starting_tcp_transport, Else}})
    end;
start_tcp(RH, Port, Sup) when pid(Sup) ->
    d("tcp listen on ~p", [Port]),
    Opts = [{port, Port}, {receive_handle, RH}],
    case megaco_tcp:listen(Sup, Opts) of
	ok ->
	    Sup;
	Else ->
	    throw({error, {failed_starting_tcp_listen, Else}})
    end.


start_udp(RH, Port, undefined) ->
    d("start udp transport"),
    case megaco_udp:start_transport() of
	{ok, Sup} ->
	    start_udp(RH, Port, Sup);
	Else ->
	    throw({error, {failed_starting_udp_transport, Else}})
    end;
start_udp(RH, Port, Sup) ->
    d("open udp ~p", [Port]),
    Opts = [{port, Port}, {receive_handle, RH}],
    case megaco_udp:open(Sup, Opts) of
	{ok, _SendHandle, _ControlPid} ->
	    Sup;
	Else ->
	    exit({error, {failed_starting_udp_listen, Else}})
    end.

%% -----------------------
%% Handle megaco callbacks
%%

handle_megaco_request({handle_connect, CH, _PV}, #mgc{mg = MGs} = S) ->
    case lists:member(CH, MGs) of
	true ->
	    i("MG already connected: ~n   ~p", [CH]),
	    {error, S};
	false ->
	    {ok, S#mgc{mg = [CH|MGs]}}
    end;

handle_megaco_request({handle_disconnect, CH, _PV, R}, S) ->
    d("handle_megaco_request(handle_disconnect) -> entry with"
      "~n   CH: ~p"
      "~n   R:  ~p", [CH, R]),
    CancelRes = (catch megaco:cancel(CH, R)), % Cancel the outstanding messages
    d("handle_megaco_request(handle_disconnect) -> megaco cancel result: ~p", [CancelRes]),
    MGs = lists:delete(CH, S#mgc.mg),
    d("handle_megaco_request(handle_disconnect) -> MGs: ~p", [MGs]),
    {ok, S#mgc{mg = MGs}};

handle_megaco_request({handle_syntax_error, _RH, _PV, _ED}, S) ->
    {reply, S};

handle_megaco_request({handle_message_error, _CH, _PV, _ED}, S) ->
    {no_reply, S};

handle_megaco_request({handle_trans_request, CH, PV, ARs}, 
		      #mgc{req_info = P} = S) when pid(P) ->
    d("handle_megaco_request(handle_trans_request,~p) -> entry", [P]),
    P ! {req_received, self(), ARs},
    do_handle_trans_request(CH, PV, ARs, S);
handle_megaco_request({handle_trans_request, CH, PV, ARs}, S) ->
    d("handle_megaco_request(handle_trans_request) -> entry"),
    do_handle_trans_request(CH, PV, ARs, S);

handle_megaco_request({handle_trans_long_request, CH, PV, RD}, S) ->
    d("handle_megaco_request(handle_long_trans_request) -> entry"),
    Reply0 = handle_act_requests(CH, PV, RD, discard_ack),
    Reply  = 
	case S of
	    #mgc{req_action = ignore, req_timeout = To} ->
		d("handle_megaco_request(handle_long_trans_request) -> "
		  "~n   To: ~p", [To]),
		{delay_reply, To, Reply0};
	    _ ->
		d("handle_megaco_request(handle_long_trans_request) -> "
		  "~n   S: ~p", [S]),
		Reply0
	end,
    {Reply, S};

handle_megaco_request({handle_trans_reply, _CH, _PV, _AR, _RD}, S) ->
    {ok, S};

handle_megaco_request({handle_trans_ack, CH, PV, AS, AD}, 
		      #mgc{ack_info = P} = S) when pid(P) ->
    d("handle_megaco_request(handle_trans_ack,~p) -> entry when"
      "~n   CH: ~p"
      "~n   PV: ~p"
      "~n   AS: ~p"
      "~n   AD: ~p", [P, CH, PV, AS, AD]),
    P ! {ack_received, self(), AS},
    {ok, S};

handle_megaco_request({handle_trans_ack, CH, PV, AS, AD}, S) ->
    d("handle_megaco_request(handle_trans_ack) -> entry with"
      "~n   CH: ~p"
      "~n   PV: ~p"
      "~n   AS: ~p"
      "~n   AD: ~p", [CH, PV, AS, AD]),
    {ok, S}.


do_handle_trans_request(CH, PV, ARs, 
			#mgc{req_action = Action, req_timeout = To} = S) ->
    d("do_handle_megaco_request(handle_trans_request) -> entry with"
      "~n   Action: ~p"
      "~n   To:     ~p", [Action, To]),
    case handle_act_requests(CH, PV, ARs, Action) of
	{pending_ignore, ActReqs} ->
	    {{pending, ActReqs}, S#mgc{req_action = ignore}};
	Reply ->
	    {{delay_reply, To, Reply}, S}
    end.


handle_act_requests(_CH, _PV, _ActReqs, ignore) ->
    ignore;
handle_act_requests(CH, PV, ActReqs, pending) ->
    {pending, ActReqs};
handle_act_requests(CH, PV, ActReqs, pending_ignore) ->
    {pending_ignore, ActReqs};
handle_act_requests(CH, PV, ActReqs, handle_ack) ->
    Reply = (catch do_handle_act_requests(CH, PV, ActReqs, [])),
    {{handle_ack, ActReqs}, Reply};
handle_act_requests(CH, PV, ActReqs, handle_sloppy_ack) ->
    Reply = (catch do_handle_act_requests(CH, PV, ActReqs, [])),
    {{handle_sloppy_ack, ActReqs}, Reply};
handle_act_requests(CH, PV, ActReqs, _) ->
    Reply = (catch do_handle_act_requests(CH, PV, ActReqs, [])),
    {discard_ack, Reply}.

do_handle_act_requests(CH, PV, [], ActReplies) ->
    lists:reverse(ActReplies);
do_handle_act_requests(CH, PV, [ActReq|ActReqs], ActReplies) ->
    ActReply = handle_act_request(CH, PV, ActReq),
    do_handle_act_requests(CH, PV, ActReqs, [ActReply|ActReplies]).
    
handle_act_request(CH, PV, ActReq) ->
    #'ActionRequest'{contextId = CtxId, commandRequests = Cmds} = ActReq,
    CmdReplies = handle_cmd_requests(CH, PV, CtxId, Cmds),
    #'ActionReply'{contextId    = CtxId,
		   commandReply = CmdReplies}.

handle_cmd_requests(CH, PV, ?megaco_null_context_id, 
			[#'CommandRequest'{command={serviceChangeReq,Req}}]) ->
    Rep = service_change(CH, PV, Req),
    [{serviceChangeReply, Rep}];
handle_cmd_requests(CH, PV, CtxId, Cmds) ->
    do_handle_cmd_requests(CH, PV, CtxId, Cmds, []).

do_handle_cmd_requests(CH, PV, CtxId, [], CmdReplies) ->
    lists:reverse(CmdReplies);
do_handle_cmd_requests(CH, PV, CtxId, [Cmd|Cmds], CmdReplies) ->
    CmdReply = handle_cmd_request(CH, PV, CtxId, Cmd),
    do_handle_cmd_requests(CH, PV, CtxId, Cmds, [CmdReply|CmdReplies]).

handle_cmd_request(CH, PV, CtxId, 
		       #'CommandRequest'{command = {Tag,Req}}) ->
    case Tag of
        notifyReq ->
            (catch handle_notify_req(CH,PV,CtxId,Req));

        serviceChangeReq ->
	    ED =  cre_error_descr(?megaco_not_implemented,
				  "Service change only allowed "
				  "on null context handled"),
	    throw(ED);

        _ ->
            Code = ?megaco_not_implemented,
            ED   = cre_error_descr(Code,"Unknown command requst received:"
                                   "~n   Tag: ~p~n   Req: ~p",[Tag,Req]),
            throw(ED)
    end.

handle_notify_req(CH, PV, CtxId, 
		      #'NotifyRequest'{terminationID            = [Tid],
				       observedEventsDescriptor = EvDesc}) ->
    handle_event(CH, PV, CtxId, Tid, EvDesc).

handle_event(CH, PV, Cid, Tid, EvDesc) ->
    d("handle_event -> received"
      "~n   EvDesc: ~p"
      "~n   Tid:    ~p", [EvDesc, Tid]),
    {notifyReply, cre_notifyRep(Tid)}.
    

service_change(CH, PV, SCR) ->
    SCP = SCR#'ServiceChangeRequest'.serviceChangeParms,
    #'ServiceChangeParm'{serviceChangeMethod  = Method,
                         serviceChangeAddress = Address,
                         serviceChangeProfile = Profile,
                         serviceChangeReason  = [Reason],
                         serviceChangeDelay   = Delay,
                         serviceChangeMgcId   = MgcId} = SCP,
    TermId = SCR#'ServiceChangeRequest'.terminationID,
    if
        TermId == [?megaco_root_termination_id] ->
            MyMid = CH#megaco_conn_handle.local_mid,
            Res = {serviceChangeResParms,
		   cre_serviceChangeResParms(MyMid, Address, Profile)},
            cre_serviceChangeReply(TermId, Res);
        true ->
            Res = {errorDescriptor,
                   cre_error_descr(?megaco_not_implemented,
				   "Only handled for root")},
            cre_serviceChangeReply(TermId, Res)
    end.

    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 
cre_serviceChangeReply(TermId, Result) ->
    #'ServiceChangeReply'{terminationID       = TermId,
			  serviceChangeResult = Result}.

cre_serviceChangeResParms(Mid, Addr, Prof) ->
    #'ServiceChangeResParm'{serviceChangeMgcId   = Mid,
			    serviceChangeAddress = Addr,
			    serviceChangeProfile = Prof}.


cre_notifyRep(Tid) ->
    #'NotifyReply'{terminationID = [Tid]}.

% cre_notifyRep(Tid,Err) ->
%     #'NotifyReply'{terminationID = [Tid], errorDescriptor = Err}.

cre_error_descr(Code,Text) ->
    #'ErrorDescriptor'{errorCode = Code, errorText = Text}.

cre_error_descr(Code,FormatString,Args) ->
    Text = lists:flatten(io_lib:format(FormatString,Args)),
    cre_error_descr(Code,Text).


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
	{reply, {delay_reply, To, Reply}, Pid} ->
	    sleep(To),
	    Reply;
	{reply, {exit, To, Reason}, Pid} ->
	    sleep(To),
	    exit(Reason);
	{reply, Reply, Pid} ->
	    Reply
    end.


reply(To, Reply) ->
    To ! {reply, Reply, self()}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sleep(X) ->
    d("sleep -> ~w", [X]),
    receive after X -> ok end.


error_msg(F,A) -> error_logger:error_msg("MGC: " ++ F ++ "~n",A).


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
    io:format("*** [~s] ~s ~p ~s ***"
	      "~n   " ++ F ++ "~n~n", 
	      [format_timestamp(now()), P, self(), get(sname) | A]);
print(_, _, _, _) ->
    ok.


format_timestamp(Now) ->
    {N1, N2, N3} = Now,
    {Date, Time}   = calendar:now_to_datetime(Now),
    {YYYY,MM,DD}   = Date,
    {Hour,Min,Sec} = Time,
    FormatDate = 
        io_lib:format("~.4w:~.2.0w:~.2.0w ~.2.0w:~.2.0w:~.2.0w 4~w",
                      [YYYY,MM,DD,Hour,Min,Sec,round(N3/1000)]),  
    lists:flatten(FormatDate).

