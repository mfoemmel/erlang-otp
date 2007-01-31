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
%% Purpose: Send and process a (sequence of) Megaco/H.248 transactions
%%----------------------------------------------------------------------

-module(megaco_messenger).

%% Application internal export
-export([
         process_received_message/4,
         receive_message/4,
         connect/4,
         disconnect/2,
         encode_actions/3, 
         call/3,
         cast/3,
         cancel/2,
         request_timeout/2,
         pending_timeout/3,
         reply_timeout/3,

	 test_request/5,
	 test_reply/5
        ]).

%% MIB stat functions
-export([
	 get_stats/0, get_stats/1, get_stats/2,
	 reset_stats/0, reset_stats/1
	]).

%% Misc functions
-export([
	 cleanup/2, 
	 which_requests/1, which_replies/1
	]).

%% Module internal export
-export([
	 process_received_message/5,
         handle_request/1,
         handle_long_request/1,
         connect_remote/3,
         disconnect_local/2,
         disconnect_remote/3,
         send_request_remote/4,
         receive_reply_remote/2
        ]).

-include_lib("megaco/include/megaco.hrl").
-include("megaco_message_internal.hrl").
-include("megaco_internal.hrl").

%% N.B. Update cancel/1 with '_' when a new field is added
-record(request,
        {trans_id,
	 remote_mid,
         timer_ref,  % {short, Ref} | {long, Ref}
         init_timer,
         init_long_timer,
         curr_timer,
         version,
         bytes,     % {send, Data} | {no_send, Data}, Data = binary() | tuple()
         send_handle,
         user_mod,
         user_args,
         reply_action, % call | cast
         reply_data
        }).


%% N.B. Update cancel/1 with '_' when a new field is added
-record(reply,
        {
          trans_id,
	  local_mid,
          state = prepare,     % prepare | eval_request | waiting_for_ack | aborted
          pending_timer_ref,
	  handler = undefined, % pid of the proc executing the callback func
          timer_ref,
          version,
          bytes,
          ack_action,          % discard_ack | {handle_ack, Data}
	  send_handle
         }).

-record(trans_id,
        {
          mid,
          serial
         }).


-ifdef(MEGACO_TEST_CODE).
-define(SIM(Other,Where),
	fun(Afun,Bfun) ->
		Kfun = {?MODULE,Bfun},
		case (catch ets:lookup(megaco_test_data, Kfun)) of
		    [{Kfun,Cfun}] ->
			Cfun(Afun);
		    _ ->
			Afun
		end
	end(Other,Where)).
-define(TC_AWAIT_CANCEL_EVENT(),
	case megaco_tc_controller:lookup(block_on_cancel) of
	    {value, {Tag, Pid}} when is_pid(Pid) ->
		Pid ! {Tag, self()},
		receive
		    {Tag, Pid} ->
			ok
		end;
	    {value, {sleep, To}} when is_integer(To) and (To > 0) ->
		receive after To -> ok end;
	    _ ->
		ok
	end).
-define(TC_AWAIT_REPLY_EVENT(Info),
	case megaco_tc_controller:lookup(block_on_reply) of
	    {value, {Tag, Pid}} when is_pid(Pid) ->
		Pid ! {Tag, self(), Info},
		receive
		    {Tag, Pid} ->
			ok
		end;
	    _ ->
		ok
	end).
-else.
-define(SIM(Other,Where),Other).
-define(TC_AWAIT_CANCEL_EVENT(),ok).
-define(TC_AWAIT_REPLY_EVENT(_),ok).
-endif.


-define(report_pending_limit_exceeded(ConnData),
	?report_important(ConnData, "<ERROR> pending limit exceeded", [])).

-ifdef(megaco_extended_trace).
-define(rt1(T,F,A),?report_trace(T,F,A)).
-define(rt2(F,A),  ?rt1(ignore,F,A)).
-define(rt3(F),    ?rt2(F,[])).
-else.
-define(rt1(T,F,A),ok).
-define(rt2(F,A),  ok).
-define(rt3(F),    ok).
-endif.


%%----------------------------------------------------------------------
%% SNMP statistics handling functions
%%----------------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: get_stats/0, get_stats/1, get_stats/2
%% Description: Retreive statistics (counters) for TCP
%%-----------------------------------------------------------------

get_stats() ->
    megaco_stats:get_stats(megaco_stats).

get_stats(ConnHandleOrCounter) ->
    megaco_stats:get_stats(megaco_stats, ConnHandleOrCounter).

get_stats(ConnHandle, Counter) ->
    megaco_stats:get_stats(megaco_stats, ConnHandle, Counter).


%%-----------------------------------------------------------------
%% Func: reset_stats/0, reaet_stats/1
%% Description: Reset statistics (counters)
%%-----------------------------------------------------------------

reset_stats() ->
    megaco_stats:reset_stats(megaco_stats).

reset_stats(ConnHandleOrCounter) ->
    megaco_stats:reset_stats(megaco_stats, ConnHandleOrCounter).



%%----------------------------------------------------------------------
%% cleanup utility functions
%%----------------------------------------------------------------------

cleanup(#megaco_conn_handle{local_mid = LocalMid}, Force) 
  when (Force == true) or (Force == false) ->
    Pat = #reply{trans_id  = '$1', 
		 local_mid = LocalMid, 
		 state     = '$2',
		 _         = '_'},
    do_cleanup(Pat, Force);
cleanup(LocalMid, Force) 
  when (Force == true) or (Force == false) ->
    Pat = #reply{trans_id  = '$1', 
		 local_mid = LocalMid, 
		 state     = '$2',
		 _         = '_'},
    do_cleanup(Pat, Force).
    
do_cleanup(Pat, Force) ->
    Match = megaco_monitor:which_replies(Pat),
    Reps  = [{V1, V2} || [V1, V2] <- Match],
    do_cleanup2(Reps, Force).

do_cleanup2([], _) ->
    ok;
do_cleanup2([{TransId, aborted}|T], Force = false) ->
    megaco_monitor:delete_reply(TransId),
    do_cleanup2(T, Force);
do_cleanup2([_|T], Force = false) ->
    do_cleanup2(T, Force);
do_cleanup2([{TransId, _State}|T], Force = true) ->
    megaco_monitor:delete_reply(TransId),
    do_cleanup2(T, Force).
		  

%%----------------------------------------------------------------------
%% which_requests and which_replies utility functions
%%----------------------------------------------------------------------

which_requests(#megaco_conn_handle{local_mid  = LocalMid, 
				   remote_mid = RemoteMid}) ->
    Pat1 = #trans_id{mid    = LocalMid, 
		     serial = '$1', _ = '_'},
    Pat2 = #request{trans_id   = Pat1, 
		    remote_mid = RemoteMid, 
		    _ = '_'},
    Match = megaco_monitor:which_requests(Pat2),
    [S || [S] <- Match];
which_requests(LocalMid) ->
    Pat1 = #trans_id{mid    = LocalMid, 
		     serial = '$1', _ = '_'},
    Pat2 = #request{trans_id   = Pat1, 
		    remote_mid = '$2', _ = '_'},
    Match0 = megaco_monitor:which_requests(Pat2),
    Match1 = [{mk_ch(LocalMid, V2), V1} || [V1, V2] <- Match0],
    which_requests1(lists:sort(Match1)).

which_requests1([]) ->
    [];
which_requests1([{CH, S}|T]) ->
    which_requests2(T, CH, [S], []).

which_requests2([], CH, Serials, Reqs) ->
    lists:reverse([{CH, Serials}|Reqs]);
which_requests2([{CH, S}|T], CH, Serials, Reqs) ->
    which_requests2(T, CH, [S|Serials], Reqs);
which_requests2([{CH1, S}|T], CH2, Serials, Reqs) ->
    which_requests2(T, CH1, [S], [{CH2, lists:reverse(Serials)}| Reqs]).
    

which_replies(#megaco_conn_handle{local_mid  = LocalMid, 
				  remote_mid = RemoteMid}) ->
    Pat1 = #trans_id{mid    = RemoteMid, 
		     serial = '$1', _ = '_'},
    Pat2 = #reply{trans_id  = Pat1, 
  		  local_mid = LocalMid, 
  		  state     = '$2', 
  		  handler   = '$3', _ = '_'},
    Match = megaco_monitor:which_replies(Pat2),
    [{V1, V2, V3} || [V1, V2, V3] <- Match];
which_replies(LocalMid) ->
    Pat1 = #trans_id{mid    = '$1', 
		     serial = '$2', _ = '_'},
    Pat2 = #reply{trans_id  = Pat1, 
		  local_mid = LocalMid, 
		  state     = '$3', 
		  handler   = '$4', _ = '_'},
    Match0 = megaco_monitor:which_replies(Pat2),
    Match1 = [{mk_ch(LocalMid,V1),{V2,V3,V4}} || [V1, V2, V3, V4] <- Match0],
    which_replies1(lists:sort(Match1)).

which_replies1([]) ->
    [];
which_replies1([{CH, Data}|T]) ->
    which_replies2(T, CH, [Data], []).

which_replies2([], CH, Data, Reps) ->
    lists:reverse([{CH, Data}|Reps]);
which_replies2([{CH, Data}|T], CH, Datas, Reps) ->
    which_replies2(T, CH, [Data|Datas], Reps);
which_replies2([{CH1, Data}|T], CH2, Datas, Reps) ->
    which_replies2(T, CH1, [Data], [{CH2, lists:reverse(Datas)}| Reps]).
    
    
mk_ch(LM, RM) ->
    #megaco_conn_handle{local_mid = LM, remote_mid = RM}.
    

%%----------------------------------------------------------------------
%% Register/unreister connections
%%----------------------------------------------------------------------

%% Returns {ok, ConnHandle} | {error, Reason}
connect(RH, RemoteMid, SendHandle, ControlPid)
  when is_record(RH, megaco_receive_handle) ->
    case megaco_config:connect(RH, RemoteMid, SendHandle, ControlPid) of
        {ok, ConnData} ->
            do_connect(ConnData);
        {error, Reason} ->
            {error, Reason}
    end;
connect(BadHandle, _CH, _SendHandle, _ControlPid) ->
    {error, {bad_receive_handle, BadHandle}}.

do_connect(CD) ->
    CH       = CD#conn_data.conn_handle,
    Version  = CD#conn_data.protocol_version,
    UserMod  = CD#conn_data.user_mod,
    UserArgs = CD#conn_data.user_args,
    ?report_trace(CD, "callback: connect", []),
    Res = (catch apply(UserMod, handle_connect, [CH, Version | UserArgs])),
    ?report_debug(CD, "return: connect", [{return, Res}]),
    case Res of
        ok ->
	    ?SIM(ok, do_encode),
	    monitor_process(CH, CD#conn_data.control_pid);
        error ->
            megaco_config:disconnect(CH),
            {error, {connection_refused, CD, error}};
        {error, ED} when is_record(ED,'ErrorDescriptor') ->
            megaco_config:disconnect(CH),
            {error, {connection_refused, CD, ED}};
        _Error ->
	    warning_msg("connect callback failed: ~w", [Res]),
            megaco_config:disconnect(CH),
            {error, {connection_refused, CD, Res}}
    end.


monitor_process(CH, ControlPid) when node(ControlPid) == node() ->
    M = ?MODULE,
    F = disconnect_local,
    A = [CH],
    Ref = megaco_monitor:apply_at_exit(M, F, A, ControlPid),
    case megaco_config:update_conn_info(CH, monitor_ref, Ref) of
        ok ->
            ?SIM({ok, CH}, monitor_process_local);
        {error, Reason} ->
            disconnect(CH, {config_update, Reason}),
            {error, Reason}
    end;
monitor_process(CH, ControlPid) when node(ControlPid) /= node() ->
    RemoteNode = node(ControlPid),
    UserMonitorPid = whereis(megaco_monitor),
    Args = [CH, ControlPid, UserMonitorPid],
    case rpc:call(RemoteNode, ?MODULE, connect_remote, Args) of
        {ok, ControlMonitorPid} ->
            M = ?MODULE,
            F = disconnect_local,
            A = [CH],
            Ref = megaco_monitor:apply_at_exit(M, F, A, ControlMonitorPid),
            case megaco_config:update_conn_info(CH, monitor_ref, Ref) of
                ok ->
		    ?SIM({ok, CH}, monitor_process_remote);
                {error, Reason} ->
                    disconnect(CH, {config_update, Reason}),
                    {error, Reason}
            end;
        {error, Reason} ->
            disconnect(CH, {connect_remote, Reason}),
            {error, Reason};
        {badrpc, Reason} ->
            Reason2 = {'EXIT', Reason},
            disconnect(CH, {connect_remote, Reason2}),
            {error, Reason2}
    end.

connect_remote(CH, ControlPid, UserMonitorPid)
  when node(ControlPid) == node(), node(UserMonitorPid) /= node() ->
    case megaco_config:lookup_local_conn(CH) of
        [_ConnData] -> 
            UserNode = node(UserMonitorPid),
            M = ?MODULE,
            F = disconnect_remote,
            A = [CH, UserNode],
            Ref = megaco_monitor:apply_at_exit(M, F, A, UserMonitorPid),
            case megaco_config:connect_remote(CH, UserNode, Ref) of
		ok ->
		    ControlMonitorPid = whereis(megaco_monitor),
		    ?SIM({ok, ControlMonitorPid}, connect_remote);
		{error, Reason} ->
		    {error, Reason}
	    end;
        [] ->
            {error, {no_connection, CH}}
    end.

disconnect(ConnHandle, DiscoReason)
  when is_record(ConnHandle, megaco_conn_handle) ->
    case megaco_config:disconnect(ConnHandle) of
        {ok, ConnData, RemoteConnData} ->
            ControlRef = ConnData#conn_data.monitor_ref,
            megaco_monitor:cancel_apply_at_exit(ControlRef),
            handle_disconnect_callback(ConnData, DiscoReason),
            ControlNode = node(ConnData#conn_data.control_pid),
            case ControlNode == node() of
                true ->
                    %% Propagate to remote users
                    CancelFun =
                        fun(RCD) ->
                                UserRef = RCD#remote_conn_data.monitor_ref,
                                megaco_monitor:cancel_apply_at_exit(UserRef),
                                RCD#remote_conn_data.user_node
                          end,
                    Nodes = lists:map(CancelFun, RemoteConnData),
		    %% io:format("NODES: ~p~n", [Nodes]),
                    M = ?MODULE,
                    F = disconnect,
                    A = [ConnHandle, DiscoReason],
                    case rpc:multicall(Nodes, M, F, A) of
                        {Res, []} ->
			    Check = fun(ok) -> false;
				       ({error, {no_connection, _CH}}) -> false;
				       (_) -> true
				    end,
                            case lists:filter(Check, Res) of
                                [] ->
                                    ok;
                                Bad ->
                                    {error, {remote_disconnect_error, ConnHandle, Bad}}
                            end;
                        {_Res, Bad} ->
                            {error, {remote_disconnect_crash, ConnHandle, Bad}}
                    end;
                false when RemoteConnData == [] ->
                    %% Propagate to remote control node
                    M = ?MODULE,
                    F = disconnect_remote,
                    A = [DiscoReason, ConnHandle, node()],
                    case rpc:call(ControlNode, M, F, A) of
                        {badrpc, Reason} ->
                            {error, {'EXIT', Reason}};
                        Other ->
                            Other
                    end
            end;
        {error, Reason} ->
            {error, Reason}
    end;
disconnect(BadHandle, Reason) ->
    {error, {bad_conn_handle, BadHandle, Reason}}.

disconnect_local(Reason, ConnHandle) ->
    disconnect(ConnHandle, {no_controlling_process, Reason}).

disconnect_remote(_Reason, ConnHandle, UserNode) ->
    case megaco_config:disconnect_remote(ConnHandle, UserNode) of
        [RCD] ->
            Ref = RCD#remote_conn_data.monitor_ref,
            megaco_monitor:cancel_apply_at_exit(Ref),
            ok;
        [] ->
            {error, {no_connection, ConnHandle}}
    end.


%%----------------------------------------------------------------------
%% Handle incoming message
%%----------------------------------------------------------------------

receive_message(ReceiveHandle, ControlPid, SendHandle, Bin) ->
    Opts = [link , {min_heap_size, 5000}],
    spawn_opt(?MODULE,
               process_received_message,
               [ReceiveHandle, ControlPid, SendHandle, Bin, self()], Opts),
    ok.

%% This function is called via the spawn_opt function with the link
%% option, therefor the unlink before the exit.
process_received_message(ReceiveHandle, ControlPid, SendHandle, Bin, Receiver) ->
    process_received_message(ReceiveHandle, ControlPid, SendHandle, Bin),
    unlink(Receiver),
    exit(normal).

process_received_message(ReceiveHandle, ControlPid, SendHandle, Bin) ->
    Flag = process_flag(trap_exit, true),
    case prepare_message(ReceiveHandle, SendHandle, Bin, ControlPid) of
        {ok, ConnData, MegaMsg} when is_record(MegaMsg, 'MegacoMessage') ->
	    ?rt1(ConnData, "message prepared", [MegaMsg]),
            Mess = MegaMsg#'MegacoMessage'.mess,
            case Mess#'Message'.messageBody of
                {transactions, Transactions} ->
                    {AckList, ReqList} = 
			prepare_trans(ConnData, Transactions, [], []),
                    handle_acks(AckList),
		    case ReqList of
			[] ->
			    ?rt3("no requests"),
			    ignore;
			[Req|Reqs] when (ConnData#conn_data.threaded == true) ->
			    [spawn(?MODULE,handle_request,[R]) || R <- Reqs],
			    handle_request(Req);
			_ ->
			    ?rt3("handle requests"),
			    case handle_requests(ReqList, []) of
				[] ->
				    ignore;
				[LongRequest | More] ->
				    [spawn(?MODULE, handle_long_request, [LR])
				     || LR <- More],
				    handle_long_request(LongRequest)
			    end
		    end;
                {messageError, Error} ->
                    handle_message_error(ConnData, Error)
            end;
        {silent_fail, ConnData, {_Code, Reason, Error}} ->
            ?report_debug(ConnData, Reason, [no_reply, Error]),
            ignore;
        {verbose_fail, ConnData, {Code, Reason, Error}} ->
            ?report_debug(ConnData, Reason, [Error]),
            send_message_error(ConnData, Code, Reason)
    end,
    process_flag(trap_exit, Flag),
    ok.

prepare_message(RH, SH, Bin, Pid)
  when is_record(RH, megaco_receive_handle) and is_pid(Pid) ->
    ?report_trace(RH, "receive bytes", [{bytes, Bin}]),
    EncodingMod    = RH#megaco_receive_handle.encoding_mod,
    EncodingConfig = RH#megaco_receive_handle.encoding_config,
    ProtVersion    = RH#megaco_receive_handle.protocol_version,
    case (catch EncodingMod:decode_message(EncodingConfig, ProtVersion, Bin)) of
        {ok, MegaMsg} when is_record(MegaMsg, 'MegacoMessage') ->
	    ?report_trace(RH, "receive message", [{message, MegaMsg}]),
            Mess       = MegaMsg#'MegacoMessage'.mess,
            RemoteMid  = Mess#'Message'.mId,
            Version    = Mess#'Message'.version,
            LocalMid   = RH#megaco_receive_handle.local_mid,
            CH         = #megaco_conn_handle{local_mid  = LocalMid,
                                             remote_mid = RemoteMid},
            case megaco_config:lookup_local_conn(CH) of
		%% 
		%% Message is not of the negotiated version
		%% 

                [#conn_data{protocol_version = NegVersion, 
			    strict_version   = true} = ConnData] 
		when NegVersion /= Version ->
		    %% Use already established connection, 
		    %% but incorrect version
		    ?rt1(ConnData, "not negotiated version", [Version]),
		    Error = {error, {not_negotiated_version, 
				     NegVersion, Version}}, 
		    handle_syntax_error_callback(RH, ConnData, 
						 prepare_error(Error));


                [ConnData] ->
                    %% Use already established connection
		    ?rt1(ConnData, "use already established connection", []),
                    ConnData2 = ConnData#conn_data{send_handle      = SH,
						   protocol_version = Version},
                    check_message_auth(CH, ConnData2, MegaMsg, Bin);


                [] ->
                    %% Setup a temporary connection
		    ?rt3("setup a temporary connection"),
                    case connect(RH, RemoteMid, SH, Pid) of
                        {ok, _} ->
			    do_prepare_message(RH, CH, SH, MegaMsg, Pid, Bin);
			{error, {already_connected, _ConnHandle}} ->
			    do_prepare_message(RH, CH, SH, MegaMsg, Pid, Bin);
			{error, {connection_refused, ConnData, Reason}} ->
			    Error = prepare_error({error, {connection_refused, Reason}}),
                            {verbose_fail, ConnData, Error};
                        {error, Reason} ->
                            ConnData = fake_conn_data(RH, RemoteMid, SH, Pid),
			    ConnData2 = ConnData#conn_data{protocol_version = Version},
			    Error    = prepare_error({error, Reason}),
                            {verbose_fail, ConnData2, Error}
                    end
            end;
        Error ->
	    ?rt2("decode error", [Error]),
	    ConnData = handle_decode_error(Error, 
					   RH, SH, Bin, Pid, 
					   EncodingMod, 
					   EncodingConfig, 
					   ProtVersion),
            handle_syntax_error_callback(RH, ConnData, prepare_error(Error))
    end;
prepare_message(RH, SendHandle, _Bin, ControlPid) ->
    ConnData = fake_conn_data(RH, SendHandle, ControlPid),
    Error    = prepare_error({'EXIT', {bad_receive_handle, RH}}),
    {verbose_fail, ConnData, Error}.


handle_decode_error({error, {unsupported_version, _}},
		    #megaco_receive_handle{local_mid = LocalMid} = RH, SH, 
		    Bin, Pid,
		    EM, EC, V) ->
    case (catch EM:decode_mini_message(EC, V, Bin)) of
	{ok, #'MegacoMessage'{mess = #'Message'{version = _Ver, mId = RemoteMid}}} ->
	    ?rt2("erroneous message received", [SH, RemoteMid, _Ver]),
            CH = #megaco_conn_handle{local_mid  = LocalMid,
				     remote_mid = RemoteMid},
	    incNumErrors(CH),
	    %% We cannot put the version into conn-data, that will
	    %% make the resulting error message impossible to sent
	    %% (unsupported version)
	    case megaco_config:lookup_local_conn(CH) of
                [ConnData] ->
		    ?rt3("known to us"),
		    ConnData#conn_data{send_handle = SH};
		[] ->
		    ?rt3("unknown to us"),
		    ConnData = fake_conn_data(RH, SH, Pid),
		    ConnData#conn_data{conn_handle = CH}
	    end;

	_ ->
	    ?rt2("erroneous message received", [SH]),
	    incNumErrors(),
	    fake_conn_data(RH, SH, Pid)
    end;

handle_decode_error(_,
		    #megaco_receive_handle{local_mid = LocalMid} = RH, SH, 
		    Bin, Pid,
		    EM, EC, V) ->
    case (catch EM:decode_mini_message(EC, V, Bin)) of
	{ok, #'MegacoMessage'{mess = #'Message'{version = Ver, mId = RemoteMid}}} ->
	    ?rt2("erroneous message received", [SH, Ver, RemoteMid]),
            CH = #megaco_conn_handle{local_mid  = LocalMid,
				     remote_mid = RemoteMid},
	    incNumErrors(CH),
	    case megaco_config:lookup_local_conn(CH) of
                [ConnData] ->
		    ?rt3("known to us"),
		    ConnData#conn_data{send_handle      = SH,
				       protocol_version = Ver};
		[] ->
		    ?rt3("unknown to us"),
		    ConnData = fake_conn_data(RH, SH, Pid),
		    ConnData#conn_data{conn_handle      = CH,
				       protocol_version = Ver}
	    end;

	_ ->
	    ?rt2("erroneous message received", [SH]),
	    incNumErrors(),
	    fake_conn_data(RH, SH, Pid)
    end.


do_prepare_message(RH, CH, SendHandle, MegaMsg, ControlPid, Bin) ->
    case megaco_config:lookup_local_conn(CH) of
	[ConnData] ->
	    case check_message_auth(CH, ConnData, MegaMsg, Bin) of
		{ok, ConnData2, MegaMsg} ->
		    %% Let the connection be permanent
		    {ok, ConnData2, MegaMsg};
		{ReplyTag, ConnData, Reason} ->
		    %% Remove the temporary connection
		    disconnect(CH, {bad_auth, Reason}),
		    {ReplyTag, ConnData, Reason}
	    end;
	[] ->
	    Reason = no_connection,
	    disconnect(CH, Reason),
	    RemoteMid = CH#megaco_conn_handle.remote_mid,
	    ConnData = fake_conn_data(RH, RemoteMid, SendHandle, ControlPid),
	    Error = prepare_error({error, Reason}),
	    {silent_fail, ConnData, Error}
    end.

check_message_auth(_ConnHandle, ConnData, MegaMsg, Bin) ->
    MsgAuth   = MegaMsg#'MegacoMessage'.authHeader,
    Mess      = MegaMsg#'MegacoMessage'.mess,
    Version   = Mess#'Message'.version,
    ConnData2 = ConnData#conn_data{protocol_version = Version},
    ConnAuth  = ConnData2#conn_data.auth_data,
    ?report_trace(ConnData2, "check message auth", [{bytes, Bin}]),
    if
	MsgAuth == asn1_NOVALUE, ConnAuth == asn1_NOVALUE ->
            ?SIM({ok, ConnData2, MegaMsg}, check_message_auth);
	true -> 
	    ED = #'ErrorDescriptor'{errorCode = ?megaco_unauthorized,
				    errorText = "Autentication is not supported"},
	    {verbose_fail, ConnData2, prepare_error({error, ED})}
    end.

handle_syntax_error_callback(ReceiveHandle, ConnData, PrepError) ->
    {Code, Reason, Error} = PrepError,
    ErrorDesc = #'ErrorDescriptor'{errorCode = Code, errorText = Reason},
    Version   = 
	case Error of
	    {error, {unsupported_version, UV}} ->
		UV;
	    _ ->
		ConnData#conn_data.protocol_version
	end,
    UserMod   = ConnData#conn_data.user_mod,
    UserArgs  = ConnData#conn_data.user_args,
    ?report_trace(ReceiveHandle, "callback: syntax error", [ErrorDesc, Error]),
    Res = (catch apply(UserMod, handle_syntax_error, [ReceiveHandle, Version, ErrorDesc | UserArgs])),
    ?report_debug(ReceiveHandle, "return: syntax error", [{return, Res}, ErrorDesc, Error]),
    case Res of
        reply ->
            {verbose_fail, ConnData, PrepError};
        {reply,#'ErrorDescriptor'{errorCode = Code1, errorText = Reason1}} ->
            {verbose_fail, ConnData, {Code1,Reason1,Error}};
        no_reply ->
            {silent_fail, ConnData, PrepError};
        {no_reply,#'ErrorDescriptor'{errorCode=Code2,errorText=Reason2}} ->
            {silent_fail, ConnData, {Code2,Reason2,Error}}; %%% OTP-????
        _ ->
	    warning_msg("syntax error callback failed: ~w", [Res]),
            {verbose_fail, ConnData, PrepError}
    end.

fake_conn_data(CH) when is_record(CH, megaco_conn_handle) ->
    case (catch megaco_config:conn_info(CH, receive_handle)) of
	RH when is_record(RH, megaco_receive_handle) ->
	    RemoteMid = CH#megaco_conn_handle.remote_mid,
	    ConnData = 
		fake_conn_data(RH, RemoteMid, no_send_handle, no_control_pid),
	    ConnData#conn_data{conn_handle = CH};
	{'EXIT', _} ->
	    UserMid = CH#megaco_conn_handle.local_mid,
	    case catch megaco_config:user_info(UserMid, receive_handle) of
		{'EXIT', _} -> % No such user
		    #conn_data{conn_handle        = CH,
			       serial             = undefined_serial,
			       control_pid        = no_control_pid,
			       monitor_ref        = undefined_monitor_ref,
			       send_mod           = no_send_mod,
			       send_handle        = no_send_handle,
			       encoding_mod       = no_encoding_mod,
			       encoding_config    = no_encoding_config,
			       reply_action       = undefined,
			       sent_pending_limit = infinity,
			       recv_pending_limit = infinity};
		RH ->
		    ConnData = 
			fake_conn_data(RH, no_send_handle, no_control_pid),
		    ConnData#conn_data{conn_handle = CH}
	    end
    end.

fake_conn_data(RH, SendHandle, ControlPid) ->
    fake_conn_data(RH, unknown_remote_mid, SendHandle, ControlPid).

fake_conn_data(RH, RemoteMid, SendHandle, ControlPid) ->
    case catch megaco_config:init_conn_data(RH, RemoteMid, SendHandle, ControlPid) of
	{'EXIT', _} -> % No such user
	    fake_user_data(RH, RemoteMid, SendHandle, ControlPid);
	ConnData ->
	    ConnData
    end.

fake_user_data(RH, RemoteMid, SendHandle, ControlPid) ->
    LocalMid = RH#megaco_receive_handle.local_mid,
    RH2 = RH#megaco_receive_handle{local_mid = default},
    case catch megaco_config:init_conn_data(RH2, RemoteMid, SendHandle, ControlPid) of
	{'EXIT', _} -> % Application stopped?
	    ConnHandle     = #megaco_conn_handle{local_mid  = LocalMid,
						 remote_mid = RemoteMid},
	    EncodingMod    = RH#megaco_receive_handle.encoding_mod,
	    EncodingConfig = RH#megaco_receive_handle.encoding_config,
	    SendMod        = RH#megaco_receive_handle.send_mod,
	    #conn_data{conn_handle        = ConnHandle,
		       serial             = undefined_serial,
		       control_pid        = ControlPid,
		       monitor_ref        = undefined_monitor_ref,
		       send_mod           = SendMod,
		       send_handle        = SendHandle,
		       encoding_mod       = EncodingMod,
		       encoding_config    = EncodingConfig,
		       reply_action       = undefined,
		       sent_pending_limit = infinity,
		       recv_pending_limit = infinity};
	ConnData ->
	    ConnData
    end.

prepare_error(Error) ->
    case Error of
        {error, ED} when is_record(ED, 'ErrorDescriptor') ->
            Code   = ED#'ErrorDescriptor'.errorCode,
            Reason = ED#'ErrorDescriptor'.errorText,
            {Code, Reason, Error};
        {error, [{reason, {bad_token, _}, Line}]} when is_integer(Line) ->
            Reason = lists:concat(["Illegal token on line ", Line]),
            Code = ?megaco_bad_request,
            {Code, Reason, Error};
        {error, [{reason, {Line, _, _}} | _]} when is_integer(Line) ->
            Reason = lists:concat(["Syntax error on line ", Line]),
            Code = ?megaco_bad_request,
            {Code, Reason, Error};
        {error, {connection_refused, ED}} when is_record(ED,'ErrorDescriptor') ->
            Code   = ED#'ErrorDescriptor'.errorCode,
            Reason = ED#'ErrorDescriptor'.errorText,
            {Code, Reason, Error};
        {error, {connection_refused, _}} ->
            Reason = "Connection refused by user",
            Code = ?megaco_unauthorized,
            {Code, Reason, Error};
        {error, {unsupported_version, V}} ->
            Reason = 
		lists:flatten(io_lib:format("Unsupported version: ~w",[V])),
            Code = ?megaco_version_not_supported, 
            {Code, Reason, Error};
        {error, {not_negotiated_version, NegV, MsgV}} ->
            Reason = 
		lists:flatten(
		  io_lib:format("Not negotiated version: ~w [negotiated ~w]",
				[MsgV, NegV])),
            Code = ?megaco_version_not_supported, 
            {Code, Reason, Error};
        {error, _} ->
            Reason = "Syntax error",
            Code = ?megaco_bad_request,
            {Code, Reason, Error};
        {ok, MegaMsg} when is_record (MegaMsg, 'MegacoMessage') ->
            Reason = "MID does not match config",
            Code = ?megaco_incorrect_identifier,
            {Code, Reason, Error};
        _ ->
            Reason = "Fatal syntax error",
            Code = ?megaco_internal_gateway_error,
            {Code, Reason, Error}
    end.

%% BUGBUG
%% Do we need something here, if we send more then one trans per message?
prepare_trans(ConnData, [Trans | Rest], AckList, ReqList) 
  when ConnData#conn_data.monitor_ref == undefined_monitor_ref ->
    %% May occur if another process already has setup a
    %% temporary connection, but the handle_connect callback
    %% function has not yet returned before the eager MG
    %% re-sends its initial service change message.
    case Trans of
        {transactionRequest, T} when is_record(T, 'TransactionRequest') ->
	    
            Serial = T#'TransactionRequest'.transactionId,
	    ConnData2 = ConnData#conn_data{serial = Serial},
	    ?report_trace(ConnData2, "Pending handle_connect", [T]),

	    %% ------------------------------------------
	    %% 
	    %%   Check pending limit
	    %% 
	    %% ------------------------------------------

	    Limit = ConnData#conn_data.sent_pending_limit,
	    TransId = to_remote_trans_id(ConnData2),
	    case check_and_maybe_incr_pending_limit(Limit, sent, TransId) of
		ok ->
		    send_pending(ConnData2);
		error ->
		    %% Pending limit:
		    %% In this (granted, highly hypothetical case)
		    %% we would make the user very confused if we 
		    %% called the abort callback function, since 
		    %% the request callback function has not yet
		    %% been called. Alas, we skip this call here.
		    send_pending_limit_error(ConnData);
		aborted ->
		    ignore
	    end,
	    prepare_trans(ConnData2, Rest, AckList, ReqList);
	_ ->
	    prepare_trans(ConnData, Rest, AckList, ReqList)
    end;
prepare_trans(ConnData, [Trans | Rest], AckList, ReqList) ->
    ?rt1(ConnData, "prepare trans", [Trans]),
    case Trans of
        {transactionRequest, #'TransactionRequest'{transactionId = asn1_NOVALUE}} ->
            ConnData2 = ConnData#conn_data{serial = 0},
	    Code   = ?megaco_bad_request,
            Reason = "Syntax error in message: transaction id missing",
	    send_trans_error(ConnData2, Code, Reason),
            prepare_trans(ConnData2, Rest, AckList, ReqList);
        {transactionRequest, T} when is_record(T, 'TransactionRequest') ->
            Serial = T#'TransactionRequest'.transactionId,
            ConnData2 = ConnData#conn_data{serial = Serial},
            prepare_request(ConnData2, T, Rest, AckList, ReqList);
        {transactionPending, T} when is_record(T, 'TransactionPending') ->
            Serial = T#'TransactionPending'.transactionId,
            ConnData2 = ConnData#conn_data{serial = Serial},
            handle_pending(ConnData2, T),
            prepare_trans(ConnData2, Rest, AckList, ReqList);
        {transactionReply, T} when is_record(T, 'TransactionReply') ->
            Serial = T#'TransactionReply'.transactionId,
            ConnData2 = ConnData#conn_data{serial = Serial},
            handle_reply(ConnData2, T),
	    prepare_trans(ConnData2, Rest, AckList, ReqList);
        {transactionResponseAck, List} when is_list(List) ->
            prepare_ack(ConnData, List, Rest, AckList, ReqList)

    end;
prepare_trans(_ConnData, [], AckList, ReqList) ->
    ?SIM({AckList, ReqList}, prepare_trans_done).

prepare_request(ConnData, T, Rest, AckList, ReqList) ->
    ?rt2("prepare request", [T]),
    LocalMid = (ConnData#conn_data.conn_handle)#megaco_conn_handle.local_mid,
    TransId = to_remote_trans_id(ConnData),
    ?rt2("prepare request", [LocalMid, TransId]),
    case megaco_monitor:lookup_reply(TransId) of
        [] ->
	    ?rt3("brand new request"),

            %% Brand new request

	    %% Check pending limit:
	    %% 
	    %% We should actually check the pending limit here
	    %% but since we have to do it later in the 
	    %% handle_request function (just before we call
	    %% the handle_trans_request callback function) we
	    %% can just as well wait (this is after all a very
	    %% unlikely case: see function prepare_trans when 
	    %% monitor_ref == undefined_monitor_ref).
	    %% 

	    #conn_data{send_handle      = SendHandle,
		       pending_timer    = InitTimer,
		       protocol_version = Version} = ConnData,
	    {WaitFor, CurrTimer} = init_timer(InitTimer),
	    M = ?MODULE,
	    F = pending_timeout,
	    A = [ConnData, TransId, CurrTimer],	    
	    PendingRef = megaco_monitor:apply_after(M, F, A, WaitFor),
            Rep = #reply{send_handle       = SendHandle,
			 trans_id          = TransId,
			 local_mid         = LocalMid,
			 pending_timer_ref = PendingRef,
			 handler           = self(),
			 version           = Version},
            megaco_monitor:insert_reply(Rep),
            prepare_trans(ConnData, Rest, AckList, 
			  [{ConnData, TransId, T} | ReqList]);

        [#reply{state             = State, 
		handler           = Pid,
		pending_timer_ref = Ref} = Rep] 
	when (State == prepare) or (State == eval_request) ->

	    ?rt2("request resend", [State, Pid, Ref]),

            %% Pending limit:
	    %% We are still preparing/evaluating the request
            %% Check if the pending limit has been exceeded...
	    %% If the pending limit is _not_ exceeded then
	    %% we shall send a pending (and actually restart 
	    %% the pending timer, but that we cannot do).
	    %% Don't care about Msg and Rep version diff

	    %% ?report_trace(ignore, "still preparing/evaluating request", []),

	    #conn_data{sent_pending_limit = Limit} = ConnData,
	    
	    case check_and_maybe_incr_pending_limit(Limit, sent, TransId) of
		ok ->

		    %% ------------------------------------------
		    %% 
		    %%   Pending limit not exceeded
		    %% 
		    %%   1) Increment number of pendings sent
		    %%      (done in the check function above)
		    %%   2) Send pending message
		    %%      (We should really restart the pending 
		    %%      timer, but we have no way of doing that).
		    %% 
		    %% ------------------------------------------

		    send_pending(ConnData),
		    prepare_trans(ConnData, Rest, AckList, ReqList);


		error ->

		    %% -------------------------------------------
		    %% 
		    %%   Pending limit exceeded
		    %% 
		    %%   1) Cancel pending timer
		    %%   2) Send 506 error message to other side
		    %%   3) Inform user (depends on state)
		    %%   4) Set reply in aborted state
		    %% 
		    %% -------------------------------------------

		    %% 
		    %% State == eval_request:
		    %%   This means that the request is currently beeing 
		    %%   evaluated by the user, and the reply timer has 
		    %%   not yet been started. 
		    %%   Either:
		    %%   a) The "other side" will resend (which will 
		    %%      trigger a pending message send) until we pass the 
		    %%      pending limit 
		    %%   b) We will send pending messages (when the pending 
		    %%      timer expire) until we pass the pending limit.
		    %%   In any event, we cannot delete the reply record
		    %%   or the pending counter in this case. Is there
		    %%   a risk we accumulate aborted reply records?
		    %% 
		    %% State == prepare:
		    %%   The user does not know about this request
		    %%   so we can safely perform cleanup.
		    %% 
		    megaco_monitor:cancel_apply_after(Ref),
		    send_pending_limit_error(ConnData),
		    if 
			State == eval_request ->
			    %% 
			    %% What if the user never replies?
			    %% In that case we will have a record
			    %% (and counters) that is never cleaned up...
			    Rep2 = Rep#reply{state             = aborted,
					     pending_timer_ref = undefined},
			    megaco_monitor:insert_reply(Rep2),
			    handle_request_abort_callback(ConnData, 
							  TransId, Pid);
			true ->
			    %% Since the user does not know about
			    %% this call yet, it is safe to cleanup.
			    %% Should we inform?
			    Rep2 = Rep#reply{state = aborted},
			    cancel_reply(ConnData, Rep2, aborted),
			    ok
		    end,
		    prepare_trans(ConnData, Rest, AckList, ReqList);


		aborted ->

		    %% -------------------------------------------
		    %% 
		    %%   Pending limit already exceeded
		    %% 
		    %%   Cleanup, just to make sure:
		    %%     reply record & pending counter
		    %% 
		    %% -------------------------------------------

		    Rep2 = Rep#reply{state = aborted},
		    cancel_reply(ConnData, Rep2, aborted),
		    prepare_trans(ConnData, Rest, AckList, ReqList)

	    end;

        [#reply{state   = waiting_for_ack, 
		bytes   = Bin, 
		version = Version} = Rep] ->
	    ?rt3("request resend when waiting for ack"),

            %% We have already sent a reply, but the receiver
            %% has obviously not got it. Resend the reply but
            %% don't restart the reply_timer.
            ConnData2 = ConnData#conn_data{protocol_version = Version},
            ?report_trace(ConnData2, 
			  "re-send trans reply", [T | {bytes, Bin}]),
            case megaco_messenger_misc:send_message(ConnData2, Bin) of
		ok ->
		    ok;
		{ok, _} ->
		    ok;
		{error, Reason} ->
		    %% Pass it on to the user (via handle_ack)
		    cancel_reply(ConnData2, Rep, Reason)
	    end,
	    prepare_trans(ConnData2, Rest, AckList, ReqList);

	[#reply{state = aborted} = Rep] ->
	    ?rt3("request resend when already in aborted state"),

	    %% OTP-4956:
	    %% Already aborted so ignore.
	    %% This furthermore means that the abnoxious user at the
	    %% other end has already been informed (pending-limit
	    %% passed => error descriptor sent), but keeps sending...
	    %% 
	    %% Shall we perform a cleanup?
	    cancel_reply(ConnData, Rep, aborted),
	    prepare_trans(ConnData, Rest, AckList, ReqList)
        end.

prepare_ack(ConnData, [TA | T], Rest, AckList, ReqList) 
  when is_record(TA, 'TransactionAck') ->
    First     = TA#'TransactionAck'.firstAck,
    Last      = TA#'TransactionAck'.lastAck,
    TA2       = TA#'TransactionAck'{lastAck = asn1_NOVALUE},
    ConnData2 = ConnData#conn_data{serial = First},
    AckList2  = do_prepare_ack(ConnData2, TA2, AckList),
    if
        Last == asn1_NOVALUE ->
            prepare_ack(ConnData, T, Rest, AckList2, ReqList);
        First < Last ->
            TA3 = TA#'TransactionAck'{firstAck = First + 1},
            prepare_ack(ConnData, [TA3 | T], Rest, AckList2, ReqList);
        First == Last ->
            prepare_ack(ConnData, T, Rest, AckList2, ReqList);
        First > Last ->
            %% Protocol violation from the sender of this ack
            ?report_important(ConnData, "<ERROR> discard trans",
			      [TA, {error, "firstAck > lastAck"}]),
	    prepare_ack(ConnData, T, Rest, AckList2, ReqList)
    end;
prepare_ack(ConnData, [], Rest, AckList, ReqList) ->
    prepare_trans(ConnData, Rest, AckList, ReqList).

do_prepare_ack(ConnData, T, AckList) ->
    TransId = to_remote_trans_id(ConnData),
    case megaco_monitor:lookup_reply(TransId) of
        [] ->
            %% The reply has already been garbage collected. Ignore.
            ?report_trace(ConnData, "discard ack (no receiver)", [T]),
            AckList;
        [Rep] when Rep#reply.state == waiting_for_ack ->
            %% Don't care about Msg and Rep version diff
            [{ConnData, Rep, T} | AckList];
        [_Rep] ->
            %% Protocol violation from the sender of this ack
            ?report_important(ConnData, "<ERROR> discard trans",
			      [T, {error, "got ack before reply was sent"}]),
            AckList
    end.


check_and_maybe_create_pending_limit(infinity, _, _) ->
    ok;
check_and_maybe_create_pending_limit(Limit, Direction, TransId) ->
    case (catch megaco_config:get_pending_counter(Direction, TransId)) of
	{'EXIT', _} ->
	    %% Has not been created yet (connect).
	    megaco_config:cre_pending_counter(Direction, TransId, 0),
	    ok;
	Val when Val =< Limit ->
	    %% Since we have no intention to increment here, it
	    %% is ok to be _at_ the limit
	    ok;
	_ ->
	    aborted
    end.

check_pending_limit(infinity, _, _) ->
    {ok, 0};
check_pending_limit(Limit, Direction, TransId) ->
    ?rt2("check pending limit", [Direction, Limit, TransId]),
    case (catch megaco_config:get_pending_counter(Direction, TransId)) of
	{'EXIT', _} ->
	    %% This function is only called when we "know" the 
	    %% counter to exist. So, the only reason that this 
	    %% would happen is of the counter has been removed.
	    %% This only happen if the pending limit has been 
	    %% reached. In any case, this is basically the same 
	    %% as aborted!
	    ?rt2("check pending limit - exit", []),
	    aborted;
	Val when Val =< Limit ->
	    %% Since we have no intention to increment here, it
	    %% is ok to be _at_ the limit
	    ?rt2("check pending limit - ok", [Val]),
	    {ok, Val};
	_Val ->
	    ?rt2("check pending limit - aborted", [_Val]),
	    aborted
    end.


check_and_maybe_incr_pending_limit(infinity, _, _) ->
    ok;
check_and_maybe_incr_pending_limit(Limit, Direction, TransId) ->
    %% 
    %% We need this kind of test to detect when we _pass_ the limit
    %% 
    ?rt2("check and maybe incr pending limit", [Direction, Limit, TransId]),
    case (catch megaco_config:get_pending_counter(Direction, TransId)) of
	{'EXIT', _} ->
	    %% Has not been created yet (connect).
	    megaco_config:cre_pending_counter(Direction, TransId, 1),
	    ok;
	Val when Val > Limit ->
	    ?rt2("check and maybe incr - aborted", [Direction, Val, Limit]),
	    aborted;      % Already passed the limit
	Val ->
	    ?rt2("check and maybe incr - incr", [Direction, Val, Limit]),
	    megaco_config:incr_pending_counter(Direction, TransId),
	    if 
		Val < Limit ->
		    ok;   % Still within the limit
		true ->
		    ?rt2("check and maybe incr - error", 
			 [Direction, Val, Limit]),
		    error % Passed the limit
	    end
    end.


%% BUGBUG BUGBUG BUGBUG
%% 
%% Do we know that the Rep is still valid? A previous transaction
%% could have taken a lot of time.
%% 
handle_request({ConnData, TransId, T}) ->
    case handle_request(ConnData, TransId, T) of
	{pending, _RequestData} ->
	    handle_long_request(ConnData, TransId, T);
	Else ->
	    Else
    end.

handle_request(ConnData, TransId, T) ->
    ?report_trace(ConnData, "handle request", [TransId, T]),

    %% Pending limit:
    %% Ok, before we begin, lets check that this request 
    %% has not been aborted. I.e. exceeded the pending 
    %% limit, so go check it...

    #conn_data{sent_pending_limit = Limit} = ConnData,

    case check_and_maybe_create_pending_limit(Limit, sent, TransId) of
	ok ->
	    %% Ok so far, now update state
	    case megaco_monitor:lookup_reply(TransId) of
		[Rep] when is_record(Rep, reply) ->
		    Rep2 = Rep#reply{state = eval_request},
		    megaco_monitor:insert_reply(Rep2),
		    
		    Actions = T#'TransactionRequest'.actions,
		    {AckAction, SendReply} = 
			handle_request_callback(ConnData, TransId, Actions, T),

		    %% Next step, while we where in the callback function,
		    %% the pending limit could have been exceeded, so check
		    %% it again...
		    do_handle_request(AckAction, SendReply, 
				      ConnData, TransId);

		_ ->
		    %% Ugh?
		    ignore
	    end;

	aborted ->
	    %% Pending limit
	    %% Already exceeded the limit
	    %% The user does not yet know about this request, so
	    %% don't bother telling that it has been aborted...
	    %% Furthermore, the reply timer has not been started,
	    %% so do the cleanup now
	    ?rt1(ConnData, "pending limit already passed", [TransId]),
	    case megaco_monitor:lookup_reply(TransId) of
		[Rep] ->
		    cancel_reply(ConnData, Rep, aborted);
		_ ->
		    ok
	    end,
	    ignore
    end.

do_handle_request(_, ignore, _ConnData, _TransId) ->
    ?rt1(_ConnData, "ignore: don't reply", [_TransId]),
    ignore;
do_handle_request(_, ignore_trans_request, ConnData, TransId) ->
    ?rt1(ConnData, "ignore trans request: don't reply", [TransId]),
    case megaco_monitor:lookup_reply(TransId) of
	[#reply{} = Rep] ->
	    cancel_reply(ConnData, Rep, ignore);
	_ ->
	    ignore
    end;
do_handle_request({pending, _RequestData}, {aborted, ignore}, _, _) ->
    ?rt2("handle request: pending - aborted - ignore => don't reply", []),
    ignore;
do_handle_request({pending, _RequestData}, {aborted, _SendReply}, _, _) ->
    ?rt2("handle request: pending - aborted => don't reply", []),
    ignore;
do_handle_request({pending, RequestData}, _SendReply, _ConnData, _) ->
    ?rt2("handle request: pending", [RequestData]),
    {pending, RequestData};
do_handle_request(AckAction, {ok, Bin}, ConnData, TransId) ->
    ?rt1(ConnData, "handle request - ok", [AckAction, TransId]),
    case megaco_monitor:lookup_reply(TransId) of
	[#reply{pending_timer_ref = PendingRef} = Rep] ->

%% 	    d("do_handle_request -> found reply record:"
%% 	      "~n   Rep: ~p", [Rep]),
	    
	    #conn_data{reply_timer = InitTimer,
		       conn_handle = ConnHandle} = ConnData,

	    %% Pending limit update:
	    %%   - Cancel the pending timer, if running
	    %%   - Delete the pending counter
	    %% 

	    megaco_monitor:cancel_apply_after(PendingRef),
	    megaco_config:del_pending_counter(sent, TransId),

	    Method = timer_method(AckAction),
	    {WaitFor, CurrTimer} = init_timer(InitTimer),
	    OptBin = opt_garb_binary(CurrTimer, Bin),
	    M = ?MODULE,
	    F = reply_timeout,
	    A = [ConnHandle, TransId, CurrTimer],
	    Ref = megaco_monitor:apply_after(Method, M, F, A, 
					     WaitFor),
	    Rep2 = Rep#reply{pending_timer_ref = undefined,
			     handler           = undefined,
			     bytes             = OptBin,
			     state             = waiting_for_ack,
			     timer_ref         = Ref,
			     ack_action        = AckAction},
	    megaco_monitor:insert_reply(Rep2), % Timing problem?

%% 	    d("do_handle_request -> "
%% 	      "~n   Rep2: ~p", [Rep2]),

	    ignore;
	_ ->
	    %% Been removed already?
	    ignore
    end;
do_handle_request(_, {error, aborted}, ConnData, TransId) ->
    ?report_trace(ConnData, "aborted during our absence", [TransId]),
    case megaco_monitor:lookup_reply(TransId) of
	[Rep] ->
	    cancel_reply(ConnData, Rep, aborted);
	_ ->
	    ok
    end,
    ignore;
do_handle_request(AckAction, {error, Reason}, ConnData, TransId) ->
    ?report_trace(ConnData, "error", [TransId, Reason]),
    case megaco_monitor:lookup_reply(TransId) of
	[Rep] ->
	    Rep2 = Rep#reply{state      = waiting_for_ack,
			     ack_action = AckAction},
	    cancel_reply(ConnData, Rep2, Reason);
	_ ->
	    ok
    end,
    ignore;
do_handle_request(AckAction, SendReply, ConnData, TransId) ->
    ?report_trace(ConnData, "unknown send trans reply result", 
		  [TransId, AckAction, SendReply]),
    ignore.
    

handle_requests([{ConnData, TransId, T} | Rest], Pending) ->
    ?rt2("handle requests", [TransId]),
    case handle_request(ConnData, TransId, T) of
	{pending, RequestData} ->
	    handle_requests(Rest, [{ConnData,TransId,RequestData} | Pending]);
	_ ->
	    handle_requests(Rest, Pending)
    end;
handle_requests([], Pending) ->
    ?rt2("handle requests - done", [Pending]),
    Pending.

%% opt_garb_binary(timeout, _Bin) -> garb_binary; % Need msg at restart of timer
opt_garb_binary(_Timer,   Bin) -> Bin.

timer_method(discard_ack) ->
    apply_method;
timer_method(_) ->
    spawn_method.


handle_long_request({ConnData, TransId, RequestData}) ->

    ?rt2("handle long request", [TransId, RequestData]),

    %% Pending limit:
    %% We need to check the pending limit, in case it was
    %% exceeded before we got this far...
    %% We dont need to be able to create the counter here,
    %% since that was done in the handle_request function.

    #conn_data{sent_pending_limit = Limit} = ConnData,

    case check_pending_limit(Limit, sent, TransId) of
	{ok, _} ->
	    handle_long_request(ConnData, TransId, RequestData);
	_ ->
	    %% Already exceeded the limit
	    ignore
    end.

handle_long_request(ConnData, TransId, RequestData) ->
    ?report_trace(ConnData, "callback: trans long request",
		  [TransId, {request_data, RequestData}]),
    
    case megaco_monitor:lookup_reply(TransId) of
	[Rep] when is_record(Rep, reply) ->
	    %% Update (possibly) new handler
	    megaco_monitor:insert_reply(Rep#reply{handler = self()}),
	    {AckAction, Res} = 
		handle_long_request_callback(ConnData, TransId, RequestData),
	    do_handle_long_request(AckAction, Res, ConnData, TransId);
	 _ ->
	    %% Been removed already?
	    ignore
    end.


do_handle_long_request(AckAction, {ok, Bin}, ConnData, TransId) ->
    case megaco_monitor:lookup_reply(TransId) of
	[Rep] when is_record(Rep, reply) ->
	    Method = timer_method(AckAction),
	    InitTimer = ConnData#conn_data.reply_timer,
	    {WaitFor, CurrTimer} = init_timer(InitTimer),
	    OptBin = opt_garb_binary(CurrTimer, Bin),
	    ConnHandle = ConnData#conn_data.conn_handle,
	    M = ?MODULE,
	    F = reply_timeout,
	    A = [ConnHandle, Rep#reply.trans_id, CurrTimer],
	    Ref = megaco_monitor:apply_after(Method, 
					     M, F, A, 
					     WaitFor),
	    Rep2 = Rep#reply{bytes      = OptBin,
			     state      = waiting_for_ack,
			     timer_ref  = Ref,
			     ack_action = AckAction},
	    megaco_monitor:insert_reply(Rep2); % Timing problem?
	_ ->
	    %% Been removed already?
	    ignore
    end;
do_handle_long_request(_, {error, Reason}, ConnData, TransId) ->
    ?report_trace(ConnData, "send trans reply", [TransId, {error, Reason}]),
    ignore.
    
handle_request_abort_callback(ConnData, TransId, Pid) ->
    ?report_trace(ConnData, "callback: trans request aborted", [TransId, Pid]),
    ConnHandle = ConnData#conn_data.conn_handle,
    Version    = ConnData#conn_data.protocol_version,
    UserMod    = ConnData#conn_data.user_mod,
    UserArgs   = ConnData#conn_data.user_args,
    Serial     = TransId#trans_id.serial,
    Res = (catch apply(UserMod, handle_trans_request_abort, [ConnHandle, Version, Serial, Pid | UserArgs])),
    ?report_debug(ConnData, "return: trans request aborted", 
		  [TransId, {return, Res}]),
    case Res of
	ok ->
	    ok;
	_ ->
	    warning_msg("transaction request abort callback failed: ~w", 
			[Res]),
	    ok
    end.

handle_request_callback(ConnData, TransId, Actions, T) ->
    ?report_trace(ConnData, "callback: trans request", [T]),
    ConnHandle = ConnData#conn_data.conn_handle,
    Version    = ConnData#conn_data.protocol_version,
    UserMod    = ConnData#conn_data.user_mod,
    UserArgs   = ConnData#conn_data.user_args,
    Res = (catch apply(UserMod, handle_trans_request, [ConnHandle, Version, Actions | UserArgs])),
    ?report_debug(ConnData, "return: trans request", [T, {return, Res}]),
    case Res of
	ignore ->  %% NOTE: Only used for testing!!
	    {discard_ack, ignore};

	ignore_trans_request -> 
	    {discard_ack, ignore_trans_request};

	{discard_ack, Replies} when is_list(Replies) ->
	    Reply     = {actionReplies, Replies},
	    SendReply = maybe_send_reply(ConnData, TransId, Reply, 
					 [], asn1_NOVALUE),
	    {discard_ack, SendReply};
	{discard_ack, Error} when is_record(Error, 'ErrorDescriptor') ->
	    Reply     = {transactionError, Error},
	    SendReply = maybe_send_reply(ConnData, TransId, Reply, 
					 [], asn1_NOVALUE),
	    {discard_ack, SendReply};
	{discard_ack, Replies, SendOpts} when is_list(Replies) and 
					      is_list(SendOpts) ->
	    Reply     = {actionReplies, Replies},
	    SendReply = maybe_send_reply(ConnData, TransId, Reply, 
					 SendOpts, asn1_NOVALUE),
	    {discard_ack, SendReply};
	{discard_ack, Error, SendOpts} 
	when is_record(Error, 'ErrorDescriptor') and 
	     is_list(SendOpts) ->
	    Reply     = {transactionError, Error},
	    SendReply = maybe_send_reply(ConnData, TransId, Reply, 
					 SendOpts, asn1_NOVALUE),
	    {discard_ack, SendReply};

	{{handle_pending_ack, AckData}, Replies} when is_list(Replies) ->
	    Reply     = {actionReplies, Replies},
	    SendReply = maybe_send_reply(ConnData, TransId, Reply, 
					 [], when_pending_sent),
	    {{handle_ack, AckData}, SendReply};
	{{handle_pending_ack, AckData}, Error} 
	when is_record(Error, 'ErrorDescriptor') ->
	    Reply     = {transactionError, Error},
	    SendReply = maybe_send_reply(ConnData, TransId, Reply, 
					 [], when_pending_sent),
	    {{handle_ack, AckData}, SendReply};
	{{handle_pending_ack, AckData}, Replies, SendOpts} 
	when is_list(Replies) and 
	     is_list(SendOpts) ->
	    Reply     = {actionReplies, Replies},
	    SendReply = maybe_send_reply(ConnData, TransId, Reply, 
					 SendOpts, when_pending_sent),
	    {{handle_ack, AckData}, SendReply};
	{{handle_pending_ack, AckData}, Error, SendOpts} 
	when is_record(Error, 'ErrorDescriptor') and 
	     is_list(SendOpts) ->
	    Reply     = {transactionError, Error},
	    SendReply = maybe_send_reply(ConnData, TransId, Reply, 
					 SendOpts, when_pending_sent),
	    {{handle_ack, AckData}, SendReply};

	{{handle_ack, AckData}, Replies} when is_list(Replies) ->
	    Reply     = {actionReplies, Replies},
	    SendReply = maybe_send_reply(ConnData, TransId, Reply, 
					 [], 'NULL'),
	    {{handle_ack, AckData}, SendReply};
	{{handle_ack, AckData}, Error} 
	when is_record(Error, 'ErrorDescriptor') ->
	    Reply     = {transactionError, Error},
	    SendReply = maybe_send_reply(ConnData, TransId, Reply, 
					 [], 'NULL'),
	    {{handle_ack, AckData}, SendReply};
	{{handle_ack, AckData}, Replies, SendOpts} 
	when is_list(Replies) and 
	     is_list(SendOpts) ->
	    Reply     = {actionReplies, Replies},
	    SendReply = maybe_send_reply(ConnData, TransId, Reply, 
					 SendOpts, 'NULL'),
	    {{handle_ack, AckData}, SendReply};
	{{handle_ack, AckData}, Error, SendOpts} 
	when is_record(Error, 'ErrorDescriptor') and 
	     is_list(SendOpts) ->
	    Reply     = {transactionError, Error},
	    SendReply = maybe_send_reply(ConnData, TransId, Reply, 
					 SendOpts, 'NULL'),
	    {{handle_ack, AckData}, SendReply};

	{{handle_sloppy_ack, AckData}, Replies} when is_list(Replies) ->
	    Reply     = {actionReplies, Replies},
	    SendReply = maybe_send_reply(ConnData, TransId, Reply, 
					 [], asn1_NOVALUE),
	    {{handle_ack, AckData}, SendReply};
	{{handle_sloppy_ack, AckData}, Error} 
	when is_record(Error, 'ErrorDescriptor') ->
	    Reply     = {transactionError, Error},
	    SendReply = maybe_send_reply(ConnData, TransId, Reply, 
					 [], asn1_NOVALUE),
	    {{handle_ack, AckData}, SendReply};
	{{handle_sloppy_ack, AckData}, Replies, SendOpts} 
	when is_list(Replies) and 
	     is_list(SendOpts) ->
	    Reply     = {actionReplies, Replies},
	    SendReply = maybe_send_reply(ConnData, TransId, Reply, 
					 SendOpts, asn1_NOVALUE),
	    {{handle_ack, AckData}, SendReply};
	{{handle_sloppy_ack, AckData}, Error, SendOpts} 
	when is_record(Error, 'ErrorDescriptor') and 
	     is_list(SendOpts) ->
	    Reply     = {transactionError, Error},
	    SendReply = maybe_send_reply(ConnData, TransId, Reply, 
					 SendOpts, asn1_NOVALUE),
	    {{handle_ack, AckData}, SendReply};
	
	{pending, RequestData} ->
	    %% The user thinks that this request will take
	    %% quite a while to evaluate. Maybe respond with 
	    %% a pending trans (depends on the pending limit)
	    SendReply = maybe_send_pending(ConnData, TransId),
	    {{pending, RequestData}, SendReply};
	
	Error ->
	    ErrorText = atom_to_list(UserMod),
	    ED = #'ErrorDescriptor'{
	      errorCode = ?megaco_internal_gateway_error,
	      errorText = ErrorText},
	    ?report_important(ConnData, 
			      "callback: <ERROR> trans request",
			      [ED, {error, Error}]),
	    error_msg("transaction request callback failed: ~w", [Error]),
	    Reply = {transactionError, ED},
	    SendReply = maybe_send_reply(ConnData, TransId, Reply, 
					 [], asn1_NOVALUE),
	    {discard_ack, SendReply}
    end.

handle_long_request_callback(ConnData, TransId, RequestData) ->
    ?report_trace(ConnData, "callback: trans long request", [RequestData]),
    ConnHandle = ConnData#conn_data.conn_handle,
    Version    = ConnData#conn_data.protocol_version,
    UserMod    = ConnData#conn_data.user_mod,
    UserArgs   = ConnData#conn_data.user_args,
    Res = (catch apply(UserMod, handle_trans_long_request, 
		       [ConnHandle, Version, RequestData | UserArgs])),
    ?report_debug(ConnData, "return: trans long request",
		  [{request_data, RequestData}, {return, Res}]),
    case Res of
	ignore ->
	    {discard_ack, ignore};
	
	{discard_ack, Replies} when is_list(Replies) ->
	    Reply     = {actionReplies, Replies},
	    SendReply = maybe_send_reply(ConnData, TransId, Reply, 
					 [], asn1_NOVALUE),
	    {discard_ack, SendReply};
	{discard_ack, Replies, SendOpts} when is_list(Replies) and 
					      is_list(SendOpts) ->
	    Reply     = {actionReplies, Replies},
	    SendReply = maybe_send_reply(ConnData, TransId, Reply, 
					 SendOpts, asn1_NOVALUE),
	    {discard_ack, SendReply};
	
	{{handle_ack, AckData}, Replies} when is_list(Replies) ->
	    Reply     = {actionReplies, Replies},
	    SendReply = maybe_send_reply(ConnData, TransId, Reply, 
					 [], 'NULL'),
	    {{handle_ack, AckData}, SendReply};
	{{handle_ack, AckData}, Replies, SendOpts} when is_list(Replies) and 
							is_list(SendOpts) ->
	    Reply     = {actionReplies, Replies},
	    SendReply = maybe_send_reply(ConnData, TransId, Reply, 
					 SendOpts, 'NULL'),
	    {{handle_ack, AckData}, SendReply};
	
	Error ->
	    ErrorText = atom_to_list(UserMod),
	    ED = #'ErrorDescriptor'{errorCode = ?megaco_internal_gateway_error,
				    errorText = ErrorText},
	    ?report_important(ConnData, "callback: <ERROR> trans long request",
			      [ED, {error, Error}]),
	    error_msg("long transaction request callback failed: ~w", [Error]),
	    Reply     = {transactionError, ED},
	    SendReply = maybe_send_reply(ConnData, TransId, Reply, 
					 [], asn1_NOVALUE),
	    {discard_ack, SendReply}
    end.

handle_pending(ConnData, T) ->
    TransId = to_local_trans_id(ConnData),
    ?rt2("handle pending", [T, TransId]),
    case megaco_monitor:lookup_request(TransId) of
	[Req] ->

	    %% ------------------------------------------
	    %% 
	    %%   Check received pending limit
	    %% 
	    %% ------------------------------------------

	    Limit = ConnData#conn_data.recv_pending_limit,
	    case check_and_maybe_incr_pending_limit(Limit, 
						    recv, TransId) of
		
		ok ->
		    %% ----------------------------------------------------
		    %% 
		    %%      Received pending limit not exceeded     
		    %% 
		    %% ----------------------------------------------------

		    handle_recv_pending(ConnData, TransId, Req, T);
		
		error ->
		    %% ----------------------------------------------------
		    %% 
		    %%      Received pending limit exceeded     
		    %% 
		    %%      Time to give up on this transaction
		    %%      1) Delete request record
		    %%      2) Cancel timers
		    %%      3) Delete the (receive) pending counter
		    %%      4) Inform the user (handle_trans_reply)
		    %% 
		    %% ----------------------------------------------------

		    handle_recv_pending_error(ConnData, TransId, Req, T);

		
		aborted ->
		    %% ----------------------------------------------------
		    %% 
		    %%      Received pending limit already exceeded     
		    %% 
		    %% BMK BMK BMK -- can this really happen?
		    %% 
		    %%      The user has already been notified about this
		    %%      (see error above)
		    %% 
		    %% ----------------------------------------------------

		    ok

	    end;

	[] ->
	    ?report_trace(ConnData, "remote pending (no receiver)", [T]),
	    return_unexpected_trans(ConnData, T)
    end.

handle_recv_pending(#conn_data{long_request_resend = LRR,
			       conn_handle         = ConnHandle} = ConnData, 
		    TransId,
		    #request{timer_ref       = {short, Ref},
			     init_long_timer = InitTimer} = Req, T) ->

    ?rt2("handle pending - long request", [LRR, InitTimer]),

    %% The request seems to take a while,
    %% let's reset our transmission timer.
    %% We now know the other side has got
    %% the request and is working on it,
    %% so there is no need to keep the binary
    %% message for re-transmission.
    
    %% Start using the long timer. 
    %% We can now drop the "bytes", since we will
    %% not resend from now on.
    
    megaco_monitor:cancel_apply_after(Ref),
    {WaitFor, CurrTimer} = init_timer(InitTimer),
    ConnHandle = ConnData#conn_data.conn_handle,
    M = ?MODULE,
    F = request_timeout,
    A = [ConnHandle, TransId],
    Ref2 = megaco_monitor:apply_after(M, F, A, WaitFor),
    Req2 = 
	case LRR of
	    true ->
		Req#request{timer_ref  = {long, Ref2},
			    curr_timer = CurrTimer};
	    false ->
		Req#request{bytes      = {no_send, garb_binary}, 
			    timer_ref  = {long, Ref2},
			    curr_timer = CurrTimer}
	end,
    ?report_trace(ConnData, "trans pending (timer restarted)", [T]),
    megaco_monitor:insert_request(Req2); % Timing problem?
    
handle_recv_pending(_ConnData, _TransId,
		    #request{timer_ref  = {long, _Ref},
			     curr_timer = timeout}, _T) ->

    ?rt3("handle pending - timeout"),

    %% The request seems to take a while,
    %% let's reset our transmission timer.
    %% We now know the other side has got
    %% the request and is working on it,
    %% so there is no need to keep the binary
    %% message for re-transmission.
    
    %% This can happen if the timer is running for the last 
    %% time. I.e. next time it expires, will be the last.
    %% Therefor we really do not need to do anything here.
    %% The cleanup will be done in request_timeout.
    
    ok;

handle_recv_pending(#conn_data{conn_handle = ConnHandle} = ConnData, TransId,
		    #request{timer_ref  = {long, Ref},
			     curr_timer = CurrTimer} = Req, T) ->
    
    ?rt2("handle pending - still waiting", [CurrTimer]),

    %% The request seems to take a while,
    %% let's reset our transmission timer.
    %% We now know the other side has got
    %% the request and is working on it,
    %% so there is no need to keep the binary
    %% message for re-transmission.
    
    %% We just need to recalculate the timer, i.e. 
    %% increment the timer (one "slot" has been consumed).
    
    megaco_monitor:cancel_apply_after(Ref),
    {WaitFor, Timer2} = recalc_timer(CurrTimer),
    ConnHandle = ConnData#conn_data.conn_handle,
    M = ?MODULE,
    F = request_timeout,
    A = [ConnHandle, TransId],
    Ref2 = megaco_monitor:apply_after(M, F, A, WaitFor),
    Req2 = Req#request{timer_ref  = {long, Ref2},
		       curr_timer = Timer2},
    ?report_trace(ConnData, 
		  "long trans pending"
		  " (timer restarted)", [T]),
    %% Timing problem?
    megaco_monitor:insert_request(Req2).
    

handle_recv_pending_error(ConnData, TransId, Req, T) ->
    %% 1) Delete the request record
    megaco_monitor:delete_request(TransId),

    %% 2) Possibly cancel the timer
    case Req#request.timer_ref of
	{_, Ref} ->
	    megaco_monitor:cancel_apply_after(Ref);
	_ ->
	    ok
    end,
    
    %% 3) Delete the (receive) pending counter
    megaco_config:del_pending_counter(recv, TransId),
    
    %% 4) Inform the user that his/her request reached 
    %%    the receive pending limit
    UserMod   = Req#request.user_mod,
    UserArgs  = Req#request.user_args,
    Action    = Req#request.reply_action,
    UserData  = Req#request.reply_data,
    UserReply = {error, exceeded_recv_pending_limit},
    ConnData2 = ConnData#conn_data{user_mod     = UserMod,
				   user_args    = UserArgs,
				   reply_action = Action,
				   reply_data   = UserData},

    ?report_trace(ConnData, "receive pending limit reached", [T]),
    return_reply(ConnData2, TransId, UserReply).


handle_reply(CD, T) ->
    TransId = to_local_trans_id(CD),
    ?rt2("handle reply", [T, TransId]), 
    case megaco_monitor:lookup_request(TransId) of
	[Req] when is_record(Req, request) and CD#conn_data.cancel == true -> 
	    ?TC_AWAIT_REPLY_EVENT(true),
	    do_handle_reply_cancel(CD, Req, T);

	[Req] -> 
	    ?TC_AWAIT_REPLY_EVENT(false),
	    %% Just in case conn_data got update after our lookup
	    %% but before we looked up the request record, we
	    %% check the cancel field again.
	    case megaco_config:conn_info(CD, cancel) of
		true ->
		    do_handle_reply_cancel(CD, Req, T);
		false ->
		    do_handle_reply(CD, Req, TransId, T)
	    end;
	[] ->
	    ?TC_AWAIT_REPLY_EVENT(undefined),
	    ?report_trace(CD, "trans reply (no receiver)", [T]),
	    return_unexpected_trans(CD, T)
    end.

do_handle_reply_cancel(CD, #request{user_mod     = UserMod,
				    user_args    = UserArgs, 
				    reply_action = Action, 
				    reply_data   = UserData}, T) ->
    CD2 = CD#conn_data{user_mod     = UserMod,
		       user_args    = UserArgs,
		       reply_action = Action,
		       reply_data   = UserData},
    return_unexpected_trans(CD2, T).

do_handle_reply(CD, #request{timer_ref    = {_Type, Ref}, % OTP-4843
			     user_mod     = UserMod,
			     user_args    = UserArgs, 
			     reply_action = Action, 
			     reply_data   = UserData}, TransId, T) ->
    %% Don't care about Req and Rep version diff
    ?report_trace(CD, "trans reply", [T]),
    megaco_monitor:delete_request(TransId),
    megaco_monitor:cancel_apply_after(Ref), %% OTP-4843
    
    %% Send acknowledgement
    maybe_send_ack(T#'TransactionReply'.immAckRequired, CD),

    UserReply =
	case T#'TransactionReply'.transactionResult of
	    {transactionError, Reason} ->
		{error, Reason};
	    {actionReplies, Replies} ->
		{ok, Replies}
	end,
    CD2 = CD#conn_data{user_mod     = UserMod,
		       user_args    = UserArgs,
		       reply_action = Action,
		       reply_data   = UserData},
    return_reply(CD2, TransId, UserReply).

handle_acks([{ConnData, Rep, T} | Rest])
  when Rep#reply.state == waiting_for_ack ->
    handle_ack(ConnData, ok, Rep, T),
    handle_acks(Rest);
handle_acks([]) ->
    ok.

%% OTP-4213: Temporary until we figure out why this work...
handle_ack(ConnData, AckStatus, Rep, T) ->
    #reply{trans_id          = TransId,
	   timer_ref         = ReplyRef,
	   pending_timer_ref = PendingRef,  %% BMK Still running?
	   ack_action        = AckAction} = Rep,
    megaco_monitor:cancel_apply_after(ReplyRef),
    megaco_monitor:cancel_apply_after(PendingRef),
    megaco_monitor:delete_reply(TransId),
    megaco_config:del_pending_counter(sent, TransId), %% BMK Still existing?
    handle_ack_callback(ConnData, AckStatus, AckAction, T).

% handle_ack(ConnData, AckStatus, Rep, T) ->
%     megaco_monitor:cancel_apply_after(Rep#reply.pending_timer_ref),
%     megaco_monitor:cancel_apply_after(Rep#reply.timer_ref),
%     TransId = to_remote_trans_id(ConnData),
%     megaco_monitor:delete_reply(TransId),
%     handle_ack_callback(ConnData, AckStatus, Rep#reply.ack_action, T).

handle_ack_callback(_CD, ok = _AS, discard_ack = _AA, _T) ->
    ok;
handle_ack_callback(ConnData, {error, Reason}, discard_ack = AckAction, T) ->
    ?report_trace(ConnData, "handle ack (no callback)",
		  [T, AckAction, {error, Reason}]);
handle_ack_callback(ConnData, AckStatus, {handle_ack, AckData}, T) ->
    ?report_trace(ConnData, "callback: trans ack", [{ack_data, AckData}]),
    ConnHandle = ConnData#conn_data.conn_handle,
    Version    = ConnData#conn_data.protocol_version,
    UserMod    = ConnData#conn_data.user_mod,
    UserArgs   = ConnData#conn_data.user_args,
    Res = (catch apply(UserMod, handle_trans_ack, [ConnHandle, Version, AckStatus, AckData | UserArgs])),
    ?report_debug(ConnData, "return: trans ack", [T, AckData, {return, Res}]),
    case Res of
	ok ->
	    ok;
	_ ->
	    warning_msg("transaction ack callback failed: ~w", [Res]),
	    ok
    end,
    Res.


handle_message_error(ConnData, _Error) 
  when ConnData#conn_data.monitor_ref == undefined_monitor_ref ->
    %% May occur if another process already has setup a
    %% temporary connection, but the handle_connect callback
    %% function has not yet returned before the eager MG
    %% re-sends its initial service change message.
    ignore;
handle_message_error(ConnData, Error) ->
    ?report_trace(ConnData, "callback: message error", [Error]),
    ConnHandle = ConnData#conn_data.conn_handle,
    Version    = ConnData#conn_data.protocol_version,
    UserMod    = ConnData#conn_data.user_mod,
    UserArgs   = ConnData#conn_data.user_args,
    Res = (catch apply(UserMod, handle_message_error, [ConnHandle, Version, Error | UserArgs])),
    ?report_debug(ConnData, "return: message error", [Error, {return, Res}]),
    case Res of
	ok ->
	    ok;
	_ ->
	    warning_msg("message error callback failed: ~w", [Res]),
	    ok
    end,
    Res.

handle_disconnect_callback(ConnData, UserReason)
  when is_record(ConnData, conn_data) ->
    ?report_trace(ConnData, "callback: disconnect", [{reason, UserReason}]),
    ConnHandle = ConnData#conn_data.conn_handle,
    Version    = ConnData#conn_data.protocol_version,
    UserMod    = ConnData#conn_data.user_mod,
    UserArgs   = ConnData#conn_data.user_args,
    Res = (catch apply(UserMod, handle_disconnect, [ConnHandle, Version, UserReason | UserArgs])),
    ?report_debug(ConnData, "return: disconnect", [{reason, UserReason}, {return, Res}]),
    case Res of
	ok ->
	    ok;
	_ ->
	    warning_msg("disconnect callback failed: ~w", [Res]),
	    ok
    end,
    Res.


%%----------------------------------------------------------------------
%% Test "outgoing" messages
%%----------------------------------------------------------------------

%% test_request/5 -> {MegacoMessage, EncodingRes}
%% 
%% This function is only intended for testing 
%% (e.g. answer the question: have I constructed a valid action request?)
%% 
%% It's not exactly the same code as a call to 'call'
%% or 'cast' but close enough.
%% 
test_request(ConnHandle, Actions, 
	     Version, EncodingMod, EncodingConfig)
  when is_record(ConnHandle, megaco_conn_handle) and
       is_integer(Version) and is_atom(EncodingMod) ->
    %% Create a fake conn_data structure
    ConnData = #conn_data{serial           = 1, 
			  protocol_version = Version,
			  conn_handle      = ConnHandle,
			  auth_data        = asn1_NOVALUE,
			  encoding_mod     = EncodingMod,
			  encoding_config  = EncodingConfig},

    TRs = test_req_compose_transactions(ConnData, Actions),
    Body = {transactions, TRs},
    MegaMsg = megaco_messenger_misc:compose_message(ConnData, Version, Body),
    EncodeRes = megaco_messenger_misc:encode_message(ConnData, MegaMsg),
    {MegaMsg, EncodeRes}.


test_req_compose_transactions(ConnData, [A|_] = ActionsList) when is_list(A) ->
    LastSerial = ConnData#conn_data.serial,
    test_req_compose_transactions(LastSerial, lists:reverse(ActionsList), []);
test_req_compose_transactions(#conn_data{serial = Serial}, Actions) ->
    TR   = #'TransactionRequest'{transactionId = Serial,
				 actions       = Actions},
    [{transactionRequest, TR}].

test_req_compose_transactions(_Serial, [], Acc) ->
    lists:reverse(Acc);
test_req_compose_transactions(Serial, [A|As], Acc) ->
    TR = #'TransactionRequest'{transactionId = Serial,
                               actions       = A},
    test_req_compose_transactions(Serial, As, [{transactionRequest, TR}|Acc]).


test_reply(ConnHandle, Version, EncodingMod, EncodingConfig, Error) 
  when is_record(Error, 'ErrorDescriptor') ->
    Reply = {transactionError, Error},
    test_reply_encode(ConnHandle, Version, EncodingMod, EncodingConfig, Reply);
test_reply(ConnHandle, Version, EncodingMod, EncodingConfig, Replies) 
  when is_list(Replies) ->
    Reply = {actionReplies, Replies},
    test_reply_encode(ConnHandle, Version, EncodingMod, EncodingConfig, Reply).

test_reply_encode(ConnHandle, Version, EncodingMod, EncodingConfig, Reply) ->
    ImmAck   = asn1_NOVALUE,
    Serial   = 1,
    %% Create a fake conn_data structure
    ConnData = #conn_data{serial           = Serial, 
			  protocol_version = Version,
			  conn_handle      = ConnHandle,
			  auth_data        = asn1_NOVALUE,
			  encoding_mod     = EncodingMod,
			  encoding_config  = EncodingConfig},
    TR = #'TransactionReply'{transactionId     = Serial,
                             immAckRequired    = ImmAck,
                             transactionResult = Reply},
    Body      = {transactions, [{transactionReply, TR}]},
    MegaMsg   = megaco_messenger_misc:compose_message(ConnData, Version, Body),
    EncodeRes = megaco_messenger_misc:encode_message(ConnData, MegaMsg),
    {MegaMsg, EncodeRes}.
    

%%----------------------------------------------------------------------
%% Send (or prepare) outgoing messages
%%----------------------------------------------------------------------

%% Description:
%% Encode a list of actions or a list of list of actions for
%% later sending (using call or cast).
%% 
%% encode_actions(CH, Acts, Opts) -> {ok, EncodedActions()} | {error, Reason}
%% CH -> connection_handle()
%% Acts -> action_reqs() | [action_reqs()]
%% action_reqs() -> [action_req()]
%% action_req() -> #'ActionRequest'
%% Opts -> [option()]
%% option() -> {Tab, Val}
%% Tag -> atom()
%% Val -> term()
%% EncodedActionsList -> binary() | [binary()]
%% Reason -> term()
encode_actions(CH, [A|_] = ActionsList, Opts) 
  when is_record(CH, megaco_conn_handle) and is_list(A) ->
    (catch encode_multi_actions(CH, ActionsList, Opts));

encode_actions(CH, [A|_] = Actions, Opts) 
  when is_record(CH, megaco_conn_handle) and is_tuple(A) ->
    do_encode_actions(CH, Actions, Opts).
    
encode_multi_actions(CH, ActionsList, Opts) ->
    case prepare_req_send_options(CH, Opts) of
	{ok, CD} ->
	    ActsList = [encode_multi_actions(CD, Acts) || Acts <- ActionsList],
	    {ok, ActsList};
	Error ->
	    Error
    end.

encode_multi_actions(CD, Actions) ->
    case megaco_messenger_misc:encode_actions(CD, "encode multi actions", Actions) of
	{ok, Bin} ->
	    Bin;
	Error ->
	    throw(Error)
    end.

do_encode_actions(CH, Actions, Opts) 
  when is_record(CH, megaco_conn_handle) ->
    case prepare_req_send_options(CH, Opts) of
        {ok, CD} ->
	    megaco_messenger_misc:encode_actions(CD, "encode actions", Actions);
	Error ->
	    Error
    end.

prepare_req_send_options(CH, Opts) ->
    case megaco_config:lookup_local_conn(CH) of
        [CD] ->
            override_req_send_options(CD, Opts);
        [] ->
            {error, {not_found, conn_data}}
    end.
    

call(ConnHandle, Actions, Options) ->
    case lists:keymember(reply_data, 1, Options) of
	true ->
	    {error, {bad_option, reply_data}};
	false ->
	    Options2 = [{reply_data, self()} | Options],
	    call_or_cast(call, ConnHandle, Actions, Options2)
    end.

cast(ConnHandle, Actions, Options) ->
    call_or_cast(cast, ConnHandle, Actions, Options).

%% In a transaction there can be several actions, so if the 
%% First element of the Actions list is an ''ActionRequest''
%% record this a list of ActionRequest's for one Transaction
%% request. If on the other hand this is not the case, then
%% the Actions list is assumed to be a list of list of
%% ActionRequest. That is, action requests for several transactions.
%% It could also be a binary or a list of binaries (if 
%% the actions has already been encoded).
call_or_cast(CallOrCast, ConnHandle, [A|_] = Actions, Options) 
  when is_tuple(A) ->
    %% Just one transaction
    case call_or_cast(CallOrCast, ConnHandle, [Actions], Options) of
	ok ->
	    ok;
	{error, Reason} ->
	    {error, Reason};
	{Version, [Reply]} when is_integer(Version) ->
	    {Version, Reply};
	{Version, Error} when is_integer(Version) ->
	    {Version, Error}
    end;

call_or_cast(CallOrCast, ConnHandle, Actions, Options) 
  when is_binary(Actions) ->
    %% Just one transaction (although the actions has already been encoded)
    case call_or_cast(CallOrCast, ConnHandle, [Actions], Options) of
	ok ->
	    ok;
	{error, Reason} ->
	    {error, Reason};
	{Version, [Reply]} when is_integer(Version) ->
	    {Version, Reply};
	{Version, Error} when is_integer(Version) ->
	    {Version, Error}
    end;

call_or_cast(CallOrCast, ConnHandle, ActionsList, Options)
  when is_record(ConnHandle, megaco_conn_handle) ->
    case prepare_req_send_options(ConnHandle, Options, ActionsList) of
        {ok, ConnData} ->
	    ?report_trace(ConnData, "call_or_cast - options prepared", []),
            case encode_requests(ConnData, ActionsList) of
                {ok, TRs, BinOrBins} -> 
		    ?report_trace(ConnData, "call_or_cast - request encoded", []),
                    send_request(ConnData, ConnHandle, 
				 TRs, CallOrCast, BinOrBins),
		    case CallOrCast of
			call -> 
			    TransIds = to_local_trans_id(ConnData, TRs),
			    wait_for_reply(TransIds);
			cast -> 
			    ok
		    end;
                {error, Reason} ->
		    Version = ConnData#conn_data.protocol_version,
                    return_error(CallOrCast, Version, {error, Reason})
            end;
        {error, Reason} ->
            return_error(CallOrCast, 1, {error, Reason})
    end;
call_or_cast(CallOrCast, ConnHandle, _Actions, _Options) ->
    return_error(CallOrCast, 1, {error, {bad_megaco_conn_handle, ConnHandle}}).


return_error(Action, Version, Error) ->
    case Action of
	call -> {Version, Error};
	cast -> Error
    end.

wait_for_reply(TransIds) ->
    wait_for_reply(TransIds, []).

wait_for_reply([], Replies0) ->
    % Make sure they come in the same order as the requests where sent
    Replies1 = lists:keysort(2,Replies0), 
    %% Must all be the same version
    [{Version,_,_}|_] = Replies1,
    Replies2 = [Result || {_Version, _TransId, Result} <- Replies1],
    {Version, Replies2};
wait_for_reply(TransIds, Replies) -> 
    receive
        {?MODULE, TransId, Version, Result} ->
	    TransIds2 = lists:delete(TransId, TransIds), 
	    wait_for_reply(TransIds2, [{Version, TransId, Result}|Replies])% ;
% 	Any ->
% 	    d("wait_for_reply -> received unexpected: ~n   ~p", [Any]),
% 	    wait_for_reply(TransIds, Replies)
	    
    end.


%% TransInfo is either [trans_id()] or a [trans_req()]

%% This is the normal case where we have just one
%% transaction to be sent (using call or cast) using
%% the transaction sender process.
send_request(#conn_data{control_pid  = CP, 
			trans_req    = true, 
			trans_sender = Pid} = CD, 
	     CH, [Serial], Action, [Bin])
  when is_pid(Pid) and is_integer(Serial) and (node(CP) == node()) ->

    ?report_trace(CD, "send_request - one transaction via trans-sender", [Serial]),

    #conn_data{request_timer       = InitTimer,
	       long_request_timer  = LongTimer} = CD,
    TransId = to_local_trans_id(CH, Serial),
    insert_request(CD, CH, TransId, Action, {Serial, Bin},
		   InitTimer, LongTimer),
    megaco_trans_sender:send_req(Pid, Serial, Bin);

%% This is the general case where we have several transactions
%% beeing sent (using call or cast) at once using
%% the transaction sender process.
send_request(#conn_data{control_pid  = CP, 
			trans_req    = true, 
			trans_sender = Pid} = CD, 
	     CH, TransInfo, Action, Bins)
  when is_pid(Pid) and is_list(Bins) and (node(CP) == node()) ->

    ?report_trace(CD, "send_request - multi transactions via trans_sender", [TransInfo, Pid]),

    #conn_data{request_timer       = InitTimer,
	       long_request_timer  = LongTimer} = CD,
    insert_requests(CD, CH, TransInfo, Action, Bins,
		    InitTimer, LongTimer),
    megaco_trans_sender:send_reqs(Pid, TransInfo, Bins);

%% This is the case when one or more transactions is
%% beeing sent in one message immediatelly (not using
%% the transaction sender process. E.g. the binary is 
%% this encoded message.
send_request(#conn_data{control_pid = CP} = CD, 
	     CH, TRs, Action, Bin)
  when is_list(TRs) and is_binary(Bin) and (node(CP) == node()) ->

%     d("send_request -> entry with"
%       "~n   TRs: ~p", [TRs]),

    ?report_trace(CD, "send_request - multi transaction", [TRs]),

    #conn_data{request_timer       = InitTimer,
	       long_request_timer  = LongTimer} = CD,
    insert_requests(CD, CH, TRs, Action, Bin,
		    InitTimer, LongTimer),
    case megaco_messenger_misc:send_message(CD, Bin) of
	{error, Reason} ->
	    cancel_requests(CD, TRs, Reason);
	{ok, _} ->
	    ignore
    end;

%% This is the case where we are not on the node where the
%% transport process run.
send_request(#conn_data{control_pid = CP} = CD, 
	     CH, TransInfo, Action, Bin) 
  when node(CP) /= node() ->

    ?report_trace(CD, "send_request - remote", [TransInfo]),

    InitTimer = infinity,
    LongTimer = infinity,
    insert_requests(CD, CH, TransInfo, Action, Bin,
		    InitTimer, LongTimer),
    Node = node(CP),
    Args = [node(), CD, TransInfo, Bin],
    rpc:cast(Node, ?MODULE, send_request_remote, Args).


insert_requests(_, _, [], _, _, _, _) ->
    ok;

insert_requests(ConnData, ConnHandle, [Serial|Serials], 
		Action, [Bin|Bins], InitTimer, LongTimer) 
  when is_integer(Serial) and is_binary(Bin) ->
    TransId = to_local_trans_id(ConnHandle, Serial),
    insert_request(ConnData, ConnHandle, 
		   TransId, Action, Bin, InitTimer, LongTimer),

    insert_requests(ConnData, ConnHandle, Serials, Action, Bins, 
		    InitTimer, LongTimer);

insert_requests(ConnData, ConnHandle, 
		[{transactionRequest, TR}|TRs], 
		Action, Bin, InitTimer, LongTimer) 
  when is_record(TR, 'TransactionRequest') and is_binary(Bin) ->
    #'TransactionRequest'{transactionId = Serial} = TR,
    TransId = to_local_trans_id(ConnHandle, Serial),
    insert_request(ConnData, ConnHandle, 
		   TransId, Action, TR, InitTimer, LongTimer),

    insert_requests(ConnData, ConnHandle, TRs, Action, Bin, 
		    InitTimer, LongTimer).


insert_request(ConnData, ConnHandle, TransId, 
	       Action, Data, InitTimer, LongTimer) ->
    #megaco_conn_handle{remote_mid = RemoteMid} = ConnHandle,
    #conn_data{protocol_version    = Version,
	       user_mod            = UserMod,
	       user_args           = UserArgs,
	       send_handle         = SendHandle,
	       reply_data          = ReplyData} = ConnData,
    {WaitFor, CurrTimer} = init_timer(InitTimer),
    M   = ?MODULE,
    F   = request_timeout,
    A   = [ConnHandle, TransId],
    Ref = megaco_monitor:apply_after(M, F, A, WaitFor),
    Req = #request{trans_id        = TransId, 
		   remote_mid      = RemoteMid,
                   timer_ref       = ?SIM({short, Ref}, init_request_timer),
                   init_timer      = InitTimer,
                   init_long_timer = LongTimer,
                   curr_timer      = CurrTimer,
                   version         = Version,
                   bytes           = {send, Data},
		   send_handle     = SendHandle,
                   user_mod        = UserMod,
                   user_args       = UserArgs,
                   reply_action    = Action,
                   reply_data      = ReplyData},
    megaco_monitor:insert_request(Req). % Timing problem?

    
send_request_remote(ReplyNode, ConnData, TransInfo, Bin) ->
    Action = remote,
    ConnHandle = ConnData#conn_data.conn_handle,
    ConnData2  = ConnData#conn_data{reply_data = ReplyNode},
    send_request(ConnData2, ConnHandle, TransInfo, Action, Bin).

prepare_req_send_options(ConnHandle, Options, Actions) ->
    %% Ensures that two processes cannot get same transaction id.
    %% Bad send options may cause spurious transaction id to be consumed.
    Incr = number_of_transactions(Actions),
    case megaco_config:incr_trans_id_counter(ConnHandle, Incr) of
        {ok, ConnData} ->
            override_req_send_options(ConnData, Options);
        {error, Reason} ->
            {error, Reason}
    end.

number_of_transactions([Action|_]) when is_tuple(Action) ->
    1;
number_of_transactions(ActionsList) ->
    length(ActionsList).

override_req_send_options(ConnData, [{Key, Val} | Tail]) ->
    case Key of
	protocol_version ->
            ConnData2 = ConnData#conn_data{protocol_version = Val},
            override_req_send_options(ConnData2, Tail);	    
        send_handle ->
            ConnData2 = ConnData#conn_data{send_handle = Val},
            override_req_send_options(ConnData2, Tail);
        request_timer ->
            case megaco_config:verify_val(Key, Val) of
                true ->
                    ConnData2 = ConnData#conn_data{request_timer = Val},
                    override_req_send_options(ConnData2, Tail);
                false ->
                    {error, {bad_send_option, {Key, Val}}}
            end;
        long_request_timer ->
            case megaco_config:verify_val(Key, Val) of
                true ->
                    ConnData2 = ConnData#conn_data{long_request_timer = Val},
                    override_req_send_options(ConnData2, Tail);
                false ->
                    {error, {bad_send_option, {Key, Val}}}
            end;
        reply_data ->
            ConnData2 = ConnData#conn_data{reply_data = Val},
            override_req_send_options(ConnData2, Tail);
        user_mod when is_atom(Val) ->
            ConnData2 = ConnData#conn_data{user_mod = Val},
            override_req_send_options(ConnData2, Tail);
        user_args when is_list(Val) ->
            ConnData2 = ConnData#conn_data{user_args = Val},
            override_req_send_options(ConnData2, Tail);
	trans_req when Val == false -> 
	    %% We only allow turning the transaction-sender off, since
	    %% the opposite (turning it on) would causing to much headake...
	    %% This will allow not using the transaction sender for
	    %% occasional messages
	    ConnData2 = ConnData#conn_data{trans_req = Val, 
					   trans_sender = undefined},
	    override_req_send_options(ConnData2, Tail);
        _Bad ->
            {error, {bad_send_option, {Key, Val}}}
    end;
override_req_send_options(ConnData, []) ->
    {ok, ConnData}.

override_rep_send_options(ConnData, [{Key, Val} | Tail]) ->
    case Key of
	protocol_version ->
            ConnData2 = ConnData#conn_data{protocol_version = Val},
            override_rep_send_options(ConnData2, Tail);	    
        send_handle ->
            ConnData2 = ConnData#conn_data{send_handle = Val},
            override_rep_send_options(ConnData2, Tail);
        reply_timer ->
            case megaco_config:verify_val(Key, Val) of
                true ->
                    ConnData2 = ConnData#conn_data{reply_timer = Val},
                    override_rep_send_options(ConnData2, Tail);
                false ->
                    {error, {bad_send_option, {Key, Val}}}
            end;
	trans_req when Val == false -> 
	    %% We only allow turning the transaction-sender off, since
	    %% the opposite (turning it on) would causing to much headake...
	    %% This will allow not using the transaction sender for
	    %% occasional messages
	    ConnData2 = ConnData#conn_data{trans_req = Val, 
					   trans_sender = undefined},
	    override_rep_send_options(ConnData2, Tail);
        _Bad ->
            {error, {bad_send_option, {Key, Val}}}
    end;
override_rep_send_options(ConnData, []) ->
    {ok, ConnData}.


%% ----
%% This list is allways atleast one (list of actions) long.
%% ----
%% The proper number of transaction id numbers has already
%% been "allocated", and the connection data record is
%% updated accordingly.
encode_requests(#conn_data{trans_req    = true,
			   trans_sender = Pid,
			   serial       = LastSerial} = CD, ActionsList) 
  when is_pid(Pid) ->
    (catch encode_requests(CD, LastSerial, 
			   lists:reverse(ActionsList), [], []));
encode_requests(#conn_data{serial    = LastSerial} = CD, ActionsList) ->
    %% We shall not accumulate transactions. 
    %% This means that we shall not encode 
    %% the transactions individually (and send
    %% them to the sender process, which
    %% accumulate transactions for later sending),
    %% Instead we encode the entire message directly.
    %% => We shall return one binary, containing, 
    %%    possibly, many transactions
    encode_requests_in_msg(CD, LastSerial, lists:reverse(ActionsList)).


%% This means that we shall compose and encode one complete 
%% megaco message, containing one or more transactions.
encode_requests_in_msg(CD, LastSerial, ActionsList) ->
    TRs  = compose_requests_in_msg(LastSerial, ActionsList, []),
    Body = {transactions, TRs},
    Res  = megaco_messenger_misc:encode_body(CD, 
					     "encode trans request(s) msg", 
					     Body),
    case Res of
	{ok, Bin} ->
	    {ok, TRs, Bin};
	Error ->
	    Error
    end.

compose_requests_in_msg(_S, [], TRs) ->
    TRs;
compose_requests_in_msg(Serial, [A|As], Acc) ->
    TR = #'TransactionRequest'{transactionId = Serial,
                               actions       = A},
    compose_requests_in_msg(Serial - 1, As, [{transactionRequest, TR}|Acc]).


%% We have done the encoding in reverse order, so there
%% is no need to reverse now.
encode_requests(_, _, [], Serials, EncodedTRs) ->
    {ok, Serials, EncodedTRs}; 
encode_requests(CD, Serial, [Actions|ActionsList], Serials, EncodedTRs) ->
    case do_encode_request(CD, Serial, Actions) of
	{ok, Bin} ->
	    encode_requests(CD, Serial - 1, ActionsList, 
			    [Serial|Serials], [Bin|EncodedTRs]);
	Error ->
	    throw(Error)
    end.


do_encode_request(CD, Serial, Actions) ->
    TR = #'TransactionRequest'{transactionId = Serial,
			       actions       = Actions},
    megaco_messenger_misc:encode_trans_request(CD, TR).
    

imm_ack_req(Counter,  when_pending_sent) when (Counter > 0) -> 'NULL';
imm_ack_req(_Counter, when_pending_sent)                    -> asn1_NOVALUE;
imm_ack_req(_Counter, ImmAck)                               -> ImmAck.

maybe_send_reply(#conn_data{sent_pending_limit = Limit} = ConnData, 
		 TransId, Result, SendOpts, ImmAck) ->

%%     d("maybe_send_reply -> entry with"
%%       "~n   Limit:    ~p"
%%       "~n   TransId:  ~p"
%%       "~n   Result:   ~p"
%%       "~n   SendOpts: ~p"
%%       "~n   ImmAck:   ~p", [Limit, TransId, Result, SendOpts, ImmAck]),

    %% Pending limit
    %% Before we can send the reply we must check that we have 
    %% not passed the pending limit (and sent an error message).
    case check_pending_limit(Limit, sent, TransId) of
	{ok, Counter} ->
	    case override_rep_send_options(ConnData, SendOpts) of
		{ok, ConnData2} ->
		    send_reply(ConnData2, Result, 
			       imm_ack_req(Counter, ImmAck));
		Error ->
		    Error
	    end;
	aborted ->
	    {error, aborted}
    end.

encode_reply(CD, TR) ->
    megaco_messenger_misc:encode_trans_reply(CD, TR).

send_reply(#conn_data{serial       = Serial,
		      trans_req    = true,
		      trans_sender = Pid} = CD, Result, ImmAck) ->
    TR = #'TransactionReply'{transactionId     = Serial,
                             immAckRequired    = ImmAck,
                             transactionResult = Result},
    case encode_reply(CD, TR) of
	{ok, Bin} ->
	    megaco_trans_sender:send_reply(Pid, Bin),
	    {ok, Bin};
	{error, Reason} = Error ->
	    ED = #'ErrorDescriptor'{errorCode = ?megaco_internal_gateway_error,
				    errorText = "encode body"},
	    Reply = {transactionError, ED},
	    TR2 =  TR#'TransactionReply'{transactionResult = Reply},
	    TraceLabel = "<ERROR> encode trans reply failed",
	    ?report_important(CD, TraceLabel, [TR, TR2, ED, Error]),
	    error_msg("failed encoding transaction reply body: ~s", 
		      [format_encode_error_reason(Reason)]),
	    Body2 = {transactions, [{transactionReply, TR2}]},
	    megaco_messenger_misc:send_body(CD, TraceLabel, Body2),
	    Error
    end;
send_reply(#conn_data{serial = Serial} = CD, Result, ImmAck) ->
    %% Encapsule the transaction result into a reply message

%%     d("send_reply -> entry with"
%%       "~n   Serial: ~p"
%%       "~n   Result: ~p"
%%       "~n   ImmAck: ~p", [Serial, Result, ImmAck]),

    TR = #'TransactionReply'{transactionId     = Serial,
                             immAckRequired    = ImmAck,
                             transactionResult = Result},
    Body = {transactions, [{transactionReply, TR}]},
    case megaco_messenger_misc:encode_body(CD, "encode trans reply", Body) of
        {ok, Bin} ->
            megaco_messenger_misc:send_message(CD, Bin);
        {error, Reason} = Error ->
	    ED = #'ErrorDescriptor'{errorCode = ?megaco_internal_gateway_error,
				    errorText = "encode body"},
            Reply      = {transactionError, ED},
            TR2        =  TR#'TransactionReply'{transactionResult = Reply},
            TraceLabel = "<ERROR> encode trans reply body failed",
            ?report_important(CD, TraceLabel, [TR, TR2, ED, Error]),
	    error_msg("failed encoding transaction reply body: ~s", 
		      [format_encode_error_reason(Reason)]),
            Body2 = {transactions, [{transactionReply, TR2}]},
            megaco_messenger_misc:send_body(CD, TraceLabel, Body2),
	    Error
    end.

format_encode_error_reason(Reason) ->
    FS = 
	case Reason of
	    {Mod, Func, [EC, Msg], {AE, CS}} when is_atom(Mod)  and 
						  is_atom(Func) and
						  is_list(EC)   and
						  is_tuple(Msg) and
						  is_list(CS) ->
		io_lib:format("~n   Encode module: ~w"
			      "~n   Func:          ~w"
			      "~n   Encode config: ~w"
			      "~n   Message part:  ~p"
			      "~n   Actual error:  ~p" 
			      "~n   Call stack:    ~w", 
			      [Mod, Func, EC, Msg, AE, CS]);

	    {Mod, Func, [EC, Msg], AE} when is_atom(Mod)  and 
					    is_atom(Func) and
					    is_list(EC)   and
					    is_tuple(Msg) ->
		io_lib:format("~n   Encode module: ~w"
			      "~n   Func:          ~w"
			      "~n   Encode config: ~w"
			      "~n   Message part:  ~p"
			      "~n   Actual error:  ~p", 
			      [Mod, Func, EC, Msg, AE]);

	    {Mod, [EC, Msg], {AE, CS}} when is_atom(Mod)  and 
					    is_list(EC)   and
					    is_tuple(Msg) and
					    is_list(CS) ->
		io_lib:format("~n   Encode module: ~w"
			      "~n   Encode config: ~w"
			      "~n   Message part:  ~p"
			      "~n   Actual error:  ~p" 
			      "~n   Call stack:    ~w", 
			      [Mod, EC, Msg, AE, CS]);

	    {Mod, [EC, Msg], AE} when is_atom(Mod)  and 
				      is_list(EC)   and
				      is_tuple(Msg) ->
		io_lib:format("~n   Encode module: ~w"
			      "~n   Encode config: ~w"
			      "~n   Message part:  ~p"
			      "~n   Actual error:  ~p", 
			      [Mod, EC, Msg, AE]);

	    Error ->
		io_lib:format("~n   ~w", [Error])
	end,
    lists:flatten(FS).

			  
%% Presumably the user would return immediately (with {pending, Data}) if it 
%% knows or suspects a request to take a long time to process. 
%% For this reason we assume that handling a resent request 
%% could not have caused an update of the pending limit counter.
maybe_send_pending(#conn_data{sent_pending_limit = Limit} = ConnData, 
		   TransId) ->	    
    case check_and_maybe_incr_pending_limit(Limit, sent, TransId) of
	ok ->
	    send_pending(ConnData);
	error ->
	    SendReply = send_pending_limit_error(ConnData),
	    {aborted, SendReply};
	aborted ->
	    {aborted, ignore}
    end.


send_pending(#conn_data{serial       = Serial,
			trans_req    = true,
			trans_sender = Pid}) ->
    megaco_trans_sender:send_pending(Pid, Serial);
send_pending(#conn_data{serial = Serial} = CD) ->
    %% Encapsule the transaction result into a pending message
    TP = #'TransactionPending'{transactionId = Serial},
    Body = {transactions, [{transactionPending, TP}]},
    megaco_messenger_misc:send_body(CD, "send trans pending", Body).
	    

maybe_send_ack('NULL', #conn_data{serial       = Serial,
				  trans_ack    = true,
				  trans_sender = Pid}) ->
    megaco_trans_sender:send_ack_now(Pid, Serial);
maybe_send_ack('NULL', CD) ->
    send_ack(CD);
maybe_send_ack(_, #conn_data{auto_ack = false}) ->
    ignore;
maybe_send_ack(_, #conn_data{serial       = Serial,
			     trans_ack    = true,
			     trans_sender = Pid}) 
  when is_pid(Pid) ->
    %% Send (later) via the transaction sender
    megaco_trans_sender:send_ack(Pid, Serial),
    ok;
maybe_send_ack(_, CD) ->
    %% Send now
    send_ack(CD).


send_ack(#conn_data{serial = Serial} = CD) ->
    %% Encapsule the transaction result into a ack message
    TRA = #'TransactionAck'{firstAck = Serial},
    Body = {transactions, [{transactionResponseAck, [TRA]}]},
    megaco_messenger_misc:send_body(CD, "send trans ack", Body).


send_pending_limit_error(ConnData) ->
    ?report_pending_limit_exceeded(ConnData),
    Code   = ?megaco_number_of_transactionpending_exceeded,
    Reason = "Pending limit exceeded",
    send_trans_error(ConnData, Code, Reason).
    
send_trans_error(ConnData, Code, Reason) ->
    %% Encapsulate the transaction error into a reply message
    ED     = #'ErrorDescriptor'{errorCode = Code, errorText = Reason},
    Serial = ConnData#conn_data.serial,
    TR     = #'TransactionReply'{transactionId     = Serial,
				 transactionResult = {transactionError, ED}},
    Body   = {transactions, [{transactionReply, TR}]},
    megaco_messenger_misc:send_body(ConnData, "send trans error", Body).


send_message_error(ConnData, Code, Reason) ->
    ED = #'ErrorDescriptor'{errorCode = Code, errorText = Reason},
    Body = {messageError, ED},
    case megaco_messenger_misc:send_body(ConnData, "send trans error", Body) of
	{error, Reason} ->
	    ?report_important(ConnData, 
			      "<ERROR> failed sending message error",
			      [Body, {error, Reason}]),
	    error;
	_ ->
	    ok
    end.
	    

cancel(ConnHandle, Reason) when is_record(ConnHandle, megaco_conn_handle) ->
    case megaco_config:lookup_local_conn(ConnHandle) of
        [CD] ->
	    megaco_config:update_conn_info(CD, cancel, true),
	    do_cancel(ConnHandle, Reason, CD#conn_data{cancel = true}),
	    megaco_config:update_conn_info(CD, cancel, false),
	    ok;
        [] ->
	    ConnData = fake_conn_data(ConnHandle),
	    do_cancel(ConnHandle, Reason, ConnData)
    end.

do_cancel(ConnHandle, Reason, ConnData) ->
    ?report_trace(ConnData, "cancel", [ConnHandle, Reason]),
    LocalMid      = ConnHandle#megaco_conn_handle.local_mid,
    RemoteMid     = ConnHandle#megaco_conn_handle.remote_mid,
    ReqTransIdPat = #trans_id{mid = LocalMid, _ = '_'}, 
    ReqPat = #request{trans_id          = ReqTransIdPat,
		      remote_mid        = RemoteMid,
		      _                 = '_'},
    CancelReq = fun(Req) ->
			cancel_request(ConnData, Req, Reason),
			{_Type, Ref} = Req#request.timer_ref,  %% OTP-4843
			megaco_monitor:cancel_apply_after(Ref)
		end,
    Requests  = megaco_monitor:match_requests(ReqPat),
    lists:foreach(CancelReq, Requests),
    RemoteMid = ConnHandle#megaco_conn_handle.remote_mid,
    RepTransIdPat = #trans_id{mid = RemoteMid, _ = '_'}, % BUGBUG List here?
    RepPat = #reply{trans_id  = RepTransIdPat,
		    local_mid = LocalMid,
		    _         = '_'},
    CancelRep = fun(Rep) -> 
			cancel_reply(ConnData, Rep, Reason) 
		end,
    Replies   = megaco_monitor:match_replies(RepPat),
    lists:foreach(CancelRep, Replies),
    ok.

cancel_requests(_ConnData, [], _Reason)  ->
    ok;
cancel_requests(ConnData, [{transactionRequest,TR}|TRs], Reason) ->
    #'TransactionRequest'{transactionId = TransId0} = TR,
    TransId = to_local_trans_id(ConnData#conn_data.conn_handle, TransId0),
    case megaco_monitor:lookup_request(TransId) of
	[] ->
	    ignore;
	[Req] when is_record(Req, request) ->
	    cancel_request(ConnData, Req, Reason)
    end,
    cancel_requests(ConnData, TRs, Reason).

cancel_request(ConnData, Req, Reason)  ->
    ?report_trace(ignore, "cancel request", [Req]),
    ?TC_AWAIT_CANCEL_EVENT(),
    TransId   = Req#request.trans_id,
    Version   = Req#request.version,
    UserMod   = Req#request.user_mod,
    UserArgs  = Req#request.user_args,
    Action    = Req#request.reply_action,
    UserData  = Req#request.reply_data,
    UserReply = {error, Reason},
    ConnData2 = ConnData#conn_data{protocol_version = Version,
				   user_mod         = UserMod,
				   user_args        = UserArgs,
				   reply_action     = Action,
				   reply_data       = UserData},
    cancel_request2(ConnData2, TransId, UserReply).

cancel_request2(ConnData, TransId, UserReply) ->
    megaco_monitor:delete_request(TransId),
    Serial    = TransId#trans_id.serial,
    ConnData2 = ConnData#conn_data{serial = Serial},
    return_reply(ConnData2, TransId, UserReply).
    

return_reply(ConnData, TransId, UserReply) ->
    ?report_trace(ConnData, "callback: trans reply", [UserReply]),
    Version  = ConnData#conn_data.protocol_version,
    UserData = ConnData#conn_data.reply_data,
    case ConnData#conn_data.reply_action of
        call when is_pid(UserData) ->
	    ?report_trace(ConnData, "callback: (call) trans reply", 
			  [UserReply]),
            Pid = UserData,
            Pid ! {?MODULE, TransId, Version, UserReply};
        cast ->
	    ?report_trace(ConnData, "callback: (cast) trans reply", [UserReply]),
	    UserMod    = ConnData#conn_data.user_mod,
	    UserArgs   = ConnData#conn_data.user_args,
            ConnHandle = ConnData#conn_data.conn_handle,
            Res = (catch apply(UserMod, handle_trans_reply, 
			       [ConnHandle, Version, UserReply, 
				UserData | UserArgs])),
	    ?report_debug(ConnData, "return: (cast) trans reply",
			  [UserReply, {return, Res}]),
	    case Res of
		ok ->
		    ok;
		_ ->
		    warning_msg("transaction reply callback failed: ~w", 
				[Res]),
		    ok
	    end,
	    Res;
        remote ->
	    ?report_trace(ConnData, "callback: (remote) trans reply", [UserReply]),
            Node = UserData,
            Args = [ConnData, UserReply],
            rpc:cast(Node, ?MODULE, receive_reply_remote, Args)
    end.

receive_reply_remote(ConnData, UserReply) ->
    TransId = to_local_trans_id(ConnData),
    case (catch megaco_monitor:lookup_request(TransId)) of
        [#request{timer_ref = {_Type, Ref}} = Req] -> %% OTP-4843
            %% Don't care about Req and Rep version diff
	    megaco_monitor:delete_request(TransId),
	    megaco_monitor:cancel_apply_after(Ref), %% OTP-4843
	
	    UserMod   = Req#request.user_mod,
	    UserArgs  = Req#request.user_args,
	    Action    = Req#request.reply_action,
	    UserData  = Req#request.reply_data,
	    ConnData2 = ConnData#conn_data{user_mod     = UserMod,
					   user_args    = UserArgs,
					   reply_action = Action,
					   reply_data   = UserData},
	    return_reply(ConnData2, TransId, UserReply);
		
	_ ->
	    ?report_trace(ConnData, "remote reply (no receiver)", 
			  [UserReply]),
	    return_unexpected_trans_reply(ConnData, TransId, UserReply)
    end.

cancel_reply(ConnData, #reply{state = waiting_for_ack} = Rep, Reason) ->
    ?report_trace(ignore, "cancel reply [waiting_for_ack]", [Rep]),
    megaco_monitor:cancel_apply_after(Rep#reply.pending_timer_ref),
    Serial = (Rep#reply.trans_id)#trans_id.serial,
    ConnData2 = ConnData#conn_data{serial = Serial},
    T = #'TransactionAck'{firstAck = Serial},
    handle_ack(ConnData2, {error, Reason}, Rep, T);

cancel_reply(_ConnData, #reply{state = aborted} = Rep, _Reason) ->
    ?report_trace(ignore, "cancel reply [aborted]", [Rep]),
    #reply{trans_id          = TransId,
	   timer_ref         = ReplyRef,
	   pending_timer_ref = PendingRef} = Rep,
    megaco_monitor:delete_reply(TransId),
    megaco_monitor:cancel_apply_after(ReplyRef),
    megaco_monitor:cancel_apply_after(PendingRef), % BMK BMK Still running?
    megaco_config:del_pending_counter(TransId),    % BMK BMK Still existing?
    ok;

cancel_reply(_ConnData, Rep, ignore) ->
    ?report_trace(ignore, "cancel reply [ignore]", [Rep]),
    #reply{trans_id          = TransId,
	   timer_ref         = ReplyRef,
	   pending_timer_ref = PendingRef} = Rep,
    megaco_monitor:delete_reply(TransId),
    megaco_monitor:cancel_apply_after(ReplyRef),
    megaco_monitor:cancel_apply_after(PendingRef), % BMK BMK Still running?
    megaco_config:del_pending_counter(TransId),    % BMK BMK Still existing?
    ok;

cancel_reply(_CD, _Rep, _Reason) ->
%%     io:format("cancel_reply -> entry with"
%% 	      "~n   CD:     ~p"
%% 	      "~n   Rep:    ~p"
%% 	      "~n   Reason: ~p"
%% 	      "~n", [_CD, _Rep, _Reason]),
    ok.


request_timeout(ConnHandle, TransId) ->
    ?rt1(ConnHandle, "request timeout", [TransId]),
    case megaco_monitor:lookup_request(TransId) of
	[] ->
	    ignore;
	[Req] when is_record(Req, request) ->
	    case megaco_config:lookup_local_conn(ConnHandle) of
		[ConnData] ->
 		    incNumTimerRecovery(ConnHandle),
 		    do_request_timeout(ConnHandle, TransId, ConnData, Req);
		[] when ConnHandle#megaco_conn_handle.remote_mid == preliminary_mid ->
 		    %% There are two possibillities:
		    %% 1) The connection has just been upgraded from a 
 		    %%    preliminary to a real connection. So this timeout
 		    %%    is just a glitch. E.g. between the removel of this
 		    %%    ConnHandle and the timer.
		    %% 2) The first message sent, the service-change, got no 
		    %%    reply (UDP without three-way-handshake).
		    %%    And then the other side (MGC) sends a request,
		    %%    which causes an auto-upgrade
 		    request_timeout_upgraded(ConnHandle, Req);
		[] ->
 		    incNumTimerRecovery(ConnHandle),
 		    ConnData = fake_conn_data(ConnHandle),
 		    do_request_timeout(ConnHandle, TransId, ConnData, Req)
	    end
    end.

request_timeout_upgraded(ConnHandle, Req) ->
    CD = fake_conn_data(ConnHandle),
    cancel_request(CD, Req, timeout).
    
do_request_timeout(ConnHandle, TransId, ConnData, 
		   #request{curr_timer = CurrTimer} = Req) ->

    SendHandle = Req#request.send_handle,
    Version    = Req#request.version,
    ConnData2  = ConnData#conn_data{send_handle      = SendHandle,
				    protocol_version = Version},
    case CurrTimer of
	timeout ->  %%%%%%%
	    cancel_request(ConnData2, Req, timeout);

	%% Restartable timer 
	%% (max_retries = infinity_restartable)
	{_, timeout} ->  
	    cancel_request(ConnData2, Req, timeout);

	Timer ->
	    {SendOrNoSend, Data} = Req#request.bytes,
	    case SendOrNoSend of
		send ->
		    case maybe_encode(ConnData2, Data) of
			{ok, Bin} ->
			    ?report_trace(ConnData2, "re-send trans request", 
					  [{bytes, Bin}]),
			    case maybe_send_message(ConnData2, Bin) of
				ok ->
				    ignore;
				{ok, _} ->
				    ignore;
				{error, Reason} ->
				    ?report_important(ConnData2, 
						      "<ERROR> "
						      "re-send trans "
						      "request failed",
						      [{bytes, Bin}, 
						       {error, Reason}])
			    end;

			{error, Reason} ->
			    %% Since it was possible to encode the original
			    %% message this should really never happen...
			    ?report_important(ConnData2, 
					      "<ERROR> "
					      "re-send trans request failed",
					      [{transaction,
						Req#request.bytes}, 
					       {error, Reason}])
		    end;
		no_send ->
		    ok
	    end,
	    {WaitFor, Timer2} = recalc_timer(Timer),
	    OptBin            = opt_garb_binary(Timer2, Data),
	    {Type, _}         = Req#request.timer_ref,
	    M = ?MODULE,
	    F = request_timeout,
	    A = [ConnHandle, TransId],
	    Ref2 = megaco_monitor:apply_after(M, F, A, WaitFor),
	    Req2 = Req#request{bytes      = {SendOrNoSend, OptBin},
			       timer_ref  = {Type, Ref2},
			       curr_timer = Timer2},
	    megaco_monitor:insert_request(Req2) % Timing problem

    end.

maybe_encode(#conn_data{trans_req = false} = CD, {_Serial, Bin}) 
  when is_binary(Bin) ->
    Body = {transactions, [{transactionRequest, Bin}]},
    megaco_messenger_misc:encode_body(CD, "encode trans request msg", Body);
maybe_encode(_CD, {_Serial, Bin} = D) when is_binary(Bin) ->
    {ok, D};
maybe_encode(#conn_data{trans_req    = true,
			trans_sender = Pid} = CD, 
	     #'TransactionRequest'{transactionId = Serial} = TR) 
  when is_pid(Pid) ->
    case megaco_messenger_misc:encode_trans_request(CD, TR) of
	{ok, Bin} ->
	    {ok, {Serial, Bin}};
	Error ->
	    Error
    end;
maybe_encode(CD, TR) 
  when is_record(TR, 'TransactionRequest') ->
    Body = {transactions, [{transactionRequest, TR}]},
    megaco_messenger_misc:encode_body(CD, "encode trans request msg", Body);
maybe_encode(_CD, Trash) ->
    {error, {invalid_bin, Trash}}.

maybe_send_message(CD, Bin) when is_binary(Bin) ->
    megaco_messenger_misc:send_message(CD, Bin);
maybe_send_message(#conn_data{trans_sender = Pid}, {Serial, Bin}) 
  when is_pid(Pid) and is_integer(Serial) and is_binary(Bin) ->
    megaco_trans_sender:send_req(Pid, Serial, Bin).

    
reply_timeout(ConnHandle, TransId, timeout) ->
    handle_reply_timer_timeout(ConnHandle, TransId);

%% This means that infinity_restartable was used for max_retries.
%% There is currently no reason to use this for the reply_timeout,
%% since there is no external event to restart the timer!
reply_timeout(ConnHandle, TransId, {_, timeout}) ->
    handle_reply_timer_timeout(ConnHandle, TransId);

reply_timeout(ConnHandle, TransId, Timer) ->
    ?report_trace(ConnHandle, "reply timeout", [Timer, TransId]),

%%     d("reply_timeout -> entry with"
%%       "~n   ConnHandle: ~p"
%%       "~n   TransId:    ~p"
%%       "~n   Timer:      ~p", [ConnHandle, TransId, Timer]),

    case megaco_monitor:lookup_reply(TransId) of
	[] ->
	    ignore; % Trace ??

	[#reply{state      = waiting_for_ack, 
		ack_action = {handle_ack, _}} = Rep] ->
	    case megaco_config:lookup_local_conn(ConnHandle) of
		[ConnData] ->
 		    incNumTimerRecovery(ConnHandle),
 		    do_reply_timeout(ConnHandle, TransId, ConnData, 
				     Timer, Rep);
		[] ->
 		    incNumTimerRecovery(ConnHandle),
 		    ConnData = fake_conn_data(ConnHandle),
 		    do_reply_timeout(ConnHandle, TransId, ConnData, 
				     Timer, Rep)
	    end;

	[#reply{state = waiting_for_ack} = Rep] ->
	    do_reply_timeout(ConnHandle, TransId, Timer, Rep);
		    
	[#reply{state = aborted} = Rep] ->
	    do_reply_timeout(ConnHandle, TransId, Timer, Rep);
		    
	_ ->
	    ignore
		
    end.

do_reply_timeout(ConnHandle, TransId, ConnData, Timer, 
		 #reply{send_handle = SH,
			version     = V,
			bytes       = Bytes} = Rep) ->

    CD = ConnData#conn_data{send_handle      = SH,
			    protocol_version = V},
    
%%     d("do_reply_timeout -> entry with"
%%       "~n   ConnHandle: ~p"
%%       "~n   TransId:    ~p"
%%       "~n   Timer:      ~p"
%%       "~n   Rep:        ~p"
%%       "~n", [ConnHandle, TransId, Timer, Rep]),

    case megaco_messenger_misc:send_message(CD, Bytes) of
	ok ->
	    ignore;
	{ok, _} ->
	    ignore;
	{error, Reason} ->
	    ?report_important(CD, "<ERROR> re-send trans reply failed",
			      [{bytes, Bytes}, {error, Reason}])
    end,
    do_reply_timeout(ConnHandle, TransId, Timer, Rep).

do_reply_timeout(ConnHandle, TransId, Timer, #reply{bytes = Bytes} = Rep) ->
    {WaitFor, Timer2} = recalc_timer(Timer),
    OptBin = opt_garb_binary(Timer2, Bytes),
    M = ?MODULE,
    F = reply_timeout,
    A = [ConnHandle, TransId, Timer2],
    Ref2 = megaco_monitor:apply_after(M, F, A, WaitFor),
    Rep2 = Rep#reply{bytes     = OptBin,
		     timer_ref = Ref2},
    megaco_monitor:insert_reply(Rep2). % Timing problem?

    
handle_reply_timer_timeout(ConnHandle, TransId) ->
    ?report_trace(ConnHandle, "reply timeout", [timeout,TransId]),
    incNumTimerRecovery(ConnHandle),
    %% OTP-4378
    case megaco_monitor:lookup_reply(TransId) of
	[#reply{state = waiting_for_ack} = Rep] ->
	    Serial = (Rep#reply.trans_id)#trans_id.serial,
	    ConnData = 
		case megaco_config:lookup_local_conn(ConnHandle) of
		    [ConnData0] ->
			ConnData0;
		    [] ->
			fake_conn_data(ConnHandle)
		end,
	    ConnData2 = ConnData#conn_data{serial = Serial},
	    T = #'TransactionAck'{firstAck = Serial},
	    handle_ack(ConnData2, {error, timeout}, Rep, T);
	[#reply{pending_timer_ref = Ref}] -> % aborted?
	    megaco_monitor:cancel_apply_after(Ref),
	    megaco_monitor:delete_reply(TransId),
	    megaco_config:del_pending_counter(sent, TransId)
    end.
    
pending_timeout(ConnData, TransId, Timer) ->
    ?report_trace(ConnData, "pending timeout", [Timer, TransId]),
    case megaco_monitor:lookup_reply(TransId) of
	[#reply{state   = State,
		handler = Pid} = Rep] when State == prepare; 
					   State == eval_request ->

	    #conn_data{sent_pending_limit = Limit,
		       conn_handle        = ConnHandle} = ConnData,

	    %% ------------------------------------------
	    %% 
	    %%   Check pending limit
	    %% 
	    %% ------------------------------------------

	    case check_and_maybe_incr_pending_limit(Limit, sent, TransId) of
		ok ->

		    %% ---------------------------------------------
		    %% 
		    %%   1) Send pending message
		    %%   2) Possibly restart the pending timer
		    %% 
		    %% ---------------------------------------------

		    send_pending(ConnData),
		    case Timer of
			timeout ->
			    %% We are done
			    incNumTimerRecovery(ConnHandle),
			    timeout_out1;
			{_, timeout} ->
			    %% We are done
			    incNumTimerRecovery(ConnHandle),
			    timeout_out2;
			_ ->
			    {WaitFor, Timer2} = recalc_timer(Timer),
			    M = ?MODULE,
			    F = pending_timeout,
			    A = [ConnData, TransId, Timer2],
			    PendingRef = 
				megaco_monitor:apply_after(M, F, A, WaitFor),
			    Rep2 = Rep#reply{pending_timer_ref = PendingRef},
			    %% Timing problem?
			    megaco_monitor:insert_reply(Rep2),
			    {restarted, WaitFor, Timer2}
		    end;


		error ->

		    %% ------------------------------------------
		    %% 
		    %%   1) Send 506 error message to other side
		    %%   2) Notify user
		    %%   3) Set reply data in aborted state
		    %% 
		    %% -------------------------------------------

		    send_pending_limit_error(ConnData),
		    handle_request_abort_callback(ConnData, TransId, Pid),
		    %% Timing problem?
		    Rep2 = Rep#reply{state = aborted},
		    %% megaco_monitor:insert_reply(Rep2);
		    cancel_reply(ConnData, Rep2, aborted),
		    pending_limit_error;


		aborted ->

		    %% ------------------------------------------
		    %% 
		    %%   Pending limit already passed 
		    %% 
		    %% -------------------------------------------
		    Rep2 = Rep#reply{state = aborted},
		    cancel_reply(ConnData, Rep2, aborted),
		    pending_limit_aborted

	    end;
	[] ->
	    reply_not_found; % Trace ??

	[#reply{state = waiting_for_ack}] ->
	    %% The reply has already been sent
	    %% No need for any pending trans reply
	    reply_has_been_sent;

	[#reply{state = aborted} = Rep] ->
	    %% glitch, but cleanup just the same
	    cancel_reply(ConnData, Rep, aborted),
	    reply_aborted_state

    end.


return_unexpected_trans_reply(ConnData, TransId, UserReply) ->
    Trans = #'TransactionReply'{transactionId     = TransId,
				transactionResult = UserReply},
    return_unexpected_trans(ConnData, Trans).

return_unexpected_trans(ConnData, Trans) ->
    UserMod    = ConnData#conn_data.user_mod,
    UserArgs   = ConnData#conn_data.user_args,
    ConnHandle = ConnData#conn_data.conn_handle,
    Version    = ConnData#conn_data.protocol_version,
    Res = (catch apply(UserMod, handle_unexpected_trans, 
		       [ConnHandle, Version, Trans | UserArgs])),
    ?report_debug(ConnData, "return: unexpected trans", 
		  [Trans, {return, Res}]),
    case Res of
	ok ->
	    ok;
	_ ->
	    warning_msg("unexpected transaction callback failed: ~w", [Res]),
	    ok
    end,
    Res.
    


to_remote_trans_id(#conn_data{conn_handle = CH, serial = Serial}) ->
    Mid = CH#megaco_conn_handle.remote_mid,
    #trans_id{mid = Mid, serial = Serial}.

to_local_trans_id(#conn_data{conn_handle = CH, serial = Serial}) ->
    Mid = CH#megaco_conn_handle.local_mid,
    #trans_id{mid = Mid, serial = Serial}.

to_local_trans_id(#conn_data{conn_handle = CH}, [S|_] = Serials) 
  when is_integer(S) ->
    Mid = CH#megaco_conn_handle.local_mid, 
    [#trans_id{mid = Mid, serial = Serial} || Serial <- Serials];
to_local_trans_id(#conn_data{conn_handle = CH}, 
		  [{transactionRequest, TR}|_] = TRs) 
  when is_record(TR, 'TransactionRequest') ->
    Mid = CH#megaco_conn_handle.local_mid, 
    [#trans_id{mid = Mid, serial = Serial} || 
	{transactionRequest, 
	 #'TransactionRequest'{transactionId = Serial}} <- TRs];

to_local_trans_id(#megaco_conn_handle{local_mid = Mid}, Serial) 
  when is_integer(Serial) ->
    #trans_id{mid = Mid, serial = Serial};
to_local_trans_id(#conn_data{conn_handle = CH}, Serial) 
  when is_integer(Serial) ->
    Mid = CH#megaco_conn_handle.local_mid, 
    #trans_id{mid = Mid, serial = Serial}.    
    

%% Returns {WaitFor, NewTimer} | {WaitFor, timeout}
init_timer(SingleWaitFor) when SingleWaitFor == infinity ->
    {SingleWaitFor, timeout};
init_timer(SingleWaitFor) when is_integer(SingleWaitFor) ->
    {SingleWaitFor, timeout};
init_timer(Timer) when is_record(Timer, megaco_incr_timer) ->
    return_incr(Timer).

return_incr(Timer) ->
    WaitFor = Timer#megaco_incr_timer.wait_for,
    case Timer#megaco_incr_timer.max_retries of
	infinity ->
	    {WaitFor, Timer};
	infinity_restartable ->
	    {WaitFor, {Timer, timeout}};
	Int when is_integer(Int), Int > 0 -> 
	    {WaitFor, Timer};
	0  ->
	    {WaitFor, timeout}
    end.

%% Returns {WaitFor, NewTimer} | {WaitFor, timeout}
recalc_timer(Timer) when is_record(Timer, megaco_incr_timer) ->
    Old     = Timer#megaco_incr_timer.wait_for,
    Factor  = Timer#megaco_incr_timer.factor,
    Incr    = Timer#megaco_incr_timer.incr,
    New     = (Old * Factor) + Incr,
    Max     = decr(Timer#megaco_incr_timer.max_retries),
    Timer2  = Timer#megaco_incr_timer{wait_for    = New,
                                      max_retries = Max},
    return_incr(Timer2);
recalc_timer({Timer, timeout}) when is_record(Timer, megaco_incr_timer) ->
    recalc_timer(Timer).

decr(infinity = V)             -> V;
decr(infinity_restartable = V) -> V;
decr(Int) when is_integer(Int) -> Int - 1.


warning_msg(F, A) ->
    ?megaco_warning(F, A).

error_msg(F, A) ->
    ?megaco_error(F, A).


%% d(F) ->
%%     d(F,[]).

%% d(F,A) ->
%%     d(true,F,A).
%%     %% d(get(dbg),F,A).

%% d(true,F,A) ->
%%     io:format("*** [~s] ~p:~p ***"
%% 	      "~n   " ++ F ++ "~n", 
%% 	      [format_timestamp(now()), self(),?MODULE|A]);
%% d(_, _, _) ->
%%     ok.

%% format_timestamp({_N1, _N2, N3} = Now) ->
%%     {Date, Time}   = calendar:now_to_datetime(Now),
%%     {YYYY,MM,DD}   = Date,
%%     {Hour,Min,Sec} = Time,
%%     FormatDate = 
%%         io_lib:format("~.4w:~.2.0w:~.2.0w ~.2.0w:~.2.0w:~.2.0w 4~w",
%%                       [YYYY,MM,DD,Hour,Min,Sec,round(N3/1000)]),  
%%     lists:flatten(FormatDate).

	      
%%-----------------------------------------------------------------
%% Func: incNumErrors/0, incNumErrors/1, incNumTimerRecovery/1
%% Description: SNMP counter increment functions
%%-----------------------------------------------------------------
incNumErrors() ->
    incNum(medGwyGatewayNumErrors).

incNumErrors(CH) ->
    incNum({CH, medGwyGatewayNumErrors}).

incNumTimerRecovery(CH) ->
    incNum({CH, medGwyGatewayNumTimerRecovery}).

incNum(Cnt) ->
    case (catch ets:update_counter(megaco_stats, Cnt, 1)) of
	{'EXIT', {badarg, _Reason}} ->
	    ets:insert(megaco_stats, {Cnt, 1});
	Old ->
	    Old
    end.
	    
