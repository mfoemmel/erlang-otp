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
%% 
%% Future: 1) Instead of encoding and storing the entire message, encode
%%            and store each individual transaction (bin's). Whenever a
%%            reply is received for a transaction, that transactions bin
%%            is removed. When a resend is to be done, a new message is
%%            encoded with the transaction (bin's).
%%         2) Piggy-back ack's with other messages sent (transaction 
%%            requests).
%%----------------------------------------------------------------------

-module(megaco_messenger).

%% Application internal export
-export([
         process_received_message/4,
         receive_message/4,
         connect/4,
         disconnect/2,
         call/3,
         cast/3,
         cancel/2,
         request_timeout/2,
         pending_timeout/2,
         reply_timeout/3% ,
% 	 send_ack/2
        ]).

-export([
	 get_stats/0, get_stats/1, get_stats/2,
	 reset_stats/0, reset_stats/1
	]).

%% Module internal export
-export([
	 process_received_message/5,
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

%% This record contain the info needed when handling 
%% piggybacking of message ack's.
-record(accu_request,
	{conn_handle,
	 timer,
	 trans_ids = []
	}).

%% N.B. Update cancel/1 with '_' when a new field is added
-record(reply,
        {
          trans_id,
	  local_mid,
          state,            % eval_request | waiting_for_ack
          pending_timer_ref,
          timer_ref,
          version,
          bytes,
          ack_action        % discard_ack | {handle_ack, Data}
         }).
-record(trans_id,
        {
          mid,
          serial
         }).


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
%% Register/unreister connections
%%----------------------------------------------------------------------

%% Returns {ok, ConnHandle} | {error, Reason}
connect(RH, RemoteMid, SendHandle, ControlPid)
  when record(RH, megaco_receive_handle) ->
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
           monitor_process(CH, CD#conn_data.control_pid);
        {error, ED} when record(ED,'ErrorDescriptor') ->
            megaco_config:disconnect(CH),
            {error, {connection_refused, CD, ED}};
        Error ->
            megaco_config:disconnect(CH),
            {error, {connection_refused, CD, Error}}
    end.



monitor_process(CH, ControlPid) when node(ControlPid) == node() ->
    M = ?MODULE,
    F = disconnect_local,
    A = [CH],
    Ref = megaco_monitor:apply_at_exit(M, F, A, ControlPid),
    case megaco_config:update_conn_info(CH, monitor_ref, Ref) of
        ok ->
            {ok, CH};
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
                    {ok, CH};
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
		    {ok, ControlMonitorPid};
		{error, Reason} ->
		    {error, Reason}
	    end;
        [] ->
            {error, {no_connection, CH}}
    end.

disconnect(ConnHandle, DiscoReason)
  when record(ConnHandle, megaco_conn_handle) ->
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
        {ok, ConnData, MegaMsg} when record(MegaMsg, 'MegacoMessage') ->
            Mess = MegaMsg#'MegacoMessage'.mess,
            case Mess#'Message'.messageBody of
                {transactions, Transactions} ->
                    {AckList, ReqList} = 
			prepare_trans(ConnData, Transactions, [], []),
                    handle_acks(AckList),
                    case handle_requests(ReqList, []) of
                        [] ->
                            ignore;
                        [LongRequest | More] ->
                            [spawn(?MODULE, handle_long_request, [LR])
                             || LR <- More],
                            handle_long_request(LongRequest)
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
  when record(RH, megaco_receive_handle), pid(Pid) ->
    ?report_trace(RH, "receive bytes", [{bytes, Bin}]),
    EncodingMod    = RH#megaco_receive_handle.encoding_mod,
    EncodingConfig = RH#megaco_receive_handle.encoding_config,
    case (catch EncodingMod:decode_message(EncodingConfig, Bin)) of
        {ok, MegaMsg} when record(MegaMsg, 'MegacoMessage') ->
	    ?report_trace(RH, "receive message", [{message, MegaMsg}]),
            Mess       = MegaMsg#'MegacoMessage'.mess,
            RemoteMid  = Mess#'Message'.mId,
            LocalMid   = RH#megaco_receive_handle.local_mid,
            CH         = #megaco_conn_handle{local_mid  = LocalMid,
                                             remote_mid = RemoteMid},
            case megaco_config:lookup_local_conn(CH) of
                [ConnData] ->
                    %% Use already established connection
                    ConnData2 = ConnData#conn_data{send_handle = SH},
                    check_message_auth(CH, ConnData2, MegaMsg, Bin);
                [] ->
                    %% Setup a temporary connection
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
			    Error    = prepare_error({error, Reason}),
                            {verbose_fail, ConnData, Error}
                    end
            end;
        Error ->
	    incNumErrors(),
            ConnData = fake_conn_data(RH, SH, Pid),
            handle_syntax_error_callback(RH, ConnData, prepare_error(Error))
    end;
prepare_message(RH, SendHandle, _Bin, ControlPid) ->
    ConnData = fake_conn_data(RH, SendHandle, ControlPid),
    Error    = prepare_error({'EXIT', {bad_receive_handle, RH}}),
    {verbose_fail, ConnData, Error}.

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
    ?report_trace(ConnData2, "receive bytes", [{bytes, Bin}]),
    if
	MsgAuth == asn1_NOVALUE, ConnAuth == asn1_NOVALUE ->
            {ok, ConnData2, MegaMsg};
	true -> 
	    ED = #'ErrorDescriptor'{errorCode = ?megaco_unauthorized,
				    errorText = "Autentication is not supported"},
	    {verbose_fail, ConnData2, prepare_error({error, ED})}
    end.

handle_syntax_error_callback(ReceiveHandle, ConnData, PrepError) ->
    {Code, Reason, Error} = PrepError,
    ErrorDesc = #'ErrorDescriptor'{errorCode = Code, errorText = Reason},
    Version   = ConnData#conn_data.protocol_version,
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
            {silent_fail, ConnData, {Code2,Reason2,Error}}; %%% OTP-XXXX
        _Bad ->
            {verbose_fail, ConnData, PrepError}
    end.

fake_conn_data(CH) when record(CH, megaco_conn_handle) ->
    case catch megaco_config:conn_info(CH, receive_handle) of
	RH when record(RH, megaco_receive_handle) ->
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
			       reply_action       = undefined};
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
		       reply_action       = undefined};
	ConnData ->
	    ConnData
    end.

prepare_error(Error) ->
    case Error of
        {error, ED} when record(ED, 'ErrorDescriptor') ->
            Code   = ED#'ErrorDescriptor'.errorCode,
            Reason = ED#'ErrorDescriptor'.errorText,
            {Code, Reason, Error};
        {error, [{reason, {bad_token, _}, Line}]} when integer(Line) ->
            Reason = lists:concat(["Illegal token on line ", Line]),
            Code = ?megaco_bad_request,
            {Code, Reason, Error};
        {error, [{reason, {Line, _, _}} | _]} when integer(Line) ->
            Reason = lists:concat(["Syntax error on line ", Line]),
            Code = ?megaco_bad_request,
            {Code, Reason, Error};
        {error, {connection_refused, ED}} when record(ED,'ErrorDescriptor') ->
            Code   = ED#'ErrorDescriptor'.errorCode,
            Reason = ED#'ErrorDescriptor'.errorText,
            {Code, Reason, Error};
        {error, {connection_refused, _}} ->
            Reason = "Connection refused by user",
            Code = ?megaco_unauthorized,
            {Code, Reason, Error};
        {error, _} ->
            Reason = "Syntax error",
            Code = ?megaco_bad_request,
            {Code, Reason, Error};
        {ok, MegaMsg} when record (MegaMsg, 'MegacoMessage') ->
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
        {transactionRequest, T} when record(T, 'TransactionRequest') ->
            Serial = T#'TransactionRequest'.transactionId,
            ConnData2 = ConnData#conn_data{serial = Serial},
	    ?report_trace(ConnData2, "Pending handle_connect", [T]),
	    send_pending(ConnData2),
	    prepare_trans(ConnData2, Rest, AckList, ReqList);
	_ ->
	    prepare_trans(ConnData, Rest, AckList, ReqList)
    end;
prepare_trans(ConnData, [Trans | Rest], AckList, ReqList) ->
    case Trans of
        {transactionRequest, #'TransactionRequest'{transactionId = asn1_NOVALUE}} ->
            ConnData2 = ConnData#conn_data{serial = 0},
	    Code   = ?megaco_bad_request,
            Reason = "Syntax error in message: transaction id missing",
	    send_trans_error(ConnData2, Code, Reason),
            prepare_trans(ConnData2, Rest, AckList, ReqList);
        {transactionRequest, T} when record(T, 'TransactionRequest') ->
            Serial = T#'TransactionRequest'.transactionId,
            ConnData2 = ConnData#conn_data{serial = Serial},
            prepare_request(ConnData2, T, Rest, AckList, ReqList);
        {transactionPending, T} when record(T, 'TransactionPending') ->
            Serial = T#'TransactionPending'.transactionId,
            ConnData2 = ConnData#conn_data{serial = Serial},
            handle_pending(ConnData2, T),
            prepare_trans(ConnData2, Rest, AckList, ReqList);
        {transactionReply, T} when record(T, 'TransactionReply') ->
            Serial = T#'TransactionReply'.transactionId,
            ConnData2 = ConnData#conn_data{serial = Serial},
            handle_reply(ConnData2, T),
	    prepare_trans(ConnData2, Rest, AckList, ReqList);
        {transactionResponseAck, List} when list(List) ->
            prepare_ack(ConnData, List, Rest, AckList, ReqList)

    end;
prepare_trans(_ConnData, [], AckList, ReqList) ->
    {AckList, ReqList}.

prepare_request(ConnData, T, Rest, AckList, ReqList) ->
    LocalMid = (ConnData#conn_data.conn_handle)#megaco_conn_handle.local_mid,
    TransId = to_remote_trans_id(ConnData),
    case megaco_monitor:lookup_reply(TransId) of
        [] ->
            %% Brand new request
	    M = ?MODULE,
	    F = pending_timeout,
	    A = [ConnData, TransId],
	    WaitFor = ConnData#conn_data.pending_timer,
	    PendingRef = megaco_monitor:apply_after(M, F, A, WaitFor),
            Version = ConnData#conn_data.protocol_version,
            Rep = #reply{trans_id = TransId,
			 local_mid = LocalMid,
			 state = eval_request,
			 pending_timer_ref = PendingRef,
			 version = Version},
            megaco_monitor:insert_reply(Rep),
            prepare_trans(ConnData, Rest, AckList, [{ConnData, Rep, T} | ReqList]);
        [Rep] when Rep#reply.state == eval_request ->
            %% We are still evaluating the request
            %% Don't care about Msg and Rep version diff
	    megaco_monitor:cancel_apply_after(Rep#reply.pending_timer_ref),
            send_pending(ConnData),
            prepare_trans(ConnData, Rest, AckList, ReqList);
        [Rep] when Rep#reply.state == waiting_for_ack ->
            %% We have already sent a reply, but the receiver
            %% has obviously not got it. Resend the reply but
            %% don't restart the reply_timer.
            Bin = Rep#reply.bytes,
            Version = Rep#reply.version,
            ConnData2 = ConnData#conn_data{protocol_version = Version},
            ?report_trace(ConnData2, "re-send trans reply", [T | {bytes, Bin}]),
            case megaco_messenger_misc:send_message(ConnData2, Bin) of
                {ok, _} ->
                    prepare_trans(ConnData2, Rest, AckList, ReqList);
                {error, Reason} ->
                    ?report_important(ConnData2, 
				      "<ERROR> re-send trans reply failed",
                                      [{bytes, Bin}, {error, Reason}]),
		    error_msg("re-send transaction reply failed: ~w", 
			      [Reason]),
                    prepare_trans(ConnData2, Rest, AckList, ReqList)
            end
        end.

prepare_ack(ConnData, [TA | T], Rest, AckList, ReqList) when record(TA, 'TransactionAck') ->
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


handle_requests([{ConnData, Rep, T} | Rest], Pending)
  when Rep#reply.state == eval_request ->
    Actions = T#'TransactionRequest'.actions,
    {AckAction, SendReply} = handle_request_callback(ConnData, Actions, T),
    megaco_monitor:cancel_apply_after(Rep#reply.pending_timer_ref),
    case AckAction of
        {pending, RequestData} ->
            handle_requests(Rest, [{ConnData, Rep, RequestData} | Pending]);
	_ ->
	    case SendReply of
		{ok, Bin} ->
		    Method = timer_method(AckAction),
		    InitTimer = ConnData#conn_data.reply_timer,
		    {WaitFor, CurrTimer} = init_timer(InitTimer),
		    OptBin = opt_garb_binary(CurrTimer, Bin),
		    ConnHandle = ConnData#conn_data.conn_handle,
		    M = ?MODULE,
		    F = reply_timeout,
		    A = [ConnHandle, Rep#reply.trans_id, CurrTimer],
		    Ref = megaco_monitor:apply_after(Method, 
						     M, F, A, WaitFor),
		    Rep2 = Rep#reply{bytes             = OptBin,
				     state             = waiting_for_ack,
				     timer_ref         = Ref,
				     ack_action        = AckAction},
		    megaco_monitor:insert_reply(Rep2), % Timing problem?
		    handle_requests(Rest, Pending);
		{error, Reason} ->
		    ?report_trace(ConnData, "send trans reply",
				  [T, {error, Reason}]),
		    handle_requests(Rest, Pending)
	    end
    end;
handle_requests([], Pending) ->
    Pending.

%%opt_garb_binary(timeout, Bin) -> garb_binary; % Need msg at restart of timer
opt_garb_binary(_Timer, Bin)   -> Bin.

timer_method(discard_ack) ->
    apply_method;
timer_method(_) ->
    spawn_method.


handle_long_request({ConnData, Rep, RequestData}) ->
    ?report_trace(ConnData, "callback: trans long request",
		  [Rep, {request_data, RequestData}]),
    {AckAction, Res} = handle_long_request_callback(ConnData, RequestData),
    case Res of
        {ok, Bin} ->
	    Method = timer_method(AckAction),
            InitTimer = ConnData#conn_data.reply_timer,
            {WaitFor, CurrTimer} = init_timer(InitTimer),
            OptBin = opt_garb_binary(CurrTimer, Bin),
            ConnHandle = ConnData#conn_data.conn_handle,
            M = ?MODULE,
            F = reply_timeout,
            A = [ConnHandle, Rep#reply.trans_id, CurrTimer],
            Ref = megaco_monitor:apply_after(Method, 
					     M, F, A, WaitFor),
            Rep2 = Rep#reply{bytes             = OptBin,
                             state             = waiting_for_ack,
                             timer_ref         = Ref,
                             ack_action        = AckAction},
            megaco_monitor:insert_reply(Rep2); % Timing problem?
        {error, Reason} ->
	    {error, Reason}
    end.

handle_request_callback(ConnData, Actions, T) ->
    ?report_trace(ConnData, "callback: trans request", [T]),
    ConnHandle = ConnData#conn_data.conn_handle,
    Version    = ConnData#conn_data.protocol_version,
    UserMod    = ConnData#conn_data.user_mod,
    UserArgs   = ConnData#conn_data.user_args,
    Res = (catch apply(UserMod, handle_trans_request, [ConnHandle, Version, Actions | UserArgs])),
    ?report_debug(ConnData, "return: trans request", [T, {return, Res}]),
    case Res of
        {discard_ack, Replies} when list(Replies) ->
            Reply = {actionReplies, Replies},
            SendReply = send_reply(ConnData, Reply, asn1_NOVALUE),
            {discard_ack, SendReply};
        {discard_ack, Error} when record(Error, 'ErrorDescriptor') ->
            Reply = {transactionError, Error},
            SendReply = send_reply(ConnData, Reply, asn1_NOVALUE),
            {discard_ack, SendReply};
        {{handle_ack, AckData}, Replies} when list(Replies) ->
            Reply = {actionReplies, Replies},
            SendReply = send_reply(ConnData, Reply, 'NULL'),
            {{handle_ack, AckData}, SendReply};
        {{handle_ack, AckData}, Error} when record(Error, 'ErrorDescriptor') ->
            Reply = {transactionError, Error},
            SendReply = send_reply(ConnData, Reply, 'NULL'),
            {{handle_ack, AckData}, SendReply};
        {{handle_sloppy_ack, AckData}, Replies} when list(Replies) ->
            Reply = {actionReplies, Replies},
            SendReply = send_reply(ConnData, Reply, asn1_NOVALUE),
            {{handle_ack, AckData}, SendReply};
        {{handle_sloppy_ack, AckData}, Error} 
	when record(Error, 'ErrorDescriptor') ->
            Reply = {transactionError, Error},
            SendReply = send_reply(ConnData, Reply, asn1_NOVALUE),
            {{handle_ack, AckData}, SendReply};
        {pending, RequestData} ->
            %% The user thinks that this request will take
            %% quite a while to evaluate. Respond with a pending trans
            SendReply = send_pending(ConnData),
            {{pending, RequestData}, SendReply};
        Error ->
	    ErrorText = atom_to_list(UserMod),
	    ED = #'ErrorDescriptor'{errorCode = ?megaco_internal_gateway_error,
				    errorText = ErrorText},
            ?report_important(ConnData, "callback: <ERROR> trans request",
			      [ED, {error, Error}]),
	    error_msg("trans request callback failed: ~w", [Error]),
            Reply = {transactionError, ED},
            SendReply = send_reply(ConnData, Reply, asn1_NOVALUE),
            {discard_ack, SendReply}
    end.

handle_long_request_callback(ConnData, RequestData) ->
    ?report_trace(ConnData, "callback: trans long request", [RequestData]),
    ConnHandle = ConnData#conn_data.conn_handle,
    Version    = ConnData#conn_data.protocol_version,
    UserMod    = ConnData#conn_data.user_mod,
    UserArgs   = ConnData#conn_data.user_args,
    Res = (catch apply(UserMod, handle_trans_long_request, [ConnHandle, Version, RequestData | UserArgs])),
    ?report_debug(ConnData, "return: trans long request",
		  [{request_data, RequestData}, {return, Res}]),
    case Res of
        {discard_ack, Replies} when list(Replies) ->
            Reply = {actionReplies, Replies},
            SendReply = send_reply(ConnData, Reply, asn1_NOVALUE),
            {discard_ack, SendReply};
        {{handle_ack, AckData}, Replies} when list(Replies) ->
            Reply = {actionReplies, Replies},
            SendReply = send_reply(ConnData, Reply, 'NULL'),
            {{handle_ack, AckData}, SendReply};
        Error ->
	    ErrorText = atom_to_list(UserMod),
	    ED = #'ErrorDescriptor'{errorCode = ?megaco_internal_gateway_error,
				    errorText = ErrorText},
            ?report_important(ConnData, "callback: <ERROR> trans long request",
			      [ED, {error, Error}]),
	    error_msg("long trans request callback failed: ~w", [Error]),
            Reply = {transactionError, ED},
            SendReply = send_reply(ConnData, Reply, asn1_NOVALUE),
            {discard_ack, SendReply}
    end.

handle_pending(ConnData, T) ->
    TransId = to_local_trans_id(ConnData),
    case megaco_monitor:lookup_request(TransId) of
	[Req] when record(Req, request) ->
	    %% The request seems to take a while,
	    %% let's reset our transmission timer.
	    %% We now know the other side has got
	    %% the request and is working on it,
	    %% so there is no need to keep the binary
	    %% message for re-transmission.
	    case Req#request.timer_ref of
		{short, Ref} ->
		    %% Start using the long timer. 
		    %% We can now drop the "bytes", since we will
		    %% not resend from now on.
		    megaco_monitor:cancel_apply_after(Ref),
		    InitTimer = Req#request.init_long_timer,
		    {WaitFor, CurrTimer} = init_timer(InitTimer),
		    ConnHandle = ConnData#conn_data.conn_handle,
		    M = ?MODULE,
		    F = request_timeout,
		    A = [ConnHandle, TransId],
		    Ref2 = megaco_monitor:apply_after(M, F, A, WaitFor),
		    Req2 = Req#request{bytes      = {no_send, garb_binary},
				       timer_ref  = Ref2,
				       curr_timer = CurrTimer},
		    ?report_trace(ConnData, 
				  "trans pending (timer restarted)", [T]),
		    megaco_monitor:insert_request(Req2); % Timing problem?

		{long, Ref} ->
		    %% We just need to recalculate the timer, i.e. 
		    %% increment the timer (one "slot" has been consumed).
		    megaco_monitor:cancel_apply_after(Ref),
		    CurrTimer = Req#request.curr_timer,
		    {WaitFor, Timer2} = recalc_timer(CurrTimer),
		    ConnHandle = ConnData#conn_data.conn_handle,
		    M = ?MODULE,
		    F = request_timeout,
		    A = [ConnHandle, TransId],
		    Ref2 = megaco_monitor:apply_after(M, F, A, WaitFor),
		    Req2 = Req#request{timer_ref  = {long, Ref2},
				       curr_timer = CurrTimer},
		    ?report_trace(ConnData, 
				  "long trans pending (timer restarted)", 
				  [T]),
		    megaco_monitor:insert_request(Req2) % Timing problem?
	    end;
		
	[] ->
	    ?report_trace(ConnData, 
			  "remote pending (no receiver)", [T]),
	    return_unexpected_trans(ConnData, T)
    end.

handle_reply(ConnData, T) ->
    TransId = to_local_trans_id(ConnData),
    case megaco_monitor:lookup_request(TransId) of
	[Req] when record(Req,request) ->
            %% Don't care about Req and Rep version diff
            ?report_trace(ConnData, "trans reply", [T]),
            megaco_monitor:delete_request(TransId),

            megaco_monitor:cancel_apply_after(Req#request.timer_ref),

	    %% Send acknowledgement
	    case T#'TransactionReply'.immAckRequired of
		'NULL'       ->
		    send_ack(ConnData);
		asn1_NOVALUE ->
		    %% BUGBUG: maybe piggy-backed in other messages?
		    maybe_send_ack(ConnData)
	    end,
	    UserMod   = Req#request.user_mod,
	    UserArgs  = Req#request.user_args,
	    Action    = Req#request.reply_action,
	    UserData  = Req#request.reply_data,
	    UserReply =
		case T#'TransactionReply'.transactionResult of
		    {transactionError, Reason} ->
			{error, Reason};
		    {actionReplies, Replies} ->
			{ok, Replies}
		end,
	    ConnData2 = ConnData#conn_data{user_mod     = UserMod,
					   user_args    = UserArgs,
					   reply_action = Action,
					   reply_data   = UserData},
	    return_reply(ConnData2, TransId, UserReply);
	[] ->
	    ?report_trace(ConnData, "trans reply (no receiver)", [T]),
	    return_unexpected_trans(ConnData, T)
    end.


handle_acks([{ConnData, Rep, T} | Rest])
  when Rep#reply.state == waiting_for_ack ->
    handle_ack(ConnData, ok, Rep, T),
    handle_acks(Rest);
handle_acks([]) ->
    ok.

%% OTP-4213: Temporary until we figure out why this work...
handle_ack(ConnData, AckStatus, Rep, T) ->
    megaco_monitor:cancel_apply_after(Rep#reply.timer_ref),
    megaco_monitor:delete_reply(Rep#reply.trans_id),
    handle_ack_callback(ConnData, AckStatus, Rep#reply.ack_action, T).

% handle_ack(ConnData, AckStatus, Rep, T) ->
%     megaco_monitor:cancel_apply_after(Rep#reply.timer_ref),
%     TransId = to_remote_trans_id(ConnData),
%     megaco_monitor:delete_reply(TransId),
%     handle_ack_callback(ConnData, AckStatus, Rep#reply.ack_action, T).

handle_ack_callback(ConnData, AckStatus, discard_ack = AckAction, T) ->
    case AckStatus of
        ok ->
            ok;
        {error, Reason} ->
            ?report_trace(ConnData, "handle ack",
                          [T, AckAction, {error, Reason}])
    end;
handle_ack_callback(ConnData, AckStatus, {handle_ack, AckData}, T) ->
    ?report_trace(ConnData, "callback: trans ack", [{ack_data, AckData}]),
    ConnHandle = ConnData#conn_data.conn_handle,
    Version    = ConnData#conn_data.protocol_version,
    UserMod    = ConnData#conn_data.user_mod,
    UserArgs   = ConnData#conn_data.user_args,
    Res = (catch apply(UserMod, handle_trans_ack, [ConnHandle, Version, AckStatus, AckData | UserArgs])),
    ?report_debug(ConnData, "return: trans ack", [T, AckData, {return, Res}]),
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
    Res.

handle_disconnect_callback(ConnData, UserReason)
  when record(ConnData, conn_data) ->
    ?report_trace(ConnData, "callback: disconnect", [{reason, UserReason}]),
    ConnHandle = ConnData#conn_data.conn_handle,
    Version    = ConnData#conn_data.protocol_version,
    UserMod    = ConnData#conn_data.user_mod,
    UserArgs   = ConnData#conn_data.user_args,
    Res = (catch apply(UserMod, handle_disconnect, [ConnHandle, Version, UserReason | UserArgs])),
    ?report_debug(ConnData, "return: disconnect", [{reason, UserReason}, {return, Res}]),
    Res.

%%----------------------------------------------------------------------
%% Send outgoing messages
%%----------------------------------------------------------------------

call(ConnHandle, Actions, Options) ->
    Options2 = [{reply_data, self()} | Options],
    call_or_cast(call, ConnHandle, Actions, Options2).

cast(ConnHandle, Actions, Options) ->
    call_or_cast(cast, ConnHandle, Actions, Options).

%% In a transaction there can be several actions, so if the 
%% First element of the Actions list is an ''ActionRequest''
%% record this a list of ActionRequest's for one Transaction
%% request. If on the other hand this is not the case, then
%% the Actions list is assumed to be a list of list of
%% ActionRequest. That is, several transactions.
call_or_cast(CallOrCast, ConnHandle, [A|_] = Actions, Options) when tuple(A) ->
    %% Just one transaction
    case call_or_cast(CallOrCast, ConnHandle, [Actions], Options) of
	ok ->
	    ok;
	{error, Reason} ->
	    {error, Reason};
	{Version, [Reply]} when integer(Version) ->
	    {Version, Reply}
    end;
    
call_or_cast(CallOrCast, ConnHandle, Actions, Options)
  when record(ConnHandle, megaco_conn_handle) ->
    case prepare_send_options(ConnHandle, Options, Actions) of
        {ok, ConnData} ->
            case encode_request(ConnData, Actions) of
                {ok, Bin} -> %% If only one transaction, ...
                    TransId = to_local_trans_id(ConnData),
                    send_request(ConnData, ConnHandle, 
				 TransId, CallOrCast, Bin),
		    case CallOrCast of
			call -> 
			    wait_for_reply(TransId);
			cast -> 
			    ok
		    end;
                {ok, TRs, Bin} -> %% If only one transaction, ...
                    send_request(ConnData, ConnHandle, 
				 TRs, CallOrCast, Bin),
		    case CallOrCast of
			call -> 
			    TransIds = to_local_trans_id(ConnData, Actions),
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
    wait_for_reply(trans_flatten(TransIds), []).

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
	    wait_for_reply(TransIds2, [{Version, TransId, Result}|Replies])
% 	Any ->
% 	    d("wait_for_reply -> received unexpected: ~n   ~p", [Any]),
% 	    wait_for_reply(TransIds, Replies)
	    
    end.


%% TransInfo is either a list of the transactions or a transaction id
send_request(ConnData, ConnHandle, TransInfo, Action, Bin)
  when node(ConnData#conn_data.control_pid) == node() ->
    #conn_data{request_timer       = InitTimer,
	       long_request_timer  = LongTimer} = ConnData,
    insert_requests(ConnData, ConnHandle, TransInfo, Action, Bin,
		    InitTimer, LongTimer),
    case megaco_messenger_misc:send_message(ConnData, Bin) of
	{error, Reason} ->
	    cancel_requests(ConnData, TransInfo, Reason);
	{ok, _} ->
	    ignore
    end;
send_request(ConnData, ConnHandle, TransInfo, Action, Bin) ->
    InitTimer = infinity,
    LongTimer = infinity,
    insert_requests(ConnData, ConnHandle, TransInfo, Action, Bin,
		    InitTimer, LongTimer),
    Node = node(ConnData#conn_data.control_pid),
    Args = [node(), ConnData, TransInfo, Bin],
    rpc:cast(Node, ?MODULE, send_request_remote, Args).


insert_requests(ConnData, ConnHandle, TransId, Action, Bin,
		InitTimer, LongTimer)
  when record(TransId, trans_id) ->
    insert_request(ConnData, ConnHandle, TransId, Action, Bin,
		   InitTimer, LongTimer);
insert_requests(_, _, [], _, _, _, _) ->
    ok;
insert_requests(ConnData, ConnHandle, [{transactionRequest, TR}|TRs], 
		Action, Bin, InitTimer, LongTimer) ->
    #'TransactionRequest'{transactionId = TransId} = TR,

    insert_request(ConnData, ConnHandle, 
		   to_local_trans_id(ConnHandle, TransId), 
		   Action, TR, InitTimer, LongTimer),

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
                   timer_ref       = {short, Ref},
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

    
trans_flatten(#trans_id{serial = Serials} = TransIds) when list(Serials) ->
    [TransIds#trans_id{serial = Serial} || Serial <- Serials];
trans_flatten(#trans_id{serial = Serial} = TransId) when integer(Serial) ->
    [TransId#trans_id{serial = [Serial]}].

    
send_request_remote(ReplyNode, ConnData, TransInfo, Bin) ->
    Action = remote,
    ConnHandle = ConnData#conn_data.conn_handle,
    ConnData2  = ConnData#conn_data{reply_data = ReplyNode},
    send_request(ConnData2, ConnHandle, TransInfo, Action, Bin).

prepare_send_options(ConnHandle, Options, Actions) ->
    %% Ensures that two processes cannot get same transaction id.
    %% Bad send options may cause spurious transaction id to be consumed.
    Incr = number_of_transactions(Actions),
    case megaco_config:incr_trans_id_counter(ConnHandle, Incr) of
        {ok, ConnData} ->
            override_send_options(ConnData, Options);
        {error, Reason} ->
            {error, Reason}
    end.

number_of_transactions([Action|_]) when tuple(Action) ->
    1;
number_of_transactions(ActionsList) ->
    length(ActionsList).

override_send_options(ConnData, [{Key, Val} | Tail]) ->
    case Key of
        send_handle ->
            ConnData2 = ConnData#conn_data{send_handle = Val},
            override_send_options(ConnData2, Tail);
        request_timer ->
            case megaco_config:verify_val(Key, Val) of
                true ->
                    ConnData2 = ConnData#conn_data{request_timer = Val},
                    override_send_options(ConnData2, Tail);
                false ->
                    {error, {bad_send_option, {Key, Val}}}
            end;
        long_request_timer ->
            case megaco_config:verify_val(Key, Val) of
                true ->
                    ConnData2 = ConnData#conn_data{long_request_timer = Val},
                    override_send_options(ConnData2, Tail);
                false ->
                    {error, {bad_send_option, {Key, Val}}}
            end;
        reply_data ->
            ConnData2 = ConnData#conn_data{reply_data = Val},
            override_send_options(ConnData2, Tail);
        user_mod when atom(Val) ->
            ConnData2 = ConnData#conn_data{user_mod = Val},
            override_send_options(ConnData2, Tail);
        user_args when list(Val) ->
            ConnData2 = ConnData#conn_data{user_args = Val},
            override_send_options(ConnData2, Tail);
        _Bad ->
            {error, {bad_send_option, {Key, Val}}}
    end;
override_send_options(ConnData, []) ->
    {ok, ConnData}.


%% Sending several transactions in one message
encode_request(ConnData, [A|_] = ActionsList) when list(A) ->
    LastSerial = ConnData#conn_data.serial,
    %% BUGBUG
    %% Do we really need to reverse?
    %% Do the TR has to be in increasing transaction is order?
    TRs = encode_request1(LastSerial, lists:reverse(ActionsList), []),
    Body = {transactions, TRs},
    EncodeRes = megaco_messenger_misc:encode_body(ConnData, 
						  "send trans request", 
						  Body),
    case EncodeRes of
	{ok, Bin} ->
	    {ok, TRs, Bin};
	Error ->
	    Error
    end;
    

%% Sending one transaction in one message
encode_request(ConnData, Actions) ->
    Serial = ConnData#conn_data.serial,
    TR   = #'TransactionRequest'{transactionId = Serial,
				 actions       = Actions},
    TRs  = [{transactionRequest, TR}],
    Body = {transactions, TRs},
    EncodeRes = megaco_messenger_misc:encode_body(ConnData, 
						  "send trans request", 
						  Body),
    case EncodeRes of
	{ok, Bin} ->
	    {ok, TRs, Bin};
	Error ->
	    Error
    end.

encode_request1(_S, [], Acc) ->
    lists:reverse(Acc);
encode_request1(Serial, [A|As], Acc) ->
    TR = #'TransactionRequest'{transactionId = Serial,
                               actions       = A},
    encode_request1(Serial - 1, As, [{transactionRequest, TR}|Acc]).

send_reply(ConnData, Result, ImmAck) ->
    %% Encapsule the transaction result into a reply message
    Serial = ConnData#conn_data.serial,
    TR = #'TransactionReply'{transactionId     = Serial,
                             immAckRequired    = ImmAck,
                             transactionResult = Result},
    Body = {transactions, [{transactionReply, TR}]},
    case megaco_messenger_misc:encode_body(ConnData, 
					   "send trans reply", Body) of
        {ok, Bin} ->
            megaco_messenger_misc:send_message(ConnData, Bin);
        {error, Reason} = Error ->
	    ED = #'ErrorDescriptor'{errorCode = ?megaco_internal_gateway_error,
				    errorText = "encode body"},
            Reply = {transactionError, ED},
            TR2 =  TR#'TransactionReply'{transactionResult = Reply},
            TraceLabel = "<ERROR> encode trans reply body failed",
            ?report_important(ConnData, TraceLabel, [TR, TR2, ED, Error]),
	    error_msg("encode trans reply body failed: ~w", [Reason]),
            Body2 = {transactions, [{transactionReply, TR2}]},
            megaco_messenger_misc:send_body(ConnData, TraceLabel, Body2)
    end.

send_pending(ConnData) ->
    %% Encapsule the transaction result into a reply message
    Serial = ConnData#conn_data.serial,
    TP = #'TransactionPending'{transactionId = Serial},
    Body = {transactions, [{transactionPending, TP}]},
    megaco_messenger_misc:send_body(ConnData, "send trans pending", Body).


maybe_send_ack(#conn_data{auto_ack = false}) ->
    ignore;
maybe_send_ack(#conn_data{accu_ack_timer = 0} = ConnData) ->
    send_ack(ConnData);
maybe_send_ack(#conn_data{serial     = Serial,
			  ack_sender = Ref}) ->
    megaco_ack_sender:send_ack(Ref, Serial),
    ok.


send_ack(ConnData) ->
    %% Encapsule the transaction result into a reply message
    Serial = ConnData#conn_data.serial,
    TRA = #'TransactionAck'{firstAck = Serial},
    Body = {transactions, [{transactionResponseAck, [TRA]}]},
    megaco_messenger_misc:send_body(ConnData, "send trans ack", Body).


send_trans_error(ConnData, Code, Reason) ->
    %% Encapsule the transaction error into a reply message
    ED     = #'ErrorDescriptor'{errorCode = Code, errorText = Reason},
    Serial = ConnData#conn_data.serial,
    TR     = #'TransactionReply'{transactionId     = Serial,
				 transactionResult = {transactionError,ED}},
    Body   = {transactions, [{transactionReply, TR}]},
    megaco_messenger_misc:send_body(ConnData, "send trans error", Body).

send_message_error(ConnData, Code, Reason) ->
    ED = #'ErrorDescriptor'{errorCode = Code, errorText = Reason},
    Body = {messageError, ED},
    megaco_messenger_misc:send_body(ConnData, "send trans error", Body).

cancel(ConnHandle, Reason) when record(ConnHandle, megaco_conn_handle) ->
    case megaco_config:lookup_local_conn(ConnHandle) of
        [ConnData] ->
	    do_cancel(ConnHandle, Reason, ConnData);
        [] ->
	    ConnData = fake_conn_data(ConnHandle),
	    do_cancel(ConnHandle, Reason, ConnData)
    end.

do_cancel(ConnHandle, Reason, ConnData) ->
    LocalMid      = ConnHandle#megaco_conn_handle.local_mid,
    RemoteMid     = ConnHandle#megaco_conn_handle.remote_mid,
    ReqTransIdPat = #trans_id{mid = LocalMid, serial = '_'}, 
    ReqPat = #request{trans_id          = ReqTransIdPat,
		      remote_mid        = RemoteMid,
		      timer_ref         = '_',
		      init_timer        = '_',
		      init_long_timer   = '_',
		      curr_timer        = '_',
		      version           = '_',
		      bytes             = '_',
		      send_handle       = '_',
		      user_mod          = '_',
		      user_args         = '_',
		      reply_action      = '_',
		      reply_data        = '_'},
    CancelReq = fun(Req) ->
			cancel_request(ConnData, Req, Reason),
			Ref = Req#request.timer_ref,
			megaco_monitor:cancel_apply_after(Ref)
		end,
    Requests  = megaco_monitor:match_requests(ReqPat),
    lists:foreach(CancelReq, Requests),
    RemoteMid = ConnHandle#megaco_conn_handle.remote_mid,
    RepTransIdPat = #trans_id{mid = RemoteMid, serial = '_'}, % BUGBUG List here?
    RepPat = #reply{trans_id   	      = RepTransIdPat,
		    local_mid  	      = LocalMid,
		    state      	      = waiting_for_ack,
		    pending_timer_ref = '_',
		    timer_ref  	      = '_',
		    version    	      = '_',
		    bytes      	      = '_',
		    ack_action 	      = '_'},
    CancelRep = fun(Rep) -> cancel_reply(ConnData, Rep, Reason) end,
    Replies   = megaco_monitor:match_replies(RepPat),
    lists:foreach(CancelRep, Replies),
    ok.

cancel_requests(ConnData, [], Reason)  ->
    ok;
cancel_requests(ConnData, [{transactionRequest,TR}|TRs], Reason) ->
    #'TransactionRequest'{transactionId = TransId0} = TR,
    TransId = to_local_trans_id(ConnData#conn_data.conn_handle, TransId0),
    case megaco_monitor:lookup_request(TransId) of
	[] ->
	    ignore;
	[Req] when record(Req, request) ->
	    cancel_request(ConnData, Req, Reason)
    end,
    cancel_requests(ConnData, TRs, Reason).

cancel_request(ConnData, Req, Reason)  ->
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
        call when pid(UserData) ->
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
	    Res;
        remote ->
	    ?report_trace(ConnData, "callback: (remote) trans reply", [UserReply]),
            Node = UserData,
            Args = [ConnData, UserReply],
            rpc:cast(Node, ?MODULE, receive_reply_remote, Args)
    end.

receive_reply_remote(ConnData, UserReply) ->
    TransId = to_local_trans_id(ConnData),
    case megaco_monitor:lookup_request(TransId) of
        [Req] when record(Req, request) ->
            %% Don't care about Req and Rep version diff
	    megaco_monitor:delete_request(TransId),
	    megaco_monitor:cancel_apply_after(Req#request.timer_ref),
	
	    UserMod   = Req#request.user_mod,
	    UserArgs  = Req#request.user_args,
	    Action    = Req#request.reply_action,
	    UserData  = Req#request.reply_data,
	    ConnData2 = ConnData#conn_data{user_mod     = UserMod,
					   user_args    = UserArgs,
					   reply_action = Action,
					   reply_data   = UserData},
	    return_reply(ConnData2, TransId, UserReply);
		
	[] ->
	    ?report_trace(ConnData, "remote reply (no receiver)", 
			  [UserReply]),
	    return_unexpected_trans_reply(ConnData, TransId, UserReply)
    end.


cancel_reply(ConnData, Rep, Reason) when record(Rep, reply) ->
    megaco_monitor:cancel_apply_after(Rep#reply.pending_timer_ref),
    Serial = (Rep#reply.trans_id)#trans_id.serial,
    ConnData2 = ConnData#conn_data{serial = Serial},
    T = #'TransactionAck'{firstAck = Serial},
    handle_ack(ConnData2, {error, Reason}, Rep, T).


request_timeout(ConnHandle, TransId) ->
    case megaco_monitor:lookup_request(TransId) of
	[] ->
	    ignore;
	[Req] when record(Req, request) ->
	    case megaco_config:lookup_local_conn(ConnHandle) of
		[ConnData] ->
 		    incNumTimerRecovery(ConnHandle),
 		    do_request_timeout(ConnHandle, TransId, ConnData, Req);
		[] when ConnHandle#megaco_conn_handle.remote_mid == preliminary_mid ->
 		    %% The connection has just been upgraded from a 
 		    %% preliminary to a real connection. So this timeout
 		    %% is just a glitch. E.g. between the removel of this
 		    %% ConnHandle and the timer.
 		    request_timeout_upgraded(TransId);
		[] ->
 		    incNumTimerRecovery(ConnHandle),
 		    ConnData = fake_conn_data(ConnHandle),
 		    do_request_timeout(ConnHandle, TransId, ConnData, Req)
	    end
    end.

request_timeout_upgraded(TransId) ->
    megaco_monitor:delete_request(TransId).

    
do_request_timeout(ConnHandle, TransId, ConnData, 
		   #request{curr_timer = CurrTimer} = Req) ->
    SendHandle = Req#request.send_handle,
    Version    = Req#request.version,
    ConnData2  = ConnData#conn_data{send_handle      = SendHandle,
				    protocol_version = Version},
    case CurrTimer of
	timeout ->
	    cancel_request(ConnData2, Req, timeout);
	Timer ->
	    {SendOrNoSend, Data} = Req#request.bytes,
	    case SendOrNoSend of
		send ->
		    case maybe_encode(ConnData, Data) of
			{ok, Bin} ->
			    ?report_trace(ConnData2, "re-send trans request", 
					  [{bytes, Bin}]),
			    case megaco_messenger_misc:send_message(ConnData2, 
								    Bin) of
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

maybe_encode(ConnData, Bin) when binary(Bin) ->
    {ok, Bin};
maybe_encode(ConnData, TR) when record(TR, 'TransactionRequest') ->
    Body = {transactions, [{transactionRequest, TR}]},
    megaco_messenger_misc:encode_body(ConnData, "send trans request", Body);
maybe_encode(_ConnData, Trash) ->
    {error, {invalid_bin, Trash}}.

    
reply_timeout(ConnHandle, TransId, Timer) ->
    case Timer of
        timeout ->
	    incNumTimerRecovery(ConnHandle),
            %% OTP-4378
            case megaco_monitor:lookup_reply(TransId) of
                [Rep] when Rep#reply.state == waiting_for_ack ->
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
                _ ->
                    megaco_monitor:delete_reply(TransId)
            end;

        Timer ->
            case megaco_monitor:lookup_reply(TransId) of
                [] ->
                    ignore; % Trace ??
                [Rep] when Rep#reply.state == waiting_for_ack ->
                    {WaitFor, Timer2} = recalc_timer(Timer),
                    OptBin = opt_garb_binary(Timer2, Rep#reply.bytes),
                    M = ?MODULE,
                    F = reply_timeout,
                    A = [ConnHandle, TransId, Timer2],
                    Ref2 = megaco_monitor:apply_after(M, F, A, WaitFor),
                    Rep2 = Rep#reply{bytes     = OptBin,
                                     timer_ref = Ref2},
                    megaco_monitor:insert_reply(Rep2) % Timing problem?
            end
    end.

pending_timeout(ConnData, TransId) ->
    case megaco_monitor:lookup_reply(TransId) of
	[] ->
	    ignore; % Trace ??
	[Rep] ->
	    case Rep#reply.state of
		waiting_for_ack ->
		    %% The reply has already been sent
		    %% No need for any pending trans reply
		    ignore;
		eval_request ->
		    incNumTimerRecovery(ConnData#conn_data.conn_handle),
		    send_pending(ConnData)
	    end
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
    (catch apply(UserMod, handle_unexpected_trans, 
		 [ConnHandle, Version, Trans | UserArgs])).


to_remote_trans_id(ConnData) when record(ConnData, conn_data) ->
    Mid = (ConnData#conn_data.conn_handle)#megaco_conn_handle.remote_mid,
    #trans_id{mid    = Mid,
              serial = ConnData#conn_data.serial}.

to_local_trans_id(ConnData) when record(ConnData, conn_data) ->
    Mid = (ConnData#conn_data.conn_handle)#megaco_conn_handle.local_mid,
    #trans_id{mid    = Mid,
              serial = ConnData#conn_data.serial}.

to_local_trans_id(ConnData, [A|_] = ActionsList) 
  when record(ConnData, conn_data), list(A) ->
    Mid = (ConnData#conn_data.conn_handle)#megaco_conn_handle.local_mid,
    NumSerial  = length(ActionsList),
    LastSerial = ConnData#conn_data.serial,
    to_local_trans_id(Mid, NumSerial, LastSerial);
to_local_trans_id(ConnData, _Actions) 
  when record(ConnData, conn_data) ->
    Mid = (ConnData#conn_data.conn_handle)#megaco_conn_handle.local_mid,
    #trans_id{mid    = Mid,
              serial = ConnData#conn_data.serial};
to_local_trans_id(ConnHandle, Serial) 
  when record(ConnHandle, megaco_conn_handle), integer(Serial) ->
    #trans_id{mid    = ConnHandle#megaco_conn_handle.local_mid, 
	      serial = Serial}.

to_local_trans_id(Mid, NumSerial, LastSerial) ->
    Serials = lists:seq(LastSerial-(NumSerial-1),LastSerial),
    #trans_id{mid = Mid, serial = Serials}.


%% Returns {WaitFor, NewTimer} | {WaitFor, timeout}
init_timer(SingleWaitFor) when SingleWaitFor == infinity ->
    {SingleWaitFor, timeout};
init_timer(SingleWaitFor) when integer(SingleWaitFor) ->
    {SingleWaitFor, timeout};
init_timer(Timer) when record(Timer, megaco_incr_timer) ->
    return_incr(Timer).

return_incr(Timer) ->
    WaitFor = Timer#megaco_incr_timer.wait_for,
    case Timer#megaco_incr_timer.max_retries of
	infinity ->
	    {WaitFor, Timer};
	Int when integer(Int), Int > 0 -> 
	    {WaitFor, Timer};
	0  ->
	    {WaitFor, timeout}
    end.

%% Returns {WaitFor, NewTimer} | {WaitFor, timeout}
recalc_timer(Timer) when record(Timer, megaco_incr_timer) ->
    Old     = Timer#megaco_incr_timer.wait_for,
    Factor  = Timer#megaco_incr_timer.factor,
    Incr    = Timer#megaco_incr_timer.incr,
    New     = (Old * Factor) + Incr,
    Max     = decr(Timer#megaco_incr_timer.max_retries),
    Timer2  = Timer#megaco_incr_timer{wait_for    = New,
                                      max_retries = Max},
    return_incr(Timer2).

decr(infinity) -> infinity;
decr(Int)      -> Int - 1.


error_msg(F, A) ->
    (catch error_logger:error_msg(F ++ "~n", A)).


% d(F) ->
%     d(F,[]).

% d(F,A) ->
%     d(true,F,A).
%     %% d(get(dbg),F,A).

% d(true,F,A) ->
%     io:format("*** [~s] ~p:~p ***"
% 	      "~n   " ++ F ++ "~n", 
% 	      [format_timestamp(now()), self(),?MODULE|A]);
% d(_, _, _) ->
%     ok.

% format_timestamp(Now) ->
%     {N1, N2, N3}   = Now,
%     {Date, Time}   = calendar:now_to_datetime(Now),
%     {YYYY,MM,DD}   = Date,
%     {Hour,Min,Sec} = Time,
%     FormatDate = 
%         io_lib:format("~.4w:~.2.0w:~.2.0w ~.2.0w:~.2.0w:~.2.0w 4~w",
%                       [YYYY,MM,DD,Hour,Min,Sec,round(N3/1000)]),  
%     lists:flatten(FormatDate).

	      
%%-----------------------------------------------------------------
%% Func: incNumErrors/0, incNumErrors/1, incNumTimerRecovery/1
%% Description: SNMP counter increment functions
%%-----------------------------------------------------------------
incNumErrors() ->
    incNum(medGwyGatewayNumErrors).

% incNumErrors(CH) ->
%     incNum({CH, medGwyGatewayNumErrors}).

incNumTimerRecovery(CH) ->
    incNum({CH, medGwyGatewayNumTimerRecovery}).

incNum(Cnt) ->
    case (catch ets:update_counter(megaco_stats, Cnt, 1)) of
	{'EXIT', {badarg, _Reason}} ->
	    ets:insert(megaco_stats, {Cnt, 1});
	Old ->
	    Old
    end.
	    
