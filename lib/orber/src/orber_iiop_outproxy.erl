%%--------------------------------------------------------------------
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
%%-----------------------------------------------------------------
%% File: orber_iiop_outproxy.erl
%% Author: Lars Thorsen
%% 
%% Description:
%%    This file contains the IIOP "proxy" for outgoing connections
%%
%% Creation date: 990425
%%
%%-----------------------------------------------------------------
-module(orber_iiop_outproxy).

-behaviour(gen_server).

-include_lib("orber/src/orber_iiop.hrl").
-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_debug.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/0, start/1, request/6, locate/2]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2, stop/2]).


%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
start() ->
    ignore.

start(Opts) ->
    gen_server:start_link(orber_iiop_outproxy, Opts, []).

request(Pid, ObjKey, Op, Parameters, TypeCodes, ResponseExpected) ->
    gen_server:call(Pid, {request, ObjKey, Op, Parameters, TypeCodes, ResponseExpected}, infinity).

locate(Pid, ObjKey) ->
    gen_server:call(Pid, {locate, ObjKey}).

%%-----------------------------------------------------------------
%% Internal interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: stop/2 (Only used for test purpose !!!!!!)
%%-----------------------------------------------------------------
stop(Pid, Timeout) ->
    gen_server:call(Pid, stop, Timeout).


%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: init/1
%%-----------------------------------------------------------------
init({connect, Host, Port, ssl, SocketOptions}) ->
    ?PRINTDEBUG2("orber_iiop_outproxy init (ssl): ~p ", [self()]),
    process_flag(trap_exit, true), 
    case catch ?IIOP_SOCKET_MOD:connect(ssl, Host, Port, SocketOptions) of
	{'EXCEPTION', E} ->
	    {stop, {'EXCEPTION', E}};
	Socket ->
	    {ok, {Socket, ssl, ets:new(orber_outgoing_requests, [set])}}
    end;
init({connect, Host, Port, SocketType, SocketOptions}) ->
    ?PRINTDEBUG2("orber_iiop_outproxy init (~p): ~p ", [SocketType, self()]),
    process_flag(trap_exit, true), 
    case catch ?IIOP_SOCKET_MOD:connect(SocketType, Host, Port, SocketOptions) of
	{'EXCEPTION', E} ->
	    {stop, {'EXCEPTION', E}};
	Socket ->
	    {ok, {Socket, SocketType, ets:new(orber_outgoing_requests, [set])}}
    end.

%%-----------------------------------------------------------------
%% Func: terminate/2
%%-----------------------------------------------------------------
terminate(Reason, {Socket, SocketType, OutRequests}) ->
    %% Kill all proxies and delete table before terminating
    ?PRINTDEBUG2("out proxy for socket ~p terminated with reason: ~p", [Socket, Reason]),
    kill_all_requests(OutRequests, ets:first(OutRequests)),
    ets:delete(OutRequests),
    ok.

kill_all_requests(_, '$end_of_table') ->
    ok;
kill_all_requests(OutRequests, Key) ->
    [{_, Pid}] = ets:lookup(OutRequests, Key),
    exit(Pid, kill),
    kill_all_requests(OutRequests, ets:next(OutRequests, Key)).

%%-----------------------------------------------------------------
%% Func: handle_call/3
%%-----------------------------------------------------------------
handle_call({request, ObjKey, Op, Parameters, TypeCodes, ResponseExpected},
	    From, {Socket, SocketType, OutRequests}) ->
    RequestId = orber_request_number:get(),
   {ok,  Pid} = orber_iiop_outrequest:start(),
    ets:insert(OutRequests, {RequestId, Pid}),
    orber_iiop_outrequest:request(Pid, RequestId, ObjKey, Op, Parameters,
				  TypeCodes, ResponseExpected, From, Socket,
				  SocketType),
    {noreply, {Socket, SocketType, OutRequests}};
handle_call({locate, ObjKey}, From, {Socket, SocketType, OutRequests}) ->
    RequestId = orber_request_number:get(),
    {ok,  Pid} = orber_iiop_outrequest:start(),
    ets:insert(OutRequests, {RequestId, Pid}),
    orber_iiop_outrequest:locate(Pid, RequestId, ObjKey, From, Socket,
				SocketType),
    {noreply, {Socket, SocketType, OutRequests}};
handle_call(stop, From, State) ->
    {stop, normal, ok, State};
handle_call(_, _, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------
%% Func: handle_cast/2
%%-----------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    {noreply, State}.

%%-----------------------------------------------------------------
%% Func: handle_info/2
%%-----------------------------------------------------------------
%% Trapping exits 
handle_info({tcp_closed, Socket}, {Socket, SocketType, OutRequests}) ->
    {stop, normal, {Socket, SocketType, OutRequests}}; %% Maybee send a com failure to all clients. ?????
handle_info({tcp, Socket, Bytes}, {Socket, SocketType, OutRequests}) ->
    handle_reply(Bytes, Socket, SocketType, OutRequests),
    {noreply, {Socket, SocketType, OutRequests}};
handle_info({tcp_error, Socket, Reason}, {Socket, SocketType, OutRequests}) ->
    corba:raise(#'INV_FLAG'{completion_status=?COMPLETED_NO});

handle_info({ssl_closed, Socket}, {Socket, SocketType, OutRequests}) ->
    {stop, normal, {Socket, SocketType, OutRequests}};
handle_info({ssl, Socket, Bytes}, {Socket, SocketType, OutRequests}) ->
    handle_reply(Bytes, Socket, SocketType, OutRequests),
    {noreply, {Socket, SocketType, OutRequests}};
handle_info({ssl_error, Socket}, {Socket, SocketType, OutRequests}) ->
    corba:raise(#'INV_FLAG'{completion_status=?COMPLETED_NO});

handle_info({'EXIT', Pid, normal}, {Socket, SocketType, OutRequests}) ->
    [{K, _}] = ets:match_object(OutRequests, {'$1', Pid}),
    ets:delete(OutRequests, K),
    {noreply, {Socket, SocketType, OutRequests}};
handle_info({'EXIT', Pid, Reason}, {Socket, SocketType, OutRequests}) ->
    ?PRINTDEBUG2("out request ~p finished with reason ~p", [Pid, Reason]),
    [{K, _}] = ets:match_object(OutRequests, {'$1', Pid}),
    ets:delete(OutRequests, K),
    {noreply, {Socket, SocketType, OutRequests}};
handle_info(stop, State) ->
    {stop, normal, State};
handle_info(X, State) ->
    {noreply, State}.

-ifdef(interceptors).

handle_reply(Bytes, Socket, SocketType, OutRequests) ->
    %% Check IIOP headers and fetch request id
    case catch checkheaders(orber_interceptors:call_receive_message_interceptors(Bytes)) of
	{'EXCEPTION', DecodeException} ->
	    corba:raise(#'COMM_FAILURE'{minor=120, completion_status=?COMPLETED_MAYBE});
	{'EXIT', message_error} ->
	    ME = cdr_encode:enc_message_error(orber:giop_version()),
	    ?IIOP_SOCKET_MOD:write(SocketType, Socket, ME),
	    corba:raise(#'COMM_FAILURE'{minor=121, completion_status=?COMPLETED_NO});
	{'EXIT', R} ->
	    corba:raise(#'UNKNOWN'{minor=40, completion_status=?COMPLETED_MAYBE});
	'message_error' ->
	    %% Perhaps a resend should be done when a message error occurs
	    corba:raise(#'COMM_FAILURE'{minor=122, completion_status=?COMPLETED_NO});
	{error, no_reply} ->
	    ok;
	{'reply', ReplyHeader, Rest, Len, ByteOrder} ->
	    case ets:lookup(OutRequests, ReplyHeader#reply_header.request_id) of
		[{_, Pid}] ->
		    %% Send reply to the correct request process
		    orber_iiop_outrequest:reply(Pid, ReplyHeader, Rest, 
						Len, ByteOrder);
		_ ->
		    ok
	    end;
	{'locate_reply', LocateReplyHeader, LocateRest, LocateLen, LocateByteOrder} ->
	    case ets:lookup(OutRequests, LocateReplyHeader#locate_reply_header.request_id) of
		[{_, Pid}] ->
		    %% Send reply to the correct request process
		    orber_iiop_outrequest:locate_reply(Pid, LocateReplyHeader, LocateRest,
						       LocateLen, LocateByteOrder);
		_ ->
		    ok
	    end;
	X ->
	    ?PRINTDEBUG2("outproxy got ~p from checkheaders", [X])
    end.

-else.

handle_reply(Bytes, Socket, SocketType, OutRequests) ->
    %% Check IIOP headers and fetch request id
    case catch checkheaders(Bytes) of
	{'EXCEPTION', DecodeException} ->
	    corba:raise(#'COMM_FAILURE'{minor=120, completion_status=?COMPLETED_MAYBE});
	{'EXIT', message_error} ->
	    ME = cdr_encode:enc_message_error(orber:giop_version()),
	    ?IIOP_SOCKET_MOD:write(SocketType, Socket, ME),
	    corba:raise(#'COMM_FAILURE'{minor=121, completion_status=?COMPLETED_NO});
	{'EXIT', R} ->
	    corba:raise(#'UNKNOWN'{minor=40, completion_status=?COMPLETED_MAYBE});
	'message_error' ->
	    %% Perhaps a resend should be done when a message error occurs
	    corba:raise(#'COMM_FAILURE'{minor=122, completion_status=?COMPLETED_NO});
	{error, no_reply} ->
	    ok;
	{'reply', ReplyHeader, Rest, Len, ByteOrder} ->
	    case ets:lookup(OutRequests, ReplyHeader#reply_header.request_id) of
		[{_, Pid}] ->
		    %% Send reply to the correct request process
		    orber_iiop_outrequest:reply(Pid, ReplyHeader, Rest, 
						Len, ByteOrder);
		_ ->
		    ok
	    end;
	{'locate_reply', LocateReplyHeader, LocateRest, LocateLen, LocateByteOrder} ->
	    case ets:lookup(OutRequests, LocateReplyHeader#locate_reply_header.request_id) of
		[{_, Pid}] ->
		    %% Send reply to the correct request process
		    orber_iiop_outrequest:locate_reply(Pid, LocateReplyHeader, LocateRest,
						       LocateLen, LocateByteOrder);
		_ ->
		    ok
	    end;
	X ->
	    ?PRINTDEBUG2("outproxy got ~p from checkheaders", [X])
    end.

-endif.

%%-----------------------------------------------------------------
%% Func: code_change/3
%%-----------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

checkheaders(Bytes) ->
    Message = cdr_decode:dec_giop_message_header(Bytes),
    case Message#giop_message.message_type of
	'reply' ->
	    {ReplyHeader, Rest, Len} = 
		cdr_decode:dec_reply_header(Message#giop_message.giop_version,
					    Message#giop_message.message, 
					    ?GIOP_HEADER_SIZE,
					    Message#giop_message.byte_order),
	    {'reply', ReplyHeader, Rest, Len, Message#giop_message.byte_order};
	'locate_reply' ->
	    {LocateReplyHeader, Rest, Len} = 
		cdr_decode:dec_locate_reply_header(Message#giop_message.giop_version,
					    Message#giop_message.message, 
					    ?GIOP_HEADER_SIZE,
					    Message#giop_message.byte_order),
	    {'locate_reply', LocateReplyHeader, Rest, Len, Message#giop_message.byte_order};
	_ ->
	    {error, no_reply}
    end.
    
