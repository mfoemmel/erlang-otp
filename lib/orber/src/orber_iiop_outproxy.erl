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
-export([start/0, start/1, request/6, request/7, request/8, locate/2, locate/3]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2, stop/2]).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 7).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
start() ->
    ignore.

start(Opts) ->
    gen_server:start_link(orber_iiop_outproxy, Opts, []).

request(Pid, ObjKey, Op, Parameters, TypeCodes, ResponseExpected) ->
    request(Pid, ObjKey, Op, Parameters, TypeCodes, ResponseExpected, 
	    infinity, orber:giop_version()).
request(Pid, ObjKey, Op, Parameters, TypeCodes, ResponseExpected, Timeout) ->
    request(Pid, ObjKey, Op, Parameters, TypeCodes, ResponseExpected, 
	    Timeout, orber:giop_version()).
request(Pid, ObjKey, Op, Parameters, TypeCodes, ResponseExpected, Timeout,
	Version) ->
    gen_server:call(Pid, {request, ObjKey, Op, Parameters, TypeCodes, 
			  ResponseExpected, Timeout, Version}, infinity).

locate(Pid, ObjKey) ->
    locate(Pid, ObjKey, orber:giop_version()).
locate(Pid, ObjKey, Version) ->
    gen_server:call(Pid, {locate, ObjKey, Version}).

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
    case catch orber_socket:connect(ssl, Host, Port, SocketOptions) of
	{'EXCEPTION', E} ->
	    ignore;
	%% We used to reply the below but since this would generate a CRASH REPORT
	%% if '-boot start_sasl' used. Due to a request to change this behaviour
	%% we changed this.
	%% {stop, {'EXCEPTION', E}};
	Socket ->
	    CodeSetCtx = #'CONV_FRAME_CodeSetContext'{char_data =  ?ISO8859_1_ID, 
						      wchar_data = ?ISO_10646_UCS_2_ID},
	    Ctx = [#'IOP_ServiceContext'{context_id=?IOP_CodeSets, context_data = CodeSetCtx}],
	    Timeout = orber:iiop_connection_timeout(),
	    {ok, {Socket, ssl, ets:new(orber_outgoing_requests, [set]),
		  Timeout, orber:get_interceptors(), Ctx}, Timeout}
    end;
init({connect, Host, Port, SocketType, SocketOptions}) ->
    ?PRINTDEBUG2("orber_iiop_outproxy init (~p): ~p ", [SocketType, self()]),
    process_flag(trap_exit, true), 
    case catch orber_socket:connect(SocketType, Host, Port, SocketOptions) of
	{'EXCEPTION', E} ->
	    ignore;
	%% We used to reply the below but since this would generate a CRASH REPORT
	%% if '-boot start_sasl' used. Due to a request to change this behaviour
	%% we changed this.
	%% {stop, {'EXCEPTION', E}};
	Socket ->
	    CodeSetCtx = #'CONV_FRAME_CodeSetContext'{char_data =  ?ISO8859_1_ID, 
						      wchar_data = ?ISO_10646_UCS_2_ID},
	    Ctx = [#'IOP_ServiceContext'{context_id=?IOP_CodeSets, context_data = CodeSetCtx}],
	    Timeout = orber:iiop_connection_timeout(),
	    {ok, {Socket, SocketType, ets:new(orber_outgoing_requests, [set]),
		  Timeout, orber:get_interceptors(), Ctx}, Timeout}
    end.

%%-----------------------------------------------------------------
%% Func: terminate/2
%%-----------------------------------------------------------------
terminate(normal, {_, _, OutRequests, _, _, _}) ->
    %% Kill all proxies and delete table before terminating
    kill_all_requests(OutRequests, ets:first(OutRequests)),
    ets:delete(OutRequests),
    ok;
terminate(Reason, {_, _, OutRequests, _, _, _}) ->
    %% Kill all proxies and delete table before terminating
    kill_all_requests(OutRequests, ets:first(OutRequests)),
    ets:delete(OutRequests),
    orber:debug_level_print("[~p] orber_iiop_outproxy:terminate(~p)", [?LINE, Reason], ?DEBUG_LEVEL),
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
handle_call({request, ObjKey, Op, Parameters, TypeCodes, ResponseExpected, Timeout,
	     Version},
	    From, {Socket, SocketType, OutRequests, ConnectTimeout, Interceptors, Ctx}) ->
    RequestId = orber_request_number:get(),
   {ok,  Pid} = orber_iiop_outrequest:start(),
    ets:insert(OutRequests, {RequestId, Pid}),
    orber_iiop_outrequest:request(Pid, RequestId, ObjKey, Op, Parameters,
				  TypeCodes, ResponseExpected, From, Socket,
				  SocketType, Timeout, Version, Interceptors, Ctx),
    {noreply, {Socket, SocketType, OutRequests, ConnectTimeout, Interceptors, []}, 
     ConnectTimeout};
handle_call({locate, ObjKey}, From, {Socket, SocketType, OutRequests, 
				     ConnectTimeout, Version, Interceptors, Ctx}) ->
    RequestId = orber_request_number:get(),
    {ok,  Pid} = orber_iiop_outrequest:start(),
    ets:insert(OutRequests, {RequestId, Pid}),
    orber_iiop_outrequest:locate(Pid, RequestId, ObjKey, From, Socket,
				SocketType, Version),
    {noreply, {Socket, SocketType, OutRequests, ConnectTimeout, Interceptors, Ctx}, 
     ConnectTimeout};
handle_call(stop, From, State) ->
    {stop, normal, ok, State};
handle_call(X, _, {Socket, SocketType, OutRequests, ConnectTimeout, Interceptors, Ctx}) ->
    ?PRINTDEBUG2("No match for ~p in orber_iiop_outproxy:handle_call", [X]),
    {noreply, {Socket, SocketType, OutRequests, ConnectTimeout, Interceptors, Ctx}, 
     ConnectTimeout}.

%%-----------------------------------------------------------------
%% Func: handle_cast/2
%%-----------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_, {Socket, SocketType, OutRequests, ConnectTimeout, Interceptors, Ctx}) ->
    {noreply, {Socket, SocketType, OutRequests, ConnectTimeout, Interceptors, Ctx}, 
     ConnectTimeout}.

%%-----------------------------------------------------------------
%% Func: handle_info/2
%%-----------------------------------------------------------------
%% Trapping exits 
handle_info({tcp_closed, Socket}, State) ->
    %% Maybee send a com failure to all clients. ?????
    {stop, normal, State}; 
handle_info({tcp, Socket, Bytes}, {Socket, SocketType, OutRequests, Timeout, 
				   Interceptors, Ctx}) ->
    handle_reply(Bytes, Socket, SocketType, OutRequests, Interceptors),
    {noreply, {Socket, SocketType, OutRequests, Timeout, Interceptors, Ctx}, Timeout};
handle_info({tcp_error, Socket, Reason}, State) ->
    corba:raise(#'INV_FLAG'{completion_status=?COMPLETED_NO});

handle_info({ssl_closed, Socket}, State) ->
    {stop, normal, State};
handle_info({ssl, Socket, Bytes}, {Socket, SocketType, OutRequests, Timeout, 
				   Interceptors, Ctx}) ->
    handle_reply(Bytes, Socket, SocketType, OutRequests, Interceptors),
    {noreply, {Socket, SocketType, OutRequests, Timeout, Interceptors, Ctx}, Timeout};
handle_info({ssl_error, Socket}, State) ->
    corba:raise(#'INV_FLAG'{completion_status=?COMPLETED_NO});

handle_info({'EXIT', Pid, normal}, {Socket, SocketType, OutRequests, Timeout, 
				    Interceptors, Ctx}) ->
    [{K, _}] = ets:match_object(OutRequests, {'$1', Pid}),
    ets:delete(OutRequests, K),
    {noreply, {Socket, SocketType, OutRequests, Timeout, Interceptors, Ctx}, Timeout};
handle_info({'EXIT', Pid, Reason}, {Socket, SocketType, OutRequests, Timeout, 
				    Interceptors, Ctx}) ->
    ?PRINTDEBUG2("out request ~p finished with reason ~p", [Pid, Reason]),
    [{K, _}] = ets:match_object(OutRequests, {'$1', Pid}),
    ets:delete(OutRequests, K),
    {noreply, {Socket, SocketType, OutRequests, Timeout, Interceptors, Ctx}, Timeout};
handle_info(stop, State) ->
    {stop, normal, State};
handle_info(timeout, {Socket, SocketType, OutRequests, Timeout, Interceptors, Ctx}) ->
    case ets:info(OutRequests, size) of
	0 ->
	    %% No pending requests, close the connection.
	    ?PRINTDEBUG2("IIOP connection via socket ~p timeout", [Socket]),
	    {stop, normal, {Socket, SocketType, OutRequests, Timeout, Interceptors, Ctx}};
	Amount ->
	    %% Still pending request, cannot close the connection.
	    ?PRINTDEBUG2("IIOP connection via socket ~p not timed out; ~p pending request(s)", 
			 [Socket, Amount]),
	    {noreply, {Socket, SocketType, OutRequests, Timeout, Interceptors, Ctx}, 
	     Timeout}
    end;
handle_info(X, {Socket, SocketType, OutRequests, Timeout, Interceptors, Ctx}) ->
    ?PRINTDEBUG2("No match for ~p in orber_iiop_outproxy:handle_info", [X]),
    {noreply, {Socket, SocketType, OutRequests, Timeout, Interceptors, Ctx}, Timeout}.


handle_reply(Bytes, Socket, SocketType, OutRequests, Interceptors) ->
    %% Check IIOP headers and fetch request id
    case catch checkheaders(Bytes) of
	{'EXCEPTION', DecodeException} ->
	    orber:debug_level_print("[~p] orber_iiop_outproxy:handle_reply(~p); decode exception(~p).", 
				    [?LINE, Bytes, DecodeException], ?DEBUG_LEVEL);
	{'EXIT', message_error} ->
	    orber:debug_level_print("[~p] orber_iiop_outproxy:handle_reply(~p); message error.", 
				    [?LINE, Bytes], ?DEBUG_LEVEL),
	    ME = cdr_encode:enc_message_error(orber:giop_version()),
	    orber_socket:write(SocketType, Socket, ME);
	{'EXIT', R} ->
	    orber:debug_level_print("[~p] orber_iiop_outproxy:handle_reply(~p); got exit(~p)", 
				    [?LINE, Bytes, R], ?DEBUG_LEVEL);
	{error, no_reply} ->
	    ok;
	{'reply', ReplyHeader, Rest, Len, ByteOrder} ->
	    case ets:lookup(OutRequests, ReplyHeader#reply_header.request_id) of
		[{_, Pid}] ->
		    %% Send reply to the correct request process
		    orber_iiop_outrequest:reply(Pid, ReplyHeader, Rest, 
						Len, ByteOrder, Bytes, Interceptors);
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
	    orber:debug_level_print("[~p] orber_iiop_outproxy:handle_reply(~p); message error(~p).", 
				    [?LINE, Bytes, X], ?DEBUG_LEVEL)
    end.


%%-----------------------------------------------------------------
%% Func: code_change/3
%%-----------------------------------------------------------------
code_change({down, OldVsn}, {Socket, SocketType, OutRequests, Timeout}, iiop_connection_timeout) ->
    ?PRINTDEBUG2("OutProxy-~p updated to no longer alllow iiop_connection_timeout", 
		 [OldVsn]),
    {ok, {Socket, SocketType, OutRequests}};
code_change(OldVsn, {Socket, SocketType, OutRequests}, iiop_connection_timeout) ->
    Timeout = orber:iiop_connection_timeout(),
    ?PRINTDEBUG2("OutProxy-~p updated to alllow iiop_connection_timeout ~p ", 
		 [OldVsn, Timeout]),
    {ok, {Socket, SocketType, OutRequests, Timeout}};
code_change({down, OldVsn}, 
	    {Socket, SocketType, OutRequests, Timeout, _, _},interceptors) ->
    {ok, {Socket, SocketType, OutRequests, Timeout}};
code_change(OldVsn, {Socket, SocketType, OutRequests, Timeout}, interceptors) ->
    {ok, {Socket, SocketType, OutRequests, Timeout, orber:get_interceptors(), []}};
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
	Other ->
	    orber:debug_level_print("[~p] orber_iiop_outproxy:checkheaders(~p); returned ~p", 
				    [?LINE, Message, Other], ?DEBUG_LEVEL),
	    {error, no_reply}
    end.
    
