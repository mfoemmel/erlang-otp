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
-export([start/0, start/1, request/5, locate/4]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2, stop/2, stop/1, checkheaders/1]).

%%-----------------------------------------------------------------
%% Macros/Defines
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 7).

-record(state, {stype, socket, db, timeout, client_timeout}).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
start() ->
    ignore.

start(Opts) ->
    gen_server:start_link(orber_iiop_outproxy, Opts, []).

request(Pid, true, Timeout, Msg, RequestId) ->
    gen_server:call(Pid, {request, Timeout, Msg, RequestId}, infinity);
request(Pid, ResponseExpected, Timeout, Msg, RequestId) ->
    %% No response expected
    gen_server:call(Pid, {oneway_request, Timeout, Msg}, infinity).

locate(Pid, Msg, RequestId, Timeout) ->
    gen_server:call(Pid, {locate, Msg, RequestId, Timeout}, infinity).

%%-----------------------------------------------------------------
%% Internal interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: stop/2
%%-----------------------------------------------------------------
stop(Pid, Timeout) ->
    gen_server:call(Pid, stop, Timeout).
stop(Pid) ->
    gen_server:cast(Pid, stop).


%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: init/1
%%-----------------------------------------------------------------
init({connect, Host, Port, SocketType, SocketOptions}) ->
    process_flag(trap_exit, true), 
    case catch orber_socket:connect(SocketType, Host, Port, SocketOptions) of
	{'EXCEPTION', E} ->
	    ignore;
	%% We used to reply the below but since this would generate a CRASH REPORT
	%% if '-boot start_sasl' used. Due to a request to change this behaviour
	%% we did.
	%% {stop, {'EXCEPTION', E}};
	Socket ->
	    Timeout = orber:iiop_connection_timeout(),
	    {ok, #state{stype = SocketType, socket = Socket,
			db = ets:new(orber_outgoing_requests, [set]),
			timeout = Timeout,
			client_timeout = orber:iiop_timeout()}, Timeout}
    end.

%%-----------------------------------------------------------------
%% Func: terminate/2
%%-----------------------------------------------------------------
terminate(normal, #state{db = OutRequests}) ->
    %% Kill all proxies and delete table before terminating
    kill_all_requests(OutRequests, ets:first(OutRequests)),
    ets:delete(OutRequests),
    ok;
terminate(Reason, #state{db = OutRequests}) ->
    %% Kill all proxies and delete table before terminating
    kill_all_requests(OutRequests, ets:first(OutRequests)),
    ets:delete(OutRequests),
    orber:dbg("[~p] orber_iiop_outproxy:terminate(~p)", [?LINE, Reason], ?DEBUG_LEVEL),
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
handle_call({request, Timeout, Msg, RequestId}, From, State) ->
    {ok, Pid} = orber_iiop_outrequest:start(From, State#state.client_timeout),
    ets:insert(State#state.db, {RequestId, Pid}),
    orber_iiop_outrequest:request(Pid, State#state.socket, State#state.stype, 
				  Timeout, Msg),
    {noreply, State, State#state.timeout};
handle_call({oneway_request, Timeout, Msg}, From, State) ->
    orber_socket:write(State#state.stype, State#state.socket, Msg),
    {reply, ok, State, State#state.timeout};
handle_call({locate, Request, RequestId, Timeout}, From, State) ->
    {ok,  Pid} = orber_iiop_outrequest:start(From, State#state.client_timeout),
    ets:insert(State#state.db, {RequestId, Pid}),
    orber_iiop_outrequest:locate(Pid, Request, State#state.socket, 
				 State#state.stype, Timeout),
    {noreply, State, State#state.timeout};
handle_call(stop, From, State) ->
    {stop, normal, ok, State};
handle_call(X, _, State) ->
    ?PRINTDEBUG2("No match for ~p in orber_iiop_outproxy:handle_call", [X]),
    {noreply, State, State#state.timeout}.

%%-----------------------------------------------------------------
%% Func: handle_cast/2
%%-----------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_, State) ->
    {noreply, State, State#state.timeout}.

%%-----------------------------------------------------------------
%% Func: handle_info/2
%%-----------------------------------------------------------------
%% Trapping exits 
handle_info({tcp_closed, Socket}, State) ->
    {stop, normal, State}; 
handle_info({tcp, Socket, Bytes}, State) ->
    handle_reply(Bytes, State);
handle_info({tcp_error, Socket, Reason}, #state{socket = Socket}) ->
    corba:raise(#'INV_FLAG'{completion_status=?COMPLETED_NO});
handle_info({ssl_closed, Socket}, State) ->
    {stop, normal, State};
handle_info({ssl, Socket, Bytes}, State) ->
    handle_reply(Bytes, State);
handle_info({ssl_error, Socket}, #state{socket = Socket}) ->
    corba:raise(#'INV_FLAG'{completion_status=?COMPLETED_NO});
handle_info({'EXIT', Pid, Reason}, State) ->
    [{K, _}] = ets:match_object(State#state.db, {'$1', Pid}),
    ets:delete(State#state.db, K),
    {noreply, State, State#state.timeout};
handle_info(stop, State) ->
    {stop, normal, State};
handle_info(timeout, State) ->
    case ets:info(State#state.db, size) of
	0 ->
	    %% No pending requests, close the connection.
	    ?PRINTDEBUG2("IIOP connection timeout", []),
	    {stop, normal, State};
	Amount ->
	    %% Still pending request, cannot close the connection.
	    ?PRINTDEBUG2("IIOP connection not timed out; ~p pending request(s)", 
			 [Amount]),
	    {noreply, State, State#state.timeout}
    end;
handle_info(X, State) ->
    ?PRINTDEBUG2("No match for ~p in orber_iiop_outproxy:handle_info", [X]),
    {noreply, State, State#state.timeout}.


handle_reply(Bytes, State) ->
    %% Check IIOP headers and fetch request id
    case catch checkheaders(cdr_decode:dec_giop_message_header(Bytes)) of
	{'EXCEPTION', DecodeException} ->
	    orber:dbg("[~p] orber_iiop_outproxy:handle_reply(~p); decode exception(~p).", 
				    [?LINE, Bytes, DecodeException], ?DEBUG_LEVEL),
	    {noreply, State, State#state.timeout};
	{'EXIT', message_error} ->
	    orber:dbg("[~p] orber_iiop_outproxy:handle_reply(~p); message error.", 
				    [?LINE, Bytes], ?DEBUG_LEVEL),
	    ME = cdr_encode:enc_message_error(orber:giop_version()),
	    orber_socket:write(State#state.stype, State#state.socket, ME),
	    {noreply, State, State#state.timeout};
	{'EXIT', R} ->
	    orber:dbg("[~p] orber_iiop_outproxy:handle_reply(~p); got exit(~p)", 
				    [?LINE, Bytes, R], ?DEBUG_LEVEL),
	    {noreply, State, State#state.timeout};
	close_connection ->
	    orber:dbg("[~p] orber_iiop_outproxy:handle_reply();
The Server-side ORB closed the connection.", [?LINE], ?DEBUG_LEVEL),
	    {stop, normal, State};
	{error, no_reply} ->
	    {noreply, State, State#state.timeout};
	{fragment, GIOPHdr, ReqId} ->
	    case ets:lookup(State#state.db, ReqId) of
		[{_, Pid}] ->
		    %% Send reply to the correct request process
		    orber_iiop_outrequest:fragment(Pid, GIOPHdr), 
		    {noreply, State, State#state.timeout};
		_ ->
		    {noreply, State, State#state.timeout}
	    end;
	{fragmented, GIOPHdr, ReqId} ->
	    case ets:lookup(State#state.db, ReqId) of
		[{_, Pid}] ->
		    %% Send reply to the correct request process
		    orber_iiop_outrequest:fragmented(Pid, GIOPHdr, Bytes), 
		    {noreply, State, State#state.timeout};
		_ ->
		    {noreply, State, State#state.timeout}
	    end;
	{'reply', ReplyHeader, Rest, Len, ByteOrder} ->
	    case ets:lookup(State#state.db, ReplyHeader#reply_header.request_id) of
		[{_, Pid}] ->
		    %% Send reply to the correct request process
		    orber_iiop_outrequest:reply(Pid, ReplyHeader, Rest, 
						Len, ByteOrder, Bytes),
		    {noreply, State, State#state.timeout};
		_ ->
		    {noreply, State, State#state.timeout}
	    end;
	{'locate_reply', LocateReplyHeader, LocateRest, LocateLen, LocateByteOrder} ->
	    case ets:lookup(State#state.db, 
			    LocateReplyHeader#locate_reply_header.request_id) of
		[{_, Pid}] ->
		    %% Send reply to the correct request process
		    orber_iiop_outrequest:locate_reply(Pid, LocateReplyHeader, LocateRest,
						       LocateLen, LocateByteOrder),
		    {noreply, State, State#state.timeout};
		_ ->
		    {noreply, State, State#state.timeout}
	    end;
	X ->
	    orber:dbg("[~p] orber_iiop_outproxy:handle_reply(~p); message error(~p).", 
		      [?LINE, Bytes, X], ?DEBUG_LEVEL),
	    {noreply, State, State#state.timeout}
    end.


%%-----------------------------------------------------------------
%% Func: code_change/3
%%-----------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
checkheaders(#giop_message{message_type = ?GIOP_MSG_CLOSE_CONNECTION}) ->
    close_connection;
checkheaders(#giop_message{message_type = ?GIOP_MSG_FRAGMENT,
			   giop_version = {1,2}} = GIOPHdr) ->
    %% A fragment; we must hav received a Request or LocateRequest
    %% with fragment-flag set to true.
    %% We need to decode the header to get the request-id.
    ReqId = cdr_decode:peak_request_id(GIOPHdr#giop_message.byte_order,
				       GIOPHdr#giop_message.message),
    {fragment, GIOPHdr, ReqId};
checkheaders(#giop_message{fragments = true,
			   giop_version = {1,2}} = GIOPHdr) ->
    %% Must be a Reply or LocateReply which have been fragmented.
    %% We need to decode the header to get the request-id.
    ReqId = cdr_decode:peak_request_id(GIOPHdr#giop_message.byte_order,
				       GIOPHdr#giop_message.message),
    {fragmented, GIOPHdr, ReqId};
checkheaders(#giop_message{fragments = false, 
			   message_type = ?GIOP_MSG_REPLY} = GIOPHdr) ->
    {ReplyHeader, Rest, Len} = 
	cdr_decode:dec_reply_header(GIOPHdr#giop_message.giop_version,
				    GIOPHdr#giop_message.message, 
				    ?GIOP_HEADER_SIZE,
				    GIOPHdr#giop_message.byte_order),
    {'reply', ReplyHeader, Rest, Len, GIOPHdr#giop_message.byte_order};
checkheaders(#giop_message{fragments = false, 
			   message_type = ?GIOP_MSG_LOCATE_REPLY} = GIOPHdr) ->
    {LocateReplyHeader, Rest, Len} = 
	cdr_decode:dec_locate_reply_header(GIOPHdr#giop_message.giop_version,
					   GIOPHdr#giop_message.message, 
					   ?GIOP_HEADER_SIZE,
					   GIOPHdr#giop_message.byte_order),
    {'locate_reply', LocateReplyHeader, Rest, Len, GIOPHdr#giop_message.byte_order};
checkheaders(_) ->
    {error, no_reply}.

