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
%% File: orber_iiop_inproxy.erl
%% 
%% Description:
%%    This file contains the IIOP "proxy" for incomming connections
%%
%% Creation date: 990425
%%
%%-----------------------------------------------------------------
-module(orber_iiop_inproxy).

-behaviour(gen_server).

-include_lib("orber/src/orber_iiop.hrl").
-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_debug.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/0, start/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2, stop/0]).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 7).

-record(state, {stype, socket, db, interceptors, ssl_port, timeout,
		partial_security}).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: start/0
%%-----------------------------------------------------------------
start() ->
    ignore.

%%-----------------------------------------------------------------
%% Func: start/1
%%-----------------------------------------------------------------
start(Opts) ->
    gen_server:start_link(orber_iiop_inproxy, Opts, []).

%%-----------------------------------------------------------------
%% Internal interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: stop/0 (Only used for test purpose !!!!!!)
%%-----------------------------------------------------------------
stop() ->
    gen_server:call(orber_iiop_inproxy, stop).

%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: init/1
%%-----------------------------------------------------------------
init({connect, Type, Socket}) ->
    process_flag(trap_exit, true),
    SSLPort = orber:iiop_ssl_port(),
    Timeout = orber:iiop_in_connection_timeout(),
    PartialSec = orber:partial_security(),
    case orber:get_interceptors() of
	false ->
	    {ok, #state{stype = Type, 
			socket = Socket, 
			db =  ets:new(orber_incoming_requests, [set]), 
			interceptors = false,
			ssl_port = SSLPort,
			timeout = Timeout,
			partial_security = PartialSec}, Timeout};
	{native, PIs} ->
	    {Address, Port} = orber_socket:peername(Type, Socket),
	    {ok, #state{stype = Type, 
			socket = Socket, 
			db =  ets:new(orber_incoming_requests, [set]), 
			interceptors = {native, 
					orber_pi:new_in_connection(PIs, 
								   Address, 
								   Port), 
					PIs},
			ssl_port = SSLPort,
			timeout = Timeout,
			partial_security = PartialSec}, Timeout};
	{Type, PIs} ->
	    {ok, #state{stype = Type, 
			socket = Socket, 
			db =  ets:new(orber_incoming_requests, [set]), 
			interceptors = {Type, PIs},
			ssl_port = SSLPort,
			timeout = Timeout,
			partial_security = PartialSec}, Timeout}
    end.

%%-----------------------------------------------------------------
%% Func: terminate/2
%%-----------------------------------------------------------------
%% We may want to kill all proxies before terminating, but the best
%% option should be to let the requests complete (especially for one-way
%% functions it's a better alternative.
terminate(Reason, #state{db = IncRequests,
			 interceptors = Interceptors}) ->
    ets:delete(IncRequests),
    case Interceptors of 
	false ->
	    ok;
	{native, Ref, PIs} ->
	    orber_pi:closed_in_connection(PIs, Ref);
	{Type, PIs} ->
	    ok
    end.

%%-----------------------------------------------------------------
%% Func: handle_call/3
%%-----------------------------------------------------------------
handle_call(stop, From, State) ->
    {stop, normal, ok, State};
handle_call(_, _, State) ->
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
%% Normal invocation
handle_info({tcp, Socket, Bytes}, State) ->
    handle_msg(normal, Socket, Bytes, State);
handle_info({ssl, Socket, Bytes}, State) ->
    handle_msg(ssl, Socket, Bytes, State);
%% Errors, closed connection
handle_info({tcp_closed, Socket}, State) ->
    {stop, normal, State};
handle_info({tcp_error, Socket}, State) ->
    {stop, normal, State};
handle_info({ssl_closed, Socket}, State) ->
    {stop, normal, State};
handle_info({ssl_error, Socket}, State) ->
    {stop, normal, State};
%% Servant termination.
handle_info({'EXIT', Pid, normal}, State) ->
    ets:delete(State#state.db, Pid),
    {noreply, State, State#state.timeout};
handle_info({message_error, Pid, ReqId}, State) ->
    ets:delete(State#state.db, ReqId),
    {noreply, State, State#state.timeout};
handle_info(timeout, State) ->
    case ets:info(State#state.db, size) of
	0 ->
	    %% No pending requests, close the connection.
	    ?PRINTDEBUG2("IIOP in connection timeout", []),
	    {stop, normal, State};
	Amount ->
	    %% Still pending request, cannot close the connection.
	    ?PRINTDEBUG2("IIOP in connection not timed out; ~p pending request(s)", 
			 [Amount]),
	    {noreply, State, State#state.timeout}
    end;
handle_info(X,State) ->
    {noreply, State, State#state.timeout}.

handle_msg(Type, Socket, Bytes, #state{stype = Type, 
				       socket = Socket,
				       interceptors = Interceptors,
				       ssl_port = SSLPort,
				       partial_security = PartialSec} = State) ->
    case catch cdr_decode:dec_giop_message_header(Bytes) of
	%% Only when using IIOP-1.2 may the client send this message. 
	%% Introduced in CORBA-2.6
	#giop_message{message_type = ?GIOP_MSG_CLOSE_CONNECTION, 
		      giop_version = {1,2}} ->
	    {stop, normal, State};
	#giop_message{message_type = ?GIOP_MSG_CANCEL_REQUEST} = GIOPHdr ->
	    ReqId = cdr_decode:peek_request_id(GIOPHdr#giop_message.byte_order,
					       GIOPHdr#giop_message.message),
	    case ets:lookup(State#state.db, ReqId) of
		[{RId, PPid}] ->
		    ets:delete(State#state.db, RId),
		    PPid ! {self(), cancel_request_header};
		[] ->
		    send_msg_error(Type, Socket, Bytes, "No such fragment id")
	    end,
	    {noreply, State, State#state.timeout};
	#giop_message{message_type = ?GIOP_MSG_CLOSE_CONNECTION} ->
	    {noreply, State, State#state.timeout};
	%% A fragment; we must hav received a Request or LocateRequest
	%% with fragment-flag set to true.
	%% We need to decode the header to get the request-id.
	#giop_message{message_type = ?GIOP_MSG_FRAGMENT,
		      giop_version = {1,2}} = GIOPHdr ->
	    ReqId = cdr_decode:peek_request_id(GIOPHdr#giop_message.byte_order,
					       GIOPHdr#giop_message.message),
	    case ets:lookup(State#state.db, ReqId) of
		[{RId, PPid}] when GIOPHdr#giop_message.fragments == true ->
		    PPid ! {self(), GIOPHdr};
		[{RId, PPid}] ->
		    ets:delete(State#state.db, RId),
		    PPid ! {self(), GIOPHdr};
		[] ->
		    send_msg_error(Type, Socket, Bytes, "No such fragment id")
	    end,
	    {noreply, State, State#state.timeout};
	%% Must be a Request or LocateRequest which have been fragmented.
	%% We need to decode the header to get the request-id.
	#giop_message{fragments = true,
		      giop_version = {1,2}} = GIOPHdr ->
	    ReqId = cdr_decode:peek_request_id(GIOPHdr#giop_message.byte_order,
					       GIOPHdr#giop_message.message),
	    Pid = orber_iiop_inrequest:start_fragment_collector(GIOPHdr, Bytes, 
								Type, Socket, 
								Interceptors, 
								ReqId, self(),
								SSLPort,
								PartialSec),
	    ets:insert(State#state.db, {Pid, ReqId}),
	    ets:insert(State#state.db, {ReqId, Pid}),
	    {noreply, State, State#state.timeout};
	GIOPHdr when record(GIOPHdr, giop_message) ->
	    Pid = orber_iiop_inrequest:start(GIOPHdr, Bytes, Type, Socket,
					     Interceptors, SSLPort, PartialSec),
	    ets:insert(State#state.db, {Pid, undefined}),
	    {noreply, State, State#state.timeout};
	message_error ->
	    send_msg_error(Type, Socket, Bytes, "Unable to decode the GIOP-header"),
	    {noreply, State, State#state.timeout}
    end;
handle_msg(Type, _, Bytes, State) ->
    orber:dbg("[~p] orber_iiop_inproxy:handle_msg(~p); 
Received a message from a socket of a different type.
Should be ~p but was ~p.", [?LINE, Bytes, State#state.stype, Type], ?DEBUG_LEVEL),
    {noreply, State, State#state.timeout}.

send_msg_error(Type, Socket, Data, Msg) ->
    orber:dbg("[~p] orber_iiop_inproxy:handle_msg(~p); 
~p.", [?LINE, Data, Msg], ?DEBUG_LEVEL),
    Reply = cdr_encode:enc_message_error(orber:giop_version()),
    orber_socket:write(Type, Socket, Reply).

%%-----------------------------------------------------------------
%% Func: code_change/3
%%-----------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

