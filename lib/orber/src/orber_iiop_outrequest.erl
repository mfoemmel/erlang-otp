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
%%--------------------------------------------------------------------
%% File: orber_iiop_outrequest.erl
%% 
%% Description:
%%    This file contains the handling of incommingoutgoing requests
%%
%% Creation date: 990426
%%
%%-----------------------------------------------------------------
-module(orber_iiop_outrequest).

-behaviour(gen_server).
-include_lib("orber/src/orber_iiop.hrl").
-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_debug.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/2, request/5, reply/6, locate/5, locate_reply/5]).



%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2, stop/2, fragment/2, fragmented/3]).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 8).

-record(state, {client, timeout, buffer = [], fragmented, bytes}).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
start(ReplyTo, Timeout) ->
    gen_server:start_link(orber_iiop_outrequest, #state{client = ReplyTo, 
							timeout = Timeout}, []).

request(Pid, Socket, SocketType, Timeout, Msg) ->
    gen_server:cast(Pid, {request, Socket, SocketType, Timeout, Msg}).

reply(Pid, ReplyHeader, Rest, Len, ByteOrder, Bytes) ->
    gen_server:cast(Pid, {reply, ReplyHeader, Rest, Len, ByteOrder, Bytes}).

locate(Pid, Msg, Socket, SocketType, Timeout) ->
    gen_server:cast(Pid, {locate_request, Msg, Socket, SocketType, Timeout}).

locate_reply(Pid, ReplyHeader, Rest, Len, ByteOrder) ->
    gen_server:cast(Pid, {locate_reply, ReplyHeader, Rest, Len, ByteOrder}).

fragment(Pid, GIOPHdr) ->
    gen_server:cast(Pid, {fragment, GIOPHdr}).

fragmented(Pid, GIOPHdr, Bytes) ->
    gen_server:cast(Pid, {fragmented, GIOPHdr, Bytes}).

%%-----------------------------------------------------------------
%% Internal interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: stop/2
%%-----------------------------------------------------------------
stop(Pid, Timeout) ->
    gen_server:call(Pid, stop, Timeout).

%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: init/1
%%-----------------------------------------------------------------
init(State) ->
    {ok, State}.

%%-----------------------------------------------------------------
%% Func: terminate/2
%%-----------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%-----------------------------------------------------------------
%% Func: handle_call/3
%%-----------------------------------------------------------------
handle_call(stop, From, State) ->
    gen_server:reply(State#state.client, 
		     {'EXCEPTION', #'COMM_FAILURE'{minor=(?ORBER_VMCID bor 3), 
						   completion_status=?COMPLETED_MAYBE}}),
    orber:dbg("[~p] orber_iiop_outrequest:handle_call(~p); stopped", 
	      [?LINE, State], ?DEBUG_LEVEL),
    {stop, normal, ok, State};
handle_call(X, _, State) ->
    ?PRINTDEBUG2("orber_iiop_outrequest:handle_call(~p) - Unknown", [X]),
    {noreply, State}.

%%-----------------------------------------------------------------
%% Func: handle_cast/2
%%-----------------------------------------------------------------
handle_cast({request, Socket, SocketType, Timeout, Msg}, State) ->
    orber_socket:write(SocketType, Socket, Msg),
    if
	integer(Timeout) ->
	    ?PRINTDEBUG2("IIOP request has set user defined timeout: ~p", [Timeout]),
	    {noreply, State, Timeout};
	true ->
	    ?PRINTDEBUG2("IIOP request has set the iiop_timeout: ~p", 
			 [State#state.timeout]),
	    {noreply, State, State#state.timeout}
    end;
handle_cast({reply, ReplyHeader, Rest, Len, ByteOrder, Bytes}, State) ->
    gen_server:reply(State#state.client, 
		     {reply, ReplyHeader, Rest, Len, ByteOrder, Bytes}),
    {stop, normal, State};
handle_cast({locate_request, Request, Socket, SocketType, Timeout}, State) ->
    orber_socket:write(SocketType, Socket, Request),
    if
	integer(Timeout) ->
	    {noreply, State, Timeout};
	true ->
	    {noreply, State, State#state.timeout}
    end;
handle_cast({locate_reply, ReplyHdr, Rest, Len, ByteOrder}, State) ->
    gen_server:reply(State#state.client, 
		     {locate_reply, ReplyHdr, Rest, Len, ByteOrder}),
    {stop, normal, State};
handle_cast({fragment, #giop_message{byte_order = ByteOrder,
				     message    = Message,
				     fragments  = true} = GIOPHdr}, State) ->
    %% There are more framents to come; just collect this message and wait for
    %% the rest.
    case catch cdr_decode:dec_message_header(null, GIOPHdr, Message) of
	{_, #fragment_header{}, FragBody, _, _} ->
	    {noreply, State#state{buffer=[FragBody|State#state.buffer]}, 
	     State#state.timeout};
	Other ->
	    gen_server:reply(State#state.client, 
			     {'EXCEPTION', #'MARSHAL'{minor=(?ORBER_VMCID bor 18), 
						      completion_status=?COMPLETED_YES}}),
	    orber:dbg("[~p] orber_iiop_outrequest:handle_cast(~p); stopped", 
		      [?LINE, State], ?DEBUG_LEVEL),
	    {stop, normal, State}
    end;	    
handle_cast({fragment, #giop_message{byte_order = ByteOrder,
				     message    = Message} = GIOPHdr}, 
	    #state{fragmented = InitGIOPHdr} = State) ->
    %% This is the last fragment. Now we can but together the fragments, decode
    %% the reply and send it to the client.
    case catch cdr_decode:dec_message_header(null, GIOPHdr, Message) of
	{_, #fragment_header{}, FragBody, _, _} ->
	    %% This buffer is all the fragments concatenated.
	    Buffer = lists:reverse([FragBody|State#state.buffer]),
	    
	    %% Create a GIOP-message which is exactly as if hadn't been fragmented.
	    NewGIOP = InitGIOPHdr#giop_message
			{message = list_to_binary([InitGIOPHdr#giop_message.message|Buffer]),
			 fragments = false},
	    case catch orber_iiop_outproxy:checkheaders(NewGIOP) of
		{'reply', ReplyHeader, Rest, Len, ByteOrder} ->
		    %% We must keep create a copy of all bytes, as if the message
		    %% wasn't fragmented, to be able handle TypeCode indirection.
		    gen_server:reply(State#state.client, 
				     {reply, ReplyHeader, Rest, Len, ByteOrder, 
				      list_to_binary([State#state.bytes|Buffer])});
		{'locate_reply', ReplyHdr, Rest, Len, ByteOrder} ->
		    gen_server:reply(State#state.client,
				     {locate_reply, ReplyHdr, Rest, Len, ByteOrder});
		Error ->
		    gen_server:reply(State#state.client, 
				     {'EXCEPTION', #'MARSHAL'{minor=(?ORBER_VMCID bor 18),
							      completion_status=?COMPLETED_YES}}),
		    orber:dbg("[~p] orber_iiop_outrequest:handle_cast(~p, ~p);
Unable to decode Reply or LocateReply header", 
			      [?LINE, NewGIOP, State], ?DEBUG_LEVEL)
	    end,
	    {stop, normal, State#state{buffer=Buffer, fragmented = NewGIOP}};
	Other ->
	    gen_server:reply(State#state.client, 
			     {'EXCEPTION', #'MARSHAL'{minor=(?ORBER_VMCID bor 18),
						      completion_status=?COMPLETED_YES}}),    
	    orber:dbg("[~p] orber_iiop_outrequest:handle_cast(~p); stopped", 
		      [?LINE, State], ?DEBUG_LEVEL),
	    {stop, normal, State}
    end;
handle_cast({fragmented, #giop_message{byte_order = ByteOrder,
				       message    = Message,
				       fragments  = true} = GIOPHdr, 
	     Bytes}, State) ->
    %% This the initial message (i.e. a LocateReply or Reply).
    {noreply, State#state{fragmented = GIOPHdr, bytes = Bytes}, State#state.timeout};
handle_cast(stop, State) ->
    gen_server:reply(State#state.client, 
		     {'EXCEPTION', #'COMM_FAILURE'{minor=(?ORBER_VMCID bor 3), 
						   completion_status=?COMPLETED_MAYBE}}),    
    orber:dbg("[~p] orber_iiop_outrequest:handle_cast(~p); stopped", 
	      [?LINE, State], ?DEBUG_LEVEL),
    {stop, normal, State};
handle_cast(X, State) ->
    ?PRINTDEBUG2("orber_iiop_outrequest:handle_cast(~p) - Unknown", [X]),
    {noreply, State}.

%%-----------------------------------------------------------------
%% Func: handle_info/2
%%-----------------------------------------------------------------
handle_info(stop, State) ->
    gen_server:reply(State#state.client, 
		     {'EXCEPTION', #'COMM_FAILURE'{minor=(?ORBER_VMCID bor 3), 
						   completion_status=?COMPLETED_MAYBE}}),    
    orber:dbg("[~p] orber_iiop_outrequest:handle_info(~p); stopped", 
	      [?LINE, State], ?DEBUG_LEVEL),
    {stop, normal, State};
handle_info(timeout, State) ->
    gen_server:reply(State#state.client, 
		     {'EXCEPTION', #'TIMEOUT'{completion_status=?COMPLETED_MAYBE}}),
    orber:dbg("[~p] orber_iiop_outrequest:handle_info(~p); timeout", 
	      [?LINE, State], ?DEBUG_LEVEL),
    {stop, normal, State};
handle_info(X,State) ->
    ?PRINTDEBUG2("orber_iiop_outrequest:handle_info(~p) - Unknown", [X]),
    {noreply, State}.

%%-----------------------------------------------------------------
%% Func: code_change/3
%%-----------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.


%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Utility Functions
%%-----------------------------------------------------------------


