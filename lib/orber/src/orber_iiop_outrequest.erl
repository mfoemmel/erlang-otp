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
-export([start/1, request/5, reply/6, locate/5, locate_reply/5]).



%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2, stop/2]).

%%-----------------------------------------------------------------
%% Macros
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 8).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
start(ReplyTo) ->
    gen_server:start_link(orber_iiop_outrequest, ReplyTo, []).

request(Pid, Socket, SocketType, Timeout, Msg) ->
    gen_server:cast(Pid, {request, Socket, SocketType, Timeout, Msg}).

reply(Pid, ReplyHeader, Rest, Len, ByteOrder, Bytes) ->
    gen_server:cast(Pid, {reply, ReplyHeader, Rest, Len, ByteOrder, Bytes}).

locate(Pid, Msg, Socket, SocketType, Timeout) ->
    gen_server:cast(Pid, {locate_request, Msg, Socket, SocketType, Timeout}).

locate_reply(Pid, ReplyHeader, Rest, Len, ByteOrder) ->
    gen_server:cast(Pid, {locate_reply, ReplyHeader, Rest, Len, ByteOrder}).

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
handle_call(stop, From, ReplyTo) ->
    gen_server:reply(ReplyTo, 
		     {'EXCEPTION', #'COMM_FAILURE'{minor=107, 
						   completion_status=?COMPLETED_MAYBE}}),
    orber:debug_level_print("[~p] orber_iiop_outrequest:handle_call(~p); stopped", 
			    [?LINE, ReplyTo], ?DEBUG_LEVEL),
    {stop, normal, ok, []};
handle_call(X, _, State) ->
    ?PRINTDEBUG2("orber_iiop_outrequest:handle_call(~p) - Unknown", [X]),
    {noreply, State}.

%%-----------------------------------------------------------------
%% Func: handle_cast/2
%%-----------------------------------------------------------------
handle_cast({request, Socket, SocketType, Timeout, Msg}, ReplyTo) ->
    orber_socket:write(SocketType, Socket, Msg),
    if
	integer(Timeout) ->
	    ?PRINTDEBUG2("IIOP request has set user defined timeout: ~p", [Timeout]),
	    {noreply, ReplyTo, Timeout};
	true ->
	    ?PRINTDEBUG2("IIOP request has set the iiop_timeout: ~p", 
			 [orber:iiop_timeout()]),
	    {noreply, ReplyTo, orber:iiop_timeout()}
    end;
handle_cast({reply, ReplyHeader, Rest, Len, ByteOrder, Bytes}, ReplyTo) ->
    gen_server:reply(ReplyTo, {reply, ReplyHeader, Rest, Len, ByteOrder, Bytes}),
    {stop, normal, []};
handle_cast({locate_request, Request, Socket, SocketType, Timeout}, ReplyTo) ->
    %% Here we must handle Interceptors
    orber_socket:write(SocketType, Socket, Request),
    if
	integer(Timeout) ->
	    {noreply, ReplyTo, Timeout};
	true ->
	    {noreply, ReplyTo, orber:iiop_timeout()}
    end;
handle_cast({locate_reply, ReplyHdr, Rest, Len, ByteOrder}, ReplyTo) ->
    %% Here we must handle Interceptors
    gen_server:reply(ReplyTo, {locate_reply, ReplyHdr, Rest, Len, ByteOrder}),
    {stop, normal, []};
handle_cast(stop, ReplyTo) ->
    gen_server:reply(ReplyTo, 
		     {'EXCEPTION', #'COMM_FAILURE'{minor=107, 
						   completion_status=?COMPLETED_MAYBE}}),    
    orber:debug_level_print("[~p] orber_iiop_outrequest:handle_cast(~p); stopped", 
			    [?LINE, ReplyTo], ?DEBUG_LEVEL),
    {stop, normal, []};
handle_cast(X, State) ->
    ?PRINTDEBUG2("orber_iiop_outrequest:handle_cast(~p) - Unknown", [X]),
    {noreply, State}.

%%-----------------------------------------------------------------
%% Func: handle_info/2
%%-----------------------------------------------------------------
handle_info(stop, ReplyTo) ->
    gen_server:reply(ReplyTo, 
		     {'EXCEPTION', #'COMM_FAILURE'{minor=107, 
						   completion_status=?COMPLETED_MAYBE}}),    
    orber:debug_level_print("[~p] orber_iiop_outrequest:handle_info(~p); stopped", 
			    [?LINE, ReplyTo], ?DEBUG_LEVEL),
    {stop, normal, ReplyTo};
handle_info(timeout, ReplyTo) ->
    gen_server:reply(ReplyTo, 
		     {'EXCEPTION', #'COMM_FAILURE'{minor=107, 
						   completion_status=?COMPLETED_MAYBE}}),
    orber:debug_level_print("[~p] orber_iiop_outrequest:handle_info(~p); timeout", 
			    [?LINE, ReplyTo], ?DEBUG_LEVEL),
    {stop, normal, ReplyTo};
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


