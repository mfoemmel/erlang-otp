%%-----------------------------------------------------------------
%%
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
%% 
%%     $Id$
%%
%%-----------------------------------------------------------------
%%
%% Purpose: Handles the Megaco/H.248 TCP connections.
%%
%%-----------------------------------------------------------------
-module(megaco_tcp_connection).

-behaviour(gen_server).


%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------

-include_lib("megaco/src/tcp/megaco_tcp.hrl").


%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([
	 start_link/1
	]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------

-export([
	 init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 code_change/3, 
	 terminate/2,

	 handle_received_message/5
	]).


%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: start_link/1
%% Description: Starts the proces that keeps track of an TCP 
%%              connection.
%%-----------------------------------------------------------------

start_link(Arg) ->
    gen_server:start_link(?MODULE, Arg, []).


%%-----------------------------------------------------------------
%% Internal interface functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: init/1
%% Description: Init funcion for the generic server
%%-----------------------------------------------------------------
init(Arg) ->
%%    process_flag(trap_exit, true),
    {ok, Arg}.

%%-----------------------------------------------------------------
%% Func: terminate/2
%% Description: Termination function for the generic server
%%-----------------------------------------------------------------
terminate(Reason, TcpRec) ->
    ok.

%%-----------------------------------------------------------------
%% Func: handle_call/3
%% Description: Handling call messages (really just stop and garbage)
%%-----------------------------------------------------------------
handle_call(stop, From, TcpRec) ->
    {stop, shutdown, ok, TcpRec};
handle_call(_, _, TcpRec) ->
    {noreply, TcpRec}.

%%-----------------------------------------------------------------
%% Func: handle_cast/2
%% Description: Handling cast messages (really just stop and garbage)
%%-----------------------------------------------------------------
handle_cast(stop, TcpRec) ->
    {stop, shutdown, TcpRec};
handle_cast(_, TcpRec) ->
    {noreply, TcpRec}.

%%-----------------------------------------------------------------
%% Func: handle_info/2
%% Description: Handling non call/cast messages. Incomming messages
%%              from the socket and exit messages.
%%-----------------------------------------------------------------
handle_info({tcp_closed, Socket}, TcpRec) ->
    {stop, shutdown, TcpRec};
handle_info({tcp_error, Socket}, TcpRec) ->
    {stop, shutdown, TcpRec};
handle_info({tcp, Socket, <<3:8, X:8, Length:16, Msg/binary>>}, TcpRec) 
  when Length < ?GC_MSG_LIMIT ->
    Mod = TcpRec#megaco_tcp.module,
    RH = TcpRec#megaco_tcp.receive_handle,
    Socket = TcpRec#megaco_tcp.socket,
    apply(Mod, receive_message, [RH, self(), Socket, Msg]),
    inet:setopts(Socket, [{active, once}]),
    {noreply, TcpRec};
handle_info({tcp, Socket, <<3:8, X:8, Length:16, Msg/binary>>}, TcpRec) ->
    Mod = TcpRec#megaco_tcp.module,
    RH = TcpRec#megaco_tcp.receive_handle,
    Socket = TcpRec#megaco_tcp.socket,
    receive_message(Mod, RH, Socket, Length, Msg),
    inet:setopts(Socket, [{active, once}]),
    {noreply, TcpRec};
handle_info({tcp, Socket, Msg}, TcpRec) ->
    error_logger:error_report([{megaco_tcp, {bad_tpkt_packet, Msg}}]),
    {noreply, TcpRec};
handle_info(X, TcpRec) ->
    error_logger:format("<WARNING> Ignoring unexpected event: ~p~n", [X]),
    {noreply, TcpRec}.


receive_message(Mod, RH, SendHandle, Length, Msg) ->
    Opts = [link , {min_heap_size, ?HEAP_SIZE(Length)}],
    spawn_opt(?MODULE, handle_received_message,
               [Mod, RH, self(), SendHandle, Msg], Opts),
    ok.


handle_received_message(Mod, RH, Parent, SH, Msg) ->
    apply(Mod, process_received_message,[RH, Parent, SH, Msg]),
    unlink(Parent),
    exit(normal).

    
%%-----------------------------------------------------------------
%% Func: code_change/3
%% Descrition: Handles code change messages during upgrade.
%%-----------------------------------------------------------------
code_change(OldVsn, C, Extra) ->
    {ok, C}.



