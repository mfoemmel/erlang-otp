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
%% 
%% Description:
%%    This file handles the H.248 UDP connections.
%%
%%-----------------------------------------------------------------
-module(megaco_udp_server).

-behaviour(gen_server).

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include_lib("megaco/src/udp/megaco_udp.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([
	 start_link/1,
	 stop/1
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
%% Description: Starts the process that keeps track of an UDP 
%%              socket.
%%-----------------------------------------------------------------
start_link(Arg) ->
    gen_server:start_link(?MODULE, Arg, []).

%%-----------------------------------------------------------------
%% Func: stop/1
%% Description: Stops the process that keeps track of an UDP 
%%              socket.
%%-----------------------------------------------------------------
stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

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
    {ok, Arg}.

%%-----------------------------------------------------------------
%% Func: terminate/2
%% Description: Termination function for the generic server
%%-----------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%-----------------------------------------------------------------
%% Func: handle_call/3
%% Description: Handling call messages (really just stop and garbage)
%%-----------------------------------------------------------------
handle_call(stop, From, UdpRec) ->
    Reply = do_stop(UdpRec),
    {stop, shutdown, Reply, UdpRec};
handle_call(Request, From, UdpRec) ->
    error_logger:error_report([{?MODULE, {garbage_call, Request, From}}]),
    {noreply, UdpRec}.

%%-----------------------------------------------------------------
%% Func: handle_cast/2
%% Description: Handling cast messages (really just stop and garbage)
%%-----------------------------------------------------------------
handle_cast(stop, UdpRec) ->
    do_stop(UdpRec),
    {stop, shutdown, UdpRec};
handle_cast(Request, UdpRec) ->
    error_logger:error_report([{?MODULE, {garbage_cast, Request}}]),
    {noreply, UdpRec}.

%%-----------------------------------------------------------------
%% Func: handle_info/2
%% Description: Handling non call/cast messages. Incomming messages
%%              from the socket and exit codes.
%%-----------------------------------------------------------------
handle_info({udp, _UdpId, Ip, Port, Msg}, UdpRec) ->
    Socket = UdpRec#megaco_udp.socket,
    Mod    = UdpRec#megaco_udp.module,
    RH = UdpRec#megaco_udp.receive_handle,
    SH = megaco_udp:create_send_handle(Socket, Ip, Port), 
    case size(Msg) of
	Sz when Sz < ?GC_MSG_LIMIT ->
	    apply(Mod, receive_message, [RH, self(), SH, Msg]);
	Sz ->
	    receive_message(Mod, RH, SH, Sz, Msg)
    end,
    inet:setopts(Socket, [{active, once}]),
    {noreply, UdpRec};
handle_info(Info, UdpRec) ->
    error_logger:error_report([{?MODULE, {garbage_info, Info}}]),
    {noreply, UdpRec}.


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
code_change(OldVsn, State, Extra) ->
    {ok, State}.

do_stop(UdpRec) ->
    Socket = UdpRec#megaco_udp.socket,
    gen_udp:close(Socket).
