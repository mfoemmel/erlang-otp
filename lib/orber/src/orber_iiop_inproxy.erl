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
%% Author: Lars Thorsen
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
    ?PRINTDEBUG2("orber_iiop_inproxy init: ~p ", [self()]),
    process_flag(trap_exit, true),
    {ok, {Socket, Type, ets:new(orber_incoming_requests, [set])}}.

%%-----------------------------------------------------------------
%% Func: terminate/2
%%-----------------------------------------------------------------
terminate(Reason, {Socket, Type, IncRequests}) ->
    ?PRINTDEBUG2("in proxy for socket ~p terminated with reason: ~p", [Socket, Reason]),
    %% Kill all proxies before terminating
    %%kill_all_proxies(IncRequests, ets:first(IncRequests)),
    ets:delete(IncRequests),
    ok.

kill_all_proxies(_, '$end_of_table') ->
    ok;
kill_all_proxies(IncRequests, Key) ->
    exit(Key, kill),
    kill_all_proxies(IncRequests, ets:next(IncRequests, Key)).

%%-----------------------------------------------------------------
%% Func: handle_call/3
%%-----------------------------------------------------------------
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
handle_info({tcp_closed, Socket}, State) ->
    {stop, normal, State};
handle_info({tcp_error, Socket}, State) ->
    {stop, normal, State};
handle_info({tcp, Socket, Bytes}, {Socket, normal, IncRequests}) ->
    Pid = orber_iiop_inrequest:start(Bytes, normal, Socket),
    ets:insert(IncRequests, {Pid, undefined}),
    {noreply, {Socket, normal, IncRequests}};
handle_info({ssl_closed, Socket}, State) ->
    {stop, normal, State};
handle_info({ssl_error, Socket}, State) ->
    {stop, normal, State};
handle_info({ssl, Socket, Bytes}, {Socket, ssl, IncRequests}) ->
    Pid = orber_iiop_inrequest:start(Bytes, ssl, Socket),
    ets:insert(IncRequests, {Pid, undefined}),
    {noreply, {Socket, ssl, IncRequests}};
%%% old sockets impl.
%handle_info({Socket, {socket_closed, normal}}, State) ->
%    {stop, normal, State};
%handle_info({Socket, {socket_closed, Error}}, State) ->
%    {stop, normal, State};
%handle_info({Socket, {fromsocket, Bytes}}, {Socket, normal, IncRequests}) ->
%    Pid = orber_iiop_inrequest:start(Bytes, normal, Socket),
%    ets:insert(IncRequests, {Pid, undefined}),
%    {noreply, {Socket, normal, IncRequests}};
handle_info({'EXIT', Pid, normal}, {Socket, Type, IncRequests}) ->
    ets:delete(IncRequests, Pid),
    {noreply, {Socket, Type, IncRequests}};
handle_info({'EXIT', Pid, Reason}, {Socket, Type, IncRequests}) ->
    ?PRINTDEBUG2("proxy ~p finished with reason ~p", [Pid, Reason]),
    ets:delete(IncRequests, Pid),
    {noreply, {Socket, Type, IncRequests}};
handle_info(X,State) ->
    {noreply, State}.


%%-----------------------------------------------------------------
%% Func: code_change/3
%%-----------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.



