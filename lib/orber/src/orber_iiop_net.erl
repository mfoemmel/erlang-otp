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
%% File: orber_iiop_net.erl
%% Author: Lars Thorsen, Peter Lundell
%% 
%% Description:
%%    This file contains the IIOP communication server
%%
%% Creation date: 970115
%%
%%-----------------------------------------------------------------
-module(orber_iiop_net).

-behaviour(gen_server).

-include_lib("orber/src/orber_iiop.hrl").
-include_lib("orber/src/orber_debug.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/1, connect/2]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

%%-----------------------------------------------------------------
%% Server state record
%%-----------------------------------------------------------------
-record(state, {db=[],ports=[]}).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: start/1
%%-----------------------------------------------------------------
start(Opts) ->
    gen_server:start_link({local, orber_iiop_net}, orber_iiop_net, Opts, []).

connect(Type, S) ->
    gen_server:call(orber_iiop_net, {connect, Type, S}, infinity).

%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: init/1
%%-----------------------------------------------------------------
init(Options) ->
    ?PRINTDEBUG2("init netserver ~p", [Options]),
    process_flag(trap_exit, true),
    State = parse_options(Options, #state{}),
    {ok, State}.

%%-----------------------------------------------------------------
%% Func: terminate/1
%%-----------------------------------------------------------------
terminate(Reason, State) ->
    ?PRINTDEBUG2("iiop net server terminated with reason: ~p and state: ~p", [Reason, State]),
    ok.

%%-----------------------------------------------------------------
%% Internal Functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: parse_options/2
%%-----------------------------------------------------------------
parse_options([{port, Type, Port} | Rest], State) ->
    Options = 
	case Type of
	    ssl ->
		[{verify, orber:ssl_server_verify()},
		 {depth, orber:ssl_server_depth()} |
		 ssl_server_extra_options([{certfile, orber:ssl_server_certfile()},
					   {cacertfile, orber:ssl_server_cacertfile()},
					   {password, orber:ssl_server_password()},
					   {keyfile, orber:ssl_server_keyfile()},
					   {ciphers, orber:ssl_client_ciphers()},
					   {cachetimeout, orber:ssl_server_cachetimeout()}], [])];
	    
		  _ ->
		      []
	      end,
    Listen = ?IIOP_SOCKET_MOD:listen(Type, Port, Options),
    ?PRINTDEBUG2("listen at ~p port ~p ", [Type, Port]),
    {ok, Pid} = orber_iiop_socketsup:start_accept(Type, Listen),
    link(Pid),
    parse_options(Rest, State#state{ports=[{Pid, Listen, Port, Type} | State#state.ports]});
parse_options([], State) ->
    State.

ssl_server_extra_options([], Acc) ->
    Acc;
ssl_server_extra_options([{Type, []}|T], Acc) ->
    ssl_server_extra_options(T, Acc);
ssl_server_extra_options([{Type, infinity}|T], Acc) ->
    ssl_server_extra_options(T, Acc);
ssl_server_extra_options([{Type, Value}|T], Acc) ->
    ssl_server_extra_options(T, [{Type, Value}|Acc]).


%%-----------------------------------------------------------------
%% Func: handle_call/3
%%-----------------------------------------------------------------
handle_call({connect, Type, Socket}, From, State) ->
    Pid = orber_iiop_insup:start_connection(Type, Socket),
    {reply, Pid, State};
handle_call(_, _, State) ->
    {noreply, State}.

%%------------------------------------------------------------
%% Standard gen_server cast handle
%%------------------------------------------------------------
handle_cast(_, State) ->
    {noreply,  State}.

%%------------------------------------------------------------
%% Standard gen_server handles
%%------------------------------------------------------------
handle_info({'EXIT', Pid, Reason}, State) when pid(Pid) ->
    ?PRINTDEBUG2("Accept process died ~p", [{'EXIT', Pid, Reason, State}]),
    NewState = case lists:keysearch(Pid, 1, State#state.ports) of
		   {value, {Pid, Listen, Port, Type}} ->
		       unlink(Pid),
		       {ok, NewPid} = orber_iiop_socketsup:start_accept(Type, Listen),
		       link(NewPid),
		       NewList = lists:keyreplace(Pid, 1, State#state.ports,
						  {NewPid, Listen, Port, Type}),
		       State#state{ports=NewList};
		   false ->
		       State
	       end,
    {noreply, NewState};
handle_info(_, State) ->
    {noreply,  State}.

%%-----------------------------------------------------------------
%% Func: code_change/3
%%-----------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.
