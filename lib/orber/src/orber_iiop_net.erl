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

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/1, connect/3, connections/0, 
	 sockname2peername/2, peername2sockname/2,
	 add_connection/4]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]).

%%-----------------------------------------------------------------
%% Server state record and definitions
%%-----------------------------------------------------------------
-define(CONNECTION_DB, orber_iiop_net_db).

-record(state, {ports=[], max_connections, db, counter = 1, queue}).

-record(connection, {pid, socket, type, peerdata, localdata}).

-record(listen, {pid, socket, port, type}).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: start/1
%%-----------------------------------------------------------------
start(Opts) ->
    gen_server:start_link({local, orber_iiop_net}, orber_iiop_net, Opts, []).

connect(Type, S, AcceptPid) ->
    gen_server:call(orber_iiop_net, {connect, Type, S, AcceptPid}, infinity).

connections() ->
    do_select([{#connection{peerdata = '$1', _='_'}, [], ['$1']}]).

sockname2peername(SockHost, SockPort) ->
    do_select([{#connection{peerdata = '$1',
			    localdata = {match_type(SockHost), 
					 match_type(SockPort)}, 
			    _='_'}, [], ['$1']}]).


peername2sockname(PeerHost, PeerPort) ->
    do_select([{#connection{peerdata = {match_type(PeerHost), 
					match_type(PeerPort)},
			    localdata = '$1', 
			    _='_'}, [], ['$1']}]).

do_select(Pattern) ->   
    case catch ets:select(?CONNECTION_DB, Pattern) of
	{'EXIT', _What} ->
	    [];
	Result ->
	    Result
    end.

match_type(0) -> 
    %% Wildcard port number
    '_';
match_type("") -> 
    %% Wildcard host
    '_';
match_type(Key) -> 
    %% Wildcard not used.
    Key.
    
add_connection(Socket, Type, PeerData, LocalData) ->
    ets:insert(?CONNECTION_DB, #connection{pid = self(), socket = Socket, 
					   type = Type, peerdata = PeerData,
					   localdata = LocalData}).

%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: init/1
%%-----------------------------------------------------------------
init(Options) ->
    process_flag(trap_exit, true),
    {ok, parse_options(Options, 
		       #state{max_connections = orber:iiop_max_in_connections(),
			      db = ets:new(?CONNECTION_DB, [set, public, 
							    named_table,
							    {keypos, 2}]), 
			      queue = queue:new()})}.

%%-----------------------------------------------------------------
%% Func: terminate/1
%%-----------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

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
    {ok, Listen, NewPort} = orber_socket:listen(Type, Port, Options),
    {ok, Pid} = orber_iiop_socketsup:start_accept(Type, Listen),
    link(Pid),
    ets:insert(?CONNECTION_DB, #listen{pid = Pid, socket = Listen, 
				       port = NewPort, type = Type}),
    parse_options(Rest, State);
parse_options([], State) ->
    State.

ssl_server_extra_options([], Acc) ->
    Acc;
ssl_server_extra_options([{_Type, []}|T], Acc) ->
    ssl_server_extra_options(T, Acc);
ssl_server_extra_options([{_Type, infinity}|T], Acc) ->
    ssl_server_extra_options(T, Acc);
ssl_server_extra_options([{Type, Value}|T], Acc) ->
    ssl_server_extra_options(T, [{Type, Value}|Acc]).


%%-----------------------------------------------------------------
%% Func: handle_call/3
%%-----------------------------------------------------------------
handle_call({connect, Type, Socket, _AcceptPid}, _From, State) 
  when State#state.max_connections == infinity;
       State#state.max_connections > State#state.counter ->
    case access_allowed(Type, Socket, Type) of
	true ->
	    {ok, Pid} = orber_iiop_insup:start_connection(Type, Socket),
	    link(Pid),
	    {reply, {ok, Pid, true}, update_counter(State, 1)};
	false ->
	    {H, P} = orber_socket:peerdata(Type, Socket),
	    orber_tb:info("Blocked connect attempt from ~s - ~p", [H, P]),
	    {reply, denied, State}
    end;
handle_call({connect, Type, Socket, AcceptPid}, _From, #state{queue = Q} = State) ->
    case access_allowed(Type, Socket, Type) of
	true ->
	    {ok, Pid} = orber_iiop_insup:start_connection(Type, Socket),
	    link(Pid),
	    Ref = erlang:make_ref(),
	    {reply, {ok, Pid, Ref}, 
	     update_counter(State#state{queue = 
					queue:in({AcceptPid, Ref}, Q)}, 1)};
	false ->
	    {H, P} = orber_socket:peerdata(Type, Socket),
	    orber_tb:info("Blocked connect attempt from ~s - ~p", [H, P]),
	    {reply, denied, State}
    end;
handle_call(_, _, State) ->
    {noreply, State}.

access_allowed(Type, Socket, Type) ->
    Flags = orber:get_flags(),
    case ?ORB_FLAG_TEST(Flags, ?ORB_ENV_USE_ACL_INCOMING) of
	false ->
	    true;
	true ->
	    SearchFor = 
		case Type of
		    normal ->
			tcp_in;
		    ssl ->
			ssl_in
		end,	    
	    {ok, {Host, Port}} = orber_socket:peername(Type, Socket),
	    case orber_acl:match(Host, SearchFor, true) of
		{true, _, 0} ->
		    true;
		{true, _, Port} ->
		    true;
		{true, _, {Min, Max}} when Port >= Min, Port =< Max ->
		    true;
		_ ->
		    false
	    end
    end.

%%------------------------------------------------------------
%% Standard gen_server cast handle
%%------------------------------------------------------------
handle_cast(_, State) ->
    {noreply,  State}.

%%------------------------------------------------------------
%% Standard gen_server handles
%%------------------------------------------------------------
handle_info({'EXIT', Pid, _Reason}, State) when pid(Pid) ->
    case ets:lookup(?CONNECTION_DB, Pid) of
	[#listen{pid = Pid, socket = Listen, port = Port, type = Type}] ->
	    ets:delete(?CONNECTION_DB, Pid),
	    unlink(Pid),
	    {ok, NewPid} = orber_iiop_socketsup:start_accept(Type, Listen),
	    link(NewPid),
	    ets:insert(?CONNECTION_DB, #listen{pid = NewPid, socket = Listen, 
					       port = Port, type = Type}),
	    %% Remove the connection if it's in the queue.
	    {noreply, 
	     State#state{queue = 
			 from_list(
			   lists:keydelete(Pid, 1, 
					   queue:to_list(State#state.queue)))}};
	[#connection{pid = Pid}] ->
	    ets:delete(?CONNECTION_DB, Pid),
	    unlink(Pid),
	    case queue:out(State#state.queue) of
		{empty, _} ->
		    {noreply, update_counter(State, -1)};
		{{value, {AcceptPid, Ref}}, Q} ->
		    AcceptPid ! {Ref, ok},
		    {noreply, update_counter(State#state{queue = Q}, -1)}
	    end;
	[] ->
	    {noreply, State}
    end;
handle_info(_, State) ->
    {noreply,  State}.

from_list(List) ->
    from_list(List, queue:new()).

from_list([], Q) ->
    Q;
from_list([H|T], Q) ->
    NewQ = queue:in(H, Q),
    from_list(T, NewQ).


%%-----------------------------------------------------------------
%% Func: code_change/3
%%-----------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------
%% Internal Functions
%%-----------------------------------------------------------------
update_counter(#state{max_connections = infinity} = State, _) ->
    State;
update_counter(State, Value) ->
    State#state{counter = State#state.counter + Value}.

