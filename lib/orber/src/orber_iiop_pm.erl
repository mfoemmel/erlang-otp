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
%% File: orber_iiop_pm.erl
%% Description:
%%    This file contains the mapping of addresses on the format {Host, Port} 
%%    to a proxy pid.
%%
%% Creation date: 990615
%%
%%-----------------------------------------------------------------
-module(orber_iiop_pm).

-behaviour(gen_server).

-include_lib("orber/src/orber_iiop.hrl").
-include_lib("orber/include/corba.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/0, start/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([connect/6, disconnect/2, list_existing_connections/0, 
	 list_setup_connections/0, list_all_connections/0,
	 init/1, handle_call/3, handle_cast/2, handle_info/2,
	 code_change/3, terminate/2, stop/0, setup_connection/7]).

%%-----------------------------------------------------------------
%% Macros/Defines
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 7).

-define(PM_CONNECTION_DB, orber_iiop_pm_db).

-record(state, {connections, queue}).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
start() ->
    ignore.
start(Opts) ->
    gen_server:start_link({local, 'orber_iiop_pm'}, ?MODULE, Opts, []).


connect(Host, Port, SocketType, Timeout, Chars, Wchars) when SocketType == normal ->
    case ets:lookup(?PM_CONNECTION_DB, {Host, Port}) of
	[{_, connecting, I, _}] ->
	    gen_server:call(orber_iiop_pm, {connect, Host, Port, SocketType, 
					    [], Chars, Wchars}, Timeout);
	[] ->
	    gen_server:call(orber_iiop_pm, {connect, Host, Port, SocketType, 
					    [], Chars, Wchars}, Timeout);
	[{_, P, I, _}] ->
	    {P, [], I}
    end;
%connect(Host, #'SSLIOP_SSL'{port = Port} = Data, SocketType, Timeout, Chars, Wchars) 
connect(Host, Port, SocketType, Timeout, Chars, Wchars) 
  when SocketType == ssl ->
    case ets:lookup(?PM_CONNECTION_DB, {Host, Port}) of
	[{_, connecting, I, _}] ->
	    SocketOptions = get_ssl_socket_options(),
	    gen_server:call(orber_iiop_pm, {connect, Host, Port, SocketType, 
					    SocketOptions, Chars, Wchars}, Timeout);
	[] ->
	    SocketOptions = get_ssl_socket_options(),
	    gen_server:call(orber_iiop_pm, {connect, Host, Port, SocketType, 
					    SocketOptions, Chars, Wchars}, Timeout);
	[{_, P, I, _}] ->
	    {P, [], I}
    end.

get_ssl_socket_options() ->
    [{verify, orber:ssl_client_verify()},
     {depth, orber:ssl_client_depth()} |
     ssl_client_extra_options([{certfile, orber:ssl_client_certfile()},
			       {cacertfile, orber:ssl_client_cacertfile()},
			       {password, orber:ssl_client_password()},
			       {keyfile, orber:ssl_client_keyfile()},
			       {ciphers, orber:ssl_client_ciphers()},
			       {cachetimeout, orber:ssl_client_cachetimeout()}], [])].

ssl_client_extra_options([], Acc) ->
    Acc;
ssl_client_extra_options([{Type, []}|T], Acc) ->
    ssl_client_extra_options(T, Acc);
ssl_client_extra_options([{Type, infinity}|T], Acc) ->
    ssl_client_extra_options(T, Acc);
ssl_client_extra_options([{Type, Value}|T], Acc) ->
    ssl_client_extra_options(T, [{Type, Value}|Acc]).


disconnect(Host, Port) ->
    gen_server:call(orber_iiop_pm, {disconnect, Host, Port}).

list_existing_connections() ->
    filter_connections(ets:match(?PM_CONNECTION_DB, {'$2', '$1', '_', '_'}), []).

list_setup_connections() ->
    lists:append(ets:match(?PM_CONNECTION_DB, {'$1', connecting, '_', '_'})).

list_all_connections() ->
    lists:append(ets:match(?PM_CONNECTION_DB, {'$1', '_', '_', '_'})).

filter_connections([], Acc) ->
    Acc;
filter_connections([[Pid, Data]|T], Acc) when pid(Pid) ->
    filter_connections(T, [Data|Acc]);
filter_connections([_|T], Acc) ->
    filter_connections(T, Acc).


%%-----------------------------------------------------------------
%% Internal interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: stop/0 (Only used for test purpose !!!!!!)
%%-----------------------------------------------------------------
stop() ->
    gen_server:call(orber_iiop_pm, stop).

%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: init/1
%%-----------------------------------------------------------------
init(Opts) ->
    process_flag(trap_exit, true),
    {ok, #state{connections = ets:new(orber_iiop_pm_db, [set, protected, named_table]),
		queue = ets:new(orber_iiop_pm_queue, [bag])}}.

%%-----------------------------------------------------------------
%% Func: terminate/2
%%-----------------------------------------------------------------
terminate(Reason, #state{queue = Q}) ->
    %% Kill all proxies and close table before terminating
    stop_all_proxies(ets:first(?PM_CONNECTION_DB)),
    ets:delete(?PM_CONNECTION_DB),
    ets:delete(Q),
    ok.

stop_all_proxies('$end_of_table') ->
    ok;
stop_all_proxies(Key) ->
    case ets:lookup(?PM_CONNECTION_DB, Key) of
	[] ->
	    ok;
	[{_, connecting, I, _}] ->
	    catch invoke_connection_closed(I);
	[{_, P, I, _}] ->
	    catch invoke_connection_closed(I),
	    catch orber_iiop_outproxy:stop(P)
    end,
    stop_all_proxies(ets:next(?PM_CONNECTION_DB, Key)).

%%-----------------------------------------------------------------
%% Func: handle_call/3
%%-----------------------------------------------------------------
handle_call({connect, Host, Port, SocketType, SocketOptions, Chars, Wchars}, From, State) ->
    case ets:lookup(?PM_CONNECTION_DB, {Host, Port}) of
	[{_, connecting, I, S}] ->
	    %% Another client already requested a connection to the given host/port. 
	    %% Just add this client to the queue.
	    ets:insert(State#state.queue, {{Host, Port}, From}),
	    {noreply, State};
	[{_, P, I, _}] ->
	    %% This case will occur if the PortMapper completed a connection
	    %% between the client's ets:lookup and receiving this request.
	    {reply, {P, [], I}, State};
	[] ->
	    %% The first time a connection is requested to the given host/port.
	    case catch spawn_link(?MODULE, setup_connection, [self(), Host, Port, 
							      SocketType, 
							      SocketOptions, 
							      Chars, Wchars]) of
		Slave when pid(Slave) ->
		    ets:insert(?PM_CONNECTION_DB, {{Host, Port}, connecting, 
						   false, Slave}),
		    ets:insert(State#state.queue, {{Host, Port}, From}),
		    {noreply, State};
		What ->
		    orber:dbg("[~p] orber_iiop_pm:handle_call(connect ~p); 
Unable to invoke setup_connection due to: ", [?LINE, What], ?DEBUG_LEVEL),
		    {reply, 
		     {'EXCEPTION', #'INTERNAL'{completion_status=?COMPLETED_NO}}, 
		     State}
	    end
    end;
handle_call({disconnect, Host, Port}, From, State) ->
    case ets:lookup(?PM_CONNECTION_DB, {Host, Port}) of
	[] ->
	    ok;
	[{_, connecting, I, _}] ->
	    ets:delete(?PM_CONNECTION_DB, {Host, Port}),
	    Exc = {'EXCEPTION',#'INTERNAL'{completion_status = ?COMPLETED_NO}},
	    send_reply_to_queue(ets:lookup(State#state.queue, {Host, Port}), Exc),
	    ets:delete(State#state.queue, {Host, Port}),
	    catch invoke_connection_closed(I);
	[{_, P, I, _}] ->
	    unlink(P),
	    catch orber_iiop_outproxy:stop(P),
	    ets:delete(?PM_CONNECTION_DB, {Host, Port}),
	    catch invoke_connection_closed(I)
    end,
    {reply, ok, State};
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
%% Trapping exits 
handle_info({'EXIT', Pid, Reason}, State) ->
    %% Check the most common scenario first, i.e., a proxy terminates.
    case ets:match_object(?PM_CONNECTION_DB, {'_', Pid, '_', '_'}) of
	[{K, _, I, _}] ->
	    ets:delete(?PM_CONNECTION_DB, K),
	    invoke_connection_closed(I),
	    {noreply, State};
	[] when Reason == normal ->
	    %% This might have been a spawned 'setup_connection' which terminated
	    %% after sucessfully setting up a new connection.
	    {noreply, State};
	[] ->
	    %% Wasn't a proxy. Hence, we must test if it was a spawned
	    %% 'setup_connection' that failed.
	    case ets:match_object(?PM_CONNECTION_DB, {'_', '_', '_', Pid}) of
		[{K, connecting, I, _}] ->
		    ets:delete(?PM_CONNECTION_DB, K),
		    invoke_connection_closed(I),
		    Exc = {'EXCEPTION',#'INTERNAL'{completion_status = ?COMPLETED_NO}},
		    send_reply_to_queue(ets:lookup(State#state.queue, K), Exc),
		    ets:delete(State#state.queue, K),
		    orber:dbg("[~p] orber_iiop_pm:handle_info(setup_failed ~p); 
It was not possible to create a connection to the given host/port.", 
			    [?LINE, K], ?DEBUG_LEVEL),
		    {noreply, State};
		_ ->
		    {noreply, State}
	    end
    end;
handle_info({setup_failed, {Host, Port}, Exc}, State) ->
    %% Deletet the data from the connection DB first to avoid clients from
    %% trying to access it again.
    ets:delete(?PM_CONNECTION_DB, {Host, Port}),
    %% Now we can send whatever exception received.
    send_reply_to_queue(ets:lookup(State#state.queue, {Host, Port}), Exc),
    ets:delete(State#state.queue, {Host, Port}),
    orber:dbg("[~p] orber_iiop_pm:handle_info(setup_failed ~p ~p); 
It was not possible to create a connection to the given host/port.", 
			    [?LINE, Host, Port], ?DEBUG_LEVEL),
    {noreply, State};
handle_info({setup_successfull, {Host, Port}, {Child, Ctx, Int}}, State) ->
    %% Create a link to the proxy and store it in the connection DB.
    link(Child),
    ets:insert(?PM_CONNECTION_DB, {{Host, Port}, Child, Int, undefined}),
    %% Send the Proxy reference to all waiting clients.
    send_reply_to_queue(ets:lookup(State#state.queue, {Host, Port}),{Child, Ctx, Int}),
    %% Reset the queue.
    ets:delete(State#state.queue, {Host, Port}),
    {noreply, State};
handle_info(X, State) ->
    {noreply, State}.


send_reply_to_queue([], _) ->
    ok;
send_reply_to_queue([{_, Client}|T], Reply) ->
    gen_server:reply(Client, Reply),
    send_reply_to_queue(T, Reply). 

%%-----------------------------------------------------------------
%% Func: code_change/3
%%-----------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%-----------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------
setup_connection(PMPid, Host, Port, SocketType, SocketOptions, Chars, Wchars) ->
    case init_interceptors(Host, Port) of
	{'EXCEPTION', E} ->
	    PMPid ! {setup_failed, {Host, Port}, {'EXCEPTION', E}},
	    ok;
	Interceptors ->
	    case catch orber_iiop_outsup:connect(Host, Port, SocketType, SocketOptions) of
		{'error', {'EXCEPTION', E}} ->
		    orber:dbg("[~p] orber_iiop_pm:handle_call(connect ~p ~p); 
Raised Exc: ~p", [?LINE, Host, Port, E], ?DEBUG_LEVEL),
		    PMPid ! {setup_failed, {Host, Port}, {'EXCEPTION', E}},
		    ok;
		{'error', Reason} ->
		    orber:dbg("[~p] orber_iiop_pm:handle_call(connect ~p ~p); 
Got EXIT: ~p", [?LINE, Host, Port, Reason], ?DEBUG_LEVEL),
		    PMPid ! {setup_failed, {Host, Port}, 
			     {'EXCEPTION', #'INTERNAL'{completion_status=?COMPLETED_NO}}},
		    ok;
		{ok, undefined} ->
		    orber:dbg("[~p] orber_iiop_pm:handle_call(connect ~p ~p); 
Probably no listener on the given Node/Port or timedout.", 
					    [?LINE, Host, Port], ?DEBUG_LEVEL),
		    PMPid ! {setup_failed, {Host, Port}, 
			     {'EXCEPTION', #'COMM_FAILURE'{minor=(?ORBER_VMCID bor 1),
							   completion_status=?COMPLETED_NO}}},
		    ok;
		{ok, Child} ->
		    BiDirCtx = orber:bidir_context(),
		    Ctx = case orber:exclude_codeset_ctx() of
			      true ->
				  BiDirCtx;
			      _ ->
				  CodeSetCtx = 
				      #'CONV_FRAME_CodeSetContext'
				    {char_data =  Chars, 
				     wchar_data = Wchars},
				  [#'IOP_ServiceContext'
				   {context_id=?IOP_CodeSets, 
				    context_data = CodeSetCtx} | BiDirCtx]
			  end,
		    PMPid ! {setup_successfull, {Host, Port}, {Child, Ctx, Interceptors}},
		    ok
	    end
    end.



invoke_connection_closed(false) ->
    ok;
invoke_connection_closed({native, Ref, PIs}) ->
    orber_pi:closed_out_connection(PIs, Ref);
invoke_connection_closed({Type, PIs}) ->
    ok.


init_interceptors(Host, Port) ->
    case orber:get_interceptors() of
	{native, PIs} ->
	    case catch orber_pi:new_out_connection(PIs, Host, Port) of
		{'EXIT', R} ->
		    orber:dbg("[~p] orber_iiop_pm:init_interceptors(~p); Got Exit: ~p. One or more Interceptor incorrect or undefined?", 
					    [?LINE, PIs, R], ?DEBUG_LEVEL),
		    {'EXCEPTION', #'COMM_FAILURE'{minor=(?ORBER_VMCID bor 2), 
						  completion_status=?COMPLETED_NO}};
		IntRef ->
		    {native, IntRef, PIs}
	    end;
	Other ->
            %% Either 'false' or {Type, PIs}.
	    Other
    end.
    

%%-----------------------------------------------------------------
%% END OF MODULE
%%-----------------------------------------------------------------
