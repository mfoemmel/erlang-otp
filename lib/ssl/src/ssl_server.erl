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

%%% Purpose : SSL server

%%
%% TODO
%%
%% XXX The ip option in listen is not general enough. It is assumed
%%     to be a tuple, which is not always the case.

-module(ssl_server).
-behaviour(gen_server).

%% External exports
-export([start_link/0]).
-export([accept/2, accept/3, connect/3, connect/4, close/1, listen/3, 
	 listen/4, proxy_join/2, peername/1, setnodelay/2, sockname/1]).

-export([start_link_prim/0]).
-export([accept_prim/4, connect_prim/5, close_prim/2, 
	 listen_prim/5, proxy_join_prim/3, peername_prim/2, setnodelay_prim/3, 
	 sockname_prim/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 code_change/3, terminate/2]).

-include("ssl_int.hrl").

-record(state, {
	  port = [],			% port() of port program
	  progpid = [],			% OS pid of port program
	  debug = false,		% debug printout flag
	  cons = [], 			% All brokers except pending accepts
	  paccepts = [], 		% Pending accept brokers
	  proxylsport = [], 		% proxy listen socket port
	  intref = 0			% internal reference counter
	 }).


%% In all functions below IP is a four tuple, e.g. {192, 236, 52, 7}. 
%% SPort, Fd and ListenFd are integers; Flags is a string of characters.

%%
%% start_link() -> {ok, Pid} | {error, Reason}
%%
start_link() ->
    gen_server:start_link({local, ssl_server}, ssl_server, [], []).

start_link_prim() ->
    gen_server:start_link({local, ssl_server_prim}, ssl_server, [], []).

%%
%% accept(ListenFd, Flags) -> {ok, Fd, ProxyLSPort} |
%%			      {error, Reason}
%%
accept(ListenFd, Flags) ->
    accept(ListenFd, Flags, infinity).
accept(ListenFd, Flags, Timeout) ->
    accept_prim(ssl_server,ListenFd, Flags, Timeout).
accept_prim(ServerName, ListenFd, Flags, Timeout) ->
    Req = {accept, self(), ListenFd, Flags}, 
    gen_server:call(ServerName, Req, Timeout).

%%
%% connect(IP, SPort, Flags) -> {ok, Fd, ProxyLSPort} |
%%					 {error, Reason}
%%
connect(IP, SPort, Flags) ->
    connect(IP, SPort, Flags, infinity).
connect(IP, SPort, Flags, Timeout) ->
    connect_prim(ssl_server, IP, SPort, Flags, Timeout).
connect_prim(ServerName, IP, SPort, Flags, Timeout) ->
    Req = {connect, self(), IP, SPort, Flags},
    gen_server:call(ServerName, Req, Timeout).
  
%%
%% listen(IP, SPort, Flags), 
%% listen(IP, SPort, Flags, BackLog) -> {ok, ListenFd, SPort0} | 
%%                                    {error, Reason}
%%
listen(IP, SPort, Flags) ->
    listen(IP, SPort, Flags, ?DEF_BACKLOG).
listen(IP, SPort, Flags, BackLog) ->
    listen_prim(ssl_server, IP, SPort, Flags, BackLog).
listen_prim(ServerName, IP, SPort, Flags, BackLog) ->
    Req = {listen, self(), IP, SPort, Flags, BackLog},
    gen_server:call(ServerName, Req, infinity).

%%
%% proxy_join(Fd, SPort) -> ok | {error, Reason}
%%
proxy_join(Fd, SPort) ->
    proxy_join_prim(ssl_server, Fd, SPort).
proxy_join_prim(ServerName, Fd, SPort) ->
    Req = {proxy_join, self(), Fd, SPort},
    gen_server:call(ServerName, Req, infinity).

%%
%% peername(Fd) -> {ok, {Address, Port}} | {error, Reason}
%%
peername(Fd) ->
    peername_prim(ssl_server, Fd).
peername_prim(ServerName, Fd) ->
    Req = {peername, self(), Fd},
    gen_server:call(ServerName, Req, infinity).

%%
%%  set_nodelay(Fd, Boolean)
%%
setnodelay(Fd, Boolean) ->
    setnodelay_prim(ssl_server, Fd, Boolean).
setnodelay_prim(ServerName, Fd, Boolean) ->
    Req = {setnodelay, self(), Fd, Boolean},
    gen_server:call(ServerName, Req, infinity).
    
%%
%% sockname(Fd) -> {ok, {Address, Port}} | {error, Reason}
%%
sockname(Fd) ->
    sockname_prim(ssl_server, Fd).
sockname_prim(ServerName, Fd) ->
    Req = {sockname, self(), Fd},
    gen_server:call(ServerName, Req, infinity).

%%
%% close(Fd) -> ok
%%
close(Fd) -> 
    close_prim(ssl_server, Fd).
close_prim(ServerName, Fd) -> 
    gen_server:call(ServerName, {close, self(), Fd}, infinity),
    ok.

%%
%% init
%%
init([]) ->
    Debug = case application:get_env(ssl, edebug) of
		{ok, true} -> 
		    true;
		_ ->
		    case application:get_env(ssl, debug) of
			{ok, true} ->
			    true;
			_  ->
			    false
		    end
	    end,
    ProgDir = 
	case init:get_argument(ssl_portprogram_dir) of
	    {ok, [[D]]} ->
		D;
	    _ ->
		case os:getenv("ERL_SSL_PORTPROGRAM_DIR") of
		    false ->
			find_priv_bin();
		    [] ->
			find_priv_bin();
		    Dir ->
			Dir
		end
	end,
    {Program, Flags} = mk_cmd_line("ssl_esock"), % "ssl_esock" is default
    Cmd = filename:join(ProgDir, Program) ++ " " ++ Flags,
    debug1(Debug, " start, Cmd =  ~s~n", [Cmd]), 
    case (catch open_port({spawn, Cmd}, [binary, {packet, 4}])) of
	Port when port(Port) ->
	    process_flag(trap_exit, true), 
	    receive 
		{Port, {data, Bin}} ->
		    {ProxyLSPort, ProgPid} = decode_msg(Bin, [int16, int32]), 
		    debug1(Debug, "port program pid = ~w~n", 
			   [ProgPid]), 
		    {ok, #state{port = Port, proxylsport = ProxyLSPort,
				progpid = ProgPid, debug = Debug}};
		{'EXIT', Port, Reason} ->
		    {stop, Reason}
	    end;
	{'EXIT', Reason} ->
	    {stop, Reason}
    end.

%%
%% accept
%%
handle_call({accept, Broker, ListenFd, Flags}, From, State) ->
    debug(State, "accept: broker = ~w, listenfd = ~w~n", 
	  [Broker, ListenFd]),
    case get_by_fd(ListenFd, State#state.cons) of
	{ok, {ListenFd, _, _}} ->
	    send_cmd(State#state.port, ?ACCEPT, [int32(ListenFd), Flags, 0]),
	    PAccepts = add({ListenFd, Broker, From}, State#state.paccepts),
	    %%
	    %% We reply when we get ACCEPT_REP or ASYNC_ACCEPT_ERR
	    %% 
	    {noreply, State#state{paccepts = PAccepts}};
	_Other ->
	    {reply, {error, ebadf}, State}
    end;

%%
%% connect
%%
handle_call({connect, Broker, IP, SPort, Flags}, From, State) ->
    debug(State, "connect: broker = ~w, ip = ~w, "
	  "sport = ~w~n", [Broker, IP, SPort]),
    Port = State#state.port,
    IPStr = io_lib:format("~w.~w.~w.~w", tuple_to_list(IP)),
    IntRef = new_intref(State),
    send_cmd(Port, ?CONNECT, [int32(IntRef), int16(SPort), IPStr, 0, 
			      Flags, 0]),
    Cons = add({{intref, IntRef}, Broker, From}, State#state.cons),
    %% We reply when we have got CONNECT_SYNC_ERR, or CONNECT_WAIT 
    %% and CONNECT_REP, or CONNECT_ERR.
    {noreply, State#state{cons = Cons, intref = IntRef}};

%%
%% listen
%%
handle_call({listen, Broker, IP, SPort, Flags, BackLog}, From, State) ->
    debug(State, "listen: broker = ~w, IP = ~w, "
	  "sport = ~w~n", [Broker, IP, SPort]),
    Port = State#state.port,
    IPStr = io_lib:format("~w.~w.~w.~w", tuple_to_list(IP)),
    IntRef = new_intref(State),
    send_cmd(Port, ?LISTEN, [int32(IntRef), int16(SPort), IPStr, 0, 
			     int16(BackLog), Flags, 0]), 
    Cons = add({{intref, IntRef}, Broker, From}, State#state.cons),
    %% We reply when we have got LISTEN_REP.
    {noreply, State#state{cons = Cons, intref = IntRef}};

%%
%% proxy join
%%
handle_call({proxy_join, Broker, Fd, SPort}, From, State) ->
    debug(State, "proxy_join: broker = ~w, fd = ~w, "
	  "sport = ~w~n", [Broker, Fd, SPort]),
    case replace_from_by_fd(Fd, State#state.cons, From) of 
	{ok, _, Cons} ->
	    send_cmd(State#state.port, ?PROXY_JOIN, [int32(Fd), 
						     int16(SPort)]), 
	    %% We reply when we get PROXY_JOIN_REP, or PROXY_JOIN_ERR.
	    {noreply, State#state{cons = Cons}};
	_Other ->
	    {reply, {error, ebadf}, State}
    end;

%%
%% peername
%%
handle_call({peername, Broker, Fd}, From, State) ->
    debug(State, "peername: broker = ~w, fd = ~w~n",
	  [Broker, Fd]),
    case replace_from_by_fd(Fd, State#state.cons, From) of 
	{ok, _, Cons} ->
	    send_cmd(State#state.port, ?GETPEERNAME, [int32(Fd)]),
	    %% We reply when we get GETPEERNAME_REP or GETPEERNAME_ERR.
	    {noreply, State#state{cons = Cons}};
	_Other ->
	    {reply, {error, ebadf}, State}
    end;

%%
%% setnodelay
%%
handle_call({setnodelay, Broker, Fd, Boolean}, From, State) ->
    debug(State, "setnodelay: broker = ~w, fd = ~w, "
	  "boolean = ~w~n", [Broker, Fd, Boolean]),
    case replace_from_by_fd(Fd, State#state.cons, From) of 
	{ok, _, Cons} ->
	    Val = if Boolean == true -> 1; true -> 0 end,
	    send_cmd(State#state.port, ?SET_SOCK_OPT, 
		     [int32(Fd), ?SET_TCP_NODELAY, Val]),
	    %% We reply when we get IOCTL_OK or IOCTL_ERR.
	    {noreply, State#state{cons = Cons}};
	_Other ->
	    {reply, {error, ebadf}, State}
    end;

%%
%% sockname
%%
handle_call({sockname, Broker, Fd}, From, State) ->
    debug(State, "sockname: broker = ~w, fd = ~w~n",
	  [Broker, Fd]),
    case replace_from_by_fd(Fd, State#state.cons, From) of 
	{ok, _, Cons} ->
	    send_cmd(State#state.port, ?GETSOCKNAME, [int32(Fd)]),
	    %% We reply when we get GETSOCKNAME_REP or GETSOCKNAME_ERR.
	    {noreply, State#state{cons = Cons}};
	_Other ->
	    {reply, {error, ebadf}, State}
    end;

%%
%% close
%%
handle_call({close, Broker, Fd}, From, State) ->
    debug(State, "close: broker = ~w, fd = ~w~n",
	  [Broker, Fd]),
    #state{port = Port, cons = Cons0, paccepts = PAccepts0} = State,
    case delete_by_fd(Fd, Cons0) of
	%% Must match Broker pid; fd may be reused already.
	{ok, {Fd, Broker, _}, Cons} ->			
	    send_cmd(Port, ?CLOSE, int32(Fd)), 
	    %% If Fd is a listen socket fd, there might be pending
	    %% accepts for that fd.
	    case delete_all_by_fd(Fd, PAccepts0) of
		{ok, DelAccepts, RemAccepts} ->
		    %% Reply {error, closed} to all pending accepts
		    lists:foreach(fun({_, _, AccFrom}) ->
					  gen_server:reply(AccFrom, 
							   {error, closed}) 
				  end, DelAccepts),
		    {reply, ok, 
		     State#state{cons = Cons, paccepts = RemAccepts}};
		_ ->
		    {reply, ok, State#state{cons = Cons}}
	    end;
	_ ->
	    {reply, ok, State}
    end;

handle_call(Request, From, State) ->
    debug(State, "unexpected call: ~w~n", [Request]),
    Reply = {error, {badcall, Request}},
    {reply, Reply, State}.

%%
%% handle_cast(Msg, State)
%%


handle_cast(Msg, State) ->
    debug(State, "unexpected cast: ~w~n", [Msg]),
    {noreply, State}.

%%
%% handle_info(Info, State)
%%

%% Data from port
%%
handle_info({Port, {data, Bin}}, State) 
  when State#state.port == Port, binary(Bin) ->

    %% io:format("++++ ssl_server got from port: ~w~n", [binary_to_list(Bin)]),

    case hd(binary_to_list(Bin, 1, 1)) of	% op code

	%%
	%% accept
	%%
	?ACCEPT_ERR when size(Bin) >= 5 ->
	    {ListenFd, Reason} = decode_msg(Bin, [int32, atom]),
	    debug(State, "accept_err: listenfd = ~w, "
		  "reason = ~w~n", [ListenFd, Reason]),
	    case delete_last_by_fd(ListenFd, State#state.paccepts) of
		{ok, {_, _, From}, PAccepts} ->
		    gen_server:reply(From, {error, Reason}),
		    {noreply, State#state{paccepts = PAccepts}};
		_Other ->
		    %% Already closed
		    {noreply, State}
	    end;
	?ACCEPT_REP when size(Bin) >= 9 ->
	    {ListenFd, Fd} = decode_msg(Bin, [int32, int32]),
	    debug(State, "accept_rep: listenfd = ~w, "
		  "fd = ~w~n", [ListenFd, Fd]),
	    case delete_last_by_fd(ListenFd, State#state.paccepts) of
		{ok, {_, Broker, From}, PAccepts} ->
		    Reply = {ok, Fd, State#state.proxylsport},
		    gen_server:reply(From, Reply),
		    Cons = add({Fd, Broker, []}, State#state.cons),
		    {noreply, State#state{cons = Cons, paccepts = PAccepts}};
		_Other ->
		    %% Already closed
		    {noreply, State}
	    end;

	%%
	%% connect
	%%
	?CONNECT_SYNC_ERR when size(Bin) >= 5 ->
	    {IntRef, Reason} = decode_msg(Bin, [int32, atom]),
	    debug(State, "connect_sync_err: intref = ~w, "
		  "reason = ~w~n", [IntRef, Reason]),
	    case delete_by_intref(IntRef, State#state.cons) of
		{ok, {_, _, From}, Cons} ->
		    gen_server:reply(From, {error, Reason}),
		    {noreply, State#state{cons = Cons}};
		_Other ->
		    {noreply, State}
	    end;
	?CONNECT_WAIT when size(Bin) >= 9 ->  
	    {IntRef, Fd} = decode_msg(Bin, [int32, int32]),
	    debug(State, "connect_wait: intref = ~w, "
		  "fd = ~w~n", [IntRef, Fd]),
	    case replace_fd_by_intref(IntRef, State#state.cons, Fd) of
		{ok, _, Cons} ->
		    %% We reply when we get CONNECT_REP or CONNECT_ERR
		    {noreply, State#state{cons = Cons}};
		_Other ->
		    %% We have a new Fd which must be closed
		    send_cmd(State#state.port, ?CLOSE, int32(Fd)),
		    {noreply, State}
	    end;
	?CONNECT_REP when size(Bin) >= 5 ->  
	    %% after CONNECT_WAIT
	    Fd = decode_msg(Bin, [int32]),
	    debug(State, "connect_rep: fd = ~w~n", [Fd]),
	    case replace_from_by_fd(Fd, State#state.cons, []) of
		{ok, {_, _, From}, Cons} ->
		    gen_server:reply(From, 
				     {ok, Fd, State#state.proxylsport}),
		    {noreply, State#state{cons = Cons}};
		_Other ->
		    {noreply, State}
	    end;
	?CONNECT_ERR when size(Bin) >= 5 ->
	    {Fd, Reason} = decode_msg(Bin, [int32, atom]),
	    debug(State, "connect_err: fd = ~w, "
		  "reason = ~w~n", [Fd, Reason]),
	    case delete_by_fd(Fd, State#state.cons) of
		{ok, {_, _, From}, Cons} ->
		    %% Fd not yet published - hence close ourselves
		    send_cmd(State#state.port, ?CLOSE, int32(Fd)),
		    gen_server:reply(From, {error, Reason}),
		    {noreply, State#state{cons = Cons}};
		_Other ->
		    %% Already closed
		    {noreply, State}
	    end;


	%%
	%% listen
	%%
	?LISTEN_SYNC_ERR when size(Bin) >= 5 ->
	    {IntRef, Reason} = decode_msg(Bin, [int32, atom]),
	    debug(State, "listen_sync_err: intref = ~w, "
		  "reason = ~w~n", [IntRef, Reason]),
	    case delete_by_intref(IntRef, State#state.cons) of
		{ok, {_, _, From}, Cons} ->
		    gen_server:reply(From, {error, Reason}),
		    {noreply, State#state{cons = Cons}};
		_Other ->
		    {noreply, State}
	    end;
	?LISTEN_REP when size(Bin) >= 11 ->  
	    {IntRef, ListenFd, SPort} = decode_msg(Bin, [int32, int32, int16]),
	    debug(State, "listen_rep: intref = ~w, "
		  "listenfd = ~w, sport = ~w~n", [IntRef, ListenFd, SPort]),
	    case replace_fd_from_by_intref(IntRef, State#state.cons, 
					   ListenFd, []) of
		{ok, {_, _, From}, Cons} ->
		    gen_server:reply(From, {ok, ListenFd, SPort}),
		    {noreply, State#state{cons = Cons}};
		_Other ->
		    %% ListenFd has to be closed.
		    send_cmd(State#state.port, ?CLOSE, int32(ListenFd)),
		    {noreply, State}
	    end;

	%%
	%% proxy join
	%%
	?PROXY_JOIN_REP when size(Bin) >= 5 -> 
	    Fd = decode_msg(Bin, [int32]),
	    debug(State, "proxy_join_rep: fd = ~w~n",
		  [Fd]),
	    case replace_from_by_fd(Fd, State#state.cons, []) of
		{ok, {_, _, From}, Cons} ->
		    gen_server:reply(From, ok),
		    {noreply, State#state{cons = Cons}};
		_Other ->
		    %% Already closed
		    {noreply, State}
	    end;
	?PROXY_JOIN_ERR when size(Bin) >= 5 -> 
	    {Fd, Reason} = decode_msg(Bin, [int32, atom]),
	    debug(State, "proxy_join_rep: fd = ~w, "
		  "reason = ~w~n", [Fd, Reason]),
	    case delete_by_fd(Fd, State#state.cons) of
		{ok, {_, _, From}, Cons} ->
		    %% Must not close Fd since it is published
		    gen_server:reply(From, {error, Reason}),
		    {noreply, State#state{cons = Cons}};
		_Other ->
		    %% Already closed
		    {noreply, State}
	    end;

	%%
	%% peername
	%%
	?GETPEERNAME_REP when size(Bin) >= 5 ->
	    {Fd, SPort, IPString} = decode_msg(Bin, [int32, int16, string]),
	    debug(State, "getpeername_rep: fd = ~w, "
		  "sport = ~w, ip = ~p~n", [Fd, SPort, IPString]),
	    case replace_from_by_fd(Fd, State#state.cons, []) of
		{ok, {_, _, From}, Cons} ->
		    gen_server:reply(From, {ok, {IPString, SPort}}),
		    {noreply, State#state{cons = Cons}};
		_Other ->
		    %% Already closed
		    {noreply, State}
	    end;
	?GETPEERNAME_ERR when size(Bin) >= 5 ->
	    {Fd, Reason} = decode_msg(Bin, [int32, atom]),
	    debug(State, "getpeername_err: fd = ~w, "
		  "reason = ~w~n", [Fd, Reason]),
	    case replace_from_by_fd(Fd, State#state.cons, []) of
		{ok, {_, _, From}, Cons} ->
		    gen_server:reply(From, {error, Reason}),
		    {noreply, State#state{cons = Cons}};
		_Other ->
		    %% Already closed
		    {noreply, State}
	    end;

	%%
	%% ioctl
	%%
	?IOCTL_OK when size(Bin) >= 5 ->
	    Fd = decode_msg(Bin, [int32]),
	    debug(State, "ioctl_ok: fd = ~w~n",
		  [Fd]),
	    case replace_from_by_fd(Fd, State#state.cons, []) of
		{ok, {_, _, From}, Cons} ->
		    gen_server:reply(From, ok),
		    {noreply, State#state{cons = Cons}};
		_Other ->
		    %% Already closed
		    {noreply, State}
	    end;
	?IOCTL_ERR when size(Bin) >= 5 ->
	    {Fd, Reason} = decode_msg(Bin, [int32, atom]),
	    debug(State, "ioctl_err: fd = ~w, "
		  "reason = ~w~n", [Fd, Reason]),
	    case replace_from_by_fd(Fd, State#state.cons, []) of
		{ok, {_, _, From}, Cons} ->
		    gen_server:reply(From, {error, Reason}),
		    {noreply, State#state{cons = Cons}};
		_Other ->
		    %% Already closed
		    {noreply, State}
	    end;

	%%
	%% sockname
	%%
	?GETSOCKNAME_REP when size(Bin) >= 5 ->
	    {Fd, SPort, IPString} = decode_msg(Bin, [int32, int16, string]),
	    debug(State, "getsockname_rep: fd = ~w, "
		  "sport = ~w, ip = ~p~n", [Fd, SPort, IPString]),
	    case replace_from_by_fd(Fd, State#state.cons, []) of
		{ok, {_, _, From}, Cons} ->
		    gen_server:reply(From, {ok, {IPString, SPort}}),
		    {noreply, State#state{cons = Cons}};
		_Other ->
		    %% Already closed
		    {noreply, State}
	    end;
	?GETSOCKNAME_ERR when size(Bin) >= 5 ->
	    {Fd, Reason} = decode_msg(Bin, [int32, atom]),
	    debug(State, "getsockname_err: fd = ~w, "
		  "reason = ~w~n", [Fd, Reason]),
	    case replace_from_by_fd(Fd, State#state.cons, []) of
		{ok, {_, _, From}, Cons} ->
		    gen_server:reply(From, {error, Reason}),
		    {noreply, State#state{cons = Cons}};
		_Other ->
		    %% Already closed
		    {noreply, State}
	    end;

	%%
	%% fromnet close
	%%
	%% This is the only case where we send an asynchronous message
	%% directly to the Broker.
	%%
	?FROMNET_CLOSE when size(Bin) >= 5 ->
	    Fd = decode_msg(Bin, [int32]),
	    debug(State, "fromnet_close: fd = ~w~n",
		  [Fd]),
	    %%
	    case delete_by_fd(Fd, State#state.cons) of
		{ok, {_, Broker, _}, Cons} ->
		    send_cmd(State#state.port, ?CLOSE, int32(Fd)), 
		    Broker ! {close, self()},
		    {noreply, State#state{cons = Cons}};
		_Other ->
		    %% Already closed
		    {noreply, State}
	    end
    end;

%%
%% EXIT
%%
handle_info({'EXIT', Pid, Reason}, State) when pid(Pid) ->
    debug(State, "exit pid = ~w, "
	  "reason = ~w~n", [Pid, Reason]),
    case delete_by_pid(Pid, State#state.cons) of
	{ok, {{intref, _}, Pid, _}, Cons} ->
	    {noreply, State#state{cons = Cons}};
	{ok, {Fd, Pid, _}, Cons} ->
	    send_cmd(State#state.port, ?CLOSE, int32(Fd)), 
	    %% If Fd is a listen socket fd, there might be pending
	    %% accepts for that fd.
	    case delete_all_by_fd(Fd, State#state.paccepts) of
		{ok, DelAccepts, RemAccepts} ->
		    %% Reply {error, closed} to all pending accepts.
		    lists:foreach(fun({_, _, From}) ->
					  gen_server:reply(From, 
							   {error, closed}) 
				  end, DelAccepts),
		    {noreply, 
		     State#state{cons = Cons, paccepts = RemAccepts}};
		_ ->
		    {noreply, State#state{cons = Cons}}
	    end;
	_ ->
	    case delete_by_pid(Pid, State#state.paccepts) of
		{ok, {ListenFd, _, _}, PAccepts} ->
		    %% decrement ref count in port program
		    send_cmd(State#state.port, ?NOACCEPT, int32(ListenFd)),
		    {noreply, State#state{paccepts = PAccepts}};
		_ ->
		    {noreply, State}
	    end
    end;

%%
%% 'badsig' means bad message to port. Port program is unaffected.
%%
handle_info({'EXIT', Port, badsig}, State) when State#state.port == Port ->
    debug(State, "badsig!!!~n", []),
    {noreply, State};

handle_info({'EXIT', Port, Reason}, State) when State#state.port == Port ->
    {stop, Reason, State};

handle_info(Info, State) ->
    debug(State, "unexpected info: ~w~n", [Info]),
    {noreply, State}.

%%
%% terminate(Reason, State) -> any
%%
terminate(Reason, State) ->
    ok.

%% 
%% code_change(OldVsn, State, Extra) -> {ok, NewState}
%%
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%
%% Send binary command to sock
%%
send_cmd(Port, Cmd, Args) ->
    Port ! {self(), {command, [Cmd| Args]}}.

%%
%% add(Descr, Cons) -> NCons
%%
add(D, L) -> 
    [D| L].

%%
%% get_by_fd(Fd, Cons) -> {ok, Descr} | not_found
%%
get_by_fd(Fd, Cons) ->
    get_by_pos(Fd, 1, Cons).

%%
%% delete_by_fd(Fd, Cons) -> {ok, OldDesc, NewCons} | not_found.
%%
delete_by_fd(Fd, Cons) ->
    delete_by_pos(Fd, 1, Cons).

%%
%% delete_all_by_fd(Fd, Cons) -> {ok, DelCons, RemCons} | not_found.
%%
delete_all_by_fd(Fd, Cons) ->
    delete_all_by_pos(Fd, 1, Cons).

%%
%% delete_by_intref(IntRef, Cons) -> {ok, OldDesc, NewCons} | not_found.
%%
delete_by_intref(IntRef, Cons) ->
    delete_by_pos({intref, IntRef}, 1, Cons).

%%
%% delete_by_pid(Pid, Cons) -> {ok, OldDesc, NewCons} | not_found.
%%
delete_by_pid(Pid, Cons) ->
    delete_by_pos(Pid, 2, Cons).

%%
%% delete_last_by_fd(Fd, Cons) -> {ok, OldDesc, NCons} | not_found
%%
delete_last_by_fd(Fd, Cons) ->
    case dlbf(Fd, Cons) of 
	{X, L} ->
	    {ok, X, L};
	_Other  ->
	    not_found
    end.

dlbf(Fd, [H]) -> 
    last_elem(Fd, H, []);
dlbf(Fd, [H|T]) ->
    case dlbf(Fd, T) of
	{X, L} -> 
	    {X, [H|L]};
	L -> 
	    last_elem(Fd, H, L)
    end;
dlbf(Fd, []) ->  
    [].

last_elem(Fd, H, L) when element(1, H) == Fd ->
    {H, L};
last_elem(_, H, L) ->
    [H|L].


%%
%% replace_from_by_fd(Fd, Cons, From) -> {ok, OldDesc, NewList} | not_found
%%
replace_from_by_fd(Fd, Cons, From) ->
    replace_posn_by_pos(Fd, 1, Cons, [{From, 3}]).

%%
%% replace_fd_by_intref(IntRef, Cons, Fd) -> {ok, OldDesc, NewList} | not_f.
%%
replace_fd_by_intref(IntRef, Cons, Fd) ->
    replace_posn_by_pos({intref, IntRef}, 1, Cons, [{Fd, 1}]).

%%
%% replace_fd_from_by_intref(IntRef, Cons, NFd, From) -> 
%%					{ok, OldDesc, NewList} |  not_found
%%
replace_fd_from_by_intref(IntRef, Cons, NFd, From) ->
    replace_posn_by_pos({intref, IntRef}, 1, Cons, [{NFd, 1}, {From, 3}]).


%%
%% All *_by_pos functions
%%

get_by_pos(Key, Pos, [H|_]) when element(Pos, H) == Key -> 
    {ok, H};
get_by_pos(Key, Pos, [_|T]) -> 
    get_by_pos(Key, Pos, T);
get_by_pos(_, _, []) -> 
    not_found.

delete_by_pos(Key, Pos, Cons) ->
    case delete_by_pos1(Key, Pos, {not_found, Cons}) of
	{not_found, _} ->
	    not_found;
	{ODesc, NCons} ->
	    {ok, ODesc, NCons}
    end.
delete_by_pos1(Key, Pos, {R, [H|T]}) when element(Pos, H) == Key ->
    {H, T};
delete_by_pos1(Key, Pos, {R, [H|T]}) ->
    {R0, T0} = delete_by_pos1(Key, Pos, {R, T}),
    {R0, [H| T0]};
delete_by_pos1(_, _, {R, []}) ->
    {R, []}.

delete_all_by_pos(Key, Pos, Cons) ->
    case lists:foldl(fun(H, {Ds, Rs}) when element(Pos, H) == Key ->
			     {[H|Ds], Rs};
			(H, {Ds, Rs}) ->
			     {Ds, [H|Rs]} 
		     end, {[], []}, Cons) of
	{[], _} ->
	    not_found;
	{DelCons, RemCons} ->
	    {ok, DelCons, RemCons}
    end.

replace_posn_by_pos(Key, Pos, Cons, Repls) ->
    case replace_posn_by_pos1(Key, Pos, {not_found, Cons}, Repls) of
	{not_found, _} ->
	    not_found;
	{ODesc, NCons} ->
	    {ok, ODesc, NCons}
    end.

replace_posn_by_pos1(Key, Pos, {R, [H| T]}, Repls) 
  when element(Pos, H) == Key ->
    NewH = lists:foldl(fun({Val, VPos}, Tuple) -> 
			     setelement(VPos, Tuple, Val) 
		     end,
		     H, Repls), 
    {H, [NewH| T]};
replace_posn_by_pos1(Key, Pos, {R, [H|T]}, Repls) ->
    {R0, T0} = replace_posn_by_pos1(Key, Pos, {R, T}, Repls),
    {R0, [H| T0]};
replace_posn_by_pos1(_, _, {R, []}, _) ->
    {R, []}.

%%
%% List/integer conversions
%%
get_int16(X1, X2) ->
    (X1 bsl 8) bor X2.

get_int32(X1,X2,X3,X4) ->
    (X1 bsl 24) bor (X2 bsl 16) bor (X3 bsl 8) bor X4.

int16(I) ->
    [(I bsr 8) band 255, I band 255].

int32(I) -> 
    [(I bsr 24) band 255,
     (I bsr 16) band 255,
     (I bsr  8) band 255,
     I band 255].

%% decode_msg(Bin, Format) -> Tuple | integer() | atom() | string()
%%
%% Decode message from binary
%% Format = [spec()]
%% spec() = int16 | int32 | string | atom 
%%
%% Note that the first byte (op code) of the binary message is removed.
%%
decode_msg(Bin, Format) ->
    [_|T] = binary_to_list(Bin),
    Dec = dec(T, Format),
    if 
	length(Dec) == 1 ->
	    hd(Dec);
	true  ->
	    list_to_tuple(Dec)
    end.

dec(_ , []) ->
    [];
dec([I1, I2| L], [int16| F]) ->
    [get_int16(I1, I2)| dec(L, F)];
dec([I1, I2, I3, I4| L], [int32| F]) ->
    [get_int32(I1, I2, I3, I4)| dec(L, F)];
dec(L, [string|F]) ->
    {Str, Rest} = dec_string(L),
    [Str| dec(Rest, F)];
dec(L, [atom|F]) ->
    {Str, Rest} = dec_string(L),
    [list_to_atom(Str)| dec(Rest, F)].

dec_string(L) ->
    {Str, Rest0} = lists:splitwith(fun (0) -> false; (_) -> true end, L),
    [_| Rest] = Rest0,
    {Str, Rest}.

%%
%% new_intref
%%
new_intref(State) ->
    (State#state.intref + 1) band 16#ffffffff.

%%
%% {Program, Flags} = mk_cmd_line(DefaultProgram)
%%
mk_cmd_line(Default) ->
    {port_program(Default), 
     lists:flatten([debug_flag(), " ", debugdir_flag(), " ", 
		   msgdebug_flag(), " ", proxylsport_flag(), " ", 
		   proxybacklog_flag()])}.

port_program(Default) ->
    case application:get_env(ssl, port_program) of
	{ok, Program} when list(Program) ->
	    Program;
	_Other ->
	    Default
    end.



%%
%% As this server may be started by the distribution, it is not safe to assume 
%% a working code server, neither a working file server.
%% I try to utilize the most primitive interfaces available to determine
%% the directory of the port_program.
%%
find_priv_bin() ->
    PrivDir = case (catch code:priv_dir(ssl)) of
		  {'EXIT', _} ->
		      %% Code server probably not startet yet
		      {ok, P} = erl_prim_loader:get_path(),
		      ModuleFile = atom_to_list(?MODULE) ++ extension(),
		      Pd = (catch lists:foldl
			    (fun(X,Acc) ->
				     M = filename:join([X, ModuleFile]),
				     %% The file server probably not started
				     %% either, has to use raw interface.
				     case file:raw_read_file_info(M) of 
					 {ok,_} -> 
					     %% Found our own module in the
					     %% path, lets bail out with
					     %% the priv_dir of this directory
					     Y = filename:split(X),
					     throw(filename:join
						   (lists:sublist
						    (Y,length(Y) - 1) 
						    ++ ["priv"])); 
					 _ -> 
					     Acc 
				     end 
			     end,
			     false,P)),
		      case Pd of
			  false ->
			      exit(ssl_priv_dir_indeterminate);
			  _ ->
			      Pd
		      end;
		  Dir ->
		      Dir
	      end,
    filename:join([PrivDir, "bin"]).

extension() ->
    %% erlang:info(machine) returns machine name as text in all uppercase
    "." ++ lists:map(fun(X) ->
			     X + $a - $A
		     end,
		     erlang:system_info(machine)).

debug_flag() ->
    get_env(debug, "-d").

msgdebug_flag() ->
    get_env(msgdebug, "-dm").

proxylsport_flag() ->
    case application:get_env(ssl, proxylsport) of
	{ok, PortNum} ->
	    "-pp " ++ integer_to_list(PortNum);
	_Other ->
	    ""
    end.

proxybacklog_flag() ->
    case application:get_env(ssl, proxylsbacklog) of
	{ok, Size} ->
	    "-pb " ++ integer_to_list(Size);
	_Other ->
	    ""
    end.

debugdir_flag() ->
    case application:get_env(ssl, debugdir) of
	{ok, Dir} when list(Dir) ->
	    "-dd " ++ Dir;
	_Other ->
	    ""
    end.
    
get_env(Key, Val) ->
    case application:get_env(ssl, Key) of
	{ok, true} ->
	    Val;
	_Other ->
	    ""
    end.

debug(State, Format, Args) ->
    debug1(State#state.debug, Format, Args).

debug1(true, Format0, Args) ->
    {MS, S, MiS} = erlang:now(),
    Secs = S rem 100, 
    MiSecs = MiS div 1000,
    Format = "++++ ~3..0w:~3..0w ssl_server (~w): " ++ Format0, 
    io:format(Format, [Secs, MiSecs, self()| Args]);
debug1(_, _, _) ->
    ok.

