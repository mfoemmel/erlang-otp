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
-module(inet_tcp).

%% Socket server for TCP/IP

-export([connect/3, connect/4, listen/2, accept/1, accept/2, close/1]).
-export([send/2, recv/2, recv/3]).
-export([controlling_process/2]).

-export([getserv/1, getaddr/1]).

%% internal
-export([accept_mgr/5, listen_mgr/3, connect_mgr/6]).

%% system
-export([system_continue/3, system_terminate/4]).

%% imports
-import(lists, [reverse/1]).

-include("inet_int.hrl").

%% inet_tcp port lookup
getserv(Port) when integer(Port) -> {ok, Port};
getserv(Name) when atom(Name)    -> inet:getservbyname(Name,tcp).

%% inet_tcp address lookup
getaddr(Address) -> inet:getaddr(Address, inet).

%%
%% Accept
%%
accept(Socket) -> gen_tcp:accept(Socket).
accept(Socket,Timeout) -> gen_tcp:accept(Socket,Timeout).

%%
%% Send data on a socket
%%
send(Socket, Packet) -> gen_tcp:send(Socket, Packet).

%%
%% Receive data from a socket (inactive only)
%%
recv(Socket, Length) -> gen_tcp:recv(Socket, Length).
recv(Socket, Length, Timeout) -> gen_tcp:recv(Socket, Length, Timeout).

%%
%% Close a socket (async)
%%
close(Socket) -> gen_tcp:close(Socket).

%%
%% Set controlling process
%%
controlling_process(Socket, NewOwner) ->
    gen_tcp:controlling_process(Socket, NewOwner).

%% 
%% Listen
%%
listen(Port, Opts) when Port >= 0, Port =< 16#ffff ->
    case inet:socket_options([{port,Port} | Opts]) of
	{ok, St} ->
	    Tag = make_ref(),
	    proc_lib:spawn(?MODULE,listen_mgr,[self(),Tag,St]),
	    receive
		{Tag,Reply} -> Reply
	    end;
	{error,Reason} ->
	    exit(Reason)
    end.

%%
%% Connect
%%
connect(Address, Port, Opts) ->
    do_connect(Address, Port, Opts, infinity).

connect(Address, Port, Opts, infinity) ->
    do_connect(Address, Port, Opts, infinity);
connect(Address, Port, Opts, Timeout) when integer(Timeout), Timeout >= 0 ->
    do_connect(Address, Port, Opts, Timeout).

do_connect({A,B,C,D}, Port, Opts, Time) when integer(A+B+C+D), integer(Port) ->
    case inet:socket_options(Opts) of
	{ok, St} ->
	    Tag = make_ref(),
	    proc_lib:spawn(?MODULE,connect_mgr,
			   [self(),Tag,{A,B,C,D},Port,St,Time]),
	    receive
		{Tag,Reply} -> Reply
	    end;
	{error,Reason} ->
	    exit(Reason)
    end;
do_connect(_, _, _, _) ->
    exit(badarg).

%% reply & convert {exit,Reason} to {error, Reason}
reply(Pid, Tag, {exit,Reason}) when pid(Pid) ->
    Pid ! {Tag, {error,Reason}};
reply(Pid, Tag, Reply) when pid(Pid) ->
    Pid ! {Tag, Reply};
reply(_, _, _) ->     %% the case when Pid is noproc (for loopback send)
    true.

%%
%% Handle exit UNLINK owner before exit otherwise 
%% the owner will get {'EXIT',Pid,normal} in message queue
%%
reply_exit(Owner, From, Tag, Reply) ->
    reply(From, Tag, Reply),
    unlink(Owner),
    exit(normal).

%%
%% Do normal exit handling
%%
handle_exit(Owner) ->
    unlink(Owner),
    exit(normal).

%%
%%  The listen manager
%%
listen_mgr(Owner, Tag, St) ->
    process_flag(trap_exit, true),
    process_flag(priority, high),
    link(Owner),
    case inet:ll_open_set_bind(tcp_inet,?INET_AF_INET,St) of
	{ok, Tcp} ->
	    B = listen_backlog(St),
	    case ll_listen(Tcp, B, Owner) of
		ok ->
		    case inet:ll_index(Tcp) of
			{ok, LIX} ->
			    listen_init(Tcp, Owner, LIX, St, Tag);
			Error ->
			    reply_exit(Owner, Owner, Tag, Error)
		    end;
		Error -> 
		    reply_exit(Owner, Owner, Tag, Error)
	    end;
	Error -> reply_exit(Owner, Owner, Tag, Error)
    end.

%% extract backlog option from option list
listen_backlog(St) ->
    case lists:keysearch(backlog, 1, St#sock.other_opts) of
	{value, {_, B}} when integer(B), B >= 0 -> B;
	_ -> ?LISTEN_BACKLOG
    end.

listen_init(Tcp, Owner, LIX, St, Tag) ->
    reply(Owner,Tag, {ok, ?mksocket(Tcp) }),
    listen_loop(Tcp, Owner, LIX, St).

listen_owner(Tcp, Owner, LIX, St, NewOwner) ->
    receive
	{commit_owner, NewOwner} ->
	    unlink(Owner),
	    link(NewOwner),
	    Owner ! {owner, NewOwner},
	    listen_loop(Tcp, NewOwner, LIX, St);
	{abort_owner, NewOwner} ->
	    listen_loop(Tcp, Owner, LIX, St)
    end.

%%
%% Note: If the listen port dies when a port is accepting
%% the listner will terminate the accepting port as well
%% and in turn the accept_mgr
%%
listen_loop(Tcp, Owner, LIX, St) ->
    receive
	{call, From, Tag, {accept,Time}} ->
	    proc_lib:spawn(?MODULE, accept_mgr,
			   [From, Tag, LIX, St, Time]),
	    listen_loop(Tcp, Owner, LIX, St);
	
	{call, From, Tag, Request} ->
	    case handle_call(Request, Tcp, Owner, St) of
		{reply, Value, St1} ->
		    reply(From, Tag, Value),
		    listen_loop(Tcp, Owner, LIX, St1);

		{noreply, St1} ->
		    listen_loop(Tcp, Owner, LIX, St1);

		{owner,NewOwner,St1} ->
		    case From of
			Owner ->
			    reply(From, Tag, ok),
			    listen_owner(Tcp, Owner, LIX, St1, NewOwner);
			_ ->
			    %% This must be done by the current Owner
			    reply(From,Tag,{error, eperm}),
			    listen_loop(Tcp, Owner, LIX, St1)
		    end;
		{stop, St1} ->
		    handle_exit(Owner);

		{exit, Reason, St1} ->
		    reply_exit(Owner, From, Tag, {error,Reason})
	    end;

	{system, From, Msg} ->
	    sys:handle_system_msg(Msg, From, Owner, ?MODULE,
				  St#sock.debug, [listen,Tcp,LIX,St]);
	
	{'EXIT', Owner, _} ->
	    true;

	{'EXIT', Tcp, Reason} ->
	    if 
		Reason == normal ->  %% port closed,  terminate
		    Owner ! {tcp_closed, ?mksocket(Tcp)};
		true ->              %% port crashed
		    Owner ! {tcp_error, ?mksocket(Tcp),  Reason},
		    Owner ! {tcp_closed,  ?mksocket(Tcp)}
	    end,
	    handle_exit(Owner);

	Other ->
	    io:format("~w: WARNING: message discarded: ~p~n", [?MODULE,Other]),
	    listen_loop(Tcp, Owner, LIX, St)
    end.

%%
%% The accept_mgr MUST set trap_exit since if a listen socket 
%% is terminated while accepting the accepting port will die
%% with eof (normal) and accept will hang !
%%
accept_mgr(Owner, Tag, LIX, St, Time) ->
    process_flag(trap_exit, true),
    process_flag(priority, high),
    link(Owner),
    case open_accept_set(LIX, St, Time, Owner) of
	{ok, Tcp} ->
	    socket(Tcp, Owner, St, Tag);
	Error -> 
	    reply_exit(Owner, Owner, Tag, Error)
    end.

%%
%% The connect manager will link to the client/owner while connecting
%% in case the client dies the socket will clean up.
%%
connect_mgr(Owner, Tag, IP, TcpPort, St, Time) ->
    process_flag(trap_exit, true),
    process_flag(priority, high),
    link(Owner),
    case inet:ll_open_set_bind(tcp_inet,?INET_AF_INET,St) of
	{ok, Tcp} ->
	    case ll_connect(Tcp, IP, TcpPort, Time, Owner) of
		ok -> 
		    socket(Tcp, Owner, St, Tag);
		Error ->
		    reply_exit(Owner, Owner, Tag, Error)
	    end;
	Error -> 
	    reply_exit(Owner, Owner, Tag, Error)
    end.

%%
%% The connection is established
%%
socket(Tcp, Owner, St, Tag) ->
    reply(Owner, Tag, {ok, ?mksocket(Tcp)}),
    socket_loop(Tcp, Owner, St).

socket_owner(Tcp, Owner, St, NewOwner) ->
    receive
	{commit_owner, NewOwner} ->
	    unlink(Owner),
	    link(NewOwner),
	    Owner ! {owner, NewOwner},
	    socket_loop(Tcp, NewOwner, St);
	{abort_owner, NewOwner} ->
	    socket_loop(Tcp, Owner, St)
    end.

socket_loop(Tcp, Owner, St) ->
    receive
	{Tcp, {data, [?INET_REP_DATA | Data]}} ->
	    handle_input(St#sock.fs, Data, Tcp, Owner, St);

	{call, From, Tag, {send, Data}} ->
	    handle_output(St#sock.fs, From, Tag, Data, Tcp, Owner, St);

	{call, From, Tag, Request} ->
	    case handle_call(Request, Tcp, Owner, St) of
		{reply, Value, St1} ->
		    reply(From, Tag, Value),
		    socket_loop(Tcp, Owner, St1);

		{noreply, St1} ->
		    socket_loop(Tcp, Owner, St1);

		{owner,NewOwner,St1} ->
		    case From of
			Owner ->
			    reply(From, Tag, ok),
			    socket_owner(Tcp, Owner, St1, NewOwner);
			_ ->
			    %% This must be done by the current Owner
			    reply(From, Tag, {error, eperm}),
			    socket_loop(Tcp, Owner, St1)
		    end;

		{stop, St1} ->
		    handle_exit(Owner);

		{exit,Reason,St1} ->
		    reply(From,Tag, {error,Reason}),
		    handle_error(Reason,Tcp,Owner,St1)
	    end;

	{system, From, Msg} ->
	    sys:handle_system_msg(Msg, From, Owner, ?MODULE,
				  St#sock.debug, [socket,Tcp,St]);

	%% Async error mostly generated by failing queued write operation
	%% FIXME: PASSIVE MODE
	{Tcp, {data, [?INET_REP_ERROR | Reason]}} ->
	    if Reason == "enotconn" ->
		    Owner ! {tcp_closed, ?mksocket(Tcp)},
		    handle_exit(Owner);
	       true ->
		    Owner ! {tcp_error, ?mksocket(Tcp), list_to_atom(Reason)},
		    socket_loop(Tcp, Owner, St)
	    end;

	%% client(owner) died lets terminate normally
	{'EXIT', Owner, Reason} ->
	    true;

	%% Port has terminated handle the error
	{'EXIT', Tcp, Reason} ->
	    handle_error(Reason,Tcp,Owner,St);

	Event ->
	    handle_event(St#sock.fs, Event, Tcp, Owner, St)
    end.

%% Passive mode error loop Error is the reason for port termination
%% This code should be replied in all calls to the socket
error_loop(Error, Owner, St) ->
    receive
	{call, From, Tag, Req} ->
	    if Req == close ->
		    reply_exit(Owner, From,Tag,ok);
	       %% Handle setopt {active,true} ???
	       true ->
		    reply(From,Tag, {error,Error}),
		    error_loop(Error, Owner, St)
	    end;
	{'EXIT', Owner, _} ->
	    true;
	Event ->
	    %% discard
	    error_loop(Error, Owner, St)
    end.

%% The port has closed
%% determine if error should be logged and if we should enter
%% passive mode loop or not
%% NOTE. We must keep the old port ref since clients will match
%% on the socket record.
handle_error(Reason, Tcp, Owner, St) ->
    if St#sock.active == false ->
	    error_loop(Reason, Owner, St);
       true ->
	    S = ?mksocket(Tcp),
	    if Reason == normal ->
		    Owner ! {tcp_closed, S};
	       true ->
		    Owner ! {tcp_error, S, Reason},
		    Owner ! {tcp_closed, S}
	    end,
	    handle_exit(Owner)
    end.


%% Call
handle_call({recv,Len,Time}, Tcp, Owner, St) ->
    case ll_recv(Tcp,Len,Time,Owner) of
	{exit,Reason} ->
	    {exit,Reason,St};
	Reply ->
	    {reply, Reply, St}
    end;
handle_call(Req, Tcp, _Owner, St) ->
    inet:handle_call(Req, Tcp, St).

%% Continue and set debug flags
system_continue(Owner, NDebug, [socket,Tcp,St]) ->
    socket_loop(Tcp, Owner, St#sock { debug = NDebug });
system_continue(Owner, NDebug, [listen,Tcp,LIX,St]) ->
    listen_loop(Tcp, Owner, LIX, St#sock { debug = NDebug }).

%% Terminate
system_terminate(Reason, Owner, Debug, [socket,Tcp, St]) ->
    inet:ll_close(Tcp);
system_terminate(Reason, Owner, Debug, [listen,Tcp,LIX,St]) ->
    inet:ll_close(Tcp).


%%
%% Process input
%%
handle_input(Fs, Data, Port, Owner, St) ->
    handle_input(reverse(Fs), [], Data, Port, Owner, St).

handle_input([], Called, Data, Port, Owner, St) ->
    Owner ! {tcp, ?mksocket(Port), Data},
    socket_loop(Port, Owner, St#sock { fs = Called });
handle_input([{Fun,S} | Fs], Called, Data, Port, Owner, St) ->
    case Fun(input, Data, S) of
	{input, Data1, S1} ->
	    handle_input(Fs, [{Fun,S1}|Called], Data1, Port, Owner, St);
	{output, Data1, S1} ->
	    handle_output(Called, noproc, notag,
			  [{Fun,S1} | Fs], Data1, Port, Owner,St);
	{false, S1} ->
	    socket_loop(Port, Owner, 
			St#sock { fs = reverse([{Fun,S1} | Fs]) ++ Called })
    end.

%%
%% Process output
%%
handle_output(Fs, From, Tag, Data, Port, Owner, St) ->
    handle_output(Fs, From, Tag, [], Data, Port, Owner, St).

handle_output([], From, Tag, Called, Data, Port, Owner, St) ->
    Reply = ll_send(Port, Data, Owner),
    case Reply of
	{exit,normal} -> reply(From,Tag,{error,enotconn});
	_Rep -> reply(From,Tag,_Rep)
    end,
    case Reply of
	{exit,Reason} ->
	    handle_error(Reason,Port,Owner,St);
	_ ->
	    socket_loop(Port, Owner, St#sock { fs = reverse(Called) })
    end;
handle_output([{Fun,S} | Fs], From, Tag, Called, Data, Port, Owner, St) ->
    case Fun(output, Data, S) of
	{output, Data1, S1} ->
	    handle_output(Fs, From, Tag, [{Fun,S1} | Called], Data1,
			  Port, Owner,St);
	{input, Data1, S1} ->
	    handle_input(Called, [{Fun,S1}|Fs], Data1, Port, Owner, St);
	{false, S1} ->
	    socket_loop(Port, Owner, 
			St#sock { fs = reverse(Called) ++ [{Fun,S1} | Fs] })
    end.

%%
%% Process Event
%%
handle_event(Fs, Event, Port, Owner, St) ->
    handle_event(Fs, [], Event, Port, Owner, St).

handle_event([], Called, Event, Port, Owner, St) ->
    io:format("~w: WARNING: event discarded: ~p~n", [?MODULE, Event]),
    socket_loop(Port, Owner, St#sock { fs = reverse(Called) });
handle_event([{Fun,S} | Fs], Called, Event, Port, Owner, St) ->
    case Fun(event, Event, S) of
	{output, Data1, S1} ->
	    handle_output(Fs, noproc, notag, 
			  [{Fun,S1} | Called], Data1, Port, Owner, St);
	{input, Data1, S1} ->
	    handle_input(Called, [{Fun,S1}|Fs], Data1, Port, Owner, St);
	{event, Event1, S1} ->
	    handle_event(Fs, [{Fun,S1}|Called], Event1, Port, Owner, St);
	{false, S1} ->
	    socket_loop(Port, Owner,
			St#sock { fs = reverse(Called) ++ [{Fun,S1} | Fs] })
    end.

%% open/accept/setopts LIX = listen index
open_accept_set(LIX, St, Time, Owner) ->
    PortOpts = St#sock.open_opts,
    case catch open_port({spawn, tcp_inet}, PortOpts) of
	{'EXIT', Reason} ->
	    {error, Reason};
	Tcp when port(Tcp) -> 
	    case ll_accept(Tcp, LIX, Time, Owner) of
		ok ->
		    case inet:ll_setopts(Tcp, St#sock.sock_opts) of
			ok -> {ok, Tcp};
			Error -> Error
		    end;
		Error -> Error
	    end;
	Error -> Error
    end.

%%
%% infinity is encoded as max int32
%% this value must no be used for other timeout values!!!
%%
enc_time(infinity) -> [255,255,255,255];
enc_time(Time) -> ?int32(Time).

%% set listen mode on a tcp socket
ll_listen(Tcp, BackLog, Owner) when integer(BackLog) ->
    case sync_cmd(Tcp, Owner, [?TCP_REQ_LISTEN |?int16(BackLog)],
		  ?TCP_REP_LISTEN) of
	{ok, _} -> ok;
	Error  -> Error
    end.

ll_connect(Tcp, {A,B,C,D}, Port, Time, Owner) ->
    case sync_cmd(Tcp,
		  Owner,
		  [?INET_REQ_CONNECT, enc_time(Time),
		   ?int16(Port),
		   [A band 16#ff, B band 16#ff, C band 16#ff, D band 16#ff]
		  ],
		  ?INET_REP_CONNECT) of
	{ok, _} -> ok;
	Error -> Error
    end.

%% accept LIX is the index of the listen port (get with ll_index)
ll_accept(Tcp, LIX, Time, Owner) ->
    case sync_cmd(Tcp,
		  Owner,
		  [?TCP_REQ_ACCEPT,enc_time(Time),?int16(LIX)],
		  ?TCP_REP_ACCEPT) of
	{ok, _} -> ok;
	{exit,eintr} -> {exit,closed};
	Error -> Error
    end.

ll_send(Tcp, Data, Owner) ->
    case sync_cmd(Tcp, Owner, [?TCP_REQ_SEND, Data], ?TCP_REP_SEND) of
	{ok,_} -> ok;
	Error -> Error
    end.

ll_recv(Tcp, Length, Time, Owner) ->
    sync_cmd(Tcp,
	     Owner,
	     [?TCP_REQ_RECV, enc_time(Time), ?int32(Length)],
	     ?INET_REP_DATA).

%% terminate if Owner terminates !!
sync_cmd(Port, Owner, Cmd, Rep) ->
    Port ! {self(), {command, Cmd}},
    receive
	{Port, {data, [Rep | T]}} -> {ok, T};
	{Port, {data, [?INET_REP_ERROR | Err]}} -> {error, list_to_atom(Err)};
	{'EXIT', Port, badsig} -> {error, einval};
	{'EXIT', Port, Reason} -> {exit, Reason};
	{'EXIT', Owner, Reason} -> exit(normal)
    end.
