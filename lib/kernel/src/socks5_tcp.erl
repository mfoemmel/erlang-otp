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
-module(socks5_tcp).

%% Socks5 wrapper for tcp

-include("inet_int.hrl").

-export([connect/3, connect/4, listen/2, accept/1, accept/2, close/1]).
-export([send/2, recv/2, recv/3]).
-export([controlling_process/2]).

-export([getserv/1, getaddr/1]).
%% internal
-export([listen_mgr/3]).

%% system
-export([system_continue/3, system_terminate/4]).

-import(lists, [reverse/1]).

%% NOTE. the sock_opts are reversed to fit ll_setopt and must be 
%% reversed again before call to setopts !!
%%

%% "Standard" inet  functions

send(Socket, Packet) -> 
    gen_tcp:send(Socket, Packet).

recv(Socket, Length) -> 
    gen_tcp:recv(Socket, Length).

recv(Socket, Length, Time) -> 
    gen_tcp:recv(Socket, Length, Time).

close(Socket) ->
    gen_tcp:close(Socket).

controlling_process(Socket, Owner) -> 
    gen_tcp:controlling_process(Socket, Owner).

accept(S) -> gen_tcp:accept(S).
accept(S, Timeout) -> gen_tcp:accept(S, Timeout).

%% inet_tcp port lookup
getserv(Port) when integer(Port) -> {ok, Port};
getserv(Name) when atom(Name)    -> inet:getservbyname(Name,tcp).

%% inet_tcp address lookup
getaddr(Address) ->
    case inet:getaddr(Address, inet) of
	{ok, IP} -> {ok, IP};
	{error, nxdomain} -> {ok, Address};
	Error -> Error
    end.

%%
%% Listen for a remote connection (May be used once only)
%%
listen(Port, Opts) when Port >= 0, Port =< 16#ffff ->
    case inet:socket_options([{port,Port} | Opts]) of
	{ok, St} ->
	    case socks5:is_direct(St#sock.local_ip) of
		true -> inet_tcp:listen(Port, Opts);
		false ->
		    Tag = make_ref(),
		    proc_lib:spawn(?MODULE,listen_mgr,[self(),Tag,St]),
		    receive
			{Tag,Reply} -> Reply
		    end
	    end;
	{error,Reason} -> 
	    exit(Reason)
    end.

listen_mgr(Owner, Tag, St) ->
    case socks5:open(St#sock.open_opts) of
	{ok, S} ->
	    case socks5:bind(S,
			     St#sock.local_ip,
			     St#sock.local_port) of
		{ok, {BndAddress, BndPort}} ->
		    inet:setopts(S, reverse(St#sock.sock_opts) ++ 
				 [{active,false}]),
		    inet:setname(S, {BndAddress, BndPort}),
		    listen_init(S, Owner, St, Tag);
		Error -> 
		    close(S),
		    reply(Owner, Tag, Error)
	    end;
	Error ->
	    reply(Owner, Tag, Error)
    end.

listen_init(Socket, Owner, St, Tag) ->
    process_flag(trap_exit, true),
    link(Owner),
    reply(Owner, Tag, {ok, ?mksocket(noport)}),
    listen_loop(Socket, Owner, St).


listen_loop(Socket, Owner, St) ->
    receive
	{call, From, Tag, {accept,Time}} ->
	    if Socket == undefined ->
		    reply(From, Tag, {error, enotsock}),
		    listen_loop(Socket, Owner, St);
	       true ->
		    case socks5:accept(Socket,Time) of
			{ok, {IP, Port}} ->
			    inet:setpeername(Socket, {IP, Port}),
			    inet_tcp:controlling_process(Socket, From),
			    inet:setopts(Socket, 
					 reverse(St#sock.sock_opts)),
			    reply(From, Tag, {ok, Socket}),
			    listen_loop(undefined, Owner, St);
			Error ->
			    reply(From, Tag, Error),
			    listen_loop(Socket, Owner, St)
		    end
	    end;
	
	{call, From, Tag, close} ->
	    unlink(Owner),
	    if Socket == undefined -> 
		    true;
	       true -> 
		    inet_tcp:close(Socket)
	    end;
	
	{call, From, Tag, {set_owner, New}} ->
	    reply(From, Tag, ok),
	    receive
		{commit_owner, NewOwner} ->
		    unlink(Owner),
		    link(NewOwner),
		    Owner ! {owner, NewOwner},
		    listen_loop(Socket, NewOwner, St);
		{abort_owner, NewOwner} ->
		    listen_loop(Socket, Owner, St)
	    end;

	{call, From, Tag, Request} ->
	    if Socket == undefined ->
		    reply(From, Tag, {error, enotsock});
	       true ->
		    reply(From, Tag, call(Socket, Request))
	    end,
	    listen_loop(Socket, Owner, St);

	{system, From, Msg} ->
	    sys:handle_system_msg(Msg, From, Owner, ?MODULE,
				  St#sock.debug, [Socket,St]);

	{tcp_closed, Socket} -> %% proxy closed and error
	    unlink(Owner),
	    Owner ! {tcp_close, ?mksocket(noport)};

	{tcp_error, Socket, Reason} ->
	    Owner ! {tcp_error, ?mksocket(noport), Reason},
	    listen_loop(Socket, Owner, St);

	{'EXIT', Owner, Reason} ->
	    true;

	Other ->
	    io:format("~w: WARNING: message discarded: ~p~n", [?MODULE,Other]),
	    listen_loop(Socket, Owner, St)
    end.

%% reset connect options
c_opts() ->
    [{active, true}, {header,0}].

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
    case socks5:is_direct({A,B,C,D}) of
	true -> inet_tcp:connect({A,B,C,D}, Port, Opts, Time);
	false -> do_connect1({A,B,C,D}, Port, Opts,Time)
    end;
do_connect(Name, Port, Opts, Time) when list(Name), integer(Port) ->
    do_connect1(Name, Port, Opts, Time).

%%
%% We use the same timeout for connecting to the socks server &
%% for doing the connection
%%
do_connect1(Address, Port, Opts, Time) ->
    case inet:socket_options(Opts) of
	{ok, St} ->
	    case socks5:open(St#sock.open_opts, Time) of
		{ok, S} ->
		    case socks5:connect(S, Address, Port, Time) of
			{ok, {BndAddress, BndPort}} ->

			    inet:setopts(S, c_opts() ++ 
					 reverse(St#sock.sock_opts)),
			    inet:setname(S, {BndAddress, BndPort}),
			    inet:setpeername(S, {Address,Port}),
			    {ok, S};
			Error ->
			    close(S),
			    Error
		    end;
		Error -> Error
	    end;
	{error,Reason} ->
	    exit(Reason)
    end.

%% Continue and set debug flags
system_continue(Owner, NDebug, [Socket, St]) ->
    listen_loop(Socket, Owner, St#sock { debug = NDebug }).

system_terminate(Reason, Owner, Debug, [Socket, St]) ->
    inet:close(Socket).

%%
%% Call/Reply
%%
call(Socket, Request) ->
    Tag = make_ref(),
    Pid = Socket#socket.pid,
    Pid ! {call, self(), Tag, Request},
    receive
	{Tag, Reply} -> Reply;
	{'EXIT', Pid, Reason} ->
	    {error, closed};
	{tcp_closed, Socket} ->
	    {error, closed}
    end.

reply(Pid, Tag, Reply) ->
    Pid ! {Tag, Reply}.


