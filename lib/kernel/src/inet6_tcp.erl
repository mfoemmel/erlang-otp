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
-module(inet6_tcp).

-export([connect/3, connect/4, listen/2, accept/1, accept/2, close/1]).
-export([send/2, recv/2, recv/3, unrecv/2]).
-export([controlling_process/2]).
-export([fdopen/2]).

-export([getserv/1, getaddr/1, getaddr/2, getaddrs/1, getaddrs/2]).

-include("inet_int.hrl").

%% inet_tcp port lookup
getserv(Port) when integer(Port) -> {ok, Port};
getserv(Name) when atom(Name)    -> inet:getservbyname(Name,tcp).

%% inet_tcp address lookup
getaddr(Address) -> inet:getaddr(Address, inet6).
getaddr(Address,Timer) -> inet:getaddr_tm(Address, inet6, Timer).

%% inet_tcp address lookup
getaddrs(Address) -> inet:getaddrs(Address, inet6).
getaddrs(Address,Timer) -> inet:getaddrs_tm(Address,inet6,Timer).

%%
%% Send data on a socket
%%
send(Socket, Packet) -> prim_inet:send(Socket, Packet).

%%
%% Receive data from a socket (inactive only)
%%
recv(Socket, Length) -> prim_inet:recv(Socket, Length).
recv(Socket, Length, Timeout) -> prim_inet:recv(Socket, Length, Timeout).

unrecv(Socket, Data) -> prim_inet:unrecv(Socket, Data).
%%
%% Close a socket (async)
%%
close(Socket) -> 
    inet:tcp_close(Socket).

%%
%% Set controlling process
%% FIXE ME: move messages to new owner!!!
%%
controlling_process(Socket, NewOwner) ->
    inet:tcp_controlling_process(Socket, NewOwner).

%%
%% Connect
%%
connect(Address, Port, Opts) ->
    do_connect(Address, Port, Opts, infinity).

connect(Address, Port, Opts, infinity) ->
    do_connect(Address, Port, Opts, infinity);
connect(Address, Port, Opts, Timeout) when integer(Timeout), Timeout >= 0 ->
    do_connect(Address, Port, Opts, Timeout).

do_connect(Addr = {A,B,C,D,E,F,G,H}, Port, Opts, Time) when 
  ?ip6(A,B,C,D,E,F,G,H), integer(Port) ->
    case inet:connect_options(Opts, inet6) of
	{error, Reason} -> exit(Reason);
	{ok, R} ->
	    Fd       = R#connect_opts.fd,
	    BAddr    = R#connect_opts.ifaddr,
	    BPort    = R#connect_opts.port,
	    SockOpts = R#connect_opts.opts,
	    case inet:open(Fd,BAddr,BPort,SockOpts,stream,inet6,?MODULE) of
		{ok, S} ->
		    case prim_inet:connect(S, Addr, Port, Time) of
			ok    -> {ok,S};
			Error ->  prim_inet:close(S), Error
		    end;
		Error -> Error
	    end
    end.

%% 
%% Listen
%%
listen(Port, Opts) when Port >= 0, Port =< 16#ffff ->
    case inet:listen_options([{port,Port} | Opts], inet6) of
	{error, Reason} -> exit(Reason);
	{ok, R} ->
	    Fd       = R#listen_opts.fd,
	    BAddr    = R#listen_opts.ifaddr,
	    BPort    = R#listen_opts.port,
	    SockOpts = R#listen_opts.opts,
	    case inet:open(Fd,BAddr,BPort,SockOpts,stream,inet6,?MODULE) of
		{ok, S} ->
		    case prim_inet:listen(S, R#listen_opts.backlog) of
			ok -> {ok, S};
			Error -> prim_inet:close(S), Error
		    end;
		Error -> Error
	    end
    end.

%%
%% Accept
%%
accept(L) -> 
    case prim_inet:accept(L) of
	{ok, S} ->
	    inet_db:register_socket(S, ?MODULE),
	    {ok,S};
	Error -> Error
    end.
	    
accept(L,Timeout) -> 
    case prim_inet:accept(L,Timeout) of
	{ok, S} ->
	    inet_db:register_socket(S, ?MODULE),
	    {ok,S};
	Error -> Error
    end.
    
%%
%% Create a port/socket from a file descriptor 
%%
fdopen(Fd, Opts) ->
    inet:fdopen(Fd, Opts, stream, inet6, ?MODULE).

