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
-export([send/2, recv/2, recv/3, unrecv/2]).
-export([controlling_process/2]).

-export([getserv/1, getaddr/1, getaddr/2, getaddrs/1, getaddrs/2]).

%% "Standard" inet  functions
%% FIXME: socks over inet6 and other ...
%%
send(Socket, Packet) -> 
    inet_tcp:send(Socket, Packet).

recv(Socket, Length) -> 
    inet_tcp:recv(Socket, Length).

recv(Socket, Length, Time) -> 
    inet_tcp:recv(Socket, Length, Time).

unrecv(Socket, Data) ->
    inet_tcp:unrecv(Socket, Data).

close(Socket) ->
    inet_tcp:close(Socket).

controlling_process(Socket, Owner) ->
    inet_tcp:controlling_process(Socket, Owner).


accept(S) ->
    accept(S, infinity).
accept(S, Timeout) -> 
    case socks5:accept(S,Timeout) of
	{ok, {IP, Port}} ->
	    inet:setpeername(S, {IP, Port}),
	    {ok, S};
	Error ->
	    Error
    end.


%% inet_tcp port lookup
getserv(Port) when integer(Port) -> {ok, Port};
getserv(Name) when atom(Name)    -> inet:getservbyname(Name,tcp).

%% inet_tcp address lookup
getaddr(Address) -> getaddr(Address, false).
getaddr(Address,Timer) ->
    case inet:getaddr_tm(Address, inet,Timer) of
	{ok, IP} -> {ok, IP};
	{error, nxdomain} -> {ok, Address};
	Error -> Error
    end.

getaddrs(Address) -> getaddrs(Address, false).
getaddrs(Address,Timer) -> 
    case inet:getaddrs_tm(Address, inet, Timer) of
	{ok, IP} -> {ok, IP};
	{error, nxdomain} -> {ok, Address};
	Error -> Error
    end.

%%
%% Listen for a remote connection (May be used once only)
%%
listen(Port, Opts) when Port >= 0, Port =< 16#ffff ->
    case inet:listen_options([{port,Port} | Opts], inet) of
	{error, Reason} -> exit(Reason);
	{ok, R} ->
	    case socks5:is_direct(R#listen_opts.ifaddr) of
		true -> inet_tcp:listen(Port, Opts);
		false ->
		    case socks5:open() of
			{ok, S} ->
			    case socks5:bind(S, R#listen_opts.ifaddr,
					     R#listen_opts.port) of
				{ok, {BndAddress, BndPort}} ->
				    %% Fixme: at accept we must
				    %% toggle active flag somehow
				    Opts1 = R#listen_opts.opts ++
					[{active,false}],
				    inet:setopts(S, Opts1),
				    inet:setsockname(S, {BndAddress, BndPort}),
				    inet_db:register_socket(S, ?MODULE),
				    {ok, S};
				Error -> 
				    close(S),
				    Error
			    end;
			Error -> Error
		    end
	    end;
	Error ->
	    Error
    end.
		    

%% reset connect options
c_opts() ->
    [{active,true}, {mode,list}, {header,0}].

%%
%% Connect
%%
connect(Address, Port, Opts) ->
    do_connect(Address, Port, Opts, infinity).

connect(Address, Port, Opts, infinity) ->
    do_connect(Address, Port, Opts, infinity);
connect(Address, Port, Opts, Timeout) when integer(Timeout), Timeout >= 0 ->
    do_connect(Address, Port, Opts, Timeout).

do_connect({A,B,C,D}, Port, Opts, Time) when ?ip(A,B,C,D), integer(Port) ->
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
    case inet:connect_options(Opts, inet) of
	{ok, R} ->
	    case socks5:open([], Time) of
		{ok, S} ->
		    case socks5:connect(S, Address, Port, Time) of
			{ok, {BndAddress, BndPort}} ->
			    inet:setopts(S, R#connect_opts.opts ++ c_opts()),
			    inet:setsockname(S, {BndAddress, BndPort}),
			    inet:setpeername(S, {Address,Port}),
			    inet_db:register_socket(S, ?MODULE),
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

