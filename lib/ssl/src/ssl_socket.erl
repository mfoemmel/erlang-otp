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
-module(ssl_socket).

%% Purpose : Old interface to SSL (version 1.0)

-export([accept/2, client/5, close/1, controlling_process/2, listen/4,
	 peername/1, resolve/0, resolve/1, sockname/1, start/0,
	 stop/0, version/0]).

-include_lib("kernel/include/inet.hrl").
-include("ssl_int.hrl").

%%
%% start()
%%
start() ->
    application:start(ssl).

%%
%% stop()
%%
stop() ->
    application:stop(ssl).

%%
%% accept(ListenSocket, SSLFlags) -> Socket
%%
%% 	ListenSocket = sslsocket()
%% 	SSLFlags = string()
%% 	Socket = pid()
%%
accept(ListenSocket, SSLFlags) when record(ListenSocket, sslsocket) ->
    {ok, Pid} = ssl_broker:start_broker(acceptor, false), % false = old i/f
    Options = [{flags, SSLFlags}],
    case ssl_broker:accept(Pid, ListenSocket, Options, infinity) of
	{ok, Socket} ->
	    Socket#sslsocket.pid;
	{error, Reason} ->
	    {error, Reason}
    end.

%%
%% client(Protocol, Family, Address, Mode, SSLFlags) -> Socket
%%
client('STREAM', 'AF_INET', {IPAddress, Port}, Mode, SSLFlags) ->
    {ok, Pid} = ssl_broker:start_broker(connector, false),
    Options = [Mode, {flags, SSLFlags}],
    case ssl_broker:connect(Pid, IPAddress, Port, Options, infinity) of
	{ok, Socket} ->
	    Socket#sslsocket.pid;
	{error, Reason} ->
	    {error, Reason}
    end.

%%
%% close(Socket) -> void()
%%
close(Socket) when record(Socket, sslsocket) ->
    close(Socket#sslsocket.pid);
close(Socket) when pid(Socket) ->
    ssl_broker:close(Socket).

%%
%% controlling_process(Socket, Pid) -> ok
%%
controlling_process(Socket, Pid) ->
    ssl_broker:controlling_process(Socket, Pid).

%%
%% listen(Protocol, Family, Address, Mode) -> ListenSock
%%
listen('STREAM', 'AF_INET', Port, Mode) ->
    {ok, Pid} = ssl_broker:start_broker(listener, false),
    Options = [Mode],
    case ssl_broker:listen(Pid, Port, Options) of
	{ok, ListenSock} ->
	    ListenSock;
	{error, Reason} ->
	    {error, Reason}
    end.

%%
%% peername(Socket)
%%
peername(Socket) ->
    case ssl_broker:peername(Socket) of
	{ok, {Address, Port}} ->
	    AddrStr = lists:flatten(io_lib:format("~w.~w.~w.~w", 
						  tuple_to_list(Address))),
	    {Port, AddrStr};
	{error, Reason} ->
	    exit('getpeername error')
    end.

%%
%% resolve() -> Result | {error, Reason}
%%
%% Silly name for [get]hostname()
%%
resolve() ->
    {ok, HostName} = inet:gethostname(),
    resolve(HostName).

%%
%% resolve(IPAddress)
%%
resolve(IPAddress) ->
    case inet:gethostbyname(IPAddress) of
	{ok, HostEnt} -> 
	    HostEnt#hostent.h_name;
	{error, Reason}  ->
	    {error, Reason}
    end.

%%
%% sockname(Socket)
%%
sockname(Socket) ->
    case ssl_broker:sockname(Socket) of
	{ok, {Address, Port}} ->
	    AddrStr = lists:flatten(io_lib:format("~w.~w.~w.~w", 
						  tuple_to_list(Address))),
	    {Port, AddrStr};
	{error, Reason} ->
	    exit('getsockname error')
    end.

%%
%% version()
%%
version() ->
    '2.0'.










