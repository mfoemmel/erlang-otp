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

%%% Purpose : Main API module for SSL.

-module(ssl).

-export([start/0, stop/0, accept/1, accept/2, close/1, connect/3, connect/4,
	 controlling_process/2, listen/2,  pid/1, port/1, peername/1, 
	 recv/2, recv/3, send/2, setopts/2, sockname/1, format_error/1]).

-include("ssl_int.hrl").

start() ->
    application:start(ssl).

stop() ->
    application:stop(ssl).

%% accept(ListenSocket) -> {ok, Socket}.
%% accept(ListenSocket, Timeout) -> {ok, Socket}.
%%
accept(ListenSocket) ->
    accept(ListenSocket, infinity).

accept(ListenSocket, Timeout) when record(ListenSocket, sslsocket) ->
    {ok, Pid} = ssl_broker:start_broker(acceptor),
    ssl_broker:accept(Pid, ListenSocket, [], Timeout).

%% close(Socket) -> ok
%%
close(Socket) when record(Socket, sslsocket) ->
    ssl_broker:close(Socket).

%% connect(Address, Port, Options) -> {ok, Socket}
%% connect(Address, Port, Options, Timeout) -> {ok, Socket}
%%
connect(Address, Port, Options) ->
    connect(Address, Port, Options, infinity).
connect(Address, Port, Options, Timeout) ->
    {ok, Pid} = ssl_broker:start_broker(connector),
    ssl_broker:connect(Pid, Address, Port, Options, Timeout).

%% controlling_process(Socket, NewOwner) -> ok | {error, Reason}
%%
controlling_process(Socket, NewOwner) when pid(NewOwner) ->
    ssl_broker:controlling_process(Socket, NewOwner).

%% listen(Port, Options) -> {ok, ListenSock} | {error, Reason}
%%
listen(Port, []) ->
    {error, enooptions};
listen(Port, Options) ->
    {ok, Pid} = ssl_broker:start_broker(listener),
    ssl_broker:listen(Pid, Port, Options).

%% peername(Socket) -> {ok, {Address, Port}} | {error, Reason}
%%
%% 
peername(Socket) when record(Socket, sslsocket) ->
    ssl_broker:peername(Socket).

%% pid(Socket) -> pid()
%%
%% 
pid(Socket) when record(Socket, sslsocket) ->
   Socket#sslsocket.pid.

%% port(Socket) -> {ok, Port} | {error, Reason}
%%
%%
port(Socket) when record(Socket, sslsocket) ->
    case sockname(Socket) of
	{ok, {_, Port}} ->
	    {ok, Port};
	{error, Reason} ->
	    {error, Reason}
    end.

%%
%% recv(Socket, Length) -> {ok, Data} | {error, reason}
%% recv(Socket, Length, Timeout) -> {ok, Data} | {error, reason}
%%
recv(Socket, Length) ->
    recv(Socket, Length, infinity).
recv(Socket, Length, Timeout) when record(Socket, sslsocket) -> 
    ssl_broker:recv(Socket, Length, Timeout).

%% send(Socket, Data) -> ok
%%
send(Socket, Data) when record(Socket, sslsocket) -> 
    ssl_broker:send(Socket, Data).

%% setopts(Socket, Options) -> ok | {error, Reason}
%%
%% 
setopts(Socket, Options) when record(Socket, sslsocket) ->
    ssl_broker:setopts(Socket, Options).

%% sockname(Socket) -> {ok, {Address, Port}} | {error, Reason}
%%
%% 
sockname(Socket) when record(Socket, sslsocket) ->
    ssl_broker:sockname(Socket).


%% format_error(Term) -> string()
%% 
%%

format_error({error, Reason}) ->
    format_error(Reason);
format_error(closed) ->
    "Connection closed for the operation in question.";
format_error(ebadsocket) ->
    "Connection not found (internal error).";
format_error(ebadstate) ->
    "Connection not in connect state (internal error).";
format_error(ebrokertype) ->
    "Wrong broker type (internal error).";
format_error(ecacertfile) ->
    "Own CA certificate file is invalid.";
format_error(ecertfile) ->
    "Own certificate file is invalid.";
format_error(echaintoolong) ->
    "The chain of certificates provided by peer is too long.";
format_error(ecipher) ->
    "Own list of specified ciphers is invalid.";
format_error(ekeyfile) ->
    "Own private key file is invalid.";
format_error(ekeymismatch) ->
    "Own private key does not match own certificate.";
format_error(enoissuercert) ->
    "Cannot find certificate of issuer of certificate provided by peer.";
format_error(enoservercert) ->
    "Attempt to do accept without having set own certificate.";
format_error(enotlistener) ->
    "Attempt to accept on a non-listening socket.";
format_error(enoproxysocket) ->
    "No proxy socket found (internal error).";
format_error(enooptions) ->
    "List of options is empty.";
format_error(eoptions) ->
    "Invalid list of options.";
format_error(epeercert) ->
    "Certificate provided by peer is in error.";
format_error(epeercertexpired) ->
    "Certificate provided by peer has expired.";
format_error(epeercertinvalid) ->
    "Certificate provided by peer is invalid.";
format_error(eselfsignedcert) ->
    "Certificate provided by peer is self signed.";
format_error(esslaccept) ->
    "Server SSL handshake procedure between client and server failed.";
format_error(esslconnect) ->
    "Client SSL handshake procedure between client and server failed.";
format_error(esslerrssl) ->
    "SSL protocol failure. Typically because of a fatal alert from peer.";
format_error(ewantconnect) ->
    "Protocol wants to connect, which is not supported in this "
	"version of the SSL application.";
format_error(ex509lookup) ->
    "Protocol wants X.509 lookup, which is not supported in this "
	"version of the SSL application.";
format_error({badcall, Call}) ->
    "Call not recognized for current mode (active or passive) and state "
	"of socket.";
format_error({badcast, Cast}) ->
    "Call not recognized for current mode (active or passive) and state "
	"of socket."; 
format_error({badinfo, Info}) ->
    "Call not recognized for current mode (active or passive) and state "
	"of socket.".

    
