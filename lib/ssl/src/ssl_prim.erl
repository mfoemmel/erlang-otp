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

%% Purpose: Primitive interface to SSL, without broker process (used by 
%% SSL distribution).

-module(ssl_prim).

-export([listen/2, connect/3, accept/1, close/1, send/2, recv/2, recv/3,
	 getll/1, getstat/2, setopts/2, controlling_process/2, peername/1,
	 sockname/1, getif/1]).

-include("ssl_int.hrl").
-include("ssl_broker_int.hrl").

-define(filter(Call), filter((catch Call))).

listen(Port, Opts) ->
    St = newstate(listener),
    ?filter(ssl_broker:listen_prim(ssl_server_prim, self(), Port, nonactive(Opts), St)).

connect(Address, Port, Opts) ->
    St = newstate(connector),
    ?filter(ssl_broker:connect_prim(ssl_server_prim, inet_tcp, self(), Address, 
				    Port, nonactive(Opts), infinity, St)).

accept(ListenSt0) when record(ListenSt0, st) ->
    case transport_accept(ListenSt0) of
	{ok, ListenSt1} ->
	    ssl_accept(ListenSt1);
	Error ->
	    Error
    end.

transport_accept(ListenSt) when record(ListenSt, st) ->
    NewSt = newstate(acceptor),
    ListenSocket=ListenSt#st.thissock,
    ListenFd = ListenSocket#sslsocket.fd,
    ListenOpts = ListenSt#st.opts,
    ?filter(ssl_broker:transport_accept_prim(
	      ssl_server_prim, ListenFd,
	      ListenOpts, infinity, NewSt)).

ssl_accept(ListenSt) when record(ListenSt, st) ->
    NewSt = newstate(acceptor),
    ListenSocket=ListenSt#st.thissock,
    ListenFd = ListenSocket#sslsocket.fd,
    ?filter(ssl_broker:ssl_accept_prim(
	      ssl_server_prim, gen_tcp, self(), ListenFd, infinity, NewSt)).


close(_St = #st{fd = Fd}) when integer(Fd) ->
    ssl_server:close_prim(ssl_server_prim, Fd),
    ok;
close(_) ->
    ok.

send(St, Data) when record(St, st), St#st.status =:= open ->
    case inet_tcp:send(St#st.proxysock, Data) of
	ok ->
	    ok;
	{error, _} ->
	    {error, closed}
    end;

send(St, _Data) when record(St, st) ->
    {error, closed}.

recv(St,Length) ->
    recv(St,Length,infinity).

recv(St, Length, Tmo) when record(St, st), St#st.status =:= open ->
    inet_tcp:recv(St#st.proxysock, Length, Tmo);

recv(St, _Length, _Tmo) when record(St, st) ->
    {error, closed}.

getll(St) when record(St, st), St#st.status =:= open  ->
    inet:getll(St#st.proxysock);

getll(St) when record(St, st) ->
    {error, closed}.

getstat(St, Opts) when record(St, st), St#st.status =:= open  ->
    inet:getstat(St#st.proxysock, Opts);

getstat(St, _Opts) when record(St, st) ->
    {error, closed}.

setopts(St, Opts) when record(St, st), St#st.status =:= open  ->
    case remove_supported(Opts) of
	[] ->
	    inet:setopts(St#st.proxysock, Opts);
	_ ->
	    {error, enotsup}
    end;

setopts(St, _Opts) when record(St, st) ->
    {error, closed}.


controlling_process(St, Pid) when record(St, st), St#st.status =:= open, pid(Pid) ->
    inet_tcp:controlling_process(St#st.proxysock, Pid);

controlling_process(St, Pid) when record(St, st),  pid(Pid) ->
    {error, closed}.

peername(St) when record(St, st), St#st.status =:= open  ->
    case ssl_server:peername_prim(ssl_server_prim, St#st.fd) of
	{ok, {Address, Port}} ->
	    {ok, At} = inet_parse:ipv4_address(Address),
	    {ok, {At, Port}};
	Error ->
	    Error
    end;

peername(St) when record(St, st) ->
    {error, closed}.

sockname(St) when record(St, st), St#st.status =:= open  ->
    case ssl_server:sockname_prim(ssl_server_prim, St#st.fd) of
	{ok, {Address, Port}} ->
	    {ok, At} = inet_parse:ipv4_address(Address),
	    {ok, {At, Port}};
	Error ->
	    Error
    end;

sockname(St) when record(St, st) ->
    {error, closed}.

getif(St) when record(St, st), St#st.status =:= open  ->
    inet:getif(St#st.proxysock);

getif(St) when record(St, st) ->
    {error, closed}.

remove_supported([{active, _}|T]) ->
    remove_supported(T);
remove_supported([{packet,_}|T]) ->
    remove_supported(T);
remove_supported([{deliver,_}|T]) ->
    remove_supported(T);
remove_supported([H|T]) ->
    [H | remove_supported(T)];
remove_supported([]) ->
    [].

filter(Result) ->
    case Result of
	{ok, _Sock,St} ->
	    {ok, St};
	{error, Reason, _St} ->
	    {error,Reason};
	Else  ->
	    {error, Else}
    end.

nonactive([{active,_}|T]) ->
    nonactive(T);
nonactive([H|T]) ->
    [H | nonactive(T)];
nonactive([]) ->
    [{active, false}].

newstate(Type) ->
   #st{brokertype = Type, server = whereis(ssl_server_prim),
       client = undefined, collector = undefiend, debug = false}. 

