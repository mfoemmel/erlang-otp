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
%
-module(http_transport).

-export([start/1, connect/1, listen/2, listen/3, accept/2, accept/3, close/2,
	 send/3, controlling_process/3, setopts/3,
	 peername/2, resolve/0]).

-include("http.hrl").
-include_lib("kernel/include/inet.hrl").

%%%=========================================================================
%%%  Internal application API
%%%=========================================================================
%% start -> ok | {error,Reason}
start(ip_comm) ->
    case inet_db:start() of
	{ok,_Pid} ->
	    ok;
	{error,{already_started,_Pid}} ->
	    ok;
	Error ->
	    Error
    end;
start({ssl,_SSLConfig}) ->
    case ssl:start() of
	ok ->
	    ok;
	{ok, _} ->
	    ok;
	{error,{already_started,_}} ->
	    ok;
	Error ->
	    Error
    end.

connect(#request{scheme = http, address = {Host, Port}}) ->
    {Opts, NewHost} = 
	case inet:getaddr(Host, inet6) of
	    {ok, IPAddr} ->
		{[binary, {packet, 0}, {active, false},
		  {reuseaddr,true}, inet6], IPAddr};
	    {error, _} ->
		{[binary, {packet, 0}, {active, false},
		  {reuseaddr,true}], Host}
	end,
    gen_tcp:connect(NewHost, Port, Opts);

connect(#request{scheme = https, settings = Settings,
		 address = {Host, Port}}) ->
    Opts = [binary, {active, false}] ++ Settings#http_options.ssl,
    ssl:connect(Host, Port, Opts).

listen(SocketType, Port) ->
    listen(SocketType, undefined, Port).

listen(ip_comm, Addr, Port) ->
    FdName = list_to_atom("httpd_" ++ integer_to_list(Port)),
    {NewPort, Opt} =
	case init:get_argument(FdName) of
	    {ok, [[FdStr]]} ->
		Fd = list_to_integer(FdStr),
                   {0,
                    sock_opt(Addr, [{backlog,128},{reuseaddr,true},
                                    {fd,Fd}])};
	    error ->
		{Port,
		 sock_opt(Addr, [{backlog,128},{reuseaddr,true}])}
	end,
    case gen_tcp:listen(NewPort, Opt) of
	{ok, ListenSocket} ->
	    ListenSocket;
	Error ->
	    Error
    end;

listen({ssl, SSLConfig}, Addr, Port) ->
    Opt = sock_opt(Addr, SSLConfig),
    case ssl:listen(Port, Opt) of
	{ok, ListenSocket} ->
	    ListenSocket;
	Error ->
	    Error
    end.

accept(A, B) ->
    accept(A, B, infinity).
accept(ip_comm, ListenSocket, T) ->
    case gen_tcp:accept(ListenSocket, T) of
	{ok,Socket} ->
	    Socket;
	Error ->
	    error_logger:error_report("accept(ip_comm) failed for reason:"
		    "~n   Error: ~p",[Error]),
	    Error
    end;
accept({ssl,_SSLConfig}, ListenSocket, T) ->
    case ssl:accept(ListenSocket, T) of
	{ok,Socket} ->
	    Socket;
	Error ->
	    error_logger:error_report("accept(ssl) failed for reason:"
		    "~n   Error: ~p",[Error]),
	    Error
    end.

controlling_process(ip_comm, Socket, Pid) ->
    gen_tcp:controlling_process(Socket, Pid);
controlling_process({ssl, _}, Socket, Pid) ->
    ssl:controlling_process(Socket, Pid).

setopts(http, Socket, Options) ->
    inet:setopts(Socket,Options);
setopts(ip_comm, Socket, Options) ->
    setopts(http, Socket,Options);

setopts({ssl, _}, Socket, Options) ->
    setopts(https, Socket, Options);
setopts(https, Socket, Options) ->
    ssl:setopts(Socket, Options).

send(http, Socket, Message) ->
    gen_tcp:send(Socket, Message);
send(ip_comm, S, M) ->
    send(http, S, M);

send({ssl, _}, S, M) ->
    send(https, S, M);
send(https, Socket, Message) ->
    ssl:send(Socket, Message).

close(http, Socket) ->
    gen_tcp:close(Socket);
close(ip_comm, S) ->
    close(http, S);

close({ssl, _}, S) ->
    close(https, S);
close(https,Socket) ->
    ssl:close(Socket).

%% peername

peername(ip_comm, Socket) ->
    peername(http, Socket);

peername(http, Socket) ->
    case inet:peername(Socket) of
	{ok,{{A,B,C,D},Port}} ->
	    PeerName = integer_to_list(A)++"."++integer_to_list(B)++"."++
		integer_to_list(C)++"."++integer_to_list(D),
	    {Port,PeerName};
	{error,Reason} ->
	    error_logger:error_report("failed getting peername:"
				      "~n   Reason: ~p"
				      "~n   Socket: ~p",
				      [Reason,Socket]),
	    {-1,"unknown"}
    end;

peername({ssl,_SSLConfig},Socket) ->
    peername(https, Socket);

peername(https, Socket) ->
    case ssl:peername(Socket) of
	{ok,{{A,B,C,D},Port}} ->
	    PeerName = integer_to_list(A)++"."++integer_to_list(B)++"."++
		integer_to_list(C)++"."++integer_to_list(D),
	    {Port,PeerName};
	{error,_Reason} ->
	    {-1,"unknown"}
    end.

resolve() ->
    {ok,Name} = inet:gethostname(),
    Name.

%%%========================================================================
%%% Internal functions
%%%========================================================================

sock_opt(undefined,Opt) -> [{packet,0},{active,false}|Opt];
sock_opt(Addr,Opt)      -> [{ip, Addr},{packet,0},{active,false}|Opt].

