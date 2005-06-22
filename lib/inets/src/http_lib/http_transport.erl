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

-export([start/1, connect/2, listen/2, listen/3, accept/2, accept/3, close/2,
	 send/3, controlling_process/3, setopts/3,
	 peername/2, resolve/0]).

-include("http.hrl").

%%%=========================================================================
%%%  Internal application API
%%%=========================================================================

%%-------------------------------------------------------------------------
%% start(SocketType) -> ok | {error, Reason}
%%      SocketType - ip_comm | {ssl, _}  
%%                                   
%% Description: Makes sure inet_db or ssl is started. 
%%-------------------------------------------------------------------------
start(ip_comm) ->
    case inet_db:start() of
	{ok, _} ->
	    ok;
	{error, {already_started, _}} ->
	    ok;
	Error ->
	    Error
    end;
start({ssl, _}) ->
    case ssl:start() of
	ok ->
	    ok;
	{ok, _} ->
	    ok;
	{error, {already_started,_}} ->
	    ok;
	Error ->
	    Error
    end.

%%-------------------------------------------------------------------------
%% connect(HTTPRequest, IPV6) -> ok | {error, Reason}
%%      HTTPRequest - #request{}
%%      IPV6 - disabled | enabled
%%                                   
%% Description: Connects to the Host and Port specified in HTTPRequest.
%%		uses ipv6 if possible.
%%-------------------------------------------------------------------------
connect(#request{scheme = http, address = {Host, Port}}, enabled) ->
    {Opts, NewHost} = 
	case (catch inet:getaddr(Host, inet6)) of
	    {ok, IPAddr} ->
		{[binary, {packet, 0}, {active, false},
		  {reuseaddr,true}, inet6], IPAddr};
	    _ ->
		{[binary, {packet, 0}, {active, false},
		  {reuseaddr,true}], Host}
	end,
    gen_tcp:connect(NewHost, Port, Opts);

connect(#request{scheme = http, address = {Host, Port}}, disabled) ->
    Opts = [binary, {packet, 0}, {active, false}, {reuseaddr,true}],
    gen_tcp:connect(Host, Port, Opts);

connect(#request{scheme = https, settings = Settings,
		 address = {Host, Port}}, _) ->
    Opts = [binary, {active, false}] ++ Settings#http_options.ssl,
    ssl:connect(Host, Port, Opts).

%%-------------------------------------------------------------------------
%% listen(SocketType, Port) -> ok | {error, Reason}
%%      SocketType - ip_comm | {ssl, SSLConfig}  
%%      Port - integer()                             
%%
%% Description: Sets up socket to listen on the port Port on the local
%% host using either gen_tcp or ssl. In the gen_tcp case the port
%% might allready have been initiated by a wrapper-program and is
%% given as an Fd that can be retrieved by init:get_argument. The
%% reason for this to enable a HTTP-server not runnig as root to use
%% port 80.
%%-------------------------------------------------------------------------
listen(SocketType, Port) ->
    listen(SocketType, undefined, Port).

listen(ip_comm, Addr, Port) ->
    FdName = list_to_atom("httpd_" ++ integer_to_list(Port)),
    {NewPort, Opt} =
	case init:get_argument(FdName) of
	    {ok, [[FdStr]]} ->
		Fd = list_to_integer(FdStr),
		{0,
		 sock_opt(ip_comm, Addr, [{backlog, 128}, 
					  {reuseaddr,true}, {fd,Fd}])};
	    error ->
		{Port,
		 sock_opt(ip_comm, Addr, [{backlog, 128}, {reuseaddr, true}])}
	end,
    gen_tcp:listen(NewPort, Opt);

listen({ssl, SSLConfig} = Ssl, Addr, Port) ->
    Opt = sock_opt(Ssl, Addr, SSLConfig),
    ssl:listen(Port, Opt).

%%-------------------------------------------------------------------------
%% accept(SocketType, ListenSocket) -> ok | {error, Reason}
%% accept(SocketType, ListenSocket, Timeout) -> ok | {error, Reason}
%%   SocketType - ip_comm | {ssl, SSLConfig}  
%%   ListenSocket - socket()    
%%   Timeout - infinity | integer() >= 0
%%                                   
%% Description: Accepts an incoming connection request on a listen socket,
%% using either gen_tcl or ssl.
%%-------------------------------------------------------------------------
accept(SocketType, ListenSocket) ->
    accept(SocketType, ListenSocket, infinity).
accept(ip_comm, ListenSocket, Timeout) ->
    gen_tcp:accept(ListenSocket, Timeout);
accept({ssl,_SSLConfig}, ListenSocket, Timeout) ->
    ssl:accept(ListenSocket, Timeout).

%%-------------------------------------------------------------------------
%% controlling_process(SocketType, Socket, NewOwner) -> ok | {error, Reason}
%%   SocketType - ip_comm | {ssl, _}  
%%   Socket - socket()        
%%   NewOwner - pid()
%%                                
%% Description: Assigns a new controlling process to Socket. 
%%-------------------------------------------------------------------------
controlling_process(ip_comm, Socket, NewOwner) ->
    gen_tcp:controlling_process(Socket, NewOwner);
controlling_process({ssl, _}, Socket, NewOwner) ->
    ssl:controlling_process(Socket, NewOwner).

%%-------------------------------------------------------------------------
%% setopts(RequestType, Socket, Options) -> ok | {error, Reason}
%%     RequestType - http | https
%%     Socket - socket()
%%     Options - list()                              
%% Description: Sets one or more options for a socket, using either
%% gen_tcl or ssl.
%%-------------------------------------------------------------------------
setopts(http, Socket, Options) ->
    inet:setopts(Socket,Options);
setopts(ip_comm, Socket, Options) ->
    setopts(http, Socket,Options);

setopts({ssl, _}, Socket, Options) ->
    setopts(https, Socket, Options);
setopts(https, Socket, Options) ->
    ssl:setopts(Socket, Options).

%%-------------------------------------------------------------------------
%% send(RequestOrSocketType, Socket, Message) -> ok | {error, Reason}
%%     RequestOrSocketType - http | https | ip_comm | {ssl, _}
%%     Socket - socket()
%%     Message - list() | binary()                           
%% Description: Sends a packet on a socket, using either gen_tcp or ssl.
%%-------------------------------------------------------------------------
send(http, Socket, Message) ->
    gen_tcp:send(Socket, Message);
send(ip_comm, S, M) ->
    send(http, S, M);

send({ssl, _}, S, M) ->
    send(https, S, M);
send(https, Socket, Message) ->
    ssl:send(Socket, Message).

%%-------------------------------------------------------------------------
%% close(RequestOrSocketType, Socket) -> ok | {error, Reason}
%%     RequestOrSocketType - http | https | ip_comm | {ssl, _}
%%     Socket - socket()  
%%                                   
%% Description: Closes a socket, using either gen_tcp or ssl.
%%-------------------------------------------------------------------------
close(http, Socket) ->
    gen_tcp:close(Socket);
close(ip_comm, S) ->
    close(http, S);

close({ssl, _}, S) ->
    close(https, S);
close(https,Socket) ->
    ssl:close(Socket).

%%-------------------------------------------------------------------------
%% peername(RequestOrSocketType, Socket) -> ok | {error, Reason}
%%     RequestOrSocketType - http | https | ip_comm | {ssl, _}
%%     Socket - socket() 
%%                          
%% Description: Returns the address and port for the other end of a connection,
%% usning either gen_tcp or ssl.
%%-------------------------------------------------------------------------
peername(ip_comm, Socket) ->
    peername(http, Socket);

peername(http, Socket) ->
    case inet:peername(Socket) of
	{ok,{{A, B, C, D}, Port}} ->
	    PeerName = integer_to_list(A)++"."++integer_to_list(B)++"."++
		integer_to_list(C)++"."++integer_to_list(D),
	    {Port, PeerName};
	{ok,{{A, B, C, D, E, F, G, H}, Port}} ->
	    PeerName =  http_util:integer_to_hexlist(A) ++ ":"++  
		http_util:integer_to_hexlist(B) ++ ":" ++  
		http_util:integer_to_hexlist(C) ++ ":" ++ 
		http_util:integer_to_hexlist(D) ++ ":" ++  
		http_util:integer_to_hexlist(E) ++ ":" ++  
		http_util:integer_to_hexlist(F) ++ ":" ++  
		http_util:integer_to_hexlist(G) ++":"++  
		http_util:integer_to_hexlist(H),
	    {Port, PeerName};
	{error, _} ->
	    {-1, "unknown"}
    end;

peername({ssl,_SSLConfig}, Socket) ->
    peername(https, Socket);

peername(https, Socket) ->
    case ssl:peername(Socket) of
	{ok,{{A, B, C, D}, Port}} ->
	    PeerName = integer_to_list(A)++"."++integer_to_list(B)++"."++
		integer_to_list(C)++"."++integer_to_list(D),
	    {Port, PeerName};
	{error, _} ->
	    {-1, "unknown"}
    end.
%%-------------------------------------------------------------------------
%% resolve() -> HostName
%%     HostName - string()
%%     
%% Description: Returns the local hostname. 
%%-------------------------------------------------------------------------
resolve() ->
    {ok,Name} = inet:gethostname(),
    Name.

%%%========================================================================
%%% Internal functions
%%%========================================================================
sock_opt(ip_comm, undefined, Opt) -> 
    case (catch inet:getaddr("localhost",inet6)) of
	{ok, _} ->
	    [inet6, {packet,0}, {active,false} | Opt];
	_ ->
	    [{packet,0}, {active,false} | Opt]
    end;
sock_opt(_, undefined, Opt) ->
    [{packet,0}, {active,false} | Opt];
sock_opt(_, Addr, Opt) when size(Addr) == 4 -> 
    [{ip, Addr}, {packet,0}, {active,false} | Opt];
sock_opt(ip_comm, Addr, Opt) -> 
    [inet6, {ip, Addr}, {packet,0}, {active,false} | Opt];
sock_opt(_, Addr, Opt) ->
    [{ip, Addr}, {packet,0}, {active,false} | Opt].
