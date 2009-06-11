%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
%
-module(http_transport).

% Internal application API
-export([start/1, connect/3, connect/4, listen/2, listen/3, 
	 accept/2, accept/3, close/2,
	 send/3, controlling_process/3, setopts/3,
	 peername/2, resolve/0]).

-export([negotiate/3]).

%%%=========================================================================
%%%  Internal application API
%%%=========================================================================

%%-------------------------------------------------------------------------
%% start(SocketType) -> ok | {error, Reason}
%%      SocketType = ip_comm | {ssl, _}  
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
	{error, {already_started,_}} ->
	    ok;
	Error ->
	    Error
    end.


%%-------------------------------------------------------------------------
%% connect(SocketType, Address, Options, Timeout) ->
%%                                            {ok, Socket} | {error, Reason}
%%      SocketType = ip_comm | {ssl, SslConfig}  
%%      Address = {Host, Port}
%%      Options = [option()]
%%      Socket = socket()
%%      option() = ipfamily() | {ip, ip_address()} | {port, integer()}
%%      ipfamily() = inet | inet6 
%%                                   
%% Description: Connects to the Host and Port specified in HTTPRequest.
%%-------------------------------------------------------------------------

connect(SocketType, Address, Opts) ->
    connect(SocketType, Address, Opts, infinity).

connect(ip_comm = _SocketType, {Host, Port}, Opts0, Timeout) 
  when is_list(Opts0) ->
    Opts = [binary, {packet, 0}, {active, false}, {reuseaddr, true} | Opts0],
    gen_tcp:connect(Host, Port, Opts, Timeout);

connect({ssl, SslConfig}, {Host, Port}, _, Timeout) ->
    Opts = [binary, {active, false}] ++ SslConfig,
    ssl:connect(Host, Port, Opts, Timeout);

connect({erl_ssl, SslConfig}, {Host, Port}, _, Timeout) ->
    Opts = [binary, {active, false}, {ssl_imp, new}] ++ SslConfig,
    ssl:connect(Host, Port, Opts, Timeout).


%%-------------------------------------------------------------------------
%% listen(SocketType, Port) -> {ok, Socket} | {error, Reason}
%%      SocketType = ip_comm | {ssl, SSLConfig}  
%%      Port = integer() 
%%      Socket = socket()                            
%%
%% Description: Sets up socket to listen on the port Port on the local
%% host using either gen_tcp or ssl. In the gen_tcp case the port
%% might allready have been initiated by a wrapper-program and is
%% given as an Fd that can be retrieved by init:get_argument. The
%% reason for this to enable a HTTP-server not running as root to use
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
		 sock_opt(ip_comm, Addr, 
			  [{backlog, 128}, {reuseaddr, true}])}
	end,
    case gen_tcp:listen(NewPort, Opt) of
	{error,eafnosupport} ->
	    gen_tcp:listen(NewPort, lists:delete(inet6, Opt));
	Other ->
	    Other
    end;

listen({ssl, SSLConfig} = Ssl, Addr, Port) ->
    Opt = sock_opt(Ssl, Addr, SSLConfig),
    ssl:listen(Port, Opt);

listen({erl_ssl, SSLConfig} = Ssl, Addr, Port) ->
    Opt = sock_opt(Ssl, Addr, SSLConfig),
    ssl:listen(Port, [{ssl_imp, new} | Opt]).


%%-------------------------------------------------------------------------
%% accept(SocketType, ListenSocket) -> {ok, Socket} | {error, Reason}
%% accept(SocketType, ListenSocket, Timeout) -> ok | {error, Reason}
%%   SocketType = ip_comm | {ssl, SSLConfig}  
%%   ListenSocket = socket()    
%%   Timeout = infinity | integer() >= 0
%%   Socket = socket()
%%                                   
%% Description: Accepts an incoming connection request on a listen socket,
%% using either gen_tcp or ssl.
%%-------------------------------------------------------------------------
accept(SocketType, ListenSocket) ->
    accept(SocketType, ListenSocket, infinity).
accept(ip_comm, ListenSocket, Timeout) ->
    gen_tcp:accept(ListenSocket, Timeout);
accept({ssl,_SSLConfig}, ListenSocket, Timeout) ->
    ssl:transport_accept(ListenSocket, Timeout).

%%-------------------------------------------------------------------------
%% controlling_process(SocketType, Socket, NewOwner) -> ok | {error, Reason}
%%   SocketType = ip_comm | {ssl, _}  
%%   Socket = socket()        
%%   NewOwner = pid()
%%                                
%% Description: Assigns a new controlling process to Socket. 
%%-------------------------------------------------------------------------
controlling_process(ip_comm, Socket, NewOwner) ->
    gen_tcp:controlling_process(Socket, NewOwner);
controlling_process({ssl, _}, Socket, NewOwner) ->
    ssl:controlling_process(Socket, NewOwner).

%%-------------------------------------------------------------------------
%% setopts(SocketType, Socket, Options) -> ok | {error, Reason}
%%     SocketType = ip_comm | {ssl, _}
%%     Socket = socket()
%%     Options = list()                              
%% Description: Sets one or more options for a socket, using either
%% gen_tcp or ssl.
%%-------------------------------------------------------------------------
setopts(ip_comm, Socket, Options) ->
    inet:setopts(Socket,Options);
setopts({ssl, _}, Socket, Options) ->
    ssl:setopts(Socket, Options).

%%-------------------------------------------------------------------------
%% send(RequestOrSocketType, Socket, Message) -> ok | {error, Reason}
%%     SocketType = ip_comm | {ssl, _}
%%     Socket = socket()
%%     Message = list() | binary()                           
%% Description: Sends a packet on a socket, using either gen_tcp or ssl.
%%-------------------------------------------------------------------------
send(ip_comm, Socket, Message) ->
    gen_tcp:send(Socket, Message);
send({ssl, _}, Socket, Message) ->
    ssl:send(Socket, Message).

%%-------------------------------------------------------------------------
%% close(SocketType, Socket) -> ok | {error, Reason}
%%     SocketType = ip_comm | {ssl, _}
%%     Socket = socket()  
%%                                   
%% Description: Closes a socket, using either gen_tcp or ssl.
%%-------------------------------------------------------------------------
close(ip_comm, Socket) ->
    gen_tcp:close(Socket);
close({ssl, _}, Socket) ->
    ssl:close(Socket).

%%-------------------------------------------------------------------------
%% peername(SocketType, Socket) -> ok | {error, Reason}
%%     SocketType = ip_comm | {ssl, _}
%%     Socket = socket() 
%%                          
%% Description: Returns the address and port for the other end of a
%% connection, usning either gen_tcp or ssl.
%%-------------------------------------------------------------------------
peername(ip_comm, Socket) ->
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

peername({ssl, _}, Socket) ->
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
%%     HostName = string()
%%     
%% Description: Returns the local hostname. 
%%-------------------------------------------------------------------------
resolve() ->
    {ok, Name} = inet:gethostname(),
    Name.


%%%========================================================================
%%% Internal functions
%%%========================================================================

%% Address any comes from directive: BindAddress "*"
sock_opt(ip_comm, any = Addr, Opt) -> 
    sock_opt1([{ip, Addr} | Opt]);
sock_opt(ip_comm, undefined, Opt) -> 
    sock_opt1(Opt);
sock_opt(_, any = Addr, Opt) ->
    sock_opt2([{ip, Addr} | Opt]);
sock_opt(_, undefined, Opt) ->
    sock_opt2(Opt);
sock_opt(_, {_,_,_,_} = Addr, Opt) -> 
    sock_opt2([{ip, Addr} | Opt]);
sock_opt(ip_comm, Addr, Opt) -> 
    sock_opt2([inet6, {ip, Addr} | Opt]);
sock_opt(_, Addr, Opt) ->
    sock_opt2([{ip, Addr} | Opt]).

sock_opt1(Opt) ->
     sock_opt2([inet6 | Opt]).

sock_opt2(Opt) ->
    [{packet, 0}, {active, false} | Opt].

negotiate(ip_comm,_,_) ->
    ok;
negotiate({ssl,_},Socket,Timeout) ->
    negotiate(Socket, Timeout);
negotiate({erl_ssl, _}, Socket, Timeout) ->
    negotiate(Socket, Timeout).

negotiate(Socket, Timeout) ->
    case ssl:ssl_accept(Socket,Timeout) of
	ok ->
	    ok;
	{error, Error} ->
	    case lists:member(Error,
			      [timeout,econnreset,esslaccept,esslerrssl]) of
		true ->
		    {error,normal};
		false ->
		    {error, Error}
           end
    end.
