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
	 send/3, config/1, controlling_process/3, setopts/3,
	 peername/2, resolve/0]).

-include("http.hrl").
-include("httpd.hrl").
-include("httpd_verbosity.hrl").
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

connect(#request{scheme = http, address = Addr}, ProxyOptions) ->
    {Host,Port}  = handle_proxy(Addr,ProxyOptions),
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

connect(#request{scheme = https,settings = Settings,
		 address = Addr}, ProxyOptions) ->
    {Host, Port} = handle_proxy(Addr, ProxyOptions),
    Opts = [binary, {active, false}] ++ Settings#http_options.ssl,
    ssl:connect(Host, Port, Opts).

listen(SocketType,Port) ->
    listen(SocketType,undefined,Port).
listen(ip_comm,Addr,Port) ->
    ?DEBUG("listening(ip_comm) to port ~p", [Port]),
    Opt = sock_opt(Addr,[{backlog,128},{reuseaddr,true}]),
    case gen_tcp:listen(Port,Opt) of
	{ok,ListenSocket} ->
	    ListenSocket;
	Error ->
	    Error
    end;
listen({ssl,SSLConfig},Addr,Port) ->
    ?DEBUG("listening(ssl) to port ~p"
           "~n   SSLConfig: ~p", [Port,SSLConfig]),
    Opt = sock_opt(Addr,SSLConfig),
    case ssl:listen(Port, Opt) of
	{ok,ListenSocket} ->
	    ListenSocket;
	Error ->
	    Error
    end.

accept(A, B) ->
    accept(A, B, infinity).
accept(ip_comm,ListenSocket, T) ->
    ?DEBUG("accept(ip_comm) on socket ~p", [ListenSocket]),
    case gen_tcp:accept(ListenSocket, T) of
	{ok,Socket} ->
	    Socket;
	Error ->
	    ?vtrace("accept(ip_comm) failed for reason:"
		    "~n   Error: ~p",[Error]),
	    Error
    end;
accept({ssl,_SSLConfig},ListenSocket, T) ->
    ?DEBUG("accept(ssl) on socket ~p", [ListenSocket]),
    case ssl:accept(ListenSocket, T) of
	{ok,Socket} ->
	    Socket;
	Error ->
	    ?vtrace("accept(ssl) failed for reason:"
		    "~n   Error: ~p",[Error]),
	    Error
    end.

%% config (debug: {certfile, "/var/tmp/server_root/conf/ssl_server.pem"})
config(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,com_type,ip_comm) of
	ssl ->
	    case ssl_certificate_file(ConfigDB) of
		undefined ->
		    {error,
		     ?NICE("Directive SSLCertificateFile "
			   "not found in the config file")};
		SSLCertificateFile ->
		    {ssl,
		     SSLCertificateFile++
		     ssl_certificate_key_file(ConfigDB)++
		     ssl_verify_client(ConfigDB)++
		     ssl_ciphers(ConfigDB)++
		     ssl_password(ConfigDB)++
		     ssl_verify_depth(ConfigDB)++
		     ssl_ca_certificate_file(ConfigDB)}
	    end;
	ip_comm ->
	    ip_comm
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
	    ?DEBUG("peername(ip_comm) on socket ~p: ~p",
		   [Socket,{Port,PeerName}]),
	    {Port,PeerName};
	{error,Reason} ->
	    ?vlog("failed getting peername:"
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
	    ?DEBUG("peername(ssl) on socket ~p: ~p", 
		   [Socket, {Port,PeerName}]),
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
%%% Check to see if the given {Host,Port} tuple is in the NoProxyList
%%% Returns an eventually updated {Host,Port} tuple, with the proxy address
handle_proxy(HostPort = {Host, _Port}, {Proxy, NoProxy}) ->
    case Proxy of
	undefined ->
	    HostPort;
	Proxy ->
	    case is_no_proxy_dest(Host, NoProxy) of
		true ->
		    HostPort;
		false ->
		    Proxy
	    end
    end.

is_no_proxy_dest(_, []) ->
    false;
is_no_proxy_dest(Host, [ "*." ++ NoProxyDomain | NoProxyDests]) ->    
    
    case is_no_proxy_dest_domain(Host, NoProxyDomain) of
	true ->
	    true;
	false ->
	    is_no_proxy_dest(Host, NoProxyDests)
    end;

is_no_proxy_dest(Host, [NoProxyDest | NoProxyDests]) ->
    IsNoProxyDest = case is_hostname(NoProxyDest) of
			true ->
			    fun is_no_proxy_host_name/2;
			false ->
			    fun is_no_proxy_dest_address/2
		    end,
    
    case IsNoProxyDest(Host, NoProxyDest) of
	true ->
	    true;
	false ->
	    is_no_proxy_dest(Host, NoProxyDests)
    end.

is_hostname(Dest) ->
    inet_parse:domain(Dest).

is_no_proxy_host_name(Host, Host) ->
    true;
is_no_proxy_host_name(_,_) ->
    false.

is_no_proxy_dest_domain(Dest, DomainPart) ->
    lists:suffix(DomainPart, Dest).

is_no_proxy_dest_address(Dest, Dest) ->
    true;
is_no_proxy_dest_address(Dest, AddressPart) ->
    lists:prefix(AddressPart, Dest).
    
ssl_certificate_file(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,ssl_certificate_file) of
	undefined ->
	    undefined;
	SSLCertificateFile ->
	    [{certfile,SSLCertificateFile}]
    end.

ssl_certificate_key_file(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,ssl_certificate_key_file) of
	undefined ->
	    [];
	SSLCertificateKeyFile ->
	    [{keyfile,SSLCertificateKeyFile}]
    end.

ssl_verify_client(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,ssl_verify_client) of
	undefined ->
	    [];
	SSLVerifyClient ->
	    [{verify,SSLVerifyClient}]
    end.

ssl_ciphers(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,ssl_ciphers) of
	undefined ->
	    [];
	Ciphers ->
	    [{ciphers, Ciphers}]
    end.

ssl_password(ConfigDB) ->
    case httpd_util:lookup(ConfigDB,ssl_password_callback_module) of
	undefined ->
	    [];
	Module ->
	    case httpd_util:lookup(ConfigDB, 
				   ssl_password_callback_function) of
		undefined ->
		    [];
		Function ->
		    case catch apply(Module, Function, []) of
			Password when list(Password) ->
			    [{password, Password}];
			Error ->
			    error_report(ssl_password,Module,Function,Error),
			    []
		    end
	    end
    end.

ssl_verify_depth(ConfigDB) ->
    case httpd_util:lookup(ConfigDB, ssl_verify_client_depth) of
	undefined ->
	    [];
	Depth ->
	    [{depth, Depth}]
    end.

ssl_ca_certificate_file(ConfigDB) ->
    case httpd_util:lookup(ConfigDB, ssl_ca_certificate_file) of
	undefined ->
	    [];
	File ->
	    [{cacertfile, File}]
    end.

sock_opt(undefined,Opt) -> [{packet,0},{active,false}|Opt];
sock_opt(Addr,Opt)      -> [{ip, Addr},{packet,0},{active,false}|Opt].

error_report(Where,M,F,Error) ->
    error_logger:error_report([{?MODULE, Where}, 
			       {apply, {M, F, []}}, Error]).
