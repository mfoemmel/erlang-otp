%%<copyright>
%% <year>1999-2008</year>
%% <holder>Ericsson AB, All Rights Reserved</holder>
%%</copyright>
%%<legalnotice>
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
%% The Initial Developer of the Original Code is Ericsson AB.
%%</legalnotice>
%%

%%% Purpose : Main API module for SSL.

-module(ssl).

-export([start/0, start/1, stop/0, accept/1, accept/2,
         transport_accept/1, transport_accept/2, ssl_accept/1, ssl_accept/2,
         ciphers/0, cipher_suites/0, cipher_suites/1, close/1, shutdown/2,
         connect/3, connect/2, connect/4, connection_info/1,
         controlling_process/2, listen/2, pid/1, port/1, 
	 peername/1, recv/2, recv/3, send/2, getopts/2, setopts/2, seed/1,
         sockname/1, peercert/1, peercert/2, version/0, versions/0,
	 session_info/1, 
	 format_error/1]).

%% Should be deprecated as soon as old ssl is removed
%%-deprecated({pid, 1, next_major_release}).
-deprecated({port, 1, next_major_release}).
-deprecated({accept, 1, next_major_release}).
-deprecated({accept, 2, next_major_release}).

-include("ssl_int.hrl").
-include("ssl_internal.hrl").

%%--------------------------------------------------------------------
%% Function: start([, Type]) -> ok
%%
%%  Type =  permanent | transient | temporary
%%  Vsns = [Vsn] 
%%  Vsn = ssl3 | tlsv1 | 'tlsv1.1'
%%
%% Description: Starts the ssl application. Default type
%% is temporary. see application(3)
%%--------------------------------------------------------------------
start() ->
    application:start(ssl).
start(Type) ->
    application:start(ssl, Type).

%%--------------------------------------------------------------------
%% Function: stop() -> ok
%%
%% Description: Stops the ssl application.
%%--------------------------------------------------------------------
stop() ->
    application:stop(ssl).

%%--------------------------------------------------------------------
%% Function: connect(Address, Port, Options[, Timeout]) -> {ok, Socket}
%%
%% Description: Connect to a ssl server.
%%--------------------------------------------------------------------
connect(Socket, SslOptions) when is_port(Socket) ->
    connect(Socket, SslOptions, infinity).

connect(Socket, SslOptions, Timeout) when is_port(Socket) ->
    EmulatedOptions = emulated_options(),
    {ok, Inetvalues} = inet:getopts(Socket, EmulatedOptions),
    inet:setopts(Socket, internal_inet_values()), 
    try handle_options(Inetvalues ++ SslOptions) of
	{ok, Options0} ->
	    {CbInfo, Options} =
		handle_transport_protocol(Options0),
	    case inet:peername(Socket) of
		{ok, {Address, Port}} ->
		    ssl_connection:connect(Address, Port, Socket, 
					   ssl_connection_options(Options), 
					   self(), 
					   CbInfo, Timeout);
		{error, Error} ->
		    {error, Error}
	    end
    catch 
	{error, Reason} ->
            {error, Reason}
    end;

connect(Address, Port, Options) ->
    connect(Address, Port, Options, infinity).

connect(Address, Port, Options0, Timeout) ->
    case proplists:get_value(ssl_imp, Options0, old) of
        new ->
            new_connect(Address, Port, Options0, Timeout);
        old ->
	    %% Allow the option reuseaddr to be present
	    %% so that new and old ssl can be run by the same
	    %% code, however the option will be ignored by old ssl
	    %% that hardcodes reuseaddr to true in its portprogram.
	    Options =  proplists:delete(reuseaddr, Options0),
            old_connect(Address, Port, Options, Timeout);
	Value ->
	    {error, {eoptions, {ssl_imp, Value}}}
    end.

%%--------------------------------------------------------------------
%% Function: listen(Port, Options) -> {ok, ListenSock} | {error, Reason}
%%
%% Description: Creates a ssl listen socket.
%%--------------------------------------------------------------------
listen(_Port, []) ->
    {error, enooptions};
listen(Port, Options0) ->
    case proplists:get_value(ssl_imp, Options0, old) of
	new ->
	    new_listen(Port, Options0);
	old ->
	    %% Allow the option reuseaddr to be present
	    %% so that new and old ssl can be run by the same
	    %% code, however the option will be ignored by old ssl
	    %% that hardcodes reuseaddr to true in its portprogram.
	    Options =  proplists:delete(reuseaddr, Options0),
	    old_listen(Port, Options);
	Value ->
	    {error, {eoptions, {ssl_imp, Value}}}
    end.

%%--------------------------------------------------------------------
%% Function: transport_accept(ListenSocket[, Timeout]) -> {ok, Socket}.
%%
%% Description: Performs transport accept on a ssl listen socket 
%%--------------------------------------------------------------------
transport_accept(ListenSocket) ->
    transport_accept(ListenSocket, infinity).

transport_accept(#sslsocket{pid = {ListenSocket, Options0},
                            fd = new_ssl} = SslSocket, Timeout) ->
    {{CbModule, _, _} = CbInfo, {SSlOpts, EmOpts, _}} =
        handle_transport_protocol(Options0),
    {ok, Socket} = CbModule:accept(ListenSocket, Timeout),
    {ok, Port} = inet:port(Socket),
    case ssl_connection_sup:start_child([server, "localhost", Port, Socket,
                                         {SSlOpts, EmOpts}, self(),
                                         CbInfo]) of
        {ok, Pid} ->
            CbModule:controlling_process(Socket, Pid),
            {ok, SslSocket#sslsocket{pid = Pid}};
        {error, Reason} ->
            {error, Reason}
    end;

transport_accept(#sslsocket{} = ListenSocket, Timeout) ->
    ensure_old_ssl_started(),
    {ok, Pid} = ssl_broker:start_broker(acceptor),
    ssl_broker:transport_accept(Pid, ListenSocket, Timeout).

%%--------------------------------------------------------------------
%% Function: ssl_accept(ListenSocket[, Timeout]) -> {ok, Socket}.
%%
%% Description: Performs accept on a ssl listen socket. e.i. performs
%%              ssl handshake. 
%%--------------------------------------------------------------------
ssl_accept(ListenSocket) ->
    ssl_accept(ListenSocket, infinity).

ssl_accept(#sslsocket{pid = Pid, fd = new_ssl}, Timeout) ->
    gen_fsm:send_event(Pid, socket_control),
    try gen_fsm:sync_send_all_state_event(Pid, started, Timeout) of
	connected ->
            ok
    catch
        exit:{noproc, _} ->
            {error, closed};
	exit:{timeout, _} ->
            {error, timeout};
	exit:{normal, _} ->
            {error, closed}
    end;

ssl_accept(ListenSocket, SslOptions)  when is_port(ListenSocket) -> 
    ssl_accept(ListenSocket, SslOptions, infinity);

%% Old ssl
ssl_accept(#sslsocket{} = Socket, Timeout)  ->
    ensure_old_ssl_started(),
    ssl_broker:ssl_accept(Socket, Timeout).

ssl_accept(ListenSocket, SslOptions, Timeout) when is_port(ListenSocket) -> 
     EmulatedOptions = emulated_options(),
    {ok, Inetvalues} = inet:getopts(ListenSocket, EmulatedOptions),
    inet:setopts(ListenSocket, internal_inet_values()), 
    try handle_options(Inetvalues ++ SslOptions) of
	{ok, Options} ->
	    {{_, _, _} = CbInfo, {SSlOpts, EmOpts, _}} =
		handle_transport_protocol(Options),
	    {ok, Port} = inet:port(ListenSocket),
	    ssl_connection:accept(Port, ListenSocket, 
				  {SSlOpts, EmOpts}, self(), CbInfo, Timeout)
    catch 
	{error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: close() -> ok 
%%
%% Description: Close a ssl connection
%%--------------------------------------------------------------------  
close(#sslsocket{pid = {ListenSocket, {_,_, Options}}, fd = new_ssl}) ->
    {CbModule, _, _} = proplists:get_value(cb_info, Options),
    CbModule:close(ListenSocket);
close(#sslsocket{pid = Pid, fd = new_ssl}) ->
    ssl_connection:close(Pid);
close(Socket = #sslsocket{}) ->
    ensure_old_ssl_started(),
    ssl_broker:close(Socket).

%%--------------------------------------------------------------------
%% Function:  send(Socket, Data) -> ok
%% 
%% Description: Sends data over the ssl connection
%%--------------------------------------------------------------------
send(#sslsocket{pid = Pid, fd = new_ssl}, Data) ->
    ssl_connection:send(Pid, Data);

send(#sslsocket{} = Socket, Data) -> 
    ensure_old_ssl_started(),
    ssl_broker:send(Socket, Data).

%%--------------------------------------------------------------------
%% Function: recv(Socket, Length [,Timeout]) -> {ok, Data} | {error, reason}
%%
%% Description: Receives data when active = false
%%--------------------------------------------------------------------
recv(Socket, Length) ->
    recv(Socket, Length, infinity).
recv(#sslsocket{pid = Pid, fd = new_ssl}, Length, Timeout) ->
    ssl_connection:recv(Pid, Length, Timeout);

recv(Socket = #sslsocket{}, Length, Timeout) ->
    ensure_old_ssl_started(),
    ssl_broker:recv(Socket, Length, Timeout).

%%--------------------------------------------------------------------
%% Function: controlling_process(Socket, NewOwner) -> ok | {error, Reason}
%%
%% Description: Changes process that receives the messages when active = true
%% or once. 
%%--------------------------------------------------------------------
controlling_process(#sslsocket{pid = Pid, fd = new_ssl}, NewOwner) 
  when is_pid(Pid) ->
    ssl_connection:new_user(Pid, NewOwner);

controlling_process(Socket, NewOwner) when is_pid(NewOwner) ->
    ensure_old_ssl_started(),
    ssl_broker:controlling_process(Socket, NewOwner).

%%--------------------------------------------------------------------
%% Function: connection_info(Socket) -> {ok, {Protocol, CipherSuite}} | 
%%                                      {error, Reason}
%% Protocol = sslv3 | tlsv1 | tlsv1.1
%% CipherSuite = {KeyExchange, Chipher, Hash, Exportable}
%% 
%%
%% Description: Returns ssl protocol and cipher used for the connection
%%--------------------------------------------------------------------
connection_info(#sslsocket{pid = Pid, fd = new_ssl}) ->
    ssl_connection:info(Pid);

connection_info(#sslsocket{} = Socket) -> 
    ensure_old_ssl_started(),
    ssl_broker:connection_info(Socket).

%%--------------------------------------------------------------------
%% Function: peercert(Socket[, Opts]) -> {ok, Cert} | {error, Reason}
%%
%% Description: 
%%--------------------------------------------------------------------
peercert(Socket) ->
    peercert(Socket, []).

peercert(#sslsocket{pid = Pid, fd = new_ssl}, Opts) ->
    case ssl_connection:peer_certificate(Pid) of
	{ok, undefined} ->
	    {error, no_peercert};
        {ok, BinCert} ->
	    PKOpts = [case Opt of ssl -> otp; pkix -> plain end || 
			 Opt <- Opts, Opt =:= ssl orelse Opt =:= pkix],
	    case PKOpts of
		[Opt] ->
		    public_key:pkix_decode_cert(BinCert, Opt);
		[] ->
		    {ok, BinCert}
	    end;
        {error, Reason}  ->
            {error, Reason}
    end;

peercert(#sslsocket{} = Socket, Opts) ->
    ensure_old_ssl_started(),
    case ssl_broker:peercert(Socket) of
        {ok, Bin} ->
            ssl_pkix:decode_cert(Bin, Opts);
        {error, Reason}  ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% Function: peername(Socket) -> {ok, {Address, Port}} | {error, Reason}
%%
%% Description:
%%--------------------------------------------------------------------
peername(#sslsocket{fd = new_ssl, pid = Pid}) ->
    ssl_connection:peername(Pid);

peername(#sslsocket{} = Socket) ->
    ensure_old_ssl_started(),
    ssl_broker:peername(Socket).

%%--------------------------------------------------------------------
%% Function: cipher_suites() -> 
%%
%% Description:
%%--------------------------------------------------------------------
cipher_suites() ->
    cipher_suites(erlang).
  
cipher_suites(erlang) ->
    Version = ssl_record:highest_protocol_version([]),
    [ssl_cipher:suite_definition(S) || S <- ssl_cipher:suites(Version)];    

cipher_suites(openssl) ->
    Version = ssl_record:highest_protocol_version([]),
    [ssl_cipher:openssl_suite_name(S) || S <- ssl_cipher:suites(Version)].

%%--------------------------------------------------------------------
%% Function: getopts(Socket, OptTags) -> {ok, Options} | {error, Reason}
%% 
%% Description:
%%--------------------------------------------------------------------
getopts(#sslsocket{fd = new_ssl, pid = Pid}, OptTags) when is_pid(Pid) ->
    ssl_connection:get_opts(Pid, OptTags);
getopts(#sslsocket{fd = new_ssl, 
		   pid = {ListenSocket, {_, EmOpts, _}}}, OptTags) ->
    getopts(ListenSocket, EmOpts, OptTags, []);
getopts(#sslsocket{} = Socket, Options) ->
    ensure_old_ssl_started(),
    ssl_broker:getopts(Socket, Options).

%%--------------------------------------------------------------------
%% Function: setopts(Socket, Options) -> ok | {error, Reason}
%% 
%% Description:
%%--------------------------------------------------------------------
setopts(#sslsocket{fd = new_ssl, pid = Pid}, Options) ->
    ssl_connection:set_opts(Pid, Options);
setopts(#sslsocket{} = Socket, Options) ->
    ensure_old_ssl_started(),
    ssl_broker:setopts(Socket, Options).

%%---------------------------------------------------------------
%% Function: shutdown(Socket, How) -> ok | {error, Reason}
%% 
%% Description: Same as gen_tcp:shutdown/2
%%--------------------------------------------------------------------
shutdown(#sslsocket{pid = {ListenSocket, Opts}, fd = new_ssl}, How) ->
    {{CbModule, _,_}, _} = handle_transport_protocol(Opts),
    CbModule:shutdown(ListenSocket, How);
shutdown(#sslsocket{pid = Pid, fd = new_ssl}, How) ->
    ssl_connection:shutdown(Pid, How).

%%--------------------------------------------------------------------
%% Function: sockname(Socket) -> {ok, {Address, Port}} | {error, Reason}
%% 
%% Description: Same as inet:sockname/1
%%--------------------------------------------------------------------
sockname(#sslsocket{fd = new_ssl, pid = {ListenSocket, _}}) ->
    inet:sockname(ListenSocket);

sockname(#sslsocket{fd = new_ssl, pid = Pid}) ->
    ssl_connection:sockname(Pid);

sockname(#sslsocket{} = Socket) ->
    ensure_old_ssl_started(),
    ssl_broker:sockname(Socket).

%%---------------------------------------------------------------
%% Function: seed(Data) -> ok | {error, edata}
%% 
%% Description:
%%--------------------------------------------------------------------
%% TODO: crypto:seed ?
seed(Data) ->
    ensure_old_ssl_started(),
    ssl_server:seed(Data).

%%---------------------------------------------------------------
%% Function: session_id(Socket) -> {ok, PropList} | {error, Reason} 
%% 
%% Description:
%%--------------------------------------------------------------------
session_info(#sslsocket{pid = Pid, fd = new_ssl}) ->
    ssl_connection:session_info(Pid).

%%---------------------------------------------------------------
%% Function: versions() -> [{SslAppVer, SupportedSslVer, AvailableSslVsn}]
%% 
%% SslAppVer = string()  - t.ex: ssl-4.0
%% SupportedSslVer = [SslVer]
%% AvailableSslVsn = [SSLVer]
%% SSLVer = sslv3 | tlsv1  | 'tlsv1.1'
%%
%% Description: Returns a list of relevant versions.
%%--------------------------------------------------------------------
versions() ->
    Vsns = ssl_record:supported_protocol_versions(),
    SupportedVsns =  lists:map(fun(Version) -> 
 				       ssl_record:protocol_version(Version)
 			       end, Vsns),
    AvailableVsns = ?DEFAULT_SUPPORTED_VERSIONS,
    [{ssl_app, ?VSN}, {supported, SupportedVsns}, 
     {available, AvailableVsns}].

%%%--------------------------------------------------------------
%%% Internal functions
%%%--------------------------------------------------------------------
new_connect(Address, Port, Options, Timeout) when is_list(Options) ->
    try handle_options(Options) of
	{ok, NewOptions} ->
            do_new_connect(Address, Port, NewOptions, Timeout)
    catch 
         {error, Reason} ->
            {error, Reason}
    end.

do_new_connect(Address, Port, Options0, Timeout) ->
    {{CbModule, _, _} = CbInfo, Options} =
	handle_transport_protocol(Options0),
    try CbModule:connect(Address, Port, 
			 connect_options(Options), Timeout) of
	{ok, Socket} ->
	    ssl_connection:connect(Address, Port, Socket, 
				   ssl_connection_options(Options), self(), 
				   CbInfo, Timeout);
	{error, Reason} ->
	    {error, Reason}
    catch
	exit:{function_clause, _} ->
	    {error, {}};
	  exit:{badarg, _} ->
	    {error,{eoptions, {inet_options, connect_options(Options)}}}

    end.

handle_transport_protocol({SSL, Emulated, Options0}) ->
    CbInfo = 
	proplists:get_value(cb_info, Options0, {gen_tcp, tcp, tcp_closed}),
    Options = proplists:delete(cb_info, Options0),
    {CbInfo, {SSL, Emulated, Options}}.

old_connect(Address, Port, Options, Timeout) ->
    ensure_old_ssl_started(),
    {ok, Pid} = ssl_broker:start_broker(connector),
    ssl_broker:connect(Pid, Address, Port, Options, Timeout).

new_listen(Port, Options0) ->
    try handle_options(Options0) of
	{ok, Options1} ->
	    {CbInfo, Options} =
		handle_transport_protocol(Options1),
	    new_listen(CbInfo, Port, Options)
    catch 
	{error, Reason} ->
	    {error, Reason}
    end.

new_listen({CbModule, _, _} = CbInfo, Port, Options) ->
    case CbModule:listen(Port, listen_options(Options)) of
	{ok, ListenSocket} ->
	    {SslOpts, EmOpts} = ssl_listen_options(Options),
	    {ok, #sslsocket{pid = {ListenSocket, {SslOpts, EmOpts, 
						  [{cb_info, CbInfo}]}},
				   fd = new_ssl}};
	{error, Reason} ->
	    {error, Reason} 
    end.
	    
old_listen(Port, Options) ->
    ensure_old_ssl_started(),
    {ok, Pid} = ssl_broker:start_broker(listener),
    ssl_broker:listen(Pid, Port, Options).

handle_options(Opts0) ->
    Opts = proplists:expand([{binary, [{mode, binary}]},
			     {list, [{mode, list}]}], Opts0),
    
    ReuseSessionFun = fun(_, _, _, _) ->
			      true
		      end,

    VerifyFun =  fun(_) ->
			 false
		 end,


    {Verify, FailIfNoPeerCert} = 
	%% Handle 0, 1, 2 for backwards compatibility
	case proplists:get_value(verify, Opts, verify_none) of
	    0 ->
		{verify_none, false};
	    1  ->
		{verify_peer, false};
	    2 ->
		{verify_peer, true};
	    verify_none ->
		{verify_none, false};
	    verify_peer ->
		{verify_peer, proplists:get_value(fail_if_no_peer_cert,
						  Opts, false)};
	    Value ->
		throw({error, {eoptions, {verify, Value}}})
	end,   

    SSLOptions = #ssl_options{
      versions   = handle_option(versions, Opts, []),
      verify     = validate_option(verify, Verify),
      verify_fun = handle_option(verify_fun, Opts, VerifyFun),
      fail_if_no_peer_cert = validate_option(fail_if_no_peer_cert, 
					     FailIfNoPeerCert),
      verify_client_once =  handle_option(verify_client_once, Opts, false),
      depth      = handle_option(depth,  Opts, 1),
      certfile   = handle_option(certfile, Opts, ""),
      keyfile    = handle_option(keyfile,  Opts, ""),
      key        = handle_option(key, Opts, undefined),
      password   = handle_option(password, Opts, ""),
      cacertfile = handle_option(cacertfile, Opts, ""),
      ciphers    = handle_option(ciphers, Opts, []),
      %% Server side option
      reuse_session = handle_option(reuse_session, Opts, ReuseSessionFun),
      reuse_sessions = handle_option(reuse_sessions, Opts, true),
      debug      = handle_option(debug, Opts, [])
     },
    EmulatedSockOpts = #socket_options{
      mode    = handle_option(mode, Opts, list),
      packet  = handle_option(packet, Opts, 0),
      header  = handle_option(header, Opts, 0),
      active  = handle_option(active, Opts, true)
     },
    
    SslOREmulated = [versions, verify, verify_fun, 
		     depth, certfile, keyfile,
		     key, password, cacertfile, ciphers,
		     debug, mode, packet, header, active,
		     reuse_session, reuse_sessions, ssl_imp],

    SockOpts = lists:foldl(fun(Key, PropList) -> 
				    proplists:delete(Key, PropList)
			    end, Opts, SslOREmulated),
    
    {ok, {SSLOptions, EmulatedSockOpts, SockOpts}}.

handle_option(OptionName, Opts, Default) ->
    validate_option(OptionName, 
		    proplists:get_value(OptionName, Opts, Default)).


validate_option(versions, Versions)  ->
    validate_versions(Versions, Versions);
validate_option(ssl_imp, Value) when Value == new; Value == old ->
    Value;
validate_option(verify, Value) 
  when Value == verify_none; Value == verify_peer ->
    Value;
validate_option(verify_fun, Value) when is_function(Value) ->
   Value;
validate_option(fail_if_no_peer_cert, Value) 
  when Value == true; Value == false ->
    Value;
validate_option(verify_client_once, Value) 
  when Value == true; Value == false ->
    Value;
validate_option(depth, Value) when is_integer(Value), 
                                   Value >= 0, Value =< 255->
    Value;
validate_option(certfile, Value) when is_list(Value) ->
    Value;
validate_option(keyfile, Value) when is_list(Value) ->
    Value;
validate_option(key, Value) when Value == undefined;
                                 is_tuple(Value) ->
    %% element(1, Value)=='RSAPrivateKey' ->
    Value;
validate_option(password, Value) when is_list(Value) ->
    Value;
validate_option(cacertfile, Value) when is_list(Value), Value =/= "" ->
    Value;
validate_option(ciphers, Value)  when is_list(Value) ->
    Version = ssl_record:highest_protocol_version([]),
    try cipher_suites(Version, Value) of
	Ciphers ->
	    Ciphers
    catch
	exit:_ ->
	    throw({error, {eoptions, {ciphers, Value}}})
    end;

validate_option(reuse_session, Value) when is_function(Value) ->
    Value;

validate_option(reuse_sessions, Value) when Value == true; 
					    Value == false ->
    Value;
validate_option(mode, Value) when Value == list;
                                  Value == binary ->
    Value;

validate_option(packet, Value) when Value == raw;
                                    Value == 0;
				    Value == 1;
				    Value == 2;
				    Value == 4;
				    Value == asn1;
				    Value == fcgi;
				    Value == sunrm;
				    Value == http;
				    Value == httph;
				    Value == cdr;
				    Value == tpkt;
				    Value == line  ->
    Value;
validate_option(header, Value)  when is_integer(Value) ->
    Value;
validate_option(active, Value) when Value == true;
                                    Value == false;
                                    Value == once ->
    
    Value;
validate_option(debug, Value) when is_list(Value); Value == true ->
    Value;
validate_option(Opt, Value) ->
    throw({error, {eoptions, {Opt, Value}}}).
    
validate_versions([], Versions) ->
    Versions;
validate_versions([Version | Rest], Versions) when Version == 'tlsv1.1'; 
                                                   Version == tlsv1; 
                                                   Version == sslv3 ->
    validate_versions(Rest, Versions);					   
validate_versions(Ver, Versions) ->
    throw({error, {eoptions, {Ver, {versions, Versions}}}}).


listen_options({_, _, InetOpts}) ->
    %% Packet, mode, active and header must be  
    %% emulated. 
    internal_inet_values() ++ InetOpts.

ssl_listen_options({SslOpts, SocketOpts, _}) ->
    {SslOpts, SocketOpts}.

connect_options({_, _, InetOpts}) ->
    %% Packet, mode, active and header must be  
    %% emulated. 
    internal_inet_values() ++ InetOpts.

emulated_options() ->
    [mode, packet, active, header].

internal_inet_values() ->
    [{packet, 0},{header, 0},{active, false},{mode,binary}].
    %%[{packet, ssl},{header, 0},{active, false},{mode,binary}].

ssl_connection_options({SslOpts, SocketOpts, _}) ->
    {SslOpts, SocketOpts}.

cipher_suites(Version, []) ->
    ssl_cipher:suites(Version);
cipher_suites(Version, [{_,_,_,_}| _] = Ciphers0) ->
    Ciphers = lists:map(fun ssl_cipher:suite/1, Ciphers0),
    cipher_suites(Version, Ciphers);
cipher_suites(Version, [Cipher0 | _] = Ciphers0) when is_binary(Cipher0) ->
    Supported = ssl_cipher:suites(Version),
    case [Cipher || Cipher <- Ciphers0, lists:member(Cipher, Supported)] of
	[] ->
	    Supported;
	Ciphers ->
	    Ciphers
    end;
cipher_suites(Version, [Head | _] = Ciphers0) when is_list(Head) ->
    %% Format: ["RC4-SHA","RC4-MD5"]
     Ciphers = lists:map(fun ssl_cipher:openssl_suite/1, Ciphers0),
    cipher_suites(Version, Ciphers); 
cipher_suites(Version, Ciphers0)  ->
    %% Format: "RC4-SHA:RC4-MD5"
    Ciphers = lists:map(fun ssl_cipher:openssl_suite/1,
			string:tokens(Ciphers0, ":")),
    cipher_suites(Version, Ciphers).

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
    "No proxy socket found (internal error or max number of file "
        "descriptors exceeded).";
format_error(enooptions) ->
    "List of options is empty.";
format_error(enotstarted) ->
    "The SSL application has not been started.";
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
format_error({badcall, _Call}) ->
    "Call not recognized for current mode (active or passive) and state "
        "of socket.";
format_error({badcast, _Cast}) ->
    "Call not recognized for current mode (active or passive) and state "
        "of socket."; 

format_error({badinfo, _Info}) ->
    "Call not recognized for current mode (active or passive) and state "
        "of socket.";
format_error(Error) ->
    case (catch inet:format_error(Error)) of
        "unkknown POSIX" ++ _ ->
            no_format(Error);
        {'EXIT', _} ->
            no_format(Error);
        Other ->
            Other
    end.

no_format(Error) ->    
    io_lib:format("No format string for error: \"~p\" available.", [Error]).

getopts(_, _, [], Acc) ->
    {ok, Acc};
getopts(Socket, Options, [mode | Rest], Acc) ->
    Value = {mode, Options#socket_options.mode},
    getopts(Socket, Options, Rest, [Value | Acc]);
getopts(Socket, Options, [packet | Rest], Acc) ->
    Value =  {packet, Options#socket_options.packet},
    getopts(Socket, Options, Rest, [Value | Acc]);
getopts(Socket, Options, [header | Rest], Acc) ->
    Value = {header, Options#socket_options.header},
    getopts(Socket, Options, Rest, [Value | Acc]);
getopts(Socket, Options, [active | Rest], Acc) ->
    Value = {active, Options#socket_options.active},
    getopts(Socket, Options, Rest, [Value | Acc]);
getopts(Socket, Options, [Tag | Rest], Acc) ->
    {ok, [Value]} = inet:getopts(Socket, Tag),
    getopts(Socket, Options, Rest, [Value | Acc]).

%% Start old ssl port program if needed.
ensure_old_ssl_started() ->
    case whereis(ssl_server) of
	undefined ->
	    (catch supervisor:start_child(ssl_sup, 
				   {ssl_server, {ssl_server, start_link, []},
				    permanent, 2000, worker, [ssl_server]}));
	_ ->
	    ok
    end.

%%%%%%%%%%%%%%%% Deprecated %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ciphers() -> 
    ensure_old_ssl_started(),
    case (catch ssl_server:ciphers()) of
        {'EXIT', _} ->
            {error, enotstarted};
        Res = {ok, _}  ->
            Res
    end.

version() -> 
    ensure_old_ssl_started(),
    SSLVsn = ?VSN,
    {CompVsn, LibVsn} = case (catch ssl_server:version()) of
                            {'EXIT', _} ->
                                {"", ""};
                            {ok, Vsns}  ->
                                Vsns
                        end,
    {ok, {SSLVsn, CompVsn, LibVsn}}.
                                
port(Socket) when record(Socket, sslsocket) ->
    case sockname(Socket) of
	{ok, {_, Port}} ->
	    {ok, Port};
	{error, Reason} ->
	    {error, Reason}
     end.

accept(#sslsocket{} = SSlListenSocket) ->
    accept(SSlListenSocket, infinity);
accept(ListenSocket) when is_port(ListenSocket) ->
    accept(ListenSocket, infinity).

accept(#sslsocket{pid = {ListenSocket, Options}, fd = new_ssl} = 
       SslListenSocket, Timeout) ->
    accept(SslListenSocket#sslsocket{pid = ListenSocket}, Options, Timeout);
accept(#sslsocket{} = ListenSocket, Timeout) ->
    accept(ListenSocket, [], Timeout);
accept(ListenSocket, Options) when is_port(ListenSocket), is_list(Options) ->
    accept(ListenSocket, Options, infinity).

accept(#sslsocket{pid = ListenSocket, fd = new_ssl}, Options0, Timeout) ->
    {{CbModule, _, _} = CbInfo, {SSlOpts, EmOpts, _}} =
	handle_transport_protocol(Options0),
    {ok, Port} = inet:port(ListenSocket),
    case CbModule:accept(ListenSocket, Timeout) of
	{ok, Socket} ->
	    ssl_connection:accept(Port, Socket, {SSlOpts, EmOpts}, self(), 
				  CbInfo, Timeout);
	Error ->
	    Error
    end;

%% Old accept
accept(#sslsocket{} = ListenSocket, _, Timeout) ->
    case transport_accept(ListenSocket, Timeout) of
        {ok, NewSocket} ->
            case ssl_accept(NewSocket, Timeout) of
                ok ->
                    {ok, NewSocket};
                Error ->
                    Error
            end;
        Error ->
            Error
    end;

accept(ListenSocket, SslOptions, Timeout) when is_port(ListenSocket) ->
    ssl_accept(ListenSocket, SslOptions, Timeout).

%% Only used to remove exit messages from old ssl
%% First is a nonsense clause to provide some
%% backward compability for orber that uses this
%% function in a none recommended way, but will
%% work correctly if a valid pid is returned.
pid(#sslsocket{fd = new_ssl}) ->
    whereis(ssl_connection_sup);
pid(#sslsocket{} = Socket) ->
    Socket#sslsocket.pid.
