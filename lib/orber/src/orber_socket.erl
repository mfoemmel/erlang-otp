%%--------------------------------------------------------------------
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
%%-----------------------------------------------------------------
%% File: orber_socket.erl
%% 
%% Description:
%%    This file contains a standard interface to the sockets to handle the differences
%%    between the implementations used.
%%
%% Creation date: 970115
%%
%%-----------------------------------------------------------------
-module(orber_socket).

-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").


%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/0, connect/4, listen/3, accept/2, write/3,
	 controlling_process/3, close/2, peername/2, sockname/2, 
	 peerdata/2, sockdata/2, setopts/3, clear/2, shutdown/3]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([]).

%%-----------------------------------------------------------------
%% Internal defines
%%-----------------------------------------------------------------
-define(DEBUG_LEVEL, 6).

%%-----------------------------------------------------------------
%% External functions
%%-----------------------------------------------------------------
start() ->	
    inet_db:start().

%%-----------------------------------------------------------------
%% Invoke the required setopts (i.e., inet or ssl)
setopts(normal, Socket, Opts) ->
    inet:setopts(Socket, Opts);
setopts(ssl, Socket, Opts) ->
    ssl:setopts(Socket, Opts).

%%-----------------------------------------------------------------
%% Connect to IIOP Port at Host in CDR mode, in order to 
%% establish a connection.
%%
connect(Type, Host, Port, Options) ->
    Timeout = orber:iiop_setup_connection_timeout(),
    Options1 = check_options(Type, Options),
    case orber:iiop_out_ports() of
	{Min, Max} ->
	    multi_connect(Min, Max, Type, Host, Port, 
			  [binary, {packet,cdr}| Options1], Timeout);
	_ ->
	    connect(Type, Host, Port, [binary, {packet,cdr}| Options1], Timeout)
    end.

connect(normal, Host, Port, Options, Timeout) ->
    case catch gen_tcp:connect(Host, Port, Options, Timeout) of
	{ok, Socket} ->
	    Socket;
	{error, timeout} ->
	    orber:dbg("[~p] orber_socket:connect(normal, ~p, ~p, ~p);~n"
		      "Timeout after ~p msec.", 
		      [?LINE, Host, Port, Options, Timeout], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{minor=(?ORBER_VMCID bor 4),
					completion_status=?COMPLETED_NO});
	Error ->
	    orber:dbg("[~p] orber_socket:connect(normal, ~p, ~p, ~p);~n"
		      "Failed with reason: ~p", 
		      [?LINE, Host, Port, Options, Error], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO})
    end;
connect(ssl, Host, Port, Options, Timeout) ->
    case catch ssl:connect(Host, Port, Options, Timeout) of
	{ok, Socket} ->
	    Socket;
	{error, timeout} ->
	    orber:dbg("[~p] orber_socket:connect(ssl, ~p, ~p, ~p);~n"
		      "Timeout after ~p msec.", 
		      [?LINE, Host, Port, Options, Timeout], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{minor=(?ORBER_VMCID bor 4), 
					completion_status=?COMPLETED_NO});
	Error ->
	    orber:dbg("[~p] orber_socket:connect(ssl, ~p, ~p, ~p);~n"
		      "Failed with reason: ~p", 
		      [?LINE, Host, Port, Options, Error], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO})
    end.

multi_connect(CurrentPort, Max, Type, Host, Port, Options, _) when CurrentPort > Max ->
    orber:dbg("[~p] orber_socket:multi_connect(~p, ~p, ~p, ~p);~n"
	      "Unable to use any of the sockets defined by 'iiop_out_ports'.~n"
	      "Either all ports are in use or to many connections already exists.", 
			    [?LINE, Type, Host, Port, Options], ?DEBUG_LEVEL),
    corba:raise(#'IMP_LIMIT'{minor=(?ORBER_VMCID bor 1), completion_status=?COMPLETED_NO});
multi_connect(CurrentPort, Max, normal, Host, Port, Options, Timeout) ->
    case catch gen_tcp:connect(Host, Port, [{port, CurrentPort}|Options], Timeout) of
	{ok, Socket} ->
	    Socket;
	_ ->
	    multi_connect(CurrentPort+1, Max, normal, Host, Port, Options, Timeout)
    end;
multi_connect(CurrentPort, Max, ssl, Host, Port, Options, Timeout) ->
     case catch ssl:connect(Host, Port, [{port, CurrentPort}|Options], Timeout) of
	{ok, Socket} ->
	    Socket;
	_ ->
	     multi_connect(CurrentPort+1, Max, ssl, Host, Port, Options, Timeout)
    end.
  


%%-----------------------------------------------------------------
%% Create a listen socket at Port in CDR mode for 
%% data connection.
%%
listen(normal, Port, Options0) ->
    Options = check_options(normal, Options0),
    Backlog = orber:iiop_backlog(),
    Options1 = case orber:ip_address_variable_defined() of
		   false ->
		       Options;
		   Host ->
		       IPVersion = orber:ip_version(),
		       {ok, IP} = inet:getaddr(Host, IPVersion),
		       [{ip, IP} | Options]
	       end,
    Options2 = case orber:iiop_max_in_requests() of
		   infinity ->
		       Options1;
		   _MaxRequests ->
		       [{active, once}|Options1]
	       end,
    Options3 = case orber_env:iiop_packet_size() of
		   infinity ->
		       Options2;
		   MaxSize ->
		       [{packet_size, MaxSize}|Options1]
	       end,
    case catch gen_tcp:listen(Port, [binary, {packet,cdr},
				     {reuseaddr,true}, {backlog, Backlog} |
				     Options3]) of
	{ok, ListenSocket} ->
	    {ok, ListenSocket, check_port(Port, normal, ListenSocket)};
	{error, eaddrinuse} ->
	    AllOpts = [binary, {packet,cdr}, 
		       {reuseaddr,true} | Options3],
	    orber:dbg("[~p] orber_socket:listen(normal, ~p, ~p);~n"
		      "Looks like the listen port is already in use.~n"
		      "Check if another Orber is started~n"
		      "on the same node and uses the same listen port (iiop_port). But it may also~n"
		      "be used by any other application; confirm with 'netstat'.",
		      [?LINE, Port, AllOpts], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO});
	Error ->
	    AllOpts = [binary, {packet,cdr}, 
		       {reuseaddr,true} | Options3],
	    orber:dbg("[~p] orber_socket:listen(normal, ~p, ~p);~n"
		      "Failed with reason: ~p", 
		      [?LINE, Port, AllOpts, Error], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO})
    end;
listen(ssl, Port, Options0) ->
    Backlog = orber:iiop_ssl_backlog(),
    Options = check_options(ssl, Options0),
    Options1 = case orber:ip_address_variable_defined() of
		   false ->
		       Options;
		   Host ->
		       IPVersion = orber:ip_version(),
		       {ok, IP} = inet:getaddr(Host, IPVersion),
		       [{ip, IP} | Options]
	       end,
    Options2 = case orber:iiop_max_in_requests() of
		   infinity ->
		       Options1;
		   _MaxRequests ->
		       [{active, once}|Options1]
	       end,
    Options3 = case orber_env:iiop_packet_size() of
		   infinity ->
		       Options2;
		   MaxSize ->
		       [{packet_size, MaxSize}|Options1]
	       end,
    case catch ssl:listen(Port, [binary, {packet,cdr}, 
				 {backlog, Backlog} | Options3]) of
	{ok, ListenSocket} ->
	    {ok, ListenSocket, check_port(Port, ssl, ListenSocket)};
	{error, eaddrinuse} ->	
	    AllOpts = [binary, {packet,cdr} | Options3],
	    orber:dbg("[~p] orber_socket:listen(ssl, ~p, ~p);~n"
		      "Looks like the listen port is already in use. Check if~n"
		      "another Orber is started on the same node and uses the~n"
		      "same listen port (iiop_port). But it may also~n"
		      "be used by any other application; confirm with 'netstat'.",
		      [?LINE, Port, AllOpts], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO});
	Error ->
	    AllOpts = [binary, {packet,cdr} | Options3],
	    orber:dbg("[~p] orber_socket:listen(ssl, ~p, ~p);~n"
		      "Failed with reason: ~p", 
		      [?LINE, Port, AllOpts, Error], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO})
    end.

%%-----------------------------------------------------------------
%% Wait in accept on the socket
%% 
accept(normal, ListenSocket) ->
    case catch gen_tcp:accept(ListenSocket) of
	{ok, S} ->
	    S;
	Error ->
	    orber:dbg("[~p] orber_socket:accept(normal, ~p);~n"
		      "Failed with reason: ~p", 
		      [?LINE, ListenSocket, Error], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO})
    end;
accept(ssl, ListenSocket) ->
    case catch ssl:accept(ListenSocket) of
	{ok, S} ->
	    S;
	Error ->
	    orber:dbg("[~p] orber_socket:accept(ssl, ~p);~n"
		      "Failed with reason: ~p", 
		      [?LINE, ListenSocket, Error], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO})
    end.

%%-----------------------------------------------------------------
%% Close the socket
%% 
close(normal, Socket) ->
    gen_tcp:close(Socket);
close(ssl, Socket) ->
    ssl:close(Socket).

%%-----------------------------------------------------------------
%% Write to socket
%% 
write(normal, Socket, Bytes) ->
    gen_tcp:send(Socket, Bytes);
write(ssl, Socket, Bytes) ->
    ssl:send(Socket, Bytes).

%%-----------------------------------------------------------------
%% Change the controlling process for the socket
%% 
controlling_process(normal, Socket, Pid) ->
    gen_tcp:controlling_process(Socket, Pid);
controlling_process(ssl, Socket, Pid) ->
    ssl:controlling_process(Socket, Pid).

%%-----------------------------------------------------------------
%% Get peername
%% 
peername(normal, Socket) ->
    inet:peername(Socket);
peername(ssl, Socket) ->
    ssl:peername(Socket).

%%-----------------------------------------------------------------
%% Get peerdata
%% 
peerdata(normal, Socket) ->
    create_data(inet:peername(Socket));
peerdata(ssl, Socket) ->
    create_data(ssl:peername(Socket)).

%%-----------------------------------------------------------------
%% Get sockname
%% 
sockname(normal, Socket) ->
    inet:sockname(Socket);
sockname(ssl, Socket) ->
    ssl:sockname(Socket).

%%-----------------------------------------------------------------
%% Get sockdata
%% 
sockdata(normal, Socket) ->
    create_data(inet:sockname(Socket));
sockdata(ssl, Socket) ->
    create_data(ssl:sockname(Socket)).


%% IPv4
create_data({ok, {{N1,N2,N3,N4}, Port}}) ->
    {lists:concat([N1, ".", N2, ".", N3, ".", N4]), Port};
%% IPv6 
create_data({ok, {{N1,N2,N3,N4,N5,N6,N7,N8}, Port}}) ->
    {lists:concat([N1, ":", N2, ":", N3, ":", N4, ":",
		   N5, ":", N6, ":", N7, ":", N8]), Port};
create_data(What) ->
    orber:dbg("[~p] orber_socket:peername() or orber_socket:sockname();~n"
	      "Failed with reason: ~p", [?LINE, What], ?DEBUG_LEVEL),
    {"Unable to lookup peername", 0}.


%%-----------------------------------------------------------------
%% Shutdown Connection
%% How = read | write | read_write
shutdown(normal, Socket, How) ->
    gen_tcp:shutdown(Socket, How);
shutdown(ssl, Socket, read_write) ->
    %% SSL do no support shutdown. For now we'll use this solution instead.
    close(ssl, Socket);
shutdown(ssl, _Socket, _How) ->
    {error, undefined}.

%%-----------------------------------------------------------------
%% Remove Messages from queue
%%
clear(normal, Socket) ->
    tcp_clear(Socket);
clear(ssl, Socket) ->
    ssl_clear(Socket).



%% Inet also checks for the following messages:
%%  * {S, {data, Data}}
%%  * {inet_async, S, Ref, Status}, 
%%  * {inet_reply, S, Status}
%% SSL doesn't.
tcp_clear(Socket) ->
    receive
        {tcp, Socket, _Data} ->
            tcp_clear(Socket);
        {tcp_closed, Socket} ->
            tcp_clear(Socket);
        {tcp_error, Socket, _Reason} ->
            tcp_clear(Socket)
    after 0 -> 
            ok
    end.

ssl_clear(Socket) ->
    receive
        {ssl, Socket, _Data} ->
            ssl_clear(Socket);
        {ssl_closed, Socket} ->
            ssl_clear(Socket);
        {ssl_error, Socket, _Reason} ->
            ssl_clear(Socket)
    after 0 -> 
            ok
    end.



%%-----------------------------------------------------------------
%% Check Port. If the user supplies 0 we pick any vacant port. But then
%% we must change the associated environment variable
check_port(0, normal, Socket) ->
    case inet:port(Socket) of
	{ok, Port} ->
	    orber:configure_override(iiop_port, Port),
	    Port;
	What ->
	    orber:dbg("[~p] orber_socket:check_port(~p);~n"
		      "Unable to extract the port number via inet:port/1~n",
		      [?LINE, What], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO})
    end;
check_port(0, ssl, Socket) ->
    case ssl:sockname(Socket) of
	{ok, {_Address, Port}} ->
	    orber:configure_override(iiop_ssl_port, Port),
	    Port;
	What ->
	    orber:dbg("[~p] orber_socket:check_port(~p);~n"
		      "Unable to extract the port number via ssl:sockname/1~n",
		      [?LINE, What], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO})
    end;
check_port(Port, _, _) ->
    Port.

%%-----------------------------------------------------------------
%% Check Options. 
%% We need this as a work-around since the SSL-app doesn't allow us
%% to pass 'inet' as an option. Also needed for R9B :-(
check_options(normal, Options) ->
    case orber:ip_version() of
	inet ->
	    Options;
	inet6 ->
	    %% Necessary for R9B. Should be [orber:ip_version()|Options];
	    [inet6|Options]
    end;
check_options(ssl, Options) ->
    case orber:ip_version() of
	inet ->
	    Options;
	inet6 ->
	    %% Will fail until SSL supports this option. 
	    %% Note, we want this happen!
	    [inet6|Options]
    end.

