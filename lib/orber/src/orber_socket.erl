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
%% Author: Lars Thorsen
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
-include_lib("orber/src/orber_debug.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/0, connect/4, listen/3, accept/2, write/3,
	 controlling_process/3, close/2, peername/2]).

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
%% Connect to IIOP Port at Host in CDR mode, in order to 
%% establish a connection.
%%
connect(Type, Host, Port, Options) ->
    Timeout = orber:iiop_setup_connection_timeout(),
    case orber:iiop_out_ports() of
	{Min, Max} ->
	    multi_connect(Min, Max, Type, Host, Port, 
			  [binary, {packet,cdr}| Options], Timeout);
	_ ->
	    connect(Type, Host, Port, [binary, {packet,cdr}| Options], Timeout)
    end.

connect(normal, Host, Port, Options, Timeout) ->
    case catch gen_tcp:connect(Host, Port, Options, Timeout) of
	{ok, Socket} ->
	    Socket;
	{error, timeout} ->
	    orber:debug_level_print("[~p] orber_socket:connect(normal, ~p, ~p, ~p);
Timeout after ~p msec.", [?LINE, Host, Port, Options, Timeout], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{minor=(?ORBER_VMCID bor 4),
					completion_status=?COMPLETED_NO});
	Error ->
	    orber:debug_level_print("[~p] orber_socket:connect(normal, ~p, ~p, ~p);
Failed with reason: ~p", [?LINE, Host, Port, Options, Error], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO})
    end;
connect(ssl, Host, Port, Options, Timeout) ->
    case catch ssl:connect(Host, Port, Options, Timeout) of
	{ok, Socket} ->
	    Socket;
	{error, timeout} ->
	    orber:debug_level_print("[~p] orber_socket:connect(ssl, ~p, ~p, ~p); 
Timeout after ~p msec.", [?LINE, Host, Port, Options, Timeout], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{minor=(?ORBER_VMCID bor 4), 
					completion_status=?COMPLETED_NO});
	Error ->
	    orber:debug_level_print("[~p] orber_socket:connect(ssl, ~p, ~p, ~p); 
Failed with reason: ~p", [?LINE, Host, Port, Options, Error], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO})
    end.

multi_connect(CurrentPort, Max, Type, Host, Port, Options, _) when CurrentPort > Max ->
    orber:debug_level_print("[~p] orber_socket:multi_connect(~p, ~p, ~p, ~p); 
Unable to use any of the sockets defined by 'iiop_out_ports'.
Either all ports are in use or to many connections already exists.", 
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
listen(normal, Port, Options) ->
    Options1 = case orber:ip_address_variable_defined() of
		   true ->
		       {ok, IP} = inet:getaddr(orber:host(),inet),
		       [{ip, IP} | Options];
		   false ->
		       Options
	       end,
    case catch gen_tcp:listen(Port, [binary, {packet,cdr}, {reuseaddr,true} |
				     Options1]) of
	{ok, ListenSocket} ->
	    ListenSocket;
	{error, eaddrinuse} ->	
	    orber:debug_level_print("[~p] orber_socket:listen(normal, ~p, ~p);
Looks like the listen port is already in use. Check if another Orber is started
on the same node and uses the same listen port (iiop_port). But it may also
be used by any other application; confirm with 'netstat'.",
[?LINE, Port, Options1], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO});
	Error ->
	    orber:debug_level_print("[~p] orber_socket:listen(normal, ~p, ~p); 
Failed with reason: ~p", [?LINE, Port, Options1, Error], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO})
    end;
listen(ssl, Port, Options) ->
    Options1 = case orber:ip_address_variable_defined() of
		   true ->
		       {ok, IP} = inet:getaddr(orber:host(),inet),
		       [{ip, IP} | Options];
		   false ->
		       Options
	       end,
    case catch ssl:listen(Port, [binary, {packet,cdr} | Options1]) of
	{ok, ListenSocket} ->
	    ListenSocket;
	{error, eaddrinuse} ->	
	    orber:debug_level_print("[~p] orber_socket:listen(ssl, ~p, ~p);
Looks like the listen port is already in use. Check if another Orber is started
on the same node and uses the same listen port (iiop_port). But it may also
be used by any other application; confirm with 'netstat'.",
[?LINE, Port, Options1], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO});
	Error ->
	    orber:debug_level_print("[~p] orber_socket:listen(ssl, ~p, ~p);
Failed with reason: ~p", [?LINE, Port, Options1, Error], ?DEBUG_LEVEL),
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
	    orber:debug_level_print("[~p] orber_socket:accept(normal, ~p); 
Failed with reason: ~p", [?LINE, ListenSocket, Error], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{completion_status=?COMPLETED_NO})
    end;
accept(ssl, ListenSocket) ->
    case catch ssl:accept(ListenSocket) of
	{ok, S} ->
	    S;
	Error ->
	    orber:debug_level_print("[~p] orber_socket:accept(ssl, ~p);
Failed with reason: ~p", [?LINE, ListenSocket, Error], ?DEBUG_LEVEL),
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
    {ok, {{N1,N2,N3,N4}, Port}} = inet:peername(Socket),
    {lists:concat([N1, ".", N2, ".", N3, ".", N4]), Port};
peername(ssl, Socket) ->
    {ok, {{N1,N2,N3,N4}, Port}} = ssl:peername(Socket),
    {lists:concat([N1, ".", N2, ".", N3, ".", N4]), Port}.
