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
	 controlling_process/3, close/2]).

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
connect(normal, Host, Port, Options) ->
    Timeout = orber:iiop_setup_connection_timeout(),
    case catch gen_tcp:connect(Host, Port, [binary, {packet,cdr}| Options], Timeout) of
	{ok, Socket} ->
	    Socket;
%	{error, etimedout} ->
%	    orber:debug_level_print("[~p] orber_socket:connect(normal, ~p, ~p, ~p); Timeout after ~p msec.", 
%				    [?LINE, Host, Port, Options, Timeout], ?DEBUG_LEVEL),
%	    corba:raise(#'TIMEOUT'{minor=100, completion_status=?COMPLETED_NO});
	{error, timeout} ->
	    orber:debug_level_print("[~p] orber_socket:connect(normal, ~p, ~p, ~p); Timeout after ~p msec.", 
				    [?LINE, Host, Port, Options, Timeout], ?DEBUG_LEVEL),
	    corba:raise(#'TIMEOUT'{minor=101, completion_status=?COMPLETED_NO});
	Error ->
	    orber:debug_level_print("[~p] orber_socket:connect(normal, ~p, ~p, ~p); Failed with reason: ~p", 
				    [?LINE, Host, Port, Options, Error], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{minor=100, completion_status=?COMPLETED_NO})
    end;
connect(ssl, Host, Port, Options) ->
    Timeout = orber:iiop_setup_connection_timeout(),
    case catch ssl:connect(Host, Port, [binary, {packet,cdr}| Options], Timeout) of
	{ok, Socket} ->
	    Socket;
%	{error, etimedout} ->
%	    orber:debug_level_print("[~p] orber_socket:connect(ssl, ~p, ~p, ~p); Timeout after ~p msec.", 
%				    [?LINE, Host, Port, Options, Timeout], ?DEBUG_LEVEL),
%	    corba:raise(#'TIMEOUT'{minor=102, completion_status=?COMPLETED_NO});
	{error, timeout} ->
	    orber:debug_level_print("[~p] orber_socket:connect(ssl, ~p, ~p, ~p); Timeout after ~p msec.", 
				    [?LINE, Host, Port, Options, Timeout], ?DEBUG_LEVEL),
	    corba:raise(#'TIMEOUT'{minor=103, completion_status=?COMPLETED_NO});
	Error ->
	    orber:debug_level_print("[~p] orber_socket:connect(ssl, ~p, ~p, ~p); Failed with reason: ~p", 
				    [?LINE, Host, Port, Options, Error], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{minor=110, completion_status=?COMPLETED_NO})
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
	Error ->
	    orber:debug_level_print("[~p] orber_socket:listen(normal, ~p, ~p); Failed with reason: ~p",
				    [?LINE, Port, Options, Error], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{minor=101, completion_status=?COMPLETED_NO})
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
	Error ->
	    orber:debug_level_print("[~p] orber_socket:listen(ssl, ~p, ~p); Failed with reason: ~p",
				    [?LINE, Port, Options, Error], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{minor=111, completion_status=?COMPLETED_NO})
    end.

%%-----------------------------------------------------------------
%% Wait in accept on the socket
%% 
accept(normal, ListenSocket) ->
    case catch gen_tcp:accept(ListenSocket) of
	{ok, S} ->
	    S;
	Error ->
	    orber:debug_level_print("[~p] orber_socket:accept(normal, ~p); Failed with reason: ~p",
				    [?LINE, ListenSocket, Error], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{minor=102, completion_status=?COMPLETED_NO})
    end;
accept(ssl, ListenSocket) ->
    case catch ssl:accept(ListenSocket) of
	{ok, S} ->
	    S;
	Error ->
	    orber:debug_level_print("[~p] orber_socket:accept(ssl, ~p); Failed with reason: ~p",
				    [?LINE, ListenSocket, Error], ?DEBUG_LEVEL),
	    corba:raise(#'COMM_FAILURE'{minor=112, completion_status=?COMPLETED_NO})
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

