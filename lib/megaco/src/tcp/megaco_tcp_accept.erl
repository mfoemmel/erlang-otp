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
%% 
%%     $Id$
%%
%%-----------------------------------------------------------------
%% Purpose: Waiting in accept for new connections.
%%-----------------------------------------------------------------
-module(megaco_tcp_accept).

%%-----------------------------------------------------------------
%% Include files
%%-----------------------------------------------------------------
-include_lib("megaco/src/tcp/megaco_tcp.hrl").
%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([
	 start_link/1
	]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([
	 net_accept/4
	]).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: start
%% Description: Starts the proces that makes the accept call.
%%-----------------------------------------------------------------
start_link({TcpRec, SupPid, Listen}) ->
    Pid = proc_lib:spawn_link(?MODULE, net_accept, [TcpRec, SupPid, Listen, self()]),
    {ok, Pid}.

%%-----------------------------------------------------------------
%% Internal Interface Functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: net_accept
%% Description: Loop function which calls accept and
%%              spawns a connection process when there is an initial
%%              contact.
%%-----------------------------------------------------------------
net_accept(TcpRec, SupPid, ListenFd, Parent) ->
    case gen_tcp:accept(ListenFd) of
	{ok, S} ->
	    ?tcp_debug(TcpRec, "tcp accept", []),
	    case megaco_tcp:start_connection(SupPid, 
					     TcpRec#megaco_tcp{socket=S}) of
		{ok, Pid} ->
		    gen_tcp:controlling_process(S, Pid);
		{error, Reason} ->
		    gen_tcp:close(S)	
	    end;
	{error, Reason} ->
	    ?tcp_debug(TcpRec, "tcp accept failed", [{error, Reason}])
    end,
    net_accept(TcpRec, SupPid, ListenFd, Parent).

