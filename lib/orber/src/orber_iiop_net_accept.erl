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
%% File: orber_iiop_net_accept.erl
%% Author: Lars Thorsen
%% 
%% Description:
%%    This file contains the process which are waiting in accept for new
%%    connections.
%%
%% Creation date: 990601
%%
%%-----------------------------------------------------------------
-module(orber_iiop_net_accept).

-include_lib("orber/src/orber_iiop.hrl").
-include_lib("orber/src/orber_debug.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/2]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([net_accept/3]).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: start/2
%%-----------------------------------------------------------------
start(Type, Listen) ->
    ?PRINTDEBUG("orber_iiop_net_accept: pre spawn_link"),
    Pid = proc_lib:spawn_link(?MODULE, net_accept, [Type, Listen, self()]),
    ?PRINTDEBUG("orber_iiop_net_accept: post spawn_link"),
    {ok, Pid}.

%%-----------------------------------------------------------------
%% Internal Functions
%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Func: net_accept/3
%%-----------------------------------------------------------------
net_accept(Type, ListenFd, Parent) ->
    ?PRINTDEBUG("orber_iiop_net_accept waiting in socket:accept"),
    S = ?IIOP_SOCKET_MOD:accept(Type, ListenFd),
    case orber_iiop_net:connect(Type, S) of
	{ok, Pid} ->
	    ?PRINTDEBUG2("changed controlling process to ~p",[Pid]),
	    ?IIOP_SOCKET_MOD:controlling_process(Type, S, Pid);
	_ ->
	    ?IIOP_SOCKET_MOD:close(Type, S)	
    end,
    net_accept(Type, ListenFd, Parent).

