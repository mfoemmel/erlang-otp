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
%% File: orber_iiop_insup.erl
%% Author: Lars Thorsen, Peter Lundell
%% 
%% Description:
%%    This file contains the IIOP communication supervisor which
%%    holds all active "in proxies"
%%
%% Creation date: 970115
%%
%%-----------------------------------------------------------------
-module(orber_iiop_insup).

-behaviour(supervisor).

-include_lib("orber/src/orber_debug.hrl").

%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/2,start_connection/2]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([init/1, terminate/2]).

%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: start/2
%%-----------------------------------------------------------------
start(sup, Opts) ->
    ?PRINTDEBUG2("start sup in ~p", [self()]),
    supervisor:start_link({local, orber_iiop_insup}, orber_iiop_insup,
			  {sup, Opts});
start(A1, A2) -> 
    ?PRINTDEBUG2("start/2 called with parameters ~p ~p", [A1, A2]),
    ok.


%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% Func: init/1
%%-----------------------------------------------------------------
init({sup, Opts}) ->
    ?PRINTDEBUG2("orber_iiop_insup init supervisor ~p", [self()]),
    SupFlags = {simple_one_for_one, 500, 100},
    ChildSpec = [
		 {name1, {orber_iiop_inproxy, start, []}, temporary, 
		  10000, worker, [orber_iiop_inproxy]}
		],
    {ok, {SupFlags, ChildSpec}};
init(Opts) ->
    ?PRINTDEBUG2("plain init ~p", [self()]),
    {ok, []}.


%%-----------------------------------------------------------------
%% Func: terminate/1
%%-----------------------------------------------------------------
terminate(Reason, State) ->
    ?PRINTDEBUG2("inproxy supervisor terminated with reason: ~p", [Reason]),
    ok.

%%-----------------------------------------------------------------
%% Func: start_connection/2
%%-----------------------------------------------------------------
start_connection(Type, Socket) ->
    ?PRINTDEBUG2("supervisor ~p starting bridge for ~p socket ~p",
		 [self(), Type, Socket]),
    supervisor:start_child(orber_iiop_insup, [{connect, Type, Socket}]).

