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
%%----------------------------------------------------------------------
%% Purpose: The supervisor for the Megaco/H.248 ack sender processes
%%----------------------------------------------------------------------

-module(megaco_acks_sup).

-behaviour(supervisor).

%% public
-export([start/0, stop/1, init/1]).
-export([start_ack_sender/3]).

%% -define(d(F,A), io:format("~p~p:" ++ F ++ "~n", [self(),?MODULE|A])).
-define(d(F,A), ok).

start() ->
    ?d("start -> entry",[]),
    SupName = {local,?MODULE},
    supervisor:start_link(SupName, ?MODULE, []).

stop(_StartArgs) ->
    ok.

init([]) -> 
    init();
init(BadArg) ->
    {error, {badarg, BadArg}}.

init() ->
    ?d("init -> entry",[]),
    Flags     = {one_for_one, 500, 100},
    Workers   = [],
    ?d("init -> done",[]),
    {ok, {Flags, Workers}}.


%%----------------------------------------------------------------------
%% Function: start_ack_sender/3
%% Description: Starts a transient worker (child) process
%%----------------------------------------------------------------------

start_ack_sender(CH, To, Max) ->
    ?d("start_ack_sender -> entry with"
	"~n   CH:  ~p"
	"~n   To:  ~p"
	"~n   Max: ~p", [CH, To, Max]),
    M = megaco_ack_sender,
    F = start_link,
    A = [CH, To, Max],
    Spec = {{M,CH}, 
	    {M,F,A}, 
	    transient, timer:seconds(1), worker, [M,gen_server]},
    supervisor:start_child(?MODULE, Spec).


    


