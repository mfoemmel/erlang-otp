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
%% Purpose:
%%----------------------------------------------------------------------
-module(megaco_udp_test).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("megaco/src/udp/megaco_udp.hrl").
-include("megaco_test_lib.hrl").

-compile(export_all).

%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([
        ]).

%%----------------------------------------------------------------------
%% Internal exports
%%----------------------------------------------------------------------
-export([
        ]).

%%----------------------------------------------------------------------
%% Macros
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Records
%%----------------------------------------------------------------------

%%======================================================================
%% External functions
%%======================================================================
%%----------------------------------------------------------------------
%% Function: t/0
%% Description: Run all test cases
%%----------------------------------------------------------------------
t()     -> megaco_test_lib:t(?MODULE).
%%----------------------------------------------------------------------
%% Function: t/1
%% Description: Run the specified test cases 
%%----------------------------------------------------------------------
t(Case) -> megaco_test_lib:t({?MODULE, Case}).
    

%%======================================================================
%% Test server callbacks
%%======================================================================
%%----------------------------------------------------------------------
%% Function: init_per_testcase/2
%% Description: 
%%----------------------------------------------------------------------
init_per_testcase(Case, Config) ->
    megaco_test_lib:init_per_testcase(Case, Config).

%%----------------------------------------------------------------------
%% Function: fin_per_testcase/2
%% Description: 
%%----------------------------------------------------------------------
fin_per_testcase(Case, Config) ->
    megaco_test_lib:fin_per_testcase(Case, Config).

%%======================================================================
%% Test case definitions
%%======================================================================
all(suite) ->
    [
     start,
     sending,
     errors
    ].

start(suite) ->
    [];
start(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    normal_start_case().

sending(suite) ->
    [];
sending(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    sendreceive().

errors(suite) ->
    [];
errors(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    failing_socket().

%%======================================================================
%% Test functions
%%======================================================================

normal_start_case() ->
    ?SKIP(not_yet_implemented).

sendreceive() ->
    ?SKIP(not_yet_implemented).

failing_socket() ->
    ?SKIP(not_yet_implemented).

%%======================================================================
%% Internal functions
%%======================================================================
compute_res(All) ->
    compute_res(All, [], 0).

compute_res([H | T], Bad, Sum) when integer(H) ->
    compute_res(T, Bad, Sum + H);
compute_res([H | T], Bad, Sum) ->
    compute_res(T, [H | Bad], Sum);
compute_res([], Bad, Sum) ->
    ok = io:format("#bytes: ~w; errors: ~p~n", [Sum, Bad]).
