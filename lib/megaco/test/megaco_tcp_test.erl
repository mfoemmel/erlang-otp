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
-module(megaco_tcp_test).

%%----------------------------------------------------------------------
%% Include files
%%----------------------------------------------------------------------
-include_lib("megaco/src/tcp/megaco_tcp.hrl").
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

errors(suite) ->
    [
     socket,
     accept_process,
     accept_supervisor,
     connection_supervisor,
     tcp_server
    ].

start(suite) ->
    [];
start(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Options = [{port, 20000}, {receivehandle, apa}],
    normal_start_case(Options).

sending(suite) ->
    [];
sending(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    sendreceive().


socket(suite) ->
    [];
socket(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    failing_socket().

accept_process(suite) ->
    [];
accept_process(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    failing_accept_process().

accept_supervisor(suite) ->
    [];
accept_supervisor(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    failing_accept_supervisor().

connection_supervisor(suite) ->
    [];
connection_supervisor(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    failing_connection_supervisor().

tcp_server(suite) ->
    [];
tcp_server(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    failing_tcp_server().

%%======================================================================
%% Test functions
%%======================================================================

normal_start_case(Options) ->
    case catch megaco_tcp:start_transport() of
	{ok, Pid} ->
	    case catch megaco_tcp:listen(Pid, Options) of
		ok ->
		    ok;
		{error, Reason} ->
		    io:format("<ERROR> couldn't start tpkt socket "
			      "because of: ~w~n",
			      [Reason])
	    end;
	{error, Reason} ->
	    io:format("<ERROR> couldn't start tpkt because of: ~w~n",
		      [Reason])
    end.

sendreceive() ->
    ?SKIP(not_yet_implemented).

failing_socket() ->
    ?SKIP(not_yet_implemented).

failing_accept_process() ->
    ?SKIP(not_yet_implemented).

failing_accept_supervisor() ->
    ?SKIP(not_yet_implemented).

failing_connection_supervisor() ->
    ?SKIP(not_yet_implemented).

failing_tcp_server() ->
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
