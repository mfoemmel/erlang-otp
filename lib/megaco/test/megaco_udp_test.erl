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


%%----------------------------------------------------------------------
%% External exports
%%----------------------------------------------------------------------
-export([
	 all/1,
	 
	 start/1,
	 start_normal/1,
	 start_invalid_opt/1,

	 sending/1,
	 sendreceive/1,

	 errors/1,
	 socket/1,

         init_per_testcase/2, fin_per_testcase/2,
 
         t/0, t/1
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
t() -> megaco_test_lib:t(?MODULE).

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
    [
     start_normal,
     start_invalid_opt
    ].

sending(suite) ->
    [
     sendreceive
    ].

errors(suite) ->
    [
     socket
    ].


%% ------------------ start ------------------------

start_normal(suite) ->
    [];
start_normal(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Opts = [{port, 0}, {receive_handle, apa}],
    start_case(Opts, ok).

start_invalid_opt(suite) ->
    [];
start_invalid_opt(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Opts = [{port, 0}, {receivehandle, apa}],
    start_case(Opts, error).


%% ------------------ sending ------------------------

sendreceive(suite) ->
    [];
sendreceive(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    sendreceive().


%% ------------------ errors ------------------------

socket(suite) ->
    [];
socket(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    failing_socket().


%%======================================================================
%% Test functions
%%======================================================================

start_case(Opts, Expect) ->
    case (catch megaco_udp:start_transport()) of
	{ok, Pid} ->
	    case (catch megaco_udp:open(Pid, Opts)) of
		{ok, _Handle, _CtrlPid} when Expect == ok ->
		    {ok, Pid};
		{ok, Handle, CtrlPid} ->
		    exit(Pid, kill),
		    ?ERROR({unexpected_start_sucesss, Handle, CtrlPid});
		{error, _Reason} when Expect == error ->
		    exit(Pid, kill),
		    ok;
		{error, Reason} ->
		    exit(Pid, kill),
		    ?ERROR({unexpected_start_failure, Reason});
		Error ->
		    ?ERROR({unexpected_result, Error})
	    end;
	{error, Reason} ->
	    ?ERROR({failed_starting_transport, Reason})
    end.

sendreceive() ->
    ?SKIP(not_yet_implemented).

failing_socket() ->
    ?SKIP(not_yet_implemented).

