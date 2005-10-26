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
%% Purpose: Verify the application specifics of the Megaco application
%%----------------------------------------------------------------------
-module(megaco_digit_map_test).

-compile(export_all).

-include("megaco_test_lib.hrl").


t()     -> megaco_test_lib:t(?MODULE).
t(Case) -> megaco_test_lib:t({?MODULE, Case}).


%% Test server callbacks
init_per_testcase(Case, Config) ->
    megaco_test_lib:init_per_testcase(Case, Config).

fin_per_testcase(Case, Config) ->
    megaco_test_lib:fin_per_testcase(Case, Config).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

all(suite) ->
    Cases = 
	[
	 tickets
	],
    Cases.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tickets(suite) ->
    [
     otp_5750
    ].


otp_5750(suite) ->
    [
     otp_5750_01,
     otp_5750_02,
     otp_5750_03
    ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_5750_01(suite) ->
    [];
otp_5750_01(doc) ->
    [];
otp_5750_01(Config) when list(Config) ->
    DM = "1 | 123",

    %% First case
    Tests = 
	[
	 {1,
	  fun() ->
		  (catch tde(DM, "1"))
	  end,
	  fun({ok, full, "1"}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end},

	 {2,
	  fun() ->
		  (catch tde(DM, "123"))
	  end,
	  fun({ok, unambiguous, "123"}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end},

	 {3,
	  fun() ->
		  (catch tde(DM, "124"))
	  end,
	  fun({error, _}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}
	 ],
	  
    dm_tests(Tests),

    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_5750_02(suite) ->
    [];
otp_5750_02(doc) ->
    [];
otp_5750_02(Config) when list(Config) ->
    DM = "xxx | xxL3 | xxS4",

    %% First case
    Tests = 
	[
	 {1,
	  fun() ->
		  %% (catch tde(DM, "113"))
		  (catch otp_5750_02_exec(500, DM, "113"))
	  end,
	  fun({ok, unambiguous, "113"}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end},

	 {2,
	  fun() ->
		  %% (catch tde(DM, "114"))
		  (catch otp_5750_02_exec(500, DM, "114"))
	  end,
	  fun({ok, unambiguous, "114"}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end},

	 {3,
	  fun() ->
		  %% (catch tde(DM, "11ssss3"))
		  (catch otp_5750_02_exec(5000, DM, "11ssss3"))
	  end,
	  fun({ok, unambiguous, "113"}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end},

	 {4,
	  fun() ->
		  %% (catch tde(DM, "11ssss4"))
		  (catch otp_5750_02_exec(5000, DM, "11ssss4"))
	  end,
	  fun({ok, unambiguous, "114"}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}

	 ],
	  
    dm_tests(Tests),

    ok.

otp_5750_02_exec(To, DM, Evs) ->
    Pid = self(),
    Tester = 
	spawn(fun() ->
		      Res = tde(DM, Evs),
		      Pid ! {result, self(), Res}
	      end),
    receive
	{result, Tester, Res} ->
	    Res
    after To ->
	    {error, timeout}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dm_tests([]) ->
    ok;
dm_tests([{No, Exec, Ver}|Tests]) 
  when is_integer(No) and is_function(Exec) and is_function(Ver) ->
    case dm_test(Exec, Ver) of
	ok ->
	    dm_tests(Tests);
	{error, Reason} ->
	    ?ERROR({Reason, No});
	Error ->
	    ?ERROR({Error, No})
    end.

dm_test(Exec, Verify) ->
    (catch Verify(Exec())).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tde(DM, Evs) -> megaco:test_digit_event(DM, Evs).

