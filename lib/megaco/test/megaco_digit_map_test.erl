%%<copyright>
%% <year>2005-2007</year>
%% <holder>Ericsson AB, All Rights Reserved</holder>
%%</copyright>
%%<legalnotice>
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% The Initial Developer of the Original Code is Ericsson AB.
%%</legalnotice>
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
     otp_5750,
     otp_5799,
     otp_5826
    ].


otp_5750(suite) ->
    [
     otp_5750_01,
     otp_5750_02
    ].

otp_5799(suite) ->
    [
     otp_5799_01
    ].

otp_5826(suite) ->
    [
     otp_5826_01,
     otp_5826_02,
     otp_5826_03
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
	  fun({ok, {full, "1"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end},

	 {2,
	  fun() ->
		  (catch tde(DM, "123"))
	  end,
	  fun({ok, {unambiguous, "123"}}) ->
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
		  (catch otp_5750_02_exec(500, DM, "113"))
	  end,
	  fun({ok, {unambiguous, "113"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end},

	 {2,
	  fun() ->
		  (catch otp_5750_02_exec(500, DM, "114"))
	  end,
	  fun({ok, {unambiguous, "114"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end},

	 {3,
	  fun() ->
		  (catch otp_5750_02_exec(5000, DM, "11ssss3"))
	  end,
	  fun({ok, {unambiguous, "113"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end},

	 {4,
	  fun() ->
		  (catch otp_5750_02_exec(5000, DM, "11ssss4"))
	  end,
	  fun({ok, {unambiguous, "114"}}) ->
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

otp_5799_01(suite) ->
    [];
otp_5799_01(doc) ->
    [];
otp_5799_01(Config) when list(Config) ->
    DM = "234 | 23456",

    %% First case
    Tests = 
	[
	 {1,
	  fun() ->
		  (catch tde(DM, "2349"))
	  end,
	  fun({ok, {full, "234", $9}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}
	],

    dm_tests(Tests),

    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_5826_01(suite) ->
    [];
otp_5826_01(doc) ->
    [];
otp_5826_01(Config) when list(Config) ->
    DM = "123Z56",

    %% First case
    Tests = 
	[
	 {1,
	  fun() ->
		  (catch tde(DM, [$1,$2,$3,{long, $5},$6]))
	  end,
	  fun({ok, {unambiguous, "123Z56"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end},
	 {2,
	  fun() ->
		  (catch tde(DM, [$1,$2,{long, $3},{long,$5},$6]))
	  end,
	  fun({ok, {unambiguous, "123Z56"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end},
	 {3,
	  fun() ->
		  (catch tde(DM, [$1,$2,$3,{long,$5},{long,$6}]))
	  end,
	  fun({ok, {unambiguous, "123Z56"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end},
	 {4,
	  fun() ->
		  (catch tde(DM, [$1,$2,{long, $3},{long,$5},{long,$6}]))
	  end,
	  fun({ok, {unambiguous, "123Z56"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}
	],

    dm_tests(Tests),

    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_5826_02(suite) ->
    [];
otp_5826_02(doc) ->
    [];
otp_5826_02(Config) when list(Config) ->
    DM = "12356",

    %% First case
    Tests = 
	[
	 {1,
	  fun() ->
		  (catch tde(DM, [$1,$2,$3,{long, $5},$6]))
	  end,
	  fun({ok, {unambiguous, "12356"}}) ->
		  ok;
	     (Else) ->
		  {error, {unexpected_digit_map_result, Else}}
	  end}
	],

    dm_tests(Tests),

    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

otp_5826_03(suite) ->
    [];
otp_5826_03(doc) ->
    [];
otp_5826_03(Config) when list(Config) ->
    DM = "12346 | 12Z346 | 12Z34Z7 | 1234Z8",

    %% First case
    Tests = 
	[
 	 {1,
 	  fun() ->
 		  (catch tde(DM, [$1,$2,{long, $3},$4,$6]))
 	  end,
 	  fun({ok, {unambiguous, "12Z346"}}) ->
 		  ok;
 	     (Else) ->
 		  {error, {unexpected_digit_map_result, Else}}
 	  end},
 	 {2,
 	  fun() ->
 		  (catch tde(DM, [$1, {long, $2}, {long, $3},$4, $6]))
 	  end,
 	  fun({ok, {unambiguous, "12Z346"}}) ->
 		  ok;
 	     (Else) ->
 		  {error, {unexpected_digit_map_result, Else}}
 	  end},
 	 {3,
 	  fun() ->
 		  (catch tde(DM, [$1,$2,{long, $3},{long, $4},$6]))
 	  end,
 	  fun({ok, {unambiguous, "12Z346"}}) ->
 		  ok;
 	     (Else) ->
 		  {error, {unexpected_digit_map_result, Else}}
 	  end},
 	 {4,
 	  fun() ->
 		  (catch tde(DM, [$1,$2,{long, $3},$4,{long, $7}]))
 	  end,
 	  fun({ok, {unambiguous, "12Z34Z7"}}) ->
 		  ok;
 	     (Else) ->
 		  {error, {unexpected_digit_map_result, Else}}
 	  end},
 	 {5,
 	  fun() ->
 		  (catch tde(DM, [$1,$2,{long, $3},$4,{long, $8}]))
 	  end,
 	  fun({error, 
	       {unexpected_event, {long, $8}, _Collected, _Expected}}) ->
 		  ok;
 	     (Else) ->
 		  {error, {unexpected_digit_map_result, Else}}
 	  end},
 	 {6,
 	  fun() ->
 		  (catch tde(DM, [$1,$2,$3,$4,{long, $8}]))
 	  end,
 	  fun({ok, {unambiguous, "1234Z8"}}) ->
 		  ok;
 	     (Else) ->
 		  {error, {unexpected_digit_map_result, Else}}
 	  end}
	],

    dm_tests(Tests),

    ok.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dm_tests([]) ->
    ok;
dm_tests([{No, Exec, Ver}|Tests]) 
  when is_integer(No) and is_function(Exec) and is_function(Ver) ->
    case dm_test(Exec, Ver) of
	ok ->
	    dm_tests(Tests);
	{error, Reason} ->
	    ?ERROR({No, Reason});
	Error ->
	    ?ERROR({No, Error})
    end.

dm_test(Exec, Verify) ->
    (catch Verify(Exec())).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tde(DM, Evs) -> megaco:test_digit_event(DM, Evs).

