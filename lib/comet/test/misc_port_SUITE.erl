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
-module(misc_port_SUITE).

% testing misc. port- and thread-related stuff

%% Test functions
-export([all/1,not_run/1,init_per_testcase/2, fin_per_testcase/2,
	 driver_program_mismatch/1, server_stop/1, concurrent_threads/1]).

%% interal export
-export([sleep_it/3]).

-include("test_server.hrl").
-define(default_timeout, ?t:minutes(1)).

init_per_testcase(Case, Config) ->
    ?line Datadir=?config(data_dir, Config),
    ?line os:cmd("regsvr32 /s " ++ filename:nativename(filename:join(Datadir, "ErlComTestServ.DLL"))),
    ?line Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].
fin_per_testcase(Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

all(suite) ->
    case os:type() of
	{win32, _} ->
	    [driver_program_mismatch, server_stop, concurrent_threads
	     ];
	_ ->
	    [not_run]
    end.

not_run(doc) -> ["Doesn't run on UNIX"];
not_run(suite) -> [];
not_run(Config) when list(Config) -> {comment, "Doesn't run on UNIX."}.

get_erl_test_obj(Driver) ->
    ?line {ok, Pid}= erl_com:get_or_start(list_to_atom("tst_"++atom_to_list(Driver)), Driver),
    ?line erl_com:create_dispatch(Pid, "{5FFFAC7E-E087-11D3-AC85-00C04F9DA8C8}", 1).

release_erl_test_obj(Obj) ->
    ?line erl_com:release(Obj),
    ?line ok= erl_com:end_thread(Obj).

driver_program_mismatch(suite) -> [];
driver_program_mismatch(doc) -> ["Testing driver program mismatch"];
driver_program_mismatch(Config) when list(Config) -> 
    ?line {ok, Pid}= erl_com:get_driver(tst_driver),
    ?line {ok, P2}= erl_com:get_program(tst_program),
    ?line true= Pid=/=P2,
    ?line {error, _}= erl_com:get_driver(tst_program),
    ?line {error, _}= erl_com:get_program(tst_driver),
    ok.

server_stop(suite) -> [];
server_stop(doc) -> ["Server stop"];
server_stop(Config) when list(Config) ->
    ?line {ok, Pid}= erl_com:get_program(tst_program),
    ?line {ok, Pid}= erl_com:get_program(tst_program),
    ?line erl_com:stop(tst_program),
    ?line {ok, P2}= erl_com:get_program(tst_program),
    ?line true= Pid=/=P2,
    ok.

sleep_it(Pid, I, S) ->
    erl_com:invoke(I, "Delay", S*1000),
    Pid ! slept.

wait_p(0) ->
    ok;
wait_p(N) ->
    receive
	slept ->
	    io:format("slept"),
	    wait_p(N-1);
	_ ->
	    wait_p(N)
    end.

concurrent_thread_test(Driver) ->
    ?line O1= get_erl_test_obj(Driver),
    ?line {com_interface, Pid, Thread, Intf}= O1,
    ?line T= erl_com:new_thread(Pid),
    ?line O2= erl_com:create_object(T, "{5FFFAC7E-E087-11D3-AC85-00C04F9DA8C8}", 1),
    ?line Now0= calendar:now_to_local_time(erlang:now()),
    ?line Now0_s= calendar:datetime_to_gregorian_seconds(Now0),
    ?line Delay_s= 2,
    ?line P1= spawn_link(?MODULE, sleep_it, [self(), O1, Delay_s]),
    ?line P2= spawn_link(?MODULE, sleep_it, [self(), O2, Delay_s]),
    ?line wait_p(2),
    ?line Now1= calendar:now_to_local_time(erlang:now()),
    ?line Now1_s= calendar:datetime_to_gregorian_seconds(Now1),
    ?line true=2>abs(Now1_s - Now0_s - Delay_s),
    ok. % ?line release_erl_test_obj(I).

concurrent_threads(suite) -> [];
concurrent_threads(doc) -> ["Testing threads concerrency"];
concurrent_threads(Config) when list(Config) -> 
    ?line ok= concurrent_thread_test(driver),
    ?line ok= concurrent_thread_test(program),
    ok.

