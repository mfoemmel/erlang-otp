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
-module(ssh_ssh_SUITE).
-include("test_server.hrl").

% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).
-define(application, ssh).

% Test server specific exports
-export([all/1]).
-export([init_per_testcase/2, fin_per_testcase/2]).
-export([init_per_suite/1, end_per_suite/1]).

% Test cases must be exported.
-export([ssh/1, ssh_compressed/1]).

%% Import some utilities
-import(ssh_test_lib, [expect_msg/0, rx/2, collect_data_upto/2,
		       get_id_keys/1, remove_id_keys/1]).

all(suite) ->
    case os:find_executable("ssh") of
	false -> {skip, "SSH not installed on host"};
	_ -> [ssh, ssh_compressed]
    end.

%%
%%
init_per_testcase(_Case, Config) ->
    ?line Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].

fin_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

init_per_suite(Config) ->
    ?line {ok, _} = get_id_keys(?config(priv_dir, Config)),
    Config.

end_per_suite(Config) ->
    ?line Dir = ?config(priv_dir, Config),
    ?line remove_id_keys(Dir),
    ?line file:delete(filename:join(Dir, "known_hosts")),
    ?line Config.

-define(EXPECT_TIMEOUT, 2000).
-define(FLUSH_TIMEOUT, 500).
-define(PRINT_FLUSH, true).
-define(PRINT_EXPECT, false).

%%
%%
ssh(doc) ->
    ["ssh on localhost."];
ssh(suite) ->
    [];
ssh(Config) when list(Config) ->
    ?line UsrDir = ?config(priv_dir, Config),
    ?line Eval1 =  "ssh_ssh:connect(\"localhost\", [{user_dir, ",
    ?line Eval2 = ["\"", UsrDir, "\""],
    ?line Eval3 = "}, {user_interaction, false}, silently_accept_hosts]).",
    ?line Eval = [Eval1, Eval2, Eval3, "\n"],
    ?line Cmd = erl_program(),
    ?line EP = open_port({spawn, Cmd}, [stream]),
    ?line flush_debug_print_msgs(EP),
    ?line D1 = collect_data_upto(EP, "1> "),
    ?line rx(D1, "^Eshell.*\n.*1> $"),
    ?line port_command(EP, Eval),
    ?line flush_debug_print_msgs(EP),
    ?line D2 = collect_data_upto(EP, ">"),
    ?line rx(D2, ">"),
    ?line port_command(EP, "echo hej\n"),
    ?line flush_debug_print_msgs(EP),
    ?line D3 = collect_data_upto(EP, "\n>"),
    ?line rx(D3, ">hej\n>"),
    ?line port_command(EP, "exit\n\n"),
    ?line flush_debug_print_msgs(EP),
    ?line port_command(EP, "q().\n"),
    ?line flush_debug_print_msgs(EP),
    ?line flush_all_data(EP),
    ?line port_close(EP),
    ?line timeout = expect_msg(),
    ?line ok.

%%
%%
ssh_compressed(doc) ->
    ["ssh on localhost."];
ssh_compressed(suite) ->
    [];
ssh_compressed(Config) when list(Config) ->
    ?line UsrDir = ?config(priv_dir, Config),
    ?line Eval1 =  "ssh_ssh:connect(\"localhost\", [{user_dir, ",
    ?line Eval2 = ["\"", UsrDir, "\""],
    ?line Eval3 = "}, {user_interaction, false}, silently_accept_hosts, "
	"{compression, zlib}]).",
    ?line Eval = [Eval1, Eval2, Eval3, "\n"],
    ?line Cmd = erl_program(),
    ?line EP = open_port({spawn, Cmd}, [stream]),
    ?line flush_debug_print_msgs(EP),
    ?line D1 = collect_data_upto(EP, "1> "),
    ?line rx(D1, "^Eshell.*\n.*1> $"),
    ?line port_command(EP, Eval),
    ?line flush_debug_print_msgs(EP),
    ?line D2 = collect_data_upto(EP, ">"),
    ?line rx(D2, ">"),
    ?line port_command(EP, "echo hej\n"),
    ?line flush_debug_print_msgs(EP),
    ?line D3 = collect_data_upto(EP, "\n>"),
    ?line rx(D3, ">hej\n>"),
    ?line port_command(EP, "exit\n\n"),
    ?line flush_debug_print_msgs(EP),
    ?line port_command(EP, "q().\n"),
    ?line flush_debug_print_msgs(EP),
    ?line flush_all_data(EP),
    ?line port_close(EP),
    ?line timeout = expect_msg(),
    ?line ok.

%% ssh_cmd(Dir) ->
%%     ssh_ssh:connect("localhost", [{user_dir, Dir}, {system_dir, Dir}]).

expect_data(Port) ->
    receive
	{Port, {data, Data}} ->
	    p_expect(Data),
	    Data
    after ?EXPECT_TIMEOUT ->
	    timeout
    end.

expect_regexp_data(Port, Regexp) ->
    receive
	{Port, {data, Data}} ->
	    p_expect(Data),
	    regexp:match(Data, Regexp)
    after ?EXPECT_TIMEOUT ->
	    timeout
    end.

p_expect(Data) when is_list(Data)->
    ?PRINT_EXPECT andalso ok == io:format("#Expect \"~s\"\n", [Data]);
p_expect(Data) ->
    ?PRINT_EXPECT andalso ok == io:format("#Expect \"~w\"\n", [Data]).

p_flush(Data) ->
    ?PRINT_FLUSH andalso ok == io:format("#Flushed \"~s\"\n", [Data]).

flush_debug_print_msgs(Port) ->
    receive
	{Port, {data, [$# | D]}} ->
	    p_flush([$# | D]),
	    flush_debug_print_msgs(Port)
    after ?FLUSH_TIMEOUT ->
	    ok
    end.

erl_program() ->
    atom_to_list(lib:progname()).

flush_all_data(Port) ->
    receive
	{Port, {data, Data}} ->
	    p_flush(Data),
	    flush_all_data(Port);
	_ ->
	    ok
    after ?FLUSH_TIMEOUT ->
	    timeout
    end.

