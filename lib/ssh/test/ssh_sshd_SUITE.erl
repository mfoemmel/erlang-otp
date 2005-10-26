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
-module(ssh_sshd_SUITE).
-include("test_server.hrl").

% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).
-define(application, ssh).

% Test server specific exports
-export([all/1]).
-export([init_per_testcase/2, fin_per_testcase/2]).

% Test cases must be exported.
-export([ssh/1, ssh_compressed/1]).

%% Import some utilities
-import(ssh_test_lib, [rx/2, collect_data_upto/2]).

-define(EXPECT_TIMEOUT, 2000).
-define(FLUSH_TIMEOUT, 500).
-define(PRINT_FLUSH, false).
-define(PRINT_EXPECT, false).

init_per_testcase(_Case, Config) ->
    ?line rename_known_hosts(backup),
    ?line Dog = test_server:timetrap(?default_timeout),
    [{watchdog, Dog} | Config].
fin_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    rename_known_hosts(restore),
    ok.


all(suite) ->
    case os:find_executable("ssh") of
	false -> {skip, "SSH not installed on host"};
	_ -> [ssh, ssh_compressed]
    end.

%%
%%
ssh(doc) ->
    ["Start a daemon, and use shell ssh to connect to it."];
ssh(suite) ->
    [];
ssh(Config) when list(Config) ->
    ?line SysDir = ?config(data_dir, Config),
    ?line {ok, Pid} = ssh_sshd:listen(9999, [{system_dir, SysDir}]),
    ?line Cmd = "ssh -p 9999 -o StrictHostKeyChecking=no localhost",
    ?line P = open_port({spawn, Cmd}, [stream]),
    ?line flush_debug_print_msgs(P),
    ?line D1 = collect_data_upto(P, "1> "),
    ?line rx(D1, "^Eshell.*\n.*1> $"),
    ?line port_command(P, "abc.\n"),
    ?line flush_debug_print_msgs(P),
    ?line D2 = collect_data_upto(P, "2> "),
    ?line rx(D2, "^abc.*\nabc.*\n.*2> $"),
    ?line port_command(P, "exit().\n"),
    ?line flush_all_data(P),
    ?line ok = ssh_sshd:stop(Pid).

%%
%%
ssh_compressed(doc) ->
    ["Start a daemon, and use shell ssh to connect to it."];
ssh_compressed(suite) ->
    [];
ssh_compressed(Config) when list(Config) ->
    ?line SysDir = ?config(data_dir, Config),
    ?line {ok, Pid} = ssh_sshd:listen(9999, [{system_dir, SysDir}]),
    ?line Cmd = "ssh -p 9999 -o StrictHostKeyChecking=no -C localhost",
    ?line P = open_port({spawn, Cmd}, [stream]),
    ?line flush_debug_print_msgs(P),
    ?line D1 = collect_data_upto(P, "1> "),
    ?line rx(D1, "^Eshell.*\n.*1> $"),
    ?line port_command(P, "abc.\n"),
    ?line flush_debug_print_msgs(P),
    ?line D2 = collect_data_upto(P, "2> "),
    ?line rx(D2, "^abc.*\nabc.*\n.*2> $"),
    ?line port_command(P, "exit().\n"),
    ?line flush_all_data(P),
    ?line ok = ssh_sshd:stop(Pid).

%%
%% Help functions
%%

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

rename_known_hosts(BR) ->
    KnownHosts = ssh_file:file_name(user, "known_hosts", []),
    B = KnownHosts ++ "xxx",
    case BR of
	backup ->
	    file:rename(KnownHosts, B);
	restore ->
	    file:delete(KnownHosts),
	    file:rename(B, KnownHosts)
    end.
    
