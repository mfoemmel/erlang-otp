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
-module(ssh_sftp_SUITE).
-include("test_server.hrl").

% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).
-define(application, ssh).

% Test server specific exports
-export([all/1]).
-export([init_per_testcase/2, fin_per_testcase/2]).
-export([init_per_suite/1, end_per_suite/1]).

% Test cases must be exported.
-export([sftp/1]).

%% Import some utilities
-import(ssh_test_lib, [expect_msg/0, rx/2, collect_data_upto/2,
		       get_id_keys/1, remove_id_keys/1]).

all(suite) ->
    case os:find_executable("ssh") of
	false -> {skip, "SSH not installed on host"};
	_ -> [sftp]
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
sftp(doc) ->
    ["sftp on localhost."];
sftp(suite) ->
    [];
sftp(Config) when list(Config) ->
    ?line UsrDir = ?config(priv_dir, Config),
    ?line DataDir = ?config(data_dir, Config),
    ?line OldF = filename:join(DataDir, "sftp.txt"),
    ?line NewF = filename:join(DataDir, "sftp2.txt"),
    ?line {ok, S} = ssh_sftp:connect("localhost", [{user_dir, UsrDir},
						   silently_accept_hosts]),
    %% open
    ?line {ok, Hreadx} = ssh_sftp:open(S, OldF, [read]),
    ?line {ok, Hwritebin} = ssh_sftp:open(S, NewF, [write, binary]),
    %% opendir
    ?line {ok, Hdirx} = ssh_sftp:opendir(S, DataDir),
    %% close
    ?line ok = ssh_sftp:close(S, Hreadx),
    ?line ok = ssh_sftp:close(S, Hdirx),
    ?line ok = ssh_sftp:close(S, Hwritebin),
    %% readdir
    ?line {ok, Hdir} = ssh_sftp:opendir(S, DataDir),
    ?line {ok, Files0} = ssh_sftp:readdir(S, Hdir),
    ?line Files1 = [A || {A, _} <- Files0],
    ?line Files2 = lists:sort(Files1),
    ?line {ok, Files3} = file:list_dir(DataDir),
    ?line Files4 = lists:sort(Files3 ++ [".", ".."]),
    io:format("Files2=~p Files4=~p\n", [Files2, Files4]),
    ?line Files2 = Files4,
    %% pread
    ?line {ok, Hread} = ssh_sftp:open(S, OldF, [read]),
    ?line {ok, "Value  \tValue   (Nam"} = ssh_sftp:pread(S, Hread, 100, 20),
    %% read
    ?line {ok, "e) \t...\n\n      8    "} = ssh_sftp:read(S, Hread, 20),
    %% apread
    ?line {async, A1} = ssh_sftp:apread(S, Hread, 140, 20),
    ?line {async_reply, A1, {ok, "\t\n      9    \t\n     "}} = expect_msg(),
    %% aread
    ?line {async, A2} = ssh_sftp:aread(S, Hread, 20),
    ?line {async_reply, A2, {ok, "10    \t\n     11    \t"}} = expect_msg(),
    %% pwrite
    ?line L = lists:seq(0, 255),
    ?line F = filename:join(UsrDir, "tst.txt"),
    ?line ok = file:write_file(F, list_to_binary(L)),
    io:format("A ~p\n", [file:read_file(F)]),
    ?line {ok, Hwrite} = ssh_sftp:open(S, F, [write, read]),
    io:format("B ~p\n", [file:read_file(F)]),
    ?line ssh_sftp:pwrite(S, Hwrite, 254, [1, 2]),
    io:format("C ~p\n", [file:read_file(F)]),
    ?line B = list_to_binary([lists:seq(0, 253), [1, 2]]),
    ?line {ok, B} = file:read_file(F),
    %% mkdir
    ?line Dir = "/tmp/ssh_sftp_test_dir1",
    ?line file:del_dir(Dir),			% in case we got leftovers
    ?line ok = ssh_sftp:make_dir(S, Dir),
    %% rmdir
    ?line ok = ssh_sftp:del_dir(S, Dir),
    %% close
    ?line ok = ssh_sftp:stop(S),
    ?line ok.

%% ssh_cmd(Dir) ->
%%     ssh_ssh:connect("localhost", [{user_dir, Dir}, {system_dir, Dir}]).

