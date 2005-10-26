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
-module(ssh_cm_SUITE).
-include("test_server.hrl").

% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).
-define(application, ssh).

% Test server specific exports
-export([all/1]).
-export([init_per_testcase/2, fin_per_testcase/2]).
-export([init_per_suite/1, end_per_suite/1]).

% Test cases must be exported.
-export([open_session/1,
	 open_session_compressed/1,
	 exec/1,
	 listen/1,
	 listen_compressed/1,
	 api/1,
	 shell/1]).

%% Import some utilities
-import(ssh_test_lib, [expect_msg/0, one_of/4, one_of/6,
		       get_id_keys/1, remove_id_keys/1]).

-define(FLUSH_TIMEOUT, 100).
-define(PRINT_FLUSH, true).

all(suite) ->
    case os:find_executable("ssh") of
	false -> {skip, "SSH not installed on host"};
	_ -> 
	    [open_session,
	     open_session_compressed,
	     exec,
	     shell,
	     listen,
	     listen_compressed,
	     api]
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
    ?line ssh_test_lib:remove_id_keys(Dir),
    ?line file:delete(filename:join(Dir, "known_hosts")),
    ?line Config.


%%
%%
open_session(doc) ->
    ["open_session to localhost."];
open_session(suite) ->
    [];
open_session(Config) when list(Config) ->
    ?line UsrDir = ?config(priv_dir, Config),
    ?line {ok, CM} =
	ssh_cm:connect("localhost", [{user_dir, UsrDir},
				     {user_interaction, false},
				     silently_accept_hosts]),
    ?line {ok, Channel} = ssh_cm:session_open(CM),
    ?line ok = ssh_cm:close(CM, Channel),
    ?line ok = ssh_cm:stop(CM).

%%
%%
open_session_compressed(doc) ->
    ["open_session to localhost."];
open_session_compressed(suite) ->
    [];
open_session_compressed(Config) when list(Config) ->
    ?line UsrDir = ?config(priv_dir, Config),
    ?line {ok, CM} =
	ssh_cm:connect("localhost", [{user_dir, UsrDir},
				     {user_interaction, false},
				     silently_accept_hosts,
				     {compression, zlib}]),
    ?line {ok, Channel} = ssh_cm:session_open(CM),
    ?line ok = ssh_cm:close(CM, Channel),
    ?line ok = ssh_cm:stop(CM).

%%
%%
listen(doc) ->
    ["listen on localhost."];
listen(suite) ->
    [];
listen(Config) when list(Config) ->
    ?line SysDir = ?config(data_dir, Config),
    ?line UsrDir = ?config(priv_dir, Config),
    ?line Self = self(),
    ?line {ok, Listener} =
	ssh_cm:listen(fun() ->
			      spawn(fun() ->
					    resend_all(Self),
					    ok
				    end)
		      end, 9999,
		      [{system_dir, SysDir}]),
    ?line receive {} -> {} after 500 -> ok end,
    ?line {ok, CM} =
	ssh_cm:connect("localhost", [{user_dir, UsrDir},
				     {user_interaction, false},
				     silently_accept_hosts]),
    ?line ok = ssh_cm:stop(CM),
    ?line ok = ssh_cm:stop_listener(Listener).

%%
%%
listen_compressed(doc) ->
    ["listen on localhost."];
listen_compressed(suite) ->
    [];
listen_compressed(Config) when list(Config) ->
    ?line SysDir = ?config(data_dir, Config),
    ?line UsrDir = ?config(priv_dir, Config),
    ?line Self = self(),
    ?line {ok, Listener} =
	ssh_cm:listen(fun() ->
			      spawn(fun() ->
					    resend_all(Self),
					    ok
				    end)
		      end, 9999,
		      [{system_dir, SysDir}]),
    ?line receive {} -> {} after 500 -> ok end,
    ?line {ok, CM} =
	ssh_cm:connect("localhost", [{user_dir, UsrDir},
				     {user_interaction, false},
				     silently_accept_hosts,
				     {compression, zlib}]),
    ?line ok = ssh_cm:stop(CM),
    ?line ok = ssh_cm:stop_listener(Listener).

resend_all(To) ->
    receive
	Msg ->
	    To ! Msg,
	    resend_all(To)
    end.

%%
%%
exec(doc) ->
    ["exec (shell) command on localhost."];
exec(suite) ->
    [];
exec(Config) when list(Config) ->
    ?line UsrDir = ?config(priv_dir, Config),
    ?line {ok, CM} =
	ssh_cm:connect("localhost", [{user_dir, UsrDir},
				     {user_interaction, false},
				     silently_accept_hosts]),
    ?line {ok, Channel} = ssh_cm:session_open(CM),
    ?line success = ssh_cm:exec(CM, Channel, "echo testing"),
    ?line Data = {ssh_cm, CM, {data, Channel, 0, <<"testing\n">>}},
    ?line EOF = {ssh_cm, CM, {eof, Channel}},
    ?line ExitStatus = {ssh_cm, CM, {exit_status, Channel, 0}},
    ?line A = expect_msg(),
    ?line B = expect_msg(),
    ?line C = expect_msg(),
    ?line one_of(A, B, C, Data, ExitStatus, EOF),
    ?line Closed = {ssh_cm, CM, {closed, Channel}},
    ?line Cl = expect_msg(),
    io:format("Cl ~p Closed ~p\n", [Cl, Closed]),
    ?line Closed = Cl,
    ?line ok = ssh_cm:stop(CM),
    ?line {ok, CM2} =
	ssh_cm:connect("localhost", [{user_dir, UsrDir},
				     {user_interaction, false},
				     silently_accept_hosts]),
    ?line {ok, Channel2} = ssh_cm:session_open(CM2),
    ?line D2 = case ssh_cm:setenv(CM2, Channel2, "ENV_TEST", "testing_setenv") of
		   success -> <<"tesing_setenv\n">>;
		   failure -> <<"\n">>
	       end,
    ?line success = ssh_cm:exec(CM2, Channel2, "echo $ENV_TEST"),
    ?line Data2 = {ssh_cm, CM2, {data, Channel2, 0, D2}},
    ?line EOF2 = {ssh_cm, CM2, {eof, Channel2}},
    ?line ExitStatus2 = {ssh_cm, CM2, {exit_status, Channel2, 0}},
    ?line D = expect_msg(),
    ?line E = expect_msg(),
    ?line F = expect_msg(),
    ?line one_of(D, E, F, Data2, ExitStatus2, EOF2),
    ?line Closed2 = {ssh_cm, CM2, {closed, Channel2}},
    ?line Closed2 = expect_msg(),
    ?line ok = ssh_cm:stop(CM2).

%%
%%

%% API TBD!!
%% adjust_window/3, detach/1,
%% tcpip_forward/3, cancel_tcpip_forward/3, direct_tcpip/5, direct_tcpip/7, 
%% i/1, info/1, open_pty/2,
%% recv_window/2, send/3, send/4, 
%% request_success/2, send_ack/3, send_ack/4, send_ack/5, send_eof/2,
%% send_window/2, 
%% open_pty/2, open_pty/6, open_pty/8,
%% set_user_ack/3,
%% signal/3, winch/4

api(doc) ->
    ["various api functions (connecting to localhost)"];
api(suite) ->
    [];
api(Config) when list(Config) ->
    ?line UsrDir = ?config(priv_dir, Config),
    %% connect/2
    ?line {ok, CM} =
	ssh_cm:connect("localhost", [{user_dir, UsrDir},
				     {user_interaction, false},
				     silently_accept_hosts]),
    %% session_open/3,
    ?line {ok, Channel} = ssh_cm:session_open(CM, 10000, 20000),
%%     %% renegotiate/1,
%%     ?line ok = ssh_cm:renegotiate(CM),
%%     %% renegotiate/2,
%%     ?line ok = ssh_cm:renegotiate(CM, []),
    %% subsystem/3,
    ?line success = ssh_cm:subsystem(CM, Channel, "sftp"),
    %% close/2,
    ?line ok = ssh_cm:close(CM, Channel),
    ?line ok = ssh_cm:stop(CM),
    ?line Self = self(),
    ?line F = fun() ->
		      {ok, CMx} =
			  ssh_cm:connect("localhost", [{user_dir, UsrDir},
						       {user_interaction, false},
						       silently_accept_hosts]),
		      Self ! {cm, self(), CMx},
		      receive
			  done -> ok
		      end
	      end,
    ?line Pid = spawn(F),
    ?line {cm, Pid, CM2} = receive
			       {cm, Pid1, CM21} -> {cm, Pid1, CM21}
			   after 1000 ->
				   timeout
			   end,
    %% session_open/1, (when not user)
    ?line {error, einval} = ssh_cm:session_open(CM2),
    %% set_user/3 (when not user)
    ?line {error, einval} = ssh_cm:set_user(CM2, Channel, Self),
    %% attach/1,
    ?line ok = ssh_cm:attach(CM2),
    %% session_open/1, (when user)
    ?line {ok, Channel2} = ssh_cm:session_open(CM2),
    %% set_user/3 (when user)
    ?line ok = ssh_cm:set_user(CM2, Channel2, self()),
    ?line ok = ssh_cm:close(CM2, Channel2),
    ?line ok = ssh_cm:stop(CM2),
    ?line Ref = erlang:monitor(process, Pid),
    ?line Pid ! done,
    ?line ok = receive
		   {'DOWN', Ref, process, Pid, normal} ->
		       ?line ok;
		   E ->
		       ?line E
	       after 2000 ->
		       ?line timeout
	       end.
    

%%
%%
shell(doc) ->
    ["shell on localhost"];
shell(suite) ->
    [];
shell(Config) when list(Config) ->
    ?line UsrDir = ?config(priv_dir, Config),
    ?line {ok, CM} =
	ssh_cm:connect("localhost", [{user_dir, UsrDir},
				     {user_interaction, false},
				     silently_accept_hosts]),
    ?line {ok, Channel} = ssh_cm:session_open(CM),
    ?line ok = ssh_cm:shell(CM, Channel),
    ?line flush_data_msgs(CM),
    ?line ssh_cm:send(CM, Channel, "echo testing\n"),
    ?line A = expect_msg(),
    ?line Data = {ssh_cm, CM, {data, Channel, 0, <<"testing\n">>}},
    ?line Data = A,
    ?line ssh_cm:send(CM, Channel, "exit\n"),
    ?line EOF = {ssh_cm, CM, {eof, Channel}},
    ?line ExitStatus = {ssh_cm, CM, {exit_status, Channel, 0}},
    ?line B = expect_msg(),
    ?line C = expect_msg(),
    ?line one_of(B, C, EOF, ExitStatus),
    ?line {ssh_cm, CM, {closed, Channel}} = expect_msg(),
    ?line ok = ssh_cm:stop(CM).


p_flush(Data) ->
    ?PRINT_FLUSH andalso ok == io:format("#Flushed \"~s\"\n", [Data]).

flush_data_msgs(CM) ->
    receive
	{ssh_cm, CM, {data, D}} ->
	    p_flush(D),
	    flush_data_msgs(CM)
    after ?FLUSH_TIMEOUT ->
	    ok
    end.
