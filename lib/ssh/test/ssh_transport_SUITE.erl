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
-module(ssh_transport_SUITE).
-include("test_server.hrl").

% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).
-define(application, ssh).

% Test server specific exports
-export([all/1]).
-export([init_per_testcase/2, fin_per_testcase/2]).

% Test cases must be exported.
-export([connect/1,
	 listen/1]).

all(suite) ->
    case os:find_executable("ssh") of
	false -> {skip, "SSH not installed on host"};
	_ -> [connect, listen]
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

%% copyfile(SrcDir, DstDir, FileName) ->
%%     file:copy(filename:join(SrcDir, FileName),
%% 	      filename:join(DstDir, FileName)).

%% %%
%% %%
%% init_first(doc) ->
%%     ["init. copy user keys to priv dir"];
%% init_first(suite) ->
%%     [];
%% init_first(Config) when list(Config) ->
%%     ?line DstDir = ?config(priv_dir, Config),
%%     ?line SrcDir = filename:join(os:getenv("HOME"), ".ssh"),
%%     ?line {ok, _} = copyfile(SrcDir, DstDir, "id_rsa"),
%%     ?line {ok, _} = copyfile(SrcDir, DstDir, "id_rsa.pub"),
%%     ok.

%%
%%
connect(doc) ->
    ["connect to localhost."];
connect(suite) ->
    [];
connect(Config) when list(Config) ->
    ?line Dir = ?config(data_dir, Config),
    ?line {ok, SSH} =
	ssh_transport:connect("localhost", [{user_dir, Dir},
					    {user_interaction, false},
					    silently_accept_hosts]),
    ?line ok = ssh_transport:close(SSH).

listen(doc) ->
    ["listen on localhost."];
listen(suite) ->
    [];
listen(Config) when list(Config) ->
    ?line Dir = ?config(data_dir, Config),
    ?line Self = self(),
    ?line {ok, Listener} = 
	ssh_transport:listen(fun(_SSH) ->
				     spawn(fun() ->
						   resend_all(Self),
						   ok
					   end)
			     end, 9999,
			     [{user_dir, Dir}, {system_dir, Dir}]),
    ?line {ok, SSH} =
	ssh_transport:connect("localhost", 9999,
			      [{user_dir, Dir},
			       {user_interaction, false}, silently_accept_hosts]),
    ?line ok = ssh_transport:close(SSH),
    ?line {ssh_msg, _Pid, {ssh_msg_disconnect,10,"Connection closed",[]}} =
	expect_msg(),
    ?line ok = ssh_transport:stop_listener(Listener).

resend_all(To) ->
    receive
	Msg ->
	    To ! Msg,
	    resend_all(To)
    end.

expect_msg() ->
    receive
	Msg ->
	    Msg
    after 1000 ->
	    timeout
    end.
