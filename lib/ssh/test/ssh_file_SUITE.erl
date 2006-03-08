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
-module(ssh_file_SUITE).
-include("test_server.hrl").

% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).
-define(application, ssh).

% Test server specific exports
-export([all/1]).
-export([init_per_testcase/2, fin_per_testcase/2]).
-export([init_per_suite/1, end_per_suite/1]).

% Test cases must be exported.
-export([known_hosts/1,
	 %% 	 id_dsa/1,
	 authorized_keys/1]).

all(suite) ->
    case os:find_executable("ssh") of
	false -> {skip, "SSH not installed on host"};
	_ -> 
	    [known_hosts,
	     authorized_keys]
    end.

init_per_testcase(_Case, Config) ->
    ?line Dog=test_server:timetrap(?default_timeout),
    [{watchdog, Dog}|Config].
fin_per_testcase(_Case, Config) ->
    Dog=?config(watchdog, Config),
    test_server:timetrap_cancel(Dog),
    ok.

%%
%%
init_per_suite(Config) ->
    ?line DstDir = ?config(priv_dir, Config),
    ?line {ok, _} = ssh_test_lib:get_id_keys(DstDir),
    ?line Config.

end_per_suite(Config) ->
    ?line Dir = ?config(priv_dir, Config),
    ?line ssh_test_lib:remove_id_keys(Dir),
    ?line file:delete(filename:join(Dir, "known_hosts")),
    ?line Config.

%%
%%
known_hosts(doc) ->
    ["check that known_hosts is updated correctly"];
known_hosts(suite) ->
    [];
known_hosts(Config) when list(Config) ->
    ?line UsrDir = ?config(priv_dir, Config),
    ?line KnownHosts = filename:join(UsrDir, "known_hosts"),
    ?line file:delete(KnownHosts),
    ?line {error, enoent} = file:read_file(KnownHosts),
    ?line {ok, CM} =
	ssh_cm:connect("localhost", [{user_dir, UsrDir},
				     {user_interaction, false},
				     silently_accept_hosts]),
    ?line {ok, Channel} = ssh_cm:session_open(CM, infinity),
    ?line ok = ssh_cm:close(CM, Channel),
    ?line ok = ssh_cm:stop(CM),
    ?line {ok, Binary} = file:read_file(KnownHosts),
    ?line Lines = string:tokens(binary_to_list(Binary), "\n"),
    ?line [Line] = Lines,
    ?line {ok, Hostname} = inet:gethostname(),
    ?line [HostAndIp, Alg, _KeyData] = string:tokens(Line, " "),
    ?line [Hostname, _Ip] = string:tokens(HostAndIp, ","),
    ?line "ssh-" ++ _ = Alg,
    ?line ok.

%%
%%
%% id_dsa(doc) ->
%%     ["check that id_dsa is read correctly"];
%% id_dsa(suite) ->
%%     [];
%% id_dsa(Config) when list(Config) ->
%%     ?line UsrDir = ?config(priv_dir, Config),
%%     ?line Rsa = filename:join(UsrDir, "id_rsa"),
%%     %% Make sure rsa files are not used
%%     ?line file:rename(Rsa, Rsa++"xxx"),
%%     ?line file:rename(Rsa++".pub", Rsa++".pubxxx"),
%%     %% Get the dsa files
%%     ?line SrcDir = filename:join(os:getenv("HOME"), ".ssh"),    
%%     ?line copyfile(SrcDir, UsrDir, "id_dsa"),
%%     ?line copyfile(SrcDir, UsrDir, "id_dsa.pub"),
%%     ?line {ok, CM} =
%% 	ssh_cm:connect("localhost", [{user_dir, UsrDir},
%% 				     {io_cb, ssh_no_io},
%% 				     silently_accept_hosts,
%% 				     {public_key_alg, ssh_dsa}]),
%%     ?line {ok, Channel} = ssh_cm:session_open(CM),
%%     ?line ok = ssh_cm:close(CM, Channel),
%%     ?line ssh_cm:stop(CM),
%%     ?line file:rename(Rsa++"xxx", Rsa),
%%     ?line file:rename(Rsa++".pubxxx", Rsa++".pub"),
%%     ?line ok.


%%
%%
authorized_keys(doc) ->
    ["check that autorized keys"];
authorized_keys(suite) ->
    [];
authorized_keys(Config) when list(Config) ->
    ?line io:format("TBD!!\n"),
%%     ?line Dir = ?config(data_dir, Config),
%%     ?line KnownHosts = filename:join(Dir, "known_hosts"),
%%     ?line file:delete(KnownHosts),
%%     ?line {error, enoent} = file:read_file(KnownHosts),
%%     ?line {ok, CM} =
%% 	ssh_cm:connect("localhost", [{user_dir, Dir},
%% 				     {io_cb, ssh_no_io},
%% 				     silently_accept_hosts]),
%%     ?line {ok, Channel} = ssh_cm:session_open(CM),
%%     ?line ok = ssh_cm:close(CM, Channel),
%%     ?line ssh_cm:stop(CM),
%%     ?line {ok, Binary} = file:read_file(KnownHosts),
%%     ?line Lines = string:tokens(binary_to_list(Binary), "\n"),
%%     ?line [Line] = Lines,
%%     ?line {ok, Hostname} = inet:gethostname(),
%%     ?line [HostAndIp, Alg, _KeyData] = string:tokens(Line, " "),
%%     ?line [Hostname, _Ip] = string:tokens(HostAndIp, ","),
%%     ?line "ssh-" ++ _ = Alg,
    ?line ok.

