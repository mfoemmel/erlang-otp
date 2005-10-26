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
-module(ssh_userauth_SUITE).
-include("test_server.hrl").
%%-include_lib("ssh/include/ssh.hrl").

% Default timetrap timeout (set in init_per_testcase).
-define(default_timeout, ?t:minutes(1)).
-define(application, ssh).

% Test server specific exports
-export([all/1]).
-export([init_per_testcase/2, fin_per_testcase/2]).
-export([init_per_suite/1, end_per_suite/1]).

% Test cases must be exported.
-export([publickey_rsa/1,
	 publickey_dsa/1,
	 password_opt/1,
	 user_passwords_opt/1,
	 %% publickey_badrsa/1,
	 password_badpw/1]).

%% import some utils
-import(ssh_test_lib, [copyfile/3]).

all(suite) ->
    case os:find_executable("ssh") of
	false -> {skip, "SSH not installed on host"};
	_ ->
	    [publickey_rsa,
	     publickey_dsa,
	     password_opt,
	     user_passwords_opt,
	     password_badpw]
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
    %% ?line {ok, _} = get_id_keys(?config(priv_dir, Config)),
    Config.

end_per_suite(Config) ->
    ?line Dir = ?config(priv_dir, Config),
    %% ?line ssh_test_lib:remove_id_keys(Dir),
    ?line file:delete(filename:join(Dir, "known_hosts")),
    ?line Config.

%%
%%
publickey_rsa(doc) ->
    ["Validate using rsa publickey."];
publickey_rsa(suite) ->
    [];
publickey_rsa(Config) when list(Config) ->
    ?line SrcDir = filename:join(os:getenv("HOME"), ".ssh"),
    ?line UsrDir = ?config(priv_dir, Config),
    ?line case copyfile(SrcDir, UsrDir, "id_rsa") of
	      {ok, _} ->
		  ?line {ok, CM} =
		      ssh_cm:connect("localhost", [{user_dir, UsrDir},
						   silently_accept_hosts,
						   {user_interaction, false}]),
		  ?line {ok, Channel} = ssh_cm:session_open(CM),
		  ?line ok = ssh_cm:close(CM, Channel),
		  ?line ok = ssh_cm:stop(CM),
		  ?line ok = file:delete(filename:join(UsrDir, "id_rsa"));
	      {error, enoent} ->
		  ?line {skip, "no ~/.ssh/id_rsa"}
	  end.

%%
%%
publickey_dsa(doc) ->
    ["Validate using dsa publickey."];
publickey_dsa(suite) ->
    [];
publickey_dsa(Config) when list(Config) ->
    ?line SrcDir = filename:join(os:getenv("HOME"), ".ssh"),
    ?line UsrDir = ?config(priv_dir, Config),
    ?line case copyfile(SrcDir, UsrDir, "id_dsa") of
	      {ok, _} ->
		  ?line {ok, CM} =
		      ssh_cm:connect("localhost", [{user_dir, UsrDir},
						   silently_accept_hosts,
						   {public_key_alg, ssh_dsa},
						   {user_interaction, false}]),
		  ?line {ok, Channel} = ssh_cm:session_open(CM),
		  ?line ok = ssh_cm:close(CM, Channel),
		  ?line ok = ssh_cm:stop(CM),
		  ?line ok = file:delete(filename:join(UsrDir, "id_dsa"));
	      {error, enoent} ->
		  ?line {skip, "no ~/.ssh/id_dsa"}
	  end.

%%
%%
password_badpw(doc) ->
    ["don't validate with bad password"];
password_badpw(suite) ->
    [];
password_badpw(Config) when list(Config) ->
    ?line UsrDir = ?config(priv_dir, Config),
    ?line {error, _} =
	ssh_cm:connect("localhost", [{user_dir, UsrDir},
				     {user_interaction, false},
				     {password, "bad"}]),
    ?line ok.

password_opt(doc) ->
    ["validate to server that uses the 'password' option"];
password_opt(suite) ->
    [];
password_opt(Config) when list(Config) ->
    ?line UsrDir = ?config(data_dir, Config),	% to make sure we don't use
    ?line SysDir = ?config(data_dir, Config),	% public-key-auth
    ?line {ok, Listener} =
	ssh_sshd:listen(9999, [{system_dir, SysDir},
			       {password, "morot"}]),
    ?line {ok, Pid} =
	ssh_cm:connect("localhost", 9999, [{user_dir, UsrDir},
					   {user, "vego"},
					   {password, "morot"},
					   {allow_user_interaction, false},
					   silently_accept_hosts]),
    ?line ok = ssh_cm:stop_listener(Listener),
    ?line ok = ssh_cm:stop(Pid).

user_passwords_opt(doc) ->
    ["validate to server that uses the 'user_passwords' option"];
user_passwords_opt(suite) ->
    [];
user_passwords_opt(Config) when list(Config) ->
    ?line UsrDir = ?config(data_dir, Config),	% to make sure we don't use
    ?line SysDir = ?config(data_dir, Config),	% public-key-auth
    ?line {ok, Listener} =
	ssh_sshd:listen(9999, [{system_dir, SysDir},
			       {user_passwords, [{"vego", "morot"}]}]),
    ?line {ok, Pid} =
	ssh_cm:connect("localhost", 9999, [{user_dir, UsrDir},
					   {user, "vego"},
					   {password, "morot"},
					   {allow_user_interaction, false},
					   silently_accept_hosts]),
    ?line ok = ssh_cm:stop_listener(Listener),
    ?line ok = ssh_cm:stop(Pid).

