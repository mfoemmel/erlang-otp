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
-module(percept_SUITE).
-include("test_server.hrl").

%% Test server specific exports
-export([all/1]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([libgd/1, fini/1]).

%% Test cases
-export([
	profile/1,
	analyze/1,
	webserver/1
	]).

%% Default timetrap timeout (set in init_per_testcase)
-define(default_timeout, ?t:minutes(1)).

init_per_suite(Config) when is_list(Config) ->
    Config.

end_per_suite(Config) when is_list(Config) ->
    Config.

init_per_testcase(_Case, Config) ->
    Dog = ?t:timetrap(?default_timeout),
    [{max_size, 300}, {watchdog,Dog} | Config].

end_per_testcase(_Case, Config) ->
    Dog = ?config(watchdog, Config),
    ?t:timetrap_cancel(Dog),
    ok.

all(suite) ->
    % Test cases
    [	{conf, libgd, [
    	webserver,
	profile,
	analyze
	], fini}
    ].

libgd(Config) when is_list(Config) ->
    case has_libgd() of
	true ->
	    Config;
	_ ->
	    {skip, "egd is not available without libgd"}
    end.

fini(Config) when is_list(Config) ->
    Config.

%%----------------------------------------------------------------------
%% Tests
%%----------------------------------------------------------------------

webserver(suite) ->
    [];
webserver(doc) ->
    ["Percept webserver test."];
webserver(Config) when is_list(Config) ->
    % Explicit start inets?
    ?line {started, _, Port} = percept:start_webserver(),
    ?line ok = percept:stop_webserver(Port), 
    ?line application:stop(inets),
    ok.

profile(suite) ->
    [];
profile(doc) ->
    ["Percept profile test."];
profile(Config) when is_list(Config) ->
    Path = ?config(data_dir, Config),
    File = filename:join([Path,"profile_test.dat"]),
    ?line {ok, _} = percept:profile(File, [procs]),
    ipc_tree:go(7),
    ?line ok = percept:stop_profile(),
    ok.

analyze(suite) ->
    [];
analyze(doc) ->
    ["Percept profile test."];
analyze(Config) when is_list(Config) ->
    Path = ?config(data_dir, Config),
    File = filename:join([Path,"profile_test.dat"]),
    ?line ok = percept:analyze(File),
    
    ok.

%%----------------------------------------------------------------------
%% Auxiliary tests
%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Auxiliary
%%----------------------------------------------------------------------

has_libgd() ->
    case code:priv_dir(percept) of
	{error, _} -> false;
	Path ->
	    Arch = erlang:system_info(system_architecture),
	    LibPath = filename:join([Path, "lib"]),
	    OpenPath = filename:join([LibPath, Arch ]),

	    Driver = "egd_drv",

	    case erl_ddll:load_driver(LibPath, Driver) of
	    	ok -> true;
		{error, already_loaded} -> true;
		{error, _} -> 
		    case erl_ddll:load_driver(OpenPath, Driver) of
			ok -> true;
			{error, already_loaded} -> true;
			_ -> false
		    end
	    end
    end.
