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
%%----------------------------------------------------------------------
%% Purpose: Test application config
%%----------------------------------------------------------------------

-module(ssh_SUITE).

-compile(export_all).

-include("ssh_test_lib.hrl").

t()     -> ssh_test_lib:t(?MODULE).
t(Case) -> ssh_test_lib:t({?MODULE, Case}).

%% Test server callbacks
init_per_testcase(Case, Config) ->
    ssh_test_lib:init_per_testcase(Case, Config).

fin_per_testcase(Case, Config) ->
    ssh_test_lib:fin_per_testcase(Case, Config).

init() ->
    process_flag(trap_exit, true),
    ssh_test_lib:flush().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Top test case

all(suite) ->
    [
     app_test
    ].

app_test(suite) ->
    [{ssh_app_test, all}].


