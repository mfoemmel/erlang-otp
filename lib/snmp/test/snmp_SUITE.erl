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
-module(snmp_SUITE).

-export([all/1, 
	 init_per_testcase/2, fin_per_testcase/2
	]).

-export([app_test/1,
	 appup_test/1,
	 snmp_test/1]).

%%
%% -----
%%

init_per_testcase(Case, Config) when list(Config) ->
    Config.

fin_per_testcase(Case, Config) when list(Config) ->
    Config.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Top test case

all(doc) ->
    ["Test suites for the snmp application.",
     "There are five different test sub-suites."];

all(suite) ->
    [
     app_test,
     appup_test,
     snmp_test
    ].

app_test(suite) ->
    [{snmp_app_test, all}].


appup_test(suite) ->
    [{snmp_appup_test, all}].


snmp_test(suite) ->
    [{snmp_test, all}].


