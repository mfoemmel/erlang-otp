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

-module(megaco_SUITE).

-compile(export_all).

-include("megaco_test_lib.hrl").
-include_lib("megaco/include/megaco.hrl").

t()     -> megaco_test_lib:t(?MODULE).
t(Case) -> megaco_test_lib:t({?MODULE, Case}).

%% Test server callbacks
init_per_testcase(Case, Config) ->
    megaco_test_lib:init_per_testcase(Case, Config).

fin_per_testcase(Case, Config) ->
    megaco_test_lib:fin_per_testcase(Case, Config).

init() ->
    process_flag(trap_exit, true),
    megaco_test_lib:flush().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Top test case

all(suite) ->
    [
     app_test,
     config,
     udp,
     tcp,
     examples,
     %%call_flow,
     digit_map,
     mess,
     measure,
     binary_term_id,
     codec,
     sdp
    ].

app_test(suite) ->
    [{megaco_app_test, all}].

config(suite) ->
    [{megaco_config_test, all}].

call_flow(suite) ->
    [{megaco_call_flow_test, all}].

digit_map(suite) ->
    [{megaco_digit_map_test, all}].

mess(suite) ->
    [{megaco_mess_test, all}].

udp(suite) ->
    [{megaco_udp_test, all}].

tcp(suite) ->
    [{megaco_tcp_test, all}].

examples(suite) ->
    [{megaco_examples_test, all}].

measure(suite) ->
    [{megaco_measure_test, all}].

binary_term_id(suite) ->
    [{megaco_binary_term_id_test, all}].

codec(suite) ->
    [{megaco_codec_test, all}].

sdp(suite) ->
    [{megaco_sdp_test, all}].

