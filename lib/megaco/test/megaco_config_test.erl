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

-module(megaco_config_test).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Top test case

all(suite) ->
    [
     config
    ].

config(suite) ->
    [];
config(Config) when list(Config) ->
    ?ACQUIRE_NODES(1, Config),
    Mid = fake_mid,
    ?VERIFY(ok, megaco:start()),
    
    ?VERIFY(0,	megaco:system_info(n_active_requests)),
    ?VERIFY(0,	megaco:system_info(n_active_replies)),
    ?VERIFY(0,	megaco:system_info(n_active_connections)),
    ?VERIFY([],	megaco:system_info(connections)),

    ?VERIFY([],    megaco:system_info(users)),
    ?VERIFY(ok,	   megaco:start_user(Mid, [])),
    ?VERIFY([Mid], megaco:system_info(users)),

    verify_user_defaults(Mid),
    nice_update_user(Mid),
    evil_update_user(Mid),

    ?VERIFY({'EXIT', _}, megaco:system_info(non_exist)),
    ?VERIFY({'EXIT', _}, megaco:user_info(non_exist, min_trans_id)),
    ?VERIFY({'EXIT', _}, megaco:user_info(Mid, non_exist)),
    ?VERIFY({error,{no_such_user, _}},
	    megaco:update_user_info(non_exist, min_trans_id, 1)),
    ?VERIFY({error, {bad_user_val, _, _, _}},
	    megaco:update_user_info(Mid, non_exist, 4711)),

    ?VERIFY({error, {user_already_exists, _}}, megaco:start_user(Mid, [])),
    ?VERIFY([Mid], megaco:system_info(users)),
    ?VERIFY(ok,	   megaco:stop_user(Mid)),
    ?VERIFY([],	   megaco:system_info(users)),

    ?VERIFY({error, {no_such_user, _}},	megaco:stop_user(Mid)),

    ?VERIFY({error,{already_started, _}}, megaco:start()),
    ?VERIFY(ok,	megaco:stop()),
    ?VERIFY({error,{not_started, _}},	megaco:stop()),
    ok.
    
verify_user_defaults(Mid) ->
    ?VERIFY([],	megaco:user_info(Mid, connections)),
    ?VERIFY(1,	megaco:user_info(Mid, min_trans_id)),
    ?VERIFY(infinity,	megaco:user_info(Mid, max_trans_id)),
    ?VERIFY(#megaco_incr_timer{}, megaco:user_info(Mid, request_timer)),
    ?VERIFY(infinity,	megaco:user_info(Mid, long_request_timer)),
    ?VERIFY(false,	megaco:user_info(Mid, auto_ack)),
    ?VERIFY(30000,	megaco:user_info(Mid, pending_timer)),
    ?VERIFY(30000,	megaco:user_info(Mid, reply_timer)),
    ?VERIFY(megaco_tcp,	megaco:user_info(Mid, send_mod)),
    ?VERIFY(megaco_pretty_text_encoder,	megaco:user_info(Mid, encoding_mod)),
    ?VERIFY([],	megaco:user_info(Mid, encoding_config)),
    ?VERIFY(1,	megaco:user_info(Mid, protocol_version)),
    ?VERIFY(undefined,	megaco:user_info(Mid, reply_data)),
    ?VERIFY(X when record(X, megaco_receive_handle),
		   megaco:user_info(Mid, receive_handle)).


-define(NICE_UPDATE(Mid, Key, Val), 
	?VERIFY(ok,  megaco:update_user_info(Mid, Key, Val)),
	?VERIFY(Val, megaco:user_info(Mid, Key))).

nice_update_user(Mid) ->
    Int = 3,
    IT = #megaco_incr_timer{max_retries = Int},

    ?NICE_UPDATE(Mid, min_trans_id, Int),
    ?NICE_UPDATE(Mid, max_trans_id, Int),
    ?NICE_UPDATE(Mid, max_trans_id, infinity),
    ?NICE_UPDATE(Mid, request_timer, Int),
    ?NICE_UPDATE(Mid, request_timer, infinity),
    ?NICE_UPDATE(Mid, request_timer, IT),
    ?NICE_UPDATE(Mid, long_request_timer, Int),
    ?NICE_UPDATE(Mid, long_request_timer, infinity),
    ?NICE_UPDATE(Mid, long_request_timer, IT),
    ?NICE_UPDATE(Mid, auto_ack, true),
    ?NICE_UPDATE(Mid, auto_ack, false),
    ?NICE_UPDATE(Mid, pending_timer, Int),
    ?NICE_UPDATE(Mid, pending_timer, infinity),
    ?NICE_UPDATE(Mid, pending_timer, IT),
    ?NICE_UPDATE(Mid, reply_timer, Int),
    ?NICE_UPDATE(Mid, reply_timer, infinity),
    ?NICE_UPDATE(Mid, reply_timer, IT),
    ?NICE_UPDATE(Mid, send_mod, an_atom),
    ?NICE_UPDATE(Mid, encoding_mod, an_atom),
    ?NICE_UPDATE(Mid, encoding_config, []),
    ?NICE_UPDATE(Mid, protocol_version, Int),
    ?NICE_UPDATE(Mid, reply_data, IT).

-define(EVIL_UPDATE(Mid, Key, Val),
	fun() ->
		OldVal = ?VERIFY(_, megaco:user_info(Mid, Key)),
		?VERIFY({error, {bad_user_val, _, _, _}},
			megaco:update_user_info(Mid, Key, Val)),
		?VERIFY(OldVal, megaco:user_info(Mid, Key))
	end()).

evil_update_user(Mid) ->
    NonInt = non_int,
    IT2 = #megaco_incr_timer{wait_for = NonInt},
    IT3 = #megaco_incr_timer{factor = NonInt},
    IT4 = #megaco_incr_timer{max_retries = NonInt},
    IT5 = #megaco_incr_timer{max_retries = non_infinity},

    ?EVIL_UPDATE(Mid, min_trans_id, NonInt),
    ?EVIL_UPDATE(Mid, max_trans_id, NonInt),
    ?EVIL_UPDATE(Mid, max_trans_id, non_infinity),
    ?EVIL_UPDATE(Mid, request_timer, NonInt),
    ?EVIL_UPDATE(Mid, request_timer, non_infinity),
    ?EVIL_UPDATE(Mid, request_timer, IT2),
    ?EVIL_UPDATE(Mid, request_timer, IT3),
    ?EVIL_UPDATE(Mid, request_timer, IT4),
    ?EVIL_UPDATE(Mid, request_timer, IT5),
    ?EVIL_UPDATE(Mid, long_request_timer, NonInt),
    ?EVIL_UPDATE(Mid, long_request_timer, non_infinity),
    ?EVIL_UPDATE(Mid, long_request_timer, IT2),
    ?EVIL_UPDATE(Mid, long_request_timer, IT3),
    ?EVIL_UPDATE(Mid, long_request_timer, IT4),
    ?EVIL_UPDATE(Mid, long_request_timer, IT5),
    ?EVIL_UPDATE(Mid, auto_ack, non_bool),
    ?EVIL_UPDATE(Mid, pending_timer, NonInt),
    ?EVIL_UPDATE(Mid, pending_timer, non_infinity),
    ?EVIL_UPDATE(Mid, pending_timer, IT2),
    ?EVIL_UPDATE(Mid, pending_timer, IT3),
    ?EVIL_UPDATE(Mid, pending_timer, IT4),
    ?EVIL_UPDATE(Mid, pending_timer, IT5),
    ?EVIL_UPDATE(Mid, reply_timer, NonInt),
    ?EVIL_UPDATE(Mid, reply_timer, non_infinity),
    ?EVIL_UPDATE(Mid, reply_timer, IT2),
    ?EVIL_UPDATE(Mid, reply_timer, IT3),
    ?EVIL_UPDATE(Mid, reply_timer, IT4),
    ?EVIL_UPDATE(Mid, reply_timer, IT5),
    ?EVIL_UPDATE(Mid, send_mod, {non_atom}),
    ?EVIL_UPDATE(Mid, encoding_mod, {non_atom}),
    ?EVIL_UPDATE(Mid, encoding_config, non_list),
    ?EVIL_UPDATE(Mid, protocol_version, NonInt).
