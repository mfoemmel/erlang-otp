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


-record(command, {id, desc, cmd, verify}).


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
    
    %% Nice values
    Int = 3,
    IT = #megaco_incr_timer{max_retries = Int},

    %% Evil values
    NonInt = non_int,
    IT2 = #megaco_incr_timer{wait_for = NonInt},
    IT3 = #megaco_incr_timer{factor = NonInt},
    IT4 = #megaco_incr_timer{max_retries = NonInt},
    IT5 = #megaco_incr_timer{max_retries = non_infinity},

    Commands = 
	[
	 %% Initial commands
	 initial_command( 1, "start", 
			  fun() -> megaco:start() end, ok),
	 initial_command( 2, "Verify no active requests", 
			  fun() -> megaco:system_info(n_active_requests) end,
			  0),
	 initial_command( 3, "Verify no active replies", 
			  fun() -> megaco:system_info(n_active_replies) end,
			  0),
	 initial_command( 4, "Verify no active connections", 
			  fun() -> 
				  megaco:system_info(n_active_connections) 
			  end,
			  0), 
	 initial_command( 5, "Verify no connections", 
			  fun() -> megaco:system_info(connections) end, []),
	 initial_command( 6, "Verify no users", 
			  fun() -> megaco:system_info(users) end, []), 
	 initial_command( 6, "Start user", 
			  fun() -> megaco:start_user(Mid, []) end, ok),


	 %% Verify user defaults
	 verify_user_default_command(Mid,  8, connections, []),
	 verify_user_default_command(Mid,  9, min_trans_id, 1), 
	 verify_user_default_command(Mid, 10, max_trans_id, infinity), 
	 verify_user_default_command(Mid, 11, request_timer, 
				          #megaco_incr_timer{}), 
	 verify_user_default_command(Mid, 12, long_request_timer, infinity), 
	 verify_user_default_command(Mid, 13, auto_ack, false), 
	 verify_user_default_command(Mid, 14, pending_timer, 30000), 
	 verify_user_default_command(Mid, 15, reply_timer, 30000), 
	 verify_user_default_command(Mid, 16, send_mod, megaco_tcp), 
	 verify_user_default_command(Mid, 17, encoding_mod, 
				          megaco_pretty_text_encoder), 
	 verify_user_default_command(Mid, 18, encoding_config, []), 
	 verify_user_default_command(Mid, 19, protocol_version, 1), 
	 verify_user_default_command(Mid, 20, reply_data, undefined), 
	 verify_user_default_command(Mid, 21, receive_handle, 
				     fun(H) when record(H, megaco_receive_handle) -> {ok, H};
					(R)  -> {error, R}
				     end), 


	 %% Nice update
	 nice_user_update_command(Mid, 22, min_trans_id, Int), 
	 nice_user_update_command(Mid, 23, max_trans_id, Int), 
	 nice_user_update_command(Mid, 24, max_trans_id, infinity), 
	 nice_user_update_command(Mid, 25, request_timer, Int), 
	 nice_user_update_command(Mid, 26, request_timer, infinity), 
	 nice_user_update_command(Mid, 27, request_timer, IT), 
	 nice_user_update_command(Mid, 28, long_request_timer, Int), 
	 nice_user_update_command(Mid, 29, long_request_timer, infinity), 
	 nice_user_update_command(Mid, 30, long_request_timer, IT), 
	 nice_user_update_command(Mid, 31, auto_ack, true), 
	 nice_user_update_command(Mid, 32, auto_ack, false), 
	 nice_user_update_command(Mid, 33, pending_timer, Int), 
	 nice_user_update_command(Mid, 34, pending_timer, infinity), 
	 nice_user_update_command(Mid, 35, pending_timer, IT), 
	 nice_user_update_command(Mid, 36, reply_timer, Int), 
	 nice_user_update_command(Mid, 37, reply_timer, infinity), 
	 nice_user_update_command(Mid, 38, reply_timer, IT), 
	 nice_user_update_command(Mid, 39, send_mod, an_atom), 
	 nice_user_update_command(Mid, 40, encoding_mod, an_atom), 
	 nice_user_update_command(Mid, 41, encoding_config, []), 
	 nice_user_update_command(Mid, 42, protocol_version, Int), 
	 nice_user_update_command(Mid, 43, reply_data, IT), 


	 %% Evil update
	 evil_user_update_command(Mid, 44, min_trans_id, NonInt), 
	 evil_user_update_command(Mid, 45, max_trans_id, NonInt), 
	 evil_user_update_command(Mid, 46, max_trans_id, non_infinity), 
	 evil_user_update_command(Mid, 47, request_timer, NonInt), 
	 evil_user_update_command(Mid, 48, request_timer, non_infinity), 
	 evil_user_update_command(Mid, 49, request_timer, IT2), 
	 evil_user_update_command(Mid, 50, request_timer, IT3), 
	 evil_user_update_command(Mid, 51, request_timer, IT4), 
	 evil_user_update_command(Mid, 52, request_timer, IT5), 
	 evil_user_update_command(Mid, 53, long_request_timer, NonInt), 
	 evil_user_update_command(Mid, 54, long_request_timer, non_infinity), 
	 evil_user_update_command(Mid, 55, long_request_timer, IT2), 
	 evil_user_update_command(Mid, 56, long_request_timer, IT3), 
	 evil_user_update_command(Mid, 57, long_request_timer, IT4), 
	 evil_user_update_command(Mid, 58, long_request_timer, IT5), 
	 evil_user_update_command(Mid, 59, auto_ack, non_bool), 
	 evil_user_update_command(Mid, 60, pending_timer, NonInt), 
	 evil_user_update_command(Mid, 61, pending_timer, non_infinity), 
	 evil_user_update_command(Mid, 62, pending_timer, IT2), 
	 evil_user_update_command(Mid, 63, pending_timer, IT3), 
	 evil_user_update_command(Mid, 64, pending_timer, IT4), 
	 evil_user_update_command(Mid, 65, pending_timer, IT5), 
	 evil_user_update_command(Mid, 66, reply_timer, NonInt), 
	 evil_user_update_command(Mid, 67, reply_timer, non_infinity), 
	 evil_user_update_command(Mid, 68, reply_timer, IT2), 
	 evil_user_update_command(Mid, 69, reply_timer, IT3), 
	 evil_user_update_command(Mid, 70, reply_timer, IT4), 
	 evil_user_update_command(Mid, 71, reply_timer, IT5), 
	 evil_user_update_command(Mid, 72, send_mod, {non_atom}), 
	 evil_user_update_command(Mid, 73, encoding_mod, {non_atom}), 
	 evil_user_update_command(Mid, 74, encoding_config, non_list), 
	 evil_user_update_command(Mid, 75, protocol_version, NonInt),


	 exit_command(76, "Verify non-existing system info", 
		      fun() -> megaco:system_info(non_exist) end),
	 exit_command(77, "Verify non-existing user user info", 
		      fun() -> megaco:user_info(non_exist, trans_id) end),
	 exit_command(78, "Verify non-existing user info", 
		      fun() -> megaco:user_info(Mid, non_exist) end),

	 error_command(79, 
		       "Try updating user info for non-existing user", 
		       fun() -> 
			       megaco:update_user_info(non_exist, trans_id, 1) 
		       end,
		       no_such_user, 2),
	 error_command(80, 
		       "Try updating non-existing user info", 
		       fun() -> 
			       megaco:update_user_info(Mid, trans_id, 4711) 
		       end,
		       bad_user_val, 4),
	 error_command(81, 
		       "Try start already started user", 
		       fun() -> 
			       megaco:start_user(Mid, []) 
		       end,
		       user_already_exists, 2),

	 command(82, "Verify started users", 
		 fun() -> megaco:system_info(users) end, [Mid]),
	 command(83, "Stop user", fun() -> megaco:stop_user(Mid) end, ok),
	 command(84, "Verify started users", 
		 fun() -> megaco:system_info(users) end, []),
	 error_command(85, "Try stop not started user",
		       fun() -> megaco:stop_user(Mid) end, no_such_user, 2),
	 error_command(86, "Try start megaco (it's already started)",
		       fun() -> megaco:start() end, already_started, 2),
	 command(87, "Stop megaco", fun() -> megaco:stop() end, ok),
	 error_command(88, "Try stop megaco (it's not running)",
		       fun() -> megaco:stop() end, not_started, 2)
	],

    
    exec(Commands).
    


exec([]) ->
    ok;
exec([#command{id     = No, 
	       desc   = Desc, 
	       cmd    = Cmd, 
	       verify = Verify}|Commands]) ->
    io:format("Executing command ~2w: ~s: ", [No, Desc]),
    case (catch Verify((catch Cmd()))) of
	{ok, OK} ->
	    io:format("ok => ~p~n", [OK]),
	    exec(Commands);
	{error, Reason} ->
	    io:format("error => ~p~n", [Reason]),
	    {error, {bad_result, No, Reason}};
	Error ->
	    io:format("exit => ~p~n", [Error]),
	    {error, {unexpected_result, No, Error}}
    end.

initial_command(No, Desc0, Cmd, VerifyVal) when is_function(Cmd) ->
    Desc = lists:flatten(io_lib:format("Initial - ~s", [Desc0])),
    command(No, Desc, Cmd, VerifyVal).
    
verify_user_default_command(Mid, No, Key, Verify) ->
    Desc = lists:flatten(io_lib:format("Defaults - Verify ~w", [Key])),
    Cmd = fun() -> megaco:user_info(Mid, Key) end,
    command(No, Desc, Cmd, Verify).
		     
nice_user_update_command(Mid, No, Key, Val) ->
    Desc = lists:flatten(io_lib:format("Nice - Update ~w", [Key])),
    Cmd = fun() -> megaco:update_user_info(Mid, Key, Val) end,
    Verify = fun(ok) ->
		     case (catch megaco:user_info(Mid, Key)) of
			 {'EXIT', R} ->
			     {error, {value_retreival_failed, R}};
			 Val ->
			     {ok, Val};
			 Invalid ->
			     {error, {value_update_failed, Val, Invalid}}
		     end;
		(R)  -> 
		     {error, R}
	     end,
    command(No, Desc, Cmd, Verify).


evil_user_update_command(Mid, No, Key, Val) ->
    Desc = lists:flatten(io_lib:format("Evil: Update ~w", [Key])),
    Cmd = fun() ->
		  case (catch megaco:user_info(Mid, Key)) of
		      {'EXIT', R} ->
			  {{error, {old_value_retreival_failed, R}}, 
			   ignore};
		      OldVal ->
			  {OldVal,
			   (catch megaco:update_user_info(Mid, Key, Val))}
		  end
	  end,
    Verify = fun({{error, _} = Error, ignore}) ->
		     Error;
		({OldVal, {error, {bad_user_val, _, _, _}}}) ->
		     case (catch megaco:user_info(Mid, Key)) of
			 {'EXIT', R} ->
			     {error, {value_retreival_failed, R}};
			 OldVal ->
			     {ok, OldVal};
			 Invalid ->
			     {error, {value_update_failed, OldVal, Invalid}}
		     end;
		(R) ->
		     {error, R}
	     end,
    command(No, Desc, Cmd, Verify).

exit_command(No, Desc, Cmd) when is_function(Cmd) ->    
    Verify = fun({'EXIT', _} = E) ->
		     {ok, E};
		(R) ->
		     {error, R}
	     end,
    command(No, Desc, Cmd, Verify).

error_command(No, Desc, Cmd, MainReason, TS) when is_function(Cmd) ->
    Verify = fun({error, Reason}) ->
		     io:format("verify -> Reason: ~n~p~n", [Reason]), 
		     case Reason of
			 {MainReason, _} when TS == 2 ->
			     {ok, MainReason};
			 {MainReason, _, _, _} when TS == 4 ->
			     {ok, MainReason};
			 _ ->
			     {error, Reason}
		     end;
		(R) ->
		     {error, R}
	     end,
    command(No, Desc, Cmd, Verify).

command(No, Desc, Cmd, Verify) when is_integer(No) and is_list(Desc) and 
                                    is_function(Cmd) and is_function(Verify) ->
    #command{id     = No, 
	     desc   = Desc,
	     cmd    = Cmd,
	     verify = Verify};
command(No, Desc, Cmd, VerifyVal) when is_integer(No) and is_list(Desc) and 
				       is_function(Cmd) ->
    Verify = fun(Val) ->
		     case Val of
			 VerifyVal ->
			     {ok, Val};
			 _ ->
			     {error, Val}
		     end
	     end,
    #command{id     = No, 
	     desc   = Desc,
	     cmd    = Cmd,
	     verify = Verify}.
    
