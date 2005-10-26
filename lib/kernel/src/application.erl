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
-module(application).

-export([start/1, start/2, start_boot/1, start_boot/2, stop/1, 
	 load/1, load/2, unload/1, takeover/2,
	 which_applications/0, which_applications/1,
	 loaded_applications/0, permit/2]).
-export([set_env/3, set_env/4, unset_env/2, unset_env/3]).
-export([get_env/1, get_env/2, get_all_env/0, get_all_env/1]).
-export([get_key/1, get_key/2, get_all_key/0, get_all_key/1]).
-export([get_application/0, get_application/1, info/0]).
-export([start_type/0]).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{start,2},{stop,1}];
behaviour_info(_Other) ->
    undefined.

%%%-----------------------------------------------------------------
%%% This module is API towards application_controller and
%%% application_master.
%%%-----------------------------------------------------------------
load(Application) ->
    load(Application, []).

load(Application, DistNodes) ->
    case application_controller:load_application(Application) of
	ok when DistNodes /= [] ->
	    AppName = get_appl_name(Application),
	    case dist_ac:load_application(AppName, DistNodes) of
		ok ->
		    ok;
		{error, R} ->
		    application_controller:unload_application(AppName),
		    {error, R}
	    end;
	Else ->
	    Else
    end.

unload(Application) ->
    application_controller:unload_application(Application).

start(Application) ->
    start(Application, temporary).

start(Application, RestartType) ->
    case load(Application) of
	ok ->
	    Name = name(Application),
	    application_controller:start_application(Name, RestartType);
	{error, {already_loaded, Name}} ->
	    application_controller:start_application(Name, RestartType);
	Error ->
	    Error
    end.

start_boot(Application) ->
    start_boot(Application, temporary).

start_boot(Application, RestartType) ->
    application_controller:start_boot_application(Application, RestartType).


takeover(Application, RestartType) ->
    dist_ac:takeover_application(Application, RestartType).

permit(Application, Bool) ->
    case Bool of
	true -> ok;
	false -> ok;
	Bad -> exit({badarg, {?MODULE, permit, [Application, Bad]}})
    end,
    case application_controller:permit_application(Application, Bool) of
	distributed_application ->
	    dist_ac:permit_application(Application, Bool);
	{distributed_application, only_loaded} ->
	    dist_ac:permit_only_loaded_application(Application, Bool);
	LocalResult ->
	    LocalResult
    end.


name({application, Name, _}) -> Name;
name(Name) -> Name.

stop(Application) ->
    application_controller:stop_application(Application).

which_applications() ->
    application_controller:which_applications().
which_applications(infinity) ->
    application_controller:which_applications(infinity);
which_applications(Timeout) when is_integer(Timeout), Timeout>=0 ->
    application_controller:which_applications(Timeout).

loaded_applications() -> application_controller:loaded_applications().

info() -> application_controller:info().

set_env(Application, Key, Val) -> 
    application_controller:set_env(Application, Key, Val).
set_env(Application, Key, Val, infinity) ->
    application_controller:set_env(Application, Key, Val, infinity);
set_env(Application, Key, Val, Timeout) when is_integer(Timeout), Timeout>=0 ->
    application_controller:set_env(Application, Key, Val, Timeout).

unset_env(Application, Key) -> 
    application_controller:unset_env(Application, Key).
unset_env(Application, Key, infinity) ->
    application_controller:unset_env(Application, Key, infinity);
unset_env(Application, Key, Timeout) when is_integer(Timeout), Timeout>=0 ->
    application_controller:unset_env(Application, Key, Timeout).

get_env(Key) -> application_controller:get_pid_env(group_leader(), Key).
get_env(Application, Key) -> application_controller:get_env(Application, Key).

get_all_env() -> application_controller:get_pid_all_env(group_leader()).
get_all_env(Application) -> application_controller:get_all_env(Application).

get_key(Key) -> application_controller:get_pid_key(group_leader(), Key).
get_key(Application, Key) -> application_controller:get_key(Application, Key).

get_all_key() -> application_controller:get_pid_all_key(group_leader()).
get_all_key(Application) -> application_controller:get_all_key(Application).

get_application() -> application_controller:get_application(group_leader()).
get_application(Pid) when pid(Pid) ->
    case process_info(Pid, group_leader) of
	{group_leader, Gl} ->
	    application_controller:get_application(Gl);
	undefined ->
	    undefined
    end;
get_application(Module) when atom(Module) ->
    application_controller:get_application_module(Module).

start_type() ->
    application_controller:start_type(group_leader()).


%% Internal
get_appl_name(Name) when atom(Name) -> Name;
get_appl_name({application, Name, _}) when atom(Name) -> Name.
