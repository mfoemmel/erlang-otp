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
%% Dynamic Driver Loader and Linker
%%
%% Interface for dynamic library/shared object driver loader/linker.
%% Provides methods for loading, unloading and listing drivers.

-module(erl_ddll).

-export([load_driver/2, load/2, 
	 unload_driver/1, unload/1, reload/2, reload_driver/2, 
	 format_error/1,info/1,info/0, start/0, stop/0]).

start() ->
    {error,{already_started,undefined}}.

stop() ->
    ok.

load_driver(Path, Driver) ->
    do_load_driver(Path, Driver, [{driver_options,[kill_ports]}]).

load(Path, Driver) ->
    do_load_driver(Path, Driver, []).

do_load_driver(Path, Driver, DriverFlags) ->
    case erl_ddll:try_load(Path, Driver,[{monitor,pending_driver}]++DriverFlags) of
	{error, inconsistent} ->
	    {error,bad_driver_name}; % BC 
	{error, What} ->
	    {error,What};
	{ok, already_loaded} ->
	    ok;
	{ok,loaded} ->
	    ok;
	{ok, pending_driver, Ref} ->
	    receive
		{'DOWN', Ref, driver, _, load_cancelled} ->
		    {error, load_cancelled};
		{'UP', Ref, driver, _, permanent} ->
		    {error, permanent}; 
		{'DOWN', Ref, driver, _, {load_failure, Failure}} ->
		    {error, Failure};
		{'UP', Ref, driver, _, loaded} ->
		    ok
	    end
    end.

do_unload_driver(Driver,Flags) ->
    case erl_ddll:try_unload(Driver,Flags) of
	{error,What} ->
	    {error,What};
	{ok, pending_process} ->
	    ok;
	{ok, unloaded} ->
	    ok;
	{ok, pending_driver} ->
	    ok;
	{ok, pending_driver, Ref} ->
	    receive
		{'UP', Ref, driver, _, permanent} ->
		    {error, permanent}; 
		{'UP', Ref, driver, _, unload_cancelled} ->
		    ok;
		{'DOWN', Ref, driver, _, unloaded} ->
		    ok
	    end
    end.

unload_driver(Driver) ->
    do_unload_driver(Driver,[{monitor,pending_driver},kill_ports]).

unload(Driver) ->
    do_unload_driver(Driver,[]).


reload(Path,Driver) ->
    do_load_driver(Path, Driver, [{reload,pending_driver}]).

reload_driver(Path,Driver) ->
    do_load_driver(Path, Driver, [{reload,pending_driver},
				  {driver_options,[kill_ports]}]).			    
format_error(Code) ->
    case Code of
	% This is the only error code returned only from erlang code...
	% 'permanent' has a translation in the emulator, even though the erlang code uses it to...
	load_cancelled ->
	    "Loading was cancelled from other process";
	_ ->
	    erl_ddll:format_error_int(Code)
    end.
    
info(Driver) ->
    [{processes, erl_ddll:info(Driver,processes)},
     {driver_options, erl_ddll:info(Driver,driver_options)},
     {port_count, erl_ddll:info(Driver,port_count)},
     {linked_in_driver, erl_ddll:info(Driver,linked_in_driver)},
     {permanent, erl_ddll:info(Driver,permanent)},
     {awaiting_load,  erl_ddll:info(Driver,awaiting_load)},
     {awaiting_unload, erl_ddll:info(Driver,awaiting_unload)}].

info() ->
    {ok,DriverList} = erl_ddll:loaded_drivers(),
    [{X,Y} || X <- DriverList,
	       Y <- [catch info(X)],
	       is_list(Y), not lists:member({linked_in_driver,true},Y)]. 
