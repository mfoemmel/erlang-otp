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
%% Simple interface for dynamic library/shared object driver loader/linker.
%% Provides methods for loading, unloading and listing drivers.
%%
%% XXX To do:
%%  - When starting, should place itself under a supervisor.
%%  - After restart, must find out which drivers are loaded and
%%    which ports use them.  Alternative, if killed, should take
%%    down the whole OTP.

-module(erl_ddll).

-behaviour(gen_server).

-export([load_driver/2,unload_driver/1,loaded_drivers/0, format_error/1]).
-export([start/0, start_link/0, stop/0]).

%% Internal exports, call-back functions.
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2]).

%% Defines for ddll_drv.

-define(LOAD_DRIVER,$l).
-define(UNLOAD_DRIVER,$u).
-define(GET_DRIVERS,$g).

%%% --------------------------------------------------------
%%% Interface Functions. 
%%% --------------------------------------------------------

start() ->
    gen_server:start({local,ddll_server}, erl_ddll, [], []).

start_link() ->
    gen_server:start_link({local,ddll_server}, erl_ddll, [], []).

init([]) ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, ddll}, []),
    {ok, {Port, []}}.

load_driver(Path, Driver) when atom(Path) ->
    load_driver(atom_to_list(Path), Driver);
load_driver(Path, Driver) when atom(Driver) ->
    load_driver(Path, atom_to_list(Driver));
load_driver(Path, Driver) when list(Path), list(Driver) ->
    FullName = filename:join(Path, Driver),
    req({load_driver, FullName, Driver}).

unload_driver(Path) when atom(Path) ->
    unload_driver(atom_to_list(Path));
unload_driver(Path) when list(Path) ->
    req({unload_driver, Path}).

stop() ->
    req(stop).

loaded_drivers() ->
    req(loaded_drivers).

format_error(already_loaded) -> "Driver already loaded";
format_error({open_error, Message}) -> "Open failed: " ++ Message;
format_error({close_error, Message}) -> "Close failed: " ++ Message;
format_error(no_driver_init) -> "No driver_init() function";
format_error(driver_init_failed) -> "Function driver_init() failed";
format_error(bad_driver_name) -> "Driver name doesn't match filename";
format_error(linked_in_driver) -> "Cannot unload linked-in driver";
format_error(driver_in_use) -> "Driver in use";
format_error(finish_failed) -> "Finish function failed";
format_error(not_loaded) -> "Driver not loaded";
format_error(not_loaded_by_this_process) -> "Driver not loaded by this process";
format_error(_) -> "Unknown error".

req(Req) ->
    Ddll = 
	case whereis(ddll_server) of
	    undefined ->
		{ok, Pid} = start(),
		Pid;
	    Pid ->
		Pid
	end,
    gen_server:call(Ddll, Req, infinity).


%%% --------------------------------------------------------
%%% The call-back functions.
%%% --------------------------------------------------------

handle_call({load_driver, Filename, Driver}, {From, _}, Data) ->
    {Result, NewData} = load_command(Filename, Driver, From, Data),
    {reply, Result, NewData};

handle_call({unload_driver, Driver}, {From, _}, Data) ->
    {Result, NewData} = unload_command(Driver, From, Data),
    {reply, Result, NewData};

handle_call(loaded_drivers, _From, {Port, Drivers}) ->
    {reply, loaded_drivers(Port), {Port, Drivers}};

handle_call(stop, _, Data) ->
    {stop, normal, ok, Data}.

handle_cast(_, Data) ->
    {noreply, Data}.

handle_info({'EXIT', Pid, Reason}, {Port, Drivers}) ->
    NewDrivers = unload_drivers(Drivers, Pid, Port, []),
    {noreply, {Port, NewDrivers}};
handle_info({'EXIT', Port, Reason}, {Port, Drivers}) ->
    {stop, {port_died, Reason}, {Port, Drivers}};
handle_info(_, Data) ->
    {noreply, Data}.

terminate(_Reason, {Port, Drivers}) ->
    unload_all_drivers(Drivers, Port),
    Port ! {self, close}.


%%% --------------------------------------------------------
%%% Implementation of server commands.
%%% --------------------------------------------------------

%% Loads a driver, or increments the reference count for the driver.

load_command(Filename, Driver, From, {Port, Drivers}) ->
    DriverAtom = list_to_atom(Driver),
    case increment_count(Drivers, From, DriverAtom, []) of
	{ok, NewDrivers} ->
	    {ok, {Port, NewDrivers}};
	not_loaded ->
	    case load_driver(Port, Filename, Driver) of
		ok ->
		    link(From),
		    {ok, {Port, [{DriverAtom, [{From, 1}]}|Drivers]}};
		Other ->
		    {Other, {Port, Drivers}}
	    end
    end.

increment_count([{Driver, Processes}|Rest], From, Driver, Result) ->
    NewProcesses = increment_process_count(Processes, From, []),
    {ok, Result ++ [{Driver, NewProcesses}|Rest]};
increment_count([Drv|Rest], From, Driver, Result) ->
    increment_count(Rest, From, Driver, [Drv|Result]);
increment_count([], From, Driver, Result) ->
    not_loaded.

increment_process_count([{From, Count}|Rest], From, Result) ->
    Result ++ [{From, Count+1}|Rest];
increment_process_count([Process|Rest], From, Result) ->
    increment_process_count(Rest, From, [Process|Rest]);
increment_process_count([], From, Result) ->
    [{From, 1}|Result].

%% Decrements the reference count for Driver in process From,
%% and unloads the driver if this was the last reference.

unload_command(Driver, From, {Port, Drivers}) ->
    DriverAtom = list_to_atom(Driver),
    case decrement_count(Drivers, From, DriverAtom, []) of
	{ok, NewDrivers} ->
	    {ok, {Port, NewDrivers}};
	{unload, NewDrivers} ->
	    {unload_driver(Port, Driver), {Port, NewDrivers}};
	Error0 ->
	    %% XXX This is a problem -- this driver will never be unloaded.
	    Error = transform_static_driver_error(Error0, Driver, Port),
	    {Error, {Port, Drivers}}
    end.

%% If the error was "not_loaded" but the driver is in the list of
%% all drivers, it must be a statically linked driver, and we transform
%% the error message to "linked_in_driver". (If loaded_drivers/0 fails,
%% we get a badmatch.)

transform_static_driver_error({error, not_loaded}, Driver, Port) ->
    {ok, All} = loaded_drivers(Port),
    case lists:member(Driver, All) of
	true ->
	    {error, linked_in_driver};
	false ->
	    {error, not_loaded}
    end;
transform_static_driver_error(Error, _, _) ->
    Error.

decrement_count([{Driver, Processes}|Rest], From, Driver, Result) ->
    case decrement_process_count(Processes, From, []) of
	[] ->
	    {unload, Result ++ Rest};
	{error, Error} ->
	    {error, Error};
	NewProcesses ->
	    {ok, Result ++ [{Driver, NewProcesses}|Rest]}
    end;
decrement_count([Drv|Rest], From, Driver, Result) ->
    decrement_count(Rest, From, Driver, [Drv|Result]);
decrement_count([], From, Driver, Result) ->
    {error, not_loaded}.

decrement_process_count([{From, 1}|Rest], From, Result) ->
    unlink(From),
    Result ++ Rest;
decrement_process_count([{From, Count}|Rest], From, Result) ->
    Result ++ [{From, Count-1}|Rest];
decrement_process_count([Process|Rest], From, Result) ->
    decrement_process_count(Rest, From, [Process|Result]);
decrement_process_count([], From, Result) ->
    {error, not_loaded_by_this_process}.

%% Unloads all drivers owned by Pid.

unload_drivers([{Driver, Processes}|Rest], Pid, Port, Result) ->
    case unload_process(Processes, Pid, []) of
	[] ->
	    %% XXX There is a problem if this unload fails.
	    unload_driver(Port, atom_to_list(Driver)),
	    unload_drivers(Rest, Pid, Port, Result);
	NewProcesses ->
	    unload_drivers(Rest, Pid, Port, [{Driver, NewProcesses}|Result])
    end;
unload_drivers([], Pid, Port, Result) ->
    Result.

unload_process([{Pid, _}|Rest], Pid, Result) ->
    Result ++ Rest;
unload_process([P|Rest], Pid, Result) ->
    unload_process(Rest, Pid, [P|Result]);
unload_process([], Pid, Result) ->
    Result.

%% Unloads all drivers (called when the server terminates).

unload_all_drivers([{Driver, _}|Rest], Port) ->
    unload_driver(Port, atom_to_list(Driver)),
    unload_all_drivers(Rest, Port);
unload_all_drivers([], _) ->
    ok.


%%% --------------------------------------------------------
%%% Talk to the linked in driver
%%% --------------------------------------------------------

load_driver(Port, FullName, Driver) ->
    command(Port, [?LOAD_DRIVER, Driver, 0, FullName, 0]).

unload_driver(Port, Driver) ->
    command(Port, [?UNLOAD_DRIVER, Driver, 0]).

loaded_drivers(Port) ->
    loaded_drivers(Port, command(Port, [?GET_DRIVERS]), []).

loaded_drivers(Port, {list_item, Item}, Result) ->
    loaded_drivers(Port, get_response(Port), [Item|Result]);
loaded_drivers(Port, ok, Result) ->
    {ok, lists:reverse(Result)};
loaded_drivers(Port, Status, []) ->
    Status.

command(Port, Command) ->
    Port ! {self(), {command, Command}},
    get_response(Port).

get_response(Port) ->
    receive
	{Port, {data, [Status|Rest]}} ->
	    get_response(Status, Rest);
	{'EXIT', Port, Reason} ->
	    exit({'ddll_drv port died', Reason})
    end.

get_response($o, []) ->
    ok;
get_response($o, List) ->
    {ok, List};
get_response($e, Atom) ->
    {error, list_to_atom(Atom)};
get_response($E, Error) ->
    get_error(Error, []);
get_response($i, Item) ->
    {list_item, Item}.

get_error([0|Message], Atom) ->
    {error, {list_to_atom(lists:reverse(Atom)), Message}};
get_error([C|Rest], Atom) ->
    get_error(Rest, [C|Atom]).
    
