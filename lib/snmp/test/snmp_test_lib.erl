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

-module(snmp_test_lib).

-export([hostname/0, sz/1, os_type/0]).
-export([replace_config/3, set_config/3, get_config/2, get_config/3]).
-export([fail/3, skip/1]).
-export([millis/0, millis_diff/2, hours/1, minutes/1, seconds/1, sleep/1]).
-export([flush_mqueue/0, trap_exit/0, trap_exit/1]).
-export([ping/1, local_nodes/0, nodes_on/1]).
-export([start_node/2]).
-export([watchdog/2, watchdog_start/1, watchdog_stop/1]).
-export([cover/1]).
-export([print/5]).


%% ----------------------------------------------------------------------
%% Misc functions
%%

hostname() ->
    from($@, atom_to_list(node())).
from(H, [H | T]) -> T;
from(H, [_ | T]) -> from(H, T);
from(H, []) -> [].


sz(L) when list(L) ->
    length(L);
sz(B) when binary(B) ->
    size(B);
sz(O) ->
    {unknown_size,O}.


os_type() ->
    case (catch test_server:os_type()) of
	{'EXIT', _} ->
	    %% Pre-R10 test server does not have this function
	    os:type();
	OsType ->
	    OsType
    end.


%% ----------------------------------------------------------------
%% Test suite utility functions
%% 

replace_config(Key, Config, NewValue) ->
    lists:keyreplace(Key, 1, Config, {Key, NewValue}).

set_config(Key, Def, Config) ->
    case get_config(Key, Config) of
        undefined ->
            [{Key, Def}|Config];
        _ ->
            Config
    end.

get_config(Key,C) ->
    get_config(Key,C,undefined).

get_config(Key,C,Default) ->
    case lists:keysearch(Key,1,C) of
        {value,{Key,Val}} ->
            Val;
        _ ->
            Default
    end.


fail(Reason, Mod, Line) ->
    exit({suite_failed, Reason, Mod, Line}).
    
skip(Reason) ->
    String = lists:flatten(io_lib:format("Skipping: ~p~n",[Reason])),
    exit({skipped, String}).
    

%% ----------------------------------------------------------------
%% Time related function
%% 

millis() ->
    erlang:now().

millis_diff(A,B) ->
    T1 = (element(1,A)*1000000) + element(2,A) + (element(3,A)/1000000),
    T2 = (element(1,B)*1000000) + element(2,B) + (element(3,B)/1000000),
    T1 - T2.

hours(N)   -> trunc(N * 1000 * 60 * 60).
minutes(N) -> trunc(N * 1000 * 60).
seconds(N) -> trunc(N * 1000).


sleep(infinity) ->
    receive
    after infinity ->
            ok
    end;
sleep(MSecs) ->
    receive
    after trunc(MSecs) ->
            ok
    end,
    ok.


%% ----------------------------------------------------------------
%% Process utility function
%% 

flush_mqueue() ->
    io:format("~p~n", [lists:reverse(flush_mqueue([]))]).

flush_mqueue(MQ) ->
    receive
        Any ->
            flush_mqueue([Any|MQ])
    after 0 ->
            ok
    end.

    
trap_exit() -> 
    {trap_exit,Flag} = process_info(self(),trap_exit),Flag.

trap_exit(Flag) -> 
    process_flag(trap_exit,Flag).



%% ----------------------------------------------------------------
%% Node utility functions
%% 

ping(N) ->
    case net_adm:ping(N) of
 	pang ->
 	    error;
 	pong ->
 	    ok
     end.

local_nodes() ->
    nodes_on(net_adm:localhost()).

nodes_on(Host) when list(Host) ->
    net_adm:world_list([list_to_atom(Host)]).


start_node(Name, Args) ->
    Opts = [{cleanup,false}, {args,Args}],
    test_server:start_node(Name, slave, Opts).


%% ----------------------------------------------------------------
%% Watchdog functions
%% 

watchdog_start(Timeout) ->
    spawn_link(?MODULE, watchdog, [Timeout, self()]).

watchdog_stop(Pid) ->
    unlink(Pid),
    exit(Pid, kill),
    ok.

watchdog(Timeout0, Pid) ->
    process_flag(priority, max),
    Timeout = timeout(Timeout0),
    receive
    after Timeout ->
            exit(Pid, watchdog_timeout)
    end.


timeout(T) ->
    trunc(timeout(T,os:type())).

timeout(T,vxworks) ->
    5 * T;
timeout(T,_) ->
    T.
            

%% ----------------------------------------------------------------------
%% cover functions
%%

cover([Suite, Case] = Args) when atom(Suite), atom(Case) ->
    Mods0 = cover:compile_directory("../src"),
    Mods1 = [Mod || {ok, Mod} <- Mods0],
    snmp_test_server:t(Args),
    Files0 = [cover:analyse_to_file(Mod) || Mod <- Mods1],
    [io:format("Cover output: ~s~n", [File]) || {ok, File} <- Files0],
    ok.


%% ----------------------------------------------------------------------
%% (debug) Print functions
%%

print(Prefix, Module, Line, Format, Args) ->
    io:format("~s[~p:~p:~p]: " ++ Format ++ "~n", 
	      [Prefix,self(),Module,Line]++Args).
