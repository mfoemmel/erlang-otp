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
-module(inets_test_lib).

-include("inets_test_lib.hrl").


%% Various small utility functions
-export([hostname/0, sz/1]).
-export([connect/3, send/3, csend/5, close/2]).
-export([update_config/3, get_config/2, get_config/3, fail/3, skip/1]).
-export([millis/0, millis_diff/2, hours/1, minutes/1, seconds/1, sleep/1]).
-export([watchdog/2, watchdog_start/1, watchdog_stop/1]).
-export([flush_mqueue/0, trap_exit/0, trap_exit/1]).
-export([info/4, log/4, debug/4, print/4]).
-export([expandable/3]).
-export([cover/1]).


%% ----------------------------------------------------------------------
%% print functions
%%

info(F, A, Mod, Line) ->
    print("INF ", F, A, Mod, Line).

log(F, A, Mod, Line) ->
    print("LOG ", F, A, Mod, Line).

debug(F, A, Mod, Line) ->
    print("DBG ", F, A, Mod, Line).

print(P, F, A, Mod, Line) ->
    io:format("~s[~p:~p:~p] : " ++ F ++ "~n", [P, self(), Mod, Line| A]).

print(F, A, Mod, Line) ->
    print("", F, A, Mod, Line).


%% ----------------------------------------------------------------------
%% Test case "suite" functions
%%

expandable(Init, Cases, Fin) when atom(Init), list(Cases), atom(Fin) ->
    {req, [], {conf, Init, Cases, Fin}}.

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


%% ----------------------------------------------------------------------
%% Socket functions:
%% open(SocketType, Host, Port) -> {ok, Socket} | {error, Reason}
%% SocketType -> ssl | ip_comm
%% Host       -> atom() | string() | {A, B, C, D} 
%% Port       -> integer()

connect(ssl,Host,Port) ->
    ssl:start(),
    case ssl:connect(Host, Port, [{packet,0}]) of
	{ok, Socket} ->
	    {ok, Socket};
	{error, Reason} ->
	    {error, Reason};
	Error ->
	    Error
    end;
connect(ip_comm,Host,Port) ->
    case gen_tcp:connect(Host,Port,[{packet,0},{reuseaddr,true}]) of
	{ok, Socket} ->
	    {ok, Socket};
	{error, {enfile,_}} ->
	    {error, enfile};
	Error ->
	    Error
    end.


csend(Type, Socket, Bin, Sz, _) when binary(Bin), size(Bin) =< Sz ->
    send(Type, Socket, Bin);
csend(Type, Socket, Bin, Sz, T) when binary(Bin) ->
    <<B:Sz/binary, Rest/binary>> = Bin,
    case send(Type, Socket, B) of
	ok ->
	    sleep(T),
	    csend(Type, Socket, Rest, Sz, T);
	{error, closed} ->
	    ok;
	Error ->
	    Error
    end;
csend(Type, Socket, L, Sz, _) when list(L), length(L) =< Sz ->
    send(Type, Socket, L);
csend(Type, Socket, L, Sz, T) when list(L) ->
    L1 = lists:sublist(L, Sz),
    case send(Type, Socket, L1) of
	ok ->
	    sleep(T),
	    csend(Type, Socket, lists:sublist(L, Sz+1, length(L)), Sz, T);
	{error, closed} ->
	    ok;
	Error ->
	    Error
    end.
    

send(ssl, Socket, Data) ->
    ssl:send(Socket, Data);
send(ip_comm,Socket,Data) ->
    gen_tcp:send(Socket,Data).


close(ssl,Socket) ->
    catch ssl:close(Socket);
close(ip_comm,Socket) ->
    catch gen_tcp:close(Socket).


%% ----------------------------------------------------------------
%% Test suite utility functions
%% 

update_config(Key, Val, Config) ->
    case get_config(Key, Config) of
	undefined ->
	    [{Key, Val}|Config];
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
    receive
	Any ->
	    ?INFO("flush_mqueue -> Any: ~p",[Any]),
	    flush_mqueue()
    after 0 ->
	    ok
    end.

    
trap_exit() -> 
    {trap_exit,Flag} = process_info(self(),trap_exit),Flag.

trap_exit(Flag) -> 
    process_flag(trap_exit,Flag).



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
    inets_test_server:t(Args),
    Files0 = [cover:analyse_to_file(Mod) || Mod <- Mods1],
    [io:format("Cover output: ~s~n", [File]) || {ok, File} <- Files0],
    ok.

