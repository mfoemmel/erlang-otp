-module(code).
%% FILE = code_ecc.erl

-export([start/1, internal/1, stop/0, root_dir/0, lib_dir/1]).
-import(lists, [member/2]).

start(Dir) ->
    register(code, spawn(?MODULE, internal, [Dir])).

internal(Dir) ->
    loop(Dir, no).

make_map([H|T], L) ->
    case member($-, H) of 
	true ->
	    H1 = mod_name(H),
	    make_map(T, [{H1, H}|L]);
	false ->
	    make_map(T, L)
    end;
make_map([], L) -> L.

mod_name([$-|T]) -> [];
mod_name([H|T]) -> [H|mod_name(T)].

stop() ->
    code ! stop.

loop(D, Map) ->
    receive
	{From, root} ->
	    From ! {code, D},
	    loop(D, Map);
	{From, {lib_dir, X}} ->
	    Map1 = lazy(Map, D),
	    From ! {dir, find_lib_dir(D, X, Map1)},
	    loop(D, Map1);
	stop ->
	    true
    end.

lazy(no, Dir) ->
    case file:list_dir(Dir ++ "/lib") of
	{ok, F}  ->
	    Map = make_map(F, []),
	    %% io:format("Map=~p~n",[Map]),
	    Map;
	_ ->
	    %% io:format("Lib dir not set~n", []),
	    []
    end;
lazy(L, _) ->
    L.

root_dir() ->
    code ! {self(), root},
    receive
	{code, D} ->
	    D
    end.

lib_dir(sasl) ->
    lib_dir("sasl");
lib_dir(X) ->
    code ! {self(), {lib_dir, X}},
    receive
	{dir, R} ->
	    R
    end.

find_lib_dir(Root, Inc, Map) ->
    LibDir = Root ++ "/lib/" ++ find_lib_dir1(Inc, Map),
    case file:file_info(LibDir) of
        {ok, _} ->
            LibDir;
        Error ->
            Error
    end.


find_lib_dir1(X, [{X,Y}|T]) -> Y;
find_lib_dir1(X, [_|T])     -> find_lib_dir1(X, T);
find_lib_dir1(X, [])        -> X.



