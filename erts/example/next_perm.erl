-module(next_perm).

-export([next_perm/1, prev_perm/1, load/0, all_perm/1]).

load() ->
    case whereis(next_perm) of
	undefined ->
	    case erl_ddll:load_driver(".", "next_perm") of
		ok -> ok;
		{error, already_loaded} -> ok;
		E -> exit(E)
	    end,
	    Port = open_port({spawn, "next_perm"}, []),
	    register(next_perm, Port);
	_ ->
	    ok
    end.

list_to_integer_binaries(L) ->
    [<<I:32/integer-native>> || I <- L].

next_perm(L) ->
    next_perm(L, 1).

prev_perm(L) ->
    next_perm(L, 2).

next_perm(L, Nxt) ->
    load(),
    B = list_to_integer_binaries(L),
    Port = whereis(next_perm),
    port_command(Port, [Nxt, B]),
    receive
	[Port | Result] ->
	    Result
    end.

all_perm(L) ->
    New = prev_perm(L),
    all_perm(New, L, [New]).

all_perm(L, L, Acc) ->
    Acc;
all_perm(L, Orig, Acc) ->
    New = prev_perm(L),
    all_perm(New, Orig, [New | Acc]).

    
