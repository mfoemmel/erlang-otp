-module(esh).

-export([main/1]).

main(_) ->
    %% init erl_prim_loader etc. are loaded ...
    Root = code_root_dir(),
    Args = ['-root', list_to_atom(Root)],
    %% erlang:display({esh, Args}),
    init:boot(Args).

%% atomize([H|T]) -> [list_to_atom(H)|atomize(T)];
%% atomize([])    -> [].

code_root_dir() ->
    case os:getenv("ERL_CODE_ROOT") of
	false ->
	    erlang:display({esh, 'ERL_CODE_ROOT not set'}),
	    erlang:display({run, 'ecc -install'}),
	    erlang:halt();
	Str ->
	    Str
    end.


