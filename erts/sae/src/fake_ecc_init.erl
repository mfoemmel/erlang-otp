-module(init).
%% Stored in file init_ecc.erl

-export([get_argument/1]).

get_argument(master) ->
    error;
get_argument(generic_debug) ->
    error;
get_argument(noshell) ->
    {ok, [a]};
get_argument(X) ->
    erlang:display({error,in,init,argumnet,X}).


