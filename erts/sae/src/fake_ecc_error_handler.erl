-module(error_handler).

%% NOTE this file name is error_handler_ecc

-export([undefined_function/3,undefined_global_name/2]).

undefined_function(M, F, A) ->
    erlang:display({error_handler,undefined_function,
		    M, F,A}),
    exit({undefined,function,M,F,A}).

undefined_global_name(Name, Message) ->
    exit({badarg,{Name,Message}}).
