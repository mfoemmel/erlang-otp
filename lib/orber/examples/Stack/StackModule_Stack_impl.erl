%% StackModule_Stack_impl example file.

-module('StackModule_Stack_impl').
-include_lib("orber/include/corba.hrl").
-include("StackModule.hrl").
-export([pop/1, push/2, empty/1, init/1, terminate/2]).


init(_Env) ->
    {ok, []}.

terminate(_From, _Reason) ->
    ok.

push(Stack, Val)  ->
    {reply, ok, [Val | Stack]}.

pop([Val | Stack]) ->
    {reply, Val, Stack};
pop([]) ->
    corba:raise(#'StackModule_EmptyStack'{}).

empty(_) ->
    {reply, ok, []}.

