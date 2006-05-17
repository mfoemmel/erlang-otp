%% StackModule_StackFactory_impl example file.

-module('StackModule_StackFactory_impl').
-include_lib("orber/include/corba.hrl").
-export([create_stack/1, destroy_stack/2, init/1, terminate/2]).


init(_Env) ->
    {ok, []}.

terminate(_From, _Reason) ->
    ok.

create_stack(State)  ->
    %% Just a create we don't want a link.
    {reply, 'StackModule_Stack':oe_create(), State}.

destroy_stack(State, Stack)  ->
    {reply, corba:dispose(Stack), State}.
