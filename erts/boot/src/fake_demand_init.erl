-module(init).

%% Module:  init.erl
%% Author:  Joe Armstrong (joe.armstrong@telia.com)
%% Date:    2001-10-17
%% Purpose: init (minimal!)
%% Note:    Stored in file fake_demand_init,erl

-export([get_argument/1, request/1]).

get_argument(master) ->
    error;
get_argument(generic_debug) ->
    error;
get_argument(noshell) ->
    {ok, [a]};
get_argument(X) ->
    erlang:display({error,in,init,argument,X}).

request(X) ->
    erlang:display({error,in,init,request,X}).
