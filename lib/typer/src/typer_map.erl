%% -*- erlang-indent-level: 2 -*-
%%%----------------------------------------------
%%% File    : typer_map.erl
%%% Author  : He, Bingwen <hebingwen@hotmail.com>
%%% Description : 
%%%
%%%----------------------------------------------

-module(typer_map).

-export([new/0, insert/2, lookup/2, 
	 from_list/1, remove/2, fold/3]).

new() ->
  dict:new().

insert(Object,Dict) ->
  {Key,Value} = Object,
  dict:store(Key, Value, Dict).

lookup(Key, Dict) ->
  case catch dict:fetch(Key, Dict) of
    {'EXIT',_} -> none;
    Result -> Result
  end.

from_list(List) ->
  dict:from_list(List).

remove(Key,Dict) ->
  dict:erase(Key,Dict).

fold(Fun, Acc0, Dict) -> 
  dict:fold(Fun, Acc0, Dict).
