%% -*- erlang-indent-level: 2 -*-
%%---------------------------------------------------------------------------
%% File        : typer_map.erl
%% Author      : He, Bingwen <hebingwen@hotmail.com>
%% Description : Adds an abstraction layer for a basic data structure
%%---------------------------------------------------------------------------

-module(typer_map).

-export([new/0, insert/2, lookup/2, from_list/1, remove/2, fold/3]).

-type(dict() :: tuple()).  %% XXX: temporarily -- TAKE ME OUT

-spec(new/0 :: () -> dict()).
new() ->
  dict:new().

-spec(insert/2 :: ({_,_}, dict()) -> dict()).
insert(Object, Dict) ->
  {Key,Value} = Object,
  dict:store(Key, Value, Dict).

-spec(lookup/2 :: (_, dict()) -> any()).
lookup(Key, Dict) ->
  try dict:fetch(Key, Dict) catch error:_ -> none end.

-spec(from_list/1 :: ([_]) -> dict()).
from_list(List) ->
  dict:from_list(List).

-spec(remove/2 :: (_, dict()) -> dict()).
remove(Key, Dict) ->
  dict:erase(Key, Dict).

-spec(fold/3 :: (fun((_,_,_) -> _), T, dict()) -> T).
fold(Fun, Acc0, Dict) -> 
  dict:fold(Fun, Acc0, Dict).
