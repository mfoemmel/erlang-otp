%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%

-module(orddict).

%% Standard interface.
-export([new/0,is_key/2,to_list/1,from_list/1,size/1]).
-export([fetch/2,find/2,fetch_keys/1,erase/2]).
-export([store/3,append/3,append_list/3,update/3,update/4,update_counter/3]).
-export([fold/3,map/2,filter/2,merge/3]).

%% Depreciated interface.
-export([dict_to_list/1,list_to_dict/1]).

%% new() -> Dictionary

new() -> [].

%% is_key(Key, Dictionary) -> Boolean

is_key(Key, [{K,Valie}|Dict]) when Key < K -> false;
is_key(Key, [{K,Value}|Dict]) when Key == K -> true;
is_key(Key, [{K,Value}|Dict]) when Key > K -> is_key(Key, Dict);
is_key(Key, []) -> false.

%% to_list(Dictionary) -> [{Key,Value}]

to_list(Dict) -> Dict.

%% from_list([{Key,Value}]) -> Dictionary.

from_list(Pairs) ->
    lists:foldl(fun ({K,V}, D) -> store(K, V, D) end, [], Pairs).

%% size(Dictionary) -> int().

size(D) -> length(D).

%% fetch(Key, Dictionary) -> Value

fetch(Key, [{K,Value}|_]) when Key == K -> Value;
fetch(Key, [{K,Value}|D]) when Key > K -> fetch(Key, D).

%% find(Key, Dictionary) -> {ok,Value} | error

find(Key, [{K,Value}|_]) when Key < K -> error;
find(Key, [{K,Value}|_]) when Key == K -> {ok,Value};
find(Key, [{K,Value}|D]) when Key > K -> find(Key, D);
find(Key, []) -> error.

%% fetch_keys(Dictionary) -> [Key]

fetch_keys([{Key,Val}|Dict]) ->
    [Key|fetch_keys(Dict)];
fetch_keys([]) ->
    [].

%% erase(Key, Dictionary) -> Dictionary'

erase(Key, [{K,Value}=E|Dict]) when Key < K -> [E|Dict];
erase(Key, [{K,Value}=E|Dict]) when Key == K -> Dict;
erase(Key, [{K,Value}=E|Dict]) when Key > K ->
    [E|erase(Key, Dict)];
erase(Key, []) -> [].

%% store(Key, Value, Dictionary) -> Dictionary'

store(Key, New, [{K,Old}=E|Dict]) when Key < K ->
    [{Key,New},E|Dict];
store(Key, New, [{K,Old}=E|Dict]) when Key == K ->
    [{Key,New}|Dict];
store(Key, New, [{K,Old}=E|Dict]) when Key > K ->
    [E|store(Key, New, Dict)];
store(Key, New, []) -> [{Key,New}].

%% append(Key, Value, Dictionary) -> Dictionary'.

append(Key, New, [{K,Old}=E|Dict]) when Key < K ->
    [{Key,[New]},E|Dict];
append(Key, New, [{K,Old}=E|Dict]) when Key == K ->
    [{Key,Old ++ [New]}|Dict];
append(Key, New, [{K,Old}=E|Dict]) when Key > K ->
    [E|append(Key, New, Dict)];
append(Key, New, []) -> [{Key,[New]}].

%% append_list(Key, ValueList, Dictionary) -> Dictionary'

append_list(Key, NewList, [{K,Old}=E|Dict]) when Key < K ->
    [{Key,NewList},E|Dict];
append_list(Key, NewList, [{K,Old}=E|Dict]) when Key == K ->
    [{Key,Old ++ NewList}|Dict];
append_list(Key, NewList, [{K,Old}=E|Dict]) when Key > K ->
    [E|append_list(Key, NewList, Dict)];
append_list(Key, NewList, []) ->
	[{Key,NewList}].

%% update(Key, Fun, Dictionary) -> Dictionary'.

update(Key, Fun, [{K,Val}=E|Dict]) when Key == K ->
    [{Key,Fun(Val)}|Dict];
update(Key, Fun, [{K,Val}=E|Dict]) when Key > K ->
    [E|update(Key, Fun, Dict)].

%% update(Key, Fun, Init, Dictionary) -> Dictionary'.

update(Key, Fun, Init, [{K,Val}=E|Dict]) when Key < K ->
    [{Key,Init},E|Dict];
update(Key, Fun, Init, [{K,Val}=E|Dict]) when Key == K ->
    [{Key,Fun(Val)}|Dict];
update(Key, Fun, Init, [{K,Val}=E|Dict]) when Key > K ->
    [E|update(Key, Fun, Init, Dict)];
update(Key, Fun, Init, []) -> [{Key,Init}].

%% update_counter(Key, Incr, Dictionary) -> Dictionary'.

update_counter(Key, Incr, [{K,Val}=E|Dict]) when Key < K ->
    [{Key,Incr},E|Dict];
update_counter(Key, Incr, [{K,Val}=E|Dict]) when Key == K ->
    [{Key,Val+Incr}|Dict];
update_counter(Key, Incr, [{K,Val}=E|Dict]) when Key > K ->
    [E|update_counter(Key, Incr, Dict)];
update_counter(Key, Incr, []) -> [{Key,Incr}].

%% fold(FoldFun, Accumulator, Dictionary) -> Accumulator.

fold(F, Acc, [{Key,Val}|D]) ->
    fold(F, F(Key, Val, Acc), D);
fold(F, Acc, []) -> Acc.

%% map(MapFun, Dictionary) -> Dictionary.

map(F, [{Key,Val}|D]) ->
    [{Key,F(Key, Val)}|map(F, D)];
map(F, []) -> [].

%% filter(FilterFun, Dictionary) -> Dictionary.

filter(F, [{Key,Val}=E|D]) ->
    case F(Key, Val) of
	true -> [E|filter(F, D)]; 
	false -> filter(F, D)
    end;
filter(F, []) -> [].

%% merge(MergeFun, Dictionary1, Dictionary2) -> Dictionary.

merge(F, [{K1,V1}=E1|D1], [{K2,V2}=E2|D2]) when K1 < K2 ->
    [E1|merge(F, D1, [E2|D2])];
merge(F, [{K1,V1}=E1|D1], [{K2,V2}=E2|D2]) when K1 =:= K2 ->
    [{K1,F(K1, V1, V2)}|merge(F, D1, D2)];
merge(F, [{K1,V1}=E1|D1], [{K2,V2}=E2|D2]) when K1 > K2 ->
    [E2|merge(F, [E1|D1], D2)];
merge(F, [], D2) -> D2;
merge(F, D1, []) -> D1.

%% Depreciated interface.

%% dict_to_list(Dictionary) -> [{Key,Value}]

dict_to_list(D) -> to_list(D).

%% list_to_dict([{Key,Value}]) -> Dictionary.

list_to_dict(L) -> from_list(L).
