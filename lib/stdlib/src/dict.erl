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
-module(dict).

%% A simple dictionary interface.

-export([new/0,is_key/2,dict_to_list/1,list_to_dict/1]).
-export([fetch/2,find/2,fetch_keys/1,store/3,append/3,append_list/3,erase/2]).

%% new() -> Dictionary

new() -> [].

%% is_key(Key, Dictionary) -> Boolean

is_key(Key, [{K,Valie}|Dict]) when Key < K -> false;
is_key(Key, [{K,Value}|Dict]) when Key == K -> true;
is_key(Key, [{K,Value}|Dict]) when Key > K -> is_key(Key, Dict);
is_key(Key, []) -> false.

%% dict_to_list(Dictionary) -> [{Key,Value}]

dict_to_list(Dict) -> Dict.

%% list_to_dict([{Key,Value}]) -> Dictionary.

list_to_dict(Pairs) ->
    lists:foldl(fun ({K,V}, D) -> store(K, V, D) end, [], Pairs).

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

%% store(Key, Value, Dictionary) -> Dictionary'

store(Key, New, [{K,Old}|Dict]) when Key < K ->
    [{Key,New},{K,Old}|Dict];
store(Key, New, [{K,Old}|Dict]) when Key == K ->
    [{Key,New}|Dict];
store(Key, New, [{K,Old}|Dict]) when Key > K ->
    [{K,Old}|store(Key, New, Dict)];
store(Key, New, []) -> [{Key,New}].

%% append(Key, Value, Dictionary) -> Dictionary'

append(Key, New, [{K,Old}|Dict]) when Key < K ->
    [{Key,[New]},{K,Old}|Dict];
append(Key, New, [{K,Old}|Dict]) when Key == K ->
    [{Key,Old ++ [New]}|Dict];
append(Key, New, [{K,Old}|Dict]) when Key > K ->
    [{K,Old}|append(Key, New, Dict)];
append(Key, New, []) ->
	[{Key,[New]}].

%% append_list(Key, ValueList, Dictionary) -> Dictionary'

append_list(Key, NewList, [{K,Old}|Dict]) when Key < K ->
    [{Key,NewList},{K,Old}|Dict];
append_list(Key, NewList, [{K,Old}|Dict]) when Key == K ->
    [{Key,Old ++ NewList}|Dict];
append_list(Key, NewList, [{K,Old}|Dict]) when Key > K ->
    [{K,Old}|append_list(Key, NewList, Dict)];
append_list(Key, NewList, []) ->
	[{Key,NewList}].

%% erase(Key, Dictionary) -> Dictionary'

erase(Key, [{K,Value}|Dict]) when Key < K -> [{K,Value}|Dict];
erase(Key, [{K,Value}|Dict]) when Key == K -> Dict;
erase(Key, [{K,Value}|Dict]) when Key > K ->
    [{K,Value}|erase(Key, Dict)];
erase(Key, []) ->
    [].
