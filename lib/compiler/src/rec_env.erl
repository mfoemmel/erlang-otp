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
%% The Initial Developer of the Original Code is Richard Carlsson.
%% Copyright (C) 1999-2001 Richard Carlsson.
%% Portions created by Ericsson are Copyright 2001, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%

%% =====================================================================
%% Abstract environments based on stdlib dictionaries, supporting
%% self-referential bindings and automatic new-key generation.
%%
%% Note: Stored values may not have the form `{'$letrec', ...}'; this is
%% reserved for internal purposes.
%% =====================================================================

%%%-define(DEBUG, true).

-module(rec_env).

-export([bind/3, bind_list/3, bind_recursive/4, empty/0, get/2,
	 is_defined/2, is_empty/1, keys/1, lookup/2, new_custom_key/2,
	 new_custom_keys/3, new_key/1, new_keys/2, size/1, to_list/1]).

-ifdef(DEBUG).
-export([test/1, test_custom/1, test_custom/2]).
-endif.

-ifdef(DEBUG).
%% Code for testing:

test(N) ->
    test_0(integer, N).

test_custom(N) ->
    F = fun (X) -> list_to_atom("X"++integer_to_list(X)) end,
    test_custom(F, N).

test_custom(F, N) ->
    test_0({custom, F}, N).

test_0(Type, N) ->
    put(new_key_calls, 0),
    put(new_key_retries, 0),
    put(new_key_max, 0),
    Env = test_1(Type, N, empty()),
    io:fwrite("\ncalls: ~w.\n", [get(new_key_calls)]),
    io:fwrite("\nretries: ~w.\n", [get(new_key_retries)]),
    io:fwrite("\nmax: ~w.\n", [get(new_key_max)]),
    dict:to_list(element(1,Env)).

test_1(integer = Type, N, Env) when integer(N), N > 0 ->
    Key = new_key(Env),
    test_1(Type, N - 1, bind(Key, value, Env));
test_1({custom, F} = Type, N, Env) when integer(N), N > 0 ->
    Key = new_custom_key(F, Env),
    test_1(Type, N - 1, bind(Key, value, Env));
test_1(_,0, Env) ->
    Env.
-endif.


%% Representation:
%%
%%	{Dictionary, {Counter, ColdStore}}
%%
%% Each environment contains one main dictionary structure for the
%% entries (for recursive bindings, entries have the special form
%% `{'$letrec', V, N, F}'). In addition, each environment contains a
%% "cold store", which consists of a dictionary for "frozen"
%% environments together with an index counter.


%% =====================================================================
%% empty() -> environment()
%%
%%	Returns an empty environment.

empty() ->
    Empty = dict:new(),
    {Empty, {0, Empty}}.


%% =====================================================================
%% is_empty() -> bool()
%%
%%	Returns `true' if the environment is empty, otherwise `false'.

is_empty({Dict, _}) ->
    dict:size(Dict) == 0.


%% =====================================================================
%% size() -> integer()
%%
%%	Returns the number of entries in the environment. This function
%%	could be useful for writing your own new-key generation
%%	function; cf `new_key' below.

size({Dict, _}) ->
    dict:size(Dict).


%% =====================================================================
%% is_defined(Key, Env) -> bool()
%%
%%	    Key = term()
%%	    Env = environment()
%%
%%	Returns `true' if `Key' is bound to some value in `Env',
%%	otherwise `false'.

is_defined(Key, {Dict, _}) ->
    dict:is_key(Key, Dict).


%% =====================================================================
%% keys(Env) -> [term()]
%%
%%	    Env = environment()
%%
%%	Returns an ordered list of all keys in the environment `Env'.

keys({Dict, _}) ->
    dict:fetch_keys(Dict).


%% =====================================================================
%% to_list(Env) -> [term()]
%%
%%	    Key = term()
%%	    Env = environment()
%%
%%	Returns an ordered list of `{Key, Value}' pairs for all keys in
%%	`Env'. For recursive entries (cf. `bind_recursive'), `Value' is
%%	computed as for `lookup'.

to_list({Dict, {_, S}}) ->
    L = dict:to_list(Dict),
    case dict:size(S) == 0 of
	true ->
	    L;    % don't waste time
	false ->
	    expand_entries(L, S)
    end.

expand_entries([{Key, Value} | Es], S) ->
    Value1 = case Value of
		 {'$letrec', N, Value, F} ->
		     recursive_value(S, N, Value, F);
		 _ ->
		     Value
	     end,
    [{Key, Value1} | expand_entries(Es, S)];		 
expand_entries([], S) ->
    [].


%% =====================================================================
%% bind(Key, Value, Env) -> Env1
%%
%%	    Key = Value = term()
%%	    Env = Env1 = environment()
%%
%%	Make a nonrecursive entry, binding `Key' to `Value'. If the key
%%	already existed in `Env', the old entry is replaced.
%%
%% bind_list(Keys, Values, Env) -> Env1
%%
%%	    Keys = Values = [term()]
%%	    Env = Env1 = environment()
%%
%%	Make N nonrecursive entries, binding each key in `Keys' to the
%%	corresponding value in `Values'. If some key already existed in
%%	`Env', the old entry is replaced. If `Keys' does not have the
%%	same length as `Values', an exception is generated.

bind(Key, Value, {Dict, Store}) ->
    {dict:store(Key, Value, Dict), Store}.

bind_list([Key | _] = Keys, Vs, {Dict, Store}) ->
    Dict1 = bind_list_1(Keys, Vs, Dict),
    {Dict1, Store};
bind_list([], [], Env) ->
    Env.

bind_list_1([Key | Ks], [V | Vs], Dict) ->
    bind_list_1(Ks, Vs, dict:store(Key, V, Dict));
bind_list_1([], _, Dict) ->
    Dict.


%% =====================================================================
%% bind_recursive(Keys, Values, Fun, Env) -> Env1
%%
%%	    Keys = Values = [term()]
%%	    Env = Env1 = environment()
%%	    Fun = (Value, Env) -> term()
%%
%%	Make N recursive entries, binding each `Key' to the value of
%%	`Fun(Value, Env1)' for the corresponding `Value'. If `Keys' does
%%	not have the same length as `Values', an exception is generated.
%%	If some key already existed in `Env', the old entry is replaced.
%%
%%	Note: the function `Fun' is evaluated each time one of the
%%	stored keys is looked up, but only then.
%%
%%	Examples:
%%
%%	    Env1 = bind_recursive([foo, bar], [1, 2],
%%	                          fun (V, E) -> V end,
%%	                          Env)
%%
%%	        This does nothing interesting; `get(foo, Env1)' yields
%%	        `1' and `get(bar, Env1)' yields `2', but the overhead is
%%	        larger than if the `bind' function had been used.
%%
%%	    Env1 = bind_recursive([foo, bar], [1, 2],
%%	                          fun (V, E) -> {V, E} end,
%%	                          Env)
%%
%%	        Here, however, `get(foo, Env1)' will yield `{1, Env1}'
%%	        and `get(bar, Env1)' will yield `{2, Env1}', i.e., the
%%	        environment `Env1' contains recursive bindings.

%% Special entries for all the keys are inserted into the dictionary,
%% which is then placed in the "store" together with the previous store;
%% then the index is incremented. This minimizes the amount of work done
%% at lookup.

bind_recursive([], [], F, Env) ->
    Env;
bind_recursive(Ks, Vs, F, {Dict, {N, Store}}) ->
    Dict1 = bind_recursive_1(Ks, Vs, F, N, Dict),
    finalize_env(Dict1, N, Store).

bind_recursive_1([K | Ks], [V | Vs], F, N, Dict) ->
    Dict1 = dict:store(K, {'$letrec', N, V, F}, Dict),
    bind_recursive_1(Ks, Vs, F, N, Dict1);
bind_recursive_1([], [], _, _, Dict) ->
    Dict.

%% This gets the updated `Dict', the old `N' and the old `Store',
%% and creates the final environment.

finalize_env(Dict, N, Store) ->
    Frozen = {Dict, Store},    % no need to store N
    Store1 = dict:store(N, Frozen, Store),
    {Dict, {N + 1, Store1}}.


%% =====================================================================
%% lookup(Key, Env) -> none | {value, Value}
%%
%%	    Key = Value = term()
%%	    Env = environment()
%%
%%	Returns `{ok, Value}' if `Key' is bound to `Value' in `Env', and
%%	`error' otherwise.

lookup(Key, {Dict, Store}) ->
    case dict:find(Key, Dict) of
	{ok, {'$letrec', N, Value, F}} ->
	    {_, S} = Store,
	    {ok, recursive_value(S, N, Value, F)};
	Other ->
	    Other
    end.

%% This fetches the corresponding frozen environment,
%% finalizes it and forms the result.

recursive_value(Store, N, Value, F) ->
    {Dict, Store1} = dict:fetch(N, Store),
    Env = finalize_env(Dict, N, Store1),
    F(Value, Env).


%% =====================================================================
%% get(Key, Env) -> Value
%%
%%	    Key = Value = term()
%%	    Env = environment()
%%
%%	Returns the value that `Key' is bound to in `Env'. If the key
%%	does not exist in `Env', an exception is generated.

get(Key, {Dict, Store}) ->
    case dict:fetch(Key, Dict) of
	{'$letrec', N, Value, F} ->
	    {_, S} = Store,
	    recursive_value(S, N, Value, F);
	Other ->
	    Other
    end.


%% =====================================================================
%% new_key(Env) -> integer()
%%
%%	    Env = environment()
%%
%%	Returns an integer which is not already used as key in `Env'.
%%	New integers are generated using an algorithm which tries to
%%	keep the values randomly distributed within a reasonably small
%%	range relative to the number of entries in the environment.
%%
%%	This function uses the module `random' to generate new keys. The
%%	seed it uses must be initialized by calling `random:seed/0' or
%%	`random:seed/3' before this function is first called.
%%
%%	Note that only the new key is returned; the given environment
%%	itself is not updated by this function.
%%
%% new_keys(N, Env) -> [integer()]
%%
%%	    N = integer()
%%	    Env = environment()
%%
%%	Returns a list of `N' distinct integers that are not already
%%	used as keys in `Env'. See `new_key' for details.

%% This function could possibly be improved. The important thing to keep
%% in mind is, that when we need a new key, we are generally in
%% mid-traversal of a syntax tree, and existing names in the tree may be
%% closely grouped and evenly distributed or even forming a compact
%% range. This means that if we generate an identifier whose value is
%% too close to those already seen (i.e., which are in the environment),
%% it is very probable that we will shadow a not-yet-seen identifier
%% further down in the tree, the result being that we induce another
%% later renaming, ending up renaming most of all identifiers,
%% completely contrary to our intention. We need to generate new
%% identifiers in a way that avoids such systematic collisions.
%%
%% The current limit of at most 2 retries before enlarging the range
%% seems to bee a good choice, according to my experiments.
%%
%% One way of getting a new key to try when the previous attempt failed
%% is of course to e.g. add one to the last tried value. However, in
%% general it's a bad idea to try adjacent identifiers: the percentage
%% of retries will typically increase a lot, so you may lose big on the
%% extra lookups while gaining only a little from the quicker
%% computation.
%%
%% We want an initial range that is large enough for many typical cases.
%% Often, the smallest environment contains a hundred or more entries
%% (due to top-level definitions).
%%
%% The following values have been shown to work well:

-define(MINIMUM_RANGE, 100).
-define(START_RANGE_FACTOR, 100).
-define(MAX_RETRIES, 3).    % retries before enlarging range
-define(ENLARGE_ENUM, 8).   % range enlargment enumerator
-define(ENLARGE_DENOM, 1).  % range enlargment denominator

-ifdef(DEBUG).
%% If you want to use these process dictionary counters, make sure to
%% initialise them to zero before you call any of the key-generating
%% functions.
%%
%%	new_key_calls		total number of calls
%%	new_key_retries		failed key generation attempts
%%	new_key_max		maximum generated integer value
%%
-define(measure_calls(),
	put(new_key_calls, 1 + get(new_key_calls))).
-define(measure_max_key(N),
	case N > get(new_key_max) of
	    true ->
		put(new_key_max, N);
	    false ->
		ok
	end).
-define(measure_retries(N),
	put(new_key_retries, get(new_key_retries) + N)).
-else.
-define(measure_calls(), ok).
-define(measure_max_key(N), ok).
-define(measure_retries(N), ok).
-endif.

new_key({Dict, _}) ->
    ?measure_calls(),
    R = start_range(Dict),
%%%    io:fwrite("Start range: ~w.\n", [R]),
    new_key(R, Dict).

new_key(R, Dict) ->
    new_key(generate(R, R), R, 0, Dict).

new_key(N, R, T, Dict) when T < ?MAX_RETRIES ->
    case dict:is_key(N, Dict) of
	true ->
%%%	    io:fwrite("CLASH: ~w.\n", [N]),
	    new_key(generate(N, R), R, T + 1, Dict);
	false ->
	    ?measure_max_key(N),
	    ?measure_retries(T),
%%%	    io:fwrite("New: ~w.\n", [N]),
	    N
    end;
new_key(N, R, T, Dict) ->
    %% Too many retries - enlarge the range and start over.
    ?measure_retries((T + 1)),
    R1 = (R * ?ENLARGE_ENUM) div ?ENLARGE_DENOM,
%%%    io:fwrite("NEW RANGE: ~w.\n", [R1]),
    new_key(generate(N, R1), R1, 0, Dict).

new_keys(N, {Dict, _}) when integer(N) ->
    R = start_range(Dict),
    new_keys(N, [], R, Dict).

new_keys(N, Ks, R, Dict) when N > 0 ->
    Key = new_key(R, Dict),
    Dict1 = dict:store(Key, true, Dict),
    new_keys(N - 1, [Key | Ks], R, Dict1);
new_keys(0, Ks, _, _) ->
    Ks.

start_range(Dict) ->
    max(dict:size(Dict) * ?START_RANGE_FACTOR,
	?MINIMUM_RANGE).

max(X, Y) when X > Y -> X;
max(X, Y) -> Y.

%% The previous key might or might not be used to compute the next key
%% to be tried. It is currently not used.
%%
%% In order to avoid causing cascading renamings, it is important that
%% this function does not generate values in order, but
%% (pseudo-)randomly distributed over the range.

generate(Key, Range) ->
    random:uniform(Range).    % works well


%% =====================================================================
%% new_custom_key(Function, Env) -> term()
%%
%%	    Function = (integer()) -> term()
%%	    Env = environment()
%%
%%	Returns a term which is not already used as key in `Env'. The
%%	term is generated by applying `Function' to a generated integer.
%%	Integers are generated using an algorithm which tries to keep
%%	the names randomly distributed within a reasonably small range
%%	relative to the number of entries in the environment (cf.
%%	`new_key').
%%
%%	This function uses the module `random' to generate new keys. The
%%	seed it uses must be initialized by calling `random:seed/0' or
%%	`random:seed/3' before this function is first called.
%%
%%	Note that only the identifier is returned; the given environment
%%	itself is not updated by this function.
%%
%% new_custom_keys(N, Function, Env) -> [term()]
%%
%%	    N = integer()
%%	    Function = (integer()) -> term()
%%	    Env = environment()
%%
%%	Returns a list of `N' distinct terms that are not already used
%%	as keys in `Env'. See `new_custom_key' for details.

%% The algorithm is the same as for `new_key':

new_custom_key(F, {Dict, _}) ->
    ?measure_calls(),
    R = start_range(Dict),
    new_custom_key(R, F, Dict).

new_custom_key(R, F, Dict) ->
    new_custom_key(generate(R, R), R, 0, F, Dict).

new_custom_key(N, R, T, F, Dict) when T < ?MAX_RETRIES ->
    A = F(N),
    case dict:is_key(A, Dict) of
	true ->
	    new_custom_key(generate(N, R), R, T + 1, F, Dict);
	false ->
	    ?measure_max_key(N),
	    ?measure_retries(T),
	    A
    end;
new_custom_key(N, R, T, F, Dict) ->
    %% Too many retries - enlarge the range and start over.
    ?measure_retries((T + 1)),
    R1 = (R * ?ENLARGE_ENUM) div ?ENLARGE_DENOM,
    new_custom_key(generate(N, R1), R1, 0, F, Dict).

new_custom_keys(N, F, {Dict, _}) when integer(N) ->
    R = start_range(Dict),
    new_custom_keys(N, [], R, F, Dict).

new_custom_keys(N, Ks, R, F, Dict) when N > 0 ->
    Key = new_custom_key(R, F, Dict),
    Dict1 = dict:store(Key, true, Dict),
    new_custom_keys(N - 1, [Key | Ks], R, F, Dict1);
new_custom_keys(0, Ks, _, _, _) ->
    Ks.


%% =====================================================================
