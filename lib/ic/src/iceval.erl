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
-module(iceval).


-export([eval_const/5, eval_e/4]).

-export([check_tk/3, get_val/1, mk_val/1]).


%% Called fr: ictype 99, 522, 533

eval_const(G, S, N, TK, Expr) ->
    case catch eval_e(G, S, N, Expr) of
	T when element(1, T) == error -> 0;
	V -> 
	    case check_tk(G, TK, V) of
		true -> ok;
		false ->
		    icgen:error(G, {bad_tk_match, Expr, TK, get_val(V)})
	    end,
	    get_val(V)
    end.


check_op(G, S, N, Types, Op, E1, E2) ->
    V1 = eval_e(G, S, N, E1),
    V2 = eval_e(G, S, N, E2),
    check_types(G, Op, E1, Types, V1),
    check_types(G, Op, E2, Types, V2),
    case check_comb(V1, V2) of
	true ->
	    {V1, V2};
	false ->
	    Err = {bad_type_combination, E1, get_val(V1), get_val(V2)},
	    icgen:error(G, Err),
	    throw({error, Err})
    end.

check_op(G, S, N, Types, Op, E1) ->
    V1 = eval_e(G, S, N, E1),
    check_types(G, Op, E1, Types, V1),
    V1.

%% Match the declared type TK against the factual value of an constant
%%
check_tk(G, Any, default) -> true;		% Default case in union
check_tk(G, positive_int, V) when integer(V), V >= 0 -> true;
check_tk(G, tk_long, V) when integer(V) -> true;
check_tk(G, tk_short, V) when integer(V) -> true;
check_tk(G, tk_ushort, V) when integer(V), V >= 0 -> true;
check_tk(G, tk_ulong, V) when integer(V), V >= 0 -> true;
check_tk(G, tk_float, V) when float(V) -> true;
check_tk(G, tk_double, V) when float(V) -> true;
check_tk(G, tk_boolean, V) -> is_bool(V);
check_tk(G, tk_char, {char, V}) -> true;
check_tk(G, {tk_string, Len}, {string, V}) -> true;
%%check_tk(G, tk_octet, V) when integer(V) -> true;
%%check_tk(G, tk_null, V) when integer(V) -> true;
%%check_tk(G, tk_void, V) when integer(V) -> true;
%%check_tk(G, tk_any, V) when integer(V) -> true;
%%check_tk(G, {tk_objref, "", "Object"}, V) when integer(V) -> true.
check_tk(G, {tk_enum, _, _, Body}, {enum_id, Id}) -> 
    until(fun(X) when X == Id -> true;
	     (X) -> %%io:format("Checking ~p to ~p~n", [X, Id]),
		    false end, Body);
check_tk(G, TK, V) -> %%io:format("Matching ~p and ~p~n", [TK, V]),
    false.

get_val({string, X}) -> X;
get_val({char, X}) -> X;
get_val({enum_id, X}) -> X;
get_val(X) -> X.

check_types(G, Op, Expr, TypeList, V) ->
    case until(fun(int) when integer(V) -> true;
		  (float) when float(V) -> true;
		  (bool) when V==true -> true;
		  (bool) when V==false -> true;
		  (_) -> false end,
	       TypeList) of
	true -> true;
	false ->
	    Err = {bad_type, Expr, Op, TypeList, V},
	    icgen:error(G, Err),
	    throw({error, Err})
    end.

%%get_op(T) when tuple(T) -> element(1, T).

%% Should be in lists
until(F, [H|T]) ->
    case F(H) of
	true -> true;
	false -> until(F, T)
    end;
until(F, []) -> false.

%% Section of all the boolean operators (because Erlang ops don't like
%% boolean values.
e_or(X, Y) when integer(X), integer(Y) -> X bor Y;
e_or(true, _) -> true;
e_or(_, true) -> true;
e_or(_, _) -> false.

e_and(X, Y) when integer(X), integer(Y) -> X band Y;
e_and(true, true) -> true;
e_and(_, _) -> false.

e_xor(X, Y) when integer(X), integer(Y) -> X bxor Y;
e_xor(X, X) -> false;
e_xor(_, _) -> true.


%% Checks combination of argument types, basically floats and ints are
%% interchangeable, and all types are allowed with themselves. No
%% other combinations are allowed
%%
check_comb(X, Y) when integer(X), integer(Y) -> true;
check_comb(X, Y) when float(X), integer(Y) -> true;
check_comb(X, Y) when integer(X), float(Y) -> true;
check_comb(X, Y) when float(X), float(Y) -> true;
check_comb({X, _}, {X, _}) -> true;		% Strings and chars are tuples
check_comb(X, Y) ->
    case {is_bool(X), is_bool(Y)} of
	{true, true} -> true;
	_ -> false
    end;
check_comb(_, _) -> false.

is_bool(true) -> true;
is_bool(false) -> true;
is_bool(_) -> false.



%%%% (15)
eval_e(G, S, N, {'or', T1, T2}) ->
    {E1, E2} = check_op(G, S, N, [int, bool], 'or', T1, T2),
    e_or(E1, E2);

%%%% (16)
eval_e(G, S, N, {'xor', T1, T2}) ->
    {E1, E2} = check_op(G, S, N, [int, bool], 'xor', T1, T2),
    e_xor(E1, E2);

%%%% (17)
eval_e(G, S, N, {'and', T1, T2}) ->
    {E1, E2} = check_op(G, S, N, [int, bool], 'and', T1, T2),
    e_and(E1, E2);

%%%% (18)
eval_e(G, S, N, {'rshift', T1, T2}) ->
    {E1, E2} = check_op(G, S, N, [int], 'rshift', T1, T2),
    E1 bsr E2;
eval_e(G, S, N, {'lshift', T1, T2}) ->
    {E1, E2} = check_op(G, S, N, [int], 'lshift', T1, T2),
    E1 bsl E2;

%%%% (19)
eval_e(G, S, N, {'+', T1, T2}) ->
    {E1, E2} = check_op(G, S, N, [int, float], '+', T1, T2),
    E1 + E2;
eval_e(G, S, N, {'-', T1, T2}) ->
    {E1, E2} = check_op(G, S, N, [int, float], '-', T1, T2),
    E1 - E2;

%%%% (20)
eval_e(G, S, N, {'*', T1, T2}) ->
    {E1, E2} = check_op(G, S, N, [int, float], '*', T1, T2),
    E1 * E2;
eval_e(G, S, N, {'/', T1, T2}) ->
    {E1, E2} = check_op(G, S, N, [int, float], '/', T1, T2),
    E1 / E2;
eval_e(G, S, N, {'%', T1, T2}) ->
    {E1, E2} = check_op(G, S, N, [int], '%', T1, T2),
    E1 rem E2;

%%%% (21)
eval_e(G, S, N, {{'-', Line}, T}) ->
    -check_op(G, S, N, [int, float], '-', T);
eval_e(G, S, N, {{'+', Line}, T}) ->
    check_op(G, S, N, [int, float], '+', T);
eval_e(G, S, N, {{'~', Line}, T}) ->
    icgen:error(G, {unsupported_op, {'~', Line}}),
    eval_e(G, S, N, T);


%% Ints are repr. by an Erlang integer val, floats and doubles by
%% Erlang floats, chars and strings must be tuplerized for type
%% checking. These tuples are removed just before returning from top
%% function.
%%
eval_e(G, S, N, {'<integer_literal>', Line, X}) -> list_to_integer(X);
eval_e(G, S, N, {'<string_literal>', Line, X}) -> {string, X};
eval_e(G, S, N, {'<character_literal>', Line, X}) -> {char, hd(X)};
eval_e(G, S, N, {'TRUE', Line}) -> true;
eval_e(G, S, N, {'FALSE', Line}) -> false;
eval_e(G, S, N, {'<floating_pt_literal>', Line, X}) -> to_float(X);
eval_e(G, S, N, X) when element(1, X) == scoped_id ->
    mk_val(ictype:scoped_lookup(G, S, N, X));
eval_e(G, S, N, {default, _}) -> default.	% Default case in union


%% Make the newly looked up value a value that can be type checked.
mk_val({_, _, {tk_string, _}, V}) -> {string, V};
mk_val({_, _, tk_char, V}) -> {char, V};
mk_val({_, _, enum_val, V}) -> 
    {enum_id, icgen:get_id2(V)};
mk_val(X) when element(1, X) == error -> X;
mk_val({_, _, TK, V}) -> 
    V;
mk_val(V) -> V.



%% Floating point numbers
%%
%%	Conversion to Erlang floating points is neccessary because
%%	list_to_float BIF differs from IDL floats. "1e2" ".4e2" is
%%	allowed in IDL and must be translated to "1.0e2" and "0.4e2"

to_float(X) ->
    list_to_float(erlangify(X)).

erlangify([$. | R]) ->
    [$0, $. | R];
erlangify(R) ->
    look_for_dot(R).

look_for_dot([$. | R]) -> [$. | dot_pending(R)];
look_for_dot([$e | R]) -> [$., $0, $e | R];
look_for_dot([$E | R]) -> [$., $0, $E | R];
look_for_dot([X | R]) -> [X | look_for_dot(R)].

dot_pending([$e | R]) -> [$0, $e | R];
dot_pending([$E | R]) -> [$0, $E | R];
dot_pending([]) -> [$0];
dot_pending(R) -> R.

