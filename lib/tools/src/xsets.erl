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
%% Portions created by Ericsson are Copyright 2000, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(xsets).

-export([from_term/1, from_term/2, from_external/2, 
	 empty_set/0, from_list/1, from_sets/1, 
	 relation/1, relation/2, family_of_subsets/1, 
	 to_external/1, type/1, to_sets/1, no_elements/1,
	 specification/2, union/2, intersection/2, 
	 difference/2, symdiff/2, product/1, product/2, 
	 is_equal/2, is_subset/2, is_set/1, is_ordset/1, is_empty_set/1, 
	 is_disjoint/2, usort/1, umerge/2]).

-export([union/1, intersection/1, canonical_relation/1]).

-export([relation_to_family/1, domain/1, range/1, composition/2, 
	 composition1/2, inverse/1, image/2, inverse_image/2,
	 relational_restriction/1, strict_relation/1, weak_relation/1, 
	 is_function/1]).

-export([restriction/2, restriction/3, drestriction/2, drestriction/3,
	 substitution/2, projection/2, 
	 partition/1, partition/2, 
	 composition_n/1, composition_n/2, multiple_compositions/2,
	 join/4]).

-export([family_to_relation/1, 
	 family_union/1, family_intersection/1,
	 union_of_families/2, union_of_families/1,
	 intersection_of_families/2, intersection_of_families/1,
	 difference_of_families/2, family_partition/2]).

-export([components/1, strong_components/1, cyclic_strong_components/1, 
	 topsort/1, subgraph/2, condensation/1]).

%% Nope, no is_member, del_member or add_member.
%% By a (local) convention, erlang:fault/2 is called in exported functions.

-define(TAG, 'Set').
-define(ORDTAG, 'OrdSet').
-define(ATOMTAG, 'AtomicSet').

-record(?TAG, {data = [], type = type}).
-record(?ORDTAG, {orddata = {}, ordtype = type}).

-define(LIST(S), S#?TAG.data).
-define(TYPE(S), S#?TAG.type).
%-define(SET(L, T), 
%	case is_type(T) of 
%	    true -> #?TAG{data = L, type = T};
%	    false -> erlang:fault(apa, [T])
%	end
%       ).
-define(SET(L, T), #?TAG{data = L, type = T}).
-define(IS_SET(S), record(S, ?TAG)).

%% Ordered sets and atoms:
-define(ORDDATA(S), S#?ORDTAG.orddata).
-define(ORDTYPE(S), S#?ORDTAG.ordtype).
-define(ORDSET(L, T), #?ORDTAG{orddata = L, ordtype = T}).
-define(IS_ORDSET(S), record(S, ?ORDTAG)).
-define(ATOM_TYPE, atom).

%% When IS_SET is true:
-define(ANYTYPE, '_').
-define(BINREL(X, Y), {X, Y}).
-define(IS_RELATION(R), tuple(R)).
-define(REL_ARITY(R), size(R)).
-define(REL_TYPE(R, I), element(I, R)).
-define(SET_OF(X), [X]).
-define(FAMILY_OF_SUBSETS(X, Y), ?BINREL(X, ?SET_OF(Y))).

%%
%%  Exported functions
%%

%%% 
%%% Create sets
%%% 

from_term(T) ->
    Type = case T of
	       _ when list(T) -> [?ANYTYPE];
	       _ -> ?ANYTYPE
	   end,
    case setify(T, Type) of
	error ->
	    erlang:fault(badarg, [T]);
	Set ->
	    Set
    end.

from_term(L, T) ->
    case is_type(T) of
	true ->
	    case setify(L, T) of
		error ->
		    erlang:fault(badarg, [L]);
		Set ->
		    Set
	    end;
	false  ->
	    erlang:fault(badarg, [L, T])
    end.

from_external(L, T) ->
    case {is_type(T), T} of
	{true, ?SET_OF(Type)} ->
	    ?SET(L, Type);
	{true, T} ->
	    ?ORDSET(L, T);
	_ ->
	    erlang:fault(badarg, [L, T])
    end.

empty_set() ->
    ?SET([], ?ANYTYPE).

from_list(L) ->
    case from_list(L, L) of
	error ->
	    erlang:fault(badarg, [L]);
	Set ->
	    Set
    end.

from_sets(Ss) when list(Ss) ->
    case set_of_sets(Ss, [], ?ANYTYPE) of
	{error, Error} ->
	    erlang:fault(Error, [Ss]);
	Set ->
	    Set
    end;
from_sets(Tuple) when tuple(Tuple) ->
    case ordset_of_sets(tuple_to_list(Tuple), [], []) of
	error ->
	    erlang:fault(badarg, [Tuple]);
	Set ->
	    Set
    end;
from_sets(T) ->
    erlang:fault(badarg, [T]).

relation([]) ->
    empty_set();
relation(Ts = [T | _]) when tuple(T) ->
    case relation(Ts, Ts, size(T)) of
	error ->
	    erlang:fault(badarg, [Ts]);
	Set ->
	    Set
    end;
relation(E) ->
    erlang:fault(badarg, [E]).

relation(Ts, Size) when integer(Size) ->
    case relation(Ts, Ts, Size) of
	error ->
	    erlang:fault(badarg, [Ts, Size]);
	Set ->
	    Set
    end.

family_of_subsets(Ts) ->
    case family_of_subsets(Ts, []) of
	error ->
	    erlang:fault(badarg, [Ts]);
	Set ->
	    Set
    end.

%%% 
%%% Functions on any sets
%%% 

to_external(S) when ?IS_SET(S) ->
    ?LIST(S);
to_external(S) when ?IS_ORDSET(S) ->
    ?ORDDATA(S).

type(S) when ?IS_SET(S) ->
    ?SET_OF(?TYPE(S));
type(S) when ?IS_ORDSET(S) ->
    ?ORDTYPE(S).

to_sets(S) when ?IS_SET(S) ->
    case ?TYPE(S) of
	?ANYTYPE -> [];
	?SET_OF(Type) -> list_of_sets(?LIST(S), Type, []);
	Type when tuple(Type) -> list_of_ordsets(?LIST(S), Type, []);
	_ -> erlang:fault(badarg, [S])
    end;
to_sets(S) when ?IS_ORDSET(S), tuple(?ORDTYPE(S)) ->
    tuple_of_sets(tuple_to_list(?ORDDATA(S)), tuple_to_list(?ORDTYPE(S)), []);
to_sets(T) ->
    erlang:fault(badarg, [T]).

no_elements(S) when ?IS_SET(S) ->
    length(?LIST(S));
no_elements(S) when ?IS_ORDSET(S), tuple(?ORDTYPE(S)) ->
    size(?ORDDATA(S));
no_elements(S) when ?IS_ORDSET(S) ->
    1.

specification(F, S) when function(F), ?IS_SET(S) ->
    ?SET(specification(?LIST(S), F, []), ?TYPE(S)).

union(S1, S2) when ?IS_SET(S1), ?IS_SET(S2) ->
    case unify_types(?TYPE(S1), ?TYPE(S2)) of
	error -> erlang:fault(type_mismatch, [S1, S2]);
	Type ->  ?SET(umerge(?LIST(S1), ?LIST(S2)), Type)
    end.

intersection(S1, S2) when ?IS_SET(S1), ?IS_SET(S2) ->
    case unify_types(?TYPE(S1), ?TYPE(S2)) of
	error -> erlang:fault(type_mismatch, [S1, S2]);
	Type ->  ?SET(intersection(?LIST(S1), ?LIST(S2), []), Type)
    end.

difference(S1, S2) when ?IS_SET(S1), ?IS_SET(S2) ->
    case unify_types(?TYPE(S1), ?TYPE(S2)) of
	error -> erlang:fault(type_mismatch, [S1, S2]);
	Type ->  ?SET(difference(?LIST(S1), ?LIST(S2), []), Type)
    end.

symdiff(S1, S2) when ?IS_SET(S1), ?IS_SET(S2) ->
    case unify_types(?TYPE(S1), ?TYPE(S2)) of
	error -> erlang:fault(type_mismatch, [S1, S2]);
	Type ->  ?SET(symdiff(?LIST(S1), ?LIST(S2), []), Type)
    end.

product(X, Y) when ?IS_SET(X), ?IS_SET(Y) ->
    if
	?TYPE(X) == ?ANYTYPE -> X;
	?TYPE(Y) == ?ANYTYPE -> Y;
	true ->
	    F = fun(A) -> {0, A} end,
	    T = ?BINREL(?TYPE(X), ?TYPE(Y)),
	    ?SET(compose(lists:map(F, ?LIST(X)), lists:map(F, ?LIST(Y))), T)
    end.

product({}) -> empty_set();
product({X, Y}) ->
    product(X, Y);
product(T) when tuple(T) ->
    Ss = tuple_to_list(T),
    L = sets_to_list(Ss),
    Type = types(Ss, []),
    case lists:member([], L) of
	true ->
	    ?SET([], Type);
	false -> 
	    ?SET(lists:reverse(prod(L, [], [])), Type)
    end.

is_equal(S1, S2) when ?IS_SET(S1), ?IS_SET(S2) ->
    case match_types(?TYPE(S1), ?TYPE(S2)) of
	false -> erlang:fault(type_mismatch, [S1, S2]);
	true  -> ?LIST(S1) == ?LIST(S2)
    end;
is_equal(S1, S2) when ?IS_ORDSET(S1), ?IS_ORDSET(S2) ->
    case match_types(?TYPE(S1), ?TYPE(S2)) of
	false -> erlang:fault(type_mismatch, [S1, S2]);
	true  -> ?ORDDATA(S1) == ?ORDDATA(S2)
    end;
is_equal(S1, S2) when ?IS_SET(S1), ?IS_ORDSET(S2) ->
    false;
is_equal(S1, S2) when ?IS_ORDSET(S1), ?IS_SET(S2) ->
    false.

is_subset(S1, S2) when ?IS_SET(S1), ?IS_SET(S2) ->
    case match_types(?TYPE(S1), ?TYPE(S2)) of
	false -> erlang:fault(type_mismatch, [S1, S2]);
	true  -> subset(?LIST(S1), ?LIST(S2))
    end.

is_set(S) when ?IS_SET(S) ->
    true;
is_set(_S) ->
    false.

is_ordset(S) when ?IS_ORDSET(S) ->
    true;
is_ordset(_S) ->
    false.

is_empty_set(S) when ?IS_SET(S) -> 
    ?LIST(S) == [].

is_disjoint(S1, S2) when ?IS_SET(S1), ?IS_SET(S2) ->
    case match_types(?TYPE(S1), ?TYPE(S2)) of
	false -> erlang:fault(type_mismatch, [S1, S2]);
	true ->     
	    case ?LIST(S1) of
		[] -> true;
		[A | As] -> disjoint(?LIST(S2), A, As)
	    end
    end.

usort([A | L]) ->
    union_lists(split(L, A, A, [A], [], []), []);
usort([]) ->
    [].

umerge([], L2) ->
    L2;
umerge(L1, []) ->
    L1;
umerge([H1 | T1], L2) ->
    lists:reverse(union2(L2, H1, T1, [])).

%%%
%%% Function on set-of-sets.
%%%

union(S) when ?IS_SET(S) ->
    case ?TYPE(S) of
	?ANYTYPE -> S;
	?SET_OF(Type) -> ?SET(lunion(?LIST(S)), Type);
	_ -> erlang:fault(badarg, [S])
    end.

intersection(S) when ?IS_SET(S) ->
    case ?LIST(S) of
	[] -> erlang:fault(badarg, [S]);
	[L | Ls] ->
	    case ?TYPE(S) of
		?SET_OF(Type) ->
		    ?SET(lintersection(Ls, L), Type);
		_ -> erlang:fault(badarg, [S])
	    end
    end.

canonical_relation(Sets) when ?IS_SET(Sets) ->
    ST = ?TYPE(Sets),
    case ST of
	?ANYTYPE -> Sets;
	?SET_OF(?ANYTYPE) -> empty_set();
	?SET_OF(Type) -> 
	    ?SET(can_map(?LIST(Sets), []), ?BINREL(Type, ST));
	_ -> erlang:fault(badarg, [Sets])
    end.

%%% 
%%% Functions on binary relations only.
%%% 

relation_to_family(R) when ?IS_SET(R) ->
    case ?TYPE(R) of
	?ANYTYPE -> R;
	?BINREL(DT, RT) -> 
	    ?SET(rel_to_fam(?LIST(R)), ?FAMILY_OF_SUBSETS(DT, RT));
	_Else    -> erlang:fault(badarg, [R])
    end.
    

domain(R) when ?IS_SET(R) ->
    case ?TYPE(R) of
	?ANYTYPE -> R;
	?BINREL(DT, _)  -> ?SET(dom(?LIST(R),  []), DT);
	_Else    -> erlang:fault(badarg, [R])
    end.

range(R) when ?IS_SET(R) ->
    case ?TYPE(R) of
	?ANYTYPE -> R;
	?BINREL(_, RT)  -> ?SET(ran(?LIST(R),  []), RT);
	_ -> erlang:fault(badarg, [R])
    end.

composition(A, B) ->
    composition1(inverse(A), B).

composition1(A, B) when ?IS_SET(A), ?IS_SET(B) ->
    AT = ?TYPE(A),
    BT = ?TYPE(B),
    {DTA, RTA} = case AT of
		     ?ANYTYPE -> {?ANYTYPE, ?ANYTYPE};
		     ?BINREL(_, _) -> AT;
		     _ -> erlang:fault(badarg, [A, B])
		 end,
    {DTB, RTB} = case BT of
		     ?ANYTYPE -> {?ANYTYPE, ?ANYTYPE};
		     ?BINREL(_, _) -> BT;
		     _ -> erlang:fault(badarg, [A, B])
		 end,
    case match_types(DTA, DTB) of
	false -> erlang:fault(type_mismatch, [A, B]);
	true when DTA == ?ANYTYPE -> A;
	true when DTB == ?ANYTYPE -> B;
	true -> ?SET(compose(?LIST(A), ?LIST(B)), ?BINREL(RTA, RTB))
    end.

inverse(R) when ?IS_SET(R) ->
    case ?TYPE(R) of
	?ANYTYPE -> R;
	?BINREL(DT, RT) -> ?SET(inverse(?LIST(R), []), ?BINREL(RT, DT));
	_ -> erlang:fault(badarg, [R])
    end.
    
image(R, X) when ?IS_SET(R), ?IS_SET(X) ->
    range(restriction(1, R, X)).

inverse_image(R, Y) when ?IS_SET(R), ?IS_SET(Y) ->
    domain(restriction(2, R, Y)).

relational_restriction(R) when ?IS_SET(R) ->
    case ?TYPE(R) of
	?ANYTYPE -> R;
	Type = ?BINREL(_, _) -> 
	    case ?LIST(R) of
		[] -> R;
		[E = {X, _Y} | Es] ->
		    ?SET(rel_restrict(Es, E, X, []), Type)
	    end;
	_ -> erlang:fault(badarg, [R])
    end.

strict_relation(R) when ?IS_SET(R) ->
    case ?TYPE(R) of
	?ANYTYPE -> R;
	Type = ?BINREL(_, _) -> 
	    ?SET(strict(?LIST(R), []), Type);
	_ -> erlang:fault(badarg, [R])
    end.
    
weak_relation(R) when ?IS_SET(R) ->
    case ?TYPE(R) of
	?ANYTYPE -> R;
	?BINREL(DT, RT) -> 
	    case unify_types(DT, RT) of
		error ->
		    erlang:fault(badarg, [R]);
		UType ->
		    ?SET(weak(?LIST(R)), ?BINREL(UType, UType))
	    end;
	_ -> erlang:fault(badarg, [R])
    end.
    
is_function(R) when ?IS_SET(R) ->
    case ?TYPE(R) of
	?ANYTYPE -> true;
	?BINREL(_, _) -> 
	    case ?LIST(R) of
		[] -> true;
		[{V,_} | Es] -> function(Es, V)
	    end;
	_ -> erlang:fault(badarg, [R])
    end.

restriction(Relation, Set) ->
    restriction(1, Relation, Set).

drestriction(Relation, Set) ->
    drestriction(1, Relation, Set).

%%% 
%%% Functions on relations (binary or other).
%%% 

-define(RESTRICTION(Function, RFun),
    Function(I, R, S) when integer(I), ?IS_SET(R), ?IS_SET(S) ->
	case ?TYPE(R) of
	    ?ANYTYPE -> 
		R;
	    Rel when ?IS_RELATION(Rel), I =< ?REL_ARITY(Rel), I >= 1 -> 
		Function(fun(T) -> element(I, T) end, R, S);
	    _ -> 
		erlang:fault(badarg, [I, R, S])
	end;
    Function(Fun, R, S) when function(Fun), ?IS_SET(R), ?IS_SET(S) ->
	RT = ?TYPE(R),
	ST = ?TYPE(S),
	case check_fun(RT, Fun, false) of
	    empty ->
		R;
	    error ->
		erlang:fault(badarg, [Fun, R, S]);
	    Sort ->
		case unify_types(Fun(RT), ST) of
		    error ->
			erlang:fault(type_mismatch, [Fun, R, S]);
		    _UType ->
			R1 = inverse_projection(?LIST(R), Fun, Sort),
			?SET(usort(Sort, RFun(?LIST(S), R1)), RT)
		end
	end).

?RESTRICTION(restriction, restrict).

?RESTRICTION(drestriction, diff_restrict).

substitution(I, Set) when integer(I), ?IS_SET(Set) ->
    case ?TYPE(Set) of
	?ANYTYPE -> 
	    Set;
	Rel when ?IS_RELATION(Rel), I =< ?REL_ARITY(Rel), I >= 1 -> 
	    substitution(fun(T) -> element(I, T) end, Set);
	_ -> 
	    erlang:fault(badarg, [I, Set])
    end;
substitution(Fun, Set) when function(Fun), ?IS_SET(Set) ->
    Type = ?TYPE(Set),
    case check_fun(Type, Fun, true) of
	empty ->
	    Set;
	error ->
	    erlang:fault(badarg, [Fun, Set]);
	Sort ->
	    ?SET(usort(Sort, substitute(?LIST(Set), Fun, [])), Fun(Type))
    end.

projection(I, Set) when integer(I), ?IS_SET(Set) ->
    case ?TYPE(Set) of
	?ANYTYPE -> 
	    Set;
	Rel when ?IS_RELATION(Rel), I =< ?REL_ARITY(Rel), I >= 1 -> 
	    projection(fun(T) -> element(I, T) end, Set);
	_ -> 
	    erlang:fault(badarg, [I, Set])
    end;
projection(Fun, Set) when function(Fun), ?IS_SET(Set) ->
    Type = ?TYPE(Set),
    case check_fun(Type, Fun, true) of
	empty ->
	    Set;
	error ->
	    erlang:fault(badarg, [Fun, Set]);
	_Sort ->
	    ?SET(project(?LIST(Set), Fun, []), ?BINREL(Type, Fun(Type)))
    end.

partition(Ps) ->
    F1 = relation_to_family(canonical_relation(Ps)),
    F2 = relation_to_family(inverse(F1)),
    range(F2).

partition(X, S) ->
    range(family_partition(X, S)).

composition_n(RT) when tuple(RT) ->
    case compose_n(RT, foo, false, false) of
	{error, Reason} -> 
	    erlang:fault(Reason, [RT]);
	Reply ->
	    Reply
    end.

composition_n(RT, G) ->
    EmptyG = case ?TYPE(G) of
		 ?ANYTYPE -> true;
		 ?BINREL(_, _) -> false;
		 _ -> erlang:fault(badarg, [RT, G])
	     end,
    case compose_n(RT, G, EmptyG, true) of
	{error, Reason} -> 
	    erlang:fault(Reason, [RT, G]);
	Reply ->
	    Reply
    end.

-define(TEST_REL(R, I, C),
    case ?TYPE(R) of
	?ANYTYPE -> ok;
	Rel when ?IS_RELATION(Rel), C == eq, I == ?REL_ARITY(Rel) -> ok;
	Rel when ?IS_RELATION(Rel), C == lte, I =< ?REL_ARITY(Rel) -> ok;
	_ -> error
    end).

multiple_compositions(T, R) when tuple(T), ?IS_SET(R) ->
    case ?TEST_REL(R, size(T), eq) of
	error -> erlang:fault(badarg, [T, R]);
	ok -> composition_n(list_to_tuple(mul_comp(tuple_to_list(T), 1, R)))
    end.

join(A, I1, B, I2) 
  when ?IS_SET(A), ?IS_SET(B), integer(I1), integer(I2) ->
    case (?TEST_REL(A, I1, lte) == ok) and (?TEST_REL(B, I2, lte) == ok) of
	false -> erlang:fault(badarg, [A, I1, B, I2]);
	true when ?TYPE(A) == ?ANYTYPE -> A;
	true when ?TYPE(B) == ?ANYTYPE -> B;
	true ->
	    R1 = raise_element(A, I1),
	    R2 = raise_element(B, I2),
	    {T, F} = 
		case (I1 == 1) and (I2 == 1)  of
		    true ->
			T11 = compose1(?LIST(R1), ?LIST(R2)),
			F11 = fun({X,Y}) -> join_element(X, Y) end,
			{T11, F11};
		    false ->
			T0 = compose(?LIST(R1), ?LIST(R2)),
			F0 = fun({X,Y}) -> 
				     list_to_tuple(join_element(X, I1, Y, I2)) 
			     end,
			{T0, F0}
		end,
	    ?SET(replace(T, F, []), F({?TYPE(A), ?TYPE(B)}))
    end.

%%%
%%% Family functions
%%%

family_to_relation(F) when ?IS_SET(F) ->
    case ?TYPE(F) of
	?ANYTYPE ->
	    F;
	?FAMILY_OF_SUBSETS(DT, RT) ->
	    ?SET(fam_to_rel(?LIST(F), []), ?BINREL(DT, RT));
	_ -> erlang:fault(badarg, [F])
    end.

family_union(F) ->
    union(range(F)).

family_intersection(F) ->
    intersection(range(F)).

-define(X_OF(FF), 
    case catch unify_types(?TYPE(F1), ?TYPE(F2)) of
	error -> 
	    erlang:fault(type_mismatch, [F1, F2]);
	?ANYTYPE -> 
	    F1;
	Type = ?FAMILY_OF_SUBSETS(_, _) -> 
	    ?SET(FF(?LIST(F1), ?LIST(F2), []), Type);
	_ ->  erlang:fault(badarg, [F1, F2])
    end).

union_of_families(F1, F2) when ?IS_SET(F1), ?IS_SET(F2) ->
    ?X_OF(fam_union).

union_of_families(Fs) when ?IS_SET(Fs) ->
    case ?TYPE(Fs) of
	?ANYTYPE -> 
	    Fs;
	?SET_OF(?ANYTYPE) ->
	    empty_set();
	?SET_OF(Type = ?FAMILY_OF_SUBSETS(_, _)) ->
	    ?SET(fam_lunion(?LIST(Fs)), Type);
	_ -> erlang:fault(badarg, [Fs])
    end.

intersection_of_families(F1, F2) when ?IS_SET(F1), ?IS_SET(F2) ->
    ?X_OF(fam_intersect).

intersection_of_families(Fs) when ?IS_SET(Fs) ->
    case ?LIST(Fs) of
	[] ->
	    erlang:fault(badarg, [Fs]);
	[L | Ls] ->
	    case ?TYPE(Fs) of
		?SET_OF(?ANYTYPE) ->
		    empty_set();
		?SET_OF(Type = ?FAMILY_OF_SUBSETS(_, _)) ->
		    ?SET(fam_intersect(Ls, L), Type);
		_ -> erlang:fault(badarg, [Fs])
	    end
    end.

difference_of_families(F1, F2) when ?IS_SET(F1), ?IS_SET(F2) ->
    ?X_OF(fam_difference).

family_partition(I, Set) when integer(I), ?IS_SET(Set) ->
    case ?TYPE(Set) of
	?ANYTYPE -> 
	    Set;
	Rel when ?IS_RELATION(Rel), I =< ?REL_ARITY(Rel), I >= 1 -> 
	    family_partition(fun(T) -> element(I, T) end, Set);
	_ -> 
	    erlang:fault(badarg, [I, Set])
    end;
family_partition(Fun, Set) when function(Fun), ?IS_SET(Set) ->
    Type = ?TYPE(Set),
    case check_fun(Type, Fun, false) of
	empty ->
	    Set;
	error ->
	    erlang:fault(badarg, [Fun, Set]);
	Sort ->
	    Ts = inverse_projection(?LIST(Set), Fun, Sort),
	    P = fam_partition(Ts, Sort),
	    ?SET(lists:reverse(P), ?BINREL(Fun(Type), ?SET_OF(Type)))
    end.

%%%
%%% Graph functions
%%%

components(Relation) when ?IS_SET(Relation) ->
    case setify_apply(Relation, components) of
	{error, Error} -> erlang:fault(Error, [Relation]);
	R -> R
    end.

strong_components(Relation) when ?IS_SET(Relation) ->
    case setify_apply(Relation, strong_components) of
	{error, Error} -> erlang:fault(Error, [Relation]);
	R -> R
    end.

cyclic_strong_components(Relation) when ?IS_SET(Relation) ->
    case setify_apply(Relation, cyclic_strong_components) of
	{error, Error} -> erlang:fault(Error, [Relation]);
	R -> R
    end.

topsort(Relation) when ?IS_SET(Relation) ->
    case apply_graph_fun(Relation, topsort) of
	{error, Error} -> erlang:fault(Error, [Relation]);
	{_Type, R} -> R
    end.

subgraph(Relation, Set) ->
    restriction(2, restriction(1, Relation, Set), Set).

condensation(Set) ->
    M = canonical_relation(strong_components(Set)),
    range(multiple_compositions({M, M}, Set)).

%%
%%  Local functions
%%

from_list([_ | S], L) ->
    from_list(S, L);
from_list([], L) ->
    ?SET(usort(L), ?ATOM_TYPE);
from_list(_E, _L) ->
    error.

set_of_sets([S | Ss], L, T0) when ?IS_SET(S) ->
    case unify_types([?TYPE(S)], T0) of
	error -> {error, type_mismatch};
	Type ->  set_of_sets(Ss, [?LIST(S) | L], Type)
    end;
set_of_sets([S | Ss], L, T0) when ?IS_ORDSET(S) ->
    case unify_types(?ORDTYPE(S), T0) of
	error -> {error, type_mismatch};
	Type ->  set_of_sets(Ss, [?ORDDATA(S) | L], Type)
    end;
set_of_sets([], L, T) ->
    ?SET(usort(L), T);
set_of_sets(_, _L, _T) ->
    {error, badarg}.

ordset_of_sets([S | Ss], L, T) when ?IS_SET(S) ->
    ordset_of_sets(Ss, [?LIST(S) | L], [[?TYPE(S)] | T]);
ordset_of_sets([S | Ss], L, T) when ?IS_ORDSET(S) ->
    ordset_of_sets(Ss, [?LIST(S) | L], [?ORDTYPE(S) | T]);
ordset_of_sets([], L, T) ->
    ?ORDSET(list_to_tuple(lists:reverse(L)), list_to_tuple(lists:reverse(T)));
ordset_of_sets(_, _L, _T) ->
    error.

relation([T | Ts], L, Sz) when tuple(T), size(T) == Sz ->
    relation(Ts, L, Sz);
relation([], L, Sz) ->
    ?SET(usort(L), list_to_tuple(lists:duplicate(Sz, ?ATOM_TYPE)));
relation(_, _L, _Sz) ->
    error.

family_of_subsets([{I, IS} | Ps], L) ->
    family_of_subsets(IS, I, IS, Ps, L);
family_of_subsets([], L) ->
    ?SET(usort(L), ?FAMILY_OF_SUBSETS(?ATOM_TYPE, ?ATOM_TYPE));
family_of_subsets(_E, _L) ->
    error.

family_of_subsets([_E | Es], I, IS, Ps, L) ->
    family_of_subsets(Es, I, IS, Ps, L);
family_of_subsets([], I, IS, Ps, L) ->
    family_of_subsets(Ps, [{I, usort(IS)} | L]);
family_of_subsets(_E, _I, _IS, _Ps, _L) ->
    error.

%% Type = OrderedSetType
%%      | SetType
%%      | atom                       % an atom
%% OrderedSetType = {Type, ..., Type}
%% SetType = [ElementType]           % list of exactly one element
%% ElementType = '_'                 % any type (implies empty set)
%%             | Type

is_type(?ATOM_TYPE) ->
    true;
is_type(?SET_OF(T)) ->
    is_element_type(T);
is_type(T) when tuple(T) ->
    is_types(size(T), T);
is_type(_T) ->
    false.

is_types(0, _T) ->
    true;
is_types(I, T) ->
    case is_type(element(I, T)) of
	true -> is_types(I-1, T);
	false -> false
    end.

is_element_type(?ANYTYPE) ->
    true;
is_element_type(T) ->
    is_type(T).

setify(L, ?SET_OF(?ATOM_TYPE)) ->
    from_list(L, L);
setify(L, ?SET_OF(Type0)) ->
    case catch is_no_lists(Type0) of
	{'EXIT', _} ->
	    case catch create(L, Type0, Type0, []) of
		{?SET_OF(Type), Set} -> ?SET(Set, Type);
		error -> error
	    end;
	N when integer(N) ->
	    relation(L, L, N);
	Sizes ->
	    make_oset(L, Sizes, L, Type0)
    end;
setify(E, Type0) ->
    case catch make_element(E, Type0, Type0) of
	{Type, OrdSet} -> ?ORDSET(OrdSet, Type);
	Error -> Error
    end.

is_no_lists(T) when tuple(T) -> 
   Sz = size(T),
   is_no_lists(T, Sz, Sz, []).

is_no_lists(_T, 0, Sz, []) ->
   Sz;
is_no_lists(_T, 0, Sz, L) ->
   {Sz, L};
is_no_lists(T, I, Sz, L) when element(I, T) == atom ->
   is_no_lists(T, I-1, Sz, L);
is_no_lists(T, I, Sz, L) ->
   is_no_lists(T, I-1, Sz, [{I,is_no_lists(element(I, T))} | L]).

create([E | Es], T, T0, L) ->
    {NT, S} = make_element(E, T, T0),
    create(Es, NT, T0, [S | L]);
create([], T, _T0, L) ->
    {?SET_OF(T), usort(L)};
create(_C, _T, _T0, _L) ->
    throw(error).

make_element(C, ?ANYTYPE, _T0) ->
    make_element(C);
make_element(C, ?ATOM_TYPE, ?ANYTYPE) when constant(C) ->
    {?ATOM_TYPE, C};
make_element(C, ?ATOM_TYPE, ?ATOM_TYPE) ->
    {?ATOM_TYPE, C};
make_element(T, TT, ?ANYTYPE) when tuple(T), tuple(TT), size(T) == size(TT) ->
    make_tuple(tuple_to_list(T), tuple_to_list(TT), [], [], ?ANYTYPE);
make_element(T, TT, T0) when tuple(T), tuple(TT), size(T) == size(TT) ->
    make_tuple(tuple_to_list(T), tuple_to_list(TT), [], [], tuple_to_list(T0));
make_element(L, [LT], ?ANYTYPE) when list(L) ->
    create(L, LT, ?ANYTYPE, []);
make_element(L, [LT], [T0]) when list(L) ->
    create(L, LT, T0, []);
make_element(_E, _T, _T0) ->
    throw(error).

make_tuple([E | Es], [T | Ts], NT, L, T0) when T0 == ?ANYTYPE ->
    {ET, ES} = make_element(E, T, T0),
    make_tuple(Es, Ts, [ET | NT], [ES | L], T0);
make_tuple([E | Es], [T | Ts], NT, L, [T0 | T0s]) ->
    {ET, ES} = make_element(E, T, T0),
    make_tuple(Es, Ts, [ET | NT], [ES | L], T0s);
make_tuple([], [], NT, L, _T0s) ->
    {list_to_tuple(lists:reverse(NT)), list_to_tuple(lists:reverse(L))}.

%% Derive type.
make_element(C) when constant(C) ->
    {?ATOM_TYPE, C};
make_element(T) when tuple(T) ->
    make_tuple(tuple_to_list(T), [], []);
make_element(L) when list(L) ->
    create(L, ?ANYTYPE, ?ANYTYPE, []).

make_tuple([E | Es], T, L) ->
    {ET, ES} = make_element(E),
    make_tuple(Es, [ET | T], [ES | L]);
make_tuple([], T, L) ->
    {list_to_tuple(lists:reverse(T)),  list_to_tuple(lists:reverse(L))}.

make_oset([T | Ts], Szs, L, Type) ->
    case test_oset(Szs, T, T) of
	true -> make_oset(Ts, Szs, L, Type);
        error -> error
    end;
make_oset([], _Szs, L, Type) ->
    ?SET(usort(L), Type);
make_oset(_, _Szs, _L, _Type) ->
    error.

%% Optimization. Avoid re-building (nested) tuples.
test_oset({Sz,Args}, T, T0) when tuple(T), size(T) == Sz ->
    test_oset_args(Args, T, T0);
test_oset(Sz, T, _T0) when tuple(T), size(T) == Sz ->
    true;
test_oset(_Sz, _T, _T0) ->
    error.

test_oset_args([{Arg,Szs} | Ss], T, T0) ->
    case test_oset(Szs, element(Arg, T), T0) of
       true -> test_oset_args(Ss, T, T0);
       Error -> Error
    end;
test_oset_args([], _T, _T0) ->
    true.

unify_types(T, T) -> T;
unify_types(Type1, Type2) ->
    catch unify_types1(Type1, Type2).

unify_types1(?ATOM_TYPE, ?ATOM_TYPE) ->
    ?ATOM_TYPE;
unify_types1(?ANYTYPE, Type) ->
    Type;
unify_types1(Type, ?ANYTYPE) ->
    Type;
unify_types1(?SET_OF(Type1), ?SET_OF(Type2)) ->
    [unify_types1(Type1, Type2)];
unify_types1(T1, T2) when tuple(T1), tuple(T2), size(T1) == size(T2) ->
    unify_typesl(size(T1), T1, T2, []);
unify_types1(_T1, _T2) ->
    throw(error).

unify_typesl(0, _T1, _T2, L) ->
    list_to_tuple(L);
unify_typesl(N, T1, T2, L) ->
    T = unify_types1(element(N, T1), element(N, T2)),
    unify_typesl(N-1, T1, T2, [T | L]).

match_types(T, T) -> true;
match_types(Type1, Type2) -> match_types1(Type1, Type2).

match_types1(?ATOM_TYPE, ?ATOM_TYPE) -> 
    true;
match_types1(?ANYTYPE, _) -> 
    true;
match_types1(_, ?ANYTYPE) -> 
    true;
match_types1(?SET_OF(Type1), ?SET_OF(Type2)) -> 
    match_types1(Type1, Type2);
match_types1(T1, T2) when tuple(T1), tuple(T2), size(T1) == size(T2) ->
    match_typesl(size(T1), T1, T2);
match_types1(_T1, _T2) ->
    false.

match_typesl(0, _T1, _T2) ->
    true;
match_typesl(N, T1, T2) ->
    case match_types1(element(N, T1), element(N, T2)) of
	true  -> match_typesl(N-1, T1, T2);
	false -> false
    end.

list_of_sets([S | Ss], Type, L) ->
    list_of_sets(Ss, Type, [?SET(S, Type) | L]);
list_of_sets([], _Type, L) ->
    lists:reverse(L).

list_of_ordsets([S | Ss], Type, L) ->
    list_of_ordsets(Ss, Type, [?ORDSET(S, Type) | L]);
list_of_ordsets([], _Type, L) ->
    lists:reverse(L).

tuple_of_sets([S | Ss], [?SET_OF(Type) | Types], L) ->
    tuple_of_sets(Ss, Types, [?SET(S, Type) | L]);
tuple_of_sets([S | Ss], [Type | Types], L) ->
    tuple_of_sets(Ss, Types, [?ORDSET(S, Type) | L]);
tuple_of_sets([], [], L) ->
    list_to_tuple(lists:reverse(L)).

specification([E | Es], F, L) ->
    case F(E) of
	true ->
	    specification(Es, F, [E | L]);
	false ->
	    specification(Es, F, L)
    end;
specification([], _F, L) ->
    lists:reverse(L).

%% Elements from the first list are kept.
intersection([H1 | T1], [H2 | T2], L) ->
    if 
	H1 < H2 ->
            intersection1(T1, H2, T2, L);
	H1 == H2 ->
	    intersection(T1, T2, [H1 | L]);
	true ->
            intersection2(T2, H1, T1, L)
    end;
intersection(_, _, L) ->
    lists:reverse(L).

intersection1([H1 | T1], H2, T2, L) ->
    if 
	H1 < H2 ->
            intersection1(T1, H2, T2, L);
	H1 == H2 ->
	    intersection(T1, T2, [H1 | L]);
	true ->
            intersection2(T2, H1, T1, L)
    end;
intersection1(_, _, _, L) ->
    lists:reverse(L).

intersection2([H2 | T2], H1, T1, L) ->
    if 
	H1 < H2 ->
            intersection1(T1, H2, T2, L);
	H1 == H2 ->
	    intersection(T1, T2, [H1 | L]);
	true ->
            intersection2(T2, H1, T1, L)
    end;
intersection2(_, _, _, L) ->
    lists:reverse(L).

difference([H1 | T1], [H2 | T2], L) ->
    if 
        H1 < H2 ->
            diff(T1, H2, T2, [H1 | L]);
        H1 == H2 ->
            difference(T1, T2, L);
        true ->
            diff2(T2, H1, T1, L)
    end;
difference(L1, _, L) ->
    lists:reverse(L, L1).

diff([H1 | T1], H2, T2, L) ->
    if 
        H1 < H2 ->
            diff(T1, H2, T2, [H1 | L]);
        H1 == H2 ->
            difference(T1, T2, L);
        true ->
            diff2(T2, H1, T1, L)
    end;
diff(_, _, _, L) ->
    lists:reverse(L).

diff2([H2 | T2], H1, T1, L) ->
    if 
        H1 < H2 ->
            diff(T1, H2, T2, [H1 | L]);
        H1 == H2 ->
            difference(T1, T2, L);
        true ->
            diff2(T2, H1, T1, L)
    end;
diff2(_, H1, T1, L) ->
    lists:reverse(L, [H1 | T1]).

symdiff([E1 | S1], S2, L) ->
    symdiff2(S2, E1, S1, L);
symdiff(_, S2, L) ->
    lists:reverse(L, S2).

symdiff1([E1 | S1], E2, S2, L) ->
    if 
	E1 < E2 ->
            symdiff1(S1, E2, S2, [E1 | L]);
	E1 == E2 ->
	    symdiff(S1, S2, L);
	true ->
            symdiff2(S2, E1, S1, [E2 | L])
    end;
symdiff1(_, E2, S2, L) ->
    lists:reverse(L, [E2 | S2]).

symdiff2([E2 | S2], E1, S1, L) ->
    if 
	E1 < E2 ->
            symdiff1(S1, E2, S2, [E1 | L]);
	E1 == E2 ->
	    symdiff(S1, S2, L);
	true ->
            symdiff2(S2, E1, S1, [E2 | L])
    end;
symdiff2(_, E1, S1, L) ->
    lists:reverse(L, [E1 | S1]).

prod([[E | Es] | Xs], T, L) ->
    prod(Es, Xs, T, prod(Xs, [E | T], L));
prod([], T, L) ->
    [list_to_tuple(lists:reverse(T)) | L].

prod([E | Es], Xs, T, L) ->
    prod(Es, Xs, T, prod(Xs, [E | T], L));
prod([], _Xs, _E, L) ->
    L.

subset([E1 | S1], [E2 | S2]) ->
    if
	E1 < E2 ->
	    false;
	E1 == E2 ->
	    subset(S1, S2);
	true ->
	    subset(S2, E1, S1)
    end;
subset([], _) ->
    true;
subset(_, []) ->
    false.

subset([E2 | S2], E1, S1) ->
    if
	E1 < E2 ->
	    false;
	E1 == E2 ->
	    subset(S1, S2);
	true ->
	    subset(S2, E1, S1)
    end;
subset([], _, _) ->
    false.

disjoint([B | Bs], A, As) ->
    if
        A < B ->
            disjoint(As, B, Bs);
        A == B ->
            false;
        true ->
            disjoint(Bs, A, As)
    end;
disjoint(_Bs, _A, _As) ->
    true.

%% Append sets that come in order, then "merge".
lunion([[] | Ls]) ->
    lunion(Ls);
lunion([S | Ss]) ->
    union_lists(lunion(Ss, lists:last(S), [S], []), []);
lunion([]) -> 
    [].

lunion([S | Ss], Last, SL, Ls) when hd(S) > Last ->
    lunion(Ss, lists:last(S), [S | SL], Ls);
lunion([S | Ss], _Last, SL, Ls) ->
    lunion(Ss, lists:last(S), [S], [lists:append(lists:reverse(SL)) | Ls]);
lunion([], _Last, SL, Ls) -> 
    [lists:append(lists:reverse(SL)) | Ls].

%% The empty list is always the first list, if present.
lintersection(_, []) ->
    [];
lintersection([S | Ss], S0) ->
    lintersection(Ss, intersection(S, S0, []));
lintersection([], S) ->
    S.

can_map([S | Ss], L) ->
    can_map(S, S, Ss, L);
can_map([], L) ->
    usort(L).

can_map([E | Es], S, Ss, L) ->
    can_map(Es, S, Ss, [{E, S} | L]);
can_map([], _S, Ss, L) ->
    can_map(Ss, L).

rel_to_fam([{X,Y} | S]) ->
    rel_to_fam(S, X, [Y], []);
rel_to_fam([]) ->
    [].

rel_to_fam([{X,Y} | S], X0, YL, L) when X0 == X ->
    rel_to_fam(S, X0, [Y | YL], L);
rel_to_fam([{X,Y} | S], X0, YL, L) ->
    rel_to_fam(S, X, [Y], [{X0,lists:reverse(YL)} | L]);
rel_to_fam([], X, YL, L) ->
    lists:reverse([{X,lists:reverse(YL)} | L]).

dom([{X,_} | Es], L) ->
    dom(Es, [X | L]);
dom([], L) ->
    usort(false, L).

ran([{_,Y} | Es], L) ->
    ran(Es, [Y | L]);
ran([], L) ->
    usort(true, L).

compose(A, B) ->
    usort(compose1(A, B)).

compose1([{Ay,Ax} | A], B) ->
    usort(compose1(B, Ay, Ax, A, []));
compose1(_A, _B) ->
    [].

compose1([{Bx,By} | B], Ay, Ax, A, L) ->
    if 
	Ay < Bx ->
	    compose2(A, Bx, By, B, L);
	Ay == Bx ->
            compose2(A, Bx, By, B, compose(Ay, Ax, B, [{Ax,By} | L]));
	true ->
	    compose1(B, Ay, Ax, A, L)
    end;
compose1(_B, _Ay, _Ax, _A, L) ->
    L.

compose2([{Ay, Ax} | A], Bx, By, B, L) ->
    if 
	Ay < Bx ->
	    compose2(A, Bx, By, B, L);
	Ay == Bx ->
            compose2(A, Bx, By, B, compose(Ay, Ax, B, [{Ax,By} | L]));
	true ->
	    compose1(B, Ay, Ax, A, L)
    end;
compose2(_, _, _, _, L) ->
    L.

compose(Ay, Ax, [{Bx,By} | B], L) when Ay == Bx ->
    compose(Ay, Ax, B, [{Ax,By} | L]);
compose(_Ay, _Ax, _B, L) ->	    
    L.

inverse([{A,B} | X], L) ->
    inverse(X, [{B,A} | L]);
inverse([], L) ->
    lists:sort(L).

rel_restrict([E = {X, _Y} | Es], _E0, X0, L) when X /= X0 ->
    rel_restrict(Es, E, X, L);
rel_restrict([E | Es], E0, X0, L) ->
    rel_restrict(Es, X0, [E, E0 | L]);
rel_restrict([], _E0, _X0, L) ->
    lists:reverse(L).

rel_restrict([E = {X, _Y} | Es], X0, L) when X == X0 ->
    rel_restrict(Es, X0, [E | L]);
rel_restrict([E = {X, _Y} | Es], _X0, L) ->
    rel_restrict(Es, E, X, L);
rel_restrict([], _X0, L) ->
    lists:reverse(L).

strict([{E,E} | Es], L) ->
    strict(Es, L);
strict([E | Es], L) ->
    strict(Es, [E | L]);
strict([], L) ->
    lists:reverse(L).

weak(Es) ->
    %% Not very efficient...
    weak(Es, ran(Es, []), []).

weak(Es=[{X,_} | _], [Y | Ys], L) when X > Y ->
    weak(Es, Ys, [{Y,Y} | L]);
weak(Es=[{X,_} | _], [X | Ys], L) ->
    weak(Es, Ys, L);
weak([E={X,Y} | Es], Ys, L) when X > Y ->
    weak1(Es, Ys, [E | L], X);
weak([E={X,X} | Es], Ys, L) ->
    weak2(Es, Ys, [E | L], X);
weak([E={X,_Y} | Es], Ys, L) -> % when X < _Y
    weak2(Es, Ys, [E, {X,X} | L], X);
weak([], [Y | Ys], L) ->
    weak([], Ys, [{Y,Y} | L]);
weak([], [], L) ->
    lists:reverse(L).

weak1([E={X,Y} | Es], Ys, L, X) when X > Y ->
    weak1(Es, Ys, [E | L], X);
weak1([E={X,X} | Es], Ys, L, X) ->
    weak2(Es, Ys, [E | L], X);
weak1([E={X,_Y} | Es], Ys, L, X) -> % when X < Y 
    weak2(Es, Ys, [E, {X,X} | L], X);
weak1(Es, Ys, L, X) ->
    weak(Es, Ys, [{X,X} | L]).

weak2([E={X,_Y} | Es], Ys, L, X) -> % when X < Y 
    weak2(Es, Ys, [E | L], X);
weak2(Es, Ys, L, _X) ->
    weak(Es, Ys, L).

function([{E,_} | Es], E0) when E /= E0 ->
    function(Es, E);
function(L, _E) ->
    L == [].

restrict([Key | Keys], Tuples) ->
    restrict(Tuples, Key, Keys, []);
restrict(_Keys, _Tuples) ->
    [].

restrict([{K,E} | Ts], Key, Keys, L) ->
    if 
	K < Key ->
	    restrict(Ts, Key, Keys, L);
	K == Key ->
	    restrict(Ts, Key, Keys, [E | L]);
	true ->
	    restrict(Keys, K, E, Ts, L)
    end;
restrict(_Ts, _Key, _Keys, L) ->
    L.
    
restrict([Key | Keys], K, E, Ts, L) ->
    if 
	K < Key ->
	    restrict(Ts, Key, Keys, L);
	K == Key ->
	    restrict(Ts, Key, Keys, [E | L]);
	true ->
	    restrict(Keys, K, E, Ts, L)
    end;
restrict(_Keys, _K, _E, _Ts, L) ->
    L.

diff_restrict([Key | Keys], Tuples) ->
    diff_restrict(Tuples, Key, Keys, []);
diff_restrict(_Keys, Tuples) ->
    diff_restrict_tail(Tuples, []).

diff_restrict([{K,E} | Ts], Key, Keys, L) ->
    if 
	K < Key ->
	    diff_restrict(Ts, Key, Keys, [E | L]);
	K == Key ->
	    diff_restrict(Ts, Key, Keys, L);
	true ->
	    diff_restrict(Keys, K, E, Ts, L)
    end;
diff_restrict(_Ts, _Key, _Keys, L) ->
    L.
    
diff_restrict([Key | Keys], K, E, Ts, L) ->
    if 
	K < Key ->
	    diff_restrict(Ts, Key, Keys, [E | L]);
	K == Key ->
	    diff_restrict(Ts, Key, Keys, L);
	true ->
	    diff_restrict(Keys, K, E, Ts, L)
    end;
diff_restrict(_Keys, _K, E, Ts, L) ->
    diff_restrict_tail(Ts, [E | L]).

diff_restrict_tail([{_K,E} | Ts], L) ->
    diff_restrict_tail(Ts, [E | L]);
diff_restrict_tail(_Ts, L) ->
    L.

project([T | Ts], Fun, L) ->
    project(Ts, Fun, [{T, Fun(T)} | L]);
project([], _Fun, L) ->
    lists:reverse(L).

replace([], _F, L) ->
    usort(L);
replace([E | Es], F, L) ->
    replace(Es, F, [F(E) | L]).

compose_n(RT, G, EmptyG, IsG) ->
    RL = tuple_to_list(RT),
    case domain_type(RL, ?ANYTYPE) of
	Error = {error, _Reason} -> 
	    Error;
	DType ->
	    Empty = lists:any(fun is_empty_set/1, RL) or EmptyG or (RT == {}),
	    RType = range_type(RL, []),
	    Type = ?BINREL(DType, RType),
	    Comp = 
		case Empty of
		    true when RType == ?ANYTYPE; DType == ?ANYTYPE ->
			empty_set();
		    true ->
			?SET([], Type);
		    false ->
			TL = ?LIST((comp_n(RL))),
			Sz = size(RT),
			Fun = fun({X,A}) -> {X, flat(Sz, A, [])} end,
			?SET(lists:map(Fun, TL), Type)
		end,
	    case IsG of
		true  -> composition(Comp, G);
		false -> Comp
	    end
    end.

comp_n([R | Rs]) ->
    comp_n(Rs, R, fun({A,_}) -> A end).

comp_n([], R, _Fun) ->
    R;
comp_n([R | Rs], R0, Fun) ->
    ?BINREL(DT, _) = ?TYPE(R),
    T = ?SET(inverse_projection(?LIST(R0), Fun, reverse), 
	     ?BINREL(DT, ?TYPE(R))),
    R1 = composition1(T, R),
    NR = substitution(fun({{X,A},AS}) -> {X,{A,AS}} end, R1),
    comp_n(Rs, NR, Fun).

flat(1, A, L) ->
    list_to_tuple([A | L]);
flat(N, {T,A}, L) ->
    flat(N-1, T, [A | L]).

domain_type([T | Ts], T0) when ?IS_SET(T) ->
    case ?TYPE(T) of
	?ANYTYPE -> 
	    domain_type(Ts, T0);
	?BINREL(DT, _RT) -> 
	    case unify_types(T0, DT) of
		error -> {error, type_mismatch};
		T1 -> domain_type(Ts, T1)
	    end;
	_ -> {error, badarg}
    end;
domain_type([], T0) ->
    T0.

range_type([T | Ts], L) ->
    case ?TYPE(T) of
	?ANYTYPE -> 
	    ?ANYTYPE;
	?BINREL(_DT, RT) -> 
	    range_type(Ts, [RT | L])
    end;
range_type([], L) -> 
    list_to_tuple(lists:reverse(L)).

mul_comp([T | Ts], I, R) when ?IS_SET(T) ->
    P = raise_element(R, I),
    F = composition1(P, T),
    [F | mul_comp(Ts, I+1, R)];
mul_comp([], _I, _R) ->
    [].

raise_element(R, _I) when ?TYPE(R) == ?ANYTYPE ->
    R;
raise_element(R, I) ->
    L = inverse_projection(?LIST(R), fun(E) -> element(I, E) end, I =/= 1),
    Type = ?TYPE(R),
    ?SET(L, ?BINREL(?REL_TYPE(Type, I), Type)).

join_element(E1, E2) ->
    [_ | L1] = tuple_to_list(E1),
    [_ | L2] = tuple_to_list(E2),
    list_to_tuple(L1 ++ L2).

join_element(E, I, E2, I2) ->
    join_element(tuple_to_list(E), I, 1, E2, I2).

join_element([A | As], I, C, E2, I2) when I /= C ->
    [A | join_element(As, I, C+1, E2, I2)];
join_element([_A | As], _I, _C, [], []) -> % when I == C
    As;
join_element([_A | As], I, C, E2, I2) -> % when I == C
    join_element(As, I, C+1, E2, I2);
join_element([], _I, _C, E2, I2) ->
    join_element(E2, I2, [], []).

fam_to_rel([{X,S} | F], L) ->
    fam_to_rel(S, X, F, L);
fam_to_rel([], L) ->
    lists:reverse(L).

fam_to_rel([Y | Ys], X, F, L) ->
    fam_to_rel(Ys, X, F, [{X,Y} | L]);
fam_to_rel([], _X, F, L) ->
    fam_to_rel(F, L).

%% Append families that come in order; sort everything on domain value;
%% collect range for each value; find union of ranges.
fam_lunion([[] | Fs]) ->
    fam_lunion(Fs);
fam_lunion([F]) ->
    F;
fam_lunion([F | Fs]) ->
    Es = union_lists(fam_lunion(Fs, lists:last(F), [F], []), []),
    P = fam_partition(Es, false),
    fam_unions(P, []);
fam_lunion([]) ->
    [].

%% Using >= may leave duplicates, or even elements in wrong order,
%% but union_lists in fam_unions fixes that.
fam_lunion([S=[{H,_} | _] | Ss], {Last, _}, SL, Ls) when H >= Last ->
    fam_lunion(Ss, lists:last(S), [S | SL], Ls);
fam_lunion([S | Ss], _Last, SL, Ls) ->
    fam_lunion(Ss, lists:last(S), [S], [lists:append(lists:reverse(SL)) | Ls]);
fam_lunion([], _Last, SL, Ls) -> 
    [lists:append(lists:reverse(SL)) | Ls].

fam_unions([{A, Ls} | Es], L) ->
    fam_unions(Es, [{A, union_lists(Ls, [])} | L]);
fam_unions([], L) ->
    L.

fam_union(F1 = [A1={A,AS} | AL], F2 = [B1={B,BS} | BL], L) ->
    if 
        A > B ->
            fam_union(F1, BL, [B1 | L]);
        A == B ->
            fam_union(AL, BL, [{A, umerge(AS, BS)} | L]);
        true ->
            fam_union(AL, F2, [A1 | L])
    end;
fam_union([], F2, L) ->
    lists:reverse(L, F2);
fam_union(F1, [], L) ->
    lists:reverse(L, F1).

fam_intersect(_, []) ->
    [];
fam_intersect([F | Fs], F0) ->
    fam_intersect(Fs, fam_intersect(F, F0, []));
fam_intersect([], F) ->
    F.

fam_intersect(F1 = [{A,AS} | AL], F2 = [{B,BS} | BL], L) ->
    if 
        A > B ->
            fam_intersect(F1, BL, L);
        A == B ->
            fam_intersect(AL, BL, [{A, intersection(AS, BS, [])} | L]);
        true ->
            fam_intersect(AL, F2, L)
    end;
fam_intersect(_, _, L) ->
    lists:reverse(L).

fam_difference(F1 = [A1={A,AS} | AL], F2 = [{B,BS} | BL], L) ->
    if 
        A > B ->
            fam_difference(F1, BL, L);
        A == B ->
            fam_difference(AL, BL, [{A, difference(AS, BS, [])} | L]);
        true ->
            fam_difference(AL, F2, [A1 | L])
    end;
fam_difference(F1, _, L) ->
    lists:reverse(L, F1).

fam_partition([{K,Vs} | Ts], Sort) ->
    fam_partition(Ts, K, [Vs], [], Sort);
fam_partition([], _Sort) ->
    [].

fam_partition([{K1,V} | Ts], K, Vs, P, S) when K1 == K ->
    fam_partition(Ts, K, [V | Vs], P, S);
fam_partition([{K1,V} | Ts], K, Vs, P, S) ->
    fam_partition(Ts, K1, [V], [{K, usort(S, Vs)} | P], S);
fam_partition([], K, Vs, P, S) ->
    [{K, usort(S, Vs)} | P].

sortl(L) ->
    usort(lists:map(fun usort/1, L)).

%% -> empty | error | true | false | reverse
check_fun(T, _F, _Dups) when T == ?ANYTYPE ->
    empty;
check_fun(T, F, Dups) ->
    {NT, MaxI} = number_tuples(T, 1),
    case catch lists:flatten(tuple2list(F(NT))) of
	{'EXIT', _} ->
	    error;
	L ->
	    case catch has_hole(L, 1) of
		DoSort when Dups == false; DoSort == true ->
		    DoSort;
		DoSort when length(L) =:= MaxI-1 ->
		    reverse;
		_Else ->
		    false
	    end
    end.

number_tuples(T, N) when tuple(T) ->
    {L, NN} = lists:mapfoldl(fun number_tuples/2, N, tuple_to_list(T)),
    {list_to_tuple(L), NN};
number_tuples(_, N) ->
    {N, N+1}.

tuple2list(T) when tuple(T) ->
    lists:map(fun tuple2list/1, tuple_to_list(T));
tuple2list(C) ->
    [C].

has_hole([I | Is], I0) when I =< I0 -> has_hole(Is, max(I+1, I0));
has_hole([], _I) -> false;
has_hole(_, _) -> true.

max(A, B) when A >= B -> A;
max(_A, B) -> B.

setify_apply(R, Fun) ->
    case apply_graph_fun(R, Fun) of
	Error = {error, _} -> Error;
	{Type, L} -> ?SET(sortl(L), ?SET_OF(Type))
    end.

apply_graph_fun(R, Fun) ->
    case ?TYPE(R) of
	?ANYTYPE -> {?ANYTYPE, []};
	?BINREL(DT, RT) -> 
	    case unify_types(DT, RT) of
		error -> 
		    {error, type_mismatch};
		Type ->  
		    G = digraph:new(),
		    Reply = (catch apply_gfun(G, R, Fun)),
		    true = digraph:delete(G),
		    case Reply of
			{'EXIT', E} ->
			    exit(E);
			_Else ->
			    {Type, Reply}
		    end
	    end;
	_Else -> {error, badarg}
    end.

apply_gfun(G, Set, Fun) ->
    AddFun = fun({From, To}) -> 
		     digraph:add_vertex(G, From),
		     digraph:add_vertex(G, To),
		     digraph:add_edge(G, From, To)
	     end,
    lists:foreach(AddFun, ?LIST(Set)), 
    digraph_utils:Fun(G).

inverse_projection(L, Fun, Sort) ->
    %% One easily sees that the inverse of the tuples created by
    %% applying Fun need to be sorted iff the tuples created by Fun
    %% need to be sorted.
    usort(Sort, substitute(L, fun(R) -> {Fun(R), R} end, [])).

substitute([E | Es], F, L) ->
    substitute(Es, F, [F(E) | L]);
substitute([], _F, L) ->
    L.

sets_to_list(Ss) ->
    lists:map(fun(S) when ?IS_SET(S) -> ?LIST(S) end, Ss).

types([], L) ->
    list_to_tuple(lists:reverse(L));
types([S | _Ss], _L) when ?TYPE(S) == ?ANYTYPE ->
    ?ANYTYPE;
types([S | Ss], L) ->
    types(Ss, [?TYPE(S) | L]).

%% usort(ToDo, L) -> [term()]
%%    ToDo = true | false | reverse
%%    L = [term()]
%% 
%% Duplicates in L are removed if ToDo is equal to 'true' or 'false'.
%%
usort(_ToDo, []) ->
    [];
usort(reverse, L) ->
    lists:reverse(L);
usort(false, [E | Es]) ->
    runiq(Es, E, []);
usort(true, L) ->
    usort(L).

runiq([E|Es], E, L) ->
    runiq(Es, E, L);
runiq([E1|Es], E, L) ->
    runiq(Es, E1, [E|L]);
runiq([], E, L) ->
    [E | L].

split([A | L], Min, Max, Low, High, Acc) when A > Max ->
    split(L, Min, A, Low, [A | High], Acc);
split([A | L], Min, Max, Low, High, Acc) when A < Min ->
    split(L, A, Max, [A | Low], High, Acc);
split([A | L], Min, Max, Low, High, Acc) when A == Min; A == Max ->
    split(L, Min, Max, Low, High, Acc);
split([A | L], _Min, _Max, Low, High, Acc) ->
    split(L, A, A, [A], [], [combine(Low, High) | Acc]);
split([], _Min, _Max, Low, High, Acc) ->
    [combine(Low, High) | Acc].

union_lists([A, B | L], Acc) ->
    union_lists(L, [union(A, B, []) | Acc]);
union_lists([L], []) ->
    L;
union_lists([L], Acc) ->
    runion_lists([lists:reverse(L) | Acc], []);
union_lists([], Acc) ->
    runion_lists(Acc, []).

runion_lists([A, B | L], Acc) ->
    runion_lists(L, [runion(A, B, []) | Acc]);
runion_lists([L], Acc) ->
    union_lists([lists:reverse(L) | Acc], []);
runion_lists([], Acc) ->
    union_lists(Acc, []).

combine(L1, []) ->
    L1;
combine(L1, L2) ->
    L1 ++ lists:reverse(L2).

%% Elements from the first list are kept.
union([H1 | T1], [H2 | T2], L) ->
    if
	H1 < H2 ->
	    union1(T1, H2, T2, [H1 | L]);
	H1 == H2 ->
	    union(T1, T2, [H1 | L]);
	true ->
	    union2(T2, H1, T1, [H2 | L])
    end;
union(S1, [], L) ->
    lists:reverse(S1, L);
union([], S2, L) ->
    lists:reverse(S2, L).

union1([H1 | T1], H2, T2, L) ->
    if
	H1 < H2 ->
	    union1(T1, H2, T2, [H1 | L]);
	H1 == H2 ->
	    union(T1, T2, [H1 | L]);
	true ->
	    union2(T2, H1, T1, [H2 | L])
    end;
union1([], H2, T2, L) ->
    lists:reverse(T2, [H2 | L]).

union2([H2 | T2], H1, T1, L) ->
    if
	H1 < H2 ->
	    union1(T1, H2, T2, [H1 | L]);
	H1 == H2 ->
	    union(T1, T2, [H1 | L]);
	true ->
	    union2(T2, H1, T1, [H2 | L])
    end;
union2([], H1, T1, L) ->
    lists:reverse(T1, [H1 | L]).

runion([H1 | T1], [H2 | T2], L) ->
    if
	H1 > H2 ->
	    runion(T1, H2, T2, [H1 | L]);
	H1 == H2 ->
	    runion(T1, T2, [H1 | L]);
	true ->
	    runion(T2, H1, T1, [H2 | L])
    end;
runion(S1, [], L) ->
    lists:reverse(S1, L);
runion([], S2, L) ->
    lists:reverse(S2, L).

runion([H1 | T1], H2, T2, L) ->
    if
	H1 > H2 ->
	    runion(T1, H2, T2, [H1 | L]);
	H1 == H2 ->
	    runion(T1, T2, [H1 | L]);
	true ->
	    runion(T2, H1, T1, [H2 | L])
    end;
runion([], H2, T2, L) ->
    lists:reverse(T2, [H2 | L]).
