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
%%     $Id: ordsets.erl,v 1.1.1.1 1999/11/25 13:52:10 erlang Exp $
%%

-module(ordsets).

-export([new/0,is_set/1,size/1,to_list/1,from_list/1]).
-export([is_element/2,add_element/2,del_element/2]).
-export([union/2,union/1,intersection/2,intersection/1]).
-export([subtract/2,is_subset/2]).
-export([fold/3,filter/2]).

%% Depreciated interface.

-export([new_set/0,set_to_list/1,list_to_set/1,subset/2]).

%% new() -> Set.
%%  Return a new empty ordered set.

new() -> [].

%% is_set(Set) -> bool().
%%  Return 'true' if Set is an ordered set of elements, else 'false'.

is_set([E|Es]) -> is_set(Es, E);
is_set([]) -> true.

is_set([E2|Es], E1) when E1 < E2 ->
    is_set(Es, E2);
is_set([E2|Es], E1) -> false;
is_set([], E1) -> true.

%% size(OrdSet) -> int().
%%  Return the number of elements in OrdSet.

size(S) -> length(S).

%% to_list(OrdSet) -> [Elem].
%%  Return the elements in OrdSet as a list.

to_list(S) -> S.

%% from_list([Elem]) -> Set.
%%  Build an ordered set from the elements in List.

from_list([E|Es]) ->
    add_element(E, from_list(Es));
from_list([]) ->
    [].

%% is_element(Element, OrdSet) -> bool().
%%  Return 'true' if Element is an element of OrdSet, else 'false'.

is_element(E, [H|Es]) when E < H -> false;
is_element(E, [H|Es]) when E == H -> true;
is_element(E, [H|Es]) when E > H -> is_element(E, Es);
is_element(E, []) -> false.

%% add_element(Element, OrdSet) -> OrdSet.
%%  Return OrdSet with Element inserted in it.

add_element(E, [H|_]=Es) when E < H -> [E|Es];
add_element(E, [H|_]=Es) when E == H -> Es;
add_element(E, [H|Es]) when E > H ->[H|add_element(E, Es)];
add_element(E, []) ->[E].

%% del_element(Element, OrdSet) -> OrdSet.
%%  Return OrdSet but with Element removed.

del_element(E, [H|_]=Es) when E < H -> Es;
del_element(E, [H|Es]) when E == H -> Es;
del_element(E, [H|Es]) when E > H -> [H|del_element(E, Es)];
del_element(E, []) ->[].

%% union(OrdSet1, OrdSet2) -> OrdSet
%%  Return the union of OrdSet1 and OrdSet2.

union([H1|Es1], [H2|Es2]) when H1 < H2 ->
    [H1|union(Es1, [H2|Es2])];
union([H1|Es1], [H2|Es2]) when H1 == H2 ->
    [H1|union(Es1, Es2)];
union([H1|Es1], [H2|Es2]) when H1 > H2 ->
    [H2|union([H1|Es1], Es2)];
union([], Es2) -> Es2;
union(Es1, []) -> Es1.

%% union([OrdSet]) -> OrdSet
%%  Return the union of the list of ordered sets.

union([S1,S2|Ss]) ->
    union1(union(S1, S2), Ss);
union([S]) -> S;
union([]) -> [].

union1(S1, [S2|Ss]) -> union1(union(S1, S2), Ss);
union1(S1, []) -> S1.

%% intersection(OrdSet1, OrdSet2) -> OrdSet.
%%  Return the intersection of OrdSet1 and OrdSet2.

intersection([H1|Es1], [H2|Es2]) when H1 < H2 ->
    intersection(Es1, [H2|Es2]);
intersection([H1|Es1], [H2|Es2]) when H1 == H2 ->
    [H1|intersection(Es1, Es2)];
intersection([H1|Es1], [H2|Es2]) when H1 > H2 ->
    intersection([H1|Es1], Es2);
intersection([], Es2) ->
    [];
intersection(Es1, []) ->
    [].

%% intersection([OrdSet]) -> OrdSet.
%%  Return the intersection of the list of ordered sets.

intersection([S1,S2|Ss]) ->
    intersection1(intersection(S1, S2), Ss);
intersection([S]) -> S;
intersection([]) -> [].

intersection1(S1, [S2|Ss]) ->
    intersection1(intersection(S1, S2), Ss);
intersection1(S1, []) -> S1.

%% subtract(OrdSet1, OrdSet2) -> OrdSet.
%%  Return all and only the elements of OrdSet1 which are not also in
%%  OrdSet2.

subtract([H1|Es1], [H2|Es2]) when H1 < H2 ->
    [H1|subtract(Es1, [H2|Es2])];
subtract([H1|Es1], [H2|Es2]) when H1 == H2 ->
    subtract(Es1, Es2);
subtract([H1|Es1], [H2|Es2]) when H1 > H2 ->
    subtract([H1|Es1], Es2);
subtract([], Es2) -> [];
subtract(Es1, []) -> Es1.

%% is_subset(OrdSet1, OrdSet2) -> bool().
%%  Return 'true' when every element of OrdSet1 is also a member of
%%  OrdSet2, else 'false'.

is_subset([H1|Es1], [H2|Es2]) when H1 < H2 ->	%H1 not in Set2
    false;
is_subset([H1|Es1], [H2|Es2]) when H1 == H2 ->
    is_subset(Es1, Es2);
is_subset([H1|Es1], [H2|Es2]) when H1 > H2 ->
    is_subset([H1|Es1], Es2);
is_subset([], Es2) -> true;
is_subset(Es1, []) -> false.

%% fold(Fun, Accumulator, OrdSet) -> Accumulator.
%%  Fold function Fun over all elements in OrdSet and return Accumulator.

fold(F, Acc, [E|Es]) ->
    fold(F, F(E, Acc), Es);
fold(F, Acc, []) -> Acc.

%% filter(Fun, OrdSet) -> OrdSet.
%%  Filter OrdSet with Fun.

filter(F, [E|Es]) ->
    case F(E) of
	true -> [E|filter(F, Es)]; 
	false -> filter(F, Es)
    end;
filter(F, []) -> [].

%% Depreciated interface.

new_set() -> new().

set_to_list(S) -> to_list(S).

list_to_set(L) -> from_list(L).

subset(S1, S2) -> is_subset(S1, S2).
