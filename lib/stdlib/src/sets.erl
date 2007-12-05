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

%% We use the dynamic hashing techniques by Per-Åke Larsson as
%% described in "The Design and Implementation of Dynamic Hashing for
%% Sets and Tables in Icon" by Griswold and Townsend.  Much of the
%% terminology comes from that paper as well.

%% The segments are all of the same fixed size and we just keep
%% increasing the size of the top tuple as the table grows.  At the
%% end of the segments tuple we keep an empty segment which we use
%% when we expand the segments.  The segments are expanded by doubling
%% every time n reaches maxn instead of increasing the tuple one
%% element at a time.  It is easier and does not seem detrimental to
%% speed.  The same applies when contracting the segments.
%%
%% Note that as the order of the keys is undefined we may freely
%% reorder keys within in a bucket.

-module(sets).

%% Standard interface.
-export([new/0,is_set/1,size/1,to_list/1,from_list/1]).
-export([is_element/2,add_element/2,del_element/2]).
-export([union/2,union/1,intersection/2,intersection/1]).
-export([subtract/2,is_subset/2]).
-export([fold/3,filter/2]).

%% Note: mk_seg/1 must be changed too if seg_size is changed.
-define(seg_size, 16).
-define(max_seg, 32).
-define(expand_load, 5).
-define(contract_load, 3).

%% Define a hash set.  The default values are the standard ones.
-record(sets,
	{size=0,				%Number of elements
	 n=?seg_size,				%Number of active slots
	 maxn=?seg_size,			%Maximum slots
	 bso=?seg_size div 2,			%Buddy slot offset
	 exp_size=?seg_size * ?expand_load,	%Size to expand at
	 con_size=?seg_size * ?contract_load,	%Size to contract at
	 empty,					%Empty segment
	 segs					%Segments
	}).

%% new() -> Set.

new() ->
    Empty = mk_seg(?seg_size),
    #sets{empty=Empty,segs={Empty}}.

%% is_set(Set) -> bool().
%%  Return 'true' if Set is a set of elements, else 'false'.

is_set(#sets{}) -> true;
is_set(_) -> false.

%% size(Set) -> int().
%%  Return the number of elements in Set.

size(S) -> S#sets.size. 

%% to_list(Set) -> [Elem].
%%  Return the elements in Set as a list.

to_list(S) ->
    fold(fun (Elem, List) -> [Elem|List] end, [], S).

%% from_list([Elem]) -> Set.
%%  Build a set from the elements in List.

from_list(L) ->
    lists:foldl(fun (E, S) -> add_element(E, S) end, new(), L).

%% is_element(Element, Set) -> bool().
%%  Return 'true' if Element is an element of Set, else 'false'.

is_element(E, S) ->
    Slot = get_slot(S, E),
    Bkt = get_bucket(S, Slot),
    lists:member(E, Bkt).

%% add_element(Element, Set) -> Set.
%%  Return Set with Element inserted in it.

add_element(E, S0) ->
    Slot = get_slot(S0, E),
    {S1,Ic} = on_bucket(fun (B0) -> add_bkt_el(E, B0, B0) end, S0, Slot),
    maybe_expand(S1, Ic).

add_bkt_el(E, [E|_], Bkt) -> {Bkt,0};
add_bkt_el(E, [_|B], Bkt) ->
    add_bkt_el(E, B, Bkt);
add_bkt_el(E, [], Bkt) -> {[E|Bkt],1}.

%% del_element(Element, OrdSet) -> OrdSet.
%%  Return OrdSet but with Element removed.

del_element(E, S0) ->
    Slot = get_slot(S0, E),
    {S1,Dc} = on_bucket(fun (B0) -> del_bkt_el(E, B0) end, S0, Slot),
    maybe_contract(S1, Dc).

del_bkt_el(E, [E|Bkt]) -> {Bkt,1};
del_bkt_el(E, [Other|Bkt0]) ->
    {Bkt1,Dc} = del_bkt_el(E, Bkt0),
    {[Other|Bkt1],Dc};
del_bkt_el(_, []) -> {[],0}.

%% union(Set1, Set2) -> Set
%%  Return the union of Set1 and Set2.

union(S1, S2) when S1#sets.size < S2#sets.size ->
    fold(fun (E, S) -> add_element(E, S) end, S2, S1);
union(S1, S2) ->
    fold(fun (E, S) -> add_element(E, S) end, S1, S2).

%% union([Set]) -> Set
%%  Return the union of the list of sets.

union([S1,S2|Ss]) ->
    union1(union(S1, S2), Ss);
union([S]) -> S;
union([]) -> new().

union1(S1, [S2|Ss]) ->
    union1(union(S1, S2), Ss);
union1(S1, []) -> S1.

%% intersection(Set1, Set2) -> Set.
%%  Return the intersection of Set1 and Set2.

intersection(S1, S2) when S1#sets.size < S2#sets.size ->
    filter(fun (E) -> is_element(E, S2) end, S1);
intersection(S1, S2) ->
    filter(fun (E) -> is_element(E, S1) end, S2).

%% intersection([Set]) -> Set.
%%  Return the intersection of the list of sets.

intersection([S1,S2|Ss]) ->
    intersection1(intersection(S1, S2), Ss);
intersection([S]) -> S.

intersection1(S1, [S2|Ss]) ->
    intersection1(intersection(S1, S2), Ss);
intersection1(S1, []) -> S1.

%% subtract(Set1, Set2) -> Set.
%%  Return all and only the elements of Set1 which are not also in
%%  Set2.

subtract(S1, S2) ->
    filter(fun (E) -> not is_element(E, S2) end, S1).

%% is_subset(Set1, Set2) -> bool().
%%  Return 'true' when every element of Set1 is also a member of
%%  Set2, else 'false'.

is_subset(S1, S2) ->
    fold(fun (E, Sub) -> Sub and is_element(E, S2) end, true, S1).

%% fold(Fun, Accumulator, Set) -> Accumulator.
%%  Fold function Fun over all elements in Set and return Accumulator.

fold(F, Acc, D) -> fold_set(F, Acc, D).

%% filter(Fun, Set) -> Set.
%%  Filter Set with Fun.

filter(F, D) -> filter_set(F, D).


%% get_slot(Hashdb, Key) -> Slot.
%%  Get the slot.  First hash on the new range, if we hit a bucket
%%  which has not been split use the unsplit buddy bucket.

get_slot(T, Key) ->
    H = erlang:phash(Key, T#sets.maxn),
    if
	H > T#sets.n -> H - T#sets.bso;
	true -> H
    end.

%% get_bucket(Hashdb, Slot) -> Bucket.

get_bucket(T, Slot) -> get_bucket_s(T#sets.segs, Slot).

%% on_bucket(Fun, Hashdb, Slot) -> {NewHashDb,Result}.
%%  Apply Fun to the bucket in Slot and replace the returned bucket.

on_bucket(F, T, Slot) ->
    SegI = ((Slot-1) div ?seg_size) + 1,
    BktI = ((Slot-1) rem ?seg_size) + 1,
    Segs = T#sets.segs,
    Seg = element(SegI, Segs),
    B0 = element(BktI, Seg),
    {B1,Res} = F(B0),				%Op on the bucket.
    {T#sets{segs=setelement(SegI, Segs, setelement(BktI, Seg, B1))},Res}.

%% fold_set(Fun, Acc, Dictionary) -> Dictionary.
%% filter_set(Fun, Dictionary) -> Dictionary.

%%  Work functions for fold and filter operations.  These traverse the
%%  hash structure rebuilding as necessary.  Note we could have
%%  implemented map and hash using fold but these should be faster.
%%  We hope!

fold_set(F, Acc, D) ->
    Segs = D#sets.segs,
    fold_segs(F, Acc, Segs, erlang:size(Segs)).

fold_segs(F, Acc, Segs, I) when I >= 1 ->
    Seg = element(I, Segs),
    fold_segs(F, fold_seg(F, Acc, Seg, erlang:size(Seg)), Segs, I-1);
fold_segs(_, Acc, _, _) -> Acc.

fold_seg(F, Acc, Seg, I) when I >= 1 ->
    fold_seg(F, fold_bucket(F, Acc, element(I, Seg)), Seg, I-1);
fold_seg(_, Acc, _, _) -> Acc.

fold_bucket(F, Acc, [E|Bkt]) ->
    fold_bucket(F, F(E, Acc), Bkt);
fold_bucket(_, Acc, []) -> Acc.

filter_set(F, D) ->
    Segs0 = tuple_to_list(D#sets.segs),
    {Segs1,Fc} = filter_seg_list(F, Segs0, [], 0),
    maybe_contract(D#sets{segs=list_to_tuple(Segs1)}, Fc).

filter_seg_list(F, [Seg|Segs], Fss, Fc0) ->
    Bkts0 = tuple_to_list(Seg),
    {Bkts1,Fc1} = filter_bkt_list(F, Bkts0, [], Fc0),
    filter_seg_list(F, Segs, [list_to_tuple(Bkts1)|Fss], Fc1);
filter_seg_list(_, [], Fss, Fc) ->
    {lists:reverse(Fss, []),Fc}.

filter_bkt_list(F, [Bkt0|Bkts], Fbs, Fc0) ->
    {Bkt1,Fc1} = filter_bucket(F, Bkt0, [], Fc0),
    filter_bkt_list(F, Bkts, [Bkt1|Fbs], Fc1);
filter_bkt_list(_, [], Fbs, Fc) ->
    {lists:reverse(Fbs),Fc}.

filter_bucket(F, [E|Bkt], Fb, Fc) ->
    case F(E) of
	true -> filter_bucket(F, Bkt, [E|Fb], Fc);
	false -> filter_bucket(F, Bkt, Fb, Fc+1)
    end;
filter_bucket(_, [], Fb, Fc) -> {Fb,Fc}.

%% get_bucket_s(Segments, Slot) -> Bucket.
%% put_bucket_s(Segments, Slot, Bucket) -> NewSegments.

get_bucket_s(Segs, Slot) ->
    SegI = ((Slot-1) div ?seg_size) + 1,
    BktI = ((Slot-1) rem ?seg_size) + 1,
    element(BktI, element(SegI, Segs)).

put_bucket_s(Segs, Slot, Bkt) ->
    SegI = ((Slot-1) div ?seg_size) + 1,
    BktI = ((Slot-1) rem ?seg_size) + 1,
    Seg = setelement(BktI, element(SegI, Segs), Bkt),
    setelement(SegI, Segs, Seg).

maybe_expand(T0, Ic) when T0#sets.size + Ic > T0#sets.exp_size ->
    T = maybe_expand_segs(T0),			%Do we need more segments.
    N = T#sets.n + 1,				%Next slot to expand into
    Segs0 = T#sets.segs,
    Slot1 = N - T#sets.bso,
    B = get_bucket_s(Segs0, Slot1),
    Slot2 = N,
    [B1|B2] = rehash(B, Slot1, Slot2, T#sets.maxn),
    Segs1 = put_bucket_s(Segs0, Slot1, B1),
    Segs2 = put_bucket_s(Segs1, Slot2, B2),
    T#sets{size=T#sets.size + Ic,
	   n=N,
	   exp_size=N * ?expand_load,
	   con_size=N * ?contract_load,
	   segs=Segs2};
maybe_expand(T, Ic) -> T#sets{size=T#sets.size + Ic}.

maybe_expand_segs(T) when T#sets.n == T#sets.maxn ->
    T#sets{maxn=2 * T#sets.maxn,
	   bso=2 * T#sets.bso,
	   segs=expand_segs(T#sets.segs, T#sets.empty)};
maybe_expand_segs(T) -> T.

maybe_contract(T, Dc) when T#sets.size - Dc < T#sets.con_size,
			   T#sets.n > ?seg_size ->
    N = T#sets.n,
    Slot1 = N - T#sets.bso,
    Segs0 = T#sets.segs,
    B1 = get_bucket_s(Segs0, Slot1),
    Slot2 = N,
    B2 = get_bucket_s(Segs0, Slot2),
    Segs1 = put_bucket_s(Segs0, Slot1, B1 ++ B2),
    Segs2 = put_bucket_s(Segs1, Slot2, []),	%Clear the upper bucket
    N1 = N - 1,
    maybe_contract_segs(T#sets{size=T#sets.size - Dc,
			       n=N1,
			       exp_size=N1 * ?expand_load,
			       con_size=N1 * ?contract_load,
			       segs=Segs2});
maybe_contract(T, Dc) -> T#sets{size=T#sets.size - Dc}.

maybe_contract_segs(T) when T#sets.n == T#sets.bso ->
    T#sets{maxn=T#sets.maxn div 2,
	   bso=T#sets.bso div 2,
	   segs=contract_segs(T#sets.segs)};
maybe_contract_segs(T) -> T.

%% rehash(Bucket, Slot1, Slot2, MaxN) -> [Bucket1|Bucket2].
%%  Yes, we should return a tuple, but this is more fun.

rehash([E|T], Slot1, Slot2, MaxN) ->
    [L1|L2] = rehash(T, Slot1, Slot2, MaxN),
    case erlang:phash(E, MaxN) of
	Slot1 -> [[E|L1]|L2];
	Slot2 -> [L1|[E|L2]]
    end;
rehash([], _, _, _) -> [[]|[]].

%% mk_seg(Size) -> Segment.

mk_seg(16) -> {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]}.

%% expand_segs(Segs, EmptySeg) -> NewSegs.
%% contract_segs(Segs) -> NewSegs.
%%  Expand/contract the segment tuple by doubling/halving the number
%%  of segments.  We special case the powers of 2 upto 32, this should
%%  catch most case.  N.B. the last element in the segments tuple is
%%  an extra element containing a default empty segment.

expand_segs({B1}, Empty) ->
    {B1,Empty};
expand_segs({B1,B2}, Empty) ->
    {B1,B2,Empty,Empty};
expand_segs({B1,B2,B3,B4}, Empty) ->
    {B1,B2,B3,B4,Empty,Empty,Empty,Empty};
expand_segs({B1,B2,B3,B4,B5,B6,B7,B8}, Empty) ->
    {B1,B2,B3,B4,B5,B6,B7,B8,
     Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty};
expand_segs({B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,B14,B15,B16}, Empty) ->
    {B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,B14,B15,B16,
     Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,
     Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty};
expand_segs(Segs, Empty) ->
    list_to_tuple(tuple_to_list(Segs) ++ lists:duplicate(erlang:size(Segs), Empty)).

contract_segs({B1,_}) ->
    {B1};
contract_segs({B1,B2,_,_}) ->
    {B1,B2};
contract_segs({B1,B2,B3,B4,_,_,_,_}) ->
    {B1,B2,B3,B4};
contract_segs({B1,B2,B3,B4,B5,B6,B7,B8,_,_,_,_,_,_,_,_}) ->
    {B1,B2,B3,B4,B5,B6,B7,B8};
contract_segs({B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,B14,B15,B16,
	       _,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_}) ->
    {B1,B2,B3,B4,B5,B6,B7,B8,B9,B10,B11,B12,B13,B14,B15,B16};
contract_segs(Segs) ->
    Ss = erlang:size(Segs) div 2,
    list_to_tuple(lists:sublist(tuple_to_list(Segs), 1, Ss)).
