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
%%     $Id $
%%
-module(lists).

-export([append/2, append/1, subtract/2, reverse/1,
	 nth/2, nthtail/2, prefix/2, suffix/2, last/1, 
	 seq/2, seq/3, sum/1, duplicate/2, min/1, max/1, sublist/2, sublist/3,
	 delete/2, sort/1, merge/1, merge/2, rmerge/2, merge3/3, rmerge3/3,
	 usort/1, umerge/1, umerge3/3, umerge/2, rumerge3/3, rumerge/2,
	 concat/1, flatten/1, flatten/2, flat_length/1, flatlength/1,
	 keydelete/3, keyreplace/4,
	 keysort/2, keymerge/3, rkeymerge/3, rukeymerge/3, 
	 ukeysort/2, ukeymerge/3, keymap/3, keymap/4]).

%% Bifs: member/2, reverse/2
%% Bifs: keymember/3, keysearch/3

-export([merge/3, rmerge/3, sort/2, umerge/3, rumerge/3, usort/2]).

-export([all/2,any/2,map/2,flatmap/2,foldl/3,foldr/3,filter/2,zf/2,
	 mapfoldl/3,mapfoldr/3,foreach/2,takewhile/2,dropwhile/2,splitwith/2]).
-export([all/3,any/3,map/3,flatmap/3,foldl/4,foldr/4,filter/3,zf/3,
	 mapfoldl/4,mapfoldr/4,foreach/3]).

%% member(X, L) -> (true | false)
%%  test if X is a member of the list L
%%  Now a BIF!

%member(X, [X|_]) -> true;
%member(X, [_|Y]) ->
%	member(X, Y);
%member(X, []) -> false.

%% append(X, Y) appends lists X and Y

append(L1, L2) -> L1 ++ L2.

%% append(L) appends the list of lists L

append([E]) -> E;
append([H|T]) -> H ++ append(T);
append([]) -> [].

%% subtract(List1, List2) subtract elements in List2 form List1.

subtract(L1, L2) -> L1 -- L2.

%% reverse(L) reverse all elements in the list L. Is now a BIF!

reverse([] = L) ->
    L;
reverse([_] = L) ->
    L;
reverse([A, B]) ->
    [B, A];
reverse([A, B | L]) ->
    lists:reverse(L, [B, A]).

%reverse([H|T], Y) ->
%    reverse(T, [H|Y]);
%reverse([], X) -> X.


%% nth(N, L) returns the N`th element of the list L
%% nthtail(N, L) returns the N`th tail of the list L

nth(1, [H|_]) -> H;
nth(N, [_|T]) when N > 1 ->
    nth(N - 1, T).

nthtail(1, [_|T]) -> T;
nthtail(N, [_|T]) when N > 1 ->
    nthtail(N - 1, T);
nthtail(0, L) when list(L) -> L.

%% prefix(Prefix, List) -> (true | false)

prefix([X|PreTail], [X|Tail]) ->
    prefix(PreTail, Tail);
prefix([], _List) -> true;
prefix(_,_) -> false.


%% suffix(Suffix, List) -> (true | false)

suffix(Suffix, Suffix) -> true;
suffix(Suffix, [_|Tail]) ->
    suffix(Suffix, Tail);
suffix(_, []) -> false.

%% last(List) returns the last element in a list.

last([E|Es]) -> last(E, Es).
last(_, [E|Es]) -> last(E, Es);
last(E, []) -> E.

%% seq(Min, Max) -> [Min,Min+1, ..., Max]
%% seq(Min, Max, Incr) -> [Min,Min+Incr, ..., Max]
%%  returns the sequence Min..Max
%%  Min <= Max and Min and Max must be integers

seq(Min, Max) when integer(Min), integer(Max), Min =< Max -> 
    seq(Min, Max, 1, []).

seq(Min, Max, Incr) when Min =< Max, Incr > 0 ->
    seq(Min, Min + ((Max-Min) div Incr) * Incr, Incr, []);
seq(Min, Max, Incr) when Min >= Max, Incr < 0 ->
    seq(Min, Min + ((Max-Min) div Incr) * Incr, Incr, []);
seq(M, M, 0) when integer(M) ->
    [M].

seq(Min, Min, _, L) -> [Min|L];
seq(Min, Max, I, L) -> seq(Min, Max-I, I, [Max|L]).

%% sum(L) suns the sum of the elements in L

sum(L)          -> sum(L, 0).
sum([H|T], Sum) -> sum(T, Sum + H);
sum([], Sum)    -> Sum.

%% duplicate(N, X) -> [X,X,X,.....,X]  (N times)
%%   return N copies of X

duplicate(N, X) when integer(N), N >= 0 -> duplicate(N, X, []).

duplicate(0, _, L) -> L;
duplicate(N, X, L) -> duplicate(N-1, X, [X|L]).


%% min(L) -> returns the minimum element of the list L

min([H|T]) -> min(T, H).

min([H|T], Min) when H < Min -> min(T, H);
min([_|T], Min)              -> min(T, Min);
min([],    Min)              -> Min. 

%% max(L) -> returns the maximum element of the list L

max([H|T]) -> max(T, H).

max([H|T], Max) when H > Max -> max(T, H);
max([_|T], Max)              -> max(T, Max);
max([],    Max)              -> Max.

%% sublist(List, Start, Length)
%%  Returns the sub-list starting at Start of length Length.

sublist(List, S, L) when integer(L), L >= 0 ->
    sublist(nthtail(S-1, List), L).

sublist(List, L) when integer(L), list(List) ->
    sublist_2(List, L).

sublist_2([H|T], L) when L > 0 ->
    [H|sublist_2(T, L-1)];
sublist_2(_, 0) ->
    [];
sublist_2(List, L) when list(List), L > 0 ->
    [].

%% delete(Item, List) -> List'
%%  Delete the first occurance of Item from the list L.

delete(Item, [Item|Rest]) -> Rest;
delete(Item, [H|Rest]) ->
    [H|delete(Item, Rest)];
delete(_, []) -> [].

%% sort(List) -> L
%%  sorts the list L

sort([X, Y | L] = L0) when X =< Y ->
    case L of
	[] -> 
	    L0;
	[Z] when Y =< Z ->
	    L0;
	[Z] when X =< Z ->
	    [X, Z, Y];
	[Z] ->
	    [Z, X, Y];
	_ when X == Y ->
	    sort_1(Y, L, [X]);
	_ ->
	    lists_sort:split_1(X, Y, L, [], [])
    end;
sort([X, Y | L]) ->
    case L of
	[] ->
	    [Y, X];
	[Z] when X =< Z ->
	    [Y, X | L];
	[Z] when Y =< Z ->
	    [Y, Z, X];
	[Z] ->
	    [Z, Y, X];
	_ ->
	    lists_sort:split_2(X, Y, L, [], [])
    end;
sort([_] = L) ->
    L;
sort([] = L) ->
    L.

sort_1(X, [Y | L], R) when X == Y ->
    sort_1(Y, L, [X | R]);
sort_1(X, [Y | L], R) when X < Y ->
    lists_sort:split_1(X, Y, L, R, []);
sort_1(X, [Y | L], R) ->
    lists_sort:split_2(X, Y, L, R, []);
sort_1(X, [], R) ->
    [X | R].

%% merge(List) -> L
%%  merges a list of sorted lists

merge(L) ->
    lists_sort:mergel(L, []).

%% merge3(X, Y, Z) -> L
%%  merges three sorted lists X, Y and Z

merge3(L1, [], L3) ->
   merge(L1, L3);
merge3(L1, L2, []) ->
   merge(L1, L2);
merge3(L1, [H2 | T2], [H3 | T3]) ->
   lists:reverse(lists_sort:merge3_1(L1, [], H2, T2, H3, T3), []).

%% rmerge3(X, Y, Z) -> L
%%  merges three reversed sorted lists X, Y and Z

rmerge3(L1, [], L3) ->
   rmerge(L1, L3);
rmerge3(L1, L2, []) ->
   rmerge(L1, L2);
rmerge3(L1, [H2 | T2], [H3 | T3]) ->
   lists:reverse(lists_sort:rmerge3_1(L1, [], H2, T2, H3, T3), []).

%% merge(X, Y) -> L
%%  merges two sorted lists X and Y

merge([], T2) ->
    T2;
merge([H1 | T1], T2) ->
    lists:reverse(lists_sort:merge2_2(T1, H1, T2, []), []).

%% rmerge(X, Y) -> L
%%  merges two reversed sorted lists X and Y

%% reverse(rmerge(reverse(A),reverse(B))) is equal to merge(I,A,B).
rmerge(T1, []) ->
    T1;
rmerge(T1, [H2 | T2]) ->
    lists:reverse(lists_sort:rmerge2_1(T1, H2, T2, []), []).

%% concat(L) concatinate the list representation of the elements
%%  in L - the elements in L can be atoms, integers of strings.
%%  Returns a list of characters.

concat(List) ->
    flatmap(fun thing_to_list/1, List).

thing_to_list(X) when integer(X) -> integer_to_list(X);
thing_to_list(X) when float(X)	 -> float_to_list(X);
thing_to_list(X) when atom(X)	 -> atom_to_list(X);
thing_to_list(X) when list(X)	 -> X.		%Assumed to be a string

%% flatten(List)
%% flatten(List, Tail)
%%  Flatten a list, adding optional tail.

flatten(List) ->
    flatten(List, [], []).

flatten(List, Tail) ->
    flatten(List, [], Tail).

flatten([H|T], Cont, Tail) when list(H) ->
    flatten(H, [T|Cont], Tail);
flatten([H|T], Cont, Tail) ->
    [H|flatten(T, Cont, Tail)];
flatten([], [H|Cont], Tail) ->
    flatten(H, Cont, Tail);
flatten([], [], Tail) ->
    Tail.

%% flat_length(List) (undocumented can be removed later)
%%  Calculate the length of a list of lists.

flat_length(List) -> flatlength(List).

%% flatlength(List)
%%  Calculate the length of a list of lists.

flatlength(List) ->
    flatlength(List, 0).

flatlength([H|T], L) when list(H) ->
    flatlength(H, flatlength(T, L));
flatlength([_|T], L) ->
    flatlength(T, L + 1);
flatlength([], L) -> L.

%% keymember(Key, Index, [Tuple]) Now a BIF!
%% keysearch(Key, Index, [Tuple]) Now a BIF!
%% keydelete(Key, Index, [Tuple])
%% keyreplace(Key, Index, [Tuple], NewTuple)
%% keysort(Index, [Tuple])
%% keymerge(Index, [Tuple], [Tuple])
%% ukeysort(Index, [Tuple])
%% ukeymerge(Index, [Tuple], [Tuple])
%% keymap(Function, Index, [Tuple])
%% keymap(Function, ExtraArgs, Index, [Tuple])

%keymember(K,N,L) when integer(N), N > 0 ->
%    keymember3(K,N,L).

%keymember3(Key, N, [T|Ts]) when element(N, T) == Key -> true;
%keymember3(Key, N, [T|Ts]) ->
%    keymember3(Key, N, Ts);
%keymember3(Key, N, []) -> false.

%keysearch(K,N,L) when integer(N), N > 0 ->
%    keysearch3(K,N,L).

%keysearch3(Key, N, [H|T]) when element(N, H) == Key ->
%    {value, H};
%keysearch3(Key, N, [H|T]) ->
%    keysearch3(Key, N, T);
%keysearch3(Key, N, []) -> false.

keydelete(K,N,L) when integer(N), N > 0 ->
    keydelete3(K,N,L).

keydelete3(Key, N, [H|T]) when element(N, H) == Key -> T;
keydelete3(Key, N, [H|T]) ->
    [H|keydelete3(Key, N, T)];
keydelete3(_, _, []) -> [].

keyreplace(K,N,L,New) when integer(N), N > 0 ->
    keyreplace3(K,N,L,New).

keyreplace3(Key, Pos, [Tup|Tail], New) when element(Pos, Tup) == Key ->
    [New|Tail];
keyreplace3(Key, Pos, [H|T], New) ->
    [H|keyreplace3(Key, Pos, T, New)];
keyreplace3(_, _, [], _) -> [].

keysort(Index, L) when integer(Index), Index > 0 ->
    case L of
	[] -> L;
	[_] -> L;
	[X, Y | T] ->
	    EX = element(Index, X),
	    EY = element(Index, Y),
	    if
		EX =< EY ->
		    lists_sort:keysplit_1(Index, X, EX, Y, EY, T, [], []);
		true ->
		    lists_sort:keysplit_2(Index, Y, EY, T, [X])
	    end
    end.

keymerge(Index, T1, L2) when integer(Index), Index > 0 -> 
    case L2 of
	[] ->
	    T1;
	[H2 | T2] ->
	    E2 = element(Index, H2),
	    M = lists_sort:keymerge2_1(Index, T1, E2, H2, T2, []),
	    lists:reverse(M, [])
    end.

%% reverse(rkeymerge(I,reverse(A),reverse(B))) is equal to keymerge(I,A,B).
rkeymerge(Index, T1, L2) when integer(Index), Index > 0 -> 
    case L2 of
	[] ->
	    T1;
	[H2 | T2] ->
	    E2 = element(Index, H2),
	    M = lists_sort:rkeymerge2_1(Index, T1, E2, H2, T2, []),
	    lists:reverse(M, [])
    end.

ukeysort(Index, L) when integer(Index), Index > 0 ->
    case L of
	[] -> L;
	[_] -> L;
	[X, Y | T] ->
	    ukeysort_1(Index, X, element(Index, X), Y, T)
    end.

ukeysort_1(I, X, EX, Y, L) when X == Y ->
    case L of
	[] ->
	    [X];
	[Z | T] ->
	    ukeysort_1(I, X, EX, Z, T)
    end;
ukeysort_1(I, X, EX, Y, L) ->
    EY = element(I, Y),
    if
	EX =< EY ->
	    lists_sort:ukeysplit_1(I, X, EX, Y, EY, L, [], []);
	true ->
	    lists_sort:ukeysplit_2(I, Y, EY, L, [X])
    end.

ukeymerge(Index, L1, T2) when integer(Index), Index > 0 ->
    case L1 of
	[] ->
	    T2;
	[H1 | T1] ->
	    E1 = element(Index, H1),
	    M = lists_sort:ukeymerge2_2(Index, T1, E1, H1, T2, []),
	    lists:reverse(M, [])
    end.

%% reverse(rukeymerge(I,reverse(A),reverse(B))) is equal to ukeymerge(I,A,B).
rukeymerge(Index, T1, L2) when integer(Index), Index > 0 ->
    case L2 of
	[] ->
	    T1;
	[H2 | T2] ->
	    E2 = element(Index, H2),
	    M = lists_sort:rukeymerge2_1(Index, T1, E2, T2, [], H2),
	    lists:reverse(M, [])
    end.

keymap(Fun, Index, [Tup|Tail]) ->
   [setelement(Index, Tup, Fun(element(Index, Tup)))|keymap(Fun, Index, Tail)];
keymap( _, _ , []) -> [].

keymap(Fun, ExtraArgs, Index, [Tup|Tail]) ->
   [setelement(Index, Tup, apply(Fun, [element(Index, Tup)|ExtraArgs]))|
    keymap(Fun, ExtraArgs, Index, Tail)];
keymap( _, _ , _, []) -> [].


%%% Suggestion from OTP-2948: sort and merge with Fun.
sort(_Fun, []) ->
    [];
sort(_Fun, [_] = L) ->
    L;
sort(Fun, [X, Y | T]) ->
    case Fun(X, Y) of
	true ->
	    lists_sort:fsplit_1(Y, X, Fun, T, [], []);
	false ->
	    lists_sort:fsplit_2(Y, T, Fun, [X])
    end.

merge(Fun, T1, [H2 | T2]) ->
    lists:reverse(lists_sort:fmerge2_1(T1, H2, Fun, T2, []), []);
merge(_Fun, T1, []) ->
    T1.

%% reverse(rmerge(F,reverse(A),reverse(B))) is equal to merge(F,A,B).
rmerge(Fun, T1, [H2 | T2]) ->
    lists:reverse(lists_sort:rfmerge2_1(T1, H2, Fun, T2, []), []);
rmerge(_Fun, T1, []) ->
    T1.

usort(_Fun, [_] = L) ->
    L;
usort(_Fun, [] = L) ->
    L;
usort(Fun, [X | L]) ->
    usort_1(Fun, X, L).

usort_1(Fun, X, [Y | L]) when X == Y ->
    if 
	L == [] ->
	    [X];
	true ->
	    usort_1(Fun, X, L)
    end;
usort_1(Fun, X, [Y | L]) ->
    case Fun(X, Y) of
	true ->
	    lists_sort:ufsplit_1(Y, X, Fun, L, [], []);
	false ->
	    lists_sort:ufsplit_2(Y, L, Fun, [X])
    end.

umerge(_Fun, [], T2) ->
    T2;
umerge(Fun, [H1 | T1], T2) ->
    lists:reverse(lists_sort:ufmerge2_2(H1, T1, Fun, T2, []), []).

%% reverse(rumerge(F,reverse(A),reverse(B))) is equal to umerge(F,A,B).
rumerge(_Fun, T1, []) ->
    T1;
rumerge(Fun, T1, [H2 | T2]) ->
    lists:reverse(lists_sort:rufmerge2_1(T1, H2, Fun, T2, []), []).

%% usort(List) -> L
%%  sorts the list L, removes duplicates

usort([X, Y | L] = L0) when X < Y ->
    case L of
	[] ->
	    L0;
	[Z] when Y < Z ->
	    L0;
	[Z] when Z < X ->
	    [Z, X, Y];
	_ ->
	    lists_sort:usplit_1(X, Y, L, [], [])
    end;
usort([X, Y | L]) when X > Y ->
    case L of
	[] ->
	    [Y, X];
	[Z] when X < Z ->
	    [Y, X | L];
	[Z] when Z < Y ->
	    [Z, Y, X];
        _ ->
            lists_sort:usplit_2(X, Y, L, [], [])
    end;
usort([X, _Y | L]) ->
    usort_1(X, L);
usort([_] = L) ->
    L;
usort([]) ->
    [].

usort_1(X, [Y | L]) when X == Y ->
    usort_1(X, L);
usort_1(X, [Y | L]) when X < Y ->
    lists_sort:usplit_1(X, Y, L, [], []);
usort_1(X, [Y | L]) ->
    lists_sort:usplit_2(X, Y, L, [], []);
usort_1(X, []) ->
    [X].

%% umerge(List) -> L
%%  merges a list of sorted lists without duplicates, removes duplicates

umerge(L) ->
    lists_sort:umergel(L).

%% umerge3(X, Y, Z) -> L
%%  merges three sorted lists X, Y and Z without duplicates, 
%%  removes duplicates

umerge3(L1, [], L3) ->
   umerge(L1, L3);
umerge3(L1, L2, []) ->
   umerge(L1, L2);
umerge3(L1, [H2 | T2], [H3 | T3]) ->
   lists:reverse(lists_sort:umerge3_1(L1, [], H2, T2, H3, T3), []).

%% rumerge3(X, Y, Z) -> L
%%  merges three reversed sorted lists X, Y and Z without duplicates,
%%  removes duplicates

rumerge3(L1, [], L3) ->
   rumerge(L1, L3);
rumerge3(L1, L2, []) ->
   rumerge(L1, L2);
rumerge3(L1, [H2 | T2], [H3 | T3]) ->
   lists:reverse(lists_sort:rumerge3_1(L1, [], H2, T2, H3, T3), []).

%% umerge(X, Y) -> L
%%  merges two sorted lists X and Y without duplicates, removes duplicates

umerge([], T2) ->
    T2;
umerge([H1 | T1], T2) ->
    lists:reverse(lists_sort:umerge2_2(T1, T2, [], H1), []).

%% rumerge(X, Y) -> L
%%  merges two reversed sorted lists X and Y without duplicates,
%%  removes duplicates

%% reverse(rumerge(reverse(A),reverse(B))) is equal to umerge(I,A,B).
rumerge(T1, []) ->
    T1;
rumerge(T1, [H2 | T2]) ->
    lists:reverse(lists_sort:rumerge2_1(T1, T2, [], H2), []).

%% all(Predicate, List)
%% any(Predicate, List)
%% map(Function, List)
%% flatmap(Function, List)
%% foldl(Function, First, List)
%% foldr(Function, Last, List)
%% filter(Predicate, List)
%% zf(Function, List)
%% mapfoldl(Function, First, List)
%% mapfoldr(Function, Last, List)
%% foreach(Function, List)
%% takewhile(Predicate, List)
%% dropwhile(Predicate, List)
%% splitwith(Predicate, List)
%%  for list programming. Function here is either a 'fun' or a tuple
%%  {Module,Name} and we use apply/2 to evaluate. The name zf is a joke!
%%
%%  N.B. Unless where the functions actually needs it only foreach/2/3,
%%  which is meant to be used for its side effects, has a defined order
%%  of evaluation.
%%
%%  There are also versions with an extra argument, ExtraArgs, which is a
%%  list of extra arguments to each call.

all(Pred, [Hd|Tail]) ->
    case Pred(Hd) of
	true -> all(Pred, Tail);
	false -> false
    end;
all(_, []) -> true. 

any(Pred, [Hd|Tail]) ->
    case Pred(Hd) of
	true -> true;
	false -> any(Pred, Tail)
    end;
any(_, []) -> false. 

map(F, [H|T]) ->
    [F(H)|map(F, T)];
map(_, []) -> [].

flatmap(F, [Hd|Tail]) ->
    F(Hd) ++ flatmap(F, Tail);
flatmap(_, []) -> [].

foldl(F, Accu, [Hd|Tail]) ->
    foldl(F, F(Hd, Accu), Tail);
foldl(_, Accu, []) -> Accu.

foldr(F, Accu, [Hd|Tail]) ->
    F(Hd, foldr(F, Accu, Tail));
foldr(_, Accu, []) -> Accu.

filter(Pred, List) -> [ E || E <- List, Pred(E) ].

zf(F, [Hd|Tail]) ->
    case F(Hd) of
	true ->
	    [Hd|zf(F, Tail)];
	{true,Val} ->
	    [Val|zf(F, Tail)];
	false ->
	    zf(F, Tail)
    end;
zf(_, []) -> [].

foreach(F, [Hd|Tail]) ->
    F(Hd),
    foreach(F, Tail);
foreach(_, []) -> ok.

mapfoldl(F, Accu0, [Hd|Tail]) ->
    {R,Accu1} = F(Hd, Accu0),
    {Rs,Accu2} = mapfoldl(F, Accu1, Tail),
    {[R|Rs],Accu2};
mapfoldl(_, Accu, []) -> {[],Accu}.

mapfoldr(F, Accu0, [Hd|Tail]) ->
    {Rs,Accu1} = mapfoldr(F, Accu0, Tail),
    {R,Accu2} = F(Hd, Accu1),
    {[R|Rs],Accu2};
mapfoldr(_, Accu, []) -> {[],Accu}.

takewhile(Pred, [Hd|Tail]) ->
    case Pred(Hd) of
	true -> [Hd|takewhile(Pred, Tail)];
	false -> []
    end;
takewhile(_, []) -> [].

dropwhile(Pred, [Hd|Tail]) ->
    case Pred(Hd) of
	true -> dropwhile(Pred, Tail);
	false -> [Hd|Tail]
    end;
dropwhile(_, []) -> [].

splitwith(Pred, List) -> splitwith(Pred, List, []).

splitwith(Pred, [Hd|Tail], Taken) ->
    case Pred(Hd) of
	true -> splitwith(Pred, Tail, [Hd|Taken]);
	false -> {reverse(Taken), [Hd|Tail]}
    end;
splitwith(_, [], Taken) -> {reverse(Taken),[]}.

%% Versions of the above functions with extra arguments.

all(Pred, Eas, [Hd|Tail]) ->
    case apply(Pred, [Hd|Eas]) of
	true -> all(Pred, Eas, Tail);
	false -> false
    end;
all(_, _, []) -> true. 

any(Pred, Eas, [Hd|Tail]) ->
    case apply(Pred, [Hd|Eas]) of
	true -> true;
	false -> any(Pred, Eas, Tail)
    end;
any(_, _, []) -> false. 

map(F, Eas, List) -> [ apply(F, [E|Eas]) || E <- List ].

flatmap(F, Eas, [Hd|Tail]) ->
    apply(F, [Hd|Eas]) ++ flatmap(F, Eas, Tail);
flatmap(_, _, []) -> [].

foldl(F, Eas, Accu, [Hd|Tail]) ->
    foldl(F, Eas, apply(F, [Hd,Accu|Eas]), Tail);
foldl(_, _, Accu, []) -> Accu.

foldr(F, Eas, Accu, [Hd|Tail]) ->
    apply(F, [Hd,foldr(F, Eas, Accu, Tail)|Eas]);
foldr(_, _, Accu, []) -> Accu.

filter(Pred, Eas, List) -> [ E || E <- List, apply(Pred, [E|Eas]) ].

zf(F, Eas, [Hd|Tail]) ->
    case apply(F, [Hd|Eas]) of
	true ->
	    [Hd|zf(F, Eas, Tail)];
	{true,Val} ->
	    [Val|zf(F, Eas, Tail)];
	false ->
	    zf(F, Eas, Tail)
    end;
zf(_, _, []) -> [].

foreach(F, Eas, [Hd|Tail]) ->
    apply(F, [Hd|Eas]),
    foreach(F, Eas, Tail);
foreach(_, _, []) -> ok.

mapfoldl(F, Eas, Accu0, [Hd|Tail]) ->
    {R,Accu1} = apply(F, [Hd,Accu0|Eas]),
    {Rs,Accu2} = mapfoldl(F, Eas, Accu1, Tail),
    {[R|Rs],Accu2};
mapfoldl(_, _, Accu, []) -> {[],Accu}.

mapfoldr(F, Eas, Accu0, [Hd|Tail]) ->
    {Rs,Accu1} = mapfoldr(F, Eas, Accu0, Tail),
    {R,Accu2} = apply(F, [Hd,Accu1|Eas]),
    {[R|Rs],Accu2};
mapfoldr(_, _, Accu, []) -> {[],Accu}.

%% takewhile/2, dropwhile/2 and splitwith/2 do not have versions with
%% extra arguments as this going to be discontinued.
