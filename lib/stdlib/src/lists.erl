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
-module(lists).

-export([append/2, append/1, subtract/2, reverse/1,
	 nth/2, nthtail/2, prefix/2, suffix/2, last/1, 
	 seq/2, seq/3, sum/1, duplicate/2, min/1, max/1, sublist/2, sublist/3,
	 delete/2, sort/1, merge/2, rmerge/2, concat/1,
	 flatten/1, flatten/2, flat_length/1, flatlength/1,
	 keydelete/3, keyreplace/4,
	 keysort/2, keymerge/3, keymap/3, keymap/4]).

%% Bifs: member/2, reverse/2
%% Bifs: keymember/3, keysearch/3

-export([merge/3, sort/2]).

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

reverse(X) -> lists:reverse(X, []).

%reverse([H|T], Y) ->
%    reverse(T, [H|Y]);
%reverse([], X) -> X.


%% nth(N, L) returns the N`th element of the list L
%% nthtail(N, L) returns the N`th tail of the list L

nth(1, [H|T]) -> H;
nth(N, [_|T]) when N > 1 ->
    nth(N - 1, T).

nthtail(1, [H|T]) -> T;
nthtail(N, [H|T]) when N > 1 ->
    nthtail(N - 1, T);
nthtail(0, L) when list(L) -> L.

%% prefix(Prefix, List) -> (true | false)

prefix([X|PreTail], [X|Tail]) ->
    prefix(PreTail, Tail);
prefix([], List) -> true;
prefix(_,_) -> false.


%% suffix(Suffix, List) -> (true | false)

suffix(Suffix, Suffix) -> true;
suffix(Suffix, [_|Tail]) ->
    suffix(Suffix, Tail);
suffix(Suffix, []) -> false.

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

seq(Min, Min, I, L) -> [Min|L];
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
sublist_2(List, 0) ->
    [];
sublist_2(List, L) when list(List), L > 0 ->
    [].

%% delete(Item, List) -> List'
%%  Delete the first occurance of Item from the list L.

delete(Item, [Item|Rest]) -> Rest;
delete(Item, [H|Rest]) ->
    [H|delete(Item, Rest)];
delete(Item, []) -> [].

%% sort(L) -> sorts the list L

sort([A|L]) ->
    split(L, A, A, [A], [], [], []);
sort([]) ->
    [].

split([A,B|L], Min, Max, Low, High, Lst, Acc) when A >= Max ->
    if
	B >= A ->
	    split(L, Min, B, Low, [B,A|High], Lst, Acc);
	B =< Min ->
	    split(L, B, A, [B|Low], [A|High], Lst, Acc);
	true ->
	    split2(L, B, B, [B], [], Low, [A|High], Lst, Acc)
    end;
split([A,B|L], Min, Max, Low, High, Lst, Acc) when A =< Min ->
    if 
	B =< A ->
	    split(L, B, Max, [B,A|Low], High, Lst, Acc);
	B >= Max ->
	    split(L, A, B, [A|Low], [B|High], Lst, Acc);
	true ->
	    split2(L, B, B, [B], [], [A|Low], High, Lst, Acc)
    end;
split([A,B|L], Min, Max, Low, High, Lst, Acc) when A =< B ->
    split2(L, A, B, [A,B], [], Low, High, Lst, Acc);
split([A,B|L], Min, Max, Low, High, Lst, Acc)  -> % when A > B
    split2(L, B, A, [B,A], [], Low, High, Lst, Acc);
split([A], Min, Max, Low, High, Lst, Acc) ->
    if 
	A =< Min ->
	    init_merge_lists([A|Low], High, Lst, Acc);
	A >= Max ->
	    init_merge_lists(Low, [A|High], Lst, Acc);
	true ->
	    split2([], A, A, [A], [], Low, High, Lst, Acc)
    end;
split([], Min, Max, Low, High, Lst, Acc) ->
    init_merge_lists(Low, High, Lst, Acc).

% Halve the length of Acc by merging once right away:
split2(L, Min, Max, Low, High, Low1, High1, [], Acc) ->
    split(L, Min, Max, Low, High, combine(Low1, High1), Acc);
split2(L, Min, Max, Low, High, Low1, High1, Lst, Acc) ->
    Lst1 = combine(Low1, High1),
    split(L, Min, Max, Low, High, [], [merge2(Lst1, Lst, []) | Acc]).

init_merge_lists(Low, High, [], []) ->
    combine(Low, High);
init_merge_lists(Low, High, [], Acc) ->
    rmergeit([combine(High, Low) | Acc], []);
init_merge_lists(Low, High, Lst, Acc) ->
    rmergeit([merge2(combine(Low, High), Lst, []) | Acc], []).

mergeit([A,B|L], Acc) ->
    mergeit(L, [merge2(A, B, []) | Acc]);
mergeit([L], []) ->
    L;
mergeit([L], Acc) ->
    rmergeit([lists:reverse(L, []) | Acc], []);
mergeit([], Acc) ->
    rmergeit(Acc, []).

rmergeit([A,B|L], Acc) ->
    rmergeit(L, [rmerge2(A, B, []) | Acc]);
rmergeit([L], Acc) ->
    mergeit([lists:reverse(L, []) | Acc], []);
rmergeit([], Acc) ->
    mergeit(Acc, []).

combine(L1, []) ->
    L1;
combine(L1, L2) ->
    L1 ++ lists:reverse(L2, []).

%% merge(X, Y) -> L
%%  merges two sorted lists X and Y

merge(X, Y) -> lists:reverse(merge2(X, Y, []), []).

merge2([H1|T1], [H2|T2], L) ->
    if
	H1 =< H2 ->
	    merge2(T1, H2, T2, [H1|L]);
	true ->
	    merge2(T2, H1, T1, [H2|L])
    end;
merge2([], [H2|T2], L) ->
    lists:reverse(T2, [H2|L]);
merge2([H1|T1], [], L) ->
    lists:reverse(T1, [H1|L]);
merge2([], [], L) -> L.

merge2([H1|T1], H2, T2, L) ->
    if
	H1 =< H2 ->
	    merge2(T1, H2, T2, [H1|L]);
	true ->
	    merge2(T2, H1, T1, [H2|L])
    end;
merge2([], H2, T2, L) ->
    lists:reverse(T2, [H2|L]).

%% rmerge(X, Y) -> L
%%  merges two reversed sorted lists X and Y

rmerge(X, Y) -> lists:reverse(rmerge2(X, Y, []), []).

rmerge2([H1|T1], [H2|T2], L) ->
    if
	H1 >= H2 ->
	    rmerge2(T1, H2, T2, [H1|L]);
	true ->
	    rmerge2(T2, H1, T1, [H2|L])
    end;
rmerge2([], [H2|T2], L) ->
    lists:reverse(T2, [H2|L]);
rmerge2([H1|T1], [], L) ->
    lists:reverse(T1, [H1|L]);
rmerge2([], [], L) -> L.

rmerge2([H1|T1], H2, T2, L) ->
    if
	H1 >= H2 ->
	    rmerge2(T1, H2, T2, [H1|L]);
	true ->
	    rmerge2(T2, H1, T1, [H2|L])
    end;
rmerge2([], H2, T2, L) ->
    lists:reverse(T2, [H2|L]).

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
flatlength([H|T], L) ->
    flatlength(T, L + 1);
flatlength([], L) -> L.

%% keymember(Key, Index, [Tuple]) Now a BIF!
%% keysearch(Key, Index, [Tuple]) Now a BIF!
%% keydelete(Key, Index, [Tuple])
%% keyreplace(Key, Index, [Tuple], NewTuple)
%% keysort(Index, [Tuple])
%% keymerge(Index, [Tuple], [Tuple])
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
keydelete3(Key, N, []) -> [].

keyreplace(K,N,L,New) when integer(N), N > 0 ->
    keyreplace3(K,N,L,New).

keyreplace3(Key, Pos, [Tup|Tail], New) when element(Pos, Tup) == Key ->
    [New|Tail];
keyreplace3(Key, Pos, [H|T], New) ->
    [H|keyreplace3(Key, Pos, T, New)];
keyreplace3(Key, Pos, [], New) -> [].

keysort(Index, L) when integer(Index), Index > 0 ->
    keysort2(Index, L).

keysort2(_I, [])  -> 
    [];
keysort2(I, [H|T]) ->
    K = element(I, H),
    {Sorted,[]} = samkeyrun(T, H, K, H, K, [H], [], I, -1),
    Sorted.

keysort2([H|T], J, Lim, Run0, I) when J =/= Lim ->
    K = element(I, H),
    {Run2,Rest1} = samkeyrun(T, H, K, H, K, [H], [], I, J),
    Run = keymerge(I, Run0, Run2),
    keysort2(Rest1, J+1, Lim, Run, I);
keysort2(Rest, _, _, Run, _) ->
    {Run,Rest}.

samkeyrun([], _EL, _LK, _EH, _HK, L, H, _I, J) ->
    {L ++ lists:reverse(H, []),[]};
samkeyrun(All=[E|Es], EL, LK, EH, HK, L, H, I, J) ->
    K = element(I, E),
    if 
	K < LK ->
	    samkeyrun(Es, E, K, EH, HK, [E|L], H, I, J);
	K >= HK ->
	    samkeyrun(Es, EL, LK, E, K, L, [E|H], I, J);
	true ->
	    keysort2(All, 1, J, L ++ lists:reverse(H, []), I)
    end.

keymerge(Index, Os, Ns) when integer(Index), Index > 0 -> 
    keymerge(Os, Ns, [], Index).

keymerge([], Ns, L, _I) ->
    lists:reverse(L, Ns);
keymerge([O|Os], Ns, L, I) ->
    K = element(I, O),
    keymerge(Ns, O, K, Os, L, I).

keymerge([], O, _K, Os, L, I) ->
    lists:reverse(L, [O|Os]);
keymerge(All = [N|Ns], O, K, Os, L, I) ->
    NK = element(I, N),
    if 
	K =< NK ->
	    keymerge(Os, All, [O|L], I);
	true ->
	    keymerge(Ns, O, K, Os, [N|L], I)
    end.

keymap(Fun, Index, [Tup|Tail]) ->
   [setelement(Index, Tup, Fun(element(Index, Tup)))|keymap(Fun, Index, Tail)];
keymap( _, _ , []) -> [].

keymap(Fun, ExtraArgs, Index, [Tup|Tail]) ->
   [setelement(Index, Tup, apply(Fun, [element(Index, Tup)|ExtraArgs]))|
    keymap(Fun, ExtraArgs, Index, Tail)];
keymap( _, _ , _, []) -> [].


%%% Suggestion from OTP-2948: sort and merge with Fun.
sort(Fun, []) ->
    [];
sort(Fun, [H|T]) ->
    {Sorted, []} = samrun(T, H, H, [H], [], -1, Fun),
    Sorted.

sort([H|T], J, Lim, Run0, F) when J =/= Lim ->
    {Run2, Rest1} = samrun(T, H, H, [H], [], J, F),
    Run = merge(F, Run0, Run2),
    sort(Rest1, J+1, Lim, Run, F);
sort(Rest, _, _, Run, _) ->
    {Run, Rest}.

samrun([], _EL, _EH, L, H, J, F) ->
    {L ++ lists:reverse(H, []), []};
samrun(R=[E|Es], EL, EH, L, H, J, F)  ->
    case F(EL, E) of
	false -> % E < EL
	    samrun(Es, E, EH, [E|L], H, J, F);
	true -> 
	    case F(EH, E) of
		true -> % E >= EH
		    samrun(Es, EL, E, L, [E|H], J, F);
		false ->
		    sort(R, 1, J, L++lists:reverse(H, []), F)
	    end
    end.

merge(Fun, X, Y) -> lists:reverse(merge(Fun, X, Y, []), []).

merge(Fun, [H1|T1], [H2|T2], L) ->
    case Fun(H1, H2) of
	true ->
	    merge(Fun, T1, H2, T2, [H1|L]);
	false ->
	    merge(Fun, T2, H1, T1, [H2|L])
    end;
merge(Fun, [], [H2|T2], L) ->
    lists:reverse(T2, [H2|L]);
merge(Fun, [H1|T1], [], L) ->
    lists:reverse(T1, [H1|L]);
merge(Fun, [], [], L) -> L.

merge(Fun, [H1|T1], H2, T2, L) ->
    case Fun(H1, H2) of
	true ->
	    merge(Fun, T1, H2, T2, [H1|L]);
	false ->
	    merge(Fun, T2, H1, T1, [H2|L])
    end;
merge(Fun, [], H2, T2, L) ->
    lists:reverse(T2, [H2|L]).

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
all(Pred, []) -> true. 

any(Pred, [Hd|Tail]) ->
    case Pred(Hd) of
	true -> true;
	false -> any(Pred, Tail)
    end;
any(Pred, []) -> false. 

map(F, [H|T]) ->
    [F(H)|map(F, T)];
map(F, []) ->
    [].

flatmap(F, [Hd|Tail]) ->
    F(Hd) ++ flatmap(F, Tail);
flatmap(F, []) -> [].

foldl(F, Accu, [Hd|Tail]) ->
    foldl(F, F(Hd, Accu), Tail);
foldl(F, Accu, []) -> Accu.

foldr(F, Accu, [Hd|Tail]) ->
    F(Hd, foldr(F, Accu, Tail));
foldr(F, Accu, []) -> Accu.

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
zf(F, []) -> [].

foreach(F, [Hd|Tail]) ->
    F(Hd),
    foreach(F, Tail);
foreach(F, []) -> ok.

mapfoldl(F, Accu0, [Hd|Tail]) ->
    {R,Accu1} = F(Hd, Accu0),
    {Rs,Accu2} = mapfoldl(F, Accu1, Tail),
    {[R|Rs],Accu2};
mapfoldl(F, Accu, []) -> {[],Accu}.

mapfoldr(F, Accu0, [Hd|Tail]) ->
    {Rs,Accu1} = mapfoldr(F, Accu0, Tail),
    {R,Accu2} = F(Hd, Accu1),
    {[R|Rs],Accu2};
mapfoldr(F, Accu, []) -> {[],Accu}.

takewhile(Pred, [Hd|Tail]) ->
    case Pred(Hd) of
	true -> [Hd|takewhile(Pred, Tail)];
	false -> []
    end;
takewhile(Pred, []) -> [].

dropwhile(Pred, [Hd|Tail]) ->
    case Pred(Hd) of
	true -> dropwhile(Pred, Tail);
	false -> [Hd|Tail]
    end;
dropwhile(Pred, []) -> [].

splitwith(Pred, List) -> splitwith(Pred, List, []).

splitwith(Pred, [Hd|Tail], Taken) ->
    case Pred(Hd) of
	true -> splitwith(Pred, Tail, [Hd|Taken]);
	false -> {reverse(Taken), [Hd|Tail]}
    end;
splitwith(Pred, [], Taken) -> {reverse(Taken),[]}.

%% Versions of the above functions with extra arguments.

all(Pred, Eas, [Hd|Tail]) ->
    case apply(Pred, [Hd|Eas]) of
	true -> all(Pred, Eas, Tail);
	false -> false
    end;
all(Pred, Eas, []) -> true. 

any(Pred, Eas, [Hd|Tail]) ->
    case apply(Pred, [Hd|Eas]) of
	true -> true;
	false -> any(Pred, Eas, Tail)
    end;
any(Pred, Eas, []) -> false. 

map(F, Eas, List) -> [ apply(F, [E|Eas]) || E <- List ].

flatmap(F, Eas, [Hd|Tail]) ->
    apply(F, [Hd|Eas]) ++ flatmap(F, Eas, Tail);
flatmap(F, Eas, []) -> [].

foldl(F, Eas, Accu, [Hd|Tail]) ->
    foldl(F, Eas, apply(F, [Hd,Accu|Eas]), Tail);
foldl(F, Eas, Accu, []) -> Accu.

foldr(F, Eas, Accu, [Hd|Tail]) ->
    apply(F, [Hd,foldr(F, Eas, Accu, Tail)|Eas]);
foldr(F, Eas, Accu, []) ->
    Accu.

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
zf(F, Eas, []) -> [].

foreach(F, Eas, [Hd|Tail]) ->
    apply(F, [Hd|Eas]),
    foreach(F, Eas, Tail);
foreach(F, Eas, []) -> ok.

mapfoldl(F, Eas, Accu0, [Hd|Tail]) ->
    {R,Accu1} = apply(F, [Hd,Accu0|Eas]),
    {Rs,Accu2} = mapfoldl(F, Eas, Accu1, Tail),
    {[R|Rs],Accu2};
mapfoldl(F, Eas, Accu, []) -> {[],Accu}.

mapfoldr(F, Eas, Accu0, [Hd|Tail]) ->
    {Rs,Accu1} = mapfoldr(F, Eas, Accu0, Tail),
    {R,Accu2} = apply(F, [Hd,Accu1|Eas]),
    {[R|Rs],Accu2};
mapfoldr(F, Eas, Accu, []) -> {[],Accu}.

%% takewhile/2, dropwhile/2 and splitwith/2 do not have versions with
%% extra arguments as this going to be discontinued.
