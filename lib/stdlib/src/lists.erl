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
	 delete/2,
	 unzip/1, unzip3/1, zip/2, zip3/3, zipwith/3, zipwith3/4,
	 sort/1, merge/1, merge/2, rmerge/2, merge3/3, rmerge3/3,
	 usort/1, umerge/1, umerge3/3, umerge/2, rumerge3/3, rumerge/2,
	 concat/1, flatten/1, flatten/2, flat_length/1, flatlength/1,
	 keydelete/3, keyreplace/4,
	 keysort/2, keymerge/3, rkeymerge/3, rukeymerge/3, 
	 ukeysort/2, ukeymerge/3, keymap/3, keymap/4]).

%% Bifs: member/2, reverse/2
%% Bifs: keymember/3, keysearch/3

-export([merge/3, rmerge/3, sort/2, umerge/3, rumerge/3, usort/2]).

-export([all/2,any/2,map/2,flatmap/2,foldl/3,foldr/3,filter/2,
	 partition/2,zf/2,
	 mapfoldl/3,mapfoldr/3,foreach/2,takewhile/2,dropwhile/2,splitwith/2,
	 split/2]).
-export([all/3,any/3,map/3,flatmap/3,foldl/4,foldr/4,filter/3,zf/3,
	 mapfoldl/4,mapfoldr/4,foreach/3]).

-deprecated([{keymap,4},{all,3},{any,3},{map,3},{flatmap,3},{foldl,4},
             {foldr,4},{filter,3},{mapfoldl,4},{mapfoldr,4},{foreach,3}]).

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
nthtail(0, L) when is_list(L) -> L.

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

seq(Min, Max) when is_integer(Min), is_integer(Max), Min =< Max -> 
    seq(Min, Max, 1, []).

seq(Min, Max, Incr) when Min =< Max, Incr > 0 ->
    seq(Min, Min + ((Max-Min) div Incr) * Incr, Incr, []);
seq(Min, Max, Incr) when Min >= Max, Incr < 0 ->
    seq(Min, Min + ((Max-Min) div Incr) * Incr, Incr, []);
seq(M, M, 0) when is_integer(M) ->
    [M].

seq(Min, Min, _, L) -> [Min|L];
seq(Min, Max, I, L) -> seq(Min, Max-I, I, [Max|L]).

%% sum(L) suns the sum of the elements in L

sum(L)          -> sum(L, 0).

sum([H|T], Sum) -> sum(T, Sum + H);
sum([], Sum)    -> Sum.

%% duplicate(N, X) -> [X,X,X,.....,X]  (N times)
%%   return N copies of X

duplicate(N, X) when is_integer(N), N >= 0 -> duplicate(N, X, []).

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

sublist(List, S, L) when is_integer(L), L >= 0 ->
    sublist(nthtail(S-1, List), L).

sublist(List, L) when is_integer(L), is_list(List) ->
    sublist_2(List, L).

sublist_2([H|T], L) when L > 0 ->
    [H|sublist_2(T, L-1)];
sublist_2(_, 0) ->
    [];
sublist_2(List, L) when is_list(List), L > 0 ->
    [].

%% delete(Item, List) -> List'
%%  Delete the first occurance of Item from the list L.

delete(Item, [Item|Rest]) -> Rest;
delete(Item, [H|Rest]) ->
    [H|delete(Item, Rest)];
delete(_, []) -> [].

%% Return [{X0, Y0}, {X1, Y1}, ..., {Xn, Yn}] for lists [X0, X1, ...,
%% Xn] and [Y0, Y1, ..., Yn].

zip([X | Xs], [Y | Ys]) -> [{X, Y} | zip(Xs, Ys)];
zip([], []) -> [].

%% Return {[X0, X1, ..., Xn], [Y0, Y1, ..., Yn]}, for a list [{X0, Y0},
%% {X1, Y1}, ..., {Xn, Yn}].

unzip(Ts) -> unzip(Ts, [], []).

unzip([{X, Y} | Ts], Xs, Ys) -> unzip(Ts, [X | Xs], [Y | Ys]);
unzip([], Xs, Ys) -> {reverse(Xs), reverse(Ys)}.

%% Return [{X0, Y0, Z0}, {X1, Y1, Z1}, ..., {Xn, Yn, Zn}] for lists [X0,
%% X1, ..., Xn], [Y0, Y1, ..., Yn] and [Z0, Z1, ..., Zn].

zip3([X | Xs], [Y | Ys], [Z | Zs]) -> [{X, Y, Z} | zip3(Xs, Ys, Zs)];
zip3([], [], []) -> [].

%% Return {[X0, X1, ..., Xn], [Y0, Y1, ..., Yn], [Z0, Z1, ..., Zn]}, for
%% a list [{X0, Y0, Z0}, {X1, Y1, Z1}, ..., {Xn, Yn, Zn}].

unzip3(Ts) -> unzip3(Ts, [], [], []).

unzip3([{X, Y, Z} | Ts], Xs, Ys, Zs) ->
    unzip3(Ts, [X | Xs], [Y | Ys], [Z | Zs]);
unzip3([], Xs, Ys, Zs) ->
    {reverse(Xs), reverse(Ys), reverse(Zs)}.

%% Return [F(X0, Y0), F(X1, Y1), ..., F(Xn, Yn)] for lists [X0, X1, ...,
%% Xn] and [Y0, Y1, ..., Yn].

zipwith(F, [X | Xs], [Y | Ys]) -> [F(X, Y) | zipwith(F, Xs, Ys)];
zipwith(F, [], []) when is_function(F, 2) -> [].

%% Return [F(X0, Y0, Z0), F(X1, Y1, Z1), ..., F(Xn, Yn, Zn)] for lists
%% [X0, X1, ..., Xn], [Y0, Y1, ..., Yn] and [Z0, Z1, ..., Zn].

zipwith3(F, [X | Xs], [Y | Ys], [Z | Zs]) ->
    [F(X, Y, Z) | zipwith3(F, Xs, Ys, Zs)];
zipwith3(F, [], [], []) when is_function(F, 3) -> [].

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

merge(T1, []) ->
    T1;
merge(T1, [H2 | T2]) ->
    lists:reverse(lists_sort:merge2_1(T1, H2, T2, []), []).

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

thing_to_list(X) when is_integer(X) -> integer_to_list(X);
thing_to_list(X) when is_float(X)   -> float_to_list(X);
thing_to_list(X) when is_atom(X)    -> atom_to_list(X);
thing_to_list(X) when is_list(X)    -> X.	%Assumed to be a string

%% flatten(List)
%% flatten(List, Tail)
%%  Flatten a list, adding optional tail.

flatten(List) when is_list(List) ->
    do_flatten(List, []).

flatten(List, Tail) when is_list(List), is_list(Tail) ->
    do_flatten(List, Tail).

do_flatten([H|T], Tail) when is_list(H) ->
    do_flatten(H, do_flatten(T, Tail));
do_flatten([H|T], Tail) ->
    [H|do_flatten(T, Tail)];
do_flatten([], Tail) ->
    Tail.

%% flat_length(List) (undocumented can be removed later)
%%  Calculate the length of a list of lists.

flat_length(List) -> flatlength(List).

%% flatlength(List)
%%  Calculate the length of a list of lists.

flatlength(List) ->
    flatlength(List, 0).

flatlength([H|T], L) when is_list(H) ->
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

%keymember(K,N,L) when is_integer(N), N > 0 ->
%    keymember3(K,N,L).

%keymember3(Key, N, [T|Ts]) when element(N, T) == Key -> true;
%keymember3(Key, N, [T|Ts]) ->
%    keymember3(Key, N, Ts);
%keymember3(Key, N, []) -> false.

%keysearch(K,N,L) when is_integer(N), N > 0 ->
%    keysearch3(K,N,L).

%keysearch3(Key, N, [H|T]) when element(N, H) == Key ->
%    {value, H};
%keysearch3(Key, N, [H|T]) ->
%    keysearch3(Key, N, T);
%keysearch3(Key, N, []) -> false.

keydelete(K,N,L) when is_integer(N), N > 0 ->
    keydelete3(K,N,L).

keydelete3(Key, N, [H|T]) when element(N, H) == Key -> T;
keydelete3(Key, N, [H|T]) ->
    [H|keydelete3(Key, N, T)];
keydelete3(_, _, []) -> [].

keyreplace(K,N,L,New) when is_integer(N), N > 0, is_tuple(New) ->
    keyreplace3(K,N,L,New).

keyreplace3(Key, Pos, [Tup|Tail], New) when element(Pos, Tup) == Key ->
    [New|Tail];
keyreplace3(Key, Pos, [H|T], New) ->
    [H|keyreplace3(Key, Pos, T, New)];
keyreplace3(_, _, [], _) -> [].

keysort(I, L) when is_integer(I), I > 0 ->
    case L of
	[] -> L;
	[_] -> L;
	[X, Y | T] ->
	    case {element(I, X), element(I, Y)} of
		{EX, EY} when EX =< EY ->
		    case T of
			[] ->
			    L;
			[Z] ->
			    case element(I, Z) of
				EZ when EY =< EZ ->
				    L;
				EZ when EX =< EZ ->
				    [X, Z, Y];
				_EZ ->
				    [Z, X, Y]
			    end;
			_ when X == Y ->
			    keysort_1(I, Y, EY, T, [X]);
			_ ->
			    lists_sort:keysplit_1(I, X, EX, Y, EY, T, [], [])
		    end;
		{EX, EY} ->
		    case T of
			[] ->
			    [Y, X];
			[Z] ->
			    case element(I, Z) of
				EZ when EX =< EZ ->
				    [Y, X | T];
				EZ when EY =< EZ ->
				    [Y, Z, X];
				_EZ ->
				    [Z, Y, X]
			    end;
			_ ->
			    lists_sort:keysplit_2(I, X, EX, Y, EY, T, [], [])
		    end
	    end
    end.

keysort_1(I, X, EX, [Y | L], R) when X == Y ->
    keysort_1(I, Y, EX, L, [X | R]);
keysort_1(I, X, EX, [Y | L], R) ->
    case element(I, Y) of
	EY when EX =< EY ->
	    lists_sort:keysplit_1(I, X, EX, Y, EY, L, R, []);
	EY ->
	    lists_sort:keysplit_2(I, X, EX, Y, EY, L, R, [])
    end;
keysort_1(_I, X, _EX, [], R) ->
    [X | R].

keymerge(Index, T1, L2) when is_integer(Index), Index > 0 -> 
    case L2 of
	[] ->
	    T1;
	[H2 | T2] ->
	    E2 = element(Index, H2),
	    M = lists_sort:keymerge2_1(Index, T1, E2, H2, T2, []),
	    lists:reverse(M, [])
    end.

%% reverse(rkeymerge(I,reverse(A),reverse(B))) is equal to keymerge(I,A,B).
rkeymerge(Index, T1, L2) when is_integer(Index), Index > 0 -> 
    case L2 of
	[] ->
	    T1;
	[H2 | T2] ->
	    E2 = element(Index, H2),
	    M = lists_sort:rkeymerge2_1(Index, T1, E2, H2, T2, []),
	    lists:reverse(M, [])
    end.

ukeysort(I, L) when is_integer(I), I > 0 ->
    case L of
	[] -> L;
	[_] -> L;
	[X, Y | T] ->
            case {element(I, X), element(I, Y)} of
                {EX, EY} when EX == EY ->
                    ukeysort_1(I, X, EX, T);
                {EX, EY} when EX < EY ->
                    case T of
                        [] ->
                            L;
                        [Z] ->
                            case element(I, Z) of
                                EZ when EY == EZ ->
                                    [X, Y];
                                EZ when EY < EZ ->
                                    [X, Y, Z];
                                EZ when EZ == EX ->
                                    [X, Y];
                                EZ when EX =< EZ ->
                                    [X, Z, Y];
                                _EZ ->
                                    [Z, X, Y]
                            end;
                        _ ->
                            lists_sort:ukeysplit_1(I, X, EX, Y, EY, T, [], [])
                    end;
                {EX, EY} ->
                    case T of
                        [] ->
                            [Y, X];
                        [Z] ->
                            case element(I, Z) of
                                EZ when EX == EZ ->
                                    [Y, X];
                                EZ when EX < EZ ->
                                    [Y, X, Z];
                                EZ when EY == EZ ->
                                    [Y, X];
                                EZ when EY =< EZ ->
                                    [Y, Z, X];
                                _EZ ->
                                    [Z, Y, X]
                            end;
                        _ ->
			    lists_sort:ukeysplit_2(I, Y, EY, T, [X])
                    end
	    end
    end.

ukeysort_1(I, X, EX, [Y | L]) ->
    case element(I, Y) of
        EY when EX == EY ->
            ukeysort_1(I, X, EX, L);
	EY when EX < EY ->
	    lists_sort:ukeysplit_1(I, X, EX, Y, EY, L, [], []);
	EY ->
	    lists_sort:ukeysplit_2(I, Y, EY, L, [X])
    end;
ukeysort_1(_I, X, _EX, []) ->
    [X].

ukeymerge(Index, L1, T2) when is_integer(Index), Index > 0 ->
    case L1 of
	[] ->
	    T2;
	[H1 | T1] ->
	    E1 = element(Index, H1),
	    M = lists_sort:ukeymerge2_2(Index, T1, E1, H1, T2, []),
	    lists:reverse(M, [])
    end.

%% reverse(rukeymerge(I,reverse(A),reverse(B))) is equal to ukeymerge(I,A,B).
rukeymerge(Index, T1, L2) when is_integer(Index), Index > 0 ->
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
keymap(Fun, Index, []) when is_integer(Index), Index >= 1, 
                            is_function(Fun, 1) -> [].

keymap(Fun, ExtraArgs, Index, [Tup|Tail]) ->
   [setelement(Index, Tup, apply(Fun, [element(Index, Tup)|ExtraArgs]))|
    keymap(Fun, ExtraArgs, Index, Tail)];
keymap(Fun, _ , _, []) when is_function(Fun) -> [].


%%% Suggestion from OTP-2948: sort and merge with Fun.
sort(Fun, []) when is_function(Fun, 2) ->
    [];
sort(Fun, [_] = L) when is_function(Fun, 2) ->
    L;
sort(Fun, [X, Y | T]) ->
    case Fun(X, Y) of
	true ->
	    lists_sort:fsplit_1(Y, X, Fun, T, [], []);
	false ->
	    lists_sort:fsplit_2(Y, X, Fun, T, [], [])
    end.

merge(Fun, T1, [H2 | T2]) when is_function(Fun, 2) ->
    lists:reverse(lists_sort:fmerge2_1(T1, H2, Fun, T2, []), []);
merge(Fun, T1, []) when is_function(Fun, 2) ->
    T1.

%% reverse(rmerge(F,reverse(A),reverse(B))) is equal to merge(F,A,B).
rmerge(Fun, T1, [H2 | T2]) when is_function(Fun, 2) ->
    lists:reverse(lists_sort:rfmerge2_1(T1, H2, Fun, T2, []), []);
rmerge(Fun, T1, []) when is_function(Fun, 2) ->
    T1.

usort(Fun, [_] = L) when is_function(Fun, 2) ->
    L;
usort(Fun, [] = L) when is_function(Fun, 2) ->
    L;
usort(Fun, [X | L]) when is_function(Fun, 2) ->
    usort_1(Fun, X, L).

usort_1(Fun, X, [Y | L]) ->
    case Fun(X, Y) of
        true ->
            case Fun(Y, X) of
                true -> % X equal to Y
                    case L of
                        [] ->
                            [X];
                        _ ->
                            usort_1(Fun, X, L)
                    end;
                false ->
                    lists_sort:ufsplit_1(Y, X, Fun, L, [], [])
            end;
        false  ->
	    lists_sort:ufsplit_2(Y, L, Fun, [X])
    end.
                    
umerge(Fun, [], T2) when is_function(Fun, 2) ->
    T2;
umerge(Fun, [H1 | T1], T2) when is_function(Fun, 2) ->
    lists:reverse(lists_sort:ufmerge2_2(H1, T1, Fun, T2, []), []).

%% reverse(rumerge(F,reverse(A),reverse(B))) is equal to umerge(F,A,B).
rumerge(Fun, T1, []) when is_function(Fun, 2) ->
    T1;
rumerge(Fun, T1, [H2 | T2]) when is_function(Fun, 2) ->
    lists:reverse(lists_sort:rufmerge2_1(T1, H2, Fun, T2, []), []).

%% usort(List) -> L
%%  sorts the list L, removes duplicates

usort([X, Y | L] = L0) when X < Y ->
    case L of
	[] ->
	    L0;
	[Z] when Y < Z ->
	    L0;
	[Z] when Y == Z ->
	    [X, Y];
	[Z] when Z < X ->
	    [Z, X, Y];
	[Z] when Z == X ->
	    [X, Y];
	[Z] ->
	    [X, Z, Y];
	_ ->
	    lists_sort:usplit_1(X, Y, L, [], [])
    end;
usort([X, Y | L]) when X > Y ->
    case L of
	[] ->
	    [Y, X];
	[Z] when X < Z ->
	    [Y, X | L];
	[Z] when X == Z ->
	    [Y, X];
	[Z] when Z < Y ->
	    [Z, Y, X];
	[Z] when Z == Y ->
	    [Y, X];
	[Z] ->
	    [Y, Z, X];
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
   lists:reverse(lists_sort:umerge3_1(L1, [H2 | H3], T2, H2, [], T3, H3), []).

%% rumerge3(X, Y, Z) -> L
%%  merges three reversed sorted lists X, Y and Z without duplicates,
%%  removes duplicates

rumerge3(L1, [], L3) ->
   rumerge(L1, L3);
rumerge3(L1, L2, []) ->
   rumerge(L1, L2);
rumerge3(L1, [H2 | T2], [H3 | T3]) ->
   lists:reverse(lists_sort:rumerge3_1(L1, T2, H2, [], T3, H3),[]).

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
%%  for list programming. Function here is a 'fun'. For backward compatibility,
%%  {Module,Function} is still accepted.
%% 
%%  The name zf is a joke!
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
all(Pred, []) when is_function(Pred, 1) -> true. 

any(Pred, [Hd|Tail]) ->
    case Pred(Hd) of
	true -> true;
	false -> any(Pred, Tail)
    end;
any(Pred, []) when is_function(Pred, 1) -> false. 

map(F, [H|T]) ->
    [F(H)|map(F, T)];
map(F, []) when is_function(F, 1) -> [].

flatmap(F, [Hd|Tail]) ->
    F(Hd) ++ flatmap(F, Tail);
flatmap(F, []) when is_function(F, 1) -> [].

foldl(F, Accu, [Hd|Tail]) ->
    foldl(F, F(Hd, Accu), Tail);
foldl(F, Accu, []) when is_function(F, 2) -> Accu.

foldr(F, Accu, [Hd|Tail]) ->
    F(Hd, foldr(F, Accu, Tail));
foldr(F, Accu, []) when is_function(F, 2) -> Accu.

filter(Pred, List) when is_function(Pred, 1) ->
    [ E || E <- List, Pred(E) ].

%% Equivalent to {filter(F, L), filter(NotF, L)}, if NotF = 'fun(X) ->
%% not F(X) end'.

partition(Pred, L) ->
    partition(Pred, L, [], []).

partition(Pred, [H | T], As, Bs) ->
    case Pred(H) of
	true -> partition(Pred, T, [H | As], Bs);
	false -> partition(Pred, T, As, [H | Bs])
    end;
partition(Pred, [], As, Bs) when is_function(Pred, 1) ->
    {reverse(As), reverse(Bs)}.

zf(F, [Hd|Tail]) ->
    case F(Hd) of
	true ->
	    [Hd|zf(F, Tail)];
	{true,Val} ->
	    [Val|zf(F, Tail)];
	false ->
	    zf(F, Tail)
    end;
zf(F, []) when is_function(F, 1) -> [].

foreach(F, [Hd|Tail]) ->
    F(Hd),
    foreach(F, Tail);
foreach(F, []) when is_function(F, 1) -> ok.

mapfoldl(F, Accu0, [Hd|Tail]) ->
    {R,Accu1} = F(Hd, Accu0),
    {Rs,Accu2} = mapfoldl(F, Accu1, Tail),
    {[R|Rs],Accu2};
mapfoldl(F, Accu, []) when is_function(F, 2) -> {[],Accu}.

mapfoldr(F, Accu0, [Hd|Tail]) ->
    {Rs,Accu1} = mapfoldr(F, Accu0, Tail),
    {R,Accu2} = F(Hd, Accu1),
    {[R|Rs],Accu2};
mapfoldr(F, Accu, []) when is_function(F, 2) -> {[],Accu}.

takewhile(Pred, [Hd|Tail]) ->
    case Pred(Hd) of
	true -> [Hd|takewhile(Pred, Tail)];
	false -> []
    end;
takewhile(Pred, []) when is_function(Pred, 1) -> [].

dropwhile(Pred, [Hd|Tail]=Rest) ->
    case Pred(Hd) of
	true -> dropwhile(Pred, Tail);
	false -> Rest
    end;
dropwhile(Pred, []) when is_function(Pred, 1) -> [].

splitwith(Pred, List) when is_function(Pred, 1) ->
    splitwith(Pred, List, []).

splitwith(Pred, [Hd|Tail], Taken) ->
    case Pred(Hd) of
	true -> splitwith(Pred, Tail, [Hd|Taken]);
	false -> {reverse(Taken), [Hd|Tail]}
    end;
splitwith(Pred, [], Taken) when is_function(Pred, 1) ->
    {reverse(Taken),[]}.

split(N, List) when is_integer(N), N >= 0, is_list(List) ->
    case split(N, List, []) of
	{_, _} = Result -> Result;
	Fault when is_atom(Fault) ->
	    erlang:error(Fault, [N,List])
    end;
split(N, List) ->
    erlang:error(badarg, [N,List]).

split(0, L, R) ->
    {lists:reverse(R, []), L};
split(N, [H|T], R) ->
    split(N-1, T, [H|R]);
split(_, [], _) ->
    badarg.

%% Versions of the above functions with extra arguments.

all(Pred, Eas, [Hd|Tail]) ->
    case apply(Pred, [Hd|Eas]) of
	true -> all(Pred, Eas, Tail);
	false -> false
    end;
all(Pred, _, []) when is_function(Pred) -> true.

any(Pred, Eas, [Hd|Tail]) ->
    case apply(Pred, [Hd|Eas]) of
	true -> true;
	false -> any(Pred, Eas, Tail)
    end;
any(Pred, _, []) when is_function(Pred) -> false. 

map(F, Eas, List) -> [ apply(F, [E|Eas]) || E <- List ].

flatmap(F, Eas, [Hd|Tail]) ->
    apply(F, [Hd|Eas]) ++ flatmap(F, Eas, Tail);
flatmap(F, _, []) when is_function(F) -> [].

foldl(F, Eas, Accu, [Hd|Tail]) ->
    foldl(F, Eas, apply(F, [Hd,Accu|Eas]), Tail);
foldl(F, _, Accu, []) when is_function(F) -> Accu.

foldr(F, Eas, Accu, [Hd|Tail]) ->
    apply(F, [Hd,foldr(F, Eas, Accu, Tail)|Eas]);
foldr(F, _, Accu, []) when is_function(F) -> Accu.

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
zf(F, _, []) when is_function(F) -> [].

foreach(F, Eas, [Hd|Tail]) ->
    apply(F, [Hd|Eas]),
    foreach(F, Eas, Tail);
foreach(F, _, []) when is_function(F) -> ok.

mapfoldl(F, Eas, Accu0, [Hd|Tail]) ->
    {R,Accu1} = apply(F, [Hd,Accu0|Eas]),
    {Rs,Accu2} = mapfoldl(F, Eas, Accu1, Tail),
    {[R|Rs],Accu2};
mapfoldl(F, _, Accu, []) when is_function(F) -> {[],Accu}.

mapfoldr(F, Eas, Accu0, [Hd|Tail]) ->
    {Rs,Accu1} = mapfoldr(F, Eas, Accu0, Tail),
    {R,Accu2} = apply(F, [Hd,Accu1|Eas]),
    {[R|Rs],Accu2};
mapfoldr(F, _, Accu, []) when is_function(F) -> {[],Accu}.

%% takewhile/2, dropwhile/2 and splitwith/2 do not have versions with
%% extra arguments as this going to be discontinued.

