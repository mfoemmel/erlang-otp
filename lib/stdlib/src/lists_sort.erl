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
-module(lists_sort).

%% These functions are to be used from lists.erl only.

-export([split_1/5, split_2/5, mergel/2, merge3_1/6, rmerge3_1/6,
         merge2_1/4, rmerge2_1/4]).
         
-export([usplit_1/5, usplit_2/5, umergel/2, umerge3_1/6, rumerge3_1/6,
         umerge2_1/4, rumerge2_1/4]).
         
-export([keysort2/2, keymerge/4, ukeysort2/2, ukeymerge/5]).

-export([sort/2, merge2/3, usort/2, umerge2/3]).

-compile({inline, 
	  [{merge3_12,7}, {merge3_21,7}, {rmerge3_12,7}, {rmerge3_21,7}]}).

-compile({inline, 
	  [{umerge3_12,7}, {umerge3_21,7}, {rumerge3_12,7}, {rumerge3_21,7}]}).

%% sort/1

%% Ascending.
split_1(X, Y, [Z | L], R, Rs) when Z >= Y ->
    split_1(Y, Z, L, [X | R], Rs);
split_1(X, Y, [Z | L], R, Rs) when Z >= X ->
    split_1(Z, Y, L, [X | R], Rs);
split_1(X, Y, [Z | L], [], Rs) ->
    split_1(X, Y, L, [Z], Rs);
split_1(X, Y, [Z | L], R, Rs) ->
    split_1_1(X, Y, L, R, Rs, Z);
split_1(X, Y, [], R, Rs) ->
    rmergel([[Y, X | R] | Rs], []).

%% One out-of-order element, S.
split_1_1(X, Y, [Z | L], R, Rs, S) when Z >= Y ->
    split_1_1(Y, Z, L, [X | R], Rs, S);
split_1_1(X, Y, [Z | L], R, Rs, S) when Z >= X ->
    split_1_1(Z, Y, L, [X | R], Rs, S);
split_1_1(X, Y, [Z | L], R, Rs, S) when S =< Z ->
    split_1(S, Z, L, [], [[Y, X | R] | Rs]);
split_1_1(X, Y, [Z | L], R, Rs, S) ->
    split_1(Z, S, L, [], [[Y, X | R] | Rs]);
split_1_1(X, Y, [], R, Rs, S) ->
    rmergel([[S], [Y, X | R] | Rs], []).

%% Descending.
split_2(X, Y, [Z | L], R, Rs) when Z =< Y ->
    split_2(Y, Z, L, [X | R], Rs);
split_2(X, Y, [Z | L], R, Rs) when Z =< X ->
    split_2(Z, Y, L, [X | R], Rs);
split_2(X, Y, [Z | L], [], Rs) ->
    split_2(X, Y, L, [Z], Rs);
split_2(X, Y, [Z | L], R, Rs) ->
    split_2_1(X, Y, L, R, Rs, Z);
split_2(X, Y, [], R, Rs) ->
    mergel([[Y, X | R] | Rs], []).

split_2_1(X, Y, [Z | L], R, Rs, S) when Z =< Y ->
    split_2_1(Y, Z, L, [X | R], Rs, S);
split_2_1(X, Y, [Z | L], R, Rs, S) when Z =< X ->
    split_2_1(Z, Y, L, [X | R], Rs, S);
split_2_1(X, Y, [Z | L], R, Rs, S) when S > Z ->
    split_2(S, Z, L, [], [[Y, X | R] | Rs]);
split_2_1(X, Y, [Z | L], R, Rs, S) ->
    split_2(Z, S, L, [], [[Y, X | R] | Rs]);
split_2_1(X, Y, [], R, Rs, S) ->
    mergel([[S], [Y, X | R] | Rs], []).

%% merge/1

mergel([[] | L], Acc) ->
    mergel(L, Acc);
mergel([A, [H2 | T2], [H3 | T3] | L], Acc) ->
    mergel(L, [merge3_1(A, [], H2, T2, H3,  T3) | Acc]);
mergel([A, [H | T]], Acc) ->
    rmergel([merge2_1(A, H, T, []) | Acc], []);
mergel([L], []) ->
    L;
mergel([L], Acc) ->
    rmergel([lists:reverse(L, []) | Acc], []);
mergel([], []) ->
    [];
mergel([], Acc) ->
    rmergel(Acc, []);
mergel([A, [] | L], Acc) ->
    mergel([A | L], Acc);
mergel([A, B, [] | L], Acc) ->
    mergel([A, B | L], Acc).

rmergel([A, [H2 | T2], [H3 | T3] | L], Acc) ->
    rmergel(L, [rmerge3_1(A, [], H2, T2, H3, T3) | Acc]);
rmergel([A, [H | T]], Acc) ->
    mergel([rmerge2_1(A, H, T, []) | Acc], []);
rmergel([L], Acc) ->
    mergel([lists:reverse(L, []) | Acc], []);
rmergel([], Acc) ->
    mergel(Acc, []).

%% merge3/3

%% Take L1 apart.
merge3_1([H1 | T1], M, H2, T2, H3, T3) when H1 =< H2 ->
    merge3_12(T1, H1, H2, T2, H3, T3, M);
merge3_1([H1 | T1], M, H2, T2, H3, T3) ->
    merge3_21(T1, H1, H2, T2, H3, T3, M);
merge3_1(_nil, M, H2, T2, H3, T3) when H2 =< H3 ->
    merge2_1(T2, H3, T3, [H2 | M]);
merge3_1(_nil, M, H2, T2, H3, T3) ->
    merge2_1(T3, H2, T2, [H3 | M]).

%% Take L2 apart.
merge3_2(T1, H1, M, [H2 | T2], H3, T3) when H1 =< H2 ->
    merge3_12(T1, H1, H2, T2, H3, T3, M);
merge3_2(T1, H1, M, [H2 | T2], H3, T3) ->
    merge3_21(T1, H1, H2, T2, H3, T3, M);
merge3_2(T1, H1, M, _nil, H3, T3) when H1 =< H3 ->
    merge2_1(T1, H3, T3, [H1 | M]);
merge3_2(T1, H1, M, _nil, H3, T3) ->
    merge2_1(T3, H1, T1, [H3 | M]).

% H1 <= H2. Inlined.
merge3_12(T1, H1, H2, T2, H3, T3, M) when H3 < H1 ->
    merge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
merge3_12(T1, H1, H2, T2, H3, T3, M) ->
    merge3_1(T1, [H1 | M], H2, T2, H3, T3).

% H1 <= H2, take L3 apart.
merge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) when H3 < H1 ->
    merge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
merge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    merge3_1(T1, [H1 | M], H2, T2, H3, T3);
merge3_12_3(T1, H1, H2, T2, M, _nil) ->
    merge2_1(T1, H2, T2, [H1 | M]).

% H1 > H2. Inlined.
merge3_21(T1, H1, H2, T2, H3, T3, M) when H3 < H2 ->
    merge3_21_3(T1, H1, H2, T2, [H3 | M], T3);
merge3_21(T1, H1, H2, T2, H3, T3, M) ->
    merge3_2(T1, H1, [H2 | M], T2, H3, T3).

% H1 > H2, take L3 apart.
merge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) when H3 < H2 ->
    merge3_21_3(T1, H1, H2, T2, [H3 | M], T3);
merge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    merge3_2(T1, H1, [H2 | M], T2, H3, T3);
merge3_21_3(T1, H1, H2, T2, M, _nil) ->
    merge2_1(T2, H1, T1, [H2 | M]).

%% rmerge/3

%% Take L1 apart.
rmerge3_1([H1 | T1], M, H2, T2, H3, T3) when H1 > H2 ->
    rmerge3_12(T1, H1, H2, T2, H3, T3, M);
rmerge3_1([H1 | T1], M, H2, T2, H3, T3) ->
    rmerge3_21(T1, H1, H2, T2, H3, T3, M);
rmerge3_1(_nil, M, H2, T2, H3, T3) when H2 > H3 ->
    rmerge2_1(T2, H3, T3, [H2 | M]);
rmerge3_1(_nil, M, H2, T2, H3, T3) ->
    rmerge2_1(T3, H2, T2, [H3 | M]).

%% Take L2 apart.
rmerge3_2(T1, H1, M, [H2 | T2], H3, T3) when H1 > H2 ->
    rmerge3_12(T1, H1, H2, T2, H3, T3, M);
rmerge3_2(T1, H1, M, [H2 | T2], H3, T3) ->
    rmerge3_21(T1, H1, H2, T2, H3, T3, M);
rmerge3_2(T1, H1, M, _nil, H3, T3) when H1 > H3 ->
    rmerge2_1(T1, H3, T3, [H1 | M]);
rmerge3_2(T1, H1, M, _nil, H3, T3) ->
    rmerge2_1(T3, H1, T1, [H3 | M]).

% H1 > H2. Inlined.
rmerge3_12(T1, H1, H2, T2, H3, T3, M) when H3 >= H1 ->
    rmerge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
rmerge3_12(T1, H1, H2, T2, H3, T3, M) ->
    rmerge3_1(T1, [H1 | M], H2, T2, H3, T3).

% H1 > H2, take L3 apart.
rmerge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) when H3 >= H1 ->
    rmerge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
rmerge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    rmerge3_1(T1, [H1 | M], H2, T2, H3, T3);
rmerge3_12_3(T1, H1, H2, T2, M, _nil) ->
    rmerge2_1(T1, H2, T2, [H1 | M]).

% H1 =< H2. Inlined.
rmerge3_21(T1, H1, H2, T2, H3, T3, M) when H3 >= H2 ->
    rmerge3_21_3(T1, H1, H2, T2, [H3 | M], T3);
rmerge3_21(T1, H1, H2, T2, H3, T3, M) ->
    rmerge3_2(T1, H1, [H2 | M], T2, H3, T3).

% H1 =< H2, take L3 apart.
rmerge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) when H3 >= H2 ->
    rmerge3_21_3(T1, H1, H2, T2, [H3 | M], T3);
rmerge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    rmerge3_2(T1, H1, [H2 | M], T2, H3, T3);
rmerge3_21_3(T1, H1, H2, T2, M, _nil) ->
    rmerge2_1(T2, H1, T1, [H2 | M]).

%% merge/2

merge2_1([H1 | T1], H2, T2, M) when H2 < H1 ->
    merge2_2(T1, H1, T2, [H2 | M]);
merge2_1([H1 | T1], H2, T2, M) ->
    merge2_1(T1, H2, T2, [H1 | M]);
merge2_1(_nil, H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

merge2_2(T1, H1, [H2 | T2], M) when H1 < H2 ->
    merge2_1(T1, H2, T2, [H1 | M]);
merge2_2(T1, H1, [H2 | T2], M) ->    
    merge2_2(T1, H1, T2, [H2 | M]);
merge2_2(T1, H1, _nil, M) ->
    lists:reverse(T1, [H1 | M]).

%% rmerge/2

rmerge2_1([H1 | T1], H2, T2, M) when H2 >= H1 ->
    rmerge2_2(T1, H1, T2, [H2 | M]);
rmerge2_1([H1 | T1], H2, T2, M) ->
    rmerge2_1(T1, H2, T2, [H1 | M]);
rmerge2_1(_nil, H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

rmerge2_2(T1, H1, [H2 | T2], M) when H1 >= H2 ->
    rmerge2_1(T1, H2, T2, [H1 | M]);
rmerge2_2(T1, H1, [H2 | T2], M) ->    
    rmerge2_2(T1, H1, T2, [H2 | M]);
rmerge2_2(T1, H1, _nil, M) ->
    lists:reverse(T1, [H1 | M]).

%% usort/1

%% Ascending.
usplit_1(X, Y, [Z | L], R, Rs) when Z > Y ->
    usplit_1(Y, Z, L, [X | R], Rs);
usplit_1(X, Y, [Z | L], R, Rs) when Z == Y ->
    usplit_1(X, Y, L, R, Rs);
usplit_1(X, Y, [Z | L], R, Rs) when Z > X ->
    usplit_1(Z, Y, L, [X | R], Rs);
usplit_1(X, Y, [Z | L], R, Rs) when Z == X ->
    usplit_1(X, Y, L, R, Rs);
usplit_1(X, Y, [Z | L], [], Rs) ->
    usplit_1(X, Y, L, [Z], Rs);
usplit_1(X, Y, [Z | L], R, Rs) ->
    usplit_1_1(X, Y, L, R, Rs, Z);
usplit_1(X, Y, [], R, Rs) ->
    rumergel([[Y, X | R] | Rs], []).

%% One out-of-order element, S.
usplit_1_1(X, Y, [Z | L], R, Rs, S) when Z > Y ->
    usplit_1_1(Y, Z, L, [X | R], Rs, S);
usplit_1_1(X, Y, [Z | L], R, Rs, S) when Z == Y ->
    usplit_1_1(X, Y, L, R, Rs, S);
usplit_1_1(X, Y, [Z | L], R, Rs, S) when Z > X ->
    usplit_1_1(Z, Y, L, [X | R], Rs, S);
usplit_1_1(X, Y, [Z | L], R, Rs, S) when Z == X; Z == S ->
    usplit_1_1(X, Y, L, R, Rs, S);
usplit_1_1(X, Y, [Z | L], R, Rs, S) when S < Z ->
    usplit_1(S, Z, L, [], [[Y, X | R] | Rs]);
usplit_1_1(X, Y, [Z | L], R, Rs, S) ->
    usplit_1(Z, S, L, [], [[Y, X | R] | Rs]);
usplit_1_1(X, Y, [], R, Rs, S) ->
    rumergel([[S], [Y, X | R] | Rs], []).

%% Descending.
usplit_2(X, Y, [Z | L], R, Rs) when Z < Y ->
    usplit_2(Y, Z, L, [X | R], Rs);
usplit_2(X, Y, [Z | L], R, Rs) when Z == Y ->
    usplit_2(X, Y, L, R, Rs);
usplit_2(X, Y, [Z | L], R, Rs) when Z < X ->
    usplit_2(Z, Y, L, [X | R], Rs);
usplit_2(X, Y, [Z | L], R, Rs) when Z == X ->
    usplit_2(X, Y, L, R, Rs);
usplit_2(X, Y, [Z | L], [], Rs) ->
    usplit_2(X, Y, L, [Z], Rs);
usplit_2(X, Y, [Z | L], R, Rs) ->
    usplit_2_1(X, Y, L, R, Rs, Z);
usplit_2(X, Y, [], R, Rs) ->
    umergel([[Y, X | R] | Rs], []).

usplit_2_1(X, Y, [Z | L], R, Rs, S) when Z < Y ->
    usplit_2_1(Y, Z, L, [X | R], Rs, S);
usplit_2_1(X, Y, [Z | L], R, Rs, S) when Z == Y ->
    usplit_2_1(X, Y, L, R, Rs, S);
usplit_2_1(X, Y, [Z | L], R, Rs, S) when Z < X ->
    usplit_2_1(Z, Y, L, [X | R], Rs, S);
usplit_2_1(X, Y, [Z | L], R, Rs, S) when Z == X; Z == S ->
    usplit_2_1(X, Y, L, R, Rs, S);
usplit_2_1(X, Y, [Z | L], R, Rs, S) when S > Z ->
    usplit_2(S, Z, L, [], [[Y, X | R] | Rs]);
usplit_2_1(X, Y, [Z | L], R, Rs, S) ->
    usplit_2(Z, S, L, [], [[Y, X | R] | Rs]);
usplit_2_1(X, Y, [], R, Rs, S) ->
    umergel([[S], [Y, X | R] | Rs], []).

%% umerge/1

umergel([[] | L], Acc) ->
    umergel(L, Acc);
umergel([A, [H2 | T2], [H3 | T3] | L], Acc) ->
    umergel(L, [umerge3_1(A, [], H2, T2, H3,  T3) | Acc]);
umergel([A, [] | L], Acc) ->
    umergel([A | L], Acc);
umergel([A, B, [] | L], Acc) ->
    umergel([A, B | L], Acc);
umergel([A, B | L], Acc) ->
    umergel(L, [umerge(A, B, []) | Acc]);
umergel([L], []) ->
    L;
umergel([L], Acc) ->
    rumergel([lists:reverse(L) | Acc], []);
umergel([], []) ->
    [];
umergel([], Acc) ->
    rumergel(Acc, []).

rumergel([A, [H2 | T2], [H3 | T3] | L], Acc) ->
    rumergel(L, [rumerge3_1(A, [], H2, T2, H3, T3) | Acc]);
rumergel([A, B | L], Acc) ->
    rumergel(L, [rumerge(A, B, []) | Acc]);
rumergel([L], Acc) ->
    umergel([lists:reverse(L) | Acc], []);
rumergel([], Acc) ->
    umergel(Acc, []).

%% umerge3/3

%% Take L1 apart.
umerge3_1([H1 | T1], M, H2, T2, H3, T3) when H1 =< H2 ->
    umerge3_12(T1, H1, H2, T2, H3, T3, M);
umerge3_1([H1 | T1], M, H2, T2, H3, T3) ->
    umerge3_21(T1, H1, H2, T2, H3, T3, M);
umerge3_1(_, M, H2, T2, H3, T3) when H2 == H3 ->
    umerge1(T2, T3, M, H3);
umerge3_1(_, M, H2, T2, H3, T3) when H2 < H3 ->
    umerge1(T2, T3, [H2 | M], H3);
umerge3_1(_, M, H2, T2, H3, T3) ->
    umerge1(T3, T2, [H3 | M], H2).

%% Take L2 apart.
umerge3_2(T1, H1, M, [H2 | T2], H3, T3) when H1 =< H2 ->
    umerge3_12(T1, H1, H2, T2, H3, T3, M);
umerge3_2(T1, H1, M, [H2 | T2], H3, T3) ->
    umerge3_21(T1, H1, H2, T2, H3, T3, M);
umerge3_2(T1, H1, M, _, H3, T3) when H1 == H3 ->
    umerge1(T1, T3, M, H3);
umerge3_2(T1, H1, M, _, H3, T3) when H1 < H3 ->
    umerge1(T1, T3, [H1 | M], H3);
umerge3_2(T1, H1, M, _, H3, T3) ->
    umerge1(T3, T1, [H3 | M], H1).

% H1 <= H2. Inlined.
umerge3_12(T1, H1, H2, T2, H3, T3, M) when H1 == H2 ->
    umerge3_1(T1, M, H2, T2, H3, T3);
umerge3_12(T1, H1, H2, T2, H3, T3, M) when H3 < H1 ->
    umerge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
umerge3_12(T1, H1, H2, T2, H3, T3, M) when H1 == H3 ->
    umerge3_1(T1, M, H2, T2, H3, T3);
umerge3_12(T1, H1, H2, T2, H3, T3, M) ->
    umerge3_1(T1, [H1 | M], H2, T2, H3, T3).

% H1 < H2, take L3 apart.
umerge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) when H3 < H1 ->
    umerge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
umerge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) when H3 == H1 ->
    umerge3_12_3(T1, H1, H2, T2, M, T3);
umerge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    umerge3_1(T1, [H1 | M], H2, T2, H3, T3);
umerge3_12_3(T1, H1, H2, T2, M, _) ->
    umerge1(T1, T2, [H1 | M], H2).

% H1 > H2. Inlined.
umerge3_21(T1, H1, H2, T2, H3, T3, M) when H3 < H2 ->
    umerge3_21_3(T1, H1, H2, T2, [H3 | M], T3);
umerge3_21(T1, H1, H2, T2, H3, T3, M) when H3 == H2 ->
    umerge3_2(T1, H1, M, T2, H3, T3);
umerge3_21(T1, H1, H2, T2, H3, T3, M) ->
    umerge3_2(T1, H1, [H2 | M], T2, H3, T3).

% H1 > H2, take L3 apart.
umerge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) when H3 < H2 ->
    umerge3_21_3(T1, H1, H2, T2, [H3 | M], T3);
umerge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) when H3 == H2 ->
    umerge3_21_3(T1, H1, H2, T2, M, T3);
umerge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    umerge3_2(T1, H1, [H2 | M], T2, H3, T3);
umerge3_21_3(T1, H1, H2, T2, M, _) ->
    umerge1(T2, T1, [H2 | M], H1).

%% Take L1 apart.
rumerge3_1([H1 | T1], M, H2, T2, H3, T3) when H1 > H2 ->
    rumerge3_12(T1, H1, H2, T2, H3, T3, M);
rumerge3_1([H1 | T1], M, H2, T2, H3, T3) ->
    rumerge3_21(T1, H1, H2, T2, H3, T3, M);
rumerge3_1(_, M, H2, T2, H3, T3) when H2 > H3 ->
    rumerge1(T2, T3, [H2 | M], H3);
rumerge3_1(_, M, H2, T2, H3, T3) when H2 == H3 ->
    rumerge1(T2, T3, M, H3);
rumerge3_1(_, M, H2, T2, H3, T3) ->
    rumerge1(T3, T2, [H3 | M], H2).

%% Take L2 apart.
rumerge3_2(T1, H1, M, [H2 | T2], H3, T3) when H1 > H2 ->
    rumerge3_12(T1, H1, H2, T2, H3, T3, M);
rumerge3_2(T1, H1, M, [H2 | T2], H3, T3) ->
    rumerge3_21(T1, H1, H2, T2, H3, T3, M);
rumerge3_2(T1, H1, M, _, H3, T3) when H1 > H3 ->
    rumerge1(T1, T3, [H1 | M], H3);
rumerge3_2(T1, H1, M, _, H3, T3) when H1 == H3 ->
    rumerge1(T1, T3, M, H3);
rumerge3_2(T1, H1, M, _, H3, T3) ->
    rumerge1(T3, T1, [H3 | M], H1).

% H1 > H2. Inlined.
rumerge3_12(T1, H1, H2, T2, H3, T3, M) when H3 > H1 ->
    rumerge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
rumerge3_12(T1, H1, H2, T2, H3, T3, M) when H3 == H1 ->
    rumerge3_1(T1, M, H2, T2, H3, T3);
rumerge3_12(T1, H1, H2, T2, H3, T3, M) ->
    rumerge3_1(T1, [H1 | M], H2, T2, H3, T3).

% H1 > H2, take L3 apart.
rumerge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) when H3 > H1 ->
    rumerge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
rumerge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) when H3 == H1 ->
    rumerge3_12_3(T1, H1, H2, T2, M, T3);
rumerge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    rumerge3_1(T1, [H1 | M], H2, T2, H3, T3);
rumerge3_12_3(T1, H1, H2, T2, M, _) ->
    rumerge1(T1, T2, [H1 | M], H2).

% H1 =< H2. Inlined.
rumerge3_21(T1, H1, H2, T2, H3, T3, M) when H1 == H2 ->
    rumerge3_2(T1, H1, M, T2, H3, T3);
rumerge3_21(T1, H1, H2, T2, H3, T3, M) when H3 > H2 ->
    rumerge3_21_3(T1, H1, H2, T2, [H3 | M], T3);
rumerge3_21(T1, H1, H2, T2, H3, T3, M) when H3 == H2->
    rumerge3_2(T1, H1, M, T2, H3, T3);
rumerge3_21(T1, H1, H2, T2, H3, T3, M) ->
    rumerge3_2(T1, H1, [H2 | M], T2, H3, T3).

% H1 < H2, take L3 apart.
rumerge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) when H3 > H2 ->
    rumerge3_21_3(T1, H1, H2, T2, [H3 | M], T3);
rumerge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) when H3 == H2 ->
    rumerge3_21_3(T1, H1, H2, T2, M, T3);
rumerge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    rumerge3_2(T1, H1, [H2 | M], T2, H3, T3);
rumerge3_21_3(T1, H1, H2, T2, M, _) ->
    rumerge1(T2, T1, [H2 | M], H1).

%% Elements from the first list are kept.
umerge([H1 | T1], [H2 | T2], L) when H1 < H2 ->
    umerge1(T1, T2, [H1 | L], H2);
umerge([H1 | T1], [H2 | T2], L) when H1 == H2 ->
    umerge(T1, T2, [H1 | L]);
umerge([H1 | T1], [H2 | T2], L) ->
    umerge2_1(T1, T2, [H2 | L], H1);
umerge(S1, [], L) ->
    lists:reverse(S1, L);
umerge(_, S2, L) ->
    lists:reverse(S2, L).

umerge1([H1 | T1], T2, L, H2) when H1 < H2 ->
    umerge1(T1, T2, [H1 | L], H2);
umerge1([H1 | T1], T2, L, H2) when H1 == H2 ->
    umerge(T1, T2, [H1 | L]);
umerge1([H1 | T1], T2, L, H2) ->
    umerge2_1(T1, T2, [H2 | L], H1);
umerge1(_, T2, L, H2) ->
    lists:reverse(T2, [H2 | L]).

%% umerge/2

umerge2_1(T1, [H2 | T2], L, H1) when H1 < H2 ->
    umerge1(T1, T2, [H1 | L], H2);
umerge2_1(T1, [H2 | T2], L, H1) when H1 == H2 ->
    umerge(T1, T2, [H1 | L]);
umerge2_1(T1, [H2 | T2], L, H1) ->
    umerge2_1(T1, T2, [H2 | L], H1);
umerge2_1(T1, _, L, H1) ->
    lists:reverse(T1, [H1 | L]).

rumerge([H1 | T1], [H2 | T2], L) when H1 > H2 ->
    rumerge1(T1, T2, [H1 | L], H2);
rumerge([H1 | T1], [H2 | T2], L) when H1 == H2 ->
    rumerge(T1, T2, [H1 | L]);
rumerge([H1 | T1], [H2 | T2], L) ->
    rumerge2_1(T1, T2, [H2 | L], H1);
rumerge(T1, [], L) ->
    lists:reverse(T1, L);
rumerge(_, T2, L) ->
    lists:reverse(T2, L).

rumerge1([H1 | T1], T2, L, H2) when H1 > H2 ->
    rumerge1(T1, T2, [H1 | L], H2);
rumerge1([H1 | T1], T2, L, H2) when H1 == H2 ->
    rumerge(T1, T2, [H1 | L]);
rumerge1([H1 | T1], T2, L, H2) ->
    rumerge2_1(T1, T2, [H2 | L], H1);
rumerge1(_, T2, L, H2) ->
    lists:reverse(T2, [H2 | L]).

%% rumerge/2

rumerge2_1(T1, [H2 | T2], L, H1) when H1 > H2 ->
    rumerge1(T1, T2, [H1 | L], H2);
rumerge2_1(T1, [H2 | T2], L, H1) when H1 == H2 ->
    rumerge(T1, T2, [H1 | L]);
rumerge2_1(T1, [H2 | T2], L, H1) ->
    rumerge2_1(T1, T2, [H2 | L], H1);
rumerge2_1(T1, _, L, H1) ->
    lists:reverse(T1, [H1 | L]).

%% keysort/2

keysort2(_I, [])  -> 
    [];
keysort2(I, [H|T]) ->
    K = element(I, H),
    {Sorted,[]} = samkeyrun(T, H, K, H, K, [H], [], I, -1),
    Sorted.

keysort2([H|T], J, Lim, Run0, I) when J =/= Lim ->
    K = element(I, H),
    {Run2,Rest1} = samkeyrun(T, H, K, H, K, [H], [], I, J),
    Run = keymerge(Run0, Run2, [], I),
    keysort2(Rest1, J+1, Lim, Run, I);
keysort2(Rest, _, _, Run, _) ->
    {Run,Rest}.

samkeyrun([], _EL, _LK, _EH, _HK, L, H, _I, _J) ->
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

%% keymerge/3

keymerge([], Ns, L, _I) ->
    lists:reverse(L, Ns);
keymerge([O|Os], Ns, L, I) ->
    K = element(I, O),
    keymerge(Ns, O, K, Os, L, I).

keymerge([], O, _K, Os, L, _I) ->
    lists:reverse(L, [O|Os]);
keymerge(All = [N|Ns], O, K, Os, L, I) ->
    NK = element(I, N),
    if 
	K =< NK ->
	    keymerge(Os, All, [O|L], I);
	true ->
	    keymerge(Ns, O, K, Os, [N|L], I)
    end.

%% ukeysort/2

ukeysort2(_I, [])  -> 
    [];
ukeysort2(I, [H|T]) ->
    K = element(I, H),
    {Sorted,[]} = usamkeyrun(T, H, K, H, K, [H], [], I, -1, H),
    Sorted.

ukeysort2([H|T], J, Lim, Run0, I) when J =/= Lim ->
    K = element(I, H),
    {Run2,Rest1} = usamkeyrun(T, H, K, H, K, [H], [], I, J, H),
    Run = ukeymerge(Run0, Run2, [], I, last),
    ukeysort2(Rest1, J+1, Lim, Run, I);
ukeysort2(Rest, _, _, Run, _) ->
    {Run,Rest}.

usamkeyrun([], _EL, _LK, _EH, _HK, L, H, _I, _J, _Last) ->
    {L ++ lists:reverse(H, []),[]};
usamkeyrun([E|Es], EL, LK, EH, HK, L, H, I, J, Last) when E == Last ->
    usamkeyrun(Es, EL, LK, EH, HK, L, H, I, J, Last);
usamkeyrun(All=[E|Es], EL, LK, EH, HK, L, H, I, J, _Last) ->
    K = element(I, E),
    if 
	K < LK ->
	    usamkeyrun(Es, E, K, EH, HK, [E|L], H, I, J, E);
	K >= HK ->
	    usamkeyrun(Es, EL, LK, E, K, L, [E|H], I, J, E);
	true ->
	    ukeysort2(All, 1, J, L ++ lists:reverse(H, []), I)
    end.

%% ukeymerge/3

ukeymerge([], [N | Ns], L, I, Last) when N == Last ->
    ukeymerge([], Ns, L, I, Last);
ukeymerge([], [N | Ns], L, I, _Last) ->
    ukeymerge([], Ns, [N | L], I, N);
ukeymerge([], [], L, _I, _Last) ->
    lists:reverse(L);
ukeymerge([O | Os], [], L, I, Last) when O == Last ->
    ukeymerge(Os, [], L, I, Last);
ukeymerge([O | Os], Ns, L, I, Last) ->
    OK = element(I, O),
    ukeymerge(Ns, O, OK, Os, L, I, Last).

ukeymerge([], O, _OK, Os, L, I, _Last) ->
    ukeymerge(Os, [], [O | L], I, O);
ukeymerge([N | Ns] = All, O, OK, Os, L, I, Last) ->
    NK = element(I, N),
    case OK =< NK of
        true when O == Last ->
            ukeymerge(Os, All, L, I, Last);
        true -> 
	    ukeymerge(Os, All, [O | L], I, O);
	false when N == Last ->
	    ukeymerge(Ns, O, OK, Os, L, I, Last);
	false ->
	    ukeymerge(Ns, O, OK, Os, [N | L], I, N)
    end.

%% sort/2

sort(_Fun, []) ->
    [];
sort(Fun, [H|T]) ->
    {Sorted, []} = samrun(T, H, H, [H], [], -1, Fun),
    Sorted.

sort([H|T], J, Lim, Run0, F) when J =/= Lim ->
    {Run2, Rest1} = samrun(T, H, H, [H], [], J, F),
    Run = merge2(F, Run0, Run2),
    sort(Rest1, J+1, Lim, Run, F);
sort(Rest, _, _, Run, _) ->
    {Run, Rest}.

samrun([], _EL, _EH, L, H, _J, _F) ->
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

%% merge/3 

merge2(Fun, X, Y) -> 
    lists:reverse(merge(Fun, X, Y, []), []).

merge(Fun, [H1|T1], [H2|T2], L) ->
    case Fun(H1, H2) of
	true ->
	    merge(Fun, T1, H2, T2, [H1|L]);
	false ->
	    merge(Fun, T2, H1, T1, [H2|L])
    end;
merge(_Fun, [], [H2|T2], L) ->
    lists:reverse(T2, [H2|L]);
merge(_Fun, [H1|T1], [], L) ->
    lists:reverse(T1, [H1|L]);
merge(_Fun, [], [], L) -> L.

merge(Fun, [H1|T1], H2, T2, L) ->
    case Fun(H1, H2) of
	true ->
	    merge(Fun, T1, H2, T2, [H1|L]);
	false ->
	    merge(Fun, T2, H1, T1, [H2|L])
    end;
merge(_Fun, [], H2, T2, L) ->
    lists:reverse(T2, [H2|L]).

%% usort/2

usort(_Fun, []) ->
    [];
usort(Fun, [H|T]) ->
    {Sorted, []} = usamrun(T, H, H, [H], [], -1, Fun, H),
    Sorted.

usort([H|T], J, Lim, Run0, F) when J =/= Lim ->
    {Run2, Rest1} = samrun(T, H, H, [H], [], J, F),
    Run = umerge2(F, Run0, Run2),
    sort(Rest1, J+1, Lim, Run, F);
usort(Rest, _, _, Run, _) ->
    {Run, Rest}.

usamrun([], _EL, _EH, L, H, _J, _F, _Last) ->
    {L ++ lists:reverse(H, []), []};
usamrun([E|Es], EL, EH, L, H, J, F, Last) when E == Last ->
    usamrun(Es, EL, EH, L, H, J, F, Last);
usamrun(R=[E|Es], EL, EH, L, H, J, F, _Last)  ->
    case F(EL, E) of
	false -> % E < EL
	    usamrun(Es, E, EH, [E|L], H, J, F, E);
	true -> 
	    case F(EH, E) of
		true -> % E >= EH
		    usamrun(Es, EL, E, L, [E|H], J, F, E);
		false ->
		    usort(R, 1, J, L++lists:reverse(H, []), F)
	    end
    end.

%% umerge/3 

umerge2(Fun, X, Y) -> 
    umerge(Fun, X, Y, [], last).

umerge(Fun, [], [H2 | T2], L, Last) when H2 == Last ->
    umerge(Fun, [], T2, L, Last);
umerge(Fun, [], [H2 | T2], L, _Last) ->
    umerge(Fun, [], T2, [H2 | L], H2);
umerge(_Fun, [], [], L, _Last) -> 
    lists:reverse(L);
umerge(Fun, [H1 | T1], [], L, Last) when H1 == Last ->
    umerge(Fun, T1, [], L, Last);
umerge(Fun, [H1 | T1], [], L, _Last) ->
    umerge(Fun, T1, [], [H1 | L], H1);
umerge(Fun, [H1 | T1] = X, [H2 | T2] = Y, L, Last) ->
    case Fun(H1, H2) of
        true when H1 == Last ->
            umerge(Fun, T1, Y, L, Last);
	true ->
	    umerge(Fun, T1, Y, [H1 | L], H1);
        false when H2 == Last ->
            umerge(Fun, X, T2, L, Last);
	false ->
	    umerge(Fun, X, T2, [H2 | L], H2)
    end.
