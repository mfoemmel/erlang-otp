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
         merge2_2/4, rmerge2_1/4]).
         
-export([usplit_1/5, usplit_2/5, umergel/1, umerge3_1/6, rumerge3_1/6,
         umerge2_2/4, rumerge2_1/4]).
         
-export([keysplit_1/8, keysplit_2/5, keymerge2_1/6, rkeymerge2_1/6]).

-export([ukeysplit_1/8, ukeysplit_2/5, ukeymerge2_2/6, rukeymerge2_1/6]).

-export([fsplit_1/6, fsplit_2/4, fmerge2_1/5, rfmerge2_1/5]).

-export([ufsplit_1/6, ufsplit_2/4, ufmerge2_2/5, rufmerge2_1/5]).

-compile({inline, 
	  [{merge3_12,7}, {merge3_21,7}, {rmerge3_12,7}, {rmerge3_21,7}]}).

-compile({inline, 
	  [{umerge3_12,7}, {umerge3_21,7}, {rumerge3_12,7},{rumerge3_21,7}]}).

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
mergel([T1, [H2 | T2], [H3 | T3] | L], Acc) ->
    mergel(L, [merge3_1(T1, [], H2, T2, H3,  T3) | Acc]);
mergel([T1, [H2 | T2]], Acc) ->
    rmergel([merge2_1(T1, H2, T2, []) | Acc], []);
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

rmergel([[H3 | T3], [H2 | T2], T1 | L], Acc) ->
    rmergel(L, [rmerge3_1(T1, [], H2, T2, H3, T3) | Acc]);
rmergel([[H2 | T2], T1], Acc) ->
    mergel([rmerge2_1(T1, H2, T2, []) | Acc], []);
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
merge3_1([], M, H2, T2, H3, T3) when H2 =< H3 ->
    merge2_1(T2, H3, T3, [H2 | M]);
merge3_1([], M, H2, T2, H3, T3) ->
    merge2_2(T2, H2, T3, [H3 | M]).

%% Take L2 apart.
merge3_2(T1, H1, M, [H2 | T2], H3, T3) when H1 =< H2 ->
    merge3_12(T1, H1, H2, T2, H3, T3, M);
merge3_2(T1, H1, M, [H2 | T2], H3, T3) ->
    merge3_21(T1, H1, H2, T2, H3, T3, M);
merge3_2(T1, H1, M, [], H3, T3) when H1 =< H3 ->
    merge2_1(T1, H3, T3, [H1 | M]);
merge3_2(T1, H1, M, [], H3, T3) ->
    merge2_2(T1, H1, T3, [H3 | M]).

% H1 =< H2. Inlined.
merge3_12(T1, H1, H2, T2, H3, T3, M) when H3 < H1 ->
    merge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
merge3_12(T1, H1, H2, T2, H3, T3, M) ->
    merge3_1(T1, [H1 | M], H2, T2, H3, T3).

% H1 =< H2, take L3 apart.
merge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) when H3 < H1 ->
    merge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
merge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    merge3_1(T1, [H1 | M], H2, T2, H3, T3);
merge3_12_3(T1, H1, H2, T2, M, []) ->
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
merge3_21_3(T1, H1, H2, T2, M, []) ->
    merge2_2(T1, H1, T2, [H2 | M]).

%% rmerge/3

%% Take L1 apart.
rmerge3_1([H1 | T1], M, H2, T2, H3, T3) when H1 > H2 ->
    rmerge3_12(T1, H1, H2, T2, H3, T3, M);
rmerge3_1([H1 | T1], M, H2, T2, H3, T3) ->
    rmerge3_21(T1, H1, H2, T2, H3, T3, M);
rmerge3_1([], M, H2, T2, H3, T3) when H2 > H3 ->
    rmerge2_1(T2, H3, T3, [H2 | M]);
rmerge3_1([], M, H2, T2, H3, T3) ->
    rmerge2_2(T2, H2, T3, [H3 | M]).

%% Take L2 apart.
rmerge3_2(T1, H1, M, [H2 | T2], H3, T3) when H1 > H2 ->
    rmerge3_12(T1, H1, H2, T2, H3, T3, M);
rmerge3_2(T1, H1, M, [H2 | T2], H3, T3) ->
    rmerge3_21(T1, H1, H2, T2, H3, T3, M);
rmerge3_2(T1, H1, M, [], H3, T3) when H1 > H3 ->
    rmerge2_1(T1, H3, T3, [H1 | M]);
rmerge3_2(T1, H1, M, [], H3, T3) ->
    rmerge2_2(T1, H1, T3, [H3 | M]).

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
rmerge3_12_3(T1, H1, H2, T2, M, []) ->
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
rmerge3_21_3(T1, H1, H2, T2, M, []) ->
    rmerge2_2(T1, H1, T2, [H2 | M]).

%% merge/2

merge2_1([H1 | T1], H2, T2, M) when H1 =< H2 ->
    merge2_1(T1, H2, T2, [H1 | M]);
merge2_1([H1 | T1], H2, T2, M) ->
    merge2_2(T1, H1, T2, [H2 | M]);
merge2_1([], H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

merge2_2(T1, H1, [H2 | T2], M) when H1 =< H2 ->
    merge2_1(T1, H2, T2, [H1 | M]);
merge2_2(T1, H1, [H2 | T2], M) ->
    merge2_2(T1, H1, T2, [H2 | M]);
merge2_2(T1, H1, [], M) ->
    lists:reverse(T1, [H1 | M]).

%% rmerge/2

rmerge2_1([H1 | T1], H2, T2, M) when H1 =< H2 ->
    rmerge2_2(T1, H1, T2, [H2 | M]);
rmerge2_1([H1 | T1], H2, T2, M) ->
    rmerge2_1(T1, H2, T2, [H1 | M]);
rmerge2_1([], H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

rmerge2_2(T1, H1, [H2 | T2], M) when H1 =< H2 ->
    rmerge2_2(T1, H1, T2, [H2 | M]);
rmerge2_2(T1, H1, [H2 | T2], M) ->
    rmerge2_1(T1, H2, T2, [H1 | M]);
rmerge2_2(T1, H1, [], M) ->
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
    rumergel([[Y, X | R] | Rs], [], asc).

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
    rumergel([[S], [Y, X | R] | Rs], [], asc).

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
    umergel([[Y, X | R] | Rs], [], desc).

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
    umergel([[S], [Y, X | R] | Rs], [], desc).

%% umerge/1

umergel(L) ->
    umergel(L, [], asc).

umergel([[] | L], Acc, O) ->
    umergel(L, Acc, O);
umergel([T1, [H2 | T2], [H3 | T3] | L], Acc, asc) ->
    umergel(L, [umerge3_1(T1, [], H2, T2, H3,  T3) | Acc], asc);
umergel([[H3 | T3], [H2 | T2], T1 | L], Acc, desc) ->
    umergel(L, [umerge3_1(T1, [], H2, T2, H3,  T3) | Acc], desc);
umergel([A, [] | L], Acc, O) ->
    umergel([A | L], Acc, O);
umergel([A, B, [] | L], Acc, O) ->
    umergel([A, B | L], Acc, O);
umergel([[H1 | T1], T2 | L], Acc, asc) ->
    umergel(L, [umerge2_2(T1, T2, [], H1) | Acc], asc);
umergel([T2, [H1 | T1] | L], Acc, desc) ->
    umergel(L, [umerge2_2(T1, T2, [], H1) | Acc], desc);
umergel([L], [], _O) ->
    L;
umergel([L], Acc, O) ->
    rumergel([lists:reverse(L, []) | Acc], [], O);
umergel([], [], _O) ->
    [];
umergel([], Acc, O) ->
    rumergel(Acc, [], O).

rumergel([[H3 | T3], [H2 | T2], T1 | L], Acc, asc) ->
    rumergel(L, [rumerge3_1(T1, [], H2, T2, H3, T3) | Acc], asc);
rumergel([T1, [H2 | T2], [H3 | T3] | L], Acc, desc) ->
    rumergel(L, [rumerge3_1(T1, [], H2, T2, H3, T3) | Acc], desc);
rumergel([[H2 | T2], T1 | L], Acc, asc) ->
    rumergel(L, [rumerge2_1(T1, T2, [], H2) | Acc], asc);
rumergel([T1, [H2 | T2] | L], Acc, desc) ->
    rumergel(L, [rumerge2_1(T1, T2, [], H2) | Acc], desc);
rumergel([L], Acc, O) ->
    umergel([lists:reverse(L, []) | Acc], [], O);
rumergel([], Acc, O) ->
    umergel(Acc, [], O).

%% umerge3/3

%% Take L1 apart.
umerge3_1([H1 | T1], M, H2, T2, H3, T3) when H1 =< H2 ->
    umerge3_12(T1, H1, H2, T2, H3, T3, M);
umerge3_1([H1 | T1], M, H2, T2, H3, T3) ->
    umerge3_21(T1, H1, H2, T2, H3, T3, M);
umerge3_1([], M, H2, T2, H3, T3) when H2 =< H3 ->
    umerge2_1(T2, T3, [H2 | M], H2, H3);
umerge3_1([], M, H2, T2, H3, T3) ->
    umerge2_2(T2, T3, [H3 | M], H2).

%% Take L2 apart.
umerge3_2(T1, H1, M, [H2 | T2], H3, T3) when H1 =< H2 ->
    umerge3_12(T1, H1, H2, T2, H3, T3, M);
umerge3_2(T1, H1, M, [H2 | T2], H3, T3) ->
    umerge3_21(T1, H1, H2, T2, H3, T3, M);
umerge3_2(T1, H1, M, [], H3, T3) when H1 =< H3 ->
    umerge2_1(T1, T3, [H1 | M], H1, H3);
umerge3_2(T1, H1, M, [], H3, T3) ->
    umerge2_2(T1, T3, [H3 | M], H1).

% H1 =< H2. Inlined.
umerge3_12(T1, H1, H2, T2, H3, T3, M) when H1 == H2 ->
    umerge3_1(T1, M, H1, T2, H3, T3);
umerge3_12(T1, H1, H2, T2, H3, T3, M) when H3 < H1 ->
    umerge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
umerge3_12(T1, H1, H2, T2, H3, T3, M) when H1 == H3 ->
    umerge3_1(T1, M, H2, T2, H1, T3);
umerge3_12(T1, H1, H2, T2, H3, T3, M) ->
    umerge3_1(T1, [H1 | M], H2, T2, H3, T3).

% H1 < H2, take L3 apart.
umerge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) when H3 < H1 ->
    umerge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
umerge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) when H3 == H1 ->
    umerge3_12_3(T1, H1, H2, T2, M, T3);
umerge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    umerge3_1(T1, [H1 | M], H2, T2, H3, T3);
umerge3_12_3(T1, H1, H2, T2, M, []) ->
    umerge2_1(T1, T2, [H1 | M], H1, H2).

% H1 > H2. Inlined.
umerge3_21(T1, H1, H2, T2, H3, T3, M) when H3 < H2 ->
    umerge3_21_3(T1, H1, H2, T2, [H3 | M], T3);
umerge3_21(T1, H1, H2, T2, H3, T3, M) when H3 == H2 ->
    umerge3_2(T1, H1, M, T2, H2, T3);
umerge3_21(T1, H1, H2, T2, H3, T3, M) ->
    umerge3_2(T1, H1, [H2 | M], T2, H3, T3).

% H1 > H2, take L3 apart.
umerge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) when H3 < H2 ->
    umerge3_21_3(T1, H1, H2, T2, [H3 | M], T3);
umerge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) when H3 == H2 ->
    umerge3_21_3(T1, H1, H2, T2, M, T3);
umerge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    umerge3_2(T1, H1, [H2 | M], T2, H3, T3);
umerge3_21_3(T1, H1, H2, T2, M, []) ->
    umerge2_2(T1, T2, [H2 | M], H1).

%% Take L1 apart.
rumerge3_1([H1 | T1], M, H2, T2, H3, T3) when H1 > H2 ->
    rumerge3_12(T1, H1, H2, T2, H3, T3, M);
rumerge3_1([H1 | T1], M, H2, T2, H3, T3) ->
    rumerge3_21(T1, H1, H2, T2, H3, T3, M);
rumerge3_1([], M, H2, T2, H3, T3) when H2 > H3 ->
    rumerge2_1(T2, T3, [H2 | M], H3);
rumerge3_1([], M, H2, T2, H3, T3) ->
    rumerge2_2(T2, T3, M, H3, H2).

%% Take L2 apart.
rumerge3_2(T1, H1, M, [H2 | T2], H3, T3) when H1 > H2 ->
    rumerge3_12(T1, H1, H2, T2, H3, T3, M);
rumerge3_2(T1, H1, M, [H2 | T2], H3, T3) ->
    rumerge3_21(T1, H1, H2, T2, H3, T3, M);
rumerge3_2(T1, H1, M, [], H3, T3) when H1 > H3 ->
    rumerge2_1(T1, T3, [H1 | M], H3);
rumerge3_2(T1, H1, M, [], H3, T3) ->
    rumerge2_2(T1, T3, M, H3, H1).

% H1 > H2. Inlined.
rumerge3_12(T1, H1, H2, T2, H3, T3, M) when H3 > H1 ->
    rumerge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
rumerge3_12(T1, H1, H2, T2, H3, T3, M) when H3 == H1 ->
    rumerge3_1(T1, M, H2, T2, H1, T3);
rumerge3_12(T1, H1, H2, T2, H3, T3, M) ->
    rumerge3_1(T1, [H1 | M], H2, T2, H3, T3).

% H1 > H2, take L3 apart.
rumerge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) when H3 > H1 ->
    rumerge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
rumerge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) when H3 == H1 ->
    rumerge3_12_3(T1, H1, H2, T2, M, T3);
rumerge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    rumerge3_1(T1, [H1 | M], H2, T2, H3, T3);
rumerge3_12_3(T1, H1, H2, T2, M, []) ->
    rumerge2_1(T1, T2, [H1 | M], H2).

% H1 =< H2. Inlined.
rumerge3_21(T1, H1, H2, T2, H3, T3, M) when H1 == H2 ->
    rumerge3_2(T1, H1, M, T2, H3, T3);
rumerge3_21(T1, H1, H2, T2, H3, T3, M) when H3 > H2 ->
    rumerge3_21_3(T1, H1, H2, T2, [H3 | M], T3);
rumerge3_21(T1, H1, H2, T2, H3, T3, M) when H3 == H2->
    rumerge3_2(T1, H1, M, T2, H2, T3);
rumerge3_21(T1, H1, H2, T2, H3, T3, M) ->
    rumerge3_2(T1, H1, [H2 | M], T2, H3, T3).

% H1 < H2, take L3 apart.
rumerge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) when H3 > H2 ->
    rumerge3_21_3(T1, H1, H2, T2, [H3 | M], T3);
rumerge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) when H3 == H2 ->
    rumerge3_21_3(T1, H1, H2, T2, M, T3);
rumerge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    rumerge3_2(T1, H1, [H2 | M], T2, H3, T3);
rumerge3_21_3(T1, H1, H2, T2, M, []) ->
    rumerge2_2(T1, T2, M, H2, H1).

%% umerge/2

%% Elements from the first list are kept and prioritized.
umerge2_1([H1 | T1], T2, M, _HdM, H2) when H1 =< H2 ->
    umerge2_1(T1, T2, [H1 | M], H1, H2);
umerge2_1([H1 | T1], T2, M, HdM, H2) when H2 == HdM ->
    umerge2_2(T1, T2, M, H1);
umerge2_1([H1 | T1], T2, M, _HdM, H2) ->
    umerge2_2(T1, T2, [H2 | M], H1);
umerge2_1([], T2, M, HdM, H2) when H2 == HdM ->
    lists:reverse(T2, M);
umerge2_1([], T2, M, _HdM, H2) ->
    lists:reverse(T2, [H2 | M]).

umerge2_2(T1, [H2 | T2], M, H1) when H1 =< H2 ->
    umerge2_1(T1, T2, [H1 | M], H1, H2);
umerge2_2(T1, [H2 | T2], M, H1) ->
    umerge2_2(T1, T2, [H2 | M], H1);
umerge2_2(T1, [], M, H1) ->
    lists:reverse(T1, [H1 | M]).

%% rumerge/2

%% Elements from the first list are kept and prioritized.
rumerge2_1([H1 | T1], T2, M, H2) when H1 =< H2 ->
    rumerge2_2(T1, T2, M, H2, H1);
rumerge2_1([H1 | T1], T2, M, H2) ->
    rumerge2_1(T1, T2, [H1 | M], H2);
rumerge2_1([], T2, M, H2) ->
    lists:reverse(T2, [H2 | M]).

rumerge2_2(T1, [H2 | T2], M, H2M, H1) when H1 =< H2 ->
    rumerge2_2(T1, T2, [H2M | M], H2, H1);
rumerge2_2(T1, [H2 | T2], M, H2M, H1) when H2M == H1 ->
    rumerge2_1(T1, T2, [H1 | M], H2);
rumerge2_2(T1, [H2 | T2], M, H2M, H1) ->
    rumerge2_1(T1, T2, [H1, H2M | M], H2);
rumerge2_2(T1, [], M, H2M, H1) when H2M == H1 ->
    lists:reverse(T1, [H1 | M]);
rumerge2_2(T1, [], M, H2M, H1) ->
    lists:reverse(T1, [H1, H2M | M]).

%% keysort/2

%% Ascending.
keysplit_1(I, X, EX, Y, EY, [Z | L], R, Rs) ->
    EZ = element(I, Z),
    if 
	EY =< EZ ->
	    keysplit_1(I, Y, EY, Z, EZ, L, [X | R], Rs);
	EX =< EZ ->
	    keysplit_1(I, Z, EZ, Y, EY, L, [X | R], Rs);
	true, R == [] ->
	    keysplit_1(I, X, EX, Y, EY, L, [Z], Rs);
	true ->
	    keysplit_1_1(I, X, EX, Y, EY, L, R, Rs, Z, EZ)
    end;
keysplit_1(I, X, _EX, Y, _EY, [], R, Rs) ->
    rkeymergel(I, [[Y, X | R] | Rs], []).

%% One out-of-order element, S.
keysplit_1_1(I, X, EX, Y, EY, [Z | L], R, Rs, S, ES) ->
    EZ = element(I, Z),
    if
	EY =< EZ ->
	    keysplit_1_1(I, Y, EY, Z, EZ, L, [X | R], Rs, S, ES);
	EX =< EZ ->
	    keysplit_1_1(I, Z, EZ, Y, EY, L, [X | R], Rs, S, ES);
	ES =< EZ ->
	    keysplit_1(I, S, ES, Z, EZ, L, [], [[Y, X | R] | Rs]);
	true ->
	    keysplit_1(I, Z, EZ, S, ES, L, [], [[Y, X | R] | Rs])
    end;
keysplit_1_1(I, X, _EX, Y, _EY, [], R, Rs, S, _ES) ->
    rkeymergel(I, [[S], [Y, X | R] | Rs], []).

%% Descending.
keysplit_2(I, Y, EY, [Z | L], R) ->
    EZ = element(I, Z),
    if
	EY =< EZ ->
            keysplit_1(I, Y, EY, Z, EZ, L, [], [lists:reverse(R, [])]);
        true ->
            keysplit_2(I, Z, EZ, L, [Y | R])
    end;
keysplit_2(_I, Y, _EY, [], R) ->
    [Y | R].

keymergel(I, [T1, [H2 | T2] | L], Acc) ->
    keymergel(I, L, [keymerge2_1(I, T1, element(I, H2), H2, T2, []) | Acc]);
keymergel(_I, [L], []) ->
    L;
keymergel(I, [L], Acc) ->
    rkeymergel(I, [lists:reverse(L, []) | Acc], []);
keymergel(I, [], Acc) ->
    rkeymergel(I, Acc, []).

rkeymergel(I, [[H2 | T2], T1 | L], Acc) ->
    rkeymergel(I, L, [rkeymerge2_1(I, T1, element(I, H2), H2, T2, []) | Acc]);
rkeymergel(I, [L], Acc) ->
    keymergel(I, [lists:reverse(L, []) | Acc], []);
rkeymergel(I, [], Acc) ->
    keymergel(I, Acc, []).

%% keymerge/3

%% Elements from the first list are prioritized.
keymerge2_1(I, [H1 | T1], E2, H2, T2, M) ->
    E1 = element(I, H1),
    if 
	E1 =< E2 ->
	    keymerge2_1(I, T1, E2, H2, T2, [H1 | M]);
	true ->
	    keymerge2_2(I, T1, E1, H1, T2, [H2 | M])
    end;
keymerge2_1(_I, [], _E2, H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

keymerge2_2(I, T1, E1, H1, [H2 | T2], M) ->
    E2 = element(I, H2),
    if
	E1 =< E2 ->
	    keymerge2_1(I, T1, E2, H2, T2, [H1 | M]);
	true ->
	    keymerge2_2(I, T1, E1, H1, T2, [H2 | M])
    end;
keymerge2_2(_I, T1, _E1, H1, [], M) ->
    lists:reverse(T1, [H1 | M]).

%% rkeymerge/3

rkeymerge2_1(I, [H1 | T1], E2, H2, T2, M) ->
    E1 = element(I, H1),
    if
	E1 =< E2 ->
	    rkeymerge2_2(I, T1, E1, T2, [H2 | M], H1);
	true ->
	    rkeymerge2_1(I, T1, E2, H2, T2, [H1 | M])
    end;
rkeymerge2_1(_I, [], _E2, H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

rkeymerge2_2(I, T1, E1, [H2 | T2], M, H1) ->
    E2 = element(I, H2),
    if
	E1 =< E2 ->
	    rkeymerge2_2(I, T1, E1, T2, [H2 | M], H1);
	true ->
	    rkeymerge2_1(I, T1, E2, H2, T2, [H1 | M])
    end;
rkeymerge2_2(_I, T1, _E1, [], M, H1) ->
    lists:reverse(T1, [H1 | M]).

%% ukeysort/2

%% Ascending.
ukeysplit_1(I, X, EX, Y, EY, [Z | L], R, Rs) when Y == Z ->
    ukeysplit_1(I, X, EX, Y, EY, L, R, Rs);
ukeysplit_1(I, X, EX, Y, EY, [Z | L], R, Rs) ->
    EZ = element(I, Z),
    if
	EY =< EZ ->
	    ukeysplit_1(I, Y, EY, Z, EZ, L, [X | R], Rs);
        X == Z ->
	    ukeysplit_1(I, X, EX, Y, EY, L, R, Rs);
        EX =< EZ ->
	    ukeysplit_1(I, Z, EZ, Y, EY, L, [X | R], Rs);
        R == [] ->
	    ukeysplit_1(I, X, EX, Y, EY, L, [Z], Rs);
        true ->
	    ukeysplit_1_1(I, X, EX, Y, EY, L, R, Rs, Z, EZ)
    end;
ukeysplit_1(I, X, _EX, Y, _EY, [], R, Rs) ->
    rukeymergel(I, [[Y, X | R] | Rs], []).

%% One out-of-order element, S.
ukeysplit_1_1(I, X, EX, Y, EY, [Z | L], R, Rs, S, ES) when Y == Z ->
    ukeysplit_1_1(I, X, EX, Y, EY, L, R, Rs, S, ES);
ukeysplit_1_1(I, X, EX, Y, EY, [Z | L], R, Rs, S, ES) ->
    EZ = element(I, Z),
    if
	EY =< EZ ->
	    ukeysplit_1_1(I, Y, EY, Z, EZ, L, [X | R], Rs, S, ES);
	X == Z ->
	    ukeysplit_1_1(I, X, EX, Y, EY, L, R, Rs, S, ES);
	EX =< EZ ->
	    ukeysplit_1_1(I, Z, EZ, Y, EY, L, [X | R], Rs, S, ES);
	S == Z ->
	    ukeysplit_1_1(I, X, EX, Y, EY, L, R, Rs, S, ES);
	ES =< EZ ->
	    ukeysplit_1(I, S, ES, Z, EZ, L, [], [[Y, X | R] | Rs]);
	true ->
	    ukeysplit_1(I, Z, EZ, S, ES, L, [], [[Y, X | R] | Rs])
    end;
ukeysplit_1_1(I, X, _EX, Y, _EY, [], R, Rs, S, _ES) ->
    rukeymergel(I, [[S], [Y, X | R] | Rs], []).

%% Descending.
ukeysplit_2(I, Y, EY, [Z | L], R) ->
    EZ = element(I, Z),
    if
	Y == Z ->
	    ukeysplit_2(I, Y, EY, L, R);
	EY =< EZ ->
            ukeysplit_1(I, Y, EY, Z, EZ, L, [], [lists:reverse(R, [])]);
	true ->
            ukeysplit_2(I, Z, EZ, L, [Y | R])
    end;
ukeysplit_2(_I, Y, _EY, [], R) ->
    [Y | R].

ukeymergel(I, [[H1 | T1], T2 | L], Acc) ->
    ukeymergel(I, L, [ukeymerge2_2(I, T1, element(I, H1), H1, T2, []) | Acc]);
ukeymergel(_I, [L], []) ->
    L;
ukeymergel(I, [L], Acc) ->
    rukeymergel(I, [lists:reverse(L, []) | Acc], []);
ukeymergel(I, [], Acc) ->
    rukeymergel(I, Acc, []).

rukeymergel(I, [[H2 | T2], T1 | L], Acc) ->
    rukeymergel(I, L, [rukeymerge2_1(I, T1, element(I,H2), T2, [], H2)|Acc]);
rukeymergel(I, [L], Acc) ->
    ukeymergel(I, [lists:reverse(L, []) | Acc], []);
rukeymergel(I, [], Acc) ->
    ukeymergel(I, Acc, []).

%% ukeymerge/3

%% Elements from the first list are kept and prioritized.
ukeymerge2_1(I, [H1 | T1], E2, HdM, T2, M, H2) ->
    E1 = element(I, H1),
    if
	E1 =< E2 ->
	    ukeymerge2_1(I, T1, E2, H1, T2, [H1 | M], H2);
	H2 == HdM ->
	    ukeymerge2_2(I, T1, E1, H1, T2, M);
	true ->
	    ukeymerge2_2(I, T1, E1, H1, T2, [H2 | M])
    end;
ukeymerge2_1(_I, [], _E2, HdM, T2, M, H2) when H2 == HdM ->
    lists:reverse(T2, M);
ukeymerge2_1(_I, [], _E2, _HdM, T2, M, H2) ->
    lists:reverse(T2, [H2 | M]).

ukeymerge2_2(I, T1, E1, H1, [H2 | T2], M) ->
    E2 = element(I, H2),
    if
	E1 =< E2 ->
	    ukeymerge2_1(I, T1, E2, H1, T2, [H1 | M], H2);
	true ->
	    ukeymerge2_2(I, T1, E1, H1, T2, [H2 | M])
    end;
ukeymerge2_2(_I, T1, _E1, H1, [], M) ->
    lists:reverse(T1, [H1 | M]).

%% rukeymerge/3

rukeymerge2_1(I, [H1 | T1], E2, T2, M, H2) ->
    E1 = element(I, H1),
    if
	E1 =< E2 ->
	    rukeymerge2_2(I, T1, E1, T2, M, H2, H1);
	true ->
	    rukeymerge2_1(I, T1, E2, T2, [H1 | M], H2)
    end;
rukeymerge2_1(_I, [], _E2, T2, M, H2) ->
    lists:reverse(T2, [H2 | M]).

rukeymerge2_2(I, T1, E1, [H2 | T2], M, H2M, H1) ->
    E2 = element(I, H2),
    if
	E1 =< E2 ->
	    rukeymerge2_2(I, T1, E1, T2, [H2M | M], H2, H1);
	H2M == H1 ->
	    rukeymerge2_1(I, T1, E2, T2, [H1 | M], H2);
	true ->
	    rukeymerge2_1(I, T1, E2, T2, [H1, H2M | M], H2)
    end;
rukeymerge2_2(_I, T1, _E1, [], M, H2M, H1) when H2M == H1 ->
    lists:reverse(T1, [H1 | M]);
rukeymerge2_2(_I, T1, _E1, [], M, H2M, H1) ->
    lists:reverse(T1, [H1, H2M | M]).

%% sort/2

%% Ascending.
fsplit_1(Y, X, Fun, [Z | L], R, Rs) ->
    case Fun(Y, Z) of
	true -> 
	    fsplit_1(Z, Y, Fun, L, [X | R], Rs);
	false ->
	    case Fun(X, Z) of
		true ->
		    fsplit_1(Y, Z, Fun, L, [X | R], Rs);
		false when R == [] ->
		    fsplit_1(Y, X, Fun, L, [Z], Rs);
		false ->
		    fsplit_1_1(Y, X, Fun, L, R, Rs, Z)
	    end
    end;
fsplit_1(Y, X, Fun, [], R, Rs) ->
    rfmergel([[Y, X | R] | Rs], [], Fun).

%% One out-of-order element, S.
fsplit_1_1(Y, X, Fun, [Z | L], R, Rs, S) ->
    case Fun(Y, Z) of
	true ->
	    fsplit_1_1(Z, Y, Fun, L, [X | R], Rs, S);
	false ->
	    case Fun(X, Z) of
		true ->
		    fsplit_1_1(Y, Z, Fun, L, [X | R], Rs, S);
		false ->
		    case Fun(S, Z) of
			true ->
			    fsplit_1(Z, S, Fun, L, [], [[Y, X | R] | Rs]);
			false ->
			    fsplit_1(S, Z, Fun, L, [], [[Y, X | R] | Rs])
		    end
	    end
    end;
fsplit_1_1(Y, X, Fun, [], R, Rs, S) ->
    rfmergel([[S], [Y, X | R] | Rs], [], Fun).

%% Descending.
fsplit_2(Y, [Z | L], Fun, R) ->
    case Fun(Y, Z) of
        true ->
            fsplit_1(Z, Y, Fun, L, [], [lists:reverse(R, [])]);
        false ->
            fsplit_2(Z, L, Fun, [Y | R])
    end;
fsplit_2(Y, [], _Fun, R) ->
    [Y | R].

fmergel([T1, [H2 | T2] | L], Acc, Fun) ->
    fmergel(L, [fmerge2_1(T1, H2, Fun, T2, []) | Acc], Fun);
fmergel([L], [], _Fun) ->
    L;
fmergel([L], Acc, Fun) ->
    rfmergel([lists:reverse(L, []) | Acc], [], Fun);
fmergel([], Acc, Fun) ->
    rfmergel(Acc, [], Fun).

rfmergel([[H2 | T2], T1 | L], Acc, Fun) ->
    rfmergel(L, [rfmerge2_1(T1, H2, Fun, T2, []) | Acc], Fun);
rfmergel([L], Acc, Fun) ->
    fmergel([lists:reverse(L, []) | Acc], [], Fun);
rfmergel([], Acc, Fun) ->
    fmergel(Acc, [], Fun).

%% merge/3 

%% Elements from the first list are prioritized.
fmerge2_1([H1 | T1], H2, Fun, T2, M) ->
    case Fun(H1, H2) of
	true ->
	    fmerge2_1(T1, H2, Fun, T2, [H1 | M]);
	false ->
	    fmerge2_2(H1, T1, Fun, T2, [H2 | M])
    end;
fmerge2_1([], H2, _Fun, T2, M) ->
    lists:reverse(T2, [H2 | M]).

fmerge2_2(H1, T1, Fun, [H2 | T2], M) ->
    case Fun(H1, H2) of
	true ->
	    fmerge2_1(T1, H2, Fun, T2, [H1 | M]);
	false ->
	    fmerge2_2(H1, T1, Fun, T2, [H2 | M])
    end;
fmerge2_2(H1, T1, _Fun, [], M) ->
    lists:reverse(T1, [H1 | M]).

%% rmerge/3

rfmerge2_1([H1 | T1], H2, Fun, T2, M) ->
    case Fun(H1, H2) of
	true ->
	    rfmerge2_2(H1, T1, Fun, T2, [H2 | M]);
	false ->
	    rfmerge2_1(T1, H2, Fun, T2, [H1 | M])
    end;
rfmerge2_1([], H2, _Fun, T2, M) ->
    lists:reverse(T2, [H2 | M]).

rfmerge2_2(H1, T1, Fun, [H2 | T2], M) ->
    case Fun(H1, H2) of
	true ->
	    rfmerge2_2(H1, T1, Fun, T2, [H2 | M]);
	false ->
	    rfmerge2_1(T1, H2, Fun, T2, [H1 | M])
    end;
rfmerge2_2(H1, T1, _Fun, [], M) ->
    lists:reverse(T1, [H1 | M]).

%% usort/2

%% Ascending.
ufsplit_1(Y, X, Fun, [Z | L], R, Rs) ->
    case Fun(Y, Z) of
	true when Y == Z ->
	    ufsplit_1(Y, X, Fun, L, R, Rs);
	true ->
	    ufsplit_1(Z, Y, Fun, L, [X | R], Rs);
	false ->
	    case Fun(X, Z) of
		true when X == Z ->
		    ufsplit_1(Y, X, Fun, L, R, Rs);
		true ->
		    ufsplit_1(Y, Z, Fun, L, [X | R], Rs);
		false when R == [] ->
		    ufsplit_1(Y, X, Fun, L, [Z], Rs);
		false ->
		    ufsplit_1_1(Y, X, Fun, L, R, Rs, Z)
	    end
    end;
ufsplit_1(Y, X, Fun, [], R, Rs) ->
    rufmergel([[Y, X | R] | Rs], [], Fun).

%% One out-of-order element, S.
ufsplit_1_1(Y, X, Fun, [Z | L], R, Rs, S) ->
    case Fun(Y, Z) of
	true when Y == Z ->
            ufsplit_1_1(Y, X, Fun, L, R, Rs, S);
	true -> 
	    ufsplit_1_1(Z, Y, Fun, L, [X | R], Rs, S);
	false ->
	    case Fun(X, Z) of
		true when X == Z ->
		    ufsplit_1_1(Y, X, Fun, L, R, Rs, S);
		true ->
		    ufsplit_1_1(Y, Z, Fun, L, [X | R], Rs, S);
		false ->
		    case Fun(S, Z) of
			true when S == Z ->
			    ufsplit_1_1(Y, X, Fun, L, R, Rs, S);
			true ->
			    ufsplit_1(Z, S, Fun, L, [], [[Y, X | R] | Rs]);
			false ->
			    ufsplit_1(S, Z, Fun, L, [], [[Y, X | R] | Rs])
		    end
	    end
    end;
ufsplit_1_1(Y, X, Fun, [], R, Rs, S) ->
    rufmergel([[S], [Y, X | R] | Rs], [], Fun).

%% Descending.
ufsplit_2(Y, [Z | L], Fun, R) ->
    case Fun(Y, Z) of
	true when Y == Z ->
	    ufsplit_2(Y, L, Fun, R);
        true ->
            ufsplit_1(Z, Y, Fun, L, [], [lists:reverse(R, [])]);
        false ->
            ufsplit_2(Z, L, Fun, [Y | R])
    end;
ufsplit_2(Y, [], _Fun, R) ->
    [Y | R].

ufmergel([[H1 | T1], T2 | L], Acc, Fun) ->
    ufmergel(L, [ufmerge2_2(H1, T1, Fun, T2, []) | Acc], Fun);
ufmergel([L], [], _Fun) ->
    L;
ufmergel([L], Acc, Fun) ->
    rufmergel([lists:reverse(L, []) | Acc], [], Fun);
ufmergel([], Acc, Fun) ->
    rufmergel(Acc, [], Fun).

rufmergel([[H2 | T2], T1 | L], Acc, Fun) ->
    rufmergel(L, [rufmerge2_1(T1, H2, Fun, T2, []) | Acc], Fun);
rufmergel([L], Acc, Fun) ->
    ufmergel([lists:reverse(L, []) | Acc], [], Fun);
rufmergel([], Acc, Fun) ->
    ufmergel(Acc, [], Fun).

%% umerge/3

%% Elements from the first list are kept and prioritized.
ufmerge2_1([H1 | T1], H2, Fun, T2, M, HdM) ->
    case Fun(H1, H2) of
	true ->
	    ufmerge2_1(T1, H2, Fun, T2, [H1 | M], H1);
	false when H2 == HdM ->
	    ufmerge2_2(H1, T1, Fun, T2, M);
	false ->
	    ufmerge2_2(H1, T1, Fun, T2, [H2 | M])
    end;
ufmerge2_1([], H2, _Fun, T2, M, HdM) when H2 == HdM ->
    lists:reverse(T2, M);
ufmerge2_1([], H2, _Fun, T2, M, _HdM) ->
    lists:reverse(T2, [H2 | M]).

ufmerge2_2(H1, T1, Fun, [H2 | T2], M) ->
    case Fun(H1, H2) of
	true ->
	    ufmerge2_1(T1, H2, Fun, T2, [H1 | M], H1);
	false ->
	    ufmerge2_2(H1, T1, Fun, T2, [H2 | M])
    end;
ufmerge2_2(H1, T1, _Fun, [], M) ->
    lists:reverse(T1, [H1 | M]).

%% rumerge/3

rufmerge2_1([H1 | T1], H2, Fun, T2, M) ->
    case Fun(H1, H2) of
	true ->
	    rufmerge2_2(H1, T1, Fun, T2, M, H2);
	false ->
	    rufmerge2_1(T1, H2, Fun, T2, [H1 | M])
    end;
rufmerge2_1([], H2, _Fun, T2, M) ->
    lists:reverse(T2, [H2 | M]).

rufmerge2_2(H1, T1, Fun, [H2 | T2], M, H2M) ->
    case Fun(H1, H2) of
	true ->
	    rufmerge2_2(H1, T1, Fun, T2, [H2M | M], H2);
	false when H2M == H1 ->
	    rufmerge2_1(T1, H2, Fun, T2, [H1 | M]);
	false ->
	    rufmerge2_1(T1, H2, Fun, T2, [H1, H2M | M])
    end;
rufmerge2_2(H1, T1, _Fun, [], M, H2M) when H2M == H1 ->
    lists:reverse(T1, [H1 | M]);
rufmerge2_2(H1, T1, _Fun, [], M, H2M) ->
    lists:reverse(T1, [H1, H2M | M]).
