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
-module(lists_sort).

%% These functions are to be used from lists.erl only.

-export([split_1/5, split_2/5, mergel/2, merge3_1/6, rmerge3_1/6,
         merge2_1/4, rmerge2_1/4]).
         
-export([usplit_1/5, usplit_2/5, umergel/1, umerge3_1/7, rumerge3_1/6,
         umerge2_2/4, rumerge2_1/4]).
         
-export([keysplit_1/8, keysplit_2/8, keymerge2_1/6, rkeymerge2_1/6]).

-export([ukeysplit_1/8, ukeysplit_2/5, ukeymerge2_2/6, rukeymerge2_1/6]).

-export([fsplit_1/6, fsplit_2/6, fmerge2_1/5, rfmerge2_1/5]).

-export([ufsplit_1/6, ufsplit_2/4, ufmerge2_2/5, rufmerge2_1/5]).

-compile({inline, 
          [{merge3_12,7}, {merge3_21,7}, {rmerge3_12,7}, {rmerge3_21,7}]}).

-compile({inline, 
          [{umerge3_12,8}, {umerge3_21,8},
	   {rumerge3_12a,7}, {rumerge3_12b,8}]}).

-compile({inline, 
          [{keymerge3_12,12}, {keymerge3_21,12}, 
           {rkeymerge3_12,12}, {rkeymerge3_21,12}]}).

-compile({inline,
          [{ukeymerge3_12,13}, {ukeymerge3_21,13},
	   {rukeymerge3_12a,11}, {rukeymerge3_21a,13},
	   {rukeymerge3_12b,12}, {rukeymerge3_21b,12}]}).

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
    mergel(L, [merge3_1(T1, [], H2, T2, H3, T3) | Acc]);
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
    merge2_2(T2, H3, T3, M, H2).

%% Take L2 apart.
merge3_2(T1, H1, M, [H2 | T2], H3, T3) when H1 =< H2 ->
    merge3_12(T1, H1, H2, T2, H3, T3, M);
merge3_2(T1, H1, M, [H2 | T2], H3, T3) ->
    merge3_21(T1, H1, H2, T2, H3, T3, M);
merge3_2(T1, H1, M, [], H3, T3) when H1 =< H3 ->
    merge2_1(T1, H3, T3, [H1 | M]);
merge3_2(T1, H1, M, [], H3, T3) ->
    merge2_2(T1, H3, T3, M, H1).

% H1 =< H2. Inlined.
merge3_12(T1, H1, H2, T2, H3, T3, M) when H1 =< H3 ->
    merge3_1(T1, [H1 | M], H2, T2, H3, T3);
merge3_12(T1, H1, H2, T2, H3, T3, M) ->
    merge3_12_3(T1, H1, H2, T2, [H3 | M], T3).

% H1 =< H2, take L3 apart.
merge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) when H1 =< H3 ->
    merge3_1(T1, [H1 | M], H2, T2, H3, T3);
merge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    merge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
merge3_12_3(T1, H1, H2, T2, M, []) ->
    merge2_1(T1, H2, T2, [H1 | M]).

% H1 > H2. Inlined.
merge3_21(T1, H1, H2, T2, H3, T3, M) when H2 =< H3 ->
    merge3_2(T1, H1, [H2 | M], T2, H3, T3);
merge3_21(T1, H1, H2, T2, H3, T3, M) ->
    merge3_21_3(T1, H1, H2, T2, [H3 | M], T3).

% H1 > H2, take L3 apart.
merge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) when H2 =< H3 ->
    merge3_2(T1, H1, [H2 | M], T2, H3, T3);
merge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    merge3_21_3(T1, H1, H2, T2, [H3 | M], T3);
merge3_21_3(T1, H1, H2, T2, M, []) ->
    merge2_2(T1, H2, T2, M, H1).

%% rmerge/3

%% Take L1 apart.
rmerge3_1([H1 | T1], M, H2, T2, H3, T3) when H1 =< H2 ->
    rmerge3_12(T1, H1, H2, T2, H3, T3, M);
rmerge3_1([H1 | T1], M, H2, T2, H3, T3) ->
    rmerge3_21(T1, H1, H2, T2, H3, T3, M);
rmerge3_1([], M, H2, T2, H3, T3) when H2 =< H3 ->
    rmerge2_2(T2, H3, T3, M, H2);
rmerge3_1([], M, H2, T2, H3, T3) ->
    rmerge2_1(T2, H3, T3, [H2 | M]).

%% Take L2 apart.
rmerge3_2(T1, H1, M, [H2 | T2], H3, T3) when H1 =< H2 ->
    rmerge3_12(T1, H1, H2, T2, H3, T3, M);
rmerge3_2(T1, H1, M, [H2 | T2], H3, T3) ->
    rmerge3_21(T1, H1, H2, T2, H3, T3, M);
rmerge3_2(T1, H1, M, [], H3, T3) when H1 =< H3 ->
    rmerge2_2(T1, H3, T3, M, H1);
rmerge3_2(T1, H1, M, [], H3, T3) ->
    rmerge2_1(T1, H3, T3, [H1 | M]).

% H1 =< H2. Inlined.
rmerge3_12(T1, H1, H2, T2, H3, T3, M) when H2 =< H3 ->
    rmerge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
rmerge3_12(T1, H1, H2, T2, H3, T3, M) ->
    rmerge3_2(T1, H1, [H2 | M], T2, H3, T3).

% H1 =< H2, take L3 apart.
rmerge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) when H2 =< H3 ->
    rmerge3_12_3(T1, H1, H2, T2, [H3 | M], T3);
rmerge3_12_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    rmerge3_2(T1, H1, [H2 | M], T2, H3, T3);
rmerge3_12_3(T1, H1, H2, T2, M, []) ->
    rmerge2_2(T1, H2, T2, M, H1).

% H1 > H2. Inlined.
rmerge3_21(T1, H1, H2, T2, H3, T3, M) when H1 =< H3 ->
    rmerge3_21_3(T1, H1, H2, T2, [H3 | M], T3);
rmerge3_21(T1, H1, H2, T2, H3, T3, M) ->
    rmerge3_1(T1, [H1 | M], H2, T2, H3, T3).

% H1 > H2, take L3 apart.
rmerge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) when H1 =< H3 ->
    rmerge3_21_3(T1, H1, H2, T2, [H3 | M], T3);
rmerge3_21_3(T1, H1, H2, T2, M, [H3 | T3]) ->
    rmerge3_1(T1, [H1 | M], H2, T2, H3, T3);
rmerge3_21_3(T1, H1, H2, T2, M, []) ->
    rmerge2_1(T1, H2, T2, [H1 | M]).

%% merge/2

merge2_1([H1 | T1], H2, T2, M) when H1 =< H2 ->
    merge2_1(T1, H2, T2, [H1 | M]);
merge2_1([H1 | T1], H2, T2, M) ->
    merge2_2(T1, H2, T2, M, H1);
merge2_1([], H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

merge2_2(T1, HdM, [H2 | T2], M, H1) when H1 =< H2 ->
    merge2_1(T1, H2, T2, [H1, HdM | M]);
merge2_2(T1, HdM, [H2 | T2], M, H1) ->
    merge2_2(T1, H2, T2, [HdM | M], H1);
merge2_2(T1, HdM, [], M, H1) ->
    lists:reverse(T1, [H1, HdM | M]).

%% rmerge/2

rmerge2_1([H1 | T1], H2, T2, M) when H1 =< H2 ->
    rmerge2_2(T1, H2, T2, M, H1);
rmerge2_1([H1 | T1], H2, T2, M) ->
    rmerge2_1(T1, H2, T2, [H1 | M]);
rmerge2_1([], H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

rmerge2_2(T1, HdM, [H2 | T2], M, H1) when H1 =< H2 ->
    rmerge2_2(T1, H2, T2, [HdM | M], H1);
rmerge2_2(T1, HdM, [H2 | T2], M, H1) ->
    rmerge2_1(T1, H2, T2, [H1, HdM | M]);
rmerge2_2(T1, HdM, [], M, H1) ->
    lists:reverse(T1, [H1, HdM | M]).

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

usplit_1_1(X, Y, [Z | L], R, Rs, S) when Z > Y ->
    usplit_1_1(Y, Z, L, [X | R], Rs, S);
usplit_1_1(X, Y, [Z | L], R, Rs, S) when Z == Y ->
    usplit_1_1(X, Y, L, R, Rs, S);
usplit_1_1(X, Y, [Z | L], R, Rs, S) when Z > X ->
    usplit_1_1(Z, Y, L, [X | R], Rs, S);
usplit_1_1(X, Y, [Z | L], R, Rs, S) when Z == X ->
    usplit_1_1(X, Y, L, R, Rs, S);
usplit_1_1(X, Y, [Z | L], R, Rs, S) when Z > S ->
    usplit_1(S, Z, L, [], [[Y, X | R] | Rs]);
usplit_1_1(X, Y, [Z | L], R, Rs, S) when Z == S ->
    usplit_1_1(X, Y, L, R, Rs, S);
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
usplit_2_1(X, Y, [Z | L], R, Rs, S) when Z == X ->
    usplit_2_1(X, Y, L, R, Rs, S);
usplit_2_1(X, Y, [Z | L], R, Rs, S) when Z < S ->
    usplit_2(S, Z, L, [], [[Y, X | R] | Rs]);
usplit_2_1(X, Y, [Z | L], R, Rs, S) when Z == S ->
    usplit_2_1(X, Y, L, R, Rs, S);
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
    umergel(L, [umerge3_1(T1, [H2 | H3], T2, H2, [], T3, H3) | Acc], asc);
umergel([[H3 | T3], [H2 | T2], T1 | L], Acc, desc) ->
    umergel(L, [umerge3_1(T1, [H2 | H3], T2, H2, [], T3, H3) | Acc], desc);
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
    rumergel(L, [rumerge3_1(T1, T2, H2, [], T3, H3) | Acc], asc);
rumergel([T1, [H2 | T2], [H3 | T3] | L], Acc, desc) ->
    rumergel(L, [rumerge3_1(T1, T2, H2, [], T3, H3) | Acc], desc);
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
umerge3_1([H1 | T1], HdM, T2, H2, M, T3, H3) when H1 =< H2 ->
    umerge3_12(T1, H1, T2, H2, M, T3, H3, HdM);
umerge3_1([H1 | T1], HdM, T2, H2, M, T3, H3) when H2 == HdM ->
    umerge3_2(T1, H1, T2, H2, M, T3, H3);
umerge3_1([H1 | T1], HdM, T2, H2, M, T3, H3) ->
    umerge3_21(T1, H1, T2, H2, M, T3, H3, HdM);
umerge3_1([], HdM, T2, H2, M, T3, H3) when H2 == HdM ->
    umerge2_1(T2, T3, M, HdM, H3);
umerge3_1([], _HdM, T2, H2, M, T3, H3) when H2 =< H3 ->
    umerge2_1(T2, T3, [H2 | M], H2, H3);
umerge3_1([], HdM, T2, H2, M, T3, H3) when H3 == HdM ->
    umerge2_2(T2, T3, M, H2);
umerge3_1([], _HdM, T2, H2, M, T3, H3) ->
    umerge2_2(T2, T3, [H3 | M], H2).

%% Take L2 apart.
umerge3_2(T1, H1, [H2 | T2], HdM, M, T3, H3) when H1 =< H2 ->
    umerge3_12(T1, H1, T2, H2, M, T3, H3, HdM);
umerge3_2(T1, H1, [H2 | T2], HdM, M, T3, H3) ->
    umerge3_21(T1, H1, T2, H2, M, T3, H3, HdM);
umerge3_2(T1, H1, [], _HdM, M, T3, H3) when H1 =< H3 ->
    umerge2_1(T1, T3, [H1 | M], H1, H3);
umerge3_2(T1, H1, [], HdM, M, T3, H3) when H3 == HdM ->
    umerge2_2(T1, T3, M, H1);
umerge3_2(T1, H1, [], _HdM, M, T3, H3) ->
    umerge2_2(T1, T3, [H3 | M], H1).

% H1 =< H2. Inlined.
umerge3_12(T1, H1, T2, H2, M, T3, H3, _HdM) when H1 =< H3 ->
    umerge3_1(T1, H1, T2, H2, [H1 | M], T3, H3);
umerge3_12(T1, H1, T2, H2, M, T3, H3, HdM) when H3 == HdM ->
    umerge3_12_3(T1, H1, T2, H2, M, T3);
umerge3_12(T1, H1, T2, H2, M, T3, H3, _HdM) ->
    umerge3_12_3(T1, H1, T2, H2, [H3 | M], T3).

% H1 =< H2, take L3 apart.
umerge3_12_3(T1, H1, T2, H2, M, [H3 | T3]) when H1 =< H3 ->
    umerge3_1(T1, H1, T2, H2, [H1 | M], T3, H3);
umerge3_12_3(T1, H1, T2, H2, M, [H3 | T3]) ->
    umerge3_12_3(T1, H1, T2, H2, [H3 | M], T3);
umerge3_12_3(T1, H1, T2, H2, M, []) ->
    umerge2_1(T1, T2, [H1 | M], H1, H2).

% H1 > H2. Inlined.
umerge3_21(T1, H1, T2, H2, M, T3, H3, _HdM) when H2 =< H3 ->
    umerge3_2(T1, H1, T2, H2, [H2 | M], T3, H3);
umerge3_21(T1, H1, T2, H2, M, T3, H3, HdM) when H3 == HdM ->
    umerge3_21_3(T1, H1, T2, H2, M, T3);
umerge3_21(T1, H1, T2, H2, M, T3, H3, _HdM) ->
    umerge3_21_3(T1, H1, T2, H2, [H3 | M], T3).

% H1 > H2, take L3 apart.
umerge3_21_3(T1, H1, T2, H2, M, [H3 | T3]) when H2 =< H3 ->
    umerge3_2(T1, H1, T2, H2, [H2 | M], T3, H3);
umerge3_21_3(T1, H1, T2, H2, M, [H3 | T3]) ->
    umerge3_21_3(T1, H1, T2, H2, [H3 | M], T3);
umerge3_21_3(T1, H1, T2, H2, M, []) ->
    umerge2_2(T1, T2, [H2 | M], H1).

%% Take L1 apart.
rumerge3_1([H1 | T1], T2, H2, M, T3, H3) when H1 =< H2 ->
    rumerge3_12a(T1, H1, T2, H2, M, T3, H3);
rumerge3_1([H1 | T1], T2, H2, M, T3, H3) when H1 =< H3 ->
    rumerge3_21_3(T1, T2, H2, M, T3, H3, H1);
rumerge3_1([H1 | T1], T2, H2, M, T3, H3) ->
    rumerge3_1(T1, T2, H2, [H1 | M], T3, H3);
rumerge3_1([], T2, H2, M, T3, H3) when H2 =< H3 ->
    rumerge2_2(T2, T3, M, H3, H2);
rumerge3_1([], T2, H2, M, T3, H3) ->
    rumerge2_1(T2, T3, [H2 | M], H3).

% H1 =< H2. Inlined.
rumerge3_12a(T1, H1, T2, H2, M, T3, H3) when H2 =< H3 ->
    rumerge3_12_3(T1, T2, H2, M, T3, H3, H1);
rumerge3_12a(T1, H1, T2, H2, M, T3, H3) ->
    rumerge3_2(T1, T2, H2, M, T3, H3, H1).

%% Take L2 apart. H2M > H3. H2M > H2.
rumerge3_2(T1, [H2 | T2], H2M, M, T3, H3, H1) when H1 =< H2 ->
    % H2M > H1.
    rumerge3_12b(T1, H1, T2, H2, M, T3, H3, H2M);
rumerge3_2(T1, [H2 | T2], H2M, M, T3, H3, H1) when H1 == H2M ->
    rumerge3_1(T1, T2, H2, [H1 | M], T3, H3);
rumerge3_2(T1, [H2 | T2], H2M, M, T3, H3, H1) when H1 =< H3 ->
    % H2M > H1.
    rumerge3_21_3(T1, T2, H2, [H2M | M], T3, H3, H1);
rumerge3_2(T1, [H2 | T2], H2M, M, T3, H3, H1) ->
    % H2M > H1.
    rumerge3_1(T1, T2, H2, [H1, H2M | M], T3, H3);
rumerge3_2(T1, [], H2M, M, T3, H3, H1) when H1 == H2M ->
    rumerge2_1(T1, T3, [H1 | M], H3);
rumerge3_2(T1, [], H2M, M, T3, H3, H1) when H1 =< H3 ->
    rumerge2_2(T1, T3, [H2M | M], H3, H1);
rumerge3_2(T1, [], H2M, M, T3, H3, H1) ->
    rumerge2_1(T1, T3, [H1, H2M | M], H3).

% H1 =< H2. Inlined.
rumerge3_12b(T1, H1, T2, H2, M, T3, H3, H2M) when H2 =< H3 ->
    rumerge3_12_3(T1, T2, H2, [H2M | M], T3, H3, H1);
rumerge3_12b(T1, H1, T2, H2, M, T3, H3, H2M) ->
    rumerge3_2(T1, T2, H2, [H2M | M], T3, H3, H1).

% H1 =< H2, take L3 apart.
rumerge3_12_3(T1, T2, H2, M, [H3 | T3], H3M, H1) when H2 =< H3 ->
    rumerge3_12_3(T1, T2, H2, [H3M | M], T3, H3, H1);
rumerge3_12_3(T1, T2, H2, M, [H3 | T3], H3M, H1) when H2 == H3M ->
    rumerge3_2(T1, T2, H2, M, T3, H3, H1);
rumerge3_12_3(T1, T2, H2, M, [H3 | T3], H3M, H1) ->
    rumerge3_2(T1, T2, H2, [H3M | M], T3, H3, H1);
rumerge3_12_3(T1, T2, H2, M, [], H3M, H1) when H2 == H3M ->
    rumerge2_2(T1, T2, M, H2, H1);
rumerge3_12_3(T1, T2, H2, M, [], H3M, H1) ->
    rumerge2_2(T1, T2, [H3M | M], H2, H1).

% H1 > H2, take L3 apart.
rumerge3_21_3(T1, T2, H2, M, [H3 | T3], H3M, H1) when H1 =< H3 ->
    rumerge3_21_3(T1, T2, H2, [H3M | M], T3, H3, H1);
rumerge3_21_3(T1, T2, H2, M, [H3 | T3], H3M, H1) when H1 == H3M ->
    rumerge3_1(T1, T2, H2, [H1 | M], T3, H3);
rumerge3_21_3(T1, T2, H2, M, [H3 | T3], H3M, H1) ->
    rumerge3_1(T1, T2, H2, [H1, H3M | M], T3, H3);
rumerge3_21_3(T1, T2, H2, M, [], H3M, H1) when H1 == H3M ->
    rumerge2_1(T1, T2, [H1 | M], H2);
rumerge3_21_3(T1, T2, H2, M, [], H3M, H1) ->
    rumerge2_1(T1, T2, [H1, H3M | M], H2).

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

% H1 =< H2M.
rumerge2_2(T1, [H2 | T2], M, H2M, H1) when H1 =< H2 ->
    rumerge2_2(T1, T2, [H2M | M], H2, H1);
rumerge2_2(T1, [H2 | T2], M, H2M, H1) when H1 == H2M ->
    rumerge2_1(T1, T2, [H1 | M], H2);
rumerge2_2(T1, [H2 | T2], M, H2M, H1) ->
    rumerge2_1(T1, T2, [H1, H2M | M], H2);
rumerge2_2(T1, [], M, H2M, H1) when H1 == H2M ->
    lists:reverse(T1, [H1 | M]);
rumerge2_2(T1, [], M, H2M, H1) ->
    lists:reverse(T1, [H1, H2M | M]).

%% keysort/2

%% Ascending.
keysplit_1(I, X, EX, Y, EY, [Z | L], R, Rs) ->
    case element(I, Z) of
	EZ when EY =< EZ ->
            keysplit_1(I, Y, EY, Z, EZ, L, [X | R], Rs);
        EZ when EX =< EZ ->
            keysplit_1(I, Z, EZ, Y, EY, L, [X | R], Rs);
        _EZ when R == [] ->
            keysplit_1(I, X, EX, Y, EY, L, [Z], Rs);
        EZ ->
            keysplit_1_1(I, X, EX, Y, EY, EZ, R, Rs, Z, L)
    end;
keysplit_1(I, X, _EX, Y, _EY, [], R, Rs) ->
    rkeymergel(I, [[Y, X | R] | Rs], [], asc).

keysplit_1_1(I, X, EX, Y, EY, ES, R, Rs, S, [Z | L]) ->
    case element(I, Z) of
	EZ when EY =< EZ ->
            keysplit_1_1(I, Y, EY, Z, EZ, ES, [X | R], Rs, S, L);
        EZ when EX =< EZ ->
            keysplit_1_1(I, Z, EZ, Y, EY, ES, [X | R], Rs, S, L);
        EZ when ES =< EZ ->
            keysplit_1(I, S, ES, Z, EZ, L, [], [[Y, X | R] | Rs]);
        EZ ->
            keysplit_1(I, Z, EZ, S, ES, L, [], [[Y, X | R] | Rs])
    end;
keysplit_1_1(I, X, _EX, Y, _EY, _ES, R, Rs, S, []) ->
    rkeymergel(I, [[S], [Y, X | R] | Rs], [], asc).

%% Descending.
keysplit_2(I, X, EX, Y, EY, [Z | L], R, Rs) ->
    case element(I, Z) of
	EZ when EY > EZ ->
            keysplit_2(I, Y, EY, Z, EZ, L, [X | R], Rs);
        EZ when EX > EZ ->
            keysplit_2(I, Z, EZ, Y, EY, L, [X | R], Rs);
        _EZ when R == [] ->
            keysplit_2(I, X, EX, Y, EY, L, [Z], Rs);
        EZ ->
            keysplit_2_1(I, X, EX, Y, EY, EZ, R, Rs, Z, L)
    end;
keysplit_2(I, X, _EX, Y, _EY, [], R, Rs) ->
    keymergel(I, [[Y, X | R] | Rs], [], desc).

keysplit_2_1(I, X, EX, Y, EY, ES, R, Rs, S, [Z | L]) ->
    case element(I, Z) of
        EZ when EY > EZ ->
            keysplit_2_1(I, Y, EY, Z, EZ, ES, [X | R], Rs, S, L);
        EZ when EX > EZ ->
            keysplit_2_1(I, Z, EZ, Y, EY, ES, [X | R], Rs, S, L);
        EZ when ES > EZ ->
            keysplit_2(I, S, ES, Z, EZ, L, [], [[Y, X | R] | Rs]);
        EZ ->
            keysplit_2(I, Z, EZ, S, ES, L, [], [[Y, X | R] | Rs])
    end;
keysplit_2_1(I, X, _EX, Y, _EY, _ES, R, Rs, S, []) ->
    keymergel(I, [[S], [Y, X | R] | Rs], [], desc).

keymergel(I, [T1, [H2 | T2], [H3 | T3] | L], Acc, O) when O == asc ->
    M = keymerge3_1(I, T1, [],O,element(I,H2), H2, T2, element(I,H3), H3, T3),
    keymergel(I, L, [M | Acc], O);
keymergel(I, [[H3 | T3], [H2 | T2], T1 | L], Acc, O) when O == desc ->
    M = keymerge3_1(I, T1, [],O,element(I,H2), H2, T2, element(I,H3), H3, T3),
    keymergel(I, L, [M | Acc], O);
keymergel(I, [T1, [H2 | T2] | L], Acc, asc) ->
    keymergel(I, L, [keymerge2_1(I, T1, element(I,H2),H2,T2,[]) | Acc], asc);
keymergel(I, [[H2 | T2], T1 | L], Acc, desc) ->
    keymergel(I, L, [keymerge2_1(I, T1, element(I,H2),H2,T2,[]) | Acc], desc);
keymergel(_I, [L], [], _O) ->
    L;
keymergel(I, [L], Acc, O) ->
    rkeymergel(I, [lists:reverse(L, []) | Acc], [], O);
keymergel(I, [], Acc, O) ->
    rkeymergel(I, Acc, [], O).

rkeymergel(I, [[H3 | T3], [H2 | T2], T1 | L], Acc, O) when O == asc ->
    M = rkeymerge3_1(I, T1, [],O,element(I,H2), H2, T2, element(I,H3), H3,T3),
    rkeymergel(I, L, [M | Acc], O);
rkeymergel(I, [T1, [H2 | T2], [H3 | T3] | L], Acc, O) when O == desc ->
    M = rkeymerge3_1(I, T1, [],O,element(I,H2), H2, T2, element(I,H3), H3,T3),
    rkeymergel(I, L, [M | Acc], O);
rkeymergel(I, [[H2 | T2], T1 | L], Acc, asc) ->
    rkeymergel(I, L, [rkeymerge2_1(I, T1, element(I,H2),H2,T2,[]) | Acc],asc);
rkeymergel(I, [T1, [H2 | T2] | L], Acc, desc) ->
    rkeymergel(I, L, [rkeymerge2_1(I,T1, element(I,H2),H2,T2,[]) | Acc],desc);
rkeymergel(I, [L], Acc, O) ->
    keymergel(I, [lists:reverse(L, []) | Acc], [], O);
rkeymergel(I, [], Acc, O) ->
    keymergel(I, Acc, [], O).

%%% An extra argument, D, just to avoid some move instructions.

%% Take L1 apart.
keymerge3_1(I, [H1 | T1], M, D, E2, H2, T2, E3, H3, T3) ->
    case element(I, H1) of
	E1 when E1 =< E2 ->
            keymerge3_12(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, D);
        E1 ->
            keymerge3_21(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, T2)
    end;
keymerge3_1(I, [], M, _D, E2, H2, T2, E3, H3, T3) when E2 =< E3 ->
    keymerge2_1(I, T2, E3, H3, T3, [H2 | M]);
keymerge3_1(I, [], M, _D, E2, H2, T2, _E3, H3, T3) ->
    keymerge2_2(I, T2, E2, H3, T3, M, H2).

%% Take L2 apart.
keymerge3_2(I, E1, H1, T1, [H2 | T2], M, D, E3, H3, T3) ->
    case element(I, H2) of
	E2 when E1 =< E2 ->
            keymerge3_12(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, T1);
        E2 ->
            keymerge3_21(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, D)
    end;
keymerge3_2(I, E1, H1, T1, [], M, _D, E3, H3, T3) when E1 =< E3 ->
    keymerge2_1(I, T1, E3, H3, T3, [H1 | M]);
keymerge3_2(I, E1, H1, T1, [], M, _D, _E3, H3, T3) ->
    keymerge2_2(I, T1, E1, H3, T3, M, H1).

% E1 =< E2. Inlined.
keymerge3_12(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, D) when E1 =< E3 ->
    keymerge3_1(I, T1, [H1 | M], D, E2, H2, T2, E3, H3, T3);
keymerge3_12(I, E1, H1, T1, E2, H2, T2, _E3, H3, T3, M, _D) ->
    keymerge3_12_3(I, E1, H1, T1, E2, H2, T2, T3, [H3 | M]).

% E1 =< E2, take L3 apart.
keymerge3_12_3(I, E1, H1, T1, E2, H2, T2, [H3 | T3], M) ->
    case element(I, H3) of
	E3 when E1 =< E3 ->
            keymerge3_1(I, T1, [H1 | M], T1, E2, H2, T2, E3, H3, T3);
        _E3 ->
            keymerge3_12_3(I, E1, H1, T1, E2, H2, T2, T3, [H3 | M])
    end;
keymerge3_12_3(I, _E1, H1, T1, E2, H2, T2, [], M) ->
    keymerge2_1(I, T1, E2, H2, T2, [H1 | M]).

% E1 > E2. Inlined.
keymerge3_21(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, D) when E2 =< E3 ->
    keymerge3_2(I, E1, H1, T1, T2, [H2 | M], D, E3, H3, T3);
keymerge3_21(I, E1, H1, T1, E2, H2, T2, _E3, H3, T3, M, _D) ->
    keymerge3_21_3(I, E1, H1, T1, E2, H2, T2, T3, [H3 | M]).

% E1 > E2, take L3 apart.
keymerge3_21_3(I, E1, H1, T1, E2, H2, T2, [H3 | T3], M) ->
    case element(I, H3) of
	E3 when E2 =< E3 ->
            keymerge3_2(I, E1, H1, T1, T2, [H2 | M], T2, E3, H3, T3);
        _E3 ->
            keymerge3_21_3(I, E1, H1, T1, E2, H2, T2, T3, [H3 | M])
    end;
keymerge3_21_3(I, E1, H1, T1, _E2, H2, T2, [], M) ->
    keymerge2_2(I, T1, E1, H2, T2, M, H1).

%% Take L1 apart.
rkeymerge3_1(I, [H1 | T1], M, D, E2, H2, T2, E3, H3, T3) ->
    case element(I, H1) of
	E1 when E1 =< E2 ->
            rkeymerge3_12(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, T2);
        E1 ->
            rkeymerge3_21(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, D)
    end;
rkeymerge3_1(I, [], M, _D, E2, H2, T2, E3, H3, T3) when E2 =< E3 ->
    rkeymerge2_2(I, E2, T2, H3, T3, M, H2);
rkeymerge3_1(I, [], M, _D, _E2, H2, T2, E3, H3, T3) ->
    rkeymerge2_1(I, T2, E3, H3, T3, [H2 | M]).

%% Take L2 apart.
rkeymerge3_2(I, E1, H1, T1, [H2 | T2], M, D, E3, H3, T3) ->
    case element(I, H2) of
	E2 when E1 =< E2 ->
            rkeymerge3_12(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, D);
        E2 ->
            rkeymerge3_21(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, T1)
    end;
rkeymerge3_2(I, E1, H1, T1, [], M, _D, E3, H3, T3) when E1 =< E3 ->
    rkeymerge2_2(I, E1, T1, H3, T3, M, H1);
rkeymerge3_2(I, _E1, H1, T1, [], M, _D, E3, H3, T3) ->
    rkeymerge2_1(I, T1, E3, H3, T3, [H1 | M]).

% E1 =< E2. Inlined.
rkeymerge3_12(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, _D) when E2 =< E3 ->
    rkeymerge3_12_3(I, E1, H1, T1, E2, H2, T2, T3, [H3 | M]);
rkeymerge3_12(I, E1, H1, T1, _E2, H2, T2, E3, H3, T3, M, D) ->
    rkeymerge3_2(I, E1, H1, T1, T2, [H2 | M], D, E3, H3, T3).

% E1 =< E2, take L3 apart.
rkeymerge3_12_3(I, E1, H1, T1, E2, H2, T2, [H3 | T3], M) ->
    case element(I, H3) of
	E3 when E2 =< E3 ->
            rkeymerge3_12_3(I, E1, H1, T1, E2, H2, T2, T3, [H3 | M]);
        E3 ->
            rkeymerge3_2(I, E1, H1, T1, T2, [H2 | M], T2, E3, H3, T3)
    end;
rkeymerge3_12_3(I, E1, H1, T1, _E2, H2, T2, [], M) ->
    rkeymerge2_2(I, E1, T1, H2, T2, M, H1).

% E1 > E2. Inlined.
rkeymerge3_21(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, _D) when E1 =< E3 ->
    rkeymerge3_21_3(I, E1, H1, T1, E2, H2, T2, T3, [H3 | M]);
rkeymerge3_21(I, _E1, H1, T1, E2, H2, T2, E3, H3, T3, M, D) ->
    rkeymerge3_1(I, T1, [H1 | M], D, E2, H2, T2, E3, H3, T3).

% E1 > E2, take L3 apart.
rkeymerge3_21_3(I, E1, H1, T1, E2, H2, T2, [H3 | T3], M) ->
    case element(I, H3) of
	E3 when E1 =< E3 ->
            rkeymerge3_21_3(I, E1, H1, T1, E2, H2, T2, T3, [H3 | M]);
        E3 ->
            rkeymerge3_1(I, T1, [H1 | M], T1, E2, H2, T2, E3, H3, T3)
    end;
rkeymerge3_21_3(I, _E1, H1, T1, E2, H2, T2, [], M) ->
    rkeymerge2_1(I, T1, E2, H2, T2, [H1 | M]).

%% keymerge/3

%% Elements from the first list are prioritized.
keymerge2_1(I, [H1 | T1], E2, H2, T2, M) ->
    case element(I, H1) of
	E1 when E1 =< E2 ->
            keymerge2_1(I, T1, E2, H2, T2, [H1 | M]);
        E1 ->
            keymerge2_2(I, T1, E1, H2, T2, M, H1)
    end;
keymerge2_1(_I, [], _E2, H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

keymerge2_2(I, T1, E1, HdM, [H2 | T2], M, H1) ->
    case element(I, H2) of
	E2 when E1 =< E2 ->
            keymerge2_1(I, T1, E2, H2, T2, [H1, HdM | M]);
        _E2 ->
            keymerge2_2(I, T1, E1, H2, T2, [HdM | M], H1)
    end;
keymerge2_2(_I, T1, _E1, HdM, [], M, H1) ->
    lists:reverse(T1, [H1, HdM | M]).

%% rkeymerge/3

rkeymerge2_1(I, [H1 | T1], E2, H2, T2, M) ->
    case element(I, H1) of
	E1 when E1 =< E2 ->
            rkeymerge2_2(I, E1, T1, H2, T2, M, H1);
        _E1 ->
            rkeymerge2_1(I, T1, E2, H2, T2, [H1 | M])
    end;
rkeymerge2_1(_I, [], _E2, H2, T2, M) ->
    lists:reverse(T2, [H2 | M]).

rkeymerge2_2(I, E1, T1, HdM, [H2 | T2], M, H1) ->
    case element(I, H2) of
	E2 when E1 =< E2 ->
            rkeymerge2_2(I, E1, T1, H2, T2, [HdM | M], H1);
        E2 ->
            rkeymerge2_1(I, T1, E2, H2, T2, [H1, HdM | M])
    end;
rkeymerge2_2(_I, _E1, T1, HdM, [], M, H1) ->
    lists:reverse(T1, [H1, HdM | M]).

%% ukeysort/2

%% Ascending.
ukeysplit_1(I, X, EX, Y, EY, [Z | L], R, Rs) ->
    case element(I, Z) of
        EZ when EY == EZ ->
            ukeysplit_1(I, X, EX, Y, EY, L, R, Rs);
	EZ when EY < EZ ->
            ukeysplit_1(I, Y, EY, Z, EZ, L, [X | R], Rs);
        EZ when EX == EZ ->
            ukeysplit_1(I, X, EX, Y, EY, L, R, Rs);
        EZ when EX < EZ ->
            ukeysplit_1(I, Z, EZ, Y, EY, L, [X | R], Rs);
        _EZ when R == [] ->
            ukeysplit_1(I, X, EX, Y, EY, L, [Z], Rs);
        EZ ->
            ukeysplit_1_1(I, X, EX, Y, EY, L, R, Rs, Z, EZ)
    end;
ukeysplit_1(I, X, _EX, Y, _EY, [], R, Rs) ->
    rukeymergel(I, [[Y, X | R] | Rs], []).

ukeysplit_1_1(I, X, EX, Y, EY, [Z | L], R, Rs, S, ES) ->
    case element(I, Z) of
        EZ when EY == EZ ->
            ukeysplit_1_1(I, X, EX, Y, EY, L, R, Rs, S, ES);
        EZ when EY < EZ ->
            ukeysplit_1_1(I, Y, EY, Z, EZ, L, [X | R], Rs, S, ES);
        EZ when EX == EZ ->
            ukeysplit_1_1(I, X, EX, Y, EY, L, R, Rs, S, ES);
        EZ when EX < EZ ->
            ukeysplit_1_1(I, Z, EZ, Y, EY, L, [X | R], Rs, S, ES);
        EZ when ES == EZ ->
            ukeysplit_1_1(I, X, EX, Y, EY, L, R, Rs, S, ES);
        EZ when ES < EZ ->
            ukeysplit_1(I, S, ES, Z, EZ, L, [], [[Y, X | R] | Rs]);
        EZ ->
            ukeysplit_1(I, Z, EZ, S, ES, L, [], [[Y, X | R] | Rs])
    end;
ukeysplit_1_1(I, X, _EX, Y, _EY, [], R, Rs, S, _ES) ->
    rukeymergel(I, [[S], [Y, X | R] | Rs], []).

%% Descending.
ukeysplit_2(I, Y, EY, [Z | L], R) ->
    case element(I, Z) of
	EZ when EY == EZ ->
            ukeysplit_2(I, Y, EY, L, R);
	EZ when EY < EZ ->
            ukeysplit_1(I, Y, EY, Z, EZ, L, [], [lists:reverse(R, [])]);
        EZ ->
            ukeysplit_2(I, Z, EZ, L, [Y | R])
    end;
ukeysplit_2(_I, Y, _EY, [], R) ->
    [Y | R].

ukeymergel(I, [T1, [H2 | T2], [H3 | T3] | L], Acc) ->
    %% The fourth argument, [H2 | H3] (=HdM), may confuse type
    %% checkers. Its purpose is to ensure that the tests H2 == HdM
    %% and H3 == HdM in ukeymerge3_1 will always fail as long as M == [].
    M = ukeymerge3_1(I, T1, Acc, [H2 | H3], element(I, H2), H2, T2, [],
                     element(I, H3), H3, T3),
    ukeymergel(I, L, [M | Acc]);
ukeymergel(I, [[H1 | T1], T2 | L], Acc) ->
    ukeymergel(I, L, [ukeymerge2_2(I, T1, element(I, H1), H1, T2, []) | Acc]);
ukeymergel(_I, [L], []) ->
    L;
ukeymergel(I, [L], Acc) ->
    rukeymergel(I, [lists:reverse(L, []) | Acc], []);
ukeymergel(I, [], Acc) ->
    rukeymergel(I, Acc, []).

rukeymergel(I, [[H3 | T3], [H2 | T2], T1 | L], Acc) ->
    M = rukeymerge3_1(I, T1, Acc, [], element(I, H2), H2, T2, [],
                      element(I, H3), H3, T3),
    rukeymergel(I, L, [M | Acc]);
rukeymergel(I, [[H2 | T2], T1 | L], Acc) ->
    rukeymergel(I, L, [rukeymerge2_1(I, T1, element(I,H2), T2, [], H2)|Acc]);
rukeymergel(I, [L], Acc) ->
    ukeymergel(I, [lists:reverse(L, []) | Acc], []);
rukeymergel(I, [], Acc) ->
    ukeymergel(I, Acc, []).

%%% An extra argument, D, just to avoid some move instructions.

%% Take L1 apart.
ukeymerge3_1(I, [H1 | T1], D, HdM, E2, H2, T2, M, E3, H3, T3) ->
    case element(I, H1) of
	E1 when E1 =< E2 ->
            ukeymerge3_12(I, E1, T1, H1, E2, H2, T2, E3, H3, T3, M, HdM, D);
        E1 when E2 == HdM ->
            ukeymerge3_2(I, E1, T1, H1, T2, HdM, T2, M, E3, H3, T3);
        E1 ->
            ukeymerge3_21(I, E1, T1, H1, E2, H2, T2, E3, H3, T3, M, HdM, T2)
    end;
ukeymerge3_1(I, [], _D, HdM, E2, _H2, T2, M, E3, H3, T3) when E2 == HdM ->
    ukeymerge2_1(I, T2, E3, HdM, T3, M, H3);
ukeymerge3_1(I, [], _D, _HdM, E2, H2, T2, M, E3, H3, T3) when E2 =< E3 ->
    ukeymerge2_1(I, T2, E3, E2, T3, [H2 | M], H3);
ukeymerge3_1(I, [], _D, HdM, E2, H2, T2, M, E3, _H3, T3) when E3 == HdM ->
    ukeymerge2_2(I, T2, E2, H2, T3, M);
ukeymerge3_1(I, [], _D, _HdM, E2, H2, T2, M, _E3, H3, T3) ->
    ukeymerge2_2(I, T2, E2, H2, T3, [H3 | M]).

%% Take L2 apart.
ukeymerge3_2(I, E1, T1, H1, [H2 | T2], HdM, D, M, E3, H3, T3) ->
    case element(I, H2) of
	E2 when E1 =< E2 ->
            ukeymerge3_12(I, E1, T1, H1, E2, H2, T2, E3, H3, T3, M, HdM, T1);
        E2 ->
            ukeymerge3_21(I, E1, T1, H1, E2, H2, T2, E3, H3, T3, M, HdM, D)
    end;
ukeymerge3_2(I, E1, T1, H1, [], _HdM, _D, M, E3, H3, T3) when E1 =< E3 ->
    ukeymerge2_1(I, T1, E3, E1, T3, [H1 | M], H3);
ukeymerge3_2(I, E1, T1, H1, [], HdM, _D, M, E3, _H3, T3) when E3 == HdM ->
    ukeymerge2_2(I, T1, E1, H1, T3, M);
ukeymerge3_2(I, E1, T1, H1, [], _HdM, _D, M, _E3, H3, T3) ->
    ukeymerge2_2(I, T1, E1, H1, T3, [H3 | M]).

% E1 =< E2. Inlined.
ukeymerge3_12(I, E1, T1, H1, E2, H2, T2, E3, H3, T3, M, _HdM, D) 
                                                             when E1 =< E3 ->
    ukeymerge3_1(I, T1, D, E1, E2, H2, T2, [H1 | M], E3, H3, T3);
ukeymerge3_12(I, E1, T1, H1, E2, H2, T2, E3, _H3, T3, M, HdM, _D) 
                                                             when E3 == HdM ->
    ukeymerge3_12_3(I, E1, T1, H1, E2, H2, T2, M, T3);
ukeymerge3_12(I, E1, T1, H1, E2, H2, T2, _E3, H3, T3, M, _HdM, _D) ->
    ukeymerge3_12_3(I, E1, T1, H1, E2, H2, T2, [H3 | M], T3).

% E1 =< E2, take L3 apart.
ukeymerge3_12_3(I, E1, T1, H1, E2, H2, T2, M, [H3 | T3]) ->
    case element(I, H3) of
	E3 when E1 =< E3 ->
            ukeymerge3_1(I, T1, T1, E1, E2, H2, T2, [H1 | M], E3, H3, T3);
        _E3 ->
            ukeymerge3_12_3(I, E1, T1, H1, E2, H2, T2, [H3 | M], T3)
    end;
ukeymerge3_12_3(I, E1, T1, H1, E2, H2, T2, M, []) ->
    ukeymerge2_1(I, T1, E2, E1, T2, [H1 | M], H2).

% E1 > E2. Inlined.
ukeymerge3_21(I, E1, T1, H1, E2, H2, T2, E3, H3, T3, M, _HdM, D) 
                                                             when E2 =< E3 ->
    ukeymerge3_2(I, E1, T1, H1, T2, E2, D, [H2 | M], E3, H3, T3);
ukeymerge3_21(I, E1, T1, H1, E2, H2, T2, E3, _H3, T3, M, HdM, _D) 
                                                             when E3 == HdM ->
    ukeymerge3_21_3(I, E1, T1, H1, E2, H2, T2, M, T3);
ukeymerge3_21(I, E1, T1, H1, E2, H2, T2, _E3, H3, T3, M, _HdM, _D) ->
    ukeymerge3_21_3(I, E1, T1, H1, E2, H2, T2, [H3 | M], T3).

% E1 > E2, take L3 apart.
ukeymerge3_21_3(I, E1, T1, H1, E2, H2, T2, M, [H3 | T3]) ->
    case element(I, H3) of
        E3 when E2 =< E3 ->
            ukeymerge3_2(I, E1, T1, H1, T2, E2, T2, [H2 | M], E3, H3, T3);
        _E3 ->
            ukeymerge3_21_3(I, E1, T1, H1, E2, H2, T2, [H3 | M], T3)
    end;
ukeymerge3_21_3(I, E1, T1, H1, _E2, H2, T2, M, []) ->
    ukeymerge2_2(I, T1, E1, H1, T2, [H2 | M]).

%%% Two extra arguments, D1 and D2, just to avoid some move instructions.

%% Take L1 apart.
rukeymerge3_1(I, [H1 | T1], D1, D2, E2, H2, T2, M, E3, H3, T3) ->
    case element(I, H1) of
	E1 when E1 =< E2 ->
	    rukeymerge3_12a(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M);
        E1 ->
            rukeymerge3_21a(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, D1, D2)
    end;
rukeymerge3_1(I, [], _D1, _D2, E2, H2, T2, M, E3, H3, T3) when E2 =< E3 ->
    rukeymerge2_2(I, T2, E2, T3, M, E3, H3, H2);
rukeymerge3_1(I, [], _D1, _D2, _E2, H2, T2, M, E3, H3, T3) ->
    rukeymerge2_1(I, T2, E3, T3, [H2 | M], H3).

% E1 =< E2. Inlined.
rukeymerge3_12a(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M) when E2 =< E3 ->
    rukeymerge3_12_3(I, E1, H1, T1, E2, H2, T2, M, E3, H3, T3);
rukeymerge3_12a(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M) ->
    rukeymerge3_2(I, E1, H1, T1, T2, H2, E2, M, E3, H3, T3).

% E1 > E2. Inlined
rukeymerge3_21a(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, _D1, _D2) 
                                                              when E1 =< E3 ->
    rukeymerge3_21_3(I, E1, H1, T1, E2, H2, T2, M, E3, H3, T3);
rukeymerge3_21a(I, _E1, H1, T1, E2, H2, T2, E3, H3, T3, M, D1, D2) ->
    rukeymerge3_1(I, T1, D1, D2, E2, H2, T2, [H1 | M], E3, H3, T3).

%% Take L2 apart. E2M > E3. E2M > E2.
rukeymerge3_2(I, E1, H1, T1, [H2 | T2], H2M, E2M, M, E3, H3, T3) ->
    case element(I, H2) of
	E2 when E1 =< E2 ->
            % E2M > E1.
	    rukeymerge3_12b(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, H2M);
        E2 when E1 == E2M ->
            rukeymerge3_1(I, T1, H1, T1, E2, H2, T2, [H1 | M], E3, H3, T3);
        E2 ->
            % E2M > E1.
	    rukeymerge3_21b(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, H2M)
    end;
rukeymerge3_2(I, E1, H1, T1, [], _H2M, E2M, M, E3, H3, T3) when E1 == E2M ->
    rukeymerge2_1(I, T1, E3, T3, [H1 | M], H3);
rukeymerge3_2(I, E1, H1, T1, [], H2M, _E2M, M, E3, H3, T3) when E1 =< E3 ->
    rukeymerge2_2(I, T1, E1, T3, [H2M | M], E3, H3, H1);
rukeymerge3_2(I, _E1, H1, T1, [], H2M, _E2M, M, E3, H3, T3) ->
    rukeymerge2_1(I, T1, E3, T3, [H1, H2M | M], H3).

% E1 =< E2. Inlined.
rukeymerge3_12b(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, H2M) 
                                                             when E2 =< E3 ->
    rukeymerge3_12_3(I, E1, H1, T1, E2, H2, T2, [H2M | M], E3, H3, T3);
rukeymerge3_12b(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M, H2M) ->
    rukeymerge3_2(I, E1, H1, T1, T2, H2, E2, [H2M | M], E3, H3, T3).

% E1 > E2. Inlined
rukeymerge3_21b(I, E1, H1, T1, E2, H2, T2, E3, H3, T3, M,H2M) when E1 =< E3 ->
    rukeymerge3_21_3(I, E1, H1, T1, E2, H2, T2, [H2M | M], E3, H3, T3);
rukeymerge3_21b(I, _E1, H1, T1, E2, H2, T2, E3, H3, T3, M, H2M) ->
    rukeymerge3_1(I, T1, H1, T1, E2, H2, T2, [H1, H2M | M], E3, H3, T3).

% E1 =< E2, take L3 apart.
rukeymerge3_12_3(I, E1, H1, T1, E2, H2, T2, M, E3M, H3M, [H3 | T3]) ->
    case element(I, H3) of
	E3 when E2 =< E3 ->
            rukeymerge3_12_3(I, E1, H1, T1, E2, H2, T2, [H3M | M], E3, H3, T3);
        E3 when E2 == E3M ->
            rukeymerge3_2(I, E1, H1, T1, T2, H2, E2, M, E3, H3, T3);
        E3 ->
            rukeymerge3_2(I, E1, H1, T1, T2, H2, E2, [H3M | M], E3, H3, T3)
    end;
rukeymerge3_12_3(I, E1, H1, T1, E2, H2, T2, M, E3M, _H3M, []) when E2 == E3M ->
    rukeymerge2_2(I, T1, E1, T2, M, E2, H2, H1);
rukeymerge3_12_3(I, E1, H1, T1, E2, H2, T2, M, _E3M, H3M, []) ->
    rukeymerge2_2(I, T1, E1, T2, [H3M | M], E2, H2, H1).

% E1 > E2, take L3 apart.
rukeymerge3_21_3(I, E1, H1, T1, E2, H2, T2, M, E3M, H3M, [H3 | T3]) ->
    case element(I, H3) of
	E3 when E1 =< E3 ->
            rukeymerge3_21_3(I, E1, H1, T1, E2, H2, T2, [H3M | M], E3, H3, T3);
        E3 when E1 == E3M ->
            rukeymerge3_1(I, T1, H1, T1, E2, H2, T2, [H1 | M], E3, H3, T3);
        E3 ->
            rukeymerge3_1(I, T1, H1, T1, E2, H2, T2, [H1,H3M | M], E3, H3, T3)
    end;
rukeymerge3_21_3(I, E1, H1, T1, E2, H2, T2, M, E3M, _H3M, []) when E1 == E3M ->
    rukeymerge2_1(I, T1, E2, T2, [H1 | M], H2);
rukeymerge3_21_3(I, _E1, H1, T1, E2, H2, T2, M, _E3M, H3M, []) ->
    rukeymerge2_1(I, T1, E2, T2, [H1, H3M | M], H2).

%% ukeymerge/3

%% Elements from the first list are kept and prioritized.
ukeymerge2_1(I, [H1 | T1], E2, HdM, T2, M, H2) ->
    case element(I, H1) of
	E1 when E1 =< E2 ->
            ukeymerge2_1(I, T1, E2, E1, T2, [H1 | M], H2);
        E1 when E2 == HdM ->
            ukeymerge2_2(I, T1, E1, H1, T2, M);
        E1 ->
            ukeymerge2_2(I, T1, E1, H1, T2, [H2 | M])
    end;
ukeymerge2_1(_I, [], E2, HdM, T2, M, _H2) when E2 == HdM ->
    lists:reverse(T2, M);
ukeymerge2_1(_I, [], _E2, _HdM, T2, M, H2) ->
    lists:reverse(T2, [H2 | M]).

ukeymerge2_2(I, T1, E1, H1, [H2 | T2], M) ->
    case element(I, H2) of
	E2 when E1 =< E2 ->
            ukeymerge2_1(I, T1, E2, E1, T2, [H1 | M], H2);
        _E2 ->
            ukeymerge2_2(I, T1, E1, H1, T2, [H2 | M])
    end;
ukeymerge2_2(_I, T1, _E1, H1, [], M) ->
    lists:reverse(T1, [H1 | M]).

%% rukeymerge/3

rukeymerge2_1(I, [H1 | T1], E2, T2, M, H2) ->
    case element(I, H1) of
	E1 when E1 =< E2 ->
            rukeymerge2_2(I, T1, E1, T2, M, E2, H2, H1);
        _E1 ->
            rukeymerge2_1(I, T1, E2, T2, [H1 | M], H2)
    end;
rukeymerge2_1(_I, [], _E2, T2, M, H2) ->
    lists:reverse(T2, [H2 | M]).

rukeymerge2_2(I, T1, E1, [H2 | T2], M, E2M, H2M, H1) ->
    case element(I, H2) of
	E2 when E1 =< E2 ->
            rukeymerge2_2(I, T1, E1, T2, [H2M | M], E2, H2, H1);
        E2 when E1 == E2M ->
            rukeymerge2_1(I, T1, E2, T2, [H1 | M], H2);
        E2 ->
            rukeymerge2_1(I, T1, E2, T2, [H1, H2M | M], H2)
    end;
rukeymerge2_2(_I, T1, E1, [], M, E2M, _H2M, H1) when E1 == E2M ->
    lists:reverse(T1, [H1 | M]);
rukeymerge2_2(_I, T1, _E1, [], M, _E2M, H2M, H1) ->
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
    rfmergel([[Y, X | R] | Rs], [], Fun, asc).

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
    rfmergel([[S], [Y, X | R] | Rs], [], Fun, asc).

%% Descending.
fsplit_2(Y, X, Fun, [Z | L], R, Rs) ->
    case Fun(Y, Z) of
        false -> 
            fsplit_2(Z, Y, Fun, L, [X | R], Rs);
        true ->
            case Fun(X, Z) of
                false ->
                    fsplit_2(Y, Z, Fun, L, [X | R], Rs);
                true when R == [] ->
                    fsplit_2(Y, X, Fun, L, [Z], Rs);
                true ->
                    fsplit_2_1(Y, X, Fun, L, R, Rs, Z)
            end
    end;
fsplit_2(Y, X, Fun, [], R, Rs) ->
    fmergel([[Y, X | R] | Rs], [], Fun, desc).

fsplit_2_1(Y, X, Fun, [Z | L], R, Rs, S) ->
    case Fun(Y, Z) of
        false ->
            fsplit_2_1(Z, Y, Fun, L, [X | R], Rs, S);
        true ->
            case Fun(X, Z) of
                false ->
                    fsplit_2_1(Y, Z, Fun, L, [X | R], Rs, S);
                true ->
                    case Fun(S, Z) of
                        false ->
                            fsplit_2(Z, S, Fun, L, [], [[Y, X | R] | Rs]);
                        true ->
                            fsplit_2(S, Z, Fun, L, [], [[Y, X | R] | Rs])
                    end
            end
    end;
fsplit_2_1(Y, X, Fun, [], R, Rs, S) ->
    fmergel([[S], [Y, X | R] | Rs], [], Fun, desc).

fmergel([T1, [H2 | T2] | L], Acc, Fun, asc) ->
    fmergel(L, [fmerge2_1(T1, H2, Fun, T2, []) | Acc], Fun, asc);
fmergel([[H2 | T2], T1 | L], Acc, Fun, desc) ->
    fmergel(L, [fmerge2_1(T1, H2, Fun, T2, []) | Acc], Fun, desc);
fmergel([L], [], _Fun, _O) ->
    L;
fmergel([L], Acc, Fun, O) ->
    rfmergel([lists:reverse(L, []) | Acc], [], Fun, O);
fmergel([], Acc, Fun, O) ->
    rfmergel(Acc, [], Fun, O).

rfmergel([[H2 | T2], T1 | L], Acc, Fun, asc) ->
    rfmergel(L, [rfmerge2_1(T1, H2, Fun, T2, []) | Acc], Fun, asc);
rfmergel([T1, [H2 | T2] | L], Acc, Fun, desc) ->
    rfmergel(L, [rfmerge2_1(T1, H2, Fun, T2, []) | Acc], Fun, desc);
rfmergel([L], Acc, Fun, O) ->
    fmergel([lists:reverse(L, []) | Acc], [], Fun, O);
rfmergel([], Acc, Fun, O) ->
    fmergel(Acc, [], Fun, O).

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

%% Ascending. X < Y
ufsplit_1(Y, X, Fun, [Z | L], R, Rs) ->
    case Fun(Y, Z) of
        true ->
            case Fun(Z, Y) of
                true -> % Z equal to Y
                    ufsplit_1(Y, X, Fun, L, R, Rs);
                false ->
                    ufsplit_1(Z, Y, Fun, L, [X | R], Rs)
            end;
        false ->
            case Fun(X, Z) of
                true ->
                    case Fun(Z, X) of
                        true -> % Z equal to X
                            ufsplit_1(Y, X, Fun, L, R, Rs);
                        false ->
                            ufsplit_1(Y, Z, Fun, L, [X | R], Rs)
                    end;
                false when R == [] ->
                    ufsplit_1(Y, X, Fun, L, [Z], Rs);
                false ->
                    ufsplit_1_1(Y, X, Fun, L, R, Rs, Z)
            end
    end;
ufsplit_1(Y, X, Fun, [], R, Rs) ->
    rufmergel([[Y, X | R] | Rs], [], Fun).

%% X < Y
ufsplit_1_1(Y, X, Fun, [Z | L], R, Rs, S) ->
    case Fun(Y, Z) of
        true ->
            case Fun(Z, Y) of
                true -> % Z equal to Y
                    ufsplit_1_1(Y, X, Fun, L, R, Rs, S);
                false ->
                    ufsplit_1_1(Z, Y, Fun, L, [X | R], Rs, S)
            end;
        false ->
            case Fun(X, Z) of
                true ->
                    case Fun(Z, X) of
                        true -> % Z equal to X
                            ufsplit_1_1(Y, X, Fun, L, R, Rs, S);
                        false ->
                            ufsplit_1_1(Y, Z, Fun, L, [X | R], Rs, S)
                    end;
                false ->
                    case Fun(S, Z) of
                        true ->
                            case Fun(Z, S) of
                                true -> % Z equal to S
                                    ufsplit_1_1(Y, X, Fun, L, R, Rs, S);
                                false ->
                                    ufsplit_1(Z, S, Fun, L, [], [[Y, X | R] | Rs])
                            end;
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
        true ->
            case Fun(Z, Y) of
                true -> % Z equal to Y
                    ufsplit_2(Y, L, Fun, R);
                false ->
                    ufsplit_1(Z, Y, Fun, L, [], [lists:reverse(R, [])])
            end;
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
%% HdM before H2.
ufmerge2_1([H1 | T1], H2, Fun, T2, M, HdM) ->
    case Fun(H1, H2) of
        true ->
            ufmerge2_1(T1, H2, Fun, T2, [H1 | M], H1);
        false ->
            case Fun(H2, HdM) of
                true -> % HdM equal to H2
                    ufmerge2_2(H1, T1, Fun, T2, M);
                false ->
                    ufmerge2_2(H1, T1, Fun, T2, [H2 | M])
            end
    end;
ufmerge2_1([], H2, Fun, T2, M, HdM) ->
    case Fun(H2, HdM) of
        true -> % HdM equal to H2
            lists:reverse(T2, M);
        false ->
            lists:reverse(T2, [H2 | M])
    end.

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

%% H1 before H2M
rufmerge2_2(H1, T1, Fun, [H2 | T2], M, H2M) ->
    case Fun(H1, H2) of
        true ->
            rufmerge2_2(H1, T1, Fun, T2, [H2M | M], H2);
        false ->
            case Fun(H2M, H1) of
                true -> % H2M equal to H1
                    rufmerge2_1(T1, H2, Fun, T2, [H1 | M]);
                false ->
                    rufmerge2_1(T1, H2, Fun, T2, [H1, H2M | M])
            end
    end;
rufmerge2_2(H1, T1, Fun, [], M, H2M) ->
    case Fun(H2M, H1) of
        true -> 
            lists:reverse(T1, [H1 | M]);
        false ->
            lists:reverse(T1, [H1, H2M | M])
    end.

