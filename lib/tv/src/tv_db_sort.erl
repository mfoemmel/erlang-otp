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

-module(tv_db_sort).



-export([mergesort/3, merge/4, get_compare_value/2]).





%%%*********************************************************************
%%% EXTERNAL FUNCTIONS
%%%*********************************************************************





mergesort(KeyNo, [X], ReverseOrder) -> 
    [X];
mergesort(KeyNo, [], ReverseOrder) -> 
    [];
mergesort(KeyNo, X, ReverseOrder) -> 
    split(KeyNo, X, [], [], ReverseOrder).








   %% If we want reverse order when just merging two lists,
   %% each of them has to be in reverse order first!

merge(KeyNo, [{E1, C1} | T1], [{E2, C2} | T2], Reverse) when Reverse == false ->
    K1 = get_compare_value(KeyNo, E1),
    K2 = get_compare_value(KeyNo, E2),
    case get_correct_order(K1, E1, K2, E2) of
	{1, 2} ->
	    [{E1, C1} | merge(KeyNo, T1, [{E2, C2} | T2], Reverse)]; 
	{2, 1} ->
	    [{E2, C2} | merge(KeyNo, [{E1, C1} | T1], T2, Reverse)]
    end;
merge(KeyNo, [{E1, C1} | T1], [{E2, C2} | T2], Reverse) ->
    K1 = get_compare_value(KeyNo, E1),
    K2 = get_compare_value(KeyNo, E2),
    case get_correct_order(K1, E1, K2, E2) of
	{1, 2} ->
	    [{E2, C2} | merge(KeyNo, [{E1, C1} | T1], T2, Reverse)];
	{2, 1} ->
	    [{E1, C1} | merge(KeyNo, T1, [{E2, C2} | T2], Reverse)] 
    end;
merge(KeyNo, [], L2, Reverse) ->       % L2 may be the empty list also!
    L2;
merge(KeyNo, L1, [], Reverse) ->       % L1 may be the empty list also!
    L1.

    




get_compare_value(KeyNo, E) when tuple(E) ->
    case catch element(KeyNo, E) of
	{'EXIT', {badarg, {?MODULE, get_compare_value, [KeyNo, E]}}} ->
	    short_tuple;
	V ->
	    {tuple, V}
    end;
get_compare_value(KeyNo, E) ->
    no_tuple.










%%%********************************************************************
%%% INTERNAL FUNCTIONS
%%%********************************************************************




split(KeyNo, [A,B|T], X, Y, Reverse) ->
    split(KeyNo, T, [A|X], [B|Y], Reverse);
split(KeyNo, [H], X, Y, Reverse) ->
    split(KeyNo, [], [H|X], Y, Reverse);
split(KeyNo, [], X, Y, Reverse) ->
    merge(KeyNo, 
	  mergesort(KeyNo, X, Reverse), 
	  mergesort(KeyNo, Y, Reverse), 
	  Reverse).






get_correct_order({tuple, V1}, E1, {tuple, V2}, E2) when V1 < V2 ->
    {1, 2};
get_correct_order({tuple, V1}, E1, {tuple, V2}, E2) ->
    {2, 1};
get_correct_order(short_tuple, E1, {tuple, V2}, E2) ->
    {1, 2};
get_correct_order({tuple, V1}, E1, short_tuple, E2) ->
    {2, 1};
get_correct_order(short_tuple, E1, short_tuple, E2) when E1 < E2 ->
    {1, 2};
get_correct_order(short_tuple, E1, short_tuple, E2) ->
    {2, 1};
get_correct_order(no_tuple, E1, no_tuple, E2) when E1 < E2 ->
    {1, 2};
get_correct_order(no_tuple, E1, no_tuple, E2) ->
    {2, 1};
get_correct_order(_Anything, E1, no_tuple, E2) ->     % Tuples first, then other 
    {1, 2};                                           % terms in correct order!
get_correct_order(no_tuple, E1, _Anything, E2) ->
    {2, 1}.
