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
-module(vector).

-export([to_list/1,sort/1]).

to_list(Vec) ->
    vector:to_list(Vec, []).

sort(Vec) ->
    heapsort1(Vec, size(Vec) div 2).

heapsort1(Vec, 0) ->
    heapsort2(Vec, size(Vec));
heapsort1(Vec, K) ->
    heapsort1(fix_down(Vec, K, size(Vec)), K-1).

heapsort2(Vec, 1) -> Vec;
heapsort2(Vec0, N) ->
    Elem1 = vector:get(1, Vec0),
    Elem2 = vector:get(N, Vec0),
    Vec1 = vector:set(1, Vec0, Elem2),
    Vec = vector:set(N, Vec1, Elem1),
    heapsort2(fix_down(Vec, 1, N-1), N-1).

fix_down(Vec, K, N) ->
    Elem = vector:get(K, Vec),
    J = 2*K,
    if
	J =< N ->
	    C1 = vector:get(J, Vec),
	    if
		J == N, Elem < C1 ->
		    fix_down(store(Vec, K, C1, J, Elem), J, N);
		J == N ->
		    Vec;
		true ->
		    case vector:get(J+1, Vec) of
			C2 when C1 < C2, Elem < C2 ->
			    fix_down(store(Vec, K, C2, J+1, Elem), J+1, N);
			C2 when Elem < C1 ->
			    fix_down(store(Vec, K, C1, J, Elem), J, N);
			_ -> Vec
		    end
	    end;
	true -> Vec
    end.

store(Vec, I, E1, J, E2) ->
    vector:set(J, vector:set(I, Vec, E1), E2).
