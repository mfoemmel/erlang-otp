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
-module(random).

%% Reasonable random number generator.
%%  The method is attributed to B.A. Wichmann and I.D.Hill
%%  See "An efficient and portable pseudo-random number generator",
%%  Journal of Applied Statistics. AS183. 1982. Also Byte March 1987.

-export([seed/0,seed/3,uniform/0,uniform/1,
	 uniform_s/1, uniform_s/2, seed0/0]).


seed0() ->
    {3172, 9814, 20125}.

%% seed()
%%  Seed random number generation with default values

seed() ->
    seed(seed0()).

seed({A1, A2, A3}) ->
    case seed(A1, A2, A3) of
	undefined -> seed0();
	Tuple -> Tuple
    end.	

%% seed(A1, A2, A3) 
%%  Seed random number generation 

seed(A1, A2, A3) ->
    put(random_seed, 
	{abs(A1) rem 30269, abs(A2) rem 30307, abs(A3) rem 30323}).

%% uniform()
%%  Returns a random float between 0 and 1.

uniform() ->
    {A1, A2, A3} = case get(random_seed) of
		       undefined -> seed0();
		       Tuple -> Tuple
		   end,
    B1 = (A1*171) rem 30269,
    B2 = (A2*172) rem 30307,
    B3 = (A3*170) rem 30323,
    put(random_seed, {B1,B2,B3}),
    R = A1/30269 + A2/30307 + A3/30323,
    R - trunc(R).

%% uniform(N) -> I
%%  Given an integer N >= 1, uniform(N) returns a random integer
%%  between 1 and N.

uniform(N) when N >= 1 ->
    trunc(uniform() * N) + 1.


%%% Functional versions

%% uniform_s(State) -> {F, NewState}
%%  Returns a random float between 0 and 1.

uniform_s({A1, A2, A3}) ->
    B1 = (A1*171) rem 30269,
    B2 = (A2*172) rem 30307,
    B3 = (A3*170) rem 30323,
    R = A1/30269 + A2/30307 + A3/30323,
    {R - trunc(R), {B1,B2,B3}}.

%% uniform_s(N, State) -> {I, NewState}
%%  Given an integer N >= 1, uniform(N) returns a random integer
%%  between 1 and N.

uniform_s(N, State0) when N >= 1 ->
    {F, State1} = uniform_s(State0),
    {trunc(F * N) + 1, State1}.
