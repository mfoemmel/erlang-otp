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
-module(queue).

%% New module interface
-export([is_empty/1,len/1,
	 cons/2,head/1,tail/1,snoc/2,last/1,daeh/1,init/1,lait/1,
	 reverse/1,join/2,split/2]).

%% New "old style" module interface
-export([in_r/2,out_r/1,from_list/1]).

%%--------------------------------------------------------------------------
%% The original module interface

-export([new/0, in/2, out/1, to_list/1]).

%% efficient implementation of fifo queues

new() -> {[],[]}.

%% Append to tail
in(X, {R,F}=Q) when is_list(R), is_list(F) -> snoc(Q, X);
in(X, Q) ->
    erlang:error(badarg, [X,Q]).

%% Take from head
out({In,[V]}) when is_list(In) ->
    {{value,V},r2f(In)};
out({In,[V|Out]}) when is_list(In) ->
    {{value,V},{In,Out}};
out({[],[]}=Q) ->
    {empty,Q};
out({[V],[]}) ->
    {{value,V},{[],[]}};
out({[_|_]=In,[]}) ->
    out(r2f(In));
out(Q) ->
    erlang:error(badarg, [Q]).

to_list({In,Out}) when is_list(In), is_list(Out) ->
    Out++lists:reverse(In, []);
to_list(Q) ->
    erlang:error(badarg, [Q]).

%%--------------------------------------------------------------------------
%% Some new "old style" functions for reversed queue handling.

%% Prepend to head
in_r(X, {R,F}=Q) when is_list(R), is_list(F) -> cons(X, Q);
in_r(X, Q) ->
    erlang:error(badarg, [X,Q]).

%% Take from tail
out_r({[V],Out}) when is_list(Out) ->
    {{value,V},f2r(Out)};
out_r({[V|In],Out}) when is_list(Out) ->
    {{value,V},{In,Out}};
out_r({[],[]}=Q) ->
    {empty,Q};
out_r({[],[V]}) ->
    {{value,V},{[],[]}};
out_r({[],[_|_]=Out}) ->
    out_r(f2r(Out));
out_r(Q) ->
    erlang:error(badarg, [Q]).

%% Create queue from list
from_list(L) when is_list(L) ->
    {[],L};
from_list(L) ->
    erlang:error(badarg, [L]).

%%--------------------------------------------------------------------------
%% New API inspired by an Erlang user contribution "deque.erl" 
%% by Claes Wikstrom <klacke@kaja.klacke.net> 1999.
%%
%% This implementation does not use the internal data format from Klacke's
%% doubly ended queues that was "shamelessly stolen" from 
%% "Purely Functional Data structures" by Chris Okasaki, since the data
%% format of this module must remain the same in case some application
%% has saved a queue in external format or sends it to an old node.
%%
%% This implementation tries to do the best of the situation and should 
%% be almost as efficient as Okasaki's queues, except for len/1 that
%% is O(n) in this implementation instead of O(1).
%%
%% The implementation does this by trying to keep least at least one 
%% element in both the forward and the reversed lists so that 
%% i.e head/1 or last/1 will not have to reverse a list
%% to find the element.
%%
%% To be compatible with the old version of this module, as much data as 
%% possible is moved to the receiving side using lists:reverse/2 when data
%% is needed, except for two elements (when possible). These two elements
%% are kept to prevent alternating tail/1 and init/1 operations from 
%% moving data back and forth between the sides.
%%
%% An alternative would be to balance for equal list length when one side
%% is exhausted. Although this could be better for a general double
%% ended queue, it would more han double the amortized cost for 
%% the normal case (one way queue).

is_empty({[],[]}) ->
    true;
is_empty({In,Out}) when is_list(In), is_list(Out) ->
    false;
is_empty(Q) ->
    erlang:error(badarg, [Q]).

len({In,Out}) when is_list(In), is_list(Out) ->
    erlang:length(In)+erlang:length(Out);
len(Q) ->
    erlang:error(badarg, [Q]).



%% Cons to head
%%
%% Put at least one element in each list, if it is cheap
cons(X, {[],[]}) ->
    {[], [X]};
cons(X, {[], [_]=F}) ->
    {F,[X]};
cons(X, {R,F}) when is_list(R), is_list(F) ->
    {R,[X|F]};
cons(X, Q) ->
    erlang:error(badarg, [X,Q]).

%% Return head element
head({R,[H|_]}) when is_list(R) ->
    H;
head({[],[]}=Q) ->
    erlang:error(empty, [Q]);
head({[H],[]}) ->
    H;
head({[_|R],[]}) ->
    [H|_] = lists:reverse(R),
    H;
head(Q) ->
    erlang:error(badarg, [Q]).

%% Remove head element and return resulting queue
tail({R, [_]}) when is_list(R) ->
    r2f(R);
tail({R, [_|F]}) when is_list(R) ->
    {R,F};
tail({[],[]}=Q) ->
    erlang:error(empty, [Q]);
tail({[_],[]}) ->
    {[],[]};
tail({[_|_]=R,[]}) ->
    tail(r2f(R));
tail(Q) ->
    erlang:error(badarg, [Q]).

%% Functions operating on the other end of the queue

%% Cons to tail
%%
%% Put at least one element in each list, if it is cheap
snoc({[],[]}, X) ->
    {[X], []};
snoc({[_]=R, []}, X) ->
    {[X], R};
snoc({R,F}, X) when is_list(R), is_list(F) ->
    {[X|R],F};
snoc(Q, X) ->
    erlang:error(badarg, [Q,X]).

%% Return last element
daeh(Q) -> last(Q).

last({[H|_],F}) when is_list(F) ->
    H;
last({[],[]}=Q) ->
    erlang:error(empty, [Q]);
last({[],[H]}) ->
    H;
last({[],[_|R]}) ->
    [H|_] = lists:reverse(R),
    H;
last(Q) ->
    erlang:error(badarg, [Q]).

%% Remove tail element and return resulting queue
lait(Q) -> init(Q).

init({[_], F}) when is_list(F) ->
    f2r(F);
init({[_|R], F}) when is_list(F) ->
    {R,F};
init({[],[]}=Q) ->
    erlang:error(empty, [Q]);
init({[],[_]}) ->
    {[],[]};
init({[],[_|_]=F}) ->
    init(f2r(F));
init(Q) ->
    erlang:error(badarg, [Q]).



%% Return reversed queue
reverse({R,F}) when is_list(R), is_list(F) ->
    {F,R};
reverse(Q) ->
    erlang:error(badarg, [Q]).

%% Join two queues (perhaps not very efficient)
join({R,F}=Q, {[],[]}) when is_list(R), is_list(F) ->
    Q;
join({[],[]}, {R,F}=Q) when is_list(R), is_list(F) ->
    Q;
join({R1,F1}, {R2,F2}) when is_list(R1), is_list(F1), is_list(R2), is_list(F2) ->
    {R2,F1++lists:reverse(R1,F2)};
join(Q1, Q2) ->
    erlang:error(badarg, [Q1,Q2]).

%% Split a queue in two (perhaps not very efficient)
split(N, {R,F}=Q) when N =:= 0, is_list(R), is_list(F) ->
    {{[],[]},Q};
split(N, {R,F}=Q) when is_integer(N), N > 0, is_list(R), is_list(F) ->
    Lf = erlang:length(F),
    if  N < Lf ->
	    {F1,F2} = lists:split(N, F),
	    {f2r(F1),{R,F2}};
        N > Lf ->
	    case catch lists:split(erlang:length(R)-(N-Lf), R) of
		{'EXIT',_} ->
		    erlang:error(badarg, [N,Q]);
		{R1,R2} ->
		    {{R2,F},r2f(R1)}
	    end;
	true ->
	    {f2r(F),r2f(R)}
    end;
split(N, Q) ->
    erlang:error(badarg, [N,Q]).

%%--------------------------------------------------------------------------
%% Internal workers

%% Move all but two from R to F, if there are enough
r2f([]) ->
    {[],[]};
r2f([_]=R) ->
    {[],R};
r2f([H1,H2]) ->
    {[H1],[H2]};
r2f([H1,H2|T]) ->
    {[H1,H2],lists:reverse(T, [])}.

%% Move all but two from F to R, if there are enough
f2r([]) ->
    {[],[]};
f2r([_]=F) ->
    {F,[]};
f2r([H1,H2]) ->
    {[H2],[H1]};
f2r([H1,H2|T]) ->
    {lists:reverse(T, []),[H1,H2]}.
