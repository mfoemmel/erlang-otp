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
%-define(ok,1).

-ifdef(ok).
%0
-module(subscriber).
-compile(export_all).

-include("subscriber.hrl").
-include_lib("mnemosyne/include/mnemosyne.hrl").
%0
-else.
-module(subscriber).
-compile(export_all).
-compile({parse_transform,mnemosyne}).

-include("subscriber.hrl").
-endif.

blocked_snbs() ->
%1
query
    [ S.snb ||                  % collect the subscriber number
        S <- table(subscriber), % where S is taken from the subscriber table
        L <- table(line),       % and L is taken from the line table
        L.state = blocked,      % and the state of that line is blocked
        L.li = S.li             % and L and S uses the same li
    ]
end
%1
.

%2
blocked_subscribers(S, subscriber) :-
        S <- table(subscriber),
        L <- table(line),
        L.state = blocked,
        L.li = S.li.
%2


blocked_snbs1() ->
mnemosyne:eval(
%3
query [ S.snb || S <- rule(blocked_subscribers) ] end
%3
).

%4
free_subscriber_numbers() ->
    mnemosyne:eval(
      query [ S.snb || S <- table(subscriber),
                       S.li = none]
      end
     ).
%4


%5
limit_exceeded(S, subscriber) :-
    S <- table(subscriber),
    A <- table(account),
    A.snb = S.snb,
    A.cost > S.cost_limit.
%5

eval_example1() ->
%6
    Handle = 
        query 
           [ S.snb || S <- table(subscriber),
                      S.li = none]
        end,

    AllAnswers = 
        mnesia:transaction(
             fun() -> 
                      mnemosyne:eval(Handle)
             end)
%6
   , AllAnswers.
      
eval_example2() ->
%7
    Handle = 
        query 
           [ S.snb || S <- table(subscriber),
                      S.li = none]
        end,

    AFewAnswers = 
        mnesia:transaction(
             fun() -> 
                     Cursor = mnemosyne:cursor(Handle),
                     % now get at least two but not 
                     % more than five solutions:
                     L = mnemosyne:next_answers(Cursor,2,5),
                     mnemosyne:delete_cursor(Cursor),
                     L
             end)
%7
   , AFewAnswers.

eval_example3() ->
%8
    Handle = 
        query 
           [ S.snb || S <- table(subscriber),
                      S.li = none]
        end,

    QuerySetup = mnemosyne:setup_query(Handle),

    AFewAnswers = 
        mnesia:transaction(
             fun() ->
                    Cursor = mnemosyne:init_query(QuerySetup),
                    mnemosyne:next_answers(Cursor, 5, 5)
             end),
      
      % Here we may call more init_query-next_answers constructions
      % with the same Handle. Note that the query is evaluated from
      % "scratch" because of the call to mnemosyne:init_query/1.

      mnemosyne:delete_query(QuerySetup)
%8
   , AFewAnswers.
      
%9
-record (blocked, {snb, li}).
blocked (X) :-
    S <- table (subscriber),
    L <- table(line),
    L.state = blocked,
    L.li = S.li,
    X = #blocked{snb=S#subscriber.snb, li=S#subscriber.li}.


%9
