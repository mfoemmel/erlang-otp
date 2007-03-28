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
-module(mnemosyne).
-export([
	 string_to_handle/1,
	 parse_transform/2,
	 format_error/1,
	 version/0,
	 eval/1,
	 reoptimize/1,
	 cursor/1, cursor/2,
	 next_answers/1, next_answers/3,
	 all_answers/1, all_answers/3,
	 delete_cursor/1,
	 setup_query/1,
	 init_query/1, init_query/2,
	 delete_query/1,

	 %% Adm:
	 info/0
	]).  

%% Debug support
-export([ms/0]).


-define(default_min, 10).
-define(default_max, 100).
-define(default_prefetch, 1).

%%%---- User functions

%% takes a query list comprehension (incl last dot and trailing whitspace)
%% and returns a Query Handle:
%%
%%     Q = mnemosyne:string_to_handle(
%%             "query [ X || X <- table(q),Y <- table(r),X.b = Y.f1] end. "),
%%     mnesia:transaction(fun() -> mnemosyne:eval(Q) end).
%%
%%
%% 

string_to_handle(Str) ->
    case erl_scan:string(Str) of
	{ok, Tokens, _} ->
	    case erl_parse:parse_exprs(Tokens) of
		{ok, ExprList} ->
		    catch(mnemosyne_lc:one_lc_to_handle(ExprList));
		Others ->
		    Others
	    end;

	Others ->
	    Others
    end.


%%%---- Entry for the list-comprehension parser

parse_transform(Form, Options) ->
    mnemosyne_lc:parse_transform(Form, Options).

format_error(mnesia_not_running) ->
    "Mnesia is not running".


version() -> "1.1".

%%%---- Dynamic compilation
%% Supposed to be called as:
%% 
%%     Q = query [ ... || ... ] end, 
%%         ....
%%    ( Q' = mnemosyne:reoptimize(Q), )*
%%         ....
%%     mnesia:transaction(
%%         fun() ->
%%	     Answers = mnemosyne:eval(Q)
%%	         ....
%%	   end),

eval(Call) ->
    Cursor = cursor(Call, 1000),
    Answers = (catch all_answers(Cursor, 1000, 2000)),
    delete_cursor(Cursor),
    case Answers of
	{'EXIT', Cause} ->
	    exit(Cause);
	Answers ->
	    Answers
    end.
    
reoptimize({call,Call,_}) -> {call,Call,mnemosyne_optimizer:phase2(Call)}.

%%%---- Cursor handling
%% Supposed to be called as:
%% 
%%     Q = query [ ... || ... ] end, 
%%         ....
%%    ( Q' = mnemosyne:reoptimize(Q), )*
%%         ....
%%     mnesia:transaction(
%%         fun() ->
%%	     Crsr = mnemosyne:cursor(Q),
%%	         ....
%%	     loop over
%%	       get_answers(Crsr),  OR  mnemosyne:all_answers(Csr)
%%	         ....
%%	     mnemosyne:delete_cursor(Crsr)
%%	   end),
%%     


cursor(Call) -> cursor(Call,?default_prefetch).

cursor(Call, Nprefetch) when integer(Nprefetch), Nprefetch>0 -> 
    Qcoll = setup_query(Call),
    init_query(Qcoll, Nprefetch).


next_answers(Cursor) -> next_answers(Cursor, ?default_min, ?default_max).

next_answers({cursor,_,ExecC}, Nmin, Nmax) when integer(Nmin), 
						integer(Nmax), 
						Nmin > 0,
						Nmin =< Nmax ->
    mnemosyne_exec:get_answers(ExecC, Nmin, Nmax).
		


all_answers(Cursor) -> all_answers(Cursor, ?default_min, ?default_max).

all_answers({cursor,_,ExecC}, Nmin, Nmax) when integer(Nmin), 
					       integer(Nmax), 
					       Nmin > 0,
					       Nmin =< Nmax ->
    collect_all_answers(ExecC, Nmin, Nmax, []).



delete_cursor({cursor,Qcoll,_}) ->
    mnemosyne_exec:kill_processes(Qcoll).


%%%---- Detailed Cursor Handling
%% Supposed to be called as:
%% 
%%     Q = query [ ... || ... ] end, 
%%         ....
%%    ( Q' = mnemosyne:reoptimize(Q), )*
%%         ....
%%     H = mnemosyne:setup_query(Q),
%%         ....
%% poosibly loop over
%%     mnesia:transaction(
%%         fun() ->
%%	     Crsr = mnemosyne:init_query(H),
%%	         ....
%%	    loop over
%%	       get_answers(Crsr),  OR  mnemosyne:all_answers(Csr)
%%	         ....
%%	   end),
%%         ....
%%     mnemosyne:delete_query(Q)
%%     


setup_query({call,_,Opt}) -> 
    {setup_query, mnemosyne_exec:setup_collector_and_query(Opt)}.

init_query(Q) -> 
    init_query(Q,?default_prefetch).

init_query({setup_query,Q}, Nprefetch) when integer(Nprefetch), Nprefetch>0 ->
    {cursor, Q, mnemosyne_exec:init_query(Q,Nprefetch)}.

delete_query({setup_query, Q}) ->
    mnemosyne_exec:kill_processes(Q).

%%%----------------------------------------------------------------
%%% Adm

info() ->
    mnemosyne_catalog:info(),
    ok.

%%%----------------------------------------------------------------
%%% Private functions


collect_all_answers(ExecC, Nmin, Nmax, Acc) ->
    case mnemosyne_exec:get_answers(ExecC, Nmin, Nmax) of
	[] -> Acc;
	L when list(L) -> collect_all_answers(ExecC, Nmin, Nmax, Acc++L)
    end.


%%%----------------------------------------------------------------
%%% Debug support (still used by mnesia as of mnesia-4.3.3).

ms() ->
    [
     mnemosyne,
     mnemosyne_catalog,
     mnemosyne_compiler,
     mnemosyne_constraint,
     mnemosyne_cost,
     mnemosyne_debug,
     mnemosyne_exec,
     mnemosyne_lc,
     mnemosyne_lib,
     mnemosyne_op,
     mnemosyne_optimizer,
     mnemosyne_pp,
     mnemosyne_slg,
     mnemosyne_sup,
     mnemosyne_transform,
     mnemosyne_unify
    ].
