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
-module(erl_eval).

%% An evaluator for Erlang abstract syntax.

-export([exprs/2,exprs/3,expr/2,expr/3,expr_list/2,expr_list/3]).
-export([new_bindings/0,bindings/1,binding/2,add_binding/3,del_binding/2]).

-export([is_constant_expr/1, partial_eval/1]).

%% The following exports are here for backwards compatibility.
-export([seq/2,seq/3,arg_list/2,arg_list/3]).

-import(lists, [reverse/1,flatmap/2,foldl/3]).

%% seq(ExpressionSeq, Bindings)
%% seq(ExpressionSeq, Bindings, LocalFuncHandler)
%% arg_list(ExpressionList, Bindings)
%% arg_list(ExpressionList, Bindings, LocalFuncHandler)
%%  These calls are here for backwards compatibility (BC sucks!).

seq(Exprs, Bs)     -> exprs(Exprs, Bs).
seq(Exprs, Bs, Lf) -> exprs(Exprs, Bs, Lf).

arg_list(Es, Bs)     -> expr_list(Es, Bs).
arg_list(Es, Bs, Lf) -> expr_list(Es, Bs, Lf).

%% exprs(ExpressionSeq, Bindings)
%% exprs(ExpressionSeq, Bindings, LocalFuncHandler)
%%  Returns:
%%	{value,Value,NewBindings}

exprs(Exprs, Bs) ->
    exprs(Exprs, Bs, none).

exprs(Exprs, Bs, Lf) ->
    exprs(Exprs, Bs, true, Lf).

exprs([E|Es], Bs0, _, Lf) ->
    {value,V,Bs} = expr(E, Bs0, Lf),
    exprs(Es, Bs, V, Lf);
exprs([], Bs, V, Lf) ->
    {value,V,Bs}.

%% expr(Expression, Bindings)
%% expr(Expression, Bindings, LocalFuncHandler)
%%  Returns:
%%	{value,Value,NewBindings}

expr(E, Bs) ->
    expr(E, Bs, none).

expr({var,L,V}, Bs, Lf) ->
    case binding(V, Bs) of
	{value,Val} ->
	    {value,Val,Bs};
	unbound ->
	    exit({{unbound,V},[{?MODULE,expr,3}]})
    end;
expr({char,_,C}, Bs, Lf) ->
    {value,C,Bs};
expr({integer,_,I}, Bs, Lf) ->
    {value,I,Bs};
expr({float,_,F}, Bs, Lf) ->
    {value,F,Bs};
expr({atom,_,A}, Bs, Lf) ->
    {value,A,Bs};
expr({string,_,S}, Bs, Lf) ->
    {value,S,Bs};
expr({nil, _}, Bs, Lf) ->
    {value,[],Bs};
expr({cons,_,H0,T0}, Bs0, Lf) ->
    {value,H,Bs1} = expr(H0, Bs0, Lf),
    {value,T,Bs2} = expr(T0, Bs0, Lf),
    {value,[H|T],merge_bindings(Bs1, Bs2)};
expr({lc,_,E,Qs}, Bs, Lf) ->
    eval_lc(E, Qs, Bs, Lf);
expr({tuple,_,Es}, Bs0, Lf) ->
    {Vs,Bs} = expr_list(Es, Bs0, Lf),
    {value,list_to_tuple(Vs),Bs};
expr({record_index,_,Name,F}, Bs, Lf) ->
    exit({undef_record,Name});
expr({record,_,Name,Fs}, Bs, Lf) ->
    exit({undef_record,Name});
expr({record_field,_,Rec,Name,F}, Bs, Lf) ->
    exit({undef_record,Name});
expr({record,_,Rec,Name,Fs}, Bs, Lf) ->
    exit({undef_record,Name});
expr({record_field,_,Rec,F}, Bs, Lf) ->
    exit(undef_record);
expr({block,_,Es}, Bs, Lf) ->
    exprs(Es, Bs, Lf);
expr({'if',_,Cs}, Bs, Lf) ->
    if_clauses(Cs, Bs, Lf);
expr({'case',_,E,Cs}, Bs0, Lf) ->
    {value,Val,Bs} = expr(E, Bs0, Lf),
    case_clauses(Val, Cs, Bs, Lf);
expr({'receive',_,Cs}, Bs, Lf) ->
    receive_clauses(Cs, Bs, Lf,  []);
expr({'receive',_, Cs, E, TB}, Bs0, Lf) ->
    {value,T,Bs} = expr(E, Bs0, Lf),
    receive_clauses(T, Cs, {TB,Bs}, Bs0, Lf, []);
expr({'fun',Line,{clauses,Cs}}, Bs, Lf) ->
    %% This is a really ugly hack!
    case length(element(3,hd(Cs))) of
	0 -> {value,fun () -> eval_fun(Cs, [], Bs, Lf) end,Bs};
	1 -> {value,fun (A) -> eval_fun(Cs, [A], Bs, Lf) end,Bs};
	2 -> {value,fun (A,B) -> eval_fun(Cs, [A,B], Bs, Lf) end,Bs};
	3 -> {value,fun (A,B,C) -> eval_fun(Cs, [A,B,C], Bs, Lf) end,Bs};
	4 -> {value,fun (A,B,C,D) -> eval_fun(Cs, [A,B,C,D], Bs, Lf) end,Bs};
	5 -> {value,
	      fun (A,B,C,D,E) -> eval_fun(Cs, [A,B,C,D,E], Bs, Lf) end,
	      Bs};
	6 -> {value,
	      fun (A,B,C,D,E,F) -> eval_fun(Cs, [A,B,C,D,E,F], Bs, Lf) end,
	      Bs};
	7 -> {value,
	      fun (A,B,C,D,E,F,G) -> eval_fun(Cs, [A,B,C,D,E,F,G], Bs, Lf) end,
	      Bs};
	8 -> {value,
	      fun (A,B,C,D,E,F,G,H) -> eval_fun(Cs, [A,B,C,D,E,F,G,H], Bs, Lf) end,
	      Bs};
	9 -> {value,
	      fun (A,B,C,D,E,F,G,H,I) -> eval_fun(Cs, [A,B,C,D,E,F,G,H,I], Bs, Lf) end,
	      Bs};
	10 -> {value,
	      fun (A,B,C,D,E,F,G,H,I,J) -> eval_fun(Cs, [A,B,C,D,E,F,G,H,I,J], Bs, Lf) end,
	      Bs};
	11 -> {value,
	      fun (A,B,C,D,E,F,G,H,I,J,K) -> eval_fun(Cs, [A,B,C,D,E,F,G,H,I,J,K], Bs, Lf) end,
	      Bs};
	12 -> {value,
	      fun (A,B,C,D,E,F,G,H,I,J,K,L) -> eval_fun(Cs, [A,B,C,D,E,F,G,H,I,J,K], Bs, Lf) end,
	      Bs};
	13 -> {value,
	      fun (A,B,C,D,E,F,G,H,I,J,K,L,M) -> eval_fun(Cs, [A,B,C,D,E,F,G,H,I,J,K,L,M], Bs, Lf) end,
	      Bs};
	14 -> {value,
	      fun (A,B,C,D,E,F,G,H,I,J,K,L,M,N) -> eval_fun(Cs, [A,B,C,D,E,F,G,H,I,J,K,L,M,N], Bs, Lf) end,
	      Bs};
	15 -> {value,
	      fun (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) -> eval_fun(Cs, [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O], Bs, Lf) end,
	      Bs};
	16 -> {value,
	      fun (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) -> eval_fun(Cs, [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], Bs, Lf) end,
	      Bs};
	17 -> {value,
	      fun (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) -> eval_fun(Cs, [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q], Bs, Lf) end,
	      Bs};
	18 -> {value,
	      fun (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) -> eval_fun(Cs, [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R], Bs, Lf) end,
	      Bs};
	19 -> {value,
	      fun (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) -> eval_fun(Cs, [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S], Bs, Lf) end,
	      Bs};
	20 -> {value,
	      fun (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) -> eval_fun(Cs, [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T], Bs, Lf) end,
	      Bs};
	Other ->
	    exit({'argument_limit',{'fun',Line,Cs}})
    end;
expr({call,_,{remote,_,Mod,Func},As0}, Bs0, Lf) ->
    {value,M,Bs1} = expr(Mod, Bs0, Lf),
    {value,F,Bs2} = expr(Func, Bs0, Lf),
    {As,Bs3} = expr_list(As0, merge_bindings(Bs1, Bs2), Lf),
    {value,apply(M, F, As),Bs3};
expr({call,_,Func0,As0}, Bs0, Lf) ->		%Local functions handler
    {value,Func,Bs1} = expr(Func0, Bs0, Lf),
    if
	function(Func) ->
	    {As,Bs2} = expr_list(As0, Bs1, Lf),
	    {value,apply(Func, As),Bs2};
	true ->
	    case erl_internal:bif(Func, length(As0)) of
		true ->
		    {As,Bs2} = expr_list(As0, Bs1, Lf),
		    {value,bif(Func, As),Bs2};
		false ->
		    local_func(Func, As0, Bs1, Lf)
	    end
    end;
expr({'catch',_,Expr}, Bs0, Lf) ->
    Ref = make_ref(),
    case catch {Ref,expr(Expr, Bs0, Lf)} of
	{Ref,{value,Val,Bs}=Ret} ->		%Nothing was thrown (guaranteed).
	    Ret;
	Other ->
	    {value,Other,Bs0}
    end;
expr({match,_,Lhs,Rhs0}, Bs0, Lf) ->
    {value,Rhs,Bs1} = expr(Rhs0, Bs0, Lf),
    case match(Lhs, Rhs, Bs1) of
	{match,Bs} ->
	    {value,Rhs,Bs};
	nomatch ->
	    exit({{badmatch,Rhs},[{erl_eval,expr,3}]})
    end;
expr({op,_,Op,A0}, Bs0, Lf) ->
    {value,A,Bs} = expr(A0, Bs0, Lf),
    {value,eval_op(Op, A),Bs};
expr({op,_,Op,L0,R0}, Bs0, Lf) ->
    {value,L,Bs1} = expr(L0, Bs0, Lf),
    {value,R,Bs2} = expr(R0, Bs0, Lf),
    {value,eval_op(Op, L, R),merge_bindings(Bs1, Bs2)};
expr({bin,_,Fs}, Bs0, Lf) ->
    eval_bits:expr_grp(Fs,Bs0,
		       fun(E, B) -> expr(E, B, Lf) end,
		       [],
		       true);
expr({remote,_,M,F}, Bs, Lf) ->
    exit({{badexpr,':'},[{erl_eval,expr,3}]});
expr({field,_,Rec,F}, Bs, Lf) ->
    exit({{badexpr, '.'},[{erl_eval,expr,3}]});
expr({value,_,Val}, Bs, Lf) ->			%Special case straight values.
    {value,Val,Bs}.

%% local_func(Function, Arguments, Bindings, LocalFuncHandler) ->
%%	{value,Value,Bindings} when
%%	LocalFuncHandler = {value,F}|{value,F,Eas}|{eval,F}|{eval,F,Eas}|none.

local_func(Func, As0, Bs0, {value,F}) ->
    {As1,Bs1} = expr_list(As0, Bs0, {value,F}),
    {value,apply(F, [Func,As1]),Bs1};
local_func(Func, As0, Bs0, {value,F,Eas}) ->
    {As1,Bs1} = expr_list(As0, Bs0, {value,F,Eas}),
    {value,apply(F, [Func,As1|Eas]),Bs1};
local_func(Func, As, Bs, {eval,F}) ->
    apply(F, [Func,As,Bs]);
local_func(Func, As, Bs, {eval,F,Eas}) ->
    apply(F, [Func,As,Bs|Eas]);
%% These two clauses are for backwards compatibility.
local_func(Func, As0, Bs0, {M,F}) ->
    {As1,Bs1} = expr_list(As0, Bs0, {M,F}),
    {value,apply(M, F, [Func,As1]),Bs1};
local_func(Func, As, Bs, {M,F,Eas}) ->
    apply(M, F, [Func,As|Eas]);
%% Default unknown function handler to undefined function.
local_func(Func, As0, Bs0, none) ->
    exit({undef,[{erl_eval,Func,length(As0)}]}).

%% eval_lc(Expr, [Qualifier], Bindings, LocalFunctionHandler) ->
%%	{value,Value,Bindings}.
%%  This is evaluating list comprehensions "straight out of the book".

eval_lc(E, Qs, Bs, Lf) ->
    {value,eval_lc1(E, Qs, Bs, Lf),Bs}.

eval_lc1(E, [{generate,_,P,L0}|Qs], Bs0, Lf) ->
    {value,L1,Bs1} = expr(L0, Bs0, Lf),
    flatmap(fun (V) ->
		    case match(P, V, new_bindings()) of
			{match,Bsn} ->
			    Bs2 = add_bindings(Bsn, Bs1),
			    eval_lc1(E, Qs, Bs2, Lf);
			nomatch -> []
		    end end, L1);
eval_lc1(E, [F|Qs], Bs0, Lf) ->
    case erl_lint:is_guard_test(F) of
	true ->
	    case guard_test(F, Bs0, Lf) of
		{value,true,Bs1} -> eval_lc1(E, Qs, Bs1, Lf);
		{value,false,Bs1} -> []
	    end;
	false ->
	    case expr(F, Bs0, Lf) of
		{value,true,Bs1} -> eval_lc1(E, Qs, Bs1, Lf);
		{value,false,Bs1} -> [];
		Other -> exit({bad_filter,[{erl_eval,expr,3}]})
	    end
    end;
eval_lc1(E, [], Bs, Lf) ->
    {value,V,_} = expr(E, Bs, Lf),
    [V].

%% eval_fun(Clauses, Arguments, Bindings, LocalFunctionHandler) ->
%%	Value

eval_fun([{clause,L,H,G,B}|Cs], As, Bs0, Lf) ->
    case match_list(H, As, new_bindings()) of
	{match,Bsn} ->				%The new bindings for the head
	    Bs1 = add_bindings(Bsn, Bs0),	% which then shadow!
	    case guard(G, Bs1, Lf) of
		true ->
		    {value,V,Bs2} = exprs(B, Bs1, Lf),
		    V;
		false -> eval_fun(Cs, As, Bs0, Lf)
	    end;
	nomatch ->
	    eval_fun(Cs, As, Bs0, Lf)
    end;
eval_fun([], As, Bs, Lf) ->
    exit({function_clause,[{?MODULE,'-inside-a-shell-fun-',As},
			   {erl_eval,expr,3}]}).

%% expr_list(ExpressionList, Bindings)
%% expr_list(ExpressionList, Bindings, LocalFuncHandler)
%%  Evaluate a list of expressions "in parallel" at the same level.

expr_list(Es, Bs) ->
    expr_list(Es, [], Bs, Bs, none).

expr_list(Es, Bs, Lf) ->
    expr_list(Es, [], Bs, Bs, Lf).

expr_list([E|Es], Vs, BsOrig, Bs0, Lf) ->
    {value,V,Bs1} = expr(E, BsOrig, Lf),
    expr_list(Es, [V|Vs], BsOrig, merge_bindings(Bs1, Bs0), Lf);
expr_list([], Vs, _, Bs, Lf) ->
    {reverse(Vs),Bs}.

eval_op('*', A1, A2) -> A1 * A2;
eval_op('/', A1, A2) -> A1 / A2;
eval_op('+', A1, A2) -> A1 + A2;
eval_op('-', A1, A2) -> A1 - A2;
eval_op('div', A1, A2) -> A1 div A2;
eval_op('rem', A1, A2) -> A1 rem A2;
eval_op('band', A1, A2) -> A1 band A2;
eval_op('bor', A1, A2) -> A1 bor A2;
eval_op('bxor', A1, A2) -> A1 bxor A2;
eval_op('bsl', A1, A2) -> A1 bsl A2;
eval_op('bsr', A1, A2) -> A1 bsr A2;
eval_op('<', E1, E2) -> E1 < E2;
eval_op('=<', E1, E2) -> E1 =< E2;
eval_op('>', E1, E2) -> E1 > E2;
eval_op('>=', E1, E2) -> E1 >= E2;
eval_op('==', E1, E2) -> E1 == E2;
eval_op('/=', E1, E2) -> E1 /= E2;
eval_op('=:=', E1, E2) -> E1 =:= E2;
eval_op('=/=', E1, E2) -> E1 =/= E2;
eval_op('and', E1, E2) -> E1 and E2;
eval_op('or', E1, E2) -> E1 or E2;
eval_op('xor', E1, E2) -> E1 xor E2;
eval_op('++', A1, A2) -> A1 ++ A2;
eval_op('--', A1, A2) -> A1 -- A2;
eval_op('!', E1, E2) -> E1 ! E2.

eval_op('+', A) -> A;
eval_op('-', A) -> -A;
eval_op('bnot', A) -> bnot A;
eval_op('not', A) -> not A.

%% bif(Name, Arguments)
%%  Evaluate the Erlang builtin function Name. N.B. Special case apply
%%  here, apply/2 needs to be explicit.

bif(apply, [M,F,As]) ->
    apply(M, F, As);
bif(apply, [F,As]) ->
    apply(F, As);
bif(Name, As) ->
    apply(erlang, Name, As).

%% if_clauses(Clauses, Bindings, LocalFuncHandler)

if_clauses([{clause,_,[],G,B}|Cs], Bs, Lf) ->
    case guard(G, Bs, Lf) of
	true -> exprs(B, Bs, Lf);
	false -> if_clauses(Cs, Bs, Lf)
    end;
if_clauses([], Bs, Lf) ->
    exit({if_clause,[{erl_eval,expr,3}]}).

%% case_clauses(Value, Clauses, Bindings, LocalFuncHandler)

case_clauses(Val, Cs, Bs, Lf) ->
    case match_clause(Cs, Val, Bs, Lf) of
	{B, Bs1} ->
	    exprs(B, Bs1, Lf);
	nomatch ->
	    exit({{case_clause,Val},[{erl_eval,expr,3}]})
    end.

%%
%% receive(Clauses, Bindings, LocalFuncHandler, Messages) 
%%
receive_clauses(Cs, Bs, Lf, Ms) ->
    receive
	Val ->
	    case match_clause(Cs, Val, Bs, Lf) of
		{B, Bs1} ->
		    merge_queue(Ms),
		    exprs(B, Bs1, Lf);
		nomatch ->
		    receive_clauses(Cs, Bs, Lf, [Val|Ms])
	    end
    end.
%%
%% receive_clauses(TimeOut, Clauses, TimeoutBody, Bindings, LocalFuncHandler)
%%
receive_clauses(T, Cs, TB, Bs, Lf, Ms) ->
    {_,_} = statistics(runtime),
    receive
	Val ->
	    case match_clause(Cs, Val, Bs, Lf) of
		{B, Bs1} ->
		    merge_queue(Ms),
		    exprs(B, Bs1, Lf);
		nomatch ->
		    {_,T1} = statistics(runtime),
		    if
			T == infinity ->
			    receive_clauses(T, Cs, TB, Bs, Lf, [Val|Ms]);
			T-T1 =< 0 ->
			    receive_clauses(0, Cs, TB, Bs, Lf, [Val|Ms]);
			true ->
			    receive_clauses(T-T1, Cs, TB, Bs, Lf, [Val|Ms])
		    end
	    end
    after T ->
	    merge_queue(Ms),
	    {B, Bs1} = TB,
	    exprs(B, Bs1, Lf)
    end.

merge_queue(Ms) ->
    send_all(recv_all(Ms), self()).

recv_all(Xs) ->
    receive
	X -> recv_all([X|Xs])
    after 0 ->
	    reverse(Xs)
    end.

send_all([X|Xs], Self) ->
    Self ! X,
    send_all(Xs, Self);
send_all([], _) -> true.
	
    
%% match_clause -> {Body, Bindings} or nomatch

match_clause([{clause,_,[P],G,B}|Cs], Val, Bs, Lf) ->
    case match(P, Val, Bs) of
	{match, Bs1} ->
	    case guard(G, Bs1, Lf) of
		true -> {B, Bs1};
		false -> match_clause(Cs, Val, Bs, Lf)
	    end;
	nomatch -> match_clause(Cs, Val, Bs, Lf)
    end;
match_clause([], _, _, _) ->
    nomatch.


%% guard(GuardTests, Bindings, LocalFuncHandler) -> true | false.
%%  Evaluate a guard.  We test if the guard is a true guard.

guard(L=[G|_], Bs0, Lf) when list(G) ->
    guard1(L, Bs0, Lf);
guard(L, Bs0, Lf) ->
    guard0(L, Bs0, Lf).

%% disjunction of guard conjunctions
guard1([G|Gs], Bs0, Lf) when list(G) ->
    case guard0(G, Bs0, Lf) of
	true ->
	    true;
	false ->
	    guard1(Gs, Bs0, Lf)
    end;
guard1([], Bs, Lf) -> false.

%% guard conjunction
guard0([G|Gs], Bs0, Lf) ->
    case erl_lint:is_guard_test(G) of
	true ->
	    case guard_test(G, Bs0, Lf) of
		{value,true,Bs} -> guard0(Gs, Bs, Lf);
		{value,false,Bs} -> false
	    end;
	false ->
	    exit({guard_expr,[{erl_eval,expr,3}]})
    end;
guard0([], Bs, Lf) -> true.
	
%% guard_test(GuardTest, Bindings, LocalFuncHandler) ->
%%	{value,bool(),NewBindings}.
%%  Evaluate one guard test.  This should never fail, just return true
%%  or false.  We DEMAND that this is valid guard test.

guard_test({call,_,{atom,_,Name},As0}, Bs0, Lf) ->
    case catch expr_list(As0, Bs0, Lf) of
	{As1,Bs1} -> {value,type_test(Name, As1),Bs1};
	Other -> {value,false,Bs0}
    end;
guard_test({op,_,Op,Lhs0,Rhs0}, Bs0, Lf) ->
    case catch begin
		   {[Lhs,Rhs],Bs1} = expr_list([Lhs0,Rhs0], Bs0, Lf),
		   {value,eval_op(Op, Lhs, Rhs),Bs1}
	       end of
	{value,Bool,Bs2} -> {value,Bool,Bs2};
	Other -> {value,false,Bs0}
    end;
guard_test({atom,_,true}, Bs, Lf) -> {value,true,Bs}.

type_test(integer, [A]) when integer(A) -> true;
type_test(float, [A]) when float(A) -> true;
type_test(number, [A]) when number(A) -> true;
type_test(atom, [A]) when atom(A) -> true;
type_test(constant, [A]) when constant(A) -> true;
type_test(list, [A]) when list(A) -> true;
type_test(tuple, [A]) when tuple(A) -> true;
type_test(pid, [A]) when pid(A) -> true;
type_test(reference, [A]) when reference(A) -> true;
type_test(port, [A]) when port(A) -> true;
type_test(record, [R,A]) when atom(R), element(1, A) == R -> true;
type_test(function, [A]) when function(A) -> true;
type_test(binary, [A]) when binary(A) -> true;
type_test(_, _) -> false.

%% match(Pattern, Term, Bindings) ->
%%	{match,NewBindings} | nomatch
%%  Try to match Pattern against Term with the current bindings.

match(Pat, Term, Bs) ->
    catch match1(Pat, Term, Bs).

string_to_conses([], Line, Tail) ->
    Tail;
string_to_conses([E|Rest], Line, Tail) ->
    {cons, Line, {integer, Line, E}, string_to_conses(Rest, Line, Tail)}.

match1({atom,_,A}, A, Bs) ->
    {match,Bs};
match1({integer,_,I}, I, Bs) ->
    {match,Bs};
match1({float,_,F}, F, Bs) ->
    {match,Bs};
match1({char,_,C}, C, Bs) ->
    {match,Bs};
match1({var,_,'_'}, _, Bs) ->			%Anonymous variable matches
    {match,Bs};					% everything, no new bindings
match1({var,_,Name}, Term, Bs) ->
    case binding(Name, Bs) of
	{value,Term} ->
	    {match,Bs};
	{value,V} ->
	    throw(nomatch);
	unbound ->
	    {match,add_binding(Name, Term, Bs)}
    end;
match1({match,Line,Pat1,Pat2}, Term, Bs0) ->
    {match, Bs1} = match1(Pat1, Term, Bs0),
    match1(Pat2, Term, Bs1);
match1({string,_,S}, S, Bs) ->
    {match,Bs};
match1({nil,_}, [], Bs) ->
    {match,Bs};
match1({cons,_,H,T}, [H1|T1], Bs0) ->
    {match,Bs} = match1(H, H1, Bs0),
    match1(T, T1, Bs);
match1({tuple,_,Elts}, Tuple, Bs) when length(Elts) == size(Tuple) ->
    match_tuple(Elts, Tuple, 1, Bs);
match1({bin, _, Fs}, B, Bs0) when binary(B) ->
    eval_bits:match_bits(Fs, B, Bs0,
			 fun(L, R, Bs) -> match1(L, R, Bs) end,
			 fun(E, Bs) -> expr(E, Bs, none) end,
			 true);
match1({op,Line,'++',{nil,_},R}, Term, Bs) ->
    match1(R, Term, Bs);
match1({op,_,'++',{cons,Li,{integer,L2,I},T},R}, Term, Bs) ->
    match1({cons,Li,{integer,L2,I},{op,Li,'++',T,R}}, Term, Bs);
match1({op,_,'++',{string,Li,L},R}, Term, Bs) ->
    match1(string_to_conses(L, Li, R), Term, Bs);
match1({op,Line,Op,A}, Term, Bs) ->
    case partial_eval({op,Line,Op,A}) of
	{op,Line,Op,A} ->
	    throw(nomatch);
	X ->
	    match1(X, Term, Bs)
    end;
match1({op,Line,Op,L,R}, Term, Bs) ->
    case partial_eval({op,Line,Op,L,R}) of
	{op,Line,Op,L,R} ->
	    throw(nomatch);
	X ->
	    match1(X, Term, Bs)
    end;
match1(_, _, _) ->
    throw(nomatch).

match_tuple([E|Es], Tuple, I, Bs0) ->
    {match,Bs} = match1(E, element(I, Tuple), Bs0),
    match_tuple(Es, Tuple, I+1, Bs);
match_tuple([], _, _, Bs) ->
    {match,Bs}.

%% match_list(PatternList, TermList, Bindings) ->
%%	{match,NewBindings} | nomatch
%%  Try to match a list of patterns against a list of terms with the
%%  current bindings.

match_list(Ps, Ts, Bs) ->
    catch match_list1(Ps, Ts, Bs).

match_list1([P|Ps], [T|Ts], Bs0) ->
    case match(P, T, Bs0) of
	{match,Bs1} -> match_list1(Ps, Ts, Bs1);
	nomatch -> throw(nomatch)
    end;
match_list1([], [], Bs) ->
    {match,Bs}.

%% new_bindings()
%% bindings(Bindings)
%% binding(Name, Bindings)
%% add_binding(Name, Value, Bindings)
%% del_binding(Name, Bindings)

new_bindings() -> orddict:new().

bindings(Bs) -> orddict:dict_to_list(Bs).

binding(Name, Bs) ->
    case orddict:find(Name, Bs) of
	{ok,Val} -> {value,Val};
	error -> unbound
    end.

add_binding(Name, Val, Bs) -> orddict:store(Name, Val, Bs).

del_binding(Name, Bs) -> orddict:erase(Name, Bs).

add_bindings(Bs1, Bs2) ->
    foldl(fun ({Name,Val}, Bs) -> orddict:store(Name, Val, Bs) end,
	  Bs2, orddict:dict_to_list(Bs1)).

merge_bindings(Bs1, Bs2) ->
    foldl(fun ({Name,Val}, Bs) ->
		  case orddict:find(Name, Bs) of
		      {ok,Val} -> Bs;		%Already with SAME value
		      {ok,V1} -> exit({{badmatch,V1},[{erl_eval,expr,3}]});
		      error -> orddict:store(Name, Val, Bs)
		  end end,
	  Bs2, orddict:dict_to_list(Bs1)).


%%----------------------------------------------------------------------------
%%
%% Evaluate expressions:
%% constants and 
%% op A
%% L op R
%% Things that evaluate to constants are accepted
%% and guard_bifs are allowed in constant expressions
%%----------------------------------------------------------------------------

is_constant_expr(Expr) ->
    case eval_expr(Expr) of
        {ok, X} when number(X) -> true;
        _ -> false
    end.

eval_expr(Expr) ->
    case catch ev_expr(Expr) of
        X when integer(X) -> {ok, X};
        X when float(X) -> {ok, X};
        X when atom(X) -> {ok,X};
        {'EXIT',Reason} -> {error, Reason};
        _ -> {error, badarg}
    end.

partial_eval(Expr) ->
    Line = line(Expr),
    case catch ev_expr(Expr) of
	X when integer(X) -> ret_expr(Expr,{integer,Line,X});
	X when float(X) -> ret_expr(Expr,{float,Line,X});
	X when atom(X) -> ret_expr(Expr,{atom,Line,X});
	_ ->
	    Expr
    end.

ev_expr({op,Ln,Op,L,R}) -> eval(Op, ev_expr(L), ev_expr(R));
ev_expr({op,Ln,Op,A}) -> eval(Op, ev_expr(A));
ev_expr({integer,_,X}) -> X;
ev_expr({float,_,X})   -> X;
ev_expr({atom,_,X})    -> X;
ev_expr({tuple,_,Es}) ->
    list_to_tuple(lists:map(fun(X) -> ev_expr(X) end, Es));
ev_expr({nil,_}) -> [];
ev_expr({cons,_,H,T}) -> [ev_expr(H) | ev_expr(T)].
%ev_expr({call,Line,{atom,_,F},As}) ->
%    true = erl_internal:guard_bif(F, length(As)),
%    apply(erlang, F, lists:map(fun(X) -> ev_expr(X) end, As));
%ev_expr({call,Line,{remote,_,{atom,_,erlang},{atom,_,F}},As}) ->
%    true = erl_internal:guard_bif(F, length(As)),
%    apply(erlang, F, lists:map(fun(X) -> ev_expr(X) end, As)).

				    
%% (we can use 'apply' here now, instead of enumerating the operators)

eval('+', X, Y) -> X + Y;
eval('-', X, Y) -> X - Y;
eval('*', X, Y) -> X * Y;
eval('/', X, Y) -> X / Y;
eval('div', X, Y) -> X div Y;
eval('rem', X, Y) -> X rem Y;
eval('band', X, Y) -> X band Y;
eval('bor', X, Y) -> X bor Y;
eval('bxor', X, Y) -> X bxor Y;
eval('bsl', X, Y) -> X bsl Y;
eval('bsr', X, Y) -> X bsr Y;
eval('and', X, Y) -> X and Y;
eval('or',  X, Y) -> X or Y;
eval('xor', X, Y) -> X xor Y;
eval('==',  X, Y) -> X == Y;
eval('/=',  X, Y) -> X /= Y;
eval('=<',  X, Y) -> X =< Y;
eval('<',   X, Y) -> X < Y;
eval('>=',  X, Y) -> X >= Y;
eval('>',   X, Y) -> X > Y;
eval('=:=', X, Y) -> X =:= Y;
eval('=/=', X, Y) -> X =/= Y;
eval('++', X, Y) -> X ++ Y;
eval('--', X, Y) -> X -- Y.

eval('+', X) -> 0 + X;
eval('-', X) -> 0 - X;
eval('bnot', X) -> bnot X;
eval('not', X) ->  not X.

ret_expr(Old, New) ->
%%    io:format("~w: reduced ~s => ~s~n",
%%	      [line(Old), erl_pp:expr(Old), erl_pp:expr(New)]),
    New.

line(Expr) -> element(2, Expr).
