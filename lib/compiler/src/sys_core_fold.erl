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
%% Purpose : Constant folding optimisation for Core

%% Propagate atomic values and fold in values of safe calls to
%% constant arguments.  Also detect and remove literals which are
%% ignored in a 'seq'.  Could handle lets better by chasing down
%% complex 'arg' expressions and finding values.
%%
%% It would be possible to extend to replace repeated evaluation of
%% "simple" expressions by the value (variable) of the first call.
%% For example, after a "let Z = X+1" then X+1 would be replaced by Z
%% where X is valid.  The Vsb uses the full Core expression as key.
%% It would complicate handling of patterns as we would have to remove
%% all values where the key contains pattern variables.

-module(sys_core_fold).

-export([module/2]).

-import(lists, [map/2,foldl/3,all/2]).

-include("core_parse.hrl").

%% Variable value info.
-record(v, {sub=dict:new()}).			%Variable substitutions

module(#c_mdef{body=B0}=Mod, Opts) ->
    B1 = map(fun function/1, B0),
    {ok,Mod#c_mdef{body=B1}}.

function(#c_fdef{body=B0}=Fdef) ->
    B1 = expr(B0, #v{}),			%This must be a fun!
    Fdef#c_fdef{body=B1}.

%% body(Expr, Vsb) -> Expr.
%%  No special handling of anything except vectors.

body(#c_vector{anno=A,es=Es0}, Vsb) ->
    Es1 = expr_list(Es0, Vsb),
    #c_vector{anno=A,es=Es1};
body(E, Vsb) -> expr(E, Vsb).

%% expr(Expr, Vsb) -> Expr.

expr(#c_var{}=V, Vsb) ->
    get_vsub(V, Vsb);
expr(#c_int{}=I, Vsb) -> I;
expr(#c_float{}=F, Vsb) -> F;
expr(#c_atom{}=A, Vsb) -> A;
expr(#c_char{}=C, Vsb) -> C;
expr(#c_string{}=S, Vsb) -> S;
expr(#c_nil{}=N, Vsb) -> N;
expr(#c_cons{anno=A,head=H,tail=T}, Vsb) ->
    #c_cons{anno=A,head=expr(H, Vsb),tail=expr(T, Vsb)};
expr(#c_tuple{anno=A,es=Es}, Vsb) ->
    #c_tuple{anno=A,es=expr_list(Es, Vsb)};
expr(#c_let{anno=A,vars=Vs0,arg=Arg0,body=B0}=Let, Vsb0) ->
    Arg1 = body(Arg0, Vsb0),			%This is a body
    %% Remove substitutions for variables in pattern (they shadow),
    %% then add new substitutions.
    Vsb1 = match_pats(Vs0, Vsb0),
    {Vs1,Args,Vsb2} = let_substs(Vs0, Arg1, Vsb1),
    B1 = body(B0, Vsb2),
    %% Optimise away let if no values remain to be set.
    if Vs1 == [] -> B1;
       true ->
	    Let#c_let{vars=make_values(Vs1),
		      arg=make_values(Args),
		      body=B1}
    end;
expr(#c_seq{arg=Arg0,body=B0}=Seq, Vsb) ->
    Arg1 = expr(Arg0, Vsb),
    B1 = body(B0, Vsb),
    %% Optimise away literal arg as its value is ignored.
    case is_literal(Arg1) of
	true -> B1;
	false -> Seq#c_seq{arg=Arg1,body=B1}
    end;
expr(#c_fun{vars=Vs,body=B0}=Fun, Vsb0) ->
    Vsb1 = pattern_list(Vs, Vsb0),
    B1 = body(B0, Vsb1),
    Fun#c_fun{body=B1};
expr(#c_case{arg=Arg0,clauses=Cs0}=Case, Vsb) ->
    Arg1 = body(Arg0, Vsb),
    Cs1 = clauses(Cs0, Vsb),
    Case#c_case{arg=Arg1,clauses=Cs1};
expr(#c_receive{clauses=Cs0,timeout=T0,action=A0}=Recv, Vsb) ->
    Cs1 = clauses(Cs0, Vsb),
    T1 = expr(T0, Vsb),
    A1 = body(A0, Vsb),
    Recv#c_receive{clauses=Cs1,timeout=T1,action=A1};
expr(#c_catch{body=B0}=Catch, Vsb) ->
    B1 = body(B0, Vsb),
    Catch#c_catch{body=B1};
expr(#c_call{anno=A,op=Op0,args=As0}=Call, Vsb) ->
    Op1 = call_op(Op0, Vsb),
    As1 = expr_list(As0, Vsb),
    call(A, Op1, As1);
expr(#c_local{}=Loc, Vsb) -> Loc.

expr_list(Es, Vsb) ->
    map(fun (E) -> expr(E, Vsb) end, Es).

%% call_op(Op) -> Op.
%%  Fold call op.  Remotes and internals can only exist here.

call_op(#c_remote{}=Rem, Vsb) -> Rem;
call_op(#c_internal{}=Int, Vsb) -> Int;
call_op(Expr, Vsb) -> expr(Expr, Vsb).

%% call(Anno, Op, Args) -> Expr.
%%  Try to safely evaluate the call.

call(A, #c_remote{mod=erlang,name=N,arity=Arity}=Op, [Arg]=Args) ->
    case catch {ok,make_literal(eval_call(N, literal_value(Arg)))} of
	{ok,Val} -> Val;
	Other -> #c_call{anno=A,op=Op,args=Args}
    end;
call(A, #c_remote{mod=erlang,name=N,arity=Arity}=Op, [Arg1,Arg2]=Args) ->
    case catch {ok,make_literal(eval_call(N,
					  literal_value(Arg1),
					  literal_value(Arg2)))} of
	{ok,Val} -> Val;
	Other -> #c_call{anno=A,op=Op,args=Args}
    end;
call(A, Op, Args) -> #c_call{anno=A,op=Op,args=Args}.

%% eval_call(Op, Arg) -> Value.
%% eval_call(Op, Arg1, Arg2) -> Value.
%%  Evaluate safe calls.  We only do arithmetic and logical operators,
%%  there are more but these are the ones that are probably
%%  worthwhile.  It would be MUCH easier if we could apply these!

eval_call('+', X) -> 0 + X;
eval_call('-', X) -> 0 - X;
eval_call('bnot', X) -> bnot X;
eval_call(abs, A) -> abs(A);
eval_call(float, A) -> float(A);
eval_call(round, A) -> round(A);
eval_call(trunc, A) -> trunc(A);
eval_call('not', X) -> not X;
eval_call(hd, L) -> hd(L);
eval_call(tl, L) -> tl(L);
eval_call(length, L) -> length(L);
eval_call(size, T) -> size(T);
eval_call(integer_to_list, I) -> integer_to_list(I);
eval_call(list_to_integer, L) -> list_to_integer(L);
eval_call(float_to_list, F) -> float_to_list(F);
eval_call(list_to_float, L) -> list_to_float(L);
eval_call(atom_to_list, A) -> atom_to_list(A);
eval_call(list_to_atom, L) -> list_to_atom(L);
eval_call(tuple_to_list, T) -> tuple_to_list(T);
eval_call(list_to_tuple, L) -> list_to_tuple(L).

eval_call('*', X, Y) -> X * Y;
eval_call('/', X, Y) -> X / Y;
eval_call('+', X, Y) -> X + Y;
eval_call('-', X, Y) -> X - Y;
eval_call('div', X, Y) -> X div Y;
eval_call('rem', X, Y) -> X rem Y;
eval_call('band', X, Y) -> X band Y;
eval_call('bor', X, Y) -> X bor Y;
eval_call('bxor', X, Y) -> X bxor Y;
eval_call('bsl', X, Y) -> X bsl Y;
eval_call('bsr', X, Y) -> X bsr Y;
eval_call('and', X, Y) -> X and Y;
eval_call('or',  X, Y) -> X or Y;
eval_call('xor', X, Y) -> X xor Y;
eval_call('=:=',  X, Y) -> X =:= Y;
eval_call('=/=',  X, Y) -> X =/= Y;
eval_call('==',  X, Y) -> X == Y;
eval_call('/=',  X, Y) -> X /= Y;
eval_call('=<',  X, Y) -> X =< Y;
eval_call('<',   X, Y) -> X < Y;
eval_call('>=',  X, Y) -> X >= Y;
eval_call('>',   X, Y) -> X > Y;
eval_call('++', X, Y) -> X ++ Y;
eval_call('--', X, Y) -> X -- Y;
eval_call(element, X, Y) -> element(X, Y).

%% literal_value(LitExpr) -> Value.
%%  Return the value of LitExpr.  This is just core_parse:normalise/1.

literal_value(#c_int{val=I}) -> I;
literal_value(#c_float{val=F}) -> F;
literal_value(#c_atom{name=A}) -> A;
literal_value(#c_char{val=C}) -> C;
literal_value(#c_string{val=S}) -> S;
literal_value(#c_nil{}) -> [];
literal_value(#c_cons{head=H,tail=T}) ->
    [literal_value(H)|literal_value(T)];
literal_value(#c_tuple{es=Es}) ->
    list_to_tuple(literal_value_list(Es)).

literal_value_list(Vals) -> map(fun literal_value/1, Vals).

%% make_literal(Value) -> LitExpr.
%%  Make a literal expression from an Erlang value.  This is just
%%  core_parse:abstract/1.

make_literal(I) when integer(I) -> #c_int{val=I};
make_literal(F) when float(F) -> #c_float{val=F};
make_literal(A) when atom(A) -> #c_atom{name=A};
make_literal([]) -> #c_nil{};
make_literal([H|T]) ->
    #c_cons{head=make_literal(H),tail=make_literal(T)};
make_literal(T) when tuple(T) ->
    #c_tuple{es=make_literal_list(tuple_to_list(T))}.

make_literal_list(Vals) -> map(fun make_literal/1, Vals). 

clauses(Cs, Vsb) ->
    map(fun (C) -> clause(C, Vsb) end, Cs).

clause(#c_clause{pat=P,guard=G0,body=B0}=Cl, Vsb0) ->
    Vsb1 = match_pats(P, Vsb0),
    G1 = expr(G0, Vsb1),
    B1 = body(B0, Vsb1),
    Cl#c_clause{guard=G1,body=B1}.

%% match_pats(MatchPat, Vsb) -> Vsb.
%%  Delete all variables in MatchPat in Vsb.

match_pats(#c_vector{es=Ps}=Pats, Vsb) -> pattern_list(Ps, Vsb);
match_pats(P, Vsb) -> pattern(P, Vsb).

%% let_substs(LetVars, MatchVal, Vsb) -> Vsb.
%%  Add suitable substitutions to Vsb of variables in LetVars.

let_substs(#c_vector{es=Ps}, #c_vector{es=Vs}, Vsb) ->
    let_subst_list(Ps, Vs, Vsb);
let_substs(#c_vector{es=[P]}, V, Vsb) -> let_subst_list([P], [V], Vsb);
let_substs(P, #c_vector{es=[V]}, Vsb) -> let_subst_list([P], [V], Vsb);
let_substs(P, V, Vsb) -> let_subst_list([P], [V], Vsb).

let_subst_list([P|Ps0], [V|Vs0], Vsb0) ->
    case is_subst(V) of
	true -> let_subst_list(Ps0, Vs0, add_vsub(P, V, Vsb0));
	false ->
	    {Ps1,Vs1,Vsb1} = let_subst_list(Ps0, Vs0, Vsb0),
	    {[P|Ps1],[V|Vs1],Vsb1}
    end;
let_subst_list([], [], Vsb) -> {[],[],Vsb}.

%% pattern(Pattern, Vsb) -> Vsb.
%%  Delete all variables in Pattern in Vsb.

pattern(#c_var{}=V, Vsb) ->
    del_vsub(V, Vsb);
pattern(#c_int{}, Vsb) -> Vsb;
pattern(#c_float{}, Vsb) -> Vsb;
pattern(#c_atom{}, Vsb) -> Vsb;
pattern(#c_char{}, Vsb) -> Vsb;
pattern(#c_string{}, Vsb) -> Vsb;
pattern(#c_nil{}, Vsb) -> Vsb;
pattern(#c_cons{head=H,tail=T}, Vsb) ->
    pattern(H, pattern(T, Vsb));
pattern(#c_tuple{es=Es}, Vsb) ->
    pattern_list(Es, Vsb);
pattern(#c_alias{var=V,pat=P}, Vsb) ->
    pattern(V, pattern(P, Vsb)).
    
pattern_list(Ps, Vsb0) ->
    foldl(fun (P, Vsb) -> pattern(P, Vsb) end, Vsb0, Ps).

%% is_subst(Expr) -> true | false.
%%  Test whether an expression is a suitable substitution.

is_subst(#c_tuple{es=[]}) -> true;		%The empty tuple
is_subst(E) -> is_atomic(E).

%% is_atomic(Expr) -> true | false.

is_atomic(#c_var{}) -> true;
is_atomic(#c_int{}) -> true;
is_atomic(#c_float{}) -> true;
is_atomic(#c_atom{}) -> true;
is_atomic(#c_char{}) -> true;
is_atomic(#c_string{}) -> true;
is_atomic(#c_nil{}) -> true;
is_atomic(E) -> false.

%% is_literal(Expr) -> true | false.

is_literal(#c_cons{head=H,tail=T}) ->
    case is_literal(H) of
	true -> is_literal(T); 
	false -> false
    end;
is_literal(#c_tuple{es=Es}) ->
    all(fun is_literal/1, Es);
is_literal(E) -> is_atomic(E).

%% get_vsub(Var, Vsub) -> Value.
%% add_vsub(Var, Value, Vsub) -> Vsub.
%% del_vsub(Var, Vsub) -> Vsub.

get_vsub(V, #v{sub=S}) ->
    case dict:find(V, S) of
	{ok,Val} -> Val;
	error -> V
    end.

add_vsub(V, Val, #v{sub=S}) ->
    #v{sub=dict:store(V, Val, S)}.

del_vsub(V, #v{sub=S}) ->
    #v{sub=dict:erase(V, S)}.

%% make_values(Expr) -> Expr | #c_vector{}.
%%  Make a suitable values structure, expr or vector, depending on
%%  Expr.

make_values([P]) -> P;
make_values(Ps) when list(Ps) -> #c_vector{es=Ps};
make_values(P) -> P.
