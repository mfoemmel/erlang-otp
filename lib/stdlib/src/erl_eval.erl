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
-module(erl_eval).

%% An evaluator for Erlang abstract syntax.

-export([exprs/2,exprs/3,exprs/4,expr/2,expr/3,expr/4,
         expr_list/2,expr_list/3,expr_list/4]).
-export([new_bindings/0,bindings/1,binding/2,add_binding/3,del_binding/2]).

-export([is_constant_expr/1, partial_eval/1]).

%% Is used by standalone Erlang (escript).
-export([match_clause/4]).

%% The following exports are here for backwards compatibility.
-export([seq/2,seq/3,arg_list/2,arg_list/3]).
-deprecated([{seq,2},{seq,3},{arg_list,2},{arg_list,3}]).

-export([check_command/2, fun_data/1]).

-import(lists, [reverse/1,flatmap/2,foldl/3,member/2,map/2]).

%% seq(ExpressionSeq, Bindings)
%% seq(ExpressionSeq, Bindings, LocalFuncHandler)
%% arg_list(ExpressionList, Bindings)
%% arg_list(ExpressionList, Bindings, LocalFuncHandler)
%%  These calls are here for backwards compatibility.

seq(Exprs, Bs)     -> exprs(Exprs, Bs).
seq(Exprs, Bs, Lf) -> exprs(Exprs, Bs, Lf).

arg_list(Es, Bs)     -> expr_list(Es, Bs).
arg_list(Es, Bs, Lf) -> expr_list(Es, Bs, Lf).

%% exprs(ExpressionSeq, Bindings)
%% exprs(ExpressionSeq, Bindings, LocalFuncHandler)
%% exprs(ExpressionSeq, Bindings, LocalFuncHandler, ExternalFuncHandler)
%%  Returns:
%%	{value,Value,NewBindings}
%%    or {'EXIT', Reason}
%% Only exprs/2 checks the command by calling erl_lint. The reason is
%% that if there is a function handler present, then it is possible
%% that there are valid constructs in Expression to be taken care of
%% by a function handler but considerad errors by erl_lint.

exprs(Exprs, Bs) ->
    case check_command(Exprs, Bs) of
        ok -> 
            RBs = none,
            exprs(Exprs, Bs, none, none, RBs);
        {error,{_Line,_Mod,Error}} ->
	    exit({Error,[{?MODULE,exprs,2}]})
    end.

exprs(Exprs, Bs, Lf) ->
    RBs = none,
    exprs(Exprs, Bs, Lf, none, RBs).

exprs(Exprs, Bs, Lf, Ef) ->
    RBs = none,
    exprs(Exprs, Bs, Lf, Ef, RBs).

exprs([E], Bs0, Lf, Ef, RBs) ->
    expr(E, Bs0, Lf, Ef, RBs);
exprs([E|Es], Bs0, Lf, Ef, RBs) ->
    RBs1 = none,
    {value,_V,Bs} = expr(E, Bs0, Lf, Ef, RBs1),
    exprs(Es, Bs, Lf, Ef, RBs).

%% expr(Expression, Bindings)
%% expr(Expression, Bindings, LocalFuncHandler)
%% expr(Expression, Bindings, LocalFuncHandler, ExternalFuncHandler)
%%  Returns:
%%	 {value,Value,NewBindings}
%%    or {'EXIT', Reason}
%%
%% Only expr/2 checks the command by calling erl_lint. See exprs/2.

expr(E, Bs) ->
    case check_command([E], Bs) of
        ok -> 
            RBs = none,
            expr(E, Bs, none, none, RBs);
        {error,{_Line,_Mod,Error}} ->
	    exit({Error,[{?MODULE,expr,2}]})
    end.

expr(E, Bs, Lf) ->
    RBs = none,
    expr(E, Bs, Lf, none, RBs).

expr(E, Bs, Lf, Ef) ->
    RBs = none,
    expr(E, Bs, Lf, Ef, RBs).

%% Check a command (a list of expressions) by calling erl_lint.
%%
%% There may be bindings {module,A} in Bs (put there by shell.erl),
%% but they are harmless.

check_command(Es, Bs) ->
    case erl_lint:exprs(Es, bindings(Bs)) of
        {ok,_Ws} ->
            ok;
        {error,[{_File,[Error|_]}],_Ws} ->
            {error,Error}
    end.

%% Check whether a term F is a function created by this module.
%% Returns 'false' if not, otherwise {fun_data,Imports,Clauses}.

fun_data(F) when is_function(F) ->
    case erlang:fun_info(F, module) of
        {module,erl_eval} ->
            {env, [FBs,_FEf,_FLf,FCs]} = erlang:fun_info(F, env),
            {fun_data,FBs,FCs};
        _ ->
            false
    end;
fun_data(_T) ->
    false.

%% {?MODULE,expr,3} is still returned when calling exit/1, dispite the
%% fact that expr now takes five arguments...

expr({var,_,V}, Bs, _Lf, _Ef, RBs) ->
    case binding(V, Bs) of
	{value,Val} ->
            ret_expr(Val, Bs, RBs);
	unbound -> % Should not happen.
	    exit({{unbound,V},[{?MODULE,expr,3}]})
    end;
expr({char,_,C}, Bs, _Lf, _Ef, RBs) ->
    ret_expr(C, Bs, RBs);
expr({integer,_,I}, Bs, _Lf, _Ef, RBs) ->
    ret_expr(I, Bs, RBs);
expr({float,_,F}, Bs, _Lf, _Ef, RBs) ->
    ret_expr(F, Bs, RBs);
expr({atom,_,A}, Bs, _Lf, _Ef, RBs) ->
    ret_expr(A, Bs, RBs);
expr({string,_,S}, Bs, _Lf, _Ef, RBs) ->
    ret_expr(S, Bs, RBs);
expr({nil, _}, Bs, _Lf, _Ef, RBs) ->
    ret_expr([], Bs, RBs);
expr({cons,_,H0,T0}, Bs0, Lf, Ef, RBs) ->
    {value,H,Bs1} = expr(H0, Bs0, Lf, Ef, none),
    {value,T,Bs2} = expr(T0, Bs0, Lf, Ef, none),
    ret_expr([H|T], merge_bindings(Bs1, Bs2), RBs);
expr({lc,_,E,Qs}, Bs, Lf, Ef, RBs) ->
    eval_lc(E, Qs, Bs, Lf, Ef, RBs);
expr({tuple,_,Es}, Bs0, Lf, Ef, RBs) ->
    {Vs,Bs} = expr_list(Es, Bs0, Lf, Ef),
    ret_expr(list_to_tuple(Vs), Bs, RBs);
expr({record_field,_,_,_}=Mod, Bs, _Lf, _Ef, RBs) ->
    case expand_module_name(Mod, Bs) of
	{atom,_,A} ->
	    ret_expr(A, Bs, RBs);    %% This is the "x.y" syntax
	_ ->
	    exit({{badexpr, '.'},[{erl_eval,expr,3}]})
    end;
expr({record_field,_,_,Name,_}, _Bs, _Lf, _Ef, _RBs) ->
    exit({{undef_record,Name},[{erl_eval,expr,3}]});
expr({record_index,_,Name,_}, _Bs, _Lf, _Ef, _RBs) ->
    exit({{undef_record,Name},[{erl_eval,expr,3}]});
expr({record,_,Name,_}, _Bs, _Lf, _Ef, _RBs) ->
    exit({{undef_record,Name},[{erl_eval,expr,3}]});
expr({record,_,_,Name,_}, _Bs, _Lf, _Ef, _RBs) ->
    exit({{undef_record,Name},[{erl_eval,expr,3}]});
expr({block,_,Es}, Bs, Lf, Ef, RBs) ->
    exprs(Es, Bs, Lf, Ef, RBs);
expr({'if',_,Cs}, Bs, Lf, Ef, RBs) ->
    if_clauses(Cs, Bs, Lf, Ef, RBs);
expr({'case',_,E,Cs}, Bs0, Lf, Ef, RBs) ->
    {value,Val,Bs} = expr(E, Bs0, Lf, Ef, none),
    case_clauses(Val, Cs, Bs, Lf, Ef, RBs);
expr({'try',_,_Es,_Scs,_Ccs}, _Bs0, _Lf, _Ef, _RBs) ->
    exit({try_not_yet_implemented,[{erl_eval,expr,3}]});
%expr({'try',_,Es,Scs,Ccs}, Bs0, Lf, Ef, RBs) ->
%    try exprs(Es, Bs0, Lf, Ef, RBs) of
%        {value,Val,Bs} when [] =/= Scs ->
%           case_clauses(Val, Scs, Bs, Lf, Ef, RBs);
%        {value,V,Bs} ->
%            ret_expr(V, Bs, RBs)
%    catch Exception ->
%        case_clauses(Exception, Ccs, Bs0, Lf, Ef, RBs)
%    end;
expr({'receive',_,Cs}, Bs, Lf, Ef, RBs) ->
    receive_clauses(Cs, Bs, Lf, Ef, [], RBs);
expr({'receive',_, Cs, E, TB}, Bs0, Lf, Ef, RBs) ->
    {value,T,Bs} = expr(E, Bs0, Lf, Ef, none),
    receive_clauses(T, Cs, {TB,Bs}, Bs0, Lf, Ef, [], RBs);
expr({'fun',_Line,{function,Name,Arity}}, _Bs0, _Lf, _Ef, _RBs) -> % R8
    exit({undef,[{erl_eval,Name,Arity}]});
expr({'fun',Line,{clauses,Cs}} = Ex, Bs, Lf, Ef, RBs) ->
    %% Save only used variables in the function environment.
    {ok,Used} = erl_lint:used_vars([Ex], Bs),
    En = orddict:filter(fun(K,_V) -> member(K,Used) end, Bs),
    %% This is a really ugly hack!
    F = 
    case length(element(3,hd(Cs))) of
	0 -> fun () -> eval_fun(Cs, [], En, Lf, Ef) end;
	1 -> fun (A) -> eval_fun(Cs, [A], En, Lf, Ef) end;
	2 -> fun (A,B) -> eval_fun(Cs, [A,B], En, Lf, Ef) end;
	3 -> fun (A,B,C) -> eval_fun(Cs, [A,B,C], En, Lf, Ef) end;
	4 -> fun (A,B,C,D) -> eval_fun(Cs, [A,B,C,D], En, Lf, Ef) end;
	5 -> fun (A,B,C,D,E) -> eval_fun(Cs, [A,B,C,D,E], En, Lf, Ef) end;
	6 -> fun (A,B,C,D,E,F) -> eval_fun(Cs, [A,B,C,D,E,F], En, Lf, Ef) end;
	7 -> fun (A,B,C,D,E,F,G) -> 
           eval_fun(Cs, [A,B,C,D,E,F,G], En, Lf, Ef) end;
	8 -> fun (A,B,C,D,E,F,G,H) -> 
           eval_fun(Cs, [A,B,C,D,E,F,G,H], En, Lf, Ef) end;
	9 -> fun (A,B,C,D,E,F,G,H,I) -> 
           eval_fun(Cs, [A,B,C,D,E,F,G,H,I], En, Lf, Ef) end;
	10 -> fun (A,B,C,D,E,F,G,H,I,J) -> 
           eval_fun(Cs, [A,B,C,D,E,F,G,H,I,J], En, Lf, Ef) end;
	11 -> fun (A,B,C,D,E,F,G,H,I,J,K) -> 
           eval_fun(Cs, [A,B,C,D,E,F,G,H,I,J,K], En, Lf, Ef) end;
	12 -> fun (A,B,C,D,E,F,G,H,I,J,K,L) -> 
           eval_fun(Cs, [A,B,C,D,E,F,G,H,I,J,K,L], En, Lf, Ef) end;
	13 -> fun (A,B,C,D,E,F,G,H,I,J,K,L,M) -> 
           eval_fun(Cs, [A,B,C,D,E,F,G,H,I,J,K,L,M], En, Lf, Ef) end;
	14 -> fun (A,B,C,D,E,F,G,H,I,J,K,L,M,N) -> 
           eval_fun(Cs, [A,B,C,D,E,F,G,H,I,J,K,L,M,N], En, Lf, Ef) end;
	15 -> fun (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O) -> 
           eval_fun(Cs, [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O], En, Lf, Ef) end;
	16 -> fun (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P) -> 
           eval_fun(Cs, [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P], En, Lf, Ef) end;
	17 -> fun (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q) -> 
           eval_fun(Cs, [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q], En, Lf, Ef) end;
	18 -> fun (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R) -> 
           eval_fun(Cs, [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R], En, Lf, Ef) end;
	19 -> fun (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S) -> 
           eval_fun(Cs, [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S], 
                    En, Lf, Ef) end;
	20 -> fun (A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T) -> 
           eval_fun(Cs, [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T], 
                    En, Lf, Ef) end;
	_Other ->
	    exit({{'argument_limit',{'fun',Line,Cs}},[{erl_eval,expr,3}]})
    end,
    ret_expr(F, Bs, RBs);
expr({call,_,{remote,_,Mod,Func},As0}, Bs0, Lf, Ef, RBs) ->
    Mod1 = expand_module_name(Mod, Bs0),
    {value,M,Bs1} = expr(Mod1, Bs0, Lf, Ef, none),
    {value,F,Bs2} = expr(Func, Bs0, Lf, Ef, none),
    {As,Bs3} = expr_list(As0, merge_bindings(Bs1, Bs2), Lf, Ef),
    do_apply({M,F}, As, Bs3, Ef, RBs);
expr({call,_,Func0,As0}, Bs0, Lf, Ef, RBs) ->
    {value,Func,Bs1} = expr(Func0, Bs0, Lf, Ef, none),
    if
        is_atom(Func) ->
	    case erl_internal:bif(Func, length(As0)) of
		true ->
		    {As,Bs2} = expr_list(As0, Bs1, Lf, Ef),
                    bif(Func, As, Bs2, Ef, RBs);
		false ->
                    local_func(Func, As0, Bs1, Lf, RBs)
	    end;
        true -> % function or {Mod,Fun}
	    {As,Bs2} = expr_list(As0, Bs1, Lf, Ef),
            do_apply(Func, As, Bs2, Ef, RBs)
    end;
expr({'catch',_,Expr}, Bs0, Lf, Ef, RBs) ->
    Ref = make_ref(),
    case catch {Ref,expr(Expr, Bs0, Lf, Ef, none)} of
	{Ref,{value,V,Bs}} ->	  % Nothing was thrown (guaranteed).
            ret_expr(V, Bs, RBs);
	Other ->
            ret_expr(Other, Bs0, RBs)
    end;
expr({match,_,Lhs,Rhs0}, Bs0, Lf, Ef, RBs) ->
    {value,Rhs,Bs1} = expr(Rhs0, Bs0, Lf, Ef, none),
    case match(Lhs, Rhs, Bs1) of
	{match,Bs} ->
            ret_expr(Rhs, Bs, RBs);
	nomatch ->
	    exit({{badmatch,Rhs},[{erl_eval,expr,3}]})
    end;
expr({op,_,Op,A0}, Bs0, Lf, Ef, RBs) ->
    {value,A,Bs} = expr(A0, Bs0, Lf, Ef, none),
    ret_expr(eval_op(Op, A), Bs, RBs);
expr({op,_,'andalso',L0,R0}, Bs0, Lf, Ef, RBs) ->
    {value,L,Bs1} = expr(L0, Bs0, Lf, Ef, none),
    V = case L of
	    true ->
		{value,R,_} = expr(R0, Bs1, Lf, Ef, none),
		case R of
		    true -> true;
		    false -> false;
		    _ -> exit({{badarg,R},[{erl_eval,expr,3}]})
		end;
	    false -> false;
	    _ -> exit({{badarg,L},[{erl_eval,expr,3}]})
	end,
    ret_expr(V, Bs1, RBs);
expr({op,_,'orelse',L0,R0}, Bs0, Lf, Ef, RBs) ->
    {value,L,Bs1} = expr(L0, Bs0, Lf, Ef, none),
    V = case L of
	    true -> true;
	    false ->
		{value,R,_} = expr(R0, Bs1, Lf, Ef, none),
		case R of
		    true -> true;
		    false -> false;
		    _ -> exit({{badarg,R},[{erl_eval,expr,3}]})
		end;
	    _ -> exit({{badarg,L},[{erl_eval,expr,3}]})
	end,
    ret_expr(V, Bs1, RBs);
expr({op,_,Op,L0,R0}, Bs0, Lf, Ef, RBs) ->
    {value,L,Bs1} = expr(L0, Bs0, Lf, Ef, none),
    {value,R,Bs2} = expr(R0, Bs0, Lf, Ef, none),
    ret_expr(eval_op(Op, L, R), merge_bindings(Bs1, Bs2), RBs);
expr({bin,_,Fs}, Bs0, Lf, Ef, RBs) ->
    {value, V, Bs} = 
        eval_bits:expr_grp(Fs,Bs0,
                           fun(E, B) -> expr(E, B, Lf, Ef, none) end,
                           [],
                           true),
    ret_expr(V, Bs, RBs);
expr({remote,_,_,_}, _Bs, _Lf, _Ef, _RBs) ->
    exit({{badexpr,':'},[{erl_eval,expr,3}]});
expr({value,_,Val}, Bs, _Lf, _Ef, RBs) ->    % Special case straight values.
    ret_expr(Val, Bs, RBs).

%% local_func(Function, Arguments, Bindings, LocalFuncHandler, RBs) ->
%%	{value,Value,Bindings} | Value when
%%	LocalFuncHandler = {value,F} | {value,F,Eas} |
%%                         {eval,F}  | {eval,F,Eas}  | none.

local_func(Func, As0, Bs0, {value,F}, RBs) when RBs == value ->
    {As1,_Bs1} = expr_list(As0, Bs0, {value,F}),
    %% Make tail recursive calls when possible.
    F(Func, As1);
local_func(Func, As0, Bs0, {value,F}, RBs) ->
    {As1,Bs1} = expr_list(As0, Bs0, {value,F}),
    ret_expr(F(Func, As1), Bs1, RBs);
local_func(Func, As0, Bs0, {value,F,Eas}, RBs) when RBs == value ->
    {As1,_Bs1} = expr_list(As0, Bs0, {value,F,Eas}),
    apply(F, [Func,As1|Eas]);
local_func(Func, As0, Bs0, {value,F,Eas}, RBs) ->
    {As1,Bs1} = expr_list(As0, Bs0, {value,F,Eas}),
    ret_expr(apply(F, [Func,As1|Eas]), Bs1, RBs);
local_func(Func, As, Bs, {eval,F}, RBs) ->
    local_func2(F(Func, As, Bs), RBs);
local_func(Func, As, Bs, {eval,F,Eas}, RBs) ->
    local_func2(apply(F, [Func,As,Bs|Eas]), RBs);
%% These two clauses are for backwards compatibility.
local_func(Func, As0, Bs0, {M,F}, RBs) ->
    {As1,Bs1} = expr_list(As0, Bs0, {M,F}),
    ret_expr(M:F(Func,As1), Bs1, RBs);
local_func(Func, As, _Bs, {M,F,Eas}, RBs) ->
    local_func2(apply(M, F, [Func,As|Eas]), RBs);
%% Default unknown function handler to undefined function.
local_func(Func, As0, _Bs0, none, _RBs) ->
    exit({undef,[{erl_eval,Func,length(As0)}]}).

local_func2({value,V,Bs}, RBs) ->
    ret_expr(V, Bs, RBs);
local_func2({eval,F,As,Bs}, RBs) -> % This reply is not documented.
    %% The shell found F. erl_eval tries to do a tail recursive call,
    %% something the shell cannot do. Do not use Ef here.
    do_apply(F, As, Bs, none, RBs).

%% bif(Name, Arguments, RBs)
%%  Evaluate the Erlang auto-imported function Name. erlang:apply/2,3
%%  are "hidden" from the external function handler.

bif(apply, [erlang,apply,As], Bs, Ef, RBs) ->
    bif(apply, As, Bs, Ef, RBs);
bif(apply, [M,F,As], Bs, Ef, RBs) ->
    do_apply({M,F}, As, Bs, Ef, RBs);
bif(apply, [F,As], Bs, Ef, RBs) ->
    do_apply(F, As, Bs, Ef, RBs);
bif(Name, As, Bs, Ef, RBs) ->
    do_apply({erlang,Name}, As, Bs, Ef, RBs).

%% do_apply(MF, Arguments, Bindings, ExternalFuncHandler, RBs) ->
%%	{value,Value,Bindings} | Value when
%%	ExternalFuncHandler = {value,F} | none.
%% MF is a tuple {Module,Function} or a fun.

do_apply(Func, As, Bs0, Ef, RBs) ->
    Info = if
               is_function(Func) -> erlang:fun_info(Func, module);
               true -> no
           end,
    case {Info,Ef} of
        {{module,?MODULE},_} ->
            {env, [FBs, FEf, FLf, FCs]} = erlang:fun_info(Func, env),
            %% If we are evaluting within another function body 
            %% (RBs =/= none), we return RBs when this function body
            %% has been evalutated, otherwise we return Bs0, the
            %% bindings when evalution of this function body started.
            NRBs = if
                       RBs == none -> Bs0;
                       true -> RBs
                   end,
            eval_fun(FCs, As, FBs, FLf, FEf, NRBs);
        {_,none} when RBs == value ->
            %% Make tail recursive calls when possible.
            apply(Func, As);
        {_,none} ->
            ret_expr(apply(Func, As), Bs0, RBs);
        {_,{value,F}} when RBs == value ->
            F(Func,As);
        {_,{value,F}} ->
            ret_expr(F(Func, As), Bs0, RBs)
    end.

%% eval_lc(Expr, [Qualifier], Bindings, LocalFunctionHandler, 
%%         ExternalFuncHandler, RetBindings) ->
%%	{value,Value,Bindings} | Value
%%  This is evaluating list comprehensions "straight out of the book".

eval_lc(E, Qs, Bs, Lf, Ef, RBs) ->
    ret_expr(eval_lc1(E, Qs, Bs, Lf, Ef), Bs, RBs).

eval_lc1(E, [{generate,_,P,L0}|Qs], Bs0, Lf, Ef) ->
    RBs = none,
    {value,L1,_Bs1} = expr(L0, Bs0, Lf, Ef, RBs),
    flatmap(fun (V) ->
		    case match(P, V, new_bindings(), Bs0) of
			{match,Bsn} ->
			    Bs2 = add_bindings(Bsn, Bs0),
			    eval_lc1(E, Qs, Bs2, Lf, Ef);
			nomatch -> []
		    end end, L1);
eval_lc1(E, [F|Qs], Bs0, Lf,  Ef) ->
    case erl_lint:is_guard_test(F) of
	true ->
	    case guard_test(F, Bs0, Lf, Ef) of
		{value,true,Bs1} -> eval_lc1(E, Qs, Bs1, Lf, Ef);
                {value,false,_} -> []
	    end;
	false ->
            RBs = none,
	    case expr(F, Bs0, Lf, Ef, RBs) of
		{value,true,Bs1} -> eval_lc1(E, Qs, Bs1, Lf, Ef);
		{value,false,_} -> [];
		_Other -> exit({bad_filter,[{erl_eval,expr,3}]})
	    end
    end;
eval_lc1(E, [], Bs, Lf, Ef) ->
    RBs = none,
    {value,V,_} = expr(E, Bs, Lf, Ef, RBs),
    [V].

%% RBs is the bindings to return when the evalution of a function
%% (fun) has finished. If RBs == none, then the evalution took place
%% outside a function. If RBs == value, only the value (not the bindings)
%% is to be returned (to a compiled function).

ret_expr(V, _Bs, RBs) when RBs == value ->
    V;
ret_expr(V, Bs, RBs) when RBs == none ->
    {value,V,Bs};
ret_expr(V, _Bs, RBs) when is_list(RBs) ->
    {value,V,RBs}.

%% eval_fun(Clauses, Arguments, Bindings, LocalFunctionHandler, 
%%          ExternalFunctionHandler) -> Value
%% This function is called when the fun is called from compiled code
%% or from apply.

eval_fun(Cs, As, Bs0, Lf, Ef) ->
    eval_fun(Cs, As, Bs0, Lf, Ef, value).

eval_fun([{clause,_,H,G,B}|Cs], As, Bs0, Lf, Ef, RBs) ->
    case match_list(H, As, new_bindings()) of
	{match,Bsn} ->                      % The new bindings for the head
	    Bs1 = add_bindings(Bsn, Bs0),   % which then shadow!
	    case guard(G, Bs1, Lf, Ef) of
		true -> exprs(B, Bs1, Lf, Ef, RBs);
		false -> eval_fun(Cs, As, Bs0, Lf, Ef, RBs)
	    end;
	nomatch ->
	    eval_fun(Cs, As, Bs0, Lf, Ef, RBs)
    end;
eval_fun([], As, _Bs, _Lf, _Ef, _RBs) ->
    exit({function_clause,[{?MODULE,'-inside-a-shell-fun-',As},
			   {erl_eval,expr,3}]}).

%% expr_list(ExpressionList, Bindings)
%% expr_list(ExpressionList, Bindings, LocalFuncHandler)
%% expr_list(ExpressionList, Bindings, LocalFuncHandler, ExternalFuncHandler)
%%  Evaluate a list of expressions "in parallel" at the same level.

expr_list(Es, Bs) ->
    expr_list(Es, Bs, none, none).

expr_list(Es, Bs, Lf) ->
    expr_list(Es, Bs, Lf, none).

expr_list(Es, Bs, Lf, Ef) ->
    expr_list(Es, [], Bs, Bs, Lf, Ef).    

expr_list([E|Es], Vs, BsOrig, Bs0, Lf, Ef) ->
    {value,V,Bs1} = expr(E, BsOrig, Lf, Ef, none),
    expr_list(Es, [V|Vs], BsOrig, merge_bindings(Bs1, Bs0), Lf, Ef);
expr_list([], Vs, _, Bs, _Lf, _Ef) ->
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

%% if_clauses(Clauses, Bindings, LocalFuncHandler, ExtFuncHandler, RBs)

if_clauses([{clause,_,[],G,B}|Cs], Bs, Lf, Ef, RBs) ->
    case guard(G, Bs, Lf, Ef) of
	true -> exprs(B, Bs, Lf, Ef, RBs);
	false -> if_clauses(Cs, Bs, Lf, Ef, RBs)
    end;
if_clauses([], _Bs, _Lf, _Ef, _RBs) ->
    exit({if_clause,[{erl_eval,expr,3}]}).

%% case_clauses(Value, Clauses, Bindings, LocalFuncHandler, ExtFuncHandler)

case_clauses(Val, Cs, Bs, Lf, Ef, RBs) ->
    case match_clause(Cs, [Val], Bs, Lf, Ef) of
	{B, Bs1} ->
	    exprs(B, Bs1, Lf, Ef, RBs);
	nomatch ->
	    exit({{case_clause,Val},[{erl_eval,expr,3}]})
    end.

%%
%% receive_clauses(Clauses, Bindings, LocalFuncHnd,ExtFuncHnd, Messages, RBs) 
%%
receive_clauses(Cs, Bs, Lf, Ef, Ms, RBs) ->
    receive
	Val ->
	    case match_clause(Cs, [Val], Bs, Lf, Ef) of
		{B, Bs1} ->
		    merge_queue(Ms),
		    exprs(B, Bs1, Lf, Ef, RBs);
		nomatch ->
		    receive_clauses(Cs, Bs, Lf, Ef, [Val|Ms], RBs)
	    end
    end.
%%
%% receive_clauses(TimeOut, Clauses, TimeoutBody, Bindings, 
%%                 ExternalFuncHandler, LocalFuncHandler, RBs)
%%
receive_clauses(T, Cs, TB, Bs, Lf, Ef, Ms, RBs) ->
    {_,_} = statistics(runtime),
    receive
	Val ->
	    case match_clause(Cs, [Val], Bs, Lf, Ef) of
		{B, Bs1} ->
		    merge_queue(Ms),
		    exprs(B, Bs1, Lf, Ef, RBs);
		nomatch ->
		    {_,T1} = statistics(runtime),
		    if
			T == infinity ->
			    receive_clauses(T, Cs, TB,Bs,Lf,Ef,[Val|Ms],RBs);
			T-T1 =< 0 ->
			    receive_clauses(0, Cs, TB,Bs,Lf,Ef,[Val|Ms],RBs);
			true ->
			    receive_clauses(T-T1, Cs,TB,Bs,Lf,Ef,[Val|Ms],RBs)
		    end
	    end
    after T ->
	    merge_queue(Ms),
	    {B, Bs1} = TB,
	    exprs(B, Bs1, Lf, Ef, RBs)
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

match_clause(Cs, Vs, Bs, Lf) ->
    match_clause(Cs, Vs, Bs, Lf, none).

match_clause([{clause,_,H,G,B}|Cs], Vals, Bs, Lf, Ef) ->
    case match_list(H, Vals, Bs) of
	{match, Bs1} ->
	    case guard(G, Bs1, Lf, Ef) of
		true -> {B, Bs1};
		false -> match_clause(Cs, Vals, Bs, Lf, Ef)
	    end;
	nomatch -> match_clause(Cs, Vals, Bs, Lf, Ef)
    end;
match_clause([], _Vals, _Bs, _Lf, _Ef) ->
    nomatch.


%% guard(GuardTests, Bindings, LocalFuncHandler, ExtFuncHandler) -> bool()
%%  Evaluate a guard.  We test if the guard is a true guard.

guard(L=[G|_], Bs0, Lf, Ef) when list(G) ->
    guard1(L, Bs0, Lf, Ef);
guard(L, Bs0, Lf, Ef) ->
    guard0(L, Bs0, Lf, Ef).

%% disjunction of guard conjunctions
guard1([G|Gs], Bs0, Lf, Ef) when list(G) ->
    case guard0(G, Bs0, Lf, Ef) of
	true ->
	    true;
	false ->
	    guard1(Gs, Bs0, Lf, Ef)
    end;
guard1([], _Bs, _Lf, _Ef) -> false.

%% guard conjunction
guard0([G|Gs], Bs0, Lf, Ef) ->
    case erl_lint:is_guard_test(G) of
	true ->
	    case guard_test(G, Bs0, Lf, Ef) of
		{value,true,Bs} -> guard0(Gs, Bs, Lf, Ef);
                {value,false,_} -> false
	    end;
	false ->
	    exit({guard_expr,[{erl_eval,expr,3}]})
    end;
guard0([], _Bs, _Lf, _Ef) -> true.
	
%% guard_test(GuardTest, Bindings, LocalFuncHandler, ExtFuncHandler) ->
%%	{value,bool(),NewBindings}.
%%  Evaluate one guard test. Never fails, returns bool().

guard_test({call,_,{atom,_,Name},As0}, Bs0, Lf, Ef) ->
    case catch expr_list(As0, Bs0, Lf, Ef) of
	{As1,Bs1} -> {value,type_test(Name, As1),Bs1}; % Ignore Ef.
	_Other -> {value,false,Bs0}
    end;
guard_test({op,_,Op,A0}, Bs0, Lf, Ef) ->
    case catch begin
		   {[A],Bs1} = expr_list([A0], Bs0, Lf, Ef),
		   {value,eval_op(Op, A),Bs1}
	       end of
	{value,true,Bs2} -> {value,true,Bs2};
	_Other -> {value,false,Bs0}
    end;
guard_test({op,_,Op,Lhs0,Rhs0}, Bs0, Lf, Ef) ->
    case catch begin
		   {[Lhs,Rhs],Bs1} = expr_list([Lhs0,Rhs0], Bs0, Lf, Ef),
		   {value,eval_op(Op, Lhs, Rhs),Bs1}
	       end of
	{value,true,Bs2} -> {value,true,Bs2};
	_Other -> {value,false,Bs0}
    end;
guard_test({atom,_,true}, Bs, _Lf, _Ef) -> {value,true,Bs};
guard_test({atom,_,false}, Bs, _Lf, _Ef) -> {value,false,Bs};
guard_test({var,_,V}, Bs, _Lf, _Ef) ->
    {value,Val} = binding(V, Bs),
    {value,Val == true,Bs};
guard_test(_Other, Bs, _Lf, _Ef) -> {value,false,Bs}.

type_test(integer, [A]) -> erlang:is_integer(A);
type_test(float, [A]) ->  erlang:is_float(A);
type_test(number, [A]) -> erlang:is_number(A);
type_test(atom, [A]) -> erlang:is_atom(A);
type_test(constant, [A]) -> erlang:is_constant(A);
type_test(list, [A]) -> erlang:is_list(A);
type_test(tuple, [A]) -> erlang:is_tuple(A);
type_test(pid, [A]) -> erlang:is_pid(A);
type_test(reference, [A]) -> erlang:is_reference(A);
type_test(port, [A]) -> erlang:is_port(A);
type_test(function, [A]) -> erlang:is_function(A);
type_test(binary, [A]) -> erlang:is_binary(A);
type_test(record, As) -> type_test(is_record, As);
type_test(is_record, [R,A]) when tuple(R), atom(A) ->
    erlang:is_record(R, A, size(R));
type_test(is_record, [_,_]) -> false;
type_test(Test, As) -> erlang:apply(erlang, Test, As).


%% match(Pattern, Term, Bindings) ->
%%	{match,NewBindings} | nomatch
%%      or exit({illegal_pattern, Pattern}).
%%  Try to match Pattern against Term with the current bindings.

match(Pat, Term, Bs) ->
    match(Pat, Term, Bs, Bs).

%% Bs are the bindings that are augmented with new bindings. BBs are
%% the bindings used for "binsize" variables (in <<X:Y>>, Y is a
%% binsize variable).

match(Pat, Term, Bs, BBs) ->
    case catch match1(Pat, Term, Bs, BBs) of
	invalid ->
	    exit({{illegal_pattern,Pat},[{erl_eval,expr,3}]});
	Other ->
	    Other
    end.

string_to_conses([], _, Tail) -> Tail;
string_to_conses([E|Rest], Line, Tail) ->
    {cons, Line, {integer, Line, E}, string_to_conses(Rest, Line, Tail)}.

match1({atom,_,A0}, A, Bs, _BBs) ->
    case A of
	A0 -> {match,Bs};
	_ -> throw(nomatch)
    end;
match1({integer,_,I0}, I, Bs, _BBs) ->
    case I of
	I0 -> {match,Bs};
	_ -> throw(nomatch)
    end;
match1({float,_,F0}, F, Bs, _BBs) ->
    case F of
	F0 -> {match,Bs};
	_ -> throw(nomatch)
    end;
match1({char,_,C0}, C, Bs, _BBs) ->
    case C of
	C0 -> {match,Bs};
	_ -> throw(nomatch)
    end;
match1({var,_,'_'}, _, Bs, _BBs) ->		%Anonymous variable matches
    {match,Bs};					% everything, no new bindings
match1({var,_,Name}, Term, Bs, _BBs) ->
    case binding(Name, Bs) of
	{value,Term} ->
	    {match,Bs};
	{value,_} ->
	    throw(nomatch);
	unbound ->
	    {match,add_binding(Name, Term, Bs)}
    end;
match1({match,_,Pat1,Pat2}, Term, Bs0, BBs) ->
    {match, Bs1} = match1(Pat1, Term, Bs0, BBs),
    match1(Pat2, Term, Bs1, BBs);
match1({string,_,S0}, S, Bs, _BBs) ->
    case S of
	S0 -> {match,Bs};
	_ -> throw(nomatch)
    end;
match1({nil,_}, Nil, Bs, _BBs) ->
    case Nil of
	[] -> {match,Bs};
	_ -> throw(nomatch)
    end;
match1({cons,_,H,T}, [H1|T1], Bs0, BBs) ->
    {match,Bs} = match1(H, H1, Bs0, BBs),
    match1(T, T1, Bs, BBs);
match1({cons,_,_,_}, _, _Bs, _BBs) ->
    throw(nomatch);
match1({tuple,_,Elts}, Tuple, Bs, BBs) when tuple(Tuple),
				       length(Elts) == size(Tuple) ->
    match_tuple(Elts, Tuple, 1, Bs, BBs);
match1({tuple,_,_}, _, _Bs, _BBs) ->
    throw(nomatch);
match1({bin, _, Fs}, B, Bs0, BBs) when binary(B) ->
    eval_bits:match_bits(Fs, B, Bs0, BBs,
			 fun(L, R, Bs) -> match1(L, R, Bs, BBs) end,
			 fun(E, Bs) -> expr(E, Bs, none, none, none) end,
			 true);
match1({bin,_,_}, _, _Bs, _BBs) ->
    throw(nomatch);
match1({op,_,'++',{nil,_},R}, Term, Bs, BBs) ->
    match1(R, Term, Bs, BBs);
match1({op,_,'++',{cons,Li,{integer,L2,I},T},R}, Term, Bs, BBs) ->
    match1({cons,Li,{integer,L2,I},{op,Li,'++',T,R}}, Term, Bs, BBs);
match1({op,_,'++',{string,Li,L},R}, Term, Bs, BBs) ->
    match1(string_to_conses(L, Li, R), Term, Bs, BBs);
match1({op,Line,Op,A}, Term, Bs, BBs) ->
    case partial_eval({op,Line,Op,A}) of
	{op,Line,Op,A} ->
	    throw(invalid);
	X ->
	    match1(X, Term, Bs, BBs)
    end;
match1({op,Line,Op,L,R}, Term, Bs, BBs) ->
    case partial_eval({op,Line,Op,L,R}) of
	{op,Line,Op,L,R} ->
	    throw(invalid);
	X ->
	    match1(X, Term, Bs, BBs)
    end;
match1(_, _, _Bs, _BBs) ->
    throw(invalid).

match_tuple([E|Es], Tuple, I, Bs0, BBs) ->
    {match,Bs} = match1(E, element(I, Tuple), Bs0, BBs),
    match_tuple(Es, Tuple, I+1, Bs, BBs);
match_tuple([], _, _, Bs, _BBs) ->
    {match,Bs}.

%% match_list(PatternList, TermList, Bindings) ->
%%	{match,NewBindings} | nomatch
%%  Try to match a list of patterns against a list of terms with the
%%  current bindings.

match_list([P|Ps], [T|Ts], Bs0) ->
    case match(P, T, Bs0) of
	{match,Bs1} -> match_list(Ps, Ts, Bs1);
	nomatch -> nomatch
    end;
match_list([], [], Bs) ->
    {match,Bs}.

%% new_bindings()
%% bindings(Bindings)
%% binding(Name, Bindings)
%% add_binding(Name, Value, Bindings)
%% del_binding(Name, Bindings)

new_bindings() -> orddict:new().

bindings(Bs) -> orddict:to_list(Bs).

binding(Name, Bs) ->
    case orddict:find(Name, Bs) of
	{ok,Val} -> {value,Val};
	error -> unbound
    end.

add_binding(Name, Val, Bs) -> orddict:store(Name, Val, Bs).

del_binding(Name, Bs) -> orddict:erase(Name, Bs).

add_bindings(Bs1, Bs2) ->
    foldl(fun ({Name,Val}, Bs) -> orddict:store(Name, Val, Bs) end,
	  Bs2, orddict:to_list(Bs1)).

merge_bindings(Bs1, Bs2) ->
    foldl(fun ({Name,Val}, Bs) ->
		  case orddict:find(Name, Bs) of
		      {ok,Val} -> Bs;		%Already with SAME value
		      {ok,V1} -> exit({{badmatch,V1},[{erl_eval,expr,3}]});
		      error -> orddict:store(Name, Val, Bs)
		  end end,
	  Bs2, orddict:to_list(Bs1)).


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

ev_expr({op,_,Op,L,R}) -> eval(Op, ev_expr(L), ev_expr(R));
ev_expr({op,_,Op,A}) -> eval(Op, ev_expr(A));
ev_expr({integer,_,X}) -> X;
ev_expr({float,_,X})   -> X;
ev_expr({atom,_,X})    -> X;
ev_expr({tuple,_,Es}) ->
    list_to_tuple(map(fun(X) -> ev_expr(X) end, Es));
ev_expr({nil,_}) -> [];
ev_expr({cons,_,H,T}) -> [ev_expr(H) | ev_expr(T)].
%ev_expr({call,Line,{atom,_,F},As}) ->
%    true = erl_internal:guard_bif(F, length(As)),
%    apply(erlang, F, map(fun(X) -> ev_expr(X) end, As));
%ev_expr({call,Line,{remote,_,{atom,_,erlang},{atom,_,F}},As}) ->
%    true = erl_internal:guard_bif(F, length(As)),
%    apply(erlang, F, map(fun(X) -> ev_expr(X) end, As)).

				    
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

ret_expr(_Old, New) ->
%%    io:format("~w: reduced ~s => ~s~n",
%%	      [line(Old), erl_pp:expr(Old), erl_pp:expr(New)]),
    New.

line(Expr) -> element(2, Expr).

%% In syntax trees, module/package names are atoms or lists of atoms.

package_to_string(A) when atom(A) -> atom_to_list(A);
package_to_string(L) when list(L) -> packages:concat(L).

expand_module_name({atom,L,A} = M, Bs) ->
    case binding({module,A}, Bs) of
	{value, A1} ->
	    {atom,L,A1};
	unbound ->
	    case packages:is_segmented(A) of
		true ->
		    M;
		false ->
%%% 		    P = case binding({module,'$package'}, Bs) of
%%% 			    {value, P1} -> P1;
%%% 			    unbound -> ""
%%% 			end,
%%% 		    A1 = list_to_atom(packages:concat(P, A)),
%%% 		    {atom,L,list_to_atom(A1)}
		    {atom,L,A}
	    end
    end;
expand_module_name(M, _) ->
    case erl_parse:package_segments(M) of
	error ->
	    M;
	M1 ->
	    L = element(2,M),
	    Mod = package_to_string(M1),
	    case packages:is_valid(Mod) of
		true ->
		    {atom,L,list_to_atom(Mod)};
		false ->
		    exit({{bad_module_name, Mod}, [{erl_eval,expr,3}]})
	    end
    end.

