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
%% Purpose : Do necessary checking of Core Erlang code.

%% Check Core module for errors.  Seeing this module is used in the
%% compiler after optimisations wedone more checking than would be
%% necessary after just parsing.  Don't check all constructs.
%%
%% We check the following:
%%
%% All referred functions, called and exported, are defined.
%% Format of export list.
%% Format of attributes
%% Used variables are defined.
%% Variables in let and funs.
%% Patterns case clauses.
%% Values only as multiple values/variables/patterns.
%% Return same number of values as requested
%% Correct number of arguments
%%
%% Checks to add:
%%
%% Consistency of values/variables
%% Consistency of function return values/calls.

-module(core_lint).


-export([module/1,module/2,format_error/1]).

-import(lists, [reverse/1,all/2,foldl/3]).
-import(ordsets, [add_element/2,is_element/2,subtract/2,union/2]).

-include("core_parse.hrl").

%% Define the lint state record.

-record(lint, {module=[],			%Current module
	       func=[],				%Current function
	       defined=[],			%Defined functions
	       called=[],			%Called functions
	       errors=[],			%Errors
	       warnings=[]}).			%Warnings

%%-deftype retcount() -> any | unknown | int().

%% format_error(Error)
%%  Return a string describing the error.

format_error(invalid_exports) -> "invalid exports";
format_error(invalid_attributes) -> "invalid attributes";
format_error({undefined_function,{F,A}}) ->
    io_lib:format("function ~w/~w undefined", [F,A]);
format_error({illegal_expr,{F,A}}) ->
    io_lib:format("illegal expression in ~w/~w", [F,A]);
format_error({illegal_pattern,{F,A}}) ->
    io_lib:format("illegal pattern in ~w/~w", [F,A]);
format_error({pattern_mismatch,{F,A}}) ->
    io_lib:format("pattern count mismatch in ~w/~w", [F,A]);
format_error({return_mismatch,{F,A}}) ->
    io_lib:format("return count mismatch in ~w/~w", [F,A]);
format_error({arg_mismatch,{F,A}}) ->
    io_lib:format("argument count mismatch in ~w/~w", [F,A]);
format_error({unbound_var,N,{F,A}}) ->
    io_lib:format("unbound variable ~s in ~w/~w", [N,F,A]);
format_error({duplicate_var,N,{F,A}}) ->
    io_lib:format("duplicate variable ~s in ~w/~w", [N,F,A]);
format_error({not_var,{F,A}}) ->
    io_lib:format("expecting variable in ~w/~w", [F,A]);
format_error({not_pattern,{F,A}}) ->
    io_lib:format("expecting pattern in ~w/~w", [F,A]);
format_error({not_bs_pattern,{F,A}}) ->
    io_lib:format("expecting bit syntax pattern in ~w/~w", [F,A]).

%% module(CoreMod) ->
%% module(CoreMod, [CompileOption]) ->
%%	{ok,[Warning]} | {error,[Error],[Warning]}

module(M) -> module(M, []).

module(#c_mdef{name=M,exports=Es,attributes=As,body=B}, Opts) ->
    St0 = #lint{module=M},
    St1 = check_exports(Es, St0),
    St2 = check_attrs(As, St1),
    St3 = module_body(B, St2),
    St4 = check_state(Es, St3),
    return_status(St4).

%% return_status(State) ->
%%	{ok,[Warning]} | {error,[Error],[Warning]}
%%  Pack errors and warnings properly and return ok | error.

return_status(St) ->
    Ws = reverse(St#lint.warnings),
    case reverse(St#lint.errors) of
	[] -> {ok,[{St#lint.module,Ws}]};
	Es -> {error,[{St#lint.module,Es}],[{St#lint.module,Ws}]}
    end.

%% add_error(ErrorDescriptor, State) -> State'
%% add_warning(ErrorDescriptor, State) -> State'

add_error(E, St) -> St#lint{errors=[{none,core_lint,E}|St#lint.errors]}.

add_warning(W, St) -> St#lint{warnings=[{none,core_lint,W}|St#lint.warnings]}.

check_exports(Es, St) ->
    case all(fun ({Name,Arity}) when atom(Name), integer(Arity) -> true;
		 (Other) -> false
	     end, Es) of
	true -> St;
	false -> add_error(invalid_exports, St)
    end.

check_attrs(As, St) ->
    case all(fun ({Name,Val}) when atom(Name) -> true;
		 (Other) -> false
	     end, As) of
	true -> St;
	false -> add_error(invalid_attributes, St)
    end.

check_state(Es, St0) ->
    Referred = foldl(fun (F, Refs) -> add_element(F, Refs) end,
		     St0#lint.called, Es),
    Undef = subtract(Referred, St0#lint.defined),
    St1 = foldl(fun (F, St) -> add_error({undefined_function,F}, St) end,
		St0, Undef),
    St1.

%% module_body(CoreBody, State) -> State.

module_body(B, St0) ->
    foldl(fun (F, St) -> function(F, St) end, St0, B).

%% function(CoreFunc, State) -> State.

function(#c_fdef{func=F,arity=Ar,body=B}, St0) ->
    Func = {F,Ar},
    St1 = St0#lint{func=Func,defined=add_element(Func, St0#lint.defined)},
    %% Body must be a fun!
    case B of
	#c_fun{} -> expr(B, [], any, St1);
	Other -> add_error({illegal_expr,St1#lint.func}, St1)
    end.

%% body(Expr, [DefVar], RetCount, State) -> State.

body(#c_values{es=Es}, Ds, Rc, St) ->
    return_match(Rc, length(Es), expr_list(Es, Ds, St));
body(E, Ds, Rt, St0) ->
    St1 = expr(E, Ds, Rt, St0),
    case core_lib:is_literal_top(E) of
	true -> return_match(Rt, 1, St1); 
	false -> St1
    end.

%% expr(Expr, [DefVar], Rt, State) -> State.

expr(#c_var{name=N}, Ds, Rt, St) -> expr_var(N, Ds, St);
expr(#c_int{}, Ds, Rt, St) -> St;
expr(#c_float{}, Ds, Rt, St) -> St;
expr(#c_atom{}, Ds, Rt, St) -> St;
expr(#c_char{}, Ds, Rt, St) -> St;
expr(#c_string{}, Ds, Rt, St) -> St;
expr(#c_nil{}, Ds, Rt, St) -> St;
expr(#c_cons{head=H,tail=T}, Ds, Rt, St) ->
    expr_list([H,T], Ds, St);
expr(#c_tuple{es=Es}, Ds, Rt, St) ->
    expr_list(Es, Ds, St);
expr(#c_bin{es=V}, Ds, Rt, St) ->
    bin_elem_list(V, Ds, St);
expr(#c_local{}, Ds, Rt, St) -> return_match(Rt, 1, St);
expr(#c_fun{vars=Vs,body=B}, Ds, Rt, St0) ->
    {Vvs,St1} = variable_list(Vs, St0),
    return_match(Rt, 1, body(B, union(Vvs, Ds), any, St1));
expr(#c_seq{arg=Arg,body=B}, Ds, Rt, St0) ->
    St1 = expr(Arg, Ds, any, St0),		%Ignore values
    body(B, Ds, Rt, St1);
expr(#c_let{vars=Vs,arg=Arg,body=B}, Ds, Rt, St0) ->
    St1 = body(Arg, Ds, let_varcount(Vs), St0),	%This is a body
    {Lvs,St2} = variable_list(Vs, St1),
    body(B, union(Lvs, Ds), Rt, St2);
expr(#c_case{arg=Arg,clauses=Cs}, Ds, Rt, St0) ->
    Pc = case_patcount(Cs),
    St1 = body(Arg, Ds, Pc, St0),
    clauses(Cs, Ds, Pc, Rt, St1);
expr(#c_receive{clauses=Cs,timeout=T,action=A}, Ds, Rt, St0) ->
    St1 = expr(T, Ds, 1, St0),
    St2 = body(A, Ds, Rt, St1),
    clauses(Cs, Ds, 1, Rt, St2);
expr(#c_call{op=Op,args=As}, Ds, Rt, St0) ->
    St1 = call_op(Op, Ds, length(As), St0),
    expr_list(As, Ds, St1);
expr(#c_catch{body=B}, Ds, Rt, St) ->
    return_match(Rt, 1, body(B, Ds, 1, St));
expr(Other, Ds, Rt, St) ->
    io:fwrite("clint: ~p~n", [Other]),
    add_error({illegal_expr,St#lint.func}, St).

%% expr_list([Expr], [DefVar], State) -> State.

expr_list(Es, Ds, St0) ->
    foldl(fun (E, St) -> expr(E, Ds, 1, St) end, St0, Es).

%% bin_elem_list([Elem], [DefVar], State) -> State.

bin_elem_list(Es, Ds, St0) ->
    foldl(fun (E, St) -> bin_elem(E, Ds, St) end, St0, Es).

bin_elem(#c_bin_elem{val=V,size=S,type=T}, Ds, St0) ->
    St1 = bit_type(T, St0),
    expr_list([V,S], Ds, St1).

%% let_varcount([Var]) -> int().

let_varcount([]) -> any;			%Ignore values
let_varcount(Es) -> length(Es).

%% case_patcount([Clause]) -> int().

case_patcount([#c_clause{pats=Ps}|Cs]) -> length(Ps).

%% clauses([Clause], [DefVar], PatCount, RetCount, State) -> State.

clauses(Cs, Ds, Pc, Rt, St0) ->
    foldl(fun (C, St) -> clause(C, Ds, Pc, Rt, St) end, St0, Cs).

%% clause(Clause, [DefVar], PatCount, RetCount, State) -> State.

clause(#c_clause{pats=Ps,guard=G,body=B}, Ds0, Pc, Rt, St0) ->
    St1 = pattern_match(Pc, length(Ps), St0),
    {Pvs,St2} = pattern_list(Ps, Ds0, St1),
    Ds1 = union(Pvs, Ds0),
    St3 = expr(G, Ds1, any, St2),
    body(B, Ds1, Rt, St3).

%% call_op(Op, [DefVar], ArgCount, State) -> State.
%%  A call op is either an internal, remote or an expression.

call_op(#c_internal{arity=Ar}, Ds, Ac, St) -> arg_match(Ac, Ar, St); 
call_op(#c_remote{arity=Ar}, Ds, Ac, St) -> arg_match(Ac, Ar, St);
call_op(E, Ds, Ac, St) -> expr(E, Ds, 1, St).	%Hard to check

%% variable(Var, [PatVar], State) -> {[Var],State}.

variable(#c_var{name=N}, Ps, St) ->
    case is_element(N, Ps) of
	true -> {[],add_error({duplicate_var,N,St#lint.func}, St)};
	false -> {[N],St}
    end;
variable(Other, Ds, St) -> {Ds,add_error({not_var,St#lint.func}, St)}.

%% variable_list([Var], State) -> {[Var],State}.
%% variable_list([Var], [PatVar], State) -> {[Var],State}.

variable_list(Vs, St) -> variable_list(Vs, [], St).

variable_list(Vs, Ps, St) ->
    foldl(fun (V, {Ps0,St0}) ->
		  {Vvs,St1} = variable(V, Ps0, St),
		  {union(Vvs, Ps0),St1}
	  end, {Ps,St}, Vs).

%% pattern(Pattern, [DefVar], State) -> {[PatVar],State}.
%% pattern(Pattern, [DefVar], [PatVar], State) -> {[PatVar],State}.
%%  Patterns are complicated by sizes in binaries.  These are pure
%%  input variables which create no bindings.  We, therefor, need to
%%  carry around the original defined variables to get the correct
%%  handling.

pattern(P, Ds, St) -> pattern(P, Ds, [], St).

pattern(#c_var{name=N}, Ds, Ps, St) ->
    pat_var(N, Ds, Ps, St);
pattern(#c_int{}, Ds, Ps, St) -> {Ps,St};
pattern(#c_float{}, Ds, Ps, St) -> {Ps,St};
pattern(#c_atom{}, Ds, Ps, St) -> {Ps,St};
pattern(#c_char{}, Ds, Ps, St) -> {Ps,St};
pattern(#c_string{}, Ds, Ps, St) -> {Ps,St};
pattern(#c_nil{}, Ds, Ps, St) -> {Ps,St};
pattern(#c_cons{head=H,tail=T}, Ds, Ps, St) ->
    pattern_list([H,T], Ds, Ps, St);
pattern(#c_tuple{es=Es}, Ds, Ps, St) ->
    pattern_list(Es, Ds, Ps, St);
pattern(#c_bin{es=Es}, Ds, Ps, St) ->
    pat_bin(Es, Ds, Ps, St);
pattern(#c_alias{var=V,pat=P}, Ds, Ps, St0) ->
    {Vvs,St1} = variable(V, Ps, St0),
    pattern(P, Ds, union(Vvs, Ps), St1);
pattern(Other, Ds, Ps, St) -> {Ps,add_error({not_pattern,St#lint.func}, St)}.

pat_var(N, Ds, Ps, St) ->
    case is_element(N, Ps) of
	true -> {Ps,add_error({duplicate_var,N,St#lint.func}, St)};
	false -> {add_element(N, Ps),St}
    end.

%% pat_bin_list([Elem], [DefVar], [PatVar], State) -> {[PatVar],State}.

pat_bin(Es, Ds, Ps0, St0) ->
    foldl(fun (E, {Ps,St}) -> pat_element(E, Ds, Ps, St) end, {Ps0,St0}, Es).

pat_element(#c_bin_elem{val=V,size=S,type=T}, Ds, Ps, St0) ->
    St1 = bit_type(T, St0),
    St2 = pat_bit_expr(S, Ds, 1, St1),
    pattern(V, Ds, Ps, St2);
pat_element(Other, Ds, Ps, St) ->
    {Ps,add_error({not_bs_pattern,St#lint.func}, St)}.

pat_bit_expr(#c_int{}, Ds, Rt, St) -> St;
pat_bit_expr(#c_var{name=N}, Ds, Rt, St) ->
    expr_var(N, Ds, St);
pat_bit_expr(#c_string{}, Ds, Rt, St) -> St;
pat_bit_expr(Other, Ds, Rt, St) ->
    add_error({illegal_expr,St#lint.func}, St).    

bit_type(Type, St) ->
    case erl_bits:set_bit_type(default, Type) of
	{ok,S,Bt} -> St; 
	{error,E} -> add_error({E,St#lint.func}, St)
    end.

%% pattern_list([Var], [DefVar], State) -> {[PatVar],State}.
%% pattern_list([Var], [DefVar], [PatVar], State) -> {[PatVar],State}.

pattern_list(Pats, Ds, St) -> pattern_list(Pats, Ds, [], St).

pattern_list(Pats, Ds, Ps0, St0) ->
    foldl(fun (P, {Ps,St}) -> pattern(P, Ds, Ps, St) end, {Ps0,St0}, Pats).

%% expr_var(VarName, [DefVar], State) -> State.

expr_var(N, Ds, St) ->
    case is_element(N, Ds) of
	true -> St;
	false -> add_error({unbound_var,N,St#lint.func}, St)
    end.

%% pattern_match(Required, Supplied, State) -> State.

pattern_match(N, N, St) -> St;
pattern_match(Req, Sup, St) ->
    add_error({pattern_mismatch,St#lint.func}, St).

%% return_match(Required, Supplied, State) -> State.

return_match(any, Sup, St) -> St;
return_match(Req, unknown, St) -> St;
return_match(N, N, St) -> St;
return_match(Req, Sup, St) ->
    add_error({return_mismatch,St#lint.func}, St).

%% arg_match(Required, Supplied, State) -> State.

arg_match(Req, unknown, St) -> St;
arg_match(N, N, St) -> St;
arg_match(Req, Sup, St) ->
    add_error({arg_mismatch,St#lint.func}, St).
