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
%% Vectors only as multiple values/variables/patterns.
%%
%% Checks to add:
%%
%% Consistency of values/variables
%% Consistency of function return values/calls.

-module(core_lint).


-export([module/1,module/2,format_error/1]).

-import(lists, [reverse/1,all/2,foldl/3]).
-import(ordsets, [add_element/2,is_element/2,subtract/2]).

-include("core_parse.hrl").

%% Define the lint state record.

-record(lint, {module=[],			%Current module
	       func=[],				%Current function
	       defined=[],			%Defined functions
	       called=[],			%Called functions
	       errors=[],			%Errors
	       warnings=[]}).			%Warnings

%% format_error(Error)
%%  Return a string describing the error.

format_error(invalid_exports) -> "invalid exports";
format_error(invalid_attributes) -> "invalid attributes";
format_error({undefined_function,{F,A}}) ->
    io_lib:format("function ~w/~w undefined", [F,A]);
format_error({illegal_expr,{F,A}}) ->
    io_lib:format("illegal expression in ~w/~w", [F,A]);
format_error({unbound_var,N,{F,A}}) ->
    io_lib:format("unbound variable ~s in ~w/~w", [N,F,A]);
format_error({not_var,{F,A}}) ->
    io_lib:format("expecting variable in ~w/~w", [F,A]);
format_error({not_pattern,{F,A}}) ->
    io_lib:format("expecting pattern in ~w/~w", [F,A]).

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
	#c_fun{} -> expr(B, [], St1);
	Other -> add_error({illegal_expr,St1#lint.func}, St1)
    end.

%% body(Expr, [DefVar], State) -> State.

body(#c_vector{es=Es}, Ds, St) ->
    expr_list(Es, Ds, St);
body(E, Ds, St) ->
    expr(E, Ds, St).

%% expr(Expr, [DefVar], State) -> State.

expr(#c_var{name=N}, Ds, St) -> expr_var(N, Ds, St);
expr(#c_int{}, Ds, St) -> St;
expr(#c_float{}, Ds, St) -> St;
expr(#c_atom{}, Ds, St) -> St;
expr(#c_char{}, Ds, St) -> St;
expr(#c_string{}, Ds, St) -> St;
expr(#c_nil{}, Ds, St) -> St;
expr(#c_cons{head=H,tail=T}, Ds, St) ->
    expr_list([H,T], Ds, St);
expr(#c_tuple{es=Es}, Ds, St) ->
    expr_list(Es, Ds, St);
expr(#c_let{vars=Vs,arg=Arg,body=B}, Ds0, St0) ->
    St1 = body(Arg, Ds0, St0),			%This is a body
    {Ds1,St2} = let_vars(Vs, Ds0, St1),
    body(B, Ds1, St2);
expr(#c_seq{arg=Arg,body=B}, Ds, St0) ->
    St1 = expr(Arg, Ds, St0),
    body(B, Ds, St1);
expr(#c_fun{vars=Vs,body=B}, Ds0, St0) ->
    {Ds1,St1} = var_list(Vs, Ds0, St0),
    body(B, Ds1, St1);
expr(#c_case{arg=Arg,clauses=Cs}, Ds, St0) ->
    St1 = body(Arg, Ds, St0),
    clauses(Cs, Ds, St1);
expr(#c_receive{clauses=Cs,timeout=T,action=A}, Ds, St0) ->
    St1 = expr(T, Ds, St0),
    St2 = body(A, Ds, St1),
    clauses(Cs, Ds, St2);
expr(#c_catch{body=B}, Ds, St) ->
    body(B, Ds, St);
expr(#c_call{op=Op,args=As}, Ds, St0) ->
    St1 = call_op(Op, Ds, St0),
    expr_list(As, Ds, St1);
expr(#c_local{}, Ds, St) -> St;
expr(Other, Ds, St) ->
    add_error({illegal_expr,St#lint.func}, St).

%% expr_list([Expr], [DefVar], State) -> State.

expr_list(Es, Ds, St0) ->
    foldl(fun (E, St) -> expr(E, Ds, St) end, St0, Es).

%% call_op(Op, [DefVar], State) -> State.
%%  A call op is either an internal, remote or an expression.

call_op(#c_internal{}, Ds, St) -> St; 
call_op(#c_remote{}, Ds, St) -> St;
call_op(E, Ds, St) -> expr(E, Ds, St).

%% let_vars([Var], [DefVar], State) -> {[DefVar],State}.

let_vars(#c_vector{es=Vs}, Ds, St) ->
    var_list(Vs, Ds, St);
let_vars(Var, Ds, St) ->
    variable(Var, Ds, St).

%% clauses([Clause], [DefVar], State) -> State.

clauses(Cs, Ds, St0) ->
    foldl(fun (C, St) -> clause(C, Ds, St) end, St0, Cs).

%% clause(Clause, [DefVar], State) -> State.

clause(#c_clause{pat=P,guard=G,body=B}, Ds0, St0) ->
    {Ds1,St1} = case_pats(P, Ds0, St0),
    St2 = expr(G, Ds1, St1),
    body(B, Ds1, St2).

%% case_pats([Var], [DefVar], State) -> {[DefVar],State}.

case_pats(#c_vector{es=Ps}, Ds, St) ->
    pat_list(Ps, Ds, St);
case_pats(Pat, Ds, St) ->
    pattern(Pat, Ds, St).

%% variable(Var, [DefVar], State) -> {[DefVar],State}.

variable(#c_var{name=N}, Ds, St) ->
    {add_element(N, Ds),St};
variable(Other, Ds, St) -> {Ds,add_error({not_var,St#lint.func}, St)}.

%% var_list([Var], [DefVar], State) -> {[DefVar],State}.

var_list(Vs, Ds0, St0) ->
    foldl(fun (V, {Ds,St}) -> variable(V, Ds, St) end, {Ds0,St0}, Vs).

%% pattern(Var, [DefVar], State) -> {[DefVar],State}.

pattern(#c_var{name=N}, Ds, St) ->
    {add_element(N, Ds),St};
pattern(#c_int{}, Ds, St) -> {Ds,St};
pattern(#c_float{}, Ds, St) -> {Ds,St};
pattern(#c_atom{}, Ds, St) -> {Ds,St};
pattern(#c_char{}, Ds, St) -> {Ds,St};
pattern(#c_string{}, Ds, St) -> {Ds,St};
pattern(#c_nil{}, Ds, St) -> {Ds,St};
pattern(#c_cons{head=H,tail=T}, Ds, St) ->
    pat_list([H,T], Ds, St);
pattern(#c_tuple{es=Es}, Ds, St) ->
    pat_list(Es, Ds, St);
pattern(#c_alias{var=V,pat=P}, Ds0, St0) ->
    {Ds1,St1} = variable(V, Ds0, St0),
    pattern(P, Ds1, St1);
pattern(Other, Ds, St) -> {Ds,add_error({not_pattern,St#lint.func}, St)}.

%% pat_list([Var], [DefVar], State) -> {[DefVar],State}.

pat_list(Ps, Ds0, St0) ->
    foldl(fun (P, {Ds,St}) -> pattern(P, Ds, St) end, {Ds0,St0}, Ps).

%% expr_var(VarName, [DefVar], State) -> State.

expr_var(N, Ds, St) ->
    case is_element(N, Ds) of
	true -> St;
	false -> add_error({unbound_var,N,St#lint.func}, St)
    end.
