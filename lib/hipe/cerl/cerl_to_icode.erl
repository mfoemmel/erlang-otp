%% -*- erlang-indent-level: 4 -*-
%% =====================================================================
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% $Id$
%%
%% @author Richard Carlsson <richardc@csd.uu.se>
%% @copyright 2000-2004 Richard Carlsson
%% @doc Translation from Core Erlang to HiPE Icode.

%% TODO: annotate Icode leaf functions as such.
%% TODO: add a pass to remove unnecessary reduction tests
%% TODO: generate branch prediction info?

-module(cerl_to_icode).

-define(NO_UNUSED, true).

-export([module/2]).
-ifndef(NO_UNUSED).
-export([function/3, function/4, module/1]).
-endif.

-import(lists, [mapfoldl/3]).

%% Added in an attempt to suppress message by Dialyzer, but I run into
%% an internal compiler error in the old inliner and commented it out.
%% The inlining is performed manually instead :-(             - Kostis
%% -compile({inline, [{error_fun_value,1}]}).

%% ---------------------------------------------------------------------
%% Macros and records

%% Icode primitive operation names

-define(OP_REDTEST, redtest).
-define(OP_CONS, cons).
-define(OP_TUPLE, mktuple).
-define(OP_ELEMENT, {erlang,element,2}).  %% This has an MFA name
-define(OP_UNSAFE_HD, unsafe_hd).
-define(OP_UNSAFE_TL, unsafe_tl).
-define(OP_UNSAFE_ELEMENT(N), {unsafe_element, N}).
-define(OP_UNSAFE_SETELEMENT(N), {unsafe_update_element, N}).
-define(OP_CHECK_GET_MESSAGE, check_get_msg).
-define(OP_NEXT_MESSAGE, next_msg).
-define(OP_SELECT_MESSAGE, select_msg).
-define(OP_SET_TIMEOUT, set_timeout).
-define(OP_CLEAR_TIMEOUT, clear_timeout).
-define(OP_WAIT_FOR_MESSAGE, suspend_msg).
-define(OP_APPLY_FIXARITY(N), {apply_N, N}).
-define(OP_MAKE_FUN(M, F, A, H, I), {mkfun, {M, F, A}, H, I}).
-define(OP_FUN_ELEMENT(N), {closure_element, N}).

%% Icode conditional tests

-define(TEST_EQ, '==').
-define(TEST_NE, '/=').
-define(TEST_EXACT_EQ, '=:=').
-define(TEST_EXACT_NE, '=/=').
-define(TEST_LT, '<').
-define(TEST_GT, '>').
-define(TEST_LE, '=<').
-define(TEST_GE, '>=').
-define(TEST_WAIT_FOR_MESSAGE_OR_TIMEOUT, suspend_msg_timeout).

%% Icode type tests

-define(TYPE_ATOM(X), {atom, X}).
-define(TYPE_INTEGER(X), {integer, X}).
-define(TYPE_FIXNUM(X), {integer, X}).    % for now
-define(TYPE_CONS, cons).
-define(TYPE_NIL, nil).
-define(TYPE_IS_N_TUPLE(N), {tuple, N}).
-define(TYPE_IS_ATOM, atom).
-define(TYPE_IS_BIGNUM, bignum).
-define(TYPE_IS_BINARY, binary).
-define(TYPE_IS_CONSTANT, constant).
-define(TYPE_IS_FIXNUM, fixnum).
-define(TYPE_IS_FLOAT, float).
-define(TYPE_IS_FUNCTION, function).
-define(TYPE_IS_INTEGER, integer).
-define(TYPE_IS_LIST, list).
-define(TYPE_IS_NUMBER, number).
-define(TYPE_IS_PID, pid).
-define(TYPE_IS_PORT, port).
-define(TYPE_IS_RECORD(Atom_, Size_), {record, Atom_, Size_}).
-define(TYPE_IS_REFERENCE, reference).
-define(TYPE_IS_TUPLE, tuple).

%% Record definitions

-record(ctxt, {final = false,
	       effect = false,
	       fail = [],		% [] or fail-to label
	       class = expr,		% expr | guard
	       line = 0,		% current line number
	       'receive'		% undefined | #receive{}
	      }).		

-record('receive', {loop}).
-record(var, {name}).
-record('fun', {label, vars}).


%% ---------------------------------------------------------------------
%% Code


%% @spec module(Module::cerl()) -> [icode()]
%% @equiv module(Module, [])

-ifndef(NO_UNUSED).
module(E) ->
    module(E, []).
-endif.
%% @clear


%% @spec module(Module::cerl(), Options::[term()]) -> [icode()]
%%
%%	    cerl() = cerl:cerl()
%%	    icode() = hipe_icode:icode()
%%
%% @doc Transforms a Core Erlang module to linear HiPE Icode. The result
%% is a list of Icode function definitions. Currently, no options are
%% available.
%%
%% <p>This function first calls the {@link cerl_hipeify:transform/2}
%% function on the module.</p>
%%
%% <p>Note: Except for the module name, which is included in the header
%% of each Icode function definition, the remaining information (exports
%% and attributes) associated with the module definition is not included
%% in the resulting Icode.</p>
%%
%% @see function/4
%% @see cerl_hipeify:transform/1

module(E, Options) ->
    module_1(cerl_hipeify:transform(E, Options), Options).

module_1(E, Options) ->
    M = cerl:atom_val(cerl:module_name(E)),
    if atom(M) ->
	    ok;
       true ->
	    error_msg("bad module name: ~P.", [M, 5]),
	    throw(error)
    end,
    S0 = init(M),
    S =  s__set_pmatch(proplists:get_value(pmatch, Options), S0),
    {Icode, _} = mapfoldl(fun function_definition/2,
			  S, cerl:module_defs(E)),
    Icode.

%% For now, we simply assume that all function bodies should have degree
%% one (i.e., return exactly one value). We clear the code ackumulator
%% before we start compiling each function.

function_definition({V, F}, S) ->
    S1 = s__set_code([], S),
    {Icode, S2} = function_1(cerl:var_name(V), F, 1, S1),
    {{icode_icode_name(Icode), Icode}, S2}.

init(Module) ->
    reset_label_counter(),		     
    s__new(Module).

%% @spec function(Module::atom(), Name::atom(), Function::cerl()) ->
%%           icode()
%% @equiv function(Module, Name, Fun, 1)

-ifndef(NO_UNUSED).
function(Module, Name, Fun) ->
    function(Module, Name, Fun, 1).
-endif.	% NO_UNUSED
%% @clear

%% @spec function(Module::atom(), Name::{atom(), integer()},
%%                Fun::cerl(), Degree::integer()) -> icode()
%%
%% @doc Transforms a Core Erlang function to a HiPE Icode function
%% definition. `Fun' must represent a fun-expression, which may not
%% contain free variables. `Module' and `Name' specify the module and
%% function name of the resulting Icode function. Note that the arity
%% part of `Name' is not necessarily equivalent to the number of
%% parameters of `Fun' (this can happen e.g., for lifted closure
%% functions).
%%
%% <p>`Degree' specifies the number of values the function is expected
%% to return; this is typically 1 (one); cf. {@link function/3}.</p>
%%
%% <p>Notes: 
%% <ul>
%%   <li>This function assumes that the code has been transformed into a
%%   very simple and explicit form, using the {@link cerl_hipeify}
%%   module.</li>
%%
%%   <li>Several primops (see "`cerl_hipe_primops.hrl'") are
%%   detected by the translation and handled specially.</li>
%%
%%   <li>Tail call optimization is handled, even when the call is
%%   "hidden" by let-definitions.</li>
%%
%%   <li>It is assumed that all `primop' calls in the code represent
%%   Icode primops or macro instructions, and that all inter-module
%%   calls (both calls to statically named functions, and dynamic
%%   meta-calls) represent <em>actual</em> inter-module calls - not
%%   primitive or built-in operations.</li>
%%
%%   <li>The following special form:
%%     ```case Test of
%%            'true' when 'true' -> True
%%            'false' when 'true' -> False
%%        end'''
%%   is recognized as an if-then-else switch where `Test' is known
%%   to always yield 'true' or 'false'. Efficient jumping code is
%%   generated for such expressions, in particular if nested. Note that
%%   there must be exactly two clauses; order is not important.</li>
%%
%%   <li>Compilation of clauses is simplistic. No pattern matching
%%   compilation or similar optimizations is done at this stage. Guards
%%   that are `true' or `false' are recognized as trivially true/false;
%%   for all other guards, code will be generated. Catch-all clauses
%%   (with `true' guard and variable-only patterns) are detected, and
%%   any following clauses are discarded.</li>
%% </ul></p>
%%
%% <p><b>Important</b>: This function does not handle occurrences of
%% fun-expressions in the body of `Fun', nor `apply'-expressions whose
%% operators are not locally bound function variables. These must be
%% transformed away before this function is called, by closure
%% conversion ({@link cerl_cconv}) using the `make_fun' and `call_fun'
%% primitive operations to create and apply functional values.</p>
%%
%% <p>`receive'-expressions are expected to have a particular
%% form:
%% <ul>
%%   <li>There must be exactly one clause, with the atom
%%   `true' as guard, and only a single variable as pattern.
%%   The variable will be bound to a message in the mailbox, and can be
%%   referred to in the clause body.</li>
%%
%%   <li>In the body of that clause, all paths must execute one of the
%%   primitive operations `receive_select/0' or
%%   `receive_next/0' before another
%%   `receive'-statement might be executed.
%%   `receive_select/0' always returns, but without a value,
%%   while `receive_next/0' never returns, either causing
%%   the nearest surrounding receive-expression to be re-tried with the
%%   next message in the input queue, or timing out.</li>
%% </ul></p>
%%
%% @see function/3

-include("cerl_hipe_primops.hrl").

%% Main translation function:

-ifndef(NO_UNUSED).
function(Module, Name, Fun, Degree) ->
    S = init(Module),
    {Icode, _} = function_1(Name, Fun, Degree, S),
    Icode.
-endif.	% NO_UNUSED
%% @clear

function_1(Name, Fun, Degree, S) ->
    reset_var_counter(),
    LowV = max_var(),
    LowL = max_label(),
    %% Create input variables for the function parameters, and a list of
    %% target variables for the result of the function.
    Args = cerl:fun_vars(Fun),
    RealArity = length(Args),
    ArgType = get_type(Args),
    Vs = make_vars(RealArity),
    Vs1 = make_vars(RealArity),    % input variable temporaries
    Ts = make_vars(Degree),

    %% Initialise environment and context.
    Env = bind_vars(Args, Vs, env__new()),
    %% TODO: if the function returns no values, we can use effect mode
    Ctxt = #ctxt{final = true, effect = false},
    %% Each basic block must begin with a label. Note that we
    %% immediately transfer the input parameters to local variables, for
    %% our self-recursive calling convention.
    Start = new_label(),
    Local = new_label(),
    S1 = add_code([icode_label(Start)]
		  ++ make_moves(Vs, Vs1)
		  ++ [icode_label(Local)],
		  s__set_function(Name, S)),
    S2 = expr(cerl:fun_body(Fun), Ts, Ctxt, Env,
	      s__set_local_entry({Local, Vs}, S1)),

    %% This creates an Icode function definition. The ranges of used
    %% variables and labels below should be nonempty. Note that the
    %% input variables for the Icode function are `Vs1', which will be
    %% transferred to `Vs' (see above).
    HighV = new_var(),    % assure nonempty range
    HighL = max_label(),
    Closure = lists:member(closure, cerl:get_ann(Fun)),
    Module = s__get_module(S2),
    Code = s__get_code(S2),
    Function = icode_icode(Module, Name, Vs1, Closure, Code,
			   {LowV, HighV}, {LowL, HighL}, ArgType),
    {Function, S2}.


%% ---------------------------------------------------------------------
%% Main expression handler

expr(E, Ts, Ctxt, Env, S0) ->
    %% Insert source code position information
    case get_line(cerl:get_ann(E)) of
	none ->
	    expr_1(E, Ts, Ctxt, Env, S0);
	Line when Line > Ctxt#ctxt.line ->
	    Txt = "Line: " ++ integer_to_list(Line),
	    S1 = add_code([icode_comment(Txt)], S0),
	    expr_1(E, Ts, Ctxt#ctxt{line = Line}, Env, S1);
	_ ->
	    expr_1(E, Ts, Ctxt, Env, S0)
    end.

expr_1(E, Ts, Ctxt, Env, S) ->
    case cerl:type(E) of
	var ->
	    expr_var(E, Ts, Ctxt, Env, S);
	literal ->
	    expr_literal(E, Ts, Ctxt, S);
	values ->
	    expr_values(E, Ts, Ctxt, Env, S);
	tuple ->
	    %% (The unit tuple `{}' is a literal, handled above.)
	    expr_tuple(E, Ts, Ctxt, Env, S);
	cons ->
	    expr_cons(E, Ts, Ctxt, Env, S);
	'let' ->
	    expr_let(E, Ts, Ctxt, Env, S);
	seq ->
	    expr_seq(E, Ts, Ctxt, Env, S);
	apply ->
	    expr_apply(E, Ts, Ctxt, Env, S);
	call ->
	    expr_call(E, Ts, Ctxt, Env, S);
	primop ->
	    expr_primop(E, Ts, Ctxt, Env, S);
	'case' ->
	    expr_case(E, Ts, Ctxt, Env, S);
	'receive' ->
	    expr_receive(E, Ts, Ctxt, Env, S);
	'try' ->
	    expr_try(E, Ts, Ctxt, Env, S);
	binary ->
	    expr_binary(E, Ts, Ctxt, Env, S);
	letrec ->
	    expr_letrec(E, Ts, Ctxt, Env, S);
	'fun' ->
	    error_msg("cannot handle fun-valued expressions; "
		      "must be closure converted."),
	    throw(error)
    end.

%% This is for when we need new target variables for all of the
%% expressions in the list, and evaluate them for value in a
%% non-tail-call context.

expr_list(Es, Ctxt, Env, S) ->
    Ctxt1 = Ctxt#ctxt{effect = false, final = false},
    mapfoldl(fun (E0, S0) ->
		     V = make_var(),
		     {V, expr(E0, [V], Ctxt1, Env, S0)}
	     end,
	     S, Es).

%% This is for when we already have the target variables. It is expected
%% that each expression in the list has degree one, so the result can be
%% assigned to the corresponding variable.

exprs([E | Es], [V | Vs], Ctxt, Env, S) ->
    S1 = expr(E, [V], Ctxt, Env, S),
    exprs(Es, Vs, Ctxt, Env, S1);
exprs([], [], _Ctxt, _Env, S) ->
    S;
exprs([], _, _Ctxt, _Env, S) ->
    warning_low_degree(),
    S;
exprs(_, [], _Ctxt, _Env, _S) ->
    error_high_degree(),
    throw(error).

get_line([L | _As]) when integer(L) ->
    L;
get_line([_ | As]) ->
    get_line(As);
get_line([]) ->
    none.


%% ---------------------------------------------------------------------
%% Variables

expr_var(_E, _Ts, #ctxt{effect = true}, _Env, S) ->
    S;
expr_var(E, Ts, Ctxt, Env, S) ->
    Name = cerl:var_name(E),
    case env__lookup(Name, Env) of
	error ->
	    %% Either an undefined variable or an attempt to use a local
	    %% function name as a value.
	    case Name of
		{N,A} when atom(N), integer(A) ->
		    %% error_fun_value(Name);
		    error_msg("cannot handle fun-values outside call context; "
			      "must be closure converted: ~P.",
			      [Name, 5]),
		    throw(error);
		_ ->
		    error_msg("undefined variable: ~P.", [Name, 5]),
		    throw(error)
	    end;
	{ok, #var{name = V}} ->
	    case Ctxt#ctxt.final of
		false ->
		    glue([V], Ts, S);
		true ->
		    add_return([V], S)
	    end;
	{ok, #'fun'{}} ->
	    %% A letrec-defined function name, used as a value.
	    %% error_fun_value(Name)
	    error_msg("cannot handle fun-values outside call context; "
		      "must be closure converted: ~P.",
		      [Name, 5]),
	    throw(error)
    end.

%% The function has been inlined manually above to suppress message by Dialyzer
%% error_fun_value(Name) ->
%%    error_msg("cannot handle fun-values outside call context; "
%%	      "must be closure converted: ~P.",
%%	      [Name, 5]),
%%    throw(error).

%% ---------------------------------------------------------------------
%% This handles all constants, both atomic and compound:

expr_literal(_E, _Ts, #ctxt{effect = true}, S) ->
    S;
expr_literal(E, [V] = Ts, Ctxt, S) ->
    Code = [icode_move(V, icode_const(cerl:concrete(E)))],
    maybe_return(Ts, Ctxt, add_code(Code, S));
expr_literal(E, Ts, _Ctxt, _S) ->
    error_degree_mismatch(length(Ts), E),
    throw(error).

%% ---------------------------------------------------------------------
%% Multiple value aggregate <X1,...,Xn>

expr_values(E, Ts, #ctxt{effect = true} = Ctxt, Env, S) ->
    {_, S1} = exprs(cerl:values_es(E), Ts, Ctxt#ctxt{final = false},
		    Env, S),
    S1;
expr_values(E, Ts, Ctxt, Env, S) ->
    S1 = exprs(cerl:values_es(E), Ts, Ctxt#ctxt{final = false}, Env, S),
    maybe_return(Ts, Ctxt, S1).

%% ---------------------------------------------------------------------
%% Nonconstant tuples

expr_tuple(E, _Ts, #ctxt{effect = true} = Ctxt, Env, S) ->
    {_Vs, S1} = expr_list(cerl:tuple_es(E), Ctxt, Env, S),
    S1;
expr_tuple(E, [_V] = Ts, Ctxt, Env, S) ->
    {Vs, S1} = expr_list(cerl:tuple_es(E), Ctxt, Env, S),
    add_code(make_op(?OP_TUPLE, Ts, Vs, Ctxt), S1);
expr_tuple(E, Ts, _Ctxt, _Env, _S) ->
    error_degree_mismatch(length(Ts), E),
    throw(error).

%% ---------------------------------------------------------------------
%% Nonconstant cons cells

expr_cons(E, _Ts, #ctxt{effect = true} = Ctxt, Env, S) ->
    {_Vs, S1} =expr_list([cerl:cons_hd(E), cerl:cons_tl(E)], Ctxt, Env, S),
    S1;
expr_cons(E, [_V] = Ts, Ctxt, Env, S) ->
    {Vs, S1} = expr_list([cerl:cons_hd(E), cerl:cons_tl(E)],
			 Ctxt, Env, S),
    add_code(make_op(?OP_CONS, Ts, Vs, Ctxt), S1);
expr_cons(E, Ts, _Ctxt, _Env, _S) ->
    error_degree_mismatch(length(Ts), E),
    throw(error).

%% ---------------------------------------------------------------------
%% Let-expressions

%% We want to make sure we are not easily tricked by expressions hidden
%% in contexts like "let X = Expr in X"; this should not destroy tail
%% call properties.

expr_let(E, Ts, Ctxt, Env, S) ->
    F = fun (B, Ctxt, Env, S) -> expr(B, Ts, Ctxt, Env, S) end,
    expr_let_1(E, F, Ctxt, Env, S).

expr_let_1(E, F, Ctxt, Env, S) ->
    E1 = cerl_lib:reduce_expr(E),
    case cerl:is_c_let(E1) of
	true ->
	    expr_let_2(E1, F, Ctxt, Env, S);
	false ->
	    %% Redispatch the new expression.
	    F(E1, Ctxt, Env, S)
    end.

expr_let_2(E, F, Ctxt, Env, S) ->
    Vars = cerl:let_vars(E),
    Vs = make_vars(length(Vars)),
    S1 = expr(cerl:let_arg(E), Vs,
	      Ctxt#ctxt{effect = false, final = false}, Env, S),
    Env1 = bind_vars(Vars, Vs, Env),
    F(cerl:let_body(E), Ctxt, Env1, S1).

%% ---------------------------------------------------------------------
%% Sequencing

%% To compile a sequencing operator, we generate code for effect only
%% for the first expression (the "argument") and then use the
%% surrounding context for the second expression (the "body"). Note that
%% we always create a new dummy target variable; this is necessary for
%% many ICode operations, even if the result is not used.

expr_seq(E, Ts, Ctxt, Env, S) ->
    F = fun (B, Ctxt, Env, S) -> expr(B, Ts, Ctxt, Env, S) end,
    expr_seq_1(E, F, Ctxt, Env, S).

expr_seq_1(E, F, Ctxt, Env, S) ->
    Ctxt1 = Ctxt#ctxt{effect = true, final = false},
    S1 = expr(cerl:seq_arg(E), [make_var()], Ctxt1, Env, S),
    F(cerl:seq_body(E), Ctxt, Env, S1).

%% ---------------------------------------------------------------------
%% Binaries

expr_binary(E, [V]=Ts, Ctxt, Env, S) ->
    Offset=make_reg(),
    Base=make_reg(),
    Segs = cerl:binary_segments(E),
    S1 =  case do_size_code(Segs, S, Env, Ctxt) of
	      {const, S0, Size} ->
		  add_code([icode_call_primop([V, Base, Offset], 
					      {hipe_bs_primop, 
					       {bs_init2, Size, 0}},
					      [])], S0);
	      {var, S0, SizeVar} ->
		   add_code([icode_call_primop([V, Base, Offset], 
					       {hipe_bs_primop, 
						{bs_init2, 0}}, 
					       [SizeVar])], S0)
	  end,
    Vars = make_vars(length(Segs)),
    S2 = binary_segments(Segs, Vars, Ctxt, Env, S1, 0, Base, Offset), 
    maybe_return(Ts, Ctxt, S2).

do_size_code(Segs, S, Env, Ctxt) ->
    case do_size_code(Segs, S, Env, cerl:c_int(0), [], []) of
	{[], [], Const, S1} ->
	    {const, S1, (cerl:concrete(Const) + 7) div 8};
	{Pairs, Bins, Const, S1} ->
	    V1=make_var(),
	    S2=add_code([icode_move(V1, icode_const(cerl:int_val(Const)))],S1),
	    {S3, SizeVar}=create_size_code(Pairs, Bins, Ctxt, V1, S2),
	    {var, S3, SizeVar}
    end.

do_size_code([Seg|Rest], S, Env, Const, Pairs, Bins) ->
    Size = cerl:bitstr_size(Seg),
    Unit = cerl:bitstr_unit(Seg),
    Val = cerl:bitstr_val(Seg),
    case calculate_size(Unit, Size, 0, Env, S) of
	{NewVal, [], S, _} ->
	    do_size_code(Rest, S, Env, add_val(NewVal, Const), Pairs, Bins);
	{UnitVal, [Var], S1, _} ->
	    do_size_code(Rest, S1, Env, Const, [{UnitVal,Var}|Pairs], Bins);
	{all, _, S} ->
	    Binary = make_var(),
	    S1 = expr(Val, [Binary], #ctxt{final=false}, Env, S), 
	    do_size_code(Rest, S1, Env, Const, Pairs, [{all,Binary}|Bins])
    end;
do_size_code([], S, _Env, Const, Pairs, Bins) ->
    {Pairs, Bins, Const, S}.

add_val(NewVal, Const) ->
    cerl:c_int(NewVal + cerl:concrete(Const)).

create_size_code([{UnitVal, Var}|Rest], Bins, Ctxt, Old, S0) ->
    Dst=make_var(),
    S=make_bs_add(UnitVal, Old, Var, Dst, Ctxt, S0),
    create_size_code(Rest, Bins, Ctxt, Dst, S);
create_size_code([], Bins, Ctxt, Old, S0) -> 
    Dst=make_var(),
    S=make_bs_bits_to_bytes(Old, Dst, Ctxt, S0),
    create_size_code(Bins, Ctxt, Dst, S).

create_size_code([{all,Bin}|Rest], Ctxt, Old, S0) ->
    Dst=make_var(),
    S=make_binary_size(Old, Bin, Dst, Ctxt, S0),
    create_size_code(Rest, Ctxt, Dst, S);
create_size_code([], _Ctxt, Dst, S) ->
    {S, Dst}.

make_bs_add(Unit, Old, Var, Dst, #ctxt{fail=FL, class=guard}, S0) ->
    SL=new_label(),
    add_code([icode_guardop([Dst], {hipe_bs_primop,{bs_add, Unit}}, [Old, Var]
			    , SL, FL),
	      icode_label(SL)], S0);
make_bs_add(Unit, Old, Var, Dst, _Ctxt, S0) ->
     add_code([icode_call_primop([Dst], {hipe_bs_primop,{bs_add, Unit}}, 
				 [Old, Var])], S0).

make_bs_bits_to_bytes(Old, Dst, #ctxt{fail=FL, class=guard}, S0) -> 
    SL=new_label(),
    add_code([icode_guardop([Dst], {hipe_bs_primop, bs_bits_to_bytes}, [Old], 
			    SL, FL),
	      icode_label(SL)], S0);
make_bs_bits_to_bytes(Old, Dst, _Ctxt, S0) ->
    add_code([icode_call_primop([Dst], {hipe_bs_primop,bs_bits_to_bytes}, 
				[Old])], S0).

make_binary_size(Old, Bin, Dst, #ctxt{fail=FL, class=guard}, S0) -> 
    SL1=new_label(),
    SL2=new_label(),
    add_code([icode_guardop([Dst], {erlang, size, 1}, [Bin], SL1, FL),
	      icode_label(SL1),
	      icode_guardop([Dst], '+', [Old, Dst], SL2, FL),
	      icode_label(SL2)], S0);
make_binary_size(Old, Bin, Dst, _Ctxt, S0) ->
    add_code([icode_call_primop([Dst], {erlang, size, 1}, [Bin]),
	      icode_call_primop([Dst], '+', [Old, Dst])], S0).

binary_segments(SegList, TList, Ctxt=#ctxt{}, Env, S, Align, Base,
		Offset) ->
    case do_const_segs(SegList, TList, S, Align, Base, Offset) of
	{[Seg|Rest], [T|Ts], S1} ->
	    {S2, NewAlign} = bitstr(Seg, [T], Ctxt, Env, S1, Align,
				    Base, Offset),
	    binary_segments(Rest, Ts, Ctxt, Env, S2, NewAlign, Base,
			    Offset);
	{[], [], S1} ->
	    S1
    end.

do_const_segs(SegList, TList, S, Align, Base, Offset) ->
    case get_segs(SegList, TList, [], 0, {[], SegList, TList}) of
	{[], SegList, TList} ->
	    {SegList, TList, S};
	{ConstSegs, RestSegs, RestT} ->
	    String=create_string(ConstSegs, <<>>, 0),
	    Flags=translate_flags1([], Align),
	    Name = {bs_put_string, String, length(String), Flags},
	    {RestSegs, RestT, add_code([icode_call_primop(
					  [Offset], 
					  {hipe_bs_primop, Name},
					  [Base, Offset])], S)}
    end.
	     
get_segs([Seg|Rest], [_|RestT], Acc, AccSize, BestPresent) ->
    Size = cerl:bitstr_size(Seg),
    Unit = cerl:bitstr_unit(Seg),
    Val = cerl:bitstr_val(Seg),
    case allowed(Size, Unit, Val, AccSize) of
	{true, NewAccSize} ->
	    case Acc of
		[] ->
		     get_segs(Rest, RestT, [Seg|Acc], NewAccSize, BestPresent);
		_ ->
		    get_segs(Rest, RestT, [Seg|Acc], NewAccSize, 
			     {lists:reverse([Seg|Acc]), Rest, RestT})
	    end;
	{possible, NewAccSize} ->
	    get_segs(Rest, RestT, [Seg|Acc], NewAccSize, BestPresent);
	false ->
	    BestPresent
    end;
get_segs([], [], _Acc, _AccSize, Best) ->
    Best.

		
create_string([Seg|Rest], Bin, TotalSize) ->
    Size = cerl:bitstr_size(Seg),
    Unit = cerl:bitstr_unit(Seg), 
    NewSize = cerl:int_val(Size) * cerl:int_val(Unit),
    LitVal = cerl:concrete(cerl:bitstr_val(Seg)),
    LiteralFlags = cerl:bitstr_flags(Seg),
    FlagVal = translate_flags(LiteralFlags, []),
    NewTotalSize = NewSize + TotalSize,
    Pad = (8 - NewTotalSize rem 8) rem 8,
    case cerl:concrete(cerl:bitstr_type(Seg)) of
	integer ->
	    case {FlagVal band 2, FlagVal band 4} of
		{2, 4} ->
		    NewBin = <<Bin:TotalSize/binary-unit:1, 
			      LitVal:NewSize/integer-little-signed, 0:Pad>>,
		    NewBin;
		{0, 4} ->
		    NewBin = <<Bin:TotalSize/binary-unit:1, 
			      LitVal:NewSize/integer-signed, 0:Pad>>,
		    NewBin;
		{2, 0} ->
		    NewBin = <<Bin:TotalSize/binary-unit:1, 
			      LitVal:NewSize/integer-little, 0:Pad>>,
		    NewBin;
		{0, 0} ->
		    NewBin = <<Bin:TotalSize/binary-unit:1, 
			      LitVal:NewSize/integer, 0:Pad>>,
		    NewBin
	    end;
	float ->
	    case FlagVal band 2 of
		2 ->
		    NewBin = <<Bin:TotalSize/binary-unit:1, LitVal:NewSize/float-little, 0:Pad>>,
		    NewBin;
		0 ->
		    NewBin = <<Bin:TotalSize/binary-unit:1, LitVal:NewSize/float, 0:Pad>>,
		    NewBin
	    end
    end,
    create_string(Rest, NewBin, NewTotalSize);

create_string([], Bin, _Size) ->
    binary_to_list(Bin).
		
allowed(Size, Unit, Val, AccSize) ->
    case {cerl:is_c_int(Size), cerl:is_literal(Val)} of
	{true, true} ->
	    NewAccSize = cerl:int_val(Size) * cerl:int_val(Unit) + AccSize,
	    case NewAccSize rem 8 of
		0 ->
		    {true, NewAccSize};
		_ ->
		    {possible, NewAccSize}
	    end;
	_ ->
	    false
    end.

bitstr(E, Ts, Ctxt, Env, S, Align, Base, Offset) ->
    Size = cerl:bitstr_size(E),
    Unit = cerl:bitstr_unit(E),
    LiteralFlags = cerl:bitstr_flags(E),
    Val = cerl:bitstr_val(E),
    Type = cerl:concrete(cerl:bitstr_type(E)),
    S0 = expr(Val, Ts, Ctxt#ctxt{final = false, effect = false}, Env, S),
    ConstInfo = get_const_info(Val, Type),
    Flags = translate_flags(LiteralFlags, Align),
    SizeInfo = calculate_size(Unit, Size, Align, Env, S0),
    bitstr_gen_op(Ts, Ctxt, SizeInfo, ConstInfo, Type, Flags, Base, Offset).

bitstr_gen_op([V], #ctxt{fail=FL, class=guard}, SizeInfo, ConstInfo,
	       Type, Flags, Base, Offset) ->
    SL = new_label(),
    case SizeInfo of
	{NewUnit, NewArgs, S1, NewAlign} ->
	    Args = [V|NewArgs] ++ [Base, Offset],
	    Name =
		case Type of
		    integer ->
			{bs_put_integer, NewUnit, Flags, ConstInfo};
		    float ->
			{bs_put_float, NewUnit, Flags, ConstInfo};
		    binary ->
			{bs_put_binary, NewUnit, Flags}
		end,
	    {add_code([icode_guardop([Offset], {hipe_bs_primop, Name} ,Args, SL, FL),
		       icode_label(SL)], S1), NewAlign};
	{all, NewAlign, S1} ->
	    Type = binary,
	    Name = {bs_put_binary_all, Flags},
	    {add_code([icode_guardop([Offset], {hipe_bs_primop, Name}, [V, Base, Offset], SL, FL),
		       icode_label(SL)], S1), NewAlign}
    end;

bitstr_gen_op([V], _Ctxt, SizeInfo, ConstInfo, Type, Flags, Base,
	      Offset) ->
    case SizeInfo of
	{NewUnit, NewArgs, S, NewAlign} ->
	    Args = [V|NewArgs] ++ [Base, Offset],
	    Name =
		case Type of
		    integer ->
			{bs_put_integer, NewUnit, Flags, ConstInfo};
		    float ->
			{bs_put_float, NewUnit, Flags, ConstInfo};
		    binary ->
			{bs_put_binary, NewUnit, Flags}
		end,
	    {add_code([icode_call_primop([Offset], {hipe_bs_primop, Name} ,Args)], S), 
	     NewAlign};
	{all, NewAlign, S} ->
	    Type = binary,
	    Name = {bs_put_binary_all, Flags},
	    {add_code([icode_call_primop([Offset], {hipe_bs_primop, Name}, 
					 [V, Base, Offset])], S), 
	     NewAlign}
    end.

%% ---------------------------------------------------------------------
%% Apply-expressions

%% Note that the arity of the called function only depends on the length
%% of the argument list; the arity stated by the function name is
%% ignored.

expr_apply(E, Ts, Ctxt, Env, S) ->
    Op = cerl_lib:reduce_expr(cerl:apply_op(E)),
    {Vs, S1} = expr_list(cerl:apply_args(E), Ctxt, Env, S),
    case cerl:is_c_var(Op) of
	true ->
	    case cerl:var_name(Op) of
		{N, A} = V when atom(N), integer(A) ->
		    case env__lookup(V, Env) of
			error ->
			    %% Assumed to be a function in the
			    %% current module; we don't check.
			    add_local_call(V, Vs, Ts, Ctxt, S1,
					   get_type(E));
			{ok, #'fun'{label = L, vars = Vs1}} ->
			    %% Call to a local letrec-bound function.
			    add_letrec_call(L, Vs1, Vs, Ctxt, S1);
			{ok, #var{}} ->
			    error_msg("cannot call via variable; must "
				      "be closure converted: ~P.",
				      [V, 5]),
			    throw(error)
		    end;
		_ ->
		    error_nonlocal_application(Op),
		    throw(error)
	    end;
	false ->
	    error_nonlocal_application(Op),
	    throw(error)
    end.

%% ---------------------------------------------------------------------
%% Call-expressions

%% Unless we know the module and function names statically, we have to
%% go through the meta-call operator for a static number of arguments.

expr_call(E, Ts, Ctxt, Env, S) ->
    Module = cerl_lib:reduce_expr(cerl:call_module(E)),
    Name = cerl_lib:reduce_expr(cerl:call_name(E)),
    case cerl:is_c_atom(Module) and cerl:is_c_atom(Name) of
	true ->
	    M = cerl:atom_val(Module),
	    F = cerl:atom_val(Name),
	    {Vs, S1} = expr_list(cerl:call_args(E), Ctxt, Env, S),
	    add_code(make_call(M, F, Ts, Vs, Ctxt), S1);
	false ->
	    Args = cerl:call_args(E),
	    N = length(Args),
	    {Vs, S1} = expr_list([Module, Name | Args], Ctxt, Env, S),
	    add_code(make_op(?OP_APPLY_FIXARITY(N), Ts, Vs, Ctxt), S1)
    end.

%% ---------------------------------------------------------------------
%% Primop calls

%% Core Erlang primop calls are generally mapped directly to Icode
%% primop calls, with a few exceptions (listed above), which are
%% expanded inline, sometimes depending on context. Note that primop
%% calls do not have specialized tail-call forms.

expr_primop(E, Ts, Ctxt, Env, S) ->
    Name = cerl:atom_val(cerl:primop_name(E)),
    As = cerl:primop_args(E),
    Arity = length(As),
    expr_primop_0(Name, Arity, As, E, Ts, Ctxt, Env, S).

expr_primop_0(Name, Arity, As, E, Ts, #ctxt{effect = true} = Ctxt, Env,
	      S) ->
    case is_safe_op(Name, Arity) of
	true ->
	    %% Just drop the operation; cf. 'expr_values(...)'.
	    {_, S1} = expr_list(As, Ctxt, Env, S),
	    S1;
	false ->
	    expr_primop_1(Name, Arity, As, E, Ts,
			  Ctxt#ctxt{effect = false}, Env, S)
    end;
expr_primop_0(Name, Arity, As, E, Ts, Ctxt, Env, S) ->
    expr_primop_1(Name, Arity, As, E, Ts, Ctxt, Env, S).

%% Some primops must be caught before their arguments are visited.

expr_primop_1(?PRIMOP_MAKE_FUN, 6, As, _E, Ts, Ctxt, Env, S) ->
    primop_make_fun(As, Ts, Ctxt, Env, S);
expr_primop_1(?PRIMOP_APPLY_FUN, 2, As, _E, Ts, Ctxt, Env, S) ->
    primop_apply_fun(As, Ts, Ctxt, Env, S);
expr_primop_1(?PRIMOP_FUN_ELEMENT, 2, As, _E, Ts, Ctxt, Env, S) ->
    primop_fun_element(As, Ts, Ctxt, Env, S);
expr_primop_1(?PRIMOP_DSETELEMENT, 3, As, _E, Ts, Ctxt, Env, S) ->
    primop_dsetelement(As, Ts, Ctxt, Env, S);
expr_primop_1(?PRIMOP_RECEIVE_SELECT, 0, _As, _E, Ts, Ctxt, _Env, S) ->
    primop_receive_select(Ts, Ctxt, S);
expr_primop_1(?PRIMOP_RECEIVE_NEXT, 0, _As, _E, _Ts, Ctxt, _Env, S) ->
    primop_receive_next(Ctxt, S);
expr_primop_1(?PRIMOP_IDENTITY, 1, [A], _E, Ts, Ctxt, Env, S) ->
    expr(A, Ts, Ctxt, Env, S);  % used for unary plus
expr_primop_1(?PRIMOP_NEG, 1, [A], _, Ts, Ctxt, Env, S) ->
    E = cerl:c_primop(cerl:c_atom('-'), [cerl:c_int(0), A]),
    expr_primop(E, Ts, Ctxt, Env, S);
expr_primop_1(?PRIMOP_GOTO_LABEL, 1, [A], _, _Ts, _Ctxt, _Env, S) ->
    primop_goto_label(A, S);
expr_primop_1(?PRIMOP_REDUCTION_TEST, 0, [], _, _Ts, Ctxt, _Env, S) ->
    primop_reduction_test(Ctxt, S);
expr_primop_1(Name, Arity, As, E, Ts, Ctxt, Env, S) ->
    Bool = case is_bool_op(Name, Arity) of
	       true ->
		   true;
	       false ->
		   case is_comp_op(Name, Arity) of
		       true ->
			   true;
		       false ->
			   is_type_test(Name, Arity)
		   end
	   end,
    case Bool of
	true ->
	    boolean_expr(E, Ts, Ctxt, Env, S);
	false ->
	    {Vs, S1} = expr_list(As, Ctxt, Env, S),
	    expr_primop_2(Name, Arity, Vs, Ts, Ctxt, S1)
    end.

expr_primop_2(?PRIMOP_ELEMENT, 2, Vs, Ts, Ctxt, S) ->
    add_code(make_op(?OP_ELEMENT, Ts, Vs, Ctxt), S);
expr_primop_2(?PRIMOP_EXIT, 1, [V], _Ts, Ctxt, S) ->
    add_exit(V, Ctxt, S);
expr_primop_2(?PRIMOP_THROW, 1, [V], _Ts, Ctxt, S) ->
    add_throw(V, Ctxt, S);
expr_primop_2(?PRIMOP_ERROR, 1, [V], _Ts, Ctxt, S) ->
    add_error(V, Ctxt, S);
expr_primop_2(?PRIMOP_ERROR, 2, [V, F], _Ts, Ctxt, S) ->
    add_error(V, F, Ctxt, S);
expr_primop_2(?PRIMOP_RETHROW, 2, [E, V], _Ts, Ctxt, S) ->
    add_rethrow(E, V, Ctxt, S);
expr_primop_2(Name, _Arity, Vs, Ts, Ctxt, S) ->
    %% Other ops are assumed to be recognized by the backend.
    add_code(make_op(Name, Ts, Vs, Ctxt), S).

%% All of M, F, and A must be literals with the right types.
%% V must represent a proper list.

primop_make_fun([M, F, A, H, I, V] = As, [_T] = Ts, Ctxt, Env, S) ->
    case cerl:is_c_atom(M) and
	cerl:is_c_atom(F) and
	cerl:is_c_int(A) and
	cerl:is_c_int(H) and
	cerl:is_c_int(I) and
	cerl:is_c_list(V) of
	true ->
	    Module = cerl:atom_val(M),
	    Name = cerl:atom_val(F),
	    Arity = cerl:int_val(A),
	    Hash = cerl:int_val(H),
	    Index = cerl:int_val(I),
	    {Vs, S1} = expr_list(cerl:list_elements(V),
				 Ctxt, Env, S),
 	    add_code(make_op(?OP_MAKE_FUN(Module, Name, Arity,
					  Hash, Index),
			     Ts, Vs, Ctxt),
		     S1);
	false ->
	    error_primop_badargs(?PRIMOP_MAKE_FUN, As),
	    throw(error)
    end.

%% V must represent a proper list.

primop_apply_fun([F, V] = As, [_T] = Ts, Ctxt, Env, S) ->
    case cerl:is_c_list(V) of
	true ->
	    %% Note that the closure itself is passed as the last value.
	    {Vs, S1} = expr_list(cerl:list_elements(V) ++ [F],
				 Ctxt, Env, S),
	    case Ctxt#ctxt.final of
		false ->
		    add_code([icode_call_fun(Ts, Vs)], S1);
		true ->
		    add_code([icode_enter_fun(Vs)], S1)
	    end;
	false ->
	    error_primop_badargs(?PRIMOP_APPLY_FUN, As),
	    throw(error)
    end.

primop_fun_element([N, F] = As, Ts, Ctxt, Env, S) ->
    case cerl:is_c_int(N) of
	true ->
	    V = make_var(),
	    S1 = expr(F, [V], Ctxt#ctxt{final = false, effect = false},
		      Env, S),
	    add_code(make_op(?OP_FUN_ELEMENT(cerl:int_val(N)),
			     Ts, [V], Ctxt),
		     S1);
	false ->
	    error_primop_badargs(?PRIMOP_FUN_ELEMENT, As),
	    throw(error)
    end.

primop_goto_label(A, S) ->
    {Label,S1} = s__get_label(A, S),
    add_code([icode_goto(Label)], S1).

is_goto(E) ->
    case cerl:type(E) of 
	primop ->
	    Name = cerl:atom_val(cerl:primop_name(E)),
	    As = cerl:primop_args(E),
	    Arity = length(As),
	    case {Name, Arity} of
		{?PRIMOP_GOTO_LABEL, 1} ->
		    true;
		_ ->
		    false
	    end;
	_ ->
	    false
    end.
		       
primop_reduction_test(Ctxt, S) ->
    add_code(make_op(?OP_REDTEST, [], [], Ctxt), S).

primop_dsetelement([N | As1] = As, Ts, Ctxt, Env, S) ->
    case cerl:is_c_int(N) of
	true ->
	    {Vs, S1} = expr_list(As1, Ctxt, Env, S),
	    add_code(make_op(?OP_UNSAFE_SETELEMENT(cerl:int_val(N)),
			     Ts, Vs, Ctxt),
		     S1);
	false ->
	    error_primop_badargs(?PRIMOP_DSETELEMENT, As),
	    throw(error)
    end.

%% ---------------------------------------------------------------------
%% Try-expressions:

%% We want to rewrite trivial things like `try A of X -> B catch ...',
%% where A is safe, into a simple let-binding `let X = A in B', avoiding
%% unnecessary try-blocks. (The `let' might become further simplified.)

expr_try(E, Ts, Ctxt, Env, S) ->
    F = fun (B, Ctxt, Env, S) -> expr(B, Ts, Ctxt, Env, S) end,
    expr_try_1(E, F, Ctxt, Env, S).

expr_try_1(E, F, Ctxt, Env, S) ->
    A = cerl:try_arg(E),
    case is_safe_expr(A) of
	true ->
	    E1 = cerl:c_let(cerl:try_vars(E), A, cerl:try_body(E)),
	    expr_let_1(E1, F, Ctxt, Env, S);
	false ->
	    expr_try_2(E, F, Ctxt, Env, S)
    end.

%% TODO: maybe skip begin_try/end_try and just use fail-labels...

expr_try_2(E, F, Ctxt, Env, S) ->
    Cont = new_continuation_label(Ctxt),
    Catch = new_label(),
    Next = new_label(),
    S1 = add_code([icode_begin_try(Catch,Next),icode_label(Next)], S),
    Vars = cerl:try_vars(E),
    Vs = make_vars(length(Vars)),
    Ctxt1 = Ctxt#ctxt{final = false},
    S2 = expr(cerl:try_arg(E), Vs, Ctxt1, Env, S1),
    Env1 = bind_vars(Vars, Vs, Env),
    S3 = add_code([icode_end_try()], S2),
    S4 = F(cerl:try_body(E), Ctxt, Env1, S3),
    S5 = add_continuation_jump(Cont, Ctxt, S4),
    EVars = cerl:try_evars(E),
    EVs = make_vars(length(EVars)),
    Env2 = bind_vars(EVars, EVs, Env),
    S6 = add_code([icode_label(Catch), icode_begin_handler(EVs)], S5),
    S7 = F(cerl:try_handler(E), Ctxt, Env2, S6),
    add_continuation_label(Cont, Ctxt, S7).

%% ---------------------------------------------------------------------
%% Letrec-expressions (local goto-labels)

%% We only handle letrec-functions as continuations. The fun-bodies are
%% always compiled in the same context as the main letrec-body. Note
%% that we cannot propagate "advanced" contexts like boolean-compilation
%% into the letrec body like we do for ordinary lets or seqs, since the
%% context for an individual local function would be depending on the
%% contexts of its call sites.

expr_letrec(E, Ts, Ctxt, Env, S) ->
    Ds = cerl:letrec_defs(E),
    Env1 = add_defs(Ds, Env),
    S1 = expr(cerl:letrec_body(E), Ts, Ctxt, Env1, S),
    Next = new_continuation_label(Ctxt),
    S2 = add_continuation_jump(Next, Ctxt, S1),
    S3 = defs(Ds, Ts, Ctxt, Env1, S2),
    add_continuation_label(Next, Ctxt, S3).

add_defs([{V, _F} | Ds], Env) ->
    {_, A} = cerl:var_name(V),
    Vs = make_vars(A),
    L = new_label(),
    Env1 = bind_fun(V, L, Vs, Env),
    add_defs(Ds, Env1);
add_defs([], Env) ->
    Env.

defs([{V, F} | Ds], Ts, Ctxt, Env, S) ->
    Name = cerl:var_name(V),
    #'fun'{label = L, vars = Vs} = env__get(Name, Env),
    S1 = add_code([icode_label(L)], S),
    Env1 = bind_vars(cerl:fun_vars(F), Vs, Env),
    S2 = expr(cerl:fun_body(F), Ts, Ctxt, Env1, S1),
    defs(Ds, Ts, Ctxt, Env, S2);
defs([], _Ts, _Ctxt, _Env, S) ->
    S.

%% ---------------------------------------------------------------------
%% Receive-expressions

%% There may only be exactly one clause, which must be a trivial
%% catch-all with exactly one (variable) pattern. Each message will be
%% read from the mailbox and bound to the pattern variable; the body of
%% the clause must do the switching and call either of the primops
%% `receive_select/0' or `receive_next/0'.

expr_receive(E, Ts, Ctxt, Env, S) ->
    F = fun (B, Ctxt, Env, S) -> expr(B, Ts, Ctxt, Env, S) end,
    expr_receive_1(E, F, Ctxt, Env, S).

expr_receive_1(E, F, Ctxt, Env, S) ->
    case cerl:receive_clauses(E) of
	[C] ->
	    case cerl:clause_pats(C) of
		[_] ->
		    case cerl_clauses:is_catchall(C) of
			true ->
			    expr_receive_2(C, E, F, Ctxt, Env, S);
			false ->
			    error_msg("receive-expression clause "
				      "must be a catch-all."),
			    throw(error)
		    end;
		_ ->
		    error_msg("receive-expression clause must "
			      "have exactly one pattern."),
		    throw(error)
	    end;
	_ ->
	    error_msg("receive-expressions must have "
		      "exactly one clause."),
	    throw(error)
    end.

%% There are a number of primitives to do the work involved in receiving
%% messages:
%%
%%	if-tests:	suspend_msg_timeout()
%%
%%	primops:	V = check_get_msg()
%%			select_msg()
%%			next_msg()
%%			set_timeout(T)
%%			clear_timeout()
%%			suspend_msg()
%%
%% `check_get_msg' tests if the mailbox is empty or not, and if not it
%% reads the message currently pointed to by the implicit message pointer.
%% `select_msg' removes the current message from the mailbox, resets the
%% message pointer and clears any timeout. `next_msg' advances the
%% message pointer but does nothing else. `set_timeout(T)' sets up the
%% timeout mechanism *unless it is already set*. `suspend_msg' suspends
%% until a message has arrived and does not check for timeout. The test
%% `suspend_msg_timeout' suspends the process and upon resuming
%% execution selects the `true' branch if a message has arrived and the
%% `false' branch otherwise. `clear_timeout' (the name is misleading)
%% resets the message pointer when a timeout has occurred.
%%
%% Note: the receiving of a message must be performed so that the
%% message pointer is always reset when the receive is done; thus, all
%% paths must go through either `select_msg' or `clear_timeout'.

%% Recall that the `final' and `effect' context flags distribute over
%% the clauses *and* the timeout action (but not over the
%% timeout-expression, which is always executed for its value).

%% This is the code we generate for a full receive:
%%
%%   Loop:	check_get_msg(Match, Wait)
%%   Wait:	set_timeout
%%		suspend_msg_timeout(Loop, Timeout)
%%   Timeout:	clear_timeout
%%		TIMEOUT-ACTION
%%		goto Next
%%   Match:     RECEIVE-CLAUSES(Loop, Next)
%%   Next:	...
%%
%% For a receive with infinity timout, we generate
%%
%%   Wait:	suspend_msg
%%		goto Loop
%%
%% For a receive with zero timout, we generate
%%
%%   Wait:	clear_timeout
%%		TIMEOUT-ACTION
%%		goto Next

expr_receive_2(C, E, F, Ctxt, Env, S0) ->
    Expiry = cerl_lib:reduce_expr(cerl:receive_timeout(E)),
    After = case cerl:is_literal(Expiry) of
		true ->
		    cerl:concrete(Expiry);
		false ->
		    undefined
	    end,
    T = make_var(),    % T will hold the timeout value
    %% It would be harmless to generate code for `infinity', but we
    %% might as well avoid it if we can.
    S1 = if After == 'infinity' -> S0;
	    true ->
		 expr(Expiry, [T],
		      Ctxt#ctxt{final = false, effect = false},
		      Env, S0)
	 end,

    %% This is the top of the receive-loop, which checks if the
    %% mailbox is empty, and otherwise reads the next message.
    Loop = new_label(),
    Wait = new_label(),
    Match = new_label(),
    V = make_var(),
    S2 = add_code([icode_label(Loop),
		   icode_call_primop([V], ?OP_CHECK_GET_MESSAGE, [],
				     Match, Wait),
		   icode_label(Wait)], S1),

    %% The wait-for-message section looks a bit different depending on
    %% whether we actually need to set a timer or not.
    Ctxt0 = #ctxt{},
    S3 = case After of
	     'infinity' ->
		 %% Only wake up when we get new messages, and never
		 %% execute the expiry body.
		 add_code(make_op(?OP_WAIT_FOR_MESSAGE, [], [], Ctxt0)
			  ++ [icode_goto(Loop)], S2);
	     0 ->
		 %% Zero limit - reset the message pointer (this is what
		 %% "clear timeout" does) and execute the expiry body.
		 add_code(make_op(?OP_CLEAR_TIMEOUT, [], [], Ctxt0),
			  S2);
	     _ ->
		 %% Other value - set the timer (if it is already set,
		 %% nothing is changed) and wait for a message or
		 %% timeout. Reset the message pointer upon timeout.
		 Timeout = new_label(),
		 add_code(make_op(?OP_SET_TIMEOUT, [], [T], Ctxt0)
			  ++ [make_if(?TEST_WAIT_FOR_MESSAGE_OR_TIMEOUT,
				      [], Loop, Timeout),
			      icode_label(Timeout)]
			  ++ make_op(?OP_CLEAR_TIMEOUT, [], [], Ctxt0),
			  S2)
	 end,

    %% We never generate code for the expiry body if the timeout value
    %% is 'infinity' (and thus we know that it will not be used), mainly
    %% because in this case it is possible (and legal) for the expiry
    %% body to not have the expected degree. (Typically, it produces a
    %% single constant value such as 'true', while the clauses may be
    %% producing 2 or more values.)
    Next = new_continuation_label(Ctxt),
    S4 = if After == 'infinity' -> S3;
	    true ->
		 add_continuation_jump(Next, Ctxt,
				       F(cerl:receive_action(E), Ctxt,
					 Env, S3))
	 end,

    %% When we compile the primitive operations that select the current
    %% message or loop to try the next message (see the functions
    %% 'primop_receive_next' and 'primop_receive_select'), we will use
    %% the receive-loop label in the context (i.e., that of the nearest
    %% enclosing receive expression).
    Ctxt1 = Ctxt#ctxt{'receive' = #'receive'{loop = Loop}},

    %% The pattern variable of the clause will be mapped to `V', which
    %% holds the message, so it can be accessed in the clause body:
    S5 = clauses([C], F, [V], Ctxt1, Env,
		 add_code([icode_label(Match)], S4)),
    add_continuation_label(Next, Ctxt, S5).

%% Primops supporting "expanded" receive-expressions on the Core level:

primop_receive_next(#ctxt{'receive' = R} = Ctxt, S0) ->
    case R of
	#'receive'{loop = Loop} ->
	    %% Note that this has the same "problem" as the fail
	    %% instruction (see the 'add_fail' function), namely, that
	    %% it unexpectedly ends a basic block. The solution is the
	    %% same - add a dummy label if necessary.
	    S1 = add_code(make_op(?OP_NEXT_MESSAGE, [], [], #ctxt{})
			  ++ [icode_goto(Loop)], S0),
	    L = new_continuation_label(Ctxt),
	    add_continuation_label(L, Ctxt, S1);
	_ ->
	    error_not_in_receive(?PRIMOP_RECEIVE_NEXT),
	    throw(error)
    end.

primop_receive_select(Ts, #ctxt{'receive' = R} = Ctxt, S) ->
    case R of
	#'receive'{} ->
	    add_code(make_op(?OP_SELECT_MESSAGE, Ts, [], Ctxt), S);
	_ ->
	    error_not_in_receive(?PRIMOP_RECEIVE_SELECT),
	    throw(error)
    end.

%% ---------------------------------------------------------------------
%% Case expressions

%% Typically, pattern matching compilation has split all switches into
%% separate groups of tuples, integers, atoms, etc., where each such
%% switch over a group of constructors is protected by a type test.
%% Thus, it is straightforward to generate switch instructions. (If no
%% pattern matching compilation has been done, we don't care about
%% efficiency anyway, so we don't spend any extra effort here.)

expr_case(E, Ts, Ctxt, Env, S) ->
    F = fun (B, Ctxt, Env, S) -> expr(B, Ts, Ctxt, Env, S) end,
    expr_case_1(E, F, Ctxt, Env, S).

expr_case_1(E, F, Ctxt, Env, S) ->
    Cs = cerl:case_clauses(E),
    A = cerl:case_arg(E),
    case cerl_lib:is_bool_switch(Cs) of
	true ->
	    %% An if-then-else with a known boolean argument
	    {True, False} = cerl_lib:bool_switch_cases(Cs),
	    bool_switch(A, True, False, F, Ctxt, Env, S);
	false ->
	    Vs = make_vars(cerl:clause_arity(hd(Cs))),
	    Ctxt1 = Ctxt#ctxt{final = false, effect = false},
	    S1 = expr(A, Vs, Ctxt1, Env, S),
	    expr_case_2(Vs, Cs, F, Ctxt, Env, S1)
    end.

%% Switching on a value

expr_case_2(Vs, Cs, F, Ctxt, Env, S1) ->
    case is_constant_switch(Cs) of
	true ->
	    switch_val_clauses(Cs, F, Vs, Ctxt, Env, S1);
	false ->
	    case is_tuple_switch(Cs) of
		true ->
		    switch_tuple_clauses(Cs, F, Vs, Ctxt, Env, S1);
		false ->
		    case is_binary_switch(Cs, S1) of
			true ->
			    switch_binary_clauses(Cs, F, Vs, Ctxt, Env,
						  S1);
			false ->
			    clauses(Cs, F, Vs, Ctxt, Env, S1)
		    end
	    end
    end.

%% Check if a list of clauses represents a switch over a number (more
%% than 1) of constants (atoms or integers/floats), or tuples (whose
%% elements are all variables)

is_constant_switch(Cs) ->
    is_switch(Cs, fun (P) -> (cerl:type(P) == literal) andalso
				 is_constant(cerl:concrete(P)) end).

is_tuple_switch(Cs) ->
    is_switch(Cs, fun (P) -> cerl:is_c_tuple(P) andalso
				 all_vars(cerl:tuple_es(P)) end).

is_binary_switch(Cs, S) ->
    case s__get_pmatch(S) of
	False when False == false; False == undefined ->
	    false;
	Other when Other == duplicate_all; Other == no_duplicates; Other == true->
	    is_binary_switch1(Cs, 0)
    end.

is_binary_switch1([C|Cs], N) ->
    case cerl:clause_pats(C) of
	[P] ->
	    case cerl:is_c_binary(P) of
		true ->
		    is_binary_switch1(Cs, N + 1);
		false -> 
		    if Cs == [], N > 0 ->
			    %% The final clause may be a catch-all.
			    cerl:type(P) == var;
		       true ->
			    false
		    end
	    end;
	_ ->
	    false
    end;
is_binary_switch1([], N) ->
    N > 0.

all_vars([E | Es]) ->
    case cerl:is_c_var(E) of
	true -> all_vars(Es);
	false -> false
    end;
all_vars([]) -> true.

is_switch(Cs, F) ->
    is_switch(Cs, F, 0).

is_switch([C | Cs], F, N) ->
    case cerl_lib:is_simple_clause(C) of
	true ->
	    [P] = cerl:clause_pats(C),
	    case F(P) of
		true ->
		    is_switch(Cs, F, N + 1);
		false ->
		    if Cs == [], N > 1 ->
			    %% The final clause may be a catch-all.
			    cerl:type(P) == var;
		       true ->
			    false
		    end
	    end;
	false -> false
    end;
is_switch([], _F, N) ->
    N > 1.

switch_val_clauses(Cs, F, Vs, Ctxt, Env, S) ->
    switch_clauses(Cs, F, Vs, Ctxt, Env,
		   fun (P) -> cerl:concrete(P) end,
		   fun icode_switch_val/4,
		   fun val_clause_body/9,
		   S).

val_clause_body(_N, _V, C, F, Next, _Fail, Ctxt, Env, S) ->
    clause_body(C, F, Next, Ctxt, Env, S).

switch_tuple_clauses(Cs, F, Vs, Ctxt, Env, S) ->
    switch_clauses(Cs, F, Vs, Ctxt, Env, 
		   fun (P) -> cerl:tuple_arity(P) end,
		   fun icode_switch_tuple_arity/4,
		   fun tuple_clause_body/9,
		   S).

tuple_clause_body(N, V, C, F, Next, Fail, Ctxt, Env, S0) ->
    Vs = make_vars(N),
    S1 = tuple_elements(Vs, V, S0),
    Es = cerl:tuple_es(hd(cerl:clause_pats(C))),
    {Env1, S2} = patterns(Es, Vs, Fail, Env, S1),
    clause_body(C, F, Next, Ctxt, Env1, S2).

switch_clauses(Cs, F, [V], Ctxt, Env, GetVal, Switch, Body, S0) ->
    Cs1 = [switch_clause(C, GetVal) || C <- Cs],
    Cases = [{V, L} || {V, L, _} <- Cs1],
    Default = [C || {default, C} <- Cs1],
    Fail = new_label(),
    S1 = add_code([Switch(V, Fail, length(Cases), Cases)], S0),
    Next = new_continuation_label(Ctxt),
    S3 = case Default of
	     [] -> add_default_case(Fail, Ctxt, S1);
	     [C] ->
		 %% Bind the catch-all variable (this always succeeds)
		 {Env1, S2} = patterns(cerl:clause_pats(C), [V], Fail,
				       Env, S1),
		 clause_body(C, F, Next, Ctxt, Env1,
			     add_code([icode_label(Fail)], S2))
	 end,
    S4 = switch_cases(Cs1, V, F, Next, Fail, Ctxt, Env, Body, S3),
    add_continuation_label(Next, Ctxt, S4).

switch_clause(C, F) ->
    [P] = cerl:clause_pats(C),
    L = new_label(),
    case cerl:type(P) of
	var -> {default, C};
	_ -> {icode_const(F(P)), L, C}
    end.

switch_binary_clauses(Cs, F, Vs, Ctxt, Env, S) ->
    {Bins, Default} = get_binary_clauses(Cs),
    BMatch = cerl_binary_pattern_match:add_offset_to_bin(Bins),
    Fail = new_label(),
    Next = new_continuation_label(Ctxt),
    S1 = binary_match(BMatch, F, Vs, Next, Fail, Ctxt, Env, S),
    S2 = case Default of
	     [] -> add_default_case(Fail, Ctxt, S1);
	     [C] ->
		 clause_body(C, F, Next, Ctxt, Env,
			     add_code([icode_label(Fail)], S1))
	 end,
    add_continuation_label(Next, Ctxt, S2).
    
get_binary_clauses(Cs) ->    
    get_binary_clauses(Cs, []).

get_binary_clauses([C|Cs], Acc) ->
    [P]=cerl:clause_pats(C),
    case cerl:is_c_binary(P) of 
	true ->
	    get_binary_clauses(Cs, [C|Acc]);
	false ->
	    {lists:reverse(Acc),[C]}
    end;
get_binary_clauses([], Acc) ->
    {lists:reverse(Acc),[]}.

switch_cases([{N, L, C} | Cs], V, F, Next, Fail, Ctxt, Env, Body,
	     S0) ->
    S1 = add_code([icode_label(L)], S0),
    S2 = Body(icode_const_val(N), V, C, F, Next, Fail, Ctxt, Env, S1),
    switch_cases(Cs, V, F, Next, Fail, Ctxt, Env, Body, S2);
switch_cases([_ | Cs], V, F, Next, Fail, Ctxt, Env, Body, S) ->
    switch_cases(Cs, V, F, Next, Fail, Ctxt, Env, Body, S);
switch_cases([], _V, _F, _Next, _Fail, _Ctxt, _Env, _Body, S) ->
    S.

%% Recall that the `final' and `effect' context flags distribute over
%% the clause bodies.

clauses(Cs, F, Vs, Ctxt, Env, S) ->
    Next = new_continuation_label(Ctxt),
    S1 = clauses_1(Cs, F, Vs, undefined, Next, Ctxt, Env, S),
    add_continuation_label(Next, Ctxt, S1).

clauses_1([C | Cs], F, Vs, Fail, Next, Ctxt, Env, S) ->
    case cerl_clauses:is_catchall(C) of
	true ->
	    %% The fail label will not actually be used in this case.
	    clause(C, F, Vs, Fail, Next, Ctxt, Env, S);
	false ->
	    %% The previous `Fail' is not used here.
	    Fail1 = new_label(),
	    S1 = clause(C, F, Vs, Fail1, Next, Ctxt, Env, S),
	    S2 = add_code([icode_label(Fail1)], S1),
	    clauses_1(Cs, F, Vs, Fail1, Next, Ctxt, Env, S2)
    end;
clauses_1([], _F, _Vs, Fail, _Next, Ctxt, _Env, S) ->
    if Fail == undefined ->
	    L = new_label(),
	    add_default_case(L, Ctxt, S);
       true ->
	    add_code([icode_goto(Fail)], S)    % use existing label
    end.

%% The exact behaviour if all clauses fail is undefined; we generate an
%% 'internal_error' exception if this happens, which is safe and will
%% not get in the way of later analyses. (Continuing execution after the
%% `case', as in a C `switch' statement, would add a new possible path
%% to the program, which could destroy program properties.) Note that
%% this code is only generated if some previous stage has created a
%% switch over clauses without a final catch-all; this could be both
%% legal and non-redundant, e.g. if the last clause does pattern
%% matching to extract components of a (known) constructor. The
%% generated default-case code *should* be unreachable, but we need it
%% in order to have a safe fail-label.

add_default_case(L, Ctxt, S) ->
    S1 = add_code([icode_label(L)], S),
    add_error(icode_const(internal_error), Ctxt, S1).

clause(C, F, Vs, Fail, Next, Ctxt, Env, S) ->
    G = cerl:clause_guard(C),
    case cerl_clauses:eval_guard(G) of
	{value, true} ->
	    {Env1, S1} = patterns(cerl:clause_pats(C), Vs, Fail, Env,
				  S),
	    clause_body(C, F, Next, Ctxt, Env1, S1);
	{value, false} ->
	    add_code([icode_goto(Fail)], S);
	_ ->
	    {Env1, S1} = patterns(cerl:clause_pats(C), Vs, Fail, Env,
				  S),
	    Succ = new_label(),
	    Ctxt1 = Ctxt#ctxt{final = false,
			      fail = Fail,
			      class = guard},
	    S2 = boolean(G, Succ, Fail, Ctxt1, Env1, S1),
	    S3 = add_code([icode_label(Succ)], S2),
	    clause_body(C, F, Next, Ctxt, Env1, S3)
    end.

clause_body(C, F, Next, Ctxt, Env, S) ->
    %% This check is inserted as a goto is always final 
    case is_goto(cerl:clause_body(C)) of
	true ->
	    F(cerl:clause_body(C), Ctxt, Env, S);
	false ->
	    S1 = F(cerl:clause_body(C), Ctxt, Env, S),
	    add_continuation_jump(Next, Ctxt, S1)
    end.

patterns([P | Ps], [V | Vs], Fail, Env, S) ->
    {Env1, S1} = pattern(P, V, Fail, Env, S),
    patterns(Ps, Vs, Fail, Env1, S1);
patterns([], [], _, Env, S) ->
    {Env, S}.

pattern(P, V, Fail, Env, S) ->
    case cerl:type(P) of
	var ->
	    {bind_var(P, V, Env), S};
	alias ->
	    {Env1, S1} = pattern(cerl:alias_pat(P), V,
				 Fail, Env, S),
	    {bind_var(cerl:alias_var(P), V, Env1), S1};
	literal ->
	    {Env, literal_pattern(P, V, Fail, S)};
	cons ->
	    cons_pattern(P, V, Fail, Env, S);
	tuple ->
	    tuple_pattern(P, V, Fail, Env, S);
	binary ->
	    binary_pattern(P, V, Fail, Env, S)
    end.

literal_pattern(P, V, Fail, S) ->
    L = new_label(),
    S1 = literal_pattern_1(P, V, Fail, L, S),
    add_code([icode_label(L)], S1).

literal_pattern_1(P, V, Fail, Next, S) ->
    case cerl:concrete(P) of
	X when atom(X) ->
	    add_code([make_type([V], ?TYPE_ATOM(X), Next, Fail)],
		     S);
	X when integer(X) ->
	    add_code([make_type([V], ?TYPE_INTEGER(X), Next, Fail)],
		     S);
	X when float(X) ->
	    V1 = make_var(),
	    L = new_label(),
	    %% First doing an "is float" test here might allow later
	    %% stages to use a specialized equality test.
	    add_code([make_type([V], ?TYPE_IS_FLOAT, L, Fail),
		      icode_label(L),
		      icode_move(V1, icode_const(X)),
		      make_if(?TEST_EQ, [V, V1], Next, Fail)],
		     S);
	[] ->
	    add_code([make_type([V], ?TYPE_NIL, Next, Fail)], S);
	X ->
	    %% Compound constants are compared with the generic exact
	    %% equality test.
	    V1 = make_var(),
	    add_code([icode_move(V1, icode_const(X)),
		      make_if(?TEST_EQ, [V, V1], Next, Fail)],
		     S)
    end.

cons_pattern(P, V, Fail, Env, S) ->
    V1 = make_var(),
    V2 = make_var(),
    Next = new_label(),
    Ctxt = #ctxt{},
    S1 = add_code([make_type([V], ?TYPE_CONS, Next, Fail),
		   icode_label(Next)]
		  ++ make_op(?OP_UNSAFE_HD, [V1], [V], Ctxt)
		  ++ make_op(?OP_UNSAFE_TL, [V2], [V], Ctxt),
		  S),
    patterns([cerl:cons_hd(P), cerl:cons_tl(P)], [V1, V2],
	     Fail, Env, S1).

tuple_pattern(P, V, Fail, Env, S) ->
    Es = cerl:tuple_es(P),
    N = length(Es),
    Vs = make_vars(N),
    Next = new_label(),
    S1 = add_code([make_type([V], ?TYPE_IS_N_TUPLE(N), Next, Fail),
		   icode_label(Next)],
		  S),
    S2 = tuple_elements(Vs, V, S1),
    patterns(Es, Vs, Fail, Env, S2).

tuple_elements(Vs, V, S) ->
    tuple_elements(Vs, V, #ctxt{}, 1, S).

tuple_elements([V1 | Vs], V0, Ctxt, N, S) ->
    Code = make_op(?OP_UNSAFE_ELEMENT(N), [V1], [V0], Ctxt),
    tuple_elements(Vs, V0, Ctxt, N + 1, add_code(Code, S));
tuple_elements([], _, _, _, S) ->
    S.

binary_pattern(P, V, Fail, Env, S) ->
    L1 = new_label(),
    Segs = cerl:binary_segments(P),
    Arity = length(Segs),
    Vars = make_vars(Arity),
    S1 = add_code([icode_guardop([], {hipe_bs_primop, bs_start_match}, [V], L1, Fail),
		   icode_label(L1)],S),
    {Env1,S2} = bin_seg_patterns(Segs, Vars, Fail, Env, S1, 0),
    L2 = new_label(),
    {Env1,add_code([icode_guardop([], {hipe_bs_primop, {bs_test_tail,0}}, [], L2, Fail),
		    icode_label(L2)], S2)}.

bin_seg_patterns([Seg|Rest], [T|Ts], Fail, Env, S, Align) ->
    {{NewEnv, S1}, NewAlign} = bin_seg_pattern(Seg, T, Fail, Env, S, Align),
    bin_seg_patterns(Rest, Ts, Fail, NewEnv, S1, NewAlign);

bin_seg_patterns([], [], _Fail, Env, S, _Align) ->
    {Env, S}.

bin_seg_pattern(P, V, Fail, Env, S, Align) ->
    L = new_label(),
    Size = cerl:bitstr_size(P),
    Unit = cerl:bitstr_unit(P),
    Type = cerl:concrete(cerl:bitstr_type(P)),
    LiteralFlags = cerl:bitstr_flags(P),
    T = cerl:bitstr_val(P), 
    Flags=translate_flags(LiteralFlags, Align),
    case calculate_size(Unit, Size, Align, Env, S) of
	{NewUnit, Args, S0, NewAlign} ->
	    Name =
		case Type of
		    integer ->
			{bs_get_integer, NewUnit, Flags};
		    float ->
			{bs_get_float, NewUnit, Flags};
		    binary ->
			{bs_get_binary, NewUnit, Flags}
		end,
	    S1= add_code([icode_guardop([V],{hipe_bs_primop, Name}, Args, L, Fail),
			  icode_label(L)], S0),
	    {pattern(T, V, Fail, Env, S1), NewAlign};
	{all, NewAlign, S0} ->
	    Type = binary,
	    Name = {bs_get_binary_all, Flags},
	    S1= add_code([icode_guardop([V], {hipe_bs_primop, Name}, [], L, Fail),
			  icode_label(L)], S0),
	    {pattern(T, V, Fail, Env, S1), NewAlign}
    end.

%binary_pattern(P, V, Fail, Env, S) ->
%    L1 = new_label(),
%    L2 = new_label(),
%    Orig = make_var(), 
%    OrigOffset = make_var(),
%    BinSize = make_var(),
%    State = {Orig, OrigOffset, BinSize},
%    {Size, NewBinSegs} = cerl_binary_pattern_match:add_offset_to_pattern(P), 
%    Arity = length(NewBinSegs),
%    Vars = make_vars(Arity),
%    S1 = add_code([icode_guardop([Orig], {hipe_bsi_primop, bs_get_orig}, [V], L1, Fail),
%		   icode_label(L1),
%		   icode_call_primop([OrigOffset], {hipe_bsi_primop, bs_get_orig_offset}, [V]),
%		   icode_call_primop([BinSize], {hipe_bsi_primop, bs_get_size}, [V])], S),
%    S2 = translate_size_expr(Size, State, L2, Fail, Env, S1), 
%    S3 = add_code([icode_label(L2)], S2),
%    bitstr_patterns(NewBinSegs, Vars, State, Fail, Env, S3).

%bitstr_patterns([Seg|Rest], [T|Ts], State, Fail, Env, S) ->
%    {NewEnv, S1} = bitstr_pattern(Seg, T, State, Fail, Env, S),
%    bitstr_patterns(Rest, Ts, State, Fail, NewEnv, S1);

%bitstr_patterns([], [], _State, _Fail, Env, S) ->
%    {Env, S}.

%bitstr_pattern(P, V, State, Fail, Env, S) ->
%    T = cerl_binary_pattern_match:new_bitstr_val(P),
%    Size = cerl_binary_pattern_match:new_bitstr_size(P),
%    Offset = cerl_binary_pattern_match:new_bitstr_offset(P),
%    {LiteralType,LiteralFlags} = cerl_binary_pattern_match:new_bitstr_type(P),
%    Align = test_align(Offset),
%    Flags=translate_flags(LiteralFlags, Align),
%    Type = cerl:atom_val(LiteralType),
%    OffsetVar = make_var(),
%    OffsetConst = cerl_binary_pattern_match:size_const(Offset),
%    {S1, SizeExpr} =
%	case type_of_size(Size) of
%	    const ->
%		{S, Size};
%	    all ->
%		{S, Size};
%	    var ->
%		SizeVar = make_var(),
%		S0 = add_size_code(Size, SizeVar, Env, S),
%		{S0, SizeVar}
%	end,
%    S2 = add_offset_code(Offset, OffsetVar, Env, S1),
%    S3 = add_final_code(Type, V, SizeExpr, OffsetVar, OffsetConst, State, Flags, S2),
%    pattern(T, V, Fail, Env, S3).


%% ---------------------------------------------------------------------
%% Boolean expressions

%% This generates code for a boolean expression (such as "primop
%% 'and'(X, Y)") in a normal expression context, when an actual `true'
%% or `false' value is to be computed. We set up a default fail-label
%% for generating a `badarg' error, unless we are in a guard.

boolean_expr(E, [V], Ctxt=#ctxt{class = guard}, Env, S) ->
    {Code, True, False} = make_bool_glue(V),
    S1 = boolean(E, True, False, Ctxt, Env, S),
    add_code(Code, S1);
boolean_expr(E, [V] = Ts, Ctxt, Env, S) ->
    {Code, True, False} = make_bool_glue(V),
    Fail = new_label(),
    Cont = new_continuation_label(Ctxt),
    Ctxt1 = Ctxt#ctxt{final = false, effect = false, fail = Fail},
    S1 = boolean(E, True, False, Ctxt1, Env, S),
    S2 = maybe_return(Ts, Ctxt, add_code(Code, S1)),
    S3 = add_continuation_jump(Cont, Ctxt, S2),
    S4 = add_code([icode_label(Fail)], S3),
    S5 = add_error(icode_const(badarg), Ctxt, S4),  % can add dummy label
    S6 = add_continuation_jump(Cont, Ctxt, S5),  % avoid empty basic block
    add_continuation_label(Cont, Ctxt, S6);
boolean_expr(_, [], _Ctxt, _Env, _S) ->
    error_high_degree(),
    throw(error);
boolean_expr(_, _, _Ctxt, _Env, _S) ->
    error_low_degree(),
    throw(error).

%% This is for when we expect a boolean result in jumping code context,
%% but are not sure what the expression will produce, or we know that
%% the result is not a boolean and we just want error handling.

expect_boolean_value(E, True, False, Ctxt, Env, S) ->
    V = make_var(),
    S1 = expr(E, [V], Ctxt#ctxt{final = false}, Env, S),
    case Ctxt#ctxt.fail of
	[] ->
	    %% No fail-label set - this means we are *sure* that the
	    %% result can only be 'true' or 'false'.
	    add_code([make_type([V], ?TYPE_ATOM(true), True, False)],
		     S1);
	Fail ->
	    Next = new_label(),
	    add_code([make_type([V], ?TYPE_ATOM(true), True, Next),
		      icode_label(Next),
		      make_type([V], ?TYPE_ATOM(false), False, Fail)],
		     S1)
    end.

%% This generates code for a case-switch with exactly one 'true' branch
%% and one 'false' branch, and no other branches (not even a catch-all).
%% Note that E must be guaranteed to produce a boolean value for such a
%% switch to have been generated.

bool_switch(E, TrueExpr, FalseExpr, F, Ctxt, Env, S) ->
    Cont = new_continuation_label(Ctxt),
    True = new_label(),
    False = new_label(),
    Ctxt1 = Ctxt#ctxt{final = false, effect = false},
    S1 = boolean(E, True, False, Ctxt1, Env, S),
    S2 = add_code([icode_label(True)], S1),
    S3 = F(TrueExpr, Ctxt, Env, S2),
    S4 = add_continuation_jump(Cont, Ctxt, S3),
    S5 = add_code([icode_label(False)], S4),
    S6 = F(FalseExpr, Ctxt, Env, S5),
    add_continuation_label(Cont, Ctxt, S6).

%% This generates jumping code for booleans. If the fail-label is set,
%% it tells where to go in case a value turns out not to be a boolean.

%% In strict boolean expressions, we set a flag to be checked if
%% necessary after both branches have been evaluated. An alternative
%% would be to duplicate the code for the second argument, for each
%% value ('true' or 'false') of the first argument.

%% (Note that subexpressions are checked repeatedly to see if they are
%% safe - this is quadratic, but I don't expect booleans to be very
%% deeply nested.)

%% Note that 'and', 'or' and 'xor' are strict (like all primops)!

boolean(E0, True, False, Ctxt, Env, S) ->
    E = cerl_lib:reduce_expr(E0),
    case cerl:type(E) of
	literal ->
	    case cerl:concrete(E) of
		true ->
		    add_code([icode_goto(True)], S);
		false ->
		    add_code([icode_goto(False)], S);
		_ ->
		    expect_boolean_value(E, True, False, Ctxt, Env, S)
	    end;
	values ->
	    case cerl:values_es(E) of
		[E1] ->
		    boolean(E1, True, False, Ctxt, Env, S);
		_ ->
		    error_msg("degree mismatch - expected boolean: ~P",
			      [E, 10]),
		    throw(error)
	    end;
	primop ->
	    Name = cerl:atom_val(cerl:primop_name(E)),
	    As = cerl:primop_args(E),
	    Arity = length(As),
	    case {Name, Arity} of
		{?PRIMOP_NOT, 1} ->
		    %% `not' simply switches true and false labels.
		    [A] = As,
		    boolean(A, False, True, Ctxt, Env, S);
		{?PRIMOP_AND, 2} ->
		    strict_and(As, True, False, Ctxt, Env, S);
		{?PRIMOP_OR, 2} ->
		    strict_or(As, True, False, Ctxt, Env, S);
		{?PRIMOP_XOR, 2} ->
		    %% `xor' always needs to evaluate both arguments
		    strict_xor(As, True, False, Ctxt, Env, S);
		_ ->
		    case is_comp_op(Name, Arity) of
			true ->
			    comparison(Name, As, True, False, Ctxt, Env,
				       S);
			false ->
			    case is_type_test(Name, Arity) of
				true ->
				    type_test(Name, As, True, False,
					      Ctxt, Env, S);
				false ->
				    expect_boolean_value(E, True, False,
							 Ctxt, Env, S)
			    end
		    end
	    end;
 	'case' ->
	    %% Propagate boolean handling into clause bodies.
	    F = fun (B, Ctxt, Env, S) ->
			boolean(B, True, False, Ctxt, Env, S)
		end,
	    expr_case_1(E, F, Ctxt, Env, S);
	seq ->
	    %% Propagate boolean handling into body.
	    F = fun (B, Ctxt, Env, S) ->
			boolean(B, True, False, Ctxt, Env, S)
		end,
	    expr_seq_1(E, F, Ctxt, Env, S);	    
	'let' ->
	    %% Propagate boolean handling into body. Note that we have
	    %% called 'cerl_lib:reduce_expr/1' above.
	    F = fun (B, Ctxt, Env, S) ->
			boolean(B, True, False, Ctxt, Env, S)
		end,
	    expr_let_1(E, F, Ctxt, Env, S);	    
	'try' ->
	    case Ctxt#ctxt.class of
		guard ->
		    %% This *must* be a "protected" guard expression on
		    %% the form "try E of X -> X catch <...> -> 'false'"
		    %% (we could of course test if the handler body is
		    %% the atom 'false', etc.).
		    Ctxt1 = Ctxt#ctxt{fail = False},
		    boolean(cerl:try_arg(E), True, False, Ctxt1, Env,
			    S);
		_ ->
		    %% Propagate boolean handling into the handler and body.
		    F = fun (B, Ctxt, Env, S) ->
				boolean(B, True, False, Ctxt, Env, S)
			end,
		    expr_try_1(E, F, Ctxt, Env, S)
	    end;
	_ ->
	    %% This handles everything else, including cases that are
	    %% known to not return a boolean.
	    expect_boolean_value(E, True, False, Ctxt, Env, S)
    end.

strict_and([A, B], True, False, Ctxt, Env, S) ->
    V = make_var(),
    {Glue, True1, False1} = make_bool_glue(V),
    S1 = boolean(A, True1, False1, Ctxt, Env, S),
    S2 = add_code(Glue, S1),
    Test = new_label(),
    S3 = boolean(B, Test, False, Ctxt, Env, S2),
    add_code([icode_label(Test),
	      make_bool_test(V, True, False)],
	     S3).

strict_or([A, B], True, False, Ctxt, Env, S) ->
    V = make_var(),
    {Glue, True1, False1} = make_bool_glue(V),
    S1 = boolean(A, True1, False1, Ctxt, Env, S),
    S2 = add_code(Glue, S1),
    Test = new_label(),
    S3 = boolean(B, True, Test, Ctxt, Env, S2),
    add_code([icode_label(Test),
	      make_bool_test(V, True, False)],
	     S3).

strict_xor([A, B], True, False, Ctxt, Env, S) ->
    V = make_var(),
    {Glue, True1, False1} = make_bool_glue(V),
    S1 = boolean(A, True1, False1, Ctxt, Env, S),
    S2 = add_code(Glue, S1),
    Test1 = new_label(),
    Test2 = new_label(),
    S3 = boolean(B, Test1, Test2, Ctxt, Env, S2),
    add_code([icode_label(Test1),
	      make_bool_test(V, False, True),
	      icode_label(Test2),
	      make_bool_test(V, True, False)],
	     S3).

%% Primitive comparison operations are inline expanded as conditional
%% branches when part of a boolean expression, rather than made into
%% primop or guardop calls. Note that Without type information, we
%% cannot reduce equality tests like `Expr == true' to simply `Expr'
%% (and `Expr == false' to `not Expr'), because we are not sure that
%% Expr will yield a boolean - if it does not, the result of the
%% comparison should be `false'.

comparison(Name, As, True, False, Ctxt, Env, S) ->
    {Vs, S1} = expr_list(As, Ctxt, Env, S),
    Test = comp_test(Name),
    add_code([make_if(Test, Vs, True, False)], S1).

comp_test(?PRIMOP_EQ) -> ?TEST_EQ;
comp_test(?PRIMOP_NE) -> ?TEST_NE;
comp_test(?PRIMOP_EXACT_EQ) -> ?TEST_EXACT_EQ;
comp_test(?PRIMOP_EXACT_NE) -> ?TEST_EXACT_NE;
comp_test(?PRIMOP_LT) -> ?TEST_LT;
comp_test(?PRIMOP_GT) -> ?TEST_GT;
comp_test(?PRIMOP_LE) -> ?TEST_LE;
comp_test(?PRIMOP_GE) -> ?TEST_GE.

type_test(?PRIMOP_IS_RECORD, As, True, False, Ctxt, Env, S)
  when length(As) == 3 ->
    is_record_test(As, True, False, Ctxt, Env, S);
type_test(Name, [A], True, False, Ctxt, Env, S) ->
    V = make_var(),
    S1 = expr(A, [V], Ctxt#ctxt{final = false, effect = false}, Env, S),
    Test = type_test(Name),
    add_code([make_type([V], Test, True, False)], S1).

%% It turned out to be easiest to generate Icode directly for this. 
is_record_test([T, A, N] = As, True, False, Ctxt, Env, S) ->
    case cerl:is_c_atom(A) andalso cerl:is_c_int(N)
	andalso (cerl:concrete(N) > 0) of
	true ->
	    V = make_var(),
	    Ctxt1 = Ctxt#ctxt{final = false, effect = false},
	    S1 = expr(T, [V], Ctxt1, Env, S),
	    Atom = cerl:concrete(A),
	    Size = cerl:concrete(N),
	    add_code([make_type([V], ?TYPE_IS_RECORD(Atom, Size),True, False)],
		     S1);
	false ->
	    error_primop_badargs(?PRIMOP_IS_RECORD, As),
	    throw(error)
    end.

type_test(?PRIMOP_IS_ATOM) -> ?TYPE_IS_ATOM;
type_test(?PRIMOP_IS_BIGNUM) -> ?TYPE_IS_BIGNUM;
type_test(?PRIMOP_IS_BINARY) -> ?TYPE_IS_BINARY;
type_test(?PRIMOP_IS_CONSTANT) -> ?TYPE_IS_CONSTANT;
type_test(?PRIMOP_IS_FIXNUM) -> ?TYPE_IS_FIXNUM;
type_test(?PRIMOP_IS_FLOAT) -> ?TYPE_IS_FLOAT;
type_test(?PRIMOP_IS_FUNCTION) -> ?TYPE_IS_FUNCTION;
type_test(?PRIMOP_IS_INTEGER) -> ?TYPE_IS_INTEGER;
type_test(?PRIMOP_IS_LIST) -> ?TYPE_IS_LIST;
type_test(?PRIMOP_IS_NUMBER) -> ?TYPE_IS_NUMBER;
type_test(?PRIMOP_IS_PID) -> ?TYPE_IS_PID;
type_test(?PRIMOP_IS_PORT) -> ?TYPE_IS_PORT;
type_test(?PRIMOP_IS_REFERENCE) -> ?TYPE_IS_REFERENCE;
type_test(?PRIMOP_IS_TUPLE) -> ?TYPE_IS_TUPLE.

is_comp_op(?PRIMOP_EQ, 2) -> true;
is_comp_op(?PRIMOP_NE, 2) -> true;
is_comp_op(?PRIMOP_EXACT_EQ, 2) -> true;
is_comp_op(?PRIMOP_EXACT_NE, 2) -> true;
is_comp_op(?PRIMOP_LT, 2) -> true;
is_comp_op(?PRIMOP_GT, 2) -> true;
is_comp_op(?PRIMOP_LE, 2) -> true;
is_comp_op(?PRIMOP_GE, 2) -> true;
is_comp_op(_, _) -> false.

is_bool_op(?PRIMOP_AND, 2) -> true;
is_bool_op(?PRIMOP_OR, 2) -> true;
is_bool_op(?PRIMOP_XOR, 2) -> true;
is_bool_op(?PRIMOP_NOT, 1) -> true;
is_bool_op(_, _) -> false.

is_type_test(?PRIMOP_IS_ATOM, 1) -> true;
is_type_test(?PRIMOP_IS_BIGNUM, 1) -> true;
is_type_test(?PRIMOP_IS_BINARY, 1) -> true;
is_type_test(?PRIMOP_IS_CONSTANT, 1) -> true;
is_type_test(?PRIMOP_IS_FIXNUM, 1) -> true;
is_type_test(?PRIMOP_IS_FLOAT, 1) -> true;
is_type_test(?PRIMOP_IS_FUNCTION, 1) -> true;
is_type_test(?PRIMOP_IS_INTEGER, 1) -> true;
is_type_test(?PRIMOP_IS_LIST, 1) -> true;
is_type_test(?PRIMOP_IS_NUMBER, 1) -> true;
is_type_test(?PRIMOP_IS_PID, 1) -> true;
is_type_test(?PRIMOP_IS_PORT, 1) -> true;
is_type_test(?PRIMOP_IS_REFERENCE, 1) -> true;
is_type_test(?PRIMOP_IS_TUPLE, 1) -> true;
is_type_test(?PRIMOP_IS_RECORD, 3) -> true;
is_type_test(_, _) -> false.


%% ---------------------------------------------------------------------
%% Utility functions

bind_var(V, Name, Env) ->
    env__bind(cerl:var_name(V), #var{name = Name}, Env).

bind_vars([V | Vs], [X | Xs], Env) ->
    bind_vars(Vs, Xs, bind_var(V, X, Env));
bind_vars([], [], Env) ->
    Env.

bind_fun(V, L, Vs, Env) ->
    env__bind(cerl:var_name(V), #'fun'{label = L, vars = Vs}, Env).

add_code(Code, S) ->
    s__add_code(Code, S).

%% This inserts code when necessary for assigning the targets in the
%% first list to those in the second.

glue([V1 | Vs1], [V2 | Vs2], S) ->
    if V1 =:= V2 ->
	    S;
       true ->
	    glue(Vs1, Vs2, add_code([icode_move(V2, V1)], S))
    end;
glue([], [], S) ->
    S;
glue([], _, S) ->
    warning_low_degree(),
    S;
glue(_, [], _) ->
    error_high_degree(),
    throw(error).

make_moves([V1 | Vs1], [V2 | Vs2]) ->
    [icode_move(V1, V2) | make_moves(Vs1, Vs2)];
make_moves([], []) ->
    [].

%% If the context signals `final', we generate a return instruction,
%% otherwise nothing happens.

maybe_return(Ts, Ctxt, S) ->
    case Ctxt#ctxt.final of
	false ->
	    S;
	true ->
	    add_return(Ts, S)
    end.

add_return(Ts, S) ->
    add_code([icode_return(Ts)], S).

new_continuation_label(Ctxt) ->
    case Ctxt#ctxt.final of
	false ->
	    new_label();
	true ->
	    undefined
    end.

add_continuation_label(Label, Ctxt, S) ->
    case Ctxt#ctxt.final of
	false ->
	    add_code([icode_label(Label)], S);
	true ->
	    S
    end.

add_continuation_jump(Label, Ctxt, S) ->
    case Ctxt#ctxt.final of
	false ->
	    add_code([icode_goto(Label)], S);
	true ->
	    S
    end.

add_local_call({Name, _Arity} = V, Vs, Ts, Ctxt, S, DstType) ->
    Module = s__get_module(S),
    case Ctxt#ctxt.final of
	false ->
	    add_code([icode_call_local(Ts, Module, Name, Vs, DstType)],S);
	true ->
	    Self = s__get_function(S),
	    if V =:= Self ->
		    %% Self-recursive tail call:
		    {Label, Vs1} = s__get_local_entry(S),
		    add_code(make_moves(Vs1, Vs) ++ [icode_goto(Label)],
			     S);
	       true ->
		    add_code([icode_enter_local(Module, Name, Vs)], S)
	    end
    end.

%% Note that this has the same "problem" as the fail instruction (see
%% the 'add_fail' function), namely, that it unexpectedly ends a basic
%% block. The solution is the same - add a dummy label if necessary.

add_letrec_call(Label, Vs1, Vs, Ctxt, S) ->
    S1 = add_code(make_moves(Vs1, Vs) ++ [icode_goto(Label)], S),
    L = new_continuation_label(Ctxt),
    add_continuation_label(L, Ctxt, S1).

add_exit(V, Ctxt, S) ->
    add_fail([V], exit, Ctxt, S).

add_throw(V, Ctxt, S) ->
    add_fail([V], throw, Ctxt, S).

add_error(V, Ctxt, S) ->
    add_fail([V], error, Ctxt, S).

add_error(V, F, Ctxt, S) ->
    add_fail([V, F], error, Ctxt, S).

add_rethrow(E, V, Ctxt, S) ->
    add_fail([E, V], rethrow, Ctxt, S).

%% Failing is special, because it can "suddenly" end the basic block,
%% even though the context was expecting the code to fall through, for
%% instance when you have a call to 'exit(X)' that is not in a tail call
%% context. In those cases a dummy label must therefore be added after
%% the fail instruction, to start a new (but unreachable) basic block.

add_fail(Vs, Class, Ctxt, S0) ->
    S1 = add_code([icode_fail(Vs, Class)], S0),
    add_continuation_label(new_continuation_label(Ctxt), Ctxt, S1).

%% We must add continuation- and fail-labels if we are in a guard context.

make_op(Name, Ts, As, Ctxt) ->
    case Ctxt#ctxt.final of
	false ->
	    case Ctxt#ctxt.class of
		guard ->
		    Next = new_label(),
		    [icode_guardop(Ts, Name, As, Next, Ctxt#ctxt.fail),
		     icode_label(Next)];
		_ ->
		    [icode_call_primop(Ts, Name, As)]
	    end;	
	true ->
	    [icode_enter_primop(Name, As)]
    end.

make_call(M, F, Ts, As, Ctxt) ->
    case Ctxt#ctxt.final of
	false ->
	    case Ctxt#ctxt.class of
		guard ->
		    Next = new_label(),
		    [icode_call_remote(Ts, M, F, As, Next,
				       Ctxt#ctxt.fail, true),
		     icode_label(Next)];
		_ ->
		    [icode_call_remote(Ts, M, F, As)]
	    end;
	true ->
	    %% A final call can't be in a guard anyway
	    [icode_enter_remote(M, F, As)]
    end.

%% Recognize useless tests that always go to the same label. This often
%% happens as an artefact of the translation.

make_if(_, _, Label, Label) ->
    icode_goto(Label);
make_if(Test, As, True, False) ->
    icode_if(Test, As, True, False).

make_type(_, _, Label, Label) ->
    icode_goto(Label);
make_type(Vs, Test, True, False) ->
    icode_type(Vs, Test, True, False).

%% Creating glue code with true/false target labels for assigning a
%% corresponding 'true'/'false' value to a specific variable. Used as
%% glue between boolean jumping code and boolean values.

make_bool_glue(V) ->
    make_bool_glue(V, true, false).

make_bool_glue(V, T, F) ->
    False = new_label(),
    True = new_label(),
    Next = new_label(),
    Code = [icode_label(False), 
	    icode_move(V, icode_const(F)),
	    icode_goto(Next),
	    icode_label(True),
	    icode_move(V, icode_const(T)),
	    icode_label(Next)],
    {Code, True, False}.

make_bool_test(V, True, False) ->
    make_type([V], ?TYPE_ATOM(true), True, False).

%% Checking if an expression is safe

is_safe_expr(E) ->
    cerl_lib:is_safe_expr(E, fun function_check/2).

function_check(safe, {Name, Arity}) ->
    is_safe_op(Name, Arity);
function_check(safe, {Module, Name, Arity}) ->
    erl_bifs:is_safe(Module, Name, Arity);
function_check(pure, {Name, Arity}) ->
    is_pure_op(Name, Arity);
function_check(pure, {Module, Name, Arity}) ->
    erl_bifs:is_pure(Module, Name, Arity);
function_check(_, _) ->
    false.

%% There are very few really safe operations (sigh!). If we have type
%% information, several operations could be rewritten into specialized
%% safe versions, such as '+'/2 -> add_integer/2.

is_safe_op(?PRIMOP_IDENTITY, 1) -> true;
is_safe_op(N, A) ->
    case is_comp_op(N, A) of
	true -> true;
	false ->
	    is_type_test(N, A)
    end.

is_pure_op(?PRIMOP_IDENTITY, 1) -> true;
is_pure_op(?PRIMOP_ELEMENT, 2) -> true;
is_pure_op(?PRIMOP_MAKE_FUN, 6) -> true;
is_pure_op(?PRIMOP_FUN_ELEMENT, 2) -> true;
is_pure_op(?PRIMOP_ADD, 2) -> true;
is_pure_op(?PRIMOP_SUB, 2) -> true;
is_pure_op(?PRIMOP_NEG, 1) -> true;
is_pure_op(?PRIMOP_MUL, 2) -> true;
is_pure_op(?PRIMOP_DIV, 2) -> true;
is_pure_op(?PRIMOP_INTDIV, 2) -> true;
is_pure_op(?PRIMOP_REM, 2) -> true;
is_pure_op(?PRIMOP_BAND, 2) -> true;
is_pure_op(?PRIMOP_BOR, 2) -> true;
is_pure_op(?PRIMOP_BXOR, 2) -> true;
is_pure_op(?PRIMOP_BNOT, 1) -> true;
is_pure_op(?PRIMOP_BSL, 2) -> true;
is_pure_op(?PRIMOP_BSR, 2) -> true;
is_pure_op(?PRIMOP_EXIT, 1) -> true;
is_pure_op(?PRIMOP_THROW, 1) -> true;
is_pure_op(?PRIMOP_ERROR, 1) -> true;
is_pure_op(?PRIMOP_ERROR, 2) -> true;
is_pure_op(?PRIMOP_RETHROW, 2) -> true;
is_pure_op(N, A) ->
    case is_bool_op(N, A) of
	true -> true;
	false ->
	    case is_comp_op(N, A) of
		true -> true;
		false ->
		    is_type_test(N, A)
	    end
    end.

translate_flags(Flags, Align) ->
    translate_flags1(cerl:concrete(Flags), Align).

translate_flags1([A|Rest],Align) ->
    case A of
	signed ->
	    4 + translate_flags1(Rest, Align);
	little ->
	    2 + translate_flags1(Rest, Align);
	native ->
	    case hipe_rtl_arch:endianess() of
		little ->
		    2 + translate_flags1(Rest, Align);
		big ->
		    translate_flags1(Rest, Align)
	    end;
	_ ->
	    translate_flags1(Rest, Align)
    end;
translate_flags1([], Align) ->
    case Align of
	0 ->
	    1;
	_ ->
	    0
    end.


get_const_info(Val, integer) ->
    case {cerl:is_c_var(Val), cerl:is_c_int(Val)} of
	{true, _} ->
	    var;
	{_, true} ->
	    pass;
	_ ->
	    fail
    end;
get_const_info(Val, float) ->
    case {cerl:is_c_var(Val), cerl:is_c_float(Val)} of
	{true, _} ->
	    var;
	{_, true} ->
	    pass;
	_ ->
	    fail
    end;

get_const_info(_Val, _Type) ->
    [].

calculate_size(Unit, Var, Align, Env, S) ->
    case cerl:is_c_atom(Var) of
	true ->
	    {cerl:atom_val(Var), Align, S};
	false ->
	    case cerl:is_c_int(Var) of
		true ->
		    NewVal = cerl:concrete(Var) * cerl:concrete(Unit),  
		    NewAlign =
			case Align of 
			    false ->
				false;
			    _ ->
				(NewVal+Align) band 7
			end,
		    {NewVal, [], S, NewAlign};
		false ->
		    NewSize = make_var(),
		    S1 = expr(Var, [NewSize], #ctxt{final=false}, Env, S), 
		    NewAlign =
			case cerl:concrete(Unit) band 7 of
			    0 ->
				Align;
			    _ ->
				false
			end,
		    {cerl:concrete(Unit), [NewSize], S1, NewAlign}
	    end
    end.


%% ---------------------------------------------------------------------
%% Environment (abstract datatype)

env__new() ->
    rec_env:empty().

env__bind(Key, Val, Env) ->
    rec_env:bind(Key, Val, Env).

env__lookup(Key, Env) ->
    rec_env:lookup(Key, Env).

env__get(Key, Env) ->
    rec_env:get(Key, Env).

%% env__new_integer_keys(N, Env) ->
%%     rec_env:new_keys(N, Env).


%% ---------------------------------------------------------------------
%% State (abstract datatype)

-record(state, {module, function, local, labels=gb_trees:empty(), code = [], pmatch=true}).

s__new(Module) ->
    #state{module = Module}.

s__get_module(S) ->
    S#state.module.

s__set_function(Name, S) ->
    S#state{function = Name}.

s__get_function(S) ->
    S#state.function.

s__set_local_entry(Info, S) ->
    S#state{local = Info}.

s__get_local_entry(S) ->
    S#state.local.

%% Generated code is kept in reverse order, to make adding fast.

s__set_code(Code, S) ->
    S#state{code = lists:reverse(Code)}.

s__get_code(S) ->
    lists:reverse(S#state.code).

s__add_code(Code, S) ->
    S#state{code = lists:reverse(Code, S#state.code)}.

s__get_label(Ref, S) ->
    Labels = S#state.labels,
    case gb_trees:lookup(Ref, Labels) of
	none ->
	    Label = new_label(),
	    S1 = S#state{labels=gb_trees:enter(Ref, Label, Labels)},
	    {Label, S1};
	{value, Label} -> 
	    {Label,S}
    end.

s__set_pmatch(V, S) ->
    S#state{pmatch = V}.

s__get_pmatch(S) ->
    S#state.pmatch.

%% ---------------------------------------------------------------------
%% Match label State

-record(mstate,{labels=gb_trees:empty()}).

get_correct_label(Alias, MState=#mstate{labels=Labels}) ->
    case gb_trees:lookup(Alias, Labels) of
    none ->
      LabelName=new_label(),
      {LabelName, MState#mstate{labels=gb_trees:insert(Alias, LabelName, Labels)}};
    {value, LabelName} ->
      {LabelName, MState}
  end.


%% ---------------------------------------------------------------------
%% General utilities

reset_var_counter() ->
    hipe_gensym:set_var(0).

reset_label_counter() ->
    hipe_gensym:set_label(0).

new_var() ->
    hipe_gensym:get_next_var().

new_label() ->
    hipe_gensym:get_next_label().

max_var() ->
    hipe_gensym:get_var().

max_label() ->
    hipe_gensym:get_label().

make_var() ->
    icode_var(new_var()).

make_vars(N) when N > 0 ->
    [make_var() | make_vars(N - 1)];
make_vars(0) ->
    [].

make_reg() ->
    icode_reg(new_var()).

get_type(List) when is_list(List)->
    get_type_list(List, []);
get_type(E) ->
    case lists:keysearch(type, 1, cerl:get_ann(E)) of
	{value, {type, Type}} -> Type;
	false -> erl_types:t_any()
    end.

get_type_list([H|T], Acc)->
    case lists:keysearch(type, 1, cerl:get_ann(H)) of
	{value, {type, Type}} -> get_type_list(T, [Type|Acc]);
	false -> get_type_list(T, [erl_types:t_any()|Acc])
    end;
get_type_list([], Acc) ->
    lists:reverse(Acc).


%% ---------------------------------------------------------------------
%% ICode interface

icode_icode(M, {F, A}, Vs, Closure, C, V, L, T) ->
    MFA = {M, F, A},
    hipe_icode:mk_typed_icode(MFA, Vs, Closure, false, C, V, L, T).

icode_icode_name(Icode) ->
    hipe_icode:icode_fun(Icode).

icode_comment(S) -> hipe_icode:mk_comment(S).

icode_var(V) -> hipe_icode:mk_var(V).

icode_reg(V) -> hipe_icode:mk_reg(V).

icode_label(L) -> hipe_icode:mk_label(L).

icode_move(V, D) -> hipe_icode:mk_move(V, D).

icode_const(X) -> hipe_icode:mk_const(X).

icode_const_val(X) -> hipe_icode:const_value(X).

icode_call_local(Ts, M, N, Vs, DstType) ->
    hipe_icode:mk_typed_call(Ts, M, N, Vs, local, DstType).

icode_call_remote(Ts, M, N, Vs) ->
    hipe_icode:mk_call(Ts, M, N, Vs, remote).

icode_call_remote(Ts, M, N, Vs, Cont, Fail, Guard) ->
    hipe_icode:mk_call(Ts, M, N, Vs, remote, Cont, Fail, Guard).

icode_enter_local(M, N, Vs) ->
    hipe_icode:mk_enter(M, N, Vs, local).

icode_enter_remote(M, N, Vs) ->
    hipe_icode:mk_enter(M, N, Vs, remote).

icode_call_fun(Ts, Vs) ->
    icode_call_primop(Ts, call_fun, Vs).

icode_enter_fun(Vs) ->
    icode_enter_primop(enter_fun, Vs).

icode_begin_try(L,Cont) -> hipe_icode:mk_begin_try(L,Cont).

icode_end_try() -> hipe_icode:mk_end_try().

icode_begin_handler(Ts) -> hipe_icode:mk_begin_handler(Ts).

icode_goto(L) -> hipe_icode:mk_goto(L).

icode_return(Ts) -> hipe_icode:mk_return(Ts).

icode_fail(Vs, C) -> hipe_icode:mk_fail(Vs, C).

icode_guardop(Ts, Name, As, Succ, Fail) ->
    hipe_icode:mk_guardop(Ts, Name, As, Succ, Fail).

icode_call_primop(Ts, Name, As) -> hipe_icode:mk_primop(Ts, Name, As).

icode_call_primop(Ts, Name, As, Succ, Fail) ->
    hipe_icode:mk_primop(Ts, Name, As, Succ, Fail).

icode_enter_primop(Name, As) -> hipe_icode:mk_enter_primop(Name, As).

icode_if(Test, As, True, False) ->
    hipe_icode:mk_if(Test, As, True, False).

icode_type(Test, As, True, False) ->
    hipe_icode:mk_type(Test, As, True, False).

icode_switch_val(Arg, Fail, Length, Cases) ->
    hipe_icode:mk_switch_val(Arg, Fail, Length, Cases).

icode_switch_tuple_arity(Arg, Fail, Length, Cases) ->
    hipe_icode:mk_switch_tuple_arity(Arg, Fail, Length, Cases).


%% ---------------------------------------------------------------------
%% Error reporting

error_not_in_receive(Name) ->
    error_msg("primitive operation `~w' missing receive-context.",
	      [Name]).

low_degree() ->
    "degree of expression less than expected.".

warning_low_degree() ->
    warning_msg(low_degree()).

error_low_degree() ->
    error_msg(low_degree()).

error_high_degree() ->
    error_msg("degree of expression greater than expected.").

error_degree_mismatch(N, E) ->
    error_msg("expression does not have expected degree (~w): ~P.",
	      [N, E, 10]).

error_nonlocal_application(Op) ->
    error_msg("application operator not a local function: ~P.",
	      [Op, 10]).

error_primop_badargs(Op, As) ->
    error_msg("bad arguments to `~w' operation: ~P.",
	      [Op, As, 15]).

%% internal_error_msg(S) ->
%%     internal_error_msg(S, []).

%% internal_error_msg(S, Vs) ->
%%     error_msg(lists:concat(["Internal error: ", S]), Vs).

error_msg(S) ->
    error_msg(S, []).

error_msg(S, Vs) ->
    error_logger:error_msg(lists:concat([?MODULE, ": ", S, "\n"]), Vs).

warning_msg(S) ->
    warning_msg(S, []).

warning_msg(S, Vs) ->
    info_msg(lists:concat(["warning: ", S]), Vs).

%% info_msg(S) ->
%%     info_msg(S, []).

info_msg(S, Vs) ->
    error_logger:info_msg(lists:concat([?MODULE, ": ", S, "\n"]), Vs).


%% --------------------------------------------------------------------------
%% Binary stuff

binary_match(BMatch, F, Vs, Next, Fail, Ctxt, Env, S) ->
    MState=#mstate{},
    Tree = cerl_binary_pattern_match:binary_match_clause_tree(BMatch),
    MTag = cerl_binary_pattern_match:binary_match_max_tag(BMatch),
    VarList=make_vars(MTag),
    Orig = make_var(), 
    OrigOffset = make_var(),
    Size = make_var(),
    L1 = new_label(),
    S1 = add_code([icode_guardop([Orig], {hipe_bsi_primop, bs_get_orig}, Vs, L1, Fail),
		   icode_label(L1),
		   icode_call_primop([OrigOffset], {hipe_bsi_primop, bs_get_orig_offset}, Vs),
		   icode_call_primop([Size], {hipe_bsi_primop, bs_get_size}, Vs)], S),
    {S2, _MState0} = clause_tree(Tree, VarList, F, {Orig, OrigOffset, Size}, Next, Fail, MState, Ctxt, Env, S1),
    S2.

clause_tree([], _VarList, _F, _State, _Next, Fail, MState, _Ctxt, _Env, S) ->
    {add_code([icode_goto(Fail)], S), MState};
clause_tree(CTree, VarList, F, State, Next, Fail, MState, Ctxt, Env, S) ->
    Instr = cerl_binary_pattern_match:clause_tree_instr(CTree),
    NextTree = cerl_binary_pattern_match:clause_tree_success(CTree),
    FailTree = cerl_binary_pattern_match:clause_tree_fail(CTree),
    case cerl_binary_pattern_match:instr_type(Instr) of
	read_seg ->
	    read_seg(Instr, NextTree,  VarList, F, State, Next, Fail, MState, Ctxt, Env, S);
	bin_guard ->
	    bin_guard(Instr, NextTree, FailTree, VarList, F, State, Next, Fail, MState, Ctxt, Env, S);
	size ->
	    do_size(Instr, NextTree, FailTree, VarList, F, State, Next, Fail, MState, Ctxt, Env, S);
	match ->
	    do_match(Instr, NextTree, FailTree, VarList, F, State, Next, Fail, MState,Ctxt, Env, S);
	label ->
	    do_label(Instr, NextTree, VarList, F, State, Next, Fail, MState,Ctxt, Env, S);
	goto ->
	    do_goto(Instr, MState, S);
	match_group ->
	    do_match_group(Instr, NextTree, FailTree, VarList, F, State, Next, Fail, MState,Ctxt, Env, S)
    end.

read_seg(Instr, NextTree, VarList, F, State, Next, Fail, MState, Ctxt, Env, S) ->
    S1 = translate_binseg(Instr, VarList, State, Env, S),
    clause_tree(NextTree, VarList, F, State, Next, Fail, MState, Ctxt, Env, S1).

bin_guard(Instr, Body, FailTree, VarList, F, State, Next, Fail, MState, Ctxt, Env, S) ->
    LabelPrimop = cerl_binary_pattern_match:bin_guard_label(Instr),
    Ref = translate_label_primop(LabelPrimop),
    {FL,S1} = s__get_label(Ref, S), 
    {Env1, S2} = translate_bin_guard(Instr, VarList, Env, S1),
    S3 = F(Body, Ctxt, Env1, S2),
    S4 = add_continuation_jump(Next, Ctxt, S3),
    S5 = add_code([icode_label(FL)], S4),
    clause_tree(FailTree, VarList, F, State, Next, Fail, MState, Ctxt, Env, S5).

do_size(Instr, NextTree, FailTree, VarList, F, State, Next, Fail, MState, Ctxt, Env, S) ->
    SL = new_label(),
    FL = new_label(),
    S1 = translate_size_expr(Instr, State, VarList, SL, FL, Env, S),
    S2 = add_code([icode_label(SL)], S1),
    {S3, MState1} = clause_tree(NextTree, VarList, F, State, Next, Fail, MState, Ctxt, Env, S2),
    S4 = add_code([icode_label(FL)], S3),
    clause_tree(FailTree, VarList, F, State, Next, Fail, MState1, Ctxt, Env, S4).

do_match(Instr, NextTree, FailTree, VarList, F, State, Next, Fail, MState, Ctxt, Env, S) ->
    SL = new_label(),
    FL = new_label(),
    S1 = translate_match(Instr, VarList, SL, FL, S),
    S2 = add_code([icode_label(SL)], S1),
    {S3, MState1} = clause_tree(NextTree, VarList, F, State, Next, Fail, MState, Ctxt, Env, S2),
    S4 = add_code([icode_label(FL)], S3),
    clause_tree(FailTree, VarList, F, State, Next, Fail, MState1, Ctxt, Env, S4).

do_match_group(Instr, NextTree, FailTree, VarList, F, State, Next, Fail, MState, Ctxt, Env, S) ->
    ValList=cerl_binary_pattern_match:match_group_vals(Instr),
    Tag=cerl_binary_pattern_match:match_group_tag(Instr),
    FL = new_label(),
    FullCases=[{Val,{translate_value(Val),new_label()}}||Val<-ValList, is_ok_val(Val)],
    Length=length(FullCases),
    Cases=[Case||{_, Case} <- FullCases],
    ResVar = get_resvar(Tag, VarList),
    S1 = add_code([icode_switch_val(ResVar,FL,Length,Cases)], S), 
    do_cases(FullCases, NextTree, FailTree, FL, VarList, F, State, Next, Fail, MState, Ctxt, Env, S1).

do_cases([{Value,{_, Label}}|Rest], NextTree, FailTree, FL, VarList, F, State, 
	 Next, Fail, MState, Ctxt, Env, S) -> 
    {value, ClauseTree}=gb_trees:lookup(Value, NextTree),
    S1 = add_code([icode_label(Label)], S),
    {S2, MState1} = clause_tree(ClauseTree, VarList, F, State, Next, Fail, MState, Ctxt, Env, S1),
    do_cases(Rest, NextTree, FailTree, FL, VarList, F, State, Next, Fail, MState1, Ctxt, Env, S2); 
do_cases([], _NextTree, FailTree, FL, VarList, F, State, Next, Fail, MState, Ctxt, Env, S) ->
    S1 = add_code([icode_label(FL)], S),
    clause_tree(FailTree, VarList, F, State, Next, Fail, MState, Ctxt, Env, S1).

do_label(Instr, NextTree, VarList, F, State, Next, Fail, MState, Ctxt, Env, S) ->
    LabelAlias=cerl_binary_pattern_match:label_name(Instr),
    {Label, MState1}=get_correct_label(LabelAlias, MState),
    S1 = add_code([icode_goto(Label),icode_label(Label)], S),
    clause_tree(NextTree, VarList, F, State, Next, Fail, MState1, Ctxt, Env, S1).

do_goto(Instr, MState, S) ->    
    LabelAlias=cerl_binary_pattern_match:goto_label(Instr),
    {Label, MState1}=get_correct_label(LabelAlias, MState),
    S1 = add_code([icode_goto(Label)], S),
    {S1, MState1}.

translate_binseg(Instr, VarList, State, Env, S) ->
    Size = cerl_binary_pattern_match:read_seg_size(Instr),
    Offset = cerl_binary_pattern_match:read_seg_offset(Instr),
    {LiteralType,LiteralFlags} = cerl_binary_pattern_match:read_seg_type(Instr),
    V = cerl_binary_pattern_match:read_seg_tag(Instr),
    ResVar = get_resvar(V, VarList),
    Align = test_align(Offset),
    Flags=translate_flags(LiteralFlags, Align),
    Type = cerl:atom_val(LiteralType),
    OffsetVar = make_var(),
    OffsetConst = cerl_binary_pattern_match:size_const(Offset),
    {S1, SizeExpr} =
	case type_of_size(Size) of
	    const ->
		{S, Size};
	    all ->
		{S, Size};
	    var ->
		SizeVar = make_var(),
		S0 = add_size_code(Size, SizeVar, VarList, Env, S),
		{S0, SizeVar}
	end,
    S2 = add_offset_code(Offset, OffsetVar, VarList, Env, S1),
    add_final_code(Type, ResVar, SizeExpr, OffsetVar, OffsetConst, State, Flags, S2).

translate_value(Val) ->
    icode_const(cerl:concrete(Val)).

is_ok_val(Val) ->
    case cerl:concrete(Val) of
	X when integer(X) ->
	    true;
	X when float(X) ->
	    true;
	_ ->
	    false
    end.

translate_match(Instr, VarList, SL, FL, S) ->
    Val = cerl_binary_pattern_match:match_val(Instr),
    V = cerl_binary_pattern_match:match_tag(Instr),
    ResVar = get_resvar(V, VarList),
    case cerl:concrete(Val) of
	X when integer(X) ->
	    add_code([make_type([ResVar], ?TYPE_INTEGER(X), SL, FL)],
		     S);
	X when float(X) ->
	    V1 = make_var(),
	    L = new_label(),
	    %% First doing an "is float" test here might allow later
	    %% stages to use a specialized equality test.
	    add_code([make_type([ResVar], ?TYPE_IS_FLOAT, L, FL),
		      icode_label(L),
		      icode_move(V1, icode_const(X)),
		      make_if(?TEST_EQ, [ResVar, V1], SL, FL)],
		     S);
	_X ->
	    %%If the constant is not a float nor an integer
	    %%The match can not succeed
	    add_code([icode_goto(FL)], S)
    end.

translate_size_expr(Instr, {_,_,BinSize}, VarList, SL, FL, Env, S) ->
    SizeVar = make_var(),
    All = cerl_binary_pattern_match:size_all(Instr),
    S1=add_size_exp_code(Instr, SizeVar, VarList, FL, Env, S),
    S2=test_eight_div(SizeVar, FL, S1),
    test_size(SizeVar, BinSize, All, SL, FL, S2).

add_final_code(integer, ResVar, SizeExpr, OffsetVar, OffsetConst,
	       {Orig, OrigOffset, _}, Flags, S) ->
    Offset=cerl:int_val(OffsetConst),
    case cerl:is_c_int(SizeExpr) of
	true ->
	    Size=cerl:int_val(SizeExpr),
	    add_code([icode_call_primop([ResVar], {hipe_bsi_primop, {bs_get_integer, Size, Offset, Flags}}, 
					[OffsetVar, Orig, OrigOffset])], S);
	false ->
	    add_code([icode_call_primop([ResVar], {hipe_bsi_primop, {bs_get_integer, Offset, Flags}}, 
					[SizeExpr, OffsetVar, Orig, OrigOffset])], S)
    end;

add_final_code(float, ResVar, SizeExpr, OffsetVar, OffsetConst,
	       {Orig, OrigOffset, _}, Flags, S) ->
    Offset=cerl:int_val(OffsetConst),
    case cerl:is_c_int(SizeExpr) of
	true ->
	    Size=cerl:int_val(SizeExpr),
	    add_code([icode_call_primop([ResVar], {hipe_bsi_primop, {bs_get_float, Size, Offset, Flags}}, 
					[OffsetVar, Orig, OrigOffset])], S);
	false ->
	    add_code([icode_call_primop([ResVar], {hipe_bsi_primop, {bs_get_float, Offset, Flags}}, 
					[SizeExpr, OffsetVar, Orig, OrigOffset])], S)
    end;

add_final_code(binary, ResVar, SizeExpr, OffsetVar, OffsetConst,
	       {Orig, OrigOffset, BinSize}, Flags, S) ->
    Offset=cerl:int_val(OffsetConst),
    case type_of_size(SizeExpr) of
	const ->
	    Size=cerl:int_val(SizeExpr),
	    add_code([icode_call_primop([ResVar], {hipe_bsi_primop, {bs_get_binary, Size, Offset, Flags}}, 
					[OffsetVar, Orig, OrigOffset])], S);
	all ->
	    add_code([icode_call_primop([ResVar], {hipe_bsi_primop, {bs_get_binary_all, Offset, Flags}}, 
					[OffsetVar, Orig, OrigOffset, BinSize])], S);
	var ->
	    add_code([icode_call_primop([ResVar], {hipe_bsi_primop, {bs_get_binary, Offset, Flags}}, 
					[SizeExpr, OffsetVar, Orig, OrigOffset])], S)
    end.


type_of_size(Size) ->
    case cerl:is_c_int(Size) of
	true ->
	    const;
	false ->
	    {SizeExpr, _Unit}=Size,
	    case cerl:is_c_atom(SizeExpr) of
		true ->
		    all;
		false ->
		    var
	    end
    end.

add_size_code({{tag,Tag}, Unit}, SizeVar, VarList, _Env, S) ->
    UnitVal = cerl:int_val(Unit),
    NewSize = get_resvar(Tag, VarList),
    add_code([icode_call_primop([SizeVar], {hipe_bsi_primop, {bs_make_size, UnitVal}}, 
				[NewSize])], S);
add_size_code({Size, Unit}, SizeVar, _VarList, Env, S) ->
    NewSize = make_var(),
    UnitVal = cerl:int_val(Unit),
    S1 = expr(Size, [NewSize], #ctxt{final=false}, Env, S), 
    add_code([icode_call_primop([SizeVar], {hipe_bsi_primop, {bs_make_size, UnitVal}}, 
				[NewSize])], S1).
add_offset_code([{{tag,Tag}, Unit}|Rest], OffsetVar, VarList, Env, S) ->
    Temp=make_var(),
    NewSize = get_resvar(Tag, VarList),
    UnitVal = cerl:int_val(Unit), 
    Code = [icode_call_primop([Temp], {hipe_bsi_primop, {bs_make_size, UnitVal}}, 
			      [NewSize]),
	    icode_call_primop([OffsetVar], '+', [OffsetVar, Temp])],
    add_offset_code(Rest, OffsetVar, VarList, Env, add_code(Code, S));

add_offset_code([{Size, Unit}|Rest], OffsetVar, VarList, Env, S) ->
    Temp=make_var(),
    NewSize = make_var(),
    UnitVal = cerl:int_val(Unit),
    S1 = expr(Size, [NewSize], #ctxt{final=false}, Env, S), 
    Code = [icode_call_primop([Temp], {hipe_bsi_primop, {bs_make_size, UnitVal}}, 
			      [NewSize]),
	    icode_call_primop([OffsetVar], {hipe_bsi_primop, bs_add},[OffsetVar, Temp])],
    add_offset_code(Rest, OffsetVar, VarList, Env, add_code(Code, S1));
add_offset_code([], _OffsetVar, _VarList, _Env, S) ->
    S;
add_offset_code(Size, OffsetVar, VarList, Env, S) ->
  Vars = cerl_binary_pattern_match:size_vars(Size),
  DefVars = cerl_binary_pattern_match:size_def_vars(Size),
    Code = [icode_move(OffsetVar, icode_const(0))],
    add_offset_code(DefVars++Vars, OffsetVar, VarList, Env, add_code(Code, S)).

add_size_exp_code(Size, SizeVar, VarList, FL, Env, S) ->
    Vars = cerl_binary_pattern_match:size_vars(Size),
    DefVars = cerl_binary_pattern_match:size_def_vars(Size),
  Const = cerl_binary_pattern_match:size_const(Size),
    Code = [icode_move(SizeVar, icode_const(0))],
    add_size_exp_code(DefVars++Vars, Const, SizeVar, VarList, FL, Env, add_code(Code, S)).

add_size_exp_code([{{tag, Tag}, Unit}|Rest], Const, SizeVar, VarList, FL, Env, S) ->
    Temp=make_var(),
    NewSize = get_resvar(Tag, VarList),
    TL=new_label(),
    UnitVal = cerl:int_val(Unit),
    Code = [icode_guardop([Temp], {hipe_bsi_primop, {bs_make_size, UnitVal}}, 
			  [NewSize],TL,FL),
	    icode_label(TL),
	    icode_call_primop([SizeVar], '+',[SizeVar, Temp])],
    add_size_exp_code(Rest, Const, SizeVar, VarList, FL, Env, add_code(Code, S));


add_size_exp_code([{Size, Unit}|Rest], Const, SizeVar, VarList, FL, Env, S) ->
    Temp=make_var(),
    TL=new_label(),
    NewSize = make_var(),
    UnitVal = cerl:int_val(Unit),
    S1 = expr(Size, [NewSize], #ctxt{final=false}, Env, S),
    Code = [icode_guardop([Temp], {hipe_bsi_primop, {bs_make_size, UnitVal}}, 
			  [NewSize],TL,FL),
	    icode_label(TL),
	    icode_call_primop([SizeVar], {hipe_bsi_primop, bs_add},[SizeVar, Temp])],
    add_size_exp_code(Rest, Const, SizeVar, VarList, FL, Env, add_code(Code, S1));

add_size_exp_code([], Const, SizeVar, _VarList, _FL, _Env, S) ->
    CC=icode_const(cerl:int_val(Const)),
    Temp=make_var(),
    Code=[icode_move(Temp, CC),
	  icode_call_primop([SizeVar], {hipe_bsi_primop, bs_add},[SizeVar, Temp])],
    add_code(Code, S).

test_eight_div(SizeVar, FL, S) ->
    TL=new_label(),
    add_code([icode_guardop([], {hipe_bsi_primop, bs_div_test},[SizeVar],TL,FL),
	      icode_label(TL)], S).

test_size(SizeVar, BinSize, All, SL, FL, S) ->
    case cerl:atom_val(All) of
	all ->
	    add_code([icode_guardop([], {hipe_bsi_primop, bs_size_test_all},[SizeVar, BinSize],SL,FL)], S);
	_ ->
	    add_code([icode_guardop([], {hipe_bsi_primop, bs_size_test},[SizeVar, BinSize],SL,FL)], S)
    end.

test_align([{_Size, Unit}|Rest]) ->
    case cerl:int_val(Unit) band 7 of
	0 ->
	    test_align(Rest);
	_ ->
	    1
    end;
test_align([]) ->
    0;
test_align(Size) -> 
    Vars = cerl_binary_pattern_match:size_vars(Size),
    test_align(Vars).

get_resvar(N, VarList) ->
    lists:nth(N+1, VarList).

translate_bin_guard(BinGuard, VarList, Env, S) ->
    Matches = cerl_binary_pattern_match:bin_guard_matches(BinGuard),
    Env1=bind_matches(Matches, VarList, Env),
    {Env1,S}.

bind_matches([Match|Rest], VarList, Env) ->      
    Tag = cerl_binary_pattern_match:match_tag(Match),
    Val = cerl_binary_pattern_match:match_val(Match),
    T=get_resvar(Tag, VarList),
    bind_matches(Rest, VarList, bind_var(Val, T, Env));

bind_matches([], _VarList, Env) ->
    Env. 

translate_label_primop(LabelPrimop) ->
    ?PRIMOP_SET_LABEL = cerl:atom_val(cerl:primop_name(LabelPrimop)),
    [Ref] = cerl:primop_args(LabelPrimop),
    Ref.
