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
%% @copyright 2000-2002 Richard Carlsson
%% @doc Translation from (HiPE-ified) Core Erlang to HiPE Icode.
%% @see cerl_hipeify
%% @end

%% TODO: use type information to avoid tests, remove dead code, etc.
%% TODO: compile case-switches over true/false using shortcut code.
%% TODO: handle binaries
%% TODO: compile tail-called letrec-defined functions as goto-loops.
%% TODO: remove unnecessary reduction tests
%% TODO: generate branch prediction info

-module(cerl_to_icode).

-export([function/3, function/4, module/1, module/2]).

-import(lists, [mapfoldl/3]).

%% @spec module(Module::cerl()) -> [icode()]
%% @equiv module(T, [])

module(T) ->
    module(T, []).

%% @spec module(Module::cerl(), Options::[term()]) -> [icode()]
%%
%%	    cerl() = cerl:cerl()
%%	    icode() = hipe_icode:icode()
%%
%% @doc Transforms a Core Erlang module to linear HiPE Icode. The result
%% is a list of Icode function definitions. Currently, no options are
%% available.
%%
%% <p>Note: Except for the module name, which is included in the header
%% of each Icode function definition, the remaining information (exports
%% and attributes) associated with the module definition is not included
%% in the resulting Icode.</p>
%%
%% @see function/4
%% @see cerl_hipeify:module/1
%% @see cerl_lambdalift:module/1

module(T, Options) ->
    %% TODO: when we handle local letrecs, change the order of these:
    module_1(cerl_lambdalift:module(cerl_hipeify:module(T)), Options).

module_1(T, _Options) ->
    M = cerl:atom_val(cerl:module_name(T)),
    if atom(M) ->
	    ok;
       true ->
	    error_msg("bad module name: ~P.", [M, 5]),
	    throw(error)
    end,
    S = init(M),
    {Icode, _} = mapfoldl(fun function_definition/2,
			  S, cerl:module_defs(T)),
    Icode.

%% For now, we simply assume that all function bodies should have degree
%% one (i.e., return exactly one value). We clear the code ackumulator
%% before we start compiling each function.

function_definition({V, F}, S) ->
    N = var_name_to_fname(cerl:var_name(V)),
    S1 = s__set_code([], S),
    {Icode, S2} = function_1(N, F, 1, S1),
    {{hipe_icode:icode_fun(Icode), Icode}, S2}.

var_name_to_fname({A, N}) when atom(A), integer(N) ->
    A;    %% The stated arity is ignored.
var_name_to_fname(A) when atom(A) ->
    A;
var_name_to_fname(N) ->
    error_msg("bad function name: ~P.", [N, 5]),
    throw(error).

init(Module) ->
    reset_label_counter(),		     
    s__new(Module).


%% @spec function(Module::atom(), Name::atom(), Function::cerl()) ->
%%           icode()
%% @equiv function(Module, Name, Fun, 1)

function(Module, Name, Fun) ->
    function(Module, Name, Fun, 1).


%% @spec function(Module::atom(), Name::atom(), Fun::cerl(),
%%                Degree::integer()) -> icode()
%%
%% @doc Transforms a Core Erlang function to a HiPE Icode function
%% definition. <code>Fun</code> must represent a fun-expression, which
%% may not contain free variables. <code>Module</code> and
%% <code>Name</code> specify the module and function name of the
%% resulting Icode function.
%%
%% <p><code>Degree</code> specifies the number of values the function is
%% expected to return; this is typically 1 (one); cf.
%% <code>function/3</code>.</p>
%%
%% <p>Notes: 
%% <ul>
%%   <li>Last call optimization is handled, even when the tail call is
%%   "hidden" by let-definitions.</li>
%%
%%   <li>It is assumed that all <code>primop</code> calls in the code
%%   represent Icode primops or macro instructions, and that all
%%   inter-module calls (both calls to statically named functions, and
%%   dynamic meta-calls) represent <em>actual</em> inter-module calls -
%%   not primitive or built-in operations.</li>
%%
%%   <li>The following primops (see
%%   "<code>cerl_hipe_primops.hrl</code>") are detected by the
%%   translation and handled specially:
%%   <table>
%%     <tr><td><code>exit/1</code>, <code>throw/1</code>,
%%             <code>error/1</code></td>
%%           <td>generate exceptions</td></tr>
%%     <tr><td><code>not/1</code>, <code>and/2</code>,
%%             <code>or/2</code></td>
%%           <td>strict boolean operators</td></tr>
%%     <tr><td><code>'=='/2</code>, <code>'/='/2</code></td>
%%           <td>arithmetic (un)equality</td></tr>
%%     <tr><td><code>'=:='/2</code>, <code>'=/='/2</code></td>
%%           <td>exact (un)equality</td></tr>
%%     <tr><td><code>'&lt;'/2</code>, <code>'>'/2</code></td>
%%           <td>smaller/greater than</td></tr>
%%     <tr><td><code>'=&lt;'/2</code></td>
%%           <td>smaller than or equal to</td></tr>
%%     <tr><td><code>'>='/2</code></td>
%%           <td>greater than or equal to</td></tr>
%%     <tr><td><code>receive_select/0</code></td>
%%           <td>select current message</td></tr>
%%     <tr><td><code>receive_next/0</code></td>
%%           <td>loop to try next message</td></tr>
%%     <tr><td><code>make_fun/6</code></td>
%%           <td>create a functional value</td></tr>
%%     <tr><td><code>apply_fun/2</code></td>
%%           <td>apply a functional value</td></tr>
%%   </table>
%%   The boolean operators are expected to be used in guard expressions,
%%   as well as in other expressions. See below for details on the
%%   <code>receive_...</code> operations.</li>
%%
%%   <li>Compilation of clauses is simplistic. No pattern matching
%%   compilation or similar optimizations is done. Indexing is not (yet)
%%   done. Guards that are <code>true</code> or <code>false</code> are
%%   recognized as trivially true/false; for all other guards, code will
%%   be generated. Catch-all clauses (with <code>true</code> guard and
%%   variable-only patterns) are detected, and any following clauses are
%%   discarded.</li>
%% </ul></p>
%%
%% <p><b>Important</b>: This function does not handle occurrences of
%% fun-expressions inside the body of <code>Fun</code> itself, nor
%% <code>apply</code>-expressions whose operators are not locally bound
%% function variables. These must be transformed away before this
%% function is called, by use of lambda lifting and the
%% <code>make_fun</code> and <code>call_fun</code> primitive operations
%% to create and apply functional values.</p>
%%
%% <p><code>receive</code>-expressions are expected to have a particular
%% form:
%% <ul>
%%   <li>There must be exactly one clause, with the atom
%%   <code>true</code> as guard, and only a single variable as pattern.
%%   The variable will be bound to a message in the mailbox, and can be
%%   referred to in the clause body.</li>
%%
%%   <li>In the body of that clause, all paths must execute one of the
%%   primitive operations <code>receive_select/0</code> or
%%   <code>receive_next/0</code> before another
%%   <code>receive</code>-statement might be executed.
%%   <code>receive_select/0</code> always returns, but without a value,
%%   while <code>receive_next/0</code> never returns, either causing
%%   the nearest surrounding receive-expression to be re-tried with the
%%   next message in the input queue, or timing out.</li>
%% </ul></p>
%%
%% @see function/3

-include("cerl_hipe_primops.hrl").

%% Icode primitive operation names

-define(OP_REDTEST, redtest).
-define(OP_CONS, cons).
-define(OP_TUPLE, mktuple).
-define(OP_UNSAFE_HD, unsafe_hd).
-define(OP_UNSAFE_TL, unsafe_tl).
-define(OP_UNSAFE_ELEMENT(N), {unsafe_element, N}).
-define(OP_GET_MESSAGE, get_msg).
-define(OP_NEXT_MESSAGE, next_msg).
-define(OP_SELECT_MESSAGE, select_msg).
-define(OP_SET_TIMEOUT, set_timeout).
-define(OP_CLEAR_TIMEOUT, clear_timeout).
-define(OP_WAIT_FOR_MESSAGE, suspend_msg).
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
-define(TEST_MAILBOX_EMPTY, mbox_empty).
-define(TEST_WAIT_FOR_MESSAGE_OR_TIMEOUT, suspend_msg_timeout).

%% Icode type tests

-define(TYPE_ATOM(X), {atom, X}).
-define(TYPE_INTEGER(X), {integer, X}).
-define(TYPE_FIXNUM(X), {integer, X}).    % for now
-define(TYPE_CONS, cons).
-define(TYPE_NIL, nil).
-define(TYPE_TUPLE(N), {tuple, N}).
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
-define(TYPE_IS_REFERENCE, reference).
-define(TYPE_IS_TUPLE, tuple).

%% Boolean temporary values

-define(BOOL_TRUE, 1).
-define(BOOL_FALSE, 0).
-define(BOOL_IS_TRUE, ?TYPE_FIXNUM(0)).
-define(BOOL_IS_FALSE, ?TYPE_FIXNUM(0)).

%% Record definitions

-record(ctxt, {final = false,
	       effect = false,
	       fail = [],
	       class = expr,
	       'receive'}).

-record('receive', {loop}).
-record(var, {id}).
-record('fun', {label}).

%% Main translation function:

function(Module, Name, Fun, Degree) ->
    S = init(Module),
    {Icode, _} = function_1(Name, Fun, Degree, S),
    Icode.

%% We use the following convention for tail-recursive calls within the
%% same module:

function_1(Name, Fun, Degree, S) ->
    reset_var_counter(),
    LowV = max_var(),
    LowL = max_label(),

    %% Create input variables for the function parameters, and a list of
    %% target variables for the result of the function.
    Args = cerl:fun_vars(Fun),
    Arity = length(Args),
    ArgType = get_type(Args),
    Vs = make_vars(Arity),
    Vs1 = make_vars(Arity),    % input variable temporaries
    Ts = make_vars(Degree),

    %% Initialise environment and context.
    Env = bind_vars(Args, Vs, env__new()),
    %% TODO: if the function returns no values, we can use effect mode.
    Ctxt = #ctxt{final = true, effect = false},

    %% Each basic block must begin with a label; also, we need to do a
    %% reduction test at the start of each function. Note that we
    %% immediately transfer the input parameters to local variables, for
    %% our self-recursive calling convention.
    Start = new_label(),
    Local = new_label(),
    S1 = add_code([icode_label(Start)]
		  ++ make_moves(Vs, Vs1)
		  ++ [icode_label(Local)]
		  ++ make_op(?OP_REDTEST, [], [], #ctxt{}),
		  s__set_function({Name, Arity}, S)),
    S2 = expr(cerl:fun_body(Fun), Ts, Ctxt, Env,
	      s__set_local_entry({Local, Vs}, S1)),

    %% This creates an Icode function definition. The ranges of used
    %% variables and labels below should be nonempty. Note that the
    %% input variables are `Vs1', which will be transferred to `Vs' (see
    %% above).
    HighV = new_var(),    % assure nonempty range
    HighL = max_label(),
    Lambda = lists:member(lambda, cerl:get_ann(Fun)),
    Leaf = false,  %% TODO: annotate functions with is-leaf info
    Module = s__get_module(S2),
    Code = s__get_code(S2),
    Function = icode_icode({Module, Name, Arity}, Vs1, Lambda, Leaf,
			   Code, {LowV, HighV}, {LowL, HighL}, ArgType),
    {Function, S2}.

expr(E, Ts, Ctxt, Env, S) ->
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
	'catch' ->
	    expr_catch(E, Ts, Ctxt, Env, S);
	binary ->
	    expr_binary(E, Ts, Ctxt, Env, S);
	'fun' ->
	    error_msg("cannot compile lambda-valued "
		      "expressions directly to Icode."),
	    throw(error);
	letrec ->
	    %% TODO: compile letrec-definitions as local code
	    error_msg("cannot compile letrec-expressions "
		      "directly to Icode."),
	    throw(error)
    end.

%% This is for when we need new target variables for all of the
%% expressions in the list, and evaluate them for value in a
%% non-last-call context.

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


%% Variables

expr_var(_E, _Ts, #ctxt{effect = true}, _Env, S) ->
    S;
expr_var(E, Ts, Ctxt, Env, S) ->
    Id = cerl:var_name(E),
    case env__lookup(Id, Env) of
	{ok, #var{id = V}} ->
	    case Ctxt#ctxt.final of
		false ->
		    glue([V], Ts, S);
		true ->
		    add_return([V], S)
	    end;
	{ok, #'fun'{}} ->
	    error_msg("cannot handle functional value: ~P.",
		      [Id, 5]),
	    throw(error);
	error ->
	    error_msg("undefined variable: ~P.", [Id, 5]),
	    throw(error)
    end.

%% This handles all constants, both atomic and compound:

expr_literal(_E, _Ts, #ctxt{effect = true}, S) ->
    S;
expr_literal(E, [V] = Ts, Ctxt, S) ->
    Code = [icode_mov(V, icode_const(cerl:concrete(E)))],
    maybe_return(Ts, Ctxt, add_code(Code, S));
expr_literal(E, Ts, _Ctxt, _S) ->
    error_degree_mismatch(length(Ts), E),
    throw(error).

%% Multiple value aggregate

expr_values(E, Ts, #ctxt{effect = true} = Ctxt, Env, S) ->
    {_, S1} = exprs(cerl:values_es(E), Ts, Ctxt#ctxt{final = false},
		    Env, S),
    S1;
expr_values(E, Ts, Ctxt, Env, S) ->
    S1 = exprs(cerl:values_es(E), Ts, Ctxt#ctxt{final = false}, Env, S),
    maybe_return(Ts, Ctxt, S1).

%% Nonconstant tuples and cons cells

expr_tuple(E, _Ts, #ctxt{effect = true} = Ctxt, Env, S) ->
    expr_list(cerl:tuple_es(E), Ctxt, Env, S);
expr_tuple(E, [_V] = Ts, Ctxt, Env, S) ->
    {Vs, S1} = expr_list(cerl:tuple_es(E), Ctxt, Env, S),
    S2 = add_code(make_op(?OP_TUPLE, Ts, Vs, Ctxt), S1),
    maybe_return(Ts, Ctxt, S2);
expr_tuple(E, Ts, _Ctxt, _Env, _S) ->
    error_degree_mismatch(length(Ts), E),
    throw(error).

expr_cons(E, _Ts, #ctxt{effect = true} = Ctxt, Env, S) ->
    expr_list([cerl:cons_hd(E), cerl:cons_tl(E)], Ctxt, Env, S);
expr_cons(E, [_V] = Ts, Ctxt, Env, S) ->
    {Vs, S1} = expr_list([cerl:cons_hd(E), cerl:cons_tl(E)],
			 Ctxt, Env, S),
    S2 = add_code(make_op(?OP_CONS, Ts, Vs, Ctxt), S1),
    maybe_return(Ts, Ctxt, S2);
expr_cons(E, Ts, _Ctxt, _Env, _S) ->
    error_degree_mismatch(length(Ts), E),
    throw(error).

%% We want to make sure we are not easily tricked by expressions hidden
%% in contexts like "let X = Expr in X"; this should not destroy last
%% call properties.

expr_let(E, Ts, Ctxt, Env, S) ->
    E1 = reduce_expr(E),
    case cerl:is_c_let(E1) of
	true ->
	    expr_let_1(E1, Ts, Ctxt, Env, S);
	false ->
	    %% Redispatch the new expression.
	    expr(E1, Ts, Ctxt, Env, S)
    end.

expr_let_1(E, Ts, Ctxt, Env, S) ->
    Vars = cerl:let_vars(E),
    Vs = make_vars(length(Vars)),
    S1 = expr(cerl:let_arg(E), Vs,
	      Ctxt#ctxt{effect = false, final = false}, Env, S),
    Env1 = bind_vars(Vars, Vs, Env),
    B = cerl:let_body(E),
    expr(B, Ts, Ctxt, Env1, S1).

%% To compile a sequencing operator, we generate code for effect only
%% for the first expression (the "argument") and then use the
%% surrounding context for the second expression (the "body") .

expr_seq(E, Ts, Ctxt, Env, S) ->
    Ctxt1 = Ctxt#ctxt{effect = true, final = false},
    S1 = expr(cerl:seq_arg(E), [], Ctxt1, Env, S),
    expr(cerl:seq_body(E), Ts, Ctxt, Env, S1).

expr_binary(E, [V]=Ts, #ctxt{fail=FL, class = guard}=Ctxt, Env, S) ->
    SL1 = new_label(),
    SL2 = new_label(),
    Segs = cerl:binary_segs(E),
    S1 = add_code([icode_guardop([], {hipe_bs_primop, {bs_init, 0,0}}, [], SL1, FL),
		   icode_label(SL1)], S),
    Vars = make_vars(length(Segs)),
    S2 = bin_segs(Segs, Vars, Ctxt, Env, 0, S1), 
    S3 = add_code([icode_guardop([V], {hipe_bs_primop, bs_final}, [], SL2, FL),
		   icode_label(SL2)], S2),
    maybe_return(Ts, Ctxt, S3);

expr_binary(E, [V]=Ts, Ctxt, Env, S) ->
    Segs = cerl:binary_segs(E),
    S1 = add_code([icode_call_primop([], {hipe_bs_primop, {bs_init, 0,0}}, [])], S),
    Vars = make_vars(length(Segs)),
    S2 = bin_segs(Segs, Vars, Ctxt, Env, S1, 0), 
    S3 = add_code([icode_call_primop([V], {hipe_bs_primop, bs_final}, [])], S2),
    maybe_return(Ts, Ctxt, S3).

bin_segs([Seg|Rest], [T|Ts], Ctxt=#ctxt{}, Env, S, Align) ->
    {S1, NewAlign} = bin_seg(Seg, [T], Ctxt, Env, S, Align),
    bin_segs(Rest, Ts, Ctxt, Env, S1, NewAlign);
bin_segs([], [], #ctxt{}, _Env, S, _Align) ->
    S.

bin_seg(E, Ts, Ctxt, Env, S, Align) ->
    Size = cerl:bin_seg_size(E),
    Unit = cerl:bin_seg_unit(E),
    LiteralFlags = cerl:bin_seg_flags(E),
    Val = cerl:bin_seg_val(E),
    Type = cerl:concrete(cerl:bin_seg_type(E)),
    S0 = expr(Val, Ts, Ctxt#ctxt{final = false}, Env, S),
    ConstInfo = get_const_info(Val, Type),
    Flags = translate_flags(LiteralFlags, Align),
    SizeInfo = calculate_size(Unit, Size, Align, Env, S0),
    bin_seg_gen_op(Ts, Ctxt, SizeInfo, ConstInfo, Type, Flags).

bin_seg_gen_op([V], #ctxt{fail=FL, class=guard}, SizeInfo, ConstInfo, Type, Flags) ->
    SL = new_label(),
    case SizeInfo of
	{NewUnit, NewArgs, S1, NewAlign} ->
	    Args = [V|NewArgs],
	    Name =
		case Type of
		    integer ->
			{bs_put_integer, NewUnit, Flags, ConstInfo};
		    float ->
			{bs_put_float, NewUnit, Flags, ConstInfo};
		    binary ->
			{bs_put_binary, NewUnit, Flags}
		end,
	    {add_code([icode_guardop([], {hipe_bs_primop, Name} ,Args, SL, FL),
		       icode_label(SL)], S1), NewAlign};
	{all, NewAlign, S1} ->
	    Type = binary,
	    Name = {bs_put_binary_all, Flags},
	    {add_code([icode_guardop([], {hipe_bs_primop, Name}, [V], SL, FL),
		       icode_label(SL)], S1), NewAlign}
    end;

bin_seg_gen_op([V], _Ctxt, SizeInfo, ConstInfo, Type, Flags) ->
    case SizeInfo of
	{NewUnit, NewArgs, S, NewAlign} ->
	    Args = [V|NewArgs],
	    Name =
		case Type of
		    integer ->
			{bs_put_integer, NewUnit, Flags, ConstInfo};
		    float ->
			{bs_put_float, NewUnit, Flags, ConstInfo};
		    binary ->
			{bs_put_binary, NewUnit, Flags}
		end,
	    {add_code([icode_call_primop([], {hipe_bs_primop, Name} ,Args)], S), NewAlign};
	{all, NewAlign, S} ->
	    Type = binary,
	    Name = {bs_put_binary_all, Flags},
	    {add_code([icode_call_primop([], {hipe_bs_primop, Name}, [V])], S), NewAlign}
    end.

%% Note that the arity of the called function only depends on the length
%% of the argument list; the arity stated by the function name is
%% ignored.

expr_apply(E, Ts, Ctxt, Env, S) ->
    Op = reduce_expr(cerl:apply_op(E)),
    Name = case cerl:is_c_var(Op) of
	       true ->
		   case cerl:var_name(Op) of
		       {N, A} = Id when atom(N), integer(A) ->
			   case env__lookup(Id, Env) of
			       error ->
				   %% This is assumed to be a function
				   %% in the same module, not
				   %% necessarily exported.
				   Id;
			       {ok, #'fun'{}} ->
				   %% This is assumed to be a
				   %% nonescaping, nonexported function,
				   %% defined locally by a
				   %% letrec-expression.
				   %% TODO: letrec-calls
				   throw(calling_local_function_not_handled);
			       {ok, #var{}} ->
				   error_msg(
				     "cannot use functional "
				     "values indirectly: ~P.",
				     [Id, 5]),
				   throw(error)
			   end;
		       _ ->
			   error_nonlocal_application(Op),
			   throw(error)
		   end;
	       false ->
		   error_nonlocal_application(Op),
		   throw(error)
	   end,
    {Vs, S1} = expr_list(cerl:apply_args(E), Ctxt, Env, S),
    add_local_call(Name, Vs, Ts, Ctxt, S1, get_type(E)).

%% If we know the module and function names statically, we do not have
%% to go through the much more inefficient generic meta-call operator.

expr_call(E, Ts, Ctxt, Env, S) ->
    Module = reduce_expr(cerl:call_module(E)),
    Name = reduce_expr(cerl:call_name(E)),
    case cerl:is_c_atom(Module) and cerl:is_c_atom(Name) of
	true ->
	    M = cerl:atom_val(Module),
	    N = cerl:atom_val(Name),
	    {Vs, S1} = expr_list(cerl:call_args(E), Ctxt, Env, S),
	    Code = case Ctxt#ctxt.final of
		       false ->
			   [icode_call_remote(Ts, M, N, Vs)];
		       true ->
			   [icode_enter_remote(M, N, Vs)]
		   end,
	    add_code(Code, S1);
	false ->
	    %% Metacalls are handled using the Icode `apply'
	    %% instruction.
	    Args = cerl:make_list(cerl:call_args(E)),
	    {As, S1} = expr_list([Module, Name, Args], Ctxt, Env, S),
	    Code = case Ctxt#ctxt.final of
		       false ->
			   [icode_call_remote(Ts, erlang, apply, As)];
		       true ->
			   [icode_enter_remote(erlang, apply, As)]
		   end,
	    add_code(Code, S1)
    end.

%% Core Erlang primop calls are generally mapped directly to Icode
%% primop calls, with a few exceptions (listed above), which are
%% expanded inline, sometimes depending on context. Note that primop
%% calls do not have specialized last-call forms.

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
expr_primop_1(?PRIMOP_RECEIVE_SELECT, 0, _As, _E, Ts, Ctxt, _Env, S) ->
    primop_receive_select(Ts, Ctxt, S);
expr_primop_1(?PRIMOP_RECEIVE_NEXT, 0, _As, _E, _Ts, Ctxt, _Env, S) ->
    primop_receive_next(Ctxt, S);
expr_primop_1(?PRIMOP_IDENTITY, 1, [A], _E, Ts, Ctxt, Env, S) ->
    expr(A, Ts, Ctxt, Env, S);  % used for unary plus
expr_primop_1(?PRIMOP_NEG, 1, [A], _, Ts, Ctxt, Env, S) ->
    E = cerl:c_primop(cerl:c_atom('-'), [cerl:c_int(0), A]),
    expr_primop(E, Ts, Ctxt, Env, S);
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

expr_primop_2(?PRIMOP_EXIT, 1, [V], _Ts, Ctxt, S) ->
    add_exit(V, Ctxt, S);
expr_primop_2(?PRIMOP_THROW, 1, [V], _Ts, Ctxt, S) ->
    add_throw(V, Ctxt, S);
expr_primop_2(?PRIMOP_ERROR, 1, [V], _Ts, Ctxt, S) ->
    add_error(V, Ctxt, S);
expr_primop_2(?PRIMOP_ERROR, 2, [V, F], _Ts, Ctxt, S) ->
    add_error(V, F, Ctxt, S);
expr_primop_2(Name, _Arity, Vs, Ts, Ctxt, S) ->
    maybe_return(Ts, Ctxt, add_code(make_op(Name, Ts, Vs, Ctxt), S)).

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
 	    S2 = add_code(make_op(?OP_MAKE_FUN(Module, Name, Arity,
 					       Hash, Index),
 				  Ts, Vs, Ctxt),
 			  S1),
	    maybe_return(Ts, Ctxt, S2);
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
	    S1 = expr(F, [V], Ctxt, Env, S),
	    S2 = add_code(make_op(?OP_FUN_ELEMENT(cerl:int_val(N)),
				  Ts, [V], Ctxt),
			  S1),
	    maybe_return(Ts, Ctxt, S2);
	false ->
	    error_primop_badargs(?PRIMOP_FUN_ELEMENT, As),
	    throw(error)
    end.

%% Catch-expressions:

expr_catch(E, [T] = Ts, Ctxt, Env, S) ->
    Cont = new_label(),
    Catch = new_label(),
    S1 = add_code([icode_pushcatch(Catch,Cont),icode_label(Cont)], S),
    Next = new_continuation_label(Ctxt),
    S2 = expr(cerl:catch_body(E), Ts, Ctxt, Env, S1),
    S3 = add_code([icode_remove_catch(Catch)], S2),
    S4 = add_continuation_jump(Next, Ctxt, S3),
    S5 = add_code([icode_label(Catch),
		   icode_restore_catch(T, Catch)],
		  S4),
    add_continuation_label(Next, Ctxt, S5);
expr_catch(_E, _Ts, _Ctxt, _Env, _S) ->
    error_msg("use of catch expression expects degree other than 1."),
    throw(error).

%% Try-expressions:

%% For now, we rewrite try-expressions to be simulated using
%% catch-expressions and unique references. The generated code has the
%% following structure:
%%
%%  let V0 = primop 'make_ref'() in
%%    case catch {V0, #TryArg#} of
%%      {V1, V2} when primop '=:='(V0, V1) -> V2
%%      V1 when 'true' ->
%%        let <#TryVars#> =
%%          case V1 of
%%            {'EXIT', V2} when 'true' -> <'EXIT', V2>
%%            V2 when 'true' -> <'THROW', V2>
%%          end
%% 	  in #TryBody#
%%     end
%%
%% (Note that even though we introduce new variables, they are only used
%% locally, so we never need to rename existing variables.)

expr_try(E, Ts, Ctxt = #ctxt{class = guard}, Env, S) ->
    %% The limited form of try-expressions allowed in guards:
    %%     "try Expr of X -> X catch <X1,X2> -> 'false' end".
    Fail = new_label(),
    Next = new_label(),
    Ctxt1 = Ctxt#ctxt{fail = Fail},
    S1 = expr(cerl:try_arg(E), Ts, Ctxt1, Env, S),
    S2 = add_code([icode_goto(Next), icode_label(Fail)], S1),
    S3 = expr(cerl:c_atom(false), Ts, Ctxt, Env, S2),
    add_code([icode_label(Next)], S3);
expr_try(_E, _Ts, _Ctxt, _Env, _S) ->
    throw(try_expressions_not_handled).
%%%     [Id0, Id1, Id2] = env__new_integer_keys(3, Env),
%%%     V0 = cerl:c_var(Id0),
%%%     V1 = cerl:c_var(Id1),
%%%     V2 = cerl:c_var(Id2),
%%%     True = cerl:c_atom('true'),
%%%     Exit = cerl:c_atom('EXIT'),
%%%     Throw = cerl:c_atom('THROW'),
%%%     Cs2 = [cerl:c_clause([cerl:c_tuple([Exit, V2])], True,
%%% 			  cerl:c_values([Exit, V2])),
%%% 	   cerl:c_clause([V2], True,
%%% 			  cerl:c_values([Throw, V2]))],
%%%     Body = cerl:c_let(cerl:try_vars(E),
%%% 		       cerl:c_case(V1, Cs2),
%%% 		       cerl:try_body(E)),
%%%     Guard = cerl:c_primop(cerl:c_atom('=:='), [V0, V1]),
%%%     Cs1 = [cerl:c_clause([cerl:c_tuple([V1, V2])], Guard, V2),
%%% 	   cerl:c_clause([V1], True, Body)],
%%%     Catch = cerl:c_catch(cerl:c_tuple([V0, cerl:try_expr(E)])),
%%%     MakeRef = cerl:c_primop(cerl:c_atom(make_ref), []),
%%%     E1 = cerl:c_let([V0], MakeRef, cerl:c_case(Catch, Cs1)),
%%% %%%    info_msg("\n" ++ cerl_prettypr:format(E1), []),
%%%     expr(Effect, E1, Ts, Ctxt, Env, S).

%% Receive-expressions: There may only be exactly one clause, which must
%% be a trivial catch-all with exactly one (variable) pattern. Each
%% message will be read from the mailbox and bound to the pattern
%% variable; the body of the clause must do the switching and call
%% either of the primops `receive_select/0' or `receive_next/0'.

expr_receive(E, Ts, Ctxt, Env, S) ->
    case cerl:receive_clauses(E) of
	[C] ->
	    case cerl:clause_pats(C) of
		[_] ->
		    case cerl_clauses:is_catchall(C) of
			true ->
			    expr_receive_1(C, E, Ts, Ctxt, Env, S);
			false ->
			    error_msg("receive-expression clause "
				      "is not a catch-all."),
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
%%	if-tests:	mbox_empty()
%%			suspend_msg_timeout()
%%
%%	primops:	V = get_msg()
%%			select_msg()
%%			next_msg()
%%			set_timeout(T)
%%			clear_timeout()
%%			suspend_msg()
%%
%% `mbox_empty' tests if the mailbox is empty or not. `get_msg' reads
%% the message currently pointed to by the implicit message pointer.
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

expr_receive_1(C, E, Ts, Ctxt, Env, S0) ->
    Expiry = reduce_expr(cerl:receive_timeout(E)),
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
		   make_if(?TEST_MAILBOX_EMPTY, [], Wait, Match),
		   icode_label(Wait)], S1),

    %% The wait-for-message section looks a bit different depending on
    %% whether we actually need to set a timer or not.
    S3 = case After of
	     'infinity' ->
		 %% Only wake up when we get new messages, and never
		 %% execute the expiry body.
		 add_code(make_op(?OP_WAIT_FOR_MESSAGE, [], [], #ctxt{})
			  ++ [icode_goto(Match)], S2);
	     0 ->
		 %% Zero limit - go directly to the expiry body.
		 S2;
	     _ ->
		 %% Other value - set the timer (if it is already set,
		 %% nothing is changed) and wait for a message or
		 %% timeout. Reset the message pointer upon timeout.
		 Timeout = new_label(),
		 Ctxt0 = #ctxt{},
		 add_code(make_op(?OP_SET_TIMEOUT, [], [T], Ctxt0)
			  ++ [make_if(?TEST_WAIT_FOR_MESSAGE_OR_TIMEOUT,
				      [], Match, Timeout),
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
				       expr(cerl:receive_action(E), Ts,
					    Ctxt, Env, S3))
	 end,

    %% When we compile the primitive operations that select the current
    %% message or loop to try the next message (see the functions
    %% 'primop_receive_next' and 'primop_receive_select'), we will use
    %% the receive-loop label in the context (i.e., that of the nearest
    %% enclosing receive expression).
    Ctxt1 = Ctxt#ctxt{'receive' = #'receive'{loop = Loop}},

    %% The pattern variable of the clause will be mapped to `V', which
    %% holds the message, so it can be accessed in the clause body:
    S5 = add_code([icode_label(Match)]
		  ++ make_op(?OP_GET_MESSAGE, [V], [], #ctxt{}),
		  S4),
    S6 = clauses([C], Ts, [V], Ctxt1, Env, S5),
    add_continuation_label(Next, Ctxt, S6).

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
	    warning_not_in_receive(?PRIMOP_RECEIVE_NEXT),
	    S0
    end.

primop_receive_select(Ts, #ctxt{'receive' = R} = Ctxt, S) ->
    case R of
	#'receive'{} ->
	    S1 = add_code(make_op(?OP_SELECT_MESSAGE, [], [], Ctxt),
			  S),
	    maybe_return(Ts, Ctxt, S1);
	_ ->
	    warning_not_in_receive(?PRIMOP_RECEIVE_SELECT),
	    S
    end.

%% Case expressions

%% We know that the pattern matching compilation has split all switches
%% into separate groups of tuples, integers, atoms, etc., and that each
%% such switch over a group of constructors is protected by a type test.
%% Thus, it is straightforward to generate switch instructions.

expr_case(E, Ts, Ctxt, Env, S) ->
    Cs = cerl:case_clauses(E),
    Vs = make_vars(clauses_degree(Cs)),
    S1 = expr(cerl:case_arg(E), Vs,
	      Ctxt#ctxt{final = false, effect = false}, Env, S),
    case is_constant_switch(Cs) of
	true ->
	    switch_val_clauses(Cs, Ts, Vs, Ctxt, Env, S1);
	false ->
	    case is_tuple_switch(Cs) of
		true ->
		    switch_tuple_clauses(Cs, Ts, Vs, Ctxt, Env, S1);
		false ->
		    clauses(Cs, Ts, Vs, Ctxt, Env, S1)
	    end
    end.

%% We check only the first clause to see what the degree of the
%% corresponding switch expression should be. (I.e., we assume that we
%% are given nice code that does not combine clauses with differing
%% pattern degrees.)

clauses_degree([C | _Cs]) ->
    length(cerl:clause_pats(C)).

%% Check if a list of clauses represents a switch over a number (more
%% than 1) of constants (atoms or integers/floats), or tuples (whose
%% elements are all variables)

is_constant_switch(Cs) ->
    is_switch(Cs, fun (P) -> (cerl:type(P) == literal) andalso
				 is_constant(cerl:concrete(P)) end).

is_tuple_switch(Cs) ->
    is_switch(Cs, fun (P) -> cerl:is_c_tuple(P) andalso
				 all_vars(cerl:tuple_es(P)) end).

all_vars([E | Es]) ->
    case cerl:is_c_var(E) of
	true -> all_vars(Es);
	false -> false
    end;
all_vars([]) -> true.

is_switch(Cs, F) ->
    is_switch(Cs, F, 0).

is_switch([C | Cs], F, N) ->
    case is_simple_clause(C) of
	{true, P} ->
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

is_simple_clause(C) ->
    case cerl:clause_pats(C) of
	[P] ->
	    G = cerl:clause_guard(C),
	    case cerl_clauses:eval_guard(G) of
		{value, true} -> {true, P};
		_ -> false
	    end;
	_ -> false
    end.

switch_val_clauses(Cs, Ts, Vs, Ctxt, Env, S) ->
    switch_clauses(Cs, Ts, Vs, Ctxt, Env,
		   fun (P) -> cerl:concrete(P) end,
		   fun icode_switch_val/4,
		   fun val_clause_body/9,
		   S).

val_clause_body(_N, _V, C, Ts, Next, _Fail, Ctxt, Env, S) ->
    clause_body(C, Ts, Next, Ctxt, Env, S).

switch_tuple_clauses(Cs, Ts, Vs, Ctxt, Env, S) ->
    switch_clauses(Cs, Ts, Vs, Ctxt, Env, 
		   fun (P) -> cerl:tuple_arity(P) end,
		   fun icode_switch_tuple_arity/4,
		   fun tuple_clause_body/9,
		   S).

tuple_clause_body(N, V, C, Ts, Next, Fail, Ctxt, Env, S0) ->
    Vs = make_vars(N),
    S1 = tuple_elements(Vs, V, S0),
    Es = cerl:tuple_es(hd(cerl:clause_pats(C))),
    {Env1, S2} = patterns(Es, Vs, Fail, Env, S1),
    clause_body(C, Ts, Next, Ctxt, Env1, S2).

switch_clauses(Cs, Ts, [V], Ctxt, Env, GetVal, Switch, Body, S0) ->
    Cs1 = [switch_clause(C, GetVal) || C <- Cs],
    Cases = [{V, L} || {V, L, _} <- Cs1],
    Default = [C || {default, C} <- Cs1],
    Fail = new_label(),
    S1 = add_code([Switch(V, Fail, length(Cases), Cases)], S0),
    Next = new_continuation_label(Ctxt),
    S2 = case Default of
	     [] -> add_infinite_loop(Fail, S1);
	     [C] ->
		 clause_body(C, Ts, Next, Ctxt, Env,
			     add_code([icode_label(Fail)], S1))
	 end,
    S3 = switch_cases(Cs1, V, Ts, Next, Fail, Ctxt, Env, Body, S2),
    add_continuation_label(Next, Ctxt, S3).

switch_clause(C, F) ->
    [P] = cerl:clause_pats(C),
    L = new_label(),
    case cerl:type(P) of
	var -> {default, C};
	_ -> {icode_const(F(P)), L, C}
    end.

switch_cases([{N, L, C} | Cs], V, Ts, Next, Fail, Ctxt, Env, Body,
	     S0) ->
    S1 = add_code([icode_label(L)], S0),
    S2 = Body(icode_const_val(N), V, C, Ts, Next, Fail, Ctxt, Env, S1),
    switch_cases(Cs, V, Ts, Next, Fail, Ctxt, Env, Body, S2);
switch_cases([_ | Cs], V, Ts, Next, Fail, Ctxt, Env, Body, S) ->
    switch_cases(Cs, V, Ts, Next, Fail, Ctxt, Env, Body, S);
switch_cases([], _V, _Ts, _Next, _Fail, _Ctxt, _Env, _Body, S) ->
    S.

%% The exact behaviour if all clauses fail is undefined; we generate
%% code to keep the executing program in an infinite loop if this
%% happens, which is safe and will not destroy information for later
%% analyses. (We do not want to throw an arbitrary exception, and
%% continuing execution after the `case', as in a C `switch' statement,
%% would add a new possible path to the program, which could destroy
%% program properties.) Recall that the `final' and `effect' context
%% flags distribute over the clause bodies.

clauses(Cs, Ts, Vs, Ctxt, Env, S) ->
    Next = new_continuation_label(Ctxt),
    S1 = clauses_1(Cs, Ts, Vs, undefined, Next, Ctxt, Env, S),
    add_continuation_label(Next, Ctxt, S1).

clauses_1([C | Cs], Ts, Vs, Fail, Next, Ctxt, Env, S) ->
    case cerl_clauses:is_catchall(C) of
	true ->
	    %% The fail label will not actually be used in this case.
	    clause(C, Ts, Vs, Fail, Next, Ctxt, Env, S);
	false ->
	    %% The previous `Fail' is not used here.
	    Fail1 = new_label(),
	    S1 = clause(C, Ts, Vs, Fail1, Next, Ctxt, Env, S),
	    S2 = add_code([icode_label(Fail1)], S1),
	    clauses_1(Cs, Ts, Vs, Fail1, Next, Ctxt, Env, S2)
    end;
clauses_1([], _Ts, _Vs, Fail, _Next, _Ctxt, _Env, S) ->
    if Fail == undefined ->
	    L = new_label(),
	    add_infinite_loop(L, S);
       true ->
	    add_code([icode_goto(Fail)], S)    % use existing label
    end.

add_infinite_loop(L, S) ->
    add_code([icode_label(L), icode_goto(L)], S).

clause(C, Ts, Vs, Fail, Next, Ctxt, Env, S) ->
    G = cerl:clause_guard(C),
    case cerl_clauses:eval_guard(G) of
	{value, true} ->
	    {Env1, S1} = patterns(cerl:clause_pats(C), Vs, Fail, Env,
				  S),
	    clause_body(C, Ts, Next, Ctxt, Env1, S1);
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
	    clause_body(C, Ts, Next, Ctxt, Env1, S3)
    end.

clause_body(C, Ts, Next, Ctxt, Env, S) ->
    S1 = expr(cerl:clause_body(C), Ts, Ctxt, Env, S),
    add_continuation_jump(Next, Ctxt, S1).

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
	    add_code([make_type(V, ?TYPE_ATOM(X), Next, Fail)],
		     S);
	X when integer(X) ->
	    add_code([make_type(V, ?TYPE_INTEGER(X), Next, Fail)],
		     S);
	X when float(X) ->
	    V1 = make_var(),
	    L = new_label(),
	    %% First doing an "is float" test here might allow later
	    %% stages to use a specialized equality test.
	    add_code([make_type(V, ?TYPE_IS_FLOAT, L, Fail),
		      icode_label(L),
		      icode_mov(V1, icode_const(X)),
		      make_if(?TEST_EQ, [V, V1], Next, Fail)],
		     S);
	[] ->
	    add_code([make_type(V, ?TYPE_NIL, Next, Fail)], S);
	X ->
	    %% Compound constants are compared with the generic exact
	    %% equality test.
	    V1 = make_var(),
	    add_code([icode_mov(V1, icode_const(X)),
		      make_if(?TEST_EQ, [V, V1], Next, Fail)],
		     S)
    end.

cons_pattern(P, V, Fail, Env, S) ->
    V1 = make_var(),
    V2 = make_var(),
    Next = new_label(),
    S1 = add_code([make_type(V, ?TYPE_CONS, Next, Fail),
		   icode_label(Next)]
		  ++ make_op(?OP_UNSAFE_HD, [V1], [V], #ctxt{})
		  ++ make_op(?OP_UNSAFE_TL, [V2], [V], #ctxt{}),
		  S),
    patterns([cerl:cons_hd(P), cerl:cons_tl(P)], [V1, V2],
	     Fail, Env, S1).

tuple_pattern(P, V, Fail, Env, S) ->
    Es = cerl:tuple_es(P),
    N = length(Es),
    Vs = make_vars(N),
    Next = new_label(),
    S1 = add_code([make_type(V, ?TYPE_TUPLE(N), Next, Fail),
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
    Segs = cerl:binary_segs(P),
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
    Size = cerl:bin_seg_size(P),
    Unit = cerl:bin_seg_unit(P),
    Type = cerl:concrete(cerl:bin_seg_type(P)),
    LiteralFlags = cerl:bin_seg_flags(P),
    T = cerl:bin_seg_val(P), 
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

%% This generates code for a boolean expression (such as "primop
%% 'and'(X, Y)") in a normal expression context, when an actual `true'
%% or `false' value is to be computed.

boolean_expr(E, [V] = Ts, Ctxt, Env, S) ->
    {Code, True, False} = make_bool_glue(V, true, false),
    Ctxt1 = Ctxt#ctxt{final = false},
    S1 = boolean(E, True, False, Ctxt1, Env, S),
    maybe_return(Ts, Ctxt, add_code(Code, S1));
boolean_expr(_, [], _Ctxt, _Env, _S) ->
    error_high_degree(),
    throw(error);
boolean_expr(_, _, _Ctxt, _Env, _S) ->
    error_low_degree(),
    throw(error).

%% This generates jumping code for booleans, but does not generally use
%% shortcuts for logical operators, unless the expression is "safe"
%% (i.e., has no side effects and cannot fail), or we are in guard
%% context; otherwise, we set a flag to be checked if necessary after
%% both branches have been evaluated. Note that since subexpressions are
%% checked repeatedly to see if they are safe, etc., this is really
%% quadratic, but I don't expect booleans to be very deeply nested.

%% TODO: use a pre-pass instead to annotate expressions as safe.

boolean(E, True, False, Ctxt, Env, S) ->
    case Ctxt#ctxt.class of
	guard ->
	    pure_boolean(E, True, False, Ctxt, Env, S);
	_ ->
	    case is_safe_expr(E) of
		true ->
		    safe_boolean(E, True, False, Ctxt, Env, S);
		false ->
		    generic_boolean(E, True, False, Ctxt, Env, S)
	    end
    end.

%% Note that 'and' and 'or' are strict! Unless we know more about their
%% subexpressions, we must evaluate both branches.

generic_boolean(E0, True, False, Ctxt, Env, S) ->
    E = reduce_expr(E0),
    case cerl:type(E) of
	primop ->
	    Name = cerl:atom_val(cerl:primop_name(E)),
	    As = cerl:primop_args(E),
	    Arity = length(As),
	    case {Name, Arity} of
		{?PRIMOP_NOT, 1} ->
		    [A] = As,
		    boolean(A, False, True, Ctxt, Env, S);
		{?PRIMOP_AND, 2} ->
		    [A, B] = As,
		    V = make_var(),
		    {Glue, True1, False1} =
			make_bool_glue(V, ?BOOL_TRUE, ?BOOL_FALSE),
		    S1 = boolean(A, True1, False1, Ctxt, Env, S),
		    S2 = add_code(Glue, S1),
		    Test = new_label(),
		    S3 = boolean(B, Test, False, Ctxt, Env, S2),
		    add_code([icode_label(Test),
			      make_type(V, ?BOOL_IS_FALSE,
					False, True)],
			     S3);
		{?PRIMOP_OR, 2} ->
		    [A, B] = As,
		    V = make_var(),
		    {Glue, True1, False1} =
			make_bool_glue(V, ?BOOL_TRUE, ?BOOL_FALSE),
		    S1 = boolean(A, True1, False1, Ctxt, Env, S),
		    S2 = add_code(Glue, S1),
		    Test = new_label(),
		    S3 = boolean(B, True, Test, Ctxt, Env, S2),
		    add_code([icode_label(Test),
			      make_type(V, ?BOOL_IS_FALSE,
					False, True)],
			     S3);
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
				    other_boolean(E, True, False, Ctxt,
						  Env, S)
			    end
		    end
	    end;
	literal ->
	    case cerl:concrete(E) of
		true ->
		    add_code([icode_goto(True)], S);
		false ->
		    add_code([icode_goto(False)], S);
		X ->
		    error_msg("not a boolean value: ~P.", [X, 5]),
		    throw(error)
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
	seq ->
	    %% Cf. 'expr_seq(...)'.
	    Ctxt1 = Ctxt#ctxt{effect = true, final = false},
	    S1 = expr(cerl:seq_arg(E), [], Ctxt1, Env, S),
	    boolean(cerl:seq_body(E), True, False, Ctxt, Env, S1);
	'let' ->
	    %% Note that we have called 'reduce_expr' above.
	    Vars = cerl:let_vars(E),
	    Vs = make_vars(length(Vars)),
	    S1 = expr(cerl:let_arg(E), Vs,
		      Ctxt#ctxt{effect = false, final = false}, Env, S),
	    Env1 = bind_vars(Vars, Vs, Env),
	    B = cerl:let_body(E),
	    boolean(B, True, False, Ctxt, Env1, S1);
	_ ->
	    %% This handles everything else, including cases that are
	    %% known to not return a boolean.
	    other_boolean(E, True, False, Ctxt, Env, S)
    end.

%% This is for when we expect a boolean result, but are not sure what
%% the expression will produce, or we know that the result is not a
%% boolean and we just want the error handling.

other_boolean(E, True, False, Ctxt, Env, S) ->
    V = make_var(),
    S1 = expr(E, [V], Ctxt#ctxt{final = false}, Env, S),
    L1 = new_label(),
    S2 = add_code([make_type(V, ?TYPE_ATOM(true), True, L1),
		   icode_label(L1)],
		  S1),
    case Ctxt#ctxt.class of
	guard ->
	    add_code([make_type(V, ?TYPE_ATOM(false), False,
				Ctxt#ctxt.fail)],
		     S2);
	_ ->
	    L2 = new_label(),
	    S3 = add_code([make_type(V, ?TYPE_ATOM(false), False, L2),
			   icode_label(L2)], S2),
	    add_exit(icode_const(error), Ctxt, S3)
    end.

%% This generates jumping code for boolean expressions where no
%% subexpression can have side effects *and* no subexpression can fail.

safe_boolean(E0, True, False, Ctxt, Env, S) ->
    E = reduce_expr(E0),
    case cerl:type(E) of
	primop ->
	    Name = cerl:atom_val(cerl:primop_name(E)),
	    As = cerl:primop_args(E),
	    Arity = length(As),
	    case {Name, Arity} of
		{?PRIMOP_AND, 2} ->
		    [A, B] = As,
		    Next = new_label(),
		    S1 = safe_boolean(A, Next, False, Ctxt, Env,
				      S),
		    S2 = add_code([icode_label(Next)], S1),
		    safe_boolean(B, True, False, Ctxt, Env, S2);
		{?PRIMOP_OR, 2} ->
		    [A, B] = As,
		    Next = new_label(),
		    S1 = safe_boolean(A, True, Next, Ctxt, Env,
				      S),
		    S2 = add_code([icode_label(Next)], S1),
		    safe_boolean(B, True, False, Ctxt, Env, S2);
		_ ->
		    generic_boolean(E, True, False, Ctxt, Env, S)
	    end;
 	seq ->
	    safe_boolean(cerl:seq_body(E), True, False, Ctxt, Env, S);
 	'try' ->
	    safe_boolean(cerl:try_arg(E), True, False, Ctxt, Env, S);
 	'catch' ->
	    safe_boolean(cerl:catch_body(E), True, False, Ctxt, Env, S);
	_ ->
	    generic_boolean(E, True, False, Ctxt, Env, S)
    end.

%% This generates jumping code for boolean expressions where no
%% subexpression can have side effects (as e.g. in a clause guard),
%% *but* subexpressions might fail.

pure_boolean(E, True, False, Ctxt, Env, S) ->
    case is_safe_expr(E) of
	true ->
	    safe_boolean(E, True, False, Ctxt, Env, S);
	false ->
	    unsafe_pure_boolean(E, True, False, Ctxt, Env, S)
    end.

unsafe_pure_boolean(E0, True, False, Ctxt, Env, S) ->
    E = reduce_expr(E0),
    case cerl:type(E) of
	primop ->
	    Name = cerl:atom_val(cerl:primop_name(E)),
	    As = cerl:primop_args(E),
	    Arity = length(As),
	    case {Name, Arity} of
		{?PRIMOP_AND, 2} ->
		    %% Done as in the "safe" case:
		    [A, B] = As,
		    Next = new_label(),
		    S1 = pure_boolean(A, Next, False, Ctxt, Env,
				      S),
		    S2 = add_code([icode_label(Next)], S1),
		    pure_boolean(B, True, False, Ctxt, Env, S2);
		{?PRIMOP_OR, 2} ->
		    %% This is done as in the generic case: since the
		    %% second argument might crash, it must be evaluated
		    %% even if the first yields `true'.
		    [A, B] = As,
		    V = make_var(),
		    {Glue, True1, False1} =
			make_bool_glue(V, ?BOOL_TRUE, ?BOOL_FALSE),
		    S1 = pure_boolean(A, True1, False1, Ctxt, Env,
				      S),
		    S2 = add_code(Glue, S1),
		    Test = new_label(),
		    S3 = pure_boolean(B, True, Test, Ctxt, Env,
				      S2),
		    add_code([icode_label(Test),
			      make_type(V, ?BOOL_IS_FALSE,
					False, True)],
			     S3);
		_ ->
		    generic_boolean(E, True, False, Ctxt, Env, S)
	    end;
 	'try' ->
	    case Ctxt#ctxt.class of
		guard ->
		    %% This should be a "protected" guard expression on
		    %% the form "try E of X -> X catch <T,R> -> 'false'"
		    %% but we don't actually check for this.
		    Ctxt1 = Ctxt#ctxt{fail = False, class = guard},
		    boolean(cerl:try_arg(E), True, False, Ctxt1, Env, S);
		_ ->
		    generic_boolean(E, True, False, Ctxt, Env, S)
	    end;
	_ ->
	    generic_boolean(E, True, False, Ctxt, Env, S)
    end.

%% Primitive comparison operations are inline expanded as conditional
%% branches when part of a boolean expression, rather than made into
%% primop or guardop calls. Without type information, however, we cannot
%% reduce comparisons `Expr == true' to simply `Expr' (and `Expr ==
%% false' to `not Expr'), because we are not sure that Expr will yield a
%% boolean - and if it does not, the result should be `false', not a
%% crash!

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

type_test(Name, [A], True, False, Ctxt, Env, S) ->
    V = make_var(),
    S1 = expr(A, [V], Ctxt, Env, S),
    Test = type_test(Name),
    add_code([make_type(V, Test, True, False)], S1).

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
is_type_test(_, _) -> false.

bind_var(V, X, Env) ->
    env__bind(cerl:var_name(V), #var{id = X}, Env).

bind_vars([V | Vs], [X | Xs], Env) ->
    bind_vars(Vs, Xs, bind_var(V, X, Env));
bind_vars([], [], Env) ->
    Env.

add_code(Code, S) ->
    s__add_code(Code, S).

%% This inserts code when necessary for assigning the targets in the
%% first list to those in the second.

glue([V1 | Vs1], [V2 | Vs2], S) ->
    if V1 =:= V2 ->
	    S;
       true ->
	    glue(Vs1, Vs2, add_code([icode_mov(V2, V1)], S))
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
    [icode_mov(V1, V2) | make_moves(Vs1, Vs2)];
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
	true ->
	    undefined;
	false ->
	    new_label()
    end.

add_continuation_label(Label, Ctxt, S) ->
    case Ctxt#ctxt.final of
	true ->
	    S;
	false ->
	    add_code([icode_label(Label)], S)
    end.

add_continuation_jump(Label, Ctxt, S) ->
    case Ctxt#ctxt.final of
	true ->
	    S;
	false ->
	    add_code([icode_goto(Label)], S)
    end.

add_exit(V, Ctxt, S) ->
    add_fail([V], exit, Ctxt, S).

add_throw(V, Ctxt, S) ->
    add_fail([V], throw, Ctxt, S).

add_error(V, Ctxt, S) ->
    add_fail([V], fault, Ctxt, S).

add_error(V, F, Ctxt, S) ->
    add_fail([V, F], fault2, Ctxt, S).

%% Failing is special, because it can "suddenly" end the basic block,
%% even though the context was expecting the code to fall through, for
%% instance when you have a call to 'exit(X)' that is not in a last call
%% context. In those cases a dummy label must therefore be added after
%% the fail instruction, to start a new (but unreachable) basic block.

add_fail(Vs, Type, Ctxt, S0) ->
    S1 = add_code([icode_fail(Vs, Type)], S0),
    add_continuation_label(new_continuation_label(Ctxt), Ctxt, S1).

%% We must add a fail-label if we are in a guard context.
%% Note that primops do not in general have an 'enter' form.

make_op(Name, Ts, As, Ctxt) ->
    case Ctxt#ctxt.class of
	guard ->
	    Next = new_label(),
	    [icode_guardop(Ts, Name, As, Next, Ctxt#ctxt.fail),
	     icode_label(Next)];
	_ ->
	    [icode_call_primop(Ts, Name, As)]
    end.

%% Recognize useless tests that always go to the same label. This often
%% happens as an artefact of the translation.

make_if(_, _, Label, Label) ->
    icode_goto(Label);
make_if(Test, As, True, False) ->
    icode_if(Test, As, True, False).

make_type(_, _, Label, Label) ->
    icode_goto(Label);
make_type(V, Test, True, False) ->
    icode_type(V, Test, True, False).

%% Creating glue code with true/false target labels for assigning a
%% corresponding 'true'/'false' value to a specific variable.

make_bool_glue(V, T, F) ->
    False = new_label(),
    True = new_label(),
    Next = new_label(),
    Code = [icode_label(False), 
	    icode_mov(V, icode_const(F)),
	    icode_goto(Next),
	    icode_label(True),
	    icode_mov(V, icode_const(T)),
	    icode_label(Next)],
    {Code, True, False}.

add_local_call({Name, _Arity} = Id, Vs, Ts, Ctxt, S, DstType) ->
    Module = s__get_module(S),
    case Ctxt#ctxt.final of
	false ->
	    add_code([icode_call_local(Ts, Module, Name, Vs, DstType)],S);
	true ->
	    Self = s__get_function(S),
	    if Id =:= Self ->
		    %% Tail-recursive call to same function.
		    {Label, Vs1} = s__get_local_entry(S),
		    add_code(make_moves(Vs1, Vs) ++
			     [icode_goto(Label)],
			     S);
	       true ->
		    add_code([icode_enter_local(Module, Name, Vs)], S)
	    end
    end.

%% In order to expose otherwise hidden cases of final expressions
%% (enabling last call optimization), and other cases when we want to
%% examine the type of some subexpression, we first try to remove all
%% trivial let-bindings (`let X = Y in X', `let X = Y in Y', `let X = Y
%% in let ... in ...', `let X = let ... in ... in ...', etc.), We do
%% not, however, try to recognize any other similar cases, even for
%% simple `case'-expressions like `case E of X -> X end'. Nor do we try
%% to optimize uses of multiple-value aggregates of other degree than 1.

reduce_expr(E) ->
    case cerl:type(E) of
	values ->
	    case cerl:values_es(E) of
		[E1] ->
		    reduce_expr(E1);
		_ ->
		    E
	    end;
	'let' ->
	    %% We give up if the body does not reduce to a single
	    %% variable. This is not a generic copy propagation.
	    B = reduce_expr(cerl:let_body(E)),
	    Vs = cerl:let_vars(E),
	    case cerl:is_c_var(B) of
		true when length(Vs) == 1 ->
		    %% We have `let <V1> = <E> in <V2>':
		    A = cerl:let_arg(E),
		    [V] = Vs,
		    case cerl:var_name(V) =:= cerl:var_name(B) of
			true ->
			    %% `let X = <E> in X' equals `<E>'
			    reduce_expr(A);
			false ->
			    %% `let X = <E> in Y' is equivalent to `Y'
			    %% if and only if `<E>' is "safe"; otherwise
			    %% it is eqivalent to `do <E> Y'.
			    case is_safe_expr(A) of
				true ->
				    B;
				false ->
				    cerl:update_c_seq(E, A, B)
			    end
		    end;
		_ ->
		    cerl:update_c_let(E, Vs, cerl:let_arg(E), B)
	    end;
	_ ->
	    E
    end.

%% Return `true' if `Node' represents a "safe" Core Erlang expression,
%% otherwise `false'. An expression is safe if it always completes
%% normally and does not modify the state (although the value it yields
%% might depend on the state). Expressions of type `apply', `call',
%% `case' and `receive' are always considered unsafe by this function.
%% For `try' (and `catch'), it would really suffice to check if the
%% guarded expression is a pure (albeit possibly unsafe) function to
%% deduce whether it is safe, but then we'd have to write a separate
%% function for that, so we don't bother to.

is_safe_expr(E) ->
    case cerl:type(E) of
	var ->
	    true;
	literal ->
	    true;
	'fun' ->
	    true;
	values ->
	    is_safe_expr_list(cerl:values_es(E));
	tuple ->
	    is_safe_expr_list(cerl:tuple_es(E));
	cons ->
	    case is_safe_expr(cerl:cons_hd(E)) of
		true ->
		    is_safe_expr(cerl:cons_tl(E));
		false ->
		    false
	    end;
	'let' ->
	    case is_safe_expr(cerl:let_arg(E)) of
		true ->
		    is_safe_expr(cerl:let_body(E));
		false ->
		    false
	    end;
	seq ->
	    case is_safe_expr(cerl:seq_arg(E)) of
		true ->
		    is_safe_expr(cerl:seq_body(E));
		false ->
		    false
	    end;
	'try' ->
	    %% If the guarded expression is safe, the try-handler
	    %% will never be evaluated, so we need not check it.
	    case is_safe_expr(cerl:try_arg(E)) of
		true ->
		    is_safe_expr(cerl:try_body(E));
		false ->
		    false
	    end;
	'catch' ->
	    is_safe_expr(cerl:catch_body(E));
	letrec ->
	    is_safe_expr(cerl:letrec_body(E));
	primop ->
	    Name = cerl:atom_val(cerl:primop_name(E)),
	    As = cerl:primop_args(E),
	    case is_safe_op(Name, length(As)) of
		true ->
		    is_safe_expr_list(As);
		false ->
		    false
	    end;
	_ ->
	    false
    end.

is_safe_expr_list([E | Es]) ->
    case is_safe_expr(E) of
	true ->
	    is_safe_expr_list(Es);
	false ->
	    false
    end;
is_safe_expr_list([]) ->
    true.

%% There are very few really safe operations (sigh!). If we have type
%% information, several operations could be rewritten into specialized
%% safe versions, such as '+'/2 -> add_integer/2 (which is safe).

is_safe_op(?PRIMOP_IDENTITY, 1) -> true;
is_safe_op(N, A) ->
    case is_comp_op(N, A) of
	true -> true;
	false ->
	    is_type_test(N, A)
    end.

translate_flags([A|Rest],Align) ->
    case cerl:concrete(A) of
	signed ->
	    4 + translate_flags(Rest, Align);
	little ->
	    2 + translate_flags(Rest, Align);
	_ ->
	    translate_flags(Rest, Align)
    end;
translate_flags([], Align) ->
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

%% env__new_integer_keys(N, Env) ->
%%     rec_env:new_keys(N, Env).


%% ---------------------------------------------------------------------
%% State (abstract datatype)

-record(state, {module, function, local, code = []}).

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
    S#state{code = push_list(Code, S#state.code)}.


%% ---------------------------------------------------------------------
%% General utilities

push_list([A | As], Bs) ->
    push_list(As, [A | Bs]);
push_list([], Bs) ->
    Bs.

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

get_type(List)->
    Type = get_type0(List, []),
    %%io:format("cerl_to_icode:get_type/1: Types: ~w\n", [Type]),
    Type.

get_type0([H|T], Acc)->
    %%io:format("cerl_to_icode:get_type/1: Annotations: ~w\n", [cerl:get_ann(H)]),
    case lists:keysearch(type, 1, cerl:get_ann(H)) of
	{value, {type, Type}} -> get_type0(T, [Type|Acc]);
	false -> get_type0(T, [erl_types:t_any()|Acc])
    end;
get_type0([], Acc) ->
    lists:reverse(Acc);
get_type0(E, Acc) ->
    get_type0([E], Acc).


%% ---------------------------------------------------------------------
%% ICode interface

icode_icode(F, Vs, Lambda, Leaf, C, V, L, T) ->
    hipe_icode:mk_typed_icode(F, Vs, Lambda, Leaf, C, V, L, T).

icode_var(V) -> hipe_icode:mk_var(V).

icode_label(L) -> hipe_icode:mk_label(L).

icode_mov(V, D) -> hipe_icode:mk_mov(V, D).

icode_const(X) -> hipe_icode:mk_const(X).

icode_const_val(X) -> hipe_icode:const_value(X).

icode_call_local(Ts, M, N, Vs, DstType) ->
    hipe_icode:mk_typed_call(Ts, M, N, Vs, local, DstType).

icode_call_remote(Ts, M, N, Vs) ->
    hipe_icode:mk_call(Ts, M, N, Vs, remote).

icode_enter_local(M, N, Vs) ->
    hipe_icode:mk_enter(M, N, Vs, local).

icode_enter_remote(M, N, Vs) ->
    hipe_icode:mk_enter(M, N, Vs, remote).

icode_call_fun(Ts, Vs) ->
    icode_call_primop(Ts, call_fun, Vs).

icode_enter_fun(Vs) ->
    icode_enter_primop(enter_fun, Vs).

icode_pushcatch(L,Cont) -> hipe_icode:mk_pushcatch(L,Cont).

icode_remove_catch(L) -> hipe_icode:mk_remove_catch(L).

icode_restore_catch(T, L) -> hipe_icode:mk_restore_catch(T, L).

icode_goto(L) -> hipe_icode:mk_goto(L).

icode_return(Ts) -> hipe_icode:mk_return(Ts).

icode_fail(V, T) -> hipe_icode:mk_fail(V, T).

icode_guardop(Ts, Name, As, Succ, Fail) ->
    hipe_icode:mk_guardop(Ts, Name, As, Succ, Fail).

icode_call_primop(Ts, Name, As) -> hipe_icode:mk_primop(Ts, Name, As).

%% enter_primop can only be used for the 'enter_fun' op.
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

warning_not_in_receive(Name) ->
    warning_msg("primitive operation `~w' has no effect "
		"outside receive-clauses.", [Name]).

low_degree() ->
    "degree of expression less than expected.".

warning_low_degree() ->
    warning_msg(low_degree()).

error_low_degree() ->
    error_msg(low_degree()).

error_high_degree() ->
    error_msg("degree of expression greater than expected.").

error_degree_mismatch(N, E) ->
    error_msg("expression does not have expected degree "
	      "(~w): ~P.",
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
