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
%% @doc HiPE-ification of Core Erlang code. Prepares Core Erlang code
%% for translation to ICode.
%% @see cerl_to_icode

-module(cerl_hipeify).

-export([module/1]).

-include("cerl_hipe_primops.hrl").

-record(ctxt, {class = expr}).


%% @spec module(Module::cerl()) -> cerl()
%%
%%    cerl() = cerl:cerl()
%%
%% @doc Rewrites a Core Erlang module to a form suitable for further
%% translation to HiPE Icode. See module <code>cerl_to_icode</code> for
%% details.
%%
%% @see cerl_to_icode

module(T) ->
    {Ds, Env, Ren} = add_defs(cerl:module_defs(T), env__new(),
			      ren__new()),
    M = cerl:module_name(T),
    {Ds1, _} = defs(Ds, true, Env, Ren, s__new(cerl:atom_val(M))),
    cerl:update_c_module(T, M, cerl:module_exports(T),
			 cerl:module_attrs(T), Ds1).

%% Note that the environment is defined on the renamed variables.

expr(T, Env, Ren, Ctxt, S0) ->
    case cerl:type(T) of
 	literal ->
	    {T, S0};
	var ->
	    variable(T, Env, Ren, Ctxt, S0);
	values ->
	    {Ts, S1} = expr_list(cerl:values_es(T), Env, Ren, Ctxt, S0),
 	    {cerl:update_c_values(T, Ts), S1};
	cons ->
	    {T1, S1} = expr(cerl:cons_hd(T), Env, Ren, Ctxt, S0),
	    {T2, S2} = expr(cerl:cons_tl(T), Env, Ren, Ctxt, S1),
	    {cerl:update_c_cons(T, T1, T2), S2};
 	tuple ->
	    {Ts, S1} = expr_list(cerl:tuple_es(T), Env, Ren, Ctxt, S0),
	    {cerl:update_c_tuple(T, Ts), S1};
 	'let' ->
	    let_expr(T, Env, Ren, Ctxt, S0);
	seq ->
	    {A, S1} = expr(cerl:seq_arg(T), Env, Ren, Ctxt, S0),
	    {B, S2} = expr(cerl:seq_body(T), Env, Ren, Ctxt, S1),
 	    {cerl:update_c_seq(T, A, B), S2};
 	apply ->
	    {Op, S1} = expr(cerl:apply_op(T), Env, Ren, Ctxt, S0),
	    {As, S2} = expr_list(cerl:apply_args(T), Env, Ren, Ctxt, S1),
 	    {cerl:update_c_apply(T, Op, As), S2};
 	call ->
	    {M, S1} = expr(cerl:call_module(T), Env, Ren, Ctxt, S0),
	    {N, S2} = expr(cerl:call_name(T), Env, Ren, Ctxt, S1),
	    {As, S3} = expr_list(cerl:call_args(T), Env, Ren, Ctxt, S2),
 	    {rewrite_call(T, M, N, As), S3};
 	primop ->
	    {As, S1} = expr_list(cerl:primop_args(T), Env, Ren, Ctxt, S0),
	    N = cerl:primop_name(T),
	    {rewrite_primop(T, N, As, S1), S1};
 	'case' ->
	    {A, S1} = expr(cerl:case_arg(T), Env, Ren, Ctxt, S0),
	    {E, Vs, S2} = clauses(cerl:case_clauses(T), Env, Ren, Ctxt, S1),
 	    {cerl:c_let(Vs, A, E), S2};
 	'fun' ->
	    Vs = cerl:fun_vars(T),
	    {Vs1, Env1, Ren1} = add_vars(Vs, Env, Ren),
	    {B, S1} = expr(cerl:fun_body(T), Env1, Ren1, Ctxt, S0),
	    {cerl:update_c_fun(T, Vs1, B), S1};
 	'receive' ->
	    receive_expr(T, Env, Ren, Ctxt, S0);
 	'try' ->
	    {E, S1} = expr(cerl:try_arg(T), Env, Ren, Ctxt, S0),
	    Vs = cerl:try_vars(T),
	    {Vs1, Env1, Ren1} = add_vars(Vs, Env, Ren),
	    {B, S2} = expr(cerl:try_body(T), Env1, Ren1, Ctxt, S1),
	    Evs = cerl:try_evars(T),
	    {Evs1, Env2, Ren2} = add_vars(Evs, Env, Ren),
	    {H, S3} = expr(cerl:try_handler(T), Env2, Ren2, Ctxt, S2),
	    {cerl:update_c_try(T, E, Vs1, B, Evs1, H), S3};
 	'catch' ->
	    {B, S1} = expr(cerl:catch_body(T), Env, Ren, Ctxt, S0),
	    {cerl:update_c_catch(T, B), S1};
	letrec ->
	    {Ds, Env1, Ren1} = add_defs(cerl:letrec_defs(T), Env, Ren),
	    {Ds1, S1} = defs(Ds, false, Env1, Ren1, S0),
	    {B, S2} = expr(cerl:letrec_body(T), Env1, Ren1, Ctxt, S1),
	    {cerl:update_c_letrec(T, Ds1, B), S2};
	binary ->
	    {Segs, S1}=expr_list(cerl:binary_segs(T), Env, Ren, Ctxt, S0),
	    {cerl:update_c_binary(T, Segs), S1};
	bin_seg ->
	    {T1,S1} = expr(cerl:bin_seg_val(T), Env, Ren, Ctxt, S0),
	    {T2,S2} = expr(cerl:bin_seg_size(T), Env, Ren, Ctxt, S1),	
	    T3 = cerl:bin_seg_unit(T), 
	    T4 = cerl:bin_seg_type(T),
	    T5 = cerl:bin_seg_flags(T),
	    {cerl:update_c_bin_seg(T, T1, T2, T3, T4, T5), S2} 
    end.

guard_expr(T, Env, Ren, Ctxt, S) ->
    expr(T, Env, Ren, Ctxt#ctxt{class = guard}, S).

expr_list(Ts, Env, Ren, Ctxt, S0) ->
    list(Ts, Env, Ren, Ctxt, S0, fun expr/5).

list([T | Ts], Env, Ren, Ctxt, S0, F) ->
    {T1, S1} = F(T, Env, Ren, Ctxt, S0),
    {Ts1, S2} = list(Ts, Env, Ren, Ctxt, S1, F),
    {[T1 | Ts1], S2};
list([], _, _, _, S, _) ->
    {[], S}.

pattern(T, Env, Ren) ->
    case cerl:type(T) of
 	literal ->
	    T;
	var ->
	    cerl:update_c_var(T, ren__map(cerl:var_name(T), Ren));
	values ->
	    Ts = pattern_list(cerl:values_es(T), Env, Ren),
 	    cerl:update_c_values(T, Ts);
	cons ->
	    T1 = pattern(cerl:cons_hd(T), Env, Ren),
	    T2 = pattern(cerl:cons_tl(T), Env, Ren),
	    cerl:update_c_cons(T, T1, T2);
 	tuple ->
	    Ts = pattern_list(cerl:tuple_es(T), Env, Ren),
	    cerl:update_c_tuple(T, Ts);
	alias ->
	    V = pattern(cerl:alias_var(T), Env, Ren),
	    P = pattern(cerl:alias_pat(T), Env, Ren),
	    cerl:update_c_alias(T, V, P);
	binary ->
	    Segs=pattern_list(cerl:binary_segs(T), Env, Ren),
	    cerl:update_c_binary(T, Segs);
	bin_seg ->
	    T1 = pattern(cerl:bin_seg_val(T), Env, Ren),
	    T2 = pattern(cerl:bin_seg_size(T), Env, Ren),	
	    T3 = cerl:bin_seg_unit(T), 
	    T4 = cerl:bin_seg_type(T),
	    T5 = cerl:bin_seg_flags(T),
	    cerl:update_c_bin_seg(T, T1, T2, T3, T4, T5)
    end.



pattern_list([T | Ts], Env, Ren) ->
    [pattern(T, Env, Ren) | pattern_list(Ts, Env, Ren)];
pattern_list([], _, _) ->
    [].

%% Visit the function body of each definition.

defs(Ds, Top, Env, Ren, S) ->
    defs(Ds, [], Top, Env, Ren, S).

defs([{V, F} | Ds], Ds1, Top, Env, Ren, S0) ->
    S1 = case Top of
	     true -> s__enter_function(cerl:var_name(V), S0);
	     false -> S0
	 end,
    {B, S2} = expr(cerl:fun_body(F), Env, Ren, #ctxt{}, S1),
    F1 = cerl:update_c_fun(F, cerl:fun_vars(F), B),
    defs(Ds, [{V, F1} | Ds1], Top, Env, Ren, S2);
defs([], Ds, _Top, _Env, _Ren, S) ->
    {lists:reverse(Ds), S}.

clauses(Cs, Env, Ren, Ctxt, S) ->
    {Cs1, S1} = clause_list(Cs, Env, Ren, Ctxt, S),
    %% Perform pattern matching compilation on the clauses.
    {E, Vs} = cerl_pmatch:clauses(Cs1, Env),
    %% We must make sure that we also visit any clause guards generated
    %% by the pattern matching compilation. We pass an empty renaming,
    %% so we do not rename any variables twice.
    {E1, S2} = revisit_expr(E, Env, ren__new(), Ctxt, S1),
    {E1, Vs, S2}.

clause_list(Cs, Env, Ren, Ctxt, S) ->
    list(Cs, Env, Ren, Ctxt, S, fun clause/5).

clause(T, Env, Ren, Ctxt, S0) ->
    Vs = cerl:clause_vars(T),
    {_, Env1, Ren1} = add_vars(Vs, Env, Ren),
    %% Visit patterns to rename variables.
    Ps = pattern_list(cerl:clause_pats(T), Env1, Ren1),
    {G, S1} = guard_expr(cerl:clause_guard(T), Env1, Ren1, Ctxt, S0),
    {B, S2} = expr(cerl:clause_body(T), Env1, Ren1, Ctxt, S1),
    {cerl:update_c_clause(T, Ps, G, B), S2}.

%% This does what 'expr' does, but only recurses into clause guard
%% expressions, 'case'-expressions, and the bodies of lets and letrecs.
%% Note that revisiting should not add further renamings, and we simply
%% ignore making any bindings at all at this level.

revisit_expr(T, Env, Ren, Ctxt, S0) ->
    case cerl:type(T) of
 	'case' ->
	    {Cs, S1} = revisit_clause_list(cerl:case_clauses(T), Env,
					   Ren, Ctxt, S0),
	    {cerl:update_c_case(T, cerl:case_arg(T), Cs), S1};
	'let' ->
	    {B, S1} = revisit_expr(cerl:let_body(T), Env, Ren, Ctxt, S0),
	    {cerl:update_c_let(T, cerl:let_vars(T), cerl:let_arg(T), B),
	     S1};
	'letrec' ->
	    {B, S1} = revisit_expr(cerl:letrec_body(T), Env, Ren, Ctxt, S0),
	    {cerl:update_c_letrec(T, cerl:letrec_defs(T), B), S1};
	_ ->
	    {T, S0}
    end.

revisit_clause_list(Cs, Env, Ren, Ctxt, S) ->
    list(Cs, Env, Ren, Ctxt, S, fun revisit_clause/5).

revisit_clause(T, Env, Ren, Ctxt, S0) ->
    %% Ignore the bindings.
    {G, S1} = guard_expr(cerl:clause_guard(T), Env, Ren, Ctxt, S0),
    {B, S2} = revisit_expr(cerl:clause_body(T), Env, Ren, Ctxt, S1),
    {cerl:update_c_clause(T, cerl:clause_pats(T), G, B), S2}.

%% We use the no-shadowing strategy, renaming variables on the fly and
%% only when necessary to uphold the invariant.

add_vars(Vs, Env, Ren) ->
    add_vars(Vs, [], Env, Ren).

add_vars([V | Vs], Vs1, Env, Ren) ->
    Name = cerl:var_name(V),
    {Name1, Ren1} = rename(Name, Env, Ren),
    add_vars(Vs, [cerl:update_c_var(V, Name1) | Vs1],
	     env__bind(Name1, variable, Env), Ren1);
add_vars([], Vs, Env, Ren) ->
    {lists:reverse(Vs), Env, Ren}.

rename(Name, Env, Ren) ->
    case env__is_defined(Name, Env) of
	false ->
	    {Name, Ren};
	true ->
	    New = env__new_name(Env),
	    {New, ren__add(Name, New, Ren)}
    end.

%% Setting up the environment for a list of letrec-bound definitions.

add_defs(Ds, Env, Ren) ->
    add_defs(Ds, [], Env, Ren).

add_defs([{V, F} | Ds], Ds1, Env, Ren) ->
    Name = cerl:var_name(V),
    {Name1, Ren1} =
	case env__is_defined(Name, Env) of
	    false ->
		{Name, Ren};
	    true ->
		{N, A} = Name,
		S = atom_to_list(N) ++ "_",
		F = fun (N) ->
			    {list_to_atom(S ++ integer_to_list(N)), A}
		    end,
		New = env__new_function_name(F, Env),
		{New, ren__add(Name, New, Ren)}
	end,
    add_defs(Ds, [{cerl:update_c_var(V, Name1), F} | Ds1],
	     env__bind(Name1, function, Env), Ren1);
add_defs([], Ds, Env, Ren) ->
    {lists:reverse(Ds), Env, Ren}.

%% We change remote calls to important built-in functions into primop
%% calls. In some cases (e.g., for the boolean operators), this is
%% mainly to allow the cerl_to_icode module to handle them more
%% straightforwardly. In most cases however, it is simply because they
%% are supposed to be represented as primop calls on the Icode level.

rewrite_call(T, M, F, As) ->
    case cerl:is_c_atom(M) and cerl:is_c_atom(F) of
	true ->
	    case call_to_primop(cerl:atom_val(M),
				cerl:atom_val(F),
				length(As))
		of
		{yes, N} ->
		    cerl:c_primop(cerl:c_atom(N), As);
		no ->
		    cerl:update_c_call(T, M, F, As)
	    end;
	false ->
	    cerl:update_c_call(T, M, F, As)
    end.

call_to_primop(erlang, 'not', 1) -> {yes, ?PRIMOP_NOT};
call_to_primop(erlang, 'and', 2) -> {yes, ?PRIMOP_AND};
call_to_primop(erlang, 'or', 2) -> {yes, ?PRIMOP_OR};
call_to_primop(erlang, '+', 2) -> {yes, ?PRIMOP_ADD};
call_to_primop(erlang, '+', 1) -> {yes, ?PRIMOP_IDENTITY};
call_to_primop(erlang, '-', 2) -> {yes, ?PRIMOP_SUB};
call_to_primop(erlang, '-', 1) -> {yes, ?PRIMOP_NEG};
%% call_to_primop(erlang, '*', 2) -> {yes, ?PRIMOP_MUL};
%% call_to_primop(erlang, '/', 2) -> {yes, ?PRIMOP_DIV};
%% call_to_primop(erlang, 'div', 2) -> {yes, ?PRIMOP_INTDIV};
%% call_to_primop(erlang, 'rem', 2) -> {yes, ?PRIMOP_REM};
call_to_primop(erlang, 'band', 2) -> {yes, ?PRIMOP_BAND};
call_to_primop(erlang, 'bor', 2) -> {yes, ?PRIMOP_BOR};
call_to_primop(erlang, 'bxor', 2) -> {yes, ?PRIMOP_BXOR};
call_to_primop(erlang, 'bnot', 1) -> {yes, ?PRIMOP_BNOT};
%% call_to_primop(erlang, 'bsl', 2) -> {yes, ?PRIMOP_BSL};
%% call_to_primop(erlang, 'bsr', 2) -> {yes, ?PRIMOP_BSR};
call_to_primop(erlang, '==', 2) -> {yes, ?PRIMOP_EQ};
call_to_primop(erlang, '/=', 2) -> {yes, ?PRIMOP_NE};
call_to_primop(erlang, '=:=', 2) -> {yes, ?PRIMOP_EXACT_EQ};
call_to_primop(erlang, '=/=', 2) -> {yes, ?PRIMOP_EXACT_NE};
call_to_primop(erlang, '<', 2) -> {yes, ?PRIMOP_LT};
call_to_primop(erlang, '>', 2) -> {yes, ?PRIMOP_GT};
call_to_primop(erlang, '=<', 2) -> {yes, ?PRIMOP_LE};
call_to_primop(erlang, '>=', 2) -> {yes, ?PRIMOP_GE};
call_to_primop(erlang, is_atom, 1) -> {yes, ?PRIMOP_IS_ATOM};
call_to_primop(erlang, is_binary, 1) -> {yes, ?PRIMOP_IS_BINARY};
call_to_primop(erlang, is_constant, 1) -> {yes, ?PRIMOP_IS_CONSTANT};
call_to_primop(erlang, is_float, 1) -> {yes, ?PRIMOP_IS_FLOAT};
call_to_primop(erlang, is_function, 1) -> {yes, ?PRIMOP_IS_FUNCTION};
call_to_primop(erlang, is_integer, 1) -> {yes, ?PRIMOP_IS_INTEGER};
call_to_primop(erlang, is_list, 1) -> {yes, ?PRIMOP_IS_LIST};
call_to_primop(erlang, is_number, 1) -> {yes, ?PRIMOP_IS_NUMBER};
call_to_primop(erlang, is_pid, 1) -> {yes, ?PRIMOP_IS_PID};
call_to_primop(erlang, is_port, 1) -> {yes, ?PRIMOP_IS_PORT};
call_to_primop(erlang, is_reference, 1) -> {yes, ?PRIMOP_IS_REFERENCE};
call_to_primop(erlang, is_tuple, 1) -> {yes, ?PRIMOP_IS_TUPLE};
call_to_primop(erlang, exit, 1) -> {yes, ?PRIMOP_EXIT};
call_to_primop(erlang, throw, 1) -> {yes, ?PRIMOP_THROW};
call_to_primop(erlang, error, 1) -> {yes, ?PRIMOP_ERROR};
call_to_primop(erlang, error, 2) -> {yes, ?PRIMOP_ERROR};
call_to_primop(erlang, fault, 1) -> {yes, ?PRIMOP_ERROR};
call_to_primop(erlang, fault, 2) -> {yes, ?PRIMOP_ERROR};
call_to_primop(_, _, _) -> no.

%% Also, some primops (introduced by Erlang to Core Erlang translation
%% and possibly other stages) must be recognized and rewritten.

rewrite_primop(T, N, As, S) ->
    case {cerl:atom_val(N), As} of
	{match_fail, [R]} ->
	    M = s__get_module_name(S),
	    {F, A} = s__get_function_name(S),
	    Stack = cerl:abstract([{M, F, A}]),
	    case cerl:type(R) of
		tuple ->
		    %% Function clause failures have a special encoding
		    %% as '{function_clause, Arg1, ..., ArgN}'.
		    case cerl:tuple_es(R) of
			[X | Xs] ->
			    case cerl:is_c_atom(X) of
				true ->
				    case cerl:atom_val(X) of
					function_clause ->
					    FStack = cerl:make_list(
						       [cerl:c_tuple(
							  [cerl:c_atom(M),
							   cerl:c_atom(F),
							   cerl:make_list(Xs)])]),
					    match_fail(T, X, FStack);
					_ ->
					    match_fail(T, R, Stack)
				    end;
				false ->
				    match_fail(T, R, Stack)
			    end;
			_ ->
			    match_fail(T, R, Stack)
		    end;
		_ ->
		    match_fail(T, R, Stack)
	    end;
	_ ->
	    cerl:update_c_primop(T, N, As)
    end.

match_fail(T, R, Stack) ->
    cerl:update_c_primop(T, cerl:c_atom(?PRIMOP_ERROR), [R, Stack]).

%% Simple let-definitions (of degree 1) in guard context are always
%% inline expanded. This is allowable, since they cannot have side
%% effects, and it makes it easy to generate good code for boolean
%% expressions. It could cause repeated evaluations, but typically,
%% local definitions within guards are used exactly once.

let_expr(T, Env, Ren, Ctxt, S) ->
    if Ctxt#ctxt.class == guard ->
	    case cerl:let_vars(T) of
		[V] ->
		    {Name, Ren1} = rename(cerl:var_name(V), Env, Ren),
		    Env1 = env__bind(Name, {expr, cerl:let_arg(T)}, Env),
		    expr(cerl:let_body(T), Env1, Ren1, Ctxt, S);
		_ ->
		    let_expr_1(T, Env, Ren, Ctxt, S)
	    end;
       true ->
	    let_expr_1(T, Env, Ren, Ctxt, S)
    end.

let_expr_1(T, Env, Ren, Ctxt, S0) ->
    {A, S1} = expr(cerl:let_arg(T), Env, Ren, Ctxt, S0),
    Vs = cerl:let_vars(T),
    {Vs1, Env1, Ren1} = add_vars(Vs, Env, Ren),
    {B, S2} = expr(cerl:let_body(T), Env1, Ren1, Ctxt, S1),
    {cerl:update_c_let(T, Vs1, A, B), S2}.

variable(T, Env, Ren, Ctxt, S) ->
    V = ren__map(cerl:var_name(T), Ren),
    if Ctxt#ctxt.class == guard ->
	    case env__lookup(V, Env) of
		{ok, {expr, E}} ->
		    expr(E, Env, Ren, Ctxt, S);   % inline
		_ ->
		    %% Since we don't track all bindings when we revisit
		    %% guards, some names will not be in the environment.
		    variable_1(T, V, S)
	    end;
       true ->
	    variable_1(T, V, S)
    end.

variable_1(T, V, S) ->
    {cerl:update_c_var(T, V), S}.

%% Receive-expressions are rewritten as follows:
%%
%%	receive
%%	    P1 when G1 -> B1
%%	      ...
%%	    Pn when Gn -> Bn
%%	after T -> A end
%% becomes:
%%	receive
%%	    M when 'true' ->
%%	      case M of
%%	        P1 when G1 -> do primop RECEIVE_SELECT B1
%%	          ...
%%	        Pn when Gn -> do primop RECEIVE_SELECT Bn
%%	        Pn+1 when 'true' -> primop RECEIVE_NEXT()
%%	    end
%%	after T -> A end

receive_expr(E, Env, Ren, Ctxt, S0) ->
    Cs = cerl:receive_clauses(E),
    {B, Vs, S1} = clauses(receive_clauses(Cs), Env, Ren, Ctxt, S0),
    {T, S2} = expr(cerl:receive_timeout(E), Env, Ren, Ctxt, S1),
    {A, S3} = expr(cerl:receive_action(E), Env, Ren, Ctxt, S2),
    Cs1 = [cerl:c_clause(Vs, B)],
    {cerl:update_c_receive(E, Cs1, T, A), S3}.

receive_clauses([C | Cs]) ->
    Call = cerl:c_primop(cerl:c_atom(?PRIMOP_RECEIVE_SELECT),
			 []),
    B = cerl:c_seq(Call, cerl:clause_body(C)),
    C1 =  cerl:update_c_clause(C, cerl:clause_pats(C),
			       cerl:clause_guard(C), B),
    [C1 | receive_clauses(Cs)];
receive_clauses([]) ->
    Call = cerl:c_primop(cerl:c_atom(?PRIMOP_RECEIVE_NEXT),
			 []),
    V = cerl:c_var('X'),    % any name is ok
    [cerl:c_clause([V], Call)].


%% ---------------------------------------------------------------------
%% Environment

env__new() ->
    rec_env:empty().

env__bind(Key, Value, Env) ->
    rec_env:bind(Key, Value, Env).

%% env__get(Key, Env) ->
%%     rec_env:get(Key, Env).

env__lookup(Key, Env) ->
    rec_env:lookup(Key, Env).

env__is_defined(Key, Env) ->
    rec_env:is_defined(Key, Env).

env__new_name(Env) ->
    rec_env:new_key(Env).

env__new_function_name(F, Env) ->
    rec_env:new_custom_key(F, Env).


%% ---------------------------------------------------------------------
%% Renaming

ren__new() ->
    dict:new().

ren__add(Key, Value, Ren) ->
    dict:store(Key, Value, Ren).

ren__map(Key, Ren) ->
    case dict:find(Key, Ren) of
	{ok, Value} ->
	    Value;
	error ->
	    Key
    end.


%% ---------------------------------------------------------------------
%% State

-record(state, {module, function}).

s__new(Module) ->
    #state{module = Module}.

s__get_module_name(S) ->
    S#state.module.

s__enter_function(F, S) ->
    S#state{function = F}.

s__get_function_name(S) ->
    S#state.function.
