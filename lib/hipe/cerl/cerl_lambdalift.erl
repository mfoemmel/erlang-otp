%% =====================================================================
%% @doc Lambda lifting of Core Erlang modules.
%% @end
%%
%% Copyright (C) 2000-2002 Richard Carlsson
%%
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
%% Author contact: richardc@csd.uu.se
%%
%% $Id$

-module(cerl_lambdalift).

-include("cerl_hipe_primops.hrl").

-export([module/1]).

%% @spec module(Module::cerl()) -> cerl()
%%
%%    cerl() = cerl:cerl()
%%
%% @doc Rewrites a Core Erlang module so that all fun-expressions
%% (lambdas) in the code are in top level function definitions, and the
%% operators of all <code>apply</code>-expressions are names of such
%% top-level functions. The primitive operations <code>make_fun</code>
%% and <code>call_fun</code> are inserted in the code to create and
%% apply functional values.
%%
%% <p>See module <code>cerl_to_icode</code> for details.</p>
%%
%% @see cerl_to_icode

module(T) ->
    M = cerl:module_name(T),
    S0 = s__new(cerl:atom_val(M)),
    {Defs1, S1} = module_defs(cerl:module_defs(T), env__new(),
			      ren__new(), S0),
    Defs2 = Defs1 ++ s__get_defs(S1),
    cerl:update_c_module(T, M, cerl:module_exports(T),
			 cerl:module_attrs(T), Defs2).

%% Note that the environment is defined on the renamed variables.

expr(T, Env, Ren, S0) ->
    case cerl:type(T) of
 	literal ->
	    {T, S0};
	var ->
	    var(T, Env, Ren, S0);
	values ->
	    {Ts, S1} = expr_list(cerl:values_es(T), Env, Ren, S0),
 	    {cerl:update_c_values(T, Ts), S1};
	cons ->
	    {T1, S1} = expr(cerl:cons_hd(T), Env, Ren, S0),
	    {T2, S2} = expr(cerl:cons_tl(T), Env, Ren, S1),
	    {cerl:update_c_cons(T, T1, T2), S2};
 	tuple ->
	    {Ts, S1} = expr_list(cerl:tuple_es(T), Env, Ren, S0),
	    {cerl:update_c_tuple(T, Ts), S1};
 	'let' ->
	    {A, S1} = expr(cerl:let_arg(T), Env, Ren, S0),
	    Vs = cerl:let_vars(T),
	    {Vs1, Env1, Ren1} = add_vars(Vs, Env, Ren),
	    {B, S2} = expr(cerl:let_body(T), Env1, Ren1, S1),
	    {cerl:update_c_let(T, Vs1, A, B), S2};
	seq ->
	    {A, S1} = expr(cerl:seq_arg(T), Env, Ren, S0),
	    {B, S2} = expr(cerl:seq_body(T), Env, Ren, S1),
 	    {cerl:update_c_seq(T, A, B), S2};
 	apply ->
	    apply_expr(T, Env, Ren, S0);
 	call ->
	    {M, S1} = expr(cerl:call_module(T), Env, Ren, S0),
	    {N, S2} = expr(cerl:call_name(T), Env, Ren, S1),
	    {As, S3} = expr_list(cerl:call_args(T), Env, Ren, S2),
 	    {cerl:update_c_call(T, M, N, As), S3};
 	primop ->
	    {As, S1} = expr_list(cerl:primop_args(T), Env, Ren, S0),
	    N = cerl:primop_name(T),
	    {cerl:update_c_primop(T, N, As), S1};
 	'case' ->
	    {A, S1} = expr(cerl:case_arg(T), Env, Ren, S0),
	    {Cs, S2} = expr_list(cerl:case_clauses(T), Env, Ren, S1),
 	    {cerl:update_c_case(T, A, Cs), S2};
 	clause ->
	    Vs = cerl:clause_vars(T),
	    {_, Env1, Ren1} = add_vars(Vs, Env, Ren),
	    %% Visit patterns to rename variables.
	    Ps = pattern_list(cerl:clause_pats(T), Env1, Ren1),
	    {G, S1} = expr(cerl:clause_guard(T), Env1, Ren1, S0),
	    {B, S2} = expr(cerl:clause_body(T), Env1, Ren1, S1),
	    {cerl:update_c_clause(T, Ps, G, B), S2};
 	'fun' ->
	    fun_expr(T, Env, Ren, S0);
 	'receive' ->
	    {Cs, S1} = expr_list(cerl:receive_clauses(T), Env, Ren, S0),
	    {E, S2} = expr(cerl:receive_timeout(T), Env, Ren, S1),
	    {A, S3} = expr(cerl:receive_action(T), Env, Ren, S2),
	    {cerl:update_c_receive(T, Cs, E, A), S3};
 	'try' ->
	    {E, S1} = expr(cerl:try_arg(T), Env, Ren, S0),
	    Vs = cerl:try_vars(T),
	    {Vs1, Env1, Ren1} = add_vars(Vs, Env, Ren),
	    {B, S2} = expr(cerl:try_body(T), Env1, Ren1, S1),
	    Evs = cerl:try_evars(T),
	    {Evs1, Env2, Ren2} = add_vars(Evs, Env, Ren),
	    {H, S3} = expr(cerl:try_handler(T), Env2, Ren2, S2),
	    {cerl:update_c_try(T, E, Vs1, B, Evs1, H), S3};
 	'catch' ->
	    {B, S1} = expr(cerl:catch_body(T), Env, Ren, S0),
	    {cerl:update_c_catch(T, B), S1};
	letrec ->
	    {Env1, S1} = letrec_defs(cerl:letrec_defs(T), Env, Ren, S0),
	    expr(cerl:letrec_body(T), Env1, Ren, S1);
        binary ->
	    {Segs, S1}=expr_list(cerl:binary_segs(T), Env, Ren, S0),
	    {cerl:update_c_binary(T, Segs),S1};
	bin_seg ->
	    {T1,S1} = expr(cerl:bin_seg_val(T), Env, Ren, S0),
	    {T2,S2} = expr(cerl:bin_seg_size(T), Env, Ren, S1),	
	    T3 = cerl:bin_seg_unit(T), 
	    T4 = cerl:bin_seg_type(T),
	    T5 = cerl:bin_seg_flags(T),
	    {cerl:update_c_bin_seg(T, T1, T2, T3, T4, T5), S2}

    end.

expr_list([T | Ts], Env, Ren, S0) ->
    {T1, S1} = expr(T, Env, Ren, S0),
    {Ts1, S2} = expr_list(Ts, Env, Ren, S1),
    {[T1 | Ts1], S2};
expr_list([], _, _, S) ->
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
	binary ->
	    Ts = pattern_list(cerl:binary_segs(T), Env, Ren),
	    cerl:update_c_binary(T, Ts);
	bin_seg ->
	    T1 = pattern(cerl:bin_seg_val(T), Env, Ren),
	    T2 = pattern(cerl:bin_seg_size(T), Env, Ren),	
	    T3 = cerl:bin_seg_unit(T), 
	    T4 = cerl:bin_seg_type(T),
	    T5 = cerl:bin_seg_flags(T),
	    cerl:update_c_bin_seg(T, T1, T2, T3, T4, T5);

	alias ->
	    V = pattern(cerl:alias_var(T), Env, Ren),
	    P = pattern(cerl:alias_pat(T), Env, Ren),
	    cerl:update_c_alias(T, V, P)
    end.

pattern_list([T | Ts], Env, Ren) ->
    [pattern(T, Env, Ren) | pattern_list(Ts, Env, Ren)];
pattern_list([], _, _) ->
    [].

%% For modules, we must handle the right-hand side fun-expressions
%% specially, since they are already at the top level, and we also
%% prefer them to remain first in the final list of definitions.

module_defs(Ds, Env, Ren, S0) ->
    {Ds1, Env1} = module_defs_1(Ds, [], Env),
    module_defs_2(Ds1, [], Env1, Ren, S0).

%% First set up the environment.

module_defs_1([{V, F} | Ds], Ds1, Env) ->
    Name = cerl:var_name(V),
    Env1 = env__bind(Name, {function, Name, []}, Env),
    module_defs_1(Ds, [{V, F, Name} | Ds1], Env1);
module_defs_1([], Ds, Env) ->
    {lists:reverse(Ds), Env}.

%% Then visit each function body.

module_defs_2([{V, F, Name} | Ds], Ds1, Env, Ren, S0) ->
    S1 = s__enter_function(Name, S0),
    Vs = cerl:fun_vars(F),
    Env1 = add_top_vars(Vs, Env),
    {B, S2} = expr(cerl:fun_body(F), Env1, Ren, S1),
    F1 = cerl:update_c_fun(F, Vs, B),
    module_defs_2(Ds, [{V, F1} | Ds1], Env, Ren, S2);
module_defs_2([], Ds, _, _, S) ->
    {lists:reverse(Ds), S}.

letrec_defs(Ds, Env, Ren, S0) ->
    {Ds1, Env1, S1} = letrec_defs_1(Ds, [], Env, Ren, S0),
    {Env1, letrec_defs_2(Ds1, Env1, Ren, S1)}.

%% First we must create the new function names and set up the
%% environment. See the discussion at the variable-handling function.

letrec_defs_1([{V, F} | Ds], Ds1, Env, Ren, S0) ->
    Free = get_free_vars(F, Env),
    {Name, S1} = new_lambda_fun_name(cerl:fun_arity(F) + length(Free),
				     Env, S0),
    Env1 = env__bind(cerl:var_name(V), {function, Name, Free}, Env),
    letrec_defs_1(Ds, [{V, F, Name, Free} | Ds1], Env1, Ren, S1);
letrec_defs_1([], Ds, Env, _Ren, S) ->
    {lists:reverse(Ds), Env, S}.

%% Then we can do the actual lambda lifting.

letrec_defs_2([{_V, F, Name, Free} | Ds], Env, Ren, S0) ->
    S1 = lift_lambda(F, Name, Free, Env, Ren, S0),
    letrec_defs_2(Ds, Env, Ren, S1);
letrec_defs_2([], _, _, S) ->
    S.

%% We use the no-shadowing strategy, renaming variables on the fly and
%% only when necessary to uphold the invariant.

add_vars(Vs, Env, Ren) -> 
    add_vars(Vs, [], Env, Ren).

add_vars([V | Vs], Vs1, Env, Ren) ->
    Name = cerl:var_name(V),
    {Name1, Ren1} = case env__is_defined(Name, Env) of
			false ->
			    {Name, Ren};
			true ->
			    New = env__new_name(Env),
			    {New, ren__add(Name, New, Ren)}
		    end,
    add_vars(Vs, [cerl:update_c_var(V, Name1) | Vs1],
	     env__bind(Name1, variable, Env), Ren1);
add_vars([], Vs, Env, Ren) ->
    {lists:reverse(Vs), Env, Ren}.

add_top_vars([V | Vs], Env) ->
    add_top_vars(Vs, env__bind(cerl:var_name(V), variable, Env));
add_top_vars([], Env) ->
    Env.

lookup_var(Id, Env) ->
    case env__lookup(Id, Env) of
	{ok, V} ->
	    V;
	error ->
	    error_msg("unbound variable `~P'.", [Id, 5]),
	    exit(badarg)
    end.

%% This handles variable references except in function application
%% operator positions (cf. apply_expr). For function-bound variables, we
%% create a 'make_fun' primitive operation call to make a closure
%% object. Note that we could also have created the closure at the site
%% of the original definition; this is a design choice, increasing
%% register pressure for the sake of faster calls to locally defined
%% functions.

var(V, Env, Ren, S) ->
    Name = ren__map(cerl:var_name(V), Ren),
    case lookup_var(Name, Env) of
	{function, F, Vs} ->
	    {make_fun_primop(F, Vs, S), S};
	variable ->
	    {cerl:update_c_var(V, Name), S}
    end.

%% Lifting source-code lambda expressions.

fun_expr(T, Env, Ren, S0) ->
    Free = get_free_vars(T, Env),
    {Name, S1} = new_lambda_fun_name(cerl:fun_arity(T) + length(Free),
				     Env, S0),
    S2 = lift_lambda(T, Name, Free, Env, Ren, S1),
    {make_fun_primop(Name, Free, S2), S2}.

make_fun_primop({Name, Arity}, Free, S) ->
    Module = s__get_module_name(S),
    Vs = [cerl:c_var(V) || V <- Free],
    cerl:c_primop(cerl:c_atom(?PRIMOP_MAKE_FUN),
		  [cerl:c_atom(Module),
		   cerl:c_atom(Name),
		   cerl:c_int(Arity),
		   cerl:c_int(0),
		   cerl:c_int(0),
		   cerl:make_list(Vs)]).

%% Only non-function variables are "really" free. For function variables
%% free in T, if they are letrec-bound, their closure variables are also
%% free in T (because the transformation may create accesses to them).

get_free_vars(T, Env) ->
    Vs = [V || V <- cerl_trees:free_variables(T)],
    ordsets:from_list(closure_vars(Vs, [], Env)).

closure_vars([V = {_, _} | Vs], As, Env) ->
    case env__lookup(V, Env) of
	{ok, {function, _, Vs1}} ->
	    closure_vars(Vs, Vs1 ++ As, Env);
	_ ->
	    closure_vars(Vs, As, Env)
    end;
closure_vars([V | Vs], As, Env) ->
    closure_vars(Vs, [V | As], Env);
closure_vars([], As, _) ->
    As.

%% This adds the free variables to the parameter list, visits the lambda
%% body recursively, and stores the resulting final name/lambda binding.
%% The free variables of the lambda are already in the environment.

lift_lambda(T, Name, Free, Env, Ren, S0) ->
    Vs = cerl:fun_vars(T),
    Env1 = add_top_vars(Vs, Env),
    %% We must add the original parameter variables to the environment
    %% before we generate the new variable for the closure.
    F = cerl:c_var(env__new_name(Env1)),
    Env2 = add_top_vars([F], Env1),
    B0 = cerl:c_let([cerl:c_var(V) || V <- Free],
		    cerl:c_values(closure_elements(length(Free), F)),
		    cerl:fun_body(T)),
    {B1, S1} = expr(B0, Env2, Ren, S0),
    %% The closure itself is passed as the last argument.
    E = cerl:ann_c_fun([lambda], Vs ++ [F], B1),
    s__add_def(cerl:c_var(Name), E, S1).

closure_elements(N, V) ->
    closure_elements(N, N + 1, V).

closure_elements(0, _, _) -> [];
closure_elements(N, M, V) ->
    [cerl:c_primop(cerl:c_atom(?PRIMOP_FUN_ELEMENT),
		   [cerl:c_int(M - N), V])
     | closure_elements(N - 1, M, V)].

new_lambda_fun_name(Arity, Env, S0) ->
    Name = case s__get_function_name(S0) of
	       {A, _} when atom(A) -> A;
	       _ -> function
	   end,
    {Id, S1} = s__new_lambda_id(S0),
    P = atom_to_list(Name) ++ "--lambda-" ++ integer_to_list(Id)
	++ "-",
    F = fun (N) ->
		{list_to_atom(P ++ integer_to_list(N)), Arity}
	end,
    {env__new_function_name(F, Env), S1}.

%% Lambda applications must be rewritten depending on the operator. For
%% a call to a known top-level function or lifted lambda, we make a
%% direct call. Otherwise, we create an "apply lambda" primitive
%% operation call.

apply_expr(T, Env, Ren, S0) ->
    {As, S1} = expr_list(cerl:apply_args(T), Env, Ren, S0),
    Op = cerl:apply_op(T),
    case cerl:is_c_var(Op) of
	true ->
	    Name = ren__map(cerl:var_name(Op), Ren),
	    case lookup_var(Name, Env) of
		{function, F, Vs} ->
		    Vs1 = [cerl:c_var(V) || V <- Vs],
		    {cerl:update_c_apply(T, cerl:c_var(F), As ++ Vs1),
		     S1};
		variable ->
		    apply_expr_1(Op, As, Env, Ren, S1)
	    end;
	_ ->
	    apply_expr_1(Op, As, Env, Ren, S1)
    end.

apply_expr_1(Op, As, Env, Ren, S0) ->
    {Op1, S1} = expr(Op, Env, Ren, S0),
    Call = cerl:c_primop(cerl:c_atom(?PRIMOP_APPLY_FUN),
			 [Op1, cerl:make_list(As)]),
    {Call, S1}.


%% ---------------------------------------------------------------------
%% Environment

env__new() ->
    rec_env:empty().

env__bind(Key, Value, Env) ->
    rec_env:bind(Key, Value, Env).

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

-record(state, {module, function, lambdas, defs = []}).

s__new(Module) ->
    #state{module = Module}.

s__get_module_name(S) ->
    S#state.module.

s__enter_function(F, S) ->
    S#state{function = F, lambdas = 0}.

s__get_function_name(S) ->
    S#state.function.

s__new_lambda_id(S) ->
    N = S#state.lambdas + 1,
    S1 = S#state{lambdas = N},
    {N, S1}.

s__add_def(N, F, S) ->
    S#state{defs = [{N, F} | S#state.defs]}.

s__get_defs(S) ->
    lists:reverse(S#state.defs).


%% ---------------------------------------------------------------------
%% Reporting

%% internal_error_msg(S) ->
%%     internal_error_msg(S, []).

%% internal_error_msg(S, Vs) ->
%%     error_msg(lists:concat(["Internal error: ", S]), Vs).

%% error_msg(S) ->
%%     error_msg(S, []).

error_msg(S, Vs) ->
    error_logger:error_msg(lists:concat([?MODULE, ": ", S, "\n"]), Vs).

%% warning_msg(S) ->
%%     warning_msg(S, []).

%% warning_msg(S, Vs) ->
%%     info_msg(lists:concat(["warning: ", S]), Vs).

%% info_msg(S) ->
%%     info_msg(S, []).

%% info_msg(S, Vs) ->
%%     error_logger:info_msg(lists:concat([?MODULE, ": ", S, "\n"]), Vs).
