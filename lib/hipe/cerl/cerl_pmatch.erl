%% =====================================================================
%% @doc Core Erlang pattern matching compiler.
%%
%% <p>For reference, see Simon L. Peyton Jones "The Implementation of
%% Functional Programming Languages", chapter 5 (by Phil Wadler).</p>
%% @end
%%
%% Copyright (C) 1999-2002 Richard Carlsson
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
%%
%% @type cerl() = cerl:cerl().
%%     Abstract Core Erlang syntax trees.
%% @type cerl_records() = cerl:cerl_records().
%%     An explicit record representation of Core Erlang syntax trees.

%% TODO: Binary-patterns

-module(cerl_pmatch).

-export([clauses/2, module/2, expr/2, core_transform/2]).

-import(lists, [all/2, splitwith/2, foldr/3, keysort/2, foldl/3,
		mapfoldl/3]).

%% @spec core_transform(Module::cerl_records(), Options::[term()]) ->
%%           cerl_records()
%%
%% @doc Transforms a module represented by records. See
%% <code>module/2</code> for details.
%%
%% <p>Use the compiler option <code>{core_transform, cerl_pmatch}</code>
%% to insert this function as a compilation pass.</p>
%%
%% @see module/2

core_transform(M, Opts) ->
    cerl:to_records(module(cerl:from_records(M), Opts)).


%% @spec module(Module::cerl(), Options::[term()]) -> cerl()
%%
%% @doc Rewrites all <code>case</code>-clauses in <code>Module</code>.
%% <code>receive</code>-clauses are not affected. Currently, no options
%% are available.
%%
%% @see clauses/2
%% @see expr/2
%% @see core_transform/2

module(M, _Opts) ->
    expr(M, env__empty()).


%% @spec clauses(Clauses::[Clause], Env) -> {Expr, Vars}
%%    Clause = cerl()
%%    Expr = cerl()
%%    Vars = [cerl()]
%%    Env = rec_env:environment()
%%
%% @doc Rewrites a sequence of clauses to an equivalent expression,
%% removing as much repeated testing as possible. Returns a pair
%% <code>{Expr, Vars}</code>, where <code>Expr</code> is the resulting
%% expression, and <code>Vars</code> is a list of new variables (i.e.,
%% not already in the given environment) to be bound to the arguments to
%% the switch. The following is a typical example (assuming
%% <code>E</code> is a Core Erlang case expression):
%% <pre>
%%   handle_case(E, Env) ->
%%       Cs = case_clauses(E),
%%       {E1, Vs} = cerl_pmatch(Cs, Env),
%%       c_let(Vs, case_arg(E), E1).
%% </pre>
%% 
%% <p>The environment is used for generating new variables which do not
%% shadow existing bindings.</p>
%% 
%% @see rec_env
%% @see expr/2
%% @see module/2

clauses(Cs, Env) ->
    clauses(Cs, none, Env).

clauses([C | _] = Cs, Else, Env) ->
    Vs = new_vars(cerl:clause_arity(C), Env),
    E = match(Vs, Cs, Else, add_vars(Vs, Env)),
    {E, Vs};
clauses([], _Else, _Env) ->
    throw(no_clauses).

%% The implementation very closely follows that described in the book.

match([], Cs, Else, _Env) ->
    %% If the "default action" is the atom 'none', it is simply not
    %% added; otherwise it is put in the body of a final catch-all
    %% clause (which is often removed by the below optimization).
    Cs1 = if Else == none -> Cs;
	     true -> Cs ++ [cerl:c_clause([], Else)]
	  end,
    %% This clause reduction is an important optimization. It selects a
    %% clause body if possible, and otherwise just removes dead clauses.
    case cerl_clauses:reduce(Cs1) of
 	{true, {C, []}} ->    % if we get bindings, something is wrong!
 	    cerl:clause_body(C);
 	{false, Cs2} ->
	    %% This happens when guards are nontrivial.
 	    cerl:c_case(cerl:c_values([]), Cs2)
    end;
match([V | _] = Vs, Cs, Else, Env) ->
    foldr(fun (Cs, Else) ->
		  match_var_con(Vs, Cs, Else, Env)
	  end,
	  Else,
	  group([unalias(C, V) || C <- Cs], fun is_var_clause/1)).

group([], _F) ->
    [];
group([X | _] = Xs, F) ->
    group(Xs, F, F(X)).

group([], _F, _P) ->
    [];
group(Xs, F, P) ->
    {First, Rest} = splitwith(fun (X) -> F(X) == P end, Xs),
    [First | group(Rest, F)].

is_var_clause(C) ->
    cerl:is_c_var(hd(cerl:clause_pats(C))).

%% To avoid code duplication, if the 'Else' expression is too big, we
%% put it in a local function definition instead, and replace it with a
%% call. (Note that it is important that 'is_lightweight' does not yield
%% 'true' for a simple function application, or we will create a lot of
%% unnecessary extra functions.)

match_var_con(Vs, Cs, none = Else, Env) ->
    match_var_con_1(Vs, Cs, Else, Env);
match_var_con(Vs, Cs, Else, Env) ->
    case is_lightweight(Else) of
	true ->
	    match_var_con_1(Vs, Cs, Else, Env);
	false ->
	    F = new_fvar("match_", 0, Env),
	    Else1 = cerl:c_apply(F, []),
	    Env1 = add_vars([F], Env),
	    cerl:c_letrec([{F, cerl:c_fun([], Else)}],
			  match_var_con_1(Vs, Cs, Else1, Env1))
    end.

match_var_con_1(Vs, Cs, Else, Env) ->
    case is_var_clause(hd(Cs)) of
	true ->
	    match_var(Vs, Cs, Else, Env);
	false ->
	    match_con(Vs, Cs, Else, Env)
    end.

match_var([V | Vs], Cs, Else, Env) ->
    Cs1 = [begin
	       [P | Ps] = cerl:clause_pats(C),
	       G = make_let([P], V, cerl:clause_guard(C)),
	       B = make_let([P], V, cerl:clause_body(C)),
	       cerl:update_c_clause(C, Ps, G, B)
	   end
	   || C <- Cs],
    match(Vs, Cs1, Else, Env).

%% Since Erlang is dynamically typed, we must include the possibility
%% that none of the constructors in the group will match, and in that
%% case the "Else" code will be executed (unless it is 'none'), in the
%% body of a final catch-all clause.

match_con([V | Vs], Cs, Else, Env) ->

    case group_con(Cs) of
 	[{_, _, Gs}] ->
 	    %% Don't create a group type switch if there is only one
 	    %% such group.

	    make_switch(V, [match_congroup(D, Vs, Cs, Else, Env)
 			    ||  {_, D, Cs} <- Gs],
 			Else, Env);
	Ts ->
	    Cs1 = [match_typegroup(T, V, Vs, Gs, Else, Env)
		   || {_, T, Gs} <- Ts],
	    make_switch(V, Cs1, Else, Env)
    end.

match_typegroup(_T, _V, Vs, [{_, D, Cs}], Else, Env) ->
    %% Don't create a group type switch if there is only one constructor
    %% in the group. (Note that this always happens for the empty list.)
    match_congroup(D, Vs, Cs, Else, Env);
match_typegroup(T, V, Vs, Gs, Else, Env) ->
    Body = make_switch(V, [match_congroup(D, Vs, Cs, Else, Env)
			   ||  {_, D, Cs} <- Gs],
		       Else, Env),
    typetest_clause(T, V, Body, Env).

match_congroup({<<0>>, Segs}, Vs, Cs, Else, Env) ->
    Body = match(Vs, Cs, Else, Env),
    cerl:c_clause([make_pat(<<0>>, Segs)], Body);

match_congroup({D, A}, Vs, Cs, Else, Env) ->
    Vs1 = new_vars(A, Env),
    Body = match(Vs1 ++ Vs, Cs, Else, add_vars(Vs1, Env)),
    cerl:c_clause([make_pat(D, Vs1)], Body).

make_switch(V, Cs, Else, Env) ->
    cerl:c_case(V, if Else == none -> Cs;
		      true -> Cs ++ [cerl:c_clause([new_var(Env)],
						   Else)]
		   end).

%% We preserve the relative order of different constructors as they were
%% originally listed. This is done by including the clause number in the
%% key used for sorting the clauses by type (this stage does not
%% distingush between different value instances of atoms, integers and
%% floats). The clauses are then grouped by type. After grouping by
%% constructor (this separates value instances), the groups are then
%% sorted again by the clause number of the first clause of each group.

group_con(Cs) ->
    {Cs1, _} = mapfoldl(fun (C, N) ->
				[P | Ps] = cerl:clause_pats(C),
				Ps1 = sub_pats(P) ++ Ps,
				G = cerl:clause_guard(C),
				B = cerl:clause_body(C),
				{{{con_type(P), N}, con_desc(P),
				  cerl:update_c_clause(C, Ps1, G, B)},
				 N + 1}
			end,
			0, Cs),
    %% Group by descriptor (separates different integers, atoms, etc.)
    %% Constructors that have the same descriptor have the same type.
    Css = group(keysort(1, Cs1), fun ({_,D,_}) -> D end),
    Gs = [{T, D, [C || {_,_,C} <- Cs]} || Cs=[{T,D,_}|_] <- Css],
    %% Group by type class (groups different-arity tuples together).
    Css1 = group(Gs, fun ({{T,_},_,_}) -> typeclass(T) end),
    Gs1 = [{N, T, Cs} || Cs=[{{T,N},_,_}|_] <- Css1],
    %% Sort type-groups by original clause order
    keysort(1, Gs1).

%% Since Erlang clause patterns can contain "alias patterns", we must
%% eliminate these, by turning them into let-definitions in the guards
%% and bodies of the clauses.

unalias(C, V) -> 
    [P | Ps] = cerl:clause_pats(C),
    B = cerl:clause_body(C),
    unalias(P, V, Ps, B, C).

unalias(P, V, Ps, B, C) ->
    case cerl:type(P) of
	alias ->
	    B1 = make_let([cerl:alias_var(P)], V, B),
	    unalias(cerl:alias_pat(P), V, Ps, B1, C);
	_ ->
	    cerl:update_c_clause(C, [P | Ps], cerl:clause_guard(C), B)
    end.

%% This returns a constructor type, for sorting of clauses. It does not
%% distinguish between value instances of atoms, integers and floats.

con_type(E) ->
    case cerl:type(E) of
	cons ->
	    cons;
	tuple ->
	    {tuple, cerl:tuple_arity(E)};
	binary ->
	    binary;
	literal ->
	    case cerl:concrete(E) of
		V when is_atom(V) -> atom;
		V when is_integer(V) -> integer;
		V when is_float(V) -> float;
		T when is_tuple(T) -> {tuple, size(T)};
		[_|_] -> cons;
		[] -> []
	    end;
	_ ->
	    throw({bad_constructor, E})
    end.

%% Getting the generic class for type grouping.

typeclass({tuple, _}) -> tuple;
typeclass(X) -> X.

%% Generating a type-switch clause

typetest_clause({tuple, _}, V, E, _Env) ->
    typetest_clause_1(is_tuple, V, E);
typetest_clause(atom, V, E, _Env) ->
    typetest_clause_1(is_atom, V, E);
typetest_clause(integer, V, E, _Env) ->
    typetest_clause_1(is_integer, V, E);
typetest_clause(float, V, E, _Env) ->
    typetest_clause_1(is_float, V, E);
typetest_clause(cons, _V, E, Env) ->
    [V1, V2] = new_vars(2, Env),
    cerl:c_clause([cerl:c_cons(V1, V2)], E);  % there is no 'is cons'
typetest_clause([], _V, E, _Env) ->
    cerl:c_clause([cerl:c_nil()], E);
typetest_clause(binary, V, E, _Env) ->
    typetest_clause_1(is_binary, V, E).

typetest_clause_1(T, V, E) ->
    cerl:c_clause([V], cerl:c_call(cerl:c_atom('erlang'),
				   cerl:c_atom(T), [V]), E).

%% This returns a constructor descriptor, to be used for grouping and
%% pattern generation. It consists of an identifier term and the arity.

-define(cons_id, [0]).
-define(tuple_id, {0}).
-define(binary_id, <<0>>).
-define(literal_id(V), V).

con_desc(E) ->
    case cerl:type(E) of
	cons -> {?cons_id, 2};
	tuple -> {?tuple_id, cerl:tuple_arity(E)};
	binary -> {?binary_id, cerl:binary_segs(E)};
	literal ->
	    case cerl:concrete(E) of
		[_|_] -> {?cons_id, 2};
		T when is_tuple(T) -> {?tuple_id, size(T)};
		V -> {?literal_id(V), 0}
	    end;
	_ ->
	    throw({bad_constructor, E})
    end.

%% This creates a new constructor pattern from a type descriptor and a
%% list of variables.

make_pat(?cons_id, [V1, V2]) -> cerl:c_cons(V1, V2);
make_pat(?tuple_id, Vs) -> cerl:c_tuple(Vs);
make_pat(?binary_id, Segs) -> cerl:c_binary(Segs);
make_pat(?literal_id(Val), []) -> cerl:abstract(Val).

%% This returns the list of subpatterns of a constructor pattern.

sub_pats(E) ->
    case cerl:type(E) of
	cons ->
	    [cerl:cons_hd(E), cerl:cons_tl(E)];
	tuple ->
	    cerl:tuple_es(E);
	binary ->
	    [];
	literal ->
	    case cerl:concrete(E) of
		[H|T] -> [cerl:abstract(H), cerl:abstract(T)];
		T when is_tuple(T) -> [cerl:abstract(X)
				       || X <- tuple_to_list(T)];
		_ -> []
	    end;
	_ ->
	    throw({bad_constructor_pattern, E})
    end.

%% This avoids generating stupid things like "let X = ... in 'true'",
%% keeping the generated code cleaner.

make_let(Vs, A, B) ->
    case cerl:type(B) of
	literal ->
	    case is_simple(A) of
		true -> B;
		false -> cerl:c_let(Vs, A, B)
	    end;
	_ -> cerl:c_let(Vs, A, B)
    end.

%% ------------------------------------------------------------------------
%% Rewriting a module or other expression:

%% @spec expr(Expression::cerl(), Env) -> cerl()
%%    Env = rec_env:environment()
%%
%% @doc Rewrites all <code>case</code>-clauses in
%% <code>Expression</code>. <code>receive</code>-clauses are not
%% affected.
%%
%% <p>The environment is used for generating new variables which do not
%% shadow existing bindings.</p>
%% 
%% @see clauses/2
%% @see rec_env

expr(E, Env) ->
    case cerl:type(E) of
 	literal ->
	    E;
	var ->
	    E;
	values ->
	    Es = expr_list(cerl:values_es(E), Env),
 	    cerl:update_c_values(E, Es);
	cons ->
	    H = expr(cerl:cons_hd(E), Env),
	    T = expr(cerl:cons_tl(E), Env),
	    cerl:update_c_cons(E, H, T);
 	tuple ->
	    Es = expr_list(cerl:tuple_es(E), Env),
	    cerl:update_c_tuple(E, Es);
 	'let' ->
	    A = expr(cerl:let_arg(E), Env),
	    Vs = cerl:let_vars(E),
	    Env1 = add_vars(Vs, Env),
	    B = expr(cerl:let_body(E), Env1),
	    cerl:update_c_let(E, Vs, A, B);
	seq ->
	    A = expr(cerl:seq_arg(E), Env),
	    B = expr(cerl:seq_body(E), Env),
 	    cerl:update_c_seq(E, A, B);
 	apply ->
	    Op = expr(cerl:apply_op(E), Env),
	    As = expr_list(cerl:apply_args(E), Env),
 	    cerl:update_c_apply(E, Op, As);
 	call ->
	    M = expr(cerl:call_module(E), Env),
	    N = expr(cerl:call_name(E), Env),
	    As = expr_list(cerl:call_args(E), Env),
 	    cerl:update_c_call(E, M, N, As);
 	primop ->
	    As = expr_list(cerl:primop_args(E), Env),
	    cerl:update_c_primop(E, cerl:primop_name(E), As);
 	'case' ->
	    A = expr(cerl:case_arg(E), Env),
	    Cs = expr_list(cerl:case_clauses(E), Env),
	    {E1, Vs} = cerl_pmatch:clauses(Cs, Env),
 	    make_let(Vs, A, E1);
 	clause ->
	    Vs = cerl:clause_vars(E),
	    Env1 = add_vars(Vs, Env),
	    G = expr(cerl:clause_guard(E), Env1),
	    B = expr(cerl:clause_body(E), Env1),
	    cerl:update_c_clause(E, cerl:clause_pats(E), G, B);
 	'fun' ->
	    Vs = cerl:fun_vars(E),
	    Env1 = add_vars(Vs, Env),
	    B = expr(cerl:fun_body(E), Env1),
	    cerl:update_c_fun(E, Vs, B);
 	'receive' ->
	    %% NOTE: No pattern matching compilation done here!
	    Cs = expr_list(cerl:receive_clauses(E), Env),
	    T = expr(cerl:receive_timeout(E), Env),
	    A = expr(cerl:receive_action(E), Env),
	    cerl:update_c_receive(E, Cs, T, A);
	'try' ->
	    A = expr(cerl:try_arg(E), Env),
	    Vs = cerl:try_vars(E),
	    B = expr(cerl:try_body(E), add_vars(Vs, Env)),
	    Evs = cerl:try_evars(E),
	    H = expr(cerl:try_handler(E), add_vars(Evs, Env)),
	    cerl:update_c_try(E, A, Vs, B, Evs, H);
 	'catch' ->
	    B = expr(cerl:catch_body(E), Env),
	    cerl:update_c_catch(E, B);
	letrec ->
	    Ds = cerl:letrec_defs(E),
	    Env1 = add_defs(Ds, Env),
	    Ds1 = defs(Ds, Env1),
	    B = expr(cerl:letrec_body(E), Env1),
	    cerl:update_c_letrec(E, Ds1, B);
	module ->
	    Ds = cerl:module_defs(E),
	    Env1 = add_defs(Ds, Env),
	    Ds1 = defs(Ds, Env1),
	    cerl:update_c_module(E, cerl:module_name(E),
				 cerl:module_exports(E),
				 cerl:module_attrs(E), Ds1)
    end.

expr_list(Es, Env) ->
    [expr(E, Env) || E <- Es].

defs(Ds, Env) ->
    [{V, expr(F, Env)} || {V, F} <- Ds].


%% ------------------------------------------------------------------------
%%	Support functions

new_var(Env) ->
    Name = env__new_vname(Env),
    cerl:c_var(Name).

new_vars(N, Env) ->
    [cerl:c_var(V) || V <- env__new_vnames(N, Env)].

new_fvar(A, N, Env) ->
    Name = env__new_fname(A, N, Env),
    cerl:c_var(Name).

add_vars(Vs, Env) ->
    foldl(fun (V, Env) -> env__bind(cerl:var_name(V), [], Env) end,
	  Env, Vs).

add_defs(Ds, Env) ->
    foldl(fun ({V, _F}, Env) ->
		  env__bind(cerl:var_name(V), [], Env)
	  end, Env, Ds).

%% For now, we duplicate code without limitations, as long as lifting
%% out the code always results in a full local function call even when
%% the code is in last call context, since this causes notable slowdown
%% in tight loops.

is_lightweight(_) -> true.

%% is_lightweight(E) ->
%%     case cerl:type(E) of
%%    	var -> true;
%%    	literal -> true;
%%    	values -> all(fun is_simple/1, cerl:values_es(E));
%%    	cons -> is_simple(cerl:cons_hd(E))
%%    		    andalso is_simple(cerl:cons_tl(E));
%%    	tuple -> all(fun is_simple/1, cerl:tuple_es(E));
%%    	'let' -> (is_simple(cerl:let_arg(E)) andalso
%%    		  is_lightweight(cerl:let_body(E)));
%%    	seq -> (is_simple(cerl:seq_arg(E)) andalso
%%    		is_lightweight(cerl:seq_body(E)));
%%    	apply ->
%%    	    is_simple(cerl:apply_op(E))
%%    		andalso all(fun is_simple/1, cerl:apply_args(E));
%%    	call ->
%%    	    is_simple(cerl:call_module(E))
%%    		andalso is_simple(cerl:call_name(E))
%%    		andalso all(fun is_simple/1, cerl:call_args(E));    
%%    	_ -> false
%%     end.

is_simple(E) ->
    case cerl:type(E) of
	var -> true;
	literal -> true;
	values -> all(fun is_simple/1, cerl:values_es(E));
	_ -> false
    end.


%% ------------------------------------------------------------------------
%% Abstract datatype: environment()

env__empty() ->
    rec_env:empty().

env__bind(Key, Val, Env) ->
    rec_env:bind(Key, Val, Env).

%% `Es' should have type `[{Key, Val}]', and `Fun' should have type
%% `(Val, Env) -> T', mapping a value together with the recursive
%% environment itself to some term `T' to be returned when the entry is
%% looked up.

%% env__bind_recursive(Ks, Vs, F, Env) ->
%%     rec_env:bind_recursive(Ks, Vs, F, Env).

%% env__lookup(Key, Env) ->
%%     rec_env:lookup(Key, Env).

%% env__get(Key, Env) ->
%%     rec_env:get(Key, Env).

%% env__is_defined(Key, Env) ->
%%     rec_env:is_defined(Key, Env).

env__new_vname(Env) ->
    rec_env:new_key(Env).

env__new_vnames(N, Env) ->
    rec_env:new_keys(N, Env).

env__new_fname(F, A, Env) ->
    rec_env:new_custom_key(fun (X) ->
				   S = integer_to_list(X),
				   {list_to_atom(F ++ S), A}
			   end,
			   Env).
