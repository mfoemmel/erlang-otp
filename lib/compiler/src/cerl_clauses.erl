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
%% The Initial Developer of the Original Code is Richard Carlsson.
%% Copyright (C) 1999-2001 Richard Carlsson.
%% Portions created by Ericsson are Copyright 2001, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$

%% =====================================================================
%% Utility functions for Core Erlang `case'/`receive' clauses.

-module(cerl_clauses).

-export([any_catchall/1, eval_guard/1, is_catchall/1, match/2,
	 match_list/2, reduce/1, reduce/2]).

-import(cerl, [alias_pat/1, alias_var/1, data_arity/1, data_es/1,
	       data_type/1, clause_guard/1, clause_pats/1, concrete/1,
	       is_data/1, is_c_var/1, let_body/1, letrec_body/1,
	       seq_body/1, try_expr/1, type/1, values_es/1]).

-import(lists, [reverse/1]).


%% =====================================================================
%% is_catchall(Clause) -> bool()
%%
%%	    Clause = coreErlang()
%%
%%	    cerl:is_c_clause(Clause)
%%
%%	Returns `true' if the clause is a catch-all clause (i.e., if its
%%	patterns are variables and its guard expression yields `true';
%%	cf. `eval_guard'). Otherwise returns `false'.
%%
%% have_catchall(Clauses) -> bool()
%%
%%	    Clauses = [Clause]
%%	    Clause = coreErlang()
%%
%%	    cerl:is_c_clause(Clause)
%%
%%	Returns `true' if any of the clauses in `Clauses' is a catch-all
%%	clause; otherwise `false'.

is_catchall(C) ->
    case all_vars(clause_pats(C)) of
	true ->
	    case eval_guard(clause_guard(C)) of
		{value, true} ->
		    true;
		_ ->
		    false
	    end;
	false ->
	    false
    end.

all_vars([C | Cs]) ->
    case is_c_var(C) of
	true ->
	    all_vars(Cs);
	false ->
	    false
    end;
all_vars([]) ->
    true.

any_catchall([C | Cs]) ->
    case is_catchall(C) of
	true ->
	    true;
	false ->
	    any_catchall(Cs)
    end;
any_catchall([]) ->
    false.


%% =====================================================================
%% eval_guard(Expr) -> false | {value, Term}
%%
%%	    Expr = coreErlang()
%%	    Term = term()
%%
%%	Returns `{value, Term}' if the clause guard expression `Expr'
%%	always yields the constant value `Term', and otherwise returns
%%	`false'. Note that even though guard expressions should only
%%	yield boolean values, this function does not require `Term' to
%%	be either `true' or `false'. Also note that only simple
%%	constructs like `values', `let', etc., are examined recursively;
%%	general constant folding is not performed.

%% This function could possibly be improved further, but constant
%% folding should in general be performed elsewhere.

eval_guard(E) ->
    case type(E) of
	literal ->
	    {value, concrete(E)};
	values ->
	    case values_es(E) of
		[E1] ->
		    eval_guard(E1);
		_ ->
		    false
	    end;
	'try' ->
	    eval_guard(try_expr(E));
	seq ->
	    eval_guard(seq_body(E));
	'let' ->
	    eval_guard(let_body(E));
	'letrec' ->
	    eval_guard(letrec_body(E));
	_ ->
	    false
    end.


%% =====================================================================
%% reduce(Clauses) -> {true, {{Clause, Data}, Bindings}}
%%                  | {false, Clauses1}
%% reduce(Clauses, Exprs) -> {true, {{Clause, Data}, Bindings}}
%%                         | {false, Clauses1}
%%
%%	    Clauses = Clauses1 = [{Clause, Data}]
%%	    Clause = coreErlang()
%%	    Data = term()
%%	    Exprs = [Expr]
%%	    Expr = any | coreErlang()
%%	    Bindings = [{Var, coreErlang()}]
%%	    Var = coreErlang()
%%
%%	    cerl:is_c_clause(Clause)
%%	    cerl:is_c_var(Var)
%%
%%	This function takes a list `Clauses' of pairs of `clause' syntax
%%	trees and respective associated arbitrary terms, and a list of
%%	switch expressions `Exprs', which may be empty. Leaving out
%%	`Exprs' is equivalent to passing the empty list. The function
%%	tries to uniquely select a single clause or discard unselectable
%%	clauses. A clause can only be selected if its guard expression
%%	always yields the atom `true', and a clause whose guard
%%	expression always yields the atom `false' can never be selected;
%%	other guard expressions are considered to have unknown value,
%%	cf. `eval_guard'. All clauses in the list must have the same
%%	number of patterns. If `Exprs' is not the empty list, it must
%%	have the same length as the number of patterns in each clause.
%%
%%	If a particular clause could be selected, the function returns
%%	`{true, {{Clause, Data}, Bindings}}', where the pair `{Clause,
%%	Data}' is the corresponding pair in `Clauses'. Note that this
%%	will not be the case if the length of `Exprs' is not the same as
%%	the number of patterns in the clauses. `Bindings' is then a list
%%	of pairs associating the variables in the patterns of `Clause'
%%	with the corresponding subexpressions in `Exprs'. The list is
%%	given in innermost-first order; see the `match' function.
%%
%%	If no clause could be definitely selected, the function returns
%%	`{false, NewClauses}', where `NewClauses' is the list of pairs
%%	`{Clause, Data}' in `Clauses' for which the clauses are not
%%	definitely unselectable, in the same relative order.

reduce(Cs) ->
    reduce(Cs, []).

reduce(Cs, Es) ->
    reduce(Cs, Es, []).

reduce([C0 | Cs], Es, Cs1) ->
    {C, D} = C0,
    Ps = clause_pats(C),
    case match_list(Ps, Es) of
	false ->
	    %% Here, we know that the current clause cannot possibly be
	    %% selected, so we drop it and visit the rest.
	    reduce(Cs, Es, Cs1);
	{false, _} ->
	    %% We are not sure if this clause might be selected, so we
	    %% save it and visit the rest.
	    reduce(Cs, Es, [C0 | Cs1]);
	{true, Bs} ->
	    case eval_guard(clause_guard(C)) of
		{value, true} when Cs1 == [] ->
		    %% We have a definite match - we return the residual
		    %% expression and signal that a selection has been
		    %% made. All other clauses are dropped.
		    {true, {C0, Bs}};
		{value, true} ->
		    %% Unless one of the previous clauses is selected,
		    %% this clause will definitely be, so we can drop
		    %% the rest.
		    {false, reverse([C0 | Cs1])};
		{value, false} ->
		    %% This clause can never be selected, since its
		    %% guard is never `true', so we drop it.
		    reduce(Cs, Es, Cs1);
		_ ->
		    %% We are not sure if this clause might be selected
		    %% (or might even cause a crash), so we save it and
		    %% visit the rest.
		    reduce(Cs, Es, [C0 | Cs1])
	    end
    end;
reduce([], _, Cs) ->
    %% All clauses visited, without a complete match. Signal "not
    %% reduced" and return the saved clauses, in the correct order.
    {false, reverse(Cs)}.


%% =====================================================================
%% match(Pattern, Expr) -> false | {false, Bindings} | {true, Bindings}
%%
%%	    Pattern = coreErlang()
%%	    Expr = any | coreErlang()
%%	    Bindings = [{Var, Expr}]
%%	    Var = coreErlang()
%%
%%	    cerl:is_c_var(Var)
%%
%%	Matches the syntax tree `Pattern' against the syntax tree
%%	`Expr', or the atom `any'. Returns `false' if a match is
%%	impossible, `{false, Bindings}' if a match cannot be excluded,
%%	and `{true, Bindings}' if `Pattern' definitely matches `Expr'.
%%	`Bindings' is then a list of pairs associating each variable in
%%	`Pattern' with the corresponding subexpression of `Expr', or
%%	with the atom `any' if no such subexpression is known. The list
%%	is given in innermost-first order (only of interest if `Pattern'
%%	contains one or more alias patterns). Recall that variables may
%%	not be repeated in a Core Erlang pattern. If the function
%%	returns `{yes, []}', the pattern and the expression were
%%	identical.
%%
%%	Note: Binary-syntax patterns are never structurally matched
%%	against binary-syntax expressions by this function.
%%
%%	Examples:
%%
%%	  - Matching a pattern `{X, Y}' against the expression `{foo,
%%	  f(Z)}' yields `{true, Bindings}' where `Bindings' associates
%%	  `X' with the subtree `foo' and `Y' with the subtree `f(Z)'.
%%
%%	  - Matching pattern `{X, {bar, Y}}' against the expression
%%	  `{foo, f(Z)}' yields `{false, Bindings}' where `Bindings'
%%	  associates `X' with the subtree `foo' and `Y' with the *atom*
%%	  `any' (because it is not known if the value of `f(Z)' might
%%	  match `{foo, Y}' or not).
%%
%%	  - Matching `{foo, bar}' against expression `{foo, f()}' yields
%%	  `{false, []}', telling us that there might be a match, but we
%%	  cannot deduce any bindings.
%%
%%	  - Matching `X = {foo, Y}' against expression `{foo, bar}'
%%	  yields `{true, Bindings}' where `Bindings' associates `Y' with
%%	  the subtree `bar', and `X' with `{Foo, bar}'.
%%
%% match_list(Patterns, Exprs) -> false | {false, Bindings}
%%			        | {true, Bindings}
%%
%%	    Patterns = [coreErlang()]
%%	    Exprs = [Expr]
%%	    Expr = any | coreErlang()
%%	    Bindings = [{Var, Expr}]
%%	    Var = coreErlang()
%%
%%	    cerl:is_c_var(Var)
%%
%%	Like `match', but matching a sequence of patterns against a
%%	corresponding sequence of expressions (and/or `any' atoms). If
%%	`Exprs' is the empty list, it is equivalent to passing a list of
%%	`any' atoms of the same length as `Patterns'.

match(P, E) ->
    match(P, E, []).

match(P, E, Bs) ->
    case type(P) of
	var ->
	    %% Variables always match, since they cannot have repeated
	    %% occurrences in a pattern.
	    {true, [{P, E} | Bs]};
	alias ->
	    %% All variables in P1 will be listed before the alias
	    %% variable in the result.
	    match(alias_pat(P), E, [{alias_var(P), E} | Bs]);
	binary ->
	    %% The most we can do is to say "definitely no match" if a
	    %% binary pattern is matched against non-binary data.
	    if E == any ->
		    {false, Bs};
	       true ->
		    case is_data(E) of
			true ->
			    false;
			false ->
			    {false, Bs}
		    end
	    end;
	_ ->
	    match_1(P, E, Bs)
    end.

match_1(P, E, Bs) ->
    case is_data(P) of
	true when E == any ->
	    %% If we don't know the structure of the value of E at this
	    %% point, we just match the subpatterns against `any', and
	    %% make sure the result is a "maybe".
	    Ps = data_es(P),
	    Es = lists:duplicate(length(Ps), any),
	    case match_list(Ps, Es, Bs) of
		{_, Bs1} ->
		    {false, Bs1};
		false ->
		    false
	    end;
	true ->
	    %% Test if the expression represents a constructor
	    case is_data(E) of
		true ->
		    T1 = {data_type(E), data_arity(E)},
		    T2 = {data_type(P), data_arity(P)},
		    %% Note that we must test for exact equality.
		    if T1 =:= T2 ->
			    match_list(data_es(P), data_es(E), Bs);
		       true ->
			    false
		    end;
		false ->
		    %% We don't know the run-time structure of E, and P
		    %% is not a variable or an alias pattern, so we
		    %% match against `any' instead.
		    match_1(P, any, Bs)
	    end;
	false ->
	    %% Strange pattern - give up, but don't say "no match".
	    {false, Bs}
    end.

match_list([], []) ->
    {true, []};    % no patterns always match
match_list(Ps, []) ->
    match_list(Ps, lists:duplicate(length(Ps), any), []);
match_list(Ps, Es) ->
    match_list(Ps, Es, []).

match_list([P | Ps], [E | Es], Bs) ->
    case match(P, E, Bs) of
	{true, Bs1} ->
	    match_list(Ps, Es, Bs1);
	{false, Bs1} ->
	    %% Make sure "maybe" is preserved
	    case match_list(Ps, Es, Bs1) of
		{_, Bs2} ->
		    {false, Bs2};
		false ->
		    false
	    end;
	false ->
	    false
    end;
match_list([], [], Bs) ->
    {true, Bs}.


%% =====================================================================
