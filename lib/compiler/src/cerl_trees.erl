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
%% Basic functions on Core Erlang abstract syntax trees.

-module(cerl_trees).

-export([depth/1, fold/3, free_variables/1, map/2, mapfold/3, size/1,
	 variables/1]).

-import(cerl, [alias_pat/1, alias_var/1, apply_args/1, apply_op/1,
	       binary_segs/1, bin_seg_val/1, bin_seg_size/1,
	       bin_seg_unit/1, bin_seg_type/1, bin_seg_flags/1,
	       call_args/1, call_module/1, call_name/1, case_arg/1,
	       case_clauses/1, catch_body/1, update_c_alias/3,
	       update_c_apply/3, update_c_binary/2, update_c_bin_seg/6,
	       update_c_call/4, update_c_case/3, update_c_catch/2,
	       update_c_clause/4, update_c_cons/3, update_c_fun/3,
	       update_c_let/4, update_c_letrec/3, update_c_module/5,
	       update_c_primop/3, update_c_receive/4, update_c_seq/3,
	       update_c_try/4, update_c_tuple/2, update_c_values/2,
	       clause_body/1, clause_guard/1, clause_pats/1, concrete/1,
	       cons_hd/1, cons_tl/1, fun_body/1, fun_vars/1, let_arg/1,
	       let_body/1, let_vars/1, letrec_body/1, letrec_defs/1,
	       letrec_vars/1, module_attrs/1, module_defs/1,
	       module_exports/1, module_name/1, module_vars/1,
	       primop_args/1, primop_name/1, receive_action/1,
	       receive_clauses/1, receive_timeout/1, seq_arg/1,
	       seq_body/1, subtrees/1, try_body/1, try_expr/1,
	       try_vars/1, tuple_es/1, type/1, values_es/1,
	       var_name/1]).


%% =====================================================================
%% depth(Tree) -> integer()
%%
%%	Returns the length of the longest path in `Tree' (i.e., a leaf
%%	node yields zero, a tree representing `{foo, bar}' yields one,
%%	etc.
%%
%% size(Tree) -> integer()
%%
%%	Returns the number of nodes in `Tree'.

depth(T) ->
    case subtrees(T) of
	[] ->
	    0;
	Gs ->
	    1 + lists:foldl(fun (G, A) -> max(depth_1(G), A) end, 0, Gs)
    end.

depth_1(Ts) ->
    lists:foldl(fun (T, A) -> max(depth(T), A) end, 0, Ts).

max(X, Y) when X > Y -> X; 
max(X, Y) -> Y.

size(T) ->
    fold(fun (X, S) -> S + 1 end, 0, T).


%% =====================================================================
%% map(Function, Tree) -> Tree1
%%
%%	   Function = (Tree) -> Tree1
%%	   Tree = Tree1 = coreErlang()
%%	   
%%	The resulting `Tree1' is the given `Tree' where every node has
%%	been replaced by the result of applying `Function' on the
%%	original node.

map(F, T) ->
    F(map_1(F, T)).

map_1(F, T) ->
    case type(T) of
 	literal ->
	    case concrete(T) of
		[_ | _] ->
		    update_c_cons(T, map(F, cons_hd(T)),
				  map(F, cons_tl(T)));
		V when tuple(V), size(V) > 0 ->
		    update_c_tuple(T, map_list(F, tuple_es(T)));
		_ ->
		    T
	    end;
 	var ->
 	    T;
	values ->
 	    update_c_values(T, map_list(F, values_es(T)));
	cons ->
	    update_c_cons(T, map(F, cons_hd(T)),
			  map(F, cons_tl(T)));
 	tuple ->
	    update_c_tuple(T, map_list(F, tuple_es(T)));
 	'let' ->
	    update_c_let(T, map_list(F, let_vars(T)),
			 map(F, let_arg(T)),
			 map(F, let_body(T)));
	seq ->
 	    update_c_seq(T, map(F, seq_arg(T)),
			 map(F, seq_body(T)));
 	apply ->
	    update_c_apply(T, map(F, apply_op(T)),
			   map_list(F, apply_args(T)));
 	call ->
 	    update_c_call(T, map(F, call_module(T)),
			  map(F, call_name(T)),
			  map_list(F, call_args(T)));
 	primop ->
	    update_c_primop(T, map(F, primop_name(T)),
			    map_list(F, primop_args(T)));
 	'case' ->
 	    update_c_case(T, map(F, case_arg(T)),
			  map_list(F, case_clauses(T)));
 	clause ->
 	    update_c_clause(T, map_list(F, clause_pats(T)),
			    map(F, clause_guard(T)),
			    map(F, clause_body(T)));
 	alias ->
	    update_c_alias(T, map(F, alias_var(T)),
			   map(F, alias_pat(T)));
 	'fun' ->
	    update_c_fun(T, map_list(F, fun_vars(T)),
			 map(F, fun_body(T)));
 	'receive' ->
	    update_c_receive(T, map_list(F, receive_clauses(T)),
			     map(F, receive_timeout(T)),
			     map(F, receive_action(T)));
 	'try' ->
 	    update_c_try(T, map(F, try_expr(T)),
			 map_list(F, try_vars(T)),
			 map(F, try_body(T)));
 	'catch' ->
	    update_c_catch(T, map(F, catch_body(T)));
	binary ->
	    update_c_binary(T, map_list(F, binary_segs(T)));
	bin_seg ->
	    update_c_bin_seg(T, map(F, bin_seg_val(T)),
			     map(F, bin_seg_size(T)),
			     map(F, bin_seg_unit(T)),
			     map(F, bin_seg_type(T)),
			     map_list(F, bin_seg_flags(T)));
	letrec ->
	    update_c_letrec(T, map_pairs(F, letrec_defs(T)),
			    map(F, letrec_body(T)));
	module ->
	    update_c_module(T, map(F, module_name(T)),
			    map_list(F, module_exports(T)),
			    map_pairs(F, module_attrs(T)),
			    map_pairs(F, module_defs(T)))
    end.

map_list(F, [T | Ts]) ->
    [map(F, T) | map_list(F, Ts)];
map_list(F, []) ->
    [].

map_pairs(F, [{T1, T2} | Ps]) ->
    [{map(F, T1), map(F, T2)} | map_pairs(F, Ps)];
map_pairs(F, []) ->
    [].


%% =====================================================================
%% fold(Function, State, Tree) -> State1
%%
%%	   Function = (Tree, State) -> State1
%%	   Tree = coreErlang()
%%	   State = State1 = term()
%%	   
%%	The resulting `State1' is the value of `Function(X1,
%%	Function(X2, ... Function(Xn, State) ... ))', where `[X1, X2,
%%	..., Xn]' are the nodes of `Tree' in a post-order traversal.

fold(F, S, T) ->
    F(T, fold_1(F, S, T)).

fold_1(F, S, T) ->
    case type(T) of
 	literal ->
	    case concrete(T) of
		[_ | _] ->
		    fold(F, fold(F, S, cons_hd(T)), cons_tl(T));
		V when tuple(V), size(V) > 0 ->
		    fold_list(F, S, tuple_es(T));
		_ ->
		    S
	    end;
 	var ->
 	    S;
	values ->
 	    fold_list(F, S, values_es(T));
	cons ->
	    fold(F, fold(F, S, cons_hd(T)), cons_tl(T));
	tuple ->
	    fold_list(F, S, tuple_es(T));
 	'let' ->
	    fold(F, fold(F, fold_list(F, S, let_vars(T)),
			 let_arg(T)),
		 let_body(T));
	seq ->
	    fold(F, fold(F, S, seq_arg(T)), seq_body(T));
	apply ->
	    fold_list(F, fold(F, S, apply_op(T)), apply_args(T));
 	call ->
	    fold_list(F, fold(F, fold(F, S, call_module(T)),
			      call_name(T)),
		      call_args(T));
 	primop ->
	    fold_list(F, fold(F, S, primop_name(T)), primop_args(T));
 	'case' ->
	    fold_list(F, fold(F, S, case_arg(T)), case_clauses(T));
 	clause ->
	    fold(F, fold(F, fold_list(F, S, clause_pats(T)),
			 clause_guard(T)),
		 clause_body(T));
 	alias ->
	    fold(F, fold(F, S, alias_var(T)), alias_pat(T));
 	'fun' ->
	    fold(F, fold_list(F, S, fun_vars(T)), fun_body(T));
 	'receive' ->
	    fold(F, fold(F, fold_list(F, S, receive_clauses(T)),
			 receive_timeout(T)),
		 receive_action(T));
 	'try' ->
	    fold(F, fold_list(F, fold(F, S, try_expr(T)),
			      try_vars(T)),
		 try_body(T));
 	'catch' ->
	    fold(F, S, catch_body(T));
	binary ->
	    fold_list(F, S, binary_segs(T));
	bin_seg ->
	    fold_list(F,
		      fold(F,
			   fold(F,
				fold(F,
				     fold(F, S, bin_seg_val(T)),
				     bin_seg_size(T)),
				bin_seg_unit(T)),
			   bin_seg_type(T)),
		      bin_seg_flags(T));
	letrec ->
	    fold(F, fold_pairs(F, S, letrec_defs(T)), letrec_body(T));
	module ->
	    fold_pairs(F, 
		       fold_pairs(F, 
				  fold_list(F,
					    fold(F, S, module_name(T)),
					    module_exports(T)),
				  module_attrs(T)),
		       module_defs(T))
    end.

fold_list(F, S, [T | Ts]) ->
    fold_list(F, fold(F, S, T), Ts);
fold_list(F, S, []) ->
    S.

fold_pairs(F, S, [{T1, T2} | Ps]) ->
    fold_pairs(F, fold(F, fold(F, S, T1), T2), Ps);
fold_pairs(F, S, []) ->
    S.


%% =====================================================================
%% mapfold(Function, State, Tree) -> {Tree1, State1}
%%
%%	   Function = (Tree, State) -> {Tree1, State1}
%%	   Tree = Tree1 = coreErlang()
%%	   State = State1 = term()
%%
%%	This function is similar to `map', but also propagates a state
%%	value from each application of `Function' to the next, starting
%%	with the given `State', while doing a post-order traversal of
%%	the tree (much like `fold').

mapfold(F, S0, T) ->
    case type(T) of
 	literal ->
	    case concrete(T) of
		[_ | _] ->
		    {T1, S1} = mapfold(F, S0, cons_hd(T)),
		    {T2, S2} = mapfold(F, S1, cons_tl(T)),
		    F(update_c_cons(T, T1, T2), S2);
		V when tuple(V), size(V) > 0 ->
		    {Ts, S1} = mapfold_list(F, S0, tuple_es(T)),
		    F(update_c_tuple(T, Ts), S1);
		_ ->
		    F(T, S0)
	    end;
 	var ->
 	    F(T, S0);
	values ->
	    {Ts, S1} = mapfold_list(F, S0, values_es(T)),
 	    F(update_c_values(T, Ts), S1);
	cons ->
	    {T1, S1} = mapfold(F, S0, cons_hd(T)),
	    {T2, S2} = mapfold(F, S1, cons_tl(T)),
	    F(update_c_cons(T, T1, T2), S2);
 	tuple ->
	    {Ts, S1} = mapfold_list(F, S0, tuple_es(T)),
	    F(update_c_tuple(T, Ts), S1);
 	'let' ->
	    {Vs, S1} = mapfold_list(F, S0, let_vars(T)),
	    {A, S2} = mapfold(F, S1, let_arg(T)),
	    {B, S3} = mapfold(F, S2, let_body(T)),
	    F(update_c_let(T, Vs, A, B), S3);
	seq ->
	    {A, S1} = mapfold(F, S0, seq_arg(T)),
	    {B, S2} = mapfold(F, S1, seq_body(T)),
 	    F(update_c_seq(T, A, B), S2);
 	apply ->
	    {E, S1} = mapfold(F, S0, apply_op(T)),
	    {As, S2} = mapfold_list(F, S1, apply_args(T)),
	    F(update_c_apply(T, E, As), S2);
 	call ->
	    {M, S1} = mapfold(F, S0, call_module(T)),
	    {N, S2} = mapfold(F, S1, call_name(T)),
	    {As, S3} = mapfold_list(F, S2, call_args(T)),
 	    F(update_c_call(T, M, N, As), S3);
 	primop ->
	    {N, S1} = mapfold(F, S0, primop_name(T)),
	    {As, S2} = mapfold_list(F, S1, primop_args(T)),
	    F(update_c_primop(T, N, As), S2);
 	'case' ->
	    {A, S1} = mapfold(F, S0, case_arg(T)),
	    {Cs, S2} = mapfold_list(F, S1, case_clauses(T)),
 	    F(update_c_case(T, A, Cs), S2);
 	clause ->
	    {Ps, S1} = mapfold_list(F, S0, clause_pats(T)),
	    {G, S2} = mapfold(F, S1, clause_guard(T)),
	    {B, S3} = mapfold(F, S2, clause_body(T)),
	    F(update_c_clause(T, Ps, G, B), S3);
 	alias ->
	    {V, S1} = mapfold(F, S0, alias_var(T)),
	    {P, S2} = mapfold(F, S1, alias_pat(T)),
	    F(update_c_alias(T, V, P), S2);
 	'fun' ->
	    {Vs, S1} = mapfold_list(F, S0, fun_vars(T)),
	    {B, S2} = mapfold(F, S1, fun_body(T)),
	    F(update_c_fun(T, Vs, B), S2);
 	'receive' ->
	    {Cs, S1} = mapfold_list(F, S0, receive_clauses(T)),
	    {E, S2} = mapfold(F, S1, receive_timeout(T)),
	    {A, S3} = mapfold(F, S2, receive_action(T)),
	    F(update_c_receive(T, Cs, E, A), S3);
 	'try' ->
	    {E, S1} = mapfold(F, S0, try_expr(T)),
	    {Vs, S2} = mapfold_list(F, S1, try_vars(T)),
	    {B, S3} = mapfold(F, S2, try_body(T)),
	    F(update_c_try(T, E, Vs, B), S3);
 	'catch' ->
	    {B, S1} = mapfold(F, S0, catch_body(T)),
	    F(update_c_catch(T, B), S1);
	binary ->
	    {Ds, S1} = mapfold_list(F, S0, binary_segs(T)),
	    F(update_c_binary(T, Ds), S1);
	bin_seg ->
	    {Val, S1} = mapfold(F, S0, bin_seg_val(T)),
	    {Size, S2} = mapfold(F, S1, bin_seg_size(T)),
	    {Unit, S3} = mapfold(F, S2, bin_seg_unit(T)),
	    {Type, S4} = mapfold(F, S3, bin_seg_type(T)),
	    {Fs, S5} = mapfold_list(F, S4, bin_seg_flags(T)),
	    F(update_c_bin_seg(T, Val, Size, Unit, Type, Fs), S5);
	letrec ->
	    {Ds, S1} = mapfold_pairs(F, S0, letrec_defs(T)),
	    {B, S2} = mapfold(F, S1, letrec_body(T)),
	    F(update_c_letrec(T, Ds, B), S2);
	module ->
	    {N, S1} = mapfold(F, S0, module_name(T)),
	    {Es, S2} = mapfold_list(F, S1, module_exports(T)),
	    {As, S3} = mapfold_pairs(F, S2, module_attrs(T)),
	    {Ds, S4} = mapfold_pairs(F, S3, module_defs(T)),
	    F(update_c_module(T, N, Es, As, Ds), S4)
    end.

mapfold_list(F, S0, [T | Ts]) ->
    {T1, S1} = mapfold(F, S0, T),
    {Ts1, S2} = mapfold_list(F, S1, Ts),
    {[T1 | Ts1], S2};
mapfold_list(F, S, []) ->
    {[], S}.

mapfold_pairs(F, S0, [{T1, T2} | Ps]) ->
    {T3, S1} = mapfold(F, S0, T1),
    {T4, S2} = mapfold(F, S1, T2),
    {Ps1, S3} = mapfold_pairs(F, S2, Ps),
    {[{T3, T4} | Ps1], S3};
mapfold_pairs(F, S, []) ->
    {[], S}.


%% =====================================================================
%% variables(Tree) -> [Name]
%% free_variables(Tree) -> [Name]
%%
%%	    Tree = coreErlang()
%%	    Name = integer() | atom() | {atom(), integer()}
%%
%%	These functions return an ordered-set list of the names of all
%%	the (free) variables and function variables occurring in the
%%	syntax tree `Tree'. An exception is thrown if `Tree' does not
%%	represent a well-formed Core Erlang syntax tree.

variables(T) ->
    variables(T, false).

free_variables(T) ->
    variables(T, true).

%% This is not exported

variables(T, S) ->
    case type(T) of
	literal ->
	    [];
	var ->
	    [var_name(T)];
	values ->
	    vars_in_list(values_es(T), S);
	cons ->
	    ordsets:union(variables(cons_hd(T), S),
			  variables(cons_tl(T), S));
	tuple ->
	    vars_in_list(tuple_es(T), S);
	'let' ->
	    Vs = variables(let_body(T), S),
	    Vs1 = var_list_names(let_vars(T)),
	    Vs2 = case S of
		      true ->
			  ordsets:subtract(Vs, Vs1);
		      false ->
			  ordsets:union(Vs, Vs1)
		  end,
	    ordsets:union(variables(let_arg(T), S), Vs2);
	seq ->
	    ordsets:union(variables(seq_arg(T), S),
			  variables(seq_body(T), S));
	apply ->
	    ordsets:union(
	      variables(apply_op(T), S),
	      vars_in_list(apply_args(T), S));
	call ->
	    ordsets:union(variables(call_module(T), S),
			  ordsets:union(
			    variables(call_name(T), S),
			    vars_in_list(call_args(T), S)));
	primop ->
	    vars_in_list(primop_args(T), S);
	'case' ->
	    ordsets:union(variables(case_arg(T), S),
			  vars_in_list(case_clauses(T), S));
	clause ->
	    Vs = ordsets:union(variables(clause_guard(T), S),
			       variables(clause_body(T), S)),
	    Vs1 = vars_in_list(clause_pats(T), S),
	    case S of
		true ->
		    ordsets:subtract(Vs, Vs1);
		false ->
		    ordsets:union(Vs, Vs1)
	    end;
	alias ->
	    ordsets:add_element(var_name(alias_var(T)),
				variables(alias_pat(T)));
	'fun' ->
	    Vs = variables(fun_body(T), S),
	    Vs1 = var_list_names(fun_vars(T)),
	    case S of
		true ->
		    ordsets:subtract(Vs, Vs1);
		false ->
		    ordsets:union(Vs, Vs1)
	    end;
	'receive' ->
	    ordsets:union(
	      vars_in_list(receive_clauses(T), S),
	      ordsets:union(variables(receive_timeout(T), S),
			    variables(receive_action(T), S)));
	'try' ->
	    Vs = variables(try_body(T), S),
	    Vs1 = var_list_names(try_vars(T)),
	    Vs2 = ordsets:subtract(Vs, Vs1),
	    ordsets:union(variables(try_expr(T), S), Vs2);
	'catch' ->
	    variables(catch_body(T), S);
	binary ->
	    vars_in_list(binary_segs(T), S);
	bin_seg ->
	    ordsets:union(variables(bin_seg_val(T), S),
			  variables(bin_seg_size(T), S));
	letrec ->
	    Vs = vars_in_defs(letrec_defs(T), S),
	    Vs1 = ordsets:union(variables(letrec_body(T), S), Vs),
	    Vs2 = var_list_names(letrec_vars(T)),
	    case S of
		true ->
		    ordsets:subtract(Vs1, Vs2);
		false ->
		    ordsets:union(Vs1, Vs2)
	    end;
	module ->
	    Vs = vars_in_defs(module_defs(T), S),
	    Vs1 = ordsets:union(vars_in_list(module_exports(T), S), Vs),
	    Vs2 = var_list_names(module_vars(T)),
	    case S of
		true ->
		    ordsets:subtract(Vs1, Vs2);
		false ->
		    ordsets:union(Vs1, Vs2)
	    end
    end.

vars_in_list(Ts, S) ->
    vars_in_list(Ts, S, []).

vars_in_list([T | Ts], S, A) ->
    vars_in_list(Ts, S, ordsets:union(variables(T, S), A));
vars_in_list([], S, A) ->
    A.

%% Note that this function only visits the right-hand side of function
%% definitions.

vars_in_defs(Ds, S) ->
    vars_in_defs(Ds, S, []).

vars_in_defs([{_, F} | Ds], S, A) ->
    vars_in_defs(Ds, S, ordsets:union(variables(F, S), A));
vars_in_defs([], S, A) ->
    A.

%% This amounts to insertion sort. Since the lists are generally short,
%% it is hardly worthwhile to use an asymptotically better sort.

var_list_names(Vs) ->
    var_list_names(Vs, []).

var_list_names([V | Vs], A) ->
    var_list_names(Vs, ordsets:add_element(var_name(V), A));
var_list_names([], A) ->
    A.


%% =====================================================================
