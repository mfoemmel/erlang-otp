%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
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
%% Copyright 2006, 2007 Tobias Lindahl and Kostis Sagonas
%% 
%%     $Id$
%%

%%%-------------------------------------------------------------------
%%% File    : dialyzer_typesig.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description : 
%%%
%%% Created : 25 Apr 2005 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------
-module(dialyzer_typesig).

-include("dialyzer.hrl").
-include("dialyzer_callgraph.hrl").

-export([analyze_scc/4]).
-export([analyze_scc_get_all_fun_types/5]).
-export([get_safe_underapprox/2]).

-import(erl_types, 
	[t_any/0, t_atom/0, t_atom_vals/1, t_binary/0, t_bool/0, 
	 t_bitstr/2, t_bitstr_concat/1, t_collect_vars/1,
	 t_cons/2, t_cons_hd/1, t_cons_tl/1, t_float/0,
	 t_has_var/1, t_is_atom/1,
	 t_is_float/1, t_from_range/2, t_fun/0, t_fun/2, t_fun_args/1,
	 t_fun_range/1, t_is_fun/1,
	 t_inf/2, t_integer/0, t_is_integer/1, t_non_neg_integer/0,
	 t_is_atom/2, t_is_cons/1, t_is_equal/2, t_is_list/1,
	 t_is_nil/1, t_is_none/1, t_is_number/1,
	 t_is_any/1, t_is_subtype/2, t_limit/2, t_list/0, t_list/1, 
	 t_list_elements/1, t_nonempty_list/1, t_maybe_improper_list/0,
	 t_number/0, t_number_vals/1, t_pid/0, t_port/0,
	 t_product/1, t_ref/0, t_subst/2, t_subtract/2, t_subtract_list/2,
	 t_tuple/0, t_tuple/1, t_sup/1, t_sup/2,
	 t_unify/2, t_is_var/1, t_var/1, t_var_name/1, t_from_term/1, 
	 t_none/0, t_unit/0]).

-record(fun_var, {'fun'  :: fun((_) -> any()), 
		   deps  :: [integer()]}).

-type type_or_fun_var() :: any().

-record(constraint, {lhs   :: type_or_fun_var(),
		     op    :: 'eq' | 'sub', 
		     rhs   :: type_or_fun_var(), 
		     deps  :: [integer()]}).

-record(constraint_list, {type  :: 'conj' | 'disj', 
			  list  :: [any()], 
			  deps  :: [integer()], 
			  id    :: {'list', integer()}
			 }).

-type type_var() :: any().

-record(constraint_ref, {id    :: type_var(), 
			 deps  :: [integer()]}).

-record(state, {callgraph         :: #dialyzer_callgraph{},
		cs                :: [#constraint{} | #constraint_list{}
		                      | #constraint_ref{}],
		cmap              :: dict(),
		fun_map           :: [{_, _}], %% Orddict
		fun_arities       :: dict(), 
		in_match = false  :: bool(), 
		in_guard = false  :: bool(), 
		name_map          :: dict(), 
		next_label        :: integer(), 
		non_self_recs     :: [integer()], 
		plt               :: #dialyzer_plt{}, 
		prop_types        :: dict(),
		records           :: dict(), 
		scc = []          :: [type_var()]}).
		

-define(TYPE_LIMIT, 4).
-define(INTERNAL_TYPE_LIMIT, 5).

%-define(DEBUG, true).
%-define(DEBUG_CONSTRAINTS, true).
-ifdef(DEBUG).
-define(DEBUG_NAME_MAP, true).
-endif.
%-define(DEBUG_NAME_MAP, true).

-ifdef(DEBUG).
-define(debug(__String, __Args), io:format(__String, __Args)).
-else.
-define(debug(__String, __Args), ok).
-endif.

-include("dialyzer_bif_constraints.inc").

%% ============================================================================
%%
%%  The analysis.
%%
%% ============================================================================

%%-----------------------------------------------------------------------------
%% Analysis of strongly connected components.
%%
%% analyze_scc(SCC, NextLabel, CallGraph, PLT) -> [{MFA, Type}]
%%
%% SCC       - [{MFA, Def, Records}]
%%             where Def = {Var, Fun} as in the Core Erlang module definitions.
%%                   Records = dict(RecName, {Arity, [{FieldName, FieldType}]})
%%
%% NextLabel - An integer that is higher than any label in the code.
%%
%% CallGraph - A callgraph as produced by dialyzer_callgraph.erl 
%%             Note: The callgraph must have been built with all the 
%%                   code that the SCC is a part of.
%% PLT       - A dialyzer PLT. This PLT should contain available information
%%             about functions that can be called by this SCC.
%%-----------------------------------------------------------------------------

-type typesig_scc() :: [{mfa(), {_, _}, dict()}].
-type typesig_ret() :: [{mfa(),erl_type()}].

-spec analyze_scc(typesig_scc(), integer(), #dialyzer_callgraph{}, #dialyzer_plt{}) ->
	 typesig_ret().

analyze_scc(SCC, NextLabel, CallGraph, Plt) when is_integer(NextLabel) ->
  assert_format_of_scc(SCC),
  analyze_scc(SCC, NextLabel, CallGraph, Plt, dict:new()).

assert_format_of_scc([{_MFA, {_Var, _Fun}, _Records}|Left]) ->
  assert_format_of_scc(Left);
assert_format_of_scc([]) ->
  ok.

-spec analyze_scc_get_all_fun_types(typesig_scc(), integer(),
				    #dialyzer_callgraph{}, 
				    #dialyzer_plt{}, dict()) -> dict().

analyze_scc_get_all_fun_types(SCC, NextLabel, CallGraph, Plt, PropTypes) ->
  assert_format_of_scc(SCC),
  analyze_scc_1(SCC, NextLabel, CallGraph, Plt, PropTypes).

analyze_scc(SCC, NextLabel, CallGraph, Plt, PropTypes) ->
  TopMap = analyze_scc_1(SCC, NextLabel, CallGraph, Plt, PropTypes),
  [{MFA, lookup_type(mk_var(Fun), TopMap)} || {MFA, {_Var, Fun}, _Rec} <- SCC].

analyze_scc_1(SCC, NextLabel, CallGraph, Plt, PropTypes) ->
  State1 = new_state(SCC, NextLabel, CallGraph, Plt, PropTypes),
  DefSet = add_def_list([Var || {_MFA, {Var, _Fun}, _Rec} <- SCC], sets:new()),
  State2 = traverse_scc(SCC, DefSet, State1),
  State3 = state__finalize(State2),
  Funs = state__scc(State3),
  pp_constrs_scc(Funs, State3),
  constraints_to_dot_scc(Funs, State3),
  solve(Funs, State3).

%% ============================================================================
%%
%%  Gets the constraints by traversing the code.
%%
%% ============================================================================

traverse_scc([{_MFA, Def, Rec}|Left], DefSet, AccState) ->
  TmpState1 = state__set_rec_dict(AccState, Rec),
  DummyLetrec = cerl:c_letrec([Def], cerl:c_atom(foo)),
  {NewAccState, _} = traverse(DummyLetrec, DefSet, TmpState1),
  traverse_scc(Left, DefSet, NewAccState);
traverse_scc([], _DefSet, AccState) ->
  AccState.

traverse(Tree, DefinedVars, State) ->
  ?debug("Handling ~p\n", [cerl:type(Tree)]),
  case cerl:type(Tree) of
    alias ->
      Var = cerl:alias_var(Tree),
      Pat = cerl:alias_pat(Tree),
      DefinedVars1 = add_def(Var, DefinedVars),
      {State1, PatVar} = traverse(Pat, DefinedVars1, State),
      State2 = state__store_conj(mk_var(Var), eq, PatVar, State1),
      {State2, PatVar};
    apply ->
      Args = cerl:apply_args(Tree),
      Arity = length(Args),
      Op = cerl:apply_op(Tree),
      {State0, ArgTypes} = traverse_list(Args, DefinedVars, State),      
      {State1, OpType} = traverse(Op, DefinedVars, State0),
      {State2, FunType} = state__get_fun_prototype(OpType, Arity, State1),
      State3 = state__store_conj(FunType, eq, OpType, State2),
      State4 = state__store_conj(mk_var(Tree), sub, t_fun_range(FunType), 
				 State3),
      State5 = state__store_conj_lists(ArgTypes, sub, t_fun_args(FunType),
				       State4),
      case state__lookup_apply(Tree, State) of
	unknown ->
	  {State5, mk_var(Tree)};
	FunLabels ->
	  case get_apply_constr(FunLabels, mk_var(Tree), ArgTypes, State5) of
	    error -> {State5, mk_var(Tree)};
	    {ok, State6} -> {State6, mk_var(Tree)}
	  end
      end;
    binary ->
      {State1, SegTypes} = traverse_list(cerl:binary_segments(Tree), 
					 DefinedVars, State),
      Type = mk_fun_var(fun(Map) ->
			    TmpSegTypes = lookup_type_list(SegTypes, Map),
			    t_bitstr_concat(TmpSegTypes)
			end, SegTypes),
      {state__store_conj(mk_var(Tree), sub, Type, State1), mk_var(Tree)};
    bitstr ->
      Size = cerl:bitstr_size(Tree),
      UnitVal = cerl:int_val(cerl:bitstr_unit(Tree)),
      Val = cerl:bitstr_val(Tree),
      {State1, [SizeType, ValType]} = 
	traverse_list([Size, Val], DefinedVars, State),
      {State2, TypeConstr} =
	case cerl:is_literal(Size) of
	  true ->
	    case cerl:concrete(Size) of
	      all -> {State1, t_bitstr(UnitVal, 0)};
	      N when is_integer(N) -> {State1, t_bitstr(0, UnitVal * N)}
	    end;
	  false ->
	    {state__store_conj(SizeType, sub, t_non_neg_integer(), State1),
	     mk_fun_var(bitstr_constr(SizeType, UnitVal), [SizeType])}
	end,
      ValTypeConstr =
	case cerl:concrete(cerl:bitstr_type(Tree)) of
	  float ->
	    case state__is_in_match(State1) of
	      true -> t_float();
	      false -> t_number()
	    end;
	  binary -> TypeConstr;
	  integer ->
	    case state__is_in_match(State1) of
	      false -> t_integer();
	      true ->
		Flags = cerl:concrete(cerl:bitstr_flags(Tree)),
		mk_fun_var(bitstr_val_constr(SizeType, UnitVal, Flags), 
			   [SizeType])
	    end
	end,
      State3 = state__store_conj(ValType, sub, ValTypeConstr, State2),
      State4 = state__store_conj(mk_var(Tree), sub, TypeConstr, State3),
      {State4, mk_var(Tree)};
    'case' ->
      Arg = cerl:case_arg(Tree),
      Clauses = filter_match_fail(cerl:case_clauses(Tree)),
      {State1, ArgVar} = traverse(Arg, DefinedVars, State),
      handle_clauses(Clauses, mk_var(Tree), ArgVar, DefinedVars, State1);
    call ->
      handle_call(Tree, DefinedVars, State);
    'catch' ->
      %% XXX: Perhaps there is something to say about this.
      {State, mk_var(Tree)};
    cons ->
      Hd = cerl:cons_hd(Tree),
      Tl = cerl:cons_tl(Tree),
      {State1, [HdVar, TlVar]} = traverse_list([Hd, Tl], DefinedVars, State),
      case cerl:is_literal(cerl:fold_literal(Tree)) of
	true ->
	  %% We do not need to do anything more here.
	  {State, t_cons(HdVar, TlVar)};
	false ->
	  ConsVar = mk_var(Tree),
	  ConsType = mk_fun_var(fun(Map)->
				    t_cons(lookup_type(HdVar, Map), 
					   lookup_type(TlVar, Map))
				end, [HdVar, TlVar]),
	  
	  HdType = mk_fun_var(fun(Map)->
				  Cons = lookup_type(ConsVar, Map),
				  case t_is_cons(Cons) of
				    false -> t_any();
				    true -> t_cons_hd(Cons)
				  end
			      end, [ConsVar]),
	  TlType = mk_fun_var(fun(Map)->
				  Cons = lookup_type(ConsVar, Map),
				  case t_is_cons(Cons) of
				    false -> t_any();
				    true -> t_cons_tl(Cons)
				  end
			      end, [ConsVar]),
	  
	  State2 = state__store_conj_lists([HdVar, TlVar, ConsVar], sub, 
					   [HdType, TlType, ConsType], 
					   State1),
	  {State2, ConsVar}
      end;
    'fun' ->
      Body = cerl:fun_body(Tree),
      Vars = cerl:fun_vars(Tree),
      DefinedVars1 = add_def_list(Vars, DefinedVars),
      State0 = state__new_constraint_context(State),
      FunFailType =
	case state__prop_domain(Tree, State0) of
	  error -> t_fun(length(Vars), t_none());
	  {ok, Dom} -> t_fun(Dom, t_none())
	end,
      State2 = 
	try
	  State1 = case state__add_prop_constrs(Tree, State0) of
		     not_called -> State0;
		     PropState -> PropState
		   end,
	  {BodyState, BodyVar} = traverse(Body, DefinedVars1, State1),
	  state__store_conj(mk_var(Tree), eq, 
			    t_fun(mk_var_list(Vars), BodyVar), BodyState)
	catch
	  throw:error ->
	    state__store_conj(mk_var(Tree), eq, FunFailType, State0)
	end,
      Cs = state__cs(State2),
      State3 = state__store_constrs(mk_var(Tree), Cs, State2),
      Ref = mk_constraint_ref(mk_var(Tree), get_deps(Cs)),
      OldCs = state__cs(State),
      State4 = state__new_constraint_context(State3),
      State5 = state__store_conj_list([OldCs, Ref], State4),
      State6 = state__store_fun_arity(Tree, State5),
      {State6, mk_var(Tree)};
    'let' ->
      Vars = cerl:let_vars(Tree),
      Arg = cerl:let_arg(Tree),
      Body = cerl:let_body(Tree),
      {State1, ArgVars} = traverse(Arg, DefinedVars, State),
      State2 = state__store_conj(t_product(mk_var_list(Vars)), eq, 
				 ArgVars, State1),
      DefinedVars1 = add_def_list(Vars, DefinedVars),
      traverse(Body, DefinedVars1, State2);
    letrec ->
      Defs = cerl:letrec_defs(Tree),
      Body = cerl:letrec_body(Tree),
      Funs = [Fun || {_Var, Fun} <- Defs],
      Vars = [Var || {Var, _Fun} <- Defs],
      
      State1 = state__store_funs(Vars, Funs, State),
      DefinedVars1 = add_def_list(Vars, DefinedVars),
      {State2, _} = traverse_list(Funs, DefinedVars1, State1),
      traverse(Body, DefinedVars1, State2);
    literal ->      
      %% This is needed for finding records
      case cerl:unfold_literal(Tree) of
	Tree -> {State, t_from_term(cerl:concrete(Tree))};
	NewTree -> traverse(NewTree, DefinedVars, State)
      end;
    module ->
      Defs = cerl:module_defs(Tree),
      Funs = [Fun || {_Var, Fun} <- Defs],
      Vars = [Var || {Var, _Fun} <- Defs],
      DefinedVars1 = add_def_list(Vars, DefinedVars),      
      State1 = state__store_funs(Vars, Funs, State),
      FoldFun = fun(Fun, AccState) ->
		    {S, _} = traverse(Fun, DefinedVars1,
				      state__new_constraint_context(AccState)),
		    S
		end,
      lists:foldl(FoldFun, State1, Funs);
    primop ->
      case cerl:atom_val(cerl:primop_name(Tree)) of
	match_fail -> throw(error);
	raise -> throw(error);
	bs_init_writable -> {State, t_from_term(<<>>)};
	Other -> erlang:error({'Unsupported primop', Other})
      end;
    'receive' ->
      Clauses = filter_match_fail(cerl:receive_clauses(Tree)),
      Timeout = cerl:receive_timeout(Tree),
      case (cerl:is_c_atom(Timeout) andalso 
	    (cerl:atom_val(Timeout) =:= infinity)) of
	false ->
	  Action = cerl:receive_action(Tree),
	  {State1, TimeoutVar} = traverse(Timeout, DefinedVars, State),
	  TimeoutType = t_sup(t_from_term(infinity), t_integer()),
	  State2 = state__store_conj(TimeoutVar, sub, TimeoutType, State1),
	  handle_clauses(Clauses, mk_var(Tree), [], Action, DefinedVars,State2);
	true ->
	  handle_clauses(Clauses, mk_var(Tree), [], DefinedVars, State)
      end;
    seq ->
      Body = cerl:seq_body(Tree),
      Arg = cerl:seq_arg(Tree),
      {State1, _} = traverse(Arg, DefinedVars, State),
      traverse(Body, DefinedVars, State1);
    'try' ->
      handle_try(Tree, DefinedVars, State);
    tuple ->
      Elements = cerl:tuple_es(Tree),
      {State1, EVars} = traverse_list(Elements, DefinedVars, State),
      {State2, TupleType} =
	case cerl:is_literal(cerl:fold_literal(Tree)) of
	  true ->
	    %% We do not need to do anything more here.
	    {State, t_tuple(EVars)};
	  false ->
	    %% We have the same basic problem as in products, but we want to
	    %% make sure that everything that can be used as tags for the
	    %% disjoint unions stays in the tuple.
	    Fun = fun(Var, AccState) ->
		      case t_has_var(Var) of
			true ->
			  {AccState1, NewVar} = state__mk_var(AccState),
			  {NewVar, 
			   state__store_conj(Var, eq, NewVar, AccState1)};
			false ->
			  {Var, AccState}
		      end
		  end,
	    {NewEvars, TmpState} = lists:mapfoldl(Fun, State1, EVars),
	    {TmpState, t_tuple(NewEvars)}
	end,
      State3 =
	case Elements of
	  [Tag|Fields] -> 
	    case cerl:is_c_atom(Tag) of
	      true ->
		Arity = length(Fields),
		case state__lookup_record(State2, cerl:atom_val(Tag), Arity) of
		  error -> State2;
		  {ok, RecType} -> state__store_conj(TupleType, sub, 
						     RecType, State2)
		end;
	      false -> State2
	    end;
	  [] -> State2
	end,
      {State3, TupleType};
    values ->
      %% We can get into trouble when unifying products that have the
      %% same element appearing several times. Handle these cases by
      %% introducing fresh variables and constraining them to be equal
      %% to the original ones. This is similar to what happens in
      %% pattern matching where the matching is done on fresh
      %% variables and guards assert that the matching is correct.
      Elements = cerl:values_es(Tree),
      {State1, EVars} = traverse_list(Elements, DefinedVars, State),
      Arity = length(EVars),
      Unique = length(ordsets:from_list(EVars)),
      case Arity =:= Unique of
	true -> {State1, t_product(EVars)};
	false ->
	  {State2, Vars} = state__mk_vars(Arity, State1),
	  State3 = state__store_conj_lists(Vars, eq, EVars, State2),
	  {State3, t_product(Vars)}
      end;
    var ->
      case is_def(Tree, DefinedVars) of
	true -> {State, mk_var(Tree)};
	false ->
	  %% If we are analyzing sccs this can be a function variable.
	  case state__lookup_undef_var(Tree, State) of
	    error -> erlang:error({'Undefined variable', Tree});
	    {ok, Type} ->
	      {State1, NewVar} = state__mk_var(State),
	      {state__store_conj(NewVar, sub, Type, State1), NewVar}
	  end
      end;
    Other ->
      erlang:error({'Unsupported type', Other})
  end.

traverse_list(Trees, DefinedVars, State) ->
  traverse_list(Trees, DefinedVars, State, []).

traverse_list([Tree|Tail], DefinedVars, State, Acc) ->
  {State1, Var} = traverse(Tree, DefinedVars, State),
  traverse_list(Tail, DefinedVars, State1, [Var|Acc]);
traverse_list([], _DefinedVars, State, Acc) ->
  {State, lists:reverse(Acc)}.

add_def(Var, Set) ->
  sets:add_element(cerl_trees:get_label(Var), Set).

add_def_list([H|T], Set) ->
  add_def_list(T, add_def(H, Set));
add_def_list([], Set) ->
  Set.

add_def_from_tree(T, DefinedVars) ->
  Vars = cerl_trees:fold(fun(X, Acc) ->
			     case cerl:is_c_var(X) of
			       true -> [X|Acc];
			       false -> Acc
			     end
			 end, [], T),
  add_def_list(Vars, DefinedVars).

add_def_from_tree_list([H|T], DefinedVars) ->
  add_def_from_tree_list(T, add_def_from_tree(H, DefinedVars));
add_def_from_tree_list([], DefinedVars) ->
  DefinedVars.

is_def(Var, Set) ->
  sets:is_element(cerl_trees:get_label(Var), Set).

%%________________________________________
%%
%% Try
%%

handle_try(Tree, DefinedVars, State) ->
  Arg = cerl:try_arg(Tree),
  Vars = cerl:try_vars(Tree),
  EVars = cerl:try_evars(Tree),
  Body = cerl:try_body(Tree),
  Handler = cerl:try_handler(Tree),
  
  State1 = state__new_constraint_context(State),
  {ArgBodyState, BodyVar} =
    try 
      {State2, ArgVar} = traverse(Arg, DefinedVars, State1),
      DefinedVars1 = add_def_list(Vars, DefinedVars),
      {State3, BodyVar1} = traverse(Body, DefinedVars1, State2),
      State4 = state__store_conj(t_product(mk_var_list(Vars)), eq, ArgVar,
				 State3),
      {State4, BodyVar1}
    catch
      throw:error -> 
	{State1, t_none()}
    end,
  State6 = state__new_constraint_context(ArgBodyState),
  {HandlerState, HandlerVar} =
    try
      DefinedVars2 = add_def_list([X || X <- EVars, cerl:is_c_var(X)], 
				  DefinedVars),
      traverse(Handler, DefinedVars2, State6)
    catch
      throw:error -> 
	{State6, t_none()}
    end,
  ArgBodyCs = state__cs(ArgBodyState),
  HandlerCs = state__cs(HandlerState),
  TreeVar = mk_var(Tree),
  OldCs = state__cs(State),
  case state__is_in_guard(State) of
    true ->
      Conj1 = mk_conj_constraint_list([ArgBodyCs, 
				       mk_constraint(BodyVar, eq, TreeVar)]),
      Disj = mk_disj_constraint_list([Conj1,
				      mk_constraint(HandlerVar, eq, TreeVar)]),
      NewState1 = state__new_constraint_context(HandlerState),
      Conj2 = mk_conj_constraint_list([OldCs, Disj]),
      NewState2 = state__store_conj(Conj2, NewState1),
      {NewState2, TreeVar};
    false ->
      {NewCs, ReturnVar} =
	case {t_is_none(BodyVar), t_is_none(HandlerVar)} of
	  {false, false} ->
	    Conj1 = 
	      mk_conj_constraint_list([ArgBodyCs,
				       mk_constraint(TreeVar, eq, BodyVar)]),
	    Conj2 = 
	      mk_conj_constraint_list([HandlerCs,
				       mk_constraint(TreeVar, eq, HandlerVar)]),
	    Disj = mk_disj_constraint_list([Conj1, Conj2]),
	    {Disj, mk_var(Tree)};
	  {false, true} ->
	    {mk_conj_constraint_list([ArgBodyCs,
				      mk_constraint(TreeVar, eq, BodyVar)]),
	     BodyVar};
	  {true, false} ->
	    {mk_conj_constraint_list([HandlerCs,
				      mk_constraint(TreeVar, eq, HandlerVar)]),
	     HandlerVar};
	  {true, true} ->
	    ?debug("Throw failed\n", []),
	    throw(error)
	end,
      Conj = mk_conj_constraint_list([OldCs, NewCs]),
      NewState1 = state__new_constraint_context(HandlerState),
      NewState2 = state__store_conj(Conj, NewState1),
      {NewState2, ReturnVar}
  end.
  
%%________________________________________
%%
%% Call
%%

handle_call(Call, DefinedVars, State) ->      
  Args = cerl:call_args(Call),
  Mod = cerl:call_module(Call),
  Fun = cerl:call_name(Call),
  Dst = mk_var(Call),
  case cerl:is_c_atom(Mod) andalso cerl:is_c_atom(Fun) of
    true ->
      M = cerl:atom_val(Mod),
      F = cerl:atom_val(Fun),
      A = length(Args),      
      {State1, ArgVars} = traverse_list(Args, DefinedVars, State),
      case state__lookup_rec_var_in_scope({M, F, A}, State) of
	error ->
	  case get_bif_constr({M, F, A}, Dst, ArgVars, State1) of
	    none -> 
	      {get_plt_constr({M, F, A}, Dst, ArgVars, State1), Dst};
	    C ->
	      {state__store_conj(C, State1), Dst}
	  end;
	{ok, Var} ->
	  %% This is part of the scc currently analyzed.
	  %% Intercept and change this to an apply instead.
	  ?debug("Found the call to ~w\n", [{M, F, A}]),
	  Label = cerl_trees:get_label(Call),
	  Apply = cerl:ann_c_apply([{label, Label}], Var, Args),
	  traverse(Apply, DefinedVars, State)
      end;
    false ->
      {State1, MF} = traverse_list([Mod, Fun], DefinedVars, State),
      {state__store_conj_lists(MF, sub, [t_atom(), t_atom()], State1), Dst}
  end.

get_plt_constr(MFA, Dst, ArgVars, State) ->
  Plt = state__plt(State),
  PltRes = dialyzer_plt:lookup(Plt, MFA),
  case dialyzer_plt:lookup_contract(Plt, MFA) of
    none ->
      case PltRes of
	none -> State;
	{value, {PltRetType, PltArgTypes}} ->
	  state__store_conj_lists([Dst|ArgVars], sub, 
				  [PltRetType|PltArgTypes], State)
      end;
    {value, C = #contract{args=GenArgs}} ->
      {RetType, ArgCs} =
	case PltRes of
	  none ->
	    {mk_fun_var(fun(Map) ->
			    ArgTypes = lookup_type_list(ArgVars, Map), 
			    dialyzer_contracts:get_contract_return(C, ArgTypes)
			end, [ArgVars]), GenArgs};
	  {value, {PltRetType, PltArgTypes}} ->
	    %% Need to combine the contract with the success typing.
	    {mk_fun_var(
	       fun(Map) ->
		   ArgTypes = lookup_type_list(ArgVars, Map),
		   CRet = dialyzer_contracts:get_contract_return(C, ArgTypes),
		   t_inf(CRet, PltRetType)
	       end, [ArgVars]),
	     [t_inf(X, Y) || {X, Y} <- lists:zip(GenArgs, PltArgTypes)]}
	end,
      state__store_conj_lists([Dst|ArgVars], sub, [RetType|ArgCs], State)
  end.

filter_match_fail([Clause]) ->
  Body = cerl:clause_body(Clause),
  case cerl:type(Body) of
    primop ->
      case cerl:atom_val(cerl:primop_name(Body)) of
	match_fail -> [];
	raise -> [];
	_ -> [Clause]
      end;
    _ -> [Clause]
  end;
filter_match_fail([H|T]) ->
  [H|filter_match_fail(T)];
filter_match_fail([]) ->
  %% This can actually happen, for example in 
  %%      receive after 1 -> ok end
  [].

%% If the number of clauses is too big, we cannot apply the
%% list-subtraction scheme since it causes the analysis to be too
%% slow.  Typically, this only affects huge, automatically generated
%% files anyway and the dataflow analysis doesn't suffer from this, so
%% we will get some information anyway.
-define(MAX_NOF_CLAUSES, 15).

handle_clauses(Clauses, TopVar, Arg, DefinedVars, State) ->
  handle_clauses(Clauses, TopVar, Arg, none, DefinedVars, State).

handle_clauses([], _, _, Action, DefinedVars, State) when Action =/= none ->
  %% Can happen when a receive has no clauses, see filter_match_fail.
  traverse(Action, DefinedVars, State);
handle_clauses(Clauses, TopVar, Arg, Action, DefinedVars, State) ->
  SubtrTypeList =
    if length(Clauses) > ?MAX_NOF_CLAUSES -> overflow;
       true -> []
    end,
  {State1, CList} = handle_clauses_1(Clauses, TopVar, Arg, DefinedVars, 
				     State, SubtrTypeList, []),
  {NewCs, NewState} =
    case Action of
      none -> 
	if CList =:= [] -> throw(error);
	   true -> {CList, State1}
	end;
      _ -> 
	try 
	  {State2, ActionVar} = traverse(Action, DefinedVars, State1),
	  TmpC = mk_constraint(TopVar, eq, ActionVar),
	  ActionCs = mk_conj_constraint_list([state__cs(State2),TmpC]),
	  {[ActionCs|CList], State2}
	catch
	  throw:error ->
	    if CList =:= [] -> throw(error);
	       true -> {CList, State1}
	    end
	end
    end,
  OldCs = state__cs(State),
  NewCList = mk_disj_constraint_list(NewCs),
  FinalState = state__new_constraint_context(NewState),
  {state__store_conj_list([OldCs, NewCList], FinalState), TopVar}.

handle_clauses_1([Clause|Tail], TopVar, Arg, DefinedVars, 
		 State, SubtrTypes, Acc) ->
  State0 = state__new_constraint_context(State),
  Pats = cerl:clause_pats(Clause),
  Guard = cerl:clause_guard(Clause),
  Body = cerl:clause_body(Clause),
  NewSubtrTypes =
    case SubtrTypes =:= overflow of
      true -> overflow;
      false -> 
	ordsets:add_element(get_safe_underapprox(Pats, Guard), SubtrTypes)
    end,
  try 
    DefinedVars1 = add_def_from_tree_list(Pats, DefinedVars),
    State1 = state__set_in_match(State0, true),
    {State2, PatVars} = traverse_list(Pats, DefinedVars1, State1),
    State3 =
      if Arg =:= [] -> State2;
         true -> 
	  S = state__store_conj(Arg, eq, t_product(PatVars), State2),
	  case SubtrTypes =:= overflow of
	    true -> S;
	    false ->
	      SubtrPatVar = mk_fun_var(fun(Map) -> 
					   TmpType = lookup_type(Arg, Map),
					   t_subtract_list(TmpType, SubtrTypes)
				       end, [Arg]),
	      state__store_conj(Arg, sub, SubtrPatVar, S)
	  end
      end,
    State4 = handle_guard(Guard, DefinedVars1, State3),
    {State5, BodyVar} = traverse(Body, DefinedVars1, 
				 state__set_in_match(State4, false)),
    State6 = state__store_conj(TopVar, eq, BodyVar, State5),
    Cs = state__cs(State6),
    handle_clauses_1(Tail, TopVar, Arg, DefinedVars, State6, 
		     NewSubtrTypes, [Cs|Acc])
  catch
    throw:error -> 
      handle_clauses_1(Tail, TopVar, Arg, DefinedVars, 
		       State, NewSubtrTypes, Acc)
  end;
handle_clauses_1([], _TopVar, _Arg, _DefinedVars, State, _SubtrType, Acc) ->
  {state__new_constraint_context(State), Acc}.

-spec get_safe_underapprox([_], core_tree()) -> erl_type().

get_safe_underapprox(Pats, Guard) ->
  try
    Map1 = cerl_trees:fold(fun(X, Acc) ->
			       case cerl:is_c_var(X) of
				 true -> 
				   dict:store(cerl_trees:get_label(X), t_any(),
					      Acc);
				 false -> Acc
			       end
			   end, dict:new(), cerl:c_values(Pats)),
    {Type, Map2} = get_underapprox_from_guard(Guard, Map1),
    Map3 = case t_is_none(t_inf(t_from_term(true), Type)) of
	     true -> throw(dont_know);
	     false ->
	       case cerl:is_c_var(Guard) of
		 false -> Map2;
		 true -> 
		   dict:store(cerl_trees:get_label(Guard), 
			      t_from_term(true), Map2)
	       end
	   end,
    {Ts, _Map4} = get_safe_underapprox_1(Pats, [], Map3),
    t_product(Ts)
  catch
    throw:dont_know -> t_none()
  end.

get_underapprox_from_guard(Tree, Map) ->
  True = t_from_term(true),
  case cerl:type(Tree) of
    call ->
      case {cerl:concrete(cerl:call_module(Tree)), 
	    cerl:concrete(cerl:call_name(Tree)), 
	    length(cerl:call_args(Tree))} of
	{erlang, is_function, 2} ->
	  [Fun, Arity] = cerl:call_args(Tree),
	  case cerl:is_c_int(Arity) of
	    false -> throw(dont_know);
	    true -> 
	      {FunType, Map1} = get_underapprox_from_guard(Fun, Map),
	      Inf = t_inf(FunType, t_fun(cerl:int_val(Arity), t_any())),
	      case t_is_none(Inf) of
		true -> throw(dont_know);
		false -> {True, dict:store(cerl_trees:get_label(Fun), Inf, Map1)}
	      end
	  end;
	MFA ->
	  case get_type_test(MFA) of
	    {ok, Type} ->
	      [Arg] = cerl:call_args(Tree),
	      {ArgType, Map1} = get_underapprox_from_guard(Arg, Map),
	      Inf = t_inf(Type, ArgType),
	      case t_is_none(Inf) of
		true -> throw(dont_know);
		false ->
		  case cerl:is_literal(Arg) of
		    true -> {True, Map1};
		    false ->
		      {True, dict:store(cerl_trees:get_label(Arg), Inf, Map1)}
		  end
	      end;
	    error ->
	      case MFA of
		{erlang, '=:=', 2} -> throw(dont_know);
		{erlang, '==', 2} -> throw(dont_know);
		{erlang, 'and', 2} ->
		  [Arg1, Arg2] = cerl:call_args(Tree),
		  case ((cerl:is_c_var(Arg1) orelse cerl:is_literal(Arg1)) 
			andalso
			(cerl:is_c_var(Arg2) orelse cerl:is_literal(Arg2))) of
		    true ->
		      {Arg1Type, _} = get_underapprox_from_guard(Arg1, Map),
		      {Arg2Type, _} = get_underapprox_from_guard(Arg1, Map),
		      case (t_is_equal(True, Arg1Type) andalso 
			    t_is_equal(True, Arg2Type)) of
			true -> {True, Map}; 
			false -> throw(dont_know)
		      end;
		    false ->
		      throw(dont_know)
		  end;
		{erlang, 'or', 2} -> throw(dont_know);
		_ -> throw(dont_know)
	      end
	  end
      end;
    var ->
      Type = 
	case dict:find(cerl_trees:get_label(Tree), Map) of
	  error -> throw(dont_know);
	  {ok, T} -> T
	end,
      {Type, Map};
    literal ->
      case cerl:unfold_literal(Tree) of
	Tree ->
	  Type =
	    case cerl:concrete(Tree) of
	      Int when is_integer(Int) -> t_from_term(Int);
	      Atom when is_atom(Atom) -> t_from_term(Atom);
	      _Other -> throw(dont_know)
	    end,
	  {Type, Map};
	OtherTree ->
	  get_underapprox_from_guard(OtherTree, Map)
      end;
    _ ->
      throw(dont_know)
  end.

get_type_test({erlang, is_atom, 1}) ->      {ok, t_atom()};
get_type_test({erlang, is_boolean, 1}) ->   {ok, t_bool()};
get_type_test({erlang, is_binary, 1}) ->    {ok, t_binary()};
get_type_test({erlang, is_float, 1}) ->     {ok, t_float()};
get_type_test({erlang, is_function, 1}) ->  {ok, t_fun()};
get_type_test({erlang, is_integer, 1}) ->   {ok, t_integer()};
get_type_test({erlang, is_list, 1}) ->      {ok, t_list()};
get_type_test({erlang, is_number, 1}) ->    {ok, t_number()};
get_type_test({erlang, is_pid, 1}) ->       {ok, t_pid()};
get_type_test({erlang, is_port, 1}) ->      {ok, t_port()};
get_type_test({erlang, is_reference, 1}) -> {ok, t_ref()};
get_type_test({erlang, is_tuple, 1}) ->     {ok, t_tuple()};
get_type_test({M, F, A}) when is_atom(M), is_atom(F), is_integer(A) -> error.


bitstr_constr(SizeType, UnitVal) ->
  fun(Map) ->
      TmpSizeType = lookup_type(SizeType, Map),
      case t_is_subtype(TmpSizeType, t_non_neg_integer()) of
	true ->
	  case t_number_vals(TmpSizeType) of
	    [OneSize] -> t_bitstr(0, OneSize * UnitVal);
	    _ ->
	      MinSize = erl_types:number_min(TmpSizeType),
	      t_bitstr(UnitVal, MinSize * UnitVal)
	  end;
	false -> 
	  t_bitstr(UnitVal, 0)
      end
  end.

bitstr_val_constr(SizeType, UnitVal, Flags) ->
  fun(Map) ->
      TmpSizeType = lookup_type(SizeType, Map),
      case t_is_subtype(TmpSizeType, t_non_neg_integer()) of
	true ->
	  case erl_types:number_max(TmpSizeType) of
	    N when is_integer(N), N < 128 -> %% Avoid illegal arithmetic
	      TotalSizeVal = N * UnitVal,
	      {RangeMin, RangeMax} =
		case lists:member(signed, Flags) of
		  true -> {-(1 bsl (TotalSizeVal - 1)),
			   1 bsl (TotalSizeVal - 1) - 1};
		  false -> {0,1 bsl TotalSizeVal - 1}
		end,
	      t_from_range(RangeMin, RangeMax);
	    _ -> t_integer()
	  end;
	_ -> 
	  t_integer()
      end
  end.
	

get_safe_underapprox_1([Pat|Left], Acc, Map) ->
  case cerl:type(Pat) of
    alias ->
      APat = cerl:alias_pat(Pat),
      AVar = cerl:alias_var(Pat),
      {[VarType], Map1} = get_safe_underapprox_1([AVar], [], Map),
      {[PatType], Map2} = get_safe_underapprox_1([APat], [], Map1),
      Inf = t_inf(VarType, PatType),
      case t_is_none(Inf) of
	true -> throw(dont_know);
	false ->
	  Map3 = dict:store(cerl_trees:get_label(AVar), Inf, Map2),
	  get_safe_underapprox_1(Left, [Inf|Acc], Map3)
      end;
    binary ->
      %% TODO: Can maybe do something here
      throw(dont_know);      
    cons ->
      {[Hd, Tl], Map1} = 
	get_safe_underapprox_1([cerl:cons_hd(Pat), cerl:cons_tl(Pat)], [], Map),
      case t_is_any(Tl) of
	true -> get_safe_underapprox_1(Left, [t_nonempty_list(Hd)|Acc], Map1);
	false -> throw(dont_know)
      end;
    literal ->
      case cerl:unfold_literal(Pat) of
	Pat ->
	  Type =
	    case cerl:concrete(Pat) of
	      Int when is_integer(Int) -> t_from_term(Int);
	      Atom when is_atom(Atom) -> t_from_term(Atom);
	      [] -> t_from_term([]);
	      _Other -> throw(dont_know)
	    end,
	  get_safe_underapprox_1(Left, [Type|Acc], Map);
	OtherPat ->
	  get_safe_underapprox_1([OtherPat|Left], Acc, Map)
      end;
    tuple ->
      Es = cerl:tuple_es(Pat),
      {Ts, Map1} = get_safe_underapprox_1(Es, [], Map),
      Type = t_tuple(Ts),
      get_safe_underapprox_1(Left, [Type|Acc], Map1);
    values ->
      Es = cerl:values_es(Pat),
      {Ts, Map1} = get_safe_underapprox_1(Es, [], Map),
      Type = t_product(Ts),
      get_safe_underapprox_1(Left, [Type|Acc], Map1);
    var ->
      case dict:find(cerl_trees:get_label(Pat), Map) of
	error -> throw(dont_know);
	{ok, VarType} -> get_safe_underapprox_1(Left, [VarType|Acc], Map)
      end
  end;
get_safe_underapprox_1([], Acc, Map) ->
  {lists:reverse(Acc), Map}.

%%________________________________________
%%
%% Guards
%%

handle_guard(Guard, DefinedVars, State) ->  
  True = t_from_term(true),
  State1 = state__set_in_guard(State, true),
  State2 = state__new_constraint_context(State1),
  {State3, Return} = traverse(Guard, DefinedVars, State2),
  State4 = state__store_conj(Return, eq, True, State3),
  Cs = state__cs(State4),
  NewCs = mk_disj_norm_form(Cs),
  OldCs = state__cs(State),
  State5 = state__set_in_guard(State4, state__is_in_guard(State)),
  State6 = state__new_constraint_context(State5),
  state__store_conj(mk_conj_constraint_list([OldCs, NewCs]), State6).


%%=============================================================================
%%
%%  Constraint solver.
%%
%%=============================================================================

solve([Fun], State) ->
  ?debug("============ Analyzing Fun: ~w ===========\n", 
	 [debug_lookup_name(Fun)]),
  solve_fun(Fun, dict:new(), State);
solve(SCC = [_|_], State) ->
  ?debug("============ Analyzing SCC: ~w ===========\n", 
	 [[debug_lookup_name(F) || F <- SCC]]),
  solve_scc(SCC, dict:new(), State).

solve_fun(Fun, FunMap, State) ->
  Cs = state__get_cs(Fun, State),
  Deps = get_deps(Cs),
  Ref = mk_constraint_ref(Fun, Deps),
  %% Note that functions are always considered to succeed.
  {ok, _MapDict, NewMap} = solve_ref_or_list(Ref, FunMap, dict:new(), State),
  NewType = lookup_type(Fun, NewMap),
  case state__get_rec_var(Fun, State) of
    error ->     NewFunMap1 = FunMap;
    {ok, Var} -> NewFunMap1 = enter_type(Var, NewType, FunMap)
  end,
  enter_type(Fun, NewType, NewFunMap1).

solve_scc(Scc, Map, State) ->
  State1 = state__mark_as_non_self_rec(Scc, State),
  Vars0 = [{Fun, state__get_rec_var(Fun, State)}||Fun <- Scc],  
  Vars = [Var || {_, {ok, Var}} <- Vars0],
  Funs = [Fun || {Fun, {ok, _}} <- Vars0],
  Types = unsafe_lookup_type_list(Funs, Map),
  RecTypes = [t_limit(Type, ?TYPE_LIMIT) || Type <- Types],
  CleanMap = lists:foldl(fun(Fun, AccFunMap)->
				dict:erase(t_var_name(Fun), AccFunMap)
			 end, Map, Scc),
  Map1 = enter_type_lists(Vars, RecTypes, CleanMap),
  ?debug("Checking scc: ~w\n", [[debug_lookup_name(F) || F <- Scc]]),
  SolveFun = fun(X, Y) -> scc_fold_fun(X, Y, State1) end,
  Map2 = lists:foldl(SolveFun, Map1, Scc),
  FunSet = ordsets:from_list([t_var_name(F) || F <- Scc]),
  case maps_are_equal(Map2, Map, FunSet) of
    true ->
      ?debug("SCC ~w reached fixpoint\n", [Scc]),
      NewTypes = unsafe_lookup_type_list(Funs, Map2),
      case lists:all(fun(T) -> t_is_none(t_fun_range(T)) end, NewTypes) of
	true ->
	  UnitTypes = [t_fun(state__fun_arity(F, State), t_unit()) 
		       || F <- Funs],
	  Map3 = enter_type_lists(Funs, UnitTypes, Map2),
	  solve_scc(Scc, Map3, State);
	false ->
	  Map2
      end;
    false -> 
      ?debug("SCC ~w did not reach fixpoint\n", [Scc]),
      solve_scc(Scc, Map2, State)
  end.

scc_fold_fun(F, FunMap, State) ->
  Deps = get_deps(state__get_cs(F, State)),
  Cs = mk_constraint_ref(F, Deps),
  %% Note that functions are always considered to succeed.
  {ok, _NewMapDict, Map} = solve_ref_or_list(Cs, FunMap, dict:new(), State),
  NewType0 = unsafe_lookup_type(F, Map),
  NewType = t_limit(NewType0, ?TYPE_LIMIT),
  case state__get_rec_var(F, State) of
    {ok, R} ->
      NewFunMap = enter_type(R, NewType, enter_type(F, NewType, FunMap));
    error ->
      NewFunMap = enter_type(F, NewType, FunMap)
  end,
  ?debug("Done solving for function ~w :: ~s\n", [debug_lookup_name(F),
						  format_type(NewType)]),
  NewFunMap.

solve_ref_or_list(#constraint_ref{id=Id, deps=Deps}, 
		  Map, MapDict, State) ->
  {OldLocalMap, Check} = 
    case dict:find(Id, MapDict) of
      error -> {dict:new(), false};
      {ok, M} -> {M, true}
    end,  
  ?debug("Checking ref to fun: ~w\n", [debug_lookup_name(Id)]),
  CheckDeps = ordsets:del_element(t_var_name(Id), Deps),
  case Check andalso maps_are_equal(OldLocalMap, Map, CheckDeps) of
    true -> 
      ?debug("Equal\n", []),
      {ok, MapDict, Map};
    false ->
      ?debug("Not equal. Solving\n", []),
      Cs = state__get_cs(Id, State),
      Res = 
	case state__is_self_rec(Id, State) of
	  true -> solve_self_recursive(Cs, Map, MapDict, Id, t_none(), State);
	  false -> solve_ref_or_list(Cs, Map, MapDict, State)
	end,
      case Res of
	{error, NewMapDict} ->	  
	  ?debug("Error solving for function ~p\n", [debug_lookup_name(Id)]),
	  Arity = state__fun_arity(Id, State),
	  FunType =
	    case state__prop_domain(Id, State) of
	      error -> t_fun(Arity, t_none());
	      {ok, Dom} -> t_fun(Dom, t_none())
	    end,

	  NewMap1 = enter_type(Id, FunType, Map),
	  NewMap2 =
	    case state__get_rec_var(Id, State) of
	      {ok, Var} -> enter_type(Var, FunType, NewMap1);
	      error -> NewMap1
	    end,
	  {ok, dict:store(Id, NewMap2, NewMapDict), NewMap2};
	{ok, NewMapDict, NewMap} ->
	  ?debug("Done solving fun: ~p\n", [debug_lookup_name(Id)]),
	  FunType = lookup_type(Id, NewMap),
	  NewMap1 = enter_type(Id, FunType, Map),
	  NewMap2 =
	    case state__get_rec_var(Id, State) of
	      {ok, Var} -> enter_type(Var, FunType, NewMap1);
	      error -> NewMap1
	    end,
	  {ok, dict:store(Id, NewMap2, NewMapDict), NewMap2}
      end
  end;
solve_ref_or_list(#constraint_list{type=Type, list=Cs, deps = Deps, id=Id}, 
		  Map, MapDict, State) ->
  {OldLocalMap, Check} = 
    case dict:find(Id, MapDict) of
      error -> {dict:new(), false};
      {ok, M} -> {M, true}
    end,
  ?debug("Checking ref to list: ~w\n", [Id]),
  case Check andalso maps_are_equal(OldLocalMap, Map, Deps) of
    true -> 
      ?debug("~w equal ~w\n", [Type, Id]),
      {ok, MapDict, Map};
    false -> 
      ?debug("~w not equal: ~w. Solving\n", [Type, Id]),
      solve_clist(Cs, Type, Id, Deps, MapDict, Map, State)
  end.

solve_self_recursive(Cs, Map, MapDict, Id, RecType0, State) ->
  ?debug("Solving self recursive ~w\n", [debug_lookup_name(Id)]),
  {ok, RecVar} = state__get_rec_var(Id, State),
  ?debug("OldRecType ~s\n", [format_type(RecType0)]),
  RecType = t_limit(RecType0, ?TYPE_LIMIT),
  
  Map1 = enter_type(RecVar, RecType, dict:erase(t_var_name(Id), Map)),
  ?debug("\tMap in: ~p\n",[[{X, format_type(Y)}||{X, Y}<-dict:to_list(Map1)]]),
  case solve_ref_or_list(Cs, Map1, MapDict, State) of
    {error, _} = Error ->
      case t_is_none(RecType0) of
	true ->
	  %% Try again and assume that this is a non-terminating function.
	  Arity = state__fun_arity(Id, State),
	  NewRecType = t_fun(duplicate(Arity, t_any()), t_unit()),
	  solve_self_recursive(Cs, Map, MapDict, Id, NewRecType, State);
	false ->
	  Error
      end;
    {ok, NewMapDict, NewMap} ->
      ?debug("\tMap: ~p\n",[[{X, format_type(Y)}
			     ||{X, Y}<-dict:to_list(NewMap)]]),
      NewRecType = unsafe_lookup_type(Id, NewMap),
      case t_is_equal(NewRecType, RecType0) of
	true -> 	  
	  {ok, NewMapDict, enter_type(RecVar, NewRecType, NewMap)};
	false ->
	  solve_self_recursive(Cs, Map, MapDict, Id, NewRecType, State)
      end
  end.

solve_clist(Cs, conj, Id, Deps, MapDict, Map, State) ->
  case solve_cs(Cs, Map, MapDict, State) of 
    {error, _} = Error -> Error;
    {ok, NewMapDict, NewMap} ->
      case Cs of
	[_] ->
	  %% Just a special case for one conjunctive constraint.
	  {ok, NewMapDict, NewMap};
	_ ->
	  case maps_are_equal(Map, NewMap, Deps) of
	    true -> {ok, dict:store(Id, NewMap, NewMapDict), NewMap};
	    false -> solve_clist(Cs, conj, Id, Deps, NewMapDict, NewMap, State)
	  end
      end
  end;
solve_clist(Cs, disj, Id, _Deps, MapDict, Map, State) ->
  Fun = fun(C, Dict) ->
	    case solve_ref_or_list(C, Map, Dict, State) of
	      {error, NewDict} -> {error, NewDict};
	      {ok, NewDict, NewMap} -> {{ok, NewMap}, NewDict}
	    end
	end,  
  {Maps, NewMapDict} = lists:mapfoldl(Fun, MapDict, Cs),
  case [X || {ok, X} <- Maps] of
    [] -> {error, NewMapDict};
    MapList -> 
      NewMap = join_maps(MapList),      
      {ok, dict:store(Id, NewMap, NewMapDict), NewMap}
  end.

solve_cs([C = #constraint_ref{}|Tail], Map, MapDict, State) ->
  case solve_ref_or_list(C, Map, MapDict, State) of
    {ok, NewMapDict, Map1} -> solve_cs(Tail, Map1, NewMapDict, State);
    {error, NewMapDict} -> {error, NewMapDict}
  end;
solve_cs([C = #constraint_list{}|Tail], Map, MapDict, State) ->
  case solve_ref_or_list(C, Map, MapDict, State) of
    {ok, NewMapDict, Map1} -> solve_cs(Tail, Map1, NewMapDict, State);
    {error, NewMapDict} -> {error, NewMapDict}
  end;
solve_cs([C = #constraint{}|Tail], Map, MapDict, State) ->
  case solve_one_c(C, Map) of
    error ->
      ?debug("+++++++++++\nFailed: ~s :: ~s ~w ~s :: ~s\n+++++++++++\n",
	     [format_type(C#constraint.lhs), 
	      format_type(lookup_type(C#constraint.lhs, Map)),
	      C#constraint.op,
	      format_type(C#constraint.rhs), 
	      format_type(lookup_type(C#constraint.rhs, Map))]),
      {error, MapDict};
    {ok, NewMap} -> 
      solve_cs(Tail, NewMap, MapDict, State)
  end;
solve_cs([], Map, MapDict, _State) ->
  {ok, MapDict, Map}.

solve_one_c(#constraint{lhs=Lhs, rhs=Rhs, op=Op}, Map) ->
  LhsType = lookup_type(Lhs, Map),
  RhsType = lookup_type(Rhs, Map),
  Inf = t_inf(LhsType, RhsType),
  ?debug("Solving: ~s :: ~s ~w ~s :: ~s\n\tInf: ~s\n",
	 [format_type(Lhs), format_type(LhsType), Op,
	  format_type(Rhs), format_type(RhsType), format_type(Inf)]),
  case t_is_none(Inf) of 
    true -> error;
    false ->
      case Op of
	sub -> solve_subtype(Lhs, Inf, Map);
	eq ->
	  case solve_subtype(Lhs, Inf, Map) of
	    error -> error;
	    {ok, Map1} -> solve_subtype(Rhs, Inf, Map1)
	  end
      end
  end.

solve_subtype(Type, Inf, Map) ->
  case cerl:is_literal(Type) of
    true -> 
      case t_is_subtype(t_from_term(cerl:concrete(Type)), Inf) of
	true -> {ok, Map};
	false -> error
      end;
    false ->
      try t_unify(Type, Inf) of
	{_, List} -> {ok, enter_type_list(List, Map)}
      catch
	throw:{mismatch, _T1, _T2} -> 
	  ?debug("Mismatch between ~s and ~s\n", 
		 [format_type(_T1), format_type(_T2)]),
	  error
      end
  end.


%% ============================================================================
%%
%%  Maps and types.
%%
%% ============================================================================

join_maps(Maps) ->
  Keys = lists:foldl(fun(TmpMap, AccKeys) ->
			 [Key || Key <- AccKeys, dict:is_key(Key, TmpMap)]
		     end,
		     dict:fetch_keys(hd(Maps)), tl(Maps)),
  join_maps(Keys, Maps, dict:new()).

join_maps([Key|Left], Maps = [Map|MapsLeft], AccMap) ->
  NewType = join_one_key(Key, MapsLeft, lookup_type(Key, Map)),
  NewAccMap = enter_type(Key, NewType, AccMap),
  join_maps(Left, Maps, NewAccMap);
join_maps([], _Maps, AccMap) ->
  AccMap.

join_one_key(Key, [Map|Left], Type) ->
  case t_is_any(Type) of
    true -> Type;
    false ->
      NewType = lookup_type(Key, Map),
      case t_is_equal(NewType, Type) of
	true -> join_one_key(Key, Left, Type);
	false ->
	  join_one_key(Key, Left, t_sup(NewType, Type))
      end
  end;
join_one_key(_Key, [], Type) ->
  Type.

maps_are_equal(Map1, Map2, Deps) ->
  NewDeps = prune_keys(Map1, Map2, Deps),
  maps_are_equal_1(Map1, Map2, NewDeps).
      
maps_are_equal_1(Map1, Map2, [H|Tail]) ->
  T1 = lookup_type(H, Map1),
  T2 = lookup_type(H, Map2),
  case t_is_equal(T1, T2) of
    true -> maps_are_equal_1(Map1, Map2, Tail);
    false -> 
      ?debug("~w: ~s =/= ~s\n", [H, format_type(T1), format_type(T2)]),
      false      
  end;
maps_are_equal_1(_Map1, _Map2, []) ->
  true.

-define(PRUNE_LIMIT, 100).

prune_keys(Map1, Map2, Deps) ->
  %% This is only worthwhile if the number of deps are reasonably large,
  %% and also bigger than the number of elements in the maps.
  NofDeps = length(Deps),
  case NofDeps > ?PRUNE_LIMIT of
    true ->
      Keys1 = dict:fetch_keys(Map1),
      case length(Keys1) > NofDeps of
	true -> 
	  Set1 = lists:sort(Keys1),
	  Set2 = lists:sort(dict:fetch_keys(Map2)),
	  ordsets:intersection(ordsets:union(Set1, Set2), Deps);
	false ->
	  Deps
      end;
    false ->
      Deps
  end.
  
enter_type(Key, Val, Map) when is_integer(Key) ->
  ?debug("Entering ~s :: ~s\n", [format_type(t_var(Key)), format_type(Val)]),
  case t_is_any(Val) of
    true ->
      dict:erase(Key, Map);
    false ->
      LimitedVal = t_limit(Val, ?INTERNAL_TYPE_LIMIT),
      case dict:find(Key, Map) of
	{ok, LimitedVal} -> Map;
	{ok, _} -> dict:store(Key, LimitedVal, Map);
	error -> dict:store(Key, LimitedVal, Map)
      end
  end;
enter_type(Key, Val, Map) ->
  ?debug("Entering ~s :: ~s\n", [format_type(Key), format_type(Val)]),
  case t_is_any(Val) of
    true ->
      dict:erase(t_var_name(Key), Map);
    false ->
      LimitedVal = t_limit(Val, ?INTERNAL_TYPE_LIMIT),
      KeyName = t_var_name(Key),
      case dict:find(KeyName, Map) of
	{ok, LimitedVal} -> Map;
	{ok, _} -> dict:store(KeyName, LimitedVal, Map);
	error -> dict:store(KeyName, LimitedVal, Map)
      end
  end.

enter_type_lists([Key|KeyTail], [Val|ValTail], Map) ->
  Map1 = enter_type(Key, Val, Map),
  enter_type_lists(KeyTail, ValTail, Map1);
enter_type_lists([], [], Map) ->
  Map.

enter_type_list([{Key, Val}|Tail], Map) ->
  Map1 = enter_type(Key, Val, Map),
  enter_type_list(Tail, Map1);
enter_type_list([], Map) ->
  Map.

lookup_type_list(List, Map) ->
  [lookup_type(X, Map)||X<-List].
      
unsafe_lookup_type(Key, Map) ->
  case dict:find(t_var_name(Key), Map) of
    {ok, Type} -> Type;
    error -> t_none()
  end.

unsafe_lookup_type_list(List, Map) ->
  [unsafe_lookup_type(X, Map)||X<-List].

lookup_type(Key, Map) when is_integer(Key) ->
  case dict:find(Key, Map) of
    error -> t_any();
    {ok, Val} -> Val
  end;
lookup_type(#fun_var{'fun'=Fun}, Map) ->
  Fun(Map);
lookup_type(Key, Map) ->
  case cerl:is_literal(Key) of
    true -> t_from_term(cerl:concrete(Key));
    false -> 
      Subst = t_subst(Key, Map),
      t_sup(Subst, Subst)
  end.

mk_var(Var) ->
  case cerl:is_literal(Var) of
    true -> Var;
    false -> 
      case cerl:is_c_values(Var) of
	true -> t_product(mk_var_no_lit_list(cerl:values_es(Var)));
	false -> t_var(cerl_trees:get_label(Var))
      end
  end.

mk_var_list(List) ->
  [mk_var(X)||X<-List].

mk_var_no_lit(Var) ->
  case cerl:is_literal(Var) of
    true -> t_from_term(cerl:concrete(Var));
    false -> mk_var(Var)
  end.

mk_var_no_lit_list(List) ->
  [mk_var_no_lit(X)||X<-List].

any_none([X | Xs]) ->
  case t_is_none(X) of
    true ->
      true;
    false ->
      any_none(Xs)
  end;
any_none([]) -> false.

%% ============================================================================
%%
%%  The State.
%%
%% ============================================================================

new_state(SCC0, NextLabel, CallGraph, Plt, PropTypes) ->
  NameMap = dict:from_list([{MFA, Var} || {MFA, {Var, _Fun}, _Rec} <- SCC0]),
  SCC = [mk_var(Fun) || {_MFA, {_Var, Fun}, _Rec} <- SCC0],
  #state{cs=[], callgraph=CallGraph, cmap=dict:new(), fun_arities=dict:new(),
	 fun_map=[], in_match=false, in_guard=false,
	 name_map=NameMap, next_label=NextLabel,
	 non_self_recs=[],
	 prop_types=PropTypes, plt=Plt, records=dict:new(), scc=SCC}.

state__set_rec_dict(State = #state{}, RecDict) ->
  State#state{records=RecDict}.

state__lookup_record(#state{records=Records}, Tag, Arity) ->
  case erl_types:lookup_record(Tag, Arity, Records) of
    {ok, Fields} -> 
      {ok, t_tuple([t_from_term(Tag)|
		    [FieldType || {_FieldName, FieldType} <- Fields]])};
    error -> 
      error
  end.

state__set_in_match(State, Bool) ->
  State#state{in_match=Bool}.

state__is_in_match(#state{in_match=Bool}) ->
  Bool.

state__set_in_guard(State, Bool) ->
  State#state{in_guard=Bool}.

state__is_in_guard(#state{in_guard=Bool}) ->
  Bool.

state__get_fun_prototype(Op, Arity, State) ->
  case t_is_fun(Op) of
    true -> {State, Op};
    false -> 
      {State1, [Ret|Args]} = state__mk_vars(Arity+1, State),
      Fun = t_fun(Args, Ret),
      {State1, Fun}
  end.
    
state__lookup_rec_var_in_scope(MFA, #state{name_map=NameMap}) ->
  dict:find(MFA, NameMap).

state__store_fun_arity(Tree, State=#state{fun_arities=Map}) ->
  Arity = length(cerl:fun_vars(Tree)),
  Id = mk_var(Tree),
  State#state{fun_arities=dict:store(Id, Arity, Map)}.

state__fun_arity(Id, #state{fun_arities=Map}) ->
  dict:fetch(Id, Map).

state__lookup_undef_var(Tree, #state{callgraph=CG, plt=Plt}) ->  
  Label = cerl_trees:get_label(Tree),
  case dialyzer_callgraph:lookup_rec_var(Label, CG) of
    error -> error;
    {ok, MFA} -> 
      case dialyzer_plt:lookup(Plt, MFA) of
	none -> error;
	{value, {RetType, ArgTypes}} -> {ok, t_fun(ArgTypes, RetType)}
      end
  end.

state__lookup_apply(Tree, #state{callgraph=Callgraph}) ->
  Apply = cerl_trees:get_label(Tree),
  case dialyzer_callgraph:lookup_call_site(Apply, Callgraph) of
    error ->
      unknown;
    {ok, List} ->
      case lists:member(external, List) of
	true -> unknown;
	false -> List
      end
  end.

get_apply_constr(FunLabels, Dst, ArgTypes, State = #state{callgraph=CG}) ->
  MFAs = [dialyzer_callgraph:lookup_name(Label, CG) || Label <- FunLabels],
  case lists:member(error, MFAs) of
    true -> error;
    false ->
      Constrs = [begin
		   State1 = state__new_constraint_context(State),
		   State2 = get_plt_constr(MFA, Dst, ArgTypes, State1),
		   state__cs(State2)
		 end || {ok, MFA} <- MFAs],
      ApplyConstr = mk_disj_constraint_list(Constrs),
      {ok, state__store_conj(ApplyConstr, State)}
  end.

state__scc(#state{scc=SCC}) ->
  SCC.

state__plt(#state{plt=Plt}) ->
  Plt.

state__new_constraint_context(State) ->
  State#state{cs=[]}.

state__prop_domain(Fun, #state{prop_types=PropTypes}) ->
  Label =
    case t_is_integer(Fun) of
      true -> Fun;
      false ->
	case t_is_var(Fun) of
	  true -> t_var_name(Fun);
	  false -> cerl_trees:get_label(Fun)
	end
    end,
  case dict:find(Label, PropTypes) of
    error -> error;
    {ok, {_Range_Fun, Dom}} -> {ok, Dom};
    {ok, FunType} -> {ok, t_fun_args(FunType)}
  end.


state__add_prop_constrs(Tree, State = #state{prop_types=PropTypes}) ->
  Label = cerl_trees:get_label(Tree),
  case dict:find(Label, PropTypes) of
    error -> State;
    {ok, FunType} ->
      case t_fun_args(FunType) of
	any -> State;
	ArgTypes ->
	  case any_none(ArgTypes) of
	    true -> not_called;
	    false ->
	      ?debug("Adding propagated constr: ~s for function ~w\n", 
		     [format_type(FunType), debug_lookup_name(mk_var(Tree))]),
	      FunVar = mk_var(Tree),
	      state__store_conj(FunVar, sub, FunType, State)
	  end
      end
  end.

state__cs(#state{cs=Cs}) ->
  mk_conj_constraint_list(Cs).

state__store_conj(C, State = #state{cs=Cs}) ->
  State#state{cs=[C|Cs]}.

state__store_conj_list([H|T], State) ->
  State1 = state__store_conj(H, State),
  state__store_conj_list(T, State1);
state__store_conj_list([], State) ->
  State.

state__store_conj(Lhs, Op, Rhs, State = #state{cs=Cs}) ->
  State#state{cs=[mk_constraint(Lhs, Op, Rhs)|Cs]}.

state__store_conj_lists(List1, Op, List2, State) ->
  {NewList1, NewList2} = strip_of_any_constrs(List1, List2),
  state__store_conj_lists_1(NewList1, Op, NewList2, State).

strip_of_any_constrs(List1, List2) ->
  strip_of_any_constrs(List1, List2, [], []).

strip_of_any_constrs([T1|Left1], [T2|Left2], Acc1, Acc2) ->
  case t_is_any(T1) orelse t_is_any(T2) of
    true -> strip_of_any_constrs(Left1, Left2, Acc1, Acc2);
    false -> strip_of_any_constrs(Left1, Left2, [T1|Acc1], [T2|Acc2])
  end;
strip_of_any_constrs([], [], Acc1, Acc2) ->
  {Acc1, Acc2}.

  
state__store_conj_lists_1([Arg1|Arg1Tail], Op, [Arg2|Arg2Tail], State) ->
  State1 = state__store_conj(Arg1, Op, Arg2, State),
  state__store_conj_lists_1(Arg1Tail, Op, Arg2Tail, State1);
state__store_conj_lists_1([], _Op, [], State) ->
  State.

state__mk_var(State = #state{next_label=NL}) ->
  {State#state{next_label=NL+1}, t_var(NL)}.
  
state__mk_vars(N, State = #state{next_label=NL}) ->
  NewLabel = NL + N,
  {State#state{next_label=NewLabel}, [t_var(X)||X<-lists:seq(NL, NewLabel-1)]}.

state__store_constrs(Id, Cs, State = #state{cmap=Dict}) ->  
  NewDict = dict:store(Id, Cs, Dict),
  State#state{cmap=NewDict}.

state__get_cs(Var, #state{cmap=Dict}) ->  
  dict:fetch(Var, Dict).

%% The functions here will not be treated as self recursive.
%% These functions will need to be handled as such manually.
state__mark_as_non_self_rec(SCC, State = #state{non_self_recs=NS}) ->
  State#state{non_self_recs=ordsets:union(NS, ordsets:from_list(SCC))}.

state__is_self_rec(Fun, #state{callgraph=CallGraph, non_self_recs=NS}) ->
  case ordsets:is_element(Fun, NS) of
    true -> false;
    false -> dialyzer_callgraph:is_self_rec(t_var_name(Fun), CallGraph)
  end.

state__store_funs(Vars0, Funs0, State = #state{fun_map=Map}) ->
  debug_make_name_map(Vars0, Funs0),
  Vars = mk_var_list(Vars0),
  Funs = mk_var_list(Funs0),
  NewMap = 
    lists:foldl(fun({Var, Fun}, TmpMap) -> orddict:store(Var, Fun, TmpMap)end,
		Map, lists:zip(Vars, Funs)),
  State#state{fun_map=NewMap}.

state__get_rec_var(Fun, #state{fun_map=Map}) ->
  case [X || {X, Y} <- Map, Y =:= Fun] of
    [Var] -> {ok, Var};
    [] -> error
  end.

state__finalize(State) ->
  State1 = enumerate_constraints(State),
  order_fun_constraints(State1).

%% ============================================================================
%%
%%  Constraints
%%
%% ============================================================================

mk_constraint(Lhs, Op, Rhs) ->
  case t_is_any(Lhs) orelse t_is_any(Rhs) of
    false ->
      Deps = find_constraint_deps([Lhs, Rhs]),
      C0 = mk_constraint_1(Lhs, Op, Rhs),
      C = C0#constraint{deps=Deps},
      case Deps =:= [] of
	true ->
	  %% This constraint is constant. Solve it immediately.
	  case solve_one_c(C, dict:new()) of
	    error -> throw(error);
	    _ -> 
	      %% This is always true, keep it anyway for logistic reasons
	      C
	  end;
	false ->
	  C
      end;
    true ->
      C = mk_constraint_1(t_any(), Op, t_any()),
      C#constraint{deps=[]}
  end.

mk_fun_var(Fun, Deps0) ->
  Deps1 = [X || X <- Deps0, cerl:is_literal(X) =:= false],
  Deps2 = [t_var_name(D) || D <- t_collect_vars(t_product(Deps1))],
  #fun_var{'fun'=Fun, deps=ordsets:from_list(Deps2)}.

get_deps(#constraint{deps=D}) -> D;
get_deps(#constraint_list{deps=D}) -> D;
get_deps(#constraint_ref{deps=D}) -> D.

find_constraint_deps(List) ->
  ordsets:from_list(find_constraint_deps(List, [])).

find_constraint_deps([#fun_var{deps=Deps}|Tail], Acc) ->
  find_constraint_deps(Tail, [Deps|Acc]);
find_constraint_deps([Type|Tail], Acc) ->
  NewAcc = [[t_var_name(D) || D <- t_collect_vars(Type)]|Acc],
  find_constraint_deps(Tail, NewAcc);
find_constraint_deps([], Acc) ->
  lists:flatten(Acc).

mk_constraint_1(Lhs, eq, Rhs) when Lhs < Rhs ->
  #constraint{lhs=Lhs, op=eq, rhs=Rhs};
mk_constraint_1(Lhs, eq, Rhs) ->
  #constraint{lhs=Rhs, op=eq, rhs=Lhs};
mk_constraint_1(Lhs, Op, Rhs) ->
  #constraint{lhs=Lhs, op=Op, rhs=Rhs}.  

mk_constraints([Lhs|LhsTail], Op, [Rhs|RhsTail]) ->
  [mk_constraint(Lhs, Op, Rhs)|mk_constraints(LhsTail, Op, RhsTail)];
mk_constraints([], _Op, []) ->
  [].

mk_constraint_ref(Id, Deps) ->
  #constraint_ref{id=Id, deps=Deps}.

mk_constraint_list(Type, List) ->
  List1 = ordsets:from_list(lift_lists(Type, List)),
  List2 = ordsets:filter(fun(X) -> get_deps(X) =/= [] end, List1),
  Deps = calculate_deps(List2),
  case Deps =:= [] of
    true -> #constraint_list{type = conj, 
			     list = [mk_constraint(t_any(), eq, t_any())],
			     deps = []};
    false -> #constraint_list{type = Type, list = List2, deps = Deps}
  end.

lift_lists(Type, List) ->
  lift_lists(Type, List, []).

lift_lists(Type, [#constraint_list{type=Type, list=List}|Tail], Acc) ->
  lift_lists(Type, Tail, List++Acc);
lift_lists(Type, [C|Tail], Acc) ->
  lift_lists(Type, Tail, [C|Acc]);
lift_lists(_Type, [], Acc) ->
  Acc.

update_constraint_list(CL = #constraint_list{}, List) ->
  CL#constraint_list{list=List}.

%% We expand guard constraints into dijunctive normal form to gain
%% precision in simple guards. However, because of the exponential
%% growth of this expansion in the presens of disjunctions we can even
%% get into trouble while expanding. 
%%
%% To limit this we only expand when the number of disjunctions are
%% below a certain limit. This limit is currently set based on the
%% behaviour of boolean 'or'. 
%%
%%         V1 = V2 or V3
%%
%% Gives us in simplified form the constraints
%%
%%         <Some cs> * ((V1 = true) + (V2 = true) + (V1 = false))
%%
%% and thus a three-parted disjunction. If want to allow for two
%% levels of disjunction we need to have 3^2 = 9 disjunctions. If we
%% want three levels we need 3^3 = 27 disjunctions. More than that
%% seems unnecessary and tends to blow up.
%%
%% Note that by not expanding we lose some precision, but we get a
%% safe over approximation.

-define(DISJ_NORM_FORM_LIMIT, 28).

mk_disj_norm_form(C = #constraint_list{}) ->
  try 
    List1 = expand_to_conjunctions(C),
    mk_disj_constraint_list(List1)
  catch
    throw:too_many_disj -> C
  end.

expand_to_conjunctions(#constraint_list{type=conj, list=List}) ->
  List1 = [C || C <- List, is_simple_constraint(C)],
  List2 = [expand_to_conjunctions(C) || C = #constraint_list{} <- List],
  case List2 =:= [] of
    true -> [mk_conj_constraint_list(List1)];
    false ->
      case List2 of
	[JustOneList] -> 
	  [mk_conj_constraint_list([L|List1]) || L <- JustOneList];
	_ ->
	  combine_conj_lists(List2, List1)
      end
  end;
expand_to_conjunctions(#constraint_list{type=disj, list=List}) ->
  if length(List) > ?DISJ_NORM_FORM_LIMIT -> throw(too_many_disj);
     true -> ok
  end,
  List1 = [C || C <- List, is_simple_constraint(C)],
  %% Just an assert.
  [] = [C || C=#constraint{} <- List1],
  Expanded = lists:flatten([expand_to_conjunctions(C) 
			    || C=#constraint_list{} <- List]),
  ReturnList = Expanded ++ List1,
  if length(ReturnList) > ?DISJ_NORM_FORM_LIMIT -> throw(too_many_disj);
     true -> ReturnList
  end.

is_simple_constraint(#constraint{}) -> true;
is_simple_constraint(#constraint_ref{}) -> true;
is_simple_constraint(#constraint_list{}) -> false.

combine_conj_lists([List1, List2|Left], Prefix) ->
  NewList = [mk_conj_constraint_list([L1, L2]) || L1 <- List1, L2 <- List2],
  if length(NewList) > ?DISJ_NORM_FORM_LIMIT -> throw(too_many_disj);
     true -> ok
  end,
  combine_conj_lists([NewList|Left], Prefix);
combine_conj_lists([List], Prefix) ->
  [mk_conj_constraint_list([mk_conj_constraint_list(Prefix), L]) || L <- List].

calculate_deps(List) ->
  calculate_deps(List, []).

calculate_deps([H|Tail], Acc) ->
  Deps = get_deps(H),
  calculate_deps(Tail, [Deps|Acc]);
calculate_deps([], Acc) ->
  ordsets:from_list(lists:flatten(Acc)).

mk_conj_constraint_list(List) ->
  mk_constraint_list(conj, List).

mk_disj_constraint_list([NotReallyAList]) ->
  NotReallyAList;
mk_disj_constraint_list(List) ->
  %% Make sure each element in the list is either a conjunction or a
  %% ref. Wrap single constraints into conjunctions.
  List1 = [wrap_simple_constr(C) || C <- List],
  mk_constraint_list(disj, List1).

wrap_simple_constr(C = #constraint{}) -> mk_conj_constraint_list([C]);
wrap_simple_constr(C = #constraint_list{}) -> C;
wrap_simple_constr(C = #constraint_ref{}) -> C.

enumerate_constraints(State) ->
  Cs = [mk_constraint_ref(Id, get_deps(state__get_cs(Id, State))) 
	|| Id <- state__scc(State)],
  {_, _, NewState} = enumerate_constraints(Cs, 0, [], State),
  NewState.

enumerate_constraints([C = #constraint_ref{id=Id}|Tail], N, Acc, State) ->
  Cs = state__get_cs(Id, State),
  {[NewCs], NewN, NewState1} = enumerate_constraints([Cs], N, [], State),
  NewState2 = state__store_constrs(Id, NewCs, NewState1),  
  enumerate_constraints(Tail, NewN+1, [C|Acc], NewState2);
enumerate_constraints([C = #constraint_list{type=conj, list=List}|Tail], 
		      N, Acc, State) ->
  %% Separate the flat constraints from the deep ones to make a
  %% separate fixpoint interation over the flat ones for speed.
  {Flat, Deep} = lists:splitwith(fun(#constraint{}) -> true;
				    (#constraint_list{}) -> false;
				    (#constraint_ref{}) -> false
				 end, List),
  {NewFlat, N1, State1} = enumerate_constraints(Flat, N, [], State),
  {NewDeep, N2, State2} = enumerate_constraints(Deep, N1, [], State1),
  {NewList, N3} =
    case shorter_than_two(NewFlat) orelse (NewDeep =:= []) of
      true -> {NewFlat ++ NewDeep, N2};
      false ->
	{NewCLists, TmpN} = group_constraints_in_components(NewFlat, N2),
	{NewCLists ++ NewDeep, TmpN}
    end,
  NewAcc = [C#constraint_list{list=NewList, id={list, N3}}|Acc],
  enumerate_constraints(Tail, N3+1, NewAcc, State2);
enumerate_constraints([C = #constraint_list{list=List, type=disj}|Tail], 
		      N, Acc, State) ->
  {NewList, NewN, NewState} = enumerate_constraints(List, N, [], State),
  NewAcc = [C#constraint_list{list=NewList, id={list, NewN}}|Acc],
  enumerate_constraints(Tail, NewN+1, NewAcc, NewState);
enumerate_constraints([C = #constraint{}|Tail], N, Acc, State) ->
  enumerate_constraints(Tail, N, [C|Acc], State);
enumerate_constraints([], N, Acc, State) ->
  {lists:reverse(Acc), N, State}.

shorter_than_two([]) -> true;
shorter_than_two([_]) -> true;
shorter_than_two([_|_]) -> false.

group_constraints_in_components(Cs, N) ->
  DepList = [Deps || #constraint{deps = Deps} <- Cs],
  case find_dep_components(DepList, []) of
    [_] -> {Cs, N};
    [_|_] = Components ->
      ConstrComp = [[C || C = #constraint{deps=D} <- Cs, 
			  ordsets:is_subset(D, Comp)]
		    || Comp <- Components],
      lists:mapfoldl(fun(CComp, TmpN) ->
			 TmpCList = mk_conj_constraint_list(CComp),
			 {TmpCList#constraint_list{id={list, TmpN}},
			  TmpN + 1}
		     end, N, ConstrComp)
  end.

find_dep_components([Set|Left], AccComponents) ->
  {Component, Ungrouped} = find_dep_components(Left, Set, []),
  case Component =:= Set of
    true -> find_dep_components(Ungrouped, [Component|AccComponents]);
    false -> find_dep_components([Component|Ungrouped], AccComponents)
  end;
find_dep_components([], AccComponents) ->
  AccComponents.

find_dep_components([Set|Left], AccSet, Ungrouped) ->
  case ordsets:intersection(Set, AccSet) of
    [] -> find_dep_components(Left, AccSet, [Set|Ungrouped]);
    [_|_] -> find_dep_components(Left, ordsets:union(Set, AccSet), Ungrouped)
  end;
find_dep_components([], AccSet, Ungrouped) ->
  {AccSet, Ungrouped}.

%% Put the fun ref constraints last in any conjunction since we need
%% to separate the environment from the interior of the function.
order_fun_constraints(State) ->
  Cs = [mk_constraint_ref(Id, get_deps(state__get_cs(Id, State))) 
	|| Id <- state__scc(State)],
  order_fun_constraints(Cs, State).

order_fun_constraints([#constraint_ref{id=Id}|Tail], State) ->
  Cs = state__get_cs(Id, State),
  {[NewCs], State1} = order_fun_constraints([Cs], [], [], State),
  NewState = state__store_constrs(Id, NewCs, State1),
  order_fun_constraints(Tail, NewState);
order_fun_constraints([], State) ->
  State.

order_fun_constraints([C=#constraint_ref{}|Tail], Funs, Acc, State) ->
  order_fun_constraints(Tail, [C|Funs], Acc, State);
order_fun_constraints([C=#constraint_list{list=List}|Tail], Funs, Acc, State) ->
  {NewList, NewState} = 
    case C#constraint_list.type of
      conj -> order_fun_constraints(List, [], [], State);
      disj ->
	FoldFun = fun(X, AccState) -> 
		      {[NewX], NewAccState} = 
			order_fun_constraints([X], [], [], AccState),
		      {NewX, NewAccState}
		  end,
	lists:mapfoldl(FoldFun, State, List)
    end,
  NewAcc = [update_constraint_list(C, NewList)|Acc],
  order_fun_constraints(Tail, Funs, NewAcc, NewState);
order_fun_constraints([C = #constraint{}|Tail], Funs, Acc, State) ->
  order_fun_constraints(Tail, Funs, [C|Acc], State);
order_fun_constraints([], Funs, Acc, State) ->
  NewState = order_fun_constraints(Funs, State),
  {lists:reverse(Acc)++Funs, NewState}.

%% ============================================================================
%%
%%  Utilities.
%%
%% ============================================================================

duplicate(0, _Element) ->
  [];
duplicate(N, Element) ->
  [Element|duplicate(N-1, Element)].

is_singleton_non_number_type(Type) ->
  case t_is_number(Type) of
    true -> false;    
    false -> is_singleton_type(Type)
  end.

is_singleton_type(Type) ->
  case t_is_atom(Type) of
    true ->
      case t_atom_vals(Type) of
	any -> false;
	[_] -> true;
	[_|_] -> false
      end;
    false ->
      case t_is_integer(Type) of
	true ->
	  case t_number_vals(Type) of
	    any -> false;
	    [_] -> true;
	    [_|_] -> false
	  end;
	false ->
	  t_is_nil(Type)
      end
  end.

%% ============================================================================
%%
%%  Pretty printer and debug facilities.
%%
%% ============================================================================

-ifdef(DEBUG_CONSTRAINTS).
-ifndef(DEBUG).
-define(DEBUG, true).
-endif.
-endif.

-ifdef(DEBUG).
format_type(#fun_var{deps=Deps}) ->
  io_lib:format("Fun(~s)", [lists:flatten([format_type(t_var(X))||X<-Deps])]);
format_type(Type) ->
  case cerl:is_literal(Type) of
    true -> io_lib:format("~w", [cerl:concrete(Type)]);
    false -> erl_types:t_to_string(Type)
  end.
-endif.

-ifdef(DEBUG_NAME_MAP).
debug_make_name_map(Vars, Funs) ->
  Map = get(dialyzer_typesig_map),
  NewMap = 
    if Map =:= undefined -> debug_make_name_map(Vars, Funs, dict:new());
       true              -> debug_make_name_map(Vars, Funs, Map)
    end,
  put(dialyzer_typesig_map, NewMap).

debug_make_name_map([Var|VarLeft], [Fun|FunLeft], Map) ->
  Name = {cerl:fname_id(Var), cerl:fname_arity(Var)},
  FunLabel = cerl_trees:get_label(Fun),
  debug_make_name_map(VarLeft, FunLeft, dict:store(FunLabel, Name, Map));
debug_make_name_map([], [], Map) ->
  Map.

debug_lookup_name(Var) ->
  case dict:find(t_var_name(Var), get(dialyzer_typesig_map)) of
    error -> Var;
    {ok, Name} -> Name
  end.

-else.
debug_make_name_map(_Vars, _Funs) ->
  ok.
-endif.

-ifdef(DEBUG_CONSTRAINTS).
pp_constrs_scc(Scc, State) ->
  [pp_constrs(Fun, state__get_cs(Fun, State), State)||Fun <- Scc].

pp_constrs(Fun, Cs, State) ->
  io:format("Constraints for fun: ~w\n", [debug_lookup_name(Fun)]),
  MaxDepth = pp_constraints(Cs, State),
  io:format("Depth: ~w\n", [MaxDepth]).


pp_constraints(Cs, State) ->
  Res = pp_constraints([Cs], none, 0, 0, State),
  io:nl(),
  Res.

pp_constraints([List|Tail], Separator, Level, MaxDepth, 
	       State) when is_list(List) ->
  pp_constraints(List++Tail, Separator, Level, MaxDepth, State);
pp_constraints([#constraint_ref{id=Id}|Left], Separator, 
	       Level, MaxDepth, State) ->
  Cs = state__get_cs(Id, State),
  io:format("%Ref ~w%", [t_var_name(Id)]),
  pp_constraints([Cs|Left], Separator, Level, MaxDepth, State);
pp_constraints([#constraint{lhs=Lhs, op=Op, rhs=Rhs}], _Separator, 
	       Level, MaxDepth, _State) ->
  io:format("~s ~w ~s", [format_type(Lhs), Op, format_type(Rhs)]),
  lists:max([Level, MaxDepth]);
pp_constraints([#constraint{lhs=Lhs, op=Op, rhs=Rhs}|Tail], Separator,
	       Level, MaxDepth, State) ->
  io:format("~s ~w ~s ~s ", [format_type(Lhs), Op, format_type(Rhs),Separator]),
  pp_constraints(Tail, Separator, Level, MaxDepth, State);
pp_constraints([#constraint_list{type=Type, list=List, id=Id}], _Separator, 
	       Level, MaxDepth, State) ->
  io:format("%List ~w(", [Id]),
  NewSeparator = case Type of
		   conj -> "*";
		   disj -> "+"
		 end,
  NewMaxDepth = pp_constraints(List, NewSeparator, Level + 1, MaxDepth, State),
  io:format(")", []),
  NewMaxDepth;
pp_constraints([#constraint_list{type=Type, list=List, id=Id}|Tail],Separator,
	       Level, MaxDepth, State) ->
  io:format("List ~w(", [Id]),
  NewSeparator = case Type of
		   conj -> "*";
		   disj -> "+"
		 end,
  NewMaxDepth = pp_constraints(List, NewSeparator, Level+1, MaxDepth, State),
  io:format(") ~s\n~s ", [Separator, Separator]),
  pp_constraints(Tail, Separator, Level, NewMaxDepth, State).
-else.
pp_constrs_scc(_Scc, _State) ->
  ok.
-endif.

-ifdef(TO_DOT).

constraints_to_dot_scc(Scc, State) ->
  io:format("SCC: ~p\n", [Scc]),
  Name = lists:flatten([io_lib:format("'~w'", [debug_lookup_name(Fun)]) 
			|| Fun <- Scc]),
  Cs = [state__get_cs(Fun, State)|| Fun <- Scc],
  constraints_to_dot(Cs, Name, State).

constraints_to_dot(Cs0, Name, State) ->
  NofCs = length(Cs0),
  Cs = lists:zip(lists:seq(1, NofCs), Cs0),
  {Graph, Opts, _N} = constraints_to_nodes(Cs, NofCs + 1, 1, [], [], State),
  hipe_dot:translate_list(Graph, "/tmp/cs.dot", "foo", Opts),
  Res = os:cmd("dot -o /tmp/"++ Name ++ ".ps -T ps /tmp/cs.dot"),
  io:format("Res: ~p~n", [Res]),
  ok.

constraints_to_nodes([{Name, #constraint_list{type=Type, list=List, id=Id}}
		      |Left], N, Level, Graph, Opts, State) ->
  N1 = N + length(List), 
  NewList = lists:zip(lists:seq(N, N1 - 1), List),
  Names = [SubName || {SubName, _C} <- NewList],
  Edges = [{Name, SubName} || SubName <- Names],
  ThisNode = [{Name, Opt} || Opt <- [{label, 
				      lists:flatten(io_lib:format("~w", [Id]))},
				     {shape, get_shape(Type)},
				     {level, Level}]],
  {NewGraph, NewOpts, N2} = constraints_to_nodes(NewList, N1, Level+1, 
						 [Edges|Graph], 
						 [ThisNode|Opts], State),
  constraints_to_nodes(Left, N2, Level, NewGraph, NewOpts, State);
constraints_to_nodes([{Name, #constraint{lhs=Lhs, op=Op, rhs=Rhs}}|Left],
		     N, Level, Graph, Opts, State) ->
  Label = lists:flatten(io_lib:format("~s ~w ~s", 
				      [format_type(Lhs), Op, 
				       format_type(Rhs)])),
  ThisNode = [{Name, Opt} || Opt <- [{label, Label}, {level, Level}]],
  NewOpts = [ThisNode|Opts],
  constraints_to_nodes(Left, N, Level, Graph, NewOpts, State);
constraints_to_nodes([{Name, #constraint_ref{id=Id0}}|Left],
		     N, Level, Graph, Opts, State) ->
  Id = debug_lookup_name(Id0),
  CList = state__get_cs(Id0, State),
  ThisNode = [{Name, Opt} || Opt <- [{label, 
				      lists:flatten(io_lib:format("~w", [Id]))},
				     {shape, ellipse},
				     {level, Level}]],  
  NewList = [{N, CList}],  
  {NewGraph, NewOpts, N1} = constraints_to_nodes(NewList, N + 1, Level + 1, 
						 [{Name, N}|Graph],
						 [ThisNode|Opts], State),
  constraints_to_nodes(Left, N1, Level, NewGraph, NewOpts, State);
constraints_to_nodes([], N, _Level, Graph, Opts, _State) ->
  {lists:flatten(Graph), lists:flatten(Opts), N}.
  
get_shape(conj) -> box;
get_shape(disj) -> diamond.  

-else.
constraints_to_dot_scc(_Scc, _State) ->
  ok.
-endif.
