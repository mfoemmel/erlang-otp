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
%% Copyright 2006, Tobias Lindahl and Kostis Sagonas
%% 
%%     $Id$
%%

%%% -*- erlang -*-
%%%-------------------------------------------------------------------
%%% File    : dialyzer_typesig.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description : 
%%%
%%% Created : 25 Apr 2005 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------
-module(dialyzer_typesig).

-include("dialyzer.hrl").

-export([analyze_scc/4]).
-export([analyze_scc_get_all_fun_types/5]).
-export([get_safe_underapprox/2]).

-import(erl_types, 
	[t_any/0, t_atom/0, t_atom_vals/1, t_binary/0, t_bool/0, t_cons/2, 
	 t_cons_hd/1, t_cons_tl/1, t_components/1, t_float/0,
	 t_has_var/1, t_is_atom/1,
	 t_is_float/1, t_from_range/2, t_fun/0, t_fun/2, t_fun_args/1,
	 t_fun_range/1, t_is_fun/1, t_pos_improper_list/0,
	 t_inf/2, t_inf_lists/2, t_integer/0, t_is_integer/1,
	 t_is_atom/2, t_is_cons/1, t_is_equal/2, t_is_list/1,
	 t_is_tuple/1, t_is_nil/1, t_is_none/1, t_is_number/1,
	 t_is_any/1, t_is_subtype/2, t_limit/2, t_list/0, t_list/1, 
	 t_list_elements/1, t_nonempty_list/1,
	 t_number/0, t_number_vals/1, t_pid/0, t_port/0,
	 t_product/1, t_ref/0, t_subst/2, t_subtract/2, t_subtract_list/2,
	 t_tuple/0, t_tuple_subtypes/1,
	 t_tuple/1, t_tuple_args/1, t_sup/1, t_sup/2,
	 t_unify/2, t_is_var/1, t_var/1, t_var_name/1, t_from_term/1, 
	 t_none/0]).

-record(state, {callgraph=none, cs, cmap, fun_map, fun_arities, in_match, 
		in_guard, name_map, next_label, non_self_recs, plt, prop_types,
		records, sccs}).
		
-record(constraint, {lhs, op, rhs, deps}).
-record(constraint_list, {type, list, deps, id}).
-record(constraint_ref, {id, deps, type}).
-record(fun_var, {'fun', deps}).

-define(TYPE_LIMIT, 4).
-define(INTERNAL_TYPE_LIMIT, 5).

%-define(DEBUG, true).
%-define(DEBUG_CONSTRAINTS, true).
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

%%% --------------------------------------------------------
%%% Analysis of strongly connected components.
%%%
%%% analyze_scc(SCC, NextLabel, CallGraph, Plt) -> [{MFA, Type}]
%%%
%%% SCC       - [{MFA, Def, Records}]
%%%             where Def = {Var, Fun} as in the Core Erlang module definitions.
%%%                   Records = dict(RecName, {Arity, [{FieldName, FieldType}]})
%%%
%%% NextLabel - An integer that is higher than any label in the code.
%%%
%%% CallGraph - A callgraph as produced by dialyzer_callgraph.erl 
%%%             Note: The callgraph must have been built with all the 
%%%                   code that the scc is a part of.
%%% Plt       - A dialyzer plt. This plt should contain available information
%%%             about functions that can be called by this SCC.
%%%

analyze_scc(SCC, NextLabel, CallGraph, Plt) when is_integer(NextLabel) ->
  assert_format_of_scc(SCC),
  analyze_scc(SCC, NextLabel, CallGraph, Plt, dict:new()).

assert_format_of_scc([{_MFA, {_Var, _Fun}, _Records}|Left]) ->
  assert_format_of_scc(Left);
assert_format_of_scc([]) ->
  ok;
assert_format_of_scc(Other) ->
  erlang:fault({illegal_scc, Other}).

analyze_scc_get_all_fun_types(SCC, NextLabel, CallGraph, Plt, PropTypes) ->
  assert_format_of_scc(SCC),
  {TopMap, _MapDicts} =analyze_scc_1(SCC, NextLabel, CallGraph, Plt, PropTypes),
  TopMap.

analyze_scc(SCC, NextLabel, CallGraph, Plt, PropTypes) ->
  {TopMap, _MapDicts} =analyze_scc_1(SCC, NextLabel, CallGraph, Plt, PropTypes),
  [{MFA, lookup_type(mk_var(Fun), TopMap)} || {MFA, {_Var, Fun}, _Rec} <- SCC].

analyze_scc_1(SCC, NextLabel, CallGraph, Plt, PropTypes) ->
  Defs = [Def || {_MFA, Def, _Rec} <- SCC],
  Records0 = [Rec || {_MFA, _Def, Rec} <- SCC],
  case dialyzer_utils:merge_record_dicts(Records0) of
    {ok, Records1} ->
      State1 = new_state(SCC, NextLabel, CallGraph, Plt, PropTypes, Records1),
      {State2, _} = traverse(cerl:c_letrec(Defs, cerl:c_atom(foo)), 
			     sets:new(), State1),
      State3 = state__finalize(State2),
      Funs = state__sccs(State3),
      [pp_constrs_scc(X, State3) || X <- Funs],
      constraints_to_dot_scc(lists:flatten(Funs), State3),
      analyze_loop(Funs, State3, dict:new());
    {record_clash, Tag, _Arity} ->
      Msg = lists:flatten(io_lib:format("Error merging definitions of #~w{}",
					[Tag])),
      throw({dialyzer_succ_typing_error, Msg})
  end.

analyze_loop(Funs, State, Map) ->
  analyze_loop(Funs, State, Map, []).

analyze_loop([[Fun]|Left], State, Map, Acc) ->
  ?debug("============ Analyzing Fun: ~w ===========\n", 
	  [debug_lookup_name(Fun)]),
  {Map1, MapDict} = solve_fun(Fun, Map, State),
  analyze_loop(Left, State, Map1, [MapDict|Acc]);
analyze_loop([SCC|Left], State, Map, Acc) ->
  ?debug("============ Analyzing SCC: ~w ===========\n", 
	 [[debug_lookup_name(F) || F <- SCC]]),
  %% This is needed to treat the whole component at the same time.
  State1 = state__mark_as_non_self_rec(SCC, State),
  {Map1, MapDicts} = solve_scc(SCC, Map, State1),
  ?debug("Checking scc: ~w\n", [[debug_lookup_name(F) || F <- SCC]]),
  case maps_are_equal(Map1, Map, SCC) of
    true ->       
      analyze_loop(Left, State, Map1, [MapDicts|Acc]);
    false -> 
      ?debug("SCC ~w did not reach fixpoint\n", [SCC]),
      analyze_loop([SCC|Left], State, Map1, Acc)
  end;
analyze_loop([], _State, Map, Acc) ->
  {Map, lists:flatten(Acc)}.


%% ============================================================================
%%
%%  Gets the constraints by traversing the code.
%%
%% ============================================================================

traverse(Tree, DefinedVars, State) ->
  %%?debug("Handling ~p\n", [cerl:type(Tree)]),
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
      {State0, OpType} = traverse(Op, DefinedVars, State),
      {State1, FunType} = state__get_fun_prototype(OpType, Arity, State0),
      State2 = state__store_conj(FunType, eq, OpType, State1),
      State3 = state__store_conj(mk_var(Tree),sub,t_fun_range(FunType), State2),
      {State4, ArgTypes} = traverse_list(Args, DefinedVars, State3),      
      State5 = state__store_conj_lists(ArgTypes,sub,t_fun_args(FunType),State4),
      {State5, mk_var(Tree)};
    binary ->
      {State1, _} = traverse_list(cerl:binary_segments(Tree), 
				  DefinedVars, State),
      State2 = state__store_conj(mk_var(Tree), eq, t_binary(), State1),
      {State2, t_binary()};
    bitstr ->
      %% Only care about Size and Value since the other fields are
      %% constant literals. Size must be an integer - NO, it can be
      %% 'all' but this has to be a literal.
      Size = cerl:bitstr_size(Tree),
      Unit = cerl:bitstr_unit(Tree),
      Val = cerl:bitstr_val(Tree),            
      {State1, [SizeType, ValType]} = 
	traverse_list([Size, Val], DefinedVars, State),
      State2 =
	case cerl:type(Size) of
	  literal ->
	    %% Safety check
	    case cerl:concrete(Size) of
	      all -> State1;
	      N when is_integer(N) -> State1
	    end;
	  var ->
	    state__store_conj(SizeType, sub, t_integer(), State1)
	end,
      State3 = 
	case cerl:concrete(cerl:bitstr_type(Tree)) of
	  float -> state__store_conj(ValType, sub, t_float(), State2);
	  binary -> state__store_conj(ValType, sub, t_binary(), State2);
	  integer ->
	    case state__is_in_match(State2) of
	      true ->
		case cerl:is_c_int(Size) of
		  true ->
		    SizeVal = cerl:int_val(Size),
		    UnitVal = cerl:int_val(Unit),
		    TotalSizeVal = SizeVal * UnitVal,
		    Flags = cerl:concrete(cerl:bitstr_flags(Tree)),
		    Type = 
		      case lists:member(signed, Flags) of
			true -> 
			  t_from_range(-(1 bsl (TotalSizeVal - 1)),
				       1 bsl (TotalSizeVal - 1) - 1);
			false -> 
			  t_from_range(0,1 bsl TotalSizeVal - 1)
		      end,
		    state__store_conj(ValType, sub, Type, State2);
		  false -> 
		    state__store_conj(ValType, sub, t_integer(), State2)
		end;
	      false ->
		state__store_conj(ValType, sub, t_integer(), State2)
	    end
	end, 
      {State3, t_any()};
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
      FunFailType = t_fun(duplicate(length(Vars), t_none()), t_none()),
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
      Ref = mk_constraint_ref(mk_var(Tree), get_deps(Cs), 'fun'),
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
	Other -> erlang:fault({'Unsupported primop', Other})
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
	    error -> erlang:fault({'Undefined variable', Tree});
	    {ok, Type} -> {State, Type}
	  end
      end;
    Other ->
      erlang:fault({'Unsupported type', Other})
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
      case state__lookup_name({M, F, A}, State) of
	error ->
	  case get_bif_constr({M, F, A}, Dst, ArgVars, State1) of
	    none -> 
	      get_plt_constr({M, F, A}, Dst, ArgVars, State1);
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
  case dialyzer_plt:lookup(Plt, MFA) of
    none -> {State, Dst};
    {value, {RetType, ArgTypes}} ->
      {state__store_conj_lists([Dst|ArgVars], sub, [RetType|ArgTypes], State), 
       Dst}
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
  State2 = state__store_constrs_list(CList, State1),
  CRefs = [mk_constraint_ref(Id, get_deps(Cs), clause) || {Id, Cs} <- CList],
  {NewCs, NewState} =
    case Action of
      none -> 
	if CList =:= [] -> throw(error);
	   true -> {CRefs, State2}
	end;
      _ -> 
	try 
	  {State3, ActionVar} = traverse(Action, DefinedVars, State2),
	  TmpC = mk_constraint(TopVar, eq, ActionVar),
	  ActionCs = mk_conj_constraint_list([state__cs(State3),TmpC]),
	  {[ActionCs|CRefs], State3}
	catch
	  throw:error ->
	    if CList =:= [] -> throw(error);
	       true -> {CRefs, State2}
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
    Id = mk_var(Clause),
    handle_clauses_1(Tail, TopVar, Arg, DefinedVars, State6, 
		     NewSubtrTypes, [{Id, Cs}|Acc])
  catch
    throw:error -> 
      handle_clauses_1(Tail, TopVar, Arg, DefinedVars, 
		       State, NewSubtrTypes, Acc)
  end;
handle_clauses_1([], _TopVar, _Arg, _DefinedVars, State, _SubtrType, Acc) ->
  {state__new_constraint_context(State), Acc}.

get_safe_underapprox(Pats, Guard) ->
  case cerl:is_c_atom(Guard) andalso (cerl:concrete(Guard) =:= true) of
    true -> 
      try
	t_product(get_safe_underapprox_1(Pats, []))
      catch
	throw:dont_know -> t_none()
      end;
    false -> t_none()
  end.

get_safe_underapprox_1([Pat|Left], Acc) ->
  case cerl:type(Pat) of
    alias ->
      get_safe_underapprox_1([cerl:alias_pat(Pat)|Left], Acc);
    binary ->
      %% TODO: Can maybe do something here
      throw(dont_know);      
    cons ->
      [Hd, Tl] = 
	get_safe_underapprox_1([cerl:cons_hd(Pat), cerl:cons_tl(Pat)], []),
      case t_is_any(Tl) of
	true -> get_safe_underapprox_1(Left, [t_nonempty_list(Hd)|Acc]);
	false -> throw(dont_know)
      end;
    literal ->
      case cerl:unfold_literal(Pat) of
	Pat ->
	  Type =
	    case cerl:concrete(Pat) of
	      Int when is_integer(Int) -> t_from_term(Int);
	      Atom when is_atom(Atom) -> t_from_term(Atom);
	      _Other -> throw(dont_know)
	    end,
	  get_safe_underapprox_1(Left, [Type|Acc]);
	OtherPat ->
	  get_safe_underapprox_1([OtherPat|Left], Acc)
      end;
    tuple ->
      Es = cerl:tuple_es(Pat),
      Type = t_tuple(get_safe_underapprox_1(Es, [])),
      get_safe_underapprox_1(Left, [Type|Acc]);
    values ->
      Type = t_product(get_safe_underapprox_1(Pat, [])),
      get_safe_underapprox_1(Left, [Type|Acc]);
    var ->
      get_safe_underapprox_1(Left, [t_any()|Acc])
  end;
get_safe_underapprox_1([], Acc) ->
  lists:reverse(Acc).

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

solve_fun(Fun, FunMap, State) ->
  Cs = state__get_cs(Fun, State),
  Deps = get_deps(Cs),
  Ref = mk_constraint_ref(Fun, Deps, 'fun'),
  %% Note that functions are always considered to succeed.
  {ok, MapDict, NewMap} = solve_ref_or_list(Ref, FunMap, State),
  NewType = lookup_type(Fun, NewMap),
  case state__get_rec_var(Fun, State) of
    error ->     NewFunMap1 = FunMap;
    {ok, Var} -> NewFunMap1 = enter_type(Var, NewType, FunMap)
  end,
  NewFunMap = enter_type(Fun, NewType, NewFunMap1),
  {NewFunMap, MapDict}.

solve_scc(Scc, FunMap, State) ->
  Vars0 = [{Fun, state__get_rec_var(Fun, State)}||Fun <- Scc],  
  Vars = [Var || {_, {ok, Var}} <- Vars0],
  Funs = [Fun || {Fun, {ok, _}} <- Vars0],
  Types = unsafe_lookup_type_list(Funs, FunMap),
  RecTypes = [t_limit(Type, ?TYPE_LIMIT) || Type <- Types],
  CleanFunMap = lists:foldl(fun(Fun, AccFunMap)->
			     dict:erase(t_var_name(Fun), AccFunMap)
			 end, FunMap, Scc),
  FunMap1 = enter_type_lists(Vars, RecTypes, CleanFunMap),
  SolveFun = fun(X, Y)-> scc_fold_fun(X, Y, State)end,
  lists:foldl(SolveFun, {FunMap1, []}, Scc).

scc_fold_fun(F, {FunMap, Acc}, State) ->
  Deps = get_deps(state__get_cs(F, State)),
  Cs = mk_constraint_ref(F, Deps, 'fun'),
  %% Note that functions are always considered to succeed.
  {ok, MapDict, Map} = solve_ref_or_list(Cs, FunMap, State),
  NewType0 = unsafe_lookup_type(F, Map),
  NewType = t_limit(NewType0, ?TYPE_LIMIT),
  case state__get_rec_var(F, State) of
    {ok, R} ->
      NewFunMap = enter_type(R, NewType, enter_type(F, NewType, FunMap));
    error ->
      NewFunMap = enter_type(F, NewType, FunMap)
  end,
  ?debug("Done solving for function ~w\n", [debug_lookup_name(F)]),
  {NewFunMap, [MapDict|Acc]}.

solve_ref_or_list(C, Map, State) ->
  case solve_ref_or_list(C, Map, dict:new(), State) of
    {error, _} -> 
      %% Cannot happen for functions.
      error;
    {ok, MapDict, Map1} -> 
      {ok, MapDict, Map1}
  end.

solve_ref_or_list(#constraint_ref{id=Id, deps=Deps, type='fun'}, 
		  Map, MapDict, State) ->
  {OldLocalMap, Check} = 
    case dict:find(Id, MapDict) of
      error -> {dict:new(), false};
      {ok, M} -> {M, true}
    end,  
  ?debug("Checking ref to fun: ~w\n", [debug_lookup_name(Id)]),
  case Check andalso maps_are_equal(OldLocalMap, Map, 
				    ordsets:del_element(Id, Deps)) of
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
	  %% HERE
	  FunType = t_fun(duplicate(Arity, t_none()), t_none()),
	  %%FunType = t_fun(Arity, t_none()),
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
solve_ref_or_list(#constraint_ref{id=Id, deps=Deps, type=clause}, 
		  Map, MapDict, State) ->
  {OldLocalMap, Check} = 
    case dict:find(Id, MapDict) of
      error -> {dict:new(), false};
      {ok, M} -> {M, true}
    end,
  ?debug("Checking ref to clause: ~w\n", [Id]),
  case Check andalso maps_are_equal(OldLocalMap, Map, Deps) of
    true -> 
      ?debug("Equal\n", []),
      {ok, MapDict, Map};
    false ->
      ?debug("Not equal. Solving\n", []),
      Cs = state__get_cs(Id, State),
      case solve_ref_or_list(Cs, Map, MapDict, State) of
	{error, NewMapDict} ->	  
	  ?debug("Failed solving ~w\n", [Id]),
	  {error, dict:erase(Id, NewMapDict)};
	{ok, NewMapDict, NewMap} ->
	  ?debug("Done solving ~w\n", [Id]),
	  {ok, dict:store(Id, NewMap, NewMapDict), NewMap}
      end
  end;
solve_ref_or_list(#constraint_list{type=Type, list=Cs, deps = Deps, id=Id}, 
		  Map, MapDict, State) ->
  {OldLocalMap, Check} = 
    case dict:find(Id, MapDict) of
      error -> {dict:new(), false};
      {ok, M} -> 
	case Type of
	  conj -> {dict:new(), false};
	  disj -> {M, true}
	end
    end,
  ?debug("Checking ref to list: ~w\n", [Id]),
  case Check andalso maps_are_equal(OldLocalMap, Map, Deps) of
    true -> 
      ?debug("Equal\n", []),
      {ok, MapDict, Map};
    false -> 
      ?debug("Not equal. Solving\n", []),
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
    {error, _} = Error -> Error;
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
  ?debug("Solving conj list ~w\n", [Id]),
  case solve_cs(Cs, Map, MapDict, State) of 
    {error, _} = Error ->
      Error;
    {ok, NewCs, NewDict, Map1} -> 
      ?debug("Checking conj list ~w\n", [Id]),
      case maps_are_equal(Map, Map1, Deps) of
	true -> 
	  ?debug("Done solving conj list ~w\n", [Id]),
	  {ok, NewDict, Map1};
	false -> 
	  ?debug("Looping\n", []),
	  solve_clist(NewCs, conj, Id, Deps, NewDict, Map1, State)
      end
  end;
solve_clist(Cs, disj, Id, _Deps, MapDict, Map, State) ->
  ?debug("Solving disj list ~w\n", [Id]),
  Fun = fun(C, Dict) ->
	    case solve_ref_or_list(C, Map, Dict, State) of
	      {error, NewDict} -> {error, NewDict};
	      {ok, NewDict, NewMap} -> {{ok, NewMap}, NewDict}
	    end
	end,  
  {Maps, NewDict} = lists:mapfoldl(Fun, MapDict, Cs),
  case [X || {ok, X} <- Maps] of
    [] -> {error, NewDict};
    MapList -> 
      ?debug("Done solving disj list ~w\n", [Id]),
      NewMap = join_maps(MapList),      
      {ok, dict:store(Id, NewMap, NewDict), NewMap}
  end.

solve_cs(List, Map, MapDict, State) ->
  solve_cs(List, Map, MapDict, State, []).

solve_cs([C = #constraint_ref{}|Tail], Map, MapDict, State, Acc) ->
  case solve_ref_or_list(C, Map, MapDict, State) of
    {ok, NewMapDict, Map1} -> solve_cs(Tail, Map1, NewMapDict, State, [C|Acc]);
    {error, NewMapDict} -> {error, NewMapDict}
  end;
solve_cs([C = #constraint_list{type=conj, list=List}|Tail], Map, 
	 MapDict, State, Acc) ->
  case solve_ref_or_list(List, Map, MapDict, State) of
    {ok, NewMapDict, Map1} -> solve_cs(Tail, Map1, NewMapDict, State, [C|Acc]);
    {error, NewMapDict} -> {error, NewMapDict}
  end;
solve_cs([C = #constraint_list{type=disj}|Tail], Map, MapDict, State, Acc) ->
  case solve_ref_or_list(C, Map, MapDict, State) of
    {error, NewMapDict} -> {error, NewMapDict};
    {ok, NewMapDict, Map1} -> solve_cs(Tail, Map1, NewMapDict, State, [C|Acc])
  end;
solve_cs([C = #constraint{lhs=Lhs, rhs=Rhs, op=Op}|Tail],
	 Map, MapDict, State, Acc) ->  
  case solve_one_c(C, Map) of
    error ->
      ?debug("+++++++++++\nFailed: ~s :: ~s ~w ~s :: ~s\n+++++++++++\n",
	     [format_type(Lhs), format_type(lookup_type(Lhs, Map)), Op,
	      format_type(Rhs), format_type(lookup_type(Rhs, Map))]),
      {error, MapDict};
    {ok, NewMap} -> 
      case cerl:is_literal(Rhs) of
	true -> 
	  solve_cs(Tail, NewMap, MapDict, State, Acc);
	false -> 
	  case cerl:is_literal(Lhs) andalso (Op =:= eq) of
	    true ->
	      solve_cs(Tail, NewMap, MapDict, State, Acc);
	    false ->
	      solve_cs(Tail, NewMap, MapDict, State, [C|Acc])
	  end
      end
  end;
solve_cs([], Map, MapDict, _State, Acc) ->
  {ok, Acc, MapDict, Map}.

solve_one_c(C, Map) ->
  Lhs = C#constraint.lhs,
  Rhs = C#constraint.rhs,
  Op  = C#constraint.op,
  LhsType = lookup_type(Lhs, Map),
  RhsType = lookup_type(Rhs, Map),
  Inf = t_inf(LhsType, RhsType),
  ?debug("Solving: ~s :: ~s ~w ~s :: ~s\n\tInf: ~s\n",
	 [format_type(Lhs), format_type(LhsType), Op,
	  format_type(Rhs), format_type(RhsType), format_type(Inf)]),
  case t_is_none(Inf) of 
    true -> error;
    false ->
      case solve_subtype(Lhs, Inf, Map) of
	error -> error;
	{ok, Map1} ->
	  case Op of
	    sub -> {ok, Map1};
	    eq ->
	      case solve_subtype(Rhs, Inf, Map1) of
		error -> error;
		{ok, Map2} -> 
		  {ok, Map2}
	      end
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
	{_, List} ->
	  {Vars, Types} = lists:unzip(List),
	  NewTypes = t_inf_lists(Types, lookup_type_list(Vars, Map)),
	  case any_none(NewTypes) of
	    true -> error;
	    false -> {ok, enter_type_lists(Vars, NewTypes, Map)}
	  end
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
  ?debug("Joining maps:\n", []),
  [?debug("\tMap: ~p\n", [[{X, format_type(Y)}||{X, Y}<-dict:to_list(_Map)]])
   || _Map <- Maps],
  Fun = fun(T, Map1, Map2)->
	    t_sup(lookup_type(T, Map1), lookup_type(T, Map2))
	end,
  Keys = lists:foldl(fun(TmpMap, AccKeys) -> 
			 Keys1 = ordsets:from_list(dict:fetch_keys(TmpMap)),
			 ordsets:intersection(Keys1, AccKeys)
		     end, 
		     ordsets:from_list(dict:fetch_keys(hd(Maps))), tl(Maps)),
  Res = merge_maps(Maps, Fun, Keys),
  ?debug("Result:\n", []),
  ?debug("\tMap: ~p\n", [[{X, format_type(Y)}||{X, Y}<-dict:to_list(Res)]]),
  Res.

merge_maps([Map], _Fun, _Keys) ->
  Map;
merge_maps([Map1, Map2|Tail], Fun, Keys) ->
  Map = merge_two_maps(Keys, Map1, Map2, Fun, dict:new()),
  merge_maps([Map|Tail], Fun, Keys).

merge_two_maps([Key|Tail], Map1, Map2, Fun, AccMap) ->
  NewAccMap = enter_type(Key, Fun(Key, Map1, Map2), AccMap),
  merge_two_maps(Tail, Map1, Map2, Fun, NewAccMap);
merge_two_maps([], _Map1, _Map2, _Fun, AccMap) ->
  AccMap.

maps_are_equal(Map1, Map2, [H|Tail]) ->
  T1 = lookup_type(H, Map1),
  T2 = lookup_type(H, Map2),
  case t_is_equal(T1, T2) of
    true -> maps_are_equal(Map1, Map2, Tail);
    false -> 
      ?debug("~w: ~s =/= ~s\n", [H, format_type(T1), format_type(T2)]),
      false      
  end;
maps_are_equal(_Map1, _Map2, []) ->
  true.


enter_type(Key, Val, Map) when is_integer(Key) ->
  ?debug("Entering ~s :: ~s\n", [format_type(t_var(Key)), format_type(Val)]),
  case t_is_any(Val) of
    true ->
      dict:erase(Key, Map);
    false ->
      dict:store(Key, t_limit(Val, ?INTERNAL_TYPE_LIMIT), Map)
  end;
enter_type(Key, Val, Map) ->
  ?debug("Entering ~s :: ~s\n", [format_type(Key), format_type(Val)]),
  case t_is_var(Key) of
    true -> 
      case t_is_any(Val) of
	true ->
	  dict:erase(t_var_name(Key), Map);
	false ->
	  dict:store(t_var_name(Key), t_limit(Val, ?INTERNAL_TYPE_LIMIT), Map)
      end;
    false ->
      Map
  end.

enter_type_lists([Key|KeyTail], [Val|ValTail], Map) ->
  Map1 = enter_type(Key, Val, Map),
  enter_type_lists(KeyTail, ValTail, Map1);
enter_type_lists([], [], Map) ->
  Map.


lookup_type(Key, Map) ->
  lookup(Key, Map, t_any()).

lookup_type_list(List, Map) ->
  [lookup_type(X, Map)||X<-List].
      
unsafe_lookup_type(Key, Map) ->
  lookup(Key, Map, t_none()).

unsafe_lookup_type_list(List, Map) ->
  [unsafe_lookup_type(X, Map)||X<-List].


lookup(Key, Map, AnyNone) when is_integer(Key) ->
  case dict:find(Key, Map) of
    error -> AnyNone;
    {ok, Val} -> Val
  end;
lookup(#fun_var{'fun'=Fun}, Map, _AnyNone) ->
  Fun(Map);
lookup(Key, Map, AnyNone) ->
  case cerl:is_literal(Key) of
    true -> t_from_term(cerl:concrete(Key));
    false -> 
      case t_is_var(Key) of
	true ->
	  case dict:find(t_var_name(Key), Map) of
	    {ok, Val} -> t_subst(Val, Map);
	    error -> AnyNone
	  end;
	false ->
	  Res = t_subst(Key, Map),
	  t_sup(Res, Res)
      end
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

new_state(SCC0, NextLabel, CallGraph, Plt, PropTypes, Records) ->
  NameMap = dict:from_list([{MFA, Var} || {MFA, {Var, _Fun}, _Rec} <- SCC0]),
  SCC = [mk_var(Fun) || {_MFA, {_Var, Fun}, _Rec} <- SCC0],
  #state{cs=[], callgraph=CallGraph, cmap=dict:new(), fun_arities=dict:new(),
	 fun_map=[], in_match=false, in_guard=false,
	 name_map=NameMap, next_label=NextLabel,
	 non_self_recs=[],
	 prop_types=PropTypes, plt=Plt, records=Records, sccs=[SCC]}.

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
    
state__lookup_name(MFA, #state{name_map=NameMap}) ->
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

state__sccs(#state{sccs=SCCs}) ->
  SCCs.

state__plt(#state{plt=Plt}) ->
  Plt.

state__new_constraint_context(State) ->
  State#state{cs=[]}.

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

state__store_constrs_list([{Id, Cs}|Left], State) ->
  State1 = state__store_constrs(Id, Cs, State),
  state__store_constrs_list(Left, State1);
state__store_constrs_list([], State) ->
  State.

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
  Deps = [X || X <- Deps0, cerl:is_literal(X) =:= false],
  #fun_var{'fun'=Fun, deps=ordsets:from_list(filter_vars(Deps))}.

get_deps(#constraint{deps=D}) -> D;
get_deps(#constraint_list{deps=D}) -> D;
get_deps(#constraint_ref{deps=D}) -> D.

find_constraint_deps(List) ->
  ordsets:from_list(find_constraint_deps(List, [])).

find_constraint_deps([#fun_var{deps=Deps}|Tail], Acc) ->
  find_constraint_deps(Tail, [Deps|Acc]);
find_constraint_deps([Type|Tail], Acc) ->
  Vars = 
    case t_is_var(Type) of
      true -> [Type];
      false ->
	case t_is_fun(Type) of
	  true -> [t_fun_range(Type)|t_fun_args(Type)];
	  false ->
	    case t_is_tuple(Type) of
	      true -> 
		case t_tuple_subtypes(Type) of
		  any -> [];
		  TupleList ->
		    lists:flatten([t_tuple_args(T) || T <- TupleList])
		end;
	      false -> t_components(Type)
	    end
	end
    end,
  find_constraint_deps(Tail, [filter_vars(Vars)|Acc]);
find_constraint_deps([], Acc) ->
  lists:flatten(Acc).
		
filter_vars([Var|Tail]) ->		
  case t_is_var(Var) of
    true -> [Var|filter_vars(Tail)];
    false -> filter_vars(Tail)
  end;
filter_vars(any) ->
  [];
filter_vars(none) ->
  [];
filter_vars([]) ->
  [].

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

mk_constraint_ref(Id, Deps, Type) ->
  #constraint_ref{id=Id, deps=Deps, type=Type}.

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
  List1 = lists:filter(fun(#constraint{}) -> true;
			  (#constraint_ref{}) -> true;
			  (#constraint_list{}) -> false
		       end, List),
  if length(List1) > ?DISJ_NORM_FORM_LIMIT -> throw(too_many_disj);
     true -> ok
  end,
  List2 = [expand_to_conjunctions(C) || C=#constraint_list{} <- List],
  case List2 =:= [] of
    true -> [mk_conj_constraint_list(List1)];
    false ->
      ReturnList = 
	case List2 of
	  [JustOneList] -> 
	    [mk_conj_constraint_list([L|List1])
	     || L <- JustOneList];
	  _ ->
	    combine_conj_lists(List2, List1)
	end,
      if length(ReturnList) > ?DISJ_NORM_FORM_LIMIT -> throw(too_many_disj);
	 true -> ReturnList
      end
  end;
expand_to_conjunctions(#constraint_list{type=disj, list=List}) ->
  if length(List) > ?DISJ_NORM_FORM_LIMIT -> throw(too_many_disj);
     true -> ok
  end,
  List1 = lists:filter(fun(#constraint{}) -> true;
			  (#constraint_ref{}) -> true;
			  (#constraint_list{}) -> false
		       end, List),
  %% Just an assert.
  [] = [C || C=#constraint{} <- List1],
  Expanded = lists:flatten([expand_to_conjunctions(C) 
			    || C=#constraint_list{} <- List]),
  ReturnList = Expanded ++ List1,
  if length(ReturnList) > ?DISJ_NORM_FORM_LIMIT -> throw(too_many_disj);
     true -> ReturnList
  end.

combine_conj_lists([List1, List2|Left], Prefix) ->
  NewList = [mk_conj_constraint_list([L1, L2]) || L1 <- List1, L2 <- List2],
  combine_conj_lists([NewList|Left], Prefix);
combine_conj_lists([List], Prefix) ->
  [mk_conj_constraint_list([mk_conj_constraint_list(Prefix), L]) || L <- List].
  
						  

calculate_deps(List) ->
  calculate_deps(List, []).

calculate_deps([H|Tail], Acc) ->
  Deps = get_deps(H),
  calculate_deps(Tail, ordsets:union(Deps, Acc));
calculate_deps([], Acc) ->
  Acc.

mk_conj_constraint_list(List) ->
  mk_constraint_list(conj, List).

mk_disj_constraint_list([NotReallyAList]) ->
  NotReallyAList;
mk_disj_constraint_list(List) ->
  %% Make sure all elements in the list is a conjunction or a
  %% ref. Wrap single constraints into conjunctions.
  List1 = lists:map(fun(C = #constraint{}) -> mk_conj_constraint_list([C]);
		       (C = #constraint_list{}) -> C;
		       (C = #constraint_ref{}) -> C
		    end, List),
  mk_constraint_list(disj, List1).

enumerate_constraints(State) ->
  Cs = [mk_constraint_ref(Id, get_deps(state__get_cs(Id, State)), 'fun') 
	|| Id <- lists:flatten(state__sccs(State))],
  {_, _, NewState} = enumerate_constraints(Cs, 0, [], State),
  NewState.

enumerate_constraints([C = #constraint_ref{id=Id}|Tail], N, Acc, State) ->
  Cs = state__get_cs(Id, State),
  {[NewCs], NewN, NewState1} = enumerate_constraints([Cs], N, [], State),
  NewState2 = state__store_constrs(Id, NewCs, NewState1),  
  enumerate_constraints(Tail, NewN+1, [C|Acc], NewState2);
enumerate_constraints([C = #constraint_list{list=List}|Tail], N, Acc, State) ->
  {NewList, NewN, NewState} = enumerate_constraints(List, N, [], State),
  NewAcc = [C#constraint_list{list=NewList, id={list, NewN}}|Acc],
  enumerate_constraints(Tail, NewN+1, NewAcc, NewState);
enumerate_constraints([C = #constraint{}|Tail], N, Acc, State) ->
  enumerate_constraints(Tail, N, [C|Acc], State);
enumerate_constraints([], N, Acc, State) ->
  {lists:reverse(Acc), N, State}.

%% Put the fun ref constraints last in any conjunction since we need
%% to separate the environment from the interior of the function.
order_fun_constraints(State) ->
  Cs = [mk_constraint_ref(Id, get_deps(state__get_cs(Id, State)), 'fun') 
	|| Id <- lists:flatten(state__sccs(State))],
  order_fun_constraints(Cs, State).

order_fun_constraints([#constraint_ref{type='fun', id=Id}|Tail], State) ->
  Cs = state__get_cs(Id, State),
  {[NewCs], State1} = order_fun_constraints([Cs], [], [], State),
  NewState = state__store_constrs(Id, NewCs, State1),
  order_fun_constraints(Tail, NewState);
order_fun_constraints([], State) ->
  State.

order_fun_constraints([C=#constraint_ref{type='fun'}|Tail], Funs, Acc, State) ->
  order_fun_constraints(Tail, [C|Funs], Acc, State);
order_fun_constraints([C=#constraint_ref{id=Id}|Tail], Funs, Acc, State) ->
  Cs = state__get_cs(Id, State),
  {[NewCs], NewState1} = order_fun_constraints([Cs], [], [], State),
  NewState2 = state__store_constrs(Id, NewCs, NewState1),
  order_fun_constraints(Tail, Funs, [C|Acc], NewState2);
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
	  false
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
  io_lib:format("Fun(~s)", [lists:flatten([format_type(X)||X<-Deps])]);
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
pp_constraints([#constraint_ref{id=Id, type=Type}|Left], Separator, 
	       Level, MaxDepth, State) ->
  Cs = state__get_cs(Id, State),
  io:format("%Ref ~w ~w%", [Type, t_var_name(Id)]),
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
constraints_to_nodes([{Name, #constraint_ref{id=Id0, type=Type}}|Left],
		     N, Level, Graph, Opts, State) ->
  Id = debug_lookup_name(Id0),
  CList = state__get_cs(Id0, State),
  ThisNode = [{Name, Opt} || Opt <- [{label, 
				      lists:flatten(io_lib:format("~w", [Id]))},
				     {shape, get_shape(Type)},
				     {level, Level}]],  
  NewList = [{N, CList}],  
  {NewGraph, NewOpts, N1} = constraints_to_nodes(NewList, N + 1, Level + 1, 
						 [{Name, N}|Graph],
						 [ThisNode|Opts], State),
  constraints_to_nodes(Left, N1, Level, NewGraph, NewOpts, State);
constraints_to_nodes([], N, _Level, Graph, Opts, _State) ->
  {lists:flatten(Graph), lists:flatten(Opts), N}.
  
get_shape('fun') -> ellipse;
get_shape(clause) -> ellipse;
get_shape(conj) -> box;
get_shape(disj) -> diamond.  

-else.
constraints_to_dot_scc(_Scc, _State) ->
  ok.
-endif.
