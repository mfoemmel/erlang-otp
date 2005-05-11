%%% -*- erlang -*-
%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : cerl_typesig.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description : 
%%%
%%% 
%%% Created : 12 Jan 2005 by Tobias Lindahl <tobiasl@it.uu.se>
%%%
%%% $Id: cerl_typesig.erl,v 1.33 2005/05/04 15:17:37 tobiasl Exp $
%%%-------------------------------------------------------------------

-module(cerl_typesig).

-export([
	 get_export_signatures/1, 
	 doit/1, 
	 core_transform/2
	]).

-import(erl_types, 
	[t_any/0, t_atom/0, t_binary/0, t_bool/0, t_cons/2,
	 t_cons_hd/1, t_cons_tl/1, t_components/1, t_float/0,
	 t_is_float/1, t_from_range/2, t_fun/0, t_fun/1, t_fun/2, t_fun_args/1,
	 t_fun_range/1, t_is_fun/1, t_improper_list/0, t_inf/1,
	 t_inf/2, t_inf_lists/2, t_integer/0, t_is_integer/1,
	 t_integers/1,
	 t_is_atom/1, t_atom_vals/1, t_is_cons/1, t_is_equal/2,
	 t_is_improper_list/1, t_is_list/1, t_is_tuple/1, t_is_none/1,
	 t_is_any/1, t_is_subtype/2, t_limit/2, t_list_elements/1,
	 t_number/0, t_is_number/1, t_number_vals/1, t_pid/0, t_port/0,
	 t_product/1, t_ref/0, t_to_string/1, t_subtract/2, t_subst/2,
	 t_tuple/0,
	 t_tuple/1, t_tuple_args/1, t_tuple_arity/1, t_sup/1, t_sup/2,
	 t_unify/2, t_is_var/1, t_var/1, t_var_name/1, t_from_term/1, 
	 t_none/0]).

-import(cerl, 
	[ann_c_fun/3, ann_c_var/2, alias_pat/1, alias_var/1,
	 apply_args/1, apply_op/1, atom_val/1, bitstr_size/1,
	 bitstr_val/1, bitstr_type/1, bitstr_flags/1,
	 binary_segments/1, c_letrec/2, c_nil/0, c_values/1,
	 call_args/1, call_module/1, call_name/1, case_arg/1,
	 case_clauses/1, catch_body/1, clause_body/1, clause_guard/1,
	 clause_pats/1, concrete/1, cons_hd/1, cons_tl/1, fun_arity/1,
	 fun_body/1, fun_vars/1, get_ann/1, int_val/1, is_c_atom/1,
	 is_c_fun/1, is_c_int/1, is_c_var/1, is_c_values/1, is_literal/1,
	 let_arg/1, let_body/1, let_vars/1, letrec_body/1,
	 letrec_defs/1, module_defs/1, module_exports/1, module_name/1,
	 pat_vars/1, primop_args/1, primop_name/1, receive_action/1,
	 receive_clauses/1, receive_timeout/1, seq_arg/1, seq_body/1,
	 set_ann/2, try_arg/1, try_body/1, try_evars/1, try_handler/1,
	 try_vars/1, tuple_arity/1, tuple_es/1, type/1, values_es/1,
	 var_name/1]).

-record(state, {cs, current_fun, next_label, 
		fun_map, fun_cs, fun_dep, fun_order, self_rec}).
-record(constraint, {lhs, op, rhs, deps}).
-record(constraint_list, {type, list, deps, id}).
-record(fun_var, {'fun', deps}).

%-define(DEBUG, true).
%-define(DEBUG_CONSTRAINTS, true).
%-define(DEBUG_PP, true).
%-define(DEBUG_TIME, true).
%-define(DOT, true).

-define(TYPE_LIMIT, 4).
-define(INTERNAL_TYPE_LIMIT, 5).

doit(Module) ->
  {ok, _, Code} = compile:file(Module,[to_core,binary]), 
  {Tree, Map} = analyze(Code),
  debug_pp(Tree, Map),
  pp_signatures(Tree, Map),
  ok.      

get_export_signatures(Code) ->
  {Tree, Map} = analyze(Code),
  %Exports = module_exports(Tree),
  Defs = module_defs(Tree),
  [{var_name(Var), unsafe_lookup_type(mk_var(Fun), Map)} || {Var, Fun} <- Defs].

core_transform(Code, _Opts) ->
  %% io:fwrite("Running type analysis..."),
  %% {T1,_} = statistics(runtime),
  Tree = annotate(Code),
  %% {T2,_} = statistics(runtime),
  %% io:fwrite("(~w ms).\n", [T2 - T1]),
  cerl:to_records(Tree).


%%% ============================================================================
%%%
%%%  Annotate all top level funs.
%%%
%%% ============================================================================

annotate(Code) ->
  {Tree, Map} = analyze(Code),
  DelAnn = fun (T) -> set_ann(T, delete_ann(typesig, get_ann(T))) end,
  SetType = fun (T, Map) ->
		X = lookup_type(mk_var(T), Map),
		case t_is_any(X) of
		  true -> DelAnn(T);
		  false -> set_ann(T, append_ann(typesig, X, get_ann(T)))
		end
	    end,
  Fun = fun (T) ->
	    case is_c_fun(T) of
	      true -> SetType(T, Map);
	      false -> T
	    end
	end,
  cerl_trees:map(Fun, Tree).

append_ann(Tag, Val, [X | Xs]) ->
  if is_tuple(X), size(X) >= 1, element(1, X) == Tag -> 
      append_ann(Tag, Val, Xs);
     true ->
      [X | append_ann(Tag, Val, Xs)]
  end;
append_ann(Tag, Val, []) ->
  [{Tag, Val}].

delete_ann(Tag, [X | Xs]) ->
  if is_tuple(X), size(X) >= 1, element(1, X) == Tag -> 
      delete_ann(Tag, Xs);
     true ->
      [X | delete_ann(Tag, Xs)]
  end;
delete_ann(_, []) ->
  [].

%%% ============================================================================
%%%
%%%  The analysis.
%%%
%%% ============================================================================

analyze(Code) ->
  {Tree, NextLabel} = cerl_trees:label(cerl:from_records(Code)),
  debug_make_fun_map(Tree),
  debug_pp(Tree, dict:new()),
  T = debug_time_start(traverse),
  State = traverse(Tree, state__new(NextLabel)),
  debug_time_stop(T),
  State1 = state__finalize(State),
  [pp_constrs_scc(Scc, State1) || Scc <- state__funs(State1)],
  to_dot(State1, Tree),
  {Tree, analyze_loop(state__funs(State1), State1, dict:new())}.

analyze_loop([[Fun]|Left], State, Map) ->
  Map1 = solve_fun(Fun, Map, State),
  case state__is_self_rec(Fun, State) of
    true ->
      case maps_are_equal(Map1, Map, [Fun]) of
	true -> analyze_loop(Left, State, Map1);
	false -> 
	  debug("============  Merry go around ===========\n", []),
	  analyze_loop([[Fun]|Left], State, Map1)
      end;
    false ->
      analyze_loop(Left, State, Map1)
  end;
analyze_loop([SCC|Left], State, Map) ->
  Map1 = solve_scc(SCC, Map, State),
  case maps_are_equal(Map1, Map, SCC) of
    true -> analyze_loop(Left, State, Map1);
    false -> 
      debug("============  Merry go around ===========\n", []),
      analyze_loop([SCC|Left], State, Map1)
  end;
analyze_loop([], _State, Map) ->
  Map.

traverse(Tree, State) ->
  %%io:format("Handling ~p\n", [type(Tree)]),
  case type(Tree) of
    alias ->
      Var = alias_var(Tree),
      Pat = alias_pat(Tree),
      {State1, PatVar} = traverse(Pat, State),
      State2 = state__store_conj(mk_var(Var), eq, PatVar, State1),
      {State2, mk_var(Var)};
    apply ->
      Args = apply_args(Tree),
      Arity = length(Args),
      Op = apply_op(Tree),
      {[FunRange|FunArgs], State1} = state__mk_vars(Arity + 1, State),
      FunType = t_fun(FunArgs, FunRange),
      State2 = state__store_conj(mk_var(Op), eq, FunType, State1),
      State3 = state__store_conj(FunRange, sub, mk_var(Tree), State2),
      {State4, [_|ArgTypes]} = traverse_list([Op|Args], State3),      
      State5 = state__store_conj_lists(ArgTypes, sub, FunArgs, State4),
      State6 = state__add_dependency(mk_var(Op), State5),
      {State6, FunRange};
    binary ->
      {State1, _} = traverse_list(binary_segments(Tree), State),
      {State1, t_binary()};
    bitstr ->
      %% Only care about Size and Value since the other fields are
      %% constant literals. Size must be an integer - NO, it can be 'all'
      Size = bitstr_size(Tree),
      Val = bitstr_val(Tree),      
      State1 = state__store_conj(mk_var(Size), sub, 
				 t_sup(t_integer(), t_from_term(all)), State),
      {State2, _} = traverse(Size, State1),      
      {case concrete(bitstr_type(Tree)) of
	 float -> state__store_conj(mk_var(Val), sub, t_float(), State2);
	 binary -> state__store_conj(mk_var(Val), sub, t_binary(), State2);
	 integer ->	  
	   case is_c_int(Size) of
	     true ->
	       SizeVal = int_val(Size),
	       Flags = concrete(bitstr_flags(Tree)),
	       Type = 
		case lists:member(signed, Flags) of
		  true -> 
		    t_from_range(-(1 bsl (SizeVal - 1)),
				 1 bsl (SizeVal - 1) - 1);
		  false -> 
		    t_from_range(0,1 bsl SizeVal - 1)
		end,
	       state__store_conj(mk_var(Val), sub, Type, State2);
	     false -> 
	       state__store_conj(mk_var(Val), sub, t_integer(), State2)
	   end
       end, t_any()};
    'case' ->
      Arg = case_arg(Tree),
      Clauses = filter_match_fail(case_clauses(Tree)),
      {State1, ArgVar} = traverse(Arg, State),
      handle_clauses(Clauses, mk_var(Tree), ArgVar, State1);
    call ->
      handle_call(Tree, State);
    'catch' ->
      %% XXX: Perhaps there is something to say about this.
      {State, mk_var(Tree)};
    cons ->
      Hd = cons_hd(Tree),
      Tl = cons_tl(Tree),

      {State1, [HdVar, TlVar]} = traverse_list([Hd, Tl], State),
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
      {State2, ConsVar};
    'fun' ->
      Body = fun_body(Tree),
      Vars = fun_vars(Tree),
      State1 = state__push_fun(mk_var(Tree), State),
      {State2, BodyVar} = 
	try 
	  traverse(Body, State1)
	catch
	  throw:error ->
	    {State1, t_none()}
	end,
      State3 = state__store_conj(mk_var(Tree), eq, 
				 t_fun(mk_var_list(Vars), BodyVar), State2),
      Cs = state__cs(State3),
      State4 = state__store_fun_constr(mk_var(Tree), Cs, State3),
      State5 = state__pop_fun(State4),
      State6 = state__add_mutual_dependency(mk_var(Tree), State5),
      {State6, mk_var(Tree)};
    'let' ->
      Vars = let_vars(Tree),
      Arg = let_arg(Tree),
      Body = let_body(Tree),

      {State1, ArgVars} = traverse(Arg, State),
      State2 = state__store_conj(t_product(mk_var_list(Vars)), eq, 
				 ArgVars, State1),
      traverse(Body, State2);
    letrec ->
      Defs = letrec_defs(Tree),
      Body = letrec_body(Tree),
      Funs = [Fun || {_Var, Fun} <- Defs],
      Vars = [Var || {Var, _Fun} <- Defs],
      
      State1 = state__store_funs(mk_var_list(Vars),
				 mk_var_list(Funs), State),
      {State2, _} = traverse_list(Funs, State1),
      traverse(Body, State2);
    literal ->      
      {State, mk_var_no_lit(Tree)};
    module ->
      Defs = module_defs(Tree),
      Funs = [Fun || {_Var, Fun} <- Defs],
      Vars = [Var || {Var, _Fun} <- Defs],
      
      State1 = state__store_funs(mk_var_list(Vars), mk_var_list(Funs), State),
      FoldFun = fun(Fun, AccState) ->
		    {S, _} = 
		      traverse(Fun, state__new_constraint_context(AccState)),
		    S
		end,
      lists:foldl(FoldFun, State1, Funs);
    primop ->
      case atom_val(primop_name(Tree)) of
	match_fail -> throw(error);
	raise -> throw(error);
	Other -> erlang:fault({'Unsupported primop', Other})
      end;
    'receive' ->
      Clauses = filter_match_fail(receive_clauses(Tree)),
      Timeout = receive_timeout(Tree),
      case is_c_atom(Timeout) andalso atom_val(Timeout) =:= infinity of
	false ->
	  Action = receive_action(Tree),
	  {State1, TimeoutVar} = traverse(Timeout, State),
	  TimeoutType = t_sup(t_from_term(infinity), t_integer()),
	  State2 = state__store_conj(TimeoutVar, sub, TimeoutType, State1),
	  handle_clauses(Clauses, mk_var(Tree), [], Action, State2);
	true ->
	  handle_clauses(Clauses, mk_var(Tree), [], State)
      end;
    seq ->
      Body = seq_body(Tree),
      Arg = seq_arg(Tree),
      {State1, _} = traverse(Arg, State),
      traverse(Body, State1);
    'try' ->
      Arg = try_arg(Tree),
      Vars = try_vars(Tree),
      Body = try_body(Tree),
      Handler = try_handler(Tree),

      State1 = state__new_constraint_context(State),
      {ArgBodyState, BodyVar} =
	try 
	  {State2, ArgVar} = traverse(Arg, State1),
	  {State3, BodyVar1} = traverse(Body, State2),
	  State4 = state__store_conj(t_product(mk_var_list(Vars)), eq, ArgVar,
				     State3),
	  {state__store_conj(mk_var(Tree), eq, BodyVar1, State4), BodyVar1}
	catch
	  throw:error -> {State1, t_none()}
	end,
      State6 = state__new_constraint_context(ArgBodyState),
      {HandlerState, HandlerVar} =
	try
	  traverse(Handler, State6)
	of
	  {State7, HandlerVar1} ->
	    {state__store_conj(mk_var(Tree), eq, HandlerVar1, State7), 
	     HandlerVar1}
	catch
	  throw:error -> {State6, t_none()}
	end,
      ArgBodyCs = state__cs(ArgBodyState),
      HandlerCs = state__cs(HandlerState),
      {NewCs, ReturnVar} =
	case {state__has_constraints(ArgBodyState), 
	      state__has_constraints(HandlerState)} of
	  {true, true} ->
	    Disj = mk_disj_constraint_list([ArgBodyCs, HandlerCs]),
	    {Disj, mk_var(Tree)};
	  {true, false} ->
	    {ArgBodyCs, BodyVar};
	  {false, true} ->
	    {HandlerCs, HandlerVar};
	  {false, false} ->
	    throw(error)
	end,
      OldCs = state__cs(State),
      Conj = mk_conj_constraint_list([OldCs, NewCs]),
      NewState1 = state__new_constraint_context(HandlerState),
      NewState2 = state__store_conj(Conj, NewState1),
      {NewState2, ReturnVar};
    tuple ->
      Elements = tuple_es(Tree),
      {State1, ElVars} = traverse_list(Elements, State),
      {State1, t_tuple(ElVars)};
    values ->
      Elements = values_es(Tree),
      {State1, EVars} = traverse_list(Elements, State),
      {State1, t_product(EVars)};
    var ->
      {State, mk_var(Tree)};
    Other ->
      erlang:fault({'Unsupported type', Other})
  end.

traverse_list(Trees, State) ->
  traverse_list(Trees, State, []).

traverse_list([Tree|Tail], State, Acc) ->
  {State1, Var} = traverse(Tree, State),
  traverse_list(Tail, State1, [Var|Acc]);
traverse_list([], State, Acc) ->
  {State, lists:reverse(Acc)}.

  
%%________________________________________
%%
%% Call
%%

handle_call(Call, State) ->      
  Args = call_args(Call),
  Mod = call_module(Call),
  Fun = call_name(Call),
  Dst = mk_var(Call),
  case is_c_atom(Mod) andalso is_c_atom(Fun) of
    true ->
      M = atom_val(Mod),
      F = atom_val(Fun),
      Args = call_args(Call),
      A = length(Args),
      {State1, ArgVars} = traverse_list(Args, State),
      case get_bif_constr({M, F, A}, Dst, ArgVars) of
	none -> {State1, Dst};
	C -> {state__store_conj(C, State1), Dst}
      end;
    false ->
      {state__store_conj_lists([mk_var(Mod), mk_var(Fun)], sub, 
			       [t_atom(), t_atom()], State), Dst}
  end.


get_bif_constr({erlang, Op, 2}, Dst, [Arg1, Arg2]) when ((Op == '+') or 
							 (Op == '-') or 
							 (Op == '*')) ->
  DstFun = 
    fun(Map)->
	Arg1Type = lookup_type(Arg1, Map),
	Arg2Type = lookup_type(Arg2, Map),
	case t_is_integer(Arg1Type) andalso t_is_integer(Arg2Type) of
	  true ->
	    Vals1 = t_number_vals(Arg1Type),
	    Vals2 = t_number_vals(Arg2Type),
	    case (Vals1 =:= any) orelse (Vals2 =:= any) of
	      true -> t_integer();
	      false ->
		AllCombs = [{X, Y} || X <- Vals1, Y <- Vals2],
		AllRes = [eval_arith(Op, X, Y) || {X, Y} <- AllCombs],
		t_integers(AllRes)
	    end;
	  false ->
	    case t_is_float(Arg1Type) orelse t_is_float(Arg2Type) of
	      true -> t_float();
	      false -> t_number()
	    end
	end
    end,
  DstFunVar = mk_fun_var(DstFun, [Arg1, Arg2]),
  
  ArgFun = 
    fun(A, Pos) ->
	F = 
	  fun(Map)->
	      DstType = lookup_type(Dst, Map),
	      AType = lookup_type(A, Map),
	      case t_is_integer(DstType) of
		true ->
		  case t_is_integer(AType) of
		    true -> 
		      DstVals = t_number_vals(DstType),
		      AVals = t_number_vals(AType),
		      case (DstVals =:= any) orelse (AVals =:= any) of
			true -> t_integer();
			false ->
			  AllCombs = [{X, Y} || X <- DstVals, Y <- AVals],
			  AllRes = [eval_inv_arith(Op, X, Y, Pos) 
				    || {X, Y} <- AllCombs],
			  t_integers(AllRes)
		    end;
		    false  ->
		      t_integer()
		  end;
		false ->
		  case t_is_integer(DstType) of
		    true -> t_integer();
		    false -> t_number()
		  end
	      end
	  end,
	mk_fun_var(F, [Dst, A])
    end,
  Arg1FunVar = ArgFun(Arg2, 2),
  Arg2FunVar = ArgFun(Arg1, 1),
  mk_conj_constraint_list([mk_constraint(Arg1, sub, Arg1FunVar),
			   mk_constraint(Arg2, sub, Arg2FunVar),
			   mk_constraint(Dst, sub, DstFunVar)]);
get_bif_constr({M, F, A}, Dst, Args) ->
  GenType = erl_bif_types:type(M, F, A),
  case t_is_none(GenType) of
    true -> throw(error);
    false ->
      ArgTypes = erl_bif_types:arg_types(M, F, A),
      ReturnType = mk_fun_var(fun(Map)-> 
				  TmpArgTypes = lookup_type_list(Args, Map),
				  erl_bif_types:type(M, F, A, TmpArgTypes)
			      end, Args),
      case ArgTypes =:= any of
	true -> 
	  case t_is_any(GenType) of
	    true -> 
	      none;
	    false ->
	      mk_constraint(Dst, sub, ReturnType)
	  end;
	false -> 
	  Cs = mk_constraints(Args, sub, ArgTypes),
	  mk_conj_constraint_list([mk_constraint(Dst, sub, ReturnType)|Cs])
      end
  end.

eval_arith('+', X, Y) -> X + Y;
eval_arith('*', X, Y) -> X * Y;
eval_arith('-', X, Y) -> X - Y.

eval_inv_arith('+', Dst, A, _Pos) -> Dst - A;
eval_inv_arith('*', Dst, A, _Pos) -> Dst div A;
eval_inv_arith('-', Dst, A, 1) -> A - Dst;
eval_inv_arith('-', Dst, A, 2) -> A + Dst.

filter_match_fail([Clause]) ->
  Body = clause_body(Clause),
  case type(Body) of
    primop ->
      case atom_val(primop_name(Body)) of
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


handle_clauses(Clauses, TopVar, Arg, State) ->
  handle_clauses(Clauses, TopVar, Arg, none, State).

handle_clauses([], _TopVar, _Arg, Action, State) when Action =/= none->
  %% Can happen when a receive has no clauses, see filter_match_fail.
  traverse(Action, State);
handle_clauses(Clauses, TopVar, Arg, Action, State) ->
  {State1, CList} = handle_clauses_1(Clauses, TopVar, Arg, State, []),
  case Action of
    none -> 
      NewCs = mk_disj_constraint_list(CList),
      State2 = State1;
    _ -> 
      {TmpState, ActionVar} = traverse(Action, State1),
      TmpCs =
	mk_conj_constraint_list([state__cs(TmpState),
				 mk_constraint(TopVar, eq, ActionVar)]),
      NewCs = mk_disj_constraint_list([TmpCs|CList]),
      State2 = state__new_constraint_context(TmpState)
  end,
  OldCs = state__cs(State),
  {state__store_conj_list([OldCs, NewCs], State2), TopVar}.

handle_clauses_1([Clause|Tail], TopVar, Arg, State, Acc) ->
  State1 = state__new_constraint_context(State),
  Pats = clause_pats(Clause),
  Guard = clause_guard(Clause),
  Body = clause_body(Clause),
  {State2, PatVars} = traverse_list(Pats, State1),
  case Arg of
    [] ->
      State3 = State2;
    _ ->
      State3 = state__store_conj(Arg, eq, 
				 t_product(PatVars), State2)
  end,
  try 
    State4 = handle_guard(Guard, State3),
    traverse(Body, State4)
    of
    {State5, BodyVar} -> 
      C = mk_constraint(TopVar, eq, BodyVar),
      Cs = mk_conj_constraint_list([state__cs(State5), C]),
      handle_clauses_1(Tail, TopVar, Arg, State5, [Cs|Acc])
  catch
    throw:error -> handle_clauses_1(Tail, TopVar, Arg, State, Acc)
  end;
handle_clauses_1([], _TopVar, _Arg, State, Acc) ->
  {state__new_constraint_context(State), Acc}.



%%________________________________________
%%
%% Guards
%%

handle_guard(Guard, State) ->  
  try handle_guard(Guard, State, dict:new()) of
      State1 -> State1
  catch
    throw:dont_know -> State
  end.

handle_guard(Guard, State, Env) ->
  %%  debug("Handling: ~w\n", [type(Guard)]),
  case type(Guard) of
    binary -> 
      State;    
    cons ->
      %% XXX: Might want to handle the elements
      State;
    literal ->
      State;
    'try' ->
      State1 = handle_guard(try_arg(Guard), State, Env),
      handle_guard(try_body(Guard), State1, Env);
    tuple ->
      %% XXX: Might want to handle the elements
      State;
    'let' ->
      Arg = let_arg(Guard),
      [Var] = let_vars(Guard),
      %%debug("Storing: ~w\n", [Var]),
      NewEnv = dict:store(get_label(Var), Arg, Env),
      handle_guard(let_body(Guard), State, NewEnv);
    var ->
      %%debug("Looking for: ~w...", [Guard]),
      case dict:find(get_label(Guard), Env) of
	error -> 
	  %debug("Did not find it\n", []),
	  State;
	{ok, Tree} -> 
	  %debug("Found it\n", []),
	  handle_guard(Tree, State, Env)
      end;
    call ->
      Args = call_args(Guard),      
      M = atom_val(call_module(Guard)),
      F = atom_val(call_name(Guard)),
      A = length(Args),      
      case {M, F, A} of
	{erlang, is_atom, 1} ->
	  [Arg] = Args,
	  state__store_conj(mk_var(Arg), sub, t_atom(), State);
	{erlang, is_boolean, 1} ->
	  [Arg] = Args,
	  state__store_conj(mk_var(Arg), sub, t_bool(), State);
	{erlang, is_binary, 1} ->
	  [Arg] = Args,
	  state__store_conj(mk_var(Arg), sub, t_binary(), State);
	{erlang, is_float, 1} ->
	  [Arg] = Args,
	  state__store_conj(mk_var(Arg), sub, t_float(), State);
	{erlang, is_function, 1} ->
	  [Arg] = Args,
	  state__store_conj(mk_var(Arg), sub, t_fun(), State);
	{erlang, is_integer, 1} ->
	  [Arg] = Args,
	  state__store_conj(mk_var(Arg), sub, t_integer(), State);
	{erlang, is_list, 1} ->
	  [Arg] = Args,
	  state__store_conj(mk_var(Arg), sub, t_improper_list(),State);
	{erlang, is_number, 1} ->
	  [Arg] = Args,
	  state__store_conj(mk_var(Arg), sub, t_number(), State);
	{erlang, is_pid, 1} ->
	  [Arg] = Args,
	  state__store_conj(mk_var(Arg), sub, t_pid(), State);
	{erlang, is_port, 1} ->
	  [Arg] = Args,
	  state__store_conj(mk_var(Arg), sub, t_port(), State);
	{erlang, internal_is_record, 3} ->
	  [Rec, Tag, Arity] = Args,
	  TagType = t_from_term(atom_val(Tag)),
	  OtherEs = duplicate(int_val(Arity) - 1, t_any()),
	  TupleType = t_tuple([TagType|OtherEs]),
	  state__store_conj(mk_var(Rec), sub, TupleType, State);
	{erlang, is_reference, 1} ->
	  [Arg] = Args,
	  state__store_conj(mk_var(Arg), sub, t_ref(), State);
	{erlang, is_tuple, 1} ->
	  [Arg] = Args,
	  state__store_conj(mk_var(Arg), sub, t_tuple(), State);
	{erlang, '=:=', 2} ->
	  [Arg1, Arg2] = Args,	  
	  State1 = handle_guard(Arg1, State, Env),
	  State2 = handle_guard(Arg2, State1, Env),
	  state__store_conj(mk_var(Arg1), eq, mk_var(Arg2), State2);
%%%	{erlang, '==', 2} ->
	{erlang, 'and', 2} ->
	  [Arg1, Arg2] = Args,
	  State1 = handle_guard(Arg1, State, Env),
	  handle_guard(Arg2, State1, Env);
	{erlang, 'or', 2} ->
	  [Arg1, Arg2] = Args,
	  State1 = state__new_constraint_context(State),
	  State2 = handle_guard(Arg1, State1, Env),
	  State3 = state__new_constraint_context(State2),
	  State4 = handle_guard(Arg2, State3, Env),
	  State5 = state__new_constraint_context(State4),
	  
	  OldCs = state__cs(State),
	  Cs1 = state__cs(State2),
	  Cs2 = state__cs(State4),

	  Disj = mk_disj_constraint_list([Cs1, Cs2]),
	  Conj = mk_conj_constraint_list([OldCs, Disj]),
	  state__store_conj(Conj, State5);
	{erlang, 'not', 1} ->
	  throw(dont_know);
	_Other ->
	  State1 = 
	    lists:foldl(fun(X, AccMap)->
			    handle_guard(X, AccMap, Env)
			end,
			State, Args),
	  {State2, _} = handle_call(Guard, State1),
	  State2
      end
  end.

%%% ============================================================================
%%%
%%%  Constraint solver.
%%%
%%% ============================================================================

solve_fun(Fun, Map, State) ->
  FunName = debug_fun_name(Fun),
  T = debug_time_start({analyze, FunName}),
  time_debug("Solving fun: ~p\n", [FunName]),
  Cs = state__fun_cs(Fun, State),
  case state__is_self_rec(Fun, State) of
    true ->
      debug("Solving self recursive\n", []),
      {ok, Var1} = state__get_rec_var(Fun, State),
      RecType =  t_limit(unsafe_lookup_type(Fun, Map),?TYPE_LIMIT),
      Map1 = enter_type(Var1, RecType, dict:erase(t_var_name(Fun), Map));
    false ->
      Map1 = Map
  end,
  debug("Fun: ~s\n", [t_to_string(lookup_type(Fun, Map1))]),
  case solve_1(Cs, Map1) of
    error -> NewType = t_none();
    {ok, NewMap} -> NewType = unsafe_lookup_type(Fun, NewMap)
  end,
  Res = 
    case state__get_rec_var(Fun, State) of
      {ok, Var2} ->
	enter_type(Var2, NewType, enter_type(Fun, NewType, Map));
      error ->
	enter_type(Fun, NewType, Map)
    end,
  debug_time_stop(T),
  Res.

solve_scc(Scc, Map, State) ->
  SccNames = [debug_fun_name(X)||X<-Scc],
  T  = debug_time_start({analyze, SccNames}),
  Cs = [state__fun_cs(Fun, State)||Fun <- Scc],
  time_debug("Solving Scc: ~p\n", [SccNames]),
  Vars0 = [{Fun, state__get_rec_var(Fun, State)}||Fun <- Scc],  
  Vars = [Var || {_, {ok, Var}} <- Vars0],
  Funs = [Fun || {Fun, {ok, _}} <- Vars0],
  Types = unsafe_lookup_type_list(Funs, Map),
  RecTypes = [t_limit(T, ?TYPE_LIMIT) || T <- Types],
  CleanMap = lists:foldl(fun(Fun, AccMap)->
			     dict:erase(t_var_name(Fun), AccMap)
			 end, Map, Scc),
  Map1 = enter_type_lists(Vars, RecTypes, CleanMap),
  SolveFun = 
    fun({F, Cs}, TmpMap) ->
	case solve_1(Cs, TmpMap) of
	  {ok, TmpMap1} ->
	    NewType = unsafe_lookup_type(F, TmpMap1);
	  error ->
	    NewType = t_none()
	end,
	TmpMap2 = enter_type(F, NewType, TmpMap),
	case state__get_rec_var(F, State) of
	  {ok, R} -> enter_type(R, t_limit(NewType, ?TYPE_LIMIT), TmpMap2);
	  error -> TmpMap2
	end
    end,
  Res = lists:foldl(SolveFun, Map1, lists:zip(Scc, Cs)),
  debug_time_stop(T),
  Res.

solve_1(Cs, Map) ->
  case solve_1(Cs, Map, dict:new()) of
    {error, _} -> error;
    {ok, _, Map1} -> {ok, Map1}
  end.

solve_1(#constraint_list{type=Type, list=Cs, deps = Deps, id=Id}, 
	Map, MapDict) ->
  {OldLocalMap, Check} = 
    case dict:find(Id, MapDict) of
      error -> {dict:new(), false};
      {ok, M} -> {M, true}
    end,
  %%%io:format("Checking list in: ~w\n", [Id]),
  case Check andalso maps_are_equal(OldLocalMap, Map, Deps) of
    true -> 
      %%%io:format("Equal\n", []),
      Type = disj, %% Sanity check.
      {ok, MapDict, Map};
    false ->
      %%%io:format("Not equal\n", []),
      Fun1 = fun(X, Acc) -> enter_type(X, lookup_type(X, Map), Acc)end,
      MapIn = lists:foldl(Fun1, dict:new(), Deps),
      case solve_clist(Cs, Type, Id, Deps, MapDict, MapIn) of
	{error, _} = Error -> 
	  %%%io:format("Failed\n", []),
	  Error;
	{ok, MapDict1, NewLocalMap} ->
	  %%%io:format("Done: ~w\n", [Id]),
	  NewMapDict = 
	    case Type of
	      conj -> MapDict1;
	      disj -> dict:store(Id, NewLocalMap, MapDict1)
	    end,
	  %% This is safe since types defined above cannot grow.
	  NewMap = dict:merge(fun(_, _, V) -> V end, Map, NewLocalMap),
	  {ok, NewMapDict, NewMap}
      end
  end;
solve_1(C = #constraint{}, Map, MapDict) ->
  case solve_cs([C], MapDict, Map) of
    {ok, _, _, NewMap} -> {ok, MapDict, NewMap};
    {error, _} -> {error, MapDict}
  end.

solve_clist(Cs, conj, Id, Deps, MapDict, Map) ->
  case solve_cs(Cs, Map, MapDict) of 
    {error, NewMapDict1} ->	  
      {error, dict:erase(Id, NewMapDict1)};
    {ok, NewCs, NewDict, Map1} -> 
      case maps_are_equal(Map, Map1, Deps) of
	true -> 
	  {ok, NewDict, Map1};
	false -> 
	  %%%io:format("Looping: ~w\n", [Id]),	  
	  solve_clist(NewCs, conj, Id, Deps, NewDict, Map1)
      end
  end;
solve_clist(Cs, disj, _Id, _Deps, MapDict, Map) ->
  Fun = fun(C, Dict) ->
	    case solve_1(C, Map, Dict) of
	      {error, NewDict} -> {error, NewDict};
	      {ok, NewDict, NewMap} -> {{ok, NewMap}, NewDict}
	    end
	end,  
  {Maps, NewDict} = lists:mapfoldl(Fun, MapDict, Cs),
  case [X || {ok, X} <- Maps] of
    [] -> {error, NewDict};
    MapList -> 
      NewMap = join_maps(MapList),
      {ok, NewDict, NewMap}
  end.

solve_cs(List, Map, MapDict) ->
  solve_cs(List, Map, MapDict, []).

solve_cs([C = #constraint_list{type=conj, list=List}|Tail], Map, 
	 MapDict, Acc) ->
  case solve_1(List, Map, MapDict) of
    {ok, NewMapDict, Map1} -> solve_cs(Tail, Map1, NewMapDict, [C|Acc]);
    {error, NewMapDict} -> {error, NewMapDict}
  end;
solve_cs([C = #constraint_list{type=disj}|Tail], Map, MapDict, Acc) ->
  case solve_1(C, Map, MapDict) of
    {error, NewMapDict} -> {error, NewMapDict};
    {ok, NewMapDict, Map1} -> solve_cs(Tail, Map1, NewMapDict, [C|Acc])
  end;
solve_cs([C|Tail], Map, MapDict, Acc) when is_record(C, constraint) ->  
  Lhs = C#constraint.lhs,
  Rhs = C#constraint.rhs,
  Op  = C#constraint.op,
  LhsType = lookup_type(Lhs, Map),
  RhsType = lookup_type(Rhs, Map),
  Inf = t_inf(LhsType, RhsType),
  debug("Solving: ~s :: ~s ~w ~s :: ~s\n\tInf: ~s\n",
	[format_type(Lhs), format_type(LhsType), Op,
	 format_type(Rhs), format_type(RhsType), format_type(Inf)]),
  debug("Map: ~p\n", [[{X, format_type(Y)}||{X, Y}<-dict:to_list(Map)]]),
  
  Res = 
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
    end,
  case Res of
    error ->
      debug("+++++++++++\nFailed: ~s :: ~s ~w ~s :: ~s\n+++++++++++\n",
	    [format_type(Lhs), format_type(LhsType), Op,
	     format_type(Rhs), format_type(RhsType)]),
      {error, MapDict};
    {ok, NewMap} -> 
      case is_literal(Rhs) of
	true -> 
%	  debug("Removing: ", []),
%	  pp_constraints([C]),
%	  debug("\n", []),
	  solve_cs(Tail, NewMap, MapDict, Acc);
	false -> 
	  case is_literal(Lhs) andalso (Op =:= eq) of
	    true ->
%	      debug("Removing: ", []),
%	      pp_constraints([C]),
%	      debug("\n", []),
	      solve_cs(Tail, NewMap, MapDict, Acc);
	    false ->
%	      debug("Keeping: ~w  ~w\n", [Lhs, Rhs]),
%	      pp_constraints([C]),
%	      debug("\n", []),
	      solve_cs(Tail, NewMap, MapDict, [C|Acc])
	  end
      end
  end;
solve_cs([], Map, MapDict, Acc) ->
  {ok, Acc, MapDict, Map}.

solve_subtype(Type, Inf, Map) ->
  case is_literal(Type) of
    true -> 
      case t_is_subtype(t_from_term(concrete(Type)), Inf) of
	true -> {ok, Map};
	false -> error
      end;
    false ->
      try t_unify(Type, Inf) of
	  {_, List} ->
	  {Vars, Types} = lists:unzip(List),
	  NewTypes = t_inf_lists(Types, lookup_type_list(Vars, Map)),
	  case any_none(NewTypes) of
	    true -> 
	      error;
	    false ->	      
	      {ok, enter_type_lists(Vars, NewTypes, Map)}
	  end
      catch
	throw:{mismatch, _, _} -> error
      end
  end.


%%% ============================================================================
%%%
%%%  Maps and types.
%%%
%%% ============================================================================

join_maps(Maps) ->
  %Time = debug_time_start(join_maps),
  debug("Joining maps:\n", []),
  [debug("\tMap: ~p\n", [[{X, format_type(Y)}||{X, Y}<-dict:to_list(Map)]])
   || Map <- Maps],
  Fun = fun(T, Map1, Map2)->
	    t_sup(lookup_type(T, Map1), lookup_type(T, Map2))
	end,
  Keys = lists:foldl(fun(TmpMap, AccKeys) -> 
			 Keys1 = ordsets:from_list(dict:fetch_keys(TmpMap)),
			 ordsets:intersection(Keys1, AccKeys)
		     end, 
		     ordsets:from_list(dict:fetch_keys(hd(Maps))), tl(Maps)),
  Res = merge_maps(Maps, Fun, Keys),
  debug("Result:\n", []),
  debug("\tMap: ~p\n", [[{X, format_type(Y)}||{X, Y}<-dict:to_list(Res)]]),
						%debug_time_stop(Time),
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
  case  T1 =:= T2 of
    true -> maps_are_equal(Map1, Map2, Tail);
    false -> 
      %io:format("Failed: ~s :: ~s =/= ~s\n", [format_type(H), format_type(T1),
%					      format_type(T2)]),
      false
  end;
maps_are_equal(_Map1, _Map2, []) ->
  true.


enter_type(Key, Val, Map) when is_integer(Key) ->
  debug("Entering ~s :: ~s\n", [format_type(t_var(Key)), format_type(Val)]),
  case t_is_any(Val) of
    true ->
      dict:erase(Key, Map);
    false ->
      dict:store(Key, t_limit(Val, ?INTERNAL_TYPE_LIMIT), Map)
  end;
enter_type(Key, Val, Map) ->
  debug("Entering ~s :: ~s\n", [format_type(Key), format_type(Val)]),
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
  case is_literal(Key) of
    true -> t_from_term(concrete(Key));
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
  case is_literal(Var) of
    true -> Var;
    false -> 
      case is_c_values(Var) of
	true -> t_product(mk_var_no_lit_list(values_es(Var)));
	false -> t_var(get_label(Var))
      end
  end.

mk_var_list(List) ->
  [mk_var(X)||X<-List].

mk_var_no_lit(Var) ->
  case is_literal(Var) of
    true -> t_from_term(concrete(Var));
    false -> mk_var(Var)
  end.

mk_var_no_lit_list(List) ->
  [mk_var_no_lit(X)||X<-List].


get_label(T) ->
  case get_ann(T) of
    [{label, L} | _] -> L;
    _ -> erlang:fault({missing_label, T})
  end.

any_none([X | Xs]) ->
  case t_is_none(X) of
    true ->
      true;
    false ->
      any_none(Xs)
  end;
any_none([]) -> false.



%%% ============================================================================
%%%
%%%  The State.
%%%
%%% ============================================================================

state__new(NextLabel) -> 
  #state{cs=[], current_fun=[], fun_cs=dict:new(), fun_dep=[],
	 fun_map=[], next_label=NextLabel}.

state__new_constraint_context(State) ->
  State#state{cs=[]}.

%state__set_constraints(Cs, State) ->
%  State#state{cs=[Cs]}.

state__cs(#state{cs=Cs}) ->
  mk_conj_constraint_list(Cs).

state__has_constraints(#state{cs=[]}) -> false;
state__has_constraints(#state{}) -> true.

state__store_conj(C, State = #state{cs=Cs}) ->
  State#state{cs=[C|Cs]}.

state__store_conj_list([H|T], State) ->
  State1 = state__store_conj(H, State),
  state__store_conj_list(T, State1);
state__store_conj_list([], State) ->
  State.

state__store_conj(Lhs, Op, Rhs, State = #state{cs=Cs}) ->
  State#state{cs=[mk_constraint(Lhs, Op, Rhs)|Cs]}.

state__store_conj_lists([Arg1|Arg1Tail], Op, [Arg2|Arg2Tail], State) ->
  State1 = state__store_conj(Arg1, Op, Arg2, State),
  state__store_conj_lists(Arg1Tail, Op, Arg2Tail, State1);
state__store_conj_lists([], _Op, [], State) ->
  State.

state__mk_vars(N, State = #state{next_label=NL}) ->
  NewLabel = NL + N,
  {[t_var(X)||X<-lists:seq(NL, NewLabel-1)], 
   State#state{next_label=NewLabel}}.

state__store_fun_constr(Fun, Cs, State = #state{fun_cs=Dict}) ->
  Cs1 = enumerate_constraints(Cs),
  NewDict = dict:store(Fun, Cs1, Dict),
  State#state{fun_cs=NewDict}.

state__fun_cs(Var, #state{fun_cs=Dict}) ->  
  dict:fetch(Var, Dict).

state__store_funs(Vars, Funs, State = #state{fun_map=Map}) ->
  NewMap = 
    lists:foldl(fun({Var, Fun}, TmpMap) -> orddict:store(Var, Fun, TmpMap)end,
		Map, lists:zip(Vars, Funs)),
  State#state{fun_map=NewMap}.

state__push_fun(Fun, State = #state{current_fun=FunList}) ->
  State#state{current_fun=[Fun|FunList]}.

state__pop_fun(State = #state{current_fun=[_|FunList]}) ->
  State#state{current_fun=FunList}.

state__add_mutual_dependency(_Fun1, State=#state{current_fun=[]}) ->
  %% If there is no parent function we do not have to add any dependency.
  State;
state__add_mutual_dependency(Fun1, State=#state{fun_dep=Dep, 
						current_fun=[Fun2|_]}) ->
  NewDep1 = ordsets:add_element({Fun1, Fun2}, Dep),
  NewDep2 = ordsets:add_element({Fun2, Fun1}, NewDep1),
  State#state{fun_dep=NewDep2}.

state__add_dependency(Op, State = #state{fun_dep=Dep, current_fun=[Fun|_],
					 fun_map=Map}) ->
  case orddict:find(Op, Map) of
    error ->
      %% This is not a known fun. Don't care.
      State;
    {ok, Fun2} ->
      NewDep = ordsets:add_element({Fun, Fun2}, Dep),
      State#state{fun_dep=NewDep}
  end.

state__funs(#state{fun_order=Funs}) ->
  Funs.

state__calculate_fun_order(State = #state{fun_dep=FunDep, fun_map=Map}) ->
  SelfRec = [X || {X, X} <- FunDep],
  CallGraph = hipe_digraph:from_list(FunDep),
  CallGraph1 = lists:foldl(fun(X, Acc)->
			       hipe_digraph:add_node(X, Acc)
			   end, CallGraph, [X || {_, X}<-Map]),
  Order = hipe_digraph:reverse_preorder_sccs(CallGraph1),
  State#state{fun_order=Order, self_rec=ordsets:from_list(SelfRec)}.

state__finalize(State) ->
  state__calculate_fun_order(State).

state__is_self_rec(Fun, #state{self_rec=SelfRec}) ->
  ordsets:is_element(Fun, SelfRec).

state__get_rec_var(Fun, #state{fun_map=Map}) ->
  case [X || {X, Y} <- Map, Y =:= Fun] of
    [Var] -> {ok, Var};
    [] -> error
  end.

%%% ============================================================================
%%%
%%%  Constraints
%%%
%%% ============================================================================


mk_constraint(Lhs, Op, Rhs) ->
  case t_is_any(Lhs) orelse t_is_any(Rhs) of
    false ->
      Deps = find_constraint_deps([Lhs, Rhs]),
      C = mk_constraint_1(Lhs, Op, Rhs),
      C#constraint{deps=Deps};
    true ->
      C = mk_constraint_1(t_any(), Op, t_any()),
      C#constraint{deps=[]}
  end.

mk_fun_var(Fun, Deps) ->
  #fun_var{'fun'=Fun, deps=ordsets:from_list(filter_vars(Deps))}.

get_deps(#constraint{deps=D}) -> D;
get_deps(#constraint_list{deps=D}) -> D.


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
	      true -> t_tuple_args(Type);
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

mk_constraint_list(Type, List) ->
  List1 = ordsets:from_list(lift_lists(Type, List)),
  List2 = ordsets:filter(fun(X)->get_deps(X)=/=[]end, List1),
  Deps = calculate_deps(List2),
  case Deps == [] of
    true -> mk_constraint(t_any(), eq, t_any());
    false -> #constraint_list{type = Type, list = List2, deps=Deps}
  end.
      

lift_lists(Type, List) ->
  lift_lists(Type, List, []).

lift_lists(Type, [#constraint_list{type=Type, list=List}|Tail], Acc) ->
  lift_lists(Type, Tail, List++Acc);
lift_lists(Type, [C|Tail], Acc) ->
  lift_lists(Type, Tail, [C|Acc]);
lift_lists(_Type, [], Acc) ->
  Acc.

calculate_deps(List) ->
  calculate_deps(List, []).

calculate_deps([#constraint{deps=Deps}|Tail], Acc) ->
  calculate_deps(Tail, ordsets:union(Deps, Acc));
calculate_deps([#constraint_list{deps=Deps}|Tail], Acc) ->
  calculate_deps(Tail, ordsets:union(Deps, Acc));
calculate_deps([], Acc) ->
  Acc.

mk_conj_constraint_list(List) ->
  mk_constraint_list(conj, List).

mk_disj_constraint_list([NotReallyAList]) ->
  NotReallyAList;
mk_disj_constraint_list(List) ->
  mk_constraint_list(disj, List).


enumerate_constraints(Cs) ->
  {[Cs1], _} = enumerate_constraints([Cs], 0, []),
  Cs1.

enumerate_constraints([C = #constraint_list{list=List}|Tail], N, Acc) ->
  {NewList, NewN} = enumerate_constraints(List, N, []),
  NewAcc = [C#constraint_list{list=NewList, id=NewN}|Acc],
  enumerate_constraints(Tail, NewN+1, NewAcc);
enumerate_constraints([C = #constraint{}|Tail], N, Acc) ->
  enumerate_constraints(Tail, N, [C|Acc]);
enumerate_constraints([], N, Acc) ->
  {lists:reverse(Acc), N}.


%%% ============================================================================
%%%
%%%  Utilities.
%%%
%%% ============================================================================

format_type(#fun_var{deps=Deps}) ->
  io_lib:format("Fun(~s)", [lists:flatten([format_type(X)||X<-Deps])]);
format_type(Type) ->
  case is_literal(Type) of
    true -> io_lib:format("~w", [concrete(Type)]);
    false -> t_to_string(Type)
  end.
      


duplicate(0, _Element) ->
  [];
duplicate(N, Element) ->
  [Element|duplicate(N-1, Element)].


pp_signatures(Tree, Map) ->
  ModuleName = atom_val(module_name(Tree)),
  io:nl(),
  PPFun = fun({Var, Body}) ->
	      {FunName, Arity} = var_name(Var),
	      Type = format_type(unsafe_lookup_type(mk_var(Body), Map)),
	      io:format("~w:~w/~w :: ~s\n", [ModuleName, FunName, Arity, Type])
	  end,
  lists:foreach(PPFun, module_defs(Tree)).



-ifdef(DEBUG).
-ifndef(DEBUG_PP).
-define(DEBUG_PP, true).
-endif.
-endif.

-ifdef(DEBUG).
debug(String, Args) -> 
  io:format(String, Args).
time_debug(String, Args) -> 
  io:format(String, Args).
-else.
debug(_String, _Args) -> 
  ok.
time_debug(_String, _Args) -> 
  ok.
-endif.

-ifdef(DEBUG_CONSTRAINTS).
pp_constrs_scc(Scc, State) ->
  [pp_constrs(Fun, state__fun_cs(Fun, State))||Fun <- Scc].

pp_constrs(Fun, Cs) ->
  io:format("Constraints for fun: ~p\n", [Fun]),
  MaxDepth = pp_constraints(Cs),
  io:format("Depth: ~w\n", [MaxDepth]).


pp_constraints(Cs) ->
  Res = pp_constraints([Cs], none, 0, 0),
  io:nl(),
  Res.

pp_constraints([List|Tail], Separator, Level, MaxDepth) when is_list(List) ->
  pp_constraints(List++Tail, Separator, Level, MaxDepth);
pp_constraints([#constraint{lhs=Lhs, op=Op, rhs=Rhs}], _Separator, 
	       Level, MaxDepth) ->
  io:format("~s ~w ~s", [format_type(Lhs), Op, format_type(Rhs)]),
  lists:max([Level, MaxDepth]);
pp_constraints([#constraint{lhs=Lhs, op=Op, rhs=Rhs}|Tail], Separator,
	       Level, MaxDepth) ->
  io:format("~s ~w ~s ~s ", [format_type(Lhs), Op, format_type(Rhs),Separator]),
  pp_constraints(Tail, Separator, Level, MaxDepth);
pp_constraints([#constraint_list{type=Type, list=List}], _Separator, 
	       Level, MaxDepth) ->
  io:format("(", []),
  NewSeparator = case Type of
		   conj -> "*";
		   disj -> "+"
		 end,
  NewMaxDepth = pp_constraints(List, NewSeparator, Level + 1, MaxDepth),
  io:format(")", []),
  NewMaxDepth;
pp_constraints([#constraint_list{type=Type, list=List}|Tail],Separator,
	       Level, MaxDepth) ->
  io:format("(", []),
  NewSeparator = case Type of
		   conj -> "*";
		   disj -> "+"
		 end,
  NewMaxDepth = pp_constraints(List, NewSeparator, Level+1, MaxDepth),
  io:format(") ~s\n~s ", [Separator, Separator]),
  pp_constraints(Tail, Separator, Level, NewMaxDepth).
-else.
pp_constrs_scc(_Scc, _State) ->
  ok.

%pp_constraints(_) ->
%  ok.
-endif.


-ifdef(DEBUG_PP).
debug_pp(Tree, Map) -> 
  io:put_chars(cerl_prettypr:format(Tree)),
  io:nl(),
  [io:format("~w :: ~s\n", [Var, format_type(Type)])
   ||{Var, Type} <- dict:to_list(Map)],
  ok.
  %tree_to_dot(Tree, Map).


-else.
debug_pp(_Tree, _Map) -> 
  ok.
-endif.

-ifdef(DEBUG_TIME).
debug_time_start(X) ->
  case get(current) of
    {T, X} -> {T, X};
    _ ->
      {Now, _Since} = statistics(runtime),
      io:format("~w: start\n", [X]),
      put(current, {Now, X}),
      {Now, X}
  end.

debug_time_stop({Start, X}) ->
  {Now, _} = statistics(runtime),
  io:format("~w: done in ~.2f secs\n", [X, (Now-Start)/1000]).


debug_make_fun_map(Tree) ->
  ModuleName = atom_val(module_name(Tree)),
  Fun = fun({Var, Body}, Acc) ->
	    {FunName, Arity} = var_name(Var),
	    Name = io_lib:format("~w:~w/~w", [ModuleName, FunName, Arity]),
	    dict:store(mk_var(Body), list_to_atom(lists:flatten(Name)), Acc)
	end,
  Res = lists:foldl(Fun, dict:new(), module_defs(Tree)),
  put(debug_fun_names, Res).

debug_fun_name(Fun) ->
  Map = get(debug_fun_names),
  case dict:find(Fun, Map) of
    error -> Fun;
    {ok, Val} -> Val
  end.


-else.

debug_time_start(_X) ->
  ok.

debug_time_stop(_X) ->
  ok.

debug_make_fun_map(_Tree) ->
  ok.

debug_fun_name(Fun) ->
  Fun.

-endif.

-ifdef(DOT).

to_dot(State, Tree) ->
  Funs = [{var_name(X), state__fun_cs(mk_var(Y), State)}
	  ||{X, Y} <- module_defs(Tree)],
  lists:foreach(fun to_dot/1, Funs).

to_dot({{Name, _}, Cs}) ->
  Edges = find_edges(Cs),
  Name1 = io_lib:format("/tmp/~w", [Name]),
  hipe_dot:translate_list(Edges, Name1++".dot", io_lib:format("~w", [Name])),
  Cmd = io_lib:format("dot -Tps -o ~s.ps ~s.dot", [Name1, Name1]),
  io:format("Cmd: ~s\n", [Cmd]),
  Res = os:cmd(Cmd),
  io:format("Res: ~s", [Res]),
  ok.

find_edges(Cs) ->
  find_edges([Cs], "Top", []).

find_edges([#constraint{lhs=L, op=Op, rhs=R}|Tail], Parent, Acc) ->
  find_edges(Tail, Parent, 
	     [{Parent, 
	       lists:flatten(
	       io_lib:format("~s ~w ~s", 
			     [format_type(L),Op,format_type(R)]))}|Acc]);
			     
find_edges([#constraint_list{type = Type, list=List, id=Id}|Tail], 
	   Parent, Acc) ->
  Acc1 = find_edges(List, {Id, Type}, Acc),
  find_edges(Tail, Parent, [{Parent, {Id, Type}}|Acc1]);
find_edges([], _Parent, Acc) ->
  Acc.

-else.
to_dot(_, _)->
  ok.

-endif.
