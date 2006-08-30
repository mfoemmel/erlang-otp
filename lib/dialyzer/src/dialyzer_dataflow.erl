%% -*- erlang-indent-level: 2 -*-
%%--------------------------------------------------------------------
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

%%%-------------------------------------------------------------------
%%% File    : dialyzer_dataflow.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description : 
%%%
%%% Created : 19 Apr 2005 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------
-module(dialyzer_dataflow).

-include("dialyzer.hrl").

-import(erl_types, 
	[t_any/0, t_atom/0, t_atom/1, t_binary/0, t_bool/0, t_cons/0, t_cons/2,
	 t_cons_hd/1, t_cons_tl/1, t_components/1, t_float/0,
	 t_from_range/2, t_fun/0, t_fun/2, t_fun_args/1, t_fun_range/1,
	 t_inf/2, t_inf_lists/2, t_integer/0, t_is_integer/1,
	 t_is_atom/1, t_atom_vals/1, t_is_equal/2, t_is_none/1,
	 t_is_any/1, t_is_subtype/2, t_limit/2,
	 t_number/0, t_number_vals/1, t_pid/0, t_port/0,
	 t_product/1, t_ref/0, t_to_string/1,
	 t_tuple/0, t_tuple/1, t_tuple_args/1,
	 t_tuple_subtypes/1, t_sup/1, t_sup/2,
	 t_from_term/1, t_none/0]).

-import(cerl, 
	[ann_c_fun/3, alias_pat/1, alias_var/1,
	 apply_args/1, apply_op/1, atom_val/1, bitstr_size/1,
	 bitstr_val/1, bitstr_type/1, bitstr_flags/1,
	 binary_segments/1, c_letrec/2, 
	 call_args/1, call_module/1, call_name/1, case_arg/1,
	 case_clauses/1, catch_body/1, clause_body/1, clause_guard/1,
	 clause_pats/1, concrete/1, cons_hd/1, cons_tl/1,
	 fname_arity/1, fname_id/1,
	 fun_arity/1, fun_body/1, fun_vars/1, get_ann/1, int_val/1,
	 is_c_fun/1, is_c_values/1, is_c_var/1, is_literal/1,
	 let_arg/1, let_body/1, let_vars/1, letrec_body/1,
	 letrec_defs/1, module_defs/1, 
	 primop_name/1, receive_action/1,
	 receive_clauses/1, receive_timeout/1, seq_arg/1, seq_body/1,
	 set_ann/2, subtrees/1,
	 try_arg/1, try_body/1, try_evars/1, try_handler/1,
	 try_vars/1, tuple_es/1, type/1, update_tree/2, values_es/1]).
	 

-export([annotate_module/1,
	 doit/1,
	 get_fun_types/3,
	 get_top_level_signatures/1,
	 pp/1]).

%-define(DEBUG, true).
%-define(DEBUG_NAME_MAP, true).
%-define(DEBUG_PP, true).
%-define(DEBUG_TIME, true).
%-define(DOT, true).

-ifdef(DEBUG).
-define(debug(S_, L_), io:format(S_, L_)).
-else.
-define(debug(S_, L_), ok).
-endif.

%-define(debug1(S_, L_), io:format(S_, L_)).
%-define(debug1(S_, L_), ok).

-define(TYPE_LIMIT, 4).


pp(Code) ->
  AnnTree = annotate_module(Code),
  io:put_chars(cerl_prettypr:format(AnnTree, [{hook, cerl_typean:pp_hook()}])),
  io:nl().

get_fun_types(Tree, FunTypes, Plt) ->
  debug_build_namemap(Tree),
  State = analyze_module(Tree, Plt, FunTypes),
  state__all_fun_types(State).

get_top_level_signatures(Code) ->
  get_top_level_signatures(Code, false).

get_top_level_signatures(Code, _DoWarningPass) ->
  {Tree, _} = cerl_trees:label(cerl:from_records(Code)),
  Plt = dialyzer_plt:from_file(dialyzer_dataflow_plt, 
			       filename:join([code:lib_dir(dialyzer),
					      "plt","dialyzer_init_plt"])),
  FunTypes = get_fun_types(Tree, dict:new(), Plt),
  FunTypes1 = lists:foldl(fun({V, F}, Acc) ->
			      Label = get_label(F),
			      case dict:find(Label, Acc) of
				error ->
				  Arity = fname_arity(V),
				  Type = t_fun(duplicate(Arity, t_none()), 
					       t_none()),
				  dict:store(Label, Type, Acc);
				{ok, _} -> Acc
			      end
			  end, FunTypes, module_defs(Tree)),
    
%  if DoWarningPass -> dialyzer_warnings:get_warnings(Tree, FunTypes1, Plt);
%     true -> ok
%  end,
  Sigs = [{{fname_id(V), fname_arity(V)}, dict:fetch(get_label(F), FunTypes1)} 
	  || {V, F} <- module_defs(Tree)],
  ordsets:from_list(Sigs).

doit(Module) ->
  {ok, _, Code} = compile:file(Module,[to_core,binary,strict_record_tests]),
  Sigs = get_top_level_signatures(Code, true),
  [io:format("~w/~w :: ~s\n", [F, A, t_to_string(T)])
   || {{F, A}, T} <- Sigs].

%%% ============================================================================
%%%
%%%  Annotate all top level funs.
%%%
%%% ============================================================================

annotate_module(Code) ->
  {Tree, _} = cerl_trees:label(cerl:from_records(Code)),
  debug_build_namemap(Tree),
  Plt = dialyzer_plt:from_file(dialyzer_dataflow_plt, 
			       filename:join([code:lib_dir(dialyzer),
					      "plt","dialyzer_init_plt"])),
  State = analyze_module(Tree, Plt),
  Map = state__get_env_out(Tree, State),
  annotate(Tree, State, Map).

annotate(Tree, State, Map) ->
  case subtrees(Tree) of
    [] -> set_type(Tree, State);
    List -> 
      NewSubTrees = [[annotate(Subtree, State, Map) || Subtree <- Group]
		     || Group <- List],
      NewTree = update_tree(Tree, NewSubTrees),
      set_type(NewTree, State)
  end.

set_type(Tree, State) ->
  case is_c_fun(Tree) of
    true ->
      Type = state__fun_type(Tree, State),
      case t_is_any(Type) of
	true -> set_ann(Tree, delete_ann(typesig, get_ann(Tree)));
	false -> set_ann(Tree, append_ann(typesig, Type, get_ann(Tree)))
      end;
    false ->
      Tree
  end.

append_ann(Tag, Val, [X | Xs]) ->
  if is_tuple(X), size(X) >= 1, element(1, X) =:= Tag -> 
      append_ann(Tag, Val, Xs);
     true ->
      [X | append_ann(Tag, Val, Xs)]
  end;
append_ann(Tag, Val, []) ->
  [{Tag, Val}].

delete_ann(Tag, [X | Xs]) ->
  if is_tuple(X), size(X) >= 1, element(1, X) =:= Tag -> 
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

analyze_module(Tree, Plt) ->
  analyze_module(Tree, Plt, dict:new()).

analyze_module(Tree, Plt, FunSigs) ->
  debug_pp(Tree, false),
  {Deps0, Esc, Calls} = dialyzer_dep:analyze(Tree),
  %% We want the reverse dependencies to know who to envoke
  Deps = reverse_deps(Deps0),
  ?debug("Reverse dependencies: ~p\n", [[{debug_lookup_name(X),
					     [debug_lookup_name(Y0)||Y0 <-Y]} ||
					     {X, Y} <- dict:to_list(Deps)]]),
%  io:format("Parents: ~p\n", [dict:to_list(Parents)]),
%  io:format("Deps: ~p\n", [dict:to_list(Deps)]),
%  io:format("Esc: ~p\n", [Esc]),
  
  TopFun =  ann_c_fun([{label, top}], [], Tree),
  State = state__new(Esc, Deps, Calls, TopFun, FunSigs, Plt),
  analyze_loop(State).

reverse_deps(Deps) ->
  dict:fold(fun(Caller, CalleeSet, Acc) ->
		lists:foldl(fun(Callee, Acc1) ->
				case dict:find(Callee, Acc1) of
				  {ok, Set} -> 
				    NewSet = ordsets:add_element(Caller, Set);
				  error -> 
				    NewSet = [Caller]
				end, 
				dict:store(Callee, NewSet, Acc1)
			    end, Acc, CalleeSet)
	    end, dict:new(), Deps).

analyze_loop(State) ->
  case state__get_work(State) of
    none -> State;
    {Fun, NewState} ->
      ArgTypes = state__get_args(Fun, NewState),
      case any_none(ArgTypes) of
	true -> 
	  ?debug("Not handling ~w: ~s\n", [debug_lookup_name(get_label(Fun)), 
					    t_to_string(t_product(ArgTypes))]),
	  analyze_loop(NewState);
	false -> 
	  ?debug("Handling fun ~p: ~p\n", [debug_lookup_name(get_label(Fun)), 
					    [t_to_string(X)||X <- ArgTypes]]),
	  Map = state__fun_env(Fun, NewState),
	  Vars = fun_vars(Fun),
	  Map1 = enter_type_lists(Vars, ArgTypes, Map),
	  {State1, Map2, BodyType} = traverse(fun_body(Fun), Map1, NewState),
	  ?debug("Done analyzing: ~w:~s\n", [debug_lookup_name(get_label(Fun)),
				      t_to_string(t_fun(ArgTypes, BodyType))]),
	  State2 = state__update_fun_entry(Fun, ArgTypes, BodyType, State1),
	  ?debug("done adding stuff for ~w\n", 
		 [debug_lookup_name(get_label(Fun))]),
	  State3 = state__store_env_out(Fun, Map2, State2),
	  %%debug_work(State3),
	  %%debug_fun_type(get_label(Fun), State3),
	  analyze_loop(State3)
      end
  end.

traverse(Tree, Map, State) ->
  %%io:format("Handling ~s\n", [cerl_prettypr:format(Tree)]),
  %%debug_pp_map(Map),
  case type(Tree) of
    apply ->
      Args = apply_args(Tree),
      Op = apply_op(Tree),
      {State1, Map1, ArgTypes} = traverse_list(Args, Map, State),
      case state__handle_apply(Tree, ArgTypes, State1) of
	unknown ->
	  {State2, Map2, OpType} = traverse(Op, Map1, State1),
	  %% This is an externally defined closure
	  OpType1 = t_inf(OpType, t_fun(length(Args), t_any())),
	  case t_is_none(OpType1) of
	    true ->
	      {State2, Map2, t_none()};
	    false ->
	      OpRange = t_fun_range(OpType1),
	      {State2, Map2, OpRange}
	  end;
	{NewArgTypes, Return} ->
	  State2 = state__forward_args(Tree, NewArgTypes, State1),
	  case t_is_none(Return) of
	    true -> 	      
	      {State2, Map1, t_none()};
	    false ->
	      {State2, Map1, Return}
	  end
      end;
    binary ->
      {State1, Map1, _} = traverse_list(binary_segments(Tree), Map, State),
      {State1, Map1, t_binary()};
    bitstr ->
      %% Only care about Size and Value since the other fields are
      %% constant literals. Size must be an integer - NO, it can be 'all'
      Size = bitstr_size(Tree),
      Val = bitstr_val(Tree),      
      {State1, Map1, SizeType} = traverse(Size, Map, State),
      {State2, Map2, ValType} = traverse(Val, Map1, State1),
      StrType =
	case concrete(bitstr_type(Tree)) of
	  float -> t_float();
	  binary -> t_binary();
	  integer ->
	    case t_is_integer(SizeType) of
	      true ->
		case t_number_vals(SizeType) of
		  any -> t_integer();
		  List ->
		    SizeVal = lists:max(List),
		    Flags = concrete(bitstr_flags(Tree)),
		    case lists:member(signed, Flags) of
		      true -> 
			t_from_range(-(1 bsl (SizeVal - 1)), 
				     1 bsl (SizeVal - 1) - 1);
		      false -> 
			t_from_range(0, 1 bsl SizeVal - 1)
		    end
		end;
	      false -> 
		case t_is_atom(SizeType) of
		  true ->
		    case t_atom_vals(SizeType) of
		      [all] -> t_integer();
		      _ -> t_none()
		    end;
		  false ->
		    t_none()
		end
	    end
	end,
      Type = t_inf(StrType, ValType),
      Map3 = enter_type(Val, Type, Map2),
      {State2, Map3, Type};
    call ->
      M = call_module(Tree),
      F = call_name(Tree),
      MFAList = [M, F|call_args(Tree)],
      {State1, Map1, [MType0, FType0|As]} = traverse_list(MFAList, Map, State),
      MType = t_inf(t_atom(), MType0),
      FType = t_inf(t_atom(), FType0),
      case any_none([MType, FType|As]) of
	true ->
	  {State1, Map1, t_none()};
	false ->
	  %% XXX: Consider doing this for all combinations of MF
	  case {t_atom_vals(MType), t_atom_vals(FType)} of
	    {[MAtom], [FAtom]} ->
	      {Return, NewArgs} = do_call(MAtom, FAtom, As, State),
	      Map2 = enter_type_lists(call_args(Tree), NewArgs, Map1),
	      {State1, Map2, Return};

	    {_MAtoms, _FAtoms} ->
	      {State1, Map1, t_any()}
	  end
      end;
    'case' ->
      Arg = case_arg(Tree),
      Clauses = filter_match_fail(case_clauses(Tree)),
      {State1, Map1, ArgType} = traverse(Arg, Map, State),
      case any_none(wrap_if_single(t_components(ArgType))) of
	true -> {State1, Map1, t_none()};
	false ->
	  {MapList, State2, Type} = handle_clauses(Clauses, Arg, ArgType, 
						   State1, [], Map1, []),
	  Map2 = join_maps(MapList, Map1),
	  {State2, Map2, Type}
      end;
    'catch' ->
      {State1, Map1, _} = traverse(catch_body(Tree), Map, State),
      State2 = state__store_env_out(Tree, Map1, State1),
      {State2, Map, t_any()};
    cons ->
      Hd = cons_hd(Tree),
      Tl = cons_tl(Tree),
      {State1, Map1, [HdType, TlType]} = traverse_list([Hd, Tl], Map, State),
      Type = t_cons(HdType, TlType),
      {State1, Map1, Type};
    'fun' ->
      Type = state__fun_type(Tree, State),
      State1 = state__add_work(get_label(Tree), State),
      State2 = state__update_fun_env(Tree, Map, State1),
      {State2, Map, Type};
    'let' ->
      Arg = let_arg(Tree),
      Body = let_body(Tree),
      {State1, Map1, ArgTypes} = traverse(Arg, Map, State),
      Vars = let_vars(Tree),
      case t_is_none(ArgTypes) of
	true -> {State1, Map1, t_none()};
	false -> 
	  VarTypes = wrap_if_single(t_components(ArgTypes)),
	  Map2 = enter_type_lists(Vars, VarTypes, Map1),
	  traverse(Body, Map2, State1)
      end;
    letrec ->
      Defs = letrec_defs(Tree),
      Body = letrec_body(Tree),
      %% Make sure we have the recursion variables in the scope.
      VarTypes = [{Var, state__fun_type(Fun, State)} || {Var, Fun} <- Defs],
      Map1 = enter_type_list(VarTypes, Map),
      {State1, Map2} = 
	lists:foldl(fun({Var, Fun}, {AccState, AccMap}) ->
			{NewAccState, NewAccMap0, FunType} = 
			  traverse(Fun, AccMap, AccState),
			NewAccMap = enter_type(Var, FunType, NewAccMap0),
			{NewAccState, NewAccMap}
		    end, {State, Map1}, Defs),
      traverse(Body, Map2, State1);
    literal ->
      {State, Map, literal_type(Tree)};      
    module ->
      Defs = module_defs(Tree),
      Defs1 = [{Var, Fun} || {Var, Fun} <- Defs, 
			     state__is_escaping(get_label(Fun), State)],
      Letrec = c_letrec(Defs1, cerl:c_int(42)),
      State0 = state__store_module_defs(Defs, State),
      {State1, Map1, _FunTypes} = traverse(Letrec, Map, State0),
      State2 = state__store_env_out(Tree, Map1, State1),
      {State2, Map1, t_any()};
    primop ->
      Type =
	case atom_val(primop_name(Tree)) of
	  match_fail -> t_none();
	  raise -> t_none();
	  Other -> erlang:fault({'Unsupported primop', Other})
	end,
      {State, Map, Type};      
    'receive' ->
      Clauses = filter_match_fail(receive_clauses(Tree)),
      Timeout = receive_timeout(Tree),
      {MapList, State1, ReceiveType} = 
	handle_clauses(Clauses, none, t_any(), State, [], Map, []),
		       
      Map1 = join_maps(MapList, Map),
      {State2, Map2, TimeoutType} = traverse(Timeout, Map1, State1),

      case (t_is_atom(TimeoutType) andalso 
	    (t_atom_vals(TimeoutType) =:= [infinity])) of
	true ->
	  {State2, Map2, ReceiveType};
	false ->
	  Action = receive_action(Tree),
	  {State3, Map3, ActionType} = traverse(Action, Map, State2),
	  Map4 = join_maps([Map3, Map1], Map),
	  Type = t_sup(ReceiveType, ActionType),
	  {State3, Map4, Type}
      end;
    seq ->
      Arg = seq_arg(Tree),
      Body = seq_body(Tree),
      {State1, Map1, ArgType} = traverse(Arg, Map, State),
      case t_is_none(ArgType) of
	true ->
	  {State1, Map1, t_none()};
	false ->
	  traverse(Body, Map1, State1)
      end;
    'try' ->
      Arg = try_arg(Tree),
      EVars = try_evars(Tree),
      Vars = try_vars(Tree),
      Body = try_body(Tree),
      Handler = try_handler(Tree),

      {State1, Map1, ArgType} = traverse(Arg, Map, State),      
      Map2 = mark_as_fresh(Vars, Map1),
      ArgTypes = wrap_if_single(t_components(ArgType)),
      case bind_pat_vars(Vars, ArgTypes, [], Map2) of
	error -> 
	  SuccState = State1,
	  SuccMap = map__new(),
	  SuccType = t_none();
	{SuccMap1, VarTypes} ->
	  %% Try to bind the argument. Will only succeed if 
	  %% it is a simple structured term.
	  case bind_pat_vars([Arg], [t_product(VarTypes)], [], SuccMap1) of
	    error -> SuccMap2 = SuccMap1;
	    {SuccMap2, _} -> ok
	  end,
	  {SuccState, SuccMap, SuccType} = traverse(Body, SuccMap2, State1)
      end,      
      ExcMap0 = mark_as_fresh(EVars, Map),
      {State2, ExcMap1, _EVarsType} = traverse_list(EVars, ExcMap0, SuccState),
      {State3, ExcMap2, HandlerType} = traverse(Handler, ExcMap1, State2),
      TryType = t_sup(SuccType, HandlerType),
      {State3, join_maps([ExcMap2, SuccMap], Map1), TryType};
    tuple ->
      Elements = tuple_es(Tree),
      {State1, Map1, EsType} = traverse_list(Elements, Map, State),
      Type = t_tuple(EsType),
      {State1, Map1, Type};
    values ->
      Elements = values_es(Tree),
      {State1, Map1, EsType} = traverse_list(Elements, Map, State),      
      Type  = t_product(EsType),
      {State1, Map1, Type};
    var ->
      case state__is_module_defined(Tree, State) of
	true ->
	  Fun = state__get_fun_from_defs(Tree, State),
	  State1 = state__add_work(Fun, State),
	  {State1, Map, state__fun_type(Fun, State)};
	false ->
	  {State, Map, lookup_type(Tree, Map)}
      end;
    Other ->
      erlang:fault({'Unsupported type', Other})
  end.

traverse_list(Trees, Map, State) ->
  traverse_list(Trees, Map, State, []).

traverse_list([Tree|Tail], Map, State, Acc) ->
  {State1, Map1, Type} = traverse(Tree, Map, State),
  traverse_list(Tail, Map1, State1, [Type|Acc]);
traverse_list([], Map, State, Acc) ->
  {State, Map, lists:reverse(Acc)}.

  
%%________________________________________
%%
%% Special instructions
%%

do_call(M, F, As, State) ->
  Arity = length(As),
  {Ret, ArgCs} = 
    case erl_bif_types:is_known(M, F, Arity) of
      true ->
	BifRet = erl_bif_types:type(M, F, Arity, As),
	BifArgs = 
	  case erl_bif_types:arg_types(M, F, Arity) of
	    any -> duplicate(Arity, t_any());
	    List -> List
	  end,
	{BifRet, BifArgs};
      false ->
	state__lookup_non_local(M, F, Arity, State)
    end,
  NewArgs = t_inf_lists(ArgCs, As),
  case any_none([Ret|NewArgs]) of
    true -> {t_none(), duplicate(Arity, t_none())};
    false -> {Ret, NewArgs}
  end.
  
handle_clauses([C|Left], Arg, ArgType, State, CaseTypes, MapIn, Acc) ->
  {State1, ClauseMap, BodyType} = do_clause(C, Arg, ArgType, MapIn, State),
  NewCaseTypes = [BodyType|CaseTypes],
  NewAcc = [ClauseMap|Acc],
  handle_clauses(Left, Arg, ArgType, State1, NewCaseTypes, MapIn, NewAcc);
handle_clauses([], _Arg, _ArgType, State, CaseTypes, _MapIn, Acc) ->
  {lists:reverse(Acc), State, t_sup(CaseTypes)}.

do_clause(C, Arg, ArgType0, Map, State) ->
  Pats = clause_pats(C),
  Guard = clause_guard(C),
  Body = clause_body(C),
  Map0 = mark_as_fresh(Pats, Map),
  ArgTypes = t_components(ArgType0),  
  
  if Arg =:= none -> Map1 = Map0;
     true ->         Map1 = bind_subst(Arg, Pats, Map0)
  end,

  case bind_pat_vars(Pats, ArgTypes, [], Map1) of
    error -> ?debug("Failed binding pattern: ~s\nto ~s\n", 
		    [cerl_prettypr:format(C), t_to_string(ArgType0)]),
	     {State, Map, t_none()};
    {Map2, PatTypes} ->
      case Arg =:= none of
	true -> Map3 = Map2;
	false ->
	  %% Try to bind the argument. Will only succeed if 
	  %% it is a simple structured term.
	  case bind_pat_vars([Arg], [t_product(PatTypes)], [], Map2) of
	    error -> Map3 = Map2;
	    {Map3, _} -> ok
	  end
      end,
      case bind_guard(Guard, Map3) of
	error -> ?debug("Failed guard: ~s\n", 
		       [cerl_prettypr:format(C, [{hook, cerl_typean:pp_hook()}])]),
		 {State, Map, t_none()};
	Map4 ->
	  {State1, Map5, Type} = traverse(Body, Map4, State),
	  State2 = state__store_env_out(C, Map5, State1),
	  {State2, Map5, Type}
      end
  end.

wrap_if_single(X) when is_list(X) -> X;
wrap_if_single(X) -> [X].

bind_subst(Arg, Pats, Map) ->
  case type(Arg) of
    values -> 
      bind_subst_list(values_es(Arg), Pats, Map);
    var ->
      [Pat] = Pats,
      enter_subst(Arg, Pat, Map);
    _ ->
      Map
  end.

bind_subst_list([Arg|ArgLeft], [Pat|PatLeft], Map) ->
  case {type(Arg), type(Pat)} of
    {var, var} ->         NewMap = enter_subst(Arg, Pat, Map);
    {var, alias} ->       NewMap = enter_subst(Arg, alias_pat(Pat), Map);
    {literal, literal} -> NewMap = Map;
    {T, T} ->             NewMap = bind_subst_list(lists:flatten(subtrees(Arg)), 
						   lists:flatten(subtrees(Pat)), 
						   Map);
    _ ->                  NewMap = Map
  end,
  bind_subst_list(ArgLeft, PatLeft, NewMap);
bind_subst_list([], [], Map) ->
  Map.

bind_pat_vars([Pat|PatLeft], [Type|TypeLeft], Acc, Map) ->
  ?debug("binding ~w\n", [type(Pat)]),
  case type(Pat) of
    alias ->
      AliasPat = alias_pat(Pat),
      Var = alias_var(Pat),
      Map1 = enter_subst(Var, AliasPat, Map),
      case bind_pat_vars([AliasPat], [Type], [], Map1) of
	error -> Res = error;
	{Map2, [PatType]} -> 	  	  
	  Res = {enter_type(Var, PatType, Map2), PatType}
      end;
    binary ->
      case t_is_none(t_inf(t_binary(), Type)) of
	true -> Res = error;
	false -> Res = {bind_bin_segs(binary_segments(Pat), Map), t_binary()}
      end;
    cons ->
      Cons = t_inf(Type, t_cons()),
      case t_is_none(Cons) of
	true -> Res = error;
	false ->
	  case bind_pat_vars([cons_hd(Pat), cons_tl(Pat)],
			     [t_cons_hd(Cons), t_cons_tl(Cons)], [], Map) of
	    error -> Res = error;
	    {Map1, [HdType, TlType]} -> Res = {Map1, t_cons(HdType, TlType)}
	  end
      end;
    literal ->
      Literal = literal_type(Pat),
      case t_is_none(t_inf(Literal, Type)) of
	true -> Res = error;
	false -> Res = {Map, Literal}
      end;
    tuple ->
      Es = tuple_es(Pat),
      Tuple = t_inf(t_tuple(length(Es)), Type),
      case t_is_none(Tuple) of
	true -> Res = error;
	false ->
	  SubTuples = t_tuple_subtypes(Tuple),
	  Results = [bind_pat_vars(Es, t_tuple_args(SubTuple), [], Map)||
		      SubTuple <- SubTuples],
	  case lists:all(fun(X) -> X =:= error end, Results) of
	    true -> Res = error;
	    false ->
	      Map1 = join_maps([M || {M, _} <- Results], Map),
	      TupleType = t_sup([t_tuple(EsTypes) || {_, EsTypes} <- Results]),
	      Res = {Map1, TupleType}
	  end
      end;
    values ->
      Es = values_es(Pat),
      case bind_pat_vars(Es, t_components(Type), [], Map) of
	error -> Res = error;
	{Map1, EsTypes} -> Res = {Map1, t_product(EsTypes)}
      end;
    var ->
      %% Must do inf when binding args to pats. Vars in pats are fresh.
      VarType = t_inf(lookup_type(Pat, Map), Type),
      case t_is_none(VarType) of
	true -> Res = error;
	false ->
	  Map1 = enter_type(Pat, VarType, Map),
	  Res = {Map1, Type}
      end;
    _Other ->
      %% Catch all is needed when binding args to pats
      ?debug("Failed match for ~p\n", [_Other]),
      Res = error
  end,
  case Res of
    error -> error;
    {NewMap, TypeOut} -> bind_pat_vars(PatLeft, TypeLeft, [TypeOut|Acc], NewMap)
  end;
bind_pat_vars([], [], Acc, Map) ->
  {Map, lists:reverse(Acc)};
bind_pat_vars(Pats, Type, Acc, Map) ->
  case t_is_any(Type) of
    true -> bind_pat_vars(Pats, duplicate(length(Pats), t_any()), Acc, Map);
    false ->
      case t_is_none(Type) of
	true -> error;
	false -> erlang:fault({'Error binding pats', Pats, Type})
      end
  end.
	  
bind_bin_segs([Bitstr|Left], Map) ->
  %% Only care about Size and Value since the other fields are
  %% constant literals. Size must be an integer - NO, it can be 'all'
  Val = bitstr_val(Bitstr),
  Size = bitstr_size(Bitstr),
  SizeType =  
    case type(Size) of
      literal ->
	%% Safety check
	case concrete(Size) of
	  all -> t_from_term(all);
	  N when is_integer(N) -> t_from_term(N)
	end;
      var ->
	t_inf(t_integer(), lookup_type(bitstr_size(Bitstr), Map))
    end,
  ValType = case type(Val) of
	      literal -> t_from_term(concrete(Val));
	      var -> t_any()
	    end,
  PatType =
    case concrete(bitstr_type(Bitstr)) of
      float -> t_float();
      binary -> t_binary();
      integer ->
	case t_is_integer(SizeType) of
	  true ->
	    case t_number_vals(SizeType) of
	      any -> t_integer();
	      List ->
		SizeVal = lists:max(List),
		Flags = concrete(bitstr_flags(Bitstr)),
		case lists:member(signed, Flags) of
		  true -> 
		    t_from_range(-(1 bsl (SizeVal - 1)), 
				 1 bsl (SizeVal - 1) - 1);
		  false -> 
		    t_from_range(0, 1 bsl SizeVal - 1)
		end
	    end;
	  false -> 
	    case t_is_atom(SizeType) of
	      true ->
		case t_atom_vals(SizeType) of
		  [all] -> t_integer();
		  _ -> t_none()
		end;
	      false ->
		t_none()
	    end
	end
    end,  
  Type = t_inf(ValType, PatType),
  bind_bin_segs(Left, enter_type(Val, Type, enter_type(Size, SizeType, Map)));
bind_bin_segs([], Map) ->
  Map.
  
  

%%________________________________________
%%
%% Guards
%%

bind_guard(Guard, Map) ->
  try bind_guard(Guard, Map, dict:new()) of
    {Map1, Type} ->
      case t_is_none(t_inf(t_atom(true), Type)) of
	true -> error;
	false -> Map1
      end
  catch
    throw:dont_know -> Map;
    throw:fail -> error;
    throw:fatal_fail -> error
  end.

bind_guard(Guard, Map, Env) ->
  ?debug("Handling guard: ~s\n", [cerl_prettypr:format(Guard)]),
  case type(Guard) of
    binary -> 
      {Map, t_binary()};
    'case' ->
      %% This is so far only possible in strict record tests and
      %% orelse-guards. This might have to be extended if general case
      %% statements are allowed here.
      Arg = case_arg(Guard),
      [] = values_es(Arg),
      [C1, C2] = case_clauses(Guard),
      B1 = clause_body(C1),
      B2 = clause_body(C2),
      G1 = clause_guard(C1),
      G2 = clause_guard(C2),
      Erlang = cerl:c_atom(erlang),
      And = cerl:c_atom('and'),
      Or = cerl:c_atom('or'),
      Call1 = cerl:c_call(Erlang, And, [G1, B1]),
      Call2 = cerl:c_call(Erlang, And, [G2, B2]),
      case is_literal(B2) andalso (concrete(B2) =:= fail) of
	true -> %% Special case for strict record tests
	  bind_guard(Call1, Map, Env);
	false ->
	  bind_guard(cerl:c_call(Erlang, Or, [Call1, Call2]), Map, Env)
      end;
    cons ->
      {Map, t_cons()};
    literal ->
      {Map, literal_type(Guard)};
    'try' ->
      Arg = try_arg(Guard),
      [Var] = try_vars(Guard),
      %%?debug("Storing: ~w\n", [Var]),
      NewEnv = dict:store(get_label(Var), Arg, Env),
      bind_guard(try_body(Guard), Map, NewEnv);
    tuple ->
      {Map1, Es} = bind_guard_list(tuple_es(Guard), Map, Env),
      {Map1, t_tuple(Es)};
    'let' ->
      Arg = let_arg(Guard),
      [Var] = let_vars(Guard),
      %%?debug("Storing: ~w\n", [Var]),
      NewEnv = dict:store(get_label(Var), Arg, Env),
      bind_guard(let_body(Guard), Map, NewEnv);
    var ->
      %?debug("Looking for: ~w...", [Guard]),
      case dict:find(get_label(Guard), Env) of
	error -> 
	  %?debug("Did not find it\n", []),
	  {Map, lookup_type(Guard, Map)};
	{ok, Tree} -> 
	  %?debug("Found it\n", []),
	  {Map1, Type} = bind_guard(Tree, Map, Env),
	  {enter_type(Guard, Type, Map1), Type}
      end;
    call ->
      Args = call_args(Guard),      
      M = atom_val(call_module(Guard)),
      F = atom_val(call_name(Guard)),
      A = length(Args),      
      case {M, F, A} of
	{erlang, is_atom, 1} ->
	  [Arg] = Args,
	  {Map1, ArgType} = bind_guard(Arg, Map, Env),
	  Type = t_inf(t_atom(), ArgType),
	  case t_is_none(Type) of
	    true -> throw(fail);
	    false -> {enter_type(Arg, Type, Map1), t_atom(true)}
	  end;
	{erlang, is_boolean, 1} ->
	  [Arg] = Args,
	  {Map1, ArgType} = bind_guard(Arg, Map, Env),
	  Type = t_inf(t_bool(), ArgType),
	  case t_is_none(Type) of
	    true -> throw(fail);
	    false -> {enter_type(Arg, Type, Map1), t_atom(true)}
	  end;
	{erlang, is_binary, 1} ->
	  [Arg] = Args,
	  {Map1, ArgType} = bind_guard(Arg, Map, Env),
	  Type = t_inf(t_binary(), ArgType),
	  case t_is_none(Type) of
	    true -> throw(fail);
	    false -> {enter_type(Arg, Type, Map1), t_atom(true)}
	  end;
	{erlang, is_float, 1} ->
	  [Arg] = Args,
	  {Map1, ArgType} = bind_guard(Arg, Map, Env),
	  Type = t_inf(t_float(), ArgType),
	  case t_is_none(Type) of
	    true -> throw(fail);
	    false -> {enter_type(Arg, Type, Map1), t_atom(true)}
	  end;
	{erlang, is_function, 1} ->
	  [Arg] = Args,
	  {Map1, ArgType} = bind_guard(Arg, Map, Env),
	  Type = t_inf(t_fun(), ArgType),
	  case t_is_none(Type) of
	    true -> throw(fail);
	    false -> {enter_type(Arg, Type, Map1), t_atom(true)}
	  end;
	{erlang, is_function, 2} ->
	  {Map1, [FunType0, ArityType0]} = bind_guard_list(Args, Map, Env),
	  ArityType = t_inf(ArityType0, t_integer()),
	  case t_is_none(ArityType) of
	    true -> throw(fail);
	    false ->
	      case t_number_vals(ArityType) of
		any -> FunTypeConstr = t_fun();
		Vals -> 
		  FunTypeConstr = t_sup([t_fun(duplicate(X, t_any()), t_any())
					 || X <- Vals])
	      end,
	      FunType = t_inf(FunType0, FunTypeConstr),
	      case t_is_none(FunType) of
		true -> throw(fail);
		false -> {enter_type_lists(Args, [FunType, ArityType], Map1), 
			  t_atom(true)}
	      end
	  end;
	{erlang, is_integer, 1} ->
	  [Arg] = Args,
	  {Map1, ArgType} = bind_guard(Arg, Map, Env),
	  Type = t_inf(t_integer(), ArgType),
	  case t_is_none(Type) of
	    true -> throw(fail);
	    false -> {enter_type(Arg, Type, Map1), t_atom(true)}
	  end;
	{erlang, is_list, 1} ->
	  [Arg] = Args,
	  {Map1, ArgType} = bind_guard(Arg, Map, Env),
	  Type = t_inf(t_cons(), ArgType),
	  case t_is_none(Type) of
	    true -> throw(fail);
	    false -> {enter_type(Arg, Type, Map1), t_atom(true)}
	  end;
	{erlang, is_number, 1} ->
	  [Arg] = Args,
	  {Map1, ArgType} = bind_guard(Arg, Map, Env),
	  Type = t_inf(t_number(), ArgType),
	  case t_is_none(Type) of
	    true -> throw(fail);
	    false -> {enter_type(Arg, Type, Map1), t_atom(true)}
	  end;
	{erlang, is_pid, 1} ->
	  [Arg] = Args,
	  {Map1, ArgType} = bind_guard(Arg, Map, Env),
	  Type = t_inf(t_pid(), ArgType),
	  case t_is_none(Type) of
	    true -> throw(fail);
	    false -> {enter_type(Arg, Type, Map1), t_atom(true)}
	  end;
	{erlang, is_port, 1} ->
	  [Arg] = Args,
	  {Map1, ArgType} = bind_guard(Arg, Map, Env),
	  Type = t_inf(t_port(), ArgType),
	  case t_is_none(Type) of
	    true -> throw(fail);
	    false -> {enter_type(Arg, Type, Map1), t_atom(true)}
	  end;
	MFA when (MFA =:= {erlang, internal_is_record, 3}) or 
		 (MFA =:= {erlang, is_record, 3}) ->
	  [Rec, Tag, Arity] = Args,
	  {Map1, RecType} = bind_guard(Rec, Map, Env),
	  TupleType = t_tuple([t_atom(atom_val(Tag))
			       |duplicate(int_val(Arity)-1, t_any())]),
	  Type = t_inf(TupleType, RecType),
	  case t_is_none(Type) of
	    true -> throw(fail);
	    false -> {enter_type(Rec, Type, Map1), t_atom(true)}
	  end;
	{erlang, is_reference, 1} ->
	  [Arg] = Args,
	  {Map1, ArgType} = bind_guard(Arg, Map, Env),
	  Type = t_inf(t_ref(), ArgType),
	  case t_is_none(Type) of
	    true -> throw(fail);
	    false -> {enter_type(Arg, Type, Map1), t_atom(true)}
	  end;
	{erlang, is_tuple, 1} ->
	  [Arg] = Args,
	  {Map1, ArgType} = bind_guard(Arg, Map, Env),
	  Type = t_inf(t_tuple(), ArgType),
	  case t_is_none(Type) of
	    true -> throw(fail);
	    false -> {enter_type(Arg, Type, Map1), t_atom(true)}
	  end;
	{erlang, '=:=', 2} ->
	  [Arg1, Arg2] = Args,
	  {Map1, Type1} = bind_guard(Arg1, Map, Env),
	  {Map2, Type2} = bind_guard(Arg2, Map1, Env),
	  ?debug("Types are:~s and ~s\n", [t_to_string(Type1), 
					  t_to_string(Type2)]),
	  Inf = t_inf(Type1, Type2),
	  case t_is_none(Inf) of
	    true -> {Map, t_none()};
	    false -> 
	      case {type(Arg1), type(Arg2)} of
		{literal, literal} -> {Map2, t_atom(true)};
		{var, var} ->
		  Map3 = enter_subst(Arg1, Arg2, Map2),
		  Map4 = enter_type(Arg2, Inf, Map3),
		  {Map4, t_atom(true)};
		{var, _} ->
		  Map3 = enter_type(Arg1, Inf, Map2),
		  {Map3, t_atom(true)};
		{_, var} ->
		  Map3 = enter_type(Arg2, Inf, Map2),
		  {Map3, t_atom(true)};
		{_, _} ->
		  {Map2, t_atom(true)}
	      end
	  end;
%%%	{erlang, '==', 2} ->
	{erlang, 'and', 2} ->
	  [Arg1, Arg2] = Args,
	  True = t_atom(true),
	  {Map1, Bool1} = bind_guard(Arg1, Map, Env),
	  case t_is_none(t_inf(Bool1, True)) of
	      true ->
	      {Map, t_none()};
	    false ->
	      {Map2, Bool2} = bind_guard(Arg2, Map1, Env),
	      case t_is_none(t_inf(Bool2, True)) of
		true ->
		  {Map, t_none()};
		false ->
		  {Map2, True}
	      end
	  end;
	{erlang, 'or', 2} ->
	  [Arg1, Arg2] = Args,
	  True = t_atom(true),
	  False = t_atom(false),
	  {Map1, Bool1} = 
	    try
	      bind_guard(Arg1, Map, Env)
	    catch
	      throw:{fail, _, _} -> {Map, False}
	    end,
	  {Map2, Bool2} = 
	    try
	      bind_guard(Arg2, Map, Env)
	    catch
	      throw:{fail, _, _} -> {Map, False}
	    end,
	  case t_is_none(t_inf(Bool1, True)) of
	    true -> 
	      case t_is_none(t_inf(Bool2, True)) of
		true -> {Map, t_none()};
		false -> {Map2, Bool2}
	      end;
	    false ->
	      case t_is_none(t_inf(Bool2, True)) of
		true -> {Map1, Bool1};
		false -> {join_maps([Map1, Map2], Map), True}
	      end
	  end;
	{erlang, 'not', 1} ->
	  throw(dont_know);
	{M, F, A} ->
	  {Map1, As} = bind_guard_list(Args, Map, Env),
	  BifRet = erl_bif_types:type(M, F, A, As),
	  case t_is_none(BifRet) of
	    true -> throw(fatal_fail);
	    false ->
	      case erl_bif_types:arg_types(M, F, A) of
		any -> BifArgs = duplicate(A, t_any());
		List -> BifArgs = List
	      end,
	      Map2 = enter_type_lists(Args, t_inf_lists(BifArgs, As), Map1),
	      {Map2, BifRet}
	  end
      end
  end.

bind_guard_list(Guards, Map, Env) ->
  bind_guard_list(Guards, Map, Env, []).

bind_guard_list([G|Left], Map, Env, Acc) ->
  {Map1, T} = bind_guard(G, Map, Env),
  bind_guard_list(Left, Map1, Env, [T|Acc]);
bind_guard_list([], Map, _Env, Acc) ->
  {Map, lists:reverse(Acc)}.

%%% ============================================================================
%%%
%%%  Maps and types.
%%%
%%% ============================================================================

map__new() ->
  {dict:new(), dict:new()}.

join_maps(Maps, MapOut) ->
  %%Time = ?debug_time_start(join_maps),
%  Keys0 = lists:foldl(fun({Map, Subst}, Acc)->
%			  [dict:fetch_keys(Map), dict:fetch_keys(Subst)|Acc]
%		      end, [], Maps),
%  Keys = ordsets:from_list(lists:flatten(Keys0)),
  
  {Map, Subst} = MapOut,
  Keys = ordsets:from_list(dict:fetch_keys(Map) ++ dict:fetch_keys(Subst)),
  join_maps(Keys, Maps, MapOut).

join_maps([Key|Left], Maps, MapOut) ->
  Type = join_maps_one_key(Maps, Key, t_none()),
  case t_is_equal(lookup_type(Key, MapOut), Type) of
    true ->  join_maps(Left, Maps, MapOut);
    false -> join_maps(Left, Maps, enter_type(Key, Type, MapOut))
  end;
join_maps([], _Maps, MapOut) ->
  MapOut.

join_maps_one_key([Map|Left], Key, AccType) ->
  case t_is_any(AccType) of
    true ->
      %% We can stop here
      AccType;
    false ->
      join_maps_one_key(Left, Key, t_sup(lookup_type(Key, Map), AccType))
  end;
join_maps_one_key([], _Key, AccType) ->
  AccType.

enter_type_lists([Key|KeyTail], [Val|ValTail], Map) ->
  Map1 = enter_type(Key, Val, Map),
  enter_type_lists(KeyTail, ValTail, Map1);
enter_type_lists([], [], Map) ->
  Map.

enter_type_list([{Key, Val}|Left], Map) ->
  Map1 = enter_type(Key, Val, Map),
  enter_type_list(Left, Map1);
enter_type_list([], Map) ->
  Map.

enter_type(Key, Val, MS = {Map, Subst}) ->  
  case is_literal(Key) of
    true -> MS;
    false ->
      case is_c_values(Key) of
	true ->
	  Keys = values_es(Key),
	  case t_is_any(Val) orelse t_is_none(Val) of
	    true ->
	      enter_type_lists(Keys, duplicate(length(Keys), Val), MS);
	    false ->
	      enter_type_lists(values_es(Key), t_components(Val), MS)
	  end;
	false ->
	  KeyLabel = get_label(Key),
	  case dict:find(KeyLabel, Subst) of
	    {ok, NewKey} ->
	      ?debug("Binding ~p to ~p\n", [KeyLabel, NewKey]),
	      enter_type(NewKey, Val, MS);
	    error ->
	      ?debug("Entering ~p :: ~s\n", [KeyLabel, t_to_string(Val)]),
	      case dict:find(KeyLabel, Map) of
		{ok, Val} -> MS;
		{ok, _OldVal} -> {dict:store(KeyLabel, Val, Map), Subst};
		error -> {dict:store(KeyLabel, Val, Map), Subst}
	      end
	  end
      end
  end.

enter_subst(Key, Val, MS = {Map, Subst}) ->
  KeyLabel = get_label(Key),
  case is_literal(Val) of
    true -> 
      NewMap = dict:store(KeyLabel, literal_type(Val), Map),
      {NewMap, Subst};
    false ->
      case is_c_var(Val) of
	false -> MS;
	true ->
	  ValLabel = get_label(Val),
	  case dict:find(ValLabel, Subst) of
	    {ok, NewVal} ->
	      enter_subst(Key, NewVal, MS);
	    error ->
	      if KeyLabel =:= ValLabel -> MS;
		 true ->
		  ?debug("Subst: storing ~p = ~p\n", [KeyLabel, ValLabel]),
		  NewSubst = dict:store(KeyLabel, ValLabel, Subst),
		  {Map, NewSubst}
	      end
	  end
      end
  end.

lookup_type(Key, {Map, Subst}) -> 
  lookup(Key, Map, Subst, t_none()).

%lookup_type_list(List, Map) ->
%  [lookup_type(X, Map)||X<-List].


lookup(Key, Map, Subst, AnyNone) ->
  case is_literal(Key) of
    true -> literal_type(Key);
    false -> 
      Label = get_label(Key),
      case dict:find(Label, Subst) of
	{ok, NewKey} -> lookup(NewKey, Map, Subst, AnyNone);
	error ->
	  case dict:find(Label, Map) of
	    {ok, Val} -> Val;
	    error -> AnyNone
	  end
      end
  end.

literal_type(Lit) ->
  t_from_term(concrete(Lit)).

mark_as_fresh(Trees, Map) ->
  Fun = fun(T, AccMap) ->
	    case is_literal(T) of
	      true -> enter_type(T, literal_type(T), AccMap);
	      false -> enter_type(T, t_any(), AccMap)
	    end
	end,
  mark_as_fresh(Trees, Fun, Map).

mark_as_fresh([Tree|Left], Fun, Map) ->
  Map1 = cerl_trees:fold(Fun, Map, Tree),
  mark_as_fresh(Left, Map1);
mark_as_fresh([], _Fun, Map) ->
  Map.

%debug_pp_map(Map = {Map0, Subst}) ->
%  Keys = ordsets:from_list(dict:fetch_keys(Map0) ++ dict:fetch_keys(Subst)),
%  io:format("Map:\n", []),
%  [io:format("\t~p :: ~s\n", [K, t_to_string(lookup_type(K, Map))])
%   ||K<-Keys],
%  ok.
  

%%% ============================================================================
%%%
%%%  Utilities
%%%
%%% ============================================================================

get_label(L) when is_integer(L) ->
  L;
get_label(T) ->
  case get_ann(T) of
    [{label, L} | _] -> L;
    _ -> erlang:fault({missing_label, T})
  end.

any_none([X|Xs]) ->
  case t_is_none(X) of
    true ->
      true;
    false ->
      any_none(Xs)
  end;
any_none([]) -> false.


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


%%% ============================================================================
%%%
%%%  The State.
%%%
%%% ============================================================================

-record(state, {deps, calls, envs, envs_out, esc, fun_tab, fun_sigs, 
		module_defs,
		plt, tree_map, work}).

state__new(Esc, Deps, Calls, Tree, FunSigs, Plt) -> 
  TreeMap = build_tree_map(Tree),
  Funs = dict:fetch_keys(TreeMap),
  FunTab = init_fun_tab(Funs, dict:new(), TreeMap, Esc, FunSigs),
  Work = init_work([get_label(Tree)]),
  #state{deps=Deps, calls=Calls, envs=dict:new(), envs_out=dict:new(), esc=Esc,
	 fun_sigs=FunSigs, fun_tab=FunTab, module_defs=[], plt=Plt, 
	 work=Work, tree_map=TreeMap}.

state__is_escaping(Fun, #state{esc=Esc}) ->
  ordsets:is_element(Fun, Esc).

state__is_module_defined(Tree, #state{module_defs=MD}) ->
  orddict:is_key(get_label(Tree), MD).
  
state__get_fun_from_defs(Var, #state{module_defs=MD}) ->
  Label = get_label(Var),
  orddict:fetch(Label, MD).

state__store_module_defs(Defs, State) ->
  State#state{module_defs=orddict:from_list([{get_label(Var), get_label(Fun)}
					      || {Var, Fun} <- Defs])}.

%state__update_module_def_types(State = #state{envs=Envs, module_defs=MD}) ->
%  Types = [{Var, state__fun_type(Fun, State)} || {Var, Fun} <- MD],
%  NewEnvs = dict:map(fun(_, Map) ->
%			 lists:foldl(fun({Var, Type}, AccMap) ->
%					 enter_type(Var, Type, AccMap)
%				     end, Map, Types)
%		     end, Envs),
%  State#state{envs=NewEnvs}.

state__get_args(Tree, #state{fun_tab=FunTab}) ->
  Fun = get_label(Tree),
  case dict:find(Fun, FunTab) of
    error -> [];
    {ok, {ArgTypes, _}} -> ArgTypes
  end.

build_tree_map(Tree) ->
  Fun =
    fun(T, Dict) ->
	case type(T) of
	  'fun' ->
	    dict:store(get_label(T), T, Dict);
	  _ ->
	    Dict
	end
    end,
  cerl_trees:fold(Fun, dict:new(), Tree).

init_fun_tab([Fun|Left], Dict, TreeMap, Esc, FunSigs) ->
  case fun_arity(dict:fetch(Fun, TreeMap)) of
    0 ->
      %% Ensure that this function will be analyzed once if it is
      %% called, by not entering anything.
      init_fun_tab(Left, Dict, TreeMap, Esc, FunSigs);
    Arity ->
      Args = 
	case ordsets:is_element(Fun, Esc) of
	  true -> 
	    case dict:find(Fun, FunSigs) of
	      error -> duplicate(Arity, t_any());
	      {ok, Sig} -> 
		case t_is_none(Sig) of
		  true -> duplicate(Arity, t_none());
		  false -> t_fun_args(Sig)
		end
	    end;
	  false -> duplicate(Arity, t_none())
	end,
      FunEntry = {Args, t_none()},
      NewDict = dict:store(Fun, FunEntry, Dict),
      init_fun_tab(Left, NewDict, TreeMap, Esc, FunSigs)
  end;
init_fun_tab([], Dict, _TreeMap, _Esc, _FunSigs) ->
  Dict.

state__update_fun_env(Tree, Map, State = #state{envs=Envs}) ->
  NewEnvs = dict:store(get_label(Tree), Map, Envs),
  State#state{envs=NewEnvs}.

state__fun_env(Tree, #state{envs=Envs}) ->
  Fun = get_label(Tree),
  case dict:find(Fun, Envs) of
    error -> map__new();
    {ok, Map} -> Map
  end.

state__all_fun_types(#state{fun_tab=FunTab}) ->
  Tab1 = dict:filter(fun(X, _) -> top =/= X end, FunTab),
  dict:map(fun(_Fun, {Args, Ret}) -> t_fun(Args, Ret)end, Tab1).
		      

state__fun_type(Fun, #state{fun_tab=FunTab}) ->
  Label = 
    if is_integer(Fun) -> Fun;
       true -> get_label(Fun)
    end,
  case dict:find(Label, FunTab) of
    error -> 
      %% This is a function with no arguments that have not been analyzed.
      t_fun(0, t_none());
    {ok, {A, R}} ->
      t_fun(A, R)
  end.

state__update_fun_entry(Tree, ArgTypes, Out0, State = #state{fun_tab=FunTab}) ->
  Out = t_limit(Out0, ?TYPE_LIMIT),
  Fun = get_label(Tree),
  case dict:find(Fun, FunTab) of
    {ok, {ArgTypes, OldOut}} ->
      case t_is_equal(OldOut, Out) of
	true -> 
	  ?debug("Fixpoint for ~w: ~s\n", 
		  [debug_lookup_name(Fun), 
		   t_to_string(t_fun(ArgTypes, Out))]),
	  State;
	false ->
	  NewEntry = {ArgTypes, Out},
	  ?debug("New Entry for ~w: ~s\n", 
		 [debug_lookup_name(Fun), 
		     t_to_string(t_fun(ArgTypes, Out))]),
	  NewFunTab = dict:store(Fun, NewEntry, FunTab),
	  State1 = State#state{fun_tab=NewFunTab},
	  state__add_work_from_fun(Tree, State1)
      end;
    {ok, {NewArgTypes, _OldOut}} ->
      %% Can only happen in self-recursive functions. Only update the out type.
      NewEntry = {NewArgTypes, Out},
      ?debug("New Entry for ~w: ~s\n", 
	     [debug_lookup_name(Fun), 
	      t_to_string(t_fun(NewArgTypes, Out))]),
      NewFunTab = dict:store(Fun, NewEntry, FunTab),
      State1 = State#state{fun_tab=NewFunTab},
      state__add_work_from_fun(Tree, State1);
    error -> 
      NewEntry = {ArgTypes, Out},
      ?debug("New Entry for ~w: ~s\n", 
	      [debug_lookup_name(Fun), 
	       t_to_string(t_fun(ArgTypes, Out))]),
      NewFunTab = dict:store(Fun, NewEntry, FunTab),
      State1 = State#state{fun_tab=NewFunTab},
      state__add_work_from_fun(Tree, State1)
  end.

state__add_work_from_fun(Tree, State = #state{deps=Deps}) ->
  Label = get_label(Tree),
  case dict:find(Label, Deps) of
    error -> State;
    {ok, List} -> 
      ?debug("~w: Will try to add:~w\n", 
	     [debug_lookup_name(get_label(Tree)),
	      [debug_lookup_name(X) || X <- List]]),
      lists:foldl(fun(X, AccState)->
		      state__add_work(X, AccState)end,
		  State, List)
  end.

state__add_work(external, State) ->
  State;
state__add_work(top, State) ->
  %% Instead of adding the top function we update all function
  %% environments to reflect the current top-level bindings.
%%  state__update_module_def_types(State);
  State;
state__add_work(Fun, State = #state{work=Work}) ->
  NewWork = add_work(Fun, Work),
  State#state{work=NewWork}.

state__get_work(State = #state{work=Work, tree_map=TreeMap}) ->
  case get_work(Work) of
    none -> none;
    {Fun, NewWork} ->
      {dict:fetch(Fun, TreeMap), State#state{work=NewWork}}
  end.

state__handle_apply(Tree, Args, State = #state{calls=Calls}) ->
  Apply = get_label(Tree),
  case dict:find(Apply, Calls) of
    error ->
      unknown;
    {ok, List} ->
      case lists:member(external, List) of
	true -> 
	  {Args, t_any()};
	false ->
	  {NewArgs, Out} =
	    case [state__fun_apply(Fun, Args, State) || Fun <- List] of
	      [{As, OutType}] -> {t_product(As), OutType};
	      FunInfoList ->
		{ArgList, FunOut} = lists:unzip(FunInfoList),
		{t_sup([t_product(A) || A <- ArgList]),
		 t_sup(FunOut)}
	    end,
	  case t_is_none(NewArgs) of
	    true -> {duplicate(length(Args), t_none()), t_none()};
	    false -> {wrap_if_single(t_components(NewArgs)), Out}
	  end
      end
  end.

state__fun_apply(Fun, Args, #state{fun_sigs=FunSigs, fun_tab=FunTab}) ->
  NewArgs =
    case dict:find(Fun, FunSigs) of
      error -> Args;
      {ok, Sig} -> 
	case t_is_none(Sig) of
	  true -> duplicate(length(Args), t_none());
	  false -> t_inf_lists(t_fun_args(Sig), Args)
	end
    end,
  case any_none(NewArgs) of
    true -> 
      {duplicate(length(Args), t_none()), t_none()};
    false ->
      case dict:find(Fun, FunTab) of
	error ->
	  %% Fun without args that have not been analyzed yet
	  {NewArgs, t_none()};
	{ok, {_OldArgs, Out}} -> 
	  %% Keep the old out value. If it changes this function will
	  %% be envoked again.
	  {NewArgs, Out}
      end
  end.

state__forward_args(Apply, ArgTypes0, State = #state{}) ->
  ArgTypes = [t_limit(T, ?TYPE_LIMIT) || T <- ArgTypes0],
  case any_none(ArgTypes) of
    true -> State;
    false ->      
      Funs = dict:fetch(get_label(Apply), State#state.calls),
      forward_args(Funs, ArgTypes, State)
  end.

forward_args([external|Left], ArgTypes, State) ->
  forward_args(Left, ArgTypes, State);
forward_args([Fun|Left], ArgTypes, State = #state{work=Work, fun_tab=FunTab}) ->
  case dict:find(Fun, FunTab) of
    error ->
      %% This must be a function without arguments that have not yet
      %% been analyzed. Add it to the worklist.
      %% For sanity,
      [] = ArgTypes,
      ?debug("~w: forwarding args ~s\n", 
	     [debug_lookup_name(Fun), t_to_string(t_product(ArgTypes))]),
      NewWork = add_work(Fun, Work),
      forward_args(Left, ArgTypes, State#state{work=NewWork});
    {ok, {OldArgTypes, OldOut}} ->
      case t_is_subtype(t_product(ArgTypes), t_product(OldArgTypes)) of
	true -> 
	  forward_args(Left, ArgTypes, State);
	false -> 
	  NewArgTypes = [t_sup(X, Y) || 
			  {X, Y} <- lists:zip(ArgTypes, OldArgTypes)],
	  NewWork = add_work(Fun, Work),
	  ?debug("~w: forwarding args ~s\n", 
		 [debug_lookup_name(Fun), 
		  t_to_string(t_product(NewArgTypes))]),
	  NewFunTab = dict:store(Fun, {NewArgTypes, OldOut}, FunTab),
	  forward_args(Left, ArgTypes, State#state{work=NewWork, 
						   fun_tab=NewFunTab})
      end
  end;
forward_args([], _ArgTypes, State) ->
  State.

state__store_env_out(Fun, Map, State = #state{envs_out=Envs}) ->  
  NewEnvs = dict:store(get_label(Fun), Map, Envs),
  State#state{envs_out=NewEnvs}.
  
state__get_env_out(Fun, #state{envs_out=Envs}) ->
  case dict:find(get_label(Fun), Envs) of
    error -> map__new();
    {ok, Map} -> Map
  end.

state__lookup_non_local(M, F, A, #state{plt=Plt}) ->
  Res = 
    case dialyzer_plt:lookup(Plt, M, F, A) of
      none ->
	{t_any(), duplicate(A, t_any())};
      {value, Ret}->
	Ret
    end,
  ?debug("Looking up ~p :: ~s\n", [{M, F, A}, 
				  t_to_string(t_fun(element(2, Res), 
						    element(1, Res)))]),
  Res.

%%% ===========================================================================
%%%
%%%  Worklist
%%%
%%% ===========================================================================

init_work(List) ->
  {List, [], sets:from_list(List)}.

get_work({[], [], _Set}) ->
  none;
get_work({[H|T], Rev, Set}) ->
  {H, {T, Rev, sets:del_element(H, Set)}};
get_work({[], Rev, Set}) ->
  get_work({lists:reverse(Rev), [], Set}).

add_work(New, Work = {List, Rev, Set}) ->
  case sets:is_element(New, Set) of
    true -> 
      ?debug("Did not add work: ~w\n", [debug_lookup_name(New)]),
      Work;
    false -> 
      ?debug("Adding work: ~w\n", [debug_lookup_name(New)]),
      {List, [New|Rev], sets:add_element(New, Set)}
  end.

%%% ============================================================================
%%%
%%%  Utilities.
%%%
%%% ============================================================================

duplicate(0, _Element) ->
  [];
duplicate(N, Element) ->
  [Element|duplicate(N-1, Element)].

-ifdef(DEBUG_PP).
debug_pp(Tree, true) -> 
  io:put_chars(cerl_prettypr:format(Tree, [{hook, cerl_typean:pp_hook()}])),
  io:nl(),
  ok;
debug_pp(Tree, false) ->
  io:put_chars(cerl_prettypr:format(Tree)),
  io:nl(),
  ok.
-else.
debug_pp(_Tree, _UseHook) ->
  ok.
-endif.

-ifdef(DEBUG_NAME_MAP).
debug_build_namemap(Tree) ->
  Fun = fun(T, AccMap1) ->
	    Defs = 
	      case cerl:type(T) of
		module -> cerl:module_defs(T);
		letrec -> cerl:letrec_defs(T);
		_      -> []
	      end,
	    lists:foldl(fun({Var, Fun}, AccMap2) ->
			    Id = cerl:fname_id(Var),
			    Arity = cerl:fname_arity(Var),
			    Label = get_label(Fun),
			    dict:store(Label, {Id, Arity}, AccMap2)
			end, AccMap1, Defs)
	end,
  Map = cerl_trees:fold(Fun, dict:new(), Tree),
  put(dialyzer_dataflow_namemap, Map),
  ok.

debug_lookup_name(Label) ->
  Map = get(dialyzer_dataflow_namemap),  
  case dict:find(Label, Map) of
    error -> Label;
    {ok, Name} -> Name
  end.
-else.
debug_build_namemap(_Tree) ->
  ok.
-endif.

-ifdef(DEBUG).
debug_fun_type(Label, #state{fun_tab=FunTab}) ->
  ?debug("Entries for fun: ~w\n", [debug_lookup_name(Label)]),
  case dict:find(Label, FunTab) of
    error -> 
      %% This is a function with no arguments that have not been analyzed.
      ok;
    {ok, {_Args, _Return}} ->
      ?debug("\t~s\n", [t_to_string(t_fun(_Args, _Return))])
  end.
-endif.
