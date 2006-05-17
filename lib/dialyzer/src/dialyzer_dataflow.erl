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

%%% -*- erlang-indent-level: 2 -*-
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
	 binary_segments/1, c_letrec/2, c_values/1,
	 call_args/1, call_module/1, call_name/1, case_arg/1,
	 case_clauses/1, catch_body/1, clause_body/1, clause_guard/1,
	 clause_pats/1, concrete/1, cons_hd/1, cons_tl/1,
	 fun_arity/1, fun_body/1, fun_vars/1, get_ann/1, int_val/1,
	 is_c_fun/1, is_c_values/1, is_literal/1,
	 let_arg/1, let_body/1, let_vars/1, letrec_body/1,
	 letrec_defs/1, module_defs/1, module_vars/1,
	 primop_name/1, receive_action/1,
	 receive_clauses/1, receive_timeout/1, seq_arg/1, seq_body/1,
	 set_ann/2, subtrees/1,
	 try_arg/1, try_body/1, try_evars/1, try_handler/1,
	 try_vars/1, tuple_es/1, type/1, update_tree/2, values_es/1]).
	 

-export([annotate_module/1,
	 doit/1,
	 get_fun_types/4,
	 pp/1]).

%-define(DEBUG, true).
%-define(DEBUG_PP, true).
%-define(DEBUG_TIME, true).
%-define(DOT, true).

-ifdef(DEBUG).
-define(debug(S_, L_), io:format(S_, L_)).
-else.
-define(debug(S_, L_), ok).
-endif.


-define(TYPE_LIMIT, 4).
-define(FORWARD_LIMIT, 3).

pp(Code) ->
  AnnTree = annotate_module(Code),
  io:put_chars(cerl_prettypr:format(AnnTree, [{hook, cerl_typean:pp_hook()}])),
  io:nl().

get_fun_types(Tree, ForwardLimit, FunTypes, Plt) ->
  State = analyze_module(Tree, ForwardLimit, Plt, FunTypes),
  state__all_fun_types_unsafe(State).

doit(Code) ->
  AnnTree = annotate_module(Code),
  Defs = module_defs(AnnTree),
  [io:format("~w:: ~s\n", 
	     [cerl:var_name(Var), 
	      t_to_string(proplists:get_value(type, cerl:get_ann(Fun)))])
   || {Var, Fun} <- Defs],
  ok.

%%% ============================================================================
%%%
%%%  Annotate all top level funs.
%%%
%%% ============================================================================

annotate_module(Code) ->
  annotate_module(Code, ?FORWARD_LIMIT).

annotate_module(Code, ForwardLimit) ->
  {Tree, _} = cerl_trees:label(cerl:from_records(Code)),
  Plt = dialyzer_plt:from_dets(dialyzer_dataflow_plt, 
			       filename:join([?DIALYZER_DIR, 
					      "plt","dialyzer_init_plt"])),
  State = analyze_module(Tree, ForwardLimit, Plt),
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
	true -> set_ann(Tree, delete_ann(type, get_ann(Tree)));
	false -> set_ann(Tree, append_ann(type, Type, get_ann(Tree)))
      end;
    false ->
      Tree
  end.

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

analyze_module(Tree, ForwardLimit, Plt) ->
  analyze_module(Tree, ForwardLimit, Plt, dict:new()).

analyze_module(Tree, ForwardLimit, Plt, FunSigs) ->
  debug_pp(Tree, false),
  {Deps0, Esc, Calls} = dialyzer_dep:analyze(Tree),
  %% We want the reverse dependencies to know who to envoke
  Deps = reverse_deps(Deps0),
%  io:format("Parents: ~p\n", [dict:to_list(Parents)]),
%  io:format("Deps: ~p\n", [dict:to_list(Deps)]),
%  io:format("Esc: ~p\n", [Esc]),
  
  TopFun =  ann_c_fun([{label, top}], [], Tree),
  State = state__new(Esc, Deps, Calls, TopFun, FunSigs, ForwardLimit, Plt),
  analyze_loop(State).

reverse_deps(Deps) ->
  dict:fold(fun(Caller, CalleeSet, Acc) ->
		lists:foldl(fun(Callee, Acc1)->
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
    {{Fun, ArgTypes}, NewState} ->
      ?debug("Handling fun ~p: ~p\n", [get_label(Fun), [t_to_string(X)
						       ||X <- ArgTypes]]),
      Map = state__fun_env(Fun, NewState),
      Vars = fun_vars(Fun),
      Map1 = enter_type_lists(Vars, ArgTypes, Map),
      {State1, Map2, BodyType} = traverse(fun_body(Fun), Map1, NewState),
      State2 = state__update_fun_entry(Fun, ArgTypes, BodyType, State1),
      State3 = state__store_env_out(Fun, Map2, State2),
      analyze_loop(State3)
  end.	

traverse(Tree, Map, State) ->
  %%io:format("Handling ~s\n", [cerl_prettypr:format(Tree)]),
  %%debug_pp_map(Map),
  case type(Tree) of
    apply ->
      Args = apply_args(Tree),
      Op = apply_op(Tree),
      {State1, Map1, ArgTypes} = traverse_list(Args, Map, State),
      {State2, Map2, OpType} = traverse(Op, Map1, State1),
      case state__handle_apply(Tree, ArgTypes, State1) of
	unknown ->
	  %% This is an externally defined closure
	  OpType1 = t_inf(OpType, t_fun(length(Args), t_any())),
	  case t_is_none(OpType1) of
	    true ->
	      Map3 = enter_type(Tree, t_none(), Map2),
	      Map4 = enter_type(Op, t_none(), Map3),
	      {State2, Map4, t_none()};
	    false ->
	      OpRange = t_fun_range(OpType1),
	      Map3 = enter_type(Tree, OpRange, Map2),
	      {State2, Map3, OpRange}
	  end;
	{NewArgTypes, Return} ->
	  Map3 = enter_type(Tree, Return, Map2),
	  Map4 = enter_type_lists(Args, NewArgTypes, Map3),
	  State3 = state__forward_args(Tree, NewArgTypes, State2),
	  {State3, Map4, Return}
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
	  {State1, enter_type(Tree, t_none(), Map1), t_none()};
	false ->
	  %% XXX: Consider doing this for all combinations of MF
	  case {t_atom_vals(MType), t_atom_vals(FType)} of
	    {[MAtom], [FAtom]} ->
	      {Return, NewArgs} = do_call(MAtom, FAtom, As, State),
	      Map2 = enter_type_lists([Tree|MFAList], 
				      [Return, MType, FType|NewArgs], Map1),
	      {State1, Map2, Return};
	    {_MAtoms, _FAtoms} ->
	      Map2 = enter_type_lists([Tree, M, F], 
				      [t_any(), MType, FType], Map1),
	      {State1, Map2, t_any()}
	  end
      end;
    'case' ->
      Arg = case_arg(Tree),
      Clauses = filter_match_fail(case_clauses(Tree)),
      {State1, Map1, ArgType} = traverse(Arg, Map, State),
      {MapList, State2, Type} = handle_clauses(Clauses, Arg, ArgType, State1, 
					       t_none(), Map1, []),
      Map2 = join_maps(MapList, Map1),
      {State2, enter_type(Tree, Type, Map2), Type};      
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
      VarTypes =
	case t_is_none(ArgTypes) of
	  true -> duplicate(length(Vars), t_none());
	  false -> wrap_if_single(t_components(ArgTypes))
	end,
      Map2 = enter_type_lists(Vars, VarTypes, Map1),
      traverse(Body, Map2, State1);
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
      Vars = c_values(module_vars(Tree)),
      Letrec = c_letrec(Defs, Vars),
      {State1, Map1, _FunTypes} = traverse(Letrec, Map, State),
      State2 = state__store_env_out(Tree, Map1, State1),
      {State2, Map1, t_any()};
    primop ->
      Type =
	case atom_val(primop_name(Tree)) of
	  match_fail -> t_none();
	  raise -> t_none();
	  Other -> erlang:fault({'Unsupported primop', Other})
	end,
      Map1 = enter_type(Tree, Type, Map),
      {State, Map1, Type};
    'receive' ->
      Clauses = filter_match_fail(receive_clauses(Tree)),
      Timeout = receive_timeout(Tree),
      {MapList, State1, ReceiveType} = 
	handle_clauses(Clauses, none, t_any(), State, t_none(), Map, []),
		       
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
      {State, Map, lookup_type(Tree, Map)};
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
  BifRet = erl_bif_types:type(M, F, Arity, As),
  case erl_bif_types:arg_types(M, F, Arity) of
    any -> BifArgs = duplicate(Arity, t_any());
    List -> BifArgs = List
  end,
  {PltRet, PltArg} = state__lookup_non_local(M, F, Arity, State),
  ArgConstrs = t_inf_lists(BifArgs, PltArg),
  NewArgs = t_inf_lists(ArgConstrs, As),
  Ret = t_inf(BifRet, PltRet),
  case any_none([Ret|NewArgs]) of
    true -> {t_none(), duplicate(Arity, t_none())};
    false -> {Ret, NewArgs}
  end.
  
handle_clauses([C|Left], Arg, ArgType, State, CaseType, MapIn, Acc) ->
  {State1, ClauseMap, BodyType} = do_clause(C, Arg, ArgType, MapIn, State),
  NewCaseType = t_sup(BodyType, CaseType),
  NewAcc = [ClauseMap|Acc],
  handle_clauses(Left, Arg, ArgType, State1, NewCaseType, MapIn, NewAcc);
handle_clauses([], _Arg, _ArgType, State, CaseType, _MapIn, Acc) ->
  {lists:reverse(Acc), State, CaseType}.

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
    throw:fail -> error
  end.

bind_guard(Guard, Map, Env) ->
  ?debug("Handling guard: ~s\n", [cerl_prettypr:format(Guard)]),
  case type(Guard) of
    binary -> 
      {Map, t_binary()};
    'case' ->
      %% Asserting that this is only stemming from the strict_record_tests.
      Arg = case_arg(Guard),
      [] = values_es(Arg),
      [C1, C2] = case_clauses(Guard),
      fail = concrete(clause_body(C2)),
      bind_guard(clause_guard(C1), Map, Env);
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
	{erlang, internal_is_record, 3} ->
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
	  throw(dont_know);
	{erlang, 'not', 1} ->
	  throw(dont_know);
	{M, F, A} ->
	  {Map1, As} = bind_guard_list(Args, Map, Env),
	  BifRet = erl_bif_types:type(M, F, A, As),
	  case t_is_none(BifRet) of
	    true -> {Map, BifRet};
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
  Keys0 = lists:foldl(fun({Map, Subst}, Acc)->
			  [dict:fetch_keys(Map), dict:fetch_keys(Subst)|Acc]
		      end, [], Maps),
  Keys = ordsets:from_list(lists:flatten(Keys0)),
  join_maps(Keys, Maps, MapOut).

join_maps([Key|Left], Maps, MapOut) ->
  Type = lists:foldl(fun(M, T) -> t_sup(lookup_type(Key, M), T)end,
		     t_none(), Maps),
  join_maps(Left, Maps, enter_type(Key, Type, MapOut));
join_maps([], _Maps, MapOut) ->
  MapOut.

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
	      NewMap = dict:store(KeyLabel, Val, Map),
	      {NewMap, Subst}
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
      ValLabel = get_label(Val),
      case dict:find(ValLabel, Subst) of
	{ok, NewVal} ->
	  enter_subst(Key, NewVal, MS);
	error ->
	  ?debug("Subst: storing ~p == ~p\n", [KeyLabel, ValLabel]),
	  NewSubst = dict:store(KeyLabel, ValLabel, Subst),
	  {Map, NewSubst}
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

all_none([]) -> false;
all_none(L) -> all_none_1(L).

all_none_1([X|Xs]) ->
  case t_is_none(X) of
    true -> all_none_1(Xs);      
    false -> false
  end;
all_none_1([]) ->
  true.


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

-record(state, {deps, calls, envs, envs_out, forward_limit, fun_tab, fun_sigs,
		plt, tree_map, work}).

state__new(Esc, Deps, Calls, Tree, FunSigs, ForwardLimit, Plt) -> 
  TreeMap = build_tree_map(Tree),
  Funs = dict:fetch_keys(TreeMap),
  FunTab = init_fun_tab(Funs, dict:new(), TreeMap, Esc, FunSigs),
  Work = init_work([{get_label(Tree), duplicate(fun_arity(Tree), t_any())}]),
  #state{deps=Deps, calls=Calls, envs=dict:new(), envs_out=dict:new(), 
	 forward_limit=ForwardLimit, fun_sigs=FunSigs, fun_tab=FunTab, plt=Plt, 
	 work=Work, tree_map=TreeMap}.

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
      FunEntry = [{Args, t_none()}],
      NewDict = dict:store(Fun, FunEntry, Dict),
      init_fun_tab(Left, NewDict, TreeMap, Esc, FunSigs)
  end;
init_fun_tab([], Dict, _TreeMap, _Esc, _FunSigs) ->
  Dict.

state__forward_limit(#state{forward_limit=FL}) ->
  FL.

state__update_fun_env(Tree, Map, State = #state{envs=Envs}) ->
  NewEnvs = dict:store(get_label(Tree), Map, Envs),
  State#state{envs=NewEnvs}.

state__fun_env(Tree, #state{envs=Envs}) ->
  Fun = get_label(Tree),
  case dict:find(Fun, Envs) of
    error -> map__new();
    {ok, Map} -> Map
  end.

state__all_fun_types_unsafe(#state{fun_tab=FunTab}) ->
  FunTab1 = dict:erase(top, FunTab),
  FoldFun = fun({Args, Return}, Acc) ->
		case any_none(Args) of
		  true -> Acc;
		  false -> t_sup(t_fun(Args, Return), Acc)
		end
	    end,
  dict:map(fun(Key, _Val) -> state__fun_type(Key, FunTab1, FoldFun)end,FunTab1).


state__fun_type(Label, #state{fun_tab=FunTab}) ->
  FoldFun = fun({Args, Return}, Acc) ->
		case t_is_none(Return) orelse any_none(Args) of
		  true -> Acc;
		  false -> t_sup(t_fun(Args, Return), Acc)
		end
	    end, 
  state__fun_type(Label, FunTab, FoldFun).

state__fun_type(Label, FunTab, FoldFun) when is_integer(Label)->
  case dict:find(Label, FunTab) of
    error -> 
      %% This is a function with no arguments that have not been analyzed.
      t_fun(0, t_none());
    {ok, {widened, {Args, Return}}} ->
      t_fun(Args, Return);
    {ok, FunEntries = [{A, _}|_]} ->
      lists:foldl(FoldFun, t_fun(duplicate(length(A), t_none()), t_none()), 
		  FunEntries)
  end;
state__fun_type(Tree, State, FoldFun) ->
  state__fun_type(get_label(Tree), State, FoldFun).


state__update_fun_entry(Tree, ArgTypes, Out0, State = #state{fun_tab=FunTab}) ->
  Out = t_limit(Out0, ?TYPE_LIMIT),
  Fun = get_label(Tree),
  case dict:find(Fun, FunTab) of
    {ok, {widened, {ArgTypes, OldOut}}} ->
      case t_is_equal(OldOut, Out) of
	true -> State;
	false ->
	  NewEntry = {widened, {ArgTypes, Out}},
	  ?debug("New Entry for ~w: ~s\n", 
		    [Fun, t_to_string(t_fun(ArgTypes, Out))]),
	  NewFunTab = dict:store(Fun, NewEntry, FunTab),
	  State1 = state__add_work_from_fun(Tree, State),
	  State1#state{fun_tab=NewFunTab}
      end;
    {ok, {widened, {_OldArgTypes, _OldOut}}} ->
      %% Can happen in self-recursive functions.
      State;
    {ok, OldEntry} ->
      case orddict:find(ArgTypes, OldEntry) of
	{ok, OldOut} ->
	  case t_is_equal(OldOut, Out) of
	    true -> State;
	    false ->
	      NewEntry = orddict:store(ArgTypes, Out, OldEntry),
	  ?debug("New Entry for ~w: ~s\n", 
		    [Fun, t_to_string(t_fun(ArgTypes, Out))]),
	      NewFunTab = dict:store(Fun, NewEntry, FunTab),
	      State1 = state__add_work_from_fun(Tree, State),
	      State1#state{fun_tab=NewFunTab}
	  end;
	error ->
	  NewEntry = orddict:store(ArgTypes, Out, OldEntry),
	  ?debug("New Entry for ~w: ~s\n", 
		    [Fun, t_to_string(t_fun(ArgTypes, Out))]),
	  NewFunTab = dict:store(Fun, NewEntry, FunTab),
	  State1 = state__add_work_from_fun(Tree, State),
	  State1#state{fun_tab=NewFunTab}
      end;
    error -> 
      NewEntry = [{ArgTypes, Out}],
	  ?debug("New Entry for ~w: ~s\n", 
		    [Fun, t_to_string(t_fun(ArgTypes, Out))]),
      NewFunTab = dict:store(Fun, NewEntry, FunTab),
      State1 = state__add_work_from_fun(Tree, State),
      State1#state{fun_tab=NewFunTab}
  end.

state__add_work_from_fun(Tree, State = #state{deps=Deps}) ->
  case dict:find(get_label(Tree), Deps) of
    error -> State;
    {ok, List} -> 
      lists:foldl(fun(X, AccState)-> state__add_work(X, AccState)end,
		  State, List)
  end.

state__add_work(external, State) ->
  State;
state__add_work(Fun, State = #state{work=Work, fun_tab=FunTab}) ->
  %% Add work for all possible arguments.
  case dict:find(Fun, FunTab) of
    error -> NewWork = add_work({Fun, []}, Work);
    {ok, {widened, {Args, _}}} ->
      NewWork = add_work({Fun, Args}, Work);
    {ok, FunEntry} ->
      AllArgs = orddict:fetch_keys(FunEntry),
      NewWork = add_work_list([{Fun, Args} || Args <- AllArgs, 
					      all_none(Args) =:=false], Work)
  end,
  State#state{work=NewWork}.

%state__add_work(Tree, Args, State = #state{work=Work}) ->
%  NewWork = add_work({get_label(Tree), Args}, Work),
%  State#state{work=NewWork}.

state__get_work(State = #state{work=Work, tree_map=TreeMap}) ->
  case get_work(Work) of
    none -> none;
    {{Label, Args}, NewWork} -> 
      case all_none(Args) of
	true -> 
	  state__get_work(State#state{work=NewWork});
	false ->
	  {{dict:fetch(Label, TreeMap), Args}, State#state{work=NewWork}}
      end
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
	  FunInfo = [state__fun_apply(Fun, Args, State) || Fun <- List],
	  Out = t_sup([FunOut || {_InfArgs, FunOut} <- FunInfo]),
	  NewArgs0 = lists:foldl(fun(X, Acc) -> t_sup(t_product(X), Acc)end,
				 t_none(), 
				 [InfArgs || {InfArgs, _FunOut} <- FunInfo]),
	  case t_is_none(NewArgs0) of
	    true -> {duplicate(length(Args), t_none()), t_none()};
	    false -> {wrap_if_single(t_components(NewArgs0)), Out}
	  end
%	  case t_is_none(NewArgs0) of
%	    true -> {duplicate(length(Args), t_none()), t_none()};
%	    false -> {wrap_if_single(t_components(NewArgs0)), Out}
%	  end
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
	{ok, {widened, {OldArgs, Out}}} ->
	  case lists:all(fun({X, Y}) -> t_is_subtype(X, Y)end,
			 lists:zip(NewArgs, OldArgs)) of
	    true -> 
	      {NewArgs, Out};
	    false ->
	      %% Haven't analyzed this yet
	      {NewArgs, t_none()}
	  end;
	{ok, FunEntries} ->
	  case orddict:find(NewArgs, FunEntries) of
	    error ->
	      %% Haven't analyzed this yet
	      {NewArgs, t_none()};
	    {ok, Return} ->
	      {NewArgs, Return}
	  end
      end
  end.

state__forward_args(Apply, ArgTypes0, State) when is_record(State, state) ->
  ArgTypes = [t_limit(T, ?TYPE_LIMIT) || T <- ArgTypes0],
  Funs = dict:fetch(get_label(Apply), State#state.calls),
  ForwardLimit = state__forward_limit(State),
  {NewFunTab, NewWork} = forward_args(Funs, ArgTypes, State#state.fun_tab, 
				      State#state.work, ForwardLimit),
  State#state{work=NewWork, fun_tab=NewFunTab}.

forward_args([external|Left], ArgTypes, FunTab, Work, ForwardLimit) ->
  forward_args(Left, ArgTypes, FunTab, Work, ForwardLimit);
forward_args([Fun|Left], ArgTypes, FunTab, Work, ForwardLimit) ->
  case all_none(ArgTypes) of
    true -> {FunTab, Work};
    false ->      
      case dict:find(Fun, FunTab) of
	error ->
	  %% This must be a function without arguments that have not yet
	  %% been analyzed. Add it to the worklist.
	  ?debug("Forwarding args ~s for fun ~w\n", 
		    [t_to_string(t_product(ArgTypes)), Fun]),
	  NewWork = add_work({Fun, ArgTypes}, Work),
	  forward_args(Left, ArgTypes, FunTab, NewWork, ForwardLimit);
	{ok, {widened, {OldArgTypes, _}}} ->
	  case t_is_subtype(t_product(ArgTypes), t_product(OldArgTypes)) of
	    true -> 
	      forward_args(Left, ArgTypes, FunTab, Work, ForwardLimit);
	    false -> 
	      NewArgTypes = [t_sup(X, Y) || 
			      {X, Y} <- lists:zip(ArgTypes, OldArgTypes)],
	      NewWork0 = clean_work(Fun, Work),
	      NewWork = add_work({Fun, NewArgTypes}, NewWork0),
	      ?debug("Forwarding args ~s for fun ~w\n", 
			[t_to_string(t_product(NewArgTypes)), Fun]),
	      
	      NewFunTab = dict:store(Fun, {widened, {NewArgTypes, t_none()}}, 
				     FunTab),
	      forward_args(Left, ArgTypes, NewFunTab, NewWork, ForwardLimit)
	  end;
	{ok, FunEntries} ->
	  case orddict:is_key(ArgTypes, FunEntries) of
	    true ->
	      forward_args(Left, ArgTypes, FunTab, Work, ForwardLimit);
	    false ->
	      case length(FunEntries) > ForwardLimit of
		true ->
		  OldArgs = lists:foldl(fun({ATs, _}, Acc) -> 
					    t_sup(t_product(ATs), Acc)
					end, t_none(), FunEntries),
		  NewArgTypes0 = t_sup(t_product(ArgTypes), OldArgs),
		  NewArgTypes = wrap_if_single(t_components(NewArgTypes0)),
		  ?debug("Forwarding args ~s for fun ~w\n", 
			    [t_to_string(t_product(NewArgTypes)), Fun]),
		  NewWork0 = clean_work(Fun, Work),
		  NewWork = add_work({Fun, NewArgTypes}, NewWork0),
		  NewFunTab = 
		    dict:store(Fun, {widened, {NewArgTypes, t_none()}}, FunTab),
		  forward_args(Left, ArgTypes, NewFunTab, 
			       NewWork, ForwardLimit);
		false ->
		  ?debug("Forwarding args ~s for fun ~w\n", 
			    [t_to_string(t_product(ArgTypes)), Fun]),
		  NewWork = add_work({Fun, ArgTypes}, Work),
		  forward_args(Left, ArgTypes, FunTab, NewWork, ForwardLimit)
	      end
	  end
      end
  end;
forward_args([], _ArgTypes, FunTab, Work, _ForwardLimit) ->
  {FunTab, Work}.

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
      Work;
    false -> 
      ?debug("Adding work: ~w: ~p\n", [element(1, New), 
				      [t_to_string(X) || X <- element(2, New)]]), 
      {List, [New|Rev], sets:add_element(New, Set)}
  end.

add_work_list([H|T], Work) ->      
  add_work_list(T, add_work(H, Work));
add_work_list([], Work) ->
  Work.

clean_work(Label, {List, RevList, Set}) ->
  %% Remove all entries that relates to the function.  Note that this
  %% is really dependent on the entries in the worklist, so it the
  %% representation changes this must also be changed.
  Fun = fun({X, _}) -> X =/= Label end,
  NewList = lists:filter(Fun, List),
  NewRevList = lists:filter(Fun, RevList),
  NewSet = sets:filter(Fun, Set),

%  io:format("Cleaning work for label ~w\n", [Label]),
%  io:format("NewLabels: ~w\nNewSet ~w\n", [[L || {L, _} <- NewList++RevList],
%					   [L || {L, _} <- sets:to_list(NewSet)]]),
  {NewList, NewRevList, NewSet}.


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
