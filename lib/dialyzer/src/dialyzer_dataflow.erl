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
	 t_from_range/2, t_fun/0, t_fun/2, t_fun_range/1,
	 t_inf/2, t_inf_lists/2, t_integer/0, t_is_integer/1, t_is_nil/1,
	 t_is_atom/1, t_atom_vals/1, t_is_equal/2, t_is_none/1,
	 t_is_any/1, t_is_subtype/2, t_limit/2,
	 t_number/0, t_number_vals/1, t_pid/0, t_port/0,
	 t_pos_improper_list/0,
	 t_product/1, t_ref/0, t_to_string/2,
	 t_tuple/0, t_tuple/1, t_tuple_args/1,
	 t_tuple_subtypes/1, t_sup/1, t_sup/2, t_subtract/2,
	 t_from_term/1, t_none/0]).


-export([annotate_module/2,
	 doit/1,
	 doit/2,
	 get_fun_types/5,
	 get_top_level_signatures/2,
	 %get_warnings/1,
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

-define(TYPE_LIMIT, 3).


pp(Code) ->
  Plt = get_def_plt(),
  AnnTree = annotate_module(Code, Plt),
  dialyzer_plt:delete(Plt),
  io:put_chars(cerl_prettypr:format(AnnTree, [{hook, cerl_typean:pp_hook()}])),
  io:nl().

get_fun_types(Tree, FunTypes, Callgraph, Plt, Records) ->
  debug_build_namemap(Tree),
  State = analyze_module(Tree, Plt, Callgraph, FunTypes, Records),
  state__all_fun_types(State).

get_top_level_signatures(Code, Records) ->
  {Tree, _} = cerl_trees:label(cerl:from_records(Code)),
  Callgraph0 = dialyzer_callgraph:new(),
  Callgraph1 = dialyzer_callgraph:scan_core_tree(Tree, Callgraph0),
  {Callgraph2, _} = dialyzer_callgraph:remove_external(Callgraph1),
  Callgraph = dialyzer_callgraph:finalize(Callgraph2),
  to_dot(Callgraph),
  Plt = get_def_plt(),
  FunTypes = get_fun_types(Tree, Plt, Callgraph, Plt, Records),
  FunTypes1 = lists:foldl(fun({V, F}, Acc) ->
			      Label = get_label(F),
			      case dict:find(Label, Acc) of
				error ->
				  Arity = cerl:fname_arity(V),
				  Type = t_fun(duplicate(Arity, t_none()), 
					       t_none()),
				  dict:store(Label, Type, Acc);
				{ok, _} -> Acc
			      end
			  end, FunTypes, cerl:module_defs(Tree)),
  dialyzer_callgraph:delete(Callgraph),
  Sigs = [{{cerl:fname_id(V), cerl:fname_arity(V)}, 
	   dict:fetch(get_label(F), FunTypes1)} 
	  || {V, F} <- cerl:module_defs(Tree)],
  ordsets:from_list(Sigs).

get_def_plt() ->
  try 
    dialyzer_plt:from_file(dialyzer_typesig_plt, 
			   filename:join([code:lib_dir(dialyzer),
					  "plt","dialyzer_init_plt"]))
  catch
    error:no_such_file -> ets:new(dialyzer_typesig_plt, [])
  end.

doit(Module) ->
  doit(Module, []).

doit(Module, Opts) ->
  AbstrCode = dialyzer_utils:get_abstract_code_from_src(Module, Opts),
  Code = dialyzer_utils:get_core_from_abstract_code(AbstrCode),
  {ok, Records} = dialyzer_utils:get_record_info(AbstrCode),
  Sigs = get_top_level_signatures(Code, Records),
  [io:format("~w/~w :: ~s\n", [F, A, t_to_string(T, Records)])
   || {{F, A}, T} <- Sigs].

%%% ============================================================================
%%%
%%%  Annotate all top level funs.
%%%
%%% ============================================================================

annotate_module(Code, Plt) ->
  {Tree, _} = cerl_trees:label(cerl:from_records(Code)),
  debug_build_namemap(Tree),
  Callgraph0 = dialyzer_callgraph:new(),
  Callgraph1 = dialyzer_callgraph:scan_core_tree(Tree, Callgraph0),
  {Callgraph2, _} = dialyzer_callgraph:remove_external(Callgraph1),
  Callgraph = dialyzer_callgraph:finalize(Callgraph2),
  State = analyze_module(Tree, Plt, Callgraph),
  Res = annotate(Tree, State),
  dialyzer_callgraph:delete(Callgraph),
  Res.

annotate(Tree, State) ->
  case cerl:subtrees(Tree) of
    [] -> set_type(Tree, State);
    List -> 
      NewSubTrees = [[annotate(Subtree, State) || Subtree <- Group]
		     || Group <- List],
      NewTree = cerl:update_tree(Tree, NewSubTrees),
      set_type(NewTree, State)
  end.

set_type(Tree, State) ->
  case cerl:type(Tree) of
    'fun' ->
      Type = state__fun_type(Tree, State),
      case t_is_any(Type) of
	true -> 
	  cerl:set_ann(Tree, delete_ann(typesig, cerl:get_ann(Tree)));
	false -> 
	  cerl:set_ann(Tree, append_ann(typesig, Type, cerl:get_ann(Tree)))
      end;
    apply ->
      case state__find_apply_return(Tree, State) of
	unknown -> Tree;
	ReturnType ->
	  case t_is_any(ReturnType) of
	    true -> 
	      cerl:set_ann(Tree, delete_ann(type, cerl:get_ann(Tree)));
	    false -> 
	      cerl:set_ann(Tree, append_ann(type, ReturnType, 
					    cerl:get_ann(Tree)))
	  end
      end;
    _ ->
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

analyze_module(Tree, Plt, Callgraph) ->
  analyze_module(Tree, Plt, Callgraph, dialyzer_plt:new(fun_sigs), dict:new()).

analyze_module(Tree, Plt, Callgraph, FunSigs, Records) ->
  debug_pp(Tree, false),
  TopFun =  cerl:ann_c_fun([{label, top}], [], Tree),
  State = state__new(Callgraph, TopFun, FunSigs, Plt, Records),
  analyze_loop(State).

analyze_loop(State) ->
  case state__get_work(State) of
    none -> State;
    {Fun, NewState} ->
      ArgTypes = state__get_args(Fun, NewState),
      case any_none(ArgTypes) of
	true -> 
	  ?debug("Not handling1 ~w: ~s\n", [debug_lookup_name(get_label(Fun)), 
					   t_to_string(t_product(ArgTypes))]),
	  analyze_loop(NewState);
	false -> 
	  case state__fun_env(Fun, NewState) of
	    none -> 
	      ?debug("Not handling2 ~w: ~s\n", 
		     [debug_lookup_name(get_label(Fun)), 
		      t_to_string(t_product(ArgTypes))]),
	      analyze_loop(NewState);
	    {Map, DefVars} ->
	      ?debug("Handling fun ~p: ~p\n", 
		      [debug_lookup_name(get_label(Fun)), 
		       [t_to_string(X)||X <- ArgTypes]]),
	      Vars = cerl:fun_vars(Fun),
	      Map1 = enter_type_lists(Vars, ArgTypes, Map),
	      Body = cerl:fun_body(Fun),
	      DefVars1 = add_def_vars(Vars, DefVars),
	      {State1, _Map2, BodyType} = 
		traverse(Body, Map1, DefVars1, NewState),
	      ?debug("Done analyzing: ~w:~s\n", 
		     [debug_lookup_name(get_label(Fun)),
		      t_to_string(t_fun(ArgTypes, BodyType))]),
	      State2 = state__update_fun_entry(Fun, ArgTypes, BodyType, State1),
	      ?debug("done adding stuff for ~w\n", 
		     [debug_lookup_name(get_label(Fun))]),
	      analyze_loop(State2)
	  end
      end
  end.

traverse(Tree, Map, DefVars, State) ->
  %%io:format("Handling ~p\n", [cerl:type(Tree)]),
  %%debug_pp_map(Map),
  case cerl:type(Tree) of
    apply ->
      Args = cerl:apply_args(Tree),
      Op = cerl:apply_op(Tree),
      {State1, Map1, ArgTypes} = traverse_list(Args, Map, DefVars, State),
      case state__handle_apply(Tree, ArgTypes, State1) of
	unknown ->
	  {State2, Map2, OpType} = traverse(Op, Map1, DefVars, State1),
	  %% This is an externally defined closure
	  OpType1 = t_inf(OpType, t_fun(length(Args), t_any())),
	  case t_is_none(OpType1) of
	    true ->
	      {State2, Map2, t_none()};
	    false ->
	      OpRange = t_fun_range(OpType1),
	      {State2, enter_type(Op, OpType1, Map2), OpRange}
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
      Segs = cerl:binary_segments(Tree),
      {State1, Map1, _} = traverse_list(Segs, Map, DefVars, State),
      {State1, Map1, t_binary()};
    bitstr ->
      %% Only care about Size and Value since the other fields are
      %% constant literals. Size must be an integer - NO, it can be 'all'
      Size = cerl:bitstr_size(Tree),
      Val = cerl:bitstr_val(Tree),      
      {State1, Map1, SizeType} = traverse(Size, Map, DefVars, State),
      {State2, Map2, ValType} = traverse(Val, Map1, DefVars, State1),
      StrType =
	case cerl:concrete(cerl:bitstr_type(Tree)) of
	  float -> t_float();
	  binary -> t_binary();
	  integer ->
	    case t_is_integer(SizeType) of
	      true ->
		case t_number_vals(SizeType) of
		  any -> t_integer();
		  List ->
		    SizeVal = lists:max(List),
		    Flags = cerl:concrete(cerl:bitstr_flags(Tree)),
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
      M = cerl:call_module(Tree),
      F = cerl:call_name(Tree),
      MFAList = [M, F|cerl:call_args(Tree)],
      {State1, Map1, [MType0, FType0|As]} = 
	traverse_list(MFAList, Map, DefVars, State),
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
	      Map2 = enter_type_lists(cerl:call_args(Tree), NewArgs, Map1),
	      {State1, Map2, Return};

	    {_MAtoms, _FAtoms} ->
	      {State1, Map1, t_any()}
	  end
      end;
    'case' ->
      Arg = cerl:case_arg(Tree),
      Clauses = filter_match_fail(cerl:case_clauses(Tree)),
      {State1, Map1, ArgType} = traverse(Arg, Map, DefVars, State),
      case any_none(wrap_if_single(t_components(ArgType))) of
	true -> {State1, Map1, t_none()};
	false ->
	  {MapList, State2, Type} = 
	    handle_clauses(Clauses, Arg, ArgType, State1, 
			   [], Map1, DefVars, []),
	  Map2 = join_maps(MapList, Map1),
	  {State2, Map2, Type}
      end;
    'catch' ->
      {State1, _Map1, _} = traverse(cerl:catch_body(Tree), Map, DefVars, State),
      {State1, Map, t_any()};
    cons ->
      Hd = cerl:cons_hd(Tree),
      Tl = cerl:cons_tl(Tree),
      {State1, Map1, HdType} = traverse(Hd, Map, DefVars, State),
      {State2, Map2, TlType} = traverse(Tl, Map1, DefVars, State1),
      Type = t_cons(HdType, TlType),
      {State2, Map2, Type};
    'fun' ->
      Type = state__fun_type(Tree, State),
      State1 = state__add_work(get_label(Tree), State),
      State2 = state__update_fun_env(Tree, {Map, DefVars}, State1),
      {State2, Map, Type};
    'let' ->
      Arg = cerl:let_arg(Tree),
      Body = cerl:let_body(Tree),
      {State1, Map1, ArgTypes} = traverse(Arg, Map, DefVars, State),
      Vars = cerl:let_vars(Tree),
      case t_is_none(ArgTypes) of
	true -> {State1, Map1, t_none()};
	false -> 
	  VarTypes = wrap_if_single(t_components(ArgTypes)),
	  Map2 = enter_type_lists(Vars, VarTypes, Map1),
	  traverse(Body, Map2, add_def_vars(Vars, DefVars), State1)
      end;
    letrec ->
      Defs = cerl:letrec_defs(Tree),
      Body = cerl:letrec_body(Tree),
      %% By not including the variables in scope we can assure that we
      %% will get the current function type when using the variables.
      FoldFun = fun({Var, Fun}, {AccState, AccMap}) ->
		    {NewAccState, NewAccMap0, FunType} = 
		      traverse(Fun, AccMap, DefVars, AccState),
		    NewAccMap = enter_type(Var, FunType, NewAccMap0),
		    {NewAccState, NewAccMap}
		end,
      {State1, Map1} = lists:foldl(FoldFun, {State, Map}, Defs),
      traverse(Body, Map1, DefVars, State1);
    literal ->
      %% This is needed for finding records
      case cerl:unfold_literal(Tree) of
	Tree -> {State, Map, literal_type(Tree)};
	NewTree -> traverse(NewTree, Map, DefVars, State)
      end;
    module ->
      %% By not including the variables in scope we can assure that we
      %% will get the current function type when using the variables.
      Defs = cerl:module_defs(Tree),
      PartFun = fun({_Var, Fun}) -> 
		    state__is_escaping(get_label(Fun), State)
		end,
      {Defs1, Defs2} = 	lists:partition(PartFun, Defs),
      Letrec = cerl:c_letrec(Defs1, cerl:c_int(42)),
      {State1, Map1, _FunTypes} = traverse(Letrec, Map, DefVars, State),
      %% Also add environments for the other top-level functions.
      VarTypes = [{Var, state__fun_type(Fun, State1)} || {Var, Fun} <- Defs],
      EnvMap = enter_type_list(VarTypes, Map),
      FoldFun = fun({_Var, Fun}, AccState) ->
		    state__update_fun_env(Fun, {EnvMap, DefVars}, AccState)
		end,
      State2 = lists:foldl(FoldFun, State1, Defs2),
      {State2, Map1, t_any()};
    primop ->
      Type =
	case cerl:atom_val(cerl:primop_name(Tree)) of
	  match_fail -> t_none();
	  raise -> t_none();
	  Other -> erlang:fault({'Unsupported primop', Other})
	end,
      {State, Map, Type};      
    'receive' ->
      Clauses = filter_match_fail(cerl:receive_clauses(Tree)),
      Timeout = cerl:receive_timeout(Tree),
      {MapList, State1, ReceiveType} = 
	handle_clauses(Clauses, none, t_any(), State, [], Map, DefVars, []),
      Map1 = join_maps(MapList, Map),
      {State2, Map2, TimeoutType} = traverse(Timeout, Map1, DefVars, State1),
      case (t_is_atom(TimeoutType) andalso 
	    (t_atom_vals(TimeoutType) =:= [infinity])) of
	true ->
	  {State2, Map2, ReceiveType};
	false ->
	  Action = cerl:receive_action(Tree),
	  {State3, Map3, ActionType} = traverse(Action, Map, DefVars, State2),
	  Map4 = join_maps([Map3, Map1], Map),
	  Type = t_sup(ReceiveType, ActionType),
	  {State3, Map4, Type}
      end;
    seq ->
      Arg = cerl:seq_arg(Tree),
      Body = cerl:seq_body(Tree),
      {State1, Map1, ArgType} = traverse(Arg, Map, DefVars, State),
      case t_is_none(ArgType) of
	true ->
	  {State1, Map1, t_none()};
	false ->
	  traverse(Body, Map1, DefVars, State1)
      end;
    'try' ->
      Arg = cerl:try_arg(Tree),
      EVars = cerl:try_evars(Tree),
      Vars = cerl:try_vars(Tree),
      Body = cerl:try_body(Tree),
      Handler = cerl:try_handler(Tree),

      {State1, Map1, ArgType} = traverse(Arg, Map, DefVars, State),      
      Map2 = mark_as_fresh(Vars, Map1),
      ArgTypes = wrap_if_single(t_components(ArgType)),
      {SuccState, SuccMap, SuccType} =       
	case bind_pat_vars(Vars, ArgTypes, [], Map2, State1) of
	  error -> 
	    {State1, map__new(), t_none()};
	  {SuccMap1, VarTypes} ->
	    %% Try to bind the argument. Will only succeed if 
	    %% it is a simple structured term.
	    case bind_pat_vars([Arg], [t_product(VarTypes)], [], 
			       SuccMap1, State1) of
	      error -> SuccMap2 = SuccMap1;
	      {SuccMap2, _} -> ok
	    end,
	    DefVars1 = add_def_vars(Vars, DefVars),
	    traverse(Body, SuccMap2, DefVars1, State1)
	end,
      ExcMap1 = mark_as_fresh(EVars, Map),
      DefVars2 = add_def_vars(EVars, DefVars),
      {State2, ExcMap2, HandlerType} = 
	traverse(Handler, ExcMap1, DefVars2, SuccState),
      TryType = t_sup(SuccType, HandlerType),
      {State2, join_maps([ExcMap2, SuccMap], Map1), TryType};
    tuple ->
      Elements = cerl:tuple_es(Tree),
      {State1, Map1, EsType} = traverse_list(Elements, Map, DefVars, State),
      %% Let's find out if this is a record construction.
      case Elements of
	[Tag|Left] ->
	  case cerl:is_c_atom(Tag) of
	    true ->
	      TagVal = cerl:atom_val(Tag),
	      case state__lookup_record(TagVal, length(Left), State1) of
		error -> {State1, Map1, t_tuple(EsType)};
		{ok, Prototype} -> 
		  TupleType = t_inf(Prototype, t_tuple(EsType)),
		  case t_is_none(TupleType) of
		    true ->
		      {State1, Map1, t_none()};
		    false ->
		      case bind_pat_vars(Elements, t_tuple_args(TupleType), [], 
					 Map1, State1) of
			error ->
			  {State1, Map1, t_none()};
			{Map2, ETypes} ->
			  {State1, Map2, t_tuple(ETypes)}
		      end
		  end
	      end;
	    false ->
	      {State1, Map1, t_tuple(EsType)}
	  end;
	[] ->
	  {State1, Map1, t_tuple([])}
      end;
    values ->
      Elements = cerl:values_es(Tree),
      {State1, Map1, EsType} = traverse_list(Elements, Map, DefVars, State),
      Type  = t_product(EsType),
      {State1, Map1, Type};
    var ->
      case is_def_var(Tree, DefVars) of
	true ->
	  {State, Map, lookup_type(Tree, Map)};
	false ->
	  ?debug("Looking up unknown variable: ~p\n", [Tree]),
	  case state__lookup_type_for_rec_var(Tree, State) of
	    error -> erlang:fault({'Non-defined variable', Tree});
	    {ok, Type} -> {State, Map, Type}
	  end
      end;
    Other ->
      erlang:fault({'Unsupported type', Other})
  end.

traverse_list(Trees, Map, DefVars, State) ->
  traverse_list(Trees, Map, DefVars, State, []).

traverse_list([Tree|Tail], Map, DefVars, State, Acc) ->
  {State1, Map1, Type} = traverse(Tree, Map, DefVars, State),
  traverse_list(Tail, Map1, DefVars, State1, [Type|Acc]);
traverse_list([], Map, _DefVars, State, Acc) ->
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
  
handle_clauses([C|Left], Arg, ArgType, State, CaseTypes, MapIn, DefVars, Acc) ->
  {State1, ClauseMap, BodyType, NewArgType} = 
    do_clause(C, Arg, ArgType, MapIn, DefVars, State),
  NewCaseTypes = [BodyType|CaseTypes],
  NewAcc = [ClauseMap|Acc],
  handle_clauses(Left, Arg, NewArgType, State1, 
		 NewCaseTypes, MapIn, DefVars, NewAcc);
handle_clauses([], _Arg, _ArgType, State, CaseTypes, _MapIn, _DefVars, Acc) ->
  {lists:reverse(Acc), State, t_sup(CaseTypes)}.

do_clause(C, Arg, ArgType0, Map, DefVars, State) ->
  Pats = cerl:clause_pats(C),
  Guard = cerl:clause_guard(C),
  Body = cerl:clause_body(C),
  Map0 = mark_as_fresh(Pats, Map),
  ArgTypes = t_components(ArgType0),  
  
  if Arg =:= none -> Map1 = Map0;
     true ->         Map1 = bind_subst(Arg, Pats, Map0)
  end,

  case bind_pat_vars(Pats, ArgTypes, [], Map1, State) of
    error -> ?debug("Failed binding pattern: ~s\nto ~s\n", 
		    [cerl_prettypr:format(C), t_to_string(ArgType0)]),
	     {State, Map, t_none(), ArgType0};
    {Map2, PatTypes} ->
      case Arg =:= none of
	true -> Map3 = Map2;
	false ->
	  %% Try to bind the argument. Will only succeed if 
	  %% it is a simple structured term.
	  case bind_pat_vars([Arg], [t_product(PatTypes)], [], Map2, State) of
	    error -> Map3 = Map2;
	    {Map3, _} -> ok
	  end
      end,
      NewArgType = 
	case Arg =:= none of
	  true -> ArgType0;
	  false ->
	    GenType = dialyzer_typesig:get_safe_underapprox(Pats, Guard),
	    t_subtract(t_product(wrap_if_single(ArgType0)), GenType)
	end,
      case bind_guard(Guard, Map3, State) of
	error -> 
	  ?debug("Failed guard: ~s\n", 
		 [cerl_prettypr:format(C, [{hook, cerl_typean:pp_hook()}])]),
	  {State, Map, t_none(), NewArgType};
	Map4 ->
	  FoldFun = fun(ST, Acc) ->
			case cerl:is_c_var(ST) of
			  true -> [ST|Acc];
			  false -> Acc
			end
		    end,
	  PatVars = [cerl_trees:fold(FoldFun, [], Pat) || Pat <- Pats],
	  DefVars1 = add_def_vars(lists:flatten(PatVars), DefVars),
	  {RetState, RetMap, BodyType} = traverse(Body, Map4, DefVars1, State),
	  {RetState, RetMap, BodyType, NewArgType}
      end
  end.

wrap_if_single(X) when is_list(X) -> X;
wrap_if_single(X) -> [X].

bind_subst(Arg, Pats, Map) ->
  case cerl:type(Arg) of
    values -> 
      bind_subst_list(cerl:values_es(Arg), Pats, Map);
    var ->
      [Pat] = Pats,
      enter_subst(Arg, Pat, Map);
    _ ->
      Map
  end.

bind_subst_list([Arg|ArgLeft], [Pat|PatLeft], Map) ->
  NewMap =
    case {cerl:type(Arg), cerl:type(Pat)} of
      {var, var} ->         enter_subst(Arg, Pat, Map);
      {var, alias} ->       enter_subst(Arg, cerl:alias_pat(Pat), Map);
      {literal, literal} -> Map;
      {T, T} ->             bind_subst_list(lists:flatten(cerl:subtrees(Arg)),
					    lists:flatten(cerl:subtrees(Pat)),
					    Map);
      _ ->                  Map
    end,
  bind_subst_list(ArgLeft, PatLeft, NewMap);
bind_subst_list([], [], Map) ->
  Map.

bind_pat_vars([Pat|PatLeft], [Type|TypeLeft], Acc, Map, State) ->
  ?debug("binding ~w\n", [cerl:type(Pat)]),
  Res =
    case cerl:type(Pat) of
      alias ->
	AliasPat = cerl:alias_pat(Pat),
	Var = cerl:alias_var(Pat),
	Map1 = enter_subst(Var, AliasPat, Map),
	case bind_pat_vars([AliasPat], [Type], [], Map1, State) of
	  error -> error;
	  {Map2, [PatType]} -> {enter_type(Var, PatType, Map2), PatType}
	end;
      binary ->
	case t_is_none(t_inf(t_binary(), Type)) of
	  true -> error;
	  false -> {bind_bin_segs(cerl:binary_segments(Pat), Map), t_binary()}
	end;
      cons ->
	Cons = t_inf(Type, t_cons()),
	case t_is_none(Cons) of
	  true -> error;
	  false ->
	    case bind_pat_vars([cerl:cons_hd(Pat), cerl:cons_tl(Pat)],
			       [t_cons_hd(Cons), t_cons_tl(Cons)], 
			       [], Map, State) of
	      error -> error;
	      {Map1, [HdType, TlType]} -> {Map1, t_cons(HdType, TlType)}
	    end
	end;
      literal ->
	Literal = literal_type(Pat),
	case t_is_none(t_inf(Literal, Type)) of
	  true -> error;
	  false -> {Map, Literal}
	end;
      tuple ->	
	Es = cerl:tuple_es(Pat),
	Prototype = 
	  case Es of
	    [] -> t_tuple([]);
	    [Tag|Left] ->
	      case cerl:is_c_atom(Tag) of
		true ->
		  TagAtom = cerl:atom_val(Tag),
		  case state__lookup_record(TagAtom, length(Left), State) of
		    error -> t_tuple(length(Es));
		    {ok, Record} -> Record
		  end;
		false -> t_tuple(length(Es))
	      end
	  end,
	Tuple = t_inf(Prototype, Type),
	case t_is_none(Tuple) of
	  true -> error;
	  false ->
	    SubTuples = t_tuple_subtypes(Tuple),
	    Results = [bind_pat_vars(Es, t_tuple_args(SubTuple), [], Map, State)
		       || SubTuple <- SubTuples],
	    case lists:all(fun(X) -> X =:= error end, Results) of
	      true -> error;
	      false ->
		Map1 = join_maps([M || {M, _} <- Results], Map),
		TupleType = t_sup([t_tuple(EsTypes)|| {_, EsTypes} <- Results]),
		{Map1, TupleType}
	    end
	end;
      values ->
	Es = cerl:values_es(Pat),
	case bind_pat_vars(Es, t_components(Type), [], Map, State) of
	  error -> error;
	  {Map1, EsTypes} -> {Map1, t_product(EsTypes)}
	end;
      var ->
	%% Must do inf when binding args to pats. Vars in pats are fresh.
	VarType = t_inf(lookup_type(Pat, Map), Type),
	case t_is_none(VarType) of
	  true -> error;
	  false ->
	    Map1 = enter_type(Pat, VarType, Map),
	    {Map1, Type}
	end;
      _Other ->
	%% Catch all is needed when binding args to pats
	?debug("Failed match for ~p\n", [_Other]),
	error
    end,
  case Res of
    error -> error;
    {NewMap, TypeOut} -> 
      bind_pat_vars(PatLeft, TypeLeft, [TypeOut|Acc], NewMap, State)
  end;
bind_pat_vars([], [], Acc, Map, _State) ->
  {Map, lists:reverse(Acc)};
bind_pat_vars(Pats, Type, Acc, Map, State) ->
  case t_is_any(Type) of
    true -> 
      bind_pat_vars(Pats, duplicate(length(Pats), t_any()), Acc, Map, State);
    false ->
      case t_is_none(Type) of
	true -> error;
	false -> erlang:fault({'Error binding pats', Pats, Type})
      end
  end.
	  
bind_bin_segs([Bitstr|Left], Map) ->
  %% Only care about Size and Value since the other fields are
  %% constant literals. Size must be an integer - NO, it can be 'all'
  Val = cerl:bitstr_val(Bitstr),
  Size = cerl:bitstr_size(Bitstr),
  SizeType =  
    case cerl:type(Size) of
      literal ->
	%% Safety check
	case cerl:concrete(Size) of
	  all -> t_from_term(all);
	  N when is_integer(N) -> t_from_term(N)
	end;
      var ->
	t_inf(t_integer(), lookup_type(cerl:bitstr_size(Bitstr), Map))
    end,
  ValType = case cerl:type(Val) of
	      literal -> t_from_term(cerl:concrete(Val));
	      var -> t_any()
	    end,
  PatType =
    case cerl:concrete(cerl:bitstr_type(Bitstr)) of
      float -> t_float();
      binary -> t_binary();
      integer ->
	case t_is_integer(SizeType) of
	  true ->
	    case t_number_vals(SizeType) of
	      any -> t_integer();
	      List ->
		SizeVal = lists:max(List),
		Flags = cerl:concrete(cerl:bitstr_flags(Bitstr)),
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

bind_guard(Guard, Map, State) ->
  try bind_guard(Guard, Map, dict:new(), State) of
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

bind_guard(Guard, Map, Env, State) ->
  ?debug("Handling guard: ~s\n", [cerl_prettypr:format(Guard)]),
  case cerl:type(Guard) of
    binary -> 
      {Map, t_binary()};
    'case' ->
      Arg = cerl:case_arg(Guard),
      {NewMap, ArgType} = bind_guard(Arg, Map, Env, State),
      Clauses = cerl:case_clauses(Guard),
      bind_guard_case_clauses(ArgType, Clauses, NewMap, Env, State);
    cons ->
      {Map, t_cons()};
    literal ->
      {Map, literal_type(Guard)};
    'try' ->
      Arg = cerl:try_arg(Guard),
      [Var] = cerl:try_vars(Guard),
      %%?debug("Storing: ~w\n", [Var]),
      NewEnv = dict:store(get_label(Var), Arg, Env),
      bind_guard(cerl:try_body(Guard), Map, NewEnv, State);
    tuple ->
      {Map1, Es} = bind_guard_list(cerl:tuple_es(Guard), Map, Env, State),
      {Map1, t_tuple(Es)};
    'let' ->
      Arg = cerl:let_arg(Guard),
      [Var] = cerl:let_vars(Guard),
      %%?debug("Storing: ~w\n", [Var]),
      NewEnv = dict:store(get_label(Var), Arg, Env),
      bind_guard(cerl:let_body(Guard), Map, NewEnv, State);
    values ->
      List = [bind_guard(V, Map, Env, State) || V <- cerl:values_es(Guard)],
      Type = t_product([T || {_, T} <- List]),
      {Map, Type};
    var ->
      %?debug("Looking for: ~w...", [Guard]),
      case dict:find(get_label(Guard), Env) of
	error -> 
	  %?debug("Did not find it\n", []),
	  {Map, lookup_type(Guard, Map)};
	{ok, Tree} -> 
	  %?debug("Found it\n", []),
	  {Map1, Type} = bind_guard(Tree, Map, Env, State),
	  {enter_type(Guard, Type, Map1), Type}
      end;
    call ->
      Args = cerl:call_args(Guard),      
      M = cerl:atom_val(cerl:call_module(Guard)),
      F = cerl:atom_val(cerl:call_name(Guard)),
      A = length(Args),      
      case {M, F, A} of
	{erlang, is_atom, 1} ->
	  [Arg] = Args,
	  {Map1, ArgType} = bind_guard(Arg, Map, Env, State),
	  Type = t_inf(t_atom(), ArgType),
	  case t_is_none(Type) of
	    true -> throw(fail);
	    false -> {enter_type(Arg, Type, Map1), t_atom(true)}
	  end;
	{erlang, is_boolean, 1} ->
	  [Arg] = Args,
	  {Map1, ArgType} = bind_guard(Arg, Map, Env, State),
	  Type = t_inf(t_bool(), ArgType),
	  case t_is_none(Type) of
	    true -> throw(fail);
	    false -> {enter_type(Arg, Type, Map1), t_atom(true)}
	  end;
	{erlang, is_binary, 1} ->
	  [Arg] = Args,
	  {Map1, ArgType} = bind_guard(Arg, Map, Env, State),
	  Type = t_inf(t_binary(), ArgType),
	  case t_is_none(Type) of
	    true -> throw(fail);
	    false -> {enter_type(Arg, Type, Map1), t_atom(true)}
	  end;
	{erlang, is_float, 1} ->
	  [Arg] = Args,
	  {Map1, ArgType} = bind_guard(Arg, Map, Env, State),
	  Type = t_inf(t_float(), ArgType),
	  case t_is_none(Type) of
	    true -> throw(fail);
	    false -> {enter_type(Arg, Type, Map1), t_atom(true)}
	  end;
	{erlang, is_function, 1} ->
	  [Arg] = Args,
	  {Map1, ArgType} = bind_guard(Arg, Map, Env, State),
	  Type = t_inf(t_fun(), ArgType),
	  case t_is_none(Type) of
	    true -> throw(fail);
	    false -> {enter_type(Arg, Type, Map1), t_atom(true)}
	  end;
	{erlang, is_function, 2} ->
	  {Map1, [FunType0, ArityType0]} = 
	    bind_guard_list(Args, Map, Env, State),
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
	  {Map1, ArgType} = bind_guard(Arg, Map, Env, State),
	  Type = t_inf(t_integer(), ArgType),
	  case t_is_none(Type) of
	    true -> throw(fail);
	    false -> {enter_type(Arg, Type, Map1), t_atom(true)}
	  end;
	{erlang, is_list, 1} ->
	  [Arg] = Args,
	  {Map1, ArgType} = bind_guard(Arg, Map, Env, State),
	  Type = t_inf(t_pos_improper_list(), ArgType),
	  case t_is_none(Type) of
	    true -> throw(fail);
	    false -> {enter_type(Arg, Type, Map1), t_atom(true)}
	  end;
	{erlang, is_number, 1} ->
	  [Arg] = Args,
	  {Map1, ArgType} = bind_guard(Arg, Map, Env, State),
	  Type = t_inf(t_number(), ArgType),
	  case t_is_none(Type) of
	    true -> throw(fail);
	    false -> {enter_type(Arg, Type, Map1), t_atom(true)}
	  end;
	{erlang, is_pid, 1} ->
	  [Arg] = Args,
	  {Map1, ArgType} = bind_guard(Arg, Map, Env, State),
	  Type = t_inf(t_pid(), ArgType),
	  case t_is_none(Type) of
	    true -> throw(fail);
	    false -> {enter_type(Arg, Type, Map1), t_atom(true)}
	  end;
	{erlang, is_port, 1} ->
	  [Arg] = Args,
	  {Map1, ArgType} = bind_guard(Arg, Map, Env, State),
	  Type = t_inf(t_port(), ArgType),
	  case t_is_none(Type) of
	    true -> throw(fail);
	    false -> {enter_type(Arg, Type, Map1), t_atom(true)}
	  end;
	MFA when (MFA =:= {erlang, internal_is_record, 3}) or 
		 (MFA =:= {erlang, is_record, 3}) ->
	  [Rec, Tag0, Arity0] = Args,
	  Tag = cerl:atom_val(Tag0),
	  Arity = cerl:int_val(Arity0),
	  {Map1, RecType} = bind_guard(Rec, Map, Env, State),
	  TupleType =
	    case state__lookup_record(Tag, Arity - 1, State) of
	      error -> t_tuple([t_atom(Tag)|duplicate(Arity - 1, t_any())]);
	      {ok, Prototype} -> Prototype
	    end,
	  Type = t_inf(TupleType, RecType),
	  case t_is_none(Type) of
	    true -> throw(fail);
	    false -> {enter_type(Rec, Type, Map1), t_atom(true)}
	  end;
	{erlang, is_reference, 1} ->
	  [Arg] = Args,
	  {Map1, ArgType} = bind_guard(Arg, Map, Env, State),
	  Type = t_inf(t_ref(), ArgType),
	  case t_is_none(Type) of
	    true -> throw(fail);
	    false -> {enter_type(Arg, Type, Map1), t_atom(true)}
	  end;
	{erlang, is_tuple, 1} ->
	  [Arg] = Args,
	  {Map1, ArgType} = bind_guard(Arg, Map, Env, State),
	  Type = t_inf(t_tuple(), ArgType),
	  case t_is_none(Type) of
	    true -> throw(fail);
	    false -> {enter_type(Arg, Type, Map1), t_atom(true)}
	  end;
	{erlang, '=:=', 2} ->
	  [Arg1, Arg2] = Args,
	  {Map1, Type1} = bind_guard(Arg1, Map, Env, State),
	  {Map2, Type2} = bind_guard(Arg2, Map1, Env, State),
	  ?debug("Types are:~s and ~s\n", [t_to_string(Type1), 
					  t_to_string(Type2)]),
	  Inf = t_inf(Type1, Type2),
	  case t_is_none(Inf) of
	    true -> {Map, t_none()};
	    false -> 
	      case {cerl:type(Arg1), cerl:type(Arg2)} of
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
	{erlang, '==', 2} ->
	  [Arg1, Arg2] = Args,
	  {Map1, Type1} = bind_guard(Arg1, Map, Env, State),
	  {Map2, Type2} = bind_guard(Arg2, Map1, Env, State),
	  ?debug("Types are:~s and ~s\n", [t_to_string(Type1), 
					  t_to_string(Type2)]),
	  %% A very special case when at least one of the operands is an atom.
	  case (t_is_atom(Type1) orelse t_is_atom(Type2)
		orelse t_is_nil(Type1) orelse t_is_nil(Type2)) of
	    true ->
	      Inf = t_inf(Type1, Type2),
	      case t_is_none(Inf) of
		true -> {Map, t_none()};
		false -> 
		  case {cerl:type(Arg1), cerl:type(Arg2)} of
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
	    false ->
	      {Map2, t_bool()}
	  end;	  
	{erlang, 'and', 2} ->
	  [Arg1, Arg2] = Args,
	  True = t_atom(true),
	  {Map1, Bool1} = bind_guard(Arg1, Map, Env, State),
	  case t_is_none(t_inf(Bool1, True)) of
	      true ->
	      {Map, t_none()};
	    false ->
	      {Map2, Bool2} = bind_guard(Arg2, Map1, Env, State),
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
	      bind_guard(Arg1, Map, Env, State)
	    catch
	      throw:fail -> {Map, False}
	    end,
	  {Map2, Bool2} = 
	    try
	      bind_guard(Arg2, Map, Env, State)
	    catch
	      throw:fail -> {Map, False}
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
	  {Map1, As} = bind_guard_list(Args, Map, Env, State),
	  BifRet = erl_bif_types:type(M, F, A, As),
	  case t_is_none(BifRet) of
	    true ->
	      %% Is this an error-bif?
	      case t_is_none(erl_bif_types:type(M, F, A)) of
		true -> throw(fail);
		false -> throw(fatal_fail)
	      end;
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

bind_guard_list(Guards, Map, Env, State) ->
  bind_guard_list(Guards, Map, Env, State, []).

bind_guard_list([G|Left], Map, Env, State, Acc) ->
  {Map1, T} = bind_guard(G, Map, Env, State),
  bind_guard_list(Left, Map1, Env, State, [T|Acc]);
bind_guard_list([], Map, _Env, _State, Acc) ->
  {Map, lists:reverse(Acc)}.

bind_guard_case_clauses(ArgType, Clauses, Map, Env, State) ->
  Clauses1 = filter_fail_clauses(Clauses),
  bind_guard_case_clauses(ArgType, Clauses1, Map, Env, t_none(), [], State).

filter_fail_clauses([Clause|Left]) ->
  case (cerl:clause_pats(Clause) =:= []) of
    true ->
      Body = cerl:clause_body(Clause),
      case cerl:is_literal(Body) andalso (cerl:concrete(Body) =:= fail) of
	true -> filter_fail_clauses(Left);
	false -> [Clause|filter_fail_clauses(Left)]
      end;
    false ->
      [Clause|filter_fail_clauses(Left)]
  end;
filter_fail_clauses([]) ->
  [].

bind_guard_case_clauses(ArgType, [Clause|Left], Map, Env, AccType, 
			AccMaps, State) ->
  Pats = cerl:clause_pats(Clause),
  NewMap1 =
    case Pats =:= [] of
      true -> Map;
      false ->
	case bind_pat_vars(Pats, t_components(ArgType), [], Map, State) of
	  error -> none;
	  {PatMap, _PatTypes} -> PatMap
	end
    end,
  case NewMap1 =:= none of
    true ->
      bind_guard_case_clauses(ArgType, Left, Map, Env, AccType, AccMaps, State);
    false ->
      {NewAccType, NewAccMaps} =
	try
	  Guard = cerl:clause_guard(Clause),
	  {NewMap2, GuardType} = bind_guard(Guard, NewMap1, Env, State),	
	  case t_is_none(t_inf(t_from_term(true), GuardType)) of
	    true -> throw(fail);
	    false -> ok
	  end,
	  {NewMap3, CType} = bind_guard(cerl:clause_body(Clause), NewMap2, 
					Env, State),
	  {t_sup(AccType, CType), [NewMap3|AccMaps]}
	catch
	  throw:fail -> {AccType, AccMaps}
	end,
      bind_guard_case_clauses(ArgType, Left, Map, Env, NewAccType, 
			      NewAccMaps, State)
  end;
bind_guard_case_clauses(_ArgType, [], Map, _Env, AccType, AccMaps, _State) ->
  case t_is_none(AccType) of
    true -> throw(fail);
    false -> {join_maps(AccMaps, Map), AccType}
  end.

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
  case cerl:is_literal(Key) of
    true -> MS;
    false ->
      case cerl:is_c_values(Key) of
	true ->
	  Keys = cerl:values_es(Key),
	  case t_is_any(Val) orelse t_is_none(Val) of
	    true ->
	      enter_type_lists(Keys, duplicate(length(Keys), Val), MS);
	    false ->
	      enter_type_lists(cerl:values_es(Key), t_components(Val), MS)
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
  case cerl:is_literal(Val) of
    true -> 
      NewMap = dict:store(KeyLabel, literal_type(Val), Map),
      {NewMap, Subst};
    false ->
      case cerl:is_c_var(Val) of
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
  case cerl:is_literal(Key) of
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

lookup_fun_sig(Fun, Callgraph, FunSigs) ->
  MFAorLabel =
    case dialyzer_callgraph:lookup_name(Fun, Callgraph) of
      {ok, MFA} -> MFA;
      error -> Fun
    end,
  dialyzer_plt:lookup(FunSigs, MFAorLabel).

literal_type(Lit) ->
  t_from_term(cerl:concrete(Lit)).

mark_as_fresh(Trees, Map) ->
  Fun = fun(T, AccMap) ->
	    case cerl:is_literal(T) of
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

%%% ============================================================================
%%%
%%%  Utilities
%%%
%%% ============================================================================

add_def_vars([H|T], DefVars) ->
  add_def_vars(T, ordsets:add_element(get_label(H), DefVars));
add_def_vars([], DefVars) ->
  DefVars.

is_def_var(Var, DefVars) ->
  ordsets:is_element(get_label(Var), DefVars).

get_label(L) when is_integer(L) ->
  L;
get_label(T) ->
  cerl_trees:get_label(T).

any_none([X|Xs]) ->
  case t_is_none(X) of
    true ->
      true;
    false ->
      any_none(Xs)
  end;
any_none([]) -> false.


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


%%% ============================================================================
%%%
%%%  The State.
%%%
%%% ============================================================================

-record(state, {callgraph, envs, fun_tab, fun_sigs, plt, 
		records, tree_map, work}).

state__new(Callgraph, Tree, FunSigs, Plt, Records) -> 
  TreeMap = build_tree_map(Tree),
  Funs = dict:fetch_keys(TreeMap),
  FunTab = init_fun_tab(Funs, dict:new(), TreeMap, Callgraph, FunSigs),
  Work = init_work([get_label(Tree)]),
  Env = dict:store(top, {map__new(), []}, dict:new()),
  #state{callgraph=Callgraph, envs=Env, fun_sigs=FunSigs,
	 fun_tab=FunTab, plt=Plt, records=Records, work=Work, tree_map=TreeMap}.

state__is_escaping(Fun, #state{callgraph=Callgraph}) ->
  dialyzer_callgraph:is_escaping(Fun, Callgraph).

state__lookup_type_for_rec_var(Var, S = #state{callgraph=Callgraph}) ->
  Label = get_label(Var),
  case dialyzer_callgraph:lookup_rec_var(Label, Callgraph) of
    error -> error;
    {ok, MFA} ->
      case dialyzer_callgraph:lookup_label(MFA, Callgraph) of
	error -> error;
	{ok, FunLabel} ->
	  {ok, state__fun_type(FunLabel, S)}
      end
  end.

state__lookup_record(Tag, Arity, #state{records=Records}) ->
  case erl_types:lookup_record(Tag, Arity, Records) of
    {ok, Fields} -> 
      {ok, t_tuple([t_from_term(Tag)|
		    [FieldType || {_FieldName, FieldType} <- Fields]])};
    error -> 
      error
  end.

state__get_args(Tree, #state{fun_tab=FunTab}) ->
  Fun = get_label(Tree),
  case dict:find(Fun, FunTab) of
    error -> [];
    {ok, {ArgTypes, _}} -> ArgTypes
  end.

build_tree_map(Tree) ->
  Fun =
    fun(T, Dict) ->
	case cerl:is_c_fun(T) of
	  true ->
	    dict:store(get_label(T), T, Dict);
	  false ->
	    Dict
	end
    end,
  cerl_trees:fold(Fun, dict:new(), Tree).

init_fun_tab([Fun|Left], Dict, TreeMap, Callgraph, FunSigs) ->
  case cerl:fun_arity(dict:fetch(Fun, TreeMap)) of
    0 ->
      %% Ensure that this function will be analyzed once if it is
      %% called, by not entering anything.
      init_fun_tab(Left, Dict, TreeMap, Callgraph, FunSigs);
    Arity ->
      Args = 
	case dialyzer_callgraph:is_escaping(Fun, Callgraph) of
	  true -> 
	    case lookup_fun_sig(Fun, Callgraph, FunSigs) of
	      none -> duplicate(Arity, t_any());
	      {value, {RetType, ArgTypes}} -> 
		case any_none([RetType|ArgTypes]) of
		  true -> duplicate(Arity, t_none());
		  false -> ArgTypes
		end
	    end;
	  false -> duplicate(Arity, t_none())
	end,
      FunEntry = {Args, t_none()},
      NewDict = dict:store(Fun, FunEntry, Dict),
      init_fun_tab(Left, NewDict, TreeMap, Callgraph, FunSigs)
  end;
init_fun_tab([], Dict, _TreeMap, _Callgraph, _FunSigs) ->
  Dict.

state__update_fun_env(Tree, MapAndDef, State = #state{envs=Envs}) ->
  NewEnvs = dict:store(get_label(Tree), MapAndDef, Envs),
  State#state{envs=NewEnvs}.

state__fun_env(Tree, #state{envs=Envs}) ->
  Fun = get_label(Tree),
  case dict:find(Fun, Envs) of
    error -> none;
    {ok, MapAndDef} -> MapAndDef
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

state__add_work_from_fun(Tree, State = #state{callgraph=Callgraph, 
					      tree_map=TreeMap}) ->
  case get_label(Tree) of
    top -> State;
    Label when is_integer(Label) ->
      case dialyzer_callgraph:in_neighbours(Label, Callgraph) of
	none -> State;
	MFAList ->
	  LabelList = [dialyzer_callgraph:lookup_label(MFA, Callgraph)
		       || MFA <- MFAList],
	  %% Must filter the result for results in this module.	  
	  FilteredList = lists:filter(fun({ok, L})->dict:is_key(L, TreeMap)end,
				      LabelList),
	  ?debug("~w: Will try to add:~w\n", 
		 [debug_lookup_name(get_label(Tree)), MFAList]),
	  lists:foldl(fun({ok, X}, AccState)->
			  state__add_work(X, AccState)end,
		      State, FilteredList)
      end
  end.

state__add_work(external, State) ->
  State;
state__add_work(top, State) ->
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

state__find_apply_return(Tree, State = #state{callgraph=Callgraph}) ->
  Apply = get_label(Tree),
  case dialyzer_callgraph:lookup_call_site(Apply, Callgraph) of
    error ->
      unknown;
    {ok, List} ->
      case lists:member(external, List) of
	true -> t_any();
	false ->
	  FunTypes = [state__fun_type(F, State) || F <- List],
	  Returns = [t_fun_range(F) || F <- FunTypes],
	  t_sup(Returns)
      end
  end.

state__handle_apply(Tree, Args, State = #state{callgraph=Callgraph}) ->
  Apply = get_label(Tree),
  case dialyzer_callgraph:lookup_call_site(Apply, Callgraph) of
    error ->
      unknown;
    {ok, [external]} ->
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

state__fun_apply(Fun, Args, #state{callgraph=Callgraph, 
				   fun_sigs=FunSigs, 
				   fun_tab=FunTab}) ->
  NewArgs =
    case lookup_fun_sig(Fun, Callgraph, FunSigs) of
      none -> Args;
      {value, {ReturnType, ArgTypes}} -> 
	case t_is_none(ReturnType) of
	  true -> duplicate(length(Args), t_none());
	  false -> t_inf_lists(ArgTypes, Args)
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
      {ok, Funs} = dialyzer_callgraph:lookup_call_site(get_label(Apply), 
						       State#state.callgraph),
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
  
state__lookup_non_local(M, F, A, #state{plt=Plt}) ->
  Res = 
    case dialyzer_plt:lookup(Plt, {M, F, A}) of
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

-ifdef(DOT).
to_dot(CG) ->
  dialyzer_callgraph:to_dot(CG).
-else.
to_dot(_) ->
  ok.
-endif.
