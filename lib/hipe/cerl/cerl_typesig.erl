%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : cerl_typesig.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description : 
%%%
%%% 
%%% Created : 12 Jan 2005 by Tobias Lindahl <tobiasl@it.uu.se>
%%%
%%% $Id$
%%%-------------------------------------------------------------------

-module(cerl_typesig).

-export([get_export_signatures/1, doit/1, core_transform/2]).




-import(erl_types, 
	[t_any/0, t_atom/0, t_binary/0, t_bool/0, t_cons/2,
	 t_cons_hd/1, t_cons_tl/1, t_components/1, t_float/0,
	 t_is_float/1, t_from_range/2, t_fun/0, t_fun/2, t_fun_args/1,
	 t_fun_range/1, t_is_fun/1, t_improper_list/0, t_inf/1,
	 t_inf/2, t_inf_lists/2, t_integer/0, t_is_integer/1,
	 t_atom_vals/1, t_is_cons/1, t_is_equal/2,
	 t_is_improper_list/1, t_is_list/1, t_is_tuple/1, t_is_none/1,
	 t_is_any/1, t_is_subtype/2, t_limit/2, t_list_elements/1,
	 t_number/0, t_is_number/1, t_number_vals/1, t_pid/0, t_port/0,
	 t_product/1, t_ref/0, t_to_string/1, t_subtract/2, t_tuple/0,
	 t_tuple/1, t_tuple_args/1, t_tuple_arity/1, t_sup/1, t_sup/2,
	 t_is_var/1, t_var/1, t_var_name/1, t_from_term/1, t_none/0]).

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


%-define(DEBUG, true).
%-define(DEBUGMORE, true).

-define(TYPE_LIMIT, 5).

-ifdef(DEBUG).
-define(NEED_FORMAT, true).
-endif.

-ifdef(DEBUGMORE).
-ifndef(NEED_FORMAT).
-define(NEED_FORMAT, true).
-endif.
-endif.

-ifdef(DEBUG).
debug(String, Args) -> 
  io:format(String, Args).
-else.
debug(_String, _Args) -> 
  ok.
-endif.

-ifdef(DEBUG).
print_diff(Map1, Map2) ->
  Keys = ordsets:union(gb_trees:keys(Map1), gb_trees:keys(Map2)),
  [io:format("Key: ~w\nWas: ~s\n is now: ~s\n", 
	     [Key, t_to_string(lookup_type(Key, Map1)), 
	      t_to_string(lookup_type(Key, Map2))])
   || Key <- Keys,
      (lookup_type(Key, Map1) =/= lookup_type(Key, Map2)) == true],
  ok.
-else.
print_diff(_, _)->
  ok.
-endif.


-ifdef(DEBUGMORE).
debug_more(Tree, Map) -> 
  io:put_chars(cerl_prettypr:format(Tree)),
  io:nl(),
  [io:format("~w :: ~s\n", [Var, t_to_string(Type)])
   ||{Var, Type} <- gb_trees:to_list(Map)],
  ok.
  %tree_to_dot(Tree, Map).
-else.
debug_more(_Tree, _Map) -> 
  ok.
-endif.

doit(Module) ->
  {ok, _, Code} = compile:file(Module,[to_core,binary]), 
  {Tree, Map} = analyze(Code),
  debug_more(Tree, Map),
  pp_signatures(Tree, Map),
  ok.      

get_export_signatures(Code) ->
  {Tree, Map} = analyze(Code),
  Exports = module_exports(Tree),
  [{var_name(Fun), lookup_type(Fun, Map)} || Fun <- Exports].

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
  DelAnn = fun (T) -> set_ann(T, delete_ann(type, get_ann(T))) end,
  SetType = fun (T, Map) ->
		X = lookup_type(T, Map),
		case t_is_any(X) of
		  true -> DelAnn(T);
		  false -> set_ann(T, append_ann(type, X, get_ann(T)))
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
  {Tree, _NextLabel} = cerl_trees:label(cerl:from_records(Code)),  
  {Tree, analyze_loop(Tree, gb_trees:empty())}.
  

analyze_loop(Tree, Map) ->
  {_, Map1} = traverse(Tree, Map),
  case gb_trees:balance(Map1) of
    Map -> Map;
    Map2 ->   
      debug("============  Merry go around ===========\n", []),
      print_diff(Map, Map2),
      analyze_loop(Tree, Map2)
  end.

traverse(Tree, Map) ->
  %%debug("Handling ~p\n", [Tree]),
  case type(Tree) of
    alias ->
      Var = alias_var(Tree),
      Pat = alias_pat(Tree),
      VarType = lookup_type(Var, Map),
      {PatType, Map1} = traverse(Pat, Map),
      Inf = t_inf(PatType, VarType),
      Map2 = enter_type(Var, mk_var(Pat), Map1),
      Map3 = enter_type(Var, Inf, Map2),
      {Inf, enter_type(Tree, Inf, Map3)};
    apply ->
      handle_apply(Tree, Map);
    binary ->
      {_, Map1} = traverse_list(binary_segments(Tree), Map),
      {t_binary(), enter_type(Tree, t_binary(), Map1)};
    bitstr ->
      %% Only care about Size and Value since the other fields are
      %% constant literals. Size must be an integer.
      Size = bitstr_size(Tree),
      {SizeType, Map1} = traverse(Size, Map),
      Map2 = enter_type(Size, t_inf(t_integer(), SizeType), Map1),
      ValType = 
	case concrete(bitstr_type(Tree)) of
	  float -> t_float();
	  binary -> t_binary();
	  integer ->
	    case t_number_vals(SizeType) of
	      [SizeVal] -> 
		Flags = concrete(bitstr_flags(Tree)),
		case lists:member(signed, Flags) of
		  true -> 
		    t_from_range(-(1 bsl (SizeVal - 1)),
				 1 bsl (SizeVal - 1) - 1);
		  false -> 
		    t_from_range(0,1 bsl SizeVal - 1)
		end;
	      _ -> t_integer()
	    end
	end,
      Map3 = enter_type(bitstr_val(Tree), ValType, Map2),
      {t_none(), Map3};
    'case' ->
      handle_case(Tree, Map);
    call ->
      handle_call(Tree, Map);
    'catch' ->
      {t_any(), Map};
    cons ->
      {[Hd, Tl], Map1} = 
	traverse_list([cons_hd(Tree), cons_tl(Tree)], Map),
      Type = t_cons(Hd, Tl),
      {Type, enter_type(Tree, Type, Map1)};
    'fun' ->
      Body = fun_body(Tree),
      Vars = fun_vars(Tree),
      {BodyType, Map1} = traverse(Body, Map),
      {VarsType, _Map2} = traverse_list(Vars, Map1),
      Type = t_limit(t_fun(VarsType, BodyType), ?TYPE_LIMIT),
      {Type, enter_type(Tree, Type, Map)};
    'let' ->
      Vars = let_vars(Tree),
      Arg = let_arg(Tree),
      Body = let_body(Tree),
      {ArgType, Map1} = traverse(Arg, Map),
      case t_components(ArgType) of
	none -> 
	  Map2 = enter_type_lists(Vars, duplicate(length(Vars),t_none()), Map),
	  {t_none(), enter_type(Tree, t_none(), Map2)};
	any ->
	  Map2 = enter_type_lists(Vars, duplicate(length(Vars),t_any()), Map1),
	  {BodyType, Map3} = traverse(Body, Map2),
	  {BodyType, enter_type(Tree, BodyType, Map3)};
	ArgTypeList ->
	  case any_none(ArgTypeList) of
	    true ->
	      {t_none(), enter_type(Tree, t_none(), Map)};
	    false ->
	      Map2 = enter_type_lists(Vars, ArgTypeList, Map1),
	      {BodyType, Map3} = traverse(Body, Map2),
	      case t_is_none(BodyType) of
		true -> {t_none(), enter_type(Tree, t_none(), Map)};
		false -> {BodyType, enter_type(Tree, BodyType, Map3)}
	      end
	  end
      end;
    letrec ->
      Defs = letrec_defs(Tree),
      Body = letrec_body(Tree),
      Funs = [Fun || {_Var, Fun} <- Defs],
      Vars = [Var || {Var, _Fun} <- Defs],

      %% We want to force self recursive calls to return none to
      %% identify the leaf cases.
      Fun = fun(X, Acc) -> 
		case t_is_any(lookup_type(X, Map)) of
		  true -> enter_type(X, t_none(), Acc);
		  false -> Acc
		end
	    end,
      Map1 = lists:foldl(Fun, Map, Vars),
      {FunTypes, Map2} = traverse_list(Funs, Map1),
      Map3 = enter_type_lists(Vars, FunTypes, Map2),
      traverse(Body, Map3);
    literal ->      
      {t_from_term(concrete(Tree)), Map};
    module ->
      Defs = module_defs(Tree),
      Exports = module_exports(Tree),
      traverse(c_letrec(Defs, c_values(Exports)), Map);
    primop ->
      case atom_val(primop_name(Tree)) of
	match_fail -> {t_none(), enter_type(Tree, t_none(), Map)};
	raise -> {t_none(), enter_type(Tree, t_none(), Map)};
	Other -> erlang:fault({'Unsupported primop', Other})
      end;
    'receive' ->
      handle_receive(Tree, Map);
    seq ->
      Body = seq_body(Tree),
      Arg = seq_arg(Tree),
      {ArgType, Map1} = traverse(Arg, Map),
      case t_is_none(ArgType) of
	true -> 	  
	  {t_none(), enter_type(Tree, t_none(), Map)};
	false -> 
	  {BodyType, Map2} = traverse(Body, Map1),
	  case t_is_none(BodyType) of
	    true -> 
	      {t_none(), enter_type(Tree, t_none(), Map)};
	    false -> 
	      {BodyType, enter_type(Tree, BodyType, Map2)}
	  end
      end;
    'try' ->
      Arg = try_arg(Tree),
      Vars = try_vars(Tree),
      Body = try_body(Tree),
      Handler = try_handler(Tree),

      {HandlerType, HandlerMap} = traverse(Handler, Map),
      debug("HandlerMap: ~p\n", [[{Key, 
				   t_to_string(lookup_type(Key, HandlerMap))}
				  || Key<-gb_trees:keys(HandlerMap)]]),

      {_ArgType, Map1} = traverse(Arg, Map),
      Arg1 = case is_c_values(Arg) of
	       true -> values_es(Arg);
	       false -> [Arg]
	     end,
      case bind_pats(Arg1, Vars, Map1) of
	{ok, Map2} -> 
	  {BodyType, BodyMap} = traverse(Body, Map2),
	  case bind_pats(Arg1, Vars, Map2) of
	    {ok, BodyMap2} ->
	      debug("BodyMap: ~p\n", [[{Key, 
					t_to_string(lookup_type(Key, BodyMap2))}
				       || Key<-gb_trees:keys(BodyMap2)]]),
	      Map3 = merge_maps([HandlerMap, BodyMap]),
	      debug("MergedMap: ~p\n", [[{Key, 
					  t_to_string(lookup_type(Key, Map3))}
					 || Key<-gb_trees:keys(Map3)]]),
	      Type = t_sup(HandlerType, BodyType);
	    error ->
	      Map3 = HandlerMap,
	      Type = HandlerType
	  end;
	error ->
	  Map3 = HandlerMap,
	  Type = HandlerType
      end,
      {Type, enter_type(Tree, Type, Map3)};
    tuple ->
      Elements = tuple_es(Tree),
      {ElementTypes, Map1} = traverse_list(Elements, Map),
      Type = t_tuple(ElementTypes),
      {Type, enter_type(Tree, Type, Map1)};
    values ->
      Elements = values_es(Tree),
      {ElementTypes, Map1} = traverse_list(Elements, Map),
      Type = t_product(ElementTypes),
      {Type, Map1};
    var ->
      Type = lookup_type(Tree, Map),
      {Type, Map};
    Other ->
      erlang:fault({'Unsupported type', Other})
  end.

traverse_list(Trees, Map) ->
  traverse_list(Trees, Map, []).

traverse_list([Tree|Tail], Map, Acc) ->
  {Type, Map1} = traverse(Tree, Map),
  traverse_list(Tail, Map1, [Type|Acc]);
traverse_list([], Map, Acc) ->
  {lists:reverse(Acc), Map}.
  

%%________________________________________
%%
%% Apply
%%

handle_apply(Apply, Map) ->
  Args = apply_args(Apply),
  {ArgTypes, Map1} = traverse_list(Args, Map),
  {OpType, Map2} = traverse(apply_op(Apply), Map1),
%  debug("\nHandling apply of ~w::~s with args ~w::~s\n",
%	[apply_op(Apply), t_to_string(OpType),
%	 Args, t_to_string(t_product(ArgTypes))]),
  case t_is_fun(OpType) of
    true ->
      case t_fun_args(OpType) of
	any ->
	  case t_fun_range(OpType) of
	    any ->
	      {t_any(), enter_type(Apply, t_any(), Map2)};
	    Range ->
	      {Range, enter_type(Apply, Range, Map2)}
	  end;
	FunArgs ->
	  NewArgTypes = [t_limit(X, ?TYPE_LIMIT) 
			 || X <- t_inf_lists(ArgTypes, FunArgs)],
	  Type = strict(NewArgTypes, t_fun_range(OpType)),
	  Map3 = enter_type_lists(Args, NewArgTypes, Map2),
	  {Type, enter_type(Apply, Type, Map3)}
      end;
    false ->
      FunType = t_inf(t_fun(), OpType),
      case t_is_none(FunType) of
	true -> 
	  {t_none(), enter_type(Apply, t_none(), Map)};
	false ->
	  Map3 = enter_type(apply_op(Apply), FunType, Map2),
	  {t_any(), enter_type(Apply, t_any(), Map3)}
      end
  end.
      

%%________________________________________
%%
%% Call
%%

handle_call(Call, Map) ->      
  Args = call_args(Call),
  M = atom_val(call_module(Call)),
  F = atom_val(call_name(Call)),
  Args = call_args(Call),
  A = length(Args),
  
  {ArgTypes, Map1} = traverse_list(Args, Map),
  case erl_bif_types:arg_types(M, F, A) of
    any ->
      NewArgTypes = ArgTypes,
      Map2 = Map1;
    Constraints ->
      NewArgTypes = t_inf_lists(ArgTypes, Constraints),
      Map2 = enter_type_lists(Args, NewArgTypes, Map1)
  end,
  ReturnType = erl_bif_types:type(M, F, A, NewArgTypes),
  {ReturnType, enter_type(Call, ReturnType, Map2)}.

%%________________________________________
%%
%% Case
%%

handle_case(Case, Map) ->
  Arg = case_arg(Case),
  Clauses = filter_match_fail(case_clauses(Case)),
  {_ArgType, Map1} = traverse(Arg, Map),
  case handle_case_clauses(Clauses, Arg, Map1) of
    [] ->
      debug("Case cannot succeed\n", []),
      %% No clause was valid! The case will always fail.
      {t_none(), enter_type(Case, t_none(), Map1)};
    BodyMapList ->
      debug("Case CAN succeed\n", []),
      Maps = [M || {Type, M} <- BodyMapList, t_is_none(Type)=:=false],
      Bodies = [Type || {Type, _M} <- BodyMapList],  
      Map2 = merge_maps(Maps),
      Type = t_sup(Bodies),
      {Type, enter_type(Case, Type, Map2)}
  end.

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
  [H|filter_match_fail(T)].

handle_case_clauses(Clauses, Arg, Map) ->
  handle_case_clauses(Clauses, Arg, Map, []).

handle_case_clauses([Clause|Tail], Arg, Map, Acc) ->
  Pats = clause_pats(Clause),
  case type(Arg) of
    values -> Arg1 = values_es(Arg);
    _ -> Arg1 = [Arg]
  end,
  {_PatType, Map1} = traverse_list(Pats, Map),
  Guard = clause_guard(Clause),
  case handle_guard(Guard, Map1) of
    {ok, Map2} ->
      case bind_pats(Arg1, Pats, Map2) of
	{ok, Map3} ->
	  Body = clause_body(Clause),
	  {BodyType, Map4} = traverse(Body, Map3),
	  case t_is_none(BodyType) of
	    true ->
	      handle_case_clauses(Tail, Arg, Map, Acc);
	    false ->
	      case bind_pats(Arg1, Pats, Map4) of
		{ok, Map5} ->
		  handle_case_clauses(Tail, Arg, Map, [{BodyType, Map5}|Acc]);
		error ->
		  handle_case_clauses(Tail, Arg, Map, Acc)
	      end
	  end;
	error ->
	  handle_case_clauses(Tail, Arg, Map, Acc)
      end;
    error ->
      handle_case_clauses(Tail, Arg, Map, Acc)
  end;
handle_case_clauses([], _Arg, _Map, Acc) ->
  Acc.

bind_pats([Arg|ArgTail], [Pat|PatTail], Map) ->
  {ArgType, _} = traverse(Arg, Map),
  {PatType, _} = traverse(Pat, Map),
  Inf = t_inf(ArgType, PatType),
  case t_is_none(Inf) of
    true -> error;
    false -> 
      case {type(Arg), type(Pat)} of
	{_, alias} ->
	  Map1 = enter_type(Arg, Inf, Map),
	  Map2 = enter_type(alias_var(Pat), mk_var(Arg), Map1),
	  Map3 = bind_pat_to_type([Arg], [Inf], Map2),
	  Map4 = bind_pat_to_type([alias_pat(Pat)], [Inf], Map3),
	  bind_pats(ArgTail, PatTail, Map4);
	{_, var} ->
	  Map1 = enter_type(Arg, Inf, Map),
	  Map2 = enter_type(Pat, mk_var(Arg), Map1),
	  bind_pats(ArgTail, PatTail, Map2);
	_ ->
	  Map1 = enter_type_lists([Arg, Pat], [Inf, Inf], Map),
	  Map2 = bind_pat_to_type([Arg], [Inf], Map1),
	  Map3 = bind_pat_to_type([Pat], [Inf], Map2),
	  bind_pats(ArgTail, PatTail, Map3)
      end
  end;
bind_pats([], [], Map) ->
  {ok, Map}.


bind_pat_to_type([Pat|PatTail], [Type|TypeTail], Map) ->
  Map1 = bind_one_pat(Pat, Type, Map),
  bind_pat_to_type(PatTail, TypeTail, Map1);
bind_pat_to_type([], [], Map) ->
  Map.

bind_one_pat(Pat, Type0, Map) ->
  case type(Pat) of
    alias ->
      Var = alias_var(Pat),
      Pat1 = alias_pat(Pat),
      Map1 = enter_type(Var, mk_var(Pat1), Map),
      bind_one_pat(Pat1, Type0, Map1);
    cons ->
      Type = t_inf(t_improper_list(), Type0),
      case t_is_cons(Type) of
	true ->
	  bind_pat_to_type([cons_hd(Pat), cons_tl(Pat)], 
			   [t_cons_hd(Type), t_cons_tl(Type)], Map);
	false ->
	  bind_pat_to_type([cons_hd(Pat), cons_tl(Pat)],
			   [t_list_elements(Type), Type], Map)
      end;
    literal ->
      Map;
    tuple ->
      Type = t_inf(t_tuple(), Type0),
      Elements = tuple_es(Pat),
      case t_tuple_args(Type) of
	any ->
	  Map;
	Args ->
	  bind_pat_to_type(Elements, Args, Map)
      end;
    values ->
      bind_pat_to_type(values_es(Pat), t_components(Type0), Map);
    var ->
      Inf = t_inf(lookup_type(Pat, Map), Type0),
      case t_is_none(Inf) of
	true -> throw(error);
	false -> enter_type(Pat, Inf, Map)
      end;
    _ ->
      Map
  end.

%%________________________________________
%%
%% Receive
%%

handle_receive(Receive, Map) ->
  Clauses = filter_match_fail(receive_clauses(Receive)),
  {Type, Maps} = handle_receive_clauses(Clauses, Map),
  Timout = receive_timeout(Receive),
  case is_c_atom(Timout) andalso atom_val(Timout) =:= 'infinity' of
    true ->
      Map1 = merge_maps(Maps),
      {Type, enter_type(Receive, Type, Map1)};
    false ->
      Action = receive_action(Receive),
      {ActionType, ActionMap} = traverse(Action, Map),
      Map1 = merge_maps([ActionMap|Maps]),
      Type1 = t_sup(Type, ActionType),
      {Type1, enter_type(Receive, Type1, Map1)}
  end.

handle_receive_clauses(Clauses, Map) ->
  handle_receive_clauses(Clauses, Map, t_none(), []).

handle_receive_clauses([Clause|Tail], Map, AccType, AccMap) ->
  {_Type, Map1} = traverse_list(clause_pats(Clause), Map),
  case handle_guard(clause_guard(Clause), Map1) of
    error -> 
      handle_receive_clauses(Tail, Map, AccType, AccMap);
    {ok, Map2} -> 
      {BodyType, Map3} = traverse(clause_body(Clause), Map2),
      case t_is_none(BodyType) of
	true -> 
	  handle_receive_clauses(Tail, Map, AccType, AccMap);
	false -> 
	  handle_receive_clauses(Tail, Map, t_sup(BodyType, AccType), [Map3|AccMap])
      end
  end;
handle_receive_clauses([], _Map, AccType, AccMap) ->
  {AccType, AccMap}.
	      
      

%%________________________________________
%%
%% Guards
%%

handle_guard(Guard, Map) ->  
  try handle_guard(Guard, Map, dict:new()) of
      {ok, _, Map1} -> {ok, Map1}
  catch
    throw:error -> error;
    throw:dont_know -> {ok, Map}
  end.

handle_guard(Guard, Map, Env) ->
  %%  debug("Handling: ~w\n", [type(Guard)]),
  case type(Guard) of
    binary -> 
      {Type, Map1} = traverse(Guard, Map),
      {ok, Type, Map1};
    literal ->
      {ok, t_from_term(concrete(Guard)), Map};
    'try' ->
      {ok, _, Map1} = handle_guard(try_arg(Guard), Map, Env),
      handle_guard(try_body(Guard), Map1, Env);
    'let' ->
      Arg = let_arg(Guard),
      [Var] = let_vars(Guard),
      %debug("Storing: ~w\n", [Var]),
      handle_guard(let_body(Guard), Map, dict:store(var_name(Var), Arg, Env));
    var ->
      %debug("Looking for: ~w...", [Guard]),
      case dict:find(var_name(Guard), Env) of
	error -> 
	  %debug("Did not find it\n", []),
	  {ok, lookup_type(Guard, Map), Map};
	{ok, Tree} -> 
	  %debug("Found it\n", []),
	  handle_guard(Tree, Map, Env)
      end;
    call ->
      Args = call_args(Guard),      
      M = atom_val(call_module(Guard)),
      F = atom_val(call_name(Guard)),
      A = length(Args),      
      case {M, F, A} of
	{erlang, is_atom, 1} ->
	  [Arg] = Args,
	  Type = t_inf(lookup_type(Arg, Map), t_atom()),
	  check_guard_call(Args, Type, Map);
	{erlang, is_boolean, 1} ->
	  [Arg] = Args,
	  Type = t_inf(lookup_type(Arg, Map), t_bool()),
	  check_guard_call(Args, Type, Map);
	{erlang, is_binary, 1} ->
	  [Arg] = Args,
	  Type = t_inf(lookup_type(Arg, Map), t_binary()),
	  check_guard_call(Args, Type, Map);
	{erlang, is_float, 1} ->
	  [Arg] = Args,
	  Type = t_inf(lookup_type(Arg, Map), t_float()),
	  check_guard_call(Args, Type, Map);
	{erlang, is_function, 1} ->
	  [Arg] = Args,
	  Type = t_inf(lookup_type(Arg, Map), t_fun()),
	  check_guard_call(Args, Type, Map);
	{erlang, is_integer, 1} ->
	  [Arg] = Args,
	  Type = t_inf(lookup_type(Arg, Map), t_integer()),
	  check_guard_call(Args, Type, Map);
	{erlang, is_list, 1} ->
	  [Arg] = Args,
	  Type = t_inf(lookup_type(Arg, Map), t_improper_list()),
	  check_guard_call(Args, Type, Map);
	{erlang, is_number, 1} ->
	  [Arg] = Args,
	  Type = t_inf(lookup_type(Arg, Map), t_number()),
	  check_guard_call(Args, Type, Map);
	{erlang, is_pid, 1} ->
	  [Arg] = Args,
	  Type = t_inf(lookup_type(Arg, Map), t_pid()),
	  check_guard_call(Args, Type, Map);
	{erlang, is_port, 1} ->
	  [Arg] = Args,
	  Type = t_inf(lookup_type(Arg, Map), t_port()),
	  check_guard_call(Args, Type, Map);
	{erlang, internal_is_record, 3} ->
	  [Arg1, Tag, Arity] = Args,
	  Type = t_inf(t_tuple([t_from_term(concrete(Tag))
				|duplicate(concrete(Arity)-1, t_any())]),
		       lookup_type(Arg1, Map)),
	  check_guard_call([Arg1], Type, Map);
	{erlang, is_reference, 1} ->
	  [Arg] = Args,
	  Type = t_inf(lookup_type(Arg, Map), t_ref()),
	  check_guard_call(Args, Type, Map);
	{erlang, is_tuple, 1} ->
	  [Arg] = Args,
	  Type = t_inf(lookup_type(Arg, Map), t_tuple()),
	  check_guard_call(Args, Type, Map);
	{erlang, '=:=', 2} ->
	  [Arg1, Arg2] = Args,
	  {ok, TypeArg1, Map1} = handle_guard(Arg1, Map, Env),
	  {ok, TypeArg2, Map2} = handle_guard(Arg2, Map1, Env),
	  Type = t_inf(TypeArg1, TypeArg2),
	  case t_is_none(Type) of
	    true -> throw(error);
	    false ->
	      case {type(Arg1), type(Arg2)} of
		{var, var} -> 
		  Map3 = enter_type(Arg1, mk_var(Arg2), Map2),
		  Map4 = enter_type(Arg1, Type, Map3),
		  check_guard_call(Args, Type, Map4);
		{_, _} -> 
		  check_guard_call(Args, Type, Map2)
	      end
	  end;
	{erlang, '==', 2} ->
	  [Arg1, Arg2] = Args,
	  {ok, TypeArg1, Map1} = handle_guard(Arg1, Map, Env),
	  {ok, TypeArg2, Map2} = handle_guard(Arg2, Map1, Env),
	  Type = t_inf(TypeArg1, TypeArg2),
	  case t_is_number(Type) of
	    true ->
	      case {t_is_number(TypeArg1), t_is_number(TypeArg2)} of
		{true, true} ->
		  case (t_is_float(TypeArg1) andalso t_is_float(TypeArg2)) of
		    true ->
		      Map3 = enter_type(Arg1, Type, Map2),
		      Map4 = enter_type(Arg2, Type, Map3),
		      {ok, t_from_term(true), Map4};
		    false ->
		      case (t_is_integer(TypeArg1) 
			    andalso t_is_integer(TypeArg2)) of
			true -> 
			  Map3 = enter_type(Arg1, Type, Map2),
			  Map4 = enter_type(Arg2, Type, Map3),
			  {ok, t_from_term(true), Map4};
			false ->
			  {ok, t_from_term(true), 
			   enter_type(Arg2, t_number(), Map2)}
		      end
		  end;
		{true, false} -> 
		  {ok, t_from_term(true), enter_type(Arg2, t_number(), Map2)};
		{false, true} ->
		  {ok, t_from_term(true), enter_type(Arg1, t_number(), Map2)};
		{false, false} ->
		  {ok, t_from_term(true), 
		   enter_type_lists(Args, duplicate(length(Args), Type), Map2)}
	      end;
	    false ->
	      check_guard_call(Args, Type, Map2)
	  end;
	{erlang, 'and', 2} ->
	  [Arg1, Arg2] = Args,
	  {ok, TypeArg1, Map1} = handle_guard(Arg1, Map, Env),
	  {ok, TypeArg2, Map2} = handle_guard(Arg2, Map1, Env),
	  True = t_from_term(true),
	  Type = t_inf(True, t_inf(TypeArg1, TypeArg2)),
	  check_guard_call(Args, Type, Map2);
	{erlang, 'or', 2} ->
	  [Arg1, Arg2] = Args,
	  NewMap = 
	    try handle_guard(Arg1, Map, Env) of
	      {ok, _TypeArg1, Map1} ->		
		try handle_guard(Arg2, Map, Env) of
		  {ok, _TypeArg2, Map2} -> 
		    merge_maps([Map1, Map2])
		catch
		  throw:error -> Map1
		end
	    catch
	      throw:error ->
		{ok, _, Map1} = handle_guard(Arg2, Map, Env),
		Map1
	    end,
	  {ok, t_from_term(true), NewMap};
	{erlang, 'not', 1} ->
	  throw(dont_know);
	_Other ->
	  Map1 = lists:foldl(fun(X, AccMap)->
				 {ok, _, TmpMap} = handle_guard(X, AccMap, Env),
				 TmpMap
			     end,
			     Map, Args),
	  {Type, Map2} = handle_call(Guard, Map1),
	  {ok, Type, Map2}
      end
  end.
	       
check_guard_call(Args, Inf, Map) ->	
  case t_is_none(Inf) of
    true ->
      throw(error);
    false ->
      {ok, t_from_term(true), 
       enter_type_lists(Args, duplicate(length(Args), Inf), Map)}
  end.

%%% ============================================================================
%%%
%%%  Maps and types.
%%%
%%% ============================================================================

merge_maps([]) ->
  gb_trees:empty();
merge_maps([Map]) ->
  Map;
merge_maps([Map1, Map2|Tail]) ->
  Keys = ordsets:union(gb_trees:keys(Map1), gb_trees:keys(Map2)),
  Map = merge_two_maps(Keys, Map1, Map2, gb_trees:empty()),
  merge_maps([Map|Tail]).

merge_two_maps([Key|Tail], Map1, Map2, AccMap) ->
  Type1 = lookup_type(Key, Map1),
  Type2 = lookup_type(Key, Map2),
  NewAccMap = gb_trees:insert(Key, t_sup(Type1, Type2), AccMap),
  merge_two_maps(Tail, Map1, Map2, NewAccMap);
merge_two_maps([], _Map1, _Map2, AccMap) ->
  AccMap.

enter_type(Key, Val, Map) ->
  debug("Entering ~w :: ~s\n", [(catch get_label(Key)), t_to_string(Val)]),
  case t_is_any(Val) of
    true -> 
      gb_trees:delete_any(Key, Map);
    false ->
      case is_literal(Key) of
	true -> Map;
	false -> 	  
	  enter_type_to_leaf(get_label(Key), Val, Map)
      end
  end.

enter_type_lists([Key|KeyTail], [Val|ValTail], Map) ->
  Map1 = enter_type(Key, Val, Map),
  enter_type_lists(KeyTail, ValTail, Map1);
enter_type_lists([], [], Map) ->
  Map.

enter_type_to_leaf(Key, Val, Map) ->
  case gb_trees:lookup(Key, Map) of
    none -> 
      gb_trees:insert(Key, Val, Map);
    {value, Val} ->
      Map;
    {value, Val2} ->
      case t_is_var(Val2) of
	true -> enter_type_to_leaf(t_var_name(Val2), Val, Map);
	false -> gb_trees:update(Key, Val, Map)
      end
  end.

lookup_type(Key, Map) ->
  Ans = lookup_type_1(Key, Map),
  case t_is_var(Ans) of
    true -> lookup_type(t_var_name(Ans), Map);
    false -> Ans
  end.

lookup_type_1(Key, Map) when is_integer(Key) ->
  case gb_trees:lookup(Key, Map) of
    none -> t_any();
    {value, Val} -> Val
  end;
lookup_type_1(Key, Map) ->
  case is_literal(Key) of
    true -> t_from_term(concrete(Key));
    false ->
      Key1 = get_label(Key),
      case gb_trees:lookup(Key1, Map) of
	none -> t_any();
	{value, Val} -> Val
      end
  end.

mk_var(Var) ->
  case is_literal(Var) of
    true -> t_from_term(concrete(Var));
    false -> t_var(get_label(Var))
  end.

get_label(T) ->
  case get_ann(T) of
    [{label, L} | _] -> L;
    _ -> erlang:fault({missing_label, T})
  end.

strict(Xs, T) ->
  case any_none(Xs) of
    true ->
      t_none();
    false ->
      T
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
%%%  Utilities.
%%%
%%% ============================================================================

duplicate(0, _Element) ->
  [];
duplicate(N, Element) ->
  [Element|duplicate(N-1, Element)].


pp_signatures(Tree, Map) ->
  ModuleName = atom_val(module_name(Tree)),
  Defs = [Var || {Var, _Body} <- module_defs(Tree)],
  io:nl(),
  PPFun = fun(FunVar) ->
	      {FunName, Arity} = var_name(FunVar),
	      Type = t_to_string(lookup_type(FunVar, Map)),
	      io:format("~w:~w/~w :: ~s\n", [ModuleName, FunName, Arity, Type])
	  end,
  lists:foreach(PPFun, Defs).
