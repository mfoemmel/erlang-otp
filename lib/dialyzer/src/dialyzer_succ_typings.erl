%%%-------------------------------------------------------------------
%%% File    : dialyzer_succ_typings.erl
%%% Author  : Tobias Lindahl <tobiasl@csd.uu.se>
%%% Description : 
%%%
%%% Created : 11 Sep 2006 by Tobias Lindahl <tobiasl@csd.uu.se>
%%%-------------------------------------------------------------------
-module(dialyzer_succ_typings).

-export([analyze_callgraph/3,
	 get_warnings/5]).

%% These are only intended as debug functions.
-export([doit/1,
	 analyze_callgraph_only_typesig/3,
	 analyze_callgraph_only_dataflow/3,
	 get_top_level_signatures/2]).

-define(TYPE_LIMIT, 4).

%-define(DEBUG, true).
%-define(DEBUG_PP, true).

-ifdef(DEBUG).
-define(debug(X__, Y__), io:format(X__, Y__)).
-else.
-define(debug(X__, Y__), ok).
-endif.

analyze_callgraph(Callgraph, Plt, Codeserver) ->
  get_refined_success_typings(Callgraph, Plt, Codeserver).

analyze_callgraph_only_dataflow(Callgraph, Plt, Codeserver) ->
  ModulePostorder = dialyzer_callgraph:module_postorder(Callgraph),
  refine_succ_typings(ModulePostorder, Callgraph, Codeserver, true, Plt),
  Plt.

analyze_callgraph_only_typesig(Callgraph, Plt, Codeserver) ->
  find_succ_typings(Callgraph, Codeserver, Plt),
  Plt.

get_refined_success_typings(Callgraph, Plt, Codeserver) ->  
  case find_succ_typings(Callgraph, Codeserver, Plt) of
    fixpoint -> Plt;
    {not_fixpoint, Callgraph1, NotFixpoint1} ->
      NotFixpoint2 = [lookup_name(F, Callgraph) || F <- NotFixpoint1],
      ModulePostorder = 
	dialyzer_callgraph:module_postorder_from_funs(NotFixpoint2, Callgraph),
      case refine_succ_typings(ModulePostorder, Callgraph1, 
			       Codeserver, false, Plt) of
	fixpoint ->
	  Plt;
	{not_fixpoint, NotFixpoint3} ->
	  %% Need to reset the callgraph.
	  NotFixpoint4 = [lookup_name(F, Callgraph1) || F <- NotFixpoint3],
	  Callgraph2 = 
	    dialyzer_callgraph:reset_from_funs(NotFixpoint4, Callgraph1),
	  get_refined_success_typings(Callgraph2, Plt, Codeserver)
      end
  end.

get_warnings(Callgraph, Plt, DocPlt, Codeserver, NoWarnUnused) ->
  analyze_callgraph_only_dataflow(Callgraph, Plt, Codeserver),
  Modules = dialyzer_callgraph:modules(Callgraph),
  get_warnings(Modules, Callgraph, Plt, DocPlt, Codeserver, NoWarnUnused, []).

get_warnings([M|Left], Callgraph, Plt, DocPlt, Codeserver, NoWarnUnused, Acc) ->
  {ok, Tree} = dialyzer_codeserver:lookup(M, core, Codeserver),
  Records = dialyzer_codeserver:lookup_records(M, Codeserver),
  {Warnings, FunTypes} = 
    dialyzer_dataflow:get_warnings(Tree, Plt, Callgraph, Records, NoWarnUnused),
  if DocPlt =:= undefined -> ok;
     true -> insert_into_plt(FunTypes, Callgraph, DocPlt)
  end,
  get_warnings(Left, Callgraph, Plt, DocPlt, Codeserver, 
	       NoWarnUnused, [Warnings|Acc]);
get_warnings([], _Callgraph, _Plt, _DocPlt, _Codeserver, _NoWarnUnused, Acc) ->
  lists:flatten(Acc).

refine_succ_typings(ModulePostorder, Callgraph, Codeserver, StoreAll, Plt) ->
  ?debug("Module postorder: ~p\n", [ModulePostorder]),
  refine_succ_typings(ModulePostorder, Callgraph, 
		      Codeserver, Plt, StoreAll, []).

refine_succ_typings([[M]|Left], Callgraph, Codeserver, Plt, StoreAll, 
		    Fixpoint) ->
  ?debug("Dataflow of one module: ~w\n", [M]),
  {ok, Tree} = dialyzer_codeserver:lookup(M, core, Codeserver),
  AllFuns = traverse_tree_list([Tree]),
  FunTypes = get_fun_types_from_plt(AllFuns, Callgraph, Plt),
  Records = dialyzer_codeserver:lookup_records(M, Codeserver),
  NewFunTypes = dialyzer_dataflow:get_fun_types(Tree, Plt, Callgraph, Records),
						
  FP = 
    case reached_fixpoint(FunTypes, NewFunTypes) of
      true -> Fixpoint;
      {false, NotFixpoint} ->
	?debug("Not fixpoint\n", []),
	NotFixpoint1 = [Fun || {Fun, _Type} <- NotFixpoint],
	case StoreAll of
	  true ->
	    insert_into_plt(NewFunTypes, Callgraph, Plt);
	  false ->
	    insert_into_plt(dict:from_list(NotFixpoint), Callgraph, Plt)
	end,
	ordsets:union(ordsets:from_list(NotFixpoint1), Fixpoint)
    end,
  refine_succ_typings(Left, Callgraph, Codeserver, Plt, StoreAll, FP);
refine_succ_typings(Mods = [SCC|Left], Callgraph, Codeserver, 
		    Plt, StoreAll, Fixpoint) ->
  ?debug("Dataflow of one SCC: ~w\n", [SCC]),
  AnsList = [refine_succ_typings([[M]], Callgraph, Codeserver, 
				 Plt, StoreAll, [])
	     || M <- SCC],
  case lists:flatten([Set || {not_fixpoint, Set} <- AnsList]) of
    [] -> 
      refine_succ_typings(Left, Callgraph, Codeserver, Plt, StoreAll, Fixpoint);
    NotFixpoint ->
      NewFixpoint = ordsets:union(ordsets:from_list(NotFixpoint), Fixpoint),
      refine_succ_typings(Mods, Callgraph, Codeserver, Plt, 
			  StoreAll, NewFixpoint)
  end;
refine_succ_typings([], _Callgraph, _Codeserver, _Plt, _StoreAll, Fixpoint) ->
  case Fixpoint of
    [] -> fixpoint;
    List -> {not_fixpoint, List}
  end.

reached_fixpoint(OldTypes, NewTypes) ->
  reached_fixpoint(OldTypes, NewTypes, false).

reached_fixpoint_strict(OldTypes, NewTypes) ->
  reached_fixpoint(OldTypes, NewTypes, true).

reached_fixpoint(OldTypes0, NewTypes0, Strict) ->
  MapFun = fun(_Key, Type) -> 	      
	       case is_failed_or_not_called_fun(Type) of
		 true -> failed_fun;
		 false -> erl_types:t_limit(Type, ?TYPE_LIMIT)
	       end
	   end,
  OldTypes = dict:map(MapFun, OldTypes0),
  NewTypes = dict:map(MapFun, NewTypes0),
  compare_types(OldTypes, NewTypes, Strict).

is_failed_or_not_called_fun(Type) ->
  any_none([erl_types:t_fun_range(Type)|erl_types:t_fun_args(Type)]).

any_none([T|Ts]) ->
  case erl_types:t_is_none(T) of
    true -> true;
    false -> any_none(Ts)
  end;
any_none([]) ->
  false.
      
compare_types(Dict1, Dict2, Strict) ->  
  List1 = lists:keysort(1, dict:to_list(Dict1)),
  List2 = lists:keysort(1, dict:to_list(Dict2)),
  compare_types_1(List1, List2, Strict, []).

compare_types_1([{X, _Type1}|Left1], [{X, failed_fun}|Left2], 
		Strict, NotFixpoint) ->
  compare_types_1(Left1, Left2, Strict, NotFixpoint);
compare_types_1([{X, failed_fun}|Left1], [{X, _Type2}|Left2], 
		Strict, NotFixpoint) ->
  compare_types_1(Left1, Left2, Strict, NotFixpoint);
compare_types_1([{X, Type1}|Left1], [{X, Type2}|Left2], false, NotFixpoint) ->
  case erl_types:t_is_subtype(Type1, Type2) of
    true -> compare_types_1(Left1, Left2, false, NotFixpoint);
    false -> 
      ?debug("Failed fixpoint for ~w: ~s =/= ~s\n",
	     [X, erl_types:t_to_string(Type1), 
	      erl_types:t_to_string(Type2)]),
      compare_types_1(Left1, Left2, false, [{X, Type2}|NotFixpoint])
  end;
compare_types_1([{X, Type1}|Left1], [{X, Type2}|Left2], true, NotFixpoint) ->
  case erl_types:t_is_equal(Type1, Type2) of
    true -> compare_types_1(Left1, Left2, true, NotFixpoint);
    false -> 
      ?debug("Failed fixpoint for ~w: ~s =/= ~s\n",
	     [X, erl_types:t_to_string(Type1), 
	      erl_types:t_to_string(Type2)]),
      compare_types_1(Left1, Left2, true, [{X, Type2}|NotFixpoint])
  end;
compare_types_1([_|Left1], List2, Strict, NotFixpoint) ->
  %% If the function was not called.
  compare_types_1(Left1, List2, Strict, NotFixpoint);
compare_types_1([], [], _Strict, []) ->
  true;
compare_types_1([], [], _Strict, NotFixpoint) ->
  {false, NotFixpoint}.

find_succ_typings(Callgraph, Codeserver, Plt) ->
  find_succ_typings(Callgraph, Codeserver, Plt, []).

find_succ_typings(Callgraph, Codeserver, Plt, NotFixpoint) ->
  case dialyzer_callgraph:take_scc(Callgraph) of
    {ok, SCC, NewCallgraph} ->
      ?debug("Taking: ~w\n", [SCC]),      
      NewNotFixpoint1 = analyze_scc(SCC, Callgraph, Codeserver, Plt),
      NewNotFixpoint2 = ordsets:union(NewNotFixpoint1, NotFixpoint),
      find_succ_typings(NewCallgraph, Codeserver, Plt, NewNotFixpoint2);
    none ->
      ?debug("Done\n\n", []),
      case NotFixpoint =:= [] of
	true -> fixpoint;
	false -> {not_fixpoint, Callgraph, NotFixpoint}
      end
  end.

analyze_scc(SCC, Callgraph, Codeserver, Plt) ->
  NextLabel = dialyzer_codeserver:next_core_label(Codeserver),
  SCC1 = [{MFA, 
	   dialyzer_codeserver:lookup(MFA, core, Codeserver),
	   dialyzer_codeserver:lookup_records(M, Codeserver)}
	  || MFA = {M,_,_} <- SCC],
  false = lists:any(fun({_, X, _}) -> X =:= error end, SCC1),
  SCC2 = [{MFA, Def, Rec} || {MFA, {ok, Def}, Rec} <- SCC1],  
  {SuccTypes, NotFixpoint} = 
    find_succ_types_for_scc(SCC2, NextLabel, Callgraph, Plt),
  insert_into_plt(SuccTypes, Callgraph, Plt),
  NotFixpoint.

find_succ_types_for_scc(SCC, NextLabel, Callgraph, Plt) ->  
  %% Assume that the Plt contains the current propagated types.
  AllFuns = traverse_tree_list([Fun || {_MFA, {_Var, Fun}, _Rec} <- SCC]),
  PropTypes = get_fun_types_from_plt(AllFuns, Callgraph, Plt),
  FunTypes = dialyzer_typesig:analyze_scc_get_all_fun_types(SCC, NextLabel, 
							    Callgraph, Plt, 
							    PropTypes),  
  AllFunSet = sets:from_list([X || {X, _} <- AllFuns]),
  FilteredFunTypes = dict:filter(fun(X, _) -> 
				     sets:is_element(X, AllFunSet) 
				 end, FunTypes),
  case reached_fixpoint_strict(PropTypes, FilteredFunTypes) of
    true -> {FilteredFunTypes, []};
    {false,_} -> 
      ?debug("Not fixpoint for: ~w\n", [AllFuns]),
      %% Might as well add all functions since this is a scc.
      {FilteredFunTypes, ordsets:from_list([Fun || {Fun, _Arity} <- AllFuns])}
  end.

get_fun_types_from_plt(FunList, Callgraph, Plt) ->
  get_fun_types_from_plt(FunList, Callgraph, Plt, dict:new()).

get_fun_types_from_plt([{FunLabel, Arity}|Left], Callgraph, Plt, Map) ->
  Type = lookup_fun_type(FunLabel, Arity, Callgraph, Plt),
  get_fun_types_from_plt(Left, Callgraph, Plt, dict:store(FunLabel, Type, Map));
get_fun_types_from_plt([], _Callgraph, _Plt, Map) ->
  Map.

traverse_tree_list(Trees) ->
  traverse_tree_list(Trees, []).

traverse_tree_list([Tree|Left], List) ->
  FoldFun = fun(SubTree, Acc) ->
		case cerl:is_c_fun(SubTree) of
		  true -> [{cerl_trees:get_label(SubTree), 
			    cerl:fun_arity(SubTree)}|Acc];
		  false -> Acc
		end
	    end,
  traverse_tree_list(Left, cerl_trees:fold(FoldFun, List, Tree));
traverse_tree_list([], List) ->
  List.

lookup_fun_type(Label, Arity, Callgraph, Plt) ->  
  ID = lookup_name(Label, Callgraph),
  case dialyzer_plt:lookup(Plt, ID) of
    none -> erl_types:t_fun(Arity, erl_types:t_any());
    {value, {RetT, ArgT}} -> erl_types:t_fun(ArgT, RetT)
  end.

insert_into_plt(SuccTypes0, Callgraph, Plt) ->
  SuccTypes = format_succ_types(SuccTypes0, Callgraph),
  debug_pp_succ_typings(SuccTypes),
  dialyzer_plt:insert(Plt, SuccTypes).

format_succ_types(SuccTypes, Callgraph) ->
  format_succ_types(dict:to_list(SuccTypes), Callgraph, []).

format_succ_types([{Label, Type0}|Left], Callgraph, Acc) ->
  Type = erl_types:t_limit(Type0, ?TYPE_LIMIT+1),
  Id = lookup_name(Label, Callgraph),
  NewTuple = {Id, erl_types:t_fun_range(Type), erl_types:t_fun_args(Type)},
  format_succ_types(Left, Callgraph, [NewTuple|Acc]);
format_succ_types([], _Callgraph, Acc) ->
  Acc.

-ifdef(DEBUG).
debug_pp_succ_typings(SuccTypes) ->
  ?debug("Succ typings:\n", []),
  [?debug("\t~w\t~s\n", 
	  [MFA, erl_types:t_to_string(erl_types:t_fun(ArgT, RetT))])
   ||{MFA, RetT, ArgT} <- SuccTypes],
  ok.
-else.
debug_pp_succ_typings(_) ->  
  ok.
-endif.

lookup_name(F, CG) ->
  case dialyzer_callgraph:lookup_name(F, CG) of
    error -> F;
    {ok, Name} -> Name
  end.

%%% ============================================================================
%%%
%%%  Debug interface.
%%%
%%% ============================================================================

doit(Module) ->
  AbstrCode = dialyzer_utils:get_abstract_code_from_src(Module, [no_copt]),
  Code = dialyzer_utils:get_core_from_abstract_code(AbstrCode, [no_copt]),
  {ok, Records} = dialyzer_utils:get_record_info(AbstrCode),
  Sigs0 = get_top_level_signatures(Code, Records),
  M = 
    if is_atom(Module) ->  
	list_to_atom(filename:basename(atom_to_list(Module)));
       is_list(Module) -> 
	list_to_atom(filename:basename(Module))
    end,
  Sigs1 = [{{M, F, A}, Type} || {{F, A}, Type} <- Sigs0],
  Sigs = ordsets:from_list(Sigs1),
  io:nl(),
  pp_signatures(Sigs, Records),
  ok.

get_top_level_signatures(Code, Records) ->
  Tree = cerl:from_records(Code),
  {LabeledTree, NextLabel} = cerl_trees:label(Tree),
  Plt = get_def_plt(),
  dialyzer_plt:delete_module(Plt, cerl:atom_val(cerl:module_name(LabeledTree))),
  analyze_module(LabeledTree, NextLabel, Plt, Records),
  M = cerl:concrete(cerl:module_name(Tree)),
  Functions = [{M, cerl:fname_id(V), cerl:fname_arity(V)} 
	       || {V, _F} <- cerl:module_defs(LabeledTree)],
  Types = [{MFA, dialyzer_plt:lookup(Plt, MFA)} || MFA <- Functions],
  dialyzer_plt:delete(Plt),
  ordsets:from_list([{{F, A}, erl_types:t_fun(ArgT, RetT)} 
		     || {{_M, F, A}, {value, {RetT, ArgT}}} <- Types]).

get_def_plt() ->
  try 
    dialyzer_plt:from_file(dialyzer_typesig_plt, 
			   filename:join([code:lib_dir(dialyzer),
					  "plt","dialyzer_init_plt"]))
  catch
    error:no_such_file -> ets:new(dialyzer_typesig_plt, []);
    throw:{dialyzer_error, _} -> ets:new(dialyzer_typesig_plt, [])
  end.

pp_signatures([{{_, module_info, 0}, _}|Left], Records) -> 
  pp_signatures(Left, Records);
pp_signatures([{{_, module_info, 1}, _}|Left], Records) -> 
  pp_signatures(Left, Records);
pp_signatures([{{M, F, A}, Type}|Left], Records) ->
  TypeString =
    case cerl:is_literal(Type) of
      true -> io_lib:format("~w", [cerl:concrete(Type)]);
      false -> erl_types:t_to_string(Type, Records)
    end,
  io:format("~w:~w/~w :: ~s\n", [M, F, A, TypeString]),
  pp_signatures(Left, Records);
pp_signatures([], _Records) ->
  ok.
      

-ifdef(DEBUG_PP).
debug_pp(Tree, Map) -> 
  Tree1 = strip_annotations(Tree),
  io:put_chars(cerl_prettypr:format(Tree1)),
  io:nl().  

strip_annotations(Tree) ->
  cerl_trees:map(fun(T) ->
		     case cerl:is_literal(T) orelse cerl:is_c_values(T) of
		       true -> cerl:set_ann(T, []);
		       false ->
			 Label = cerl_trees:get_label(T),
			 cerl:set_ann(T, [{'label', Label}])
		     end
		 end, Tree).
			    
-else.
debug_pp(_Tree, _Map) -> 
  ok.
-endif.


%%% --------------------------------------------------------
%%% Analysis of one module.
%%%

analyze_module(LabeledTree, NextLabel, Plt, Records) ->
  debug_pp(LabeledTree, dict:new()),
  Callgraph1 = dialyzer_callgraph:new(),
  Callgraph2 = dialyzer_callgraph:scan_core_tree(LabeledTree, Callgraph1),
  {Callgraph3, _Ext} = dialyzer_callgraph:remove_external(Callgraph2),
  Callgraph4 = dialyzer_callgraph:finalize(Callgraph3),
  Codeserver1 = dialyzer_codeserver:new(),
  ModuleName = cerl:concrete(cerl:module_name(LabeledTree)),
  Insert = [{ModuleName, LabeledTree}],
  Codeserver2 = dialyzer_codeserver:insert(Insert, core, Codeserver1),
  Codeserver3 = 
    dialyzer_codeserver:update_next_core_label(NextLabel, Codeserver2),
  Codeserver4 = 
    dialyzer_codeserver:store_records(ModuleName, Records, Codeserver3),
  Res = analyze_callgraph(Callgraph4, Plt, Codeserver4),
  dialyzer_callgraph:delete(Callgraph4),
  dialyzer_codeserver:delete(Codeserver4),
  Res.
