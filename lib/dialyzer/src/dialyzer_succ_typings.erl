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
%%% File    : dialyzer_succ_typings.erl
%%% Author  : Tobias Lindahl <tobiasl@csd.uu.se>
%%% Description : 
%%%
%%% Created : 11 Sep 2006 by Tobias Lindahl <tobiasl@csd.uu.se>
%%%-------------------------------------------------------------------
-module(dialyzer_succ_typings).

-export([analyze_callgraph/3, analyze_callgraph/4,
	 get_warnings/7]).

%% These are only intended as debug functions.
-export([doit/1,
	 analyze_callgraph_only_typesig/3,
	 analyze_callgraph_only_dataflow/3,
	 get_top_level_signatures/2,
	 get_top_level_signatures/3]).

-define(TYPE_LIMIT, 4).

%-define(DEBUG, true).
%-define(DEBUG_PP, true).

-ifdef(DEBUG).
-define(debug(X__, Y__), io:format(X__, Y__)).
-else.
-define(debug(X__, Y__), ok).
-endif.

-record(state, {callgraph,
		codeserver,
		no_warn_unused,
		parent=none :: none | pid(),
		plt}).

analyze_callgraph(Callgraph, Plt, Codeserver) ->
  analyze_callgraph(Callgraph, Plt, Codeserver, none).

analyze_callgraph(Callgraph, Plt, Codeserver, Parent) ->
  State = #state{callgraph=Callgraph, plt=Plt, 
		 codeserver=Codeserver, parent=Parent},
  NewState = get_refined_success_typings(State),
  NewState#state.plt.

analyze_callgraph_only_dataflow(Callgraph, Plt, Codeserver) ->
  ModulePostorder = dialyzer_callgraph:module_postorder(Callgraph),
  %% When analyzing only with dataflow, the fixpoint iteration is unnecessary.
  State = #state{callgraph=Callgraph, plt=Plt, codeserver=Codeserver},
  case refine_succ_typings(ModulePostorder, State, true) of
    {fixpoint, #state{plt=Plt1}} -> Plt1;
    {not_fixpoint, _, #state{plt=Plt1}} -> Plt1
  end.

analyze_callgraph_only_typesig(Callgraph, Plt, Codeserver) ->
  State = #state{callgraph=Callgraph, plt=Plt, codeserver=Codeserver},
  %% When analyzing only with typesig, the fixpoint iteration is unnecessary.
  case find_succ_typings(State) of
    {fixpoint, #state{plt=Plt1}} -> Plt1;
    {not_fixpoint, _, #state{plt=Plt1}} -> Plt1
  end.

get_refined_success_typings(State) ->
  case find_succ_typings(State) of
    {fixpoint, State1} -> State1;
    {not_fixpoint, NotFixpoint1, State1} ->
      Callgraph = State1#state.callgraph,
      NotFixpoint2 = [lookup_name(F, Callgraph) || F <- NotFixpoint1],
      ModulePostorder = 
	dialyzer_callgraph:module_postorder_from_funs(NotFixpoint2, Callgraph),
      case refine_succ_typings(ModulePostorder, State1, false) of
	{fixpoint, State2} ->
	  State2;
	{not_fixpoint, NotFixpoint3, State2} ->
	  Callgraph1 = State2#state.callgraph,
	  %% Need to reset the callgraph.
	  NotFixpoint4 = [lookup_name(F, Callgraph1) || F <- NotFixpoint3],
	  Callgraph2 = dialyzer_callgraph:reset_from_funs(NotFixpoint4, 
							  Callgraph1),
	  State3 = State2#state{callgraph=Callgraph2},
	  get_refined_success_typings(State3)
      end
  end.

get_warnings(Callgraph, Plt, DocPlt, Codeserver, NoWarnUnused, AnalysisType,
	     Parent) ->
  State = #state{callgraph=Callgraph, plt=Plt,
		 no_warn_unused=NoWarnUnused,
		 codeserver=Codeserver, parent=Parent},
  {Warnings, State1, NewDocPlt} =
    case AnalysisType of
      dataflow ->
	Modules = dialyzer_callgraph:module_postorder(Callgraph),
	get_warnings(Modules, State, DocPlt, true, []);
      succ_typings ->
	NewState = get_refined_success_typings(State),
	NewPlt = NewState#state.plt,
	Modules = dialyzer_callgraph:modules(NewState#state.callgraph),
	CWarnings = dialyzer_contracts:get_invalid_contract_warnings(Modules, 
								     Codeserver,
								     NewPlt),
	WrappedModules = [[M] || M <- Modules],
	get_warnings(WrappedModules, NewState, DocPlt, false, CWarnings)
    end,
  {Warnings, State1#state.plt, NewDocPlt}.

get_warnings([[M]|Left], State, DocPlt, Store, Acc) ->
  send_log(State#state.parent, io_lib:format("Getting warnings for ~w\n", [M])),
  %% We can get the warnings straight away.
  Codeserver = State#state.codeserver,
  {ok, Tree} = dialyzer_codeserver:lookup(M, core, Codeserver),
  Records = dialyzer_codeserver:lookup_records(M, Codeserver),
  Contracts = dialyzer_codeserver:lookup_contracts(M, Codeserver),
  AllFuns0 = traverse_tree_list([Tree]),
  %% Check if there are contracts for functions that does not exist
  Warnings1 = 
    dialyzer_contracts:contracts_without_fun(Contracts, AllFuns0, 
					     State#state.callgraph),
  #state{plt=Plt, callgraph=Callgraph, no_warn_unused=NoWarnUnused} = State,
  {Warnings2, FunTypes} = dialyzer_dataflow:get_warnings(Tree, Plt, Callgraph, 
							 Records, NoWarnUnused),
  NewDocPlt = insert_into_doc_plt(FunTypes, Callgraph, DocPlt),
  NewState =
    if Store -> insert_into_plt(FunTypes, State);
       true -> State
    end,
  NewAcc = [Warnings1, Warnings2|Acc],
  get_warnings(Left, NewState, NewDocPlt, Store, NewAcc);
get_warnings([SCC|Left], State, DocPlt, Store, Acc) ->
  %% In a scc we must first analyze the modules until fixpoint, and
  %% then get the warnings. Note that the "fixpoint" in the code means
  %% if something changed, not if it was analysed to fixpoint. A SCC
  %% is always analyzed to a fixpoint.
  State1 =
    case refine_succ_typings([SCC], State, true) of
      {fixpoint, S} -> S;
      {not_fixpoint, _, S} -> S
    end,
  Mods = [[M] || M <- SCC],
  {NewAcc, State2, DocPlt1} = get_warnings(Mods, State1, DocPlt, Store, Acc),
  get_warnings(Left, State2, DocPlt1, Store, NewAcc);
get_warnings([], State, DocPlt, _Store, Acc) ->
  {lists:flatten(Acc), State, DocPlt}.

refine_succ_typings(ModulePostorder, State, StoreAll) ->
  ?debug("Module postorder: ~p\n", [ModulePostorder]),
  refine_succ_typings(ModulePostorder, State, StoreAll, []).

refine_succ_typings([[M]|Left], State, StoreAll, Fixpoint) ->
  Msg = io_lib:format("Dataflow analysis of module: ~w\n", [M]),
  send_log(State#state.parent, Msg),
  ?debug("~s\n", [Msg]),
  {ok, Tree} = dialyzer_codeserver:lookup(M, core, State#state.codeserver),
  AllFuns = traverse_tree_list([Tree]),
  FunTypes = get_fun_types_from_plt(AllFuns, State),
  Records = dialyzer_codeserver:lookup_records(M, State#state.codeserver),
  NewFunTypes = dialyzer_dataflow:get_fun_types(Tree, State#state.plt, 
						State#state.callgraph, Records),
  {State1, FP} = 
    case reached_fixpoint(FunTypes, NewFunTypes) of
      true -> {State, Fixpoint};
      {false, NotFixpoint} ->
	?debug("Not fixpoint\n", []),
	NotFixpoint1 = [Fun || {Fun, _Type} <- NotFixpoint],
	FP0 = ordsets:union(ordsets:from_list(NotFixpoint1), Fixpoint),
	case StoreAll of
	  true -> {insert_into_plt(NewFunTypes, State), FP0};
	  false -> {insert_into_plt(dict:from_list(NotFixpoint), State), FP0}
	end
    end,
  refine_succ_typings(Left, State1, StoreAll, FP);
refine_succ_typings(Mods = [SCC|Left], State, StoreAll, Fixpoint) ->
  Msg = io_lib:format("Dataflow of one SCC: ~w\n", [SCC]),
  send_log(State#state.parent, Msg),
  ?debug("~s\n", [Msg]),
  {NewState, NotFixpoint} =
    lists:foldl(fun(M, {AccState, AccFixpoint}) ->
		    case refine_succ_typings([[M]], AccState, StoreAll, []) of
		      {fixpoint, NewAccState} -> 
			{NewAccState, AccFixpoint};
		      {not_fixpoint, List, NewAccState} -> 
			{NewAccState, [List|AccFixpoint]}
		    end
		end, {State, []}, SCC),
  case NotFixpoint =:= [] of
    true -> 
      refine_succ_typings(Left, NewState, StoreAll, Fixpoint);
    false ->
      NotFixpoint1 = lists:flatten(NotFixpoint),
      NewFixpoint = ordsets:union(ordsets:from_list(NotFixpoint1), Fixpoint),
      refine_succ_typings(Mods, NewState, StoreAll, NewFixpoint)
  end;
refine_succ_typings([], State, _StoreAll, Fixpoint) ->
  case Fixpoint =:= [] of
    true -> {fixpoint, State};
    false -> {not_fixpoint, Fixpoint, State}
  end.

reached_fixpoint(OldTypes, NewTypes) ->
  reached_fixpoint(OldTypes, NewTypes, false).

reached_fixpoint_strict(OldTypes, NewTypes) ->
  case reached_fixpoint(OldTypes, NewTypes, true) of
    true -> true;
    {false, _} -> false
  end.

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

find_succ_typings(State) ->
  find_succ_typings(State, []).

find_succ_typings(State, NotFixpoint) ->
  case dialyzer_callgraph:take_scc(State#state.callgraph) of
    {ok, SCC, NewCallgraph} ->
      Msg = io_lib:format("Typesig analysis for scc: ~w\n", [format_scc(SCC)]),
      ?debug("~s\n", [Msg]),
      send_log(State#state.parent, Msg),
      {NewState, NewNotFixpoint1} = 
	analyze_scc(SCC, State#state{callgraph=NewCallgraph}),
      NewNotFixpoint2 = ordsets:union(NewNotFixpoint1, NotFixpoint),
      find_succ_typings(NewState, NewNotFixpoint2);
    none ->
      ?debug("Done\n\n", []),
      case NotFixpoint =:= [] of
	true -> {fixpoint, State};
	false -> {not_fixpoint, NotFixpoint, State}
      end
  end.

analyze_scc(SCC, State = #state{codeserver=Codeserver}) ->
  SCC1 = [{MFA, 
	   dialyzer_codeserver:lookup(MFA, core, Codeserver),
	   dialyzer_codeserver:lookup_records(M, Codeserver)}
	  || MFA = {M,_,_} <- SCC],
  false = lists:any(fun({_, X, _}) -> X =:= error end, SCC1),
  SCC2 = [{MFA, Def, Rec} || {MFA, {ok, Def}, Rec} <- SCC1],
  Contracts1 = [{MFA, dialyzer_codeserver:lookup_contract(MFA, Codeserver)}
		||  MFA = {_, _, _} <- SCC],
  Contracts2 = [{MFA, Contract} || {MFA, {ok, Contract}} <- Contracts1],
  Contracts3 = orddict:from_list(Contracts2),
  {SuccTypes, PltContracts, NotFixpoint} = 
    find_succ_types_for_scc(SCC2, Contracts3, State),
  State1 = insert_into_plt(SuccTypes, State),
  ContrPlt = dialyzer_plt:insert_contracts(State1#state.plt, PltContracts),
  {State1#state{plt=ContrPlt}, NotFixpoint}.

find_succ_types_for_scc(SCC, Contracts, 
			State = #state{codeserver=Codeserver, 
				       callgraph=Callgraph,
				       plt=Plt}) ->
  %% Assume that the Plt contains the current propagated types.
  AllFuns = traverse_tree_list([Fun || {_MFA, {_Var, Fun}, _Rec} <- SCC]),
  PropTypes = get_fun_types_from_plt(AllFuns, State),
  MFAs = [MFA || {MFA, {_Var, _Fun}, _Rec} <- SCC],
  NextLabel = dialyzer_codeserver:next_core_label(Codeserver),
  Plt1 = dialyzer_plt:delete_contract_list(State#state.plt, MFAs),
  FunTypes = dialyzer_typesig:analyze_scc_get_all_fun_types(SCC, NextLabel, 
							    Callgraph, Plt1, 
							    PropTypes),
  AllFunSet = sets:from_list([X || {X, _} <- AllFuns]),
  FilteredFunTypes = dict:filter(fun(X, _) -> 
				     sets:is_element(X, AllFunSet) 
				 end, FunTypes),
  %% Check contracts
  PltContracts = dialyzer_contracts:check_contracts(Contracts, Callgraph, 
						    FilteredFunTypes),
  ContractFixpoint =
    not lists:any(fun({MFA, _C}) ->
		      %% Check the non-deleted plt
		      case dialyzer_plt:lookup_contract(Plt, MFA) of
			none -> true;
			{value, _} -> false
		      end
		  end, PltContracts),
  case (ContractFixpoint andalso 
	reached_fixpoint_strict(PropTypes, FilteredFunTypes)) of
    true ->
      {FilteredFunTypes, PltContracts, []};
    false -> 
      ?debug("Not fixpoint for: ~w\n", [AllFuns]),
      {FilteredFunTypes, PltContracts, 
       ordsets:from_list([Fun || {Fun, _Arity} <- AllFuns])}
  end.

get_fun_types_from_plt(FunList, State) ->
  get_fun_types_from_plt(FunList, State, dict:new()).

get_fun_types_from_plt([{FunLabel, Arity}|Left], State, Map) ->
  Type = lookup_fun_type(FunLabel, Arity, State),
  get_fun_types_from_plt(Left, State, dict:store(FunLabel, Type, Map));
get_fun_types_from_plt([], _State, Map) ->
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

lookup_fun_type(Label, Arity, #state{callgraph=Callgraph, plt=Plt}) ->  
  ID = lookup_name(Label, Callgraph),
  case dialyzer_plt:lookup(Plt, ID) of
    none -> erl_types:t_fun(Arity, erl_types:t_any());
    {value, {RetT, ArgT}} -> erl_types:t_fun(ArgT, RetT)
  end.

insert_into_doc_plt(_FunTypes, _Callgraph, undefined) ->
  undefined;
insert_into_doc_plt(FunTypes, Callgraph, DocPlt) ->
  SuccTypes = format_succ_types(FunTypes, Callgraph),
  dialyzer_plt:insert(DocPlt, SuccTypes).

insert_into_plt(SuccTypes0, State = #state{callgraph=Callgraph, plt=Plt}) ->
  SuccTypes = format_succ_types(SuccTypes0, Callgraph),
  debug_pp_succ_typings(SuccTypes),
  State#state{plt=dialyzer_plt:insert(Plt, SuccTypes)}.

format_succ_types(SuccTypes, Callgraph) ->
  format_succ_types(dict:to_list(SuccTypes), Callgraph, []).

format_succ_types([{Label, {contract, RetFun, Args}}|Left], Callgraph, Acc) ->
  %Type = erl_types:t_limit(Type0, ?TYPE_LIMIT+1),
  Id = lookup_name(Label, Callgraph),
  NewTuple = {Id, {contract, RetFun, Args}},
  format_succ_types(Left, Callgraph, [NewTuple|Acc]);
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
  [?debug("\t~w\t~s\n", 
	  [MFA, erl_types:t_to_string(erl_types:t_fun(ArgT, RetFun(ArgT)))])
   ||{MFA, {contract, RetFun, ArgT}} <- SuccTypes],
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

send_log(none, _Msg) ->
  ok;
send_log(Parent, Msg) ->
  Parent ! {self(), log, lists:flatten(Msg)}.

format_scc(SCC) ->
  [X || X = {_M, _F, _A} <- SCC].

%%% ============================================================================
%%%
%%%  Debug interface.
%%%
%%% ============================================================================

doit(Module) ->
  AbstrCode = dialyzer_utils:get_abstract_code_from_src(Module, 
							[no_copt,
							 {i,"../include"}]),
					      
  Code = dialyzer_utils:get_core_from_abstract_code(AbstrCode, [no_copt]),
  {ok, Records} = dialyzer_utils:get_record_and_type_info(AbstrCode),
  %% contract typing info in dictionary format
  {ok, Contracts} = dialyzer_utils:get_spec_info(AbstrCode, Records),
  Sigs0 = get_top_level_signatures(Code, Records, Contracts),
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
  get_top_level_signatures(Code, Records, dict:new()).

get_top_level_signatures(Code, Records, Contracts) ->
  %% TODO: Should include contracts
  Tree = cerl:from_records(Code),
  {LabeledTree, NextLabel} = cerl_trees:label(Tree),
  Plt = get_def_plt(),
  ModuleName = cerl:atom_val(cerl:module_name(LabeledTree)),
  Plt1 = dialyzer_plt:delete_module(Plt, ModuleName),
  Plt2 = analyze_module(LabeledTree, NextLabel, Plt1, Records, Contracts),
  M = cerl:concrete(cerl:module_name(Tree)),
  Functions = [{M, cerl:fname_id(V), cerl:fname_arity(V)} 
	       || {V, _F} <- cerl:module_defs(LabeledTree)],
  %% First contracts check
  AllContracts = dict:fetch_keys(Contracts),
  ErrorContracts = lists:subtract(AllContracts, Functions),  
  lists:map(fun(C) -> 
	       io:format("Contract for function that does not exist: ~w\n",[C])
	    end, ErrorContracts),

  Types = [{MFA, dialyzer_plt:lookup(Plt2, MFA)} || MFA <- Functions],
  ordsets:from_list([{{F, A}, erl_types:t_fun(ArgT, RetT)} 
		     || {{_M, F, A}, {value, {RetT, ArgT}}} <- Types]),
    Sigs = [{{F, A}, erl_types:t_fun(ArgT, RetT)} 
	  || {{_M, F, A}, {value, {RetT, ArgT}}} <- Types],
  ordsets:from_list(Sigs).

get_def_plt() ->
  try 
    dialyzer_plt:from_file(filename:join([code:lib_dir(dialyzer),
					  "plt","dialyzer_init_plt"]))
  catch
    error:no_such_file -> dialyzer_plt:new();
    throw:{dialyzer_error, _} -> dialyzer_plt:new()
  end.

pp_signatures([{{_, module_info, 0}, _}|Left], Records) -> 
  pp_signatures(Left, Records);
pp_signatures([{{_, module_info, 1}, _}|Left], Records) -> 
  pp_signatures(Left, Records);
pp_signatures([{{M, F, A}, Type}|Left], Records) ->
  TypeString =
    case cerl:is_literal(Type) of
      true -> io_lib:format("~w", [cerl:concrete(Type)]);
      false -> "fun" ++ String = erl_types:t_to_string(Type, Records),
	       String
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

analyze_module(LabeledTree, NextLabel, Plt, Records, Contracts) ->
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
  Codeserver5 =  
    dialyzer_codeserver:store_contracts(ModuleName, Contracts, Codeserver4),
  Res = analyze_callgraph(Callgraph4, Plt, Codeserver5),
  dialyzer_callgraph:delete(Callgraph4),
  dialyzer_codeserver:delete(Codeserver5),
  Res.


