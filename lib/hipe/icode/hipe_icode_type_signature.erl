%%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : hipe_icode_type_signature.erl
%%% Author  : Tobias Lindahl <tobiasl@csd.uu.se>
%%% Description : A constraint based type analysis to find a functions
%%%              type signature.
%%%
%%% Created :  6 Sep 2004 by Tobias Lindahl <tobiasl@csd.uu.se>
%%%-------------------------------------------------------------------
-module(hipe_icode_type_signature).

-export([cfg/2]).

-import(erl_types, [t_any/0, t_atom/1, t_atom/0, t_atom_vals/1, 
		    t_binary/0, t_bool/0, t_cons/0, t_cons/2, 
		    t_improper_list/0, t_float/0, 
		    t_from_term/1, t_fun/0, t_fun/2, t_fun_args/1, 
		    t_identifier/0, t_inf/2, t_inf/1, t_inf_lists/2,
		    t_integer/0, t_integer/1,
		    t_is_atom/1, t_is_any/1, t_is_binary/1, t_is_bool/1,
		    t_is_char/1, t_is_cons/1, 
		    t_is_improper_list/1, t_is_equal/2, t_is_float/1, 
		    t_is_fun/1, t_is_integer/1, t_is_number/1,
		    t_is_list/1, t_is_nil/1, t_is_nonempty_list/1,
		    t_is_port/1, t_is_pid/1, t_is_ref/1, 
		    t_is_subtype/2, t_is_tuple/1,
		    t_is_none/1, t_limit/2, t_list/0, t_list/1, 
		    t_nil/0, t_nonempty_list/0,
		    t_number/0, t_number/1, t_number_vals/1,
		    t_pid/0, t_port/0, t_ref/0, t_string/0, t_subtract/2, 
		    t_sup/1, t_sup/2,
		    t_var/1, t_is_var/1,
		    t_to_string/1,
		    t_tuple/0, t_tuple/1, t_tuple_arity/1, t_tuple_arities/1,
		    t_none/0]).



-record(subtype_constraint, {type1, type2}).
-record(eq_constraint, {type1, type2}).
-record(call, {'fun', args}).
-record(subtract, {type1, type2}).
-record(sup, {types}).
-record(inf, {types}).
-record(cons, {head, tail}).
-record(list, {contents}).

%%-define(DEBUG, true).

-define(TYPE_DEPTH, 5).
-define(RETURN_VAR, t_var(0)).
-define(FIRST_ARGVAR, 1).

-ifdef(DEBUG).
debug(S, A) ->
  io:format(S, A).
-else.
debug(_S, _A) ->
  ok.
-endif.

cfg(Cfg, _Options) ->
  State = traverse_cfg(new_state(Cfg)),
  %%hipe_icode_cfg:pp(state__cfg(State)),
  case solve_constraints(State) of
%    ok -> ok;
    {ok, Map} ->
      pp(State, Map),
      ok
%    {error, Var, LH, RH, Map} ->
%      io:format("+++++ Failing constraint: ~s (~s) </: ~s\n", 
%		[format(Var), 
%		 format(evaluate_type(LH, Map)), 
%		 format(evaluate_type(RH, Map))]),
		 
%      pp(State, Map),
%      ok
  end.
      


%% _________________________________________________________________
%%
%% Annotate all variables in the cfg with type variables.
%% Make constraints based on each instruction.
%%

traverse_cfg(State) ->
  Labels = state__labels(State),
  %% Add proper annotations for the arguments
  NewState1 = annotate_state_args(State),
  %% First handle each block in isolation.
  NewState2 = traverse_bb(Labels, NewState1),
  %%pp(NewState2, gb_trees:empty()),
  %% Add constraints for each edge to glue the basic blocks together.
  {NewState3, EdgeMap} = handle_edges_out(Labels, gb_trees:empty(), NewState2),
  handle_edges_in(Labels, NewState3, EdgeMap).  
  
traverse_bb([Label|Left], State) ->
  BB = state__bb(State, Label),
  Code = hipe_bb:code(BB),
  {NewCode, Map, NewState1}= 
    traverse_code(lists:reverse(Code), Label, State),
  NewBB = hipe_bb:code_update(BB, NewCode),
  NewState2 = state__enter_map_in(NewState1, Label, Map),
  NewState3 = state__bb_add(NewState2, Label, NewBB),
  traverse_bb(Left, NewState3);
traverse_bb([], State) ->
  State.

traverse_code(Code, Label, State) ->
  NextVar = state__next_var(State),
  LiveOut = state__liveout(State, Label),
  Fun = fun(V, {M, N}) -> {gb_trees:insert(V, t_var(N), M), N + 1} end,
  {Map, NewNextVar1} = lists:foldl(Fun, {gb_trees:empty(), NextVar}, LiveOut),
  {NewCode, NewMap, NewNextVar2} = annotate_code(Code, Map, NewNextVar1, []),
  NewState1 = state__next_var_update(State, NewNextVar2),
  NewState2 = get_constr_from_code(NewCode, NewState1),
  NewState3 = state__enter_map_out(NewState2, Label, Map),
  {NewCode, NewMap, NewState3}.

annotate_code([I|Left], Map, NextVar, Acc) ->
  Defs = defines(I),
  Uses = uses(I),
  {SubstDef, NewMap1, NewNextVar1} = annotate_vars(Defs, Map, NextVar),
  Fun = fun(X, Acc) -> gb_trees:delete(X, Acc) end,
  NewMap2 = lists:foldl(Fun, NewMap1, Defs),
  {SubstUses, NewMap3, NewNextVar2} = annotate_vars(Uses, NewMap2, NewNextVar1),
  NewI1 = hipe_icode:subst_defines(SubstDef, I),
  NewI2 = hipe_icode:subst_uses(SubstUses, NewI1),
  annotate_code(Left, NewMap3, NewNextVar2, [NewI2|Acc]);
annotate_code([], Map, NextVar, Acc) ->
  {Acc, gb_trees:balance(Map), NextVar}.


annotate_vars(Vars, Map, NextTypeVar) ->
  annotate_vars(Vars, Map, NextTypeVar, []).

annotate_vars([Var|Left], Map, NextTypeVar, Acc) ->
  case gb_trees:lookup(Var, Map) of
    none ->
      TypeVar = t_var(NextTypeVar),
      NewMap = gb_trees:insert(Var, TypeVar, Map),
      NewAcc = [{Var, hipe_icode:annotate_var(Var, TypeVar)}|Acc],
      annotate_vars(Left, NewMap, NextTypeVar + 1, NewAcc);
    {value, TypeVar} ->
      NewAcc = [{Var, hipe_icode:annotate_var(Var, TypeVar)}|Acc],
      annotate_vars(Left, Map, NextTypeVar, NewAcc)
  end;
annotate_vars([], Map, NextTypeVar, Acc) ->
  {Acc, Map, NextTypeVar}.  

annotate_state_args(State) ->
  case state__fun_args(State) of
    [] -> State;
    Args ->
      Fun = fun(Arg, AccState)->
		NextVar = state__next_var(AccState),
		NewArg = hipe_icode:annotate_var(Arg, t_var(NextVar)),
		NewAccState = state__next_var_update(AccState, NextVar+1),
		{NewArg, NewAccState}
	    end,
      {NewArgs, NewState} = lists:mapfoldl(Fun, State, Args),
      state__fun_args_update(NewState, NewArgs)
  end.
      

get_constr_from_code([I], State) ->
  case hipe_icode:type(I) of
    enter ->
      Args = get_args_type(I),
      Fun = hipe_icode:enter_fun(I),
      case erl_bif_types:arg_types(Fun) of
	any -> 
	  State;
	ArgTypes ->
	  state__add_constraints(State, mk_subtype_constr_list(Args, ArgTypes))
      end;
    _ ->
      %% Control flow operations will be handled together with the edges.
      State
  end;
get_constr_from_code([I|Left], State) ->
  case get_constr_from_ins(I, State) of
    [] ->
      get_constr_from_code(Left, State);
    Constr ->
      get_constr_from_code(Left, state__add_constraints(State, 
							lists:flatten(Constr)))
  end.

get_constr_from_ins(I, State) ->
  Args = get_args_type(I),
  Defs = get_defs_type(I),
  case hipe_icode:type(I) of
    move ->
      [Def] = Defs,
      [Arg] = Args,
      [mk_eq_constr(Arg, Def)];
    call ->
      OwnName = hipe_icode_cfg:function(state__cfg(State)),
      case hipe_icode:call_fun(I) of
	unsafe_tl -> 
	  [Def] = Defs,
	  [Arg] = Args,
	  [mk_subtype_constr(Arg, #cons{head=t_any(), tail=Def})];
	unsafe_hd ->
	  [Def] = Defs,
	  [Arg] = Args,
	  [mk_subtype_constr(Arg, #cons{head=Def, tail=t_any()})];
	'+' ->
	  [Def] = Defs,
	  [mk_subtype_constr_list(Args, [t_number(), t_number()]),
	   mk_subtype_constr_list(Args, [Def, Def]),
	   mk_sup_constr_eq(Def, Args)];
	'-' ->
	  [Def] = Defs,
	  [mk_subtype_constr_list(Args, [t_number(), t_number()]),
	   mk_subtype_constr_list(Args, [Def, Def]),
	   mk_sup_constr_eq(Def, Args)];
	'*' ->
	  [Def] = Defs,
	  [mk_subtype_constr_list(Args, [t_number(), t_number()]),
	   mk_subtype_constr_list(Args, [Def, Def]),
	   mk_sup_constr_eq(Def, Args)];
	'/' ->
	  [Def] = Defs,
	  [mk_subtype_constr_list(Args, [t_number(), t_number()]),
	   mk_eq_constr(Def, t_float())];
	OwnName ->
	  Fun = fun(Var, Nr)->{mk_subtype_constr(Var, t_var(Nr)), Nr+1}end,
	  {Constr1, _} = lists:mapfoldl(Fun, ?FIRST_ARGVAR, Args),
	  case Defs of
	    [] -> Constr1;
	    [Def] -> [mk_subtype_constr(Def, ?RETURN_VAR)|Constr1]
	  end;
	_Fun ->
	  []
      end;
    phi ->
      [Def] = Defs,
      [mk_subtype_constr(A, Def) || A <- Args];
    restore_catch ->
      [Def] = Defs,
      [mk_subtype_constr(Def, t_any())];
    
    
    %% These do not add any constraints
    fmov ->
      [];
    comment ->
      [];

    %% TODO: Don't know how to handle this
    restore_try ->
      []
  end.
  

%% -------------------------------------------------------------------------
%%
%% Traverse the edges in the cfg and find the constraints that comes
%% from the last instruction in each block.
%%

handle_edges_out([Label|Left], EdgeMap, State) ->
  I = hipe_bb:last(state__bb(State, Label)),
  TypeOfIns = hipe_icode:type(I),
  ArgVarsCurrent = get_args_type(I),
  %DefVarsCurrent = get_defs_type(I),
  Args = unannotated_args(I),
  %Defs = unannotated_defines(I),
  {NewEdgeMap, Constr} = 
    case TypeOfIns of
      %% These do not add any constraints here.
      enter -> {EdgeMap, []};
      fail -> {EdgeMap, []};
      return ->	{EdgeMap, []};
      goto -> {EdgeMap, []};
      %% -------------------------------------
      call ->
	{EdgeMap, []};
      'if' ->
	{EdgeMap, []};
      switch_val ->
	[Arg] = Args,
	Cases = hipe_icode:switch_val_cases(I),
	Fail = hipe_icode:switch_val_fail_label(I),
	Fun = 
	  fun({Val, Lbl}, AccOrddict) ->
	      NewVal = t_from_term(hipe_icode:const_value(Val)),
	      case orddict:find(Lbl, AccOrddict) of
		{ok, OldVal} -> 
		  orddict:store(Lbl, t_sup(NewVal,OldVal), AccOrddict);
		error ->
		  orddict:store(Lbl, NewVal, AccOrddict)
	      end
	  end,
	LabelMap = lists:foldl(Fun, [], Cases),
	TrueType = lists:foldl(fun({_Lbl, Type}, Acc)->t_sup(Type, Acc)end,
			       t_none(), LabelMap),
	{LabelMap1, TmpConstr2} =
	  case state__is_legal_label(State, Fail) of
	    true -> 
	      TmpConstr =
		case gb_trees:lookup(Arg, state__map_in(State, Fail)) of
		  {value, Var} ->
		    [mk_sup_constr(hd(ArgVarsCurrent), [TrueType,Var])];
		  none ->
		    mk_subtype_constr_list(ArgVarsCurrent, [TrueType])
		end,
	      {orddict:erase(Fail, LabelMap), TmpConstr};
	    false -> 
	      {LabelMap, mk_subtype_constr_list(ArgVarsCurrent, [TrueType])}
	  end,
	{add_edge_out_constr([{Arg, LabelMap1}], Label, EdgeMap), 
	 TmpConstr2};
      switch_tuple_arity ->
	[];
      type ->
	TrueType = true_branch_info(hipe_icode:type_type(I)),
	Succ = hipe_icode:type_true_label(I),
	Fail = hipe_icode:type_false_label(I),
	[Arg] = Args,

	case Succ == Fail of
	  true -> {EdgeMap, []};
	  false ->
	    case state__is_legal_label(State, Fail) of
	      false ->
		{EdgeMap, mk_subtype_constr_list(ArgVarsCurrent, [TrueType])};
	      true ->
		LabelMap = [{Succ, TrueType}],
		{add_edge_out_constr([{Arg, LabelMap}], Label, EdgeMap), []}
	    end
	end
    end,
  NewState1 = state__add_constraints(State, Constr),
  NewState2 =
    case TypeOfIns of
      return -> 
	state__add_return(NewState1, hd(get_args_type(I)));
      enter -> 
	EnterFun = hipe_icode:enter_fun(I),	
	state__add_return(NewState1, 
			  mk_call_constr(EnterFun, get_args_type(I)));
      _ ->
	NewState1
    end,
  SuccConstr = mk_succ_constraints(NewState1, Label),
  debug("Handling ~w\nNew edge out constraints:\n", [Label]),
  [debug("\t~s\n", [format_constraint(X)])||X<-Constr++SuccConstr],
  debug("End\n\n", []),
  NewState3 = state__add_constraints(NewState2, SuccConstr),
  handle_edges_out(Left, NewEdgeMap, NewState3);
handle_edges_out([], EdgeMap, State) ->
  %% Finally add the constraints for the arguments and the return of
  %% the function.
  AnnotatedArgs = state__fun_args(State),
  TypeVars = [hipe_icode:var_annotation(X)||X<-AnnotatedArgs],
  Args = strip_annotations_from_vars(AnnotatedArgs),
  Start = hipe_icode_cfg:start_label(state__cfg(State)),
  Constr1 = mk_edge_constraints_eq(Args, TypeVars, state__map_in(State, Start)),
  Constr2 = mk_eq_constr(?RETURN_VAR, state__return(State)),
  {state__add_constraints(State, [Constr2|Constr1]), EdgeMap}.


add_edge_out_constr([{Var, LabelMap}|Left], From, EdgeMap) ->
  Fun = fun({To, C}, Acc)->
	    add_edge_out_constr_one_edge(From, To, Var, C, Acc)
	end,
  NewEdgeMap = lists:foldl(Fun, EdgeMap, LabelMap),
  add_edge_out_constr(Left, From, NewEdgeMap);
add_edge_out_constr([], _From, EdgeMap) ->
  EdgeMap.

add_edge_out_constr_one_edge(From, To, Var, Constr, EdgeMap) ->
  case gb_trees:lookup({From, To}, EdgeMap) of
    none -> gb_trees:insert({From, To}, [{Var, Constr}], EdgeMap);
    {value, Orddict} ->
      gb_trees:update({From, To}, orddict:store(Var, Constr, Orddict), EdgeMap)
  end.

mk_succ_constraints(State, Label) ->
  case lists:filter(fun(X)->state__is_legal_label(State, X)end,
		    state__succ(State, Label)) of
    [] -> [];
    [Succ] ->
      MapIn = state__map_in(State, Succ),
      MapOut = state__map_out(State, Label),
      merge_two_maps(MapOut, MapIn);
    Succs ->
      MapOut = state__map_out(State, Label),
      MapsIn = [state__map_in(State, X) || X <- Succs],      
      Vars = gb_trees:keys(MapOut),
      Fun = fun(Var, Acc) ->
		TVars = [gb_trees:get(Var, Map)|| 
			  Map<-MapsIn, gb_trees:is_defined(Var, Map)],
		case ordsets:from_list(TVars) of
		  [] -> Acc;
		  [TVar] -> [mk_subtype_constr(gb_trees:get(Var, MapOut), TVar)
			     |Acc];
		  TVars1 -> [mk_sup_constr(gb_trees:get(Var, MapOut), TVars1)
			     |Acc]
		end
	    end,
      lists:foldl(Fun, [], Vars)
  end.

merge_two_maps(MapOut, MapIn) ->
  merge_two_maps(gb_trees:to_list(MapOut), MapIn, []).

merge_two_maps([{Var, TVar1}|Left], Map, Acc) ->
  case gb_trees:lookup(Var, Map) of
    {value, TVar2} ->
      merge_two_maps(Left, Map, [mk_subtype_constr(TVar1, TVar2)|Acc]);
    none ->
      merge_two_maps(Left, Map, Acc)
  end;
merge_two_maps([], _Map, Acc) ->
  Acc.

handle_edges_in([Label|Left], State, EdgeMap) ->  
  Constr =
    case state__pred(State, Label) of
      [] -> [];
      [Pred] ->
	MapIn = state__map_in(State, Label),
	MapOut = state__map_out(State, Pred),
	Constr1 = merge_two_maps(MapIn, MapOut),
	case gb_trees:lookup({Pred, Label}, EdgeMap) of
	  none ->
	    Constr1;
	  {value, VarMap} ->
	    Fun = fun({Var, C}, Acc) ->
		      case gb_trees:lookup(Var, MapIn) of
			none -> Acc;
			{value, TVar} -> [mk_subtype_constr(TVar, C)|Acc]
		      end
		  end,
	    Constr2 = lists:foldl(Fun, [], VarMap),
	    Constr1 ++ Constr2
	end;
      Preds ->
	MapIn = state__map_in(State, Label),
	MapsOut = [{{X, Label}, state__map_out(State, X)} || X <- Preds],
	mk_edge_in_constr(gb_trees:to_list(MapIn), MapsOut, EdgeMap)
    end,
  debug("Handling ~w\nNew edge in constraints:\n", [Label]),
  [debug("\t~s\n", [format_constraint(X)])||X<-Constr],
  debug("End\n\n", []),
  NewState = 
    case Constr of
      [] -> State;
      _ -> state__add_constraints(State, Constr)
    end,
  handle_edges_in(Left, NewState, EdgeMap);
handle_edges_in([], State, _EdgeMap) ->
  State.

mk_edge_in_constr(List, MapsOut, EdgeMap) ->
  mk_edge_in_constr(List, MapsOut, EdgeMap, []).

mk_edge_in_constr([{Var, TVar}|Left], MapsOut, EdgeMap, Acc) ->
  Constr = mk_edge_in_constr_one_var(Var, TVar, MapsOut, EdgeMap),
  mk_edge_in_constr(Left, MapsOut, EdgeMap, [Constr|Acc]);
mk_edge_in_constr([], _MapsOut, _EdgeMap, Acc) ->
  lists:flatten(Acc).

mk_edge_in_constr_one_var(Var, LHS, MapsOut, EdgeMap) ->
  Fun = fun({Edge, Map}, Acc) ->
	    case gb_trees:lookup(Var, Map) of
	      {value, TVar} -> [{Edge, TVar}|Acc];
	      none -> Acc
	    end
	end,
  TVarsOut = lists:foldl(Fun, [], MapsOut),
  case TVarsOut of
    [] -> exit('Liveness problem');
    [{Edge, RHS}] -> 
      %% Check the edge for additional constraints
      case gb_trees:lookup(Edge, EdgeMap) of
	{value, Orddict} ->
	  case orddict:find(Var, Orddict) of
	    {ok, C} ->
	      [mk_subtype_constr(LHS, RHS),
	       mk_subtype_constr(LHS, C)];
	    error ->
	      [mk_subtype_constr(LHS, RHS)]
	  end;
	none ->
	  [mk_subtype_constr(LHS, RHS)]
      end;
    List -> 
      %% The variable is live in over more than one edge 
      Edges = [X || {X, _} <- List],
      TVars = [X || {_, X} <- List],
      Fun2 = fun(Edge, Acc) ->
		 case gb_trees:lookup(Edge, EdgeMap) of
		   {value, Orddict} ->
		     case orddict:find(Var, Orddict) of
		       {ok, C} -> [C|Acc];
		       error -> Acc
		     end;
		   none -> Acc
		 end
	     end,
      Cs = lists:foldl(Fun2, [], Edges),
		
      %% The variable constraints
      Constr1 = 
	case ordsets:from_list(TVars) of
	  [] -> [];
	  [TVar] -> [mk_subtype_constr(LHS, TVar)];
	  TVars1 -> [mk_sup_constr(LHS, TVars1)]
	end,
      %% Additional edge constraints
      Constr2 = 
	case ordsets:from_list(Cs) of
	  [] -> [];
	  [C] -> [mk_subtype_constr(LHS, C)];
	  Cs1 -> 
	    case lists:all(fun type_is_const/1, Cs1) of
	      true ->
		[mk_subtype_constr(LHS, t_sup(Cs1))];
	      false ->
		[mk_sup_constr(LHS, Cs1)]
	    end
	end,
      Constr1++Constr2
  end.

%% _________________________________________________________________
%%
%% Constraints.
%%


mk_subtype_constr(T1, T2) ->
  #subtype_constraint{type1 = T1, type2 = T2}.

mk_subtype_constr_list([T1|Left1], [T2|Left2]) ->
  [mk_subtype_constr(T1, T2)|mk_subtype_constr_list(Left1, Left2)];
mk_subtype_constr_list([], []) ->
  [];
mk_subtype_constr_list(Some, Any) ->
  case t_is_any(Any) of
    true -> [];
    false -> exit({?MODULE, 'Non-matching argument lists', {Some, Any}})
  end.

mk_eq_constr(T1, T2) ->      
  #eq_constraint{type1 = T1, type2 = T2}.

%mk_subtract_constr(T1, T2, T3) ->
%  #subtype_constraint{type1 = T1, type2 = #subtract{type1 = T2, type2 = T3}}.
%mk_subtract_constr(T1, T2, _T3) ->
%  mk_subtype_constr(T1, T2).

%mk_subtract_constr_eq(T1, T2, T3) ->
%  [#subtype_constraint{type1 = T1, type2 = #subtract{type1 = T2, type2 = T3}},
%   #subtype_constraint{type2 = T1, type1 = #subtract{type1 = T2, type2 = T3}}].

mk_sup_constr(T1, List) ->
  #subtype_constraint{type1 = T1, type2 = #sup{types=List}}.

mk_sup_constr_eq(T1, List) ->
  #eq_constraint{type1 = T1, type2 = #sup{types=ordsets:from_list(List)}}.

mk_call_constr(Fun, Args) ->
  #call{'fun'=Fun, args=Args}.  


mk_edge_constraints_eq(Keys, RHS, Map) ->
  mk_edge_constraints_eq(Keys, RHS, Map, []).

mk_edge_constraints_eq([Key|KeyTail], [RHS|RHSTail], Map, Acc) ->
  case gb_trees:lookup(Key, Map) of
    none -> mk_edge_constraints_eq(KeyTail, RHSTail, Map, Acc);
    {value, LHS} -> 
      NewAcc = [mk_eq_constr(LHS, RHS)|Acc],
      mk_edge_constraints_eq(KeyTail, RHSTail, Map, NewAcc)
  end;
mk_edge_constraints_eq([], [], _Map, Acc) ->
  Acc.


  
%% _________________________________________________________________
%%
%% Some help functions.
%%

defines(I)->
  keep_vars(hipe_icode:defines(I)).

args(I)->
  keep_vars_and_consts(hipe_icode:args(I)).

unannotated_args(I) ->
  strip_annotations_from_vars(args(I)).

%unannotated_defines(I) ->
%  strip_annotations_from_vars(defines(I)).

strip_annotations_from_vars([H|T])->
  NewH =
    case hipe_icode:is_annotated_var(H) of
      true ->
	hipe_icode:unannotate_var(H);
      false ->
	H
    end,
  [NewH|strip_annotations_from_vars(T)];
strip_annotations_from_vars([]) ->
  [].

keep_vars_and_consts(Vars)->
  lists:filter(fun(X)->
		   hipe_icode:is_var(X) orelse hipe_icode:is_annotated_var(X)
		     orelse hipe_icode:is_const(X)
	       end, 
	       Vars).

uses(I)->
  keep_vars(hipe_icode:uses(I)).

keep_vars(Vars)->
  lists:filter(fun(X)->
		   hipe_icode:is_var(X) orelse hipe_icode:is_annotated_var(X)
	       end, Vars).

get_args_type(I) ->
  Args = args(I),
  [expand_type(X) || X <- Args].

expand_type(VarOrConst) ->
  case hipe_icode:is_const(VarOrConst) of
    true -> t_from_term(hipe_icode:const_value(VarOrConst));
    false -> hipe_icode:var_annotation(VarOrConst)
  end.

get_defs_type(I) ->
  Defs = defines(I),
  [hipe_icode:var_annotation(X) || X <- Defs].
  


%% _________________________________________________________________
%%
%% The State.
%%

-record(state, {cfg, constraints, fun_args, label_map, 		
		legal_labels, liveness, next_type_var, 
		pred_map, return, succ_map}).
		

new_state(Cfg) ->
  SuccMap = hipe_icode_cfg:succ_map(Cfg),
  PredMap = hipe_icode_cfg:pred_map(Cfg),
  Liveness = hipe_icode_ssa:ssa_liveness__analyze(Cfg),
  LegalLabels = find_legal_labels(Cfg),
  #state{cfg=Cfg,
	 constraints=gb_sets:empty(), 
	 fun_args=hipe_icode_cfg:params(Cfg),
	 liveness=Liveness,
	 legal_labels=LegalLabels,
	 label_map=gb_trees:empty(), 
	 next_type_var=1,
	 pred_map=PredMap,
	 return=[],
	 succ_map=SuccMap}.

state__cfg(#state{cfg=Cfg}) ->
  Cfg.

state__fun_args(#state{fun_args=Args}) ->
  Args.

state__fun_args_update(State, Args) ->
  State#state{fun_args=Args}.

state__labels(#state{legal_labels=LegalLabels}) ->
  LegalLabels.

state__is_legal_label(#state{legal_labels=LegalLabels}, Label) ->
  ordsets:is_element(Label, LegalLabels).

state__bb(#state{cfg=Cfg}, Label) ->
  hipe_icode_cfg:bb(Cfg, Label).

state__bb_add(S = #state{cfg=Cfg}, Label, BB) ->
  S#state{cfg=hipe_icode_cfg:bb_add(Cfg, Label, BB)}.

state__succ(#state{succ_map=SM}, Label) ->
  hipe_icode_cfg:succ(SM, Label).

state__pred(#state{pred_map=PM}, Label) ->
  hipe_icode_cfg:pred(PM, Label).

state__liveout(#state{liveness=Liveness}, Label) ->
  hipe_icode_ssa:ssa_liveness__liveout(Liveness, Label).

state__next_var(#state{next_type_var=N}) ->
  N.

state__next_var_update(S = #state{}, NextVar) ->
  S#state{next_type_var=NextVar}.

state__enter_map_in(S = #state{label_map = LabelMap}, Label, Map) ->
  S#state{label_map = gb_trees:enter({Label, in}, Map, LabelMap)}.

state__enter_map_out(S = #state{label_map = LabelMap}, Label, Map) ->
  S#state{label_map = gb_trees:enter({Label, out}, Map, LabelMap)}.

state__map_in(#state{label_map = LabelMap}, Label) ->
  gb_trees:get({Label, in}, LabelMap).

state__map_out(#state{label_map = LabelMap}, Label) ->
  gb_trees:get({Label, out}, LabelMap).


state__add_constraints(State = #state{constraints=Constr}, Add) ->
  NewConstr = lists:foldl(fun(X, M) -> gb_sets:add(X, M)end, 
			  Constr, Add),
  State#state{constraints=NewConstr}.
  
state__constraints(#state{constraints=C}) ->
  C.

state__add_return(S = #state{return=Return}, Ret) ->
  S#state{return=ordsets:add_element(Ret, Return)}.

state__return(#state{return=Return}) ->
  type_sup(Return).

%% _________________________________________________________________
%%
%% Find the legal labels of the cfg.
%%

find_legal_labels(Cfg) ->
  Labels = hipe_icode_cfg:labels(Cfg),
  PredMap = hipe_icode_cfg:pred_map(Cfg),
  find_legal_labels(ordsets:from_list(Labels), PredMap, Cfg, []).

find_legal_labels([Label|Left], PredMap, Cfg, Acc) ->
  case ordsets:is_element(Label, Acc) of
    true -> find_legal_labels(Left, PredMap, Cfg, Acc);
    false ->
      case hipe_icode:type(hipe_bb:last(hipe_icode_cfg:bb(Cfg, Label))) of
	Return when Return == return; Return == enter ->
	  NewAcc = follow_trace([Label], PredMap, Acc),
	  find_legal_labels(ordsets:subtract(Left, NewAcc), PredMap, 
			    Cfg, NewAcc);
	_ ->
	  find_legal_labels(Left, PredMap, Cfg, Acc)
      end
  end;
find_legal_labels([], _PredMap, _Cfg, Acc) ->
  Acc.

follow_trace([Label|Left], PredMap, Acc) ->
  case ordsets:is_element(Label, Acc) of
    true ->
      follow_trace(Left, PredMap, Acc);
    false ->
      NewAcc = ordsets:add_element(Label, Acc),
      Pred = [X || X <- hipe_icode_cfg:pred(PredMap, Label),
		   not ordsets:is_element(X, NewAcc)],
      follow_trace(Left, PredMap, 
		   ordsets:union(Acc, follow_trace(Pred, PredMap, NewAcc)))
  end;
follow_trace([], _PredMap, Acc) ->
  Acc.
      

%% _________________________________________________________________
%%
%% The types for type tests
%%

true_branch_info(integer) ->
  t_integer();
true_branch_info({integer, N}) ->
  t_integer(N);
true_branch_info(float) ->
  t_float();  
true_branch_info(number) ->
  t_number();
true_branch_info(atom) ->
  t_atom();
true_branch_info({atom, A}) ->
  t_atom(A);
true_branch_info(list) ->
  type_sup(type_cons(), t_nil());
true_branch_info(tuple) ->
  t_tuple();
true_branch_info({tuple, N}) ->
  t_tuple(N);
true_branch_info(pid) ->
  t_pid();
true_branch_info(port) ->
  t_port();
true_branch_info(binary) ->
  t_binary();
true_branch_info(reference) ->
  t_ref();
true_branch_info(function) ->
  t_fun();
true_branch_info(cons) ->
  type_cons();
true_branch_info(nil) ->
  t_nil();
true_branch_info(boolean) ->
  t_bool();
true_branch_info(constant) ->
  %% Since we do not have negative types like "not tuple"
  t_any();
true_branch_info(T) ->
  exit({?MODULE,unknown_typetest,T}).


%% _________________________________________________________________
%%
%% Constraint solver.
%%

solve_constraints(State) ->  
  {Graph, Map} = build_constr_graph(state__constraints(State)),  
  pp_graph(Graph),
  solve_loop_iterate(lists:reverse(dfs(Graph)), Graph, Map).
  

solve_loop_iterate(Nodes, Graph, Map) ->
  case solve_loop(Nodes, Graph, Map) of
    {ok, NewMap} ->
      Map1 = gb_trees:balance(Map),
      case gb_trees:balance(NewMap) of
	Map1 -> {ok, NewMap};
	_ -> solve_loop_iterate(Nodes, Graph, NewMap)
      end;
    Res -> Res
  end.

solve_loop([Node|Left], Graph, Map) ->
  case Node of
    #sup{types = Ts} ->
      NewType = type_sup(lookup_type_list(Ts, Map)),
      solve_loop(Left, Graph, enter_type(Node, NewType, Map));
    _ ->
      case graph__edges_from(Node, Graph) of
	[] -> solve_loop(Left, Graph, Map);
	SuperNodes ->
	  Type = lookup_type(Node, Map),
	  debug("Considering ~s (~s) <: ~s\n", 
		[format(Node), format(Type), format_args(SuperNodes)]),
	  case solve_constraints(Node, Type, SuperNodes, Map) of
	    {ok, Node} -> solve_loop(Left, Graph, Map);
	    {ok, NewType} -> 
	      NewMap = enter_type(Node, NewType, Map),
	      debug("Entering ~s ::= ~s \n\n\n", 
		    [format(Node), format(NewType)]),
	      solve_loop(Left, Graph, NewMap);
	    Error -> Error
	  end
      end
  end;
solve_loop([], _Graph, Map) ->
  {ok, Map}.

solve_constraints(Node, Type, [SuperNode|Left], Map) ->
  SuperType = lookup_type(SuperNode, Map),
  debug("\tConsidering ~s (~s) <: ~s\n", [format(Node), format(Type), 
					  format(SuperType)]),
  case type_is_subtype(Type, SuperType) of
    true ->
      %% This is trivially true, do not propagate the information
      %% to avoid recursive dependencies.
      debug("\tKeeping Type ~s\n", [format(Type)]),
      solve_constraints(Node, Type, Left, Map);
    false ->
      Inf = type_inf(Type, SuperType),
      debug("\tNewType is ~s\n", [format(Inf)]),
      solve_constraints(Node, Inf, Left, Map)
  end;
solve_constraints(_Node, Type, [], _Map) ->
  {ok, Type}.
      

%% _________________________________________________________________
%%
%% Type handling
%%


evaluate_type(#sup{types=Types}, Map) ->
  t_sup([evaluate_type(T, Map)||T <- Types]);
evaluate_type(#inf{types=Types}, Map) ->
  t_inf([evaluate_type(T, Map)||T <- Types]);
evaluate_type(#subtract{type1=T1, type2=T2}, Map) ->
  t_subtract(evaluate_type(T1, Map), lookup_type(T2, Map));
evaluate_type(#call{'fun'=Fun, args=Args}, Map) -> 
  hipe_icode_primops:type(Fun, evaluate_type_list(Args, Map));
evaluate_type(C = #cons{head=H, tail=T}, Map) ->   
  T2 = lookup_type(T, Map),
  %debug("Evaluating cons\n", []),
  case split_cons_or_nil(T2, Map) of
    {ok, C} ->
      %%debug("Found a proper list!\n", []),
      t_list(evaluate_type(H, Map));
    {ok, _Other} ->
      %%debug("Found a improper list: ~s!\n", [format(Other)]),
      t_cons(evaluate_type(H, Map), 
	     evaluate_type(T, Map));
    error -> 
      %%debug("Tail was not cons or nil!\n", []),
      t_cons(evaluate_type(H, Map), 
	     evaluate_type(T, Map))
  end;
evaluate_type(VarOrConst, Map) ->
  case t_is_var(VarOrConst) of
    true ->
      evaluate_type(lookup_type(VarOrConst, Map), Map);
    false ->
      VarOrConst
  end.

evaluate_type_list([H|T], Map) ->
  [evaluate_type(H, Map)|evaluate_type_list(T, Map)];
evaluate_type_list([], _Map) ->
  [].

lookup_type_list([H|T], Map) ->
  [lookup_type(H, Map)|lookup_type_list(T, Map)];
lookup_type_list([], _Map) ->
  [].

lookup_type(Node, Map) ->
  case t_is_var(Node) of
    true ->
      case gb_trees:lookup(Node, Map) of
	none -> t_any();
	{value, Type} -> 
	  case t_is_var(Type) of
	    true -> lookup_type(Type, Map);
	    false -> Type
	  end
      end;
    false ->
      case Node of
	#sup{types=Ts} -> 
	  type_sup(lookup_type_list(Ts, Map));
	_ -> Node
      end
  end.

enter_type(Node, Type, Map) ->
  case gb_trees:lookup(Node, Map) of
    none -> 
      gb_trees:insert(Node, Type, Map);
    {value, Type} ->
      Map;
    {value, OldType} ->
      case t_is_var(OldType) of
	true ->
	  enter_type(OldType, Type, Map);
	false ->
	  gb_trees:update(Node, Type, Map)
      end
  end.

type_is_const(Node) ->
  case Node of
    #inf{} ->        false;
    #sup{} ->        false;
    #subtract{} ->   false;
    #call{} ->       false;
    #cons{} ->       false;
    _ ->             not t_is_var(Node)
  end.

split_cons_or_nil(#sup{types=T}, Map) ->
  Nil = ordsets:filter(fun(X)->t_is_nil(lookup_type(X, Map))end, T),
  Cons = ordsets:filter(fun(X)->type_is_cons(lookup_type(X, Map))end, T),
  case ordsets:union(Nil, Cons) of
    T -> {ok, type_sup(lookup_type_list(Cons, Map))};
    _ -> error
  end;
split_cons_or_nil(_, _Map) ->
  error.


type_sup([T1|Left]) ->
  type_sup(T1, type_sup(Left));
type_sup([]) ->
  t_none().


type_sup(T, T) ->
  T;
type_sup(T1, T2) -> 
  case t_is_none(T1) of
    true -> T2;
    false ->
      case t_is_none(T2) of
	true -> T1;
	false ->
	  case type_sup_1(T1, T2) of
	    #sup{types=Ts} ->
	      NewTs =
		case [X || X <- Ts, type_is_const(X)==true] of
		  [] -> Ts;
		  ConstT -> 
		    NewTs1 = ordsets:subtract(Ts, ConstT),
		    ordsets:add_element(t_sup(ConstT), NewTs1)
		end,
	      case NewTs of
		[T] -> T;
		_ -> #sup{types=NewTs}
	      end;
	    Other ->
	      Other
	  end
      end
  end.

type_sup_1(#sup{types=T1}, #sup{types=T2}) ->
  #sup{types=ordsets:union(T1, T2)};
type_sup_1(T1, #sup{types=T2}) ->
  #sup{types=ordsets:add_element(T1, T2)};
type_sup_1(#sup{types=T1}, T2) ->
  #sup{types=ordsets:add_element(T2, T1)};
type_sup_1(#cons{head=H1, tail=T1}, #cons{head=H2, tail=T2}) ->
  #cons{head=type_sup(H1, H2), tail=type_sup(T1, T2)};
type_sup_1(T1, T2) ->
  #sup{types=ordsets:from_list([T1, T2])}.


type_inf(T, T) ->
  T;
type_inf(T1, T2) -> 
  case t_is_any(T1) of
    true -> T2;
    false ->
      case t_is_any(T2) of
	true -> T1;
	false ->
	  case type_inf_1(T1, T2) of
	    #inf{types=Ts} ->
	      NewTs =
		case [X || X <- Ts, type_is_const(X)==true] of
		  [] -> Ts;
		  ConstT -> 
		    NewTs1 = ordsets:subtract(Ts, ConstT),
		    ordsets:add_element(t_inf(ConstT), NewTs1)
		end,
	      case NewTs of
		[T] -> T;
		_ -> #inf{types=NewTs}
	      end;
	    Other -> Other
	  end
      end
  end.

type_inf_1(#inf{types=T1}, #inf{types=T2}) ->
  #inf{types=ordsets:union(T1, T2)};
type_inf_1(T1, #inf{types=T2}) ->
  #inf{types=ordsets:add_element(T1, T2)};
type_inf_1(#inf{types=T1}, T2) ->
  #inf{types=ordsets:add_element(T2, T1)};
type_inf_1(Sup = #sup{types=T1}, T2) ->
  case ordsets:is_element(T2, T1) of
    true -> T2;
    false -> #inf{types=ordsets:from_list([Sup, T2])}
  end;
type_inf_1(T1, Sup = #sup{types=T2}) ->
  case ordsets:is_element(T1, T2) of
    true -> T1;
    false -> #inf{types=ordsets:from_list([Sup, T1])}
  end;
type_inf_1(#cons{head=H1, tail=T1}, #cons{head=H2, tail=T2}) ->
  #cons{head=type_inf(H1, H2), tail=type_inf(T1, T2)};
type_inf_1(T1, T2) ->
  case t_is_any(T1) of
    true -> T2;
    false ->
      case t_is_any(T2) of
	true -> T1;
	false ->
	  case type_is_const(T1) andalso type_is_const(T2) of
	    true -> t_inf(T1, T2);
	    false ->
	      #inf{types=ordsets:from_list([T1, T2])}
	  end
      end
  end.

type_is_subtype(T1, T2) ->
  case type_inf(T1, T2) of
    T1 -> true;
    _ -> false
  end.


type_cons() ->
  type_cons(t_any(), t_any()).

type_cons(H, T) ->
  #cons{head=H, tail=T}.

type_is_cons(#cons{}) -> true;
type_is_cons(_) ->       false.
  

%% _________________________________________________________________
%%
%% Constraint graph
%%

-record(graph, {edges, reverse_edges}).

new_graph() ->
  #graph{edges=gb_trees:empty(), reverse_edges=gb_trees:empty()}.

build_constr_graph(Constrs) ->
  CList = gb_sets:to_list(Constrs),
  {Graph, _Map} = graph__add_all_constr(CList, new_graph(), gb_trees:empty()),
  Map = build_var_map(Graph),
  graph__add_all_constr(CList, new_graph(), Map).

graph__nodes(#graph{edges=Edges}) ->
  gb_trees:keys(Edges).

graph__add_all_constr([C = #subtype_constraint{type1=T1, type2=T2}|Left], 
		      Graph, Map) ->
  case type_is_const(T1) andalso type_is_const(T2) of
    true ->
      case t_is_subtype(T1, T2) of
	true -> graph__add_all_constr(Left, Graph, Map);
	false -> exit({'Constant constraint not fullfilled',C})
		       
      end;
    false ->
      case {replace_node(T1, Map), replace_node(T2, Map)} of
	{T, T} -> graph__add_all_constr(Left, Graph, Map);
	{NewT1, NewT2} ->
	  NewGraph = graph__add_edge(NewT1, NewT2, Graph),
	  graph__add_all_constr(Left, NewGraph, Map)
      end
  end;
graph__add_all_constr([#eq_constraint{type1=T1, type2=T2}|Left], 
		      Graph, Map) ->
  case {replace_node(T1, Map), replace_node(T2, Map)} of
    {T, T} -> graph__add_all_constr(Left, Graph, Map);
    {NewT1, NewT2} ->
      NewGraph1 = graph__add_edge(NewT1, NewT2, Graph),
      NewGraph2 = graph__add_edge(NewT2, NewT1, NewGraph1),
      graph__add_all_constr(Left, NewGraph2, Map)
  end;
graph__add_all_constr([], Graph, Map) ->
  {Graph, Map}.

graph__add_edge(T1, T2, G = #graph{edges=Edges, reverse_edges=REdges}) ->
  NewEdges =
    case gb_trees:lookup(T1, Edges) of
      none ->
	gb_trees:insert(T1, [T2], Edges);
      {value, Ordset1} ->
	gb_trees:update(T1, ordsets:add_element(T2, Ordset1), Edges)
    end,
  NewREdges =
    case gb_trees:lookup(T2, REdges) of
      none ->
	gb_trees:insert(T2, [T1], REdges);
      {value, Ordset2} ->
	gb_trees:update(T2, ordsets:add_element(T1, Ordset2), REdges)
    end,
  G#graph{edges=NewEdges, reverse_edges=NewREdges}.

build_var_map(Graph) ->
  Sccs = find_sccs(Graph),
  build_var_map(Sccs, gb_trees:empty()).

build_var_map([Scc|Left], Map) ->
  case Scc of
    [_] ->
      build_var_map(Left, Map);
    Ordset ->
      case [X || X <- Ordset, t_is_var(X)==true] of
	[] -> build_var_map(Left, Map);
	[_] -> build_var_map(Left, Map);
	[ID|Tail] ->
	  Fun = fun(X, Acc)->gb_trees:insert(X, ID, Acc) end,
	  NewMap = lists:foldl(Fun, Map, Tail),
	  build_var_map(Left, gb_trees:insert(ID, t_any(), NewMap))
      end
  end;
build_var_map([], Map) ->
  Map.

find_sccs(Graph) ->
  find_sccs(graph__nodes(Graph), Graph, []).

find_sccs([Node|Left], Graph, Acc) ->
  {_, Visited1} = dfs(Node, Graph),
  {_, Visited2} = dfs_reverse(Node, Graph),
  SCC = ordsets:intersection(Visited1, Visited2),
  SCC1 = [X || X <- SCC, t_is_var(X) == true],
  find_sccs(ordsets:subtract(Left, SCC), Graph, [SCC1|Acc]);
find_sccs([], _Graph, Acc) ->
  Acc.

replace_node(T, Map) ->  
  case T of
    #cons{head=H, tail=Tail} ->
      #cons{head=replace_node(H, Map), tail=replace_node(Tail, Map)};
    #sup{types=Types} ->
      #sup{types=ordsets:from_list([replace_node(X, Map)||X<-Types])};
    #inf{types=Types} ->
      #inf{types=ordsets:from_list([replace_node(X, Map)||X<-Types])};
    #subtract{type1=T1, type2=T2} ->
      #subtract{type1=replace_node(T1, Map), type2=replace_node(T2, Map)};
    _ ->
      case t_is_var(T) of
	true ->
	  case gb_trees:lookup(T, Map) of
	    none -> T;
	    {value, NewT1} -> 
	      case t_is_var(NewT1) of
		true -> NewT1;
		false -> T
	      end
	  end;
	false ->
	  T
      end
  end.

graph__edges_from(Node, #graph{edges=Map}) ->
  case gb_trees:lookup(Node, Map) of
    none -> [];
    {value, E} -> E
  end.

graph__edges_from_reverse(Node, #graph{reverse_edges=Map}) ->
  case gb_trees:lookup(Node, Map) of
    none -> [];
    {value, E} -> E
  end.

-ifdef(DEBUG).
pp_graph(Graph) ->
  Fun = 
    fun(X, List) ->	
	io:format("~s:\n", [format(X)]),
	[io:format("\t~s:\n", [format(Y)]) || Y <- List],
	ok
    end,
  io:format("=======  Edges: ========\n", []),
  lists:foreach(fun(X) -> Fun(X, graph__edges_from(X, Graph))end,
		graph__nodes(Graph)),
  ok.
-else.
pp_graph(_Graph) ->
  ok.
-endif.


dfs(Graph) ->
  Nodes = graph__nodes(Graph),
  {Order, _Visited} = dfs(Nodes, fun graph__edges_from/2, 
			  Graph, ordsets:new(), []),
  Order.

dfs(Node, Graph) ->
  dfs([Node], fun graph__edges_from/2, Graph, ordsets:new(), []).

dfs_reverse(Node, Graph) ->
  dfs([Node], fun graph__edges_from_reverse/2, Graph, ordsets:new(), []).


dfs([Node|Left], Fun, Graph, Visited, Order)->
  case ordsets:is_element(Node, Visited) of
    true ->
      dfs(Left, Fun, Graph, Visited, Order);
    false ->
      Succ = Fun(Node, Graph), 
      {NewOrder, NewVisited} = dfs(Succ, Fun, Graph, 
				   ordsets:add_element(Node, Visited), Order),
      dfs(Left, Fun, Graph, NewVisited, [Node|NewOrder])
  end;
dfs([], _Fun, _Graph, Visited, Order) ->
  {Order, Visited}.


%% _________________________________________________________________
%%
%% Prettyprinter
%%


pp(State, Map) ->
  io:format("\n=======================================================\n", []),
%  List = gb_sets:to_list(State#state.constraints),
%  io:format("********  Constraints ************\n", []),
%  [io:format("~s\n", [format_constraint(C)]) || C <- List],
%  io:format("********  Map ************\n", []),
%  [io:format("~s = ~s\n", [format(X), format(lookup_type(X, Map))]) 
%   || {X, _} <- gb_trees:to_list(Map)],
%  hipe_icode_cfg:pp(state__cfg(State)),
  hipe_icode_cfg:pp(update_var_types_cfg(State, Map)),
  pp_signature(State, Map),
  ok.
  

format_constraint(#eq_constraint{type1=T1, type2=T2}) ->
  io_lib:format("~s = ~s", [format(T1), format(T2)]);
format_constraint(#subtype_constraint{type1=T1, type2=T2}) ->
  io_lib:format("~s <: ~s", [format(T1), format(T2)]).

format(#call{'fun'=Fun, args=Args}) ->
  io_lib:format("Call to ~p(~s)", [Fun, format_args(Args)]);
format(#subtract{type1=T1, type2=T2}) ->
  io_lib:format("(~s \\ ~s)", [format(T1), format(T2)]);
format(#sup{types=Types}) ->
  io_lib:format("sup(~s)", [format_args(Types)]);
format(#inf{types=Types}) ->
  io_lib:format("inf(~s)", [format_args(Types)]);
format(#cons{head=H, tail=T}) ->
  io_lib:format("cons(~s, ~s)", [format(H), format(T)]);
format(T) ->
  t_to_string(T).

format_args([]) ->
  [];
format_args([T]) ->
  format(T);
format_args([T|Left]) ->
  format(T) ++ ", " ++ format_args(Left).

pp_signature(State, Map) ->
  io:format("========= Fun signature ==========\n", []),
  Args = [hipe_icode:var_annotation(X) || X <- state__fun_args(State)],
  ArgTypes = [evaluate_type(X, Map)||X <- Args],
  Return = evaluate_type(?RETURN_VAR, Map),
  {M, F, A} = hipe_icode_cfg:function(state__cfg(State)),
  if length(Args) > 0 ->
      io:format("~w:~w/~w(~s) -> ~s\n", 
		[M, F, A, format_args(ArgTypes), format(Return)]);
     true ->
      io:format("~w:~w/~w() -> ~s\n", 
		[M, F, A, format(Return)])
  end.



update_var_types_cfg(State, Map) ->
  Cfg = state__cfg(State),
  Args = [update_var_types(A, Map) || A <- state__fun_args(State)],
  NewCfg = hipe_icode_cfg:params_update(Cfg, Args),
  Labels = state__labels(State),
  update_var_types_bbs(Labels, NewCfg, Map).
  
update_var_types_bbs([Label|Left], Cfg, Map)->
  BB = hipe_icode_cfg:bb(Cfg, Label),
  Code = hipe_bb:code(BB),
  NewCode = update_var_types_instr_list(Code, Map, []),
  NewBB = hipe_bb:code_update(BB, NewCode),
  NewCfg = hipe_icode_cfg:bb_add(Cfg, Label, NewBB),
  update_var_types_bbs(Left, NewCfg, Map);
update_var_types_bbs([], Cfg, _Map) ->
  Cfg.

update_var_types_instr_list([I|Left], Map, Acc)->
  NewI = update_var_types_instr(I, Map),
  update_var_types_instr_list(Left, Map, [NewI|Acc]);
update_var_types_instr_list([], _Map, Acc)->
  lists:reverse(Acc).

update_var_types_instr(I, Map)->
  Def = defines(I),
  Use = uses(I),
  Fun = fun update_var_types/2,
  hipe_icode:subst([{Var, Fun(Var, Map)} || Var <- Def++Use], I).

update_var_types(Var, Map) ->
  case hipe_icode:is_annotated_var(Var) of
    true ->
      Ann = hipe_icode:var_annotation(Var),
      Type = evaluate_type(lookup_type(Ann, Map), Map),
      hipe_icode:annotate_var(Var, Type);
    false ->
      Var
  end.
