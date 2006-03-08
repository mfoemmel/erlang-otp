%%%-------------------------------------------------------------------
%%% File    : hipe_icode_fp.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description : One pass analysis to find floating point values. 
%%%               Mapping to fp variables and creation of fp ebbs.
%%%
%%% Created : 23 Apr 2003 by Tobias Lindahl <tobiasl@it.uu.se>
%%%
%%% CVS      :
%%%              $Author: tobiasl $
%%%              $Date: 2006/02/16 10:00:01 $
%%%              $Revision: 1.30 $
%%%-------------------------------------------------------------------

-module(hipe_icode_fp).

-export([cfg/1]).

-include("hipe_icode.hrl").

-record(state, {info, block_map, edge_map, cfg}).

cfg(Cfg) ->
  %%hipe_icode_cfg:pp(Cfg),
  NewCfg = annotate_fclearerror(Cfg),
  State = new_state(NewCfg),
  NewState = place_fp_blocks(State),
  %%hipe_icode_cfg:pp(state__cfg(NewState)),
  NewState2 = finalize(NewState),
  NewCfg1 = state__cfg(NewState2),
  %%hipe_icode_cfg:pp(NewCfg1),
  NewCfg2 = unannotate_fclearerror(NewCfg1),

  NewCfg2.

%%____________________________________________________________
%%
%% Annotate fclearerror with information of the fail label of the
%% corresponding fcheckerror.
%%

annotate_fclearerror(Cfg) ->
  Labels = hipe_icode_cfg:reverse_postorder(Cfg),
  annotate_fclearerror(Labels, Cfg).

annotate_fclearerror([Label|Left], Cfg) ->
  BB = hipe_icode_cfg:bb(Cfg, Label),
  Code = hipe_bb:code(BB),
  NewCode = annotate_fclearerror1(Code, Label, Cfg, []),
  NewBB = hipe_bb:code_update(BB, NewCode),
  NewCfg = hipe_icode_cfg:bb_add(Cfg, Label, NewBB),
  annotate_fclearerror(Left, NewCfg);
annotate_fclearerror([], Cfg) ->
  Cfg.

annotate_fclearerror1([I|Left], Label, Cfg, Acc) ->
  case I of
    #call{} ->
      case hipe_icode:call_fun(I) of
	fclearerror ->
	  Fail = lookahead_for_fcheckerror(Left, Label, Cfg),
	  NewI = hipe_icode:call_fun_update(I, {fclearerror, Fail}),
	  annotate_fclearerror1(Left, Label, Cfg, [NewI|Acc]);
	_ ->
	  annotate_fclearerror1(Left, Label, Cfg, [I|Acc])
      end;
    _ ->
      annotate_fclearerror1(Left, Label, Cfg, [I|Acc])
  end;
annotate_fclearerror1([], _Label, _Cfg, Acc) ->
  lists:reverse(Acc).

lookahead_for_fcheckerror([I|Left], Label, Cfg) ->
  case I of
    #call{} ->
      case hipe_icode:call_fun(I) of
	fcheckerror ->
	  hipe_icode:call_fail_label(I);
	_ ->
	  lookahead_for_fcheckerror(Left, Label, Cfg)
      end;
    _ ->
       lookahead_for_fcheckerror(Left, Label, Cfg)
  end;
lookahead_for_fcheckerror([], Label, Cfg) ->
  case hipe_icode_cfg:succ(hipe_icode_cfg:succ_map(Cfg), Label) of
    [] -> exit("Unterminated fp ebb");
    SuccList ->
      Succ = hd(SuccList),
      Code = hipe_bb:code(hipe_icode_cfg:bb(Cfg, Label)),
      lookahead_for_fcheckerror(Code, Succ, Cfg)
  end.

unannotate_fclearerror(Cfg) ->
  Labels = hipe_icode_cfg:reverse_postorder(Cfg),
  unannotate_fclearerror(Labels, Cfg).

unannotate_fclearerror([Label|Left], Cfg) ->
  BB = hipe_icode_cfg:bb(Cfg, Label),
  Code = hipe_bb:code(BB),
  NewCode = unannotate_fclearerror1(Code, []),
  NewBB = hipe_bb:code_update(BB, NewCode),
  NewCfg = hipe_icode_cfg:bb_add(Cfg, Label, NewBB),
  unannotate_fclearerror(Left, NewCfg);
unannotate_fclearerror([], Cfg) ->
  Cfg.

unannotate_fclearerror1([I|Left], Acc) ->
  case I of
    #call{} ->
      case hipe_icode:call_fun(I) of
	{fclearerror, _Fail} ->
	  NewI = hipe_icode:call_fun_update(I, fclearerror),
	  unannotate_fclearerror1(Left, [NewI|Acc]);
	_ ->
	  unannotate_fclearerror1(Left, [I|Acc])
      end;
    _ ->
      unannotate_fclearerror1(Left, [I|Acc])
  end;
unannotate_fclearerror1([], Acc) ->
  lists:reverse(Acc).


%%____________________________________________________________
%%
%% Make float ebbs
%%

place_fp_blocks(State)->
  WorkList = new_worklist(State),
  transform_block(WorkList, State).

transform_block(WorkList, State)->
  case get_work(WorkList) of
    none ->
      State;
    {Label, NewWorkList} ->      
      %%io:format("Handling ~w \n", [Label]),
      BB = state__bb(State, Label),
      Code = hipe_bb:code(BB),
      NofPreds = length(state__pred(State, Label)),
      Map = state__map(State, Label),
      FilteredMap = filter_map(Map, NofPreds),
      %%io:format("Label: ~w\nPhiMap: ~p\nFilteredMap ~p\n", 
      %%	[Label, gb_trees:to_list(Map), gb_trees:to_list(FilteredMap)]),
      {Prelude, NewFilteredMap} = do_prelude(FilteredMap),
      {NewMap, NewCode} = 
	transform_instrs(Code, Map, NewFilteredMap, []),
      NewBB = hipe_bb:code_update(BB, Prelude++NewCode),
      NewState = state__bb_add(State, Label, NewBB),
      case state__map_update(NewState, Label, NewMap) of
	fixpoint ->
	  transform_block(NewWorkList, NewState);
	NewState1 ->
	  Succ = state__succ(NewState1, Label),
	  NewWorkList1 = add_work(NewWorkList, Succ),
	  transform_block(NewWorkList1, NewState1)
      end
  end.


transform_instrs([I|Left], PhiMap, Map, Acc)->
  Defines = defines(I),
  NewMap = delete_all(Defines, Map),
  NewPhiMap = delete_all(Defines, PhiMap),
  
  case I of
    #phi{} ->
      Uses = uses(I),
      case [X||X<-Uses,lookup(X, PhiMap)/=none] of
	[] ->
	  %% No ordinary variables from the argument has been untagged.
	  transform_instrs(Left, NewPhiMap, NewMap, [I|Acc]);
	Uses ->
	  %% All arguments are untagged. Let's untag the destination.
	  Dst = hipe_icode:phi_dst(I),
	  NewDst = hipe_icode:mk_new_fvar(),
	  NewMap1 = gb_trees:enter(Dst, NewDst, NewMap),
	  NewI = subst_phi_uncond(I, NewDst, PhiMap),
	  transform_instrs(Left, NewPhiMap, NewMap1, [NewI|Acc]);
	_ ->
	  %% Some arguments are untagged. Keep the destination.
	  Dst = hipe_icode:phi_dst(I),
	  NewI = subst_phi(I, Dst, PhiMap),
	  transform_instrs(Left, NewPhiMap, NewMap, [NewI|Acc])
      end;
    #call{} ->
      case hipe_icode:call_fun(I) of
	X when X == unsafe_untag_float; X == conv_to_float ->
	  [Dst] = defines(I),
	  case uses(I) of
	    [] -> %% Constant
	      transform_instrs(Left, NewPhiMap, NewMap, [I|Acc]);
	    [Src] ->
	      case lookup(Src, Map) of
		none ->
		  NewMap1 = gb_trees:enter(Src, {assigned, Dst}, NewMap),
		  transform_instrs(Left, NewPhiMap, NewMap1, [I|Acc]);
		Dst ->
		  %% This is the instruction that untagged the variable.
		  %% Use old maps.
		  transform_instrs(Left, NewPhiMap, Map, [I|Acc]);
		FVar -> 
		  %% The variable was already untagged. 
		  %% This instruction can be changed to a fmove.
		  NewI = hipe_icode:mk_fmove(Dst, FVar),
		  case hipe_icode:call_continuation(I) of
		    [] ->
		      transform_instrs(Left,NewPhiMap,NewMap,[NewI|Acc]);
		    ContLbl ->
		      Goto = hipe_icode:mk_goto(ContLbl),
		      transform_instrs(Left, NewPhiMap, NewMap, 
				       [Goto, NewI|Acc])
		  end
	      end
	  end;
	unsafe_tag_float ->
	  [Dst] = defines(I),
	  [Src] = uses(I),
	  NewMap1 = gb_trees:enter(Dst, {assigned, Src}, NewMap),
	  transform_instrs(Left, NewPhiMap, NewMap1,[I|Acc]);
	_ ->
	  {NewMap1, NewAcc} = check_for_fop_candidates(I, NewMap, Acc),
	  transform_instrs(Left, NewPhiMap, NewMap1, NewAcc)
      end;
    _ ->
      NewIns = handle_untagged_arguments(I, NewMap),
      transform_instrs(Left, NewPhiMap, NewMap, NewIns ++ Acc)
  end;
transform_instrs([], _PhiMap, Map, Acc) ->
  {Map, lists:reverse(Acc)}.

check_for_fop_candidates(I, Map, Acc)->
  case is_fop_cand(I) of
    false ->
      NewIs = handle_untagged_arguments(I, Map),
      {Map, NewIs ++ Acc};
    true ->
      Fail = hipe_icode:call_fail_label(I),
      Cont = hipe_icode:call_continuation(I),
      Op = fun_to_fop(hipe_icode:call_fun(I)), 
      case Fail of
	[] ->
	  Args = args(I),
	  ConstArgs = [X || X <- Args, hipe_icode:is_const(X)],
	  case catch [float(hipe_icode:const_value(X)) || X <- ConstArgs] of
	    {'EXIT', _} -> 
	      %% This instruction will fail at runtime. The warning
	      %% should already have happened in hipe_icode_type.
	      NewIs = handle_untagged_arguments(I, Map),
	      {Map, NewIs ++ Acc};
	    _ ->
	      %%io:format("Changing ~w to ~w\n", [hipe_icode:call_fun(I), Op]),
	      Uses = uses(I),
	      Defines = defines(I),
	      Convs = [X||X <- remove_duplicates(Uses), lookup(X, Map)==none],
	      NewMap0 = add_new_bindings_assigned(Convs, Map),
	      NewMap = add_new_bindings_unassigned(Defines, NewMap0),
	      ConvIns = get_conv_instrs(Convs, NewMap),
	      NewI = hipe_icode:mk_primop(lookup_list(Defines, NewMap), Op,
					  lookup_list_keep_consts(Args,NewMap),
					  Cont, Fail),
	      NewI2 = conv_consts(ConstArgs, NewI),
	      {NewMap, [NewI2|ConvIns]++Acc}
	  end;
	_ -> %% Bailing out! Can't handle instructions in catches (yet).
	  NewIs = handle_untagged_arguments(I, Map),
	  {Map, NewIs ++ Acc}
      end
  end.


%% If this is an instruction that needs to operate on tagged values,
%% which currently are untagged, we must tag the values and perhaps
%% end the fp ebb.

handle_untagged_arguments(I, Map)->
  case lists:filter(fun(X)-> must_be_tagged(X, Map)end, uses(I)) of
    [] ->
      [I];
    Tag ->
      TagIntrs = 
	[hipe_icode:mk_primop([Dst], unsafe_tag_float, 
			      [gb_trees:get(Dst, Map)])||
	  Dst<-Tag],
      [I|TagIntrs]
  end.

%% Add phi nodes for untagged fp values.

do_prelude(Map)->  
  case gb_trees:lookup(phi, Map) of
    none ->
      {[], Map};
    {value, List} ->
      %%io:format("Adding phi: ~w\n", [List]),
      Fun = fun({FVar, Bindings}, Acc) -> 
		[hipe_icode:mk_phi(FVar, Bindings)|Acc]end,
      {lists:foldl(Fun, [], List), gb_trees:delete(phi, Map)}
  end.

split_code(Code)->
  split_code(Code, []).

split_code([I|[]], Acc) ->
  {lists:reverse(Acc), I};
split_code([I|Left], Acc)->
  split_code(Left, [I|Acc]).


%% When all code is mapped to fp instructions we must make sure that
%% the fp ebb information goin in to each block is the same as the
%% information coming out of each predecessor. Otherwise, we must add
%% a block in between.

finalize(State)->
  Worklist = new_worklist(State),
  NewState = place_error_handling(Worklist, State),
  Edges = needs_fcheckerror(NewState),
  finalize(Edges, NewState).

finalize([{From, To}|Left], State) ->
  NewState = add_fp_ebb_fixup(From, To, State),
  finalize(Left, NewState);
finalize([], State) ->
  State.

needs_fcheckerror(State) ->
  Cfg = state__cfg(State),
  Labels = hipe_icode_cfg:labels(Cfg),
  needs_fcheckerror(Labels, State, []).

needs_fcheckerror([Label|Left], State, Acc) ->
  case state__get_in_block_in(State, Label) of
    {true, _} ->
      needs_fcheckerror(Left, State, Acc);
    false ->
      Pred = state__pred(State, Label),
      case [X || X <- Pred, state__get_in_block_out(State, X) /= false] of
	[] ->
	  needs_fcheckerror(Left, State, Acc);
	NeedsFcheck ->	  
	  case length(Pred) == length(NeedsFcheck) of
	    true ->
	      %% All edges needs fcheckerror. Add this to the
	      %% beginning of the block instead.
	      needs_fcheckerror(Left, State, [{none, Label}|Acc]);
	    false ->
	      Edges = [{X, Label} || X <- NeedsFcheck],
	      needs_fcheckerror(Left, State, Edges ++ Acc)
	  end
      end
  end;
needs_fcheckerror([], _State, Acc) ->
  Acc.

add_fp_ebb_fixup(From, To, State) when From == none ->
  %% Add the fcheckerror to the start of the block.
  BB = state__bb(State, To),
  Code = hipe_bb:code(BB),
  Phis = lists:takewhile(fun(X)-> hipe_icode:is_phi(X)end,Code),
  TailCode = lists:dropwhile(fun(X)-> hipe_icode:is_phi(X)end,Code),
  FC = hipe_icode:mk_primop([], fcheckerror, []),
  NewCode = Phis ++ [FC|TailCode],
  state__bb_add(State, To, hipe_bb:code_update(BB, NewCode));
add_fp_ebb_fixup(From, To, State) ->
  FCCode = [hipe_icode:mk_primop([], fcheckerror, [], To, [])],
  FCBB = hipe_bb:mk_bb(FCCode),
  FCLabel = hipe_icode:label_name(hipe_icode:mk_new_label()),
  NewState = state__bb_add(State, FCLabel, FCBB),
  NewState1 = state__redirect(NewState, From, To, FCLabel),
  ToBB = state__bb(NewState, To),
  ToCode = hipe_bb:code(ToBB),
  NewToCode = redirect_phis(ToCode, From, FCLabel),
  NewToBB = hipe_bb:code_update(ToBB, NewToCode),
  state__bb_add(NewState1, To, NewToBB).

redirect_phis(Code, OldFrom, NewFrom)->
  redirect_phis(Code, OldFrom, NewFrom, []).

redirect_phis([I|Left], OldFrom, NewFrom, Acc)->
  case I of
    #phi{} ->
      NewI = hipe_icode:phi_redirect_pred(I, OldFrom, NewFrom),
      redirect_phis(Left, OldFrom, NewFrom, [NewI|Acc]);
    _ ->
      lists:reverse(Acc)++[I|Left]
  end;
redirect_phis([], _OldFrom, _NewFrom, Acc) ->
  lists:reverse(Acc).

subst_phi(I, Dst, Map) ->
  ArgList = subst_phi_uses0(hipe_icode:phi_arglist(I), Map, []),
  hipe_icode:mk_phi(Dst, ArgList).

subst_phi_uses0([{Pred, Var}|Left], Map, Acc) ->
  case gb_trees:lookup(Var, Map) of
    {value, List} -> 
      case lists:keysearch(Pred, 1, List) of
	{value, {Pred, {assigned, _NewVar}}} -> 
	  %% The variable is untagged, but it has been assigned. Keep it!
	  subst_phi_uses0(Left, Map, [{Pred, Var}|Acc]);
	{value, {Pred, NewVar}}-> 
	  %% The variable is untagged and it has never been assigned as tagged.
	  subst_phi_uses0(Left, Map, [{Pred, NewVar}|Acc]);
	false ->
	  %% The variable is not untagged.
	  subst_phi_uses0(Left, Map, [{Pred, Var}|Acc])
      end;
    none ->
      %% The variable is not untagged.
      subst_phi_uses0(Left, Map, [{Pred, Var}|Acc])
  end;
subst_phi_uses0([], _Map, Acc) ->
  Acc.

subst_phi_uncond(I, Dst, Map) ->
  ArgList = subst_phi_uses_uncond0(hipe_icode:phi_arglist(I), Map, []),
  hipe_icode:mk_phi(Dst, ArgList).

subst_phi_uses_uncond0([{Pred, Var}|Left], Map, Acc) ->
  case gb_trees:lookup(Var, Map) of
    {value, List} -> 
      case lists:keysearch(Pred, 1, List) of
	{value, {Pred, {assigned, NewVar}}} -> 
	  %% The variable is untagged!
	  subst_phi_uses_uncond0(Left, Map, [{Pred, NewVar}|Acc]);
	{value, {Pred, NewVar}}-> 
	  %% The variable is untagged!
	  subst_phi_uses_uncond0(Left, Map, [{Pred, NewVar}|Acc]);
	false ->
	  %% The variable is not untagged.
	  subst_phi_uses_uncond0(Left, Map, [{Pred, Var}|Acc])
      end;
    none ->
      %% The variable is not untagged.
      subst_phi_uses_uncond0(Left, Map, [{Pred, Var}|Acc])
  end;
subst_phi_uses_uncond0([], _Map, Acc) ->
  Acc.


place_error_handling(WorkList, State) ->
  case get_work(WorkList) of
    none ->
      State;
    {Label, NewWorkList} ->      
      BB = state__bb(State, Label),
      Code = hipe_bb:code(BB),
      case state__join_in_block(State, Label) of
	fixpoint ->
	  place_error_handling(NewWorkList, State);
	{NewState, NewInBlock} ->
	  {NewCode1, InBlockOut} = place_error(Code, NewInBlock, []),
	  Succ = state__succ(NewState, Label),
	  NewCode2 = handle_unchecked_end(Succ, NewCode1, InBlockOut),
	  NewBB = hipe_bb:code_update(BB, NewCode2),
	  NewState1 = state__bb_add(NewState, Label, NewBB),
	  NewState2 = state__in_block_out_update(NewState1, Label, InBlockOut),
	  NewWorkList1 = add_work(NewWorkList, Succ),
	  place_error_handling(NewWorkList1, NewState2)
      end
  end.

place_error([I|Left], InBlock, Acc) ->
  case I of
    #call{} ->
      case hipe_icode:call_fun(I) of
	X when X == fp_add; X == fp_sub; 
	       X == fp_mul; X == fp_div; X == fnegate ->
	  case InBlock of
	    false ->
	      Clear = hipe_icode:mk_primop([], {fclearerror, []}, []),
	      place_error(Left, {true, []}, [I, Clear|Acc]);
	    {true, _} ->
	      place_error(Left, InBlock, [I|Acc])
	  end;
	unsafe_tag_float ->
	  case InBlock of
	    {true, Fail} ->
	      Check = hipe_icode:mk_primop([], fcheckerror, [], [], Fail),
	      place_error(Left, false, [I, Check|Acc]);
	    false ->
	      place_error(Left, InBlock, [I|Acc])
	  end;
	{fclearerror, Fail} ->
	  case InBlock of
	    {true, Fail} ->
	      %% We can remove this fclearerror!
	      case hipe_icode:call_continuation(I) of
		[] ->
		  place_error(Left, InBlock, Acc);
		Cont ->
		  place_error(Left, InBlock, [hipe_icode:mk_goto(Cont)|Acc])
	      end;
	    {true, _OtherFail} ->
	      %% TODO: This can be handled but it requires breaking up
	      %% the BB in two. Currently this should not happen.
	      exit("Starting fp ebb with different fail label");
	    false ->
	      place_error(Left, {true, Fail}, [I|Acc])
	  end;
	fcheckerror ->
	  case {true, hipe_icode:call_fail_label(I)} of
	    InBlock ->
	      %% No problem
	      place_error(Left, false, [I|Acc]);
	    NewInblock ->
	      exit({"Fcheckerror has the wrong fail label",
		    InBlock, NewInblock})
	  end;
	X when X == conv_to_float; X == unsafe_untag_float ->
	  place_error(Left, InBlock, [I|Acc]);
	_Other ->
	  case hipe_icode_primops:fails(hipe_icode:call_fun(I)) of
	    false ->
	      place_error(Left, InBlock, [I|Acc]);
	    true ->
	      case InBlock of
		{true, Fail} ->
		  Check = hipe_icode:mk_primop([], fcheckerror, [], [], Fail),
		  place_error(Left, false, [I, Check|Acc]);
		false ->
		  place_error(Left, InBlock, [I|Acc])
	      end
	  end
      end;
    #fail{} ->
      place_error_1(I, Left, InBlock, Acc);
    #return{} ->
      place_error_1(I, Left, InBlock, Acc);
    #enter{} ->
      place_error_1(I, Left, InBlock, Acc);
    Other ->
      case instr_allowed_in_fp_ebb(Other) of
	true ->
	  place_error(Left, InBlock, [I|Acc]);
	false ->
	  case InBlock of
	    {true, []} ->
	      Check = hipe_icode:mk_primop([], fcheckerror, []),
	      place_error(Left, false, [I, Check|Acc]);
	    {true, _} ->
	      exit({"Illegal instruction in caught fp ebb", I});
	    false ->
	      place_error(Left, InBlock, [I|Acc])
	  end
      end
  end;
place_error([], InBlock, Acc) ->
  {lists:reverse(Acc), InBlock}.

place_error_1(I, Left, InBlock, Acc) ->
  case InBlock of
    {true, []} ->
      Check = hipe_icode:mk_primop([], fcheckerror, []),
      place_error(Left, false, [I,Check|Acc]);
    {true, _} ->
      exit({"End of control flow in caught fp ebb", I});
    false ->
      place_error(Left, InBlock, [I|Acc])
  end.

%% If the block has no successors and we still are in a fp ebb we must
%% end it to make sure we don't have any unchecked fp exceptions.

handle_unchecked_end(Succ, Code, InBlock) ->
  case Succ of
    [] ->
      case InBlock of
	{true, []} ->
	  {TopCode, Last} = split_code(Code),
	  NewI = hipe_icode:mk_primop([], fcheckerror, []),
	  TopCode++[NewI, Last];
	false ->
	  Code
      end;
    _ ->
      Code
  end.      

instr_allowed_in_fp_ebb(Instr) ->
  case Instr of
    #comment{} -> true;
    #fmove{} -> true;
    #goto{} -> true;
    #'if'{} -> true;
    #move{} -> true;
    #phi{} -> true;
    #begin_handler{} -> true;
    #switch_tuple_arity{} -> true;
    #switch_val{} -> true;
    #type{} -> true;
    _ -> false
  end.


%%____________________________________________________________
%%
%% Help functions
%%

%% ------------------------------------------------------------ 
%% Handling the gb_tree

empty() ->
  gb_trees:empty().

delete_all([Key|Left], Tree) ->
  delete_all(Left, gb_trees:delete_any(Key, Tree));
delete_all([], Tree) ->
  Tree.

lookup_list(List, Info) ->
  lookup_list(List, fun lookup/2, Info, []).

lookup_list([H|T], Fun, Info, Acc) ->
  lookup_list(T, Fun, Info, [Fun(H, Info)|Acc]);
lookup_list([], _,  _, Acc) ->
  lists:reverse(Acc).

lookup(Key, Tree) ->
  case hipe_icode:is_const(Key) of
    %% This can be true if the same constant has been
    %% untagged more than once
    true -> none;
    false ->
      case gb_trees:lookup(Key, Tree) of
	none -> none;
	{value, {assigned, Val}} -> Val;
	{value, Val} -> Val
      end
  end.

lookup_list_keep_consts(List, Info) ->
  lookup_list(List, fun lookup_keep_consts/2, Info, []).

lookup_keep_consts(Key, Tree) ->
  case hipe_icode:is_const(Key) of
    true -> Key;
    false ->
      case gb_trees:lookup(Key, Tree) of
	none -> none;
	{value, {assigned, Val}} -> Val;
	{value, Val} -> Val
      end
  end.

get_type(Var) ->
  case hipe_icode:is_const(Var) of
    true -> erl_types:t_from_term(hipe_icode:const_value(Var));
    false ->
      case hipe_icode:is_annotated_var(Var) of
	true -> hipe_icode:var_annotation(Var)
%%%     false -> erl_types:t_any()
      end
  end.

%% ------------------------------------------------------------ 
%% Handling the map from variables to fp-variables

join_maps(Preds, BlockMap) ->
  join_maps(Preds, BlockMap, empty()).

join_maps([Pred|Left], BlockMap, Map) ->
  case gb_trees:lookup(Pred, BlockMap) of
    none ->
      %%join_maps(Left, BlockMap, Map);
      %% All predecessors have not been handled. Use empty map.
      empty();
    {value, OldMap} ->
      NewMap = join_maps0(gb_trees:to_list(OldMap), Pred, Map),
      join_maps(Left, BlockMap, NewMap)
  end;
join_maps([], _, Map) ->
  Map.

join_maps0([{phi, _}|Tail], Pred,  Map) ->
  join_maps0(Tail, Pred, Map);
join_maps0([{Var, FVar}|Tail], Pred,  Map) ->
  case gb_trees:lookup(Var, Map) of
    none ->
      join_maps0(Tail, Pred, gb_trees:enter(Var, [{Pred, FVar}], Map));
    {value, List} ->
      case lists:keysearch(Pred, 1, List) of
	{value, {Pred, FVar}} ->
	  %% No problem.
	  join_maps0(Tail, Pred, Map);
	{value, _}->
	  exit('New binding to same variable');
	false ->
	  join_maps0(Tail, Pred, gb_trees:update(Var, [{Pred, FVar}|List], Map))
      end
  end;
join_maps0([], _, Map) ->
  Map.

filter_map(Map, NofPreds) ->
  filter_map(gb_trees:to_list(Map), NofPreds, Map).

filter_map([{Var, Bindings}|Left], NofPreds, Map) ->
  case length(Bindings) == NofPreds of
    true ->
      case all_args_equal(Bindings) of
	true ->
	  {_, FVar} = hd(Bindings),
	  filter_map(Left, NofPreds, gb_trees:update(Var, FVar, Map));
	false ->
	  PhiDst = hipe_icode:mk_new_fvar(),
	  PhiArgs = strip_of_assigned(Bindings),
	  NewMap =
	    case gb_trees:lookup(phi, Map) of
	      none ->
		gb_trees:insert(phi, [{PhiDst, PhiArgs}], Map);
	      {value, Val} ->
		gb_trees:update(phi, [{PhiDst, PhiArgs}|Val], Map)
	    end,
	  filter_map(Left, NofPreds, gb_trees:update(Var, PhiDst, NewMap))
      end;
    false ->
      filter_map(Left, NofPreds, gb_trees:delete(Var, Map))
  end;
filter_map([], _NofPreds, Map) ->
  Map.

%% all_args_equal returns true if the mapping for a variable is the
%% same from all predecessors, i.e., we do not need a phi-node.

all_args_equal([{_, FVar}|Left]) ->
  all_args_equal(Left, FVar).

all_args_equal([{_, FVar1}|Left], FVar1) ->
  all_args_equal(Left, FVar1);
all_args_equal([], _) ->
  true;
all_args_equal(_, _) ->
  false.


%% We differentiate between values that have been assigned as
%% tagged variables and those that got a 'virtual' binding.

add_new_bindings_unassigned([Var|Left], Map) ->
  FVar = hipe_icode:mk_new_fvar(),
  add_new_bindings_unassigned(Left, gb_trees:insert(Var, FVar, Map));
add_new_bindings_unassigned([], Map) ->
  Map.

add_new_bindings_assigned([Var|Left], Map) ->
  case lookup(Var, Map) of
    none ->
      FVar = hipe_icode:mk_new_fvar(),
      NewMap = gb_trees:insert(Var, {assigned, FVar}, Map),
      add_new_bindings_assigned(Left, NewMap);
    _ ->
      add_new_bindings_assigned(Left, Map)
  end;
add_new_bindings_assigned([], Map) ->
  Map.

strip_of_assigned(List) ->
  strip_of_assigned(List, []).

strip_of_assigned([{Pred, {assigned, Val}}|Left], Acc) ->
  strip_of_assigned(Left, [{Pred, Val}|Acc]);
strip_of_assigned([Tuple|Left], Acc) ->
  strip_of_assigned(Left, [Tuple|Acc]);
strip_of_assigned([], Acc) ->
  Acc.

%% ------------------------------------------------------------ 
%% Help functions for the transformation from ordinary instruction to
%% fp-instruction

is_fop_cand(I) ->
  case hipe_icode:call_fun(I) of
    '/' -> true;
    Fun ->
      case fun_to_fop(Fun) of
	false -> false;
	_ -> any_is_float(args(I))
      end
  end.

any_is_float([Var|Left]) ->
  case erl_types:t_is_float(get_type(Var)) of
    true -> true;
    false -> any_is_float(Left)
  end;
any_is_float([]) ->
  false.

remove_duplicates(List) ->
  remove_duplicates(List, []).
remove_duplicates([X|Left], Acc) ->
  case lists:member(X, Acc) of
    true ->
      remove_duplicates(Left, Acc);
    false ->
      remove_duplicates(Left, [X|Acc])
  end;
remove_duplicates([], Acc) ->
  Acc.

fun_to_fop(Fun) ->
  case Fun of
    '+' -> fp_add;
    '-' -> fp_sub;
    '*' -> fp_mul;
    '/' -> fp_div;
    _ ->false
  end.


%% If there is a tagged version of this variable available we don't
%% have to tag the untagged version.

must_be_tagged(Var, Map) ->
  case gb_trees:lookup(Var, Map) of
    none -> false;
    {value, {assigned, _}} -> false; 
    {value, Val} -> 
      case hipe_icode:is_fvar(Val) of
	true->
	  true;
	false ->
	  false
      end
  end.


%% Converting to floating point variables

get_conv_instrs(Vars, Map) ->
  get_conv_instrs(Vars, Map, []).

get_conv_instrs([Var|Left], Map, Acc) ->
  {_, Dst} = gb_trees:get(Var, Map),
  NewI = 
    case erl_types:t_is_float(get_type(Var)) of
      true ->
	[hipe_icode:mk_primop([Dst],unsafe_untag_float,[Var])];
      false ->
	[hipe_icode:mk_primop([Dst],conv_to_float,[Var])] 
    end,
  get_conv_instrs(Left, Map, NewI++Acc);
get_conv_instrs([], _, Acc) ->
  Acc.


conv_consts(ConstArgs, I) ->
  conv_consts(ConstArgs, I, []).

conv_consts([Const|Left], I, Subst) ->
  NewConst = hipe_icode:mk_const(float(hipe_icode:const_value(Const))),
  conv_consts(Left, I, [{Const, NewConst}|Subst]);
conv_consts([], I, Subst) ->
  hipe_icode:subst_uses(Subst, I).
  


%% ------------------------------------------------------------ 
%% Defines and uses

defines(I)->
  hipe_icode:defines(I).

args(I)->
  hipe_icode:args(I).

uses(I)->
  hipe_icode:uses(I).

%% _________________________________________________________________
%%
%% Handling the state
%%

new_state(Cfg)->
  Start = hipe_icode_cfg:start_label(Cfg),  
  BlockMap = gb_trees:insert({inblock, Start}, false, empty()),
  EdgeMap = gb_trees:empty(),
  #state{cfg=Cfg, block_map=BlockMap, edge_map=EdgeMap}.

state__cfg(#state{cfg=Cfg})->
  Cfg.

state__succ(#state{cfg=Cfg}, Label)->
  hipe_icode_cfg:succ(hipe_icode_cfg:succ_map(Cfg), Label).

state__pred(#state{cfg=Cfg}, Label)->
  hipe_icode_cfg:pred(hipe_icode_cfg:pred_map(Cfg), Label).

state__redirect(S=#state{cfg=Cfg}, From, ToOld, ToNew)->
  NewCfg = hipe_icode_cfg:redirect(Cfg, From, ToOld, ToNew),
  S#state{cfg=NewCfg}.

state__bb(#state{cfg=Cfg}, Label)->
  hipe_icode_cfg:bb(Cfg, Label).
  
state__bb_add(S=#state{cfg=Cfg}, Label, BB)->
  NewCfg = hipe_icode_cfg:bb_add(Cfg, Label, BB),
  S#state{cfg=NewCfg}.

state__map(S=#state{block_map=BM}, Label)->
  join_maps(state__pred(S, Label), BM).

state__map_update(S=#state{block_map=BM}, Label, Map)->
  MapChanged = 
    case gb_trees:lookup(Label, BM) of
      {value, Map1} -> not match(Map1, Map);
      none -> true
    end,
  case MapChanged of
    true ->
      NewBM = gb_trees:enter(Label, Map, BM),
      S#state{block_map = NewBM};
    false ->
      fixpoint
  end.

state__join_in_block(S=#state{edge_map = Map}, Label)->
  Pred = state__pred(S, Label),
  Edges = [{X, Label} || X <- Pred],
  NewInBlock = join_in_block([gb_trees:lookup(X, Map) || X <- Edges]),
  case gb_trees:lookup({inblock_in, Label}, Map) of
    none ->
      NewMap = gb_trees:insert({inblock_in, Label}, NewInBlock, Map),
      {S#state{edge_map = NewMap}, NewInBlock};
    {value, NewInBlock} ->
      fixpoint;
    _Other ->
      NewMap = gb_trees:update({inblock_in, Label}, NewInBlock, Map),
      {S#state{edge_map = NewMap}, NewInBlock}
  end.

state__in_block_out_update(S=#state{edge_map = Map}, Label, NewInBlock)->
  Succ = state__succ(S, Label),
  Edges = [{Label, X} || X <- Succ],
  NewMap = update_edges(Edges, NewInBlock, Map),
  NewMap1 = gb_trees:enter({inblock_out, Label}, NewInBlock, NewMap),
  S#state{edge_map = NewMap1}.

update_edges([Edge|Left], NewInBlock, Map)->
  NewMap = gb_trees:enter(Edge, NewInBlock, Map),
  update_edges(Left, NewInBlock, NewMap);
update_edges([], _NewInBlock, NewMap) ->
  NewMap.

join_in_block([])->
  false;
join_in_block([none|_])->
  false;
join_in_block([{value, InBlock}|Left]) ->
  join_in_block(Left, InBlock).

join_in_block([none|_], _Current)->
  false;
join_in_block([{value, InBlock}|Left], Current) ->
  if Current == InBlock -> join_in_block(Left, Current);
     Current == false -> false;
     InBlock == false -> false;
     true -> exit("Basic block is in two different fp ebb:s")
  end;
join_in_block([], Current) ->
  Current.
	

state__get_in_block_in(#state{edge_map=Map}, Label)->
  gb_trees:get({inblock_in, Label}, Map).

state__get_in_block_out(#state{edge_map=Map}, Label)->
  gb_trees:get({inblock_out, Label}, Map).


new_worklist(#state{cfg=Cfg})->
  Start = hipe_icode_cfg:start_label(Cfg),
  {[Start], [], gb_sets:insert(Start, gb_sets:empty())}.

get_work({[Label|Left], List, Set})->
  {Label, {Left, List, gb_sets:delete(Label, Set)}};
get_work({[], [], _Set}) ->
  none;
get_work({[], List, Set}) ->
  get_work({lists:reverse(List), [], Set}).

add_work({List1, List2, Set}, [Label|Left])->
  case gb_sets:is_member(Label, Set) of
    true -> 
      add_work({List1, List2, Set}, Left);
    false -> 
      %%io:format("Added work: ~w\n", [Label]),
      NewSet = gb_sets:insert(Label, Set),
      add_work({List1, [Label|List2], NewSet}, Left)
  end;
add_work(WorkList, []) ->
  WorkList.


match(Tree1, Tree2)->
  match_1(gb_trees:to_list(Tree1), Tree2) andalso 
    match_1(gb_trees:to_list(Tree2), Tree1).

match_1([{Key, Val}|Left], Tree2)->
  case gb_trees:lookup(Key, Tree2) of
    {value, Val} ->
      match_1(Left, Tree2);
    _ -> false
  end;
match_1([], _) ->
  true.
