%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Semi-local copy propagation, constant propagation, constant folding,
%% and dead code removal.
%%
%% Works on extended basic blocks. No iteration is done at this point.
%%
%% The environment binds (RTL) variables to:
%%    - constants
%%    - variables
%%

-module(hipe_rtl_prop).
-export([do/1]).

-include("../main/hipe.hrl").

do(CFG) ->
  CFG1 = prop(CFG),
  CFG2 = hipe_rtl_cfg:remove_trivial_bbs(CFG1),
  Liveness = hipe_rtl_liveness:analyze(CFG2),
  EBBs2 = hipe_rtl_ebb:cfg(CFG2),
  CFG3 = dead_code_ebbs(EBBs2, CFG2, Liveness),
  CFG4 = remove_loads(CFG3),
  CFG5 = hipe_rtl_cfg:remove_unreachable_code(CFG4),
  CFG6 = hipe_rtl_cfg:remove_trivial_bbs(CFG5),
  remove_stores(CFG6).

prop(CFG) ->
  Start = hipe_rtl_cfg:start_label(CFG),
  Tree = gb_trees:empty(),
  PredMap = hipe_rtl_cfg:pred_map(CFG),
  Info = fix_point([Start], CFG, PredMap, Tree),
  pass_through([Start], CFG, PredMap, Info, gb_sets:singleton(Start)).

%%
%% Propagate copies and constants for one instruction.
%%

prop_instr(I, Env) ->
  NewEnv = unbind(hipe_rtl:defines(I), Env),
  case hipe_rtl:type(I) of
    move ->
      Srcs = [hipe_rtl:move_src(I)],
      Dsts = [hipe_rtl:move_dst(I)],
      bind_all(Srcs, Dsts, I, NewEnv);
    multimove ->
      Srcs = hipe_rtl:multimove_srclist(I),
      Dsts = hipe_rtl:multimove_dstlist(I),
      bind_all(Srcs, Dsts, I, NewEnv);
    fconv ->
      {I, Env};
    _ ->
      Uses = hipe_rtl:uses(I),
      %% Map = [{U, lookup(U, Env)} || U <- Uses],
      Map = map_all(Uses, Env),
      NewI = hipe_rtl:subst_uses(Map, I),
      eval(NewI, NewEnv)
  end.

map_all([], _Env) ->
  [];
map_all([V|Vs], Env) ->
  [{V, lookup(V, Env)} | map_all(Vs, Env)].

bind_all(Srcs, Dsts, I, Env) ->
  bind_all(Srcs, Dsts, I, Env, Env).

%%
%% We have two envs, Env where we do lookups and
%%                   NewEnv where the new bindings are entered.
bind_all([Src|Srcs], [Dst|Dsts], I, Env, NewEnv) ->
  case hipe_rtl:is_imm(Src) of
    true ->
      bind_all(Srcs, Dsts, I, Env, bind(NewEnv, Dst, Src));
    false ->  %% its a variable
      SrcVal = lookup(Src, Env),
      %% Uncomment this and only constants will be propagated
      %% case hipe_rtl:is_imm(SrcVal) of
      %%   true ->
      NewI = hipe_rtl:subst_uses([{Src, SrcVal}], I),
      bind_all(Srcs, Dsts, NewI, Env, bind(NewEnv, Dst, SrcVal))
      %%  false ->
      %%     bind_all(Srcs, Dsts, I, Env, NewEnv)
      %% end
  end;
bind_all([], [], I, _, Env) ->
  {I, Env}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Evaluate an instruction. Returns {NewI, NewEnv}.
%%

eval(I, Env) ->
  case hipe_rtl:type(I) of
    alu ->
      Src1 = hipe_rtl:alu_src1(I),
      Src2 = hipe_rtl:alu_src2(I),
      case {hipe_rtl:is_imm(Src1), hipe_rtl:is_imm(Src2)} of
	{true, true} ->
	  Val = eval_constant_alu(hipe_rtl:alu_op(I), 
				  hipe_rtl:imm_value(Src1), 
				  hipe_rtl:imm_value(Src2)),
	  Dst = hipe_rtl:alu_dst(I),
	  {hipe_rtl:mk_move(Dst, Val), bind(Env, Dst, Val)};
	_ ->
	  {I, Env}
      end;
    alub ->
      Src1 = hipe_rtl:alub_src1(I),
      Src2 = hipe_rtl:alub_src2(I),
      case {hipe_rtl:is_imm(Src1), hipe_rtl:is_imm(Src2)} of
	{true, true} ->
	  Val1 = hipe_rtl:imm_value(Src1),
	  Val2 = hipe_rtl:imm_value(Src2),
	  Op = hipe_rtl:alub_op(I),
	  Cond = hipe_rtl:alub_cond(I),
	  case hipe_rtl_arch:eval_alub(Op, Cond, Val1, Val2) of
	    {Val3, Bool} ->
	      Src3 = hipe_rtl:mk_imm(Val3),
	      Label =
		case Bool of
		  true -> hipe_rtl:alub_true_label(I);
		  false -> hipe_rtl:alub_false_label(I)
		end,
	      Dst = hipe_rtl:alub_dst(I),
	      {[hipe_rtl:mk_move(Dst, Src3),
		hipe_rtl:mk_goto(Label)],
	       bind(Env, Dst, Src3)};
	    _ ->
	      {I, Env}
	  end;
	_ ->
	  {I, Env}
      end;
    branch ->
      Src1 = hipe_rtl:branch_src1(I),
      Src2 = hipe_rtl:branch_src2(I),
      case {hipe_rtl:is_imm(Src1), hipe_rtl:is_imm(Src2)} of
	{true, true} ->
	  case hipe_rtl_arch:eval_cond(hipe_rtl:branch_cond(I), 
				       hipe_rtl:imm_value(Src1), 
				       hipe_rtl:imm_value(Src2)) of
	    true ->
	      {hipe_rtl:mk_goto(hipe_rtl:branch_true_label(I)), Env};
	    false ->
	      {hipe_rtl:mk_goto(hipe_rtl:branch_false_label(I)), Env}
	  end;
	_ ->
	  {I, Env}
      end;
    enter ->
      {I, new_env()};
    return ->
      {I, new_env()};
    _ ->
      {I, Env}
  end.

eval_constant_alu(Op, Arg1, Arg2) ->
  {Res,_N,_Z,_V,_C} = hipe_rtl_arch:eval_alu(Op, Arg1, Arg2),
  hipe_rtl:mk_imm(Res).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Dead code elimination
%%

dead_code_ebbs([], CFG, _Live) ->
  CFG;
dead_code_ebbs([EBB|EBBs], CFG, Live) ->
  {CFG0, _} = dead_code_ebb(EBB, CFG, Live),
  dead_code_ebbs(EBBs, CFG0, Live).

dead_code_ebb(EBB, CFG, Live) ->
  case hipe_rtl_ebb:type(EBB) of
    node ->
      Succ = hipe_rtl_ebb:node_successors(EBB),
      Lbl = hipe_rtl_ebb:node_label(EBB),
      {CFG0, LiveOut} =
	case Succ of
	  [] ->
	    {CFG,gb_sets:from_list(hipe_rtl_liveness:liveout(Live, Lbl))};
	  _ ->
	    dead_code_succ(Succ, CFG, Live)
	end,
      BB = hipe_rtl_cfg:bb(CFG0, Lbl),
      {NewCode, LiveIn} = dead_code_instrs(hipe_bb:code(BB), LiveOut),
      NewBB = hipe_bb:code_update(BB, NewCode),
      NewCFG = hipe_rtl_cfg:bb_add(CFG0, Lbl, NewBB),
      {NewCFG, LiveIn};
    leaf ->
      Lbl = hipe_rtl_ebb:leaf_next(EBB),
      {CFG, gb_sets:from_list(hipe_rtl_liveness:livein(Live, Lbl))}
  end.

dead_code_succ([], CFG, _Live) ->
  {CFG, gb_sets:new()};
dead_code_succ([EBB|EBBs], CFG, Live) ->
  {CFG0, LiveOut0} = dead_code_ebb(EBB, CFG, Live),
  {NewCFG, LiveOut1} = dead_code_succ(EBBs, CFG0, Live),
  {NewCFG, gb_sets:union(LiveOut0, LiveOut1)}.

dead_code_instrs([], LiveOut) ->
  {[], LiveOut};
dead_code_instrs([I|Is], LiveOut) ->
  {NewIs, LiveOut0} = dead_code_instrs(Is, LiveOut),
  NewI = simplify_mm(I, LiveOut0),
  Def = gb_sets:from_list(hipe_rtl:defines(NewI)),
  Dead = gb_sets:size(gb_sets:intersection(LiveOut0, Def)) == 0,
  case {hipe_rtl:is_safe(NewI), Dead} of
    {true, true} ->
      {NewIs, LiveOut0};
    _ ->
      case dead_move(NewI) of
	true ->
	  {NewIs, LiveOut0};
	false ->
	  Use = gb_sets:from_list(hipe_rtl:uses(I)),
	  LiveIn = gb_sets:union(Use, gb_sets:subtract(LiveOut0, Def)),
	  {[NewI|NewIs], LiveIn}
      end
  end.


%%
%% Identity moves can be safely deleted.
%%

dead_move(X) ->
  (hipe_rtl:is_move(X) andalso
   (hipe_rtl:move_src(X) =:= hipe_rtl:move_dst(X))).

%%
%% Simplify multimoves
%%
simplify_mm(I, LiveOut) ->
  case hipe_rtl:type(I) of
    multimove ->
      {NewSourceList, NewDestList} =
	 simplify_mm(hipe_rtl:multimove_srclist(I),
		     hipe_rtl:multimove_dstlist(I), LiveOut),
      hipe_rtl:mk_multimove(NewDestList, NewSourceList);
    _ ->
      I
  end.

simplify_mm(Ss, Ds, LiveOut) ->
  simplify_mm(Ss, Ds, [], [], LiveOut).

simplify_mm([S|Srcs], [S|Dsts], SAcc, DAcc, LiveOut) ->
  simplify_mm(Srcs, Dsts, SAcc, DAcc, LiveOut);
simplify_mm([S|Srcs], [D|Dsts], SAcc, DAcc, LiveOut) ->
  case gb_sets:is_element(D, LiveOut) of
    true -> %% The dest is live after the instruction.
      simplify_mm(Srcs, Dsts, [S|SAcc], [D|DAcc],LiveOut);
    false -> %% The dest is dead, move unnecessary.
      simplify_mm(Srcs, Dsts, SAcc, DAcc, LiveOut)
  end;
simplify_mm([], [], SAcc, DAcc, _) ->
  {SAcc, DAcc}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The environment. Rewrite if we go global.
%%
%% An environment has two mappings:
%%   If x is bound to y then
%%   map 1 contains {x,y} and map 2 contains {y,[x|_]}

new_env() ->
  {gb_trees:empty(),gb_trees:empty()}.

%%
%% Find what X is bound to (as a last restort variables are bound to
%% themselves).
%%

lookup(X,{Map,_}) ->
  case gb_trees:lookup(X,Map) of
    {value, Y} -> Y;
    none -> X
  end.

%%
%% Bind X to Y in Map
%%

bind({Map1,Map2}, X, Y) -> 
  NewMap2 =
    case gb_trees:lookup(Y,Map2) of
      none ->
	gb_trees:enter(Y,[X],Map2);
      {value,Ys} ->
	gb_trees:enter(Y,[X|Ys],Map2)
    end, 
  {gb_trees:enter(X,Y,Map1),NewMap2}.

%%
%% Kill bindings with references to X
%%

kill(X,M = {Map1,Map2}) ->
  %% First check if anyting is bound to X.
  M1 = {M11,M12} =
    case gb_trees:lookup(X,Map2) of
      none -> M;
      {value,Y1s} -> %% All Y1s bound to X
	{lists:foldl(fun (Y1,MapAcc) ->
			 gb_trees:delete_any(Y1,MapAcc)
		     end,Map1,Y1s),
	 gb_trees:delete(X,Map2)}
    end,
  %% Now check if X is bound to anything.
  case gb_trees:lookup(X,M11) of
    {value,Y2} -> %% X bound to Y2
      {gb_trees:delete(X,M11),
       case gb_trees:lookup(Y2,M12) of
	 none ->
	   M12;
	 {value,Y2s} ->
	   gb_trees:enter(Y2,lists:delete(X,Y2s),M12)
       end};
    none -> 
      M1
  end.

unbind([], Map) ->
  Map;
unbind([V|Vs], Map) ->
  unbind(Vs, kill(V, Map)).

pass_through([Label|Labels], CFG, PredMap, Info, Passed) ->
  Pred = hipe_rtl_cfg:pred(PredMap,Label),
  InfoIn = join(Pred, Info),
  CurrentBB = hipe_rtl_cfg:bb(CFG, Label),
  OldCode = hipe_bb:code(CurrentBB),
  {NewCode, _NewInfoOut} = prop2_instrs(OldCode, InfoIn), 
  NewBB = hipe_bb:mk_bb(NewCode),
  NewCFG = hipe_rtl_cfg:bb_add(CFG, Label, NewBB),
  NewSuccMap = hipe_rtl_cfg:succ_map(NewCFG),
  Succ = hipe_rtl_cfg:succ(NewSuccMap, Label),
  {NewLbls, NewPassed} = not_members_of_set(Succ, Passed),
  NewPredMap = hipe_rtl_cfg:pred_map(NewCFG),
  pass_through(Labels ++ NewLbls, NewCFG, NewPredMap, Info, NewPassed);
pass_through([], CFG, _PredMap, _Info, _Passed) ->
  CFG.

not_members_of_set(Labels, Set) -> 
  not_members_of_set(Labels, Set, []).

not_members_of_set([Label|Labels], Set, Acc) ->
  case gb_sets:is_member(Label, Set) of
    true ->
      not_members_of_set(Labels, Set, Acc);
    false ->
      not_members_of_set(Labels, gb_sets:add(Label,Set), [Label|Acc])
  end;
not_members_of_set([], Set, Acc) ->
  {Acc, Set}.

fix_point([Label|Labels], CFG, PredMap, Info) ->
  Pred = hipe_rtl_cfg:pred(PredMap,Label),
  InfoIn = join(Pred, Info),
  OldInfoOut = 
    case gb_trees:lookup(Label, Info) of
      {value, V} -> V;
      X -> X
    end,
  CurrentBB = hipe_rtl_cfg:bb(CFG, Label),
  OldCode = hipe_bb:code(CurrentBB),
  case prop2_instrs(OldCode, InfoIn) of
    {_OldCode, OldInfoOut} ->
      fix_point(Labels, CFG, PredMap, Info);
    {NewCode, NewInfoOut} ->
      NewBB = hipe_bb:mk_bb(NewCode),
      NewCFG = hipe_rtl_cfg:bb_add(CFG, Label, NewBB),
      NewSuccMap = hipe_rtl_cfg:succ_map(NewCFG),
      Succ = hipe_rtl_cfg:succ(NewSuccMap, Label), 
      NewList = add_succ_to_list(Succ, Labels),
      NewInfo = add_info(Label, NewInfoOut, Info),
      fix_point(NewList, CFG, PredMap, NewInfo)
  end;
fix_point([], _CFG, _PredMap, Info) ->
  Info.

prop2_instrs(Code, Env) ->
  prop2_instrs(Code, Env, []).

prop2_instrs([Instr|Rest], Env, Acc) -> 
  case prop_instr(Instr, Env) of
    {NewI, NewEnv} when is_list(NewI) ->
      prop2_instrs(Rest, NewEnv, Acc++NewI);
    {NewI, NewEnv} ->
      prop2_instrs(Rest, NewEnv, Acc++[NewI])
  end;
prop2_instrs([], Env, Acc) ->
  {Acc, Env}.

join([], _Info) ->
  new_env();
join([Pred], Info) ->
  case gb_trees:lookup(Pred, Info) of
    {value, V} -> V;
    none ->  new_env()
  end;
join(Preds, Info) ->
  join(Preds, Info, [], []).

join([Pred|Rest], Info, Acc1, Acc2) ->
  case gb_trees:lookup(Pred, Info) of
    {value,{Tree1, Tree2}} ->
      {List1, List2} = {gb_trees:to_list(Tree1),gb_trees:to_list(Tree2)} ,
      {Set1, Set2} = {gb_sets:from_ordset(List1),gb_sets:from_ordset(List2)},
      join(Rest, Info, [Set1|Acc1], [Set2|Acc2]);
    none ->
      join(Rest, Info, Acc1, Acc2)
  end;
join([], _Info, [], []) ->
  new_env();

join([], _Info, Sets1, Sets2) ->
  {I1, I2} = {gb_sets:intersection(Sets1),gb_sets:intersection(Sets2)},
  {L1, L2} = {gb_sets:to_list(I1), gb_sets:to_list(I2)},
  {gb_trees:from_orddict(L1), gb_trees:from_orddict(L2)}.


add_succ_to_list([First|Rest], List) ->
  case add_element(List, First) of
    fail ->
      add_succ_to_list(Rest, List);
    NewList ->
      add_succ_to_list(Rest, NewList)
  end;
add_succ_to_list([], List) ->
  List.

add_element(List, Element) ->
  add_element(List, Element, []).

add_element([Element|_Rest], Element, _Acc) ->
  fail;
add_element([First|Rest], Element, Acc) ->
  add_element(Rest, Element, [First|Acc]);
add_element([], Element, Acc) ->
  lists:reverse([Element|Acc]).
add_info(Label, NewInfo, OldInfo) ->
  gb_trees:enter(Label, NewInfo, OldInfo).

remove_loads(CFG) ->
  PredMap = hipe_rtl_cfg:pred_map(CFG),  
  SuccMap = hipe_rtl_cfg:succ_map(CFG),
  Info = gb_trees:empty(),  
  Start = hipe_rtl_cfg:start_label(CFG),  
  NewInfo=fix_remove([Start], CFG, PredMap, SuccMap, Info),
  Labels = hipe_rtl_cfg:reverse_postorder(CFG),
  update_cfg(Labels, CFG, PredMap, NewInfo).

update_cfg([Label|Labels], CFG, PredMap, Info) ->
  Pred = hipe_rtl_cfg:pred(PredMap,Label),
  InfoIn = join_load(Pred, Info),
  CurrentBB = hipe_rtl_cfg:bb(CFG, Label),
  OldCode = hipe_bb:code(CurrentBB),
  case spread_info(OldCode, InfoIn) of
    {OldCode, _} ->
      update_cfg(Labels, CFG, PredMap, Info);
    {NewCode, _} ->
      NewBB = hipe_bb:code_update(CurrentBB, NewCode),
      NewCFG = hipe_rtl_cfg:bb_add(CFG, Label, NewBB),
      NewPredMap = hipe_rtl_cfg:pred_map(NewCFG),
      update_cfg(Labels, NewCFG, NewPredMap, Info)
  end;
update_cfg([], CFG, _PredMap, _Info) ->
  CFG.
    
fix_remove([Label|Labels], CFG, PredMap, SuccMap, Info) ->
  Pred = hipe_rtl_cfg:pred(PredMap,Label),
  InfoIn = join_load(Pred, Info),
  OldInfoOut = 
    case gb_trees:lookup(Label, Info) of
      {value, V} -> V;
      none -> none
    end,
  CurrentBB = hipe_rtl_cfg:bb(CFG, Label),
  OldCode = hipe_bb:code(CurrentBB),
  case spread_info(OldCode, InfoIn) of
    {_,OldInfoOut} ->
      fix_remove(Labels, CFG, PredMap, SuccMap, Info);
    {_,NewInfoOut} ->
      Succ = hipe_rtl_cfg:succ(SuccMap, Label), 
      NewList = add_succ_to_list(Succ, Labels),
      NewInfo = add_info(Label, NewInfoOut, Info),
      fix_remove(NewList, CFG, PredMap, SuccMap, NewInfo)
  end;
fix_remove([], _CFG, _PredMap, _SuccMap, Info) ->
  Info.

spread_info(Code, Info) ->
  lists:foldl(fun do_instr/2, {[],Info}, Code).

do_instr(Instr, {Acc,Info}) ->
  case hipe_rtl:type(Instr) of
    call ->
      {Acc++[Instr],new_load_env()};
    store ->  
      {Acc++[Instr],new_load_env()};
    load ->
      Dst = hipe_rtl:load_dst(Instr),
      LoadType = {Dst, hipe_rtl:load_src(Instr), hipe_rtl:load_offset(Instr), 
		  hipe_rtl:load_size(Instr), hipe_rtl:load_sign(Instr)},
      NewInstr = 
	case get_aliased_var(LoadType, Info) of
	  {var, Var} ->
	    hipe_rtl:mk_move(Dst, Var);
	  none ->
	    Instr
	end,
      Defs = hipe_rtl:defines(Instr),
      CleanInfo = remove_loads(Defs, Info),
      {Acc++[NewInstr],gb_sets:add(LoadType, CleanInfo)};
    _ ->
      Defs = hipe_rtl:defines(Instr),
      {Acc++[Instr],remove_loads(Defs, Info)}
  end.
  
remove_loads([Def|Defs], Info) ->
  NewInfo = gb_sets:filter(fun(X) -> not_part(X, Def) end, Info),
  remove_loads(Defs, NewInfo);
remove_loads([], Info) ->
  Info.

remove_stores(CFG) ->
  Labels=hipe_rtl_cfg:labels(CFG),
  remove_stores(Labels, CFG).

remove_stores([Label|Labels], CFG) ->
  BB=hipe_rtl_cfg:bb(CFG, Label),
  OldCode = hipe_bb:code(BB),
  case remove_store_from_bb(OldCode) of
    OldCode ->
      remove_stores(Labels, CFG);
    NewCode ->
      NewBB = hipe_bb:code_update(BB, NewCode),
      NewCFG = hipe_rtl_cfg:bb_add(CFG, Label, NewBB),
      remove_stores(Labels, NewCFG)
  end;
remove_stores([], CFG) ->
  CFG.

remove_store_from_bb(Code) ->
  remove_store_from_bb(lists:reverse(Code), new_load_env(), []).

remove_store_from_bb([Instr|Instrs], Env, Acc) ->
  {NewAcc, NewEnv} =
    case hipe_rtl:type(Instr) of
      call ->
	{[Instr|Acc],new_load_env()};
      store ->  
	Base = hipe_rtl:store_base(Instr),
	Offset = hipe_rtl:store_offset(Instr),
	Size = hipe_rtl:store_size(Instr),
	StoreType = {Base, Offset, Size},
	case store_already_done(StoreType, Env) of
	  true ->
	    {Acc, Env};
	  false ->
	    {[Instr|Acc], gb_sets:add(StoreType, Env)}
	end;
      load ->
	{[Instr|Acc],new_load_env()};
      _ -> 
      Defs = hipe_rtl:defines(Instr),
	{[Instr|Acc], update_store_env(Defs, Env)}
    end,
  remove_store_from_bb(Instrs, NewEnv, NewAcc);
remove_store_from_bb([], _Env, Acc) ->
  Acc.

update_store_env([Def|Defs], Env) ->
  NewEnv = gb_sets:filter(fun(X) -> not_part_of_store(X, Def) end, Env),
  update_store_env(Defs, NewEnv);
update_store_env([], Env) ->
  Env.

store_already_done(StoreType, Env) ->
  Iterator = gb_sets:iterator(Env),
  check_store(StoreType, Iterator).

check_store(StoreType, Iterator) ->
  case gb_sets:next(Iterator) of
    none ->
      false;				    
    {StoreType, _} ->
      true;
    {_, Next} ->
      check_store(StoreType, Next)
  end.

not_part_of_store({Def,_,_}, Def) -> false;
not_part_of_store({_,Def,_}, Def) -> false;
not_part_of_store(_, _Def) -> true.

not_part({Def,_,_,_,_}, Def) -> 
  false;
not_part({_,Def,_,_,_}, Def) ->
  false;
not_part({_,_,Def,_,_}, Def) ->
  false;
not_part(_, _Def) ->
  true.

get_aliased_var({_, Src, Offset, Size, Sign}, Info) -> 
  Iterator = gb_sets:iterator(Info),
  find_aliased_var({Src, Offset, Size, Sign}, Iterator).

find_aliased_var({Src, Offset, Size, Sign}, Iterator) ->
  case gb_sets:next(Iterator) of
    none ->
      none;
    {{Var, Src, Offset, Size, Sign}, _}  ->
      {var, Var};
    {_,Next} ->
      find_aliased_var({Src, Offset, Size, Sign}, Next)
  end.

new_load_env() ->
  gb_sets:empty().

join_load(Preds, Info) ->
  join_load(Preds, Info, []).

join_load([Pred|Preds], Info, Acc) ->
  case gb_trees:lookup(Pred, Info) of
    {value, V} ->
      join_load(Preds, Info, [V|Acc]);
    none ->
      join_load(Preds, Info, Acc)
  end;

join_load([], _Info, Acc) ->
  intersect(Acc).

intersect([]) ->
  new_load_env();
intersect([Value]) ->
  Value;
intersect(List) ->
  gb_sets:intersection(List).
