%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Semi-local copy propagation, constant propagation, constant folding
%% and dead code removal.
%%
%% Works on extended basic blocks. No iteration is done at this point.
%%
%% The environment binds (rtl) variables to:
%%    - constants
%%    - variables
%%

-module(hipe_rtl_prop).
-export([cfg/1]).
-include("../main/hipe.hrl").

cfg(CFG) ->
  ?opt_start_timer("Rtl Prop"),
  ?opt_start_timer("EBBs"),
  EBBs = hipe_rtl_ebb:cfg(CFG),

  ?opt_stop_timer("EBBs"),
  ?opt_start_timer("Prop"),
  CFG0 = prop_ebbs(EBBs, CFG),
  ?opt_stop_timer("Prop"),
  
  ?opt_start_timer("Liveness"),
  Liveness = hipe_rtl_liveness:analyze(CFG0),
  ?opt_stop_timer("Liveness"),
  ?opt_start_timer("Dead code"),
  EBBs2 = hipe_rtl_ebb:cfg(CFG0),
  CFG1 = dead_code_ebbs(EBBs2, CFG0, Liveness),
  ?opt_stop_timer("Dead code"),
  ?opt_stop_timer("RtlProp"),
  CFG1.


%%
%% Iterate over the extended basic blocks of a cfg.
%%

prop_ebbs([], CFG) ->
  CFG;
prop_ebbs([Ebb|Ebbs], CFG) ->
  CFG0 = prop_ebb(Ebb, new_env(), CFG),
  prop_ebbs(Ebbs, CFG0).


%%
%% If Lbl is a member of the extended block Ebb. Then propagate info 
%% and continue with its successors.
%%

prop_ebb(Ebb, Env, CFG) ->
  case hipe_rtl_ebb:type(Ebb) of
    node ->
      Lbl = hipe_rtl_ebb:node_label(Ebb),
      BB = hipe_rtl_cfg:bb(CFG, Lbl),
      {NewCode, NewEnv} = prop_instrs(hipe_bb:code(BB), Env),
      NewBB = hipe_bb:code_update(BB, NewCode),
      NewCFG = hipe_rtl_cfg:bb_update(CFG, Lbl, NewBB),
      Succ = hipe_rtl_ebb:node_successors(Ebb),
      prop_succ(Succ, NewEnv, NewCFG);
    leaf ->
      CFG
  end.


prop_succ([], _Env, CFG) ->
  CFG;
prop_succ([EBB|EBBs], Env, CFG) ->
  NewCFG = prop_ebb(EBB, Env, CFG),
  prop_succ(EBBs, Env, NewCFG).

prop_instrs(Is, Env) ->
  {NewIs, NewEnv} = lists:mapfoldl(fun prop_instr/2, Env, Is),
  {lists:flatten(NewIs), NewEnv}.


%prop_instrs([], Env) ->
%  {[], Env};
%prop_instrs([I|Is], Env) ->
%  {NewI, Env0} = prop_instr(I, Env),
%  {NewIs, NewEnv} = prop_instrs(Is, Env0),
%  %% io:format("I ~w to ~w\n",[I,NewI]),
%  case NewI of
%    [_|_] -> {NewI++NewIs, NewEnv};	%% alub -> [move, goto]
%    _ -> {[NewI|NewIs], NewEnv}
%  end.


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
      Srcs = hipe_rtl:multimove_src(I),
      Dsts = hipe_rtl:multimove_dst(I),
      bind_all(Srcs, Dsts, I, NewEnv);
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

%%%
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
	  Val = eval_constant_alu(hipe_rtl:imm_value(Src1), 
				  hipe_rtl:alu_op(I), 
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
	  case eval_constant_alub(Val1, Op, Val2, Cond) of
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
	  case hipe_rtl:branch_cond(I) of
	    eq ->
	      if Src1 =:= Src2 ->
		  {hipe_rtl:mk_goto(hipe_rtl:branch_true_label(I)), Env};
		 true ->
		  {hipe_rtl:mk_goto(hipe_rtl:branch_false_label(I)), Env}
	      end;
	    ne ->
	      if Src1 =:= Src2 ->
		  {hipe_rtl:mk_goto(hipe_rtl:branch_false_label(I)), Env};
		 true ->
		  {hipe_rtl:mk_goto(hipe_rtl:branch_true_label(I)), Env}
	      end;
	    Cond ->
	      %% remind us what we need to implement later
	      io:format(standard_io,
			"Warning: ~w: failed to evaluate branch cond ~w\n",
			[?MODULE, Cond]),
	      {I, Env}
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


eval_constant_alu(Arg1, Op, Arg2) ->
  case Op of
    'sub' ->
      hipe_rtl:mk_imm(Arg1 - Arg2);
    'add' ->
      hipe_rtl:mk_imm(Arg1 + Arg2);
    'sra' ->
      hipe_rtl:mk_imm(Arg1 bsr Arg2);
    'srl' ->
      %% sanity check: Arg1 must denote an unsigned machine word
      %% XXX: Reconsider if we go 64-bit.
      if Arg1 >= 0, Arg1 =< 16#ffffffff -> ok;
	 true -> exit({?MODULE, eval_constant_alu, {Op, Arg1}})
      end,
      hipe_rtl:mk_imm(Arg1 bsr Arg2);
    'sll' ->
      hipe_rtl:mk_imm((Arg1 bsl Arg2) band 16#ffffffff);
    'or' ->
      hipe_rtl:mk_imm(Arg1 bor Arg2);
    'and' ->
      hipe_rtl:mk_imm(Arg1 band Arg2);
    'xor' ->
      hipe_rtl:mk_imm(Arg1 bxor Arg2);
    Op ->
      exit({?MODULE, eval_constant_alu, {"unknown alu op", Op}})
  end.

eval_constant_alub(Val1, Op, Val2, Cond) ->
  Val3 = 
    case Op of
      'and' ->
	Val1 band Val2;
      'sub' ->
	Val1 - Val2;
      _ ->
	fail
    end,
  if Val3 =:= fail ->
      eval_constant_alub_fail(Val1, Op, Val2, Cond);
     true ->
      case Cond of
	'eq' ->
	  {Val3, Val3 =:= 0};
	'ne' ->
	  {Val3, Val3 =/= 0};
	'overflow' ->
	  %% XXX: Fix this at some point.
	  %% We do a safe approximation
	  if (Val3 < 4096) and (Val3 >= 0) ->
	      {Val3, false};
	     %% Were not sure if this will overflow...
	     true -> []
	  end;
	_ ->
	  eval_constant_alub_fail(Val1, Op, Val2, Cond)
      end
  end.

eval_constant_alub_fail(Val1, Op, Val2, Cond) ->
  %% log this to stdout to remind us what we need to implement later
  io:format(standard_io, "Warning: ~w: failed to evaluate alub <~w,~w,~w,~w>\n",
	    [?MODULE, Val1, Op, Val2, Cond]),
  [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%% Dead code elimination
%%%

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
      NewCFG = hipe_rtl_cfg:bb_update(CFG0, Lbl, NewBB),
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
  case {hipe_rtl:is_pure(NewI), Dead} of
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


%% Simplify multimoves
simplify_mm(I, LiveOut) ->
  case hipe_rtl:type(I) of
    multimove ->
      {NewSource, NewDest} = simplify_mm(hipe_rtl:multimove_src(I),
					 hipe_rtl:multimove_dst(I), LiveOut),
      Info = hipe_rtl:multimove_info(I),
      hipe_rtl:multimove_info_update(hipe_rtl:mk_multimove(NewDest, NewSource),
				     Info);
    _ ->
      I
  end.

simplify_mm(Ss, Ds, LiveOut) ->
  simplify_mm(Ss, Ds, [], [], LiveOut).

simplify_mm([S|Srcs],[S|Dsts], SAcc, DAcc, LiveOut) ->
  simplify_mm(Srcs, Dsts, SAcc, DAcc, LiveOut);
simplify_mm([S|Srcs],[D|Dsts], SAcc, DAcc, LiveOut) ->
  case gb_sets:is_element(D, LiveOut) of
    true -> %% The dest is live after the instruction.
      simplify_mm(Srcs, Dsts, [S|SAcc] , [D|DAcc],LiveOut);
    false -> %% The dest is dead, move unnecessary.
      simplify_mm(Srcs, Dsts, SAcc, DAcc, LiveOut)
  end;
simplify_mm([], [], SAcc, DAcc, _) ->
  {SAcc, DAcc}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% the environment, Rewrite if we go global.
%%
%% An environment has two mappings If x is bound to y then
%% map 1 contains {x,y} and map 2 contains {y,[x|_]}
new_env() ->
  {gb_trees:empty(),gb_trees:empty()}.


%%
%% Find what X is bound to (as a last restort varaibles are bound 
%% to themselves).
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
       none -> gb_trees:enter(Y,[X],Map2);
       {value,Ys} ->
	 gb_trees:enter(Y,[X|Ys],Map2)
    end, 
    {gb_trees:enter(X,Y,Map1),
     NewMap2}.



%%
%% Kill bindings with references to X
%%
kill(X,M = {Map1,Map2}) ->
  %% First check if anyting is bound to X.
  M1 = {M11,M12} =
    case gb_trees:lookup(X,Map2) of
      none -> M;
      {value,Y1s} -> %% All Y1s bound to X
	{lists:foldl(
	   fun (Y1,MapAcc) ->
	       gb_trees:delete_any(Y1,MapAcc)
	   end,Map1,Y1s),
	 gb_trees:delete(X,Map2)}
    end,
  %% Now check if X is bound to anything.
  case gb_trees:lookup(X,M11) of
    {value,Y2} -> %% X bound to Y2
      {gb_trees:delete(X,M11),
       case gb_trees:lookup(Y2,M12) of
	 none -> M12;
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

