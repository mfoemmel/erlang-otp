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


cfg(CFG) ->
  EBBs = hipe_rtl_ebb:cfg(CFG),
  CFG0 = prop_ebbs(EBBs, CFG),
  dead_code_ebbs(EBBs, CFG0, hipe_rtl_liveness:analyze(CFG0)).


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


prop_succ([], Env, CFG) ->
  CFG;
prop_succ([EBB|EBBs], Env, CFG) ->
  NewCFG = prop_ebb(EBB, Env, CFG),
  prop_succ(EBBs, Env, NewCFG).


prop_instrs([], Env) ->
  {[], Env};
prop_instrs([I|Is], Env) ->
  {NewI, Env0} = prop_instr(I, Env),
  {NewIs, NewEnv} = prop_instrs(Is, Env0),
  %% io:format("I ~w to ~w\n",[I,NewI]),
  case NewI of
    [_|_] -> {NewI++NewIs, NewEnv};	%% alub -> [move, goto]
    _ -> {[NewI|NewIs], NewEnv}
  end.


%%
%% Propagate copies and constants for one instruction.
%%

prop_instr(I, Env) ->
  NewEnv = unbind(hipe_rtl:defines(I), Env),
  case hipe_rtl:type(I) of
    move ->
      Srcs = [hipe_rtl:multimove_src(I)],
      Dsts = [hipe_rtl:multimove_dst(I)],
      bind_all(Srcs, Dsts, I, NewEnv);
    multimove ->
      Srcs = hipe_rtl:multimove_src(I),
      Dsts = hipe_rtl:multimove_dst(I),
      bind_all(Srcs, Dsts, I, NewEnv);
    jsr ->
      {I, new_env()};
    _ ->
      Uses = hipe_rtl:uses(I),
      %% Map = [{U, lookup(U, Env)} || U <- Uses],
      Map = map_all(Uses, Env),
      NewI = hipe_rtl:subst_uses(Map, I),
      eval(NewI, NewEnv)
  end.


map_all([], Env) ->
  [];
map_all([V|Vs], Env) ->
  [{V, lookup(V, Env)} | map_all(Vs, Env)].


bind_all(Srcs, Dsts, I, Env) ->
  bind_all(Srcs, Dsts, I, Env, Env).

%%%
%% We have two envs, Env wher we do lookups and
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
    jsr ->
      {I, new_env()};
    esr ->
      {I, new_env()};
    jmp ->
      {I, new_env()};
    jmp_link ->
      {I, new_env()};
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
  case Op of
    'and' ->
      Val3 = Val1 band Val2,
      case Cond of
	'eq' ->
	  {Val3, Val3 =:= 0};
	'ne' ->
	  {Val3, Val3 =/= 0};
	_ ->
	  eval_constant_alub_fail(Val1, Op, Val2, Cond)
      end;
    _ ->
      eval_constant_alub_fail(Val1, Op, Val2, Cond)
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

dead_code_ebbs([], CFG, Live) ->
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
	    {CFG,hipe_rtl_liveness:liveout(Live, Lbl)};
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
      {CFG, hipe_rtl_liveness:livein(Live, Lbl)}
  end.


dead_code_succ([], CFG, Live) ->
  {CFG, ordsets:new()};
dead_code_succ([EBB|EBBs], CFG, Live) ->
  {CFG0, LiveOut0} = dead_code_ebb(EBB, CFG, Live),
  {NewCFG, LiveOut1} = dead_code_succ(EBBs, CFG0, Live),
  {NewCFG, ordsets:union(LiveOut0, LiveOut1)}.


dead_code_instrs([], LiveOut) ->
  {[], LiveOut};
dead_code_instrs([I|Is], LiveOut) ->
  {NewIs, LiveOut0} = dead_code_instrs(Is, LiveOut),
  NewI = simplify_mm(I, LiveOut0),
  Def = ordsets:from_list(hipe_rtl:defines(NewI)),
  Dead = ordsets:intersection(LiveOut0, Def) == ordsets:new(),
  case {hipe_rtl:is_pure(NewI), Dead} of
    {true, true} ->
      {NewIs, LiveOut0};
    _ ->
      case dead_move(NewI) of
	true ->
	  {NewIs, LiveOut0};
	false ->
	  Use = ordsets:from_list(hipe_rtl:uses(I)),
	  LiveIn = ordsets:union(Use, ordsets:subtract(LiveOut0, Def)),
	  {[NewI|NewIs], LiveIn}
      end
  end.


%%
%% Identity moves can be safely deleted.
%%

dead_move(X) ->
  (hipe_rtl:is_move(X) and
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
  case ordsets:is_element(D, LiveOut) of
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

new_env() ->
  [].


%%
%% Find what X is bound to (as a last restort varaibles are bound 
%% to themself).
%%

lookup(X, []) ->
  X;
lookup(X, [{X, Y}|_]) ->
  Y;
lookup(X, [_|Map]) ->
  lookup(X, Map).


%%
%% Bind X to Y in Map
%%

bind(Map, X, Y) -> 
  [{X, Y} | Map].


%%
%% Kill bindings with references to X
%%

kill(X, []) ->
  [];
kill(X, [{X,_}|Xs]) ->
  kill(X, Xs);
kill(X, [{_,X}|Xs]) ->
  kill(X, Xs);
kill(X, [D|Xs]) ->
  [D | kill(X,Xs)].


unbind([], Map) ->
  Map;
unbind([V|Vs], Map) ->
  unbind(Vs, kill(V, Map)).

