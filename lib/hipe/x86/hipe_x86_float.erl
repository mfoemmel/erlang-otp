%% -*- erlang-indent-level: 2 -*-
%% Floating point handling.

-module(hipe_x86_float).
-include("hipe_x86.hrl").
-include("../main/hipe.hrl").
-export([map/1]).

map(Defun) ->
  CFG0 = hipe_x86_cfg:init(Defun),
  Liveness = hipe_x86_liveness:analyse(CFG0),
  StartLabel = hipe_x86_cfg:start_label(CFG0),
  SuccMap = hipe_x86_cfg:succ_map(CFG0),
  {CFG1, _} = do_blocks([],[StartLabel], CFG0, Liveness, [], SuccMap, 
			gb_trees:empty()),
  CFG2 = hipe_x86_cfg:var_range_update(CFG1, []),
  hipe_x86_cfg:linearise(CFG2).


do_blocks(Pred,[Lbl|Lbls], CFG, Liveness, Map, SuccMap, BlockMap) ->
  case gb_trees:lookup(Lbl, BlockMap) of
    none ->
      %% This block has not been visited.
      Block = hipe_x86_cfg:bb(CFG, Lbl),
      Succ = hipe_x86_cfg:succ(SuccMap, Lbl),
      NewBlockMap = gb_trees:insert(Lbl, Map, BlockMap),
      LiveOut = [X || X <- hipe_x86_liveness:liveout(Liveness, Lbl),
		      is_fp(X)],
      Code = hipe_bb:code(Block),
      ReverseCode = lists:reverse(Code),
      {NewCode0, NewMap, NewBlockMap1, Dirty} = 
	do_block(ReverseCode, LiveOut, Map, NewBlockMap),
      NewCFG1 =
	case Dirty of
	  true ->
	    NewBlock = hipe_bb:code_update(Block, NewCode0),
	    hipe_x86_cfg:bb_update(CFG, Lbl, NewBlock);
	  _ ->
	    CFG
	end,
      {NewCFG3, NewBlockMap2} = 
	do_blocks(Lbl,Succ, NewCFG1, Liveness, NewMap, 
		  SuccMap, NewBlockMap1),
      do_blocks(Pred,Lbls, NewCFG3, Liveness, 
		Map, SuccMap, NewBlockMap2);
    {value, {fail, not_handled}} ->
      {NewCFG, NewBlockMap} =
	do_fail(Lbl, CFG, BlockMap),
      Succ = hipe_x86_cfg:succ(SuccMap, Lbl),
      {NewCFG3, NewBlockMap2} = 
	do_blocks(Lbl,Succ, NewCFG, Liveness, [],
		  SuccMap, NewBlockMap),
      do_blocks(Pred,Lbls, NewCFG3, Liveness, 
		Map, SuccMap, NewBlockMap2);
    {value, {fail, handled}} ->
      %% Don't have to follow this trace any longer.
      do_blocks(Pred,Lbls, CFG, Liveness, 
		Map, SuccMap, BlockMap);
    {value, ExistingMap}->
      %% This block belongs to a trace already handled.
      %% The Map coming in must be identical to the one used
      %% when the block was processed.
      if ExistingMap == Map -> 
	  do_blocks(Pred,Lbls, CFG, Liveness, 
		    Map, SuccMap, BlockMap);
	 true ->
	  NewCFG = do_shuffle(Pred,Lbl,CFG, Map,ExistingMap),
	  NewSuccMap = hipe_x86_cfg:succ_map(NewCFG),
	  do_blocks(Pred,Lbls, NewCFG, Liveness, Map, 
		    NewSuccMap, BlockMap)
      end
  end;
do_blocks(Pred,[], CFG, _,_,Map,BlockMap) ->
  %% At the end of each trace that doesn't merge with an earlier
  %% one, the stack must be empty!
  if length(Map)/=0 -> 
      ?EXIT({'FpStackNotEmptyAtEndOfTrace',{label,Pred}});
     true->
      {CFG, BlockMap}
  end.

do_fail(Lbl, CFG, BlockMap)->
  %% Remove any floats on the stack that might be left 
  %% after a failed call.
  Block = hipe_x86_cfg:bb(CFG, Lbl),
  Code = hipe_bb:code(Block),
  NewCode = free_all()++Code,
  NewBlock = hipe_bb:code_update(Block, NewCode),
  NewCFG1 = hipe_x86_cfg:bb_update(CFG, Lbl, NewBlock),	    
  NewBlockMap = gb_trees:update(Lbl,{fail,handled}, BlockMap),
  {NewCFG1, NewBlockMap}.

do_block([I|Is], LiveOut, Map, BlockMap) ->
  do_block([I|Is], LiveOut, Map, BlockMap, false).
do_block([I|Is], LiveOut, Map, BlockMap, Dirty) ->
  case handle_insn(I) of
    false -> 
      {NewCode, NewMap, NewBlockMap, NewDirty} = 
	do_block(Is, LiveOut,Map,BlockMap,Dirty),
      {NewCode++[I], NewMap, NewBlockMap, NewDirty};
    true ->
      Def = ordsets:from_list(hipe_x86_defuse:insn_def(I)),
      Use = ordsets:from_list(hipe_x86_defuse:insn_use(I)),
      NewLiveOut = 
	ordsets:filter(fun(X)->is_fp(X)end,
		       ordsets:union(
			 ordsets:subtract(LiveOut, Def),Use)),

      {NewCode, NewMap, NewBlockMap, NewDirty} = 
	do_block(Is, NewLiveOut, Map, BlockMap,Dirty),
      {NewI, NewMap1, NewBlockMap1} = 
	do_insn(I, LiveOut, NewMap, NewBlockMap),
      NewDirty1 =
	if NewDirty == true -> true;
	   NewI =:= [I] -> false;
	   true -> true
	end,
      {NewCode++NewI, NewMap1, NewBlockMap1, NewDirty1}
  end;

do_block([],LiveOut, Map, BlockMap, Dirty) ->
  %% Make sure we don't have anything on the stack
  %% that isn't live out from this point.
  case ordsets:subtract(ordsets:from_list(Map),LiveOut) of
    [] -> {[], Map, BlockMap, Dirty};
    Dead -> 
      {Insn, NewMap} = pop_dead(ordsets:to_list(Dead),Map),
      {Insn, NewMap, BlockMap, true}
  end.

do_shuffle(Pred,Lbl,CFG, OldMap, NewMap)->
  %% First make sure both maps has the same members.
  Push = NewMap -- OldMap,
  Pop = OldMap -- NewMap,
  {PopInsn, OldMap0} = pop_dead(Pop, OldMap),
  {PushInsn, OldMap1} = push_list(Push, OldMap0),
  Code =
    if OldMap1==NewMap ->
	%% It was enough to push and pop.
	PopInsn ++ PushInsn ++
	  [hipe_x86:mk_jmp_label(Lbl)];
       true->
	%% Shuffle the positions so the maps match
	Cycles = find_swap_cycles(OldMap1, NewMap),
	SwitchInsns = do_switching(Cycles),
	PopInsn++PushInsn++SwitchInsns++
	  [hipe_x86:mk_jmp_label(Lbl)]
    end,
  %% Update the CFG.
  NewLabel = hipe_gensym:get_next_label(x86),
  {LLo,_} = hipe_x86_cfg:label_range(CFG),
  LHi = hipe_gensym:get_label(x86),
  NewCFG0 = hipe_x86_cfg:label_range_update(CFG, {LLo, LHi}),
  NewCFG1 = hipe_x86_cfg:bb_add(NewCFG0, NewLabel, 
				hipe_bb:mk_bb(Code,false)),
  OldPred = hipe_x86_cfg:bb(NewCFG1, Pred),
  PredCode = hipe_bb:code(OldPred),
  NewLast = redirect(lists:last(PredCode), Lbl,NewLabel),
  NewPredCode = butlast(PredCode)++[NewLast],
  NewPredBB = hipe_bb:code_update(OldPred, NewPredCode),
  hipe_x86_cfg:bb_update(NewCFG1, Pred, NewPredBB).


find_swap_cycles(OldMap, NewMap)->
  Moves = [get_pos(X,NewMap,1) || X <- OldMap],
  find_swap_cycles(OldMap, Moves, lists:seq(1,length(OldMap)), []).

find_swap_cycles(OldMap, Moves, NotHandled, Cycles)->
  if length(NotHandled) == 0 -> Cycles;
     true -> 
      Cycle = find_cycle(Moves, [hd(NotHandled)]),
      NewNotHandled = NotHandled -- Cycle,
      case lists:member(1, Cycle) of
	true->
	  %% The cycle that contains the first element on the stack
	  %% must be processed last.
	  NewCycle = [format_cycle(Cycle)],
	  find_swap_cycles(OldMap, Moves, NewNotHandled,
			   Cycles++[NewCycle]);
	_ ->
	  find_swap_cycles(OldMap, Moves, NewNotHandled,
			   [Cycle|Cycles])
      end
  end.

find_cycle(Moves, Cycle)->
  To = lists:nth(lists:last(Cycle),Moves),
  if To == hd(Cycle) -> Cycle;
     true -> find_cycle(Moves, Cycle++[To])
  end.

format_cycle(C)->
  %% The position numbers start with 1 - should start with 0.
  %% If position 0 is in the cycle it will be permuted until
  %% the 0 is first and then remove it.
  %% Otherwise the first element is also added last.
  NewCycle = [X - 1 || X <- C],
  case lists:member(0,NewCycle) of
    true -> format_cycle(NewCycle, []);
    _ -> NewCycle ++ hd(NewCycle)
  end.
format_cycle([H|T], NewCycle)->
  case H of
    0 -> T++NewCycle;
    _ -> format_cycle(T,NewCycle++[H])
  end.

do_switching(Cycles)->
  do_switching(Cycles, []).
do_switching([C|Cycles], Insns)->
  if length(C)<2 -> do_switching(Cycles, Insns);
     true ->
      NewInsns = Insns ++ [hipe_x86:mk_fop(fxch, mk_st(X), []) || X <- C],
      do_switching(Cycles, NewInsns)
  end;
do_switching([],Insns) ->
  Insns.

redirect(Insn, OldLbl, NewLbl)->
  case Insn of
    #pseudo_call{contlab=ContLab, exnlab=ExnLab}->
      if ContLab =:= OldLbl -> 
	  Insn#pseudo_call{contlab=NewLbl};
	 ExnLab =:= OldLbl ->
	  Insn#pseudo_call{exnlab=NewLbl}
      end;
    _ -> 
      hipe_x86_cfg:redirect_jmp(Insn, OldLbl, NewLbl)
  end.

do_insn(I, LiveOut, Map, BlockMap) ->
  case I of
    #pseudo_call{'fun'={_,Fun},exnlab = ExnLab}->
      case (Fun=='check_fp_exception') or
	(Fun == 'conv_big_to_float') of
	true ->
	  NewBlockMap = 
	    case gb_trees:lookup(ExnLab, BlockMap) of
	      {value,{fail,handled}} ->
		BlockMap;
	      {value, _} ->
		gb_trees:update(ExnLab, {fail, not_handled},
				BlockMap);
	      _ ->
		gb_trees:insert(ExnLab, {fail, not_handled},
				BlockMap)
	    end,
	  {pop_all(Map)++[I],[],NewBlockMap};
	_ ->
	  {pop_all(Map)++[I],[],BlockMap}
      end;
    #pseudo_tailcall_prepare{}->
      {pop_all(Map)++[I],[],BlockMap};
    #fop{}->
      {NewI, NewMap} = do_fop(I, LiveOut, Map),
      {NewI, NewMap, BlockMap};
    #fmov{src=Src, dst=Dst}->
      if Src=:=Dst->
	  %% Don't need to keep this instruction!
	  %% However, we may need to pop from the stack.
	  case is_liveOut(Src, LiveOut) of
	    true->
	      {[], Map, BlockMap};
	    false ->
	      {SwitchInsn, NewMap0} = switch_first(Dst, Map),
	      NewMap = pop(NewMap0),
	      {SwitchInsn++pop_insn(), NewMap, BlockMap}
	  end;
	 true -> 
	  {NewI, NewMap} = do_fmov(Src, Dst, LiveOut, Map),
	  {NewI, NewMap, BlockMap}
      end;
    _ ->
      {[I], Map, BlockMap}
  end.

do_fmov(Src, Dst=#x86_mem{},_LiveOut, Map) ->
%%% Storing a float from the stack into memory.
  {SwitchInsn, NewMap0} = switch_first(Src, Map),
  NewMap1 = pop(NewMap0),
  {SwitchInsn ++[hipe_x86:mk_fop(fstp, [], Dst)], NewMap1};

do_fmov(Src=#x86_mem{}, Dst, _LiveOut, Map) ->
%%% Pushing a float into the stack.
  case in_map(Dst, Map) of
    true -> ?EXIT({loadingExistingFpVariable,{Src,Dst}});
    _ -> ok
  end,
  {PushOp, [_|NewMap0]} = push(Src, Map),
  %% We want Dst in the map rather than Src.
  NewMap = [Dst|NewMap0],
  {PushOp, NewMap};

do_fmov(Src, Dst, LiveOut, Map) ->
%%% Copying a float that either is spilled or is on the fp stack,
%%% or converting a fixnum in a temp to a float on the fp stack.
  case in_map(Dst, Map) of
    true -> ?EXIT({copyingToExistingFpVariable,{Src,Dst}});
    _ -> ok
  end,
  IsConv =
    case Src of
      #x86_temp{type=Type}-> Type /= 'double';
      _ -> false
    end,
  case IsConv of
    true ->
      do_conv(Src,Dst,Map);
    _ ->
      %% Copying.
      case {is_liveOut(Src, LiveOut),in_map(Src, Map)} of
	{false,true}->
	  %% Just remap Dst to Src
	  {Head,[_|T]} = lists:splitwith(fun(X)->X/=Src end,Map),
	  {[], Head++[Dst|T]};
	_ ->
	  {PushOp, [_|NewMap0]} = push(Src, Map),
	  %% We want Dst in the map rather than Src.
	  NewMap = [Dst|NewMap0],
	  {PushOp, NewMap}
      end
  end.

do_conv(Src=#x86_temp{reg=Reg},Dst,Map)->
  %% Converting. Src must not be a register, so we 
  %% might have to put it into memory in between.
  {Move, NewSrc} = 
    case hipe_x86_registers:is_precoloured(Reg) of
      true ->
	Temp = hipe_x86:mk_new_temp('untagged'),
	{[hipe_x86:mk_move(Src,Temp)],Temp};
      _ ->
	{[],Src}
    end,
  {PushOp, [_|NewMap0]} = push(NewSrc, Map),
  %% We want Dst in the map rather than NewSrc.
  NewMap = [Dst|NewMap0],
  case length(PushOp) of
    1 -> %% No popping of memory object on fpstack
      {Move++[hipe_x86:mk_fop(fild, NewSrc, [])], NewMap};
    _ -> %% H contains pop instructions. Must be kept!
      Head = butlast(PushOp),
      {Move++Head++[hipe_x86:mk_fop(fild, NewSrc, [])], NewMap}
  end.


do_fop(I = #fop{dst=Dst, op=fchs}, Liveout, Map ) ->
  %% This is fchs, the only operation without a
  %% popping version. Needs special handling.
  case is_liveOut(Dst, Liveout) of
    true ->
      {SwitchInsn, NewMap} = switch_first(Dst, Map),	
      {SwitchInsn++[I#fop{dst=[]}], NewMap};
    false ->
      %% Don't need to keep this instruction!
      %% However, we may need to pop Dst from the stack.
      case in_map(Dst, Map) of
	true ->
	  {SwitchInsn, NewMap0} = switch_first(Dst, Map),
	  NewMap = pop(NewMap0),
	  {SwitchInsn++pop_insn(), NewMap};
	_ ->
	  {[],Map}
      end
  end;

do_fop(#fop{src=Src, dst=Dst, op=Op},
       LiveOut, Map) ->
  case {is_liveOut(Src, LiveOut), is_liveOut(Dst, LiveOut)} of
    {true, true} ->
      keep_both(Op, Src, Dst, Map);
    {true, false} ->
      keep_src(Op, Src, Dst, Map);
    {false, true} ->
      keep_dst(Op, Src, Dst, Map);
    {false, false} ->
      %% Both Dst and Src is popped.
      keep_none(Op, Src, Dst, Map)
  end.

keep_both(Op, Src, Dst, Map)->
  %% Keep both Dst and Src if it is there.
  {SwitchInsn, NewMap} = switch_first(Dst, Map),	
  NewSrc = get_new_opnd(Src, NewMap),
  Insn = format_fop(Op, NewSrc, mk_st(0)),
  {SwitchInsn++Insn, NewMap}.

keep_src(Op, Src, Dst, Map)->
  %% Pop Dst but keep Src in stack if it is there.
  {SwitchInsn, NewMap0} = switch_first(Dst, Map),
  NewSrc = get_new_opnd(Src, NewMap0),
  NewMap = pop(NewMap0),
  Insn = format_fop(Op, NewSrc, mk_st(0)),
  {SwitchInsn++Insn++pop_insn(), NewMap}.

keep_dst(Op, Src, Dst, Map)->
  %% Keep Dst but pop Src.
  %% Dst must be in stack.
  {PushInsn, NewMap0} = 
    case in_map(Dst, Map) of
      true -> {[], Map};
      _ -> push(Dst, Map) 
    end,
  case in_map(Src, NewMap0) of
    true->
      %% Src must be popped.
      {SwitchInsn, NewMap1} = switch_first(Src, NewMap0),
      NewDst = get_new_opnd(Dst,NewMap1),
      NewOp = mk_op_pop(Op),
      NewMap = pop(NewMap1),
      Insn = format_fop(NewOp, mk_st(0), NewDst),
      {PushInsn++SwitchInsn++
       Insn,NewMap};
    _ ->
      %% Src isn't in the map so it doesn't have to be popped.
      {SwitchInsn, NewMap} = switch_first(Dst, Map),
      {SwitchInsn++[#fop{dst=[],src=Src,op=Op}], NewMap}
  end.

keep_none(Op, Src, Dst, Map)->
  %% Dst must be on stack.
  {PushInsn, NewMap0} = 
    case in_map(Dst, Map) of
      true -> {[], Map};
      _ -> push(Dst, Map)
    end,
  case in_map(Src, NewMap0) of
    true->
      %% Src must be popped.
      {SwitchInsn1, NewMap1} = switch_first(Src, NewMap0),
      NewOp = mk_op_pop(Op),
      NewDst = get_new_opnd(Dst,NewMap1),
      NewMap2 = pop(NewMap1),
      %% Then Dst has to be popped.
      {PopInsn,NewMap} = pop_member(Dst,NewMap2),
      Insn = format_fop(NewOp, mk_st(0), NewDst),
      {PushInsn++SwitchInsn1++Insn++PopInsn,
       NewMap};
    _ ->
      %% Src isn't in the map so it doesn't have to be popped.
      {SwitchInsn, NewMap1} = switch_first(Dst, NewMap0),
      NewMap = pop(NewMap1),
      {SwitchInsn++[#fop{dst=[],src=Src,op=Op}]++pop_insn(),
       NewMap}
  end.

format_fop(Op, Src=#x86_temp{}, Dst=#x86_fpreg{reg=Reg}) ->
  %% Handle that st(0) is sometimes implicit.
  if Reg==0-> [hipe_x86:mk_fop(Op, Src, [])];
     true-> [hipe_x86:mk_fop(Op, Src, Dst)]
  end;
format_fop(Op, Src, Dst) ->
  [hipe_x86:mk_fop(Op, Src, Dst)].

in_map(X, Map) ->
  lists:member(X, Map).

push_list(L, Map) ->
  push_list(L, Map, []).
push_list([H|T], Map, Acc) ->
  {Insn, NewMap} = push(H,Map),
  push_list(T, NewMap, Acc++Insn);
push_list([], Map, Acc) ->
  {Acc, Map}.

push(X, Map0) ->
  {PopInsn, Map} = 
    if length(Map0)>7 -> pop_a_temp(Map0);
       true -> {[], Map0}
    end,
  NewX = get_new_opnd(X,Map),
  NewMap = [X | Map],
  PushOp = [hipe_x86:mk_fop(fld, NewX, [])],
  {PopInsn++PushOp, NewMap}.

free_all()->
  free_all(7,[]).
free_all(-1,Code)->
  Code;
free_all(X,Code)->
  free_all(X-1,[hipe_x86:mk_fop('ffree',mk_st(X),[])|Code]).

pop([_|Map]) ->
  Map.

pop_insn() ->
  [hipe_x86:mk_fop('fstp',mk_st(0),[])].

pop_dead(Dead, Map) ->
  Dead0 = [X || X <- Map, lists:member(X,Dead)],
  pop_dead(Dead0, Map, []).

pop_dead([D|Dead], Map, Code) ->
  {I, NewMap0} = switch_first(D, Map),
  NewMap = pop(NewMap0),
  Store = case D of
	    #x86_temp{}->[hipe_x86:mk_fop('fstp', [], D)];
	    _ -> ?EXIT({pseudoFloatInStackOnCall,{D}})
		   %%pop_insn()
		   end,
  pop_dead(Dead,NewMap,Code++I++Store);
pop_dead([],Map,Code) ->
  {Code,Map}.

pop_all(Map) ->
  {Code, _} = pop_dead(Map, Map),
  Code.

pop_member(Member, Map)->
  {Head,[_|T]} = lists:splitwith(fun(X)->X/=Member end,Map),
  {[hipe_x86:mk_fop('fstp', mk_st(get_pos(Member, Map, 0)),[])],
   Head++T}.

pop_a_temp(Map) ->
  Temp = find_a_temp(Map),
  {SwitchInsn, NewMap0} = switch_first(Temp, Map),
  NewMap = pop(NewMap0),
  {SwitchInsn++[hipe_x86:mk_fop('fstp', [], Temp)], NewMap}.

find_a_temp([H = #x86_temp{}|_])->
  H;
find_a_temp([_|T]) ->
  find_a_temp(T);
find_a_temp([]) ->
  ?EXIT({noTempOnFPStack,{}}).

switch_first(X, Map = [H|_]) ->
  Pos = get_pos(X, Map, 0),
  case Pos of
    0 -> 
      {[], Map};
    notFound ->
      push(X, Map);
    _ ->	    
      {[_|Head], [_|Tail]} = lists:splitwith(fun(Y)->Y/=X end, Map),
      NewMap = [X|Head]++[H|Tail],
      Ins = hipe_x86:mk_fop(fxch, mk_st(Pos), []),
      {[Ins], NewMap}
  end;
switch_first(X, Map) ->
  push(X, Map).

get_pos(X, [H|T], Pos) ->
  if X =:= H -> Pos;
     true -> get_pos(X, T, Pos+1)
  end;
get_pos(_, [], _) ->
  notFound.

get_new_opnd(X, Map) ->
  I = get_pos(X, Map, 0),
  case I of
    notFound ->
      %% The operand is probably a spilled float.
      X;
    _ ->
      mk_st(I)
  end.

is_fp(#x86_fpreg{}) ->
  true;
is_fp(#x86_mem{type=Type}) ->
  Type =:= 'double';
is_fp(#x86_temp{type=Type}) ->
  Type =:= 'double'.

handle_insn(I) ->
  case I of
    #fmov{} -> true;
    #fop{} -> true;
    #pseudo_call{}->true;
    _ -> false
  end.

is_liveOut(X, LiveOut) ->
  ordsets:is_element(X, LiveOut).

mk_st(X) ->
  hipe_x86:mk_fpreg(X, false).

mk_op_pop(Op) ->
  case Op of
    'fadd'-> 'faddp';
    'fdiv' -> 'fdivp';
    'fdivr' -> 'fdivrp';
    'fmul' -> 'fmulp';
    'fsub' -> 'fsubp';
    'fsubr' -> 'fsubrp';
    _ -> ?EXIT({operandHasNoPopVariant,{Op}})
	   end.

butlast([X|Xs]) -> butlast(Xs,X).
butlast([],_) -> [];
butlast([X|Xs],Y) -> [Y|butlast(Xs,X)].
