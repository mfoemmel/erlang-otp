%% -*- erlang-indent-level: 2 -*-
%%=======================================================================
%% File        : hipe_frame.erl
%% Author(s)   : Christer Jonsson and Erik Johansson
%% Description : Add stack-frames to calls. 
%%               Also handle stack and catch-frames.
%%=======================================================================
%% Notes:
%% TODO:  Cleanup the two passes add_save_restore and expand.
%%=======================================================================

-module(hipe_frame).
-export([add_save_restore/1,
	 expand/1]).

-include("hipe_literals.hrl").

add_save_restore(CFG) ->
 %% hipe_rtl_cfg:pp(CFG),
 %%   io:format("~w~n",[CFG]),
  Liveness = hipe_rtl_liveness:analyze(CFG),
  Labels = hipe_rtl_cfg:labels(CFG),
  {LLow, LHigh} = hipe_rtl_cfg:label_range(CFG),
  hipe_gensym:set_label(rtl,LHigh),
  {VLow, VHigh} = hipe_rtl_cfg:var_range(CFG),
  hipe_gensym:set_var(rtl,VHigh),
  add_save_restore(Labels, CFG, Liveness, 0, 0,[]).


%%
%% Apart from generating code to save frames around function calls
%% this procedure keeps track of the largest possible frame and adds
%% a stacktest at the start of the function.
%% If it is a leaf function that uses less than ?HIPE_SPARC_LEAF_WORDS
%% stack-words then no stack test is needed.
%% All other (non-leaf) functions on the other hand, has to ensure
%% that there is room for Need+?HIPE_SPARC_LEAF_WORDS words on the stack.
%% This function also adds catch-frames round those calls that need it.
%%
%%
%% Given a stack at a function call:
%%
%%   |           | <- SP
%%   |-----------|       ^
%%   | OLD FRAME |       |  Stack grows up.
%%   |           |
%%   |-----------| 
%%
%% Any live variables are stored on the stack
%% A call pushes arguments not passed in register on the stack and
%% then the old return address is pushed.
%%
%%   |           |  <- SP
%%   |-----------|
%%   | Arg N     |
%%   | .....     |
%%   | Arg 6     |
%%   | Ret Adr   |   
%%   | Save Var M|
%%   | .....     | 
%%   | Save Var 0| 
%%   |-----------|       ^
%%   | OLD FRAME |       |  Stack grows up.
%%   |           |      
%%   |-----------|
%%
%% If a call has a fail-label this indicates a call within a catch,
%% any exceptions within the call should be caught by the exception
%% handler at the fail-label. A special catch frame is then pushed to
%% the stack (just after the return address.)
%%
%%   |           |  <- SP
%%   |-----------|
%%   | Arg N     |
%%   | .....     |
%%   | Arg 6     |
%%   | Catch     |
%%   | Ret Adr   |   
%%   | Save Var M|
%%   | .....     | 
%%   | Save Var 0| 
%%   |-----------|       ^
%%   | OLD FRAME |       |  Stack grows up.
%%   |           |      
%%   |-----------|
%%
%%
%% When an ordinary call returns, the arguments have been popped by
%% the callee.
%% 
%%   |           |  <- SP
%%   | Ret Adr   |   
%%   | Save Var M|
%%   | .....     | 
%%   | Save Var 0| 
%%   |-----------|       ^
%%   | OLD FRAME |       |  Stack grows up.
%%   |           |      
%%   |-----------|
%%
%% The return address and the saved vars are then restored to
%% registers.
%%
%%   |           | <- SP
%%   |-----------|       ^
%%   | OLD FRAME |       |  Stack grows up.
%%   |           |
%%   |-----------| 
%%
%% A call within a catch can return in two ways: 
%%  1) As an ordinary return to the continuation label,
%%     then the catch word is still on the frame:
%% 
%%   |           |  <- SP
%%   | Catch     |
%%   | Ret Adr   |   
%%   | Save Var M|
%%   | .....     | 
%%   | Save Var 0| 
%%   |-----------|       ^
%%   | OLD FRAME |       |  Stack grows up.
%%   |           |      
%%   |-----------|
%%
%%   The catch word is poped
%% 
%%   |           |  <- SP
%%   | Ret Adr   |   
%%   | Save Var M|
%%   | .....     | 
%%   | Save Var 0| 
%%   |-----------|       ^
%%   | OLD FRAME |       |  Stack grows up.
%%   |           |      
%%   |-----------|
%%
%%   Then the registers are restored (including the return address.
%%
%%   |           | <- SP
%%   |-----------|       ^
%%   | OLD FRAME |       |  Stack grows up.
%%   |           |
%%   |-----------| 
%%
%%  2) If an exception is thrown then the call returns to the
%%     fail-label and the catch word is popped
%%
%% 
%%   |           |  <- SP
%%   | Ret Adr   |   
%%   | Save Var M|
%%   | .....     | 
%%   | Save Var 0| 
%%   |-----------|       ^
%%   | OLD FRAME |       |  Stack grows up.
%%   |           |      
%%   |-----------|
%%
%%   Then the registers are restored (including the return address.
%%
%%   |           | <- SP
%%   |-----------|       ^
%%   | OLD FRAME |       |  Stack grows up.
%%   |           |
%%   |-----------| 
%%
%% This scheme saves and pops all live variables at each call site.
%% An optimization pass: (see hipe_rtl_trim_frame.erl can reduce
%% the number of read and writes to a frame by saving vars in the
%% right order and then keeping them on the stack over several calls.)
%%

add_save_restore([Lbl|Lbls], CFG, Liveness, MaxF, SumC, Restores) ->
   BB = hipe_rtl_cfg:bb(CFG, Lbl),
   Code = hipe_bb:code(BB),
   LiveOut = hipe_rtl_liveness:liveout(Liveness, Lbl),
   {NewCode, MaxFb, SumCb, Restore} = add_save(Code, LiveOut),
   NewCFG = hipe_rtl_cfg:bb_update(CFG, Lbl, hipe_bb:code_update(BB, NewCode)),
   add_save_restore(Lbls, NewCFG, Liveness, 
		    max(MaxF,MaxFb), SumC+SumCb,
		    [Restore|Restores]);
add_save_restore([], CFG0, Liveness, MaxF, SumC, Restores) ->
  %% io:format("Restores ~w~n",[Restores]),
  CFG1 = add_restores(Restores, CFG0),
  %% Check if we really need a stack test
  Leaf = lists:member(leaf, hipe_rtl_cfg:info(CFG1)),
  
  CFG5 = 
    if (MaxF+SumC) =:= 0 -> CFG1;
       (MaxF+SumC) < ?HIPE_SPARC_LEAF_WORDS, Leaf =:= true -> CFG1;
       true ->
	Start = hipe_rtl_cfg:start_label(CFG1),
	 %% The variables we need to save across the call to stack_inc
	 Live = hipe_rtl_liveness:livein(Liveness, Start),
	 ToSave = ordsets:from_list(ordsets:subtract(Live, do_not_save())),
	 %% The maximum amount of stack this function needs
	 %% the ?HIPE_SPARC_LEAF_WORDS is somewhat ugly, but signifies the amount of stack
	 %% a leaf function can use without a stacktest
	 StackNeed = max(MaxF, length(ToSave)) + SumC +
	    hipe_sparc_registers:number_of_physical() + ?HIPE_SPARC_LEAF_WORDS,
	 %% Code to call function that extends the stack
	 OverflowLbl = hipe_rtl:mk_new_label(),
	 RetLbl = hipe_rtl:mk_new_label(),
	 OC = [hipe_rtl:mk_stackneed(length(ToSave)),
	       hipe_rtl:mk_save_frame(ToSave),
	       hipe_rtl:mk_call([], inc_stack_0, [], c, 
			   hipe_rtl:label_name(RetLbl), []), 
	       RetLbl,
	       hipe_rtl:mk_restore_frame(ToSave),
	       hipe_rtl:mk_pop_frame(length(ToSave)),
	       hipe_rtl:mk_goto(Start)],
	 OCb = hipe_bb:mk_bb(OC),
	 %% Code to test if we need more stack
	 NewStart = hipe_rtl:mk_new_label(),
	 SP = hipe_rtl:mk_reg(hipe_sparc_registers:stack_pointer()),
	 SL = hipe_rtl:mk_reg(hipe_sparc_registers:stack_limit()),
	 Tmp = hipe_rtl:mk_new_reg(),
	 TC = [hipe_rtl:mk_alu(Tmp, SP, add, hipe_rtl:mk_imm(StackNeed*4)),
	       hipe_rtl:mk_branch(Tmp, lt, SL, Start, hipe_rtl:label_name(OverflowLbl), 
			     0.99)],
	 TCb = hipe_bb:mk_bb(TC),
	 %% Update the cfg
	 CFG2 = hipe_rtl_cfg:bb_add(CFG1, hipe_rtl:label_name(NewStart), TCb),
	 CFG3 = hipe_rtl_cfg:start_label_update(CFG2, hipe_rtl:label_name(NewStart)),
	CFG4 = hipe_rtl_cfg:bb_add(CFG3, hipe_rtl:label_name(OverflowLbl), OCb)
    end,
  {VLow, _} = hipe_rtl_cfg:var_range(CFG5),
  CFG6 = hipe_rtl_cfg:var_range_update(CFG5, {VLow, hipe_gensym:get_var(rtl)}),
  {LLow, _} = hipe_rtl_cfg:label_range(CFG6),
  hipe_rtl_cfg:label_range_update(CFG6, {LLow, hipe_gensym:get_label(rtl)}).


add_save([], LiveOut) ->
  {[], 0, 0, []};
add_save([I], LiveOut) ->
  case hipe_rtl:type(I) of
    call ->
      Live0 = ordsets:subtract(LiveOut, ordsets:from_list(hipe_rtl:call_dst(I))),
      ToSave = ordsets:from_list(ordsets:subtract(Live0, do_not_save())),
      check_saves(ToSave),
      Need = length(ToSave),
      Cont = hipe_rtl:call_continuation(I),
      NewCont = hipe_rtl:label_name(hipe_rtl:mk_new_label()),
      Fail = hipe_rtl:call_fail(I),
      case Fail of
	[] -> %% Not in a catch
	  NewI = hipe_rtl:call_continuation_update(
		   I,
		   NewCont),
	  {[hipe_rtl:mk_stackneed(Need),
	    hipe_rtl:mk_save_frame(ToSave),
	    NewI], 
	   Need, 0, {ToSave,{Cont, NewCont}}};
	_ -> %% In a catch
	  NFail = hipe_rtl:label_name(hipe_rtl:mk_new_label()),
	  NewI = hipe_rtl:call_continuation_update(
		   hipe_rtl:call_fail_update(I, NFail),
		   NewCont),
	  SP = hipe_rtl:mk_reg(hipe_sparc_registers:stack_pointer()),
	  IncStack = hipe_rtl:mk_alu(SP, SP, add, hipe_rtl:mk_imm((Need+1)*4)),
	  FrameCode = save_vars(ToSave, SP, 0, 
				hipe_tagscheme:write_catch_frame(SP, Need*4, NFail)++
				[IncStack,NewI]),
	  {FrameCode,
	   Need+1, 0, {ToSave,{Cont, NewCont},
		       {Fail,NFail,hipe_rtl:call_dst(NewI), Need}}}
      end;
    _ ->
      {[I], 0, 0, []}
  end;
add_save([I|Is], LiveOut) ->
  case hipe_rtl:type(I) of
    call ->
      exit({I,Is});
    stackneed ->
      {NewIs, MaxF, SumC, Restores} = 
	add_save(Is, LiveOut),
      {[I|NewIs], MaxF, SumC+hipe_rtl:stackneed_words(I),
       Restores};
    _ ->
      {NewIs, MaxF, SumC, Restores} = 
	add_save(Is, LiveOut),
      {[I|NewIs], MaxF, SumC, Restores}
  end.


add_restores([{ToRestore,{Cont,NewC}}|Restores], CFG) ->
  %% Not in a catch.
  Code =
    [hipe_rtl:mk_restore_frame(ToRestore),
     hipe_rtl:mk_pop_frame(length(ToRestore)),
     hipe_rtl:mk_goto(Cont)],
  NewCFG = hipe_rtl_cfg:bb_add(CFG, NewC, hipe_bb:mk_bb(Code)),
  add_restores(Restores, NewCFG);
add_restores([{ToRestore,{Cont,NewC},{Fail,NewF,Dst, Need}}|Restores], CFG) ->
  %% In a catch.
  SP = hipe_rtl:mk_reg(hipe_sparc_registers:stack_pointer()),
  R = hipe_rtl:mk_new_reg(),
  CCode =
    [hipe_rtl:mk_alu(SP, SP, sub, hipe_rtl:mk_imm(4)),
     hipe_rtl:mk_restore_frame(ToRestore),
     hipe_rtl:mk_pop_frame(Need),
     hipe_rtl:mk_goto(Cont)],
  CCFG = hipe_rtl_cfg:bb_add(CFG, NewC, hipe_bb:mk_bb(CCode)),
  FCode =
%%    move_args_to_vars(Dst) ++
    [hipe_rtl:mk_restore_frame(ToRestore),
     hipe_rtl:mk_pop_frame(Need),
     hipe_rtl:mk_goto(Fail)],
  FCFG = hipe_rtl_cfg:bb_add(CCFG, NewF, hipe_bb:mk_bb(FCode)),
  add_restores(Restores, FCFG);
add_restores([[]|Restores], CFG) ->
  add_restores(Restores,CFG);
add_restores([],CFG) -> CFG.



check_saves([]) ->
   ok;
check_saves([S|Ss]) ->
   case hipe_rtl:is_var(S) of
      true ->
	 check_saves(Ss);
      false ->
	 exit({?MODULE, {"illegal save", S}})
   end.


max(X, Y) when X > Y -> X;
max(X, Y) -> Y.


%%
%% The set of registers that is not to be saved in a frame
%%

do_not_save() ->
   ordsets:from_list(lists:map(fun(R) -> hipe_rtl:mk_reg(R) end,
			       hipe_sparc_registers:global())).

%%
%%
%%

expand(CFG) ->
   Labels = hipe_rtl_cfg:labels(CFG),
   {LowLbl, HighLbl} = hipe_rtl_cfg:label_range(CFG),
   hipe_gensym:set_label(rtl,HighLbl),
   {LowVar, HighVar} = hipe_rtl_cfg:var_range(CFG),
   hipe_gensym:set_var(rtl,HighVar),
   CFG0 = add_head(expand0(Labels, CFG)),
   CFG1 = hipe_rtl_cfg:label_range_update(CFG0, {LowLbl, hipe_gensym:get_label(rtl)}),
   hipe_rtl_cfg:var_range_update(CFG1, {LowVar, hipe_gensym:get_var(rtl)}).



add_head(CFG) ->
   StartLabel = hipe_rtl_cfg:start_label(CFG),
   Params = hipe_rtl_cfg:params(CFG),
   Code = move_args_to_vars(Params) ++ [hipe_rtl:mk_goto(StartLabel)],
   BB = hipe_bb:mk_bb(Code),
   NewLabel = hipe_rtl:label_name(hipe_rtl:mk_new_label()),
   CFG0 = hipe_rtl_cfg:bb_update(CFG, NewLabel, BB),
   hipe_rtl_cfg:start_label_update(CFG0, NewLabel).

expand0(Lbls, CFG) ->
  NewCode = expand0(Lbls, [], CFG),
  hipe_rtl_cfg:update_code(CFG, NewCode).


expand0([], Acc, CFG) ->
   lists:flatten(Acc);
expand0([Lbl|Lbls], Acc, CFG) ->
   BB = hipe_rtl_cfg:bb(CFG, Lbl),
   Code = hipe_bb:code(BB),
   %% io:format("Code:~w\n",[Code]),
   NewCode = block_expand(Code),
   expand0(Lbls, [Acc,hipe_rtl:mk_label(Lbl),NewCode], CFG).

%%
%%
%%

block_expand([]) ->
   [];
block_expand([I|Is]) ->
   %% io:format("I:~w~n",[I]),
   case hipe_rtl:type(I) of
      call ->
	 Args = hipe_rtl:call_args(I),
 	 Dst = hipe_rtl:call_dst(I),
         RetLbl = hipe_rtl:mk_new_label(),
	 Cont = hipe_rtl:call_continuation(I),
	 Fail = hipe_rtl:call_fail(I),
	 {RegArgs, Moves1} = move_vars_to_args(Args),
	 Jsr = hipe_rtl:mk_jsr(hipe_rtl:call_fun(I), hipe_rtl:call_type(I), RegArgs,
			  hipe_rtl:label_name(RetLbl), 
			  Fail),
        
         Goto = hipe_rtl:mk_goto(Cont),
	 Moves2 = move_args_to_vars(Dst),
	 [Moves1, Jsr,RetLbl, Moves2, Goto | block_expand(Is)];
      enter ->
	 Args = hipe_rtl:enter_args(I),
	 {RegArgs, Moves} = move_vars_to_args(Args),
	 Esr = hipe_rtl:mk_esr(hipe_rtl:enter_fun(I), hipe_rtl:enter_type(I), RegArgs),
	 Moves++[Esr|block_expand(Is)];
      return ->
	 CP = hipe_rtl:mk_var(hipe_sparc_registers:return_address()),
	 {RegArgs, Moves} = move_vars_to_args(hipe_icode:return_vars(I)),
	 Jmp = hipe_rtl:mk_jmp(CP, hipe_rtl:mk_imm(8), RegArgs),
	 Moves ++ [Jmp|block_expand(Is)];
      save_frame ->
	 case hipe_rtl:save_frame_vars(I) of
           [] -> block_expand(Is);
	   Vars ->
	     SP = hipe_rtl:mk_reg(hipe_sparc_registers:stack_pointer()),
	     IncStack = hipe_rtl:mk_alu(SP, SP, add, hipe_rtl:mk_imm(length(Vars)*4)),
	     Code = save_vars(Vars, SP, 0, [IncStack]),
	     [hipe_rtl:mk_comment(save_frame) | Code] ++ block_expand(Is)
	 end;
      restore_frame ->
         case hipe_rtl:restore_frame_vars(I) of
	   [] -> block_expand(Is);
	 Vars ->
	     SP = hipe_rtl:mk_reg(hipe_sparc_registers:stack_pointer()),
	     Code = restore_vars(lists:reverse(Vars), SP, 0, []),
	     [hipe_rtl:mk_comment(restore_frame) | Code] ++ block_expand(Is)
	 end;
      restore_catch ->
	 Dst = hipe_rtl:restore_catch_vars(I),
         Moves = move_args_to_vars(Dst),
	 [hipe_rtl:mk_comment({restore_catch, Dst}) | Moves] ++ block_expand(Is);
      fail_to ->
	 Dst = [hipe_rtl:fail_to_reason(I)],
         {RegArgs, Moves} = move_vars_to_args(Dst),
         L = hipe_rtl:fail_to_label(I),
	 [hipe_rtl:mk_comment(fail_to) | Moves] ++ [hipe_rtl:mk_goto(L)] ++ block_expand(Is);

      pop_frame ->
	 case hipe_rtl:pop_frame_size(I) of
	    0 ->
	       block_expand(Is);
	    FrameSize ->
	       SP = hipe_rtl:mk_reg(hipe_sparc_registers:stack_pointer()),
	       NewI = hipe_rtl:mk_alu(SP, SP, 'sub', hipe_rtl:mk_imm(FrameSize*4)),
	       [hipe_rtl:mk_comment(pop_frame), NewI | block_expand(Is)]
	 end;
      stackneed ->
	 block_expand(Is);
      X ->
	 [I|block_expand(Is)]
   end.

save_vars([], _, _, Code) -> Code;
save_vars([Var | Vars], SP, I, Code) ->
   save_vars(Vars, SP, I+4, save_var(Var, SP, hipe_rtl:mk_imm(I), Code)).
save_var(Var, SP, Off, Code) ->
   case is_CP(Var) of
      true -> hipe_tagscheme:save_CP(Var, SP, Off, Code);
      false -> [hipe_rtl:mk_store(SP, Off, Var) | Code]
   end.

restore_vars([], _, _, Code) -> Code;
restore_vars([Var | Vars], SP, I, Code) ->
   restore_vars(Vars, SP, I+4, restore_var(Var, SP, hipe_rtl:mk_imm(-I-4), Code)).
restore_var(Var, SP, Off, Code) ->
   case is_CP(Var) of
      true -> hipe_tagscheme:restore_CP(Var, SP, Off, Code);
      false -> [hipe_rtl:mk_load(Var, SP, Off) | Code]
   end.

%%
%% Test if a save/restore variable is CP. We make sure that CP
%% always is an rtl var, not reg, to simplify the test.
%%
%% The JAM compiler sometimes creates unused variables (with "alloc").
%% IX isn't able to eliminate these from saved frames, but it is able
%% to propagate their initial values (nil) to all use sites.
%% Therefore, save_frame var lists sometimes contain rtl immediates.
%% At the moment, this has only been observed in save_frames constructed
%% for JAM pushCatch instructions.
%%
is_CP(Var) ->
   case hipe_rtl:is_var(Var) of
      true -> hipe_rtl:var_name(Var) == hipe_sparc_registers:return_address();
      false -> false
  end.


%%
%% Generate instructions so that a list of variables end up
%% in registers and on the stack ready for a function call.
%% Returns a tuple {RegArgs, Code} where RegArgs is a list
%% of registers used to pass arguments.
%%

move_vars_to_args(Vars) ->
  {InRegs, OnStack} = get_arg_pos(Vars, hipe_sparc_registers:register_args(),[]),

  NoRegArgs = length(InRegs),
  case NoRegArgs > 0 of
    true ->
      RegArgs = arg_vars(NoRegArgs-1),
      I = hipe_rtl:mk_multimove(RegArgs, InRegs),

      StackArgs = length(OnStack),
      case StackArgs > 0 of
	true ->
	  {RegArgs, [I|move_vars_to_args(OnStack, NoRegArgs)]};
	false ->
	  {RegArgs, [I]}
      end;
    false ->
      {[], []}
  end.
  


move_vars_to_args([], ArgIndex) ->
  RegArgs = hipe_sparc_registers:register_args(),
  SP = hipe_rtl:mk_reg(hipe_sparc_registers:stack_pointer()),
  StackGrowth = (ArgIndex-RegArgs)*4,
  [hipe_rtl:mk_alu(SP, SP, 'add', hipe_rtl:mk_imm(StackGrowth))];

move_vars_to_args([V|Vs], ArgIndex) ->
  RegArgs = hipe_sparc_registers:register_args(),
  Code = move_vars_to_args(Vs, ArgIndex+1),
  Off = (ArgIndex-RegArgs)*4,
  SP = hipe_rtl:mk_reg(hipe_sparc_registers:stack_pointer()),
  I = hipe_rtl:mk_store(SP, hipe_rtl:mk_imm(Off), V),
  [I|Code].


%%
%% Generate instructions so that the arguments to a function end up 
%% in registers.
%%

move_args_to_vars(Vars) ->
  {InRegs,OnStack} = get_arg_pos(Vars, hipe_sparc_registers:register_args(),[]),
  move_args_to_vars(InRegs, OnStack).

get_arg_pos([],N,InRegs) ->
  {lists:reverse(InRegs),[]};

get_arg_pos([V|Vs], N, InRegs) when N > 0 ->
  get_arg_pos(Vs, N-1, [V|InRegs]);
get_arg_pos(Args, N, InRegs) ->
  { lists:reverse(InRegs), Args}.

move_args_to_vars(InRegs, OnStack) ->

  RegArgs = length(InRegs),
  case RegArgs > 0 of
    true ->
      StackArgs = length(OnStack),
      I = hipe_rtl:mk_multimove(InRegs, arg_vars(RegArgs-1)),
      case StackArgs > 0 of
	true ->
	  [I|move_args_to_vars(OnStack, RegArgs, StackArgs+RegArgs)];
	false ->
	  [I]
      end;
    false ->
      []
  end.
   
move_args_to_vars([],ArgIndex,_) ->
  RegArgs = hipe_sparc_registers:register_args(),
  SP = hipe_rtl:mk_reg(hipe_sparc_registers:stack_pointer()),
  StackDec = (ArgIndex-RegArgs)*4,
  [hipe_rtl:mk_alu(SP, SP, 'sub', hipe_rtl:mk_imm(StackDec))];

move_args_to_vars([V|Vs], ArgIndex, Arity) -> 
  SP = hipe_rtl:mk_reg(hipe_sparc_registers:stack_pointer()),
  StackIndex = (ArgIndex-Arity)*4,
  I = hipe_rtl:mk_load(V, SP, hipe_rtl:mk_imm(StackIndex)),
  [I | move_args_to_vars(Vs, ArgIndex+1, Arity)].


%%
%% Return a variable corresponding to argument number 'X'
%%

arg_var(X) ->
   hipe_rtl:mk_var(hipe_sparc_registers:arg(X)).

arg_vars(N) ->
  arg_vars(N,[]).

arg_vars(N, Acc) when N >= 0 ->
  arg_vars(N-1, [arg_var(N)|Acc]);
arg_vars(_, Acc) -> Acc.





			   
