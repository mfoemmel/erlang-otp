%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%%  Filename : 	hipe_sparc_frame.erl
%%  Module   :	hipe_sparc_frame
%%  Purpose  :  
%%  Notes    :  
%%              The code may only have spilled registers in
%%              the pseudo instructions: 
%%
%%                pseudo_spill   t1, t2 ; spills t1 to spillpos of t2
%%                pseudo_unspill t1, t2 ; loads t1 from spillpos of t2 
%% 
%%              The TempMap must contain mappings from all used temps
%%              to physical registers or spill positions.   
%%
%%              XXX: New immediates are introduced as stack offsets.
%%                   If more than 1024 stack slots are needed,
%%                   the immediate offsets will be to big for a
%%                   sparc instruction.
%%                   This can happen with the naive allocator.
%%                   A special pass called from hipe_main 
%%                   rectifies this situation.
%% 
%%  History  :	* 2001-10-25 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: kostis $
%%              $Date: 2005/03/16 20:48:40 $
%%              $Revision: 1.31 $
%% ====================================================================
%%  Exports  :
%%
%% TODO:
%%  Optimize 
%%  Handle multiple return values
%%
%% Testcases:
%% hipe:c({len,len,2},[pp_rtl,{regalloc,naive}]).
%% hipe:c({len,len,2},[pp_sparc,{regalloc,lfls}]).
%% hipe:c({late,t,0},[pp_sparc,{regalloc,lfls}]).
%% hipe:c({late,seven,7},[pp_sparc,{regalloc,lfls}]).
%% hipe:c({late,test_enter,7},[pp_sparc,{regalloc,lfls}]).
%% hipe:c({trivial_19,test,0},[pp_sparc,{regalloc,lfls}]).
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc_frame).
-export([frame/5]).

%% -define(DO_ASSERT,true).
-include("../main/hipe.hrl").
-include("../rtl/hipe_literals.hrl").

-define(MK_CP_REG(),(hipe_sparc:mk_reg(hipe_sparc_registers:return_address()))).
-define(MK_SP_REG(),(hipe_sparc:mk_reg(hipe_sparc_registers:stack_pointer()))).
-define(MK_TEMP_REG(),(hipe_sparc:mk_reg(hipe_sparc_registers:temp0()))).
-define(MK_TEMP_FPREG(),(hipe_sparc:mk_fpreg(0))).
-define(IS_SPILLED(Reg,Map),hipe_temp_map:is_spilled(hipe_sparc:reg_nr(Reg),Map)).

frame(Cfg, TempMap, FpMap, NextSpillPos, _Options) ->
  %%  io:format("~p\n",[TempMap]),
  rewrite(Cfg, TempMap, FpMap, NextSpillPos).


rewrite(Cfg, TempMap, FpMap, NextSpillPos) ->
  %%  hipe_sparc_cfg:pp(Cfg),

  Sparc = hipe_sparc_cfg:linearize(Cfg),
  Arity = hipe_sparc:sparc_arity(Sparc),
  Code = hipe_sparc:sparc_code(Sparc),

  
  %% ConvNeed is added if there are any conv_fp instructions.
  ConvNeed = 
    case get(hipe_inline_fp) of
      true ->  
      	lists:foldl(
	  fun(I, Acc) -> case hipe_sparc:is_conv_fp(I) of true->1; _->Acc end
	  end, 0, Code);
      _->
	0
    end,
  
  %% Need is stack positions for spills
  Need = NextSpillPos + 1 + ConvNeed, 
  %% +1 for CP, not realy needed for leaf
  %%            functions... 
  
  %% ExtraNeed is room for argumets in calls. 
  ExtraNeed = extra_stack_need(Code,Need,Arity),
  StackNeed = Need + ExtraNeed,

  SP = ?MK_SP_REG(),
  CP = ?MK_CP_REG(),
  NewStart = hipe_sparc:label_create_new(),

  {StackTestCode, StackIncCode} =  
    gen_stack_test(StackNeed, SP, CP, Arity, Cfg),
  AllocCode = gen_alloc_code(Need*4, SP),

  EntryCode = 
    [NewStart,
     StackTestCode,  %% (We will flatten the list later.)
     AllocCode,
     gen_save_cp(Need)],

  {RetCode,RetLabel} = gen_ret(Need, Arity),
  
  NewCode = 
    [EntryCode,
     %% Rewrite instructions to use physical registers 
     %% instead of temps.
     %% Also, handle pseudo instructions.
     if is_tuple(TempMap) ->
	 [rewrite_instr(I, TempMap, FpMap, Need, Arity, RetLabel) || 
	   I <- Code];
	true ->
	 rewrite_instrs(Code, [], TempMap, FpMap, Need, Arity, RetLabel)
     end,
     RetCode,
     %% code to call increment stack.
     StackIncCode],

  %% Create a new CFG
  NewSparc = hipe_sparc:sparc_code_update(Sparc, lists:flatten(NewCode)),
  CFG0 = hipe_sparc_cfg:init(NewSparc),
  %% io:format("\n\n\n\n\n\n\n\nAfter frame\n"),
  %%  hipe_sparc_cfg:pp(CFG1),
  CFG0.

rewrite_instrs([I|Is], TempMap, TempMaps, FpMap, Need, Arity, RetLabel) ->
  NewMap = 
    case hipe_sparc:is_label(I) of
      true ->
%%	io:format("~w\n",[hipe_vectors:get(TempMaps, hipe_sparc:label_name(I))]),
      	hipe_vectors:get(TempMaps, hipe_sparc:label_name(I));
      false ->
	TempMap
    end,
  [rewrite_instr(I, NewMap, FpMap, Need, Arity, RetLabel) |
   rewrite_instrs(Is, NewMap, TempMaps, FpMap, Need, Arity, RetLabel)];
rewrite_instrs([],_,_,_,_,_,_) -> [].
    

rewrite_instr(I, TempMap, FpMap, Need, Arity, RetLabel) ->
  %%  io:format("\n\n~w -> \n",[I]),
  I1 = remap_fp_regs(I, FpMap),
  I2 = rewrite_instr2(I1, TempMap, FpMap,Need, Arity, RetLabel),
  %%  io:format("~w\n",[I2]),
  I2.

remap_fp_regs(I, FpMap) ->
  case hipe_sparc:type(I) of
    %% These instructions handles their own remapping.
    fmove -> I;
    pseudo_spill-> I;
    pseudo_unspill-> I;
    _->
      Defs = hipe_sparc:fp_reg_defines(I),
      NewI = remap_fp(remap_fp(I, Defs, FpMap), 
		      hipe_sparc:fp_reg_uses(I),
		      FpMap),
      NewI
  end.

rewrite_instr2(I, TempMap, FpMap, Need, Arity, RetLabel) ->

  case hipe_sparc:type(I) of
    pseudo_push ->
      ?EXIT({pseudo_push, not_supportet_any_more,I});
    pseudo_pop ->
      Temp = hipe_sparc:pseudo_pop_reg(I),
      Reg = map(Temp,TempMap),
      %% Arg position on stack = -(arg index + need)*4 
      Pos = - ((hipe_sparc:imm_value(
		  hipe_sparc:pseudo_pop_index(I)) +
		Need) * 4),
      gen_stack_load(Pos, Reg);
    move ->
      rewrite_move(I, TempMap);
    pseudo_spill ->
      rewrite_spill(I, TempMap, FpMap);
    pseudo_unspill ->
      rewrite_unspill(I, TempMap, FpMap);

    call_link ->
      rewrite_call(I, TempMap, Need, Arity);

    pseudo_return ->
      %% Note we assume that the arguments to return have been 
      %% mapped to physical register in the translation RTL->SPARC.

      %% XXX: Fix if we start using multiple return values
      %%      Acctually just fix gen_ret/2 to annotate the
      %%      jmp with the live registers...
      case length(hipe_sparc:pseudo_return_regs(I)) == 1 of
	true -> %% Just one retval all is ok
	  hipe_sparc:goto_create(RetLabel);
	false ->
	  %% Number of retvalues not 1.
	  ?EXIT({multiple_retvals_not_handled,I})
      end;

    pseudo_enter ->
      rewrite_enter(I, TempMap, Need, Arity);

    fmove ->
      rewrite_fmove(I, TempMap, FpMap);
    load_fp -> 
      rewrite_load_fp(I, TempMap);
    store_fp -> 
      rewrite_store_fp(I, TempMap);
    conv_fp ->
      NewI = map_temps(I, TempMap),
      Source = hipe_sparc:conv_fp_src(NewI),
      Dest = hipe_sparc:conv_fp_dest(NewI),
      Off = hipe_sparc:mk_imm(get_offset(Need - 1)),
      SP = hipe_sparc:mk_reg(hipe_sparc_registers:stack_pointer()),

      [hipe_sparc:store_create(SP, Off, w, Source),
       hipe_sparc:load_fp_create(Dest, 32, single, SP, Off),
       hipe_sparc:conv_fp_create(Dest, Dest)];
    	
    _ -> 
      %% All other instructions.
      %% No special handling, just map the temps to regs or spill positions.
      map_temps(I, TempMap)
  end.


rewrite_enter(I, TempMap, Need, Arity) ->
  %% Calculate number of args (to current fun) on stack
  %% Consider moving this out of the loop.
  NoRegArgs = hipe_sparc_registers:register_args(),
  ArgsOnStack = 
    if Arity > NoRegArgs -> Arity - NoRegArgs; 
       true -> 0 
    end,

  %% Get arguments to enter.
  Args = hipe_sparc:pseudo_enter_args(I), 
  RegArgs = lists:sublist(Args,1,NoRegArgs),
  %% Get target (closure or address).
  T = hipe_sparc:pseudo_enter_target(I),
  {Target,GetTInstr} =
    case hipe_sparc:pseudo_enter_is_known(I) of
      false ->
	{map(T, TempMap),[]};
      true ->
	TR = ?MK_TEMP_REG(),
	TT = case hipe_sparc:pseudo_enter_type(I) of
	       remote -> remote_function;
	       not_remote -> local_function
	     end,
	TI = hipe_sparc:load_address_create(TR, T, TT),
	{TR,TI}
    end,
  CP = ?MK_CP_REG(),

  %% The code for enter.
  [
   %% Restore CP from stack
   %% TODO: Do this only in non-leaf functions.
   gen_stack_load(-(Need*4),CP),
   
   %% If arguments to the called function spill over to the 
   %% stack, do some shuffling.
   %% Otherwise just adjust the stackpointer.
   case length(Args) > NoRegArgs of
     true ->
       enter_stack_args(Args, Need, TempMap, ArgsOnStack);
     false ->
       gen_dealloc_code((ArgsOnStack+Need)*4)
   end,
   
   %% Jump to the target function.
   GetTInstr,
   hipe_sparc:jmp_create(Target, hipe_sparc:mk_imm(0), 
			 [map(R,TempMap)||R <- RegArgs] , [])].

map_temps(I, TempMap)->
  Defs = hipe_sparc:keep_registers(hipe_sparc:defines(I)),
  Uses = hipe_sparc:keep_registers(hipe_sparc:uses(I)),
  remap(remap(I, Defs, TempMap), Uses, TempMap).


rewrite_call(Call, TempMap, Need, Arity) ->
  I = set_stack_size(Call,Need, Arity),
  Args = hipe_sparc:call_link_args(I),
  PushCode = push_call_args(Args, TempMap),
  NewI = remap_call_regs(I, TempMap),
  [PushCode,NewI].


remap_call_regs(I, TempMap) ->
  Defs = hipe_sparc:keep_registers(hipe_sparc:defines(I)),
  ArgsInRegs = lists:sublist(hipe_sparc:call_link_args(I), 
			     hipe_sparc_registers:register_args()),
  case hipe_sparc:call_link_is_known(I) of
    false ->
      Target = hipe_sparc:call_link_target(I),
      case ?IS_SPILLED(Target,TempMap) of
	true ->
	  RemappedCall = remap(I, [(Defs ++ ArgsInRegs)],TempMap),
	  Temp = ?MK_TEMP_REG(),
	  Pos = get_spill_offset(hipe_temp_map:find(hipe_sparc:reg_nr(Target), TempMap)),
	  [gen_stack_load(Pos, Temp),
	   hipe_sparc:call_link_target_update(RemappedCall, Temp)];
	false ->
	  remap(I, [Target | (Defs ++ ArgsInRegs)],TempMap)
      end;
    true ->
      remap(I,Defs ++ ArgsInRegs, TempMap)
  end.


%% ____________________________________________________________________
%%
%% Returns: 	
%% Arguments:	
%% Description:	 
%% ____________________________________________________________________
rewrite_move(I, TempMap) ->
  Src = hipe_sparc:move_src(I),
  Dest = hipe_sparc:move_dest(I),
  DestReg = hipe_sparc:reg_nr(Dest),

  case hipe_sparc:is_reg(Src) of
    false ->
      case hipe_temp_map:is_spilled(DestReg,TempMap) of
	true ->
	  Pos = get_spill_offset(hipe_temp_map:find(DestReg, TempMap)),
	  Reg = ?MK_TEMP_REG(),
	  [hipe_sparc:move_dest_update(I,Reg)|
	   gen_stack_store(Pos, Reg)];
	false ->
	  remap(I, [Dest], TempMap)
      end;
    true ->
      SrcReg = hipe_sparc:reg_nr(Src),

  
      case {hipe_temp_map:is_spilled(SrcReg,TempMap),
	    hipe_temp_map:is_spilled(DestReg,TempMap)} of
	{false,true} -> %% Spill
	  Pos = get_spill_offset(hipe_temp_map:find(DestReg, TempMap)),
	  Reg = map(Src,TempMap),
	  gen_stack_store(Pos, Reg);
	{true, false} -> %% unspill
	  Pos = get_spill_offset(hipe_temp_map:find(SrcReg, TempMap)),
	  Reg = map(Dest,TempMap),
	  gen_stack_load(Pos, Reg);
	{false,false} ->
	  remap(I, [Src,Dest], TempMap);
	{true,true} ->
	  Pos1 = get_spill_offset(hipe_temp_map:find(SrcReg, TempMap)),
	  Pos2 = get_spill_offset(hipe_temp_map:find(DestReg, TempMap)),
	  Reg = ?MK_TEMP_REG(),
	  [gen_stack_load(Pos1, Reg),
	   gen_stack_store(Pos2, Reg)]
      end
  end.

%% ____________________________________________________________________
%%      
%% Returns: 	
%% Arguments:	
%% Description:	 
%% ____________________________________________________________________
rewrite_spill(I, TempMap, FpMap) ->
  SrcReg = hipe_sparc:pseudo_spill_reg(I),
  Pos = get_spill_offset(hipe_sparc:pseudo_spill_pos(I)),

  case hipe_sparc:is_fpreg(SrcReg) of
    true ->
      SrcFpRegNr = hipe_sparc:fpreg_nr(SrcReg),
      case hipe_temp_map:in_fp_reg(SrcFpRegNr,FpMap) of
	false ->
	  %% Spilling a spilled reg...
	  ?ASSERT(check_spill(SrcFpRegNr,FpMap,Pos,I)),
	  hipe_sparc:comment_create({already_spilled,I});
	true -> %% Not spilled...
	  Reg = map_fp(SrcReg, FpMap),
	  gen_stack_store_fp(Pos, Reg)
      end;
    false ->
      SrcRegNr = hipe_sparc:reg_nr(SrcReg),
      case hipe_temp_map:in_reg(SrcRegNr,TempMap) of
	false ->
	  %% Spilling a spilled reg...
	  ?ASSERT(check_spill(SrcRegNr,TempMap,Pos,I)),
	  hipe_sparc:comment_create({already_spilled,I});
	true -> %% Not spilled...
	  Reg = map(SrcReg,TempMap),
	  gen_stack_store(Pos, Reg)
      end
  end.

%% ____________________________________________________________________
%%      
%% Returns: 	
%% Arguments:	
%% Description:	 
%% ____________________________________________________________________
rewrite_unspill(I, TempMap, FpMap) ->
  DstReg = hipe_sparc:pseudo_unspill_reg(I),
  Pos = get_spill_offset(hipe_sparc:pseudo_unspill_pos(I)),
  
  case hipe_sparc:is_fpreg(DstReg) of
    true ->
      DstFpRegNr = hipe_sparc:fpreg_nr(DstReg),
      case hipe_temp_map:in_fp_reg(DstFpRegNr,FpMap) of
	false ->
	  %% Unspilling to a spilled fpreg...
	  ?ASSERT(check_spill(DstFpRegNr,FpMap,Pos,I)),
	  hipe_sparc:comment_create({already_unspilled,I});
	true -> %% Not spilled...
	  Reg = map_fp(DstReg, FpMap),
	  gen_stack_load_fp(Pos, Reg)
      end;
    false ->
      DstRegNr = hipe_sparc:reg_nr(DstReg),
      case hipe_temp_map:in_reg(DstRegNr,TempMap) of
	false -> %% Unspilling to a spilled reg.
	  ?ASSERT(check_spill(DstRegNr,TempMap,Pos,I)),
	  hipe_sparc:comment_create({already_unspilled,I});
	true -> %% Not spilled.
	  Reg = map(DstReg,TempMap),
	  gen_stack_load(Pos,Reg)
      end
  end.

-ifdef(DO_ASSERT).
check_spill(RegNr,TempMap,Pos,I) ->
  case (catch hipe_temp_map:find(RegNr, TempMap)) of
    unknown -> true;
    DPos -> (get_spill_offset(DPos) =:= Pos)
  end.
-endif.

rewrite_fmove(I0, TempMap, FpMap)->
  I = map_temps(I0, TempMap),
  Src = hipe_sparc:fmove_src(I),
  Dest = hipe_sparc:fmove_dest(I),
  DestReg = hipe_sparc:fpreg_nr(Dest),

  case hipe_sparc:is_fpreg(Src) of
    false -> %% Can this happen?
      case hipe_temp_map:is_spilled(DestReg,FpMap) of
	true ->
	  Pos = get_spill_offset(hipe_temp_map:find(DestReg, FpMap)),
	  Reg = ?MK_TEMP_FPREG(),
	  [hipe_sparc:fmove_dest_update(I,Reg)|
	   gen_stack_store_fp(Pos, Reg)];
	false ->
	  remap_fp(I, [Dest], FpMap)
      end;
    true ->
      SrcReg = hipe_sparc:fpreg_nr(Src),

      case {hipe_temp_map:is_spilled(SrcReg,FpMap),
	    hipe_temp_map:is_spilled(DestReg,FpMap)} of
	{false,true} -> %% Spill
	  Pos = get_spill_offset(hipe_temp_map:find(DestReg, FpMap)),
	  Reg = map_fp(Src,FpMap),
	  gen_stack_store_fp(Pos, Reg);
	{true, false} -> %% unspill
	  Pos = get_spill_offset(hipe_temp_map:find(SrcReg, FpMap)),
	  Reg = map_fp(Dest,FpMap),
	  gen_stack_load_fp(Pos, Reg);
	{false,false} ->
	  remap_fp(I, [Src,Dest], FpMap);
	{true,true} ->
	  Pos1 = get_spill_offset(hipe_temp_map:find(SrcReg, FpMap)),
	  Pos2 = get_spill_offset(hipe_temp_map:find(DestReg, FpMap)),
	  Reg = ?MK_TEMP_FPREG(),
	  [gen_stack_load_fp(Pos1, Reg),
	   gen_stack_store_fp(Pos2, Reg)]
      end
  end.  

rewrite_load_fp(I, TempMap)->
  NewI = map_temps(I, TempMap),
  Source = hipe_sparc:load_fp_src(NewI),
  Dest = hipe_sparc:load_fp_dest(NewI),
  Off = hipe_sparc:load_fp_off(NewI),
  Align = hipe_sparc:load_fp_align(NewI),
  case hipe_sparc:load_fp_type(NewI) of
    double ->
      Dest2 = hipe_sparc:mk_fpreg(hipe_sparc:fpreg_nr(Dest) + 1),
      case hipe_sparc:is_imm(Off) of
	true ->
	  Off2 = hipe_sparc:mk_imm(hipe_sparc:imm_value(Off) + 4),
	  [hipe_sparc:load_fp_create(Dest, Align, single, Source, Off),
	   hipe_sparc:load_fp_create(Dest2, Align, single, Source, Off2)];
	_ -> %% The offset is a register
	  [hipe_sparc:load_fp_create(Dest, Align, single, Source, Off),
	   hipe_sparc:alu_create(Off, Off, '+', hipe_sparc:mk_imm(4)),
	   hipe_sparc:load_fp_create(Dest2, Align, single, Source, Off),
	   hipe_sparc:alu_create(Off, Off, '-', hipe_sparc:mk_imm(4))]
      end;
    _ ->
      NewI
  end.

rewrite_store_fp(I, TempMap)->
  NewI = map_temps(I, TempMap),
  Source = hipe_sparc:store_fp_src(NewI),
  Dest = hipe_sparc:store_fp_dest(NewI),
  Off = hipe_sparc:store_fp_off(NewI),
  Align = hipe_sparc:store_fp_align(NewI),
  case hipe_sparc:store_fp_type(NewI) of
    double ->
      Source2 = hipe_sparc:mk_fpreg(hipe_sparc:fpreg_nr(Source) + 1),
      case hipe_sparc:is_imm(Off) of
	true ->
	  Off2 = hipe_sparc:mk_imm(hipe_sparc:imm_value(Off) + 4),
	  [hipe_sparc:store_fp_create(Dest, Off, single, Align, Source),
	   hipe_sparc:store_fp_create(Dest, Off2, single, Align, Source2)];
	_ ->
	  [hipe_sparc:store_fp_create(Dest, Off, single, Align, Source),
	   hipe_sparc:alu_create(Off, Off, '+', hipe_sparc:mk_imm(4)),
	   hipe_sparc:store_fp_create(Dest, Off, single, Align, Source2),
	   hipe_sparc:alu_create(Off, Off, '-', hipe_sparc:mk_imm(4))]
      end;
    _ ->
      NewI
  end.

get_offset(O) ->
  -4*(O).

get_spill_pos(P) ->
  get_offset(P+1).

get_spill_offset(T) ->
 get_spill_pos(element(2,T)).

enter_stack_args(Args, Need, TempMap, ArgsOnStack) ->
  enter_stack_args(0, hipe_sparc_registers:register_args(), Args,
		   Need, TempMap, ArgsOnStack).

%%
%% The stack on SPARC grows uppwards (At least now...)
%%
%% Assume a function f with Arity = j is doing a tail-call to
%% a function g with arity k. 
%% Also assume the current function has spilled i temps.
%%
%%  Current stack frame:
%%
%%        |         |
%% SP ->  |         |
%%        | Spill 0 |
%%        |  ...    |
%%        | Spill i |
%%        | Dead CP |  (Read to %o7)    
%%        | Arg j   |
%%        | ...     |
%%        | Arg 6   |  (Currently 5 args in reg)
%%        |OLD FRAME|
%%
%%  Stack just before call to target:
%%        |         |
%% sp ->  |         |
%%        |         |
%% SP ->  |         |                   
%%        | Arg' k  |
%%        | ...     |
%%        | ...     |
%%        | ...     |
%%        | Arg' 6  |  (Currently 5 args in reg)
%%        |OLD FRAME|
%%
%% The arguments to g might be on the stack
%% If they are in the dangezone (the area overwritten by the arguments)
%% They have to be moved.
%% e.g. ( d marks the dangerzone)
%%           f/7               g/10
%%        |         |
%% SP ->  |         |
%%        | Spill 1 |   SP ->|         |
%%   d    | Spill 2 |        | Spill 1 |
%%   d    | Spill 3 |        | Spill 2 |
%%   d    | Dead CP |        | Spill 3 |
%%   d    | Arg 7   |        | Arg 6   |
%%   d    | Arg 6   |        | t1      |
%%        |OLD FRAME|        |OLD FRAME|
%%
%%   The DangerZone is SP - 2 to SP - 6
%%  We go through the arguments to g bottom up:
%%
%%   DZ     ARG      POSITION       SAVE  
%%
%%    < -6, t1     : in reg -> OK
%%    < -5, Arg 6  : SP-6 -> in DZ, Save = [SP-6]  
%%    < -4, Spill 2: SP-2 -> OK
%%    < -3, Spill 3: SP-1 -> OK
%%    < -2, Spill 1: SP-3 -> in DZ, Save = [SP-3, SP-6]
%%
%%  Then we copy the positions in Save to the top of the stack.
%%        | Arg 6   |
%% SP ->  | Spill 1 |
%%        | Spill 3 |
%%   d    | Spill 2 |
%%   d    | Spill 1 |
%%   d    | Dead CP |
%%   d    | Arg 7   |
%%   d    | Arg 6   |
%%        |OLD FRAME|
%%
%%  Now we can rewrite the stack safely:
%%        | A g 6   |
%% sp ->  | S i l 1 |
%% SP ->  | s i l 2 |
%%        | Spill 1 |
%%        | Spill 2 |
%%        | Spill 3 |
%%        | Arg 6   |
%%        | t1      |
%%        |OLD FRAME|

%% TODO: Verify that this realy is correct.

enter_stack_args(ArgNo, ArgNo, Args, Need, TempMap, ArgsOnStack) ->
  %% How many args has to be spilled to the stack?
  NoNewArgsOnStack = length(Args),

  %% Where does the first arg end up?
  %% SP - (Need + ArgsOnStack)*4
  Base = -(Need + ArgsOnStack),

  {ToSave,RealArgs} = args_in_danger(Args, Base-1, [], 0, [], TempMap),

  TempReg = ?MK_TEMP_REG(),
  SaveCode = save(ToSave,0, TempReg),
  PushCode = pushem(RealArgs, Base, TempReg),

  AdjustSP = 
    gen_dealloc_code((ArgsOnStack + Need - NoNewArgsOnStack)*4),
  [ SaveCode, PushCode, AdjustSP];
enter_stack_args(No, NoRegArgs, [_Arg|Args], Need, TempMap, ArgsOnStack) ->
  enter_stack_args(No+1, NoRegArgs, Args, Need, TempMap, ArgsOnStack).

args_in_danger([Arg|Args], DangerZone, ToSave, SavePos, MappedArgs, TempMap) ->
  case hipe_sparc:is_reg(Arg) of
    true ->
      case hipe_temp_map:find(hipe_sparc:reg_nr(Arg), TempMap) of
	{reg, Reg} -> %% Arg in reg all cool.
	  args_in_danger(Args, DangerZone+1, ToSave, SavePos, 
			 [{reg,Reg}|MappedArgs], TempMap);
	{spill, Pos} ->
	  if -Pos < DangerZone -> %% Spill in dangerzone
	      args_in_danger(Args, DangerZone+1, [-Pos|ToSave], SavePos+1,
			     [{spill, SavePos}| MappedArgs], TempMap);
	     true ->
	      args_in_danger(Args, DangerZone+1, ToSave, SavePos,
			     [{spill, -(Pos+1)}| MappedArgs], TempMap)
	  end
      end;
    false ->
      case hipe_sparc:is_imm(Arg) of 
	true ->
	  args_in_danger(Args, DangerZone+1, ToSave, SavePos, 
			 [{imm,	hipe_sparc:imm_value(Arg)}|MappedArgs], TempMap);
	false ->
	  %% TODO: XXX: Dont handle fpreg ...
	  ?EXIT({do_no_handle,Arg})
      end
  end;
args_in_danger([],_,ToSave,_,Args,_) ->
  {lists:reverse(ToSave),lists:reverse(Args)}.

save([Pos|Poses], Offset, TempReg) ->
  ToPos = Offset*4,
  FromPos = get_offset(Pos+1),
  [gen_stack_load(FromPos,TempReg),
   gen_stack_store(ToPos, TempReg)
   | save(Poses, Offset+1, TempReg)];
save([],_,_) -> [].

pushem([{reg,R}|Args], Pos, Temp) ->
  ToPos = Pos*4,
  [gen_stack_store(ToPos, hipe_sparc:mk_reg(R))|
   pushem(Args,Pos+1, Temp)];
pushem([{spill,P}|Args], Pos, Temp) ->
  FromPos = P*4, 
  ToPos = Pos*4,
  [gen_stack_load(FromPos,Temp),
   gen_stack_store(ToPos, Temp)|
   pushem(Args,Pos+1, Temp)];
pushem([{imm,I}|Args], Pos, Temp) ->
  ToPos = Pos*4,
  [hipe_sparc:move_create(Temp, hipe_sparc:mk_imm(I)),
   gen_stack_store(ToPos, Temp)|
   pushem(Args,Pos+1, Temp)];
pushem([],_,_) -> [].


find_reg(Temp,Map) ->
  case hipe_temp_map:find(hipe_sparc:reg_nr(Temp), Map) of 
    {reg, Pos} ->
      Pos;
    Where -> ?EXIT({temp_assumed_in_reg,Temp,Where})
  end.

find_fpreg(Temp,Map) ->
  case hipe_temp_map:find(hipe_sparc:fpreg_nr(Temp), Map) of 
    {fp_reg, Pos} ->
      Pos;
    Where -> ?EXIT({temp_assumed_in_fpreg,Temp,Where})
  end.

%%find_pos(Temp,Map) ->
%%    case hipe_temp_map:find(hipe_sparc:reg_nr(Temp), Map) of
%%    {spill, Pos} ->
%%      Pos;
%%    Where -> ?EXIT({temp_assumed_on_stack,Temp,Where})
%%  end.

remap(I, Temps, Map) ->
%%  io:format("\n\n~w~w\n~w\n",[I,Temps,Map]),
  Substs = 
    [{Temp, hipe_sparc:mk_reg(find_reg(Temp,Map))}
     || Temp <- Temps],
  hipe_sparc:subst(I, Substs).

remap_fp(I, Temps, Map) ->
%%  io:format("\n\n~w~w\n~w\n",[I,Temps,Map]),
  Substs = 
    [{Temp, hipe_sparc:mk_fpreg(find_fpreg(Temp,Map))}
     || Temp <- Temps],
  hipe_sparc:subst(I, Substs).

map_fp(Temp, Map) ->
  case hipe_sparc:is_fpreg(Temp) of
    true ->
      hipe_sparc:mk_fpreg(find_fpreg(Temp, Map));
    false ->
      Temp
  end.

map(Temp, Map) ->
  case hipe_sparc:is_reg(Temp) of
    true ->
      hipe_sparc:mk_reg(find_reg(Temp, Map));
    false ->
      Temp
  end.

	 

max(P,A) ->
  if P > A -> P;
     true -> A
  end.
min(P,A) ->
  if P < A -> P;
     true -> A
  end.



push_call_args(Args, TempMap) ->
  ArgsOnStack = 
    args_on_stack(hipe_sparc_registers:register_args(), Args),
  NoArgs = length(ArgsOnStack),      
  if 
    NoArgs > 0 ->
      MappedArgs = 
	[
	 case hipe_sparc:is_reg(Arg) of
	   true ->
	     case hipe_temp_map:find(hipe_sparc:reg_nr(Arg), TempMap) of
	       {reg, Reg} -> {reg,Reg};
	       {spill, Pos} -> {spill, -(1+Pos+NoArgs)}
	     end;
	   false ->
	     case hipe_sparc:is_imm(Arg) of 
	       true ->
		 {imm,hipe_sparc:imm_value(Arg)};
	       false ->
		 %% TODO: XXX: Dont handle fpreg ...
		 ?EXIT({do_no_handle,Arg})
	     end
	 end
	 || Arg <- ArgsOnStack],


      SP = ?MK_SP_REG(),
      TempReg = ?MK_TEMP_REG(),
      AdjustSP = 
	hipe_sparc:alu_create(SP,SP,'+',hipe_sparc:mk_imm(4*NoArgs)),
      PushCode = pushem(MappedArgs,-NoArgs,TempReg),
      [AdjustSP|PushCode];
    true -> [] %% No args on the stack.
  end.

args_on_stack(0, Args) ->
  Args;
args_on_stack(N,[_Arg|Args]) ->
  args_on_stack(N-1,Args);
args_on_stack(_N,[]) -> 
  [].


extra_stack_need(Code,Need,Arity) ->
  %% Calculate number of args (to current fun) on stack
  NoRegArgs = hipe_sparc_registers:register_args(),
  ArgsOnStack = 
    if Arity > NoRegArgs -> Arity - NoRegArgs; 
       true -> 0 
    end,
  PrevSPLevel = ArgsOnStack + Need,  
  lists:foldl(fun (I,Max) ->
		  max(stack_need(I,PrevSPLevel), Max)
	      end, 
	      0,
	      Code).

stack_need(I, PrevSPLevel) ->
  %% io:format("\n\n~w\n~w\n",[I,TempMap]),
  case hipe_sparc:type(I) of
    call_link ->
      Argneed = call_args_need(hipe_sparc:call_link_args(I)),
      Argneed;

    pseudo_enter ->
      NoArgs = length(hipe_sparc:pseudo_enter_args(I)),
      NoRegArgs = hipe_sparc_registers:register_args(),
      %% stack, do some shuffling.
      %% Otherwise just adjust the stackpointer.
      case NoArgs > NoRegArgs of
	true ->        
	  %% Arguments to the called function spill 
	  NewOnStack = NoArgs - NoRegArgs,

	  case NewOnStack > PrevSPLevel of
	    true -> NewOnStack - PrevSPLevel;
	    false -> 0
	  end;
	false -> %% All arguments on stack...
	  0
      end;
    _ -> 
      %% All other instructions no extra need.
      0
  end.

call_args_need(Args) ->
  length(args_on_stack(hipe_sparc_registers:register_args(), Args)).


set_stack_size(Call,Size, Arity) ->
  SD = hipe_sparc:call_link_stack_desc(Call),
  NewSD = hipe_sparc:sdesc_size_update(SD, Size),
  NewSD2 = hipe_sparc:sdesc_arity_update(NewSD, Arity),
  hipe_sparc:call_link_stack_desc_update(Call,NewSD2).


gen_stack_test(StackNeed, SP, CP, Arity, Cfg) ->
  Leaf =  hipe_sparc_cfg:is_leaf(Cfg),
  TempReg = ?MK_TEMP_REG(),
  TempReg1 = hipe_sparc:mk_reg(hipe_sparc_registers:temp1()),  

  if (StackNeed) =:= 0 ->
      {[],[]};
     (StackNeed) < ?HIPE_SPARC_LEAF_WORDS, Leaf =:= true ->
      {[],[]};
     true -> 
      Zero = hipe_sparc:mk_reg(hipe_sparc_registers:zero()),
      SL = hipe_sparc:mk_reg(hipe_sparc_registers:stack_limit()),
      Pred = 0.01,
      OverflowLbl = hipe_sparc:label_create_new(),
      StartLbl = hipe_sparc:label_create_new(),
      TestLbl = hipe_sparc:label_create_new(),
      RetLbl = hipe_sparc:label_create_new(),
      OverflowName = hipe_rtl:label_name(OverflowLbl), 
      StartName = hipe_rtl:label_name(StartLbl),
      TestName = hipe_rtl:label_name(TestLbl),
      ByteNeed = (?HIPE_SPARC_LEAF_WORDS+StackNeed)*4, 
      TestCode = 
	if ByteNeed > 16#fff -> %% Max in simm13.
	    [
	     hipe_sparc:alu_create(TempReg, SL, '-', SP),
	     hipe_sparc:sethi_create(TempReg1,
				     hipe_sparc:mk_imm(high22(ByteNeed))),
	     hipe_sparc:alu_create(TempReg1, TempReg1, 'or', 
				   hipe_sparc:mk_imm(low10(ByteNeed))),
	     hipe_sparc:alu_cc_create(Zero, TempReg, '-', TempReg1),
	     hipe_sparc:b_create(l, OverflowName, StartName, Pred, na),
	     StartLbl];
	   true ->
	    [hipe_sparc:alu_create(TempReg, SL, '-', SP),
	     hipe_sparc:alu_cc_create(Zero, TempReg, '-', 
				      hipe_sparc:mk_imm(ByteNeed)),
	     hipe_sparc:b_create(l, OverflowName, StartName, Pred, na),
	     StartLbl]
	end,
      CPSAVE = hipe_sparc:mk_reg(hipe_sparc_registers:cpsave()),
      {[hipe_sparc:goto_create(TestName),
	TestLbl|TestCode],
       [OverflowLbl,
	hipe_sparc:move_create(CPSAVE, CP),
	hipe_sparc:call_link_create(inc_stack_fun(Arity),
				    CP,
				    [CPSAVE],
				    hipe_rtl:label_name(RetLbl), [], not_remote),
	RetLbl,
	hipe_sparc:move_create(CP, CPSAVE),
	hipe_sparc:goto_create(TestName)]
      }
  end.

inc_stack_fun(Arity) ->
  ArgsInRegs = min(Arity, hipe_sparc_registers:register_args()), 
  list_to_atom("inc_stack_" ++ integer_to_list(ArgsInRegs) ++ "args_0").


gen_alloc_code(NeededBytes,SP) ->
  TempReg = ?MK_TEMP_REG(),
  %% Make room on stack for spills.

  %% Alloc space on stack.
  if NeededBytes > 4092 -> %% To big for imm
      [hipe_sparc:sethi_create(TempReg,
			       hipe_sparc:mk_imm(high22(NeededBytes))),
       hipe_sparc:alu_create(TempReg, TempReg, 'or', 
			     hipe_sparc:mk_imm(low10(NeededBytes))),
       hipe_sparc:alu_create(SP,SP,'+',TempReg)];
     true ->
      [hipe_sparc:alu_create(SP,SP,'+',hipe_sparc:mk_imm(NeededBytes))]
  end.
 
gen_dealloc_code(Bytes) ->
  TempReg = ?MK_TEMP_REG(),
  SP = ?MK_SP_REG(),
  %% dealloc space on stack.
  if Bytes > 4092 -> %% To big for imm
      [hipe_sparc:sethi_create(TempReg,
			       hipe_sparc:mk_imm(high22(Bytes))),
       hipe_sparc:alu_create(TempReg, TempReg, 'or', 
			     hipe_sparc:mk_imm(low10(Bytes))),
       hipe_sparc:alu_create(SP,SP,'-',TempReg)];
     true ->
      [hipe_sparc:alu_create(SP,SP,'-',hipe_sparc:mk_imm(Bytes))]
  end.
					
gen_save_cp(Need)->
  CP = ?MK_CP_REG(),
  Offset = get_offset(Need),
  gen_stack_store(Offset,CP).


gen_ret(Need, Arity) ->
  %% Generate code for stack cleanup and return.

  %% XXX: Fix if we start using multiple return values
  RegArgs = [hipe_sparc:mk_reg(hipe_sparc_registers:ret(0))], 
  CP = ?MK_CP_REG(),
  %% Calculate number of args (to current fun) on stack
  %% Consider moving this out of the loop.
  NoRegArgs = hipe_sparc_registers:register_rets(),
  ArgsOnStack = 
    if Arity > NoRegArgs -> Arity - NoRegArgs; 
       true -> 0 
    end,
  RetLabel = hipe_sparc:label_create_new(),
  
  StackAdjust = (ArgsOnStack+Need)*4,
  {[RetLabel,
    gen_stack_load(get_offset(Need),CP),
    gen_dealloc_code(StackAdjust),
    hipe_sparc:jmp_create(CP, hipe_sparc:mk_imm(8), RegArgs, [])
   ],
   hipe_sparc:label_name(RetLabel)}.
	 

high22(X) -> X bsr 10.
low10(X) -> X band 16#3ff.

gen_stack_load(Offset, DestReg) ->
  if Offset < -4092 ->
      load_huge_spillpos(Offset,DestReg);
     true ->
      SP = ?MK_SP_REG(),
      ImmOffset = hipe_sparc:mk_imm(Offset),
      [hipe_sparc:load_create(DestReg, uw, SP, ImmOffset)]
  end.

gen_stack_store(Offset, Reg) ->
  if Offset < -4092 ->
      store_huge_spillpos(Offset,Reg);
     true ->
      SP = ?MK_SP_REG(),
      ImmOffset = hipe_sparc:mk_imm(Offset),
      [hipe_sparc:store_create(SP,ImmOffset, w, Reg)]
  end.

store_huge_spillpos(Offset,RegToSave)->
  SP = ?MK_SP_REG(),
  {DecCode, IncCode, NewOffset} = adjust(Offset,SP),
  [DecCode, 
   hipe_sparc:store_create(SP,hipe_sparc:mk_imm(NewOffset),RegToSave),
   IncCode].
load_huge_spillpos(Offset,RegToLoad)->
  SP = ?MK_SP_REG(),
  {DecCode, _IncCode, NewOffset} = adjust(Offset,RegToLoad),
  [hipe_sparc:move_create(RegToLoad,SP),
   DecCode, 
   hipe_sparc:load_create(RegToLoad,RegToLoad,
			  hipe_sparc:mk_imm(NewOffset))].

gen_stack_load_fp(Offset, DestReg) ->
  Offset2 = Offset -4,
  if Offset2 < -4092 ->
      load_huge_spillpos_fp(Offset2,DestReg);
     true ->
      DestRegNr = hipe_sparc:fpreg_nr(DestReg),
      DestReg2 = hipe_sparc:mk_fpreg(DestRegNr + 1),
      SP = ?MK_SP_REG(),
      [hipe_sparc:load_fp_create(DestReg, 32, single, SP, 
				 hipe_sparc:mk_imm(Offset)),
       hipe_sparc:load_fp_create(DestReg2, 32, single, SP,
				 hipe_sparc:mk_imm(Offset2))]
  end.

gen_stack_store_fp(Offset, SrcReg) ->
  Offset2 = Offset-4,
  if Offset2 < -4092 ->
      store_huge_spillpos_fp(Offset2,SrcReg);
     true ->  
      SrcRegNr = hipe_sparc:fpreg_nr(SrcReg),
      SrcReg2 = hipe_sparc:mk_fpreg(SrcRegNr + 1),
      SP = ?MK_SP_REG(),
      [hipe_sparc:store_fp_create(SP, hipe_sparc:mk_imm(Offset), 
				  single, 32, SrcReg),
       hipe_sparc:store_fp_create(SP, hipe_sparc:mk_imm(Offset2), 
				  single, 32, SrcReg2)]
  end.

store_huge_spillpos_fp(Offset2,RegToSave)->
  RegNr = hipe_sparc:fpreg_nr(RegToSave),
  RegToSave2 = hipe_sparc:mk_fpreg(RegNr+ 1),
  SP = ?MK_SP_REG(),
  {DecCode, IncCode, NewOffset2} = adjust(Offset2,SP),
  [DecCode,
   hipe_sparc:store_fp_create(SP, hipe_sparc:mk_imm(NewOffset2+4), 
			      single, 32, RegToSave),
   hipe_sparc:store_fp_create(SP, hipe_sparc:mk_imm(NewOffset2), 
			      single, 32, RegToSave2),
   IncCode].

load_huge_spillpos_fp(Offset2,RegToLoad)->
  RegNr = hipe_sparc:fpreg_nr(RegToLoad),
  RegToLoad2 = hipe_sparc:mk_fpreg(RegNr + 1),
  SP = ?MK_SP_REG(),
  {DecCode, IncCode, NewOffset2} = adjust(Offset2,SP),
  [DecCode,
   hipe_sparc:load_fp_create(RegToLoad, 32, single, SP, 
			     hipe_sparc:mk_imm(NewOffset2+4)),
   hipe_sparc:load_fp_create(RegToLoad2, 32, single, SP, 
			     hipe_sparc:mk_imm(NewOffset2)),
   IncCode].

adjust(Offset,Reg) ->
  adjust(Offset, Reg, [],[]).

adjust(Offset, _Reg, Dec,Inc) when Offset > -4092 ->
  {Dec,Inc,Offset};
adjust(Offset, Reg, Dec,Inc) ->
  NewOffset = Offset + 4092,
  Step = hipe_sparc:mk_imm(4092),
  adjust(NewOffset, Reg,
	 [hipe_sparc:alu_create(Reg,Reg,'-',Step)| Dec],
	 [hipe_sparc:alu_create(Reg,Reg,'+',Step)| Inc]).
	 

  
