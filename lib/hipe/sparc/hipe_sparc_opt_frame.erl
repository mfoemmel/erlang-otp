%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%%  Filename : 	hipe_sparc_opt_frame.erl
%%  Module   :	hipe_sparc_opt_frame
%%  Purpose  :  To minimize loads and stores to stackframes by 
%%              propagating information about what is stored on the 
%%              stack and in registers.
%%              This is done in two steps:
%%               1. Forward copy propagation:
%%                  Information about copies of registers in oter regs
%%                  and on the stack is propagated through the CFG.
%%                  If a store to a stack slot is redundant 
%%                  (i.e the value to store is already in that slot)
%%                  the store is removed.
%%               2. Backward dead code elimination.
%%                  Liveness information is propagated backward through
%%                  each basic block. If a load (or a move) to a dead
%%                  temporary is encountered, the instruction is deleted.
%%
%%  Notes    :  This propagation is designed to go after the regalloc
%%              phase but before the code has been rewritten to use
%%              physical registers. This means that the propagation 
%%              has to be a bit conservative in order to not extend
%%              live ranges of temporaries, causing conflict in register
%%              usage.
%%  History  :	* 2001-12-04 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: kostis $
%%              $Date: 2004/06/22 10:14:02 $
%%              $Revision: 1.14 $
%% ====================================================================
%%  Exports  : cfg/1 - Takes a SPARC CFG and rewrites it.
%%
%%  TODO     : More efficent data structure.
%%             Turn on the fixpoint iteration.
%%             Extend to handle alu instructions better... 
%%             Turn commented print_code into ifdef debug.
%%             Cleanup.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc_opt_frame).
-export([cfg/1]).
-include("../main/hipe.hrl").
-import(hipe_sparc_prop_env,
	[bind/3,bind_hpos/3, inc_hp/2, inc_sp/2,
	 end_of_bb/1, find_hpos/2, find_spos/2,
	 kill/2, kill_all/2, kill_hp/1, kill_phys_regs/1, kill_sp/1,
	 kill_uses/2, lookup/2, new_genv/1, set_active_block/2, succ/1,
	 zap_heap/1, zap_stack/1, bind_spos/3]).

%% ____________________________________________________________________
%% 
cfg(CFG) ->
  %%  hipe_sparc_cfg:pp(CFG),
  Lbls = [hipe_sparc_cfg:start_label(CFG)],

  %% Forward prop to get rid of stores.
  {CFG0,_GEnv0} = prop_bbs(Lbls, CFG, new_genv(CFG),[]),
  %% hipe_sparc_cfg:pp(CFG0),
  CFG1 = CFG0, %% prop(Lbls, CFG0, GEnv0,100),

  %% Backward prop to get rid of loads.
  CFG2 = remove_dead(CFG1),
  CFG2.


%% ____________________________________________________________________
%% 
remove_dead(CFG) ->
  Liveness = hipe_sparc_liveness:analyze(CFG),
  Lbls = hipe_sparc_cfg:labels(CFG),
  bwd_prop(Lbls, CFG, Liveness).

bwd_prop([L|Lbls], CFG, Liveness) ->
  BB = hipe_sparc_cfg:bb(CFG, L),
  LiveOut = hipe_sparc_liveness:liveout(Liveness, L),
  {NewCode,_} = bwd_prop_bb(hipe_bb:code(BB),LiveOut),
  NewBB = hipe_bb:code_update(BB, NewCode),
  NewCFG = hipe_sparc_cfg:bb_add(CFG, L, NewBB),
  bwd_prop(Lbls, NewCFG, Liveness);
bwd_prop([],CFG,_) ->
  CFG.

bwd_prop_bb([I|Is], LiveOut) ->
  {NewIs, NewLiveOut} = bwd_prop_bb(Is,LiveOut),
  {NewI,Out} = bwd_prop_i(I,NewLiveOut),
  {[NewI|NewIs], Out};
bwd_prop_bb([], LiveOut) -> {[], LiveOut}.

bwd_prop_i(I,Live) ->
  Uses = ordsets:from_list(hipe_sparc:uses(I)),
  Defines = 
    ordsets:from_list(
      case hipe_sparc:type(I) of
	call_link -> 
	  [hipe_sparc:mk_reg(X) 
	   || X <- hipe_sparc_registers:allocatable()];
	_ -> hipe_sparc:defines(I)
      end),
  
  case ordsets:intersection(Defines,Live) of
    [] ->
      %% Nothing defined is live -- potentialy dead
      case can_kill(I) of
	true ->
	  {hipe_sparc:comment_create({"Removed instr",I}),
	   Live};
	false ->
	  {I,
	   ordsets:union(ordsets:subtract(Live,Defines),Uses)}
      end;
    _ -> %% The result is needed.
       {I,ordsets:union(ordsets:subtract(Live,Defines),Uses)}
  end.

can_kill(I) ->
  %% TODO: Expand this function.
  case hipe_sparc:type(I) of
    pseudo_unspill ->
      %% io:format("hipe_sparc_opt_frame:can_kill/1 -> pseudo_unspill"),
      Dst = hipe_sparc:pseudo_unspill_reg(I),
      case hipe_sparc:is_reg(Dst) of
	true->
	  case hipe_sparc_registers:is_precoloured(hipe_sparc:reg_nr(Dst)) of
	    true -> false;
	    _ -> true
	  end;
	false -> %% is fp_reg
	  true
      end;
    move ->
	  %%hipe_sparc:pp_instr(I),
	  %%io:format("hipe_sparc_opt_frame:can_kill/1 -> move"),
	  Dest = hipe_sparc:move_dest(I),
	  case hipe_sparc:is_reg(Dest) of
	      true ->
		  case hipe_sparc_registers:is_precoloured(hipe_sparc:reg_nr(Dest)) of
		      true -> false;
		      _ -> true
		  end;
	      _ -> false
	  end;
      _ -> 
	  false
  end.


%% ____________________________________________________________________
%% 
%% Fixpoint iteration.
% prop(_Start,CFG,_Env,0) ->
%   %% io:format("Limit hit\n"),
%   CFG;
% prop(Start,CFG,Env,N) ->
%   case hipe_sparc_prop_env:genv__changed(Env) of
%     true ->
%       {CFG0,GEnv0} = prop_bbs(Start, CFG, hipe_sparc_prop_env:genv__changed_clear(Env), []),
%       prop(Start,CFG0, GEnv0, N-1);
%     false ->
%       CFG
%   end.


%% ____________________________________________________________________
%% 
%%
%% Iterate over the basic blocks of a cfg.
%%
prop_bbs([], CFG, GEnv,_) ->
  {CFG,GEnv};
prop_bbs([BB|BBs], CFG, GEnv,Vis) ->
  {Succs, CFG0,GEnv0, NewVis} = prop_bb(BB, GEnv, CFG,Vis),
  prop_bbs(BBs++Succs, CFG0, GEnv0, NewVis).
  
  

%%
%% If Lbl is a member of the extended block Ebb. Then propagate info 
%% and continue with its successors.
%%

prop_bb(Lbl, GEnv, CFG, Vis) ->
  case lists:member(Lbl, Vis) of
    true -> {[],CFG, GEnv, Vis};
    false ->
      BB = hipe_sparc_cfg:bb(CFG, Lbl),
      %% io:format("\n~w:\n========\n~p\n",[Lbl,hipe_sparc_prop_env:genv__env(set_active_block(Lbl,GEnv))]),
      {NewCode, NewGEnv} = prop_instrs(hipe_bb:code(BB), 
				       set_active_block(Lbl,GEnv)),
      NewBB = hipe_bb:code_update(BB, NewCode),
      NewCFG = hipe_sparc_cfg:bb_add(CFG, Lbl, NewBB),
      Succ = succ(NewGEnv),
      %% io:format("Succs: ~w\n",[Succ]),
      {Succ, NewCFG, NewGEnv,[Lbl|Vis]}
  end.


% prop_succ([], GEnv, CFG, Vis) ->
%   {CFG,GEnv, Vis};
% prop_succ([BB|BBs], GEnv, CFG, Vis) ->
%   {NewCFG,NewGEnv, NewVis} = prop_bb(BB, GEnv, CFG, Vis),
%   prop_succ(BBs, NewGEnv, NewCFG, NewVis).


prop_instrs([], GEnv) ->
  {[], end_of_bb(GEnv)};
prop_instrs([I|Is], GEnv) ->
  {NewI, Env0} = prop_instr(I, GEnv),
%%  if I =/= NewI ->
%%            io:format("REWRITE\n"),
%%        hipe_sparc_pp:pp_instr(NewI),
%%      ok;
%%     true -> ok
%%  end,
  GEnv0 = hipe_sparc_prop_env:genv__env_update(Env0,GEnv),
  {NewIs, NewEnv} = prop_instrs(Is, GEnv0),

  case NewI of %% This is not realy necessary...
    [_|_] -> {NewI++NewIs, NewEnv};	
    _ -> {[NewI|NewIs], NewEnv}
  end.


%%
%% Propagate copies and constants for one instruction.
%%

prop_instr(I, Env) ->
  %%   pp_lenv(Env),
  %%   hipe_sparc_pp:pp_instr(I),

  case hipe_sparc:type(I) of
    move ->
      Srcs = [hipe_sparc:move_src(I)],
      Dsts = [hipe_sparc:move_dest(I)],
      {_I0,Env0} = bind_all(Srcs, Dsts, I, hipe_sparc_prop_env:genv__env(Env)),
      {I, kill_uses(hipe_sparc:defines(I), Env0)};
    multimove ->
      NewEnv = kill_all(hipe_sparc:defines(I), hipe_sparc_prop_env:genv__env(Env)),
      Srcs = hipe_sparc:multimove_src(I),
      Dsts = hipe_sparc:multimove_dest(I),
      {_I0,Env0} = bind_all(Srcs, Dsts, I, NewEnv),
      {I,Env0};
    _ ->
      eval(I, hipe_sparc_prop_env:genv__env(Env))
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Evaluate an instruction. Returns {NewI, NewEnv}.
%%

eval(I, Env) ->
  case hipe_sparc:type(I) of
    store -> prop_store(I,Env);
    load ->  prop_load(I,Env);
    pseudo_spill -> prop_spill(I,Env);
    pseudo_unspill -> prop_unspill(I,Env);
    %%    cmov_cc -> prop_cmov_cc(I,Env);
    %%    cmov_r -> prop_cmov_r(I,Env);
    alu -> prop_alu(I,Env);
    %%    alu_cc -> prop_alu_cc(I,Env);
    %%    sethi ->  prop_sethi(I,Env);

    %%    load_atom ->  prop_load_atom(I,Env);
    %%    load_word_index ->  prop_word_index(I,Env);
    %%    load_address ->  prop_load_address(I,Env);


    %%    b ->  prop_b(I,Env);
    %%    br ->  prop_br(I,Env);
    %%    goto ->  prop_got(I,Env);
    %%    jmp ->  prop_jmp(I,Env);

    call_link ->  prop_call_link(I,Env);

    nop ->  {I,Env};
    align ->  {I,Env};
    comment -> {I,Env};

    _ -> 
      NewEnv = kill_all(hipe_sparc:defines(I), Env),
      {I,NewEnv}
end.    

%% ____________________________________________________________________
%% 
prop_store(I,Env) ->
  Base = hipe_sparc:store_dest(I),
  Offset = hipe_sparc:store_off(I),
  Src = hipe_sparc:store_src(I),
  SP = hipe_sparc:mk_reg(hipe_sparc_registers:stack_pointer()),
  HP = hipe_sparc:mk_reg(hipe_sparc_registers:heap_pointer()),
  if
    Base =:= SP ->
      prop_stack_store(I,Env,Offset,Src);
    Base =:= HP ->
      prop_heap_store(I,Env,Offset,Src);
    Offset =:= SP ->
      ?EXIT({dont_use_sp_as_offset,I});
    Offset =:= HP ->
      ?EXIT({dont_use_hp_as_offset,I});
    true ->
      %% A store off stack and heap (Probably PCB).
      %% We assume there is no interference here!!!
      {I,Env}
  end.

prop_spill(I,Env) ->
  Pos = hipe_sparc:imm_value(hipe_sparc:pseudo_spill_pos(I))*4,
  Src = hipe_sparc:pseudo_spill_reg(I),
  case find_spos(Pos, Env) of
    Src ->
      %% Already spilled.
      NewI = hipe_sparc:comment_create({"Removed spill",I}),
      {NewI, Env};
    _ ->
      %% Not spilled.
      NewEnv = bind_spos(Pos, Src, Env),
      {I, NewEnv}
  end. 

prop_unspill(I,Env) ->
  Pos = hipe_sparc:imm_value(hipe_sparc:pseudo_unspill_pos(I))*4,
  Dest = hipe_sparc:pseudo_unspill_reg(I),
  case find_spos(Pos, Env) of
    undefined ->
      {I, kill(Dest,Env)};
    Val ->
      bind_all([Val],[Dest],I,Env)
  end.




prop_stack_store(I,Env,Offset,Src) ->
  case hipe_sparc_prop_env:env__sp(Env) of
    unknown ->
       %% We are updating via unknown SP.
	{I, zap_stack(Env)};
    SOff ->
      case hipe_sparc:is_imm(Offset) of
	false ->
	  %% We are updating the stack via a reg...
	  %% TODO: Check wehter the the reg is bound to a const...
	  %% We have to zap the stack...
	  {I, zap_stack(Env)};
	true ->
	  Pos = hipe_sparc:imm_value(Offset) + SOff,
	  NewEnv = bind_spos(Pos, Src, Env),
	  %% TODO: Indicate that Src is copied on stack.
	  {I, NewEnv}
      end
  end.

prop_heap_store(I,Env,Offset,Src) ->
    case hipe_sparc_prop_env:env__hp(Env) of
      unknown ->
	%% We are updating via unknown HP.    
	{I, zap_heap(Env)};	
      HOff ->
	case hipe_sparc:is_imm(Offset) of
	  false ->
	    %% We are updating the heap via a reg...
	    %% TODO: Check wehter the the reg is bound to a const...
	    %% We have to zap the heap...
	    {I, zap_heap(Env)};
	  true ->
	    Pos = hipe_sparc:imm_value(Offset) + HOff,
	    NewEnv = bind_hpos(Pos, Src, Env),
	    %% TODO: Indicate that Src is copied on heap.
	    {I, NewEnv}
	end
    end.

prop_load(I,Env) ->
  Base = hipe_sparc:load_src(I),
  Offset = hipe_sparc:load_off(I),
  Dest = hipe_sparc:load_dest(I),
  SP = hipe_sparc:mk_reg(hipe_sparc_registers:stack_pointer()),
  HP = hipe_sparc:mk_reg(hipe_sparc_registers:heap_pointer()),
  if
    Base =:= SP ->
      prop_stack_load(I,Env,Offset,Dest);
    Base =:= HP ->
      prop_heap_load(I,Env,Offset,Dest);
    Offset =:= SP ->
      ?EXIT({dont_use_sp_as_offset,I});
    Offset =:= HP ->
      ?EXIT({dont_use_hp_as_offset,I});
    true ->
      %% A load off stack and heap (Probably PCB).
      %% We assume there is no interference here!!!
      NewEnv = kill(Dest,Env),
      {I,NewEnv}
  end.

prop_stack_load(I,Env,Offset,Dest) ->
  case hipe_sparc_prop_env:env__sp(Env) of
    unknown ->
      {I, kill(Dest,Env)};
    SOff ->
      case hipe_sparc:is_imm(Offset) of
	false ->
	  %% We are reading the stack via a reg...
	  %% TODO: Check wehter the the reg is bound to a const...
	  {I, kill(Dest,Env)};
	true ->
	  Pos = hipe_sparc:imm_value(Offset) + SOff,
	  
	  case find_spos(Pos, Env) of
	    undefined ->
	      {I, kill(Dest,Env)};
	    Val ->
	      case lookup(Dest, Env) of
		Val -> {hipe_sparc:comment_create("Removed load"),
			Env};
		_ ->
		  bind_all([Val],[Dest],I,kill_uses([Dest],Env))
	      end
	  end
      end
  end.
 
prop_heap_load(I,Env,Offset,Dest) ->
  case hipe_sparc_prop_env:env__hp(Env) of
    unknown ->
      {I, kill(Dest,Env)};
    HOff ->
      case hipe_sparc:is_imm(Offset) of
	false ->
	  %% We are reading the heap via a reg...
	  %% TODO: Check wehter the the reg is bound to a const...
	  {I, kill(Dest,Env)};
	true ->
	  Pos = hipe_sparc:imm_value(Offset) + HOff,
	  
	  case find_hpos(Pos, Env) of
	    undefined ->
	      {I, kill(Dest,Env)};
	    Val ->
	      bind_all([Val],[Dest],I,kill_uses([Dest],Env))
	  end
      end
  end.


%% ____________________________________________________________________
%% 
prop_alu(I,Env) ->
  OP = hipe_sparc:alu_operator(I),
  Src1 = hipe_sparc:alu_src1(I),
  Src2 = hipe_sparc:alu_src2(I),
  Dest = hipe_sparc:alu_dest(I),
  SP = hipe_sparc:mk_reg(hipe_sparc_registers:stack_pointer()),
  HP = hipe_sparc:mk_reg(hipe_sparc_registers:heap_pointer()),
  if
    Dest =:= SP ->
      case Src1 of
	SP ->
	  prop_sp_op(I,Env,OP,Src2);
	_ ->
	  %% TODO: handle SP = x op SP
	  %% unknown update of SP.
	  {I,kill_sp(zap_stack(Env))}
      end;
    Dest =:= HP ->
      case Src1 of
	HP ->
	  prop_hp_op(I,Env,OP,Src2);
	_ ->
	  %% TODO: handle HP = x op HP
	  %% unknown update of HP.
	  {I,kill_hp(zap_heap(Env))}
      end;
    true ->
      %% TODO: Fold consts ...
      {I, kill(Dest,Env)}
  end.

prop_sp_op(I,Env,'+',Src) ->
  case hipe_sparc:is_imm(Src) of
    true ->
      {I, inc_sp(Env,hipe_sparc:imm_value(Src))};
    false ->
      {I,kill_sp(zap_stack(Env))}
  end;
prop_sp_op(I,Env,'-',Src) ->
  case hipe_sparc:is_imm(Src) of
    true ->
      {I, inc_sp(Env, - hipe_sparc:imm_value(Src))};
    false ->
      {I,kill_sp(zap_stack(Env))}
  end;
prop_sp_op(I,Env,_Op,_Src) ->
  %% Dont know how to handle other ops...
  {I,kill_sp(zap_stack(Env))}.

prop_hp_op(I,Env,'+',Src) ->
  case hipe_sparc:is_imm(Src) of
    true ->
      {I,inc_hp(Env,hipe_sparc:imm_value(Src))};
    false ->
      {I,kill_sp(zap_stack(Env))}
  end;
prop_hp_op(I,Env,'-',Src) ->
  case hipe_sparc:is_imm(Src) of
    true ->
      {I, inc_hp(Env, - hipe_sparc:imm_value(Src))};
    false ->
      {I,kill_sp(zap_stack(Env))}
  end;
prop_hp_op(I,Env,_Op,_Src) ->
  %% Dont know how to handle other ops...
  {I,kill_hp(zap_heap(Env))}.

%% ____________________________________________________________________
%% 
prop_call_link(I,Env) ->
  Dests = hipe_sparc:call_link_dests(I),
  Env1 = kill_uses(Dests,kill_phys_regs(Env)),
  NoArgs =  length(hipe_sparc:call_link_args(I)),
  ArgsInRegs =  hipe_sparc_registers:register_args(),
  case NoArgs > ArgsInRegs of
    true ->
      StackAdjust = NoArgs - ArgsInRegs,
      Env2 = inc_sp(Env1, - StackAdjust*4),
      {I,Env2};
    false ->
      {I,Env1}
  end.


%% ____________________________________________________________________
%% 

bind_all(Srcs, Dsts, I, Env) ->
  bind_all(Srcs, Dsts, I, Env, Env).

%%%
%% We have two envs, Env where we do lookups and
%%                   NewEnv where the new bindings are entered.
bind_all([Src|Srcs], [Dst|Dsts], I, Env, NewEnv) ->
  case hipe_sparc:is_imm(Src) of
    true ->
      bind_all(Srcs, Dsts, I, Env, bind(NewEnv, Dst, Src));
    false ->  %% its a variable
      SrcVal = lookup(Src, Env),
      %% Uncomment this and only constants will be propagated
      %% case hipe_rtl:is_imm(SrcVal) of
      %%   true ->
      NewI = hipe_sparc:subst_uses(I,[{Src, SrcVal}]),
      bind_all(Srcs, Dsts, NewI, Env, bind(NewEnv, Dst, SrcVal))
      %%  false ->
      %%     bind_all(Srcs, Dsts, I, Env, NewEnv)
      %% end
  end;
bind_all([], [], I, _, Env) ->
  {I, Env}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


