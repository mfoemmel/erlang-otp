%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%%  Filename : 	hipe_sparc_prop.erl
%%  Module   :	hipe_sparc_prop
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-12-04 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: richardc $
%%              $Date: 2002/10/01 12:47:16 $
%%              $Revision: 1.5 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc_pre_ra_prop).
-export([cfg/1]).
-include("../main/hipe.hrl").

cfg(CFG) ->
  %%  hipe_sparc_cfg:pp(CFG),
  Lbls = [hipe_sparc_cfg:start(CFG)],
  {CFG0,GEnv0} = prop_bbs(Lbls, CFG, new_genv(CFG),[]),
  %% hipe_sparc_cfg:pp(CFG0),
  CFG1 = prop(Lbls, CFG0, GEnv0,100),
  CFG2 = remove_dead(CFG1),
  CFG2.

remove_dead(CFG) ->
  Liveness = hipe_sparc_liveness:analyze(CFG),
  Lbls = hipe_sparc_cfg:labels(CFG),
  bwd_prop(Lbls, CFG, Liveness).

bwd_prop([L|Lbls], CFG, Liveness) ->
  BB = hipe_sparc_cfg:bb(CFG, L),
  LiveOut = hipe_sparc_liveness:liveout(Liveness, L),
  {NewCode,_} = bwd_prop_bb(hipe_bb:code(BB),LiveOut),
  NewBB = hipe_bb:code_update(BB, NewCode),
  NewCFG = hipe_sparc_cfg:bb_update(CFG, L, NewBB),
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
  
  case ordsets:union(Defines,Live) of
    [] ->
      %% Nothing defined is live -- potentialy dead
      case can_kill(I) of
	true ->
	  {hipe_sparc:comment_create({"Removed instr",I},[I]),
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
      Dest = hipe_sparc:reg_nr(hipe_sparc:pseudo_spill_reg(I)),
      case hipe_sparc_registers:is_precolored(Dest) of
	true -> false;
	_ -> true
      end;
    move ->
      Dest = hipe_sparc:reg_nr(hipe_sparc:move_dest(I)),
      case hipe_sparc_registers:is_precolored(Dest) of
	true -> false;
	_ -> true
      end;
    _ -> 
      false
  end.



prop(Start,CFG,Env,0) ->
  io:format("Limit hit\n"),
  CFG;
prop(Start,CFG,Env,N) ->
  case changed(Env) of
    true ->
      {CFG0,GEnv0} = prop_bbs(Start, CFG, clear_changed(Env), []),
      prop(Start,CFG0, GEnv0, N-1);
    false ->
      CFG
  end.

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
      %% io:format("\n~w:\n========\n~p\n",[Lbl,env(set_active_block(Lbl,GEnv))]),
      {NewCode, NewGEnv} = prop_instrs(hipe_bb:code(BB), 
				       set_active_block(Lbl,GEnv)),
      NewBB = hipe_bb:code_update(BB, NewCode),
      NewCFG = hipe_sparc_cfg:bb_update(CFG, Lbl, NewBB),
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
  GEnv0 = set_env(Env0,GEnv),
  {NewIs, NewEnv} = prop_instrs(Is, GEnv0),

  case NewI of
    [_|_] -> {NewI++NewIs, NewEnv};	%% alub -> [move, goto]
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
      {I0,Env0} = bind_all(Srcs, Dsts, I, env(Env)),
      {I, kill_uses(hipe_sparc:defines(I0), Env0)};
    multimove ->
      NewEnv = unbind(hipe_sparc:defines(I), env(Env)),
      Srcs = hipe_sparc:multimove_src(I),
      Dsts = hipe_sparc:multimove_dest(I),
      bind_all(Srcs, Dsts, I, NewEnv);
    _ ->
      %% Uses = hipe_sparc:uses(I),
      %% %% Map = [{U, lookup(U, Env)} || U <- Uses],
      %% Map = map_all(Uses, env(Env)),
      %% NewI = hipe_sparc:subst_uses(I,Map),
      %% eval(NewI, env(Env))
      eval(I, env(Env))
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
%    cmov_cc -> prop_cmov_cc(I,Env);
%    cmov_r -> prop_cmov_r(I,Env);
    alu -> prop_alu(I,Env);
%    alu_cc -> prop_alu_cc(I,Env);
%    sethi ->  prop_sethi(I,Env);

%    load_atom ->  prop_load_atom(I,Env);
%    load_word_index ->  prop_word_index(I,Env);
%    load_address ->  prop_load_address(I,Env);


%    b ->  prop_b(I,Env);
%    br ->  prop_br(I,Env);
%    goto ->  prop_got(I,Env);
%    jmp ->  prop_jmp(I,Env);

    call_link ->  prop_call_link(I,Env);

    nop ->  {I,Env};
    align ->  {I,Env};

    _ -> 
      NewEnv = unbind(hipe_sparc:defines(I), Env),
      {I,NewEnv};

    comment -> {I,Env}

end.    

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
      NewI = hipe_sparc:comment_create({"Removed spill",I},[]),
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
  case sp(Env) of
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

    case hp(Env) of
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
  case sp(Env) of
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
	      bind_all([Val],[Dest],I,kill_uses([Dest],Env))
	  end
      end
  end.
 
prop_heap_load(I,Env,Offset,Dest) ->
  case hp(Env) of
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
prop_sp_op(I,Env,Op,Src) ->
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
prop_hp_op(I,Env,Op,Src) ->
  %% Dont know how to handle other ops...
  {I,kill_hp(zap_heap(Env))}.

%% ____________________________________________________________________
%% 
prop_call_link(I,Env) ->
  Dests = hipe_sparc:call_link_dests(I),
  Env1 = kill_uses(Dests,kill_phys_regs(Env)),
  {I,Env1}.


%% ____________________________________________________________________
%% 
% map_all([], Env) ->
%   [];
% map_all([V|Vs], Env) ->
%   [{V, lookup(V, Env)} | map_all(Vs, Env)].


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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% the environment, Rewrite if we go global.
%% {Regs,SP,HP, Stack, Heap}
-record(genv,{block,cfg,succ_map,pred_map,env,envs,changed}).
new_genv(CFG)->  
  #genv{cfg=CFG,
	succ_map=hipe_sparc_cfg:succ_map(CFG),
	pred_map=hipe_sparc_cfg:pred_map(CFG),
	env=new_env(),
	envs=[],
	changed=false
       }.
set_env(Env,GEnv) ->
  GEnv#genv{env=Env}.
env(GEnv) ->
  GEnv#genv.env.
env(L,GEnv) ->
  case plookup(L, GEnv#genv.envs) of
    undefined ->new_env();
    E -> E
  end.
      
preds(L,GEnv) ->
  hipe_sparc_cfg:pred(GEnv#genv.pred_map,L).

succ(GEnv) ->
  hipe_sparc_cfg:succ(GEnv#genv.succ_map, GEnv#genv.block).

end_of_bb(GEnv) ->
  Env = GEnv#genv.env,
  L = GEnv#genv.block,
  Envs =  GEnv#genv.envs,
  case plookup(L,Envs) of
    Env -> GEnv;
    Env0 ->
      GEnv#genv{envs=[{L,Env}|Envs], changed=true}
  end.

changed(GEnv) ->
   GEnv#genv.changed.

clear_changed(GEnv) ->
   GEnv#genv{changed=false}.


set_active_block(L, GEnv) ->
  case preds(L,GEnv) of
    [Pred] ->
      GEnv#genv{block=L, env=env(Pred,GEnv)};
    [Pred|Preds] ->
      case plookup(Pred, GEnv#genv.envs) of
	undefined -> GEnv#genv{block=L, env=new_env()};
	E ->
	  %% io:format("Joining: L:~w\n",[Pred]),
	  %% pp_env(E),
	  Env = join(Preds,E, GEnv),
	  GEnv#genv{block=L, env=Env}
      end;
    _ ->
      GEnv#genv{block=L, env=new_env()}
  end.

join([L|Ls], Env, GEnv) ->
%%  io:format("Joining: L:~w\n",[L]),
  case plookup(L, GEnv#genv.envs) of
    undefined -> new_env();
    E ->
  %%    pp_env(E),
      join(Ls,merge(E,Env), GEnv)
  end;
join([],Env,Genv) ->
 %% io:format("Join:\n"),
 %% pp_env(Env),
  Env.

%% ____________________________________________________________________
%% 
-record(env,{regs=[],sp=0,hp=0,stack=[],heap=[]}).
new_env()->  #env{}.
regs(Env)-> Env#env.regs.
sp(Env)-> Env#env.sp.
hp(Env)-> Env#env.hp.
stack(Env)-> Env#env.stack.
heap(Env)-> Env#env.heap.

merge(E1, E2) ->
  Env0 = #env{regs=m(regs(E1),regs(E2))},
  Env1 = 
    case sp(E1) =:= sp(E2) of
      true ->
	Stack = m(stack(E1),stack(E2)),
	Env0#env{sp=sp(E1),stack= Stack};
      false -> Env0
    end,
  Env2 = 
    case hp(E1) =:= hp(E2) of
      true ->
	Heap = m(heap(E1),heap(E2)),
	Env1#env{hp=hp(E1),heap=Heap};
      false -> Env1
    end,
  Env2.
    
m(R1,R2) ->
  [{X,Y} || {X,Y} <- R1, {value, {X,Y}} =:= lists:keysearch(X, 1, R2)].


pp_lenv(GEnv) ->
  Env = env(GEnv),
  pp_env(Env).

pp_env(Env) ->
  pp_regs(regs(Env)),
  pp_mem(sp(Env),stack(Env),"STACK"),
  pp_mem(hp(Env),heap(Env)," HEAP").

pp_regs([{X,Y}|Regs]) ->
  case hipe_sparc:is_reg(X) of
    false ->
      ?EXIT({bad_reg_in_environment,{X,Y}});
    true ->
      hipe_sparc_pp:pp_arg(standard_io,X),
      io:format(" <- "),
      hipe_sparc_pp:pp_arg(standard_io,Y),
      io:format("\n"),
      pp_regs(Regs)
  end;
pp_regs([]) -> ok.

pp_mem(unknown,_,_) ->
  ok;
pp_mem(_,[],_) ->
  ok;
pp_mem(Pointer,Mem,What) ->
  io:format("~s:\n",[What]),
  case lists:sort(Mem) of
    [] -> ok;
    [First|_] = Slots ->
      pp_slots(min(element(1,First),Pointer),Pointer,Slots,What),
      io:format("               |-----|\n")
  end.

min(X,Y) when X =< Y ->
  X;
min(_,Y) ->
  Y.


pp_slots(Cur, Pointer, Slots = [{Next,V}|Rest], What) ->
  if Cur =:= Pointer ->
      io:format("~s_P -> ",[What]);
     true ->
      io:format("           ")
  end,
  io:format("~3w | ",[Cur]),
  if Cur =:= Next ->
      hipe_sparc_pp:pp_arg(standard_io, V),
      io:format(" |\n"),
      pp_slots(Cur+4, Pointer, Rest, What);
     true ->
      io:format("           |\n"),
      pp_slots(Cur+4, Pointer, Slots, What)
  end;
pp_slots(Cur,Pointer,[],What) ->
  if Cur =< Pointer ->
      pp_empty_slots(Cur,Pointer,What);
     true ->
      ok
  end.

pp_empty_slots(Cur,Pointer,What) ->
  if Cur =:= Pointer ->
      io:format("~s_P -> ~3w |      |\n",[What,Cur]);
     Cur > Pointer ->
      ?EXIT(bad_env);
     true ->
      io:format("           ~3w |      |\n",[Cur]),
      pp_empty_slots(Cur+4,Pointer,What)
  end.

  

zap_stack(Env)-> Env#env{stack=[]}.
zap_heap(Env)-> Env#env{heap=[]}.
kill_sp(Env)->Env#env{sp=unknown}.
kill_hp(Env)->Env#env{hp=unknown}.
kill_phys_regs(Env)->
  Env0 = Env#env{regs=[{X,Y}||{X,Y} <- regs(Env),
			    not_physical(X), not_physical(Y)]},
  Env1 = Env0#env{stack=[{X,Y}||{X,Y} <- stack(Env0),
				not_physical(Y)]},
  Env2 = Env1#env{heap=[{X,Y}||{X,Y} <- heap(Env1),
			       not_physical(Y)]}.
  

not_physical(R) ->
  case hipe_sparc:is_reg(R) of
    false -> true;
    true ->
      not hipe_sparc_registers:is_precolored(hipe_sparc:reg_nr(R))
  end.
		
inc_sp(Env, Val) ->  
  case sp(Env) of
    unknown ->
      Env;
    PrevVal ->
      Env#env{sp=PrevVal+Val}
  end.
inc_hp(Env, Val) ->  
  case hp(Env) of
    unknown ->
      Env;
    PrevVal ->
      Env#env{hp=PrevVal+Val}
  end.


%% Bind a value/reg to a stack position
bind_spos(Pos, Val, Env) ->
  Env#env{stack=[{Pos,Val}| lists:keydelete(Pos,1,Env#env.stack)]}.

%% Bind a value/reg to a heap position
bind_hpos(Pos, Val, Env) ->
  Env#env{heap=[{Pos,Val}| lists:keydelete(Pos,1,Env#env.heap)]}.

%% Find a value on a stackpos
find_spos(Pos, Env) ->
  plookup(Pos,Env#env.stack).

%% Find a value on a heappos
find_hpos(Pos, Env) ->
  plookup(Pos,Env#env.heap).

%%
%% Find what memory pos P is bound to.
%% As a last resort undefined is returned.
%%
plookup(P, []) ->
  undefined;
plookup(P, [{P, Y}|_]) ->
  Y;
plookup(P, [_|Map]) ->
  plookup(P, Map).

%%
%% Bind reg X to Y in Map
%%

bind(Map, X, Y) ->
  Regs = Map#env.regs,
  Map#env{regs=[{X, Y} | lists:keydelete(X,1,Regs)]}.

%%
%% Find what X is bound to (as a last restort varaibles are bound 
%% to themself).
%%

lookup(X, Env) ->
  mlookup(X,regs(Env)).

mlookup(X, []) ->
  X;
mlookup(X, [{X, Y}|_]) ->
  Y;
mlookup(X, [_|Map]) ->
  mlookup(X, Map).

%%
%% Kill bindings with references to register X
%%
kill(X, Env0)->
  Env1 = Env0#env{regs=mkill(X,regs(Env0))},
  Env2 = Env1#env{stack=mkill(X,stack(Env1))},
  Env3 = Env2#env{heap=mkill(X,heap(Env2))}.

mkill(X, []) ->
  [];
mkill(X, [{X,_}|Xs]) ->
  mkill(X, Xs);
mkill(X, [{_,X}|Xs]) ->
  mkill(X, Xs);
mkill(X, [D|Xs]) ->
  [D | mkill(X,Xs)].

unbind(X, Env)->
  Env#env{regs=munbind(X,regs(Env))}.

munbind([], Map) ->
  Map;
munbind([V|Vs], Map) ->
  munbind(Vs, mkill(V, Map)).


kill_use(X, []) ->
  [];
kill_use(X, [{_,X}|Xs]) ->
  kill_use(X, Xs);
kill_use(X, [D|Xs]) ->
  [D | kill_use(X,Xs)].

kill_uses([X|Xs],Env0) ->
  Env1 = Env0#env{regs=kill_use(X,regs(Env0))},
  Env2 = Env1#env{stack=kill_use(X,stack(Env1))},
  Env3 = Env2#env{heap=kill_use(X,heap(Env2))},
  kill_uses(Xs,Env3);
kill_uses([], Env) -> Env.
