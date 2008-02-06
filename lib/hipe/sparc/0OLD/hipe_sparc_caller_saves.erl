%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%%  Filename : 	hipe_sparc_caller_saves.erl
%%  Module   :	hipe_sparc_caller_saves
%%  Purpose  :  To add save and restore code for caller save regs
%%              at call sites.
%%  Notes    :  LastSpillPos is a bad name in this module,
%%               since it denotes the postion after the last used
%%               position.
%%  History  :	* 2001-11-16 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: mikpe $
%%              $Date: 2007/12/18 09:18:21 $
%%              $Revision: 1.1 $
%% ====================================================================
%%  Exports  :
%%
%%  Description:
%%
%%   1. Calculate liveness. 
%%      (Consider reusing the liveness calculated by the regalloc.)
%%
%%   2. Go through each basic block bottom up.
%%      For each call c:
%%         For each live out temp t. (that is not a retval of c)
%%             If t is spilled: Do nothing.
%%             If t is not spilled:
%%                a) Get a spillposition p for t.
%%                b) Add code for spilling t to p before the call.
%%                c) add code for unspilling t from p after the call.
%%                d) If any spillcode was added rewrite the call
%%                     If there is an exception handler: add the unspill
%%                       code to both branches of the call.
%%
%%   3. Optimization pass: Place the spillcode "optimally".
%%                        Implemented in hipe_sparc_opt_frame
%%
%%  TODO: Either make this module completely target independent or
%%        completely target dependent.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc_caller_saves).
-export([rewrite/5]).

-define(HIPE_INSTRUMENT_COMPILER, true). %% Turn on instrumentation.
-include("../main/hipe.hrl").
-include("hipe_sparc.hrl").

%% XXX: Make target independent.
-define(TARGET, hipe_sparc_specific).

%% XXX: Make target independent.
rewrite(Cfg, TempMap, FpMap, LastSpillPos, Options) ->
  %% hipe_sparc_cfg:pp(Cfg),  
  ?opt_start_timer("Caller Saves"),
  ?start_time_caller_saves(Options),
  %% Get Info.
  Liveness = liveness(Cfg, ?TARGET),
  State = new_state(Liveness,  Cfg, TempMap, FpMap, LastSpillPos),
  BBs = hipe_sparc_cfg:predictionorder(Cfg),

  %% Rewrite.
  {NewCode, NextPos, NewBlocks} = traverse(BBs, State),

  %% Make new CFG.
  Sparc0 = hipe_sparc_cfg:linearize(Cfg),
  Sparc1 = hipe_sparc:sparc_code_update(Sparc0, NewCode),
  Sparc2 = hipe_sparc:sparc_var_range_update(
	     Sparc1, {0,hipe_gensym:get_var(sparc)}),
  Sparc3 = hipe_sparc:sparc_label_range_update(
	     Sparc2, {0,hipe_gensym:get_label(sparc)}),  
  Cfg1 = hipe_sparc_cfg:init(Sparc3),

  %% Minimize the number of loads and stores.
  ?opt_stop_timer("Caller Saves"),
  ?opt_start_timer("Opt Frame"),
  %% hipe_sparc_cfg:pp(Cfg1),
  Cfg2 = hipe_sparc_opt_frame:cfg(Cfg1),
  %% hipe_sparc_cfg:pp(Cfg2),
  ?opt_stop_timer("Opt Frame"),
  ?stop_time_caller_saves(Options),
  case proplists:get_value(regalloc,Options) of
    cs ->
      {Cfg2, NextPos, NewBlocks};
    _ ->
      {Cfg2, NextPos}
  end.

%% Go through each BasicBlock
traverse(BBs, State) ->
  State1 = lists:foldr(fun handle_bb/2,State,BBs),  
  {state__instrs(State1),state__spill_index(State1),state__newblocks(State1)}.
  
%% For each BasicBlock go through the code bottom-up.
handle_bb(BB,State) ->
  %% io:format("--------------\n",[]),
  Code =  hipe_bb:code(bb(state__cfg(State),BB, ?TARGET)),
  State1 = state__enter_bb(BB, State),
  State2 = bottom_up(Code, State1),
  %% io:format("-- Block ~w --\n",[BB]),
  state__add_instrs(hipe_sparc:label_create(BB), State2).

%% Go through each instruction from the last to the first.
%% (Collect liveness from live out and backwards.)
bottom_up(Is, State) ->
  lists:foldr(fun handle/2,State,Is).

%% For each instruction rewrite calls so that live temps are saved
handle(I,State) ->
  {FpDefs, Defs}  = defines(I, ?TARGET),
  {FpUses, Uses} = uses(I, ?TARGET),
  LiveOut = state__live(State)-- hipe_sparc_registers:global(),
  Live = (LiveOut--Defs), %% Remove killed
  LiveIn = ordsets:union(ordsets:from_list(Live),ordsets:from_list(Uses))
    -- hipe_sparc_registers:global(),

  FpLiveOut = state__live_fp(State),
  FpLive = (FpLiveOut--FpDefs), %% Remove killed
  FpLiveIn = ordsets:union(ordsets:from_list(FpLive),ordsets:from_list(FpUses)),


    %% io:format("~nLiveOut: ~w~n",[LiveOut-- hipe_sparc_registers:global()]),
    %% hipe_sparc_pp:pp_instr(I),
    %% io:format("LiveIn: ~w~n",[LiveIn]),

  %% Add save and restores if neccessary.
  State1 = update_call(I,State,Live, LiveOut, FpLive),

  %% Update the liveness information in the state.
  State2 = state__update_live(LiveIn, State1),
  state__update_live_fp(FpLiveIn, State2).


%% XXX: Make this target independent.
update_call(I,State,Live,_LiveOut,FpLive) ->
  case I of
    #call_link{} ->
      case {live_in_regs(Live), FpLive} of
	{[], []} -> 
	  %% All live temps are allready on the stack.
	  %% No need for extra saving.
	  %% Just add the live-info to the stack descriptor 
	  %%  of the call.
	  state__add_instrs(set_live_slots(I,State,Live),State);
	{ToSpill, ToSpillFp} ->
	  %% We have at least one live temp that is not on the stack
	  handle_call(I,State,ToSpill,Live, ToSpillFp)
      end;
    
    _ ->
      %% This is not a call, nothing to save.
      %% Just add the instruction to the state.
      state__add_instrs(I,State)
  end.



%% Ugly function that finds the stack slots of all live temps
%% and adds these to the stack descriptor of the call. 
set_live_slots(Call,State,LiveOut) ->
  Live =
    [stack_pos(T,State, reg) || T <- LiveOut],
  SD = hipe_sparc:call_link_stack_desc(Call),
  NewSD = hipe_sparc:sdesc_live_slots_update(SD, Live),
  hipe_sparc:call_link_stack_desc_update(Call,NewSD).

%% Helper function to set_live_slots.
%% Maps a Temp to its stack pos.
stack_pos(T,State, Type) ->
  Map = 
    case Type of
      reg -> state__tempmap(State);
      fp_reg-> state__fpmap(State)
    end,
  case hipe_temp_map:find(T, Map) of
    {spill, P} ->
      P;
    _ ->
      element(1,get_pos(T,state__extramap(State), Type))
  end.
      
%% XXX: Make target independent.
live_in_regs(Live) ->
  %% XXX: We have to save spilled regs with new call-split allocation
  %%      Make this modular so other allocators are unaffected.
  [T || T <- Live ] 
    -- hipe_sparc_registers:global().


handle_call(I,State,ToSpill,LiveOut, FpLive)->
  %% a) Get a spillposition p for t.
  %% b) Add code for spilling t to p before the call.
  %% c) add code for unspilling t from p after the call.
  %% d) If any spillcode was added rewrite the call
  %%      If there is an exception handler: add the unspill
  %%        code to both branches of the call.


  ExtraMap = state__extramap(State),
  {SpillPositions, NewExtraMap0} =
    get_spill_positions(ToSpill, ExtraMap, State, reg),
  {Saves0, Restores0} = gen_save_restore(SpillPositions, reg),
  {SpillPositions_fp, NewExtraMap} =
    get_spill_positions(FpLive, NewExtraMap0, State, fp_reg),
  {Saves1, Restores1} = gen_save_restore(SpillPositions_fp, fp_reg),

  State1 = state__update_extramap(NewExtraMap, State),

  ContLab = hipe_sparc:call_link_continuation(I),
  FailLab = hipe_sparc:call_link_fail(I),
  NewContLab = hipe_sparc:label_create_new(),
  GotoCont =  hipe_sparc:goto_create(ContLab),
  {NewI,State2} =
    case ContLab of
      [] -> {I, State1};
      _ ->
	{
	hipe_sparc:call_link_continuation_update(
	  I,
	  hipe_sparc:label_name(NewContLab)),
	state__add_block(hipe_sparc:label_name(NewContLab),ContLab, State1)}
    end,

   
  %% Update stack descriptor. Fp regs must not be marked as live.
  NewCall = set_live_slots(NewI,State2,LiveOut),

  case ContLab of
    [] ->
       NewCode = 
	[Saves0,
	 Saves1,
	 NewCall,
	 Restores0,
	 Restores1],
      state__add_instrs(NewCode,State2);
    _ ->
      case FailLab of
	[] ->
	  NewCode = 
	    [Saves0,
	     Saves1,
	     NewCall,
	     NewContLab,
	     Restores0,
	     Restores1,
	     GotoCont],
	  state__add_instrs(NewCode,State2);
	_ ->
	  NewFailLab = hipe_sparc:label_create_new(),
	  GotoFail =  hipe_sparc:goto_create(FailLab),
	  NewI2 = 
	    hipe_sparc:call_link_fail_update(
	      NewCall,
	      hipe_sparc:label_name(NewFailLab)),
	  State3 = 
	    state__add_block(hipe_sparc:label_name(NewFailLab),FailLab, State2),
	  NewCode = 
	    [Saves0, 
	     Saves1,
	     NewI2,
	     NewContLab,
	     Restores0,
	     Restores1,
	     GotoCont,
	     NewFailLab,
	     Restores0,
	     Restores1,
	     GotoFail],
	  state__add_instrs(NewCode,State3)
      end
  end.


get_spill_positions([T|Temps], ExtraMap, State, Type) ->
  TempMap = 
    case Type of
      reg -> state__tempmap(State);
      fp_reg -> state__fpmap(State)
    end,
  {Pos, NewMap} = 
    case hipe_temp_map:is_spilled(T, TempMap) of
      true ->
	{stack_pos(T, State, Type),ExtraMap};
      false ->
	get_pos(T,ExtraMap, Type)
    end,
  {Positions, Map} = get_spill_positions(Temps, NewMap, State, Type),
  {[{T,Pos}|Positions], Map};
get_spill_positions([],Map,_, _) ->
  {[],Map}.

gen_save_restore([{T,Pos}|Positions], Type) ->
  SparcTemp = 
    case Type of
      fp_reg -> hipe_sparc:mk_fpreg(T);
      reg -> hipe_sparc:mk_reg(T)
    end,
  SparcPos = hipe_sparc:mk_imm(Pos),
  {Saves, Restores} = gen_save_restore(Positions, Type),
  Save = hipe_sparc:pseudo_spill_create(SparcTemp,SparcPos),
  Restore = hipe_sparc:pseudo_unspill_create(SparcTemp,SparcPos),
  {[Save | Saves], [Restore | Restores]};
gen_save_restore([], _) -> {[],[]}.



%% ================================================================= %%
%%                             A  D  T s                             %%
%% ================================================================= %%


%% ================================================================= %%
%% The ExtraMap structure
%% Maps unspilled temps to spillpositions.
%% 

%% ____________________________________________________________________
%%  new_extramap(LastSpillPos)    
%% Returns: 	An empty ExtraMap.
%% Arguments:	LastSpillPos - The next spill will be to this pos.
%% Description:	Creates an empty ExtraMap 
%% ____________________________________________________________________
new_extramap(LastSpillPos) ->
  {LastSpillPos, empty()}.

index(Map) ->
  %% io:format("Map: ~w\n",[Map]),
  element(1,Map).

%% ____________________________________________________________________
%%  get_pos(T,ExtraMap, Type)    
%% Returns: 	
%% Arguments:	
%% Description:	Finds the spill position of the temp T,
%%              If T is not in the map T will be added. 
%%              Type is either reg or fp_reg 
%% ____________________________________________________________________
get_pos(T,{Next,Map}, Type) ->
  case lookup(T,Map) of
    none ->
      case Type of
	fp_reg -> {Next, {Next+2,insert(T,Next,Map)}};
	reg -> {Next, {Next+1,insert(T,Next,Map)}}
      end;
    {value, Pos} ->
      {Pos, {Next,Map}}
  end.


%% ____________________________________________________________________
%% 
%% The Map kernel.


empty() ->
  gb_trees:empty().
lookup(T,Map) ->
  gb_trees:lookup(T,Map).
insert(T,Next,Map) ->
  gb_trees:insert(T,Next,Map).



%% ================================================================= %%
%%
%% The state structure
%%
%% ================================================================= %%

%% ____________________________________________________________________
%% The state record.
-record(state, {instrs, extramap, live, live_fp, liveness, cfg, tempmap, fpmap, newblocks}).

%% Create a new state.
new_state(Liveness,  Cfg, TempMap, FpMap, LastSpillPos) ->
  #state{instrs = [], 
	 extramap = new_extramap(LastSpillPos), 
	 live = [],
	 live_fp = [],
	 liveness = Liveness,  
	 cfg = Cfg,
	 tempmap = TempMap,
	 fpmap = FpMap,
	 newblocks = []}.

%% ____________________________________________________________________
%% Selectors

state__instrs(#state{instrs=Instrs}) -> lists:flatten(Instrs).
state__extramap(#state{extramap=ExtraMap}) -> ExtraMap.
state__live(#state{live=Live}) -> Live.
state__live_fp(#state{live_fp=LiveFP}) -> LiveFP.
state__liveness(#state{liveness=Liveness}) -> Liveness.
state__cfg(#state{cfg=CFG}) -> CFG.
state__tempmap(#state{tempmap=TempMap}) -> TempMap.
state__fpmap(#state{fpmap=FpMap}) -> FpMap.
state__newblocks(#state{newblocks=NewBlocks}) -> NewBlocks.

state__spill_index(State) -> index(state__extramap(State)).

%% ____________________________________________________________________
%% Updates

state__enter_bb(BB, State) ->
  {FpReg, Reg} = liveout(state__liveness(State), BB, hipe_sparc_specific),
  State#state{live=ordsets:from_list(Reg), live_fp=ordsets:from_list(FpReg)}.
state__add_instrs(I,State) ->
  State#state{instrs=[I|state__instrs(State)]}.
state__update_extramap(NewExtraMap, State) ->
  State#state{extramap=NewExtraMap}.
state__update_live(NewLive, State) ->
  State#state{live=NewLive}.
state__update_live_fp(NewLive, State) ->
  State#state{live_fp=NewLive}.
state__add_block(New,Old, State) ->
   State#state{newblocks=[{New,Old}|state__newblocks(State)]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Interface to target specific external functions.
%% XXX: Make this efficient somehow...
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

liveness(CFG, Target) ->
   Target:analyze(CFG).

bb(CFG, L, Target) ->
  Target:bb(CFG,L).

liveout(Liveness,L, Target)->
  regnames(Target:liveout(Liveness,L), Target).

uses(I, Target)->
  regnames(Target:uses(I), Target).

defines(I, Target) ->
  regnames(Target:defines(I), Target).

regnames(Regs, _Target) ->
  regnames(Regs, [], []).

regnames([Reg|Rest], Fp_regs, Regs) ->
  case hipe_sparc:is_fpreg(Reg) of
    true->
      regnames(Rest, [hipe_sparc:fpreg_nr(Reg) | Fp_regs], Regs);
    _ ->
      regnames(Rest, Fp_regs, [hipe_sparc:reg_nr(Reg) | Regs])
  end;
regnames([], Fp_regs, Regs) ->
  {Fp_regs, Regs}.
  
