%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2000 by Erik Johansson.  All Rights Reserved 
%% Time-stamp: <01/08/05 18:03:49 happi>
%% ====================================================================
%%  Filename : 	sparc_memory_regalloc.erl
%%  Module   :	sparc_memory_regalloc
%%  Purpose  :  To do a silly register allocation to be used as
%%               baseline for benchmarking register allocators.
%%  Notes    : 
%%  History  :	* 2000-08-21 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: happi $
%%              $Date: 2001/08/05 16:04:57 $
%%              $Revision: 1.2 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc_ra_memory).
-export([alloc/2]).
-define(HIPE_INSTRUMENT_COMPILER, true). %% Turn on instrumentation.
-include("../main/hipe.hrl").

alloc(SparcCfg, Options) ->
  EntryPoints = [start(SparcCfg)|fail_entrypoints(SparcCfg)], 
  
  {Code, NoSpills, NewConstTab}  = 
    traverse(EntryPoints, SparcCfg, 
	     hipe_sparc_cfg:data(SparcCfg)),

  ?add_spills(Options, NoSpills),

  %% Create linear sparc datastructure
  Sparc = make_sparc(Code, SparcCfg),
  NewSparcCgf = init(Sparc, fail_entrypoints(SparcCfg)),  
  FinalSparcCfg = hipe_sparc_cfg:update_data(NewSparcCgf,
					     NewConstTab),

  %% return CFG and the new const table.
  FinalSparcCfg.

make_sparc(Code, SparcCfg) ->
  Fun = function(SparcCfg),
  VarRange = var_range(SparcCfg),
  LabelRange= label_range(SparcCfg),
  Data = hipe_sparc_cfg:data(SparcCfg),
  mk_sparc(Fun, Code, Data, VarRange, LabelRange).

traverse(EntryPoints, SparcCFG, ConstTab) ->
  traverse(EntryPoints, %% To do list
	   [],          %% Visited
	   [],          %% NewCode
	   empty_allocation(),          %% Allocations
	   SparcCFG,    %% The CFG
	   succ_map(SparcCFG), %% Successormap
	   pred_map(SparcCFG),  %% Predecessors
	   ConstTab
	  ).

traverse([L|ToDo], Visited, NewCode, 
	 Allocations, CFG, SuccMap, PredMap, ConstTab) ->

  %% Check if we already have processed this BB
  case lists:member(L, Visited) of
    false -> %% This is the first time we see this Block
      Code = hipe_bb:code(bb(CFG,L)),
      {NewNewCode, NewAllocation, NewConstTab} = 
	traverse_code([label_create(L,[])|Code], 
		      Allocations,
		      NewCode,          %% Code
		      ConstTab
		     ),


      %% The code is reversed so the first instruction is the jump (at the end...)
      [Jmp|BBCode] = NewNewCode,

      Succ = succ(SuccMap, L),
      NewToDo = order_succ(Jmp, Succ, ToDo),

      %% Indicate that we are done with this BB.
      NewVisited = [L|Visited],

      traverse(NewToDo, [L|Visited], 
	       NewNewCode,
	       NewAllocation,
	       CFG,
	       SuccMap, PredMap, NewConstTab);
    
    true -> %% We have seen this BB already, ignore it.
      traverse(ToDo, Visited, NewCode,
	       Allocations, 
	       CFG, SuccMap, PredMap, ConstTab)
  end;
traverse([], _, Code,  Allocation, _, _, _, ConstTab) -> 
  {lists:reverse(Code), no_spills(Allocation), ConstTab}.

order_succ(_,   [],    ToDo) -> ToDo;
order_succ(_,   [Succ],ToDo) -> [Succ|ToDo];
order_succ(Jmp, [Taken, NotTaken], ToDo) ->
  Pred = cond_pred(Jmp),
  if Pred >= 0.5 -> [Taken, NotTaken | ToDo];
     true -> [NotTaken, Taken | ToDo]
  end;
order_succ(Jmp, Multi, ToDo) ->
  Multi ++ ToDo. %% This is not realy supported.

cond_pred(I) ->
   case hipe_sparc:type(I) of
      br -> hipe_sparc:br_pred(I);
      b -> hipe_sparc:b_pred(I);
      _ -> 0.5
   end.




traverse_code([I|Is], Allocation, Code, ConstTab) ->
  Uses = uses(I),


  %% TODO:
  %% When allocating make sure all regs used in the instruction are kept, 
  %%  i.e. not spilled to make room for the other registers...
 
  %% For each use make sure it is allocated, otherwise unspill it.
  %% (There *can't* be a reg that is neither allocated nor spilled.)
  {UseSubst, Regallocation, Free, NewCode} = 
    allocate_uses(Uses,     %% Used temps
		   Allocation,
		   [],       %% Substlist
		   Code      %% Acccode
		  ),
  Defs = defines(I),
  %% For each def see if it is allocated otherwise allocate it.
  {NewAllocation, DefSubst, StoreCode, NewConstTab} = 
    allocate_defs(Defs, Allocation, Regallocation, Free, ConstTab),


  %% Rewrite the instruction.
  NewI = subst_defines(subst_uses(I, UseSubst), DefSubst),

  traverse_code(Is, NewAllocation, 
		StoreCode ++ [NewI|NewCode], NewConstTab);
traverse_code([], Allocation, Code, ConstTab) ->
  {Code, Allocation, ConstTab}.





allocate_defs(Defs, SpillAllocation, Allocation, Free, ConstTab) ->
  allocate_defs(Defs, SpillAllocation, Allocation, Free, [], [], ConstTab).

allocate_defs([Temp|Temps],  SpillAllocation, Allocation, Free,
	      Substs, StoreCode, ConstTab) ->
  case is_precolored(Temp) of
    true ->
      
      allocate_defs(Temps, SpillAllocation,  Allocation, Free, 
		    [{mk_reg(Temp),
		      mk_reg(physical_name(Temp))}|
		      Substs], StoreCode, ConstTab);
    _ ->
      {Pos, NewSpillAllocation, NewConstTab} =
	case spill_allocated(Temp, SpillAllocation) of
	  false ->
	    allocate_new(Temp, SpillAllocation, ConstTab);
	  SPos -> {SPos, SpillAllocation, ConstTab}
	end,
      {PhysReg, NewAllocation, NewFree} =
	case allocated(Temp, Allocation) of 
	  false -> 
	    allocate(Temp, Free, Allocation);
	  Reg ->
	    {Reg, Allocation, Free}
	end,
      NewSpillCode = get_spill_code(PhysReg, Pos),
      Mapping = [{mk_reg(Temp), mk_reg(PhysReg)}| Substs],
      allocate_defs(Temps, NewSpillAllocation,  NewAllocation, NewFree, Mapping, 
		    NewSpillCode ++ StoreCode, NewConstTab)
  end;
allocate_defs([], SpillAllocation, Allocation, Free, DefSubsts,
	      StoreCode, ConstTab) ->
  {SpillAllocation, DefSubsts, StoreCode, ConstTab}.


allocate_uses(Uses, SpillAllocation, Substs, Code) ->
  allocate_uses(Uses, empty_regalloc(), free_regs(), SpillAllocation, Substs, Code).

allocate_uses([Temp|Temps], Allocation, Free, SpillAllocation, Substs, Code) ->
  case is_precolored(Temp) of
    true ->
      allocate_uses(Temps, Allocation, Free , SpillAllocation, 
		    [{mk_reg(Temp),
		      mk_reg(physical_name(Temp))}|Substs],
		    Code);
    false ->
      {PhysReg, NewAllocation, NewFree} =
	case allocated(Temp, Allocation) of 
	  false -> 
	    allocate(Temp, Free, Allocation);
	  Reg ->
	    {Reg, Allocation, Free}
	end,
      case spill_allocated(Temp, SpillAllocation) of
	false ->
	  ?EXIT(this_shouldnthappen);
	SpillPos ->
	  NewSpillCode = get_unspill_code(PhysReg, SpillPos),
	  allocate_uses(Temps, NewAllocation, NewFree , SpillAllocation, 
			[{mk_reg(Temp), mk_reg(PhysReg)}| Substs],
			NewSpillCode ++ Code)
      end
  end;
allocate_uses([],Allocation, Free, SpillAllocation, Substs, Code) ->
  {Substs, Allocation, Free, Code}.
      
get_unspill_code(PhysReg, Pos) ->
  TempReg = mk_reg(hipe_sparc_registers:temp0()),

  [hipe_sparc:load_create(mk_reg(PhysReg), uw, TempReg,
			  hipe_sparc:mk_imm(0), [{reg,unspill}]),
   hipe_sparc:load_address_create(TempReg, Pos, constant, [])].

      
get_spill_code(PhysReg, Pos) ->
  TempReg = mk_reg(hipe_sparc_registers:temp0()),
  [hipe_sparc:store_create(TempReg, hipe_sparc:mk_imm(0), uw, hipe_sparc:mk_reg(PhysReg), [{reg,spill}]),
   hipe_sparc:load_address_create(TempReg, Pos, constant, [])].

  




%% ------------------------------------------------------------
%% Spillallocation structure.


empty_allocation() ->
  {0,[]}.
no_spills({N,_}) ->
  N.

spill_allocated(Temp, {Last, SpillAllocation}) ->
  case lists:keysearch(Temp, 1, SpillAllocation) of
    {value, {_,Pos}} ->
      Pos;
    _ -> false
  end.

allocate_new(Temp, {Last, SpillAllocation}, ConstTab) ->
  {NewConstTab, SpillArea} = 
    hipe_consttab:insert_block(ConstTab, 4 , word, [0]),

  {SpillArea, {Last + 1, [{Temp, SpillArea} | SpillAllocation]}, NewConstTab}.

%% ------------------------------------------------------------
allocated(Temp, Allocation) ->
  case lists:keysearch(Temp, 1, Allocation) of
    {value, {_,PhysReg}} ->
      PhysReg;
    _ -> false
  end.

allocate(Temp, Free, Allocation) ->
  PhysReg = hd(Free),
  {PhysReg, 
   [{Temp,PhysReg} | Allocation],
   tl(Free)}.

empty_regalloc() ->  
  [].
%% ------------------------------------------------------------

free_regs() ->
  hipe_sparc_registers:allocatable() -- [hipe_sparc_registers:temp0()].

is_precolored(Temp) -> hipe_sparc_registers:is_precolored(Temp).
init(Sparc, Entries) -> hipe_sparc_cfg:init(Sparc, Entries).
bb(CFG,L) -> hipe_sparc_cfg:bb(CFG,L).


physical_name(R) -> hipe_sparc_registers:physical_name(R).

start(SparcCfg) -> hipe_sparc_cfg:start(SparcCfg).
fail_entrypoints(SparcCfg) -> hipe_sparc_cfg:fail_entrypoints(SparcCfg).

%% allocatable() -> [1,2]. 

function(SparcCfg) -> hipe_sparc_cfg:function(SparcCfg).
var_range(SparcCfg) -> hipe_sparc_cfg:var_range(SparcCfg).
label_range(SparcCfg) -> hipe_sparc_cfg:label_range(SparcCfg).
mk_sparc(Fun, Code, Data, VarRange, LabelRange) ->
  hipe_sparc:mk_sparc(Fun, Code, Data, VarRange, LabelRange).




mk_reg(R) -> hipe_sparc:mk_reg(R).

label_create(L,Info) -> hipe_sparc:label_create(L,Info).
subst_defines(I, Subst) -> hipe_sparc:subst_defines(I, Subst).
subst_uses(I, Subst) -> hipe_sparc:subst_uses(I, Subst).

uses(I)->
  regnames(hipe_sparc:uses(I)).

defines(I) ->
  regnames(hipe_sparc:defines(I)).

regnames([R|Rs]) ->
  [hipe_sparc:reg_nr(R)|regnames(Rs)];
regnames([]) -> [].






succ(SuccMap, L)->
  hipe_sparc_cfg:succ(SuccMap, L).



succ_map(CFG) ->
  hipe_sparc_cfg:succ_map(CFG).

pred_map(CFG) ->
  hipe_sparc_cfg:pred_map(CFG).


