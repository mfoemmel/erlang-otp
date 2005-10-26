%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2000 by Erik Johansson.  All Rights Reserved 
%% ====================================================================
%%  Filename : 	sparc_ra_naive.erl
%%  Purpose  :  Implements a naive register allocation to be used as
%%              baseline for benchmarking register allocators.
%%  Notes    : 
%%  History  :	* 2000-08-21 Erik Johansson (happi@csd.uu.se): 
%%                Created.
%%              * 2002-05-09 Niklas Andersson, Andreas Lundin
%%                Added support for stack frame minimization
%%  CVS      :
%%              $Author: kostis $
%%              $Date: 2005/08/29 14:34:43 $
%%              $Revision: 1.2 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc_ra_naive).
-export([alloc/2, split_constants/1]).

-define(HIPE_INSTRUMENT_COMPILER, true). %% Turn on instrumentation.

-include("../main/hipe.hrl").

alloc(SparcCfg, Options) ->
  {Map, _SpillPos} = spill(SparcCfg),
  %% io:format("ListMap:~w\n",[Map]),
  %% hipe_sparc_cfg:pp(SparcCfg),
  TempMap = hipe_temp_map:cols2tuple(Map, hipe_sparc_specific),
  %% io:format("Map:~w\n",[TempMap]),

  %% Code to minimize stack size by allocation of temps to spill positions
  ?opt_start_timer("Minimize"),
  {TempMap2, NewSpillPos} = 
    hipe_spillmin:stackalloc(SparcCfg, [], 0, Options, 
			     hipe_sparc_specific, TempMap),
  TempMap3 = hipe_spillmin:mapmerge(hipe_temp_map:to_substlist(TempMap), 
				    TempMap2),
  TempMap4 = hipe_temp_map:cols2tuple(TempMap3, hipe_sparc_specific),
  ?opt_stop_timer("Minimize"),    

  NewCfg = hipe_sparc_ra_post_ls:rewrite(SparcCfg, TempMap4, Options),
%%  hipe_sparc_cfg:pp(NewCfg),
%%  {SparcCfg2, NextPos} = hipe_sparc_caller_saves:rewrite(
%%			   NewCfg, TempMap,  
%%			   SpillPos+1,
%%			   Options),
  ?add_spills(Options, NewSpillPos+1),
 
  {NewCfg, TempMap4, NewSpillPos+1}.


spill(Cfg) ->
  Last = hipe_gensym:get_var(sparc),
  Map = hipe_vectors:new(Last+1,undef),
  Code = hipe_sparc:sparc_code(hipe_sparc_cfg:linearize(Cfg)),
  {NewMap, SpillPos} = traverse(Code,Map),
  %% Unconditionally include the last precoloured register, forcing
  %% hipe_temp_map:cols2tuple/2 to cover all precoloured registers.
  %% Liveness includes the global precoloured registers. Pseudos have
  %% higher numbers than precoloured registers, so this is not a problem
  %% except in really simple intermediate code which has no pseudos.
  LastPrecoloured = hipe_sparc_registers:first_virtual() - 1,
  {[{LastPrecoloured,{reg,hipe_sparc_registers:physical_name(LastPrecoloured)}} |
    [{T-1,Pos} || {T,Pos} <- hipe_vectors:list(NewMap), Pos =/= undef]],
   SpillPos}.

traverse(Code, Map) ->
  lists:foldl(fun map/2, {Map,0}, Code).

map(I,Map) ->
  {Def,Use} = hipe_sparc:def_use(I),
  lists:foldl(fun map_temp/2, Map, Def++Use).
   
map_temp(T,{Map,SpillPos}) ->
  RealTemp = hipe_sparc:reg_nr(T),
  Temp = RealTemp+1,
  case hipe_vectors:get(Map,Temp) of
    undef ->
      case hipe_sparc_registers:is_precoloured(RealTemp) of
	true ->
	  {hipe_vectors:set(Map, Temp,
			    {reg, hipe_sparc_registers:physical_name(RealTemp)}),
	  SpillPos};
	false ->
	  {hipe_vectors:set(Map, Temp, {spill,SpillPos}),
	   SpillPos+1}
      end;
    _ ->
      {Map,SpillPos}
  end.


%% Fixes big immediates which can be introduced as stack offsets due
%% to excessive spilling.

split_constants(CFG) ->
  Labels = hipe_sparc_cfg:labels(CFG),
  split_bbs(Labels, CFG).

split_bbs([], CFG) ->
  CFG;
split_bbs([Lbl|Lbls], CFG) ->
  BB = hipe_sparc_cfg:bb(CFG, Lbl),
  Code = hipe_bb:code(BB),
  case split_instrs(Code, [], unchanged) of
    unchanged ->
      split_bbs(Lbls, CFG);
    NewCode ->
      NewCFG = hipe_sparc_cfg:bb_add(CFG, Lbl, hipe_bb:code_update(BB, NewCode)),
      split_bbs(Lbls, NewCFG)
  end.

split_instrs([], _RevCode, unchanged) ->
  unchanged;
split_instrs([], RevCode, changed) ->
  lists:reverse(RevCode);
split_instrs([I|Is], RevCode, Status) ->
  case split_instr(I) of
    unchanged ->
      split_instrs(Is, [I|RevCode], Status);
    NewCode ->
      split_instrs(Is, NewCode++RevCode, changed)
  end.

split_instr(I) ->
  Uses = hipe_sparc:imm_uses(I),
  case big_constants(Uses) of
    {[], []} -> unchanged;
    {Code, Subst} -> [hipe_sparc:subst(I, Subst) | Code]
  end.

big_constants([]) ->
  {[], []};
big_constants([V|Vs]) ->
  C = hipe_sparc:imm_value(V),
  %% Since this is the naive allocator any allocatable register should
  %% do as a temp reg.
  TempReg = hipe_sparc:mk_reg(hd(hipe_sparc_registers:allocatable())), 
  case is_big(C) of
    true ->
      Low = low10(C),
      Code =
	if Low =:= 0 ->
	    [hipe_sparc:sethi_create(TempReg, hipe_sparc:mk_imm(high22(C)))];
	   true ->	     
	    [hipe_sparc:alu_create(TempReg, TempReg, 'or', hipe_sparc:mk_imm(Low)),
	     hipe_sparc:sethi_create(TempReg, hipe_sparc:mk_imm(high22(C)))]
	end,
      {MoreCode, MoreSubst} = big_constants(Vs),
      {Code++MoreCode, [{V, TempReg} | MoreSubst]};
    false ->
      big_constants(Vs)
  end.

is_big(X) ->
  if X > 16#fff ->
      true;
     X < -4096 -> 
      true;
     true ->
      false
  end.

high22(X) -> X bsr 10.
low10(X) -> X band 16#3ff.
