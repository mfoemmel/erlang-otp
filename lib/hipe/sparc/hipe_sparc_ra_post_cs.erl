%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%%  Filename : 	hipe_sparc_ra_postconditions.erl
%%  Module   :	hipe_sparc_ra_postconditions
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-11-01 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: kostis $
%%              $Date: 2006/04/13 13:04:15 $
%%              $Revision: 1.7 $
%% ====================================================================
%%  Exports  :
%%hipe:c({test13,test,0},[late_frames,{regalloc,lfls},pp_sparc]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc_ra_post_cs).
-export([rewrite/4]).
-include("../main/hipe.hrl").


rewrite(Cfg, TempMaps, DontSpill, _Options) ->
  Sparc = hipe_sparc_cfg:linearize(Cfg), 
  {NewCode, NewDontSpill} =
    rewrite_instrs(hipe_sparc:sparc_code(Sparc), 
		   TempMaps, [], DontSpill,[]),
  
  NewSparc = hipe_sparc:sparc_code_update(Sparc, NewCode),
  NewSparc2 = hipe_sparc:sparc_var_range_update(NewSparc, {0,hipe_gensym:get_var(sparc)}),
  NewCfg = hipe_sparc_cfg:init(NewSparc2),

  {NewCfg, NewDontSpill}.

rewrite_instrs([I|Is], TempMaps, AccIs, DontSpill, CurrentMap) ->
  NewMap = 
    case hipe_sparc:is_label(I) of
      true ->
	hipe_vectors:get(TempMaps,hipe_sparc:label_name(I));
      false ->
	CurrentMap
    end,
  {NewIs, NewDontSpill} = rewrite_instrs(Is, TempMaps, AccIs,
					 DontSpill, NewMap),
  {NewI, FinalDontSpill} =rewrite_instr(I, CurrentMap, NewDontSpill),
  {NewI ++ NewIs, FinalDontSpill};
rewrite_instrs([],_, Is, DontSpill,_) ->
  {Is, DontSpill}.  

rewrite_instr(I, TempMap, DontSpill) ->
  case hipe_sparc:is_move(I) of
    true ->
      {[I],DontSpill};
    false ->
      %% io:format("\n\n~w\n",[I]),  
      Defs = hipe_sparc:defines(I),
      case all_spills(Defs, TempMap) of
	[] ->
	  rewrite_uses(I, TempMap, DontSpill);
	[Spill] ->
	  NewTemps = [{Spill,hipe_sparc:mk_reg(hipe_sparc_registers:temp1())}],
	  {NewI,_NewDontSpill} =
		  rewrite_uses(hipe_sparc:subst_defines(I, NewTemps),
			       TempMap, DontSpill),
	  %%      {NewI ++ [hipe_sparc:pseudo_spill_create(NewTemp,
	  %%					       hipe_temp_map:find(
	  %%						 hipe_sparc:reg_nr(Spill),TempMap)) ||
	  {NewI ++ [hipe_sparc:move_create(SpillR,NewTemp) ||
		     {SpillR,NewTemp} <- NewTemps],
	   DontSpill}
      end
  end.

rewrite_uses(I, TempMap, DontSpill) ->
  Uses = hipe_sparc:uses(I),
  case all_spills(Uses, TempMap) of
    [] -> {[I], DontSpill};
    [Spill1,Spill2] ->
      
      NewTemps  = [{Spill1,hipe_sparc:mk_reg(hipe_sparc_registers:temp1())},
		   {Spill2,hipe_sparc:mk_reg(hipe_sparc_registers:temp2())}],
      { [hipe_sparc:move_create(NewTemp, Spill) ||
	  {Spill,NewTemp} <- NewTemps] ++ 
	[remap(I, NewTemps)],
	DontSpill};
    [Spill1] ->
      
      NewTemps  = [{Spill1,hipe_sparc:mk_reg(hipe_sparc_registers:temp1())}],
      { [hipe_sparc:move_create(NewTemp, Spill) ||
	  {Spill,NewTemp} <- NewTemps] ++ 
	[remap(I, NewTemps)],
	DontSpill};
    _ -> %% This must be a psudocall... not a problem?
      {[I], DontSpill}
  
  end.

	   
all_spills(Temps, TempMap) ->
 %% io:format("~w\n~w\n",[Temps,TempMap]),
  [T || T <- Temps,
	hipe_temp_map:is_spilled(hipe_sparc:reg_nr(T), TempMap)].


remap(I, Substs) ->
  hipe_sparc:subst(I, Substs).
