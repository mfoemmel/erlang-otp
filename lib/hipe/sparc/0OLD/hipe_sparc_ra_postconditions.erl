%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%%  Filename : 	hipe_sparc_ra_postconditions.erl
%%  Module   :	hipe_sparc_ra_postconditions
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-11-01 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: mikpe $
%%              $Date: 2007/12/18 09:18:22 $
%%              $Revision: 1.1 $
%% ====================================================================
%%  Exports  :
%%hipe:c({test13,test,0},[late_frames,{regalloc,lfls},pp_sparc]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc_ra_postconditions).
-export([rewrite/4]).
-include("../main/hipe.hrl").


rewrite(Cfg, TempMap, DontSpill, _Options) ->
  Sparc = hipe_sparc_cfg:linearize(Cfg), 
  {NewCode, NewDontSpill} =
    rewrite_instrs(hipe_sparc:sparc_code(Sparc), 
		   TempMap, [], DontSpill),
  
  NewSparc = hipe_sparc:sparc_code_update(Sparc, NewCode),
  NewSparc2 = hipe_sparc:sparc_var_range_update(NewSparc, {0,hipe_gensym:get_var(sparc)}),
  NewCfg = hipe_sparc_cfg:init(NewSparc2),

  {NewCfg, NewDontSpill}.

rewrite_instrs([I|Is], TempMap, AccIs, DontSpill) ->
  {NewIs, NewDontSpill} = rewrite_instrs(Is, TempMap, AccIs, DontSpill),
  {NewI, FinalDontSpill} =rewrite_instr(I, TempMap, NewDontSpill),
  {NewI ++ NewIs, FinalDontSpill};
rewrite_instrs([],_, Is, DontSpill) ->
  {Is, DontSpill}.  

rewrite_instr(I, TempMap, DontSpill) ->
  case hipe_sparc:is_move(I) of
    true ->
      {[I],DontSpill};
    false ->
      %% io:format("\n\n~w\n",[I]),  
      Defs = hipe_sparc:keep_registers(hipe_sparc:defines(I)),
      case all_spills(Defs, TempMap) of
	[] ->
	  rewrite_uses(I, TempMap, DontSpill);
	Spills ->
	
	  NewTemps  = [{Spill,hipe_sparc:mk_new_reg()} || Spill <- Spills],
	  {NewI, NewDontSpill} = rewrite_uses(hipe_sparc:subst_defines(I, NewTemps),
					      TempMap, DontSpill),
	  %%      {NewI ++ [hipe_sparc:pseudo_spill_create(NewTemp,
	  %%					       hipe_temp_map:find(
	  %%						 hipe_sparc:reg_nr(Spill),TempMap)) ||
	  {NewI ++ [hipe_sparc:move_create(Spill,NewTemp) ||
		     {Spill,NewTemp} <- NewTemps],
	   [ T || {_,T} <- NewTemps] ++ NewDontSpill}
      end
  end.

rewrite_uses(I, TempMap, DontSpill) ->
  Uses = hipe_sparc:keep_registers(hipe_sparc:uses(I)),
  case all_spills(Uses, TempMap) of
    [] -> {[I], DontSpill};
    Spills ->
      
      NewTemps  = [{Spill,hipe_sparc:mk_new_reg()} || Spill <- Spills],
%%      { [hipe_sparc:pseudo_unspill_create(NewTemp, 
%%					  hipe_temp_map:find(hipe_sparc:reg_nr(Spill),TempMap)
%%					 ) ||
      { [hipe_sparc:move_create(NewTemp, Spill) ||
	  {Spill,NewTemp} <- NewTemps] ++ 
	 [remap(I, NewTemps)],
      [ element(2,T) || T <- NewTemps] ++DontSpill}
  end.

	   
all_spills(Temps, TempMap) ->
 %% io:format("~w\n~w\n",[Temps,TempMap]),
  [T || T <- Temps,
	hipe_temp_map:is_spilled(hipe_sparc:reg_nr(T), TempMap)].


remap(I, Substs) ->
  hipe_sparc:subst(I, Substs).
