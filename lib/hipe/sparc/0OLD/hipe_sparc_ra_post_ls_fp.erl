%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%%  Filename : 	hipe_sparc_ra_post_ls_fp.erl
%%  Module   :	hipe_sparc_ra_post_ls_fp
%%  Purpose  :  
%%  Notes    : 
%%  CVS      :
%%              $Author: mikpe $
%%              $Date: 2007/12/18 09:18:22 $
%%              $Revision: 1.1 $
%% ====================================================================
%%  Exports  :
%%hipe:c({test13,test,0},[late_frames,{regalloc,lfls},pp_sparc]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc_ra_post_ls_fp).
-export([rewrite/3]).
-include("../main/hipe.hrl").

rewrite(Cfg, FpMap, _Options) ->
  Sparc = hipe_sparc_cfg:linearize(Cfg), 
  NewCode = rewrite_instrs(hipe_sparc:sparc_code(Sparc), FpMap, []),
  
  NewSparc = hipe_sparc:sparc_code_update(Sparc, NewCode),
  NewSparc2 = hipe_sparc:sparc_var_range_update(NewSparc, {0,hipe_gensym:get_var(sparc)}),
  NewCfg = hipe_sparc_cfg:init(NewSparc2),

  NewCfg.

rewrite_instrs([I|Is], FpMap, AccIs) ->
  NewIs = rewrite_instrs(Is, FpMap, AccIs),
  NewI =rewrite_instr(I, FpMap),
  NewI ++ NewIs;
rewrite_instrs([],_, Is) ->
  Is.  

rewrite_instr(I, FpMap) ->
  case hipe_sparc:is_fmove(I) of
    true -> [I];
    false ->
      Defs = hipe_sparc:fp_reg_defines(I),
      case all_spills(Defs, FpMap) of
	[] ->
	  rewrite_uses(I, FpMap);
	[Spill] ->
	  NewTemps = [{Spill,hipe_sparc:mk_fpreg(0)}],
	  NewI = 
	    rewrite_uses(hipe_sparc:subst_defines(I, NewTemps),
			 FpMap),
	  
	  NewIs = NewI ++ [hipe_sparc:fmove_create(SpillR,NewTemp) ||
		    {SpillR,NewTemp} <- NewTemps],
	  NewIs
      end
  end.

rewrite_uses(I, FpMap) ->
  Uses = hipe_sparc:fp_reg_uses(I),
  case all_spills(Uses, FpMap) of
    [] -> [I];
    [Spill1,Spill2] ->
      
      NewTemps  = [{Spill1,hipe_sparc:mk_fpreg(0)},
		   {Spill2,hipe_sparc:mk_fpreg(2)}],
      [hipe_sparc:fmove_create(NewTemp, Spill) ||
	{Spill,NewTemp} <- NewTemps] ++ 
	[remap(I, NewTemps)];

    [Spill1] ->
      NewTemps  = [{Spill1,hipe_sparc:mk_fpreg(0)}],
      [hipe_sparc:fmove_create(NewTemp, Spill) ||
	{Spill,NewTemp} <- NewTemps] ++ 
	[remap(I, NewTemps)];
    _ -> %% This must be a pseudocall... not a problem?
      [I]
  end.

	   
all_spills(Temps, FpMap) ->
  [T || T <- Temps,
	hipe_temp_map:is_spilled(hipe_sparc:fpreg_nr(T), FpMap)].

remap(I, Substs) ->
  hipe_sparc:subst(I, Substs).
