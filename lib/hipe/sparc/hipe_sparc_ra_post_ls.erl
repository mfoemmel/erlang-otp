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
%%              $Date: 2005/11/06 13:10:52 $
%%              $Revision: 1.9 $
%% ====================================================================
%%  Exports  :
%%hipe:c({test13,test,0},[late_frames,{regalloc,lfls},pp_sparc]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc_ra_post_ls).
-export([rewrite/3]).

-include("../main/hipe.hrl").
-include("hipe_sparc.hrl").

rewrite(Cfg, TempMap, _Options) ->
  Sparc = hipe_sparc_cfg:linearize(Cfg), 
  NewCode = rewrite_instrs(hipe_sparc:sparc_code(Sparc), TempMap, []),
  NewSparc = hipe_sparc:sparc_code_update(Sparc, NewCode),
  NewSparc2 = hipe_sparc:sparc_var_range_update(NewSparc, {0,hipe_gensym:get_var(sparc)}),
  NewCfg = hipe_sparc_cfg:init(NewSparc2),
  NewCfg.

rewrite_instrs([I|Is], TempMap, AccIs) ->
  NewIs = rewrite_instrs(Is, TempMap, AccIs),
  NewI = rewrite_instr(I, TempMap),
  NewI ++ NewIs;
rewrite_instrs([],_, Is) ->
  Is.  

rewrite_instr(I, TempMap) ->
  case hipe_sparc:is_move(I) of
    true -> [I];
    false ->
      Defs = hipe_sparc:keep_registers(hipe_sparc:defines(I)),
      case all_spills(Defs, TempMap) of
	[] ->
	  rewrite_uses(I, TempMap);
	[Spill] ->
	
	  NewTemps = [{Spill,
		       hipe_sparc:mk_reg(hipe_sparc_registers:temp1())}],
	  NewI = 
	    rewrite_uses(hipe_sparc:subst_defines(I, NewTemps),
			 TempMap),
	  NewI ++ [hipe_sparc:move_create(SpillR,NewTemp) ||
		     {SpillR,NewTemp} <- NewTemps]
      end
  end.

rewrite_uses(I, TempMap) ->
  Uses = hipe_sparc:keep_registers(hipe_sparc:uses(I)),
  case all_spills(Uses, TempMap) of
    [] -> [I];
    [Spill1,Spill2] ->
      
      NewTemps  = [{Spill1,hipe_sparc:mk_reg(hipe_sparc_registers:temp1())},
		   {Spill2,hipe_sparc:mk_reg(hipe_sparc_registers:temp2())}],
      [hipe_sparc:move_create(NewTemp, Spill) ||
	{Spill,NewTemp} <- NewTemps] ++ 
	[remap(I, NewTemps)];

    [Spill1] ->
      NewTemps  = [{Spill1,hipe_sparc:mk_reg(hipe_sparc_registers:temp1())}],
      [hipe_sparc:move_create(NewTemp, Spill) ||
	{Spill,NewTemp} <- NewTemps] ++ 
	[remap(I, NewTemps)];
    _ ->
      case I of
	#store{} ->
	  %% Store can have three spilled temporaries.
	  Src = hipe_sparc:store_src(I),
	  Dst = hipe_sparc:store_dest(I),
	  Offs = hipe_sparc:store_off(I),
	  Tmp1 = hipe_sparc:mk_reg(hipe_sparc_registers:temp1()),
	  Tmp2 = hipe_sparc:mk_reg(hipe_sparc_registers:temp2()),
	  [hipe_sparc:move_create(Tmp1, Dst),
	   hipe_sparc:move_create(Tmp2, Offs),
	   hipe_sparc:alu_create(Tmp1, Tmp1, '+', Tmp2),
	   hipe_sparc:move_create(Tmp2, Src),
	   remap(I, [{Dst, Tmp1}, {Offs, hipe_sparc:mk_imm(0)}, {Src, Tmp2}])];
	_ -> 
	  %% This must be a psudocall... not a problem?
	  [I]
      end
  end.

	   
all_spills(Temps, TempMap) ->
  [T || T <- Temps,
	hipe_temp_map:is_spilled(hipe_sparc:reg_nr(T), TempMap)].

remap(I, Substs) ->
  hipe_sparc:subst(I, Substs).
