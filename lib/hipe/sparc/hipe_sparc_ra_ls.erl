%%% $Id$
%%% Linear Scan register allocator for sparc
-module(hipe_sparc_ra_ls).
-export([alloc/2]).
-define(HIPE_INSTRUMENT_COMPILER, true). %% Turn on instrumentation.
%-define(DEBUG,true).
-include("../main/hipe.hrl").

-define(no_temps,hipe_sparc_specific:number_of_temporaries).

alloc(CFG, Options) ->
  ?inc_counter(ra_calls_counter,1), 
  SpillLimit = ?no_temps(CFG),
  ?inc_counter(bbs_counter, length(hipe_sparc_cfg:labels(CFG))),
  alloc(CFG, SpillLimit, Options).

alloc(SparcCfg, SpillLimit, Options) ->
  ?inc_counter(ra_iteration_counter,1), 
  ?opt_start_timer("Alloc"),  
  {Map,_NewSpillIndex} = 
    hipe_ls_regalloc:regalloc(
      SparcCfg,
      hipe_sparc_registers:allocatable() -- 
      %% Save temp1 and temp2 for spill load & stores
      [hipe_sparc_registers:temp1(),hipe_sparc_registers:temp2()],
      [hipe_sparc_cfg:start_label(SparcCfg)],
      0,
      SpillLimit,
      Options,
      hipe_sparc_specific),
  ?opt_stop_timer("Alloc Done"),
  TempMap = hipe_temp_map:cols2tuple(Map, hipe_sparc_specific),

  %% Code to minimize stack size by allocation of temps to spillpositions
  ?opt_start_timer("Minimize"),  
%  {TempMap2, NewSpillIndex2} = {TempMap,NewSpillIndex},
  {TempMap2, NewSpillIndex2} = 
    hipe_spill_minimize:stackalloc(
      SparcCfg, [], 0, Options, 
      hipe_sparc_specific, TempMap),

  TempMap3 = hipe_spill_minimize:mapmerge(
	       hipe_temp_map:to_substlist(TempMap), 
	       TempMap2),

  ?opt_stop_timer("Minimize Done"),
  TempMap4 = hipe_temp_map:cols2tuple( TempMap3, hipe_sparc_specific),

  ?opt_start_timer("Rewrite"),
  NewCfg =
    hipe_sparc_ra_post_ls:rewrite(
      SparcCfg, TempMap4, Options),
  ?opt_stop_timer("Rewrite Done"),
  ?add_spills(Options, NewSpillIndex2),

  {NewCfg, TempMap4, NewSpillIndex2}.



