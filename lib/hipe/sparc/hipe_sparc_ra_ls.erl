%%% $Id$
%%% Linear Scan register allocator for sparc
-module(hipe_sparc_ra_ls).
-export([alloc/2]).
-define(HIPE_INSTRUMENT_COMPILER, true). %% Turn on instrumentation.
%-define(DEBUG,true).
-include("../main/hipe.hrl").

alloc(CFG, Options) ->
  ?inc_counter(ra_calls_counter,1), 
  SpillLimit = hipe_sparc_specific:number_of_temporaries(
		 CFG),
  ?inc_counter(bbs_counter, length(hipe_sparc_cfg:labels(CFG))),
  alloc(CFG, 0, SpillLimit, Options).

alloc(SparcCfg, SpillIndex, SpillLimit, Options) ->
  ?inc_counter(ra_iteration_counter,1), 
  {Map, NewSpillIndex} = 
    hipe_ls_regalloc:regalloc(
      SparcCfg,
      hipe_sparc_registers:allocatable() -- 
      [hipe_sparc_registers:temp1(),hipe_sparc_registers:temp2()],
      [hipe_sparc_cfg:start(SparcCfg)],
      SpillIndex,
      SpillLimit,
      Options,
      hipe_sparc_specific),

  TempMap = hipe_temp_map:cols2tuple(Map, hipe_sparc_specific),
  NewCfg =
    hipe_sparc_ra_post_ls:rewrite(
      SparcCfg, TempMap, Options),

  ?add_spills(Options, NewSpillIndex),

  {NewCfg, TempMap, NewSpillIndex}.

