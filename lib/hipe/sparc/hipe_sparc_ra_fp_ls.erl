%% $Id$
%% Linear Scan register allocator for sparc

-module(hipe_sparc_ra_fp_ls).
-export([alloc/4]).
-define(HIPE_INSTRUMENT_COMPILER, true). %% Turn on instrumentation.
%-define(DEBUG,true).
-include("../main/hipe.hrl").

alloc(CFG, Options, NextPos, _TempMap) ->
  SpillLimit = hipe_sparc_specific_fp:number_of_temporaries(CFG),
  alloc1(CFG, NextPos, SpillLimit, Options).

alloc1(SparcCfg, SpillIndex, SpillLimit, Options) ->
  {Map, NewSpillIndex} = 
    hipe_ls_regalloc:regalloc(SparcCfg,
			      hipe_sparc_specific_fp:allocatable(),
			      [hipe_sparc_cfg:start_label(SparcCfg)],
			      SpillIndex,
			      SpillLimit,
			      Options,
			      hipe_sparc_specific_fp),

  FpMap = hipe_temp_map:cols2tuple(Map, hipe_sparc_specific_fp),
  NewCfg = hipe_sparc_ra_post_ls_fp:rewrite(SparcCfg, FpMap, Options),

  %% Consider counting fp spills here.

  {NewCfg, FpMap, NewSpillIndex}.



