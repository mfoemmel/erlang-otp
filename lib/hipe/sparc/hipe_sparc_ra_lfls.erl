%%% $Id$
%%% Linear Scan register allocator for x86

%%
%%  hipe:c({len,len,2},[late_frames,{regalloc,lfls},pp_sparc]).
%%  hipe:c( {random_test,mergel,2},[pp_sparc,late_frames,{regalloc,lfls},pp_rtl]).
%%  hipe:c({barnes2,resolve_body_conflict,9},[time,o2,late_frames,{regalloc,lfls},verbose,pp_rtl]).

-module(hipe_sparc_ra_lfls).
-export([alloc/2]).
-define(HIPE_INSTRUMENT_COMPILER, true). %% Turn on instrumentation.
%-define(DEBUG,true).
-include("../main/hipe.hrl").

alloc(CFG, Options) ->
  ?inc_counter(ra_calls_counter,1), 

  SpillLimit = 
    hipe_sparc_specific:number_of_temporaries(
      CFG),
  
  ?inc_counter(bbs_counter, length(hipe_sparc_cfg:labels(CFG))),
  alloc(CFG, 0, SpillLimit, Options).


alloc(SparcCfg, SpillIndex, SpillLimit, Options) ->
  ?inc_counter(ra_iteration_counter,1), 
  %% hipe_sparc_cfg:pp(SparcCfg),

  {Map, NewSpillIndex} = 
    hipe_ls_regalloc:regalloc(
      SparcCfg,
      hipe_sparc_registers:allocatable() -- 
      [hipe_sparc_registers:temp1(),hipe_sparc_registers:temp2()],
%%       hipe_sparc_registers:temp3()],
      [hipe_sparc_cfg:start(SparcCfg)],
      SpillIndex,
      SpillLimit,
      Options,
      hipe_sparc_specific),
%% io:format("Spill ~w\n",[NewSpillIndex]),
  TempMap = hipe_temp_map:cols2tuple(Map, hipe_sparc_specific),

  {NewCfg, DontSpill} =
    hipe_sparc_ra_post_ls:rewrite(
      SparcCfg, TempMap, [], Options),

  case DontSpill of
    [] -> 
      ?add_spills(Options, NewSpillIndex),
      %%      {SparcCfg2, NextPos} = hipe_sparc_caller_saves:rewrite(
      %%		    NewCfg, TempMap, NewSpillIndex, Options),
      %%   io:format("Spill+caller_saves ~w\n",[NextPos]),
      %%{SparcCfg2, TempMap, NextPos};
      {NewCfg, TempMap, NewSpillIndex};
    _ -> 
      %% Since SpillLimit is used as a low-water-mark
      %% the list of temps not to spill is uninteresting.
      exit(i_did_not_want_this)
  end.

