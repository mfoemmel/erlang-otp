%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% Time-stamp: <01/08/05 18:05:48 happi>
%% ====================================================================
%%  Filename : 	hipe_sparc_ra.erl
%%  Module   :	hipe_sparc_ra
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-07-20 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: kostis $
%%              $Date: 2001/08/08 14:53:19 $
%%              $Revision: 1.5 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc_ra).
-export([allocate/3]).
-define(HIPE_INSTRUMENT_COMPILER, true). %% Turn on instrumentation.
-include("../main/hipe.hrl").

allocate(Fun, SparcCfg, Options) ->
  ?when_option(time, Options, ?start_timer("Regalloc")),
  ?start_ra_instrumentation(Options, 
			    hipe_sparc_size:count_instrs_cfg(SparcCfg),
			    element(2,hipe_sparc_cfg:var_range(SparcCfg))),
  
  NewCfg = 
    case property_lists:get_value(regalloc,Options) of
      linear_scan ->
	hipe_sparc_ra_ls:alloc(SparcCfg, Options);
      graph_color ->
	hipe_sparc_ra_graph_color:alloc(SparcCfg, Options);
      optimistic_ls ->
	hipe_sparc_ra_ols:alloc(SparcCfg, Options);
      coalescing ->
	hipe_sparc_ra_coalescing:alloc( 
	  hipe_sparc_multimove:remove_multimoves(SparcCfg), Options);
      naive ->
	hipe_sparc_ra_memory:alloc(SparcCfg, Options);
      _ -> %% linear_scan made default register allocator
	hipe_sparc_ra_ls:alloc(SparcCfg, Options)
    end,

  ?when_option(time, Options, ?stop_timer("Regalloc done")),
  ?stop_ra_instrumentation(Options, 
			    hipe_sparc_size:count_instrs_cfg(NewCfg),
			    element(2,hipe_sparc_cfg:var_range(NewCfg))),

  NewCfg.
