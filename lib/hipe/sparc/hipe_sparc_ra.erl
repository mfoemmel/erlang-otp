%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% Time-stamp: <02/10/09 18:51:08 happi>
%% ====================================================================
%%  Filename : 	hipe_sparc_ra.erl
%%  Module   :	hipe_sparc_ra
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-07-20 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: kostis $
%%              $Date: 2004/08/21 16:39:00 $
%%              $Revision: 1.24 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc_ra).
-export([allocate/3]).
-define(HIPE_INSTRUMENT_COMPILER, true). %% Turn on instrumentation.
-include("../main/hipe.hrl").

allocate(_Fun, SparcCfg, Options) ->
  ?inc_counter(ra_caller_saves_counter,count_caller_saves(SparcCfg)),
  ?opt_start_timer("Regalloc"),
  ?start_ra_instrumentation(Options, 
			    hipe_sparc_size:count_instrs_cfg(SparcCfg),
			    hipe_gensym:get_var(sparc)),

  {NewCfg,TempMap, NextPos}  = 
    case proplists:get_value(regalloc,Options) of
      linear_scan ->
	hipe_sparc_ra_ls:alloc(
	  hipe_sparc_multimove:remove_multimoves(SparcCfg), Options);
      graph_color ->
	hipe_sparc_ra_graph_color:lf_alloc(
	  hipe_sparc_multimove:remove_multimoves(SparcCfg), Options);
      coalescing ->
	hipe_sparc_ra_coalescing:lf_alloc( 
	  hipe_sparc_multimove:remove_multimoves(SparcCfg), Options);
      naive ->
	hipe_sparc_ra_memory:alloc(
	  hipe_sparc_multimove:remove_multimoves(SparcCfg), Options);
%%    cs ->
%%	hipe_sparc_ra_cs:alloc(
%%	  hipe_sparc_multimove:remove_multimoves(SparcCfg), Options);
%%    newls ->
%%	hipe_sparc_ra_new_ls:alloc(
%%	  hipe_sparc_multimove:remove_multimoves(SparcCfg), Options);
      _ -> %% linear_scan made default register allocator
	hipe_sparc_ra_ls:alloc(
	  hipe_sparc_multimove:remove_multimoves(SparcCfg), Options)
    end,
  
  ?opt_stop_timer("Regalloc done"),
  ?stop_ra_instrumentation(Options, 
			    hipe_sparc_size:count_instrs_cfg(NewCfg),
			    hipe_gensym:get_var(NewCfg)),

  {NewCfg2, FpMap, NextPos2}  = 
    case get(hipe_inline_fp) of
      true ->  
	hipe_sparc_ra_fp_ls:alloc(NewCfg, Options, NextPos, TempMap);
      _ -> 
	{NewCfg, [], NextPos}
    end,

  {NewCfg3, NextPos3}  = 
    hipe_sparc_caller_saves:rewrite(
      NewCfg2, TempMap, FpMap, NextPos2, Options),

  {{NewCfg3,TempMap, NextPos3}, FpMap}.


%% This is only a info gathering function used for benchmarking
%% purposes. 
count_caller_saves(CFG) ->
  Liveness = hipe_sparc_liveness:analyze(CFG),
  count_caller_saves(CFG, Liveness, hipe_sparc_specific).

count_caller_saves(CFG, Liveness, T) ->
  %% Count how many temps are live over a call.
  length(
    %% Fold the count for each basic block.
    lists:foldr(
      %% For each BB, take the set of CallerSaves from previous BBs
      fun(L, CallerSaves) -> 
	  %% Just keep temps that are not precoloured.
	  [ X || 
	    %% Get the set of caller saves (from {Liveness, CS}).
	    X <- element(2,
	      %% Fold each instruction in the BB (backwards).
	      lists:foldr(
		%% For each instruction
		fun(I,{LiveOut,CS}) ->
		    %% Calculate live-in
		    UsesSet = ordsets:from_list(uses(I,T)),
		    DefsSet = ordsets:from_list(defines(I,T)),
		    LiveOverI = ordsets:subtract(LiveOut, DefsSet),      
		      NewCS = 
			case hipe_sparc:type(I) of
			  %% If this is a call instruction, keep the CS-temps.
			  call_link ->
			    ordsets:union(CS,LiveOverI);
			  _ -> CS
			end,
		    NewLiveOut = 
		      ordsets:union(LiveOverI, UsesSet),
		    {NewLiveOut,NewCS}
		end,
		%% Start with live out of the BB
		{ordsets:from_list(liveout(Liveness,L,T)),
		 CallerSaves},
		%% Get the instructions in the BB.
		hipe_bb:code(T:bb(CFG,L)))),
	    %% Filter
	    not T:is_precoloured(X)]	
      end,
      [],
      %% Get BBs
      T:labels(CFG))).

liveout(Liveness,L, Target)->
  regnames(Target:liveout(Liveness,L), Target).

uses(I, Target)->
  regnames(Target:uses(I), Target).

defines(I, Target) ->
  regnames(Target:defines(I), Target).

regnames(Regs2, Target) ->
  Regs = 
    case Target of
      hipe_sparc_specific ->
	hipe_sparc:keep_registers(Regs2);
      %% hipe_sparc_specific_fp ->
      %%  hipe_sparc:keep_fp_registers(Regs2);
      _ ->
	Regs2
    end,
  [Target:reg_nr(X) || X <- Regs].
