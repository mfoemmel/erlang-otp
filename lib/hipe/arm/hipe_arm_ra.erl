%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

-module(hipe_arm_ra).
-export([ra/2]).

ra(Defun0, Options) ->
  %% hipe_arm_pp:pp(Defun0),
  {Defun1, Coloring_fp, SpillIndex}
    = case proplists:get_bool(inline_fp, Options) of
%%	true ->
%%	  hipe_regalloc_loop:ra_fp(Defun0, Options,
%%				   hipe_coalescing_regalloc,
%%				   hipe_arm_specific_fp);
	false ->
	  {Defun0,[],0}
      end,
  %% hipe_arm_pp:pp(Defun1),
  {Defun2, Coloring}
    = case proplists:get_value(regalloc, Options, coalescing) of
	coalescing ->
	  ra(Defun1, SpillIndex, Options, hipe_coalescing_regalloc);
%%	optimistic ->
%%	  ra(Defun1, SpillIndex, Options, hipe_optimistic_regalloc);
%%	graph_color ->
%%	  ra(Defun1, SpillIndex, Options, hipe_graph_coloring_regalloc);
%%	linear_scan ->
%%	  hipe_arm_ra_ls:ra(Defun1, SpillIndex, Options);
	naive ->
	  hipe_arm_ra_naive:ra(Defun1, Coloring_fp, Options);
        _ ->
	  exit({unknown_regalloc_compiler_option,
		proplists:get_value(regalloc,Options)})
      end,
  %% hipe_arm_pp:pp(Defun2),
  hipe_arm_ra_finalise:finalise(Defun2, Coloring, Coloring_fp).

ra(Defun, SpillIndex, Options, RegAllocMod) ->
  hipe_regalloc_loop:ra(Defun, SpillIndex, Options, RegAllocMod, hipe_arm_specific).
