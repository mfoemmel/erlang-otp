%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

-module(hipe_ppc_ra).
-export([ra/2]).

ra(Defun0, Options) ->
  %% hipe_ppc_pp:pp(Defun0),
  {Defun1, Coloring_fp, SpillIndex}
    = case proplists:get_bool(inline_fp, Options) of
	true ->
	  hipe_regalloc_loop:ra_fp(Defun0, Options,
				   hipe_coalescing_regalloc,
				   hipe_ppc_specific_fp);
	false ->
	  {Defun0,[],0}
      end,
  %% hipe_ppc_pp:pp(Defun1),
  {Defun2, Coloring}
    = case proplists:get_value(regalloc, Options, coalescing) of
	coalescing ->
	  ra(Defun1, SpillIndex, Options, hipe_coalescing_regalloc);
	graph_color ->
	  ra(Defun1, SpillIndex, Options, hipe_graph_coloring_regalloc);
	linear_scan ->
	  io:format("{regalloc,linear_scan} NYI; using coalescing\n"),
	  ra(Defun1, SpillIndex, Options, hipe_coalescing_regalloc);
%%%	  hipe_ppc_ra_ls:ra(Defun1, SpillIndex, Options);
	naive ->
	  hipe_ppc_ra_dummy:ra(Defun1, Coloring_fp, Options);
        _ ->
	  exit({unknown_regalloc_compiler_option,
		proplists:get_value(regalloc,Options)})
      end,
  %% hipe_ppc_pp:pp(Defun2),
  hipe_ppc_ra_finalise:finalise(Defun2, Coloring, Coloring_fp).

ra(Defun, SpillIndex, Options, RegAllocMod) ->
  hipe_regalloc_loop:ra(Defun, SpillIndex, Options, RegAllocMod, hipe_ppc_specific).
