%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

-module(hipe_amd64_ra).
-export([ra/2]).

%%-define(HIPE_INSTRUMENT_COMPILER, true). %% Turn on instrumentation.
-include("../main/hipe.hrl").

ra(Defun0, Options) ->
  %% hipe_amd64_pp:pp(Defun0),
  {Defun1, Coloring_fp, SpillIndex} =
    case proplists:get_bool(inline_fp, Options) and
      (proplists:get_value(regalloc, Options, coalescing) =/= naive)
      of
      true ->
	case proplists:get_bool(x87, Options) of
	  true ->
	    hipe_amd64_ra_x87_ls:ra(Defun0, Options);
	  false ->
	    hipe_regalloc_loop:ra_fp(Defun0, Options,
				     hipe_coalescing_regalloc,
				     hipe_amd64_specific_sse2)
	end;
      false ->
	{Defun0,[],0}
    end,
  %% hipe_amd64_pp:pp(Defun1),
  ?start_ra_instrumentation(Options,
			    length(hipe_x86:defun_code(Defun1)),
			    element(2,hipe_x86:defun_var_range(Defun1))),
  {Defun2, Coloring}
    = case proplists:get_value(regalloc, Options, coalescing) of
	coalescing ->
	  ra(Defun1, SpillIndex, Options, hipe_coalescing_regalloc);
	graph_color ->
	  ra(Defun1, SpillIndex, Options, hipe_graph_coloring_regalloc);
 	linear_scan ->
 	  hipe_amd64_ra_ls:ra(Defun1, SpillIndex, Options);
	naive ->
	  hipe_amd64_ra_naive:ra(Defun1, Coloring_fp, Options);
        _ ->
	  exit({unknown_regalloc_compiler_option,
		proplists:get_value(regalloc,Options)})
      end,
  ?stop_ra_instrumentation(Options,
			   length(hipe_x86:defun_code(Defun2)),
			   element(2,hipe_x86:defun_var_range(Defun2))),
  %% hipe_amd64_pp:pp(Defun2),
  hipe_amd64_ra_finalise:finalise(Defun2, Coloring, Coloring_fp, Options).

ra(Defun, SpillIndex, Options, RegAllocMod) ->
  hipe_regalloc_loop:ra(Defun, SpillIndex, Options, RegAllocMod, hipe_amd64_specific).
