%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

-module(hipe_ppc_ra_naive).
-export([ra/3]).

-include("hipe_ppc.hrl").

ra(Defun, _Coloring_fp, _Options) ->	% -> {Defun, Coloring}
  {NewDefun,_DidSpill} =
    hipe_ppc_ra_postconditions:check_and_rewrite2(Defun, [], 'naive'),
  {NewDefun, []}.
