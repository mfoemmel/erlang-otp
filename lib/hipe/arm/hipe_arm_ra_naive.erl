%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

-module(hipe_arm_ra_naive).
-export([ra/3]).

-include("hipe_arm.hrl").

ra(Defun, _Coloring_fp, _Options) ->	% -> {Defun, Coloring}
  {NewDefun,_DidSpill} =
    hipe_arm_ra_postconditions:check_and_rewrite2(Defun, [], 'naive'),
  {NewDefun, []}.
