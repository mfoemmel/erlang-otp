%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% ====================================================================
%%  Filename : 	hipe_x86_ra_graph_color.erl
%%  Module   :	hipe_x86_ra_graph_color
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-07-26 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: happi $
%%              $Date: 2001/09/04 15:01:29 $
%%              $Revision: 1.6 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_x86_ra_graph_color).
-export([ra/2]).
-include("hipe_x86.hrl").
-define(HIPE_INSTRUMENT_COMPILER, true). %% Turn on instrumentation.
-include("../main/hipe.hrl").

ra(X86Defun,Options) ->
  ?inc_counter(ra_calls_counter,1), 
  SpillLimit =
    hipe_x86_specific:number_of_temporaries(hipe_x86_cfg:init(X86Defun)),
  alloc(X86Defun, 0, SpillLimit, Options).

alloc(X86Defun, SpillIndex, SpillLimit, Options) ->
  ?inc_counter(ra_iteration_counter,1), 
  X86Cfg = hipe_x86_cfg:init(X86Defun),
  {Coloring, NewSpillIndex} = 
    hipe_graph_coloring_regalloc:regalloc(
      X86Cfg, SpillIndex, SpillLimit, hipe_x86_specific),

  {NewX86Defun, _, DontSpill} =
    hipe_x86_ra_postconditions:check_and_rewrite(
      X86Defun, Coloring, [], Options),

  case DontSpill of
    [] -> %% No new temps, we are done.
      ?add_spills(Options, NewSpillIndex),
      {NewX86Defun, Coloring};
    _ -> 
      %% Since SpillLimit is used as a low-water-mark
      %% the list of temps not to spill is uninteresting.
      alloc( NewX86Defun, 0, SpillLimit, Options)
  end.



