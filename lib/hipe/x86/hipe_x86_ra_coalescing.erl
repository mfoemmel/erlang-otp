%%% $Id$
%%% Coalescing register allocator for x86

-module(hipe_x86_ra_coalescing).
-export([ra/3]).
-include("hipe_x86.hrl").
-define(HIPE_INSTRUMENT_COMPILER, true). %% Turn on instrumentation.
-include("../main/hipe.hrl").


ra(X86Defun, SpillIndex, Options) ->
    ?inc_counter(ra_calls_counter,1), 
    SpillLimit = hipe_x86_specific:number_of_temporaries(
		   hipe_x86_cfg:init(X86Defun)),
    alloc(X86Defun, SpillLimit, SpillIndex, Options).

alloc(X86Defun, SpillLimit, SpillIndex, Options) ->
    ?inc_counter(ra_iteration_counter,1), 
    X86Cfg = hipe_x86_cfg:init(X86Defun),
    {Coloring, NewSpillIndex} = 
	hipe_coalescing_regalloc:regalloc(X86Cfg, SpillIndex, 
					  SpillLimit, hipe_x86_specific ),

    {NewX86Defun, _, DontSpill} =
	hipe_x86_ra_postconditions:check_and_rewrite(X86Defun,
						     Coloring,
						     [],
						     Options),
    case DontSpill of
	[] -> %% No new temps, we are done.
	    %% io:format("Coloring: ~w\n",[Coloring]),
	    ?add_spills(Options, NewSpillIndex),
% 	    {NewX86Defun2, _, _} =
% 		hipe_x86_ra_ls_postconditions:check_and_rewrite(NewX86Defun,
% 								Coloring,
% 								[],
% 								Options),
%	    {NewX86Defun2, Coloring};
	    {NewX86Defun, Coloring};
	_ -> 
	    %% Since SpillLimit is used as a low-water-mark
	    %% the list of temps not to spill is uninteresting.
	    alloc( NewX86Defun, SpillLimit, SpillIndex, Options)
    end.


