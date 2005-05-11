%%% $Id: hipe_x86_ra_fp_ls.erl,v 1.3 2004/02/09 22:24:25 kostis Exp $
%%% Linear Scan register allocator for x86

-module(hipe_x86_ra_fp_ls).
-export([ra/2]).

%%-define(DEBUG,1).

-include("hipe_x86.hrl").
-define(HIPE_INSTRUMENT_COMPILER, false). %% Turn off instrumentation.
-include("../main/hipe.hrl").

ra(X86Defun, Options) ->
    ?inc_counter(ra_calls_counter,1), 
    CFG = hipe_x86_cfg:init(X86Defun),
    %% ?inc_counter(ra_caller_saves_counter,count_caller_saves(CFG)),
    SpillIndex = 0,
    SpillLimit = hipe_x86_specific_fp:number_of_temporaries(CFG),
    ?inc_counter(bbs_counter, length(hipe_x86_cfg:labels(CFG))),

    ?inc_counter(ra_iteration_counter,1), 
    %% hipe_x86_pp:pp(X86Defun),
    X86Cfg = hipe_x86_cfg:init(X86Defun),

    {Coloring,NewSpillIndex} = 
	hipe_x86_ra_ls:regalloc(X86Cfg, 
				hipe_x86_specific_fp:allocatable(),
				[hipe_x86_cfg:start_label(X86Cfg)],
				SpillIndex, SpillLimit, Options,
				hipe_x86_specific_fp),

    ?add_spills(Options, NewSpillIndex),
    {X86Defun, Coloring, NewSpillIndex}.

