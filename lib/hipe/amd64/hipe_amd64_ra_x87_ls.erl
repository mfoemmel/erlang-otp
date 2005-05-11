%%% $Id: hipe_amd64_ra_x87_ls.erl,v 1.3 2005/03/29 11:46:30 mikpe Exp $
%%% Linear Scan register allocator for amd64/x87

-module(hipe_amd64_ra_x87_ls).
-export([ra/2]).

%%-define(DEBUG,1).

-define(HIPE_INSTRUMENT_COMPILER, false). %% Turn off instrumentation.
-include("../main/hipe.hrl").

ra(Amd64Defun, Options) ->
    ?inc_counter(ra_calls_counter,1), 
    CFG = hipe_x86_cfg:init(Amd64Defun),
    %% ?inc_counter(ra_caller_saves_counter,count_caller_saves(CFG)),
    SpillIndex = 0,
    SpillLimit = hipe_amd64_specific_x87:number_of_temporaries(CFG),
    ?inc_counter(bbs_counter, length(hipe_x86_cfg:labels(CFG))),

    ?inc_counter(ra_iteration_counter,1), 
    %% hipe_amd64_pp:pp(Amd64Defun),
    Amd64Cfg = hipe_x86_cfg:init(Amd64Defun),

    {Coloring,NewSpillIndex} = 
	hipe_amd64_ra_ls:regalloc(Amd64Cfg,
				  hipe_amd64_registers:allocatable_x87(),
				  [hipe_x86_cfg:start_label(Amd64Cfg)],
				  SpillIndex, SpillLimit, Options,
				  hipe_amd64_specific_x87),

    ?add_spills(Options, NewSpillIndex),
    {Amd64Defun, Coloring, NewSpillIndex}.

