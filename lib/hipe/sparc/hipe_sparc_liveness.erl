%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% LIVENESS ANALYSIS
%%
%% Exports:
%% ~~~~~~~
%% analyze(CFG) - returns a liveness analysis of CFG.
%% liveout(Liveness, Label) - returns a set of variables that are live at
%%      exit from basic block named Label.
%% livein(Liveness, Label) - returns a set of variables that are live at
%%      entry to the basic block named Label.
%% list(Instructions, LiveOut) - Given a list of instructions and a liveout-set,
%%      returns a set of variables live at the first instruction.

-module(hipe_sparc_liveness).

%% The following, is addition to all exports of liveness.inc
-export([update_livein/3]).

-define(LIVEOUT_NEEDED,true).   % needed for liveness.inc below.

-include("../flow/liveness.inc").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Interface to CFG and SPARC.
%%

cfg_bb(CFG, L) ->
  hipe_sparc_cfg:bb(CFG, L).

cfg_postorder(CFG) ->
  hipe_sparc_cfg:postorder(CFG).

cfg_succ_map(CFG) ->
  hipe_sparc_cfg:succ_map(CFG).

cfg_succ(CFG, L) ->
  hipe_sparc_cfg:succ(CFG, L).

uses(Instr) ->
  hipe_sparc:uses(Instr).

defines(Instr) ->
  hipe_sparc:defines(Instr).

%%
%% This is the list of registers that are live at exit from a function
%%

liveout_no_succ() ->
  ordsets:from_list(lists:map(fun(R) -> hipe_sparc:mk_reg(R) end,
		     hipe_sparc_registers:global())).

%%
%% The following are used only if annotation of the code is requested.
%%
-ifdef(DEBUG_LIVENESS).

cfg_labels(CFG) ->
  hipe_sparc_cfg:labels(CFG).

cfg_bb_add(CFG, L, NewBB) ->
  hipe_sparc_cfg:bb_add(CFG, L, NewBB).

mk_comment(Text) ->
  hipe_sparc:comment_create(Text, []).

-endif.
