%%% $Id$
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% LIVENESS ANALYSIS
%
% Exports:
% ~~~~~~~
% analyze(CFG) - returns a livenes analyzis of CFG.
% liveout(Liveness, Label) - returns a set of variables that are alive at
%      exit from basic block named Label.
% livein(Liveness, Label) - returns a set of variables that are alive at
%      entry to the basic block named Label.
% list(Instructions, LiveOut) - Given a list of instructions and a liveout-set,
%      returns a set of variables live at the first instruction.
%

-module(hipe_rtl_liveness).

-include("../flow/liveness.inc").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Interface to CFG and rtl.
%

cfg_bb(CFG, L) ->
   hipe_rtl_cfg:bb(CFG, L).


cfg_postorder(CFG) ->
   hipe_rtl_cfg:postorder(CFG).


cfg_labels(CFG) ->
   hipe_rtl_cfg:labels(CFG).


cfg_succ_map(CFG) ->
   hipe_rtl_cfg:succ_map(CFG).


cfg_succ(CFG, L) ->
   hipe_rtl_cfg:succ(CFG, L).


cfg_bb_update(CFG, L, NewBB) ->
   hipe_rtl_cfg:bb_update(CFG, L, NewBB).


uses(Instr) ->
   hipe_rtl:uses(Instr).


defines(Instr) ->
   hipe_rtl:defines(Instr).


mk_comment(Text) ->
   hipe_rtl:mk_comment(Text).

%
% This is the list of registers that are live at exit from a function
%

liveout_no_succ() ->
    hipe_rtl_arch:live_at_return().
