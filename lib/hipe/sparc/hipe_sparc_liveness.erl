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

-module(hipe_sparc_liveness).

-include("../util/hipe_vector.hrl").
-include("../flow/liveness.inc").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Interface to CFG and rtl.
%

cfg_bb(CFG, L) ->
   hipe_sparc_cfg:bb(CFG, L).


cfg_postorder(CFG) ->
   hipe_sparc_cfg:postorder(CFG).


cfg_labels(CFG) ->
   hipe_sparc_cfg:labels(CFG).


cfg_succ_map(CFG) ->
   hipe_sparc_cfg:succ_map(CFG).


cfg_succ(CFG, L) ->
   hipe_sparc_cfg:succ(CFG, L).


cfg_bb_update(CFG, L, NewBB) ->
   hipe_sparc_cfg:bb_update(CFG, L, NewBB).


uses(Instr) ->
  hipe_sparc:uses(Instr).

defines(Instr) ->
  hipe_sparc:defines(Instr).

mk_comment(Text) ->
   hipe_sparc:comment_create(Text, []).


%
% This is the list of registers that are live at exit from a function
%

liveout_no_succ() ->
   ordsets:from_list(lists:map(fun(R) -> hipe_sparc:mk_reg(R) end,
			    hipe_sparc_registers:global())).
