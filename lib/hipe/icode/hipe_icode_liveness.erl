%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ICODE LIVENESS ANALYSIS
%

-module(hipe_icode_liveness).

-include("../flow/liveness.inc").


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Interface to CFG and icode.
%

cfg_bb(CFG, L) ->
   hipe_icode_cfg:bb(CFG, L).


cfg_postorder(CFG) ->
   hipe_icode_cfg:postorder(CFG).


cfg_labels(CFG) ->
   hipe_icode_cfg:labels(CFG).


cfg_bb_update(CFG, L, NewBB) ->
   hipe_icode_cfg:bb_update(CFG, L, NewBB).


cfg_succ_map(CFG) ->
   hipe_icode_cfg:succ_map(CFG).


cfg_succ(CFG, L) ->
   hipe_icode_cfg:succ(CFG, L).


uses(Instr) ->
   hipe_icode:uses(Instr).


defines(Instr) ->
   hipe_icode:defines(Instr).


mk_comment(Text) ->
   hipe_icode:mk_comment(Text).



%
% This is the list of registers that are live at exit from a function
%

liveout_no_succ() ->
    ordsets:new().
