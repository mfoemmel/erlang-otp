
%%%----------------------------------------------------------------------
%%% File    : ssa.erl
%%% Author  : 
%%% Purpose : 
%%% Created : 
%%%----------------------------------------------------------------------


-module(hipe_icode_ssa).
-include("../ssa/hipe_ssa.inc").
     
place_phi(CFG, DominanceFrontier) ->
  hipe_icode_ssa_phi:place(CFG, DominanceFrontier).

rename(CFG, DominatorTree) ->
  hipe_icode_ssa_rename:rename(CFG, DominatorTree).

label_range(CFG) ->
  hipe_icode_cfg:label_range(CFG).

start_label(CFG) ->
  hipe_icode_cfg:start_label(CFG).

mk_goto(L) ->
  hipe_icode:mk_goto(L).

bb_add(CFG, Label, BB) ->
  hipe_icode_cfg:bb_add(CFG, Label, BB).

start_label_update(CFG, NewStartLabel) ->
  hipe_icode_cfg:start_label_update(CFG, NewStartLabel).

label_range_update(CFG, Range) ->
  hipe_icode_cfg:label_range_update(CFG, Range).

succ_map(CFG) ->
  hipe_icode_cfg:succ_map(CFG).
