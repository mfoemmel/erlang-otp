
%%%----------------------------------------------------------------------
%%% File    : ssa.inc
%%% Author  : 
%%% Purpose : 
%%% Created : 
%%%----------------------------------------------------------------------
-export([ssa/1]).
-include("../main/hipe.hrl").

ssa(CFG) ->
    CFG1 = insertNewStartNode(CFG),

    ?opt_start_timer("idom"),
    DominatorTree = hipe_domtree:create(CFG1),
    ?opt_stop_timer("idom done"),

    ?opt_start_timer("Dominance Frontier"),
    SuccMap = succ_map(CFG1),

    DominanceFrontier = hipe_df:make(SuccMap, DominatorTree),
    ?opt_stop_timer("Dominance Frontier done"),

    ?opt_start_timer("addPhi"),
    CFG2 = place_phi(CFG1, DominanceFrontier),
    ?opt_stop_timer("addPhi done"),

    ?opt_start_timer("Rename"),
    CFG3 = rename(CFG2, DominatorTree),
    ?opt_stop_timer("Rename done"),

    CFG3.
     

insertNewStartNode(CFG) ->
    {LMin, LMax} = label_range(CFG),
    StartLabel = start_label(CFG),
    NewStartLabel = LMax + 1, 
    BB = hipe_bb:mk_bb([mk_goto(StartLabel)]),
    CFG2 = bb_add(CFG, NewStartLabel, BB),
    CFG3 = start_label_update(CFG2, NewStartLabel),
    label_range_update(CFG3, {LMin, LMax + 1}).
    
