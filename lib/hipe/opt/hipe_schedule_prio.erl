%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	      PRIORITY HANDLING AND PRIORITY CALCULATION
%
% Handling of ready nodes and priorities.
% - at present, all nodes have the same priority and so on.
%
% *** UNFINISHED ***
% - should compute a static priority estimate
% - should dynamically modify priorities + possibly insert NOPs
%   (e.g., to separate branches, etc.)
% - thus, ought to be passed the current schedule and/or resources as well

-module(hipe_schedule_prio).
-export([init_ready/2,
	 init_instr_prio/3,
	 initial_ready_set/4,
	 next_ready/4,
	 add_ready_nodes/2,
	 insert_node/3
	 ]).

-define(prio,hipe_ultra_prio).

init_ready(Size,Preds) ->
    ?prio:init_ready(Size,Preds).

init_instr_prio(N,Nodes,DAG) ->
    ?prio:init_instr_prio(N,Nodes,DAG).

initial_ready_set(M,N,Preds,Ready) ->
    ?prio:initial_ready_set(M,N,Preds,Ready).

next_ready(C,Ready,Rsrc,Prio) ->
    ?prio:next_ready(C,Ready,Rsrc,Prio).

add_ready_nodes(NodeLst,Ready) ->
    ?prio:add_ready_nodes(NodeLst,Ready).

insert_node(C,I,Ready) ->
    ?prio:insert_node(C,I,Ready).
