%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%	      PRIORITY HANDLING AND PRIORITY CALCULATION
%%
%% Handling of ready nodes and priorities.
%% - at present, all nodes have the same priority and so on.
%%
%% *** UNFINISHED ***
%% - should compute a static priority estimate
%% - should dynamically modify priorities + possibly insert NOPs
%%   (e.g., to separate branches, etc.)
%% - thus, ought to be passed the current schedule and/or resources as well

-module(hipe_schedule_prio).
-export([init_ready/2,
	 init_instr_prio/2,
	 initial_ready_set/4,
	 next_ready/7,
	 add_ready_nodes/2,
	 insert_node/3
	]).

init_ready(Size,Preds) ->
  hipe_ultra_prio:init_ready(Size,Preds).

init_instr_prio(N,DAG) ->
  hipe_ultra_prio:init_instr_prio(N,DAG).

initial_ready_set(M,N,Preds,Ready) ->
  hipe_ultra_prio:initial_ready_set(M,N,Preds,Ready).

next_ready(C,Ready,Prio,Nodes,DAG,Preds,Earl) ->
  hipe_ultra_prio:next_ready(C,Ready,Prio,Nodes,DAG,Preds,Earl).

add_ready_nodes(NodeLst,Ready) ->
  hipe_ultra_prio:add_ready_nodes(NodeLst,Ready).

insert_node(C,I,Ready) ->
  hipe_ultra_prio:insert_node(C,I,Ready).
