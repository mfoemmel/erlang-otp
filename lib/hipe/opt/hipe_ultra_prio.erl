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

-module(hipe_ultra_prio).
-export([init_ready/2,
	 init_instr_prio/3,
	 initial_ready_set/4,
	 next_ready/4,
	 add_ready_nodes/2,
	 insert_node/3
	 ]).

% At first, only nodes with no predecessors are selected.
% - if R is empty, there is an error (unless BB itself is empty)

init_ready(Size,Preds) ->
    P = hipe_vectors:vsize(Preds),
    Ready = hipe_vectors:init(Size,[]),
    R = initial_ready_set(1,P,Preds,[]),
    hipe_vectors:set(Ready,1,R).

init_instr_prio(N,Nodes,DAG) ->
    critical_path(N,DAG).

initial_ready_set(M,N,Preds,Ready) ->
    if
	M > N ->
	    Ready;
	true ->
	    case hipe_vectors:get(Preds,M) of
		0 ->
		    initial_ready_set(M+1,N,Preds,[M|Ready]);
		V when V > 0 ->
		    initial_ready_set(M+1,N,Preds,Ready)
	    end
    end.

% The following handles the nodes ready to schedule:
% 1. select the ready queue of given cycle
% 2. if queue empty, return none
% 3. otherwise, remove entry with highest priority
%    and return {next,Highest_Prio,NewReady}

next_ready(C,Ready,Rsrc,Prio) ->
    Curr = hipe_vectors:get(Ready,C),
    case Curr of
	[] -> 
	    none;
	[I|Is] ->
	    {next,I,hipe_vectors:set(Ready,C,Is)}
    end.

add_ready_nodes([],Ready) -> Ready;
add_ready_nodes([{C,I}|Xs],Ready) ->
    add_ready_nodes(Xs,insert_node(C,I,Ready)).

insert_node(C,I,Ready) ->
    Old = hipe_vectors:get(Ready,C),
    hipe_vectors:set(Ready,C,[I|Old]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

critical_path(N,DAG) ->
    critical_path(1,N,DAG,hipe_vectors:init(N,-1)).

critical_path(M,N,DAG,Prio) ->
    if
	M > N ->
	    Prio;
	true ->
	    critical_path(M+1,N,DAG,cpath(M,DAG,Prio))
    end.

% - if node has prio -1, it has not been visited
% - otherwise, compute priority as max of priorities of successors (+ latency)

cpath(M,DAG,Prio) ->
    InitPrio = hipe_vectors:get(Prio,M),
    if
	InitPrio == -1 ->
	    cpath_node(M,DAG,Prio);
	true ->
	    Prio
    end.

cpath_node(N,DAG,Prio) ->
    SuccL = dag_succ(DAG,N),
    {Max,NewPrio} = cpath_succ(SuccL,DAG,Prio),
    hipe_vectors:set(NewPrio,N,Max).

cpath_succ(SuccL,DAG,Prio) ->
    cpath_succ(SuccL,DAG,Prio,0).

% performs an unnecessary lookup of priority of Succ, but that
% might not be such a big deal

cpath_succ([],DAG,Prio,NodePrio) -> {NodePrio,Prio};
cpath_succ([{Lat,Succ}|Xs],DAG,Prio,NodePrio) ->
    NewPrio = cpath(Succ,DAG,Prio),
    NewNodePrio = max(hipe_vectors:get(NewPrio,Succ)+Lat,NodePrio),
    cpath_succ(Xs,DAG,NewPrio,NewNodePrio).

dag_succ(DAG,N) ->
    hipe_vectors:get(DAG,N).

max(X,Y) ->
    if
	X < Y -> Y;
	true -> X
    end.
