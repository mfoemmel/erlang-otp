%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%			INSTRUCTION SCHEDULER
%%
%% This is a basic ILP cycle scheduler:
%% * set cycle = 0
%% * while ready[cycle] nonempty do
%%   - take x with greatest priority from ready[cycle]
%%   - try to schedule x;
%%     * if scheduling x was possible, 
%%       - reserve resources
%%       - add x to schedule and delete x from dag
%%       - update earliest-time for all successor nodes
%%         as max[earliest[y],cycle+latency[x]]
%%       - if some node y now has no predecessors,
%%         add y to ready[earliest[y]]
%%     * if it was impossible, put x in ready[cycle+1]
%%       (= try again)
%%
%% We use the following data structures:
%% 1. all nodes are numbered and indices used as array keys
%% 2. priority per node can be computed statically or dynamically
%%    * statically: before scheduling, each node gets a priority value
%%    * dynamically: at each cycle, compute priorities for all ready nodes
%% 3. earliest: earliest cycle of issue, starts at 0
%%    and is updated as predecessors issue
%% 4. predecessors: number of predecessors (0 = ready to issue)
%% 5. successors: list of {Latency,NodeID}
%% 6. ready: an array indexed by cycle-time (integer), where
%%    ready nodes are kept.
%% 7. resources: a resource representation (ADT) that answers
%%    certain queries, e.g., "can x be scheduled this cycle"
%%    and "reserve resources for x".
%% 8. schedule: list of scheduled instructions {Instr,Cycle}
%%    in the order of issue
%% 9. instructions: maps IDs back to instructions
%%
%% Inputs:
%% - a list of {ID,Node} pairs (where ID is a unique key)
%% - a dependence list {ID0,Latency,ID1}, which is used to
%%   build the DAG.
%%
%% Note that there is some leeway in how things are represented
%% from here.
%%
%% MODIFICATIONS:
%% - Some basic blocks are not worth scheduling (e.g., GC save/restore code)
%%   yet are pretty voluminous. How do we skip them?
%% - Scheduling should be done at finalization time: when basic block is
%%   linearized and is definitely at Sparc assembly level, THEN reorder
%%   stuff.

-module(hipe_schedule).
-export([deps/1, block/1, cfg/1, est_cfg/1]).


-define(debug(Str,Args),ok).
%-define(debug(Str,Args),io:format(Str,Args)).

-define(debug_ultra(Str,Args),ok).
%-define(debug_ultra(Str,Args),io:format(Str,Args)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Schedule all the basic blocks of the CFG:

cfg(CFG) ->
    update_all( [ {L, hipe_bb:mk_bb(block(hipe_bb:code(hipe_sparc_cfg:bb(CFG,L))))}
		 || L <- hipe_sparc_cfg:labels(CFG) ], CFG).

update_all([],CFG) -> CFG;
update_all([{L,NewB}|Ls],CFG) ->
    update_all(Ls,hipe_sparc_cfg:bb_update(CFG,L,NewB)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

est_cfg(CFG) ->
    update_all( [ {L, hipe_bb:mk_bb(est_block(hipe_bb:code(hipe_sparc_cfg:bb(CFG,L))))}
		 || L <- hipe_sparc_cfg:labels(CFG) ], CFG).

est_block(Blk) ->
    lists:append( [ est_block2(Sub_block) 
		   || Sub_block <- split_serial(Blk) ] ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Schedule a basic block
%
% Note: does not consider delay slots!
%   (another argument for using only annulled delay slots?)
% * how do we add delay slots? somewhat tricky to reconcile with the
%   sort of scheduling we consider. (as-early-as-possible)
%    => rewrite scheduler into as-late-as-possible?
%    (=> just reverse the dependence arcs??)

block(Blk) ->
    lists:append( [ block2(Sub_block) || Sub_block <- split_serial(Blk) ] ).

% Block scheduler 0:
% - annotate instructions with the cycle of issue
%
% Don't fire up the scheduler if there's no work to do.
%block0([]) -> [];
%block0([I]) -> [I];
%block0(Blk) ->
%    {IxBlk,Deps} = deps(Blk),
%    Sch = bb(IxBlk,Deps),
%    resolve_block(Sch,IxBlk).

%resolve_block(Sch,IxBlk) ->
%    [ {{cycle,C},lookup_instr(IxBlk,N)} || {{cycle,C},{node,N}} <- Sch ].

lookup_instr([{N,I}|_],N) -> I;
lookup_instr([_|Xs],N) -> lookup_instr(Xs,N).

% Block scheduler 1:
% - erase all cycle-info to yield valid code stream
%
% Don't fire up the scheduler if there's no work to do.
%block1([]) -> [];
%block1([I]) -> [I];
%block1(Blk) ->
%    {IxBlk,Deps} = deps(Blk),
%    Sch = bb(IxBlk,Deps),
%    erase_block(Sch,IxBlk).

%erase_block(Sch,IxBlk) ->
%    [ lookup_instr(IxBlk,N) || {{cycle,C},{node,N}} <- Sch ].

% Block scheduler 2:
% - insert comment annotations per cycle
%
% Don't fire up the scheduler if there's no work to do.

block2([]) -> [];
block2([I]) -> [I];
block2(Blk) ->
    {IxBlk,Deps} = deps(Blk),
    Sch = bb(IxBlk,Deps),
    separate_block(Sch,IxBlk).

separate_block(Sch,IxBlk) ->
    sep_comments( [ {C,lookup_instr(IxBlk,N)} 
		   || {{cycle,C},{node,N}} <- Sch ] ).

sep_comments([]) -> [];
sep_comments([{C,I}|Xs]) ->
    [hipe_sparc:comment_create({cycle,C},[]), I | sep_comments(Xs,C) ].

sep_comments([],_) -> [];
sep_comments([{C1,I}|Xs],C0) ->
    if
	C1 > C0 ->
	    [hipe_sparc:comment_create({cycle,C1},[]),I|sep_comments(Xs,C1)];
	true ->
	    [I|sep_comments(Xs,C0)]
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Provides an estimation of how quickly a block will execute.
% This is done by chaining all instructions in sequential order
% by 0-cycle dependences (which means they will never be reordered), 
% then scheduling the mess.

est_block2([]) -> [];
est_block2([I]) -> [I];
est_block2(Blk) ->
    {IxBlk,Deps} = est_deps(Blk),
    Sch = bb(IxBlk,Deps),
    separate_block(Sch,IxBlk).

est_deps(Blk) ->
    {IxBB,Deps} = deps(Blk),
    {IxBB, chain_instrs(IxBB,Deps)}.

chain_instrs([{N,X}|Xs],Deps) ->
    chain_i(N,Xs,Deps).

chain_i(N,[],Deps) -> Deps;
chain_i(N,[{M,_}|Xs],Deps) ->
    chain_i(M,Xs,dep_arc(N,zero_latency(),M,Deps)).

zero_latency() -> 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

split_serial(Is) -> 
    split_serial(Is,[]).

% Note that Curr is a reversed list, which is re-reversed
% when block is added to list of blocks. When we build the dependence graph,
% we traverse the list back-to-front, so if we were willing to take the pain,
% we could simply return the unreversed list and rewrite the dep-graph builders
    
split_serial([],Curr) ->
    emit_block(Curr,[]);
split_serial([I|Is],Curr) ->
    case serializing_instr(I) of
	true ->
	    emit_block(Curr,emit_block([I],split_serial(Is,[])));
	false ->
	    split_serial(Is,[I|Curr])
    end.

% Avoid emitting empty blocks

emit_block([],Blks) -> Blks;
emit_block(Xs,Blks) ->
    [ lists:reverse(Xs) | Blks ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Schedule a sequential trace of instructions
%
% Nodes is a list of {ID,Instr}
% Deps is a list of {ID,Latency,ID}

bb(Nodes,Deps) ->
    bb(length(Nodes),Nodes,Deps).

bb(N,Nodes,Deps) ->
    {DAG,Preds} = build_ddg(N,Deps),
    Earliest = init_earliest(N),
    BigArray = N*10,                     % "nothing" is this big :-)
    Ready = hipe_schedule_prio:init_ready(BigArray,Preds),
    I_res = init_instr_resources(N,Nodes),
    Prio = hipe_schedule_prio:init_instr_prio(N,Nodes,DAG),
    Rsrc = init_resources(BigArray),
    ?debug('cycle 1~n',[]),
    Sch = empty_schedule(),
    cycle_sched(1,Ready,DAG,Preds,Earliest,Rsrc,I_res,Prio,Sch,N).

% Scheduler main loop:
% - C is current cycle, 1 or more
% - Ready is an array (Cycle -> [Node])
%     yielding the collection of nodes ready to be scheduled in a cycle
% - DAG is an array (Instr -> [{Latency,Instr}])
%     represents the dependence DAG
% - Preds is an array (Instr -> NumPreds)
%     counts the number of predecessors (0 preds = ready to be scheduled)
% - Earl is an array (Instr -> EarliestCycle)
%     holds the earliest cycle an instruction can be scheduled
% - Rsrc is a 'resource ADT' that handles scheduler resource management
%     checks whether instruction can be scheduled this cycle w/o a stall
% - I_res is an array (Instr -> Required_resources)
%     holds the resources required to schedule an instruction
% - Sch is the representation of the schedule
%     current schedule
% - N is the number of nodes remaining to be scheduled
%     tells us when to stop the scheduler

cycle_sched(C,Ready,DAG,Preds,Earl,Rsrc,I_res,Prio,Sch,N) ->
    % pick next ready node in priority order for cycle C
    % until none remain.
    % * check each node if it can be scheduled w/o stalling
    % * if so, schedule it
    % * otherwise, bump the node to the next cycle
    case hipe_schedule_prio:next_ready(C,Ready,Rsrc,Prio) of
	{next,I,Ready1} ->
	    ?debug('try ~p~n',[I]),
	    case resources_available(C,I,Rsrc,I_res) of
		{yes,NewRsrc} ->
		    ?debug(' scheduled~n',[]),
		    NewSch = add_to_schedule(I,C,Sch),
		    {ReadyNs,NewDAG,NewPreds,NewEarl} = 
			delete_node(C,I,DAG,Preds,Earl),
		    NewReady = hipe_schedule_prio:add_ready_nodes(ReadyNs,
								  Ready1),
		    cycle_sched(C,NewReady,NewDAG,NewPreds,NewEarl,
				NewRsrc,I_res,Prio,NewSch,N-1);
		no ->
		    ?debug(' resource conflict~n',[]),
		    NewReady = hipe_schedule_prio:insert_node(C+1,I,Ready1),
		    cycle_sched(C,NewReady,DAG,Preds,Earl,Rsrc,
				I_res,Prio,Sch,N)
	    end;
	none ->  % schedule next cycle if some node remains
	    if
		N > 0 ->
		    ?debug('cycle ~p~n',[C+1]),
		    cycle_sched(C+1,Ready,DAG,Preds,Earl,
				advance_cycle(Rsrc),
				I_res,Prio,Sch,N);
		true ->
		    present_schedule(Sch)
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_earliest(N) ->
    hipe_vectors:init(N,1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Schedule is kept reversed until the end.

-define(present_node(I,Cycle),{{cycle,Cycle},{node,I}}).
%-define(present_node(I,Cycle),I).

empty_schedule() -> [].

add_to_schedule(I,Cycle,Sch) ->
    [?present_node(I,Cycle)|Sch].

present_schedule(Sch) -> lists:reverse(Sch).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Interface to resource manager:
%
% init_resources(Size):
%    Yields a 'big enough' array mapping (Cycle -> Resources);
%    this array is called Rsrc below.
% init_instr_resources(N,Nodes):
%    Nodes is a list of the instructions
%    N is the number of nodes
%    return a vector (NodeID -> Resource_requirements)
% resources_available(Cycle,I,Rsrc,I_res):
%   Cycle is the current cycle
%   I is the current instruction (index = NodeID)
%   Rsrc is a map (Cycle -> Resources)
%   I_res maps (NodeID -> Resource_requirements)
%   returns {yes,NewResTab} | no

init_resources(S) ->
    hipe_target_machine:init_resources(S).

init_instr_resources(N,Nodes) ->
    hipe_target_machine:init_instr_resources(N,Nodes).

resources_available(Cycle,I,Rsrc,I_res) ->
    hipe_target_machine:resources_available(Cycle,I,Rsrc,I_res).

advance_cycle(Rsrc) ->
    hipe_target_machine:advance_cycle(Rsrc).

%init_resources(S) ->
%    init_resources_simple(S).
%
%init_instr_resources(N,Nodes) ->
%    init_instr_resources_simple(N,Nodes).
%
%resources_available(Cycle,I,Rsrc,I_res) ->
%    resources_available_simple(Cycle,I,Rsrc,I_res).
%
%advance_cycle(Rsrc) ->
%    advance_cycle_simple(Rsrc).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Almost trivial machine model: only restricted by issue()
%

%init_resources_simple(Size) ->
%    hipe_vectors:init(Size,0).

%init_instr_resources_simple(N,Nodes) ->  % each node costs 1 issue slot
%    hipe_vectors:init(N,1).

%resources_available_simple(Cycle,I,Rsrc,I_res) ->
%    Alloc = hipe_vectors:get(Rsrc,Cycle),
%    Req = hipe_vectors:get(I_res,I),
%    Max = issue(),
%    if
%	Alloc+Req =< Max ->
%	    {yes, hipe_vectors:set(Rsrc,Cycle,Alloc+Req)};
%	true ->
%	    no
%    end.

%issue() -> 2.

%advance_cycle_simple(Rsrc) ->
%    Rsrc.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Building the dependence DAG. Each node has a set of successors
% represented as {Latency,NodeID} and a number of predecessors, an integer.
% - a node with 0 predecessors is ready to be scheduled
%
% *** UNFINISHED ***
% - At present, there may be several arcs from N1 to N2; this means
%   unnecessary work is done. We can replace n arcs of latency l1,...,ln
%   with a single arc of latency max(l1,...,ln).

build_ddg(N,Deps) ->
    {DAG,Preds} = build_dag(Deps,hipe_vectors:init(N,[]),hipe_vectors:init(N,0)),
    ?debug('DAG: ~p~n',[hipe_vectors:list(DAG)]),
    {DAG,Preds}.

build_dag([],DAG,Preds) -> {DAG,Preds};
build_dag([{N1,Lat,N2}|Ds],DAG,Preds) when N1 < N2 ->
    NewDAG = add_arc(N1,Lat,N2,DAG),
    NewPreds = add_pred(N2,Preds),
    build_dag(Ds,NewDAG,NewPreds);
build_dag([{N1,Lat,N2}|Ds],DAG,Preds) when N1 >= N2 ->
    io:format('dependence arc ~p -> ~p yields poss. circular graph~n',[N1,N2]),
    build_dag(Ds,DAG,Preds).

add_arc(N1,Lat,N2,DAG) ->
    Old = hipe_vectors:get(DAG,N1),
    hipe_vectors:set(DAG,N1,[{Lat,N2}|Old]).

add_pred(N2,Pred) ->
    hipe_vectors:set(Pred,N2,hipe_vectors:get(Pred,N2)+1).

%dag_succ(DAG,N) ->
%    hipe_vectors:get(DAG,N).

% Delete a node from the DAG and present any new nodes at the
% correct cycles.
%
% One of the main workhorses.
%  Returns {ReadyNodes,NewDAG,NewPreds,NewEarl}
% where ReadyNodes is a list of {Cycle,Node}
%  and New{DAG,Preds,Earl} are the modified versions of the originals.

delete_node(Cycle,I,DAG,Preds,Earl) ->
    Succ = hipe_vectors:get(DAG,I),
    NewDAG = hipe_vectors:set(DAG,I,scheduled),  % provides debug 'support'
    {ReadyNs,NewPreds,NewEarl} = update_earliest(Succ,Cycle,Preds,Earl,[]),
    ?debug('earliest after ~p: ~p~n',[I,hipe_vectors:list(NewEarl)]),
    {ReadyNs,NewDAG,NewPreds,NewEarl}.

update_earliest([],Cycle,Preds,Earl,Ready) ->
    {Ready,Preds,Earl};
update_earliest([{Lat,N}|Xs],Cycle,Preds,Earl,Ready) ->
    Old_earl = hipe_vectors:get(Earl,N),
    New_earl = max(Old_earl,Cycle+Lat),
    NewEarl = hipe_vectors:set(Earl,N,New_earl),
    Num_preds = hipe_vectors:get(Preds,N),
    NewPreds = hipe_vectors:set(Preds,N,Num_preds-1),
    if
	Num_preds == 0 ->
	    ?debug('inconsistent DAG~n',[]),
	    exit({update_earliest,N});
	Num_preds == 1 ->
	    NewReady = [{New_earl,N}|Ready],
	    NewPreds2 = hipe_vectors:set(NewPreds,N,0),
	    update_earliest(Xs,Cycle,NewPreds2,NewEarl,NewReady);
	Num_preds > 1 ->
	    update_earliest(Xs,Cycle,NewPreds,NewEarl,Ready)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Collect instruction dependences.
%
% Three forms:
% - data/register
%   * insert RAW, WAR, WAW dependences
% - memory
%   * stores serialize memory references
%   * alias analysis may allow loads to bypass stores
% - control
%   * unsafe operations are 'trapped' between branches
%   * branches are ordered
%
% returns { [{Index,Instr}], DepDAG }
%   DepDAG is defined below.

deps(BB) ->
    IxBB = indexed_bb(BB),
    DG = empty_dg(length(BB)),
    {DepTab,DG1} = dd(IxBB,DG),
    DG2 = md(IxBB,DG1),
%    DG3 = cd(IxBB,DG2),     % at this time, branches handled sep.
    {IxBB,DG2}.

%deps(T,BB) ->
%    IxBB = indexed_bb(BB),
%    DG = empty_dg(length(BB)),
%    DG1 = case T of
%	      cd ->
%		  cd(IxBB,DG);
%	      md ->
%		  md(IxBB,DG);
%	      dd ->
%		  {_,DG2} = dd(IxBB,DG),
%		  DG2
%	  end,
%    {IxBB,DG1}.

indexed_bb(BB) ->
    indexed_bb(BB,1).

indexed_bb([],N) -> [];
indexed_bb([X|Xs],N) ->
    case hipe_sparc:type(X) of
	comment ->
	    indexed_bb(Xs,N);
	_ ->
	    [{N,X}|indexed_bb(Xs,N+1)]
    end.

%%%%%%%%%%%%%%%%%%%%
%
% At present, we return a list of dependences. A mature version would
% instead build the DG at once. (In the form of {DAG,Preds} above.)
% The current form simplifies debugging, however.
% 
% At present, DepDAG = [{From,Latency,To}]
%  where From and To are instruction IDs 
%  and Latency is the instruction latency in cycles (0 or greater)

empty_dg(N) -> [].

dep_arc(From,Lat,To,DG) ->
    [{From,Lat,To}|DG].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The register/data dependence DAG of a block is represented
% as a mapping (Variable -> {NextWriter,NextReaders})
%  where NextWriter is a pair {Ix,Type}
%  and NextReaders is a list of pairs {Ix,Type}.
%
% Type is used to determine latencies of operations; on the UltraSparc,
% latencies of arcs (n -> m) are determined by both n and m. (E.g., if
% n is an integer op and m is a store, then latency is 0; if m is an
% integer op, it's 1.)

dd([],DG) -> { empty_deptab(), DG };
dd([{N,I}|Is],DG0) ->
    {DepTab,DG1} = dd(Is,DG0),
    add_deps(N,I,DepTab,DG1).

add_deps(N,Instr,DepTab,DG) ->
    {Ds,Us} = def_use(Instr),
    Type = dd_type(Instr),
    {DepTab1,DG1} = add_write_deps(Ds,N,Type,DepTab,DG),
    add_read_deps(Us,N,Type,DepTab1,DG1).

% Instructions are classified into symbolic categories,
% which are subsequently used to determine operation latencies

dd_type(Instr) ->
    case hipe_sparc:type(Instr) of
	b -> branch;
	br -> branch;
	call_link -> branch;
	jmp_link -> branch;
	jmp -> branch;
	goto -> branch;
	load -> load;
	store -> store;
	alu -> alu;
	move -> alu;
	multimove -> alu;
	sethi -> alu;
	alu_cc -> alu_cc;
	cmov_cc -> alu_cc;
	cmov_r -> alu;
	load_atom -> alu;
	load_address -> alu;
	load_word_index -> alu;
	_ -> pseudo
    end.

%%%%%%%%%%%%%%%%%%%%

add_write_deps([],N,Ty,DepTab,DG) -> {DepTab,DG};
add_write_deps([D|Ds],N,Ty,DepTab,DG) ->
    {NewDepTab,NewDG} = add_write_dep(D,N,Ty,DepTab,DG),
    add_write_deps(Ds,N,Ty,NewDepTab,NewDG).

add_write_dep(X,N,Ty,DepTab,DG) ->
    {NxtWriter,NxtReaders} = lookup(X,DepTab),
    NewDepTab = writer(X,N,Ty,DepTab),
    NewDG = write_deps(N,Ty,NxtWriter,NxtReaders,DG),
    { NewDepTab, NewDG }.

% If NxtWriter is 'none', then this is the last write
% Add WAW Instr -> NxtWriter (if it exists)
% Add RAW Instr -> NxtReaders
% *** UNFINISHED ***
% - unfinished how?

write_deps(Instr,Ty,NxtWriter,NxtReaders,DG) ->
    DG1 = case NxtWriter of
	      none ->
		  DG;
	      {Instr,_} ->
		  DG;
	      {Wr,WrTy} ->
		  dep_arc(Instr,hipe_target_machine:waw_latency(Ty,WrTy),Wr,DG)
	  end,
    raw_deps(Instr,Ty,NxtReaders,DG1).

raw_deps(Instr,Type,[],DG) -> DG;
raw_deps(Instr,Ty,[{Rd,RdTy}|Xs],DG) ->
    raw_deps(Instr,Ty,Xs,
	     dep_arc(Instr,hipe_target_machine:raw_latency(Ty,RdTy),Rd,DG)).

%%%%%%%%%%%%%%%%%%%%

add_read_deps([],N,Ty,DepTab,DG) -> {DepTab,DG};
add_read_deps([U|Us],N,Ty,DepTab,DG) ->
    {NewDepTab,NewDG} = add_read_dep(U,N,Ty,DepTab,DG),
    add_read_deps(Us,N,Ty,NewDepTab,NewDG).

add_read_dep(X,N,Ty,DepTab,DG) ->
    {NxtWriter,NxtReaders} = lookup(X,DepTab),
    NewDepTab = reader(X,N,Ty,DepTab),
    NewDG = read_deps(N,Ty,NxtWriter,NxtReaders,DG),
    { NewDepTab, NewDG }.

% If NxtWriter is 'none', then this var is not written subsequently
% Add WAR from Instr to NxtWriter (if it exists)
% *** UNFINISHED ***

read_deps(Instr,Ty,none,_,DG) ->
    DG;
read_deps(Instr,Ty,{Instr,_},_,DG) ->
    DG;
read_deps(Instr,Ty,{NxtWr,NxtWrTy},_,DG) ->
    dep_arc(Instr,hipe_target_machine:war_latency(Ty,NxtWrTy),NxtWr,DG).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

empty_deptab() ->
    hipe_hash:empty().

lookup(X,DepTab) ->
    case hipe_hash:lookup(X,DepTab) of
	not_found ->
	    {none,[]};
	{found,{W,Rs}} ->
	    {W,Rs}
    end.

writer(X,N,Ty,DepTab) ->
    hipe_hash:update(X,{{N,Ty},[]},DepTab).

reader(X,N,Ty,DepTab) ->
    {W,Rs} = lookup(X,DepTab),
    hipe_hash:update(X,{W,[{N,Ty}|Rs]},DepTab).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Memory dependences: if a load possibly overlaps with a preceding
%  store, add a RAW/WAR. If a store possibly overlaps with prec. store,
%  add WAW.
%
% At present, extremely simple worst-case. (md_simple)
%
% Version 2: separate stack refs from heap refs (md1 below)
%
% Version 3: alias analysis to allow heap loads to bypass heap stores
%
% *** UNFINISHED ***

%md(BB,DG) ->
%    md_simple(BB,DG,none,[]).

md(BB,DG) ->
    md1(BB,DG).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%md_simple([],DG,PrevSt,PrevLds) ->
%    DG;
%md_simple([{N,I}|Xs],DG,PrevSt,PrevLds) ->
%    case md_type(I) of
%	{store,Ty} ->
%	    NewDG = store_deps_simple(PrevSt,PrevLds,N,Ty,DG),
%	    md_simple(Xs,NewDG,{N,Ty},[]);
%	{load,Ty} ->
%	    NewDG = load_deps_simple(PrevSt,PrevLds,N,Ty,DG),
%	    md_simple(Xs,NewDG,PrevSt,[{N,Ty}|PrevLds]);
%	other ->
%	    md_simple(Xs,DG,PrevSt,PrevLds)
%    end.

% *** UNFINISHED ***

%load_deps_simple(none,PrevLds,CurrLoadIx,CurrTy,DG) ->
%    DG;
%load_deps_simple({PrevSt,Ty},PrevLds,CurrLoadIx,CurrTy,DG) ->
%    dep_arc(PrevSt,hipe_target_machine:m_raw_latency(Ty,CurrTy),CurrLoadIx,DG).

% *** UNFINISHED ***

%store_deps_simple(PrevSt,PrevLds,CurrStoreIx,CurrTy,DG) ->
%    DG1 = case PrevSt of
%	      none ->
%		  DG;
%	      {St,StTy} ->
%		  dep_arc(St,hipe_target_machine:m_waw_latency(StTy,CurrTy),
%			  CurrStoreIx,DG)
%	  end,
%    m_war_deps(PrevLds,CurrStoreIx,CurrTy,DG1).

%m_war_deps([],N,Ty,DG) -> DG;
%m_war_deps([{Ld,LdTy}|Lds],N,Ty,DG) ->
%    m_war_deps(Lds,N,Ty,
%	       dep_arc(Ld,hipe_target_machine:m_war_latency(LdTy,Ty),N,DG)).

% Is the operation a load, a store or something else?
% *** UNFINISHED ***
% - what about various 'implicit loads' (return?) etc?

%md_type(I) ->
%    case hipe_sparc:type(I) of
%	load ->
%	    {load,unknown};
%	store ->
%	    {store,unknown};
%	_ ->
%	    other
%    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The following version of md/2 separates heap- and stack operations,
% which allows for greater reordering.

md1(Xs,DG) ->
    md1(Xs,empty_md_state(),DG).

md1([],St,DG) -> DG;
md1([{N,I}|Is],St,DG) ->
    case md_type_1(I) of
	other ->
	    md1(Is,St,DG);
	{st,T} ->
	    { WAW_nodes, WAR_nodes, NewSt } = st_overlap(N,T,St),
	    md1(Is,NewSt, 
		md1_war_deps(WAR_nodes,N,md1_waw_deps(WAW_nodes,N,DG)));
	{ld,T} ->
	    { RAW_nodes, NewSt } = ld_overlap(N,T,St),
	    md1(Is,NewSt,
		md1_raw_deps(RAW_nodes,N,DG))
    end.

md1_war_deps([],N,DG) -> DG;
md1_war_deps([M|Ms],N,DG) ->
    md1_war_deps(Ms,N,dep_arc(M,hipe_target_machine:m_war_latency(),N,DG)).

md1_waw_deps([],N,DG) -> DG;
md1_waw_deps([M|Ms],N,DG) ->
    md1_waw_deps(Ms,N,dep_arc(M,hipe_target_machine:m_waw_latency(),N,DG)).

md1_raw_deps([],N,DG) -> DG;
md1_raw_deps([M|Ms],N,DG) ->
    md1_raw_deps(Ms,N,dep_arc(M,hipe_target_machine:m_raw_latency(),N,DG)).

empty_md_state() -> { [], [], [], [] }.

md_type_1(I) ->
    case hipe_sparc:type(I) of
	load ->
	    Sp = hipe_sparc_registers:stack_pointer(),
	    Src = hipe_sparc:load_src(I),
	    N = hipe_sparc:reg_nr(Src),
	    Off = hipe_sparc:load_off(I),
	    if
		N == Sp -> % operation on stack
		    {ld,{sp,Off}};
		true ->
		    {ld,{hp,Src,Off}}
	    end;
	store ->
	    Sp = hipe_sparc_registers:stack_pointer(),
	    Dst = hipe_sparc:store_dest(I),
	    N = hipe_sparc:reg_nr(Dst),
	    Off = hipe_sparc:store_off(I),
	    if
		N == Sp ->
		    {st,{sp,Off}};
		true ->
		    {st,{hp,Dst,Off}}
	    end;
	_ ->
	    other
    end.

% Given a memory operation and a 'memory op state',
% overlap(N,MemOp,State) returns { Preceding_Dependent_Ops, NewState }.
%  which are either a tuple { WAW_deps, WAR_deps } or a list RAW_deps.
%
% NOTES:
%  Note that Erlang's semantics ("heap stores never overwrite existing data")
% means we can be quite free in reordering stores to the heap.
%  Ld/St to the stack are simply handled by their offsets; since we do not
% rename the stack pointer, this is sufficient.
%  *** We assume all memory ops have uniform size = 4 ***
%
% Alias state:
%   { [StackOp], [HeapOp], [StackOp], [HeapOp] }
% where StackOp = {InstrID, Offset}
%       HeapOp = {InstrID, Reg, Offset}

st_overlap(N, {sp,Off}, { St_Sp, St_Hp, Ld_Sp, Ld_Hp }) ->
    {DepSt,IndepSt_Sp} = st_sp_dep(St_Sp,Off),
    {DepLd,IndepLd_Sp} = ld_sp_dep(Ld_Sp,Off),
    { DepSt, DepLd, { [{N,Off}|IndepSt_Sp], St_Hp, IndepLd_Sp, Ld_Hp }};
st_overlap(N, {hp,Dst,Off}, { St_Sp, St_Hp, Ld_Sp, Ld_Hp }) ->
    { [], [], { St_Sp, [{N,Dst,Off}|St_Hp], Ld_Sp, Ld_Hp }}.


ld_overlap(N, {sp,Off}, { St_Sp, St_Hp, Ld_Sp, Ld_Hp }) ->
    DepSt =  sp_dep_only(St_Sp,Off),
    { DepSt, { St_Sp, St_Hp, [{N,Off}|Ld_Sp], Ld_Hp }};
ld_overlap(N, {hp,Src,Off}, { St_Sp, St_Hp, Ld_Sp, Ld_Hp }) ->
    DepSt = hp_dep_only(St_Hp, Src, Off),
    { DepSt, { St_Sp, St_Hp, Ld_Sp, [{N,Src,Off}|Ld_Hp] }}.

st_sp_dep(Stores,Off) ->
    sp_dep(Stores,Off,[],[]).

ld_sp_dep(Loads,Off) ->
    sp_dep(Loads,Off,[],[]).

sp_dep([],Off,Dep,Indep) -> {Dep,Indep};
sp_dep([{N,Off}|Xs],Off,Dep,Indep) ->
    sp_dep(Xs,Off,[N|Dep],Indep);
sp_dep([X|Xs],Off,Dep,Indep) ->
    sp_dep(Xs,Off,Dep,[X|Indep]).

sp_dep_only(Stores,Off) ->
    [ N || {N,Off0} <- Stores, Off == Off0 ].

% Dependences from heap stores to heap loads.
% *** UNFINISHED ***
% - but works
% This is somewhat subtle:
% - a heap load can only bypass a heap store if we KNOW it won't
%   load the stored value
% - unfortunately, we do not know the relationships between registers
%   at this point, so we can't say that store(p+4) is independent of
%   load(q+0).
%    (OR CAN WE? A bit closer reasoning might show that it's possible?)
% - We can ONLY say that st(p+c) and ld(p+c') are independent when c /= c'
%
% (As said before, it might be possible to lighten this restriction?)

hp_dep_only([],Reg,Off) -> [];
hp_dep_only([{N,Reg,Off_1}|Xs],Reg,Off) when Off_1 =/= Off ->
    hp_dep_only(Xs,Reg,Off);
hp_dep_only([{N,_,_}|Xs],Reg,Off) ->
    [N|hp_dep_only(Xs,Reg,Off)].
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Control dependences:
% - add dependences so that
%   * branches are performed in order
%   * unsafe operations are 'fenced in' by surrounding branches
% - note: basic blocks are split at serializing instructions (such as
%   calls) prior to this point

%cd(BB,DG) ->
%    cd(BB,DG,none,[]).

%cd([],DG,PrevBr,PrevUnsafe) ->
%    DG;
%cd([{N,I}|Xs],DG,PrevBr,PrevUnsafe) ->
%%    case cd_type(I) of
%	{branch,Ty} ->
%	    NewDG = cd_branch_deps(PrevBr,PrevUnsafe,N,Ty,DG),
%	    cd(Xs,NewDG,{N,Ty},[]);
%	{unsafe,Ty} ->
%	    NewDG = cd_unsafe_deps(PrevBr,N,Ty,DG),
%	    cd(Xs,NewDG,PrevBr,[{N,Ty}|PrevUnsafe]);
%	{other,Ty} ->
%	    cd(Xs,DG,PrevBr,PrevUnsafe)
%    end.

% Is the operation a branch, an unspeculable op or something else?
%
% Returns
%   {branch,BranchType}
%   {unsafe,OpType}
%   {other,OpType}
%
% *** UNFINISHED ***

%cd_type(I) ->
%    case hipe_sparc:type(I) of
%	goto ->
%	    {branch,uncond};
%	br ->
%	    {branch,cond};
%	b ->
%	    {branch,cond};
%	call_link ->
%	    {branch,call};
%	jmp_link ->
	%    {branch,call};
%	jmp ->
%	    {branch,call};
%	load ->
%	    {unsafe,load};
%	store ->
%	    {unsafe,load};
%	T ->
%	    {other,T}
%    end.

% add dependences to keep order of branches + unspeculable ops:

%cd_branch_deps(PrevBr,PrevUnsafe,N,Ty,DG) ->
%    DG1 = case PrevBr of
%	      none ->
%		  DG;
%	      {Br,BrTy} ->
%		  dep_arc(Br,hipe_target_machine:br_br_latency(BrTy,Ty),N,DG)
%	  end,
%    deps_to_unsafe(PrevUnsafe,N,Ty,DG1).

%deps_to_unsafe([],N,Ty,DG) -> DG;
%deps_to_unsafe([{M,UTy}|Us],N,Ty,DG) ->
%    deps_to_unsafe(Us,N,Ty,
%		   dep_arc(M,hipe_target_machine:unsafe_to_br_latency(UTy,Ty),N,DG)).

% add dependences so that unsafe operations can't be speculated:

%cd_unsafe_deps(none,N,Ty,DG) ->
%    DG;
%cd_unsafe_deps({Br,BrTy},N,Ty,DG) ->
%    dep_arc(Br,hipe_target_machine:br_to_unsafe_latency(BrTy,Ty),N,DG).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

max(X,Y) ->
    if
	X < Y -> Y;
	true -> X
    end.

% Note: at present, we regard ALL branches as serializing. This means
% we do not have to consider control dependences at this time.

serializing_instr(Instr) ->
    case hipe_sparc:type(Instr) of
	call_link -> true;
	jmp_link -> true;
	jmp -> true;
	br -> true;
	b -> true;
	goto -> true;
	_ -> false
    end.

def_use(Instr) ->
    { hipe_sparc:defines(Instr), hipe_sparc:uses(Instr) }.

