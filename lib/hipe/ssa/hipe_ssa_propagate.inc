%%% -*- Erlang -*-
%% -*- erlang-indent-level: 2 -*-
%%----------------------------------------------------------------------
%% File    : hipe_ssa_propagate.inc
%% Created : 18 Mar 2002 by 
%% Author  : Christoffer Vikström, Daniel Deogun, and Jesper Bengtsson
%% Purpose : This file contains the function that propagates the assignments
%%           in the phi-functions up to their appropriate predecessor node.
%%----------------------------------------------------------------------

-export([propagate/1]).

%%>----------------------------------------------------------------------<
%% Procedure : lookup/2
%% Purpose   : Looks up an element in the hash map.
%% Arguments : Key   - The key of the element sought
%%             Table - The hash map.
%% Return    : A number
%% Notes     : 
%%>----------------------------------------------------------------------<

lookup(Key, Table) ->
    case ?hash:lookup(Key, Table) of
	{found, Other} ->
	    Other;
	not_found ->
	    {error, {?MODULE, lookup, 2}}
    end.

%%>----------------------------------------------------------------------<
%% Procedure : propagate/2
%% Purpose   : This function removes all phi functions and propagates all
%%             assignments up to the appropriate predecessor.
%% Arguments : CFG     - Control Flow Graph
%%             Node    - A node in the CFG
%% Return    : CFG
%% Notes     : Uses propagateChildren/3.
%%>----------------------------------------------------------------------<

propagate(CFG) ->
  propagate(?cfg:reverse_postorder(CFG), CFG).

propagate([Node|Left], CFG) ->
  BB = ?cfg:bb(CFG, Node),
  Code = hipe_bb:code(BB),
  {Phis, Code2} = getPhiFuncts(Code, []),
  BB2 = hipe_bb:code_update(BB, Code2),
  CFG2 = ?cfg:bb_update(CFG, Node, BB2),
  Pred = ?cfg:pred(?cfg:pred_map(CFG2), Node),
  CFG3 = propagatePhis(Pred, Node, Phis, CFG2),
  propagate(Left, CFG3);
propagate([], CFG) ->
  CFG.

%%>----------------------------------------------------------------------<
%% Procedure : propagatePhis/4
%% Purpose   : This function does the propagate work. It propagates the phi-
%%             functions up to a new node inserted between this Node and its
%%             coresponding predecessor.
%% Arguments : CFG     - Control Flow Graph
%%             Node    - A node in the CFG
%%             Pred    - A Predecessor to a node
%%             Phis    - A list of phi functions
%% Return    : CFG
%% Notes     : 
%%>----------------------------------------------------------------------<

propagatePhis([Pred|T], Node, Phis, CFG) ->
    CFG2 = insertFilterNode(Pred, Node, Phis, CFG),
    propagatePhis(T, Node, Phis, CFG2);
propagatePhis([], _, _, CFG) -> CFG.

%%>----------------------------------------------------------------------<
%% Procedure : insertFilterNode/4
%% Purpose   : This function inserts extra nodes between the current and
%%             its predecessors.
%% Arguments : CFG     - Control Flow Graph
%%             Node    - A node in the CFG
%%             Pred    - A Predecessor to a node
%%             Phis    - A list of phi functions
%% Return    : CFG
%% Notes     : 
%%>----------------------------------------------------------------------<

insertFilterNode(Pred, Node, Phis, CFG) ->
    GotoNode = ?code:mk_goto(Node),
    createFilterNode(Phis, Pred, Node, CFG, [GotoNode]).
   
%%>----------------------------------------------------------------------<
%% Procedure : createFilterNode/5
%% Purpose   : This function creates the filter node, i.e. the node between 
%%             the current and its predecessors.
%% Arguments : CFG     - Control Flow Graph
%%             Node    - A node in the CFG
%%             Pred    - A Predecessor to a node
%%             Phi     - A phi function instruction
%%             Result  - Ackumulativ parameter to store the result
%% Return    : CFG
%% Notes     : 
%%>----------------------------------------------------------------------<

createFilterNode([Phi|T], Pred, Node, CFG, Result) ->
    createFilterNode(T, Pred, Node, CFG, makePhiMove(Phi, Pred, Result));
createFilterNode([], Pred, Node, CFG, Result) ->
    case Result of
	[_] ->
	    CFG;
	_ ->
	    BB = hipe_bb:mk_bb(Result),
	    {CFG2, NewNode} = insertNewNode(CFG, BB),
	    BB2 = ?cfg:bb(CFG2, Pred),
	    [Branch|Code] = lists:reverse(hipe_bb:code(BB2)),
	    Code2 = lists:reverse([?code:redirect_jmp(Branch,
						      Node,
						      NewNode)|Code]),
	    BB3 = hipe_bb:code_update(BB2, Code2),
	    ?cfg:bb_update(CFG2, Pred, BB3)
    end.

%%>----------------------------------------------------------------------<
%% Procedure : makePhiMove
%% Purpose   : 
%% Arguments : Pred    - A Predecessor to a node
%%             Phi     - A phi function instruction
%%             Code   - Intermediate code
%% Return    : Code
%% Notes     : 
%%>----------------------------------------------------------------------<

makePhiMove(Phi, Pred, Code) ->
    Dst = ?code:phi_dst(Phi),
    Args = ?code:phi_getArgMap(Phi),
    case lookup(Pred, Args) of
	Dst ->
	    Code;
	Src -> 
	    case ?code:is_var(Dst) of
		true ->
		    case ?code:is_fvar(Src) of
			false ->
			    [?code:mk_mov(Dst, Src)|Code];
			true ->
			    [?code:mk_primop([Dst],unsafe_tag_float,[Src])|
			     Code]
		    end;
		false ->
		    case ?code:is_var(Src) of
			false ->
			    [?code:mk_fmov(Dst, Src)|Code];
			true ->
			    [?code:mk_primop([Dst],conv_to_float,[Src])|Code]
		    end
	    end
    end.

%%>----------------------------------------------------------------------<
%% Procedure : insertNewNode/2
%% Purpose   : This function inserts a new node (basic block) in the CFG.
%% Arguments : CFG   - Control Flow Graph
%%             BB    - Basic Block
%%             Label - A node label from the CFG
%% Return    : (CFG, Label)
%% Notes     : 
%%>----------------------------------------------------------------------<

insertNewNode(CFG, BB) ->
    {LMin, LMax} = ?cfg:label_range(CFG),
    NewLabel = LMax + 1,
    CFG2 = ?cfg:bb_add(CFG, NewLabel, BB),
    {?cfg:label_range_update(CFG2, {LMin, NewLabel}), NewLabel}.

%%>----------------------------------------------------------------------<
%% Procedure : getPhiFuncts/2
%% Purpose   : This function returns the list of phi-functions from a list
%%             of intermediate code instructions.
%% Arguments : Code  - Intermediate code
%%             List   - A list of Code
%%             Result - Ackumulativ parameter to store the result
%% Return    : 
%% Notes     : 
%%>----------------------------------------------------------------------<

getPhiFuncts([Code|T] = List, Result) ->
    case ?code:is_phi(Code) of
	true ->
	    getPhiFuncts(T, [Code|Result]);
	_ ->
	    {Result, List}
    end;
getPhiFuncts([], Result) -> {Result, []}.
