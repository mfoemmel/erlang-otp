%%%----------------------------------------------------------------------
%%% File    : df.erl
%%% Author  : 
%%% Purpose : 
%%% Created : 18 Mar 2002 by 
%%%----------------------------------------------------------------------

-module(hipe_df).
-export([get/2,
	 make/2]).

-define(hash,hipe_hash).

%%>----------------------------------------------------------------------<
%  Procedure : create
%  Purpose   : This function creates an empty instance of DF. It is 
%              represented by a hash table.
%  Arguments : N - The number of Dominance Frontiers
%  Notes     : 
%%>----------------------------------------------------------------------<
create() ->
    ?hash:empty().

%%>----------------------------------------------------------------------<
%  Procedure : add
%  Purpose   : This function adds Node to N in DF.
%  Arguments : N    - The value being inserted
%              Node - The node getting the value
%              DF   - The Dominance Frontiers
%  Return    : DF
%  Notes     : If Node already exists at position N, it is not added again.
%%>----------------------------------------------------------------------<
add(N, Node, DF) ->
    case ?hash:lookup(N, DF) of
	not_found ->
	    ?hash:update(N, [Node], DF);
	{found, DFList} ->
	    case lists:member(Node, DFList) of
		true ->
		    DF;
		false ->
		    ?hash:update(N, [Node|DFList], DF)
	    end
    end.


%%>----------------------------------------------------------------------<
%  Procedure : get
%  Purpose   : This function gets the Dominance Frontier for Node
%  Arguments : Node - The node which Dominance Frontier we request
%              DF - The Dominance Frontiers
%  Return    : 
%  Notes     : 
%%>----------------------------------------------------------------------<
get(Node, DF) ->
    case ?hash:lookup(Node, DF) of
	not_found ->  [];
	{found, List} -> List
    end.


%%>----------------------------------------------------------------------<
%  Procedure : make
%  Purpose   : This function calculates the Dominance Frontiers from
%              a CFG and a Dominantor Tree.
%  Arguments : SuccMap - The successor map of the CFG we are working with
%              DomTree - The dominance tree of the CFG.
%  Notes     : DomTree must actually be the dominance tree of the CFG.
%%>----------------------------------------------------------------------<
make(SuccMap, DomTree) ->
    make(hipe_domtree:getRoot(DomTree), SuccMap, DomTree, create()).

make(Node, SuccMap, DomTree, DF) ->

    Children = hipe_domtree:getChildren(Node, DomTree),
    Succ = hipe_gen_cfg:succ(SuccMap, Node),
    DF1 = checkIDomList(Succ, Node, DomTree, DF),
    makeDFChildren(Children, Node, SuccMap, DomTree, DF1).


%%>----------------------------------------------------------------------<
%  Procedure : makeDFChildren
%  Purpose   : This function calculates the dominance frontiers of the
%              children of the parent and adds the nodes in these
%              dominance frontiers who are not immediate dominantors of
%              the parent to parents dominance frontier.
%  Arguments : ChildList - The list of children that the function traverses
%              Parent - The parent of the children
%              SuccMap - The successor map of the CFG
%              DomTree - The dominantor tree of the CFG
%              DF - The dominance frontiers so far
%  Notes     : 
%%>----------------------------------------------------------------------<
makeDFChildren([Child|T], Parent, SuccMap, DomTree, DF) ->
    DF1 = make(Child, SuccMap, DomTree, DF),
    DF2 = checkIDomList(get(Child, DF1), Parent, DomTree, DF1),
    makeDFChildren(T, Parent, SuccMap, DomTree, DF2);
makeDFChildren([], _, _, _, DF) -> DF.


%%>----------------------------------------------------------------------<
%  Procedure : checIDomList
%  Purpose   : Adds all the nodes in the list to the parents dominance
%              frontier who do not have parent as immediate dominator.
%  Arguments : NodeList - The list of nodes that the function traverses
%              Parent - The parent of the nodes
%              DomTree - Our dominator tree
%              DF - The dominance frontiers so far
%  Notes     : 
%%>----------------------------------------------------------------------<
checkIDomList([Node|T], Parent, DomTree, DF) ->
    DF1 = checkIDom(Node, Parent, DomTree, DF),
    checkIDomList(T, Parent, DomTree, DF1);
checkIDomList([], _, _, DF) -> DF.

						     
%%>----------------------------------------------------------------------<
%  Procedure : checkIdom
%  Purpose   : adds Node1 to Node2s dominance frontier if Node2 is not
%              Node1s immediate dominator.
%  Arguments : Node1 - a node
%              Node2 - another node
%              DomTree - the dominator tree
%              DF - the dominance frontier so far
%  Notes     : 
%%>----------------------------------------------------------------------<
checkIDom(Node1, Node2, DomTree, DF) ->
    case hipe_domtree:getIDom(Node1, DomTree) of
	Node2 ->
	    DF;
	none ->
	    DF;
	_ ->
	    add(Node2, Node1, DF)
    end.






