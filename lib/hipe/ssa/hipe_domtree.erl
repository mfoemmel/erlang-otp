%%%----------------------------------------------------------------------
%%% File    : hipe_domtree.erl
%%% Author  : Christoffer Vikström (chvi3471@student.uu.se)
%%%           Daniel Deogun        (dade4543@student.uu.se)
%%            Jesper Bengtsson     (jebe8371@student.uu.se)
%%% Purpose : Containes functions for creating and handling dominator trees.
%%% Created : 18 Mar 2002 by 
%%%----------------------------------------------------------------------

-module(hipe_domtree).

-export([create/1,
	 getChildren/2,
	 getIDom/2,
	 getRoot/1,
	 getSize/1,
	 pp/1,
	 dominates/3]).

-include("hipe_domtree.hrl").

-define(hash, hipe_hash).

-record(workDataCell, {dfnum = 0, dfparent = none, semi = none, 
		       ancestor = none, best = none, samedom = none, 
		       bucket = []}).




%%>----------------------------------------------------------------------<
%  Procedure : empty/0
%  Purpose   : Creates an empty dominator tree.
%  Arguments : The root node
%  Return    : A dominator tree
%  Notes     : 
%%>----------------------------------------------------------------------<
empty(Node) ->
    #domTree{root = Node,
	     size = 0,
	     nodes = ?hash:empty()}.


%%>----------------------------------------------------------------------<
%  Procedure : createNode/2
%  Purpose   : Creates a new node and inserts it into the dominator tree. 
%  Arguments : Node    - The new node
%              DomTree - The target dominator tree
%  Return    : A dominator tree
%  Notes     : 
%%>----------------------------------------------------------------------<
createNode(Node, DomTree) ->
    DomTree2 = setNodes(?hash:update(Node, {none, []},
				     getNodes(DomTree)), DomTree),
    incSize(DomTree2).


%%>----------------------------------------------------------------------<
%  Procedure : getNode/2
%  Purpose   : Returns a specific node in the dominator tree.
%  Arguments : Node - The new node
%              DomTree - The target dominator tree
%  Return    : Node
%  Notes     : 
%%>----------------------------------------------------------------------<
getNode(Node, DomTree) ->
    case ?hash:lookup(Node, getNodes(DomTree)) of
	{found, Data} ->
	    Data;
	not_found -> 
	    not_found
    end.


%%>----------------------------------------------------------------------<
%  Procedure : getNodes/1
%  Purpose   : Retrieves the nodes structures from a dominator tree.
%  Arguments : DomTree - The target dominator tree
%  Return    : A hashmap containing the nodes of the dominator tree.
%  Notes     : 
%%>----------------------------------------------------------------------<
getNodes(DomTree) when record(DomTree, domTree) ->
    DomTree#domTree.nodes.


%%>----------------------------------------------------------------------<
%  Procedure : setNodes/2
%  Purpose   : Replaces the set of nodes in a dominator tree with a 
%              new set of nodes.
%  Arguments : Nodes   - The new set of nodes
%              DomTree - The target dominator tree
%  Return    : DomTree
%  Notes     : 
%%>----------------------------------------------------------------------<
setNodes(Nodes, DomTree) when record(DomTree, domTree) ->
    DomTree#domTree{nodes = Nodes}.


%%>----------------------------------------------------------------------<
%  Procedure : setSize/2
%  Purpose   : Sets the size of the dominator tree, i.e. the number of 
%              nodes in it.
%  Arguments : Size    - The new size of the target dominator tree
%              DomTree - The target dominator tree
%  Return    : A dominator tree
%  Notes     : 
%%>----------------------------------------------------------------------<
setSize(Size, DomTree) when record(DomTree, domTree) ->
    DomTree#domTree{size = Size}.


%%>----------------------------------------------------------------------<
%  Procedure : incSize/1
%  Purpose   : Increases the size of the dominator tree with one.
%  Arguments : DomTree - The target dominator tree
%  Return    : DomTree
%  Notes     : 
%%>----------------------------------------------------------------------<
incSize(DomTree) when record(DomTree, domTree) ->
    Size = getSize(DomTree),
    setSize(Size + 1, DomTree).


%%>----------------------------------------------------------------------<
%  Procedure : get IDom/2
%  Purpose   : Retrieves the immediate dominators of a node in the 
%              dominator tree.
%  Arguments : Node    - The new node
%              DomTree - The target dominator tree
%  Return    : The immediate dominator
%  Notes     : 
%%>----------------------------------------------------------------------<
getIDom(Node, DomTree) ->
    case getNode(Node, DomTree) of
	not_found ->
	    [];
	{IDom, _} ->
	    IDom
    end.


%%>----------------------------------------------------------------------<
%  Procedure : getChildren/2
%  Purpose   : Retrieves the children of a node in the dominator tree.
%  Arguments : Node    - The new node
%              DomTree - The target dominator tree
%  Return    : [children]
%  Notes     : 
%%>----------------------------------------------------------------------<
getChildren(Node, DomTree) ->
    case getNode(Node, DomTree) of
	not_found ->
	    [];
	{_, Children} ->
	    Children
    end.


%%>----------------------------------------------------------------------<
%  Procedure : getSize/1
%  Purpose   : Retrieves the size of a dominator tree.
%  Arguments : DomTree - The target dominator tree
%  Return    : Number
%  Notes     : 
%%>----------------------------------------------------------------------<
getSize(DomTree) ->
    DomTree#domTree.size.


%%>----------------------------------------------------------------------<
%  Procedure : getRoot/2
%  Purpose   : Retrieves the number of the root node in the dominator tree.
%  Arguments : DomTree - The target dominator tree
%  Return    : Number
%  Notes     : 
%%>----------------------------------------------------------------------<
getRoot(DomTree) ->
    DomTree#domTree.root.


%%>----------------------------------------------------------------------<
%  Procedure : addChild/3
%  Purpose   : Inserts a new node as a child to another node in the dominator
%              tree.
%  Arguments : Node    - The old node that should get a new child
%              Child   - The new child node
%              DomTree - The target dominator tree
%  Return    : DomTree
%  Notes     : 
%%>----------------------------------------------------------------------<
addChild(Node, Child, DomTree) ->
    {IDom, Children} = case getNode(Node, DomTree) of
			   not_found ->
			       {none,[]};
			   Tuple ->
			       Tuple
		       end,
    Temp = lists:member(Child, Children),
    Nodes = case Temp of 
		true ->
		    getNodes(DomTree);
		false ->
		    ?hash:update(Node, {IDom, [Child|Children]},
				 getNodes(DomTree))
	    end,

    setNodes(Nodes, DomTree).


%%>----------------------------------------------------------------------<
%  Procedure : setIDom/3
%  Purpose   : Sets the immediate domminator of a node in the domminator tree.
%  Arguments : Node    - The node whose immediate domminator we are seting
%              IDom    - The immediate domminator
%              DomTree - The target dominator tree
%  Return    : DomTree
%  Notes     : Is used to build the dominator tree.
%%>----------------------------------------------------------------------<
setIDom(Node, IDom, DomTree) ->
    DomTree1 = case getNode(Node, DomTree) of
		   not_found ->
		       createNode(Node, DomTree);
		   _ ->
		       DomTree
	       end,
    DomTree2 = addChild(IDom, Node, DomTree1),
    {_, Children} = getNode(Node, DomTree2),
    setNodes(?hash:update(Node,
			  {IDom, Children},
			  getNodes(DomTree2)),
	     DomTree2).


%%>----------------------------------------------------------------------<
%  Procedure : lookup
%  Purpose   : This function is used as a wrapper for the lookup function
%              defined in the hash library. The function retrieves a 
%              particular element (defined by Field) stored in a workDataCell
%              in the hash table (defined by Table).
%  Arguments : Field - Value defined in the workDataCell record
%              Key   - Value used as a key in the hash table
%              Table - Hashtable storing workDataCells
%  Return    : A value defined in the workDataCell record
%  Notes     : 
%%>----------------------------------------------------------------------<
lookup({Field, Key}, Table) ->
    WD = lookup(Key, Table),
    lookup(Field, WD);

lookup(Node, DomTree) when record(DomTree, domTree) ->
    case ?hash:lookup(Node, getNodes(DomTree)) of
	{found, Data} ->
	    Data;
	not_found ->
	    {none, []}
    end;

lookup(Field, WD) when record(WD, workDataCell) ->
    case Field of
	dfnum    -> WD#workDataCell.dfnum; 
	dfparent -> WD#workDataCell.dfparent; 
	semi     -> WD#workDataCell.semi;
	ancestor -> WD#workDataCell.ancestor;
	best     -> WD#workDataCell.best;
	samedom  -> WD#workDataCell.samedom;
	bucket   -> WD#workDataCell.bucket;
       	_Other    -> {error, {idom, lookup, 2}}
    end;

lookup(N, Table) when integer(N) ->
    case ?hash:lookup(N, Table) of
	{found, Data} ->
	    Data;
	not_found ->
	    #workDataCell{}
    end.
    
 	
%%>----------------------------------------------------------------------<
%  Procedure : insert/3
%  Purpose   : This function is used as a wrapper for the insert function 
%              defined in the hash library.
%  Arguments : Key   - Value used as a key in the hash table
%              Field - Value defined in the workDataCell record.
%              Value - The new value that should replace the old in the table
%              Table - Hashtable storing workDataCells
%  Return    : Hashtable
%  Notes     : 
%%>----------------------------------------------------------------------<
insert(Key, List, Table) ->
    ?hash:insert(Key, update(List, lookup(Key, Table)), Table).


%%>----------------------------------------------------------------------<
%  Procedure : update
%  Purpose   : This function is used as a wrapper for the update function 
%              defined in the hash library. The main purpose of the update 
%              function is therefore change a particular cell in the hash
%              table (Table) to the value given as an argument (Value).
%  Arguments : Key   - Value used as a key in the hash table
%              Field - Value defined in the workDataCell record.
%              Value - The new value that should replace the old in the table
%              Table - Hash table storing workDataCells
%  Return    : HashTable               
%  Notes     : 
%%>----------------------------------------------------------------------<
update(Key, {Field, Value}, Table) ->
    ?hash:update(Key, updateCell(Value, Field,
				     lookup(Key, Table)), Table);

update(Key, List, Table) ->
    ?hash:update(Key, update(List, lookup(Key, Table)), Table).

update([{Field, Value} | T], WD) -> 
    update(T, updateCell(Value, Field, WD));

update([], WD) -> WD.

updateCell(Value, Field, WD) ->
    case Field of
	dfnum   -> WD#workDataCell{dfnum   = Value}; 
	dfparent-> WD#workDataCell{dfparent= Value}; 
	semi    -> WD#workDataCell{semi    = Value}; 
	ancestor-> WD#workDataCell{ancestor= Value}; 
	best    -> WD#workDataCell{best    = Value}; 
	samedom -> WD#workDataCell{samedom = Value}; 
	bucket  -> WD#workDataCell{bucket  = Value};
	_Other   -> {error, {idom, update, 3}} 
    end.


%%>----------------------------------------------------------------------<
%  Procedure : create/1
%  Purpose   : Creates a complete dominator tree given a CFG.
%  Arguments : CFG - a Control Flow Graph representation
%  Return    : A dominance tree
%  Notes     : 
%%>----------------------------------------------------------------------<
create(CFG) ->
    PredMap = hipe_gen_cfg:pred_map(CFG),
    {WorkData, DFS, N} = dfs(CFG),
    {DomData, WorkData2} = getIdoms(CFG, empty(hipe_gen_cfg:start_label(CFG)), WorkData, N, 
				    DFS, PredMap),
    finalize(WorkData2, DomData, 1, N, DFS).


%%>----------------------------------------------------------------------<
%  Procedure : dfs/1
%  Purpose   : The main purpose of this function is to traverse the CFG in
%              a depth first order. It is aslo used to initialize certain 
%              elements defined in a workDataCell.
%  Arguments : CFG - a Control Flow Graph representation
%  Return    : A hash table (WorkData) and the total number of elements in
%              the CFG.
%  Notes     : 
%%>----------------------------------------------------------------------<
dfs(CFG) ->
    {WorkData, DFS, N} = dfs(CFG, hipe_gen_cfg:start_label(CFG), 
			none, 1, ?hash:empty(), ?hash:empty()),
    {WorkData, DFS, N-1}.

dfs(CFG, Node, Parent, N, WorkData, DFS) ->
    case lookup({dfnum, Node}, WorkData) of
	0 -> 	  
	    WorkData2 = update(Node, [{dfnum, N}, {dfparent, Parent}, 
				      {semi, Node}, {best, Node}], WorkData),
	    DFS2 = ?hash:update(N, Node, DFS),
	    dfsTraverse(hipe_gen_cfg:succ(CFG, Node), CFG, Node, 
			N + 1, WorkData2, DFS2);
	
	_ -> {WorkData, DFS, N}
    end.

%%>----------------------------------------------------------------------<
%  Procedure : dfsTraverse/6
%  Purpose   : This function acts as a help function for the dfs algorithm
%              in the sence that it traverses a list of nodes given by the 
%              CFG. 
%  Arguments : Node     - The first element in the node list
%              SuccLst  - The remainder of the node list
%              CFG      - Control Flow Graph representation
%              Parent   - Node representing the parent of the Node defined
%                         above.
%              N        - The total number of processed nodes.
%              WorkData - Hashtable consisting of workDataCells
%  Return    : An updated version of the hash table (WorkData) and the 
%              total number of nodes processed.
%  Notes     : 
%%>----------------------------------------------------------------------<
dfsTraverse([Node|T], CFG, Parent, N, WorkData, DFS) ->
    {WorkData2, DFS2, N2} = dfs(CFG, Node, Parent, N, WorkData, DFS),
    dfsTraverse(T, CFG, Parent, N2, WorkData2, DFS2);

dfsTraverse([], _, _, N, WorkData, DFS) -> {WorkData, DFS, N}.


%%>----------------------------------------------------------------------<
%  Procedure : getIdoms/6
%  Purpose   : The purpose of this function is to compute the immediate
%              dominators. This is accomplished by traversing the CFG nodes
%              by their depth first number in a bottom up manner. That is, 
%              the nodes are processed in a backward order (highest to 
%              lowest number).
%  Arguments : CFG      - Control Flow Graph representation
%              Domdata  - Hashtable consisting of domTree cells
%              WorkData - Hashtable consisting of workDataCells
%              Index    - The index used for retrieving the node to be 
%                         processed
%  Return    : An updated version of the hash tables DomData and WorkData
%  Notes     : 
%%>----------------------------------------------------------------------<
getIdoms(CFG, DomData, WorkData, Index, DFS, PredMap) 
  when integer(Index), Index > 1 ->
    Node = lookup(Index, DFS),
    PredLst = hipe_gen_cfg:pred(PredMap, Node),
    Par = lookup({dfparent, Node}, WorkData),
    DfNumN = lookup({dfnum, Node}, WorkData),
    {S, WorkData2} = getSemiDominator(PredLst, DfNumN, Par, WorkData),
    WorkData3 = update(Node, {semi, S}, WorkData2),
    OldBucket = lookup({bucket, S}, WorkData3),
    WorkData4 = update(S, {bucket, [Node | OldBucket]}, WorkData3),
    WorkData5 = linkTrees(Par, Node, WorkData4),
    {WorkData6, DomData2} = filterBucket(lookup({bucket, Par}, WorkData5), 
					 Par, WorkData5, DomData),
    WorkData7 = update(Par, {bucket, []}, WorkData6),
    getIdoms(CFG, DomData2, WorkData7, Index - 1, DFS, PredMap);

getIdoms(_, DomData, WorkData, 1, _, _) -> {DomData, WorkData}.
 

%%>----------------------------------------------------------------------<
%  Procedure : getSemiDominator/4
%  Purpose   : The main purpose of this algorithm is to compute the semi 
%              dominator of the node Node based on the Semidominator Theorem
%  Arguments : Pred     - Predecessor of the node Node
%              PredLst  - The remainder of the predecessor list of node Node
%              Node     - Node in the CFG
%              S        - Parent of node Node (depth first parent)
%              WorkData - Hashtable consisting of workDataCells
%  Return    : A tuple containing the semidominator and an updated version
%              of the hash table WorkData.
%  Notes     :
%%>----------------------------------------------------------------------<
getSemiDominator([Pred | PredLst], DfNumChild, S, WorkData) ->
    {Sp, WorkData3} = case lookup({dfnum, Pred}, WorkData) =< DfNumChild of
			  true  -> 
			      {Pred, WorkData};
			  false ->  
			      {AncLowSemi, WorkData2} = 
				  getAncestorWithLowestSemi(Pred, WorkData),
				   
			      {lookup({semi, AncLowSemi}, WorkData2), 
			       WorkData2}
		      end,
    S2 = case lookup({dfnum, Sp}, WorkData3) < 
	      lookup({dfnum, S}, WorkData3) of
	     true  -> Sp;
	     false -> S
	 end,
    getSemiDominator(PredLst, DfNumChild, S2, WorkData3);

getSemiDominator([], _, S, WorkData) -> {S, WorkData}.


%%>----------------------------------------------------------------------<
%  Procedure : getAncestorWithLowestSemi/2
%  Purpose   : The main purpose of this function is to retrieve the ancestor 
%              of a node with the lowest depth first number (semi). The
%              function is also using path compression, i.e. it remembers the
%              best node (the one with the lowest semi number) and hence the
%              algorithm is only processing the minimal number of nodes.
%  Arguments : Node     - Node in the tree
%              WorkData - Hashtable consisting of workDataCells
%  Return    : A node (the one with the lowest semi) and an updated version
%              of the hash table WorkData.
%  Notes     : 
%%>----------------------------------------------------------------------<
getAncestorWithLowestSemi(Node, WorkData) ->
    Best = lookup({best, Node}, WorkData),
    case lookup({ancestor, Node}, WorkData) of
	none -> {Best, WorkData};
	A -> 
	    case lookup({ancestor, A}, WorkData) of
		none -> 
		    {Best, WorkData};
		_ -> 
		    {B, WorkData2} = 
			 getAncestorWithLowestSemi(A, WorkData),
		    AncA = lookup({ancestor, A}, WorkData2),
		    WorkData3 = update(Node, {ancestor, AncA}, WorkData2),
		    DfSemiB = lookup({dfnum, lookup({semi, B}, WorkData3)},
				     WorkData3),
		    BestN = lookup({best, Node}, WorkData3),
		    SemiB = lookup({semi, BestN}, WorkData3),
		    DfSemiBestN = lookup({dfnum, SemiB}, WorkData3),
		    case DfSemiB < DfSemiBestN of
			true  ->
			    {B, update(Node, {best, B}, WorkData3)};
			false -> 
			    {BestN, WorkData3}
		    end
	    end
    end.


%%>----------------------------------------------------------------------<
%  Procedure : linkTrees/3
%  Purpose   : The main purpose of this function is to combine two trees
%              into one (accomplished by setting the ancestor for node 
%              Node to Parent). The algorithm is also updating the best field
%              in the workDataCell for node Node to the value of itself.
%  Arguments : Parent   - The parent of the node Node.
%              Node     - The node to process
%              WorkData - Hashtable consisting of workDataCells
%  Return    : An updated version of hash table WorkData
%  Notes     : 
%%>----------------------------------------------------------------------<
linkTrees(Parent, Node, WorkData) ->
    update(Node, [{ancestor, Parent}, {best, Node}], WorkData).


%%>----------------------------------------------------------------------<
%  Procedure : filterBucket/4 
%  Purpose   : The purpose of this algorith is to compute the dominator of
%              the node Node by utilizing the first clause of the Dominator
%              Theorem. If the first clause of the theorem doesn't apply 
%              then the computation of that particular node is deferred to
%              a later stage (see finalize).
%  Arguments : Node     - Node in the CFG
%              Bucket   - The remainder of the list of nodes that need to be 
%                         computed.
%              Parent   - The parent of the nodes in the list [Node | Bucket]
%              WorkData - Hashtable consisting of workDataCells
%              DomData  - a hash table consisting of domTree cells.
%  Return    : An updated version of the hash tables WorkData and DomData
%  Notes     : 
%%>----------------------------------------------------------------------<
filterBucket([Node | Bucket], Parent, WorkData, DomData) ->
    {Y, WorkData2} = getAncestorWithLowestSemi(Node, WorkData),
    {WorkData3, DomData2} = 
	case lookup({semi, Y}, WorkData2) == 
	     lookup({semi, Node}, WorkData2) of
	    true  -> {WorkData2, setIDom(Node, Parent, DomData)};
	    false -> {update(Node, {samedom, Y}, WorkData2), DomData}
	end,
    filterBucket(Bucket, Parent, WorkData3, DomData2);

filterBucket([], _, WorkData, DomData) -> {WorkData, DomData}.	     


%%>----------------------------------------------------------------------<
%  Procedure : finalize/5
%  Purpose   : This algorithm finishes up the second clause of the Dominator
%              Theorem. Hence, the main purpose of this function is therefore
%              to update the dominator tree with the nodes that were deferred
%              in the filterBucket algorithm.
%  Arguments : WorkData - Hashtable consisting of workDataCells
%              Domdata  - Hashtable consisting of domTree cells
%              N        - The index used for retrieving the node to be 
%                         processed
%              Max      - Maximum node index
%  Return    : An updated version of the hash table DomData
%  Notes     : 
%%>----------------------------------------------------------------------<
finalize(WorkData, DomData, N, Max, DFS) when N =< Max ->
    Node = lookup(N, DFS),
    case lookup({samedom, Node}, WorkData) of
	none     -> finalize(WorkData, DomData, N + 1, Max, DFS);
	SameDomN -> IdomSameDomN = getIDom(SameDomN, DomData),
		    DomData2 = setIDom(Node, IdomSameDomN, DomData),
		    finalize(WorkData, DomData2, N + 1, Max, DFS)
    end;

finalize(_, DomData, _, _, _) -> DomData.   

%%>----------------------------------------------------------------------<
%  Procedure : dominates/3
%  Purpose   : checks wheter Node1 dominates Node2 with respect to the
%              dominatortree DomTree
%  Arguments : Node1 the possible dominator, Node2 which might be dominated 
%              and DomTree  - the target dominator tree.
%  Notes     : Relies on lists:any to return false when the a list is empty
%%>----------------------------------------------------------------------<     
dominates(Node1, Node1, DomTree) ->
  true;
	
dominates(Node1, Node2, DomTree) ->
  Children = getChildren(Node1, DomTree),
  lists:any(fun(X)-> dominates(X,Node2,DomTree) end, Children).
		


%%>----------------------------------------------------------------------<
%  Procedure : pp/1
%  Purpose   : Pretty Printing a dominator tree.
%  Arguments : DomTree  - the target dominator tree.
%  Notes     : Uses pp/2 and pp_children to perform its task.
%%>----------------------------------------------------------------------<     
pp(DomTree) ->
    io:format("Domtree:\nRoot: ~w\nSize: ~w\n", [getRoot(DomTree),
						 getSize(DomTree)]),
    pp(getRoot(DomTree), DomTree).

pp(N, DomTree) ->
    case getNode(N, DomTree) of
	{IDom, Children} ->
     	    io:format("Node: ~w\n\tIDom: ~w\n\tChildren: ~w\n\n",
		      [N, IDom, Children]),
	    pp_children(Children, DomTree);
	not_found ->
	    failed
    end.


pp_children([Child|T], DomTree) ->
    pp(Child, DomTree),
    pp_children(T, DomTree);

pp_children([], _) -> ok.
