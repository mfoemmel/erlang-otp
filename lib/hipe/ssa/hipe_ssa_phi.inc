%%%----------------------------------------------------------------------
%%% File    : addphi.erl
%%% Author  : 
%%% Purpose : 
%%% Created : 5 Mar 2002 by Christoffer Vikstrom <chvi3471@rama.it.uu.se>
%%%----------------------------------------------------------------------
-export([place/2]).

%%>----------< Auxiliry Functions >----------<%%

%%----------------------------------------------------------------------
% Procedure : lookup/3 
% Purpose   : Wrapper for diffrent hashtables using lookup.
% Arguments : Key   - the hashtable key
%             Table - the hashtable
%             Type  - the type of hashtable. Can be: work, hasAlready or assMap
% Return    : The value at key place or for Type=work/hasAlready -> 0 and
%              for Type=assMap -> [].
% Notes     : 
%%----------------------------------------------------------------------
lookup(Key, Table, Type) -> 
    case Type of
	work ->
	    case ?hash:lookup(Key, Table) of
		not_found -> 0;
		{found, Other} -> Other
	    end;
	hasAlready ->
	    case ?hash:lookup(Key, Table) of
		not_found -> 0;
		{found, Other} -> Other
	    end;
	assMap -> 
	    case ?hash:lookup(Key, Table) of
		not_found -> [];
		{found, Other} -> Other
	    end;
	_Other ->
	    {error, {addphi, lookup, 3}}
    end.
    

%%----------------------------------------------------------------------
% Procedure : lookup/3 
% Purpose   : Wrapper for ?hash:lookup().
% Arguments : Key   - the hashtable key
%             Table - the hashtable
%             Value - the value
% Return    : Table
% Notes     : 
%%----------------------------------------------------------------------      
update(Key, Value, Table) -> ?hash:update(Key, Value, Table).



%%>----------< PlacePhi Algorithm >----------<%%

%%----------------------------------------------------------------------
% Procedure : place/2 
% Purpose   : Places phi at appropriate places in the CFG.
% Arguments : CFG - Control Flow Graph.
%             DF  - Dominance Frontier.
% Return    : CFG with phi functions.
% Notes     : 
%%----------------------------------------------------------------------
place(CFG, DF) ->
    PredMap = ?cfg:pred_map(CFG),
    AssMap = insertParams(CFG),
    AssMap2 = preProcessing(CFG, AssMap),
    VarList = ?hash:list(AssMap2),
    variableTraverse(CFG, DF, ?hash:empty(), ?hash:empty(), 
		     0, AssMap2, VarList, PredMap).
    

%%----------------------------------------------------------------------
% Procedure : insertParams/1 
% Purpose   : Inserts the parameters of the CFG into the AssMap.
% Arguments : CFG - Control Flow Graph
% Return    : AssMap - Assignment map.
% Notes     : 
%%----------------------------------------------------------------------
insertParams(CFG) ->
    StartLabel = ?cfg:start_label(CFG),
    Params = ?cfg:params(CFG),
    insertParams(Params, StartLabel, ?hash:empty()).

insertParams([Param | T], StartLabel, AssMap) ->
    insertParams(T, StartLabel, update(Param, [StartLabel], AssMap));

insertParams([], _, AssMap) -> AssMap.


%%----------------------------------------------------------------------
% Procedure : preProcessing/2
% Purpose   : Creates the assignment map.
% Arguments : CFG     - Control Flow Graph
%             AssMap  - Assignment map
% Return    : AssMap.
% Notes     : 
%%----------------------------------------------------------------------
preProcessing(CFG, AssMap) -> 
    traverseLabels(CFG, ?cfg:labels(CFG), AssMap).


%%----------------------------------------------------------------------
% Procedure : traverseLabels/3
% Purpose   : Traverses all labels and adds all assignments in the basic
%             block to the assignment map.
% Arguments : CFG    - Control Flow Graph
%             AssMap - Assignment Map
%             Label  - A label for a node
% Return    : AssMap. 
% Notes     : 
%%----------------------------------------------------------------------
traverseLabels(CFG, [Label|T], AssMap) ->
    Code = hipe_bb:code(?cfg:bb(CFG, Label)),
    NewVarList = getAssignments(Code),
    traverseLabels(CFG, T, updateAssMap(NewVarList, Label, AssMap)); 
traverseLabels(_, [], AssMap) -> AssMap. 


%%----------------------------------------------------------------------
% Procedure : getAssignments/1
% Purpose   : Retrieves all assigned variables in a basic block.
% Arguments : InstrLst - A list of instructions from a basic block.
%             VarList  - A list of variables.
% Return    : VarList.
% Notes     : This function may return a list containing duplicated elements
%%----------------------------------------------------------------------
getAssignments(InstrLst) -> getAssignments(InstrLst, []).
getAssignments([Instr | T], VarList) ->
    getAssignments(T, ?code:defines(Instr) ++ VarList);

getAssignments([], VarList) -> VarList.


%%----------------------------------------------------------------------
% Procedure : updateAssMap/3
% Purpose   : Updates the assignment map with. Each variable in the AssVar
%             list is inserted with the value Label.
% Arguments : Label  - a label of a node
%             AssVar - a variable that is assigned at Label
%             AssMap - Assignment map.
% Return    : AssMap.
% Notes     : 
%%----------------------------------------------------------------------
updateAssMap([AssVar|T], Label, AssMap) ->
    Lst = lookup(AssVar, AssMap, assMap),
    updateAssMap(T, Label, update(AssVar, [Label|Lst], AssMap));

updateAssMap([], _, AssMap) -> AssMap.    


%%----------------------------------------------------------------------
% Procedure : variableTraverse/4
% Purpose   : This function traverses all variables and adds phi functions 
%             at appropriate nodes.
% Arguments : CFG        - Control Flow Graph
%             DFMap      - Dominance Frontier Map
%             HasAlready - A map of which nodes that already has phi functions
%             Work       - 
%             IterCount  - Counter of how many itterations that has been done
%             AssMap     - Assignment map
%             VarLst     - Variable list that is traversed
%             PredMap    - A map of predecessors in the CFG
% Return    : CFG.
% Notes     : 
%%----------------------------------------------------------------------
variableTraverse(CFG, DFMap, HasAlready, Work, 
		 IterCount, AssMap, [{Var,_}|VarLst], PredMap) ->
    IterCount2 = IterCount + 1,	
    DefLst = lookup(Var, AssMap, assMap),
    {Work2, WorkLst2} = workListBuilder(DefLst, Work, [], IterCount2),
    {CFG2, HasAlready2, Work3} =  doWork(CFG, DFMap, HasAlready, 
					 Work2, IterCount2, WorkLst2,
					 Var, PredMap),   
    variableTraverse(CFG2, DFMap, HasAlready2, Work3, 
		     IterCount2, AssMap, VarLst, PredMap);

variableTraverse(CFG,_,_,_,_,_,[],_) -> CFG.


%%----------------------------------------------------------------------
% Procedure : workListBuilder/4
% Purpose   : Builds the worklist that the algorithm is working on.
% Arguments : Work       - 
%             WorkLst    - The worklist that is worked through
%             IterCount  - Counter of how many itterations that has been done
%             Node       - A node in the CFG
% Return    : 
% Notes     : 
%%----------------------------------------------------------------------
workListBuilder([Node|T], Work, WorkLst, IterCount) ->
    case lookup(Node, Work, work) of
	0 ->
	    Work2 = update(Node, IterCount, Work),
	    workListBuilder(T, Work2, [Node|WorkLst], IterCount);
	_ ->
	    Work2 = update(Node, IterCount, Work),
	    workListBuilder(T, Work2, [Node|WorkLst], IterCount)
	end;

workListBuilder([], Work, WorkLst, _IterCount) -> {Work, WorkLst}.


%%----------------------------------------------------------------------
% Procedure : doWork/8
% Purpose   : This procedure works itself through the worklist and checks
%             if a node needs a phi functions.
% Arguments : CFG        - Control Flow Graph
%             DFMap      - Dominance Frontier Map
%             HasAlready - A map of which nodes that already has phi functions
%             Work       - 
%             IterCount  - Counter of how many itterations that has been done
%             WorkLst    - The worklist that is worked through
%             Var        - Variable
%             PredMap    - A map of predecessors in the CFG
% Return    : (CFG, HasAlready, Work)
% Notes     : 
%%----------------------------------------------------------------------   
doWork(CFG, DFMap, HasAlready, Work, IterCount,
       [Node|WorkLst], Var, PredMap) ->
    DFofX = hipe_df:get(Node, DFMap),
    {CFG2, HasAlready2, Work2, WorkLst2} =
	checkPhiNeeds(CFG, DFofX, HasAlready, Work,
		      IterCount, WorkLst, Var, PredMap),
    doWork(CFG2, DFMap, HasAlready2, Work2,
	   IterCount, WorkLst2, Var, PredMap);

doWork(CFG, _, HasAlready, Work, _, [], _, _) ->
    {CFG, HasAlready, Work}.    
 

%%----------------------------------------------------------------------
% Procedure : checkPhiNeeds/8
% Purpose   : This function checks if a node needs a phi function and adds
%             one if its needed.
% Arguments : CFG        - Control Flow Graph
%             DFofX      - Dominance Frontier of a node
%             HasAlready - A map of which nodes that already has phi functions
%             Work       - 
%             IterCount  - Counter of how many itterations that has been done
%             WorkLst    - The worklist that is worked through
%             Var        - Variable
%             PredMap    - A map of predecessors in the CFG
% Return    : (CFG, HasAlready, Work, WorkLst)
% Notes     : 
%%----------------------------------------------------------------------
checkPhiNeeds(CFG, [Node|DFofX], HasAlready, Work,
	      IterCount, WorkLst, Var, PredMap) ->
    case lookup(Node, HasAlready, hasAlready) < IterCount of
	true ->
	    CFG2 = insertPhiCode(CFG, Node, Var, PredMap),
	    HasAlready2 = update(Node, IterCount, HasAlready),
	    case  lookup(Node, Work, work) < IterCount of
		true ->
		    Work2 = update(Node, IterCount, Work),
		    WorkLst2 = [Node|WorkLst],
		    checkPhiNeeds(CFG2, DFofX, HasAlready2, Work2, IterCount,
				  WorkLst2, Var, PredMap);
		false ->
		    checkPhiNeeds(CFG2, DFofX, HasAlready2, Work, 
				  IterCount, WorkLst, Var, PredMap)
	    end;
	false ->
	    checkPhiNeeds(CFG, DFofX, HasAlready, Work, IterCount, 
			  WorkLst, Var, PredMap)
    end;

checkPhiNeeds(CFG, [], HasAlready, Work, _, WorkLst, _, _) ->
    {CFG, HasAlready, Work, WorkLst}.	


%%----------------------------------------------------------------------
% Procedure : insertPhiCode/4
% Purpose   : 
% Arguments : CFG     - Control Flow Graph
%             Node    - A node
%             Var     - A variable
%             PredMap - A map of predecessors in the CFG
% Return    : CFG
% Notes     : 
%%----------------------------------------------------------------------
insertPhiCode(CFG, Node, Var, PredMap) ->
   BB = ?cfg:bb(CFG, Node),
   PredList = ?cfg:pred(PredMap, Node),
   Phi = ?code:mk_phi(Var, PredList),
   Code = [Phi | hipe_bb:code(BB)],
   ?cfg:bb_update(CFG, Node, hipe_bb:code_update(BB, Code)).





