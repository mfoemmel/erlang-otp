%%% -*- Erlang -*-
%%%----------------------------------------------------------------------
%%% File    : ssa_rename.inc
%%% Author  : Christoffer Vikström
%%%           Daniel Deogun
%%%           Jesper Bengtsson
%%% Purpose : This file contains the function to perform the rename pass of
%%%           ssa.
%%% Created : 18 Mar 2002 by 
%%%----------------------------------------------------------------------
-export([rename/2]).
-include("../ssa/hipe_domtree.hrl").

%%>---------< Auxiliary Functions >----------<%%

%%>----------------------------------------------------------------------<
%  Procedure : createMap/0
%  Purpose   : Creates a hashmap
%  Arguments : NumberOfElements - the size of the hashmap.
%  Return    : A hash table
%  Notes     : 
%%>----------------------------------------------------------------------<
createMap() ->
    ?hash:empty().


%%>----------------------------------------------------------------------<
%  Procedure : lookup/2
%  Purpose   : Looks up an element in the hash map.
%  Arguments : Key   - The key of the element sought
%              Table - The hash map.
%  Return    : A number
%  Notes     : 
%%>----------------------------------------------------------------------<
lookup(Key, Table) ->
    case ?hash:lookup(Key, Table) of
	{found, Other} ->
	    Other;
	not_found ->
	    not_found
    end.


%%>----------------------------------------------------------------------<
%  Procedure : update/3
%  Purpose   : Updates Table and places Value with Key as hash key.
%  Arguments : Key   - the hash key
%              Value - the new value
%              Table - the hash map
%  Return    : Table
%  Notes     : 
%%>----------------------------------------------------------------------<
update(Key, Value, Table) ->
    ?hash:update(Key, Value, Table).



%%>---------< Renaming Functions >----------<%%

%%>----------------------------------------------------------------------<
%  Procedure : insertParams/1
%  Purpose   : Inserts the parameters of the cfg into the working hashmaps.
%  Arguments : CFG     - the target control flow graph.
%              Count   - the map that keeps count of variables naming.
%              Current - the current variable we are working on.
%  Return    : (CFG, Count, Current)
%  Notes     : 
%%>----------------------------------------------------------------------<
insertParams(CFG) ->
     Params = ?cfg:params(CFG),
    {Count, Current, Params2} = insertParams(Params, -1, createMap(), []),
    CFG2 = ?cfg:params_update(CFG, Params2),
    {CFG2, Count, Current}.
    
insertParams([Param|T], Count, Current, Result) ->
    {Count2, Current2} = updateIndexes(Count, Current, Param),
    insertParams(T, Count2, Current2, [lookup(Param, Current2)|Result]);

insertParams([], Count, Current, Result) ->
    {Count, Current, lists:reverse(Result)}.


%%>----------------------------------------------------------------------<
%  Procedure : rename/2
%  Purpose   : Renames all the variables in CFG according to the ssa 
%              convention
%  Arguments : CFG       - The CFG being translated
%              DomTree   - The dominator tree of CFG
%  Return    : CFG
%  Notes     : Uses rename/5.
%%>----------------------------------------------------------------------<
rename(CFG, DomTree) ->
    {CFG2, Count, Current} = insertParams(CFG),
    
    {CFG3, Count2} = rename(CFG2, ?cfg:start_label(CFG2), DomTree, 
			    Count, Current),
    ?cfg:var_range_update(CFG3, {0, Count2}).
    
rename(CFG, Node, DomTree, Count, Current) ->
    BB = ?cfg:bb(CFG, Node),
    Statements = hipe_bb:code(BB),
    {Statements2, Count2, Current2} = renameVars(Statements, Count, Current),
    CFG1 = ?cfg:bb_update(CFG, Node, hipe_bb:code_update(BB, Statements2)),
    Succ = ?cfg:succ(CFG1, Node),
    CFG2 = updateSuccPhi(Succ, Node, CFG1, Current2),
    Children = hipe_domtree:getChildren(Node, DomTree),
    childrenRename(Children, CFG2, DomTree, Count2, Current2).


%%>----------------------------------------------------------------------<
%  Procedure : childrenRename/5
%  Purpose   : renames all the nodes in a list according to the ssa standard
%  Arguments : ChildList - the list of nodes being renamed
%              CFG       - the CFG that the children are a part of
%              DomTree   - The dominator tree for the CFG
%              Count     - the index of the highest named variable so far
%              Current   - the current index of all variables encountered
%  Return    : {CFG, Count}
%  Notes     : 
%%>----------------------------------------------------------------------<
childrenRename([Child | T], CFG, DomTree, Count, Current) ->
    {CFG2, Count2} = rename(CFG, Child, DomTree, Count, Current),
    childrenRename(T, CFG2, DomTree, Count2, Current);

childrenRename([], CFG, _, Count, _) -> {CFG, Count}.


%%>----------------------------------------------------------------------<
%  Procedure : renameVars/3
%  Purpose   : Renames the variables in basic block
%  Arguments : Statements - the basic block
%              Count      - the index of the highest named variable so far
%              Current    - the current index of all variables encountered.
%  Return    : {Statements, Count, Current}
%  Notes     : Uses renameVars/4.
%%>----------------------------------------------------------------------<
renameVars(Statements, Count, Current) ->    
    renameVars(Statements, Count, Current, []).

renameVars([Statement | T], Count, Current, Result) ->
    Statement2 = renameUses(Statement, Current),
    {Statement3, Count2, Current2} = renameDefs(Statement2, Count, Current),
    renameVars(T, Count2, Current2, [Statement3 | Result]);

renameVars([], Count, Current, Result) -> 
    {lists:reverse(Result), Count, Current}.


%%>----------------------------------------------------------------------<
%  Procedure : renameUses/2
%  Purpose   : renames all the variable uses in a statment.
%  Arguments : Statement - the statement being renamed.
%              Current   - the current index of all variables encountered.
%  Return    : Satement
%  Notes     : 
%%>----------------------------------------------------------------------<   
renameUses(Statement, Current) ->
    case ?code:is_phi(Statement) of 
	true  -> Statement;
	false -> VarList = ?code:uses(Statement),
		 updateStatementUses(VarList, Statement, Current)
    end.


%%>----------------------------------------------------------------------<
%  Procedure : updateStatementUses/3
%  Purpose   : Traverses the variable list and renames all the instances
%              of a variable in the Statement uses to its current value.
%  Arguments : VarList   - the list of variables being updated.
%              Statement - the statement being updated.
%              Current   - the current index of all variables encountered.
%  Return    : An icode statement.
%  Notes     : 
%%>----------------------------------------------------------------------< 
updateStatementUses([Var | T] = _VarList, Statement, Current) ->
    NewVar = {colored, lookup(Var, Current)},
    Statement2 = ?code:subst_uses([{Var, NewVar}], Statement),
    updateStatementUses(T, Statement2, Current);

updateStatementUses([], Statement, _) -> unColor(Statement).


%%>----------------------------------------------------------------------<
%  Procedure : unColor/1
%  Purpose   : Remove all coloring of variables.     
%  Arguments : Statement - a statement to have its coloring removed.
%  Return    : Statement.
%  Notes     : Uses unColor/2.
%%>----------------------------------------------------------------------< 
unColor(Statement) ->
    unColor(?code:uses(Statement), Statement).

unColor([{colored, Var} = CVar | T], Statement) ->
    unColor(T, ?code:subst_uses([{CVar, Var}], Statement));
unColor([_|T], Statement) ->
    unColor(T, Statement);
unColor([], Statement) ->
    Statement.
		   

%%>----------------------------------------------------------------------<
%  Procedure : renameDefs/3
%  Purpose   : Renames all the definitons in Statement.
%  Arguments : Statement - the statement where the definitions are being
%              renamed.               
%              Count     - the index of the highest named variable so far
%              Current   - the current index of all variables encountered.
%  Return    : Statement
%  Notes     : 
%%>----------------------------------------------------------------------< 
renameDefs(Statement, Count, Current) ->
    VarList = ?code:defines(Statement),
    updateStatementDefs(VarList, Statement, Count, Current).


%%>----------------------------------------------------------------------<
%  Procedure : updateStatementDefs/4
%  Purpose   : traverses a variable list and exchanges all instances of
%              the variable in the statements definitions by its current
%              value.
%  Arguments : VariableList - the list of varibles being renamed
%              Statement - the statement whos definitions are being changed
%              Count - the index of the highest named variable so far
%              Current - the current index of all variables encountered
%  Return    : {Statement, Count, Current}
%  Notes     : Per Gustafsson:
%              I changed this function to update the statement only when
%              all substitutions are found
%%>----------------------------------------------------------------------< 
updateStatementDefs(Vars, Statement, Count, Current) ->
updateStatementDefs(Vars, Statement, Count, Current, []).

updateStatementDefs([Var| T], Statement, Count, Current, Acc) ->
  {Count2, Current2} = updateIndexes(Count, Current, Var),
  NewVar = 
    case ?code:is_var(Var) of
      true-> ?code:mk_var(Count2);
      _ -> case ?code:is_fvar(Var) of
	     true -> ?code:mk_fvar(Count2);
	     _ -> ?code:mk_reg(Count2)
	   end
    end,
  updateStatementDefs(T, Statement, Count2, Current2, [{Var, NewVar}|Acc]);

updateStatementDefs([], Statement, Count, Current, Acc) -> 
  Statement2 = ?code:subst_defines(Acc, Statement),
  {Statement2, Count, Current}.


%%>----------------------------------------------------------------------<
%  Procedure : updateIndexes/3
%  Purpose   : This function is used for updating the Count index and Current
%              hash table.
%  Arguments : Count    - An index variable
%              Current  - Hash table containg the current index for a 
%                         particular variable.
%              Variable - The variable that is used as key in the hash table.
%  Return    : A tuple containing an updated version of Count and Current.
%  Notes     : 
%%>----------------------------------------------------------------------< 
updateIndexes(Count, Current, Variable) ->
  case ?code:is_var(Variable) of
    true ->
      {Count + 1, update(Variable, ?code:mk_var(Count + 1), Current)};
    _ ->
      case ?code:is_fvar(Variable) of
	true ->
	  {Count + 1, update(Variable, ?code:mk_fvar(Count + 1), Current)};
	_ ->
	  {Count + 1, update(Variable, ?code:mk_reg(Count + 1), Current)}
      end
  end.


%%>----------------------------------------------------------------------<
%  Procedure : updateSuccPhi/4
%  Purpose   : This function is used for updating phi functions in a 
%              particular node's successors. That is, the function 
%              traverses the successor list of a node and updates the 
%              arguments in the phi function calls.
%  Arguments : Succ    - A successor to the node Parent.
%              T       - The remainder of the successor list
%              Parent  - The parent of the node Succ
%              CFG     - Control Flow Graph
%              Current - Hash table containg the current index for a 
%                        particular variable
%  Return    : An updated version of the CFG
%  Notes     : 
%%>----------------------------------------------------------------------< 
updateSuccPhi([Succ | T], Parent, CFG, Current) ->
    CFG2 = updatePhi(Succ, Parent, CFG, Current),
    updateSuccPhi(T, Parent, CFG2, Current);

updateSuccPhi([], _, CFG, _) -> CFG.


%%>----------------------------------------------------------------------<
%  Procedure : updatePhi/4
%  Purpose   : This function prepares for an update of a phi function call. 
%              That is, if an icode statement contains a phi function call 
%              then the number of predecessors are computed and the index 
%              of the parent in the predecessor list is used for computing
%              which variable in the argument list of the phi function call
%              that need to be updated.
%  Arguments : Node    - A node in the CFG
%              Parent  - The parent of the node Node in the dominator tree 
%              CFG     - Control Flow Graph
%              Current - Hash table containg the current index for a 
%                        particular variable
%  Return    : An updated version of the CFG
%  Notes     : 
%%>----------------------------------------------------------------------< 
updatePhi(Node, Parent, CFG, Current) ->
    BB = ?cfg:bb(CFG, Node),
    case hipe_bb:code(BB) of
	[Code|_] = Statements ->
	    case ?code:is_phi(Code) of
		true ->
		    Code2 = updateCode(Statements, Parent, Current),
		    ?cfg:bb_update(CFG, Node,hipe_bb:code_update(BB, Code2)); 
		_ -> CFG
	    end;
	_ -> CFG
    end.


%%>----------------------------------------------------------------------<
%  Procedure : updateCode/3
%  Purpose   : This function updates an icode statement that contains a 
%              phi function call, i.e. it changes the arguments in the call
%              to their correct name. 
%  Arguments : Code   - A list of intermediate code
%              Pred    - A predecessor of the node containing the
%                        phi-function
%              Current - Hash table containg the current index for a 
%                        particular variable
%  Return    : A list of Code
%  Notes     : Uses updateCode/4.
%%>----------------------------------------------------------------------< 
updateCode(Code, Pred, Current) ->
    updateCode(Code, Pred, Current, []).

updateCode([Statement | T] = Statements, Pred, Current, Result) ->
    case ?code:is_phi(Statement) of
	true ->
	    Var = ?code:phi_name(Statement),
	    Result2 = case lookup(Var, Current) of
			  not_found ->
			      Result;
			  Var2 ->
			      Statement2 =
				  ?code:subst_phi_arg(Statement,
							   Pred, Var2),
			      [Statement2|Result]
		      end,	    
	    updateCode(T, Pred, Current, Result2);
	_ ->
	    Result ++ Statements
    end.

