%%%-------------------------------------------------------------------
%%% File    : hipe_ssa_checker.inc
%%% Author  : Gustafsson <pegu2945@fan.it.uu.se>
%%% Description : Checks if a CFG has SSA form.
%%%
%%% Created : 19 Feb 2003 by Gustafsson <pegu2945@fan.it.uu.se>
%%%-------------------------------------------------------------------

-export([check/1]).

%% This function checks that all variables in a CFG are only defined once
%% and that all uses of a function is dominated by the define 
%% If a variable does not abide by these rules a message will
%% be printed on stdout.
check(CFG) ->
  Labels=?cfg:labels(CFG),
  VarTree=traverse_labels(Labels,CFG),
  DomTree=hipe_domtree:create(CFG),
  test_uses(Labels, VarTree, DomTree, CFG).


%%This function traverses all the labels in a cfg
traverse_labels(Labels, CFG) ->
  VarTree=add_args(?cfg:params(CFG)),
  traverse_labels(Labels, VarTree, CFG).
traverse_labels([Label|Rest], VarTree, CFG) ->
  BB=?cfg:bb(CFG,Label),
  Code=hipe_bb:code(BB),
  NewVarTree=traverse_code(Code, VarTree, Label),
  traverse_labels(Rest, NewVarTree, CFG);

traverse_labels([], VarTree,_CFG) ->
  VarTree.


%%This function traverses the code in a BB
traverse_code([Instr|Rest], VarTree, Label) ->
  Defined=?code:defines(Instr),
  NewVarTree=add_to_var_tree(Defined, VarTree, Instr, Label), 
  traverse_code(Rest, NewVarTree, Label);
    
traverse_code([], VarTree,_) ->
  VarTree.

%% This function adds a variable to the variable tree if it is defined
%% The entry in the varaible tree will have the variable as key and
%% a two tuple consisting of a list of Instructions and alist of labels
%% where the variable is defined. If a variable is defined a second time
%% a message to this effect is printed on stdout
add_to_var_tree([Var|Rest], VarTree, Instr, Label) ->
  case gb_trees:lookup(Var, VarTree) of
    {value, {OldInstr, OldLabel}} ->
      io:format("Variable: ~w defined second time ~n in Instr: ~w ~n at Label ~w~n 
variable first defined at Label(s) ~w~n in Instr(s): ~w~n -> non ssa form~n", [Var, Instr, Label, OldLabel, OldInstr]),
      add_to_var_tree(Rest, gb_trees:update(Var, {[Instr|OldInstr], [Label|OldLabel]}, VarTree), Instr, Label);
    none ->
      add_to_var_tree(Rest, gb_trees:insert(Var, {[Instr], [Label]}, VarTree), Instr, Label)
  end;

add_to_var_tree([], VarTree,_,_) ->
  VarTree.


%%This function adds the argument of a function to the VarTree.
%%They are defined at Label 0
add_args(Args) ->
  add_args(Args, gb_trees:empty()).
add_args([Arg|Rest], VarTree ) ->
  add_args(Rest, gb_trees:insert(Arg, {[argument_variable],[0]}, VarTree));
add_args([], VarTree) ->
  VarTree.



%% The functions below are used to test that a use is dominated by the def.
%% This function is analogus to traverse_labels
test_uses([Label|Rest], VarTree, DomTree,CFG) ->
  BB=?cfg:bb(CFG,Label),
  Code=hipe_bb:code(BB),
  test_code(Code, VarTree, Label, DomTree,[]),
  test_uses(Rest, VarTree, DomTree, CFG);
test_uses([], _VarTree, _DomTree, _CFG) ->
  ok.


%%This function is anologus to traverse_code
test_code([Instr|Instrs], VarTree, Label, DomTree,Old) ->
  case element(1, Instr) of
    phi->
      ok;
    _ ->
      Used=?code:uses(Instr),
      def_doms_use(Used, VarTree, Label, DomTree,Old)
  end,
  test_code(Instrs, VarTree, Label, DomTree,[Instr|Old]);
test_code([], _VarTree, _Label, _DomTree, _Old) ->
  ok.


%% This function checks wheter a use is dominated by a def.
%% There are five different cases:
%% 1. A use of an argument register. This use is dominated by the def
%% 2. Use and Def in same basic block if Use comes first this will 
%%    lead to a message, otherwise it is ok.
%% 3. The deinition is in a basic block that dominates the basic block
%%    of the use. This is ok.
%% 4. The definition is in a basic block that does not dominate the use.
%%    This will lead to a message.
%% 5. A use without any definition. This will lead to a message
def_doms_use([Var|Vars], VarTree, Label, DomTree, Old) ->
  case gb_trees:lookup(Var, VarTree) of
    {value, {_,[DefLabel|_]}} ->
      case DefLabel of
	0 ->
	  ok;
	Label ->
	  Fun = fun(X) -> Defines = ?code:defines(X), 
			  lists:any(fun(Y) -> Var == Y end, Defines) end,
	  case lists:any(Fun, Old) of
	    true ->
	      ok;
	    false ->
	      io:format("Variable : ~w used before definition ~n in bb: ~w~n", [Var, Label])
	  end;
	_ ->
	  case hipe_domtree:dominates(DefLabel, Label, DomTree) of
	    true ->
	      ok;
	    false ->
	      io:format("Definition does not dominate use for variable: ~w ~n at label: ~w definition label: ~w~n", 
			[Var, Label, DefLabel])
	  end
      end;
    none ->
      io:format("Use with no definition of variable: ~w ~n at label: ~w~n",
		[Var, Label])
  end,
  def_doms_use(Vars, VarTree, Label,DomTree, Old);
def_doms_use([], _VarTree, _Label, _DomTree, _Old) ->
  ok.
