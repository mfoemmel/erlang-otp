%% -*- Erlang -*-
%% $Id$
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% IDENTIFIES THE EXTENDED BASIC BLOCKS OF A CFG
%%

-module(?MODULE_NAME).
-export([cfg/1,
	 dag/2,
	 pp/1,
	 type/1,
	 node_label/1,
	 node_successors/1,
	 leaf_next/1]).


-define(cfg, ?CFG).


%%
%% Returns a list of extended basic blocks.
%%

cfg(CFG) ->
  Start = ?cfg:start_label(CFG),
  Labels = ?cfg:reverse_postorder(CFG),
  Roots = [Start],% | ?cfg:fail_entrypoints(CFG)],
  Blocks = Labels -- Roots,
  SuccMap = ?cfg:succ_map(CFG),
  PredMap = ?cfg:pred_map(CFG),
  {_,LMax} = ?cfg:label_range(CFG),
  Visited = new_visited(LMax),
  Ebb = build_all_ebb(Roots, Blocks, Visited, SuccMap, PredMap),
  Ebb.

new_visited(Max) ->
  vector:new(Max+1,false).
visited(L,Visited) ->
  vector:get(L+1,Visited).
visit(L,Visited) ->
  vector:set(L+1,Visited,true).

build_all_ebb(Roots, Blocks, Visited, SuccMap, PredMap) ->
  build_all_ebb(Roots, Blocks, Visited, SuccMap, PredMap, []).
build_all_ebb([], [],_, _SuccMap, _PredMap, Ebbs) ->
  lists:reverse(Ebbs);
build_all_ebb([], [BlockLeft|BlocksLeft], Visited, SuccMap, PredMap, Ebbs) ->
  case visited(BlockLeft,Visited) of
    true -> 
      build_all_ebb([], BlocksLeft, Visited, SuccMap, PredMap,Ebbs);
    false ->
      build_all_ebb([BlockLeft], BlocksLeft, Visited, SuccMap, PredMap,Ebbs)
  end;
build_all_ebb([Root|Roots], Blocks, Visited, SuccMap, PredMap,Ebbs) ->
  {Ebb, NewVisited} = 
    build_ebb(Root, Visited, SuccMap, PredMap),
  build_all_ebb(Roots, Blocks, NewVisited, SuccMap, PredMap,[Ebb | Ebbs]).



%%
%% Build the extended basic block with Lbl as its root.
%%
build_ebb(Lbl, Visited, SuccMap, PredMap) ->
  build_ebb(Lbl, Visited, 
	    fun (NodeL,NewVisited) ->
		{NodeL,NewVisited}
	    end,
	    [],SuccMap, PredMap).

build_ebb(Lbl, Visited, MkFun, EBBs, SuccMap, PredMap) ->
  Succ = ?cfg:succ(SuccMap, Lbl),
  add_succ(Succ, visit(Lbl,Visited), Lbl, MkFun, EBBs, SuccMap, PredMap).



add_succ([], Visited,Node, MkFun,EBBs, _SuccMap, _PredMap) ->
  MkFun(mk_node(Node, lists:reverse(EBBs)), Visited);
add_succ([Lbl|Lbls], Visited,Node, MkFun,EBBs, SuccMap, PredMap) ->
  case [visited(Lbl, Visited)|?cfg:pred(PredMap, Lbl)] of
    [false,_] ->
      build_ebb(Lbl, Visited, 
		fun (NewEbb, Visited0) ->
		    add_succ(Lbls, Visited0, Node, MkFun, [NewEbb|EBBs], 
			     SuccMap, PredMap)
		end, [],SuccMap, PredMap);

    _ ->
      add_succ(Lbls, Visited, Node, MkFun,[mk_leaf(Lbl)|EBBs],
		 SuccMap, PredMap)
   end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Generate a list of dags.
%%

dag(EBBs, CFG) ->
   Start = ?cfg:start_label(CFG),
   Entries = ?cfg:fail_entrypoints(CFG),
   Roots = [Start | Entries],
   Edges = all_adges(EBBs, Roots),
   start_dag(Roots, Edges, []).

start_dag([], _Edges, _Visit) ->
   [];
start_dag([Root|Roots], Edges, Visit) ->
   case lists:member(Root, Visit) of
      true ->
	 start_dag(Roots, Edges, Visit);
      false ->
	 {Dag, Roots0, Visit0} =
	    fill_dag(Root, [Root], Edges, Roots, [Root|Visit]),
	 [lists:reverse(Dag) | start_dag(Roots0, Edges, Visit0)]
   end.

fill_dag(Lbl, Dag, Edges, Roots, Visit) ->
   Succ = find_succ(Lbl, Edges),
   add_dag_succ(Succ, Dag, Edges, Roots, Visit).


add_dag_succ([], Dag, _Edges, Roots, Visit) ->
   {Dag, Roots, Visit};
add_dag_succ([S|Ss], Dag, Edges, Roots, Visit) ->
   {Dag0, Roots0, Visit0} = add_dag_succ(Ss, Dag, Edges, Roots, Visit),
   Pred = find_pred(S, Edges),
   case all_in(Pred, Dag0) of
      true ->
	 fill_dag(S, [S|Dag0], Edges, Roots0, [S|Visit0]);
      false ->
	 {Dag0, [S|Roots], Visit0}
   end.


find_succ(_Lbl, []) ->
   [];
find_succ(Lbl, [{Lbl, Succ}|Edges]) ->
   [Succ | find_succ(Lbl, Edges)];
find_succ(Lbl, [_|Edges]) ->
   find_succ(Lbl, Edges).

find_pred(_Lbl, []) ->
   [];
find_pred(Lbl, [{Pred, Lbl}|Edges]) ->
   [Pred | find_pred(Lbl, Edges)];
find_pred(Lbl, [_|Edges]) ->
   find_pred(Lbl, Edges).


all_adges([], _Roots) ->
   [];
all_adges([EBB|EBBs], Roots) ->
   succ_adges(node_label(EBB), ebb_successors(EBB), EBBs, Roots).

succ_adges(Lbl, [], EBBs, Roots) ->
   case lists:member(Lbl, Roots) of
      true ->
	 [{start, Lbl} | all_adges(EBBs, Roots)];
      false ->
	 all_adges(EBBs, Roots)
   end;
succ_adges(Lbl, [S|Ss], EBBs, Roots) ->
   [{Lbl, S} | succ_adges(Lbl, Ss, EBBs, Roots)].


all_in([], _List) ->
   true;
all_in([X|Xs], List) ->
   lists:member(X, List) andalso all_in(Xs, List).


%% find_ebb(Lbl, [EBB|EBBs]) ->
%%   case node_label(EBB) of
%%      Lbl ->
%%	 EBB;
%%      _ ->
%%	 find_ebb(Lbl, EBBs)
%%   end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% The extended basic block datatype
%%
%% An EBB is identified with the label of the root node. 
%% It's a tree
%%
%% EBB :=  {node, Label, [EBB]}
%%      |  {leaf, Label, SuccesorEBB}
%%

mk_node(Label, Successors) -> {node, Label, Successors}.
node_label({node, Label, _Successors}) -> Label.
node_successors({node, _Label, Successors}) -> Successors.


mk_leaf(NextEbb) -> {leaf, NextEbb}.
leaf_next({leaf, NextEbb}) -> NextEbb.

type({node, _Label, _SuccessorNodes}) -> node;
type({leaf, _SuccessorEbb}) -> leaf.


ebb_successors(EBB) ->
   ordsets:from_list(ebb_successors0(EBB)).

ebb_successors0({leaf, NextEBB}) ->
   [NextEBB];
ebb_successors0({node, _Label, SuccessorNodes}) ->
   lists:append(lists:map(fun ebb_successors0/1, SuccessorNodes)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Prettyprint a list of extended basic blocks
%%

pp(EBBs) ->
   lists:map(fun(E) -> pp(E, 0) end, EBBs).

pp(EBB, Indent) ->
   io:format([$~]++integer_to_list(Indent)++[$c],[$ ]),
   case type(EBB) of
      node ->
	 io:format("~w~n", [node_label(EBB)]),
	 lists:map(fun(E) -> pp(E, Indent+3) end, node_successors(EBB));
      leaf ->
	 io:format("* -> ~w~n", [leaf_next(EBB)])
   end.
