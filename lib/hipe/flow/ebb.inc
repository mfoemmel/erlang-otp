%%% -*- Erlang -*-
%%% $Id$
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% IDENTIFIES THE EXTENDED BASIC BLOCKS OF A CFG
%

-module(?MODULE_NAME).
-export([cfg/1,
	 dag/2,
	 pp/1,
	 type/1,
	 node_label/1,
	 node_successors/1,
	 leaf_next/1]).

% Shut up silly compiler warning
-export([find_ebb/2]).


-define(cfg, ?CFG).


%
% Returns a list of extended basic blocks.
%

cfg(CFG) ->
   Start = ?cfg:start_label(CFG),
   Labels = ?cfg:labels(CFG),
   Roots = [Start | ?cfg:fail_entrypoints(CFG)],
   Blocks = Labels -- Roots,
   SuccMap = ?cfg:succ_map(CFG),
   PredMap = ?cfg:pred_map(CFG),
   {Ebb, _} = build_all_ebb(Roots, Blocks, SuccMap, PredMap),
   Ebb.



build_all_ebb([], [], SuccMap, PredMap) ->
   {[], []};
build_all_ebb([], [BlockLeft|BlocksLeft], SuccMap, PredMap) ->
   build_all_ebb([BlockLeft], BlocksLeft, SuccMap, PredMap);
build_all_ebb([Root|Roots], Blocks, SuccMap, PredMap) ->
   {Ebb, BlocksLeft} = build_ebb(Root, Blocks, SuccMap, PredMap),
   {MoreEbbs, NewBlocksLeft} = 
      build_all_ebb(Roots, BlocksLeft, SuccMap, PredMap),
   {[Ebb | MoreEbbs], NewBlocksLeft}.


%
% Build the extended basic block with Lbl as its root.
%

build_ebb(Lbl, BlocksLeft, SuccMap, PredMap) ->
   Succ = ?cfg:succ(SuccMap, Lbl),
   {SuccEBBs, NewBlocksLeft} = add_succ(Succ, BlocksLeft, SuccMap, PredMap),
   {mk_node(Lbl, SuccEBBs), NewBlocksLeft}.


add_succ([], BlocksLeft, SuccMap, PredMap) ->
   {[], BlocksLeft};
add_succ([Lbl|Lbls], BlocksLeft, SuccMap, PredMap) ->
   case {lists:member(Lbl, BlocksLeft), ?cfg:pred(PredMap, Lbl)} of
      {true, [_]} ->
	 {NewEbb, BlocksLeft0} = 
	    build_ebb(Lbl, lists:delete(Lbl, BlocksLeft), SuccMap, PredMap),
	 {NewEbbs, BlocksLeft1} = 
	    add_succ(Lbls, BlocksLeft0, SuccMap, PredMap),
	 {[NewEbb | NewEbbs], BlocksLeft1};
      _ ->
	 {NewEbbs, BlocksLeft0} = 
	    add_succ(Lbls, BlocksLeft, SuccMap, PredMap),
	 {[mk_leaf(Lbl) | NewEbbs], BlocksLeft0}
   end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Generate a list of dags.
%

dag(EBBs, CFG) ->
   Start = ?cfg:start_label(CFG),
   Entries = ?cfg:fail_entrypoints(CFG),
   Roots = [Start | Entries],
   Edges = all_adges(EBBs, Roots),
   start_dag(Roots, Edges, []).

start_dag([], Edges, Visit) ->
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


add_dag_succ([], Dag, Edges, Roots, Visit) ->
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


find_succ(Lbl, []) ->
   [];
find_succ(Lbl, [{Lbl, Succ}|Edges]) ->
   [Succ | find_succ(Lbl, Edges)];
find_succ(Lbl, [_|Edges]) ->
   find_succ(Lbl, Edges).

find_pred(Lbl, []) ->
   [];
find_pred(Lbl, [{Pred, Lbl}|Edges]) ->
   [Pred | find_pred(Lbl, Edges)];
find_pred(Lbl, [_|Edges]) ->
   find_pred(Lbl, Edges).


all_adges([], Roots) ->
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



all_in([], List) ->
   true;
all_in([X|Xs], List) ->
   lists:member(X, List) and all_in(Xs, List).


find_ebb(Lbl, [EBB|EBBs]) ->
   case node_label(EBB) of
      Lbl ->
	 EBB;
      _ ->
	 find_ebb(Lbl, EBBs)
   end.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The extended basic block datatype
%
% An EBB is identified with the label of the root node. 
% It's a tree
%
% EBB :=  {node, Label, [EBB]}
%      |  {leaf, Label, SuccesorEBB}
%

mk_node(Label, Successors) -> {node, Label, Successors}.
node_label({node, Label, Successors}) -> Label.
node_successors({node, Label, Successors}) -> Successors.


mk_leaf(NextEbb) -> {leaf, NextEbb}.
leaf_next({leaf, NextEbb}) -> NextEbb.

type({node, Label, SuccessorNodes}) -> node;
type({leaf, SuccessorEbb}) -> leaf.


ebb_successors(EBB) ->
   ordsets:from_list(ebb_successors0(EBB)).

ebb_successors0({leaf, NextEBB}) ->
   [NextEBB];
ebb_successors0({node, Label, SuccessorNodes}) ->
   lists:append(lists:map(fun ebb_successors0/1, SuccessorNodes)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Prettyprint a list of extended basic blocks
%

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
