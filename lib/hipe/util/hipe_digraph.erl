%%%-------------------------------------------------------------------
%%% File    : hipe_digraph.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description : Simple implementation of a directed graph.
%%%
%%% Created :  9 Feb 2005 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------
-module(hipe_digraph).

-export([add_node/2, from_list/1, reverse_preorder_sccs/1]).

-define(NO_UNUSED, true).

-ifndef(NO_UNUSED).
-export([new/0, add_edge/3, delete_edge/3, sccs/1]).
-endif.

-record(digraph, {edges, nodes}).

from_list(List) ->
  Edges = ordsets:from_list(List),
  Nodes = lists:foldl(fun({X, Y}, Acc) -> 
			  ordsets:add_element(X, ordsets:add_element(Y, Acc))
		      end, [], Edges),  
  #digraph{edges=Edges, nodes=Nodes}.

add_node(NewNode, DG) ->
  add_edge(NewNode, NewNode, DG).

add_edge(From, To, DG = #digraph{edges=Edges, nodes=Nodes}) ->
  DG#digraph{edges=ordsets:add_element({From, To}, Edges),
	     nodes=ordsets:add_element(From, ordsets:add_element(To, Nodes))}.

-ifndef(NO_UNUSED).
delete_edge(From, To, DG = #digraph{edges=Edges}) ->
  NewEdges = ordsets:del_element({From, To}, Edges),
  Nodes = lists:foldl(fun({X, Y}, Acc) -> 
			  ordsets:add_element(X, ordsets:add_element(Y, Acc))
		      end, NewEdges),  
  DG#digraph{edges=NewEdges, nodes=Nodes}.
-endif.

%%---------------------------------------------------------------------
%% Find the strongly connected components.

-ifndef(NO_UNUSED).
sccs(#digraph{edges=Edges, nodes=Nodes}) ->
  sccs(Nodes, Edges).
-endif.

sccs(Nodes, Edges) ->
  sccs(Nodes, Edges, []).

sccs([Node|Left], Edges, Acc) ->
  {_PreOrder, VisitedDFS} = dfs(Node, Edges),
  {_RDFS, VisitedRDFS} = rdfs(Node, Edges),
  SCC = ordsets:intersection(VisitedDFS, VisitedRDFS),
  sccs(ordsets:subtract(Left, SCC), Edges, [SCC|Acc]);
sccs([], _Edges, Acc) ->
  Acc.

get_scc_edges(Nodes, Edges) ->
  Sccs = sccs(Nodes, Edges),
  {_, OrderedSccs} = lists:foldl(fun(Scc, {Id, Acc})->
				     {Id+1, [{Id, Scc}|Acc]}
				 end, {0, []}, Sccs),
  NodeToSccMap = lists:foldl(fun({Id, Ns}, Map)->
				 lists:foldl(fun(N, M)->
						 gb_trees:insert(N, Id, M)
					     end, Map, Ns)
			     end, gb_trees:empty(), OrderedSccs),
  {get_scc_edges(Nodes, NodeToSccMap, Edges, []), lists:reverse(OrderedSccs)}.

get_scc_edges([Node|Left], NodeToSccMap, NodeEdges, Acc) ->
  SccId = gb_trees:get(Node, NodeToSccMap),
  EdgesOutNode = [Y|| {X, Y} <- NodeEdges, Node =:= X],
  NewSccEdges = [{SccId, gb_trees:get(X, NodeToSccMap)} || X <- EdgesOutNode],
  NewSccEdgeSet = ordsets:from_list(NewSccEdges),
  NewSccEdgeSet1 = ordsets:del_element({SccId, SccId}, NewSccEdgeSet),
  get_scc_edges(Left, NodeToSccMap, NodeEdges, 
		ordsets:union(Acc, NewSccEdgeSet1));
get_scc_edges([], _NodeToSccMap, _NodeEdges, Acc) ->
  Acc.

reverse_preorder_sccs(#digraph{edges=Edges, nodes=Nodes}) ->
  {SccEdges, SccToNodesMap} = get_scc_edges(Nodes, Edges),
  Order = reverse_preorder([X || {X, _} <- SccToNodesMap], SccEdges),
  [reverse_preorder_scc(orddict:fetch(X, SccToNodesMap), Edges) || X <- Order].

reverse_preorder(Nodes, Edges) ->
  lists:flatten(reverse_preorder_1(Nodes, Edges, [])).

reverse_preorder_1([Node|Left], Edges, Visited) ->
  {PreOrder, DFSVisited} = dfs([Node], Edges, Visited, []),
  NewLeft = ordsets:subtract(Left, Visited),
  NewVisited = ordsets:union(DFSVisited, Visited),
  [lists:reverse(PreOrder) | 
   reverse_preorder_1(NewLeft, Edges, NewVisited)];
reverse_preorder_1([], _Edges, _Visited) ->
  [].

reverse_preorder_scc([N], _AllEdges) ->
  [N];
reverse_preorder_scc([N1, N2], _AllEdges) ->
  [N1, N2];
reverse_preorder_scc(Nodes, AllEdges) ->
  Set = ordsets:from_list(Nodes),
  LocalEdges = lists:filter(fun({X, Y})->
				ordsets:is_element(X, Set) andalso
				  ordsets:is_element(Y, Set)
			    end, AllEdges),
  {Order, _} = dfs(hd(Nodes), LocalEdges),
  lists:reverse(Order).
				

%%---------------------------------------------------------------------
%% dfs/2 returns a preordered depth first search and the nodes visited.

dfs(Node, Edges) ->
  dfs([Node], Edges, [], []).

dfs([Node|Left], Edges, Visited, Order)->
  case ordsets:is_element(Node, Visited) of
    true ->
      dfs(Left, Edges, Visited, Order);
    false ->
      Filter = fun({F, _T}) -> Node =:= F end,
      Succ = [X || {_, X} <- ordsets:filter(Filter, Edges)],
      {NewOrder, NewVisited} = dfs(Succ, Edges, 
				   ordsets:add_element(Node, Visited), Order),
      dfs(Left, Edges, NewVisited, [Node|NewOrder])
  end;
dfs([], _Edges, Visited, Order) ->
  {Order, Visited}.


%%---------------------------------------------------------------------
%% rdfs/2 returns a preordered depth first search of the inverted graph,
%% and the nodes visited.

rdfs(Node, Edges) ->
  rdfs([Node], Edges, [], []).

rdfs([Node|Left], Edges, Visited, Order)->
  case ordsets:is_element(Node, Visited) of
    true ->
      rdfs(Left, Edges, Visited, Order);
    false ->
      Filter = fun({_F, T}) -> Node =:= T end,
      Succ = [X || {X, _} <- ordsets:filter(Filter, Edges)],
      {NewOrder, NewVisited} = rdfs(Succ, Edges, 
				    ordsets:add_element(Node, Visited), Order),
      rdfs(Left, Edges, NewVisited, [Node|NewOrder])
  end;
rdfs([], _Edges, Visited, Order) ->
  {Order, Visited}.
