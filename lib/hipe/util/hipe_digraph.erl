%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
%% File    : hipe_digraph.erl
%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%% Purpose : Provides a simple implementation of a directed graph.
%%
%% Created :  9 Feb 2005 by Tobias Lindahl <tobiasl@it.uu.se>
%%-----------------------------------------------------------------------
-module(hipe_digraph).

-export([new/0, add_edge/3, add_node/2, add_node_list/2,
	 from_list/1, to_list/1, get_parents/2, get_children/2]).
-export([reverse_preorder_sccs/1]).

%%------------------------------------------------------------------------

-type(set()     :: tuple()).  % XXX: temporarily
-type(dict()    :: tuple()).  % XXX: temporarily
-type(ordset(T) :: [T]).      % XXX: temporarily

-include("hipe_digraph.hrl").

%%------------------------------------------------------------------------

-spec(new/0 :: () -> #hipe_digraph{}).

new() ->
  #hipe_digraph{edges=dict:new(), rev_edges=dict:new(), 
		leaves=ordsets:new(), nodes=sets:new()}.

-spec(from_list/1 :: ([_]) -> #hipe_digraph{}).

from_list(List) ->
  Edges = lists:foldl(fun({From, To}, Dict) -> 
			  Fun = fun(Set) -> ordsets:add_element(To, Set) end,
			  dict:update(From, Fun, [To], Dict)
		      end,
		      dict:new(), List),
  
  RevEdges = lists:foldl(fun({From, To}, Dict) -> 
			     Fun = fun(Set) -> 
				       ordsets:add_element(From, Set) 
				   end,
			     dict:update(To, Fun, [From], Dict)
			 end,
			 dict:new(), List),
  Keys1 = sets:from_list(dict:fetch_keys(Edges)),
  Keys2 = sets:from_list(dict:fetch_keys(RevEdges)),
  Nodes = sets:union(Keys1, Keys2),
  #hipe_digraph{edges=Edges, leaves=[], rev_edges=RevEdges, nodes=Nodes}.

-spec(to_list/1 :: (#hipe_digraph{}) -> [_]).

to_list(#hipe_digraph{edges=Edges}) ->
  List1 = dict:to_list(Edges),
  List2 = lists:foldl(fun({From, ToList}, Acc) ->
			  [[{From, To} || To <- ToList]|Acc]
		      end, [], List1),
  lists:flatten(List2).

-spec(add_node/2 :: (_, #hipe_digraph{}) -> #hipe_digraph{}).

add_node(NewNode, DG = #hipe_digraph{nodes=Nodes}) ->
  DG#hipe_digraph{nodes=sets:add_element(NewNode, Nodes)}.

-spec(add_node_list/2 :: ([_], #hipe_digraph{}) -> #hipe_digraph{}).

add_node_list(NewNodes, DG = #hipe_digraph{nodes=Nodes}) ->
  Set = sets:from_list(NewNodes),
  DG#hipe_digraph{nodes=sets:union(Set, Nodes)}.

-spec(add_edge/3 :: (_, _, #hipe_digraph{}) -> #hipe_digraph{}).

add_edge(From, To, #hipe_digraph{edges=Edges, rev_edges=RevEdges, 
				 leaves=Leaves, nodes=Nodes}) ->
  Fun1 = fun(Set) -> ordsets:add_element(To, Set) end,
  NewEdges = dict:update(From, Fun1, [To], Edges),
  Fun2 = fun(Set) -> ordsets:add_element(From, Set) end,
  NewRevEdges = dict:update(To, Fun2, [From], RevEdges),
  NewLeaves = ordsets:del_element(From, Leaves),
  #hipe_digraph{edges=NewEdges,
		rev_edges=NewRevEdges,
		leaves=NewLeaves,
		nodes=sets:add_element(From, sets:add_element(To, Nodes))}.

%%-------------------------------------------------------------------------

-spec(take_indep_scc/1 ::
      (#hipe_digraph{}) -> 'none' | {'ok', [_], #hipe_digraph{}}).

take_indep_scc(DG = #hipe_digraph{edges=Edges, rev_edges=RevEdges, 
				  leaves=Leaves, nodes=Nodes}) ->
  case sets:size(Nodes) =:= 0 of
    true -> none;
    false ->
      {SCC, NewLeaves} =
	case Leaves of
	  [H|T] -> 
	    {[H], T};
	  [] ->
	    case find_all_leaves(Edges) of
	      [] ->
		{[Node|_], _} = dfs(Nodes, RevEdges),
		{SCC1, _} = dfs(Node, Edges),
		{SCC1, []};
	      [H|T] ->
		{[H], T}
	    end
	end,
      NewEdges = remove_edges(SCC, Edges, RevEdges),
      NewRevEdges = remove_edges(SCC, RevEdges, Edges),
      NewNodes = sets:subtract(Nodes, sets:from_list(SCC)),
      {ok, reverse_preorder(SCC, Edges),
       DG#hipe_digraph{edges=NewEdges, rev_edges=NewRevEdges, 
		       leaves=NewLeaves, nodes=NewNodes}}
  end.

find_all_leaves(Edges) ->
  List = dict:fold(fun(Key, [Key], Acc) -> [Key|Acc];
		      (_, _, Acc) -> Acc
		   end, [], Edges),
  ordsets:from_list(List).

remove_edges(Nodes0, Edges, RevEdges) ->
  Nodes = ordsets:from_list(Nodes0),
  Fun = fun(N, Dict) -> dict:erase(N, Dict) end,
  Edges1 = lists:foldl(Fun, Edges, Nodes),
  remove_edges_in(Nodes, Edges1, RevEdges).

remove_edges_in([Node|Left], Edges, RevEdges) ->
  case dict:find(Node, RevEdges) of
    error ->
      remove_edges_in(Left, Edges, RevEdges);
    {ok, Set} ->
      Fun = fun(Key, Dict) ->
		case dict:find(Key, Dict) of
		  error -> 
		    Dict;
		  {ok, OldTo} ->
		    case ordsets:del_element(Node, OldTo) of
		      [] -> dict:store(Key, [Key], Dict);
		      NewSet -> dict:store(Key, NewSet, Dict)
		    end
		end
	    end,
      NewEdges = lists:foldl(Fun, Edges, Set),
      remove_edges_in(Left, NewEdges, RevEdges)
  end;
remove_edges_in([], Edges, _RevEdges) ->
  Edges.

reverse_preorder([Node], _Edges) ->
  [Node];
reverse_preorder(Nodes0 = [H|_], Edges) ->
  Nodes = sets:from_list(Nodes0),
  {PreOrder, _} = dfs(H, Edges),
  DFS = [X || X <- PreOrder, sets:is_element(X, Nodes)],
  lists:reverse(DFS).

%%---------------------------------------------------------------------

-spec(reverse_preorder_sccs/1 :: (#hipe_digraph{}) -> [[_]]).

reverse_preorder_sccs(DG) ->
  reverse_preorder_sccs(DG, []).

reverse_preorder_sccs(DG, Acc) ->
  case take_indep_scc(DG) of
    none -> lists:reverse(Acc);
    {ok, SCC, DG1} -> reverse_preorder_sccs(DG1, [SCC|Acc])
  end.

%%---------------------------------------------------------------------

-spec(get_parents/2 :: (_, #hipe_digraph{}) -> [_]).

get_parents(Node, #hipe_digraph{rev_edges=RevEdges}) ->
  case dict:is_key(Node, RevEdges) of
    true -> dict:fetch(Node, RevEdges);
    false -> []
  end.

-spec(get_children/2 :: (_, #hipe_digraph{}) -> [_]).

get_children(Node, #hipe_digraph{edges=Edges}) ->
  case dict:is_key(Node, Edges) of
    true -> dict:fetch(Node, Edges);
    false -> []
  end.

%%---------------------------------------------------------------------
%% dfs/2 returns a preordered depth first search and the nodes visited.

dfs(Node, Edges) ->
  case sets:is_set(Node) of
    true ->
      dfs(sets:to_list(Node), Edges, sets:new(), []);
    false ->
      dfs([Node], Edges, sets:new(), [])
  end.

dfs([Node|Left], Edges, Visited, Order) ->
  case sets:is_element(Node, Visited) of
    true ->
      dfs(Left, Edges, Visited, Order);
    false ->
      NewVisited = sets:add_element(Node, Visited),
      case dict:find(Node, Edges) of
	error ->
	  dfs(Left, Edges, NewVisited, [Node|Order]);
	{ok, Succ} ->
	  {NewOrder, NewVisited1} = dfs(Succ, Edges, NewVisited, Order),
	  dfs(Left, Edges, NewVisited1, [Node|NewOrder])
      end
  end;
dfs([], _Edges, Visited, Order) ->
  {Order, Visited}.
