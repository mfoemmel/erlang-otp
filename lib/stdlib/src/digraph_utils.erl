%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(digraph_utils).

%%% Operations on directed (and undirected) graphs.
%%%
%%% Implementation based on Landsbury, John: Graph Algorithms with a 
%%% Functional Flavour, in Jeuring, Johan, and Meijer, Erik (Eds.): 
%%% Advanced Functional Programming, Lecture Notes in Computer 
%%% Science 925, Springer Verlag, 1995.

-export([components/1, strong_components/1, reachable/2, reaching/2,
	 topsort/1, isacyclic/1, reflexive_vertices/1, path/3,
	 preorder/1, postorder/1]).

%%
%%  Exported functions
%%

%% components(Graph) -> [Component]
%%    Graph = graph()
%%    Component = [Vertex]
%%
%% Returns a list of connected components. The order of vertices and
%% the order of components are arbitrary.
%%
%% A connected component is a subset of the graph vertices such that
%% there is a path between each pair of vertices, considering all
%% edges undirected. The returned components constitute a partitioning
%% of the graph vertices; for the sake of simplicity, each vertex is
%% assumed to have an edge going directly back to the vertex itself.
%% 
components(G) ->
    forest(G, fun inout/3).
    
%% strong_components(Graph) -> [StrongComponent]
%%    Graph = graph()
%%    StrongComponent = [Vertex]
%%
%% Returns a list of strongly connected components. The order of
%% vertices and the order of components are arbitrary.
%%
%% A strongly connected component is a subset of the graph vertices
%% such that there is a path between each pair of vertices. The
%% returned components constitute a partitioning of the graph
%% vertices; for the sake of simplicity, each vertex is assumed to
%% have an edge going directly back to the vertex itself.
%% 
strong_components(G) ->
    forest(G, fun in/3, revpostorder(G)).

%% reachable([Vertex], Graph) -> [Vertex]
%%    Graph = graph()
%%
%% Returns an unsorted list of graph vertices such that for each
%% vertex in the list, there is a path from some of the given vertices
%% to the vertex. Each vertex is assumed to have an edge going
%% directly back to the vertex itself; each given vertex occurs
%% somewhere in the returned list.
%%
reachable(Vs, G) when list(Vs) ->
    lists:append(forest(G, fun out/3, Vs)).
    
%% reaching([Vertex], Graph) -> [Vertex]
%%    Graph = graph()
%%
%% Returns an unsorted list of graph vertices such that for each
%% vertex in the list, there is a path from the vertex to some of the
%% given vertices.  Each vertex is assumed to have an edge going
%% directly back to the vertex itself; each given vertex occurs
%% somewhere in the returned list.
%%
reaching(Vs, G) when list(Vs) ->
    lists:append(forest(G, fun in/3, Vs)).
    
%% topsort(Graph) -> [Vertex] | false
%%    Graph = graph()
%%
%% Returns false if there is some cycle of length at least two,
%% otherwise returns a topologically sorted list of all vertices in
%% the graph.
%%
%% A topologically sorted list of vertices of a directed asyclig graph
%% is a list such that for each vertex there is no edge to a vertex
%% earlier in the list.
%%
topsort(G) ->
    L = revpostorder(G),
    case length(forest(G, fun in/3, L)) == length(digraph:vertices(G)) of
	true ->
	    L;
	false ->
	    false
    end.

%% isacyclic(Graph) -> boolean()
%%    Graph = graph()
%%
%% Returns true if and only if there are no cycles of length two or
%% more in the graph. An edge going from a vertex back to the vertex
%% itself is _not_ considered a cycle.
%%
isacyclic(G) ->
    length(strong_components(G)) == length(digraph:vertices(G)).
    
%% reflexive_vertices(Graph) -> [Vertex]
%%
%% Returns a list of vertices such that for each vertex in the list,
%% there is an edge in the graph going directly back to the vertex
%% itself.
%%
reflexive_vertices(G) ->
    F = fun(V) -> lists:member(V, digraph:out_neighbours(G, V)) end,
    lists:filter(F, digraph:vertices(G)).

%% path(Vertex1, Vertex2, Graph) -> boolean()
%%    Graph = graph()
%%
%% Returns true if there is a path in Graph from Vertex1 to Vertex2, 
%% otherwise false.
%%
path(V1, V2, G) ->
    lists:member(V2, reachable([V1], G)).

%% preorder(Graph) -> [Vertex]
%%    Graph = graph()
%%
%% Returns all vertices in the graph. The order is given by a depth-first
%% search of the graph, collecting visited vertices in preorder.
%%
preorder(G) ->
    lists:reverse(revpreorder(G)).

%% postorder(Graph) -> [Vertex]
%%    Graph = graph()
%%
%% Returns all vertices in the graph. The order is given by a depth-first
%% search of the graph, collecting visited vertices in postorder.
%%
postorder(G) ->
    lists:reverse(revpostorder(G)).

%%
%%  Local functions
%%

forest(G, SF) ->
    forest(G, SF, digraph:vertices(G)).

forest(G, SF, Vs) ->
    T = ets:new(forest, [set]),
    F = fun(V, LL) -> pretraverse([V], SF, G, T, [], LL) end,
    LL = lists:foldl(F, [], Vs),
    ets:delete(T),
    LL.

pretraverse([V|Vs], SF, G, T, Rs, LL) ->
    case ets:lookup(T, V) of
	[] ->
	    ets:insert(T, {V}),
	    pretraverse(SF(G, V, Vs), SF, G, T, [V|Rs], LL);
	_Else ->
	    pretraverse(Vs, SF, G, T, Rs, LL)
    end;
pretraverse([], _SF, _G, _T, [], LL) ->
    LL;
pretraverse([], _SF, _G, _T, Rs, LL) ->
    [Rs|LL].

revpreorder(G) ->
    lists:append(forest(G, fun out/3)).

revpostorder(G) ->
    T = ets:new(forest, [set]),
    L = posttraverse(digraph:vertices(G), G, T, []),
    ets:delete(T),
    L.

posttraverse([], G, T, L) ->
    L;
posttraverse([V|Vs], G, T, L) ->
    L1 = case ets:lookup(T, V) of
	     [] ->
		 ets:insert(T, {V}),
		 [V | posttraverse(out(G, V, []), G, T, L)];
	     _Else ->
		 L
	 end,
    posttraverse(Vs, G, T, L1).

in(G, V, Vs) ->
    digraph:in_neighbours(G, V) ++ Vs.

out(G, V, Vs) ->
    digraph:out_neighbours(G, V) ++ Vs.

inout(G, V, Vs) ->
    in(G, V, out(G, V, Vs)).
