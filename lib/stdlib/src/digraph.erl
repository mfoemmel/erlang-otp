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
-module(digraph).

-export([new/0, new/1, delete/1, info/1]).

-export([add_vertex/1, add_vertex/2, add_vertex/3]).
-export([del_vertex/2, del_vertices/2]).
-export([vertex/2, no_vertices/1, vertices/1]).
-export([source_vertices/1, sink_vertices/1]).

-export([add_edge/3, add_edge/4, add_edge/5]).
-export([del_edge/2, del_edges/2, del_path/3]).
-export([edge/2, no_edges/1, edges/1]).

-export([out_neighbours/2, in_neighbours/2]).
-export([out_edges/2, in_edges/2, edges/2]).
-export([out_degree/2, in_degree/2]).
-export([get_path/3, get_cycle/2]).

-export([get_short_path/3, get_short_cycle/2]).

-record(graph, {
		vtab = notable,
		etab = notable,
		ntab = notable,
	        cyclic = true } ).

%%
%% Type is a list of
%%  protected | private
%%  acyclic | cyclic
%%
%%  default is [cyclic,protected]
%%
new() -> new([]).

new(Type) ->
    case check_type(Type, protected, []) of
	{error, What} -> {error, What};
	{Access,Ts} -> 
	    V = ets:new(vertices, [set,Access]),
	    E = ets:new(edges, [set,Access]),
	    N = ets:new(neighbours, [bag,Access]),
	    ets:insert(N, [{'$vid', 0}, {'$eid', 0}]),
	    set_type(Ts, #graph{vtab=V, 
				etab=E,
				ntab=N })
    end.

%%
%% Check type of graph
%%
check_type([acyclic|Ts], A, L) ->
    check_type(Ts, A,[{cyclic,false} | L]);
check_type([cyclic | Ts], A, L) ->
    check_type(Ts, A, [{cyclic,true} | L]);
check_type([protected | Ts], _, L) ->
    check_type(Ts, protected, L);
check_type([private | Ts], _, L) ->
    check_type(Ts, private, L);
check_type([T | _], _, _) -> 
    {error, {unknown_type, T}};
check_type([], A, L) -> {A,L}.

%%
%% Set graph type
%%
set_type([{cyclic,V} | Ks], G) ->
    set_type(Ks, G#graph{cyclic = V});
set_type([], G) -> G.


%% Data access functions

delete(G) ->
    ets:delete(G#graph.vtab),
    ets:delete(G#graph.etab),
    ets:delete(G#graph.ntab).

info(G) ->
    VT = G#graph.vtab,
    ET = G#graph.etab,
    NT = G#graph.ntab,
    Cyclicity = case G#graph.cyclic of
		    true  -> cyclic;
		    false -> acyclic
		end,
    Protection = ets:info(VT, protection),
    Memory = ets:info(VT, memory) 
	+ ets:info(ET, memory)
	+ ets:info(NT, memory),
    [{cyclicity, Cyclicity}, 
     {memory, Memory},
     {protection, Protection}].

add_vertex(G) -> 
    do_add_vertex({new_vertex_id(G),[]}, G).

add_vertex(G, V) -> 
    do_add_vertex({V,[]}, G).

add_vertex(G, V, D) ->
    do_add_vertex({V,D}, G).

del_vertex(G, V) ->
    do_del_vertex(V, G).

del_vertices(G, Vs) -> 
    do_del_vertices(Vs, G).

vertex(G, V) ->
    case ets:lookup(G#graph.vtab, V) of
	[] -> false;
	[Vertex] -> Vertex
    end.

no_vertices(G) ->
    ets:info(G#graph.vtab, size).

vertices(G) -> 
    ets:select(G#graph.vtab, [{{'$1', '_'}, [], ['$1']}]).

source_vertices(G) ->
    collect_vertices(G, in).

sink_vertices(G) ->
    collect_vertices(G, out).

in_degree(G, V) ->
    length(ets:lookup(G#graph.ntab,{in,V})).

in_neighbours(G,V) ->
    ET = G#graph.etab,
    NT = G#graph.ntab,
    collect_elems(ets:lookup(NT,{in,V}), ET, 2).

in_edges(G, V) ->
    ets:select(G#graph.ntab, [{{{in,V},'$1'},[],['$1']}]).

out_degree(G, V) -> 
    length(ets:lookup(G#graph.ntab,{out,V})).

out_neighbours(G, V) -> 
    ET = G#graph.etab,
    NT = G#graph.ntab,
    collect_elems(ets:lookup(NT,{out,V}), ET, 3).

out_edges(G, V) -> 
    ets:select(G#graph.ntab, [{{{out,V},'$1'},[],['$1']}]).

add_edge(G, V1, V2) -> 
    do_add_edge({new_edge_id(G),V1,V2,[]}, G).

add_edge(G, V1, V2, D) -> 
    do_add_edge({new_edge_id(G),V1,V2,D}, G).

add_edge(G, E, V1, V2, D) -> 
    do_add_edge({E,V1,V2,D}, G).

del_edge(G, E) -> 
    do_del_edges([E], G).

del_edges(G, Es) ->  
    do_del_edges(Es, G).

no_edges(G) ->
    ets:info(G#graph.etab, size).

edges(G) ->
    ets:select(G#graph.etab, [{{'$1', '_', '_', '_'}, [], ['$1']}]).

edges(G, V) ->
    ets:select(G#graph.ntab, [{{{out,V},'$1'},[],['$1']},
			      {{{in,V},'$1'},[],['$1']}]).

edge(G, E) ->
    case ets:lookup(G#graph.etab,E) of
	[] -> false;
	[Edge] -> Edge
    end.

%%
%% Generate a "unique" edge identifier (relative this graph)
%% ['$e' | N]
%%
new_edge_id(G) ->
    NT = G#graph.ntab,
    [{'$eid', K}] = ets:lookup(NT, '$eid'),
    true = ets:delete(NT, '$eid'),
    true = ets:insert(NT, {'$eid', K+1}),
    ['$e' | K].

%%
%% Generate a "unique" vertex identifier (relative this graph)
%% ['$v' | N]
%%
new_vertex_id(G) ->
    NT = G#graph.ntab,
    [{'$vid', K}] = ets:lookup(NT, '$vid'),
    true = ets:delete(NT, '$vid'),
    true = ets:insert(NT, {'$vid', K+1}),
    ['$v' | K].

%%
%% Collect elements for a index in a tuple
%%
collect_elems(Keys, Table, Index) ->
    collect_elems(Keys, Table, Index, []).

collect_elems([{_,Key}|Keys], Table, Index, Acc) ->
    collect_elems(Keys, Table, Index,
		  [ets:lookup_element(Table, Key, Index)|Acc]);
collect_elems([], _, _, Acc) -> Acc.

do_add_vertex({V,Label}, G) ->
    ets:insert(G#graph.vtab, {V,Label}),
    V.

%%
%% Collect either source or sink vertices.
%%
collect_vertices(#graph{vtab=VT,ntab=NT}, Type) ->
    Vs = ets:select(VT, [{{'$1', '_'}, [], ['$1']}]),
    lists:foldl(fun(V, A) ->
			case ets:member(NT, {Type,V}) of
			    true -> A;
			    false -> [V|A]
			end
		end, [], Vs).

%%
%% Delete vertices
%%
do_del_vertices([V | Vs], G) ->
    do_del_vertex(V, G),
    do_del_vertices(Vs, G);
do_del_vertices([], #graph{}) -> true.

do_del_vertex(V, G) ->
    do_del_nedges(ets:lookup(G#graph.ntab, {in,V}), G),
    do_del_nedges(ets:lookup(G#graph.ntab, {out,V}), G),
    ets:delete(G#graph.vtab, V).

do_del_nedges([{_,E} | Ns], G) ->
    case ets:lookup(G#graph.etab, E) of
	[{E,V1,V2,_}] ->
	    do_del_edge(E,V1,V2,G),
	    do_del_nedges(Ns, G);
	[] ->
	    do_del_nedges(Ns, G)
    end;
do_del_nedges([], #graph{}) -> true.

%%
%% Delete edges
%%
do_del_edges([E | Es], G) ->
    case ets:lookup(G#graph.etab, E) of
	[{E,V1,V2,_}] ->
	    do_del_edge(E,V1,V2,G),
	    do_del_edges(Es, G);
	[] ->
	    do_del_edges(Es, G)
    end;
do_del_edges([], #graph{}) -> true.

do_del_edge(E,V1,V2,G) ->
    ets:select_delete(G#graph.ntab, [{{{in,V2},E},[],[true]},
				     {{{out,V1},E},[],[true]}]),
    ets:delete(G#graph.etab, E).

rm_edges([V1,V2|Vs], G) ->
    rm_edge(V1,V2,G),
    rm_edges([V2|Vs],G);
rm_edges(_, _) -> true.

rm_edge(V1,V2,G) ->
    Ns = ets:lookup(G#graph.ntab,{out,V1}),
    rm_edge_0(Ns,V1,V2,G).
    
rm_edge_0([{_,E}|Es],V1,V2,G) ->
    case ets:lookup(G#graph.etab,E) of
	[{E,V1,V2,_}]  ->
	    ets:delete(G#graph.etab,E),
	    rm_edge_0(Es,V1,V2,G);
	_ ->
	    rm_edge_0(Es,V1,V2,G)
    end;
rm_edge_0([],_,_,#graph{}) -> ok.
    
%%
%% Check that endpoints exists
%%
do_add_edge({E,V1,V2,Label}, G) ->
    case ets:member(G#graph.vtab, V1) of
	false -> {error, {bad_vertex, V1}};
	true  ->
	    case ets:member(G#graph.vtab, V2) of
		false -> {error, {bad_vertex,V2}};
		true when G#graph.cyclic =:= false ->
		    acyclic_add_edge(E, V1, V2, Label, G);
		true ->
		    do_insert_edge(E, V1, V2, Label, G)
	    end
    end.


do_insert_edge(E, V1, V2, Label, #graph{ntab=NT,etab=ET}) ->
    ets:insert(NT, [{{out,V1},E},{{in,V2},E}]),
    ets:insert(ET, {E,V1,V2,Label}),
    E.

acyclic_add_edge(_E, V1, V2, _L, _G) when V1 =:= V2 ->
    {error, {bad_edge, [V1, V2]}};
acyclic_add_edge(E,V1,V2,Label,G) ->
    case get_path(G,V2,V1) of
	false -> do_insert_edge(E,V1,V2,Label,G);
	Path -> {error, {bad_edge, Path}}
    end.

%%
%% Delete all paths from vertex V1 to vertex V2
%%

del_path(G,V1,V2) ->
    case get_path(G,V1,V2) of
	false -> true;
	Path ->
	    rm_edges([V1|Path], G),
	    del_path(G, V1, V2)
    end.

%%
%% Find a cycle through V
%% return the cycle as list of vertices [V ... V]
%% if no cycle exists false is returned
%% if only a cycle of length one exists it will be
%% returned as [V] but only after longer cycles have
%% be searched.
%%

get_cycle(G, V) ->
    case one_path(out_neighbours(G,V), V, [], [V], [V], 2, G, 1) of
	false ->
	    case lists:member(V, out_neighbours(G, V)) of
		true -> [V];
		false -> false
	    end;
	Vs -> Vs
    end.

%%
%% Find a path from V1 to V2
%% return the path as list of vertices [V1 ... V2]
%% if no path exists false is returned
%%

get_path(G, V1, V2) ->
    one_path(out_neighbours(G, V1), V2, [], [V1], [V1], 1, G, 1).

%%
%% prune_short_path (evaluate conditions on path)
%% short : if path is too short
%% ok    : if path is ok
%%
prune_short_path(Counter, Min) when Counter < Min ->
    short;
prune_short_path(_Counter, _Min) ->
    ok.

one_path([W|Ws], W, Cont, Xs, Ps, Prune, G, Counter) ->
    case prune_short_path(Counter, Prune) of
	short -> one_path(Ws, W, Cont, Xs, Ps, Prune, G, Counter);
	ok -> lists:reverse([W|Ps])
    end;
one_path([V|Vs], W, Cont, Xs, Ps, Prune, G, Counter) ->
    case lists:member(V, Xs) of
	true ->  one_path(Vs, W, Cont, Xs, Ps, Prune, G, Counter);
	false -> one_path(out_neighbours(G, V), W, 
			  [{Vs,Ps} | Cont], [V|Xs], [V|Ps], 
			  Prune, G, Counter+1)
    end;
one_path([], W, [{Vs,Ps}|Cont], Xs, _, Prune, G, Counter) ->
    one_path(Vs, W, Cont, Xs, Ps, Prune, G, Counter-1);
one_path([], _, [], _, _, _, _, _Counter) -> false.

%%
%% Like get_cycle/2, but a cycle of length one is preferred.
%%

get_short_cycle(G, V) ->
    get_short_path(G, V, V).

%%
%% Like get_path/3, but using a breadth-first search makes it possible
%% to find a short path.
%%

get_short_path(G, V1, V2) ->
    T = new(),
    add_vertex(T, V1),
    Q = queue:new(),
    Q1 = queue_out_neighbours(V1, G, Q),
    L = spath(Q1, G, V2, T),
    delete(T),
    L.
    
spath(Q, G, Sink, T) ->
    case queue:out(Q) of
	{{value, E}, Q1} ->
	    {_E, V1, V2, _Label} = edge(G, E),
	    if 
		Sink =:= V2 ->
		    follow_path(V1, T, [V2]);
		true ->
		    case vertex(T, V2) of
			false ->
			    add_vertex(T, V2),
			    add_edge(T, V2, V1),
			    NQ = queue_out_neighbours(V2, G, Q1),
			    spath(NQ, G, Sink, T);
			_V ->
			    spath(Q1, G, Sink, T)
		    end
	    end;
	{empty, _Q1} ->
	    false
    end.

follow_path(V, T, P) ->
    P1 = [V | P],
    case out_neighbours(T, V) of
	[N] ->
	    follow_path(N, T, P1);
	[] ->
	    P1
    end.

queue_out_neighbours(V, G, Q0) ->
    lists:foldl(fun(E, Q) -> queue:in(E, Q) end, Q0, out_edges(G, V)).
