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

-export([new/0, new/1, delete/1]).

-export([add_vertex/1, add_vertex/2, add_vertex/3]).
-export([del_vertex/2, del_vertices/2]).
-export([vertex/2, vertices/1]).

-export([add_edge/3, add_edge/4, add_edge/5]).
-export([del_edge/2, del_edges/2, del_path/3]).
-export([edge/2, edges/1]).

-export([out_neighbours/2, in_neighbours/2]).
-export([out_edges/2, in_edges/2, edges/2]).
-export([out_degree/2, in_degree/2]).
-export([get_path/3, get_cycle/2]).


-record(graph, {
		vtab = notable,
		etab = notable,
		ntab = notable,
	        cyclic = true } ).

%%
%% Type is a list of
%%  protected | private | public
%%  acyclic | cyclic
%%
%%  default is [cyclic,protected]
%%
new() -> new([cyclic]).

new(Type) ->
    case check_type(Type, protected, []) of
	{error, What} -> {error, What};
	{Access,Ts} -> 
	    V = ets:new(vertices, [set,Access]),
	    E = ets:new(edges, [set,Access]),
	    N = ets:new(neighbours, [bag,Access]),
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
check_type([public | Ts], _, L) ->
    check_type(Ts, public, L);
check_type([T | _], _, _) -> 
    {error, {unknow_type, T}};
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

vertices(G) -> 
    collect_keys(ets:first(G#graph.vtab), G#graph.vtab).

in_degree(G, V) ->
    length(ets:lookup(G#graph.ntab,{in,V})).

in_neighbours(G,V) ->
    ET = G#graph.etab,
    NT = G#graph.ntab,
    collect_elems(ets:lookup(NT,{in,V}),ET,2).

in_edges(G, V) ->
    collect_objs(ets:lookup(G#graph.ntab,{in,V})).

out_degree(G, V) -> 
    length(ets:lookup(G#graph.ntab,{out,V})).

out_neighbours(G, V) -> 
    ET = G#graph.etab,
    NT = G#graph.ntab,
    collect_elems(ets:lookup(NT,{out,V}),ET,3).

out_edges(G, V) -> 
    collect_objs(ets:lookup(G#graph.ntab, {out,V})).

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

edges(G) ->
    collect_keys(ets:first(G#graph.etab), G#graph.etab).

edges(G, V) ->
    NT = G#graph.ntab,
    collect_objs(lists:append(ets:lookup(NT,{out,V}),ets:lookup(NT,{in,V}))).



edge(G, E) ->
    case ets:lookup(G#graph.etab,E) of
	[] -> false;
	[Edge] -> Edge
    end.

%%
%% Generate a "unique" edge identifier (relative this graph)
%% ['$e'|N]
%%
new_edge_id(G) ->
    ET = G#graph.etab,
    N = 
	case ets:lookup(ET,'$eid') of
	    [{'$eid',K}] -> K;
	    [] -> 1
	end,
    ets:insert(ET,{'$eid',N+1}),
    ['$e'|N].

%%
%% Generate a "unique" vertex identifier (relative this graph)
%% ['$v'|N]
%%
new_vertex_id(G) ->
    VT = G#graph.vtab,
    N =
	case ets:lookup(VT,'$vid') of
	    [{'$vid',K}] -> K;
	    [] -> 1
	end,
    ets:insert(VT,{'$vid',N+1}),
    ['$v'|N].

%%
%% Scan table for all keys
%%
collect_keys(Key, Table) ->
    collect_keys(Key, Table, []).

collect_keys('$end_of_table', _, Keys) -> Keys;
collect_keys('$vid', Table, L) ->
    collect_keys(ets:next(Table,'$vid'), Table, L);
collect_keys('$eid', Table, L) ->
    collect_keys(ets:next(Table,'$eid'), Table, L);
collect_keys(Key, Table, L) ->
    collect_keys(ets:next(Table,Key), Table, [Key|L]).

%%
%% Collect elements for a index in a tuple
%%
collect_elems(Keys, Table, Index) ->
    collect_elems(Keys, Table, Index,[]).

collect_elems([{_,Key}|Keys], Table, Index, Acc) ->
    case ets:lookup(Table, Key) of
	[X] when tuple(X),size(X) >= Index ->
	    collect_elems(Keys, Table, Index, [element(Index,X)|Acc]);
	_ ->
	    collect_elems(Keys, Table, Index, Acc)
    end;
collect_elems([], _, _, Acc) -> Acc.

%%
%% Remove keys from a {Key,Value} list
%%
collect_objs(Ls) ->
    collect_objs(Ls, []).
    
collect_objs([{Key,Value}|T],Acc) ->
    collect_objs(T, [Value|Acc]);
collect_objs([], Acc) -> Acc.


do_add_vertex({V,Data}, G) ->
    ets:insert(G#graph.vtab, {V,Data}),
    V.

%%
%% Delete vertices
%%
do_del_vertices([V | Vs], G) ->
    do_del_vertex(V, G),
    do_del_vertices(Vs, G);
do_del_vertices([], _) -> true.


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
do_del_nedges([], _) -> true.

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
do_del_edges([], _) -> true.

do_del_edge(E,V1,V2,G) ->
    ets:match_delete(G#graph.ntab, {{in,V2},E}),
    ets:match_delete(G#graph.ntab, {{out,V1},E}),
    ets:delete(G#graph.etab, E).


rm_edges([V1,V2|Vs], G) ->
    rm_edge(V1,V2,G),
    rm_edges([V2|Vs],G);
rm_edges(_,_) -> true.

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
rm_edge_0([],_,_,_) -> ok.
    
%%
%% Check that endpoints exists
%%
do_add_edge({E,V1,V2,Data}, G) ->
    case ets:lookup(G#graph.vtab,V1) of
	[] -> {error, {bad_vertex, V1}};
	_  ->
	    case ets:lookup(G#graph.vtab,V2) of
		[] -> {error, {bad_vertex,V2}};
		_  ->
		    if
			G#graph.cyclic == false ->
			    acyclic_add_edge(E,V1,V2,Data,G);
			true ->
			    do_insert_edge(E,V1,V2,Data,G)
		    end
	    end
    end.


do_insert_edge(E,V1,V2,Data,G) ->
    NT = G#graph.ntab,
    ets:insert(NT, {{out,V1},E}),
    ets:insert(NT, {{in,V2},E}),
    ets:insert(G#graph.etab, {E,V1,V2,Data}),
    E.

acyclic_add_edge(E,V1,V2,Data,G) ->
    case get_path(G,V2,V1) of
	false -> do_insert_edge(E,V1,V2,Data,G);
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
    case one_path(out_neighbours(G,V), V, [], [], [V], {2,infinity}, G) of
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
    one_path(out_neighbours(G, V1), V2, [], [], [V1], {1,infinity}, G).

%%
%% prune_path (evaluate conditions on path)
%% long  : if path is too long
%% short : if path is to short
%% ok    : if path is ok
%%
prune_path(Path, {Min,Max}) ->
    N = length(Path),
    if
	N < Min -> short;
	Max == infinite -> ok;
	N > Max -> long;
	true -> ok
    end.

one_path([W|Ws], W, Cont, Xs, Ps, Prune, G) ->
    case prune_path(Ps, Prune) of
	long  -> one_path([], W, Cont, Xs, Ps, Prune, G);
	short -> one_path(Ws, W, Cont, Xs, Ps, Prune, G);
	ok -> lists:reverse([W|Ps])
    end;
one_path([V|Vs], W, Cont, Xs, Ps, Prune, G) ->
    case prune_path(Ps, Prune) of
	long -> one_path([], W, Cont, Xs, Ps, Prune, G);
	_ ->
	    case lists:member(V, Xs) of
		true ->  one_path(Vs, W, Cont, Xs, Ps, Prune, G);
		false -> one_path(out_neighbours(G, V), W, 
				  [{Vs,Ps} | Cont], [V|Xs], [V|Ps], Prune, G)
	    end
    end;
one_path([], W, [{Vs,Ps}|Cont], Xs, _, Prune, G) ->
    one_path(Vs, W, Cont, Xs, Ps, Prune, G);
one_path([], _, [], _, _, _, _) -> false.
