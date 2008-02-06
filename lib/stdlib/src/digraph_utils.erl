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
%% Portions created by Ericsson are Copyright 2000, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(digraph_utils).

%%% Operations on directed (and undirected) graphs.
%%%
%%% Implementation based on Launchbury, John: Graph Algorithms with a 
%%% Functional Flavour, in Jeuring, Johan, and Meijer, Erik (Eds.): 
%%% Advanced Functional Programming, Lecture Notes in Computer 
%%% Science 925, Springer Verlag, 1995.

-export([components/1, strong_components/1, cyclic_strong_components/1, 
	 reachable/2, reachable_neighbours/2, 
	 reaching/2, reaching_neighbours/2,
	 topsort/1, is_acyclic/1, 
         arborescence_root/1, is_arborescence/1, is_tree/1, 
         loop_vertices/1,
	 subgraph/2, subgraph/3, condensation/1,
	 preorder/1, postorder/1]).

%%
%%  Exported functions
%%

components(G) ->
    forest(G, fun inout/3).
    
strong_components(G) ->
    forest(G, fun in/3, revpostorder(G)).

cyclic_strong_components(G) ->
    remove_singletons(strong_components(G), G, []).

reachable(Vs, G) when is_list(Vs) ->
    lists:append(forest(G, fun out/3, Vs, first)).
    
reachable_neighbours(Vs, G) when is_list(Vs) ->
    lists:append(forest(G, fun out/3, Vs, not_first)).
    
reaching(Vs, G) when is_list(Vs) ->
    lists:append(forest(G, fun in/3, Vs, first)).
    
reaching_neighbours(Vs, G) when is_list(Vs) ->
    lists:append(forest(G, fun in/3, Vs, not_first)).
    
topsort(G) ->
    L = revpostorder(G),
    case length(forest(G, fun in/3, L)) =:= length(digraph:vertices(G)) of
	true  -> L;
	false -> false
    end.

is_acyclic(G) ->
    case loop_vertices(G) of
	[] -> topsort(G) =/= false;
	_  -> false
    end.
    
arborescence_root(G) ->
    case digraph:no_edges(G) =:= digraph:no_vertices(G) - 1 of
        true ->
            try
                F = fun(V, Z) ->
                            case digraph:in_degree(G, V) of
                                1 -> Z;
                                0 when Z =:= [] -> [V]
                            end
                    end,
                [Root] = lists:foldl(F, [], digraph:vertices(G)),
                {yes, Root}
            catch _:_ ->
                no
            end;
        false ->
            no
    end.

is_arborescence(G) ->
    arborescence_root(G) =/= no.

is_tree(G) ->
    (digraph:no_edges(G) =:= digraph:no_vertices(G) - 1)
    andalso (length(components(G)) =:= 1).

loop_vertices(G) ->
    lists:filter(fun(V) -> is_reflexive_vertex(V, G) end, digraph:vertices(G)).

subgraph(G, Vs) ->
    subgraph_opts(G, Vs, []).

subgraph(G, Vs, Opts) ->
    subgraph_opts(G, Vs, Opts).

condensation(G) ->
    SCs = strong_components(G),
    %% Each component is assigned a number.
    %% V2I: from vertex to number.
    %% I2C: from number to component.
    V2I = ets:new(condensation, []),
    I2C = ets:new(condensation, []),
    CFun = fun(SC, N) -> lists:foreach(fun(V) -> 
					  true = ets:insert(V2I, {V,N}) 
				       end, 
				       SC), 
			 true = ets:insert(I2C, {N, SC}), 
			 N + 1 
	   end,
    lists:foldl(CFun, 1, SCs),
    SCG = subgraph_opts(G, [], []),
    lists:foreach(fun(SC) -> condense(SC, G, SCG, V2I, I2C) end, SCs),
    ets:delete(V2I),
    ets:delete(I2C),
    SCG.
			   
preorder(G) ->
    lists:reverse(revpreorder(G)).

postorder(G) ->
    lists:reverse(revpostorder(G)).

%%
%%  Local functions
%%

forest(G, SF) ->
    forest(G, SF, digraph:vertices(G)).

forest(G, SF, Vs) ->
    forest(G, SF, Vs, first).

forest(G, SF, Vs, HandleFirst) ->
    T = ets:new(forest, [set]),
    F = fun(V, LL) -> pretraverse(HandleFirst, V, SF, G, T, LL) end,
    LL = lists:foldl(F, [], Vs),
    ets:delete(T),
    LL.

pretraverse(first, V, SF, G, T, LL) ->
    ptraverse([V], SF, G, T, [], LL);
pretraverse(not_first, V, SF, G, T, LL) ->
    case ets:member(T, V) of
	false -> ptraverse(SF(G, V, []), SF, G, T, [], LL);
	true  -> LL
    end.

ptraverse([V | Vs], SF, G, T, Rs, LL) ->
    case ets:member(T, V) of
	false ->
	    ets:insert(T, {V}),
	    ptraverse(SF(G, V, Vs), SF, G, T, [V | Rs], LL);
	true ->
	    ptraverse(Vs, SF, G, T, Rs, LL)
    end;
ptraverse([], _SF, _G, _T, [], LL) ->
    LL;
ptraverse([], _SF, _G, _T, Rs, LL) ->
    [Rs | LL].

revpreorder(G) ->
    lists:append(forest(G, fun out/3)).

revpostorder(G) ->
    T = ets:new(forest, [set]),
    L = posttraverse(digraph:vertices(G), G, T, []),
    ets:delete(T),
    L.

posttraverse([V | Vs], G, T, L) ->
    L1 = case ets:member(T, V) of
	     false ->
		 ets:insert(T, {V}),
		 [V | posttraverse(out(G, V, []), G, T, L)];
	     true ->
		 L
	 end,
    posttraverse(Vs, G, T, L1);
posttraverse([], _G, _T, L) ->
    L.

in(G, V, Vs) ->
    digraph:in_neighbours(G, V) ++ Vs.

out(G, V, Vs) ->
    digraph:out_neighbours(G, V) ++ Vs.

inout(G, V, Vs) ->
    in(G, V, out(G, V, Vs)).

remove_singletons([C=[V] | Cs], G, L) ->
    case is_reflexive_vertex(V, G) of
	true  -> remove_singletons(Cs, G, [C | L]);
	false -> remove_singletons(Cs, G, L)
    end;
remove_singletons([C | Cs], G, L) ->
    remove_singletons(Cs, G, [C | L]);
remove_singletons([], _G, L) ->
    L.

is_reflexive_vertex(V, G) ->
    lists:member(V, digraph:out_neighbours(G, V)).

subgraph_opts(G, Vs, Opts) ->
    subgraph_opts(Opts, inherit, true, G, Vs).

subgraph_opts([{type, Type} | Opts], _Type0, Keep, G, Vs)
  when Type =:= inherit; is_list(Type) ->
    subgraph_opts(Opts, Type, Keep, G, Vs);
subgraph_opts([{keep_labels, Keep} | Opts], Type, _Keep0, G, Vs)
  when Keep; not Keep ->
    subgraph_opts(Opts, Type, Keep, G, Vs);
subgraph_opts([], inherit, Keep, G, Vs) ->
    Info = digraph:info(G),
    {_, {_, Cyclicity}} = lists:keysearch(cyclicity, 1, Info),
    {_, {_, Protection}} = lists:keysearch(protection, 1, Info),
    subgraph(G, Vs, [Cyclicity, Protection], Keep);
subgraph_opts([], Type, Keep, G, Vs) ->
    subgraph(G, Vs, Type, Keep);
subgraph_opts([Opt | _], _Type, _Keep, _G, _Vs) ->
    {error, {invalid_option, Opt}}.

subgraph(G, Vs, Type, Keep) ->
    case digraph:new(Type) of
	Error = {error, _} ->
	    Error;
	SG ->
	    lists:foreach(fun(V) -> subgraph_vertex(V, G, SG, Keep) end, Vs),
	    EFun = fun(V) -> lists:foreach(fun(E) -> 
					       subgraph_edge(E, G, SG, Keep) 
                                           end,
                                           digraph:out_edges(G, V))
		   end,
	    lists:foreach(EFun, digraph:vertices(SG)),
	    SG
    end.

subgraph_vertex(V, G, SG, Keep) ->
    case digraph:vertex(G, V) of
	false -> ok;
	_ when not Keep -> digraph:add_vertex(SG, V);
	{_V, Label} when Keep -> digraph:add_vertex(SG, V, Label)
    end.

subgraph_edge(E, G, SG, Keep) ->
    {_E, V1, V2, Label} = digraph:edge(G, E),
    case digraph:vertex(SG, V2) of
	false -> ok;
	_ when not Keep -> digraph:add_edge(SG, E, V1, V2, []);
	_ when Keep -> digraph:add_edge(SG, E, V1, V2, Label)
    end.

condense(SC, G, SCG, V2I, I2C) ->
    T = ets:new(condense, []),
    NFun = fun(Neighbour) ->
		   [{_V,I}] = ets:lookup(V2I, Neighbour),
		   ets:insert(T, {I})
	   end,
    VFun = fun(V) -> lists:foreach(NFun, digraph:out_neighbours(G, V)) end,
    lists:foreach(VFun, SC),
    digraph:add_vertex(SCG, SC),
    condense(ets:first(T), T, SC, G, SCG, I2C),
    ets:delete(T).

condense('$end_of_table', _T, _SC, _G, _SCG, _I2C) ->
    ok;
condense(I, T, SC, G, SCG, I2C) ->
    [{_,C}] = ets:lookup(I2C, I),
    digraph:add_vertex(SCG, C),
    digraph:add_edge(SCG, SC, C),
    condense(ets:next(T, I), T, SC, G, SCG, I2C).

