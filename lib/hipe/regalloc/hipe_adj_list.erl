%% -*- erlang-indent-level: 2 -*-
%%----------------------------------------------------------------------
%% File    : hipe_adj_list.erl
%% Author  : Andreas Wallin <d96awa@it.uu.se>
%% Purpose : Keeps track of adjacency lists for the inference graph.
%% Created : 18 Mar 2000 by Andreas Wallin <d96awa@it.uu.se>
%%----------------------------------------------------------------------

-module(hipe_adj_list).
-author("Andreas Wallin").
-export([new/1, 
	 add_edge/3, 
	 %% add_edges/3, 
	 %% remove_edge/3, 
	 %% remove_edges/3, 
	 edges/2]).

%%----------------------------------------------------------------------
%% Function:    new
%%
%% Description: Creates an empty structure for adjacency lists
%%
%% Parameters:
%%   Max_nodes          -- Limit for node numbers
%%   
%% Returns:
%%   Empty adj_list structure
%%
%%----------------------------------------------------------------------

new(Max_nodes) ->
    hipe_vectors_wrapper:empty(Max_nodes, ordsets:new()).

%%----------------------------------------------------------------------
%% Function:    add_edges
%%
%% Description: Adds edges from a node to other nodes
%%
%% Parameters:
%%   U                  -- A node
%%   Vs                 -- Nodes to add edges to
%%   Adj_list           -- Old adjacency lists
%%   
%% Returns:
%%   An updated adj_list data-structure
%%
%%----------------------------------------------------------------------

%%add_edges(_, [], Adj_list) -> Adj_list;
%%add_edges(U, Vs, Adj_list) when is_list(Vs), is_integer(U) ->
%%    hipe_vectors_wrapper:set(Adj_list, U, ordsets:union(Vs, hipe_vectors_wrapper:get(Adj_list, U))).

%%----------------------------------------------------------------------
%% Function:    add_edge
%%
%% Description: Creates an edge between two nodes
%%
%% Parameters:
%%   U                  -- A node
%%   V                  -- Another node
%%   Adj_list           -- Old adjacency lists
%%   
%% Returns:
%%   New adj_list data-structure with (U and V connected)
%%
%%----------------------------------------------------------------------

add_edge(U, U, Adj_list) -> Adj_list;
add_edge(U, V, Adj_list) when is_integer(U), is_integer(V) ->
    hipe_vectors_wrapper:set(Adj_list, U,
			     ordsets:add_element(V, hipe_vectors_wrapper:get(Adj_list, U))).

%%----------------------------------------------------------------------
%% Function:    remove_edges
%%
%% Description: Removes edges from a node to other nodes
%%
%% Parameters:
%%   U                  -- A node
%%   Vs                 -- Nodes to remove edges to
%%   Adj_list           -- Old adjacency lists
%%   
%% Returns:
%%   An updated adj_list data-structure
%%
%%----------------------------------------------------------------------

%% remove_edges(_, [], Adj_list) -> Adj_list;
%% remove_edges(U, Vs, Adj_list) when is_list(Vs), is_integer(U) ->
%%     hipe_vectors_wrapper:set(Adj_list, U, ordsets:subtract(hipe_vectors_wrapper:get(Adj_list, U), Vs)).

%%----------------------------------------------------------------------
%% Function:    remove_edge
%%
%% Description: Removes an edge between two nodes
%%
%% Parameters:
%%   U                  -- A node
%%   V                  -- Another node
%%   Adj_list           -- Old adjacency lists
%%   
%% Returns:
%%   New adjacency lists with (U and V not connected)
%%
%%----------------------------------------------------------------------

%% remove_edge(U, U, Adj_list) -> Adj_list;
%% remove_edge(U, V, Adj_list) when is_integer(U), is_integer(V) ->
%%     remove_edges(U,  [V], Adj_list).

%%----------------------------------------------------------------------
%% Function:    edges
%%
%% Description: Tells where the edges of a node go
%%
%% Parameters:
%%   U                  -- A node
%%   Adj_list           -- Adjacency lists
%%   
%% Returns:
%%   The set of nodes connected to U
%%
%%----------------------------------------------------------------------

edges(U, Adj_list) when is_integer(U) ->
    hipe_vectors_wrapper:get(Adj_list, U).
