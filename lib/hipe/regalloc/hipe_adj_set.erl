%%%----------------------------------------------------------------------
%%% File    : hipe_adj_set.erl
%%% Author  : Andreas Wallin <d96awa@ida.dis.uu.se>
%%% Purpose : Used to represent nodes that are connected with edges.
%%%            This is a much faster way to check if to nodes are
%%%            connected than checking with the adj_list 
%%%            data-structure.
%%% Created : 03 Feb 2000 by Andreas Wallin <d96awa@ida.dis.uu.se>
%%%----------------------------------------------------------------------

-module(hipe_adj_set).
-author("Andreas Wallin").
-export([new/0, 
	 add_edge/3, 
	 add_edges/3, 
	 remove_edge/3, 
	 remove_edges/3, 
	 adjacent/3]).


%%%----------------------------------------------------------------------
% Function:    new
%
% Description: Creates a new adj_set data structure. It is used to
%               represent all edges that are connected in an
%               undirected graph.
%
% Parameters:
%   None
%
% Returns: 
%   A new adj_set data structure.
%%%----------------------------------------------------------------------
new() ->
    hipe_hash:empty().

%%%----------------------------------------------------------------------
% Function:    add_edge
%
% Description: Adds a new edge between U and V
%
% Parameters:
%   U              -- A node
%   V              -- A node
%   Set            -- An adj_set data-structure
%
% Returns: 
%   An updated adj_set data structure
%%%----------------------------------------------------------------------
add_edge(U, U, Set) -> Set;
add_edge(U, V, Set) ->
    case adjacent(U, V, Set) of 
	true  -> Set; % Ok, do nothing. It was already in set
	false -> Set1 = hipe_hash:insert({U, V}, interfere, Set),
		 hipe_hash:insert({V, U}, interfere, Set1);
	_ -> error_logger:error_msg("[~w:add_edge] Could not happen situation",[?MODULE])
    end.

%%%----------------------------------------------------------------------
% Function:    add_edges
%
% Description: Adds edges between the node From to a number of other
%               nodes [T|Ts]
%
% Parameters:
%   From           -- A node that you which to connect to a number of 
%                      other nodes.
%   [T|Ts]         -- A list of nodes that you which to connect with 
%                      the From node
%   Set            -- An adj_set datastructure
%
% Returns: 
%   An updated adj_set data structure
%%%----------------------------------------------------------------------
add_edges(_, [], Set) -> Set;
add_edges(From, [T|Ts], Set) ->
    add_edges(From, Ts, add_edge(From, T, Set)).

%%%----------------------------------------------------------------------
% Function:    remove_edge
%
% Description: Removes the edge between U and V
%
% Parameters:
%   U              -- A node
%   V              -- A node
%   Set            -- An adj_set datastructure
%
% Returns: 
%   If the edge exists  --  An updated adj_set data structure
%   Otherwise           --  Throws an exception
%%%----------------------------------------------------------------------
remove_edge(U, U, Set) -> Set;
remove_edge(U, V, Set) ->
    case adjacent(U, V, Set) of 
	true -> Set1 = hipe_hash:delete({U, V}, Set),
		hipe_hash:delete({V, U}, Set1);
	false -> throw({adj_set, remove_directed_edge, "Edge does not exist"})
    end.


%%%----------------------------------------------------------------------
% Function:    remove_edges
%
% Description: Removes edges between the node From and a number of other
%               nodes [T|Ts]
%
% Parameters:
%   From           -- A node that you which to remove edges to from
%   [T|Ts]         -- A list of nodes that you don't want to have an
%                      edge to the From node any more.
%   Set            -- An adj_set datastructure
%
% Returns: 
%    If all edges exists  --  An updated adj_set data structure
%    Otherwise            --  Throws an exception
%%%----------------------------------------------------------------------
remove_edges(_, [], Set) -> Set;
remove_edges(From, [T|Ts], Set) ->
    remove_edges(From, Ts, remove_edge(From, T, Set)).


%%%----------------------------------------------------------------------
% Function:    adjacent
%
% Description: Tells if an edge exists between U and V
%
% Parameters:
%   U              -- A node
%   V              -- A node
%   Set            -- An adj_set datastructure
%
% Returns: 
%   true   --  If there exists an edge between U and V
%   false  --  Otherwise
%%%----------------------------------------------------------------------
adjacent(U, V, Set) ->
    case hipe_hash:lookup({U, V}, Set) of 
	{found, _} -> true;
	not_found -> false;
	_ -> exit({adj_set, adjacent, "Could not happend"})
    end.


