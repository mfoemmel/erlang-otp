%%% -*- erlang-indent-level: 2 -*-
%%% $Id$
%%% Implements sets of adjacent nodes.
%%% Symmetry implies that when (U,V) is a member, then so is (V,U).
%%% Hence, only (U,V), where U<V, is actually stored.

-module(hipe_adj_set).
-export([new/0,
	 add_edge/3,
	 adjacent/3]).

new() ->
  gb_sets:empty().

add_edge(U, V, Set) ->
  if U =:= V -> Set;
     U < V -> gb_sets:add_element({U,V}, Set);
     true -> gb_sets:add_element({V,U}, Set)
  end.

adjacent(U, V, Set) ->
  if U < V -> gb_sets:is_member({U,V}, Set);
     true -> gb_sets:is_member({V,U}, Set)
  end.
