%% -*- erlang-indent-level: 2 -*-
%% $Id$

-module(hipe_node_sets).

-export([new/0,
	 spilled/1,
	 colored/1,
	 add_spilled/2,
	 add_colored/2
	]).

-record(node_sets, 
	{spilled,    % Nodes marked for spilling
	 colored     % Nodes succesfully colored
	}).

spilled(Node_sets) -> Node_sets#node_sets.spilled.
colored(Node_sets) -> Node_sets#node_sets.colored.
    
set_spilled(Spilled, Node_sets) -> Node_sets#node_sets{spilled = Spilled}.
set_colored(Colored, Node_sets) -> Node_sets#node_sets{colored = Colored}.

new() ->
  #node_sets{spilled = [], colored = []}.

add_spilled(Node, Node_sets) ->
  set_spilled([Node | spilled(Node_sets)], Node_sets).

add_colored(Node, Node_sets) ->
  set_colored([Node | colored(Node_sets)], Node_sets).
