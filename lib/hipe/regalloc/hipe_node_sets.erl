%%%----------------------------------------------------------------------
%%% File    : node_sets.erl
%%% Author  : Andreas Wallin <d96awa@zeppo.it.uu.se>, Thorild
%%% Purpose : This data-structure encapsulates all node sets.
%%% Created : 18 Mar 2000 by Andreas Wallin <d96awa@zeppo.it.uu.se>
%%%----------------------------------------------------------------------

-module(hipe_node_sets).
-author('d96awa@zeppo.it.uu.se, Thorild').

-export([new/4,
	 initial/1,
	 precolored/1,
	 spilled/1,
	 coalesced/1,
	 colored/1,
	 add/3,
	 remove/3,
	 member/3,
	 is_empty/2
	]).

-record(node_sets, 
	{initial,    % Temporary registers, not precolored
	 precolored, % Machine registers, preassigned a color
	 spilled,    % Nodes marked for spilling
	 coalesced,  % Registers that have been coalesced
	 colored     % Nodes succesfully colored
	}).


%%%----------------------------------------------------------------------
% Function:    initial, precolored, spilled, coalesced, colored
%
% Description: Selectors for node_sets structure.
%
% Parameters:
%   Node_sets     --  A Node_sets data-structure
%
% Returns:
%   The specified element you want to extract from Node_sets
%%%----------------------------------------------------------------------
initial(Node_sets)    -> Node_sets#node_sets.initial.
precolored(Node_sets) -> Node_sets#node_sets.precolored.
spilled(Node_sets)    -> Node_sets#node_sets.spilled.
coalesced(Node_sets)  -> Node_sets#node_sets.coalesced.
colored(Node_sets)    -> Node_sets#node_sets.colored.
    
%%%----------------------------------------------------------------------
% Function:    Modifiers
%
% Description: Modifiers for the node_sets data-structure.
% Parameters:
%   Set           --  The set you want to set
%   Node_sets     --  A Node_sets data-structure
%
% Returns:
%   The specified element you want to extract from Node_sets
%%%----------------------------------------------------------------------
set_initial(Initial, Node_sets)       -> Node_sets#node_sets{initial = Initial}.
set_precolored(Precolored, Node_sets) -> Node_sets#node_sets{precolored = Precolored}.
set_spilled(Spilled, Node_sets)       -> Node_sets#node_sets{spilled = Spilled}.
set_coalesced(Coalesced, Node_sets)   -> Node_sets#node_sets{coalesced = Coalesced}.
set_colored(Colored, Node_sets)       -> Node_sets#node_sets{colored = Colored}.

%%%----------------------------------------------------------------------
% Function:    new
%
% Description: Creates a new node_sets data-structure. This is the
%               function that is target general.
% Parameters:
%   Min_temporary  -- The minimum temporary number
%   Max_temporary  -- The maximum temporary number
%   Target         -- Target module name
%
% Returns:
%   A new node_sets data-structure.
%%%----------------------------------------------------------------------
new(Target, Min_temporary, Max_temporary, NonAlloc) ->
    #node_sets{
     initial    = non_precolored(Target, Min_temporary, Max_temporary,
				 []) -- [Target:reg_nr(X) || X <- NonAlloc],
     precolored = Target:all_precolored(),
     spilled    = ordsets:new(),
     coalesced  = ordsets:new(),
     colored    = ordsets:new()
    }.


%%%----------------------------------------------------------------------
% Function:    add
%
% Description: Add a node to one of the node sets.
%
% Parameters:
%   Set            -- One of initial, precolored, spilled, coalesced,
%                       colored
%   Node           -- The node you want to insert.
%   Node_sets      -- A node_sets data-structure. 
%
% Returns:
%   An updated node_sets data-structure.
%%%----------------------------------------------------------------------
add(initial, Node, Node_sets) ->
    set_initial(ordsets:add_element(Node, initial(Node_sets)), Node_sets);
add(precolored, Node, Node_sets) ->
    set_precolored(ordsets:add_element(Node, precolored(Node_sets)), Node_sets);
add(spilled, Node, Node_sets) ->
    set_spilled(ordsets:add_element(Node, spilled(Node_sets)), Node_sets);
add(coalesced, Node, Node_sets) ->
    set_coalesced(ordsets:add_element(Node, coalesced(Node_sets)), Node_sets);
add(colored, Node, Node_sets) ->
    set_colored(ordsets:add_element(Node, colored(Node_sets)), Node_sets).


%%%----------------------------------------------------------------------
% Function:    member
%
% Description: Check if a node is a member of a selected set.
%
% Parameters:
%   Set            -- One of initial, precolored, spilled, coalesced,
%                       colored
%   Node           -- The node you want to insert.
%   Node_sets      -- A node_sets data-structure. 
%
% Returns:
%   true  --  If Node is a member of selected set
%   false --  Otherwise
%%%----------------------------------------------------------------------
member(initial, Node, Node_sets) ->
    ordsets:is_element(Node, initial(Node_sets));
member(precolored, Node, Node_sets) ->
    ordsets:is_element(Node, precolored(Node_sets));
member(spilled, Node, Node_sets) ->
    ordsets:is_element(Node, spilled(Node_sets));
member(coalesced, Node, Node_sets) ->
    ordsets:is_element(Node, coalesced(Node_sets));
member(colored, Node, Node_sets) ->
    ordsets:is_element(Node, colored(Node_sets)).


%%%----------------------------------------------------------------------
% Function:    remove
%
% Description: Removes a node from a selected set.
%
% Parameters:
%   Set            -- One of initial, precolored, spilled, coalesced,
%                       colored
%   Node           -- The node you want to remove.
%   Node_sets      -- A node_sets data-structure. 
%
% Returns:
%   An updated node_sets data-structure with Node removed from 
%    selected set.
%%%----------------------------------------------------------------------
remove(initial, Node, Node_sets) ->
    set_initial(ordsets:del_element(Node, initial(Node_sets)), Node_sets);
remove(precolored, Node, Node_sets) ->
    set_precolored(ordsets:del_element(Node, precolored(Node_sets)), Node_sets);
remove(spilled, Node, Node_sets) ->
    set_spilled(ordsets:del_element(Node, spilled(Node_sets)), Node_sets);
remove(coalesced, Node, Node_sets) ->
    set_coalesced(ordsets:del_element(Node, coalesced(Node_sets)), Node_sets);
remove(colored, Node, Node_sets) ->
    set_colored(ordsets:del_element(Node, colored(Node_sets)), Node_sets).


%%%----------------------------------------------------------------------
% Function:    is_empty
%
% Description: Checks if one of the sets is empty.
%
% Parameters:
%   Set            -- One of initial, precolored, spilled, coalesced,
%                       colored
%   Node_sets      -- A node_sets data-structure. 
%
% Returns:
%   true  --  If selected set is empty.
%   false --  Otherwise
%%%----------------------------------------------------------------------
is_empty(initial, Node_sets) ->
    initial(Node_sets) == [];
is_empty(precolored, Node_sets) ->
    precolored(Node_sets) == [];
is_empty(spilled, Node_sets) ->
    spilled(Node_sets) == [];
is_empty(coalesced, Node_sets) ->
    coalesced(Node_sets) == [];
is_empty(colored, Node_sets) ->
    colored(Node_sets) == [].

%%----------------------------------------------------------------------
% Function:    non_precolored
%
% Description: Generates a list with temporaries that's not 
%               precolored within the range Current-Max_temporary.
%
% Parameters:
%   Target         -- Target specific module.
%   Current        -- The minimum temporary number
%   Max_temporary  -- The maximum temporary number
%   Initial        -- An empty list
%
% Returns:
%   A list of temporaries that's not precolored.
%%----------------------------------------------------------------------
non_precolored(Target,Max_temporary, Max_temporary, Initial) -> 
    case Target:is_precolored(Max_temporary) of
	true ->
	    Initial;
	false ->
	    [Max_temporary|Initial]
    end;
non_precolored(Target, Current, Max_temporary, Initial) ->
    case Target:is_precolored(Current) of
	true ->
	    non_precolored(Target,Current+1, Max_temporary, Initial);
	false ->
	    non_precolored(Target,Current+1, Max_temporary, [Current|Initial])
    end.
