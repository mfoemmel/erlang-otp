%%----------------------------------------------------------------------
%% File    : node_sets.erl
%% Author  : Andreas Wallin <d96awa@it.uu.se>
%% Purpose : This data-structure encapsulates all node sets.
%% Created : 18 Mar 2000 by Andreas Wallin <d96awa@it.uu.se>
%%----------------------------------------------------------------------

-module(hipe_node_sets).
-author('d96awa@it.uu.se').

-export([new/4,
	 initial/1,
	 precoloured/1,
	 spilled/1,
	 coalesced/1,
	 colored/1,
	 add_spilled/2,
	 add_coalesced/2,
	 add_colored/2
	 %% remove/3,
	 %% is_empty/2,
	 %% member_precoloured/2,
	 %% member_coalesced/2,
	 %% member_colored/2
	]).

-record(node_sets, 
	{initial,    % Temporary registers, not precoloured
	 target,     % Target module with info on precoloured registers
	 spilled,    % Nodes marked for spilling
	 coalesced,  % Registers that have been coalesced
	 colored     % Nodes succesfully colored
	}).


%%%----------------------------------------------------------------------
% Function:    initial, precoloured, spilled, coalesced, colored
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
precoloured(Node_sets)-> (Node_sets#node_sets.target):all_precoloured().
spilled(Node_sets)    -> Node_sets#node_sets.spilled.
coalesced(Node_sets)  -> Node_sets#node_sets.coalesced.
colored(Node_sets)    -> Node_sets#node_sets.colored.
    
%%%----------------------------------------------------------------------
%% Function:    Modifiers
%%
%% Description: Modifiers for the node_sets data-structure.
%% Parameters:
%%   Set           --  The set you want to set
%%   Node_sets     --  A Node_sets data-structure
%%
%% Returns:
%%   The specified element you want to extract from Node_sets
%%%----------------------------------------------------------------------

set_spilled(Spilled, Node_sets)       -> Node_sets#node_sets{spilled = Spilled}.
set_coalesced(Coalesced, Node_sets)   -> Node_sets#node_sets{coalesced = Coalesced}.
set_colored(Colored, Node_sets)       -> Node_sets#node_sets{colored = Colored}.

%%%----------------------------------------------------------------------
%% Function:    new
%%
%% Description: Creates a new node_sets data-structure. This is the
%%               function that is target general.
%% Parameters:
%%   Min_temporary  -- The minimum temporary number
%%   Max_temporary  -- The maximum temporary number
%%   Target         -- Target module name
%%
%% Returns:
%%   A new node_sets data-structure.
%%%----------------------------------------------------------------------

new(Target, Min_temporary, Max_temporary, NonAlloc) ->
    #node_sets{
     initial    = non_precoloured(Target, Min_temporary, Max_temporary,
				 []) -- [Target:reg_nr(X) || X <- NonAlloc],
     target	= Target,
     spilled    = ordsets:new(),
     coalesced  = ordsets:new(),
     colored    = ordsets:new()
    }.

%%%----------------------------------------------------------------------
%% Function:    add
%%
%% Description: Add a node to one of the node sets.
%%
%% Parameters:
%%   Node           -- The node you want to insert.
%%   Node_sets      -- A node_sets data-structure. 
%%
%% Returns:
%%   An updated node_sets data-structure.
%%%----------------------------------------------------------------------

add_spilled(Node, Node_sets) ->
  set_spilled(ordsets:add_element(Node, spilled(Node_sets)), Node_sets).

add_coalesced(Node, Node_sets) ->
  set_coalesced(ordsets:add_element(Node, coalesced(Node_sets)), Node_sets).

add_colored(Node, Node_sets) ->
  set_colored(ordsets:add_element(Node, colored(Node_sets)), Node_sets).

%%%----------------------------------------------------------------------
%% Function:    member
%%
%% Description: Check if a node is a member of a selected set.
%%
%% Parameters:
%%   Node           -- The node you want to insert.
%%   Node_sets      -- A node_sets data-structure. 
%%
%% Returns:
%%   true  --  If Node is a member of selected set
%%   false --  Otherwise
%%%----------------------------------------------------------------------

%% member_precoloured(Node, Node_sets) ->
%%   (Node_sets#node_sets.target):is_precoloured(Node).

%% member_coalesced(Node, Node_sets) ->
%%   ordsets:is_element(Node, coalesced(Node_sets)).

%% member_colored(Node, Node_sets) ->
%%   ordsets:is_element(Node, colored(Node_sets)).

%%%----------------------------------------------------------------------
%% Function:    remove
%%
%% Description: Removes a node from a selected set.
%%
%% Parameters:
%%   Set            -- One of initial, precoloured, spilled, coalesced,
%%                       colored
%%   Node           -- The node you want to remove.
%%   Node_sets      -- A node_sets data-structure. 
%%
%% Returns:
%%   An updated node_sets data-structure with Node removed from 
%%   selected set.
%%%----------------------------------------------------------------------

%% remove(initial, Node, Node_sets) ->
%%   set_initial(ordsets:del_element(Node, initial(Node_sets)), Node_sets);
%% remove(precoloured, Node, Node_sets) ->
%%   set_precoloured(ordsets:del_element(Node, precoloured(Node_sets)), Node_sets);
%% remove(spilled, Node, Node_sets) ->
%%   set_spilled(ordsets:del_element(Node, spilled(Node_sets)), Node_sets);
%% remove(coalesced, Node, Node_sets) ->
%%   set_coalesced(ordsets:del_element(Node, coalesced(Node_sets)), Node_sets);
%% remove(colored, Node, Node_sets) ->
%%   set_colored(ordsets:del_element(Node, colored(Node_sets)), Node_sets).

%%%----------------------------------------------------------------------
%% Function:    is_empty
%%
%% Description: Checks if one of the sets is empty.
%%
%% Parameters:
%%   Set            -- One of initial, precoloured, spilled, coalesced,
%%                       colored
%%   Node_sets      -- A node_sets data-structure. 
%%
%% Returns:
%%   true  --  If selected set is empty.
%%   false --  Otherwise
%%%----------------------------------------------------------------------

%% is_empty(initial, Node_sets) ->
%%   initial(Node_sets) == [];
%% is_empty(precoloured, Node_sets) ->
%%   precoloured(Node_sets) == [];
%% is_empty(spilled, Node_sets) ->
%%   spilled(Node_sets) == [];
%% is_empty(coalesced, Node_sets) ->
%%   coalesced(Node_sets) == [];
%% is_empty(colored, Node_sets) ->
%%   colored(Node_sets) == [].

%%----------------------------------------------------------------------
%% Function:    non_precoloured
%%
%% Description: Generates a list with temporaries that's not 
%%              precoloured within the range Current-Max_temporary.
%%
%% Parameters:
%%   Target         -- Target specific module.
%%   Current        -- The minimum temporary number
%%   Max_temporary  -- The maximum temporary number
%%   Initial        -- An empty list
%%
%% Returns:
%%   A list of temporaries that's not precoloured.
%%----------------------------------------------------------------------

non_precoloured(Target,Max_temporary, Max_temporary, Initial) -> 
    case Target:is_precoloured(Max_temporary) of
	true ->
	    Initial;
	false ->
	    [Max_temporary|Initial]
    end;
non_precoloured(Target, Current, Max_temporary, Initial) ->
    case Target:is_precoloured(Current) of
	true ->
	    non_precoloured(Target,Current+1, Max_temporary, Initial);
	false ->
	    non_precoloured(Target,Current+1, Max_temporary, [Current|Initial])
    end.
