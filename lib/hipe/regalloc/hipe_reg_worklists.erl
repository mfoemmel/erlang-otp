%%% -*- erlang-indent-level: 2 -*-
%%% $Id$
%%%----------------------------------------------------------------------
%%% File    : hipe_reg_worklists.erl
%%% Author  : Andreas Wallin <d96awa@csd.uu.se>
%%% Purpose : Represents sets of nodes/temporaries that we are
%%%           working on, such as simplify and spill sets.
%%% Created : 3 Feb 2000 by Andreas Wallin <d96awa@csd.uu.se>
%%%----------------------------------------------------------------------

-module(hipe_reg_worklists).
-author(['Andreas Wallin',  'Thorild Selén']).
-export([new/6,
	 simplify/1,
	 spill/1,
	 freeze/1,
	 stack/1,
	 add_simplify/2,
	 add_freeze/2,
	 add_coalesced/2,
	 add_spill/2,
	 push_stack/3,
	 remove_simplify/2,
	 remove_spill/2,
	 remove_freeze/2,
	 is_empty_simplify/1,
	 is_empty_spill/1,
	 is_empty_freeze/1,
	 member_freeze/2,
	 member_stack_or_coalesced/2,
	 non_stacked_or_coalesced_nodes/2,
	 transfer_freeze_simplify/2,
	 transfer_freeze_spill/2,
	 print_memberships/1
	]).

-record(worklists, 
	{simplify,   % Low-degree non move-related nodes
	 stack,	     % Stack of removed low-degree nodes, with adjacency lists
	 membership, % Mapping from temp to which set it is in
	 spill,	     % Significant-degree nodes
	 freeze      % Low-degree move-related nodes
	}).

%%-ifndef(DEBUG).
%%-define(DEBUG,true).
%%-endif.
-include("../main/hipe.hrl").

%%%----------------------------------------------------------------------
%% Function:    new
%%
%% Description: Constructor for worklists structure
%%
%% Parameters:
%%   IG              -- Interference graph
%%   Target          -- Target module name
%%   CFG             -- Target-specific CFG
%%   Move_sets       -- Move information
%%   K               -- Number of registers
%%   
%% Returns:
%%   A new worklists data structure
%%
%%%----------------------------------------------------------------------

new(IG, Target, CFG, Move_sets, K, No_temporaries) ->
  init(initial(Target, CFG), K, IG, Move_sets, empty(No_temporaries)).

initial(Target, CFG) ->
  {Min_temporary, Max_temporary} = Target:var_range(CFG),
  NonAlloc = Target:non_alloc(CFG),
  non_precoloured(Target, Min_temporary, Max_temporary, [])
    -- [Target:reg_nr(X) || X <- NonAlloc].

non_precoloured(Target, Current, Max_temporary, Initial) ->
  if Current > Max_temporary ->
      Initial;
     true ->
      NewInitial =
	case Target:is_precoloured(Current) of
	  true -> Initial;
	  false -> [Current|Initial]
	end,
      non_precoloured(Target, Current+1, Max_temporary, NewInitial)
  end.

%% construct an empty initialized worklists data structure
empty(No_temporaries) ->
    #worklists{
       membership = hipe_bifs:array(No_temporaries, 'none'),
       simplify = ordsets:new(),
       stack    = [],
       spill    = ordsets:new(),
       freeze   = ordsets:new()
      }.    

%% Selectors for worklists record

simplify(Worklists) -> Worklists#worklists.simplify.
spill(Worklists)    -> Worklists#worklists.spill.
freeze(Worklists)   -> Worklists#worklists.freeze.
stack(Worklists)    -> Worklists#worklists.stack.

%% Updating worklists records

set_simplify(Simplify, Worklists) ->
    Worklists#worklists{simplify = Simplify}.
set_spill(Spill, Worklists) ->
    Worklists#worklists{spill = Spill}.
set_freeze(Freeze, Worklists) ->
    Worklists#worklists{freeze = Freeze}.


%%----------------------------------------------------------------------
%% Function:    init
%%
%% Description: Initializes worklists
%%
%% Parameters:
%%   Initials        -- Not precoloured temporaries
%%   K               -- Number of registers
%%   IG              -- Interference graph
%%   Move_sets       -- Move information
%%   Worklists       -- (Empty) worklists structure
%%   
%% Returns:
%%   Initialized worklists structure
%%
%%----------------------------------------------------------------------

init([], _, _, _, Worklists) -> Worklists;
init([Initial|Initials], K, IG, Move_sets, Worklists) -> 
    case hipe_ig:is_trivially_colourable(Initial, K, IG) of
	false ->
	    New_worklists = add_spill(Initial, Worklists),
	    init(Initials, K, IG, Move_sets, New_worklists);
	_ ->
	    case hipe_moves:move_related(Initial, Move_sets) of
		true ->
		    New_worklists = add_freeze(Initial, Worklists),
		    init(Initials, K, IG, Move_sets, New_worklists);
		_ ->
		    New_worklists = add_simplify(Initial, Worklists),
		    init(Initials, K, IG, Move_sets, New_worklists)
	    end
    end.

%%%----------------------------------------------------------------------
%% Function:    is_empty
%%
%% Description: Tests if the selected worklist if empty or not.
%%
%% Parameters:
%%   Worklists                -- A worklists data structure
%%   
%% Returns:
%%   true  -- If the worklist was empty
%%   false -- otherwise
%%
%%%----------------------------------------------------------------------

is_empty_simplify(Worklists) ->
    simplify(Worklists) == [].

is_empty_spill(Worklists) ->
    spill(Worklists) == [].

is_empty_freeze(Worklists) ->
    freeze(Worklists) == [].

%%%----------------------------------------------------------------------
%% Function:    add
%%
%% Description: Adds one element to one of the worklists.
%%
%% Parameters:
%%   Element                  -- An element you want to add to the 
%%                                selected worklist. The element should 
%%                                be a node/temporary.
%%   Worklists                -- A worklists data structure
%%   
%% Returns:
%%   An worklists data-structure that have Element in selected 
%%    worklist.
%%
%%%----------------------------------------------------------------------
add_coalesced(Element, Worklists) ->
  Membership = Worklists#worklists.membership,
  hipe_bifs:array_update(Membership, Element, 'stack_or_coalesced'),
  Worklists.

add_simplify(Element, Worklists) ->
  Membership = Worklists#worklists.membership,
  hipe_bifs:array_update(Membership, Element, 'simplify'),
  Simplify = ordsets:add_element(Element, simplify(Worklists)),
  set_simplify(Simplify, Worklists).

add_spill(Element, Worklists) ->
  Membership = Worklists#worklists.membership,
  hipe_bifs:array_update(Membership, Element, 'spill'),
  Spill = ordsets:add_element(Element, spill(Worklists)),
  set_spill(Spill, Worklists).

add_freeze(Element, Worklists) ->
  Membership = Worklists#worklists.membership,
  hipe_bifs:array_update(Membership, Element, 'freeze'),
  Freeze = ordsets:add_element(Element, freeze(Worklists)),
  set_freeze(Freeze, Worklists).

push_stack(Node, AdjList, Worklists) ->
  Membership = Worklists#worklists.membership,
  hipe_bifs:array_update(Membership, Node, 'stack_or_coalesced'),
  Stack = Worklists#worklists.stack,
  Worklists#worklists{stack = [{Node,AdjList}|Stack]}.

%%%----------------------------------------------------------------------
%% Function:    remove
%%
%% Description: Removes one element to one of the worklists.
%%
%% Parameters:
%%   Element                  -- An element you want to remove from the 
%%                                selected worklist. The element should 
%%                                be a node/temporary.
%%   Worklists                -- A worklists data structure
%%   
%% Returns:
%%   A worklists data-structure that don't have Element in selected 
%%    worklist.
%%
%%%----------------------------------------------------------------------
remove_simplify(Element, Worklists) ->
  Membership = Worklists#worklists.membership,
  hipe_bifs:array_update(Membership, Element, 'none'),
  Simplify = ordsets:del_element(Element, simplify(Worklists)),
  set_simplify(Simplify, Worklists).

remove_spill(Element, Worklists) ->
  Membership = Worklists#worklists.membership,
  hipe_bifs:array_update(Membership, Element, 'none'),
  Spill = ordsets:del_element(Element, spill(Worklists)),
  set_spill(Spill, Worklists).

remove_freeze(Element, Worklists) ->
  Membership = Worklists#worklists.membership,
  hipe_bifs:array_update(Membership, Element, 'none'),
  Freeze = ordsets:del_element(Element, freeze(Worklists)),
  set_freeze(Freeze, Worklists).

%%%----------------------------------------------------------------------
%% Function:    transfer
%%
%% Description: Moves element from one worklist to another.
%%
%%%----------------------------------------------------------------------
transfer_freeze_simplify(Element, Worklists) ->
  add_simplify(Element, remove_freeze(Element, Worklists)).

transfer_freeze_spill(Element, Worklists) ->
  add_spill(Element, remove_freeze(Element, Worklists)).

%%%----------------------------------------------------------------------
%% Function:    member
%%
%% Description: Checks if one element if member of selected worklist.
%%
%% Parameters:
%%   Element                  -- Element you want to know if it's a 
%%                                member of selected worklist.
%%   Worklists                -- A worklists data structure
%%   
%% Returns:
%%   true   --  if Element is a member of selected worklist
%%   false  --  Otherwise
%%
%%%----------------------------------------------------------------------

member_freeze(Element, Worklists) ->
  hipe_bifs:array_sub(Worklists#worklists.membership, Element) == 'freeze'.

member_stack_or_coalesced(Element, Worklists) ->
  hipe_bifs:array_sub(Worklists#worklists.membership, Element) == 'stack_or_coalesced'.

non_stacked_or_coalesced_nodes(Nodes, Worklists) ->
  Membership = Worklists#worklists.membership,
  [Node || Node <- Nodes,
	   hipe_bifs:array_sub(Membership, Node) =/= 'stack_or_coalesced'].

print_memberships(Worklists) ->
  ?debug_msg("Worklist memeberships:\n", []),
  Membership = Worklists#worklists.membership,
  NrElems = hipe_bifs:array_length(Membership),
  print_membership(NrElems, Membership).

print_membership(0, _) ->
  true;
print_membership(Element, Membership) ->
  NextElement = Element - 1,
  ?debug_msg("worklist ~w ~w\n", [NextElement, hipe_bifs:array_sub(Membership, NextElement)]),
  print_membership(NextElement, Membership).
