%%% -*- erlang-indent-level: 2 -*-
%%% $Id$
%%%----------------------------------------------------------------------
%%% File    : hipe_reg_worklists.erl
%%% Author  : Andreas Wallin <d96awa@csd.uu.se>
%%% Purpose : Represents sets of nodes/temporaries that we are
%%%           working on, such as simplify and spill sets.
%%% Created : 3 Feb 2000 by Andreas Wallin <d96awa@csd.uu.se>
%%%----------------------------------------------------------------------

-module(hipe_optimistic_worklists).
-author(['NilsOla Linnermark', 'Andreas Wallin',  'Thorild Selén']).
-export([new/5,
	 simplify/1,
	 spill/1,
	 stack/1,
	 add_simplify/2,
	 add_coalesced/3,
	 add_spill/2,
	 push_stack/3,
	 remove_simplify/2,
	 remove_spill/2,
	 is_empty_simplify/1,
	 is_empty_spill/1,
	 member_coalesced_to/2,
	 member_stack_or_coalesced/2,
	 non_stacked_or_coalesced_nodes/2,
	 print_memberships/1
	]).

-record(worklists, 
	{simplify,   % Low-degree nodes
	 stack,	     % Stack of removed low-degree nodes, with adjacency lists
	 membership, % Mapping from temp to which set it is in
	 coalesced_to,  % if the node is coalesced to
	 spill,	     % Significant-degree nodes
	 elems       % The number of elems -- Only used for debug printouts --
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

new(IG, Target, CFG, K, No_temporaries) ->
  init(initial(Target, CFG), K, IG, empty(No_temporaries)).

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
       coalesced_to = hipe_bifs:array(No_temporaries, 'none'),
       simplify = ordsets:new(),
       stack    = [],
       spill    = ordsets:new(),
       elems    = No_temporaries
      }.    

%% Selectors for worklists record

simplify(Worklists) -> Worklists#worklists.simplify.
spill(Worklists)    -> Worklists#worklists.spill.
stack(Worklists)    -> Worklists#worklists.stack.

%% Updating worklists records

set_simplify(Simplify, Worklists) ->
    Worklists#worklists{simplify = Simplify}.
set_spill(Spill, Worklists) ->
    Worklists#worklists{spill = Spill}.


%%----------------------------------------------------------------------
%% Function:    init
%%
%% Description: Initializes worklists
%%
%% Parameters:
%%   Initials        -- Not precoloured temporaries
%%   K               -- Number of registers
%%   IG              -- Interference graph
%%   Worklists       -- (Empty) worklists structure
%%   
%% Returns:
%%   Initialized worklists structure
%%
%%----------------------------------------------------------------------

init([], _, _, Worklists) -> Worklists;
init([Initial|Initials], K, IG, Worklists) -> 
    case hipe_ig:is_trivially_colourable(Initial, K, IG) of
	false ->
	    New_worklists = add_spill(Initial, Worklists),
	    init(Initials, K, IG, New_worklists);
	_ ->
	    New_worklists = add_simplify(Initial, Worklists),
	    init(Initials, K, IG, New_worklists)
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
add_coalesced(From, To, Worklists) ->
  Membership = Worklists#worklists.membership,
  hipe_bifs:array_update(Membership, From, 'stack_or_coalesced'),
  Coalesced_to = Worklists#worklists.coalesced_to,
  hipe_bifs:array_update(Coalesced_to, To, 'coalesced_to'),
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

member_coalesced_to(Element, Worklists) ->
    hipe_bifs:array_sub(Worklists#worklists.coalesced_to, Element) == 'coalesced_to'.

member_stack_or_coalesced(Element, Worklists) ->
  hipe_bifs:array_sub(Worklists#worklists.membership, Element) == 'stack_or_coalesced'.

non_stacked_or_coalesced_nodes(Nodes, Worklists) ->
  Membership = Worklists#worklists.membership,
  [Node || Node <- Nodes,
	   hipe_bifs:array_sub(Membership, Node) =/= 'stack_or_coalesced'].

print_memberships(Worklists) ->
  ?debug_msg("Worklist memeberships:\n", []),
  NrElems = Worklists#worklists.elems,
  Membership = Worklists#worklists.membership,
  Coalesced_to = Worklists#worklists.coalesced_to,
  print_membership(NrElems, Membership, Coalesced_to).

print_membership(0, _, _) ->
  true;
print_membership(Element, Membership, Coalesced_to) ->
  NextElement = Element - 1,
  ?debug_msg("worklist ~w ~w ~w\n", [NextElement, hipe_bifs:array_sub(Membership, NextElement), hipe_bifs:array_sub(Coalesced_to, NextElement)]),
  print_membership(NextElement, Membership, Coalesced_to).
