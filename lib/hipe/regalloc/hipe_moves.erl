%%%----------------------------------------------------------------------
%% File    : hipe_moves.erl
%% Author  : Thorild Selén <d95ths@it.uu.se>
%%           Andreas Wallin <d96awa@it.uu.se>
%%           Ingemar Åberg <d95ina@it.uu.se>
%% Purpose : Keep track of 'copy' (move) instructions and place them
%%           in one of the move sets depending of their state. 
%% Created:  11 Mar 2000 by Thorild Selén <d95ths@it.uu.se>
%%%----------------------------------------------------------------------

-module(hipe_moves).
-author({"Thorild Selén", "Andreas Wallin", "Ingemar Åberg"}).
-export([new/1,
	 set_movelist/2,
	 movelist/1,
	 worklist/1,
	 node_moves/2,
	 move_related/2,
	 is_empty_worklist/1,
	 remove_worklist/2,
	 remove_active/2,
	 add_worklist/2,
	 add_active/2,
	 member_active/2
	]).

-record(moves, {worklist,    % Moves enabled for possible coalescing
		active,      % Moves not yet ready for coalescing
		movelist     % Mapping from node to list of moves it's associated with
	       }).

%%%----------------------------------------------------------------------
%% Function:    worklist,active,movelist
%%
%% Description: Selectors for moves structure
%%
%% Parameters:
%%   Moves          -- Moves structure
%%   
%% Returns:
%%   Selected set from the moves structure.
%%
%%%----------------------------------------------------------------------

worklist(Moves)    -> Moves#moves.worklist.
active(Moves)      -> Moves#moves.active.
movelist(Moves)    -> Moves#moves.movelist.


%%%----------------------------------------------------------------------
%% Function:    set_worklist, set_active, set_movelist
%%
%% Description: Modifiers for moves structure
%%
%% Parameters:
%%   Moves          -- Moves structure
%%   
%% Returns:
%%   Updated moves data-structure
%%
%%%----------------------------------------------------------------------
set_worklist(New_worklist, Moves) ->
    Moves#moves{worklist = New_worklist}.
set_active(New_active, Moves) ->
    Moves#moves{active = New_active}.
set_movelist(New_movelist, Moves) ->
    Moves#moves{movelist = New_movelist}.



%%%----------------------------------------------------------------------
% Function:    new
%
% Description: Creates a new moves datastructure initiated with 
%               som work from the building of the interference graph.
%
% Parameters:
%   IG             --  The interference graph
%   
% Returns:
%   A new moves data structure.
%
%%%----------------------------------------------------------------------

new(IG) ->
    #moves{worklist    = hipe_ig_moves:worklist_moves(hipe_ig:ig_moves(IG)),
	   active      = gb_sets:new(),
	   movelist    = hipe_ig_moves:movelist(hipe_ig:ig_moves(IG))}.


%%%----------------------------------------------------------------------
% Function:    remove
%
% Description: Removes Element from one of the sets.
%
% Parameters:
%    Element       --  The element you want to remove from selected set
%    Moves         --  A moves data structure
%   
% Returns:
%   An updated moves data structure where Element is removed from
%    selected set.
%
%%%----------------------------------------------------------------------
remove_worklist(Element, Moves) ->
    set_worklist(ordsets:del_element(Element, worklist(Moves)), Moves).

remove_active(Element, Moves) ->
    set_active(gb_sets:del_element(Element, active(Moves)), Moves).

%%%----------------------------------------------------------------------
% Function:    add
%
% Description: Adds Element to one of the sets.
%
% Parameters:
%    Element       --  The element you want to add to selected set
%    Moves         --  A moves data structure
%   
% Returns:
%   An updated moves data structure where Element is a member of
%    selected set.
%
%%%----------------------------------------------------------------------
add_worklist(Element, Moves) ->
    set_worklist(ordsets:add_element(Element, worklist(Moves)), Moves).

add_active(Element, Moves) ->
    set_active(gb_sets:add_element(Element, active(Moves)), Moves).

%%%----------------------------------------------------------------------
% Function:    member
%
% Description: Finds out if Element is a member of a selected set
%
% Parameters:
%    Element       --  The element you want to add to selected set
%    Moves         --  A moves data structure
%   
% Returns:
%   true  --  If Element is a member of asked set
%   false --  Otherwise
%
%%%----------------------------------------------------------------------
member_active(Element, Moves) ->
    gb_sets:is_element(Element, active(Moves)).

%%%----------------------------------------------------------------------
% Function:    is_empty
%
% Description: Checks if a set is empty or not.
%
% Parameters:
%    Moves         --  A moves data structure
%   
% Returns:
%   true  --  If selected set is empty
%   false --  Otherwise
%
%%%----------------------------------------------------------------------
is_empty_worklist(Moves) ->
    worklist(Moves) == [].

%%%----------------------------------------------------------------------
%% Function:    node_moves
%%
%% Description: Move instructions associated with Node that's not frozen,
%%              constrained (target and source interfere) or already
%%              coalesced.
%%
%% Parameters:
%%    Node          --  A node/temporary 
%%    Moves         --  A moves data structure
%%   
%% Returns:
%%   A set of temporaries that's not frozen, constrained or coalesced.
%%----------------------------------------------------------------------

node_moves(Node, Moves) when is_integer(Node) ->
%% Associated = movelist_associated_with(Node, Moves),
%%  Associated /\ (Active U Worklist)

%% Since the union Active Worklist might be much larger than 
%% the Associated set we do not want to generate the set the
%% straightforward way. (We would like to do the union lazily).

%% Straightforward code:
%  Q = ordsets:intersection(
%	movelist_associated_with(Node, Moves),
%	ordsets:union(ordsets:from_list(gb_sets:to_list(active(Moves))), 
%		      worklist(Moves))),
%    %% io:format("node_moves: node ~p moves: ~p~n",[Node,Q]),
%    Q.

  %% Two lookups for each element.
  %% Could go one step further and use the fact that the lists are ordered.
  Associated = movelist_associated_with(Node, Moves),
  Active = active(Moves),
  Worklist = worklist(Moves),
  [X || X <- Associated,
	  gb_sets:is_member(X,Active) orelse
	  ordsets:is_element(X,Worklist)].
      


%%----------------------------------------------------------------------
%% Function:    move_related
%%
%% Description: Checks if a specified node/temporary have any move
%%               instructions that's not frozen, constrained or 
%%               coalesced.
%%
%% Parameters:
%%    Node          --  A node/temporary 
%%    Moves         --  A moves data structure
%%   
%% Returns:
%%   true  --  If it has at least one instruction that's not frozen, constrained
%%             or coalesced.
%%   false --  Otherwise.
%%----------------------------------------------------------------------

move_related(Node, Moves) when is_integer(Node) ->
    %% We know that node_moves/2 returns an ordset (a list).
    not (node_moves(Node, Moves) == []).

movelist_associated_with(Node, Moves) when is_integer(Node) ->
    hipe_vectors_wrapper:get(movelist(Moves), Node).
