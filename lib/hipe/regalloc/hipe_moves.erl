%%%----------------------------------------------------------------------
%%% File    : hipe_moves.erl
%%% Author  : Thorild Selén <d95ths@zeppo.it.uu.se>
%%%           Andreas Wallin <d96awa@zeppo.it.uu.se>
%%%           Ingemar Åberg <d95ina@zeppo.it.uu.se>
%%% Purpose : Keep track of 'copy' (move) instructions and place them
%%%           in one of the move sets depending of their state. 
%%% Created:  11 Mar 2000 by Thorild Selén <d95ths@zeppo.it.uu.se>
%%%----------------------------------------------------------------------

-module(hipe_moves).
-author({"Thorild Selén", "Andreas Wallin", "Ingemar Åberg"}).
-export([new/1,
	 set_coalesced/2,
	 set_constrained/2,
	 set_frozen/2,
	 set_worklist/2,
	 set_active/2,
	 set_movelist/2,
	 movelist/1,
	 coalesced/1,
	 constrained/1,
	 frozen/1,
	 worklist/1,
	 active/1,
	 node_moves/2,
	 move_related/2,
	 is_empty/2,
	 remove/3,
	 add/3,
	 member/3
	]	 
       ).

-record(moves, {coalesced,   % Moves that are considered for coalescaing
		constrained, % Moves whose source and target interference
		frozen,      % Moves that will no longer we considered for coalescing
		worklist,    % Moves enabled for possible coalescing
		active,      % Moves not yet ready for coalescing
		movelist     % Mapping from node to list of moves it's associated with
	       }).

%%%----------------------------------------------------------------------
% Function:    coalesced,constrained,frozen,worklist,active,movelist
%
% Description: Selectors for moves structure
%
% Parameters:
%   Moves          -- Moves structure
%   
% Returns:
%   Selected set from the moves structure.
%
%%%----------------------------------------------------------------------

coalesced(Moves)   -> Moves#moves.coalesced.
constrained(Moves) -> Moves#moves.constrained.
frozen(Moves)      -> Moves#moves.frozen.
worklist(Moves)    -> Moves#moves.worklist.
active(Moves)      -> Moves#moves.active.
movelist(Moves)    -> Moves#moves.movelist.


%%%----------------------------------------------------------------------
% Function:    set_coalesced, set_constrained, set_frozen, set_worklist, set_active, set_movelist
%
% Description: Modifiers for moves structure
%
% Parameters:
%   Data-structure -- A data-structure you want to update the Moves 
%                      structure with. Coalesced for example.
%   Moves          -- Moves structure
%   
% Returns:
%   Updated moves data-structure
%
%%%----------------------------------------------------------------------
set_coalesced(New_coalesced, Moves) ->
    Moves#moves{coalesced = New_coalesced}.
set_constrained(New_constrained, Moves) ->
    Moves#moves{constrained = New_constrained}.
set_frozen(New_frozen, Moves) ->
    Moves#moves{frozen = New_frozen}.
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
    #moves{coalesced   = ordsets:new(),
	   constrained = ordsets:new(),
	   frozen      = ordsets:new(),
	   worklist    = hipe_ig_moves:worklist_moves(hipe_ig:ig_moves(IG)),
	   active      = ordsets:new(),
	   movelist    = hipe_ig_moves:movelist(hipe_ig:ig_moves(IG))}.


%%%----------------------------------------------------------------------
% Function:    remove
%
% Description: Removes Element from one of the sets.
%
% Parameters:
%    moveset       --  The set you want to remove Element from. Can be
%                       one of the these atoms coalesced, constrained,
%                       frozen, worklist, active, movelist
%    Element       --  The element you want to remove from selected set
%    Moves         --  A moves data structure
%   
% Returns:
%   An updated moves data structure where Element is removed from
%    selected set.
%
%%%----------------------------------------------------------------------
remove(coalesced, Element, Moves) ->
    set_coalesced(ordsets:del_element(Element, coalesced(Moves)), Moves);
remove(constrained, Element, Moves) ->
    set_constrained(ordsets:del_element(Element, constrained(Moves)), Moves);
remove(frozen, Element, Moves) ->
    set_frozen(ordsets:del_element(Element, frozen(Moves)), Moves);
remove(worklist, Element, Moves) ->
    set_worklist(ordsets:del_element(Element, worklist(Moves)), Moves);
remove(active, Element, Moves) ->
    set_active(ordsets:del_element(Element, active(Moves)), Moves);
remove(movelist, Element, Moves) -> 
    set_movelist(ordsets:del_element(Element, movelist(Moves)), Moves).


%%%----------------------------------------------------------------------
% Function:    add
%
% Description: Adds Element to one of the sets.
%
% Parameters:
%    moveset       --  The set you want to add Element to. Can be
%                       one of the these atoms coalesced, constrained,
%                       frozen, worklist, active, movelist
%    Element       --  The element you want to add to selected set
%    Moves         --  A moves data structure
%   
% Returns:
%   An updated moves data structure where Element is a member of
%    selected set.
%
%%%----------------------------------------------------------------------
add(coalesced, Element, Moves) ->
    set_coalesced(ordsets:add_element(Element, coalesced(Moves)), Moves);
add(constrained, Element, Moves) ->
    set_constrained(ordsets:add_element(Element, constrained(Moves)), Moves);
add(frozen, Element, Moves) ->
    set_frozen(ordsets:add_element(Element, frozen(Moves)), Moves);
add(worklist, Element, Moves) ->
    set_worklist(ordsets:add_element(Element, worklist(Moves)), Moves);
add(active, Element, Moves) ->
    set_active(ordsets:add_element(Element, active(Moves)), Moves);
add(movelist, Element, Moves) ->
    set_movelist(ordsets:add_element(Element, movelist(Moves)), Moves).


%%%----------------------------------------------------------------------
% Function:    member
%
% Description: Finds out if Element is a member of a selected set
%
% Parameters:
%    moveset       --  The set you want to know if Element if part of. 
%                       Can be one of the these atoms coalesced, 
%                       constrained, frozen, worklist, active, movelist
%    Element       --  The element you want to add to selected set
%    Moves         --  A moves data structure
%   
% Returns:
%   true  --  If Element is a member of asked set
%   false --  Otherwise
%
%%%----------------------------------------------------------------------
member(coalesced, Element, Moves) -> 
    ordsets:is_element(Element, coalesced(Moves));
member(constrained, Element, Moves) ->
    ordsets:is_element(Element, constrained(Moves));
member(frozen, Element, Moves) ->
    ordsets:is_element(Element, frozen(Moves));
member(worklist, Element, Moves) ->
    ordsets:is_element(Element, worklist(Moves));
member(active, Element, Moves) ->
    ordsets:is_element(Element, active(Moves));
member(movelist, Element, Moves) ->
    ordsets:is_element(Element, movelist(Moves)).


%%%----------------------------------------------------------------------
% Function:    is_empty
%
% Description: Checks if a set is empty or not.
%
% Parameters:
%    moveset       --  The set you want to know if it's empty. 
%                       Can be one of the these atoms coalesced, 
%                       constrained, frozen, worklist, active, movelist
%    Moves         --  A moves data structure
%   
% Returns:
%   true  --  If selected set is empty
%   false --  Otherwise
%
%%%----------------------------------------------------------------------
is_empty(coalesced, Moves) ->
    coalesced(Moves) == [];
is_empty(constrained, Moves) ->
    constrained(Moves) == [];
is_empty(frozen, Moves) ->
    frozen(Moves) == [];
is_empty(worklist, Moves) ->
    worklist(Moves) == [];
is_empty(active, Moves) ->
    active(Moves) == [];
is_empty(movelist, Moves) ->
    movelist(Moves) == [].


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

node_moves(Node, Moves) when integer(Node) ->
    Q = ordsets:intersection(movelist_associated_with(Node, Moves),
			  ordsets:union(active(Moves), worklist(Moves))),
    %% io:format("node_moves: node ~p moves: ~p~n",[Node,Q]),
    Q.

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

move_related(Node, Moves) when integer(Node) ->
    %% We know that node_moves/2 returns an ordset (a list).
    not (node_moves(Node, Moves) == []).

movelist_associated_with(Node, Moves) when integer(Node) ->
    hipe_vectors_wrapper:get(movelist(Moves), Node).
