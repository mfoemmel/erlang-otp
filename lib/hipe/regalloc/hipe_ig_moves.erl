%% -*- erlang-indent-level: 4 -*-
%%%----------------------------------------------------------------------
%%% File    : ig_moves.erl
%%% Author  : Andreas Wallin <d96awa@ida.dis.uu.se>
%%% Purpose : The interference graph stores information about move
%%%            instructions that are used later in the register 
%%%            allocation algorithm. It contains information about
%%%            move instructions that temporaries are associated
%%%            with. And also all copy moves that exists. 
%%%            NOTE that only 'copy moves' are stored here. imm moves
%%%            are treated as regular instructions.
%%% Created : 04 Feb 2000 by Andreas Wallin <d96awa@ida.dis.uu.se>
%%%----------------------------------------------------------------------

-module(hipe_ig_moves).
-author("Andreas Wallin").
-export([new/1, 
	 movelist/1,
	 worklist_moves/1,
	 set_movelist/2,
	 set_worklist_moves/2,
	 add/4,
	 add/3]
       ).



-record(igraph_moves, {movelist, worklist_moves}).


%%%----------------------------------------------------------------------
% Function:    new
%
% Description: Create a new ig_moves data structure.
%
% Parameters:
%   No_temporaries  --  Number of temporaries.
%   
% Returns:
%   A new ig_moves data structure
%
%%%----------------------------------------------------------------------
new(No_temporaries) -> 
    #igraph_moves{
     movelist = hipe_vectors_wrapper:empty(No_temporaries, ordsets:new()), 
     worklist_moves = ordsets:new()
    }.

%%%----------------------------------------------------------------------
% Function:    movelist, worklist_moves
%
% Description: Selector functions. Used to get one of the encapsulated 
%               data-structure contained in the ig_moves structure.
% Parameters:
%   IG_moves       --  An ig_moves data-structure
%
% Returns: 
%   One of the encapsulated data-structures.
%%%----------------------------------------------------------------------
movelist(IG_moves) -> IG_moves#igraph_moves.movelist.
worklist_moves(IG_moves) -> IG_moves#igraph_moves.worklist_moves.

%%%----------------------------------------------------------------------
% Function:    set_movelist, set_worklist_moves
%
% Description: Modifier functions. Used to set one of the encapsulated 
%               data-structure contained in the IG structure.
% Parameters:
%   Data-structure --  Data-structure you want to set. The movelist 
%                       data-structure for example.
%   IG_moves       --  An ig_moves data-structure.
%
% Returns: 
%   An updated ig_moves data-structure.
%%%----------------------------------------------------------------------
set_movelist(Movelist, IG_moves) -> IG_moves#igraph_moves{movelist=Movelist}.
set_worklist_moves(Worklist_moves, IG_moves) -> IG_moves#igraph_moves{worklist_moves=Worklist_moves}.


%%%----------------------------------------------------------------------
% Function:    add
%
% Description: Associates a copy instruction with a temporary if
%               you add to the movelist or just remembers that we've
%               seen the copy instruction if you add it to 
%               worklist_moves.
%                
% Parameters:
%   movelist, worklist_moves  --  Add to one of these
%   Instruction               --  Add this (copy) instruction to above
%   Temporary                 --  Associate instruction with this 
%                                  temporary
%   IG_moves                  --  An IG_moves data structure
%   
% Returns:
%   An updated ig_moves data structure
%
%%%----------------------------------------------------------------------
add(movelist, Instruction, Temporary, IG_moves) when integer(Temporary) ->
    Movelist = movelist(IG_moves),
    Assoc_moves = hipe_vectors_wrapper:get(Movelist, Temporary),
    New_movelist = hipe_vectors_wrapper:set(Movelist, Temporary, ordsets:add_element(Instruction, Assoc_moves)),
    set_movelist(New_movelist, IG_moves).
add(worklist_moves, Instruction, IG_moves) ->
    Worklist = worklist_moves(IG_moves),
    New_worklist = ordsets:add_element(Instruction, Worklist),
    set_worklist_moves(New_worklist, IG_moves).




