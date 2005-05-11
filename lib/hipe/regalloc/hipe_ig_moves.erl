%% -*- erlang-indent-level: 2 -*-
%% $Id: hipe_ig_moves.erl,v 1.9 2004/12/30 00:54:34 mikpe Exp $

-module(hipe_ig_moves).
-export([new/1,
	 new_move/3,
	 get_moves/1]).

-record(ig_moves,
	{movelist,	% mapping from temp to set of associated move numbers
	 nrmoves,	% number of distinct move insns seen so far
	 moveinsns,	% list of move insns, in descending move number order
	 moveset}).	% set of move insns

new(NrTemps) ->
  #ig_moves{
     movelist = hipe_vectors_wrapper:empty(NrTemps, ordsets:new()),
     nrmoves = 0,
     moveinsns = [],
     moveset = gb_sets:empty()}.

new_move(Dst, Src, IG_moves) ->
  MoveSet = IG_moves#ig_moves.moveset,
  MoveInsn = {Dst,Src},
  case gb_sets:is_member(MoveInsn, MoveSet) of
    true ->
      IG_moves;
    false ->
      MoveNr = IG_moves#ig_moves.nrmoves,
      Movelist0 = IG_moves#ig_moves.movelist,
      Movelist1 = add_movelist(MoveNr, Dst, add_movelist(MoveNr, Src, Movelist0)),
      IG_moves#ig_moves{nrmoves = MoveNr+1,
			movelist = Movelist1,
			moveinsns = [MoveInsn|IG_moves#ig_moves.moveinsns],
			moveset = gb_sets:insert(MoveInsn, MoveSet)}
  end.

add_movelist(MoveNr, Temp, Movelist) ->
  Assoc_moves = hipe_vectors_wrapper:get(Movelist, Temp),
  %% XXX: MoveNr does not occur in moveList[Temp], but the new list must be an
  %% ordset due to the ordsets:union in hipe_coalescing_regalloc:combine().
  hipe_vectors_wrapper:set(Movelist, Temp, ordsets:add_element(MoveNr, Assoc_moves)).

get_moves(IG_moves) -> % -> {MoveList, NrMoves, MoveInsns}
  {IG_moves#ig_moves.movelist,
   IG_moves#ig_moves.nrmoves,
   list_to_tuple(lists:reverse(IG_moves#ig_moves.moveinsns))}.
