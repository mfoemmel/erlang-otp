%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Basic Block Module
%
% Exports:
% ~~~~~~~~
% mk_bb(Code, NextBB) - construct a basic block, NextBB is the fall-through bb.
% code(BB) - returns the code.
% code_update(BB, NewCode) - replace the code in a basic block.
% last(BB) - returns the last instruction.
%

-module(hipe_bb).
-export([mk_bb/1,
	 mk_bb/2,
	 code/1,
	 annot/1,
	 code_update/2,
	 last/1,
	 butlast/1]).

%
% Constructs a basic block.
% Returns a basic block: {bb, Code}
%   * Code is a list of instructions

mk_bb(Code) ->
   {bb, Code, []}.

mk_bb(Code, Annot) ->
   {bb, Code, Annot}.

code_update({bb, Code, Annot}, NewCode) ->
   {bb, NewCode, Annot}.

code({bb, Code, Annot}) -> 
   Code.

annot({bb, Code, Annot}) -> 
   Annot.

last({bb, Code, Annot}) -> 
   lists:last(Code).


butlast({bb, Code, Annot}) ->
    butlast_1(Code).

butlast_1([X|Xs]) -> butlast_1(Xs,X).

butlast_1([],X) -> [];
butlast_1([X|Xs],Y) -> [Y|butlast_1(Xs,X)].
