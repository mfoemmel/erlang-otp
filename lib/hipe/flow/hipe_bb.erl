%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Basic Block Module
%%
%% Exports:
%% ~~~~~~~~
%% mk_bb(Code) - construct a basic block.
%% code(BB) - returns the code.
%% code_update(BB, NewCode) - replace the code in a basic block.
%% last(BB) - returns the last instruction.
%%

-module(hipe_bb).
-export([mk_bb/1,
	 code/1,
	 code_update/2,
	 is_bb/1,
	 last/1,
	 butlast/1]).

%%
%% Constructs a basic block.
%% Returns a basic block: {bb, Code}
%%   * Code is a list of instructions

mk_bb(Code) ->
   {bb, Code, []}.

is_bb({bb, _, _}) -> true;
is_bb(_) -> false.

code_update({bb, _Code, Annot}, NewCode) ->
   {bb, NewCode, Annot}.

code({bb, Code, _Annot}) -> 
   Code.

last({bb, Code, _Annot}) -> 
   lists:last(Code).


butlast({bb, Code, _Annot}) ->
    butlast_1(Code).

butlast_1([X|Xs]) -> butlast_1(Xs,X).

butlast_1([],_) -> [];
butlast_1([X|Xs],Y) -> [Y|butlast_1(Xs,X)].
