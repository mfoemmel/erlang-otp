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
%% butlast(BB) - returns the code with the last instruction removed.
%%

-module(hipe_bb).

-export([mk_bb/1,
	 code/1,
	 code_update/2,
	 is_bb/1,
	 last/1,
	 butlast/1]).

-record(bb, {code=[]}).

%%
%% Constructs a basic block.
%% Returns a basic block: {bb, Code}
%%   * Code is a list of instructions

mk_bb(Code) ->
    #bb{code=Code}.

is_bb(#bb{}) -> true;
is_bb(_) -> false.

code_update(BB, Code) ->
    BB#bb{code = Code}.

code(#bb{code = Code}) -> 
    Code.

last(#bb{code = Code}) -> 
    lists:last(Code).

butlast(#bb{code = Code}) ->
    butlast_1(Code).

butlast_1([X|Xs]) -> butlast_1(Xs,X).

butlast_1([X|Xs],Y) -> [Y|butlast_1(Xs,X)];
butlast_1([],_) -> [].
