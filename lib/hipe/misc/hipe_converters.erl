%% Copyright (c) 1998 by Erik Johansson.  All Rights Reserved 
%% Time-stamp: <01/01/17 16:31:31 happi>
%% ====================================================================
%%  Filename : 	hipe_converters.erl
%%  Module   :	hipe_converters
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 1998-04-16 Erik Johansson (happi@csd.uu.se): Created.
%% CVS:
%%    $Author: mikpe $
%%    $Date: 2001/07/16 20:45:04 $
%%    $Revision: 1.2 $
%% ====================================================================
%% Exported functions (short description):
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_converters).
-export([bytelist_to_16bitlist/1,word_to_lo_16/1,word_to_hi_16/1,
	 tuple_to_word/1,word_to_tuple/1,int_to_hex/1,
	 int_to_Hex/1]).

bytelist_to_16bitlist([Byte1,Byte2,Byte3,Byte4|Rest]) ->
  [(Byte1 bsl 8) bor Byte2,
   (Byte3 bsl 8) bor Byte4 | bytelist_to_16bitlist(Rest)];
bytelist_to_16bitlist([Byte1,Byte2,Byte3]) ->
  [(Byte1 bsl 8) bor Byte2,
   (Byte3 bsl 8)];
bytelist_to_16bitlist([Byte1,Byte2]) ->
  [(Byte1 bsl 8) bor Byte2,0];
bytelist_to_16bitlist([Byte1]) ->
  [(Byte1 bsl 8),0];
bytelist_to_16bitlist([]) -> [].

word_to_lo_16(X) -> X band     16#ffff.
word_to_hi_16(X) -> (X bsr 16) band 16#ffff.

tuple_to_word({Hi,Lo}) -> (Hi bsl 16) bor Lo.
word_to_tuple(Word) -> {word_to_hi_16(Word),word_to_lo_16(Word)}.

int_to_hex(N) -> int_to_hex(N, $a-10).
int_to_Hex(N) -> int_to_hex(N, $A-10).

int_to_hex(N, LetterBase) ->
    if N < 0 -> [$- | int_to_hex(-N, [], LetterBase)];
       true -> int_to_hex(N, [], LetterBase)
    end.

int_to_hex(N, Tail, LetterBase) ->
    NewN = N bsr 4,
    Digit = N band 15,
    Char =
	if Digit < 10 -> Digit+$0;
	   true -> Digit+LetterBase
	end,
    NewTail = [Char | Tail],
    if NewN =:= 0 -> NewTail;
       true -> int_to_hex(NewN, NewTail, LetterBase)
    end.
