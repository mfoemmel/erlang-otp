%% Copyright (c) 1997 by Erik Johansson.  All Rights Reserved 
%% Time-stamp: <02/05/10 23:55:29 happi>
%% ====================================================================
%%  Filename : 	hipe_sparc_size.erl
%%  Module   :	hipe_sparc_size
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 1997-11-06 Erik Johansson (happi@csd.uu.se): Created.
%% CVS:
%%    $Author: kostis $
%%    $Date: 2003/12/07 23:18:48 $
%%    $Revision: 1.3 $
%% ====================================================================
%% Exported functions (short description):
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc_size).
-export([count_instrs_cfg/1]).
%% -export([code_size/1, byte_instruction_size/1, count_instrs/1]).

%% %% Size in 4-byte words.
%% code_size(Code) -> code_size(Code,0).
%% 
%% code_size([],Size) -> Size;
%% code_size([Instr|Rest],Size) ->
%%   code_size(Rest,(icount(Instr))+Size).
%% 
%% byte_instruction_size(Instr) ->
%%   icount(Instr) * 4.

count_instrs_cfg(CFG) ->
  count_instrs(hipe_sparc:sparc_code(hipe_sparc_cfg:linearize(CFG))).

count_instrs(Code) ->
  count_instrs(Code,0).

count_instrs([Instr|Is], Acc) ->
  count_instrs(Is, Acc + icount(Instr));
count_instrs([],Acc) -> Acc.

icount(I) ->
  case hipe_sparc:type(I) of
    label ->
      0;
    comment ->
      0;
    load_address ->
      2;
    multimove ->
      %% These should have been removed before assembly...
      %% To be on the safe size we calculate that we need one extra
      %% move (to a temp) for each register that need to be moved.
      %% [r1,r2] <- [r3, r4] can be handled as:
      %% r5 <- r3
      %% r6 <- r4
      %% r1 <- r5
      %% r2 <- r6
      length(hipe_sparc:multimove_dest(I))*2;
    _Other ->
      1
  end.
