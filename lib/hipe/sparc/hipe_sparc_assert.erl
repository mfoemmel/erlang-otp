%% Copyright (c) 1998 by Erik Johansson.  All Rights Reserved 
%% Time-stamp: <99/10/17 21:16:19 happi>
%% ====================================================================
%%  Filename : 	hipe_sparc_assert.erl
%%  Module   :	hipe_sparc_assert
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 1998-06-18 Erik Johansson (happi@csd.uu.se): Created.
%% CVS:
%%    $Author: kostis $
%%    $Date: 2003/12/07 23:18:47 $
%%    $Revision: 1.2 $
%% ====================================================================
%% Exported functions (short description):
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc_assert).
-export([check_branch_length/1]).
%% -export([check_immediates/1]).
%% 
%% %%
%% %% check_immediates asserts that all immediates are less than 13 bits
%% %% Returns true if the code is ok.
%% %% Exception: {'EXIT',{immediate_too_large, [{Instr1,[Imm1,Imm2]},{Instr2,[Imm3|...]}|...]}}
%% %%  if Imm1 and Imm2 are too large.
%% %%
%% check_immediates(Instrs) -> check_immediates(Instrs,[]).
%% 
%% check_immediates([],[]) -> true;
%% check_immediates([],Problems) -> exit({immediate_too_large,Problems});
%% check_immediates([Instr|Rest],Problems) ->
%%   case hipe_sparc:is_sethi(Instr) of
%%     true -> check_immediates(Rest,Problems);
%%     _ -> 
%%       case check_imm(hipe_sparc:imm_uses(Instr),[]) of
%% 	true ->
%% 	  check_immediates(Rest,Problems);
%% 	MoreProblems -> 
%% 	  check_immediates(Rest,[{Instr,MoreProblems}|Problems])
%%       end
%%   end.
%% 
%% check_imm([],[]) -> true;
%% check_imm([],Problems) -> Problems;
%% check_imm([Use|Rest],Problems) ->
%%   case hipe_sparc:is_imm(Use) of 
%%     true -> 
%%       Val = hipe_sparc:imm_value(Use),
%%       if 
%% 	Val > 4095 -> %% Largest possible immediate.
%% 	  check_imm(Rest,[Val|Problems]);
%% 	true -> 
%% 	  check_imm(Rest,Problems)
%%       end;
%%     _ -> 	
%%       check_imm(Rest,Problems)
%%   end.


%%
%% check_branch_length(relativeDestination) 
%%  Checks that the destination is within reach.
%%
%% XXX: this only works for disp19-type branches.
%% XXX: update for disp16 (BPr) and disp22 (Bicc) type branches
%%

check_branch_length(Dest) ->
  if (abs(Dest) band (16#FFF80000)) > 0 ->
      exit({too_long_branch,(Dest)});
     true -> true
  end.

