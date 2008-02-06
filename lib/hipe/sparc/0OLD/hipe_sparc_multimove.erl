%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2000 by Erik Johansson.  All Rights Reserved 
%% ====================================================================
%%  Filename : 	sparc_multimove.erl
%%  Module   :	sparc_multimove
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2000-09-06 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: mikpe $
%%              $Date: 2007/12/18 09:18:22 $
%%              $Revision: 1.1 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc_multimove).
-export([remove_multimoves/1]).

-include("../main/hipe.hrl").
-include("hipe_sparc.hrl").

%%=====================================================================

remove_multimoves(CFG) ->
  traverse(hipe_sparc_cfg:labels(CFG), CFG).

traverse([Name|BBs], CFG) ->
  BB = hipe_sparc_cfg:bb(CFG, Name),
  Code =  hipe_bb:code(BB),
  NewCode = traverse_code(Code, []),
  NewBB = hipe_bb:code_update(BB, NewCode),
  NewCFG = hipe_sparc_cfg:bb_add(CFG, Name, NewBB),
  traverse(BBs, NewCFG);
traverse([], CFG) -> CFG.

traverse_code([I|Is], Acc) ->
  case I of
    #multimove{} ->
      Dests = hipe_sparc:multimove_dest(I),
      Source = hipe_sparc:multimove_src(I),
      NewAcc = make_moves(Dests, Source, Acc, [], false),
      traverse_code(Is, NewAcc);
    _Other ->
      traverse_code(Is, [I|Acc])
  end;
traverse_code([], Acc) ->
  lists:reverse(Acc).

make_moves([D|Ds], [S|Ss], Acc, Unresolved, Changed) ->
  case lists:member(D,Ss) or unresolved(D, Unresolved) of 
    false ->
      make_moves(Ds, Ss, [hipe_sparc:move_create(D, S)|Acc],
		 Unresolved, true);
    true  ->
      make_moves(Ds, Ss, Acc, [{D,S}|Unresolved], Changed)
  end;
make_moves(_, _, Acc, [], _) -> 
  %% No unresolved moves, return the code.
  Acc;
make_moves(_, _, Acc, [{D,S}], _) ->
  %% Just one unresolved move definitly no conflict
  [hipe_sparc:move_create(D, S)|Acc];
make_moves(_, _, Acc, Unresolved, true) ->
  %% Some moves were resolved so try again with the unresolved ones.
  {Ds, Ss} = lists:unzip(Unresolved),
  make_moves(Ds, Ss, Acc, [], false);
make_moves(_, _, Acc, Unresolved, false) ->
  %% No moves where resolved and we have unresolved ones. 
  %%  Write these moves to memory.
  %% TODO: 
  %%  Make an optimal resolver that uses some register
  %%   from the nonconflicting moves to resolve the 
  %%   conflicting moves.
  %%    e.g.
  %%     [r1,r2,r3] = [r4,r3,r2] 
  %%      ->  r1 = r2, r2 = r3, r3 = r1, r1 = r4 
  ?WARNING_MSG("Conflict in multimove ~w\n", [Unresolved]),
  SP = hipe_sparc_registers:stack_pointer(),
  NewAcc = save(Unresolved, Acc, SP, 0),
  restore(Unresolved, NewAcc, SP, 0).

save([{_,S}|More], Acc, SP, Pos) ->
  case hipe_sparc:is_reg(S) of
    true ->
      save(More,
	   [hipe_sparc:store_create(hipe_sparc:mk_reg(SP),
				    hipe_sparc:mk_imm(Pos bsl ?log2_wordsize),
				    w,
				    S)
	    |Acc],
	   SP,
	   Pos + 1);
    _ ->
      save(More, Acc, SP, Pos)
  end;
save([], Acc, _, _) ->
  Acc.

restore([{D,S}|More], Acc, SP, Pos) ->
  case hipe_sparc:is_reg(S) of
    true ->
      restore(More,
	      [hipe_sparc:load_create(D,
				      uw,
				      hipe_sparc:mk_reg(SP),
				      hipe_sparc:mk_imm(Pos bsl ?log2_wordsize))
	       |Acc],
	      SP, 
	      Pos + 1);
    _ ->
      restore(More, [hipe_sparc:move_create(D,S)|Acc], SP, Pos)
  end;
restore([], Acc, _, _) ->
  Acc.

%% Check if the current destination is the source in any unresolved moves.

unresolved(_, []) -> false;
unresolved(D, [{_,D}|_]) ->
  true;
unresolved(D, [_|Unresolved]) ->
  unresolved(D, Unresolved).


%% rewrite_mm(I, Mapping, SpillArea, SpillAreaReg, ScratchReg, Acc) ->
%%   Dests = hipe_sparc:multimove_dest(I),
%%   Source = hipe_sparc:multimove_src(I),
%% 
%%   SA = hipe_sparc:load_address_create(SpillAreaReg,
%% 				  SpillArea, constant),
%%   MMCode = spill_rewrite_mm(Dests, Source, [], hipe_sparc:info(I),
%% 		Mapping, [], [], SpillAreaReg, ScratchReg),
%%   %% io:format("MMcode ~w\n",[MMCode]),
%%   MMCode ++ [SA|Acc].
%% 
%% spill_rewrite_mm([D|Ds], [S|Ss], Acc, Info, Mapping, Unresolved,
%% 	      Overwritten, SpillAreaReg, ScratchReg) ->
%%   Dest = hipe_temp_map:find(hipe_sparc:reg_nr(D), Mapping),
%% 
%%   case hipe_sparc:is_imm(S) of
%%     true ->
%%       case Dest of
%% 	{spill, Off1} ->
%% 	  spill_rewrite_mm(Ds, Ss, 
%% 			   [hipe_sparc:store_create(SpillAreaReg, hipe_sparc:mk_imm(Off1 bsl ?log2_wordsize), w, ScratchReg, []),
%% 			    hipe_sparc:move_create(ScratchReg, S, Info)
%% 			    |Acc],
%% 			   Info,
%% 			   Mapping, Unresolved, [D|Overwritten],
%% 			   SpillAreaReg, ScratchReg);
%% 	{reg, DestR} ->
%% 	  DR = hipe_sparc:mk_reg(DestR),
%% 	  spill_rewrite_mm(Ds, Ss, 
%% 			   [hipe_sparc:move_create(DR, S, Info)|Acc],
%% 			   Info,
%% 			   Mapping, Unresolved, [DR|Overwritten],
%% 			   SpillAreaReg, ScratchReg)
%%       end; %% is imm
%%     false ->
%%       %% Source is a register.
%%       Source = hipe_temp_map:find(
%% 		 hipe_sparc:reg_nr(S), Mapping),
%%   
%%       case lists:member(Source, Overwritten)  of 
%% 	false ->
%% 	  case Dest of
%% 	    {spill, Off1} ->
%% 	      case Source of
%% 		{spill, Off2} ->
%% 		  spill_rewrite_mm(Ds, Ss, 
%% 				   [hipe_sparc:store_create(SpillAreaReg, 
%% 						       hipe_sparc:mk_imm(Off1 bsl ?log2_wordsize), w, ScratchReg, []),
%% 				    hipe_sparc:load_create(ScratchReg, w, SpillAreaReg, 
%% 						      hipe_sparc:mk_imm(Off2 bsl ?log2_wordsize), [])|
%% 				    Acc], Info,
%% 				   Mapping, Unresolved, [D|Overwritten],
%% 				   SpillAreaReg, ScratchReg);
%% 		{reg, SourceR} ->
%% 		  SR = hipe_sparc:mk_reg(SourceR),
%% 		  spill_rewrite_mm(Ds, Ss, 
%% 				   [hipe_sparc:store_create(SpillAreaReg, 
%% 							    hipe_sparc:mk_imm(Off1 bsl ?log2_wordsize), 
%% 							    w, SR, [])|Acc],
%% 				   Info,
%% 				   Mapping, Unresolved, [D|Overwritten],
%% 				   SpillAreaReg, ScratchReg)
%% 	      end;
%% 	    {reg, DR} ->
%% 	      DestR = hipe_sparc:mk_reg(DR),
%% 	      case Source of
%% 		{spill, Off2} ->
%% 		  spill_rewrite_mm(Ds, Ss, 
%% 				   [hipe_sparc:load_create(DestR, uw, SpillAreaReg, hipe_sparc:mk_imm(Off2 bsl ?log2_wordsize), [])|Acc],
%% 				   Info,
%% 				   Mapping, Unresolved, [DestR|Overwritten],
%% 				   SpillAreaReg, ScratchReg);
%% 		{reg, SR} ->
%% 		  SourceR = hipe_sparc:mk_reg(SR),
%% 		  spill_rewrite_mm(Ds, Ss, 
%% 				   [hipe_sparc:move_create(DestR, SourceR, Info)|Acc], Info,
%% 				   Mapping, Unresolved, [DestR|Overwritten],
%% 				   SpillAreaReg, ScratchReg)
%% 	      end
%% 	  end;
%% 
%% 	true  ->
%% 	  spill_rewrite_mm(Ds, Ss, 
%% 			   Acc, Info,
%% 			   Mapping, [{Dest,Source}|Unresolved], Overwritten,
%% 			   SpillAreaReg, ScratchReg)
%%       end
%%   end;
%% spill_rewrite_mm([], [], Acc, _, _, [], _, _, _) ->
%%   Acc;
%% spill_rewrite_mm([], [], Acc, Info, _, Unresolved, _, SpillAreaReg, ScratchReg) ->
%%   io:format("Ix Warning: Conflicting multimove ~w\n",[Unresolved]),
%%   save(Unresolved, Acc, 0, Info, SpillAreaReg, ScratchReg).
%% 
%% 
%% save([{Dest,Source}|More], Acc, StackIndex, Info, SpillAreaReg, ScratchReg) ->
%%   SP = hipe_sparc_registers:stack_pointer(),
%%   case Dest of
%%     {spill, Off1} ->
%%       case Source of
%% 	{spill, Off2} ->
%% 	  save(More,
%% 	       [hipe_sparc:store_create(SpillAreaReg, hipe_sparc:mk_imm(Off1 bsl ?log2_wordsize), ScratchReg),
%% 		hipe_sparc:load_create(ScratchReg, uw, SP, hipe_sparc:mk_imm(StackIndex bsl ?log2_wordsize), [])|
%% 		Acc] ++
%% 	       [hipe_sparc:store_create(SP, hipe_sparc:mk_imm(StackIndex bsl ?log2_wordsize), ScratchReg),
%% 		hipe_sparc:load_create(ScratchReg, uw, SpillAreaReg, hipe_sparc:mk_imm(Off2 bsl ?log2_wordsize), [])],
%% 	       StackIndex + 1, Info, SpillAreaReg, ScratchReg);
%% 	{reg, SourceR} ->
%% 	  save(More,
%% 	       [hipe_sparc:store_create(SpillAreaReg,hipe_sparc:mk_imm( Off1),
%% 				   ScratchReg),
%% 		hipe_sparc:load_create(ScratchReg, uw, SP, hipe_sparc:mk_imm(StackIndex bsl ?log2_wordsize), [])|
%% 		Acc] ++
%% 	       [hipe_sparc:store_create(SP, hipe_sparc:mk_imm(StackIndex bsl ?log2_wordsize), SourceR)],
%% 	       StackIndex + 1, Info, SpillAreaReg, ScratchReg)
%%       end;
%%     {reg, DestR} ->
%%       case Source of
%% 	{spill, Off2} ->
%% 	  save(More,
%% 	       [hipe_sparc:load_create(DestR, uw, SP, hipe_sparc:mk_imm(StackIndex bsl ?log2_wordsize), [])|
%% 		Acc] ++
%% 	       [hipe_sparc:store_create(SP, hipe_sparc:mk_imm(StackIndex bsl ?log2_wordsize), ScratchReg),
%% 		hipe_sparc:load_create(ScratchReg, uw, SpillAreaReg, hipe_sparc:mk_imm(Off2 bsl ?log2_wordsize), [])],
%% 	       StackIndex + 1, Info, SpillAreaReg, ScratchReg);
%% 	{reg, SourceR} ->
%% 	  save(More,
%% 	       [hipe_sparc:load_create(DestR, uw, SP, hipe_sparc:mk_imm(StackIndex bsl ?log2_wordsize), [])|
%% 		Acc] ++
%% 	       [hipe_sparc:store_create(SP, hipe_sparc:mk_imm(StackIndex bsl ?log2_wordsize), SourceR)],
%% 	       StackIndex + 1, Info, SpillAreaReg, ScratchReg)
%%       end
%%   end;
%% save([],Acc,_,_,_,_) -> Acc.
