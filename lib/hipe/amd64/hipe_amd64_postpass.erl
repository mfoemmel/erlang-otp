%% -*- erlang-indent-level: 2 -*-

-module(hipe_amd64_postpass).

-include("hipe_amd64.hrl").

-export([postpass/2]).

%%>----------------------------------------------------------------------<
%%  Procedure : postpass/1
%%  Purpose   : Function that performs a nr of postpast optimizations on
%%              the hipe amd64-assembler code before it is encoded and loaded.
%%  Arguments : Defun - Function definition. Contain all assembler code of 
%%              a function in a list.
%%  Return    : An optimized defun-record.
%%  Notes     : 
%%>----------------------------------------------------------------------<
postpass(Defun, Options) ->
  #defun{code=Code0} = Defun,
  Code1 = expand(Code0),                   % Expand pseudo instructions
  case proplists:get_bool(peephole, Options) of
    true  -> Code2 = peep(Code1, [], []);  % Do peephole optimizations
    false -> Code2 = Code1                 % Don't do peephole optimizations
  end,
  Code3 = peepN(Code2),                    % Do necessary peephole things
  Defun#defun{code=Code3}.

%%>----------------------------------------------------------------------<
%%  Procedure : expand/1
%%  Purpose   : Expands pseudo instructions.
%%  Arguments : Insns - An amd64-instruction list.
%%  Return    : An optimized instruction list.
%%  Notes     : 
%%>----------------------------------------------------------------------<
expand(Insns) -> expand(Insns, []).
expand([I|Tail], Res) ->
  case I of
    #pseudo_jcc{cc=Cc,true_label=TrueLab,false_label=FalseLab} ->
      expand(Tail, [hipe_amd64:mk_jmp_label(FalseLab),
		    hipe_amd64:mk_jcc(Cc, TrueLab) | Res]);
    #pseudo_tailcall_prepare{} ->
      expand(Tail, Res);
    #pseudo_call{'fun'=Fun,sdesc=SDesc,contlab=ContLab,linkage=Linkage} ->
      expand(Tail, [hipe_amd64:mk_jmp_label(ContLab),
		    hipe_amd64:mk_call(Fun, SDesc, Linkage) | Res]);
    _ ->
      expand(Tail, [I|Res])
  end;
expand([], Res) -> lists:reverse(Res).

%%>----------------------------------------------------------------------<
%%  Procedure : peep/1
%%  Purpose   : Function that does peephole optimizations. It works by 
%%              moving a window over the code and looking at a sequence of 
%%              a few instructions. Replaces long sequences of instructions
%%              with shorter ones and removes unnecesary ones.
%%  Arguments : Insns   - List of pseudo amd64-assembler records.
%%              Res     - Returned list of pseudo amd64-assembler records. 
%%                        Kept reversed, until it is returned.
%%              Lst     - List of optimizations done. For debuging.
%%  Return    : An optimized list of pseudo amd64-assembler records with 
%%              (hopefully) fewer or faster instructions.
%%>----------------------------------------------------------------------< 
%% MoveSelf related peep-opts 
%% ------------------------------
peep([#fmove{src=Src, dst=Src} | Insns], Res,Lst) -> 
    peep(Insns, Res, [moveSelf1|Lst]);
peep([I=#fmove{src=Src, dst=Dst}, 
      #fmove{src=Dst, dst=Src} | Insns], Res,Lst) -> 
    peep(Insns, [I|Res], [moveSelf2|Lst]);
peep([#movsx{src=Src, dst=Src} | Insns], Res,Lst) -> 
    peep(Insns, Res, [moveSelf3|Lst]);
peep([I=#movsx{src=Src, dst=Dst}, 
      #movsx{src=Dst, dst=Src} | Insns], Res,Lst) -> 
    peep(Insns, [I|Res], [moveSelf4|Lst]);
peep([#movzx{src=Src, dst=Src} | Insns], Res,Lst) -> 
    peep(Insns, Res, [moveSelf5|Lst]);
peep([I=#movzx{src=Src, dst=Dst}, 
      #movzx{src=Dst, dst=Src} | Insns], Res,Lst) ->
    peep(Insns, [I|Res], [moveSelf6|Lst]);
peep([#cmovcc{src=Src, dst=Src} | Insns], Res,Lst) -> 
    peep(Insns, Res, [moveSelf7|Lst]);
peep([I=#cmovcc{src=Src, dst=Dst}, 
      #cmovcc{src=Dst, dst=Src}|Insns], Res,Lst) -> 
    peep(Insns, [I|Res], [moveSelf8|Lst]);
peep([#move{src=Src, dst=Src} | Insns], Res,Lst) -> 
    peep(Insns, Res, [moveSelf9|Lst]);
peep([I=#move{src=Src, dst=Dst}, 
      #move{src=Dst, dst=Src} | Insns], Res,Lst) -> 
    peep(Insns, [I|Res], [moveSelf0|Lst]);

%%% Removed since they could never ever have been working... /Luna
%%% See x86 for code
%% ElimBinALMDouble
%% ----------------
%% ElimFBinDouble
%% --------------

%% CommuteBinALMD
%% --------------
peep([M = #move{src=Src1, dst=Dst1}, 
      B = #alu{aluop=Op,src=Src2,dst=Dst2}|Insns], Res, Lst) ->
  case (Src1==#amd64_imm{}) and (Src2/=#amd64_imm{}) and (Dst1==Dst2) of
    true ->
      case (Op=='add') or (Op=='and') or (Op=='or') or (Op=='xor') of
	true ->
	  peep(Insns, 
	       [#alu{aluop=Op,src=Src1,dst=Dst2},
		#move{src=Src2, dst=Dst1}|Res], 
	       [commuteBinALMD|Lst]);
	false ->
	  peep(Insns, [B,M|Res], Lst)
      end;
    false ->
      peep(Insns, [B,M|Res], Lst)
  end;

%% ElimCmp0
%% --------
peep([C=#cmp{src=Src, dst=Dst},J=#jcc{cc=Cond, label=Lab}|Insns],Res,Lst) ->
  case (((Src == #amd64_imm{value=0}) or (Dst == #amd64_imm{value=0})) and
	((Cond == 'eq') or (Cond == 'neq'))) of
    true ->
      Src2 = case Src of #amd64_imm{value=0} -> Src; _ -> Dst end, 
      Cond2 = case Cond of 'eq' -> 'z'; 'neq' -> 'nz' end,
      Test = #test{src=Src2, dst=#amd64_imm{value=0}},
      Jump = #jcc{cc=Cond2, label=Lab},
      peep(Insns, [Jump, Test|Res], [elimCmp0|Lst]);
    _ ->
      peep(Insns, [J,C|Res], Lst)
  end;

%% ElimCmpTest
%% -----------
peep([I|Insns],Res,Lst) when (I == #cmp{}) or (I == #test{}) -> 
  case check(Insns) of
    #jcc{} ->
      peep(Insns, [I|Res], Lst);
    #jmp_fun{} ->
      peep(Insns, [I|Res], Lst);
    #jmp_label{} ->
      peep(Insns, [I|Res], Lst);
    #jmp_switch{} ->
      peep(Insns, [I|Res], Lst);
    #cmovcc{} ->
      peep(Insns, [I|Res], Lst);
    #ret{} ->
      peep(Insns, [I|Res], Lst);
    _ ->
      peep(Insns, Res, [elimCmpTest|Lst])
  end;

%% ElimPushPop
%% -----------
peep([#push{src=Opr}, #pop{dst=Opr} | Insns], Res, Lst) ->
  peep(Insns, Res, [elimPushPop|Lst]);

%% ElimIFF
%% -------
peep([#jcc{label=Lab}, I=#label{label=Lab}|Insns], Res, Lst) ->
  peep(Insns, [I, #jmp_label{label=Lab}|Res], [elimIFF|Lst]);

%% ElimSet0
%% --------
peep([#move{src=#amd64_imm{value=0},dst=Dst}|Insns],Res,Lst) 
  when (Dst==#amd64_temp{}) ->
  peep(Insns, [#alu{aluop='xor', src=Dst, dst=Dst}|Res], [elimSet0|Lst]);

%% ElimMDPow2
%% ----------
peep([B = #alu{aluop=Op,src=#amd64_imm{value=Val},dst=Dst}|Insns], Res, Lst) ->
  {IsLog2, Size, Sign} = log2(Val),
  case ((Op == imul) or (Op == idiv)) and IsLog2 of
    true ->
      Sh = case Sign of positive -> 'bsl'; negative -> 'bsr' end,
      peep(Insns, 
	   [#shift{shiftop=Sh, src=#amd64_imm{value=Size}, dst=Dst}|Res], 
	   [elimMDPow2|Lst]);
    false ->
      peep(Insns, [B|Res], Lst)
  end;

%% Standard list recursion clause
%% ------------------------------
peep([I | Insns], Res, Lst) ->
  peep(Insns, [I|Res], Lst);

%% Base case.
%% ------------------------------------------------
peep([], Res, _Lst) ->
  lists:reverse(Res). 

%% Simple goto elimination (vital, dont know why..?)
%% -------------------------------------------------
peepN(Insns) -> peepN(Insns, [], []).
peepN([#jmp_label{label=Label}, I = #label{label=Label}|Insns], Res,Lst) ->
  peepN([I|Insns], Res, [nearGotoElim|Lst]);
peepN([I | Insns], Res, Lst) ->
  peepN(Insns, [I|Res], Lst);
peepN([], Res, _Lst) ->
  lists:reverse(Res). 

%%  Miscellaneous helper functions
%% >-------------------------------------------------------------------------< 

%% Log2 function
%% -------------
%% Used by ElimMDPow2 clause of peep(..)
log2(Nr) -> log2(Nr, 0).
log2(0, _) -> {false, 0, positive};
log2(Nr, I) ->
  case (Nr band 1) == 1 of
    true ->
      case Nr of
	1 ->
	  {true, I, positive};
	-1 ->
	  {true, I, negative};
	_ ->
	  {false, 0, positive}
      end;
    false ->
      log2((Nr bsr 1), I+1)
  end.

%% Skips through all comments and move instructions and returns the next one
%% ------------------------------------------------------------------------
%% Used by ElimCmpTest above.
check([I|Ins]) ->
  case I of
    #comment{} ->
      check(Ins);
    #move{} ->
      check(Ins);
    #fmove{} ->
      check(Ins);
    #movsx{} ->
      check(Ins);
    #movzx{} ->
      check(Ins);
    OtherI ->
      OtherI
  end.

