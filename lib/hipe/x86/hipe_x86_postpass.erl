%%%----------------------------------------------------------------------
%%% File    : hipe_x86_postpass.erl
%%% Author  : Christoffer Vikström <chvi3471@student.uu.se>
%%% Purpose : Contain postpass optimisations for x86-assembler code.
%%% Created : 5 Aug 2003 by Christoffer Vikström <chvi3471@student.uu.se>
%%%----------------------------------------------------------------------

-module(hipe_x86_postpass).
-author('chvi3471@student.uu.se').

-include("hipe_x86.hrl").

-export([postpass/1]).

-define(DO_PEEP, true).    % Turn on/off peephole optimizations
%% -define(DO_LOGGING, true). % Uncomment this to get peephole opts logged to..
-define(LOG_FILE, "peepcount.txt"). % ..this file..
-define(LOG_DIR, "/tmp/"). % ..in this dir


%%>----------------------------------------------------------------------<
%  Procedure : postpass/1
%  Purpose   : Function that performs a nr of postpast optimizations on
%              the hipe x86-assembler code before it is encoded and loaded.
%  Arguments : Defun - Function definition. Contain all assembler code of 
%              a function in a list.
%  Return    : An optimized defun-record.
%  Notes     : 
%%>----------------------------------------------------------------------<
postpass(Defun) ->
    #defun{mfa=MFA, code=Code0} = Defun,
    printLogHeader(MFA),           
    Code1 = expand(Code0),      % Expand pseudo instructions
    Code2 = peep(Code1),        % Do peephole optimizations
    Code3 = peepN(Code2),       % Do necessary peephole optimizations
    Defun#defun{code=Code3}.


%%>----------------------------------------------------------------------<
%  Procedure : peep/1
%  Purpose   : Function that does peephole optimizations. It works by 
%              moving a window over the code and looking at a sequence of 
%              a few instructions. Replaces long sequences of instructions
%              with shorter ones and removes unnecesary ones.
%  Arguments : Insns   - List of pseudo x86-assembler records.
%              Res     - Returned list of pseudo x86-assembler records. 
%                        Kept reversed, until it is returned.
%              Lst     - List of optimizations done. For debuging.
%  Return    : An optimized list of pseudo x86-assembler records with 
%              (hopefully) fewer or faster instructions.
%  Notes     : Creates a file in the users tmp directory that contain 
%              analysis information, if macro ?DO_LOGGING is defined.
%%>----------------------------------------------------------------------< 
peep(Insns) -> 
    case ?DO_PEEP of
	true ->
	    peep(Insns, [], []);
	_ ->
	    Insns
    end.


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
peep([#move{src=#x86_temp{reg=X}, 
	    dst=#x86_temp{reg=X}} | Insns], Res,Lst) -> 
    peep(Insns, Res, [moveSelf9|Lst]);
peep([I=#move{src=#x86_temp{reg=Src}, dst=#x86_temp{reg=Dst}}, 
      #move{src=#x86_temp{reg=Dst}, dst=#x86_temp{reg=Src}} | Insns], Res,Lst) -> 
    peep(Insns, [I|Res], [moveSelf0|Lst]);


%% ElimBinALMDouble
%% ----------------
peep([#move{src=Src, dst=Dst}, #alu{aluop=OP, src=Src, dst=Dst}|Insns], Res, Lst) ->
  BinALNew = #alu{aluop=OP, src=Dst, dst=Dst},
  peep(Insns, [BinALNew|Res], [elimBinALMDouble|Lst]);
peep([#movsx{src=Src, dst=Dst}, #alu{aluop=OP, src=Src, dst=Dst}|Insns], Res, Lst) ->
  BinALNew = #alu{aluop=OP, src=Dst, dst=Dst},
  peep(Insns, [BinALNew|Res], [elimBinALMDouble|Lst]);
peep([#movzx{src=Src, dst=Dst}, #alu{aluop=OP, src=Src, dst=Dst}|Insns], Res, Lst) ->
  BinALNew = #alu{aluop=OP, src=Dst, dst=Dst},
  peep(Insns, [BinALNew|Res], [elimBinALMDouble|Lst]);


%% ElimFBinDouble
%% --------------
peep([#fmove{src=Src, dst=Dst}, 
      #fp_binop{op=Op, src=Src, dst=Dst}|Insns], Res, Lst) ->
  peep(Insns, 
       [#fp_binop{op=Op,src=Dst,dst=Dst}|Res], 
       [elimFBinDouble|Lst]);


%% CommuteBinALMD
%% --------------
peep([#move{src=Src1, dst=Dst}, 
      #alu{aluop=Op,src=Src2,dst=Dst}|Insns], Res, Lst)
  when (Src1 == #x86_imm{}) and (Src2 /= #x86_imm{}) and 
       ((Op=='add') or (Op=='and') or (Op=='or') or (Op=='xor'))  ->
  peep(Insns, [#alu{aluop=Op,src=Src1,dst=Dst},
	       #move{src=Src2, dst=Dst}|Res], 
       [commuteBinALMD|Lst]);


%% ElimCmp0
%% --------
peep([C=#cmp{src=Src, dst=Dst},J=#jcc{cc=Cond, label=Lab}|Insns],Res,Lst) ->
    case (((Src == #x86_imm{value=0}) or (Dst == #x86_imm{value=0})) and
	  ((Cond == 'eq') or (Cond == 'neq'))) of
	true ->
	    Src2 = case Src of #x86_imm{value=0} -> Src; _ -> Dst end, 
	    Cond2 = case Cond of 'eq' -> 'z'; 'neq' -> 'nz' end,
	    Test = #test{src=Src2, dst=#x86_imm{value=0}},
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


% %% ElimIFF
% %% -------
peep([#jcc{label=Lab}, I=#label{label=Lab}|Insns], Res, Lst) ->
     peep(Insns, [I, #jmp_label{label=Lab}|Res], [elimIFF|Lst]);


%% ElimSet0
%% --------
peep([#move{src=#x86_imm{value=0},dst=Dst}|Insns],Res,Lst) 
when (Dst==#x86_temp{}) ->
    peep(Insns, [#alu{aluop='xor', src=Dst, dst=Dst}|Res], [elimSet0|Lst]);
    

%% ElimAddSub1
%% -----------
%% This seemed as a good idea to have but since inc and dec creates 
%% condition codes overhead they are slightly worse than 
%% peep([I=#alu{aluop=Op,src=#x86_imm{value=1},dst=Dst},I2|Insns], Res, Lst) ->
%%     case I2 of
%% 	#jcc{} ->
%% 	    peep([I2|Insns], [I|Res], Lst);
%% 	#cmovcc{} ->
%% 	    peep([I2|Insns], [I|Res], Lst);
%% 	#jmp_switch{} ->
%% 	    peep([I2|Insns], [I|Res], Lst);
%% 	_ ->
%% 	    case Op of
%% 		'add' ->
%% 		    peep([I2|Insns], [#inc{dst=Dst}|Res], [elimAddSub1|Lst]);
%% 		'sub' ->
%% 		    peep([I2|Insns], [#dec{dst=Dst}|Res], [elimAddSub1|Lst]);
%% 		_ ->
%% 		    peep([I2|Insns], [I|Res], Lst)
%% 	    end
%%     end;


%% ElimMDPow2
%% ----------
peep([B = #alu{aluop=Op,src=#x86_imm{value=Val},dst=Dst}|Insns], Res, Lst) ->
    {IsLog2, Size, Sign} = log2(Val),
    case ((Op == imul) or (Op == idiv)) and IsLog2 of
	true ->
	    Sh = case Sign of positive -> 'bsl'; negative -> 'bsr' end,
	    peep(Insns, 
		 [#shift{shiftop=Sh, src=#x86_imm{value=Size}, dst=Dst}|Res], 
		 [elimMDPow2|Lst]);
	false ->
	    peep(Insns, [B|Res], Lst)
    end;


%% Standard list recursion clause
%% ------------------------------
peep([I | Insns], Res, Lst) ->
     peep(Insns, [I|Res], Lst);

%% Base case. Optionally prints an optimization log
%% ------------------------------------------------
peep([], Res, Lst) ->
    printLst(Lst),
    lists:reverse(Res). 

%% Simple goto elimination (vital, dont know why..?)
%% -------------------------------------------------
peepN(Insns) -> peepN(Insns, [], []).
peepN([#jmp_label{label=Label}, I = #label{label=Label}|Insns], Res,Lst) ->
     peepN([I|Insns], Res, [nearGotoElim|Lst]);
peepN([I | Insns], Res, Lst) ->
     peepN(Insns, [I|Res], Lst);
peepN([], Res, Lst) ->
    printLst(Lst),
    lists:reverse(Res). 



%%>----------------------------------------------------------------------<
%%  Procedure : expand/1
%%  Purpose   : Expands pseudo instructions.
%%  Arguments : Insns - An x86-instruction list.
%%  Return    : An optimized instruction list.
%%  Notes     : 
%%>----------------------------------------------------------------------<
expand(Insns) -> expand(Insns, []).
expand([I|Tail], Res) ->
    case I of
	#pseudo_jcc{cc=Cc,true_label=TrueLab,false_label=FalseLab} ->
	    expand(Tail, [hipe_x86:mk_jmp_label(FalseLab),
			  hipe_x86:mk_jcc(Cc, TrueLab) | Res]);
	#pseudo_tailcall_prepare{} ->
	    expand(Tail, Res);
	#pseudo_call{'fun'=Fun,sdesc=SDesc,contlab=ContLab,linkage=Linkage} ->
	    expand(Tail, [hipe_x86:mk_jmp_label(ContLab),
			  hipe_x86:mk_call(Fun, SDesc, Linkage) | Res]);
	_ ->
	    expand(Tail, [I|Res])
    end;
expand([], Res) -> lists:reverse(Res).


%%>----------------------------------------------------------------------<
%%  Procedure : printLogHeader/1
%%  Purpose   : Prints the headers for each function compiled.
%%  Arguments : MFA -  #x86_mfa{} record
%%  Return    : unit
%%  Notes     : 
%%>----------------------------------------------------------------------<
-ifdef(DO_LOGGING).
printLogHeader(MFA) ->
    {x86_mfa, M, F, _} = MFA,
    {ok, Dir} = file:get_cwd(),
    file:set_cwd(?LOG_DIR),
    {ok, File} = file:open(?LOG_FILE, [read, append]),
    io:format(File, 
	      "\nModule: ~w  Function: ~w" ++ 
	      "\n>==============================================<\n", 
	      [M, F]),
    file:set_cwd(Dir),
    file:close(File).
-else.
printLogHeader(_) ->
    ok.
-endif.


%%>----------------------------------------------------------------------<
%%  Procedure : printLst/1
%%  Purpose   : Prints ths name of the peephole optimizations done in the
%%              current function being compiled.
%%  Arguments : Lst - A list of numbers.
%%  Return    : unit
%%  Notes     : Prints (append) the atoms in the list with a space in 
%%              between to a file called 'peepcount.txt'. 
%%>----------------------------------------------------------------------<
-ifdef(DO_LOGGING).
printLst(Lst) ->
    {ok, Dir} = file:get_cwd(),
    file:set_cwd(?LOG_DIR),
    {ok, File} = file:open(?LOG_FILE, [read, append]),
    printLst(File, Lst),
    file:set_cwd(Dir),
    file:close(File).

printLst(File, [Opt|Lst]) ->
    %% io:format(File, "Peephole applied ~w times!\n", [length(Lst)]),
    io:format(File, "Peephole optimization applied: ", []),
    io:write(File, Opt),
    io:format(File, "\n", []),
    printLst(File, Lst);
printLst(_, []) -> done.

-else. %% DO_LOGGING undefined

printLst(_) ->
    ok.
-endif.


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


%%% Replacing add/sub 1 with inc/dec is incorrect if the carry flag
%%% defined by the add/sub is used later -- disabled for now.
%peep([#alu{aluop='sub',src=#x86_imm{value=1},dst=Dst} | Insns]) ->
%    [hipe_x86:mk_dec(Dst) | peep(Insns)];
%peep([#alu{aluop='add',src=#x86_imm{value=1},dst=Dst} | Insns]) ->
%    [hipe_x86:mk_inc(Dst) | peep(Insns)];
%peep([#push{src=Src} | (Insns = [#pop{src=Src} | _])]) -> peep(Insns);


%% Old Working Code - Left for safety
%% --------------------------
% peepN(Insns) -> peepN(Insns, []).    
% peepN([#jmp_label{label=Label} | (Insns = [#label{label=Label}|_])], Lst) ->
%     peepN(Insns, [nearGotoElim | Lst]);
% peepN([I | Insns], Lst) ->
%     [I | peepN(Insns, Lst)];
% peepN([], Lst) ->
%     printLst(Lst),
%     [].

