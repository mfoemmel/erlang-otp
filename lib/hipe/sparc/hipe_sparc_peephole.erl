%%%----------------------------------------------------------------------
%%% File    : hipe_sparc_peephole.erl
%%% Author  : Christoffer Vikstrom <chvi3471@student.uu.se>
%%% Purpose : Contain peephole optimisations for sparc-assembler code.
%%% Created : 16 Feb 2004 by Christoffer Vikström <chvi3471@student.uu.se>
%%%----------------------------------------------------------------------

-module(hipe_sparc_peephole).
-author('chvi3471@student.uu.se').

-export([peep/1]).

-include("hipe_sparc.hrl").

-define(DO_PEEP, true).
%%-define(DO_LOGGING, true).  % Uncomment this to get peephole opts logged to..
-define(LOG_FILE, "peepcount.txt"). % ..this file..
-define(LOG_DIR, "/home/chvi3471/otp/tests/"). % ..in this dir


%%>----------------------------------------------------------------------<
%  Procedure : peep/1
%  Purpose   : Function that does peephole optimizations. It works by 
%              moving a window over the code and looking at a sequence of 
%              a few instructions. Replaces long sequences of instructions
%              with shorter ones and removes unnecesary ones.
%  Arguments : LinearCode  - List of pseudo sparc-assembler records.
%              Res     - Returned list of pseudo sparc-assembler records. 
%                        Kept reversed, until it is returned.
%              Lst     - List of optimizations done. For debuging.
%  Return    : An optimized list of pseudo sparc-assembler records with 
%              (hopefully) fewer or faster instructions.
%  Notes     : Creates a file in the users home directory that contain 
%              analysis information, if macro ?DO_LOGGING is defined.
%%>----------------------------------------------------------------------<
peep(LinearCode) -> 
    case ?DO_PEEP of
	true ->
	    peep(LinearCode, [], []);
	_ ->
	    LinearCode
    end.


%% MoveSelfSimple
%% --------------
peep([#move{dst=Dst, src=Dst}|Insns], Ack, Lst) ->
    peep(Insns, Ack, [moveSelf1|Lst]);
peep([#cmov_cc{dst=Dst, src=Dst}|Insns], Ack, Lst) ->
    peep(Insns, Ack, [moveSelf2|Lst]);
peep([#fmove{dst=Dst, src=Dst, negate=false, abs=false}|Insns], Ack, Lst) ->
    peep(Insns, Ack, [moveSelf2|Lst]);

%% MoveSelfDouble
%% --------------
peep([#move{dst=Dst, src=Src}, #move{dst=Src, src=Dst}|Insns], Ack, Lst) ->
    peep(Insns, [#move{dst=Dst, src=Src}|Ack], [moveSelfDouble1|Lst]);
peep([#cmov_cc{dst=Dst, src=Src},#cmov_cc{dst=Src, src=Dst}|Insns],Ack,Lst) ->
    peep(Insns, [#cmov_cc{dst=Dst, src=Src}|Ack], [moveSelfDouble2|Lst]);
peep([#fmove{dst=Dst, src=Src}, 
      #fmove{dst=Src, src=Dst, negate=false, abs=false}|Insns], Ack, Lst) ->
    peep(Insns, [#fmove{dst=Dst, src=Src}|Ack], [moveSelfDouble1|Lst]);


%% CommuteBinALMD
%% --------------
peep([M=#move{dst=Dst, src=SrcImm}, 
      A=#alu{dst=Dst, src1=Dst, op=Op, src2=Src2}|Insns], Ack, Lst) -> 
    case (Op=='+') or (Op=='and') or (Op=='or') or (Op=='xor') or
	(Op=='xnor') or (Op=='andn') of
	true -> 
	    case hipe_sparc:is_imm(SrcImm) of
		true ->
		    A2 = #alu{dst=Dst, src1=SrcImm, op=Op, src2=Src2},
		    peep(Insns, [A2|Ack], [commuteBinALMD|Lst]);
		false ->
		    peep(Insns, [A,M|Ack], Lst)
	    end;
	false ->
	    peep(Insns, [A,M|Ack], Lst)
    end;
peep([M=#move{dst=Dst, src=SrcImm},  
      A=#alu{dst=Dst, src1=Src1, op=Op, src2=Dst}|Insns], Ack, Lst) -> 
    case (Op=='+') or (Op=='and') or (Op=='or') or (Op=='xor') or
	(Op=='xnor') or (Op=='andn') of
	true -> 
	    case hipe_sparc:is_imm(SrcImm) of
		true ->
		    A2 = #alu{dst=Dst, src1=SrcImm, op=Op, src2=Src1},
		    peep(Insns, [A2|Ack], [commuteBinALMD|Lst]);
		false ->
		    peep(Insns, [A,M|Ack], Lst)
	    end;
	false ->
	    peep(Insns, [A,M|Ack], Lst)
    end;


%% ElimBinALMDouble
%% ----------------
peep([M=#move{dst=Dst}, I=#alu{dst=Dst,src1=Src1,src2=Src2}|Insns],Ack,Lst) ->
    case (Dst/=Src1) and (Dst/=Src2) of
	true ->
	    peep(Insns, [I|Ack], [elimBinALMDouble|Lst]);
	false ->
	    peep(Insns, [I,M|Ack], Lst)
    end;


%% ElimIFF
%% -------
peep([#b{true_label=Label, false_label=Label}|Insns], Ack, Lst) ->
    peep(Insns, [#goto{label=Label}|Ack], [elimIFF|Lst]);
    

%% ElimSet0
%% --------
peep([I=#move{dst=Dst, src={sparc_imm, Val}}|Insns], Ack, Lst) ->
    case (Val==0) of
	true ->
	    peep(Insns, 
		 [#alu{dst=Dst, src1=Dst, op='xor', src2=Dst}|Ack], 
		 [elimSet0|Lst]);
	false ->
	    peep(Insns, [I|Ack], Lst)
    end;
	    

%% ElimPushPop
%% -----------
peep([#pseudo_push{reg=Reg}, #pseudo_pop{reg=Reg} | Insns], Ack, Lst) ->
    peep(Insns, Ack, [elimPushPop|Lst]);
    

%% ElimStoreLoad
%% -------------
peep([S=#store{src=Src, dst=Dst, off=Off}, 
         #load{dst=Src, src=Dst, off=Off}|Insns], Ack, Lst) -> 
    peep(Insns, [S|Ack], Lst);

%% ElimLoad
%% --------
peep([S=#store{src=Src, dst=Dst, off=Off}, 
         #load{dst=Dst2,src=Dst, off=Off}|Insns], Ack, Lst) ->
    peep(Insns, [#move{dst=Dst2, src=Src}, S|Ack], Lst);
    

%% ElimMDPow2
%% ----------
peep([B = #alu{dst=Dst, src1=Src1, op=Op, src2={sparc_imm, Val}}|Insns],
     Res, Lst) ->
    {IsLog2, Size, Sign} = log2(Val),
    case ((Op == 'smul') or (Op == 'sdiv')) and IsLog2 of
	true ->
	    Sh = case Sign of positive -> '<<'; negative -> '>>?' end,
	    Alu = #alu{dst=Dst, src1=Src1, op=Sh, src2={sparc_imm, Size}},
	    peep(Insns, [Alu|Res], [elimMDPow2|Lst]);
	false ->
	    case ((Op == 'umul') or (Op == 'udiv')) and IsLog2 of
		true ->
		    Sh = case Sign of positive -> '<<'; negative -> '>>' end,
		    Alu = #alu{dst=Dst, src1=Src1, 
			       op=Sh, src2={sparc_imm, Size}},
		    peep(Insns, [Alu|Res], [elimMDPow2|Lst]);
		false ->
		    peep(Insns, [B|Res], Lst)
	    end
    end;



%% GotoSelf
%% --------
peep([#goto{label=Label}, L = #label{id=Label}|Insns], Ack, Lst) ->
    peep(Insns, [L|Ack], [gotoSelf|Lst]);

peep([I|Insns], Ack, Lst) ->
    peep(Insns, [I|Ack], Lst);
peep([], Ack, Lst) -> printLst(Lst), lists:reverse(Ack).
    
    


%%  Miscellaneous helper functions
%% >-------------------------------------------------------------------------<


%% Log2 function
%% -------------
%% Used by ElimMDPow2 clause of peep(..)
log2(Nr) -> log2(Nr, 0).
log2(0, _) -> {false, 0, positive};
log2(Nr, I) ->
    case (Nr band 1) == 1 of
	false ->
	    log2((Nr bsr 1), I+1);
	true ->
	    case Nr of
		1 ->
		    {true, I, positive};
		-1 ->
		    {true, I, negative};
		_ ->
		    {false, 0, positive}
	    end
    end.


%%>----------------------------------------------------------------------<
%% Procedure : printLogHeader/1
%% Purpose   : Prints the headers for each function compiled.
%% Arguments : 
%% Return    : unit
%% Notes     : 
%%>----------------------------------------------------------------------<
%% -ifdef(DO_LOGGING).
%% printLogHeader(MFA) ->
%%     {x86_mfa, M, F, _} = MFA,
%%     {ok, Dir} = file:get_cwd(),
%%     file:set_cwd(?LOG_DIR),
%%     {ok, File} = file:open(?LOG_FILE, [read, append]),
%%     io:format(File, 
%% 		 "\nModule: ~w  Function: ~w" ++ 
%% 		 "\n>==============================================<\n", 
%% 		 [M, F]),
%%     file:set_cwd(Dir),
%%     file:close(File).
%% -else.
%% printLogHeader(_) ->
%%     ok.
%% -endif.


%%>----------------------------------------------------------------------<
%% Procedure : printLst/1
%% Purpose   : Prints ths name of the peephole optimizations done in the
%%             current function being compiled.
%% Arguments : Lst - A list of atoms.
%% Return    : unit
%% Notes     : Prints (append) the atoms in the list, with a space in 
%%             between, to a file specified by the macro LOG_FILE. 
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

-else.

printLst(_) ->
    ok.
-endif.
