%%% $Id$
%%% apply final code expansions and some peephole optimisations before assembly
%%%
%%% Currently implemented transformations:
%%% - replace pseudo_jcc with jcc;jmp
%%% - remove pseudo_tailcall_prepare
%%% - remove jmp to the next instruction
%%% - replace add,sub 1 with inc,dec [disabled, needs stricter checks]

-module(hipe_x86_finalise).
-export([finalise/1]).
-include("hipe_x86.hrl").

finalise(Defun) ->
    #defun{code=Code0} = Defun,
    Code1 = transform(Code0),
    Defun#defun{code=Code1}.

transform(Code) ->
    %% done in two passes for simplicity -- merge the passes later IF
    %% this turns out to be a performance bottleneck
    peep(expand(Code)).

%%%
%%% Apply some peephole optimisations.
%%%

peep([#jmp_label{label=Label} | (Insns = [#label{label=Label} | _])]) ->
    peep(Insns);
%%% Replacing add/sub 1 with inc/dec is incorrect if the carry flag
%%% defined by the add/sub is used later -- disabled for now.
%peep([#alu{aluop='sub',src=#x86_imm{value=1},dst=Dst} | Insns]) ->
%    [hipe_x86:mk_dec(Dst) | peep(Insns)];
%peep([#alu{aluop='add',src=#x86_imm{value=1},dst=Dst} | Insns]) ->
%    [hipe_x86:mk_inc(Dst) | peep(Insns)];
peep([I | Insns]) ->
    [I | peep(Insns)];
peep([]) ->
    [].

%%%
%%% Apply final code expansions.
%%%

expand([I | Insns]) ->
    expand_insn(I, expand(Insns));
expand([]) ->
    [].

expand_insn(I, Tail) ->
    case I of
	#pseudo_jcc{} ->
	    expand_pseudo_jcc(I, Tail);
	#pseudo_tailcall_prepare{} ->
	    Tail;
	_ ->
	    [I | Tail]
    end.

expand_pseudo_jcc(I, Tail) ->
    #pseudo_jcc{cc=Cc,true_label=TrueLab,false_label=FalseLab} = I, % ignore pred
    [hipe_x86:mk_jcc(Cc, TrueLab), hipe_x86:mk_jmp_label(FalseLab) | Tail].
