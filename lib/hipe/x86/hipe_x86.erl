%%% $Id$
%%% representation of 2-address pseudo-x86 code

-module(hipe_x86).

-include("hipe_x86.hrl").

-export([mk_temp/2,
	 mk_nonallocatable_temp/2,
	 mk_new_temp/1,
	 mk_new_nonallocatable_temp/1,
	 is_temp/1,
	 temp_reg/1,
	 temp_type/1,
	 temp_is_allocatable/1,

	 mk_imm/1,
	 mk_imm_from_addr/2,
	 mk_imm_from_atom/1,
	 is_imm/1,
	 imm_value/1,

	 mk_mem/3,
	 is_mem/1,
	 mem_base/1,
	 mem_off/1,
	 mem_type/1,

	 mk_fpreg/1,
	 mk_fpreg/2,
	 is_fpreg/1,
	 fpreg_is_pseudo/1,
	 fpreg_reg/1,

	 mk_mfa/3,
	 is_mfa/1,
	 mfa_mfa/1,

	 mk_prim/1,
	 is_prim/1,
	 prim_prim/1,

	 mk_sdesc/4,

	 insn_type/1,

	 mk_alu/3,
	 is_alu/1,
	 alu_op/1,
	 alu_src/1,
	 alu_dst/1,

	 is_shift/1,

	 mk_call/2,
	 is_call/1,
	 call_fun/1,
	 call_sdesc/1,

	 mk_cmovcc/3,
	 is_cmovcc/1,
	 cmovcc_cc/1,
	 cmovcc_src/1,
	 cmovcc_dst/1,

	 mk_cmp/2,
	 is_cmp/1,
	 cmp_src/1,
	 cmp_dst/1,

	 mk_comment/1,
	 is_comment/1,
	 comment_term/1,

	 mk_dec/1,
	 is_dec/1,
	 dec_dst/1,

	 mk_fmov/2,
	 is_fmov/1,
	 fmov_src/1,
	 fmov_dst/1,

	 mk_fop/3,
	 is_fop/1,
	 fop_src/1,
	 fop_dst/1,
	 fop_op/1,

	 mk_inc/1,
	 is_inc/1,
	 inc_dst/1,

	 mk_jcc/2,
	 is_jcc/1,
	 jcc_cc/1,
	 jcc_label/1,

	 mk_jmp_fun/1,
	 is_jmp_fun/1,
	 jmp_fun_fun/1,

	 mk_jmp_label/1,
	 is_jmp_label/1,
	 jmp_label_label/1,

	 mk_jmp_switch/3,
	 is_jmp_switch/1,
	 jmp_switch_temp/1,
	 jmp_switch_jtab/1,
	 jmp_switch_labels/1,

	 mk_label/2,
	 is_label/1,
	 label_label/1,
	 label_isfail/1,

	 mk_lea/2,
	 is_lea/1,
	 lea_mem/1,
	 lea_temp/1,

	 mk_move/2,
	 is_move/1,
	 move_src/1,
	 move_dst/1,

	 mk_movsx/2,
	 is_movsx/1,
	 movsx_src/1,
	 movsx_dst/1,

	 mk_movzx/2,
	 is_movzx/1,
	 movzx_src/1,
	 movzx_dst/1,

	 mk_nop/0,
	 is_nop/1,

	 mk_pseudo_call/5,
	 is_pseudo_call/1,
	 pseudo_call_dsts/1,
	 pseudo_call_fun/1,
	 pseudo_call_arity/1,
	 pseudo_call_contlab/1,
	 pseudo_call_exnlab/1,

	 mk_pseudo_jcc/4,
	 is_pseudo_jcc/1,
	 pseudo_jcc_cc/1,
	 pseudo_jcc_true_label/1,
	 pseudo_jcc_false_label/1,
	 pseudo_jcc_pred/1,

	 mk_pseudo_tailcall/3,
	 is_pseudo_tailcall/1,
	 pseudo_tailcall_fun/1,
	 pseudo_tailcall_arity/1,
	 pseudo_tailcall_stkargs/1,

	 mk_pseudo_tailcall_prepare/0,
	 is_pseudo_tailcall_prepare/1,

	 mk_push/1,
	 is_push/1,
	 push_src/1,

	 mk_ret/1,
	 is_ret/1,
	 ret_npop/1,

	 mk_defun/8,
	 defun_mfa/1,
	 defun_formals/1,
	 defun_is_closure/1,
	 defun_is_leaf/1,
	 defun_code/1,
	 defun_data/1,
	 defun_var_range/1,
	 defun_label_range/1]).

%%%
%%% Low-level accessors.
%%%

mk_temp(Reg, Type) -> #x86_temp{reg=Reg, type=Type, allocatable=true}.
mk_nonallocatable_temp(Reg, Type) -> #x86_temp{reg=Reg, type=Type, allocatable=false}.
mk_new_temp(Type) ->
    mk_temp(hipe_gensym:get_next_var(), Type).
mk_new_nonallocatable_temp(Type) ->
   mk_nonallocatable_temp(hipe_gensym:get_next_var(), Type).
is_temp(X) -> case X of #x86_temp{} -> true; _ -> false end.
temp_reg(#x86_temp{reg=Reg}) -> Reg.
temp_type(#x86_temp{type=Type}) -> Type.
temp_is_allocatable(#x86_temp{allocatable=A}) -> A.

mk_imm(Value) -> #x86_imm{value=Value}.
mk_imm_from_addr(Addr, Type) ->
    mk_imm({Addr, Type}).
mk_imm_from_atom(Atom) ->
    mk_imm(Atom).
is_imm(X) -> case X of #x86_imm{} -> true; _ -> false end.
imm_value(#x86_imm{value=Value}) -> Value.

mk_mem(Base, Off, Type) -> #x86_mem{base=Base, off=Off, type=Type}.
is_mem(X) -> case X of #x86_mem{} -> true; _ -> false end.
mem_base(#x86_mem{base=Base}) -> Base.
mem_off(#x86_mem{off=Off}) -> Off.
mem_type(#x86_mem{type=Type}) -> Type.

mk_fpreg(Reg) -> #x86_fpreg{reg=Reg, pseudo=true}.
mk_fpreg(Reg, Pseudo) -> #x86_fpreg{reg=Reg, pseudo=Pseudo}.
is_fpreg(F) -> case F of #x86_fpreg{} -> true;_ -> false end.
fpreg_is_pseudo(#x86_fpreg{pseudo=Pseudo}) -> Pseudo.
fpreg_reg(#x86_fpreg{reg=Reg}) -> Reg.

mk_mfa(M, F, A) -> #x86_mfa{m=M, f=F, a=A}.
is_mfa(X) -> case X of #x86_mfa{} -> true; _ -> false end.
mfa_mfa(#x86_mfa{m=M, f=F, a=A}) -> {M, F, A}.

mk_prim(Prim) -> #x86_prim{prim=Prim}.
is_prim(X) -> case X of #x86_prim{} -> true; _ -> false end.
prim_prim(#x86_prim{prim=Prim}) -> Prim.

mk_sdesc(ExnLab, FSize, Arity, Live) ->
    #x86_sdesc{exnlab=ExnLab, fsize=FSize, arity=Arity, live=Live}.

insn_type(Insn) ->
    element(1, Insn).

is_insn_type(Insn, Type) ->
    case insn_type(Insn) of
	Type -> true;
	_ -> false
    end.

mk_alu(Op, Src, Dst) -> #alu{aluop=Op, src=Src, dst=Dst}.
is_alu(Insn) -> is_insn_type(Insn, alu).
alu_op(Alu) -> Alu#alu.aluop.
alu_src(Alu) -> Alu#alu.src.
alu_dst(Alu) -> Alu#alu.dst.

is_shift(Instr) ->
    case is_alu(Instr) of
	false ->
	    false;
	true ->
	    case alu_op(Instr) of
		sal ->
		    true;
		sar ->
		    true;
		shl ->
		    true;
		shr ->
		    true;
		_ ->
		    false
	    end
    end.

mk_call(Fun, SDesc) -> #call{'fun'=Fun, sdesc=SDesc}.
is_call(Insn) -> is_insn_type(Insn, call).
call_fun(Call) -> Call#call.'fun'.
call_sdesc(Call) -> Call#call.sdesc.

mk_cmovcc(Cc, Src, Dst) -> #cmovcc{cc=Cc, src=Src, dst=Dst}.
is_cmovcc(Insn) -> is_insn_type(Insn, cmovcc).
cmovcc_cc(C) -> C#cmovcc.cc.
cmovcc_src(C) -> C#cmovcc.src.
cmovcc_dst(C) -> C#cmovcc.dst.

mk_cmp(Src, Dst) -> #cmp{src=Src, dst=Dst}.
is_cmp(Insn) -> is_insn_type(Insn, cmp).
cmp_src(C) -> C#cmp.src.
cmp_dst(C) -> C#cmp.dst.

mk_comment(Term) -> #comment{term=Term}.
is_comment(Insn) -> is_insn_type(Insn, comment).
comment_term(Comment) -> Comment#comment.term.

mk_dec(Dst) -> #dec{dst=Dst}.
is_dec(Insn) -> is_insn_type(Insn, dec).
dec_dst(Dec) -> Dec#dec.dst.

mk_fmov(Src, Dst) -> #fmov{src=Src, dst=Dst}.
is_fmov(F) -> is_insn_type(F, fmov).
fmov_src(F) -> F#fmov.src.
fmov_dst(F) -> F#fmov.dst.

mk_fop(Op, Src, Dst) -> #fop{op=Op, src=Src, dst=Dst}.
is_fop(F) -> is_insn_type(F, fop).
fop_src(F) -> F#fop.src.
fop_dst(F) -> F#fop.dst.
fop_op(F) -> F#fop.op.

mk_inc(Dst) -> #inc{dst=Dst}.
is_inc(Insn) -> is_insn_type(Insn, inc).
inc_dst(Inc) -> Inc#inc.dst.

mk_jcc(Cc, Label) -> #jcc{cc=Cc, label=Label}.
is_jcc(Insn) -> is_insn_type(Insn, jcc).
jcc_cc(J) -> J#jcc.cc.
jcc_label(J) -> J#jcc.label.

mk_jmp_fun(Fun) -> #jmp_fun{'fun'=Fun}.
is_jmp_fun(Insn) -> is_insn_type(Insn, jmp_fun).
jmp_fun_fun(JF) -> JF#jmp_fun.'fun'.

mk_jmp_label(Label) -> #jmp_label{label=Label}.
is_jmp_label(Insn) -> is_insn_type(Insn, jmp_label).
jmp_label_label(JL) -> JL#jmp_label.label.

mk_jmp_switch(Temp, JTab, Labels) ->
    #jmp_switch{temp=Temp, jtab=JTab, labels=Labels}.
is_jmp_switch(Insn) -> is_insn_type(Insn, jmp_switch).
jmp_switch_temp(JS) -> JS#jmp_switch.temp.
jmp_switch_jtab(JS) -> JS#jmp_switch.jtab.
jmp_switch_labels(JS) -> JS#jmp_switch.labels.

mk_label(Label, IsFail) -> #label{label=Label, isfail=IsFail}.
is_label(Insn) -> is_insn_type(Insn, label).
label_label(Label) -> Label#label.label.
label_isfail(Label) -> Label#label.isfail.

mk_lea(Mem, Temp) -> #lea{mem=Mem, temp=Temp}.
is_lea(Insn) -> is_insn_type(Insn, lea).
lea_mem(Lea) -> Lea#lea.mem.
lea_temp(Lea) -> Lea#lea.temp.

mk_move(Src, Dst) -> #move{src=Src, dst=Dst}.
is_move(Insn) -> is_insn_type(Insn, move).
move_src(Move) -> Move#move.src.
move_dst(Move) -> Move#move.dst.

mk_movsx(Src, Dst) -> #movsx{src=Src, dst=Dst}.
is_movsx(Insn) -> is_insn_type(Insn, movsx).
movsx_src(Movsx) -> Movsx#movsx.src.
movsx_dst(Movsx) -> Movsx#movsx.dst.

mk_movzx(Src, Dst) -> #movzx{src=Src, dst=Dst}.
is_movzx(Insn) -> is_insn_type(Insn, movzx).
movzx_src(Movzx) -> Movzx#movzx.src.
movzx_dst(Movzx) -> Movzx#movzx.dst.

mk_nop() -> #nop{}.
is_nop(Insn) -> is_insn_type(Insn, nop).

mk_pseudo_call(Dsts, Fun, Arity, ContLab, ExnLab) ->
    #pseudo_call{dsts=Dsts, 'fun'=Fun, arity=Arity, contlab=ContLab, exnlab=ExnLab}.
is_pseudo_call(Insn) -> is_insn_type(Insn, pseudo_call).
pseudo_call_dsts(C) -> C#pseudo_call.dsts.
pseudo_call_fun(C) -> C#pseudo_call.'fun'.
pseudo_call_arity(C) -> C#pseudo_call.arity.
pseudo_call_contlab(C) -> C#pseudo_call.contlab.
pseudo_call_exnlab(C) -> C#pseudo_call.exnlab.

mk_pseudo_jcc(Cc, TrueLabel, FalseLabel, Pred) ->	% 'smart' constructor
    if Pred >= 0.5 ->
	    mk_pseudo_jcc_simple(neg_cc(Cc), FalseLabel, TrueLabel, 1.0-Pred);
       true ->
	    mk_pseudo_jcc_simple(Cc, TrueLabel, FalseLabel, Pred)
    end.
neg_cc(Cc) ->
    case Cc of
	'e'	-> 'ne';	% ==, !=
	'ne'	-> 'e';		% !=, ==
	'g'	-> 'le';	% >, <=
	'a'	-> 'be';	% >u, <=u
	'ge'	-> 'l';		% >=, <
	'ae'	-> 'b';		% >=u, <u
	'l'	-> 'ge';	% <, >=
	'b'	-> 'ae';	% <u, >=u
	'le'	-> 'g';		% <=, >
	'be'	-> 'a';		% <=u, >u
	'o'	-> 'no';	% overflow, not_overflow
	'no'	-> 'o';		% not_overflow, overflow
	_	-> exit({?MODULE, {"unknown cc", Cc}})
    end.
mk_pseudo_jcc_simple(Cc, TrueLabel, FalseLabel, Pred) ->
    #pseudo_jcc{cc=Cc, true_label=TrueLabel, false_label=FalseLabel, pred=Pred}.
is_pseudo_jcc(Insn) -> is_insn_type(Insn, pseudo_jcc).
pseudo_jcc_cc(J) -> J#pseudo_jcc.cc.
pseudo_jcc_true_label(J) -> J#pseudo_jcc.true_label.
pseudo_jcc_false_label(J) -> J#pseudo_jcc.false_label.
pseudo_jcc_pred(J) -> J#pseudo_jcc.pred.

mk_pseudo_tailcall(Fun, Arity, StkArgs) ->
    #pseudo_tailcall{'fun'=Fun, arity=Arity, stkargs=StkArgs}.
is_pseudo_tailcall(Insn) -> is_insn_type(Insn, pseudo_tailcall).
pseudo_tailcall_fun(C) -> C#pseudo_tailcall.'fun'.
pseudo_tailcall_arity(C) -> C#pseudo_tailcall.arity.
pseudo_tailcall_stkargs(C) -> C#pseudo_tailcall.stkargs.

mk_pseudo_tailcall_prepare() -> #pseudo_tailcall_prepare{}.
is_pseudo_tailcall_prepare(Insn) -> is_insn_type(Insn, pseudo_tailcall_prepare).

mk_push(Src) -> #push{src=Src}.
is_push(Insn) -> is_insn_type(Insn, push).
push_src(Push) -> Push#push.src.

mk_ret(NPop) -> #ret{npop=NPop}.
is_ret(Insn) -> is_insn_type(Insn, ret).
ret_npop(Ret) -> Ret#ret.npop.

mk_defun(MFA, Formals, IsClosure, IsLeaf, Code, Data, VarRange, LabelRange) ->
   #defun{mfa=MFA, formals=Formals, code=Code, data=Data,
	  isclosure=IsClosure, isleaf=IsLeaf,
	  var_range=VarRange, label_range=LabelRange}.
defun_mfa(DF) -> DF#defun.mfa.
defun_formals(DF) -> DF#defun.formals.
defun_is_closure(DF) -> DF#defun.isclosure.
defun_is_leaf(DF) -> DF#defun.isleaf.
defun_code(DF) -> DF#defun.code.
defun_data(DF) -> DF#defun.data.
defun_var_range(DF) -> DF#defun.var_range.
defun_label_range(DF) -> DF#defun.label_range.
