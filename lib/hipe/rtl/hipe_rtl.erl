% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% RTL - register transfer language
%
%
% Consists of the instructions
%
%    - move - Dst, Src, Info
%    - multimove - [Dst1, ..., DstN], [Src1, ..., SrcN], Info
%    - alu - Dst, Src1, Op, Src2, Info
%    - fp - Dst, Src1, Op, Src2, Info}
%    - load_tagged  Dst, Src, Off, Tag, Info
%    - {load, Dst, Src, Off, Info}
%    - {load_atom, Dst, Atom, Info}
%    - {load_address, Dst, Addr, Type, Info}
%    - {store, Dst, Off, Src, Info}
%    - {label, Name, Info}
%    - {branch, Src1, RelOp, Src2, TrueLabel, FalseLabel, Info}
%    - {alu_branch, Src1, RelOp, Src2, TrueLabel, FalseLabel, Info}
%    - {switch, Src1, Labels, Info}
%    - {goto, Label, Info}
%    - {return, Vars, Info}
%    - {comment, Text, Info}
%
%      RTL(1)
%    - {call, Dst, Fun, Args, Type, Continuation, FailContinuation,, Info}
%       Type is one of {local, remote, primop, closure}
%    - {enter, Fun, Args, Type, Info}
%       Type is one of {local, remote, primop, closure}
%    - {save_frame, Vars, Info}
%    - {restore_frame, Vars, Info}
%    - {pop_frame, Size, Info}
%    - {restore_catch, Vars, Info}
%    - {fail_to, Reason, Label, Info}
%
%      RTL(2)
%    - {jsr, Fun, Type, Continuation, FailContinuation, Info}
%       Type is one of {local, remote, primop, closure}
%    - {esr, Fun, Type, Info}
%       Type is one of {local, remote, primop, closure}
%    - {jmp, Target, Off, Info}
%    - {jmp_link, Target, Off, Link, Continuation, FailContinuation, Info}
%
%
% There are three kinds of 'registers' in rtl.
%    - Variables containing tagged data that are traced by the GC.
%    - Registers that are ignored by the GC.
%    - Floating point registers. (- NYI or at least NYUsed)
%  
%

-module(hipe_rtl).
-include("hipe_rtl.hrl").

-export([mk_rtl/6,
	 rtl_fun/1,
	 rtl_params/1,
	 rtl_code/1,
	 rtl_code_update/2,
	 rtl_data/1,
	 rtl_data_update/2,
	 rtl_var_range/1,
	 rtl_var_range_update/2,
	 rtl_label_range/1,
	 rtl_label_range_update/2,
	 rtl_info/1,
	 rtl_info_update/2,
	 type/1]).

-export([mk_move/2,
	 move_dst/1,
	 move_src/1,
	 move_src_update/2,
	 is_move/1,

	 mk_multimove/2,
	 multimove_dst/1,
	 multimove_src/1,
	 multimove_src_update/2,
	 is_multimove/1,
	 multimove_info/1,
	 multimove_info_update/2,

	 mk_alu/4,
	 alu_dst/1,
	 alu_src1/1,
	 alu_src1_update/2,
	 alu_src2/1,
	 alu_src2_update/2,
	 alu_op/1,
	 is_alu_op/1,
	 
	 mk_load/3,
	 load_dst/1,
	 load_src/1,
	 load_offset/1,

	 mk_load_tagged/4,
	 load_tagged_dst/1,
	 load_tagged_src/1,
	 load_tagged_offset/1,
	 load_tagged_tag/1,

	 mk_load_atom/2,
	 load_atom_dst/1,
	 load_atom_atom/1,
	 
	 mk_load_word_index/3,
	 load_word_index_dst/1,
	 load_word_index_index/1,
	 load_word_index_block/1,
	 
	 mk_goto_index/3,
	 goto_index_index/1,
	 goto_index_block/1,
	 goto_index_labels/1,

	 mk_load_address/3,
	 load_address_dst/1,
	 load_address_dst_update/2,
	 load_address_address/1,
	 load_address_type/1,
	 load_address_type_update/2,

	 mk_store/3,
	 store_dst/1,
	 store_src/1,
	 store_offset/1,

	 mk_label/1,
	 mk_new_label/0,
	 label_name/1,
	 label_info_add/2,
	 is_label/1,

	 mk_branch/5,
	 mk_branch/6,
	 branch_src1/1,
	 branch_src2/1,
	 branch_cond/1,
	 branch_true_label/1,
	 branch_false_label/1,
	 branch_pred/1,
	 is_branch/1,
	 branch_true_label_update/2,
	 branch_false_label_update/2,

	 mk_alub/7,
	 mk_alub/8,
	 alub_dst/1,
	 alub_src1/1,
	 alub_op/1,
	 alub_src2/1,
	 alub_cond/1,
	 alub_true_label/1,
	 alub_true_label_update/2,
	 alub_false_label/1,
	 alub_false_label_update/2,
	 alub_pred/1,
	 alub_info/1,
	 is_alub/1,

	 mk_switch/2,
	 mk_switch/3,
	 mk_sorted_switch/3,
	 switch_src/1,
	 switch_src_update/2,
	 switch_labels/1,
	 switch_labels_update/2,
	 switch_sort_order/1,
	 switch_sort_order_update/2,
	 switch_info/1,
	 switch_info_update/2,


	 mk_jsr/5,
	 jsr_fun/1,
	 jsr_type/1,
	 jsr_args/1,
	 jsr_continuation/1,
	 jsr_fail/1,
	 jsr_info/1,
	 jsr_info_update/2,
	 
	 mk_esr/3,
	 esr_fun/1,
	 esr_type/1,
	 esr_args/1,
	 
	 mk_jmp/3,
	 jmp_target/1,
	 jmp_offset/1,
	 jmp_args/1,
%	 jmp_continuation/1,
%	 jmp_fail/1,

	 mk_jmp_link/6,

	 mk_goto/1,
	 goto_label/1,
	 is_goto/1,
	 goto_label_update/2,

	 mk_fail_to/2,
	 fail_to_label/1,
	 fail_to_reason/1,
	 
	 mk_call/6,
	 call_fun/1,
	 call_args/1,
	 call_dst/1,
	 call_type/1,
	 call_continuation/1,
	 call_fail/1,
	 call_info/1,
	 call_info/1,
	 call_info_update/2,
	 call_continuation_update/2,
	 call_fail_update/2,

	 mk_enter/3,
	 enter_fun/1,
	 enter_args/1,
	 enter_type/1,
	 
	 mk_return/1,
	 return_vars/1,
	 
	 mk_save_frame/1,
	 save_frame_vars/1,
	 save_frame_size/1,
	 
	 mk_restore_frame/1,
	 restore_frame_vars/1,
	 restore_frame_size/1,
	 
	 mk_pop_frame/1,
	 pop_frame_size/1,

	 mk_restore_catch/1,
	 restore_catch_vars/1,
	 restore_catch_size/1,

	 mk_stackneed/1,
	 stackneed_words/1,
	 stackneed_info/1,
	 stackneed_info_update/2,

	 mk_gctest/1,
	 gctest_words/1,
	 gctest_info/1,
	 gctest_info_update/2,

	 mk_comment/1,
	 comment_text/1,
	 is_comment/1,

	 mk_var/1,
	 mk_new_var/0,
	 is_var/1,
	 var_name/1,

	 mk_reg/1,
	 mk_new_reg/0,
	 is_reg/1,
	 reg_name/1,

	 mk_fpreg/1,
	 is_fpreg/1,
	 fpreg_name/1,

	 mk_imm/1,
	 is_imm/1,
	 imm_value/1,

	 info/1,
	 info_add/2,
	 info_update/2,
	 uses/1,
	 subst_uses/2,
	 defines/1,
	 redirect_jmp/3,
	 remove_empty_bbs/1,
	 is_pure/1,
	 pp/1,
	 pp/2]).


%
% rtl
%

mk_rtl(Fun, Args, Code, Data, VarRange, LabelRange) ->
   #rtl{'fun'=Fun, args=Args, code=Code, 
	data=Data,
	var_range=VarRange, 
	label_range=LabelRange}.
rtl_fun(Rtl) -> Rtl#rtl.'fun'.
rtl_params(Rtl) -> Rtl#rtl.args.
rtl_code(Rtl) -> Rtl#rtl.code.
rtl_code_update(Rtl, Code) -> Rtl#rtl{code=Code}.
rtl_data(Rtl) -> Rtl#rtl.data.
rtl_data_update(Rtl, Data) -> Rtl#rtl{data=Data}.
rtl_var_range(Rtl) -> Rtl#rtl.var_range.
rtl_var_range_update(Rtl, VarRange) -> Rtl#rtl{var_range=VarRange}.
rtl_label_range(Rtl) -> Rtl#rtl.label_range.
rtl_label_range_update(Rtl, LabelRange) -> Rtl#rtl{label_range=LabelRange}.
rtl_info(Rtl) -> Rtl#rtl.info.
rtl_info_update(Rtl, Info) -> Rtl#rtl{info=Info}.


%
% move
%

mk_move(Dst, Src) -> #move{dst=Dst, src=Src, info=[]}.
move_dst(M) -> M#move.dst.
move_src(M) -> M#move.src.
move_src_update(M, NewSrc) -> M#move{src=NewSrc}.
move_info(M) -> M#move.info.
is_move({move,_,_,_}) -> true;
is_move(_) -> false.
move_info_update(M, I) -> M#move{info=I}.

%
% multi_move
%

mk_multimove(Dst, Src) -> 
  case length(Dst) =:= length(Src) of
    true -> true;
    false ->
      exit({rtl, multimove, different_src_dest_arity})
  end,
  #multimove{dst=Dst, src=Src, info=[]}.
multimove_dst(M) -> M#multimove.dst.
multimove_src(M) -> M#multimove.src.
multimove_src_update(M, NewSrc) -> M#multimove{src=NewSrc}.
multimove_info(M) -> M#multimove.info.
is_multimove({multimove,_,_,_}) -> true;
is_multimove(_) -> false.
multimove_info_update(M, I) -> M#multimove{info=I}.

%
% alu
%

mk_alu(Dst, Src1, Op, Src2) -> 
   #alu{dst=Dst, src1=Src1, op=Op, src2=Src2, info=[]}.
alu_dst(Alu) -> Alu#alu.dst.
alu_src1(Alu) -> Alu#alu.src1.
alu_src1_update(Alu, NewSrc) -> Alu#alu{src1=NewSrc}.
alu_src2(Alu) -> Alu#alu.src2.
alu_src2_update(Alu, NewSrc) -> Alu#alu{src2=NewSrc}.
alu_op(Alu) -> Alu#alu.op.
alu_info(Alu)-> Alu#alu.info.
alu_info_update(Instr, Info) -> Instr#alu{info=Info}.


%
% load
%

mk_load(Dst, Src, Off) -> #load{dst=Dst, src=Src, off=Off, info=[]}.
load_dst(L) -> L#load.dst.
load_src(L) -> L#load.src.
load_src_update(L, NewSrc) -> L#load{src=NewSrc}.
load_offset(L) -> L#load.off.
load_offset_update(L, NewOff) -> L#load{off=NewOff}.
load_info(L) -> L#load.info.
load_info_update(Instr, Info) -> Instr#load{info=Info}.

%
% load_tagged
%

mk_load_tagged(Dst, Src, Off, Tag) -> {load_tagged, Dst, Src, Off, Tag, []}.
load_tagged_dst({load_tagged, Dst, Src, Off, Tag, Info}) -> Dst.
load_tagged_src({load_tagged, Dst, Src, Off, Tag, Info}) -> Src.
load_tagged_offset({load_tagged, Dst, Src, Off, Tag, Info}) -> Off.
load_tagged_tag({load_tagged, Dst, Src, Off, Tag, Info}) -> Tag.
load_tagged_info({load_tagged, Dst, Src, Off, Tag, Info}) -> Info.
load_tagged_info_update(Instr, Info) -> Instr#load_tagged{info=Info}.

%
% load_atom
%

mk_load_atom(Dst, Atom) -> {load_atom, Dst, Atom, []}.
load_atom_dst({load_atom, Dst, Atom, Info}) -> Dst.
load_atom_atom({load_atom, Dst, Atom, Info}) -> Atom.
load_atom_info({load_atom, Dst, Atom, Info}) -> Info.
load_atom_info_update(Instr, Info) -> Instr#load_atom{info=Info}.


mk_load_word_index(Dst, Block, Index) -> {load_word_index, Dst, Block, Index, []}.
load_word_index_dst({load_word_index, Dst, Block, Index, Info}) -> Dst.
load_word_index_index({load_word_index, Dst, Block, Index, Info}) -> Index.
load_word_index_block({load_word_index, Dst, Block, Index, Info}) -> Block.
load_word_index_info({load_word_index, Dst, Block, Index, Info}) -> Info.
load_word_index_info_update(Instr, Info) -> Instr#load_word_index{info=Info}.

mk_goto_index(Block, Index, Labels) -> {goto_index, Block, Index, Labels, []}.
goto_index_index({goto_index, Block, Index, Labels, Info}) -> Index.
goto_index_block({goto_index, Block, Index, Labels, Info}) -> Block.
goto_index_labels({goto_index, Block, Index, Labels, Info}) -> Labels.
goto_index_info({goto_index, Block, Index, Labels, Info}) -> Info.
goto_index_info_update(Instr, Info) -> Instr#goto_index{info=Info}.



%
% load_address
%

mk_load_address(Dst, Addr, Type) -> 
   #load_address{dst=Dst,address=Addr, type=Type, info=[]}.
load_address_dst(LoadAddress) -> LoadAddress#load_address.dst.
load_address_dst_update(LA, NewDst) -> LA#load_address{dst=NewDst}.
load_address_address(LoadAddress) -> LoadAddress#load_address.address.
load_address_type(LoadAddress) -> LoadAddress#load_address.type.
load_address_type_update(LA, NewType) -> LA#load_address{type=NewType}.
load_address_info(LA) -> LA#load_address.info.
load_address_info_update(Instr, Info) -> Instr#load_address{info=Info}.

%
% store
%

mk_store(Dst, Off, Src) -> 
   #store{dst=Dst, off=Off, src=Src, info=[]}.
store_dst(S) -> S#store.dst.
store_dst_update(S, NewDst) -> S#store{dst=NewDst}.
store_offset(S) -> S#store.off.
store_offset_update(S, NewOff) -> S#store{off=NewOff}.
store_src(S) -> S#store.src.
store_src_update(S, NewSrc) -> S#store{src=NewSrc}.
store_info(S) -> S#store.info.
store_info_update(Instr, Info) -> Instr#store{info=Info}.

%
% label
%

mk_label(Name) -> {label, Name, []}.
mk_new_label() -> mk_label(hipe_gensym:get_next_label(rtl)).
label_name({label, Name, Info}) -> Name.
label_info({label, Name, Info}) -> Info.
is_label(I) -> case I of {label, _, _} -> true; _ -> false end.
label_info_add({label, Name, Info}, MoreInfo) -> 
  {label, Name, Info++MoreInfo}.
label_info_update({label, Name, Info}, NewInfo) -> {label, Name, NewInfo}.


%
% branch
%

mk_branch(Src1, Op, Src2, True, False) ->
   mk_branch(Src1, Op, Src2, True, False, 0.5).
mk_branch(Src1, Op, Src2, True, False, P) ->
   #branch{src1=Src1, cond=Op, src2=Src2, true_label=True, false_label=False,
	   p=P, info=[]}.
branch_src1(Br) -> Br#branch.src1.
branch_src1_update(Br, NewSrc) -> Br#branch{src1=NewSrc}.
branch_src2(Br) -> Br#branch.src2.
branch_src2_update(Br, NewSrc) -> Br#branch{src2=NewSrc}.
branch_cond(Br) -> Br#branch.cond.
branch_true_label(Br) -> Br#branch.true_label.
branch_true_label_update(Br, NewTrue) -> Br#branch{true_label=NewTrue}.
branch_false_label(Br) -> Br#branch.false_label.
branch_false_label_update(Br, NewFalse) -> Br#branch{false_label=NewFalse}.
branch_pred(Br) -> Br#branch.p.
branch_info(Br) -> Br#branch.info.
is_branch(X) when record(X, branch) -> true; is_branch(_) -> false.
branch_info_update(Instr, Info) -> Instr#branch{info=Info}.

%
% alub
%

mk_alub(Dst, Src1, Op, Src2, Cond, True, False) ->
   mk_branch(Src1, Op, Src2, True, False, 0.5).
mk_alub(Dst, Src1, Op, Src2, Cond, True, False, P) ->
   #alub{dst=Dst, src1=Src1, op=Op, src2=Src2, cond=Cond,
	 true_label=True, false_label=False, p=P, info=[]}.
alub_dst(A) -> A#alub.dst.
alub_src1(A) -> A#alub.src1.
alub_src1_update(A, NewSrc) -> A#alub{src1=NewSrc}.
alub_op(A) -> A#alub.op.
alub_src2(A) -> A#alub.src2.
alub_src2_update(A, NewSrc) -> A#alub{src2=NewSrc}.
alub_cond(A) -> A#alub.cond.
alub_true_label(A) -> A#alub.true_label.
alub_true_label_update(A, NewTrue) -> A#alub{true_label=NewTrue}.
alub_false_label(A) -> A#alub.false_label.
alub_false_label_update(A, NewFalse) -> A#alub{false_label=NewFalse}.
alub_pred(A) -> A#alub.p.
alub_info(A) -> A#alub.info.
is_alub(X) when record(X, alub) -> true; is_alub(_) -> false.
alub_info_update(Instr, Info) -> Instr#alub{info=Info}.


%
% switch
%
mk_switch(Src, Labels) -> #switch{src=Src, labels=Labels, info=[]}.
mk_sorted_switch(Src, Labels, Order) -> #switch{src=Src, labels=Labels,
					 sorted_by=Order, info=[]}.
mk_switch(Src, Labels, Info) -> #switch{src=Src, labels=Labels, info=Info}.
switch_src(I) -> I#switch.src.
switch_src_update(I, N) -> I#switch{src=N}.
switch_labels(I) -> I#switch.labels.
switch_labels_update(I,N) -> I#switch{labels=N}.
switch_sort_order(I) -> I#switch.sorted_by.
switch_sort_order_update(I,N) -> I#switch{sorted_by=N}.
switch_info(I) -> I#switch.info.
switch_info_update(I,N) -> I#switch{info=N}.

 

%
% goto
%

mk_goto(Label) -> {goto, Label, []}.
goto_label({goto, Label, Info}) -> Label.
goto_info({goto, Label, Info}) -> Info.
is_goto(X) -> case X of {goto, _, _} -> true; _ -> false end.
goto_label_update({goto, Label, Info}, NewLabel) -> {goto, NewLabel, Info}.
goto_info_update(Instr, Info) -> Instr#goto{info=Info}.

%
% fail_to
%

mk_fail_to(Reason, Label) -> #fail_to{reason=Reason, label=Label, info=[]}.
fail_to_label(I) -> I#fail_to.label.
fail_to_reason(I) -> I#fail_to.reason.
fail_to_info(I) -> I#fail_to.info.
fail_to_reason_update(I, NewReason) ->
  I#fail_to{reason=NewReason}.
fail_to_label_update(I, NewL) ->
  I#fail_to{label=NewL}.
fail_to_info_update(I, NewInfo) ->
  I#fail_to{info=NewInfo}.


%
% jsr
%

mk_jsr(Fun, Type, Args, Continuation, FailContinuation) -> 
  #jsr{'fun'=Fun, type=Type, args=Args, 
       continuation=Continuation, 
       failcontinuation=FailContinuation,
       info=[]}.
jsr_fun(Jsr) -> Jsr#jsr.'fun'.
jsr_fun_update(C, F) -> C#jsr{'fun'=F}.
jsr_type(Jsr) -> Jsr#jsr.type.
jsr_args(Jsr) -> Jsr#jsr.args.
jsr_continuation(Jsr) -> Jsr#jsr.continuation.
jsr_fail(Jsr) -> Jsr#jsr.failcontinuation.
jsr_info(Jsr) -> Jsr#jsr.info.
jsr_args_update(Jsr, NewArgs) -> Jsr#jsr{args=NewArgs}.
jsr_continuation_update(I, NewCont) -> I#jsr{continuation=NewCont}.
jsr_fail_update(I, NewCont) -> I#jsr{failcontinuation=NewCont}.
jsr_info_update(Jsr, I) -> Jsr#jsr{info=I}.

%
% esr
%

mk_esr(Fun, Type, Args) -> #esr{'fun'=Fun, type=Type, args=Args, info=[]}.
esr_fun(Esr) -> Esr#esr.'fun'.
esr_fun_update(Esr, F) -> Esr#esr{'fun'=F}.
esr_type(Esr) -> Esr#esr.type.
esr_args(Esr) -> Esr#esr.args.
%esr_args_update(Esr, NewArgs) -> Esr#esr{args=NewArgs}.
esr_info(Esr) -> Esr#esr.info.
esr_info_update(Instr, Info) -> Instr#esr{info=Info}.

%
% jmp
%

mk_jmp(Target, Off, Args) -> 
  #jmp{target=Target, off=Off, args=Args,
       info=[]}.
jmp_target(Jmp) -> Jmp#jmp.target.
jmp_target_update(Jmp, NewTarget) -> Jmp#jmp{target=NewTarget}.
jmp_offset(Jmp) -> Jmp#jmp.off.
jmp_offset_update(Jmp, NewOff) -> Jmp#jmp{off=NewOff}.
jmp_args(Jmp) -> Jmp#jmp.args.
%jmp_args_update(Jmp, NewArgs) -> Jmp#jmp{args=NewArgs}.
%jmp_continuation(I) -> I#jmp.continuation.
%jmp_fail(I) -> I#jmp.failcontinuation.
jmp_info(Jmp) -> Jmp#jmp.info.
%jmp_continuation_update(I, NewCont) -> I#jmp{continuation=NewCont}.
%jmp_fail_update(I, NewCont) -> I#jmp{failcontinuation=NewCont}.
jmp_info_update(Instr, Info) -> Instr#jmp{info=Info}.

%
% jmp_link
%

mk_jmp_link(Target, Off, Link, Args, Continuation, FailContinuation) ->
   #jmp_link{target=Target, off=Off, link=Link, args=Args, 
	     continuation=Continuation, 
	     failcontinuation=FailContinuation,
	     info=[]}.
jmp_link_target(Jmpl) -> Jmpl#jmp_link.target.
jmp_link_target_update(Jmpl, NewTarget) -> Jmpl#jmp_link{target=NewTarget}.
jmp_link_offset(Jmpl) -> Jmpl#jmp_link.off.
jmp_link_offset_update(Jmpl, NewOff) -> Jmpl#jmp_link{off=NewOff}.
jmp_link_link(Jmpl) -> Jmpl#jmp_link.link.
jmp_link_args(Jmpl) -> Jmpl#jmp_link.args.
%jmp_link_args_update(Jmpl, NewArgs) -> Jmpl#jmp_link{args=NewArgs}.
jmp_link_continuation(I) -> I#jmp_link.continuation.
jmp_link_fail(I) -> I#jmp_link.failcontinuation.
jmp_link_info(Jmpl) -> Jmpl#jmp_link.info.
jmp_link_continuation_update(I, NewCont) -> I#jmp_link{continuation=NewCont}.
jmp_link_fail_update(I, NewCont) -> I#jmp_link{failcontinuation=NewCont}.
jmp_link_info_update(Instr, Info) -> Instr#jmp_link{info=Info}.

%
% call
%

mk_call(Dst, Fun, Args, Type, Continuation, FailContinuation) -> 
   #call{dst=Dst, 'fun'=Fun, args=Args, type=Type, 
	 continuation=Continuation, 
	 failcontinuation=FailContinuation,
	 info=[]}.
call_fun(C) -> C#call.'fun'.
call_fun_update(C, F) -> C#call{'fun'=F}.
call_args(C) -> C#call.args.
call_args_update(C, NewArgs) -> C#call{args=NewArgs}.
call_dst(C) -> C#call.dst.
call_type(C) -> C#call.type.
call_continuation(I) -> I#call.continuation.
call_fail(I) -> I#call.failcontinuation.
call_info(C) -> C#call.info.
call_continuation_update(I, NewCont) -> I#call{continuation=NewCont}.
call_fail_update(I, NewCont) -> I#call{failcontinuation=NewCont}.
call_info_update(C, NewI) -> C#call{info=NewI}.

%
% enter
%

mk_enter(Fun, Args, Type) -> 
   #enter{'fun'=Fun, args=Args, type=Type, info=[]}.
enter_fun(E) -> E#enter.'fun'.
enter_fun_update(I, F) -> I#enter{'fun' = F}.
enter_args(E) -> E#enter.args.
enter_args_update(E, NewArgs) -> E#enter{args=NewArgs}.
enter_type(E) -> E#enter.type.
enter_info(E) -> E#enter.info.
enter_info_update(Instr, Info) -> Instr#enter{info=Info}.

%
% return
%

mk_return(Vars) -> #return{vars=Vars, info=[]}.
return_vars(R) -> R#return.vars.
return_vars_update(R, NewVars) -> R#return{vars=NewVars}.
return_info(R) -> R#return.info.
return_info_update(Instr, Info) -> Instr#return{info=Info}.

%
% save_frame
%

mk_save_frame(Vars) -> {save_frame, Vars, []}.
save_frame_vars({save_frame, Vars, Info}) -> Vars.
save_frame_vars_update({save_frame, Vars, Info}, NewVars) ->
   {save_frame, NewVars, Info}.
save_frame_size({save_frame, Vars, Info}) -> length(Vars).
save_frame_info({save_frame, Vars, Info}) -> Info.
save_frame_info_update(Instr, Info) -> Instr#save_frame{info=Info}.

%
% restore_frame
%

mk_restore_frame(Vars) -> {restore_frame, Vars, []}.
restore_frame_vars({restore_frame, Vars, Info}) -> Vars.
%restore_frame_vars_update({restore_frame, Vars, Info}, NewVars) ->
%   {restore_frame, NewVars, Info}.
restore_frame_size({restore_frame, Vars, Info}) -> length(Vars).
restore_frame_info({restore_frame, Vars, Info}) -> Info.
restore_frame_info_update(Instr, Info) -> Instr#restore_frame{info=Info}.

%
% pop_frame
%

mk_pop_frame(Size) -> {pop_frame, Size, []}.
pop_frame_size({pop_frame, Size, Info}) -> Size.
pop_frame_info({pop_frame, Size, Info}) -> Info.
pop_frame_info_update(Instr, Info) -> Instr#pop_frame{info=Info}.

%
% restore_catch
%

mk_restore_catch(Vars) -> {restore_catch, Vars, []}.
restore_catch_vars({restore_catch, Vars, Info}) -> Vars.
%restore_catch_vars_update({restore_catch, Vars, Info}, NewVars) ->
%   {restore_catch, NewVars, Info}.
restore_catch_size({restore_catch, Vars, Info}) -> length(Vars).
restore_catch_info({restore_catch, Vars, Info}) -> Info.
restore_catch_info_update(Instr, Info) -> Instr#restore_catch{info=Info}.

%
% stackneed
%

mk_stackneed(Words) -> #stackneed{words=Words, info=[]}.
stackneed_words(S) -> S#stackneed.words.
stackneed_info(S) -> S#stackneed.info.
stackneed_info_update(S, Info) -> S#stackneed{info=Info}.


%
% gctest
%

mk_gctest(Words) -> #gctest{words=Words, info=[]}.
gctest_words(S) -> S#gctest.words.
gctest_info(S) -> S#gctest.info.
gctest_info_update(S, Info) -> S#gctest{info=Info}.


%
% comment
%

mk_comment(Text) -> {comment, Text, []}.
comment_text({comment, Text, Info}) -> Text.
comment_info({comment, Text, Info}) -> Text.
comment_info_update(Instr, Info) -> Instr#comment{info=Info}.
is_comment({comment,_,_}) -> true;
is_comment(_) -> false.

%
% The values
%

mk_reg(Name) -> {rtl_reg, Name}.
mk_new_reg() -> mk_reg(hipe_gensym:get_next_var(rtl)).
is_reg(I) -> case I of {rtl_reg, _} -> true; _ -> false end.
reg_name({rtl_reg, Name}) -> Name.

mk_var(Name) -> {rtl_var, Name}.
mk_new_var() -> mk_var(hipe_gensym:get_next_var(rtl)).
is_var(I) -> case I of {rtl_var, _} -> true; _ -> false end.
var_name({rtl_var, Name}) -> Name.

mk_fpreg(Name) -> {rtl_fpreg, Name}.
is_fpreg(I) -> case I of {rtl_fpreg, _} -> true; _ -> false end.
fpreg_name({rtl_fpreg, Name}) -> Name.

mk_imm(Value) -> {rtl_imm, Value}.
is_imm(I) -> case I of {rtl_imm, _} -> true; _ -> false end.
imm_value({rtl_imm, Value}) -> Value.


%
% The type of an instructions
%

type(RtlIns) ->
   element(1, RtlIns).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Utilities - no representation visible below this point
%

%
% A list of variables an instruction uses
%

uses(I) ->
   Uses = case type(I) of
	     move -> [move_src(I)];
	     multimove -> multimove_src(I);
	     alu -> [alu_src1(I), alu_src2(I)];
	     load -> [load_src(I), load_offset(I)];
	     store -> [store_dst(I), store_offset(I), store_src(I)];
	     branch -> [branch_src1(I), branch_src2(I)];
	     alub -> [alub_src1(I), alub_src2(I)];
	     switch -> [switch_src(I)];
	     call -> 
	      case call_type(I) of
		closure -> 
		  [call_fun(I)|call_args(I)];
		_ ->
		  call_args(I)
	      end;
	     enter -> 
	      case enter_type(I) of
		closure -> 
		      hipe_rtl_arch:add_ra_reg([enter_fun(I) | enter_args(I)]);
		_ ->
		      hipe_rtl_arch:add_ra_reg(enter_args(I))
	      end;
	     return -> hipe_rtl_arch:add_ra_reg(return_vars(I));
	     load_atom -> [];
	     load_word_index -> [];
	     goto_index -> [];
	     load_address -> [];
	     goto -> [];
	     fail_to -> [fail_to_reason(I)];
	     jsr -> 
	      case jsr_type(I) of
		closure -> 
		  [jsr_fun(I)|jsr_args(I)];
		_ ->
		  jsr_args(I)
	      end;
	     esr -> 
	      case esr_type(I) of
		closure -> 
		      hipe_rtl_arch:add_ra_reg([esr_fun(I) | esr_args(I)]);
		_ ->   
		      hipe_rtl_arch:add_ra_reg(esr_args(I))
	      end;
	     jmp -> [jmp_target(I), jmp_offset(I) | jmp_args(I)];
	     jmp_link ->
		[jmp_link_target(I), jmp_link_offset(I) | jmp_link_args(I)];
	     save_frame -> save_frame_vars(I);
	     restore_frame -> [];
	     restore_catch -> [];
	     pop_frame -> [];
	     stackneed -> [];
	     gctest -> [];
	     label -> [];
	     comment -> []
	  end,
   remove_immediates(Uses).


%
% A list of variables an instruction defines
%

defines(Ins) ->
   Defs = case type(Ins) of
	     move -> [move_dst(Ins)];
	     multimove -> multimove_dst(Ins);
	     alu -> [alu_dst(Ins)];
	     alub -> [alub_dst(Ins)];
	     load -> [load_dst(Ins)];
	     load_atom -> [load_atom_dst(Ins)];
	     load_word_index -> [load_word_index_dst(Ins)];
	     goto_index -> [];
	     load_address -> [load_address_dst(Ins)];
	     call -> call_dst(Ins);
	     enter -> [];
	     return -> [];
	     store -> [];
	     branch -> [];
	     switch -> [];
	     goto -> [];
	     fail_to -> [];
	     jsr -> [];
	     esr -> [];
	     jmp -> [];
	     jmp_link -> [jmp_link_link(Ins)];
	     save_frame -> [];
	     restore_frame -> restore_frame_vars(Ins);
	     restore_catch -> restore_catch_vars(Ins);
	     pop_frame -> [];
	     stackneed -> [];
	     gctest -> [];
	     label -> [];
	     comment -> []
	  end,
   remove_immediates(Defs).


%
% Remove immediates from a list
%

remove_immediates([]) ->
   [];
remove_immediates([I|Is]) ->
   case is_imm(I) of
      true -> remove_immediates(Is);
      false -> [I | remove_immediates(Is)]
   end.


%
% Substitution: replace occurrences of X by Y if {X,Y} is in Subst.
%

subst_uses(Subst, I) ->
   case type(I) of
      move -> 
	 move_src_update(I, subst1(Subst, move_src(I)));
      multimove -> 
	 multimove_src_update(I, subst1(Subst, multimove_src(I)));
      alu ->
	 I0 = alu_src1_update(I, subst1(Subst, alu_src1(I))),
	 alu_src2_update(I0, subst1(Subst, alu_src2(I)));
      alub ->
	 I0 = alub_src1_update(I, subst1(Subst, alub_src1(I))),
	 alub_src2_update(I0, subst1(Subst, alub_src2(I)));
      load ->
	 I0 = load_src_update(I, subst1(Subst, load_src(I))),
	 load_offset_update(I0, subst1(Subst, load_offset(I)));
      load_atom ->
	 I;
      load_word_index ->
	 I;
      goto_index ->
	 I;
      load_address ->
	 I;
%      load_tagged -> load_tagged_info(Ins);
      call ->
       case call_type(I) of
	 closure ->
	   I0 = call_fun_update(I, subst1(Subst, call_fun(I))),
	   call_args_update(I0, subst_list(Subst, call_args(I0)));
	 _ ->
	   call_args_update(I, subst_list(Subst, call_args(I)))
       end;
      enter ->
       case enter_type(I) of
	 closure -> 
	   I0 = enter_fun_update(I, subst1(Subst, enter_fun(I))),
	   enter_args_update(I0, subst_list(Subst, enter_args(I0)));
	 _ ->
	   enter_args_update(I, subst_list(Subst, enter_args(I)))
       end;
      return ->
	 return_vars_update(I, subst_list(Subst, return_vars(I)));
      store ->
	 I0 = store_src_update(I, subst1(Subst, store_src(I))),
	 I1 = store_dst_update(I0, subst1(Subst, store_dst(I))),
	 store_offset_update(I1, subst1(Subst, store_offset(I)));
      branch ->
	 I0 = branch_src1_update(I, subst1(Subst, branch_src1(I))),
	 branch_src2_update(I0, subst1(Subst, branch_src2(I)));
      switch ->
	 switch_src_update(I, subst1(Subst, switch_src(I)));
      goto ->
	 I;
      fail_to ->
        fail_to_reason_update(I,subst1(Subst, fail_to_reason(I)));
      jsr ->
        case jsr_type(I) of
	 closure ->
	   I0 = jsr_fun_update(I, subst1(Subst, jsr_fun(I))),
	   jsr_args_update(I0, subst_list(Subst, jsr_args(I0)));
	 _ ->
	   jsr_args_update(I, subst_list(Subst, jsr_args(I)))
        end;
      esr ->
        case esr_type(I) of
	 closure ->
	    esr_fun_update(I, subst1(Subst, esr_fun(I)));
	  _ ->
	    I % esr_args_update(I, subst_list(Subst, esr_args(I)));
	end;
      jmp ->
	 I0 = jmp_target_update(I, subst1(Subst, jmp_target(I))),
	 I1 = jmp_offset_update(I, subst1(Subst, jmp_offset(I)));
	 % jmp_args_update(I1, subst_list(Subst, jmp_args(I)));
      jmp_link ->
	 I0 = jmp_link_target_update(I, subst1(Subst, jmp_link_target(I))),
	 I1 = jmp_link_offset_update(I, subst1(Subst, jmp_link_offset(I)));
	 % jmp_link_args_update(I1, subst_list(Subst, jmp_link_args(I)));
      save_frame ->
	 save_frame_vars_update(I, subst_list(Subst, save_frame_vars(I)));
      restore_frame ->
	 I;
      restore_catch ->
	 I;
      pop_frame ->
	 I;
      stackneed ->
	 I;
      gctest ->
	 I;
      label ->
	 I;
      comment ->
	 I
   end.


subst_list(S,Xs) ->
   [subst1(S,X) || X <- Xs].

subst1([],X) -> X;
subst1([{X,Y}|Xs],X) -> Y;
subst1([_|Xs],X) -> subst1(Xs,X).



%
% Return the info-field of an instruction
%

info(Ins) ->
   case type(Ins) of
      move -> move_info(Ins);
      multimove -> multimove_info(Ins);
      alu -> alu_info(Ins);
      load -> load_info(Ins);
      load_atom -> load_atom_info(Ins);
      load_word_index -> load_word_index_info(Ins);
      goto_index -> goto_index_info(Ins);
      load_address -> load_address_info(Ins);
      load_tagged -> load_tagged_info(Ins);
      call -> call_info(Ins);
      enter -> enter_info(Ins);
      return -> return_info(Ins);
      store -> store_info(Ins);
      branch -> branch_info(Ins);
      switch -> switch_info(Ins);
      alub -> alub_info(Ins);
      goto -> goto_info(Ins);
      fail_to -> fail_to_info(Ins);
      jsr -> jsr_info(Ins);
      esr -> esr_info(Ins);
      jmp -> jmp_info(Ins);
      jmp_link -> jmp_link_info(Ins);
      save_frame -> save_frame_info(Ins);
      restore_frame -> restore_frame_info(Ins);
      restore_catch -> restore_catch_info(Ins);
      pop_frame -> pop_frame_info(Ins);
      stackneed -> stackneed_info(Ins);
      label -> label_info(Ins);
      comment -> comment_info(Ins)
   end.


%
% Add info to an istruction
%

info_add(I, Info) ->
   OldInfo = info(I),
   case type(I) of
      stackneed -> stackneed_info_update(I, [Info|OldInfo]);
      label -> label_info_update(I, [Info|OldInfo]);
      call -> call_info_update(I, [Info|OldInfo]);
      jsr -> jsr_info_update(I, [Info|OldInfo]);
      move -> move_info_update(I, [Info|OldInfo]);
      multimove -> multimove_info_update(I, [Info|OldInfo]);
      alu -> alu_info_update(I, [Info|OldInfo]);
      load -> load_info_update(I, [Info|OldInfo]);
      load_atom -> load_atom_info_update(I, [Info|OldInfo]);
      load_word_index -> load_word_index_info_update(I, [Info|OldInfo]);
      goto_index -> goto_index_info_update(I, [Info|OldInfo]);
      load_address -> load_address_info_update(I, [Info|OldInfo]);
      load_tagged -> load_tagged_info_update(I, [Info|OldInfo]);
      enter -> enter_info_update(I, [Info|OldInfo]);
      return -> return_info_update(I, [Info|OldInfo]);
      store -> store_info_update(I, [Info|OldInfo]);
      branch -> branch_info_update(I, [Info|OldInfo]);
      switch -> switch_info_update(I, [Info|OldInfo]);
      alub -> alub_info_update(I, [Info|OldInfo]);
      goto -> goto_info_update(I, [Info|OldInfo]);
      fail_to -> fail_to_info_update(I, [Info|OldInfo]);
      esr -> esr_info_update(I, [Info|OldInfo]);
      jmp -> jmp_info_update(I, [Info|OldInfo]);
      jmp_link -> jmp_link_info_update(I, [Info|OldInfo]);
      save_frame -> save_frame_info_update(I, [Info|OldInfo]);
      restore_frame -> restore_frame_info_update(I, [Info|OldInfo]);
      restore_catch -> restore_catch_info_update(I, [Info|OldInfo]);
      pop_frame -> pop_frame_info_update(I, [Info|OldInfo]);
      comment -> comment_info_update(I, [Info|OldInfo])   

   end.


info_update(I, Info) ->
   case type(I) of
      stackneed -> stackneed_info_update(I, Info);
      label -> label_info_update(I, Info);
      call -> call_info_update(I, Info);
      jsr -> jsr_info_update(I, Info);
      move -> move_info_update(I, Info);
      multimove -> multimove_info_update(I, Info);
      alu -> alu_info_update(I, Info);
      load -> load_info_update(I, Info);
      load_atom -> load_atom_info_update(I, Info);
      load_word_index -> load_word_index_info_update(I, Info);
      goto_index -> goto_index_info_update(I, Info);
      load_address -> load_address_info_update(I, Info);
      load_tagged -> load_tagged_info_update(I, Info);
      enter -> enter_info_update(I, Info);
      return -> return_info_update(I, Info);
      store -> store_info_update(I, Info);
      branch -> branch_info_update(I, Info);
      switch -> switch_info_update(I, Info);
      alub -> alub_info_update(I, Info);
      goto -> goto_info_update(I, Info);
      fail_to -> fail_to_info_update(I, Info);
      esr -> esr_info_update(I, Info);
      jmp -> jmp_info_update(I, Info);
      jmp_link -> jmp_link_info_update(I, Info);
      save_frame -> save_frame_info_update(I, Info);
      restore_frame -> restore_frame_info_update(I, Info);
      restore_catch -> restore_catch_info_update(I, Info);
      pop_frame -> pop_frame_info_update(I, Info);
      comment -> comment_info_update(I, Info)   
   end.


%
% True if an instructions is pure and can be deleted.
%

is_pure(I) ->
   case type(I) of
      store -> false;
      branch -> false;
      switch -> false; %% Maybe this is pure...
      alub -> false;
      jsr -> false;
      esr -> false;
      call -> false;
      enter -> false;
      goto -> false;
      goto_index -> false;  % ???
      fail_to -> false;
      return -> false;
      jmp -> false;
      jmp_link -> false;
      stackneed -> false;
      gctest -> false;
      comment -> false;
      save_frame -> false;
      restore_frame -> false;
      restore_catch -> false;
      pop_frame -> false;
      _ -> true
   end.


%
% True if argument is an alu-operator
%

is_alu_op(add) -> true;
is_alu_op(sub) -> true;
is_alu_op('or') -> true;
is_alu_op('and') -> true;
is_alu_op('xor') -> true;
is_alu_op(andnot) -> true;
is_alu_op(sll) -> true;
is_alu_op(srl) -> true;
is_alu_op(sra) -> true;
is_alu_op(_) -> false.


%
% True if argument is an relational operator
%

% is_rel_op(eq) -> true;
% is_rel_op(ne) -> true;
% is_rel_op(gt) -> true;
% is_rel_op(gtu) -> true;
% is_rel_op(ge) -> true;
% is_rel_op(geu) -> true;
% is_rel_op(lt) -> true;
% is_rel_op(ltu) -> true;
% is_rel_op(le) -> true;
% is_rel_op(leu) -> true;
% is_rel_op(overflow) -> true;
% is_rel_op(not_overflow) -> true;
% is_rel_op(_) -> false.


%
% Removes empty basic blocks from the code
%

remove_empty_bbs(Rtl) ->
  Code = rtl_code(Rtl),
  LabelPairs = find_empty_bbs(Code),
  NewCode = update_labels(Code, LabelPairs),
  NewData = hipe_consttab:update_referred_labels(
	      rtl_data(Rtl),strip_lp(LabelPairs,[])),
  mk_rtl(rtl_fun(Rtl), rtl_params(Rtl), 
	 NewCode, 
	 NewData,
	 rtl_var_range(Rtl), 
	 rtl_label_range(Rtl)).

strip_lp([{L1,L2}|Rest], Acc) ->
  strip_lp(Rest,[{label_name(L1),label_name(L2)}|Acc]);
strip_lp([],Acc) -> Acc.

find_empty_bbs([_]) ->
   [];
find_empty_bbs([I1, I2|Is]) ->
   case {is_label(I1), is_label(I2)} of
      {true, true} ->
	 [{I1, I2} | find_empty_bbs([I2|Is])];
      _ ->
	 find_empty_bbs([I2|Is])
   end.


update_labels([], Labels) ->
   [];
update_labels([I|Is], Labels) ->
   case type(I) of
      label ->
	 Rem = lists:any(fun({F,T}) when I==F-> true; (_)-> false end, Labels),
	 if Rem =:= true ->
	       Info = label_info(I),
	       [label_info_add(hd(Is), Info) | update_labels(tl(Is), Labels)];
	    true ->
	       [I | update_labels(Is, Labels)]
	 end;
      branch ->
	 I1 = case find_label(branch_true_label(I), Labels) of
		{From, To} ->
		  branch_true_label_update(I, label_name(To));
		not_found ->
		  I
	      end,
	 NewI = case find_label(branch_false_label(I), Labels) of
		    {From1, To1} ->
			branch_false_label_update(I1, label_name(To1));
		    not_found ->
			I1
		end,
	 [NewI | update_labels(Is, Labels)];
      switch -> 
       NewLbls = 
	 lists:map(fun(Lbl) ->
		       case find_label(Lbl, Labels) of
			 {From, To} -> To;
			 not_found -> Lbl
		       end
		   end, switch_labels(I)),
       [switch_labels_update(I, NewLbls) |
	update_labels(Is, Labels)];
      alub ->
	 I1 = case find_label(alub_true_label(I), Labels) of
		  {From, To} ->
		      alub_true_label_update(I, label_name(To));
		  not_found ->
		      I
	      end,
	 NewI = case find_label(alub_false_label(I), Labels) of
		    {From1, To1} ->
			alub_false_label_update(I1, label_name(To1));
		    not_found ->
			I1
		end,
	 [NewI | update_labels(Is, Labels)];
      goto ->
	 NewI = case find_label(goto_label(I), Labels) of
		    {From, To} ->
			goto_label_update(I, label_name(To));
		    not_found ->
			I
		end,
	 [NewI | update_labels(Is, Labels)];
       fail_to ->
	 NewI = case find_label(fail_to_label(I), Labels) of
		    {From, To} ->
			fail_to_label_update(I, label_name(To));
		    not_found ->
			I
		end,
	 [NewI | update_labels(Is, Labels)];
      call ->
       I0 = case find_label(call_continuation(I), Labels) of
	      {From, To} ->
		call_continuation_update(I, label_name(To));
	      not_found ->
		I
	    end,
       I1 = case find_label(call_fail(I0), Labels) of
	      {From1, To1} ->
		call_fail_update(I0, label_name(To1));
	      not_found ->
		I0
	    end,
       [I1 | update_labels(Is, Labels)];

      jsr ->
       I0 = case find_label(jsr_continuation(I), Labels) of
	      {From, To} ->
		jsr_continuation_update(I, label_name(To));
	      not_found ->
		I
	    end,
       I1 = case find_label(jsr_fail(I0), Labels) of
	      {From1, To1} ->
		jsr_fail_update(I0, label_name(To1));
	      not_found ->
		I0
	    end,
       [I1 | update_labels(Is, Labels)];
       jmp_link ->
       I0 = case find_label(jmp_link_continuation(I), Labels) of
	      {From, To} ->
		jmp_link_continuation_update(I, label_name(To));
	      not_found ->
		I
	    end,
       I1 = case find_label(jmp_link_fail(I0), Labels) of
	      {From1, To1} ->
		jmp_link_fail_update(I0, label_name(To1));
	      not_found ->
		I0
	    end,
       [I1 | update_labels(Is, Labels)];
      _ ->
	 [I | update_labels(Is, Labels)]
   end.


find_label(LabelName, []) ->
   not_found;
find_label(LabelName, [{From, To}|LabelPairs]) ->
   FromName = label_name(From),
   if FromName =:= LabelName ->
	 {From, To};
      true ->
	 find_label(LabelName, LabelPairs)
   end.



redirect_jmp(Jmp, ToOld, ToNew) ->
   case type(Jmp) of
      branch ->
	 case branch_true_label(Jmp) of
	    ToOld ->
	       branch_true_label_update(Jmp, ToNew);
	    _ ->
	       case branch_false_label(Jmp) of
		  ToOld ->
		     branch_false_label_update(Jmp, ToNew);
		  _ ->
		     Jmp
	       end
	 end;

     switch ->
       NewLbls = 
	 lists:map(fun(Lbl) when Lbl =:= ToOld -> ToNew;
		      (Lbl) -> (Lbl)
		   end, switch_labels(Jmp)),
       switch_labels_update(Jmp, NewLbls);

      alub ->
	 case alub_true_label(Jmp) of
	    ToOld ->
	       alub_true_label_update(Jmp, ToNew);
	    _ ->
	       case alub_false_label(Jmp) of
		  ToOld ->
		     alub_false_label_update(Jmp, ToNew);
		  _ ->
		     Jmp
	       end
	 end;
      goto ->
	 case goto_label(Jmp) of
	    ToOld ->
	       goto_label_update(Jmp, ToNew);
	    _ ->
	       Jmp
	 end;
      fail_to ->
	 case fail_to_label(Jmp) of
	    ToOld ->
	       fail_to_label_update(Jmp, ToNew);
	    _ ->
	       Jmp
	 end;
      jsr ->
	 case jsr_continuation(Jmp) of
	    ToOld ->
	       jsr_continuation_update(Jmp, ToNew);
	    _ ->
	       case jsr_fail(Jmp) of
		  ToOld ->
		     jsr_fail_update(Jmp, ToNew);
		  _ ->
		     Jmp
	       end
	 end;

     jmp_link ->
	 case jmp_link_continuation(Jmp) of
	    ToOld ->
	       jmp_link_continuation_update(Jmp, ToNew);
	    _ ->
	       case jmp_link_fail(Jmp) of
		  ToOld ->
		     jmp_link_fail_update(Jmp, ToNew);
		  _ ->
		     Jmp
	       end
	 end;
     call ->
	 case call_continuation(Jmp) of
	    ToOld ->
	       call_continuation_update(Jmp, ToNew);
	    _ ->
	       case call_fail(Jmp) of
		  ToOld ->
		     call_fail_update(Jmp, ToNew);
		  _ ->
		     Jmp
	       end
	 end;


      _ ->
	 Jmp
   end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Pretty printer
%
% - added pp_exit/1, which exits when RTL can't be printed
% - modified pp/1 to handle this and sub-functions to exit
%   appropriately. (Similarly to Sparc.)

pp(Rtl) ->
   pp(standard_io, Rtl).

pp(Dev, Rtl) ->
   io:format(Dev, "~w(", [rtl_fun(Rtl)]),
   pp_args(Dev, rtl_params(Rtl)),
   io:format(Dev, ") ->~n", []),
   io:format(Dev, ";; Info: ~w\n", [rtl_info(Rtl)]),
   io:format(Dev, ".DataSegment\n", []),
   hipe_data_pp:pp(Dev, rtl_data(Rtl), rtl, ""), 
   io:format(Dev, ".CodeSegment\n", []),
   pp_instrs(Dev, rtl_code(Rtl)).

pp_instrs(Dev, []) ->
   clogs_done;
pp_instrs(Dev, [I|Is]) ->
   pp_instr(Dev, I),
   pp_instrs(Dev, Is).


pp_instr(Dev, I) ->
   case type(I) of
      move ->
	 io:format(Dev, "    ", []),
	 pp_arg(Dev, move_dst(I)),
	 io:format(Dev, " <- ", []),
	 pp_arg(Dev, move_src(I)),
	 io:format(Dev, "~n", []);
      multimove ->
	 io:format(Dev, "    ", []),
	 pp_args(Dev, multimove_dst(I)),
	 io:format(Dev, " <= ", []),
	 pp_args(Dev, multimove_src(I)),
	 io:format(Dev, "~n", []);
      alu ->
	 io:format(Dev, "    ", []),
	 pp_arg(Dev, alu_dst(I)),
	 io:format(Dev, " <- ", []),
	 pp_arg(Dev, alu_src1(I)),
	 io:format(Dev, " ~w ", [alu_op(I)]),
	 pp_arg(Dev, alu_src2(I)),
	 io:format(Dev, "~n", []);
%      fp -> ;
%      load_tagged -> ;
      load ->
	 io:format(Dev, "    ", []),
	 pp_arg(Dev, load_dst(I)),
	 io:format(Dev, " <- [", []),
	 pp_arg(Dev, load_src(I)),
	 io:format(Dev, "+", []),
	 pp_arg(Dev, load_offset(I)),
	 io:format(Dev, "]~n", []);
      load_atom ->
	 io:format(Dev, "    ", []),
	 pp_arg(Dev, load_atom_dst(I)),
	 io:format(Dev, " <- atom_no(\'~s\')~n", [load_atom_atom(I)]);
      load_word_index ->
	 io:format(Dev, "    ", []),
	 pp_arg(Dev, load_word_index_dst(I)),
	 io:format(Dev, " <- word_index_no( DL~p[~p] )~n", [load_word_index_block(I),load_word_index_index(I)]);
      goto_index ->
	 io:format(Dev, "    ", []),
	 io:format(Dev, "goto_index DL~p[~p]~n", [goto_index_block(I), goto_index_index(I)]);
      load_address ->
	 io:format(Dev, "    ", []),
	 pp_arg(Dev, load_address_dst(I)),
         case load_address_type(I) of
	   constant ->
	     io:format(Dev, " <- DL~p~n", [load_address_address(I)]);
	   closure ->
	     io:format(Dev, " <- L~p [closure]~n", [load_address_address(I)]);
	   Type ->
	     io:format(Dev, " <- L~p [Type]~n", [load_address_address(I)])
	 end;
      store ->
	 io:format(Dev, "    [", []),
	 pp_arg(Dev, store_dst(I)),
	 io:format(Dev, "+", []),
	 pp_arg(Dev, store_offset(I)),
	 io:format(Dev, "] <- ", []),
	 pp_arg(Dev, store_src(I)),
	 io:format(Dev, "~n", []);
      label ->
       case info(I) of
	 [] -> 
	   io:format(Dev, "L~w:~n", [label_name(I)]);
	 Info ->
	   io:format(Dev, "L~w: ~w~n", [label_name(I), Info])
       end;
      branch ->
	 io:format(Dev, "    if (", []),
	 pp_arg(Dev, branch_src1(I)),
	 io:format(Dev, " ~w ", [branch_cond(I)]),
	 pp_arg(Dev, branch_src2(I)),
	 io:format(Dev, ") then L~w (~.2f) else L~w~n", 
		   [branch_true_label(I), branch_pred(I), 
		    branch_false_label(I)]);
      switch ->
	 io:format(Dev, "    switch (", []),
	 pp_arg(Dev, switch_src(I)),
	 io:format(Dev, ") <", []),
         pp_switch_labels(Dev, switch_labels(I)),
	 io:format(Dev, ">\n", []);

      alub ->
	 io:format(Dev, "    ", []),
	 pp_arg(Dev, alub_dst(I)),
	 io:format(Dev, " <- ", []),
	 pp_arg(Dev, alub_src1(I)),
	 io:format(Dev, " ~w ", [alub_op(I)]),
	 pp_arg(Dev, alub_src2(I)),
	 io:format(Dev, " if",[]),
	 io:format(Dev, " ~w ", [alub_cond(I)]),
	 io:format(Dev, "then L~w (~.2f) else L~w~n", 
		   [alub_true_label(I), alub_pred(I), 
		    alub_false_label(I)]);
      goto ->
	 io:format(Dev, "    goto L~w~n", [goto_label(I)]);
      fail_to ->
	 io:format(Dev, "    fail_to L~w(", [fail_to_label(I)]),
         pp_arg(Dev, fail_to_reason(I)),
         io:format(Dev, ")~n",[]);
      jsr ->
       case jsr_type(I) of
	 closure ->
	   io:format(Dev, "    jsr (",[]),
	   pp_arg(Dev, jsr_fun(I)),
	   io:format(Dev, ")[~w] (",[jsr_type(I)]);
	 _ ->
	   case jsr_fun(I) of
	     {M, F, A} ->
	       io:format(Dev, 
			 "    jsr ~w:~w/~w [~w] (", [M, F, A, jsr_type(I)]);
	    {N, A} ->
	       io:format(Dev, "    jsr ~w/~w [~w] (", [N, A, jsr_type(I)]);
	    F ->
	       io:format(Dev, "    jsr ~w [~w] (", [F, jsr_type(I)])
	   end
       end,
       pp_args(Dev, jsr_args(I)),
       io:format(Dev, ") then L~w", [jsr_continuation(I)]),

       
         case jsr_fail(I) of
	   [] -> true;
	   L ->
	     io:format(Dev, " fail_to L~w", [L])
	 end,
         io:format(Dev, "~n", []);
      esr ->
        case esr_type(I) of
	 closure ->
	   io:format(Dev, "    esr (",[]),
	   pp_arg(Dev, esr_fun(I)),
	   io:format(Dev, ")[~w] (",[esr_type(I)]);
	  _ ->
	    case esr_fun(I) of
	      {M, F, A} ->
		io:format(Dev, 
			  "    esr ~w:~w/~w [~w] (", [M, F, A, esr_type(I)]);
	      {N, A} ->
		io:format(Dev, "    esr ~w/~w [~w] (", [N, A, esr_type(I)]);
	      F ->
		io:format(Dev, "    esr ~w [~w] (", [F, esr_type(I)])
	    end
	 end,
	 pp_args(Dev, esr_args(I)),
	 io:format(Dev, ")~n", []);
      jmp ->
	 io:format(Dev, "    jmp ", []),
	 pp_arg(Dev, jmp_target(I)),
	 io:format(Dev, "+", []),
	 pp_arg(Dev, jmp_offset(I)),
	 io:format(Dev, " (", []),
	 pp_args(Dev, jmp_args(I)),
	 io:format(Dev, ")~n", []);

      jmp_link -> 
	 io:format(Dev, "     jmp_link [", []),
	 pp_args(Dev, jmp_link_link(I)),
	 io:format(Dev, "] ", []),
	 pp_arg(Dev, jmp_target(I)),
	 io:format(Dev, "+", []),
	 pp_arg(Dev, jmp_offset(I)),
	 io:format(Dev, " (", []),
	 pp_args(Dev, jmp_args(I)),
	 io:format(Dev, ")", []),
         io:format(Dev, " then L~w", [jmp_link_continuation(I)]),
         case jmp_link_fail(I) of
	   [] -> true;
	   L ->
	     io:format(Dev, " fail_to L~w", [L])
	 end,
         io:format(Dev, "~n", []);
      call ->
       io:format(Dev, "    ", []),
       pp_args(Dev, call_dst(I)),
       io:format(Dev, " <- ", []),
       case call_type(I) of
	 closure ->
	   io:format(Dev, "(",[]),
	   pp_arg(Dev, call_fun(I)),
	   io:format(Dev, ")(",[]);
	 _ ->
	   case call_fun(I) of
	     {M, F, A} ->
	       io:format(Dev, "~w:~w(", [M, F]);
	    {N, A} ->
	       io:format(Dev, "~w(", [N]);
	    A ->
	       io:format(Dev, "~w(", [A])
	   end
       end,
       pp_args(Dev, call_args(I)),
       io:format(Dev, ") [~w]", [call_type(I)]),
       io:format(Dev, " then L~w", [call_continuation(I)]),
         case call_fail(I) of
	   [] -> true;
	   L ->
	     io:format(Dev, " fail_to L~w", [L])
	 end,
         io:format(Dev, "~n", []);
      enter ->
	 io:format(Dev, "    ", []),
       case enter_type(I) of
	 closure ->
	   io:format(Dev, "(",[]),
	   pp_arg(Dev, enter_fun(I)),
	   io:format(Dev, ")(",[]);
	 _ ->
	   case enter_fun(I) of
	     {M, F, A} ->
	       io:format(Dev, "~w:~w(", [M, F]);
	     {N, A} ->
	       io:format(Dev, "~w(", [N]);
	     A ->
	       io:format(Dev, "~w(", [A])
	   end
       end,
	 pp_args(Dev, enter_args(I)),
	 io:format(Dev, ") [~w]~n", [enter_type(I)]);
      return ->
	 io:format(Dev, "    return(", []),
	 pp_args(Dev, return_vars(I)),
	 io:format(Dev, ")~n", []);
      comment ->
	 io:format(Dev, "    ;; ~p~n", [comment_text(I)]);
      save_frame ->
	 io:format(Dev, "    save_frame(", []),
	 pp_args(Dev, save_frame_vars(I)),
	 io:format(Dev, ")~n", []);
      restore_frame ->
	 io:format(Dev, "    restore_frame(", []),
	 pp_args(Dev, restore_frame_vars(I)),
	 io:format(Dev, ")~n", []);
      restore_catch ->
	 io:format(Dev, "    ", []),
	 pp_args(Dev,  restore_catch_vars(I)),
	 io:format(Dev, " <- restore_catch()\n", []);

      pop_frame ->
	 io:format(Dev, "    pop_frame(~w)~n", [pop_frame_size(I)]);
      stackneed ->
	 io:format(Dev, "    stackneed(~w)~n", [stackneed_words(I)]);
      gctest ->
	 io:format(Dev, "    gctest(~w)~n", [gctest_words(I)]);
      Other ->
	 exit({rtl, {"unkown instruction", Other}})
   end.




pp_args(Dev, []) ->
   ok;
pp_args(Dev, [A]) ->
   pp_arg(Dev, A);
pp_args(Dev, [A|As]) ->
   pp_arg(Dev, A),
   io:format(Dev, ", ", []),
   pp_args(Dev, As).

pp_hard_reg(Dev, N) ->
    io:format(Dev, "~s", [hipe_rtl_arch:reg_name(N)]).

pp_reg(Dev, A) ->
    N = reg_name(A),	% RTL's reg_name, not the back-end's !
    case hipe_rtl_arch:is_precoloured(N) of
	true ->
	    pp_hard_reg(Dev, N);
	false ->
	    io:format(Dev, "r~w", [N])
    end.

pp_var(Dev, A) ->
    N = var_name(A),
    case hipe_rtl_arch:is_precoloured(N) of
	true ->
	    pp_hard_reg(Dev, N);
	false ->
	    io:format(Dev, "v~w", [N])
    end.

pp_arg(Dev, A) ->
   case is_var(A) of
      true -> 
	 pp_var(Dev, A);
      false ->
	 case is_reg(A) of
	    true ->
	       pp_reg(Dev, A);
	    false ->
	       case is_imm(A) of
		  true ->
		     io:format(Dev, "~w", [imm_value(A)]);
		  false ->
		     case is_fpreg(A) of
			true ->
			   io:format(Dev, "f~w", [fpreg_name(A)]);
			false ->
			   exit({bad_rtl_arg,A})
		     end
	       end
	 end
   end.

pp_switch_labels(Dev,Lbls) -> 
  pp_switch_labels(Dev,Lbls,1).

pp_switch_labels(Dev, [L], Pos) -> 
  io:format(Dev, "L~w", [L]);
pp_switch_labels(Dev, [L|Ls], Pos) -> 
  io:format(Dev, "L~w, ", [L]),
  NewPos = 
    case Pos of
      5 -> io:format(Dev, "\n              ",[]),
	   0;
      N -> N + 1
    end,
  pp_switch_labels(Dev, Ls, NewPos);
pp_switch_labels(Dev, [], _) -> ok.
