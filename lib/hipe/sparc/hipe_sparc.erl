%% -*- erlang-indent-level: 2 -*-
%% =====================================================================
%% Copyright (c) 1997 by the HiPE group.  All Rights Reserved 
%% =====================================================================
%%  Filename : 	hipe_sparc.erl
%%  Group    :  Optimizer
%%  Purpose  :  Primitives for handling sparc three address instructions
%%  Notes    : 
%% =====================================================================
%% Exported functions (short description):
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc).

-export([mk_sparc/5,
	 sparc_fun/1,
	 sparc_code/1,
	 sparc_code_update/2,
	 sparc_data/1,
	 sparc_data_update/2,
	 sparc_var_range/1,
	 sparc_var_range_update/2,
	 sparc_label_range/1,
	 sparc_label_range_update/2,
	 sparc_size/1]).

-export([store_create/5,
	 store_dest/1,
	 store_dest_update/2,
	 store_off/1,
	 store_off_update/2,
	 store_src/1,
	 store_src_update/2,
	 store_type/1,
	 store_type_update/2]).

-export([jmp_link_create/5,
	 jmp_link_off/1,
	 jmp_link_off_update/2,
	 jmp_link_target/1,
	 jmp_link_target_update/2,
	 jmp_link_args/1,
	 jmp_link_args_update/2,
	 jmp_link_link/1,
	 jmp_link_link_update/2,
	 
	 jmp_create/4,
	 jmp_create/5,
	 jmp_off/1,
	 jmp_off_update/2,
	 jmp_target/1,
	 jmp_target_update/2,
	 jmp_args/1,
	 jmp_args_update/2,
	 jmp_destinations/1,
	 jmp_destinations_update/2,
	 
	 call_link_create/7,
	 call_link_target/1,
	 call_link_target_update/2,
	 call_link_link/1,
	 call_link_link_update/2,
	 call_link_args/1,
	 call_link_args_update/2,
	 call_link_continuation/1,
	 call_link_continuation_update/2,
	 call_link_fail/1,
	 call_link_fail_update/2,
	 call_link_type/1,

	 label_create/2,
	 label_create_new/0,
	 label_name/1,
	 label_name_update/2,

	 load_atom_create/3,
	 load_atom_dest/1,
	 load_atom_dest_update/2,
	 load_atom_atom/1,
	 
	 load_word_index_create/4,
	 load_word_index_dest/1,
	 load_word_index_dest_update/2,
	 load_word_index_index/1,
	 load_word_index_block/1,

	 load_address_create/4,
	 load_address_dest/1,
	 load_address_address/1,
	 load_address_type/1,
	 load_address_type_update/2,
	 load_address_dest_update/2,

	 load_create/5,
	 load_dest/1,
	 load_dest_update/2,
	 load_off/1,
	 load_off_update/2,
	 load_src/1,
	 load_src_update/2,
	 load_type/1,
	 load_type_update/2,

	 move_create/3,
	 move_dest/1,
	 move_dest_update/2,
	 move_src/1,
	 move_src_update/2,

	 multimove_create/3,
	 multimove_dest/1,
	 multimove_dest_update/2,
	 multimove_src/1,
	 multimove_src_update/2,

	 nop_create/1,
	 
	 sethi_const/1,
	 sethi_const_update/2,
	 sethi_create/3,
	 sethi_dest/1,
	 sethi_dest_update/2]).

-export([align_alignment/1,
	 align_alignment_update/2,
	 align_create/2,

	 alu_cc_create/5,
	 alu_cc_dest/1,
	 alu_cc_dest_update/2, 
	 alu_cc_operator/1,
	 alu_cc_operator_update/2,
	 alu_cc_src1/1, 
	 alu_cc_src1_update/2,
	 alu_cc_src2/1,
	 alu_cc_src2_update/2,
	 
	 alu_create/5,
	 alu_dest/1,
	 alu_dest_update/2,
	 alu_operator/1,
	 alu_operator_update/2,
	 alu_src1/1,
	 alu_src1_update/2,
	 alu_src2/1,
	 alu_src2_update/2, 

	 b_create/6,
	 b_annul/1,
	 b_annul_update/2,
	 b_cond/1,
	 b_cond_update/2,
	 b_label/1,
	 b_label_update/2,
	 b_true_label/1,
	 b_true_label_update/2,
	 b_false_label/1,
	 b_false_label_update/2,
	 b_pred/1,
	 b_pred_update/2,
	 b_taken/1,
	 
	 br_create/7,
	 br_annul/1,
	 br_annul_update/2,
	 br_label/1,
	 br_label_update/2,
	 br_true_label/1,
	 br_true_label_update/2,
	 br_false_label/1,
	 br_false_label_update/2,
	 br_pred/1,
	 br_pred_update/2,
	 br_taken/1,
	 br_reg/1,
	 br_reg_update/2,
	 br_regcond/1,
	 br_regcond_update/2,

	 goto_create/2,
	 goto_label/1,
	 goto_label_update/2,
	 is_goto/1,

	 cmov_cc_cond/1,
	 cmov_cc_cond_update/2,
	 cmov_cc_create/4,
	 cmov_cc_dest/1,
	 cmov_cc_dest_update/2,
	 cmov_cc_src/1,
	 cmov_cc_src_update/2,

	 cmov_r_create/5,
	 cmov_r_dest/1,
	 cmov_r_dest_update/2,
	 cmov_r_reg/1,
	 cmov_r_reg_update/2,
	 cmov_r_regcond/1,
	 cmov_r_regcond_update/2,
	 cmov_r_src/1,
	 cmov_r_src_update/2,
	 comment_create/2,

	 comment_text/1,
	 comment_text_update/2,

	 imm16/1,
	 imm19/1,
	 info/1,
	 info_update/2,
	 is_align/1,
	 is_alu/1,
	 is_alu_cc/1,
	 is_any_alu/1,
	 is_any_branch/1,
	 is_any_cmov/1,
	 is_any_memop/1,
	 is_b/1,
	 is_br/1,
	 is_call_link/1,
	 is_cmov_cc/1,
	 is_cmov_r/1,
	 is_comment/1,
	 is_jmp/1,
	 is_jmp_link/1,
	 is_label/1,
	 is_load/1,
	 is_move/1,
	 is_multimove/1,
	 is_nop/1,
	 is_sethi/1,
	 is_store/1]).

-export([redirect_jmp/3,
	 cc_negate/1,
	 uses/1,
	 all_uses/1,
	 imm_uses/1,
	 defines/1,
	 def_use/1,
	 subst/2,
	 subst_uses/2,
	 subst_defines/2,
	 verify/2,
	 pp/1,
	 pp/2,
	 pp_instr/1,
	 pp_instrs/3]).

-export([mk_reg/1,
	 mk_new_reg/0,
	 is_reg/1,
	 reg_nr/1,
	 mk_fpreg/1,
	 is_fpreg/1,
	 fpreg_nr/1,
	 mk_imm/1,
	 is_imm/1,
	 imm_value/1]).
-export([type/1]).
-export([highest_reg/1]).


-include("hipe_sparc.hrl").

-define(DO_ASSERT,true).
-include("../main/hipe.hrl").



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Constructors
%

mk_sparc(Fun, Code, Data, VarRange, LabelRange) ->
  #sparc{'fun'=Fun, code=Code, data=Data, var_range=VarRange, label_range=LabelRange}.
sparc_fun(Sparc) -> Sparc#sparc.'fun'.
sparc_code(Sparc) -> Sparc#sparc.code.
sparc_code_update(Sparc, NewCode) -> Sparc#sparc{code=NewCode}.
sparc_data(Sparc) -> Sparc#sparc.data.
sparc_data_update(Sparc, NewData) -> Sparc#sparc{data=NewData}.
sparc_var_range(Sparc) -> Sparc#sparc.var_range.
sparc_var_range_update(Sparc, NewRange) -> 
  NewSparc = Sparc#sparc{var_range=NewRange},
  ?VERBOSE_ASSERT(check_var_range(NewSparc)),
  NewSparc.

sparc_label_range(Sparc) -> Sparc#sparc.label_range.
sparc_label_range_update(Sparc, NewRange) -> Sparc#sparc{label_range=NewRange}.
sparc_size(Sparc) ->
  RealIns = fun(I) -> not(is_label(I) or is_comment(I)) end,
  length(lists:filter(RealIns, sparc_code(Sparc))).

info_update(Insn, NewInfo) ->
   if
      record(Insn,label) -> Insn#label{info=NewInfo};
      record(Insn,nop) -> Insn#nop{info=NewInfo};
      record(Insn,align) -> Insn#align{info=NewInfo};
      record(Insn,comment) -> Insn#comment{info=NewInfo};
      record(Insn,move) -> Insn#move{info=NewInfo};
      record(Insn,multimove) -> Insn#multimove{info=NewInfo};
      record(Insn,cmov_cc) -> Insn#cmov_cc{info=NewInfo};
      record(Insn,cmov_r) -> Insn#cmov_r{info=NewInfo};
      record(Insn,alu) -> Insn#alu{info=NewInfo};
      record(Insn,alu_cc) -> Insn#alu_cc{info=NewInfo};
      record(Insn,sethi) -> Insn#sethi{info=NewInfo};
      record(Insn,load) -> Insn#load{info=NewInfo};
      record(Insn,store) -> Insn#store{info=NewInfo};
      record(Insn,b) -> Insn#b{info=NewInfo};
      record(Insn,br) -> Insn#br{info=NewInfo};
      record(Insn,jmp_link) -> Insn#jmp_link{info=NewInfo};
      record(Insn,jmp) -> Insn#jmp{info=NewInfo};
      record(Insn,call_link) -> Insn#call_link{info=NewInfo};
      record(Insn,load_atom) -> Insn#load_atom{info=NewInfo};
      record(Insn,load_word_index) -> Insn#load_word_index{info=NewInfo};
      record(Insn,load_address) -> Insn#load_address{info=NewInfo}
   end.


%% Load atom %%
load_atom_create(Dest,Atom,Info) -> #load_atom{dst=Dest,atom=Atom,info=Info}.
load_atom_dest(LoadAtom) -> LoadAtom#load_atom.dst.
load_atom_atom(LoadAtom) -> LoadAtom#load_atom.atom.
load_atom_dest_update(LA, NewDest) -> LA#load_atom{dst=NewDest}.

load_word_index_create(Dest,Block,Index,Info) -> #load_word_index{dst=Dest, block = Block, index=Index, info=Info}.
load_word_index_dest(LoadAtom) ->LoadAtom#load_word_index.dst.
load_word_index_dest_update(LA, NewDest) -> LA#load_word_index{dst=NewDest}.
load_word_index_block(LoadAtom) ->LoadAtom#load_word_index.block.
load_word_index_index(LoadAtom) ->LoadAtom#load_word_index.index.

%
% Load address
%

load_address_create(Dst, Addr, Type, Info) -> 
   #load_address{dst=Dst, address=Addr, type=Type, info=Info}.
load_address_dest(LoadAddress) -> LoadAddress#load_address.dst.
load_address_address(LoadAddress) -> LoadAddress#load_address.address.
load_address_dest_update(LA, NewDst) -> LA#load_address{dst=NewDst}.
load_address_type(LoadAddress) -> LoadAddress#load_address.type.
load_address_type_update(LA, NewType) -> LA#load_address{type=NewType}.


% 
% label
%

label_create(Name,Info) -> #label{id=Name,info=Info}.
label_create_new() -> label_create(hipe_gensym:get_next_label(), []).
label_name(Label) -> Label#label.id.
label_name_update(Label,NewName) -> Label#label{id=NewName}.
is_label(Insn) -> case type(Insn) of label -> true; _ -> false end.


%
% nop
%

nop_create(Info) -> #nop{info=Info}.
is_nop(Insn) -> case type(Insn) of nop -> true; _ -> false end.


%% Align %%
align_create(Number,Info) -> #align{alignment=Number,info=Info}.
align_alignment(Align) -> Align#align.alignment.
align_alignment_update(Align,NewNumber) -> Align#align{alignment=NewNumber}.
is_align(Insn) -> case type(Insn) of align -> true; _ -> false end.

%% Comment %%
comment_create(Text, Info) -> #comment{text=Text,info=Info}.
comment_text(Comment) -> Comment#comment.text.
comment_text_update(Comment, NewText) -> Comment#comment{text=NewText}.
is_comment(Insn) -> case type(Insn) of  comment -> true; _ -> false end.

%% Move %%
move_create(Dest, Source, Info) -> #move{dst=Dest, src=Source, info=Info}.
move_dest(Move) -> Move#move.dst.
move_src(Move) -> Move#move.src.
move_dest_update(Move, NewDest) -> Move#move{dst=NewDest}.
move_src_update(Move, NewSource) -> Move#move{src=NewSource}.
is_move(Insn) -> case type(Insn) of  move -> true; _ -> false end.

%% MultiMove %%
multimove_create(Dest, Source, Info) -> 
  case length(Dest) =:= length(Source) of
    true -> true;
    false -> exit({sparc, multimove, src_dst_aritydiff})
  end,
  #multimove{dst=Dest, src=Source, info=Info}.
multimove_dest(Move) -> Move#multimove.dst.
multimove_src(Move) -> Move#multimove.src.
multimove_dest_update(Move, NewDest) -> Move#multimove{dst=NewDest}.
multimove_src_update(Move, NewSource) -> Move#multimove{src=NewSource}.
is_multimove(Insn) -> case type(Insn) of  multimove -> true; _ -> false end.

%% Cmov_cc %%
cmov_cc_create(Dest,Source,Cond,Info) -> 
   #cmov_cc{dst=Dest,src=Source,cc=Cond,info=Info}.
cmov_cc_dest(CmovCc) -> CmovCc#cmov_cc.dst.
cmov_cc_src(CmovCc) -> CmovCc#cmov_cc.src.
cmov_cc_cond(CmovCc) -> CmovCc#cmov_cc.cc.
cmov_cc_dest_update(CmovCc,NewDest) -> CmovCc#cmov_cc{dst=NewDest}.
cmov_cc_src_update(CmovCc,NewSource) -> CmovCc#cmov_cc{src=NewSource}.
cmov_cc_cond_update(CmovCc,NewCond) -> CmovCc#cmov_cc{cc=NewCond}.

%% Cmov_r %%
cmov_r_create(Dest,Source,Reg,RegCond,Info) -> 
   #cmov_r{dst=Dest,src=Source,rcc=RegCond,reg=Reg,info=Info}.
cmov_r_dest(CmovR) -> CmovR#cmov_r.dst.
cmov_r_src(CmovR) -> CmovR#cmov_r.src.
cmov_r_reg(CmovR) -> CmovR#cmov_r.reg.
cmov_r_regcond(CmovR) -> CmovR#cmov_r.rcc.
cmov_r_dest_update(CmovR,NewDest) -> CmovR#cmov_r{dst=NewDest}.
cmov_r_src_update(CmovR,NewSource) -> CmovR#cmov_r{src=NewSource}.
cmov_r_reg_update(CmovR,NewReg) -> CmovR#cmov_r{reg=NewReg}.
cmov_r_regcond_update(CmovR,NewRegCond) -> CmovR#cmov_r{rcc=NewRegCond}.
is_cmov_r(Insn) -> case type(Insn) of  cmov_r -> true; _ -> false end.

%% Alu %%
alu_create(Dest,Src1,Op,Src2,Info) -> 
   #alu{dst=Dest,src1=Src1,src2=Src2,op=Op,info=Info}.
alu_dest(Alu) -> Alu#alu.dst.
alu_src1(Alu) -> Alu#alu.src1.
alu_operator(Alu) -> Alu#alu.op.
alu_src2(Alu) -> Alu#alu.src2.
alu_dest_update(Alu,NewDest) -> Alu#alu{dst=NewDest}.
alu_src1_update(Alu,NewSource1) -> Alu#alu{src1=NewSource1}.
alu_operator_update(Alu,NewOp) -> Alu#alu{op=NewOp}.
alu_src2_update(Alu,NewSource2) -> Alu#alu{src2=NewSource2}.
is_alu(Insn) -> case type(Insn) of alu -> true; _ -> false end.

%% Alu_cc %%
alu_cc_create(Dest,Src1,Op,Src2,Info) -> 
   #alu_cc{dst=Dest,src1=Src1,src2=Src2,op=Op,info=Info}.
alu_cc_dest(Alucc) -> Alucc#alu_cc.dst.
alu_cc_src1(Alucc) -> Alucc#alu_cc.src1.
alu_cc_operator(Alucc) -> Alucc#alu_cc.op.
alu_cc_src2(Alucc) -> Alucc#alu_cc.src2.
alu_cc_dest_update(Alu,NewDest) -> Alu#alu_cc{dst=NewDest}.
alu_cc_src1_update(Alu,NewSource1) -> Alu#alu_cc{src1=NewSource1}.
alu_cc_operator_update(Alu,NewOp) -> Alu#alu_cc{op=NewOp}.
alu_cc_src2_update(Alu,NewSource2) -> Alu#alu_cc{src2=NewSource2}.
is_alu_cc(Insn) -> case type(Insn) of alu_cc -> true; _ -> false end.

%% Sethi %%
sethi_create(Dest,Const,Info) -> 
   #sethi{dst=Dest,const=Const,info=Info}.
sethi_dest(Sethi) -> Sethi#sethi.dst.
sethi_const(Sethi) -> Sethi#sethi.const.
sethi_dest_update(SetHi,NewDest) -> SetHi#sethi{dst=NewDest}.
sethi_const_update(SetHi,NewConst) -> SetHi#sethi{const=NewConst}.

%% Load %%
load_create(Dest,Type,Source,Off,Info) -> 
   #load{dst=Dest,type=Type,src=Source,off=Off,info=Info}.
load_dest(Load) -> Load#load.dst.
load_type(Load) -> Load#load.type.
load_src(Load) -> Load#load.src.
load_off(Load) -> Load#load.off.
load_dest_update(Load,NewDest) -> Load#load{dst=NewDest}.
load_type_update(Load,NewType) -> Load#load{type=NewType}.
load_src_update(Load,NewSource) -> Load#load{src=NewSource}.
load_off_update(Load,NewOff) -> Load#load{off=NewOff}.
is_load(Insn) -> case type(Insn) of load -> true; _ -> false end.

%% Store %%
store_create(Target,Off,Type,Source,Info) -> 
   #store{dst=Target,type=Type,src=Source,off=Off,info=Info}.
store_dest(Store) -> Store#store.dst.
store_off(Store) -> Store#store.off.
store_type(Store) -> Store#store.type.
store_src(Store) -> Store#store.src.
store_dest_update(Store,NewTarget) -> Store#store{dst=NewTarget}.
store_off_update(Store,NewOff) -> Store#store{off=NewOff}.
store_type_update(Store,NewType) -> Store#store{type=NewType}.
store_src_update(Store,NewSource) -> Store#store{src=NewSource}.
is_store(Insn) -> case type(Insn) of store -> true; _ -> false end.

%% B %%
b_create(Cond,TrueLabel,FalseLabel,Pred,Annul,Info) -> 
   #b{cc=Cond,true_label=TrueLabel,false_label=FalseLabel, pred=Pred,
      annul=Annul,info=Info}.
b_cond(B) -> B#b.cc.
b_cond_update(B,NewCond) -> B#b{cc=NewCond}.
b_label(B) -> b_true_label(B).
b_label_update(B,NewLabel) -> b_true_label_update(B,NewLabel).
b_true_label(B) -> B#b.true_label.
b_true_label_update(B,NewLabel) -> B#b{true_label=NewLabel}.
b_false_label(B) -> B#b.false_label.
b_false_label_update(B,NewLabel) -> B#b{false_label=NewLabel}.
b_pred(B) -> B#b.pred.
b_pred_update(B,NewPred) -> B#b{pred=NewPred}.
b_annul_update(B,NewAnnul) -> B#b{annul=NewAnnul}.
b_annul(B) -> B#b.annul.
b_taken(B) -> P = b_pred(B), if P > 0.5 -> true; true -> false end.
is_b(Insn) -> case type(Insn) of b -> true; _ -> false end.

%% Br %%
br_create(Reg,RegCond,TrueLabel,FalseLabel,Pred,Annul,Info) -> 
   #br{reg=Reg,rcc=RegCond,true_label=TrueLabel,false_label=FalseLabel,
       pred=Pred,annul=Annul,info=Info}.
br_reg(Br) -> Br#br.reg.
br_reg_update(Br,NewReg) -> Br#br{reg=NewReg}.
br_regcond(Br) -> Br#br.rcc.
br_regcond_update(Br,NewRegCond) -> Br#br{rcc=NewRegCond}.
br_label(Br) -> br_true_label(Br).
br_label_update(Br,NewLabel) -> br_true_label_update(Br,NewLabel).
br_true_label(Br) -> Br#br.true_label.
br_true_label_update(Br,NewLabel) -> Br#br{true_label=NewLabel}.
br_false_label(Br) -> Br#br.false_label.
br_false_label_update(Br,NewLabel) -> Br#br{false_label=NewLabel}.
br_pred(Br) -> Br#br.pred.
br_pred_update(Br,NewPred) -> Br#br{pred=NewPred}.
br_annul(Br) -> Br#br.annul.
br_annul_update(Br,NewAnnul) -> Br#br{annul=NewAnnul}.
br_taken(Br) -> P = br_pred(Br), if P > 0.5 -> true; true -> false end.
is_br(Insn) -> case type(Insn) of br -> true; _ -> false end.


%% Goto %%
goto_create(Label, Info) -> #goto{label=Label, info=Info}.
goto_label(Goto) -> Goto#goto.label.
goto_label_update(Goto, NewLabel) -> Goto#goto{label=NewLabel}.
is_goto(I) -> case type(I) of goto -> true; _ -> false end.
  

%% Jmp_link %%
jmp_link_create(Target, Offset, Link, Args, Info) -> 
   #jmp_link{target=Target, off=Offset, link=Link, args=Args, info=Info}.
jmp_link_target(JmpLink) -> JmpLink#jmp_link.target.
jmp_link_target_update(JmpLink,NewTarget) -> 
   JmpLink#jmp_link{target=NewTarget}.
jmp_link_off(JmpLink) -> JmpLink#jmp_link.off.
jmp_link_off_update(JmpLink,NewOffset) -> JmpLink#jmp_link{off=NewOffset}.
jmp_link_link(JmpLink) -> JmpLink#jmp_link.link.
jmp_link_link_update(JmpLink,NewLink) -> JmpLink#jmp_link{link=NewLink}.
jmp_link_args(JmpLink) -> JmpLink#jmp_link.args.
jmp_link_args_update(JmpLink,NewArgs) -> JmpLink#jmp_link{args=NewArgs}.
is_jmp_link(Insn) -> case type(Insn) of jmp_link -> true; _ -> false end.

%% Jmp %%
jmp_create(Target, Offset, Args, Info) -> 
   #jmp{target=Target, off=Offset, args=Args, info=Info}.
jmp_create(Target, Offset, Args, Destinations, Info) -> 
   #jmp{target=Target, off=Offset, args=Args, 
	destinations=Destinations, info=Info}.
jmp_target(Jmp) -> Jmp#jmp.target.
jmp_target_update(Jmp,NewTarget) -> Jmp#jmp{target=NewTarget}.
jmp_off(Jmp) -> Jmp#jmp.off.
jmp_off_update(Jmp,NewOffset) -> Jmp#jmp{off=NewOffset}.
jmp_args(Jmp) -> Jmp#jmp.args.
jmp_args_update(Jmp,NewArgs) -> Jmp#jmp{args=NewArgs}.
jmp_destinations(Jmp) -> Jmp#jmp.destinations.
jmp_destinations_update(Jmp,NewDests) -> Jmp#jmp{destinations=NewDests}.

%% Call_link %%
call_link_create(Target,Link,Args,Cont, Fail, Type, Info) ->
   #call_link{target=Target, link=Link, args=Args, 
	      continuation_label=Cont,
	      fail_label=Fail, type = Type, info=Info}.
call_link_target(CL) -> CL#call_link.target.
call_link_target_update(CL,NewTarget) -> CL#call_link{target=NewTarget}.
call_link_link(CL) -> CL#call_link.link.
call_link_link_update(CL,NewLink) -> CL#call_link{link=NewLink}.
call_link_args(CL) -> CL#call_link.args.
call_link_args_update(CL,NewArgs) -> CL#call_link{args=NewArgs}.
call_link_continuation(I) ->  I#call_link.continuation_label.
call_link_continuation_update(CL,NewC) -> CL#call_link{continuation_label=NewC}.
call_link_fail(I) ->  I#call_link.fail_label.
call_link_fail_update(CL,NewF) -> CL#call_link{fail_label=NewF}.
call_link_type(I) ->  I#call_link.type.
is_call_link(Insn) -> case type(Insn) of call_link -> true; _ -> false end.

%% Integer regs %%
mk_reg(RegNr) -> {sparc_reg, RegNr}.
mk_new_reg() -> mk_reg(hipe_gensym:get_next_var()).
is_reg(I) -> case I of {sparc_reg, _} -> true; _ -> false end.
reg_nr({sparc_reg, Name}) -> Name.

%% FP regs %%
mk_fpreg(RegNr) -> {sparc_fpreg, RegNr}.
is_fpreg(I) -> case I of {sparc_fpreg, _} -> true; _ -> false end.
fpreg_nr({sparc_fpreg, RegNr}) -> RegNr.

%% Immediates %%
mk_imm(Value) -> {sparc_imm, Value}.
is_imm(I) -> case I of {sparc_imm, _} -> true; _ -> false end.
imm_value({sparc_imm, Value}) -> Value.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Selectors
%

type(Insn) ->
   element(1, Insn).
%   if
%      record(Insn,label) -> label;
%      record(Insn,nop) -> nop;
%      record(Insn,align) -> align;
%      record(Insn,comment) -> comment;
%      record(Insn,move) -> move;
%      record(Insn,cmov_cc) -> cmov_cc;
%      record(Insn,cmov_r) -> cmov_r;
%      record(Insn,alu) -> alu;
%      record(Insn,alu_cc) -> alu_cc;
%      record(Insn,sethi) -> sethi;
%      record(Insn,load) -> load;
%      record(Insn,store) -> store;
%      record(Insn,b) -> b;
%      record(Insn,br) -> br;
%      record(Insn,goto) -> goto;
%      record(Insn,jmp_link) -> jmp_link;
%      record(Insn,jmp) -> jmp;
%      record(Insn,call_link) -> call_link;
%      record(Insn,load_atom) -> load_atom;
%      record(Insn,load_address) -> load_address
%   end.

info(Insn) ->
    if
       record(Insn,label) -> Insn#label.info;
       record(Insn,nop) -> Insn#nop.info;
       record(Insn,align) -> Insn#align.info;
       record(Insn,comment) -> Insn#comment.info;
       record(Insn,move) -> Insn#move.info;
       record(Insn,multimove) -> Insn#multimove.info;
       record(Insn,cmov_cc) -> Insn#cmov_cc.info;
       record(Insn,cmov_r) -> Insn#cmov_r.info;
       record(Insn,alu) -> Insn#alu.info;
       record(Insn,alu_cc) -> Insn#alu_cc.info;
       record(Insn,sethi) -> Insn#sethi.info;
       record(Insn,load) -> Insn#load.info;
       record(Insn,store) -> Insn#store.info;
       record(Insn,b) -> Insn#b.info;
       record(Insn,br) -> Insn#br.info;
       record(Insn,goto) -> Insn#goto.info;
       record(Insn,jmp_link) -> Insn#jmp_link.info;
       record(Insn,jmp) -> Insn#jmp.info;
       record(Insn,load_atom) -> Insn#load_atom.info;
       record(Insn,load_word_index) -> Insn#load_word_index.info;
       record(Insn,load_address) -> Insn#load_address.info;
       record(Insn,call_link) -> Insn#call_link.info
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Predicates
%

is_cmov_cc(Insn) -> case type(Insn) of cmov_cc -> true; _ -> false end.
is_sethi(Insn) -> case type(Insn) of  sethi -> true; _ -> false end.
is_jmp(Insn) -> case type(Insn) of jmp -> true; _ -> false end.
% is_load_atom(Insn) -> case type(Insn) of load_atom -> true; _ -> false end.
% is_load_address(I) -> case type(I) of load_address -> true; _ -> false end.

is_any_alu(Insn) -> 
   case type(Insn) of 
      alu -> true; 
      alu_cc ->true; 
      _ -> false 
   end.

is_any_cmov(Insn) -> 
   case type(Insn) of
      cmov_cc -> true;
      cmov_r -> true;
      _ -> false
   end.

is_any_memop(Insn) -> 
   case type(Insn) of
      load -> true;
      store -> true;
      _ -> false
   end.

is_any_branch(Insn) -> 
   case type(Insn) of
      b -> true;
      br -> true;
      goto -> true;
      jmp -> true;
      jmp_link -> true;
      call_link -> true;
      _ -> false
   end.


%
% Changes the target of a jump.
%

redirect_jmp(Jmp, ToOld, ToNew) ->
   case type(Jmp) of
      br ->
	 case br_true_label(Jmp) of
	    ToOld ->
	       br_true_label_update(Jmp, ToNew);
	    _ ->
	       case br_false_label(Jmp) of
		  ToOld ->
		     br_false_label_update(Jmp, ToNew);
		  _ ->
		     Jmp
	       end
	 end;
      b ->
	 case b_true_label(Jmp) of
	    ToOld ->
	       b_true_label_update(Jmp, ToNew);
	    _ ->
	       case b_false_label(Jmp) of
		  ToOld ->
		     b_false_label_update(Jmp, ToNew);
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
      call_link ->
        case call_link_continuation(Jmp) of
	    ToOld ->
	    call_link_continuation_update(Jmp, ToNew);
	    _ ->
	       case call_link_fail(Jmp) of
		  ToOld ->
		     call_link_fail_update(Jmp, ToNew);
		  _ ->
		     Jmp
	       end
	 end;
      _ ->
	 Jmp
   end.


%
% The negation of a conditional.
%

cc_negate(Cond) ->
    case Cond of
	'a'	-> 'n';
	'n'	-> 'a';
	'ne'	-> 'e';
	'e'	-> 'ne';
	'g'	-> 'le';
	'le'	-> 'g';
	'ge'	-> 'l';
	'l'	-> 'ge';
	'gu'	-> 'leu';
	'leu'	-> 'gu';
	'geu'	-> 'lu';
	'lu'	-> 'geu';
	'pos'	-> 'neg';
	'neg'	-> 'pos';
	'vc'	-> 'vs';
	'vs'	-> 'vc'
   end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Def/Use by instruction.
%
% The condition code registers is read and written like any other. 
% We define a _virtual_ for icc and xcc.
%

icc_reg() -> mk_reg(hipe_sparc_registers:icc()).
xcc_reg() -> mk_reg(hipe_sparc_registers:xcc()).


%
% Returnes a list of registers that are used by an instruction
%

uses(Instr) -> 
   remove_immediates(all_uses(Instr)).

all_uses(Ins) ->
   case type(Ins) of
      label -> [];
      nop -> [];
      align -> [];
      comment -> [];
      move -> [move_src(Ins)];
      multimove -> multimove_src(Ins);
      cmov_cc -> [icc_reg(), xcc_reg(), cmov_cc_src(Ins)];
      cmov_r -> [cmov_r_src(Ins), cmov_r_reg(Ins)];
      alu -> [alu_src1(Ins), alu_src2(Ins)];
      alu_cc -> [alu_cc_src1(Ins), alu_cc_src2(Ins)];
      sethi -> [sethi_const(Ins)];
      load -> [load_src(Ins), load_off(Ins)];
      store -> [store_dest(Ins), store_src(Ins), store_off(Ins)];
      b -> [icc_reg(), xcc_reg()];
      br -> [br_reg(Ins)];
      goto -> [];
      jmp_link ->
	 [jmp_link_target(Ins), jmp_link_off(Ins) | jmp_link_args(Ins)];
      jmp -> [jmp_target(Ins), jmp_off(Ins) | jmp_args(Ins)];
      call_link -> 
       case call_link_type(Ins) of
	 closure ->
	   [call_link_target(Ins)|call_link_args(Ins)];
	 _ ->
	   call_link_args(Ins)
       end;
      load_atom -> [];
      load_word_index -> [];
      load_address -> []
   end.

imm_uses(Instr) -> 
  remove_registers(all_uses(Instr)).


%
% Returnes a list of registers that are defined by an instruction
%

defines(Ins) ->
   Defs = case type(Ins) of
	     label -> [];
	     nop -> [];
	     align -> [];
	     comment -> [];
	     move -> [move_dest(Ins)];
	     multimove -> multimove_dest(Ins);
	     cmov_cc -> [cmov_cc_dest(Ins)];
	     cmov_r -> [cmov_r_dest(Ins)];
	     alu -> [alu_dest(Ins)];
	     alu_cc -> [icc_reg(), xcc_reg(), alu_cc_dest(Ins)];
	     sethi -> [sethi_dest(Ins)];
	     load -> [load_dest(Ins)];
	     store -> [];
	     b -> [];
	     br -> [];
	     goto -> [];
	     jmp_link -> [jmp_link_link(Ins)];
	     jmp -> [];
	     call_link -> [call_link_link(Ins)];
	     load_atom -> [load_atom_dest(Ins)];
	     load_word_index -> [load_word_index_dest(Ins)];
	     load_address -> [load_address_dest(Ins)]
	  end,
   remove_immediates(Defs).


%
% Totally redundant, but we need it for speed.
%

def_use(I) ->
  {Def,Use} = 
    case type(I) of
      label -> {[],[]};
      nop -> {[],[]};
      align -> {[],[]};
      comment ->  {[],[]};
      move -> 
	 {[move_dest(I)],
	  [move_src(I)]};
      multimove -> 
	 {multimove_dest(I),
	  multimove_src(I)};
      cmov_cc ->
	 {[cmov_cc_dest(I)],
	 [icc_reg(), xcc_reg(), cmov_cc_src(I)]};
      cmov_r ->
	 {[cmov_r_dest(I)],
	  [cmov_r_src(I), cmov_r_reg(I)]};
      alu -> 
	{[alu_dest(I)],
	 [alu_src1(I), alu_src2(I)]};
      alu_cc ->
	 {[icc_reg(), xcc_reg(), alu_cc_dest(I)],
	  [alu_cc_src1(I), alu_cc_src2(I)]};
      sethi ->
	 {[sethi_dest(I)], 
	  [sethi_const(I)]};
      load -> 
	 {[load_dest(I)],
	  [load_src(I), load_off(I)]};
      store ->
	 {[],
	  [store_dest(I), store_src(I), store_off(I)]};
      b -> 
	 {[],
	  [icc_reg(), xcc_reg()]};
      br -> 
	 {[],
	  [br_reg(I)]};
      goto -> {[],[]};
      jmp_link ->
	 {[jmp_link_link(I)],
	  [jmp_link_target(I), jmp_link_off(I) | jmp_link_args(I)]};
      jmp ->
	 {[],
	  [jmp_target(I), jmp_off(I) | jmp_args(I)]};
      call_link ->
	 {[call_link_link(I)],
	  case call_link_type(I) of
	    closure -> 
	      [call_link_target(I)|call_link_args(I)];
	    _-> call_link_args(I)
	  end};
      load_atom ->
	 {[load_atom_dest(I)],
	  []};
      load_word_index ->
	 {[load_word_index_dest(I)],
	  []};
      load_address -> 
	 {[load_address_dest(I)],
	  []}
   end,
   {remove_immediates(Def), remove_immediates(Use)}.
   

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
% Remove registers from a list
%

remove_registers([]) -> 
   [];
remove_registers([I|Is]) ->
   case is_reg(I) of
      true -> remove_registers(Is);
      false -> [I | remove_registers(Is)]
   end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Substitution: replace occurrences of X by Y if {X,Y} is in Subst.
%

subst(Ins, Subst) ->
   case type(Ins) of
      label -> Ins;
      nop -> Ins;
      align -> Ins;
      comment -> Ins;
      b -> Ins;
      goto -> Ins;
      sethi ->
	 NewDst = subst1(Subst, sethi_dest(Ins)),
	 sethi_dest_update(Ins, NewDst);
      br ->
	 NewReg = subst1(Subst, br_reg(Ins)),
	 br_reg_update(Ins, NewReg);
      call_link ->
         Ins1 =
           case call_link_type(Ins) of
	     closure ->
	       NewTarget = subst1(Subst, call_link_target(Ins)),
	       call_link_target_update(Ins, NewTarget);
	     _ -> Ins
	   end,
%         NewArgs = subst1(Subst, call_link_link(Ins1)),
%	 call_link_link_update(Ins1, NewLink);

	 NewLink = subst1(Subst, call_link_link(Ins1)),
	 call_link_link_update(Ins1, NewLink);
      load_atom ->
	 NewDst = subst1(Subst, load_atom_dest(Ins)),
	 load_atom_dest_update(Ins, NewDst);
      load_word_index ->
	 NewDst = subst1(Subst, load_word_index_dest(Ins)),
	 load_word_index_dest_update(Ins, NewDst);
      load_address ->
	 NewDst = subst1(Subst, load_address_dest(Ins)),
	 load_address_dest_update(Ins, NewDst);
      move ->
	 NewSrc = subst1(Subst, move_src(Ins)),
	 NewDst = subst1(Subst, move_dest(Ins)),
	 I0 = move_dest_update(Ins, NewDst),
	 move_src_update(I0, NewSrc);
      multimove ->
	 NewSrc = subst_list(Subst, multimove_src(Ins)),
	 NewDst = subst_list(Subst, multimove_dest(Ins)),
	 I0 = multimove_dest_update(Ins, NewDst),
	 multimove_src_update(I0, NewSrc);
      cmov_cc ->
	 NewSrc = subst1(Subst, cmov_cc_src(Ins)),
	 NewDst = subst1(Subst, cmov_cc_dest(Ins)),
	 I0 = cmov_cc_dest_update(Ins, NewDst),
	 cmov_cc_src_update(I0, NewSrc);
      cmov_r ->
	 NewSrc = subst1(Subst, cmov_r_src(Ins)),
	 NewReg = subst1(Subst, cmov_r_reg(Ins)),
	 NewDst = subst1(Subst, cmov_r_dest(Ins)),
	 I0 = cmov_r_dest_update(Ins, NewDst),
	 I1 = cmov_r_src_update(I0, NewSrc),
	 cmov_r_reg_update(I1, NewReg);
      alu ->
	 NewSrc1 = subst1(Subst, alu_src1(Ins)),
	 NewSrc2 = subst1(Subst, alu_src2(Ins)),
	 NewDst = subst1(Subst, alu_dest(Ins)),
	 I0 = alu_dest_update(Ins, NewDst),
	 I1 = alu_src1_update(I0, NewSrc1),
	 alu_src2_update(I1, NewSrc2);
      alu_cc ->
	 NewSrc1 = subst1(Subst, alu_cc_src1(Ins)),
	 NewSrc2 = subst1(Subst, alu_cc_src2(Ins)),
	 NewDst = subst1(Subst, alu_cc_dest(Ins)),
	 I0 = alu_cc_dest_update(Ins, NewDst),
	 I1 = alu_cc_src1_update(I0, NewSrc1),
	 alu_cc_src2_update(I1, NewSrc2);
      load ->
	 NewSrc = subst1(Subst, load_src(Ins)),
	 NewOff = subst1(Subst, load_off(Ins)),
	 NewDst = subst1(Subst, load_dest(Ins)),
	 I0 = load_dest_update(Ins, NewDst),
	 I1 = load_src_update(I0, NewSrc),
	 load_off_update(I1, NewOff);
      store ->
	 NewSrc = subst1(Subst, store_src(Ins)),
	 NewOff = subst1(Subst, store_off(Ins)),
	 NewDst = subst1(Subst, store_dest(Ins)),
	 I0 = store_dest_update(Ins, NewDst),
	 I1 = store_src_update(I0, NewSrc),
	 store_off_update(I1, NewOff);
      jmp_link ->
	 NewTarget = subst1(Subst, jmp_link_target(Ins)),
	 NewOff = subst1(Subst, jmp_link_off(Ins)),
	 NewLink = subst1(Subst, jmp_link_link(Ins)),
	 I0 = jmp_link_link_update(Ins, NewLink),
	 I1 = jmp_link_target_update(I0, NewTarget),
	 jmp_link_off_update(I1, NewOff);
      jmp ->
	 NewTarget = subst1(Subst, jmp_target(Ins)),
	 NewOff = subst1(Subst, jmp_off(Ins)),
	 I0 = jmp_target_update(Ins, NewTarget),
	 jmp_off_update(I0, NewOff)
   end.


subst_defines(Ins, Subst) ->
   case type(Ins) of
      label -> Ins;
      nop -> Ins;
      align -> Ins;
      comment -> Ins;
      b -> Ins;
      goto -> Ins;
      jmp -> Ins;
      br -> Ins;
      sethi ->
	 NewDst = subst1(Subst, sethi_dest(Ins)),
	 sethi_dest_update(Ins, NewDst);
      call_link ->
	 NewLink = subst1(Subst, call_link_link(Ins)),
	 call_link_link_update(Ins, NewLink);
      load_atom ->
	 NewDst = subst1(Subst, load_atom_dest(Ins)),
	 load_atom_dest_update(Ins, NewDst);
      load_word_index ->
	 NewDst = subst1(Subst, load_word_index_dest(Ins)),
	 load_word_index_dest_update(Ins, NewDst);
      load_address ->
	 NewDst = subst1(Subst, load_address_dest(Ins)),
	 load_address_dest_update(Ins, NewDst);
      move ->
	 NewDst = subst1(Subst, move_dest(Ins)),
	 move_dest_update(Ins, NewDst);
      multimove ->
	 NewDst = subst_list(Subst, multimove_dest(Ins)),
	 multimove_dest_update(Ins, NewDst);
      cmov_cc ->
	 NewDst = subst1(Subst, cmov_cc_dest(Ins)),
	 cmov_cc_dest_update(Ins, NewDst);
      cmov_r ->
	 NewDst = subst1(Subst, cmov_r_dest(Ins)),
	 cmov_r_dest_update(Ins, NewDst);
      alu ->
	 NewDst = subst1(Subst, alu_dest(Ins)),
	 alu_dest_update(Ins, NewDst);
      alu_cc ->
	 NewDst = subst1(Subst, alu_cc_dest(Ins)),
	 alu_cc_dest_update(Ins, NewDst);
      load ->
	 NewDst = subst1(Subst, load_dest(Ins)),
	 load_dest_update(Ins, NewDst);
      store -> Ins;
      jmp_link ->
	 NewLink = subst1(Subst, jmp_link_link(Ins)),
	 jmp_link_link_update(Ins, NewLink)
   end.


subst_uses(Ins, Subst) ->
   case type(Ins) of
      label -> Ins;
      nop -> Ins;
      align -> Ins;
      comment -> Ins;
      b -> Ins;
      goto -> Ins;
      sethi -> Ins;
      call_link -> 
       case call_link_type(Ins) of
	 closure ->
	   NewTarget = subst1(Subst, call_link_target(Ins)),
	   call_link_target_update(Ins, NewTarget);
	 _ -> Ins
       end;
      load_atom -> Ins;
      load_word_index -> Ins;
      load_address -> Ins;
      br ->
	 NewReg = subst1(Subst, br_reg(Ins)),
	 br_reg_update(Ins, NewReg);
      move ->
	 NewSrc = subst1(Subst, move_src(Ins)),
	 move_src_update(Ins, NewSrc);
      multimove ->
	 NewSrc = subst_list(Subst, multimove_src(Ins)),
	 multimove_src_update(Ins, NewSrc);
      cmov_cc ->
	 NewSrc = subst1(Subst, cmov_cc_src(Ins)),
	 cmov_cc_src_update(Ins, NewSrc);
      cmov_r ->
	 NewSrc = subst1(Subst, cmov_r_src(Ins)),
	 NewReg = subst1(Subst, cmov_r_reg(Ins)),
	 I1 = cmov_r_src_update(Ins, NewSrc),
	 cmov_r_reg_update(I1, NewReg);
      alu ->
	 NewSrc1 = subst1(Subst, alu_src1(Ins)),
	 NewSrc2 = subst1(Subst, alu_src2(Ins)),
	 I1 = alu_src1_update(Ins, NewSrc1),
	 alu_src2_update(I1, NewSrc2);
      alu_cc ->
	 NewSrc1 = subst1(Subst, alu_cc_src1(Ins)),
	 NewSrc2 = subst1(Subst, alu_cc_src2(Ins)),
	 I1 = alu_cc_src1_update(Ins, NewSrc1),
	 alu_cc_src2_update(I1, NewSrc2);
      load ->
	 NewSrc = subst1(Subst, load_src(Ins)),
	 NewOff = subst1(Subst, load_off(Ins)),
	 I1 = load_src_update(Ins, NewSrc),
	 load_off_update(I1, NewOff);
      store ->
	 NewSrc = subst1(Subst, store_src(Ins)),
	 NewOff = subst1(Subst, store_off(Ins)),
	 NewDst = subst1(Subst, store_dest(Ins)),
	 I0 = store_dest_update(Ins, NewDst),
	 I1 = store_src_update(I0, NewSrc),
	 store_off_update(I1, NewOff);
      jmp_link ->
	 NewTarget = subst1(Subst, jmp_link_target(Ins)),
	 NewOff = subst1(Subst, jmp_link_off(Ins)),
	 I1 = jmp_link_target_update(Ins, NewTarget),
	 jmp_link_off_update(I1, NewOff);
      jmp ->
	 NewTarget = subst1(Subst, jmp_target(Ins)),
	 NewOff = subst1(Subst, jmp_off(Ins)),
	 I0 = jmp_target_update(Ins, NewTarget),
	 jmp_off_update(I0, NewOff)
   end.

subst_list(S,Xs) ->
   [subst1(S,X) || X <- Xs].

subst1([],X) -> 
   X;
subst1([{X,Y}|Xs],X) -> 
   Y;
subst1([_|Xs],X) -> 
   subst1(Xs,X).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Verifies that a code sequence is well-formed.
%
% NOTE: does not consider branch offsets.

verify([],Version) -> ok;
verify([X|Xs],Version) ->
   case catch ver(X,Version) of
      {'EXIT',_} ->
	 report('instruction ~w malformed~n',[X]);
      _ -> ok
   end,
   verify(Xs,Version).

ver(Label,_Version) when record(Label,label) ->
   L = Label#label.id,
   if
      integer(L), L > 0 -> ok
   end;

ver(Nop,_Version) when record(Nop,nop) -> 
   ok;
ver(Comment,_Version) when record(Comment,comment) -> 
   ok;
ver(Align,_Version) when record(Align,align) ->
   N = Align#align.alignment,
   if
      N == 1 -> ok;
      N == 2 -> ok;
      N == 4 -> ok;
      N == 8 -> ok;
      N == 16 -> ok
   end;
ver(Move,_Version) when record(Move,move) ->
   Dst = Move#move.dst,
   Src = Move#move.src,
   case {reg(Dst),reg_or_imm13(Src)} of
      {true,true} -> ok
   end;
ver(Cmov_cc,Version) when record(Cmov_cc,cmov_cc) ->
   if 
      Version == 9 ->
	 Dst = Cmov_cc#cmov_cc.dst,
	 Src = Cmov_cc#cmov_cc.src,
	 CC = Cmov_cc#cmov_cc.cc,
	 case {reg(Dst),reg_or_imm11(Src),int_cc(CC)} of
	    {true,true,true} -> ok
	 end
   end;
ver(Cmov_r,Version) when record(Cmov_r,cmov_r) ->
   if 
      Version == 9 ->
	 Dst = Cmov_r#cmov_r.dst,
	 Src = Cmov_r#cmov_r.src,
	 Pred = Cmov_r#cmov_r.reg,
	 Cond = Cmov_r#cmov_r.rcc,
	 case {reg(Dst),reg_or_imm10(Src),reg(Pred),reg_cc(Cond)} of
	    {true,true,true,true} -> ok
	 end
   end;
ver(Alu,Version) when record(Alu,alu) ->
   Dst = Alu#alu.dst,
   Src1 = Alu#alu.src1,
   Op = Alu#alu.op,
   Src2 = Alu#alu.src2,
   case {reg(Dst),reg(Src1),alu_op(Op,Version),reg_or_imm13(Src2)} of
      {true,true,true,true} ->
	 ok
   end;
ver(Alu_cc,Version) when record(Alu_cc,alu_cc) ->
   Dst = Alu_cc#alu_cc.dst,
   Src1 = Alu_cc#alu_cc.src1,
   Op = Alu_cc#alu_cc.op,
   Src2 = Alu_cc#alu_cc.src2,
   case {reg(Dst),reg(Src1),alu_cc_op(Op,Version),reg_or_imm13(Src2)} of
      {true,true,true,true} -> ok
   end;
ver(Sethi,_Version) when record(Sethi,sethi) ->
   Dst = Sethi#sethi.dst,
   Const = Sethi#sethi.const,
   case {reg(Dst),imm22(Const)} of
      {true,true} -> ok
   end;
ver(Load,_Version) when record(Load,load) ->
   Dst = Load#load.dst,
   Type = Load#load.type,
   Src = Load#load.src,
   Off = Load#load.off,
   case {reg(Dst),loading_type(Type),reg(Src),reg_or_imm13(Off)} of
      {true,true,true,true} -> ok
   end;
ver(Store,_Version) when record(Store,store) ->
   Dst = Store#store.dst,
   Off = Store#store.off,
   Type = Store#store.type,
   Src = Store#store.src,
   case {reg(Dst),reg_or_imm13(Off),storing_type(Type),reg(Src)} of
      {true,true,true,true} -> ok
   end;
ver(B,Version) when record(B,b) ->
   CC = B#b.cc,
   Pred = B#b.pred,
   Annul = B#b.annul,
   if
      Version == 9 ->
	 case {int_cc(CC),prediction(Pred),annul_info(Annul)} of
	    {true,true,true} -> ok
	 end;
      Version == 8 ->
	 case {int_cc(CC),prediction(Pred),annul_info(Annul)} of
	    {true,true,true} -> ok
	 end
   end;
ver(Br,Version) when record(Br,br) ->
   Reg = Br#br.reg,
   RC = Br#br.rcc,
   Pred = Br#br.pred,
   Annul = Br#br.annul,
   if
      Version == 9 ->
	 case {reg(Reg),reg_cc(RC),prediction(Pred),annul_info(Annul)} of
	    {true,true,true,true} -> ok
	 end
   end;
ver(Jmp_link,_Version) when record(Jmp_link,jmp_link) ->
   Target = Jmp_link#jmp_link.target,
   Off = Jmp_link#jmp_link.off,
   Link = Jmp_link#jmp_link.link,
   case {reg(Target),reg_or_imm13(Off),reg(Link)} of
      {true,true,true} -> ok
   end;
ver(Jmp,_Version) when record(Jmp,jmp) ->
   Target = Jmp#jmp.target,
   Off = Jmp#jmp.off,
   case {reg(Target),reg_or_imm13(Off)} of
      {true,true} -> ok
   end;
ver(Call_link,_Version) when record(Call_link,call_link) ->
   Link = Call_link#call_link.link,
   case reg(Link) of
      true -> ok
   end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Condition code handling.
%

reg_cc(RCond) ->	% for BPr and CMOVr isns
    case RCond of
	'z'	-> true;
	'lez'	-> true;
	'lz'	-> true;
	'nz'	-> true;
	'gz'	-> true;
	'gez'	-> true;
	_	-> false	% XXX: serious error, should exit
    end.

int_cc(Cond) ->
    case Cond of
	'a'	-> true;
	'n'	-> true;
	'ne'	-> true;
	'e'	-> true;
	'g'	-> true;
	'le'	-> true;
	'ge'	-> true;
	'l'	-> true;
	'gu'	-> true;
	'leu'	-> true;
	'geu'	-> true;
	'lu'	-> true;
	'pos'	-> true;
	'neg'	-> true;
	'vc'	-> true;
	'vs'	-> true;
	_	-> false	% XXX: serious error, should exit
   end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

reg({reg,R}) when integer(R), R >= 0 -> true;
reg(_) -> false.

reg_or_imm13({imm,Imm}) ->
   imm13(Imm);
reg_or_imm13(Reg) -> reg(Reg).

reg_or_imm11({imm,Imm}) ->
   imm11(Imm);
reg_or_imm11(Reg) -> reg(Reg).

reg_or_imm10({imm,Imm}) ->
   imm10(Imm);
reg_or_imm10(Reg) -> reg(Reg).

% Note: does not verify branch offsets

%label_imm16({label,L}) when L > 0 -> true;  
%label_imm16(_) -> false.

%label_imm19({label,L}) when L > 0 -> true;
%label_imm19(_) -> false.

%label_imm22({label,L}) when L > 0 -> true;
%label_imm22(_) -> false.

%label_imm30({label,L}) when L > 0 -> true;
%label_imm30(_) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

imm10(N) -> imm(N,10).
imm11(N) -> imm(N,11).
imm13(N) -> imm(N,13).
imm16(N) -> imm(N,16).
imm19(N) -> imm(N,19).
imm22(N) -> imm(N,22).

imm(Imm,N) when Imm >= -((1 bsl N+1)-1), Imm < (1 bsl N+1) ->
    true;
imm(Imm,N) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prediction(taken) -> true;
prediction(untaken) -> true;
prediction(_) -> false.

annul_info(a) -> true;
annul_info(na) -> true;
annul_info(_) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

loading_type(X) -> data_type(X).

storing_type(X) -> data_type(X).

data_type(sb) -> true;
data_type(sh) -> true;
data_type(sw) -> true;
data_type(ub) -> true;
data_type(uh) -> true;
data_type(uw) -> true;
data_type(xw) -> true;
data_type(_) -> false.

alu_op(X,Version) -> int_op(X,Version).

alu_cc_op(X,Version) -> int_op(X,Version).

int_op('+',Version) -> is_version(Version,[8,9]);
int_op('+c',Version) -> is_version(Version,[8,9]);
int_op('and',Version) -> is_version(Version,[8,9]);
int_op('andn',Version) -> is_version(Version,[8,9]);
int_op('or',Version) -> is_version(Version,[8,9]);
int_op('orn',Version) -> is_version(Version,[8,9]);
int_op('xor',Version) -> is_version(Version,[8,9]);
int_op('xnor',Version) -> is_version(Version,[8,9]);
int_op('-',Version) -> is_version(Version,[8,9]);
int_op('-c',Version) -> is_version(Version,[8,9]);
int_op('<<',Version) -> is_version(Version,[8,9]);
int_op('>>',Version) -> is_version(Version,[8,9]);
int_op('>>?',Version) -> is_version(Version,[8,9]);
int_op('*s',Version) -> is_version(Version,[8,9]);
int_op('*u',Version) -> is_version(Version,[8,9]);
int_op('/s',Version) -> is_version(Version,[8,9]);
int_op('/u',Version) -> is_version(Version,[8,9]);
int_op('<<64',Version) -> is_version(Version,9);
int_op('>>64',Version) -> is_version(Version,9);
int_op('>>?64',Version) -> is_version(Version,9);
int_op('*64',Version) -> is_version(Version,9);
int_op('/s64',Version) -> is_version(Version,9);
int_op('/u64',Version) -> is_version(Version,9);
int_op(_,_) -> false.

is_version(Version,Version) -> true;
is_version(Version,VersionList) -> member(Version,VersionList).

member(Version,[Version|_]) -> true;
member(Version,[_|Versions]) -> member(Version,Versions);
member(_,[]) -> false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

report(String,Args) -> io:format(String,Args), io:format('~n',[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Pretty printer
%
% - pp/1: pretty prints Sparc code
% - pp_exit/1: same as pp/1, but exits if instruction could not be printed
%    (yields reason "{pp,BadInstr}")
%
%

%%pp(Sparc, FileName) ->


pp(Sparc) ->
   pp(Sparc, standard_io).

pp(Sparc, Dev) ->
   {M, F, A} = sparc_fun(Sparc),
   Fname = atom_to_list(M)++"_"++atom_to_list(F)++"_"++integer_to_list(A),
   io:format(Dev, ".section    \".text\"~n", []),
   io:format(Dev, "    .align 4~n", []),
   io:format(Dev, "    .global ", []),
   io:format(Dev, "~s~n", [Fname]),
   io:format(Dev, ".section    \".data\"\n", []),
   hipe_data_pp:pp(Dev, sparc_data(Sparc), sparc, Fname), 
   io:format(Dev, ".section    \".code\"\n", []),
   io:format(Dev, "~s:~n", [Fname]),
   pp_instrs(sparc_code(Sparc), Dev, Fname),
   io:format(Dev, "~n~n", []).

pp_instrs([], Dev, Fname) ->
   ok;
pp_instrs([I|Is], Dev, Fname) ->
   pp_instr(I, Dev, Fname),
   pp_instrs(Is, Dev, Fname).


pp_instr(I) ->
   pp_instr(I, standard_io, "").

pp_instr(I, Dev, Pre) ->
   case type(I) of
      label ->
       case info(I) of
	 [] -> 	   io:format(Dev, ".~s_~w:~n", [Pre, label_name(I)]);
	 Info ->
	   io:format(Dev, ".~s_~w: ! ~w~n", [Pre, label_name(I), Info])
       end;
      comment ->
	 io:format(Dev, "    ! ~p~n", [comment_text(I)]);
      nop ->
	 io:format(Dev, "    nop~n", []);
      move ->
	 io:format(Dev, "    mov ", []),
	 pp_arg(Dev, move_src(I)),
	 io:format(Dev, ", ", []),
	 pp_arg(Dev, move_dest(I)),
	 io:format(Dev, "~n", []);
      multimove ->
        Srcs = multimove_src(I),
        Dsts = multimove_dest(I),
        case length(Srcs) of
	  1 ->
	    io:format(Dev, "    mov ", []),
	    pp_arg(Dev, hd(Srcs)),
	    io:format(Dev, ", ", []),
	    pp_arg(Dev, hd(Dsts)),
	    io:format(Dev, " ! mmove ~n", []);
	  _ ->
	    io:format(Dev, "    ! Multimove   !~n",[]),
	    pp_mmoves(Dev, Srcs, Dsts),
	    io:format(Dev, "    ! End mmove   !~n",[])
	end;
%	 io:format(Dev, "    mov [", []),
%	 pp_args(Dev, multimove_src(I)),
%	 io:format(Dev, "], [", []),
%	 pp_args(Dev, multimove_dest(I)),
%	 io:format(Dev, "]~n", []);

      alu ->
	 io:format(Dev, "    ", []),
	 pp_alu_op(Dev, alu_operator(I)),
	 io:format(Dev, " ", []),
	 pp_arg(Dev, alu_src1(I)),
	 io:format(Dev, ", ", []),
	 pp_arg(Dev, alu_src2(I)),
	 io:format(Dev, ", ", []),
	 pp_arg(Dev, alu_dest(I)),
	 io:format(Dev, "~n", []);
      alu_cc ->
	 io:format(Dev, "    ", []),
	 pp_alu_op(Dev, alu_cc_operator(I)),
	 io:format(Dev, "cc ", []),
	 pp_arg(Dev, alu_cc_src1(I)),
	 io:format(Dev, ", ", []),
	 pp_arg(Dev, alu_cc_src2(I)),
	 io:format(Dev, ", ", []),
	 pp_arg(Dev, alu_cc_dest(I)),
	 io:format(Dev, "~n", []);
      br ->
	 io:format(Dev, "    br", []),
	 pp_regcc(Dev, br_regcond(I)),
	 io:format(Dev, " ", []),
	 pp_arg(Dev, br_reg(I)),
	 io:format(Dev, ", .~s_~w ! .~s_~w  pred: ~.2f~n", 
		   [Pre, br_true_label(I), Pre, br_false_label(I), 
		    br_pred(I)]);
      b ->
	 io:format(Dev, "    b", []),
	 pp_cc(Dev, b_cond(I)),
	 Pred = case b_taken(I) of
		   true -> "pt";
		   false -> "pn"
		end,
	 io:format(Dev, ",~s ", [Pred]),
	 io:format(Dev, "%icc, .~s_~w ! .~s_~w  pred: ~.2f~n", 
		   [Pre, b_true_label(I), Pre, b_false_label(I), b_pred(I)]);
      goto ->
	 io:format(Dev, "    ba .~s_~w~n", [Pre, goto_label(I)]);
      jmp ->
	 io:format(Dev, "    jmpl ", []),
	 pp_arg(Dev, jmp_target(I)),
	 io:format(Dev, "+", []),
	 pp_arg(Dev, jmp_off(I)),
	 io:format(Dev, ", %g0 ! (", []),
	 pp_args(Dev, jmp_args(I)),
	 io:format(Dev, ") ", []),
         case jmp_destinations(I) of
	   [] -> io:format(Dev, "~n", []);
	   Lbls -> pp_switch_labels(Dev,Lbls, Pre),
		   io:format(Dev, "~n", [])
	 end;
	   
      %% jmp_link ->
      call_link ->
	 io:format(Dev, "    call ", []),
         case call_link_type(I) of
	   closure ->
	     pp_arg(Dev,call_link_target(I)),
	     io:format(Dev, " ! (",[]);
	   _ ->
	     case call_link_target(I) of
	       {M, F, A} -> io:format(Dev, "~w_~w_~w ! (", [M, F, A]);
	       {F, A} -> io:format(Dev, "~w_~w ! (", [F, A]);
	       F -> io:format(Dev, "~w ! (", [F])
	     end
	 end,
	 pp_args(Dev, call_link_args(I)),
	 io:format(Dev, ")", []),
	 io:format(Dev, "[~w]",[call_link_type(I)]),
         case call_link_fail(I) of
	   [] -> true;
	   L ->
	     io:format(Dev, " fail_to ~s_~w", [Pre,L])
	 end,
         io:format(Dev, "~n", []),
         io:format(Dev, "    ba .~s_~w~n", [Pre,call_link_continuation(I)]);
      load ->
	 io:format(Dev, "    ", []),
	 pp_load_op(Dev, load_type(I)),
	 io:format(Dev, " [", []),
	 pp_arg(Dev, load_src(I)),
	 io:format(Dev, "+", []),
	 pp_arg(Dev, load_off(I)),
	 io:format(Dev, "], ", []),
	 pp_arg(Dev, load_dest(I)),
	 io:format(Dev, "~n", []);
      load_atom ->
         Atom = load_atom_atom(I),
%	 io:format(Dev, "    ! ", []),
%	 pp_arg(Dev, load_atom_dest(I)),
%	 io:format(Dev, " = \'~w\'~n", [load_atom_atom(I)]),
	 io:format(Dev, "    mov ~w, ", [hipe_bifs:atom_to_word(Atom)]),
	 pp_arg(Dev, load_atom_dest(I)),
	 io:format(Dev, " ! load_atom('~w') ~n", [Atom]);
      load_word_index ->
      	 io:format(Dev, "    mov word_index(~s_dl_~w, ~w), ", [Pre, load_word_index_block(I), load_word_index_index(I)]),
      	 pp_arg(Dev, load_word_index_dest(I)),
      	 io:format(Dev, "~n",[]);
      load_address ->
       io:format(Dev, "    lda ", []),
       Address = load_address_address(I),
       Type = load_address_type(I), 
       case Type of
	 function ->
	   case Address of
	     {M, F, A} -> io:format(Dev, "~w_~w_~w, ", [M, F, A]);
	     {F, A} -> io:format(Dev, "~w_~w, ", [F, A]);
	     F -> io:format(Dev, "~w, ", [F])
	   end;
	 constant ->
	    io:format(Dev, "~s_dl_~w, ", [Pre, Address]);
	 label ->
	   io:format(Dev, "~s_~w, ", [Pre, Address]);
	 'catch' ->
	   io:format(Dev, "~s_~w, ", [Pre, Address]);
	 closure ->
	   io:format(Dev, "~s_~w, ", [Pre, Address])
	 end,
	 pp_arg(Dev, load_address_dest(I)),
         case Type of
	   'catch' ->
	     io:format(Dev, " ! [catch]",[]);
	   'closure' ->
	     io:format(Dev, " ! [closure]",[]);
	   _ -> ok
	 end,
	 io:format(Dev, "~n", []);
      store ->
	 io:format(Dev, "    ", []),
	 pp_store_op(Dev, store_type(I)),
	 io:format(Dev, " ", []),
	 pp_arg(Dev, store_src(I)),
	 io:format(Dev, ", [", []),
	 pp_arg(Dev, store_dest(I)),
	 io:format(Dev, "+", []),
	 pp_arg(Dev, store_off(I)),
	 io:format(Dev, "]~n", []);
      sethi ->
	 io:format(Dev, "    sethi ", []),
	 pp_arg(Dev, sethi_const(I)),
	 io:format(Dev, ", ", []),
	 pp_arg(Dev, sethi_dest(I)),
	 io:format(Dev, "~n", []);
      X ->
	 exit({sparc, pp_instr, {"unknow instruction", X}})
   end.

pp_mmoves(Dev, [Src|Srcs], [Dst| Dsts]) ->
  io:format(Dev, "    mov ", []),
  pp_arg(Dev, Src),
  io:format(Dev, ", ", []),
  pp_arg(Dev, Dst),
  io:format(Dev, " !~n", []),
  pp_mmoves(Dev, Srcs, Dsts);
pp_mmoves(_,[],[]) -> ok.


pp_alu_op(Dev, Op) ->
    Str = case Op of
	      '+' -> "add";
	      '-' -> "sub";
	      '>>' -> "srl";
	      '>>64' -> "srlx";
	      '>>?' -> "sra";
	      '>>?64' -> "srax";
	      '<<' -> "sll";
	      '<<64' -> "sllx";
	      'and' -> "and";
	      'or' -> "or";
	      'xor' -> "xor";
	      '+c' -> "add_c";
	      '-c' -> "sub_c";
	      'andn' -> "andn";
	      'xnor' ->  "xnor";
	      X -> exit({sparc, {"unkown alu-op", X}}), ""
	  end,
    io:format(Dev, "~s", [Str]).

pp_load_op(Dev, Type) ->
    Str = case Type of
	      ub -> "ldub";
	      sb -> "ldsb";
	      uh -> "lduh";
	      sh -> "ldsh";
	      sw -> "ld";
	      uw -> "lduw";
	      xw -> "ldx"
	  end,
   io:format(Dev, "~s", [Str]).

pp_store_op(Dev, Type) ->
   Str = case Type of
	     ub -> "stb";
	     sb -> "stb";
	     uh -> "sth";
	     sh -> "sth";
	     sw -> "st";
	     uw -> "st";
	     xw -> "stx"
	 end,
   io:format(Dev, "~s", [Str]).

pp_regcc(Dev, CC) ->
   io:format(Dev, "~s", [CC]).

pp_cc(Dev, CC) ->
   io:format(Dev, "~s", [CC]).

pp_arg(Dev, Arg) ->
   case is_reg(Arg) of
      true ->
	 io:format(Dev, "~s", [hipe_sparc_registers:reg_name(reg_nr(Arg))]);
      false ->
	 case is_imm(Arg) of
	    true ->
	       io:format(Dev, "~w", [imm_value(Arg)]);
	    false ->
	       exit({sparc, {line, ?LINE}})
	 end
   end.


pp_args(Dev, []) ->
   ok;
pp_args(Dev, [A]) ->
   pp_arg(Dev, A);
pp_args(Dev, [A|As]) ->
   pp_arg(Dev, A),
   io:format(Dev, ", ", []),
   pp_args(Dev, As).


pp_switch_labels(Dev,Lbls, Pre) -> 
  pp_switch_labels(Dev,Lbls,1, Pre).

pp_switch_labels(Dev, [L], Pos, Pre) -> 
  io:format(Dev, "~s_~w", [Pre,L]);
pp_switch_labels(Dev, [L|Ls], Pos,Pre) -> 
  io:format(Dev, "~s_~w, ", [Pre,L]),
  NewPos = 
    case Pos of
      3 -> io:format(Dev, "\n             ! ",[]),
	   0;
      N -> N + 1
    end,
  pp_switch_labels(Dev, Ls, NewPos, Pre);
pp_switch_labels(Dev, [], _, _) -> ok.

%% ---------------------------------------------

highest_reg(Code) ->
  highest_reg(Code,0).
		      
highest_reg([I|Is],Max) ->
  Defs = defines(I),
  Uses = uses(I),
  highest_reg(Is,new_max(Defs++Uses,Max));
highest_reg([],Max) ->
  Max.

new_max([V|Vs],Max) ->
  VName = reg_nr(V),
  if VName > Max ->
      new_max(Vs, VName);
     true ->
      new_max(Vs, Max)
  end;
new_max([],Max) -> Max.

% ----------------------------------------

check_var_range(Sparc) ->
  Code = sparc_code(Sparc),
  RMax = highest_reg(Code),
  {Low, High} = sparc_var_range(Sparc),
  RMax =< High.
