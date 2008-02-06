%% -*- erlang-indent-level: 2 -*-
%% =====================================================================
%% Copyright (c) 1997 by the HiPE group.  All Rights Reserved 
%% =====================================================================
%%  Filename : 	hipe_sparc.erl
%%  Purpose  :  Provides primitives for handling SPARC instructions.
%%  Notes    : 
%% =====================================================================
%%
%% @doc Provides primitives for handling SPARC instructions.
%% <h3>Basic types</h3>
%% <ul> 
%%   <li>{@link sparc()}</li>
%%   <li>{@link sparc_instruction()}</li>
%%   <li>{@link mfa()}</li>
%%   <li>{@link fa()}</li>
%% </ul>
%% <h3>Basic instructions</h3>
%% <ul> 
%%   <li>{@link label()}</li>
%%   <li>{@link nop()}</li>
%%   <li>{@link comment()}</li>
%%   <li>{@link move()}</li>
%%   <li>{@link multimove()}</li>
%%   <li>{@link alu()}</li>
%%   <li>{@link alu_cc()}</li>
%%   <li>{@link sethi()}</li>
%%   <li>{@link load()}</li>
%%   <li>{@link store()}</li>
%%   <li>{@link b()}</li>
%%   <li>{@link goto()}</li>
%%   <li>{@link jmp_link()}</li>
%%   <li>{@link jmp()}</li>
%%   <li>{@link call_link()}</li>
%%   <li>{@link load_address()}</li>
%%   <li>{@link load_atom()}</li>
%%   <li>{@link load_word_index()}</li>
%% </ul>
%% <h3>Floating-point instructions</h3>
%% <ul> 
%%   <li>{@link load_fp()}</li>
%%   <li>{@link store_fp()}</li>
%%   <li>{@link fop()}</li>
%%   <li>{@link fmove()}</li>
%%   <li>{@link conv_fp()}</li>
%% </ul>
%% <h3>Pseudo instructions</h3>
%% <ul> 
%%   <li>{@link pseudo_return()}</li>
%%   <li>{@link pseudo_enter()}</li>
%%   <li>{@link pseudo_pop()}</li>
%%   <li>{@link pseudo_spill()}</li>
%%   <li>{@link pseudo_unspill()}</li>
%% </ul>
%% <h3>Field types</h3>
%% <ul> 
%%   <li>{@link operand()}</li>
%%   <li>{@link src()}</li>
%%   <li>{@link reg()}</li>
%%   <li>{@link fp_reg()}</li>
%%   <li>{@link imm()}</li>
%%   <li>{@link aluop()}</li>
%%   <li>{@link condition_code()}</li>
%%   <li>{@link label_name()}</li>
%%   <li>{@link load_type()}</li>
%%   <li>{@link store_type()}</li>
%%   <li>{@link prediction()}</li>
%%   <li>{@link annulbit()}</li>
%%   <li>{@link call_type()}</li>
%%   <li>{@link target()}</li>
%%   <li>{@link la_type()}</li>
%%   <li>{@link la_addr()}</li>
%%   <li>{@link fp_type()}</li>
%%   <li>{@link fp_cond()}</li>
%%   <li>{@link fp_op()}</li>
%% </ul>
%% @end
%% (Not currently used: pseudo_push(), align(), cmov_cc(), cmov_r(),
%% br(), fb(), fcmp(), fcc_reg().)
%%
%% Basic types:
%%
%% @type sparc().
%% A datastructure for SPARC code for one function. 
%% <p>Has the following properties
%% <ul> 
%%   <li>{@type FunctionName::mfa()} -- Name of the function.</li>
%%   <li><code>Arity::integer()</code> -- This is needed since the arity
%%   in the function name might differ from the real arity. </li>
%%   <li><code>IsClosure::bool()</code> -- True if this is the code for
%%   a closure.</li>
%%   <li><code>IsLeaf::bool()</code> -- True if this is a "leaf
%%   function". </li>
%%   <li><code>Code::[sparc_instruction()]</code> -- List of sparc
%%   instructions. </li>
%%   <li><code>Data::hipe_consttab:const_tab()</code> -- Data
%%   segment.</li>
%%   <li><code>VarRange::{integer(),integer()}</code> -- First and last
%%   name of reg/fp_reg.</li>
%%   <li><code>LabelRange::{integer(),integer()}</code> -- First and
%%   last labelname in the code.</li>
%% </ul>
%% </p> 
%% @type mfa()=hipe:mfa(). Name of a function in any module.
%% @type fa()={Function::atom(), Arity::integer()}. Name of a local function.
%% @type sparc_instruction() =
%%	         label()
%%            |  nop()
%%            |  comment() 
%%            |  move() 
%%            |  multimove() 
%%            |  alu()
%%            |  alu_cc()
%%            |  sethi()
%%            |  load()
%%            |  store()
%%            |  b()
%%            |  goto()
%%            |  jmp_link()
%%            |  jmp()
%%            |  call_link()
%%            |  load_address().
%%            |  load_atom()
%%            |  load_word_index()
%%            |  load_fp()
%%            |  store_fp()
%%            |  fop()
%%            |  fmove()
%%            |  conv_fp()
%%            |  pseudo_return()
%%            |  pseudo_enter() 
%%            |  pseudo_pop() 
%%            |  pseudo_spill() 
%%            |  pseudo_unspill() 
%% This datatype defines all the different SPARC instructions that
%% HiPE handles.
%% @end
%% (Not currently used: pseudo_push(), align(), cmov_cc(), cmov_r(),
%% br(), fb(), fcmp().)
%%
%% Instruction types:
%%
%% @type label(). See {@link label_create/1}.
%% @type nop(). See {@link nop_create/0}.
%% @type comment(). See {@link comment_create/1}.
%% @type move(). See {@link move_create/2}.
%% @type multimove(). See {@link multimove_create/2}.
%% @type alu(). See {@link alu_create/4}.
%% @type alu_cc(). See {@link alu_cc_create/4}.
%% @type sethi(). See {@link sethi_create/2}.
%% @type load(). See {@link load_create/3}.
%% @type store(). See {@link store_create/3}.
%% @type b(). See {@link b_create/5}.
%% @type goto(). See {@link goto_create/1}.
%% @type jmp_link(). See {@link jmp_link_create/4}.
%% @type jmp(). See {@link jmp_create/4}.
%% @type call_link(). See {@link call_link_create/5}.
%% @type load_address(). See {@link load_address_create/3}.
%% @type load_atom(). See {@link load_atom_create/2}.
%% @type load_word_index(). See {@link load_word_index_create/3}.
%% @end
%% (Not currently used: cmov_r(), cmov_cc(), br().)
%%
%% Floating-point instructions:
%%
%% @type load_fp(). See {@link load_fp_create/3}.
%% @type store_fp(). See {@link store_fp_create/3}.
%% @type fop(). See {@link fop_create/4}.
%% @type fmove(). See {@link fmove_create/2}.
%% @type conv_fp(). See {@link conv_fp_create/2}.
%% @end
%% (Not currently used: fb(), fcmp().)
%%
%% Pseudo instructions:
%%
%% @type pseudo_return(). See {@link pseudo_return_create/1}.
%% @type pseudo_enter(). See {@link pseudo_enter_create/2}.
%% @type pseudo_pop(). See {@link pseudo_pop_create/2}.
%% @type pseudo_spill(). See {@link pseudo_spill_create/2}.
%% @type pseudo_unspill(). See {@link pseudo_unspill_create/2}.
%% @end
%% (Not currently used: pseudo_push().)
%%
%% Field types:
%%
%% @type operand() = src() | fp_reg()
%% @type src() = reg() | imm()
%% @type reg() = {sparc_reg, Name::integer()}
%% @type fp_reg() = {sparc_fpreg, Name::integer()}
%% @end (Not used) @type fcc_reg() = 0 | 1 | 2 | 3  
%% @type imm() = {sparc_imm, Value::integer()}
%% @type aluop() = 
%%               '+' | '-' |  '+c' | '-c' | 'and' | 'andn' |  'or' 
%%             | 'xor' | 'xnor' | '>>' |  '>>64' |  '>>?' |  '>>?64'
%%             | '<<' | '<<64' 
%% @type condition_code() =
%%	       'a' | 'n' | 'ne' | 'e' | 'g' | 'le' | 'ge' | 'l' | 'gu'
%%           | 'leu' | 'geu' | 'lu' | 'pos' | 'neg' | 'vc' | 'vs'
%% @type label_name(). At the moment it is safe to assume this is an integer...
%% @type load_type() = uw | sb | sh | sw | ub | uh | x
%% @type store_type() = b | h | w  | x
%% @type prediction() = float(). The value must be between 0 and 1, inclusive.
%% @type annulbit() = a | na 
%% @type call_type() = closure | c
%% @type target() = reg() | mfa() | fa() | atom()
%% @type la_type() = function | constant | label | closure
%% @type la_addr() = mfa() | fa() | atom() | hipe_consttab:lbl() |
%%                                 label_name().
%%                                 This type is dependent on la_type().
%%                                 <ul>
%%                                 <li>la_type == function --  
%%                                        la_addr() =  mfa() | fa() | atom()</li>
%%                                 <li>la_type == constant --  
%%                                        la_addr() =  hipe_consttab:lbl()</li>
%%                                 <li>la_type == label --  
%%                                        la_addr() =  label_name()</li>
%%                                 <li>la_type == closure --  
%%                                        la_addr() =  label_name()</li>
%%                                 </ul>
%% @type fp_type() = single | double | quad
%% @type fp_cond() =  'a' | 'n' | 'u' | 'g' | 'ug' | 'l' | 'ul' | 'lg'
%%                                 | 'ne' | 'e' | 'ue' | 'ge' | 'uge'
%%                                 | 'le' | 'ule' | 'o' 
%% @type fp_op() = '+' | '-' | '*' 
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(hipe_sparc).

-export([mk_sparc/8, %% mk_sparc/6,
	 sparc_fun/1, sparc_arity/1, sparc_is_closure/1,
	 sparc_is_leaf/1, sparc_code/1, sparc_code_update/2, sparc_data/1,
	 %% sparc_data_update/2, sparc_var_range/1,
	 sparc_var_range_update/2,
	 %% sparc_label_range/1,
	 sparc_label_range_update/2 %%, sparc_size/1
	]).
-export([store_create/3, store_create/4, store_dest/1, store_dest_update/2,
	 store_off/1, store_off_update/2, store_src/1, store_src_update/2,
	 store_type/1 %% store_type_update/2
	]).
-export([jmp_link_create/4,
	 jmp_link_off/1, %% jmp_link_off_update/2,
	 jmp_link_target/1, %% jmp_link_target_update/2,
	 jmp_link_args/1, %% jmp_link_args_update/2,
	 jmp_link_link/1, %% jmp_link_link_update/2,
	 %% jmp_create/3,
	 jmp_create/4,
	 jmp_off/1, %% jmp_off_update/2,
	 jmp_target/1, %% jmp_target_update/2,
	 jmp_args/1, %% jmp_args_update/2,
	 jmp_destinations/1, %% jmp_destinations_update/2,
	 call_link_create/6, call_link_create/7,
	 call_link_target/1, call_link_target_update/2,
	 call_link_link/1, %% call_link_link_update/2,
	 call_link_args/1, %% call_link_args_update/2,
	 call_link_dests/1,
	 call_link_continuation/1, call_link_continuation_update/2,
	 call_link_fail/1, call_link_fail_update/2,
	 call_link_type/1,
	 call_link_is_known/1,
	 call_link_stack_desc/1, call_link_stack_desc_update/2,
	 label_create/1, label_create_new/0,
	 label_name/1, %% label_name_update/2,
	 load_atom_create/2,
	 load_atom_dest/1, %% load_atom_dest_update/2,
	 load_atom_atom/1,
	 load_word_index_create/3,
	 load_word_index_dest/1, %% load_word_index_dest_update/2,
	 load_word_index_index/1, load_word_index_block/1,
	 load_address_create/3,
	 load_address_addr/1, load_address_addr_update/2,
	 load_address_type/1, %% load_address_type_update/2,
	 load_address_dest/1, %% load_address_dest_update/2,
 	 load_create/3, load_create/4,
	 load_dest/1, %% load_dest_update/2,
	 load_off/1, load_off_update/2,
	 load_src/1, load_src_update/2,
	 load_type/1, %% load_type_update/2,
	 move_create/2, move_dest/1, move_dest_update/2,
	 move_src/1, %% move_src_update/2,
	 multimove_create/2, multimove_dest/1, %% multimove_dest_update/2,
	 multimove_src/1, %% multimove_src_update/2,
	 nop_create/0,
	 rdy_create/1, rdy_dest/1,
 	 sethi_const/1, %% sethi_const_update/2,
	 sethi_dest/1, %% sethi_dest_update/2,
	 sethi_create/2]).
-export([load_fp_create/3,
	 load_fp_create/5,
	 load_fp_dest/1, %% load_fp_dest_update/2,
	 load_fp_align/1,
	 load_fp_off/1, %% load_fp_off_update/2,
	 load_fp_src/1, %% load_fp_src_update/2,
	 load_fp_type/1 %% load_fp_type_update/2
	]).
-export([store_fp_create/3,
	 store_fp_create/5,
	 store_fp_dest/1, %% store_fp_dest_update/2,
	 store_fp_off/1,  %% store_fp_off_update/2,
	 store_fp_align/1,
	 store_fp_src/1,  %% store_fp_src_update/2,
	 store_fp_type/1  %% store_fp_type_update/2
	]).

-export([%% fb_create/7,
	 %% fb_create/6,
	 %% fb_annul/1,
	 %% fb_annul_update/2,
	 %% fb_cond/1,
	 %% fb_cond_update/2,
	 %% fb_fcc_reg/1,
	 %% fb_fcc_reg_update/2,
	 %% fb_label/1,
	 %% fb_label_update/2,
	 %% fb_true_label/1,
	 %% fb_true_label_update/2,
	 %% fb_false_label/1,
	 %% fb_false_label_update/2,
	 %% fb_pred/1,
	 %% fb_pred_update/2,
	 %% fb_taken/1
	]).

-export([fop_create/4,
	 %% fop_create/5,
	 fop_dest/1,
	 %% fop_dest_update/2,
	 fop_type/1,
	 %% fop_type_update/2,
	 fop_operator/1,
	 %% fop_operator_update/2,
	 fop_src1/1,
	 %% fop_src1_update/2,
	 fop_src2/1
	 %% fop_src2_update/2
	]).

-export([%% fcmp_create/3,
	 %% fcmp_create/6,
	 %% fcmp_fcc_reg/1,
	 %% fcmp_type/1,
	 %% fcmp_src1/1,
	 %% fcmp_src2/1,
	 %% fcmp_exception/1
	 %% fcmp_fcc_reg_update/2,
	 %% fcmp_type_update/2,
	 %% fcmp_src1_update/2,
	 %% fcmp_src2_update/2,
	 %% fcmp_exception_update/2
	]).

-export([fmove_create/2,
	 fmove_create/5,
	 fmove_dest/1,
	 fmove_dest_update/2,
	 fmove_type/1,
	 %% fmove_type_update/2,
	 fmove_src/1,
	 %% fmove_src_update/2,
	 fmove_negate/1,	 
	 fmove_abs/1]).

-export([conv_fp_create/2,
	 %% conv_fp_create/3,
	 conv_fp_dest/1,
	 conv_fp_dest_type/1,
	 conv_fp_src/1
	 %% conv_fp_dest_update/2,
	 %% conv_fp_dest_type_update/2,
	 %% conv_fp_src_update/2
	]).


-export([pseudo_return_create/1, %% pseudo_return_create/2,
	 pseudo_return_regs/1, %% pseudo_return_regs_update/2,
	 pseudo_enter_create/3,
	 pseudo_enter_target/1, %% pseudo_enter_target_update/2,
	 pseudo_enter_type/1,
	 pseudo_enter_is_known/1,
	 pseudo_enter_args/1, %% pseudo_enter_args_update/2,
	 pseudo_spill_create/2, %% pseudo_spill_create/3,
	 pseudo_spill_reg/1,
	 pseudo_spill_pos/1,
	 pseudo_unspill_create/2, %% pseudo_unspill_create/3,
	 pseudo_unspill_reg/1, pseudo_unspill_pos/1,
	 %% pseudo_push_create/1, pseudo_push_create/2,
	 % pseudo_push_reg/1, pseudo_push_reg_update/2,
	 pseudo_pop_create/2, %% pseudo_pop_create/3,
	 pseudo_pop_reg/1, pseudo_pop_index/1 %%, pseudo_pop_reg_update/2
	]).
-export([%%align_alignment/1, align_alignment_update/2, align_create/2,
	 alu_cc_create/4, alu_cc_dest/1, %% alu_cc_dest_update/2,
	 alu_cc_operator/1, %% alu_cc_operator_update/2,
	 alu_cc_src1/1, alu_cc_src1_update/2,
	 alu_cc_src2/1, alu_cc_src2_update/2,
	 alu_create/4, alu_dest/1, %% alu_dest_update/2,
	 alu_operator/1, %% alu_operator_update/2,
	 alu_src1/1, alu_src1_update/2, alu_src2/1, alu_src2_update/2, 
	 b_create/5,
	 b_annul/1, %% b_annul_update/2,
	 b_cond/1, %% b_cond_update/2,
	 b_label/1, b_label_update/2, b_true_label/1, %% b_true_label_update/2,
	 b_false_label/1, b_false_label_update/2, b_pred/1, %% b_pred_update/2,
	 b_taken/1,
	 %% br_create/7,
	 %% br_annul/1, %% br_annul_update/2,
	 %% br_label/1, %% br_label_update/2,
	 %% br_true_label/1, %% br_true_label_update/2,
	 %% br_false_label/1, br_false_label_update/2,
	 %% br_pred/1, %% br_pred_update/2,
	 %% br_taken/1,
	 %% br_reg/1, %% br_reg_update/2,
	 %% br_regcond/1, %% br_regcond_update/2,
	 goto_create/1, goto_label/1, goto_label_update/2, is_goto/1,
	 %% cmov_cc_cond/1, cmov_cc_cond_update/2, cmov_cc_create/4,
	 %% cmov_cc_dest/1, cmov_cc_dest_update/2,
	 %% cmov_cc_src/1, cmov_cc_src_update/2,
	 %% cmov_r_create/4,
	 %% cmov_r_dest/1, cmov_r_dest_update/2, cmov_r_reg/1,
	 %% cmov_r_reg_update/2, cmov_r_regcond/1, cmov_r_regcond_update/2,
	 %% cmov_r_src/1, cmov_r_src_update/2,
	 comment_create/1,
	 comment_text/1, %% comment_text_update/2,
	 %% is_align/1, is_alu/1, is_alu_cc/1,
	 %% is_any_alu/1,
	 is_any_branch/1, has_delayslot/1,
	 %% is_any_cmov/1, is_any_memop/1,
	 %% is_b/1, is_br/1,
	 is_call_link/1, %% is_cmov_cc/1, is_cmov_r/1,
	 is_comment/1, %% is_jmp/1, is_jmp_link/1,
	 is_label/1, is_load/1, is_move/1, 
	 %% is_multimove/1, is_nop/1,
	 %% is_sethi/1, is_load_fp/1,
	 %% is_store_fp/1, is_fb/1, is_fop/1, is_fcmp/1,
	 is_fmove/1, is_conv_fp/1, is_store/1]).
-export([redirect_jmp/3, cc_negate/1, %% fcc_negate/1,
	 uses/1, defines/1, def_use/1,
	 %% all_uses/1,
	 imm_uses/1,
	 fp_reg_uses/1, fp_reg_defines/1,
	 %% fpreg_and_regs_defines/1,
	 %% fpregs_and_regs_def_use/1, fpregs_and_regs_uses/1,
	 subst/2, subst_uses/2, subst_defines/2, keep_registers/1,
	 keep_fp_registers/1 %%, fp_reg_def_use/1
	]).
-export([mk_reg/1, mk_new_reg/0, is_reg/1, reg_nr/1, mk_fpreg/1,
	 mk_new_fpreg/0, is_fpreg/1, fpreg_nr/1, mk_imm/1,
	 is_imm/1, imm_value/1, %% mk_spill/1,
	 is_spill/1, spill_pos/1]).

%%-define(DO_ASSERT,true).
-include("../main/hipe.hrl").
-include("hipe_sparc_sdesc.hrl").
-include("hipe_sparc.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Constructors
%%

%% @spec mk_sparc(Fun::mfa(), Arity::integer(), Closure::bool(), 
%%                               Leaf::bool(), Code::[sparc_instruction()] , 
%%                               Data::hipe_consttab:const_tab(), 
%%                               VarRange::{integer(),integer()} ,
%%                               LabelRange::{integer(),integer()} 
%%                               ) -> sparc()
mk_sparc(Fun, Arity, Closure, Leaf, Code, Data, VarRange, LabelRange) ->
  #sparc{'fun'=Fun, 
	 arity=Arity, closure=Closure, leaf=Leaf,
	 code=Code, data=Data, var_range=VarRange, label_range=LabelRange}.
%% %% @spec mk_sparc(Fun::mfa(), Arity::integer(), 
%% %%                            Code::[sparc_instruction()] , 
%% %%                            Data::hipe_consttab:const_tab(), 
%% %%                            VarRange::{integer(),integer()} ,
%% %%                            LabelRange::{integer(),integer()} 
%% %%                               ) -> sparc()
%% mk_sparc(Fun, Arity, Code, Data, VarRange, LabelRange) ->
%%   #sparc{'fun'=Fun, 
%% 	 arity=Arity, closure=false, leaf=false,
%% 	 code=Code, data=Data, var_range=VarRange, label_range=LabelRange}.

%% @spec sparc_fun(Sparc::sparc()) -> mfa()
sparc_fun(#sparc{'fun'=Fun}) -> Fun.
%% @spec sparc_arity(Sparc::sparc()) -> integer()
sparc_arity(#sparc{arity=Arity}) when is_integer(Arity), 0 =< Arity -> Arity.
%% @spec (Sparc::sparc()) -> bool()
sparc_is_closure(#sparc{closure=IsClosure}) -> IsClosure.
%% @spec (Sparc::sparc()) -> bool()
sparc_is_leaf(#sparc{leaf=IsLeaf}) -> IsLeaf.
%% @spec (Sparc::sparc()) -> [sparc_instruction()]
sparc_code(#sparc{code=Code}) -> Code.
%% @spec (Sparc::sparc(),Code::[sparc_instruction()]) -> sparc()
sparc_code_update(Sparc, NewCode) -> Sparc#sparc{code=NewCode}.
%% @spec (Sparc::sparc()) -> hipe_consttab:const_tab()
sparc_data(#sparc{data=Data}) -> Data.
%% %% @spec (Sparc::sparc(),Data::hipe_consttab:const_tab()) -> sparc()
%% sparc_data_update(Sparc, NewData) -> Sparc#sparc{data=NewData}.
-ifdef(DO_ASSERT).
%% @spec (Sparc::sparc()) -> {integer(),integer()} 
sparc_var_range(#sparc{var_range=VarRange}) -> VarRange.
-endif.
%% @clear
%% @spec (Sparc::sparc(), NewRange::{integer(),integer()}) -> sparc()
sparc_var_range_update(Sparc, NewRange) -> 
  NewSparc = Sparc#sparc{var_range=NewRange},
  ?ASSERT(check_var_range(NewSparc)),
  NewSparc.
%% %% @spec (Sparc::sparc()) -> {integer(),integer()}
%% sparc_label_range(#sparc{label_range=LabelRange}) -> LabelRange.
%% @spec (Sparc::sparc(), NewRange::{integer(),integer()}) -> sparc()
sparc_label_range_update(Sparc, NewRange) -> Sparc#sparc{label_range=NewRange}.
%% %% @spec (Sparc::sparc()) -> integer()
%% %% @doc Returns the number of instructions in the SPARC code.
%% %% <strong> Note: This is not guaranteed to correspond to the actual
%% %% memory footprint of the SPARC code.</strong> 
%% sparc_size(Sparc) ->
%%   RealIns = fun(I) -> not(is_label(I) or is_comment(I)) end,
%%   length(lists:filter(RealIns, sparc_code(Sparc))).


%% Enter - pseudo op %
%% @spec  (Target::target(), Args::[reg()], Type::atom()) -> pseudo_enter()
pseudo_enter_create(Target, Args, Type) ->
  case Type of
    remote -> ok;
    not_remote -> ok
  end,
  #pseudo_enter{target=Target, args=Args, type=Type}.
pseudo_enter_args(#pseudo_enter{args=Args}) ->
  Args.
pseudo_enter_args_update(I,Args) ->
  I#pseudo_enter{args=Args}.
pseudo_enter_target(#pseudo_enter{target=Target}) ->
  Target.
pseudo_enter_target_update(I,Target) ->
  I#pseudo_enter{target=Target}.
pseudo_enter_type(#pseudo_enter{type=Type}) ->
  Type.
pseudo_enter_is_known(I) ->
  not is_reg(pseudo_enter_target(I)).


%% Return - pseudo op %
pseudo_return_create(Regs) ->
  #pseudo_return{regs=Regs}.
pseudo_return_regs(#pseudo_return{regs=Regs}) ->
  Regs.
pseudo_return_regs_update(I,Regs) ->
  I#pseudo_return{regs=Regs}.


%% Push - pseudo op %
%% pseudo_push_create(Reg) ->
%%   #pseudo_push{reg=Reg}.
%% pseudo_push_reg(#pseudo_push{reg=Reg}) ->
%%   Reg.
%% pseudo_push_reg_update(I,Reg) ->
%%   I#pseudo_push{reg=Reg}.

%% Pop - pseudo op %
pseudo_pop_create(Reg, Index) ->
  #pseudo_pop{reg=Reg, index=Index}.
pseudo_pop_reg(#pseudo_pop{reg=Reg}) ->
  Reg.
pseudo_pop_index(#pseudo_pop{index=Index}) ->
  Index.
pseudo_pop_reg_update(I,Reg) ->
  I#pseudo_pop{reg=Reg}.

%% spill - pseudo op %
pseudo_spill_create(Reg1, Pos) ->
  #pseudo_spill{source=Reg1, dest=Pos}.
pseudo_spill_reg(#pseudo_spill{source=Src}) ->
  Src.
pseudo_spill_pos(#pseudo_spill{dest=Dst}) ->
  Dst.
pseudo_spill_reg_update(I,Reg) ->
  I#pseudo_spill{source=Reg}.

%% unspill - pseudo op %
pseudo_unspill_create(Reg1, Pos) ->
  #pseudo_unspill{source=Reg1, dest=Pos}.
pseudo_unspill_reg(#pseudo_unspill{source=Src}) ->
  Src.
pseudo_unspill_pos(#pseudo_unspill{dest=Dst}) ->
  Dst.
pseudo_unspill_reg_update(I,Reg) ->
  I#pseudo_unspill{source=Reg}.


%% Load atom %%
%% @spec (Dest::reg(),Atom::atom()) -> load_atom()
load_atom_create(Dest,Atom) -> 
  ?ASSERT(is_reg(Dest)),
  #load_atom{dst=Dest,atom=Atom}.
load_atom_dest(#load_atom{dst=Dst}) -> Dst.
load_atom_atom(#load_atom{atom=Atom}) -> Atom.
load_atom_dest_update(LA, NewDest) -> LA#load_atom{dst=NewDest}.

%% @spec (Dest::reg(),Block::term(),Index::integer()) -> load_word_index()
load_word_index_create(Dst,Block,Index) -> 
  ?ASSERT(is_reg(Dest)),
  #load_word_index{dst=Dst, block=Block, index=Index}.
load_word_index_dest(#load_word_index{dst=Dst}) -> Dst.
load_word_index_dest_update(LA, NewDest) -> LA#load_word_index{dst=NewDest}.
load_word_index_block(#load_word_index{block=Block}) -> Block.
load_word_index_index(#load_word_index{index=Index}) -> Index.

%%
%% Load address
%%
%% @spec (Dst::reg(),Addr::la_addr(), Type::la_type()) -> load_address()
load_address_create(Dst, Addr, Type) -> 
  #load_address{dst=Dst, addr=Addr, type=Type}.
load_address_dest(#load_address{dst=Dst}) -> Dst.
load_address_dest_update(LA, NewDst) -> LA#load_address{dst=NewDst}.
load_address_addr(#load_address{addr=Addr}) -> Addr.
load_address_addr_update(LoadAddress, NewA) -> LoadAddress#load_address{addr=NewA}.
load_address_type(#load_address{type=Type}) -> Type.
%% load_address_type_update(LA, NewType) -> LA#load_address{type=NewType}.


%% 
%% label
%%
%% @spec (Name::label_name()) -> label()
label_create(Name) -> #label{id=Name}.
%% @spec () -> label()
label_create_new() -> label_create(hipe_gensym:get_next_label(sparc)).
%% @spec (label()) -> label_name()
label_name(#label{id=Name}) -> Name.
%% %% @spec (label(),label_name()) -> label()
%% label_name_update(Label,NewName) -> Label#label{id=NewName}.
%% @spec (sparc_instruction()) -> bool()
is_label(Insn) -> case Insn of #label{} -> true; _ -> false end.


%%
%% nop
%%
%% @spec () -> nop()
nop_create() -> #nop{}.
%% %% @spec (sparc_instruction()) -> bool()
%% is_nop(Insn) -> case Insn of #nop{} -> true; _ -> false end.


%% %% Align %%
%% %% @spec (integer()) -> align()
%% align_create(Number) -> #align{alignment=Number}.
%% %% @spec (align()) -> integer()
%% align_alignment(#align{alignment=Number}) -> Number.
%% %% @spec (align(),integer()) -> align()
%% align_alignment_update(Align,NewNumber) -> Align#align{alignment=NewNumber}.
%% %% @spec (sparc_instruction()) -> bool()
%% is_align(Insn) -> case Insn of #align{} -> true; _ -> false end.

%% Comment %%
%% @spec (Text::string()) -> comment()
comment_create(Text) -> #comment{text=Text}.
%% @spec   (comment()) -> string()
comment_text(#comment{text=Text}) -> Text.
%% %% @spec   (comment(), Text::string()) -> comment()
%% comment_text_update(Comment, NewText) -> Comment#comment{text=NewText}.
%% @spec (sparc_instruction()) -> bool()
is_comment(Insn) -> case Insn of #comment{} -> true; _ -> false end.

%% Move %%
%% @spec (Dst::reg(), Src::src())  -> move()
move_create(Dst, Src) -> #move{dst=Dst, src=Src}.
move_src(#move{src=Src}) -> Src.
move_src_update(Move, NewSource) -> Move#move{src=NewSource}.
move_dest(#move{dst=Dst}) -> Dst.
move_dest_update(Move, NewDest) -> Move#move{dst=NewDest}.
%% @spec (sparc_instruction()) -> bool()
is_move(Insn) -> case Insn of #move{} -> true; _ -> false end.

%% MultiMove %%
%% @spec (Dsts::[reg()], Srcs::[reg()])  -> multimove()
multimove_create(Dest, Source) ->
  ?ASSERT(length(Dest) =:= length(Source)),
  #multimove{dst=Dest, src=Source}.
multimove_dest(#multimove{dst=DstList}) -> DstList.
multimove_dest_update(Move, NewDest) -> Move#multimove{dst=NewDest}.
multimove_src(#multimove{src=SrcList}) -> SrcList.
multimove_src_update(Move, NewSource) -> Move#multimove{src=NewSource}.
%% %% @spec (sparc_instruction()) -> bool()
%% is_multimove(Insn) -> case Insn of #multimove{} -> true; _ -> false end.

%% %% Cmov_cc %%
%% %% cmov_cc_create(Dest,Source,Cond) -> 
%%   #cmov_cc{dst=Dest,src=Source,cc=Cond}.
%% cmov_cc_dest(#cmov_cc{dst=Dst}) -> Dst.
%% cmov_cc_src(#cmov_cc{src=Src}) -> Src.
%% cmov_cc_cond(#cmov_cc{cc=Cond}) -> Cond.
%% cmov_cc_dest_update(CmovCc,NewDest) -> CmovCc#cmov_cc{dst=NewDest}.
%% cmov_cc_src_update(CmovCc,NewSource) -> CmovCc#cmov_cc{src=NewSource}.
%% cmov_cc_cond_update(CmovCc,NewCond) -> CmovCc#cmov_cc{cc=NewCond}.

%% %% Cmov_r %%
%% %% cmov_r_create(Dest,Source,Reg,RegCond) -> 
%%   #cmov_r{dst=Dest,src=Source,rcc=RegCond,reg=Reg}.
%% cmov_r_dest(#cmov_r{dst=Dst}) -> Dst.
%% cmov_r_src(#cmov_r{src=Src}) -> Src.
%% cmov_r_reg(#cmov_r{reg=Reg}) -> Reg.
%% cmov_r_regcond(#cmov_r{rcc=RegCond}) -> RegCond.
%% cmov_r_dest_update(CmovR,NewDest) -> CmovR#cmov_r{dst=NewDest}.
%% cmov_r_src_update(CmovR,NewSource) -> CmovR#cmov_r{src=NewSource}.
%% cmov_r_reg_update(CmovR,NewReg) -> CmovR#cmov_r{reg=NewReg}.
%% cmov_r_regcond_update(CmovR,NewRegCond) -> CmovR#cmov_r{rcc=NewRegCond}.
%% %% @spec (sparc_instruction()) -> bool()
%% is_cmov_r(Insn) -> case Insn of #cmov_r{} -> true; _ -> false end.

%% Alu %%
alu_create(Dest,Src1,Op,Src2) -> 
  #alu{dst=Dest,src1=Src1,src2=Src2,op=Op}.
alu_dest(#alu{dst=Dst}) -> Dst.
alu_src1(#alu{src1=Src1}) -> Src1.
alu_operator(#alu{op=Op}) -> Op.
alu_src2(#alu{src2=Src2}) -> Src2.
alu_dest_update(Alu,NewDest) -> Alu#alu{dst=NewDest}.
alu_src1_update(Alu,NewSource1) -> Alu#alu{src1=NewSource1}.
%% alu_operator_update(Alu,NewOp) -> Alu#alu{op=NewOp}.
alu_src2_update(Alu,NewSource2) -> Alu#alu{src2=NewSource2}.
%% %% @spec (sparc_instruction()) -> bool()
%% is_alu(Insn) -> case Insn of #alu{} -> true; _ -> false end.

%% Alu_cc %%
alu_cc_create(Dest,Src1,Op,Src2) -> 
  #alu_cc{dst=Dest,src1=Src1,src2=Src2,op=Op}.
alu_cc_dest(#alu_cc{dst=Dst}) -> Dst.
alu_cc_src1(#alu_cc{src1=Src1}) -> Src1.
alu_cc_operator(#alu_cc{op=Op}) -> Op.
alu_cc_src2(#alu_cc{src2=Src2}) -> Src2.
alu_cc_dest_update(Alu,NewDest) -> Alu#alu_cc{dst=NewDest}.
alu_cc_src1_update(Alu,NewSource1) -> Alu#alu_cc{src1=NewSource1}.
%% alu_cc_operator_update(Alu,NewOp) -> Alu#alu_cc{op=NewOp}.
alu_cc_src2_update(Alu,NewSource2) -> Alu#alu_cc{src2=NewSource2}.
%% %% @spec (sparc_instruction()) -> bool()
%% is_alu_cc(Insn) -> case Insn of #alu_cc{} -> true; _ -> false end.

%% rd %y,Rd %%
rdy_create(Dest) -> #rdy{dst=Dest}.
rdy_dest(#rdy{dst=Dst}) -> Dst.
rdy_dest_update(Rdy,NewDest) -> Rdy#rdy{dst=NewDest}.

%% Sethi %%
sethi_create(Dest,Const) -> #sethi{dst=Dest,const=Const}.
sethi_dest(#sethi{dst=Dst}) -> Dst.
sethi_const(#sethi{const=Const}) -> Const.
sethi_dest_update(SetHi,NewDest) -> SetHi#sethi{dst=NewDest}.
%% sethi_const_update(SetHi,NewConst) -> SetHi#sethi{const=NewConst}.

%% Load %%
load_create(Dest,Source,Off) -> 
  #load{dst=Dest,type=uw,src=Source,off=Off}.
load_create(Dest,Type,Source,Off) -> 
  ?ASSERT((Type =:= uw) orelse (Type =:= ub) orelse (Type =:= uh) orelse
	  (Type =:= sw) orelse (Type =:= sb) orelse (Type =:= sh) orelse
	  (Type =:= x)),
  #load{dst=Dest,type=Type,src=Source,off=Off}.
load_dest(#load{dst=Dst}) -> Dst.
load_type(#load{type=Type}) -> Type.
load_src(#load{src=Src}) -> Src.
load_off(#load{off=Offset}) -> Offset.
load_dest_update(Load,NewDest) -> Load#load{dst=NewDest}.
%% load_type_update(Load,NewType) -> Load#load{type=NewType}.
load_src_update(Load,NewSource) -> Load#load{src=NewSource}.
load_off_update(Load,NewOff) -> Load#load{off=NewOff}.
%% @spec (sparc_instruction()) -> bool()
is_load(Insn) -> case Insn of #load{} -> true; _ -> false end.

%% Store %%
store_create(Target,Off,Source) -> 
  #store{dst=Target,type=w,src=Source,off=Off}.
store_create(Target,Off,Type,Source) -> 
  ?ASSERT((Type =:= w) orelse (Type =:= b) orelse (Type =:= h) orelse
	  (Type =:= x)),
  #store{dst=Target,type=Type,src=Source,off=Off}.
store_dest(#store{dst=Dst}) -> Dst.
store_off(#store{off=Offset}) -> Offset.
store_type(#store{type=Type}) -> Type.
store_src(#store{src=Src}) -> Src.
store_dest_update(Store,NewTarget) -> Store#store{dst=NewTarget}.
store_off_update(Store,NewOff) -> Store#store{off=NewOff}.
%% store_type_update(Store,NewType) -> Store#store{type=NewType}.
store_src_update(Store,NewSource) -> Store#store{src=NewSource}.
%% @spec (sparc_instruction()) -> bool()
is_store(Insn) -> case Insn of #store{} -> true; _ -> false end.

%% B %%
b_create(Cond,TrueLabel,FalseLabel,Pred,Annul) -> 
  #b{cc=Cond,true_label=TrueLabel,false_label=FalseLabel, pred=Pred,
     annul=Annul}.
b_cond(#b{cc=Cond}) -> Cond.
%% b_cond_update(B,NewCond) -> B#b{cc=NewCond}.
b_label(B) -> b_true_label(B).
b_label_update(B,NewLabel) -> b_true_label_update(B,NewLabel).
b_true_label(#b{true_label=TrueLabel}) -> TrueLabel.
b_true_label_update(B,NewLabel) -> B#b{true_label=NewLabel}.
b_false_label(#b{false_label=FalseLabel}) -> FalseLabel.
b_false_label_update(B,NewLabel) -> B#b{false_label=NewLabel}.
b_pred(#b{pred=Pred}) -> Pred.
%% b_pred_update(B,NewPred) -> B#b{pred=NewPred}.
%% b_annul_update(B,NewAnnul) -> B#b{annul=NewAnnul}.
b_annul(#b{annul=Annul}) -> Annul.
b_taken(B) -> P = b_pred(B), if P > 0.5 -> true; true -> false end.
%% %% @spec (sparc_instruction()) -> bool()
%% is_b(Insn) -> case Insn of #b{} -> true; _ -> false end.

%% Br %%
%% br_create(Reg,RegCond,TrueLabel,FalseLabel,Pred,Annul) -> 
%%   #br{reg=Reg,rcc=RegCond,true_label=TrueLabel,false_label=FalseLabel,
%%       pred=Pred,annul=Annul}.
%% br_reg(#br{reg=Reg}) -> Reg.
%% br_reg_update(Br,NewReg) -> Br#br{reg=NewReg}.
%% br_regcond(#br{rcc=RegCond}) -> RegCond.
%% br_regcond_update(Br,NewRegCond) -> Br#br{rcc=NewRegCond}.
%% br_label(Br) -> br_true_label(Br).
%% br_label_update(Br,NewLabel) -> br_true_label_update(Br,NewLabel).
%% br_true_label(#br{true_label=TrueLabel}) -> TrueLabel.
%% br_true_label_update(Br,NewLabel) -> Br#br{true_label=NewLabel}.
%% br_false_label(#br{false_label=FalseLabel}) -> FalseLabel.
%% br_false_label_update(Br,NewLabel) -> Br#br{false_label=NewLabel}.
%% br_pred(#br{pred=Pred}) -> Pred.
%% br_pred_update(Br,NewPred) -> Br#br{pred=NewPred}.
%% br_annul(#br{annul=Annul}) -> Annul.
%% br_annul_update(Br,NewAnnul) -> Br#br{annul=NewAnnul}.
%% br_taken(Br) -> P = br_pred(Br), if P > 0.5 -> true; true -> false end.
%% %% @spec (sparc_instruction()) -> bool()
%% is_br(Insn) -> case Insn of #br{} -> true; _ -> false end.


%% Goto %%
goto_create(Label) -> #goto{label=Label}.
goto_label(#goto{label=Label}) -> Label.
goto_label_update(Goto, NewLabel) -> Goto#goto{label=NewLabel}.
%% @spec (sparc_instruction()) -> bool()
is_goto(I) -> case I of #goto{} -> true; _ -> false end.


%% Jmp_link %%
jmp_link_create(Target, Offset, Link, Args) -> 
  #jmp_link{target=Target, off=Offset, link=Link, args=Args}.
jmp_link_target(#jmp_link{target=Target}) -> Target.
jmp_link_target_update(JmpLink,NewTarget) -> 
  JmpLink#jmp_link{target=NewTarget}.
jmp_link_off(#jmp_link{off=Offset}) -> Offset.
jmp_link_off_update(JmpLink,NewOffset) -> JmpLink#jmp_link{off=NewOffset}.
jmp_link_link(#jmp_link{link=Link}) -> Link.
jmp_link_link_update(JmpLink,NewLink) -> JmpLink#jmp_link{link=NewLink}.
jmp_link_args(#jmp_link{args=Args}) -> Args.
%% jmp_link_args_update(JmpLink,NewArgs) -> JmpLink#jmp_link{args=NewArgs}.
%% %% @spec (sparc_instruction()) -> bool()
%% is_jmp_link(Insn) -> case Insn of #jmp_link{} -> true; _ -> false end.

%% Jmp %%
% jmp_create(Target, Offset, Args) -> 
%   #jmp{target=Target, off=Offset, args=Args}.
jmp_create(Target, Offset, Args, Destinations) -> 
  #jmp{target=Target, off=Offset, args=Args, 
       destinations=Destinations}.
jmp_target(#jmp{target=Target}) -> Target.
jmp_target_update(Jmp,NewTarget) -> Jmp#jmp{target=NewTarget}.
jmp_off(#jmp{off=Offset}) -> Offset.
jmp_off_update(Jmp,NewOffset) -> Jmp#jmp{off=NewOffset}.
jmp_args(#jmp{args=Args}) -> Args.
%% jmp_args_update(Jmp,NewArgs) -> Jmp#jmp{args=NewArgs}.
jmp_destinations(#jmp{destinations=Dests}) -> Dests.
jmp_destinations_update(Jmp,NewDests) -> Jmp#jmp{destinations=NewDests}.

%% Call_link %%
call_link_create(Target,Link,Args,Cont,Fail,Type) ->
  call_link_create(Target,Link,
		   %% TODO: Fix this when we have multiple retvals.
		   [mk_reg(hipe_sparc_registers:ret(0))],
		   Args,Cont,Fail,Type).
call_link_create(Target,Link,Dests,Args,Cont,Fail,Type) ->
  case Type of
    remote -> ok;
    not_remote -> ok
  end,
  #call_link{target=Target, link=Link, 
	     dests=Dests,
	     args=Args, 
	     type=Type,
	     continuation_label=Cont, fail_label=Fail,
	     stack_descriptor=sdesc_mk_empty()}.
call_link_target(#call_link{target=Target}) -> Target.
call_link_target_update(CL,NewTarget) -> CL#call_link{target=NewTarget}.
call_link_link(#call_link{link=Link}) -> Link.
call_link_link_update(CL,NewLink) -> CL#call_link{link=NewLink}.
call_link_args(#call_link{args=Args}) -> Args.
call_link_dests(#call_link{dests=Dests}) -> Dests.
call_link_args_update(CL,NewArgs) -> CL#call_link{args=NewArgs}.
call_link_continuation(#call_link{continuation_label=Cont}) -> Cont.
call_link_continuation_update(CL,NewC) -> CL#call_link{continuation_label=NewC}.
call_link_fail(#call_link{fail_label=Fail}) -> Fail.
call_link_fail_update(CL,NewF) -> 
  CL#call_link{fail_label=NewF}.
call_link_type(#call_link{type=Type}) -> Type.
call_link_stack_desc(#call_link{stack_descriptor=SDesc}) -> SDesc.
call_link_stack_desc_update(I,NewSD) -> I#call_link{stack_descriptor=NewSD}.
%% @spec (sparc_instruction()) -> bool()
is_call_link(Insn) -> case Insn of #call_link{} -> true; _ -> false end.
call_link_is_known(I) ->
  not is_reg(call_link_target(I)).

%%--------------------------------------------------------------------------------
%% FP - instructions
%%--------------------------------------------------------------------------------

%% Load float
%% Type is one of {single, double, quad}

%% Standard use is double with 32 bit alignment...
load_fp_create(Dest,Source,Off) -> 
  load_fp_create(Dest,32,double,Source,Off).
load_fp_create(Dest,Align,Type,Source,Off) -> 
  ?ASSERT(is_fpreg(Dest)),
  ?ASSERT((is_reg(Off) orelse is_imm(Off))),
  ?ASSERT(is_reg(Source)),
  ?ASSERT(is_fptype(Type)),
  #load_fp{dst=Dest,align=Align,type=Type,src=Source,off=Off}.
load_fp_dest(#load_fp{dst=Dst}) -> Dst.
load_fp_align(#load_fp{align=Align}) -> Align.
load_fp_type(#load_fp{type=Type}) -> Type.
load_fp_src(#load_fp{src=Src}) -> Src.
load_fp_off(#load_fp{off=Offset}) -> Offset.
load_fp_dest_update(Load,NewDest) -> Load#load_fp{dst=NewDest}.
%% load_fp_type_update(Load,NewType) -> Load#load_fp{type=NewType}.
load_fp_src_update(Load,NewSource) -> Load#load_fp{src=NewSource}.
load_fp_off_update(Load,NewOff) -> Load#load_fp{off=NewOff}.
%% %% @spec (sparc_instruction()) -> bool()
%% is_load_fp(Insn) -> case Insn of #load_fp{} -> true; _ -> false end.

%% Store %%
%% Type is one of {single, double, quad}

%% Standard use is double with 32 bit alignment...
store_fp_create(Target,Off,Source) -> 
  store_fp_create(Target,Off,double,32,Source).
store_fp_create(Target,Off,Type,Align,Source) -> 
  ?ASSERT(is_reg(Target)),
  ?ASSERT((is_reg(Off) orelse is_imm(Off))),
  ?ASSERT(is_fpreg(Source)),
  ?ASSERT(is_fptype(Type)),
  #store_fp{dst=Target,type=Type,align=Align,src=Source,off=Off}.
store_fp_dest(#store_fp{dst=Dst}) -> Dst.
store_fp_off(#store_fp{off=Offset}) -> Offset.
store_fp_type(#store_fp{type=Type}) -> Type.
store_fp_align(#store_fp{align=Align}) -> Align.
store_fp_src(#store_fp{src=Src}) -> Src.
store_fp_dest_update(Store,NewTarget) -> Store#store_fp{dst=NewTarget}.
store_fp_off_update(Store,NewOff) -> Store#store_fp{off=NewOff}.
%% store_fp_type_update(Store,NewType) -> Store#store_fp{type=NewType}.
store_fp_src_update(Store,NewSource) -> Store#store_fp{src=NewSource}.
%% %% @spec (sparc_instruction()) -> bool()
%% is_store_fp(Insn) -> case Insn of #store_fp{} -> true; _ -> false end.


%% fb %%
%% fb_create(Cond,N,TrueLabel,FalseLabel,Pred,Annul) ->
%%   ?ASSERT(is_fp_cond(Cond)),
%%   ?ASSERT(is_fcc(N)),
%%   ?ASSERT(Pred >= 0 andalso Pred =< 1),
%%   ?ASSERT(Annul =:= a orelse Annul =:= na),
%%   #fb{fcc=Cond,n=N,
%%       true_label=TrueLabel,
%%       false_label=FalseLabel, 
%%       pred=Pred,
%%       annul=Annul}.
%% fb_create(Cond,TrueLabel,FalseLabel,Pred,Annul) -> 
%%   fb_create(Cond,0,TrueLabel,FalseLabel,Pred,Annul).
%% fb_cond(#fb{fcc=Cond}) -> Cond.
%% fb_cond_update(B,NewCond) -> B#fb{fcc=NewCond}.
%% fb_fcc_reg(#fb{n=N}) -> N.
%% fb_fcc_reg_update(B,NewN) -> B#fb{n=NewN}.
%% fb_label(B) -> fb_true_label(B).
%% fb_label_update(B,NewLabel) -> fb_true_label_update(B,NewLabel).
%% fb_true_label(#fb{true_label=TrueLabel}) -> TrueLabel.
%% fb_true_label_update(B,NewLabel) -> B#fb{true_label=NewLabel}.
%% fb_false_label(#fb{false_label=FalseLabel}) -> FalseLabel.
%% fb_false_label_update(B,NewLabel) -> B#fb{false_label=NewLabel}.
%% fb_pred(#fb{pred=Pred}) -> Pred.
%% fb_pred_update(B,NewPred) -> B#fb{pred=NewPred}.
%% fb_annul_update(B,NewAnnul) -> B#fb{annul=NewAnnul}.
%% fb_annul(#fb{annul=Annul}) -> Annul.
%% fb_taken(B) -> P = b_pred(B), if P > 0.5 -> true; true -> false end.
%% %% @spec (sparc_instruction()) -> bool()
%% is_fb(Insn) -> case Insn of #fb{} -> true; _ -> false end.


%% Floating point op %%
fop_create(Dest,Src1,Op,Src2) -> 
  fop_create(Dest,double,Src1,Op,Src2).
fop_create(Dest,Type,Src1,Op,Src2) -> 
  ?ASSERT(is_fpreg(Dest)),
  ?ASSERT(is_fpreg(Src1)),
  ?ASSERT(is_fpreg(Src2)),
  ?ASSERT(is_fptype(Type)),
  ?ASSERT(is_fop_op(Op)),
  #fop{dst=Dest,type=Type,src1=Src1,src2=Src2,fop=Op}.
fop_dest(#fop{dst=Dst}) -> Dst.
fop_type(#fop{type=Type}) -> Type.
fop_src1(#fop{src1=Src1}) -> Src1.
fop_operator(#fop{fop=Op}) -> Op.
fop_src2(#fop{src2=Src2}) -> Src2.
fop_dest_update(Fop,NewDest) -> Fop#fop{dst=NewDest}.
%% fop_type_update(Fop,NewType) -> Fop#fop{type=NewType}.
fop_src1_update(Fop,NewSource1) -> Fop#fop{src1=NewSource1}.
%% fop_operator_update(Fop,NewOp) -> Fop#fop{fop=NewOp}.
fop_src2_update(Fop,NewSource2) -> Fop#fop{src2=NewSource2}.
%% %% @spec (sparc_instruction()) -> bool()
%% is_fop(Insn) -> case Insn of #fop{} -> true; _ -> false end.

  
%% Floating point compare %%
%% fcmp_create(Fcc,Src1,Src2) -> 
%%  fcmp_create(Fcc,double,Src1,Src2,false,[]).
%% fcmp_create(Fcc,Type,Src1,Src2,Exception) -> 
%%   ?ASSERT(is_fcc(Fcc)),
%%   ?ASSERT(is_fpreg(Src1)),
%%   ?ASSERT(is_fpreg(Src2)),
%%   ?ASSERT(is_fptype(Type)),
%%   ?ASSERT(is_bool(Exception)),
%%   #fcmp{fccn=Fcc,type=Type,src1=Src1,src2=Src2,exception=Exception}.
%% fcmp_fcc_reg(#fcmp{fccn=Fcc}) -> Fcc.
%% fcmp_type(#fcmp{type=Type}) -> Type.
%% fcmp_src1(#fcmp{src1=Src1}) -> Src1.
%% fcmp_src2(#fcmp{src2=Src2}) -> Src2.
%% fcmp_exception(#fcmp{exception=Exception}) -> Exception.
%% fcmp_fcc_reg_update(Fcmp,NewN) -> Fcmp#fcmp{fccn=NewN}.
%% fcmp_type_update(Fcmp,NewType) -> Fcmp#fcmp{type=NewType}.
%% fcmp_src1_update(Fcmp,NewSource1) -> Fcmp#fcmp{src1=NewSource1}.
%% fcmp_src2_update(Fcmp,NewSource2) -> Fcmp#fcmp{src2=NewSource2}.
%% fcmp_exception_update(Fcmp,NewOp) -> Fcmp#fcmp{exception=NewOp}.
%% %% @spec (sparc_instruction()) -> bool()
%% is_fcmp(Insn) -> case Insn of #fcmp{} -> true; _ -> false end.
      

%% Floating point move %%
fmove_create(Dest,Source) -> 
  fmove_create(Dest,double,Source,false,false).
fmove_create(Dest,Type,Source,Negate,Abs) -> 
  ?ASSERT(is_fpreg(Dest)),
  ?ASSERT(is_fpreg(Source) orelse is_spill(Source)),
  ?ASSERT(is_fptype(Type)),
  ?ASSERT(Negate=true orelse Negate=false),
  ?ASSERT(Abs=true orelse Abs=false),
  #fmove{dst=Dest,type=Type,src=Source,negate=Negate,abs=Abs}.
fmove_dest(#fmove{dst=Dst}) -> Dst.
fmove_type(#fmove{type=Type}) -> Type.
fmove_src(#fmove{src=Src}) -> Src.
fmove_negate(#fmove{negate=Negate}) -> Negate.
fmove_abs(#fmove{abs=Abs}) -> Abs.
fmove_dest_update(FM,NewDest) -> FM#fmove{dst=NewDest}.
%% fmove_type_update(FM,NewType) -> FM#fmove{type=NewType}.
fmove_src_update(FM,NewSource) -> FM#fmove{src=NewSource}.
%% @spec (sparc_instruction()) -> bool()
is_fmove(Insn) -> case Insn of #fmove{} -> true; _ -> false end.

%% Convert from fixnum to float %%
%% Standard use is double
conv_fp_create(Target,Source) -> 
  conv_fp_create(Target,double,Source).
conv_fp_create(Target,DstType,Source) -> 
  ?ASSERT(is_fpreg(Target)),
  ?ASSERT(is_fpreg(Source) orelse is_reg(Source)),
  ?ASSERT(is_fptype(DstType)),
  #conv_fp{dst=Target,dst_type=DstType,src=Source}.
conv_fp_dest(#conv_fp{dst=Dst}) -> Dst.
conv_fp_dest_type(#conv_fp{dst_type=DstType}) -> DstType.
conv_fp_src(#conv_fp{src=Src}) -> Src.
conv_fp_dest_update(Conv,NewTarget) -> Conv#conv_fp{dst=NewTarget}.
%% conv_fp_dest_type_update(Conv,NewType) -> Conv#conv_fp{dst_type=NewType}.
conv_fp_src_update(Conv,NewSource) -> Conv#conv_fp{src=NewSource}.
%% @spec (sparc_instruction()) -> bool()
is_conv_fp(Insn) -> case Insn of #conv_fp{} -> true; _ -> false end.



%% ____________________________________________________________________
%% 

%% ASSERTS
-ifdef(DO_ASSERT).
is_fptype(Type) ->
  (Type =:= single) 
    orelse 
      (Type =:= double) 
    orelse 
      (Type =:= quad).

is_fcc(N) ->
  (is_integer(N) andalso N >= 0 andalso N =< 3).

is_fop_op(Op) ->
  case Op of
    '+' -> true;
    '-' -> true;
    '*' -> true;
    '/' -> true;
    _   -> false
  end.

is_fp_cond(FCC) ->
  case FCC of 
    'a'	->  true;
    'n'	->  true;
    'u' ->  true;
    'g'	->  true;
    'ug' -> true;
    'l'	->  true;
    'ul' -> true;
    'lg' -> true;
    'ne' -> true;
    'e'	->  true;
    'ue' -> true;
    'ge' -> true;
    'uge'-> true;
    'le' -> true;
    'ule'-> true;
    'o'	->  true;
    _  -> false
  end.

is_bool(true) ->  true;
is_bool(false) -> true;
is_bool(_) ->     false.

check_var_range(Sparc) ->
  Code = sparc_code(Sparc),
  RMax = highest_reg(Code),
  {Low, High} = sparc_var_range(Sparc),
  RMax =< High.

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

-endif.	%% DO_ASSERT

%% ____________________________________________________________________
%% 

%% Integer regs %%
%% @spec (Name::integer) -> reg()
mk_reg(RegNr) when is_integer(RegNr) -> {sparc_reg, RegNr}.
%% @spec () -> reg()
mk_new_reg() -> mk_reg(hipe_gensym:get_next_var(sparc)).
%% @spec (operand()) -> bool()
is_reg(I) -> case I of {sparc_reg, _} -> true ; _ -> false end.
%% @spec (reg(RegNr)) -> RegNr
%%  RegNr = integer() 
reg_nr({sparc_reg, RegNr}) when is_integer(RegNr) -> RegNr.

%% FP regs %%
%% @spec (Name::integer) -> fp_reg()
mk_fpreg(RegNr) when is_integer(RegNr) -> {sparc_fpreg, RegNr}.
%% @spec () -> fp_reg()
mk_new_fpreg() -> mk_fpreg(hipe_gensym:get_next_var(sparc)).
%% @spec (operand()) -> bool()
is_fpreg(I) -> case I of {sparc_fpreg, _} -> true; _ -> false end.
%% @spec (fp_reg(Name)) -> Name
%%  Name = integer() 
fpreg_nr({sparc_fpreg, RegNr}) when is_integer(RegNr) -> RegNr.

%% Immediates %%
%% @spec (Value::integer()) -> imm()
mk_imm(Value) when is_integer(Value) -> {sparc_imm, Value}.
%% @spec (operand()) -> bool()
is_imm(I) -> case I of {sparc_imm, _} -> true; _ -> false end.
%% @spec (imm(Value)) -> Value
%% Value = integer()
imm_value({sparc_imm, Value}) when is_integer(Value) -> Value.

%% Spill locations
%% mk_spill(Pos) -> {spill, Pos}.
is_spill({spill,_}) -> true;
is_spill(_) -> false.
spill_pos({spill, Pos}) -> Pos.
  
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Predicates
%%

%% %% @spec (sparc_instruction()) -> bool()
%% is_cmov_cc(Insn) -> case Insn of #cmov_cc{} -> true; _ -> false end.
%% %% @spec (sparc_instruction()) -> bool()
%% is_sethi(Insn) -> case Insn of #sethi{} -> true; _ -> false end.
%% %% @spec (sparc_instruction()) -> bool()
%% is_jmp(Insn) -> case Insn of #jmp{} -> true; _ -> false end.
%% is_load_atom(Insn) -> case Insn of #load_atom{} -> true; _ -> false end.
%% is_load_address(Insn) -> case Insn of #load_address{} -> true; _ -> false end.

%% %% @spec (sparc_instruction()) -> bool()
%% is_any_alu(Insn) ->
%%   case Insn of
%%     #alu{} -> true;
%%     #alu_cc{} ->true;
%%     _ -> false
%%   end.

%% %% @spec (sparc_instruction()) -> bool()
%% is_any_cmov(Insn) ->
%%   case Insn of
%%     #cmov_cc{} -> true;
%%     #cmov_r{} -> true;
%%     _ -> false
%%   end.

%% %% @spec (sparc_instruction()) -> bool()
%% is_any_memop(Insn) -> 
%%   case Insn of
%%     #load{} -> true;
%%     #store{} -> true;
%%     #load_fp{} -> true;
%%     #store_fp{} -> true;
%%     _ -> false
%%   end.

%% These instrs are branches that need delayslot filling.
%% @spec (sparc_instruction()) -> bool()
is_any_branch(Insn) -> 
  case Insn of
    #b{} -> true;
%%  #br{} -> true;
    #goto{} -> true;
    #jmp{} -> true;
    #jmp_link{} -> true;
    #call_link{} -> case call_link_continuation(Insn) of
		      [] -> false;
		      _ -> true
		    end;
    #pseudo_return{} -> true;
    #pseudo_enter{} -> true;
%%  #fb{} -> true;
    _ -> false
  end.

%% These instrs are branches that need delayslot filling.
%% @spec (sparc_instruction()) -> bool()
has_delayslot(Insn) -> 
  case Insn of
    #b{} -> true;
%%  #br -> true;
    #goto{} -> true;
    #jmp{} -> true;
    #jmp_link{} -> true;
    #call_link{} -> true;
    #pseudo_return{} -> true;
    #pseudo_enter{} -> true;
%%  #fb{} -> true;
    _ -> false
  end.

%% @spec (I::sparc_instruction(), Old::label_name(),  New::label_name()) -> sparc_instruction()
%% @doc Changes the target of a jump.
%% Replaces any references to the target label Old by the label New.
redirect_jmp(Jmp, ToOld, ToNew) ->
  case Jmp of
%%  #br{} ->
%%       case br_true_label(Jmp) of
%% 	ToOld ->
%% 	  br_true_label_update(Jmp, ToNew);
%% 	_ ->
%% 	  case br_false_label(Jmp) of
%% 	    ToOld ->
%% 	      br_false_label_update(Jmp, ToNew);
%% 	    _ ->
%% 	      Jmp
%% 	  end
%%       end;
    #b{} ->
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
    #goto{} ->
      case goto_label(Jmp) of
	ToOld ->
	  goto_label_update(Jmp, ToNew);
	_ ->
	  Jmp
      end;
    #jmp{} -> 
      NewDests = replace(ToOld, ToNew, jmp_destinations(Jmp)),
      jmp_destinations_update(Jmp, NewDests);
    #call_link{} ->
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
%%  #fb{} ->
%%       case fb_true_label(Jmp) of
%% 	ToOld ->
%% 	  fb_true_label_update(Jmp, ToNew);
%% 	_ ->
%% 	  case fb_false_label(Jmp) of
%% 	    ToOld ->
%% 	      fb_false_label_update(Jmp, ToNew);
%% 	    _ ->
%% 	      Jmp
%% 	  end
%%       end;
    _ ->
      Jmp
  end.

replace(Old, New, List) ->
  [if X =:= Old ->
       New; 
      true -> X
   end || X <- List].
		    
%% @spec (condition_code()) -> condition_code()
%% @doc Returns the negation of an integer condition code.
cc_negate(Cond) ->
  case Cond of
    'a'	-> 'n';
    'n'	-> 'a';
    'ne'-> 'e';
    'e'	-> 'ne';
    'g'	-> 'le';
    'le' -> 'g';
    'ge' -> 'l';
    'l'	-> 'ge';
    'gu' -> 'leu';
    'leu' -> 'gu';
    'geu' -> 'lu';
    'lu'  -> 'geu';
    'pos' -> 'neg';
    'neg' -> 'pos';
    'vc' ->  'vs';
    'vs' ->  'vc'
  end.


%% %% @spec (fp_cond()) -> fp_cond()
%% %% @doc Returns the negation of an fp condition code.
%% fcc_negate(Cond) ->
%%  case Cond of
%%    'a'	->  'n';
%%    'n'	->  'a';
%%    'u'	->  'o';
%%    'g'	->  'ule';
%%    'ug' -> 'le';
%%    'l'	->  'uge';
%%    'ul' -> 'ge';
%%    'lg' -> 'ue';
%%    'ne' -> 'e';
%%    'e'	->  'ne';
%%    'ue' -> 'lg';
%%    'ge' -> 'ul';
%%    'uge'-> 'l';
%%    'le' -> 'ug';
%%    'ule'-> 'g';
%%    'o'	->  'u'
%%   end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Def/Use by instruction.
%%
%% The condition code registers is read and written like any other. 
%% We define a _virtual_ for icc and xcc.
%%
%% @spec () -> reg()
icc_reg() -> mk_reg(hipe_sparc_registers:icc()).
%% @spec () -> reg()
xcc_reg() -> mk_reg(hipe_sparc_registers:xcc()).
%% %% @spec (N::fcc_reg()) -> reg()
%% fcc_reg(N) -> mk_reg(hipe_sparc_registers:fcc(N)).
y_reg() -> mk_reg(hipe_sparc_registers:y()).


%% %% @spec (sparc_instruction()) -> [reg()]
%% %% @doc Returns a list of both floating point and integer registers
%% %%  that are used by an instruction.
%% fpregs_and_regs_uses(Instr) -> 
%%   remove_immediates(all_uses(Instr)).


%% @spec (sparc_instruction()) -> [reg()]
%% @doc Returns a list of integer registers that are used by an instruction.
uses(Instr) -> 
  %% keep_registers(all_uses(Instr)).
  remove_immediates(all_uses(Instr)).

%% @spec (sparc_instruction()) -> [operand()]
%% @doc Returns a list of operands that are used by an instruction
all_uses(Ins) ->
  case Ins of
    #label{} -> [];
    #nop{} -> [];
%%  #align{} -> [];
    #comment{} -> [];
    #move{} -> [move_src(Ins)];
    #multimove{} -> multimove_src(Ins);
%%  #cmov_cc{} -> [icc_reg(), xcc_reg(), cmov_cc_src(Ins)];
%%  #cmov_r{} -> [cmov_r_src(Ins), cmov_r_reg(Ins)];
    #alu{} -> [alu_src1(Ins), alu_src2(Ins)];
    #alu_cc{} -> [alu_cc_src1(Ins), alu_cc_src2(Ins)];
    #rdy{} -> [y_reg()];
    #sethi{} -> [sethi_const(Ins)];
    #load{} -> [load_src(Ins), load_off(Ins)];
    #store{} -> [store_dest(Ins), store_src(Ins), store_off(Ins)];
    #b{} -> [icc_reg(), xcc_reg()];
%%  #br{} -> [br_reg(Ins)];
    #goto{} -> [];
    #jmp_link{} ->
      [jmp_link_target(Ins), jmp_link_off(Ins) | jmp_link_args(Ins)];
    #jmp{} -> [jmp_target(Ins), jmp_off(Ins) | jmp_args(Ins)];
    #call_link{} -> 
      case call_link_is_known(Ins) of
	false ->
	  [call_link_target(Ins)|call_link_args(Ins)];
	true ->
	  call_link_args(Ins)
      end;
    #load_atom{} -> [];
    #load_word_index{} -> [];
    #pseudo_return{} -> pseudo_return_regs(Ins);
    #pseudo_enter{} -> 
      case pseudo_enter_is_known(Ins) of
	false ->
	  [pseudo_enter_target(Ins)|
	   pseudo_enter_args(Ins)];
	true ->
	  pseudo_enter_args(Ins)
      end;
    #pseudo_spill{} -> [pseudo_spill_reg(Ins)];
    #pseudo_unspill{} -> [];
%%  #pseudo_push{} -> [pseudo_push_reg(Ins)];
    #pseudo_pop{} -> [];

    #load_fp{} -> [load_fp_off(Ins), load_fp_src(Ins)];
    #store_fp{} ->
      case store_fp_type(Ins) of
	single -> [store_fp_dest(Ins), store_fp_off(Ins),store_fp_src(Ins)];
	_ ->  [store_fp_dest(Ins), store_fp_off(Ins)| 
	       format_fpreg([store_fp_src(Ins)])]
      end;
%%  #fb{} -> [fcc_reg(fb_fcc_reg(Ins))];
    #fop{} -> format_fpreg([fop_src1(Ins), fop_src2(Ins)]);
%%  #fcmp{} -> format_fpreg([fcmp_src1(Ins), fcmp_src2(Ins)]);
    #fmove{} -> format_fpreg([fmove_src(Ins)]);
    #conv_fp{} -> format_fpreg([conv_fp_src(Ins)]);
    #load_address{} -> []
  end.

%% @spec (sparc_instruction()) -> [imm()]
%% @doc Returns a list of immediates that are used by an instruction
imm_uses(Instr) ->
  keep_imms(all_uses(Instr)).

%% @spec (sparc_instruction()) -> [fp_reg()]
%% @doc Returns a list of fp-registers that are used by an instruction
fp_reg_uses(Instr) -> 
  keep_fp_registers(all_uses(Instr)).


%% @spec (sparc_instruction()) -> [reg()]
%% @doc Returns a list of registers that are defined by an instruction
defines(Ins) ->
  %% keep_registers(all_defines(Ins)).
  remove_immediates(all_defines(Ins)).
%% @spec (sparc_instruction()) -> [fp_reg()]
%% @doc Returns a list of fp-registers that are defined by an instruction
fp_reg_defines(Ins) ->
  keep_fp_registers(all_defines(Ins)).

%% fpregs_and_regs_defines(Ins) ->
%%   keep_imms(all_defines(Ins)).

%% @spec (sparc_instruction()) -> [operand()]
%% @doc Returns a list of operands that are defined by an instruction
all_defines(Ins)->
  case Ins of
    #label{} -> [];
    #nop{} -> [];
%%  #align{} -> [];
    #comment{} -> [];
    #move{} -> [move_dest(Ins)];
    #multimove{} -> multimove_dest(Ins);
%%  #cmov_cc{} -> [cmov_cc_dest(Ins)];
%%  #cmov_r{} -> [cmov_r_dest(Ins)];
    #alu{} -> [alu_dest(Ins)];
    #alu_cc{} -> [icc_reg(), xcc_reg(), alu_cc_dest(Ins)];
    #rdy{} -> [rdy_dest(Ins)];
    #sethi{} -> [sethi_dest(Ins)];
    #load{} -> [load_dest(Ins)];
    #store{} -> [];
    #b{} -> [];
%%  #br{} -> [];
    #goto{} -> [];
    #jmp_link{} -> [jmp_link_link(Ins)];
    #jmp{} -> [];
    #call_link{} -> 
      [mk_reg(hipe_sparc_registers:ret(0)), %% For exceptions.
       call_link_link(Ins)|
       call_link_dests(Ins)]; 
    #load_atom{} -> [load_atom_dest(Ins)];
    #load_word_index{} -> [load_word_index_dest(Ins)];
    #pseudo_return{} -> [];
    #pseudo_enter{} -> [];
%%  #pseudo_push{} -> [];
    #pseudo_pop{} -> [pseudo_pop_reg(Ins)];
    #pseudo_spill{} -> [];
    #pseudo_unspill{} -> [pseudo_unspill_reg(Ins)];
    #load_fp{} -> 
      case load_fp_type(Ins) of
	single ->[load_fp_dest(Ins)];
	_ -> format_fpreg([load_fp_dest(Ins)])
      end;
    #store_fp{} -> [];
%%  #fb{} -> [];
    #fop{} -> format_fpreg([fop_dest(Ins)]);
%%  #fcmp{} -> [fcc_reg(fcmp_fcc_reg(Ins))];
    #fmove{} -> format_fpreg([fmove_dest(Ins)]);
    #conv_fp{} -> format_fpreg([conv_fp_dest(Ins)]);
    #load_address{} -> [load_address_dest(Ins)]
  end.


%% @spec (sparc_instruction()) -> {[operand()],[operand()]}
%% @doc Returns a tuple of defs and uses in instruction.
%% Totally redundant, but we need it for speed.
%%
all_def_uses(I) ->
  case I of
    #label{} -> {[],[]};
    #nop{} -> {[],[]};
%%  #align{} -> {[],[]};
    #comment{} ->  {[],[]};
    #move{} -> 
	{[move_dest(I)],
	 [move_src(I)]};
    #multimove{} -> 
	{multimove_dest(I),
	 multimove_src(I)};
%%  #cmov_cc{} ->
%% 	{[cmov_cc_dest(I)],
%% 	 [icc_reg(), xcc_reg(), cmov_cc_src(I)]};
%%       cmov_r ->
%% 	{[cmov_r_dest(I)],
%% 	 [cmov_r_src(I), cmov_r_reg(I)]};
    #alu{} ->
	{[alu_dest(I)],
	 [alu_src1(I), alu_src2(I)]};
    #alu_cc{} ->
	{[icc_reg(), xcc_reg(), alu_cc_dest(I)],
	 [alu_cc_src1(I), alu_cc_src2(I)]};
    #rdy{} ->
	{[rdy_dest(I)],
	 [y_reg()]};
    #sethi{} ->
	{[sethi_dest(I)], 
	 [sethi_const(I)]};
    #load{} -> 
	{[load_dest(I)],
	 [load_src(I), load_off(I)]};
    #store{} ->
	{[],
	 [store_dest(I), store_src(I), store_off(I)]};
    #b{} ->
	{[],
	 [icc_reg(), xcc_reg()]};
%%  #br{} ->
%% 	{[],
%% 	 [br_reg(I)]};
    #goto{} -> {[],[]};
    #jmp_link{} ->
	{[jmp_link_link(I)],
	 [jmp_link_target(I), jmp_link_off(I) | jmp_link_args(I)]};
    #jmp{} ->
	{[],
	 [jmp_target(I), jmp_off(I) | jmp_args(I)]};
    #call_link{} ->
	{[call_link_link(I)|call_link_dests(I)],
	 case call_link_is_known(I) of
	   false -> 
	     [call_link_target(I)|call_link_args(I)];
	   true -> call_link_args(I)
	 end};
    #load_atom{} ->
	{[load_atom_dest(I)],
	 []};
    #load_word_index{} ->
	{[load_word_index_dest(I)],
	 []};
    #pseudo_return{} ->
	{[],
	 pseudo_return_regs(I)};
    #pseudo_enter{} ->
	{[],
	 case pseudo_enter_is_known(I) of
	   false ->
	     [pseudo_enter_target(I)|
	      pseudo_enter_args(I)];
	   true ->
	     pseudo_enter_args(I)
	 end};
    #pseudo_spill{} ->
	{[],
	 [pseudo_spill_reg(I)]};
    #pseudo_unspill{} ->
	{[pseudo_unspill_reg(I)],
	 []};
%   #pseudo_push{} ->
% 	{[],
% 	 [pseudo_push_reg(I)]};
    #pseudo_pop{} ->
	{[pseudo_pop_reg(I)],
	 []};
    #load_fp{} ->
	case load_fp_type(I) of
	  single ->
	    {[load_fp_dest(I)],
	     [load_fp_src(I), load_fp_off(I)]};
	  _ ->
	    {format_fpreg([load_fp_dest(I)]),
	     [load_fp_src(I), load_fp_off(I)]}
	end;
    #store_fp{} ->
	case store_fp_type(I) of
	  single ->
	    {[],
	     [store_fp_src(I), store_fp_off(I), store_fp_dest(I)]};
	  _ ->
	    {[],
	     [store_fp_off(I), store_fp_dest(I) |
	      format_fpreg([store_fp_src(I)])]}
	end;
%%  #fb{} ->
%% 	{[],
%% 	 [fcc_reg(fb_fcc_reg(I))]};
    #fop{} ->
	{format_fpreg([fop_dest(I)]),
	 format_fpreg([fop_src1(I), fop_src2(I)])}; 
%%  fcmp{} -> 
%% 	{[fcc_reg(fcmp_fcc_reg(I))],
%% 	 format_fpreg([fcmp_src1(I), fcmp_src2(I)])};
    #fmove{} -> 
	{format_fpreg([fmove_dest(I)]),
	 format_fpreg([fmove_src(I)])};
    #conv_fp{} -> 
	{format_fpreg([conv_fp_dest(I)]),
	 [conv_fp_src(I)]};
    #load_address{} -> 
	{[load_address_dest(I)],
	 []}
  end.

%% @spec (sparc_instruction()) -> {[reg()],[reg()]}
%% @doc Returns a tuple of reg-defs and reg-uses in instruction.
%% Totally redundant, but we need it for speed.
def_use(I) ->
  {Def,Use} = all_def_uses(I),
  {keep_registers(Def), keep_registers(Use)}.

%% %% @spec (sparc_instruction()) -> {[fp_reg()],[fp_reg()]}
%% %% @doc Returns a tuple of fp-reg-defs and fp-reg-uses in instruction.
%% %% Totally redundant, but we need it for speed.
%% fp_reg_def_use(I) ->
%%   {Def,Use} = all_def_uses(I),
%%   {keep_fp_registers(Def), keep_fp_registers(Use)}.

%% fpregs_and_regs_def_use(I) ->
%%   {Def,Use} = all_def_uses(I),
%%   {remove_immediates(Def), remove_immediates(Use)}.

%%
%% Makes sure the single precision registers that makes up a double 
%% precision one is marked as defines and uses.
%%

format_fpreg(Reg)->
  format_fpreg(Reg, []).

format_fpreg([H|T], Acc)->
  case is_fpreg(H) of
    true ->
      RegNr = fpreg_nr(H),
      case hipe_sparc_specific_fp:is_precoloured(RegNr) of
	true ->
	  format_fpreg(T, [H, mk_fpreg(RegNr+1)|Acc]);
	_ ->
	  format_fpreg(T, [H|Acc])
      end;
    _ ->
      format_fpreg(T, [H|Acc])
  end;
format_fpreg([], Acc) ->
  Acc.

%%
%% Remove immediates from a list
%%

remove_immediates([]) -> 
  [];
remove_immediates([I|Is]) ->
  case is_imm(I) of
    true -> remove_immediates(Is);
    false -> [I | remove_immediates(Is)]
  end.


%%
%% Remove registers from a list
%%

%% remove_registers([]) -> 
%%   [];
%% remove_registers([I|Is]) ->
%%   case is_reg(I) of
%%     true -> remove_registers(Is);
%%     false -> 
%%       case is_fpreg(I) of
%% 	true -> remove_registers(Is);
%% 	false -> [I | remove_registers(Is)]
%%       end
%%   end.


%%
%% Keep registers in a list
%%

keep_registers([]) -> 
  [];
keep_registers([I|Is]) ->
  case is_reg(I) of
    true -> [I | keep_registers(Is)];
    false -> keep_registers(Is)
  end.

%%
%% Keep float registers in a list
%%

keep_fp_registers([]) -> 
  [];
keep_fp_registers([I|Is]) ->
  case is_fpreg(I) of
    true -> [I | keep_fp_registers(Is)];
    false -> keep_fp_registers(Is)
  end.

%%
%% Keep immediates in a list
%%

keep_imms([]) -> 
  [];
keep_imms([I|Is]) ->
  case is_imm(I) of
    true -> [I | keep_imms(Is)];
    false -> keep_imms(Is)
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @spec (Ins::sparc_instruction(), Subst) -> sparc_instruction()
%%            Subst = [{Old, New}]
%%            Old = operand()
%%            New = operand()
%% @doc Substitution -- replace occurrences of the operator Old by New if {Old,New} is in Subst.
subst(Ins, Subst) ->
  case Ins of
    #label{} -> Ins;
    #nop{} -> Ins;
%%  #align{} -> Ins;
    #comment{} -> Ins;
    #b{} -> Ins;
    #goto{} -> Ins;
    #rdy{} ->
      NewDst = subst1(Subst, rdy_dest(Ins)),
      rdy_dest_update(Ins, NewDst);
    #sethi{} ->
      NewDst = subst1(Subst, sethi_dest(Ins)),
      sethi_dest_update(Ins, NewDst);
%%  #br{} ->
%%    NewReg = subst1(Subst, br_reg(Ins)),
%%    br_reg_update(Ins, NewReg);
    #call_link{} ->
      Ins1 =
	case call_link_is_known(Ins) of
	  false ->
	    NewTarget = subst1(Subst, call_link_target(Ins)),
	    call_link_target_update(Ins, NewTarget);
	  true -> Ins
	end,
      NewArgs = subst_list(Subst, call_link_args(Ins1)),
      Ins2 =  call_link_args_update(Ins1,NewArgs),
      NewLink = subst1(Subst, call_link_link(Ins2)),
      call_link_link_update(Ins2, NewLink);
    #load_atom{} ->
      NewDst = subst1(Subst, load_atom_dest(Ins)),
      load_atom_dest_update(Ins, NewDst);
    #load_word_index{} ->
      NewDst = subst1(Subst, load_word_index_dest(Ins)),
      load_word_index_dest_update(Ins, NewDst);
    #load_address{} ->
      NewDst = subst1(Subst, load_address_dest(Ins)),
      load_address_dest_update(Ins, NewDst);
    #move{} ->
      NewSrc = subst1(Subst, move_src(Ins)),
      NewDst = subst1(Subst, move_dest(Ins)),
      I0 = move_dest_update(Ins, NewDst),
      move_src_update(I0, NewSrc);
    #multimove{} ->
      NewSrc = subst_list(Subst, multimove_src(Ins)),
      NewDst = subst_list(Subst, multimove_dest(Ins)),
      I0 = multimove_dest_update(Ins, NewDst),
      multimove_src_update(I0, NewSrc);
%%  #cmov_cc{} ->
%%    NewSrc = subst1(Subst, cmov_cc_src(Ins)),
%%    NewDst = subst1(Subst, cmov_cc_dest(Ins)),
%%    I0 = cmov_cc_dest_update(Ins, NewDst),
%%    cmov_cc_src_update(I0, NewSrc);
%%  #cmov_r{} ->
%%    NewSrc = subst1(Subst, cmov_r_src(Ins)),
%%    NewReg = subst1(Subst, cmov_r_reg(Ins)),
%%    NewDst = subst1(Subst, cmov_r_dest(Ins)),
%%    I0 = cmov_r_dest_update(Ins, NewDst),
%%    I1 = cmov_r_src_update(I0, NewSrc),
%%    cmov_r_reg_update(I1, NewReg);
    #alu{} ->
      NewSrc1 = subst1(Subst, alu_src1(Ins)),
      NewSrc2 = subst1(Subst, alu_src2(Ins)),
      NewDst = subst1(Subst, alu_dest(Ins)),
      I0 = alu_dest_update(Ins, NewDst),
      I1 = alu_src1_update(I0, NewSrc1),
      alu_src2_update(I1, NewSrc2);
    #alu_cc{} ->
      NewSrc1 = subst1(Subst, alu_cc_src1(Ins)),
      NewSrc2 = subst1(Subst, alu_cc_src2(Ins)),
      NewDst = subst1(Subst, alu_cc_dest(Ins)),
      I0 = alu_cc_dest_update(Ins, NewDst),
      I1 = alu_cc_src1_update(I0, NewSrc1),
      alu_cc_src2_update(I1, NewSrc2);
    #load{} ->
      NewSrc = subst1(Subst, load_src(Ins)),
      NewOff = subst1(Subst, load_off(Ins)),
      NewDst = subst1(Subst, load_dest(Ins)),
      I0 = load_dest_update(Ins, NewDst),
      I1 = load_src_update(I0, NewSrc),
      load_off_update(I1, NewOff);
    #store{} ->
      NewSrc = subst1(Subst, store_src(Ins)),
      NewOff = subst1(Subst, store_off(Ins)),
      NewDst = subst1(Subst, store_dest(Ins)),
      I0 = store_dest_update(Ins, NewDst),
      I1 = store_src_update(I0, NewSrc),
      store_off_update(I1, NewOff);
    #jmp_link{} ->
      NewTarget = subst1(Subst, jmp_link_target(Ins)),
      NewOff = subst1(Subst, jmp_link_off(Ins)),
      NewLink = subst1(Subst, jmp_link_link(Ins)),
      I0 = jmp_link_link_update(Ins, NewLink),
      I1 = jmp_link_target_update(I0, NewTarget),
      jmp_link_off_update(I1, NewOff);
    #pseudo_return{} ->
      NewRegs = subst_list(Subst, pseudo_return_regs(Ins)),
      pseudo_return_regs_update(Ins, NewRegs);
    #pseudo_enter{} ->
      NewI = 
	case pseudo_enter_is_known(Ins) of
	  false ->
	    pseudo_enter_target_update(Ins,
				       subst1(Subst,pseudo_enter_target(Ins)));
	  true ->
	    Ins
	end,
      NewRegs = subst_list(Subst, pseudo_enter_args(NewI)),
      pseudo_enter_args_update(NewI, NewRegs);
    #pseudo_spill{} ->
      NewReg = subst1(Subst, pseudo_spill_reg(Ins)),
      pseudo_spill_reg_update(Ins, NewReg);
    #pseudo_unspill{} ->
      NewReg = subst1(Subst, pseudo_unspill_reg(Ins)),
      pseudo_unspill_reg_update(Ins, NewReg);
%   #pseudo_push{} ->
%     NewReg = subst1(Subst, pseudo_push_reg(Ins)),
%     pseudo_push_reg_update(Ins, NewReg);
    #pseudo_pop{} ->
      NewReg = subst1(Subst, pseudo_pop_reg(Ins)),
      pseudo_pop_reg_update(Ins, NewReg);

    #load_fp{} ->
      NewSrc = subst1(Subst, load_fp_src(Ins)),
      NewOff = subst1(Subst, load_fp_off(Ins)),
      NewDst = subst1(Subst, load_fp_dest(Ins)),
      I0 = load_fp_dest_update(Ins, NewDst),
      I1 = load_fp_src_update(I0, NewSrc),
      load_fp_off_update(I1, NewOff);
    #store_fp{} ->
      NewSrc = subst1(Subst, store_fp_src(Ins)),
      NewOff = subst1(Subst, store_fp_off(Ins)),
      NewDst = subst1(Subst, store_fp_dest(Ins)),
      I0 = store_fp_dest_update(Ins, NewDst),
      I1 = store_fp_src_update(I0, NewSrc),
      store_fp_off_update(I1, NewOff);
%%  #fb{} -> Ins;
    #fop{} ->
      NewSrc1 = subst1(Subst, fop_src1(Ins)),
      NewSrc2 = subst1(Subst, fop_src2(Ins)),
      NewDst = subst1(Subst, fop_dest(Ins)),
      I0 = fop_dest_update(Ins, NewDst),
      I1 = fop_src1_update(I0, NewSrc1),
      fop_src2_update(I1, NewSrc2);
%%  #fcmp{} ->
%%    NewSrc1 = subst1(Subst, fcmp_src1(Ins)),
%%    NewSrc2 = subst1(Subst, fcmp_src2(Ins)),
%%    I1 = fcmp_src1_update(Ins, NewSrc1),
%%  #fcmp_src2_update(I1, NewSrc2);
    #fmove{} ->
      NewSrc = subst1(Subst, fmove_src(Ins)),
      NewDst = subst1(Subst, fmove_dest(Ins)),
      I0 = fmove_dest_update(Ins, NewDst),
      fmove_src_update(I0, NewSrc);
    #conv_fp{} ->
      NewSrc = subst1(Subst, conv_fp_src(Ins)),
      NewDst = subst1(Subst, conv_fp_dest(Ins)),
      I0 = conv_fp_dest_update(Ins, NewDst),
      conv_fp_src_update(I0, NewSrc);
    #jmp{} ->
      NewTarget = subst1(Subst, jmp_target(Ins)),
      NewOff = subst1(Subst, jmp_off(Ins)),
      I0 = jmp_target_update(Ins, NewTarget),
      jmp_off_update(I0, NewOff)
  end.

%% @spec (Ins::sparc_instruction(), Subst) -> sparc_instruction()
%%            Subst = [{Old, New}]
%%            Old = operand()
%%            New = operand()
%% @doc Substitution -- 
%% replace defined occurrences of the operator Old by New if {Old,New} is in Subst.
subst_defines(Ins, Subst) ->
  case Ins of
    #label{} -> Ins;
    #nop{} -> Ins;
%%  #align{} -> Ins;
    #comment{} -> Ins;
    #b{} -> Ins;
    #goto{} -> Ins;
    #jmp{} -> Ins;
%%  #br{} -> Ins;
    #rdy{} ->
      NewDst = subst1(Subst, rdy_dest(Ins)),
      rdy_dest_update(Ins, NewDst);
    #sethi{} ->
      NewDst = subst1(Subst, sethi_dest(Ins)),
      sethi_dest_update(Ins, NewDst);
    #call_link{} ->
      NewLink = subst1(Subst, call_link_link(Ins)),
      call_link_link_update(Ins, NewLink);
    #load_atom{} ->
      NewDst = subst1(Subst, load_atom_dest(Ins)),
      load_atom_dest_update(Ins, NewDst);
    #load_word_index{} ->
      NewDst = subst1(Subst, load_word_index_dest(Ins)),
      load_word_index_dest_update(Ins, NewDst);
    #load_address{} ->
      NewDst = subst1(Subst, load_address_dest(Ins)),
      load_address_dest_update(Ins, NewDst);
    #move{} ->
      NewDst = subst1(Subst, move_dest(Ins)),
      move_dest_update(Ins, NewDst);
    #multimove{} ->
      NewDst = subst_list(Subst, multimove_dest(Ins)),
      multimove_dest_update(Ins, NewDst);
%%  #cmov_cc{} ->
%%       NewDst = subst1(Subst, cmov_cc_dest(Ins)),
%%       cmov_cc_dest_update(Ins, NewDst);
%%  #cmov_r{} ->
%%       NewDst = subst1(Subst, cmov_r_dest(Ins)),
%%       cmov_r_dest_update(Ins, NewDst);
    #alu{} ->
      NewDst = subst1(Subst, alu_dest(Ins)),
      alu_dest_update(Ins, NewDst);
    #alu_cc{} ->
      NewDst = subst1(Subst, alu_cc_dest(Ins)),
      alu_cc_dest_update(Ins, NewDst);
    #load{} ->
      NewDst = subst1(Subst, load_dest(Ins)),
      load_dest_update(Ins, NewDst);
    #store{} -> Ins;
    #pseudo_return{} -> Ins;
    #pseudo_enter{} -> Ins;
    #pseudo_spill{} -> Ins;
    #pseudo_unspill{} -> 
      NewReg = subst1(Subst, pseudo_unspill_reg(Ins)),
      pseudo_unspill_reg_update(Ins, NewReg);
%   #pseudo_push{} -> Ins;
    #pseudo_pop{} ->
      NewReg = subst1(Subst, pseudo_pop_reg(Ins)),
      pseudo_pop_reg_update(Ins, NewReg);
    #load_fp{} ->
      NewDst = subst1(Subst, load_fp_dest(Ins)),
      load_fp_dest_update(Ins, NewDst);
    #store_fp{} -> Ins;
%%  #fb{} -> Ins;
    #fop{} ->
      NewDst = subst1(Subst, fop_dest(Ins)),
      fop_dest_update(Ins, NewDst);
%%  #fcmp{} -> Ins; %% XXX: Should handle fccn updates.
    #fmove{} ->
      NewDst = subst1(Subst, fmove_dest(Ins)),
      fmove_dest_update(Ins, NewDst);
    #conv_fp{} ->
      NewDst = subst1(Subst, conv_fp_dest(Ins)),
      conv_fp_dest_update(Ins, NewDst);
    #jmp_link{} ->
      NewLink = subst1(Subst, jmp_link_link(Ins)),
      jmp_link_link_update(Ins, NewLink)
  end.


%% @spec (Ins::sparc_instruction(), Subst) -> sparc_instruction()
%%            Subst = [{Old, New}]
%%            Old = operand()
%%            New = operand()
%% @doc Substitution -- 
%% replace used occurrences of the operator Old by New if {Old,New} is in Subst.
subst_uses(Ins, Subst) ->
  case Ins of
    #label{} -> Ins;
    #nop{} -> Ins;
%%  #align{} -> Ins;
    #comment{} -> Ins;
    #b{} -> Ins;
    #goto{} -> Ins;
    #rdy{} -> Ins;
    #sethi{} -> Ins;
    #call_link{} -> 
      case call_link_is_known(Ins) of
	false ->
	  NewTarget = subst1(Subst, call_link_target(Ins)),
	  call_link_target_update(Ins, NewTarget);
	true -> Ins
      end;
    #load_atom{} -> Ins;
    #load_word_index{} -> Ins;
    #load_address{} -> Ins;
%%  #br{} ->
%%    NewReg = subst1(Subst, br_reg(Ins)),
%%    br_reg_update(Ins, NewReg);
    #move{} ->
      NewSrc = subst1(Subst, move_src(Ins)),
      move_src_update(Ins, NewSrc);
    #multimove{} ->
      NewSrc = subst_list(Subst, multimove_src(Ins)),
      multimove_src_update(Ins, NewSrc);
%%  #cmov_cc{} ->
%%    NewSrc = subst1(Subst, cmov_cc_src(Ins)),
%%    cmov_cc_src_update(Ins, NewSrc);
%%  #cmov_r{} ->
%%    NewSrc = subst1(Subst, cmov_r_src(Ins)),
%%    NewReg = subst1(Subst, cmov_r_reg(Ins)),
%%    I1 = cmov_r_src_update(Ins, NewSrc),
%%    cmov_r_reg_update(I1, NewReg);
    #alu{} ->
      NewSrc1 = subst1(Subst, alu_src1(Ins)),
      NewSrc2 = subst1(Subst, alu_src2(Ins)),
      I1 = alu_src1_update(Ins, NewSrc1),
      alu_src2_update(I1, NewSrc2);
    #alu_cc{} ->
      NewSrc1 = subst1(Subst, alu_cc_src1(Ins)),
      NewSrc2 = subst1(Subst, alu_cc_src2(Ins)),
      I1 = alu_cc_src1_update(Ins, NewSrc1),
      alu_cc_src2_update(I1, NewSrc2);
    #load{} ->
      NewSrc = subst1(Subst, load_src(Ins)),
      NewOff = subst1(Subst, load_off(Ins)),
      I1 = load_src_update(Ins, NewSrc),
      load_off_update(I1, NewOff);
    #store{} ->
      NewSrc = subst1(Subst, store_src(Ins)),
      NewOff = subst1(Subst, store_off(Ins)),
      NewDst = subst1(Subst, store_dest(Ins)),
      I0 = store_dest_update(Ins, NewDst),
      I1 = store_src_update(I0, NewSrc),
      store_off_update(I1, NewOff);
    #jmp_link{} ->
      NewTarget = subst1(Subst, jmp_link_target(Ins)),
      NewOff = subst1(Subst, jmp_link_off(Ins)),
      I1 = jmp_link_target_update(Ins, NewTarget),
      jmp_link_off_update(I1, NewOff);
    #pseudo_return{} ->
      NewRegs = subst_list(Subst, pseudo_return_regs(Ins)),
      pseudo_return_regs_update(Ins, NewRegs);
    #pseudo_enter{} ->
      NewI = 
	case pseudo_enter_is_known(Ins) of
	  false ->
	    pseudo_enter_target_update(Ins,
				       subst1(Subst,pseudo_enter_target(Ins)));
	  true ->
	    Ins
	end,
      NewRegs = subst_list(Subst, pseudo_enter_args(NewI)),
      pseudo_enter_args_update(NewI, NewRegs);
    #pseudo_spill{} ->
      NewReg = subst1(Subst, pseudo_spill_reg(Ins)),
      pseudo_spill_reg_update(Ins, NewReg);
    #pseudo_unspill{} ->
      Ins;
%   #pseudo_push{} ->
%     NewReg = subst1(Subst, pseudo_push_reg(Ins)),
%     pseudo_push_reg_update(Ins, NewReg);
    #pseudo_pop{} ->
      Ins;
    #load_fp{} ->
      NewSrc = subst1(Subst, load_fp_src(Ins)),
      NewOff = subst1(Subst, load_fp_off(Ins)),
      I1 = load_fp_src_update(Ins, NewSrc),
      load_fp_off_update(I1, NewOff);
    #store_fp{} ->
      NewSrc = subst1(Subst, store_fp_src(Ins)),
      NewOff = subst1(Subst, store_fp_off(Ins)),
      NewDst = subst1(Subst, store_fp_dest(Ins)),
      I0 = store_fp_dest_update(Ins, NewDst),
      I1 = store_fp_src_update(I0, NewSrc),
      store_fp_off_update(I1, NewOff);
%%  #fb{} -> Ins; %% XXX: Should realy handle subst of fcc-regs.
    #fop{} ->
      NewSrc1 = subst1(Subst, fop_src1(Ins)),
      NewSrc2 = subst1(Subst, fop_src2(Ins)),
      I1 = fop_src1_update(Ins, NewSrc1),
      fop_src2_update(I1, NewSrc2);
%%  #fcmp{} ->
%%    NewSrc1 = subst1(Subst, fcmp_src1(Ins)),
%%    NewSrc2 = subst1(Subst, fcmp_src2(Ins)),
%%    I1 = fcmp_src1_update(Ins, NewSrc1),
%%    fcmp_src2_update(I1, NewSrc2);
    #fmove{} ->
      NewSrc = subst1(Subst, fmove_src(Ins)),
      fmove_src_update(Ins, NewSrc);
    #conv_fp{} ->
      NewSrc = subst1(Subst, conv_fp_src(Ins)),
      conv_fp_src_update(Ins, NewSrc);
    #jmp{} ->
      NewTarget = subst1(Subst, jmp_target(Ins)),
      NewOff = subst1(Subst, jmp_off(Ins)),
      I0 = jmp_target_update(Ins, NewTarget),
      jmp_off_update(I0, NewOff)
  end.

subst_list(S,Xs) ->
  [subst1(S,X) || X <- Xs].

subst1([],X) -> 
  X;
subst1([{X,Y}|_],X) -> 
  Y;
subst1([_|Xs],X) -> 
  subst1(Xs,X).

%% ----------------------------------------------
