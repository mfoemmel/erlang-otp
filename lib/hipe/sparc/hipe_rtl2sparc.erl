%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% hipe_rtl2sparc - translate RTL-code to SPARC-code
%%

-module(hipe_rtl2sparc).
-export([translate/1]).
-include("../main/hipe.hrl").

%%
%% Translates RTL-code to SPARC-code.
%%

translate(Rtl) ->
  hipe_gensym:set_var(hipe_sparc_registers:first_virtual()),
  VarMap = new_var_map(),
  Code = hipe_rtl:rtl_code(Rtl),
  Data = hipe_rtl:rtl_data(Rtl),
  {SparcCode, NewData} = translate_instructions(Code, VarMap, Data),
  Sparc =
    hipe_sparc:mk_sparc(hipe_rtl:rtl_fun(Rtl),
			SparcCode, 
			NewData,
			{1, hipe_gensym:get_var()}, 
			hipe_rtl:rtl_label_range(Rtl)),
  %% hipe_sparc:pp(Sparc),
  Sparc.


translate_instructions([], Map, ConstTab) ->
  {[], ConstTab};
translate_instructions([I|Is], Map, ConstTab) ->
  {NewIs, NewMap, NewConstTab} = translate_instruction(I, Map, ConstTab),
  {NewIs2, NewConstTab2} = translate_instructions(Is, NewMap, NewConstTab),
  {NewIs ++  NewIs2, NewConstTab2}.

translate_instruction(I, Map, ConstTab) ->
  case hipe_rtl:type(I) of
    label ->
      Ins = [hipe_sparc:label_create(hipe_rtl:label_name(I), 
				     hipe_rtl:info(I))],
      {Ins, Map, ConstTab};
    move ->
      {Dst, Map0} = rv2sr(hipe_rtl:move_dst(I), Map),
      {Src, Map1} = rv2sr(hipe_rtl:move_src(I), Map0),
      Ins = [hipe_sparc:move_create(Dst, Src, [])],
      {Ins, Map1, ConstTab};
    multimove ->
      {Dst, Map0} = rvs2srs(hipe_rtl:multimove_dst(I), Map),
      {Src, Map1} = rvs2srs(hipe_rtl:multimove_src(I), Map0),
      Ins = [hipe_sparc:multimove_create(Dst, Src, [])],
      {Ins, Map1, ConstTab};
    alu ->
      Op = rtl_op2sparc_op(hipe_rtl:alu_op(I)),
      {Dst, Map0} = rv2sr(hipe_rtl:alu_dst(I), Map),
      {Src1, Map1} = rv2sr(hipe_rtl:alu_src1(I), Map0),
      {Src2, Map2} = rv2sr(hipe_rtl:alu_src2(I), Map1),
      Ins = [hipe_sparc:alu_create(Dst, Src1, Op, Src2, [])],
      {Ins, Map2, ConstTab};
    goto ->
      Ins = [hipe_sparc:goto_create(hipe_rtl:goto_label(I), [])],
      {Ins, Map, ConstTab};
    load -> %% v9-specific
      {Dst, Map0} = rv2sr(hipe_rtl:load_dst(I), Map),
      {Src, Map1} = rv2sr(hipe_rtl:load_src(I), Map0),
      {Off, Map2} = rv2sr(hipe_rtl:load_offset(I), Map1),
      Ins = [hipe_sparc:load_create(Dst, uw, Src, Off, [])],
      {Ins, Map2, ConstTab};
    load_atom ->
      {Dst, Map0} = rv2sr(hipe_rtl:load_atom_dst(I), Map),
      Ins = [hipe_sparc:load_atom_create(Dst, hipe_rtl:load_atom_atom(I), [])],
      {Ins, Map0, ConstTab};
    load_word_index ->
      Block = hipe_rtl:load_word_index_block(I),
      Index = hipe_rtl:load_word_index_index(I),
      {Dst, Map0} = rv2sr(hipe_rtl:load_word_index_dst(I), Map),
      Ins = [hipe_sparc:load_word_index_create(Dst, Block, Index, [])],
      {Ins, Map0, ConstTab};
    goto_index ->
      Block = hipe_rtl:goto_index_block(I),
      Index = hipe_rtl:goto_index_index(I),
      Labels = hipe_rtl:goto_index_labels(I),
      JReg = hipe_sparc:mk_new_reg(),
      Ins = [hipe_sparc:load_word_index_create(JReg, Block, Index, []),
	     hipe_sparc:jmp_create(JReg, hipe_sparc:mk_imm(0),[], Labels, [])
	    ],
      {Ins, Map, ConstTab};
    load_address ->
      {Dst, Map0} = rv2sr(hipe_rtl:load_address_dst(I), Map),
      Ins = [hipe_sparc:load_address_create(Dst,
					    hipe_rtl:load_address_address(I), 
					    hipe_rtl:load_address_type(I), 
					    [])],
      {Ins, Map0, ConstTab};
    store ->
      {Dst, Map0} = rv2sr(hipe_rtl:store_dst(I), Map),
      {Src, Map1} = rv2sr(hipe_rtl:store_src(I), Map0),
      {Off, Map2} = rv2sr(hipe_rtl:store_offset(I), Map1),
      Ins = [hipe_sparc:store_create(Dst, Off, uw, Src, [])],
      {Ins, Map2, ConstTab};
    jmp ->
      {Target, Map0} = rv2sr(hipe_rtl:jmp_target(I), Map),
      {Offset, Map1} = rv2sr(hipe_rtl:jmp_offset(I), Map0),
      {Args, Map2} = rvs2srs(hipe_rtl:jmp_args(I), Map1),
      Ins = [hipe_sparc:jmp_create(Target, Offset, Args, [])],
      {Ins, Map2, ConstTab};
    jmp_link ->
      ?EXIT({jmp_link,nyi});
    %% {Target, Map0} = rv2sr(hipe_rtl:jmp_link_target(I), Map),
    %% {Offset, Map1} = rv2sr(hipe_rtl:jmp_link_offset(I), Map0),
    %% {Link, Map2} = rv2sr(hipe_rtl:jmp_link_link(I), Map1),
    %% {Args, Map3} = rvs2srs(hipe_rtl:jmp_link_args(I), Map2),
    %% Ins = [hipe_sparc:jmp_create(Target, Offset, Link, Args, [])],
    %% {Ins, Map3, ConstTab};
    jsr ->
      CP = hipe_sparc:mk_reg(hipe_sparc_registers:return_address()),
      {Args, Map0} = rvs2srs(hipe_rtl:jmp_args(I), Map),
      {Target, Map1} = 
	case hipe_rtl:jsr_type(I) of
	  closure -> 
	    rv2sr(hipe_rtl:jsr_fun(I), Map0);
	  _ -> 
	    {hipe_rtl:jsr_fun(I), Map0}
	end,
      Ins = [hipe_sparc:call_link_create(Target, CP, Args, 
					 hipe_rtl:jsr_continuation(I),
					 hipe_rtl:jsr_fail(I),
					 hipe_rtl:jsr_type(I),
					 hipe_rtl:info(I))],
      {Ins, Map1, ConstTab};
    esr ->
      {Args, Map0} = rvs2srs(hipe_rtl:esr_args(I), Map),
      case hipe_rtl:esr_type(I) of
	closure -> 
	  {Target, Map1} = rv2sr(hipe_rtl:esr_fun(I), Map0),
	  Args0 = 
	    [hipe_sparc:mk_reg(hipe_sparc_registers:return_address()) | 
	     Args],
	  {[hipe_sparc:jmp_create(Target,
				  hipe_sparc:mk_imm(0), Args0, [])],
	   Map1, ConstTab};
	Type ->
	  Tmp = hipe_sparc:mk_new_reg(),
	  Args0 = 
	    [hipe_sparc:mk_reg(hipe_sparc_registers:return_address()) | 
	     Args],
	  Ins = 
	    [hipe_sparc:load_address_create(Tmp, hipe_rtl:esr_fun(I), 
					    function, 
					    [Type]),
	     hipe_sparc:jmp_create(Tmp, hipe_sparc:mk_imm(0), Args0, [])],
	  {Ins, Map0, ConstTab}
      end;
    branch ->
      {Src1, Map0} = rv2sr(hipe_rtl:branch_src1(I), Map),
      {Src2, Map1} = rv2sr(hipe_rtl:branch_src2(I), Map0),
      Zero = hipe_sparc:mk_reg(hipe_sparc_registers:zero()),
      Ins = [hipe_sparc:alu_cc_create(Zero, Src1, '-', Src2, []),
	     hipe_sparc:b_create(rtl_cond_to_sparc_cc(hipe_rtl:branch_cond(I)),
				 hipe_rtl:branch_true_label(I),
				 hipe_rtl:branch_false_label(I),
				 hipe_rtl:branch_pred(I),
				 na,
				 [])],
      {Ins, Map1, ConstTab};
    alub -> %% this instruction is a little more high-level
      Op = rtl_op2sparc_op(hipe_rtl:alub_op(I)),
      {Dst, Map0} = rv2sr(hipe_rtl:alub_dst(I), Map),
      {Src1, Map1} = rv2sr(hipe_rtl:alub_src1(I), Map0),
      {Src2, Map2} = rv2sr(hipe_rtl:alub_src2(I), Map1),
      Cond = rtl_cond_to_sparc_cc(hipe_rtl:alub_cond(I)),
      Ins = [hipe_sparc:alu_cc_create(Dst, Src1, Op, Src2, []),
	     hipe_sparc:b_create(Cond, 
				 hipe_rtl:alub_true_label(I),
				 hipe_rtl:alub_false_label(I),
				 hipe_rtl:alub_pred(I),
				 na,
				 [])],
      {Ins, Map2, ConstTab};
    switch -> % this instruction is a lot more high-level
      Labels = hipe_rtl:switch_labels(I),
      LMap = lists:map(fun (L) -> {label,L} end, Labels),
      {NewConstTab, JmpT} = 
	case hipe_rtl:switch_sort_order(I) of
	  [] -> 
	    hipe_consttab:insert_block(ConstTab, 4,word, LMap);
	  SortOrder ->
	    hipe_consttab:insert_sorted_block(ConstTab, 4, word, LMap, SortOrder)
	end,
      JTR = hipe_sparc:mk_new_reg(),
      AlignedR = hipe_sparc:mk_new_reg(),
      DestR = hipe_sparc:mk_new_reg(),
      {StartR, Map1} = rv2sr(hipe_rtl:switch_src(I), Map),
      Ins = 
	[hipe_sparc:load_address_create(JTR, JmpT, constant, 
					[{comment,jump_table}]),
	 
	 %% Multiply by 4, wordalign 
	 hipe_sparc:alu_create(AlignedR, StartR, '<<', hipe_sparc:mk_imm(2),
			       [{comment,do_wordalign}]),
	 
	 %% Get the destination from: &JmpT + (Index-Min) * 4
	 hipe_sparc:load_create(DestR, uw, JTR, AlignedR, []),
	 %% Jump to the dest.
	 hipe_sparc:jmp_create(DestR, hipe_sparc:mk_imm(0), [], 
			       Labels, [])],
      
      {Ins, Map1, NewConstTab};
    comment ->
      Ins = [hipe_sparc:comment_create(hipe_rtl:comment_text(I), [])],
      {Ins, Map, ConstTab};
    X ->
      throw({?MODULE, {"unknown rtl-instruction", X}})
  end.

%%
%% Convert a operator from rtl to a sparc condition code.
%%

rtl_op2sparc_op(add) -> '+';
rtl_op2sparc_op(sub) -> '-';
rtl_op2sparc_op('or') -> 'or';
rtl_op2sparc_op('and') -> 'and';
rtl_op2sparc_op('xor') -> 'xor';
rtl_op2sparc_op('xornot') -> 'xnor';
rtl_op2sparc_op(andnot) -> andn;
rtl_op2sparc_op(sll) -> '<<';
rtl_op2sparc_op(sllx) -> '<<64';
rtl_op2sparc_op(srl) -> '>>';
rtl_op2sparc_op(srlx) -> '>>64';
rtl_op2sparc_op(sra) -> '>>?';
rtl_op2sparc_op(srax) -> '>>?64';
rtl_op2sparc_op(X) -> exit({?MODULE, {"unknown alu-op", X}}).


%%
%% Convert a relational operator from rtl to a sparc condition code.
%%

rtl_cond_to_sparc_cc(Cond) ->
  case Cond of
    eq	-> 'e';
    ne	-> 'ne';
    gt	-> 'g';
    gtu	-> 'gu';
    ge	-> 'ge';
    geu	-> 'geu';
    lt	-> 'l';
    ltu	-> 'lu';
    le	-> 'le';
    leu	-> 'leu';
    overflow -> 'vs';
    not_overflow -> 'vc';
    %% not generated: pos, neg, a, n
    _	-> exit({?MODULE, {"unknown cond-op", Cond}})
  end.

%%
%% Convert a rtl instruction argument to a sparc ditto
%%

rv2sr(Var, Map) ->
  case hipe_rtl:is_imm(Var) of
    true ->
      {hipe_sparc:mk_imm(hipe_rtl:imm_value(Var)), Map};
    false ->
      Name = case hipe_rtl:is_var(Var) of
	       true ->
		 hipe_rtl:var_name(Var);
	       false ->
		 case hipe_rtl:is_reg(Var) of
		   true ->
		     hipe_rtl:reg_name(Var);
		   false ->
		     throw({?MODULE, {"not a value", Var}})
		 end
	     end,
      case hipe_sparc_registers:is_precolored(Name) of
	true ->
	  {hipe_sparc:mk_reg(Name), Map};
	false ->
	  case lists:keysearch(Name, 1, Map) of
	    {value, {_, NewVar}} ->
	      {NewVar, Map};
	    false ->
	      NewVar = hipe_sparc:mk_new_reg(),
	      {NewVar, [{Name, NewVar}|Map]}
	  end
      end
  end.


rvs2srs([], Map) ->
  {[], Map};
rvs2srs([V|Vs], Map) ->
  {NewV, Map0} = rv2sr(V, Map),
  {NewVs, Map1} = rvs2srs(Vs, Map0),
  {[NewV|NewVs], Map1}.

%%
%% This should probably be something more efficient than an a-list.
%%

new_var_map() ->
  [].
