%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% hipe_rtl2sparc - translate RTL-code to SPARC-code
%%

-module(hipe_rtl2sparc).
-export([translate/2]).
-include("../main/hipe.hrl").

%%
%% Translates RTL-code to SPARC-code.
%%

translate(Rtl, _Options) ->
  VarMap = new_var_map(),
  Code = hipe_rtl:rtl_code(Rtl),
  Data = hipe_rtl:rtl_data(Rtl),
  hipe_gensym:init(sparc),
  hipe_gensym:set_label(sparc,element(2,hipe_rtl:rtl_label_range(Rtl))+1),
  hipe_gensym:set_var(sparc,hipe_sparc_registers:first_virtual()),
  Arity = length(hipe_rtl:rtl_params(Rtl)),
  Closure = hipe_rtl:rtl_is_closure(Rtl),
  Leaf = hipe_rtl:rtl_is_leaf(Rtl),

  {InitCode, VarMap1} = head(Rtl,VarMap),
  {SparcCode, NewData} = translate_instructions(Code, VarMap1, Data),
  SparcCode1 = lists:flatten(SparcCode), 
  FirstLab = hd(SparcCode1),
  Goto = hipe_sparc:goto_create(hipe_sparc:label_name(FirstLab),[]),
  Sparc =
    hipe_sparc:mk_sparc(hipe_rtl:rtl_fun(Rtl),
			Arity, Closure, Leaf,
			InitCode++[Goto|SparcCode1],
			NewData,
			{1, hipe_gensym:get_var(sparc)}, 
			{1, hipe_gensym:get_label(sparc)}),
  %%  hipe_sparc_pp:pp(Sparc),
  Sparc.


head(Rtl,Map) ->
  StartLabel = hipe_sparc:label_create_new(),
  Params = hipe_rtl:rtl_params(Rtl),
  {SparcParams, Map0} = rvs2srs(Params, Map),
  {_Regs,Moves} = move_args_to_vars(SparcParams),
  {[StartLabel|Moves],Map0}.
  
translate_instructions([], _Map, ConstTab) ->
  {[], ConstTab};
translate_instructions([I|Is], Map, ConstTab) ->
  {NewIs, NewMap, NewConstTab} = translate_instruction(I, Map, ConstTab),
  {NewIs2, NewConstTab2} = translate_instructions(Is, NewMap, NewConstTab),
  {NewIs ++  NewIs2, NewConstTab2}.

translate_instruction(I, Map, ConstTab) ->
  case hipe_rtl:type(I) of
    label ->           translate_label(I, Map, ConstTab);
    move ->            translate_move(I, Map, ConstTab);
    multimove ->       translate_multimove(I, Map, ConstTab);
    alu ->             translate_alu(I, Map, ConstTab);
    goto ->            translate_goto(I, Map, ConstTab);
    load ->            translate_load(I, Map, ConstTab);
    load_atom ->       translate_load_atom(I, Map, ConstTab);
    load_word_index -> translate_load_word_index(I, Map, ConstTab);
    goto_index ->      translate_goto_index(I, Map, ConstTab);
    load_address ->    translate_load_address(I, Map, ConstTab);
    store ->           translate_store(I, Map, ConstTab);
    call ->            translate_call(I, Map, ConstTab);
    enter ->
      {Args, Map0} = rvs2srs(hipe_rtl:enter_args(I), Map),
      {RegArgs,Head} = move_vars_to_args(Args),

      Fun = hipe_rtl:enter_fun(I),
      Type = hipe_rtl:enter_type(I),
      {Target, Map1} = case Type of
			 closure -> rv2sr(Fun, Map0);
			 _ -> {Fun, Map0}
		       end,

      Ins = [Head, hipe_sparc:pseudo_enter_create(Target, RegArgs, Type, [])],
      {Ins, Map1, ConstTab};

    branch ->
      {Src1, Map0} = rv2sr(hipe_rtl:branch_src1(I), Map),
      {Src2, Map1} = rv2sr(hipe_rtl:branch_src2(I), Map0),
      Zero = hipe_sparc:mk_reg(hipe_sparc_registers:zero()),
      Pred = hipe_rtl:branch_pred(I),
      Cond = rtl_cond_to_sparc_cc(hipe_rtl:branch_cond(I)),
      Ins = [hipe_sparc:alu_cc_create(Zero, Src1, '-', Src2, []),
	     if Pred >= 0.5 ->
		 hipe_sparc:b_create(
		   Cond,
		   hipe_rtl:branch_true_label(I),
		   hipe_rtl:branch_false_label(I),
		   Pred,
		   na,
		   []);
		true ->
		 hipe_sparc:b_create(
		   neg_cond(Cond),
		   hipe_rtl:branch_false_label(I),
		   hipe_rtl:branch_true_label(I),
		   1.0 - Pred,
		   na,
		   [])
	     end],
      {Ins, Map1, ConstTab};
    alub -> %% this instruction is a little more high-level
      Op = rtl_op2sparc_op(hipe_rtl:alub_op(I)),
      {Dst, Map0} = rv2sr(hipe_rtl:alub_dst(I), Map),
      {Src1, Map1} = rv2sr(hipe_rtl:alub_src1(I), Map0),
      {Src2, Map2} = rv2sr(hipe_rtl:alub_src2(I), Map1),
      Cond = rtl_cond_to_sparc_cc(hipe_rtl:alub_cond(I)),
      Pred = hipe_rtl:alub_pred(I),
      Branch = 
	if Pred >= 0.5 ->
	    hipe_sparc:b_create(Cond, 
				hipe_rtl:alub_true_label(I),
				hipe_rtl:alub_false_label(I),
				Pred,
				na,
				[]);
	   true ->
	    hipe_sparc:b_create(neg_cond(Cond), 
				hipe_rtl:alub_false_label(I),
				hipe_rtl:alub_true_label(I),
				1.0 - Pred,
				na,
				[])
	end,

      Ins = [hipe_sparc:alu_cc_create(Dst, Src1, Op, Src2, []),
	     Branch],
      {Ins, Map2, ConstTab};
    switch -> % this instruction is a lot more high-level
      Labels = hipe_rtl:switch_labels(I),
      LMap = [{label,L}|| L <- Labels],
      {NewConstTab, JmpT} = 
	case hipe_rtl:switch_sort_order(I) of
	  [] -> 
	    hipe_consttab:insert_block(ConstTab, 4, word, LMap);
	  SortOrder ->
	    hipe_consttab:insert_sorted_block(ConstTab, 4, 
					      word, LMap, SortOrder)
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
    return ->
      {Regs, Map1} = rvs2srs(hipe_rtl:return_vars(I), Map),
      {RegArgs, Moves} = move_vars_to_retregs(Regs),
      RetIs = [hipe_sparc:pseudo_return_create(RegArgs)],
      {Moves ++ RetIs, Map1, ConstTab};
    restore_catch ->
      {Vars, Map1} = rvs2srs(hipe_rtl:restore_catch_vars(I), Map),
      {_Regs, Moves} = move_rets_to_vars(Vars),
      {Moves, Map1, ConstTab};
    fail_to ->
      {Vars, Map1} = rvs2srs([hipe_rtl:fail_to_reason(I)], Map),
      {_RegArgs, Moves} = move_vars_to_retregs(Vars),
      case hipe_rtl:fail_to_label(I) of
	[] -> {Moves, Map1, ConstTab};
	L ->
	  {Moves ++ [hipe_sparc:goto_create(L,[])],Map1, ConstTab}
      end;
    comment ->
      Ins = [hipe_sparc:comment_create(hipe_rtl:comment_text(I), [])],
      {Ins, Map, ConstTab};
    fload ->
      {Dst, Map0} = rv2sr(hipe_rtl:fload_dst(I), Map),
      {Src, Map1} = rv2sr(hipe_rtl:fload_src(I), Map0),
      {Off, Map2} = rv2sr(hipe_rtl:fload_offset(I), Map1),      
      Ins = [hipe_sparc:load_fp_create(Dst, Src, Off)],
      {Ins, Map2, ConstTab};
    fstore ->
      {Dst, Map0} = rv2sr(hipe_rtl:fstore_dst(I), Map),
      {Src, Map1} = rv2sr(hipe_rtl:fstore_src(I), Map0),
      {Off, Map2} = rv2sr(hipe_rtl:fstore_offset(I), Map1),
      Ins = [hipe_sparc:store_fp_create(Dst, Off, Src)],
      {Ins, Map2, ConstTab};
    fp ->
      {Dst, Map0} = rv2sr(hipe_rtl:fp_dst(I), Map),
      {Src1, Map1} = rv2sr(hipe_rtl:fp_src1(I), Map0),
      {Src2, Map2} = rv2sr(hipe_rtl:fp_src2(I), Map1),
      Op = rtl_op2sparc_op(hipe_rtl:fp_op(I)),
      Ins = [hipe_sparc:fop_create(Dst, Src1, Op, Src2)],
      {Ins, Map2, ConstTab};
    fp_unop ->
      {Dst, Map0} = rv2sr(hipe_rtl:fp_unop_dst(I), Map),
      {Src, Map1} = rv2sr(hipe_rtl:fp_unop_src(I), Map0),
      Ins = 
	case rtl_op2sparc_op(hipe_rtl:fp_op(I)) of
	  'fchs' ->
	    [hipe_sparc:fmov_create(Dst,double,Src,true,false,[])];
	  Op ->
	    exit({?MODULE, {"unknown fp_unop", Op}})
	end,
      {Ins, Map1, ConstTab};
    fmov ->
      {Dst, Map0} = rv2sr(hipe_rtl:fmov_dst(I), Map),
      {Src, Map1} = rv2sr(hipe_rtl:fmov_src(I), Map0),
      Ins = [hipe_sparc:fmov_create(Dst,double,Src,false,false,[])],
      {Ins, Map1, ConstTab};
    fconv ->
      {Dst, Map0} = rv2sr(hipe_rtl:fconv_dst(I), Map),
      {Src, Map1} = rv2sr(hipe_rtl:fconv_src(I), Map0),
      Ins = [hipe_sparc:conv_fp_create(Dst,Src)],
      {Ins, Map1, ConstTab};   
    X ->
      throw({?MODULE, {"unknown rtl-instruction", X}})
  end.


translate_label(I, Map, ConstTab)->
  Ins = [hipe_sparc:label_create(hipe_rtl:label_name(I), 
				 hipe_rtl:info(I))],
  {Ins, Map, ConstTab}.

translate_move(I, Map, ConstTab)->
  {Dst, Map0} = rv2sr(hipe_rtl:move_dst(I), Map),
  {Src, Map1} = rv2sr(hipe_rtl:move_src(I), Map0),
  Ins = [hipe_sparc:move_create(Dst, Src, [])],
  {Ins, Map1, ConstTab}.

translate_multimove(I, Map, ConstTab) ->
  {Dst, Map0} = rvs2srs(hipe_rtl:multimove_dst(I), Map),
  {Src, Map1} = rvs2srs(hipe_rtl:multimove_src(I), Map0),
  Ins = [hipe_sparc:multimove_create(Dst, Src, [])],
  {Ins, Map1, ConstTab}.

translate_alu(I, Map, ConstTab) ->
  Op = rtl_op2sparc_op(hipe_rtl:alu_op(I)),
  {Dst, Map0} = rv2sr(hipe_rtl:alu_dst(I), Map),
  {Src1, Map1} = rv2sr(hipe_rtl:alu_src1(I), Map0),
  {Src2, Map2} = rv2sr(hipe_rtl:alu_src2(I), Map1),
  Ins = [hipe_sparc:alu_create(Dst, Src1, Op, Src2, [])],
  {Ins, Map2, ConstTab}.

translate_goto(I, Map, ConstTab) ->
  Ins = [hipe_sparc:goto_create(hipe_rtl:goto_label(I), [])],
  {Ins, Map, ConstTab}.


translate_load(I, Map, ConstTab) ->
  %% v9-specific
  %% byte, halfword load added
  {Dst, Map0} = rv2sr(hipe_rtl:load_dst(I), Map),
  {Src, Map1} = rv2sr(hipe_rtl:load_src(I), Map0),
  {Off, Map2} = rv2sr(hipe_rtl:load_offset(I), Map1),
  case hipe_rtl:load_size(I) of
    word ->
      Ins = [hipe_sparc:load_create(Dst, uw, Src, Off, [])];
    _ ->
      case hipe_rtl:load_sign(I) of
	signed ->
	  case hipe_rtl:load_size(I) of
	    
	    halfword ->
	      Ins = [hipe_sparc:load_create(Dst, sh, Src, Off, [])];
	    
	    byte ->
	      Ins = [hipe_sparc:load_create(Dst, sb, Src, Off, [])]
	  end;
	unsigned ->
	  case hipe_rtl:load_size(I) of
	   
	    halfword ->
	      Ins = [hipe_sparc:load_create(Dst, uh, Src, Off, [])];
	    
	    byte ->
	      Ins = [hipe_sparc:load_create(Dst, ub, Src, Off, [])]
	   
	  end
      end
  end,
{Ins, Map2, ConstTab}.

translate_load_atom(I, Map, ConstTab) ->
  {Dst, Map0} = rv2sr(hipe_rtl:load_atom_dst(I), Map),
  Ins = [hipe_sparc:load_atom_create(Dst, hipe_rtl:load_atom_atom(I), [])],
  {Ins, Map0, ConstTab}.


translate_load_word_index(I, Map, ConstTab) ->
  Block = hipe_rtl:load_word_index_block(I),
  Index = hipe_rtl:load_word_index_index(I),
  {Dst, Map0} = rv2sr(hipe_rtl:load_word_index_dst(I), Map),
  Ins = [hipe_sparc:load_word_index_create(Dst, Block, Index, [])],
  {Ins, Map0, ConstTab}.

translate_goto_index(I, Map, ConstTab) ->
  Block = hipe_rtl:goto_index_block(I),
  Index = hipe_rtl:goto_index_index(I),
  Labels = hipe_rtl:goto_index_labels(I),
  JReg = hipe_sparc:mk_new_reg(),
  Ins = [hipe_sparc:load_word_index_create(JReg, Block, Index, []),
	 hipe_sparc:jmp_create(JReg, hipe_sparc:mk_imm(0),[], Labels, [])
	],
  {Ins, Map, ConstTab}.


translate_load_address(I, Map, ConstTab) ->
  {Dst, Map0} = rv2sr(hipe_rtl:load_address_dst(I), Map),
  Ins = [hipe_sparc:load_address_create(
	   Dst,
	   hipe_rtl:load_address_address(I), 
	   hipe_rtl:load_address_type(I), 
	   [])],
  {Ins, Map0, ConstTab}.

translate_store(I, Map, ConstTab) ->
%%byte and halfword stores added
  {Dst, Map0} = rv2sr(hipe_rtl:store_dst(I), Map),
  {Src, Map1} = rv2sr(hipe_rtl:store_src(I), Map0),
  {Off, Map2} = rv2sr(hipe_rtl:store_offset(I), Map1),
  Ins= 
    case  hipe_rtl:store_size(I) of
      word ->
	[hipe_sparc:store_create(Dst, Off, w, Src,[])];
      halfword ->
	[hipe_sparc:store_create(Dst, Off, h, Src,[])];
      byte ->
	[hipe_sparc:store_create(Dst, Off, b, Src,[])]
    end,
  {Ins, Map2, ConstTab}.

translate_call(I, Map, ConstTab) ->
  {Args, Map0} = rvs2srs(hipe_rtl:call_args(I), Map),
  {RetVals, Map1} = rvs2srs(hipe_rtl:call_dst(I), Map0),
  Type = hipe_rtl:call_type(I),
  {Target, Map2} = 
    case Type of
      closure -> 
	rv2sr(hipe_rtl:call_fun(I), Map1);
      _ -> 
	{hipe_rtl:call_fun(I), Map1}
    end,
  Ins = conv_call(
	  RetVals, Target, Args,  hipe_rtl:call_continuation(I),
	  hipe_rtl:call_fail(I), Type,
	  hipe_rtl:info(I)),
  {Ins, Map2, ConstTab}.

%% Finalise the conversion of a call instruction.

conv_call(Dsts, Fun, Args, ContLab, ExnLab, Type, Info) ->
  {RetArgs,RealContLab, Tail} =
    case move_rets_to_vars(Dsts) of
      {[],[]} ->
	%% Avoid consing up a dummy basic block if the moves list
	%% is empty, as is typical for calls to suspend/0.
	{[],ContLab, []};
      {RAs,Moves}  ->
	%% Change the call to continue at a new basic block.
	%% In this block, move the result registers to the Dsts,
	%% then continue at the call's original continuation.
	NewContLab = hipe_sparc:label_create_new(),
	case ContLab of
	  [] -> 
	     {RAs,
	     hipe_sparc:label_name(NewContLab),
	     [NewContLab |
	      Moves]};
	  _ ->
	    {RAs,
	     hipe_sparc:label_name(NewContLab),
	     [NewContLab |
	      Moves ++
	      [hipe_sparc:goto_create(ContLab,[])]]}
	end
	
    end,
  {RegArgs,Head} = move_vars_to_args(Args),
  CP = hipe_sparc:mk_reg(hipe_sparc_registers:return_address()),

  CallInsn = hipe_sparc:call_link_create(
	       Fun, CP, RetArgs, RegArgs,RealContLab,
	       ExnLab, Type, Info),

  [Head | [[CallInsn | Tail]]].


%%
%% Generate instructions so that a list of variables end up
%% in registers and on the stack ready for a function call.
%% Returns a tuple {RegArgs, Code} where RegArgs is a list
%% of registers used to pass arguments.
%%

move_vars_to_args(Vars) ->
  {InRegs, OnStack} = 
    get_arg_pos(Vars, hipe_sparc_registers:register_args(),[]),

  NoRegArgs = length(InRegs),
  case NoRegArgs > 0 of
    true ->
      RegArgs = arg_vars(NoRegArgs-1),
      I = hipe_sparc:multimove_create(RegArgs, InRegs,[]),

      StackArgs = length(OnStack),
      case StackArgs > 0 of
	true ->
	  {RegArgs++OnStack,[I]};
		    % move_vars_to_args(OnStack, NoRegArgs)]};
	false ->
	  {RegArgs,[I]}
      end;
    false ->
      {[],[]}
  end.

move_vars_to_retregs(Vars) ->
  {InRegs, OnStack} = 
    get_ret_pos(Vars, hipe_sparc_registers:register_rets(),[]),

  NoRegArgs = length(InRegs),
  case NoRegArgs > 0 of
    true ->
      RegArgs = ret_vars(NoRegArgs-1),
      I = hipe_sparc:multimove_create(RegArgs, InRegs,[]),

      StackArgs = length(OnStack),
      case StackArgs > 0 of
	true ->
	  {RegArgs++OnStack,[I]};
		    % move_vars_to_args(OnStack, NoRegArgs)]};
	false ->
	  {RegArgs,[I]}
      end;
    false ->
      {[],[]}
  end.


%% TODO: make into listcomprehension.

%% move_vars_to_args([], _ArgIndex) ->
%%   [];
%% %%  RegArgs = hipe_sparc_registers:register_args(),
%% %%  SP = hipe_rtl:mk_reg(hipe_sparc_registers:stack_pointer()),
%% %%  StackGrowth = (_ArgIndex-RegArgs)*4,
%% %%  [hipe_rtl:mk_alu(SP, SP, 'add', hipe_rtl:mk_imm(StackGrowth))];
%% move_vars_to_args([V|Vs], ArgIndex) ->
%%   %%  RegArgs = hipe_sparc_registers:register_args(),
%%   Code = move_vars_to_args(Vs, ArgIndex+1),
%%   %%  Off = (ArgIndex-RegArgs)*4,
%%   %%  SP = hipe_sparc:mk_reg(hipe_sparc_registers:stack_pointer()),
%%   I = hipe_sparc:pseudo_push_create(V),
%%   [I|Code].


%%
%% Generate instructions so that the arguments to a function end up 
%% in registers.
%%

move_args_to_vars(Vars) ->
  {InRegs,OnStack} =
    get_arg_pos(Vars, hipe_sparc_registers:register_args(), []),
  move_args_to_vars(InRegs, OnStack).

get_arg_pos([],_N,InRegs) ->
  {lists:reverse(InRegs),[]};

get_arg_pos([V|Vs], N, InRegs) when N > 0 ->
  get_arg_pos(Vs, N-1, [V|InRegs]);
get_arg_pos(Args, _N, InRegs) ->
  {lists:reverse(InRegs), Args}.


move_rets_to_vars(Vars) ->
  {InRegs,OnStack} =
    get_ret_pos(Vars, hipe_sparc_registers:register_rets(), []),
  move_rets_to_vars(InRegs, OnStack).

get_ret_pos([],_N,InRegs) ->
  {lists:reverse(InRegs),[]};

get_ret_pos([V|Vs], N, InRegs) when N > 0 ->
  get_ret_pos(Vs, N-1, [V|InRegs]);
get_ret_pos(Args, _N, InRegs) ->
  {lists:reverse(InRegs), Args}.


move_args_to_vars(InRegs, OnStack) ->
  NoRegArgs = length(InRegs),
  case NoRegArgs > 0 of
    true ->
      StackArgs = length(OnStack),
      RegArgs = arg_vars(NoRegArgs-1),
      I = hipe_rtl:mk_multimove(InRegs, RegArgs),
      case StackArgs > 0 of
	true ->
	  {RegArgs,[I|move_args_to_vars(OnStack, NoRegArgs, StackArgs+NoRegArgs)]};
	false ->
	  {RegArgs,[I]}
      end;
    false ->
      {[],[]}
  end.

move_args_to_vars([],_ArgIndex,_) ->
  [];
%%  RegArgs = hipe_sparc_registers:register_args(),
%%  SP = hipe_rtl:mk_reg(hipe_sparc_registers:stack_pointer()),
%%  StackDec = (_ArgIndex-RegArgs)*4,
%%  [hipe_rtl:mk_alu(SP, SP, 'sub', hipe_rtl:mk_imm(StackDec))];

move_args_to_vars([V|Vs], ArgIndex, Arity) -> 
  %%  SP = hipe_rtl:mk_reg(hipe_sparc_registers:stack_pointer()),
  %%  StackIndex = (ArgIndex-Arity)*4,
  I = hipe_sparc:pseudo_pop_create(V,hipe_sparc:mk_imm(-(ArgIndex-Arity))),
  %% , SP, hipe_rtl:mk_imm(StackIndex)),
  [I | move_args_to_vars(Vs, ArgIndex+1, Arity)].


move_rets_to_vars(InRegs, OnStack) ->
  NoRegArgs = length(InRegs),
  case NoRegArgs > 0 of
    true ->
      StackArgs = length(OnStack),
      RegArgs = ret_vars(NoRegArgs-1),
      I = hipe_rtl:mk_multimove(InRegs, RegArgs),
      case StackArgs > 0 of
	true ->
	  {RegArgs,[I|move_rets_to_vars(OnStack, NoRegArgs, StackArgs+NoRegArgs)]};
	false ->
	  {RegArgs,[I]}
      end;
    false ->
      {[],[]}
  end.

move_rets_to_vars([],_ArgIndex,_) ->
  [];
%%  RegArgs = hipe_sparc_registers:register_args(),
%%  SP = hipe_rtl:mk_reg(hipe_sparc_registers:stack_pointer()),
%%  StackDec = (_ArgIndex-RegArgs)*4,
%%  [hipe_rtl:mk_alu(SP, SP, 'sub', hipe_rtl:mk_imm(StackDec))];

move_rets_to_vars([V|Vs], ArgIndex, Arity) -> 
  %%  SP = hipe_rtl:mk_reg(hipe_sparc_registers:stack_pointer()),
  %%  StackIndex = (ArgIndex-Arity)*4,
  I = hipe_sparc:pseudo_pop_create(V,hipe_sparc:mk_imm(-(ArgIndex-Arity))),
  %% , SP, hipe_rtl:mk_imm(StackIndex)),
  [I | move_rets_to_vars(Vs, ArgIndex+1, Arity)].


%%
%% Return a variable corresponding to argument number 'X'
%%

arg_var(X) ->
  hipe_sparc:mk_reg(hipe_sparc_registers:arg(X)).

arg_vars(N) ->
  arg_vars(N,[]).

arg_vars(N, Acc) when N >= 0 ->
  arg_vars(N-1, [arg_var(N)|Acc]);
arg_vars(_, Acc) -> Acc.

ret_var(X) ->
  hipe_sparc:mk_reg(hipe_sparc_registers:ret(X)).

ret_vars(N) ->
  ret_vars(N,[]).

ret_vars(N, Acc) when N >= 0 ->
  ret_vars(N-1, [ret_var(N)|Acc]);
ret_vars(_, Acc) -> Acc.



%%% Convert a 'fun' operand (MFA, prim, or temp)

%%
%% Convert a operator from rtl to a sparc condition code.
%%

rtl_op2sparc_op(add) -> '+';
rtl_op2sparc_op(sub) -> '-';
rtl_op2sparc_op(fadd) -> '+';
rtl_op2sparc_op(fchs) -> 'fchs';
rtl_op2sparc_op(fsub) -> '-';
rtl_op2sparc_op(fmul) -> '*';
rtl_op2sparc_op(fdiv) -> '/';
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


neg_cond(Cond) ->
  hipe_sparc:cc_negate(Cond).


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
		     case hipe_rtl:is_fpreg(Var) of
		       true ->
			 hipe_rtl:fpreg_name(Var);
		       false ->
			 ?EXIT({"bad rtl value", Var})
		     end
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
	      case hipe_rtl:is_fpreg(Var) of
		true ->
		  NewVar = hipe_sparc:mk_new_fpreg();
		false ->
		  NewVar = hipe_sparc:mk_new_reg()
	      end,
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
