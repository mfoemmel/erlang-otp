%% -*- erlang-indent-level: 2 -*-
%%=======================================================================
%% File        : hipe_icode2rtl.erl
%% Author(s)   : Erik Johansson
%% Description : Translates Icode to RTL
%%=======================================================================
%% $Id$
%%=======================================================================
%% Notes:
%%        The concepts of bifs and primops are a bit muddy in this 
%%        module at the moment. The Icode structure does not enforce
%%        a strict use of them.
%%        It should be like this:
%%         - erlang:exit/1, erlang:throw/1, erlang:fault/X should use the 
%%            fail-instruction in Icode.
%%         - All other BIFs, operators, or internal functions that are 
%%            implemented in C (or assembler, or inlined, or whatever)
%%            should use the call instruction with primop tag.
%%            If the call is tail-recursive it will have to be
%%            implemented as call primop + return.
%%         - Calls or tail-recursive calls to all other Erlang functions
%%            should be implemented with call or enter respectively.
%%             
%%        At the moment this is not always the case.  Therefore some
%%        checking is done to also handle cases outside this model.  These
%%        tests will be removed as soon as possible, don't count on them.
%%
%% TODO: 
%%       Better handling of switches...
%%=======================================================================

-module(hipe_icode2rtl).

-export([translate/2]).
-export([translate_instrs/5]).  %% used in hipe_rtl_mk_switch

%%-------------------------------------------------------------------------
-define(DEBUG,1).
-include("hipe_icode2rtl.hrl").
-include("../main/hipe.hrl").
-include("hipe_literals.hrl").

%%-------------------------------------------------------------------------

%%
%% Translates Icode to RTL-code
%%
translate(Fun,Options) ->
  ?IF_DEBUG_LEVEL(2,put(hipe_mfa,hipe_icode:icode_fun(Fun)),true),  
  %% hipe_icode_pp:pp(Fun),

  %% Initialize gensym and varmap
  {Args, VarMap} = hipe_rtl_varmap:init(Fun),
  %% Get the name and other info of the function to translate.
  MFA = hipe_icode:icode_fun(Fun),
  Data = hipe_icode:icode_data(Fun),
  Icode = hipe_icode:icode_code(Fun),
  IsClosure = hipe_icode:icode_is_closure(Fun), 
  IsLeaf = hipe_icode:icode_is_leaf(Fun), 
  Info = hipe_icode:icode_info(Fun),

  %% Create code that build exit terms with a trace.
  {ExitInfo, ConstTab1} =
    hipe_rtl_exceptions:gen_exception(MFA, Data),

  %% Translate Icode instructions to RTL instructions
  ?opt_start_timer("Icode to nested RTL"),
  {Code, _VarMap1, ConstTab2} = 
    translate_instrs(Icode,
		     VarMap,
		     ConstTab1,
		     Options,
		     ExitInfo),
  ?opt_stop_timer("Icode to nested RTL"),
  %% We build the code as list of lists of...
  %%  in order to avoid appends.
  ?opt_start_timer("Flatten RTL"),
  Code1 = lists:flatten(Code), 
  ?opt_stop_timer("Flatten RTL"),
  %% Build the RTL structure.
  Rtl = hipe_rtl:mk_rtl(MFA,
			Args,
			IsClosure,
			IsLeaf,
			Code1,
			ConstTab2,
			{1, hipe_gensym:get_var(rtl)},
			{1, hipe_gensym:get_label(rtl)}),

  %% hipe_rtl:pp(Rtl),
  %% Propagate info from Icode to RTL.
  hipe_rtl:rtl_info_update(Rtl, Info).
%% ____________________________________________________________________
%% 



%% ____________________________________________________________________
%% 
%%
%% Translate all icode instructions to rtl instructions
%%
translate_instrs(Is, VarMap, ConstTab, Options, ExitInfo) ->
  translate_instrs(Is, VarMap, [], ConstTab, Options, ExitInfo).

translate_instrs([], VarMap, Code, ConstTab, _Options, ExitInfo) ->
  %% We add the code for building exit values last.
  ExitCode = hipe_rtl_exceptions:exit_code(ExitInfo),
  {[Code,ExitCode], VarMap, ConstTab};
translate_instrs([I|Is], VarMap, AccCode, ConstTab, Options, ExitInfo) ->
  %% Translate one instruction. 
  {Code, VarMap0, ConstTab0} = 
    translate_instruction(I, VarMap, ConstTab, Options, ExitInfo),
  %%  ?IF_DEBUG_LEVEL(3,?msg("  To Instr: ~w~n",[Code]),no_debug),
  ?IF_DEBUG(?when_option(rtl_show_translation, Options,
			 ?msg("  To Instr: ~w~n",[Code])),
	    no_debug),
  translate_instrs(Is, VarMap0, [AccCode,Code], ConstTab0, Options, ExitInfo).


%%
%% Translate a icode instruction to one or more rtl instructions
%%

translate_instruction(I, VarMap, ConstTab, Options, ExitInfo) ->
  %%  ?IF_DEBUG_LEVEL(3,?msg("From Instr: ~w~n",[I]),no_debug),
  ?IF_DEBUG(?when_option(rtl_show_translation, Options,
			 ?msg("From Instr: ~w~n",[I])),no_debug),
  case hipe_icode:type(I) of
    mov ->   gen_move(I, VarMap, ConstTab);
    call ->  gen_call(I, VarMap, Options, ExitInfo, ConstTab);
    enter -> gen_enter(I, VarMap, Options, ExitInfo, ConstTab);
    return ->
      {RetVars, VarMap0} = 
	hipe_rtl_varmap:ivs2rvs(hipe_icode:return_vars(I), VarMap),
      {hipe_rtl:mk_return(RetVars), VarMap0, ConstTab};
    'if' ->  gen_if(I, VarMap, ConstTab);
    switch_val -> 
      gen_switch_val(I, VarMap, ConstTab, Options, ExitInfo);
    switch_tuple_arity -> 
      gen_switch_tuple(I, VarMap, ConstTab, Options, ExitInfo);
    type ->   gen_type(I, VarMap, ConstTab);
    goto ->
      {Label, Map0} = 
	hipe_rtl_varmap:icode_label2rtl_label(hipe_icode:goto_label(I), VarMap),
      {hipe_rtl:mk_goto(hipe_rtl:label_name(Label)), Map0, ConstTab};
    fail ->
      hipe_rtl_exceptions:gen_fail(I, VarMap, ConstTab, ExitInfo);
    label ->
      LabelName = hipe_icode:label_name(I),
      {NewLabel, Map0} = hipe_rtl_varmap:icode_label2rtl_label(LabelName, VarMap),
      {hipe_rtl:info_update(NewLabel, hipe_icode:info(I)), Map0, ConstTab};
    restore_catch ->
      hipe_rtl_exceptions:gen_restore_catch(I, VarMap, ConstTab, Options);
    comment ->
      {hipe_rtl:mk_comment(hipe_icode:comment_text(I)), VarMap, ConstTab};  
    fmov ->  gen_fmove(I, VarMap, ConstTab);
    X ->
      exit({?MODULE, {"unknown icode instruction", X}})
  end.


%% ____________________________________________________________________
%% MOVE
gen_move(I, VarMap, ConstTab) ->
  MovedSrc = hipe_icode:mov_src(I),
  case hipe_icode:is_var(MovedSrc) orelse hipe_icode:is_reg(MovedSrc)  of
    true -> %% A move from one var to another
      {[Dst,Src], VarMap0} =
	hipe_rtl_varmap:ivs2rvs([hipe_icode:mov_dst(I),MovedSrc], VarMap),
      {hipe_rtl:mk_move(Dst, Src), VarMap0, ConstTab};
    false -> %% A move of a constant to a var has to be expanded.
      {Dst, VarMap0} =
	hipe_rtl_varmap:icode_var2rtl_var(hipe_icode:mov_dst(I), VarMap),
      {Code, NewConstMap} = 
	gen_const_move(Dst, MovedSrc, ConstTab),
      {Code, VarMap0, NewConstMap}
  end.

%% ____________________________________________________________________
%% FMOVE. fvar := fvar or fvar := -fvar
gen_fmove(I, VarMap, ConstTab) ->
  {[Dst,Src], VarMap0} =
    hipe_rtl_varmap:ivs2rvs([hipe_icode:fmov_dst(I),
			     hipe_icode:fmov_src(I)], VarMap),
  {hipe_rtl:mk_fmov(Dst, Src), VarMap0, ConstTab}.

%% ____________________________________________________________________
%% TYPE
gen_type(I, VarMap, ConstTab)->
  {[X], Map0} = hipe_rtl_varmap:ivs2rvs([hipe_icode:type_var(I)], VarMap),
  {TrueLbl, Map1} =
    hipe_rtl_varmap:icode_label2rtl_label(hipe_icode:type_true_label(I), Map0),
  {FalseLbl, Map2} =
    hipe_rtl_varmap:icode_label2rtl_label(hipe_icode:type_false_label(I), Map1),
  {Code,NewConstTab} = gen_type_test(X, hipe_icode:type_type(I), 
				     hipe_rtl:label_name(TrueLbl),
				     hipe_rtl:label_name(FalseLbl),
				     hipe_icode:type_pred(I),
				     ConstTab),
  {Code, Map2, NewConstTab}.

%% ____________________________________________________________________
%% IF
gen_if(I, VarMap, ConstTab) ->
  {Code, NewConstTab, Args, Map0} 
    =
    case hipe_icode:if_args(I) of
      [Arg1, Arg2] ->
	case {hipe_icode:is_var(Arg1), hipe_icode:is_var(Arg2)} of
	  {true, true} ->
	    {Args1, Map1} = hipe_rtl_varmap:ivs2rvs([Arg1,Arg2], VarMap),
	    {[], ConstTab, Args1, Map1};
	  {true, false} ->
	    {NewArg1, Map1} = hipe_rtl_varmap:icode_var2rtl_var(Arg1, VarMap),
	    Dst = hipe_rtl:mk_new_var(),
	    {Code1, NewConstTab1} = 
	      gen_const_move(Dst, Arg2, ConstTab),
			     
	    Args1 = [NewArg1,Dst],
	    {Code1, NewConstTab1, Args1, Map1};
	  {false, true} ->
	    {NewArg2, Map1} = hipe_rtl_varmap:icode_var2rtl_var(Arg2, VarMap),
	    Dst = hipe_rtl:mk_new_var(),
	    {Code1, NewConstTab1} = 
	      gen_const_move(Dst, Arg1, ConstTab),
	    Args1 = [Dst, NewArg2],
	    {Code1, NewConstTab1, Args1, Map1};
	  {false, false} ->
	    Dst1 = hipe_rtl:mk_new_var(),
	    {Code1, ConstTab1} = 
	      gen_const_move(Dst1, Arg1, ConstTab),
	    Dst2 = hipe_rtl:mk_new_var(),
	    {Code2, NewConstTab1} = 
	      gen_const_move(Dst2, Arg2, ConstTab1), 
	    {[Code1,Code2],
	     NewConstTab1,
	     [Dst1, Dst2],
	     VarMap}
	end;
      [] ->
	{[], ConstTab, [], VarMap}
    end,

  {TrueLbl, Map2} = 
    hipe_rtl_varmap:icode_label2rtl_label(hipe_icode:if_true_label(I), Map0),
  {FalseLbl, Map3} = 
    hipe_rtl_varmap:icode_label2rtl_label(hipe_icode:if_false_label(I), Map2),
  CondCode = 
    gen_cond(hipe_icode:if_op(I),
	     Args,
	     hipe_rtl:label_name(TrueLbl),
	     hipe_rtl:label_name(FalseLbl),
	     hipe_icode:if_pred(I)),
  {[Code,CondCode], Map3, NewConstTab}.


args_to_vars([Arg|Args],VarMap, ConstTab) ->
  {Vars, VarMap1, ConstTab1, Code} = 
    args_to_vars(Args, VarMap, ConstTab),

  case hipe_icode:is_var_or_fvar_or_reg(Arg) of
    true ->
      {Var, VarMap2} = hipe_rtl_varmap:icode_var2rtl_var(Arg,VarMap1),
      {[Var|Vars], VarMap2, ConstTab1, Code};
    false ->
      Var = hipe_rtl:mk_new_var(),
      {Code2, ConstTab2} = 
	gen_const_move(Var, Arg, ConstTab1),
      {[Var|Vars], VarMap1, ConstTab2, [Code2,Code]}
  end;
args_to_vars([], VarMap, ConstTab) ->
  {[], VarMap, ConstTab, []}.



%% ____________________________________________________________________
%% CALL
gen_call(I, VarMap, Options, ExitInfo, ConstTab) ->
  Fun = hipe_icode:call_fun(I),
  {Dst, VarMap0} = hipe_rtl_varmap:ivs2rvs(hipe_icode:call_dst(I), VarMap),
  {Args, VarMap1, ConstTab1, InitCode} = 
    args_to_vars(hipe_icode:call_args(I), VarMap0, ConstTab),
  

  Fail = hipe_icode:call_fail(I),

  {Code, VarMap5, ConstTab4} =
    case hipe_icode:call_in_guard(I) of
      true ->
	hipe_rtl_guardops:gen_guardop(I, VarMap, ConstTab1);
      false ->
	{FailLblName, VarMap3} =
	  case Fail of 
	    [] -> %% Not in a catch
	      {[], VarMap1};
	    _ ->
	      {FLbl, VarMap2} = 
		hipe_rtl_varmap:icode_label2rtl_label(Fail, VarMap1),
	      {hipe_rtl:label_name(FLbl), VarMap2}
	  end,

	case hipe_icode:call_continuation(I) of
	  [] -> %% This call does not end a BB.
	    ContLbl = hipe_rtl:mk_new_label(),
	    ContLblName = hipe_rtl:label_name(ContLbl),
	    case hipe_bif:is_bif(Fun) of 
	      true -> 
		Annot = hipe_icode:info(I),
		{Code1, VarMap4, ConstTab2} =
		  hipe_rtl_primops:gen_primop(
		    {Fun, Dst, Args, ContLblName, FailLblName, Annot},
		    {VarMap3, ConstTab1},
		    {Options, ExitInfo}),
		{[Code1,ContLbl], VarMap4, ConstTab2};
	      false ->
		Call = hipe_rtl:mk_call(Dst, Fun, Args, 
					hipe_icode:call_type(I),
					ContLblName,
					FailLblName),
		{[Call,ContLbl], VarMap3, ConstTab1}
	    end;

	  Cont -> %% This call ends a BB.
	    {ContLbl, VarMap4} = 
	      hipe_rtl_varmap:icode_label2rtl_label(Cont,
						    VarMap3),
	    ContLblName = hipe_rtl:label_name(ContLbl),
	    case hipe_bif:is_bif(Fun) of 
	      true -> 
		Annot = hipe_icode:info(I),
		
		hipe_rtl_primops:gen_primop(
		  {Fun, Dst, Args, ContLblName, FailLblName, Annot},
		  {VarMap4, ConstTab1},
		  {Options, ExitInfo});
	      false ->
		Call = hipe_rtl:mk_call(Dst, Fun, Args, 
					hipe_icode:call_type(I),
					ContLblName,
					FailLblName),
		{Call, VarMap4, ConstTab1}
	    
	    end
	end
    end,
  {[InitCode,Code], VarMap5, ConstTab4}.


%% ____________________________________________________________________
%% ENTER
gen_enter(I, VarMap, Options, ExitInfo, ConstTab) ->
  Fun = hipe_icode:enter_fun(I),
  Annot = hipe_icode:info(I),

  {Args, VarMap1, ConstTab1, InitCode} = 
    args_to_vars(hipe_icode:enter_args(I), VarMap, ConstTab),

  {Code4, VarMap4, ConstTab4} =
    case hipe_bif:is_bif(Fun) of 
      true -> 
	case Fun of 
	  %% This is a tail-call to a closure
	  enter_fun ->
	    {hipe_rtl_primops:gen_enter_fun(Args, ExitInfo), VarMap1, ConstTab1};
	  _ ->
	    Dst = hipe_rtl:mk_new_var(),
	    OkLab = hipe_rtl:mk_new_label(),
	    {Code,VarMap2,ConstTab2} = 
	      hipe_rtl_primops:gen_primop({Fun,
					   [Dst],Args,  
					   hipe_rtl:label_name(OkLab), [],
					   Annot},
					  {VarMap1, ConstTab},
					  {Options, ExitInfo}),
	    {Code ++ [OkLab, hipe_rtl:mk_return([Dst])], VarMap2, ConstTab2}
	end;      
      false ->
	Enter = hipe_rtl:mk_enter(hipe_icode:enter_fun(I),
				  Args,hipe_icode:enter_type(I)),
	{Enter, VarMap1, ConstTab}
    end,
  {[InitCode,Code4], VarMap4, ConstTab4}.  


%% ____________________________________________________________________
%%
%% Generate code for the if-conditional
%%

gen_cond(CondOp, Args, TrueLbl, FalseLbl, Pred) ->
  Tmp = hipe_rtl:mk_new_reg(),
  GenLbl = hipe_rtl:mk_new_label(),
  TestRetLbl = hipe_rtl:mk_new_label(),
  TestRetName = hipe_rtl:label_name(TestRetLbl),
  %% Arity = length(Args),
  case CondOp of
    '=:=' ->
      [Arg1, Arg2] = Args,
      [hipe_rtl:mk_branch(Arg1, eq, Arg2, TrueLbl,
			  hipe_rtl:label_name(GenLbl), Pred),
       GenLbl,
       hipe_rtl:mk_call([Tmp], op_exact_eqeq_2, Args, c, TestRetName, []),
       TestRetLbl,
       hipe_rtl:mk_branch(Tmp, ne, hipe_rtl:mk_imm(0),
			  TrueLbl, FalseLbl, Pred)];
    '=/=' ->
      [Arg1, Arg2] = Args,
      [hipe_rtl:mk_branch(Arg1, eq, Arg2, FalseLbl,
			  hipe_rtl:label_name(GenLbl), 1-Pred),
       GenLbl,
       hipe_rtl:mk_call([Tmp], op_exact_eqeq_2, Args, c, TestRetName, []),
       TestRetLbl,
       hipe_rtl:mk_branch(Tmp, ne, hipe_rtl:mk_imm(0),
			  FalseLbl, TrueLbl, Pred)];
    '==' ->
      [Arg1, Arg2] = Args,
      [hipe_rtl:mk_branch(Arg1, eq, Arg2,
			  TrueLbl, hipe_rtl:label_name(GenLbl), Pred),
       GenLbl,
       hipe_rtl:mk_call([Tmp], cmp_2, Args, c, TestRetName, []),
       TestRetLbl,
       hipe_rtl:mk_branch(Tmp, eq, hipe_rtl:mk_imm(0),
			  TrueLbl, FalseLbl, Pred)];
    '/=' ->
      [Arg1, Arg2] = Args,
      [hipe_rtl:mk_branch(Arg1, eq, Arg2,
			  FalseLbl, hipe_rtl:label_name(GenLbl), 1-Pred),
       GenLbl,
       hipe_rtl:mk_call([Tmp], cmp_2, Args, c, TestRetName, []),
       TestRetLbl,
       hipe_rtl:mk_branch(Tmp, ne, hipe_rtl:mk_imm(0),
			  TrueLbl, FalseLbl, Pred)];
    '>' ->
      [Arg1, Arg2] = Args,
      [hipe_tagscheme:test_two_fixnums(Arg1, Arg2,
				       hipe_rtl:label_name(GenLbl)),
       hipe_tagscheme:fixnum_gt(Arg1, Arg2, TrueLbl, FalseLbl, Pred),
       GenLbl,
       hipe_rtl:mk_call([Tmp], cmp_2, Args, c, TestRetName, []),
       TestRetLbl,
       hipe_rtl:mk_branch(Tmp, gt, hipe_rtl:mk_imm(0),
			  TrueLbl, FalseLbl, Pred)];
    '<' ->
      [Arg1, Arg2] = Args,
      [hipe_tagscheme:test_two_fixnums(Arg1, Arg2,
				       hipe_rtl:label_name(GenLbl)),
       hipe_tagscheme:fixnum_lt(Arg1, Arg2, TrueLbl, FalseLbl, Pred),
       GenLbl,
       hipe_rtl:mk_call([Tmp], cmp_2, Args, c, TestRetName, []),
       TestRetLbl,
       hipe_rtl:mk_branch(Tmp, lt, hipe_rtl:mk_imm(0),
			  TrueLbl, FalseLbl, Pred)];
    '>=' ->
      [Arg1, Arg2] = Args,
      [hipe_tagscheme:test_two_fixnums(Arg1, Arg2,
				       hipe_rtl:label_name(GenLbl)),
       hipe_tagscheme:fixnum_ge(Arg1, Arg2, TrueLbl, FalseLbl, Pred),
       GenLbl,
       hipe_rtl:mk_call([Tmp], cmp_2, Args, c, TestRetName, []),
       TestRetLbl,
       hipe_rtl:mk_branch(Tmp, ge, hipe_rtl:mk_imm(0),
			  TrueLbl, FalseLbl, Pred)];
    '=<' ->
      [Arg1, Arg2] = Args,
      [hipe_tagscheme:test_two_fixnums(Arg1, Arg2,
				       hipe_rtl:label_name(GenLbl)),
       hipe_tagscheme:fixnum_le(Arg1, Arg2, TrueLbl, FalseLbl, Pred),
       GenLbl,
       hipe_rtl:mk_call([Tmp], cmp_2, Args, c, TestRetName, []),
       TestRetLbl,
       hipe_rtl:mk_branch(Tmp, le, hipe_rtl:mk_imm(0),
			  TrueLbl, FalseLbl, Pred)];
    mbox_empty ->
      [hipe_rtl_arch:call_bif([Tmp], CondOp, [], TestRetName, []),
       TestRetLbl,
       hipe_rtl:mk_branch(Tmp, ne, hipe_rtl:mk_imm(0),
			  TrueLbl, FalseLbl, Pred)];
    suspend_ok ->
      %% Check if the process really should suspend.
      %% set_timout(0) may indicate that the process should not suspend.
      Flags = hipe_rtl:mk_new_reg(),
      Timo = hipe_rtl:mk_new_reg(),
      ClearTimo = hipe_rtl:mk_new_label(),
      NewFlags = hipe_rtl:mk_new_reg(),
      [hipe_rtl_arch:pcb_load(Flags, ?P_FLAGS),
       hipe_rtl:mk_alub(Timo, Flags, 'and', hipe_rtl:mk_imm(?F_TIMO), 'ne',
			hipe_rtl:label_name(ClearTimo), TrueLbl, Pred),
       ClearTimo,
       hipe_rtl:mk_alu(NewFlags, Flags, 'xor', hipe_rtl:mk_imm(?F_TIMO)),
       hipe_rtl_arch:pcb_store(?P_FLAGS, NewFlags),
       hipe_rtl:mk_goto(FalseLbl)];
    _Other ->
      [hipe_rtl:mk_call([Tmp], CondOp, Args, c, TestRetName, []),
       TestRetLbl,
       hipe_rtl:mk_branch(Tmp, ne, hipe_rtl:mk_imm(0),
			  TrueLbl, FalseLbl, Pred)]
  end.

%% ____________________________________________________________________
%% SWITCH

%%
%% Rewrite switch_val to the equivalent icode if-then-else sequence,
%% then translate that sequence instead.
%% Doing this at the rtl level would generate the exact same code,
%% but would also require _a_lot_ more work.
%% (Don't believe me? Try it. I did, and threw the code away in disgust.
%% The main ugliness comes from (1) maintaining ConstTab for the constants
%% that may be added there [switch_val is not limited to immediates!],
%% (2) maintaining Map for the translated labels, and (3) expanding
%% equality tests to eq-or-call-primop-exact_eqeq_2.)
%%
%% TODO:
%% - separate immediate and non-immediate cases,
%%   and translate each list separately
%%
-ifdef(usesjumptable).
-define(uumess,?msg("~w Use jtab: ~w\n",
		    [Options,proplists:get_bool(use_jumptable, Options)])).
-else.
-define(uumess,ok).
-endif.
gen_switch_val(I, VarMap, ConstTab, Options, ExitInfo) ->

  %% If you want to see wheter jumptables are used or not...
  ?uumess,
  hipe_rtl_mk_switch:gen_switch_val(I, VarMap, ConstTab, Options, ExitInfo).


gen_switch_tuple(I, Map, ConstTab, Options, ExitInfo) ->
  hipe_rtl_mk_switch:gen_switch_tuple(I, Map, ConstTab, Options, ExitInfo).


%%
%% Translate a move where the source is a constant
%%
gen_const_move(Dst, Const, ConstTab) ->
  case hipe_icode:is_const_fun(Const) of
    true -> 
      gen_fun_move(Dst, hipe_icode:const_value(Const), ConstTab);
    false ->
      gen_untagged_const_move(Dst, hipe_icode:const_value(Const), ConstTab)
  end.
 
gen_fun_move(Dst, Fun, ConstTab) ->
  ?warning_msg("Funmove ~w! -- NYI\n",[Fun]),
  {NewTab, Label} = hipe_consttab:insert_fun(ConstTab, Fun),
  {[hipe_rtl:mk_load_address(Dst, Label, constant)],
   NewTab}.

gen_untagged_const_move(Dst, Const, ConstTab) ->
  case Const of
    [] ->
      Src = hipe_rtl:mk_imm(tagged_val_of([])),
      {hipe_rtl:mk_move(Dst, Src), ConstTab};
    %%      List when list(List) ->
    %%	 gen_const_list(Dst, List, ConstTab);
    X when is_integer(X) ->
      case hipe_tagscheme:is_fixnum(X) of
	true ->
	  Src = hipe_rtl:mk_imm(tagged_val_of(X)),
	  {hipe_rtl:mk_move(Dst, Src), ConstTab};
	false ->
	  gen_big_move(Dst, X, ConstTab)
      end;
    A when is_atom(A) ->
      {hipe_rtl:mk_load_atom(Dst, A), ConstTab};
    Big ->
      gen_big_move(Dst, Big, ConstTab)
  end.

gen_big_move(Dst, Big, ConstTab) ->
  {NewTab, Label} = hipe_consttab:insert_term(ConstTab, Big),
  %%  Tmp = hipe_rtl:mk_new_var(),
  {[hipe_rtl:mk_load_address(Dst, Label, constant)],
   %%  gen_tag(Dst, Tmp, type_of(Big))],
   NewTab}.

%% gen_tag(Dst, Addr, Type) ->
%%   case Type of
%%     tuple -> hipe_tagscheme:tag_tuple(Dst, Addr);
%%     cons -> hipe_tagscheme:tag_cons(Dst, Addr);
%%     float -> hipe_tagscheme:tag_flonum(Dst, Addr);
%%     bignum -> hipe_tagscheme:tag_bignum(Dst, Addr)
%%   end.

%% type_of(Term) ->
%%   if Term =:= [] -> nil;
%%      list(Term) -> cons;
%%      tuple(Term) -> tuple;
%%      integer(Term) ->
%%       case hipe_tagscheme:is_fixnum(Term) of
%% 	true -> fixnum;
%% 	false -> bignum
%%       end;
%%      float(Term) -> float
%%   end.

%% gen_const_list(Dst, [], ConstTab) ->
%%   gen_const_move(Dst, [], ConstTab);
%% gen_const_list(Dst, [C|Cs], ConstTab) ->
%%   Head = hipe_rtl:mk_new_reg(),
%%   Tail = hipe_rtl:mk_new_reg(),
%%   [Code = gen_const_list(Tail, Cs),
%%    gen_const_move(Head, C),
%%    gen_cons(Dst, [Head, Tail])].


%%
%% Make a tagged value of an erlang constant
%%

tagged_val_of([]) -> hipe_tagscheme:mk_nil();
tagged_val_of(X) when is_integer(X) -> hipe_tagscheme:mk_fixnum(X).

%%
%% Generate code for a typetest. If X is not of type Type then goto Label.
%%

gen_type_test(X, Type, TrueLbl, FalseLbl, Pred, ConstTab) ->
  case Type of
    nil ->
      {hipe_tagscheme:test_nil(X, TrueLbl, FalseLbl, Pred), ConstTab};
    cons ->
      {hipe_tagscheme:test_cons(X, TrueLbl, FalseLbl, Pred), ConstTab};
    list ->
      {hipe_tagscheme:test_list(X, TrueLbl, FalseLbl, Pred), ConstTab};
    float ->
      {hipe_tagscheme:test_flonum(X, TrueLbl, FalseLbl, Pred), ConstTab};
    fixnum ->
      {hipe_tagscheme:test_fixnum(X, TrueLbl, FalseLbl, Pred), ConstTab};
    integer ->
      {hipe_tagscheme:test_integer(X, TrueLbl, FalseLbl, Pred), ConstTab};
    {integer, N} when is_integer(N) -> 
      %% XXX: warning, does not work for bignums
      case hipe_tagscheme:is_fixnum(N) of
	true ->
	  Int = hipe_tagscheme:mk_fixnum(N),
	  {hipe_rtl:mk_branch(X, eq, hipe_rtl:mk_imm(Int),
			      TrueLbl, FalseLbl, Pred), 
	   ConstTab};
	false ->
	  BignumLbl = hipe_rtl:mk_new_label(),
	  RetLbl = hipe_rtl:mk_new_label(),
	  BigN = hipe_rtl:mk_new_var(),
	  Tmp = hipe_rtl:mk_new_var(),
	  {BigCode,NewConstTab} = gen_big_move(BigN, N, ConstTab),
	  {[hipe_tagscheme:test_fixnum(X, FalseLbl,
				       hipe_rtl:label_name(BignumLbl),1-Pred),
	    BignumLbl|BigCode]
	   ++
	   [hipe_rtl:mk_call([Tmp], op_exact_eqeq_2 , [X,BigN], c,
			     hipe_rtl:label_name(RetLbl),[]),
	    RetLbl,
	    hipe_rtl:mk_branch(Tmp, ne, hipe_rtl:mk_imm(0),
			       TrueLbl, FalseLbl, Pred)],
	   NewConstTab}
      end;
    {integer, NAI} -> exit({test_integer,not_an_integer,NAI}); 
    number ->
      {hipe_tagscheme:test_number(X, TrueLbl, FalseLbl, Pred), ConstTab};
    tuple ->
      {hipe_tagscheme:test_tuple(X, TrueLbl, FalseLbl, Pred), ConstTab};
    {tuple, N} ->
      {hipe_tagscheme:test_tuple_N(X, N, TrueLbl, FalseLbl, Pred), ConstTab};
    atom ->
      {hipe_tagscheme:test_atom(X, TrueLbl, FalseLbl, Pred), ConstTab};
    {atom, Atom} ->
      Tmp = hipe_rtl:mk_new_var(),
      {[hipe_rtl:mk_load_atom(Tmp, Atom),
	hipe_rtl:mk_branch(X, eq, Tmp, TrueLbl, FalseLbl, Pred)], ConstTab};
    bignum ->
      {hipe_tagscheme:test_bignum(X, TrueLbl, FalseLbl, Pred), ConstTab};
    pid ->
      {hipe_tagscheme:test_any_pid(X, TrueLbl, FalseLbl, Pred), ConstTab};
    port ->
      {hipe_tagscheme:test_any_port(X, TrueLbl, FalseLbl, Pred), ConstTab};
    reference ->
      {hipe_tagscheme:test_ref(X, TrueLbl, FalseLbl, Pred), ConstTab};
    function ->
      {hipe_tagscheme:test_fun(X, TrueLbl, FalseLbl, Pred), ConstTab};
    binary ->
      {hipe_tagscheme:test_binary(X, TrueLbl, FalseLbl, Pred), ConstTab};
    constant ->
      {hipe_tagscheme:test_constant(X, TrueLbl, FalseLbl, Pred), ConstTab};
    Other ->
      exit({?MODULE, {"unknown type", Other}})
  end.
