%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% Time-stamp: <01/06/14 11:20:36 happi>
%% ====================================================================
%%  Filename : 	hipe_rtl_guardops.erl
%%  Module   :	hipe_rtl_guardops
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-04-10 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: mikpe $
%%              $Date: 2001/09/10 16:23:54 $
%%              $Revision: 1.9 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_rtl_guardops).
-export([gen_guardop/4]).
%%-------------------------------------------------------------------------

-include("../main/hipe.hrl").
-include("hipe_icode2rtl.hrl").
-include("hipe_literals.hrl").

%%-------------------------------------------------------------------------

%%
%% Generate code for guardops. This is mostly a dispatch function
%%
gen_guardop(GOp, VarMap, Options, ConstTab) ->
  Op = hipe_icode:call_fun(GOp),
  {Dst, VarMap0} = hipe_rtl_varmap:ivs2rvs(hipe_icode:call_dst(GOp), VarMap),
  {Args, VarMap1} = hipe_rtl_varmap:ivs2rvs(hipe_icode:call_args(GOp), VarMap0),
  {TrueLbl, VarMap2} =
    hipe_rtl_varmap:icode_label2rtl_label(hipe_icode:call_continuation(GOp), VarMap1),
  {FalseLbl, VarMap3} =
    hipe_rtl_varmap:icode_label2rtl_label(hipe_icode:call_fail(GOp), VarMap2),
  TrueLblName = hipe_rtl:label_name(TrueLbl),
  FalseLblName = hipe_rtl:label_name(FalseLbl),

  Annot = hipe_icode:info(GOp),

  case Op of
    {hipe_bs_primop, BsOP} ->
      {Code, NewTab} =
	hipe_rtl_bs_ops:gen_rtl(BsOP,Args, Dst,TrueLblName,
				FalseLblName, ConstTab),
      {Code, VarMap3, NewTab};
    _ ->
      Code = 
	case Op of
	  '+' ->
	    gen_guard_add_sub_2(Dst, Args, Annot, Op, add,
				TrueLblName, FalseLblName);
	  '-' ->
	    gen_guard_add_sub_2(Dst, Args, Annot, Op, sub,
				TrueLblName, FalseLblName);
	  'band' ->
	    gen_guard_bitop_2(Dst, Args, Annot, Op, 'and',
			      TrueLblName, FalseLblName);
	  'bor' ->
	    gen_guard_bitop_2(Dst, Args, Annot, Op, 'or',
			      TrueLblName, FalseLblName);
	  'bxor' ->
	    gen_guard_bitop_2(Dst, Args, Annot, Op, 'xor',
			      TrueLblName, FalseLblName);
	  'bnot' ->
	    gen_guard_bnot_2(Dst, Args, Annot, Op,
			     TrueLblName, FalseLblName);

	  element ->
	    [Dst1] = Dst,
	    [Index, Tuple] = Args,
	    gen_guard_element_2(Dst1, Index, Tuple, Annot,
				TrueLblName, FalseLblName);

	  get_msg ->
	    ?EXIT({not_allowed_in_guards,Op});
	  next_msg ->
	    ?EXIT({not_allowed_in_guards,Op});
	  select_msg ->
	    ?EXIT({not_allowed_in_guards,Op});
	  set_timeout ->
	    ?EXIT({not_allowed_in_guards,Op});

	  _ ->
	    %% TODO: Check if the bif is alloed in a guard.
	    %%	{Mod,BifName,Arity} = Op,
	    %%	Fails = hipe_bif:fails(Arity,BifName),
	    %%	if Fails =:= true ->
	    gen_general_guard_op(Dst, Args,  Op, 
				 TrueLblName, FalseLblName)
	    %%	   true ->
	    %%	    [hipe_rtl:mk_call(Dst, Op, Args, c, TrueLblName,[])]
	    %%	end
	end,
      {Code, VarMap3, ConstTab}
  end.

%%
%% Generate a call to a guard operator (with inlined fail test)
%%

gen_general_guard_op(Res, Args, Op, TrueLbl,FalseLbl) ->
  %%  RetLabel =  hipe_rtl:mk_new_label(),
  [hipe_rtl:mk_call(Res, Op, Args, c, TrueLbl,FalseLbl)].
%%		    hipe_rtl:label_name(RetLabel), []),
%%   RetLabel,
%%   test_bif_result(Res, TrueLbl, FalseLbl)].

%% Test the result of a bifcall.
%% test_bif_result(Result, Continuation, Fail) ->
%%   [hipe_rtl:mk_branch(Result, eq, hipe_rtl:mk_imm(0), Fail, Continuation, 0.01)].



%% ____________________________________________________________________
%% 

gen_guard_add_sub_2([Res], Args, Annot, Op, AluOp,SuccLabel,FailLabel) ->
  [Arg1, Arg2] = Args,
  GenCaseLabel = hipe_rtl:mk_new_label(),
  [hipe_tagscheme:test_two_fixnums(Arg1, Arg2,
				   hipe_rtl:label_name(GenCaseLabel)),
   hipe_tagscheme:fixnum_addsub(AluOp, Arg1, Arg2, Res, GenCaseLabel)|
   gen_guardop_general_case(Res, Op, Args, SuccLabel, FailLabel, 
			    GenCaseLabel)].

gen_guardop_general_case(Res, Op, Args, SuccLabel, FailLabel, GenCaseLabel) ->
  RetLabel =  hipe_rtl:mk_new_label(),
  [hipe_rtl:mk_goto(SuccLabel),
   GenCaseLabel,
   gen_general_guard_op([Res], Args, Op, SuccLabel, FailLabel)].



gen_guard_bitop_2([Res], Args, Annot, Op, BitOp, EndLabel, FailLabel) ->
  [Arg1, Arg2] = Args,
  Arg1p = hipe_rtl:mk_new_reg(),
  Flag = hipe_rtl:mk_new_reg(),
  OtherLabel = hipe_rtl:mk_new_label(),
  [hipe_tagscheme:test_two_fixnums(Arg1, Arg2,
				   hipe_rtl:label_name(OtherLabel)),
   hipe_tagscheme:fixnum_andorxor(BitOp, Arg1, Arg2, Res),
   gen_guardop_general_case(Res, Op, Args, EndLabel, FailLabel, 
			    OtherLabel)].





%%
%% Inline guard 'not'.
%%

gen_guard_bnot_2([Res], Args, Annot, Op,EndLabel, FailLabel) ->
  [Arg] = Args,
  FixLabel = hipe_rtl:mk_new_label(),
  OtherLabel = hipe_rtl:mk_new_label(),
  [hipe_tagscheme:test_fixnum(Arg, hipe_rtl:label_name(FixLabel),
			      hipe_rtl:label_name(OtherLabel), 0.99),
   FixLabel,
   hipe_tagscheme:fixnum_not(Arg, Res),
   gen_guardop_general_case(Res, Op, Args, EndLabel, FailLabel, 
			    OtherLabel)].




%%
%% guard element
%%
gen_guard_element_2(Dst1, Index, Tuple, Annot, TrueLblName,
		    FailLbl) ->

  [hipe_tagscheme:element(Dst1, Index, Tuple, hipe_rtl:mk_label(FailLbl)),
   hipe_rtl:mk_goto(TrueLblName)
  ].

