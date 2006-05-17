%% -*- erlang-indent-level: 2 -*-
%%========================================================================
%%
%% Filename : hipe_tagscheme.erl
%% Note     : This is specific to Erlang 5.0 / R9.
%%
%% Modifications:
%%  020904: Happi - added support for external pids and ports.
%%     
%%========================================================================
%% $Id$
%%========================================================================

-module(hipe_tagscheme).

-export([mk_nil/0, mk_fixnum/1, mk_arityval/1, mk_non_value/0]).
-export([is_fixnum/1]).
-export([tag_tuple/2, tag_cons/2]).
-export([test_is_boxed/4, get_header/2]).
-export([test_nil/4, test_cons/4, test_flonum/4, test_fixnum/4,
	 test_tuple/4, test_atom/4, test_bignum/4, 
	 test_any_pid/4, test_any_port/4,
	 test_ref/4, test_fun/4, test_fun2/5, test_binary/4, test_list/4,
	 test_integer/4, test_number/4, test_constant/4, test_tuple_N/5]).
-export([realtag_fixnum/2, tag_fixnum/2, realuntag_fixnum/2, untag_fixnum/2]).
-export([test_two_fixnums/3, test_fixnums/4, unsafe_fixnum_add/3,
	 fixnum_gt/5, fixnum_lt/5, fixnum_ge/5, fixnum_le/5, fixnum_val/1,
	 fixnum_addsub/5, fixnum_andorxor/4, fixnum_not/2, fixnum_bsr/3]).
-export([unsafe_car/2, unsafe_cdr/2,
	 unsafe_constant_element/3, unsafe_update_element/3, element/6]).
-export([unsafe_closure_element/3]).
-export([mk_fun_header/0, tag_fun/2]).
-export([unsafe_untag_float/2, unsafe_tag_float/2]).
-export([unsafe_mk_sub_binary/4, unsafe_mk_float/3, unsafe_mk_big/3, unsafe_load_float/3]).
-export([safe_mk_sub_binary/4]).
-export([test_subbinary/3, test_heap_binary/3]).
-export([finalize_bin/4, get_base/2]).
-export([create_heap_binary/3, create_refc_binary/3]).
-export([get_erts_mb/1]).
-export([extract_offset/2, extract_binsize/2, extract_base/2, extract_orig/2]).
-export([update_offset/2, create_matchstate/6]).
-export([extract_slot/3, update_slot/3, extract_matchbuffer/2]).

-include("hipe_literals.hrl").

-ifdef(EFT_NATIVE_ADDRESS).
-export([if_fun_get_arity_and_address/5]).
-endif.

-undef(TAG_PRIMARY_BOXED).
-undef(TAG_IMMED2_MASK).
-undef(TAG_IMMED2_CATCH).
-undef(TAG_IMMED2_SIZE).

%%------------------------------------------------------------------------

-define(TAG_PRIMARY_SIZE,   2).
-define(TAG_PRIMARY_MASK,   16#3).
-define(TAG_PRIMARY_HEADER, 16#0).
-define(TAG_PRIMARY_LIST,   16#1).
-define(TAG_PRIMARY_BOXED,  16#2).
-define(TAG_PRIMARY_IMMED1, 16#3).

-define(TAG_IMMED1_SIZE,  4).
-define(TAG_IMMED1_MASK,  16#F).
-define(TAG_IMMED1_PID,   ((16#0 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_IMMED1)).
-define(TAG_IMMED1_PORT,  ((16#1 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_IMMED1)).
-define(TAG_IMMED1_IMMED2,((16#2 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_IMMED1)).
-define(TAG_IMMED1_SMALL, ((16#3 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_IMMED1)).

-define(TAG_IMMED2_SIZE,  6).
-define(TAG_IMMED2_MASK,  16#3F).
-define(TAG_IMMED2_ATOM,  ((16#0 bsl ?TAG_IMMED1_SIZE) bor ?TAG_IMMED1_IMMED2)).
-define(TAG_IMMED2_CATCH, ((16#1 bsl ?TAG_IMMED1_SIZE) bor ?TAG_IMMED1_IMMED2)).
-define(TAG_IMMED2_NIL,   ((16#3 bsl ?TAG_IMMED1_SIZE) bor ?TAG_IMMED1_IMMED2)).

-define(TAG_HEADER_ARITYVAL,((16#0 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(TAG_HEADER_BIN_MATCHSTATE,   ((16#1 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(TAG_HEADER_POS_BIG, ((16#2 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(TAG_HEADER_NEG_BIG, ((16#3 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(BIG_SIGN_BIT,	     (16#1 bsl ?TAG_PRIMARY_SIZE)).
-define(TAG_HEADER_REF,     ((16#4 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(TAG_HEADER_FUN,     ((16#5 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(TAG_HEADER_FLOAT,   ((16#6 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(BINARY_XXX_MASK,     (16#3 bsl ?TAG_PRIMARY_SIZE)).
-define(TAG_HEADER_REFC_BIN,((16#8 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(TAG_HEADER_HEAP_BIN,((16#9 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(TAG_HEADER_SUB_BIN, ((16#A bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(TAG_HEADER_EXTERNAL_PID, ((16#C bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(TAG_HEADER_EXTERNAL_PORT,((16#D bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(TAG_HEADER_EXTERNAL_REF, ((16#E bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).

-define(TAG_HEADER_MASK, 16#3F).
-define(HEADER_ARITY_OFFS, 6).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mk_header(SZ,TAG) -> (SZ bsl ?HEADER_ARITY_OFFS) + TAG.
mk_arityval(SZ)	-> mk_header(SZ, ?TAG_HEADER_ARITYVAL).

mk_fixnum(X) -> (X bsl ?TAG_IMMED1_SIZE) + ?TAG_IMMED1_SMALL.

-define(NIL, ((-1 bsl ?TAG_IMMED2_SIZE) bor ?TAG_IMMED2_NIL)).
mk_nil()	-> ?NIL.
%% mk_atom(X)	-> (X bsl ?TAG_IMMED2_SIZE) + ?TAG_IMMED2_ATOM.
mk_non_value()	-> ?THE_NON_VALUE.

is_fixnum(N) ->
  Bits     = (hipe_rtl_arch:word_size() * 8) - ?TAG_IMMED1_SIZE,
  (N =< ((1 bsl (Bits - 1)) - 1)) and (N >= -(1 bsl (Bits - 1))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(HEADER_FUN, mk_header(?ERL_FUN_SIZE-2,?TAG_HEADER_FUN)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tag_boxed(Res, X) ->
  hipe_rtl:mk_alu(Res, X, 'add', hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED)).

%% tag_bignum(Res, X) -> tag_boxed(Res, X).
tag_flonum(Res, X) -> tag_boxed(Res, X).
tag_tuple(Res, X) -> tag_boxed(Res, X).

tag_cons(Res, X) ->
  hipe_rtl:mk_alu(Res, X, 'add', hipe_rtl:mk_imm(?TAG_PRIMARY_LIST)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% write_catch_frame(SP, Off,  CatchLab) ->
%%   %% SP[Off] = make_catch(CatchLab)
%%   %% loader should transform the label to a catch table index,
%%   %% tag it, and emit a 'load constant' instruction
%%   CatchPC = hipe_rtl:mk_new_reg(),
%%   [hipe_rtl:mk_load_address(CatchPC, CatchLab, 'catch'),
%%    hipe_rtl:mk_store(SP, hipe_rtl:mk_imm(Off), CatchPC)].

%%% no longer needed
%tag_catch(Ix) -> (Ix bsl ?TAG_IMMED2_SIZE) bor ?TAG_IMMED2_CATCH.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Operations to test if an object has a known type T.

test_nil(X, TrueLab, FalseLab, Pred) ->
  hipe_rtl:mk_branch(X, eq, hipe_rtl:mk_imm(?NIL), TrueLab, FalseLab, Pred).

test_cons(X, TrueLab, FalseLab, Pred) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  Mask = hipe_rtl:mk_imm(?TAG_PRIMARY_MASK - ?TAG_PRIMARY_LIST),
  hipe_rtl:mk_alub(Tmp, X, 'and', Mask, 'eq', TrueLab, FalseLab, Pred).

test_is_boxed(X, TrueLab, FalseLab, Pred) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  Mask = hipe_rtl:mk_imm(?TAG_PRIMARY_MASK - ?TAG_PRIMARY_BOXED),
  hipe_rtl:mk_alub(Tmp, X, 'and', Mask, 'eq', TrueLab, FalseLab, Pred).

get_header(Res, X) ->
  hipe_rtl:mk_load(Res, X, hipe_rtl:mk_imm(-(?TAG_PRIMARY_BOXED))).

mask_and_compare(X, Mask, Value, TrueLab, FalseLab, Pred) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  [hipe_rtl:mk_alu(Tmp, X, 'and', hipe_rtl:mk_imm(Mask)),
   hipe_rtl:mk_branch(Tmp, 'eq', hipe_rtl:mk_imm(Value), TrueLab, FalseLab, Pred)].

test_immed1(X, Value, TrueLab, FalseLab, Pred) ->
  mask_and_compare(X, ?TAG_IMMED1_MASK, Value, TrueLab, FalseLab, Pred).

test_internal_pid(X, TrueLab, FalseLab, Pred) ->
  test_immed1(X, ?TAG_IMMED1_PID, TrueLab, FalseLab, Pred).

test_any_pid(X, TrueLab, FalseLab, Pred) ->
  NotInternalPidLab = hipe_rtl:mk_new_label(),
  [test_internal_pid(X, TrueLab, hipe_rtl:label_name(NotInternalPidLab), Pred),
   NotInternalPidLab,
   test_external_pid(X, TrueLab,FalseLab,Pred)].

test_external_pid(X, TrueLab, FalseLab, Pred) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  HalfTrueLab = hipe_rtl:mk_new_label(),
  ExternalPidMask = ?TAG_HEADER_MASK,
  [test_is_boxed(X, hipe_rtl:label_name(HalfTrueLab), FalseLab, Pred),
   HalfTrueLab,
   get_header(Tmp, X),
   mask_and_compare(Tmp, ExternalPidMask, ?TAG_HEADER_EXTERNAL_PID,
		    TrueLab, FalseLab, Pred)].

test_internal_port(X, TrueLab, FalseLab, Pred) ->
  test_immed1(X, ?TAG_IMMED1_PORT, TrueLab, FalseLab, Pred).

test_any_port(X, TrueLab, FalseLab, Pred) ->
  NotInternalPortLab = hipe_rtl:mk_new_label(),
  [test_internal_port(X, TrueLab, hipe_rtl:label_name(NotInternalPortLab), Pred),
   NotInternalPortLab,
   test_external_port(X, TrueLab,FalseLab,Pred)].

test_external_port(X, TrueLab, FalseLab, Pred) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  HalfTrueLab = hipe_rtl:mk_new_label(),
  ExternalPortMask = ?TAG_HEADER_MASK,
  [test_is_boxed(X, hipe_rtl:label_name(HalfTrueLab), FalseLab, Pred),
   HalfTrueLab,
   get_header(Tmp, X),
   mask_and_compare(Tmp, ExternalPortMask, ?TAG_HEADER_EXTERNAL_PORT,
		    TrueLab, FalseLab, Pred)].

test_fixnum(X, TrueLab, FalseLab, Pred) ->
  test_immed1(X, ?TAG_IMMED1_SMALL, TrueLab, FalseLab, Pred).

test_atom(X, TrueLab, FalseLab, Pred) ->
  mask_and_compare(X, ?TAG_IMMED2_MASK, ?TAG_IMMED2_ATOM,
		   TrueLab, FalseLab, Pred).

test_tuple(X, TrueLab, FalseLab, Pred) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  Tmp2 = hipe_rtl:mk_new_reg_gcsafe(),
  HalfTrueLab = hipe_rtl:mk_new_label(),
  [test_is_boxed(X, hipe_rtl:label_name(HalfTrueLab), FalseLab, Pred),
   HalfTrueLab,
   get_header(Tmp, X),
   hipe_rtl:mk_alub(Tmp2, Tmp, 'and', hipe_rtl:mk_imm(?TAG_HEADER_MASK), 'eq',
		    TrueLab, FalseLab, Pred)].

test_tuple_N(X, N, TrueLab, FalseLab, Pred) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  HalfTrueLab = hipe_rtl:mk_new_label(),
  [test_is_boxed(X, hipe_rtl:label_name(HalfTrueLab), FalseLab, Pred),
   HalfTrueLab,
   get_header(Tmp, X),
   hipe_rtl:mk_branch(Tmp, 'eq', hipe_rtl:mk_imm(mk_arityval(N)),
		      TrueLab, FalseLab, Pred)].

test_ref(X, TrueLab, FalseLab, Pred) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  HalfTrueLab = hipe_rtl:mk_new_label(),
  TwoThirdsTrueLab = hipe_rtl:mk_new_label(),
  [test_is_boxed(X, hipe_rtl:label_name(HalfTrueLab), FalseLab, Pred),
   HalfTrueLab,
   get_header(Tmp, X),
   mask_and_compare(Tmp, ?TAG_HEADER_MASK, ?TAG_HEADER_REF,
		    TrueLab, hipe_rtl:label_name(TwoThirdsTrueLab), Pred),
   TwoThirdsTrueLab,
   mask_and_compare(Tmp, ?TAG_HEADER_MASK, ?TAG_HEADER_EXTERNAL_REF,
		    TrueLab, FalseLab, Pred)
  ].

test_fun(X, TrueLab, FalseLab, Pred) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  HalfTrueLab = hipe_rtl:mk_new_label(),
  [test_is_boxed(X, hipe_rtl:label_name(HalfTrueLab), FalseLab, Pred),
   HalfTrueLab,
   get_header(Tmp, X),
   mask_and_compare(Tmp, ?TAG_HEADER_MASK, ?TAG_HEADER_FUN,
		    TrueLab, FalseLab, Pred)].

test_fun2(X, Arity, TrueLab, FalseLab, Pred) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  TFalse = hipe_rtl:mk_new_reg_gcsafe(),
  HalfTrueLab = hipe_rtl:mk_new_label(),
  [hipe_rtl:mk_call([Tmp], {erlang,is_function,2}, [X,Arity],
		    hipe_rtl:label_name(HalfTrueLab), FalseLab, 'not_remote'),
   HalfTrueLab,
   hipe_rtl:mk_load_atom(TFalse, 'false'),
   hipe_rtl:mk_branch(Tmp, 'ne', TFalse,
		      TrueLab, FalseLab, Pred)].

flonum_header() ->
  mk_header(8 div hipe_rtl_arch:word_size(), ?TAG_HEADER_FLOAT).

test_flonum(X, TrueLab, FalseLab, Pred) ->
  HeaderFlonum = flonum_header(),
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  HalfTrueLab = hipe_rtl:mk_new_label(),
  [test_is_boxed(X, hipe_rtl:label_name(HalfTrueLab), FalseLab, Pred),
   HalfTrueLab,
   get_header(Tmp, X),
   hipe_rtl:mk_branch(Tmp, 'eq', hipe_rtl:mk_imm(HeaderFlonum),
		      TrueLab, FalseLab, Pred)].

test_bignum(X, TrueLab, FalseLab, Pred) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  HalfTrueLab = hipe_rtl:mk_new_label(),
  BigMask = ?TAG_HEADER_MASK - ?BIG_SIGN_BIT,
  [test_is_boxed(X, hipe_rtl:label_name(HalfTrueLab), FalseLab, Pred),
   HalfTrueLab,
   get_header(Tmp, X),
   mask_and_compare(Tmp, BigMask, ?TAG_HEADER_POS_BIG,
		    TrueLab, FalseLab, Pred)].

test_binary(X, TrueLab, FalseLab, Pred) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  HalfTrueLab = hipe_rtl:mk_new_label(),
  Mask = ?TAG_HEADER_MASK - ?BINARY_XXX_MASK,
  [test_is_boxed(X, hipe_rtl:label_name(HalfTrueLab), FalseLab, Pred),
   HalfTrueLab,
   get_header(Tmp, X),
   mask_and_compare(Tmp, Mask, ?TAG_HEADER_REFC_BIN, TrueLab, FalseLab, Pred)].

test_list(X, TrueLab, FalseLab, Pred) ->
  Lab = hipe_rtl:mk_new_label(),
  [test_cons(X, TrueLab, hipe_rtl:label_name(Lab), 0.5),
   Lab,
   test_nil(X, TrueLab, FalseLab, Pred)].

test_integer(X, TrueLab, FalseLab, Pred) ->
  Lab = hipe_rtl:mk_new_label(),
  [test_fixnum(X, TrueLab, hipe_rtl:label_name(Lab), 0.5),
   Lab,
   test_bignum(X, TrueLab, FalseLab, Pred)].

test_number(X, TrueLab, FalseLab, Pred) ->
  Lab1 = hipe_rtl:mk_new_label(),
  Lab2 = hipe_rtl:mk_new_label(),
  Lab3 = hipe_rtl:mk_new_label(),
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  BigMask = ?TAG_HEADER_MASK - ?BIG_SIGN_BIT,
  HeaderFlonum = flonum_header(),
  [test_fixnum(X, TrueLab, hipe_rtl:label_name(Lab1), 0.5),
   Lab1,
   test_is_boxed(X, hipe_rtl:label_name(Lab2), FalseLab, 0.5),
   Lab2,
   get_header(Tmp, X),
   mask_and_compare(Tmp, BigMask, ?TAG_HEADER_POS_BIG,
		    TrueLab, hipe_rtl:label_name(Lab3), 0.5),
   Lab3,
   hipe_rtl:mk_branch(Tmp, 'eq', hipe_rtl:mk_imm(HeaderFlonum),
		      TrueLab, FalseLab, Pred)].

%% CONS, NIL, and TUPLE are not constants, everything else is
test_constant(X, TrueLab, FalseLab, Pred) ->
  Lab1 = hipe_rtl:mk_new_label(),
  Lab2 = hipe_rtl:mk_new_label(),
  Pred1 = 1-Pred,
  [test_cons(X, FalseLab, hipe_rtl:label_name(Lab1), Pred1),
   Lab1,
   test_nil(X, FalseLab, hipe_rtl:label_name(Lab2), Pred1),
   Lab2,
   test_tuple(X, FalseLab, TrueLab, Pred1)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tag_fixnum(DestVar, SrcReg) ->
  [hipe_rtl:mk_fixnumop(DestVar, SrcReg, tag)].
%% [hipe_rtl:mk_alu(DestVar, SrcReg, sll, hipe_rtl:mk_imm(?TAG_IMMED1_SIZE)),
%%   hipe_rtl:mk_alu(DestVar, DestVar, add, hipe_rtl:mk_imm(?TAG_IMMED1_SMALL))].

realtag_fixnum(DestVar, SrcReg) ->
  [hipe_rtl:mk_alu(DestVar, SrcReg, sll, hipe_rtl:mk_imm(?TAG_IMMED1_SIZE)),
   hipe_rtl:mk_alu(DestVar, DestVar, add, hipe_rtl:mk_imm(?TAG_IMMED1_SMALL))].


untag_fixnum(DestReg, SrcVar) ->
  hipe_rtl:mk_fixnumop(DestReg, SrcVar, untag).
%%  hipe_rtl:mk_alu(DestReg, SrcVar, 'sra', hipe_rtl:mk_imm(?TAG_IMMED1_SIZE)).

realuntag_fixnum(DestReg, SrcVar) ->
  hipe_rtl:mk_alu(DestReg, SrcVar, 'sra', hipe_rtl:mk_imm(?TAG_IMMED1_SIZE)).

fixnum_val(Fixnum) ->
  Fixnum bsr ?TAG_IMMED1_SIZE.

test_fixnums(Args, TrueLab, FalseLab, Pred)->
  {Reg, Ands} = test_fixnums_1(Args, []),
  Ands ++ [test_fixnum(Reg, TrueLab, FalseLab, Pred)].

test_fixnums_1([Arg1, Arg2], Acc)->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  {Tmp, lists:reverse([hipe_rtl:mk_alu(Tmp, Arg1, 'and', Arg2)|Acc])};
test_fixnums_1([Arg1, Arg2|Args], Acc)->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  test_fixnums_1([Tmp|Args], [hipe_rtl:mk_alu(Tmp, Arg1, 'and', Arg2)|Acc]).

test_two_fixnums(Arg1, Arg2, FalseLab) ->
  TrueLab = hipe_rtl:mk_new_label(),
  case hipe_rtl:is_imm(Arg2) of
    true ->
      Value = hipe_rtl:imm_value(Arg2),
      case Value band ?TAG_IMMED1_MASK of
	?TAG_IMMED1_SMALL ->
	  [test_fixnum(Arg1, hipe_rtl:label_name(TrueLab), FalseLab, 0.99),
	   TrueLab];
	_ ->
	  [hipe_rtl:mk_goto(FalseLab)]
      end;
    false ->
      Tmp = hipe_rtl:mk_new_reg_gcsafe(),
      [hipe_rtl:mk_alu(Tmp, Arg1, 'and', Arg2),
       test_fixnum(Tmp, hipe_rtl:label_name(TrueLab), FalseLab, 0.99),
       TrueLab]
  end.

fixnum_cmp(Arg1, Arg2, TrueLab, FalseLab, Pred, CmpOp) ->
  hipe_rtl:mk_branch(Arg1, CmpOp, Arg2, TrueLab, FalseLab, Pred).

fixnum_gt(Arg1, Arg2, TrueLab, FalseLab, Pred) ->
  fixnum_cmp(Arg1, Arg2, TrueLab, FalseLab, Pred, gt).

fixnum_lt(Arg1, Arg2, TrueLab, FalseLab, Pred) ->
  fixnum_cmp(Arg1, Arg2, TrueLab, FalseLab, Pred, lt).

fixnum_ge(Arg1, Arg2, TrueLab, FalseLab, Pred) ->
  fixnum_cmp(Arg1, Arg2, TrueLab, FalseLab, Pred, ge).

fixnum_le(Arg1, Arg2, TrueLab, FalseLab, Pred) ->
  fixnum_cmp(Arg1, Arg2, TrueLab, FalseLab, Pred, le).

%% We know the answer will be a fixnum
unsafe_fixnum_add(Arg1, Arg2, Res) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  [hipe_rtl:mk_alu(Tmp, Arg2, sub, 
		   hipe_rtl:mk_imm(?TAG_IMMED1_SMALL)),
   hipe_rtl:mk_alu(Res, Arg1, add, Tmp)].

%%% (16X+tag)+((16Y+tag)-tag) = 16X+tag+16Y = 16(X+Y)+tag
%%% (16X+tag)-((16Y+tag)-tag) = 16X+tag-16Y = 16(X-Y)+tag
fixnum_addsub(AluOp, Arg1, Arg2, Res, OtherLab) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  %% XXX: Consider moving this test to the users of fixnum_addsub.
  case Arg1 =/= Res andalso Arg2 =/= Res of 
    true -> 
      %% Args differ from res.
      NoOverflowLab = hipe_rtl:mk_new_label(),
      [hipe_rtl:mk_alu(Tmp, Arg2, sub, 
		       hipe_rtl:mk_imm(?TAG_IMMED1_SMALL)),
       hipe_rtl:mk_alub(Res, Arg1, AluOp, Tmp, not_overflow,
			hipe_rtl:label_name(NoOverflowLab), 
			hipe_rtl:label_name(OtherLab), 0.99),
       NoOverflowLab];
    false ->
      %% At least one of the arguments is the same as Res.
      Tmp2 = hipe_rtl:mk_new_var(), % XXX: shouldn't this var be a reg?
      NoOverflowLab = hipe_rtl:mk_new_label(),
      [hipe_rtl:mk_alu(Tmp, Arg2, sub, 
		       hipe_rtl:mk_imm(?TAG_IMMED1_SMALL)),
       hipe_rtl:mk_alub(Tmp2, Arg1, AluOp, Tmp, not_overflow,
			hipe_rtl:label_name(NoOverflowLab), 
			hipe_rtl:label_name(OtherLab), 0.99),
       NoOverflowLab,
       hipe_rtl:mk_move(Res, Tmp2)]
  end.

%%% ((16X+tag) div 16) * ((16Y+tag)-tag) + tag = X*16Y+tag = 16(XY)+tag
%% fixnum_mul(Arg1, Arg2, Res, OtherLab) ->
%%   Tmp = hipe_rtl:mk_new_reg_gcsafe(),
%%   U1 = hipe_rtl:mk_new_reg_gcsafe(),
%%   U2 = hipe_rtl:mk_new_reg_gcsafe(),
%%   NoOverflowLab = hipe_rtl:mk_new_label(),
%%   [untag_fixnum(U1, Arg1),
%%    hipe_rtl:mk_alu(U2, Arg2, 'sub', hipe_rtl:mk_imm(?TAG_IMMED1_SMALL)),
%%    hipe_rtl:mk_alub(Tmp, U1, 'mul', U2, overflow, hipe_rtl:label_name(OtherLab),
%% 		    hipe_rtl:label_name(NoOverflowLab), 0.01),
%%    NoOverflowLab,
%%    hipe_rtl:mk_alu(Res, Tmp, 'add', hipe_rtl:mk_imm(?TAG_IMMED1_SMALL))].

fixnum_andorxor(AluOp, Arg1, Arg2, Res) ->
  case AluOp of
    'xor' ->
      Tmp = hipe_rtl:mk_new_reg_gcsafe(),
      [hipe_rtl:mk_alu(Tmp, Arg1, 'xor', Arg2),	% clears tag :-(
       hipe_rtl:mk_alu(Res, Tmp, 'or', hipe_rtl:mk_imm(?TAG_IMMED1_SMALL))];
    _ -> hipe_rtl:mk_alu(Res, Arg1, AluOp, Arg2)
  end.

fixnum_not(Arg, Res) ->
  Mask = (-1 bsl ?TAG_IMMED1_SIZE),
  hipe_rtl:mk_alu(Res, Arg, 'xor', hipe_rtl:mk_imm(Mask)).

fixnum_bsr(Arg1, Arg2, Res) ->
  Tmp1 = hipe_rtl:mk_new_reg_gcsafe(),
  Tmp2 = hipe_rtl:mk_new_reg_gcsafe(),
  [untag_fixnum(Tmp1, Arg2),
   hipe_rtl:mk_alu(Tmp2, Arg1, 'sra', Tmp1),
   hipe_rtl:mk_alu(Res, Tmp2, 'or', hipe_rtl:mk_imm(?TAG_IMMED1_SMALL))].
   
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unsafe_car(Dst, Arg) ->
  hipe_rtl:mk_load(Dst, Arg, hipe_rtl:mk_imm(-(?TAG_PRIMARY_LIST))).

unsafe_cdr(Dst, Arg) ->
  WordSize = hipe_rtl_arch:word_size(),
  hipe_rtl:mk_load(Dst, Arg, hipe_rtl:mk_imm(-(?TAG_PRIMARY_LIST)+WordSize)).

unsafe_constant_element(Dst, Index, Tuple) ->	% Index is an immediate
  WordSize = hipe_rtl_arch:word_size(),
  Offset = -(?TAG_PRIMARY_BOXED) + WordSize * hipe_rtl:imm_value(Index),
  hipe_rtl:mk_load(Dst, Tuple, hipe_rtl:mk_imm(Offset)).

unsafe_update_element(Tuple, Index, Value) ->   % Index is an immediate
  WordSize = hipe_rtl_arch:word_size(),
  Offset = -(?TAG_PRIMARY_BOXED) + WordSize * hipe_rtl:imm_value(Index),
  hipe_rtl:mk_store(Tuple, hipe_rtl:mk_imm(Offset), Value).

%%% wrong semantics
% unsafe_variable_element(Dst, Index, Tuple) ->	% Index is an unknown fixnum
%     %% Load word at (Tuple - 2) + ((Index >> 4) << 2).
%     %% Offset = ((Index >> 4) << 2) - 2.
%     %% Index = x..x1111 (fixnum tag is 2#1111).
%     %% (Index >> 2) = 00x..x11 and ((Index >> 4) << 2) = 00x..x00.
%     %% Therefore, ((Index >> 4) << 2) = (Index >> 2) - 3.
%     %% So Offset = ((Index >> 4) << 2) - 2 = (Index >> 2) - (3 + 2).
%     Tmp1 = hipe_rtl:mk_new_reg_gcsafe(),
%     Tmp2 = hipe_rtl:mk_new_reg_gcsafe(),
%     Shift = ?TAG_IMMED1_SIZE - 2,
%     OffAdj = (?TAG_IMMED1_SMALL bsr Shift) + ?TAG_PRIMARY_BOXED,
%     [hipe_rtl:mk_alu(Tmp1, Index, 'srl', hipe_rtl:mk_imm(Shift)),
%      hipe_rtl:mk_alu(Tmp2, Tmp1, 'sub', hipe_rtl:mk_imm(OffAdj)),
%      hipe_rtl:mk_load(Dst, Tuple, Tmp2)].

element(Dst, Index, Tuple, FailLabName, {tuple, A}, IndexInfo) ->
  FixnumOkLab = hipe_rtl:mk_new_label(),
  IndexOkLab = hipe_rtl:mk_new_label(),
  Ptr = hipe_rtl:mk_new_reg(), % offset from Tuple
  UIndex = hipe_rtl:mk_new_reg_gcsafe(),
  Arity = hipe_rtl:mk_imm(A),
  InvIndex = hipe_rtl:mk_new_reg_gcsafe(),
  Offset = hipe_rtl:mk_new_reg_gcsafe(),

  case IndexInfo of
    valid ->
      %% This is no branch, 1 load and 3 alus = 4 instr
      [untag_fixnum(UIndex, Index),
       hipe_rtl:mk_alu(Ptr, Tuple, 'sub', hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED)),
       hipe_rtl:mk_alu(Offset, UIndex, 'sll', 
		       hipe_rtl:mk_imm(hipe_rtl_arch:log2_word_size())),
       hipe_rtl:mk_load(Dst, Ptr, Offset)];
    fixnums ->
      %% This is 1 branch, 1 load and 4 alus = 6 instr
      [untag_fixnum(UIndex, Index),
       hipe_rtl:mk_alu(Ptr, Tuple, 'sub',hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED))|
       gen_element_tail(Dst, Ptr, InvIndex, Arity, Offset, UIndex, 
			FailLabName, IndexOkLab)];
    _ ->
      %% This is 3 branches, 1 load and 5 alus = 9 instr
      [test_fixnum(Index, hipe_rtl:label_name(FixnumOkLab),
		   FailLabName, 0.99),
       FixnumOkLab,
       untag_fixnum(UIndex, Index),
       hipe_rtl:mk_alu(Ptr, Tuple, 'sub',hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED))|
       gen_element_tail(Dst, Ptr, InvIndex, Arity, Offset, UIndex, 
			FailLabName, IndexOkLab)]
  end;
element(Dst, Index, Tuple, FailLabName, tuple, IndexInfo) ->
  FixnumOkLab = hipe_rtl:mk_new_label(),
  IndexOkLab = hipe_rtl:mk_new_label(),
  Ptr = hipe_rtl:mk_new_reg(), % offset from Tuple
  Header = hipe_rtl:mk_new_reg_gcsafe(),
  UIndex = hipe_rtl:mk_new_reg_gcsafe(),
  Arity = hipe_rtl:mk_new_reg_gcsafe(),
  InvIndex = hipe_rtl:mk_new_reg_gcsafe(),
  Offset = hipe_rtl:mk_new_reg_gcsafe(),
  
  case IndexInfo of
    fixnums ->
      %% This is 1 branch, 2 loads and 5 alus = 8 instr
      [hipe_rtl:mk_alu(Ptr, Tuple, 'sub', hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED)),
       hipe_rtl:mk_load(Header, Ptr, hipe_rtl:mk_imm(0)),
       untag_fixnum(UIndex, Index),
       hipe_rtl:mk_alu(Arity,Header,'srl',hipe_rtl:mk_imm(?HEADER_ARITY_OFFS))|
       gen_element_tail(Dst, Ptr, InvIndex, Arity, Offset, UIndex, 
			FailLabName, IndexOkLab)];
    Num when is_integer(Num) ->
      %% This is 1 branch, 1 load and 3 alus = 5 instr
      [hipe_rtl:mk_alu(Ptr, Tuple, 'sub', hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED))|
       gen_element_tail(Dst, Ptr, InvIndex, hipe_rtl:mk_imm(Num), 
			Offset, UIndex, FailLabName, IndexOkLab)];
    _ ->
      %% This is 2 branches, 2 loads and 6 alus = 10 instr
      [test_fixnum(Index, hipe_rtl:label_name(FixnumOkLab),
		   FailLabName, 0.99),
       FixnumOkLab,      
       hipe_rtl:mk_alu(Ptr, Tuple, 'sub', hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED)),
       hipe_rtl:mk_load(Header, Ptr, hipe_rtl:mk_imm(0)),
       untag_fixnum(UIndex, Index),
       hipe_rtl:mk_alu(Arity,Header,'srl',hipe_rtl:mk_imm(?HEADER_ARITY_OFFS))|
       gen_element_tail(Dst, Ptr, InvIndex, Arity, Offset, UIndex, 
			FailLabName, IndexOkLab)]
  end;
element(Dst, Index, Tuple, FailLabName, _TupleInfo, IndexInfo) ->
  FixnumOkLab = hipe_rtl:mk_new_label(),
  BoxedOkLab = hipe_rtl:mk_new_label(),
  TupleOkLab = hipe_rtl:mk_new_label(),
  IndexOkLab = hipe_rtl:mk_new_label(),
  Ptr = hipe_rtl:mk_new_reg(), % offset from Tuple
  Header = hipe_rtl:mk_new_reg_gcsafe(),
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  UIndex = hipe_rtl:mk_new_reg_gcsafe(),
  Arity = hipe_rtl:mk_new_reg_gcsafe(),
  InvIndex = hipe_rtl:mk_new_reg_gcsafe(),
  Offset = hipe_rtl:mk_new_reg_gcsafe(),

  case IndexInfo of
    fixnums ->
      %% This is 3 branches, 2 loads and 5 alus = 10 instr
      [test_is_boxed(Tuple, hipe_rtl:label_name(BoxedOkLab),
		     FailLabName, 0.99),
       BoxedOkLab,
       hipe_rtl:mk_alu(Ptr, Tuple, 'sub', hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED)),
       hipe_rtl:mk_load(Header, Ptr, hipe_rtl:mk_imm(0)),
       hipe_rtl:mk_alub(Tmp, Header, 'and', 
			hipe_rtl:mk_imm(?TAG_HEADER_MASK), 'eq',
			hipe_rtl:label_name(TupleOkLab), FailLabName, 0.99),
       TupleOkLab,
       untag_fixnum(UIndex, Index),
       hipe_rtl:mk_alu(Arity, Header, 'srl',
		       hipe_rtl:mk_imm(?HEADER_ARITY_OFFS))|
       gen_element_tail(Dst, Ptr, InvIndex, Arity, Offset, 
			UIndex, FailLabName, IndexOkLab)];

    Num when is_integer(Num) ->
      %% This is 3 branches, 2 loads and 4 alus = 9 instr
      [test_is_boxed(Tuple, hipe_rtl:label_name(BoxedOkLab),
		     FailLabName, 0.99),
       BoxedOkLab,
       hipe_rtl:mk_alu(Ptr, Tuple, 'sub', hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED)),
       hipe_rtl:mk_load(Header, Ptr, hipe_rtl:mk_imm(0)),
       hipe_rtl:mk_alub(Tmp, Header, 'and', 
			hipe_rtl:mk_imm(?TAG_HEADER_MASK), 'eq',
			hipe_rtl:label_name(TupleOkLab), FailLabName, 0.99),
       TupleOkLab,
       hipe_rtl:mk_alu(Arity, Header, 'srl', 
		       hipe_rtl:mk_imm(?HEADER_ARITY_OFFS))|
       gen_element_tail(Dst, Ptr, InvIndex, Arity, Offset, 
			hipe_rtl:mk_imm(Num), FailLabName, IndexOkLab)];
    _ ->
      %% This is 4 branches, 2 loads, and 6 alus = 12 instr :(
      [test_fixnum(Index, hipe_rtl:label_name(FixnumOkLab),
		   FailLabName, 0.99),
       FixnumOkLab,      
       test_is_boxed(Tuple, hipe_rtl:label_name(BoxedOkLab),
		     FailLabName, 0.99),
       BoxedOkLab,
       hipe_rtl:mk_alu(Ptr, Tuple, 'sub', hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED)),
       hipe_rtl:mk_load(Header, Ptr, hipe_rtl:mk_imm(0)),
       hipe_rtl:mk_alub(Tmp, Header, 'and', 
			hipe_rtl:mk_imm(?TAG_HEADER_MASK), 'eq',
			hipe_rtl:label_name(TupleOkLab), FailLabName, 0.99),
       TupleOkLab,
       untag_fixnum(UIndex, Index),
       hipe_rtl:mk_alu(Arity, Header, 'srl',
		       hipe_rtl:mk_imm(?HEADER_ARITY_OFFS))|
       gen_element_tail(Dst, Ptr, InvIndex, Arity, Offset, UIndex, 
			FailLabName, IndexOkLab)]
  end.

gen_element_tail(Dst, Ptr, InvIndex, Arity, Offset, 
		 UIndex, FailLabName, IndexOkLab)->
  %% now check that 1 <= UIndex <= Arity
  %% if UIndex < 1, then (Arity - UIndex) >= Arity
  %% if UIndex > Arity, then (Arity - UIndex) < 0, which is >=u Arity
  %% otherwise, 0 <= (Arity - UIndex) < Arity
  [hipe_rtl:mk_alu(InvIndex, Arity, 'sub', UIndex),
   hipe_rtl:mk_branch(InvIndex, 'geu', Arity, FailLabName,
		      hipe_rtl:label_name(IndexOkLab), 0.01),
   IndexOkLab,
   hipe_rtl:mk_alu(Offset, UIndex, 'sll',
                   hipe_rtl:mk_imm(hipe_rtl_arch:log2_word_size())),
   hipe_rtl:mk_load(Dst, Ptr, Offset)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unsafe_closure_element(Dst, Index, Closure) ->	% Index is an immediate
  Offset = -(?TAG_PRIMARY_BOXED)    %% Untag
    + ?EFT_ENV                        %% Field offset
  %% Index from 1 to N hence -1)
    + (hipe_rtl_arch:word_size() * (hipe_rtl:imm_value(Index)-1)),
  hipe_rtl:mk_load(Dst, Closure, hipe_rtl:mk_imm(Offset)).

mk_fun_header() ->
  hipe_rtl:mk_imm(?HEADER_FUN).

tag_fun(Res, X) ->
  tag_boxed(Res, X).

%% untag_fun(Res, X) ->
%%   hipe_rtl:mk_alu(Res, X, 'sub', hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED)).

-ifdef(EFT_NATIVE_ADDRESS).
if_fun_get_arity_and_address(ArityReg, AddressReg, FunP, BadFunLab, Pred) ->
%%    EmuAddressPtrReg = hipe_rtl:mk_new_reg(),
%%    FEPtrReg = hipe_rtl:mk_new_reg(),
%%    ArityReg = hipe_rtl:mk_new_reg(),
%%    NumFreeReg = hipe_rtl:mk_new_reg(),
%%    RealArityReg = hipe_rtl:mk_new_reg(),
  TrueLab0 = hipe_rtl:mk_new_label(),
%%    TrueLab1 = hipe_rtl:mk_new_label(),
  
  IsFunCode = test_fun(FunP, hipe_rtl:label_name(TrueLab0), BadFunLab, Pred),
  GetArityCode =
    [TrueLab0,
     %% Funp->arity contains the arity
     hipe_rtl:mk_load(ArityReg, FunP,
		      hipe_rtl:mk_imm(-(?TAG_PRIMARY_BOXED)+
				      ?EFT_ARITY)),
     hipe_rtl:mk_load(AddressReg, FunP,
		      hipe_rtl:mk_imm(-(?TAG_PRIMARY_BOXED)+
				      ?EFT_NATIVE_ADDRESS))],
  IsFunCode ++ GetArityCode.
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Floating point stuff.
%%

unsafe_untag_float(Dst, Src) ->
  Offset = -(?TAG_PRIMARY_BOXED) + hipe_rtl_arch:word_size(),
  [hipe_rtl:mk_fload(Dst, Src, hipe_rtl:mk_imm(Offset))].

unsafe_tag_float(Dst, Src) ->
  {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  Head = hipe_rtl:mk_imm(flonum_header()),
  WordSize = hipe_rtl_arch:word_size(),
  [GetHPInsn,
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(0), Head),
   hipe_rtl:mk_fstore(HP, hipe_rtl:mk_imm(WordSize), Src),
   tag_flonum(Dst, HP),
   hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(WordSize+8)),
   PutHPInsn].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Binary stuff
%%
create_heap_binary(Base, Size, Dst) when is_integer(Size) ->
  {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  WordSize = hipe_rtl_arch:word_size(),
  NoWords=(Size + 3*WordSize-1) div WordSize,
  NoBytes = NoWords*WordSize,
  HeapBinHeader = hipe_rtl:mk_imm(mk_header(NoWords-1, 
					    ?TAG_HEADER_HEAP_BIN)),
  [GetHPInsn,
   tag_boxed(Dst, HP),
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(?HEAP_BIN_THING_WORD), HeapBinHeader),
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(?HEAP_BIN_SIZE), hipe_rtl:mk_imm(Size)),
   hipe_rtl:mk_alu(Base, HP, add, hipe_rtl:mk_imm(?HEAP_BIN_DATA)),
   hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(NoBytes)),
   PutHPInsn];

create_heap_binary(Base, Size, Dst) ->
  {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  WordSize = hipe_rtl_arch:word_size(),
  Log2WordSize = hipe_rtl_arch:log2_word_size(),
  EvenWordSize = hipe_rtl:mk_new_reg_gcsafe(),
  Tmp1 = hipe_rtl:mk_new_reg_gcsafe(),
  Tmp2 = hipe_rtl:mk_new_reg_gcsafe(),
  Header = hipe_rtl:mk_new_reg_gcsafe(),
  Tmp3 = hipe_rtl:mk_new_reg(), % offset from HP
  Tmp4 = hipe_rtl:mk_new_reg(), % offset from HP
  [GetHPInsn,
   hipe_rtl:mk_alu(Tmp1, Size, add, hipe_rtl:mk_imm(WordSize-1)),
   hipe_rtl:mk_alu(EvenWordSize, Tmp1, sra, hipe_rtl:mk_imm(Log2WordSize)),
   hipe_rtl:mk_alu(Tmp2, EvenWordSize, add, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(Base, HP, add, hipe_rtl:mk_imm(?HEAP_BIN_DATA)),
   mk_var_header(Header, Tmp2, ?TAG_HEADER_HEAP_BIN),
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(?HEAP_BIN_THING_WORD), Header),
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(?HEAP_BIN_SIZE), Size),
   tag_boxed(Dst, HP),
   hipe_rtl:mk_alu(Tmp3, HP, add, Size),
   hipe_rtl:mk_alu(Tmp4, Tmp3, add, hipe_rtl:mk_imm(3*WordSize-1)),
   hipe_rtl:mk_alu(HP, Tmp4, 'and', hipe_rtl:mk_imm(-WordSize)),
   PutHPInsn].

create_refc_binary(Base, Size, Dst) ->
  {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  ProcBinHeader = hipe_rtl:mk_imm(mk_header((?PROC_BIN_WORDSIZE)-1, 
					    ?TAG_HEADER_REFC_BIN)),
  WordSize = hipe_rtl_arch:word_size(),
  Tmp2 = hipe_rtl:mk_new_reg(), % offset from Base
  [GetHPInsn,
   tag_boxed(Dst, HP),
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(?PROC_BIN_THING_WORD), ProcBinHeader),
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(?PROC_BIN_BINSIZE), Size),
   heap_arch_spec(HP),
   hipe_rtl:mk_alu(Tmp2, Base, sub, hipe_rtl:mk_imm(?BINARY_ORIG_BYTES)),
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(?PROC_BIN_VAL), Tmp2),
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(?PROC_BIN_BYTES), Base),
   hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(?PROC_BIN_WORDSIZE*WordSize)),
   PutHPInsn].

finalize_bin(Dst, Base, Offset, TrueLblName) ->
  {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  WordSize = hipe_rtl_arch:word_size(),
  WordSizeShift = hipe_rtl_arch:log2_word_size(),
  TmpOffset = hipe_rtl:mk_new_reg_gcsafe(),
  Tmp2 = hipe_rtl:mk_new_reg(), % offset from Base
  HeapLbl = hipe_rtl:mk_new_label(),
  REFCLbl = hipe_rtl:mk_new_label(),
  ProcBinHeader = hipe_rtl:mk_imm(mk_header((?PROC_BIN_WORDSIZE)-1, 
					    ?TAG_HEADER_REFC_BIN)),
  
  [GetHPInsn,
   tag_boxed(Dst, HP),
   hipe_rtl:mk_alu(TmpOffset, Offset, sra, hipe_rtl:mk_imm(3)),
   hipe_rtl:mk_branch(TmpOffset, le, hipe_rtl:mk_imm(?MAX_HEAP_BIN_SIZE), 
		      hipe_rtl:label_name(HeapLbl), hipe_rtl:label_name(REFCLbl)),
   HeapLbl,
   hipe_rtl:mk_alu(HP, HP, add, TmpOffset),
   hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(2*WordSize+(WordSize-1))),
   %%hipe_rtl:mk_alu(HP, Tmp2, 'and', hipe_rtl:mk_imm (-WordSize)),
   hipe_rtl:mk_alu(HP, HP, srl, hipe_rtl:mk_imm(WordSizeShift)),
   hipe_rtl:mk_alu(HP, HP, sll, hipe_rtl:mk_imm(WordSizeShift)),
   PutHPInsn,
   hipe_rtl:mk_goto(TrueLblName),
   REFCLbl,
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(?PROC_BIN_THING_WORD), ProcBinHeader),
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(?PROC_BIN_BINSIZE), TmpOffset),
   heap_arch_spec(HP),
   hipe_rtl:mk_alu(Tmp2, Base, sub, hipe_rtl:mk_imm(?BINARY_ORIG_BYTES)),
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(?PROC_BIN_VAL), Tmp2),
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(?PROC_BIN_BYTES), Base),
   hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(?PROC_BIN_WORDSIZE*WordSize)),
   PutHPInsn,
   hipe_rtl:mk_goto(TrueLblName)].

heap_arch_spec(HP) ->
  Tmp1 = hipe_rtl:mk_new_reg(), % MSO state
  [hipe_rtl_arch:pcb_load(Tmp1, ?P_OFF_HEAP_MSO),
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(?PROC_BIN_NEXT), Tmp1),
   hipe_rtl_arch:pcb_store(?P_OFF_HEAP_MSO, HP)].

get_base(Base, ByteSize) ->
  {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(), 
  Header = hipe_rtl:mk_new_reg_gcsafe(),
  Tmp1 = hipe_rtl:mk_new_reg_gcsafe(),
  Tmp2 = hipe_rtl:mk_new_reg_gcsafe(),
  EvenWordSize = hipe_rtl:mk_new_reg_gcsafe(),
  WordSize = hipe_rtl_arch:word_size(),
  [GetHPInsn,
   hipe_rtl:mk_alu(Tmp1, ByteSize, add, hipe_rtl:mk_imm(WordSize-1)),
   hipe_rtl:mk_alu(EvenWordSize, Tmp1, sra, hipe_rtl:mk_imm(hipe_rtl_arch:log2_word_size())),
   hipe_rtl:mk_alu(Tmp2, EvenWordSize, add, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(Base, HP, add, hipe_rtl:mk_imm(2*WordSize)),
   mk_var_header(Header, Tmp2, ?TAG_HEADER_HEAP_BIN),
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(0), Header),
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(WordSize), ByteSize),
   PutHPInsn].

safe_mk_sub_binary(Dst, Size, Offs, Orig) ->
  [hipe_rtl:mk_gctest(?SUB_BIN_WORDSIZE)|unsafe_mk_sub_binary(Dst, Size, Offs, Orig)].
unsafe_mk_sub_binary(Dst, Size, Offs, Orig) -> 
  {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  WordSize = hipe_rtl_arch:word_size(),
  Head = hipe_rtl:mk_imm(mk_header(2, ?TAG_HEADER_SUB_BIN)),
  [GetHPInsn,
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(?SUB_BIN_THING_WORD), Head),
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(?SUB_BIN_BINSIZE), Size),
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(?SUB_BIN_OFFS), Offs),
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(?SUB_BIN_ORIG), Orig),
   tag_boxed(Dst, HP),
   hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(?SUB_BIN_WORDSIZE*WordSize)),
   PutHPInsn].

%% safe_mk_float(Dst, FloatLo, FloatHi) ->
%%   [hipe_rtl:mk_gctest(3)|unsafe_mk_float(Dst, FloatLo, FloatHi)].

%% This mk_float takes two 32 bit regs and writes them starting at the
%% first word boundary. It is not really 64 bit safe, but in 64 bit
%% mode this will not be called, so it should be protected with an
%% ifdef

unsafe_mk_float(Dst, FloatLo, FloatHi) ->
  {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  WordSize = hipe_rtl_arch:word_size(),
  Head = hipe_rtl:mk_imm(flonum_header()),
  [GetHPInsn,
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(0*WordSize), Head),
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(1*WordSize), FloatLo),
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(1*WordSize+4), FloatHi),
   tag_boxed(Dst, HP),
   hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(WordSize+8)),
   PutHPInsn].

unsafe_load_float(DstLo, DstHi, Src) ->
  WordSize = hipe_rtl_arch:word_size(),
  Offset1 = -(?TAG_PRIMARY_BOXED) + WordSize,
  Offset2 = Offset1 + 4, %% This should really be 4 and not WordSize
  case hipe_rtl_arch:endianess() of
    little ->
      [hipe_rtl:mk_load(DstLo, Src, hipe_rtl:mk_imm(Offset1),int32,unsigned),
       hipe_rtl:mk_load(DstHi, Src, hipe_rtl:mk_imm(Offset2),int32,unsigned)];
    big ->
      [hipe_rtl:mk_load(DstHi, Src, hipe_rtl:mk_imm(Offset1),int32,unsigned),
       hipe_rtl:mk_load(DstLo, Src, hipe_rtl:mk_imm(Offset2),int32,unsigned)]
  end. 

unsafe_mk_big(Dst, Src, Signedness) ->
  case hipe_rtl_arch:endianess() of
    big ->
      unsafe_mk_big_bigendian(Dst, Src, Signedness);
    little ->
      unsafe_mk_big_littleendian(Dst, Src, Signedness)
  end.

unsafe_mk_big_bigendian(Dst, Src, Signedness) ->
  WordSize = hipe_rtl_arch:word_size(),
  {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  PosHead = hipe_rtl:mk_imm(mk_header(1, ?TAG_HEADER_POS_BIG)),
  NegHead = hipe_rtl:mk_imm(mk_header(1, ?TAG_HEADER_NEG_BIG)),
  PosLabel = hipe_rtl:mk_new_label(),
  NegLabel = hipe_rtl:mk_new_label(),
  JoinLabel = hipe_rtl:mk_new_label(),
  Tmp1 = hipe_rtl:mk_new_reg_gcsafe(),
  [GetHPInsn | case Signedness of
		 unsigned ->
		   [hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(0*WordSize), PosHead),
		    hipe_rtl:mk_alu(Tmp1, Src, sll, hipe_rtl:mk_imm(16)), % XXX: 64-bit unsafe
		    hipe_rtl:mk_alu(Src, Src, srl, hipe_rtl:mk_imm(16)), % XXX: 64-bit unsafe
		    hipe_rtl:mk_alu(Src, Src, 'or', Tmp1),
		    hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(WordSize), Src),
		    tag_boxed(Dst, HP),
		    hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(2*WordSize)),
		    PutHPInsn];
		 signed ->
		   [hipe_rtl:mk_branch(Src, ge, hipe_rtl:mk_imm(0), 
				       hipe_rtl:label_name(PosLabel), 
				       hipe_rtl:label_name(NegLabel)),
		    PosLabel,
		    hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(0*WordSize), PosHead),
		    hipe_rtl:mk_goto(hipe_rtl:label_name(JoinLabel)),
		    NegLabel,
		    hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(0*WordSize), NegHead),
		    JoinLabel,
		    hipe_rtl:mk_alu(Tmp1, Src, sll, hipe_rtl:mk_imm(16)), % XXX: 64-bit unsafe
		    hipe_rtl:mk_alu(Src, Src, srl, hipe_rtl:mk_imm(16)), % XXX: 64-bit unsafe
		    hipe_rtl:mk_alu(Src, Src, 'or', Tmp1),
		    hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(1*WordSize), Src),
		    tag_boxed(Dst, HP),
		    hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(2*WordSize)),
		    PutHPInsn]
	       end].

unsafe_mk_big_littleendian(Dst, Src, Signedness) ->
  WordSize = hipe_rtl_arch:word_size(),
  {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  PosHead = hipe_rtl:mk_imm(mk_header(1, ?TAG_HEADER_POS_BIG)),
  NegHead = hipe_rtl:mk_imm(mk_header(1, ?TAG_HEADER_NEG_BIG)),
  PosLabel = hipe_rtl:mk_new_label(),
  NegLabel = hipe_rtl:mk_new_label(),
  JoinLabel = hipe_rtl:mk_new_label(),
  [GetHPInsn | case Signedness of
		 unsigned ->
		   [hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(0*WordSize), PosHead),
		    hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(1*WordSize), Src),
		    tag_boxed(Dst, HP),
		    hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(2*WordSize)),
		    PutHPInsn];
		 signed ->
		   [hipe_rtl:mk_branch(Src, ge, hipe_rtl:mk_imm(0), 
				       hipe_rtl:label_name(PosLabel), 
				       hipe_rtl:label_name(NegLabel)),
		    PosLabel,
		    hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(0*WordSize), PosHead),
		    hipe_rtl:mk_goto(hipe_rtl:label_name(JoinLabel)),
		    NegLabel,
		    hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(0*WordSize), NegHead),
		    JoinLabel,
		    hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(1*WordSize), Src),
		    tag_boxed(Dst, HP),
		    hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(2*WordSize)),
		    PutHPInsn]
	       end].

test_subbinary(Binary, TrueLblName, FalseLblName) ->
  Tmp1 = hipe_rtl:mk_new_reg_gcsafe(),
  Tmp2 = hipe_rtl:mk_new_reg_gcsafe(),
  [get_header(Tmp1, Binary),
   hipe_rtl:mk_alu(Tmp2, Tmp1, 'and', hipe_rtl:mk_imm(?TAG_HEADER_MASK)),
   hipe_rtl:mk_branch(Tmp2, eq, hipe_rtl:mk_imm(?TAG_HEADER_SUB_BIN), TrueLblName, FalseLblName)].

test_heap_binary(Binary, TrueLblName, FalseLblName) ->
  Tmp1 = hipe_rtl:mk_new_reg_gcsafe(),
  Tmp2 = hipe_rtl:mk_new_reg_gcsafe(),
  [get_header(Tmp1, Binary),
   hipe_rtl:mk_alu(Tmp2, Tmp1, 'and', hipe_rtl:mk_imm(?TAG_HEADER_MASK)),
   hipe_rtl:mk_branch(Tmp2, eq, hipe_rtl:mk_imm(?TAG_HEADER_HEAP_BIN), TrueLblName, FalseLblName)].

mk_var_header(Header, Size, Tag) ->
  Tmp = hipe_rtl:mk_new_reg_gcsafe(),
  [hipe_rtl:mk_alu(Tmp, Size, sll, hipe_rtl:mk_imm(?HEADER_ARITY_OFFS)),
   hipe_rtl:mk_alu(Header, Tmp, 'add', hipe_rtl:mk_imm(Tag))].

get_erts_mb(MatchBuf) ->
  case ?P_SCHED_DATA of
    [] ->
      [hipe_rtl:mk_load_address(MatchBuf, erts_mb, c_const)];
    Offset ->
      Tmp = hipe_rtl:mk_new_reg(), % points to per-CPU state
      [hipe_rtl_arch:pcb_load(Tmp, Offset),
       hipe_rtl:mk_alu(MatchBuf, Tmp, 'add', hipe_rtl:mk_imm(?SCHED_DATA_ERTS_MB_OFFS))]
  end.

extract_offset(Offset, MatchState) ->
  Offs  = ?MS_MATCHBUFFER + ?MB_OFFSET - ?TAG_PRIMARY_BOXED,
  hipe_rtl:mk_load(Offset, MatchState, hipe_rtl:mk_imm(Offs)).

extract_orig(Orig, MatchState) ->
  Offs  = ?MS_MATCHBUFFER + ?MB_ORIG - ?TAG_PRIMARY_BOXED,
  hipe_rtl:mk_load(Orig, MatchState, hipe_rtl:mk_imm(Offs)).

extract_base(Base, MatchState) ->
  Offs  = ?MS_MATCHBUFFER + ?MB_BASE - ?TAG_PRIMARY_BOXED,
  hipe_rtl:mk_load(Base, MatchState, hipe_rtl:mk_imm(Offs)).

extract_binsize(BinSize, MatchState) ->
  Offs  = ?MS_MATCHBUFFER + ?MB_SIZE - ?TAG_PRIMARY_BOXED,
  hipe_rtl:mk_load(BinSize, MatchState, hipe_rtl:mk_imm(Offs)).

update_offset(Offset, MatchState) ->
  Offs  = ?MS_MATCHBUFFER + ?MB_OFFSET - ?TAG_PRIMARY_BOXED,
  hipe_rtl:mk_store(MatchState, hipe_rtl:mk_imm(Offs), Offset).

extract_slot(Dst, Slot, MatchState) ->
  WordSize = hipe_rtl_arch:word_size(),
  Offs  = ?MS_SAVEOFFSET + Slot*WordSize - ?TAG_PRIMARY_BOXED,
  hipe_rtl:mk_load(Dst, MatchState, hipe_rtl:mk_imm(Offs)).

extract_matchbuffer(Dst, MatchState) -> 
  Offs  = ?MS_MATCHBUFFER - ?TAG_PRIMARY_BOXED,
  hipe_rtl:mk_alu(Dst, MatchState, add, hipe_rtl:mk_imm(Offs)).

update_slot(Slot, Src, MatchState) ->
  WordSize = hipe_rtl_arch:word_size(),
  Offs  = ?MS_SAVEOFFSET + Slot*WordSize - ?TAG_PRIMARY_BOXED,
  hipe_rtl:mk_store(MatchState, hipe_rtl:mk_imm(Offs), Src).

create_matchstate(Max, BinSize, Base, Offset, Orig, Ms) -> 
  WordSize = hipe_rtl_arch:word_size(),
  {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  ByteSize = Max*WordSize + ?MS_SAVEOFFSET,
  SizeInWords = ((ByteSize div WordSize) -1),
  Header = hipe_rtl:mk_imm(mk_header(SizeInWords, 
				     ?TAG_HEADER_BIN_MATCHSTATE)),
  HeaderOffs = hipe_rtl:mk_imm(?MS_THING_WORD - ?TAG_PRIMARY_BOXED),
  OrigOffs =  hipe_rtl:mk_imm(?MS_MATCHBUFFER + ?MB_ORIG - 
			      ?TAG_PRIMARY_BOXED),
  BaseOffs =   hipe_rtl:mk_imm(?MS_MATCHBUFFER + ?MB_BASE - 
			       ?TAG_PRIMARY_BOXED),
  BinSizeOffs =  hipe_rtl:mk_imm(?MS_MATCHBUFFER + ?MB_SIZE - 
				 ?TAG_PRIMARY_BOXED),
  OffsetOffs = hipe_rtl:mk_imm(?MS_MATCHBUFFER + ?MB_OFFSET - 
			       ?TAG_PRIMARY_BOXED),
  [GetHPInsn,
   hipe_rtl:mk_alu(Ms, HP, add, hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED)),
   hipe_rtl:mk_store(Ms, HeaderOffs, Header),
   hipe_rtl:mk_store(Ms, OrigOffs, Orig),
   hipe_rtl:mk_store(Ms, BaseOffs, Base),
   hipe_rtl:mk_store(Ms, BinSizeOffs, BinSize),
   hipe_rtl:mk_store(Ms, OffsetOffs, Offset),
   hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(ByteSize)),
   PutHPInsn].
