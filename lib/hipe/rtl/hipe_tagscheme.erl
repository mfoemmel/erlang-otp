%%% $Id$
%%%
%%% hipe_tagscheme.erl
%%%
%%% XXX: This is specific for Erlang 5.0 / R9.
%%% 
%%%  020904: Happi - added support for external pids and ports.
%%%     

-module(hipe_tagscheme).

-export([mk_nil/0, mk_fixnum/1, mk_arityval/1, mk_atom/1, mk_non_value/0]).
-export([is_fixnum/1]).
-export([tag_bignum/2, tag_flonum/2, tag_tuple/2, tag_cons/2]).
-export([write_catch_frame/3]).
-export([save_CP/4, restore_CP/4]).
-export([test_is_boxed/4, get_header/2]).
-export([test_nil/4, test_cons/4, test_flonum/4, test_fixnum/4,
	 test_tuple/4, test_atom/4, test_bignum/4, 
	 test_any_pid/4,test_any_port/4,
	 test_internal_pid/4,
	 test_internal_port/4, test_ref/4, test_fun/4, test_binary/4, test_list/4,
	 test_integer/4, test_number/4, test_constant/4, test_tuple_N/5]).
-export([untag_fixnum/2]).
-export([test_two_fixnums/3,
	 fixnum_gt/5, fixnum_lt/5, fixnum_ge/5, fixnum_le/5,
	 fixnum_addsub/5, fixnum_mul/4,
	 fixnum_andorxor/4, fixnum_not/2]).
-export([unsafe_car/2, unsafe_cdr/2,
	 unsafe_constant_element/3, unsafe_update_element/3, element/6]).
-export([unsafe_closure_element/3]).
-export([mk_fun_header/0, tag_fun/2, untag_fun/2,
	 if_fun_get_arity_and_address/5]).
-export([unsafe_untag_float/2, unsafe_tag_float/3]).
-export([unsafe_mk_sub_binary/4, unsafe_mk_float/3, unsafe_mk_big/4, unsafe_load_float/3]).
-export([test_subbinary/3, test_heap_binary/3]).
-export([finalize_bin/4, mk_var_header/3, get_base/2]).
-include("hipe_icode2rtl.hrl").
-include("hipe_literals.hrl").
-undef(TAG_PRIMARY_BOXED).
-undef(TAG_IMMED2_MASK).
-undef(TAG_IMMED2_CATCH).
-undef(TAG_IMMED2_SIZE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
-define(TAG_HEADER_VECTOR,  ((16#1 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(TAG_HEADER_POS_BIG, ((16#2 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(TAG_HEADER_NEG_BIG, ((16#3 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(BIG_SIGN_BIT, (16#1 bsl ?TAG_PRIMARY_SIZE)).
-define(TAG_HEADER_REF,     ((16#4 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(TAG_HEADER_FUN,     ((16#5 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(TAG_HEADER_FLOAT,   ((16#6 bsl ?TAG_PRIMARY_SIZE) bor ?TAG_PRIMARY_HEADER)).
-define(BINARY_XXX_MASK, (16#3 bsl ?TAG_PRIMARY_SIZE)).
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

mk_fixnum(X)	-> (X bsl ?TAG_IMMED1_SIZE) + ?TAG_IMMED1_SMALL.

-define(NIL, ((-1 bsl ?TAG_IMMED2_SIZE) bor ?TAG_IMMED2_NIL)).
mk_nil()	-> ?NIL.
mk_atom(X)	-> (X bsl ?TAG_IMMED2_SIZE) + ?TAG_IMMED2_ATOM.
mk_non_value()	-> ?THE_NON_VALUE.

-define(SMALL_BITS, 28).
-define(MAX_SMALL, ((1 bsl (?SMALL_BITS - 1)) - 1)).
-define(MIN_SMALL, (-(1 bsl (?SMALL_BITS - 1)))).

is_fixnum(N) when N =< ?MAX_SMALL, N >= ?MIN_SMALL -> true;
is_fixnum(_) -> false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(HEADER_FUN, mk_header(?ERL_FUN_SIZE-2,?TAG_HEADER_FUN)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

tag_boxed(Res, X) ->
    hipe_rtl:mk_alu(Res, X, 'add', hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED)).

tag_bignum(Res, X) -> tag_boxed(Res, X).
tag_flonum(Res, X) -> tag_boxed(Res, X).
tag_tuple(Res, X) -> tag_boxed(Res, X).

tag_cons(Res, X) ->
    hipe_rtl:mk_alu(Res, X, 'add', hipe_rtl:mk_imm(?TAG_PRIMARY_LIST)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

write_catch_frame(SP, Off,  CatchLab) ->
    %% SP[Off] = make_catch(CatchLab)
    %% loader should transform the label to a catch table index,
    %% tag it, and emit a 'load constant' insn
    CatchPC = hipe_rtl:mk_new_reg(),
    [hipe_rtl:mk_load_address(CatchPC, CatchLab, 'catch'),
     hipe_rtl:mk_store(SP, hipe_rtl:mk_imm(Off), CatchPC)].

%%% no longer needed
%tag_catch(Ix) -> (Ix bsl ?TAG_IMMED2_SIZE) bor ?TAG_IMMED2_CATCH.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% This is safe for SPARC and other RISCs, which always create 32-bit
%%% aligned return addresses.
%%% For the x86, we assume that either CALL insns are aligned to ensure
%%% 32-bit aligned return addresses, or that stack/register maps are
%%% generated to inform the gc which words contain return addresses.
%%%
%%% XXX: this is trivial now -- inline at call sites?

save_CP(CP, SP, Off, Code) ->
    [hipe_rtl:mk_store(SP, Off, CP) |
     Code].

restore_CP(CP, SP, Off, Code) ->
    [hipe_rtl:mk_load(CP, SP, Off) |
     Code].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Operations to test if an object has a known type T.

test_nil(X, TrueLab, FalseLab, Pred) ->
    hipe_rtl:mk_branch(X, eq, hipe_rtl:mk_imm(?NIL), TrueLab, FalseLab, Pred).

test_cons(X, TrueLab, FalseLab, Pred) ->
    Tmp = hipe_rtl:mk_new_reg(),
    Mask = hipe_rtl:mk_imm(?TAG_PRIMARY_MASK - ?TAG_PRIMARY_LIST),
    hipe_rtl:mk_alub(Tmp, X, 'and', Mask, 'eq', TrueLab, FalseLab, Pred).

test_is_boxed(X, TrueLab, FalseLab, Pred) ->
    Tmp = hipe_rtl:mk_new_reg(),
    Mask = hipe_rtl:mk_imm(?TAG_PRIMARY_MASK - ?TAG_PRIMARY_BOXED),
    hipe_rtl:mk_alub(Tmp, X, 'and', Mask, 'eq', TrueLab, FalseLab, Pred).

get_header(Res, X) ->
    hipe_rtl:mk_load(Res, X, hipe_rtl:mk_imm(-(?TAG_PRIMARY_BOXED))).

mask_and_compare(X, Mask, Value, TrueLab, FalseLab, Pred) ->
    Tmp = hipe_rtl:mk_new_reg(),
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
  Tmp = hipe_rtl:mk_new_reg(),
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
  Tmp = hipe_rtl:mk_new_reg(),
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
    Tmp = hipe_rtl:mk_new_reg(),
    Tmp2 = hipe_rtl:mk_new_reg(),
    HalfTrueLab = hipe_rtl:mk_new_label(),
    [test_is_boxed(X, hipe_rtl:label_name(HalfTrueLab), FalseLab, Pred),
     HalfTrueLab,
     get_header(Tmp, X),
     hipe_rtl:mk_alub(Tmp2, Tmp, 'and', hipe_rtl:mk_imm(?TAG_HEADER_MASK), 'eq',
		 TrueLab, FalseLab, Pred)].

test_tuple_N(X, N, TrueLab, FalseLab, Pred) ->
    Tmp = hipe_rtl:mk_new_reg(),
    HalfTrueLab = hipe_rtl:mk_new_label(),
    [test_is_boxed(X, hipe_rtl:label_name(HalfTrueLab), FalseLab, Pred),
     HalfTrueLab,
     get_header(Tmp, X),
     hipe_rtl:mk_branch(Tmp, 'eq', hipe_rtl:mk_imm(mk_arityval(N)),
		   TrueLab, FalseLab, Pred)].

test_ref(X, TrueLab, FalseLab, Pred) ->
    Tmp = hipe_rtl:mk_new_reg(),
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
    Tmp = hipe_rtl:mk_new_reg(),
    HalfTrueLab = hipe_rtl:mk_new_label(),
    [test_is_boxed(X, hipe_rtl:label_name(HalfTrueLab), FalseLab, Pred),
     HalfTrueLab,
     get_header(Tmp, X),
     mask_and_compare(Tmp, ?TAG_HEADER_MASK, ?TAG_HEADER_FUN,
		      TrueLab, FalseLab, Pred)].

test_flonum(X, TrueLab, FalseLab, Pred) ->
    HeaderFlonum = mk_header(2, ?TAG_HEADER_FLOAT),
    Tmp = hipe_rtl:mk_new_reg(),
    HalfTrueLab = hipe_rtl:mk_new_label(),
    [test_is_boxed(X, hipe_rtl:label_name(HalfTrueLab), FalseLab, Pred),
     HalfTrueLab,
     get_header(Tmp, X),
     hipe_rtl:mk_branch(Tmp, 'eq', hipe_rtl:mk_imm(HeaderFlonum),
		   TrueLab, FalseLab, Pred)].

test_bignum(X, TrueLab, FalseLab, Pred) ->
    Tmp = hipe_rtl:mk_new_reg(),
    HalfTrueLab = hipe_rtl:mk_new_label(),
    BigMask = ?TAG_HEADER_MASK - ?BIG_SIGN_BIT,
    [test_is_boxed(X, hipe_rtl:label_name(HalfTrueLab), FalseLab, Pred),
     HalfTrueLab,
     get_header(Tmp, X),
     mask_and_compare(Tmp, BigMask, ?TAG_HEADER_POS_BIG,
		      TrueLab, FalseLab, Pred)].

test_binary(X, TrueLab, FalseLab, Pred) ->
    Tmp = hipe_rtl:mk_new_reg(),
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
    Tmp = hipe_rtl:mk_new_reg(),
    BigMask = ?TAG_HEADER_MASK - ?BIG_SIGN_BIT,
    HeaderFlonum = mk_header(2, ?TAG_HEADER_FLOAT),
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

%%% CONS, NIL, and TUPLE are not constants, everything else is
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

untag_fixnum(DestReg, SrcVar) ->
  hipe_rtl:mk_alu(DestReg, SrcVar, 'sra', hipe_rtl:mk_imm(?TAG_IMMED1_SIZE)).

test_two_fixnums(Arg1, Arg2, FalseLab) ->
    Tmp = hipe_rtl:mk_new_reg(),
    TrueLab = hipe_rtl:mk_new_label(),
    [hipe_rtl:mk_alu(Tmp, Arg1, 'and', Arg2),
     test_fixnum(Tmp, hipe_rtl:label_name(TrueLab), FalseLab, 0.99),
     TrueLab].

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

%%% (16X+tag)+((16Y+tag)-tag) = 16X+tag+16Y = 16(X+Y)+tag
%%% (16X+tag)-((16Y+tag)-tag) = 16X+tag-16Y = 16(X-Y)+tag
fixnum_addsub(AluOp, Arg1, Arg2, Res, OtherLab) ->
    Tmp = hipe_rtl:mk_new_reg(),
    %% XXX: Consider moving this test to the users of fixnum_addsub.
    case Arg1 =/= Res andalso Arg2 =/= Res of 
        true -> 
	    %% Args differ from res.
	    NoOverflowLab = hipe_rtl:mk_new_label(),
	   [hipe_rtl:mk_alu(Tmp, Arg2, sub, 
			    hipe_rtl:mk_imm(?TAG_IMMED1_SMALL)),
	    hipe_rtl:mk_alub(Res, Arg1, AluOp, Tmp, overflow, 
			     hipe_rtl:label_name(OtherLab),
			     hipe_rtl:label_name(NoOverflowLab), 0.01),
	    NoOverflowLab];
        false ->
            %% At least one of the arguments is the same as Res.
            Tmp2 = hipe_rtl:mk_new_var(),
            NoOverflowLab = hipe_rtl:mk_new_label(),
            [hipe_rtl:mk_alu(Tmp, Arg2, sub, 
			     hipe_rtl:mk_imm(?TAG_IMMED1_SMALL)),
	     hipe_rtl:mk_alub(Tmp2, Arg1, AluOp, Tmp, overflow, 
			      hipe_rtl:label_name(OtherLab),
			      hipe_rtl:label_name(NoOverflowLab), 0.01),
	     NoOverflowLab,
	     hipe_rtl:mk_move(Res, Tmp2)]
    end.

%%% ((16X+tag) div 16) * ((16Y+tag)-tag) + tag = X*16Y+tag = 16(XY)+tag
fixnum_mul(Arg1, Arg2, Res, OtherLab) ->
    Tmp = hipe_rtl:mk_new_reg(),
    U1 = hipe_rtl:mk_new_reg(),
    U2 = hipe_rtl:mk_new_reg(),
    NoOverflowLab = hipe_rtl:mk_new_label(),
    [hipe_rtl:mk_alu(U1, Arg1, 'sra', hipe_rtl:mk_imm(?TAG_IMMED1_SIZE)),
     hipe_rtl:mk_alu(U2, Arg2, 'sub', hipe_rtl:mk_imm(?TAG_IMMED1_SMALL)),
     hipe_rtl:mk_alub(Tmp, U1, 'mul', U2, overflow, hipe_rtl:label_name(OtherLab),
		 hipe_rtl:label_name(NoOverflowLab), 0.01),
     NoOverflowLab,
     hipe_rtl:mk_alu(Res, Tmp, 'add', hipe_rtl:mk_imm(?TAG_IMMED1_SMALL))].

fixnum_andorxor(AluOp, Arg1, Arg2, Res) ->
    case AluOp of
	'xor' ->
	    Tmp = hipe_rtl:mk_new_reg(),
	    [hipe_rtl:mk_alu(Tmp, Arg1, 'xor', Arg2),	% clears tag :-(
	     hipe_rtl:mk_alu(Res, Tmp, 'or', hipe_rtl:mk_imm(?TAG_IMMED1_SMALL))];
	_ -> hipe_rtl:mk_alu(Res, Arg1, AluOp, Arg2)
    end.

fixnum_not(Arg, Res) ->
    Mask = (-1 bsl ?TAG_IMMED1_SIZE),
    hipe_rtl:mk_alu(Res, Arg, 'xor', hipe_rtl:mk_imm(Mask)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unsafe_car(Dst, Arg) ->
    hipe_rtl:mk_load(Dst, Arg, hipe_rtl:mk_imm(-(?TAG_PRIMARY_LIST))).

unsafe_cdr(Dst, Arg) ->
    hipe_rtl:mk_load(Dst, Arg, hipe_rtl:mk_imm(-(?TAG_PRIMARY_LIST)+4)).

unsafe_constant_element(Dst, Index, Tuple) ->	% Index is an immediate
    Offset = -(?TAG_PRIMARY_BOXED) + 4 * hipe_rtl:imm_value(Index),
    hipe_rtl:mk_load(Dst, Tuple, hipe_rtl:mk_imm(Offset)).

unsafe_update_element(Tuple, Index, Value) ->   % Index is an immediate
    Offset = -(?TAG_PRIMARY_BOXED) + 4 * hipe_rtl:imm_value(Index),
    hipe_rtl:mk_store(Tuple, hipe_rtl:mk_imm(Offset), Value).

%%% wrong semantics
% unsafe_variable_element(Dst, Index, Tuple) ->	% Index is an unknown fixnum
%     %% Load word at (Tuple - 2) + ((Index >> 4) << 2).
%     %% Offset = ((Index >> 4) << 2) - 2.
%     %% Index = x..x1111 (fixnum tag is 2#1111).
%     %% (Index >> 2) = 00x..x11 and ((Index >> 4) << 2) = 00x..x00.
%     %% Therefore, ((Index >> 4) << 2) = (Index >> 2) - 3.
%     %% So Offset = ((Index >> 4) << 2) - 2 = (Index >> 2) - (3 + 2).
%     Tmp1 = hipe_rtl:mk_new_reg(),
%     Tmp2 = hipe_rtl:mk_new_reg(),
%     Shift = ?TAG_IMMED1_SIZE - 2,
%     OffAdj = (?TAG_IMMED1_SMALL bsr Shift) + ?TAG_PRIMARY_BOXED,
%     [hipe_rtl:mk_alu(Tmp1, Index, 'srl', hipe_rtl:mk_imm(Shift)),
%      hipe_rtl:mk_alu(Tmp2, Tmp1, 'sub', hipe_rtl:mk_imm(OffAdj)),
%      hipe_rtl:mk_load(Dst, Tuple, Tmp2)].

element(Dst, Index, Tuple, FailLab, {tuple, A}, IndexInfo) ->
  FailLabName = hipe_rtl:label_name(FailLab),
  FixnumOkLab = hipe_rtl:mk_new_label(),
  IndexOkLab = hipe_rtl:mk_new_label(),
  Ptr = hipe_rtl:mk_new_reg(),
  UIndex = hipe_rtl:mk_new_reg(),
  Arity = hipe_rtl:mk_imm(A),
  InvIndex = hipe_rtl:mk_new_reg(),
  Offset = hipe_rtl:mk_new_reg(),

  case IndexInfo of
    valid ->
      %% This is no branch, 1 load and 3 alus = 4 instr
      [hipe_rtl:mk_alu(UIndex, Index, 'sra',hipe_rtl:mk_imm(?TAG_IMMED1_SIZE)),
       hipe_rtl:mk_alu(Ptr, Tuple, 'sub', hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED)),
       hipe_rtl:mk_alu(Offset, UIndex, 'sll', hipe_rtl:mk_imm(2)),
       hipe_rtl:mk_load(Dst, Ptr, Offset)];
    fixnums ->
      %% This is 1 branch, 1 load and 4 alus = 6 instr
      [hipe_rtl:mk_alu(UIndex, Index,'sra',hipe_rtl:mk_imm(?TAG_IMMED1_SIZE)),
       hipe_rtl:mk_alu(Ptr, Tuple, 'sub',hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED))|
       gen_element_tail(Dst, Ptr, InvIndex, Arity, Offset, UIndex, 
			FailLabName, IndexOkLab)];
    _ ->
      %% This is 3 branches, 1 load and 5 alus = 9 instr
      [test_fixnum(Index, hipe_rtl:label_name(FixnumOkLab),
		   FailLabName, 0.99),
       FixnumOkLab,
       hipe_rtl:mk_alu(UIndex, Index,'sra',hipe_rtl:mk_imm(?TAG_IMMED1_SIZE)),
       hipe_rtl:mk_alu(Ptr, Tuple, 'sub',hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED))|
       gen_element_tail(Dst, Ptr, InvIndex, Arity, Offset, UIndex, 
			FailLabName, IndexOkLab)]
  end;
element(Dst, Index, Tuple, FailLab, tuple, IndexInfo) ->
  FailLabName = hipe_rtl:label_name(FailLab),
  FixnumOkLab = hipe_rtl:mk_new_label(),
  IndexOkLab = hipe_rtl:mk_new_label(),
  Ptr = hipe_rtl:mk_new_reg(),
  Header = hipe_rtl:mk_new_reg(),
  UIndex = hipe_rtl:mk_new_reg(),
  Arity = hipe_rtl:mk_new_reg(),
  InvIndex = hipe_rtl:mk_new_reg(),
  Offset = hipe_rtl:mk_new_reg(),

  case IndexInfo of
    fixnums ->
      %% This is 1 branch, 2 loads and 5 alus = 8 instr
      [hipe_rtl:mk_alu(Ptr, Tuple, 'sub', hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED)),
       hipe_rtl:mk_load(Header, Ptr, hipe_rtl:mk_imm(0)),
       hipe_rtl:mk_alu(UIndex, Index, 'sra',hipe_rtl:mk_imm(?TAG_IMMED1_SIZE)),
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
       hipe_rtl:mk_alu(UIndex, Index, 'sra',hipe_rtl:mk_imm(?TAG_IMMED1_SIZE)),
       hipe_rtl:mk_alu(Arity,Header,'srl',hipe_rtl:mk_imm(?HEADER_ARITY_OFFS))|
       gen_element_tail(Dst, Ptr, InvIndex, Arity, Offset, UIndex, 
			FailLabName, IndexOkLab)]
  end;
element(Dst, Index, Tuple, FailLab, _TupleInfo, IndexInfo) ->
  FailLabName = hipe_rtl:label_name(FailLab),
  FixnumOkLab = hipe_rtl:mk_new_label(),
  BoxedOkLab = hipe_rtl:mk_new_label(),
  TupleOkLab = hipe_rtl:mk_new_label(),
  IndexOkLab = hipe_rtl:mk_new_label(),
  Ptr = hipe_rtl:mk_new_reg(),
  Header = hipe_rtl:mk_new_reg(),
  Tmp = hipe_rtl:mk_new_reg(),
  UIndex = hipe_rtl:mk_new_reg(),
  Arity = hipe_rtl:mk_new_reg(),
  InvIndex = hipe_rtl:mk_new_reg(),
  
  Offset = hipe_rtl:mk_new_reg(),

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
       hipe_rtl:mk_alu(UIndex, Index, 'sra',hipe_rtl:mk_imm(?TAG_IMMED1_SIZE)),
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
       hipe_rtl:mk_alu(UIndex, Index, 'sra',hipe_rtl:mk_imm(?TAG_IMMED1_SIZE)),
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
   hipe_rtl:mk_alu(Offset, UIndex, 'sll', hipe_rtl:mk_imm(2)),
   hipe_rtl:mk_load(Dst, Ptr, Offset)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
unsafe_closure_element(Dst, Index, Closure) ->	% Index is an immediate
    Offset = -(?TAG_PRIMARY_BOXED)    %% Untag
    + ?EFT_ENV                        %% Field offset
    + (4 * (hipe_rtl:imm_value(Index)-1)), %% Index from 1 to N hence -1)
    hipe_rtl:mk_load(Dst, Closure, hipe_rtl:mk_imm(Offset)).

mk_fun_header() ->
  hipe_rtl:mk_imm(?HEADER_FUN).

tag_fun(Res, X) ->
    tag_boxed(Res, X).

untag_fun(Res, X) ->
    hipe_rtl:mk_alu(Res, X, 'sub', hipe_rtl:mk_imm(?TAG_PRIMARY_BOXED)).

if_fun_get_arity_and_address(ArityReg, AddressReg, FunP, BadFunLab, Pred) ->
%    EmuAddressPtrReg = hipe_rtl:mk_new_reg(),
%    FEPtrReg = hipe_rtl:mk_new_reg(),
%    ArityReg = hipe_rtl:mk_new_reg(),
%    NumFreeReg = hipe_rtl:mk_new_reg(),
%    RealArityReg = hipe_rtl:mk_new_reg(),
    TrueLab0 = hipe_rtl:mk_new_label(),
%    TrueLab1 = hipe_rtl:mk_new_label(),

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Floating point stuff.
%

unsafe_untag_float(Dst, Src) ->
    %% The tag is 2. Use 2 as offset and we don't have to untag.
    [hipe_rtl:mk_fload(Dst, Src, hipe_rtl:mk_imm(2))].

unsafe_tag_float(Dst, Src, Options) ->
  {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  Head = hipe_rtl:mk_imm(mk_header(2, ?TAG_HEADER_FLOAT)),
  
  Code = [GetHPInsn,
	  hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(0), Head),
	  hipe_rtl:mk_fstore(HP, hipe_rtl:mk_imm(4), Src),
	  tag_flonum(Dst, HP),
	  hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(12)),
	  PutHPInsn],
  case ?AddGC(Options) of
    true -> [hipe_rtl:mk_gctest(3)|Code];
    false -> Code
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Binary stuff
%
finalize_bin(Dst, Base, Offset, TrueLblName) ->
  {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  TmpOffset = hipe_rtl:mk_new_reg(),
  Tmp1 = hipe_rtl:mk_new_reg(),
  Tmp2 = hipe_rtl:mk_new_reg(),
  TmpOver = hipe_rtl:mk_new_reg(),
  NewOver = hipe_rtl:mk_new_reg(),
  OldOver = hipe_rtl:mk_new_reg(),
  HeapLbl = hipe_rtl:mk_new_label(),
  REFCLbl = hipe_rtl:mk_new_label(),
  ProcBinHeader = hipe_rtl:mk_imm(mk_header(?PROC_BIN_BYTESIZE-1, ?TAG_HEADER_REFC_BIN)),
  
  [GetHPInsn,
   tag_boxed(Dst, HP),
   hipe_rtl:mk_alu(TmpOffset, Offset, sra, hipe_rtl:mk_imm(3)),
   hipe_rtl:mk_branch(TmpOffset, le, hipe_rtl:mk_imm(?MAX_HEAP_BIN_SIZE), 
		      hipe_rtl:label_name(HeapLbl), hipe_rtl:label_name(REFCLbl)),
   HeapLbl,
   hipe_rtl:mk_alu(Tmp1, HP, add, TmpOffset),
   hipe_rtl:mk_alu(Tmp2, Tmp1, add, hipe_rtl:mk_imm(11)),
   hipe_rtl:mk_alu(HP, Tmp2, 'and', hipe_rtl:mk_imm(16#fffffffc)),
   PutHPInsn,
   hipe_rtl:mk_goto(TrueLblName),
   REFCLbl,
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(?PROC_BIN_THING_WORD), ProcBinHeader),
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(?PROC_BIN_BINSIZE), TmpOffset),
   hipe_rtl:mk_alu(TmpOver, TmpOffset, srl, hipe_rtl:mk_imm(round(math:log(?OVERHEAD_FACTOR)/math:log(2)))),
   hipe_rtl_arch:pcb_load(OldOver, ?P_OFF_HEAP_OVERHEAD),
   hipe_rtl:mk_alu(NewOver, OldOver, 'add', TmpOver),
   hipe_rtl_arch:pcb_store(?P_OFF_HEAP_OVERHEAD, NewOver),
   hipe_rtl_arch:pcb_load(Tmp1, ?P_OFF_HEAP_MSO),
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(?PROC_BIN_NEXT), Tmp1),
   hipe_rtl_arch:pcb_store(?P_OFF_HEAP_MSO, HP),
   hipe_rtl:mk_alu(Tmp2, Base, sub, hipe_rtl:mk_imm(?BINARY_ORIG_BYTES)),
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(?PROC_BIN_VAL), Tmp2),
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(?PROC_BIN_BYTES), Base),
   hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(?PROC_BIN_BYTESIZE*4)),
   PutHPInsn,
   hipe_rtl:mk_goto(TrueLblName)].

get_base(Base, ByteSize) ->
  {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(), 
  Header = hipe_rtl:mk_new_reg(),
  Tmp1 = hipe_rtl:mk_new_reg(),
  Tmp2 = hipe_rtl:mk_new_reg(),
  EvenWordSize = hipe_rtl:mk_new_reg(),
  [GetHPInsn,
   hipe_rtl:mk_alu(Tmp1, ByteSize, add, hipe_rtl:mk_imm(3)),
   hipe_rtl:mk_alu(EvenWordSize, Tmp1, sra, hipe_rtl:mk_imm(2)),
   hipe_rtl:mk_alu(Tmp2, EvenWordSize, add, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(Base, HP, add, hipe_rtl:mk_imm(8)),
   mk_var_header(Header, Tmp2, ?TAG_HEADER_HEAP_BIN),
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(0), Header),
   hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(4), ByteSize),
   PutHPInsn].

unsafe_mk_sub_binary(Dst, Size, Offs, Orig) -> 
    {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
    Head = hipe_rtl:mk_imm(mk_header(2, ?TAG_HEADER_SUB_BIN)),
    [GetHPInsn,
     hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(0), Head),
     hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(4), Size),
     hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(8), Offs),
     hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(12), Orig),
     tag_boxed(Dst, HP),
     hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(16)),
     PutHPInsn].

unsafe_mk_float(Dst, FloatLo, FloatHi) ->
    {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
    Head = hipe_rtl:mk_imm(mk_header(2, ?TAG_HEADER_FLOAT)),
    [GetHPInsn,
     hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(0), Head),
     hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(4), FloatLo),
     hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(8), FloatHi),
     tag_boxed(Dst, HP),
     hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(12)),
     PutHPInsn].

unsafe_load_float(Dst1, Dst2, Src) ->
  case get(hipe_target_arch) of
    x86 ->
      [hipe_rtl:mk_load(Dst1, Src, hipe_rtl:mk_imm(2)),
       hipe_rtl:mk_load(Dst2, Src, hipe_rtl:mk_imm(6))];
    ultrasparc ->
      [hipe_rtl:mk_load(Dst2, Src, hipe_rtl:mk_imm(2)),
       hipe_rtl:mk_load(Dst1, Src, hipe_rtl:mk_imm(6))]
  end.



unsafe_mk_big(Dst, Src, Signedness, ultrasparc) ->
  {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  PosHead = hipe_rtl:mk_imm(mk_header(1, ?TAG_HEADER_POS_BIG)),
  NegHead = hipe_rtl:mk_imm(mk_header(1, ?TAG_HEADER_NEG_BIG)),
  PosLabel = hipe_rtl:mk_new_label(),
  NegLabel = hipe_rtl:mk_new_label(),
  JoinLabel = hipe_rtl:mk_new_label(),
  Tmp1 = hipe_rtl:mk_new_reg(),
  [GetHPInsn | case Signedness of
    unsigned ->
      [hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(0), PosHead),
       hipe_rtl:mk_alu(Tmp1, Src, sll, hipe_rtl:mk_imm(16)),
       hipe_rtl:mk_alu(Src, Src, srl, hipe_rtl:mk_imm(16)),
       hipe_rtl:mk_alu(Src, Src, 'or', Tmp1),
       hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(4), Src),
       tag_boxed(Dst, HP),
       hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(8)),
       PutHPInsn];
    signed ->
      [hipe_rtl:mk_alub(Tmp1, Src, 'and', hipe_rtl:mk_imm(1 bsl 31), eq, hipe_rtl:label_name(PosLabel), hipe_rtl:label_name(NegLabel)),
       PosLabel,
       hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(0), PosHead),
       hipe_rtl:mk_goto(hipe_rtl:label_name(JoinLabel)),
       NegLabel,
       hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(0), NegHead),
       JoinLabel,
       hipe_rtl:mk_alu(Tmp1, Src, sll, hipe_rtl:mk_imm(16)),
       hipe_rtl:mk_alu(Src, Src, srl, hipe_rtl:mk_imm(16)),
       hipe_rtl:mk_alu(Src, Src, 'or', Tmp1),
       hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(4), Src),
       tag_boxed(Dst, HP),
       hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(8)),
       PutHPInsn]
  end];
unsafe_mk_big(Dst, Src, Signedness, x86) ->
  {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  PosHead = hipe_rtl:mk_imm(mk_header(1, ?TAG_HEADER_POS_BIG)),
  NegHead = hipe_rtl:mk_imm(mk_header(1, ?TAG_HEADER_NEG_BIG)),
  PosLabel = hipe_rtl:mk_new_label(),
  NegLabel = hipe_rtl:mk_new_label(),
  JoinLabel = hipe_rtl:mk_new_label(),
  Tmp1 = hipe_rtl:mk_new_reg(),
  [GetHPInsn | case Signedness of
    unsigned ->
      [hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(0), PosHead),
       hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(4), Src),
       tag_boxed(Dst, HP),
       hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(8)),
       PutHPInsn];
    signed ->
      [hipe_rtl:mk_alub(Tmp1, Src, 'and', hipe_rtl:mk_imm(1 bsl 31), eq, hipe_rtl:label_name(PosLabel), hipe_rtl:label_name(NegLabel)),
       PosLabel,
       hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(0), PosHead),
       hipe_rtl:mk_goto(hipe_rtl:label_name(JoinLabel)),
       NegLabel,
       hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(0), NegHead),
       JoinLabel,
       hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(4), Src),
       tag_boxed(Dst, HP),
       hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(8)),
       PutHPInsn]
  end].

test_subbinary(Binary, TrueLblName, FalseLblName) ->
  Tmp1 = hipe_rtl:mk_new_reg(),
  Tmp2 = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_load(Tmp1, Binary, hipe_rtl:mk_imm(-2)),
   hipe_rtl:mk_alu(Tmp2, Tmp1, 'and', hipe_rtl:mk_imm(?TAG_HEADER_MASK)),
   hipe_rtl:mk_branch(Tmp2, eq, hipe_rtl:mk_imm(?TAG_HEADER_SUB_BIN), TrueLblName, FalseLblName)].

test_heap_binary(Binary, TrueLblName, FalseLblName) ->
  Tmp1 = hipe_rtl:mk_new_reg(),
  Tmp2 = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_load(Tmp1, Binary, hipe_rtl:mk_imm(-2)),
   hipe_rtl:mk_alu(Tmp2, Tmp1, 'and', hipe_rtl:mk_imm(?TAG_HEADER_MASK)),
   hipe_rtl:mk_branch(Tmp2, eq, hipe_rtl:mk_imm(?TAG_HEADER_HEAP_BIN), TrueLblName, FalseLblName)].

mk_var_header(Header, Size, Tag) ->
  Tmp = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_alu(Tmp, Size, sll, hipe_rtl:mk_imm(?HEADER_ARITY_OFFS)),
   hipe_rtl:mk_alu(Header, Tmp, 'add', hipe_rtl:mk_imm(Tag))].


   
   
   
