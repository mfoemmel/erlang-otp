%%%-------------------------------------------------------------------
%%% File    : hipe_rtl_bin_util.erl
%%% Author  : Per Gustafsson <pergu@amanda.it.uu.se>
%%% Description : 
%%%
%%% Created :  4 Mar 2004 by Per Gustafsson <pergu@amanda.it.uu.se>
%%%-------------------------------------------------------------------
-module(hipe_rtl_bin_util).

-export([get_unaligned_int/7, get_int/7, get_big_unknown_int/7,
	 get_little_unknown_int/7, bs_call/9, load_bytes/5, 
	 make_size/3]).
-include("hipe_literals.hrl").
-define(LOW_BITS, 7). %% Three lowest bits set
-define(BYTE_SIZE, 8).
-define(MAX_SMALL_BITS, hipe_rtl_arch:word_size() * ?BYTE_SIZE - 5).
-define(BYTE_SHIFT, 3).


get_unaligned_int(Dst1, Size, Base, Offset, Shiftr, 
		  Type, TrueLblName) ->
  [ByteOffset, ShiftBits, LoadDst, Tmp, LowBits, TotBits] = create_regs(6),
  [MoreLbl, LessLbl, JoinLbl] = create_lbls(3),
  MinLoad = (Size-1) div ?BYTE_SIZE +1,
  MaxLoad = MinLoad + 1,
  WordSize = hipe_rtl_arch:word_size(),
  Code1 = 
    [hipe_rtl:mk_alu(LowBits, Offset, 'and', hipe_rtl:mk_imm(?LOW_BITS)),
     hipe_rtl:mk_alu(TotBits, LowBits, add, hipe_rtl:mk_imm(Size)),
     hipe_rtl:mk_alu(ByteOffset, Offset, srl, 
		      hipe_rtl:mk_imm(?BYTE_SHIFT))],
  Code2 =
  case {Size rem ?BYTE_SIZE, MinLoad} of
    {1, _} ->
      [load_bytes(LoadDst, Base, ByteOffset, Type, MinLoad),
       hipe_rtl:mk_alu(ShiftBits, LowBits, add, hipe_rtl:mk_imm((WordSize-MinLoad)*?BYTE_SIZE)),
       hipe_rtl:mk_goto(hipe_rtl:label_name(JoinLbl))];
    {_, WordSize} ->
      [hipe_rtl:mk_branch(TotBits, le, hipe_rtl:mk_imm(MinLoad*?BYTE_SIZE), 
			  hipe_rtl:label_name(LessLbl), 
			  hipe_rtl:label_name(MoreLbl)),
       LessLbl,
       load_bytes(LoadDst, Base, ByteOffset, Type, MinLoad),
       hipe_rtl:mk_alu(ShiftBits, LowBits, add, hipe_rtl:mk_imm((WordSize-MinLoad)*?BYTE_SIZE)),
       hipe_rtl:mk_goto(hipe_rtl:label_name(JoinLbl)),
       MoreLbl,
       load_bytes(LoadDst, Base, ByteOffset, {unsigned,big}, MinLoad),
       hipe_rtl:mk_alu(LoadDst, LoadDst, sll, LowBits),
       load_bytes(Tmp, Base, ByteOffset, {unsigned,big}, 1),
       hipe_rtl:mk_alu(LowBits, hipe_rtl:mk_imm(?BYTE_SIZE), sub, LowBits),
       hipe_rtl:mk_alu(Tmp, Tmp, srl, LowBits),
       hipe_rtl:mk_alu(LoadDst, LoadDst, 'or', Tmp),
       hipe_rtl:mk_move(ShiftBits, hipe_rtl:mk_imm(0))];
    {_, _} ->
      [hipe_rtl:mk_branch(TotBits, le, hipe_rtl:mk_imm(MinLoad*?BYTE_SIZE), 
			  hipe_rtl:label_name(LessLbl), 
			  hipe_rtl:label_name(MoreLbl)),
       LessLbl,
       load_bytes(LoadDst, Base, ByteOffset, Type, MinLoad),
       hipe_rtl:mk_alu(ShiftBits, LowBits, add, hipe_rtl:mk_imm((WordSize-MinLoad)*?BYTE_SIZE)),
       hipe_rtl:mk_goto(hipe_rtl:label_name(JoinLbl)),
       MoreLbl,
       load_bytes(LoadDst, Base, ByteOffset, Type, MaxLoad),
       hipe_rtl:mk_alu(ShiftBits, LowBits, add, 
		       hipe_rtl:mk_imm((WordSize-MaxLoad)*?BYTE_SIZE))]
  end,
  Code3 = 
    [JoinLbl,
     hipe_rtl:mk_alu(Tmp, LoadDst, sll, ShiftBits),
     hipe_rtl:mk_alu(Tmp, Tmp, Shiftr, 
		     hipe_rtl:mk_imm(WordSize*?BYTE_SIZE-Size))] ++
    do_bignum_code(Size, Type, Tmp, Dst1, TrueLblName),
  Code1 ++ Code2 ++ Code3.

  
get_int(Dst1, Size, Base, Offset, Shiftr, Type,
		 TrueLblName) ->
  [LoadDst, ByteOffset] = create_regs(2),
  Code1 =
    [hipe_rtl:mk_alu(ByteOffset, Offset, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
     load_bytes(LoadDst, Base, ByteOffset, Type, ((Size-1) div ?BYTE_SIZE +1))],
  Code2 =
    case Size rem ?BYTE_SIZE  of
      0 ->
	[];
      _ ->
	[hipe_rtl:mk_alu(LoadDst, LoadDst, Shiftr, 
			 hipe_rtl:mk_imm(?BYTE_SIZE -Size rem ?BYTE_SIZE ))]
    end,
  Code1 ++ Code2 ++ do_bignum_code(Size, Type, LoadDst, Dst1, TrueLblName).
   
get_big_unknown_int(Dst1, Base, Offset, NewOffset, Shiftr, Type, TrueLblName) ->
  [LoadDst, ByteOffset, Limit, Tmp, LowBits] = create_regs(5),
  [ContLbl, BackLbl, LoopLbl, TagLbl, LastLbl, EndLbl] = create_lbls(6),
  [hipe_rtl:mk_move(LoadDst, hipe_rtl:mk_imm(0)),
   hipe_rtl:mk_branch(Offset, ne, NewOffset, hipe_rtl:label_name(ContLbl), 
		      hipe_rtl:label_name(TagLbl), 0.99),
   ContLbl,
   hipe_rtl:mk_alu(Limit, NewOffset, sub, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(Limit, Limit, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
   hipe_rtl:mk_alu(ByteOffset, Offset, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
   load_bytes(LoadDst, Base, ByteOffset, Type, 1),
   BackLbl,
   hipe_rtl:mk_branch(ByteOffset, le, Limit, hipe_rtl:label_name(LoopLbl), 
		      hipe_rtl:label_name(EndLbl)),
   LoopLbl,
   load_bytes(Tmp, Base, ByteOffset, {unsigned, big}, 1),
   hipe_rtl:mk_alu(LoadDst, LoadDst, sll, hipe_rtl:mk_imm(?BYTE_SIZE)),
   hipe_rtl:mk_alu(LoadDst, LoadDst, 'or', Tmp),
   hipe_rtl:mk_goto(hipe_rtl:label_name(BackLbl)),
   EndLbl,
   hipe_rtl:mk_alub(LowBits, NewOffset, 'and', hipe_rtl:mk_imm(?LOW_BITS), eq,
		    hipe_rtl:label_name(TagLbl), hipe_rtl:label_name(LastLbl)),
   LastLbl,
   hipe_rtl:mk_alu(LowBits, hipe_rtl:mk_imm(?BYTE_SIZE), 'sub', LowBits),
   hipe_rtl:mk_alu(LoadDst, LoadDst, Shiftr, LowBits),
   TagLbl] ++
    do_bignum_code(64, Type, LoadDst, Dst1, TrueLblName).
    

get_little_unknown_int(Dst1, Base, Offset, NewOffset, Shiftr, Type, TrueLblName) ->
  [LoadDst, ByteOffset, Limit, ShiftReg, LowBits, Tmp] = create_regs(6),
  [ContLbl, BackLbl, LoopLbl, DoneLbl, TagLbl] = create_lbls(5),
  [hipe_rtl:mk_move(LoadDst, hipe_rtl:mk_imm(0)),
   hipe_rtl:mk_branch(Offset, ne, NewOffset, hipe_rtl:label_name(ContLbl), 
		      hipe_rtl:label_name(TagLbl), 0.99),
   ContLbl,
   hipe_rtl:mk_alu(Tmp, NewOffset, sub, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(ByteOffset, Offset, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
   hipe_rtl:mk_alu(Limit, Tmp, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
   hipe_rtl:mk_move(ShiftReg, hipe_rtl:mk_imm(0)),
   BackLbl,
   hipe_rtl:mk_branch(ByteOffset, lt, Limit, 
		      hipe_rtl:label_name(LoopLbl), 
		      hipe_rtl:label_name(DoneLbl)),
   LoopLbl,
   load_bytes(Tmp, Base, ByteOffset, {unsigned, big}, 1),
   hipe_rtl:mk_alu(Tmp, Tmp, sll, ShiftReg),
   hipe_rtl:mk_alu(ShiftReg, ShiftReg, add, hipe_rtl:mk_imm(?BYTE_SIZE)),
   hipe_rtl:mk_alu(LoadDst, LoadDst, 'or', Tmp),
   hipe_rtl:mk_goto(hipe_rtl:label_name(BackLbl)),
   DoneLbl,
   hipe_rtl:mk_alu(LowBits, NewOffset, 'and', hipe_rtl:mk_imm(?LOW_BITS)),
   hipe_rtl:mk_alu(LowBits, hipe_rtl:mk_imm(?BYTE_SIZE), sub, LowBits),
   hipe_rtl:mk_alu(LowBits, LowBits, 'and', hipe_rtl:mk_imm(?LOW_BITS)),
   load_bytes(Tmp, Base, ByteOffset, Type, 1),
   hipe_rtl:mk_alu(Tmp, Tmp, Shiftr, LowBits),
   hipe_rtl:mk_alu(Tmp, Tmp, sll, ShiftReg),
   hipe_rtl:mk_alu(LoadDst, LoadDst, 'or', Tmp),
   TagLbl] ++
    do_bignum_code(64, Type, LoadDst, Dst1, TrueLblName).

do_bignum_code(Size, {Signedness,_}, Src, Dst1, TrueLblName) when is_integer(Size) ->
  case {Size>?MAX_SMALL_BITS, Signedness} of
    {false, _} ->
      [hipe_tagscheme:tag_fixnum(Dst1, Src),
       hipe_rtl:mk_goto(TrueLblName)];
    {true, signed} ->
      signed_bignum(Dst1, Src, TrueLblName);
    {true, unsigned} ->
      unsigned_bignum(Dst1, Src, TrueLblName)
    end.

signed_bignum(Dst1, Src, TrueLblName) ->
  Tmp1 = hipe_rtl:mk_new_reg(),
  BignumLabel = hipe_rtl:mk_new_label(),
  [hipe_tagscheme:tag_fixnum(Dst1, Src),
   hipe_tagscheme:untag_fixnum(Tmp1, Dst1),
   hipe_rtl:mk_branch(Tmp1, eq, Src, TrueLblName, 
		      hipe_rtl:label_name(BignumLabel)),
   BignumLabel,
   hipe_tagscheme:unsafe_mk_big(Dst1, Src, signed),
   hipe_rtl:mk_goto(TrueLblName)].

unsigned_bignum(Dst1, Src, TrueLblName) ->
  Tmp1 = hipe_rtl:mk_new_reg(),
  BignumLabel = hipe_rtl:mk_new_label(),
  NextLabel =  hipe_rtl:mk_new_label(),
  [hipe_tagscheme:tag_fixnum(Dst1, Src),
   hipe_rtl:mk_alu(Tmp1, Dst1, srl, hipe_rtl:mk_imm(4)),
   hipe_rtl:mk_branch(Tmp1, eq, Src, hipe_rtl:label_name(NextLabel), 
		      hipe_rtl:label_name(BignumLabel)),
   NextLabel,
   hipe_rtl:mk_branch(Dst1, ge, hipe_rtl:mk_imm(0), TrueLblName, 
		      hipe_rtl:label_name(BignumLabel)),
   BignumLabel,
   hipe_tagscheme:unsafe_mk_big(Dst1, Src, unsigned),
   hipe_rtl:mk_goto(TrueLblName)].

bs_call(Name, Args, DstVar, BinSize, Base, Offset, Orig, 
	TrueLblName, FalseLblName) ->
  MatchBuf = hipe_rtl:mk_new_reg(),
  hipe_tagscheme:get_erts_mb(MatchBuf) ++
    save_matchbuffer(MatchBuf, BinSize, Base, Offset, Orig) ++
   [hipe_rtl_arch:call_bif([DstVar], Name, Args,
			  TrueLblName, FalseLblName)].

save_matchbuffer(MatchBuf, BinSize, Base, Offset, Orig) ->
  [hipe_rtl:mk_store(MatchBuf, hipe_rtl:mk_imm(?MB_SIZE), BinSize),
   hipe_rtl:mk_store(MatchBuf, hipe_rtl:mk_imm(?MB_BASE), Base),
   hipe_rtl:mk_store(MatchBuf, hipe_rtl:mk_imm(?MB_OFFSET), Offset),
   hipe_rtl:mk_store(MatchBuf, hipe_rtl:mk_imm(?MB_ORIG), Orig)].


load_bytes(Dst, Base, Offset, {Signedness, _Endianess},1) ->
  [hipe_rtl:mk_load(Dst, Base, Offset, byte, Signedness),
   hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1))];
load_bytes(Dst, Base, Offset, {Signedness, Endianess},2) ->
  case Endianess of
    big ->
      hipe_rtl_arch:load_big_2(Dst, Base, Offset, Signedness);
    little ->
      hipe_rtl_arch:load_little_2(Dst, Base, Offset, Signedness)
  end;
load_bytes(Dst, Base, Offset, {Signedness, Endianess},3) ->
  Tmp1 = hipe_rtl:mk_new_reg(),
  case Endianess of
    big ->
      [hipe_rtl:mk_load(Dst, Base, Offset, byte, Signedness),
       hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1)),
       hipe_rtl:mk_alu(Dst, Dst, sll, hipe_rtl:mk_imm(8)),
       hipe_rtl:mk_load(Tmp1, Base, Offset, byte, unsigned),
       hipe_rtl:mk_alu(Dst, Dst, 'or', Tmp1),
       hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1)),
       hipe_rtl:mk_alu(Dst, Dst, sll, hipe_rtl:mk_imm(8)),
       hipe_rtl:mk_load(Tmp1, Base, Offset, byte, unsigned),
       hipe_rtl:mk_alu(Dst, Dst, 'or', Tmp1),
       hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1))];

    little ->
      [hipe_rtl:mk_load(Dst, Base, Offset, byte, unsigned),
       hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1)),
       hipe_rtl:mk_load(Tmp1, Base, Offset, byte,unsigned),
       hipe_rtl:mk_alu(Tmp1, Tmp1, sll, hipe_rtl:mk_imm(8)),
       hipe_rtl:mk_alu(Dst, Dst, 'or', Tmp1),
       hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1)),
       hipe_rtl:mk_load(Tmp1, Base, Offset, byte,Signedness),
       hipe_rtl:mk_alu(Tmp1, Tmp1, sll, hipe_rtl:mk_imm(16)),
       hipe_rtl:mk_alu(Dst, Dst, 'or', Tmp1),
       hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1))]
  end; 
load_bytes(Dst, Base, Offset, {Signedness, Endianess},4) ->
  case Endianess of
    big ->
      hipe_rtl_arch:load_big_4(Dst, Base, Offset, Signedness);
    little ->
      hipe_rtl_arch:load_little_4(Dst, Base, Offset, Signedness)
  end;

load_bytes(Dst, Base, Offset, {Signedness, Endianess}, X) when X > 1 ->
  [LoopLbl, EndLbl] = create_lbls(2),
  [Tmp1, Limit, TmpOffset] = create_regs(3),
  case Endianess of
    big ->
      [hipe_rtl:mk_alu(Limit, Offset, add, hipe_rtl:mk_imm(X)),
       hipe_rtl:mk_load(Dst, Base, Offset, byte, Signedness),
       hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1)),
       LoopLbl,
       hipe_rtl:mk_load(Tmp1, Base, Offset, byte, unsigned),
       hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1)),
       hipe_rtl:mk_alu(Dst, Dst, sll, hipe_rtl:mk_imm(8)),
       hipe_rtl:mk_alu(Dst, Dst, 'or', Tmp1),
       hipe_rtl:mk_branch(Offset, lt, Limit, hipe_rtl:label_name(LoopLbl),
			  hipe_rtl:label_name(EndLbl)),
       EndLbl];
    little ->
      [hipe_rtl:mk_alu(Limit, Offset, add, hipe_rtl:mk_imm(X)),
       hipe_rtl:mk_alu(TmpOffset, Limit, sub, hipe_rtl:mk_imm(1)),
       hipe_rtl:mk_load(Dst, Base, TmpOffset, byte, Signedness),
       LoopLbl,
       hipe_rtl:mk_alu(TmpOffset, TmpOffset, sub, hipe_rtl:mk_imm(1)),
       hipe_rtl:mk_load(Tmp1, Base, TmpOffset, byte, Signedness),
       hipe_rtl:mk_alu(Dst, Dst, sll, hipe_rtl:mk_imm(8)),
       hipe_rtl:mk_alu(Dst, Dst, 'or', Tmp1),
       hipe_rtl:mk_branch(Offset, lt, TmpOffset, hipe_rtl:label_name(LoopLbl),
			  hipe_rtl:label_name(EndLbl)),
       EndLbl,
       hipe_rtl:mk_move(Offset, Limit)]
  end.

create_regs(X) when X > 0 ->
  [hipe_rtl:mk_new_reg()|create_regs(X-1)];
create_regs(0) ->
  [].

create_lbls(X) when X > 0 ->
  [hipe_rtl:mk_new_label()|create_lbls(X-1)];
create_lbls(0) ->
  [].

first_part(Variable, Register, FalseLblName) ->
  [SuccessLbl1, SuccessLbl2] = create_lbls(2),
  [hipe_tagscheme:test_fixnum(Variable, hipe_rtl:label_name(SuccessLbl1),
			     FalseLblName, 0.99),
  SuccessLbl1,
  hipe_tagscheme:fixnum_ge(Variable, hipe_rtl:mk_imm(15), hipe_rtl:label_name(SuccessLbl2), FalseLblName, 0.99),
  SuccessLbl2,
  hipe_tagscheme:untag_fixnum(Register, Variable)].

make_size(1, BitsVar, FalseLblName) ->
  [DstReg] = create_regs(1),
  {first_part(BitsVar, DstReg, FalseLblName), DstReg};
make_size(?BYTE_SIZE, BitsVar, FalseLblName) ->
  [DstReg] = create_regs(1),
  Code = 
    first_part(BitsVar, DstReg, FalseLblName) ++
    [hipe_rtl:mk_alu(DstReg, DstReg, sll, hipe_rtl:mk_imm(?BYTE_SHIFT))],
  {Code, DstReg};
make_size(UnitImm, BitsVar, FalseLblName) ->
  [DstReg] = create_regs(1),
  UnitList = number2list(UnitImm),
  Code = multiply_code(UnitList, BitsVar, DstReg, FalseLblName),
  {Code, DstReg}.

multiply_code(List=[Head|_Tail], Variable, Result, FalseLblName) ->
  Test = set_high(Head),
  Tmp1 = hipe_rtl:mk_new_reg(),
  SuccessLbl = hipe_rtl:mk_new_label(),
  Register = hipe_rtl:mk_new_reg(),
  Code =[hipe_rtl:mk_move(Result, hipe_rtl:mk_imm(0))|
	 first_part(Variable, Register, FalseLblName)]
	 
	 ++
    [hipe_rtl:mk_alub(Tmp1, Register, 'and', hipe_rtl:mk_imm(Test), 
		      eq, hipe_rtl:label_name(SuccessLbl), 
		      FalseLblName, 0.99),
     SuccessLbl],
  multiply_code(List, Register, Result, FalseLblName, Tmp1, Code).

multiply_code([ShiftSize| Rest], Register, Result, FalseLblName, Tmp1, OldCode) ->
  SuccessLbl = hipe_rtl:mk_new_label(),
  Code = OldCode ++ [hipe_rtl:mk_alu(Tmp1, Register, sll, hipe_rtl:mk_imm(ShiftSize)),
		     hipe_rtl:mk_alub(Result, Tmp1, 'add', Result, not_overflow, hipe_rtl:label_name(SuccessLbl), FalseLblName, 0.99),
		     SuccessLbl],
  multiply_code(Rest, Register, Result, FalseLblName, Tmp1, Code);
multiply_code([], _Register, _Result, _FalseLblName, _Tmp1, Code) ->
  Code.

number2list(X) when is_integer(X), X>=0 ->
  number2list(X, []).

number2list(1, Acc) ->
  lists:reverse([0|Acc]);
number2list(0, Acc) ->
  lists:reverse(Acc);
number2list(X, Acc) ->
  number2list(X-round(math:pow(2,floorlog2(X))), [floorlog2(X)|Acc]).

floorlog2(X) ->
  round(math:log(X)/math:log(2)-0.5). 

set_high(X) ->
  set_high(X, 0).
set_high(0, Y) ->
  Y;
set_high(X, Y) ->
  set_high(X-1, Y+round(math:pow(2,27-X))).
