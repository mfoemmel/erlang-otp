%%%-------------------------------------------------------------------
%%% File    : hipe_rtl_binary_match.erl
%%% Author  : Per Gustafsson <pergu@it.uu.se>
%%% Description : 
%%%
%%% Created :  5 Mar 2007 by Per Gustafsson <pergu@it.uu.se>
%%%-------------------------------------------------------------------
-module(hipe_rtl_binary_match).

-export([gen_rtl/5]).

-include("hipe_literals.hrl").

-define(MAX_BINSIZE, trunc(?MAX_HEAP_BIN_SIZE / hipe_rtl_arch:word_size()) + 2).
-define(BYTE_SHIFT, 3). %% Turn bits into bytes or vice versa
-define(LOW_BITS, 7). %% Three lowest bits set
-define(BYTE_SIZE, 8).
-define(MAX_SMALL_BITS, (hipe_rtl_arch:word_size() * ?BYTE_SIZE - 5)).

gen_rtl({bs_start_match, Max}, [Ms], [Binary],  
	TrueLblName, FalseLblName) ->
  Lbl = hipe_rtl:mk_new_label(),
  BinSize = hipe_rtl:mk_new_reg_gcsafe(),
  Offset = hipe_rtl:mk_new_reg_gcsafe(),
  Base = hipe_rtl:mk_new_reg(),
  Orig = hipe_rtl:mk_new_var(),
  [hipe_rtl:mk_gctest(5+Max),
   get_binary_bytes(Binary, BinSize, Base, Offset, 
		    Orig, hipe_rtl:label_name(Lbl), FalseLblName),
   Lbl,
   hipe_tagscheme:create_matchstate(Max, BinSize, Base, Offset, Orig, Ms),
   hipe_rtl:mk_goto(TrueLblName)];
gen_rtl({bs_start_match, _Max}, [], [Binary],  
	TrueLblName, FalseLblName) ->
  [hipe_tagscheme:test_binary(Binary, TrueLblName, FalseLblName, 0.99)];
gen_rtl({bs_get_integer,0,_Flags}, [Dst,NewMs], [Ms],  
	TrueLblName, _FalseLblName) ->
  update_ms(NewMs, Ms) ++
    [hipe_rtl:mk_move(Dst, hipe_rtl:mk_imm(15)),
     hipe_rtl:mk_goto(TrueLblName)];
gen_rtl({bs_get_integer,Size,Flags}, [Dst,NewMs], Args,  
	TrueLblName, FalseLblName) ->
  Signed = signed(Flags),
  LittleEndian = littleendian(Flags),
  Aligned = aligned(Flags),
  case Args of
    [Ms] ->
      GCCode = make_int_gc_code(Size),
      CCode= int_get_c_code(Dst, Ms, hipe_rtl:mk_imm(Size), 
			    Flags, TrueLblName, FalseLblName),
      update_ms(NewMs, Ms) ++ GCCode ++
	get_static_int(Dst, Ms, Size, CCode,
		       Signed, LittleEndian, Aligned, 
		       TrueLblName, FalseLblName);
    [Ms,Arg] ->
      {SizeCode1, SizeReg1} = 
	make_size(Size, Arg, FalseLblName),
      GCCode = make_int_gc_code(SizeReg1),
      {SizeCode2, SizeReg2} = 
	sparc_safety_code(SizeReg1, Size, Arg, FalseLblName),
      CCode = int_get_c_code(Dst, Ms, SizeReg2, Flags, 
			     TrueLblName, FalseLblName),
      InCode=get_dynamic_int(Dst, Ms, SizeReg2, CCode, 
			     Signed, LittleEndian, Aligned, 
			     TrueLblName, FalseLblName),
      update_ms(NewMs, Ms) ++ SizeCode1 ++ GCCode ++ SizeCode2 ++ InCode
  end;
gen_rtl({bs_get_float,Size,Flags}, [Dst1,NewMs], Args, 
	TrueLblName, FalseLblName) ->
  %% Inlined when float size is 64 and binary is byte-aligned
  LittleEndian = littleendian(Flags),
  Aligned = aligned(Flags),
  [hipe_rtl:mk_gctest(3)] ++
    case Args of
      [Ms] ->
	CCode = float_get_c_code(Dst1, Ms, hipe_rtl:mk_imm(Size), Flags, 
				 TrueLblName, FalseLblName),
	update_ms(NewMs, Ms) ++ 
	  get_static_float(Dst1, Ms, Size, CCode, 
			   LittleEndian, Aligned, 
			   TrueLblName, FalseLblName);
      [Ms,Arg]  ->
	{SizeCode, SizeReg} = make_size(Size, Arg, 
							  FalseLblName),
	CCode = float_get_c_code(Dst1, Ms, SizeReg, Flags, 
				 TrueLblName, FalseLblName),
	update_ms(NewMs, Ms) ++ SizeCode ++
	  get_dynamic_float(Dst1, Ms, SizeReg, CCode, 
			    LittleEndian, Aligned, 
			    TrueLblName, FalseLblName)
    end;
gen_rtl({bs_get_binary_all, Unit, _Flags}, [Dst,NewMs], [Ms], 
	TrueLblName, FalseLblName) ->
  [hipe_rtl:mk_gctest(?SUB_BIN_WORDSIZE)] ++
    update_ms(NewMs, Ms) ++
    get_binary_all(Dst, Unit, Ms, TrueLblName,FalseLblName);
gen_rtl({bs_get_binary,Size,_Flags}, [Dst,NewMs], Args, 
	TrueLblName, FalseLblName) ->
  case Args of
    [Ms] ->
      SizeReg = hipe_rtl:mk_new_reg(),
      SizeCode = [hipe_rtl:mk_move(SizeReg, hipe_rtl:mk_imm(Size))];
    [Ms, BitsVar]  ->
      {SizeCode, SizeReg} = 
	make_size(Size, BitsVar, FalseLblName)
  end,
  InCode = get_binary(Dst, Ms, SizeReg,
		      TrueLblName, FalseLblName),
  [hipe_rtl:mk_gctest(?SUB_BIN_WORDSIZE)] ++ 
    update_ms(NewMs, Ms) ++ SizeCode ++ InCode;
gen_rtl({bs_test_tail, NumBits}, [NewMs], [Ms], TrueLblName, FalseLblName) ->
  {[Offset,BinSize], ExCode} = extract_matchstate_vars([offset,binsize], Ms),
    update_ms(NewMs, Ms) ++ ExCode ++
    [hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(NumBits)),
     hipe_rtl:mk_branch(Offset, eq, BinSize, TrueLblName, FalseLblName)];
gen_rtl({bs_test_tail, NumBits}, [], [Ms], TrueLblName, FalseLblName) ->
  {[Offset,BinSize], ExCode} = extract_matchstate_vars([offset,binsize], Ms),
    ExCode ++
    [hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(NumBits)),
     hipe_rtl:mk_branch(Offset, eq, BinSize, TrueLblName, FalseLblName)];
gen_rtl({bs_skip_bits_all, Unit, _Flags}, Dst, [Ms], 
	TrueLblName, FalseLblName) ->
  opt_update_ms(Dst, Ms) ++
    skip_bits_all(Unit, Ms, TrueLblName, FalseLblName);
gen_rtl({bs_skip_bits, Bits}, Dst, [Ms|Args], TrueLblName, FalseLblName) ->
  opt_update_ms(Dst,Ms) ++
  case Args of
    [] ->
      skip_bits2(Ms, hipe_rtl:mk_imm(Bits), TrueLblName, FalseLblName);
    [Arg] ->
      {SizeCode, SizeReg} = 
	make_size(Bits, Arg, FalseLblName),
      InCode = skip_bits2(Ms, SizeReg, TrueLblName, FalseLblName),
      SizeCode ++ InCode
  end;
gen_rtl({bs_restore, Slot}, [NewMs], [Ms], TrueLblName, _FalseLblName) ->
  Tmp1=hipe_rtl:mk_new_reg_gcsafe(),
  update_ms(NewMs, Ms) ++
    [hipe_tagscheme:extract_slot(Tmp1, Slot, Ms),
     hipe_tagscheme:update_offset(Tmp1, Ms),
     hipe_rtl:mk_goto(TrueLblName)];
gen_rtl({bs_save, Slot}, [NewMs], [Ms], TrueLblName, _FalseLblName) ->
  {Offset, Instr} = extract_matchstate_var(offset, Ms),
  update_ms(NewMs, Ms) ++
    [Instr,
     hipe_tagscheme:update_slot(Slot, Offset, Ms),
     hipe_rtl:mk_goto(TrueLblName)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Calls to C %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

int_get_c_code(Dst1, Ms, Size, Flags, TrueLblName, FalseLblName) ->
  get_c_code(bs_get_integer_2, Dst1, Ms, Size, Flags, TrueLblName, FalseLblName).

float_get_c_code(Dst1, Ms, Size, Flags, TrueLblName, FalseLblName) ->
  get_c_code(bs_get_float_2, Dst1, Ms, Size, Flags, TrueLblName, FalseLblName).

get_c_code(Func, Dst1, Ms, Size, Flags, TrueLblName, FalseLblName) ->  
  SizeReg = hipe_rtl:mk_new_reg_gcsafe(),
  FlagsReg = hipe_rtl:mk_new_reg_gcsafe(),
  MatchBuf = hipe_rtl:mk_new_reg(),
  RetLabel = hipe_rtl:mk_new_label(),
  NonVal = hipe_rtl:mk_imm(hipe_tagscheme:mk_non_value()),
  [hipe_rtl:mk_move(SizeReg, Size),
   hipe_rtl:mk_move(FlagsReg, hipe_rtl:mk_imm(Flags)),
   hipe_tagscheme:extract_matchbuffer(MatchBuf, Ms),
   hipe_rtl_arch:call_bif([Dst1], Func, [SizeReg, FlagsReg, MatchBuf], 
			  hipe_rtl:label_name(RetLabel), FalseLblName), 
   RetLabel,
   hipe_rtl:mk_branch(Dst1, eq, NonVal,
		      FalseLblName, 
		      TrueLblName, 0.01)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Int Code %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_int_gc_code(I) when integer(I) ->
  case hipe_tagscheme:bignum_sizeneed(I) of
    0 -> [];
    X when is_integer(X) -> [hipe_rtl:mk_gctest(X)]
  end;
make_int_gc_code(SReg) ->
  FixNumLbl = hipe_rtl:mk_new_label(),
  FixNumLblName = hipe_rtl:label_name(FixNumLbl),
  {ResReg,Code} = hipe_tagscheme:bignum_sizeneed_code(SReg,FixNumLblName),
  Code ++
    [hipe_rtl:mk_gctest(ResReg),
     hipe_rtl:mk_goto(FixNumLblName),
     FixNumLbl].

sparc_safety_code(SizeReg, Size, Arg, FalseLblName) ->
  case hipe_rtl_arch:safe_handling_of_registers() of
    true ->
      {[],SizeReg};
    false ->
      make_size(Size, Arg, FalseLblName)
  end.

get_static_int(Dst1, Ms, Size, CCode, Signed, LittleEndian, Aligned, 
	       TrueLblName, FalseLblName) ->
  WordSize = hipe_rtl_arch:word_size(),
  case Size =< WordSize*?BYTE_SIZE of
    true ->
      case {Aligned, LittleEndian} of
	{true, false} ->
	  get_int_from_bin(Ms, Size, Dst1,Signed, LittleEndian, 
				     FalseLblName, TrueLblName);
	{true, true} ->
	  case Size rem ?BYTE_SIZE of
	    0 ->
	      get_int_from_bin(Ms, Size, Dst1, Signed, LittleEndian,
					 FalseLblName, TrueLblName);
	    _ ->
	      CCode
	  end;
	{false, false} ->
	  get_int_from_unaligned_bin(Ms, Size, Dst1, Signed, 
				     FalseLblName, TrueLblName);
	{false, true} ->
	  CCode
      end;
    false ->
      CCode
  end.

get_dynamic_int(Dst1, Ms, SizeReg, CCode, Signed, LittleEndian, true, 
		TrueLblName, FalseLblName) ->
  {Init, End} = make_dyn_prep(SizeReg, CCode),
  Init ++
    get_unknown_size_int(SizeReg, Ms, Dst1, Signed, LittleEndian, 
			 FalseLblName, TrueLblName) ++
    End;
get_dynamic_int(_Dst1, _Ms, _SizeReg, CCode, _Signed, _LittleEndian, false, 
		_TrueLblName, _FalseLblName) ->
  CCode.

get_int_from_bin(Ms, Size, Dst1, Signed, LittleEndian,
		 FalseLblName, TrueLblName) ->
  Shiftr = shift_type(Signed),
  Type = get_type(Signed, LittleEndian),
  NewOffset = hipe_rtl:mk_new_reg_gcsafe(),
  [SuccessLbl] = create_lbls(1),
  {[Base,Offset,BinSize], ExCode} = extract_matchstate_vars([base,offset,binsize], Ms),
  ExCode ++
    [check_size(Offset, hipe_rtl:mk_imm(Size), BinSize, NewOffset,
		hipe_rtl:label_name(SuccessLbl), FalseLblName),
     SuccessLbl] ++
    [hipe_tagscheme:update_offset(NewOffset, Ms)] ++
    get_int(Dst1, Size, Base, Offset, 
			      Shiftr, Type, TrueLblName).

get_int_from_unaligned_bin(Ms, Size, Dst1, Signed,
			   FalseLblName, TrueLblName)  ->
  Shiftr = shift_type(Signed),
  Type = get_type(Signed, false), 
  NewOffset = hipe_rtl:mk_new_reg_gcsafe(),
  [SuccessLbl] = create_lbls(1),
  {[Base,Offset,BinSize], ExCode} = extract_matchstate_vars([base,offset,binsize], Ms),
  ExCode ++
  [check_size(Offset, hipe_rtl:mk_imm(Size), BinSize, NewOffset, 
	      hipe_rtl:label_name(SuccessLbl), FalseLblName),
   SuccessLbl] ++
    [hipe_tagscheme:update_offset(NewOffset, Ms)] ++
    get_unaligned_int(Dst1, Size, Base, Offset, 
				       Shiftr, Type, TrueLblName).

get_unknown_size_int(SizeReg, Ms, Dst1, Signed, Little,
		     FalseLblName, TrueLblName) ->
  Shiftr = shift_type(Signed),
  Type = get_type(Signed, false),
  [NewOffset] = create_gcsafe_regs(1),
  [SuccessLbl] = create_lbls(1),
  {[Base,Offset,BinSize], ExCode} = extract_matchstate_vars([base,offset,binsize], Ms),
  ExCode ++
  [check_size(Offset, SizeReg, BinSize, NewOffset, hipe_rtl:label_name(SuccessLbl), 
	      FalseLblName),
   SuccessLbl,
   hipe_tagscheme:update_offset(NewOffset, Ms)] ++
  case Little of
    true ->
      get_little_unknown_int(Dst1, Base, Offset, NewOffset, 
					      Shiftr, Type, TrueLblName);
    false ->
      get_big_unknown_int(Dst1, Base, Offset, NewOffset, 
					      Shiftr, Type, TrueLblName)
  end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Binary Code %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_binary_all(Dst1, 1, Ms, TrueLblName, _FalseLblName) ->
  [SizeReg] = create_gcsafe_regs(1),
  {[Offset,BinSize,Orig], ExCode} = 
    extract_matchstate_vars([offset,binsize,orig], Ms),
  MakeCode =
    [hipe_rtl:mk_alu(SizeReg, BinSize, sub, Offset)|
     construct_subbin(Dst1,SizeReg,Offset,Orig)] ++
    [hipe_tagscheme:update_offset(BinSize, Ms),
     hipe_rtl:mk_goto(TrueLblName)],
  ExCode ++ MakeCode;
get_binary_all(Dst1, Unit, Ms, TrueLblName, FalseLblName) ->
  [SizeReg] = create_gcsafe_regs(1),
  [SuccessLbl] = create_lbls(1),
  SLblName = hipe_rtl:label_name(SuccessLbl),
  {[Offset,BinSize,Orig], ExCode} = 
    extract_matchstate_vars([offset,binsize,orig], Ms),
  MakeCode =
    [hipe_rtl:mk_alu(SizeReg, BinSize, sub, Offset)|
     test_alignment_code(SizeReg,Unit,SLblName,FalseLblName)] ++
    [SuccessLbl|
     construct_subbin(Dst1,SizeReg,Offset,Orig)] ++
    [hipe_tagscheme:update_offset(BinSize, Ms),
     hipe_rtl:mk_goto(TrueLblName)],
  ExCode ++ MakeCode.

get_binary(Dst1, Ms, SizeReg, 
	   TrueLblName, FalseLblName) ->
  [SuccessLbl] = create_lbls(1),
  [EndOffset] = create_gcsafe_regs(1),
  {[Offset,BinSize,Orig], ExCode} = 
    extract_matchstate_vars([offset,binsize,orig], Ms),
  CheckCode =
    [check_size(Offset, SizeReg, BinSize, EndOffset, 
		hipe_rtl:label_name(SuccessLbl), FalseLblName),
     SuccessLbl],
  MakeCode =
    construct_subbin(Dst1,SizeReg,Offset,Orig)
    ++ [hipe_tagscheme:update_offset(EndOffset, Ms),
	hipe_rtl:mk_goto(TrueLblName)],
  ExCode ++ CheckCode ++ MakeCode.

construct_subbin(Dst,Size,Offset,Orig) ->
  [BitOffset, ByteOffset, BitSize, ByteSize] = create_gcsafe_regs(4),
  [hipe_rtl:mk_alu(ByteSize, Size, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
   hipe_rtl:mk_alu(BitSize, Size, 'and', hipe_rtl:mk_imm(?LOW_BITS)),
   hipe_rtl:mk_alu(ByteOffset, Offset, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
   hipe_rtl:mk_alu(BitOffset, Offset, 'and', hipe_rtl:mk_imm(?LOW_BITS)),
   hipe_tagscheme:unsafe_mk_sub_binary(Dst, ByteSize, ByteOffset, 
				      BitSize, BitOffset, Orig)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Float Code %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%get_static_float(Dst1, Ms, 64, _CCode, LittleEndian, true, 
%		 TrueLblName, FalseLblName) ->
%  [LoBytes, HiBytes, ByteOffset, NewOffset] = create_gcsafe_regs(4),
%  [SuccessLbl] = create_lbls(1),
%  Type = get_type(false, LittleEndian),
%  {[Base,Offset,BinSize], ExCode} = 
%    extract_matchstate_vars([base,offset,binsize], Ms),
%  Code1 = [check_size(Offset, hipe_rtl:mk_imm(64), BinSize, NewOffset,
%		      hipe_rtl:label_name(SuccessLbl), FalseLblName),
%	   SuccessLbl,
%	   hipe_tagscheme:update_offset(NewOffset, Ms),
%	   hipe_rtl:mk_alu(ByteOffset, Offset, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
%	   load_bytes(LoBytes, Base, ByteOffset, Type, 4),
%	   load_bytes(HiBytes, Base, ByteOffset, Type, 4)],
  
%  case {LittleEndian, hipe_rtl_arch:endianess()}  of 
%	    {false, big} ->
%	      [hipe_tagscheme:unsafe_mk_float(Dst1, LoBytes, HiBytes),
%	       hipe_rtl:mk_goto(TrueLblName)];
%	    {true, big} ->
%	      [hipe_tagscheme:unsafe_mk_float(Dst1, HiBytes, LoBytes),
%	       hipe_rtl:mk_goto(TrueLblName)];
%	    {true, little} ->
%	      [hipe_tagscheme:unsafe_mk_float(Dst1, LoBytes, HiBytes),
%	       hipe_rtl:mk_goto(TrueLblName)];
%	    {false, little} ->
%	      [hipe_tagscheme:unsafe_mk_float(Dst1, HiBytes, LoBytes),
%	       hipe_rtl:mk_goto(TrueLblName)]
%	  end,
%  ExCode ++ Code1 ++ Code2;
get_static_float(_Dst1, _Ms, _Size, CCode, _LittleEndian, _Aligned, 
		 _TrueLblName, _FalseLblName) ->
  CCode.

get_dynamic_float(Dst1, Ms, SizeReg, CCode, LittleEndian, true, 
		  TrueLblName, FalseLblName) ->
  [CLbl, SuccessLbl] = create_lbls(2),
  [hipe_rtl:mk_branch(SizeReg, eq, hipe_rtl:mk_imm(64), 
		      hipe_rtl:label_name(SuccessLbl), 
		      hipe_rtl:label_name(CLbl)),
   SuccessLbl] ++
    get_static_float(Dst1, Ms, 64, CCode, LittleEndian, true, 
		     TrueLblName, FalseLblName) ++
    [CLbl|CCode];
get_dynamic_float(_Dst1, _Ms, _SizeReg, CCode, _LittleEndian, _Aligned, 
		 _TrueLblName, _FalseLblName) ->
  CCode.

%%%%%%%%%%%%%%%%%%%%%%%%% Skip Bits %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
skip_bits_all(1,Ms, TrueLblName,_FalseLblName) ->
  {[BinSize], ExCode} = extract_matchstate_vars([binsize], Ms),
  ExCode ++
    [hipe_tagscheme:update_offset(BinSize,Ms),
     hipe_rtl:mk_goto(TrueLblName)];
skip_bits_all(Unit,Ms, TrueLblName,FalseLblName) ->
  [Size]  = create_gcsafe_regs(1),
  [SuccessLbl] = create_lbls(1),
  SLblName = hipe_rtl:label_name(SuccessLbl),
  {[Offset,BinSize], ExCode} = extract_matchstate_vars([offset,binsize], Ms),
  ExCode ++
    [hipe_rtl:mk_alu(Size,BinSize,sub,Offset)]
    ++
    test_alignment_code(Size,Unit,SLblName,FalseLblName) ++
    [SuccessLbl,
     hipe_tagscheme:update_offset(BinSize,Ms),
     hipe_rtl:mk_goto(TrueLblName)].

test_alignment_code(Size,Unit,SLblName,FalseLblName) ->
  case Unit of
    2 -> get_fast_test_code(Size,1,SLblName,FalseLblName);
    4 -> get_fast_test_code(Size,3,SLblName,FalseLblName);
    8 -> get_fast_test_code(Size,7,SLblName,FalseLblName);
    16 -> get_fast_test_code(Size,15,SLblName,FalseLblName);
    32 -> get_fast_test_code(Size,31,SLblName,FalseLblName);
    _ -> get_slow_test_code(Size,Unit,SLblName,FalseLblName)
  end.

get_fast_test_code(Size,AndTest,SLblName,FalseLblName) ->
  [Tmp]  = create_gcsafe_regs(1),
  [hipe_rtl:mk_alub(Tmp,Size,'and',hipe_rtl:mk_imm(AndTest),
		    eq,SLblName,FalseLblName)].

%% This is really slow
get_slow_test_code(Size,Unit,SLblName,FalseLblName) ->
  [Tmp]  = create_gcsafe_regs(1),
  [LoopLbl,Lbl1,Lbl2] = create_lbls(3),
  LoopLblName = hipe_rtl:label_name(LoopLbl),
  Lbl1Name = hipe_rtl:label_name(Lbl1),
  Lbl2Name = hipe_rtl:label_name(Lbl2),
  [hipe_rtl:mk_move(Tmp,Size),
   LoopLbl,
   hipe_rtl:mk_branch(Tmp, eq, hipe_rtl:mk_imm(0), SLblName, Lbl1Name),
   Lbl1,
   hipe_rtl:mk_branch(Tmp, lt, hipe_rtl:mk_imm(0), FalseLblName, Lbl2Name),
   Lbl2,
   hipe_rtl:mk_alu(Tmp,Tmp,sub,hipe_rtl:mk_imm(Unit)),
   hipe_rtl:mk_goto(LoopLblName)].

skip_bits2(Ms, NoOfBits, TrueLblName, FalseLblName) ->
  [NewOffset]  = create_gcsafe_regs(1),
  [SuccessLbl] = create_lbls(1),
  {[Offset,BinSize], ExCode} = extract_matchstate_vars([offset,binsize], Ms),
  ExCode ++
  [hipe_rtl:mk_alub(NewOffset, NoOfBits, add, Offset, overflow, 
		    FalseLblName, hipe_rtl:label_name(SuccessLbl),0.01),
   SuccessLbl,
   hipe_tagscheme:update_offset(NewOffset,Ms),
   hipe_rtl:mk_branch(BinSize, lt, NewOffset, FalseLblName, 
		      TrueLblName, 0.01)].


%%%%%%%%%%%%%%%%%%%%%%%%%% Code for start match %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_binary_bytes(Binary, BinSize, Base, Offset, Orig, 
		 TrueLblName, FalseLblName) ->
  [OrigOffset,BitSize,BitOffset] = create_gcsafe_regs(3),
  [SuccessLbl,SubLbl,OtherLbl,JoinLbl] = create_lbls(4),
  [hipe_tagscheme:test_binary(Binary, hipe_rtl:label_name(SuccessLbl), FalseLblName, 0.99),
   SuccessLbl,
   hipe_rtl:mk_load(BinSize, Binary, hipe_rtl:mk_imm(?SUB_BIN_BINSIZE-2)),
   hipe_rtl:mk_alu(BinSize, BinSize, sll, hipe_rtl:mk_imm(?BYTE_SHIFT)),
   hipe_tagscheme:test_subbinary(Binary, hipe_rtl:label_name(SubLbl), hipe_rtl:label_name(OtherLbl)),
   SubLbl,
   hipe_rtl:mk_load(OrigOffset, Binary, hipe_rtl:mk_imm(?SUB_BIN_OFFS-2)),
   hipe_rtl:mk_alu(Offset, OrigOffset, sll, hipe_rtl:mk_imm(?BYTE_SHIFT)),
   hipe_rtl:mk_load(BitOffset, Binary, hipe_rtl:mk_imm(?SUB_BIN_BITOFFS-2)),
   hipe_rtl:mk_alu(Offset, Offset, add, BitOffset),
   hipe_rtl:mk_load(BitSize, Binary, hipe_rtl:mk_imm(?SUB_BIN_BITSIZE-2)),
   hipe_rtl:mk_alu(BinSize, BinSize, add, Offset),
   hipe_rtl:mk_alu(BinSize, BinSize, add, BitSize),
   hipe_rtl:mk_load(Orig, Binary, hipe_rtl:mk_imm(?SUB_BIN_ORIG-2)),
   hipe_rtl:mk_goto(hipe_rtl:label_name(JoinLbl)),
   OtherLbl,
   hipe_rtl:mk_move(Offset, hipe_rtl:mk_imm(0)),
   hipe_rtl:mk_move(Orig, Binary),
   JoinLbl] ++
    get_base(Orig,Base) ++
    [hipe_rtl:mk_goto(TrueLblName)].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% UTILS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


get_base(Orig,Base) ->
  [HeapLbl,REFCLbl,EndLbl] = create_lbls(3),
  [hipe_tagscheme:test_heap_binary(Orig, hipe_rtl:label_name(HeapLbl), hipe_rtl:label_name(REFCLbl)),
   HeapLbl,
   hipe_rtl:mk_alu(Base, Orig, add, hipe_rtl:mk_imm(?HEAP_BIN_DATA-2)),
   hipe_rtl:mk_goto(hipe_rtl:label_name(EndLbl)),
   REFCLbl,
   hipe_rtl:mk_load(Base, Orig, hipe_rtl:mk_imm(?PROC_BIN_BYTES-2)),
   EndLbl].

extract_matchstate_var(binsize, Ms) ->
  BinSize = hipe_rtl:mk_new_reg_gcsafe(),
  {BinSize, hipe_tagscheme:extract_binsize(BinSize, Ms)};
extract_matchstate_var(offset, Ms) ->
  Offset = hipe_rtl:mk_new_reg_gcsafe(),
  {Offset, hipe_tagscheme:extract_offset(Offset, Ms)};
extract_matchstate_var(base, Ms) ->
  Base = hipe_rtl:mk_new_reg(),
  {Base, hipe_tagscheme:extract_base(Base, Ms)};
extract_matchstate_var(orig, Ms) ->
  Orig = hipe_rtl:mk_new_var(),
  {Orig, hipe_tagscheme:extract_orig(Orig, Ms)}.

extract_matchstate_vars(List, Ms) ->
  lists:unzip([extract_matchstate_var(Name, Ms) || Name <- List]).

check_size(Offset, Size, BinSize, Tmp1, ContLblName, FalseLblName) ->
  [hipe_rtl:mk_alu(Tmp1, Offset, add, Size),
   hipe_rtl:mk_branch(Tmp1, leu, BinSize, ContLblName, FalseLblName, 0.99)].

shift_type(true) ->
  sra;
shift_type(false) ->
  srl.

get_type(true, LittleEndian) ->
  {signed, endianess(LittleEndian)};
get_type(false, LittleEndian) ->
  {unsigned, endianess(LittleEndian)}.

endianess(true) ->
  little;
endianess(false) ->
  big.    

aligned(Flags) ->
  case Flags band ?BSF_ALIGNED of
    1 -> true;
    0 -> false
  end.

littleendian(Flags) ->
  case Flags band 2 of
    2 -> true;
    0 -> false
  end.

signed(Flags) ->
  case Flags band 4 of
    4 -> true;
    0 -> false
  end.

opt_update_ms([NewMs], OldMs) ->
  [hipe_rtl:mk_move(NewMs, OldMs)];
opt_update_ms([], _OldMs) ->
  [].

update_ms(NewMs, OldMs) ->
  [hipe_rtl:mk_move(NewMs, OldMs)].

create_lbls(0) ->
  [];
create_lbls(X) when X > 0->
  [hipe_rtl:mk_new_label()|create_lbls(X-1)].

create_gcsafe_regs(0) ->
  [];
create_gcsafe_regs(X) when X > 0->
  [hipe_rtl:mk_new_reg_gcsafe()|create_gcsafe_regs(X-1)].

make_dyn_prep(SizeReg, CCode) ->   
  [CLbl, SuccessLbl] = create_lbls(2),
  Init = [hipe_rtl:mk_branch(SizeReg, le, hipe_rtl:mk_imm(?MAX_SMALL_BITS),  
			     hipe_rtl:label_name(SuccessLbl), 
			     hipe_rtl:label_name(CLbl)),
	  SuccessLbl],
  End = [CLbl|CCode],
  {Init, End}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% From hipe_rtl_binutil.erl
%%------------------------------------------------------------------------

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
   
get_big_unknown_int(Dst1, Base, Offset, NewOffset,
		    Shiftr, Type, TrueLblName) ->
  [LoadDst, ByteOffset, Limit, Tmp, LowBits] = create_regs(5),
  [ContLbl, BackLbl, LoopLbl, TagLbl, LastLbl, EndLbl] = create_lbls(6),
  [hipe_rtl:mk_move(LoadDst, hipe_rtl:mk_imm(0)),
   hipe_rtl:mk_branch(NewOffset, ne, Offset, hipe_rtl:label_name(ContLbl), 
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
    

get_little_unknown_int(Dst1, Base, Offset, NewOffset,
		       Shiftr, Type, TrueLblName) ->
  [LoadDst, ByteOffset, Limit, ShiftReg, LowBits, Tmp] = create_regs(6),
  [ContLbl, BackLbl, LoopLbl, DoneLbl, TagLbl] = create_lbls(5),
  [hipe_rtl:mk_move(LoadDst, hipe_rtl:mk_imm(0)),
   hipe_rtl:mk_branch(NewOffset, ne, Offset, hipe_rtl:label_name(ContLbl), 
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

do_bignum_code(Size, {Signedness,_}, Src, Dst1, TrueLblName)
  when is_integer(Size) ->
  case {Size > ?MAX_SMALL_BITS, Signedness} of
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
  [hipe_tagscheme:realtag_fixnum(Dst1, Src),
   hipe_tagscheme:realuntag_fixnum(Tmp1, Dst1),
   hipe_rtl:mk_branch(Tmp1, eq, Src, TrueLblName, 
		      hipe_rtl:label_name(BignumLabel)),
   BignumLabel,
   hipe_tagscheme:unsafe_mk_big(Dst1, Src, signed),
   hipe_rtl:mk_goto(TrueLblName)].

unsigned_bignum(Dst1, Src, TrueLblName) ->
  Tmp1 = hipe_rtl:mk_new_reg_gcsafe(),
  BignumLbl = hipe_rtl:mk_new_label(),
  BignumLblName = hipe_rtl:label_name(BignumLbl),
  NxtLbl = hipe_rtl:mk_new_label(),
  NxtLblName = hipe_rtl:label_name(NxtLbl),
  [hipe_rtl:mk_branch(Src, lt, hipe_rtl:mk_imm(0), BignumLblName, NxtLblName),
   NxtLbl,
   hipe_tagscheme:realtag_fixnum(Dst1, Src),
   hipe_tagscheme:realuntag_fixnum(Tmp1, Dst1),
   hipe_rtl:mk_branch(Tmp1, eq, Src, TrueLblName, BignumLblName),
   BignumLbl,
   hipe_tagscheme:unsafe_mk_big(Dst1, Src, unsigned),
   hipe_rtl:mk_goto(TrueLblName)].

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

first_part(Var, Register, FalseLblName) ->
  [SuccessLbl1, SuccessLbl2] = create_lbls(2),
  [hipe_tagscheme:test_fixnum(Var, hipe_rtl:label_name(SuccessLbl1),
			     FalseLblName, 0.99),
  SuccessLbl1,
  hipe_tagscheme:fixnum_ge(Var, hipe_rtl:mk_imm(hipe_tagscheme:mk_fixnum(0)), 
			   hipe_rtl:label_name(SuccessLbl2), FalseLblName, 0.99),
  SuccessLbl2,
  hipe_tagscheme:untag_fixnum(Register, Var)].

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
