%%%-------------------------------------------------------------------
%%% File    : hipe_rtl_ibo_2.erl
%%% Author  : Per Gustafsson <pergu@dhcp-12-245.it.uu.se>
%%% Description : 
%%%
%%% Created :  7 Sep 2005 by Per Gustafsson <pergu@dhcp-12-245.it.uu.se>
%%%-------------------------------------------------------------------
-module(hipe_rtl_binary).

-export([gen_rtl/5]).

-include("hipe_literals.hrl").

-define(MAX_BINSIZE, trunc(?MAX_HEAP_BIN_SIZE / hipe_rtl_arch:word_size()) + 2).
-define(BYTE_SHIFT, 3). %% Turn bits into bytes or vice versa
-define(FIVE_HIGH_BITS, 31 bsl 27).
-define(LOW_BITS, 7). %% Three lowest bits set
-define(BYTE_SIZE, 8).
-define(MAX_SMALL_BITS, 27).

gen_rtl({bs_start_match_2, Max}, [Ms], [Binary],  
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
gen_rtl({bs_start_match_2, _Max}, [], [Binary],  
	TrueLblName, FalseLblName) ->
  [hipe_tagscheme:test_binary(Binary, TrueLblName, FalseLblName, 0.99)];
gen_rtl({bs_get_integer_2,0,_Flags}, [Dst,NewMs], [Ms],  
	TrueLblName, _FalseLblName) ->
  update_ms(NewMs, Ms) ++
    [hipe_rtl:mk_move(Dst, hipe_rtl:mk_imm(15)),
     hipe_rtl:mk_goto(TrueLblName)];
gen_rtl({bs_get_integer_2,Size,Flags}, [Dst,NewMs], Args,  
	TrueLblName, FalseLblName) ->
  Signed = signed(Flags),
  LittleEndian = littleendian(Flags),
  Aligned = aligned(Flags),
  case Args of
    [Ms] ->
      CCode= int_get_c_code(Dst, Ms, hipe_rtl:mk_imm(Size), 
			    Flags, TrueLblName, FalseLblName),
      update_ms(NewMs, Ms) ++
	get_static_int(Dst, Ms, Size, CCode,
		       Signed, LittleEndian, Aligned, 
		       TrueLblName, FalseLblName);
    [Ms,Arg] ->
      {SizeCode, SizeReg} = 
	hipe_rtl_bin_util:make_size(Size, Arg, FalseLblName),
      CCode = int_get_c_code(Dst, Ms, SizeReg, Flags, 
			     TrueLblName, FalseLblName),
      InCode=get_dynamic_int(Dst, Ms, SizeReg, CCode, 
			     Signed, LittleEndian, Aligned, 
			     TrueLblName, FalseLblName),
      update_ms(NewMs, Ms) ++ SizeCode ++ InCode
  end;
gen_rtl({bs_get_float_2,Size,Flags}, [Dst1,NewMs], Args, 
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
	{SizeCode, SizeReg} = hipe_rtl_bin_util:make_size(Size, Arg, 
							  FalseLblName),
	CCode = float_get_c_code(Dst1, Ms, SizeReg, Flags, 
				 TrueLblName, FalseLblName),
	update_ms(NewMs, Ms) ++ SizeCode ++
	  get_dynamic_float(Dst1, Ms, SizeReg, CCode, 
			    LittleEndian, Aligned, 
			    TrueLblName, FalseLblName)
    end;
gen_rtl({bs_get_binary_all_2, Flags}, [Dst,NewMs], [Ms], 
	TrueLblName, FalseLblName) ->
  Aligned = aligned(Flags),
  [hipe_rtl:mk_gctest(?SUB_BIN_WORDSIZE)] ++
    update_ms(NewMs, Ms) ++
    get_binary_all(Dst, Ms, Aligned, TrueLblName, FalseLblName);
gen_rtl({bs_get_binary_2,Size,Flags}, [Dst,NewMs], Args, 
	TrueLblName, FalseLblName) ->
  Aligned = aligned(Flags),
  case Args of
    [Ms] ->
      SizeReg = hipe_rtl:mk_new_reg(),
      SizeCode = [hipe_rtl:mk_move(SizeReg, hipe_rtl:mk_imm(Size))];
    [Ms, BitsVar]  ->
      {SizeCode, SizeReg} = 
	hipe_rtl_bin_util:make_size(Size, BitsVar, FalseLblName)
  end,
  CCode = binary_get_c_code(Dst, Ms, SizeReg, Flags, TrueLblName, FalseLblName),
  InCode = get_binary(Dst, Ms, SizeReg, CCode, Aligned, 
		      TrueLblName, FalseLblName),
  [hipe_rtl:mk_gctest(?MAX_BINSIZE)] ++ 
    update_ms(NewMs, Ms) ++ SizeCode ++ InCode;
gen_rtl({bs_test_tail_2, NumBits}, [NewMs], [Ms], TrueLblName, FalseLblName) ->
  {[Offset,BinSize], ExCode} = extract_matchstate_vars([offset,binsize], Ms),
    update_ms(NewMs, Ms) ++ ExCode ++
    [hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(NumBits)),
     hipe_rtl:mk_branch(Offset, eq, BinSize, TrueLblName, FalseLblName)];
gen_rtl({bs_test_tail_2, NumBits}, [], [Ms], TrueLblName, FalseLblName) ->
  {[Offset,BinSize], ExCode} = extract_matchstate_vars([offset,binsize], Ms),
    ExCode ++
    [hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(NumBits)),
     hipe_rtl:mk_branch(Offset, eq, BinSize, TrueLblName, FalseLblName)];
gen_rtl({bs_skip_bits_all_2, Flags}, Dst, [Ms], 
	TrueLblName, FalseLblName) ->
  Aligned = aligned(Flags),
  opt_update_ms(Dst, Ms) ++
    skip_bits_all(Ms, Aligned,TrueLblName, FalseLblName);
gen_rtl({bs_skip_bits_2, Bits}, Dst, [Ms|Args], TrueLblName, FalseLblName) ->
  opt_update_ms(Dst,Ms) ++
  case Args of
    [] ->
      skip_bits2(Ms, hipe_rtl:mk_imm(Bits), TrueLblName, FalseLblName);
    [Arg] ->
      {SizeCode, SizeReg} = 
	hipe_rtl_bin_util:make_size(Bits, Arg, FalseLblName),
      InCode = skip_bits2(Ms, SizeReg, TrueLblName, FalseLblName),
      SizeCode ++ InCode
  end;
gen_rtl({bs_restore_2, Slot}, [NewMs], [Ms], TrueLblName, _FalseLblName) ->
  Tmp1=hipe_rtl:mk_new_reg_gcsafe(),
  update_ms(NewMs, Ms) ++
    [hipe_tagscheme:extract_slot(Tmp1, Slot, Ms),
     hipe_tagscheme:update_offset(Tmp1, Ms),
     hipe_rtl:mk_goto(TrueLblName)];
gen_rtl({bs_save_2, Slot}, [NewMs], [Ms], TrueLblName, _FalseLblName) ->
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

binary_get_c_code(Dst1, Ms, Size, Flags, TrueLblName, FalseLblName) ->
  get_c_code(bs_get_binary_2, Dst1, Ms, Size, Flags, TrueLblName, FalseLblName).

get_c_code(Func, Dst1, Ms, Size, Flags, TrueLblName, FalseLblName) ->  
  SizeReg = hipe_rtl:mk_new_reg_gcsafe(),
  FlagsReg = hipe_rtl:mk_new_reg_gcsafe(),
  MatchBuf = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_move(SizeReg, Size),
   hipe_rtl:mk_move(FlagsReg, hipe_rtl:mk_imm(Flags)),
   hipe_tagscheme:extract_matchbuffer(MatchBuf, Ms),
   hipe_rtl_arch:call_bif([Dst1], Func, [SizeReg, FlagsReg, MatchBuf], 
			  TrueLblName, FalseLblName)].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Int Code %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
    hipe_rtl_bin_util:get_int(Dst1, Size, Base, Offset, 
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
    hipe_rtl_bin_util:get_unaligned_int(Dst1, Size, Base, Offset, 
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
      hipe_rtl_bin_util:get_little_unknown_int(Dst1, Base, Offset, NewOffset, 
					      Shiftr, Type, TrueLblName);
    false ->
      hipe_rtl_bin_util:get_big_unknown_int(Dst1, Base, Offset, NewOffset, 
					      Shiftr, Type, TrueLblName)
  end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Binary Code %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


get_binary_all(Dst1, Ms, Aligned, TrueLblName, FalseLblName) ->
  [Tmp1, BitSize, ByteSize, ByteOffset] = 
    create_gcsafe_regs(4),
  [OkLbl] = create_lbls(1),
  {[Offset,BinSize,Orig], ExCode} = 
    extract_matchstate_vars([offset,binsize,orig], Ms),
  CheckCode =
    case Aligned of
      true ->
	[];
      false ->
	[hipe_rtl:mk_alub(Tmp1, Offset, 'and', hipe_rtl:mk_imm(?LOW_BITS), eq, 
			  hipe_rtl:label_name(OkLbl), FalseLblName, 0.99),
	 OkLbl]
    end,
    MakeCode =
    [hipe_rtl:mk_alu(BitSize, BinSize, sub, Offset),
     hipe_rtl:mk_alu(ByteSize, BitSize, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
     hipe_rtl:mk_alu(ByteOffset, Offset, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
     hipe_tagscheme:update_offset(BinSize, Ms),
     hipe_tagscheme:unsafe_mk_sub_binary(Dst1, ByteSize, ByteOffset, Orig),
     hipe_rtl:mk_goto(TrueLblName)],
  ExCode ++ CheckCode ++ MakeCode.

get_binary(Dst1, Ms, SizeReg, CCode, 
	   Aligned, TrueLblName, FalseLblName) ->
  [SuccessLbl0,SuccessLbl1,SuccessLbl2,CLbl] = create_lbls(4),
  [Tmp, EndOffset, ByteOffset, ByteSize] = create_gcsafe_regs(4),
  {[Offset,BinSize,Orig], ExCode} = 
    extract_matchstate_vars([offset,binsize,orig], Ms),
  CheckCode =
    [hipe_rtl:mk_alub(Tmp, SizeReg, 'and', hipe_rtl:mk_imm(?LOW_BITS), eq,
		      hipe_rtl:label_name(SuccessLbl1), FalseLblName, 0.99),
     SuccessLbl1,
     check_size(Offset, SizeReg, BinSize, EndOffset, 
		hipe_rtl:label_name(SuccessLbl2), FalseLblName),
     SuccessLbl2],
  MakeCode =
    [hipe_rtl:mk_alu(ByteOffset, Offset, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
     hipe_rtl:mk_alu(ByteSize, SizeReg, srl, hipe_rtl:mk_imm(?BYTE_SHIFT))]
    ++ hipe_tagscheme:unsafe_mk_sub_binary(Dst1, ByteSize, ByteOffset, Orig)
    ++ [hipe_tagscheme:update_offset(EndOffset, Ms),
	hipe_rtl:mk_goto(TrueLblName)],
  SubCode = CheckCode ++ MakeCode,
  case Aligned of 
    true ->
      ExCode ++ SubCode;
    false ->
      ExCode ++
      [hipe_rtl:mk_alub(Tmp, Offset, 'and', hipe_rtl:mk_imm(?LOW_BITS), eq, 
			hipe_rtl:label_name(SuccessLbl0), 
			hipe_rtl:label_name(CLbl)),
       SuccessLbl0] ++
	SubCode ++
	[CLbl|CCode]
  end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Float Code %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


get_static_float(Dst1, Ms, 64, _CCode, LittleEndian, true, 
		 TrueLblName, FalseLblName) ->
  [LoBytes, HiBytes, ByteOffset, NewOffset] = create_gcsafe_regs(4),
  [SuccessLbl] = create_lbls(1),
  Type = get_type(false, LittleEndian),
  {[Base,Offset,BinSize], ExCode} = 
    extract_matchstate_vars([base,offset,binsize], Ms),
  Code1 = [check_size(Offset, hipe_rtl:mk_imm(64), BinSize, NewOffset,
		      hipe_rtl:label_name(SuccessLbl), FalseLblName),
	   SuccessLbl,
	   hipe_tagscheme:update_offset(NewOffset, Ms),
	   hipe_rtl:mk_alu(ByteOffset, Offset, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
	   hipe_rtl_bin_util:load_bytes(LoBytes, Base, ByteOffset, Type, 4),
	   hipe_rtl_bin_util:load_bytes(HiBytes, Base, ByteOffset, Type, 4)],
  Code2 = case {LittleEndian, hipe_rtl_arch:endianess()}  of 
	    {false, big} ->
	      [hipe_tagscheme:unsafe_mk_float(Dst1, LoBytes, HiBytes),
	       hipe_rtl:mk_goto(TrueLblName)];
	    {true, big} ->
	      [hipe_tagscheme:unsafe_mk_float(Dst1, HiBytes, LoBytes),
	       hipe_rtl:mk_goto(TrueLblName)];
	    {true, little} ->
	      [hipe_tagscheme:unsafe_mk_float(Dst1, LoBytes, HiBytes),
	       hipe_rtl:mk_goto(TrueLblName)];
	    {false, little} ->
	      [hipe_tagscheme:unsafe_mk_float(Dst1, HiBytes, LoBytes),
	       hipe_rtl:mk_goto(TrueLblName)]
	  end,
  ExCode ++ Code1 ++ Code2;
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

skip_bits_all(Ms, Aligned, TrueLblName, FalseLblName) ->
  {[Offset,BinSize], ExCode} = extract_matchstate_vars([offset,binsize], Ms),
  ExCode ++
    case Aligned of
      true ->
	[hipe_tagscheme:update_offset(BinSize,Ms),
	 hipe_rtl:mk_goto(TrueLblName)];
      false ->
	[SuccessLbl] = create_lbls(1),
	[hipe_rtl:mk_alub(Offset, Offset, 'and', hipe_rtl:mk_imm(?LOW_BITS), eq, 
			  hipe_rtl:label_name(SuccessLbl), FalseLblName, 0.99),
	 SuccessLbl,
	 hipe_tagscheme:update_offset(BinSize,Ms),
	 hipe_rtl:mk_goto(TrueLblName)]
    end.

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
  [OrigOffset] = create_gcsafe_regs(1),
  [SuccessLbl,SubLbl,OtherLbl,JoinLbl] = create_lbls(4),
  [hipe_tagscheme:test_binary(Binary, hipe_rtl:label_name(SuccessLbl), FalseLblName, 0.99),
   SuccessLbl,
   hipe_rtl:mk_load(BinSize, Binary, hipe_rtl:mk_imm(?SUB_BIN_BINSIZE-2)),
   hipe_rtl:mk_alu(BinSize, BinSize, sll, hipe_rtl:mk_imm(?BYTE_SHIFT)),
   hipe_tagscheme:test_subbinary(Binary, hipe_rtl:label_name(SubLbl), hipe_rtl:label_name(OtherLbl)),
   SubLbl,
   hipe_rtl:mk_load(OrigOffset, Binary, hipe_rtl:mk_imm(?SUB_BIN_OFFS-2)),
   hipe_rtl:mk_alu(Offset, OrigOffset, sll, hipe_rtl:mk_imm(?BYTE_SHIFT)),
   hipe_rtl:mk_alu(BinSize, BinSize, add, Offset),
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

%create_regs(0) ->
%  [];
%create_regs(X) when X > 0->
%  [hipe_rtl:mk_new_reg()|create_regs(X-1)].

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
