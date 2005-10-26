%%% -*- erlang-indent-level: 2 -*-
%%% $Id$
%%%-------------------------------------------------------------------
%%% File    : hipe_rtl_cerl_bs_ops.erl
%%% Author  : Per Gustafsson <pergu@dhcp-12-245.it.uu.se>
%%% Description : 
%%%
%%% Created : 26 Jun 2003 by Per Gustafsson <pergu@dhcp-12-245.it.uu.se>
%%%-------------------------------------------------------------------
-module(hipe_rtl_cerl_bs_ops).

-export([gen_rtl/5]).
-include("../main/hipe.hrl").
-include("hipe_literals.hrl").
-define(FIVE_HIGH_BITS, 31 bsl 27).
-define(BYTE_SHIFT, 3).
-define(BYTE_SIZE, 8).
-define(MAX_SMALL_BITS, 27).
-define(BIT_SET(X,Y), (1 bsl X)-(1 bsl Y)).
-define(LOW_BITS, 7).

gen_rtl(BsOP,Args, Dst, TrueLblName, FalseLblName) ->    
  %%io:format("Bs:~w~n", [BsOP]),
  case BsOP of 
    bs_get_orig ->
      [Binary]=Args,
      case Dst of
	[Dst1] ->
	  BinaryLbl = hipe_rtl:mk_new_label(),
	  SubLbl=hipe_rtl:mk_new_label(),
	  OtherLbl=hipe_rtl:mk_new_label(),
	  [hipe_tagscheme:test_binary(Binary, hipe_rtl:label_name(BinaryLbl),
				      FalseLblName, 0.99),
	   BinaryLbl,
	   hipe_tagscheme:test_subbinary(Binary, hipe_rtl:label_name(SubLbl), 
					 hipe_rtl:label_name(OtherLbl)),
	   SubLbl,
	   hipe_rtl:mk_load(Dst1, Binary, hipe_rtl:mk_imm(?SUB_BIN_ORIG-2)),
	   hipe_rtl:mk_goto(TrueLblName),
	   OtherLbl,
	   hipe_rtl:mk_move(Dst1, Binary),
	   hipe_rtl:mk_goto(TrueLblName)];
	_ ->
	  [hipe_tagscheme:test_binary(Binary, TrueLblName, FalseLblName, 0.99)]
      end;
    
    bs_get_orig_offset ->
      case Dst of 
	[Dst1] ->
	  [Binary]=Args,
	  Tmp1 = hipe_rtl:mk_new_reg(),
	  SubLbl=hipe_rtl:mk_new_label(),
	  OtherLbl=hipe_rtl:mk_new_label(),
	  [hipe_tagscheme:test_subbinary(Binary, hipe_rtl:label_name(SubLbl), hipe_rtl:label_name(OtherLbl)),
	   SubLbl,
	   hipe_rtl:mk_load(Tmp1, Binary, hipe_rtl:mk_imm(?SUB_BIN_OFFS-2)),
	   hipe_tagscheme:tag_fixnum(Dst1, Tmp1),
	   hipe_rtl:mk_goto(TrueLblName),
	   OtherLbl,
	   hipe_rtl:mk_move(Dst1, hipe_rtl:mk_imm(hipe_tagscheme:mk_fixnum(0))),
	   hipe_rtl:mk_goto(TrueLblName)];
	_ ->
	  [hipe_rtl:mk_goto(TrueLblName)]
      end;
    
    bs_get_size ->
      [Dst1]=Dst,
      [Binary]=Args,
      Tmp1 = hipe_rtl:mk_new_reg(),
      [hipe_rtl:mk_load(Tmp1, Binary, hipe_rtl:mk_imm(?SUB_BIN_BINSIZE-2)),
       hipe_rtl:mk_alu(Tmp1, Tmp1, sll, hipe_rtl:mk_imm(?BYTE_SHIFT)),
       hipe_tagscheme:tag_fixnum(Dst1, Tmp1),
       hipe_rtl:mk_goto(TrueLblName)]; 
    
    {bs_make_size, Unit} ->
      [Size]=Args,
      case Dst of
	[Dst1] ->
	  {Code, Tmp1} = 
	    hipe_rtl_bin_util:make_size(Unit, Size, FalseLblName),
	  Code ++
	    [hipe_tagscheme:tag_fixnum(Dst1, Tmp1),
	     hipe_rtl:mk_goto(TrueLblName)];
	[] ->
	  hipe_tagscheme:test_fixnum(Size, TrueLblName, FalseLblName, 0.99)
      end;

    bs_add ->
      [Tmp1, Tmp2] = Args,
      case Dst of
	[Dst1] ->
	  hipe_tagscheme:fixnum_addsub(add, Tmp1, Tmp2, Dst1, hipe_rtl:mk_label(FalseLblName)) ++
	    [hipe_rtl:mk_goto(TrueLblName)];
	[] ->
	  [hipe_rtl:mk_goto(TrueLblName)]
      end;
    
    bs_div_test ->
      [PatternSize] = Args,
      Tmp1 = hipe_rtl:mk_new_reg(),
      [hipe_rtl:mk_alu(Tmp1, PatternSize, 'and', 
		       hipe_rtl:mk_imm(?BIT_SET(7,4))),
       hipe_rtl:mk_branch(Tmp1, eq, hipe_rtl:mk_imm(0), TrueLblName, FalseLblName)];
    
    bs_size_test ->
      [PatternSize, BinSize] = Args,
      [hipe_rtl:mk_branch(PatternSize, eq, BinSize, TrueLblName, FalseLblName)];

    bs_size_test_all ->
      [PatternSize, BinSize] = Args,
      [hipe_rtl:mk_branch(PatternSize, le, BinSize, TrueLblName, FalseLblName)];
      
    {bs_get_integer, Size, OffsetConst, Flags} ->
      Base = hipe_rtl:mk_new_reg(),
      Offset = hipe_rtl:mk_new_reg(),
      BinSize = hipe_rtl:mk_new_reg(),
      case Dst of
	[] ->
	  [hipe_rtl:mk_goto(TrueLblName)];
	[Dst1] ->
	  Type = get_type(Flags),
	  {_Signedness, Endianess} = Type,
	  [OffsetVar, Orig, OrigOffset] = Args,
	  case {hipe_tagscheme:is_fixnum((1 bsl Size) - 1), Flags band 1, 
		Endianess, OffsetConst band ?LOW_BITS} of
	    {true, 1, _, 0} ->
	      get_base(Base, Orig, OrigOffset, OffsetVar) ++
		get_int_from_bin(Size, Base, OffsetConst, Dst1, TrueLblName, Type);
	    {true, _, big, _} ->
		get_base(Base, Orig, OrigOffset) ++
		[hipe_tagscheme:untag_fixnum(Offset, OffsetVar),
		 hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(OffsetConst))] ++
		get_int_from_unaligned_bin(Size, Base, Offset, Dst1, TrueLblName, Type);
	    _ ->
	      Tmp1 = hipe_rtl:mk_new_reg(),
	      Tmp2 = hipe_rtl:mk_new_reg(),
	      get_base(Base, Orig, OrigOffset) ++
		get_offset_and_binsize(Offset, BinSize, hipe_rtl:mk_imm(Size), OffsetConst, OffsetVar) ++
		[hipe_rtl:mk_move(Tmp1, hipe_rtl:mk_imm(Size)),
		 hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flags)),
		 gen_bs_call(bs_get_integer,
			     [Tmp1, Tmp2],
			     Dst1, BinSize, Base, Offset, Orig,
			     TrueLblName)]
	  end
      end;
    {bs_get_integer, OffsetConst, Flags} ->
      Base = hipe_rtl:mk_new_reg(),
      Offset = hipe_rtl:mk_new_reg(),
      BinSize = hipe_rtl:mk_new_reg(),
      case Dst of
	[] ->
	  [hipe_rtl:mk_goto(TrueLblName)];
	[Dst1] ->
	  Type=get_type(Flags),
	  [SizeVar, OffsetVar, Orig, OrigOffset] = Args,
	  case {Flags band 1, OffsetConst band ?LOW_BITS} of
	    {1, 0} ->
	      [hipe_rtl:mk_gctest(2)] ++
	      get_base(Base, Orig, OrigOffset, OffsetVar) ++
	      get_unknown_size_int(SizeVar, Flags, Base, OffsetConst, Orig, Dst1, TrueLblName, Type); 
	    _ ->
	      Tmp1 = hipe_rtl:mk_new_reg(),
	      Tmp2 = hipe_rtl:mk_new_reg(),
	      get_base(Base, Orig, OrigOffset) ++
		[hipe_tagscheme:untag_fixnum(Tmp1, SizeVar)] ++
		get_offset_and_binsize(Offset, BinSize, Tmp1, OffsetConst, OffsetVar) ++
		[hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flags)),
		 gen_bs_call(bs_get_integer,
			     [Tmp1, Tmp2],
			     Dst1, BinSize, Base, Offset, Orig,
			     TrueLblName)]
	  end
      end;
    {bs_get_binary, Size, OffsetConst, Flag} ->
      Tmp1 = hipe_rtl:mk_new_reg(),
      Tmp2 = hipe_rtl:mk_new_reg(),
      case Dst of 
	[] ->
	  hipe_rtl:mk_goto(TrueLblName);
	[Dst1] ->
	  [OffsetVar, Orig, OrigOffset] = Args,
	  SuccessLbl=hipe_rtl:mk_new_label(),
	  CallLbl=hipe_rtl:mk_new_label(),
	  Offset=hipe_rtl:mk_new_reg(),
	  OffsetReg=hipe_rtl:mk_new_reg(),
	  OrigOffsetReg=hipe_rtl:mk_new_reg(),
	  Base=hipe_rtl:mk_new_reg(),
	  BinSize=hipe_rtl:mk_new_reg(),
	  Code1 =
	    [hipe_rtl:mk_gctest(?SUB_BIN_WORDSIZE),
	     hipe_tagscheme:untag_fixnum(OffsetReg, OffsetVar),
	     hipe_tagscheme:untag_fixnum(OrigOffsetReg, OrigOffset),
	     hipe_rtl:mk_alu(Offset, OffsetReg, add, hipe_rtl:mk_imm(OffsetConst))],
	  SubCode =
	    [hipe_rtl:mk_alu(Offset, Offset, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
	     hipe_rtl:mk_alu(Offset, Offset, add, OrigOffsetReg),
	     hipe_rtl:mk_move(Tmp1, hipe_rtl:mk_imm(Size bsr ?BYTE_SHIFT))]
	  ++ hipe_tagscheme:safe_mk_sub_binary(Dst1, Tmp1, Offset, Orig)
	    ++ [hipe_rtl:mk_goto(TrueLblName)],
	  Code2=
	    case  {Flag band 1, OffsetConst band ?LOW_BITS} of 
	      {1, 0} ->
		SubCode;
	      {_, _X} ->
		[hipe_rtl:mk_alub(Tmp2, Offset, 'and', hipe_rtl:mk_imm(?LOW_BITS), eq, 
				  hipe_rtl:label_name(SuccessLbl), hipe_rtl:label_name(CallLbl)),
		 SuccessLbl] ++
		  SubCode ++
		  [CallLbl,
		   get_base(Base, Orig, OrigOffset),
		   hipe_rtl:mk_alu(BinSize, Offset, add, hipe_rtl:mk_imm(Size)),
		   hipe_rtl:mk_move(Tmp1, hipe_rtl:mk_imm(Size)),
		   hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flag)),
		   gen_bs_call(bs_get_binary,
			       [Tmp1, Tmp2],
			       Dst1, BinSize, Base, Offset, Orig,
			       TrueLblName)]
	    end,
	  Code1++Code2
      end;
    {bs_get_binary, OffsetConst, Flag} ->
      Tmp1 = hipe_rtl:mk_new_reg(),
      Tmp2 = hipe_rtl:mk_new_reg(),
      case Dst of 
	[] ->
	  hipe_rtl:mk_goto(TrueLblName);
	[Dst1] ->
	  [SizeVar, OffsetVar, Orig, OrigOffset] = Args,
	  SuccessLbl=hipe_rtl:mk_new_label(),
	  CallLbl=hipe_rtl:mk_new_label(),
	  Offset=hipe_rtl:mk_new_reg(),
	  OffsetReg=hipe_rtl:mk_new_reg(),
	  OrigOffsetReg=hipe_rtl:mk_new_reg(),
	  SizeReg = hipe_rtl:mk_new_reg(),
	  Base=hipe_rtl:mk_new_reg(),
	  BinSize=hipe_rtl:mk_new_reg(),
	  Code1 =
	    [hipe_rtl:mk_gctest(?SUB_BIN_WORDSIZE),
	     hipe_tagscheme:untag_fixnum(SizeReg, SizeVar),
	     hipe_tagscheme:untag_fixnum(OffsetReg, OffsetVar),
	     hipe_tagscheme:untag_fixnum(OrigOffsetReg, OrigOffset),
	     hipe_rtl:mk_alu(Offset, OffsetReg, add, hipe_rtl:mk_imm(OffsetConst))],
	  SubCode =
	    [hipe_rtl:mk_alu(Offset, Offset, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
	     hipe_rtl:mk_alu(Offset, Offset, add, OrigOffsetReg),
	     hipe_rtl:mk_alu(Tmp1, SizeReg, srl,  hipe_rtl:mk_imm(?BYTE_SHIFT))]
	    ++ hipe_tagscheme:safe_mk_sub_binary(Dst1, Tmp1, Offset, Orig)
	    ++ [hipe_rtl:mk_goto(TrueLblName)],
	  Code2=
	    case  {Flag band 1, OffsetConst band ?LOW_BITS} of 
	      {1, 0} ->
		SubCode;
	      {_, _X} ->
		[hipe_rtl:mk_alub(Tmp2, Offset, 'and', hipe_rtl:mk_imm(?LOW_BITS), eq, 
				  hipe_rtl:label_name(SuccessLbl), hipe_rtl:label_name(CallLbl)),
		 SuccessLbl] ++
		  SubCode ++
		  [CallLbl,
		   get_base(Base, Orig, OrigOffset),
		   hipe_rtl:mk_alu(BinSize, Offset, add, SizeReg),
		   hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flag)),
		   gen_bs_call(bs_get_binary,
			       [SizeReg, Tmp2],
			       Dst1, BinSize, Base, Offset, Orig,
			       TrueLblName)]
	    end,
	  Code1++Code2
      end;	
    {bs_get_binary_all, OffsetConst, _Flags} ->
      case Dst of 
	[] ->
	  hipe_rtl:mk_goto(TrueLblName);
	[Dst1] ->
	  Offset=hipe_rtl:mk_new_reg(),
	  OffsetReg=hipe_rtl:mk_new_reg(),
	  OrigOffsetReg=hipe_rtl:mk_new_reg(),
	  Tmp1 = hipe_rtl:mk_new_reg(),
	  BinSizeReg=hipe_rtl:mk_new_reg(),
	  [OffsetVar, Orig, OrigOffset, BinSize] = Args,
	  [hipe_rtl:mk_gctest(?SUB_BIN_WORDSIZE),
	   hipe_tagscheme:untag_fixnum(OffsetReg, OffsetVar),
	   hipe_tagscheme:untag_fixnum(OrigOffsetReg, OrigOffset),
	   hipe_tagscheme:untag_fixnum(BinSizeReg, BinSize),
	   hipe_rtl:mk_alu(Offset, OffsetReg, add, hipe_rtl:mk_imm(OffsetConst)),
	   hipe_rtl:mk_alu(Offset, Offset, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
	   hipe_rtl:mk_alu(BinSizeReg, BinSizeReg, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
	   hipe_rtl:mk_alu(Tmp1, BinSizeReg, sub,  Offset),
	   hipe_rtl:mk_alu(Offset, Offset, add, OrigOffsetReg)]
	    ++ hipe_tagscheme:unsafe_mk_sub_binary(Dst1, Tmp1, Offset, Orig)
	    ++ [hipe_rtl:mk_goto(TrueLblName)]
      end;
%%Floats mean trouble
    
    {bs_get_float, Size, OffsetConst, Flags} ->
      Tmp1 = hipe_rtl:mk_new_reg(),
      Tmp2 = hipe_rtl:mk_new_reg(),
      BinSize = hipe_rtl:mk_new_reg(),
      Offset=hipe_rtl:mk_new_reg(),
      Base=hipe_rtl:mk_new_reg(),
      [Dst1] = Dst,
      [OffsetVar, Orig, OrigOffset] = Args,
      Type=get_type(Flags),
      Code1 =
	[hipe_rtl:mk_gctest(3),
	 get_base(Base, Orig, OrigOffset)] ++
	[hipe_tagscheme:untag_fixnum(Offset, OffsetVar),
	 hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(OffsetConst))],
      Code2 =
	case {Size, Flags band 1, OffsetConst band ?LOW_BITS} of
	  {64, 1, 0} ->        %inlined for fix size 64 and byte-aligned
	    get_64float_from_bin(Base, Offset, Dst1, TrueLblName, Type);
	  
	  _ ->
	    [hipe_rtl:mk_alu(BinSize, Offset, add, hipe_rtl:mk_imm(Size)),	
	     hipe_rtl:mk_move(Tmp1, hipe_rtl:mk_imm(Size)),
	     hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flags)),
	     gen_bs_call(bs_get_float,
			 [Tmp1, Tmp2],
			 Dst1, BinSize, Base, Offset, Orig, 
			 TrueLblName)]
	end,
      Code1 ++ Code2;
    
    {bs_get_float, OffsetConst, Flags} ->	  
      SizeReg = hipe_rtl:mk_new_reg(),
      Tmp2 = hipe_rtl:mk_new_reg(),
      BinSize = hipe_rtl:mk_new_reg(),
      Offset=hipe_rtl:mk_new_reg(),
      Base=hipe_rtl:mk_new_reg(),
      [Dst1] = Dst,
      [SizeVar, OffsetVar, Orig, OrigOffset] = Args,
      Type=get_type(Flags),
      Code1 =
	[hipe_rtl:mk_gctest(3),
	 get_base(Base, Orig, OrigOffset)] ++
	[hipe_tagscheme:untag_fixnum(Offset, OffsetVar),
	 hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(OffsetConst)),
	 hipe_tagscheme:untag_fixnum(SizeReg, SizeVar)],
      Code2 =
	case {Flags band 1, OffsetConst band ?LOW_BITS} of    
	  {1, 0} ->              %inlined for variable size 64 and byte-aligned
	    UsecLbl=hipe_rtl:mk_new_label(),
	    SuccessLbl=hipe_rtl:mk_new_label(),
	    [hipe_rtl:mk_branch(SizeReg, eq, hipe_rtl:mk_imm(64), hipe_rtl:label_name(SuccessLbl), hipe_rtl:label_name(UsecLbl)),
	     SuccessLbl,
	     get_64float_from_bin(Base, Offset, Dst1, TrueLblName, Type),
	     UsecLbl,
	     hipe_rtl:mk_alu(BinSize, Offset, add, SizeReg),
	     hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flags)),
	     gen_bs_call(bs_get_float,
			 [SizeReg, Tmp2],
			 Dst1, BinSize, Base, Offset, Orig,
			 TrueLblName)];
	  _ ->
	    [hipe_rtl:mk_alu(BinSize, Offset, add, SizeReg),
	     hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flags)),
	     gen_bs_call(bs_get_float,
			 [SizeReg, Tmp2],
			 Dst1, BinSize, Base, Offset, Orig,
			 TrueLblName)]
	end,
      Code1 ++ Code2
  end.
get_base(Base, Orig, OrigOffset) ->
  Tmp1=hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_binbase(Tmp1, Orig, OrigOffset),
   hipe_rtl:mk_move(Base, Tmp1)].

%get_base(Base, Orig, OrigOffset) ->
%  Tmp1=hipe_rtl:mk_new_reg(),
%  HeapLbl=hipe_rtl:mk_new_label(),
%  REFCLbl=hipe_rtl:mk_new_label(),
%  JoinLbl=hipe_rtl:mk_new_label(),
%  [hipe_tagscheme:test_heap_binary(Orig, hipe_rtl:label_name(HeapLbl), hipe_rtl:label_name(REFCLbl)),
%   HeapLbl,
%   hipe_rtl:mk_alu(Base, Orig, add, hipe_rtl:mk_imm(?HEAP_BIN_DATA-2)),
%   hipe_rtl:mk_goto(hipe_rtl:label_name(JoinLbl)),
%   REFCLbl,
%   hipe_rtl:mk_load(Base, Orig, hipe_rtl:mk_imm(?PROC_BIN_BYTES-2)),
%   JoinLbl,
%   hipe_tagscheme:untag_fixnum(Tmp1, OrigOffset),
%   hipe_rtl:mk_alu(Base, Base, add, Tmp1)].

get_base(Base, Orig, OrigOffset, OffsetVar) ->
  Tmp1=hipe_rtl:mk_new_reg(),
  get_base(Base, Orig, OrigOffset) ++
    [hipe_tagscheme:untag_fixnum(Tmp1, OffsetVar),
     hipe_rtl:mk_alu(Tmp1, Tmp1, sra, hipe_rtl:mk_imm(?BYTE_SHIFT)),
     hipe_rtl:mk_alu(Base, Base, add, Tmp1)].

gen_bs_call(Name, Args, DstVar, BinSize, Base, Offset, Orig, TrueLblName) ->
  hipe_rtl_bin_util:bs_call(Name, Args, DstVar, BinSize, Base, Offset, 
			    Orig, TrueLblName, []).

get_int_from_unaligned_bin(Size, Base, Offset, Dst1, TrueLblName, Type)  ->
  Shiftr= get_shiftr(Type),
  hipe_rtl_bin_util:get_unaligned_int(Dst1, Size, Base, Offset, Shiftr, 
				    Type, TrueLblName). 

get_int_from_bin(Size, Base, Offset, Dst1, TrueLblName, Type) ->
  Shiftr= get_shiftr(Type),
  hipe_rtl_bin_util:get_int(Dst1, Size, Base, hipe_rtl:mk_imm(Offset), 
			    Shiftr, Type, TrueLblName). 

get_unknown_size_int(SizeVar, Flags, Base, OffsetConst, Orig, Dst1, 
		     TrueLblName, Type) -> 
  Shiftr= get_shiftr(Type),
  SuccessLbl = hipe_rtl:mk_new_label(),
  UsecLbl = hipe_rtl:mk_new_label(),
  Tmp1 = hipe_rtl:mk_new_reg(),
  Tmp2 = hipe_rtl:mk_new_reg(),
  BinSize = hipe_rtl:mk_new_reg(),
  Offset = hipe_rtl:mk_imm(OffsetConst),
  WordSize = hipe_rtl_arch:word_size(),
  Code1 = [hipe_tagscheme:untag_fixnum(Tmp1, SizeVar),
	   hipe_rtl:mk_branch(Tmp1, le, hipe_rtl:mk_imm(?BYTE_SIZE*WordSize), 
			      hipe_rtl:label_name(SuccessLbl), 
			      hipe_rtl:label_name(UsecLbl)),
	   SuccessLbl,
	   hipe_rtl:mk_alu(Tmp1, Tmp1, add, Offset)],
  Code2 =
    case element(2,Type) of
      little ->
	hipe_rtl_bin_util:get_little_unknown_int(Dst1, Base, Offset, Tmp1, 
						Shiftr, Type, TrueLblName);
      big ->
	hipe_rtl_bin_util:get_big_unknown_int(Dst1, Base, Offset, Tmp1, 
					     Shiftr, Type, TrueLblName)
    end,
  Code3 =
    [UsecLbl,
     hipe_rtl:mk_alu(BinSize, Offset, add, Tmp1), 
     hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flags))] ++
    gen_bs_call(bs_get_integer,
	       [Tmp1, Tmp2],
	       Dst1, BinSize, Base, Offset, Orig,
	       TrueLblName),
  Code1 ++ Code2 ++ Code3.


get_offset_and_binsize(Offset, BinSize, Size, OffsetConst, OffsetVar) ->
  OffsetReg = hipe_rtl:mk_new_reg(),
  [hipe_tagscheme:untag_fixnum(OffsetReg, OffsetVar),
   hipe_rtl:mk_alu(Offset, OffsetReg, add, hipe_rtl:mk_imm(OffsetConst)),
   hipe_rtl:mk_alu(BinSize, Offset, add, Size)].

get_64float_from_bin(Base, Offset, Dst1, TrueLblName, Type) ->
  Tmp2 = hipe_rtl:mk_new_reg(),
  Tmp3 = hipe_rtl:mk_new_reg(), 
  ByteOffset = hipe_rtl:mk_new_reg(),
  Code1 = [hipe_rtl:mk_alu(ByteOffset, Offset, srl, hipe_rtl:mk_imm(3)),
	   hipe_rtl_bin_util:load_bytes(Tmp2, Base, ByteOffset, Type, 4),
	   hipe_rtl_bin_util:load_bytes(Tmp3, Base, ByteOffset, Type, 4)],
  Code2 = case {Type, hipe_rtl_arch:endianess()}  of 
	    {{_, big}, big} ->
	      [hipe_tagscheme:unsafe_mk_float(Dst1, Tmp2, Tmp3),
	       hipe_rtl:mk_goto(TrueLblName)];
	    {{_, little}, big} ->
	      [hipe_tagscheme:unsafe_mk_float(Dst1, Tmp3, Tmp2),
	       hipe_rtl:mk_goto(TrueLblName)];
	    {{_, little}, little} ->
	      [hipe_tagscheme:unsafe_mk_float(Dst1, Tmp2, Tmp3),
	       hipe_rtl:mk_goto(TrueLblName)];
	    {{_, big}, little} ->
	      [hipe_tagscheme:unsafe_mk_float(Dst1, Tmp3, Tmp2),
	       hipe_rtl:mk_goto(TrueLblName)]
	  end,
  Code1 ++ Code2.

get_type(Flags) ->
  Signedness =          %expands the Flags argument
    case Flags band 4 of
      4 -> signed;
      0 -> unsigned
    end,
  Endianess = 
    case Flags band 2 of
      2 -> little;
      0 -> big
    end,
  {Signedness, Endianess}.

get_shiftr({unsigned,_}) -> srl;
get_shiftr({signed,_}) -> sra.	  		   
