%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%%  Filename : 	hipe_rtl_inline_bs_ops.erl
%%  Module   :	hipe_rtl_inline_bs_ops
%%  Purpose  :  
%%  Notes    : 
%%  History  :	*2001-06-14 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: pergu $
%%              $Date: 2003/05/22 09:26:10 $
%%              $Revision: 1.19 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_rtl_inline_bs_ops).
-export([gen_rtl/6]).
%%-------------------------------------------------------------------------

-include("../main/hipe.hrl").
-include("hipe_icode2rtl.hrl").
-include("hipe_literals.hrl").
-define(FIVE_HIGH_BITS, 31 bsl 27).
-define(SIZE_OFFSET, 4).
-define(BYTE_SHIFT, 3).
-define(SUB_OFFSET_OFFSET, 8).
-define(SUB_ORIG_OFFSET, 12).
-define(HEAP_DATA_OFFSET, 8).
-define(REFC_POINTER_DATA_OFFSET, 16).
%% -------------------------------------------------------------------------
%% The code is generated as a list of lists, it will be flattened later.
%% 

gen_rtl(BsOP,Args, Dst,TrueLblName, FalseLblName, ConstTab) ->
  case BsOP of
    {bs_put_string, String, SizeInBytes} ->
      [NewOffset] = Dst,
      StringBase = hipe_rtl:mk_new_reg(),
      {NewTab, Lbl} = 
	hipe_consttab:insert_block(ConstTab, 4, byte, String),
      [Base, Offset] = Args,
      {[hipe_rtl:mk_load_address(StringBase, Lbl, constant)|
	copy_string(StringBase, SizeInBytes, Base, Offset, NewOffset, TrueLblName)],
       NewTab};

    _ -> 
      Code = 
	case BsOP of

	  {bs_init, fail} ->
	    [hipe_rtl:mk_goto(FalseLblName)];

	  {bs_init, {Const, Units}} ->
	    SaveVar = hipe_rtl:mk_new_var(),
	    Size = hipe_rtl:mk_new_reg(),
	    Tmp1 = hipe_rtl:mk_new_reg(),
	    ByteSize = hipe_rtl:mk_new_reg(),
	    NumberOfBytes = hipe_rtl:mk_new_reg(),
	    NumberOfWords = hipe_rtl:mk_new_reg(),
	    SuccessLbl = hipe_rtl:mk_new_label(),
	    HeapLbl = hipe_rtl:mk_new_label(),
	    REFCLbl = hipe_rtl:mk_new_label(),
	    [Base, Offset] = Dst,
	    calculate_size(Size, Const, Units, Args, FalseLblName) ++ 
	      [hipe_rtl:mk_alub(Tmp1, Size, 'and', hipe_rtl:mk_imm(7), eq, hipe_rtl:label_name(SuccessLbl), FalseLblName, 0.99),
	       SuccessLbl,
	       hipe_rtl:mk_alu(ByteSize, Size, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
	       hipe_rtl:mk_alu(SaveVar, ByteSize, sll, hipe_rtl:mk_imm(4)),
	       hipe_rtl:mk_alu(SaveVar, SaveVar, add, hipe_rtl:mk_imm(15)),
	       hipe_rtl:mk_branch(ByteSize, le, hipe_rtl:mk_imm(64), 
				  hipe_rtl:label_name(HeapLbl), hipe_rtl:label_name(REFCLbl), 0.50),
	       HeapLbl,
	       hipe_rtl:mk_alu(NumberOfBytes, ByteSize, 'add', hipe_rtl:mk_imm(11)),  %%Add 11 to ensure that everyrthing has room
	       hipe_rtl:mk_alu(NumberOfWords, NumberOfBytes, sra, hipe_rtl:mk_imm(2)),
	       hipe_rtl:mk_gctest(NumberOfWords),
	       hipe_tagscheme:untag_fixnum(ByteSize, SaveVar),
	       hipe_tagscheme:get_base(Base, ByteSize),
	       hipe_rtl:mk_move(Offset, hipe_rtl:mk_imm(0)),
	       hipe_rtl:mk_goto(TrueLblName),
	       REFCLbl,
	       hipe_rtl:mk_gctest(tagged(?PROC_BIN_BYTESIZE)),
	       hipe_tagscheme:untag_fixnum(ByteSize, SaveVar),
	       hipe_rtl:mk_move(Offset, hipe_rtl:mk_imm(0)),
	       hipe_rtl:mk_call([Base], bs_allocate, [ByteSize],  c, TrueLblName,[])];
	  
	  {bs_create_space, Size, Shifts} ->
	    NumberOfWords=hipe_rtl:mk_new_reg(),
	    Scratch = hipe_rtl:mk_new_reg(),
	    [hipe_rtl:mk_move(NumberOfWords, hipe_rtl:mk_imm(Size))]
	      ++ expand_runtime(Shifts, Args, NumberOfWords, Scratch)
	      ++ [hipe_rtl:mk_gctest(NumberOfWords),
		  hipe_rtl:mk_goto(TrueLblName)];
					
	  bs_start_match ->
	    
	    [BinSize, Base, Offset, Orig, OrigOffset] = Dst,
	    [Binary] = Args,
	    get_binary_bytes(Binary,BinSize, Base, Offset, Orig, OrigOffset, TrueLblName, FalseLblName);
	   
	  %% {bs_need_buf, Need} -> []; %ignorerar bs_need_buf

	  {bs_put_binary_all, Flags} ->
	    Orig = hipe_rtl:mk_new_reg(),
	    OrigOffset = hipe_rtl:mk_new_reg(),
	    CopyBase = hipe_rtl:mk_new_reg(),
	    CopyOffset = hipe_rtl:mk_new_reg(),
	    CopyEnd = hipe_rtl:mk_new_reg(),
	    CopySize = hipe_rtl:mk_new_reg(),
	    NewSize = hipe_rtl:mk_new_reg(),
	    NextLbl = hipe_rtl:mk_new_label(),
	    [Src, Base, Offset] = Args,
	    [NewOffset] = Dst,
	    Code1 = get_binary_bytes(Src, CopyEnd, CopyBase, CopyOffset, Orig, OrigOffset,
				     hipe_rtl:label_name(NextLbl), FalseLblName)++
	      [NextLbl,
	       hipe_rtl:mk_alu(CopySize, CopyEnd, sub, CopyOffset)],
	    case Flags band 1 of
	      1 ->
		Code1 ++ 
		  copy_aligned_bytes_all(CopyBase, CopyOffset, CopySize, Base, Offset, NewOffset, TrueLblName);
	      0 ->
		Code1 ++
		  [hipe_rtl:mk_move(NewSize, CopySize)|
		   copy_bytes(CopyBase, CopyOffset, CopySize, Base, Offset, NewOffset, NewSize, TrueLblName, FalseLblName)]	
	    end;

	  {bs_put_binary, Size, Flags} ->
	    Tmp2 = hipe_rtl:mk_new_reg(),
	    Orig = hipe_rtl:mk_new_reg(),
	    OrigOffset = hipe_rtl:mk_new_reg(),
	    CopyBase = hipe_rtl:mk_new_reg(),
	    CopyOffset = hipe_rtl:mk_new_reg(),
	    CopySize = hipe_rtl:mk_new_reg(),
	    NextLbl = hipe_rtl:mk_new_label(),
	    [NewOffset] = Dst,
	    case Args of
	      [Src, Base, Offset] ->
		Code1 = get_binary_bytes(Src, CopySize, CopyBase, CopyOffset, Orig, OrigOffset,
					 hipe_rtl:label_name(NextLbl), FalseLblName)++
		  [NextLbl],
		case Size of 
		  0 ->
		    [hipe_rtl:mk_goto(TrueLblName)];
		  _ ->
		    case Size band 7 of
		      0 ->
			case Flags band 1 of
			  1 ->
			    Code1 ++ 
			      copy_aligned_static_bytes(CopyBase, CopyOffset, CopySize, 
							Base, Offset, NewOffset, Size, TrueLblName, FalseLblName);
			  0 ->
			    Code1 ++
			      copy_static_bytes(CopyBase, CopyOffset, CopySize, Base, 
						Offset, NewOffset, Size, TrueLblName, FalseLblName)
			end;
		      _ ->
			[hipe_rtl:mk_goto(FalseLblName)]
		    end
		end;
	      [Src, Bits,  Base, Offset]  ->
		Code1 = 
		  get_binary_bytes(Src, CopySize, CopyBase, CopyOffset, Orig, OrigOffset,
				   hipe_rtl:label_name(NextLbl), FalseLblName) ++	
		  [NextLbl] ++
		  gen_make_size(Tmp2, Size, Bits, FalseLblName),
		case Flags band 1 of
		  1 ->
		    Code1 ++ 
		      copy_aligned_bytes(CopyBase, CopyOffset, CopySize, Base, Offset, NewOffset, Tmp2, TrueLblName, FalseLblName);
		  0 ->
		    Code1 ++
		      copy_bytes(CopyBase, CopyOffset, CopySize, Base, Offset, NewOffset, Tmp2, TrueLblName, FalseLblName)	
		end
	    end;

	  {bs_put_float, Size, Flags, ConstInfo} ->    
	    SizeReg = hipe_rtl:mk_new_reg(),
	    FlagsReg = hipe_rtl:mk_new_reg(),	
	    PassedLbl = hipe_rtl:mk_new_label(),
	    [NewOffset] = Dst,
	    case ConstInfo of
	      fail ->
		[hipe_rtl:mk_goto(FalseLblName)];
	      _ ->
		case Args of
		  [Src, Base, Offset] ->
		    case {Flags band 3, Size}  of
		      {1, 64} ->
			copy_float_big(Base, Offset, NewOffset, Src,  FalseLblName, TrueLblName, ConstInfo);
		      {3, 64} ->
			copy_float_little(Base, Offset, NewOffset, Src, FalseLblName, TrueLblName, ConstInfo);
		      _ ->
			[hipe_rtl:mk_move(SizeReg, hipe_rtl:mk_imm(Size)),
			 hipe_rtl:mk_move(FlagsReg, hipe_rtl:mk_imm(Flags)),
			 gen_test_sideffect_bs_call(bs_put_small_float, [Src, SizeReg, Base, Offset, FlagsReg],
						    hipe_rtl:label_name(PassedLbl), FalseLblName),
			 PassedLbl,
			 hipe_rtl:mk_alu(NewOffset, Offset, add, SizeReg),
			 hipe_rtl:mk_goto(TrueLblName)];
		      _ ->
			[hipe_rtl:mk_goto(FalseLblName)]
		    end;
		    
		  [Src, Bits, Base, Offset] ->
		    gen_make_size(SizeReg, Size, Bits, FalseLblName) ++
		      [hipe_rtl:mk_move(FlagsReg, hipe_rtl:mk_imm(Flags)),
		       gen_test_sideffect_bs_call(bs_put_small_float, [Src, SizeReg, Base, Offset, FlagsReg],
						  hipe_rtl:label_name(PassedLbl), FalseLblName),
		       PassedLbl,
		       hipe_rtl:mk_alu(NewOffset, Offset, add, SizeReg),
		       hipe_rtl:mk_goto(TrueLblName)]
		end
	    end;

	  {bs_put_integer, Size, Flags, ConstInfo} ->
	    SizeReg = hipe_rtl:mk_new_reg(),
	    FlagsReg = hipe_rtl:mk_new_reg(),
	    UntaggedSrc = hipe_rtl:mk_new_reg(),
	    [NewOffset] = Dst,
	    case ConstInfo of
	      fail ->
		[hipe_rtl:mk_goto(FalseLblName)];
	      _ ->
		case Args of
		 [Src, Base, Offset]  ->
		    Arguments = [SizeReg, Base, Offset, FlagsReg],
		    Prep= [hipe_rtl:mk_move(SizeReg, hipe_rtl:mk_imm(Size)),
			   hipe_rtl:mk_move(FlagsReg, hipe_rtl:mk_imm(Flags))] ++
		      first_step(Src, UntaggedSrc, Arguments, NewOffset, TrueLblName, FalseLblName),
		    End = [hipe_rtl:mk_goto(TrueLblName)],
		      case Flags band 3  of
			0 ->
			  Prep ++
			  copy_offset_int_big(Base, Offset, NewOffset, Size, UntaggedSrc) ++
			    End;
			1 ->
			  Prep ++
			  copy_int_big(Base, Offset, NewOffset, Size, UntaggedSrc) ++
			    End;
			2 ->
			  PassedLbl = hipe_rtl:mk_new_label(),
			  [hipe_rtl:mk_move(SizeReg, hipe_rtl:mk_imm(Size)),
			   hipe_rtl:mk_move(FlagsReg, hipe_rtl:mk_imm(Flags)),
			   gen_test_sideffect_bs_call(bs_put_big_integer,[Src|Arguments], 
						      hipe_rtl:label_name(PassedLbl), FalseLblName),
			   PassedLbl,
			   hipe_rtl:mk_alu(NewOffset, Offset, add, SizeReg)] ++
			    End;
			3 ->
			  Prep ++
			    copy_int_little(Base, Offset, NewOffset, Size, UntaggedSrc) ++
			    End
		      end;
		  
		  [Src, Bits, Base, Offset] ->
		    Arguments = [SizeReg, Base, Offset, FlagsReg],
		    Prep =
		      [hipe_rtl:mk_move(FlagsReg, hipe_rtl:mk_imm(Flags))|
		       gen_make_size(SizeReg, Size, Bits, FalseLblName)] ++
		      first_step(Src, UntaggedSrc, [SizeReg, Base, Offset, FlagsReg], NewOffset, TrueLblName, FalseLblName),
		    End = [hipe_rtl:mk_goto(TrueLblName)],  
		    case Flags band 3  of
		      0 ->
			Prep ++
			  copy_offset_int_big(Base, Offset, NewOffset, Size, UntaggedSrc) ++
			  End;
		      1 ->
			Prep ++
			  copy_int_big(Base, Offset, NewOffset, Size, UntaggedSrc) ++
			  End;
		      2 ->
			PassedLbl = hipe_rtl:mk_new_label(),
			[gen_test_sideffect_bs_call(bs_put_big_integer,[Src|Arguments], 
						      hipe_rtl:label_name(PassedLbl), FalseLblName),
			 PassedLbl,
			 hipe_rtl:mk_alu(NewOffset, Offset, add, SizeReg)] ++
			  End;
		      3 ->
			Prep ++
			  copy_int_little(Base, Offset, NewOffset, Size, UntaggedSrc) ++
			  End
		    end
		end
	    end;
	  
	  %%Inlined for all possible cases
	  {bs_skip_bits_all, Flags} ->
	    [NewOffset] = Dst,
	    [BinSize, Offset]= Args,
	    case (Flags band ?BSF_ALIGNED) of
	      1 -> %% This can't fail
		[hipe_rtl:mk_move(NewOffset, BinSize),
		 hipe_rtl:mk_goto(TrueLblName)];
	      _ ->
		SuccessLbl=hipe_rtl:mk_new_label(),
		[hipe_rtl:mk_alub(Offset, Offset, 'and', hipe_rtl:mk_imm(2#111), eq, hipe_rtl:label_name(SuccessLbl), FalseLblName, 0.99),
		 SuccessLbl,
		 hipe_rtl:mk_move(NewOffset, BinSize),
		 hipe_rtl:mk_goto(TrueLblName)]
	    end;

	  %%Inlined for all possible cases
	  {bs_skip_bits, Bits} ->
	    [NewOffset] = Dst,
	    case Args of
	      [BinSize, Offset] ->
		skip_no_of_bits(BinSize, Offset, NewOffset,  hipe_rtl:mk_imm(Bits), TrueLblName, FalseLblName);

	      [Arg,BinSize, Offset] ->
		Tmp1 = hipe_rtl:mk_new_reg(),
		gen_make_size(Tmp1, Bits, Arg, FalseLblName) 
		  ++ skip_no_of_bits(BinSize, Offset, NewOffset, Tmp1, TrueLblName, FalseLblName)
	    end;

	  {bs_get_integer,0,_Flag} ->
	    [Dst1, NewOffset] = Dst,
	    [_BinSize, _Base, Offset, _Orig]= Args,
 
	    [hipe_rtl:mk_move(NewOffset, Offset),
	     hipe_rtl:mk_move(Dst1, hipe_rtl:mk_imm(15)),
	     hipe_rtl:mk_goto(TrueLblName)];

	  %%Inlined when Size is const or variable smaller than 28 and the normalendianess 
	  {bs_get_integer,Size,Flag} ->
	    
	    [Dst1, NewOffset] = Dst,
	    Signedness = 
	      case Flag band 4 of
		4 -> signed;
		0 -> unsigned
	      end,
	    Endianess = 
	      case Flag band 2  of
		2 -> little;
		0 -> big
	      end,
	    Type={Signedness, Endianess},
	    case Args of
	      [BinSize, Base, Offset, Orig] ->
		case {Size =< 32, Flag band 1, Endianess} of
		  
		  %%Case when size is known and Binary is aligned
		  {true, 1, _} ->                          
		    get_int_from_bin(Size, BinSize, Base, Offset, NewOffset, Dst1, FalseLblName, TrueLblName, Type);
		  
		  %%Case when size is known binary might be aligned
		  {true, 0, big} ->
		    get_int_from_unaligned_bin(Size, Flag, BinSize, Base, Offset, NewOffset, Orig, Dst1, FalseLblName, TrueLblName, Type);
		
		  _ ->
		    Tmp1 = hipe_rtl:mk_new_reg(),
		    Tmp2 = hipe_rtl:mk_new_reg(),
		    [hipe_rtl:mk_move(Tmp1, hipe_rtl:mk_imm(Size)),
		     hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flag)),
		     gen_bs_call(bs_get_integer,
				 [Tmp1, Tmp2],
				 Dst1, BinSize, Base, Offset, NewOffset, Orig,
				 TrueLblName, FalseLblName)]
		end;

	      [Arg, BinSize, Base, Offset, Orig] ->
		case {Flag band 1, Size rem 8, Endianess} of
		  %%Case when size is unknown binary aligned
		  {1, 0, _}  ->
		    get_unknown_size_int(Arg, Size, Flag, BinSize, Base, Offset, NewOffset, Orig, Dst1, FalseLblName, TrueLblName, Type);
		  {1,_,big} ->
		    get_unknown_size_int(Arg, Size, Flag, BinSize, Base, Offset, NewOffset, Orig, Dst1, FalseLblName, TrueLblName, Type);
		  _ ->
		    Tmp1 = hipe_rtl:mk_new_reg(),
		    Tmp2 = hipe_rtl:mk_new_reg(),
		    [gen_make_size(Tmp1, Size, Arg, FalseLblName),
		     hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flag)),
		     gen_bs_call(bs_get_integer,
				 [Tmp1, Tmp2],
				 Dst1, BinSize, Base, Offset, NewOffset, Orig,
				 TrueLblName, FalseLblName)]
		end
	    end;

	  %%Inlined when float size is 64 and binary is byte-aligned
	  {bs_get_float,Size,Flag} ->
	    Tmp1 = hipe_rtl:mk_new_reg(),
	    Tmp2 = hipe_rtl:mk_new_reg(),
	    [Dst1, NewOffset] = Dst,
	    Signedness =          %expands the Flags argument
	      case Flag band 4 of
		4 -> signed;
		0 -> unsigned
	      end,
	    Endianess = 
	      case Flag band 2 of
		2 -> little;
		0 -> big
	      end,
	    Type={Signedness, Endianess},
	    case Args of
	      [BinSize, Base, Offset, Orig] ->
		case {Size, Flag band 1} of
		  {64, 1} ->        %inlined for fix size 64 and byte-aligned
		    get_64float_from_bin(BinSize, Base, Offset, NewOffset, Dst1, FalseLblName, TrueLblName, Type);

		  _ ->
		    [hipe_rtl:mk_move(Tmp1, hipe_rtl:mk_imm(Size)),
		     hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flag)),
		     gen_bs_call(bs_get_float,
				 [Tmp1, Tmp2],
				 Dst1, BinSize, Base, Offset, NewOffset, Orig, 
				 TrueLblName, FalseLblName)]
		end;
	      [Arg,BinSize, Base, Offset, Orig]  ->
	
		case {Flag band 1} of    
		  1 ->              %inlined for variable size 64 and byte-aligned
		    UsecLbl=hipe_rtl:mk_new_label(),
		    SuccessLbl=hipe_rtl:mk_new_label(),
		    [gen_make_size(Tmp1, Size, Arg, FalseLblName),
		     hipe_rtl:mk_branch(Tmp1, eq, hipe_rtl:mk_imm(64), hipe_rtl:label_name(SuccessLbl), hipe_rtl:label_name(UsecLbl)),
		     SuccessLbl,
		     get_64float_from_bin(BinSize, Base, Offset, NewOffset, Dst1, FalseLblName, TrueLblName, Type),
		     UsecLbl,
		     hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flag)),
		     gen_bs_call(bs_get_float,
				   [Tmp1, Tmp2],
				   Dst1, BinSize, Base, Offset, NewOffset, Orig,
				   TrueLblName, FalseLblName)];
		  _ ->
		    [gen_make_size(Tmp1, Size, Arg, FalseLblName),
		     hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flag)),
		     gen_bs_call(bs_get_float,
				 [Tmp1, Tmp2],
				 Dst1, BinSize, Base, Offset, NewOffset, Orig,
				 TrueLblName, FalseLblName)]
		end
	    end;

	  %%Inlined for all cases
	  {bs_get_binary_all, Flags} -> 
	    Tmp1 = hipe_rtl:mk_new_reg(),
	    Tmp2 = hipe_rtl:mk_new_reg(),
	    Tmp3 = hipe_rtl:mk_new_reg(),

	    OkLbl =  hipe_rtl:mk_new_label(),
	    [Dst1, NewOffset] = Dst,
	   
	    [BinSize, Offset, Orig, OrigOffset] = Args,
	    
	    Code1 =
	      case Flags band 1 of 
		1 ->
		  [];
		0 ->
		  [hipe_rtl:mk_alub(Tmp1, Offset, 'and', hipe_rtl:mk_imm(7), eq, hipe_rtl:label_name(OkLbl), FalseLblName, 0.99),
		   OkLbl]
	      end,
	    Code1 ++ 
	      [hipe_rtl:mk_alu(Tmp2, BinSize, sub, Offset),
	       hipe_rtl:mk_alu(Tmp2, Tmp2, srl, hipe_rtl:mk_imm(3)),
	       hipe_rtl:mk_alu(Tmp1, Offset, srl, hipe_rtl:mk_imm(3)),
	       hipe_rtl:mk_alu(Tmp3, Tmp1, add, OrigOffset),
	       hipe_rtl:mk_move(NewOffset, BinSize),
	       hipe_tagscheme:unsafe_mk_sub_binary(Dst1, Tmp2, Tmp3, Orig),
	       hipe_rtl:mk_goto(TrueLblName)];

	  %%Inlined when aligned
	  {bs_get_binary,Size,Flag} ->
	    Tmp1 = hipe_rtl:mk_new_reg(),
	    Tmp2 = hipe_rtl:mk_new_reg(),
	    Tmp3 = hipe_rtl:mk_new_reg(),
	    [Dst1, NewOffset] = Dst,
	    
	    Code1=
	      case Args of
		[BinSize, Base, Offset, Orig, OrigOffset] ->
		  [hipe_rtl:mk_move(Tmp1, hipe_rtl:mk_imm(Size))];

		[BitsVar, BinSize, Base, Offset, Orig, OrigOffset]  ->
		  gen_make_size(Tmp1, Size, BitsVar, FalseLblName)
	      end,
	    SuccessLbl0=hipe_rtl:mk_new_label(),
	    SuccessLbl1=hipe_rtl:mk_new_label(),
	    SuccessLbl2=hipe_rtl:mk_new_label(),
	    CallLbl=hipe_rtl:mk_new_label(),
	    RealOffset=hipe_rtl:mk_new_reg(),
	    SubCode =
	      [hipe_rtl:mk_alub(Tmp2, Tmp1, 'and', hipe_rtl:mk_imm(7), eq,hipe_rtl:label_name(SuccessLbl1),FalseLblName, 0.99),
	       SuccessLbl1,
	       check_size(Offset, Tmp1, BinSize, Tmp2, hipe_rtl:label_name(SuccessLbl2), FalseLblName),
	       SuccessLbl2,
	       hipe_rtl:mk_alu(Offset, Offset, srl, hipe_rtl:mk_imm(3)),
	       hipe_rtl:mk_alu(RealOffset, Offset, add, OrigOffset),
	       hipe_rtl:mk_alu(Tmp1, Tmp1, srl, hipe_rtl:mk_imm(3))]
	      ++ hipe_tagscheme:unsafe_mk_sub_binary(Dst1, Tmp1, RealOffset, Orig)
	      ++ [hipe_rtl:mk_move(NewOffset, Tmp2),
		  hipe_rtl:mk_goto(TrueLblName)],
	    Code2=
	      case Flag of 
		1 ->
		  SubCode;

		_ ->
		  [hipe_rtl:mk_alub(Tmp3, Offset, 'and', hipe_rtl:mk_imm(7), eq, 
				    hipe_rtl:label_name(SuccessLbl0), hipe_rtl:label_name(CallLbl)),
		   SuccessLbl0] ++
		    SubCode ++
		    [CallLbl,
		     hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flag)),
		     gen_bs_call(bs_get_binary,
				 [Tmp1, Tmp2],
				 Dst1, BinSize, Base, Offset, NewOffset, Orig,
				 TrueLblName, FalseLblName)]
	      end,
	    Code1++Code2;

	  %%Inlined for all cases
	  {bs_test_tail, NumBits} ->
	    [BinSize, Offset] = Args,
	    [hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(NumBits)),
	     hipe_rtl:mk_branch(Offset, eq, BinSize, TrueLblName, FalseLblName)];

	  {bs_restore, Index} ->
	    MatchBuf = hipe_rtl:mk_new_reg(),
	    [BinSize, Base, Offset, Orig] = Dst,
	    [hipe_rtl:mk_load_address(MatchBuf, erts_save_mb, c_const),
	     hipe_rtl:mk_alu(MatchBuf, MatchBuf, add, hipe_rtl:mk_imm(Index * 16)), 
	     load_matchbuffer(MatchBuf, BinSize, Base, Offset, Orig),
	     hipe_rtl:mk_goto(TrueLblName)];

	  {bs_save, Index} ->
	    MatchBuf = hipe_rtl:mk_new_reg(),
	    [BinSize, Base, Offset, Orig] = Args,
	    [hipe_rtl:mk_load_address(MatchBuf, erts_save_mb, c_const),
	     hipe_rtl:mk_alu(MatchBuf, MatchBuf, add, hipe_rtl:mk_imm(Index * 16)), 
	     save_matchbuffer(MatchBuf, BinSize, Base, Offset, Orig),
	     hipe_rtl:mk_goto(TrueLblName)];

	  bs_final ->
	    [Base, Offset] = Args,
	    case Dst of
	      [DstVar] ->
		hipe_tagscheme:finalize_bin(DstVar, Base, Offset, TrueLblName); 
	      [] ->
		[hipe_rtl:mk_goto(TrueLblName)]
	    end;

	  _ -> 
	    ?EXIT({unhandled_bs_primop, BsOP})
	      end,
      {Code, ConstTab}
  end.







%% ____________________________________________________________________
%% Help functions for calling c fuctions with wrapping and to produce inlined code
%% The following functions are called from the translation switch:
%% 
%% gen_bs_call/10 Are used to call c-functions that return an Erlang value it also makes
%% sure to update the offset before calling and to reload the matchbuffer after the call.
%%
%% gen_test_sideffect_bs_call/4 is used to make a c-call that might fail but doesn't return
%% an erlang value
%%
%% gen_make_size/4 a function that creates an inlined multiplication of a positive immediate and
%% a register. If the result is not a fixnum or the register doesn't contain a positive fixnum it fails.
%%
%% get_64float_from_bin/7 creates code to extract a float of size 64 from a byte-aligned binary.
%%
%% get_int_from_bin/8 creates code to extract a fixed size integer from a byte-aligned binary
%%
%% get_int_from_unaligned_bin/11 creates code to extract a fixed size integer from a binary or
%% makes a c-call if it doesn't conform to some certain rules
%%
%% get_unknown_size_int/11 creates code to extract a variable size byte-aligned integer from a binary or
%% makes a c-call if it doesn't conform to some certain rules
%%
%% skip_no_of_bits/5 creates code to skip a variable amount of bits in a binary
%%
%% load_match_buffer/7 reloads the c-matchbuffer to rtl-registers
%%
%% expand_runtime/4 creates code that calculates a maximal heapneed before a binary match
%%
%% load_bytes/5 creates code to load 1-4 bytes in big or little-endian order
%% in a signed or unsigned way
tagged(Int) ->
  Int*16+15.

gen_bs_call(Name, Args = [SizeReg|_], DstVar, BinSize, Base, Offset, NewOffset, Orig, TrueLblName,FalseLblName) ->
  RetLbl =  hipe_rtl:mk_new_label(),
  OkLbl =  hipe_rtl:mk_new_label(),
  NonVal = hipe_rtl:mk_imm(hipe_tagscheme:mk_non_value()),
  MatchBuf = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_load_address(MatchBuf, erts_mb, c_const)] ++
    save_matchbuffer(MatchBuf, BinSize, Base, Offset, Orig) ++
   [hipe_rtl_arch:call_bif([DstVar], Name, Args,
			  hipe_rtl:label_name(RetLbl), []),
    RetLbl,
    hipe_rtl:mk_branch(DstVar, eq, NonVal,
		       FalseLblName, 
		       hipe_rtl:label_name(OkLbl), 0.01),
    OkLbl,
    hipe_rtl:mk_alu(NewOffset, Offset, add, SizeReg),
    hipe_rtl:mk_goto(TrueLblName)].

gen_test_sideffect_bs_call(Name,Args,TrueLblName,FalseLblName) ->
  Tmp1 = hipe_rtl:mk_new_reg(),
  RetLbl =  hipe_rtl:mk_new_label(),
  [hipe_rtl:mk_call([Tmp1], Name, Args,  c,
		    hipe_rtl:label_name(RetLbl),[]),
   RetLbl,
   hipe_rtl:mk_branch(Tmp1, eq, hipe_rtl:mk_imm(0), 
		      FalseLblName, TrueLblName, 0.01)].

gen_make_size(DstReg, UnitImm, BitsVar, FalseLblName) ->
  UnitList = number2list(UnitImm),
  multiply_code(UnitList, BitsVar, DstReg, FalseLblName).

get_64float_from_bin(BinSize, Base, Offset, NewOffset, Dst1, FalseLblName, TrueLblName, Type) ->
  Tmp2 = hipe_rtl:mk_new_reg(),
  Tmp3 = hipe_rtl:mk_new_reg(), 
  ByteOffset = hipe_rtl:mk_new_reg(),
  SuccessLbl1=hipe_rtl:mk_new_label(),
  Code1 = [check_size(Offset, hipe_rtl:mk_imm(64), BinSize, NewOffset, hipe_rtl:label_name(SuccessLbl1), FalseLblName),
	   SuccessLbl1,
	   hipe_rtl:mk_alu(ByteOffset, Offset, srl, hipe_rtl:mk_imm(3)),
	   load_bytes(Tmp2, Base, ByteOffset, Type, 4),
	   load_bytes(Tmp3, Base, ByteOffset, Type, 4)],
  Code2 = case {Type,get(hipe_target_arch)}  of 
	    {{_, big}, ultrasparc} ->
	      [hipe_tagscheme:unsafe_mk_float(Dst1, Tmp2, Tmp3),
	       hipe_rtl:mk_goto(TrueLblName)];
	    {{_, little}, ultrasparc} ->
	      [hipe_tagscheme:unsafe_mk_float(Dst1, Tmp3, Tmp2),
	       hipe_rtl:mk_goto(TrueLblName)];
	    {{_, little}, x86} ->
	      [hipe_tagscheme:unsafe_mk_float(Dst1, Tmp2, Tmp3),
	       hipe_rtl:mk_goto(TrueLblName)];
	    {{_, big}, x86} ->
	      [hipe_tagscheme:unsafe_mk_float(Dst1, Tmp3, Tmp2),
	       hipe_rtl:mk_goto(TrueLblName)]
	  end,
  Code1 ++ Code2.

get_int_from_bin(Size, BinSize, Base, Offset, NewOffset, Dst1, FalseLblName, TrueLblName, Type) ->
  Shiftr=
    case element(1,Type) of
      unsigned -> srl;
      signed -> sra
    end,
  Tmp1=hipe_rtl:mk_new_reg(),
  Tmp2=hipe_rtl:mk_new_reg(),
  Tmp3=hipe_rtl:mk_new_reg(),
  ByteOffset = hipe_rtl:mk_new_reg(),
  SuccessLbl1=hipe_rtl:mk_new_label(),
  SuccessLbl2=hipe_rtl:mk_new_label(),
  BignumLabel=hipe_rtl:mk_new_label(),
  NewTestLabel=hipe_rtl:mk_new_label(),
  Code1 = [check_size(Offset, hipe_rtl:mk_imm(Size), BinSize, NewOffset,
		      hipe_rtl:label_name(SuccessLbl1), FalseLblName),
	   SuccessLbl1,
	   hipe_rtl:mk_alu(ByteOffset, Offset, srl, hipe_rtl:mk_imm(3)),
	   load_bytes(Tmp3, Base, ByteOffset, Type, ((Size-1) div 8 +1))],
  Code2=
    case Size rem 8 of
      0 ->
	[hipe_rtl:mk_move(Tmp2, Tmp3)];

      _ ->
	[hipe_rtl:mk_alu(Tmp2, Tmp3, Shiftr, hipe_rtl:mk_imm(8-Size rem 8))]
    end,
  Code3=
    case {Size>27, element(1,Type)} of
      {false, _} ->
	[hipe_rtl:mk_alu(Tmp2, Tmp2, sll, hipe_rtl:mk_imm(4)),
	 hipe_rtl:mk_alu(Dst1, Tmp2, 'or', hipe_rtl:mk_imm(15)),
	 hipe_rtl:mk_goto(TrueLblName)];
      {true, signed} ->
	[hipe_rtl:mk_alub(Tmp1, Tmp2, 'and', hipe_rtl:mk_imm(?FIVE_HIGH_BITS), eq, hipe_rtl:label_name(SuccessLbl2), 
			  hipe_rtl:label_name(NewTestLabel)),
	 NewTestLabel,
	 hipe_rtl:mk_branch(Tmp1, eq, hipe_rtl:mk_imm(?FIVE_HIGH_BITS), hipe_rtl:label_name(SuccessLbl2), 
			    hipe_rtl:label_name(BignumLabel)),
	 SuccessLbl2,
	 hipe_rtl:mk_alu(Tmp2, Tmp2, sll, hipe_rtl:mk_imm(4)),
	 hipe_rtl:mk_alu(Dst1, Tmp2, 'or', hipe_rtl:mk_imm(15)),
	 hipe_rtl:mk_goto(TrueLblName),
	 BignumLabel,
	 hipe_tagscheme:unsafe_mk_big(Dst1, Tmp2, signed, get(hipe_target_arch)),
	 hipe_rtl:mk_goto(TrueLblName)];
      {true, unsigned} ->
	[hipe_rtl:mk_alub(Tmp1, Tmp2, 'and', hipe_rtl:mk_imm(?FIVE_HIGH_BITS), eq, hipe_rtl:label_name(SuccessLbl2), 
			  hipe_rtl:label_name(BignumLabel)),
	 SuccessLbl2,
	 hipe_rtl:mk_alu(Tmp2, Tmp2, sll, hipe_rtl:mk_imm(4)),
	 hipe_rtl:mk_alu(Dst1, Tmp2, 'or', hipe_rtl:mk_imm(15)),
	 hipe_rtl:mk_goto(TrueLblName),
	 BignumLabel,
	 hipe_tagscheme:unsafe_mk_big(Dst1, Tmp2, unsigned, get(hipe_target_arch)),
	 hipe_rtl:mk_goto(TrueLblName)]
    end,
  Code1 ++ Code2 ++ Code3.

get_int_from_unaligned_bin(Size, Flag, BinSize, Base, Offset, NewOffset, Orig, Dst1, FalseLblName, TrueLblName, Type)  ->
  Shiftr=
    case element(1,Type) of
      unsigned -> srl;
      signed -> sra
    end,
  ByteOffset = hipe_rtl:mk_new_reg(),
  Tmp1=hipe_rtl:mk_new_reg(),
  Tmp2=hipe_rtl:mk_new_reg(),
  Tmp3=hipe_rtl:mk_new_reg(),
  Tmp4=hipe_rtl:mk_new_reg(),
  SuccessLbl1 = hipe_rtl:mk_new_label(),
  SuccessLbl2 = hipe_rtl:mk_new_label(),
  MoreLbl = hipe_rtl:mk_new_label(),
  LessLbl = hipe_rtl:mk_new_label(),
  JoinLbl = hipe_rtl:mk_new_label(),
  TooBigLbl = hipe_rtl:mk_new_label(),
  Code1 = [hipe_rtl:mk_alu(Tmp1, Offset, 'and', hipe_rtl:mk_imm(7)),
	   hipe_rtl:mk_alu(Tmp2, Tmp1, add, hipe_rtl:mk_imm(Size))],
  Code2 =
    case Size < 26 of
      true ->
	[];
      false ->
	[hipe_rtl:mk_branch(Tmp2, le, hipe_rtl:mk_imm(32), hipe_rtl:label_name(SuccessLbl1), hipe_rtl:label_name(TooBigLbl)),
	 SuccessLbl1]
    end,
  Code3 = [check_size(Offset, hipe_rtl:mk_imm(Size), BinSize, NewOffset, hipe_rtl:label_name(SuccessLbl2), FalseLblName),
	   SuccessLbl2,
	   hipe_rtl:mk_alu(ByteOffset, Offset, srl, hipe_rtl:mk_imm(3))],
  Code4 =
	if Size < 2 ->
	    [load_bytes(Tmp3, Base, ByteOffset, {unsigned,big}, 1),
	     hipe_rtl:mk_alu(Tmp1, Tmp1, add, hipe_rtl:mk_imm(24))] ;
	   Size < 9 ->
	   [ hipe_rtl:mk_branch(Tmp2, le, hipe_rtl:mk_imm(8), hipe_rtl:label_name(LessLbl), hipe_rtl:label_name(MoreLbl)),
	     LessLbl,
	     load_bytes(Tmp3, Base, ByteOffset, {unsigned,big}, 1),
	     hipe_rtl:mk_alu(Tmp1, Tmp1, add, hipe_rtl:mk_imm(24)),
	     hipe_rtl:mk_goto(hipe_rtl:label_name(JoinLbl)),
	     MoreLbl,
	     load_bytes(Tmp3, Base, ByteOffset, {unsigned,big}, 2),
	     hipe_rtl:mk_alu(Tmp1, Tmp1,add, hipe_rtl:mk_imm(16))];
	   Size < 17 ->
	   [ hipe_rtl:mk_branch(Tmp2, le, hipe_rtl:mk_imm(16), hipe_rtl:label_name(LessLbl), hipe_rtl:label_name(MoreLbl)),
	     LessLbl,
	     load_bytes(Tmp3, Base, ByteOffset, {unsigned,big}, 2),
	     hipe_rtl:mk_alu(Tmp1, Tmp1, add, hipe_rtl:mk_imm(16)),
	     hipe_rtl:mk_goto(hipe_rtl:label_name(JoinLbl)),
	     MoreLbl,
	     load_bytes(Tmp3, Base, ByteOffset, {unsigned,big}, 3),
	     hipe_rtl:mk_alu(Tmp1, Tmp1,add, hipe_rtl:mk_imm(8))];
	   true ->
	    [hipe_rtl:mk_branch(Tmp2, le, hipe_rtl:mk_imm(8), hipe_rtl:label_name(LessLbl), hipe_rtl:label_name(MoreLbl)),
	     LessLbl,
	     load_bytes(Tmp3, Base, ByteOffset, {unsigned,big}, 3),
	     hipe_rtl:mk_alu(Tmp1, Tmp1, add, hipe_rtl:mk_imm(8)),
	     hipe_rtl:mk_goto(hipe_rtl:label_name(JoinLbl)),
	     MoreLbl,
	     load_bytes(Tmp3, Base, ByteOffset, {unsigned,big}, 4)]
	end,
  Code5 = [JoinLbl,
	   hipe_rtl:mk_alu(Tmp4, Tmp3, sll, Tmp1),
	   hipe_rtl:mk_alu(Dst1, Tmp4, Shiftr, hipe_rtl:mk_imm(28-Size)),
	   hipe_rtl:mk_alu(Dst1, Dst1, 'or', hipe_rtl:mk_imm(15)),
	   hipe_rtl:mk_goto(TrueLblName)],
  Code6 =
    case Size < 26 of
      true ->
	[];
      false ->
	[TooBigLbl,
	 hipe_rtl:mk_move(Tmp1, hipe_rtl:mk_imm(Size)),
	 hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flag)),
	 gen_bs_call(bs_get_integer,
		     [Tmp1, Tmp2],
		     Dst1, BinSize, Base, Offset, NewOffset, Orig,
		     TrueLblName, FalseLblName)]
    end,
  Code1 ++ Code2 ++ Code3 ++ Code4 ++ Code5 ++ Code6.


get_unknown_size_int(Arg, Size, Flag, BinSize, Base, Offset, NewOffset, Orig, Dst1, FalseLblName, TrueLblName, Type) ->
  
  Shiftr=
    case element(1,Type) of
      unsigned -> srl;
      signed -> sra
    end,
  SuccessLbl = hipe_rtl:mk_new_label(),
  FirstTestLbl = hipe_rtl:mk_new_label(),
  SecondTestLbl = hipe_rtl:mk_new_label(),
  ThirdTestLbl = hipe_rtl:mk_new_label(),
  ThreeByteLbl = hipe_rtl:mk_new_label(),
  HalfwordLbl = hipe_rtl:mk_new_label(),
  ZeroLbl = hipe_rtl:mk_new_label(),
  ByteLbl = hipe_rtl:mk_new_label(),
  JoinLbl = hipe_rtl:mk_new_label(),
  UsecLbl = hipe_rtl:mk_new_label(),
  Tmp1 = hipe_rtl:mk_new_reg(),
  Tmp2 = hipe_rtl:mk_new_reg(),
  Tmp3 = hipe_rtl:mk_new_reg(),
  [gen_make_size(Tmp1, Size, Arg, FalseLblName),
   check_size(Offset, Tmp1, BinSize, Tmp2, hipe_rtl:label_name(SuccessLbl), FalseLblName),
   SuccessLbl,
   hipe_rtl:mk_alu(Tmp3, Offset, srl, hipe_rtl:mk_imm(3)),
   hipe_rtl:mk_branch(Tmp1, le, hipe_rtl:mk_imm(0), hipe_rtl:label_name(ZeroLbl), hipe_rtl:label_name(FirstTestLbl)),
   FirstTestLbl,
   hipe_rtl:mk_branch(Tmp1, le, hipe_rtl:mk_imm(8), hipe_rtl:label_name(ByteLbl), hipe_rtl:label_name(SecondTestLbl)),
   SecondTestLbl,
   hipe_rtl:mk_branch(Tmp1, le, hipe_rtl:mk_imm(16), hipe_rtl:label_name(HalfwordLbl), hipe_rtl:label_name(ThirdTestLbl)),
   ThirdTestLbl,
   hipe_rtl:mk_branch(Tmp1, le, hipe_rtl:mk_imm(24), hipe_rtl:label_name(ThreeByteLbl), hipe_rtl:label_name(UsecLbl)),
   ThreeByteLbl,
   load_bytes(Tmp2, Base, Tmp3, Type, 3),
   hipe_rtl:mk_goto(hipe_rtl:label_name(JoinLbl)),
   HalfwordLbl,
   load_bytes(Tmp2, Base, Tmp3, Type, 2),
   hipe_rtl:mk_goto(hipe_rtl:label_name(JoinLbl)),
   ByteLbl,
   load_bytes(Tmp2, Base, Tmp3, Type, 1),
   JoinLbl,
   hipe_rtl:mk_alu(NewOffset, Offset, add, Tmp1),
   hipe_rtl:mk_alu(Tmp1, Tmp1, 'and', hipe_rtl:mk_imm(7)),
   hipe_rtl:mk_alu(Tmp1, hipe_rtl:mk_imm(8), sub, Tmp1),
   hipe_rtl:mk_alu(Tmp1, Tmp1, 'and', hipe_rtl:mk_imm(7)),
   hipe_rtl:mk_alu(Tmp2, Tmp2, Shiftr, Tmp1),
   hipe_rtl:mk_alu(Tmp2, Tmp2, sll, hipe_rtl:mk_imm(4)),
   hipe_rtl:mk_alu(Dst1, Tmp2, 'or', hipe_rtl:mk_imm(15)),
   hipe_rtl:mk_goto(TrueLblName),
   ZeroLbl,
   hipe_rtl:mk_move(NewOffset, Offset),
   hipe_rtl:mk_move(Dst1, hipe_rtl:mk_imm(15)),
   hipe_rtl:mk_goto(TrueLblName),	    
   UsecLbl,
   hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flag)),
   gen_bs_call(bs_get_integer,
	       [Tmp1, Tmp2],
	       Dst1, BinSize, Base, Offset, NewOffset, Orig,
	       TrueLblName, FalseLblName)].


check_size(Offset, Size, BinSize, Tmp1, ContLblName, FalseLblName) ->
  [hipe_rtl:mk_alu(Tmp1, Offset, add, Size),
   hipe_rtl:mk_branch(Tmp1, le, BinSize, ContLblName, FalseLblName, 0.99)].

load_matchbuffer(MatchBuf, BinSize, Base, Offset, Orig) ->
  [hipe_rtl:mk_load(BinSize, MatchBuf, hipe_rtl:mk_imm(?MB_SIZE)),
   hipe_rtl:mk_load(Base, MatchBuf, hipe_rtl:mk_imm(?MB_BASE)),
   hipe_rtl:mk_load(Offset, MatchBuf, hipe_rtl:mk_imm(?MB_OFFSET)),
   hipe_rtl:mk_load(Orig, MatchBuf, hipe_rtl:mk_imm(?MB_ORIG))].

save_matchbuffer(MatchBuf, BinSize, Base, Offset, Orig) ->
  [hipe_rtl:mk_store(MatchBuf, hipe_rtl:mk_imm(?MB_SIZE), BinSize),
   hipe_rtl:mk_store(MatchBuf, hipe_rtl:mk_imm(?MB_BASE), Base),
   hipe_rtl:mk_store(MatchBuf, hipe_rtl:mk_imm(?MB_OFFSET), Offset),
   hipe_rtl:mk_store(MatchBuf, hipe_rtl:mk_imm(?MB_ORIG), Orig)].

expand_runtime(Shifts, Args, WordReg, Scratch) ->
  expand_runtime(Shifts, Args, WordReg, Scratch, []).

expand_runtime([size|Shifts], [Bin|Args], WordReg, Scratch, Code) ->
  LessThanZeroLbl = hipe_rtl:mk_new_label(), 
  Code1 = Code ++ 
    [get_bin_wordsize(Scratch, Bin, hipe_rtl:label_name(LessThanZeroLbl)),
     hipe_rtl:mk_alu(Scratch, Scratch, add, hipe_rtl:mk_imm(1)),
     hipe_rtl:mk_alu(WordReg, WordReg, add, Scratch),
     LessThanZeroLbl],
  expand_runtime(Shifts, Args, WordReg, Scratch, Code1);

expand_runtime([Shift|Shifts], [Arg|Args], WordReg, Scratch, Code) ->
  LessThanZeroLbl = hipe_rtl:mk_new_label(),
  ContLbl = hipe_rtl:mk_new_label(),
  Code1 = Code ++ 
    [hipe_tagscheme:untag_fixnum(Scratch, Arg),
     hipe_rtl:mk_branch(Scratch, ge, hipe_rtl:mk_imm(0), hipe_rtl:label_name(ContLbl), hipe_rtl:label_name(LessThanZeroLbl), 0.99),
     ContLbl,
     hipe_rtl:mk_alu(Scratch, Scratch, sll, hipe_rtl:mk_imm(Shift)),
     hipe_rtl:mk_alu(Scratch, Scratch, 'add', hipe_rtl:mk_imm(31)),
     hipe_rtl:mk_alu(Scratch, Scratch, srl, hipe_rtl:mk_imm(4)),
     hipe_rtl:mk_alu(WordReg, WordReg, add, Scratch),
     LessThanZeroLbl],
  expand_runtime(Shifts, Args, WordReg, Scratch, Code1);

expand_runtime([], [], _WordReg, _Scratch, Code) ->
  Code.

skip_no_of_bits(SizeOfBin, Offset, NewOffset, NoOfBits, TrueLblName, FalseLblName) ->
  SuccessLbl = hipe_rtl:mk_new_label(),
  [hipe_rtl:mk_alub(NewOffset, NoOfBits, add, Offset, overflow, FalseLblName, hipe_rtl:label_name(SuccessLbl),0.01),
   SuccessLbl,
   hipe_rtl:mk_branch(SizeOfBin, lt, NewOffset, FalseLblName, TrueLblName, 0.01)].

multiply_code(List=[Head|_Tail], Variable, Result, FalseLblName) ->
  Test = set_high(Head),
  Tmp1 = hipe_rtl:mk_new_reg(),
  SuccessLbl1 = hipe_rtl:mk_new_label(),
  SuccessLbl2 = hipe_rtl:mk_new_label(),
  SuccessLbl3 = hipe_rtl:mk_new_label(),
  Register = hipe_rtl:mk_new_reg(),
  Code =[hipe_rtl:mk_move(Result, hipe_rtl:mk_imm(0)),
	 hipe_tagscheme:test_fixnum(Variable, hipe_rtl:label_name(SuccessLbl1), FalseLblName, 0.99),
	 SuccessLbl1,
	 hipe_tagscheme:fixnum_ge(Variable, hipe_rtl:mk_imm(15), hipe_rtl:label_name(SuccessLbl2), FalseLblName, 0.99),
	 SuccessLbl2,
	 hipe_tagscheme:untag_fixnum(Register, Variable),
	 hipe_rtl:mk_alub(Tmp1, Register, 'and', hipe_rtl:mk_imm(Test), eq, hipe_rtl:label_name(SuccessLbl3), FalseLblName, 0.99),
	 SuccessLbl3],
  multiply_code(List, Register, Result, FalseLblName, Tmp1, Code).

multiply_code([ShiftSize| Rest], Register, Result, FalseLblName, Tmp1, OldCode) ->
  SuccessLbl = hipe_rtl:mk_new_label(),
  Code = OldCode ++ [hipe_rtl:mk_alu(Tmp1, Register, sll, hipe_rtl:mk_imm(ShiftSize)),
		     hipe_rtl:mk_alub(Result, Tmp1, 'add', Result, not_overflow, hipe_rtl:label_name(SuccessLbl), FalseLblName, 0.99),
		     SuccessLbl],
  multiply_code(Rest, Register, Result, FalseLblName, Tmp1, Code);

multiply_code([], _Register, _Result, _FalseLblName, _Tmp1, Code) ->
  Code.

number2list(X) when is_integer(X), X>=0  ->
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

load_bytes(Dst, Base, Offset, {Signedness, _Endianess},1) ->
  [hipe_rtl:mk_load(Dst, Base, Offset, byte, Signedness),
   hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1))];



load_bytes(Dst, Base, Offset, {Signedness, Endianess},2) ->
  Tmp1=hipe_rtl:mk_new_reg(),
  case Endianess of
    big ->

      [hipe_rtl:mk_load(Dst, Base, Offset, byte, Signedness),
       hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1)),
       hipe_rtl:mk_alu(Dst, Dst, sll, hipe_rtl:mk_imm(8)),
       hipe_rtl:mk_load(Tmp1, Base, Offset, byte, unsigned),
       hipe_rtl:mk_alu(Dst, Dst, 'or', Tmp1),
       hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1))];
    little ->
      case get(hipe_target_arch) of
	ultrasparc ->
	  [hipe_rtl:mk_load(Dst, Base, Offset, byte, unsigned),
	   hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1)),
	   hipe_rtl:mk_load(Tmp1, Base, Offset, byte,Signedness),
	   hipe_rtl:mk_alu(Tmp1, Tmp1, sll, hipe_rtl:mk_imm(8)),
	   hipe_rtl:mk_alu(Dst, Dst, 'or', Tmp1),
	   hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1))];
	x86 ->
	  [hipe_rtl:mk_load(Dst, Base, Offset, halfword, Signedness),
	   hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(2))]
      end
  end;

load_bytes(Dst, Base, Offset, {Signedness, Endianess},3) ->

  Tmp1=hipe_rtl:mk_new_reg(),
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

  Tmp1=hipe_rtl:mk_new_reg(),
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
       hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1)),
       hipe_rtl:mk_alu(Dst, Dst, sll, hipe_rtl:mk_imm(8)),
       hipe_rtl:mk_load(Tmp1, Base, Offset, byte, unsigned),
       hipe_rtl:mk_alu(Dst, Dst, 'or', Tmp1),
       hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1))];
    
    little ->    
      case get(hipe_target_arch) of
	ultrasparc ->
	  [hipe_rtl:mk_load(Dst, Base, Offset, byte,  unsigned),
	   hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1)),
	   hipe_rtl:mk_load(Tmp1, Base, Offset, byte, unsigned),
	   hipe_rtl:mk_alu(Tmp1, Tmp1, sll, hipe_rtl:mk_imm(8)),
	   hipe_rtl:mk_alu(Dst, Dst, 'or', Tmp1),
	   hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1)),
	   hipe_rtl:mk_load(Tmp1, Base, Offset, byte, unsigned),
	   hipe_rtl:mk_alu(Tmp1, Tmp1, sll, hipe_rtl:mk_imm(16)),
	   hipe_rtl:mk_alu(Dst, Dst, 'or', Tmp1),
	   hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1)),
	   hipe_rtl:mk_load(Tmp1, Base, Offset, byte, Signedness),
	   hipe_rtl:mk_alu(Tmp1, Tmp1, sll, hipe_rtl:mk_imm(24)),
	   hipe_rtl:mk_alu(Dst, Dst, 'or', Tmp1),
	   hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(1))];
	x86 ->
	  [hipe_rtl:mk_load(Dst, Base, Offset, word, Signedness),
	   hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(4))]  
      end
  end.

get_binsize(BinSize, Binary, FalseLblName) ->
  SuccessLbl=hipe_rtl:mk_new_label(),
  BinHeader=hipe_rtl:mk_new_reg(),
  [hipe_tagscheme:test_binary(Binary, hipe_rtl:label_name(SuccessLbl), FalseLblName, 0.99),
   SuccessLbl,
   hipe_rtl:mk_alu(BinHeader, Binary, sub, hipe_rtl:mk_imm(2)),
   hipe_rtl:mk_load(BinSize, BinHeader, hipe_rtl:mk_imm(?SIZE_OFFSET)),
   hipe_rtl:mk_alu(BinSize, BinSize, sll, hipe_rtl:mk_imm(?BYTE_SHIFT))].

get_bin_wordsize(BinSize, Binary, FalseLblName) ->
  SuccessLbl=hipe_rtl:mk_new_label(),
  BinHeader=hipe_rtl:mk_new_reg(),
  [hipe_tagscheme:test_binary(Binary, hipe_rtl:label_name(SuccessLbl), FalseLblName, 0.99),
   SuccessLbl,
   hipe_rtl:mk_alu(BinHeader, Binary, sub, hipe_rtl:mk_imm(2)),
   hipe_rtl:mk_load(BinSize, BinHeader, hipe_rtl:mk_imm(?SIZE_OFFSET)),
   hipe_rtl:mk_alu(BinSize, BinSize, srl, hipe_rtl:mk_imm(2))].
get_binary_bytes(Binary, BinSize, Base, Offset, Orig, OrigOffset, TrueLblName, FalseLblName) ->
  SuccessLbl=hipe_rtl:mk_new_label(),
  SubLbl=hipe_rtl:mk_new_label(),
  OtherLbl=hipe_rtl:mk_new_label(),
  HeapLbl=hipe_rtl:mk_new_label(),
  REFCLbl=hipe_rtl:mk_new_label(),
  SubHeapLbl=hipe_rtl:mk_new_label(),
  SubREFCLbl=hipe_rtl:mk_new_label(),
  SubJoinLbl=hipe_rtl:mk_new_label(),
  [hipe_tagscheme:test_binary(Binary, hipe_rtl:label_name(SuccessLbl), FalseLblName, 0.99),
   SuccessLbl,
   hipe_rtl:mk_load(BinSize, Binary, hipe_rtl:mk_imm(?SIZE_OFFSET-2)),
   hipe_rtl:mk_alu(BinSize, BinSize, sll, hipe_rtl:mk_imm(?BYTE_SHIFT)),
   hipe_rtl:mk_move(Offset, hipe_rtl:mk_imm(0)),
   hipe_tagscheme:test_subbinary(Binary, hipe_rtl:label_name(SubLbl), hipe_rtl:label_name(OtherLbl)),
   SubLbl,
   hipe_rtl:mk_load(OrigOffset, Binary, hipe_rtl:mk_imm(?SUB_OFFSET_OFFSET-2)),
   hipe_rtl:mk_load(Orig, Binary, hipe_rtl:mk_imm(?SUB_ORIG_OFFSET-2)),
   hipe_tagscheme:test_heap_binary(Orig, hipe_rtl:label_name(SubHeapLbl), hipe_rtl:label_name(SubREFCLbl)),
   SubHeapLbl,
   hipe_rtl:mk_alu(Base, Orig, add, hipe_rtl:mk_imm(?HEAP_DATA_OFFSET-2)),
   hipe_rtl:mk_goto(hipe_rtl:label_name(SubJoinLbl)),
   SubREFCLbl,
   hipe_rtl:mk_load(Base, Orig, hipe_rtl:mk_imm(?REFC_POINTER_DATA_OFFSET-2)),
   SubJoinLbl,
   hipe_rtl:mk_alu(Base, Base, add, OrigOffset),
   hipe_rtl:mk_goto(TrueLblName),
   OtherLbl,
   hipe_rtl:mk_move(OrigOffset, hipe_rtl:mk_imm(0)),
   hipe_rtl:mk_move(Orig, Binary),
   hipe_tagscheme:test_heap_binary(Binary, hipe_rtl:label_name(HeapLbl), hipe_rtl:label_name(REFCLbl)),
   HeapLbl,
   hipe_rtl:mk_alu(Base, Binary, add, hipe_rtl:mk_imm(?HEAP_DATA_OFFSET-2)),
   hipe_rtl:mk_goto(TrueLblName),
   REFCLbl,
   hipe_rtl:mk_load(Base, Binary, hipe_rtl:mk_imm(?REFC_POINTER_DATA_OFFSET-2)),
   hipe_rtl:mk_goto(TrueLblName)].
 
copy_aligned_static_bytes(CopyBase, CopyOffset, CopySize, Base, Offset, NewOffset, Size, TrueLblName, FalseLblName) ->
  BaseDst = hipe_rtl:mk_new_reg(),
  BaseSrc = hipe_rtl:mk_new_reg(),
  BothOffset = hipe_rtl:mk_new_reg(),
  Iter = hipe_rtl:mk_imm(Size div 8),

  small_check(hipe_rtl:mk_imm(Size), CopySize, FalseLblName) ++
    initializations(BaseSrc, BaseDst, BothOffset, CopyOffset, Offset, CopyBase, Base) ++
    [hipe_rtl:mk_alu(NewOffset, Offset, 'add', hipe_rtl:mk_imm(Size))] ++
    easy_loop(BaseSrc, BaseDst, BothOffset, Iter, TrueLblName).

copy_aligned_bytes(CopyBase, CopyOffset, CopySize, Base, Offset, NewOffset, Size, TrueLblName, FalseLblName) ->
  
  BaseDst = hipe_rtl:mk_new_reg(),
  BaseSrc = hipe_rtl:mk_new_reg(),
  Iter = hipe_rtl:mk_new_reg(),
  BothOffset = hipe_rtl:mk_new_reg(),
  ContLbl = hipe_rtl:mk_new_label(), 
 
  big_check(Size, CopySize, FalseLblName) ++
    initializations(BaseSrc, BaseDst, BothOffset, CopyOffset, Offset, CopyBase, Base) ++
    [hipe_rtl:mk_alu(Iter, Size, srl, hipe_rtl:mk_imm(3)), 
     hipe_rtl:mk_branch(BothOffset, ne, Iter, hipe_rtl:label_name(ContLbl), TrueLblName, 0.99),
     ContLbl,
     hipe_rtl:mk_alu(NewOffset, Offset, 'add', Size)] ++
    easy_loop(BaseSrc, BaseDst, BothOffset, Iter, TrueLblName).

copy_aligned_bytes_all(CopyBase, CopyOffset, CopySize, Base, Offset, NewOffset, TrueLblName) ->
  Tmp1 = hipe_rtl:mk_new_reg(),
  BaseDst = hipe_rtl:mk_new_reg(),
  BaseSrc = hipe_rtl:mk_new_reg(),
  Iter = hipe_rtl:mk_new_reg(),
  BothOffset = hipe_rtl:mk_new_reg(),
  LoopLbl = hipe_rtl:mk_new_label(), 
    initializations(BaseSrc, BaseDst, BothOffset, CopyOffset, Offset, CopyBase, Base) ++
    [hipe_rtl:mk_alu(Iter, CopySize, srl, hipe_rtl:mk_imm(3)), 
     hipe_rtl:mk_alu(NewOffset, Offset, 'add', CopySize),
     hipe_rtl:mk_branch(BothOffset, ne, Iter, hipe_rtl:label_name(LoopLbl), TrueLblName, 0.99),
     LoopLbl,
     hipe_rtl:mk_load(Tmp1, BaseSrc, BothOffset, byte, unsigned),
     hipe_rtl:mk_store(BaseDst, BothOffset, Tmp1, byte),
     hipe_rtl:mk_alu(BothOffset, BothOffset, add, hipe_rtl:mk_imm(1)),
     hipe_rtl:mk_branch(BothOffset, ne, Iter, hipe_rtl:label_name(LoopLbl), TrueLblName, 0.99)].


copy_static_bytes(CopyBase, CopyOffset, CopySize, Base, Offset, NewOffset, Size, TrueLblName, FalseLblName) ->
  
  InitOffs = hipe_rtl:mk_new_reg(),
  BaseDst = hipe_rtl:mk_new_reg(),
  BaseSrc = hipe_rtl:mk_new_reg(),
  BothOffset = hipe_rtl:mk_new_reg(),
  EasyLbl = hipe_rtl:mk_new_label(),
  HardLbl = hipe_rtl:mk_new_label(),
  Iter = hipe_rtl:mk_imm(Size div 8),
  
  small_check(hipe_rtl:mk_imm(Size div 8), CopySize, FalseLblName) ++
    initializations(BaseSrc, BaseDst, BothOffset, CopyOffset, Offset, CopyBase, Base) ++
    [hipe_rtl:mk_alub(InitOffs, Offset, 'and', hipe_rtl:mk_imm(7), eq, hipe_rtl:label_name(EasyLbl), hipe_rtl:label_name(HardLbl)),
     EasyLbl,
     hipe_rtl:mk_alu(NewOffset, Offset, 'add', hipe_rtl:mk_imm(Size))] ++
    easy_loop(BaseSrc, BaseDst, BothOffset, Iter, TrueLblName) ++
    [HardLbl,
     hipe_rtl:mk_alu(NewOffset, Offset, 'add', hipe_rtl:mk_imm(Size))] ++
    hard_loop(BaseSrc, BaseDst, BothOffset, Iter, InitOffs, TrueLblName).
 

copy_bytes(CopyBase, CopyOffset, CopySize, Base, Offset, NewOffset, Size, TrueLblName, FalseLblName) ->
  
  InitOffs = hipe_rtl:mk_new_reg(),
  Iter = hipe_rtl:mk_new_reg(),
  BaseSrc = hipe_rtl:mk_new_reg(),
  BaseDst = hipe_rtl:mk_new_reg(),
  BothOffset = hipe_rtl:mk_new_reg(),
 
  ContLbl = hipe_rtl:mk_new_label(),
  EasyLbl = hipe_rtl:mk_new_label(),
  HardLbl = hipe_rtl:mk_new_label(),
  ZeroLbl = hipe_rtl:mk_new_label(),
 
  big_check(Size, CopySize, FalseLblName) ++
    initializations(BaseSrc, BaseDst, BothOffset, CopyOffset, Offset, CopyBase, Base) ++
    [hipe_rtl:mk_alu(Iter, Size, srl, hipe_rtl:mk_imm(3)), 
     hipe_rtl:mk_branch(BothOffset, ne, Iter, hipe_rtl:label_name(ContLbl), hipe_rtl:label_name(ZeroLbl), 0.99),
     ContLbl,
     hipe_rtl:mk_alub(InitOffs, Offset, 'and', hipe_rtl:mk_imm(7), eq, hipe_rtl:label_name(EasyLbl), hipe_rtl:label_name(HardLbl)),
     EasyLbl,
     hipe_rtl:mk_alu(NewOffset, Offset, 'add', Size)] ++
    easy_loop(BaseSrc, BaseDst, BothOffset, Iter, TrueLblName) ++
    [HardLbl,
     hipe_rtl:mk_alu(NewOffset, Offset, 'add', Size)] ++
    hard_loop(BaseSrc, BaseDst, BothOffset, Iter, InitOffs, TrueLblName) ++
    [ZeroLbl,
     hipe_rtl:mk_move(NewOffset, Offset),
     hipe_rtl:mk_goto(TrueLblName)].

copy_string(StringBase, StringSize, BinBase, BinOffset, NewOffset, TrueLblName) ->
  TmpOffset = hipe_rtl:mk_new_reg(),
  BothOffset = hipe_rtl:mk_new_reg(),
  NewBinBase = hipe_rtl:mk_new_reg(),
  InitOffs = hipe_rtl:mk_new_reg(),
  EasyLbl = hipe_rtl:mk_new_label(),
  HardLbl = hipe_rtl:mk_new_label(),
  [hipe_rtl:mk_alu(TmpOffset, BinOffset, srl, hipe_rtl:mk_imm(3)),
   hipe_rtl:mk_alu(NewBinBase, BinBase, add, TmpOffset),
   hipe_rtl:mk_move(BothOffset, hipe_rtl:mk_imm(0)),
   hipe_rtl:mk_alub(InitOffs, BinOffset, 'and', hipe_rtl:mk_imm(7), eq, hipe_rtl:label_name(EasyLbl), hipe_rtl:label_name(HardLbl)),
   EasyLbl,
   hipe_rtl:mk_alu(NewOffset, BinOffset, add, hipe_rtl:mk_imm(StringSize*8))] ++
   easy_loop(StringBase, NewBinBase, BothOffset, hipe_rtl:mk_imm(StringSize), TrueLblName) ++
    [HardLbl,
     hipe_rtl:mk_alu(NewOffset, BinOffset, add, hipe_rtl:mk_imm(StringSize*8))] ++
    hard_loop(StringBase, NewBinBase, BothOffset, hipe_rtl:mk_imm(StringSize), InitOffs, TrueLblName).

small_check(SizeVar, CopySize, FalseLblName) ->
  SuccessLbl = hipe_rtl:mk_new_label(),
  [hipe_rtl:mk_branch(SizeVar, le, CopySize, hipe_rtl:label_name(SuccessLbl), FalseLblName),
   SuccessLbl].

big_check(SizeVar, CopySize, FalseLblName) ->
  SuccessLbl = hipe_rtl:mk_new_label(),
  Tmp1 = hipe_rtl:mk_new_reg(),
  small_check(SizeVar, CopySize, FalseLblName) ++
    [hipe_rtl:mk_alub(Tmp1, SizeVar, 'and', hipe_rtl:mk_imm(7), eq, hipe_rtl:label_name(SuccessLbl), FalseLblName),
     SuccessLbl].

easy_loop(BaseSrc, BaseDst, BothOffset, Iterations, TrueLblName) -> 
  Tmp1 = hipe_rtl:mk_new_reg(),
  LoopLbl = hipe_rtl:mk_new_label(),
  [LoopLbl,
   hipe_rtl:mk_load(Tmp1, BaseSrc, BothOffset, byte, unsigned),
   hipe_rtl:mk_store(BaseDst, BothOffset, Tmp1, byte),
   hipe_rtl:mk_alu(BothOffset, BothOffset, add, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_branch(BothOffset, ne, Iterations, hipe_rtl:label_name(LoopLbl), TrueLblName, 0.99)].

% easy_neg_loop(BaseSrc, BaseDst, SrcOffset, DstOffset, Iterations, TrueLblName) -> 
%   Tmp1 = hipe_rtl:mk_new_reg(),
%   Test=hipe_rtl:mk_new_reg(),
%   LoopLbl = hipe_rtl:mk_new_label(),
%   [hipe_rtl:mk_alu(Test, DstOffset, add, Iterations),
%    LoopLbl,
%    hipe_rtl:mk_load(Tmp1, BaseSrc, SrcOffset, byte, unsigned),
%    hipe_rtl:mk_store(BaseDst, DstOffset, Tmp1, byte),
%    hipe_rtl:mk_alu(DstOffset, DstOffset, add, hipe_rtl:mk_imm(1)),
%    hipe_rtl:mk_alu(SrcOffset, SrcOffset, sub, hipe_rtl:mk_imm(1)),
%    hipe_rtl:mk_branch(DstOffset, ne, Test, hipe_rtl:label_name(LoopLbl), TrueLblName, 0.99)].

hard_loop(BaseSrc, BaseDst, BothOffset, Iterations, InitOffset, TrueLblName) ->
  Tmp1 = hipe_rtl:mk_new_reg(),
  Tmp2 = hipe_rtl:mk_new_reg(),
  OldByte = hipe_rtl:mk_new_reg(),
  NewByte = hipe_rtl:mk_new_reg(),
  SaveByte = hipe_rtl:mk_new_reg(),
  LoopLbl = hipe_rtl:mk_new_label(),
  EndLbl = hipe_rtl:mk_new_label(),
  [hipe_rtl:mk_load(OldByte, BaseDst, BothOffset, byte, unsigned),
   hipe_rtl:mk_alu(Tmp1, hipe_rtl:mk_imm(8), sub, InitOffset), 
   LoopLbl, 
   hipe_rtl:mk_load(NewByte, BaseSrc, BothOffset, byte, unsigned),
   hipe_rtl:mk_alu(Tmp2, NewByte, srl, InitOffset),
   hipe_rtl:mk_alu(SaveByte, OldByte, 'or', Tmp2),
   hipe_rtl:mk_store(BaseDst, BothOffset, SaveByte, byte),
   hipe_rtl:mk_alu(OldByte, NewByte, sll, Tmp1),
   hipe_rtl:mk_alu(BothOffset, BothOffset, 'add', hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_branch(BothOffset, ne, Iterations, hipe_rtl:label_name(LoopLbl), hipe_rtl:label_name(EndLbl)),
   EndLbl,
   hipe_rtl:mk_store(BaseDst, BothOffset, OldByte, byte),
   hipe_rtl:mk_goto(TrueLblName)].

% hard_neg_loop(BaseSrc, BaseDst, SrcOffset, DstOffset, Iterations, InitOffset, TrueLblName) ->
%   Tmp1 = hipe_rtl:mk_new_reg(),
%   Tmp2 = hipe_rtl:mk_new_reg(),
%   Test = hipe_rtl:mk_new_reg(),
%   OldByte = hipe_rtl:mk_new_reg(),
%   NewByte = hipe_rtl:mk_new_reg(),
%   SaveByte = hipe_rtl:mk_new_reg(),
%   LoopLbl = hipe_rtl:mk_new_label(),
%   EndLbl = hipe_rtl:mk_new_label(),
%   [hipe_rtl:mk_load(OldByte, BaseDst, DstOffset, byte, unsigned),
%    hipe_rtl:mk_alu(Test, DstOffset, add, Iterations),
%    hipe_rtl:mk_alu(Tmp1, hipe_rtl:mk_imm(8), sub, InitOffset), 
%    LoopLbl, 
%    hipe_rtl:mk_load(NewByte, BaseSrc, SrcOffset, byte, unsigned),
%    hipe_rtl:mk_alu(Tmp2, NewByte, srl, InitOffset),
%    hipe_rtl:mk_alu(SaveByte, OldByte, 'or', Tmp2),
%    hipe_rtl:mk_store(BaseDst, DstOffset, SaveByte, byte),
%    hipe_rtl:mk_alu(OldByte, NewByte, sll, Tmp1),
%    hipe_rtl:mk_alu(DstOffset, DstOffset, 'add', hipe_rtl:mk_imm(1)),
%    hipe_rtl:mk_alu(SrcOffset, SrcOffset, 'sub', hipe_rtl:mk_imm(1)),
%    hipe_rtl:mk_branch(DstOffset, ne, Test, hipe_rtl:label_name(LoopLbl), hipe_rtl:label_name(EndLbl)),
%    EndLbl,
%    hipe_rtl:mk_store(BaseDst, DstOffset, OldByte, byte),
%    hipe_rtl:mk_goto(TrueLblName)].

initializations(BaseTmp1, BaseTmp2, BothOffset, CopyOffset, Offset, CopyBase, Base) ->
  OffsetTmp1 = hipe_rtl:mk_new_reg(),
  OffsetTmp2 = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_alu(OffsetTmp1, CopyOffset, srl, hipe_rtl:mk_imm(3)),
   hipe_rtl:mk_alu(OffsetTmp2, Offset, srl, hipe_rtl:mk_imm(3)),
   hipe_rtl:mk_alu(BaseTmp1, CopyBase, add, OffsetTmp1),
   hipe_rtl:mk_alu(BaseTmp2, Base, add, OffsetTmp2),
   hipe_rtl:mk_move(BothOffset, hipe_rtl:mk_imm(0))].

copy_int_little(Base, Offset, NewOffset, Size, Tmp1) when integer(Size) ->
  Tmp2 = hipe_rtl:mk_new_reg(),
  TmpOffset = hipe_rtl:mk_new_reg(),
  ByteSize = Size div 8, 
    [hipe_rtl:mk_alu(TmpOffset, Offset, srl, hipe_rtl:mk_imm(3)),
     hipe_rtl:mk_alu(Tmp2, hipe_rtl:mk_imm(ByteSize), 'add', TmpOffset)] ++
    
    little_loop(Tmp1, Tmp2, TmpOffset, Base) ++
    
    case Size band 7 of
      0 ->
	[hipe_rtl:mk_alu(NewOffset, Offset, 'add', hipe_rtl:mk_imm(Size))];
      Bits ->
	[hipe_rtl:mk_alu(Tmp1, Tmp1, sll, hipe_rtl:mk_imm(8-Bits)),
	 hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
	 hipe_rtl:mk_alu(NewOffset, Offset, 'add', hipe_rtl:mk_imm(Size))]
    end;
    
copy_int_little(Base, Offset, NewOffset, Size, Tmp1) ->
  Tmp2 = hipe_rtl:mk_new_reg(),
  Tmp3 = hipe_rtl:mk_new_reg(),
  Tmp4 = hipe_rtl:mk_new_reg(),
  TmpOffset = hipe_rtl:mk_new_reg(), 
  
    [hipe_rtl:mk_alu(Tmp2, Size, srl, hipe_rtl:mk_imm(3)),
     hipe_rtl:mk_alu(TmpOffset, Offset, srl, hipe_rtl:mk_imm(3)),
     hipe_rtl:mk_alu(Tmp3, Tmp2, 'add', TmpOffset)] ++
    
    little_loop(Tmp1, Tmp3, TmpOffset, Base) ++
  
    [hipe_rtl:mk_alu(Tmp4, Size, 'and', hipe_rtl:mk_imm(7)),
     hipe_rtl:mk_alu(Tmp1, Tmp1, sll, Tmp4),
     hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
     hipe_rtl:mk_alu(NewOffset, Offset, 'add', Size)].

first_step(Src, Tmp1, Args=[Size,_Base,Offset,_Flags], NewOffset, TrueLblName,FalseLblName) ->
  SuccessLbl = hipe_rtl:mk_new_label(),
  BignumLbl = hipe_rtl:mk_new_label(),
  PassedLbl = hipe_rtl:mk_new_label(),
  [hipe_tagscheme:test_fixnum(Src, hipe_rtl:label_name(SuccessLbl), hipe_rtl:label_name(BignumLbl), 0.99),
   BignumLbl,
   gen_test_sideffect_bs_call(bs_put_big_integer,[Src|Args], hipe_rtl:label_name(PassedLbl), FalseLblName),
   PassedLbl,
   hipe_rtl:mk_alu(NewOffset, Offset, add, Size),
   hipe_rtl:mk_goto(TrueLblName),
   SuccessLbl,
   hipe_tagscheme:untag_fixnum(Tmp1,Src)].

little_loop(Tmp1, Tmp3, TmpOffset, Base) ->
  BranchLbl =  hipe_rtl:mk_new_label(),
  BodyLbl = hipe_rtl:mk_new_label(),
  EndLbl = hipe_rtl:mk_new_label(), 
  [BranchLbl,
   hipe_rtl:mk_branch(TmpOffset, ne, Tmp3, hipe_rtl:label_name(BodyLbl), hipe_rtl:label_name(EndLbl)),
   BodyLbl,
   hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
   hipe_rtl:mk_alu(Tmp1, Tmp1, sra, hipe_rtl:mk_imm(8)),
   hipe_rtl:mk_alu(TmpOffset, TmpOffset, 'add', hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_goto(hipe_rtl:label_name(BranchLbl)),
   EndLbl].

big_loop(Tmp1, Tmp3, TmpOffset, Base) ->
  BranchLbl =  hipe_rtl:mk_new_label(),
  BodyLbl = hipe_rtl:mk_new_label(),
  EndLbl = hipe_rtl:mk_new_label(), 
  [BranchLbl,
   hipe_rtl:mk_branch(TmpOffset, ne, Tmp3, hipe_rtl:label_name(BodyLbl), hipe_rtl:label_name(EndLbl)),
   BodyLbl,
   hipe_rtl:mk_alu(TmpOffset, TmpOffset, sub, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
   hipe_rtl:mk_alu(Tmp1, Tmp1, sra, hipe_rtl:mk_imm(8)),
   hipe_rtl:mk_goto(hipe_rtl:label_name(BranchLbl)),
   EndLbl].

copy_int_big(_Base, Offset, NewOffset, 0, _Tmp1) ->
  [hipe_rtl:mk_move(NewOffset, Offset)];
copy_int_big(Base, Offset, NewOffset, 8, Tmp1) ->
  TmpOffset = hipe_rtl:mk_new_reg(),
    [hipe_rtl:mk_alu(TmpOffset, Offset, srl, hipe_rtl:mk_imm(3)),
     hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
     hipe_rtl:mk_alu(NewOffset, Offset, add, hipe_rtl:mk_imm(8))];
copy_int_big(Base, Offset, NewOffset, 16, Tmp1) ->
  TmpOffset = hipe_rtl:mk_new_reg(),
    [hipe_rtl:mk_alu(TmpOffset, Offset, srl, hipe_rtl:mk_imm(3)),
     hipe_rtl:mk_alu(TmpOffset, TmpOffset, add, hipe_rtl:mk_imm(1)),
     hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
     hipe_rtl:mk_alu(TmpOffset, TmpOffset, sub, hipe_rtl:mk_imm(1)),
     hipe_rtl:mk_alu(Tmp1, Tmp1, sra, hipe_rtl:mk_imm(8)),
     hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
     hipe_rtl:mk_alu(NewOffset, Offset, add, hipe_rtl:mk_imm(16))];
copy_int_big(Base, Offset, NewOffset, 24, Tmp1) ->
  TmpOffset = hipe_rtl:mk_new_reg(), 
    [hipe_rtl:mk_alu(TmpOffset, Offset, srl, hipe_rtl:mk_imm(3)),
     hipe_rtl:mk_alu(TmpOffset, TmpOffset, add, hipe_rtl:mk_imm(2)),
     hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
     hipe_rtl:mk_alu(TmpOffset, TmpOffset, sub, hipe_rtl:mk_imm(1)),
     hipe_rtl:mk_alu(Tmp1, Tmp1, sra, hipe_rtl:mk_imm(8)),
     hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
     hipe_rtl:mk_alu(TmpOffset, TmpOffset, sub, hipe_rtl:mk_imm(1)),
     hipe_rtl:mk_alu(Tmp1, Tmp1, sra, hipe_rtl:mk_imm(8)),
     hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
     hipe_rtl:mk_alu(NewOffset, Offset, add, hipe_rtl:mk_imm(24))];
copy_int_big(Base, Offset,NewOffset, 32, Tmp1) ->
    copy_big_word(Base, Offset, NewOffset, Tmp1);



copy_int_big(Base, Offset, NewOffset, Size, Tmp1) when integer(Size) ->
  OldOffset = hipe_rtl:mk_new_reg(),
  TmpOffset = hipe_rtl:mk_new_reg(),
  Bits = hipe_rtl:mk_new_reg(),
  ByteSize = (Size + 7) div 8,
  case Size band 7 of 
    0 -> 
      [hipe_rtl:mk_alu(OldOffset, Offset, sra, hipe_rtl:mk_imm(3)),
       hipe_rtl:mk_alu(TmpOffset, OldOffset, add, hipe_rtl:mk_imm(ByteSize))];
    
    Rest ->
      [hipe_rtl:mk_alu(OldOffset, Offset, sra, hipe_rtl:mk_imm(3)),
       hipe_rtl:mk_alu(TmpOffset, OldOffset, add, hipe_rtl:mk_imm(ByteSize-1)),
       hipe_rtl:mk_alu(Bits, Tmp1, sll, hipe_rtl:mk_imm(8-Rest)),
       hipe_rtl:mk_store(Base, TmpOffset, Bits, byte),
       hipe_rtl:mk_alu(Tmp1, Tmp1, sra, hipe_rtl:mk_imm(Rest))]
  end ++
    big_loop(Tmp1, OldOffset, TmpOffset, Base) ++
    [hipe_rtl:mk_alu(NewOffset, Offset, 'add', hipe_rtl:mk_imm(Size))];  

copy_int_big(Base, Offset, NewOffset, Size, Tmp1) ->
  Tmp2 = hipe_rtl:mk_new_reg(),
  Tmp3 = hipe_rtl:mk_new_reg(),
  Tmp4 = hipe_rtl:mk_new_reg(),
  Tmp5 = hipe_rtl:mk_new_reg(),
  Tmp6 = hipe_rtl:mk_new_reg(),
  TmpOffset = hipe_rtl:mk_new_reg(),
  
    [hipe_rtl:mk_alu(Tmp2, Size, srl, hipe_rtl:mk_imm(3)),
     hipe_rtl:mk_alu(Tmp3, Offset, srl, hipe_rtl:mk_imm(3)),
     hipe_rtl:mk_alu(TmpOffset, Tmp2, 'add', Tmp3),
     hipe_rtl:mk_alu(Tmp4, Size, 'and', hipe_rtl:mk_imm(7)),
     hipe_rtl:mk_alu(Tmp5, Tmp1, sll, Tmp4),
     hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
     hipe_rtl:mk_alu(Tmp6, Tmp4, sub, hipe_rtl:mk_imm(8)),
     hipe_rtl:mk_alu(Tmp1, Tmp1, srl, Tmp6)] ++
    
    big_loop(Tmp1, Tmp3, TmpOffset, Base) ++
  
    [hipe_rtl:mk_alu(NewOffset, Offset, 'add', Size)].

copy_big_word(Base, Offset, NewOffset, Word) ->
  TmpOffset = hipe_rtl:mk_new_reg(),
    [hipe_rtl:mk_alu(TmpOffset, Offset, srl, hipe_rtl:mk_imm(3)),
     hipe_rtl:mk_alu(TmpOffset, TmpOffset, add, hipe_rtl:mk_imm(3)),
     hipe_rtl:mk_store(Base, TmpOffset, Word, byte),
     hipe_rtl:mk_alu(TmpOffset, TmpOffset, sub, hipe_rtl:mk_imm(1)),
     hipe_rtl:mk_alu(Word, Word, sra, hipe_rtl:mk_imm(8)),
     hipe_rtl:mk_store(Base, TmpOffset, Word, byte),
     hipe_rtl:mk_alu(TmpOffset, TmpOffset, sub, hipe_rtl:mk_imm(1)),
     hipe_rtl:mk_alu(Word, Word, sra, hipe_rtl:mk_imm(8)),
     hipe_rtl:mk_store(Base, TmpOffset, Word, byte),
     hipe_rtl:mk_alu(TmpOffset, TmpOffset, sub, hipe_rtl:mk_imm(1)),
     hipe_rtl:mk_alu(Word, Word, sra, hipe_rtl:mk_imm(8)),
     hipe_rtl:mk_store(Base, TmpOffset, Word, byte),
     hipe_rtl:mk_alu(NewOffset, Offset, add, hipe_rtl:mk_imm(32))].

copy_little_word(Base, Offset, NewOffset, Word) ->
  TmpOffset = hipe_rtl:mk_new_reg(),
    [hipe_rtl:mk_alu(TmpOffset, Offset, srl, hipe_rtl:mk_imm(3)),
     hipe_rtl:mk_store(Base, TmpOffset, Word, byte),
     hipe_rtl:mk_alu(TmpOffset, TmpOffset, add, hipe_rtl:mk_imm(1)),
     hipe_rtl:mk_alu(Word, Word, sra, hipe_rtl:mk_imm(8)),
     hipe_rtl:mk_store(Base, TmpOffset, Word, byte),
     hipe_rtl:mk_alu(TmpOffset, TmpOffset, add, hipe_rtl:mk_imm(1)),
     hipe_rtl:mk_alu(Word, Word, sra, hipe_rtl:mk_imm(8)),
     hipe_rtl:mk_store(Base, TmpOffset, Word, byte),
     hipe_rtl:mk_alu(TmpOffset, TmpOffset, add, hipe_rtl:mk_imm(1)),
     hipe_rtl:mk_alu(Word, Word, sra, hipe_rtl:mk_imm(8)),
     hipe_rtl:mk_store(Base, TmpOffset, Word, byte),
     hipe_rtl:mk_alu(NewOffset, Offset, add, hipe_rtl:mk_imm(32))].

copy_offset_int_big(Base, Offset, NewOffset, Size, Tmp1) when integer(Size) ->
  Tmp2 = hipe_rtl:mk_new_reg(),
  Tmp3 = hipe_rtl:mk_new_reg(),
  Tmp4 = hipe_rtl:mk_new_reg(),
  Tmp5 = hipe_rtl:mk_new_reg(),
  Tmp6 = hipe_rtl:mk_new_reg(),
  Tmp7 = hipe_rtl:mk_new_reg(),
  Tmp8 = hipe_rtl:mk_new_reg(),
  OldByte = hipe_rtl:mk_new_reg(),
  TmpOffset = hipe_rtl:mk_new_reg(),
  BranchLbl =  hipe_rtl:mk_new_label(),
  BodyLbl = hipe_rtl:mk_new_label(),
  EndLbl = hipe_rtl:mk_new_label(),
  NextLbl = hipe_rtl:mk_new_label(),
  [hipe_rtl:mk_alu(Tmp2, Offset, 'and', hipe_rtl:mk_imm(7)),
   hipe_rtl:mk_alu(Tmp3, Offset, srl, hipe_rtl:mk_imm(3)),
   hipe_rtl:mk_alu(NewOffset, Offset, 'add', hipe_rtl:mk_imm(Size)),
   hipe_rtl:mk_alu(TmpOffset, NewOffset, srl, hipe_rtl:mk_imm(3)),
   hipe_rtl:mk_alu(Tmp4, NewOffset, 'and', hipe_rtl:mk_imm(7)),
   hipe_rtl:mk_alu(Tmp6, hipe_rtl:mk_imm(8), sub, Tmp4),
   hipe_rtl:mk_move(Tmp5, Tmp1),
   hipe_rtl:mk_alu(Tmp1, Tmp1, sll, Tmp6), 
   hipe_rtl:mk_branch(TmpOffset, ne, Tmp3, hipe_rtl:label_name(NextLbl), hipe_rtl:label_name(EndLbl)),
   NextLbl,
   hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
   hipe_rtl:mk_move(Tmp1, Tmp5),
   hipe_rtl:mk_alu(Tmp1, Tmp1, sra, Tmp4),
   hipe_rtl:mk_alu(TmpOffset, TmpOffset, sub, hipe_rtl:mk_imm(1)),
   BranchLbl,
   hipe_rtl:mk_branch(TmpOffset, ne, Tmp3, hipe_rtl:label_name(BodyLbl), hipe_rtl:label_name(EndLbl)),
   BodyLbl,
   hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
   hipe_rtl:mk_alu(Tmp1, Tmp1, sra, hipe_rtl:mk_imm(8)),
   hipe_rtl:mk_alu(TmpOffset, TmpOffset, sub, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_goto(hipe_rtl:label_name(BranchLbl)),
   EndLbl,
   hipe_rtl:mk_load(OldByte, Base, TmpOffset, byte, unsigned),
   hipe_rtl:mk_alu(Tmp8, hipe_rtl:mk_imm(8), 'sub', Tmp2),
   hipe_rtl:mk_alu(OldByte, OldByte, srl, Tmp8),
   hipe_rtl:mk_alu(OldByte, OldByte, sll, Tmp8),
   hipe_rtl:mk_alu(Tmp7, Tmp2, 'add', hipe_rtl:mk_imm(24)),
   hipe_rtl:mk_alu(Tmp1, Tmp1, sll, Tmp7),
   hipe_rtl:mk_alu(Tmp1, Tmp1, srl, Tmp7),
   hipe_rtl:mk_alu(Tmp1, Tmp1, 'or', OldByte),
   hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte)].

copy_float_little(Base, Offset, NewOffset, Src, _FalseLblName, TrueLblName, pass) ->
  FloatLo = hipe_rtl:mk_new_reg(),
  FloatHi = hipe_rtl:mk_new_reg(),
  TmpOffset =hipe_rtl:mk_new_reg(),
   hipe_tagscheme:unsafe_load_float(FloatLo, FloatHi, Src) ++
    copy_little_word(Base, Offset, TmpOffset, FloatLo) ++
    copy_little_word(Base, TmpOffset, NewOffset, FloatHi) ++
    [hipe_rtl:mk_goto(TrueLblName)];

copy_float_little(Base, Offset, NewOffset, Src, FalseLblName, TrueLblName, var) ->
  SuccessLbl = hipe_rtl:mk_new_label(),
  hipe_tagscheme:test_flonum(Src, hipe_rtl:label_name(SuccessLbl), FalseLblName, 0.99) ++
    [SuccessLbl|copy_float_little(Base, Offset, NewOffset, Src, FalseLblName, TrueLblName, pass)].

copy_float_big(Base, Offset, NewOffset, Src, _FalseLblName, TrueLblName, pass) ->
  FloatLo = hipe_rtl:mk_new_reg(),
  FloatHi = hipe_rtl:mk_new_reg(),
  TmpOffset =hipe_rtl:mk_new_reg(),
  hipe_tagscheme:unsafe_load_float(FloatLo, FloatHi, Src) ++
    copy_big_word(Base, Offset, TmpOffset, FloatHi) ++
    copy_big_word(Base, TmpOffset, NewOffset, FloatLo) ++
    [hipe_rtl:mk_goto(TrueLblName)];

copy_float_big(Base, Offset, NewOffset, Src, FalseLblName, TrueLblName, var) ->
  SuccessLbl = hipe_rtl:mk_new_label(),
  hipe_tagscheme:test_flonum(Src, hipe_rtl:label_name(SuccessLbl), FalseLblName, 0.99) ++
    [SuccessLbl|copy_float_big(Base, Offset, NewOffset, Src, FalseLblName, TrueLblName, pass)].

% % copy_offset_float_little(Base, Offset, NewOffset, Src, _FalseLblName, TrueLblName, pass) ->
% % FloatBase = hipe_rtl:mk_new_reg(),
% % [hipe_rtl:mk_alu(FloatBase, Src, add, hipe_rtl:mk_imm(2))|
% %  copy_string(FloatBase, 8, Base, Offset, NewOffset, TrueLblName)];
 

% % copy_offset_float_little(Base, Offset, NewOffset, Src, FalseLblName, TrueLblName, var) ->
% %   SuccessLbl = hipe_rtl:mk_new_label(),
% %   hipe_tagscheme:test_flonum(Src, hipe_rtl:label_name(SuccessLbl), FalseLblName, 0.99) ++
% %     [SuccessLbl|copy_offset_float_little(Base, Offset, NewOffset, Src, FalseLblName, TrueLblName, pass)].
 

% % copy_offset_float_big(Base, Offset, NewOffset, Src, _FalseLblName, TrueLblName, pass) ->
% %   SrcOffset = hipe_rtl:mk_new_reg(),
% %   DstOffset = hipe_rtl:mk_new_reg(),
% %   SrcBase = hipe_rtl:mk_new_reg(),
% %   InitOffs = hipe_rtl:mk_new_reg(),
% %   EasyLbl = hipe_rtl:mk_new_label(),
% %   HardLbl = hipe_rtl:mk_new_label(),
% %   [hipe_rtl:mk_alu(DstOffset, Offset, srl, hipe_rtl:mk_imm(3)),
% %    hipe_rtl:mk_alu(SrcBase, Src, 'add', hipe_rtl:mk_imm(2)),
% %    hipe_rtl:mk_move(SrcOffset, hipe_rtl:mk_imm(7)),
% %    hipe_rtl:mk_alub(InitOffs, Offset, 'and', hipe_rtl:mk_imm(7), eq, hipe_rtl:label_name(EasyLbl), hipe_rtl:label_name(HardLbl)),
% %    EasyLbl,
% %    hipe_rtl:mk_alu(NewOffset, Offset, add, hipe_rtl:mk_imm(64))] ++
% %     easy_neg_loop(SrcBase, Base, SrcOffset, DstOffset, hipe_rtl:mk_imm(8), TrueLblName) ++
% %     [HardLbl,
% %      hipe_rtl:mk_alu(NewOffset, Offset, add, hipe_rtl:mk_imm(64))] ++
% %     hard_neg_loop(SrcBase, Base, SrcOffset, DstOffset, hipe_rtl:mk_imm(8), InitOffs, TrueLblName);

% % copy_offset_float_big(Base, Offset, NewOffset, Src, FalseLblName, TrueLblName, var) ->
% %   SuccessLbl = hipe_rtl:mk_new_label(),
% %   hipe_tagscheme:test_flonum(Src, hipe_rtl:label_name(SuccessLbl), FalseLblName, 0.99) ++
% %     [SuccessLbl|copy_offset_float_big(Base, Offset, NewOffset, Src, FalseLblName, TrueLblName, pass)].
 
  
calculate_size(Size, Const, Units, Args, FalseLblName) ->
  VarSize = hipe_rtl:mk_new_reg(),
  AllSize = hipe_rtl:mk_new_reg(),
  Tmp1 = hipe_rtl:mk_new_reg(),
  {Code1, RestArgs} = calculate_var_size(Units, Args, VarSize, FalseLblName), 
  Code2 = calculate_all_size(RestArgs, AllSize, FalseLblName), 
  case {Code1, Code2, Const} of
    {[], [], Const} ->
      [hipe_rtl:mk_move(Size, hipe_rtl:mk_imm(Const))];
    {_, [], 0} ->
      Code1 ++ [hipe_rtl:mk_move(Size, VarSize)]; 
    {_, [], _} ->
      Code1 ++ [hipe_rtl:mk_alu(Size, VarSize, 'add', hipe_rtl:mk_imm(Const))]; 
    {[], _, 0} ->
      Code2 ++ [hipe_rtl:mk_move(Size, AllSize)]; 
    {[], _, _} ->
      Code2 ++ [hipe_rtl:mk_alu(Size, AllSize, 'add', hipe_rtl:mk_imm(Const))]; 
    {_, _, 0} ->
      Code1 ++ Code2 ++ [hipe_rtl:mk_alu(Size, VarSize, 'add', AllSize)]; 
    {_, _, _} ->
      Code1 ++ Code2 ++ [hipe_rtl:mk_alu(Tmp1, VarSize, 'add', AllSize),
			 hipe_rtl:mk_alu(Size, Tmp1, 'add', hipe_rtl:mk_imm(Const))] 
  end.

calculate_var_size([], Args, _VarSize, _FalseLblName) ->
  {[], Args};

calculate_var_size([Unit|Units], [Var|Vars], VarSize, FalseLblName) ->
  ThisSize = hipe_rtl:mk_new_reg(),
  calculate_var_size(Units, Vars, VarSize, ThisSize, gen_make_size(ThisSize, Unit, Var, FalseLblName), FalseLblName).

calculate_var_size([Unit|Units], [Var|Vars], VarSize, LastSize, AccCode, FalseLblName) ->
  ThisSize = hipe_rtl:mk_new_reg(),
  ThisCode = [gen_make_size(ThisSize, Unit, Var, FalseLblName),
	      hipe_rtl:mk_alu(ThisSize, ThisSize, 'add', LastSize)],
  calculate_var_size(Units, Vars, VarSize, ThisSize, AccCode ++ ThisCode, FalseLblName);
calculate_var_size([], Vars, VarSize, LastSize, AccCode, _FalseLblName) ->
  {AccCode ++ [hipe_rtl:mk_move(VarSize, LastSize)], Vars}.

calculate_all_size([], _VarSize, _FalseLblName) ->
  [];

calculate_all_size([Bin|Rest], VarSize, FalseLblName) ->
  ThisSize = hipe_rtl:mk_new_reg(),
  calculate_all_size(Rest, VarSize, ThisSize, get_binsize(ThisSize, Bin, FalseLblName), FalseLblName).

calculate_all_size([Bin|Rest],  VarSize, LastSize, AccCode, FalseLblName) ->
  ThisSize = hipe_rtl:mk_new_reg(),
  ThisCode = [get_binsize(ThisSize, Bin, FalseLblName),
	      hipe_rtl:mk_alu(ThisSize, ThisSize, 'add', LastSize)],
  calculate_all_size(Rest, VarSize, ThisSize, AccCode ++ ThisCode, FalseLblName);
calculate_all_size([], VarSize, LastSize, AccCode, _FalseLblName) ->
  AccCode ++ [hipe_rtl:mk_move(VarSize, LastSize)].


   

