%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%%  Filename : 	hipe_rtl_inline_bs_ops.erl
%%  Module   :	hipe_rtl_inline_bs_ops
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-06-14 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: richardc $
%%              $Date: 2002/10/01 12:44:28 $
%%              $Revision: 1.12 $
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

%% -------------------------------------------------------------------------

%% The code is generated as a list of lists, it will be flattened later.
%% 

gen_rtl(BsOP,Args, Dst,TrueLblName, FalseLblName, ConstTab) ->
  case BsOP of
    {bs_put_string, String, SizeInBytes} ->
      Tmp1 = hipe_rtl:mk_new_reg(),
      Tmp2 = hipe_rtl:mk_new_reg(),
      {NewTab, Lbl} = 
	hipe_consttab:insert_block(ConstTab, 4, byte, String),

      {[hipe_rtl:mk_load_address(Tmp1, Lbl, constant),
	hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(SizeInBytes)),
	gen_test_sideffect_bs_call(bs_put_string,
				   [Tmp1,Tmp2], 
				   TrueLblName,FalseLblName)],
       NewTab};

    _ -> 
      Code = 
	case BsOP of
	  {bs_init, _Size, _Flag} ->
	    [hipe_rtl:mk_call([], bs_init, [], c, TrueLblName, [])];
	  
	  {bs_create_space, Size, Shifts} ->
	    NumberOfWords=hipe_rtl:mk_new_reg(),
	    Scratch = hipe_rtl:mk_new_reg(),
	    [hipe_rtl:mk_move(NumberOfWords, hipe_rtl:mk_imm(Size))]
	      ++ expand_runtime(Shifts, Args, NumberOfWords, Scratch)
	      ++ [hipe_rtl:mk_gctest(NumberOfWords),
		  hipe_rtl:mk_goto(TrueLblName)];
					

	  bs_start_match ->
	    SuccessLbl1=hipe_rtl:mk_new_label(),
	    [MatchBuf, BinSize, Base, Offset, Orig] = Dst,
	    [gen_test_sideffect_bs_call(bs_start_match, Args, hipe_rtl:label_name(SuccessLbl1), FalseLblName),
	     SuccessLbl1,
	     load_matchbuffer(MatchBuf,  BinSize, Base, Offset, Orig, TrueLblName, FalseLblName)];

	  %% {bs_need_buf, Need} -> []; %ignorerar bs_need_buf

	  {bs_put_binary_all, _Flags} ->
	    gen_test_sideffect_bs_call(bs_put_binary_all,Args,
				       TrueLblName,FalseLblName);

	  {bs_put_binary, Size} ->
	    Tmp2 = hipe_rtl:mk_new_reg(),
	    case Args of
	      [Src] -> 
		[hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Size)),
		 gen_test_sideffect_bs_call(bs_put_binary, [Src,Tmp2],
					    TrueLblName,FalseLblName)];
	      [Src, Bits] -> 
		Tmp3 = hipe_rtl:mk_new_reg(),
		gen_make_size(Tmp3, Size, Bits, FalseLblName) ++
		  gen_test_sideffect_bs_call(bs_put_binary, [Src,Tmp3],
					     TrueLblName,FalseLblName)
	    end;

	  {bs_put_float, Size, Flags} ->    
	    SizeReg = hipe_rtl:mk_new_reg(),
	    FlagsReg = hipe_rtl:mk_new_reg(),
	    case Args of
	      [Src] -> 
		[hipe_rtl:mk_move(SizeReg, hipe_rtl:mk_imm(Size)),
		 hipe_rtl:mk_move(FlagsReg, hipe_rtl:mk_imm(Flags)),
		 gen_test_sideffect_bs_call(bs_put_float, 
					    [Src, SizeReg, FlagsReg],
					    TrueLblName,FalseLblName)];
	      [Src, Bits] -> 
		[gen_make_size(SizeReg, Size, Bits, FalseLblName),
		 hipe_rtl:mk_move(FlagsReg, hipe_rtl:mk_imm(Flags)),
		 gen_test_sideffect_bs_call( bs_put_float,
					     [Src,SizeReg,FlagsReg],
					     TrueLblName,FalseLblName)]
	    end;

	  {bs_put_integer, Size, Flags} ->
	    SizeReg = hipe_rtl:mk_new_reg(),
	    FlagsReg = hipe_rtl:mk_new_reg(),
	    case Args of
	      [Src] ->
		[hipe_rtl:mk_move(SizeReg, hipe_rtl:mk_imm(Size)),
		 hipe_rtl:mk_move(FlagsReg, hipe_rtl:mk_imm(Flags)),
		 gen_test_sideffect_bs_call(bs_put_integer,
					    [Src,SizeReg,FlagsReg], 
					    TrueLblName,FalseLblName)];
	      [Src, Bits] -> 
		[gen_make_size(SizeReg, Size, Bits, FalseLblName),
		 hipe_rtl:mk_move(FlagsReg, hipe_rtl:mk_imm(Flags)),
		 gen_test_sideffect_bs_call(bs_put_integer,
					    [Src,SizeReg,FlagsReg], 
					    TrueLblName,FalseLblName)]
	    end;
	  
	  %%Inlined for all possible cases
	  {bs_skip_bits_all, Flags} ->
	    [_MatchBuf, BinSize, _Base, Offset, _Orig]=Args,
	    case (Flags band ?BSF_ALIGNED) of
	      1 -> %% This can't fail
		[hipe_rtl:mk_move(Offset, BinSize),
		 hipe_rtl:mk_goto(TrueLblName)];
	      _ ->
		SuccessLbl=hipe_rtl:mk_new_label(),
		[hipe_rtl:mk_alub(Offset, Offset, 'and', hipe_rtl:mk_imm(2#111), eq, hipe_rtl:label_name(SuccessLbl), FalseLblName, 0.99),
		 SuccessLbl,
		 hipe_rtl:mk_move(Offset, BinSize),
		 hipe_rtl:mk_goto(TrueLblName)]
	    end;

	  %%Inlined for all possible cases
	  {bs_skip_bits, Bits} ->
	    case Args of
	      [_MatchBuf, BinSize, _Base, Offset, _Orig] ->
		skip_no_of_bits(BinSize, Offset, hipe_rtl:mk_imm(Bits), TrueLblName, FalseLblName);

	      [Arg, _MatchBuf, BinSize, _Base, Offset, _Orig] ->
		Tmp1 = hipe_rtl:mk_new_reg(),
		gen_make_size(Tmp1, Bits, Arg, FalseLblName) 
		  ++ skip_no_of_bits(BinSize, Offset, Tmp1, TrueLblName, FalseLblName)
	    end;

	  {bs_get_integer,0,_Flag} ->
	    [Dst1|_State] = Dst,
	    [hipe_rtl:mk_move(Dst1, hipe_rtl:mk_imm(15)),
	     hipe_rtl:mk_goto(TrueLblName)];

	  %%Inlined when Size is const or variable smaller than 28 and the normalendianess 
	  {bs_get_integer,Size,Flag} ->
	    
	    [Dst1|_State] = Dst,
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
	      [MatchBuf, BinSize, Base, Offset, Orig] ->
		case {Size =< 27, Flag band 1, Endianess} of
		  
		  %%Case when size is known and Binary is aligned
		  {true, 1, _} ->                          
		    get_int_from_bin(Size, BinSize, Base, Offset, Dst1, FalseLblName, TrueLblName, Type);
		  
		  %%Case when size is known binary might be aligned
		  {true, 0, big} ->
		    get_int_from_unaligned_bin(Size, Flag, MatchBuf, BinSize, Base, Offset, Orig, Dst1, FalseLblName, TrueLblName, Type);
		
		  _ ->
		    Tmp1 = hipe_rtl:mk_new_reg(),
		    Tmp2 = hipe_rtl:mk_new_reg(),
		    [hipe_rtl:mk_move(Tmp1, hipe_rtl:mk_imm(Size)),
		     hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flag)),
		     gen_bs_call(bs_get_integer,
				 [Tmp1, Tmp2],
				 Dst1, MatchBuf, BinSize, Base, Offset, Orig,
				 TrueLblName, FalseLblName)]
		end;

	      [Arg, MatchBuf, BinSize, Base, Offset, Orig] ->
		case {Flag band 1, Size rem 8, Endianess} of
		  %%Case when size is unknown binary aligned
		  {1, 0, _}  ->
		    get_unknown_size_int(Arg, Size, Flag, MatchBuf, BinSize, Base, Offset, Orig, Dst1, FalseLblName, TrueLblName, Type);
		  {1,_,big} ->
		    get_unknown_size_int(Arg, Size, Flag, MatchBuf, BinSize, Base, Offset, Orig, Dst1, FalseLblName, TrueLblName, Type);
		  _ ->
		    Tmp1 = hipe_rtl:mk_new_reg(),
		    Tmp2 = hipe_rtl:mk_new_reg(),
		    [gen_make_size(Tmp1, Size, Arg, FalseLblName),
		     hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flag)),
		     gen_bs_call(bs_get_integer,
				 [Tmp1, Tmp2],
				 Dst1, MatchBuf, BinSize, Base, Offset, Orig,
				 TrueLblName, FalseLblName)]
		end
	    end;

	  %%Inlined when float size is 64 and binary is byte-aligned
	  {bs_get_float,Size,Flag} ->
	    Tmp1 = hipe_rtl:mk_new_reg(),
	    Tmp2 = hipe_rtl:mk_new_reg(),
	    [Dst1|_State] = Dst,
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
	      [MatchBuf, BinSize, Base, Offset, Orig] ->
		case {Size, Flag band 1} of
		  {64, 1} ->        %inlined for fix size 64 and byte-aligned
		    get_64float_from_bin(BinSize, Base, Offset, Dst1, FalseLblName, TrueLblName, Type);

		  _ ->
		    [hipe_rtl:mk_move(Tmp1, hipe_rtl:mk_imm(Size)),
		     hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flag)),
		     gen_bs_call(bs_get_float,
				 [Tmp1, Tmp2],
				 Dst1, MatchBuf, BinSize, Base, Offset, Orig, 
				 TrueLblName, FalseLblName)]
		end;
	      [Arg, MatchBuf, BinSize, Base, Offset, Orig] ->
		case {Flag band 1} of    
		  1 ->              %inlined for variable size 64 and byte-aligned
		    UsecLbl=hipe_rtl:mk_new_label(),
		    SuccessLbl=hipe_rtl:mk_new_label(),
		    [gen_make_size(Tmp1, Size, Arg, FalseLblName),
		     hipe_rtl:mk_branch(Tmp1, eq, hipe_rtl:mk_imm(64), hipe_rtl:label_name(SuccessLbl), hipe_rtl:label_name(UsecLbl)),
		     SuccessLbl,
		     get_64float_from_bin(BinSize, Base, Offset, Dst1, FalseLblName, TrueLblName, Type),
		     UsecLbl,
		     hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flag)),
		     gen_bs_call(bs_get_float,
				   [Tmp1, Tmp2],
				   Dst1, MatchBuf, BinSize, Base, Offset, Orig,
				   TrueLblName, FalseLblName)];
		  _ ->
		    [gen_make_size(Tmp1, Size, Arg, FalseLblName),
		     hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flag)),
		     gen_bs_call(bs_get_float,
				 [Tmp1, Tmp2],
				 Dst1, MatchBuf, BinSize, Base, Offset, Orig,
				 TrueLblName, FalseLblName)]
		end
	    end;

	  %%Inlined for all cases
	  {bs_get_binary_all, Flags} -> 
	    Tmp1 = hipe_rtl:mk_new_reg(),
	    Tmp2 = hipe_rtl:mk_new_reg(),
	    OkLbl =  hipe_rtl:mk_new_label(),
	    [Dst1| _State] = Dst,
	    [_MatchBuf, BinSize, _Base, Offset, Orig] = Args,
	    Code1 =
	      case Flags band 1 of 
		1 ->
		  [];
		0->
		  [hipe_rtl:mk_alub(Tmp1, Offset, 'and', hipe_rtl:mk_imm(7), eq, hipe_rtl:label_name(OkLbl), FalseLblName, 0.99),
		   OkLbl]
	      end,
	    Code1 ++ 
	      [hipe_rtl:mk_alu(Tmp2, BinSize, sub, Offset),
	       hipe_rtl:mk_alu(Tmp2, Tmp2, srl, hipe_rtl:mk_imm(3)),
	       hipe_rtl:mk_alu(Tmp1, Offset, srl, hipe_rtl:mk_imm(3)),
	       hipe_rtl:mk_move(Offset, BinSize),
	       hipe_tagscheme:unsafe_mk_sub_binary(Dst1, Tmp2, Tmp1, Orig),
	       hipe_rtl:mk_goto(TrueLblName)];

	  %%Inlined when aligned
	  {bs_get_binary,Size,Flag} ->
	    Tmp1 = hipe_rtl:mk_new_reg(),
	    Tmp2 = hipe_rtl:mk_new_reg(),
	    [Dst1|_State] = Dst,
	    Code1=
	      case Args of
		[MatchBuf, BinSize, Base, Offset, Orig] ->
		  [hipe_rtl:mk_move(Tmp1, hipe_rtl:mk_imm(Size))];

		[BitsVar, MatchBuf, BinSize, Base, Offset, Orig] ->
		  gen_make_size(Tmp1, Size, BitsVar, FalseLblName)
	      end,
	    Code2=
	      case Flag of 
		1 ->
		  SuccessLbl1=hipe_rtl:mk_new_label(),
		  SuccessLbl2=hipe_rtl:mk_new_label(),
		  [hipe_rtl:mk_alub(Tmp2, Tmp1, 'and', hipe_rtl:mk_imm(7), eq,hipe_rtl:label_name(SuccessLbl1),FalseLblName, 0.99),  
		   SuccessLbl1,
		   check_size(Offset, Tmp1, BinSize, Tmp2, hipe_rtl:label_name(SuccessLbl2), FalseLblName),
		   SuccessLbl2,
		   hipe_rtl:mk_alu(Offset, Offset, srl, hipe_rtl:mk_imm(3)),
		   hipe_rtl:mk_alu(Tmp1, Tmp1, srl, hipe_rtl:mk_imm(3))]
		    ++ hipe_tagscheme:unsafe_mk_sub_binary(Dst1, Tmp1, Offset, Orig)
		    ++ [hipe_rtl:mk_move(Offset, Tmp2),
			hipe_rtl:mk_goto(TrueLblName)];

		_ ->
		  [hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flag)),
		   gen_bs_call(bs_get_binary,
			       [Tmp1, Tmp2],
			       Dst1, MatchBuf, BinSize, Base, Offset, Orig,
			       TrueLblName, FalseLblName)]
	      end,
	    Code1++Code2;

	  %%Inlined for all cases
	  {bs_test_tail, NumBits} ->
	    [_MatchBuf, BinSize, _Base, Offset, _Orig] = Args,
	    [hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(NumBits)),
	     hipe_rtl:mk_branch(Offset, eq, BinSize, TrueLblName, FalseLblName)];

	  {bs_restore, Index} ->
	    Tmp1 = hipe_rtl:mk_new_reg(),
	    SuccessLbl = hipe_rtl:mk_new_label(),
	    [MatchBuf, BinSize, Base, Offset, Orig] = Args,
	    [hipe_rtl:mk_move(Tmp1, hipe_rtl:mk_imm(Index)),
	     hipe_rtl:mk_call([],bs_restore,[Tmp1],c, hipe_rtl:label_name(SuccessLbl),[]),
	     SuccessLbl,
	     load_matchbuffer(MatchBuf,  BinSize, Base, Offset, Orig, TrueLblName, FalseLblName)];

	  {bs_save, Index} ->
	    Tmp1 = hipe_rtl:mk_new_reg(),
	    SuccessLbl = hipe_rtl:mk_new_label(),
	    [MatchBuf, BinSize, Base, Offset, Orig] = Args,
	    [hipe_rtl:mk_move(Tmp1, hipe_rtl:mk_imm(Index)),
	     hipe_rtl:mk_store(MatchBuf, hipe_rtl:mk_imm(?MB_OFFSET), Offset),
	     hipe_rtl:mk_call([],bs_save,[Tmp1],c, hipe_rtl:label_name(SuccessLbl),[]),
	     SuccessLbl,
	     load_matchbuffer(MatchBuf,  BinSize, Base, Offset, Orig, TrueLblName, FalseLblName)];

	  bs_final ->
	    [hipe_rtl_arch:call_bif(Dst, bs_final, [],
				    TrueLblName, FalseLblName)];

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

gen_bs_call(Name,Args,DstVar,MatchBuf, BinSize, Base, Offset, Orig, TrueLblName,FalseLblName) ->
  RetLbl =  hipe_rtl:mk_new_label(),
  OkLbl =  hipe_rtl:mk_new_label(),
  NonVal = hipe_rtl:mk_imm(hipe_tagscheme:mk_non_value()),
  [hipe_rtl:mk_store(MatchBuf, hipe_rtl:mk_imm(?MB_OFFSET), Offset),
   hipe_rtl_arch:call_bif([DstVar], Name, Args,
			  hipe_rtl:label_name(RetLbl), []),
   RetLbl,
   hipe_rtl:mk_branch(DstVar, eq, NonVal,
		      FalseLblName, 
		      hipe_rtl:label_name(OkLbl), 0.01),
   OkLbl,
   load_matchbuffer(MatchBuf,  BinSize, Base, Offset, Orig, TrueLblName, FalseLblName)].

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

get_64float_from_bin(BinSize, Base, Offset, Dst1, FalseLblName, TrueLblName, Type) ->
  Tmp1 = hipe_rtl:mk_new_reg(),
  Tmp2 = hipe_rtl:mk_new_reg(),
  Tmp3 = hipe_rtl:mk_new_reg(), 
  SuccessLbl1=hipe_rtl:mk_new_label(),
  Code1 = [check_size(Offset, hipe_rtl:mk_imm(64), BinSize, Tmp1, hipe_rtl:label_name(SuccessLbl1), FalseLblName),
	   SuccessLbl1,
	   hipe_rtl:mk_alu(Offset, Offset, srl, hipe_rtl:mk_imm(3)),
	   load_bytes(Tmp2, Base, Offset, Type, 4),
	   load_bytes(Tmp3, Base, Offset, Type, 4),
	   hipe_rtl:mk_move(Offset, Tmp1)],
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

get_int_from_bin(Size, BinSize, Base, Offset, Dst1, FalseLblName, TrueLblName, Type) ->
  Shiftr=
    case element(1,Type) of
      unsigned -> srl;
      signed -> sra
    end,
  Tmp1=hipe_rtl:mk_new_reg(),
  Tmp2=hipe_rtl:mk_new_reg(),
  SuccessLbl=hipe_rtl:mk_new_label(),
  Code1 = [check_size(Offset, hipe_rtl:mk_imm(Size), BinSize, Tmp1, hipe_rtl:label_name(SuccessLbl), FalseLblName),
	   SuccessLbl,
	   hipe_rtl:mk_alu(Offset, Offset, srl, hipe_rtl:mk_imm(3)),
	   load_bytes(Tmp2, Base, Offset, Type, ((Size-1) div 8 +1)),
	   hipe_rtl:mk_move(Offset, Tmp1)],
  Code2=
    case Size rem 8 of
      0 ->
	[hipe_rtl:mk_alu(Tmp2, Tmp2, sll, hipe_rtl:mk_imm(4)),
	 hipe_rtl:mk_alub(Dst1, Tmp2, 'or', hipe_rtl:mk_imm(15), not_overflow, TrueLblName, FalseLblName)];

      _ ->
	[hipe_rtl:mk_alu(Tmp2, Tmp2, Shiftr, hipe_rtl:mk_imm(8-Size rem 8)),
	 hipe_rtl:mk_alu(Tmp2, Tmp2, sll, hipe_rtl:mk_imm(4)),
	 hipe_rtl:mk_alub(Dst1, Tmp2, 'or', hipe_rtl:mk_imm(15), not_overflow, TrueLblName, FalseLblName)]
    end,
  Code1 ++ Code2.

get_int_from_unaligned_bin(Size, Flag, MatchBuf, BinSize, Base, Offset, Orig, Dst1, FalseLblName, TrueLblName, Type)  ->
  Shiftr=
    case element(1,Type) of
      unsigned -> srl;
      signed -> sra
    end,

  Tmp1=hipe_rtl:mk_new_reg(),
  Tmp2=hipe_rtl:mk_new_reg(),
  Tmp3=hipe_rtl:mk_new_reg(),
  SuccessLbl1 = hipe_rtl:mk_new_label(),
  SuccessLbl2 = hipe_rtl:mk_new_label(),
  MoreLbl = hipe_rtl:mk_new_label(),
  JoinLbl = hipe_rtl:mk_new_label(),
  TooBigLbl = hipe_rtl:mk_new_label(),
  Code1 = [hipe_rtl:mk_alu(Tmp1, Offset, 'and', hipe_rtl:mk_imm(7)),
	   hipe_rtl:mk_alu(Tmp2, Tmp1, add, hipe_rtl:mk_imm(Size)),
	   hipe_rtl:mk_branch(Tmp2, le, hipe_rtl:mk_imm(32), hipe_rtl:label_name(SuccessLbl1), hipe_rtl:label_name(TooBigLbl)),
	   SuccessLbl1,
	   check_size(Offset, hipe_rtl:mk_imm(Size), BinSize, Tmp3, hipe_rtl:label_name(SuccessLbl2), FalseLblName),
	   SuccessLbl2,
	   hipe_rtl:mk_alu(Offset, Offset, srl, hipe_rtl:mk_imm(3))],
  Code2 =
	if Size < 2 ->
	    [load_bytes(Dst1, Base, Offset, {unsigned,big}, 1),
	     hipe_rtl:mk_alu(Dst1, Dst1, sll, hipe_rtl:mk_imm(24))] ;
	   Size < 9 ->
	   [load_bytes(Dst1, Base, Offset, {unsigned,big}, 1),
	     hipe_rtl:mk_alu(Dst1, Dst1, sll, hipe_rtl:mk_imm(24)),
	     hipe_rtl:mk_branch(Tmp2, le, hipe_rtl:mk_imm(8), hipe_rtl:label_name(JoinLbl), hipe_rtl:label_name(MoreLbl)), 
	     MoreLbl,
	     load_bytes(Tmp2, Base, Offset, {unsigned,big}, 1),
	     hipe_rtl:mk_alu(Tmp2, Tmp2, sll, hipe_rtl:mk_imm(16)),
	     hipe_rtl:mk_alu(Dst1, Dst1, 'or', Tmp2)];
	   Size < 17 ->
	    [load_bytes(Dst1, Base, Offset, {unsigned,big}, 2),
	     hipe_rtl:mk_alu(Dst1, Dst1, sll, hipe_rtl:mk_imm(16)),
	     hipe_rtl:mk_branch(Tmp2, le, hipe_rtl:mk_imm(16), hipe_rtl:label_name(JoinLbl), hipe_rtl:label_name(MoreLbl)), 
	     MoreLbl,
	     load_bytes(Tmp2, Base, Offset, {unsigned,big}, 1),
	     hipe_rtl:mk_alu(Tmp2, Tmp2, sll, hipe_rtl:mk_imm(8)),
	     hipe_rtl:mk_alu(Dst1, Dst1, 'or', Tmp2)];
	   true ->
	    [load_bytes(Dst1, Base, Offset, {unsigned,big}, 3),
	     hipe_rtl:mk_alu(Dst1, Dst1, sll, hipe_rtl:mk_imm(8)),
	     hipe_rtl:mk_branch(Tmp2, le, hipe_rtl:mk_imm(24), hipe_rtl:label_name(JoinLbl), hipe_rtl:label_name(MoreLbl)), 
	     MoreLbl,
	     load_bytes(Tmp2, Base, Offset, {unsigned,big}, 1),
	     hipe_rtl:mk_alu(Dst1, Dst1, 'or', Tmp2)]
	end,
  Code3 = [JoinLbl,
	   hipe_rtl:mk_move(Offset, Tmp3),
	   hipe_rtl:mk_alu(Dst1, Dst1, sll, Tmp1),
	   hipe_rtl:mk_alu(Dst1, Dst1, Shiftr, hipe_rtl:mk_imm(28-Size)),
	   hipe_rtl:mk_alu(Dst1, Dst1, 'or', hipe_rtl:mk_imm(15)),
	   hipe_rtl:mk_goto(TrueLblName),
	   TooBigLbl,
	   hipe_rtl:mk_move(Tmp1, hipe_rtl:mk_imm(Size)),
	   hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flag)),
	   gen_bs_call(bs_get_integer,
		       [Tmp1, Tmp2],
		       Dst1, MatchBuf, BinSize, Base, Offset, Orig,
		       TrueLblName, FalseLblName)],
  Code1 ++ Code2 ++ Code3.


get_unknown_size_int(Arg, Size, Flag, MatchBuf, BinSize, Base, Offset, Orig, Dst1, FalseLblName, TrueLblName, Type) ->
  
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
   hipe_rtl:mk_alu(Offset, Offset, add, Tmp1),
   hipe_rtl:mk_alu(Tmp1, Tmp1, 'and', hipe_rtl:mk_imm(7)),
   hipe_rtl:mk_alu(Tmp1, hipe_rtl:mk_imm(8), sub, Tmp1),
   hipe_rtl:mk_alu(Tmp1, Tmp1, 'and', hipe_rtl:mk_imm(7)),
   hipe_rtl:mk_alu(Tmp2, Tmp2, Shiftr, Tmp1),
   hipe_rtl:mk_alu(Tmp2, Tmp2, sll, hipe_rtl:mk_imm(4)),
   hipe_rtl:mk_alu(Dst1, Tmp2, 'or', hipe_rtl:mk_imm(15)),
   hipe_rtl:mk_goto(TrueLblName),
   ZeroLbl,
   hipe_rtl:mk_move(Dst1, hipe_rtl:mk_imm(15)),
   hipe_rtl:mk_goto(TrueLblName),	    
   UsecLbl,
   hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flag)),
   gen_bs_call(bs_get_integer,
	       [Tmp1, Tmp2],
	       Dst1, MatchBuf, BinSize, Base, Offset, Orig,
	       TrueLblName, FalseLblName)].


check_size(Offset, Size, BinSize, Tmp1, ContLblName, FalseLblName) ->
  [hipe_rtl:mk_alu(Tmp1, Offset, add, Size),
   hipe_rtl:mk_branch(Tmp1, le, BinSize, ContLblName, FalseLblName, 0.99)].

load_matchbuffer(MatchBuf,  BinSize, Base, Offset, Orig, TrueLblName, FalseLblName) ->
  SuccessLbl=hipe_rtl:mk_new_label(),
  [hipe_rtl:mk_call([MatchBuf], bs_get_matchbuffer, [], c, hipe_rtl:label_name(SuccessLbl), FalseLblName),
   SuccessLbl,
   hipe_rtl:mk_load(BinSize, MatchBuf, hipe_rtl:mk_imm(?MB_SIZE)),
   hipe_rtl:mk_load(Base, MatchBuf, hipe_rtl:mk_imm(?MB_BASE)),
   hipe_rtl:mk_load(Offset, MatchBuf, hipe_rtl:mk_imm(?MB_OFFSET)),
   hipe_rtl:mk_load(Orig, MatchBuf, hipe_rtl:mk_imm(?MB_ORIG)),
   hipe_rtl:mk_goto(TrueLblName)].

expand_runtime(Shifts, Args, WordReg, Scratch) ->
  expand_runtime(Shifts, Args, WordReg, Scratch, []).

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

skip_no_of_bits(SizeOfBin, Offset, NoOfBits, TrueLblName, FalseLblName) ->
  SuccessLbl = hipe_rtl:mk_new_label(),
  [hipe_rtl:mk_alub(Offset, NoOfBits, add, Offset, overflow, FalseLblName, hipe_rtl:label_name(SuccessLbl),0.01),
   SuccessLbl,
   hipe_rtl:mk_branch(SizeOfBin, lt, Offset, FalseLblName, TrueLblName, 0.01)].

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


