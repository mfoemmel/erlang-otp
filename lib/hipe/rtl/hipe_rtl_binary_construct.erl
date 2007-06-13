%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%%  Filename : 	hipe_rtl_inline_bs_ops.erl
%%  Module   :	hipe_rtl_inline_bs_ops
%%  Purpose  :  
%%  Notes    : 
%%  History  :	*2001-06-14 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: kostis $
%%              $Date: 2007/05/04 07:48:56 $
%%              $Revision: 1.3 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_rtl_binary_construct).
-export([gen_rtl/6]).

%%-------------------------------------------------------------------------

-include("../main/hipe.hrl").
-include("hipe_rtl.hrl").
-include("hipe_literals.hrl").

-define(BYTE_SHIFT, 3). %% Turn bits into bytes or vice versa
-define(LOW_BITS, 7). %% Three lowest bits set
-define(BYTE_SIZE, 8).

%% -------------------------------------------------------------------------
%% The code is generated as a list of lists, it will be flattened later.
%% 

gen_rtl(BsOP, Dst, Args, TrueLblName, FalseLblName, ConstTab) ->
  %%io:format("~w, ~w, ~w~n", [BsOP, Args, Dst]),
  case BsOP of
    {bs_put_string, String, SizeInBytes} ->
      [NewOffset] = get_real(Dst),
      [Base, Offset] = Args,
      put_string(NewOffset, ConstTab, String, SizeInBytes, Base, Offset, 
		 TrueLblName);
    _ -> 
      Code = 
	case BsOP of
	  bs_bits_to_bytes2 ->
	    [Src]=Args,
	    [Dst0]=Dst,
	    SuccessLbl = hipe_rtl:mk_new_label(),
	    Raw = hipe_rtl:mk_new_reg(),
	    [hipe_tagscheme:test_fixnum(Src, hipe_rtl:label_name(SuccessLbl),
				    FalseLblName, 0.99),
	     SuccessLbl,
	     hipe_tagscheme:untag_fixnum(Raw, Src),
	     hipe_rtl:mk_alu(Raw, Raw, add, hipe_rtl:mk_imm(?LOW_BITS)),
	     hipe_rtl:mk_alu(Raw, Raw, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
	     hipe_tagscheme:tag_fixnum(Dst0, Raw),
	     hipe_rtl:mk_goto(TrueLblName)];
	  bs_bits_to_bytes ->
	    [Src] = Args,
	    [Dst0] = Dst,
	    SuccessLbl = hipe_rtl:mk_new_label(),
	    NextLbl = hipe_rtl:mk_new_label(),
	    Raw = hipe_rtl:mk_new_reg(),
	    Tmp = hipe_rtl:mk_new_reg_gcsafe(),
	    [hipe_tagscheme:test_fixnum(Src, hipe_rtl:label_name(SuccessLbl),
					FalseLblName, 0.99),
	     SuccessLbl,
	     hipe_tagscheme:untag_fixnum(Raw, Src),
	     hipe_rtl:mk_alub(Tmp, Raw, 'and', hipe_rtl:mk_imm(?LOW_BITS),eq,
			      hipe_rtl:label_name(NextLbl),FalseLblName),
	     NextLbl,
	     hipe_rtl:mk_alu(Raw, Raw, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
	     hipe_tagscheme:tag_fixnum(Dst0, Raw),
	     hipe_rtl:mk_goto(TrueLblName)];
	  {bs_add, Const, Unit} ->
	    Raw = hipe_rtl:mk_imm(Const),
	    Tmp1 = hipe_rtl:mk_new_reg(),
	    Tmp2 = hipe_rtl:mk_new_reg(),
	    [New] = Args,
	    [Dst0] = Dst,
	    {SizeCode, SizeReg} = make_size(Unit, New, FalseLblName),
	    SizeCode ++
	      [hipe_rtl:mk_alu(Tmp1, SizeReg, add, Raw),
	       hipe_tagscheme:tag_fixnum(Dst0, Tmp1),
	       hipe_tagscheme:untag_fixnum(Tmp2, Dst0),
	       hipe_rtl:mk_branch(Tmp1, eq, Tmp2, TrueLblName, FalseLblName)];
	  
	  {bs_add, Unit} ->
	    Raw = hipe_rtl:mk_new_reg(),
	    Tmp1 = hipe_rtl:mk_new_reg(),
	    Tmp2 = hipe_rtl:mk_new_reg(),
	    [Old, New] = Args,
	    [Dst0] = Dst,
	    {SizeCode, SizeReg} = make_size(Unit, New, FalseLblName),
	    SizeCode ++
	      [hipe_tagscheme:untag_fixnum(Raw, Old),
	       hipe_rtl:mk_alu(Tmp1, SizeReg, add, Raw),
	       hipe_tagscheme:tag_fixnum(Dst0, Tmp1),
	       hipe_tagscheme:untag_fixnum(Tmp2, Dst0),
	       hipe_rtl:mk_branch(Tmp1, eq, Tmp2, TrueLblName, FalseLblName)];
	  
	  {bs_init, Size, _Flags} -> 
	    [] = Args,
	    [Dst0, Base, Offset] = Dst,
	    const_init2(Size, Dst0, Base, Offset, TrueLblName, FalseLblName);
	  
	  {bs_init, _Flags} -> 
	    [Size] = Args,
	    [Dst0, Base, Offset] = Dst,
	    var_init2(Size, Dst0, Base, Offset, TrueLblName, FalseLblName);
	 
	  {bs_put_binary_all, _Flags} ->
	    [Src, Base, Offset] = Args,
	    [NewOffset] = get_real(Dst),
	    put_binary_all(NewOffset, Src, Base, Offset, TrueLblName, FalseLblName);
	   
	  {bs_put_binary, Size, _Flags} ->
	    [NewOffset] = get_real(Dst),
	    case Args of
	      [Src, Base, Offset] ->
		put_static_binary(NewOffset, Src, Size, Base, Offset, 
					    TrueLblName, FalseLblName);
	      [Src, Bits, Base, Offset]  ->
		{SizeCode, SizeReg} = make_size(Size, Bits, FalseLblName),
		InCode = put_dynamic_binary(NewOffset, Src, SizeReg, Base, 
					    Offset, TrueLblName, FalseLblName),
		SizeCode ++ InCode
	    end;
		
	  {bs_put_float, Size, Flags, ConstInfo} ->    
	    [NewOffset] = get_real(Dst),
	    Aligned = aligned(Flags),
	    LittleEndian = littleendian(Flags),
	    case Args of
	      [Src, Base, Offset] ->
		CCode = static_float_c_code(NewOffset, Src, Base, Offset, Size, Flags, 
					    TrueLblName, FalseLblName),
		put_float(NewOffset, Src, Base, Offset, Size, CCode, Aligned, 
			  LittleEndian, ConstInfo, TrueLblName);
	      [Src, Bits, Base, Offset] ->
		{SizeCode, SizeReg} = make_size(Size, Bits, FalseLblName),
		InCode = float_c_code(NewOffset, Src, Base, Offset, SizeReg, 
					    Flags, TrueLblName, FalseLblName),
		SizeCode ++ InCode
	    end;

	  {bs_put_integer, Size, Flags, ConstInfo} ->
	    Aligned = aligned(Flags),
	    LittleEndian = littleendian(Flags),
	    [NewOffset] = get_real(Dst),
	    case ConstInfo of
	      fail ->
		[hipe_rtl:mk_goto(FalseLblName)];
	      _ ->
		case Args of
		 [Src, Base, Offset]  ->
		    CCode = static_int_c_code(NewOffset, Src, Base, Offset, Size, Flags, 
					      TrueLblName, FalseLblName),
		    put_static_int(NewOffset, Src, Base, Offset, Size, 
						    CCode, Aligned, LittleEndian, 
						    TrueLblName);
		  [Src, Bits, Base, Offset] ->
		    {SizeCode, SizeReg} = make_size(Size, Bits, FalseLblName),
		    CCode = int_c_code(NewOffset, Src, Base, Offset, SizeReg, Flags, 
					      TrueLblName, FalseLblName),
		    InCode = 
		      put_dynamic_int(NewOffset, Src, Base, Offset, 
				      SizeReg, CCode, Aligned, 
				      LittleEndian, TrueLblName),
		    SizeCode ++ InCode
		end
	    end;    
	  {unsafe_bs_put_integer, 0, _Flags, _ConstInfo} ->
	    [NewOffset] = get_real(Dst),
	    case Args of
	      [_Src, _Base, Offset]  ->
		[hipe_rtl:mk_move(NewOffset,Offset),
		 hipe_rtl:mk_goto(TrueLblName)];
	      [_Src, _Bits, _Base, Offset] ->
		[hipe_rtl:mk_move(NewOffset,Offset),
		 hipe_rtl:mk_goto(TrueLblName)]
	    end;
	    
	  {unsafe_bs_put_integer, Size, Flags, ConstInfo} ->
	    Aligned = aligned(Flags),
	    LittleEndian = littleendian(Flags),
	    [NewOffset] = get_real(Dst),
	    case ConstInfo of
	      fail ->
		[hipe_rtl:mk_goto(FalseLblName)];
	      _ ->
		case Args of
		 [Src, Base, Offset]  ->
		    CCode = static_int_c_code(NewOffset, Src, Base, Offset, Size, Flags, 
					      TrueLblName, FalseLblName),
		    put_unsafe_static_int(NewOffset, Src, Base, Offset, Size, 
					  CCode, Aligned, LittleEndian, 
					  TrueLblName);
		  [Src, Bits, Base, Offset] ->
		    {SizeCode, SizeReg} = make_size(Size, Bits, FalseLblName),
		    CCode = int_c_code(NewOffset, Src, Base, Offset, SizeReg, Flags, 
					      TrueLblName, FalseLblName),
		    InCode = 
		      put_unsafe_dynamic_int(NewOffset, Src, Base, Offset, 
					     SizeReg, CCode, Aligned, 
					     LittleEndian, TrueLblName),
		    SizeCode ++ InCode
		end
	    end; 
	  bs_final ->
	    [Src, Offset] = Args,
	    BitSize = hipe_rtl:mk_new_reg(),
	    ByteSize = hipe_rtl:mk_new_reg(),
	    ShortLbl = hipe_rtl:mk_new_label(),
	    LongLbl = hipe_rtl:mk_new_label(),
	    case Dst of
	      [DstVar] ->
		[
		 hipe_rtl:mk_alub(BitSize, Offset, 'and', hipe_rtl:mk_imm(?LOW_BITS),
				 eq, hipe_rtl:label_name(ShortLbl), 
				  hipe_rtl:label_name(LongLbl)),
		 ShortLbl,
		 hipe_rtl:mk_move(DstVar, Src),
		 hipe_rtl:mk_goto(TrueLblName),
		 %hipe_rtl:mk_goto(hipe_rtl:label_name(LongLbl)),
		 LongLbl,
		 hipe_rtl:mk_alu(ByteSize, Offset, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
		 hipe_tagscheme:unsafe_mk_sub_binary(DstVar, ByteSize, hipe_rtl:mk_imm(0),
						   BitSize, hipe_rtl:mk_imm(0), Src),
		 hipe_rtl:mk_goto(TrueLblName)]; 
	      [] ->
		[hipe_rtl:mk_goto(TrueLblName)]
	    end
	end,
      {Code, ConstTab}
  end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Code that is used to create calls to beam functions
%%
%%  X_c_code/8, used for putting terms into binaries
%%
%%  X_get_c_code/10, used for getting terms from binaries
%%
%% - gen_test_sideffect_bs_call/4 is used to make a C-call that might
%%       fail but doesn't return an erlang value.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

static_float_c_code(NewOffset, Src, Base, Offset, Size, Flags, TrueLblName, 
		    FalseLblName) ->
   SizeReg = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_move(SizeReg, hipe_rtl:mk_imm(Size))|
   float_c_code(NewOffset, Src, Base, Offset, SizeReg, Flags, TrueLblName, FalseLblName)].

float_c_code(NewOffset, Src, Base, Offset, SizeReg, Flags, TrueLblName, FalseLblName) ->
  put_c_code(bs_put_small_float, NewOffset, Src, Base, Offset, SizeReg, 
	     Flags, TrueLblName, FalseLblName).

static_int_c_code(NewOffset, Src, Base, Offset, Size, Flags, TrueLblName, 
		    FalseLblName) ->
  SizeReg = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_move(SizeReg, hipe_rtl:mk_imm(Size))|
   int_c_code(NewOffset, Src, Base, Offset, SizeReg, Flags, TrueLblName, FalseLblName)].

int_c_code(NewOffset, Src, Base, Offset, SizeReg, Flags, TrueLblName, FalseLblName) ->
  put_c_code(bs_put_big_integer, NewOffset, Src, Base, Offset, SizeReg, 
	     Flags, TrueLblName, FalseLblName).

binary_c_code(NewOffset, Src, Base, Offset, Size, TrueLblName) ->
  PassedLbl = hipe_rtl:mk_new_label(),
  SizeReg = hipe_rtl:mk_new_reg(),
  FlagsReg = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_move(FlagsReg, hipe_rtl:mk_imm(0)),
   hipe_rtl:mk_move(SizeReg, Size),
   hipe_rtl:mk_call([], bs_put_bits, [Src, SizeReg, Base, Offset, FlagsReg], 
		    hipe_rtl:label_name(PassedLbl),[],not_remote),
   PassedLbl,
   hipe_rtl:mk_alu(NewOffset, Offset, add, SizeReg),
   hipe_rtl:mk_goto(TrueLblName)].

put_c_code(Func, NewOffset, Src, Base, Offset, SizeReg, Flags, TrueLblName, 
	   FalseLblName) ->
  PassedLbl = hipe_rtl:mk_new_label(),
  FlagsReg = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_move(FlagsReg, hipe_rtl:mk_imm(Flags)),
   gen_test_sideffect_bs_call(Func, [Src, SizeReg, Base, Offset, FlagsReg], 
			      hipe_rtl:label_name(PassedLbl), FalseLblName),
   PassedLbl,
   hipe_rtl:mk_alu(NewOffset, Offset, add, SizeReg),
   hipe_rtl:mk_goto(TrueLblName)].

gen_test_sideffect_bs_call(Name,Args,TrueLblName,FalseLblName) ->
  Tmp1 = hipe_rtl:mk_new_reg(),
  RetLbl =  hipe_rtl:mk_new_label(),
  [hipe_rtl:mk_call([Tmp1], Name, Args, hipe_rtl:label_name(RetLbl),[],not_remote),
   RetLbl,
   hipe_rtl:mk_branch(Tmp1, eq, hipe_rtl:mk_imm(0), 
		      FalseLblName, TrueLblName, 0.01)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Small utility functions:
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_regs(X) when X > 0 ->
  [hipe_rtl:mk_new_reg()|create_regs(X-1)];
create_regs(0) ->
  [].

create_lbls(X) when X > 0 ->
  [hipe_rtl:mk_new_label()|create_lbls(X-1)];
create_lbls(0) ->
  [].

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

get_real([]) ->
  [hipe_rtl:mk_new_reg()];
get_real([NewOffset]) ->
  [NewOffset].

%%-----------------------------------------------------------------------------
%% Help functions implementing the bs operations in rtl code.
%%
%% The following functions are called from the translation switch:
%% 
%% - put_string/8 creates code to copy a string to a binary
%%   starting at base+offset and ending at base+newoffset   
%%
%% - const_init2/6 initializes the creation of a binary of constant size
%%      
%% - var_init2/6 initializes the creation of a binary of variable size
%%
%% - get_int_from_unaligned_bin/11 creates code to extract a fixed
%%       size integer from a binary or makes a c-call if it does not
%%       conform to some certain rules.
%%
%% - get_unknown_size_int/11 creates code to extract a variable size
%%       byte-aligned integer from a binary or makes a c-call if it
%%       does not conform to some certain rules.
%%
%% - skip_no_of_bits/5 creates code to skip a variable amount of bits in a binary.
%%
%% - load_match_buffer/7 reloads the C-matchbuffer to RTL registers.
%%
%% - expand_runtime/4 creates code that calculates a maximal heap need
%%       before a binary match
%%
%%-----------------------------------------------------------------------------

put_string(NewOffset, ConstTab, String, SizeInBytes, Base, Offset, TLName) ->
  [StringBase] = create_regs(1),
  {NewTab, Lbl} = hipe_consttab:insert_block(ConstTab, byte, String),
  {[hipe_rtl:mk_load_address(StringBase, Lbl, constant)|
    copy_string(StringBase, SizeInBytes, Base, Offset, 
		NewOffset, TLName)],
   NewTab}.
							  
const_init2(Size, Dst, Base, Offset, TrueLblName, _FalseLblName) ->
  Log2WordSize = hipe_rtl_arch:log2_word_size(),
  WordSize = hipe_rtl_arch:word_size(),
  NextLbl = hipe_rtl:mk_new_label(),
  case Size =< ?MAX_HEAP_BIN_SIZE of
    true ->
      [hipe_rtl:mk_gctest(((Size + 3*WordSize-1) bsr Log2WordSize)+?SUB_BIN_WORDSIZE),
       hipe_tagscheme:create_heap_binary(Base, Size, Dst),
       hipe_rtl:mk_move(Offset, hipe_rtl:mk_imm(0)),
       hipe_rtl:mk_goto(TrueLblName)];
    false ->
      ByteSize = hipe_rtl:mk_new_reg(),
      [hipe_rtl:mk_gctest(?PROC_BIN_WORDSIZE+?SUB_BIN_WORDSIZE),
       hipe_rtl:mk_move(Offset, hipe_rtl:mk_imm(0)),
       hipe_rtl:mk_move(ByteSize, hipe_rtl:mk_imm(Size)),
       hipe_rtl:mk_call([Base], bs_allocate, [ByteSize],
			hipe_rtl:label_name(NextLbl),[],not_remote),
       NextLbl,
       hipe_tagscheme:create_refc_binary(Base, ByteSize, Dst),
       hipe_rtl:mk_goto(TrueLblName)]
  end.

var_init2(Size, Dst, Base, Offset, TrueLblName, _FalseLblName) ->
  Log2WordSize = hipe_rtl_arch:log2_word_size(),
  WordSize = hipe_rtl_arch:word_size(),
  HeapLbl = hipe_rtl:mk_new_label(),
  REFCLbl = hipe_rtl:mk_new_label(),
  NextLbl = hipe_rtl:mk_new_label(),
  USize = hipe_rtl:mk_new_reg(),
  Tmp = hipe_rtl:mk_new_reg(),
  [hipe_tagscheme:untag_fixnum(USize, Size),
   
   hipe_rtl:mk_move(Offset, hipe_rtl:mk_imm(0)),
   hipe_rtl:mk_branch(USize, le, hipe_rtl:mk_imm(?MAX_HEAP_BIN_SIZE), 
		      hipe_rtl:label_name(HeapLbl), 
		      hipe_rtl:label_name(REFCLbl)),
   HeapLbl,
   hipe_rtl:mk_alu(Tmp, USize, add, hipe_rtl:mk_imm(3*WordSize-1)),
   hipe_rtl:mk_alu(Tmp, Tmp, srl, hipe_rtl:mk_imm(Log2WordSize)),
   hipe_rtl:mk_alu(Tmp, Tmp, add, hipe_rtl:mk_imm(?MAX_HEAP_BIN_SIZE)),
   hipe_rtl:mk_gctest(Tmp),
   hipe_tagscheme:untag_fixnum(USize, Size),
   hipe_tagscheme:create_heap_binary(Base, USize, Dst),
   hipe_rtl:mk_goto(TrueLblName),
   REFCLbl,
   hipe_rtl:mk_gctest(?PROC_BIN_WORDSIZE+?SUB_BIN_WORDSIZE),
   hipe_tagscheme:untag_fixnum(USize, Size),
   hipe_rtl:mk_call([Base], bs_allocate, [USize],
		    hipe_rtl:label_name(NextLbl),[],not_remote),
   NextLbl,
   hipe_tagscheme:create_refc_binary(Base, USize, Dst),
   hipe_rtl:mk_goto(TrueLblName)].

put_binary_all(NewOffset, Src, Base, Offset, TLName, FLName) ->
  [SrcBase,SrcOffset,NumBits] = create_regs(3),
  CCode = binary_c_code(NewOffset, Src, Base, Offset, NumBits, TLName),
  AlignedCode = copy_aligned_bytes(SrcBase, SrcOffset, NumBits, Base, Offset, 
				   NewOffset, TLName),
  get_base_offset_size(Src, SrcBase, SrcOffset, NumBits,FLName) ++
    test_alignment(SrcOffset,NumBits,Offset,AlignedCode,CCode).

test_alignment(SrcOffset,NumBits,Offset,AlignedCode,CCode) ->
  Tmp = hipe_rtl:mk_new_reg(),  
  AlignedLbl = hipe_rtl:mk_new_label(),
  CLbl = hipe_rtl:mk_new_label(),
  [hipe_rtl:mk_alu(Tmp, SrcOffset, 'or', NumBits),
   hipe_rtl:mk_alu(Tmp, Tmp, 'or', Offset),
   hipe_rtl:mk_alub(Tmp, Tmp, 'and', hipe_rtl:mk_imm(?LOW_BITS), eq, 
		    hipe_rtl:label_name(AlignedLbl), hipe_rtl:label_name(CLbl)),
   AlignedLbl,
   AlignedCode,
   CLbl,
   CCode].
 

put_static_binary(NewOffset, Src, Size, Base, Offset, TLName, FLName) ->
  [SrcBase, SrcOffset, SrcSize] = create_regs(3),
    case Size of 
      0 ->
	get_base_offset_size(Src, SrcBase, SrcOffset, SrcSize, FLName) ++
	[hipe_rtl:mk_move(NewOffset, Offset), 
	 hipe_rtl:mk_goto(TLName)];
      _ ->
	CCode = binary_c_code(NewOffset, Src, Base, Offset, hipe_rtl:mk_imm(Size), TLName),
	AlignedCode = copy_aligned_bytes(SrcBase, SrcOffset, hipe_rtl:mk_imm(Size), Base, Offset, 
					 NewOffset, TLName),
	get_base_offset_size(Src, SrcBase, SrcOffset, SrcSize, FLName) ++
	  small_check(hipe_rtl:mk_imm(Size), SrcSize, FLName) ++
	  test_alignment(SrcOffset,hipe_rtl:mk_imm(Size),Offset,AlignedCode, CCode)
     end.

put_dynamic_binary(NewOffset, Src, SizeReg, Base, Offset, TLName, FLName) ->
  [SrcBase, SrcOffset, SrcSize] = create_regs(3),
  CCode = binary_c_code(NewOffset, Src, Base, Offset, SizeReg, TLName),
  AlignedCode = copy_aligned_bytes(SrcBase, SrcOffset, SizeReg, Base, Offset, 
				   NewOffset, TLName),
  get_base_offset_size(Src, SrcBase, SrcOffset, SrcSize, FLName) ++
    small_check(SizeReg, SrcSize, FLName) ++
    test_alignment(SrcOffset, SizeReg, Offset, AlignedCode, CCode).



put_float(NewOffset, Src, Base, Offset, 64, CCode, Aligned, LittleEndian, 
			  ConstInfo, TrueLblName) ->
  [CLbl] = create_lbls(1),
  case {Aligned, LittleEndian} of
    {true, false} ->
      copy_float_big(Base, Offset, NewOffset, Src,  hipe_rtl:label_name(CLbl),
		     TrueLblName, ConstInfo) ++
	[CLbl|CCode];
    {true, true} ->
      copy_float_little(Base, Offset, NewOffset, Src, hipe_rtl:label_name(CLbl),  
			TrueLblName, ConstInfo) ++
	[CLbl|CCode];
    {false, _} ->
      CCode
  end;

put_float(_NewOffset, _Src, _Base, _Offset, _Size, CCode, _Aligned, _LittleEndian, 
	  _ConstInfo, _TrueLblName) ->
  CCode.

put_static_int(NewOffset, Src, Base, Offset, Size, CCode, Aligned, 
	       LittleEndian, TrueLblName) ->
  {Init, End, UntaggedSrc} = make_init_end(Src, CCode, TrueLblName),
  case {Aligned, LittleEndian} of
    {true, true} ->
      Init ++
	copy_int_little(Base, Offset, NewOffset, Size, UntaggedSrc) ++
	End;
    {true, false} ->
      Init ++
	copy_int_big(Base, Offset, NewOffset, Size, UntaggedSrc) ++
	End;
    {false, true} ->
      CCode;
    {false, false} ->
       Init ++
	copy_offset_int_big(Base, Offset, NewOffset, Size, UntaggedSrc) ++
	End
  end.

put_unsafe_static_int(NewOffset, Src, Base, Offset, Size, CCode, Aligned, 
		      LittleEndian, TrueLblName) ->
  {Init, End, UntaggedSrc} = make_init_end(Src, TrueLblName),
  case {Aligned, LittleEndian} of
    {true, true} ->
      Init ++
	copy_int_little(Base, Offset, NewOffset, Size, UntaggedSrc) ++
	End;
    {true, false} ->
      Init ++
	copy_int_big(Base, Offset, NewOffset, Size, UntaggedSrc) ++
	End;
    {false, true} ->
      CCode;
    {false, false} ->
       Init ++
	copy_offset_int_big(Base, Offset, NewOffset, Size, UntaggedSrc) ++
	End
  end.

put_dynamic_int(NewOffset, Src, Base, Offset, SizeReg, CCode, Aligned, 
		LittleEndian, TrueLblName) ->
  {Init, End, UntaggedSrc} = make_init_end(Src, CCode, TrueLblName),
  case Aligned of
    true ->
      case LittleEndian of
	true ->
	   Init ++
	    copy_int_little(Base, Offset, NewOffset, SizeReg, UntaggedSrc) ++
	    End;
	false ->
	  Init ++
	    copy_int_big(Base, Offset, NewOffset, SizeReg, UntaggedSrc) ++
	    End
	end;
    false ->
      CCode
  end.

put_unsafe_dynamic_int(NewOffset, Src, Base, Offset, SizeReg, CCode, Aligned, 
		       LittleEndian, TrueLblName) ->
  {Init, End, UntaggedSrc} = make_init_end(Src, TrueLblName),
  case Aligned of
    true ->
      case LittleEndian of
	true ->
	   Init ++
	    copy_int_little(Base, Offset, NewOffset, SizeReg, UntaggedSrc) ++
	    End;
	false ->
	  Init ++
	    copy_int_big(Base, Offset, NewOffset, SizeReg, UntaggedSrc) ++
	    End
	end;
    false ->
      CCode
  end.
    



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Help functions used by the above
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_init_end(Src, CCode, TrueLblName) ->
  [CLbl, SuccessLbl] = create_lbls(2),
  [UntaggedSrc] = create_regs(1),
  Init = [hipe_tagscheme:test_fixnum(Src, hipe_rtl:label_name(SuccessLbl), 
				     hipe_rtl:label_name(CLbl), 0.99),
	  SuccessLbl,
	  hipe_tagscheme:untag_fixnum(UntaggedSrc,Src)],
  End = [hipe_rtl:mk_goto(TrueLblName), CLbl| CCode],
  {Init, End, UntaggedSrc}.

make_init_end(Src, TrueLblName) ->
  [UntaggedSrc] = create_regs(1),
  Init = [hipe_tagscheme:untag_fixnum(UntaggedSrc,Src)],
  End = [hipe_rtl:mk_goto(TrueLblName)],
  {Init, End, UntaggedSrc}.

get_base_offset_size(Binary, SrcBase, SrcOffset, SrcSize, FLName) -> 
  EndLbl = hipe_rtl:mk_new_label(),
  EndLblName = hipe_rtl:label_name(EndLbl),
  SuccessLbl = hipe_rtl:mk_new_label(),
  SubLbl = hipe_rtl:mk_new_label(),
  OtherLbl = hipe_rtl:mk_new_label(),
  HeapLbl = hipe_rtl:mk_new_label(),
  REFCLbl = hipe_rtl:mk_new_label(),
  SubHeapLbl = hipe_rtl:mk_new_label(),
  SubREFCLbl = hipe_rtl:mk_new_label(),
  BitSize = hipe_rtl:mk_new_reg(),
  BitOffset = hipe_rtl:mk_new_reg(),
  Orig = hipe_rtl:mk_new_reg(),
  [hipe_tagscheme:test_binary(Binary, hipe_rtl:label_name(SuccessLbl), FLName, 0.99),
   SuccessLbl,
   hipe_rtl:mk_load(SrcSize, Binary, hipe_rtl:mk_imm(?SUB_BIN_BINSIZE-2)),
   hipe_rtl:mk_alu(SrcSize, SrcSize, sll, hipe_rtl:mk_imm(?BYTE_SHIFT)),
   hipe_tagscheme:test_subbinary(Binary, hipe_rtl:label_name(SubLbl), hipe_rtl:label_name(OtherLbl)),
   SubLbl,
   hipe_rtl:mk_load(BitSize, Binary, hipe_rtl:mk_imm(?SUB_BIN_BITSIZE-2)),
   hipe_rtl:mk_alu(SrcSize, SrcSize, add, BitSize),
   hipe_rtl:mk_load(SrcOffset, Binary, hipe_rtl:mk_imm(?SUB_BIN_OFFS-2)),
   hipe_rtl:mk_load(BitOffset, Binary, hipe_rtl:mk_imm(?SUB_BIN_BITOFFS-2)),
   hipe_rtl:mk_alu(SrcOffset, SrcOffset, sll, hipe_rtl:mk_imm(?BYTE_SHIFT)),
   hipe_rtl:mk_alu(SrcOffset, SrcOffset, add, BitOffset),
   hipe_rtl:mk_load(Orig, Binary, hipe_rtl:mk_imm(?SUB_BIN_ORIG-2)),
   hipe_tagscheme:test_heap_binary(Orig, hipe_rtl:label_name(SubHeapLbl), hipe_rtl:label_name(SubREFCLbl)),
   SubHeapLbl,
   hipe_rtl:mk_alu(SrcBase, Orig, add, hipe_rtl:mk_imm(?HEAP_BIN_DATA-2)),
   hipe_rtl:mk_goto(EndLblName),
   SubREFCLbl,
   hipe_rtl:mk_load(SrcBase, Orig, hipe_rtl:mk_imm(?PROC_BIN_BYTES-2)),
   hipe_rtl:mk_goto(EndLblName),
   OtherLbl,
   hipe_rtl:mk_move(SrcOffset, hipe_rtl:mk_imm(0)),
   hipe_tagscheme:test_heap_binary(Binary, hipe_rtl:label_name(HeapLbl), hipe_rtl:label_name(REFCLbl)),
   HeapLbl,
   hipe_rtl:mk_alu(SrcBase, Binary, add, hipe_rtl:mk_imm(?HEAP_BIN_DATA-2)),
   hipe_rtl:mk_goto(EndLblName),
   REFCLbl,
   hipe_rtl:mk_load(SrcBase, Binary, hipe_rtl:mk_imm(?PROC_BIN_BYTES-2)),
   EndLbl].

copy_aligned_bytes(CopyBase, CopyOffset, Size, Base, Offset, NewOffset, TrueLblName) ->
  BaseDst = hipe_rtl:mk_new_reg(),
  BaseSrc = hipe_rtl:mk_new_reg(),
  Iter = hipe_rtl:mk_new_reg(),
  Extra = hipe_rtl:mk_new_reg(),
  BothOffset = hipe_rtl:mk_new_reg(), 
    initializations(BaseSrc, BaseDst, BothOffset, CopyOffset, Offset, CopyBase, Base) ++
    [hipe_rtl:mk_alu(Extra, Size, 'and', hipe_rtl:mk_imm(?LOW_BITS)), 
     hipe_rtl:mk_alu(Iter, Size, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)), 
     hipe_rtl:mk_alu(NewOffset, Offset, 'add', Size)] ++
    easy_loop(BaseSrc, BaseDst, BothOffset, Iter, Extra, TrueLblName).

copy_string(StringBase, StringSize, BinBase, BinOffset, NewOffset, TrueLblName) ->
  TmpOffset = hipe_rtl:mk_new_reg(),
  BothOffset = hipe_rtl:mk_new_reg(),
  NewBinBase = hipe_rtl:mk_new_reg(),
  InitOffs = hipe_rtl:mk_new_reg(),
  EasyLbl = hipe_rtl:mk_new_label(),
  HardLbl = hipe_rtl:mk_new_label(),
  [hipe_rtl:mk_alu(TmpOffset, BinOffset, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
   hipe_rtl:mk_alu(NewBinBase, BinBase, add, TmpOffset),
   hipe_rtl:mk_move(BothOffset, hipe_rtl:mk_imm(0)),
   hipe_rtl:mk_alub(InitOffs, BinOffset, 'and', hipe_rtl:mk_imm(?LOW_BITS), eq, hipe_rtl:label_name(EasyLbl), hipe_rtl:label_name(HardLbl)),
   EasyLbl,
   hipe_rtl:mk_alu(NewOffset, BinOffset, add, 
		   hipe_rtl:mk_imm(?bytes_to_bits(StringSize)))] ++
   easy_loop(StringBase, NewBinBase, BothOffset, hipe_rtl:mk_imm(StringSize), hipe_rtl:mk_imm(0), TrueLblName) ++
    [HardLbl,
     hipe_rtl:mk_alu(NewOffset, BinOffset, add,
		     hipe_rtl:mk_imm(?bytes_to_bits(StringSize)))] ++
    hard_loop(StringBase, NewBinBase, BothOffset, hipe_rtl:mk_imm(StringSize),
	      InitOffs, hipe_rtl:mk_imm(0), TrueLblName).

%copy_aligned_string(StringBase, StringSize, BinBase, BinOffset, NewOffset,
%		    TrueLblName) ->
%  WordSize = hipe_rtl_arch:word_size(),
%  TmpOffset = hipe_rtl:mk_new_reg(),
%  BothOffset = hipe_rtl:mk_new_reg(),
%  NewBinBase = hipe_rtl:mk_new_reg(),
%  ExtraBytes = StringSize rem WordSize,
%  Code1 = [hipe_rtl:mk_alu(TmpOffset, BinOffset, srl, hipe_rtl:mk_imm(3)),
%	   hipe_rtl:mk_alu(NewBinBase, BinBase, add, TmpOffset),
%	   hipe_rtl:mk_move(BothOffset, hipe_rtl:mk_imm(0)),
%	   hipe_rtl:mk_alu(NewOffset, BinOffset, add,
%			   hipe_rtl:mk_imm(?bytes_to_bits(StringSize)))],
%  Code2 = case WordSize * (StringSize div WordSize) of
%	    0 ->
%	      [];
%	    X ->
%	      tight_loop(StringBase, NewBinBase, BothOffset, hipe_rtl:mk_imm(X))
%	  end,
%  Code3 = last_part(StringBase, NewBinBase, BothOffset, ExtraBytes, TrueLblName),
%  Code1 ++ Code2 ++ Code3.

%% store_4 should be replaced by store unaligned word

%tight_loop(BaseSrc, BaseDst, BothOffset, Iterations) -> 
%  Tmp1 = hipe_rtl:mk_new_reg(),
%  LoopLbl = hipe_rtl:mk_new_label(),
%  FinishLbl = hipe_rtl:mk_new_label(),
%  [LoopLbl,
%   hipe_rtl:mk_load(Tmp1, BaseSrc, BothOffset),
%   hipe_rtl_arch:store_4(BaseDst, BothOffset, Tmp1),
%   hipe_rtl:mk_branch(BothOffset, ne, Iterations, 
%		      hipe_rtl:label_name(LoopLbl), 
%		      hipe_rtl:label_name(FinishLbl), 0.99),
%   FinishLbl].

%last_part(_StringBase, _NewBinBase, _BothOffset, 0, TrueLblName) ->
%  [hipe_rtl:mk_goto(TrueLblName)];
%last_part(StringBase, NewBinBase, BothOffset, 1, TrueLblName) ->
%  Tmp1 = hipe_rtl:mk_new_reg(),
%  [hipe_rtl:mk_load(Tmp1, StringBase, BothOffset, byte, unsigned),
%   hipe_rtl:mk_store(NewBinBase, BothOffset, Tmp1, byte),
%   hipe_rtl:mk_goto(TrueLblName)];
%last_part(StringBase, NewBinBase, BothOffset, N, TrueLblName) ->
%  Tmp1 = hipe_rtl:mk_new_reg(),
%  [hipe_rtl:mk_load(Tmp1, StringBase, BothOffset, byte, unsigned),
%   hipe_rtl:mk_store(NewBinBase, BothOffset, Tmp1, byte),
%   hipe_rtl:mk_alu(BothOffset, BothOffset, add, hipe_rtl:mk_imm(1))|
%   last_part(StringBase, NewBinBase, BothOffset, N-1, TrueLblName)].

small_check(SizeVar, CopySize, FalseLblName) ->
  SuccessLbl = hipe_rtl:mk_new_label(),
  [hipe_rtl:mk_branch(SizeVar, le, CopySize, hipe_rtl:label_name(SuccessLbl), FalseLblName),
   SuccessLbl].

easy_loop(BaseSrc, BaseDst, BothOffset, Iterations, Extra, TrueLblName) -> 
  Tmp1 = hipe_rtl:mk_new_reg(),
  Shift = hipe_rtl:mk_new_reg(),
  LoopLbl = hipe_rtl:mk_new_label(),
  TopLbl = hipe_rtl:mk_new_label(),
  EndLbl = hipe_rtl:mk_new_label(),
  ExtraLbl = hipe_rtl:mk_new_label(),
  [TopLbl,
   hipe_rtl:mk_branch(BothOffset, ne, Iterations, hipe_rtl:label_name(LoopLbl),  hipe_rtl:label_name(EndLbl), 0.99),
   LoopLbl,
   hipe_rtl:mk_load(Tmp1, BaseSrc, BothOffset, byte, unsigned),
   hipe_rtl:mk_store(BaseDst, BothOffset, Tmp1, byte),
   hipe_rtl:mk_alu(BothOffset, BothOffset, add, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_goto(hipe_rtl:label_name(TopLbl)),
   EndLbl,
   hipe_rtl:mk_branch(Extra, eq, hipe_rtl:mk_imm(0), TrueLblName, hipe_rtl:label_name(ExtraLbl)),
   ExtraLbl,
   hipe_rtl:mk_load(Tmp1, BaseSrc, BothOffset, byte, unsigned),
   hipe_rtl:mk_alu(Shift, hipe_rtl:mk_imm(?BYTE_SIZE), sub, Extra),
   hipe_rtl:mk_alu(Tmp1, Tmp1, srl, Shift),
   hipe_rtl:mk_alu(Tmp1, Tmp1, sll, Shift),
   hipe_rtl:mk_store(BaseDst, BothOffset, Tmp1, byte),
   hipe_rtl:mk_goto(TrueLblName)].


hard_loop(BaseSrc, BaseDst, BothOffset, Iterations, 
	  InitOffset, Extra, TrueLblName) ->
  Tmp1 = hipe_rtl:mk_new_reg(),
  Tmp2 = hipe_rtl:mk_new_reg(),
  OldByte = hipe_rtl:mk_new_reg(),
  NewByte = hipe_rtl:mk_new_reg(),
  SaveByte = hipe_rtl:mk_new_reg(),
  Total = hipe_rtl:mk_new_reg(),
  Shift = hipe_rtl:mk_new_reg(),
  LoopLbl = hipe_rtl:mk_new_label(),
  EndLbl = hipe_rtl:mk_new_label(),
  ExtraLbl = hipe_rtl:mk_new_label(),
  SimpleLbl = hipe_rtl:mk_new_label(),
  TopLbl = hipe_rtl:mk_new_label(),
  [hipe_rtl:mk_load(OldByte, BaseDst, BothOffset, byte, unsigned),
   hipe_rtl:mk_alu(Tmp1, hipe_rtl:mk_imm(?BYTE_SIZE), sub, InitOffset), 
   TopLbl,
   hipe_rtl:mk_branch(BothOffset, ne, Iterations, hipe_rtl:label_name(LoopLbl), 
		      hipe_rtl:label_name(EndLbl)),
   LoopLbl, 
   hipe_rtl:mk_load(NewByte, BaseSrc, BothOffset, byte, unsigned),
   hipe_rtl:mk_alu(Tmp2, NewByte, srl, InitOffset),
   hipe_rtl:mk_alu(SaveByte, OldByte, 'or', Tmp2),
   hipe_rtl:mk_store(BaseDst, BothOffset, SaveByte, byte),
   hipe_rtl:mk_alu(OldByte, NewByte, sll, Tmp1),
   hipe_rtl:mk_alu(BothOffset, BothOffset, 'add', hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_goto(hipe_rtl:label_name(TopLbl)),
   EndLbl,
   hipe_rtl:mk_branch(Extra, eq, hipe_rtl:mk_imm(0), 
		      hipe_rtl:label_name(SimpleLbl), 
		      hipe_rtl:label_name(ExtraLbl)),
   ExtraLbl,
   hipe_rtl:mk_load(NewByte, BaseSrc, BothOffset, byte, unsigned),
   hipe_rtl:mk_alu(Shift, hipe_rtl:mk_imm(?BYTE_SIZE), sub, Extra),
   hipe_rtl:mk_alu(NewByte, NewByte, srl, Shift),
   hipe_rtl:mk_alu(NewByte, NewByte, sll, Shift),
   hipe_rtl:mk_alu(Tmp2, NewByte, srl, InitOffset),
   hipe_rtl:mk_alu(SaveByte, OldByte, 'or', Tmp2),
   hipe_rtl:mk_store(BaseDst, BothOffset, SaveByte, byte),
   hipe_rtl:mk_alu(OldByte, NewByte, sll, Tmp1),
   hipe_rtl:mk_alu(Total, Extra, add, InitOffset),
   hipe_rtl:mk_branch(Total, gt, hipe_rtl:mk_imm(?BYTE_SIZE), 
		      hipe_rtl:label_name(SimpleLbl), TrueLblName),
   SimpleLbl,
   hipe_rtl:mk_store(BaseDst, BothOffset, OldByte, byte),
   hipe_rtl:mk_goto(TrueLblName)].


initializations(BaseTmp1, BaseTmp2, BothOffset, CopyOffset, Offset, CopyBase, Base) ->
  OffsetTmp1 = hipe_rtl:mk_new_reg(),
  OffsetTmp2 = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_alu(OffsetTmp1, CopyOffset, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
   hipe_rtl:mk_alu(OffsetTmp2, Offset, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
   hipe_rtl:mk_alu(BaseTmp1, CopyBase, add, OffsetTmp1),
   hipe_rtl:mk_alu(BaseTmp2, Base, add, OffsetTmp2),
   hipe_rtl:mk_move(BothOffset, hipe_rtl:mk_imm(0))].

copy_int_little(Base, Offset, NewOffset, Size, Tmp1) when is_integer(Size) ->
  Tmp2 = hipe_rtl:mk_new_reg(),
  TmpOffset = hipe_rtl:mk_new_reg(),
  ByteSize = Size div ?BYTE_SIZE, 
    [hipe_rtl:mk_alu(TmpOffset, Offset, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
     hipe_rtl:mk_alu(Tmp2, hipe_rtl:mk_imm(ByteSize), 'add', TmpOffset)] ++
    
    little_loop(Tmp1, Tmp2, TmpOffset, Base) ++
    
    case Size band ?LOW_BITS of
      0 ->
	[hipe_rtl:mk_alu(NewOffset, Offset, 'add', hipe_rtl:mk_imm(Size))];
      Bits ->
	[hipe_rtl:mk_alu(Tmp1, Tmp1, sll, hipe_rtl:mk_imm(?BYTE_SIZE-Bits)),
	 hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
	 hipe_rtl:mk_alu(NewOffset, Offset, 'add', hipe_rtl:mk_imm(Size))]
    end;
    
copy_int_little(Base, Offset, NewOffset, Size, Tmp1) ->
  Tmp2 = hipe_rtl:mk_new_reg(),
  Tmp3 = hipe_rtl:mk_new_reg(),
  Tmp4 = hipe_rtl:mk_new_reg(),
  TmpOffset = hipe_rtl:mk_new_reg(), 
  
    [hipe_rtl:mk_alu(Tmp2, Size, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
     hipe_rtl:mk_alu(TmpOffset, Offset, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
     hipe_rtl:mk_alu(Tmp3, Tmp2, 'add', TmpOffset)] ++
    
    little_loop(Tmp1, Tmp3, TmpOffset, Base) ++
  
    [hipe_rtl:mk_alu(Tmp4, Size, 'and', hipe_rtl:mk_imm(?LOW_BITS)),
     hipe_rtl:mk_alu(Tmp4, hipe_rtl:mk_imm(?BYTE_SIZE), 'sub', Tmp4), 
     hipe_rtl:mk_alu(Tmp1, Tmp1, sll, Tmp4),
     hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
     hipe_rtl:mk_alu(NewOffset, Offset, 'add', Size)].

little_loop(Tmp1, Tmp3, TmpOffset, Base) ->
  BranchLbl =  hipe_rtl:mk_new_label(),
  BodyLbl = hipe_rtl:mk_new_label(),
  EndLbl = hipe_rtl:mk_new_label(), 
  [BranchLbl,
   hipe_rtl:mk_branch(TmpOffset, ne, Tmp3, hipe_rtl:label_name(BodyLbl), hipe_rtl:label_name(EndLbl)),
   BodyLbl,
   hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
   hipe_rtl:mk_alu(Tmp1, Tmp1, sra, hipe_rtl:mk_imm(?BYTE_SIZE)),
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
   hipe_rtl:mk_alu(Tmp1, Tmp1, sra, hipe_rtl:mk_imm(?BYTE_SIZE)),
   hipe_rtl:mk_goto(hipe_rtl:label_name(BranchLbl)),
   EndLbl].

copy_int_big(_Base, Offset, NewOffset, 0, _Tmp1) ->
  [hipe_rtl:mk_move(NewOffset, Offset)];
copy_int_big(Base, Offset, NewOffset, ?BYTE_SIZE, Tmp1) ->
  TmpOffset = hipe_rtl:mk_new_reg(),
    [hipe_rtl:mk_alu(TmpOffset, Offset, srl, hipe_rtl:mk_imm(3)),
     hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
     hipe_rtl:mk_alu(NewOffset, Offset, add, hipe_rtl:mk_imm(8))];
copy_int_big(Base, Offset, NewOffset, 2*?BYTE_SIZE, Tmp1) ->
  TmpOffset = hipe_rtl:mk_new_reg(),
    [hipe_rtl:mk_alu(TmpOffset, Offset, srl, hipe_rtl:mk_imm(3)),
     hipe_rtl:mk_alu(TmpOffset, TmpOffset, add, hipe_rtl:mk_imm(1)),
     hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
     hipe_rtl:mk_alu(TmpOffset, TmpOffset, sub, hipe_rtl:mk_imm(1)),
     hipe_rtl:mk_alu(Tmp1, Tmp1, sra, hipe_rtl:mk_imm(8)),
     hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
     hipe_rtl:mk_alu(NewOffset, Offset, add, hipe_rtl:mk_imm(16))];
copy_int_big(Base, Offset, NewOffset, 3*?BYTE_SIZE, Tmp1) ->
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
copy_int_big(Base, Offset,NewOffset, 4*?BYTE_SIZE, Tmp1) ->
  copy_big_word(Base, Offset, NewOffset, Tmp1);
copy_int_big(Base, Offset, NewOffset, Size, Tmp1) when is_integer(Size) ->
  OldOffset = hipe_rtl:mk_new_reg(),
  TmpOffset = hipe_rtl:mk_new_reg(),
  Bits = hipe_rtl:mk_new_reg(),
  ByteSize = (Size + 7) div ?BYTE_SIZE,
  case Size band 7 of 
    0 -> 
      [hipe_rtl:mk_alu(OldOffset, Offset, sra, hipe_rtl:mk_imm(3)),
       hipe_rtl:mk_alu(TmpOffset, OldOffset, add, hipe_rtl:mk_imm(ByteSize))];
    
    Rest ->
      [hipe_rtl:mk_alu(OldOffset, Offset, sra, hipe_rtl:mk_imm(3)),
       hipe_rtl:mk_alu(TmpOffset, OldOffset, add, hipe_rtl:mk_imm(ByteSize-1)),
       hipe_rtl:mk_alu(Bits, Tmp1, sll, hipe_rtl:mk_imm(?BYTE_SIZE-Rest)),
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
  EvenLbl = hipe_rtl:mk_new_label(),
  OddLbl = hipe_rtl:mk_new_label(),
  [hipe_rtl:mk_alu(Tmp2, Size, srl, hipe_rtl:mk_imm(3)),
   hipe_rtl:mk_alu(Tmp3, Offset, srl, hipe_rtl:mk_imm(3)),
   hipe_rtl:mk_alu(TmpOffset, Tmp2, 'add', Tmp3),
   hipe_rtl:mk_alub(Tmp4, Size, 'and', hipe_rtl:mk_imm(7), eq, 
		    hipe_rtl:label_name(EvenLbl), hipe_rtl:label_name(OddLbl)),
   OddLbl,
   hipe_rtl:mk_alu(Tmp6, hipe_rtl:mk_imm(8), sub, Tmp4),
   hipe_rtl:mk_alu(Tmp5, Tmp1, sll, Tmp6),
   hipe_rtl:mk_store(Base, TmpOffset, Tmp5, byte),
   EvenLbl,
   hipe_rtl:mk_alu(Tmp1, Tmp1, srl, Tmp4)] ++
    
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
  [hipe_rtl:mk_alu(TmpOffset, Offset, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
   hipe_rtl:mk_store(Base, TmpOffset, Word, byte),
   hipe_rtl:mk_alu(TmpOffset, TmpOffset, add, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(Word, Word, sra, hipe_rtl:mk_imm(?BYTE_SIZE)),
   hipe_rtl:mk_store(Base, TmpOffset, Word, byte),
   hipe_rtl:mk_alu(TmpOffset, TmpOffset, add, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(Word, Word, sra, hipe_rtl:mk_imm(?BYTE_SIZE)),
   hipe_rtl:mk_store(Base, TmpOffset, Word, byte),
   hipe_rtl:mk_alu(TmpOffset, TmpOffset, add, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(Word, Word, sra, hipe_rtl:mk_imm(?BYTE_SIZE)),
   hipe_rtl:mk_store(Base, TmpOffset, Word, byte),
   hipe_rtl:mk_alu(NewOffset, Offset, add, hipe_rtl:mk_imm(32))].

copy_offset_int_big(Base, Offset, NewOffset, Size, Tmp1) when is_integer(Size) ->
  Tmp2 = hipe_rtl:mk_new_reg(),
  Tmp3 = hipe_rtl:mk_new_reg(),
  Tmp4 = hipe_rtl:mk_new_reg(),
  Tmp5 = hipe_rtl:mk_new_reg(),
  Tmp6 = hipe_rtl:mk_new_reg(),
  Tmp7 = hipe_rtl:mk_new_reg(),
  Tmp8 = hipe_rtl:mk_new_reg(),
  Tmp9 = hipe_rtl:mk_new_reg(),
  OldByte = hipe_rtl:mk_new_reg(),
  TmpOffset = hipe_rtl:mk_new_reg(),
  BranchLbl =  hipe_rtl:mk_new_label(),
  BodyLbl = hipe_rtl:mk_new_label(),
  EndLbl = hipe_rtl:mk_new_label(),
  NextLbl = hipe_rtl:mk_new_label(),
  WordSize = hipe_rtl_arch:word_size(),
  [hipe_rtl:mk_alu(Tmp2, Offset, 'and', hipe_rtl:mk_imm(?LOW_BITS)),
   hipe_rtl:mk_alu(Tmp3, Offset, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
   hipe_rtl:mk_alu(NewOffset, Offset, 'add', hipe_rtl:mk_imm(Size)),
   hipe_rtl:mk_alu(Tmp9, NewOffset, 'sub', hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_alu(TmpOffset, Tmp9, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
   hipe_rtl:mk_alu(Tmp4, NewOffset, 'and', hipe_rtl:mk_imm(?LOW_BITS)),
   hipe_rtl:mk_alu(Tmp6, hipe_rtl:mk_imm(?BYTE_SIZE), sub, Tmp4),
   hipe_rtl:mk_alu(Tmp6, Tmp6, 'and', hipe_rtl:mk_imm(?LOW_BITS)),
   hipe_rtl:mk_alu(Tmp4, hipe_rtl:mk_imm(?BYTE_SIZE), sub, Tmp6),
   hipe_rtl:mk_move(Tmp5, Tmp1),
   hipe_rtl:mk_alu(Tmp1, Tmp1, sll, Tmp6), 
   hipe_rtl:mk_branch(TmpOffset, ne, Tmp3, hipe_rtl:label_name(NextLbl), 
		      hipe_rtl:label_name(EndLbl)),
   NextLbl,
   hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
   hipe_rtl:mk_move(Tmp1, Tmp5),
   hipe_rtl:mk_alu(Tmp1, Tmp1, sra, Tmp4),
   hipe_rtl:mk_alu(TmpOffset, TmpOffset, sub, hipe_rtl:mk_imm(1)),
   BranchLbl,
   hipe_rtl:mk_branch(TmpOffset, ne, Tmp3, hipe_rtl:label_name(BodyLbl), hipe_rtl:label_name(EndLbl)),
   BodyLbl,
   hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte),
   hipe_rtl:mk_alu(Tmp1, Tmp1, sra, hipe_rtl:mk_imm(?BYTE_SIZE)),
   hipe_rtl:mk_alu(TmpOffset, TmpOffset, sub, hipe_rtl:mk_imm(1)),
   hipe_rtl:mk_goto(hipe_rtl:label_name(BranchLbl)),
   EndLbl,
   hipe_rtl:mk_load(OldByte, Base, TmpOffset, byte, unsigned),
   hipe_rtl:mk_alu(Tmp8, hipe_rtl:mk_imm(?BYTE_SIZE), 'sub', Tmp2),
   hipe_rtl:mk_alu(OldByte, OldByte, srl, Tmp8),
   hipe_rtl:mk_alu(OldByte, OldByte, sll, Tmp8),
   hipe_rtl:mk_alu(Tmp7, Tmp2, 'add',
		   hipe_rtl:mk_imm(?bytes_to_bits(WordSize-1))),
   hipe_rtl:mk_alu(Tmp1, Tmp1, sll, Tmp7),
   hipe_rtl:mk_alu(Tmp1, Tmp1, srl, Tmp7),
   hipe_rtl:mk_alu(Tmp1, Tmp1, 'or', OldByte),
   hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte)].

copy_float_little(_Base, _Offset, _NewOffset, _Src, FalseLblName, _TrueLblName, fail) ->
  [hipe_rtl:mk_goto(FalseLblName)];
copy_float_little(Base, Offset, NewOffset, Src, _FalseLblName, TrueLblName, pass) ->
  FloatLo = hipe_rtl:mk_new_reg(),
  FloatHi = hipe_rtl:mk_new_reg(),
  TmpOffset = hipe_rtl:mk_new_reg(),
   hipe_tagscheme:unsafe_load_float(FloatLo, FloatHi, Src) ++
    copy_little_word(Base, Offset, TmpOffset, FloatLo) ++
    copy_little_word(Base, TmpOffset, NewOffset, FloatHi) ++
    [hipe_rtl:mk_goto(TrueLblName)];
copy_float_little(Base, Offset, NewOffset, Src, FalseLblName, TrueLblName, var) ->
  SuccessLbl = hipe_rtl:mk_new_label(),
  hipe_tagscheme:test_flonum(Src, hipe_rtl:label_name(SuccessLbl), FalseLblName, 0.99) ++
    [SuccessLbl|copy_float_little(Base, Offset, NewOffset, Src, FalseLblName, TrueLblName, pass)].

copy_float_big(_Base, _Offset, _NewOffset, _Src, FalseLblName, _TrueLblName, fail) ->
  [hipe_rtl:mk_goto(FalseLblName)];
copy_float_big(Base, Offset, NewOffset, Src, _FalseLblName, TrueLblName,pass) ->
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

multiply_code([ShiftSize|Rest], Register, Result, FalseLblName, Tmp1, OldCode) ->
  SuccessLbl = hipe_rtl:mk_new_label(),
  Code = OldCode ++ [hipe_rtl:mk_alu(Tmp1, Register, sll, hipe_rtl:mk_imm(ShiftSize)),
		     hipe_rtl:mk_alub(Result, Tmp1, 'add', Result, not_overflow, hipe_rtl:label_name(SuccessLbl), FalseLblName, 0.99),
		     SuccessLbl],
  multiply_code(Rest, Register, Result, FalseLblName, Tmp1, Code);
multiply_code([], _Register, _Result, _FalseLblName, _Tmp1, Code) ->
  Code.

number2list(X) when is_integer(X), X >= 0 ->
  number2list(X, []).

number2list(1, Acc) ->
  lists:reverse([0|Acc]);
number2list(0, Acc) ->
  lists:reverse(Acc);
number2list(X, Acc) ->
  F = floorlog2(X),
  number2list(X-(1 bsl F), [F|Acc]).

floorlog2(X) ->
  round(math:log(X)/math:log(2)-0.5). 

set_high(X) ->
  set_high(X, 0).
set_high(0, Y) ->
  Y;
set_high(X, Y) ->
  set_high(X-1, Y+(1 bsl (27-X))).

first_part(Var, Register, FalseLblName) ->
  [SuccessLbl1, SuccessLbl2] = create_lbls(2),
  [hipe_tagscheme:test_fixnum(Var, hipe_rtl:label_name(SuccessLbl1),
			      FalseLblName, 0.99),
  SuccessLbl1,
  hipe_tagscheme:fixnum_ge(Var, hipe_rtl:mk_imm(hipe_tagscheme:mk_fixnum(0)), 
			   hipe_rtl:label_name(SuccessLbl2), FalseLblName, 0.99),
  SuccessLbl2,
  hipe_tagscheme:untag_fixnum(Register, Var)].

