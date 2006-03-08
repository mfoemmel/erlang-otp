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
%%              $Date: 2005/12/12 23:20:17 $
%%              $Revision: 1.55 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_rtl_inline_bs_ops).
-export([gen_rtl/6]).

%%-------------------------------------------------------------------------

-include("../main/hipe.hrl").
-include("hipe_literals.hrl").

-define(BYTE_SHIFT, 3). %% Turn bits into bytes or vice versa
-define(FIVE_HIGH_BITS, 31 bsl 27).
-define(LOW_BITS, 7). %% Three lowest bits set
-define(BYTE_SIZE, 8).
-define(MAX_SMALL_BITS, 27).
%% -------------------------------------------------------------------------
%% The code is generated as a list of lists, it will be flattened later.
%% 

gen_rtl(BsOP, Args, Dst, TrueLblName, FalseLblName, ConstTab) ->
  %%io:format("~w, ~w, ~w~n", [BsOP, Args, Dst]),
  case BsOP of
    {bs_put_string, String, SizeInBytes} ->
      [NewOffset] = get_real(Dst),
      [Base, Offset] = Args,
      put_string(NewOffset, ConstTab, String, SizeInBytes, Base, Offset, 
		 TrueLblName,false);
      
    {bs_put_string, String, SizeInBytes, Flags} ->
      [NewOffset] = get_real(Dst),
      [Base, Offset] = Args,
      Aligned=aligned(Flags),
      put_string(NewOffset, ConstTab, String, SizeInBytes, 
		 Base, Offset, TrueLblName, Aligned);
    
    _ -> 
      Code = 
	case BsOP of
	  bs_bits_to_bytes ->
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
	  
	  {bs_add, Const, Unit} ->
	    Raw = hipe_rtl:mk_imm(Const),
	    Tmp1 = hipe_rtl:mk_new_reg(),
	    Tmp2 = hipe_rtl:mk_new_reg(),
	    [New]=Args,
	    [Dst0]=Dst,
	    {SizeCode, SizeReg} = hipe_rtl_bin_util:make_size(Unit, New, FalseLblName),
	    SizeCode ++
	      [hipe_rtl:mk_alu(Tmp1, SizeReg, add, Raw),
	       hipe_tagscheme:tag_fixnum(Dst0, Tmp1),
	       hipe_tagscheme:untag_fixnum(Tmp2, Dst0),
	       hipe_rtl:mk_branch(Tmp1, eq, Tmp2, TrueLblName, FalseLblName)];
	  
	  {bs_add, Unit} ->
	    Raw = hipe_rtl:mk_new_reg(),
	    Tmp1 = hipe_rtl:mk_new_reg(),
	    Tmp2 = hipe_rtl:mk_new_reg(),
	    [Old, New]=Args,
	    [Dst0]=Dst,
	    {SizeCode, SizeReg} = hipe_rtl_bin_util:make_size(Unit, New, FalseLblName),
	    SizeCode ++
	      [hipe_tagscheme:untag_fixnum(Raw, Old),
	       hipe_rtl:mk_alu(Tmp1, SizeReg, add, Raw),
	       hipe_tagscheme:tag_fixnum(Dst0, Tmp1),
	       hipe_tagscheme:untag_fixnum(Tmp2, Dst0),
	       hipe_rtl:mk_branch(Tmp1, eq, Tmp2, TrueLblName, FalseLblName)];

	  {bs_init, fail} ->
	    [hipe_rtl:mk_goto(FalseLblName)];
	  
	  {bs_init, {Const, Units}} ->
	    init(Dst, Const, Units, Args, FalseLblName, TrueLblName);
	  
	  {bs_init2, Size, _Flags} -> 
	    [] = Args,
	    [Dst0, Base, Offset] = Dst,
	    const_init2(Size, Dst0, Base, Offset, TrueLblName, FalseLblName);
	  
	  {bs_init2, _Flags} -> 
	    [Size] = Args,
	    [Dst0, Base, Offset] = Dst,
	    var_init2(Size, Dst0, Base, Offset, TrueLblName, FalseLblName);

	  {bs_create_space, Size, Shifts} ->
	    create_space(Size, Shifts, Args, TrueLblName);
					
	  bs_start_match ->
	    [BinSize, Base, Offset, Orig, OrigOffset] = Dst,
	    [Binary] = Args,
	     get_binary_bytes(Binary, BinSize, Base, Offset, 
					       Orig, OrigOffset, TrueLblName, 
					       FalseLblName);
	   
	  {bs_put_binary_all, Flags} ->
	    Aligned=aligned(Flags),
	    [Src, Base, Offset] = Args,
	    [NewOffset] = get_real(Dst),
	    put_binary_all(NewOffset, Src, Base, Offset, Aligned, 
					    TrueLblName, FalseLblName);
	   
	  {bs_put_binary, Size, Flags} ->
	    Aligned = aligned(Flags),
	    [NewOffset] = get_real(Dst),
	    case Args of
	      [Src, Base, Offset] ->
		put_static_binary(NewOffset, Src, Base, Offset, Size, Aligned, 
					    TrueLblName, FalseLblName);
	      [Src, Bits, Base, Offset]  ->
		{SizeCode, SizeReg} = hipe_rtl_bin_util:make_size(Size, Bits, FalseLblName),
		InCode = put_dynamic_binary(NewOffset, Src, SizeReg, Base, 
					    Offset, Aligned, TrueLblName,
					    FalseLblName),
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
		{SizeCode, SizeReg} = hipe_rtl_bin_util:make_size(Size, Bits, FalseLblName),
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
		    {SizeCode, SizeReg} = hipe_rtl_bin_util:make_size(Size, Bits, FalseLblName),
		    CCode = int_c_code(NewOffset, Src, Base, Offset, SizeReg, Flags, 
					      TrueLblName, FalseLblName),
		    InCode = 
		      put_dynamic_int(NewOffset, Src, Base, Offset, 
				      SizeReg, CCode, Aligned, 
				      LittleEndian, TrueLblName),
		    SizeCode ++ InCode
		end
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
		    {SizeCode, SizeReg} = hipe_rtl_bin_util:make_size(Size, Bits, FalseLblName),
		    CCode = int_c_code(NewOffset, Src, Base, Offset, SizeReg, Flags, 
					      TrueLblName, FalseLblName),
		    InCode = 
		      put_unsafe_dynamic_int(NewOffset, Src, Base, Offset, 
					     SizeReg, CCode, Aligned, 
					     LittleEndian, TrueLblName),
		    SizeCode ++ InCode
		end
	    end; 
	  
	  %% Inlined for all possible cases
	  {bs_skip_bits_all, Flags} ->
	    Aligned = aligned(Flags),
	    [NewOffset] = Dst,
	    [BinSize, Offset]= Args,
	    skip_bits_all(NewOffset, Offset, BinSize, Aligned, 
					   TrueLblName, FalseLblName);

	  %% Inlined for all possible cases
	  {bs_skip_bits, Bits} ->
	    [NewOffset] = Dst,
	    case Args of
	      [BinSize, Offset] ->
		skip_bits(BinSize, Offset, NewOffset,  
					   hipe_rtl:mk_imm(Bits), TrueLblName, 
					   FalseLblName);
	      [Arg, BinSize, Offset] ->
		{SizeCode, SizeReg} = hipe_rtl_bin_util:make_size(Bits, Arg, FalseLblName),
		InCode = skip_bits(BinSize, Offset, NewOffset,
						    SizeReg, TrueLblName, 
						    FalseLblName),
		SizeCode ++ InCode
	    end;

	  {bs_get_integer,0,_Flags} ->
	    [Dst1, NewOffset] = Dst,
	    [_BinSize, _Base, Offset, _Orig]= Args,
 
	    [hipe_rtl:mk_move(NewOffset, Offset),
	     hipe_rtl:mk_move(Dst1, hipe_rtl:mk_imm(15)),
	     hipe_rtl:mk_goto(TrueLblName)];

	  %% Inlined when Size is const or variable smaller than 28 and the normalendianess 
	  {bs_get_integer,Size,Flags} ->
	    [Dst1, NewOffset] = Dst,
	    Signed = signed(Flags),
	    LittleEndian = littleendian(Flags),
	    Aligned = aligned(Flags),
	    case Args of
	      [BinSize, Base, Offset, Orig] ->
		CCode = static_int_get_c_code(Dst1, BinSize, Base, Offset, NewOffset, Orig, Size, Flags, 
					      TrueLblName, FalseLblName),
		get_static_int(Dst1, NewOffset, BinSize, Base, Offset, Size, CCode,
			       Signed, LittleEndian, Aligned, TrueLblName, 
			       FalseLblName);
	      

	      [Arg, BinSize, Base, Offset, Orig] ->
		{SizeCode, SizeReg} = hipe_rtl_bin_util:make_size(Size, Arg, FalseLblName),
		CCode = int_get_c_code(Dst1, BinSize, Base, Offset, NewOffset, 
				       Orig, SizeReg, Flags, 
				       TrueLblName, FalseLblName),
		InCode=get_dynamic_int(Dst1, NewOffset, BinSize, Base, Offset,
				       SizeReg, CCode, Signed, LittleEndian, 
				       Aligned, TrueLblName, FalseLblName),
		SizeCode ++ InCode
	    end;
		
	  %% Inlined when float size is 64 and binary is byte-aligned
	  {bs_get_float,Size,Flags} ->
	    LittleEndian = littleendian(Flags),
	    Aligned = aligned(Flags),
	    [Dst1, NewOffset] = Dst,
	    case Args of
	      [BinSize, Base, Offset, Orig] ->
		CCode = static_float_get_c_code(Dst1, BinSize, Base, Offset, NewOffset, Orig, Size, Flags, 
						TrueLblName, FalseLblName),
		get_static_float(Dst1, NewOffset, BinSize, Base, 
						  Offset, Size, CCode, 
						  LittleEndian, Aligned, 
						  TrueLblName, FalseLblName);
	      [Arg, BinSize, Base, Offset, Orig]  ->
		{SizeCode, SizeReg} = hipe_rtl_bin_util:make_size(Size, Arg, FalseLblName),
		CCode = float_get_c_code(Dst1, BinSize, Base, Offset, NewOffset,
					 Orig, SizeReg, Flags, TrueLblName, FalseLblName),
		InCode=get_dynamic_float(Dst1, NewOffset, BinSize, Base, Offset, 
					 SizeReg, CCode, LittleEndian, 
					 Aligned, TrueLblName, FalseLblName),
		SizeCode ++ InCode
	    end;

	  %% Inlined for all cases
	  {bs_get_binary_all, Flags} -> 
	    Aligned = aligned(Flags),
	    [Dst1, NewOffset] = Dst,
	    [BinSize, Offset, Orig, OrigOffset] = Args,
	    get_binary_all(Dst1, NewOffset, BinSize, Offset, Orig, 
					    OrigOffset, Aligned, TrueLblName, 
					    FalseLblName);

	  %% Inlined when aligned
	  {bs_get_binary,Size,Flags} ->
	    Aligned = aligned(Flags),
	    [Dst1, NewOffset] = Dst,
	    case Args of
		[BinSize, Base, Offset, Orig, OrigOffset] ->
		  SizeReg = hipe_rtl:mk_new_reg(),
		  SizeCode = [hipe_rtl:mk_move(SizeReg, hipe_rtl:mk_imm(Size))];
		[BitsVar, BinSize, Base, Offset, Orig, OrigOffset]  ->
		  {SizeCode, SizeReg} = hipe_rtl_bin_util:make_size(Size, BitsVar, FalseLblName)
	      end,
	    CCode = binary_get_c_code(Dst1, BinSize, Base, Offset, NewOffset, 
				      Orig, SizeReg, Flags, TrueLblName, 
				      FalseLblName),
	    InCode = get_binary(Dst1, NewOffset, SizeReg, BinSize, 
						 Offset, Orig, OrigOffset, CCode, 
						 Aligned, TrueLblName, FalseLblName),
	    SizeCode ++ InCode;
	
	  %% Inlined for all cases
	  {bs_test_tail, NumBits} ->
	    [BinSize, Offset] = Args,
	    [hipe_rtl:mk_alu(Offset, Offset, add, hipe_rtl:mk_imm(NumBits)),
	     hipe_rtl:mk_branch(Offset, eq, BinSize, TrueLblName, FalseLblName)];

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Code that is used to create calls to beam functions
%%
%%  X_c_code/8, used for putting terms into binaries
%%
%%  X_get_c_code/10, used for getting terms from binaries
%%
%% - gen_bs_call/10 is used to call C-functions that return an Erlang
%%       value; it also makes sure to update the offset before calling
%%       and to reload the match buffer after the call.
%%
%% - gen_test_sideffect_bs_call/4 is used to make a C-call that might
%%       fail but doesn't return an erlang value.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

static_int_get_c_code(Dst1, BinSize, Base, Offset, NewOffset, Orig, Size, Flags, 
		      TrueLblName, FalseLblName) ->
  SizeReg = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_move(SizeReg, hipe_rtl:mk_imm(Size))|
   int_get_c_code(Dst1, BinSize, Base, Offset, NewOffset, Orig, SizeReg, Flags, 
		  TrueLblName, FalseLblName)].

int_get_c_code(Dst1, BinSize, Base, Offset, NewOffset, Orig, SizeReg, Flags, 
	       TrueLblName, FalseLblName) ->
  get_c_code(bs_get_integer, Dst1, BinSize, Base, Offset, NewOffset, Orig, SizeReg, Flags, 
	   TrueLblName, FalseLblName).

static_float_get_c_code(Dst1, BinSize, Base, Offset, NewOffset, Orig, Size, Flags, 
		      TrueLblName, FalseLblName) ->
  SizeReg = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_move(SizeReg, hipe_rtl:mk_imm(Size))|
   float_get_c_code(Dst1, BinSize, Base, Offset, NewOffset, Orig, SizeReg, Flags, 
		  TrueLblName, FalseLblName)].

float_get_c_code(Dst1, BinSize, Base, Offset, NewOffset, Orig, SizeReg, Flags, 
	       TrueLblName, FalseLblName) ->
  get_c_code(bs_get_float, Dst1, BinSize, Base, Offset, NewOffset, Orig, SizeReg, Flags, 
	   TrueLblName, FalseLblName).

binary_get_c_code(Dst1, BinSize, Base, Offset, NewOffset, Orig, SizeReg, Flags,
		  TrueLblName, FalseLblName) ->
  get_c_code(bs_get_binary,Dst1, BinSize, Base, Offset, NewOffset, Orig, SizeReg, Flags, 
	     TrueLblName, FalseLblName). 

get_c_code(Func, Dst1, BinSize, Base, Offset, NewOffset, Orig, SizeReg, Flags, 
	   TrueLblName, FalseLblName) ->  
  Tmp2 = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flags)),
   gen_bs_call(Func,
	       [SizeReg, Tmp2],
	       Dst1, BinSize, Base, Offset, NewOffset, Orig, 
	       TrueLblName, FalseLblName)].

gen_bs_call(Name, Args = [SizeReg|_], DstVar, BinSize, Base, Offset,
	    NewOffset, Orig, TrueLblName,FalseLblName) ->
  RetLbl =  hipe_rtl:mk_new_label(),
  OkLbl =  hipe_rtl:mk_new_label(),
  NonVal = hipe_rtl:mk_imm(hipe_tagscheme:mk_non_value()),
  [hipe_rtl_bin_util:bs_call(Name, Args, DstVar, BinSize, Base, Offset, 
			     Orig, hipe_rtl:label_name(RetLbl), 
			     FalseLblName),
   RetLbl,
   hipe_rtl:mk_branch(DstVar, eq, NonVal, FalseLblName, 
		      hipe_rtl:label_name(OkLbl), 0.01),
   OkLbl,
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

create_regs(0) ->
  [];
create_regs(X) when X > 0->
  [hipe_rtl:mk_new_reg()|create_regs(X-1)].

create_lbls(0) ->
  [];
create_lbls(X) when X > 0->
  [hipe_rtl:mk_new_label()|create_lbls(X-1)].

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

get_real([]) ->
  [hipe_rtl:mk_new_reg()];
get_real([NewOffset]) ->
  [NewOffset].




%%---------------------------------------------------------------------------------
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
%%---------------------------------------------------------------------------------



put_string(NewOffset, ConstTab, String, SizeInBytes, Base, Offset, TLName, 
	   Aligned) ->
  [StringBase] = create_regs(1),
  {NewTab, Lbl} = 
    hipe_consttab:insert_block(ConstTab, byte, String),
  case Aligned of
    true ->
      {[hipe_rtl:mk_load_address(StringBase, Lbl, constant)|
	copy_aligned_string(StringBase, SizeInBytes, Base, 
			    Offset, NewOffset, TLName)],
       NewTab};
    false ->
      {[hipe_rtl:mk_load_address(StringBase, Lbl, constant)|
	copy_string(StringBase, SizeInBytes, Base, Offset, 
		    NewOffset, TLName)],
       NewTab}
  end.
							  
const_init2(Size, Dst, Base, Offset, TrueLblName, _FalseLblName) ->
  Log2WordSize=hipe_rtl_arch:log2_word_size(),
  WordSize=hipe_rtl_arch:word_size(),
  NextLbl = hipe_rtl:mk_new_label(),
  case Size =< ?MAX_HEAP_BIN_SIZE of
    true ->
      [hipe_rtl:mk_gctest(((Size + 3*WordSize-1) bsr Log2WordSize)),
       hipe_tagscheme:create_heap_binary(Base, Size, Dst),
       hipe_rtl:mk_move(Offset, hipe_rtl:mk_imm(0)),
       hipe_rtl:mk_goto(TrueLblName)];
    false ->
      ByteSize = hipe_rtl:mk_new_reg(),
      [hipe_rtl:mk_gctest(?PROC_BIN_WORDSIZE),
       hipe_rtl:mk_move(Offset, hipe_rtl:mk_imm(0)),
       hipe_rtl:mk_move(ByteSize, hipe_rtl:mk_imm(Size)),
       hipe_rtl:mk_call([Base], bs_allocate, [ByteSize],
			hipe_rtl:label_name(NextLbl),[],not_remote),
       NextLbl,
       hipe_tagscheme:create_refc_binary(Base, ByteSize, Dst),
       hipe_rtl:mk_goto(TrueLblName)]
  end.

var_init2(Size, Dst, Base, Offset, TrueLblName, _FalseLblName) ->
  Log2WordSize=hipe_rtl_arch:log2_word_size(),
  WordSize=hipe_rtl_arch:word_size(),
  HeapLbl = hipe_rtl:mk_new_label(),
  REFCLbl = hipe_rtl:mk_new_label(),
  NextLbl = hipe_rtl:mk_new_label(),
  USize=hipe_rtl:mk_new_reg(),
  Tmp=hipe_rtl:mk_new_reg(),
  [hipe_tagscheme:untag_fixnum(USize, Size),
   hipe_rtl:mk_move(Offset, hipe_rtl:mk_imm(0)),
   hipe_rtl:mk_branch(USize, le, hipe_rtl:mk_imm(?MAX_HEAP_BIN_SIZE), 
		      hipe_rtl:label_name(HeapLbl), 
		      hipe_rtl:label_name(REFCLbl)),
   HeapLbl,
   hipe_rtl:mk_alu(Tmp, USize, add, hipe_rtl:mk_imm(3*WordSize-1)),
   hipe_rtl:mk_alu(Tmp, Tmp, srl, hipe_rtl:mk_imm(Log2WordSize)),
   hipe_rtl:mk_gctest(Tmp),
   hipe_tagscheme:untag_fixnum(USize, Size),
   hipe_tagscheme:create_heap_binary(Base, USize, Dst),
   hipe_rtl:mk_goto(TrueLblName),
   REFCLbl,
   hipe_rtl:mk_gctest(?PROC_BIN_WORDSIZE),
   hipe_tagscheme:untag_fixnum(USize, Size),
   hipe_rtl:mk_call([Base], bs_allocate, [USize],
		    hipe_rtl:label_name(NextLbl),[],not_remote),
   NextLbl,
   hipe_tagscheme:create_refc_binary(Base, USize, Dst),
   hipe_rtl:mk_goto(TrueLblName)].

init(Dst, Const, Units, Args, FLName, TLName) ->
  SaveVar = hipe_rtl:mk_new_var(),
  WordSize = hipe_rtl_arch:word_size(),
  [Size, Tmp1, ByteSize, NumberOfBytes, NumberOfWords] = create_regs(5),
  [SuccessLbl, HeapLbl, REFCLbl] = create_lbls(3),
  [Base, Offset] = Dst,
  calculate_size(Size, Const, Units, Args, FLName) ++ 
    [hipe_rtl:mk_alub(Tmp1, Size, 'and', hipe_rtl:mk_imm(?LOW_BITS), eq, 
		      hipe_rtl:label_name(SuccessLbl), FLName, 0.99),
     SuccessLbl,
     hipe_rtl:mk_alu(ByteSize, Size, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
     hipe_tagscheme:tag_fixnum(SaveVar, ByteSize),
     hipe_rtl:mk_branch(ByteSize, le, hipe_rtl:mk_imm(?MAX_HEAP_BIN_SIZE), 
			hipe_rtl:label_name(HeapLbl), hipe_rtl:label_name(REFCLbl), 0.50),
     HeapLbl,
     hipe_rtl:mk_alu(NumberOfBytes, ByteSize, 'add', hipe_rtl:mk_imm(3*WordSize-1)),  %%Add 11 to ensure that everyrthing has room
     hipe_rtl:mk_alu(NumberOfWords, NumberOfBytes, sra, 
		     hipe_rtl:mk_imm(hipe_rtl_arch:log2_word_size())),
     hipe_rtl:mk_gctest(NumberOfWords),
     hipe_tagscheme:untag_fixnum(ByteSize, SaveVar),
     hipe_tagscheme:get_base(Base, ByteSize),
     hipe_rtl:mk_move(Offset, hipe_rtl:mk_imm(0)),
     hipe_rtl:mk_goto(TLName),
     REFCLbl,
     hipe_rtl:mk_gctest(?PROC_BIN_WORDSIZE),
     hipe_tagscheme:untag_fixnum(ByteSize, SaveVar),
     hipe_rtl:mk_move(Offset, hipe_rtl:mk_imm(0)),
     hipe_rtl:mk_call([Base], bs_allocate, [ByteSize],
		      TLName,[],not_remote)].

create_space(Size, Shifts, Args, TLName) ->
  [NumberOfWords, Scratch] = create_regs(2),
  [hipe_rtl:mk_move(NumberOfWords, hipe_rtl:mk_imm(Size))]
    ++ expand_runtime(Shifts, Args, NumberOfWords, Scratch)
    ++ [hipe_rtl:mk_gctest(NumberOfWords),
	hipe_rtl:mk_goto(TLName)].

put_binary_all(NewOffset, Src, Base, Offset, Aligned, TLName, FLName) ->
  [Orig, OrigOffset, CopyBase, CopyOffset, CopyEnd, CopySize, NewSize] = create_regs(7),
  [NextLbl] = create_lbls(1),
  get_binary_bytes(Src, CopyEnd, CopyBase, CopyOffset, Orig, OrigOffset,
		   hipe_rtl:label_name(NextLbl), FLName) ++
    [NextLbl,
     hipe_rtl:mk_alu(CopySize, CopyEnd, sub, CopyOffset)] ++
  case Aligned of
    true ->
	copy_aligned_bytes_all(CopyBase, CopyOffset, CopySize, Base, Offset, 
			       NewOffset, TLName);
    false ->
	[hipe_rtl:mk_move(NewSize, CopySize)|
	 copy_bytes(CopyBase, CopyOffset, CopySize, Base, Offset, NewOffset, 
		    NewSize, TLName, FLName)]
  end.

put_static_binary(NewOffset, Src, Base, Offset, Size, Aligned, TLName, FLName) ->
  [Orig, OrigOffset, CopyBase, CopyOffset, CopySize] = create_regs(5),
  [NextLbl] = create_lbls(1),
  get_binary_bytes(Src, CopySize, CopyBase, CopyOffset, Orig, OrigOffset,
		   hipe_rtl:label_name(NextLbl), FLName) ++
    [NextLbl] ++
    case Size of 
       0 ->
	 [hipe_rtl:mk_move(NewOffset, Offset), 
	  hipe_rtl:mk_goto(TLName)];
       _ ->
	 case Size band ?LOW_BITS of
	   0 ->
	     case Aligned of
	       true -> 
		 copy_aligned_static_bytes(CopyBase, CopyOffset, CopySize, Base, 
					     Offset, NewOffset, Size, TLName, 
					     FLName);
	       false ->
		 copy_static_bytes(CopyBase, CopyOffset, CopySize, Base, Offset, 
				     NewOffset, Size, TLName, FLName)
	     end;
	   _ ->
	     [hipe_rtl:mk_goto(FLName)]
	 end
     end.

put_dynamic_binary(NewOffset, Src, SizeReg, Base, Offset, Aligned, 
		   TLName, FLName) ->
  [Orig, OrigOffset, CopyBase, CopyOffset, CopySize] = create_regs(5),
  [NextLbl] = create_lbls(1),
  get_binary_bytes(Src, CopySize, CopyBase, CopyOffset, Orig, OrigOffset,
				   hipe_rtl:label_name(NextLbl), FLName) ++	
		  [NextLbl] ++
    case Aligned of
      true -> 
	copy_aligned_bytes(CopyBase, CopyOffset, CopySize, Base, Offset, 
			   NewOffset, SizeReg, TLName, FLName);
      false ->
	copy_bytes(CopyBase, CopyOffset, CopySize, Base, Offset, NewOffset, 
		   SizeReg, TLName, FLName)	
    end.

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
    
skip_bits_all(NewOffset, Offset, BinSize, Aligned, TrueLblName, FalseLblName) ->
  case Aligned of
    true ->
      [hipe_rtl:mk_move(NewOffset, BinSize),
       hipe_rtl:mk_goto(TrueLblName)];
    false ->
      [SuccessLbl] = create_lbls(1),
      [hipe_rtl:mk_alub(Offset, Offset, 'and', hipe_rtl:mk_imm(?LOW_BITS), eq, 
			hipe_rtl:label_name(SuccessLbl), FalseLblName, 0.99),
       SuccessLbl,
       hipe_rtl:mk_move(NewOffset, BinSize),
       hipe_rtl:mk_goto(TrueLblName)]
  end.

skip_bits(SizeOfBin, Offset, NewOffset, NoOfBits, TrueLblName, FalseLblName) ->
  SuccessLbl = hipe_rtl:mk_new_label(),
  [hipe_rtl:mk_alub(NewOffset, NoOfBits, add, Offset, overflow, FalseLblName, hipe_rtl:label_name(SuccessLbl),0.01),
   SuccessLbl,
   hipe_rtl:mk_branch(SizeOfBin, lt, NewOffset, FalseLblName, 
		      TrueLblName, 0.01)].

get_static_int(Dst1, NewOffset, BinSize, Base, Offset, Size, CCode, Signed,
	       LittleEndian, Aligned, TrueLblName, FalseLblName) ->
  WordSize = hipe_rtl_arch:word_size(),
  case Size =< WordSize*?BYTE_SIZE of
    true ->
      case {Aligned, LittleEndian} of
	{true, false} ->
	  get_int_from_bin(Size, BinSize, Base, Offset, NewOffset, Dst1, 
			   FalseLblName, TrueLblName, Signed, LittleEndian);
	{true, true} ->
	  case Size rem ?BYTE_SIZE of
	    0 ->
	      get_int_from_bin(Size, BinSize, Base, Offset, NewOffset, Dst1, 
			       FalseLblName, TrueLblName, Signed, LittleEndian);
	    _ ->
	      CCode
	  end;
	{false, false} ->
	  get_int_from_unaligned_bin(Size, BinSize, Base, Offset, NewOffset, Dst1, 
				     FalseLblName, TrueLblName, 
				     Signed);
	{false, true} ->
	  CCode
      end;
    false ->
      CCode
  end.

get_dynamic_int(Dst1, NewOffset, BinSize, Base, Offset, SizeReg, CCode, 
		Signed, LittleEndian, true, TrueLblName, FalseLblName) ->
  {Init, End} = make_dyn_prep(SizeReg, CCode),
  Init ++
    get_unknown_size_int(SizeReg, BinSize, Base, Offset, NewOffset, 
			 Dst1, FalseLblName, TrueLblName, Signed, 
			 LittleEndian) ++
    End;
get_dynamic_int(_Dst1, _NewOffset, _BinSize, _Base, _Offset, _SizeReg, 
		CCode, _Signed, _LittleEndian, false, _TrueLblName, _FalseLblName) ->
  CCode.

%% THIS SHOULD BE DIFFERENT FOR AMD64

get_static_float(Dst1, NewOffset, BinSize, Base, Offset, 64, _CCode, 
		 LittleEndian, true, TrueLblName, FalseLblName) ->
  [Tmp2, Tmp3, ByteOffset] = create_regs(3),
  [SuccessLbl] = create_lbls(1),
  Type = get_type(false, LittleEndian),
  Code1 = [check_size(Offset, hipe_rtl:mk_imm(64), BinSize, NewOffset,
		      hipe_rtl:label_name(SuccessLbl), FalseLblName),
	   SuccessLbl,
	   hipe_rtl:mk_alu(ByteOffset, Offset, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
	   hipe_rtl_bin_util:load_bytes(Tmp2, Base, ByteOffset, Type, 4),
	   hipe_rtl_bin_util:load_bytes(Tmp3, Base, ByteOffset, Type, 4)],
  Code2 = case {LittleEndian, hipe_rtl_arch:endianess()}  of 
	    {false, big} ->
	      [hipe_tagscheme:unsafe_mk_float(Dst1, Tmp2, Tmp3),
	       hipe_rtl:mk_goto(TrueLblName)];
	    {true, big} ->
	      [hipe_tagscheme:unsafe_mk_float(Dst1, Tmp3, Tmp2),
	       hipe_rtl:mk_goto(TrueLblName)];
	    {true, little} ->
	      [hipe_tagscheme:unsafe_mk_float(Dst1, Tmp2, Tmp3),
	       hipe_rtl:mk_goto(TrueLblName)];
	    {false, little} ->
	      [hipe_tagscheme:unsafe_mk_float(Dst1, Tmp3, Tmp2),
	       hipe_rtl:mk_goto(TrueLblName)]
	  end,
  Code1 ++ Code2;
get_static_float(_Dst1, _NewOffset, _BinSize, _Base, _Offset, _Size, CCode, 
		 _LittleEndian, _Aligned, _TrueLblName, _FalseLblName) ->
  CCode.

get_dynamic_float(Dst1, NewOffset, BinSize, Base, Offset, SizeReg, CCode, 
		 LittleEndian, true, TrueLblName, FalseLblName) ->
  [CLbl, SuccessLbl] = create_lbls(2),
  [hipe_rtl:mk_branch(SizeReg, eq, hipe_rtl:mk_imm(64), 
		      hipe_rtl:label_name(SuccessLbl), 
		      hipe_rtl:label_name(CLbl)),
   SuccessLbl] ++
    get_static_float(Dst1, NewOffset, BinSize, Base, Offset, 64, CCode, 
		     LittleEndian, true, TrueLblName, FalseLblName) ++
    [CLbl|CCode];
get_dynamic_float(_Dst1, _NewOffset, _BinSize, _Base, _Offset, _Size, CCode, 
		 _LittleEndian, _Aligned, _TrueLblName, _FalseLblName) ->
  CCode.


get_binary_all(Dst1, NewOffset, BinSize, Offset, Orig, OrigOffset, Aligned, 
	       TrueLblName, FalseLblName) ->
  [Tmp1, Tmp2, Tmp3] = create_regs(3),
  [OkLbl] = create_lbls(1),
  Code1 = case Aligned of
	    true ->
	      [];
	    false ->
	      [hipe_rtl:mk_alub(Tmp1, Offset, 'and', hipe_rtl:mk_imm(?LOW_BITS), eq, hipe_rtl:label_name(OkLbl), FalseLblName, 0.99),
	       OkLbl]
	  end,
  Code2 =
    [hipe_rtl:mk_alu(Tmp2, BinSize, sub, Offset),
     hipe_rtl:mk_alu(Tmp2, Tmp2, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
     hipe_rtl:mk_alu(Tmp1, Offset, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
     hipe_rtl:mk_alu(Tmp3, Tmp1, add, OrigOffset),
     hipe_rtl:mk_move(NewOffset, BinSize),
     hipe_tagscheme:unsafe_mk_sub_binary(Dst1, Tmp2, Tmp3, Orig),
     hipe_rtl:mk_goto(TrueLblName)],
  Code1 ++ Code2.

get_binary(Dst1, NewOffset, SizeReg, BinSize, Offset, Orig, OrigOffset, CCode, 
	   Aligned, TrueLblName, FalseLblName) ->
  [SuccessLbl0,SuccessLbl1,SuccessLbl2,CLbl] = create_lbls(4),
  [Tmp1, Tmp2, RealOffset] = create_regs(3),
  SubCode =
    [hipe_rtl:mk_alub(Tmp2, SizeReg, 'and', hipe_rtl:mk_imm(?LOW_BITS), eq,
		      hipe_rtl:label_name(SuccessLbl1), FalseLblName, 0.99),
     SuccessLbl1,
     check_size(Offset, SizeReg, BinSize, Tmp2, hipe_rtl:label_name(SuccessLbl2), FalseLblName),
     SuccessLbl2,
     hipe_rtl:mk_alu(RealOffset, Offset, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)),
     hipe_rtl:mk_alu(RealOffset, RealOffset, add, OrigOffset),
     hipe_rtl:mk_alu(Tmp1, SizeReg, srl, hipe_rtl:mk_imm(?BYTE_SHIFT))]
    ++ hipe_tagscheme:unsafe_mk_sub_binary(Dst1, Tmp1, RealOffset, Orig)
    ++ [hipe_rtl:mk_move(NewOffset, Tmp2),
	hipe_rtl:mk_goto(TrueLblName)],
  case Aligned of 
    true ->
      SubCode;
    false ->
      [hipe_rtl:mk_alub(Tmp2, Offset, 'and', hipe_rtl:mk_imm(?LOW_BITS), eq, 
			hipe_rtl:label_name(SuccessLbl0), hipe_rtl:label_name(CLbl)),
       SuccessLbl0] ++
	SubCode ++
	[CLbl|CCode]
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Help functions used by the above
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_dyn_prep(SizeReg, CCode) ->   
  [CLbl, SuccessLbl] = create_lbls(2),
  Init = [hipe_rtl:mk_branch(SizeReg, le, hipe_rtl:mk_imm(?MAX_SMALL_BITS),  
			     hipe_rtl:label_name(SuccessLbl), 
			     hipe_rtl:label_name(CLbl)),
	  SuccessLbl],
  End = [CLbl|CCode],
  {Init, End}.


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

get_int_from_bin(Size, BinSize, Base, Offset, NewOffset, Dst1,
		 FalseLblName, TrueLblName, Signed, LittleEndian) ->
  Shiftr = shift_type(Signed),
  Type = get_type(Signed, LittleEndian),
  [SuccessLbl] = create_lbls(1),
  [check_size(Offset, hipe_rtl:mk_imm(Size), BinSize, NewOffset,
	      hipe_rtl:label_name(SuccessLbl), FalseLblName),
   SuccessLbl] ++
    hipe_rtl_bin_util:get_int(Dst1, Size, Base, Offset, 
				       Shiftr, Type, TrueLblName).

get_int_from_unaligned_bin(Size, BinSize, Base, Offset, NewOffset, Dst1, 
			   FalseLblName, TrueLblName, Signed)  ->
  Shiftr = shift_type(Signed),
  Type = get_type(Signed, false),
  [SuccessLbl] = create_lbls(1),
  [check_size(Offset, hipe_rtl:mk_imm(Size), BinSize, NewOffset, 
	      hipe_rtl:label_name(SuccessLbl), FalseLblName),
   SuccessLbl] ++
    hipe_rtl_bin_util:get_unaligned_int(Dst1, Size, Base, Offset, 
				       Shiftr, Type, TrueLblName).
  


get_unknown_size_int(SizeReg, BinSize, Base, Offset, NewOffset, Dst1, 
		     FalseLblName, TrueLblName, Signed, Little) ->
  Shiftr = shift_type(Signed),
  Type = get_type(Signed, false),
  [SuccessLbl] = create_lbls(1),
  [check_size(Offset, SizeReg, BinSize, NewOffset, hipe_rtl:label_name(SuccessLbl), 
	      FalseLblName),
   SuccessLbl] ++
  case Little of
    true ->
      hipe_rtl_bin_util:get_little_unknown_int(Dst1, Base, Offset, NewOffset, 
					      Shiftr, Type, TrueLblName);
    false ->
      hipe_rtl_bin_util:get_big_unknown_int(Dst1, Base, Offset, NewOffset, 
					      Shiftr, Type, TrueLblName)
  end.
  

check_size(Offset, Size, BinSize, Tmp1, ContLblName, FalseLblName) ->
  [hipe_rtl:mk_alu(Tmp1, Offset, add, Size),
   hipe_rtl:mk_branch(Tmp1, leu, BinSize, ContLblName, FalseLblName, 0.99)].

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


get_binsize(BinSize, Binary, FalseLblName) ->
  SuccessLbl=hipe_rtl:mk_new_label(),
  BinHeader=hipe_rtl:mk_new_reg(),
  [hipe_tagscheme:test_binary(Binary, hipe_rtl:label_name(SuccessLbl), FalseLblName, 0.99),
   SuccessLbl,
   hipe_rtl:mk_alu(BinHeader, Binary, sub, hipe_rtl:mk_imm(2)),
   hipe_rtl:mk_load(BinSize, BinHeader, hipe_rtl:mk_imm(?SUB_BIN_BINSIZE)),
   hipe_rtl:mk_alu(BinSize, BinSize, sll, hipe_rtl:mk_imm(?BYTE_SHIFT))].

get_bin_wordsize(BinSize, Binary, FalseLblName) ->
  SuccessLbl=hipe_rtl:mk_new_label(),
  BinHeader=hipe_rtl:mk_new_reg(),
  [hipe_tagscheme:test_binary(Binary, hipe_rtl:label_name(SuccessLbl), FalseLblName, 0.99),
   SuccessLbl,
   hipe_rtl:mk_alu(BinHeader, Binary, sub, hipe_rtl:mk_imm(2)),
   hipe_rtl:mk_load(BinSize, BinHeader, hipe_rtl:mk_imm(?SUB_BIN_BINSIZE)),
   hipe_rtl:mk_alu(BinSize, BinSize, srl, hipe_rtl:mk_imm(2))].

get_binary_bytes(Binary, BinSize, Base, Offset, Orig, OrigOffset,
		 TrueLblName, FalseLblName) ->
  SuccessLbl = hipe_rtl:mk_new_label(),
  SubLbl = hipe_rtl:mk_new_label(),
  OtherLbl = hipe_rtl:mk_new_label(),
  HeapLbl = hipe_rtl:mk_new_label(),
  REFCLbl = hipe_rtl:mk_new_label(),
  SubHeapLbl = hipe_rtl:mk_new_label(),
  SubREFCLbl = hipe_rtl:mk_new_label(),
  SubJoinLbl = hipe_rtl:mk_new_label(),
  [hipe_tagscheme:test_binary(Binary, hipe_rtl:label_name(SuccessLbl), FalseLblName, 0.99),
   SuccessLbl,
   hipe_rtl:mk_load(BinSize, Binary, hipe_rtl:mk_imm(?SUB_BIN_BINSIZE-2)),
   hipe_rtl:mk_alu(BinSize, BinSize, sll, hipe_rtl:mk_imm(?BYTE_SHIFT)),
   hipe_rtl:mk_move(Offset, hipe_rtl:mk_imm(0)),
   hipe_tagscheme:test_subbinary(Binary, hipe_rtl:label_name(SubLbl), hipe_rtl:label_name(OtherLbl)),
   SubLbl,
   hipe_rtl:mk_load(OrigOffset, Binary, hipe_rtl:mk_imm(?SUB_BIN_OFFS-2)),
   hipe_rtl:mk_load(Orig, Binary, hipe_rtl:mk_imm(?SUB_BIN_ORIG-2)),
   hipe_tagscheme:test_heap_binary(Orig, hipe_rtl:label_name(SubHeapLbl), hipe_rtl:label_name(SubREFCLbl)),
   SubHeapLbl,
   hipe_rtl:mk_alu(Base, Orig, add, hipe_rtl:mk_imm(?HEAP_BIN_DATA-2)),
   hipe_rtl:mk_goto(hipe_rtl:label_name(SubJoinLbl)),
   SubREFCLbl,
   hipe_rtl:mk_load(Base, Orig, hipe_rtl:mk_imm(?PROC_BIN_BYTES-2)),
   SubJoinLbl,
   hipe_rtl:mk_alu(Base, Base, add, OrigOffset),
   hipe_rtl:mk_goto(TrueLblName),
   OtherLbl,
   hipe_rtl:mk_move(OrigOffset, hipe_rtl:mk_imm(0)),
   hipe_rtl:mk_move(Orig, Binary),
   hipe_tagscheme:test_heap_binary(Binary, hipe_rtl:label_name(HeapLbl), hipe_rtl:label_name(REFCLbl)),
   HeapLbl,
   hipe_rtl:mk_alu(Base, Binary, add, hipe_rtl:mk_imm(?HEAP_BIN_DATA-2)),
   hipe_rtl:mk_goto(TrueLblName),
   REFCLbl,
   hipe_rtl:mk_load(Base, Binary, hipe_rtl:mk_imm(?PROC_BIN_BYTES-2)),
   hipe_rtl:mk_goto(TrueLblName)].
 
copy_aligned_static_bytes(CopyBase, CopyOffset, CopySize, Base, Offset, NewOffset,
			  Size, TrueLblName, FalseLblName) ->
  BaseDst = hipe_rtl:mk_new_reg(),
  BaseSrc = hipe_rtl:mk_new_reg(),
  BothOffset = hipe_rtl:mk_new_reg(),
  Iter = hipe_rtl:mk_imm(Size div ?BYTE_SIZE),

  small_check(hipe_rtl:mk_imm(Size), CopySize, FalseLblName) ++
    initializations(BaseSrc, BaseDst, BothOffset, CopyOffset, Offset, CopyBase, Base) ++
    [hipe_rtl:mk_alu(NewOffset, Offset, 'add', hipe_rtl:mk_imm(Size))] ++
    easy_loop(BaseSrc, BaseDst, BothOffset, Iter, hipe_rtl:mk_imm(Size band ?LOW_BITS), TrueLblName).

copy_aligned_bytes(CopyBase, CopyOffset, CopySize, Base, Offset, NewOffset,
		   Size, TrueLblName, FalseLblName) ->
  BaseDst = hipe_rtl:mk_new_reg(),
  BaseSrc = hipe_rtl:mk_new_reg(),
  Iter = hipe_rtl:mk_new_reg(),
  Extra = hipe_rtl:mk_new_reg(),
  BothOffset = hipe_rtl:mk_new_reg(), 
 
  small_check(Size, CopySize, FalseLblName) ++
    initializations(BaseSrc, BaseDst, BothOffset, CopyOffset, Offset, CopyBase, Base) ++
    [hipe_rtl:mk_alu(Extra, Size, 'and', hipe_rtl:mk_imm(?LOW_BITS)), 
     hipe_rtl:mk_alu(Iter, Size, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)), 
     hipe_rtl:mk_alu(NewOffset, Offset, 'add', Size)] ++
    easy_loop(BaseSrc, BaseDst, BothOffset, Iter, Extra, TrueLblName).

copy_aligned_bytes_all(CopyBase, CopyOffset, CopySize, Base, Offset, NewOffset,
		       TrueLblName) ->
  Tmp1 = hipe_rtl:mk_new_reg(),
  BaseDst = hipe_rtl:mk_new_reg(),
  BaseSrc = hipe_rtl:mk_new_reg(),
  Iter = hipe_rtl:mk_new_reg(),
  BothOffset = hipe_rtl:mk_new_reg(),
  LoopLbl = hipe_rtl:mk_new_label(), 
    initializations(BaseSrc, BaseDst, BothOffset, CopyOffset, Offset, CopyBase, Base) ++
    [hipe_rtl:mk_alu(Iter, CopySize, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)), 
     hipe_rtl:mk_alu(NewOffset, Offset, 'add', CopySize),
     hipe_rtl:mk_branch(BothOffset, ne, Iter, hipe_rtl:label_name(LoopLbl), TrueLblName, 0.99),
     LoopLbl,
     hipe_rtl:mk_load(Tmp1, BaseSrc, BothOffset, byte, unsigned),
     hipe_rtl:mk_store(BaseDst, BothOffset, Tmp1, byte),
     hipe_rtl:mk_alu(BothOffset, BothOffset, add, hipe_rtl:mk_imm(1)),
     hipe_rtl:mk_branch(BothOffset, ne, Iter, hipe_rtl:label_name(LoopLbl), TrueLblName, 0.99)].

copy_static_bytes(CopyBase, CopyOffset, CopySize, Base, Offset, NewOffset, Size,
		  TrueLblName, FalseLblName) ->
  InitOffs = hipe_rtl:mk_new_reg(),
  BaseDst = hipe_rtl:mk_new_reg(),
  BaseSrc = hipe_rtl:mk_new_reg(),
  BothOffset = hipe_rtl:mk_new_reg(),
  EasyLbl = hipe_rtl:mk_new_label(),
  HardLbl = hipe_rtl:mk_new_label(),
  Iter = hipe_rtl:mk_imm(Size div ?BYTE_SIZE),
  
  small_check(hipe_rtl:mk_imm(Size div ?BYTE_SIZE), CopySize, FalseLblName) ++
    initializations(BaseSrc, BaseDst, BothOffset, CopyOffset, Offset, CopyBase, Base) ++
    [hipe_rtl:mk_alub(InitOffs, Offset, 'and', hipe_rtl:mk_imm(?LOW_BITS), eq, hipe_rtl:label_name(EasyLbl), hipe_rtl:label_name(HardLbl)),
     EasyLbl,
     hipe_rtl:mk_alu(NewOffset, Offset, 'add', hipe_rtl:mk_imm(Size))] ++
    easy_loop(BaseSrc, BaseDst, BothOffset, Iter, hipe_rtl:mk_imm(Size), TrueLblName) ++
    [HardLbl,
     hipe_rtl:mk_alu(NewOffset, Offset, 'add', hipe_rtl:mk_imm(Size))] ++
    hard_loop(BaseSrc, BaseDst, BothOffset, Iter, InitOffs, hipe_rtl:mk_imm(Size band ?LOW_BITS), TrueLblName).
 

copy_bytes(CopyBase, CopyOffset, CopySize, Base, Offset, NewOffset, Size,
	   TrueLblName, FalseLblName) ->
  InitOffs = hipe_rtl:mk_new_reg(),
  Iter = hipe_rtl:mk_new_reg(),
  BaseSrc = hipe_rtl:mk_new_reg(),
  BaseDst = hipe_rtl:mk_new_reg(),
  BothOffset = hipe_rtl:mk_new_reg(),
  Extra = hipe_rtl:mk_new_reg(),
  EasyLbl = hipe_rtl:mk_new_label(),
  HardLbl = hipe_rtl:mk_new_label(),
 
  small_check(Size, CopySize, FalseLblName) ++
    initializations(BaseSrc, BaseDst, BothOffset, CopyOffset, Offset, CopyBase, Base) ++
    [hipe_rtl:mk_alu(Iter, Size, srl, hipe_rtl:mk_imm(?BYTE_SHIFT)), 
     hipe_rtl:mk_alu(Extra, Size, 'and', hipe_rtl:mk_imm(?LOW_BITS)), 
     hipe_rtl:mk_alub(InitOffs, Offset, 'and', hipe_rtl:mk_imm(?LOW_BITS), eq, hipe_rtl:label_name(EasyLbl), hipe_rtl:label_name(HardLbl)),
     EasyLbl,
     hipe_rtl:mk_alu(NewOffset, Offset, 'add', Size)] ++
    easy_loop(BaseSrc, BaseDst, BothOffset, Iter, Extra, TrueLblName) ++
    [HardLbl,
     hipe_rtl:mk_alu(NewOffset, Offset, 'add', Size)] ++
    hard_loop(BaseSrc, BaseDst, BothOffset, Iter, InitOffs, Extra, TrueLblName).

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
   hipe_rtl:mk_alu(NewOffset, BinOffset, add, hipe_rtl:mk_imm(StringSize*?BYTE_SIZE))] ++
   easy_loop(StringBase, NewBinBase, BothOffset, hipe_rtl:mk_imm(StringSize), hipe_rtl:mk_imm(0), TrueLblName) ++
    [HardLbl,
     hipe_rtl:mk_alu(NewOffset, BinOffset, add, hipe_rtl:mk_imm(StringSize*?BYTE_SIZE))] ++
    hard_loop(StringBase, NewBinBase, BothOffset, hipe_rtl:mk_imm(StringSize), InitOffs, hipe_rtl:mk_imm(0), TrueLblName).

copy_aligned_string(StringBase, StringSize, BinBase, BinOffset, NewOffset,
		    TrueLblName) ->
  WordSize = hipe_rtl_arch:word_size(),
  TmpOffset = hipe_rtl:mk_new_reg(),
  BothOffset = hipe_rtl:mk_new_reg(),
  NewBinBase = hipe_rtl:mk_new_reg(),
  ExtraBytes=StringSize rem WordSize,
  Code1 = [hipe_rtl:mk_alu(TmpOffset, BinOffset, srl, hipe_rtl:mk_imm(3)),
   hipe_rtl:mk_alu(NewBinBase, BinBase, add, TmpOffset),
   hipe_rtl:mk_move(BothOffset, hipe_rtl:mk_imm(0)),
   hipe_rtl:mk_alu(NewOffset, BinOffset, add, hipe_rtl:mk_imm(StringSize*8))],
  Code2 = case WordSize*(StringSize div WordSize) of
	    0 ->
	      [];
	    X ->
	      tight_loop(StringBase, NewBinBase, BothOffset, hipe_rtl:mk_imm(X))
	  end,
  Code3 = last_part(StringBase, NewBinBase, BothOffset, ExtraBytes, TrueLblName),
  Code1 ++ Code2 ++ Code3.

%% store_4 should be replaced by store unaligned word

tight_loop(BaseSrc, BaseDst, BothOffset, Iterations) -> 
  Tmp1 = hipe_rtl:mk_new_reg(),
  LoopLbl = hipe_rtl:mk_new_label(),
  FinishLbl = hipe_rtl:mk_new_label(),
  [LoopLbl,
   hipe_rtl:mk_load(Tmp1, BaseSrc, BothOffset),
   hipe_rtl_arch:store_4(BaseDst, BothOffset, Tmp1),
   hipe_rtl:mk_branch(BothOffset, ne, Iterations, hipe_rtl:label_name(LoopLbl), hipe_rtl:label_name(FinishLbl), 0.99),
   FinishLbl].

last_part(_StringBase, _NewBinBase, _BothOffset, 0, TrueLblName) ->
  [hipe_rtl:mk_goto(TrueLblName)];
last_part(StringBase, NewBinBase, BothOffset, 1, TrueLblName) ->
  Tmp1 = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_load(Tmp1, StringBase, BothOffset, byte, unsigned),
   hipe_rtl:mk_store(NewBinBase, BothOffset, Tmp1, byte),
   hipe_rtl:mk_goto(TrueLblName)];
last_part(StringBase, NewBinBase, BothOffset, N, TrueLblName) ->
  Tmp1 = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_load(Tmp1, StringBase, BothOffset, byte, unsigned),
   hipe_rtl:mk_store(NewBinBase, BothOffset, Tmp1, byte),
   hipe_rtl:mk_alu(BothOffset, BothOffset, add, hipe_rtl:mk_imm(1))|
   last_part(StringBase, NewBinBase, BothOffset, N-1, TrueLblName)].

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
copy_int_big(Base, Offset, NewOffset, Size, Tmp1) when is_integer(Size) ->
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
   hipe_rtl:mk_alu(Tmp7, Tmp2, 'add', hipe_rtl:mk_imm((WordSize-1)*?BYTE_SIZE)),
   hipe_rtl:mk_alu(Tmp1, Tmp1, sll, Tmp7),
   hipe_rtl:mk_alu(Tmp1, Tmp1, srl, Tmp7),
   hipe_rtl:mk_alu(Tmp1, Tmp1, 'or', OldByte),
   hipe_rtl:mk_store(Base, TmpOffset, Tmp1, byte)].

copy_float_little(_Base, _Offset, _NewOffset, _Src, FalseLblName, _TrueLblName, fail) ->
  [hipe_rtl:mk_goto(FalseLblName)];
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

%%
%% Size calculation functions below
%%

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
  {SizeCode, ThisSize} =  hipe_rtl_bin_util:make_size(Unit, Var, FalseLblName),
  calculate_var_size(Units, Vars, VarSize, ThisSize, SizeCode, FalseLblName).

calculate_var_size([Unit|Units], [Var|Vars], VarSize, LastSize, AccCode, FalseLblName) ->
  
  {SizeCode,ThisSize} =  hipe_rtl_bin_util:make_size(Unit, Var, FalseLblName),
  ThisCode = [SizeCode,
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
  calculate_all_size(Rest, VarSize, ThisSize, AccCode++ThisCode, FalseLblName);
calculate_all_size([], VarSize, LastSize, AccCode, _FalseLblName) ->
  AccCode ++ [hipe_rtl:mk_move(VarSize, LastSize)].

