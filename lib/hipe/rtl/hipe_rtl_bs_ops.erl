%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% Time-stamp: <02/05/13 14:53:23 happi>
%% ====================================================================
%%  Filename : 	hipe_rtl_bs_ops.erl
%%  Module   :	hipe_rtl_bs_ops
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-06-14 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: pegu2945 $
%%              $Date: 2002/09/13 15:26:20 $
%%              $Revision: 1.9 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_rtl_bs_ops).
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
      {NewTab, Label} = 
	hipe_consttab:insert_block(ConstTab, 4, byte, String),

      {[hipe_rtl:mk_load_address(Tmp1, Label, constant),
	hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(SizeInBytes)),
	gen_test_sideffect_bs_call(bs_put_string,
				   [Tmp1,Tmp2], 
				   TrueLblName,FalseLblName)],
       NewTab};
    _ -> 
      Code = 
	case BsOP of
	  bs_init ->
	    [hipe_rtl:mk_call([], bs_init, [], c, TrueLblName, [])];
	  bs_start_match ->
	    gen_test_sideffect_bs_call(bs_start_match,
				       Args, TrueLblName, FalseLblName);

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
	  %% put float
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
	  {bs_skip_bits_all, Flags} ->
	    case (Flags band ?BSF_ALIGNED) of
	      1 -> %% This can't fail.
		[hipe_rtl:mk_call([], bs_skip_bits_all, [], c,
				  TrueLblName,[])];
	      _ -> 
		gen_test_sideffect_bs_call(bs_skip_bits_all,
					   [], 
					   TrueLblName,FalseLblName)
	    end;

	  {bs_skip_bits, Bits} ->
	    case Args of
	      [] ->
		Tmp1 = hipe_rtl:mk_new_reg(),
		[hipe_rtl:mk_move(Tmp1, hipe_rtl:mk_imm(Bits))|
		 gen_test_sideffect_bs_call(bs_skip_bits,
					    [Tmp1], 
					    TrueLblName,FalseLblName)];
	      [Arg] ->
		Tmp1 = hipe_rtl:mk_new_reg(),
		gen_make_size(Tmp1, Bits, Arg, FalseLblName) ++
		  gen_test_sideffect_bs_call(bs_skip_bits,
					     [Tmp1], 
					     TrueLblName,FalseLblName)

	    end;
	  {bs_get_integer,Size,Flag} ->
	    Tmp1 = hipe_rtl:mk_new_reg(),
	    Tmp2 = hipe_rtl:mk_new_reg(),
	    [Dst1] = Dst,
	    case Args of
	      [] ->
		[hipe_rtl:mk_move(Tmp1, hipe_rtl:mk_imm(Size)),
		 hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flag)),
		 gen_bs_call(bs_get_integer,
			     [Tmp1, Tmp2],
			     Dst1,
			     TrueLblName,FalseLblName)];
	      [Arg] ->
		[gen_make_size(Tmp1, Size, Arg, FalseLblName),
		 hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flag)),
		 gen_bs_call(bs_get_integer,
			     [Tmp1, Tmp2],
			     Dst1,
			     TrueLblName,FalseLblName)]
	    end;

	  {bs_get_float,Size,Flag} ->
	    Tmp1 = hipe_rtl:mk_new_reg(),
	    Tmp2 = hipe_rtl:mk_new_reg(),
	    [Dst1] = Dst,
	    case Args of
	      [] ->
		[hipe_rtl:mk_move(Tmp1, hipe_rtl:mk_imm(Size)),
		 hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flag)),
		 gen_bs_call(bs_get_float,
			     [Tmp1, Tmp2],
			     Dst1,
			     TrueLblName,FalseLblName)];
	      [Arg] ->
		[gen_make_size(Tmp1, Size, Arg, FalseLblName),
		 hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flag)),
		 gen_bs_call(bs_get_float,
			     [Tmp1, Tmp2],
			     Dst1,
			     TrueLblName,FalseLblName)]
	    end;



	  {bs_get_binary_all, _Flags} -> 
	    [Dst1] = Dst, 
	    gen_bs_call(bs_get_binary_all,
			[],
			Dst1,
			TrueLblName,FalseLblName);

	  {bs_get_binary,Size,Flag} ->
	    Tmp1 = hipe_rtl:mk_new_reg(),
	    Tmp2 = hipe_rtl:mk_new_reg(),
	    [Dst1] = Dst,

	    case Args of
	      [] ->
		[hipe_rtl:mk_move(Tmp1, hipe_rtl:mk_imm(Size))];
	      [BitsVar] ->
		gen_make_size(Tmp1, Size, BitsVar, FalseLblName)
	    end ++
	      [hipe_rtl:mk_move(Tmp2, hipe_rtl:mk_imm(Flag))|
	       gen_bs_call(bs_get_binary,
			   [Tmp1, Tmp2],
			   Dst1,
			   TrueLblName,FalseLblName)];

	  {bs_test_tail, NumBits} ->
	    Tmp1 = hipe_rtl:mk_new_reg(),
	    [hipe_rtl:mk_move(Tmp1, hipe_rtl:mk_imm(NumBits)),
	     gen_test_sideffect_bs_call(bs_test_tail,
					[Tmp1],TrueLblName,FalseLblName)];

	  {bs_restore, Index} ->
	    Tmp1 = hipe_rtl:mk_new_reg(),
	    [hipe_rtl:mk_move(Tmp1, hipe_rtl:mk_imm(Index)),
	     hipe_rtl:mk_call([],bs_restore,[Tmp1],c, TrueLblName,[])];

	  {bs_save, Index} ->
	    Tmp1 = hipe_rtl:mk_new_reg(),
	    [hipe_rtl:mk_move(Tmp1, hipe_rtl:mk_imm(Index)),
	     hipe_rtl:mk_call([],bs_save,[Tmp1],c, TrueLblName,[])];


	  bs_final ->
	    [hipe_rtl_arch:call_bif(Dst, bs_final, [],
				    TrueLblName, FalseLblName)];

	  _ -> ?EXIT({unhandled_bs_primop, BsOP})
	end,
      {Code, ConstTab}
  end.







%% ____________________________________________________________________
%% 
gen_bs_call(Name,Args,DstVar,TrueLblName,FalseLblName) ->
  RetLabel =  hipe_rtl:mk_new_label(),
  OkLabel =  hipe_rtl:mk_new_label(),
  NonVal = hipe_rtl:mk_imm(hipe_tagscheme:mk_non_value()),
  [hipe_rtl_arch:call_bif([DstVar], Name, Args,
			  hipe_rtl:label_name(RetLabel), []),
   RetLabel,
   hipe_rtl:mk_branch(DstVar, eq, NonVal,
		      FalseLblName, 
		      hipe_rtl:label_name(OkLabel), 0.01),
   OkLabel,
   hipe_rtl:mk_goto(TrueLblName)].


gen_test_sideffect_bs_call(Name,Args,TrueLblName,FalseLblName) ->
  Tmp1 = hipe_rtl:mk_new_reg(),
  RetLabel =  hipe_rtl:mk_new_label(),

  [hipe_rtl:mk_call([Tmp1], Name, Args,  c,
		    hipe_rtl:label_name(RetLabel),[]),
   RetLabel,
   hipe_rtl:mk_branch(Tmp1, eq, hipe_rtl:mk_imm(0), 
		      FalseLblName, TrueLblName, 0.01)].


gen_make_size(DstReg, UnitImm, BitsVar, FalseLblName) ->
  MulDoneLabel =  hipe_rtl:mk_new_label(),
  MulOkLabel =  hipe_rtl:mk_new_label(),
  PosNumOkLabel = hipe_rtl:mk_new_label(),
  UnitVar = hipe_rtl:mk_new_var(),
  TmpVar = hipe_rtl:mk_new_var(),
  ZeroVar = hipe_rtl:mk_new_var(),
  ZeroConst = hipe_rtl:mk_imm(hipe_tagscheme:mk_fixnum(0)),
  UnitConst = hipe_rtl:mk_imm(hipe_tagscheme:mk_fixnum(UnitImm)),
  [hipe_rtl:mk_move(UnitVar, UnitConst),  
   hipe_rtl:mk_call([TmpVar], '*', [BitsVar, UnitVar], c,
			 hipe_rtl:label_name(MulDoneLabel),
			 FalseLblName),
   MulDoneLabel,
   hipe_tagscheme:test_fixnum(TmpVar, hipe_rtl:label_name(MulOkLabel), 
			      FalseLblName, 0.99),
   MulOkLabel,
   hipe_rtl:mk_move(ZeroVar, ZeroConst),  
   hipe_tagscheme:fixnum_ge(TmpVar, ZeroVar, hipe_rtl:label_name(PosNumOkLabel), FalseLblName, 0.99), 
   PosNumOkLabel,
   hipe_tagscheme:untag_fixnum(DstReg, TmpVar)].


