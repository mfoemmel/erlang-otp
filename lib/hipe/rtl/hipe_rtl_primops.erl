%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% Time-stamp: <2003-04-02 18:06:28 richardc>
%% ====================================================================
%%  Filename : 	map.erl
%%  Module   :	map
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-03-15 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: tobiasl $
%%              $Date: 2003/05/07 17:44:23 $
%%              $Revision: 1.48 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_rtl_primops). 
-export([gen_primop/3,gen_enter_fun/2]).
-export([gen_mk_tuple/3]).
-export([type/2]).

%%------------------------------------------------------------------------

-include("../main/hipe.hrl").
-include("hipe_icode2rtl.hrl").
-include("hipe_literals.hrl").

%%------------------------------------------------------------------------


%% ____________________________________________________________________
%% CALL PRIMOP
%%
%% Generate code for primops. This is mostly a dispatch function
%%
gen_primop({Op, Dst, Args, Cont, Fail, Annot},
	   {VarMap,  ConstTab},
	   {Options, ExitInfo}) ->
  GotoCont = hipe_rtl:mk_goto(Cont),
  
  case Op of
    %% ------------------------------------------------
    %% Binary Syntax
    %%
    {hipe_bs_primop, BsOP} ->

      FailLabel =  hipe_rtl:mk_new_label(),
      case get(hipe_inline_bs) of
	true ->	
	  {Code1, NewTab} =
	    hipe_rtl_inline_bs_ops:gen_rtl(BsOP, Args, Dst, Cont,
					   hipe_rtl:label_name(FailLabel),
					   ConstTab);
	_ ->
	  {Code1, NewTab} =
	    hipe_rtl_bs_ops:gen_rtl(BsOP, Args, Dst, Cont,
				    hipe_rtl:label_name(FailLabel),
				    ConstTab)
      end,


      FailCode = 
	case Fail of
	  [] ->
	    hipe_rtl_exceptions:gen_exit_atom(badarg, ExitInfo);
	  _ ->
	    hipe_rtl_exceptions:gen_fail_code(Fail,hipe_rtl:mk_new_var(),
					      badarg,  ExitInfo)
	end,
      {[Code1,FailLabel,FailCode], VarMap, NewTab};

    _ ->
      Code = 
	case Op of
	  %% Arithmetic
	  '+' ->
	    gen_add_sub_2(Dst, Args, Cont, Fail, Annot, Op, add, ExitInfo);
	  '-' ->
	    gen_add_sub_2(Dst, Args, Cont, Fail, Annot, Op, sub, ExitInfo);
	  'band' ->
	    Dst1 =
	      case Dst of
		[] -> %% The result is not used.
		  [hipe_rtl:mk_new_var()];
		Dst0 -> Dst0
	      end,
	    gen_bitop_2(Dst1, Args, Cont, Fail, Annot, Op, 'and', ExitInfo);
	  'bor' ->
	    Dst1 =
	      case Dst of
		[] -> %% The result is not used.
		  [hipe_rtl:mk_new_var()];
		Dst0 -> Dst0
	      end,
	    gen_bitop_2(Dst1, Args, Cont, Fail, Annot, Op, 'or', ExitInfo);
	  'bxor' ->
	    Dst1 =
	      case Dst of
		[] -> %% The result is not used.
		  [hipe_rtl:mk_new_var()];
		Dst0 -> Dst0
	      end,
	    gen_bitop_2(Dst1, Args, Cont, Fail, Annot, Op, 'xor', ExitInfo);
	  'bnot' ->
	    Dst1 =
	      case Dst of
		[] -> %% The result is not used.
		  [hipe_rtl:mk_new_var()];
		Dst0 -> Dst0
	      end,
	    gen_bnot_2(Dst1, Args, Cont, Fail, Annot, Op, ExitInfo);


	  %% These are just calls to the bifs...
%	    '*' ->
%		gen_call_bif(Dst, Args, Cont, Fail, Op, badarith, ExitInfo);
%	    '/' ->
%		gen_call_bif(Dst, Args, Cont, Fail, Op, badarith, ExitInfo);
%	    'div' ->
%		gen_call_bif(Dst, Args, Cont, Fail, Op, badarith, ExitInfo);
%	    'rem' ->
%		gen_call_bif(Dst, Args, Cont, Fail, Op, badarith, ExitInfo);
%	    'bsl' ->
%		gen_call_bif(Dst, Args, Cont, Fail, Op, badarith, ExitInfo);
%	    'bsr' ->
%		gen_call_bif(Dst, Args, Cont, Fail, Op, badarith, ExitInfo);

	  %% List handling
	  cons ->
	    case Dst of
	      [] -> %% The result is not used.
		[GotoCont];
	      [Dst1] ->	    
		[gen_cons(Dst1, Args, Options),GotoCont]
	    end;
	  unsafe_hd ->
	    case Dst of
	      [] -> %% The result is not used.
		[GotoCont];
	      [Dst1] ->	  
		[gen_unsafe_hd(Dst1, Args),GotoCont]
	    end;
	  unsafe_tl ->
	    case Dst of
	      [] -> %% The result is not used.
		[GotoCont];
	      [Dst1] ->	 
		[gen_unsafe_tl(Dst1, Args),GotoCont]
	    end;


	  %% Tuple handling
	  mktuple ->
	    case Dst of
	      [] -> %% The result is not used.
		[GotoCont];
	      [Dst1] ->
		[gen_mk_tuple(Dst1, Args, Options),GotoCont]
	    end;

	  %% TODO: Remove unused element functions...
	  unsafe_element ->
	    [Index, Tuple] = Args,
	    case Dst of
	      [] -> %% The result is not used.
		[gen_unsafe_element(hipe_rtl:mk_new_var(), Index, Tuple),GotoCont];
	      [Dst1] ->
		[gen_unsafe_element(Dst1, Index, Tuple),GotoCont]
	    end;
	  {unsafe_element, N} ->
	    case Dst of
	      [] -> %% The result is not used.
		[GotoCont];
	      [Dst1] ->
		[Tuple] = Args,
		[gen_unsafe_element(Dst1, hipe_rtl:mk_imm(N), Tuple),GotoCont]
	    end;
	  {unsafe_update_element, N} ->
	    [] = Dst,
	    [Tuple, Value] = Args,
	    [gen_unsafe_update_element(Tuple, hipe_rtl:mk_imm(N), Value),
	     GotoCont];
	  {erlang,element,2} ->
	    Dst1 =
	      case Dst of
		[] -> %% The result is not used.
		  hipe_rtl:mk_new_var();
		[Dst0] -> Dst0
	      end,
	    [Index, Tuple] = Args,
	    [gen_element_2(Dst1, Fail, Index, Tuple, 
			   Cont, Annot, ExitInfo, [], [])];
	  {erlang,element,2, TypeInfo} ->
	    Dst1 =
	      case Dst of
		[] -> %% The result is not used.
		  hipe_rtl:mk_new_var();
		[Dst0] -> Dst0
	      end,
	    [TupleInfo, IndexInfo] = TypeInfo,	    
	    [Index, Tuple] = Args,	    
	    [gen_element_2(Dst1, Fail, Index, Tuple, 
			   Cont, Annot, ExitInfo, TupleInfo, IndexInfo)];
%	  element -> %% Obsolete.
%	    [Dst1,  _Flag] = Dst,
%	    [Index, Tuple] = Args,
%	    [gen_element_2(Dst1, Fail, Index, Tuple, 
%			   Cont, Annot, ExitInfo)];

	  %% GC test
	  {gc_test, Need} ->
	    [hipe_rtl:mk_gctest(Need),GotoCont];

	  %% Process handling.
	  {erlang,self,0} ->
	    case Dst of
	      [] -> %% The result is not used.
		[GotoCont];
	      [Dst1] ->
		[load_p_field(Dst1, ?P_ID),
		 GotoCont]
	    end;
	  redtest ->
	    [gen_redtest(1),GotoCont];
	  %% Receives
	  get_msg ->
	    hipe_rtl_arch:call_bif(Dst, get_msg, [], Cont, Fail);
	  next_msg ->
	    hipe_rtl_arch:call_bif(Dst, next_msg, [], Cont, Fail);
	  select_msg ->
	    hipe_rtl_arch:call_bif(Dst, select_msg, [], Cont, Fail);
	  clear_timeout ->
	    NewArgs = Args, 
	    hipe_rtl:mk_call(Dst, Op, NewArgs, c, Cont, Fail);
	  suspend_msg ->
	    hipe_rtl:mk_call(Dst, Op, Args, c, Cont, Fail);


	  %% Closures
	  call_fun ->
	    gen_call_fun(Dst, Args, Cont, Fail, ExitInfo);
	  {mkfun,MFA,MagicNum,Index} ->
	    case Dst of
	      [] -> %% The result is not used.
		[GotoCont];
	      _ ->
		[gen_mkfun(Dst, MFA, MagicNum, Index, Args, Fail),GotoCont]
	    end;

	  {closure_element, N} ->
	    case Dst of
	      [] -> %% The result is not used.
		[GotoCont];
	      [Dst1] ->
		[Closure] = Args,
		[gen_closure_element(Dst1, hipe_rtl:mk_imm(N), Closure),
		 GotoCont]
	    end;

	  {hipe_bifs, in_native, 0} ->
	    Dst1 =
	      case Dst of
		[] -> %% The result is not used.
		  hipe_rtl:mk_new_var();
		[Dst0] -> Dst0
	      end,
	    [ hipe_rtl:mk_load_atom(Dst1, true),
	      GotoCont];
	  {erlang, apply, 3} ->
	    %%	TODO:    gen_apply(Dst,Args, Cont, Fail, ExitInfo);
	    [hipe_rtl:mk_call(Dst, 
			      {hipe_internal, apply, 3}, 
			      Args, 
			      c,
			      Cont, Fail)];

	  %% Floating point stuff.

	  fp_add ->
	    [Arg1, Arg2] = Args,
	    case Dst of
	      [] ->
		hipe_rtl:mk_fp(hipe_rtl:mk_new_fpreg(), Arg1, 'fadd', Arg2);
	      [Dst1] ->
		hipe_rtl:mk_fp(Dst1, Arg1, 'fadd', Arg2)
	    end;

	  fp_sub ->
	    [Arg1, Arg2] = Args,
	    case Dst of
	      [] ->
		hipe_rtl:mk_fp(hipe_rtl:mk_new_fpreg(), Arg1, 'fsub', Arg2);
	      [Dst1] ->
		hipe_rtl:mk_fp(Dst1, Arg1, 'fsub', Arg2)
	    end;	  

	  fp_mul ->
	    [Arg1, Arg2] = Args,
	    case Dst of
	      [] ->
		hipe_rtl:mk_fp(hipe_rtl:mk_new_fpreg(), Arg1, 'fmul', Arg2);
	      [Dst1] ->
		hipe_rtl:mk_fp(Dst1, Arg1, 'fmul', Arg2)
	    end;

	  fp_div ->
	    [Arg1, Arg2] = Args,
	    case Dst of
	      [] ->
		hipe_rtl:mk_fp(hipe_rtl:mk_new_fpreg(), Arg1, 'fdiv', Arg2);
	      [Dst1] ->
		hipe_rtl:mk_fp(Dst1, Arg1, 'fdiv', Arg2)
	    end;	  

	  fnegate ->
	    [Arg] = Args,
	    case Dst of
	      [] ->
		hipe_rtl:mk_fp_unop(hipe_rtl:mk_new_fpreg(), Arg, 'fchs');
	      [Dst1] ->
		hipe_rtl:mk_fp_unop(Dst1, Arg, 'fchs')
	    end;	  

	  fclearerror ->
	    gen_fclearerror();

	  fcheckerror ->
	    gen_fcheckerror(Cont, Fail, ExitInfo);

	  conv_to_float ->
	    case Dst of
	      [] ->
		gen_conv_to_float(hipe_rtl:mk_new_fpreg(), Args, Cont, Fail, ExitInfo);
	      [Dst1] ->
		gen_conv_to_float(Dst1, Args, Cont, Fail, ExitInfo)
	    end;

	  unsafe_untag_float ->
	    [Arg] = Args,
	    case Dst of
	      [] ->
		hipe_tagscheme:unsafe_untag_float(hipe_rtl:mk_new_fpreg(),
						  Arg); 
	      [Dst1]->
		hipe_tagscheme:unsafe_untag_float(Dst1, Arg)
	    end;
	  
	  unsafe_tag_float ->
	    [Arg] = Args,
	    case Dst of
	      [] ->
		hipe_tagscheme:unsafe_tag_float(hipe_rtl:mk_new_var(),
						Arg, Options);
	      [Dst1]->
		hipe_tagscheme:unsafe_tag_float(Dst1, Arg, Options)
	    end;

	  _ ->
	    generic_primop(Dst, Op, Args, Cont, Fail, ExitInfo)
	end,
      {Code, VarMap, ConstTab}
  end.


%% ____________________________________________________________________
%% 
%%
%% Generate code for a generic call to a bif.
generic_primop(Dsts, Op, Args, Continuation, Fail, ExitInfo) ->
  %% Get arity and name
  {Arity, Name} = 
    case Op of 
      {_Mod,BifName,A} -> %% An ordinary MFA
	{A, BifName};
      _ -> %% Some internal primop with just a name.
	{length(Args),Op}
    end,

  %% Test if the bif can fail
  Fails = hipe_bif:fails(Arity,Name),
  if Fails =:= false ->
      %% The bif can't fail just call it.
      [hipe_rtl:mk_call(Dsts, Op , Args, c, Continuation, [])];
     true ->
      %% The bif can fail, call it and test.
      failing_primop(Dsts, Op, Args, Continuation, Fail, ExitInfo)
  end.

%% Generate code for a bif that can fail.
failing_primop(Dsts, Op, Args, Continuation, Fail, _ExitInfo) ->
  [hipe_rtl:mk_call(Dsts, Op, Args, c, Continuation, Fail)].


%% Generate code for a bif that can fail.
gen_call_bif(Res, Args, Cont, Fail, Op, _ExitReason, _ExitInfo) ->
  [hipe_rtl:mk_call(Res, Op, Args, c, Cont, Fail)].



%% ____________________________________________________________________
%% 

%% ____________________________________________________________________
%% ARITHMETIC
%%

%%
%% Inline addition & subtraction
%%

gen_add_sub_2(Dst, Args, Cont, Fail, _Annot, Op, AluOp, ExitInfo) ->
  [Arg1, Arg2] = Args,
  GenCaseLabel = hipe_rtl:mk_new_label(),
  case Dst of
    [] ->
      gen_op_general_case(hipe_rtl:mk_new_var(),
			  Op, Args, Cont, Fail, GenCaseLabel, ExitInfo);
    [Res] ->
      [hipe_tagscheme:test_two_fixnums(Arg1, Arg2,
				       hipe_rtl:label_name(GenCaseLabel)),
       hipe_tagscheme:fixnum_addsub(AluOp, Arg1, Arg2, Res, GenCaseLabel)|
       gen_op_general_case(Res,Op, Args, Cont, Fail, GenCaseLabel, ExitInfo)]
  end.


gen_op_general_case(Res, Op, Args, Cont, Fail, GenCaseLabel, ExitInfo) ->
  [hipe_rtl:mk_goto(Cont),
   GenCaseLabel,
   gen_call_bif([Res], Args, Cont, Fail, Op, badarith, ExitInfo)].

%%
%% We don't inline multiplication at the moment
%%

%%gen_mul_2([Res], Args, Cont, Fail, Annot, Op, ExitInfo) ->
%%   [Arg1, Arg2] = Args,
%%   GenCaseLabel = hipe_rtl:mk_new_label(),
%%   [hipe_tagscheme:test_two_fixnums(Arg1, Arg2,
%%				    hipe_rtl:label_name(GenCaseLabel)),
%%    hipe_tagscheme:fixnum_mul(Arg1, Arg2, Res, GenCaseLabel)|
%%    gen_op_general_case(Res, Op, Args, Cont, Fail, GenCaseLabel, ExitInfo)].

%%
%% Inline bitoperations.
%% Only works for band, bor and bxor.
%% The shift operations are too expensive to inline.
%%

gen_bitop_2([Res], Args, Cont, Fail, _Annot, Op, BitOp, ExitInfo) ->
  [Arg1, Arg2] = Args,
  GenCaseLabel = hipe_rtl:mk_new_label(),

  [hipe_tagscheme:test_two_fixnums(Arg1, Arg2,
				   hipe_rtl:label_name(GenCaseLabel)),
   hipe_tagscheme:fixnum_andorxor(BitOp, Arg1, Arg2, Res)|
   gen_op_general_case(Res, Op, Args, Cont, Fail, GenCaseLabel, ExitInfo)].

%%
%% Inline not.
%%

gen_bnot_2([Res], Args, Cont, Fail, _Annot, Op, ExitInfo) ->
  [Arg] = Args,
  FixLabel = hipe_rtl:mk_new_label(),
  OtherLabel = hipe_rtl:mk_new_label(),

  [hipe_tagscheme:test_fixnum(Arg, hipe_rtl:label_name(FixLabel),
			      hipe_rtl:label_name(OtherLabel), 0.99),
   FixLabel,
   hipe_tagscheme:fixnum_not(Arg, Res),
   gen_op_general_case(Res, Op, Args, Cont, Fail, OtherLabel, ExitInfo)
  ].


%% ____________________________________________________________________
%% 

%%
%% Inline cons
%%

gen_cons(Dst, [Arg1, Arg2], Options) ->
  Tmp = hipe_rtl:mk_new_reg(),
  {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  Code = 
    [
     GetHPInsn,
     hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(0), Arg1),
     hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(4), Arg2),
     hipe_rtl:mk_move(Tmp, HP),
     hipe_tagscheme:tag_cons(Dst, Tmp),
     hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm(8)),
     PutHPInsn],
  case ?AddGC(Options) of
    true -> [hipe_rtl:mk_gctest(2)|Code];
    false -> Code
  end.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ____________________________________________________________________
%%
%% Handling of closures...
%% ____________________________________________________________________

%% ____________________________________________________________________
%% gen_mkfun
%%
%%    The gc_test should have expanded to
%%    unsigned needed = ERL_FUN_SIZE + num_free;
%%    ErlFunThing* funp = (ErlFunThing *) HAlloc(p, needed);
%%
%% The code generated should do the eq of:
%%  Copy arguments to the fun thing
%%    Eterm* hp = funp->env;
%%    for (i = 0; i < num_free; i++) {
%%	*hp++ = reg[i];
%%    }
%%
%%  Fill in fileds
%%    funp->thing_word = HEADER_FUN;
%%    funp->fe = fe;
%%    funp->num_free = num_free;
%%    funp->creator = p->id;
%%    funp->native_code = fe->native_code;
%%  Increase refcount
%%    fe->refc++;
%%
%%  Link to the process off_heap.funs list
%%    funp->next = p->off_heap.funs;
%%    p->off_heap.funs = funp;
%%
%%  Tag the thing
%%    return make_fun(funp);
%%
gen_mkfun([Dst], {Mod,FunId,Arity}, MagicNr, Index, FreeVars, _Fail) ->
  {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  NumFree = length(FreeVars),

  %%  Copy arguments to the fun thing
  %%    Eterm* hp = funp->env;
  %%    for (i = 0; i < num_free; i++) {
  %%	*hp++ = reg[i];
  %%    }
  CopyFreeVarsCode = gen_free_vars(FreeVars, HP),

  %%  Fill in fields
  %%    funp->thing_word = HEADER_FUN;
  %%    funp->fe = fe;
  %%    funp->num_free = num_free;
  %%    funp->creator = p->id;
  %%    funp->native_code = fe->native_code;
  %%  Increase refcount
  %%    fe->refc++;

  SkeletonCode = gen_fun_thing_skeleton(HP, {Mod,FunId,Arity},
					NumFree, MagicNr, Index),

  %%  Link to the process off_heap.funs list
  %%    funp->next = p->off_heap.funs;
  %%    p->off_heap.funs = funp;
  LinkCode = gen_link_closure(HP),

  %%  Tag the thing and increase the heap_pointer.
  %%    make_fun(funp);
  TagCode = [hipe_tagscheme:tag_fun(Dst, HP),
	     %%  AdjustHPCode 
	     hipe_rtl:mk_alu(HP, HP, add,
			     hipe_rtl:mk_imm((?ERL_FUN_SIZE + NumFree) * 4)),
	     PutHPInsn],

  [[GetHPInsn | CopyFreeVarsCode], SkeletonCode, LinkCode, TagCode].


gen_fun_thing_skeleton(FunP, FunName={_Mod,_FunId,Arity}, NumFree, 
		       MagicNr, Index) ->
  %% Assumes that funp == heap_pointer
  %%  Fill in fields
  %%    funp->thing_word = HEADER_FUN;
  %%    funp->fe = fe;
  %%    funp->num_free = num_free;
  %%    funp->creator = p->id;
  %%    funp->native_code = fe->native_code;
  %%  And creates a fe (at load time).
  FeVar = hipe_rtl:mk_new_reg(),
  PidVar = hipe_rtl:mk_new_reg(),
  NativeVar = hipe_rtl:mk_new_reg(),
  RefcVar = hipe_rtl:mk_new_reg(),

  [hipe_rtl:mk_load_address(FeVar, {FunName, MagicNr, Index}, closure),
   store_struct_field(FunP, ?EFT_FE, FeVar),
   load_struct_field(NativeVar, FeVar, ?EFE_NATIVE_ADDRESS),
   store_struct_field(FunP, ?EFT_NATIVE_ADDRESS, NativeVar),

   store_struct_field(FunP, ?EFT_ARITY, hipe_rtl:mk_imm(Arity-NumFree)),

   load_struct_field(RefcVar, FeVar, ?EFE_REFC),
   hipe_rtl:mk_alu(RefcVar, RefcVar, add, hipe_rtl:mk_imm(1)),
   store_struct_field(FeVar, ?EFE_REFC, RefcVar),

   store_struct_field(FunP, ?EFT_NUM_FREE, hipe_rtl:mk_imm(NumFree)),
   load_p_field(PidVar, ?P_ID),
   store_struct_field(FunP, ?EFT_CREATOR, PidVar),
   store_struct_field(FunP, ?EFT_THING, 
		      hipe_tagscheme:mk_fun_header())].


-ifdef(HEAP_ARCH_PRIVATE).
gen_link_closure(FUNP) ->
  %% Link to the process off_heap.funs list
  %%   funp->next = p->off_heap.funs;
  %%   p->off_heap.funs = funp;
  FunsVar = hipe_rtl:mk_new_reg(),

  [load_p_field(FunsVar,?P_OFF_HEAP_FUNS),
   hipe_rtl:mk_store(FUNP, hipe_rtl:mk_imm(?EFT_NEXT), FunsVar),
   store_p_field(FUNP,?P_OFF_HEAP_FUNS)].
-endif.

-ifdef(HEAP_ARCH_SHARED).
gen_link_closure(FUNP) -> [].
-endif.

load_p_field(Dst,Offset) ->
  hipe_rtl_arch:pcb_load(Dst, Offset).
store_p_field(Src, Offset) ->
  hipe_rtl_arch:pcb_store(Offset, Src).

store_struct_field(StructP, Offset, Src) ->
  hipe_rtl:mk_store(StructP, hipe_rtl:mk_imm(Offset), Src).

load_struct_field(Dest, StructP, Offset) ->
  hipe_rtl:mk_load(Dest, StructP, hipe_rtl:mk_imm(Offset)).

gen_free_vars(Vars, HPReg) ->
  HPVar = hipe_rtl:mk_new_var(),  
  [hipe_rtl:mk_alu(HPVar, HPReg, add, hipe_rtl:mk_imm(?EFT_ENV)) |
   gen_free_vars(Vars, HPVar, 0, [])].

gen_free_vars([Var|Vars], EnvPVar, Offset, AccCode) ->
  Code = hipe_rtl:mk_store(EnvPVar, hipe_rtl:mk_imm(Offset), Var),
  gen_free_vars(Vars, EnvPVar, Offset + 4, [Code|AccCode]);
gen_free_vars([], _, _, AccCode) -> AccCode.

%% ------------------------------------------------------------------
%%
%% enter_fun and call_fun
%%
gen_enter_fun(Args, ExitInfo) ->
  gen_call_fun([], Args, [], [], ExitInfo).

gen_call_fun(Dst, ArgsAndFun, Continuation, Fail, ExitInfo) ->  
  NAddressReg = hipe_rtl:mk_new_reg(),
  ArityReg = hipe_rtl:mk_new_reg(),
  BadFunLab =  hipe_rtl:mk_new_label(),
  BadArityLab =  hipe_rtl:mk_new_label(),
  [Fun|Args] = lists:reverse(ArgsAndFun),

  FailCode = hipe_rtl_exceptions:gen_funcall_fail(Fail, Fun, BadFunLab,
						  BadArityLab, ExitInfo),

  CheckGetCode = 
    hipe_tagscheme:if_fun_get_arity_and_address(ArityReg,
						NAddressReg, Fun, 
						hipe_rtl:label_name(BadFunLab),
						0.9),
  CheckArityCode = check_arity(ArityReg, length(Args), 
			       hipe_rtl:label_name(BadArityLab)),

  CallCode = 
    case Continuation of
      [] -> %% This is a tailcall
	[hipe_rtl:mk_enter(NAddressReg, 
			   ArgsAndFun,
			   closure)]; 
      _ -> %% Ordinary call
	[hipe_rtl:mk_call(Dst, NAddressReg, 
			  ArgsAndFun,
			  closure, 
			  Continuation, Fail)]
    end,
  [CheckGetCode,CheckArityCode, CallCode, FailCode].


check_arity(ArityReg, Arity, BadArityLab) ->
  TrueLab1 = hipe_rtl:mk_new_label(),
  _ArityCheckCode = 
    [hipe_rtl:mk_branch(ArityReg, eq, hipe_rtl:mk_imm(Arity),  
			hipe_rtl:label_name(TrueLab1), BadArityLab,
			0.9),
     TrueLab1].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% apply
%% NYI.
%%

%%gen_apply(Dst, Args, Cont, Fail, ExitInfo) ->
%%  Lab1 = hipe_rtl:mk_new_label(),
%%  Lab2 = hipe_rtl:mk_new_label(),
%%  CallEmuLab = hipe_rtl:mk_new_label(),
%%  CallDirectLab = hipe_rtl:mk_new_label(),
%%  ArityR = hipe_rtl:mk_new_reg(), 
%%  MFAReg =  hipe_rtl:mk_new_reg(), 
%%  AtomNoReg =  hipe_rtl:mk_new_reg(), 
%%  RequestReg =  hipe_rtl:mk_new_reg(), 
%%  AddressReg =  hipe_rtl:mk_new_reg(), 

%%  [M,F,AppArgs] = Args,

%%  [hipe_rtl:mk_call(ArityR, {erlang,length,1} , [AppArgs], c,  
%%		    hipe_rtl:label_name(Lab1), Fail),
%%   Lab1,
%%   hipe_rtl:mk_move(AtomNoReg, hipe_rtl:mk_imm(native_address))] ++
%%   gen_mk_tuple(MFAReg, [M,F,ArityR], []) ++
%%   gen_mk_tuple(RequestReg, [MFAReg,AtomNoReg], []) ++
%%    [
%%     hipe_rtl:mk_call(AddressReg, 
%%		      {hipe_bifs,get_funinfo,1} , [RequestReg], c,  
%%		      hipe_rtl:label_name(Lab2), Fail),
%%     Lab2,
%%     hipe_rtl:mk_branch(AddressReg, eq, 
%%			hipe_rtl:mk_imm(hipe_tagscheme:mk_nil()), 
%%			 hipe_rtl:label_name(CallEmuLab), 
%%			hipe_rtl:label_name(CallDirectLab), 0.01),
%%     CallEmuLab,
%%     generic_primop(Dst, {hipe_internal, apply, 3}, 
%%			   Args, Cont, Fail, ExitInfo),
%%     CallDirectLab,
%%     hipe_rtl:mk_call(Dst, AddressReg, 
%%			  AppArgs,
%%			  closure, 
%%			  Cont, 
%%		      Fail)].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% mkTuple
%%

gen_mk_tuple(Dst, Elements, Options) ->
  {GetHPInsn, HP, PutHPInsn} = hipe_rtl_arch:heap_pointer(),
  Arity = length(Elements),

  Code = [
	  GetHPInsn,
	  gen_tuple_header(HP, Arity),
	  set_tuple_elements(HP, 4, Elements, []),
	  hipe_tagscheme:tag_tuple(Dst, HP),
	  hipe_rtl:mk_alu(HP, HP, add, hipe_rtl:mk_imm((Arity+1)*4)),
	  PutHPInsn],
  case ?AddGC(Options) of
    true -> [hipe_rtl:mk_gctest(Arity + 1)|Code];
    false -> Code
  end.

set_tuple_elements(HP, Offset, [Element|Elements], Stores) ->
  Store = hipe_rtl:mk_store(HP, hipe_rtl:mk_imm(Offset), Element),
  set_tuple_elements(HP, Offset + 4, Elements, [Store | Stores]);
set_tuple_elements(_, _, [], Stores) ->
  lists:reverse(Stores).

%%
%% Reduction test
%%
gen_redtest(Amount) ->
  Reds = hipe_rtl:mk_reg(hipe_rtl_arch:fcalls_reg()),
  SuspendLabel = hipe_rtl:mk_new_label(),
  StayLabel = hipe_rtl:mk_new_label(),
  [hipe_rtl:mk_alub(Reds, Reds, 'sub', hipe_rtl:mk_imm(Amount), 'lt',
		    hipe_rtl:label_name(SuspendLabel),
		    hipe_rtl:label_name(StayLabel), 0.01),
   SuspendLabel,
   hipe_rtl:mk_call([], suspend_0, [], c, hipe_rtl:label_name(StayLabel), []),
   StayLabel].

%%
%% Generate unsafe head
%%
gen_unsafe_hd(Dst, [Arg]) -> hipe_tagscheme:unsafe_car(Dst, Arg).

%%
%% Generate unsafe tail
%%
gen_unsafe_tl(Dst, [Arg]) -> hipe_tagscheme:unsafe_cdr(Dst, Arg).

%%
%% element
%%
gen_element_2(Dst, Fail, Index, Tuple, Cont, _Annot, 
	      ExitInfo, TupleInfo, IndexInfo) ->
  FailLbl = hipe_rtl:mk_new_label(),
  FailCode = hipe_rtl_exceptions:gen_fail_code(Fail, Dst, badarg, ExitInfo),
  [hipe_tagscheme:element(Dst, Index, Tuple, FailLbl, TupleInfo, IndexInfo),
   hipe_rtl:mk_goto(Cont),
   FailLbl,
   FailCode].

%%
%% unsafe element
%%
gen_unsafe_element(Dst, Index, Tuple) ->
  case hipe_rtl:is_imm(Index) of
    true -> hipe_tagscheme:unsafe_constant_element(Dst, Index, Tuple);
    false -> ?EXIT({illegal_index_to_unsafe_element,Index})
	       end.

gen_unsafe_update_element(Tuple, Index, Value) ->
  case hipe_rtl:is_imm(Index) of
    true -> 
      hipe_tagscheme:unsafe_update_element(Tuple, Index, Value);
    false -> ?EXIT({illegal_index_to_unsafe_update_element,Index})
	       end.


gen_closure_element(Dst, Index, Closure) ->
  hipe_tagscheme:unsafe_closure_element(Dst, Index, Closure).

%%
%% Generate code that writes a tuple header
%%
gen_tuple_header(Ptr, Arity) ->
  Header = hipe_tagscheme:mk_arityval(Arity),
  hipe_rtl:mk_store(Ptr, hipe_rtl:mk_imm(0), hipe_rtl:mk_imm(Header)).


%% ____________________________________________________________________
%%
%% Floating point handling 
%%

gen_fclearerror() ->
  Addr = hipe_rtl:mk_new_reg(),
  [hipe_rtl:mk_load_address(Addr, erl_fp_exception, c_const),
   hipe_rtl:mk_store(Addr, hipe_rtl:mk_imm(0), hipe_rtl:mk_imm(0))].

gen_fcheckerror(ContLbl, FailLbl, ExitInfo)->
  Tmp = hipe_rtl:mk_new_reg(),
  TmpFailLbl0 = hipe_rtl:mk_new_label(),
  TmpFailLbl1 = hipe_rtl:mk_new_label(),
  Result = hipe_rtl:mk_new_var(),

  Addr = hipe_rtl:mk_new_reg(),

  FailCode = fp_fail_code(TmpFailLbl0, TmpFailLbl1, 
			  FailLbl, Result, ExitInfo),
  ExceptionAtom = 
    case get(hipe_target_arch) of
      x86 -> erl_fp_check_exception;
      ultrasparc -> erl_fp_exception
    end,
  
  [hipe_rtl:mk_load_address(Addr, ExceptionAtom, c_const),
   hipe_rtl:mk_load(Tmp, Addr, hipe_rtl:mk_imm(0)),
   hipe_rtl:mk_branch(Tmp, eq, hipe_rtl:mk_imm(0), 
		      ContLbl, hipe_rtl:label_name(TmpFailLbl0))]++
    FailCode.

gen_conv_to_float(Dst, [Src], ContLbl, FailLbl, ExitInfo) ->
  case hipe_rtl:is_var(Src) of
    true ->
      Tmp = hipe_rtl:mk_new_var(),
      TmpReg = hipe_rtl:mk_new_reg(),
      TrueFixNum = hipe_rtl:mk_new_label(),
      ContFixNum = hipe_rtl:mk_new_label(),
      TrueFp = hipe_rtl:mk_new_label(),
      ContFp = hipe_rtl:mk_new_label(),
      ContBigNum = hipe_rtl:mk_new_label(),
      TestFixNum = hipe_tagscheme:test_fixnum(Src, hipe_rtl:label_name(TrueFixNum),
					      hipe_rtl:label_name(ContFixNum), 0.5),
      TestFp = hipe_tagscheme:test_flonum(Src, hipe_rtl:label_name(TrueFp),
					  hipe_rtl:label_name(ContFp), 0.5),
      GotoCont = hipe_rtl:mk_goto(ContLbl),
      TmpFailLbl0 = hipe_rtl:mk_new_label(),
      TmpFailLbl1 = hipe_rtl:mk_new_label(),
      Result = hipe_rtl:mk_new_var(),
      FailCode = fp_fail_code(TmpFailLbl0, TmpFailLbl1, 
			      FailLbl, Result, ExitInfo),

      TestFixNum ++
	[TrueFixNum, 
    	 hipe_tagscheme:untag_fixnum(TmpReg, Src),
	 hipe_rtl:mk_fconv(Dst,TmpReg),
	 GotoCont,
	 ContFixNum] ++ 
	TestFp ++
	[TrueFp, 
	 hipe_tagscheme:unsafe_untag_float(Dst, Src), 
	 GotoCont, 
	 ContFp] ++
	[hipe_rtl:mk_call([Tmp],conv_big_to_float,[Src], c,
			  hipe_rtl:label_name(ContBigNum),
			  hipe_rtl:label_name(TmpFailLbl0))]++
	FailCode ++
	[ContBigNum,
	 hipe_tagscheme:unsafe_untag_float(Dst, Tmp)];
    _ ->
      %% This must be an attempt to convert an illegal term.
      Result = hipe_rtl:mk_new_var(),
      [hipe_rtl_exceptions:gen_fail_code(FailLbl, Result, badarith, ExitInfo)]
  end.

fp_fail_code(TmpFailLbl0, TmpFailLbl1, FailLbl, Result, ExitInfo)->
  case get(hipe_target_arch) of
    x86 ->
      [TmpFailLbl0,
       hipe_rtl:mk_call([], handle_fp_exception, [], c,
			hipe_rtl:label_name(TmpFailLbl1), []),
       TmpFailLbl1,
       hipe_rtl_exceptions:gen_fail_code(FailLbl, Result, 
					 badarith, ExitInfo)];
    ultrasparc ->
      [TmpFailLbl0,
       hipe_rtl_exceptions:gen_fail_code
       (FailLbl, Result, badarith, ExitInfo)]
  end.
  
%% ____________________________________________________________________
%%
%% Type handling.
%%

type('+', Args)->
  erl_bif_types:type(erlang, '+', 2, Args);
type('-', Args)->
  erl_bif_types:type(erlang, '-', 2, Args);
type(cons, [HeadType, TailType])->
  erl_types:t_cons(HeadType, TailType);
type(unsafe_tl, [Type]) ->
  case erl_types:t_is_cons(Type) of
    true -> erl_types:t_cons_tl(Type);
    _ -> erl_types:t_undefined()
  end;
type(unsafe_hd, [Type]) ->
  case erl_types:t_is_cons(Type) of
    true -> erl_types:t_cons_hd(Type);
    _ -> erl_types:t_undefined()
  end;
type(mktuple, TypeList) ->
  erl_types:t_tuple(TypeList);
type(unsafe_element, [IndexType, TupleType]) ->
  case erl_types:t_number_vals(IndexType) of
    [N] when is_integer(N)->
      type({unsafe_element, N}, TupleType);
    _ ->
      case erl_types:t_is_tuple(TupleType) of
	false ->
	  erl_types:t_any();
	_ ->
	  case erl_types:t_tuple_args(TupleType) of
	    [H|T] = List when is_list(List) ->
	      case lists:all(fun(X)->X=:=H end, T) of
		true -> H;
		_ -> erl_types:t_any()
	      end;
	    _ ->
	      erl_types:t_any()
	  end
      end
  end;
type({unsafe_element, N}, [Type]) ->
  case erl_types:t_is_tuple(Type) of
    false ->
      erl_types:t_any();  
    _ ->
      case erl_types:t_tuple_args(Type) of
	ArgTypes when is_list(ArgTypes) ->
	  lists:nth(N, ArgTypes);
	T -> T
      end
  end;
type(unsafe_tag_float, _) ->
  erl_types:t_float();
type('bor', Args)->
  erl_bif_types:type(erlang, 'bor', 2, Args);
type('band', Args)->
  erl_bif_types:type(erlang, 'band', 2, Args);
type('bxor', Args)->
  erl_bif_types:type(erlang, 'bxor', 2, Args);
type('bnot', Args)->
  erl_bif_types:type(erlang, 'bnot', 2, Args);
type({hipe_bs_primop, {bs_get_integer, _, _}}, _) ->
  %%TODO: Here we could find out if this is a fixnum.
  erl_types:t_integer();
type({hipe_bs_primop, {bs_get_float, _, _}}, _) ->
  erl_types:t_float();
type({hipe_bs_primop, {bs_get_binary, _, _}}, _) ->
  erl_types:t_binary();
type({hipe_bs_primop, {bs_get_binary_all, _}}, _) ->
  erl_types:t_binary();
type({hipe_bs_primop, bs_final}, _) ->
  erl_types:t_binary();
type(_Op, _) ->
  %%io:format("Don't have any information for ~w\n", [Op]),
  erl_types:t_any().

