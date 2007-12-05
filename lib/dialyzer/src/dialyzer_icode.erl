%% -*- erlang-indent-level: 2 -*-
%%--------------------------------------------------------------------
%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% Copyright 2006, 2007 Tobias Lindahl and Kostis Sagonas
%% 
%%     $Id$
%%

%%%-------------------------------------------------------------------
%%% File    : dialyzer_icode.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description : 
%%%
%%% Created : 15 Apr 2005 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------
-module(dialyzer_icode).

-export([run_analysis/5]).

-include("hipe_icode.hrl").
-include("dialyzer.hrl").
-define(TYPE_DEPTH, 3).

-import(erl_types, [t_any/0, t_atom/1, t_atom/0, t_atom_vals/1,
		    t_binary/0, t_bool/0, t_cons/0, t_constant/0, 
		    t_maybe_improper_list/0,
		    t_float/0, t_from_term/1, t_fun/0, t_fun/1, t_fun/2,
		    t_fun_args/1, t_fun_arity/1,
		    t_fun_range/1,t_inf/2, t_inf_lists/2, 
		    t_integer/0,
		    t_integer/1, t_is_atom/1, t_is_atom/2, 
		    t_is_any/1, t_is_binary/1,
		    t_is_bool/1, t_is_cons/1, t_is_constant/1,
		    t_is_maybe_improper_list/1, t_is_equal/2, t_is_float/1,
		    t_is_fun/1, t_is_integer/1, t_is_number/1,
		    t_is_list/1, t_is_nil/1, t_is_port/1, t_is_pid/1,
		    t_is_ref/1, t_is_subtype/2, 
		    t_is_tuple/1,
		    t_is_none/1, t_limit/2, t_list/0, t_nil/0,
		    t_number/0, t_number/1, t_number_vals/1, t_pid/0,
		    t_port/0, t_product/1, t_ref/0, t_subtract/2, 
		    t_sup/1, t_sup/2,
		    t_to_string/1, t_tuple/0, t_tuple/1,
		    t_tuple_arities/1, t_none/0]).


-record(state, {succmap, predmap, info_map, cfg, liveness, arg_types,
		created_funs=[]}).

%%-define(DEBUG, true).

run_analysis(Cfg, IcodeFun, Plt, NoWarnUnused, SendWarn) ->
  OldSig = init_mfa_info(IcodeFun, Plt),
  State = analyze(Cfg),
  pp(State, IcodeFun, "Pre-specialization"),
  NewState = cfg_loop(State),
  pp(NewState, IcodeFun, "Post-specialization"),
  Warnings =
    case SendWarn of
      true -> warn_on_type_errors(Cfg, State, NewState, NoWarnUnused, IcodeFun);
      false -> []
    end,
  return(NewState, IcodeFun, OldSig, Warnings).

cfg_loop(State) ->
  Labels = hipe_icode_cfg:reverse_postorder(state__cfg(State)),  
  simplify_controlflow(Labels, State).


return(State, IcodeFun, OldSig, Warnings) ->
  Labels = hipe_icode_cfg:labels(state__cfg(State)),
  {IcodeReturn0, _, _} = find_return_type(Labels, State),
  IcodeArgs = State#state.arg_types,
  case t_is_none(IcodeReturn0) of
    true ->
      IcodeReturn = t_any();
    false ->
      IcodeReturn = IcodeReturn0
  end,
  
  NewArgs =  
    case t_fun_args(OldSig) of
      any -> IcodeArgs;
      PltArgs -> PltArgs
    end,
  
  Fixpoint =   
    case t_is_equal(IcodeReturn, t_fun_range(OldSig)) of
      true -> fixpoint;
      false -> not_fixpoint
    end,
  {Fixpoint, {IcodeFun, IcodeReturn, NewArgs}, Warnings}.


%% _________________________________________________________________
%% 
%% Global type analysis on the whole function. Demands that the code
%% is in SSA-form. When we encounter a phi-node, the types of the
%% arguments are joined. At the end of a block the information out is
%% joined with the current information in for all _valid_ successors,
%% that is, of all successors that actually can be reached. If the
%% join produces new information in for the successor, this
%% information is added to the worklist.
%%

analyze(Cfg) ->
  %%hipe_icode_cfg:pp(Cfg),
  State1 = new_state(Cfg),
  analyze_blocks(State1).

analyze_blocks(State) ->
  Work = init_work(State),
  analyze_blocks(Work, State).

analyze_blocks(Work, State) ->
  case get_work(Work) of
    fixpoint ->
      State;
    {Label, NewWork} ->
      Info = state__info_in(State, Label),
      {NewState2, NewLabels} = analyze_block(Label, Info, State),
      NewWork2 = add_work(NewWork, NewLabels),
      analyze_blocks(NewWork2, NewState2)
  end.
  
analyze_block(Label, InfoIn, State) ->
  %%io:format("Handling ~w\n", [Label]),
  BB = state__bb(State, Label),
  Code = hipe_bb:butlast(BB),
  Last = hipe_bb:last(BB),
  InfoOut = analyze_insns(Code, InfoIn),
  NewState = state__info_out_update(State, Label, InfoOut),

  case Last of
    #'if'{} ->
      UpdateInfo = do_if(Last, InfoOut),
      do_updates(NewState, UpdateInfo);
    #type{} ->
      UpdateInfo = do_type(Last, InfoOut),
      do_updates(NewState, UpdateInfo);
    #switch_tuple_arity{} ->
      UpdateInfo = do_switch_tuple_arity(Last, InfoOut),
      do_updates(NewState, UpdateInfo);
    #switch_val{} ->
      UpdateInfo = do_switch_val(Last, InfoOut),
      do_updates(NewState, UpdateInfo);
    #call{} ->
      {DstType, NewArgTypes} = do_call(Last, InfoOut),
      NewInfoOut = enter_defines(Last, DstType, InfoOut),
      NewState1 = state__info_out_update(NewState, Label, NewInfoOut),
      ContInfo = enter_lists(args(Last), NewArgTypes, NewInfoOut),
      Cont = hipe_icode:call_continuation(Last),
      Fail = hipe_icode:call_fail_label(Last),
      UpdateInfo =
	case Fail of
	  [] ->
	    case call_always_fails(Last, InfoOut) of
	       true ->
		[];
	      false ->
		[{Cont, ContInfo}]
	    end;
	  _ ->
	    case call_always_fails(Last, InfoOut) of
	      true ->
		[{Fail, NewInfoOut}];
	      false ->
		Fun = hipe_icode:call_fun(Last),
		case hipe_icode_primops:fails(Fun) of
		  true ->
		    [{Cont, ContInfo}, {Fail, NewInfoOut}];
		  false ->
		    [{Cont, ContInfo}]
		end
	    end
	end,
      do_updates(NewState1, UpdateInfo);
    _ ->
      UpdateInfo = [{X, InfoOut}||X<-state__succ(NewState, Label)],
      do_updates(NewState, UpdateInfo)
  end.

analyze_insns([I|Insns], Info) ->
  NewInfo = analyze_insn(I, Info),
  analyze_insns(Insns, NewInfo);
analyze_insns([], Info) ->
  Info.

analyze_insn(I, Info) ->
  case I of
    #move{} ->
      do_move(I, Info);
    #call{} ->
      {DstType, NewArgTypes} = do_call(I, Info),
      NewInfo = enter_defines(I, DstType, Info),
      enter_lists(args(I), NewArgTypes, NewInfo);
    #phi{} ->
      Type = t_limit(join_list(args(I), Info), ?TYPE_DEPTH),
      enter_defines(I, Type, Info);
    #begin_handler{} ->
      enter_defines(I, t_any(), Info);
    _ ->
      %% Just an assert
      case defines(I) of
	[] -> Info;
	_ -> erlang:error({"Instruction with destination not analyzed", I})
      end
  end.

do_move(I, Info) ->
  %% Can't use uses/1 since we must keep constants.
  [Src] = args(I),
  case hipe_icode:is_const(Src) of
    true -> 
      enter_defines(I, const_type(Src), Info);
    false ->
      %% Make the destination point to the source.
      enter_defines(I, Src, Info)
  end.

do_call(I, Info) ->
  Fun = hipe_icode:call_fun(I),
  CallType = hipe_icode:call_type(I),
  call_or_enter_type(I, Fun, args(I), Info, CallType).

call_or_enter_type(_I, Fun, Args, Info, primop) ->
  ArgTypes = lookup_list(Args, Info),
  DstType = primop_type(Fun, ArgTypes),      
  case t_fun_args(find_signature_primop(Fun, length(ArgTypes))) of
    any -> {DstType, ArgTypes};
    ArgConstr -> {DstType, t_inf_lists(ArgTypes, ArgConstr)}
  end;
call_or_enter_type(I, MFA, Args, Info, OtherType) when OtherType =:= local;
						       OtherType =:= remote ->
  ArgTypes = lookup_list(Args, Info),
  PltSig = find_signature(MFA, ArgTypes),
  {M, F, A} = MFA,
  BifType = erl_bif_types:type(M, F, A, ArgTypes),
  %% XXX: This should be removed when the annotated form is not used anymore.
  AnnotatedType =
    case I of
      #call{} ->
	case hipe_icode:call_dst_type(I) of
	  [] -> t_any();
	  T -> 
	    %% Make sure that the core type pass has not 
	    %% annotated this with none()
	    case t_is_none(T) of
	      true -> t_any();
	      false -> T
	    end
	end;
      #enter{} ->
	t_any()
    end,
  DstType = t_inf(AnnotatedType, t_inf(BifType, t_fun_range(PltSig))),
  case t_fun_args(PltSig) of
    any ->
      {DstType, ArgTypes};
    ArgConstr ->      
      NewArgTypes = t_inf_lists(ArgTypes, ArgConstr),
      case any_is_none(NewArgTypes) of
	true -> {t_none(), NewArgTypes};
	false -> {DstType, NewArgTypes}
      end
  end.

do_if(I, Info) ->
  %%% XXX: Could probably do better than this.
  TrueLab = hipe_icode:if_true_label(I),
  FalseLab = hipe_icode:if_false_label(I),
  case hipe_icode:if_args(I) of
    [Arg1, Arg2] = Args ->
      [Type1, Type2] = lookup_list(Args, Info),
      %% io:format("Type1 = ~w, Type2 = ~w\n", [Type1, Type2]),
      case t_is_none(Type1) orelse t_is_none(Type2) of
	true ->
	  [{TrueLab, Info}, {FalseLab, Info}];
	false ->
	  Inf = t_inf(Type1, Type2),
	  case hipe_icode:if_op(I) of
	    '=:='->
	      case t_is_none(Inf) of
		true -> [{FalseLab, Info}];
		false -> [{TrueLab, enter(Arg1, Inf, enter(Arg2, Inf, Info))}, 
			  {FalseLab, Info}]
	      end;
	    '=/=' ->
	      case t_is_none(Inf) of
		true -> 
		  [{TrueLab, Info}];
		false ->
		  [{FalseLab, enter(Arg1, Inf, enter(Arg2, Inf, Info))}, 
		   {TrueLab, Info}]
	      end;
	    Op ->
	      Eval = erl_bif_types:type(erlang, Op, 2, [Type1, Type2]),
	      case t_is_atom(true, Eval) of
		true -> [{TrueLab, Info}];
		false ->
		  case t_is_atom(false, Eval) of
		    true -> [{FalseLab, Info}];
		    false -> [{TrueLab, Info}, {FalseLab, Info}]
		  end
	      end
	  end
      end;
    _ ->
      %% Only care for binary if:s
      [{TrueLab, Info}, {FalseLab, Info}]
  end.

do_type(I, Info) ->
  case args(I) of
    [Var] -> do_type(I, Info, Var);
    [Var1,Var2] -> do_type2(I, Info, Var1, Var2)
  end.

do_type2(I, Info, FunVar, ArityVar) -> % function2(Fun,Arity)
  %% Just for sanity.
  function2 = hipe_icode:type_type(I),  
  FunType = lookup(FunVar, Info),
  ArityType = lookup(ArityVar, Info),
  TrueLab = hipe_icode:type_true_label(I),
  FalseLab = hipe_icode:type_false_label(I),
  case combine_test(test_type(function, FunType), 
		    test_type(integer, ArityType)) of
    true ->
      Arity = t_fun_arity(FunType),
      case t_number_vals(ArityType) of
	any ->
	  if Arity =/= any -> [{TrueLab, Info}, {FalseLab, Info}];
	     true -> [{TrueLab, enter(ArityVar, t_from_term(Arity), Info)},
		      {FalseLab, Info}]
	  end;
	[Arity] ->
	  [{TrueLab, Info}];
	List ->
	  case lists:member(Arity, List) of
	    true ->
	      T = t_from_term(Arity), 
	      [{TrueLab, enter(ArityVar, T, Info)},
	       {FalseLab, enter(ArityVar, t_subtract(ArityType, T), Info)}];
	    false ->
	      [{FalseLab, Info}]
	  end
      end;
    false ->
      [{FalseLab, Info}];
    maybe ->
      GenTrueArity = t_inf(t_integer(), ArityType),
      GenTrueFun = t_inf(t_fun(), FunType),
      case {t_number_vals(GenTrueArity), t_fun_arity(GenTrueFun)} of
	{any, any} -> 
	  TrueInfo = enter_lists([FunVar, ArityVar], 
				 [GenTrueFun, GenTrueArity], Info),
	  [{TrueLab, TrueInfo}, {FalseLab, Info}];
	{any, Arity} when is_integer(Arity) ->
	  TrueInfo = enter_lists([FunVar, ArityVar], 
				 [GenTrueFun, t_integer(Arity)], Info),
	  [{TrueLab, TrueInfo}, {FalseLab, Info}];
	{[Val], any} when is_integer(Val) ->
	  TrueInfo = enter_lists([FunVar, ArityVar], 
				 [t_inf(GenTrueFun, t_fun(Val, t_any())),
				  GenTrueArity], Info),
	  [{TrueLab, TrueInfo}, {FalseLab, Info}];
	{Vals, any} when is_list(Vals) ->
	  %% The function type gets widened when we have more than one arity.
	  TrueInfo = enter_lists([FunVar, ArityVar], 
				 [GenTrueFun, GenTrueArity], Info),
	  [{TrueLab, TrueInfo}, {FalseLab, Info}];
	{Vals, Arity} when is_list(Vals), is_integer(Arity) ->
	  case lists:member(Arity, Vals) of
	    false ->
	      [{FalseLab, Info}];
	    true ->
	      TrueInfo = enter_lists([FunVar, ArityVar], 
				     [GenTrueFun, t_integer(Arity)], Info),
	      [{TrueLab, TrueInfo}, {FalseLab, Info}]
	  end
      end
  end.

combine_test(true, true) -> true;
combine_test(false, _)   -> false;
combine_test(_, false)   -> false;
combine_test(_, _)       -> maybe.


do_type(I, Info, Var) ->
  TrueLab = hipe_icode:type_true_label(I),
  FalseLab = hipe_icode:type_false_label(I),
  None = t_none(),
  
  case lookup(Var, Info) of
    None ->
      [{TrueLab, Info}, {FalseLab, Info}];
    VarInfo ->
      case hipe_icode:type_type(I) of
	cons ->
	  test_cons_or_nil(t_cons(), Var, VarInfo, TrueLab, FalseLab, Info);
	nil ->
	  test_cons_or_nil(t_nil(), Var, VarInfo, TrueLab, FalseLab, Info);
	{atom, A} ->
	  test_number_or_atom(fun(X) -> t_atom(X) end,
			      A, Var, VarInfo, {atom, A}, 
			      TrueLab, FalseLab, Info);
	{integer, N} ->
	  test_number_or_atom(fun(X) -> t_number(X) end, 
			      N, Var, VarInfo, {integer, N}, 
			      TrueLab, FalseLab, Info);
	{record, Atom, Size} ->
	  test_record(Atom, Size, Var, VarInfo, TrueLab, FalseLab, Info);
	Other ->
	  case t_is_any(VarInfo) of
	    true ->
	      TrueType = t_inf(true_branch_info(Other), VarInfo),
	      TrueInfo = enter(Var, TrueType, Info),
	      [{TrueLab, TrueInfo}, {FalseLab, Info}];
	    false ->
	      case test_type(Other, VarInfo) of
		true ->
		  [{TrueLab, Info}];
		false ->
		  [{FalseLab, Info}];
		maybe ->
		  TrueType = t_inf(true_branch_info(Other), VarInfo),
		  TrueInfo = enter(Var, TrueType, Info),
		  FalseType = t_subtract(VarInfo, TrueType),
		  FalseInfo = enter(Var, FalseType, Info),
		  [{TrueLab, TrueInfo}, {FalseLab, FalseInfo}]
	      end
	  end
      end
  end.

do_switch_tuple_arity(I, Info) ->
  Arg = hipe_icode:switch_tuple_arity_arg(I),
  ArgType = lookup(Arg, Info),
  Cases = hipe_icode:switch_tuple_arity_cases(I),
  FailLabel = hipe_icode:switch_tuple_arity_fail_label(I),
  case legal_switch_tuple_arity_cases(Cases, ArgType) of
    [] ->
      [{FailLabel, Info}];
    LegalCases ->      
      {Fail, UpdateInfo} =
	switch_tuple_arity_update_info(LegalCases, Arg, ArgType, 
				       FailLabel, ArgType, Info, []),
      case switch_tuple_arity_can_fail(LegalCases, ArgType) of
	true -> [Fail|UpdateInfo];
	false -> UpdateInfo
      end
  end.

legal_switch_tuple_arity_cases(Cases, Type) ->
  case t_is_tuple(Type) of
    false -> 
      Inf = t_inf(t_tuple(), Type),
      case t_is_tuple(Inf) of
	true -> legal_switch_tuple_arity_cases_1(Cases, Inf);
	false -> []
      end;
    true ->
      legal_switch_tuple_arity_cases_1(Cases, Type)
  end.

legal_switch_tuple_arity_cases_1(Cases, Type) ->
  case t_tuple_arities(Type) of
    any -> 
      Cases;
    TupleArities ->
      [{Arity, Label} || {Arity, Label} <- Cases, 
			 lists:member(hipe_icode:const_value(Arity), 
				      TupleArities)]
  end.

switch_tuple_arity_can_fail(LegalCases, ArgType) ->
  case t_is_tuple(ArgType) of
    false -> true;
    true ->
      case t_tuple_arities(ArgType) of
	any -> true;
	Arities1 ->
	  Arities2 = [hipe_icode:const_value(X) || {X, _} <- LegalCases],
	  Set1 = sets:from_list(Arities1),
	  Set2 = sets:from_list(Arities2),
	  not sets:is_subset(Set1, Set2)
      end
  end.

switch_tuple_arity_update_info([{Arity, Label}|Left], Var, TupleType, 
			       FailLabel, FailType, Info, Acc) ->
  Inf = t_inf(TupleType, t_tuple(hipe_icode:const_value(Arity))),
  NewInfo = enter(Var, Inf, Info),
  NewFailType = t_subtract(FailType, Inf),
  switch_tuple_arity_update_info(Left, Var, TupleType, FailLabel, NewFailType,
				 Info, [{Label, NewInfo}|Acc]);
switch_tuple_arity_update_info([], Var, _TupleType, 
			       FailLabel, FailType, Info, Acc) ->
  {{FailLabel, enter(Var, FailType, Info)}, Acc}.


do_switch_val(I, Info) ->
  Arg = hipe_icode:switch_val_arg(I),
  ArgType = lookup(Arg, Info),
  Cases = hipe_icode:switch_val_cases(I),
  FailLabel = hipe_icode:switch_val_fail_label(I),
  case legal_switch_val_cases(Cases, ArgType) of
    [] ->
      [{FailLabel, Info}];
    LegalCases ->
      switch_val_update_info(LegalCases, Arg, ArgType, 
			     FailLabel, ArgType, Info, [])
  end.

legal_switch_val_cases(Cases, Type) ->
  legal_switch_val_cases(Cases, Type, []).

legal_switch_val_cases([{Val, Label}|Left], Type, Acc) ->
  ConstType = t_from_term(hipe_icode:const_value(Val)),
  case t_is_subtype(ConstType, Type) of
    true ->
      legal_switch_val_cases(Left, Type, [{Val, Label}|Acc]);
    false ->
      legal_switch_val_cases(Left, Type, Acc)
  end;
legal_switch_val_cases([], _Type, Acc) ->
  lists:reverse(Acc).

switch_val_update_info([{Const, Label}|Left], Arg, ArgType, 
		       FailLabel, FailType, Info, Acc) ->
  TrueType = t_from_term(hipe_icode:const_value(Const)),
  NewInfo = enter(Arg, TrueType, Info),
  NewFailType = t_subtract(FailType, TrueType),
  switch_val_update_info(Left, Arg, ArgType, FailLabel, NewFailType,
			 Info, [{Label, NewInfo}|Acc]);
switch_val_update_info([], Arg, _ArgType, FailLabel, FailType,Info, Acc) ->
  [{FailLabel, enter(Arg, FailType, Info)}|Acc].

test_cons_or_nil(Type, Var, VarInfo, TrueLab, FalseLab, Info) ->
  case t_is_any(VarInfo) of
    true -> 
      [{TrueLab, enter(Var, Type, Info)},
       {FalseLab, Info}];
    false ->      
      TrueType = t_inf(VarInfo, Type),
      FalseType = t_subtract(VarInfo, TrueType),
      case t_is_none(FalseType) of
	true ->
	  [{TrueLab, Info}];
	false ->
	  case t_is_none(TrueType) of
	    true ->
	      [{FalseLab, Info}];
	    false ->
	      [{TrueLab, enter(Var, TrueType, Info)},
	       {FalseLab, enter(Var, FalseType, Info)}]
	  end
      end
  end.

test_number_or_atom(Fun, X, Var, VarInfo, TypeTest,
		    TrueLab, FalseLab, Info) ->
  case t_is_any(VarInfo) of
    true ->
      [{TrueLab, enter(Var, Fun(X), Info)},
       {FalseLab, Info}];
    false ->
      case test_type(TypeTest, VarInfo) of
	false ->
	  [{FalseLab, Info}];
	true ->
	  [{TrueLab, Info}];
	maybe ->
	  FalseType = t_subtract(VarInfo, Fun(X)),
	  [{TrueLab, enter(Var, Fun(X), Info)},
	   {FalseLab, enter(Var, FalseType, Info)}]
      end
  end.

test_record(Atom, Size, Var, VarInfo, TrueLab, FalseLab, Info) ->
  AnyList = lists:duplicate(Size - 1, t_any()),
  RecordType = t_tuple([t_atom(Atom)|AnyList]),
  Inf = t_inf(RecordType, VarInfo),
  case t_is_none(Inf) of
    true ->
      [{FalseLab, Info}];
    false ->
      Sub = t_subtract(VarInfo, Inf),
      case t_is_none(Sub) of
	true ->
	  [{TrueLab, enter(Var, Inf, Info)}];
	false ->
	  [{TrueLab, enter(Var, Inf, Info)},
	   {FalseLab, enter(Var, Sub, Info)}]
      end
  end.

test_type(Test, Type) ->
  case t_is_any(Type) of
    true -> maybe;
    false ->
      TrueTest = true_branch_info(Test),
      Inf = t_inf(TrueTest, Type),
      case t_is_equal(Type, Inf) of
	true ->
	  not t_is_none(Type);
	false ->
	  case t_is_equal(TrueTest, Inf) of
	    true ->
	      case test_type0(Test, Type) of
		false ->
		  maybe;
		true ->
		  true;
		maybe ->
		  maybe
	      end;
	    false ->
	      case test_type0(Test, Inf) of
		true ->
		  maybe;
		false ->
		  false;
		maybe ->
		  maybe
	      end
	  end
      end
  end.

test_type0(integer, T) ->
  t_is_integer(T);
test_type0({integer, N}, T) ->
  case t_is_integer(T) of
    true -> 
      case t_number_vals(T) of
	[N] -> true;
	List when is_list(List) -> 
	  case lists:member(N, List) of
	    true -> maybe;
	    false -> false
	  end;
	any ->
	  maybe
      end;
    false -> false
  end;
test_type0(float, T) ->
  t_is_float(T);  
test_type0(number, T) ->
  t_is_number(T);
test_type0(atom, T) ->
  t_is_atom(T);
test_type0({atom, A}, T) ->
  case t_is_atom(T) of
    true -> 
      case t_atom_vals(T) of
	[A] -> true;
	List when is_list(List) -> 
	  case lists:member(A, List) of
	    true -> maybe;
	    false -> false
	  end;
	any ->
	  maybe
      end;
    false -> false
  end;
test_type0(tuple, T) ->
  t_is_tuple(T);
test_type0({tuple, N}, T) ->
  case t_is_tuple(T) of
    true -> 
      case t_tuple_arities(T) of
	[N] -> true;
	[X] when is_integer(X) -> false;
	List when is_list(List) ->
	  case lists:member(N, List) of
	    true -> maybe;
	    false -> false
	  end;
	any ->
	  maybe
      end;
    false -> false
  end;
test_type0(pid, T) ->
  t_is_pid(T);
test_type0(port, T) ->
  t_is_port(T);
test_type0(binary, T) ->
  t_is_binary(T);
test_type0(reference, T) ->
  t_is_ref(T);
test_type0(function, T) ->
  t_is_fun(T);
test_type0(boolean, T) ->
  t_is_bool(T);
test_type0(list, T) ->
  t_is_maybe_improper_list(T);
test_type0(cons, T) ->
  t_is_cons(T);
test_type0(nil, T) ->
  t_is_nil(T);
test_type0(constant, T) ->
  t_is_constant(T).


true_branch_info(integer) ->
  t_integer();
true_branch_info({integer, N}) ->
  t_integer(N);
true_branch_info(float) ->
  t_float();  
true_branch_info(number) ->
  t_number();
true_branch_info(atom) ->
  t_atom();
true_branch_info({atom, A}) ->
  t_atom(A);
true_branch_info(list) ->
  t_maybe_improper_list();
true_branch_info(tuple) ->
  t_tuple();
true_branch_info({tuple, N}) ->
  t_tuple(N);
true_branch_info(pid) ->
  t_pid();
true_branch_info(port) ->
  t_port();
true_branch_info(binary) ->
  t_binary();
true_branch_info(reference) ->
  t_ref();
true_branch_info(function) ->
  t_fun();
true_branch_info(cons) ->
  t_cons();
true_branch_info(nil) ->
  t_nil();
true_branch_info(boolean) ->
  t_bool();
true_branch_info(constant) ->
  t_constant();
true_branch_info(T) ->
  erlang:error({?MODULE,unknown_typetest,T}).


%% _________________________________________________________________
%%
%% Remove the redundant type tests. If a test is removed the trace
%% that isn't taken is explicitly removed from the cfg to simpilify
%% the handling of phi nodes. If a phi node is left and at least one
%% branch into it has disappeared the ssa propagation pass can't
%% handle it.
%%
%% If the cfg has changed at the end of this pass, the analysis is
%% done again since we might be able to find more information because
%% of the simplification of the cfg.
%%

simplify_controlflow([Label|Left], State) ->
  case state__bb(State, Label) of
    not_found ->
      simplify_controlflow(Left, State);
    BB ->
      I = hipe_bb:last(BB),
      case I of
	#'if'{} ->
	  Info = state__info_out(State, Label),
	  case do_if(I, Info) of
	    [{Lab, _}] ->
	      NewState = mk_goto(State, BB, Label, Lab),
	      simplify_controlflow(Left, NewState);
	    [_,_] ->
	      simplify_controlflow(Left, State)
	  end;
	#type{} ->
	  Info = state__info_out(State, Label),
	  FalseLab = hipe_icode:type_false_label(I),
	  case hipe_icode:type_true_label(I) of
	    FalseLab ->
	      %% true label = false label, this can occur!
	      NewState = mk_goto(State, BB, Label, FalseLab),
	      simplify_controlflow(Left, NewState);
	    TrueLab ->
	      case do_type(I, Info) of
		[{TrueLab, _}] -> 
		  NewState = mk_goto(State, BB, Label, TrueLab),
		  simplify_controlflow(Left, NewState);
		[{FalseLab, _}] -> 
		  NewState = mk_goto(State, BB, Label, FalseLab),
		  simplify_controlflow(Left, NewState);
		[_,_] -> %% Maybe
		  simplify_controlflow(Left, State)
	      end
	  end;
	#switch_tuple_arity{} ->
	  Cases = hipe_icode:switch_tuple_arity_cases(I),
	  Info = state__info_out(State, Label),
	  Var = hipe_icode:switch_tuple_arity_arg(I),
	  Type = safe_lookup(Var, Info),	  
	  case legal_switch_tuple_arity_cases(Cases, Type) of
	    [] ->
	      Fail = hipe_icode:switch_tuple_arity_fail_label(I),
	      NewState = mk_goto(State, BB, Label, Fail),
	      simplify_controlflow(Left, NewState);
	    Cases -> 
	      %% Nothing changed.
	      case switch_tuple_arity_can_fail(Cases, Type) of
		true -> simplify_controlflow(Left, State);
		false ->
		  NewCases = butlast(Cases),
		  {_Arity, NewFail} = lists:last(Cases),
		  TmpI = 
		    hipe_icode:switch_tuple_arity_fail_label_update(I, NewFail),
		  NewI =
		    hipe_icode:switch_tuple_arity_cases_update(TmpI, NewCases),
		  NewBB = hipe_bb:code_update(BB, hipe_bb:butlast(BB)++[NewI]),
		  NewState = state__bb_add(State, Label, NewBB),
		  simplify_controlflow(Left, NewState)
	      end;
	    LegalCases ->
	      NewI =
		case switch_tuple_arity_can_fail(LegalCases, Type) of
		  true -> 
		    hipe_icode:switch_tuple_arity_cases_update(I, LegalCases);
		  false ->
		    NewCases = butlast(LegalCases),
		    {_Arity, NewFail} = lists:last(LegalCases),
		    TmpI = 
		      hipe_icode:switch_tuple_arity_cases_update(I, NewCases),
		    hipe_icode:switch_tuple_arity_fail_label_update(TmpI, 
								    NewFail)
		end,
	      NewBB = hipe_bb:code_update(BB, hipe_bb:butlast(BB) ++ [NewI]),
	      NewState = state__bb_add(State, Label, NewBB),
	      simplify_controlflow(Left, NewState)
	  end;
	#switch_val{} ->
	  Cases = hipe_icode:switch_val_cases(I),
	  Info = state__info_out(State, Label),
	  Arg = hipe_icode:switch_val_arg(I),
	  ArgType = safe_lookup(Arg, Info),
	  case legal_switch_val_cases(Cases, ArgType) of
	    [] ->
	      Fail = hipe_icode:switch_val_fail_label(I),
	      NewState = mk_goto(State, BB, Label, Fail),
	      simplify_controlflow(Left, NewState);
	    Cases ->
	      %% Nothing changed!
	      simplify_controlflow(Left, State);
	    %% TODO: Find out whether switch_val can fail 
	    %% just as switch_tuple_arity
	    LegalCases ->
	      NewI = hipe_icode:switch_val_cases_update(I, LegalCases),
	      NewBB = hipe_bb:code_update(BB, hipe_bb:butlast(BB) ++ [NewI]),
	      NewState = state__bb_add(State, Label, NewBB),
	      simplify_controlflow(Left, NewState)
	  end;
	#call{} ->
	  Info = state__info_out(State, Label),
	  case call_always_fails(I, Info) of
	    false ->
	      Fun = hipe_icode:call_fun(I),
	      case hipe_icode_primops:fails(Fun) of
		false ->
		  case hipe_icode:call_fail_label(I) of
		    [] ->
		      simplify_controlflow(Left, State);
		    _ ->
		      NewState = unset_fail(State, BB, Label, I),
		      simplify_controlflow(Left, NewState)
		  end;
		true ->
		  simplify_controlflow(Left, State)
	      end;
	    true ->
	      case hipe_icode:call_in_guard(I) of
		false ->
		  simplify_controlflow(Left, State);
		true ->
		  FailLabel = hipe_icode:call_fail_label(I),
		  NewState = mk_goto(State, BB, Label, FailLabel),
		  simplify_controlflow(Left, NewState)
	      end
	  end;
	_ ->
	  simplify_controlflow(Left, State)
      end
  end;
simplify_controlflow([], State) ->
  state__remove_unreachable(State).

mk_goto(State, BB, Label, Succ) ->
  NewI = hipe_icode:mk_goto(Succ),
  NewBB = hipe_bb:code_update(BB, hipe_bb:butlast(BB)++[NewI]),
  state__bb_add(State, Label, NewBB).

unset_fail(State, BB, Label, I) ->
  NewI = hipe_icode:call_set_fail_label(I, []),
  NewBB = hipe_bb:code_update(BB, hipe_bb:butlast(BB)++[NewI]),
  state__bb_add(State, Label, NewBB).

%% _________________________________________________________________
%%
%% Handling the map.
%%

%% ==========================================
%% Lookup treats anything that is neither in 
%% the map or a constant as t_none(). 
%% Use this during type propagation!

lookup(Var, Tree) ->
  case gb_trees:lookup(Var, Tree) of
    none ->
      case hipe_icode:is_const(Var) of
	true -> const_type(Var);
	false -> t_none()
      end;
    {value,Val} ->
      case hipe_icode:is_var(Val) of
	true ->
	  lookup(Val, Tree);
	false ->
	  Val
      end
  end.

lookup_list(List, Info) ->
  lookup_list0(List, Info, []).

lookup_list0([H|T], Info, Acc) ->
  lookup_list0(T, Info, [lookup(H, Info)|Acc]);
lookup_list0([], _, Acc) ->
  lists:reverse(Acc).

%% ==========================================
%% safe_lookup/2 treats anything that is 
%% neither in the map nor a constant as t_any(). 
%% Use this during transformations.

safe_lookup(Var, Tree) ->
  case gb_trees:lookup(Var, Tree) of
    none ->
      case hipe_icode:is_const(Var) of
	true -> const_type(Var);
	false -> t_any()
      end;
    {value,Val} ->
      case hipe_icode:is_var(Val) of
	true -> safe_lookup(Val, Tree);
	false -> Val
      end
  end.

safe_lookup_list(List, Info) ->
  safe_lookup_list0(List, Info, []).

safe_lookup_list0([H|T], Info, Acc) ->
  safe_lookup_list0(T, Info, [safe_lookup(H, Info)|Acc]);
safe_lookup_list0([], _, Acc) ->
  lists:reverse(Acc).

enter_lists([Var|VarLeft], [Type|TypeLeft], Info) ->
  NewInfo = enter(Var, Type, Info),
  enter_lists(VarLeft, TypeLeft, NewInfo);
enter_lists([], [], Info) ->
  Info.

enter([Key], Value, Tree) ->
  enter(Key, Value, Tree);
enter(Key, Value, Tree) ->
  case hipe_icode:is_var(Key) of
    true ->
      enter_to_leaf(Key, Value, Tree);
    false ->
      Tree
  end.

enter_to_leaf(Key, Value, Tree) ->
  case gb_trees:lookup(Key, Tree) of
    {value, Value} ->
      Tree;
    {value, Val} ->
      case hipe_icode:is_var(Val) of
	true->
	  enter_to_leaf(Val, Value, Tree);
	false ->
	  case t_is_none(Value) of
	    true ->
	      gb_trees:delete(Key, Tree);
	    false ->
	      gb_trees:enter(Key, Value, Tree)
	  end
      end;
    none ->
      case t_is_none(Value) of
	true ->
	  Tree;
	false ->
	  gb_trees:insert(Key, Value, Tree)
      end
  end.

empty() ->
  gb_trees:empty().

%% ==========================================
%% Joining maps from different edges in to 
%% a basic block.

join_list(List, Info) ->
  join_list(List, Info, t_none()).
 
join_list([H|T], Info, Acc) ->
  Type = t_sup(lookup(H, Info), Acc),
  join_list(T, Info, Type);
join_list([], _, Acc) ->
  Acc.

join_info_in(Vars, OldInfo, NewInfo) ->
  NewInfo2 = join_info_in(Vars, Vars, OldInfo, NewInfo, gb_trees:empty()),
  case info_is_equal(NewInfo2, OldInfo) of
    true -> fixpoint;
    false -> NewInfo2
  end.
      
%% NOTE: Variables can be bound to other variables. Joining these is
%% only possible if the binding is the same from both traces and this
%% variable is still live.

join_info_in([Var|Left], LiveIn, Info1, Info2, Acc) ->
  Type1 = gb_trees:lookup(Var, Info1),
  Type2 = gb_trees:lookup(Var, Info2),
  case {Type1, Type2} of
    {none, none} ->
      join_info_in(Left, LiveIn, Info1, Info2, Acc);
    {none, {value, Val}} ->
      NewVal =
	case hipe_icode:is_var(Val) of
	  true -> lookup(Val, Info2);
	  false -> Val
	end,
      NewTree = gb_trees:insert(Var, NewVal, Acc),
      join_info_in(Left, LiveIn, Info1, Info2, NewTree);
    {{value, Val}, none} ->
      NewVal =
	case hipe_icode:is_var(Val) of
	  true -> lookup(Val, Info2);
	  false -> Val
	end,
      NewTree = gb_trees:insert(Var, NewVal, Acc),
      join_info_in(Left, LiveIn, Info1, Info2, NewTree);
    {{value, Val}, {value, Val}} ->
      NewVal =
	case hipe_icode:is_var(Val) of
	  false -> Val;
	  true ->
	    case ordsets:is_element(Val, LiveIn) of
	      true -> Val;
	      false -> t_sup(lookup(Val, Info1), lookup(Val, Info2))
	    end
	end,
      NewTree = gb_trees:insert(Var, NewVal, Acc),
      join_info_in(Left, LiveIn, Info1, Info2, NewTree);
    {{value, Val1}, {value, Val2}} ->
      NewVal = 
	case {hipe_icode:is_var(Val1), hipe_icode:is_var(Val2)} of
	  {false, false} -> t_sup(Val1, Val2);
	  {true, true} -> t_sup(lookup(Val1, Info1), lookup(Val2, Info2));
	  {false, true} -> t_sup(Val1, lookup(Val2, Info2));
	  {true, false} -> t_sup(lookup(Val1, Info1), Val2)
	end,
      NewTree = gb_trees:insert(Var, NewVal, Acc),
      join_info_in(Left, LiveIn, Info1, Info2, NewTree)
  end;
join_info_in([], _LiveIn, _Info1, _Info2, Acc) ->
  gb_trees:balance(Acc).

%% ==========================================
%% Comparing two maps.

info_is_equal(Info1, Info2) ->
  compare(gb_trees:to_list(Info1), gb_trees:to_list(Info2)).

compare([{Var, Type1}|Left1], [{Var, Type2}|Left2]) ->
  case t_is_equal(Type1, Type2) of
    true -> compare(Left1, Left2);
    false -> false
  end;
compare([], []) ->
  true;
compare(_, _) ->
  false.

%% ==========================================
%% Update the maps in for the successors of 
%% a finished basic block.
%%

do_updates(State, List) ->
  do_updates(State, List, []).

do_updates(State, [{Label, Info}|Tail], Worklist) ->
  case state__info_in_update(State, Label, Info) of
    fixpoint ->
      do_updates(State, Tail, Worklist);
    NewState ->
      do_updates(NewState, Tail, [Label|Worklist])
  end;
do_updates(State, [], Worklist) ->
  {State, Worklist}.

enter_defines(I, Types, Info) when is_list(Types) ->
  case defines(I) of
    [] -> Info;
    Def ->
      {NewInfo, _} =
	lists:foldl(fun(X, {Inf,[Type|Tail]}) -> {enter(X,Type,Inf), Tail} end,
		    {Info, Types}, Def),
      NewInfo
  end;
enter_defines(I, Type, Info) ->
  case defines(I) of
    [] -> Info;
    Def ->
      lists:foldl(fun(X, Acc) -> enter(X, Type, Acc) end, Info, Def)
  end.

%% _________________________________________________________________
%%
%% Various help functions.
%%

add_arg_types(Args, Types) ->
  add_arg_types(Args,  Types, empty()).

add_arg_types([Arg|Args], [Type|Types], Acc) ->
  add_arg_types(Args,Types, enter(Arg, Type, Acc));
add_arg_types([_], [], Acc) ->
  %% In funs the list of arguments may contain the closure element.
  Acc;
add_arg_types([], [], Acc) ->
  Acc.

const_type(Const) ->
  t_from_term(hipe_icode:const_value(Const)).

defines(I) ->
  keep_vars(hipe_icode:defines(I)).

-ifdef(DEBUG).
uses(I) ->
  keep_vars(hipe_icode:uses(I)).
-endif.

args(I) ->
  hipe_icode:args(I).

keep_vars(Vars) ->
  lists:filter(fun(X) -> hipe_icode:is_var(X) end, Vars).

butlast([_]) ->
  [];
butlast([H|T]) ->
  [H|butlast(T)].

any_is_none([H|T]) ->
  case t_is_none(H) of
    true -> true;
    false -> any_is_none(T)
  end;
any_is_none([]) ->
  false.
      
%% _________________________________________________________________
%%
%% Handling the state
%%

new_state(Cfg) ->
  SuccMap = hipe_icode_cfg:succ_map(Cfg),
  PredMap = hipe_icode_cfg:pred_map(Cfg),
  Start = hipe_icode_cfg:start_label(Cfg),  
  Params = hipe_icode_cfg:params(Cfg),
  Arity = case hipe_icode_cfg:is_closure(Cfg) of
	    true -> hipe_icode_cfg:closure_arity(Cfg);
	    false -> length(Params)
	  end,
  ArgTypes = 
    case lists:keysearch(arg_type, 1, hipe_icode_cfg:info(Cfg)) of
      false ->
	lists:duplicate(Arity, t_any());
      {value, {_, AnTs}}->
	case any_is_none(AnTs) of
	  true ->
	    %% The call to this function is masked by an error.
	    %% We cannot trust the arguments.
	    lists:duplicate(Arity, t_any());
	  false ->
	    %% In case of closures
	    lists:sublist(AnTs, Arity)
	end
    end,  
  Info = add_arg_types(Params, ArgTypes),
  InfoMap = gb_trees:insert({Start, in}, Info, empty()),
  Liveness = hipe_icode_ssa:ssa_liveness__analyze(Cfg),
  #state{succmap=SuccMap, predmap=PredMap, info_map=InfoMap, 
	 cfg=Cfg, liveness=Liveness, arg_types=ArgTypes}.

%state__arg_types(#state{arg_types=AT}) ->
%  AT.

state__cfg(#state{cfg=Cfg}) ->
  Cfg.

state__remove_unreachable(State = #state{cfg=Cfg}) ->
  State#state{cfg=hipe_icode_cfg:remove_unreachable_code(Cfg)}.
  

state__succ(#state{succmap=SM}, Label) ->
  hipe_icode_cfg:succ(SM, Label).

state__bb(#state{cfg=Cfg}, Label) ->
  hipe_icode_cfg:bb(Cfg, Label).
  
state__bb_add(S=#state{cfg=Cfg}, Label, BB) ->
  NewCfg = hipe_icode_cfg:bb_add(Cfg, Label, BB),
  S#state{cfg=NewCfg}.

state__info_in(S, Label) ->
  state__info(S, {Label, in}).

state__info_out(S, Label) ->
  state__info(S, {Label, out}).

state__info(#state{info_map=IM}, Label) ->
  case gb_trees:lookup(Label, IM) of
    {value, Info}-> Info;
    none -> gb_trees:empty()
  end.

state__info_in_update(S=#state{info_map=IM, liveness=Liveness}, Label, Info) ->
  LiveIn = hipe_icode_ssa:ssa_liveness__livein(Liveness, Label),
  case gb_trees:lookup({Label, in}, IM) of
    none -> 
      OldInfo = gb_trees:empty(),
      case join_info_in(LiveIn, OldInfo, Info) of
	fixpoint -> 
	  %% If the bb has not been handled we ignore the fixpoint.
	  S#state{info_map=gb_trees:enter({Label, in}, OldInfo, IM)};
	NewInfo ->
	  S#state{info_map=gb_trees:enter({Label, in}, NewInfo, IM)}
      end;
    {value, OldInfo} ->
      case join_info_in(LiveIn, OldInfo, Info) of
	fixpoint -> 
	  fixpoint;
	NewInfo ->
	  S#state{info_map=gb_trees:enter({Label, in}, NewInfo, IM)}
      end
  end.

state__info_out_update(S=#state{info_map=IM}, Label, Info) ->
  S#state{info_map=gb_trees:enter({Label, out}, Info, IM)}.

%% _________________________________________________________________
%%
%% The worklist.
%%

init_work(State) ->
  Labels = [hipe_icode_cfg:start_label(state__cfg(State))],
  {Labels, [], gb_sets:from_list(Labels)}.

get_work({[Label|Left], List, Set}) ->
  NewWork = {Left, List, gb_sets:delete(Label, Set)},
  {Label, NewWork};
get_work({[], [], _Set}) ->
  fixpoint;
get_work({[], List, Set}) ->
  get_work({lists:reverse(List), [], Set}).

add_work(Work = {List1, List2, Set}, [Label|Left]) ->
  case gb_sets:is_member(Label, Set) of
    true ->
      add_work(Work, Left);
    false ->
      add_work({List1, [Label|List2], gb_sets:insert(Label, Set)}, Left)
  end;
add_work(Work, []) ->
  Work.


%% _________________________________________________________________
%%
%% Handle warnings on type errors.
%%

call_always_fails(I, Info) ->
  {Args, Type, Fun} = 
  case I of
    #call{} ->
      {hipe_icode:call_args(I),
       hipe_icode:call_type(I),
       hipe_icode:call_fun(I)};
    #enter{} ->
      {hipe_icode:enter_args(I),
       hipe_icode:enter_type(I),
       hipe_icode:enter_fun(I)}
  end,
  case Fun of
    %% These can actually be calls too.
    {erlang, halt, 0} -> false;
    {erlang, halt, 1} -> false;
    {erlang, exit, 1} -> false;
    {erlang, error, 1} -> false;
    {erlang, error, 2} -> false;
    {erlang, fault, 1} -> false;
    {erlang, fault, 2} -> false;
    {erlang, raise, 3} -> false;
    {erlang, throw, 1} -> false;    
    {erlang, hibernate, 3} -> false;
    _Other ->      
      {ReturnType, _} = call_or_enter_type(I, Fun, Args, Info, Type),
      t_is_none(ReturnType)
  end.

%% The call might be using the old fun-notation, that is a 2-tuple.
check_for_tuple_as_fun(Fun, [Arg|TailArgs]) ->
  case t_is_tuple(t_inf(t_tuple(2), Arg)) of
    false ->
      %% This cannot succeed!
      false;
    true ->
      not t_is_none(t_inf(find_plt_return(Fun, [t_fun()|TailArgs]),
			  primop_type(Fun, [t_fun()|TailArgs])))
  end.

warn_on_type_errors(PreAnalysisCfg, OrigState, FinalState, 
		    NoWarnUnused, IcodeFun) ->
  Warn1 =
    case sets:is_element(IcodeFun, NoWarnUnused) of
      true -> [];
      false -> [warn_on_args(IcodeFun, PreAnalysisCfg)]
    end,
  OrigCfg = state__cfg(OrigState),
  OrigLabels = hipe_icode_cfg:reverse_postorder(OrigCfg),
  Warn2 = warn_on_control_flow(OrigLabels, OrigState, IcodeFun, Warn1),
  FinalCfg = state__cfg(FinalState),
  FinalLabels = hipe_icode_cfg:reverse_postorder(FinalCfg),
  Warn3 = warn_on_bb(FinalLabels, FinalState, IcodeFun, Warn2),
  Warn4 = [warn_on_return_val(FinalLabels, FinalState, IcodeFun)|Warn3],
  lists:flatten(lists:reverse(Warn4)).

warn_on_return_val(FinalLabels, FinalState, IcodeFun) ->
  {RetType, IsExplicit, HasReturn} = 
    find_return_type(FinalLabels, FinalState),
  case t_is_none(RetType) of
    true ->
      case (not HasReturn) andalso (not IsExplicit) of
	true ->
	  [{?WARN_RETURN_NO_RETURN, 
	    {IcodeFun, "Function has no local return\n"}}];
	false ->
	  case IsExplicit andalso not HasReturn of
	    true ->
	      [{?WARN_RETURN_ONLY_EXIT, 
		{IcodeFun, 
		 "Function only terminates with explicit exception\n"}}];
	    false ->
	      []
	  end
      end;
    false ->
      []
  end.

check_for_exit_enters(Fun) ->
  case Fun of
    {erlang, halt, 0} -> true;
    {erlang, halt, 1} -> true;
    {erlang, exit, 1} -> true;
    {erlang, error, 1} -> true;
    {erlang, error, 2} -> true;
    {erlang, fault, 1} -> true;
    {erlang, fault, 2} -> true;
    {erlang, raise, 3} -> true;
    {erlang, throw, 1} -> true;
    {erlang, hibernate, 3} -> true;
    {M, F, A} when is_atom(M), is_atom(F), is_integer(A), A >= 0 -> false;
    {apply_N, N} when is_integer(N) -> false;
    {closure_element, N} when is_integer(N) -> false;
    {mkfun, {M, F, A}, MagicNum, Index} when is_atom(M), is_atom(F),
      is_integer(A), A >= 0, is_integer(MagicNum), is_integer(Index) -> false;
    Atom when is_atom(Atom) -> false  % catches all primops
  end.
      
warn_on_args(IcodeFun, Cfg) ->
  case lists:keysearch(arg_type, 1, hipe_icode_cfg:info(Cfg)) of
    false ->
      [];
    {value, {_, ArgType}} when length(ArgType) > 0 ->
      case any_is_none(ArgType) of
	true -> 
	  [{?WARN_NOT_CALLED, {IcodeFun, "Function will never be called!\n"}}];
	false ->
	  []
      end;
    _ ->
      []
  end.

warn_on_bb([Label|Left], State, IcodeFun, WarnAcc) ->
  BB = state__bb(State, Label),
  Code = hipe_bb:code(BB),
  InfoIn = state__info_in(State, Label),
  NewWarnAcc = warn_on_instr(Code, InfoIn, IcodeFun, WarnAcc),
  warn_on_bb(Left, State, IcodeFun, NewWarnAcc);
warn_on_bb([], _State, _IcodeFun, WarnAcc) ->
  WarnAcc.

warn_on_instr([I|Left], Info, IcodeFun, Acc) ->
  NewInfo = analyze_insn(I, Info),
  case any_is_none(lookup_list(args(I), Info)) of
    true -> 
      %% We can abort here since this must be in an impossible trace
      Acc;
    false ->
      {NewWarn, Left1} = 
	case I of
	  #call{} ->
	    case hipe_icode:call_fun(I) of
	      cons -> 
		{warn_on_cons(hipe_icode:call_args(I), Info, IcodeFun),Left};
	      {erlang, '++', 2} -> 
		{'warn_on_++'(I, Info, IcodeFun), Left};
	      call_fun -> 
		{warn_on_call_fun(hipe_icode:call_args(I), Info, IcodeFun),
		 Left};
	      _ -> 
		warn_on_call(I, Left, Info, IcodeFun)
	    end;
	  #enter{} ->
	    case hipe_icode:enter_fun(I) of
	      cons -> 
		W = warn_on_cons(hipe_icode:enter_args(I), Info, IcodeFun),
		{W, []};
	      {erlang, '++', 2} -> 
		W = 'warn_on_++'(I, Info, IcodeFun),
		{W, []};
	      enter_fun -> 
		W = warn_on_call_fun(hipe_icode:enter_args(I), Info, IcodeFun),
		{W, []};
	      _ -> 
		{warn_on_enter(I, Info, IcodeFun), []}
	    end;
	  _ ->
	    {none, Left}
	end,
      if NewWarn =:= none -> NewAcc = Acc;
	 true -> NewAcc = [NewWarn|Acc]
      end,
      warn_on_instr(Left1, NewInfo, IcodeFun, NewAcc)
  end;
warn_on_instr([], _Info, _IcodeFun, Acc) ->
  Acc.

warn_on_control_flow([Label|Left], State, IcodeFun, Acc) ->
  I = hipe_bb:last(state__bb(State, Label)),
  Info = state__info_out(State, Label),
  case any_is_none(lookup_list(args(I), Info)) of
    true -> 
      %% Try to avoid follow-up warnings.
      NewAcc = Acc;
    false ->
      W = 
	case I of
	  #'if'{} ->
	    warn_on_if(I, Info, IcodeFun);
	  #switch_tuple_arity{} ->      
	    warn_on_switch_tuple_arity(I, Info, IcodeFun);
	  #switch_val{} ->
	    warn_on_switch_val(I, Info, IcodeFun);
	  #type{} ->
	    warn_on_type(I, Info, IcodeFun);
	  #call{} ->
	    case hipe_icode:call_in_guard(I) of
	      true ->
		warn_on_call(I, Info, IcodeFun);
	      false ->
		none
	    end;
	  _ ->
	    none
	end,
      if W =:= none -> NewAcc = Acc;
	 W =:= [] -> NewAcc = Acc;
	 true -> NewAcc = [W|Acc]
      end
  end,
  warn_on_control_flow(Left, State, IcodeFun, NewAcc);
warn_on_control_flow([], _State, _IcodeFun, Acc) ->
  Acc.

warn_on_type(I, Info, IcodeFun) ->
  FalseLab = hipe_icode:type_false_label(I),
  case do_type(I, Info) of
    [{FalseLab, _}] ->
      Test = hipe_icode:type_type(I),
      [Arg] = hipe_icode:type_args(I),
      ArgType = format_type(safe_lookup(Arg, Info)),
      W = 
	case Test of
	  'nil' ->
	    io_lib:format("Pattern matching with [] will always fail "
			  "since variable has type ~s!\n",
			  [ArgType]);
	  {atom, Atom} ->
	    io_lib:format("Pattern matching with '~s' will always fail "
			  "since variable has type ~s!\n",
			  [Atom, ArgType]);
	  {tuple, N} ->
	    Tuple = construct_n_tuple(N),
	    io_lib:format("Pattern matching with ~s will always fail "
			  "since variable has type ~s!\n",
			  [Tuple, ArgType]);
	  {record, Atom, _Size} ->
	    io_lib:format("Pattern matching with #~w{} will always fail "
			  "since variable has type ~s!\n",
			  [Atom, ArgType]);
	  _ ->
	    io_lib:format("Type guard ~w will always fail "
			  "since variable has type ~s!\n",
			  [Test, ArgType])
	end,
      {?WARN_MATCHING, {IcodeFun, W}};
    _ ->
      none
  end.

construct_n_tuple(N) ->
  "{"++construct_underscores(N, [])++"}".

construct_underscores(0, Acc) ->
  Acc;
construct_underscores(1, Acc) ->
  "_"++Acc;
construct_underscores(N, Acc) ->
  construct_underscores(N-1, ",_"++Acc).

warn_on_cons(Args, Info, IcodeFun) ->
  [_Head, Tail] = safe_lookup_list(Args, Info),
  case t_is_list(t_inf(Tail, t_list())) of
    true ->
      none;
    false ->
      W = io_lib:format("Cons will produce"
			" a non-proper list since its 2nd arg is ~s!\n",
			[format_type(Tail)]),
      {?WARN_NON_PROPER_LIST, {IcodeFun, W}}
  end.

'warn_on_++'(I, Info, IcodeFun) ->
  case call_always_fails(I, Info) of
    true ->
      call_warning({erlang, '++', 2}, IcodeFun, 
			 safe_lookup_list(args(I), Info));
    false ->
      [_Head, Tail] = safe_lookup_list(args(I), Info),
      case t_is_list(t_inf(Tail, t_list())) of
	true ->
	  none;
	false ->
	  W = io_lib:format("Call to '++'/2 will produce"
			    " a non-proper list; 2nd arg is ~s!\n",
			    [format_type(Tail)]),
	  {?WARN_NON_PROPER_LIST, {IcodeFun, W}}
      end
  end.

warn_on_call_fun(Args0, Info, IcodeFun) ->
  Args = safe_lookup_list(Args0, Info),
  [Fun|TailArgs0] = lists:reverse(Args),
  TailArgs = lists:reverse(TailArgs0),
  ReturnType = primop_type(call_fun, Args),
  case t_is_none(ReturnType) of
    false ->
      none;
    true ->
      case t_is_fun(Fun) of
	true ->
	  ArgString = lists:flatten(pp_args(TailArgs)),
	  Msg = io_lib:format("Trying to use fun with type ~s "
			      "with arguments ~s\n",
			      [format_type(Fun), ArgString]),
	  {?WARN_FUN_APP, {IcodeFun, Msg}};
	false ->
 	  case t_is_tuple(Fun) of
	    true ->
	      Msg = io_lib:format("Tuple used as fun will fail in "
				  "native compiled code.\n", []),
	      {?WARN_TUPLE_AS_FUN, {IcodeFun, Msg}};
	    false ->
	      Msg = io_lib:format("Fun application using type ~s "
				  "instead of a fun!\n", [format_type(Fun)]),
	      {?WARN_FUN_APP, {IcodeFun, Msg}}
	  end
      end
  end.

format_type(T) ->
  case t_is_number(T) of
    true ->
      format_number(T);
    false ->
      case t_is_atom(T) of
	true -> 
	  case t_atom_vals(T) of
	    any -> "an atom";
	    _ -> t_to_string(T)
	  end;
	false -> t_to_string(T)
      end
  end.

format_number(T) ->
  case t_number_vals(T) of
    Numbers when is_list(Numbers) ->
      case lists:all(fun(N) -> is_float(N) end, Numbers) of
	true ->
	  case t_is_any(T) of
	    true -> io_lib:format("a float", []);
	    false -> io_lib:format("the float ~s", [t_to_string(T)])
	  end;
	false ->
	  case lists:all(fun(N) -> is_integer(N) end, Numbers) of
	    true ->
	      case t_is_any(T) of
		true -> io_lib:format("an integer", []);
		false -> io_lib:format("the integer ~s", [t_to_string(T)])
	      end;
	    false ->
	      case t_is_any(T) of
		true -> io_lib:format("a number", [])
	      end
	  end
      end;
    any ->
      t_to_string(T)
  end.

warn_on_call(I, Ins, Info, IcodeFun) ->
  case call_always_fails(I, Info) of
    true ->
      %% Don't need to propagate the call args when the call fails.
      {warn_on_call_1(I, Info, IcodeFun), Ins};
    false  ->
      CallFun = hipe_icode:call_fun(I),
      Warn = 
	case CallFun of
	  {mkfun, {_M,F,_A}, _, _} when length(Ins) =:= 1 -> {true, {'fun', F}};
	  {erlang, make_ref, 0} when length(Ins) =:= 1 -> {true, ref};
	  _ -> false
	end,
      case Warn of
	{true, Type} ->
	  [I2] = Ins,
	  case (is_record(I2, 'if') andalso 
		(hipe_icode:if_op(I2) =:= '=:=')) of
	    true ->
	      [Def] = defines(I),
	      case [X || X <- args(I2), Def =:= X] of
		[_] ->
		  W = 
		    case Type of
		      {'fun', Name} ->
			io_lib:format("Matching on newly created fun "
				      "~w will never succeed\n",
				      [Name]);
		      ref ->
			"Matching on newly created reference "
			  "will never succeed\n"
		    end,
		  {{?WARN_MATCHING, {IcodeFun, W}}, []};
		_ -> {none, Ins}
	      end;
	    _ -> {none, Ins}
	  end;
	false -> 
	  {none, Ins}
      end
  end.

warn_on_call(I, Info, IcodeFun) ->
  case call_always_fails(I, Info) of
    true ->
      warn_on_call_1(I, Info, IcodeFun);
    false ->
      none
  end.

warn_on_call_1(I, Info, IcodeFun) ->
  case hipe_icode:call_in_guard(I) of
    true ->
      warn_on_guard(I, IcodeFun, safe_lookup_list(args(I), Info));
    false ->
      Fun = hipe_icode:call_fun(I),
      Args = safe_lookup_list(hipe_icode:call_args(I), Info),
      case (hipe_icode:call_type(I) =/= primop) andalso 
	check_for_tuple_as_fun(Fun, Args) of
	true -> 
	  W = io_lib:format("Unsafe use of tuple as a fun"
			    " in call to ~w\n", [Fun]),
	  {?WARN_TUPLE_AS_FUN, {IcodeFun, W}};
	false ->
	  call_warning(Fun, IcodeFun, safe_lookup_list(args(I), Info))
      end
  end.

warn_on_enter(I, Info, IcodeFun) ->
  Fun = hipe_icode:enter_fun(I),
  Args = safe_lookup_list(hipe_icode:enter_args(I), Info),
  case call_always_fails(I, Info) of
    true ->
      case (hipe_icode:enter_type(I) =/= primop) andalso 
	check_for_tuple_as_fun(Fun, Args) of
	true -> 
	  W = io_lib:format("Unsafe use of tuple as a fun in call "
			    "to ~w\n", [Fun]),
	  {?WARN_TUPLE_AS_FUN, {IcodeFun, W}};
	false ->
	  call_warning(Fun, IcodeFun, safe_lookup_list(args(I), Info))
      end;
    false ->
      none
  end.

warn_on_switch_tuple_arity(I, Info, IcodeFun) ->
  Arg = hipe_icode:switch_tuple_arity_arg(I),
  ArgType = safe_lookup(Arg, Info),
  Cases = hipe_icode:switch_tuple_arity_cases(I),
  case legal_switch_tuple_arity_cases(Cases, ArgType) of
    Cases ->
      none;
    LegalCases ->
      ArgType1 = format_type(ArgType),
      IllegalCases = [X || X <- Cases, not lists:member(X, LegalCases)],
      Arities = [hipe_icode:const_value(X) || {X, _} <- IllegalCases],
      Ws = [{IcodeFun, io_lib:format("The clause matching on tuple with"
				     " arity ~w will never match;"
				     " argument is of type ~s!\n", 
				     [X, ArgType1])}|| X <- Arities],
      [{?WARN_MATCHING, X} || X <- Ws]
  end.

warn_on_switch_val(I, Info, IcodeFun) ->
  Arg = hipe_icode:switch_val_arg(I),
  ArgType = safe_lookup(Arg, Info),
  Cases = hipe_icode:switch_val_cases(I),
  case legal_switch_val_cases(Cases, ArgType) of
    Cases ->
      none;
    LegalCases ->
      IllegalCases = [X || X <- Cases, not lists:member(X, LegalCases)],
      Vals = [t_from_term(hipe_icode:const_value(X)) || {X, _} <- IllegalCases],
      ArgTypeString = format_type(ArgType),
      Ws = [{IcodeFun, io_lib:format("The clause matching on ~s will"
				     " never match; argument is of type ~s\n", 
				     [format_type(X), ArgTypeString])}
	    || X <- Vals],
      [{?WARN_MATCHING, X} || X <- Ws]
  end.

warn_on_if(I, Info, IcodeFun) ->
  TrueLab = hipe_icode:if_true_label(I),
  FalseLab = hipe_icode:if_false_label(I),
  Op = hipe_icode:if_op(I),
  case do_if(I, Info) of
    [_, _] ->
      none;
    [{FalseLab, _}] ->
      W = 
	case Op of
	  '=:=' ->
	    [Arg1, Arg2] = safe_lookup_list(hipe_icode:if_args(I), Info),
	    io_lib:format("=:= between ~s and ~s will always fail!\n",
			  [format_type(Arg1), format_type(Arg2)]);
	  '=/=' ->
	    [Arg1, Arg2] = safe_lookup_list(hipe_icode:if_args(I), Info),
	    io_lib:format("=/= between ~s and ~s will always fail!\n",
			  [format_type(Arg1), format_type(Arg2)]);
	  '==' ->
	    [Arg1, Arg2] = safe_lookup_list(hipe_icode:if_args(I), Info),
	    io_lib:format("== between ~s and ~s will always fail!\n",
			  [format_type(Arg1), format_type(Arg2)]);
	  '/=' ->
	    [Arg1, Arg2] = safe_lookup_list(hipe_icode:if_args(I), Info),
	    io_lib:format("/= between ~s and ~s will always fail!\n",
			  [format_type(Arg1), format_type(Arg2)]);
	  '<' ->
	    [Arg1, Arg2] = safe_lookup_list(hipe_icode:if_args(I), Info),
	    io_lib:format("< between ~s and ~s will always fail!\n",
			  [format_type(Arg1), format_type(Arg2)]);
	  '=<' ->
	    [Arg1, Arg2] = safe_lookup_list(hipe_icode:if_args(I), Info),
	    io_lib:format("=< between ~s and ~s will always fail!\n",
			  [format_type(Arg1), format_type(Arg2)]);
	  '>=' ->
	    [Arg1, Arg2] = safe_lookup_list(hipe_icode:if_args(I), Info),
	    io_lib:format(">= between ~s and ~s will always fail!\n",
			  [format_type(Arg1), format_type(Arg2)]);
	  '>' ->
	    [Arg1, Arg2] = safe_lookup_list(hipe_icode:if_args(I), Info),
	    io_lib:format("> between ~s and ~s will always fail!\n",
			  [format_type(Arg1), format_type(Arg2)])
	end,
      {?WARN_COMP, {IcodeFun, W}};
    [{TrueLab, _}] ->
      none
  end.

warn_on_guard(I, IcodeFun, Args) ->
  Fun = hipe_icode:call_fun(I),  
  W = io_lib:format("The guard ~w will always fail since "
		    "the arguments are of type ~s!\n", 
		    [Fun, pp_args(Args)]),
  {?WARN_GUARDS, {IcodeFun, W}}.

call_warning(Fun, IcodeFun, Args) ->
  case hipe_icode_primops:fails(Fun) of
    true ->
      Signature = find_signature(Fun, length(Args)),
      ArgTypes = t_fun_args(Signature),
      RetType = t_fun_range(Signature),
      case any_is_none([RetType|ArgTypes]) of
	true -> none;
	false ->
	  Bif = 
	    case t_is_subtype(t_product(Args), t_product(ArgTypes)) of
	      true ->
		%% This must be because of some hard-coded information.
		{M, F, A} = Fun,
		case erl_bif_types:is_known(M, F, A) of
		  true -> "built-in ";
		  false ->
		    erlang:error({assert_failed, {not_known, Fun}})
		end;
	      false ->
		""
	    end,
	  W = io_lib:format("Call to ~sfunction ~w with signature ~s will "
			    "fail since the arguments are of type ~s!\n",
			    [Bif, Fun, format_sig(Signature),
			     pp_args(Args)]),
	  {?WARN_FAILING_CALL, {IcodeFun, W}}
      end;
    false ->
      W = "Unsafe BEAM code! Please recompile with a newer BEAM compiler.\n",
      {?WARN_OLD_BEAM, {IcodeFun, W}}
  end.

format_sig(Type) ->
  "fun(" ++ Sig = lists:flatten(t_to_string(Type)),
  ")" ++ RevSig = lists:reverse(Sig),
  lists:reverse(RevSig).


%% _________________________________________________________________
%%
%% Find return type. Returns the return type for the compiled
%% function.
%%

%% Labels must be specified so that we do not use unreachable returns.
find_return_type(Labels, State) ->
  find_return_type(Labels, State, t_none(), false, false).

find_return_type([Label|Left], State, ReturnType, ExplExit, HasReturn) ->
  case state__succ(State, Label) of
    [] ->
      BB = state__bb(State, Label),
      I = hipe_bb:last(BB),
      case I of
	#return{} ->
	  Info = state__info_in(State, Label),
	  Code = hipe_bb:code(BB),
	  NewReturnType = find_return_type_in_block(Code, Info),
	  find_return_type(Left, State, t_sup(NewReturnType, ReturnType),
			   ExplExit, true);
	#enter{} ->
	  Info = state__info_in(State, Label),
	  Code = hipe_bb:code(BB),
	  NewReturnType = find_return_type_in_block(Code, Info),
	  ExplExit1 = check_for_exit_enters(hipe_icode:enter_fun(I)),
	  NewExplExit = ExplExit orelse ExplExit1,
	  NewHasReturn = HasReturn orelse not ExplExit1,
	  find_return_type(Left, State, t_sup(NewReturnType, ReturnType),
			   NewExplExit, NewHasReturn);
	#fail{} ->
	  NewExplExit =
	    case hipe_icode:fail_class(I) of
	      throw -> true;
	      exit -> true;
	      rethrow -> ExplExit;
	      error -> ExplExit
	    end,
	  find_return_type(Left, State, ReturnType, NewExplExit, HasReturn);
	_ ->
	  find_return_type(Left, State, ReturnType, ExplExit, HasReturn)
      end;
    _ ->
      find_return_type(Left, State, ReturnType, ExplExit, HasReturn)
  end;
find_return_type([], _State, ReturnType, ExplExit, HasReturn) ->
  {t_limit(ReturnType, ?TYPE_DEPTH), ExplExit, HasReturn}.


find_return_type_in_block([I], Info) ->
  case I of
    #enter{} ->
      Fun = hipe_icode:enter_fun(I),
      Args = hipe_icode:enter_args(I),
      Type = hipe_icode:enter_type(I),
      {DstType, _} = call_or_enter_type(I, Fun, Args, Info, Type),
      DstType;
    #return{} ->
      [Arg] = hipe_icode:return_vars(I),
      lookup(Arg, Info)
  end;
find_return_type_in_block([I|Left], Info) ->
  find_return_type_in_block(Left, analyze_insn(I, Info)).

%% _________________________________________________________________
%%
%% Plt and bif info.
%%

init_mfa_info(MFA, Plt) ->
  put(dialyzer_icode_plt, Plt),
  plt_lookup(MFA).

find_signature(MFA = {_, _, _}, _) -> find_signature_mfa(MFA);
find_signature(Primop, Arity) -> find_signature_primop(Primop, Arity).

find_signature_mfa(MFA = {M, F, A}) ->
  PltSig = plt_lookup(MFA),
  BifRet = erl_bif_types:type(M, F, A),
  case erl_bif_types:arg_types(M, F, A) of
    any ->
      t_inf(PltSig, t_fun(BifRet));
    BifArgs ->
      t_inf(PltSig, t_fun(BifArgs, BifRet))
  end.

find_signature_primop(Primop, Arity) when is_integer(Arity), 0 =< Arity ->
  ReturnType = hipe_icode_primops:type(Primop),
  case erl_bif_types:arg_types(Primop) of
    any ->
      t_fun(Arity, ReturnType);
    ArgTypes ->
      t_fun(ArgTypes, ReturnType)
  end.

find_plt_return(MFA, ArgTypes) ->
  Sig = find_signature_mfa(MFA),
  SigArgTypes = t_fun_args(Sig),
  case t_is_any(SigArgTypes) of
    true ->
      t_fun_range(Sig);
    false ->
      case any_is_none(t_inf_lists(ArgTypes, SigArgTypes)) of
	true -> t_none();
	false -> t_fun_range(Sig)
      end
  end.

plt_lookup(MFA) ->
  case dialyzer_plt:lookup(get(dialyzer_icode_plt), MFA) of
    none ->
      t_fun();
    {value, {RetType, ArgTypes}} ->
      t_fun(ArgTypes, RetType)
  end.

primop_type(Op, Args) ->
  case Op of
    {mkfun, MFA = {_M, _F, _A}, _MagicNum, _Index} ->
      t_inf(t_fun(), find_signature_mfa(MFA));
    _ ->
      case get_unsafe_arithop(Op) of
	false ->
	  hipe_icode_primops:type(Op, Args);
	{true, Fun} ->
	  case all_fixnum_values(Args) of
	    false ->
	      hipe_icode_primops:type(Op, Args);
	    true ->
	      evaluate_unsafe_arith(Fun, Args)
	  end
      end
  end.

get_unsafe_arithop(Fun) ->
  case Fun of
    '+' ->              {true, fun(A, B) -> A + B end};
    '-' ->              {true, fun(A, B) -> A - B end};
    'band' ->           {true, fun(A, B) -> A band B end};
    'bor'  ->           {true, fun(A, B) -> A bor B end};
    'bxor' ->           {true, fun(A, B) -> A bxor B end};
    'bnot' ->           {true, fun(A) -> bnot A end};
    'bsr' ->            {true, fun(A, B) -> A bsr B end};
    'bsl' ->            {true, fun(A, B) -> A bsl B end};
    extra_unsafe_add -> {true, fun(A, B) -> A + B end};
    unsafe_add ->       {true, fun(A, B) -> A + B end};
    unsafe_sub ->       {true, fun(A, B) -> A - B end};
    unsafe_band ->      {true, fun(A, B) -> A band B end};
    unsafe_bor ->       {true, fun(A, B) -> A bor B end};
    unsafe_bxor ->      {true, fun(A, B) -> A bxor B end};
    unsafe_bnot ->      {true, fun(A) -> bnot A end};
    _ -> false
  end.

all_fixnum_values([Type|Left]) ->
  case t_is_number(Type) of
    false -> false;
    true ->
      case t_number_vals(Type) of
	any -> false;
	Vals ->
	  case lists:all(fun(X) -> hipe_tagscheme:is_fixnum(X) end, Vals) of
	    true ->
	      all_fixnum_values(Left);
	    false ->
	      false
	  end
      end
  end;
all_fixnum_values([]) ->
  true.

evaluate_unsafe_arith(Fun, [Arg]) ->
  t_sup([t_from_term(Fun(X)) || X <- t_number_vals(Arg)]);
evaluate_unsafe_arith(Fun, [Arg1, Arg2]) ->
  t_sup([t_from_term(Fun(X, Y)) || X <- t_number_vals(Arg1), 
				   Y <- t_number_vals(Arg2)]).

%% _________________________________________________________________
%%
%% Pretty printer
%%

-ifdef(DEBUG).
pp(State, IcodeFun, Msg) ->
  io:format("-------------- " ++ Msg ++ " ---------------\n", []),
  AnnotatedCfg = state__cfg(annotate_cfg(State)),
  Labels = hipe_icode_cfg:labels(AnnotatedCfg),
  {ReturnType, _, _} = find_return_type(Labels, State),
  hipe_icode_cfg:pp(AnnotatedCfg),
  case t_is_none(ReturnType) of
    true ->
      io:format("Function ~w will never return a proper value!\n", 
		[IcodeFun]);
    false ->
      io:format("Return type for ~w: ~s\n", [IcodeFun, 
					     format_type(ReturnType)])
  end.

annotate_cfg(State) ->
  Cfg = state__cfg(State),
  Labels = hipe_icode_cfg:labels(Cfg),
  annotate_bbs(Labels, State).
  
annotate_bbs([Label|Left], State) ->
  BB = state__bb(State, Label),
  Code = hipe_bb:code(BB),
  Info = state__info_in(State, Label),
  NewCode = annotate_instr_list(Code, Info, []),
  NewBB = hipe_bb:code_update(BB, NewCode),
  NewState = state__bb_add(State, Label, NewBB),
  annotate_bbs(Left, NewState);
annotate_bbs([], State) ->
  State.

annotate_instr_list([I], Info, Acc) ->
  case I of
    #call{} ->
      {DstType, _} = do_call(I, Info),
      NewInfo = enter_defines(I, DstType, Info),
      NewI = annotate_instr(I, NewInfo, Info),
      lists:reverse([NewI|Acc]);
    _ ->
      NewInfo = analyze_insn(I, Info),
      NewI = annotate_instr(I, NewInfo, Info),
      lists:reverse([NewI|Acc])
  end;
annotate_instr_list([I|Left], Info, Acc) ->
  NewInfo = analyze_insn(I, Info),
  NewI = annotate_instr(I, NewInfo, Info),
  annotate_instr_list(Left, NewInfo, [NewI|Acc]).

annotate_instr(I, DefInfo, UseInfo) ->
  Def = defines(I),
  Use = uses(I),
  Fun = fun(X, Y) -> hipe_icode:annotate_var(X, Y) end,
  DefSubst = [{X, Fun(X, lookup(X, DefInfo))} || X <- Def],
  UseSubst = [{X, Fun(X, lookup(X, UseInfo))} || X <- Use],
  case DefSubst ++ UseSubst of
    [] ->
      I;
    Subst ->
      hipe_icode:subst(Subst, I)
  end.
-else.
pp(_, _, _) ->
  ok.
-endif.

pp_args(Args) ->
  "(" ++ pp_args_1(Args) ++ ")".

pp_args_1([Arg1, Arg2|Tail]) ->
  case t_is_any(Arg1) of
    true -> "_";
    false -> t_to_string(Arg1)
  end ++ "," ++ pp_args_1([Arg2|Tail]);
pp_args_1([Arg]) ->
  case t_is_any(Arg) of
    true -> "_";
    false -> t_to_string(Arg)
  end;
pp_args_1([]) ->
  [].

