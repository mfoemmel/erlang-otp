%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : hipe_icode_type.erl
%%% Author  : Tobias Lindahl <Tobias.Lindahl@it.uu.se>
%%% Description : Propagate type information.
%%%
%%% Created : 25 Feb 2003 by Tobias Lindahl <Tobias.Lindahl@it.uu.se>
%%%
%%% CVS :
%%%     $Id: hipe_icode_type.erl,v 1.134 2005/05/03 13:52:18 tobiasl Exp $
%%%-------------------------------------------------------------------

-module(hipe_icode_type).

-export([cfg/3, const_type/1]).

-include("hipe_icode_type.hrl").

-import(erl_types, [t_any/0, t_atom/1, t_atom/0, t_atom_vals/1,
		    t_binary/0, t_bool/0, t_cons/0, t_improper_list/0,
		    t_float/0, t_from_term/1, t_fun/0, t_fun/1, t_fun/2,
		    t_fun_args/1, t_fun_range/1,t_inf/2, t_inf_lists/2, 
		    t_integer/0,
		    t_integer/1, t_is_atom/1, t_is_any/1, t_is_binary/1,
		    t_is_bool/1, t_is_char/1, t_is_cons/1,
		    t_is_improper_list/1, t_is_equal/2, t_is_float/1,
		    t_is_fun/1, t_is_integer/1, t_is_number/1,
		    t_is_list/1, t_is_nil/1, t_is_port/1, t_is_pid/1,
		    t_is_ref/1, t_is_subtype/2, 
		    t_is_tuple/1,
		    t_is_none/1, t_limit/2, t_list/0, t_nil/0,
		    t_number/0, t_number/1, t_number_vals/1, t_pid/0,
		    t_port/0, t_ref/0, t_subtract/2, t_sup/2,
		    t_to_string/1, t_tuple/0, t_tuple/1,
		    t_tuple_arity/1, t_tuple_arities/1, t_none/0]).


cfg(Cfg, IcodeFun, Options) ->
  OldSig = init_mfa_info(IcodeFun, Options),
  State = analyse(Cfg),
  pp(State, IcodeFun, Options, "Pre-specialization"),
  NewState = cfg_loop(State, IcodeFun),
  pp(NewState, IcodeFun, Options, "Post-specialization"),

  warn_on_type_errors(Cfg, State, NewState, IcodeFun, Options),

  Fixpoint = update_mfa_info(NewState, IcodeFun, OldSig, Options),

  case proplists:get_bool(inline_fp, Options) of
    true ->
      {Fixpoint, hipe_icode_fp:cfg(state__cfg(NewState), 
				   state__info_map(NewState))};
    false ->
      {Fixpoint, state__cfg(NewState)}
  end.


cfg_loop(State, IcodeFun) ->
  NewState0 = specialize(State),
  Labels = hipe_icode_cfg:reverse_postorder(state__cfg(NewState0)),
  case simplify_controlflow(Labels, NewState0, false) of
    {dirty, Cfg} ->
      NewCfg = hipe_icode_cfg:remove_unreachable_code(Cfg),
      NewState1 = analyse(NewCfg),
      cfg_loop(NewState1, IcodeFun);
    NewState1 ->
      NewState1
  end.


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

analyse(Cfg)->
  %%hipe_icode_cfg:pp(Cfg),
  State = new_state(Cfg),
  analyse_blocks(State).

analyse_blocks(State)->
  Work = init_work(State),
  analyse_blocks(Work, State).

analyse_blocks(Work, State)->
  case get_work(Work) of
    fixpoint ->
      State;
    {Label, NewWork} ->
      Info = state__info_in(State, Label),
      {NewState2, NewLabels} = analyse_block(Label, Info, State),
      NewWork2 = add_work(NewWork, NewLabels),
      analyse_blocks(NewWork2, NewState2)
  end.
  
analyse_block(Label, InfoIn, State)->
  %%io:format("Handling ~w\n", [Label]),
  BB = state__bb(State, Label),
  Code = hipe_bb:butlast(BB),
  Last = hipe_bb:last(BB),
  InfoOut = analyse_insns(Code, InfoIn),
  NewState = state__info_out_update(State, Label, InfoOut),

  case hipe_icode:type(Last) of
    'if' ->
      UpdateInfo = do_if(Last, InfoOut),
      %%io:format("Update info for ~w:\n~w", [Label, UpdateInfo]),
      do_updates(NewState, UpdateInfo);
    type ->
      UpdateInfo = do_type(Last, InfoOut),
      %%io:format("Update info for ~w:\n", [Label]),
      %%[io:format("~w: ~p\n", [X, gb_trees:to_list(Y)])||{X, Y} <- UpdateInfo],
      do_updates(NewState, UpdateInfo);
    switch_tuple_arity ->
      UpdateInfo = do_switch_tuple_arity(Last, InfoOut),
      %%io:format("Update info for ~w:\n", [Label]),
      %%[io:format("~w: ~p\n", [X, gb_trees:to_list(Y)])||{X, Y} <- UpdateInfo],
      do_updates(NewState, UpdateInfo);
    switch_val ->
      UpdateInfo = do_switch_val(Last, InfoOut),
      %%io:format("Update info for ~w:\n~w", [Label, UpdateInfo]),
      do_updates(NewState, UpdateInfo);
    call ->
      NewInfoOut = do_call(Last, InfoOut),
      NewState1 = state__info_out_update(NewState, Label, NewInfoOut),
      ContInfo = update_call_arguments(Last, NewInfoOut),      
      Cont = hipe_icode:call_continuation(Last),
      Fail = hipe_icode:call_fail_label(Last),
      UpdateInfo =
	case Fail of
	  [] ->
	    [{Cont, ContInfo}];
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
      %%io:format("Update info for ~w:\n~w\n", [Label, UpdateInfo]),
      do_updates(NewState1, UpdateInfo);
    _ ->
      UpdateInfo = [{X, InfoOut}||X<-state__succ(NewState, Label)],
      %%io:format("Update info for ~w:\n~w", [Label, UpdateInfo]),
      do_updates(NewState, UpdateInfo)
  end.

analyse_insns([I|Insns], Info)->
  NewInfo = analyse_insn(I, Info),
  analyse_insns(Insns, NewInfo);
analyse_insns([], Info) ->
  Info.

analyse_insn(I, Info) ->
  case hipe_icode:type(I) of
    move ->
      do_move(I, Info);
    call ->
      update_call_arguments(I, do_call(I, Info));
    phi ->
      Type = t_limit(join_list(args(I), Info), ?TYPE_DEPTH),
      enter_defines(I, Type, Info);
    begin_handler ->
      enter_defines(I, t_any(), Info);
    _ ->
      %% Just an assert
      case defines(I) of
	[] -> Info;
	_ -> exit({"Instruction with destination not analysed", I})
      end
  end.

do_move(I, Info)->
  %% Can't use uses/1 since we must keep constants.
  [Src] = args(I),
  case hipe_icode:is_const(Src) of
    true -> 
      enter_defines(I, const_type(Src), Info);
    false ->
      %% Make the destination point to the source.
      enter_defines(I, Src, Info)
  end.

do_call(I, Info)->
  case hipe_icode:call_type(I) of
    primop ->
      Fun = hipe_icode:call_fun(I),
      ArgTypes = lookup_list(args(I), Info),
      DstType = primop_type(Fun, ArgTypes),      
      %%io:format("Entering ~s for ~w as a return from\n", 
      %%	[format_type(DstType), defines(I)]),
      enter_defines(I, DstType, Info);
    remote ->
      MFA = {M, F, A} = hipe_icode:call_fun(I),
      ArgTypes = lookup_list(args(I), Info),
      PltType = find_plt_return(MFA, ArgTypes),
      BifType = erl_bif_types:type(M, F, A, ArgTypes),
      Type = t_inf(BifType, PltType),
      %%      io:format("The result of the call to ~p is ~s\n", 
      %%      		[hipe_icode:call_fun(I), format_type(Type)]),
      %%      io:format("Argtypes: ~p\n", [[format_type(X)||X<-ArgTypes]]),
      enter_defines(I, Type, Info);
    local ->
      AnnotatedType =
	case hipe_icode:call_dst_type(I) of
	  [] -> t_any();
	  T -> 
	    %% Make sure that the core type pass has not 
	    %% annotated this with none()
	    case t_is_none(T) of
	      true -> t_any();
	      false -> T
	    end
	end,
      MFA = hipe_icode:call_fun(I),
      ArgTypes = lookup_list(args(I), Info),
      PltType = find_plt_return(MFA, ArgTypes),
      Type = t_inf(AnnotatedType, PltType),
      %%      io:format("The result of the call to ~w is ~s\n", 
      %%		[hipe_icode:call_fun(I), format_type(Type)]),
      %%      io:format("Annotated type: ~s\n", 
      %%		[format_type(AnnotatedType)]),
      %%      io:format("Plt type: ~s\n", 
      %%		[format_type(PltType)]),
      enter_defines(I, Type, Info)
  end.

do_if(I, Info) ->
  %%% XXX: Could probably do better than this.
  TrueLab = hipe_icode:if_true_label(I),
  FalseLab = hipe_icode:if_false_label(I),
  case hipe_icode:if_args(I) of
    [Arg1, Arg2] = Args ->
      [Type1, Type2] = lookup_list(Args, Info),
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
	    _ ->
	      [{TrueLab, Info}, {FalseLab, Info}]
	  end
      end;
    _ ->
      %% Only care for binary if:s
      [{TrueLab, Info}, {FalseLab, Info}]
  end.

do_type(I, Info)->
  [Var] = args(I),
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
	  test_number_or_atom(fun(X)->t_atom(X)end, 
			      fun(X)->t_atom_vals(X)end,
			      A, Var, VarInfo, {atom, A}, 
			      TrueLab, FalseLab, Info);
	{integer, N} ->
	  test_number_or_atom(fun(X)->t_number(X)end, 
			      fun(X)->t_number_vals(X)end,
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

do_switch_tuple_arity(I, Info)->
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

legal_switch_tuple_arity_cases(Cases, Type)->
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

legal_switch_tuple_arity_cases_1(Cases, Type)->
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


do_switch_val(I, Info)->
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

test_number_or_atom(Fun, FunVals, X, Var, VarInfo, TypeTest,
		    TrueLab, FalseLab, Info)->
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
	  case FunVals(VarInfo) == any of
	    true ->
	      [{TrueLab, enter(Var, Fun(X), Info)},
	       {FalseLab, Info}];
	    false ->
	      FalseType = t_subtract(VarInfo, Fun(X)),
	      [{TrueLab, enter(Var, Fun(X), Info)},
	       {FalseLab, enter(Var, FalseType, Info)}]
	  end
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


test_type(Test, Type)->
  %%io:format("Test is: ~w\n", [Test]),
  %%io:format("Type is: ~s\n", [format_type(Type)]),
  Ans = 
    case t_is_any(Type) of
      true -> maybe;
      false ->
	TrueTest = true_branch_info(Test),
	Inf = t_inf(TrueTest, Type),
	%%io:format("TrueTest is: ~s\n", [format_type(TrueTest)]),
	%%io:format("Inf is: ~s\n", [format_type(Inf)]),
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
    end,
  %%io:format("Result is: ~s\n\n", [Ans]),
  Ans.

test_type0(integer, T)->
  t_is_integer(T);
test_type0({integer, N}, T)->
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
	[X] when integer(X) -> false;
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
  t_is_improper_list(T);
test_type0(cons, T) ->
  t_is_cons(T);
test_type0(nil, T)->
  t_is_nil(T);
%% CONS, NIL, and TUPLE are not constants, everything else is
test_type0(constant, T) ->
  case t_is_improper_list(T) of
    true -> false;
    false -> 
      case t_is_tuple(T) of
	true -> false;
	false -> true
      end
  end;
test_type0(T, _) ->
  exit({unknown_typetest, T}).


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
  t_improper_list();
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
  %% Since we do not have negative types like "not tuple"
  t_any();
true_branch_info(T) ->
  exit({?MODULE,unknown_typetest,T}).


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


simplify_controlflow([Label|Left], State, Dirty)->
  case state__bb(State, Label) of
    not_found ->
      simplify_controlflow(Left, State, Dirty);
    BB ->
      I = hipe_bb:last(BB),
      case hipe_icode:type(I) of
	'if' ->
	  Info = state__info_out(State, Label),
	  case do_if(I, Info) of
	    [{Lab, _}] ->
	      NewState = mk_goto(State, BB, Label, Lab),
	      simplify_controlflow(Left, NewState, true);
	    [_,_] ->
	      simplify_controlflow(Left, State, Dirty)
	  end;
	type ->
	  Info = state__info_out(State, Label),
	  FalseLab = hipe_icode:type_false_label(I),
	  case hipe_icode:type_true_label(I) of
	    FalseLab ->
	      %% true label = false label, this can occur!
	      NewState = mk_goto(State, BB, Label, FalseLab),
	      simplify_controlflow(Left, NewState, true);
	    TrueLab ->
	      case do_type(I, Info) of
		[{TrueLab, _}] -> 
		  NewState = mk_goto(State, BB, Label, TrueLab),
		  simplify_controlflow(Left, NewState, true);
		[{FalseLab, _}] -> 
		  NewState = mk_goto(State, BB, Label, FalseLab),
		  simplify_controlflow(Left, NewState, true);
		[_,_] -> %% Maybe
		  simplify_controlflow(Left, State, Dirty)
	      end
	  end;
	switch_tuple_arity ->
	  Cases = hipe_icode:switch_tuple_arity_cases(I),
	  Info = state__info_out(State, Label),
	  Var = hipe_icode:switch_tuple_arity_arg(I),
	  Type = safe_lookup(Var, Info),	  
	  case legal_switch_tuple_arity_cases(Cases, Type) of
	    [] ->
	      Fail = hipe_icode:switch_tuple_arity_fail_label(I),
	      NewState = mk_goto(State, BB, Label, Fail),
	      simplify_controlflow(Left, NewState, true);
	    Cases -> 
	      %% Nothing changed.
	      case switch_tuple_arity_can_fail(Cases, Type) of
		true -> simplify_controlflow(Left, State, Dirty);
		false ->
		  NewCases = butlast(Cases),
		  {_Arity, NewFail} = lists:last(Cases),
		  TmpI = 
		    hipe_icode:switch_tuple_arity_fail_label_update(I, NewFail),
		  NewI =
		    hipe_icode:switch_tuple_arity_cases_update(TmpI, NewCases),
		  NewBB = hipe_bb:code_update(BB, hipe_bb:butlast(BB)++[NewI]),
		  NewState = state__bb_add(State, Label, NewBB),
		  simplify_controlflow(Left, NewState, true)
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
	      simplify_controlflow(Left, NewState, true)
	  end;
	switch_val ->
	  Cases = hipe_icode:switch_val_cases(I),
	  Info = state__info_out(State, Label),
	  Arg = hipe_icode:switch_val_arg(I),
	  ArgType = safe_lookup(Arg, Info),
	  case legal_switch_val_cases(Cases, ArgType) of
	    [] ->
	      Fail = hipe_icode:switch_val_fail_label(I),
	      NewState = mk_goto(State, BB, Label, Fail),
	      simplify_controlflow(Left, NewState, true);
	    Cases ->
	      %% Nothing changed!
	      simplify_controlflow(Left, State, Dirty);
	    %% TODO: Find out whether switch_val can fail 
	    %% just as switch_tuple_arity
	    LegalCases ->
	      NewI = hipe_icode:switch_val_cases_update(I, LegalCases),
	      NewBB = hipe_bb:code_update(BB, hipe_bb:butlast(BB) ++ [NewI]),
	      NewState = state__bb_add(State, Label, NewBB),
	      simplify_controlflow(Left, NewState, true)
	  end;
	call ->
	  Info = state__info_out(State, Label),
	  case call_always_fails(I, Info) of
	    false ->
	      Fun = hipe_icode:call_fun(I),
	      case hipe_icode_primops:fails(Fun) of
		false ->
		  case hipe_icode:call_fail_label(I) of
		    [] ->
		      simplify_controlflow(Left, State, Dirty);
		    _ ->
		      NewState = unset_fail(State, BB, Label, I),
		      simplify_controlflow(Left, NewState, true)
		  end;
		true ->
		  simplify_controlflow(Left, State, Dirty)
	      end;
	    true ->
	      case hipe_icode:call_in_guard(I) of
		false ->
		  simplify_controlflow(Left, State, Dirty);
		true ->
		  FailLabel = hipe_icode:call_fail_label(I),
		  NewState = mk_goto(State, BB, Label, FailLabel),
		  simplify_controlflow(Left, NewState, true)
	      end
	  end;
	_ ->
	  simplify_controlflow(Left, State, Dirty)
      end
  end;
simplify_controlflow([], State, true) ->
  %% Redo the type analysis since the cfg has changed.
  {dirty, state__cfg(State)};
simplify_controlflow([], State, _) ->
  State.

mk_goto(State, BB, Label, Succ)->
  NewI = hipe_icode:mk_goto(Succ),
  NewBB = hipe_bb:code_update(BB, hipe_bb:butlast(BB)++[NewI]),
  state__bb_add(State, Label, NewBB).

unset_fail(State, BB, Label, I)->
  %%io:format("Setting a guard that cannot fail\n", []),
  NewI = hipe_icode:call_set_fail_label(I, []),
  NewBB = hipe_bb:code_update(BB, hipe_bb:butlast(BB)++[NewI]),
  state__bb_add(State, Label, NewBB).

%% _________________________________________________________________
%%
%% Make transformations (specialisations) based on the type knowledge. 
%%
%% Annotate the variables with the local information. Since we have
%% the code in SSA form and the type information can only depend on
%% assignments or branches (type tests), we can use the information
%% out of the block to annotate all variables in it.
%%

specialize(State)->
  Cfg = state__cfg(State),
  Labels = hipe_icode_cfg:labels(Cfg),
  transform_bbs(Labels, State).

transform_bbs([Label|Left], State)->
  BB = state__bb(State, Label),
  Code = hipe_bb:code(BB),
  Info = state__info_in(State, Label),
  NewCode = make_transformations(Code, Info, []),
  NewBB = hipe_bb:code_update(BB, NewCode),
  NewState = state__bb_add(State, Label, NewBB),
  transform_bbs(Left, NewState);
transform_bbs([], State) ->
  State.

make_transformations([I|Left], Info, Acc)->
  NewInfo = analyse_insn(I, Info),
  NewI = transform_insn(I, Info),
  make_transformations(Left, NewInfo, [NewI|Acc]);
make_transformations([], _Info, Acc) ->
  lists:reverse(Acc).

transform_insn(I, Info) ->
  case hipe_icode:type(I) of      
    IType when IType == call; IType == enter ->
      case call_or_enter_fun(I) of
	'band' -> transform_arith(I, 'band', Info);
	'bor'  -> transform_arith(I, 'bor', Info);
	'bxor' -> transform_arith(I, 'bxor', Info);
	'bnot' -> transform_arith(I, 'bnot', Info);
	'+'    -> transform_arith(I, '+', Info);
	'-'    -> transform_arith(I, '-', Info);
	{element, _} ->
	  transform_insn(update_call_or_enter(I, {erlang,element,2}), Info);
	{erlang,element,2} -> 
	  NewI1 = transform_element2(I, Info),
	  case IType == call andalso hipe_icode:call_in_guard(I) of
	    true ->
	      case hipe_icode:call_fun(NewI1) of
		{unsafe_element, _} -> NewI1;
		_ -> I
	      end;
	    false -> 
	      NewI1
	  end;
	{erlang,hd,1} -> transform_hd_or_tl(I, unsafe_hd, Info);
	{erlang,tl,1} -> transform_hd_or_tl(I, unsafe_tl, Info);
	{hipe_bs_primop, {bs_put_integer, Size, Flags, ConstInfo}} ->
	  transform_bs_put_integer(I, {hipe_bs_primop,
				       {unsafe_bs_put_integer, 
					Size, Flags, ConstInfo}},
				   Info);
	conv_to_float -> 
	  [Src] = args(I),
	  case t_is_float(safe_lookup(Src, Info)) of
	    true -> 
	      update_call_or_enter(I, unsafe_untag_float);
	    %% hipe_icode:call_fun_update(I, unsafe_untag_float);
	    false -> %% TODO: This information can be used by conv_to_float
	      I
	  end;
	%%      {erlang,What,Arity} ->
	%%	io:format("{erlang, ~w, ~w}\n", [What, Arity]),
	%%	I;
	_ ->
	  I
      end;
    _ ->
      I
  end.

call_or_enter_fun(I) ->
  case hipe_icode:is_call(I) of
    true -> hipe_icode:call_fun(I);
    false -> hipe_icode:enter_fun(I)
  end.

update_call_or_enter(I, NewFun) ->
  case hipe_icode:is_call(I) of
    true -> hipe_icode:call_fun_update(I, NewFun);
    false -> hipe_icode:enter_fun_update(I, NewFun)
  end.

update_call_or_enter(I, NewFun, NewArgs) ->
  case hipe_icode:is_call(I) of
    true -> 
      I1 = hipe_icode:call_args_update(I, NewArgs),
      hipe_icode:call_fun_update(I1, NewFun);
    false -> 
      I1 = hipe_icode:enter_args_update(I, NewArgs),
      hipe_icode:enter_fun_update(I1, NewFun)
  end.


transform_element2(I, Info)->
  %%Any = t_any(),
  [Index, Tuple] = args(I),
  IndexType = safe_lookup(Index, Info),
  TupleType = safe_lookup(Tuple, Info),
  NewIndex =
    case test_type(integer, IndexType) of
      true ->
	case t_number_vals(IndexType) of
	  Vals when is_list(Vals) -> {number, Vals};
	  Other -> Other
	end;
      _ -> 
	[] %% Might fail - don't care.
    end,
  NewTuple =
    case test_type(tuple, TupleType) of
      true ->
	case t_tuple_arity(TupleType) of
	  Arity when is_number(Arity)-> {tuple, Arity};
	  _ -> tuple
	end;
      _ -> [] %% Might fail - don't care.
    end,
  case {NewTuple, NewIndex} of
    {{tuple, A}, {number, N}} ->
      case lists:all(fun(X)->A>=X andalso X>0 end, N) of
	true -> 
	  case N of
	    [Num] ->
	      [_, Tuple] = args(I),
	      update_call_or_enter(I, {unsafe_element, Num}, [Tuple]);
	    _ ->
	      NewFun = {element, [{tuple, A}, valid]},
	      update_call_or_enter(I, NewFun)
	  end;
	false ->
	  case lists:all(fun(X)->hipe_tagscheme:is_fixnum(X) end, N) of
	    true ->
	      NewFun = {element, [{tuple, A}, fixnums]},
	      update_call_or_enter(I, NewFun);
	    false ->
	      NewFun = {element, [{tuple, A}, []]},
	      update_call_or_enter(I, NewFun)
	  end
      end;
    _ ->
      case all_fixnums([IndexType]) of
	true ->
	  NewFun = {element, [NewTuple, fixnums]},
	  update_call_or_enter(I, NewFun);
	false ->
	  NewFun = {element, [NewTuple, NewIndex]},	  
	  update_call_or_enter(I, NewFun)
      end
  end.

transform_hd_or_tl(I, Primop, Info)->
  [Arg] = args(I),
  case t_is_cons(safe_lookup(Arg, Info)) of
    true->
      update_call_or_enter(I, Primop);
    false ->
      I
  end.

transform_bs_put_integer(I, Primop, Info)-> 
  [Src|_] = safe_lookup_list(args(I), Info),
  case all_fixnums([Src]) of
    true ->
      update_call_or_enter(I, Primop);
    false -> I
  end.


transform_arith(I, '+', Info)->
  Args = safe_lookup_list(args(I), Info),
  NewInfo = analyse_insn(I, Info),
  Dst = case hipe_icode:is_call(I) of
	  true -> safe_lookup_list(call_dstlist(I), NewInfo);
	  false -> []
	end,
  case all_fixnums(Dst++Args) of
    true ->
      update_call_or_enter(I, extra_unsafe_add);
    false ->
      case all_fixnums(Args) of
	true ->
	  update_call_or_enter(I, unsafe_add);
	false ->
	  I
      end
  end;
transform_arith(I, Op, Info)->
  Args = safe_lookup_list(args(I), Info),
  case all_fixnums(Args) of
    true -> 
      update_call_or_enter(I, arithop_to_unsafe(Op));
    false -> I
  end.

arithop_to_unsafe(Op)->
  case Op of
    '+'    -> unsafe_add;
    '-'    -> unsafe_sub;
    'band' -> unsafe_band;
    'bor'  -> unsafe_bor;
    'bxor' -> unsafe_bxor;
    'bnot' -> unsafe_bnot  
  end.

get_unsafe_arithop(Fun)->  
  case Fun of
    '+' ->              {true, fun(A, B)-> A + B end};
    '-' ->              {true, fun(A, B)-> A - B end};
    'band' ->           {true, fun(A, B)-> A band B end};
    'bor'  ->           {true, fun(A, B)-> A bor B end};
    'bxor' ->           {true, fun(A, B)-> A bxor B end};
    'bnot' ->           {true, fun(A)-> bnot A end};
    extra_unsafe_add -> {true, fun(A, B)-> A + B end};
    unsafe_add ->       {true, fun(A, B)-> A + B end};
    unsafe_sub ->       {true, fun(A, B)-> A - B end};
    unsafe_band ->      {true, fun(A, B)-> A band B end};
    unsafe_bor ->       {true, fun(A, B)-> A bor B end};
    unsafe_bxor ->      {true, fun(A, B)-> A bxor B end};
    unsafe_bnot ->      {true, fun(A)-> bnot A end};
    _ -> false
  end.

all_fixnums([Type|Left])->
  case t_is_char(Type) of
    true ->
      all_fixnums(Left);
    false ->
      case all_fixnum_values([Type]) of
	true -> all_fixnums(Left);
	false -> false
      end
  end;
all_fixnums([]) ->
  true.

all_fixnum_values([Type|Left])->
  case t_is_number(Type) of
    false -> false;
    true ->
      case t_number_vals(Type) of
	any -> false;
	Vals ->
	  case lists:all(fun(X)-> hipe_tagscheme:is_fixnum(X) end, Vals) of
	    true ->
	      all_fixnum_values(Left);
	    false ->
	      false
	  end
      end
  end;
all_fixnum_values([]) ->
  true.


primop_type(Op, Args)->
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

evaluate_unsafe_arith(Fun, [Arg])->
  sup_list([t_from_term(Fun(X)) || X<-t_number_vals(Arg)]);
evaluate_unsafe_arith(Fun, [Arg1, Arg2])->
  sup_list([t_from_term(Fun(X, Y)) || X<-t_number_vals(Arg1), 
				      Y<-t_number_vals(Arg2)]).

sup_list(List)->
  sup_list(List, t_none()).

sup_list([H|T], Acc)->
  sup_list(T, t_sup(H, Acc));
sup_list([], Acc) ->
  Acc.
	      
  


%% _________________________________________________________________
%%
%% Various help functions.
%%

add_arg_types(Args,Types)->
  add_arg_types(Args, Types, empty()).

add_arg_types([Arg|Args],[Type|Types], Acc)->
  Type1 =
    case t_is_none(Type) of
      true -> t_any();
      false -> Type
    end,
  add_arg_types(Args,Types, enter(Arg, Type1, Acc));
add_arg_types([],[],Acc) ->
  Acc;
add_arg_types(A,B,_) ->
  exit({wrong_number_of_arguments, {A, B}}).


%% Lookup treats anything that is neither in the map or a constant as
%% t_none(). Use this during type propagation!

lookup(Var, Tree)->
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

lookup_list(List, Info)->
  lookup_list0(List, Info, []).

lookup_list0([H|T], Info, Acc)->
  lookup_list0(T, Info, [lookup(H, Info)|Acc]);
lookup_list0([], _, Acc) ->
  lists:reverse(Acc).


%% safe_lookup treats anything that is neither in the map nor a
%% constant as t_any(). Use this during transformations.

safe_lookup(Var, Tree)->
  case gb_trees:lookup(Var, Tree) of
    none ->
      case hipe_icode:is_const(Var) of
	true -> const_type(Var);
	false ->
	  %% io:format("Expression has undefined type\n",[]),
	  t_any()
      end;
    {value,Val} ->
      case hipe_icode:is_var(Val) of
	true ->
	  safe_lookup(Val, Tree);
	false ->
	  Val
      end
  end.

safe_lookup_list(List, Info)->
  safe_lookup_list0(List, Info, []).

safe_lookup_list0([H|T], Info, Acc)->
  safe_lookup_list0(T, Info, [safe_lookup(H, Info)|Acc]);
safe_lookup_list0([], _, Acc) ->
  lists:reverse(Acc).

enter_list([Var|VarLeft], [Type|TypeLeft], Info) ->
  NewInfo = enter(Var, Type, Info),
  enter_list(VarLeft, TypeLeft, NewInfo);
enter_list([], [], Info) ->
  Info.

enter([Key], Value, Tree)->
  enter(Key, Value, Tree);
enter(Key, Value, Tree)->
  case hipe_icode:is_var(Key) of
    true ->
      enter_to_leaf(Key, Value, Tree);
    false ->
      Tree
  end.

enter_to_leaf(Key, Value, Tree)->
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

empty()->
  gb_trees:empty().

join_list(List, Info)->
  join_list(List, Info, t_none()).
 
join_list([H|T], Info, Acc)->
  Type = t_sup(lookup(H, Info), Acc),
  join_list(T, Info, Type);
join_list([], _, Acc) ->
  Acc.

join_info_in(Vars, OldInfo, NewInfo)->
  NewInfo2 = join_info_in(Vars, OldInfo, NewInfo, gb_trees:empty()),
  case info_is_equal(NewInfo2, OldInfo) of
    true -> fixpoint;
    false -> NewInfo2
  end.
      
join_info_in([Var|Left], Info1, Info2, Acc)->
  Type1 = gb_trees:lookup(Var, Info1),
  Type2 = gb_trees:lookup(Var, Info2),
  case {Type1, Type2} of
    {none, none} ->
      join_info_in(Left, Info1, Info2, Acc);
    {none, {value, Val}} ->
      join_info_in(Left, Info1, Info2, gb_trees:insert(Var, Val, Acc));
    {{value, Val}, none} ->
      join_info_in(Left, Info1, Info2, gb_trees:insert(Var, Val, Acc));
    {{value, Val1}, {value, Val2}} ->
      NewVal = t_sup(Val1, Val2),
      join_info_in(Left, Info1, Info2, gb_trees:insert(Var, NewVal, Acc))
  end;
join_info_in([], _Info1, _Info2, Acc) ->
  gb_trees:balance(Acc).

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
      

const_type(Const)->
  t_from_term(hipe_icode:const_value(Const)).

do_updates(State, List)->
  do_updates(State, List, []).

do_updates(State, [{Label, Info}|Tail], Worklist)->
  case state__info_in_update(State, Label, Info) of
    fixpoint ->
      %%io:format("Info in for ~w is: fixpoint\n", [Label]),
      do_updates(State, Tail, Worklist);
    NewState ->
      %%io:format("Info in for ~w is:\n", [Label]),
      %%[io:format("~w: ~p\n", [X, format_type(Y)])
      %%|| {X, Y} <- gb_trees:to_list(state__info_in(NewState, Label))],
      do_updates(NewState, Tail, [Label|Worklist])
  end;
do_updates(State, [], Worklist) ->
  {State, Worklist}.

enter_defines(I, Types, Info) when is_list(Types)->
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

defines(I)->
  keep_vars(hipe_icode:defines(I)).

call_dstlist(I) ->
  hipe_icode:call_dstlist(I).

args(I)->
  hipe_icode:args(I).

uses(I)->
  keep_vars(hipe_icode:uses(I)).

keep_vars(Vars)->
  lists:filter(fun(X)->hipe_icode:is_var(X)end, Vars).

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

-record(state, {succmap, predmap, info_map, cfg, liveness, arg_types,
		created_funs=[]}).

new_state(Cfg)->
  SuccMap = hipe_icode_cfg:succ_map(Cfg),
  PredMap = hipe_icode_cfg:pred_map(Cfg),
  Start = hipe_icode_cfg:start_label(Cfg),  
  Info = case lists:keysearch(arg_type, 1, hipe_icode_cfg:info(Cfg)) of
	   false ->
	     Any = t_any(),
	     lists:foldl(fun(X, Tree) -> gb_trees:insert(X, Any, Tree) end,
			 empty(), hipe_icode_cfg:params(Cfg));
	   {value, {_, ArgType}}->
	     case lists:any(fun(X) -> t_is_none(X) end, ArgType) of
	       true ->
		 %% The call to this function is masked by an error.
		 %% We cannot trust the arguments.
		 Any = t_any(),
		 lists:foldl(fun(X, Tree) -> gb_trees:insert(X, Any, Tree) end,
			     empty(), hipe_icode_cfg:params(Cfg));
	       false ->
		 add_arg_types(hipe_icode_cfg:params(Cfg), ArgType)
	     end
	 end,
  ArgTypes0 = lookup_list(hipe_icode_cfg:params(Cfg), Info),
  %% We cannot trust the arity for closures.
  ArgTypes =
    case hipe_icode_cfg:is_closure(Cfg) of    
      true -> lists:sublist(ArgTypes0, hipe_icode_cfg:closure_arity(Cfg));
      false -> ArgTypes0
    end,
  InfoMap = gb_trees:insert({Start, in}, Info, empty()),
  Liveness = hipe_icode_ssa:ssa_liveness__analyze(Cfg),
  #state{succmap=SuccMap, predmap=PredMap,info_map=InfoMap, 
	 cfg=Cfg, liveness=Liveness, arg_types=ArgTypes}.

state__cfg(#state{cfg=Cfg})->
  Cfg.

state__succ(#state{succmap=SM}, Label)->
  hipe_icode_cfg:succ(SM, Label).

state__pred(#state{predmap=PM}, Label)->
  hipe_icode_cfg:pred(PM, Label).

state__bb(#state{cfg=Cfg}, Label)->
  hipe_icode_cfg:bb(Cfg, Label).
  
state__bb_add(S=#state{cfg=Cfg}, Label, BB)->
  NewCfg = hipe_icode_cfg:bb_add(Cfg, Label, BB),
  S#state{cfg=NewCfg}.

state__info_in(S, Label)->
  state__info(S, {Label, in}).

state__info_out(S, Label)->
  state__info(S, {Label, out}).

state__info(#state{info_map=IM}, Label)->
  case gb_trees:lookup(Label, IM) of
    {value, Info}-> Info;
    none -> gb_trees:empty()
  end.

state__info_map(#state{info_map=IM})->
  IM.

state__info_in_update(S=#state{info_map=IM, liveness=Liveness}, Label, Info)->
  Pred = state__pred(S, Label),
  RawLiveIn = [hipe_icode_ssa:ssa_liveness__livein(Liveness, Label, X) ||
		X <- Pred],
  LiveIn = ordsets:from_list(lists:flatten(RawLiveIn)),
  case gb_trees:lookup({Label, in}, IM) of
    none -> 
      OldInfo = gb_trees:empty(),
      case join_info_in(LiveIn, OldInfo, Info) of
	fixpoint -> 
	  %% If the bb has not been handled we ignore the fixpoint.
%	  io:format("Label: ~w\First handling\nNewInfo: ~p\n", 
%		    [Label, OldInfo]),
	  S#state{info_map=gb_trees:enter({Label, in}, OldInfo, IM)};
	NewInfo ->
%	  io:format("Label: ~w\nFirst handling\nNewInfo: ~p\n", 
%		    [Label, NewInfo]),
	  S#state{info_map=gb_trees:enter({Label, in}, NewInfo, IM)}
      end;
    {value, OldInfo} ->
      case join_info_in(LiveIn, OldInfo, Info) of
	fixpoint -> 
%	  io:format("Label: ~w\nLiveIn: ~p\nOldInfo: ~p\nInfo: ~p\nFixpoint\n", 
%		    [Label, LiveIn, OldInfo, Info]),
	  fixpoint;
	NewInfo ->
%	  io:format("Label: ~w\nOldInfo: ~p\nNewInfo: ~p\n", 
%		    [Label, OldInfo, NewInfo]),
	  S#state{info_map=gb_trees:enter({Label, in}, NewInfo, IM)}
      end
  end.

state__info_out_update(S=#state{info_map=IM}, Label, Info)->
  %%io:format("Info out for ~w is:\n", [Label]),
  %%[io:format("~w: ~p\n", [X, format_type(Y)])|| {X, Y} <- gb_trees:to_list(Info)],
  S#state{info_map=gb_trees:enter({Label, out}, Info, IM)}.

%% _________________________________________________________________
%%
%% The worklist.
%%

init_work(State)->
  %%Labels = hipe_icode_cfg:reverse_postorder(state__cfg(State)),
  Labels = [hipe_icode_cfg:start_label(state__cfg(State))],
  {Labels, [], gb_sets:from_list(Labels)}.

get_work({[Label|Left], List, Set})->
  NewWork = {Left, List, gb_sets:delete(Label, Set)},
  {Label, NewWork};
get_work({[], [], _Set}) ->
  fixpoint;
get_work({[], List, Set}) ->
  get_work({lists:reverse(List), [], Set}).

add_work(Work = {List1, List2, Set},[Label|Left])->
  case gb_sets:is_member(Label, Set) of
    true ->
      add_work(Work, Left);
    false ->
      %%io:format("Adding work: ~w\n", [Label]),
      add_work({List1, [Label|List2], gb_sets:insert(Label, Set)}, Left)
  end;
add_work(Work, []) ->
  Work.


%% _________________________________________________________________
%%
%% Handle warnings on type errors.
%%

call_always_fails(I, Info)->
  {Args, Type, Fun} = 
  case hipe_icode:type(I) of      
    call ->
      {safe_lookup_list(hipe_icode:call_args(I), Info),
       hipe_icode:call_type(I),
       hipe_icode:call_fun(I)};
    enter ->
      {safe_lookup_list(hipe_icode:enter_args(I), Info),
       hipe_icode:enter_type(I),
       hipe_icode:enter_fun(I)}
  end,
  case Fun of
    %% These can actually be calls too.
    {erlang, halt, 0} -> false;
    {erlang, exit, 1} -> false;
    {erlang, error, 1} -> false;
    {erlang, error, 2} -> false;
    {erlang, fault, 1} -> false;
    {erlang, fault, 2} -> false;
    {erlang, throw, 1} -> false;
    Fun ->
      case Type of
	primop -> 
	  ReturnType = primop_type(Fun, Args),
	  t_is_none(ReturnType);
	_ -> t_is_none(find_plt_return(Fun, Args))
      end
  end.

%% The call might be using the old fun-notation, that is a 2-tuple.
check_for_tuple_as_fun(Fun, [Arg|TailArgs]) ->
  case t_is_tuple(t_inf(t_tuple(2), Arg)) of
    false ->
      %% This cannot succed!
      false;
    true ->
      not t_is_none(t_inf(find_plt_return(Fun, [t_fun()|TailArgs]),
			  primop_type(Fun, [t_fun()|TailArgs])))
  end.


warn_on_type_errors(PreAnalysisCfg, OrigState, FinalState, IcodeFun, Options) ->
  Warn =
    case proplists:get_value(type_warnings, Options) of
      true ->
	true;
      {pid, Pid} ->
	put(warn_pid, Pid),
	true;
      _ ->
	false
    end,
  case Warn of
    true ->
      case maybe_check_for_inlining(IcodeFun, Options) of
	true ->
	  ok;
	false ->
	  warn_on_args(IcodeFun, PreAnalysisCfg),
	  OrigCfg = state__cfg(OrigState),
	  OrigLabels = hipe_icode_cfg:reverse_postorder(OrigCfg),
	  warn_on_control_flow(OrigLabels, OrigState, IcodeFun),
	  FinalCfg = state__cfg(FinalState),
	  FinalLabels = hipe_icode_cfg:reverse_postorder(FinalCfg),
	  warn_on_bb(FinalLabels, FinalState, IcodeFun),
	  {RetType, IsExplicit, HasReturn} = 
	    find_return_type(FinalLabels, FinalState),
	  case t_is_none(RetType) of
	    true ->
	      case (not HasReturn) andalso (not IsExplicit) of
		true ->
		  warn(?WARN_RETURN_NO_RETURN, 
		       io_lib:format("~w: Function has no local return\n",
				     [IcodeFun]));
		false ->
		  case IsExplicit andalso not HasReturn of
		    true ->
		      warn(?WARN_RETURN_ONLY_EXIT, 
			   io_lib:format("~w: Function only terminates with "
					 "explicit exception\n", [IcodeFun]));
		    false ->
		      ok
		  end
	      end;
	    false ->
	      ok
	  end
      end;
    false ->
      ok
  end.

check_for_exit_enters(Fun) ->
  case Fun of
    {erlang, halt, 0} -> true;
    {erlang, exit, 1} -> true;
    {erlang, error, 1} -> true;
    {erlang, error, 2} -> true;
    {erlang, fault, 1} -> true;
    {erlang, fault, 2} -> true;
    {erlang, throw, 1} -> true;
    _ -> false
  end.

maybe_check_for_inlining({M, _F, _A}, Options) ->
  case proplists:get_bool(check_for_inlining, Options) of
    true ->
      ModInfo = M:module_info(),
      {value, {compile, CompInfo}} = lists:keysearch(compile, 1, ModInfo),
      case lists:keysearch(options, 1, CompInfo) of
	{value, {options, CompOpts}} -> 
	  lists:any(fun is_inline_option/1, CompOpts);
	false -> false
      end;
    false ->
      false
  end.

is_inline_option(X) -> 
  case X of
    inline -> true;
    {inline, _} -> true;
    _ -> false
  end.
      
warn_on_args(IcodeFun, Cfg) ->
  case lists:keysearch(arg_type, 1, hipe_icode_cfg:info(Cfg)) of
    false ->
      ok;
    {value, {_, ArgType}} when length(ArgType) > 0 ->
      IsNone = fun(X) -> t_is_none(X) end,
      %% XXX: the following should be a call to lists:any() !!
      %%      it is temporarily a call to lists:all() to supress a bug
      case lists:any(IsNone, ArgType) of
	true -> 
	  warn(?WARN_NOT_CALLED,
	       io_lib:format("~w: Function will never be called!\n", [IcodeFun]));
	false ->
	  maybe_send_args(IcodeFun, ArgType),
	  ok
      end;
    _ ->
      ok
  end.

warn_on_bb([Label|Left], State, IcodeFun) ->
  BB = state__bb(State, Label),
  Code = hipe_bb:code(BB),
  InfoOut = state__info_out(State, Label),
  case gb_trees:is_empty(InfoOut) of
    true ->
      %% This block is in a non-possible trace.
      warn_on_bb(Left, State, IcodeFun);
    false ->
      InfoIn = state__info_in(State, Label),
      warn_on_instr(Code, InfoIn, IcodeFun),
      warn_on_bb(Left, State, IcodeFun)
  end;
warn_on_bb([], _State, _IcodeFun) ->
  ok.

warn_on_instr([I|Left], Info, IcodeFun) ->
  NewInfo = analyse_insn(I, Info),
  case any_is_none(lookup_list(args(I), Info)) of
    true -> 
      %% Try to avoid follow-up warnings.
      warn_on_instr(Left, NewInfo, IcodeFun); 
    false ->
      Left1 = 
	case hipe_icode:type(I) of
	  call ->
	    case hipe_icode:call_fun(I) of
	      cons -> 
		warn_on_cons(hipe_icode:call_args(I), Info, IcodeFun),
		Left;
	      {erlang, '++', 2} -> 
		'warn_on_++'(I, Info, IcodeFun),
		Left;
	      call_fun -> 
		warn_on_call_fun(hipe_icode:call_args(I), Info, IcodeFun),
		Left;
	      _ -> 
		warn_on_call(I, Left, Info, IcodeFun)
	    end;
	  enter ->
	    case hipe_icode:enter_fun(I) of
	      cons -> 
		warn_on_cons(hipe_icode:enter_args(I), Info, IcodeFun);
	      {erlang, '++', 2} -> 
		'warn_on_++'(I, Info, IcodeFun);
	      enter_fun -> 
		warn_on_call_fun(hipe_icode:enter_args(I), Info, IcodeFun);
	      _ -> 
		warn_on_enter(I, Info, IcodeFun)
	    end,
	    [];
	  _ ->
	    Left
	end,
      warn_on_instr(Left1, NewInfo, IcodeFun)
  end;
warn_on_instr([], _Info, _IcodeFun) ->
  ok.


warn_on_control_flow([Label|Left], State, IcodeFun) ->
  I = hipe_bb:last(state__bb(State, Label)),
  Info = state__info_out(State, Label),
  case any_is_none(lookup_list(args(I), Info)) of
    true -> 
      %% Try to avoid follow-up warnings.
      ok;
    false ->
      case hipe_icode:type(I) of
	'if' ->
	  warn_on_if(I, Info, IcodeFun);
	switch_tuple_arity ->      
	  warn_on_switch_tuple_arity(I, Info, IcodeFun);
	switch_val ->
	  warn_on_switch_val(I, Info, IcodeFun);
	type ->
	  warn_on_type(I, Info, IcodeFun, State);
	call ->
	  case hipe_icode:call_in_guard(I) of
	    true ->
	      warn_on_call(I, Info, IcodeFun);
	    false ->
	      ok
	  end;
	_ ->
	  ok
      end
  end,
  warn_on_control_flow(Left, State, IcodeFun);
warn_on_control_flow([], _State, _IcodeFun) ->
  ok.

warn_on_type(I, Info, IcodeFun, State) ->
  FalseLab = hipe_icode:type_false_label(I),
  case do_type(I, Info) of
    [{FalseLab, _}] ->
      Test = hipe_icode:type_type(I),
      [Arg] = hipe_icode:type_args(I),
      ArgType = format_type(safe_lookup(Arg, Info)),
      W = 
	case Test of
%	  tuple ->
%	    TrueLabel = hipe_icode:type_true_label(I),
%	    String = construct_tuple(TrueLabel, State),
%	    io_lib:format(String, [IcodeFun, ArgType]);
	  {tuple, N} ->
	    TrueLabel = hipe_icode:type_true_label(I),
	    Tuple = construct_n_tuple(N, TrueLabel, State),
	    io_lib:format("~w: Pattern matching with ~s will always fail since "
			  "variable has type ~s!\n",
			  [IcodeFun, Tuple, ArgType]);
	  {record, Atom, _Size} ->
	    io_lib:format("~w: Pattern matching with #~w{} will always "
			  "fail since variable has type ~s!\n",
			  [IcodeFun, Atom, ArgType]);
	  _ ->
	    io_lib:format("~w: Type guard ~w will always fail since "
			  "variable has type ~s!\n", [IcodeFun, Test, ArgType])
	end,
      warn(?WARN_MATCHING, W);
    _ ->
      ok
  end.

construct_n_tuple(N, _Label, _State) ->
  "{"++construct_underscores(N, [])++"}".
%%  case hipe_bb:code(state__bb(State, Label)) of
%%    [I1, I2] -> 
%%      case {hipe_icode:type(I1), hipe_icode:type(I2)} of
%%	{call, type} -> 
%%	  case hipe_icode:call_fun(I1) of
%%	    {unsafe_element, 1} -> 
%%	      Dst = defines(I1),
%%	      case hipe_icode:type_type(I2) of
%%		{atom, Atom} -> 
%%		  case args(I2) of
%%		    Dst -> "#"++atom_to_list(Atom)++"{}";
%%		    _ -> "{"++construct_underscores(N, [])++"}"
%%		  end;
%%		_ -> "{"++construct_underscores(N, [])++"}"
%%	      end;
%%	    _ -> "{"++construct_underscores(N, [])++"}"
%%	  end;
%%	_ -> "{"++construct_underscores(N, [])++"}"
%%      end;
%%    _ -> "{"++construct_underscores(N, [])++"}"
%%  end.

construct_underscores(1, Acc) ->
  "_"++Acc;
construct_underscores(N, Acc) ->
  construct_underscores(N-1, ",_"++Acc).

warn_on_cons(Args, Info, IcodeFun) ->
  [_Head, Tail] = safe_lookup_list(Args, Info),
  case t_is_list(t_inf(Tail, t_list())) of
    true ->
      ok;
    false ->
      W = io_lib:format("~w: Cons will produce"
			" a non-proper list since its 2nd arg is ~s!\n",
			[IcodeFun, format_type(Tail)]),
      warn(?WARN_NON_PROPER_LIST, W)
  end.

'warn_on_++'(I, Info, IcodeFun) ->
  case call_always_fails(I, Info) of
    true ->
      print_call_warning({erlang, '++', 2}, IcodeFun, 
			 safe_lookup_list(args(I), Info));
    false ->
      [_Head, Tail] = safe_lookup_list(args(I), Info),
      case t_is_list(t_inf(Tail, t_list())) of
	true ->
	  ok;
	false ->
	  W = io_lib:format("~w: Call to '++'/2 will produce"
			    " a non-proper list; 2nd arg is ~s!\n",
			    [IcodeFun, format_type(Tail)]),
	  warn(?WARN_NON_PROPER_LIST, W)
      end
  end.

warn_on_call_fun(Args0, Info, IcodeFun) ->
  Args = safe_lookup_list(Args0, Info),
  [Fun|TailArgs0] = lists:reverse(Args),
  TailArgs = lists:reverse(TailArgs0),
  ReturnType = primop_type(call_fun, Args),
  case t_is_none(ReturnType) of
    false ->
      ok;
    true ->
      {Tag, W} =
	case t_is_fun(Fun) of
	  true ->
	    {?WARN_FUN_APP, 
	     io_lib:format("~w: Trying to use fun with type ~s "
			   "with arguments ",
			   [IcodeFun, format_type(Fun)]) ++
	     pp_args(TailArgs) ++
	     io_lib:format("!\n", [])};
	  false ->
	    case t_is_tuple(Fun) of
	      true ->
		{?WARN_TUPLE_AS_FUN,  
		 io_lib:format("~w: Tuple used as fun will fail in "
			       "native compiled code.\n", [IcodeFun])};
	      false ->
		{?WARN_FUN_APP,
		 io_lib:format("~w: Fun application using type ~s "
			       "instead of a fun!\n",
			       [IcodeFun, format_type(Fun)])}
	    end
	end,
      warn(Tag, W)
  end.

pp_args(Args)->
  "(" ++ pp_args_1(Args) ++ ")".

pp_args_1([Arg1, Arg2|Tail]) ->
  case t_is_any(Arg1) of
    true -> "_";
    false -> format_type(Arg1)
  end
    ++ "," ++ pp_args_1([Arg2|Tail]);
pp_args_1([Arg]) ->
  case t_is_any(Arg) of
    true -> "_";
    false -> format_type(Arg)
  end;
pp_args_1([]) ->
  [].

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
      case lists:all(fun(N)->is_float(N)end, Numbers) of
	true ->
	  case t_is_any(T) of
	    true -> io_lib:format("a float", []);
	    false -> io_lib:format("the float ~s", [t_to_string(T)])
	  end;
	false ->
	  case lists:all(fun(N)->is_integer(N)end, Numbers) of
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
      warn_on_call_1(I, Info, IcodeFun),
      Ins;
    false  ->
      CallFun = hipe_icode:call_fun(I),
      Warn = 
	case CallFun of
	  {mkfun, {_M, F, _A},_,_} when length(Ins) == 1 -> {true, {'fun', F}};
	  {erlang, make_ref, 0} when length(Ins) == 1 -> {true, ref};
	  _  -> false
	end,
      case Warn of
	{true, Type} ->
	  [I2] = Ins,
	  case ((hipe_icode:type(I2) =:= 'if') andalso 
		(hipe_icode:if_op(I2) =:= '=:=')) of
	    true ->
	      [Def] = defines(I),
	      case [X || X <- args(I2), Def =:= X] of
		[_] ->
		  W = 
		    case Type of
		      {'fun', Name} ->
			io_lib:format("~w: Matching on newly created fun "
				      "~w will never succeed\n",
				      [IcodeFun, Name]);
		      ref ->
			io_lib:format("~w: Matching on newly created reference "
				      "will never succeed\n",
				      [IcodeFun])
		    end,
		  warn(?WARN_MATCHING, W),
		  [];
		_ -> Ins
	      end;
	    _ -> Ins
	  end;
	_ -> Ins
      end
  end.

warn_on_call(I, Info, IcodeFun) ->
  case call_always_fails(I, Info) of
    true ->
      warn_on_call_1(I, Info, IcodeFun);
    false ->
      ok
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
	  W = io_lib:format("~w: Unsafe use of tuple as a fun"
			    " in call to ~w\n", [IcodeFun, Fun]),
	  warn(?WARN_TUPLE_AS_FUN, W);
	false ->
	  print_call_warning(Fun, IcodeFun, safe_lookup_list(args(I), Info))
      end
  end.

warn_on_enter(I, Info, IcodeFun)->
  case call_always_fails(I, Info) of
    true ->
      Fun = hipe_icode:enter_fun(I),
      Args = safe_lookup_list(hipe_icode:enter_args(I), Info),
      case (hipe_icode:enter_type(I) =/= primop) andalso 
	check_for_tuple_as_fun(Fun, Args) of
	true -> 
	  W = io_lib:format("~w: Unsafe use of tuple as a fun in call "
			    "to ~w\n", [IcodeFun, Fun]),
	  warn(?WARN_TUPLE_AS_FUN, W);
	false ->
	  print_call_warning(Fun, IcodeFun, safe_lookup_list(args(I), Info))
      end;
    false ->
      ok
  end.


warn_on_switch_tuple_arity(I, Info, IcodeFun) ->
  Arg = hipe_icode:switch_tuple_arity_arg(I),
  ArgType = safe_lookup(Arg, Info),
  Cases = hipe_icode:switch_tuple_arity_cases(I),
  case legal_switch_tuple_arity_cases(Cases, ArgType) of
    Cases ->
      ok;
    LegalCases ->
      ArgType1 = format_type(ArgType),
      IllegalCases = [X || X <- Cases, not lists:member(X, LegalCases)],
      Arities = [hipe_icode:const_value(X) || {X, _} <- IllegalCases],
      Ws = [io_lib:format("~w: The clause matching on tuple with"
			  " arity ~w will never match;"
			  " argument is of type ~s!\n", 
			  [IcodeFun, X, ArgType1])|| X <- Arities],
      [warn(?WARN_MATCHING, X) || X <- Ws],
      ok
  end.

warn_on_switch_val(I, Info, IcodeFun) ->
  Arg = hipe_icode:switch_val_arg(I),
  ArgType = safe_lookup(Arg, Info),
  Cases = hipe_icode:switch_val_cases(I),
  case legal_switch_val_cases(Cases, ArgType) of
    Cases ->
      ok;
    LegalCases ->
      IllegalCases = [X || X <- Cases, not lists:member(X, LegalCases)],
      Vals = [t_from_term(hipe_icode:const_value(X)) || {X, _} <- IllegalCases],
      ArgTypeString = format_type(ArgType),
      Ws = [io_lib:format("~w: The clause matching on ~s will"
			  " never match; argument is of type ~s\n", 
			  [IcodeFun, format_type(X), ArgTypeString])
	    || X <- Vals],
      [warn(?WARN_MATCHING, X) || X <- Ws],
      ok
  end.

warn_on_if(I, Info, IcodeFun) ->
  TrueLab = hipe_icode:if_true_label(I),
  FalseLab = hipe_icode:if_false_label(I),
  Op = hipe_icode:if_op(I),
  case do_if(I, Info) of
    [_, _] ->
      ok;
    [{FalseLab, _}] ->
      W = 
	case Op of
	  '=:=' ->
	    [Arg1, Arg2] = safe_lookup_list(hipe_icode:if_args(I), Info),
	    io_lib:format("~w: =:= between ~s and ~s will"
			  " always fail!\n",
			  [IcodeFun, format_type(Arg1), format_type(Arg2)]);
	  '=/=' ->
	    [Arg1, Arg2] = safe_lookup_list(hipe_icode:if_args(I), Info),
	    io_lib:format("~w: =/= between ~s and ~s will"
			  "always fail!\n",
			  [IcodeFun, format_type(Arg1), format_type(Arg2)])
	end,
      warn(?WARN_COMP, W);
    [{TrueLab, _}] ->
      ok
  end.

warn_on_guard(I, IcodeFun, Args) ->
  Fun = hipe_icode:call_fun(I),  
  W = io_lib:format("~w: The guard ~w will always fail since "
		    "the arguments are of type ~s!\n", 
		    [IcodeFun, format_fun(Fun), pp_args(Args)]),
  warn(?WARN_GUARDS, W).

print_call_warning(Fun, IcodeFun, Args) ->
  case hipe_icode_primops:fails(Fun) of
    true ->
      Signature = find_signature(Fun, length(Args)),
      W = io_lib:format("~w: Call to function ~w with signature ~s will fail "
			"since the arguments are of type ~s!\n",
			[IcodeFun, format_fun(Fun), t_to_string(Signature),
			 pp_args(Args)]),
      warn(?WARN_FAILING_CALL, W);
    false ->
      W = io_lib:format("~w: Unsafe BEAM code! "
			"Please recompile with a newer BEAM compiler.\n",
			[IcodeFun]),
      warn(?WARN_OLD_BEAM, W)
  end.

format_fun({element, _}) ->
  {erlang, element, 2};
format_fun(Other) ->
  Other.


warn(Tag, W) ->
  case get(warn_pid) of
    undefined ->
      io:format(W, []);
    Pid when is_pid(Pid) -> Pid ! {type_warning, {Tag, W}}
  end.

maybe_send_args(IcodeFun, Args) ->
  case get(warn_pid) of
    undefined ->
      ok;
    Pid when is_pid(Pid) ->
      IsAnyOrNone = fun(X) -> t_is_none(X) orelse t_is_any(X) end,
      case lists:all(IsAnyOrNone, Args) of
	true -> ok;
	false -> 
	  Pid ! {arg_call_types, IcodeFun, Args}
      end
  end.


%% _________________________________________________________________
%%
%% Pretty printer
%%

pp(State, IcodeFun, Options, Msg)->
  PP =
    case proplists:get_value(pp_typed_icode, Options) of
      true ->
	true;
      {only,Lst} ->
	lists:member(IcodeFun,Lst);
      Other ->
	Other
    end,
  case PP of
    true ->
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
      end;

    {file,FileName} ->
      AnnotatedCfg = state__cfg(annotate_cfg(State)),
      Labels = hipe_icode_cfg:labels(AnnotatedCfg),
      {ReturnType, _, _} = find_return_type(Labels, State),
      {ok,File} = file:open(FileName,[write,append]),
      io:format(File, "-------------- " ++ Msg ++ " ---------------\n", []),
      hipe_icode_cfg:pp(File, AnnotatedCfg),
      case t_is_none(ReturnType) of
	true ->
	  io:format(File, "Function ~w will never return a proper value!\n", 
		    [IcodeFun]);
	false ->
	  io:format(File, "Return type for ~w: ~s\n", [IcodeFun, 
						       format_type(ReturnType)])
      end;
    _ ->
      ok
  end.

annotate_cfg(State) ->
  Cfg = state__cfg(State),
  Labels = hipe_icode_cfg:labels(Cfg),
  annotate_bbs(Labels, State).
  
annotate_bbs([Label|Left], State)->
  BB = state__bb(State, Label),
  Code = hipe_bb:code(BB),
  Info = state__info_in(State, Label),
  NewCode = annotate_instr_list(Code, Info, []),
  NewBB = hipe_bb:code_update(BB, NewCode),
  NewState = state__bb_add(State, Label, NewBB),
  annotate_bbs(Left, NewState);
annotate_bbs([], State) ->
  State.

annotate_instr_list([I], Info, Acc)->
  case hipe_icode:type(I) of
    call ->
      NewInfo = do_call(I, Info),
      NewI = annotate_instr(I, NewInfo, Info),
      lists:reverse([NewI|Acc]);
    _ ->
      NewInfo = analyse_insn(I, Info),
      NewI = annotate_instr(I, NewInfo, Info),
      lists:reverse([NewI|Acc])
  end;
annotate_instr_list([I|Left], Info, Acc)->
  NewInfo = analyse_insn(I, Info),
  NewI = annotate_instr(I, NewInfo, Info),
  annotate_instr_list(Left, NewInfo, [NewI|Acc]).

annotate_instr(I, DefInfo, UseInfo)->
  Def = defines(I),
  Use = uses(I),
  Fun = fun(X, Y)->hipe_icode:annotate_var(X, Y)end,
  DefSubst = [{X, Fun(X, lookup(X, DefInfo))}||X<-Def],
  UseSubst = [{X, Fun(X, lookup(X, UseInfo))}||X<-Use],
  case  DefSubst ++ UseSubst of
    [] ->
      I;
    Subst ->
      hipe_icode:subst(Subst, I)
  end.


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
      case hipe_icode:type(I) of
	return ->
	  Info = state__info_in(State, Label),
	  Code = hipe_bb:code(BB),
	  NewReturnType = find_return_type_in_block(Code, Info),
	  find_return_type(Left, State, t_sup(NewReturnType, ReturnType),
			   ExplExit, true);
	enter ->
	  Info = state__info_in(State, Label),
	  Code = hipe_bb:code(BB),
	  NewReturnType = find_return_type_in_block(Code, Info),
	  ExplExit1 = check_for_exit_enters(hipe_icode:enter_fun(I)),
	  NewExplExit = ExplExit orelse ExplExit1,
	  NewHasReturn = HasReturn orelse not ExplExit1,
	  find_return_type(Left, State, t_sup(NewReturnType, ReturnType),
			   NewExplExit, NewHasReturn);
	fail ->
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
  case hipe_icode:type(I) of
    enter ->
      Fun = hipe_icode:enter_fun(I),
      Args = hipe_icode:enter_args(I),
      ArgTypes = lookup_list(Args, Info),
      case hipe_icode:enter_type(I) of
	primop ->
	  primop_type(Fun, ArgTypes);
	_ ->
	  {M, F, A} = Fun,
	  PltType = find_plt_return(Fun, ArgTypes),
	  BifType = erl_bif_types:type(M, F, A, ArgTypes),
	  t_inf(BifType, PltType)
      end;
    return ->
      [Arg] = hipe_icode:return_vars(I),
      lookup(Arg, Info)
  end;
find_return_type_in_block([I|Left], Info) ->
  find_return_type_in_block(Left, analyse_insn(I, Info)).
  

%% _________________________________________________________________
%%
%% Find the types of the arguments to a call
%%

update_call_arguments(I, Info) ->
  Args = hipe_icode:call_args(I),
  ArgTypes = lookup_list(Args, Info),
  Signature = find_signature(hipe_icode:call_fun(I), length(Args)),
  PltArgTypes = t_fun_args(Signature),
  case t_is_any(PltArgTypes) of
    true ->
      Info;
    false ->
      NewArgTypes = t_inf_lists(ArgTypes, PltArgTypes),
      enter_list(Args, NewArgTypes, Info)
  end.

%% _________________________________________________________________
%%
%% Plt info
%%


init_mfa_info(MFA, Options) ->
  case proplists:get_value(icode_type, Options) of
    {plt, Plt} ->
      put(hipe_mfa_plt, {ok, Plt}),
      plt_lookup(MFA);
    _ ->
      put(hipe_mfa_plt, none),
      none
  end.

update_mfa_info(State, IcodeFun, OldSig, Options) ->
  case proplists:get_value(icode_type, Options) of
    {plt, Plt} ->      
      Labels = hipe_icode_cfg:labels(state__cfg(State)),
      {ReturnType, _, _} = find_return_type(Labels, State),
      ArgTypes=State#state.arg_types,
      NewSig = t_fun(ArgTypes, ReturnType),
      case t_is_none(NewSig) of
	true ->
	  NewReturnType = t_none();
	false ->
	  NewReturnType = t_fun_range(NewSig)
      end,
      case t_is_none(NewReturnType) of
	true ->
	  insert_mfa(Plt, {IcodeFun, t_any(), ArgTypes});
	false ->
	  insert_mfa(Plt, {IcodeFun, NewReturnType, ArgTypes})
      end,
      OldReturnType = t_fun_range(OldSig),
      case proplists:get_value(use_callgraph, Options) of
	fixpoint ->
	  case t_is_none(NewReturnType) andalso t_is_any(OldReturnType) of
	    true -> 
	      fixpoint;
	    false ->
	      case t_is_equal(NewReturnType, OldReturnType) of
		true ->
		  fixpoint;
		false ->
%%		  io:format("~w: Not fixpoint: ~s /= ~s\n", 
%%			    [IcodeFun, format_type(ReturnType), 
%%			     format_type(OldReturnType)]),
		  not_fixpoint
	      end
	  end;
	_ ->
	  ok
      end;
    _ ->
      ok
  end.

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

find_signature_primop(Primop, Arity) ->
  case erl_bif_types:arg_types(Primop) of
    any ->
      t_fun(Arity, hipe_icode_primops:type(Primop));
    ArgTypes ->
      t_fun(ArgTypes, hipe_icode_primops:type(Primop))
  end.

find_plt_return(MFA, ArgTypes) ->
  PltSig = plt_lookup(MFA),
  BifSig = find_signature_mfa(MFA),
  Sig = t_inf(PltSig, BifSig),
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
  case get(hipe_mfa_plt) of
    none ->
      t_fun();
    {ok, Plt} ->
      case lookup_mfa(Plt, MFA) of
	none ->
	  t_fun();
%	{value, {_, RetType}} ->
%	  t_fun(RetType);
	{value, {_, RetType, ArgTypes}} ->
	  t_fun(ArgTypes, RetType)
      end
  end.

lookup_mfa(Plt, MFA) ->
  case ets:lookup(Plt, MFA) of
    [Val] -> {value, Val};
    [] -> none
  end.

insert_mfa(Plt, Object) ->
  ets:insert(Plt, Object).
