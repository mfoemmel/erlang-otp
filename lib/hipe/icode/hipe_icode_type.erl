%%%-------------------------------------------------------------------
%%% File    : hipe_icode_type.erl
%%% Author  : Tobias Lindahl <Tobias.Lindahl@it.uu.se>
%%% Description : Propagate type information.
%%%
%%% Created : 25 Feb 2003 by Tobias Lindahl <Tobias.Lindahl@it.uu.se>
%%%-------------------------------------------------------------------

%% TODO: Change the representation of the {erlang, element, 2, *}

-module(hipe_icode_type).

-export([cfg/3]).

-import(erl_types, [t_any/0, t_atom/1, t_atom/0, t_atom_vals/1, 
		    t_binary/0, t_cons/0, 
		    t_cons_or_nil/0, t_float/0, 
		    t_from_term/1, t_fun/0, t_inf/2, 
		    t_integer/0, t_integer/1, t_nil/1, 
		    t_is_atom/1,
		    t_is_any/1, t_is_binary/1, t_is_cons/1, 
		    t_is_cons_or_nil/1, t_is_float/1, 
		    t_is_fun/1, t_is_integer/1, t_is_number/1,
		    t_is_list/1, t_is_nil/1, 
		    t_is_port/1, t_is_pid/1, t_is_ref/1, t_is_tuple/1,
		    t_is_undefined/1, t_list/0, t_nil/0, t_nonempty_list/0,
		    t_number/0, t_number/1, t_number_vals/1,
		    t_pid/0, t_port/0, t_ref/0, t_subtract/2, t_sup/2, 
		    t_to_string/1,
		    t_tuple/0, t_tuple/1, t_tuple_arity/1, t_undefined/0]).

%% If debug_test is set to true the type tests are not removed but
%% fail code is inserted, so if at runtime the outcome is not the
%% expected one the program fails with an error message.
%%-define(debug_test, true).
-define(debug_test, false).

cfg(Cfg, IcodeFun, Options) ->
  State = analyse(Cfg, IcodeFun, Options),
  state__cfg(State).

analyse(Cfg, IcodeFun, Options)->
  %%hipe_icode_cfg:pp(Cfg),
  State = new_state(Cfg),
  Labels = hipe_icode_cfg:reverse_postorder(Cfg),
  NewState1 = analyse_blocks(Labels, State),
  case remove_typetests(Labels, NewState1, false) of
    {dirty, NewState2} ->
      %% Redo the analysis. See the comment at remove_typetest.
      NewCfg = hipe_icode_cfg:remove_unreachable_code(state__cfg(NewState2)),
      analyse(NewCfg, IcodeFun, Options);
    NewState2 ->
      %%hipe_icode_cfg:pp(state__cfg(NewState2)),      
      annotate(NewState2, IcodeFun, Options)
  end.


%% _________________________________________________________________
%% 
%% Global analysis on the whole function. Demands that the code is in
%% SSA-form. When we encounter a phi-node the types of the arguments
%% are joined. At the end of a block the information out is joined
%% with the current information in for all _valid_ successors, that
%% is, of all successors that actually can be reached. If the join
%% produces new information in for the successor it is added to the
%% worklist.
%%

analyse_blocks(Labels, State)->
  analyse_blocks(Labels, Labels, State, gb_sets:empty()).

analyse_blocks(Labels, [Label|Left], State, Acc)->
  Info = state__info_in(State, Label),
  {NewState, NewLabels} = analyse_block(Label, Info, State),
  NewAcc = lists:foldl(fun(X,Y)->gb_sets:add(X, Y)end, Acc, NewLabels),
  analyse_blocks(Labels, Left, NewState, NewAcc);
analyse_blocks(Labels, [], State, Acc) ->
  case gb_sets:is_empty(Acc) of
    true ->
      State;
    _ ->
      Worklist = lists:filter(fun(X)->gb_sets:is_member(X, Acc)end,Labels),
      analyse_blocks(Labels, Worklist, State, gb_sets:empty())
  end.	  
  
analyse_block(Label, InfoIn, State)->
  %%io:format("Handling ~w\n", [Label]),
  BB = state__bb(State, Label),
  Code = hipe_bb:code(BB),
  Last = hipe_bb:last(BB),
  NewInfoIn = analyse_insns(Code, InfoIn),
  NewState = state__info_out_update(State, Label, NewInfoIn),

  case hipe_icode:type(Last) of
    type ->
      UpdateInfo = do_type(Last, NewInfoIn),
      %%io:format("Update info for ~w:\n~w", [Label, UpdateInfo]),
      do_updates(NewState, UpdateInfo);
    _ ->
      %% TODO: switch and switch_tuple_arity
      UpdateInfo = [{X, NewInfoIn}||X<-state__succ(NewState, Label)],
      %%io:format("Update info for ~w:\n~w", [Label, UpdateInfo]),
      do_updates(NewState, UpdateInfo)
  end.

analyse_insns([I|Insns], Info)->
  NewInfo = 
    case hipe_icode:type(I) of
      mov ->
	do_mov(I, Info);
      call ->
	do_call(I, Info);
      phi ->
	Type = join_list(uses(I), Info),
	enter_defines(I, Type, Info);
      _ ->
	enter_defines(I, t_any(), Info)
    end,
  analyse_insns(Insns, NewInfo);  
analyse_insns([], Info) ->
  Info.

do_mov(I, Info)->
  %% Can't use uses/1 since we must keep constants.
  Src = hipe_icode:mov_src(I), 
  case const_type(Src) of
    not_a_constant ->
      %% Make the destination point to the source.
      enter_defines(I, Src, Info);
    ConstType ->
      enter_defines(I, ConstType, Info)
  end.

do_call(I, Info)->
  case hipe_icode:call_type(I) of
    remote ->
      {M, F, A} = hipe_icode:call_fun(I),
      ArgTypes = lookup_list(uses(I), Info),
      Type = erl_bif_types:type(M, F, A, ArgTypes),
      enter_defines(I, Type, Info);
    local ->
      case lists:keysearch(dst_type, 1, hipe_icode:info(I)) of
	false ->
	  enter_defines(I, t_any(), Info);
	{value,{_, [Type]}}->
	  %%io:format("The result of the call to ~w is ~w\n", 
	  %%[hipe_icode:call_fun(I), Type]),
	  enter_defines(I, Type, Info)
      end;
    primop ->
      Fun = hipe_icode:call_fun(I),
      ArgType = lookup_list(uses(I), Info),
      DstType = hipe_rtl_primops:type(Fun, ArgType),
%%      io:format("Entering ~s for ~w as a return from\n", 
%%                [erl_types:t_to_string(hd(DstType)), defines(I)]),
%%      hipe_icode_pp:pp_instrs(standard_io, [I]),
      enter_defines(I, DstType, Info)
  end.

do_type(I, Info)->
  [Var] = uses(I),
  TrueLab = hipe_icode:type_true_label(I),
  FalseLab = hipe_icode:type_false_label(I),
  Undef = t_undefined(),
  
  case lookup(Var, Info) of
    Undef ->
      [{TrueLab, Info}, {FalseLab, Info}];
    VarInfo ->
      case hipe_icode:type_type(I) of
	cons ->
	  test_cons(Var, VarInfo, TrueLab, FalseLab, Info);
	nil ->
	  test_nil(Var, VarInfo, TrueLab, FalseLab, Info);
	{atom, A} ->
	  test_number_or_atom(fun(X)->erl_types:t_atom(X)end, 
			      fun(X)->erl_types:t_atom_vals(X)end,
			      A, Var, VarInfo, {atom, A}, 
			      TrueLab, FalseLab, Info);
	{integer, N} ->
	  test_number_or_atom(fun(X)->erl_types:t_number(X)end, 
			      fun(X)->erl_types:t_number_vals(X)end,
			      N, Var, VarInfo, {integer, N}, 
			      TrueLab, FalseLab, Info);
	Other ->
	  case t_is_any(VarInfo) of
	    true ->
	      TrueType = t_inf(true_branch_info(Other), VarInfo),
	      TrueInfo = enter(Var, TrueType, Info),
	      [{TrueLab, TrueInfo}, {FalseLab, Info}];
	    _ ->
	      case test_type(Other, VarInfo) of
		true ->
		  [{TrueLab, Info}];
		false ->
		  [{FalseLab, Info}];
		_ ->
		  TrueType = t_inf(true_branch_info(Other), VarInfo),
		  TrueInfo = enter(Var, TrueType, Info),
		  FalseType = t_subtract(VarInfo, TrueType),
		  FalseInfo = enter(Var, FalseType, Info),
		  [{TrueLab, TrueInfo}, {FalseLab, FalseInfo}]
	      end
	  end
      end
  end.

test_cons(Var, VarInfo, TrueLab, FalseLab, Info)->
  case t_is_cons_or_nil(VarInfo) of
    false ->
      case t_is_cons_or_nil(t_inf(VarInfo, t_cons_or_nil())) of
	true ->
	  [{TrueLab, enter(Var, t_cons(), Info)}, 
	   {FalseLab, enter(Var, t_subtract(VarInfo, t_cons()), Info)}];
	_ ->
	  [{FalseLab, Info}]
      end;
    _ ->
      case t_is_list(VarInfo) of
	false ->
	  case t_is_cons(VarInfo) of
	    true ->
	      [{TrueLab, Info}];
	    false ->
	      TrueType = t_inf(t_cons(), VarInfo),
	      FalseType = t_inf(t_list(), VarInfo),
	      [{TrueLab, enter(Var, TrueType, Info)},
	       {FalseLab, enter(Var, FalseType, Info)}]
	  end;
	_ ->
	  case t_is_nil(VarInfo) of
	    true->
	      [{FalseLab, Info}];		  
	    _ ->
	      TrueType = t_inf(t_nonempty_list(), VarInfo),
	      [{TrueLab, enter(Var, TrueType, Info)},
	       {FalseLab, enter(Var, t_nil(), Info)}]
	  end
      end
  end.

test_nil(Var, VarInfo, TrueLab, FalseLab, Info)->
  case t_is_cons_or_nil(VarInfo) of
    false ->
      case t_is_cons_or_nil(t_inf(VarInfo, t_cons_or_nil())) of
	true ->
	  [{TrueLab, enter(Var, t_nil(), Info)}, 
	   {FalseLab, enter(Var, t_subtract(VarInfo, t_nil()), Info)}];
	_ ->
	  [{FalseLab, Info}]
      end;
    _ ->
      case t_is_cons(VarInfo) of
	true ->
	  [{FalseLab, Info}];
	_ ->
	  FalseType = t_inf(t_nonempty_list(), VarInfo),
	  [{TrueLab, enter(Var, t_nil(), Info)},
	   {FalseLab, enter(Var, FalseType, Info)}]
      end
  end.

test_number_or_atom(Fun, FunVals, X, Var, VarInfo, TypeTest,
		    TrueLab, FalseLab, Info)->
  case t_is_any(VarInfo) of
    true ->
      [{TrueLab, enter(Var, Fun(X), Info)},
       {FalseLab, Info}];
    _ ->
      case test_type(TypeTest, VarInfo) of
	false ->
	  [{FalseLab, Info}];
	true ->
	  [{TrueLab, Info}];
	_ ->
	  Any = t_any(),
	  case FunVals(VarInfo) of
	    Any ->
	      [{TrueLab, enter(Var, Fun(X), Info)},
	       {FalseLab, Info}];
	    _ ->
	      FalseType = t_subtract(VarInfo, Fun(X)),
	      [{TrueLab, enter(Var, Fun(X), Info)},
	       {FalseLab, enter(Var, FalseType, Info)}]
	  end
      end
  end.
			  
test_type(Test, Type)->
  Undef = t_undefined(),
%  MeetType = t_inf(true_branch_info(Test), Type),
%  io:format("Test is: ~w\n", [Test]),
%  io:format("Type is: ~s\n", [t_to_string(Type)]),
%  io:format("Meet type is: ~s\n", [t_to_string(MeetType)]),
  case t_inf(true_branch_info(Test), Type) of
    Type ->
      test_type0(Test, Type);
    Undef ->
      false;
    Other ->
      case test_type0(Test, Other) of
	true ->
	  maybe;
	false ->
	  false;
	maybe ->
	  maybe
      end
  end.
  
test_type0(integer, T)->
  t_is_integer(T);
test_type0({integer, N}, T)->
  Any = t_any(),
  case t_is_integer(T) of
    true -> case t_number_vals(T) of
	      [N] -> true;
	      Any -> maybe;
	      List -> case lists:member(N, List) of
			true -> maybe;
			false -> false
		      end
	    end;
    _ -> false
  end;
test_type0(float, T) ->
  t_is_float(T);  
test_type0(number, T) ->
  t_is_number(T);
test_type0(atom, T) ->
  t_is_atom(T);
test_type0({atom, A}, T) ->
  Any = t_any(),
  case t_is_atom(T) of
    true -> case t_atom_vals(T) of
	      [A] -> true;
	      Any -> maybe;
	      List -> case lists:member(A, List) of
			true -> maybe;
			false -> false
		      end
	    end;
    _ -> false
  end;
test_type0(tuple, T) ->
  t_is_tuple(T);
test_type0({tuple, N}, T) ->
  case t_is_tuple(T) of
    true -> case t_tuple_arity(T) of
	      N -> true;
	      X when integer(X)-> false;
	      _ -> maybe
	    end;
    _ -> false
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
test_type0(list, T) ->
  t_is_cons_or_nil(T);
test_type0(cons, T) ->
  case t_is_cons_or_nil(T) of
    false -> false;
    _ -> case t_is_list(T) of
	   false -> case t_is_cons(T) of
		      true -> true;
		      _ -> maybe
		    end;
	   _ -> case t_is_nil(T) of
		  true-> false;
		  _ -> maybe
		end
	 end
  end;
test_type0(nil, T)->
  case t_is_nil(T) of
    true -> true;
    _ -> case t_is_list(T) of
	   true-> maybe;
	   _ -> case t_is_cons_or_nil(T) of
		  true -> maybe;
		  _ -> false
		end
	 end
  end;
%% CONS, NIL, and TUPLE are not constants, everything else is
test_type0(constant, T) ->
  case t_is_cons_or_nil(T) of
    true -> false;
    _ -> case t_is_tuple(T) of
	   true -> false;
	   _ -> true
	 end
  end;
test_type0(T, _) ->
  exit({unknown_typetest, T}).


true_branch_info(integer)->
  t_integer();
true_branch_info({integer, N})->
  t_integer(N);
true_branch_info(float) ->
  t_float();  
true_branch_info(number) ->
  t_number();
true_branch_info(atom) ->
  t_atom();
true_branch_info({atom, A}) ->
  t_atom(A);
true_branch_info(list)->
  t_cons_or_nil();
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
true_branch_info(nil)->
  t_nil();
true_branch_info(constant)->
  t_any();
true_branch_info(T) ->
  exit({unknown_typetest, T}).


%% _________________________________________________________________
%%
%% Remove the redundant type tests. If a test is removedthe trace
%% that isn't taken is explicitly removed from the cfg to simpilify
%% the handling of phi nodes. If a phi node is left and at least one
%% branch into it has disappeared the ssa propagation pass can't
%% handle it.
%%
%% If the cfg has changed at the end of this pass, the analysis is
%% done again since we might be able to find more information because
%% of the simplification of the cfg.
%%

remove_typetests([Label|Left], State, Dirty) when ?debug_test ->
  case state__bb(State, Label) of
    not_found ->
      remove_typetests(Left, State, Dirty);
    BB ->
      I = hipe_bb:last(BB),
      case hipe_icode:type(I) of
	type ->
	  Info = state__info_out(State, Label),
	  [Var] = uses(I),
	  VarInfo = lookup(Var, Info),
	  Any = t_any(),
	  case VarInfo of
	    Any ->
	      remove_typetests(Left, State, Dirty);
	    _ ->	 
	      case test_type(hipe_icode:type_type(I), VarInfo) of
		maybe ->
		  remove_typetests(Left, State, Dirty);
		Res ->
		  {Taken, NotTaken} = 
		    case Res of
		      true -> 
			{hipe_icode:type_true_label(I),
			 hipe_icode:type_false_label(I)};
		      _ -> 
			{hipe_icode:type_false_label(I),
			 hipe_icode:type_true_label(I)}
		    end,
		  case Taken of
		    NotTaken ->
		      %% true label = false label, this can occur!
		      NewState = mk_goto(State, BB, Label, Taken),
		      remove_typetests(Left, NewState, dirty);
		    _ ->
		      %% Insert a fail block.
		      Cfg = state__cfg(State),
		      {LLo,LHi} = hipe_icode_cfg:label_range(Cfg),
		      FailLab = LHi+1,
		      NewCfg1 = 
			hipe_icode_cfg:label_range_update(Cfg, {LLo, FailLab}),
		      {VLo,VHi} = hipe_icode_cfg:var_range(NewCfg1),
		      V = hipe_icode:mk_var(VHi+1),
		      NewCfg2 = 
			hipe_icode_cfg:var_range_update(NewCfg1,{VLo, VHi+1}),
		      FailAtom = list_to_atom("type_test_failed in "++
					      integer_to_list(Label)),
		      Reason = hipe_icode:mk_const(FailAtom),
		      FailCode = [hipe_icode:mk_mov(V, Reason),
				  hipe_icode:mk_fail([V], fault)],
		      FailBB = hipe_bb:mk_bb(FailCode),
		      NewCfg3 = 
			hipe_icode_cfg:bb_add(NewCfg2, FailLab, FailBB),
		      NewState = state__cfg_update(State, NewCfg3),
		      
		      %% Redirect the typetest.
		      NewI = hipe_icode:redirect_jmp(I, NotTaken, FailLab),
		      NewBB = hipe_bb:code_update(BB, hipe_bb:butlast(BB)++[NewI]),
		      NewState2 = state__bb_update(NewState, Label, NewBB),
		      remove_typetests(Left, NewState2, dirty)
		  end
	      end
	  end;
	_ ->
	  remove_typetests(Left, State, Dirty)
      end
  end;
remove_typetests([Label|Left], State, Dirty)->
  case state__bb(State, Label) of
    not_found ->
      remove_typetests(Left, State, Dirty);
    BB ->
      I = hipe_bb:last(BB),
      case hipe_icode:type(I) of
	type ->
	  Info = state__info_out(State, Label),
	  [Var] = uses(I),
	  VarInfo = lookup(Var, Info),
	  Any = t_any(),
	  case VarInfo of
	    Any ->
	      remove_typetests(Left, State, Dirty);
	    _ ->	 
	      FalseLab = hipe_icode:type_false_label(I),
	      case hipe_icode:type_true_label(I) of
		FalseLab ->
		  %% true label = false label, this can occur!
		  NewState = mk_goto(State, BB, Label, FalseLab),
		  remove_typetests(Left, NewState, dirty);
		TrueLab ->
		  case test_type(hipe_icode:type_type(I), VarInfo) of
		    true -> 
		      NewState = mk_goto(State, BB, Label, TrueLab),
		      remove_typetests(Left, NewState, dirty);
		    false ->
		      NewState = mk_goto(State, BB, Label, FalseLab),
		      remove_typetests(Left, NewState, dirty);
		    _ ->
		      remove_typetests(Left, State, Dirty)
		  end
	      end
	  end;
	_ ->
	  remove_typetests(Left, State, Dirty)
      end
  end;
remove_typetests([], State, dirty) ->
  %% Redo the type analysis since the cfg has changed.
  %% If debug_test is true this might lead to an infinite loop.
  if ?debug_test -> State;
     true ->   {dirty, State}
  end;
remove_typetests([], State, _) ->
  State.

mk_goto(State, BB, Label, Succ)->
  %%io:format("Removing a typetest\n", []),
  NewI = hipe_icode:mk_goto(Succ),
  NewBB = hipe_bb:code_update(BB, hipe_bb:butlast(BB)++[NewI]),
  state__bb_update(State, Label, NewBB).

%% _________________________________________________________________
%%
%% Annotate the variables with the local information. Since we have
%% SSA form and the type information can only depend on assignment or
%% branches (type tests), we can use the information out of the block
%% to annotate all variables in it.
%%
%% The code is only annotated if the code is to be prettyprinted. 


annotate(State, IcodeFun, Options)->
  Cfg = state__cfg(State),
  Labels = hipe_icode_cfg:labels(Cfg),
  NewState = fold(fun make_transformations/2, Labels, State),
  case proplists:get_value(pp_typed_icode, Options) of
    true ->
      PPState = fold(fun annotate_instr/2, Labels, NewState),
      hipe_icode_cfg:pp(state__cfg(PPState));
    {only,Lst} ->
      case lists:member(IcodeFun,Lst) of
	true ->
	  PPState = fold(fun annotate_instr/2, Labels, NewState),
	  hipe_icode_cfg:pp(state__cfg(PPState));
	false ->
	  ok
      end;
    {file,FileName} ->
      {ok,File} = file:open(FileName,[write,append]),
      PPState = fold(fun annotate_instr/2, Labels, NewState),
      hipe_icode_cfg:pp(File, state__cfg(PPState));
    _ ->
      ok
  end,
  NewState.


fold(Fun, [Label|Left], State)->
  Info = state__info_out(State, Label),
  case gb_trees:is_empty(Info) of
    true ->
      fold(Fun, Left, State);
    _ ->
      BB = state__bb(State, Label),
      Code = hipe_bb:code(BB),
      Info = state__info_out(State, Label),
      NewCode = lists:map(fun(X)->Fun(X, Info)end, Code),
      NewBB = hipe_bb:code_update(BB, NewCode),
      NewState = state__bb_update(State, Label, NewBB),
      fold(Fun, Left, NewState)
  end;
fold(_Fun, [], State) ->
  State.

annotate_instr(I, Info)->
  Def = defines(I),
  Undefined = t_undefined(),
  Vars = Def++uses(I),
  Fun = fun(X, Y)->hipe_icode:add_type_to_var(X, Y)end,
  Subst = [{X, Fun(X, Y)}||X<-Vars, (Y=lookup(X, Info))/=Undefined],
  case  Subst of
    [] ->
      I;
    _ ->
      NewI = hipe_icode:subst(Subst, I),
%      io:format("Changed the instruction:\n", []),
%      hipe_icode_pp:pp_instrs(standard_io, [I]),
%      io:format("To the instruction:\n", []),
%      hipe_icode_pp:pp_instrs(standard_io, [NewI]),
      NewI
  end.

make_transformations(I, Info)->
  Fun =
    case hipe_icode:type(I) of      
      call ->
	hipe_icode:call_fun(I);
%% NYI
%      enter ->
%	hipe_icode:enter_fun(I);
      _ ->
	[]
    end,
  NewI =
    case Fun of
      {erlang, element, 2} -> 
	case hipe_icode:call_in_guard(I) of
	  true -> I;
	  _ -> transform_element2(I, Info)
	end;
      {erlang, hd, 1} -> transform_hd_or_tl(I, unsafe_hd, Info);
      {erlang, tl, 1} -> transform_hd_or_tl(I, unsafe_tl, Info);
      
%      {erlang, What, Arity} ->
%	io:format("{erlang, ~w, ~w}\n", [What, Arity]),
%	I;
      _ ->
	I
    end,
%  case NewI of
%    I ->
%      ok;
%    _ ->
%      io:format("Changed the instruction:\n", []),
%      hipe_icode_pp:pp_instrs(standard_io, [I]),
%      io:format("To the instruction:\n", []),
%      hipe_icode_pp:pp_instrs(standard_io, [NewI])
%  end,
  NewI.

transform_element2(I, Info)->
  %%Any = t_any(),
  [Index, Tuple] = uses(I),
  IndexType = lookup(Index, Info),
  TupleType = lookup(Tuple, Info),
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
	  Cont = hipe_icode:call_continuation(I),
	  Dst = defines(I),
	  case N of
	    [Num] ->
	      hipe_icode:mk_primop(Dst, {unsafe_element, Num}, 
				   [Tuple], Cont, []);
	    _ ->
	      hipe_icode:call_fun_update(I, {erlang, element, 2,
					       [{tuple, A}, valid]})
	  end;
	_ ->
	  case lists:all(fun(X)->hipe_tagscheme:is_fixnum(X) end, N) of
	    true ->
	      hipe_icode:call_fun_update(I, {erlang, element, 2, 
					     [{tuple, A}, fixnums]});
	    _ ->
	      hipe_icode:call_fun_update(I, {erlang, element, 2, 
					     [{tuple, A}, []]})
	  end
      end;
    _ ->
      hipe_icode:call_fun_update(I, {erlang, element, 2, [NewTuple, NewIndex]})
  end.

transform_hd_or_tl(I, Primop, Info)->
  [Arg] = hipe_icode:call_args(I),
  case t_is_cons(lookup(Arg, Info)) of
    true->
      Dst = defines(I),
      Cont = hipe_icode:call_continuation(I),
      hipe_icode:mk_primop(Dst, Primop, [Arg], Cont, []);
    _ ->
      I
  end.


%% _________________________________________________________________
%%
%% Various help functions.
%%

add_arg_types(Args,Types)->
  add_arg_types(Args, Types, empty()).

add_arg_types([Arg|Args],[Type|Types], Acc)->
  add_arg_types(Args,Types, enter(Arg, Type, Acc));
add_arg_types([],[],Acc) ->
  Acc;
add_arg_types(A,B,_) ->
  exit({wrong_number_of_arguments, {A, B}}).

lookup(Var, Tree)->
  case gb_trees:lookup(Var, Tree) of
    none ->
      t_undefined();
    {value, Val} ->
      case hipe_icode:is_var(Val) of
	true ->
	  lookup(Val, Tree);
	_ ->
	  Val
      end
  end.

lookup_list(List, Info)->
  lookup_list0(List, Info, []).

lookup_list0([H|T], Info, Acc)->
  lookup_list0(T, Info, [lookup(H, Info)|Acc]);
lookup_list0([], _, Acc) ->
  lists:reverse(Acc).

enter([Key], Value, Tree)->
  enter(Key, Value, Tree);
enter(Key, Value, Tree)->
  case t_is_undefined(Value) of
    true ->
      Tree;
    _ ->
      enter_to_leaf(Key, Value, Tree)
  end.

enter_to_leaf(Key, Value, Tree)->
  case gb_trees:lookup(Key, Tree) of
    {value, Value} ->
      Tree;
    {value, Val} ->
      case hipe_icode:is_var(Val) of
	true->
	  enter_to_leaf(Val, Value, Tree);
	_ ->
	  gb_trees:enter(Key, Value, Tree)
      end;
    _ ->
      gb_trees:insert(Key, Value, Tree)
  end.

empty()->
  gb_trees:empty().

join_list(List, Info)->
  join_list(List, Info, t_undefined()).
 
join_list([H|T], Info, Acc)->
  Type = t_sup(lookup(H, Info), Acc),
  join_list(T, Info, Type);
join_list([], _, Acc) ->
  Acc.

join_info_in([{Var, Type}|Tail], InfoIn)->
  case gb_trees:lookup(Var, InfoIn) of
    none ->
      join_info_in(Tail, enter(Var, Type, InfoIn));
    {value, Type} ->
      join_info_in(Tail, InfoIn);
    {value, OldType}->
      join_info_in(Tail, enter(Var, t_sup(OldType, Type), InfoIn))
  end;
join_info_in([], Acc) ->
  Acc.

const_type(Var)->
  case hipe_icode:is_const(Var) of
    false ->
      not_a_constant;
    true ->
      t_from_term(hipe_icode:const_value(Var))
  end.

do_updates(State, List)->
  do_updates(State, List, []).

do_updates(State, [{Label, Info}|Tail], Worklist)->
  case state__info_in_update(State, Label, Info) of
    fixpoint ->
      do_updates(State, Tail, Worklist);
    NewState ->
      do_updates(NewState, Tail, [Label|Worklist])
  end;
do_updates(State, [], Worklist) ->
  {State, Worklist}.

enter_defines(I, Types, Info)when is_list(Types)->
  case defines(I) of
    []-> Info;
    Def->
      {NewInfo, _} =
	lists:foldl(fun(X, {Info, [Type|Tail]})->{enter(X,Type,Info), Tail}end,
		    {Info, Types}, Def),
      NewInfo
  end;
enter_defines(I, Type, Info)->
  case defines(I) of
    []-> Info;
    Def->
      lists:foldl(fun(X, Acc)->enter(X, Type, Acc)end, Info, Def)
  end.

defines(I)->
  keep_vars(hipe_icode:defines(I)).

uses(I)->
  keep_vars(hipe_icode:uses(I)).

keep_vars(Vars)->
  lists:filter(fun(X)->hipe_icode:is_var(X)end, Vars).

      

%% _________________________________________________________________
%%
%% Handling the state
%%

-record(state, {succmap, info_map, cfg}).

new_state(Cfg)->
  Succ = hipe_icode_cfg:succ_map(Cfg),
  Start = hipe_icode_cfg:start(Cfg),  
  Info = case lists:keysearch(arg_type, 1, hipe_icode_cfg:info(Cfg)) of
	   false ->
	     Any = t_any(),
	     lists:foldl(fun(X, Tree)->gb_trees:insert(X, Any, Tree)end,
			 empty(), hipe_icode_cfg:params(Cfg));
	   {value,{_, ArgType}}->
	     add_arg_types(hipe_icode_cfg:params(Cfg), ArgType)
	 end,
  InfoMap = gb_trees:insert({Start, in}, Info, empty()),
  #state{succmap=Succ, info_map=InfoMap, cfg=Cfg}.

state__cfg(#state{cfg=Cfg})->
  Cfg.

state__cfg_update(S, Cfg)->
  S#state{cfg=Cfg}.

state__succ(#state{succmap=SM}, Label)->
  hipe_icode_cfg:succ(SM, Label).

state__bb(#state{cfg=Cfg}, Label)->
  hipe_icode_cfg:bb(Cfg, Label).
  
state__bb_update(S=#state{cfg=Cfg}, Label, BB)->
  NewCfg = hipe_icode_cfg:bb_update(Cfg, Label, BB),
  S#state{cfg=NewCfg}.

state__info_in(S, Label)->
  state__info(S, {Label, in}).

state__info_out(S, Label)->
  state__info(S, {Label, out}).

state__info(#state{info_map=IM}, Label)->
  case gb_trees:lookup(Label, IM) of
    {value, Info}-> Info;
    _ -> gb_trees:empty()
  end.

state__info_map(#state{info_map=IM})->
  IM.

state__info_in_update(S=#state{info_map=IM}, Label, Info)->
  case gb_trees:lookup({Label, in}, IM) of
    none ->
      S#state{info_map=gb_trees:enter({Label, in}, Info, IM)};
    {value, Info} ->
      fixpoint;
    {value, OldInfo} ->
      NewInfo = join_info_in(gb_trees:to_list(OldInfo), Info),
      S#state{info_map=gb_trees:enter({Label, in}, NewInfo, IM)}
  end.

state__info_out_update(S=#state{info_map=IM}, Label, Info)->
  S#state{info_map=gb_trees:enter({Label, out}, Info, IM)}.
