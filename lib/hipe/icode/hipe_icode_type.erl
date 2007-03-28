%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : hipe_icode_type.erl
%%% Author  : Tobias Lindahl <Tobias.Lindahl@it.uu.se>
%%% Description : Propagate type information.
%%%
%%% Created : 25 Feb 2003 by Tobias Lindahl <Tobias.Lindahl@it.uu.se>
%%%
%%% CVS :
%%%     $Id$
%%%-------------------------------------------------------------------

-module(hipe_icode_type).

-export([cfg/3, unannotate_cfg/1]).

-include("hipe_icode.hrl").
-include("hipe_icode_primops.hrl").
-include("hipe_icode_type.hrl").

-define(DO_HIPE_ICODE_TYPE_TEST, false).

-ifdef(DO_HIPE_ICODE_TYPE_TEST).
-export([test/0]).
-endif.


%% -define(BITS, (hipe_rtl_arch:word_size() * 8) - ?TAG_IMMED1_SIZE).
%% -define(TAG_IMMED1_SIZE, 4).

%% -define(MFA_debug, fun(MFA, X, Y) -> 
%%  		       if MFA =:= {pseudoknot,p_apply,3} ->
%%  			   io:format("~s ~p~n", [X, Y]);
%%  			  true  ->
%%  			   ok
%%  		       end
%%  		   end).
-define(MFA_debug, fun(_, _, _) -> ok end).

%-define(debug, fun(X, Y) -> io:format("~s ~p~n", [X, Y]) end).
-define(debug, fun(_, _) -> ok end).

%-define(flow_debug, fun(X, Y) -> io:format("flow: ~s ~p~n", [X, Y]) end).
-define(flow_debug, fun(_, _) -> ok end).

%-define(widening_debug, fun(X, Y) -> io:format("wid: ~s ~p~n", [X, Y]) end).
-define(widening_debug, fun(_, _) -> ok end).

%-define(call_debug, fun(X, Y) -> io:format("call: ~s ~p~n", [X, Y]) end).
-define(call_debug, fun(_, _) -> ok end).

%-define(ineq_debug, fun(X, Y) -> io:format("ineq: ~s ~p~n", [X, Y]) end).
-define(ineq_debug, fun(_, _) -> ok end).

%-define(server_debug, fun(X, Y) -> io:format("~p server: ~s ~p~n", [self(), X, Y]) end).
-define(server_debug, fun(_, _) -> ok end).

-import(erl_types, [t_any/0, t_atom/1, t_atom/0, t_atom_vals/1,
		    t_binary/0, t_bool/0, t_cons/0, t_constant/0,
		    t_pos_improper_list/0,
		    t_float/0, t_from_term/1, t_fun/0, t_fun/1, t_fun/2,
		    t_fun_args/1, t_fun_arity/1, 
		    t_fun_range/1,t_inf/2, t_inf_lists/2, 
		    t_integer/0,
		    t_integer/1, t_is_atom/1, t_is_any/1, t_is_binary/1,
		    t_is_bool/1, t_is_fixnum/1, t_is_cons/1, t_is_constant/1,
		    t_is_pos_improper_list/1, t_is_equal/2, t_is_float/1,
		    t_is_fun/1, t_is_integer/1, t_is_number/1,
		    t_is_list/1, t_is_nil/1, t_is_port/1, t_is_pid/1,
		    t_is_ref/1, t_is_subtype/2, 
		    t_is_tuple/1,
		    t_is_none/1, t_limit/2, t_list/0, t_nil/0,
		    t_number/0, t_number/1, t_number_vals/1, t_pid/0,
		    t_port/0, t_ref/0, t_subtract/2, t_sup/2,
		    t_to_string/1, t_tuple/0, t_tuple/1,
		    %%t_tuple_arity/1, 
		    t_tuple_arities/1, t_none/0,
		    number_min/1, number_max/1, t_from_range/2, 
		    min/2, max/2, widening/3, t_is_bitwidth/1]).


cfg(Cfg, IcodeFun, Options) ->
  ?flow_debug("starting", {IcodeFun, self()}),
  OldSig = init_mfa_info(IcodeFun, Options),
  Exports = proplists:get_value(exports, Options),
  {_, F, A} = IcodeFun,
  Is_exported = lists:member({F,A}, Exports),
  Is_closure = hipe_icode_cfg:is_closure(Cfg),
  case server__has_needed_info(IcodeFun, Is_exported, Is_closure) of
    true ->
      State = analyse(Cfg, {IcodeFun, {Is_exported, OldSig}}),
      pp(State, IcodeFun, Options, "Pre-specialization"),
      NewState = cfg_loop(State, {IcodeFun, {Is_exported, OldSig}}),  
      pp(NewState, IcodeFun, Options, "Post-specialization"),
      warn_on_type_errors(Cfg, State, NewState, IcodeFun, Options),
      Fixpoint = update_mfa_info(NewState, IcodeFun, OldSig, Options),
      ?flow_debug("done", {IcodeFun, Fixpoint}),
      {Fixpoint, state__cfg(annotate_cfg(NewState))};
    false ->
      ?flow_debug("dosen't have needed info", IcodeFun),
      {none, Cfg}
  end.

cfg_loop(State, {IcodeFun, {Is_exported, OldSig}}) ->
  NewState0 = specialize(State),
  Labels = hipe_icode_cfg:reverse_postorder(state__cfg(NewState0)),
  case simplify_controlflow(Labels, NewState0, false) of
    {dirty, Cfg} ->
      ?flow_debug("restarting analysis", IcodeFun),
      NewCfg = hipe_icode_cfg:remove_unreachable_code(Cfg),
      NewState1 = analyse(NewCfg, {IcodeFun, {Is_exported, OldSig}}),
      cfg_loop(NewState1, {IcodeFun, {Is_exported, OldSig}});
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

analyse(Cfg, {IcodeFun, {Is_exported, OldSig}}) ->
  %%hipe_icode_cfg:pp(Cfg),
  State = new_state(Cfg, {IcodeFun, {Is_exported, OldSig}}),
  analyse_blocks(State).

analyse_blocks(State) ->
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
  
analyse_block(Label, InfoIn, State) ->
  %%io:format("Handling ~w\n", [Label]),
  BB = state__bb(State, Label),
  Code = hipe_bb:butlast(BB),
  Last = hipe_bb:last(BB),
  PhiNodes = state__phi_nodes(State),
  {InfoOut, NewPhiNodes} = analyse_insns(Code, {InfoIn, PhiNodes}),
  NewState = state__info_out_update(
	       state__phi_nodes_update(State, NewPhiNodes),
	       Label, InfoOut),
%%  InfoOut = analyse_insns(Code, InfoIn),
%%  NewState = state__info_out_update(State, Label, InfoOut),
  
  case Last of
    #'if'{} ->
      UpdateInfo = do_if(Last, InfoOut),
      %%io:format("Update info for ~w:\n~w", [Label, UpdateInfo]),
      do_updates(NewState, UpdateInfo);
    #type{} ->
      UpdateInfo = do_type(Last, InfoOut),
      %%io:format("Update info for ~w:\n", [Label]),
      %%[io:format("~w: ~p\n", [X, gb_trees:to_list(Y)])||{X, Y} <- UpdateInfo],
      do_updates(NewState, UpdateInfo);
    #switch_tuple_arity{} ->
      UpdateInfo = do_switch_tuple_arity(Last, InfoOut),
      %%io:format("Update info for ~w:\n", [Label]),
      %%[io:format("~w: ~p\n", [X, gb_trees:to_list(Y)])||{X, Y} <- UpdateInfo],
      do_updates(NewState, UpdateInfo);
    #switch_val{} ->
      UpdateInfo = do_switch_val(Last, InfoOut),
      %%io:format("Update info for ~w:\n~w", [Label, UpdateInfo]),
      do_updates(NewState, UpdateInfo);
    #enter{} ->
      update_enter_call_args(Last, InfoOut),
      %% UpdateInfo realy needed??
      UpdateInfo = [{X, InfoOut} || X <- state__succ(NewState, Label)],
      %%io:format("Update info for ~w:\n~w", [Label, UpdateInfo]),
      do_updates(NewState, UpdateInfo);
    #call{} ->
      NewInfoOut = do_call(Last, InfoOut),
      NewState1 = state__info_out_update(NewState, Label, NewInfoOut),
      ContInfo = update_call_arguments(Last, NewInfoOut),      
      Cont = hipe_icode:call_continuation(Last),
      Fail = hipe_icode:call_fail_label(Last),
      ?call_debug("Continfo, NewInfoOut", {ContInfo, NewInfoOut}),
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
      UpdateInfo = [{X, InfoOut} || X <- state__succ(NewState, Label)],
      %%io:format("Update info for ~w:\n~w", [Label, UpdateInfo]),
      do_updates(NewState, UpdateInfo)
  end.

analyse_insns([I|Insns], {Info, PhiNodes}) ->
  case analyse_insn(I, Info, PhiNodes) of
    {new_phi, NewInfo, NewPhiNodes} ->
      analyse_insns(Insns, {NewInfo, NewPhiNodes});
    NewInfo ->
      analyse_insns(Insns, {NewInfo, PhiNodes})
  end;
analyse_insns([], {Info, PhiNodes}) ->
  {Info, PhiNodes}.

analyse_insn(I, Info) ->
  case analyse_insn(I, Info, gb_trees:empty()) of
    {new_phi, NewInfo, _NewPhiNodes} ->
      NewInfo;
    NewInfo ->
      NewInfo
  end.

analyse_insn(I, Info, PhiNodes) ->
  case I of
    #move{} ->
      do_move(I, Info);
    #call{} ->
      A = do_call(I, Info),
      update_call_arguments(I, A);
    #phi{} ->
      Type = t_limit(join_list(args(I), Info), ?TYPE_DEPTH),
      {WidendType, NewPhiNodes} = 
	phi_widening(Type, PhiNodes, hipe_icode:phi_dst(I)),
      {new_phi, enter_defines(I, WidendType, Info), NewPhiNodes};
    #begin_handler{} ->
      enter_defines(I, t_any(), Info);
    _ ->
      %% Just an assert
      case defines(I) of
	[] -> Info;
	_ -> exit({"Instruction with destination not analysed", I})
      end
  end.

phi_widening(Type, PhiNodes, Key) ->
  {Version, OldType} = phi_lookup(Key, PhiNodes),
  WidenedType = widening(Type, OldType, Version),
  ?widening_debug("Key, Version, Old, Arg and Ans to phi", 
		  {Key, Version, OldType, Type, WidenedType}),
  %%io:format("Phi widenedTYpe ~p ~n", [WidenedType]),
  New_phi_tree = 
  case t_is_equal(WidenedType, OldType) of
    true ->
      PhiNodes;
    false ->
      gb_trees:enter(Key, {Version +1, WidenedType}, PhiNodes)
  end,
  ?widening_debug("diff", 
		  gb_trees:to_list(New_phi_tree) -- 
		  gb_trees:to_list(PhiNodes)),
  {WidenedType, New_phi_tree}.

phi_lookup(Var, Tree) ->
  case gb_trees:lookup(Var, Tree) of
    none ->
      {0, t_none()};
    {value, Val} ->
      Val
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
      %PltType = find_plt_return(MFA, ArgTypes),
      {PltType, _} = server__get_return_type(MFA),
      %%Type = PltType,
      ?call_debug("remote call MFA, Args", {MFA, args(I)}),
      ?call_debug("Args", ArgTypes),

      BifType = erl_bif_types:type(M, F, A, ArgTypes),

      Type = t_inf(BifType, PltType),
      ?call_debug("t_inf with", {PltType, BifType, Type}),
      %%      io:format("The result of the call to ~p is ~s\n", 
      %%      		[hipe_icode:call_fun(I), format_type(Type)]),
      %%      io:format("Argtypes: ~p\n", [[format_type(X) || X <- ArgTypes]]),
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
      ?call_debug("local call", MFA),
      server__update_call_args(MFA, lookup_list(args(I), Info)),
      %%      io:format("The result of the call to ~w is ~s\n", 
      %%		[hipe_icode:call_fun(I), format_type(Type)]),
      %%      io:format("Annotated type: ~s\n", 
      %%		[format_type(AnnotatedType)]),
      %%      io:format("Plt type: ~s\n", 
      %%		[format_type(PltType)]),
      enter_defines(I, Type, Info)
  end.

update_enter_call_args(I, Info) ->
  case hipe_icode:enter_type(I) of
    local ->
      MFA = hipe_icode:enter_fun(I),
      server__update_call_args(MFA, lookup_list(args(I), Info));
    remote ->
      ok;
    primop ->
      ok
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
	    '==' ->
	      [{TrueLab, Info}, {FalseLab, Info}];
	    '/=' ->
	      [{TrueLab, Info}, {FalseLab, Info}];
	    Op ->
	      integer_range_inequality_propagation(Op, Arg1, Arg2,
						   TrueLab, FalseLab, Info)
	      %%_ ->
	      %%  [{TrueLab, Info}, {FalseLab, Info}]
	  end
      end;
    _ ->
      %% Only care for binary if:s
      [{TrueLab, Info}, {FalseLab, Info}]
  end.

integer_range_inequality_propagation(Op, A1, A2, TrueLab, FalseLab, Info) ->
  Arg1 = lookup(A1, Info), 
  Arg2 = lookup(A2, Info), 
  ?ineq_debug("args", [Arg1,Arg2]),
  IntArg1 = t_inf(Arg1, t_integer()),
  IntArg2 = t_inf(Arg2, t_integer()),
  NonIntArg1 = t_subtract(Arg1, t_integer()),
  NonIntArg2 = t_subtract(Arg2, t_integer()),
  ?ineq_debug("nonintargs", [NonIntArg1,NonIntArg2]),
  case t_is_none(IntArg1) or t_is_none(IntArg2) of
    true ->
      ?ineq_debug("one is none", [IntArg1,IntArg2]),
      [{TrueLab, Info}, {FalseLab, Info}];
    false ->
      case Op of
	'>=' ->
 	  {FalseArg1, FalseArg2, TrueArg1, TrueArg2} =
 	    integer_range_less_then_propagator(IntArg1, IntArg2);
 	'>' ->
 	  {TrueArg2, TrueArg1, FalseArg2, FalseArg1} =
 	    integer_range_less_then_propagator(IntArg2, IntArg1);
 	'<' ->
 	  {TrueArg1, TrueArg2, FalseArg1, FalseArg2} =
 	    integer_range_less_then_propagator(IntArg1, IntArg2);
 	'=<' ->
 	  {FalseArg2, FalseArg1, TrueArg2, TrueArg1} =
 	    integer_range_less_then_propagator(IntArg2, IntArg1)
	end,
      ?ineq_debug("int res", [TrueArg1,TrueArg2, FalseArg1, FalseArg2]),
      False = {FalseLab, enter(A1, t_sup(FalseArg1, NonIntArg1),
			       enter(A2, t_sup(FalseArg2, NonIntArg2), Info))},
      True = {TrueLab, enter(A1, t_sup(TrueArg1, NonIntArg1),
			     enter(A2, t_sup(TrueArg2, NonIntArg2), Info))},
      [True, False]
  end.


integer_range_less_then_propagator(IntArg1, IntArg2) ->
  Min1 = number_min(IntArg1),
  Max1 = number_max(IntArg1),
  Min2 = number_min(IntArg2),
  Max2 = number_max(IntArg2),
  %% is this the same as erl_types:t_subtract?? no ... ??
  TrueMax1 = min(Max1, erl_bif_types:infinity_add(Max2, -1)),
  TrueMin2 = max(erl_bif_types:infinity_add(Min1, 1), Min2),
  FalseMin1 = max(Min1, Min2),
  FalseMax2 = min(Max1, Max2),
  {t_from_range(Min1, TrueMax1),
   t_from_range(TrueMin2, Max2),
   t_from_range(FalseMin1, Max1),
   t_from_range(Min2, FalseMax2)}.

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
	  TrueInfo = enter_list([FunVar, ArityVar], 
				[GenTrueFun, GenTrueArity], Info),
	  [{TrueLab, TrueInfo}, {FalseLab, Info}];
	{any, Arity} when is_integer(Arity) ->
	  TrueInfo = enter_list([FunVar, ArityVar], 
				[GenTrueFun, t_integer(Arity)], Info),
	  [{TrueLab, TrueInfo}, {FalseLab, Info}];
	{[Val], any} when is_integer(Val) ->
	  TrueInfo = enter_list([FunVar, ArityVar], 
				[t_inf(GenTrueFun, t_fun(Val, t_any())),
				 GenTrueArity], Info),
	  [{TrueLab, TrueInfo}, {FalseLab, Info}];
	{Vals, any} when is_list(Vals) ->
	  %% The function type gets widened when we have more than one arity.
	  TrueInfo = enter_list([FunVar, ArityVar], 
				[GenTrueFun, GenTrueArity], Info),
	  [{TrueLab, TrueInfo}, {FalseLab, Info}];
	{Vals, Arity} when is_list(Vals), is_integer(Arity) ->
	  case lists:member(Arity, Vals) of
	    false ->
	      [{FalseLab, Info}];
	    true ->
	      TrueInfo = enter_list([FunVar, ArityVar], 
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
	  case FunVals(VarInfo) =:= any of
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

test_type(Test, Type) ->
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
  t_is_pos_improper_list(T);
test_type0(cons, T) ->
  t_is_cons(T);
test_type0(nil, T)->
  t_is_nil(T);
test_type0(constant, T) ->
  t_is_constant(T);
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
  t_pos_improper_list();
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

simplify_controlflow([Label|Left], State, Dirty) ->
  case state__bb(State, Label) of
    not_found ->
      simplify_controlflow(Left, State, Dirty);
    BB ->
      I = hipe_bb:last(BB),
      case I of
	#'if'{} ->
	  Info = state__info_out(State, Label),
	  case do_if(I, Info) of
	    [{Lab, _}] ->
	      NewState = mk_goto(State, BB, Label, Lab),
	      simplify_controlflow(Left, NewState, true);
	    [_,_] ->
	      simplify_controlflow(Left, State, Dirty)
	  end;
	#type{} ->
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
	#switch_tuple_arity{} ->
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
	#switch_val{} ->
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
	#call{} ->
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

mk_goto(State, BB, Label, Succ) ->
  NewI = hipe_icode:mk_goto(Succ),
  NewBB = hipe_bb:code_update(BB, hipe_bb:butlast(BB)++[NewI]),
  state__bb_add(State, Label, NewBB).

unset_fail(State, BB, Label, I) ->
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

specialize(State) ->
  Cfg = state__cfg(State),
  Labels = hipe_icode_cfg:labels(Cfg),
  transform_bbs(Labels, State).

transform_bbs([Label|Left], State) ->
  BB = state__bb(State, Label),
  Code = hipe_bb:code(BB),
  Info = state__info_in(State, Label),
  NewCode = make_transformations(Code, Info, []),
  NewBB = hipe_bb:code_update(BB, NewCode),
  NewState = state__bb_add(State, Label, NewBB),
  transform_bbs(Left, NewState);
transform_bbs([], State) ->
  State.

make_transformations([I|Left], Info, Acc) ->
  NewInfo = analyse_insn(I, Info),
  NewI = transform_insn(I, Info),
  make_transformations(Left, NewInfo, [NewI|Acc]);
make_transformations([], _Info, Acc) ->
  lists:reverse(Acc).

transform_insn(I, Info) ->
  case I of
    I when is_record(I, call) orelse is_record(I, enter) ->
      case call_or_enter_fun(I) of
	'band' -> transform_arith(I, 'band', Info);
	'bor'  -> transform_arith(I, 'bor', Info);
	'bxor' -> transform_arith(I, 'bxor', Info);
	'bnot' -> transform_arith(I, 'bnot', Info);
	'bsl'  -> transform_arith(I, 'bsl', Info);
	'bsr'  -> transform_arith(I, 'bsr', Info);
	'+'    -> transform_arith(I, '+', Info);
	'-'    -> transform_arith(I, '-', Info);
	#element{} ->
	  transform_insn(update_call_or_enter(I, {erlang,element,2}), Info);
	{erlang,element,2} -> 
	  NewI1 = transform_element2(I, Info),
	  case is_record(I, call) andalso hipe_icode:call_in_guard(I) of
	    true ->
	      case hipe_icode:call_fun(NewI1) of
		#unsafe_element{} -> NewI1;
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
    #'if'{} ->
      UsesFixnums = all_fixnums(uses(I)),
      if UsesFixnums ->
	  CurrentIfOp = hipe_icode:if_op(I),
	  hipe_icode:if_op_update(I, fixnum_ifop(CurrentIfOp));
	 true ->
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

transform_element2(I, Info) ->
  [Index, Tuple] = args(I),
  IndexType = safe_lookup(Index, Info),
  TupleType = safe_lookup(Tuple, Info),
  ?debug("Tuple", TupleType),
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
	?debug("is tuple", TupleType),
	Arities = t_tuple_arities(TupleType),
	Any = t_any(),
	if Arities =:= Any ->
	    Arities;
	   true ->
	    lists:min(Arities)
	end;
%% 	case t_tuple_arity(TupleType) of
%% 	  Arity when is_number(Arity) -> {tuple, Arity};
%% 	  _ -> tuple
%% 	end;
      _ -> [] %% Might fail - don't care.
    end,
  case {NewTuple, NewIndex} of
    {{tuple, A}, {number, N}} ->
      case lists:all(fun(X) -> A>=X andalso X>0 end, N) of
	true -> 
	  case N of
	    [Num] ->
	      [_, Tuple] = args(I),
	      update_call_or_enter(I, #unsafe_element{index=Num}, [Tuple]);
	    _ ->
	      NewFun = {element, [{tuple, A}, valid]},
	      update_call_or_enter(I, NewFun)
	  end;
	false ->
	  case lists:all(fun(X) -> hipe_tagscheme:is_fixnum(X) end, N) of
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

transform_hd_or_tl(I, Primop, Info) ->
  [Arg] = args(I),
  case t_is_cons(safe_lookup(Arg, Info)) of
    true->
      update_call_or_enter(I, Primop);
    false ->
      I
  end.

transform_bs_put_integer(I, Primop, Info) -> 
  [Src|_] = safe_lookup_list(args(I), Info),
  case all_fixnums([Src]) of
    true ->
      update_call_or_enter(I, Primop);
    false -> I
  end.

transform_arith(I, Op = '+', Info) ->
  Args = safe_lookup_list(args(I), Info),
  NewInfo = analyse_insn(I, Info),
  AllTypes = case hipe_icode:is_call(I) of
	       true -> safe_lookup_list(call_dstlist(I), NewInfo) ++ Args;
	       false -> [erl_bif_types:type(erlang, Op, 2, Args)|Args]
	     end,
  case all_fixnums(AllTypes) of
    true ->
      update_call_or_enter(I, extra_unsafe_add);
    false ->
      case all_fixnums(Args) of
	true ->
	  update_call_or_enter(I, arithop_to_unsafe(Op));
	false ->
	  I
      end
  end;
transform_arith(I, Op = '-', Info) ->
  Args = safe_lookup_list(args(I), Info),
  NewInfo = analyse_insn(I, Info),
  AllTypes = case hipe_icode:is_call(I) of
	       true -> safe_lookup_list(call_dstlist(I), NewInfo) ++ Args;
	       false -> [erl_bif_types:type(erlang, Op, 2, Args)|Args]
	     end,
  case all_fixnums(AllTypes) of
    true ->
      update_call_or_enter(I, extra_unsafe_sub);
    false ->
      case all_fixnums(Args) of
	true ->
	  update_call_or_enter(I, arithop_to_unsafe(Op));
	false ->
	  I
      end
  end;
transform_arith(I, Op, Info) ->
  Args = safe_lookup_list(args(I), Info),
  NewInfo = analyse_insn(I, Info),
  List = case hipe_icode:is_call(I) of
	      true -> safe_lookup_list(call_dstlist(I), NewInfo);
	      false -> [erl_bif_types:type(erlang, Op, length(Args), Args)]
	     end,
  %% the t_is_fixnum(TempDst) check is unnecessary for bitwise boolean Ops.
  case valid_unsafe_args(Args, Op) and all_fixnums(List) of
    true -> 
      update_call_or_enter(I, arithop_to_unsafe(Op));
    false -> I
  end.

valid_unsafe_args(Args, Op) ->
  if Op =:= 'bnot' ->
      [Arg] = Args,
      t_is_fixnum(Arg);
     true ->
      [LeftArg, RightArg] = Args,
      case Op of
	'bsl'  -> t_is_fixnum(LeftArg) and t_is_bitwidth(RightArg);
	'bsr'  -> t_is_fixnum(LeftArg) and t_is_bitwidth(RightArg);
	%% 'div'  -> t_is_fixnum(LeftArg) and not span_zero(RightArg);%not needed now
	_      -> t_is_fixnum(LeftArg) and t_is_fixnum(RightArg)
      end
  end.

arithop_to_unsafe(Op) ->
  case Op of
    '+'    -> unsafe_add;
    '-'    -> unsafe_sub;
    'band' -> unsafe_band;
    'bor'  -> unsafe_bor;
    'bxor' -> unsafe_bxor;
    'bnot' -> unsafe_bnot;
    'bsl'  -> unsafe_bsl;
    'bsr'  -> unsafe_bsr
  end.

fixnum_ifop(Op) ->
  case Op of
    '=:=' -> 'fixnum_eq';
    '=/=' -> 'fixnum_neq';
    '>' -> 'fixnum_gt';
    '<' -> 'fixnum_lt';
    '>=' -> 'fixnum_ge';
    '=<' -> 'fixnum_le';
    Op -> Op
  end.

all_fixnums([Type|Left]) ->
  t_is_fixnum(Type) andalso all_fixnums(Left);
all_fixnums([]) ->
  true.

get_standard_primop(unsafe_bsl) -> 'bsl';
get_standard_primop(unsafe_bsr) -> 'bsr';
get_standard_primop(unsafe_add) -> '+';
get_standard_primop(extra_unsafe_add) -> '+';
get_standard_primop(unsafe_bnot) -> 'bnot';
get_standard_primop(unsafe_bxor) -> 'bxor';
get_standard_primop(unsafe_band) -> 'band';
get_standard_primop(unsafe_bor) -> 'bor';
get_standard_primop(unsafe_sub) -> '-';
get_standard_primop(extra_unsafe_sub) -> '-';
get_standard_primop(Op) -> Op.

primop_type(Op, Args) ->
  case Op of
    #mkfun{mfa=MFA} ->
      t_inf(t_fun(), find_signature_mfa(MFA));
    _ ->
      hipe_icode_primops:type(get_standard_primop(Op), Args)
  end.

%% sup_list(List) ->
%%   sup_list(List, t_none()).

%% sup_list([H|T], Acc) ->
%%   sup_list(T, t_sup(H, Acc));
%% sup_list([], Acc) ->
%%   Acc.

%% _________________________________________________________________
%%
%% Various help functions.
%%

add_arg_types(Args, Types) ->
  add_arg_types(Args, Types, empty()).

add_arg_types([Arg|Args], [Type|Types], Acc) ->
  Type1 =
    case t_is_none(Type) of
      true -> t_any();
      false -> Type
    end,
  add_arg_types(Args,Types, enter(Arg, Type1, Acc));
add_arg_types([], [], Acc) ->
  Acc;
add_arg_types(A, B, _) ->
  exit({wrong_number_of_arguments, {A, B}}).


%% Lookup treats anything that is neither in the map or a constant as
%% t_none(). Use this during type propagation!

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


%% safe_lookup treats anything that is neither in the map nor a
%% constant as t_any(). Use this during transformations.

safe_lookup(Var, Tree) ->
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

safe_lookup_list(List, Info) ->
  safe_lookup_list0(List, Info, []).

safe_lookup_list0([H|T], Info, Acc) ->
  safe_lookup_list0(T, Info, [safe_lookup(H, Info)|Acc]);
safe_lookup_list0([], _, Acc) ->
  lists:reverse(Acc).

enter_list([Var|VarLeft], [Type|TypeLeft], Info) ->
  NewInfo = enter(Var, Type, Info),
  enter_list(VarLeft, TypeLeft, NewInfo);
enter_list([], [], Info) ->
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
      

const_type(Const) ->
  t_from_term(hipe_icode:const_value(Const)).

do_updates(State, List) ->
  do_updates(State, List, []).

do_updates(State, [{Label, Info}|Tail], Worklist) ->
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

defines(I) ->
  keep_vars(hipe_icode:defines(I)).

call_dstlist(I) ->
  hipe_icode:call_dstlist(I).

args(I) ->
  hipe_icode:args(I).

uses(I) ->
  keep_vars(hipe_icode:uses(I)).

keep_vars(Vars) ->
  [V || V <- Vars, hipe_icode:is_var(V)].

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
		created_funs=[], phi_nodes = gb_trees:empty()}).

new_state(Cfg, {_MFA, {Is_exported, OldSig}}) ->
  SuccMap = hipe_icode_cfg:succ_map(Cfg),
  PredMap = hipe_icode_cfg:pred_map(Cfg),
  Start = hipe_icode_cfg:start_label(Cfg),  
  Params = hipe_icode_cfg:params(Cfg),
  ParamTypes = 
    case hipe_icode_cfg:is_closure(Cfg) of
      true -> 
	type_loop(t_any(), hipe_icode_cfg:closure_arity(Cfg) + 1, []);
      false -> 
	NoServer = not server__is_active(),
	if Is_exported or NoServer ->
	    type_loop(t_any(), length(Params), []);
	   true ->
	    t_fun_args(OldSig)
	end
    end,
  Info = add_arg_types(Params, ParamTypes),
  InfoMap = gb_trees:insert({Start, in}, Info, empty()),
  Liveness = hipe_icode_ssa:ssa_liveness__analyze(Cfg),
  #state{succmap=SuccMap, predmap=PredMap,info_map=InfoMap, 
	 cfg=Cfg, liveness=Liveness, arg_types=ParamTypes}.

state__cfg(#state{cfg=Cfg}) ->
  Cfg.

state__succ(#state{succmap=SM}, Label) ->
  hipe_icode_cfg:succ(SM, Label).

state__pred(#state{predmap=PM}, Label) ->
  hipe_icode_cfg:pred(PM, Label).

state__bb(#state{cfg=Cfg}, Label) ->
  hipe_icode_cfg:bb(Cfg, Label).
  
state__bb_add(S=#state{cfg=Cfg}, Label, BB) ->
  NewCfg = hipe_icode_cfg:bb_add(Cfg, Label, BB),
  S#state{cfg=NewCfg}.

state__phi_nodes(#state{phi_nodes = PhiNodes}) -> PhiNodes.

state__phi_nodes_update(State, PhiNodes) -> 
  State#state{phi_nodes = PhiNodes}.

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
  %%io:format("Info out for ~w is:\n", [Label]),
  %%[io:format("~w: ~p\n", [X, format_type(Y)]) || {X, Y} <- gb_trees:to_list(Info)],
  S#state{info_map=gb_trees:enter({Label, out}, Info, IM)}.

%% _________________________________________________________________
%%
%% The worklist.
%%

init_work(State) ->
  %%Labels = hipe_icode_cfg:reverse_postorder(state__cfg(State)),
  Labels = [hipe_icode_cfg:start_label(state__cfg(State))],
  {Labels, [], gb_sets:from_list(Labels)}.

get_work({[Label|Left], List, Set}) ->
  NewWork = {Left, List, gb_sets:delete(Label, Set)},
  {Label, NewWork};
get_work({[], [], _Set}) ->
  fixpoint;
get_work({[], List, Set}) ->
  get_work({lists:reverse(List), [], Set}).

add_work(Work = {List1, List2, Set},[Label|Left]) ->
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

call_always_fails(I, Info) ->
  {Args, Type, Fun} = 
  case I of      
    #call{} ->
      {safe_lookup_list(hipe_icode:call_args(I), Info),
       hipe_icode:call_type(I),
       hipe_icode:call_fun(I)};
    #enter{} ->
      {safe_lookup_list(hipe_icode:enter_args(I), Info),
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
    {erlang, throw, 1} -> false;
    {erlang, hibernate, 3} -> false;
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
      %% This cannot succeed!
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
  InfoIn = state__info_in(State, Label),
  warn_on_instr(Code, InfoIn, IcodeFun),
  warn_on_bb(Left, State, IcodeFun);
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
	case I of
	  #call{} ->
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
	  #enter{} ->
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
	      ok
	  end;
	_ ->
	  ok
      end
  end,
  warn_on_control_flow(Left, State, IcodeFun);
warn_on_control_flow([], _State, _IcodeFun) ->
  ok.

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
	    io_lib:format("~w: Pattern matching with [] will always fail "
			  "since variable has type ~s!\n",
			  [IcodeFun, ArgType]);
	  {tuple, N} ->
	    Tuple = construct_n_tuple(N),
	    io_lib:format("~w: Pattern matching with ~s will always fail "
			  "since variable has type ~s!\n",
			  [IcodeFun, Tuple, ArgType]);
	  {record, Atom, _Size} ->
	    io_lib:format("~w: Pattern matching with #~w{} will always fail "
			  "since variable has type ~s!\n",
			  [IcodeFun, Atom, ArgType]);
	  _ ->
	    io_lib:format("~w: Type guard ~w will always fail "
			  "since variable has type ~s!\n",
			  [IcodeFun, Test, ArgType])
	end,
      warn(?WARN_MATCHING, W);
    _ ->
      ok
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
    false -> t_to_string(Arg1)
  end
    ++ "," ++ pp_args_1([Arg2|Tail]);
pp_args_1([Arg]) ->
  case t_is_any(Arg) of
    true -> "_";
    false -> t_to_string(Arg)
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
      warn_on_call_1(I, Info, IcodeFun),
      Ins;
    false  ->
      CallFun = hipe_icode:call_fun(I),
      Warn = 
	case CallFun of
	  #mkfun{mfa={_M,F,_A}} when length(Ins) =:= 1 -> {true, {'fun', F}};
	  {erlang, make_ref, 0} when length(Ins) =:= 1 -> {true, ref};
	  _  -> false
	end,
      case Warn of
	{true, Type} ->
	  [I2] = Ins,
	  case ((is_record(I2, 'if')) andalso 
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

warn_on_enter(I, Info, IcodeFun) ->
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
			  [IcodeFun, X, ArgType1]) || X <- Arities],
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

format_fun(#element{}) ->
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

pp(State, IcodeFun, Options, Msg) ->
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
  case I of
    #call{} ->
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
  Fun = fun(X, Y) -> hipe_icode:annotate_var(X, Y) end,
  DefSubst = [{X, Fun(X, lookup(X, DefInfo))} || X <- Def],
  UseSubst = [{X, Fun(X, lookup(X, UseInfo))} || X <- Use],
  case  DefSubst ++ UseSubst of
    [] ->
      I;
    Subst ->
      hipe_icode:subst(Subst, I)
  end.

unannotate_cfg(Cfg) ->
  Labels = hipe_icode_cfg:labels(Cfg),
  unannotate_bbs(Labels, Cfg).
  
unannotate_bbs([Label|Left], Cfg) ->
  BB = hipe_icode_cfg:bb(Cfg, Label),
  Code = hipe_bb:code(BB),
  NewCode = unannotate_instr_list(Code, []),
  NewBB = hipe_bb:code_update(BB, NewCode),
  NewCfg = hipe_icode_cfg:bb_add(Cfg, Label, NewBB),
  unannotate_bbs(Left, NewCfg);
unannotate_bbs([], Cfg) ->
  Cfg.

unannotate_instr_list([I|Left], Acc) ->
  NewI = unannotate_instr(I),
  unannotate_instr_list(Left, [NewI|Acc]);
unannotate_instr_list([], Acc) ->
  lists:reverse(Acc).

unannotate_instr(I) ->
  DefUses = hipe_icode:defines(I) ++ hipe_icode:uses(I),
  Subst = [{X, hipe_icode:unannotate_var(X)}
	   || X <- DefUses,
	      hipe_icode:is_annotated_var(X) =:= true],
  if Subst =:= [] -> I;
     true -> hipe_icode:subst(Subst, I)
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
  LimitReturnType = t_limit(ReturnType, ?TYPE_DEPTH),
  {LimitReturnType, ExplExit, HasReturn}.


find_return_type_in_block([I], Info) ->
  case I of
    #enter{} ->
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
    #return{} ->
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
  case proplists:get_value(compilation_server, Options) of
    {value, Server} ->
      put(compilation_server, {value, Server}),
      server__get_signature(MFA);
    _ ->
      put(compilation_server, none),
      t_fun()
  end.

update_mfa_info(State, IcodeFun, OldSig, Options) ->
  case proplists:get_value(compilation_server, Options) of
    {value, _} ->      
      Labels = hipe_icode_cfg:labels(state__cfg(State)),
      {ReturnType, _, _} = find_return_type(Labels, State),
      ArgTypes=State#state.arg_types,
      NewSig = t_fun(ArgTypes, ReturnType),
      ?server_debug("Old & New Sig", {IcodeFun, OldSig, NewSig}),
      Temp_return = 
      case t_is_none(NewSig) of
	true ->
	  t_none();
	false ->
	  t_fun_range(NewSig)
      end,
      NewReturnType = 
	case t_is_none(Temp_return) of
	  true ->
	    t_any();
	  false ->
	    Temp_return
	end,
      server__update_return_value(IcodeFun, NewReturnType),
      OldReturnType = t_fun_range(OldSig),
      case t_is_none(NewReturnType) andalso t_is_any(OldReturnType) of
	true -> 
	  fixpoint;
	false ->
	  case t_is_equal(NewReturnType, OldReturnType) of
	    true ->
	      fixpoint;
	    false ->
%% 		  io:format("~w: Not fixpoint: ~s /= ~s\n", 
%% 			    [IcodeFun, format_type(ReturnType), 
%% 			     format_type(OldReturnType)]),
	      not_fixpoint
	  end
      end;
    _ ->
      fixpoint
  end.

find_signature(MFA = {_, _, _}, _) -> find_signature_mfa(MFA);
find_signature(Primop, Arity) -> find_signature_primop(Primop, Arity).

find_signature_mfa(MFA = {M, F, A}) ->
  PltSig = server__get_signature(MFA),
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
      t_fun(Arity, hipe_icode_primops:type(get_standard_primop(Primop)));
    ArgTypes ->
      t_fun(ArgTypes, hipe_icode_primops:type(get_standard_primop(Primop)))
  end.

find_plt_return(MFA, ArgTypes) ->
  PltSig = server__get_signature(MFA),
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

%
% Server
%


sup_lists([], [], Acc) -> lists:reverse(Acc);
sup_lists([Arg1|Tail1], [Arg2|Tail2], Acc) ->
  sup_lists(Tail1, Tail2, [t_sup(Arg1, Arg2)|Acc]).

server__has_needed_info(MFA, Is_exported, Is_closure) ->
  case get(compilation_server) of
    {value, Server} ->
      Server ! {self(), {load, message, {MFA, args}}},
      receive
	none ->
	  ?flow_debug("no info", {Is_exported, Is_closure, MFA}),
	  Is_exported or Is_closure;
	{value, Args} ->
	  ?flow_debug("has Value", Args),
	  true
      end;
    Val ->
      ?flow_debug("no comp_server", Val),
      true
  end.


server__update_call_args(MFA, ArgTypes) ->
  ?server_debug("in update call args", MFA),
  case get(compilation_server) of
    {value, Server} ->
      %% This code isn't ugly ... I know.
      Key = {MFA, args},
      PID = self(), %% debug only
      Fun = fun(MessageTree) ->
		{Version, OldArgtypes} = 
		  case gb_trees:lookup(Key, MessageTree) of
		    none ->
		      {0, t_any()};
		    {value, {LookupVersion, LookupType}} ->
		      {LookupVersion, LookupType}
		  end,
		Unions = 
		  case Version =:= 0 of
		    true ->
		      ArgTypes;
		    false ->
		      Widening = 
			lists:zipwith(fun(New, Old) -> 
					  widening(New, Old, Version) end,
				      ArgTypes, OldArgtypes),
		      sup_lists(OldArgtypes, Widening, [])
		  end,
		case t_is_equal(Unions, OldArgtypes) of
		  true ->
		    MessageTree;
		  false ->
		    ?server_debug("update call args: pid, mfa, old, new",
				  {PID, MFA, OldArgtypes, Unions}),
		    gb_trees:enter(Key, {Version + 1, Unions}, 
				   MessageTree)
		end
	    end,
      Server ! {self(), {transaction, Fun}};
    _ ->
      ok
  end.

server__update_return_value(MFA, ReturnType) ->
  ?server_debug("in update_return_value", {MFA, ReturnType}),
  case get(compilation_server) of
    {value, Server} ->
      Key = {MFA, return_range}, 
      Fun = fun(MessageTree) ->
		{OldReturntype, Version} = 
		  case gb_trees:lookup(Key, MessageTree) of
		    none ->
		      {t_any(), 0};
		    {value, {LookupType, LookupVersion}} ->
		      {LookupType, LookupVersion}
		  end,
		case t_is_equal(OldReturntype, ReturnType) of 
		  true ->
		    MessageTree;
		  false ->
		    ?server_debug("New return value", {MFA, ReturnType}),
		    gb_trees:enter(Key, {ReturnType, Version + 1}, 
				   MessageTree)
		end
	    end,
      
      Server ! {self(), {transaction, Fun}};
    _ ->
      ok
  end.
  
server__get_return_type(MFA) ->
  case get(compilation_server) of
    {value, Server} ->
      Server ! {self(), {load, message, {MFA, return_range}}},
      receive
	none ->
	  {t_any(), 0};
	{value, LookupType} ->
	  LookupType
      end;
    _ ->
      {t_any(), 0}
  end.

type_loop(_Type, 0, Acc) -> Acc;
type_loop(Type, N, Acc) -> type_loop(Type, N - 1, [Type|Acc]).

server__is_active() ->
  case get(compilation_server) of
    {value, _Server} ->
      true;
    _ ->
      false
  end.

server__get_arg_types(MFA) ->
  server__get_arg_types(MFA, unkown, t_any()).

server__get_arg_types(MFA, Length, None_value) ->
  Res = 
  case get(compilation_server) of
    {value, Server} ->
      Server ! {self(), {load, message, {MFA, args}}},
      receive
	none ->
	  if Length =:= unkown ->
	      {0, t_any()};
	     true ->
	      {0, type_loop(None_value, Length, [])}
	  end;
	{value, {Version, LookupType}} ->
	  {Version, LookupType}
      end;
    _ ->
      if Length =:= unkown ->
	  {0, t_any()};
	 true ->
	  {0, type_loop(None_value, Length, [])}
      end
  end,
  ?server_debug("Reading", {MFA, Res}),
  Res.

server__get_signature(MFA) ->
  {_, Args} = server__get_arg_types(MFA), 
  {Return, _Version} = server__get_return_type(MFA),
  Ans = 
    case t_is_none(Args) or t_is_none(Return) of
      true ->
	t_fun();
      false ->
	case (Args =:= t_any()) of
	  true ->
	    t_fun(Return);
	  false ->
	    t_fun(Args, Return)
	end
    end,
  ?server_debug("Signature", {MFA, Ans}),
  Ans.

%%=====================================================================
%% Testing function below
%%=====================================================================

-ifdef(DO_HIPE_ICODE_TYPE_TEST).

test() ->
  Range1 = t_from_range(1, pos_inf),
  Range2 = t_from_range(0, 5),
  Var1 = {var, 1},
  Var2 = {var, 2},

  Info = enter(Var1, Range1, enter(Var2, Range2, gb_trees:empty())),
  io:format("A1 ~p~n", [Info]),
  A = integer_range_inequality_propagation('<', Var1, Var2, 1, 2, Info),
  B = integer_range_inequality_propagation('>=', Var1, Var2, 1, 2, Info),
  C = integer_range_inequality_propagation('=<', Var1, Var2, 1, 2, Info),
  D = integer_range_inequality_propagation('>', Var1, Var2, 1, 2, Info),

  io:format("< ~p~n", [A]),
  io:format(">= ~p~n", [B]),
  io:format("<= ~p~n", [C]),
  io:format("> ~p~n", [D]).

-endif.
