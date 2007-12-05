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
%% $Id$
%%

%%%-------------------------------------------------------------------
%%% File    : dialyzer_dataflow.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description : 
%%%
%%% Created : 19 Apr 2005 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------
-module(dialyzer_dataflow).

-export([annotate_module/2,
	 doit/1,
	 doit/2,
	 get_fun_types/4,
	 get_top_level_signatures/2,
	 get_warnings/5,
	 pp/1]).

-include("dialyzer.hrl").
-include("dialyzer_callgraph.hrl").

-import(erl_types, 
	[t_any/0, t_atom/0, t_atom/1, t_binary/0, t_bool/0, t_cons/0, t_cons/2,
	 t_bitstr/0, t_bitstr/2, t_bitstr_concat/1, t_bitstr_match/2,
	 t_cons_hd/1, t_cons_tl/1, t_components/1, t_float/0,
	 t_from_range/2, t_fun/0, t_fun/2, t_fun_args/1, t_fun_range/1,
	 t_inf/2, t_inf_lists/2, t_integer/0, t_is_nil/1,
	 t_is_atom/1, t_is_atom/2, t_is_bool/1, t_is_unit/1,
	 t_atom_vals/1, t_is_equal/2, t_is_none/1, t_is_none_or_unit/1,
	 t_is_any/1, t_is_subtype/2, t_limit/2, t_list/0,
	 t_non_neg_integer/0, t_number/0, t_number_vals/1, t_pid/0, t_port/0,
	 t_maybe_improper_list/0,
	 t_product/1, t_ref/0, t_to_string/2,
	 t_tuple/0, t_tuple/1, t_tuple_args/1,
	 t_tuple_subtypes/1, t_sup/1, t_sup/2, t_subtract/2,
	 t_from_term/1, t_none/0, t_unit/0]).

%-define(DEBUG, true).
%-define(DEBUG_PP, true).
%-define(DEBUG_TIME, true).
%-define(DOT, true).

-ifdef(DEBUG).
-import(erl_types, [t_to_string/1]).
-define(debug(S_, L_), io:format(S_, L_)).
-else.
-define(debug(S_, L_), ok).
-endif.

%-define(debug1(S_, L_), io:format(S_, L_)).
%-define(debug1(S_, L_), ok).

-define(TYPE_LIMIT, 3).

-record(state, {callgraph              :: #dialyzer_callgraph{},
		envs,                  %% dict()
		forward_mode           :: 'constrained' | 'unconstrained',
		fun_tab,               %% dict()
		plt,
		records,
		tree_map,              %% dict()
		warning_mode = 'false' :: bool(),
		warnings = []          :: [_],
		work}).

pp(File) ->
  Code = dialyzer_utils:get_core_from_src(File, [no_copt]),
  Plt = get_def_plt(),
  AnnTree = annotate_module(Code, Plt),
  dialyzer_plt:delete(Plt),
  io:put_chars(cerl_prettypr:format(AnnTree, [{hook, cerl_typean:pp_hook()}])),
  io:nl().

get_warnings(Tree, Plt, Callgraph, Records, NoWarnUnused) ->
  State = analyze_module(Tree, Plt, Callgraph, Records, true),
  {state__get_warnings(State, NoWarnUnused), state__all_fun_types(State)}.

get_fun_types(Tree, Plt, Callgraph, Records) ->
  State = analyze_module(Tree, Plt, Callgraph, Records, false),
  state__all_fun_types(State).

get_top_level_signatures(Code, Records) ->
  {Tree, _} = cerl_trees:label(cerl:from_records(Code)),
  Callgraph0 = dialyzer_callgraph:new(),
  Callgraph1 = dialyzer_callgraph:scan_core_tree(Tree, Callgraph0),
  {Callgraph2, _} = dialyzer_callgraph:remove_external(Callgraph1),
  Callgraph = dialyzer_callgraph:finalize(Callgraph2),
  to_dot(Callgraph),
  Plt = get_def_plt(),
  FunTypes = get_fun_types(Tree, Plt, Callgraph, Records),
  FunTypes1 = lists:foldl(fun({V, F}, Acc) ->
			      Label = get_label(F),
			      case dict:find(Label, Acc) of
				error ->
				  Arity = cerl:fname_arity(V),
				  Type = t_fun(lists:duplicate(Arity,
							       t_none()), 
					       t_none()),
				  dict:store(Label, Type, Acc);
				{ok, _} -> Acc
			      end
			  end, FunTypes, cerl:module_defs(Tree)),
  dialyzer_callgraph:delete(Callgraph),
  Sigs = [{{cerl:fname_id(V), cerl:fname_arity(V)}, 
	   dict:fetch(get_label(F), FunTypes1)} 
	  || {V, F} <- cerl:module_defs(Tree)],
  ordsets:from_list(Sigs).

get_def_plt() ->
  try 
    dialyzer_plt:from_file(filename:join([code:lib_dir(dialyzer),
					  "plt","dialyzer_init_plt"]))
  catch
    throw:{dialyzer_error, _} -> dialyzer_plt:new()
  end.

doit(Module) ->
  doit(Module, [no_copt]).

doit(Module, Opts) ->
  AbstrCode = dialyzer_utils:get_abstract_code_from_src(Module, Opts),
  Code = dialyzer_utils:get_core_from_abstract_code(AbstrCode, Opts),
  {ok, Records} = dialyzer_utils:get_record_and_type_info(AbstrCode),
  Sigs = get_top_level_signatures(Code, Records),
  [io:format("~w/~w :: ~s\n", [F, A, t_to_string(T, Records)])
   || {{F, A}, T} <- Sigs].

%%% ===========================================================================
%%%
%%%  Annotate all top level funs.
%%%
%%% ===========================================================================

annotate_module(Code, Plt) ->
  {Tree, _} = cerl_trees:label(cerl:from_records(Code)),
  Callgraph0 = dialyzer_callgraph:new(),
  Callgraph1 = dialyzer_callgraph:scan_core_tree(Tree, Callgraph0),
  {Callgraph2, _} = dialyzer_callgraph:remove_external(Callgraph1),
  Callgraph = dialyzer_callgraph:finalize(Callgraph2),
  State = analyze_module(Tree, Plt, Callgraph),
  Res = annotate(Tree, State),
  dialyzer_callgraph:delete(Callgraph),
  Res.

annotate(Tree, State) ->
  case cerl:subtrees(Tree) of
    [] -> set_type(Tree, State);
    List -> 
      NewSubTrees = [[annotate(Subtree, State) || Subtree <- Group]
		     || Group <- List],
      NewTree = cerl:update_tree(Tree, NewSubTrees),
      set_type(NewTree, State)
  end.

set_type(Tree, State) ->
  case cerl:type(Tree) of
    'fun' ->
      Type = state__fun_type(Tree, State),
      case t_is_any(Type) of
	true -> 
	  cerl:set_ann(Tree, delete_ann(typesig, cerl:get_ann(Tree)));
	false -> 
	  cerl:set_ann(Tree, append_ann(typesig, Type, cerl:get_ann(Tree)))
      end;
    apply ->
      case state__find_apply_return(Tree, State) of
	unknown -> Tree;
	ReturnType ->
	  case t_is_any(ReturnType) of
	    true -> 
	      cerl:set_ann(Tree, delete_ann(type, cerl:get_ann(Tree)));
	    false -> 
	      cerl:set_ann(Tree, append_ann(type, ReturnType, 
					    cerl:get_ann(Tree)))
	  end
      end;
    _ ->
      Tree
  end.

append_ann(Tag, Val, [X | Xs]) ->
  if tuple_size(X) >= 1, element(1, X) =:= Tag -> 
      append_ann(Tag, Val, Xs);
     true ->
      [X | append_ann(Tag, Val, Xs)]
  end;
append_ann(Tag, Val, []) ->
  [{Tag, Val}].

delete_ann(Tag, [X | Xs]) ->
  if tuple_size(X) >= 1, element(1, X) =:= Tag -> 
      delete_ann(Tag, Xs);
     true ->
      [X | delete_ann(Tag, Xs)]
  end;
delete_ann(_, []) ->
  [].

%%% ===========================================================================
%%%
%%%  The analysis.
%%%
%%% ===========================================================================

analyze_module(Tree, Plt, Callgraph) ->
  analyze_module(Tree, Plt, Callgraph, dict:new(), false).

analyze_module(Tree, Plt, Callgraph, Records, GetWarnings) ->
  debug_pp(Tree, false),
  TopFun = cerl:ann_c_fun([{label, top}], [], Tree),
  case GetWarnings of
    true ->
      State = state__new(Callgraph, TopFun, Plt, Records, unconstrained),
      State1 = analyze_loop(State),
      State2 = state__set_warning_mode(State1),
      analyze_loop(State2);
    false ->
      State = state__new(Callgraph, TopFun, Plt, Records, constrained),
      analyze_loop(State)
  end.

analyze_loop(State) ->
  case state__get_work(State) of
    none -> state__clean_not_called(State);
    {Fun, NewState} ->
      ArgTypes = state__get_args(Fun, NewState),
      case any_none(ArgTypes) of
	true -> 
	  ?debug("Not handling1 ~w: ~s\n", 
		 [state__lookup_name(get_label(Fun), State), 
		  t_to_string(t_product(ArgTypes))]),
	  analyze_loop(NewState);
	false -> 
	  case state__fun_env(Fun, NewState) of
	    none -> 
	      ?debug("Not handling2 ~w: ~s\n", 
		     [state__lookup_name(get_label(Fun), State), 
		      t_to_string(t_product(ArgTypes))]),
	      analyze_loop(NewState);
	    Map ->
	      ?debug("Handling fun ~p: ~s\n", 
		     [state__lookup_name(get_label(Fun), State), 
		      t_to_string(state__fun_type(Fun, NewState))]),
	      NewState1 = state__mark_fun_as_handled(NewState, Fun),
	      Vars = cerl:fun_vars(Fun),
	      Map1 = enter_type_lists(Vars, ArgTypes, Map),
	      Body = cerl:fun_body(Fun),
	      {NewState2, _Map2, BodyType} = 
		traverse(Body, Map1, NewState1),
	      ?debug("Done analyzing: ~w:~s\n", 
		     [state__lookup_name(get_label(Fun), State),
		      t_to_string(t_fun(ArgTypes, BodyType))]),
	      NewState3 =
		state__update_fun_entry(Fun, ArgTypes, BodyType, NewState2),
	      ?debug("done adding stuff for ~w\n", 
		     [state__lookup_name(get_label(Fun), State)]),
	      analyze_loop(NewState3)
	  end
      end
  end.

traverse(Tree, Map, State) ->
  ?debug("Handling ~p\n", [cerl:type(Tree)]),
  %%debug_pp_map(Map),
  case cerl:type(Tree) of
    apply -> 
      handle_apply(Tree, Map, State);
    binary ->
      Segs = cerl:binary_segments(Tree),
      {State1, Map1, SegTypes} = traverse_list(Segs, Map, State),
      {State1, Map1, t_bitstr_concat(SegTypes)};
    bitstr ->
      handle_bitstr(Tree, Map, State);
    call ->
      handle_call(Tree, Map, State);
    'case' ->
      handle_case(Tree, Map, State);
    'catch' ->
      {State1, _Map1, _} = traverse(cerl:catch_body(Tree), Map, State),
      {State1, Map, t_any()};
    cons ->
      handle_cons(Tree, Map, State);
    'fun' ->
      Type = state__fun_type(Tree, State),
      case state__warning_mode(State) of
	true -> {State, Map, Type};
	false ->
	  State1 = state__add_work(get_label(Tree), State),
	  State2 = state__update_fun_env(Tree, Map, State1),
	  {State2, Map, Type}
      end;
    'let' ->
      handle_let(Tree, Map, State);
    letrec ->
      Defs = cerl:letrec_defs(Tree),
      Body = cerl:letrec_body(Tree),
      %% By not including the variables in scope we can assure that we
      %% will get the current function type when using the variables.
      FoldFun = fun({Var, Fun}, {AccState, AccMap}) ->
		    {NewAccState, NewAccMap0, FunType} = 
		      traverse(Fun, AccMap, AccState),
		    NewAccMap = enter_type(Var, FunType, NewAccMap0),
		    {NewAccState, NewAccMap}
		end,
      {State1, Map1} = lists:foldl(FoldFun, {State, Map}, Defs),
      traverse(Body, Map1, State1);
    literal ->
      %% This is needed for finding records
      case cerl:unfold_literal(Tree) of
	Tree -> {State, Map, literal_type(Tree)};
	NewTree -> traverse(NewTree, Map, State)
      end;
    module ->
      handle_module(Tree, Map, State);
    primop ->
      Type =
	case cerl:atom_val(cerl:primop_name(Tree)) of
	  match_fail -> t_none();
	  raise -> t_none();
	  bs_init_writable -> t_from_term(<<>>);
	  Other -> erlang:error({'Unsupported primop', Other})
	end,
      {State, Map, Type};      
    'receive' ->
      handle_receive(Tree, Map, State);
    seq ->
      Arg = cerl:seq_arg(Tree),
      Body = cerl:seq_body(Tree),
      {State1, Map1, ArgType} = traverse(Arg, Map, State),
      case t_is_none_or_unit(ArgType) of
	true ->
	  {State1, Map1, ArgType};
	false ->
	  traverse(Body, Map1, State1)
      end;
    'try' ->
      handle_try(Tree, Map, State);
    tuple ->
      handle_tuple(Tree, Map, State);
    values ->
      Elements = cerl:values_es(Tree),
      {State1, Map1, EsType} = traverse_list(Elements, Map, State),
      Type  = t_product(EsType),
      {State1, Map1, Type};
    var ->
      ?debug("Looking up unknown variable: ~p\n", [Tree]),
      case state__lookup_type_for_rec_var(Tree, State) of
	error -> {State, Map, lookup_type(Tree, Map)};
	{ok, Type} -> {State, Map, Type}
      end;
    Other ->
      erlang:error({'Unsupported type', Other})
  end.

traverse_list(Trees, Map, State) ->
  traverse_list(Trees, Map, State, []).

traverse_list([Tree|Tail], Map, State, Acc) ->
  {State1, Map1, Type} = traverse(Tree, Map, State),
  traverse_list(Tail, Map1, State1, [Type|Acc]);
traverse_list([], Map, State, Acc) ->
  {State, Map, lists:reverse(Acc)}.

  
%%________________________________________
%%
%% Special instructions
%%

handle_apply(Tree, Map, State) ->
  Args = cerl:apply_args(Tree),
  Op = cerl:apply_op(Tree),
  {State1, Map1, ArgTypes} = traverse_list(Args, Map, State),
  {State2, Map2, OpType} = traverse(Op, Map1, State1),
  case any_none(ArgTypes) of
    true -> 
      {State2, Map2, t_none()};
    false ->
      {CallSitesKnown, FunList} =
	case state__lookup_call_site(Tree, State2) of
	  error -> {false, []};
	  {ok, [external]} -> {false, {}};
	  {ok, List} -> {true, List}
	end,
      case CallSitesKnown of
	false ->
	  OpType1 = t_inf(OpType, t_fun(length(Args), t_any())),
	  case t_is_none(OpType1) of
	    true -> {State2, Map2, t_none()};
	    false ->
	      NewArgs = t_inf_lists(ArgTypes, t_fun_args(OpType1)),
	      case any_none(NewArgs) of
		true -> 
		  Msg = 
		    io_lib:format("Fun application with arguments ~s will fail "
				  "since the function has type ~s",
				  [format_args(Args, ArgTypes, State),
				   format_type(OpType, State)]),
		  State3 = state__add_warning(State2, ?WARN_FAILING_CALL,
					      Tree, Msg),
		  {State3, enter_type(Op, OpType1, Map2), t_none()};
		false ->
		  Map3 = enter_type_lists(Args, NewArgs, Map2),
		  {State2, enter_type(Op, OpType1, Map3), t_fun_range(OpType1)}
	      end
	  end;
	true ->
	  FunInfoList = [{local, state__fun_info(Fun, State)} 
			 || Fun <- FunList],
	  handle_apply_or_call(FunInfoList, Args, ArgTypes, Map2, Tree, State1)
      end
  end.

handle_apply_or_call(FunInfoList, Args, ArgTypes, Map, Tree, State) ->
  handle_apply_or_call(FunInfoList, Args, ArgTypes, Map, Tree, State, 
		       lists:duplicate(length(ArgTypes), t_none()), t_none()).

handle_apply_or_call([{local, external}|Left], Args, ArgTypes, Map, Tree, State,
		     _AccArgTypes, _AccRet) ->
  handle_apply_or_call(Left, Args, ArgTypes, Map, Tree, 
		       State, ArgTypes, t_any());
handle_apply_or_call([{TypeOfApply, {Fun, Sig, Contr, LocalRet}}|Left],
		     Args, ArgTypes, Map, Tree, State, AccArgTypes, AccRet) ->
  GenSig = {lists:duplicate(length(Args), t_any()), fun(_) -> t_any() end},
  {CArgs, CRange} = 
    case Contr of
      {value, C = #contract{args=As}} ->
	{As, fun(FunArgs) -> 
		 dialyzer_contracts:get_contract_return(C, FunArgs)
	     end};
      none -> GenSig
    end,
  {BifArgs, BifRange} =
    case TypeOfApply of
      remote ->
	{M, F, A} = Fun,
	case erl_bif_types:is_known(M, F, A) of
	  true -> {erl_bif_types:arg_types(M, F, A),
		   fun(FunArgs) -> erl_bif_types:type(M, F, A, FunArgs) end};
	  false -> GenSig
	end;
      local ->
	GenSig
    end,
  {SigArgs, SigRange} =
    case Sig of
      {value, {SR, SA}} -> {SA, SR};
      none -> {lists:duplicate(length(Args), t_any()), t_any()}
    end,

  NewArgsSig = t_inf_lists(SigArgs, ArgTypes),
  NewArgsContract = t_inf_lists(CArgs, ArgTypes),
  NewArgsBif = t_inf_lists(BifArgs, ArgTypes),
  NewArgTypes0 = t_inf_lists(NewArgsSig, NewArgsContract), 
  NewArgTypes = t_inf_lists(NewArgTypes0, NewArgsBif),

  RetWithoutLocal = t_inf(t_inf(CRange(NewArgTypes), BifRange(NewArgTypes)), 
			  SigRange),
  ?debug("Fun: ~p\n", [Fun]),
  ?debug("Args: ~s\n", [erl_types:t_to_string(t_product(ArgTypes))]),
  ?debug("NewArgsSig: ~s\n", [erl_types:t_to_string(t_product(NewArgsSig))]),
  ?debug("NewArgsContract: ~s\n", 
	 [erl_types:t_to_string(t_product(NewArgsContract))]),
  ?debug("NewArgsBif: ~s\n", [erl_types:t_to_string(t_product(NewArgsBif))]),
  ?debug("NewArgTypes: ~s\n", [erl_types:t_to_string(t_product(NewArgTypes))]),
  ?debug("RetWithoutLocal: ~s\n", [erl_types:t_to_string(RetWithoutLocal)]),
  ?debug("BifRet: ~s\n", [erl_types:t_to_string(BifRange(NewArgTypes))]),
  ?debug("ContrRet: ~s\n", [erl_types:t_to_string(CRange(NewArgTypes))]),
  ?debug("SigRet: ~s\n", [erl_types:t_to_string(SigRange)]),

  FailedConj = any_none([RetWithoutLocal|NewArgTypes]),
  IsFailBif = t_is_none(BifRange(BifArgs)),
  IsFailSig = t_is_none(SigRange),
  State1 =
    case FailedConj andalso not (IsFailBif orelse IsFailSig) of
      true ->
	FailedSig = any_none(NewArgsSig),
	FailedContract = any_none([CRange(NewArgsContract)|NewArgsContract]),
	FailedBif = any_none([BifRange(NewArgsBif)|NewArgsBif]),
	InfSig = t_inf(t_fun(SigArgs, SigRange), 
		       t_fun(BifArgs, BifRange(BifArgs))),
	FailReason = apply_fail_reason(FailedSig, FailedBif, FailedContract),
	Msg = get_apply_fail_msg(Fun, Args, ArgTypes, NewArgTypes, InfSig, 
				 Contr, State, FailReason),
	state__add_warning(State, ?WARN_FAILING_CALL, Tree, Msg);
      false ->
	State
    end,
  State2 =
    case TypeOfApply of
      local -> 
	case state__is_escaping(Fun, State1) of
	  true -> State1;
	  false ->
	    case state__forward_mode(State) of
	      unconstrained -> 
		ForwardArgs = [t_limit(X, ?TYPE_LIMIT) || X <- ArgTypes],
		forward_args(Fun, ForwardArgs, State1);
	      constrained ->
		case FailedConj andalso (not IsFailSig) of
		  true -> State1;
		  false -> 
		    ForwardArgs = [t_limit(X, ?TYPE_LIMIT) || X <- NewArgTypes],
		    forward_args(Fun, ForwardArgs, State1)
		end
	    end
	end;
      remote ->
	add_bif_warnings(Fun, NewArgTypes, Tree, State1)
    end,
  NewAccArgTypes = 
    case FailedConj of
      true -> AccArgTypes;
      false -> [t_sup(X, Y) || {X, Y} <- lists:zip(NewArgTypes, AccArgTypes)]
    end,
  NewAccRet = t_sup(AccRet, t_inf(RetWithoutLocal, LocalRet)),
  handle_apply_or_call(Left, Args, ArgTypes, Map, Tree, 
		       State2, NewAccArgTypes, NewAccRet);
handle_apply_or_call([], Args, _ArgTypes, Map, _Tree, State, 
		     AccArgTypes, AccRet) ->
  NewMap = enter_type_lists(Args, AccArgTypes, Map),
  {State, NewMap, AccRet}.

apply_fail_reason(FailedSig, FailedBif, FailedContract) ->
  if
    (FailedSig orelse FailedBif) andalso (not FailedContract) -> only_sig;
    FailedContract andalso (not (FailedSig orelse FailedBif)) -> only_contract;
    true                                                      -> both
  end.

get_apply_fail_msg(Fun, Args, ArgTypes, NewArgTypes, 
		   Sig, Contract, State, FailReason) ->
  case state__warning_mode(State) of
    false -> [];
    true ->
      Contract1 =
	case Contract of
	  {value, C = #contract{}} -> C;
	  none -> none
	end,
      Msg1 =
	case state__lookup_name(Fun, State) of
	  {M, F, _A} ->
	    io_lib:format("The call ~w:~w~s",
			  [M, F, format_args(Args, ArgTypes, State)]);
	  Label when is_integer(Label) ->
	    io_lib:format("Fun application with arguments ~s",
			  [format_args(Args, ArgTypes, State)])
	end,
      EnumArgTypes = lists:zip(lists:seq(1, length(NewArgTypes)), NewArgTypes),
      ArgNs = [Arg || {Arg, Type} <- EnumArgTypes, t_is_none(Type)],
      get_apply_fail_msg_1(FailReason, Msg1, Sig, Contract1, State, ArgNs)
  end.

get_apply_fail_msg_1(FailReason, Msg, Sig, Contract, State, ArgNs) ->
  PositionString =
    case ArgNs of
      [] -> [];
      [N1] -> io_lib:format("position ~w", [N1]);
      [_|_] -> 
	" and"++ArgString = lists:flatten([io_lib:format(" and ~w", [N]) 
					   || N <- ArgNs]),
	"positions" ++ ArgString
    end,
  case FailReason of
    only_sig ->
      case PositionString =:= [] of
	true ->
	  %% We do not know which arguments that caused the failure. 
	  io_lib:format("~s will fail since the success typing arguments"
			" are ~s\n", 
			[Msg, format_sig_args(Sig, State)]);
	false ->
	  io_lib:format("~s will fail since it differs in argument ~s from the "
			"success typing arguments: ~s\n", 
			[Msg, PositionString, format_sig_args(Sig, State)])
      end;
    only_contract -> 
      case ((PositionString =:= []) 
	    orelse dialyzer_contracts:is_overloaded(Contract)) of
	true ->
	  %% We do not know which arguments that caused the failure. 
	  io_lib:format("~s breaks the contract ~s\n", 
			[Msg, format_sig(Contract, State)]);
	false ->
	  io_lib:format("~s breaks the contract ~s in argument ~s\n",
			[Msg, format_sig(Contract, State), PositionString])
      end;
    both  ->
      io_lib:format("~s will fail since the success typing is ~s and "
		    "the contract is ~s\n",
		    [Msg, format_sig(Sig, State), format_sig(Contract, State)])
  end.

add_bif_warnings({erlang, '=:=', 2}, [T1, T2], Tree, State) ->
  Inf = t_inf(T1, T2),
  case t_is_none(Inf) andalso (not any_none([T1, T2])) of
    true ->
      Msg = io_lib:format("~s =:= ~s can never evaluate to 'true'\n", 
			  [format_type(T1, State), format_type(T2, State)]),
      state__add_warning(State, ?WARN_MATCHING, Tree, Msg);
    false ->
      State
  end;
add_bif_warnings(_, _, _, State) ->
  State.

%%----------------------------------------

handle_bitstr(Tree, Map, State) ->
  %% Construction of binaries.
  Size = cerl:bitstr_size(Tree),
  Val = cerl:bitstr_val(Tree),
  BitstrType = cerl:concrete(cerl:bitstr_type(Tree)),
  {State1, Map1, SizeType0} = traverse(Size, Map, State),
  {State2, Map2, ValType0} = traverse(Val, Map1, State1),
  case cerl:is_c_atom(Size) andalso (cerl:concrete(Size) =:= all) of
    true ->
      %% Just an assert
      binary = BitstrType,
      ValType = t_inf(ValType0, t_bitstr()),
      Map3 = enter_type(Val, ValType, Map2),
      {State2, Map3, ValType};
    false ->
      SizeType = t_inf(SizeType0, t_non_neg_integer()),
      ValType =
	case BitstrType of
	  float -> t_inf(ValType0, t_number());
	  binary -> t_inf(ValType0, t_bitstr());
	  integer -> t_inf(ValType0, t_integer())
	end,
      case any_none([SizeType, ValType]) of
	true ->
	  {Msg, Offending} = 
	    case t_is_none(SizeType) of
	      true ->
		{io_lib:format("Binary construction will fail since "
			       "the size field ~s in "
			       "binary segment ~s has type ~s\n",
			       [format_cerl(Size), format_cerl(Tree),
				format_type(SizeType0, State2)]),
		 Size};
	      false ->
		{io_lib:format("Binary construction will fail since "
			       "the value field ~s in "
			       "binary segment ~s has type ~s\n",
			       [format_cerl(Val), format_cerl(Tree),
				format_type(ValType0, State2)]),
		 Val}
	    end,
	  State3 = state__add_warning(State2, ?WARN_FAILING_CALL, 
				      Offending, Msg),
	  {State3, Map2, t_none()};
	false ->
	  UnitVal = cerl:concrete(cerl:bitstr_unit(Tree)),
	  Type =
	    case t_number_vals(SizeType) of
	      [OneSize] -> t_bitstr(0, OneSize * UnitVal);
	      _ -> 
		MinSize = erl_types:number_min(SizeType),
		t_bitstr(UnitVal, UnitVal * MinSize)
	    end,
	  Map3 = enter_type_lists([Val, Size, Tree],
				  [ValType, SizeType, Type], Map2),
	  {State2, Map3, Type}
      end
  end.

%%----------------------------------------

handle_call(Tree, Map, State) ->
  M = cerl:call_module(Tree),
  F = cerl:call_name(Tree),
  Args = cerl:call_args(Tree),
  MFAList = [M, F|Args],
  {State1, Map1, [MType0, FType0|As]} = traverse_list(MFAList, Map, State),
  MType = t_inf(t_atom(), MType0),
  FType = t_inf(t_atom(), FType0),
  Map2 = enter_type_lists([M,F], [MType, FType], Map1),
  case any_none([MType, FType|As]) of
    true ->
      {State1, Map2, t_none()};
    false ->
      %% XXX: Consider doing this for all combinations of MF
      case {t_atom_vals(MType), t_atom_vals(FType)} of
	{[MAtom], [FAtom]} ->
	  FunInfo = [{remote, state__fun_info({MAtom, FAtom, length(Args)},
					      State1)}],
	  handle_apply_or_call(FunInfo, Args, As, Map2, Tree, State1);
	{_MAtoms, _FAtoms} ->
	  {State1, Map2, t_any()}
      end
  end.

%%----------------------------------------

handle_case(Tree, Map, State) ->
  Arg = cerl:case_arg(Tree),
  Clauses = filter_match_fail(cerl:case_clauses(Tree)),
  {State1, Map1, ArgType} = traverse(Arg, Map, State),
  ArgComps = wrap_if_single(t_components(ArgType)),
  case any_none(ArgComps) of
    true -> {State1, Map1, t_none()};
	false ->
      case any_unit(ArgComps) of
	true -> {State1, Map1, t_unit()};
	false ->
	  {MapList, State2, Type} = 
	    handle_clauses(Clauses, Arg, ArgType, ArgType, State1, 
			   [], Map1, []),
	  Map2 = join_maps(MapList, Map1),
	  debug_pp_map(Map2),
	  {State2, Map2, Type}
      end
  end.

%%----------------------------------------

handle_cons(Tree, Map, State) ->
  Hd = cerl:cons_hd(Tree),
  Tl = cerl:cons_tl(Tree),
  {State1, Map1, HdType} = traverse(Hd, Map, State),
  {State2, Map2, TlType} = traverse(Tl, Map1, State1),
  State3 = 
    case t_is_none(t_inf(TlType, t_list())) of
      true ->
	Msg = io_lib:format("Cons will produce an improper list since its "
			    "2nd argument is ~s\n",
			    [format_type(TlType, State2)]),
	state__add_warning(State2, ?WARN_NON_PROPER_LIST, Tree, Msg);
      false ->
	State2
    end,
  Type = t_cons(HdType, TlType),
  {State3, Map2, Type}.

%%----------------------------------------

handle_let(Tree, Map, State) ->
  Arg = cerl:let_arg(Tree),
  Vars = cerl:let_vars(Tree),
  Map0 =
    case cerl:is_c_var(Arg) of
      true ->
	[Var] = Vars,
	enter_subst(Var, Arg, Map);
      false ->
	    Map
    end,
  Body = cerl:let_body(Tree),
  {State1, Map1, ArgTypes} = traverse(Arg, Map0, State),
  case t_is_none_or_unit(ArgTypes) of
    true -> {State1, Map1, ArgTypes};
    false -> 
      VarTypes = wrap_if_single(t_components(ArgTypes)),
      Map2 = enter_type_lists(Vars, VarTypes, Map1),
      traverse(Body, Map2, State1)
  end.

%%----------------------------------------

handle_module(Tree, Map, State) ->
  %% By not including the variables in scope we can assure that we
  %% will get the current function type when using the variables.
  Defs = cerl:module_defs(Tree),
  PartFun = fun({_Var, Fun}) -> 
		state__is_escaping(get_label(Fun), State)
	    end,
  {Defs1, Defs2} = 	lists:partition(PartFun, Defs),
  Letrec = cerl:c_letrec(Defs1, cerl:c_int(42)),
  {State1, Map1, _FunTypes} = traverse(Letrec, Map, State),
  %% Also add environments for the other top-level functions.
  VarTypes = [{Var, state__fun_type(Fun, State1)} || {Var, Fun} <- Defs],
  EnvMap = enter_type_list(VarTypes, Map),
  FoldFun = fun({_Var, Fun}, AccState) ->
		state__update_fun_env(Fun, EnvMap, AccState)
	    end,
  State2 = lists:foldl(FoldFun, State1, Defs2),
  {State2, Map1, t_any()}.

%%----------------------------------------

handle_receive(Tree, Map, State) ->
  Clauses = filter_match_fail(cerl:receive_clauses(Tree)),
  Timeout = cerl:receive_timeout(Tree),
  {MapList, State1, ReceiveType} = 
    handle_clauses(Clauses, none, t_any(), t_any(), State, [], Map, []),
  Map1 = join_maps(MapList, Map),
  {State2, Map2, TimeoutType} = traverse(Timeout, Map1, State1),
  case (t_is_atom(TimeoutType) andalso 
	(t_atom_vals(TimeoutType) =:= [infinity])) of
    true ->
      {State2, Map2, ReceiveType};
    false ->
      Action = cerl:receive_action(Tree),
      {State3, Map3, ActionType} = traverse(Action, Map, State2),
      Map4 = join_maps([Map3, Map1], Map),
      Type = t_sup(ReceiveType, ActionType),
      {State3, Map4, Type}
  end.

%%----------------------------------------

handle_try(Tree, Map, State) ->
  Arg = cerl:try_arg(Tree),
  EVars = cerl:try_evars(Tree),
  Vars = cerl:try_vars(Tree),
  Body = cerl:try_body(Tree),
  Handler = cerl:try_handler(Tree),
  
  {State1, Map1, ArgType} = traverse(Arg, Map, State),      
  Map2 = mark_as_fresh(Vars, Map1),
  ArgTypes = wrap_if_single(t_components(ArgType)),
  {SuccState, SuccMap, SuccType} =       
    case bind_pat_vars(Vars, ArgTypes, [], Map2, State1) of
      {error, _} -> 
	{State1, map__new(), t_none()};
      {SuccMap1, VarTypes} ->
	%% Try to bind the argument. Will only succeed if 
	%% it is a simple structured term.
	case bind_pat_vars_reverse([Arg], [t_product(VarTypes)], [], 
				   SuccMap1, State1) of
	  {error, _} -> SuccMap2 = SuccMap1;
	  {SuccMap2, _} -> ok
	end,
	traverse(Body, SuccMap2, State1)
    end,
  ExcMap1 = mark_as_fresh(EVars, Map),
  {State2, ExcMap2, HandlerType} = traverse(Handler, ExcMap1, SuccState),
  TryType = t_sup(SuccType, HandlerType),
  {State2, join_maps([ExcMap2, SuccMap], Map1), TryType}.

%%----------------------------------------

handle_tuple(Tree, Map, State) ->
  Elements = cerl:tuple_es(Tree),
  {State1, Map1, EsType} = traverse_list(Elements, Map, State),
  TupleType = t_tuple(EsType),
  case t_is_none(TupleType) of
    true ->
      {State1, Map1, t_none()};
    false ->
      %% Let's find out if this is a record construction.
      case Elements of
	[Tag|Left] ->
	  case cerl:is_c_atom(Tag) of
	    true ->
	      TagVal = cerl:atom_val(Tag),
	      case state__lookup_record(TagVal, length(Left), State1) of
		error -> {State1, Map1, TupleType};
		{ok, Prototype} -> 
		  InfTupleType = t_inf(Prototype, TupleType),
		  case t_is_none(InfTupleType) of
		    true ->
		      Msg = io_lib:format("Record construction ~s violates the "
					  "declared type for #~w{}\n", 
					  [format_type(TupleType, State1),
					   TagVal]),
		      State2 = state__add_warning(State1, ?WARN_MATCHING, 
						  Tree, Msg),
		      {State2, Map1, t_none()};
		    false ->
		      case bind_pat_vars(Elements, t_tuple_args(InfTupleType), 
					 [], Map1, State1) of
			{error, {ErrorPat, ErrorType}} ->
			  Msg = io_lib:format("Record construction violates"
					      " the declared type for #~w{}"
					      " since ~s cannot be of"
					      " type ~s\n", 
					      [TagVal,
					       format_patterns(ErrorPat),
					       format_type(ErrorType, State1)
					       ]),
			  State2 = state__add_warning(State1, ?WARN_MATCHING, 
						      Tree, Msg),
			  {State2, Map1, t_none()};
			{Map2, ETypes} ->
			  {State1, Map2, t_tuple(ETypes)}
		      end
		  end
	      end;
	    false ->
	      {State1, Map1, t_tuple(EsType)}
	  end;
	[] ->
	  {State1, Map1, t_tuple([])}
      end
  end.

%%________________________________________
%%
%% Clauses
%%

handle_clauses([C|Left], Arg, ArgType, OrigArgType, 
	       State, CaseTypes, MapIn, Acc) ->
  {State1, ClauseMap, BodyType, NewArgType} = 
    do_clause(C, Arg, ArgType, OrigArgType, MapIn, State),
  {NewCaseTypes, NewAcc} =
    case t_is_none(BodyType) of
      true -> {CaseTypes, Acc};
      false -> {[BodyType|CaseTypes], [ClauseMap|Acc]}
    end,
  handle_clauses(Left, Arg, NewArgType, OrigArgType, State1, 
		 NewCaseTypes, MapIn, NewAcc);
handle_clauses([], _Arg, _ArgType, _OrigArgType, State, CaseTypes, 
	       _MapIn, Acc) ->
  {lists:reverse(Acc), State, t_sup(CaseTypes)}.

do_clause(C, Arg, ArgType0, OrigArgType, Map, State) ->
  Pats = cerl:clause_pats(C),
  Guard = cerl:clause_guard(C),
  Body = cerl:clause_body(C),
  Map0 = mark_as_fresh(Pats, Map),
  ArgTypes = t_components(ArgType0),  
  
  if Arg =:= none -> Map1 = Map0;
     true ->         Map1 = bind_subst(Arg, Pats, Map0)
  end,

  case bind_pat_vars(Pats, ArgTypes, [], Map1, State) of
    {error, _} -> 
      ?debug("Failed binding pattern: ~s\nto ~s\n", 
	     [cerl_prettypr:format(C), format_type(ArgType0, State)]),
      case state__warning_mode(State) of
	false -> 
	  {State, Map, t_none(), ArgType0};
	true ->
	  PatternString = format_patterns(Pats),
	  CovMsg = io_lib:format("The ~s can never match since previous"
				 " clauses completely covered the type ~s\n",
				 [PatternString, 
				  format_type(OrigArgType, State)]),
	  FailedMsg1 = io_lib:format("The ~s can never match the type ~s\n",
				     [PatternString, 
				     format_type(OrigArgType, State)]),
	  FailedMsg2 = io_lib:format("The ~s can never match the type ~s\n",
				     [PatternString, 
				      format_type(ArgType0, State)]),

	  
	  {Msg, Force} = 
	    case t_is_none(ArgType0) of
	      true ->
		%% See if this is covered by an earlier clause or if it
		%% simply cannot match
		OrigArgTypes = t_components(OrigArgType),
		case bind_pat_vars(Pats, OrigArgTypes, [], Map1, State) of
		  {error, _} -> {FailedMsg1, false};
		  {_, _} -> {CovMsg, false}
		end;
	      false ->
		%% Try to find out if this is a default clause in a list 
		%% comprehension and supress this. A real Hack(tm)
		Force0 =
		  case is_compiler_generated(cerl:get_ann(C)) of
		    true ->
		      case Pats of
			[Pat] -> 
			  case cerl:is_c_cons(Pat) of
			    true ->
			      not (cerl:is_c_var(cerl:cons_hd(Pat)) andalso
				   cerl:is_c_var(cerl:cons_tl(Pat)) andalso
				   cerl:is_literal(Guard) andalso
				   (cerl:concrete(Guard) =:= true));
			    false ->
			      true
			  end;
			_ -> true
		      end;
		    false ->
		      true
		  end,
		{FailedMsg2, Force0}
	    end,
	  {state__add_warning(State, ?WARN_MATCHING, C, Msg, Force), 
	   Map, t_none(), ArgType0}
      end;
    {Map2, PatTypes} ->
      case Arg =:= none of
	true -> Map3 = Map2;
	false ->
	  %% Try to bind the argument. Will only succeed if 
	  %% it is a simple structured term.
	  case bind_pat_vars_reverse([Arg], [t_product(PatTypes)], [], 
				     Map2, State) of
	    {error, _} -> Map3 = Map2;
	    {Map3, _} -> ok
	  end
      end,
      NewArgType = 
	case Arg =:= none of
	  true -> ArgType0;
	  false ->
	    GenType = dialyzer_typesig:get_safe_underapprox(Pats, Guard),
	    t_subtract(t_product(wrap_if_single(ArgType0)), GenType)
	end,
      case bind_guard(Guard, Map3, State) of
	{error, Reason} -> 
	  ?debug("Failed guard: ~s\n", 
		 [cerl_prettypr:format(C, [{hook, cerl_typean:pp_hook()}])]),
	  PatternString = format_patterns(Pats),
	  DefaultMsg = 
	    case Pats =:= [] of
	      true -> "Clause guard cannot succeed.\n";
	      false ->
		io_lib:format("Clause guard cannot succeed. The ~s was matched"
			      " against the type ~s\n",
			      [PatternString, format_type(ArgType0, State)])
	    end,
	  State1 =
	    case Reason of
	      none -> state__add_warning(State, ?WARN_MATCHING, C, DefaultMsg);
	      {FailGuard, Msg} ->
		case is_compiler_generated(cerl:get_ann(FailGuard)) of
		  false -> 
		    state__add_warning(State, ?WARN_MATCHING, FailGuard, Msg);
		  true ->
		    state__add_warning(State, ?WARN_MATCHING, C, Msg)
		end
	    end,
	  {State1, Map, t_none(), NewArgType};
	Map4 ->
	  {RetState, RetMap, BodyType} = traverse(Body, Map4, State),
	  {RetState, RetMap, BodyType, NewArgType}
      end
  end.

bind_subst(Arg, Pats, Map) ->
  case cerl:type(Arg) of
    values -> 
      bind_subst_list(cerl:values_es(Arg), Pats, Map);
    var ->
      [Pat] = Pats,
      enter_subst(Arg, Pat, Map);
    _ ->
      Map
  end.

bind_subst_list([Arg|ArgLeft], [Pat|PatLeft], Map) ->
  NewMap =
    case {cerl:type(Arg), cerl:type(Pat)} of
      {var, var} ->         enter_subst(Arg, Pat, Map);
      {var, alias} ->       enter_subst(Arg, cerl:alias_pat(Pat), Map);
      {literal, literal} -> Map;
      {T, T} ->             bind_subst_list(lists:flatten(cerl:subtrees(Arg)),
					    lists:flatten(cerl:subtrees(Pat)),
					    Map);
      _ ->                  Map
    end,
  bind_subst_list(ArgLeft, PatLeft, NewMap);
bind_subst_list([], [], Map) ->
  Map.

%%________________________________________
%%
%% Patterns
%%

bind_pat_vars(Pats, Types, Acc, Map, State) ->
  try 
    bind_pat_vars(Pats, Types, Acc, Map, State, false)
  catch 
    throw:{bind_error, ErrorPats, ErrorType} -> {error, {ErrorPats, ErrorType}}
  end.

bind_pat_vars_reverse(Pats, Types, Acc, Map, State) ->
  try 
    bind_pat_vars(Pats, Types, Acc, Map, State, true)
  catch 
    throw:{bind_error, ErrorPats, ErrorType} -> {error, {ErrorPats, ErrorType}}
  end.

bind_pat_vars([Pat|PatLeft], [Type|TypeLeft], Acc, Map, State, Rev) ->
  ?debug("Binding pat: ~w to ~s\n", [cerl:type(Pat), format_type(Type, State)]),
  {NewMap, TypeOut} =
    case cerl:type(Pat) of
      alias ->
	AliasPat = cerl:alias_pat(Pat),
	Var = cerl:alias_var(Pat),
	Map1 = enter_subst(Var, AliasPat, Map),
	{Map2, [PatType]} = bind_pat_vars([AliasPat], [Type], [], 
					  Map1, State, Rev),
	{enter_type(Var, PatType, Map2), PatType};
      binary ->
	%% Cannot bind the binary if we are in reverse match since
	%% binary patterns and binary construction are not symetric.
	case Rev of
	  true -> {Map, t_bitstr()};
	  false ->
	    BinType = t_inf(t_bitstr(), Type),
	    case t_is_none(BinType) of
	      true -> bind_error([Pat], Type);
	      false ->
		Segs = cerl:binary_segments(Pat),
		{Map1, SegTypes} = bind_bin_segs(Segs, BinType, Map, State),
		{Map1, t_bitstr_concat(SegTypes)}
	    end
	end;
      cons ->
	Cons = t_inf(Type, t_cons()),
	case t_is_none(Cons) of
	  true -> bind_error([Pat], Type);
	  false ->
	    {Map1, [HdType, TlType]} = 
	      bind_pat_vars([cerl:cons_hd(Pat), cerl:cons_tl(Pat)],
			    [t_cons_hd(Cons), t_cons_tl(Cons)], 
			    [], Map, State, Rev),
	    {Map1, t_cons(HdType, TlType)}
	end;
      literal ->
	Literal = literal_type(Pat),
	case t_is_none(t_inf(Literal, Type)) of
	  true -> bind_error([Pat], Type);
	  false -> {Map, Literal}
	end;
      tuple ->	
	Es = cerl:tuple_es(Pat),
	Prototype = 
	  case Es of
	    [] -> t_tuple([]);
	    [Tag|Left] ->
	      case cerl:is_c_atom(Tag) of
		true ->
		  TagAtom = cerl:atom_val(Tag),
		  case state__lookup_record(TagAtom, length(Left), State) of
		    error -> t_tuple(length(Es));
		    {ok, Record} -> Record
		  end;
		false -> t_tuple(length(Es))
	      end
	  end,
	Tuple = t_inf(Prototype, Type),
	case t_is_none(Tuple) of
	  true -> bind_error([Pat], Type);
	  false ->
	    SubTuples = t_tuple_subtypes(Tuple),
	    %% Need to call the top function to get the try-catch wrapper
	    Results = 
	      case Rev of
		true ->
		  [bind_pat_vars_reverse(Es, t_tuple_args(SubTuple), [], 
					 Map, State)
		   || SubTuple <- SubTuples];
		false ->
		  [bind_pat_vars(Es, t_tuple_args(SubTuple), [], Map, State)
		   || SubTuple <- SubTuples]
	      end,
	    case [M || {M, _} <- Results, M =/= error] of
	      [] -> bind_error([Pat], Tuple);
	      Maps ->
		Map1 = join_maps(Maps, Map),
		TupleType = t_sup([t_tuple(EsTypes)
				   || {M, EsTypes} <- Results, M =/= error]),
		{Map1, TupleType}
	    end
	end;
      values ->
	Es = cerl:values_es(Pat),
	{Map1, EsTypes} =  
	  bind_pat_vars(Es, t_components(Type), [], Map, State, Rev),
	{Map1, t_product(EsTypes)};
      var ->
	VarType1 =
	  case state__lookup_type_for_rec_var(Pat, State) of
	    error -> lookup_type(Pat, Map);
	    {ok, RecType} -> RecType
	  end,
	%% Must do inf when binding args to pats. Vars in pats are fresh.
	VarType2 = t_inf(VarType1, Type),
	case t_is_none(VarType2) of
	  true -> bind_error([Pat], Type);
	  false ->
	    Map1 = enter_type(Pat, VarType2, Map),
	    {Map1, VarType2}
	end;
      _Other ->
	%% Catch all is needed when binding args to pats
	?debug("Failed match for ~p\n", [_Other]),
	bind_error([Pat], Type)
    end,
  bind_pat_vars(PatLeft, TypeLeft, [TypeOut|Acc], NewMap, State, Rev);
bind_pat_vars([], [], Acc, Map, _State, _Rev) ->
  {Map, lists:reverse(Acc)};
bind_pat_vars(Pats, Type, Acc, Map, State, Rev) ->
  case t_is_any(Type) of
    true -> 
      AnyList = lists:duplicate(length(Pats), t_any()),
      bind_pat_vars(Pats, AnyList, Acc, Map, State, Rev);
    false ->
      case t_is_none_or_unit(Type) of
	true -> bind_error(Pats, Type);
	false -> erlang:error({'Error binding pats', Pats, Type})
      end
  end.

bind_bin_segs(BinSegs, BinType, Map, State) ->
  bind_bin_segs(BinSegs, BinType, [], Map, State).
	  
bind_bin_segs([Bitstr|Left], BinType, Acc, Map, State) ->
  Size = cerl:bitstr_size(Bitstr),
  Val = cerl:bitstr_val(Bitstr),
  BitstrType = cerl:concrete(cerl:bitstr_type(Bitstr)),
  UnitVal = cerl:concrete(cerl:bitstr_unit(Bitstr)),
  case cerl:is_c_atom(Size) andalso (cerl:concrete(Size) =:= all) of
    true ->
      %% Just an assert
      binary = BitstrType, [] = Left,
      {Map1, [Type]} = bind_pat_vars([Val], 
				     [t_inf(t_bitstr(UnitVal, 0), BinType)], 
				     [], Map, State, false),
      bind_bin_segs(Left, t_bitstr(0,0), [Type|Acc], Map1, State);
    false ->
      {Map1, [SizeType]} = 
	bind_pat_vars([Size], [t_non_neg_integer()], [], Map, State, false),
      Type =
	case t_number_vals(SizeType) of
	  [OneSize] -> t_bitstr(0, UnitVal * OneSize);
	  _ ->
	    MinSize = erl_types:number_min(SizeType),
	    t_bitstr(UnitVal, UnitVal * MinSize)
	end,
      ValConstr =
	case BitstrType of
	  float -> t_float();
	  binary -> Type; %% The same constraints as for the whole bitstr
	  integer -> 
	    case t_number_vals(SizeType) of
	      any -> t_integer();
	      List ->
		SizeVal = lists:max(List),
		Flags = cerl:concrete(cerl:bitstr_flags(Bitstr)),
		case lists:member(signed, Flags) of
		  true -> t_from_range(-(1 bsl (SizeVal*UnitVal - 1)), 
				       1 bsl (SizeVal*UnitVal - 1) - 1);
		  false -> t_from_range(0, 1 bsl SizeVal*UnitVal - 1)
		end
	    end
	end,
      {Map2, [_]} = bind_pat_vars([Val], [ValConstr], [], Map1, State, false),
      NewBinType = t_bitstr_match(Type, BinType),
      case t_is_none(NewBinType) of
	true -> bind_error([Bitstr], BinType);
	false -> bind_bin_segs(Left, NewBinType, [Type|Acc], Map2, State)
      end
  end;
bind_bin_segs([], _BinType, Acc, Map, _State) ->
  {Map, lists:reverse(Acc)}.

bind_error(Pats, Type) ->
  throw({bind_error, Pats, Type}).
  
%%________________________________________
%%
%% Guards
%%

bind_guard(Guard, Map, State) ->
  try bind_guard(Guard, Map, dict:new(), pos, State) of
      {Map1, _Type} -> Map1
  catch
    throw:{fail, Warning} -> {error, Warning};
    throw:{fatal_fail, Warning} -> {error, Warning}
  end.

bind_guard(Guard, Map, Env, Eval, State) ->
  ?debug("Handling ~w guard: ~s\n", 
	 [Eval, cerl_prettypr:format(Guard, [{noann, true}])]),
  case cerl:type(Guard) of
    binary -> 
      {Map, t_binary()};
    'case' ->
      Arg = cerl:case_arg(Guard),
      Clauses = cerl:case_clauses(Guard),
      bind_guard_case_clauses(Arg, Clauses, Map, Env, Eval, State);
    cons ->
      Hd = cerl:cons_hd(Guard),
      Tl = cerl:cons_tl(Guard),
      {Map1, HdType} = bind_guard(Hd, Map, Env, dont_know, State),
      {Map2, TlType} = bind_guard(Tl, Map1, Env, dont_know, State),
      {Map2, t_cons(HdType, TlType)};
    literal ->
      {Map, literal_type(Guard)};
    'try' ->
      Arg = cerl:try_arg(Guard),
      [Var] = cerl:try_vars(Guard),
      %%?debug("Storing: ~w\n", [Var]),
      NewEnv = dict:store(get_label(Var), Arg, Env),
      bind_guard(cerl:try_body(Guard), Map, NewEnv, Eval, State);
    tuple ->
      Es0 = cerl:tuple_es(Guard),
      {Map1, Es} = bind_guard_list(Es0, Map, Env, dont_know, State),
      {Map1, t_tuple(Es)};
    'let' ->
      Arg = cerl:let_arg(Guard),
      [Var] = cerl:let_vars(Guard),
      %%?debug("Storing: ~w\n", [Var]),
      NewEnv = dict:store(get_label(Var), Arg, Env),
      bind_guard(cerl:let_body(Guard), Map, NewEnv, Eval, State);
    values ->
      Es = cerl:values_es(Guard),
      List = [bind_guard(V, Map, Env, dont_know, State) || V <- Es],
      Type = t_product([T || {_, T} <- List]),
      {Map, Type};
    var ->
      ?debug("Looking for var(~w)...", [cerl_trees:get_label(Guard)]),
      case dict:find(get_label(Guard), Env) of
	error -> 
	  ?debug("Did not find it\n", []),
	  Type = lookup_type(Guard, Map),
	  Constr = 
	    case Eval of
	      pos -> t_from_term(true);
	      neg -> t_from_term(false);
	      dont_know -> Type
	    end,
	  Inf = t_inf(Constr, Type),
	  {enter_type(Guard, Inf, Map), Inf};
	{ok, Tree} -> 
	  ?debug("Found it\n", []),
	  {Map1, Type} = bind_guard(Tree, Map, Env, Eval, State),
	  {enter_type(Guard, Type, Map1), Type}
      end;
    call ->
      Args = cerl:call_args(Guard),      
      MFA = {cerl:atom_val(cerl:call_module(Guard)),
	     cerl:atom_val(cerl:call_name(Guard)),
	     length(Args)},
      case MFA of
	{erlang, F, 1} when F =:= is_atom; F =:= is_boolean;
			    F =:= is_binary; F =:= is_bitstring;
			    F =:= is_float; F =:= is_function;
			    F =:= is_integer; F =:= is_list;
			    F =:= is_number; F =:= is_pid; F =:= is_port;
			    F =:= is_reference; F =:= is_tuple ->
	  [Arg] = Args,
	  {Map1, ArgType} = bind_guard(Arg, Map, Env, dont_know, State),
	  case bind_type_test(Eval, F, ArgType) of
	    error -> 
	      ?debug("Type test: ~w failed\n", [F]),
	      signal_guard_fail(Guard, [ArgType], State);
	    {ok, NewArgType, Ret} -> 
	      ?debug("Type test: ~w succeeded, NewType: ~s, Ret: ~s\n", 
		     [F, t_to_string(NewArgType), t_to_string(Ret)]),
	      {enter_type(Arg, NewArgType, Map1), Ret}
	  end;
	{erlang, is_function, 2} ->
	  {Map1, [FunType0, ArityType0]} = 
	    bind_guard_list(Args, Map, Env, dont_know, State),
	  ArityType = t_inf(ArityType0, t_integer()),
	  case t_is_none(ArityType) of
	    true -> signal_guard_fail(Guard, [FunType0, ArityType0], State);
	    false ->
	      FunTypeConstr =
		case t_number_vals(ArityType) of
		  any -> t_fun();
		  Vals ->
		    t_sup([t_fun(lists:duplicate(X, t_any()), t_any()) || X <- Vals])
		end,
	      FunType = t_inf(FunType0, FunTypeConstr),
	      case t_is_none(FunType) of
		true -> 
		  case Eval of
		    pos -> signal_guard_fail(Guard, [FunType0, ArityType0], 
					     State);
		    neg -> {Map1, t_atom(false)};
		    dont_know -> {Map1, t_atom(false)}
		  end;
		false ->
		  case Eval of
		    pos -> {enter_type_lists(Args, [FunType, ArityType], Map1),
			    t_atom(true)};
		    neg -> {Map1, t_atom(false)};
		    dont_know -> {Map1, t_bool()}
		  end
	      end
	  end;
	MFA when (MFA =:= {erlang, internal_is_record, 3}) or 
		 (MFA =:= {erlang, is_record, 3}) ->
	  [Rec, Tag0, Arity0] = Args,
	  Tag = cerl:atom_val(Tag0),
	  Arity = cerl:int_val(Arity0),
	  {Map1, RecType} = bind_guard(Rec, Map, Env, dont_know, State),
	  ArityMin1 = Arity - 1,
	  TupleType =
	    case state__lookup_record(Tag, ArityMin1, State) of
	      error ->
		t_tuple([t_atom(Tag)|lists:duplicate(ArityMin1, t_any())]);
	      {ok, Prototype} -> Prototype
	    end,
	  Type = t_inf(TupleType, RecType),
	  case t_is_none(Type) of
	    true -> 
	      case Eval of
		pos -> signal_guard_fail(Guard, 
					 [RecType, t_from_term(Tag), 
					  t_from_term(Arity)],
					 State);
		neg -> {Map1, t_atom(false)};
		dont_know -> {Map1, t_atom(false)}
	      end;
	    false -> 
	      case Eval of
		pos -> {enter_type(Rec, Type, Map1), t_atom(true)};
		neg -> {Map1, t_atom(false)};
		dont_know -> {Map1, t_bool()}
	      end
	  end;
	{erlang, '=:=', 2} ->
	  [Arg1, Arg2] = Args,
	  case {cerl:type(Arg1), cerl:type(Arg2)} of
	    {literal, literal} ->
	      case cerl:concrete(Arg1) =:= cerl:concrete(Arg2) of
		true ->
		  if Eval =:= neg -> throw({fail, none});
		     Eval =:= pos -> {Map, t_from_term(true)};
		     Eval =:= dont_know -> {Map, t_from_term(true)}
		  end;
		false ->
		  if Eval =:= neg -> {Map, t_from_term(false)};
		     Eval =:= dont_know -> {Map, t_from_term(false)};
		     Eval =:= pos -> 
 		      ArgTypes = [t_from_term(cerl:concrete(Arg1)),
				  t_from_term(cerl:concrete(Arg2))],
		      signal_guard_fail(Guard, ArgTypes, State)
		  end
	      end;
	    {literal, _} when Eval =:= pos ->
	      bind_eqeq_guard_lit_other(Guard, Arg1, Arg2, Map, Env, State);
	    {_, literal} when Eval =:= pos ->
	      bind_eqeq_guard_lit_other(Guard, Arg2, Arg1, Map, Env, State);
	    {_, _} ->
	      bind_eqeq_guard(Guard, Arg1, Arg2, Map, Env, Eval, State)
	  end;
	{erlang, '==', 2} ->
	  [Arg1, Arg2] = Args,
	  case {cerl:type(Arg1), cerl:type(Arg2)} of
	    {literal, literal} ->
	      case cerl:concrete(Arg1) == cerl:concrete(Arg2) of
		true -> 
		  if 
		    Eval =:= pos -> {Map, t_from_term(true)};
		    Eval =:= neg -> throw({fail, none});
		    Eval =:= dont_know -> {Map, t_from_term(true)}
		  end;
		false ->
		  if 
		    Eval =:= neg -> {Map, t_from_term(false)};
		    Eval =:= dont_know -> {Map, t_from_term(false)};
		    Eval =:= pos -> 
		      ArgTypes = [t_from_term(cerl:concrete(Arg1)),
				  t_from_term(cerl:concrete(Arg2))],
		      signal_guard_fail(Guard, ArgTypes, State)
		  end
	      end;
	    {literal, _} when Eval =:= pos ->
	      case cerl:concrete(Arg1) of
		Atom when is_atom(Atom) ->
		  bind_eqeq_guard_lit_other(Guard, Arg1, Arg2, Map, Env, State);
		[] ->
		  bind_eqeq_guard_lit_other(Guard, Arg1, Arg2, Map, Env, State);
		_ ->
		  bind_eq_guard(Guard, Arg1, Arg2, Map, Env, Eval, State)
	      end;
	    {_, literal} when Eval =:= pos ->
	      case cerl:concrete(Arg2) of
		Atom when is_atom(Atom) ->
		  bind_eqeq_guard_lit_other(Guard, Arg2, Arg1, Map, Env, State);
		[] ->
		  bind_eqeq_guard_lit_other(Guard, Arg2, Arg1, Map, Env, State);
		_ ->
		  bind_eq_guard(Guard, Arg1, Arg2, Map, Env, Eval, State)
	      end;
	    {_, _} ->
	      bind_eq_guard(Guard, Arg1, Arg2, Map, Env, Eval, State)
	  end;
	{erlang, 'and', 2} ->
	  [Arg1, Arg2] = Args,
	  case Eval of
	    pos ->
	      {Map1, Type1} = bind_guard(Arg1, Map, Env, Eval, State),
	      case t_is_atom(true, Type1) of
		false -> throw({fail, none});
		true ->
		  {Map2, Type2} = bind_guard(Arg2, Map1, Env, Eval, State),
		  case t_is_atom(true, Type2) of
		    false -> throw({fail, none});
		    true -> {Map2, t_atom(true)}
		  end
	      end;
	    neg ->
	      {Map1, Type1} = 
		try bind_guard(Arg1, Map, Env, neg, State)
		catch throw:{fail, _} -> bind_guard(Arg2, Map, Env, pos, State)
		end,
	      {Map2, Type2} = 
		try bind_guard(Arg1, Map, Env, neg, State)
		catch throw:{fail, _} -> bind_guard(Arg2, Map, Env, pos, State)
		end,
	      case t_is_atom(false, Type1) orelse t_is_atom(false, Type2) of
		true -> {join_maps([Map1, Map2], Map), t_from_term(false)};
		false -> throw({fail, none})
	      end;
	    dont_know ->
	      True = t_atom(true),
	      {Map1, Type1} = bind_guard(Arg1, Map, Env, dont_know, State),
	      case t_is_none(t_inf(Type1, t_bool())) of
		true -> throw({fail, none});
		false ->
		  {Map2, Type2} = bind_guard(Arg2, Map1, Env, Eval, State),
		  case t_is_none(t_inf(Type2, t_bool())) of
		    true -> throw({fail, none});
		    false -> {Map2, True}
		  end
	      end
	  end;
	{erlang, 'or', 2} ->
	  [Arg1, Arg2] = Args,
	  case Eval of
	    pos ->
	      {Map1, Bool1} = 
		try bind_guard(Arg1, Map, Env, pos, State)
		catch 
		  throw:{fail,_} -> bind_guard(Arg1, Map, Env, dont_know, State)
		end,
	      {Map2, Bool2} = 
		try bind_guard(Arg2, Map, Env, pos, State)
		catch 
		  throw:{fail,_} -> bind_guard(Arg2, Map, Env, dont_know, State)
		end,
	      case ((t_is_atom(true, Bool1) andalso t_is_bool(Bool2))
		    orelse 
		    (t_is_atom(true, Bool2) andalso t_is_bool(Bool1))) of
		true -> {join_maps([Map1, Map2], Map), t_from_term(true)};
		false -> throw({fail, none})
	      end;
	    neg ->
	      {Map1, Type1} = bind_guard(Arg1, Map, Env, neg, State),
	      case t_is_atom(true, Type1) of
		false -> throw({fail, none});
		true ->
		  {Map2, Type2} = bind_guard(Arg2, Map1, Env, neg, State),
		  case t_is_atom(true, Type2) of
		    false -> throw({fail, none});
		    true -> {Map2, t_atom(false)}
		  end
	      end;
	    dont_know ->
	      {Map1, Bool1} = bind_guard(Arg1, Map, Env, dont_know, State),
	      {Map2, Bool2} = bind_guard(Arg2, Map, Env, dont_know, State),
	      case t_is_bool(Bool1) andalso t_is_bool(Bool2) of
		true -> {join_maps([Map1, Map2], Map), t_sup(Bool1, Bool2)};
		false -> throw({fail, none})
	      end
	  end;
	{erlang, 'not', 1} ->
	  [Arg] = Args,
	  case Eval of
	    neg -> 
	      {Map1, Type} = bind_guard(Arg, Map, Env, pos, State),
	      case t_is_atom(true, Type) of
		true -> {Map1, t_atom(false)};
		false -> throw({fail, none})
	      end;
	    pos -> 
	      {Map1, Type} = bind_guard(Arg, Map, Env, neg, State),
	      case t_is_atom(false, Type) of
		true -> {Map1, t_atom(true)};
		false -> throw({fail, none})
	      end;
	    dont_know -> 
	      {Map1, Type} = bind_guard(Arg, Map, Env, dont_know, State),
	      Bool = t_inf(Type, t_bool()),
	      case t_is_none(Bool) of
		true -> throw({fatal_fail, none});
		false ->
		  case t_atom_vals(Bool) of
		    [true] -> {Map1, t_from_term(false)};
		    [false] -> {Map1, t_from_term(true)};
		    [_, _] -> {Map1, Bool}
		  end
	      end
	  end;
	{M, F, A} ->
	  {Map1, As} = bind_guard_list(Args, Map, Env, dont_know, State),
	  BifRet = erl_bif_types:type(M, F, A, As),
	  case t_is_none(BifRet) of
	    true ->
	      %% Is this an error-bif?
	      case t_is_none(erl_bif_types:type(M, F, A)) of
		true -> signal_guard_fail(Guard, As, State);
		false -> signal_guard_fatal_fail(Guard, As, State)
	      end;
	    false ->
	      case erl_bif_types:arg_types(M, F, A) of
		any -> BifArgs = lists:duplicate(A, t_any());
		List -> BifArgs = List
	      end,
	      Map2 = enter_type_lists(Args, t_inf_lists(BifArgs, As), Map1),
	      Ret = 
		case Eval of
		  pos -> t_inf(t_from_term(true), BifRet);
		  neg -> t_inf(t_from_term(false), BifRet);
		  dont_know -> BifRet
		end,
	      case t_is_none(Ret) of
		true ->
		  case Eval =:= pos of
		    true -> signal_guard_fail(Guard, As, State);
		    false -> throw({fail, none})
		  end;
		false -> {Map2, Ret}
	      end
	  end
      end
  end.

bind_type_test(Eval, TypeTest, ArgType) ->
  Type = case TypeTest of
	   is_atom -> t_atom();
	   is_boolean -> t_bool();
	   is_binary -> t_binary();
	   is_bitstring -> t_bitstr();
	   is_float -> t_float();
	   is_function -> t_fun();
	   is_integer -> t_integer();
	   is_list -> t_maybe_improper_list();
	   is_number -> t_number();
	   is_pid -> t_pid();
	   is_port -> t_port();
	   is_reference -> t_ref();
	   is_tuple -> t_tuple()
	 end,
  case Eval of
    pos ->
      Inf = t_inf(Type, ArgType),
      case t_is_none(Inf) of
	true -> error;
	false -> {ok, Inf, t_from_term(true)}
      end;
    neg ->
      Sub = t_subtract(ArgType, Type),
      case t_is_none(Sub) of
	true -> error;
	false -> {ok, Sub, t_from_term(false)}
      end;
    dont_know ->
      {ok, ArgType, t_bool()}
  end.

bind_eq_guard(Guard, Arg1, Arg2, Map, Env, Eval, State) ->
  {Map1, Type1} = bind_guard(Arg1, Map, Env, dont_know, State),
  {Map2, Type2} = bind_guard(Arg2, Map1, Env, dont_know, State),
  case (t_is_nil(Type1) orelse t_is_nil(Type2) orelse
	t_is_atom(Type1) orelse t_is_atom(Type2)) of
    true -> bind_eqeq_guard(Guard, Arg1, Arg2, Map, Env, Eval, State);
    false ->
      case Eval of
	pos -> {Map2, t_from_term(true)};
	neg -> {Map2, t_from_term(false)};
	dont_know -> {Map2, t_bool()}
      end
  end.

bind_eqeq_guard(Guard, Arg1, Arg2, Map, Env, Eval, State) ->
  {Map1, Type1} = bind_guard(Arg1, Map, Env, dont_know, State),
  {Map2, Type2} = bind_guard(Arg2, Map1, Env, dont_know, State),
  ?debug("Types are:~s =:= ~s\n", [t_to_string(Type1), 
				   t_to_string(Type2)]),
  Inf = t_inf(Type1, Type2),
  case t_is_none(Inf) of
    true ->
      case Eval of
	neg -> {Map2, t_from_term(false)};
	dont_know -> {Map2, t_from_term(false)};
	pos -> signal_guard_fail(Guard, [Type1, Type2], State)
      end;
    false ->
      case Eval of
	pos ->
	  case {cerl:type(Arg1), cerl:type(Arg2)} of
	    {var, var} ->
	      Map3 = enter_subst(Arg1, Arg2, Map2),
	      Map4 = enter_type(Arg2, Inf, Map3),
	      {Map4, t_atom(true)};
	    {var, _} ->
	      Map3 = enter_type(Arg1, Inf, Map2),
	      {Map3, t_atom(true)};
	    {_, var} ->
	      Map3 = enter_type(Arg2, Inf, Map2),
	      {Map3, t_atom(true)};
	    {_, _} ->
	      {Map2, t_atom(true)}
	  end;
	neg ->
	  {Map2, t_from_term(false)};
	dont_know ->
	  {Map2, t_bool()}
      end
  end.

bind_eqeq_guard_lit_other(Guard, Arg1, Arg2, Map, Env, State) ->
  %% Assumes positive evaluation
  case cerl:concrete(Arg1) of
    true ->
      {Map1, Type} = bind_guard(Arg2, Map, Env, pos, State),
      case t_is_atom(true, Type) of
	true -> {Map1, Type};
	false -> 
	  {_, Type0} = bind_guard(Arg2, Map, Env, dont_know, State),
	  signal_guard_fail(Guard, [Type0, t_from_term(true)], State)
      end;
    false -> 
      {Map1, Type} = bind_guard(Arg2, Map, Env, neg, State),
      case t_is_atom(false, Type) of
	true -> {Map1, t_atom(true)};
	false -> 
	  {_, Type0} = bind_guard(Arg2, Map, Env, dont_know, State),
	  signal_guard_fail(Guard, [Type0, t_from_term(true)], State)
      end;
    Term ->
      LitType = t_from_term(Term),
      {Map1, Type} = bind_guard(Arg2, Map, Env, dont_know, State),
      case t_is_subtype(LitType, Type) of
	false -> signal_guard_fail(Guard, [Type, LitType], State);
	true ->
	  case cerl:is_c_var(Arg2) of
	    true -> {enter_type(Arg2, LitType, Map1), t_from_term(true)};
	    false -> {Map1, t_from_term(true)}
	  end
      end
  end.

bind_guard_list(Guards, Map, Env, Eval, State) ->
  bind_guard_list(Guards, Map, Env, Eval, State, []).

bind_guard_list([G|Left], Map, Env, Eval, State, Acc) ->
  {Map1, T} = bind_guard(G, Map, Env, Eval, State),
  bind_guard_list(Left, Map1, Env, Eval, State, [T|Acc]);
bind_guard_list([], Map, _Env, _Eval, _State, Acc) ->
  {Map, lists:reverse(Acc)}.

-spec(signal_guard_fail/3 :: (_, [_], #state{}) -> no_return()).

signal_guard_fail(Guard, ArgTypes, State) ->
  Args = cerl:call_args(Guard),
  F = cerl:atom_val(cerl:call_name(Guard)),
  MFA = {cerl:atom_val(cerl:call_module(Guard)), F, length(Args)},
  Msg =
    case is_infix_op(MFA) of
      true -> 
	[ArgType1, ArgType2] = ArgTypes,
	[Arg1, Arg2] = Args,
	io_lib:format("Guard test ~s ~s ~s can never succeed.\n",
		      [format_args_1([Arg1], [ArgType1], State), 
		       atom_to_list(F),
		       format_args_1([Arg2], [ArgType2], State)]);
      false ->
	io_lib:format("Guard test ~w~s can never succeed\n", 
		      [F, format_args(Args, ArgTypes, State)])
    end,
  throw({fail, {Guard, Msg}}).

is_infix_op({erlang, '=:=', 2}) -> true;
is_infix_op({erlang, '==', 2}) -> true;
is_infix_op({erlang, '=/=', 2}) -> true;
is_infix_op({erlang, '=/', 2}) -> true;
is_infix_op({erlang, '<', 2}) -> true;
is_infix_op({erlang, '=<', 2}) -> true;
is_infix_op({erlang, '>', 2}) -> true;
is_infix_op({erlang, '>=', 2}) -> true;
is_infix_op({M, F, A}) when is_atom(M), is_atom(F),
			    is_integer(A), 0 =< A, A =< 255 -> false.

-spec(signal_guard_fatal_fail/3 :: (_, [_], #state{}) -> no_return()).

signal_guard_fatal_fail(Guard, ArgTypes, State) ->
  Args = cerl:call_args(Guard),      
  F = cerl:atom_val(cerl:call_name(Guard)),
  Msg = io_lib:format("Guard test ~w~s can never succeed\n",
		      [F, format_args(Args, ArgTypes, State)]),
  throw({fatal_fail, {Guard, Msg}}).

bind_guard_case_clauses(Arg, Clauses, Map, Env, Eval, State) ->
  Clauses1 = filter_fail_clauses(Clauses),
  {GenMap, GenArgType} = bind_guard(Arg, Map, Env, dont_know, State),
  bind_guard_case_clauses(GenArgType, GenMap, Arg, Clauses1, Map, Env, Eval, 
			  t_none(), [], State).

filter_fail_clauses([Clause|Left]) ->
  case (cerl:clause_pats(Clause) =:= []) of
    true ->
      Body = cerl:clause_body(Clause),
      case cerl:is_literal(Body) andalso (cerl:concrete(Body) =:= fail) of
	true -> filter_fail_clauses(Left);
	false -> [Clause|filter_fail_clauses(Left)]
      end;
    false ->
      [Clause|filter_fail_clauses(Left)]
  end;
filter_fail_clauses([]) ->
  [].

bind_guard_case_clauses(GenArgType, GenMap, ArgExpr, [Clause|Left], 
			Map, Env, Eval, AccType, AccMaps, State) ->
  Pats = cerl:clause_pats(Clause),
  {NewMap0, ArgType} =
    case Pats of
      [Pat] ->
	case cerl:is_literal(Pat) of
	  true ->
	    try
	      case cerl:concrete(Pat) of
		true -> bind_guard(ArgExpr, Map, Env, pos, State);
		false -> bind_guard(ArgExpr, Map, Env, neg, State);
		_ -> {GenMap, GenArgType}
	      end
	    catch 
	      throw:{fail, _} -> {none, GenArgType}
	    end;
	  false ->
	    {GenMap, GenArgType}
	end;
      _ -> {GenMap, GenArgType}
    end,
  NewMap1 =
    case Pats =:= [] of
      true -> NewMap0;
      false ->
	case bind_pat_vars(Pats, t_components(ArgType), [], NewMap0, State) of
	  {error, _} -> none;
	  {PatMap, _PatTypes} -> PatMap
	end
    end,
  Guard = cerl:clause_guard(Clause),
  GenPatType = dialyzer_typesig:get_safe_underapprox(Pats, Guard),
  NewGenArgType = t_subtract(GenArgType, GenPatType),
  case (NewMap1 =:= none) orelse t_is_none(GenArgType) of
    true ->
      bind_guard_case_clauses(NewGenArgType, GenMap, ArgExpr, Left, Map, Env, 
			      Eval, AccType, AccMaps, State);
    false ->
      {NewAccType, NewAccMaps} =
	try
	  {NewMap2, GuardType} = bind_guard(Guard, NewMap1, Env, pos, State),
	  case t_is_none(t_inf(t_from_term(true), GuardType)) of
	    true -> throw({fail, none});
	    false -> ok
	  end,
	  {NewMap3, CType} = bind_guard(cerl:clause_body(Clause), NewMap2, 
					Env, Eval, State),
	  case Eval of
	    pos -> 
	      case t_is_atom(true, CType) of
		true -> ok;
		false -> throw({fail, none})
	      end;
	    neg -> 
	      case t_is_atom(false, CType) of
		true -> ok;
		false -> throw({fail, none})
	      end;
	    dont_know ->
	      ok
	  end,
	  {t_sup(AccType, CType), [NewMap3|AccMaps]}
	catch
	  throw:{fail, _What} -> {AccType, AccMaps}
	end,
      bind_guard_case_clauses(NewGenArgType, GenMap, ArgExpr, Left, Map, Env, 
			      Eval, NewAccType, NewAccMaps, State)
  end;
bind_guard_case_clauses(_GenArgType, _GenMap, _ArgExpr, [], Map, _Env, _Eval, 
			AccType, AccMaps, _State) ->
  case t_is_none(AccType) of
    true -> throw({fail, none});
    false -> {join_maps(AccMaps, Map), AccType}
  end.

%%% ===========================================================================
%%%
%%%  Maps and types.
%%%
%%% ===========================================================================

map__new() ->
  {dict:new(), dict:new()}.

join_maps(Maps, MapOut) ->
  {Map, Subst} = MapOut,
  Keys = ordsets:from_list(dict:fetch_keys(Map) ++ dict:fetch_keys(Subst)),
  join_maps(Keys, Maps, MapOut).

join_maps([Key|Left], Maps, MapOut) ->
  Type = join_maps_one_key(Maps, Key, t_none()),
  case t_is_equal(lookup_type(Key, MapOut), Type) of
    true ->  join_maps(Left, Maps, MapOut);
    false -> join_maps(Left, Maps, enter_type(Key, Type, MapOut))
  end;
join_maps([], _Maps, MapOut) ->
  MapOut.

join_maps_one_key([Map|Left], Key, AccType) ->
  case t_is_any(AccType) of
    true ->
      %% We can stop here
      AccType;
    false ->
      join_maps_one_key(Left, Key, t_sup(lookup_type(Key, Map), AccType))
  end;
join_maps_one_key([], _Key, AccType) ->
  AccType.

enter_type_lists([Key|KeyTail], [Val|ValTail], Map) ->
  Map1 = enter_type(Key, Val, Map),
  enter_type_lists(KeyTail, ValTail, Map1);
enter_type_lists([], [], Map) ->
  Map.

enter_type_list([{Key, Val}|Left], Map) ->
  Map1 = enter_type(Key, Val, Map),
  enter_type_list(Left, Map1);
enter_type_list([], Map) ->
  Map.

enter_type(Key, Val, MS = {Map, Subst}) ->  
  case cerl:is_literal(Key) of
    true -> MS;
    false ->
      case cerl:is_c_values(Key) of
	true ->
	  Keys = cerl:values_es(Key),
	  case t_is_any(Val) orelse t_is_none(Val) of
	    true ->
	      enter_type_lists(Keys, lists:duplicate(length(Keys), Val), MS);
	    false ->
	      enter_type_lists(cerl:values_es(Key), t_components(Val), MS)
	  end;
	false ->
	  KeyLabel = get_label(Key),
	  case dict:find(KeyLabel, Subst) of
	    {ok, NewKey} ->
	      ?debug("Binding ~p to ~p\n", [KeyLabel, NewKey]),
	      enter_type(NewKey, Val, MS);
	    error ->
	      ?debug("Entering ~p :: ~s\n", [KeyLabel, t_to_string(Val)]),
	      case dict:find(KeyLabel, Map) of
		{ok, Val} -> MS;
		{ok, _OldVal} -> {dict:store(KeyLabel, Val, Map), Subst};
		error -> {dict:store(KeyLabel, Val, Map), Subst}
	      end
	  end
      end
  end.

enter_subst(Key, Val, MS = {Map, Subst}) ->
  KeyLabel = get_label(Key),
  case cerl:is_literal(Val) of
    true -> 
      NewMap = dict:store(KeyLabel, literal_type(Val), Map),
      {NewMap, Subst};
    false ->
      case cerl:is_c_var(Val) of
	false -> MS;
	true ->
	  ValLabel = get_label(Val),
	  case dict:find(ValLabel, Subst) of
	    {ok, NewVal} ->
	      enter_subst(Key, NewVal, MS);
	    error ->
	      if KeyLabel =:= ValLabel -> MS;
		 true ->
		  ?debug("Subst: storing ~p = ~p\n", [KeyLabel, ValLabel]),
		  NewSubst = dict:store(KeyLabel, ValLabel, Subst),
		  {Map, NewSubst}
	      end
	  end
      end
  end.

lookup_type(Key, {Map, Subst}) -> 
  lookup(Key, Map, Subst, t_none()).

lookup(Key, Map, Subst, AnyNone) ->
  case cerl:is_literal(Key) of
    true -> literal_type(Key);
    false -> 
      Label = get_label(Key),
      case dict:find(Label, Subst) of
	{ok, NewKey} -> lookup(NewKey, Map, Subst, AnyNone);
	error ->
	  case dict:find(Label, Map) of
	    {ok, Val} -> Val;
	    error -> AnyNone
	  end
      end
  end.

lookup_fun_sig(Fun, Callgraph, Plt) ->
  MFAorLabel =
    case dialyzer_callgraph:lookup_name(Fun, Callgraph) of
      error -> Fun;
      {ok, MFA} -> MFA
    end,
  dialyzer_plt:lookup(Plt, MFAorLabel).

literal_type(Lit) ->
  t_from_term(cerl:concrete(Lit)).

mark_as_fresh([Tree|Left], Map) ->
  SubTrees1 = lists:append(cerl:subtrees(Tree)),
  {SubTrees2, Map1} =
    case cerl:type(Tree) of
      bitstr ->
	%% The Size field is not fresh.
	{SubTrees1 -- [cerl:bitstr_size(Tree)], Map};
      var ->
	{SubTrees1, enter_type(Tree, t_any(), Map)};
      _ ->
	{SubTrees1, Map}
    end,
  mark_as_fresh(SubTrees2 ++ Left, Map1);
mark_as_fresh([], Map) ->
  Map.

-ifdef(DEBUG).
debug_pp_map(Map = {Map0, _Subst}) ->
  Keys = dict:fetch_keys(Map0),
  io:format("Map:\n", []),
  [io:format("\t~w :: ~s\n", [Key, t_to_string(lookup_type(Key, Map))])
   || Key <- Keys],
  ok.
-else.
debug_pp_map(_Map) -> ok.
-endif.

%%% ===========================================================================
%%%
%%%  Utilities
%%%
%%% ===========================================================================

-spec(wrap_if_single/1 :: (_) -> [_]).

wrap_if_single(X) when is_list(X) -> X;
wrap_if_single(X) -> [X].

get_label(L) when is_integer(L) ->
  L;
get_label(T) ->
  cerl_trees:get_label(T).

any_none(Ts) ->
  lists:any(fun erl_types:t_is_none/1, Ts).

any_unit(Ts) ->
  lists:any(fun erl_types:t_is_unit/1, Ts).

filter_match_fail([Clause]) ->
  Body = cerl:clause_body(Clause),
  case cerl:type(Body) of
    primop ->
      case cerl:atom_val(cerl:primop_name(Body)) of
	match_fail -> [];
	raise -> [];
	_ -> [Clause]
      end;
    _ -> [Clause]
  end;
filter_match_fail([H|T]) ->
  [H|filter_match_fail(T)];
filter_match_fail([]) ->
  %% This can actually happen, for example in 
  %%      receive after 1 -> ok end
  [].


%%% ===========================================================================
%%%
%%%  The State.
%%%
%%% ===========================================================================

state__new(Callgraph, Tree, Plt, Records, ForwardMode) -> 
  TreeMap = build_tree_map(Tree),
  Funs = dict:fetch_keys(TreeMap),
  FunTab = init_fun_tab(Funs, dict:new(), TreeMap, Callgraph, Plt, ForwardMode),
  
  Work = init_work([get_label(Tree)]),
  Env = dict:store(top, map__new(), dict:new()),
  #state{callgraph=Callgraph, envs=Env, fun_tab=FunTab, 
	 forward_mode=ForwardMode,
	 plt=Plt, records=Records, warning_mode=false, warnings=[],
	 work=Work, tree_map=TreeMap}.

state__mark_fun_as_handled(State = #state{fun_tab=FunTab}, Fun0) ->
  Fun = get_label(Fun0),
  case dict:find(Fun, FunTab) of
    {ok, {not_handled, Entry}} -> 
      State#state{fun_tab=dict:store(Fun, Entry, FunTab)};
    {ok, {_, _}} ->
      State
  end.

state__warning_mode(#state{warning_mode=WM}) ->
  WM.

state__set_warning_mode(State = #state{tree_map=TreeMap, fun_tab=FunTab}) ->
  ?debug("Starting warning pass\n", []),
  Funs = dict:fetch_keys(TreeMap),
  State#state{work=init_work([top|Funs--[top]]), fun_tab=FunTab,
	      warning_mode=true}.

state__add_warning(State, Tag, Tree, Msg) ->
  state__add_warning(State, Tag, Tree, Msg, false).

state__add_warning(S = #state{warning_mode=false}, _Tag, _Tree, _Msg, _Force) ->
  S;
state__add_warning(State = #state{warnings=Warnings, warning_mode=true}, 
		   Tag, Tree, Msg, Force) ->
  Ann = cerl:get_ann(Tree),
  case Force of
    true ->
      Warn = {Tag, {{get_file(Ann), abs(get_line(Ann))}, Msg}},
      State#state{warnings=[Warn|Warnings]};
    false ->
      case is_compiler_generated(Ann) of
	    true -> State;
	false ->
	  Warn = {Tag, {{get_file(Ann), get_line(Ann)}, Msg}},
	  State#state{warnings=[Warn|Warnings]}
      end
  end.

state__get_warnings(State = #state{tree_map=TreeMap, 
				   fun_tab=FunTab,
				   callgraph=Callgraph,
				   plt=Plt},
		    NoWarnUnused) ->
  FoldFun = 
    fun({top, _}, AccState) -> AccState;
       ({FunLbl, Fun}, AccState) ->
	{NotCalled, Ret} =
	  case dict:fetch(get_label(Fun), FunTab) of
	    {not_handled, {_Args0, Ret0}} -> {true, Ret0};
	    {Args0, Ret0} -> {any_none(Args0), Ret0}
	  end,
	case NotCalled of
	  true ->
	    {Warn, NameString} =
	      case dialyzer_callgraph:lookup_name(FunLbl, Callgraph) of
		error -> {true, "Function "};
		{ok, {_M, F, A}} = MFA-> 
		  {not sets:is_element(MFA, NoWarnUnused),    
		   io_lib:format("Function ~w/~w ", [F, A])}
	      end,
	    Msg = NameString ++ "will never be called\n",
	    if Warn -> state__add_warning(AccState, ?WARN_NOT_CALLED, Fun, Msg);
	       true -> AccState
	    end;
	  false ->
	    {NameString, Contract} =
	      case dialyzer_callgraph:lookup_name(FunLbl, Callgraph) of
		error -> {"Function ", none};
		{ok, {_M, F, A} = MFA} -> 
		  {io_lib:format("Function ~w/~w ", [F, A]),
		   dialyzer_plt:lookup_contract(Plt, MFA)}
	      end,
	    case t_is_none(Ret) of
	      true ->
		%% Check if the function has a contract that allows this.
		Warn =
		  case Contract of
		    none -> true;
		    {value, C} ->
		      GenRet = dialyzer_contracts:get_contract_return(C),
		      not t_is_unit(GenRet)
		  end,
		case Warn of
		  true ->
		    case classify_returns(Fun) of
		      only_explicit ->
			Msg = NameString ++ 
			  "only terminates with explicit exception\n",
			state__add_warning(AccState, ?WARN_RETURN_ONLY_EXIT, 
					   Fun, Msg);
		      only_normal ->
			Msg = NameString ++ "has no local return\n",
			state__add_warning(AccState, ?WARN_RETURN_NO_RETURN, 
					   Fun, Msg);
		      both ->
			Msg = NameString ++ "has no local return\n",
			state__add_warning(AccState, ?WARN_RETURN_NO_RETURN, 
					   Fun, Msg)
		    end;
		  false ->
		    AccState
		end;
	      false ->
		AccState
	    end
	end
    end,
  #state{warnings=Warn} = lists:foldl(FoldFun, State, dict:to_list(TreeMap)),
  Warn.

state__is_escaping(Fun, #state{callgraph=Callgraph}) ->
  dialyzer_callgraph:is_escaping(Fun, Callgraph).

state__lookup_type_for_rec_var(Var, S = #state{callgraph=Callgraph}) ->
  Label = get_label(Var),
  case dialyzer_callgraph:lookup_rec_var(Label, Callgraph) of
    error -> error;
    {ok, MFA} ->
      case dialyzer_callgraph:lookup_label(MFA, Callgraph) of
	error -> error;
	{ok, FunLabel} ->
	  {ok, state__fun_type(FunLabel, S)}
      end
  end.

state__lookup_name(MFA = {_,_,_}, #state{}) ->
  MFA;
state__lookup_name(top, #state{}) ->
  top;
state__lookup_name(Fun, #state{callgraph=Callgraph}) ->
  case dialyzer_callgraph:lookup_name(Fun, Callgraph) of
    {ok, MFA} -> MFA;
    error -> Fun
  end.

state__lookup_record(Tag, Arity, #state{records=Records}) ->
  case erl_types:lookup_record(Tag, Arity, Records) of
    {ok, Fields} -> 
      {ok, t_tuple([t_from_term(Tag)|
		    [FieldType || {_FieldName, FieldType} <- Fields]])};
    error -> 
      error
  end.

state__get_args(Tree, #state{fun_tab=FunTab}) ->
  Fun = get_label(Tree),
  case dict:find(Fun, FunTab) of
    {ok, {not_handled, {ArgTypes, _}}} -> ArgTypes;
    {ok, {ArgTypes, _}} -> ArgTypes
  end.

build_tree_map(Tree) ->
  Fun =
    fun(T, Dict) ->
	case cerl:is_c_fun(T) of
	  true ->
	    dict:store(get_label(T), T, Dict);
	  false ->
	    Dict
	end
    end,
  cerl_trees:fold(Fun, dict:new(), Tree).

init_fun_tab([top|Left], Dict, TreeMap, Callgraph, Plt, ForwardMode) ->
  NewDict = dict:store(top, {not_handled, {[], t_none()}}, Dict),
  init_fun_tab(Left, NewDict, TreeMap, Callgraph, Plt, ForwardMode);
init_fun_tab([Fun|Left], Dict, TreeMap, Callgraph, Plt, ForwardMode) ->
  Arity = cerl:fun_arity(dict:fetch(Fun, TreeMap)),
  FunEntry =
    case dialyzer_callgraph:is_escaping(Fun, Callgraph) of
      true -> 
	%% TODO: Add contract
	case lookup_fun_sig(Fun, Callgraph, Plt) of
	  none -> {lists:duplicate(Arity, t_any()), t_unit()};
	  {value, {RetType, ArgTypes0}} ->
	    ArgTypes = 
	      case ForwardMode of
		unconstrained -> lists:duplicate(Arity, t_any());
		constrained -> ArgTypes0
	      end,
	    case t_is_none(RetType) of
	      true -> {ArgTypes, t_none()};
	      false -> {ArgTypes, t_unit()}
	    end
	end;
      false -> {lists:duplicate(Arity, t_none()), t_unit()}
    end,
  NewDict = dict:store(Fun, {not_handled, FunEntry}, Dict),
  init_fun_tab(Left, NewDict, TreeMap, Callgraph, Plt, ForwardMode);
init_fun_tab([], Dict, _TreeMap, _Callgraph, _Plt, _ForwardMode) ->
  Dict.

state__update_fun_env(Tree, Map, State = #state{envs=Envs}) ->
  NewEnvs = dict:store(get_label(Tree), Map, Envs),
  State#state{envs=NewEnvs}.

state__fun_env(Tree, #state{envs=Envs}) ->
  Fun = get_label(Tree),
  case dict:find(Fun, Envs) of
    error -> none;
    {ok, Map} -> Map
  end.

state__clean_not_called(State = #state{fun_tab=FunTab}) ->
  NewFunTab =
    dict:map(fun(top, Entry) -> Entry;
		(_Fun, {not_handled, {Args, _}}) -> {Args, t_none()};
		(_Fun, Entry) -> Entry
	     end, FunTab),
  State#state{fun_tab=NewFunTab}.

state__all_fun_types(#state{fun_tab=FunTab}) ->
  Tab1 = dict:erase(top, FunTab),
  dict:map(fun(_Fun, {Args, Ret}) -> t_fun(Args, Ret)end, Tab1).

state__fun_type(Fun, #state{fun_tab=FunTab}) ->
  Label = 
    if is_integer(Fun) -> Fun;
       true -> get_label(Fun)
    end,
  case dict:find(Label, FunTab) of
    {ok, {not_handled, {A, R}}} ->
      t_fun(A, R);
    {ok, {A, R}} ->
      t_fun(A, R)
  end.

state__update_fun_entry(Tree, ArgTypes, Out0, 
			State = #state{fun_tab=FunTab, callgraph=CG, plt=Plt})->
  Fun = get_label(Tree),
  Out1 = 
    if Fun =:= top -> Out0;
       true ->
	case lookup_fun_sig(Fun, CG, Plt) of
	  {value, {SigRet, _}} -> t_inf(SigRet, Out0);
	  none -> Out0
	end
    end,
  Out = t_limit(Out1, ?TYPE_LIMIT),
  case dict:find(Fun, FunTab) of
    {ok, {ArgTypes, OldOut}} ->
      case t_is_equal(OldOut, Out) of
	true -> 
	  ?debug("Fixpoint for ~w: ~s\n", 
		 [state__lookup_name(Fun, State), 
		  t_to_string(t_fun(ArgTypes, Out))]),
	  State;
	false ->
	  NewEntry = {ArgTypes, Out},
	  ?debug("New Entry for ~w: ~s\n", 
		 [state__lookup_name(Fun, State), 
		     t_to_string(t_fun(ArgTypes, Out))]),
	  NewFunTab = dict:store(Fun, NewEntry, FunTab),
	  State1 = State#state{fun_tab=NewFunTab},
	  state__add_work_from_fun(Tree, State1)
      end;
    {ok, {NewArgTypes, _OldOut}} ->
      %% Can only happen in self-recursive functions. Only update the out type.
      NewEntry = {NewArgTypes, Out},
      ?debug("New Entry for ~w: ~s\n", 
	     [state__lookup_name(Fun, State), 
	      t_to_string(t_fun(NewArgTypes, Out))]),
      NewFunTab = dict:store(Fun, NewEntry, FunTab),
      State1 = State#state{fun_tab=NewFunTab},
      state__add_work_from_fun(Tree, State1)
  end.

state__add_work_from_fun(_Tree, State = #state{warning_mode=true}) ->
  State;
state__add_work_from_fun(Tree, State = #state{callgraph=Callgraph, 
					      tree_map=TreeMap}) ->
  case get_label(Tree) of
    top -> State;
    Label when is_integer(Label) ->
      case dialyzer_callgraph:in_neighbours(Label, Callgraph) of
	none -> State;
	MFAList ->
	  LabelList = [dialyzer_callgraph:lookup_label(MFA, Callgraph)
		       || MFA <- MFAList],
	  %% Must filter the result for results in this module.	  
	  FilteredList = lists:filter(fun({ok, L})->dict:is_key(L, TreeMap)end,
				      LabelList),
	  ?debug("~w: Will try to add:~w\n", 
		 [state__lookup_name(get_label(Tree), State), MFAList]),
	  lists:foldl(fun({ok, X}, AccState)->
			  state__add_work(X, AccState)end,
		      State, FilteredList)
      end
  end.

state__add_work(external, State) ->
  State;
state__add_work(top, State) ->
  State;
state__add_work(Fun, State = #state{work=Work}) ->
  NewWork = add_work(Fun, Work),
  State#state{work=NewWork}.

state__get_work(State = #state{work=Work, tree_map=TreeMap}) ->
  case get_work(Work) of
    none -> none;
    {Fun, NewWork} ->
      {dict:fetch(Fun, TreeMap), State#state{work=NewWork}}
  end.

state__lookup_call_site(Tree, #state{callgraph=Callgraph}) ->
  Label = get_label(Tree),
  dialyzer_callgraph:lookup_call_site(Label, Callgraph).

state__fun_info(external, #state{}) ->
  external;
state__fun_info(MFA = {_, _, _}, #state{plt=Plt}) ->
  {MFA,
   dialyzer_plt:lookup(Plt, MFA), 
   dialyzer_plt:lookup_contract(Plt, MFA),
   t_any()};
state__fun_info(Fun, #state{callgraph=Callgraph, fun_tab=FunTab, plt=Plt}) ->
  {Sig, Contract} =
    case dialyzer_callgraph:lookup_name(Fun, Callgraph) of
      error -> 
	{dialyzer_plt:lookup(Plt, Fun), none};
      {ok, MFA} -> 
	{dialyzer_plt:lookup(Plt, MFA), dialyzer_plt:lookup_contract(Plt, MFA)}
    end,
  LocalRet =
    case dict:fetch(Fun, FunTab) of
      {not_handled, {_Args, Ret}} -> Ret;
      {_Args, Ret} -> Ret
    end,
  {Fun, Sig, Contract, LocalRet}.

state__find_apply_return(Tree, State = #state{callgraph=Callgraph}) ->
  Apply = get_label(Tree),
  case dialyzer_callgraph:lookup_call_site(Apply, Callgraph) of
    error ->
      unknown;
    {ok, List} ->
      case lists:member(external, List) of
	true -> t_any();
	false ->
	  FunTypes = [state__fun_type(F, State) || F <- List],
	  Returns = [t_fun_range(F) || F <- FunTypes],
	  t_sup(Returns)
      end
  end.

state__forward_mode(#state{forward_mode=FM}) ->
  FM.

forward_args(Fun, ArgTypes, State = #state{work=Work, fun_tab=FunTab}) ->
  {OldArgTypes, OldOut, Fixpoint} =
    case dict:find(Fun, FunTab) of
      {ok, {not_handled, {OldArgTypes0, OldOut0}}} -> 
	{OldArgTypes0, OldOut0, false};
      {ok, {OldArgTypes0, OldOut0}} ->
	{OldArgTypes0, OldOut0, 
	 t_is_subtype(t_product(ArgTypes), t_product(OldArgTypes0))}
    end,
  case Fixpoint of
    true -> State;
    false -> 
      NewArgTypes = [t_sup(X, Y) || {X, Y} <- lists:zip(ArgTypes, OldArgTypes)],
      NewWork = add_work(Fun, Work),
      ?debug("~w: forwarding args ~s\n", 
	     [state__lookup_name(Fun, State), 
	      t_to_string(t_product(NewArgTypes))]),
      NewFunTab = dict:store(Fun, {NewArgTypes, OldOut}, FunTab),
      State#state{work=NewWork, fun_tab=NewFunTab}
  end.
  
%%% ===========================================================================
%%%
%%%  Worklist
%%%
%%% ===========================================================================

init_work(List) ->
  {List, [], sets:from_list(List)}.

get_work({[], [], _Set}) ->
  none;
get_work({[H|T], Rev, Set}) ->
  {H, {T, Rev, sets:del_element(H, Set)}};
get_work({[], Rev, Set}) ->
  get_work({lists:reverse(Rev), [], Set}).

add_work(New, Work = {List, Rev, Set}) ->
  case sets:is_element(New, Set) of
    true -> Work;
    false -> {List, [New|Rev], sets:add_element(New, Set)}
  end.

%%% ===========================================================================
%%%
%%%  Utilities.
%%%
%%% ===========================================================================

get_line([Line|_]) when is_integer(Line) -> Line;
get_line([_|Tail]) -> get_line(Tail);
get_line([]) -> -1.

get_file([{file, File}|_]) -> File;
get_file([_|Tail]) -> get_file(Tail).

is_compiler_generated(Ann) ->
  lists:member(compiler_generated, Ann) orelse (get_line(Ann) < 1).

-spec(format_args/3 :: ([_], [_], #state{}) -> [char(),...]).

format_args([], [], _State) ->
  "()";
format_args(ArgList, TypeList, State) ->
  "(" ++ format_args_1(ArgList, TypeList, State) ++ ")".

-spec(format_args_1/3 :: ([any(),...], [any(),...], #state{}) -> string()).

format_args_1([Arg], [Type], State) ->
  format_arg(Arg) ++ format_type(Type, State);
format_args_1([Arg|Args], [Type|Types], State) ->
  String =
    case cerl:is_literal(Arg) of
      true -> format_cerl(Arg);
      false -> format_arg(Arg) ++ format_type(Type, State)
    end,
  String ++ "," ++ format_args_1(Args, Types, State).

format_arg(Arg) ->
  Default = "",
  case cerl:is_c_var(Arg) of
    true ->
      case cerl:var_name(Arg) of
	Atom when is_atom(Atom) ->
	  case atom_to_list(Atom) of
	    "cor"++_ -> Default;
	    "rec"++_ -> Default;
	    Name -> Name ++ "::"
	  end;
	_What -> Default
      end;
    false ->
      Default
  end.

-spec(format_type/2 :: (_, #state{}) -> string()).

format_type(Type, #state{records=R}) ->
  t_to_string(Type, R).

-spec(format_sig/2 :: (_, #state{}) -> string()).

format_sig(C = #contract{}, #state{}) ->
  dialyzer_contracts:contract_to_string(C);
format_sig(Type, #state{records=R}) ->
  dialyzer_utils:format_sig(Type, R).

-spec(format_sig_args/2 :: (_, #state{}) -> string()).

format_sig_args(#contract{}, #state{}) ->
  erlang:error("Cannot format sig args for contracts");
format_sig_args(Type, #state{records=R}) ->
  SigArgs = t_fun_args(Type),
  lists:flatten("(" ++ t_to_string(hd(SigArgs), R) 
		    ++ ["," ++ t_to_string(T, R) || T <- tl(SigArgs)] ++ ")").

format_cerl(Tree) ->
  cerl_prettypr:format(cerl:set_ann(Tree, []), 
		       [{hook, dialyzer_utils:pp_hook()},
			{noann, true},
			{paper, 100000}, %% These guys strip
			{ribbon, 100000} %% newlines.
		       ]).

format_patterns(Pats) ->
  NewPats = map_pats(cerl:c_values(Pats)),
  String = format_cerl(NewPats),
  case Pats of
    [PosVar] ->
      case cerl:is_c_var(PosVar) andalso (cerl:var_name(PosVar) =/= '') of
	true -> "variable "++String;
	false -> "pattern "++String
      end;
    _ ->
      "pattern "++String
  end.

map_pats(Pats) ->
  Fun = fun(Tree) ->
	    case cerl:is_c_var(Tree) of
	      true ->
		case cerl:var_name(Tree) of
		  Atom when is_atom(Atom) ->
		    case atom_to_list(Atom) of
		      "cor"++_ -> cerl:c_var('');
		      "rec"++_ -> cerl:c_var('');
		      _ -> cerl:set_ann(Tree, [])
		    end;
		  _What -> cerl:c_var('')
		end;
	      false ->
		cerl:set_ann(Tree, [])
	    end
	end,
  cerl_trees:map(Fun, Pats).

classify_returns(Tree) ->
  case find_terminals(cerl:fun_body(Tree)) of
    {true, false} -> only_explicit;
    {false, true} -> only_normal;
    {true, true} -> both
  end.

find_terminals(Tree) ->
  case cerl:type(Tree) of
    apply -> {false, true};
    binary -> {false, true};
    bitstr -> {false, true};
    call ->
      M0 = cerl:call_module(Tree),
      F0 = cerl:call_name(Tree),
      A = length(cerl:call_args(Tree)),
      case cerl:is_literal(M0) andalso cerl:is_literal(F0) of
	false ->
	  %% We cannot make assumptions. Say that both are true.
	  {true, true};
	true ->
	  M = cerl:concrete(M0),
	  F = cerl:concrete(F0),
	  case (erl_bif_types:is_known(M, F, A) 
		andalso t_is_none(erl_bif_types:type(M, F, A))) of
	    true -> {true, false};
	    false -> {false, true}
	  end
      end;
    'case' -> find_terminals_list(cerl:case_clauses(Tree));
    'catch' -> find_terminals(cerl:catch_body(Tree));
    clause -> find_terminals(cerl:clause_body(Tree));
    cons -> {false, true};
    'fun' -> {false, true};
    'let' -> find_terminals(cerl:let_body(Tree));
    letrec -> find_terminals(cerl:letrec_body(Tree));
    literal -> {false, true};
    primop -> {false, false}; %% match_fail etc are not explicit exits.
    'receive' -> 
      Timeout = cerl:receive_timeout(Tree),
      Clauses = cerl:receive_clauses(Tree),
      case (cerl:is_literal(Timeout) andalso
	    (cerl:concrete(Timeout) =:= infinity)) of
	true -> 
	  if Clauses =:= [] -> {false, true}; %% An neverending receive.
	     true -> find_terminals_list(Clauses)
	  end;
	false -> find_terminals_list([cerl:receive_action(Tree)|Clauses])
      end;
    seq -> find_terminals(cerl:seq_body(Tree));
    'try' -> find_terminals_list([cerl:try_handler(Tree), cerl:try_body(Tree)]);
    tuple -> {false, true};
    values -> {false, true};
    var -> {false, true}
  end.

find_terminals_list(List) ->
  find_terminals_list(List, false, false).

find_terminals_list([Tree|Left], Explicit1, Normal1) ->
  {Explicit2, Normal2} = find_terminals(Tree),
  case {Explicit1 or Explicit2, Normal1 or Normal2} of
    {true, true} = Ans -> Ans;
    {NewExplicit, NewNormal} ->
      find_terminals_list(Left, NewExplicit, NewNormal)
  end;
find_terminals_list([], Explicit, Normal) ->
  {Explicit, Normal}.

%%----------------------------------------------------------------------------

-ifdef(DEBUG_PP).
debug_pp(Tree, true) -> 
  io:put_chars(cerl_prettypr:format(Tree, [{hook, cerl_typean:pp_hook()}])),
  io:nl(),
  ok;
debug_pp(Tree, false) ->
  io:put_chars(cerl_prettypr:format(strip_annotations(Tree))),
  io:nl(),
  ok.

strip_annotations(Tree) ->
  Fun = fun(T) ->
	    case cerl:type(T) of
	      var ->
		cerl:set_ann(T, [{label, cerl_trees:get_label(T)}]);
	      'fun' ->
		cerl:set_ann(T, [{label, cerl_trees:get_label(T)}]);
	      _ ->
		cerl:set_ann(T, [])
	    end
	end,
  cerl_trees:map(Fun, Tree).

-else.

debug_pp(_Tree, _UseHook) ->
  ok.
-endif.

%%----------------------------------------------------------------------------

-spec(to_dot/1 :: (#dialyzer_callgraph{}) -> 'ok').

-ifdef(DOT).
to_dot(CG) ->
  dialyzer_callgraph:to_dot(CG).
-else.
to_dot(#dialyzer_callgraph{}) ->
  ok.
-endif.

%%----------------------------------------------------------------------------
