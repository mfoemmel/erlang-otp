%%
%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2008-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%

%%%----------------------------------------------------------------------
%%% File    : dialyzer_races.erl
%%% Author  : Maria Christakis <christakismaria@gmail.com>
%%% Description : Utility functions for race condition detection 
%%%
%%% Created : 21 Nov 2008 by Maria Christakis <christakismaria@gmail.com>
%%%----------------------------------------------------------------------
-module(dialyzer_races).

-export([race/5,
         inter_module_race/5,
	 fixup_inter_module_race_forward/5,
	 fixup_inter_module_race_backward/2,
         store_code/3,
         end_of_code_storing/2]).

-include("dialyzer.hrl").
-include("dialyzer_callgraph.hrl").
-include("dialyzer_dataflow.hrl").

-import(erl_types,
        [t_atom_vals/1, t_is_list/1, t_is_tuple/1,
         t_list_elements/1, t_tuple/0, t_tuple_args/1,
         t_tuple_subtypes/1]).

%%-define(DEBUG, true).

-ifdef(DEBUG).
-define(debug(S_, L_), io:format(S_, L_)).
-else.
-define(debug(S_, L_), ok).
-endif.

%%% ===========================================================================
%%%
%%%  Intra-modular Analysis
%%%
%%% ===========================================================================

%% Checks for intra-modular race conditions.
-spec race(fun_type(), [_], [_], file_line(), #state{}) -> {#state{}, dial_race_warn_tag(), [_]}.

race(Fun, ArgTypes, Args, FileLine, State = #state{races=Races}) ->
  ?debug("Fun: ~p\n", [Fun]),
  case is_in_local_fun(Races#dialyzer_races.curr_fun_label, Races#dialyzer_races.local_calls) of
    false -> 
      {Return, _LocalCallRaceList} =
        race_helper(Fun, ArgTypes, Args, FileLine, State),
      Return;
    LocalFunRaceList ->
      {{State2, RaceWarnTag, DependencyList}, NewLocalFunRace} =
        race_helper(Fun, ArgTypes, Args, FileLine, State),
      case NewLocalFunRace of
        none -> {State2, RaceWarnTag, DependencyList};
        _Other ->
          {state__renew_local_calls(Races#dialyzer_races.curr_fun_label, [NewLocalFunRace|LocalFunRaceList], State2),
            RaceWarnTag, DependencyList}
      end
  end.

race_helper(Fun, ArgTypes, Args, FileLine, State = #state{races=Races}) -> 
  CurrFun = Races#dialyzer_races.curr_fun,
  {{NewRaceList, RaceWarnTag, DependencyList}, LocalFunRace} = 
    case CurrFun of
      {_Module, module_info, A} when A =:= 0 orelse A =:= 1 -> {{[], ?WARN_NO_WARN, []}, none};
      _ ->
        RaceList = Races#dialyzer_races.race_list,
        case Fun of
          Int when is_integer(Int) -> {{[{Int}|RaceList], ?WARN_NO_WARN, []}, {Int}};
          {erlang, whereis, 1} ->
            [Arg1] = ArgTypes,
            NewWList = [t_atom_vals(Arg1)],
            State1 = cleanup_suspicious_calls_for_storing(State),
	    {{[{whereis, NewWList, ArgTypes, Args, State1, FileLine}|RaceList], ?WARN_NO_WARN, []},
              {whereis, NewWList, ArgTypes, Args, State1, FileLine}};
          {ets, lookup, 2} ->
            [Arg1, Arg2] = ArgTypes,
            NewLList = [t_atom_vals(Arg1)] ++ [t_atom_vals(Arg2)],
            State1 = cleanup_suspicious_calls_for_storing(State),
            {{[{ets_lookup, NewLList, ArgTypes, Args, State1, FileLine}|RaceList], ?WARN_NO_WARN, []},
              {ets_lookup, NewLList, ArgTypes, Args, State1, FileLine}}; 
          {erlang, register, 2} ->
            Races1 = races__fixup_race_list(State),
            RaceList1 = Races1#dialyzer_races.race_list,
            [Arg1, _Arg2] = ArgTypes,
            NewRList = [t_atom_vals(Arg1)],
            case RaceList1 of
              [] -> {{[{register, NewRList}], ?WARN_NO_WARN, []}, {register, NewRList}};
              [_Head|_Tail] ->
                case which_is_first(whereis, register, RaceList1) of
                  {} ->
                    {{[{register, NewRList}|RaceList1], ?WARN_NO_WARN, []}, {register, NewRList}};
                  {whereis, _WList, _AT, _A, _S, _FL} ->
                    {refine_race_list(RaceList1, NewRList, ?WARN_WHEREIS_REGISTER), {register, NewRList}};
                  {register, _RList} ->
                    case lists:any(fun(A) -> is_suspicious(A, whereis) end, RaceList1) of
                      true -> {refine_race_list(RaceList1, NewRList, ?WARN_WHEREIS_REGISTER), {register, NewRList}};
                      false -> {{RaceList1, ?WARN_NO_WARN, []}, {register, NewRList}}
                    end
                end
            end;
          {ets, insert, 2} ->
            Races1 = races__fixup_race_list(State),
            RaceList1 = Races1#dialyzer_races.race_list,
            [Arg1, Arg2] = ArgTypes,
            ITabList = [t_atom_vals(Arg1)],
            NewIList =
              case t_is_tuple(Arg2) of
                true ->
                  add_to_arg_list(Arg2, ITabList);
                false ->
                  case t_is_list(Arg2) of
                    true -> add_to_arg_list(t_list_elements(Arg2), ITabList);
                    false -> [none]
                  end
              end,
            case RaceList1 of
              [] -> {{[{ets_insert, NewIList}], ?WARN_NO_WARN, []}, {ets_insert, NewIList}};
              [_Head|_Tail] ->
                case which_is_first(ets_lookup, ets_insert, RaceList1) of
                  {} ->
                    {{[{ets_insert, NewIList}|RaceList1], ?WARN_NO_WARN, []}, {ets_insert, NewIList}};
                  {ets_lookup, _LList, _AT, _A, _S, _FL} ->
                    {refine_race_list(RaceList1, NewIList, ?WARN_ETS_LOOKUP_INSERT), {ets_insert, NewIList}};
                  {ets_insert, _IList} ->
                    case lists:any(fun(A) -> is_suspicious(A, ets_lookup) end, RaceList1) of
                      true -> {refine_race_list(RaceList1, NewIList, ?WARN_WHEREIS_REGISTER), {ets_insert, NewIList}};
                      false -> {{RaceList1, ?WARN_NO_WARN, []}, {ets_insert, NewIList}}
                    end
                end
            end;
          {erlang, get_module_info, B} when B =:= 1 orelse B =:= 2-> {{[], ?WARN_NO_WARN, []}, none};
          _ -> {{RaceList, ?WARN_NO_WARN, []}, none}
        end
    end,
  {{state__renew_race_list(NewRaceList, State), RaceWarnTag, DependencyList}, LocalFunRace}.

%%% ===========================================================================
%%%
%%%  Inter-Modular Forward Analysis
%%%
%%% ===========================================================================

%% Fixes up the race list according to the inter module calls
%% (forward analysis).
-spec fixup_inter_module_race_forward(fun_type(), fun_type(), [_], [_], #state{}) -> #state{}.

fixup_inter_module_race_forward(CurrFun, Fun, InterModuleCalls,
				InterModuleCallsToAnalyze,
				State = #state{races = Races}) ->
  RaceList = Races#dialyzer_races.race_list,
  case InterModuleCalls of
    [] -> State;
    [Head|Tail] ->
      case Head of
        {CurrFun, Fun, _C, Code, _B, true} -> 
	  state__renew_race_list(fixup_helper_forward(Fun, Code, RaceList,
            lists:delete(Head, InterModuleCallsToAnalyze), State), State);
        {Fun, _F2, _C1, _C2, false, _B2} -> State;
        {_F1, Fun, _C1, _C2, _B1, false} -> State;
	{_F1, _F2, _C1, _C2, _B1, _B2} ->
	  fixup_inter_module_race_forward(CurrFun, Fun, Tail, InterModuleCallsToAnalyze, State)
      end
  end.
  
fixup_helper_forward(CurrFun, Code, RaceList, InterModuleCalls, State) ->
  case Code of
    [] -> RaceList;
    [Head|Tail] ->
      NewRaceList =
        case Head of
          {whereis, _WList, _ArgTypes, _Args, _S, _FileLine} ->
     	    [Head|RaceList];
          {ets_lookup, _LList, _ArgTypes, _Args, _S, _FileLine} ->
     	    [Head|RaceList];
  	  {register, RList} ->
            {RefinedRaceList, _RaceWarnTag, _DependencyList} =
              refine_race_list(RaceList, RList, ?WARN_WHEREIS_REGISTER),
            RefinedRaceList;
  	  {ets_insert, IList} ->
            {RefinedRaceList, _RaceWarnTag, _DependencyList} =
              refine_race_list(RaceList, IList, ?WARN_ETS_LOOKUP_INSERT),
            RefinedRaceList;
	  {user, Call} ->
	    State1 = fixup_inter_module_race_forward(CurrFun, Call,
						     InterModuleCalls,
						     InterModuleCalls,
						     State),
	    Races = State1#state.races,
	    Races#dialyzer_races.race_list;
          {end_clause, EndClauseList} -> EndClauseList;
          {end_case, EndCaseList} -> EndCaseList;
	  _ -> RaceList
        end,
      fixup_helper_forward(CurrFun, Tail, NewRaceList, InterModuleCalls, State)
  end.

%%% ===========================================================================
%%%
%%%  Inter-Modular Backward Analysis
%%%
%%% ===========================================================================

%% Fixes up the race list according to the inter module calls
%% (backward analysis).
-spec fixup_inter_module_race_backward([_], #state{}) -> #state{}.

fixup_inter_module_race_backward(InterModuleCalls,
				 State = #state{callgraph=Callgraph,
						races=Races}) ->
  CurrFun = Races#dialyzer_races.curr_fun,
  case InterModuleCalls of
    [] -> State;
    [Head|Tail] ->
      case Head of
        {Parent, CurrFun, _C1, _C2, _B1, true} ->
          NewInterModuleCalls = lists:delete(Head, Callgraph#dialyzer_callgraph.inter_module_calls),
          state__renew_race_list(
            fixup_helper_backward(Parent, NewInterModuleCalls,
              NewInterModuleCalls, [CurrFun], State), State);
        {Parent, CurrFun, [{user, CurrFun}|_RaceList], _C2, _B1, _B2} ->
          NewInterModuleCalls = lists:delete(Head, Callgraph#dialyzer_callgraph.inter_module_calls),
	  state__renew_race_list(
	    fixup_helper_backward(Parent, NewInterModuleCalls, 
              NewInterModuleCalls, [CurrFun], State), State);
	{_F1, _F2, _C1, _C2, _B1, _B2} -> fixup_inter_module_race_backward(Tail, State)
      end
  end.

fixup_helper_backward(Parent, InterModuleCalls, InterModuleCallsToAnalyze, FunList,
		      State = #state{callgraph=Callgraph}) ->
  case InterModuleCalls of
    [] ->
      State1 = fixup_helper_backward1([], Parent, Callgraph#dialyzer_callgraph.inter_module_calls,
				      Callgraph#dialyzer_callgraph.inter_module_calls, FunList, State),
      Races = State1#state.races,
      Races#dialyzer_races.race_list;
    [Head|Tail] ->
      case Head of
        {GParent, Parent, _C1, _C2, _B1, true} ->
          NewInterModuleCallsToAnalyze = lists:delete(Head, InterModuleCallsToAnalyze),
	  fixup_helper_backward(GParent, NewInterModuleCallsToAnalyze,
            NewInterModuleCallsToAnalyze, [Parent|FunList], State);
        {GParent, Parent, [{user, Parent}|_RaceList], _C2, _B1, _B2} ->
          NewInterModuleCallsToAnalyze = lists:delete(Head, InterModuleCallsToAnalyze),
	  fixup_helper_backward(GParent, NewInterModuleCallsToAnalyze,
            NewInterModuleCallsToAnalyze, [Parent|FunList], State);
	{_F1,_F2,_C1,_C2,_B1,_B2} -> 
	  fixup_helper_backward(Parent, Tail, InterModuleCallsToAnalyze, FunList, State)
      end
  end.

fixup_helper_backward1(RaceList, Parent, InterModuleCalls, InterModuleCallsToAnalyze, FunList, State) ->
  case InterModuleCalls of
    [] -> State;
    [Head|Tail] ->
      case Head of
        {Parent, _F2, Code, _C2, B1, _B2} ->
          NewCode =
            case B1 of 
              true -> Code;
              false -> reverse_list(Code)
            end,
	  state__renew_race_list(
	    fixup_helper_backward2(NewCode, RaceList,
				   lists:delete(Head, InterModuleCallsToAnalyze),
				   FunList, State), State);
	{_F1, _F2, _C1, _C2, _B1, _B2} ->
          fixup_helper_backward1(RaceList, Parent, Tail, InterModuleCallsToAnalyze, FunList, State)
      end
  end.  
  
fixup_helper_backward2(_Code, _RaceList, _InterModuleCalls, [], _State) -> [];
fixup_helper_backward2(Code, RaceList, InterModuleCalls, List = [CurrFun|FunList], State) ->
  case Code of
    [] ->
      case FunList of
        [] -> [];
        _ -> 
          State1 = fixup_helper_backward1(RaceList, CurrFun, InterModuleCalls, InterModuleCalls,
            FunList, State),
	  Races1 = State1#state.races,
          Races1#dialyzer_races.race_list
      end;
    [Head|Tail] ->
      {NewRaceList, Return} =
        case Head of
          {whereis, _WList, _ArgTypes, _Args, _S, _FileLine} ->
            {[Head|RaceList], false};
          {ets_lookup, _LList, _ArgTypes, _Args, _S, _FileLine} ->
            {[Head|RaceList], false};
  	  {register, RList} -> 
            {RefinedRaceList, _RaceWarnTag, _DependencyList} = refine_race_list(RaceList, RList, ?WARN_WHEREIS_REGISTER),
            {RefinedRaceList, false};
  	  {ets_insert, IList} -> 
            {RefinedRaceList, _RaceWarnTag, _DependencyList} = refine_race_list(RaceList, IList, ?WARN_ETS_LOOKUP_INSERT),
            {RefinedRaceList, false};
  	  {user, Call} -> 
	    case Call of
	      CurrFun -> {RaceList, true};
	      _ ->
	        State1 = fixup_helper_backward1(RaceList, Call, InterModuleCalls, InterModuleCalls,
                  FunList, State),
	        Races1 = State1#state.races,
  	        {Races1#dialyzer_races.race_list, false}
	    end;
          {end_clause, EndClauseList} -> {EndClauseList, false};
          {end_case, EndCaseList} -> {EndCaseList, false};
  	  _ -> {RaceList, false}
        end,
      case Return of
        true -> fixup_helper_backward2(Tail, [], InterModuleCalls, List, State) ++ NewRaceList;
        false -> fixup_helper_backward2(Tail, NewRaceList, InterModuleCalls, List, State)
      end
  end.

%%% ===========================================================================
%%%
%%%  Inter-Modular Analysis -- Code Storing
%%%
%%% ===========================================================================

%% Stores suspicious inter-modular calls
%% and other inter-modular calls defined by the user.
-spec inter_module_race(fun_type(), [_], [_], file_line(), #state{}) -> #state{}.

inter_module_race(Fun, ArgTypes, Args, FileLine, State = #state{callgraph=Callgraph, races=Races}) ->
  case Callgraph#dialyzer_callgraph.inter_module_calls of
    [] -> State;
    Calls -> 
      CurrFun = Races#dialyzer_races.curr_fun,
      {CodeToAdd, IsSame} =
        case Fun of
          {erlang, whereis, 1} ->
            [Arg1] = ArgTypes,
            NewWList = [t_atom_vals(Arg1)],
            State1 = cleanup_suspicious_calls_for_storing(State),
            {{whereis, NewWList, ArgTypes, Args, State1, FileLine}, false};
          {ets, lookup, 2} ->
            [Arg1, Arg2] = ArgTypes,
            NewLList = [t_atom_vals(Arg1)] ++ [t_atom_vals(Arg2)],
            State1 = cleanup_suspicious_calls_for_storing(State),
            {{ets_lookup, NewLList, ArgTypes, Args, State1, FileLine}, false};
          {erlang, register, 2} ->
            [Arg1, _Arg2] = ArgTypes,
            NewRList = [t_atom_vals(Arg1)],
	    {{register, NewRList}, false};
          {ets, insert, 2} ->
            [Arg1, Arg2] = ArgTypes,
            ITabList = [t_atom_vals(Arg1)],
            NewIList =
              case t_is_tuple(Arg2) of
                true ->
                  add_to_arg_list(Arg2, ITabList);
                false ->
                  case t_is_list(Arg2) of
                    true -> add_to_arg_list(t_list_elements(Arg2), ITabList);
                    false -> [none]
                  end
              end,
            {{ets_insert, NewIList}, false};
          {_M, _F, _A} = MFA ->
	    case is_inter_module_call(Fun, Calls) of
	      true  -> {{user, MFA}, false};
	      false -> {{}, true}
	    end;
	  _ -> {{}, true}
        end,
      case IsSame of
        true -> State;
        false ->
          NewCallgraph = Callgraph#dialyzer_callgraph{inter_module_calls =
            store_code(CodeToAdd, CurrFun, Calls)},
          State#state{callgraph=NewCallgraph}
      end
  end.

%% Adds useful code to 3rd element of the tuple of the current
%% function (#dialyzer_callgraph.inter_module_calls field).
-spec store_code(tuple(), fun_type(), [_]) -> [_].

store_code(Code, CurrFun, InterModuleCalls) ->
  case InterModuleCalls of
    [] -> [];
    [Head|Tail] ->
      NewHead =
        case Head of
  	  {CurrFun, TupleB, ListA, ListB, false, BoolB} ->
	    {CurrFun, TupleB, [Code|ListA], ListB, false, BoolB};
	  {TupleA, CurrFun, ListA, ListB, BoolA, false} ->
	    {TupleA, CurrFun, ListA, [Code|ListB], BoolA, false};
	  {_TupleA, _TupleB, _ListA, _ListB, _BoolA, _BoolB} -> Head
        end,
      [NewHead|store_code(Code, CurrFun, Tail)]
  end.

%% Turns the 4th element of the tuple of the current function
%% (#dialyzer_callgraph.inter_module_calls field) true if we have
%% reached the end of the function.
-spec end_of_code_storing(fun_type(), [_]) -> [_].

end_of_code_storing(CurrFun, InterModuleCalls) ->
  case InterModuleCalls of
    [] -> [];
    [Head|Tail] ->
      {NewHead, IsSame} =
        case Head of
  	  {CurrFun, TupleB, ListA, ListB, false, BoolB} ->
            {{CurrFun, TupleB, reverse_list(ListA), ListB, true, BoolB}, false};
	  {TupleA, CurrFun, ListA, ListB, BoolA, false} ->
            {{TupleA, CurrFun, ListA, reverse_list(ListB), BoolA, true}, false};
          {CurrFun, _TupleB, _ListA, _ListB, true, _BoolB} -> {InterModuleCalls, true};
          {_TupleA, CurrFun, _ListA, _ListB, _BoolA, true} -> {InterModuleCalls, true};
	  {_TupleA, _TupleB, _ListA, _ListB, _BoolA, _BoolB} -> {Head, false}
	end,
      case IsSame of
	true -> InterModuleCalls;
	false -> [NewHead|end_of_code_storing(CurrFun, Tail)]
      end
  end.

%%% ===========================================================================
%%%
%%%  Utilities
%%%
%%% ===========================================================================

%% Compares argument types of the two suspicious calls.
compare_lists(AList, BList, RaceWarnTag) ->
  case RaceWarnTag of
    ?WARN_ETS_LOOKUP_INSERT ->
      [AHead, ATail] = AList,
      [BHead|BTail] = BList,
      case AHead =:= BHead of
        true -> lists:member(ATail, BTail);
        false -> false
      end;
    _ -> ((AList -- BList) =:= []) andalso ((BList -- AList) =:= [])
  end.

%% Sorts out the necessary suspicious calls from the unnecessary.
refine_race_list(RaceList, CompareList, RaceWarnTag) ->
  case RaceList of
    [] -> {[], ?WARN_NO_WARN, []};
    [Head|Tail] -> 
      {NewRaceList, NewRaceWarnTag, DependencyList} = refine_race_list(Tail, CompareList, RaceWarnTag),
      case RaceWarnTag of 
        ?WARN_WHEREIS_REGISTER ->
          case Head of
            {whereis, WList, _ArgTypes, _Args, _S, _FileLine} ->
              case compare_lists(WList, CompareList, ?WARN_WHEREIS_REGISTER) of
                true -> {NewRaceList, RaceWarnTag, member(Head, DependencyList, tail)};
                false -> {member(Head, NewRaceList, head), NewRaceWarnTag, DependencyList}
              end;
            {ets_lookup, _LList, _ArgTypes, _Args, _S, _FileLine} ->
              {member(Head, NewRaceList, head), NewRaceWarnTag, DependencyList};
            _ -> {member(Head, NewRaceList, head), NewRaceWarnTag, DependencyList}
          end;
        ?WARN_ETS_LOOKUP_INSERT ->
          case Head of
            {whereis, _WList, _ArgTypes, _Args, _S, _FileLine} ->
              {member(Head, NewRaceList, head), NewRaceWarnTag, DependencyList};
            {ets_lookup, LList, _ArgTypes, _Args, _S, _FileLine} ->
              case compare_lists(LList, CompareList, ?WARN_ETS_LOOKUP_INSERT) of
                true -> {NewRaceList, RaceWarnTag, member(Head, DependencyList, tail)};
                false -> {member(Head, NewRaceList, head), NewRaceWarnTag, DependencyList}
              end;
            _ -> {member(Head, NewRaceList, head), NewRaceWarnTag, DependencyList}
          end
      end
  end.    

%% Checks whether an element is a list member
%% and if it is not it is added to the list head or tail.
member(Elem, List, Where) ->
  case lists:member(Elem, List) of
    true -> List;
    false -> 
      case Where of
        head -> [Elem|List];
        tail -> List ++ [Elem]
      end
  end.

%% Concatenates the atom values of a t_tuple t_list.
add_to_arg_list(ArgList, FrontList) ->
  NewArgList =
    case t_is_tuple(ArgList) of
      true -> ArgList;
      false -> t_tuple()
    end,
  FrontList ++
    case t_tuple_subtypes(NewArgList) of
      any -> [any];
      List when is_list(List) ->
        [t_atom_vals(TupleArg1) || [TupleArg1|_RestTupleArgs] <- [tuple_args(T) || T <- List]]
    end.

tuple_args(Elem) ->
  TupleArgs = t_tuple_args(Elem),
  case is_list(TupleArgs) of
    true -> TupleArgs;
    false -> [any]
  end.

%% Checks whether CallA or CallB is found first in the race list.
which_is_first(CallA, CallB, RaceList) ->
  case RaceList of
    [] -> {};
    [Head|Tail] ->
      case Head of
        {CallA, _AList} -> Head;
        {CallA, _AList, _ArgTypes, _Args, _S, _FileLine} -> Head;
        {CallB, _BList} -> Head;
        {CallB, _BList, _ArgTypes, _Args, _S, _FileLine} -> Head;
        _ -> which_is_first(CallA, CallB, Tail)
      end
  end.  

%% Checks whether a list member is of a certain form.
is_suspicious(Elem, Atom) ->
  case Elem of
    {Atom, _List, _AT, _A, _S, _FL} -> true;
    _ -> false
  end.

%% Checks whether the current function is one of the inter-module
%% calls (#dialyzer_callgraph.inter_module_calls field).
is_inter_module_call(Fun, InterModuleCalls) ->
  case InterModuleCalls of
    [] -> false;
    [Head|Tail] ->
      case Head of
	{Fun, _TB, _LA, _LB, _BA, _BB} -> true;
	{_TA, Fun, _LA, _LB, _BA, _BB} -> true;
	{_TupleA, _TupleB, _ListA, _ListB, _BoolA, _BoolB} ->
	  is_inter_module_call(Fun, Tail)
      end
  end.

%% Cleans up the code from useless paths and then reverses it.
reverse_list(List) ->
  case List of
    [] -> [];
    [{end_case, _CaseRaceList}, {end_clause, _ClauseRaceList}|RestCode] ->
      lists:reverse(RestCode);
    _Other -> lists:reverse(List)
  end.

%% Cleans up the state record that is about to be stored
%% for space efficiency.
cleanup_suspicious_calls_for_storing(#state{records=Records}) ->
  #state{records = Records}.

state__renew_race_list(RaceList, State = #state{races=Races}) ->
  State#state{races = races__renew_race_list(RaceList, Races)}.

races__renew_race_list(RaceList, Races) ->
  Races#dialyzer_races{race_list = RaceList}.

races__fixup_race_list(State = #state{races=Races}) ->
  RaceList = Races#dialyzer_races.race_list,
  LocalCalls = Races#dialyzer_races.local_calls,
  CurrFunLabel = Races#dialyzer_races.curr_fun_label,
  PrevFunLabel = Races#dialyzer_races.prev_fun_label,
  Races#dialyzer_races{
    race_list=races__fixup_local_fun_race_list(races__fixup_race_list_forward(RaceList, State) ++
    races__fixup_race_list_backward(PrevFunLabel, CurrFunLabel, LocalCalls, LocalCalls), LocalCalls),
    prev_fun_label=[]}.

races__fixup_race_list_forward(RaceList, State = #state{races=Races}) ->
  case RaceList of
    [] -> [];
    [Head|Tail] ->
      case is_local_fun(Head) of
        true ->
          {Fun} = Head,
          case lookup_local_call(Fun, Races#dialyzer_races.local_calls, Races#dialyzer_races.local_calls) of
            undefined -> [];
            not_for_use -> [];
            LocalCallRaceList -> LocalCallRaceList ++ races__fixup_race_list_forward(Tail, State)
          end;
        false -> [Head|races__fixup_race_list_forward(Tail, State)]
      end
  end.

races__fixup_race_list_backward(PrevFunLabel, CurrFunLabel, LocalCalls, AllLocalCalls) ->
  case LocalCalls of
    [] -> [];
    [Head|Tail] ->
      case PrevFunLabel of
        [] -> [];
        [HPrevFunLabel|TPrevFunLabel] ->
          case Head of
            {HPrevFunLabel, [{CurrFunLabel}|LocalFunRaceList], _Bool} ->
              NewLocalCalls = lists:delete(Head, AllLocalCalls),
              LocalFunRaceList ++ races__fixup_race_list_backward(TPrevFunLabel, HPrevFunLabel, NewLocalCalls, NewLocalCalls);
            _Other -> races__fixup_race_list_backward(PrevFunLabel, CurrFunLabel, Tail, AllLocalCalls)
          end
      end
  end.

is_local_fun(Fun) ->
  case Fun of
    {Int} when is_integer(Int) -> true;
    _Other -> false
  end.

%% Checks whether a call belongs to the body of a module local call.
is_in_local_fun(CurrFun, LocalCalls) ->
  case LocalCalls of
    [] -> false;
    [Head|Tail] ->
      case Head of
        {CurrFun, FunRaceList, processing} -> FunRaceList;
        _ -> is_in_local_fun(CurrFun, Tail)
      end
  end.

%% Checks whether a module local call has been called again.
lookup_local_call(Call, LocalCalls, AllLocalCalls) ->
  case LocalCalls of
    [] -> undefined;
    [Head|Tail] ->
      case Head of
        {Call, FunRaceList, true} ->
          races__fixup_local_fun_race_list(FunRaceList, AllLocalCalls);
        {Call, _FunRaceList, _Bool} -> not_for_use;
        _Other -> lookup_local_call(Call, Tail, AllLocalCalls)
      end
  end.

state__renew_local_calls(LocalCall, LocalCallRaceList, State = #state{races=Races}) ->
  State#state{races =
    races__renew_local_calls(
    races__find_local_calls(LocalCall, LocalCallRaceList, Races#dialyzer_races.local_calls), Races)}.

races__renew_local_calls(LocalCalls, Races) ->
  Races#dialyzer_races{local_calls = LocalCalls}.

races__find_local_calls(LocalCall, LocalCallRaceList, LocalCalls) ->
  case LocalCalls of
    [] -> [];
    [Head|Tail] ->
      case Head of
        {LocalCall, _LocalCallRaceList, Bool} when Bool =:= false orelse Bool =:= processing ->
          [{LocalCall, LocalCallRaceList, Bool}|Tail];
        _Other -> [Head|races__find_local_calls(LocalCall, LocalCallRaceList, Tail)]
      end
  end.

%% Replaces all the local calls in the local fun race list
%% with their actual race lists.
races__fixup_local_fun_race_list(LocalFunRaceList, LocalCalls) ->
  cleanup_race_list(
    races__fixup_local_fun_race_list_helper(LocalFunRaceList, LocalCalls)).

races__fixup_local_fun_race_list_helper(LocalFunRaceList, LocalCalls) ->
  case LocalFunRaceList of 
    [] -> [];
    [{Int}|Tail] when is_integer(Int) ->
      races__fixup_local_fun_race_list_helper(
        replace_local_call(Int, LocalCalls), lists_delete(Int, LocalCalls))
        ++ races__fixup_local_fun_race_list_helper(Tail, LocalCalls);
    [Head|Tail] ->
      [Head|races__fixup_local_fun_race_list_helper(Tail, LocalCalls)]
  end.

replace_local_call(Int, LocalCalls) ->
  case LocalCalls of
    [] -> [];
    [Head|Tail] ->
      case Head of
        {Int, RaceList, true} -> RaceList;
        _Other -> replace_local_call(Int, Tail)
      end
  end.

lists_delete(Elem, LocalCalls) ->
  case LocalCalls of
    [] -> [];
    [Head|Tail] ->
      case Head of
        {Elem, _RaceList, _Bool} -> Tail;
        _Other -> [Head|lists_delete(Elem, Tail)]
      end
  end.

cleanup_race_list(RaceList) ->
  case RaceList of
    [] -> [];
    [Head|Tail] ->
      case Head of
        {register, RList} ->
          {NewRaceList, _RaceWarnTag, _DependencyList} =
            refine_race_list(cleanup_race_list(Tail), RList, ?WARN_WHEREIS_REGISTER),
          NewRaceList;
        {ets_insert, IList} ->
          {NewRaceList, _RaceWarnTag, _DependencyList} =
            refine_race_list(cleanup_race_list(Tail), IList, ?WARN_ETS_LOOKUP_INSERT),
          NewRaceList;
        _Other -> [Head|cleanup_race_list(Tail)]
      end
  end.
