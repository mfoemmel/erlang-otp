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

-export([race/5, race_var_map/4, bind_dict_vars/3,
         format_args/4, compare_var_list/3]).

-include("dialyzer.hrl").
-include("dialyzer_callgraph.hrl").
-include("dialyzer_dataflow.hrl").

%%-define(DEBUG, true).

-ifdef(DEBUG).
-define(debug(S_, L_), io:format(S_, L_)).
-else.
-define(debug(S_, L_), ok).
-endif.

%%% ===========================================================================
%%%
%%%  Race Analysis
%%%
%%% ===========================================================================

-spec race(mfa_or_funlbl(), [erl_type()], [core_vars()], file_line(),
           #state{}) ->
           {#state{}, dial_race_warn_tag(), dep_list()}.

race(Fun, ArgTypes, Args, FileLine,
            #state{callgraph = Callgraph, races = Races,
		   warning_mode = WarningMode} = State) -> 
  ?debug("Fun: ~p, Args: ~p\n",
    [Fun, dialyzer_dataflow:format_args(Args, ArgTypes, State)]),
  CurrFun = Races#dialyzer_races.curr_fun,
  CleanState = cleanup_state_for_storing(State),
  {NewRaceList, RaceWarnTag, DependencyList, NewTable} = 
    case CurrFun of
      {_Module, module_info, A} when A =:= 0 orelse A =:= 1 ->
        {[], ?WARN_NO_WARN, [], no_t};
      _Thing ->
        RaceList = Races#dialyzer_races.race_list,
        case Fun of
	  {ets, new, 2} ->
	    NewNList = format_args(Args, ArgTypes, CleanState, ets_new),
            OptionsList = lists:nth(4, NewNList),
            NewTable1 =
              case lists:member("'public'", OptionsList) of
                true ->
                  case lists:member("'named_table'", OptionsList) of
                    true ->
                      {named, lists:nth(1, NewNList), lists:nth(2, NewNList)};
                    false -> other
                  end;
                false -> no_t
              end,
	    {RaceList, ?WARN_NO_WARN, [], NewTable1};
          {erlang, whereis, 1} ->
            NewWList = format_args(Args, ArgTypes, CleanState, whereis),
	    {[#dep_call{call_name = whereis, args = NewWList,
                        arg_types = ArgTypes, vars = Args,
                        state = CleanState, file_line = FileLine}|
              RaceList], ?WARN_NO_WARN, [], no_t};
          {ets, lookup, 2} ->
            NewLList = format_args(Args, ArgTypes, CleanState, ets_lookup),
            {[#dep_call{call_name = ets_lookup, args = NewLList,
                        arg_types = ArgTypes, vars = Args,
                        state = CleanState, file_line = FileLine}|
              RaceList], ?WARN_NO_WARN, [], no_t}; 
          {erlang, register, 2} ->
            NewRList = format_args(Args, ArgTypes, CleanState, register),
            case WarningMode of
              true ->
                case fixup_race_list(?WARN_WHEREIS_REGISTER, NewRList,
                                     State) of
                  [] ->
                    {[#warn_call{call_name = register, args = NewRList}|
                      RaceList], ?WARN_NO_WARN, [], no_t};
                  DepList ->
                    {[#warn_call{call_name = register, args = NewRList}|
                      RaceList], ?WARN_WHEREIS_REGISTER, DepList, no_t}
                end;
              false ->
                {[#warn_call{call_name = register, args = NewRList}|
                  RaceList], ?WARN_NO_WARN, [], no_t}
            end;
          {ets, insert, 2} ->
            NewIList = format_args(Args, ArgTypes, CleanState, ets_insert),
            case WarningMode of
              true ->
                case fixup_race_list(?WARN_ETS_LOOKUP_INSERT,
                                     NewIList, State) of
                  [] ->
                    {[#warn_call{call_name = ets_insert, args = NewIList}|
                      RaceList], ?WARN_NO_WARN, [], no_t};
                  DepList ->
                    {[#warn_call{call_name = ets_insert, args = NewIList}|
                      RaceList], ?WARN_ETS_LOOKUP_INSERT, DepList, no_t}
                end;
              false ->
                {[#warn_call{call_name = ets_insert, args = NewIList}|
                  RaceList], ?WARN_NO_WARN, [], no_t}
            end;
          Int when is_integer(Int) ->
            {[#fun_call{caller = CurrFun, callee = Int, arg_types =  ArgTypes,
                        vars = Args}|RaceList],
              ?WARN_NO_WARN, [], no_t};
          {erlang, get_module_info, B} when B =:= 1 orelse B =:= 2 ->
            {[], ?WARN_NO_WARN, [], no_t};
          _Other ->
            case digraph:vertex(Callgraph#dialyzer_callgraph.digraph, Fun) of
              {Fun, confirmed} -> 
                {[#fun_call{caller = CurrFun, callee = Fun, arg_types =  ArgTypes,
                            vars = Args}|RaceList],
                  ?WARN_NO_WARN, [], no_t};
              false -> {RaceList, ?WARN_NO_WARN, [], no_t}
            end
        end
    end,
  {state__renew_race_list(NewRaceList, state__renew_table(NewTable, State)),
    RaceWarnTag, DependencyList}.

fixup_race_list(RaceWarnTag, VarArgTypes,
		#state{callgraph = Callgraph, races = Races} = State) ->
  CurrFun = Races#dialyzer_races.curr_fun,
  CurrFunLabel = Races#dialyzer_races.curr_fun_label,
  RaceList = Races#dialyzer_races.race_list,
  RaceVarMap = Callgraph#dialyzer_callgraph.race_var_map,
  Digraph = Callgraph#dialyzer_callgraph.digraph,
  Exports = Callgraph#dialyzer_callgraph.exports,
  Calls = digraph:edges(Digraph),
  RaceTag =
    case RaceWarnTag of
      ?WARN_WHEREIS_REGISTER -> whereis_register;
      ?WARN_ETS_LOOKUP_INSERT -> ets_lookup_insert
    end,
  DepList1 =
    fixup_race_forward(CurrFun, CurrFunLabel, Calls,
		       lists:reverse(NewRaceList = [RaceTag|RaceList]),
		       [], CurrFun, VarArgTypes, RaceWarnTag, RaceVarMap,
                       State),
  Tested =
    fixup_race_backward(CurrFun, Calls, Calls, []),
  Filtered =
    filter_parents(lists:usort(Tested), Exports, Digraph),
  NewTested =
    case lists:member(CurrFun, Filtered) of
      true -> Filtered;
      false -> [CurrFun|Filtered]
    end,
  NewState = state__renew_race_list(NewRaceList, State),
  DepList2 =
    fixup_race_list_helper(NewTested, Calls, CurrFun, VarArgTypes,
    RaceWarnTag, RaceVarMap, cleanup_state_for_forward(NewState)),
  lists:usort(DepList1 ++ DepList2).

fixup_race_list_helper(Tested, Calls, CurrFun,
                       VarArgTypes, RaceWarnTag, RaceVarMap,
                       #state{callgraph = Callgraph} = State) ->
  case Tested of
    [] -> [];
    [Head|Tail] ->
      Code = get_code(Head,
        Callgraph#dialyzer_callgraph.race_code),
      {ok, FunLabel} =
        dialyzer_callgraph:lookup_label(Head, Callgraph),
      fixup_race_forward(Head, FunLabel, Calls, Code, [],
        CurrFun, VarArgTypes, RaceWarnTag, RaceVarMap, State) ++
        fixup_race_list_helper(Tail, Calls, CurrFun, VarArgTypes,
        RaceWarnTag, RaceVarMap, State)
  end.

%%% ===========================================================================
%%%
%%%  Forward Analysis
%%%
%%% ===========================================================================

fixup_race_forward(CurrFun, CurrFunLabel, Calls, Code, RaceList,
                   InitFun, VarArgTypes, RaceWarnTag,
                   RaceVarMap, #state{callgraph = Callgraph} = State) ->
  case Code of
    [] -> [];
    [Head|Tail] ->
      {NewRaceList1, DepList} =
        case Head of
          #dep_call{} ->
     	    {[Head|RaceList], []};
  	  #warn_call{} ->
     	    {[Head|RaceList], []};
          #fun_call{caller = CurrFun, callee = Int1} when is_integer(Int1) ->
            {RaceList, []};
	  #fun_call{caller = CurrFun} ->
            {RaceList, []};
          beg_case ->
            {[Head|RaceList], []};
          beg_clause ->
            {[Head|RaceList], []};
          end_clause ->
            {[Head|RaceList], []};
          end_case ->
            NewRaceList2 =
              case cleanup_case_paths(RaceList) of
                {[beg_case|TRaceList], _EmptyClause} -> TRaceList;
                {CaseRaceList, true} ->
                  [Head|[end_clause|[beg_clause|CaseRaceList]]];
                {CaseRaceList, false} -> [Head|CaseRaceList]
              end,
            {NewRaceList2, []};
%%            {[Head|RaceList], []};
          #curr_fun{} ->
            {RaceList, []};
          RaceTag ->
            NewHead =
              case RaceTag of
                whereis_register ->
                  #warn_call{call_name = register, args = VarArgTypes};
                ets_lookup_insert ->
                  #warn_call{call_name = ets_insert, args = VarArgTypes}
              end,
            {NewDepList, _Return} =
              get_deplist_paths(RaceList, VarArgTypes, RaceWarnTag,
              RaceVarMap),
            {[NewHead|RaceList], NewDepList}
        end,
      {NewCurrFun, NewCurrFunLabel, NewCalls, NewCode, NewRaceList,
        NewVarArgTypes, NewRaceVarMap} =
        case Head of
          #fun_call{caller = CurrFun, callee = Call, arg_types =  FunArgTypes,
                    vars = FunArgs} ->
            {Name, Label} =
              case is_integer(Call) of
                true ->
                  {dialyzer_callgraph:lookup_name(Call, Callgraph), {ok, Call}};
                false ->
                  {{ok, Call}, dialyzer_callgraph:lookup_label(Call, Callgraph)}
              end,
            case Name =:= error orelse Label =:= error of
              true ->
                {CurrFun, CurrFunLabel, Calls, Tail, NewRaceList1,
                   VarArgTypes, RaceVarMap};
              false ->
                {ok, Fun} = Name,
                {ok, Int} = Label,
                {RetCurrFun, RetCurrFunLabel, RetCalls, RetCode,
                  RetRaceList, RetVarArgTypes, RetRaceVarMap} =
                  fixup_race_forward_helper(CurrFun, CurrFunLabel,
                    Fun, Int, Calls, Calls,
                    [#curr_fun{mfa = CurrFun, label = CurrFunLabel,
                               args = VarArgTypes}|Tail],
                    NewRaceList1, InitFun, VarArgTypes, FunArgs, FunArgTypes,
                    RaceWarnTag, RaceVarMap, State),
                {RetCurrFun, RetCurrFunLabel, RetCalls, RetCode, RetRaceList,
                  RetVarArgTypes, RetRaceVarMap}
            end;
          #curr_fun{mfa = CurrFun1, label = CurrFunLabel1,
                    args = VarArgTypes1} ->
            {CurrFun1, CurrFunLabel1, Calls, Tail, NewRaceList1,
              VarArgTypes1, RaceVarMap};
          _Thing ->
            {CurrFun, CurrFunLabel, Calls, Tail, NewRaceList1,
              VarArgTypes, RaceVarMap}
        end,
    DepList ++
      fixup_race_forward(NewCurrFun, NewCurrFunLabel, NewCalls,
      NewCode, NewRaceList, InitFun, NewVarArgTypes, RaceWarnTag,
      NewRaceVarMap, State)
  end.

get_deplist_paths(RaceList, VarArgTypes, RaceWarnTag, RaceVarMap) ->
  case RaceList of
    [] -> {[], true};
    [Head|Tail] ->
      case Head of
        end_case ->
          {DepList1, Return1} = handle_case(Tail, VarArgTypes, RaceWarnTag,
            RaceVarMap),
          case Return1 of
            true ->
              {DepList2, Return2} =
                get_deplist_paths(fixup_case_path_forward_helper(Tail),
                VarArgTypes, RaceWarnTag, RaceVarMap),
              {DepList1 ++ DepList2, Return2};
            false -> {DepList1, false}
          end;
        beg_clause ->
           get_deplist_paths(fixup_case_path_forward_helper(Tail), VarArgTypes,
             RaceWarnTag, RaceVarMap);
        #warn_call{call_name = register, args = VarArgTypes1} ->
          case compare_register(VarArgTypes, VarArgTypes1, RaceVarMap) of
            true -> {[], false};
            false ->
              get_deplist_paths(Tail, VarArgTypes, RaceWarnTag, RaceVarMap)
          end;
        #warn_call{call_name = ets_insert, args = VarArgTypes1} ->
          case compare_ets_insert(VarArgTypes, VarArgTypes1, RaceVarMap) of
            true -> {[], false};
            NewVarArgTypes ->
              get_deplist_paths(Tail, NewVarArgTypes, RaceWarnTag, RaceVarMap)
          end;
        #dep_call{} ->
          {DepList, Return} =
            get_deplist_paths(Tail, VarArgTypes, RaceWarnTag, RaceVarMap),
          {refine_race(Head, VarArgTypes, RaceWarnTag, DepList, RaceVarMap),
            Return};
        _Other ->
          get_deplist_paths(Tail, VarArgTypes, RaceWarnTag, RaceVarMap)
      end
  end.

handle_case(RaceList, VarArgTypes, RaceWarnTag, RaceVarMap) ->
  case RaceList of
    [] -> {[], true};
    [Head|Tail] ->
      case Head of
        end_clause ->
          {RestRaceList, DepList1, Return1} =
            do_clause(Tail, VarArgTypes, RaceWarnTag, RaceVarMap),
          {DepList2, Return2} =
            handle_case(RestRaceList, VarArgTypes, RaceWarnTag, RaceVarMap),
          {DepList1 ++ DepList2, Return1 orelse Return2};
        beg_case -> {[], false}
      end
  end.

do_clause(RaceList, VarArgTypes, RaceWarnTag, RaceVarMap) ->
  {DepList, Return} =
    get_deplist_paths(fixup_case_path_forward(RaceList, 0), VarArgTypes,
    RaceWarnTag, RaceVarMap),
  {fixup_case_rest_paths_forward(RaceList, 0), DepList, Return}.

fixup_case_path_forward(RaceList, NestingLevel) ->
  case RaceList of
    [] -> [];
    [Head|Tail] ->
      {NewNestingLevel, Return} =
        case Head of
          beg_case -> {NestingLevel - 1, false};
          end_case -> {NestingLevel + 1, false};
          beg_clause ->
            case NestingLevel =:= 0 of
              true -> {NestingLevel, true};
              false -> {NestingLevel, false}
            end;
          _Other -> {NestingLevel, false}
        end,
      case Return of
        true -> [];
        false -> [Head|fixup_case_path_forward(Tail, NewNestingLevel)]
      end
  end.

%% Gets the race list before a case clause.
fixup_case_path_forward_helper(RaceList) ->
  case RaceList of
    [] -> [];
    [Head|Tail] ->
      case Head of
        end_clause ->
          fixup_case_path_forward_helper(
            fixup_case_rest_paths_forward(Tail, 0));
        beg_case -> Tail
      end
  end.

fixup_case_rest_paths_forward(RaceList, NestingLevel) ->
  case RaceList of
    [] -> [];
    [Head|Tail] ->
      {NewNestingLevel, Return} =
        case Head of
          beg_case -> {NestingLevel - 1, false};
          end_case -> {NestingLevel + 1, false};
          beg_clause ->
            case NestingLevel =:= 0 of
              true -> {NestingLevel, true};
              false -> {NestingLevel, false}
            end;
          _Other -> {NestingLevel, false}
        end,
      case Return of
        true -> Tail;
        false -> fixup_case_rest_paths_forward(Tail, NewNestingLevel)
      end
  end.

fixup_race_forward_helper(CurrFun, CurrFunLabel, Fun, FunLabel,
                          Calls, CallsToAnalyze, Code, RaceList,
                          InitFun, VarArgTypes, FunArgs,
                          FunArgTypes, RaceWarnTag, RaceVarMap,
                          #state{callgraph = Callgraph,
                                 races = Races} = State) ->
  RaceCode = Callgraph#dialyzer_callgraph.race_code,
  CleanState = cleanup_state_for_storing(State),
  case Calls of
    [] ->
      {NewRaceList,
        #curr_fun{mfa = NewCurrFun, label = NewCurrFunLabel,
                  args = NewVarArgTypes},
        NewCode} = 
        fixup_empty_call(RaceList,
        #curr_fun{mfa = CurrFun, label = CurrFunLabel, args = VarArgTypes},
        Code),
      {NewCurrFun, NewCurrFunLabel, CallsToAnalyze, NewCode,
        NewRaceList, NewVarArgTypes, RaceVarMap};
    [Head|Tail] ->
      case Head of
        {InitFun, InitFun} when CurrFun =:= InitFun andalso
                                    Fun =:= InitFun ->
          NewCallsToAnalyze = lists:delete(Head, CallsToAnalyze),
          case dict:find(InitFun, RaceCode) of
            %% TO CHECK
            error ->
              Args = [],
              NewVarArgTypes =
                race_type_analysis(Args, FunArgTypes, VarArgTypes, RaceWarnTag,
       	          RaceVarMap, CleanState),
              NewRaceVarMap =
                race_var_map(Args, FunArgs, RaceVarMap, bind),
              NewCode =
                lists:reverse(Races#dialyzer_races.race_list) ++
                fixup_calls(InitFun, InitFun, FunLabel, NewVarArgTypes, Args,
                RaceWarnTag,
                lists:reverse([#curr_fun{mfa =  InitFun, label = CurrFunLabel,
                                         args = NewVarArgTypes}|
                Races#dialyzer_races.race_list]), Code, RaceVarMap,
                CleanState),
              {InitFun, FunLabel, NewCallsToAnalyze, NewCode, RaceList,
                NewVarArgTypes, NewRaceVarMap};
            {ok, [Args, CodeB]} ->
              NewVarArgTypes =
                race_type_analysis(Args, FunArgTypes, VarArgTypes, RaceWarnTag,
       	          RaceVarMap, CleanState),
              NewRaceVarMap =
                race_var_map(Args, FunArgs, RaceVarMap, bind),
              NewCode = 
                fixup_calls(InitFun, InitFun, FunLabel, VarArgTypes, Args,
                RaceWarnTag, CodeB ++
                [#curr_fun{mfa =  InitFun, label = CurrFunLabel,
                           args = VarArgTypes}],
                lists:reverse(Races#dialyzer_races.race_list) ++
                fixup_calls(InitFun, InitFun, FunLabel, NewVarArgTypes, Args,
                RaceWarnTag,
                lists:reverse([#curr_fun{mfa = InitFun, label = CurrFunLabel,
                                         args = NewVarArgTypes}|
                Races#dialyzer_races.race_list]), Code, RaceVarMap,
                CleanState), RaceVarMap, CleanState),
  	      {InitFun, FunLabel, NewCallsToAnalyze, NewCode, RaceList,
                NewVarArgTypes, NewRaceVarMap}
          end;
        {CurrFun, Fun} ->
          NewCallsToAnalyze = lists:delete(Head, CallsToAnalyze),
          case dict:find(Fun, RaceCode) of
            %% TO CHECK
            error ->
              Args = [],
              case Fun of
                InitFun ->
                  NewVarArgTypes =
                    race_type_analysis(Args, FunArgTypes, VarArgTypes,
                    RaceWarnTag, RaceVarMap, CleanState),
                  NewRaceVarMap =
                    race_var_map(Args, FunArgs, RaceVarMap, bind),
                  NewCode =
                    lists:reverse(Races#dialyzer_races.race_list) ++
                      fixup_calls(CurrFun, Fun, FunLabel, VarArgTypes, Args,
                      RaceWarnTag,
                      lists:reverse([#curr_fun{mfa = CurrFun, label = CurrFunLabel,
                                               args = VarArgTypes}|
                      Races#dialyzer_races.race_list]), Code, RaceVarMap,
                      CleanState),
                  {Fun, FunLabel, NewCallsToAnalyze, NewCode, RaceList,
                    NewVarArgTypes, NewRaceVarMap};
                _Other ->
                  {NewRaceList,
                     #curr_fun{mfa = NewCurrFun, label = NewCurrFunLabel,
                               args = NewVarArgTypes},
                     NewCode} =
                     fixup_empty_call(RaceList,
                     #curr_fun{mfa = CurrFun, label = CurrFunLabel,
                               args = VarArgTypes},
                     Code),
                  {NewCurrFun, NewCurrFunLabel, NewCallsToAnalyze,
                    NewCode, NewRaceList, NewVarArgTypes, RaceVarMap}
              end;
            {ok, [Args, CodeB]} ->
              NewRaceVarMap =
                race_var_map(Args, FunArgs, RaceVarMap, bind),
              case {Fun,
                    dialyzer_callgraph:is_self_rec(Fun, Callgraph)} of
                {InitFun, false} ->
                  NewVarArgTypes =
                    race_type_analysis(Args, FunArgTypes, VarArgTypes,
                    RaceWarnTag, RaceVarMap, CleanState),
                  NewCode =
                    lists:reverse(Races#dialyzer_races.race_list) ++
                    fixup_calls(CurrFun, Fun, FunLabel, VarArgTypes, Args,
                    RaceWarnTag,
                    lists:reverse([#curr_fun{mfa = CurrFun, label = CurrFunLabel,
                                             args = VarArgTypes}|
                    Races#dialyzer_races.race_list]), Code, RaceVarMap,
                    CleanState),
                  {Fun, FunLabel, NewCallsToAnalyze, NewCode, RaceList,
                    NewVarArgTypes, NewRaceVarMap};
                {InitFun, true} ->
                  NewCode =
                    CodeB ++ fixup_calls(CurrFun, Fun, FunLabel, VarArgTypes,
                    Args, RaceWarnTag, CodeB ++
                    [#curr_fun{mfa = CurrFun, label = CurrFunLabel,
                               args = VarArgTypes}],
                    Code, RaceVarMap, CleanState),
                  {Fun, FunLabel, NewCallsToAnalyze, NewCode, RaceList,
                    VarArgTypes, NewRaceVarMap};
                _Other ->
                  NewCode =
                    CodeB ++ fixup_calls(CurrFun, Fun, FunLabel, VarArgTypes,
                    Args, RaceWarnTag, CodeB ++
                    [#curr_fun{mfa = CurrFun, label = CurrFunLabel,
                               args = VarArgTypes}],
                    Code, RaceVarMap, CleanState),
                  {Fun, FunLabel, NewCallsToAnalyze, NewCode, RaceList,
                    VarArgTypes, NewRaceVarMap}
              end
          end;
	{_TupleA, _TupleB} ->
	  fixup_race_forward_helper(CurrFun, CurrFunLabel, Fun, FunLabel,
            Tail, CallsToAnalyze, Code, RaceList, InitFun, VarArgTypes,
            FunArgs, FunArgTypes, RaceWarnTag, RaceVarMap, State)
      end
  end.

%%% ===========================================================================
%%%
%%%  Backward Analysis
%%%
%%% ===========================================================================

fixup_race_backward(CurrFun, Calls, CallsToAnalyze, Tested) ->
  case Calls of 
    [] ->
      case lists:member(CurrFun, Tested) orelse is_integer(CurrFun) of
        true -> Tested;
        false -> [CurrFun|Tested]
      end;
    [Head|Tail] ->
      MorePaths =
        case Head of
          {Parent, CurrFun} -> true;
          {Parent, _TupleB} -> false
        end,
      case MorePaths of
        true ->
          NewCallsToAnalyze = lists:delete(Head, CallsToAnalyze),
          NewTested =
            fixup_race_backward(Parent, NewCallsToAnalyze,
              NewCallsToAnalyze, Tested),
          fixup_race_backward(CurrFun, Tail, NewCallsToAnalyze, NewTested);
        false ->
          fixup_race_backward(CurrFun, Tail, CallsToAnalyze, Tested)
      end
  end.

%%% ===========================================================================
%%%
%%%  Utilities
%%%
%%% ===========================================================================

are_bound(Label1, Label2, RaceVarMap) ->
  case dict:find(Label1, RaceVarMap) of
    error -> false;
    {ok, Values} ->
      case lists:member(Label2, Values) of
        true -> true;
        false -> are_bound_helper(Values, Label1, Label2, RaceVarMap)
      end
  end.

are_bound_helper(Values, OldValue, CompValue, RaceVarMap) ->
  case dict:new() of
    RaceVarMap -> false;
    _Else ->
      case Values of
        [] -> false;
        [Head|Tail] ->
          NewRaceVarMap = dict:erase(OldValue, RaceVarMap),
          case are_bound(Head, CompValue, NewRaceVarMap) of
            true -> true;
            false -> are_bound_helper(Tail, Head, CompValue, NewRaceVarMap)
          end
      end
  end.

%% Cleans up the empty clauses.
cleanup_case_paths(RaceList) ->
  case RaceList of
    [] -> {[], false};
    [Head|_Tail] ->
      case Head of
        end_clause ->
          {ClauseRaceList, RestRaceList} =
            cleanup_case_paths_helper(RaceList, 0),
          {NewClauseRaceList, EmptyClause} =
            case ClauseRaceList of
              [end_clause, beg_clause] -> {[], true};
              _Other -> {ClauseRaceList, false}
            end,
          {NewRaceList, NewEmptyClause} = cleanup_case_paths(RestRaceList),
          {NewClauseRaceList ++ NewRaceList,
            EmptyClause orelse NewEmptyClause};
        beg_case -> {RaceList, false}
      end
  end.

cleanup_case_paths_helper(RaceList, NestingLevel) ->
  case RaceList of
    [] -> {[], []};
    [Head|Tail] ->
      {NewNestingLevel, Return} =
        case Head of
          beg_case -> {NestingLevel - 1, false};
          end_case -> {NestingLevel + 1, false};
          beg_clause ->
            case NestingLevel =:= 0 of
              true -> {NestingLevel, true};
              false -> {NestingLevel, false}
            end;
          _Other -> {NestingLevel, false}
        end,
      case Return of
        true -> {[Head], Tail};
        false ->
          {ClauseRaceList, RestRaceList} =
            cleanup_case_paths_helper(Tail, NewNestingLevel),
          {[Head|ClauseRaceList], RestRaceList}
      end
  end.

cleanup_clause_code(CurrTuple, Code, NestingLevel) ->
  case Code of
    [] -> {CurrTuple, []};
    [Head|Tail] ->
      {NewNestingLevel, Return} =
        case Head of
          beg_case -> {NestingLevel + 1, false};
          end_case -> {NestingLevel - 1, false};
          end_clause ->
            case NestingLevel =:= 0 of
              true -> {NestingLevel, true};
              false -> {NestingLevel, false}
            end;
          _Other -> {NestingLevel, false}
        end,
      case Return of
        true -> {CurrTuple, Tail};
        false ->
          case Head of
            #curr_fun{} ->
              cleanup_clause_code(Head, Tail, NewNestingLevel);
            _Else -> cleanup_clause_code(CurrTuple, Tail, NewNestingLevel)
          end
      end
  end.

cleanup_callgraph(#dialyzer_callgraph{name_map = NameMap,
                                      rev_name_map = RevNameMap,
                                      self_rec = SelfRecs,
                                      race_code = RaceCode}) ->
  #dialyzer_callgraph{name_map = NameMap,
                      rev_name_map = RevNameMap,
                      self_rec = SelfRecs,
                      race_code = RaceCode}.

cleanup_races(#dialyzer_races{race_list = RaceList}) ->
  #dialyzer_races{race_list = RaceList}.

%% Cleans up the state record that is about to be stored
%% for space efficiency.
cleanup_state_for_storing(#state{records = Records}) ->
  #state{records = Records}.

cleanup_state_for_forward(#state{callgraph = Callgraph,
                                 races = Races,
                                 records = Records}) ->
  #state{callgraph = cleanup_callgraph(Callgraph),
         races = cleanup_races(Races),
         records = Records}.

filter_parents(Tested, Exports, Digraph) ->
  Filtered = filter_parents_helper1(Tested, Exports),
  filter_parents_helper2(Filtered, Filtered, Digraph).

filter_parents_helper1(Tested, Exports) ->
  case Tested of
    [] -> [];
    [Head|Tail] ->
      NewHead =
        case lists:member(Head, Exports) of
          true -> [Head];
          false -> []
        end,
      NewHead ++ filter_parents_helper1(Tail, Exports)
  end.

filter_parents_helper2(Tested, NewTested, Digraph) ->
  case Tested of
    [] -> NewTested;
    [Head|Tail] ->
      NewTested1 =
        filter_parents_helper3(Head, Tail, NewTested, Digraph),
      filter_parents_helper2(Tail, NewTested1, Digraph)
  end.

filter_parents_helper3(First, Rest, NewTested, Digraph) ->
  case Rest of
    [] -> NewTested;
    [Head|Tail] ->
      NewTested1 = filter_parents_helper4(First, Head, NewTested, Digraph),
      filter_parents_helper3(First, Tail, NewTested1, Digraph)
  end.

filter_parents_helper4(Fun1, Fun2, NewTested, Digraph) ->
  case digraph:get_path(Digraph, Fun1, Fun2) of
    false ->
      case digraph:get_path(Digraph, Fun2, Fun1) of
        false -> NewTested;
        _Vertices ->
          case lists:member(Fun2, NewTested) of
            true -> NewTested -- [Fun1];
            false -> NewTested
          end
      end;
    _Vertices ->
      case lists:member(Fun1, NewTested) of
        true -> NewTested -- [Fun2];
        false -> NewTested
      end
  end.

fixup_calls(CurrFun, NextFun, NextFunLabel, VarArgTypes, Args,
            RaceWarnTag, CodeToReplace, Code, RaceVarMap, CleanState) ->
  case Code of
    [] -> [];
    [Head|Tail] ->
      NewCode =
        case Head of
          #fun_call{caller = CurrFun, callee = NextFun,
                    arg_types = ArgTypes} ->
            [#curr_fun{mfa = NextFun, label = NextFunLabel,
                       args = race_type_analysis(Args, ArgTypes, VarArgTypes,
                                                 RaceWarnTag, RaceVarMap,
                                                 CleanState)}|
              CodeToReplace];
          #fun_call{caller = CurrFun, callee = NextFunLabel,
                    arg_types = ArgTypes} ->
            [#curr_fun{mfa = NextFun, label = NextFunLabel,
                       args = race_type_analysis(Args, ArgTypes, VarArgTypes,
                                                 RaceWarnTag, RaceVarMap,
                                                 CleanState)}|
              CodeToReplace];
          _Other -> [Head]
        end,
      NewCode ++ fixup_calls(CurrFun, NextFun, NextFunLabel, VarArgTypes, Args,
         RaceWarnTag, CodeToReplace, Tail, RaceVarMap, CleanState)
  end.

fixup_empty_call(RaceList, CurrTuple, Code) -> 
  NewRaceList = fixup_case_rest_paths_forward(RaceList, 0),
  {NewCurrTuple, NewCode} =
    cleanup_clause_code(CurrTuple, Code, 0),
  ReturnTuple = {NewRaceList, NewCurrTuple, NewCode},
  case NewRaceList of
    [beg_case|RTail] ->
      case NewCode of
        [end_case|CTail] ->
          fixup_empty_call(RTail, NewCurrTuple, CTail);
        _Other -> ReturnTuple
      end;
    _Else -> ReturnTuple
  end.

get_code(CurrFun, RaceCode) ->
  case dict:find(CurrFun, RaceCode) of
    error -> [];
    {ok, [_Args, Code]} -> Code
  end.
    
lists_key_member(?no_label, _L, _N) ->
  0;
lists_key_member(Member, List, N) ->
  case List of
    [] -> 0;
    [Head|Tail] ->
      NewN = N + 1,
      case Head of
        Member -> NewN;
        _Other -> lists_key_member(Member, Tail, NewN)
      end
  end.

lists_key_member_helper(VarList, FunList, N) ->
  case VarList of
    [] -> 0;
    [Head|Tail] ->
      case lists_key_member(Head, FunList, N) of
        0 -> lists_key_member_helper(Tail, FunList, N);
        Other -> Other
      end
  end. 

lists_key_replace(N, [Head|Tail], NewMember) ->
  case N =:= 1 of 
    true -> [NewMember|Tail];
    false -> [Head|lists_key_replace(N - 1, Tail, NewMember)]
  end.

races__renew_race_list(RaceList, Races) ->
  Races#dialyzer_races{race_list = RaceList}.

races__renew_table(Table, Races) ->
  Races#dialyzer_races{new_table = Table}.

refine_race(Race, CompareList, RaceWarnTag, DependencyList, RaceVarMap) ->
  case RaceWarnTag of 
    ?WARN_WHEREIS_REGISTER ->
      case Race of
        #dep_call{call_name = whereis, args = WList} ->
          case compare_types(WList, CompareList, ?WARN_WHEREIS_REGISTER,
            RaceVarMap) of
            true -> [Race|DependencyList];
            false -> DependencyList
          end;
        #dep_call{call_name = ets_lookup} ->
          DependencyList
      end;
    ?WARN_ETS_LOOKUP_INSERT ->
      case Race of
        #dep_call{call_name = whereis} ->
          DependencyList;
        #dep_call{call_name = ets_lookup, args = LList} ->
          case compare_types(LList, CompareList,
                             ?WARN_ETS_LOOKUP_INSERT, RaceVarMap) of
            true -> [Race|DependencyList];
            false -> DependencyList
          end
      end
  end.    

state__renew_race_list(RaceList, #state{races = Races} = State) ->
  State#state{races = races__renew_race_list(RaceList, Races)}.

state__renew_table(Table, #state{races = Races} = State) ->
  State#state{races = races__renew_table(Table, Races)}.

%%% ===========================================================================
%%%
%%%  Variable and Type Utilities
%%%
%%% ===========================================================================

any_args(StrList) ->
  case StrList of
    [] -> false;
    [Head|Tail] ->
      case string:rstr(Head, "()") of
        0 -> any_args(Tail);
        _Other -> true
      end
  end.

-spec bind_dict_vars(label(), label(), dict()) -> dict().

bind_dict_vars(Key, Value, RaceVarMap) ->
  case dict:find(Key, RaceVarMap) of
    error -> dict:store(Key, [Value], RaceVarMap);
    {ok, Values} ->
      case lists:member(Value, Values) of
        true -> RaceVarMap;
        false -> dict:store(Key, [Value|Values], RaceVarMap)
      end
  end.

bind_dict_vars_helper(Key, Values, RaceVarMap) ->
  case Values of
    [] -> RaceVarMap;
    [Head|Tail] ->
      bind_dict_vars_helper(Key, Tail,
        bind_dict_vars(Key, Head, RaceVarMap))
  end.

compare_ets_insert(OldVarArgTypes, NewVarArgTypes, RaceVarMap) ->
  Old1 = lists:nth(1, OldVarArgTypes),
  New1 = lists:nth(1, NewVarArgTypes),
  Old2 = lists:nth(2, OldVarArgTypes),
  New2 = lists:nth(2, NewVarArgTypes),
  Old3 = lists:nth(3, OldVarArgTypes),
  New3 = lists:nth(3, NewVarArgTypes),
  Old4 = lists:nth(4, OldVarArgTypes),
  New4 = lists:nth(4, NewVarArgTypes),
  Bool =
    case any_args(Old2) of
      true -> compare_vars(Old1, New1, RaceVarMap);
      false ->
        case any_args(New2) of
          true -> compare_vars(Old1, New1, RaceVarMap);
          false ->
            case compare_vars(Old1, New1, RaceVarMap) of
              true -> true;
              false -> Old2 =:= New2
            end
        end
    end,
  case Bool of
    true -> 
      case any_args(Old4) of
        true ->
          case compare_list_vars(ets_list_args(Old3), ets_list_args(New3),
                                 [], RaceVarMap) of
            true -> true;
            Args3 -> lists_key_replace(3, OldVarArgTypes, Args3)
          end;
        false ->
           case any_args(New4) of
             true ->
               case compare_list_vars(ets_list_args(Old3), ets_list_args(New3),
                                      [], RaceVarMap) of
                 true -> true;
                 Args3 -> lists_key_replace(3, OldVarArgTypes, Args3)
               end;
             false ->
               case compare_list_vars(ets_list_args(Old3), ets_list_args(New3),
                                      [], RaceVarMap) of
                 true -> true;
                 Args3 ->
                   lists_key_replace(4,
                     lists_key_replace(3, OldVarArgTypes, Args3), Old4 -- New4)
               end
           end
      end;
    false -> OldVarArgTypes
  end.

compare_register(OldVarArgTypes, NewVarArgTypes, RaceVarMap) ->
  Old1 = lists:nth(1, OldVarArgTypes),
  New1 = lists:nth(1, NewVarArgTypes),
  Old2 = lists:nth(2, OldVarArgTypes),
  New2 = lists:nth(2, NewVarArgTypes),
  case any_args(Old2) of
    true -> compare_vars(Old1, New1, RaceVarMap);
    false ->
      case any_args(New2) of
        true -> compare_vars(Old1, New1, RaceVarMap);
        false ->
          case compare_vars(Old1, New1, RaceVarMap) of
            true -> true;
            false -> Old2 =:= New2
          end
      end
  end.

compare_tuple_argtypes(ListA, ListB) when ListA =:= [] orelse ListB =:= [] ->
  false;
compare_tuple_argtypes([Head|Tail], ListB) ->
  case lists:member(Head, ListB) of
    true -> true;
    false -> compare_tuple_argtypes(Tail, ListB)
  end.

%% Compares the argument types of the two suspicious calls.
compare_types(AList, BList, RaceWarnTag, RaceVarMap) ->
  case RaceWarnTag of
    ?WARN_WHEREIS_REGISTER ->
      AListElem1 = lists:nth(1, AList),
      BListElem1 = lists:nth(1, BList),
      AListElem2 = lists:nth(2, AList),
      BListElem2 = lists:nth(2, BList),
      case any_args(AListElem2) of
        true -> compare_vars(AListElem1, BListElem1, RaceVarMap);
        false ->
          case any_args(BListElem2) of
            true -> compare_vars(AListElem1, BListElem1, RaceVarMap);
            false ->
              compare_vars(AListElem1, BListElem1, RaceVarMap) orelse
                length(BListElem2) > length(BListElem2 -- AListElem2)
          end
      end;
    ?WARN_ETS_LOOKUP_INSERT ->
      AListElem1 = lists:nth(1, AList),
      BListElem1 = lists:nth(1, BList),
      AListElem2 = lists:nth(2, AList),
      BListElem2 = lists:nth(2, BList),
      AListElem3 = lists:nth(3, AList),
      BListElem3 = lists:nth(3, BList),
      AListElem4 = lists:nth(4, AList),
      BListElem4 = lists:nth(4, BList),
      Bool =
        case any_args(AListElem2) of
          true -> compare_vars(AListElem1, BListElem1, RaceVarMap);
          false ->
            case any_args(BListElem2) of
              true -> compare_vars(AListElem1, BListElem1, RaceVarMap);
              false -> 
                compare_vars(AListElem1, BListElem1, RaceVarMap) orelse
                  length(BListElem2) > length(BListElem2 -- AListElem2)
            end
        end,
      case Bool of
        true ->
          case any_args(AListElem4) of
            true ->
              compare_var_list(AListElem3, ets_list_args(BListElem3),
                RaceVarMap);
            false ->
              case any_args(BListElem4) of
                true ->
                  compare_var_list(AListElem3, ets_list_args(BListElem3),
                    RaceVarMap);
                false ->
                  compare_var_list(AListElem3, ets_list_args(BListElem3),
                    RaceVarMap) orelse
                    compare_tuple_argtypes(AListElem4, BListElem4)
              end
          end;
        false -> false 
      end
  end.

compare_list_vars(List1, List2, NewList1, RaceVarMap) ->
  case List1 of
    [] ->
      case NewList1 of
        [] -> true;
        _Other -> NewList1
      end;
    [Head|Tail] ->
      NewHead =
        case compare_var_list(Head, List2, RaceVarMap) of
          true -> [];
          false -> [{Head}]
        end,
      compare_list_vars(Tail, List2, NewHead ++ NewList1, RaceVarMap)
  end.

compare_vars(Label1, Label2, RaceVarMap) when is_integer(Label1) andalso
                                              is_integer(Label2) ->
  Label1 =:= Label2 orelse
    are_bound(Label1, Label2, RaceVarMap) orelse
    are_bound(Label2, Label1, RaceVarMap);
compare_vars(_Var1, _Var2, _RaceVarMap) ->
  false.

-spec compare_var_list(label_type(), [label_type()], dict()) -> bool().

compare_var_list(Var, VarList, RaceVarMap) ->
  case VarList of
    [] -> false;
    [Head|Tail] ->
      case compare_vars(Var, Head, RaceVarMap) of
        true -> true;
        false -> compare_var_list(Var, Tail, RaceVarMap)
      end
  end.

ets_list_args(Thing) ->
  case is_list(Thing) of
    true ->
      case Thing =:= [] of
        true -> [];
        false -> [ets_tuple_args(T) || T <- Thing]
      end;
    false -> [ets_tuple_args(Thing)]
 end. 

ets_list_argtypes(ListStr) ->
  ListStr1 = string:strip(ListStr, left, $[),
  ListStr2 = string:strip(ListStr1, right, $]),
  ListStr3 = string:strip(ListStr2, right, $.),
  string:strip(ListStr3, right, $,).

ets_tuple_args(Thing) ->
  case is_tuple(Thing) of
    true -> element(1, Thing);
    false -> ?no_label
  end.

ets_tuple_argtypes(TupleList) ->
  case TupleList of
    [] -> [];
    [Head|Tail] ->
      Head1 = string:strip(Head, left, $[),
      Head2 = string:strip(Head1, left, ${),
      [Head3|_T] = string:tokens(Head2, " ,"),
      NewHead = string:tokens(Head3, " |"),
      NewHead ++ ets_tuple_argtypes(Tail)
  end.

format_arg(Arg) ->
  case cerl:type(Arg) of
    var -> cerl_trees:get_label(Arg);
    tuple -> erlang:list_to_tuple([format_arg(A) || A <- cerl:tuple_es(Arg)]);
    cons -> [format_arg(cerl:cons_hd(Arg))|format_arg(cerl:cons_tl(Arg))];
    alias -> format_arg(cerl:alias_var(Arg));
    literal ->
      case cerl:is_c_nil(Arg) of
        true -> [];
        false -> ?no_label
      end;
    _Other -> ?no_label
  end.

-spec format_args([core_vars()], [erl_type()], #state{}, call()) -> args().

format_args([], [], _State, _Call) ->
  [];
format_args(ArgList, TypeList, CleanState, Call) ->
  format_args_2(format_args_1(ArgList, TypeList, CleanState), Call).

format_args_1([Arg], [Type], CleanState) ->
  [format_arg(Arg), format_type(Type, CleanState)];
format_args_1([Arg|Args], [Type|Types], CleanState) ->
  List =
    case cerl:is_literal(Arg) of
      true -> [?no_label, format_cerl(Arg)];
      false -> [format_arg(Arg), format_type(Type, CleanState)]
    end,
  List ++ format_args_1(Args, Types, CleanState).

format_args_2(StrArgList, Call) ->
  case Call of
    whereis ->
      lists_key_replace(2, StrArgList,
        string:tokens(lists:nth(2, StrArgList), " |"));
    register ->
      lists_key_replace(2, StrArgList,
        string:tokens(lists:nth(2, StrArgList), " |"));
    ets_new ->
      StrArgList1 = lists_key_replace(2, StrArgList,
        string:tokens(lists:nth(2, StrArgList), " |")),
      lists_key_replace(4, StrArgList1,
        string:tokens(
	ets_list_argtypes(lists:nth(4, StrArgList1)), " |"));
    ets_lookup ->
      StrArgList1 = lists_key_replace(2, StrArgList,
        string:tokens(lists:nth(2, StrArgList), " |")),
      lists_key_replace(4, StrArgList1,
        string:tokens(lists:nth(4, StrArgList1), " |"));
    ets_insert ->
      StrArgList1 = lists_key_replace(2, StrArgList,
        string:tokens(lists:nth(2, StrArgList), " |")),
      lists_key_replace(4, StrArgList1,
        ets_tuple_argtypes(string:tokens(lists:nth(4, StrArgList1), " }")));
    function_call -> StrArgList
  end.

format_cerl(Tree) ->
  cerl_prettypr:format(cerl:set_ann(Tree, []),
                       [{hook, dialyzer_utils:pp_hook()},
                        {noann, true},
                        {paper, 100000},
                        {ribbon, 100000}
                       ]).

format_type(Type, #state{records = R}) -> erl_types:t_to_string(Type, R).

race_type_analysis(FunDef, FunCall, RaceArgs, RaceWarnTag, RaceVarMap,
                   CleanState) ->
  FunArgs = format_args(FunDef, FunCall, CleanState, function_call),
  [Head|_Tail] = RaceArgs,
  Vars1 =
    case dict:find(Head, RaceVarMap) of
      error -> [Head];
      {ok, VList1} -> [Head|VList1]
    end,
  NewRaceArgs =
    case lists_key_member_helper(Vars1, FunArgs, 0) of
      0 -> RaceArgs;
      N1 when is_integer(N1) ->
        NewRaceType1 =
          string:tokens(lists:nth(N1 + 1, FunArgs), " |"),
        lists_key_replace(2, RaceArgs, NewRaceType1)
    end,
  case RaceWarnTag of
    ?WARN_ETS_LOOKUP_INSERT ->
      Third = lists:nth(3, NewRaceArgs),
      Vars2 =
        case dict:find(Third, RaceVarMap) of
          error -> [Third];
          {ok, VList2} -> [Third|VList2]
        end,
      case lists_key_member_helper(Vars2, FunArgs, 0) of
        0 -> NewRaceArgs;
        N2 when is_integer(N2) ->
          NewRaceType2 =
            string:tokens(lists:nth(N2 + 1, FunArgs), " }"),
          lists_key_replace(2, NewRaceArgs, NewRaceType2)
      end;
    _Else -> NewRaceArgs
  end.

-spec race_var_map(var_to_map(), var_to_map(), dict(), 'bind' | 'unbind') ->
                   dict().

race_var_map(Vars1, Vars2, RaceVarMap, Op) ->
  case Vars1 =:= ?no_arg orelse Vars2 =:= ?no_arg of
    true -> RaceVarMap;
    false ->
      case is_list(Vars1) andalso is_list(Vars2) of
        true ->
          case Vars1 of
            [] -> RaceVarMap;
            [AHead|ATail] ->
              case Vars2 of
                [] -> RaceVarMap;
                [PHead|PTail] ->
                  NewRaceVarMap = race_var_map(AHead, PHead, RaceVarMap, Op),
                  race_var_map(ATail, PTail, NewRaceVarMap, Op)
              end
          end;
        false ->
          {NewVars1, NewVars2, Bool} =
            case is_list(Vars1) of
              true ->
                case Vars1 of
                  [Var1] -> {Var1, Vars2, true};
                  _Thing -> {Vars1, Vars2, false}
                end;
              false ->
                case is_list(Vars2) of
                  true ->
                    case Vars2 of
                      [Var2] -> {Vars1, Var2, true};
                      _Thing -> {Vars1, Vars2, false}
                    end;
                  false -> {Vars1, Vars2, true}
                end
            end,
          case Bool of
            true ->
              case cerl:type(NewVars1) of
                var ->
                  case cerl:type(NewVars2) of
                    var ->
                      ALabel = cerl_trees:get_label(NewVars1),
                      PLabel = cerl_trees:get_label(NewVars2),
                      case Op of
                        bind ->
                          TempRaceVarMap =
                            bind_dict_vars(ALabel, PLabel, RaceVarMap),
                          bind_dict_vars(PLabel, ALabel, TempRaceVarMap);
                        unbind ->
                          TempRaceVarMap =
                            unbind_dict_vars(ALabel, PLabel, RaceVarMap),
                          unbind_dict_vars(PLabel, ALabel, TempRaceVarMap)
                      end;
                    alias ->
                      race_var_map(NewVars1, cerl:alias_var(NewVars2),
                        RaceVarMap, Op);
                    values ->
                      race_var_map(NewVars1, cerl:values_es(NewVars2),
                        RaceVarMap, Op);
                    _Other -> RaceVarMap
                  end;
                tuple ->
                  case cerl:type(NewVars2) of
                    tuple ->
                      race_var_map(cerl:tuple_es(NewVars1),
                        cerl:tuple_es(NewVars2), RaceVarMap, Op);
                    alias ->
                      race_var_map(NewVars1, cerl:alias_var(NewVars2),
                        RaceVarMap, Op);
                    values ->
                      race_var_map(NewVars1, cerl:values_es(NewVars2),
                        RaceVarMap, Op);
                    _Other -> RaceVarMap
                  end;
                cons ->
                  case cerl:type(NewVars2) of
                    cons ->
                      NewRaceVarMap = race_var_map(cerl:cons_hd(NewVars1),
                        cerl:cons_hd(NewVars2), RaceVarMap, Op),
                      race_var_map(cerl:cons_tl(NewVars1),
                        cerl:cons_tl(NewVars2), NewRaceVarMap, Op);
                    alias ->
                      race_var_map(NewVars1, cerl:alias_var(NewVars2),
                        RaceVarMap, Op);
                    values ->
                      race_var_map(NewVars1, cerl:values_es(NewVars2),
                        RaceVarMap, Op);
                    _Other -> RaceVarMap
                  end;
                alias ->
                  case cerl:type(NewVars2) of
                    alias ->
                      race_var_map(cerl:alias_var(NewVars1),
                        cerl:alias_var(NewVars2), RaceVarMap, Op);
                    _Other ->
                      race_var_map(cerl:alias_var(NewVars1),
                        NewVars2, RaceVarMap, Op)
                  end;
                values ->
                  case cerl:type(NewVars2) of
                    values ->
                      race_var_map(cerl:values_es(NewVars1),
                        cerl:values_es(NewVars2), RaceVarMap, Op);
                    _Other ->
                      race_var_map(cerl:values_es(NewVars1),
                        NewVars2, RaceVarMap, Op)
                  end;
                _Other -> RaceVarMap
              end;
            false -> RaceVarMap
          end
      end
  end.

unbind_dict_vars(Label1, Label2, RaceVarMap) ->
  case dict:find(Label1, RaceVarMap) of
    error -> RaceVarMap;
    {ok, Values} ->
      case Values of
        [] -> dict:erase(Label1, RaceVarMap);
        _Else ->
          case lists:member(Label2, Values) of
            true -> 
              unbind_dict_vars(Label1, Label2,
                bind_dict_vars_helper(Label1, Values -- [Label2],
                dict:erase(Label1, RaceVarMap)));
            false ->
              unbind_dict_vars_helper(Values, Label1, Label2, RaceVarMap)
          end
      end
  end.

unbind_dict_vars_helper(Values, Key, CompValue, RaceVarMap) ->
  case dict:new() of
    RaceVarMap -> RaceVarMap;
    _Else ->
      case Values of
        [] -> RaceVarMap;
        [Head|Tail] ->
          NewRaceVarMap =
            case are_bound(Head, CompValue, RaceVarMap) orelse
                 are_bound(CompValue, Head, RaceVarMap) of
              true ->
                bind_dict_vars_helper(Key, Values -- [Head],
                  dict:erase(Key, RaceVarMap));
              false -> RaceVarMap
            end,
          unbind_dict_vars_helper(Tail, Key, CompValue, NewRaceVarMap)
      end
  end.

%%print(List) ->
%%  case List of
%%    [] -> io:format("\n", []);
%%    [Head|Tail] ->
%%      NewHead =
%%        case is_integer(Head) of
%%          true -> erlang:integer_to_list(Head);
%%          false -> Head
%%        end,
%%      io:format("~s,, ", [NewHead]),
%%      print(Tail)
%%  end.
