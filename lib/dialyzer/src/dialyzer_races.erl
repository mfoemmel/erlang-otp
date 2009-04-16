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
         store_code/3,
         end_of_code_storing/2]).

-include("dialyzer.hrl").
-include("dialyzer_callgraph.hrl").
-include("dialyzer_dataflow.hrl").

-import(erl_types,
        [t_any/0, t_atom/0, t_fun/0, t_identifier/0, t_integer/0, t_list/0,
         t_nonempty_list/0, t_number/0, t_to_string/1, t_tuple/0]).

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

%% Checks for intra-modular race conditions.
-spec race(mfa_or_funlbl(), [erl_type()], args(), file_line(), #state{}) ->
  {#state{}, dial_race_warn_tag(), dep_list()}.

race(Fun, ArgTypes, Args, FileLine, #state{races = Races} = State) ->
  ?debug("Fun: ~p, Args: ~p\n",
    [Fun, dialyzer_dataflow:format_args(Args, ArgTypes, State)]),
  case is_in_local_fun(Races#dialyzer_races.curr_fun_label,
                       Races#dialyzer_races.local_calls) of
    false -> 
      {Return, _LocalCallRaceList} =
        race_helper(Fun, ArgTypes, Args, FileLine, State),
      Return;
    LocalFunRaceList ->
      {{State2, RaceWarnTag, DependencyList}, NewLocalFunRace} =
        race_helper(Fun, ArgTypes, Args, FileLine, State),
      case NewLocalFunRace of
        nothing -> {State2, RaceWarnTag, DependencyList};
        _Other ->
          {state__renew_local_calls(Races#dialyzer_races.curr_fun_label,
                                    [NewLocalFunRace|LocalFunRaceList],
                                    State2),
            RaceWarnTag, DependencyList}
      end
  end.

race_helper(Fun, ArgTypes, Args, FileLine,
            #state{callgraph = Callgraph, races = Races,
		   warning_mode = WarningMode} = State) -> 
  CurrFun = Races#dialyzer_races.curr_fun,
  {{NewRaceList, RaceWarnTag, DependencyList}, LocalFunRace} = 
    case CurrFun of
      {_Module, module_info, A} when A =:= 0 orelse A =:= 1 ->
        {{[], ?WARN_NO_WARN, []}, nothing};
      _Thing ->
        RaceList = Races#dialyzer_races.race_list,
        case Fun of
          {erlang, whereis, 1} ->
            State1 = cleanup_suspicious_calls_for_storing(State),
            NewWList = format_args(Args, ArgTypes, whereis),
	    {{[{whereis, NewWList, ArgTypes, Args, State1, FileLine}|
              RaceList], ?WARN_NO_WARN, []},
              {whereis, NewWList, ArgTypes, Args, State1, FileLine}};
          {ets, lookup, 2} ->
            NewLList = format_args(Args, ArgTypes, ets_lookup),
            State1 = cleanup_suspicious_calls_for_storing(State),
            {{[{ets_lookup, NewLList, ArgTypes, Args, State1, FileLine}|
              RaceList], ?WARN_NO_WARN, []},
              {ets_lookup, NewLList, ArgTypes, Args, State1, FileLine}}; 
          {erlang, register, 2} ->
            NewRList = format_args(Args, ArgTypes, register),
            case WarningMode of
              true ->
                case fixup_race_list(?WARN_WHEREIS_REGISTER, NewRList,
                                     State) of
                  [] ->
                    {{[{register, NewRList}|RaceList], ?WARN_NO_WARN, []},
                      {register, NewRList}};
                  DepList ->
                    {{[{register, NewRList}|RaceList], ?WARN_WHEREIS_REGISTER,
                      DepList}, {register, NewRList}}
                end;
              false ->
                {{[{register, NewRList}|RaceList], ?WARN_NO_WARN, []},
                  {register, NewRList}}
            end;
          {ets, insert, 2} ->
            NewIList = format_args(Args, ArgTypes, ets_insert),
            case WarningMode of
              true ->
                case fixup_race_list(?WARN_ETS_LOOKUP_INSERT,
                                     NewIList, State) of
                  [] ->
                    {{[{ets_insert, NewIList}|RaceList], ?WARN_NO_WARN, []},
                      {ets_insert, NewIList}};
                  DepList ->
                    {{[{ets_insert, NewIList}|RaceList],
                      ?WARN_ETS_LOOKUP_INSERT, DepList},
                      {ets_insert, NewIList}}
                end;
              false ->
                {{[{ets_insert, NewIList}|RaceList], ?WARN_NO_WARN, []},
                  {ets_insert, NewIList}}
            end;
          Int when is_integer(Int) ->
            {{[{CurrFun, Int, ArgTypes}|RaceList], ?WARN_NO_WARN, []},
              {CurrFun, Int, ArgTypes}};
          {erlang, get_module_info, B} when B =:= 1 orelse B =:= 2 ->
            {{[], ?WARN_NO_WARN, []}, nothing};
          _Other ->
            case is_inter_module_call(Fun,
                   Callgraph#dialyzer_callgraph.inter_module_calls) of
              true -> 
                {{[{CurrFun, Fun, ArgTypes}|RaceList], ?WARN_NO_WARN, []},
                  {CurrFun, Fun, ArgTypes}};
              false -> {{RaceList, ?WARN_NO_WARN, []}, nothing}
            end
        end
    end,
  {{state__renew_race_list(NewRaceList, State), RaceWarnTag, DependencyList},
    LocalFunRace}.

fixup_race_list(RaceWarnTag, VarArgTypes,
		#state{callgraph = Callgraph, races = Races} = State) ->
  CurrFun = Races#dialyzer_races.curr_fun,
  CurrFunLabel = Races#dialyzer_races.curr_fun_label,
  RaceList = Races#dialyzer_races.race_list,
  LocalCalls = Races#dialyzer_races.local_calls,
  InterModuleCalls = Callgraph#dialyzer_callgraph.inter_module_calls,
  ModuleLocalCalls = Callgraph#dialyzer_callgraph.module_local_calls,
  RaceTag =
    case RaceWarnTag of
      ?WARN_WHEREIS_REGISTER -> whereis_register;
      ?WARN_ETS_LOOKUP_INSERT -> ets_lookup_insert
    end,
  DepList1 =
    fixup_race_forward(CurrFun, CurrFunLabel, InterModuleCalls,
		       ModuleLocalCalls, LocalCalls,
		       lists:reverse(NewRaceList = [RaceTag|RaceList]),
		       [], CurrFun, VarArgTypes, RaceWarnTag, State),
  NewState = state__renew_race_list(NewRaceList, State),
  {DepList2, _Tested} =
    fixup_intra_module_race_backward(CurrFun, InterModuleCalls,
				     ModuleLocalCalls, ModuleLocalCalls,
				     [], VarArgTypes, RaceWarnTag, NewState),
  lists:usort(DepList1 ++ DepList2).

%%% ===========================================================================
%%%
%%%  Forward Analysis
%%%
%%% ===========================================================================

fixup_race_forward(CurrFun, CurrFunLabel, InterModuleCalls, ModuleLocalCalls,
                   LocalCalls, Code, RaceList, InitFun, VarArgTypes,
                   RaceWarnTag, State) ->
  case Code of
    [] -> [];
    [Head|Tail] ->
      {NewRaceList, DepList} =
        case Head of
          {whereis, _WList, _ArgTypes, _Args, _S, _FileLine} ->
     	    {[Head|RaceList], []};
          {ets_lookup, _LList, _ArgTypes, _Args, _S, _FileLine} ->
     	    {[Head|RaceList], []};
  	  {register, _RList} ->
     	    {[Head|RaceList], []};
  	  {ets_insert, _IList} ->
     	    {[Head|RaceList], []};
          {CurrFun, Int, FunArgTypes} when is_integer(Int) ->
            {[], fixup_intra_module_race_forward(CurrFun, CurrFunLabel, Int,
              InterModuleCalls, ModuleLocalCalls, ModuleLocalCalls, LocalCalls,
               [{curr, CurrFun, CurrFunLabel, VarArgTypes}|Tail], RaceList,
               InitFun, VarArgTypes, FunArgTypes, RaceWarnTag, State)};
	  {CurrFun, Call, FunArgTypes} ->
            {RaceList, fixup_inter_module_race_forward(CurrFun, CurrFunLabel,
              Call, InterModuleCalls, InterModuleCalls, ModuleLocalCalls,
              LocalCalls, [{curr, CurrFun, CurrFunLabel, VarArgTypes}|Tail],
              RaceList, InitFun, VarArgTypes, FunArgTypes, RaceWarnTag,
              State)};
          beg_case ->
            {[Head|RaceList], []};
          beg_clause ->
            {[Head|RaceList], []};
          end_clause ->
            {[Head|RaceList], []};
          end_case ->
            NewRaceList1 =
              case cleanup_case_paths(RaceList) of
                {[beg_case|TRaceList], _EmptyClause} -> TRaceList;
                {CaseRaceList, true} ->
                  [Head|[end_clause|[beg_clause|CaseRaceList]]];
                {CaseRaceList, false} -> [Head|CaseRaceList]
              end,
            {NewRaceList1, []};
          {curr, _NewCurrFun, _NewCurrFunLabel, _VarArgTypes} ->
            {RaceList, []};
          RaceTag ->
            NewHead =
              case RaceTag of
                whereis_register ->
                  {register, VarArgTypes};
                ets_lookup_insert ->
                  {ets_insert, VarArgTypes}
              end,
            {NewDepList, _Return} =
              get_deplist_paths(RaceList, VarArgTypes, RaceWarnTag),
            {[NewHead|RaceList], NewDepList}
        end,
      case Head of
        {CurrFun, Integer, _Arguments} when is_integer(Integer) -> DepList;
        {CurrFun, Fun, _Arguments} ->
          case is_user_defined_inter_module_call(Fun, InterModuleCalls) of
            true -> DepList;
            false -> 
              DepList ++ fixup_race_forward(CurrFun, CurrFunLabel,
                InterModuleCalls, ModuleLocalCalls, LocalCalls, Tail,
                NewRaceList, InitFun, VarArgTypes, RaceWarnTag, State)
          end;
        {curr, NewCurrFun, NewCurrFunLabel, NewVarArgTypes} ->
          fixup_race_forward(NewCurrFun, NewCurrFunLabel, InterModuleCalls,
            ModuleLocalCalls, LocalCalls, Tail, NewRaceList, InitFun,
            NewVarArgTypes, RaceWarnTag, State);
        _Thing ->
          DepList ++ fixup_race_forward(CurrFun, CurrFunLabel,
            InterModuleCalls, ModuleLocalCalls, LocalCalls, Tail, NewRaceList,
            InitFun, VarArgTypes, RaceWarnTag, State) 
      end
  end.

get_deplist_paths(RaceList, ArgList, RaceWarnTag) ->
  case RaceList of
    [] -> {[], true};
    [Head|Tail] ->
      case Head of
        end_case ->
          {DepList1, Return1} = handle_case(Tail, ArgList, RaceWarnTag),
          case Return1 of
            true ->
              {DepList2, Return2} =
                get_deplist_paths(fixup_case_path_forward_helper(Tail),
                ArgList, RaceWarnTag),
              {DepList1 ++ DepList2, Return2};
            false -> {DepList1, false}
          end;
        beg_clause ->
           get_deplist_paths(fixup_case_path_forward_helper(Tail), ArgList,
             RaceWarnTag);
        {register, ArgList} -> {[], false};
        {ets_insert, ArgList} -> {[], false};
        {whereis, _WList, _ArgTypes, _Args, _S, _FileLine} ->
          {DepList, Return} =
            get_deplist_paths(Tail, ArgList, RaceWarnTag),
          {refine_race(Head, ArgList, RaceWarnTag, DepList), Return};
        {ets_lookup, _LList, _ArgTypes, _Args, _S, _FileLine} ->
          {DepList, Return} =
            get_deplist_paths(Tail, ArgList, RaceWarnTag),
          {refine_race(Head, ArgList, RaceWarnTag, DepList), Return};
        _Other ->
          get_deplist_paths(Tail, ArgList, RaceWarnTag)
      end
  end.

handle_case(RaceList, ArgList, RaceWarnTag) ->
  case RaceList of
    [] -> {[], true};
    [Head|Tail] ->
      case Head of
        end_clause ->
          {RestRaceList, DepList1, Return1} =
            do_clause(Tail, ArgList, RaceWarnTag),
          {DepList2, Return2} =
            handle_case(RestRaceList, ArgList, RaceWarnTag),
          {DepList1 ++ DepList2, Return1 orelse Return2};
        beg_case -> {[], false}
      end
  end.

do_clause(RaceList, ArgList, RaceWarnTag) ->
  {DepList, Return} =
    get_deplist_paths(fixup_case_path_forward(RaceList, 0), ArgList,
    RaceWarnTag),
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

%%% ===========================================================================
%%%
%%%  Intra-Modular Forward Analysis
%%%
%%% ===========================================================================

%% Fixes up the race list according to the intra module calls
%% (forward analysis).
fixup_intra_module_race_forward(CurrFun, CurrFunLabel, Fun, InterModuleCalls,
                                ModuleLocalCalls, ModuleLocalCallsToAnalyze,
                                LocalCalls, Code, RaceList, InitFun,
                                VarArgTypes, FunArgTypes, RaceWarnTag,
                                #state{races = Races} = State) ->
  case ModuleLocalCalls of
    [] ->
      {NewRaceList, {curr, NewCurrFun, NewCurrFunLabel, NewVarArgTypes},
        NewCode} = 
        fixup_empty_module_local_call(RaceList,
        {curr, CurrFun, CurrFunLabel, VarArgTypes}, Code),
      fixup_race_forward(NewCurrFun, NewCurrFunLabel, InterModuleCalls,
        ModuleLocalCallsToAnalyze, LocalCalls, NewCode, NewRaceList, InitFun,
        NewVarArgTypes, RaceWarnTag, State);
    [Head|Tail] ->
      case Head of
        {InitFun, CurrFunLabel, InitFun, Fun, ArgsB, _CodeA, empty, _Bool} -> %TO FIX
          NewVarArgTypes =
            race_type_analysis(ArgsB, FunArgTypes, VarArgTypes, RaceWarnTag),
          NewModuleLocalCallsToAnalyze =
            lists:delete(Head, ModuleLocalCallsToAnalyze),
          fixup_intra_module_race_forward_helper(CurrFun, CurrFunLabel, InitFun,
            Fun, InterModuleCalls, NewModuleLocalCallsToAnalyze,LocalCalls,
            LocalCalls, Code, RaceList, InitFun, VarArgTypes, NewVarArgTypes,
            RaceWarnTag, State);
        {InitFun, CurrFunLabel, InitFun, Fun, ArgsB, _CodeA, CodeB, _Bool} ->
          NewVarArgTypes =
            race_type_analysis(ArgsB, FunArgTypes, VarArgTypes, RaceWarnTag),
          NewModuleLocalCallsToAnalyze =
            lists:delete(Head, ModuleLocalCallsToAnalyze),
          NewCode1 = fixup_calls(InitFun, InitFun, Fun, VarArgTypes, ArgsB,
            RaceWarnTag, CodeB ++ [{curr, InitFun, CurrFunLabel, VarArgTypes}],
            lists:reverse(Races#dialyzer_races.race_list) ++
            fixup_calls(InitFun, InitFun, Fun, NewVarArgTypes, ArgsB,
            RaceWarnTag, lists:reverse([{curr, InitFun, CurrFunLabel,
            NewVarArgTypes}|Races#dialyzer_races.race_list]), Code)),
          NewCode2 = fixup_calls(InitFun, InitFun, Fun, NewVarArgTypes, ArgsB,
            RaceWarnTag, lists:reverse([{curr, InitFun, CurrFunLabel,
            NewVarArgTypes}|Races#dialyzer_races.race_list]),
            CodeB ++ fixup_calls(InitFun, InitFun, Fun, VarArgTypes, ArgsB,
            RaceWarnTag, CodeB ++ [{curr, InitFun, CurrFunLabel, VarArgTypes}],
            Code)),
	  fixup_race_forward(InitFun, Fun, InterModuleCalls, 
            NewModuleLocalCallsToAnalyze, LocalCalls, NewCode1, RaceList,
            InitFun, NewVarArgTypes, RaceWarnTag, State) ++
	    fixup_race_forward(InitFun, Fun, InterModuleCalls, 
            NewModuleLocalCallsToAnalyze, LocalCalls, NewCode2, RaceList,
            InitFun, NewVarArgTypes, RaceWarnTag, State);
        {CurrFun, CurrFunLabel, TupleB, Fun, ArgsB, _CodeA, empty, _Bool} -> %TO FIX
          {NewModuleLocalCallsToAnalyze, NewVarArgTypes} =
            case TupleB of
              InitFun ->
                {lists:delete(Head, ModuleLocalCallsToAnalyze),
                  race_type_analysis(ArgsB, FunArgTypes, VarArgTypes,
                  RaceWarnTag)};
              _Other ->
                {lists:delete(Head, ModuleLocalCallsToAnalyze), VarArgTypes}
            end,
	  fixup_intra_module_race_forward_helper(CurrFun, CurrFunLabel, TupleB,
            Fun, InterModuleCalls, NewModuleLocalCallsToAnalyze, LocalCalls,
            LocalCalls, Code, RaceList, InitFun, NewVarArgTypes,
            NewVarArgTypes, RaceWarnTag, State);
        {CurrFun, CurrFunLabel, TupleB, Fun, ArgsB, _CodeA, CodeB, true} ->
          {NewModuleLocalCallsToAnalyze, NewVarArgTypes, NewCode} =
            case TupleB of
              InitFun ->
                {lists:delete(Head, ModuleLocalCallsToAnalyze),
                  race_type_analysis(ArgsB, FunArgTypes, VarArgTypes,
                  RaceWarnTag), lists:reverse(Races#dialyzer_races.race_list)
                  ++ fixup_calls(CurrFun, TupleB, Fun, VarArgTypes, ArgsB,
                  RaceWarnTag, lists:reverse([{curr, CurrFun, CurrFunLabel,
                  VarArgTypes}|Races#dialyzer_races.race_list]), Code)};
              _Other ->
                {lists:delete(Head, ModuleLocalCallsToAnalyze), VarArgTypes,
                  CodeB ++ fixup_calls(CurrFun, TupleB, Fun, VarArgTypes,
                  ArgsB, RaceWarnTag, CodeB ++
                  [{curr, CurrFun, CurrFunLabel, VarArgTypes}], Code)}
            end,
	  fixup_race_forward(TupleB, Fun, InterModuleCalls, 
            NewModuleLocalCallsToAnalyze, LocalCalls, NewCode, RaceList,
            InitFun, NewVarArgTypes, RaceWarnTag, State) ++
            case {TupleB, is_self_recursive(CodeB, TupleB, Fun)} of
              {InitFun, true} ->
                fixup_race_forward(TupleB, Fun, InterModuleCalls,
                  NewModuleLocalCallsToAnalyze, LocalCalls,
                  CodeB ++ fixup_calls(CurrFun, TupleB, Fun, VarArgTypes,
                  ArgsB, RaceWarnTag, CodeB ++
                  [{curr, CurrFun, CurrFunLabel, VarArgTypes}], Code),
                  RaceList, InitFun, VarArgTypes, RaceWarnTag, State);
              _Else -> []
            end;
	{_TupleA, _IntA, _TupleB, _IntB, _ArgsB, _CodeA, _CodeB, _Bool} ->
	  fixup_intra_module_race_forward(CurrFun, CurrFunLabel, Fun,
            InterModuleCalls, Tail, ModuleLocalCallsToAnalyze, LocalCalls,
            Code, RaceList, InitFun, VarArgTypes, FunArgTypes, RaceWarnTag,
            State)
      end
  end.

fixup_intra_module_race_forward_helper(CurrFun, CurrFunLabel, FunName, Fun,
                                       InterModuleCalls, ModuleLocalCalls,
                                       LocalCalls, LocalCallsToAnalyze, Code,
                                       RaceList, InitFun, VarArgTypes,
                                       NewVarArgTypes, RaceWarnTag,
                                       #state{races = Races} = State) ->
  case LocalCalls of
    [] -> [];
    [Head|Tail] ->
      case Head of
        {Fun, Args, FunRaceList, true} ->
          NewCode =
            case FunName of
              InitFun -> 
                lists:reverse(Races#dialyzer_races.race_list) ++
                  fixup_calls(CurrFun, FunName, Fun, NewVarArgTypes, Args,
                  RaceWarnTag,
                  lists:reverse([{curr, CurrFun, CurrFunLabel, NewVarArgTypes}|
                  Races#dialyzer_races.race_list]), Code);
              _Other -> 
                lists:reverse(FunRaceList) ++ fixup_calls(CurrFun, FunName,
                  Fun, NewVarArgTypes, Args, RaceWarnTag,
                  lists:reverse([{curr, CurrFun, CurrFunLabel, NewVarArgTypes}|
                  FunRaceList]), Code)
            end,
          case {CurrFun, FunName} of
            {InitFun, InitFun} ->
              CodeToReplace =
                lists:reverse([{curr, InitFun, CurrFunLabel, VarArgTypes}|
                FunRaceList]),
              NewCode1 =
                fixup_calls(InitFun, InitFun, Fun, VarArgTypes, Args,
                RaceWarnTag, CodeToReplace, NewCode),
              NewCode2 = 
                fixup_calls(InitFun, InitFun, Fun, NewVarArgTypes, Args,
                RaceWarnTag,
                lists:reverse([{curr, InitFun, CurrFunLabel, NewVarArgTypes}|
                Races#dialyzer_races.race_list]),
                lists:reverse(FunRaceList) ++ fixup_calls(InitFun, InitFun,
                Fun, VarArgTypes, Args, RaceWarnTag, CodeToReplace, Code)),
              fixup_race_forward(InitFun, Fun, InterModuleCalls,
                ModuleLocalCalls, LocalCalls, NewCode1, RaceList, InitFun,
                NewVarArgTypes, RaceWarnTag, State) ++
                fixup_race_forward(InitFun, Fun, InterModuleCalls,
                ModuleLocalCalls, LocalCalls, NewCode2, RaceList, InitFun,
                NewVarArgTypes, RaceWarnTag, State);              
            _Else ->
              fixup_race_forward(FunName, Fun, InterModuleCalls,
                ModuleLocalCalls, LocalCallsToAnalyze, NewCode, RaceList,
                InitFun, NewVarArgTypes, RaceWarnTag, State)
          end;
	_Other ->
	  fixup_intra_module_race_forward_helper(CurrFun, CurrFunLabel,
            FunName, Fun, InterModuleCalls, ModuleLocalCalls, Tail,
            LocalCallsToAnalyze, Code, RaceList, InitFun, VarArgTypes,
            NewVarArgTypes, RaceWarnTag, State)
      end
  end.

%%% ===========================================================================
%%%
%%%  Inter-Modular Forward Analysis
%%%
%%% ===========================================================================

%% Fixes up the race list according to the inter module calls
%% (forward analysis).
fixup_inter_module_race_forward(CurrFun, CurrFunLabel, Fun, InterModuleCalls,
				InterModuleCallsToAnalyze,
                                ModuleLocalCalls, LocalCalls,
				Code, RaceList, InitFun, VarArgTypes,
                                FunArgTypes, RaceWarnTag,
                                #state{races = Races} = State) ->
  case InterModuleCalls of
    [] -> [];
    [Head|Tail] ->
      case Head of
        {CurrFun, Fun, ArgsB, _C, CodeA, _B, true} -> 
          FunLabel = get_fun_label(Fun, ModuleLocalCalls),
          {NewInterModuleCallsToAnalyze, NewVarArgTypes, NewCode} =
            case Fun of
              InitFun ->
                {lists:delete(Head, InterModuleCallsToAnalyze),
                  race_type_analysis(ArgsB, FunArgTypes, VarArgTypes,
                  RaceWarnTag), lists:reverse(Races#dialyzer_races.race_list)
                  ++ fixup_calls(CurrFun, Fun, FunLabel, VarArgTypes, ArgsB,
                  RaceWarnTag,
                  lists:reverse([{curr, CurrFun, CurrFunLabel, VarArgTypes}|
                  Races#dialyzer_races.race_list]), Code)};
              _Other ->
                {lists:delete(Head, InterModuleCallsToAnalyze), VarArgTypes,
                  CodeA ++ fixup_calls(CurrFun, Fun, FunLabel, VarArgTypes,
                  ArgsB, RaceWarnTag, CodeA ++
                  [{curr, CurrFun, CurrFunLabel, VarArgTypes}], Code)} 
            end,
	  fixup_race_forward(Fun, FunLabel, NewInterModuleCallsToAnalyze,
            ModuleLocalCalls, LocalCalls, NewCode, RaceList,
            InitFun, NewVarArgTypes, RaceWarnTag, State);
        {Fun, _F2, _ArgsB, _C1, _C2, false, _B2} -> [];
        {_F1, Fun, _ArgsB, _C1, _C2, _B1, false} -> [];
	{_F1, _F2, _ArgsB, _C1, _C2, _B1, _B2} ->
	  fixup_inter_module_race_forward(CurrFun, CurrFunLabel, Fun, Tail,
            InterModuleCallsToAnalyze, ModuleLocalCalls, LocalCalls, Code,
            RaceList, InitFun, VarArgTypes, FunArgTypes, RaceWarnTag, State)
      end
  end.

%%% ===========================================================================
%%%
%%%  Intra-Modular Backward Analysis
%%%
%%% ===========================================================================

fixup_intra_module_race_backward(CurrFun, InterModuleCalls, ModuleLocalCalls,
                                 ModuleLocalCallsToAnalyze, Tested,
                                 VarArgTypes, RaceWarnTag,
                                 #state{callgraph = Callgraph,
					races = Races} = State) ->
  case ModuleLocalCalls of
    [] ->
      case is_called_intermodularly(CurrFun, InterModuleCalls) of
        true ->
          fixup_inter_module_race_backward(CurrFun, InterModuleCalls,
            InterModuleCalls, ModuleLocalCallsToAnalyze, Tested, VarArgTypes,
            RaceWarnTag, State);
        false -> 
          Code = get_code(CurrFun,
            Callgraph#dialyzer_callgraph.inter_module_calls,
            Callgraph#dialyzer_callgraph.module_local_calls,
            Races#dialyzer_races.local_calls),
          NewCode =
            case Races#dialyzer_races.curr_fun of
              CurrFun ->
                case Code of 
                  [] -> lists:reverse(Races#dialyzer_races.race_list);
                  _Other -> Code
                end;
              _Other -> Code
            end,
          case lists:member(CurrFun, Tested) of
            false ->
              case is_integer(CurrFun) of
                false ->
                  {fixup_race_forward(CurrFun, get_fun_label(CurrFun,
                    Callgraph#dialyzer_callgraph.module_local_calls),
                    Callgraph#dialyzer_callgraph.inter_module_calls,
                    Callgraph#dialyzer_callgraph.module_local_calls,
                    Races#dialyzer_races.local_calls, NewCode, [],
                    Races#dialyzer_races.curr_fun, VarArgTypes,
                    RaceWarnTag, State), [CurrFun|Tested]};
                true -> {[], [CurrFun|Tested]}
              end;
            true -> {[], Tested}
          end
      end;
    [Head|Tail] ->
      MorePaths =
        case Head of
          {Parent, IntA, CurrFun, _IntB, _ArgsB, empty, _CodeB, true} ->
            is_stored_locally(IntA, Races#dialyzer_races.local_calls);
          {Parent, _IntA, CurrFun, _IntB, _ArgsB, _CodeA, _CodeB, true} ->
            true;
          {Parent, _IntA, _TupleB, _IntB, _ArgsB, _CodeA, _CodeB, _Bool} ->
            false
        end,
      case MorePaths of
        true ->
          NewModuleLocalCallsToAnalyze =
            lists:delete(Head, ModuleLocalCallsToAnalyze),
          {DepList1, NewTested1} =
            case Tested of
              [] -> 
                fixup_intra_module_race_backward(Parent, InterModuleCalls,
                  NewModuleLocalCallsToAnalyze, NewModuleLocalCallsToAnalyze,
                  Tested, VarArgTypes, RaceWarnTag, State);
              _Else ->
                case Parent of
                  CurrFun -> {[], Tested};
                  _Other ->
                    fixup_intra_module_race_backward(Parent, InterModuleCalls,
                      NewModuleLocalCallsToAnalyze,
                      NewModuleLocalCallsToAnalyze, Tested, VarArgTypes,
                      RaceWarnTag, State)
                end
            end,
          {DepList2, NewTested2} = 
            case is_called_intramodularly(CurrFun, Tail) of
              true ->
                fixup_intra_module_race_backward(CurrFun, InterModuleCalls,
                  Tail, NewModuleLocalCallsToAnalyze, NewTested1, VarArgTypes,
                  RaceWarnTag, State);
              false -> {[], NewTested1}
            end,
          {DepList3, NewTested3} =
            case is_called_intermodularly(CurrFun, InterModuleCalls) of
              true ->
                fixup_inter_module_race_backward(CurrFun, InterModuleCalls,
                  InterModuleCalls, NewModuleLocalCallsToAnalyze, NewTested2,
                  VarArgTypes, RaceWarnTag, State);
              false -> {[], NewTested2}
            end,
            {DepList1 ++ DepList2 ++ DepList3, NewTested3};
        false ->
          fixup_intra_module_race_backward(CurrFun, InterModuleCalls, Tail,
            ModuleLocalCallsToAnalyze, Tested, VarArgTypes, RaceWarnTag, State)
      end
  end.

%%% ===========================================================================
%%%
%%%  Inter-Modular Backward Analysis
%%%
%%% ===========================================================================

%% Fixes up the race list according to the inter module calls
%% (backward analysis).
fixup_inter_module_race_backward(CurrFun, InterModuleCalls,
                                 InterModuleCallsToAnalyze, ModuleLocalCalls,
                                 Tested, VarArgTypes, RaceWarnTag,
                                 #state{callgraph = Callgraph,
					races = Races} = State) ->
  case InterModuleCalls of
    [] ->
      case is_called_intramodularly(CurrFun, ModuleLocalCalls) of
        true ->
          fixup_intra_module_race_backward(CurrFun, InterModuleCallsToAnalyze,
            ModuleLocalCalls, ModuleLocalCalls, Tested, VarArgTypes,
            RaceWarnTag, State);
        false ->
          Code = get_code(CurrFun,
            Callgraph#dialyzer_callgraph.inter_module_calls,
            Callgraph#dialyzer_callgraph.module_local_calls,
            Races#dialyzer_races.local_calls),
          NewCode =
            case Races#dialyzer_races.curr_fun of
              CurrFun -> 
                case Code of 
                  [] -> lists:reverse(Races#dialyzer_races.race_list);
                  _Other -> Code
                end;
              _Other -> Code
            end,
          case lists:member(CurrFun, Tested) of
            false ->
              case is_integer(CurrFun) of
                false ->
                  {fixup_race_forward(CurrFun, get_fun_label(CurrFun,
                    Callgraph#dialyzer_callgraph.module_local_calls),
                    Callgraph#dialyzer_callgraph.inter_module_calls,
                    Callgraph#dialyzer_callgraph.module_local_calls,
                    Races#dialyzer_races.local_calls, NewCode, [],
                    Races#dialyzer_races.curr_fun, VarArgTypes,
                    RaceWarnTag, State), [CurrFun|Tested]};
                true -> {[], [CurrFun|Tested]}
              end;
            true -> {[], Tested}
          end
      end;
    [Head|Tail] ->
      MorePaths =
        case Head of
          {Parent, CurrFun, _ArgsB, _C1, _C2, _B1, true} -> true;
          {Parent, CurrFun, _ArgsB, [{Parent, CurrFun, _ArgTypes}|_RaceList],
            _C2, _B1, _B2} ->
            true;
  	  {Parent, _F2, _ArgsB, _C1, _C2, _B1, _B2} -> false
        end,
      case MorePaths of
        true ->
          NewInterModuleCallsToAnalyze =
            lists:delete(Head, InterModuleCallsToAnalyze),
          {DepList1, NewTested1} =
            fixup_inter_module_race_backward(Parent,
            NewInterModuleCallsToAnalyze, NewInterModuleCallsToAnalyze,
            ModuleLocalCalls, Tested, VarArgTypes, RaceWarnTag, State),
          {DepList2, NewTested2} =
            case is_called_intermodularly(CurrFun, Tail) of
              true ->
                fixup_inter_module_race_backward(CurrFun, Tail,
                  NewInterModuleCallsToAnalyze, ModuleLocalCalls, NewTested1,
                  VarArgTypes, RaceWarnTag, State);
              false -> {[], NewTested1}
            end,
          {DepList3, NewTested3} =
            case is_called_intramodularly(CurrFun, ModuleLocalCalls) of
              true ->
                fixup_intra_module_race_backward(CurrFun,
                  NewInterModuleCallsToAnalyze, ModuleLocalCalls,
                  ModuleLocalCalls, NewTested2, VarArgTypes, RaceWarnTag,
                  State);
              false -> {[], NewTested2}
            end,
          {DepList1 ++ DepList2 ++ DepList3, NewTested3};
        false ->
          fixup_inter_module_race_backward(CurrFun, Tail,
            InterModuleCallsToAnalyze, ModuleLocalCalls,
            Tested, VarArgTypes, RaceWarnTag, State)
      end
  end.

%%% ===========================================================================
%%%
%%%  Inter-Modular Analysis -- Code Storing
%%%
%%% ===========================================================================

%% Stores suspicious inter-modular calls
%% and other inter-modular calls defined by the user.
-spec inter_module_race(mfa_or_funlbl(), [erl_type()], args(), file_line(), #state{}) ->
                        #state{}.

inter_module_race(Fun, ArgTypes, Args, FileLine,
                  #state{callgraph = Callgraph, races = Races} = State) ->
  case Callgraph#dialyzer_callgraph.inter_module_calls of
    [] -> State;
    Calls -> 
      CurrFun = Races#dialyzer_races.curr_fun,
      {CodeToAdd, IsSame} =
        case Fun of
          {erlang, whereis, 1} ->
            State1 = cleanup_suspicious_calls_for_storing(State),
            {{whereis, format_args(Args, ArgTypes, whereis), ArgTypes, Args,
              State1, FileLine}, false};
          {ets, lookup, 2} ->
            NewLList = format_args(Args, ArgTypes, ets_lookup),
            State1 = cleanup_suspicious_calls_for_storing(State),
            {{ets_lookup, NewLList, ArgTypes, Args, State1, FileLine}, false};
          {erlang, register, 2} ->
	    {{register, format_args(Args, ArgTypes, register)}, false};
          {ets, insert, 2} ->
            {{ets_insert, format_args(Args, ArgTypes, ets_insert)}, false};
          {_M, _F, _A} = MFA ->
	    case is_inter_module_call(Fun, Calls) of
	      true  -> {{Races#dialyzer_races.curr_fun, MFA, ArgTypes}, false};
	      false -> {{}, true}
	    end;
          Int when is_integer(Int) ->
            {{Races#dialyzer_races.curr_fun, Int, ArgTypes}, false};
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
-spec store_code(rcommand(), mfa_or_funlbl(), inter_module_calls()) ->
                 inter_module_calls().

store_code(Code, CurrFun, InterModuleCalls) ->
  case InterModuleCalls of
    [] -> [];
    [Head|Tail] ->
      NewHead =
        case Head of
  	  {CurrFun, TupleB, ArgsB, ListA, ListB, false, BoolB} ->
	    {CurrFun, TupleB, ArgsB, [Code|ListA], ListB, false, BoolB};
	  {TupleA, CurrFun, ArgsB, ListA, ListB, BoolA, false} ->
	    {TupleA, CurrFun, ArgsB, ListA, [Code|ListB], BoolA, false};
	  {_TupleA, _TupleB, _ArgsB, _ListA, _ListB, _BoolA, _BoolB} -> Head
        end,
      [NewHead|store_code(Code, CurrFun, Tail)]
  end.

%% Turns the 4th element of the tuple of the current function
%% (#dialyzer_callgraph.inter_module_calls field) true if we have
%% reached the end of the function.
-spec end_of_code_storing(mfa_or_funlbl(), inter_module_calls()) ->
                          inter_module_calls().

end_of_code_storing(CurrFun, InterModuleCalls) ->
  case InterModuleCalls of
    [] -> [];
    [Head|Tail] ->
      {NewHead, IsSame} =
        case Head of
  	  {CurrFun, TupleB, ArgsB, ListA, ListB, false, BoolB} ->
            {{CurrFun, TupleB, ArgsB, lists:reverse(ListA), ListB, true,
              BoolB}, false};
	  {TupleA, CurrFun, ArgsB, ListA, ListB, BoolA, false} ->
            {{TupleA, CurrFun, ArgsB, ListA, lists:reverse(ListB), BoolA,
              true}, false};
          {CurrFun, _TupleB, _ArgsB, _ListA, _ListB, true, _BoolB} ->
            {InterModuleCalls, true};
          {_TupleA, CurrFun, _ArgsB, _ListA, _ListB, _BoolA, true} ->
            {InterModuleCalls, true};
	  {_TupleA, _TupleB, _ArgsB, _ListA, _ListB, _BoolA, _BoolB} ->
            {Head, false}
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

%% Cleans up empty clauses.
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
            {curr, _CurrFun, _CurrFunLabel, _VarArgTypes} ->
              cleanup_clause_code(Head, Tail, NewNestingLevel);
            _Else -> cleanup_clause_code(CurrTuple, Tail, NewNestingLevel)
          end
      end
  end.

%% Cleans up the state record that is about to be stored
%% for space efficiency.
cleanup_suspicious_calls_for_storing(#state{records = Records}) ->
  #state{records = Records}.

%% Compares argument types of the two suspicious calls.
compare_lists(AList, BList, RaceWarnTag) ->
  case RaceWarnTag of
    ?WARN_WHEREIS_REGISTER ->
      AListElem = lists:nth(2, AList),
      BListElem = lists:nth(2, BList),
      length(AListElem -- BListElem) =/= length(AListElem);
    ?WARN_ETS_LOOKUP_INSERT ->
      AListElem1 = lists:nth(2, AList),
      BListElem1 = lists:nth(2, BList),
      AListElem2 = lists:nth(4, AList),
      BListElem2 = lists:nth(4, BList),
      length(AListElem1 -- BListElem1) =/= length(AListElem1) andalso
        length(AListElem2 -- ets_tuple_args(BListElem2)) =/=
        length(AListElem2)
  end.

fixup_calls(CurrFun, NextFun, NextFunLabel, VarArgTypes, Args,
            RaceWarnTag, CodeToReplace, Code) ->
  case Code of
    [] -> [];
    [Head|Tail] ->
      NewCode =
        case Head of
          {CurrFun, NextFun, ArgTypes} ->
            [{curr, NextFun, NextFunLabel, race_type_analysis(Args, ArgTypes,
              VarArgTypes, RaceWarnTag)}|CodeToReplace];
          {CurrFun, NextFunLabel, ArgTypes} ->
            [{curr, NextFun, NextFunLabel, race_type_analysis(Args, ArgTypes,
              VarArgTypes, RaceWarnTag)}|CodeToReplace];
          _Other -> [Head]
        end,
      NewCode ++ fixup_calls(CurrFun, NextFun, NextFunLabel, VarArgTypes, Args,
         RaceWarnTag, CodeToReplace, Tail)
  end.

fixup_empty_module_local_call(RaceList, CurrTuple, Code) -> 
  NewRaceList = fixup_case_rest_paths_forward(RaceList, 0),
  {NewCurrTuple, NewCode} =
    cleanup_clause_code(CurrTuple, Code, 0),
  ReturnTuple = {NewRaceList, NewCurrTuple, NewCode},
  case NewRaceList of
    [beg_case|RTail] ->
      case NewCode of
        [end_case|CTail] ->
          fixup_empty_module_local_call(RTail, NewCurrTuple, CTail);
        _Other -> ReturnTuple
      end;
    _Else -> ReturnTuple
  end.

get_code(CurrFun, InterModuleCalls, ModuleLocalCalls, LocalCalls) ->
  case InterModuleCalls of
    [] -> get_code_helper1(CurrFun, ModuleLocalCalls, LocalCalls);
    [Head|Tail] ->
      case Head of
        {CurrFun, _F2, _ArgsB, Code, _C2, true, true} -> Code;
        {_F1, CurrFun, _ArgsB, _C1, Code, true, true} -> Code;
        {_F1, _F2, _ArgsB, _C1, _C2, _B1, _B2} ->
          get_code(CurrFun, Tail, ModuleLocalCalls, LocalCalls)
      end
  end.

get_code_helper1(CurrFun, ModuleLocalCalls, LocalCalls) ->
  case ModuleLocalCalls of
    [] -> [];
    [Head|Tail] ->
      case Head of
        {CurrFun, Int, _TupleB, _IntB, _ArgsB, empty, _CodeB, true} ->
          get_code_helper2(Int, LocalCalls);
        {_TupleA, _IntA, CurrFun, Int, _ArgsB, _CodeA, empty, true} ->
          get_code_helper2(Int, LocalCalls);
        {CurrFun, _IntA, _TupleB, _IntB, _ArgsB, Code, _CodeB, true} -> Code;
        {_TupleA, _IntA, CurrFun, _IntB, _ArgsB, _CodeA, Code, true} -> Code;
        {_TupleA, _IntA, _TupleB, _IntB, _ArgsB, _CodeA, _CodeB, _Bool} ->
          get_code_helper1(CurrFun, Tail, LocalCalls)
      end
  end.
     
get_code_helper2(Int, LocalCalls) ->
  case LocalCalls of
    [] -> [];
    [Head|Tail] ->
      case Head of
        {Int, _Args, RaceList, true} -> lists:reverse(RaceList);
        _Other -> get_code_helper2(Int, Tail)
      end
  end.

get_fun_label(Fun, ModuleLocalCalls) ->
  case ModuleLocalCalls of
    [] -> empty;
    [Head|Tail] ->
      case Head of
        {Fun, Int, _TupleB, _IntB, _ArgsB, _CodeA, _CodeB, _Bool} -> Int;
        {_TupleA, _IntA, Fun, Int, _ArgsB, _CodeA, _CodeB, _Bool} -> Int;
        _Other -> get_fun_label(Fun, Tail)
      end
  end.

is_called_intermodularly(CurrFun, InterModuleCalls) ->
  case InterModuleCalls of
    [] -> false;
    [Head|Tail] ->
      case Head of
        {_F1, CurrFun, _ArgsB, _C1, _C2, _B1, true} -> true;
        {_F1, _F2, _ArgsB, _C1, _C2, _B1, _B2} ->
          is_called_intermodularly(CurrFun, Tail)
      end
  end.

is_called_intramodularly(CurrFun, ModuleLocalCalls) ->
  case ModuleLocalCalls of
    [] -> false;
    [Head|Tail] ->
      case Head of
        {_TupleA, _IntA, CurrFun, _IntB, _ArgsB, _CodeA, _CodeB, true} -> true;
        {_TupleA, _IntA, _TupleB, _IntB, _ArgsB, _CodeA, _CodeB, _Bool} ->
          is_called_intramodularly(CurrFun, Tail)
      end
  end.

is_self_recursive(Code, Fun, Int) ->
  case Code of
    [] -> false;
    [Head|Tail] ->
      case Head of
        {Fun, Int, _ArgTypes} -> true;
        _Other -> is_self_recursive(Tail, Fun, Int)
      end
  end.

is_stored_locally(Int, LocalCalls) ->
  case LocalCalls of
    [] -> false;
    [Head|Tail] ->
      case Head of
        {Int, _Args, _RaceList, true} -> true;
        _Other -> is_stored_locally(Int, Tail)
      end
  end.

is_user_defined_inter_module_call(Fun, InterModuleCalls) ->
  case InterModuleCalls of
    [] -> true;
    [Head|Tail] ->
      case Head of
        {_F1, Fun, _ArgsB, _C1, _C2, _B1, true} -> true;
        {Fun, _F2, _ArgsB, _C1, _C2, true, _B2} -> true;
        {_F1, Fun, _ArgsB, _C1, [], _B1, false} -> false;
	%% {Fun, _F2, _ArgsB, [], _C2, false, _B2} -> false;
        {_F1, Fun, _ArgsB, _C1, _C2, _B1, false} -> true;
        {Fun, _F2, _ArgsB, _C1, _C2, false, _B2} -> true;
        {_F1, _F2, _ArgsB, _C1, _C2, _B1, _B2} ->
          is_user_defined_inter_module_call(Fun, Tail)
      end
  end.

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

lists_key_replace(N, [Head|Tail], NewMember) ->
  case N =:= 1 of 
    true -> [NewMember|Tail];
    false -> [Head|lists_key_replace(N - 1, Tail, NewMember)]
  end.

races__renew_race_list(RaceList, Races) ->
  Races#dialyzer_races{race_list = RaceList}.

%% Sorts out the necessary suspicious calls from the unnecessary.
refine_race(Race, CompareList, RaceWarnTag, DependencyList) ->
  case RaceWarnTag of 
    ?WARN_WHEREIS_REGISTER ->
      case Race of
        {whereis, WList, _ArgTypes, _Args, _S, _FileLine} ->
          case compare_lists(WList, CompareList, ?WARN_WHEREIS_REGISTER) of
            true -> [Race|DependencyList];
            false -> DependencyList
          end;
        {ets_lookup, _LList, _ArgTypes, _Args, _S, _FileLine} ->
          DependencyList
      end;
    ?WARN_ETS_LOOKUP_INSERT ->
      case Race of
        {whereis, _WList, _ArgTypes, _Args, _S, _FileLine} -> DependencyList;
        {ets_lookup, LList, _ArgTypes, _Args, _S, _FileLine} ->
          case compare_lists(LList, CompareList,
                             ?WARN_ETS_LOOKUP_INSERT) of
            true -> [Race|DependencyList];
            false -> DependencyList
          end
      end
  end.    

state__renew_race_list(RaceList, #state{races = Races} = State) ->
  State#state{races = races__renew_race_list(RaceList, Races)}.

%%% ===========================================================================
%%%
%%%  Intra-modular Utilities
%%%
%%% ===========================================================================

%% Checks whether a call belongs to the body of a module local call.
is_in_local_fun(CurrFun, LocalCalls) ->
  case LocalCalls of
    [] -> false;
    [Head|Tail] ->
      case Head of
        {CurrFun, _Args, FunRaceList, processing} -> FunRaceList;
        _ -> is_in_local_fun(CurrFun, Tail)
      end
  end.

races__find_local_calls(LocalCall, LocalCallRaceList, LocalCalls) ->
  case LocalCalls of
    [] -> [];
    [Head|Tail] ->
      case Head of
        {LocalCall, Args, _LocalCallRaceList, Bool}
          when Bool =:= false orelse Bool =:= processing ->
          [{LocalCall, Args, LocalCallRaceList, Bool}|Tail];
        _Other ->
          [Head|races__find_local_calls(LocalCall, LocalCallRaceList, Tail)]
      end
  end.

races__renew_local_calls(LocalCalls, Races) ->
  Races#dialyzer_races{local_calls = LocalCalls}.

state__renew_local_calls(LocalCall, LocalCallRaceList,
                         #state{races = Races} = State) ->
  State#state{races =
    races__renew_local_calls(
    races__find_local_calls(LocalCall, LocalCallRaceList,
    Races#dialyzer_races.local_calls), Races)}.

%%% ===========================================================================
%%%
%%%  Inter-modular Utilities
%%%
%%% ===========================================================================

%% Checks whether the current function is one of the inter-module
%% calls (#dialyzer_callgraph.inter_module_calls field).
is_inter_module_call(Fun, InterModuleCalls) ->
  case InterModuleCalls of
    [] -> false;
    [Head|Tail] ->
      case Head of
	{Fun, _TB, _AB, _LA, _LB, _BA, _BB} -> true;
	{_TA, Fun, _AB, _LA, _LB, _BA, _BB} -> true;
	{_TupleA, _TupleB, _ArgsB, _ListA, _ListB, _BoolA, _BoolB} ->
	  is_inter_module_call(Fun, Tail)
      end
  end.
                
%%% ===========================================================================
%%%
%%%  Type Utilities
%%%
%%% ===========================================================================

ets_tuple_args(TupleList) ->
  case TupleList of
    [] -> [];
    [Head|Tail] ->
      Head1 = string:strip(Head, left, $[),
      Head2 = string:strip(Head1, left, ${),
      [Head3|_T] = string:tokens(Head2, " ,"),
      NewHead = replace_any_types(string:tokens(Head3, " |")),
      NewHead ++ ets_tuple_args(Tail)
  end.

format_arg(Arg) ->
  Default = "",
  case cerl:is_c_var(Arg) of
    true ->
      case cerl:var_name(Arg) of
        Atom when is_atom(Atom) ->
          case atom_to_list(Atom) of
            "cor"++_ -> Default;
            "rec"++_ -> Default;
            Name -> Name
          end;
        _What -> Default
      end;
    false ->
      Default
  end.

format_args([], [], _Call) ->
  [];
format_args(ArgList, TypeList, Call) ->
  format_args_2(format_args_1(ArgList, TypeList), Call).

format_args_1([Arg], [Type]) ->
  [format_arg(Arg), format_type(Type)];
format_args_1([Arg|Args], [Type|Types]) ->
  List =
    case cerl:is_literal(Arg) of
      true -> ["", format_cerl(Arg)];
      false -> [format_arg(Arg), format_type(Type)]
    end,
  List ++ format_args_1(Args, Types).

format_args_2(StrArgList, Call) ->
  case Call of
    whereis ->
      lists_key_replace(2, StrArgList,
        replace_any_types(string:tokens(lists:nth(2, StrArgList), " |")));
    register ->
      lists_key_replace(2, StrArgList,
        replace_any_types(string:tokens(lists:nth(2, StrArgList), " |")));
    ets_lookup ->
      StrArgList1 = lists_key_replace(2, StrArgList,
        replace_any_types(string:tokens(lists:nth(2, StrArgList), " |"))),
      lists_key_replace(4, StrArgList1,
        string:tokens(lists:nth(4, StrArgList1), " |"));
    ets_insert ->
      StrArgList1 = lists_key_replace(2, StrArgList,
        replace_any_types(string:tokens(lists:nth(2, StrArgList), " |"))),
      lists_key_replace(4, StrArgList1,
        string:tokens(lists:nth(4, StrArgList1), " }"));
    function_call -> StrArgList
  end.

format_cerl(Tree) ->
  cerl_prettypr:format(cerl:set_ann(Tree, []),
                       [{hook, dialyzer_utils:pp_hook()},
                        {noann, true},
                        {paper, 100000},
                        {ribbon, 100000}
                       ]).

format_type(Type) -> t_to_string(Type).

race_type_analysis(FunDef, FunCall, RaceArgs, RaceWarnTag) ->
  FunArgs = format_args(FunDef, FunCall, function_call),
  AnyArgs = [t_to_string(t_any())],
  [Head|_Tail] = RaceArgs,
  NewRaceArgs =
    case lists_key_member(Head, FunArgs, 0) of
      0 -> RaceArgs;
      N1 when is_integer(N1) ->
        NewRaceType1 =
          replace_any_types(string:tokens(lists:nth(N1 + 1, FunArgs), " |")),
        case lists:nth(2, RaceArgs) of
          AnyArgs -> RaceArgs;
          _Other1 -> lists_key_replace(2, RaceArgs, NewRaceType1)
        end
    end,
  case RaceWarnTag of
    ?WARN_ETS_LOOKUP_INSERT ->
      Third = lists:nth(3, NewRaceArgs),
      case lists_key_member(Third, FunArgs, 0) of
        0 -> NewRaceArgs;
        N2 when is_integer(N2) ->
          NewRaceType2 =
            replace_any_types(string:tokens(lists:nth(N2 + 1, FunArgs), " }")),
          case lists:nth(2, NewRaceArgs) of
            AnyArgs -> NewRaceArgs;
            _Other2 -> lists_key_replace(2, NewRaceArgs, NewRaceType2)
          end
      end;
    _Else -> NewRaceArgs
  end.

replace_any_types(StrList) ->
  case StrList of
    [] -> [];
    [Head|Tail] ->
      AnyTypes = [t_atom(), t_fun(), t_identifier(), t_nonempty_list(),
                  t_list(), t_integer(), t_number(), t_tuple()],
      StrAnyTypes = [t_to_string(T) || T <- AnyTypes],
      NewHead =
        case lists:member(Head, StrAnyTypes) of
          true -> t_to_string(t_any());
          false -> Head
        end,
      [NewHead|replace_any_types(Tail)]
  end.

%%print(List) ->
%%  case List of
%%    [] -> io:format("\n", []);
%%    [Head|Tail] ->
%%      io:format("~s,, ", [Head]),
%%      print(Tail)
%%  end.
