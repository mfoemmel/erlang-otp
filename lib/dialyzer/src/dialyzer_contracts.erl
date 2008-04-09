%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
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
%% Copyright 2006-2008, Tobias Lindahl and Kostis Sagonas
%%
%% $Id$
%%

-module(dialyzer_contracts).

-export([check_contract/2,
	 check_contracts/3, 
	 contracts_without_fun/3,
	 contract_from_form/2,
	 contract_to_string/1,
	 get_invalid_contract_warnings/3,
	 get_contract_args/1,
	 get_contract_return/1,
	 get_contract_return/2,
	 %% get_contract_signature/1,
	 is_overloaded/1]).

-include("dialyzer.hrl").
-include("dialyzer_callgraph.hrl").

%-define(DEBUG, true).

-ifdef(DEBUG).
-define(debug(X__, Y__), io:format(X__, Y__)).
-else.
-define(debug(X__, Y__), ok).
-endif.

%%-----------------------------------------------------------------------

-spec(get_contract_return/1 :: (#contract{}) -> erl_type()).
get_contract_return(#contract{contracts=Cs, args=GenArgs}) ->
  process_contracts(Cs, GenArgs).

-spec(get_contract_return/2 :: (#contract{}, [erl_type()]) -> erl_type()).
get_contract_return(#contract{contracts=Cs}, Args) ->
  process_contracts(Cs, Args).

-spec(get_contract_args/1 :: (#contract{}) -> [erl_type()]).
get_contract_args(#contract{args=Args}) ->
  Args.

-spec(get_contract_signature/1 :: (#contract{}) -> erl_type()).
get_contract_signature(#contract{contracts=Cs, args=GeneralDomain}) ->
  Range = process_contracts(Cs, GeneralDomain),
  erl_types:t_fun(GeneralDomain, Range).

-spec(is_overloaded/1 :: (#contract{}) -> bool()).
is_overloaded(#contract{contracts=Cs}) ->
  not(Cs =:= []).

-spec(contract_to_string/1 :: (#contract{}) -> string()).
contract_to_string(#contract{forms=Forms}) ->
  contract_to_string_1(Forms).

contract_to_string_1([{Contract, []}]) ->
  strip_fun(erl_types:t_form_to_string(Contract));
contract_to_string_1([{Contract, []}|Rest]) ->
  strip_fun(erl_types:t_form_to_string(Contract)) ++ "; " 
    ++ contract_to_string_1(Rest);
contract_to_string_1([{Contract, Constraints}]) ->
  strip_fun(erl_types:t_form_to_string(Contract)) ++ " when " 
    ++ constraints_to_string(Constraints);
contract_to_string_1([{Contract, Constraints}|Rest]) ->
  strip_fun(erl_types:t_form_to_string(Contract)) ++ " when " 
    ++ constraints_to_string(Constraints) ++ ";" ++
    contract_to_string_1(Rest).

strip_fun("fun(" ++ String) ->
  butlast(String).

butlast([]) -> [];
butlast([_]) -> [];
butlast([H|T]) -> [H|butlast(T)].

constraints_to_string([]) ->
  "";
constraints_to_string([{type, _, constraint, [{atom, _, What}, Types]}]) ->
  atom_to_list(What) ++ "(" ++
    sequence([erl_types:t_form_to_string(T) || T <- Types], ",") ++ ")";
constraints_to_string([{type, _, constraint, [{atom, _, What}, Types]}|Rest]) ->
  atom_to_list(What) ++ "(" ++
    sequence([erl_types:t_form_to_string(T) || T <- Types], ",") 
    ++ "), " ++ constraints_to_string(Rest).

sequence([], _Delimiter) -> "";
sequence([H], _Delimiter) -> H;
sequence([H|T], Delimiter) -> H ++ Delimiter ++ sequence(T, Delimiter).

-spec(check_contracts/3 :: 
      (orddict(), #dialyzer_callgraph{}, dict()) -> [{mfa(), #contract{}}]).

check_contracts(Contracts, Callgraph, FunTypes) ->
  FoldFun =
    fun(Label, Type, NewContracts) -> 
	{ok, MFA = {M,F,A}} = dialyzer_callgraph:lookup_name(Label, Callgraph),
	case orddict:find(MFA, Contracts) of
	  error -> NewContracts;
	  {ok, {_Line, Contract}} -> 
	    case check_contract(Contract, Type) of
	      error -> NewContracts;
	      {error, _Error} -> NewContracts;
	      ok ->
		case erl_bif_types:is_known(M, F, A) of
		  true ->
		    %% Disregard the contracts since 
		    %% this is a known function.
		    NewContracts;
		  false ->
		    [{MFA, Contract}|NewContracts]
		end
	    end
	end
    end, 
  dict:fold(FoldFun, [], FunTypes).

%% Checks all components of a contract
-spec(check_contract/2 ::
      (#contract{}, erl_type()) -> 'ok' | 'error' | {'error',_}).

check_contract(#contract{contracts=Contracts}, SuccType) ->
  try 
    Contracts1 = [{Contract, insert_constraints(Constraints, dict:new())} 
		  || {Contract, Constraints} <- Contracts],
    Contracts2 = [erl_types:t_subst(Contract, Dict)
		  || {Contract, Dict} <- Contracts1],
    GenDomains = [erl_types:t_fun_args(C) || C <- Contracts2],
    case check_domains(GenDomains) of
      error ->
	{error, {overlapping_contract, []}};
      ok -> 
	InfList = [erl_types:t_inf(Contract, SuccType) 
		   || Contract <- Contracts2],
	InfDomains = lists:flatmap(fun erl_types:t_fun_args/1, InfList),
	case erl_types:any_none_or_unit(InfDomains) of
	  true -> error;
	  false ->
	    STRange = erl_types:t_fun_range(SuccType),
	    case erl_types:t_is_none_or_unit(STRange) of
	      true -> ok;
	      false -> 
		GenRanges = [erl_types:t_fun_range(C) || C <- Contracts2],
		case lists:any(fun(Range) -> 
				   InfRange = erl_types:t_inf(STRange, Range),
				   erl_types:t_is_none(InfRange)
			       end, GenRanges) of
		  true -> error;
		  false -> ok
		end
	    end
	end
    end
  catch throw:{error, Error} -> {error, Error}
  end.

check_domains([_]) -> ok;
check_domains([Dom1|Left]) ->
  CheckFun = fun(Dom2) ->
		 erl_types:any_none_or_unit(erl_types:t_inf_lists(Dom1, Dom2))
	     end,
  case lists:all(CheckFun, Left) of
    true -> check_domains(Left);
    false -> error
  end.

%% This is the heart of the "range function"
-spec(process_contracts/2 :: ([{_,_}], [erl_type()]) -> erl_type()).

process_contracts(OverContracts, Args) ->
  process_contracts(OverContracts, Args, erl_types:t_none()).
  
process_contracts([OverContract|Left], Args, AccRange) ->
  NewAccRange = 
    case process_contract(OverContract, Args) of
      error -> AccRange;
      {ok, Range} -> erl_types:t_sup(AccRange, Range)
    end,
  process_contracts(Left, Args, NewAccRange);
process_contracts([], _Args, AccRange) ->
  AccRange.

-spec(process_contract/2 ::
      ({_,_}, [erl_type()]) -> 'error' | {'ok',erl_type()}).

process_contract({Contract, Constraints}, CallTypes0) ->
  CallTypesFun = erl_types:t_fun(CallTypes0, erl_types:t_any()),
  ContArgsFun = erl_types:t_fun(erl_types:t_fun_args(Contract), 
				erl_types:t_any()),
  ?debug("Instance: Contract:  ~s\n          Arguments: ~s\n",
	 [erl_types:t_to_string(ContArgsFun), 
	  erl_types:t_to_string(CallTypesFun)]),
  case solve_constraints(ContArgsFun, CallTypesFun, Constraints) of 
    {ok, VarDict} ->
      {ok, erl_types:t_subst(erl_types:t_fun_range(Contract), VarDict)};
    error -> error
  end.

solve_constraints(Contract, Call, Constraints) ->
  %% First make sure the call follows the constraints
  CDict = insert_constraints(Constraints, dict:new()),
  Contract1 = erl_types:t_subst(Contract, CDict),
  %% Just a safe over-approximation. 
  %% TODO: Find the types for type variables properly
  ContrArgs = erl_types:t_fun_args(Contract1),
  CallArgs = erl_types:t_fun_args(Call),
  InfList = erl_types:t_inf_lists(ContrArgs, CallArgs),
  case erl_types:any_none_or_unit(InfList) of
    true -> error;
    false -> {ok, CDict}
  end.
  %%Inf = erl_types:t_inf(Contract1, Call),
  %% Then unify with the constrained call type.
  %%  ?debug("Call: ~s\n", [erl_types:t_to_string(Call)]),
  %%  ?debug("Contract: ~s\n", [erl_types:t_to_string(Contract)]),
  %%  ?debug("Contract1: ~s\n", [erl_types:t_to_string(Contract1)]),
  %%  ?debug("Inf: ~s\n", [erl_types:t_to_string(Inf)]),
  %%  erl_types:t_assign_variables_to_subtype(Contract, Inf).

%% Checks the contracts for functions that are not implemented
-spec(contracts_without_fun/3 :: 
      (dict(), [_], #dialyzer_callgraph{}) -> [dial_warning()]).

contracts_without_fun(Contracts, AllFuns0, Callgraph) ->
  AllFuns1 = [{dialyzer_callgraph:lookup_name(Label, Callgraph), Arity} 
	      || {Label, Arity} <- AllFuns0],
  AllFuns2 = [{M, F, A} || {{ok,{M,F,_}}, A} <- AllFuns1],
  AllContracts = dict:fetch_keys(Contracts),
  ErrorContracts = lists:subtract(AllContracts, AllFuns2),
  lists:map(fun({M,F,A}) -> 
		File = atom_to_list(M) ++ ".erl",
		{Line, _Contract} = dict:fetch({M,F,A}, Contracts),
 		{?WARN_CONTRACT_SYNTAX, {File, Line}, 
		 {spec_missing_fun, [M, F, A]}}
	    end, ErrorContracts).

%% This treats the "when" constraints. It will be extended
insert_constraints([{subtype, Type1, Type2}|Left], Dict) ->
  case erl_types:t_is_var(Type1) of
    true -> 
      Name = erl_types:t_var_name(Type1),
      case dict:find(Name, Dict) of  
	error -> 
	  Dict1 = dict:store(Name, Type2, Dict);
  	{ok, VarType} -> 
	  Dict1 = dict:store(Name, erl_types:t_inf(VarType, Type2), Dict)
      end;
    false ->
      %% A lot of things should change to add supertypes 
      Dict1 = Dict,
      throw({error, io_lib:format("First argument of is_subtype constraint "
				  "must be a type variable\n",[])})
  end,
  insert_constraints(Left, Dict1);
insert_constraints([], Dict) -> Dict.

-spec(contract_from_form/2 :: ([_], dict()) -> #contract{}).

contract_from_form(Forms, RecDict) ->
  {Cs, Forms1} = contract_from_form(Forms, RecDict, [], []),
  #contract{contracts=Cs, 
	    args=general_domain(Cs),
	    forms=Forms1}.

contract_from_form([Form = {type, _, 'fun', [_, _]} | Left], RecDict, 
		   TypeAcc, FormAcc) ->
  NewTypeAcc = [{erl_types:t_from_form(Form, RecDict), []} | TypeAcc],
  NewFormAcc = [{Form, []}|FormAcc],
  contract_from_form(Left, RecDict, NewTypeAcc, NewFormAcc);
contract_from_form([{type, _L1, bounded_fun, 
		     [Form = {type, _L2, 'fun', [_, _]}, Constr]}| Left],
		   RecDict, TypeAcc, FormAcc) ->
  Constr1 = [constraint_from_form(C, RecDict) || C <- Constr],
  VarDict = insert_constraints(Constr1, dict:new()),
  Fun = erl_types:t_from_form(Form, RecDict, VarDict),
  NewTypeAcc = [{Fun, Constr1} | TypeAcc],
  NewFormAcc = [{Form, Constr} | FormAcc],
  contract_from_form(Left, RecDict, NewTypeAcc, NewFormAcc);
contract_from_form([], _RecDict, TypeAcc, FormAcc) -> 
  {lists:reverse(TypeAcc), lists:reverse(FormAcc)}.

constraint_from_form({type, _, constraint, [{atom,_,is_subtype}, 
					    [Type1, Type2]]}, RecDict) ->
  {subtype, erl_types:t_from_form(Type1, RecDict), 
   erl_types:t_from_form(Type2, RecDict)};
constraint_from_form({type, _, constraint, [{atom,_,What}, List]}, _RecDict) ->
  Arity = length(List),
  throw({error, io_lib:format("Unsupported type guard ~w/~w\n",[What, Arity])}).

%% Gets the most general domain of a list of domains of all 
%% the overloaded contracts

general_domain(List) ->
  general_domain(List, erl_types:t_none()).

general_domain([{Sig, Constraints}|Left], AccSig) ->
  Dict = insert_constraints(Constraints, dict:new()),
  Sig1 = erl_types:t_subst(Sig, Dict),
  general_domain(Left, erl_types:t_sup(AccSig, Sig1));
general_domain([], AccSig) ->
  %% Get rid of all variables in the domain.
  AccSig1 = erl_types:subst_all_vars_to_any(AccSig),
  erl_types:t_fun_args(AccSig1).

-spec(get_invalid_contract_warnings/3 ::
      ([atom()], #dialyzer_codeserver{}, #dialyzer_plt{}) -> [dial_warning()]).

get_invalid_contract_warnings(Modules, CodeServer, Plt) ->
  get_invalid_contract_warnings_modules(Modules, CodeServer, Plt, []).

get_invalid_contract_warnings_modules([Mod|Left], CodeServer, Plt, Acc) ->
  Contracts1 = dialyzer_codeserver:lookup_contracts(Mod, CodeServer),
  Contracts2 = dict:to_list(Contracts1),
  Records = dialyzer_codeserver:lookup_records(Mod, CodeServer),
  NewAcc = get_invalid_contract_warnings_funs(Contracts2, Plt, Records, Acc),
  get_invalid_contract_warnings_modules(Left, CodeServer, Plt, NewAcc);
get_invalid_contract_warnings_modules([], _CodeServer, _Plt, Acc) ->
  Acc.

get_invalid_contract_warnings_funs([{MFA, {FileLine, Contract}}|Left], 
				   Plt, RecDict, Acc) ->
  {value, {Ret, Args}} = dialyzer_plt:lookup(Plt, MFA),
  Sig = erl_types:t_fun(Args, Ret),
  NewAcc =
    case check_contract(Contract, Sig) of
      error -> [invalid_contract_warning(MFA, FileLine, Sig, RecDict)|Acc];
      {error, Msg} -> [{?WARN_CONTRACT_SYNTAX, FileLine, Msg}|Acc];
      ok ->
	{M, F, A} = MFA,
	CSig0 = get_contract_signature(Contract),
	CSig = erl_types:subst_all_vars_to_any(CSig0),
	case erl_bif_types:is_known(M, F, A) of
	  true ->
	    %% This is strictly for contracts in otp.
	    BifArgs = erl_bif_types:arg_types(M, F, A),
	    BifRet = erl_bif_types:type(M, F, A),
	    BifSig = erl_types:t_fun(BifArgs, BifRet),
	    case check_contract(Contract, BifSig) of
	      error -> 
		[invalid_contract_warning(MFA, FileLine, BifSig, RecDict)|Acc];
	      ok ->
		picky_contract_check(CSig, BifSig, MFA, FileLine, 
				     Contract, RecDict, Acc)
	    end;
	  false ->
	    picky_contract_check(CSig, Sig, MFA, FileLine, Contract, 
				 RecDict, Acc)
	end
    end,
  get_invalid_contract_warnings_funs(Left, Plt, RecDict, NewAcc);
get_invalid_contract_warnings_funs([], _Plt, _RecDict, Acc) ->
  Acc.

invalid_contract_warning({M, F, A}, FileLine, Type, RecDict) ->
  {?WARN_CONTRACT_TYPES, FileLine,
   {invalid_contract, [M, F, A, dialyzer_utils:format_sig(Type, RecDict)]}}.
  

picky_contract_check(CSig0, Sig0, MFA, FileLine, Contract, RecDict, Acc) ->
  CSig = erl_types:t_abstract_records(CSig0, RecDict),
  Sig = erl_types:t_abstract_records(Sig0, RecDict),
  case erl_types:t_is_equal(CSig, Sig) of
    true -> Acc;
    false -> 
      case (erl_types:t_is_none(erl_types:t_fun_range(Sig)) andalso
	    erl_types:t_is_unit(erl_types:t_fun_range(CSig))) of
	true -> Acc;
	false ->
	  case extra_contract_warning(MFA, FileLine, Contract, 
				      CSig, Sig, RecDict) of
	    no_warning -> Acc;
	    {warning, Warning} -> [Warning|Acc]
	  end
      end
  end.

extra_contract_warning({M, F, A}, FileLine, Contract, CSig, Sig, RecDict) ->
  SigString = lists:flatten(dialyzer_utils:format_sig(Sig, RecDict)),
  ContractString0 = lists:flatten(dialyzer_utils:format_sig(CSig, RecDict)),
  case SigString =:= ContractString0 of
    true ->
      %% The only difference is in record fields containing 'undefined' or not.
      no_warning;
    false ->
      ContractString = contract_to_string(Contract),
      {Tag, Msg} =
	case erl_types:t_is_subtype(CSig, Sig) of
	  true -> 
	    {?WARN_CONTRACT_SUBTYPE, 
	     {contract_subtype, [M, F, A, ContractString, SigString]}};
	  false ->
	    case erl_types:t_is_subtype(Sig, CSig) of
	      true ->
		{?WARN_CONTRACT_SUPERTYPE, 
		 {contract_supertype, [M, F, A, ContractString, SigString]}};
	      false ->
		{?WARN_CONTRACT_NOT_EQUAL, 
		 {contract_diff, [M, F, A, ContractString, SigString]}}
	    end
	end,
      {warning, {Tag, FileLine, Msg}}
  end.
