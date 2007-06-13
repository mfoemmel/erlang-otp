%% -*- erlang-indent-level: 2 -*-

-module(dialyzer_contracts).

-export([type_fun/4, 
	 type_domain/4, 
	 check_contract/5,
	 check_contract/3, 
	 check_contract/2, 
	 get_contract/2,
	 get_contract_info/2,
	 check_contract_refined/1,
	 delete_contracts/3,
	 pp_type/1]).

%-define(DEBUG, true).
%-define(DEBUG_PP, true).

-ifdef(DEBUG).
-define(debug(X__, Y__), io:format(X__, Y__)).
-else.
-define(debug(X__, Y__), ok).
-endif.


%%---------------------------------------------------------------------
%%--------------------------   Interface   ----------------------------


%% Equivalent to erl_bif_types type/4 function
%% (MFA, Codeserver) -> (Domain -> Range)
%% Output: Fun || error
type_fun(M, F, A, CS) ->
  case get_contract({M, F, A}, CS) of
    error -> io:format("No contract for function ~w/~w\n", [F, A]),
	     error;
    Contract_List -> 
%%      Print = fun([T1,T2]) ->
%%		  T11 = pp_type(T1),
%%		  T22 = pp_type(T2),
%%		  io:format("Mismatch of types:\n Contract: ~s\n Call:  ~s\n", 
%%			    [T11, T22])
%%	      end,
      Fun = fun(Domain) -> 
		Error_queue = queue:new(),
		case process_contracts(Contract_List, Domain, Error_queue) of
		  {none, _New_Error_queue} -> 
%%		    lists:map(Print, queue:to_list(New_Error_queue)),
		    none;
		  {error, _New_Error_queue} ->
%%		    lists:map(Print, queue:to_list(New_Error_queue)),
		    none;
		  {Function, _} -> erl_types:t_fun_range(Function)
		end
	    end,
      {ok, Fun}
  end.


%% (M, F, A, Codeserver) -> Most general form of Contract Domain
%% Output: {of, Domain} || error
type_domain(M, F, A, CS) ->
  case get_contract({M, F, A}, CS) of
    error -> %% io:format("No contract for function ~w/~w", [F, A]),
	     error;
    Contract_List -> 
      Domains = [new_t_from_form_aux(Dom) || [Dom, _Range] <- Contract_List],
      General_domain = contract_sup(Domains),
      {ok, General_domain}
  end.


%% (General_Domain, Range, Success_type) -> ok || error
%% (General_Domain, Range) <- get_contract_info
check_contract(General_Domain, Contract_Range, Success_type) ->
  Success_Domain = erl_types:t_fun_args(Success_type),
  case check_subtype(General_Domain, Success_Domain) of
    error -> 
      io:format("Number of arguments of the contract and 
                  of the function do not match\n"), 
      error;
    {error, C_type, F_Type} -> 
      io:format("Invalid contract. ~s is not a subtype of ~s.\n", 
 		[erl_types:t_to_string(C_type), 
 		 erl_types:t_to_string(F_Type)]),
      error;
    ok -> % everything ok with arguments. Now we check the range
      %% Anoying thing to solve ----> Arguments are in a list, Range not 
      %% but they use same functions
      Success_Range = erl_types:t_fun_range(Success_type),
      case check_subtype([Contract_Range], [Success_Range]) of
 	error -> error;
 	ok -> ok; % everything ok with this function
 	{error, C_type, F_Type} -> 
 	  io:format("Invalid contract. ~s is not a subtype of ~s.\n", 
 		    [erl_types:t_to_string(C_type), 
 		     erl_types:t_to_string(F_Type)]),
 	  error
      end
  end.


check_contract(Contracts, Success_type) ->
  Cont_Funs = 
    [erl_types:t_fun(
       new_t_from_form_aux(Domain),
       erl_types:t_from_form(Range))
     || [Domain, Range] <- Contracts],
  Infimum = process_contracts(Cont_Funs, Success_type),  
  Domains_inf = [erl_types:t_fun_args(Inf) || Inf <- Infimum, 
					      erl_types:t_is_fun(Inf)],
  General_domain = contract_sup(Domains_inf),
  %%Range_inf = [[erl_types:t_fun_range(Inf)] || Inf <- Infimum, 
	%%				       erl_types:t_is_fun(Inf)],
  %%case Range_inf of
  %%  [] -> General_range = none; 
  %%  _Other -> [General_range] = contract_sup(Range_inf)
  %%end,  
  Fun = fun(Args) -> 
	    %%io:format("Applying range_fun for aguments:\n"),
	    %%lists:map(fun(X) -> io:format("Argumento: ~s\n", 
	    %%     [erl_types:t_to_string(X)]) end, Args),
	    Error_queue = queue:new(), 
	    case process_contracts(Contracts, Args, Error_queue) of
	      {none, _New_Error_queue} -> 
		none;
	      {error, _New_Error_queue} -> 
		none;
	      {Function, _} -> 
		case (catch erl_types:t_unify_subtypes(Function,Success_type))of
		  {mismatch, _T1, _T2} -> none;
		  {T, _} ->
		    ?debug("          Unified Final: ~s\n",[pp_type(T)]), 
		    erl_types:t_fun_range(T)
		end
	    end
	end,
  %% We check if the contract is ok
  case General_domain of
    none -> none;
    _Type -> {contract, Fun, General_domain}
  end.


%% (M, F, A, Codeserver, Success_type) -> ok || error 
check_contract(M, F, A, CS, Success_type) ->
  case get_contract({M, F, A}, CS) of
    error -> %% io:format("No contract for function ~w/~w\n", [F, A]),
	     error;
    Contract_List ->   
      check_contract(Contract_List, Success_type)
  end.  


%% (M, F, A, CS) -> error || Contract 
%% Contract = [Domain, Range] 
%% Domain = [Arguments in parse forms]
%% Range = Range in parse form
get_contract({M,F,A}, CS) ->
  Contracts = dialyzer_codeserver:lookup_contracts(M, CS),
  case dict:is_key({F,A}, Contracts) of
    true -> 
      dict:fetch({F,A}, Contracts);
    false ->
      error
  end.


%% (MFA, CS) -> error || {ok, General_domain, Constrains(Range function)}
get_contract_info({M, F, A}, CS) when is_atom(F) ->
  case type_fun(M, F, A, CS) of
    error -> error;
    {ok, Range_Fun} -> 
      case type_domain(M, F, A, CS) of
	error -> error;
	{ok, Domain} -> {ok, Domain, Range_Fun}
      end
  end.

	       
check_contract_refined([{MFA, {value, {Cont_Rang, _Cont_Dom}}, Refined}|Left]) ->
  RefinedRange = erl_types:t_fun_range(Refined),
  %% If the contract range is a subtype of the refined ST range --> type clash
  case erl_types:t_is_subtype(RefinedRange, Cont_Rang) of
    true -> ok;
    false ->
      io:format("Mismach of types in function ~w range:\nContract: ~s\nInferred type:  ~s\n \n",
		[MFA, erl_types:t_to_string(Cont_Rang), 
		 erl_types:t_to_string(RefinedRange)])
  end,
  check_contract_refined(Left);
check_contract_refined([]) ->
   ok.


%%---------------------------------------------------------------
%%---------------------   Auxiliar funs  ------------------------


%%% We dont want the first list to be transformed
new_t_from_form_aux(Args) ->
  lists:map(fun(Arg) -> erl_types:t_from_form(Arg) end, Args).

subst_all_vars_to_any_aux(Args) ->
  lists:map(fun(Arg) -> erl_types:subst_all_vars_to_any(Arg) end, Args).


%%Loops over the different contracts in a function to find
%% the proper range. Two contracts can match with the same calls
%% (one is a subset of the other) so we must do a supremum of the ranges

%% Contracts in parse forms
process_contracts(Contracts_parse, Call_Types, Error_queue) ->
  Contracts = [erl_types:t_fun(new_t_from_form_aux(Cont_Domain),
			       erl_types:t_from_form(Cont_Range))
	       || [Cont_Domain,Cont_Range] <- Contracts_parse],
  Call_Fun = erl_types:t_fun(Call_Types, any),
  process_contracts2(Contracts, Call_Fun, Error_queue).


%% Contracts in erl_types format
%% We only get the 1st successfull contract here, later we'll 
%% have to consider that one contract of the union can be 
%% subset of another
process_contracts2([Contract|Left], CallTypes, Error_queue) ->
  ContArgs = erl_types:t_fun_args(Contract),
  ContArgsFun = erl_types:t_fun(ContArgs, any),
  ?debug("Unifying: Contract ~s\n          Args ~s\n",
	 [pp_type(ContArgsFun), pp_type(CallTypes)]),
  case (catch erl_types:t_unify_subtypes(ContArgsFun, CallTypes)) of
    {mismatch, T1, T2} ->
      New_Error_queue = queue:in([T1, T2], Error_queue), 
      process_contracts2(Left, CallTypes, New_Error_queue);
    {Function0, Parameters} -> 
      FunctionArgs = erl_types:t_fun_args(Function0), 
      FunctionRange = erl_types:t_subst(
			erl_types:t_fun_range(Contract), 
			Parameters),
      Function = erl_types:t_fun(FunctionArgs, FunctionRange),
      ?debug("          Unified C-A: ~s\n", [pp_type(Function)]),
      {Type, New_Error_queue} = 
 	process_contracts2(Left, CallTypes, Error_queue),
      {erl_types:t_sup(Type, Function), New_Error_queue}
  end;
process_contracts2([], _, Error_queue) -> {none, Error_queue}.


%% Version with succ_types
process_contracts(Contracts, Succ_type) ->
  lists:map(
    fun(Contract) ->
	?debug("Unifying: Contract ~s\n          Succ_type ~s\n", 
	       [pp_type(Contract), pp_type(Succ_type)]),
	case (catch erl_types:t_unify_subtypes(Contract, Succ_type)) of
	  {mismatch, _T1, _T2} -> none;
	  {Function, _Parameters} -> 
	    ?debug("          Unified C-ST: ~s\n", [pp_type(Function)]),
	    Function 
	end
    end, Contracts).



check_subtype([Cont_Type|Rest1],[Type|Rest2]) ->
   case erl_types:t_is_subtype(Cont_Type, Type) of
     true -> check_subtype(Rest1, Rest2);
     false -> {error, Cont_Type, Type}
  end;
check_subtype([],[]) -> ok;
check_subtype([],_) -> error;
check_subtype(_,[]) -> error.



%% Takes a list of transformed domains of all the contracts (unions) 
%% of a function
contract_sup([Dom1|Left]) ->
  Fun = fun(D1, D2) ->
	    lists:zipwith(fun(A1, A2) -> 
			      erl_types:t_sup(A1, A2) end,
			  D1, D2)
        end,
  General_domain = lists:foldl(Fun, Dom1, Left),
  subst_all_vars_to_any_aux(General_domain);
contract_sup([]) -> none.



delete_contracts([M|Left], NotFixpoint, Codeserver) -> 
  Contracts = dialyzer_codeserver:lookup_contracts(M, Codeserver),
  New_contracts = 
    dict:filter
      (fun({F,A}, _Contract) -> 
 	   not lists:member({M,F,A}, NotFixpoint)
       end, Contracts),
  Codeserver2 = 
    dialyzer_codeserver:store_contracts(M, New_contracts, Codeserver),  
  delete_contracts(Left, NotFixpoint, Codeserver2);
delete_contracts([], _NotFixpoint, Codeserver) -> Codeserver.



-ifdef(DEBUG_PP).
pp_type(Type) ->
  case cerl:is_literal(Type) of
    true -> io_lib:format("~w", [cerl:concrete(Type)]);
    false -> erl_types:t_to_string(Type)
  end.
-else.
pp_type(_Type) -> 
  ok.
-endif.
