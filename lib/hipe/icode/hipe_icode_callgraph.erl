%% -*- erlang-indent-level: 2 -*-
%%%-------------------------------------------------------------------
%%% File    : hipe_icode_callgraph.erl
%%% Author  : Tobias Lindahl <tobiasl@csd.uu.se>
%%% Description : Create a call graph to find out in what order functions in 
%%%               a module have to be compiled to gain best information in 
%%%               hipe_icode_type.erl.
%%%
%%% Created :  7 Jun 2004 by Tobias Lindahl <tobiasl@csd.uu.se>
%%%
%%% CVS: $Id$
%%%-------------------------------------------------------------------
-module(hipe_icode_callgraph).
-define(NO_UNUSED, true).

-export([construct/1, 
	 get_called_modules/1,
	 is_empty/1,
	 take_first/1,
	 to_list/1]).

-ifndef(NO_UNUSED).
-export([pp/1]).
-endif.

-include("hipe_icode.hrl").
-include("hipe_icode_primops.hrl").

-record(callgraph, {codedict, scc_order}).

is_empty(#callgraph{scc_order=SCCs}) ->
  length(SCCs) =:= 0.

take_first(CG=#callgraph{scc_order=SCCs, codedict=Dict}) when length(SCCs)>0 ->
  [H|T] = SCCs,
  SCCCode = [{X, dict:fetch(X, Dict)}||X <- H],
  {SCCCode, CG#callgraph{scc_order=T}}.

to_list(#callgraph{scc_order=SCCs, codedict=Dict})->
  FlatList = lists:flatten(SCCs),
  [{X, dict:fetch(X, Dict)}||X <- FlatList].

-ifndef(NO_UNUSED).
pp(#callgraph{scc_order=SCCs})->
  io:format("Callgraph ~p\n", [SCCs]).
-endif.

construct(List) ->
  Calls = get_local_calls(List),
  %% io:format("Calls: ~p\n", [lists:keysort(1, Calls)]),
  Edges = get_edges(Calls),  
  %% io:format("Edges: ~p\n", [Edges]),
  DiGraph = hipe_digraph:from_list(Edges),
  SCCs = hipe_digraph:reverse_preorder_sccs(DiGraph),
  #callgraph{scc_order=SCCs, codedict=dict:from_list(List)}.

%%---------------------------------------------------------------------
%% Get the modules called from this module

get_called_modules(List)->
  get_remote_calls(List, []).

get_remote_calls([{_MFA, Icode}|Left], Acc) ->
  CallSet = get_remote_calls_1(hipe_icode:icode_code(Icode), Acc),
  get_remote_calls(Left, ordsets:union(Acc, CallSet));
get_remote_calls([], Acc) ->
  Acc.

get_remote_calls_1([I|Left], Set) ->
  NewSet =
    case I of
      #call{} ->
	case hipe_icode:call_type(I) of
	  remote ->
	    {M, _F, _A} = hipe_icode:call_fun(I),
	    ordsets:add_element(M, Set);
	  _ ->
	    Set
	end;
      #enter{} ->
	case hipe_icode:enter_type(I) of
	  remote ->
	    {M, _F, _A} = hipe_icode:enter_fun(I),
	    ordsets:add_element(M, Set);
	  _ ->
	    Set
	end;
      _ ->
	Set
    end,
  get_remote_calls_1(Left, NewSet);
get_remote_calls_1([], Set) ->
  Set.


%%---------------------------------------------------------------------
%% Find functions called (or entered) by each function.

get_local_calls(List) ->
  get_local_calls(List, []).

get_local_calls([{MFA = {M, _F, _A}, Icode}|Left], Acc) ->
  CallSet = get_local_calls_1(hipe_icode:icode_code(Icode)),
  %% Exclude calls to your own module_info and recursive calls.
  CallSet1 = ordsets:del_element(MFA, CallSet),
  CallSet2 = ordsets:del_element({M, module_info, 0}, CallSet1),
  CallSet3 = ordsets:del_element({M, module_info, 1}, CallSet2),
  get_local_calls(Left, [{MFA, CallSet3}|Acc]);
get_local_calls([], Acc) ->
  Acc.

get_local_calls_1(Icode) ->
  get_local_calls_1(Icode, []).

get_local_calls_1([I|Left], Set) ->
  NewSet =
    case I of
      #call{} ->
	case hipe_icode:call_type(I) of
	  local ->
	    Fun = hipe_icode:call_fun(I),
	    ordsets:add_element(Fun, Set);
	  primop ->
	    case hipe_icode:call_fun(I) of
	      #mkfun{mfa=Fun} ->
		ordsets:add_element(Fun, Set);
	      _ ->
		Set
	    end;
	  _ ->
	    Set
	end;
      #enter{} ->
	case hipe_icode:enter_type(I) of
	  local ->
	    Fun = hipe_icode:enter_fun(I),
	    ordsets:add_element(Fun, Set);
	  primop ->
	    case hipe_icode:enter_fun(I) of
	      #mkfun{mfa=Fun} ->
		ordsets:add_element(Fun, Set);
	      _ ->
		Set
	    end;
	  _ ->
	    Set
	end;
      _ ->
	Set
    end,
  get_local_calls_1(Left, NewSet);
get_local_calls_1([], Set) ->
  Set.


%%---------------------------------------------------------------------
%% Find the edges in the callgraph.

get_edges(Calls) ->
  get_edges(Calls, []).

get_edges([{MFA, []}|Left], Edges) ->  
  %% Add en self-edge to ensure that the function is not lost
  get_edges(Left, ordsets:add_element({MFA, MFA}, Edges));
get_edges([{MFA, Set}|Left], Edges) ->  
  EdgeList = [{MFA, X} || X <- Set],
  EdgeSet = ordsets:from_list(EdgeList),
  get_edges(Left, ordsets:union(EdgeSet, Edges));
get_edges([], Edges) ->
  Edges.
