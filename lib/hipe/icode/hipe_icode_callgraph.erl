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


-record(callgraph, {codedict, scc_order}).

is_empty(#callgraph{scc_order=SCCs}) ->
  length(SCCs) == 0.

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
  Nodes = ordsets:from_list([X || {X, _} <- Calls]),
  {SCCs, SccMap} = get_sccs(Nodes, Edges),
  SCCEdges = get_scc_edges(Nodes, SccMap, Calls),
  SCCOrder = order_sccs(SCCs, SCCEdges),
  #callgraph{scc_order=finalize_order(SCCOrder, SCCs),
	     codedict=dict:from_list(List)}.


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
    case hipe_icode:type(I) of
      call ->
	case hipe_icode:call_type(I) of
	  remote ->
	    {M, _F, _A} = hipe_icode:call_fun(I),
	    ordsets:add_element(M, Set);
	  _ ->
	    Set
	end;
      enter ->
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
    case hipe_icode:type(I) of
      call ->
	case hipe_icode:call_type(I) of
	  local ->
	    Fun = hipe_icode:call_fun(I),
	    ordsets:add_element(Fun, Set);
	  primop ->
	    case hipe_icode:call_fun(I) of
	      {mkfun, Fun, _, _} ->
		ordsets:add_element(Fun, Set);
	      _ ->
		Set
	    end;
	  _ ->
	    Set
	end;
      enter ->
	case hipe_icode:enter_type(I) of
	  local ->
	    Fun = hipe_icode:enter_fun(I),
	    ordsets:add_element(Fun, Set);
	  primop ->
	    case hipe_icode:enter_fun(I) of
	      {mkfun, Fun, _, _} ->
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

get_edges([{MFA, Set}|Left], Edges) ->  
  EdgeList = [{MFA, X} || X <- Set],
  EdgeSet = ordsets:from_list(EdgeList),
  get_edges(Left, ordsets:union(EdgeSet, Edges));
get_edges([], Edges) ->
  Edges.

%%---------------------------------------------------------------------
%% Find the strongly connected components in the callgraph.

get_sccs(Nodes, Edges) ->
  get_sccs(Nodes, Edges, 0, gb_trees:empty(), gb_trees:empty()).

get_sccs([Node|Left], Edges, Id, SccMap, Acc) ->  
  {PreOrder, VisitedDFS} = dfs(Node, Edges),
  {_RDFS, VisitedRDFS} = rdfs(Node, Edges),
  SCC = ordsets:intersection(VisitedDFS, VisitedRDFS),
  OrderedSCC = lists:filter(fun(X) -> ordsets:is_element(X, SCC)end, 
			    lists:reverse(PreOrder)),
  %%io:format("DFS: ~p\nRDFS: ~p\nSCC: ~p\n", [DFS, RDFS, SCC]),
  NewSccMap = lists:foldl(fun(X, Map)->gb_trees:insert(X, Id, Map)end,
			  SccMap, SCC),
  get_sccs(ordsets:subtract(Left, SCC), Edges, 
	   Id + 1, NewSccMap, gb_trees:insert(Id, OrderedSCC, Acc));
get_sccs([], _Edges, _Id, SccMap, Acc) ->
  {Acc, SccMap}.

  
%%---------------------------------------------------------------------
%% Find the edges between the strongly connected components.
  
get_scc_edges(Nodes, SccMap, Calls) ->
  get_scc_edges(Nodes, SccMap, 
		gb_trees:from_orddict(orddict:from_list(Calls)), []).

get_scc_edges([Node|Left], SccMap, Calls, Acc) ->
  SccId = gb_trees:get(Node, SccMap),
  NodeCalls = gb_trees:get(Node, Calls),
  NewEdges = [{SccId, gb_trees:get(X, SccMap)} || X <- NodeCalls],
  NewEdgeSet = ordsets:from_list(NewEdges),
  NewEdgeSet1 = ordsets:del_element({SccId, SccId}, NewEdgeSet),
  get_scc_edges(Left, SccMap, Calls, ordsets:union(Acc, NewEdgeSet1));
get_scc_edges([], _SccMap, _Calls, Acc) ->
  Acc.


%%---------------------------------------------------------------------
%% Order the SCCs in a reverse preordering to ensure that all
%% successors to a node has been treated before the node is treated
%% itself.

order_sccs(SCCs, Edges) ->
  Nodes = gb_trees:keys(SCCs),
  %%io:format("SCCs: ~p\n", [gb_trees:to_list(SCCs)]),  
  %%io:format("Edges: ~p\n", [Edges]),  
  lists:flatten(order_sccs_1(Nodes, Edges, [])).

order_sccs_1([Node|Left], Edges, Visited) ->
  {PreOrder, DFSVisited} = dfs([Node], Edges, Visited, []),
  NewLeft = ordsets:subtract(Left, Visited),
  NewVisited = ordsets:union(DFSVisited, Visited),
  [lists:reverse(PreOrder) | order_sccs_1(NewLeft, Edges, NewVisited)];
order_sccs_1([], _Edges, _Visited) ->
  [].


%%---------------------------------------------------------------------
%% Put the actual function names in the order to process them.

finalize_order([Id|Left], SCCs)->
  [gb_trees:get(Id, SCCs) | finalize_order(Left, SCCs)];
finalize_order([], _SCCs) ->
  [].


%%---------------------------------------------------------------------
%% dfs/2 returns a preordered depth first search and the nodes visited.

dfs(Node, Edges) ->
  dfs([Node], Edges, [], []).

dfs([Node|Left], Edges, Visited, Order)->
  case ordsets:is_element(Node, Visited) of
    true ->
      dfs(Left, Edges, Visited, Order);
    false ->
      Filter = fun({F, _T}) -> Node =:= F end,
      Succ = [X || {_, X} <- ordsets:filter(Filter, Edges)],
      {NewOrder, NewVisited} = dfs(Succ, Edges, 
				   ordsets:add_element(Node, Visited), Order),
      dfs(Left, Edges, NewVisited, [Node|NewOrder])
  end;
dfs([], _Edges, Visited, Order) ->
  {Order, Visited}.


%%---------------------------------------------------------------------
%% rdfs/2 returns a preordered depth first search of the inverted graph,
%% and the nodes visited.

rdfs(Node, Edges) ->
  rdfs([Node], Edges, [], []).

rdfs([Node|Left], Edges, Visited, Order)->
  case ordsets:is_element(Node, Visited) of
    true ->
      rdfs(Left, Edges, Visited, Order);
    false ->
      Filter = fun({_F, T}) -> Node =:= T end,
      Succ = [X || {X, _} <- ordsets:filter(Filter, Edges)],
      {NewOrder, NewVisited} = rdfs(Succ, Edges, 
				    ordsets:add_element(Node, Visited), Order),
      rdfs(Left, Edges, NewVisited, [Node|NewOrder])
  end;
rdfs([], _Edges, Visited, Order) ->
  {Order, Visited}.
