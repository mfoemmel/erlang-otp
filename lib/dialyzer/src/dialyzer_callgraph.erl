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
%% Copyright 2006, Tobias Lindahl and Kostis Sagonas
%% 
%%     $Id$
%%

%%%-------------------------------------------------------------------
%%% File    : dialyzer_callgraph.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description : 
%%%
%%% Created : 30 Mar 2005 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------
-module(dialyzer_callgraph).

-export([finalize/1,
	 is_self_rec/2,
	 non_local_calls/1,
	 lookup_rec_var/2,	 
	 scan_core_tree/2,
	 scan_icode/2,
	 new/0, 
	 take_scc/1, 
	 remove_external/1]).

-export([all_nodes/1]).

%%----------------------------------------------------------------------
%%
%% A callgraph is a directed graph where the nodes are functions and a
%% call between two functions is an edge from the caller to the
%% callee. 
%%
%% digraph     -  A hipe_digraph representing the callgraph. 
%%                Nodes are represented as MFAs.
%% rec_var_map -  a dict mapping from letrec bound labels to function names. 
%%                Only for top level functions.
%% self_rec    -  a set containing all self recursive functions.
%%                Note that this contains MFAs for named functions and labels
%%                whenever applicable.
%%

-record(dialyzer_callgraph, {digraph, rec_var_map, self_rec}).

new() ->
  Digraph = digraph_new(),
  RecVarMap = dict:new(),
  SelfRecs = sets:new(),
  #dialyzer_callgraph{digraph=Digraph, 
		      rec_var_map=RecVarMap, 
		      self_rec=SelfRecs}.

all_nodes(#dialyzer_callgraph{digraph=DG}) ->
  lists:flatten(DG).

lookup_rec_var(Label, #dialyzer_callgraph{rec_var_map=RecVarMap}) ->
  dict:find(Label, RecVarMap).

is_self_rec(MFA, #dialyzer_callgraph{self_rec=SelfRecs}) ->
  sets:is_element(MFA, SelfRecs).

add_edges([], CG) ->
  CG;
add_edges(Edges, CG =#dialyzer_callgraph{digraph=Callgraph}) ->
  CG#dialyzer_callgraph{digraph=digraph_add_edges(Edges, Callgraph)}.

add_edges(Edges, MFAs, CG =#dialyzer_callgraph{digraph=DG}) ->
  DG1 = digraph_confirm_vertices(MFAs, DG),
  add_edges(Edges, CG#dialyzer_callgraph{digraph=DG1}).
  

take_scc(CG = #dialyzer_callgraph{digraph=Callgraph}) ->
  case digraph_take_indep_scc(Callgraph) of
    {ok, SCC, NewCallgraph} ->
      {ok, SCC, CG#dialyzer_callgraph{digraph=NewCallgraph}};
    none -> none
  end.

remove_external(CG = #dialyzer_callgraph{digraph=DG}) ->
  {NewDG, External} = digraph_remove_external(DG),
  {CG#dialyzer_callgraph{digraph=NewDG}, External}.

non_local_calls(#dialyzer_callgraph{digraph=DG}) ->
  Edges = digraph_edges(DG),		
  find_non_local_calls(Edges, sets:new()).

find_non_local_calls([{{M, _, _}, {M, _, _}}|Left], Set) ->
  find_non_local_calls(Left, Set);
find_non_local_calls([Edge={{M1, _, _},{M2, _, _}}|Left], Set) when M1 =/= M2 ->
  find_non_local_calls(Left, sets:add_element(Edge, Set));
find_non_local_calls([], Set) ->
  sets:to_list(Set).

finalize(CG = #dialyzer_callgraph{digraph=DG}) ->
  CG#dialyzer_callgraph{digraph=digraph_finalize(DG)}.

%%____________________________________________________________
%%
%% Core code
%%

%% The core tree must be labeled as by cerl_trees:label/1 (or /2).
%% The set of labels in the tree must be disjoint from the set of
%% labels already occuring in the callgraph.

scan_core_tree(Tree, CG=#dialyzer_callgraph{rec_var_map=RecVarMap, 
					    self_rec=OldSelfRec}) ->
  %% Build name map and recursion variable maps.
  {NameMap, RecVarMap1} = build_maps(Tree, RecVarMap),
  
  %% First find the module-local dependencies.
  {Deps0, _EscapingFuns, _Calls} = dialyzer_dep:analyze(Tree),
  LabelEdges = get_edges_from_deps(Deps0),
  
  %% Find the self recursive functions. Named functions gets both the
  %% key and its name for convenience.
  SelfRecs0 = lists:foldl(fun({Key, Key}, Acc) -> 
			      case dict:find(Key, NameMap) of
				error      -> [Key|Acc];
				{ok, Name} -> [Key, Name|Acc]
			      end;
			     (_, Acc) -> Acc
			  end, [], LabelEdges),
  SelfRecs = sets:union(sets:from_list(SelfRecs0), OldSelfRec),
  
  %% Remove the 'top' but add self edges to the top function to ensure
  %% that they are present in the callgraph. 
  LabelEdges1 = lists:foldl(fun({top, X}, Acc) -> [{X, X}|Acc];
			       (Edge, Acc)     -> [Edge|Acc]
			    end, [], LabelEdges),
  NamedEdges1 = name_edges(LabelEdges1, NameMap),
  
  %% We need to scan for intermodular calls since this is not tracked
  %% by dialyzer_dep. Note that the caller is always recorded as the
  %% top level function. This is ok since the included functions are
  %% stored as scc with the parent.
  NamedEdges2 = scan_core_funs(Tree),

  %% Confirm all nodes in the tree.
  Names = lists:flatten([[X, Y]||{X, Y} <- NamedEdges1]),
  CG1 = add_edges(NamedEdges2++NamedEdges1, Names, CG),
  
  CG1#dialyzer_callgraph{rec_var_map=RecVarMap1, self_rec=SelfRecs}.

build_maps(Tree, RecVarMap) ->
  %% We only care about the named (top level) functions. The anonymous
  %% functions will be analysed together with their parents. 
  Defs = cerl:module_defs(Tree),
  Mod = cerl:atom_val(cerl:module_name(Tree)),
  lists:foldl(fun({Var, Function}, {NameMap, AccRecVarMap}) ->
		  FunName = cerl:fname_id(Var),
		  Arity = cerl:fname_arity(Var),
		  MFA = {Mod, FunName, Arity},
		  {dict:store(get_label(Function), MFA, NameMap),
		   dict:store(get_label(Var), MFA, AccRecVarMap)}
	      end, {dict:new(), RecVarMap}, Defs).


get_edges_from_deps(Deps) ->
  %% Convert the dependencies as produced by dialyzer_dep to a list of
  %% edges. Also, remove 'external' since we are not interested
  %% in this information.
  Edges = dict:fold(fun(external, _Set, Acc) -> Acc;
		       (Caller, Set, Acc)    -> [[{Caller, Callee} 
						  || Callee <- Set, 
						     Callee =/= external]|Acc]
		    end, [], Deps),
  lists:flatten(Edges).

name_edges(Edges, NameMap) ->
  %% If a label is present in the name map it is renamed. Otherwise
  %% keep the label as the identity.
  MapFun = fun(X) -> 
	       case dict:find(X, NameMap) of
		 error -> X;
		 {ok, MFA} -> MFA
	       end
	   end,
  name_edges(Edges, MapFun, NameMap, []).

name_edges([{From, To}|Left], MapFun, NameMap, Acc) ->
  NewFrom = MapFun(From),
  NewTo = MapFun(To),
  name_edges(Left, MapFun, NameMap, [{NewFrom, NewTo}|Acc]);
name_edges([], _MapFun, _NameMap, Acc) ->
  Acc.

scan_core_funs(Tree) ->
  Defs = cerl:module_defs(Tree),
  Mod = cerl:atom_val(cerl:module_name(Tree)),
  DeepEdges = lists:foldl(fun({Var, Function}, Edges) ->
			      FunName = cerl:fname_id(Var),
			      Arity = cerl:fname_arity(Var),
			      MFA = {Mod, FunName, Arity},
			      [scan_one_core_fun(Function, MFA)|Edges]
			  end, [], Defs),
  lists:flatten(DeepEdges).

scan_one_core_fun(TopTree, FunName) ->
  FoldFun = fun(Tree, Acc) ->
		case cerl:type(Tree) of
		  call ->
		    CalleeM = cerl:call_module(Tree),
		    CalleeF = cerl:call_name(Tree),
		    A = length(cerl:call_args(Tree)),
		    case (cerl:is_c_atom(CalleeM) andalso 
			  cerl:is_c_atom(CalleeF)) of
		      true -> 
			M = cerl:atom_val(CalleeM),
			F = cerl:atom_val(CalleeF),
			case erl_bif_types:is_known(M, F, A) of
			  true -> Acc;
			  false -> [{FunName, {M, F, A}}|Acc]
			end;
		      false -> 
			%% We cannot handle run-time bindings
			Acc
		    end;
		  _ ->
		    %% Nothing that can introduce new edges in the callgraph.
		    Acc
		end
	    end,
  cerl_trees:fold(FoldFun, [], TopTree).
				      
get_label(T) ->
  case cerl:get_ann(T) of
    [{label, L} | _] -> L;
    _ -> erlang:fault({missing_label, T})
  end.


%%____________________________________________________________
%%
%% Icode
%%

scan_icode(List, Callgraph = #dialyzer_callgraph{self_rec=SelfRec}) ->
  {NewSelfRec, Edges} = scan_icode_funs(List, SelfRec, []),
  MFAs = [MFA || {MFA, _} <- List],
  add_edges(Edges, MFAs, Callgraph#dialyzer_callgraph{self_rec=NewSelfRec}).
  %add_edges(Edges, Callgraph#dialyzer_callgraph{self_rec=NewSelfRec}).

scan_icode_funs([{MFA, Cfg}|Left], SelfRec, Edges) ->
  Icode = hipe_icode_cfg:cfg_to_linear(Cfg),
  Code = hipe_icode:icode_code(Icode),
  {NewSelfRec, NewEdges} = scan_icode_code(Code, MFA, SelfRec, 
					   [{MFA, MFA}|Edges]),
  scan_icode_funs(Left, NewSelfRec, NewEdges);
scan_icode_funs([], SelfRec, Edges) ->
  {SelfRec, Edges}.

scan_icode_code([Ins|Left], MFA, SelfRecs, Edges) ->
  {NewSelfRecs, NewEdges} =
    case hipe_icode:is_call(Ins) orelse hipe_icode:is_enter(Ins) of
      true ->
	case call_or_enter_fun(Ins) of
	  {mkfun, Closure,_,_} -> {SelfRecs, [{MFA, Closure}|Edges]};
	  MFA ->
	    %% A self recursive call.
	    {sets:add_element(MFA, SelfRecs), Edges};
	  {M, F, A} -> 
	    case erl_bif_types:is_known(M, F, A) of
	      true -> {SelfRecs, Edges};
	      false -> {SelfRecs, [{MFA, {M, F, A}}|Edges]}
	    end;
	  _ -> {SelfRecs, Edges}
	end;
      false ->
	{SelfRecs, Edges}
    end,
  scan_icode_code(Left, MFA, NewSelfRecs, NewEdges);
scan_icode_code([], _MFA, SelfRecs, Edges) ->
  {SelfRecs, Edges}.	  

call_or_enter_fun(Ins) ->      
  case hipe_icode:is_call(Ins) of
    true -> hipe_icode:call_fun(Ins);
    false -> hipe_icode:enter_fun(Ins)
  end.


%%____________________________________________________________
%%
%% Digraph
%%

%digraph_new() ->
%  hipe_digraph:new().

%digraph_add_edges([{From, To}|Left], DG) ->
%  digraph_add_edges(Left, hipe_digraph:add_edge(From, To, DG));
%digraph_add_edges([], DG) ->
%  DG.

%digraph_remove_external(DG) ->
%  List1 = hipe_digraph:to_list(DG),
%  RealNodes = sets:from_list([X || {X, _} <- List1]),
%  ExtCallees = sets:from_list([Y || {X, Y} <- List1, 
%				    X =/= Y, 
%				    sets:is_element(Y, RealNodes) =:= false]),
%  List2 = lists:filter(fun({_, X})->sets:is_element(X, RealNodes)end, List1),
%  {hipe_digraph:from_list(List2), sets:to_list(ExtCallees)}.

%digraph_take_indep_scc(Callgraph) ->
%  hipe_digraph:take_indep_scc(Callgraph).

digraph_new() ->
  digraph:new().

digraph_add_edges([{From, To}|Left], DG) ->
  case digraph:vertex(DG, From) of
    false -> digraph:add_vertex(DG, From);
    {From, _} -> ok
  end,
  case digraph:vertex(DG, To) of
    false -> digraph:add_vertex(DG, To);
    {To, _} -> ok
  end,
  digraph:add_edge(DG, {From, To}, From, To, []),
  digraph_add_edges(Left, DG);
digraph_add_edges([], DG) ->
  DG.

digraph_confirm_vertices([MFA|Left], DG) ->
  digraph:add_vertex(DG, MFA, confirmed),
  digraph_confirm_vertices(Left, DG);
digraph_confirm_vertices([], DG) ->
  DG.
  
digraph_remove_external(DG) ->
  Vertices = digraph:vertices(DG),
  Unconfirmed = remove_unconfirmed(Vertices, DG),
  {DG, Unconfirmed}.

digraph_edges(DG) ->
  digraph:edges(DG).

digraph_finalize(DG) ->
  DG1 = digraph_utils:condensation(DG),
  digraph:delete(DG),
  Postorder = digraph_utils:postorder(DG1),
  digraph:delete(DG1),
  Postorder.

remove_unconfirmed(Vertexes, DG) ->
  remove_unconfirmed(Vertexes, DG, []).

remove_unconfirmed([V|Left], DG, Unconfirmed) ->
  case digraph:vertex(DG, V) of
    {V, confirmed} -> remove_unconfirmed(Left, DG, Unconfirmed);
    {V, []} -> remove_unconfirmed(Left, DG, [V|Unconfirmed])
  end;
remove_unconfirmed([], DG, Unconfirmed) ->
  BadCalls = lists:append([digraph:in_edges(DG, V) || V <- Unconfirmed]),
  BadCallsSorted = lists:keysort(1, BadCalls),
  digraph:del_vertices(DG, Unconfirmed),
  BadCallsSorted.

digraph_take_indep_scc([SCC|Left]) ->
  {ok, SCC, Left};
digraph_take_indep_scc([]) ->
  none.
  
