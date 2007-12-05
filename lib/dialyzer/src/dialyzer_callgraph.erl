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

-export([all_nodes/1,
	 delete/1,
	 finalize/1,
	 is_escaping/2,
	 is_self_rec/2,	 
	 non_local_calls/1,
	 lookup_rec_var/2,	 
	 lookup_call_site/2,
	 lookup_label/2,
	 lookup_name/2,
	 modules/1,
	 module_deps/1,
	 module_postorder/1,
	 module_postorder_from_funs/2,
	 in_neighbours/2,
	 reset_from_funs/2,
	 scan_core_tree/2,
	 scan_icode/2,
	 split_into_components/1,
	 new/0, 
	 take_scc/1, 
	 remove_external/1]).

%-define(NO_UNUSED, true).
-ifndef(NO_UNUSED).
-export([to_dot/1]).
-endif.

%%----------------------------------------------------------------------

-include("dialyzer_callgraph.hrl").

%%----------------------------------------------------------------------

new() ->
  #dialyzer_callgraph{calls=dict:new(),
		      digraph=digraph_new(),
		      esc=sets:new(),
		      name_map=dict:new(),
		      postorder=[],
		      rec_var_map=dict:new(),
		      rev_name_map=dict:new(),
		      self_rec=sets:new()}.

delete(#dialyzer_callgraph{digraph=Digraph}) ->
  digraph_delete(Digraph).

split_into_components(CG = #dialyzer_callgraph{digraph=Digraph}) ->
  %% Splits the callgraph into connected components that can be
  %% analyzed in parallell. Note that we keep all other information in
  %% the callgraph the same in all parts even if it means a lot of
  %% redundant information.
  DigraphList = digraph_components(Digraph),
  [CG#dialyzer_callgraph{digraph=DG} || DG <- DigraphList].

all_nodes(#dialyzer_callgraph{digraph=DG}) ->
  digraph_vertices(DG).

lookup_rec_var(Label, #dialyzer_callgraph{rec_var_map=RecVarMap}) 
  when is_integer(Label) ->
  dict:find(Label, RecVarMap).

lookup_call_site(Label,#dialyzer_callgraph{calls=Calls})when is_integer(Label)->
  dict:find(Label, Calls).

-spec(lookup_name/2 ::
      (integer(), #dialyzer_callgraph{}) -> 'error' | {'ok',mfa()}).

lookup_name(Label,#dialyzer_callgraph{name_map=NameMap})when is_integer(Label)->
  dict:find(Label, NameMap).

-spec(lookup_label/2 ::
      (mfa(), #dialyzer_callgraph{}) -> 'error' | {'ok',mfa()}
     ;(integer(), #dialyzer_callgraph{}) -> {'ok',integer()}).

lookup_label(MFA = {_,_,_}, #dialyzer_callgraph{rev_name_map=RevNameMap}) ->
  dict:find(MFA, RevNameMap);
lookup_label(Label, #dialyzer_callgraph{}) when is_integer(Label) ->
  {ok, Label}.

-spec(in_neighbours/2 ::
      (integer() | mfa(), #dialyzer_callgraph{}) -> 'none' | [any(),...]).

in_neighbours(Label, CG=#dialyzer_callgraph{}) when is_integer(Label) ->
  Name = case dict:find(Label, CG#dialyzer_callgraph.name_map) of
	   {ok, Val} -> Val;
	   error -> Label
	 end,
  digraph_in_neighbours(Name, CG#dialyzer_callgraph.digraph);
in_neighbours(MFA={_,_,_}, CG=#dialyzer_callgraph{}) ->
  digraph_in_neighbours(MFA, CG#dialyzer_callgraph.digraph).

-spec(is_self_rec/2 :: (integer() | mfa(), #dialyzer_callgraph{}) -> bool()).

is_self_rec(MfaOrLabel, #dialyzer_callgraph{self_rec=SelfRecs}) ->
  sets:is_element(MfaOrLabel, SelfRecs).

-spec(is_escaping/2 :: (integer(), #dialyzer_callgraph{}) -> bool()).

is_escaping(Label, #dialyzer_callgraph{esc=Esc}) when is_integer(Label) ->
  sets:is_element(Label, Esc).  

add_edges([], CG) ->
  CG;
add_edges(Edges, CG = #dialyzer_callgraph{digraph=Callgraph}) ->
  CG#dialyzer_callgraph{digraph=digraph_add_edges(Edges, Callgraph)}.

add_edges(Edges, MFAs, CG = #dialyzer_callgraph{digraph=DG}) ->
  DG1 = digraph_confirm_vertices(MFAs, DG),
  add_edges(Edges, CG#dialyzer_callgraph{digraph=DG1}).

take_scc(CG = #dialyzer_callgraph{postorder=[SCC|Left]}) ->
  {ok, SCC, CG#dialyzer_callgraph{postorder=Left}};
take_scc(#dialyzer_callgraph{postorder=[]}) ->
  none.

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
find_non_local_calls([{{_,_,_}, Label}|Left], Set) when is_integer(Label) ->
  find_non_local_calls(Left, Set);  
find_non_local_calls([{Label, {_,_,_}}|Left], Set) when is_integer(Label) ->
  find_non_local_calls(Left, Set);
find_non_local_calls([{Label1, Label2}|Left], Set) when is_integer(Label1),
							is_integer(Label2) ->
  find_non_local_calls(Left, Set);
find_non_local_calls([], Set) ->
  sets:to_list(Set).

modules(#dialyzer_callgraph{digraph=DG}) ->
  ordsets:from_list([M || {M,_F,_A} <- digraph_vertices(DG)]).

module_postorder(#dialyzer_callgraph{digraph=DG}) ->
  Edges = digraph_edges(DG),
  Nodes = ordsets:from_list([M || {M,_F,_A} <- digraph_vertices(DG)]),
  MDG = digraph_new(),
  MDG1 = digraph_confirm_vertices(Nodes, MDG),
  MDG2 = create_module_digraph(Edges, MDG1),
  MDG3 = digraph_utils:condensation(MDG2),
  PostOrder = digraph_utils:postorder(MDG3),
  PostOrder1 = sort_sccs_internally(PostOrder, MDG2),
  digraph:delete(MDG2),
  digraph_delete(MDG3),
  PostOrder1.

%% The module deps of a module is who is depending on the module.
module_deps(#dialyzer_callgraph{digraph=DG}) ->
  Edges = digraph_edges(DG),
  Nodes = ordsets:from_list([M || {M,_F,_A} <- digraph_vertices(DG)]),
  MDG = digraph_new(),
  MDG1 = digraph_confirm_vertices(Nodes, MDG),
  MDG2 = create_module_digraph(Edges, MDG1),
  Deps = [{N, ordsets:from_list(digraph:in_neighbours(MDG2, N))}
	  || N <- Nodes],
  digraph_delete(MDG2),
  dict:from_list(Deps).

sort_sccs_internally(PO, MDG) ->
  sort_sccs_internally(PO, MDG, []).

sort_sccs_internally([SCC|Left], MDG, Acc) ->
  case length(SCC) >= 3 of
    false -> sort_sccs_internally(Left, MDG, [SCC|Acc]);
    true ->
      TmpDG = digraph_utils:subgraph(MDG, SCC),
      NewSCC = digraph_utils:postorder(TmpDG),
      digraph_delete(TmpDG),
      sort_sccs_internally(Left, MDG, [NewSCC|Acc])
  end;
sort_sccs_internally([], _MDG, Acc) ->
  lists:reverse(Acc).

create_module_digraph([{{M,_,_}, {M,_,_}}|Left], MDG) ->
  create_module_digraph(Left, MDG);
create_module_digraph([{{M1,_,_},{M2,_,_}}|Left], MDG) ->
  create_module_digraph(Left, digraph_add_edge(M1, M2, MDG));
create_module_digraph([{_, _}|Left], MDG) ->
  create_module_digraph(Left, MDG);
create_module_digraph([], MDG) ->
  MDG.

finalize(CG = #dialyzer_callgraph{digraph=DG}) ->
  CG#dialyzer_callgraph{postorder=digraph_finalize(DG)}.

reset_from_funs(Funs, CG = #dialyzer_callgraph{digraph=DG}) ->
  SubGraph = digraph_reaching_subgraph(Funs, DG),
  Postorder = digraph_finalize(SubGraph),
  digraph_delete(SubGraph),
  CG#dialyzer_callgraph{postorder=Postorder}.

module_postorder_from_funs(Funs, CG = #dialyzer_callgraph{digraph=DG}) ->
  SubGraph = digraph_reaching_subgraph(Funs, DG),
  PO = module_postorder(CG#dialyzer_callgraph{digraph=SubGraph}),
  digraph_delete(SubGraph),
  PO.
  
%%____________________________________________________________
%%
%% Core code
%%

%% The core tree must be labeled as by cerl_trees:label/1 (or /2).
%% The set of labels in the tree must be disjoint from the set of
%% labels already occuring in the callgraph.

scan_core_tree(Tree, CG=#dialyzer_callgraph{calls=OldCalls,
					    esc=OldEsc,
					    name_map=OldNameMap,
					    rec_var_map=OldRecVarMap, 
					    rev_name_map=OldRevNameMap,
					    self_rec=OldSelfRec}) ->
  %% Build name map and recursion variable maps.
  {NewNameMap, NewRevNameMap, NewRecVarMap} = 
    build_maps(Tree, OldRecVarMap, OldNameMap, OldRevNameMap),
  
  %% First find the module-local dependencies.
  {Deps0, EscapingFuns, Calls} = dialyzer_dep:analyze(Tree),
  NewCalls = dict:merge(fun(_Key, Val, Val) -> Val end, OldCalls, Calls),
  NewEsc = sets:union(sets:from_list(EscapingFuns), OldEsc),
  LabelEdges = get_edges_from_deps(Deps0),
  
  %% Find the self recursive functions. Named functions gets both the
  %% key and its name for convenience.
  SelfRecs0 = lists:foldl(fun({Key, Key}, Acc) -> 
			      case dict:find(Key, NewNameMap) of
				error      -> [Key|Acc];
				{ok, Name} -> [Key, Name|Acc]
			      end;
			     (_, Acc) -> Acc
			  end, [], LabelEdges),
  SelfRecs = sets:union(sets:from_list(SelfRecs0), OldSelfRec),
  
  NamedEdges1 = name_edges(LabelEdges, NewNameMap),
  
  %% We need to scan for intermodular calls since this is not tracked
  %% by dialyzer_dep. Note that the caller is always recorded as the
  %% top level function. This is ok since the included functions are
  %% stored as scc with the parent.
  NamedEdges2 = scan_core_funs(Tree),

  %% Confirm all nodes in the tree.
  Names1 = lists:flatten([[X, Y] || {X, Y} <- NamedEdges1]),
  Names2 = ordsets:from_list(Names1),
  Names3 = ordsets:del_element(top, Names2),
  CG1 = add_edges(NamedEdges2++NamedEdges1, Names3, CG),
  
  CG1#dialyzer_callgraph{calls=NewCalls,
			 esc=NewEsc,
			 name_map=NewNameMap,
			 rec_var_map=NewRecVarMap, 
			 rev_name_map=NewRevNameMap,
			 self_rec=SelfRecs}.

build_maps(Tree, RecVarMap, NameMap, RevNameMap) ->
  %% We only care about the named (top level) functions. The anonymous
  %% functions will be analysed together with their parents. 
  Defs = cerl:module_defs(Tree),
  Mod = cerl:atom_val(cerl:module_name(Tree)),
  lists:foldl(fun({Var, Function}, {AccNameMap, AccRevNameMap, AccRecVarMap}) ->
		  FunName = cerl:fname_id(Var),
		  Arity = cerl:fname_arity(Var),
		  MFA = {Mod, FunName, Arity},
		  {dict:store(get_label(Function), MFA, AccNameMap),
		   dict:store(MFA, get_label(Function), AccRevNameMap),
		   dict:store(get_label(Var), MFA, AccRecVarMap)}
	      end, {NameMap, RevNameMap, RecVarMap}, Defs).


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
    [{label, L} | _] when is_integer(L) -> L;
    _ -> erlang:error({missing_label, T})
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

digraph_new() ->
  digraph:new().

digraph_add_edges([{From, To}|Left], DG) ->
  digraph_add_edges(Left, digraph_add_edge(From, To, DG));
digraph_add_edges([], DG) ->
  DG.

digraph_add_edge(From, To, DG) ->
  case digraph:vertex(DG, From) of
    false -> digraph:add_vertex(DG, From);
    {From, _} -> ok
  end,
  case digraph:vertex(DG, To) of
    false -> digraph:add_vertex(DG, To);
    {To, _} -> ok
  end,
  digraph:add_edge(DG, {From, To}, From, To, []),
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

digraph_delete(DG) ->
  digraph:delete(DG).

digraph_edges(DG) ->
  digraph:edges(DG).

digraph_vertices(DG) ->
  digraph:vertices(DG).

digraph_in_neighbours(V, DG) ->
  case digraph:in_neighbours(DG, V) of
    [] -> none;
    List -> List
  end.

digraph_components(Digraph) ->
  Res = [digraph_utils:subgraph(Digraph, Vertices) 
	 || Vertices <- digraph_utils:components(Digraph)],
  digraph_delete(Digraph),
  Res.

digraph_postorder(Digraph) ->
  %% Remove all self-edges for Sccs.
  Edges = [digraph:edge(Digraph, E) || E <- digraph:edges(Digraph)],
  SelfEdges = [E || {E, V, V, _} <- Edges],
  true = digraph:del_edges(Digraph, SelfEdges),
  digraph_postorder(Digraph, -1, []).


%%% Pick all the independent nodes (leaves) from one module. Then try
%%% to stay within the module until no more independent nodes can be
%%% chosen. Then pick a new module and so on.
%%%
%%% Note that a SCC that range over more than one module is considered
%%% to belong to all modules to make sure that we do not lose any
%%% nodes.
digraph_postorder(Digraph, LastModule, Acc) ->
  Leaves = [V || V <- digraph:vertices(Digraph),
		 digraph:out_degree(Digraph, V) =:= 0],
  case Leaves =:= [] of
    true -> lists:append(lists:reverse(Acc));
    false ->
      case [SCC || SCC <- Leaves, scc_belongs_to_module(SCC, LastModule)] of
	[] ->
	  %% Choose a new module.
	  NewModule = find_module(hd(Leaves)),
	  NewTaken = [SCC || SCC <- Leaves, 
			     scc_belongs_to_module(SCC, NewModule)],
	  true = digraph:del_vertices(Digraph, NewTaken),
	  digraph_postorder(Digraph, NewModule, [NewTaken|Acc]);
	NewTaken ->
	  true = digraph:del_vertices(Digraph, NewTaken),
	  digraph_postorder(Digraph, LastModule, [NewTaken|Acc])
      end
  end.

-spec(scc_belongs_to_module/2 :: ([integer() | mfa()], atom()) -> bool()).

scc_belongs_to_module([Label|Left], Module) when is_integer(Label) ->
  scc_belongs_to_module(Left, Module);
scc_belongs_to_module([{M, _, _}|Left], Module) ->
  if M =:= Module -> true;
     true -> scc_belongs_to_module(Left, Module)
  end;
scc_belongs_to_module([], _Module) ->
  false.
      
-spec(find_module/1 :: ([integer() | mfa(),...]) -> atom()).

find_module([{M, _, _}|_]) -> M;
find_module([Label|Left]) when is_integer(Label) -> find_module(Left).

digraph_finalize(DG) ->
  DG1 = digraph_utils:condensation(DG),
  Postorder = digraph_postorder(DG1),
  digraph:delete(DG1),
  Postorder.

digraph_reaching_subgraph(Funs, DG) ->  
  Vertices = digraph_utils:reaching(Funs, DG),
  digraph_utils:subgraph(DG, Vertices).

%%=============================================================================
%% Utilities for 'dot'
%%=============================================================================

-ifndef(NO_UNUSED).
to_dot(CG = #dialyzer_callgraph{digraph=DG, esc=Esc}) ->
  Fun = fun(L) ->
	    case lookup_name(L, CG) of
	      error -> L;
	      {ok, Name} -> Name
	    end
	end,
  Escaping = [{Fun(L), {color, red}} 
	      || L <- sets:to_list(Esc), L =/= external],
  Vertices = digraph_edges(DG),
  hipe_dot:translate_list(Vertices, "/tmp/cg.dot", "CG", Escaping),
  os:cmd("dot -T ps -o /tmp/cg.ps /tmp/cg.dot"),
  ok.
-endif.
