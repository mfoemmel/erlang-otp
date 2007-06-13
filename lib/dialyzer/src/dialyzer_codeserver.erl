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
%%% File    : dialyzer_codeserver.erl
%%% Author  : Tobias Lindahl <tobiasl@it.uu.se>
%%% Description : 
%%%
%%% Created :  4 Apr 2005 by Tobias Lindahl <tobiasl@it.uu.se>
%%%-------------------------------------------------------------------
-module(dialyzer_codeserver).

-export([all_exports/1, 
	 delete/1,
	 insert/3, 
	 insert_exports/2,	 
	 is_exported/2,
	 lookup/3, 
	 lookup_records/2,
	 lookup_contracts/2,
	 lookup_contract/2,
	 new/0,
	 next_core_label/1,
	 store_records/3,
	 store_contracts/3,
	 update_next_core_label/2]).


-record(dialyzer_codeserver, {table, exports, next_core_label, records, contracts}).

new() ->
  Table = table__new(),
  Exports = sets:new(),
  #dialyzer_codeserver{table=Table, exports=Exports, next_core_label=0,
		       records=dict:new(), contracts=dict:new()}.

delete(#dialyzer_codeserver{table=Table}) ->
  table__delete(Table).

insert(List, Tag, CS) ->
  %% Note that ID is a MFA in icode and a module in core.
  List1 = [{{ID, Tag}, Code}||{ID, Code} <- List],
  NewTable = table__insert(CS#dialyzer_codeserver.table, List1),
  CS#dialyzer_codeserver{table=NewTable}.

insert_exports(List, CS = #dialyzer_codeserver{exports=Exports}) ->
  Set = sets:from_list(List),
  NewExports = sets:union(Exports, Set),
  CS#dialyzer_codeserver{exports=NewExports}.

is_exported(MFA, #dialyzer_codeserver{exports=Exports}) ->
  sets:is_element(MFA, Exports).

all_exports(#dialyzer_codeserver{exports=Exports}) ->
  Exports.

lookup(Id, Tag, CS) ->
  table__lookup(CS#dialyzer_codeserver.table, {Id, Tag}).

next_core_label(#dialyzer_codeserver{next_core_label=NCL}) ->
  NCL.

update_next_core_label(NCL, CS = #dialyzer_codeserver{}) ->
  CS#dialyzer_codeserver{next_core_label=NCL}.

store_records(Module, Dict, 
	      CS=#dialyzer_codeserver{records=RecDict}) when is_atom(Module) ->
  case dict:size(Dict) =:= 0 of
    true -> CS;
    false ->
      CS#dialyzer_codeserver{records=dict:store(Module, Dict, RecDict)}
  end.

lookup_records(Module, 
	       #dialyzer_codeserver{records=RecDict}) when is_atom(Module) ->
  case dict:find(Module, RecDict) of
    error -> dict:new();
    {ok, Dict} -> Dict
  end.


store_contracts(Module, Dict, 
	      CS=#dialyzer_codeserver{contracts=ContDict}) when is_atom(Module) ->
  case dict:size(Dict) =:= 0 of
    true -> CS;
    false ->
      CS#dialyzer_codeserver{contracts=dict:store(Module, Dict, ContDict)}
  end.

lookup_contracts(Module, 
	       #dialyzer_codeserver{contracts=ContDict}) when is_atom(Module) ->
  case dict:find(Module, ContDict) of
    error -> dict:new();
    {ok, Dict} -> Dict
  end.

lookup_contract({M,F,A}, #dialyzer_codeserver{contracts=ContDict}) ->
  case dict:find(M, ContDict) of
    error -> error;
    {ok, Dict} -> 
	dict:find({F,A}, Dict)
  end.

table__new() ->
  spawn_link(fun() -> table__loop(none, dict:new())end).

table__delete(TablePid) ->
  TablePid ! stop.

table__lookup(TablePid, Key) ->
  TablePid ! {self(), lookup, Key},
  receive
    {TablePid, Key, Ans} -> Ans
  end.

table__insert(Table, List) ->
  List1 = [{Key, term_to_binary(Val, [compressed])} || {Key, Val} <- List],
  Table ! {insert, List1},
  Table.

table__loop(Cached, Map) ->
  receive
    stop -> ok;
    {Pid, lookup, Key = {{M, F, A}, core}} ->
      {NewCached, Ans} =
	case Cached of
	  {M, Tree} ->
	    [Val] = [{Var, Fun} || {Var, Fun} <- cerl:module_defs(Tree),
				   cerl:fname_id(Var) =:= F,
				   cerl:fname_arity(Var) =:= A],
	    {Cached, {ok, Val}};
	  _ ->
	    Bin = dict:fetch({M, core}, Map),
	    Tree = binary_to_term(Bin),
	    [Val] = [{Var, Fun} || {Var, Fun} <- cerl:module_defs(Tree),
				   cerl:fname_id(Var) =:= F,
				   cerl:fname_arity(Var) =:= A],
	    {{M, Tree}, {ok, Val}}
	end,
      Pid ! {self(), Key, Ans},
      table__loop(NewCached, Map);
    {Pid, lookup, Key} ->
      Bin = dict:fetch(Key, Map),
      Tree = binary_to_term(Bin),
      Pid ! {self(), Key, {ok, Tree}},
      table__loop(Cached, Map);
    {insert, List} ->
      NewMap = lists:foldl(fun({Key, Val}, AccMap) -> 
			       dict:store(Key, Val, AccMap)
			   end, Map, List),
      table__loop(Cached, NewMap)
  end.
