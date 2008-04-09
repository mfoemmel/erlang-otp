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
	 insert/2, 
	 insert_exports/2,	 
	 is_exported/2,
	 lookup/2, 
	 lookup_records/2,
	 lookup_contracts/2,
	 lookup_contract/2,
	 new/0,
	 next_core_label/1,
	 store_records/3,
	 store_contracts/3,
	 update_next_core_label/2]).

-include("dialyzer.hrl").

%%--------------------------------------------------------------------

-spec(new/0 :: () -> #dialyzer_codeserver{}).
new() ->
  Table = table__new(),
  Exports = sets:new(),
  #dialyzer_codeserver{table=Table, exports=Exports, next_core_label=0,
		       records=dict:new(), contracts=dict:new()}.

-spec(delete/1 :: (#dialyzer_codeserver{}) -> 'ok').
delete(#dialyzer_codeserver{table=Table}) ->
  table__delete(Table).

-spec(insert/2:: ([_], #dialyzer_codeserver{}) -> #dialyzer_codeserver{}).
insert(List, CS) ->
  NewTable = table__insert(CS#dialyzer_codeserver.table, List),
  CS#dialyzer_codeserver{table=NewTable}.

-spec(insert_exports/2 ::
      ([mfa()], #dialyzer_codeserver{}) -> #dialyzer_codeserver{}).
insert_exports(List, CS = #dialyzer_codeserver{exports=Exports}) ->
  Set = sets:from_list(List),
  NewExports = sets:union(Exports, Set),
  CS#dialyzer_codeserver{exports=NewExports}.

-spec(is_exported/2 :: (mfa(), #dialyzer_codeserver{}) -> bool()).
is_exported(MFA, #dialyzer_codeserver{exports=Exports}) ->
  sets:is_element(MFA, Exports).

-spec(all_exports/1 :: (#dialyzer_codeserver{}) -> set()).
all_exports(#dialyzer_codeserver{exports=Exports}) ->
  Exports.

-spec(lookup/2 :: (_, #dialyzer_codeserver{}) -> any()).
lookup(Id, CS) ->
  table__lookup(CS#dialyzer_codeserver.table, Id).

-spec(next_core_label/1 :: (#dialyzer_codeserver{}) -> non_neg_integer()).
next_core_label(#dialyzer_codeserver{next_core_label=NCL}) ->
  NCL.

-spec(update_next_core_label/2 ::
      (non_neg_integer(), #dialyzer_codeserver{}) -> #dialyzer_codeserver{}).
update_next_core_label(NCL, CS = #dialyzer_codeserver{}) ->
  CS#dialyzer_codeserver{next_core_label=NCL}.

-spec(store_records/3 ::
      (atom(), dict(), #dialyzer_codeserver{}) -> #dialyzer_codeserver{}).
store_records(Module, Dict, 
	      CS=#dialyzer_codeserver{records=RecDict}) when is_atom(Module) ->
  case dict:size(Dict) =:= 0 of
    true -> CS;
    false ->
      CS#dialyzer_codeserver{records=dict:store(Module, Dict, RecDict)}
  end.

-spec(lookup_records/2 :: (atom(), #dialyzer_codeserver{}) -> dict()). 
lookup_records(Module, 
	       #dialyzer_codeserver{records=RecDict}) when is_atom(Module) ->
  case dict:find(Module, RecDict) of
    error -> dict:new();
    {ok, Dict} -> Dict
  end.

-spec(store_contracts/3 ::
      (atom(), dict(), #dialyzer_codeserver{}) -> #dialyzer_codeserver{}). 
store_contracts(Module, Dict, 
		CS=#dialyzer_codeserver{contracts=C}) when is_atom(Module) ->
  case dict:size(Dict) =:= 0 of
    true -> CS;
    false -> CS#dialyzer_codeserver{contracts=dict:store(Module, Dict, C)}
  end.

-spec(lookup_contracts/2 :: (atom(), #dialyzer_codeserver{}) -> dict()). 
lookup_contracts(Mod, 
		 #dialyzer_codeserver{contracts=ContDict}) when is_atom(Mod) ->
  case dict:find(Mod, ContDict) of
    error -> dict:new();
    {ok, Dict} -> Dict
  end.

-spec(lookup_contract/2 ::
      (mfa(), #dialyzer_codeserver{}) -> 'error' | {'ok',_}).
lookup_contract(MFA={M,_F,_A}, #dialyzer_codeserver{contracts=ContDict}) ->
  case dict:find(M, ContDict) of
    error -> error;
    {ok, Dict} -> dict:find(MFA, Dict)
  end.

table__new() ->
  spawn_link(fun() -> table__loop(none, dict:new()) end).

table__delete(TablePid) ->
  TablePid ! stop,
  ok.

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
    {Pid, lookup, Key = {M, F, A}} ->
      {NewCached, Ans} =
	case Cached of
	  {M, Tree} ->
	    [Val] = [{Var, Fun} || {Var, Fun} <- cerl:module_defs(Tree),
				   cerl:fname_id(Var) =:= F,
				   cerl:fname_arity(Var) =:= A],
	    {Cached, Val};
	  _ ->
	    Tree = fetch_and_expand(M, Map),
	    [Val] = [{Var, Fun} || {Var, Fun} <- cerl:module_defs(Tree),
				   cerl:fname_id(Var) =:= F,
				   cerl:fname_arity(Var) =:= A],
	    {{M, Tree}, Val}
	end,
      Pid ! {self(), Key, {ok, Ans}},
      table__loop(NewCached, Map);
    {Pid, lookup, Key} ->
      Ans = case Cached of
	      {Key, Tree} -> Tree;
	      _ -> fetch_and_expand(Key, Map)
	    end,
      Pid ! {self(), Key, {ok, Ans}},
      table__loop({Key, Ans}, Map);
    {insert, List} ->
      NewMap = lists:foldl(fun({Key, Val}, AccMap) -> 
			       dict:store(Key, Val, AccMap)
			   end, Map, List),
      table__loop(Cached, NewMap)
  end.

fetch_and_expand(Key, Map) ->
  Bin = dict:fetch(Key, Map),
  binary_to_term(Bin).
