%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2006-2009. All Rights Reserved.
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

-spec new() -> #dialyzer_codeserver{}.

new() ->
  #dialyzer_codeserver{table_pid = table__new()}.

-spec delete(#dialyzer_codeserver{}) -> 'ok'.

delete(#dialyzer_codeserver{table_pid = TablePid}) ->
  table__delete(TablePid).

-spec insert([_], #dialyzer_codeserver{}) -> #dialyzer_codeserver{}.

insert(List, CS) ->
  NewTablePid = table__insert(CS#dialyzer_codeserver.table_pid, List),
  CS#dialyzer_codeserver{table_pid = NewTablePid}.

-spec insert_exports([mfa()], #dialyzer_codeserver{}) -> #dialyzer_codeserver{}.

insert_exports(List, #dialyzer_codeserver{exports = Exports} = CS) ->
  Set = sets:from_list(List),
  NewExports = sets:union(Exports, Set),
  CS#dialyzer_codeserver{exports = NewExports}.

-spec is_exported(mfa(), #dialyzer_codeserver{}) -> bool().

is_exported(MFA, #dialyzer_codeserver{exports = Exports}) ->
  sets:is_element(MFA, Exports).

-spec all_exports(#dialyzer_codeserver{}) -> set().

all_exports(#dialyzer_codeserver{exports = Exports}) ->
  Exports.

-spec lookup(_, #dialyzer_codeserver{}) -> {'ok', any()}.

lookup(Id, CS) ->
  table__lookup(CS#dialyzer_codeserver.table_pid, Id).

-spec next_core_label(#dialyzer_codeserver{}) -> label().

next_core_label(#dialyzer_codeserver{next_core_label = NCL}) ->
  NCL.

-spec update_next_core_label(label(), #dialyzer_codeserver{}) -> #dialyzer_codeserver{}.

update_next_core_label(NCL, CS) ->
  CS#dialyzer_codeserver{next_core_label = NCL}.

-spec store_records(module(), dict(), #dialyzer_codeserver{}) -> #dialyzer_codeserver{}.

store_records(Mod, Dict, 
	      CS = #dialyzer_codeserver{records = RecDict}) when is_atom(Mod) ->
  case dict:size(Dict) =:= 0 of
    true -> CS;
    false -> CS#dialyzer_codeserver{records = dict:store(Mod, Dict, RecDict)}
  end.

-spec lookup_records(module(), #dialyzer_codeserver{}) -> dict(). 

lookup_records(Mod, 
	       #dialyzer_codeserver{records = RecDict}) when is_atom(Mod) ->
  case dict:find(Mod, RecDict) of
    error -> dict:new();
    {ok, Dict} -> Dict
  end.

-spec store_contracts(module(), dict(), #dialyzer_codeserver{}) -> #dialyzer_codeserver{}. 

store_contracts(Mod, Dict, 
		CS = #dialyzer_codeserver{contracts = C}) when is_atom(Mod) ->
  case dict:size(Dict) =:= 0 of
    true -> CS;
    false -> CS#dialyzer_codeserver{contracts = dict:store(Mod, Dict, C)}
  end.

-spec lookup_contracts(module(), #dialyzer_codeserver{}) -> dict(). 

lookup_contracts(Mod, 
		 #dialyzer_codeserver{contracts = ContDict}) when is_atom(Mod) ->
  case dict:find(Mod, ContDict) of
    error -> dict:new();
    {ok, Dict} -> Dict
  end.

-spec lookup_contract(mfa(), #dialyzer_codeserver{}) -> 'error' | {'ok', _}.

lookup_contract({M,_F,_A} = MFA, #dialyzer_codeserver{contracts = ContDict}) ->
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

table__insert(TablePid, List) ->
  List1 = [{Key, term_to_binary(Val, [compressed])} || {Key, Val} <- List],
  TablePid ! {insert, List1},
  TablePid.

table__loop(Cached, Map) ->
  receive
    stop -> ok;
    {Pid, lookup, {M, F, A} = MFA} ->
      {NewCached, Ans} =
	case Cached of
	  {M, Tree} ->
	    [Val] = [VarFun || {Var, _Fun} = VarFun <- cerl:module_defs(Tree),
			       cerl:fname_id(Var) =:= F,
			       cerl:fname_arity(Var) =:= A],
	    {Cached, Val};
	  _ ->
	    Tree = fetch_and_expand(M, Map),
	    [Val] = [VarFun || {Var, _Fun} = VarFun <- cerl:module_defs(Tree),
			       cerl:fname_id(Var) =:= F,
			       cerl:fname_arity(Var) =:= A],
	    {{M, Tree}, Val}
	end,
      Pid ! {self(), MFA, {ok, Ans}},
      table__loop(NewCached, Map);
    {Pid, lookup, Mod} when is_atom(Mod) ->
      Ans = case Cached of
	      {Mod, Tree} -> Tree;
	      _ -> fetch_and_expand(Mod, Map)
	    end,
      Pid ! {self(), Mod, {ok, Ans}},
      table__loop({Mod, Ans}, Map);
    {insert, List} ->
      NewMap = lists:foldl(fun({Key, Val}, AccMap) -> 
			       dict:store(Key, Val, AccMap)
			   end, Map, List),
      table__loop(Cached, NewMap)
  end.

fetch_and_expand(Mod, Map) ->
  try
    Bin = dict:fetch(Mod, Map),
    binary_to_term(Bin)
  catch
    _:_ ->
      S = atom_to_list(Mod),
      Msg = "found no module named '" ++ S ++ "' in the analyzed files",
      exit({error, Msg})
  end.
