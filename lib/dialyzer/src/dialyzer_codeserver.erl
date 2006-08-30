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
	 insert/3, 
	 insert_exports/2,	 
	 is_exported/2,
	 lookup/3, 
	 new/0,
	 next_core_label/1,
	 update_next_core_label/2]).


-record(dialyzer_codeserver, {table, exports, next_core_label}).

new() ->
  Table = table__new(),
  Exports = sets:new(),
  #dialyzer_codeserver{table=Table, exports=Exports, next_core_label=0}.

insert(List, Tag, CS) ->
  List1 = [{{MFA, Tag}, Code}||{MFA, Code} <- List],
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

lookup(MFA, Tag, CS) ->
  table__lookup(CS#dialyzer_codeserver.table, {MFA, Tag}).      

next_core_label(#dialyzer_codeserver{next_core_label=NCL}) ->
  NCL.

update_next_core_label(CS = #dialyzer_codeserver{}, NCL) ->
  CS#dialyzer_codeserver{next_core_label=NCL}.

table__new() ->
  ets:new(dialyzer_codeserver, []).

table__lookup(Table, Key) ->
  case ets:lookup(Table, Key) of
    [{Key, Val}] -> {ok, Val};
    _ -> error
  end.

table__insert(Table, List) ->
  true = ets:insert(Table, List),
  Table.
