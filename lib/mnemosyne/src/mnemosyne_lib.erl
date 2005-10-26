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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(mnemosyne_lib).
-export([
	 table_defined/1,
	 assert_table_defined/2,
	 elements/2,
	 db_data/2,
	 db_read_clauses/1,
	 list_pos/2,
	 unique_var/1,
	 unique_id/0,
	 format_error/1,
	 new/0, insertL/3, insert/3, lookup/2
]).

%%-define(debug,1).

-include("mnemosyne_debug.hrl").
-include("mnemosyne_internal_form.hrl").

%%%================================================================
%%% 		Exports

table_defined(Name) ->
    lists:member(Name, mnesia:system_info(tables)).

assert_table_defined(Name, Line) ->
    case table_defined(Name) of
	true ->
	    ok;
	false ->
	    throw({error, {Line,?MODULE,{undefined_table,Name}}})
    end.

	

elements(N, L) when list(L) -> lists:map(fun(H) -> element(N,H) end, L).


db_data(record_defs, Module) -> get_from_module(Module, {data,record_defs});

db_data(argtypes, Module) -> get_from_module(Module, {data,argtypes}).


db_read_clauses(P) when record(P,pred_sym), P#pred_sym.type==rule ->
    Module = P#pred_sym.module,
    Name = P#pred_sym.functor,
    mk_bodies(get_from_module(Module,Name), P).

list_pos(X, L) -> list_pos(L, X, 1).
    

unique_var(Name) -> {'#var', {Name,unique_id()}}.

unique_id() -> make_ref().

%%%----------------------------------------------------------------
%%% bin tree.

new() -> nil.

insertL([K|Ks], [V|Vs], T) -> insertL(Ks, Vs, insert(K,V,T));
insertL([], [], T) -> T.

insert(K,V,nil) -> {{K,V},nil,nil};
insert(K0,V0,{{K,V},L,R}) when K0 < K -> {{K,V},insert(K0,V0,L),R};
insert(K0,V0,{{K,V},L,R}) when K0 > K -> {{K,V},L,insert(K0,V0,R)};
insert(K0,V,{{K0,_},L,R}) -> {{K0,V},L,R}.   %% Key allready there

%% -> false | {true,Value}
lookup(E,nil) -> false;
lookup(K0,{{K0,V},_,_}) -> {true,V};
lookup(K0,{{K,_},L,_}) when K0 < K -> lookup(K0,L);
lookup(K0,{_,_,R}) -> lookup(K0,R).

%%%----------------------------------------------------------------

format_error(Msg) ->
    case Msg of
	{undefined_table, Name} ->
	    io_lib:format("The table ~w is not defined in the schema", [Name])
    end.

%%%================================================================
%%% 		Private

get_from_module(Module, Name) ->
    case Module:'MNEMOSYNE RULE'(Name) of
	{'EXIT',{undef,{Module,'MNEMOSYNE RULE',[Name]}}} ->
	    throw({'EXIT',{undef,{Module,Name,[]}}});
	{'EXIT',{undef,[{Module,'MNEMOSYNE RULE',[Name]}| _]}} ->
	    throw({'EXIT',{undef,{Module,Name,[]}}});
	
	{'EXIT',Cause} ->
	    throw({'EXIT',Cause});
	
	Data ->
	    Data
    end.

%%%----------------------------------------------------------------
mk_bodies(Clauses, Pred) ->
    mk_bodies(Clauses, Pred#pred_sym.args, []).

mk_bodies([R|Clauses], Args, Acc) ->
    {rule,Head,Body} = mnemosyne_unify:rename_variables(R),
    mk_bodies(Clauses, Args,
	      case mnemosyne_unify:unify(Head#pred_sym.args, Args) of
		  false -> Acc;
		  Bs -> [mnemosyne_unify:instantiate(Body,Bs) | Acc]
	      end);

mk_bodies([], _, Acc) ->
    Acc.


%%%----------------------------------------------------------------
list_pos([X|_], X, N) -> N;
list_pos([_|Xs], X,N) -> list_pos(Xs, X, N+1);
list_pos([], _, N) -> false.
