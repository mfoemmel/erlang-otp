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
-module(dbg_ets).

%% Wrapper module to allow ets tables to be accessed on any node.

-export([delete/1,
	 delete/2,
	 insert/2,
	 lookup/2,
	 lookup_element/3,
	 match/2,
	 match_delete/2,
	 match_object/2,
	 new/2]).


lookup(T,K) when atom(T) ->
    ets:lookup(T,K);
lookup(T,K) when integer(T) ->
    ets:lookup(T,K);
lookup({T,Pid},K) when node(Pid) == node() ->         
    ets:lookup(T,K);
lookup({T,Pid},K)  ->         
    chk(rpc:block_call(node(Pid), ets, lookup, [T, K])).

lookup_element(T,K,Pos) when atom(T) ->
    lookup_element(T,K,Pos);
lookup_element(T,K,Pos) when integer(T) ->
    lookup_element(T,K,Pos);
lookup_element({T,Pid},K,Pos) when node(Pid) == node() ->         
    lookup_element(T,K,Pos);
lookup_element({T,Pid},K,Pos)  ->         
    chk(rpc:block_call(node(Pid), ets, lookup_element, [T, K,Pos])).

insert(T,V) when atom(T) ->
    ets:insert(T,V);
insert(T,V) when integer(T) ->
    ets:insert(T,V);
insert({T, Pid},V) when node(Pid) == node() ->
    ets:insert(T,V);
insert({T, Pid},V)  ->
    chk(rpc:block_call(node(Pid), ets, insert, [T,V])).

delete(T,K) when atom(T) ->
    ets:delete(T,K);
delete(T,K) when integer(T) ->
    ets:delete(T,K);
delete({T, Pid},K) when node(Pid) == node() ->
    ets:delete(T,K);
delete({T, Pid},K)  ->
     chk(rpc:block_call(node(Pid), ets, delete, [T,K])).

delete(T) when atom(T) ->      
    ets:delete(T);
delete(T) when integer(T) ->      
    ets:delete(T);
delete({T, Pid}) when node(Pid) == node() ->      
    ets:delete(T);
delete({T, Pid})  ->      
    chk(rpc:block_call(node(Pid), ets, delete, [T])).

new(Name, Type) ->     {ets:new(Name, Type),self()}.

match(T,Pattern) when atom(T) ->
    ets:match(T,Pattern);
match(T,Pattern) when integer(T) ->
    ets:match(T,Pattern);
match({T, Pid},Pattern)   when node(Pid) == node() ->
    ets:match(T,Pattern);
match({T, Pid},Pattern)   ->
    chk(rpc:block_call(node(Pid), ets, match, [T,Pattern])).

match_object(T,Pattern) when atom(T) ->
    ets:match_object(T,Pattern);
match_object(T,Pattern) when integer(T) ->
    ets:match_object(T,Pattern);
match_object({T, Pid},Pattern)   when node(Pid) == node() ->
    ets:match_object(T,Pattern);
match_object({T, Pid},Pattern)   ->
    chk(rpc:block_call(node(Pid), ets, match_object, [T,Pattern])).

match_delete(Tn, Pattern) when atom(Tn) ->
    ets:match_delete(Tn,Pattern);
match_delete(Tn, Pattern) when integer(Tn) ->
    ets:match_delete(Tn,Pattern);
match_delete({Tn, Pid}, Pattern) when node(Pid) == node() ->
    ets:match_delete(Tn,Pattern);
match_delete({T, Pid}, Pattern) ->
    chk(rpc:call(node(Pid), ets , match_delete, [T, Pattern])).

chk({badrpc,R}) -> exit(R);
chk(X) -> X.
