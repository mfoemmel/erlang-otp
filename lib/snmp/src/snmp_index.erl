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
-module(snmp_index).

-export([new/1 ,new/2, insert/3, delete/2, delete/1, get/2, get_next/2,
	 get_last/1, key_to_oid/2]).

%%%-----------------------------------------------------------------
%%% This module implements an SNMP index structure as an ADT.
%%% It is supposed to be used as a separate structure which implements
%%% the SNMP ordering of the keys in the SNMP table.  The advantage
%%% with this is that the get-next operation is automatically
%%% taken care of.
%%%-----------------------------------------------------------------

%%-----------------------------------------------------------------
%% Args: KeyTypes = key() | {key(), ...}
%%       key() = integer | string | fix_string
%% Returns: handle()
%%-----------------------------------------------------------------

new(KeyTypes) ->
    new(KeyTypes,[]).

new(KeyTypes, Name) ->
    case is_snmp_type(to_list(KeyTypes)) of
	true ->
	    {EtsName,EtsOpts} = case Name of
				    N when atom(N) ->
					{N,[public,
					    ordered_set,
					    named_table]};
				    [] ->
					{snmp_index, [public,
						     ordered_set]};
				    _ ->
					exit({badarg, 
					      {?MODULE, new, [KeyTypes,Name]}})
				end,
	    {ets:new(EtsName,EtsOpts), KeyTypes};
	false ->
	    exit({badarg, {?MODULE, new, [KeyTypes,Name]}})
    end.

get({OrdSet, _KeyTypes}, KeyOid) ->
    case ets:lookup(OrdSet, KeyOid) of
	[X] ->
	    {ok,X};
	_ ->
	    undefined
    end.

get_next({OrdSet, KeyTypes}, KeyOid) ->
    case ets:next(OrdSet, KeyOid) of
	'$end_of_table' ->
	    undefined;
	Key ->
	    get({OrdSet, KeyTypes}, Key)
    end.

get_last({OrdSet, KeyTypes}) ->
    case ets:last(OrdSet) of
	'$end_of_table' ->
	    undefined;
	Key ->
	    get({OrdSet, KeyTypes}, Key)
    end.

insert({OrdSet, KeyTypes}, Key, Val) ->
    ets:insert(OrdSet, {key_to_oid_i(Key, KeyTypes), Val}),
    {OrdSet, KeyTypes}.

delete({OrdSet, KeyTypes}, Key) ->
    ets:delete(OrdSet, key_to_oid_i(Key, KeyTypes)),
    {OrdSet, KeyTypes}.

delete({OrdSet, KeyTypes}) ->
    ets:delete(OrdSet).

key_to_oid({_OrdSet, KeyTypes}, Key) ->
    key_to_oid_i(Key, KeyTypes).

to_list(Tuple) when tuple(Tuple) -> tuple_to_list(Tuple);
to_list(X) -> [X].

is_snmp_type([integer | T]) -> is_snmp_type(T);
is_snmp_type([string | T]) -> is_snmp_type(T);
is_snmp_type([fix_string | T]) -> is_snmp_type(T);
is_snmp_type([]) -> true;
is_snmp_type(_) -> false.

%%-----------------------------------------------------------------
%% Args: Key = key()
%%       key() = int() | string() | {int() | string(), ...}
%%       Type = {fix_string | term()}
%% Make an OBJECT IDENTIFIER out of it.
%% Variable length objects are prepended by their length.
%% Ex. Key = {"pelle", 42} AND Type = {string, integer} =>
%%        OID [5, $p, $e, $l, $l, $e, 42]
%%     Key = {"pelle", 42} AND Type = {fix_string, integer} =>
%%        OID [$p, $e, $l, $l, $e, 42]
%%-----------------------------------------------------------------
key_to_oid_i(Key, _Type) when integer(Key) -> [Key];
key_to_oid_i(Key, fix_string) -> Key;
key_to_oid_i(Key, _Type) when list(Key) -> [length(Key) | Key];
key_to_oid_i(Key, Types) -> keys_to_oid(size(Key), Key, [], Types).

keys_to_oid(0, _Key, Oid, _Types) -> Oid;
keys_to_oid(N, Key, Oid, Types) ->
    Oid2 = lists:append(key_to_oid_i(element(N, Key), element(N, Types)), Oid),
    keys_to_oid(N-1, Key, Oid2, Types).
