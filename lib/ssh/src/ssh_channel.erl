%%<copyright>
%% <year>2008-2008</year>
%% <holder>Ericsson AB, All Rights Reserved</holder>
%%</copyright>
%%<legalnotice>
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

-module(ssh_channel).

-include("ssh_connect.hrl").

-export([cache_create/0, cache_lookup/2, cache_update/2, 
	 cache_delete/1, cache_delete/2,  cache_foldl/3,
	 cache_find/2, is_empty/1]).

%%====================================================================
%% Internal application API
%%====================================================================

cache_create() ->
    ets:new(cm_tab, [set,{keypos, #channel.local_id}]).

cache_lookup(Cache, Key) ->
    case ets:lookup(Cache, Key) of
	[Channel] ->
	    Channel;
	[] ->
	    undefined
    end.

cache_update(Cache, #channel{local_id = Id} = Entry) when Id =/= undefined ->
    ets:insert(Cache, Entry).

cache_delete(Cache, Key) ->
    ets:delete(Cache, Key).

cache_delete(Cache) ->
    ets:delete(Cache).

cache_foldl(Fun, Acc, Cache) ->
    ets:foldl(Fun, Acc, Cache).
    
cache_find(ChannelPid, Cache) ->
   case ets:match_object(Cache, #channel{user = ChannelPid}) of
       [] ->
	   undefined;
       [Channel] ->
	   Channel
   end.

is_empty(Cache) ->
    case ets:info(Cache, size) of
	0 ->
	    true;
	_ ->
	    false
    end.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
