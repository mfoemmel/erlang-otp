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
-module(snmpm_misc_sup).

-include("snmp_debug.hrl").

-behaviour(supervisor).

%% External exports
-export([
	 start_link/0, 
	 start_net_if/2, stop_net_if/0,
	 start_note_store/2, stop_note_store/0
	]).

%% Internal exports
-export([init/1]).

-define(SUP, ?MODULE).


%%%-----------------------------------------------------------------
%%% This is a supervisor for the mib and net_ifprocesses.
%%% Each agent has one mib process.
%%%-----------------------------------------------------------------

start_link() ->
    ?d("start_link -> entry", []),
    SupName = {local,?SUP},
    supervisor:start_link(SupName, ?MODULE, []).


start_net_if(Mod, NoteStore) ->
    ?d("start_net_if -> entry with"
       "~n   Mod:       ~p"
       "~n   NoteStore: ~p", [Mod, NoteStore]),
    SupName = ?SUP, 
    start_net_if(SupName, Mod, [self(), NoteStore]).

start_net_if(SupName, Mod, Args) ->
    %% make sure we start from scratch...
    Children = supervisor:which_children(SupName),
    case lists:keysearch(net_if, 1, Children) of
	{value, {_, _Pid, _, _}} ->
	    stop_net_if(SupName);
	_ ->
	    ok
    end,
    NetIf = {net_if, 
	     {Mod, start_link, Args},
	     permanent, 2000, worker, [Mod]},
    supervisor:start_child(SupName, NetIf).

stop_net_if() ->
    stop_net_if(?SUP).

stop_net_if(SupName) ->
    case whereis(SupName) of
	undefined ->
	    ok;
	_ ->
	    supervisor:terminate_child(SupName, net_if),
	    supervisor:delete_child(SupName, net_if)
    end.


%% The note store is a common code component, so we must
%% pass all the arguments in a way that works both for
%% the agent and the manager entities. I.e. as arguments
%% to the start_link function.
start_note_store(Prio, Opts) ->
    ?d("start_note_store -> entry with"
       "~n   Prio: ~p"
       "~n   Opts: ~p", [Prio, Opts]),
    SupName = ?MODULE, 
    start_note_store(SupName, Prio, Opts).

start_note_store(SupName, Prio, Opts) ->
    %% make sure we start from scratch...
    Children = supervisor:which_children(SupName),
    case lists:keysearch(note_cache, 1, Children) of
	{value, {_, _Pid, _, _}} ->
	    stop_note_store(SupName);
	_ ->
	    ok
    end,
    Mod = snmp_note_store,
    Note = {note_store, 
	    {Mod, start_link, [Prio, snmpm, Opts]},
	    permanent, 2000, worker, [Mod]},
    supervisor:start_child(SupName, Note).

stop_note_store() ->
    stop_note_store(?SUP).

stop_note_store(SupName) ->
    case whereis(SupName) of
	undefined ->
	    ok;
	_ ->
	    supervisor:terminate_child(SupName, note_store),
	    supervisor:delete_child(SupName, note_store)
    end.


init([]) ->
    SupFlags = {one_for_all, 0, 3600},
    {ok, {SupFlags, []}}.

