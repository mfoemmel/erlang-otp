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

%%%----------------------------------------------------------------------
%%% File    : old_file_server.erl
%%% Author  : Raimo Niskanen <raimo@erix.ericsson.se>
%%% Purpose : Proxy process for calls to the old file_server name
%%% Created : 31 Jan 2001 by Raimo Niskanen <raimo@erix.ericsson.se>
%%%----------------------------------------------------------------------

-module(old_file_server).

-export([start/0, start_link/0]).

-define(FILE_SERVER,     file_server_2). % Registered name
-define(OLD_FILE_SERVER, file_server).   % Registered name

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    do_start(spawn).

start_link() ->
    do_start(spawn_link).

%%%----------------------------------------------------------------------
%%% Local functions
%%%----------------------------------------------------------------------

do_start(Spawn) ->
    case init:get_argument(master) of
	error ->
	    do_start(Spawn, node());
	{ok, [[Node]]} ->
	    do_start(Spawn, list_to_atom(Node));
	X ->
	    {error, {get_argument, master, X}}
    end.


do_start(Spawn, Node) when Node == node() ->
    case whereis(?FILE_SERVER) of
	Filer when pid(Filer) ->
	    do_start_relay(Spawn, Filer, ?OLD_FILE_SERVER);
	undefined ->
	    {error, {whereis, ?FILE_SERVER}}
    end;
do_start(Spawn, Node) ->
    case rpc:call(Node, erlang, whereis, [?FILE_SERVER]) of
	Filer when pid(Filer) ->
	    do_start_relay(Spawn, Filer, ?OLD_FILE_SERVER);
	undefined ->
	    case rpc:call(Node, erlang, whereis, [?OLD_FILE_SERVER]) of
		Filer when pid(Filer) ->
		    do_start_relay(Spawn, Filer, ?OLD_FILE_SERVER);
		X ->
		    {error, {rpc, Node, whereis, ?OLD_FILE_SERVER, X}}
	    end;
	X ->
	    {error, {rpc, Node, whereis, ?OLD_FILE_SERVER, X}}
    end.

do_start_relay(spawn_link, Filer, Name) ->
    Self = self(),
    Token = make_ref(),
    Relay = spawn_link(fun() -> relay_start(Self, Token, Filer, Name) end),
    receive
	{started, Token} ->
	    {ok, Relay}
    end;
do_start_relay(spawn, Filer, Name) ->
    Self = self(),
    Token = make_ref(),
    Relay = spawn(fun() -> relay_start(Self, Token, Filer, Name) end),
    Mref = erlang:monitor(process, Relay),
    receive
	{started, Token} ->
	    erlang:demonitor(Mref),
	    receive {'DOWN', Mref, _, _, _} -> ok after 0 -> ok end,
	    {ok, Relay};
	{'DOWN', Mref, _, _, Reason} ->
	    exit(Reason)
    end.

relay_start(Parent, Token, Filer, Name) ->
    case catch register(Name, self()) of
	true ->
	    ok;
	_ ->
	    exit({already_started, whereis(Name)})
    end,
    %%% This will fail towards an R5 node or older, Filer is a pid()
    Mref = erlang:monitor(process, Filer),
    process_flag(trap_exit, true),
    Parent ! {started, Token},
    relay_loop(Parent, Filer, Mref).

relay_loop(Parent, Filer, Mref) ->
    receive
	{'DOWN', Mref, _, _, Reason} ->
	    exit(Reason);
	{'EXIT', Parent, Reason} ->
	    exit(Reason);
        Msg ->
            Filer ! Msg
    end,
    relay_loop(Parent, Filer, Mref).
