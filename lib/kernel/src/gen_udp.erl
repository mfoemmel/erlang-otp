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
-module(gen_udp).

%% Generic UDP module

-export([open/1, open/2, close/1]).
-export([send/4, recv/2, recv/3]).
-export([controlling_process/2]).

-include("inet_int.hrl").

open(Port) -> 
    open(Port, []).

open(Port, Opts) ->
    Mod = mod(Opts),
    {ok,UP} = Mod:getserv(Port),
    Mod:open(Port, Opts).

close(S) when record(S, socket) ->
    receive 
	{udp_closed, S} -> ok
    after 0 ->
	    inet:close(S)
    end.

send(S, Address, Port, Packet) when record(S,socket) ->
    Mod = S#socket.type,
    case Mod:getaddr(Address) of
	{ok,IP} ->
	    case Mod:getserv(Port) of
		{ok,UP} -> call(S, {sendto, IP, UP, Packet});
		{error,einval} -> exit(badarg);
		Error -> Error
	    end;
	{error,einval} -> exit(badarg);
	Error -> Error
    end.

recv(S,Len) when record(S, socket), integer(Len) ->
    call(S, {recv, Len, infinity}).

recv(S,Len,Time) when record(S, socket),
		      integer(Len),
		      integer(Time),
		      Time >= 0 ->
    call(S, {recv, Len, Time});
recv(S,Len,infinity) when record(S, socket), integer(Len) ->
    call(S, {recv, Len, infinity}).

%%
%% Set controlling process
%%
%%
%% Set controlling process
%%
controlling_process(S, NewOwner) when record(S, socket), pid(NewOwner) ->
    case call(S, {set_owner, NewOwner}) of
	ok -> 
	    sync_input(S, NewOwner),
	    S#socket.pid ! {commit_owner, NewOwner},
	    receive
		{owner, NewOwner} -> ok
	    end;
	Error -> Error
    end.

sync_input(S, Owner) ->
    receive
	{udp, S, IP, UP, Data} ->
	    Owner ! {udp, S, IP, UP, Data},
	    sync_input(S, Owner);
	{udp_closed, S} ->
	    Owner ! {udp_closed, S},
	    sync_input(S, Owner)
    after 0 -> 
	    ok
    end.

call(S, Request) ->
    Tag = make_ref(),
    Pid = S#socket.pid,
    Pid ! {call, self(), Tag, Request},
    receive
	{Tag, Reply} -> Reply;
	{'EXIT', Pid, Reason} ->
	    {error, closed};
	{udp_closed, S} ->
	    {error, closed}
    end.

%% Get the udp_module
mod() -> inet_db:udp_module().

%% Get the udp_module, but option udp_module overrides
mod(Opts) when list(Opts) ->
    case lists:keysearch(udp_module, 1, Opts) of
	{value, {_, Mod}} -> Mod;
	_ -> mod()
    end.
