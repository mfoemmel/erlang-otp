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
-module(gen_tcp).

%% Generic TCP module

-export([connect/3, connect/4, listen/2, accept/1, accept/2, close/1]).
-export([send/2, recv/2, recv/3]).
-export([controlling_process/2]).

-include("inet_int.hrl").

%%
%% Connect a socket
%%
connect(Address, Port, Opts) -> 
    connect(Address,Port,Opts,infinity).

connect(Address, Port, Opts, Time) ->
    Mod = mod(Opts),
    case Mod:getaddr(Address) of
	{ok,IP} ->
	    case  Mod:getserv(Port) of
		{ok,TP} -> Mod:connect(IP, TP, Opts, Time);
		{error,einval} -> exit(badarg);
		Error -> Error
	    end;
	{error,einval} -> exit(badarg);
	Error -> Error
    end.

%%
%% Listen on a tcp port
%%
listen(Port, Opts) ->
    Mod = mod(Opts),
    case Mod:getserv(Port) of
	{ok,TP} ->
	    Mod:listen(TP, Opts);
	{error,einval} ->
	    exit(badarg);
	Other -> Other
    end.

%%
%% Generic tcp accept
%%
accept(S) when record(S, socket) ->
    call(S, {accept, infinity}).

accept(S, infinity) when record(S, socket) ->
    call(S, {accept, infinity});
accept(S, Time) when record(S, socket), integer(Time), Time >= 0 ->
    call(S, {accept, Time}).

%%
%% Close
%%
close(S) when record(S, socket) -> 
    receive
	{tcp_closed, S} -> ok
    after 0 ->
	    inet:close(S)
    end.

%%
%% Send
%%
send(S, Packet) when record(S, socket) ->
    call(S, {send, Packet}).

%%
%% Receive data from a socket (passive mode)
%%
recv(S, Length) when  record(S,socket), integer(Length) ->
    call(S, {recv, Length, infinity}).

recv(S, Length, infinity) when record(S,socket), integer(Length) ->
    call(S, {recv, Length, infinity});
recv(S, Length, Time) when record(S,socket), integer(Length),
                                integer(Time),Time >= 0 ->
    call(S, {recv, Length, Time}).

%%
%% Set controlling process
%%
%%
%% Set controlling process
%%
controlling_process(S, NewOwner) when record(S,socket),pid(NewOwner) ->
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
	{tcp, S, Data} ->
	    Owner ! {tcp, S, Data},
	    sync_input(S, Owner);
	{tcp_closed, S} ->
	    Owner ! {tcp_closed, S},
	    sync_input(S, Owner)
    after 0 -> 
	    ok
    end.

%% call socket server
call(S, Request) ->
    Tag = make_ref(),
    Pid = S#socket.pid,
    Pid ! {call, self(), Tag, Request},
    receive
	{Tag, Reply} -> Reply;
	{'EXIT', Pid, Reason} ->
	    {error, closed};
	{tcp_closed, S} ->
	    {error, closed}
    end.

%% Get the tcp_module
mod() -> inet_db:tcp_module().

%% Get the tcp_module, but option tcp_module overrides
mod(Opts) ->
    case lists:keysearch(tcp_module, 1, Opts) of
	{value, {_, Mod}} -> Mod;
	_ -> mod()
    end.
