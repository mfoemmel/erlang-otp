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
-module(inet_udp).

-export([open/1, open/2, close/1]).
-export([send/4, recv/2]).
-export([controlling_process/2]).

-export([getserv/1, getaddr/1]).

%% internal
-export([open_mgr/3]).

%% system
-export([system_continue/3, system_terminate/4]).

-import(lists, [reverse/1]).

-include("inet_int.hrl").


%% inet_udp port lookup
getserv(Port) when integer(Port) -> {ok, Port};
getserv(Name) when atom(Name)    -> inet:getservbyname(Name,tcp).

%% inet_udp address lookup
getaddr(Address) -> inet:getaddr(Address, inet).


open(Port) -> 
    open(Port, []).

open(Port, Opts) when Port >= 0, Port =< 16#ffff ->
    case inet:socket_options([{port,Port} | Opts]) of
	{ok, St} ->
	    Tag = make_ref(),
	    proc_lib:spawn(?MODULE, open_mgr, [self(),Tag,St]),
	    receive
		{Tag, Reply} -> Reply
	    end;
	{error,Reason} ->
	    exit(Reason)
    end.

send(S,{A,B,C,D},P,Data) when integer(A+B+C+D),P>=0,P=<16#ffff ->
    call(S, {sendto, {A,B,C,D}, P, Data}).

recv(S,Len) ->
    gen_udp:recv(S, Len).

close(S) ->
    gen_udp:close(S).


%%
%% Set controlling process:
%% 1) First sync socket into a known state
%% 2) Move all messages onto the new owners message queue
%% 3) Commit the owner 
%% 4) Wait for ack of new Owner (since socket does some link and unlink)
%%

controlling_process(Socket, NewOwner) ->
    gen_udp:controlling_process(Socket, NewOwner).

%%
%% Call/Cast/Reply
%%
call(Socket, Request) when record(Socket, socket) ->
    Tag = make_ref(),
    Pid = Socket#socket.pid,
    Pid ! {call, self(), Tag, Request},
    receive
	{Tag, Reply} -> Reply;
	{'EXIT', Pid, Reason} ->
	    {error, closed};
	{udp_closed, Socket} ->
	    {error, closed}
    end.

%% reply & convert {exit,Reason} to {error, Reason}
reply(Pid, Tag, {exit,Reason}) when pid(Pid) ->
    Pid ! {Tag, {error,Reason}};
reply(Pid, Tag, Reply) when pid(Pid) ->
    Pid ! {Tag, Reply};
reply(_, _, _) ->     %% the case when Pid is noproc (for loopback send)
    true.

%%
%% Handle exit UNLINK owner before exit otherwise 
%% the owner will get {'EXIT',Pid,normal} in message queue
%%
reply_exit(Owner, From, Tag, Reply) ->
    reply(From, Tag, Reply),
    unlink(Owner),
    exit(normal).

%%
%% Do normal exit handling
%%
handle_exit(Owner) ->
    unlink(Owner),
    exit(normal).

%%
%% Open udp sockets
%%

open_mgr(Owner, Tag, St) ->
    process_flag(trap_exit, true),
    link(Owner),
    case inet:ll_open_set_bind(udp_inet,?INET_AF_INET,St) of
	{ok, Udp} -> socket(Udp, Owner, St, Tag);
	Error -> reply_exit(Owner, Owner, Tag, Error)
    end.

socket(Udp, Owner, St, Tag) ->
    reply(Owner, Tag, {ok, ?mksocket(Udp)}),
    socket_loop(Udp, Owner, St).


socket_owner(Udp, Owner, St, NewOwner) ->
    receive
	{commit_owner, NewOwner} ->
	    unlink(Owner),
	    link(NewOwner),
	    Owner ! {owner, NewOwner},
	    socket_loop(Udp, NewOwner, St);
	{abort_owner, NewOwner} ->
	    socket_loop(Udp, Owner, St)
    end.

socket_loop(Udp, Owner, St) ->
    receive
	{Udp, {data, [?INET_REP_DATA,P1,P0,A,B,C,D|Data]}} ->
	    handle_input(St#sock.fs, {{A,B,C,D},?u16(P1,P0),Data},
			 Udp, Owner, St);

	{call, From, Tag, {sendto,IP,Port,Data}} ->
	    reply(From, Tag, ok),
	    handle_output(St#sock.fs, {IP,Port,Data}, Udp, Owner, St);

	{call, From, Tag, Request} ->
	    case handle_call(Request, Udp, St) of
		{reply, Value, St1} ->
		    reply(From, Tag, Value),
		    socket_loop(Udp, Owner, St1);

		{noreply, St1} ->
		    socket_loop(Udp, Owner, St1);

		{owner,NewOwner,St1} ->
		    reply(From, Tag, ok),
		    socket_owner(Udp, Owner, St1, NewOwner);

		{stop, St1} ->
		    handle_exit(Owner);

		{exit, Reason, St1} ->
		    reply(From,Tag, {error,Reason}),
		    handle_error(Reason,Udp,Owner,St1)
	    end;
	
	{system, From, Msg} ->
	    sys:handle_system_msg(Msg, From, Owner, ?MODULE,
				  St#sock.debug, [Udp,St]);

	{'EXIT', Owner, Reason} -> 
	    true;

	{'EXIT', Udp, Reason} ->
	    handle_error(Reason,Udp,Owner,St);

	Event ->
	    handle_event(St#sock.fs, Event, Udp, Owner, St)
    end.

%% Passive mode error loop Error is the reason for port termination
%% This code should be replied in all calls to the socket
error_loop(Error, Owner, St) ->
    receive
	{call, From, Tag, Req} ->
	    if Req == close ->
		    reply_exit(Owner, From,Tag,ok);
	       %% Handle setopt {active,true} ???
	       true ->
		    reply(From,Tag, {error,Error}),
		    error_loop(Error, Owner, St)
	    end;
	{'EXIT', Owner, _} ->
	    true;
	Event ->
	    %% discard
	    error_loop(Error, Owner, St)
    end.

%% The port has closed
%% determine if error should be logged and if we should enter
%% passive mode loop or not
%% NOTE. We must keep the old port ref since clients will match
%% on the socket record.
handle_error(Reason, Udp, Owner, St) ->
    if St#sock.active == false ->
	    error_loop(Reason, Owner, St);
       true ->
	    S = ?mksocket(Udp),
	    if Reason == normal ->
		    Owner ! {udp_closed, S};
	       true ->
		    Owner ! {udp_error, S, Reason},
		    Owner ! {udp_closed, S}
	    end,
	    handle_exit(Owner)
    end.

%%
%% Handle calls
%%
handle_call({recv,Len}, Udp, St) ->
    case ll_recv(Udp,Len,infinity) of
	{exit,Reason} ->   {exit,Reason,St};
	Reply ->           {reply, Reply, St}
    end;
handle_call({recv,Len,Timeout}, Udp, St) ->
    case ll_recv(Udp,Len,Timeout) of
	{exit,Reason} ->   {exit,Reason,St};
	Reply ->           {reply, Reply, St}
    end;
handle_call({connect,IP,Port}, Udp, St) ->
    case inet:ll_connect(Udp, IP, Port) of
	{exit,Reason} ->   {exit,Reason,St};
	Reply ->           {reply, Reply,St}
    end;
handle_call(Req, Port, St) ->
    inet:handle_call(Req, Port, St).

%% system continue
system_continue(Owner, NDebug, [Udp,St]) ->
    socket_loop(Udp, Owner, St#sock { debug = NDebug }).

%% system terminate
system_terminate(Reason, Owner, Debug, [Udp, St]) ->
    inet:ll_close(Udp).

%%
%% Process input
%%
handle_input(Fs, Data, Port, Owner, St) ->
    handle_input(reverse(Fs), [], Data, Port, Owner, St).

handle_input([], Called, {IP,UP,Data}, Port, Owner, St) ->
    Owner ! {udp, ?mksocket(Port), IP, UP, Data},
    socket_loop(Port, Owner, St#sock { fs = Called });
handle_input([{Fun,S} | Fs], Called, Data, Port, Owner, St) ->
    case Fun(input, Data, S) of
	{input, Data1, S1} ->
	    handle_input(Fs, [{Fun,S1}|Called], Data1, Port, Owner, St);
	{output, Data1, S1} ->
	    handle_output(Called, [{Fun,S1} | Fs], Data1, Port, Owner,St);
	{false, S1} ->
	    socket_loop(Port, Owner, 
			St#sock { fs = reverse([{Fun,S1} | Fs]) ++ Called })
    end.

%%
%% Process output
%%
handle_output(Fs, Data, Port, Owner, St) ->
    handle_output(Fs, [], Data, Port, Owner, St).

handle_output([], Called, {IP,UP,Data}, Port, Owner, St) ->
    ll_sendto(Port, IP, UP, Data),
    socket_loop(Port, Owner, St#sock { fs = reverse(Called) });
handle_output([{Fun,S} | Fs], Called, Data, Port, Owner, St) ->
    case Fun(output, Data, S) of
	{output, Data1, S1} ->
	    handle_output(Fs, [{Fun,S1} | Called], Data1, Port, Owner,St);
	{input, Data1, S1} ->
	    handle_input(Called, [{Fun,S1}|Fs], Data1, Port, Owner, St);
	{false, S1} ->
	    socket_loop(Port, Owner, 
			St#sock { fs = reverse(Called) ++ [{Fun,S1} | Fs] })
    end.

%%
%% Porcess Event
%%
handle_event(Fs, Event, Port, Owner, St) ->
    handle_event(Fs, [], Event, Port, Owner, St).

handle_event([], Called, Event, Port, Owner, St) ->
    io:format("~w: WARNING: event discarded: ~p~n", [?MODULE, Event]),
    socket_loop(Port, Owner, St#sock { fs = reverse(Called) });
handle_event([{Fun,S} | Fs], Called, Event, Port, Owner, St) ->
    case Fun(event, Event, S) of
	{output, Data1, S1} ->
	    handle_output(Fs, [{Fun,S1} | Called], Data1, Port, Owner, St);
	{input, Data1, S1} ->
	    handle_input(Called, [{Fun,S1}|Fs], Data1, Port, Owner, St);
	{event, Event1, S1} ->
	    handle_event(Fs, [{Fun,S1}|Called], Event1, Port, Owner, St);
	{false, S1} ->
	    socket_loop(Port, Owner,
			St#sock { fs = reverse(Called) ++ [{Fun,S1} | Fs] })
    end.

%%
%% infinity is encoded as max int32
%% this value must no be used for other timeout values!!!
%%
enc_time(infinity) -> [255,255,255,255];
enc_time(Time) -> ?int32(Time).

ll_recv(Udp, Length, Time) ->
    case sync_cmd(Udp, 
		  [?UDP_REQ_RECV,enc_time(Time),?int32(Length)],
		  ?INET_REP_DATA) of
	{ok, [P1,P0,A,B,C,D|Data]} ->
	    {ok, {{A,B,C,D}, ?u16(P1,P0), Data}};
	Error ->
	    Error
    end.

ll_sendto(Udp,{A,B,C,D},UdpPort,Data) when integer(UdpPort) ->
    Udp ! {self(), {command, [?UDP_REQ_SENDTO, ?int16(UdpPort),
			      [A,B,C,D], Data]}},
    ok.

sync_cmd(Port, Cmd, Rep) ->
    Port ! {self(), {command, Cmd}},
    receive
	{Port, {data, [Rep | T]}} -> {ok, T};
	{Port, {data, [?INET_REP_ERROR | Err]}} -> {error, list_to_atom(Err)};
	{'EXIT', Port, badsig} -> {error, einval};
	{'EXIT', Port, Reason} -> {exit, Reason}
    end.
