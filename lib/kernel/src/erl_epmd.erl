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
-module(erl_epmd).

-behaviour(gen_server).

%% External exports
-export([start/0, start_link/0, stop/0, port_please/2, names/0, names/1,
	 register_node/2, open/0, open/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-import(lists, [reverse/1]).

-record(state, {socket, port_no = -1, name = ""}).

-define(int16(X), [(X bsr 8) band 16#ff, X band 16#ff]).

-define(u16(X1,X2),
	(((X1) bsl 8) bor X2)).
 
-define(u32(X1,X2,X3,X4), 
	(((X1) bsl 24) bor ((X2) bsl 16) bor ((X3) bsl 8) bor X4)).

-include("erl_epmd.hrl").


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->
    gen_server:start({local, erl_epmd}, ?MODULE, [], []).


start_link() ->
    gen_server:start_link({local, erl_epmd}, ?MODULE, [], []).


stop() ->
    gen_server:call(erl_epmd, stop).


%% Lookup a node "Name" at Host
%% return {port, P, Version} | noport
%%
port_please(Node,HostName) when atom(HostName) ->
  port_please1(Node,atom_to_list(HostName));
port_please(Node,HostName) when list(HostName) ->
  port_please1(Node,HostName);
port_please(Node, EpmdAddr) ->
  get_port(Node, EpmdAddr).

port_please1(Node,HostName) ->
  case inet:gethostbyname(HostName) of
    {ok,{hostent, _Name, _ , _Af, _Size, [EpmdAddr | _]}} ->
      get_port(Node, EpmdAddr);
    Else ->
      Else
  end.

names() -> names(inet:gethostname()).

names(HostName) when atom(HostName) ->
  names1(atom_to_list(HostName));
names(HostName) when list(HostName) ->
  names1(HostName);
names(EpmdAddr) ->
  get_names(EpmdAddr).

names1(HostName) ->
  case inet:gethostbyname(HostName) of
    {ok,{hostent, _Name, _ , _Af, _Size, [EpmdAddr | _]}} ->
      get_names(EpmdAddr);
    Else ->
      Else
  end.


register_node(Name, PortNo) ->
    gen_server:call(erl_epmd, {register, Name, PortNo}, 15000).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

init(_) ->
    {ok, #state{socket = -1}}.
	    
%%----------------------------------------------------------------------

handle_call({register, Name, PortNo}, _From, State) ->
    case State#state.socket of
	P when P < 0 ->
	    case do_register_node(Name, PortNo) of
		{alive, Socket, Creation} ->
		    S = State#state{socket = Socket,
				    port_no = PortNo,
				    name = Name},
		    {reply, {ok, Creation}, S};
		Error ->
		    {reply, Error, State}
	    end;
	_ ->
	    {reply, {error, already_registered}, State}
    end;

handle_call(client_info_req, _From, State) ->
  Reply = {ok,{r4,State#state.name,State#state.port_no}},
  {reply,Reply,State};
  
handle_call(stop, _From, State) ->
    {stop, shutdown, ok, State}.

%%----------------------------------------------------------------------

handle_cast(_, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------

handle_info({tcp_closed, Socket}, State) when State#state.socket == Socket ->
    {noreply, State#state{socket = -1}};
handle_info(_, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------

terminate(_, #state{socket = Socket}) when Socket > 0 ->
    close(Socket),
    ok;
terminate(_, _) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%
%% Epmd socket
%%
open() -> open({127,0,0,1}).  % The localhost IP address.

open(EpmdAddr) ->
    inet_tcp:connect(EpmdAddr, ?erlang_daemon_port, []).

close(Socket) ->
    inet_tcp:close(Socket).


do_register_node_v0(NodeName, TcpPort) ->
    case open() of
	{ok, Socket} ->
	    Name = cstring(NodeName),
	    Len = 1+2+length(Name),
	    inet_tcp:send(Socket, [?int16(Len), ?EPMD_ALIVE,
				   ?int16(TcpPort), Name]),
	    wait_for_reg_reply_v0(Socket, []);
	Error ->
	    Error
    end.

do_register_node(NodeName, TcpPort) ->
    case open() of
	{ok, Socket} ->
	    Name = to_string(NodeName),
	    Extra = "",
	    Elen = length(Extra),
	    Len = 1+2+1+1+2+2+2+length(Name)+2+Elen,
	    inet_tcp:send(Socket, [?int16(Len), ?EPMD_ALIVE2_REQ,
				   ?int16(TcpPort),
				   $M,
				   0,
				   ?int16(epmd_dist_high()),
				   ?int16(epmd_dist_low()),
				   ?int16(length(Name)),
				   Name,
				   ?int16(Elen),
				   Extra]),
	    case wait_for_reg_reply(Socket, []) of
		{error, epmd_close} ->
		    %% could be old epmd; try old protocol
%		    erlang:display('trying old'),
		    do_register_node_v0(NodeName, TcpPort);
		Other ->
		    Other
	    end;
	Error ->
	    Error
    end.

epmd_dist_high() ->
    case os:getenv("ERL_EPMD_DIST_HIGH") of
	false ->
	   ?epmd_dist_high; 
	Version ->
	    case (catch list_to_integer(Version)) of
		N when integer(N), N < ?epmd_dist_high ->
		    N;
		_ ->
		   ?epmd_dist_high
	    end
    end.

epmd_dist_low() ->
    case os:getenv("ERL_EPMD_DIST_LOW") of
	false ->
	   ?epmd_dist_low; 
	Version ->
	    case (catch list_to_integer(Version)) of
		N when integer(N), N > ?epmd_dist_low ->
		    N;
		_ ->
		   ?epmd_dist_low
	    end
    end.
		    


%%% (When we reply 'duplicate_name', it's because it's the most likely
%%% reason; there is no interpretation of the error result code.)
wait_for_reg_reply(Socket, SoFar) ->
    receive
	{tcp, Socket, Data0} ->
	    case SoFar ++ Data0 of
		[$y, Result, A, B] ->
		    case Result of
			0 ->
			    {alive, Socket, ?u16(A, B)};
			_ ->
			    {error, duplicate_name}
		    end;
		Data when length(Data) < 4 ->
		    wait_for_reg_reply(Socket, Data);
		Garbage ->
		    {error, {garbage_from_epmd, Garbage}}
	    end;
	{tcp_closed, Socket} ->
	    {error, epmd_close}
    after 10000 ->
	    inet_tcp:close(Socket),
	    {error, no_reg_reply_from_epmd}
    end.
    
wait_for_reg_reply_v0(Socket, SoFar) ->
    receive
	{tcp, Socket, Data0} ->
	    case SoFar ++ Data0 of
		[$Y, A, B] ->
		    {alive, Socket, ?u16(A, B)};
		Data when length(Data) < 3 ->
		    wait_for_reg_reply(Socket, Data);
		Garbage ->
		    {error, {garbage_from_epmd, Garbage}}
	    end;
	{tcp_closed, Socket} ->
	    {error, duplicate_name}		% A guess -- the most likely reason.
    after 10000 ->
	    inet_tcp:close(Socket),
	    {error, no_reg_reply_from_epmd}
    end.
%%
%% Lookup a node "Name" at Host
%%
get_port_v0(Node, EpmdAddress) ->
    case open(EpmdAddress) of
	{ok, Socket} ->
	    Name = cstring(Node),
	    Len = 1+length(Name),
	    inet_tcp:send(Socket, [?int16(Len),?EPMD_PORT_PLEASE, Name]),
	    wait_for_port_reply_v0(Socket, []);
	Error -> 
	    noport
    end.

get_port(Node, EpmdAddress) ->
    case open(EpmdAddress) of
	{ok, Socket} ->
	    Name = to_string(Node),
	    Len = 1+length(Name),
	    inet_tcp:send(Socket, [?int16(Len),?EPMD_PORT_PLEASE2_REQ, Name]),
	    Reply = wait_for_port_reply(Socket, []),
	    case Reply of
		closed ->
		    get_port_v0(Node, EpmdAddress);
		Other ->
		    Other
	    end;
	Error -> 
	    noport
    end.

wait_for_port_reply_v0(Socket, SoFar) ->
    receive
	{tcp, Socket, Data0} ->
%	    io:format("got ~p~n", [Data0]),
	    case SoFar ++ Data0 of
		[A, B] ->
		    wait_for_close(Socket, {port, ?u16(A, B), 0});
%		    wait_for_close(Socket, {port, ?u16(A, B)});
		Data when length(Data) < 2 ->
		    wait_for_port_reply_v0(Socket, Data);
		Garbage ->
		    {error, {garbage_from_epmd, Garbage}}
	    end;
	{tcp_closed, Socket} ->
	    noport
    after 10000 ->
	    inet_tcp:close(Socket),
	    noport
    end.

wait_for_port_reply(Socket, SoFar) ->
    receive
	{tcp, Socket, Data0} ->
%	    io:format("got ~p~n", [Data0]),
	    case SoFar ++ Data0 of
		[$w, Result | Rest] ->
		    case Result of
			0 ->
			    wait_for_port_reply_cont(Socket, Rest);
			_ ->
			    wait_for_close(Socket, noport)
		    end;
		Data when length(Data) < 2 ->
		    wait_for_port_reply(Socket, Data);
		Garbage ->
		    {error, {garbage_from_epmd, Garbage}}
	    end;
	{tcp_closed, Socket} ->
	    closed
    after 10000 ->
	    inet_tcp:close(Socket),
	    noport
    end.

wait_for_port_reply_cont(Socket, SoFar) when length(SoFar) >= 10 ->
    wait_for_port_reply_cont2(Socket, SoFar);
wait_for_port_reply_cont(Socket, SoFar) ->
    receive
	{tcp, Socket, Data0} ->
	    case SoFar ++ Data0 of
		Data when length(Data) >= 10 ->
		    wait_for_port_reply_cont2(Socket, Data);
		Data when length(Data) < 10 ->
		    wait_for_port_reply_cont(Socket, Data);
		Garbage ->
		    {error, {garbage_from_epmd, Garbage}}
	    end;
	{tcp_closed, Socket} ->
	    noport
    after 10000 ->
	    inet_tcp:close(Socket),
	    noport
    end.

wait_for_port_reply_cont2(Socket, Data) ->
    [A, B, Type, Proto, HighA, HighB,
     LowA, LowB, NLenA, NLenB | Rest] = Data,
    Name = wait_for_port_reply_name(Socket,
				    ?u16(NLenA, NLenB),
				    Rest),
    Low = ?u16(LowA, LowB),
    High = ?u16(HighA, HighB),
    Version = best_version(Low, High),
%    io:format("Returning ~p~n", [{port, ?u16(A, B), Version}]),
    {port, ?u16(A, B), Version}.
%    {port, ?u16(A, B)}.

%%% Throw away the rest of the message; we won't use any of it anyway,
%%% currently.
wait_for_port_reply_name(Socket, Len, Sofar) ->
    receive
	{tcp, Socket, Data} ->
%	    io:format("data = ~p~n", Data),
	    wait_for_port_reply_name(Socket, Len, Sofar);
	{tcp_closed, Socket} ->
	    "foobar"
    end.
		    

best_version(Low, High) ->
    OurLow =  epmd_dist_low(),
    OurHigh =  epmd_dist_high(),
    select_best_version(OurLow, OurHigh, Low, High).

%%% We silently assume that the low's are not greater than the high's.
%%% We should report if the intervals don't overlap.
select_best_version(L1, H1, L2, H2) when L1 > H2 ->
    0;
select_best_version(L1, H1, L2, H2) when L2 > H1 ->
    0;
select_best_version(L1, H1, L2, H2) when L2 > H1 ->
    0;
select_best_version(L1, H1, L2, H2) ->
    min(H1, H2).

min(A, B) when A < B ->
    A;
min(A, B) ->
    B.

wait_for_close(Socket, Reply) ->
    receive
	{tcp_closed, Socket} -> 
	    Reply
    after 10000 ->
	    inet_tcp:close(Socket),
	    Reply
    end.


%%
%% Creates a (flat) null terminated string from atom or list.
%%
cstring(S) when atom(S) -> cstring(atom_to_list(S));
cstring(S) when list(S) -> S ++ [0].

to_string(S) when atom(S) -> atom_to_list(S);
to_string(S) when list(S) -> S.

%%
%% Find names on epmd
%%
%%
get_names(EpmdAddress) ->
    case open(EpmdAddress) of
	{ok, Socket} ->
	    do_get_names(Socket);
	Error ->
	    {error, address}
    end.

do_get_names(Socket) ->
    inet_tcp:send(Socket, [?int16(1),?EPMD_NAMES]),
    receive
	{tcp, Socket, [P0,P1,P2,P3|T]} ->
	    EpmdPort = ?u32(P0,P1,P2,P3),
	    if EpmdPort == ?erlang_daemon_port ->
		    names_loop(Socket, T, []);
	       true ->
		    close(Socket),
		    {error, address}
	    end;
	{tcp_closed, Socket} ->
	    {ok, []}
    end.

names_loop(Socket, Acc, Ps) ->
    receive
	{tcp, Socket, Bytes} ->
	    {NAcc, NPs} = scan_names(Acc ++ Bytes, Ps),
	    names_loop(Socket, NAcc, NPs);
	{tcp_closed, Socket} ->
	    {_, NPs} = scan_names(Acc, Ps),
	    {ok, NPs}
    end.

scan_names(Buf, Ps) ->
    case scan_line(Buf, []) of
	{Line, NBuf} ->
	    case parse_line(Line) of
		{ok, Entry} -> 
		    scan_names(NBuf, [Entry | Ps]);
		error ->
		    scan_names(NBuf, Ps)
	    end;
	[] -> {Buf, Ps}
    end.


scan_line([$\n | Buf], Line) -> {reverse(Line), Buf};
scan_line([C | Buf], Line) -> scan_line(Buf, [C|Line]);
scan_line([], _) -> [].

parse_line(["name " ++ Buf0]) ->
    case parse_name(Buf0, []) of
	{Name, Buf1}  ->
	    case Buf1 of
		["at port " ++ Buf2] ->
		    case catch list_to_integer(Buf2) of
			{'EXIT', _} -> error;
			Port -> {ok, {Name, Port}}
		    end;
		_ -> error
	    end;
	error -> error
    end;
parse_line(_) -> error.


parse_name([$\s | Buf], Name) -> {reverse(Name), Buf};
parse_name([C | Buf], Name) -> parse_name(Buf, [C|Name]);
parse_name([], Name) -> error.



