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
-module(prim_inet).

%% Primitive inet_drv interface


-export([open/1, open/2, fdopen/2, fdopen/3, close/1]).
-export([bind/3, listen/1, listen/2]). 
-export([connect/3, connect/4, async_connect/4]).
-export([accept/1, accept/2, async_accept/2]).
-export([shutdown/2]).
-export([send/2, sendto/4]).
-export([recv/2, recv/3, async_recv/3]).
-export([unrecv/2]).
-export([recvfrom/2, recvfrom/3]).
-export([setopt/3, setopts/2, getopt/2, getopts/2, is_sockopt_val/2]).
-export([getstat/2, getfd/1, getindex/1, getstatus/1, gettype/1, 
	 getiflist/1, ifget/3, ifset/3,
	 gethostname/1]).
-export([getservbyname/3, getservbyport/3]).
-export([peername/1, setpeername/2]).
-export([sockname/1, setsockname/2]).
-export([attach/1, detach/1]).

-include("inet_int.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% OPEN(stream | dgram, inet | inet6)  ->
%%       {ok, insock()} |
%%       {error, Reason}
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

open(Type)          -> open1(Type, ?INET_AF_INET).

open(Type,   inet)  -> open1(Type, ?INET_AF_INET);
open(Type,  inet6)  -> open1(Type, ?INET_AF_INET6);
open(_, _)          -> {error, einval}.

fdopen(Type,Fd)         -> fdopen1(Type, ?INET_AF_INET, Fd).

fdopen(Type, Fd, inet)  -> fdopen1(Type, ?INET_AF_INET, Fd);
fdopen(Type, Fd, inet6) -> fdopen1(Type, ?INET_AF_INET6, Fd);
fdopen(_, _, _)         -> {error, einval}.
    
open1(Type, Family) ->
    case open0(Type) of
	{ok, S} ->
	    case ctl_cmd(S,?INET_REQ_OPEN,[Family]) of
		{ok, _} -> {ok,S};
		Error -> close(S), Error
	    end;
	Error -> Error
    end.

fdopen1(Drv, Family, Fd) when integer(Fd) ->
    case open0(Drv) of
	{ok, S} ->
	    case ctl_cmd(S,?INET_REQ_FDOPEN,[Family,?int32(Fd)]) of
		{ok, _} -> {ok,S};
		Error -> close(S), Error
	    end;
	Error -> Error
    end.

open0(Type) ->
    case catch case Type of
		   stream -> erlang:open_port({spawn,tcp_inet}, [binary]);
		   dgram  -> erlang:open_port({spawn, udp_inet}, [binary]);
		   _  -> {error, einval}
	       end of
	Port when port(Port) -> {ok, Port};
	{'EXIT', Reason} -> {error, Reason};
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Shutdown(insock(), atom()) -> ok
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

shutdown(S, read) when is_port(S) ->
    shutdown_2(S, 0);
shutdown(S, write) when is_port(S) ->
    shutdown_1(S, 1);
shutdown(S, read_write) when is_port(S) ->
    shutdown_1(S, 2).

shutdown_1(S, How) ->
    case subscribe(S, [subs_empty_out_q]) of
	{ok,[{subs_empty_out_q,N}]} when N > 0 ->
	    shutdown_pend_loop(S, N);   %% wait for pending output to be sent
	_Other -> ok
    end,
    shutdown_2(S, How).

shutdown_2(S, How) ->
    case ctl_cmd(S, ?TCP_REQ_SHUTDOWN, [How]) of
	{ok, []} -> ok;
	Error -> Error
    end.

shutdown_pend_loop(S, N0) ->
    receive
	{empty_out_q,S} -> ok
    after ?INET_CLOSE_TIMEOUT ->
	    case getstat(S, [send_pend]) of
                {ok,[{send_pend,N0}]} -> ok;
                {ok,[{send_pend,N}]} -> shutdown_pend_loop(S, N);
		_ -> ok
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% CLOSE(insock()) -> ok
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

close(S) when port(S) ->
    unlink(S),               %% avoid getting {'EXIT', S, Reason}
    case subscribe(S, [subs_empty_out_q]) of
	{ok, [{subs_empty_out_q,N}]} when N > 0 ->
	    close_pend_loop(S, N);   %% wait for pending output to be sent
	_ ->
	    catch erlang:port_close(S),
	    ok
    end.

close_pend_loop(S, N) ->
    receive
	{empty_out_q,S} ->
	    catch erlang:port_close(S), ok
    after ?INET_CLOSE_TIMEOUT ->
	    case getstat(S, [send_pend]) of
                {ok, [{send_pend,N1}]} ->
                    if N1 == N -> catch erlang:port_close(S), ok;
                       true -> close_pend_loop(S, N1)
                    end;
		_ ->
		    catch erlang:port_close(S), ok
	    end
    end.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% BIND(insock(), IP, Port) -> {ok, integer()} | {error, Reason}
%%
%% bind the insock() to the interface address given by IP and Port
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bind(S,IP,Port) when port(S), Port >= 0, Port =< 65535 ->
    case ctl_cmd(S,?INET_REQ_BIND,[?int16(Port),ip_to_bytes(IP)]) of
	{ok, [P1,P0]} -> {ok, ?u16(P1, P0)};
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% CONNECT(insock(), IP, Port [,Timeout]) -> ok | {error, Reason}
%%
%% connect the insock() to the address given by IP and Port
%% if timeout is given:
%%       timeout < 0  -> infinity
%%                 0  -> immediate connect (mostly works for loopback)
%%               > 0  -> wait for timout ms if not connected then 
%%                       return {error, timeout} 
%%
%% ASYNC_CONNECT(insock(), IP, Port, Timeout) -> {ok, S, Ref} | {error, Reason}
%%
%%  a {inet_async,S,Ref,Status} will be sent on socket condition
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connect(S, IP, Port) -> connect0(S, IP, Port, -1).

connect(S, IP, Port, infinity) -> connect0(S, IP, Port, -1);
connect(S, IP, Port, Time)     -> connect0(S, IP, Port, Time).

connect0(S, IP, Port, Time) when port(S), Port > 0, Port =< 65535,
				 integer(Time) ->
    case async_connect(S, IP, Port, Time) of
	{ok, S, Ref} ->
	    receive
		{inet_async, S, Ref, Status} ->
		    Status
	    end;
	Error -> Error
    end.

async_connect(S, IP, Port, Time) ->
    case ctl_cmd(S, ?INET_REQ_CONNECT,
		 [enc_time(Time),?int16(Port),ip_to_bytes(IP)]) of
	{ok, [R1,R0]} -> {ok, S, ?u16(R1,R0)};
	Error -> Error
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% ACCEPT(insock() [,Timeout] ) -> {ok,insock()} | {error, Reason}
%%
%% accept incoming connection on listen socket 
%% if timeout is given:
%%       timeout < 0  -> infinity
%%                 0  -> immediate accept (poll)
%%               > 0  -> wait for timout ms for accept if no accept then 
%%                       return {error, timeout}
%%
%% ASYNC_ACCEPT(insock(), Timeout)
%%
%%  async accept. return {ok,S,Ref} or {error, Reason}
%%  the owner of socket S will receive an {inet_async,S,Ref,Status} on 
%%  socket condition
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

accept(L)            -> accept0(L, -1).

accept(L, infinity)  -> accept0(L, -1);
accept(L, Time)      -> accept0(L, Time).

accept0(L, Time) when port(L), integer(Time) ->
    case async_accept(L, Time) of
	{ok, S, Ref} ->
	    receive 
		{inet_async, S, Ref, Status} ->
		    case Status of
			ok -> accept_opts(L, S);
			Error -> close(S), Error
		    end
	    end;
	Error -> Error
    end.

%% setup options from listen socket on the connected socket
accept_opts(L, S) ->
    case getopts(L, [active, nodelay, keepalive, delay_send, priority, tos]) of
	{ok, Opts} ->
	    case setopts(S, Opts) of
		ok -> {ok, S};
		Error -> close(S), Error
	    end;
	Error ->
	    close(S), Error
    end.

async_accept(L, Time) ->
    case getindex(L) of
	{ok, IX} ->
	    case gettype(L) of
		{ok, {_, Type}} ->
		    case open0(Type) of
			{ok,S} ->
			    case ctl_cmd(S,?TCP_REQ_ACCEPT,
					 [enc_time(Time),?int16(IX)]) of
				{ok, [R1,R0]} -> {ok, S, ?u16(R1,R0)};
				Error -> close(S), Error
			    end;
			Error -> Error
		    end;
		Error -> Error
	    end;
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% LISTEN(insock() [,Backlog]) -> ok | {error, Reason}
%%
%% set listen mode on socket
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
listen(S) -> listen(S, ?LISTEN_BACKLOG).
    
listen(S, BackLog) when port(S), integer(BackLog) ->
    case ctl_cmd(S, ?TCP_REQ_LISTEN, [?int16(BackLog)]) of
	{ok, _} -> ok;
	Error  -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% SEND(insock(), Data) -> ok | {error, Reason}
%%
%% send Data on the socket (io-list)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

send(S, Data) when port(S) ->
    case catch erlang:port_command(S, Data) of
	true ->
	    receive
		{inet_reply, S, Status} -> Status
	    end;
	{'EXIT', _Reason} -> 
	    {error, einval}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% SENDTO(insock(), IP, Port, Data) -> ok | {error, Reason}
%%
%% send Datagram to the IP at port (Should add sync send!)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sendto(S,IP,Port,Data) when port(S), Port >= 0, Port =< 65535 ->
    case catch erlang:port_command(S, [?int16(Port), ip_to_bytes(IP),Data]) of
	true -> 
	    receive
		{inet_reply, S, Reply} -> Reply
	    end;
	{'EXIT', _Reason} -> 
	    {error, einval}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% RECV(insock(), Lenth [Timeout]) -> {ok,Data} | {error, Reason}
%%
%% receive Length data bytes from a socket
%% if 0 is given then a Data packet is requested (see setopt (packet))
%%    N read N bytes
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
recv(S, Length) -> recv0(S, Length, -1).

recv(S, Length, infinity) -> recv0(S, Length,-1);

recv(S, Length, Time) when integer(Time) -> recv0(S, Length, Time).

recv0(S, Length, Time) when port(S), integer(Length), Length >= 0 ->
    case async_recv(S, Length, Time) of
	{ok, Ref} ->
	    receive
		{inet_async, S, Ref, Status} -> Status;
		{'EXIT', S, _Reason} ->
		    {error, closed}
	    end;
	Error -> Error
    end.
	     

async_recv(S, Length, Time) ->
    case ctl_cmd(S, ?TCP_REQ_RECV, [enc_time(Time), ?int32(Length)]) of
	{ok,[R1,R0]} -> {ok, ?u16(R1,R0)};
	Error -> Error
    end.
		    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% RECVFROM(insock(), Lenth [Timeout]) -> {ok,{IP,Port,Data}} | {error, Reason}
%%
%% receive Length data bytes from a datagram socket sent from IP at Port
%% if 0 is given then a Data packet is requested (see setopt (packet))
%%    N read N bytes
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

recvfrom(S, Length) -> recvfrom0(S, Length, -1).

recvfrom(S, Length, infinity)                -> recvfrom0(S, Length, -1);
recvfrom(S, Length, Time) when integer(Time) -> recvfrom0(S, Length, Time).

recvfrom0(S, Length, Time) when port(S), integer(Length), Length >= 0 ->
    case ctl_cmd(S, ?UDP_REQ_RECV,[enc_time(Time),?int32(Length)]) of
	{ok,[R1,R0]} ->
	    Ref = ?u16(R1,R0),
	    receive
		{inet_async, S, Ref, {ok, [F,P1,P0 | AddrData]}} ->
		    {IP,Data} = get_ip(F, AddrData),
		    {ok, {IP, ?u16(P1,P0), Data}};
		{inet_async, S, Ref, Status} ->
		    Status
	    end;
	Error ->
	    Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% PEERNAME(insock()) -> {ok, {IP, Port}} | {error, Reason}
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

peername(S) when port(S) ->
    case ctl_cmd(S, ?INET_REQ_PEER, []) of
	{ok, [F, P1,P0 | Addr]} ->
	    {IP, _} = get_ip(F, Addr),
	    {ok, { IP, ?u16(P1, P0) }};
	Error -> Error
    end.

setpeername(S, {IP,Port}) when port(S) ->
    case ctl_cmd(S, ?INET_REQ_SETPEER, [?int16(Port),ip_to_bytes(IP)]) of
	{ok,[]} -> ok;
	Error -> Error
    end;
setpeername(S, undefined) when port(S) ->
    case ctl_cmd(S, ?INET_REQ_SETPEER, []) of
	{ok,[]} -> ok;
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% SOCKNAME(insock()) -> {ok, {IP, Port}} | {error, Reason}
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sockname(S) when port(S) ->
    case ctl_cmd(S, ?INET_REQ_NAME, []) of
	{ok, [F, P1, P0 | Addr]} ->
	    {IP, _} = get_ip(F, Addr),
	    {ok, { IP, ?u16(P1, P0) }};
	Error -> Error
    end.

setsockname(S, {IP,Port}) when port(S) ->
    case ctl_cmd(S, ?INET_REQ_SETNAME, [?int16(Port),ip_to_bytes(IP)]) of
	{ok,[]} -> ok;
	Error -> Error
    end;
setsockname(S, undefined) when port(S) ->
    case ctl_cmd(S, ?INET_REQ_SETNAME, []) of
	{ok,[]} -> ok;
	Error -> Error
    end.
	     

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% SETOPT(insock(), Opt, Value) -> ok | {error, Reason}
%% SETOPTS(insock(), [{Opt,Value}]) -> ok | {error, Reason}
%%
%% set socket, ip and driver option
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setopt(S, Opt, Value) when port(S) -> 
    setopts(S, [{Opt,Value}]).

setopts(S, Opts) when port(S) ->
    case encode_opt_val(Opts) of
	{ok, Buf} ->
	    case ctl_cmd(S, ?INET_REQ_SETOPTS, Buf) of
		{ok, _} -> ok;
		Error -> Error
	    end;
	Error  -> Error
    end.	    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% GETOPT(insock(), Opt) -> {ok,Value} | {error, Reason}
%% GETOPTS(insock(), [Opt]) -> {ok, [{Opt,Value}]} | {error, Reason}
%% get socket, ip and driver option
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getopt(S, Opt) when port(S), atom(Opt) ->
    case getopts(S, [Opt]) of
	{ok,[{_,Value}]} -> {ok, Value};
	Error -> Error
    end.

getopts(S, Opts) when port(S), list(Opts) ->
    case encode_opts(Opts) of
	{ok,Buf} ->
	    case ctl_cmd(S, ?INET_REQ_GETOPTS, Buf) of
		{ok, Rep} -> decode_opt_val(Rep);
		Error -> Error
	    end;
	Error -> Error
    end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% IFLIST(insock()) -> {ok,IfNameList} | {error, Reason}
%%
%% get interface name list
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getiflist(S) when port(S) ->
    case ctl_cmd(S, ?INET_REQ_GETIFLIST, []) of
	{ok, Data} -> {ok, build_iflist(Data)};
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% ifget(insock(), IFOpts) -> {ok,IfNameList} | {error, Reason}
%%
%% get interface name list
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ifget(S, Name, Opts) ->
    case encode_ifname(Name) of
	{ok, Buf1} ->
	    case encode_ifopts(Opts,[]) of
		{ok, Buf2} ->
		    case ctl_cmd(S, ?INET_REQ_IFGET, [Buf1,Buf2]) of
			{ok, Data} -> decode_ifopts(Data,[]);
			Error -> Error
		    end;
		Error -> Error
	    end;
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% ifset(insock(), Name, IFOptVals) -> {ok,IfNameList} | {error, Reason}
%%
%% set interface parameters
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ifset(S, Name, Opts) ->
    case encode_ifname(Name) of
	{ok, Buf1} ->
	    case encode_ifopt_val(Opts,[]) of
		{ok, Buf2} ->
		    case ctl_cmd(S, ?INET_REQ_IFSET, [Buf1,Buf2]) of
			{ok, _} -> ok;
			Error -> Error
		    end;
		Error -> Error
	    end;
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% subscribe(insock(), SubsList) -> {ok,StatReply} | {error, Reason}
%%
%% Subscribe on socket events (from driver)
%%
%% Available event subscriptions:
%%   subs_empty_out_q: StatReply = [{subs_empty_out_q, N}], where N
%%                     is current queue length. When the queue becomes empty
%%                     a {empty_out_q, insock()} message will be sent to
%%                     subscribing process and the subscription will be
%%                     removed. If N = 0, the queue is empty and no
%%                     subscription is made.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subscribe(S, Sub) when port(S),list(Sub) ->
    case encode_subs(Sub) of
	{ok, Bytes} ->
	    case ctl_cmd(S, ?INET_REQ_SUBSCRIBE, Bytes) of
		{ok, Data} -> decode_subs(Data);
		Error -> Error
	    end;
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% GETSTAT(insock(), StatList) -> {ok,StatReply} | {error, Reason}
%%
%% get socket statistics (from driver)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getstat(S, Stats) when port(S),list(Stats) ->
    case encode_stats(Stats) of
	{ok, Bytes} ->
	    case ctl_cmd(S, ?INET_REQ_GETSTAT, Bytes) of
		{ok, Data} -> decode_stats(Data);
		Error -> Error
	    end;
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% GETFD(insock()) -> {ok,integer()} | {error, Reason}
%%
%% get internal file descriptor
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getfd(S) when port(S) ->
    case ctl_cmd(S, ?INET_REQ_GETFD, []) of
	{ok, [S3,S2,S1,S0]} -> {ok, ?u32(S3,S2,S1,S0)};
	Error -> Error
    end.        

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% GETIX(insock()) -> {ok,integer()} | {error, Reason}
%%
%% get internal socket index
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getindex(S) when port(S) ->
    case ctl_cmd(S, ?INET_REQ_GETIX, []) of
	{ok, [I3,I2,I1,I0]} -> {ok, ?u32(I3,I2,I1,I0)};
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% GETTYPE(insock()) -> {ok,{Family,Type}} | {error, Reason}
%%
%% get family/type of a socket
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gettype(S) when port(S) ->
    case ctl_cmd(S, ?INET_REQ_GETTYPE, []) of
	{ok, [F3,F2,F1,F0,T3,T2,T1,T0]} ->
	    Family = case ?u32(F3,F2,F1,F0) of
			 ?INET_AF_INET -> inet;
			 ?INET_AF_INET6 -> inet6;
			 _ -> undefined
		     end,
	    Type = case ?u32(T3,T2,T1,T0) of
		       ?INET_TYPE_STREAM -> stream;
		       ?INET_TYPE_DGRAM  -> dgram;
		       _ -> undefined
		   end,
	    {ok, {Family, Type}};
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% GETSTATUS(insock()) -> {ok,Status} | {error, Reason}
%%
%% get socket status
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getstatus(S) when port(S) ->
    case ctl_cmd(S, ?INET_REQ_GETSTATUS, []) of
	{ok, [S3,S2,S1,S0]} ->	
	    {ok, dec_status(?u32(S3,S2,S1,S0))};
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% GETHOSTNAME(insock()) -> {ok,HostName} | {error, Reason}
%%
%% get host name
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

gethostname(S) when port(S) ->
    ctl_cmd(S, ?INET_REQ_GETHOSTNAME, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% GETSERVBYNAME(insock(),Name,Proto) -> {ok,Port} | {error, Reason}
%%
%% get service port
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getservbyname(S,Name,Proto) when port(S),atom(Name),atom(Proto) ->
    getservbyname1(S, atom_to_list(Name), atom_to_list(Proto));
getservbyname(S,Name,Proto) when port(S),atom(Name),list(Proto) ->
    getservbyname1(S, atom_to_list(Name), Proto);
getservbyname(S,Name,Proto) when port(S),list(Name),atom(Proto) ->
    getservbyname1(S, Name, atom_to_list(Proto));
getservbyname(S,Name,Proto) when port(S),list(Name),list(Proto) ->
    getservbyname1(S, Name, Proto);
getservbyname(_,_, _) ->
    {error, einval}.

getservbyname1(S,Name,Proto) ->
    L1 = length(Name),
    L2 = length(Proto),
    if L1 > 255 -> {error, einval};
       L2 > 255 -> {error, einval};
       true ->
	    case ctl_cmd(S, ?INET_REQ_GETSERVBYNAME, [L1,Name,L2,Proto]) of
		{ok, [P1,P0]} ->
		    {ok, ?u16(P1,P0)};
		Error -> 
		    Error
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% GETSERVBYPORT(insock(),Port,Proto) -> {ok,Port} | {error, Reason}
%%
%% get service port from portnumber and protocol
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

getservbyport(S,Port,Proto) when port(S),atom(Proto) ->
    getservbyport1(S, Port, atom_to_list(Proto));
getservbyport(S,Port,Proto) when port(S),list(Proto) ->
    getservbyport1(S, Port, Proto);
getservbyport(_, _, _) ->
    {error, einval}.

getservbyport1(S,Port,Proto) ->
    L = length(Proto),
    if Port < 0 -> {error, einval};
       Port > 16#ffff -> {error, einval};
       L > 255 -> {error, einval};
       true ->
	    case ctl_cmd(S, ?INET_REQ_GETSERVBYPORT, [?int16(Port),L,Proto]) of
		{ok, Name} -> {ok, Name};
		Error -> Error
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% UNRECV(insock(), data) -> ok | {error, Reason}
%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unrecv(S, Data) ->
    case ctl_cmd(S, ?TCP_REQ_UNRECV, Data) of
	{ok, _} -> ok;
	Error  -> Error
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% DETACH(insock()) -> ok
%%
%%   unlink from a socket 
%%
%% ATTACH(insock()) -> ok | {error, Reason}
%%
%%   link and connect to a socket 
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

detach(S) when port(S) ->
    unlink(S),
    ok.

attach(S) when port(S) ->
    case catch erlang:port_connect(S, self()) of
	true -> link(S), ok;
	{'EXIT', Reason} -> {error, Reason}
    end.

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% INTERNAL FUNCTIONS
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_sockopt_val(Opt, Val) ->
    case type_opt(Opt) of
	undefined -> false;
	Type -> type_value(Type,Val)
    end.

%%
%% Socket options processing
%%
enc_opt(reuseaddr)       -> ?INET_OPT_REUSEADDR;
enc_opt(keepalive)       -> ?INET_OPT_KEEPALIVE;
enc_opt(dontroute)       -> ?INET_OPT_DONTROUTE;
enc_opt(linger)          -> ?INET_OPT_LINGER;
enc_opt(broadcast)       -> ?INET_OPT_BROADCAST;
enc_opt(sndbuf)          -> ?INET_OPT_SNDBUF;
enc_opt(recbuf)          -> ?INET_OPT_RCVBUF;
enc_opt(priority)        -> ?INET_OPT_PRIORITY;
enc_opt(tos)             -> ?INET_OPT_TOS;
enc_opt(nodelay)         -> ?TCP_OPT_NODELAY;
enc_opt(multicast_if)    -> ?UDP_OPT_MULTICAST_IF;
enc_opt(multicast_ttl)   -> ?UDP_OPT_MULTICAST_TTL;
enc_opt(multicast_loop)  -> ?UDP_OPT_MULTICAST_LOOP;
enc_opt(add_membership)  -> ?UDP_OPT_ADD_MEMBERSHIP;
enc_opt(drop_membership) -> ?UDP_OPT_DROP_MEMBERSHIP;
enc_opt(buffer)          -> ?INET_LOPT_BUFFER;
enc_opt(header)          -> ?INET_LOPT_HEADER;
enc_opt(active)          -> ?INET_LOPT_ACTIVE;
enc_opt(packet)          -> ?INET_LOPT_PACKET;
enc_opt(mode)            -> ?INET_LOPT_MODE;
enc_opt(deliver)         -> ?INET_LOPT_DELIVER;
enc_opt(exit_on_close)   -> ?INET_LOPT_EXITONCLOSE;
enc_opt(high_watermark)  -> ?INET_LOPT_TCP_HIWTRMRK;
enc_opt(low_watermark)   -> ?INET_LOPT_TCP_LOWTRMRK;
enc_opt(bit8)            -> ?INET_LOPT_BIT8;
enc_opt(send_timeout)    -> ?INET_LOPT_TCP_SEND_TIMEOUT;
enc_opt(delay_send)      -> ?INET_LOPT_TCP_DELAY_SEND;
enc_opt(packet_size)     -> ?INET_LOPT_PACKET_SIZE;
enc_opt(_) ->          -1.

dec_opt(?INET_OPT_REUSEADDR)      -> reuseaddr;
dec_opt(?INET_OPT_KEEPALIVE)      -> keepalive;
dec_opt(?INET_OPT_DONTROUTE)      -> dontroute;
dec_opt(?INET_OPT_LINGER)         -> linger;
dec_opt(?INET_OPT_BROADCAST)      -> broadcast;
dec_opt(?INET_OPT_SNDBUF)         -> sndbuf;
dec_opt(?INET_OPT_RCVBUF)         -> recbuf;
dec_opt(?INET_OPT_PRIORITY)       -> priority;
dec_opt(?INET_OPT_TOS)            -> tos;
dec_opt(?TCP_OPT_NODELAY)         -> nodelay;
dec_opt(?UDP_OPT_MULTICAST_IF)    -> multicast_if;
dec_opt(?UDP_OPT_MULTICAST_TTL)   -> multicast_ttl;
dec_opt(?UDP_OPT_MULTICAST_LOOP)  -> multicast_loop;
dec_opt(?UDP_OPT_ADD_MEMBERSHIP)  -> add_membership;
dec_opt(?UDP_OPT_DROP_MEMBERSHIP) -> drop_membership;
dec_opt(?INET_LOPT_BUFFER)        -> buffer;
dec_opt(?INET_LOPT_HEADER)        -> header;
dec_opt(?INET_LOPT_ACTIVE)        -> active;
dec_opt(?INET_LOPT_PACKET)        -> packet;
dec_opt(?INET_LOPT_MODE)          -> mode;
dec_opt(?INET_LOPT_DELIVER)       -> deliver;
dec_opt(?INET_LOPT_EXITONCLOSE)   -> exit_on_close;
dec_opt(?INET_LOPT_TCP_HIWTRMRK)  -> high_watermark;
dec_opt(?INET_LOPT_TCP_LOWTRMRK)  -> low_watermark;
dec_opt(?INET_LOPT_BIT8)          -> bit8;
dec_opt(?INET_LOPT_TCP_SEND_TIMEOUT) -> send_timeout;
dec_opt(?INET_LOPT_TCP_DELAY_SEND) -> delay_send;
dec_opt(?INET_LOPT_PACKET_SIZE)   -> packet_size;
dec_opt(_)                        -> undefined.

type_opt(reuseaddr)       -> bool;
type_opt(keepalive)       -> bool;
type_opt(dontroute)       -> bool;
type_opt(linger)          -> {bool, int};
type_opt(broadcast)       -> bool;
type_opt(sndbuf)          -> int;
type_opt(recbuf)          -> int;
type_opt(priority)        -> int;
type_opt(tos)             -> int;
type_opt(nodelay)         -> bool;
%% multicast
type_opt(multicast_ttl)   -> int;
type_opt(multicast_loop)  -> bool;
type_opt(multicast_if)    -> ip;
type_opt(add_membership)  -> {ip,ip};
type_opt(drop_membership) -> {ip,ip};
%% driver options
type_opt(header)          -> uint;
type_opt(buffer)          -> int;
type_opt(active) ->
    {enum, [{false, 0}, {true, 1}, {once, 2}]};
type_opt(packet) -> 
    {enum, [{0, ?TCP_PB_RAW},
	    {1, ?TCP_PB_1},
	    {2, ?TCP_PB_2},
	    {4, ?TCP_PB_4},
	    {raw,?TCP_PB_RAW},
	    {sunrm, ?TCP_PB_RM},
	    {asn1, ?TCP_PB_ASN1},
	    {cdr, ?TCP_PB_CDR},
	    {fcgi, ?TCP_PB_FCGI},
	    {line, ?TCP_PB_LINE_LF},
	    {tpkt, ?TCP_PB_TPKT},
	    {http, ?TCP_PB_HTTP},
	    {httph,?TCP_PB_HTTPH}]};
type_opt(mode) ->
    {enum, [{list, ?INET_MODE_LIST},
	    {binary, ?INET_MODE_BINARY}]};
type_opt(deliver) ->
    {enum, [{port, ?INET_DELIVER_PORT},
	    {term, ?INET_DELIVER_TERM}]};

type_opt(exit_on_close)   -> bool;
type_opt(low_watermark)   -> int;
type_opt(high_watermark)  -> int;
type_opt(bit8) ->
    {enum, [{clear, ?INET_BIT8_CLEAR},
	    {set,   ?INET_BIT8_SET},
	    {on,    ?INET_BIT8_ON},
	    {off,   ?INET_BIT8_OFF}]};
type_opt(send_timeout)    -> time;
type_opt(delay_send)      -> bool;
type_opt(packet_size)     -> uint;
type_opt(_)               -> undefined.



type_value(bool, true)                     -> true;
type_value(bool, false)                    -> true;
type_value(int, X) when integer(X)         -> true;
type_value(uint, X) when integer(X)        -> true;
type_value(time, infinity)                 -> true;
type_value(time, X) when integer(X), X>=0  -> true;
type_value(ip,{A,B,C,D}) when ?ip(A,B,C,D) -> true;
type_value(ether,[_X1,_X2,_X3,_X4,_X5,_X6])      -> true;
type_value({X,Y},{XV,YV}) -> type_value(X,XV) and type_value(Y,YV);
type_value({enum,List},Enum) -> 
    case enum_val(Enum, List) of
	{value,_} -> true;
	false -> false
    end;
type_value({bitenumlist,List},EnumList) -> 
    case enum_vals(EnumList, List) of
	Ls when list(Ls) -> true;
	false -> false
    end;
type_value(_, _) -> false.


enc_value(bool, true)     -> [0,0,0,1];
enc_value(bool, false)    -> [0,0,0,0];
enc_value(int, Val)       -> ?int32(Val);
enc_value(uint, Val)      -> ?int32(Val);
enc_value(time, infinity) -> ?int32(-1);
enc_value(time, Val)      -> ?int32(Val);
enc_value(ip,{A,B,C,D}) -> [A,B,C,D];
enc_value(ip,any)       -> [0,0,0,0];
enc_value(ip,loopback)  -> [127,0,0,1];
enc_value(ether,[X1,X2,X3,X4,X5,X6]) -> [X1,X2,X3,X4,X5,X6];
enc_value({enum,List},Enum) ->
    {value,Val} = enum_val(Enum, List),
    ?int32(Val);
enc_value({bitenumlist,List},EnumList) ->
    Vs = enum_vals(EnumList, List),
    Val = borlist(Vs, 0),
    ?int32(Val);
enc_value({X,Y},{XV,YV}) -> [enc_value(X,XV),enc_value(Y,YV)].


dec_value(bool, [0,0,0,0 | T]) -> {false, T};
dec_value(bool, [_,_,_,_ | T]) -> {true, T};
dec_value(int,  [X3,X2,X1,X0|T]) -> {?i32(X3,X2,X1,X0), T};
dec_value(uint, [X3,X2,X1,X0|T]) -> {?u32(X3,X2,X1,X0), T};
dec_value(time, [X3,X2,X1,X0|T]) ->
    case ?i32(X3,X2,X1,X0) of
	-1 -> {infinity, T};
	Val -> {Val, T}
    end;
dec_value(ip, [A,B,C,D|T]) -> {{A,B,C,D}, T};
dec_value(ether,[X1,X2,X3,X4,X5,X6|T]) -> {[X1,X2,X3,X4,X5,X6],T};
dec_value({enum,List}, [X3,X2,X1,X0|T]) ->
    Val = ?i32(X3,X2,X1,X0),
    case enum_name(Val, List) of
	{name, Enum} -> {Enum, T};
	_ -> {undefined, T}
    end;
dec_value({bitenumlist,List}, [X3,X2,X1,X0|T]) ->
    Val = ?i32(X3,X2,X1,X0),
    {enum_names(Val, List), T};
dec_value({X,Y}, T0) ->
    case dec_value(X, T0) of
	{XV, T1} ->
	    case dec_value(Y, T1) of
		{YV, T2} -> {{XV,YV}, T2};
		Other -> Other
	    end;
	Other -> Other
    end;
dec_value(_, B) ->
    {undefined, B}.

borlist([V|Vs], Value) ->
    borlist(Vs, V bor Value);
borlist([], Value) -> Value.
    

enum_vals([Enum|Es], List) ->
    case enum_val(Enum, List) of
	false -> false;
	{value,Value} -> [Value | enum_vals(Es, List)]
    end;
enum_vals([], _) -> [].

enum_names(Val, [{Enum,BitVal} |List]) ->
    if Val band BitVal == BitVal ->
	    [Enum | enum_names(Val, List)];
       true ->
	    enum_names(Val, List)
    end;
enum_names(_, []) -> [].
    
enum_val(Enum, [{Enum,Value}|_]) -> {value,Value};
enum_val(Enum, [_|List]) -> enum_val(Enum, List);
enum_val(_, []) -> false.

enum_name(Val, [{Enum,Val}|_]) -> {name,Enum};
enum_name(Val, [_|List]) -> enum_name(Val, List);
enum_name(_, []) -> false.

%% encode opt/val REVERSED since options are stored in reverse order
%% i.e. the recent options first (we must process old -> new)
encode_opt_val(Opts) -> enc_opt_val(Opts, []).

enc_opt_val([{Opt,Val} | Opts], Acc) ->
    Type = type_opt(Opt),
    case type_value(Type, Val) of
	true -> enc_opt_val(Opts, [enc_opt(Opt), enc_value(Type,Val) | Acc]);
	false -> {error, einval}
    end;
enc_opt_val([binary | Opts], Acc) -> enc_opt_val([{mode,binary}|Opts], Acc);
enc_opt_val([list   | Opts], Acc) -> enc_opt_val([{mode,list}|Opts], Acc);
enc_opt_val([], Acc) -> {ok,Acc}.

encode_opts(Opts) -> 
    case catch enc_opts(Opts) of
	{error, Error} -> {error, Error};
	Buf -> {ok, Buf}
    end.

enc_opts([Opt | Opts]) ->
    case enc_opt(Opt) of
	-1 -> throw({error, einval});
	B  -> [B | enc_opts(Opts)]
    end;
enc_opts([]) -> [].


decode_opt_val(Buf) ->
    case catch dec_opt_val(Buf) of
	List when list(List) -> {ok,List};
	Error -> Error
    end.

dec_opt_val([B | Buf]) ->
    case dec_opt(B) of
	undefined -> throw({error, einval});
	Opt ->
	    {Val,T} = dec_value(type_opt(Opt), Buf),
	    [{Opt,Val} | dec_opt_val(T)]
    end;
dec_opt_val(_) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% handle interface options
%%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

type_ifopt(addr)      -> ip;
type_ifopt(broadaddr) -> ip;
type_ifopt(dstaddr)   -> ip;
type_ifopt(mtu)       -> int;
type_ifopt(netmask)   -> ip;
type_ifopt(flags) ->
    {bitenumlist,
     [{up, ?INET_IFF_UP},
      {down, ?INET_IFF_DOWN},
      {broadcast, ?INET_IFF_BROADCAST},
      {no_broadcast, ?INET_IFF_NBROADCAST},
      {loopback,  ?INET_IFF_LOOPBACK},
      {pointtopoint, ?INET_IFF_POINTTOPOINT},
      {no_pointtopoint, ?INET_IFF_NPOINTTOPOINT},
      {running, ?INET_IFF_RUNNING},
      {multicast, ?INET_IFF_MULTICAST}]};
type_ifopt(hwaddr)  -> ether;
type_ifopt(_)       -> undefined.

enc_ifopt(addr)      -> ?INET_IFOPT_ADDR;
enc_ifopt(broadaddr) -> ?INET_IFOPT_BROADADDR;
enc_ifopt(dstaddr)   -> ?INET_IFOPT_DSTADDR;
enc_ifopt(mtu)       -> ?INET_IFOPT_MTU;
enc_ifopt(netmask)   -> ?INET_IFOPT_NETMASK;
enc_ifopt(flags)     -> ?INET_IFOPT_FLAGS;
enc_ifopt(hwaddr)    -> ?INET_IFOPT_HWADDR;
enc_ifopt(_) -> -1.

dec_ifopt(?INET_IFOPT_ADDR)      -> addr;
dec_ifopt(?INET_IFOPT_BROADADDR) -> broadaddr;
dec_ifopt(?INET_IFOPT_DSTADDR)   -> dstaddr;
dec_ifopt(?INET_IFOPT_MTU)       -> mtu;
dec_ifopt(?INET_IFOPT_NETMASK)   -> netmask;
dec_ifopt(?INET_IFOPT_FLAGS)     -> flags;
dec_ifopt(?INET_IFOPT_HWADDR)    -> hwaddr;
dec_ifopt(_)                     -> undefined.

%% decode if options returns a reversed list
decode_ifopts([B | Buf], Acc) ->
    case dec_ifopt(B) of
	undefined -> 
	    {error, einval};
	Opt ->
	    {Val,T} = dec_value(type_ifopt(Opt), Buf),
	    decode_ifopts(T, [{Opt,Val} | Acc])
    end;
decode_ifopts(_,Acc) -> {ok,Acc}.


%% encode if options return a reverse list
encode_ifopts([Opt|Opts], Acc) ->
    case enc_ifopt(Opt) of
	-1 -> {error, einval};
	B  -> encode_ifopts(Opts,[B|Acc])
    end;
encode_ifopts([],Acc) -> {ok,Acc}.

	    
encode_ifopt_val([{Opt,Val} | Opts], Buf) ->
    Type = type_ifopt(Opt),
    case type_value(Type, Val) of
	true -> encode_ifopt_val(Opts,
				 [Buf,enc_ifopt(Opt),enc_value(Type,Val)]);
	false -> {error, einval}
    end;
encode_ifopt_val([], Buf) -> {ok,Buf}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% handle subscribe options
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_subs(L) ->
    case catch enc_subs(L) of
	List when list(List) -> {ok, List};
	Error -> Error
    end.

enc_subs([H|T]) ->
    case H of
	subs_empty_out_q -> [?INET_SUBS_EMPTY_OUT_Q | enc_subs(T)];
	_ -> throw({error, einval})
    end;
enc_subs([]) -> [].


decode_subs(Bytes) -> 
    case catch dec_subs(Bytes) of
	List when list(List) -> {ok, List};
	Error -> Error
    end.

dec_subs([X,X3,X2,X1,X0 | R]) ->
    Val = ?u32(X3,X2,X1,X0),
    case X of
	?INET_SUBS_EMPTY_OUT_Q  -> [{subs_empty_out_q,Val} | dec_subs(R)];
	_  -> throw({error, einval})
    end;
dec_subs([]) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% handle statictics options
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode_stats(L) ->
    case catch enc_stats(L) of
	List when list(List) -> {ok, List};
	Error -> Error
    end.

enc_stats([H|T]) ->
    case H of
	recv_cnt  -> [?INET_STAT_RECV_CNT | enc_stats(T)];
	recv_max  -> [?INET_STAT_RECV_MAX | enc_stats(T)];
	recv_avg  -> [?INET_STAT_RECV_AVG | enc_stats(T)];
	recv_dvi  -> [?INET_STAT_RECV_DVI | enc_stats(T)];
	send_cnt  -> [?INET_STAT_SEND_CNT | enc_stats(T)];
	send_max  -> [?INET_STAT_SEND_MAX | enc_stats(T)];
	send_avg  -> [?INET_STAT_SEND_AVG  | enc_stats(T)];
	send_pend -> [?INET_STAT_SEND_PEND | enc_stats(T)];
	send_oct  -> [?INET_STAT_SEND_OCT  | enc_stats(T)];
	recv_oct  -> [?INET_STAT_RECV_OCT  | enc_stats(T)];
	_ -> throw({error, einval})
    end;
enc_stats([]) -> [].


decode_stats(Bytes) -> 
    case catch dec_stats(Bytes) of
	List when list(List) -> {ok, List};
	Error -> Error
    end.


dec_stats([?INET_STAT_SEND_OCT,X7,X6,X5,X4,X3,X2,X1,X0 | R]) ->
    Val = ?u64(X7,X6,X5,X4,X3,X2,X1,X0),
    [{send_oct, Val} | dec_stats(R)];
dec_stats([?INET_STAT_RECV_OCT,X7,X6,X5,X4,X3,X2,X1,X0 | R]) ->
    Val = ?u64(X7,X6,X5,X4,X3,X2,X1,X0),
    [{recv_oct, Val} | dec_stats(R)];
dec_stats([X,X3,X2,X1,X0 | R]) ->
    Val = ?u32(X3,X2,X1,X0),
    case X of
	?INET_STAT_RECV_CNT  -> [{recv_cnt,Val} | dec_stats(R)];
	?INET_STAT_RECV_MAX  -> [{recv_max,Val} | dec_stats(R)];
	?INET_STAT_RECV_AVG  -> [{recv_avg,Val} | dec_stats(R)];
	?INET_STAT_RECV_DVI  -> [{recv_dvi,Val} | dec_stats(R)];
	?INET_STAT_SEND_CNT  -> [{send_cnt,Val} | dec_stats(R)];
	?INET_STAT_SEND_MAX  -> [{send_max,Val} | dec_stats(R)];
	?INET_STAT_SEND_AVG  -> [{send_avg,Val} | dec_stats(R)];
	?INET_STAT_SEND_PEND -> [{send_pend,Val} | dec_stats(R)];
	_  -> throw({error, einval})
    end;
dec_stats([]) -> [].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% handle status options
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dec_status(Flags) ->
    enum_names(Flags,
	       [
		{busy, ?INET_F_BUSY},
		{listening, ?INET_F_LST},
		{accepting, ?INET_F_ACC},
		{connecting, ?INET_F_CON},
		{listen, ?INET_F_LISTEN},
		{connected, ?INET_F_ACTIVE},
		{bound, ?INET_F_BOUND},
		{open, ?INET_F_OPEN}
	       ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% UTILS
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

enc_time(Time) when Time < 0 -> [255,255,255,255];
enc_time(Time) -> ?int32(Time).

encode_ifname(Name) when atom(Name) -> encode_ifname(atom_to_list(Name));
encode_ifname(Name) ->
    N = length(Name),
    if N > 255 -> {error, einval};
       true -> {ok,[N | Name]}
    end.
    
build_iflist(Cs) ->
    build_iflist(Cs, [], []).

%% Turn a NULL separated list of chars into a list of strings, removing
%% duplicates.
build_iflist([0|L], Acc, [H|T]) ->
    case rev(Acc) of
	H -> build_iflist(L, [], [H|T]);
	N -> build_iflist(L, [], [N,H|T])
    end;
build_iflist([0|L], Acc, []) ->
    build_iflist(L, [], [rev(Acc)]);
build_iflist([C|L], Acc, List) ->
    build_iflist(L, [C|Acc], List);
build_iflist([], [], List) ->
    rev(List);
build_iflist([], Acc, List) ->
    build_iflist([0], Acc, List).

rev(L) -> rev(L,[]).
rev([C|L],Acc) -> rev(L,[C|Acc]);
rev([],Acc) -> Acc.

ip_to_bytes(IP) when size(IP) == 4 -> ip4_to_bytes(IP);
ip_to_bytes(IP) when size(IP) == 8 -> ip6_to_bytes(IP).


ip4_to_bytes({A,B,C,D}) ->
    [A band 16#ff, B band 16#ff, C band 16#ff, D band 16#ff].

ip6_to_bytes({A,B,C,D,E,F,G,H}) ->
    [?int16(A), ?int16(B), ?int16(C), ?int16(D),
     ?int16(E), ?int16(F), ?int16(G), ?int16(H)].

get_ip(?INET_AF_INET, Addr)  -> get_ip4(Addr);
get_ip(?INET_AF_INET6, Addr) -> get_ip6(Addr).

get_ip4([A,B,C,D | T]) -> {{A,B,C,D},T}.

get_ip6([X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16 | T]) ->
    { { ?u16(X1,X2),?u16(X3,X4),?u16(X5,X6),?u16(X7,X8),
	?u16(X9,X10),?u16(X11,X12),?u16(X13,X14),?u16(X15,X16)}, T}.


%% Control command
ctl_cmd(Port, Cmd, Args) ->
    case catch erlang:port_control(Port, Cmd, Args) of
	[?INET_REP_OK | Reply]      -> {ok, Reply};
	[?INET_REP_ERROR| Err] -> {error, list_to_atom(Err)};
	{'EXIT', _} -> {error, einval};
	_ -> {error, internal}
    end.
