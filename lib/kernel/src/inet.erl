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
-module(inet).

-include_lib("kernel/include/inet.hrl").
-include("inet_int.hrl").

%% socket
-export([peername/1, sockname/1, port/1, send/2,
	 setopts/2, getopts/2, 
	 getif/1, getif/0, getiflist/0, getiflist/1,
	 ifget/3, ifget/2, ifset/3, ifset/2,
	 getstat/1, getstat/2,
	 ip/1, stats/0, options/0, 
	 pushf/3, popf/1, close/1, gethostname/0, gethostname/1]).

-export([connect_options/2, listen_options/2, udp_options/2]).

-export([i/0, i/1, i/2]).

-export([getll/1, getfd/1, open/7, fdopen/5]).

-export([tcp_controlling_process/2, udp_controlling_process/2,
	 tcp_close/1, udp_close/1]).
%% used by socks5
-export([setsockname/2, setpeername/2]).

%% resolve
-export([gethostbyname/1, gethostbyname/2, gethostbyname/3, 
	 gethostbyname_tm/3]).
-export([gethostbyaddr/1, gethostbyaddr/2, 
	 gethostbyaddr_tm/2]).

-export([getservbyname/2, getservbyport/2]).
-export([getaddrs/2, getaddrs/3, getaddrs_tm/3,
	 getaddr/2, getaddr/3, getaddr_tm/3]).

-export([bytes_to_ip6/16, ip6_to_bytes/1, ip4_to_bytes/1, ip_to_bytes/1]).

%% format error
-export([format_error/1]).

%% timer interface
-export([start_timer/1, timeout/1, timeout/2, stop_timer/1]).


%% imports
-import(lists, [append/1, duplicate/2, member/2, filter/2,
		map/2, foldl/3, foreach/2]).

%%
close(Socket) ->
    prim_inet:close(Socket).

peername(Socket) -> prim_inet:peername(Socket).


setpeername(Socket, {IP,Port}) ->
    prim_inet:setpeername(Socket, {IP,Port});
setpeername(Socket, undefined) ->
    prim_inet:setpeername(Socket, undefined).

sockname(Socket) -> prim_inet:sockname(Socket).

setsockname(Socket, {IP,Port}) -> 
    prim_inet:setsockname(Socket, {IP,Port});
setsockname(Socket, undefined) ->
    prim_inet:setsockname(Socket, undefined).

port(Socket) ->
    case prim_inet:sockname(Socket) of
	{ok, {_,Port}} -> {ok, Port};
	Error -> Error
    end.

send(Socket, Packet) -> 
    prim_inet:send(Socket, Packet).
    
setopts(Socket, Opts) -> 
    prim_inet:setopts(Socket, Opts).

getopts(Socket, Opts) ->
    prim_inet:getopts(Socket, Opts).


getiflist(Socket) -> prim_inet:getiflist(Socket).
getiflist() -> withsocket(fun(S) -> prim_inet:getiflist(S) end).
    
ifget(Socket, Name, Opts) -> prim_inet:ifget(Socket, Name, Opts).
ifget(Name, Opts) -> withsocket(fun(S) -> prim_inet:ifget(S, Name, Opts) end).

ifset(Socket, Name, Opts) -> prim_inet:ifset(Socket, Name, Opts).
ifset(Name, Opts) -> withsocket(fun(S) -> prim_inet:ifset(S, Name, Opts) end).


getif() -> withsocket(fun(S) -> getif(S) end).

%% backwards compatible getif
getif(Socket) ->
    case prim_inet:getiflist(Socket) of
	{ok, IfList} ->
	    {ok, lists:foldl(
		   fun(Name,Acc) ->
			   case prim_inet:ifget(Socket,Name,
						[addr,broadaddr,netmask]) of
			       {ok,[{addr,A},{broadaddr,B},{netmask,M}]} ->
				   [{A,B,M}|Acc];
			       %% Some interfaces does not have a b-addr
			       {ok,[{addr,A},{netmask,M}]} ->
				   [{A,undefined,M}|Acc];
			       _ ->
				   Acc
			   end
		   end, [], IfList)};
	Error -> Error
    end.


withsocket(Fun) ->
    case inet_udp:open(0,[]) of
	{ok,Socket} ->
	    Res = Fun(Socket),
	    inet_udp:close(Socket),
	    Res;
	Error ->
	    Error
    end.
    

pushf(Socket, Fun, State) when function(Fun) ->
    {error, einval}.

popf(Socket) ->
    {error, einval}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% the hostname is not cached any more because this
% could cause troubles on at least windows with plug-and-play
% and network-cards inserted and removed in conjunction with
% use of the DHCP-protocol
% should never fail
gethostname() ->
    case inet_udp:open(0,[]) of
	{ok,U} ->
	    {ok,Res} = gethostname(U),
	    inet_udp:close(U),
	    {Res2,_} = lists:splitwith(fun($.)->false;(_)->true end,Res),
	    {ok, Res2};
	_ ->
	    {ok, "nohost.nodomain"}
    end.

gethostname(Socket) ->
    prim_inet:gethostname(Socket).

getstat(Socket) ->
    prim_inet:getstat(Socket, stats()).

getstat(Socket,What) ->
    prim_inet:getstat(Socket, What).


gethostbyname(Name) -> gethostbyname_tm(Name, inet, false).

gethostbyname(Name,Family) -> gethostbyname_tm(Name, Family, false).

gethostbyname(Name,Family,Timeout) ->
    Timer = start_timer(Timeout),
    Res = gethostbyname_tm(Name,Family,Timer),
    stop_timer(Timer),
    Res.

gethostbyname_tm(Name,Family,Timer) ->
    gethostbyname_tm(Name,Family,Timer,inet_db:res_option(lookup)).


gethostbyaddr(Address) ->
    gethostbyaddr_tm(Address, false).

gethostbyaddr(Address,Timeout) ->
    Timer = start_timer(Timeout),    
    Res = gethostbyaddr_tm(Address, Timer),
    stop_timer(Timer),
    Res.

gethostbyaddr_tm(Address,Timer) ->
    gethostbyaddr_tm(Address, Timer, inet_db:res_option(lookup)).

ip({A,B,C,D}) when ?ip(A,B,C,D) ->
    {ok, {A,B,C,D}};
ip(Name) ->
    case gethostbyname(Name) of
	{ok, Ent} ->
	    {ok, hd(Ent#hostent.h_addr_list)};
	Error -> Error
    end.

%% This function returns the erlang port used (with inet_drv)
%% Return values: {ok,#Port} if ok
%%                {error, einval} if not applicable
getll(Socket) when port(Socket) ->
    {ok, Socket}.

%%
%% Return the internal file descriptor number
%%
getfd(Socket) ->
    prim_inet:getfd(Socket).

%%
%% Lookup an ip address
%%
getaddr(Address, Family) ->
    getaddr(Address, Family, infinity).

getaddr(Address, Family, Timeout) ->
    Timer = start_timer(Timeout),
    Res = getaddr_tm(Address, Family, Timer),
    stop_timer(Timer),
    Res.
    
getaddr_tm(Address, Family, Timer) ->
    case getaddrs_tm(Address, Family, Timer) of
	{ok, [IP|_]} -> {ok, IP};
	Error -> Error
    end.

getaddrs(Address, Family) -> 
    getaddrs(Address, Family, infinity).

getaddrs(Address, Family,Timeout) -> 
    Timer = start_timer(Timeout),    
    Res = getaddrs_tm(Address, Family, Timer),
    stop_timer(Timer),
    Res.    


getservbyport(Port, Proto) ->
    case inet_udp:open(0, []) of
	{ok,U} ->
	    Res = prim_inet:getservbyport(U,Port, Proto),
	    inet_udp:close(U),
	    Res;
	Error -> Error
    end.

getservbyname(Name, Proto) when atom(Name) ->
    case inet_udp:open(0, []) of
	{ok,U} ->
	    Res = prim_inet:getservbyname(U,Name, Proto),
	    inet_udp:close(U),
	    Res;
	Error -> Error
    end.

%% Return a list of available options
options() ->
    [
     reuseaddr, keepalive, dontroute, linger,
     broadcast, sndbuf, recbuf, nodelay,
     buffer, header, active, packet, deliver, mode,
     multicast_if, multicast_ttl, multicast_loop,
     exit_on_close, high_watermark, low_watermark,
     bit8, send_timeout
    ].

%% Return a list of statistics options
stats() ->
    [recv_oct, recv_cnt, recv_max, recv_avg, recv_dvi,
     send_oct, send_cnt, send_max, send_avg, send_pend].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Available options for tcp:connect
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
connect_options() ->
    [reuseaddr, keepalive, linger, sndbuf, recbuf, nodelay,
     header, active, packet, buffer, mode, deliver,
     exit_on_close, high_watermark, low_watermark, bit8, send_timeout].
    
connect_options(Opts, Family) ->
    case con_opt(Opts, #connect_opts { }, connect_options()) of
	{ok, R} ->
	    {ok, R#connect_opts {
		   ifaddr = translate_ip(R#connect_opts.ifaddr, Family)
		  }};
	Error -> Error	    
    end.

con_opt([Opt | Opts], R, As) ->
    case Opt of
	{ip,IP}     -> con_opt(Opts, R#connect_opts { ifaddr = IP }, As);
	{ifaddr,IP} -> con_opt(Opts, R#connect_opts { ifaddr = IP }, As);
	{port,P}    -> con_opt(Opts, R#connect_opts { port = P }, As);
	{fd,Fd}     -> con_opt(Opts, R#connect_opts { fd = Fd }, As);
	binary      -> con_add(mode, binary, R, Opts, As);
	list        -> con_add(mode, list, R, Opts, As);
	{tcp_module,_}  -> con_opt(Opts, R, As);
	{Name,Val} when atom(Name) -> con_add(Name, Val, R, Opts, As);
	_ -> {error, badarg}
    end;
con_opt([], R, _) ->
    {ok, R}.

con_add(Name, Val, R, Opts, AllOpts) ->
    case add_opt(Name, Val, R#connect_opts.opts, AllOpts) of
	{ok, SOpts} ->
	    con_opt(Opts, R#connect_opts { opts = SOpts }, AllOpts);
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Available options for tcp:listen
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
listen_options() ->
    [reuseaddr, keepalive, linger, sndbuf, recbuf, nodelay,
     header, active, packet, buffer, mode, deliver, backlog,
     exit_on_close, high_watermark, low_watermark, bit8, send_timeout].

listen_options(Opts, Family) ->
    case list_opt(Opts, #listen_opts { }, listen_options()) of
	{ok, R} ->
	    {ok, R#listen_opts {
		   ifaddr = translate_ip(R#listen_opts.ifaddr, Family)
		  }};
	Error -> Error
    end.
	
list_opt([Opt | Opts], R, As) ->
    case Opt of
	{ip,IP}      ->  list_opt(Opts, R#listen_opts { ifaddr = IP }, As);
	{ifaddr,IP}  ->  list_opt(Opts, R#listen_opts { ifaddr = IP }, As);
	{port,P}     ->  list_opt(Opts, R#listen_opts { port = P }, As);
	{fd,Fd}      ->  list_opt(Opts, R#listen_opts { fd = Fd }, As);
	{backlog,BL} ->  list_opt(Opts, R#listen_opts { backlog = BL }, As);
	binary       ->  list_add(mode, binary, R, Opts, As);
	list         ->  list_add(mode, list, R, Opts, As);
	{tcp_module,_}  -> list_opt(Opts, R, As);
	{Name,Val} when atom(Name) -> list_add(Name, Val, R, Opts, As);
	_ -> {error, badarg}
    end;
list_opt([], R, SockOpts) ->
    {ok, R}.

list_add(Name, Val, R, Opts, As) ->
    case add_opt(Name, Val, R#listen_opts.opts, As) of
	{ok, SOpts} ->
	    list_opt(Opts, R#listen_opts { opts = SOpts }, As);
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Available options for udp:open
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

udp_options() ->
    [reuseaddr, sndbuf, recbuf, header, active, buffer, mode, deliver,
     broadcast, dontroute, multicast_if, multicast_ttl, multicast_loop].


udp_options(Opts, Family) ->
    case udp_opt(Opts, #udp_opts { }, udp_options()) of
	{ok, R} ->
	    {ok, R#udp_opts {
		   ifaddr = translate_ip(R#udp_opts.ifaddr, Family)
		  }};
	Error -> Error
    end.

udp_opt([Opt | Opts], R, As) ->
    case Opt of
	{ip,IP}     ->  udp_opt(Opts, R#udp_opts { ifaddr = IP }, As);
	{ifaddr,IP} ->  udp_opt(Opts, R#udp_opts { ifaddr = IP }, As);
	{port,P}    ->  udp_opt(Opts, R#udp_opts { port = P }, As);
	{fd,Fd}     ->  udp_opt(Opts, R#udp_opts { fd = Fd }, As);
	binary      ->  udp_add(mode, binary, R, Opts, As);
	list        ->  udp_add(mode, list, R, Opts, As);
	{udp_module,_} -> udp_opt(Opts, R, As);
	{Name,Val} when atom(Name) -> udp_add(Name, Val, R, Opts, As);
	_ -> {error, badarg}
    end;
udp_opt([], R, SockOpts) ->
    {ok, R}.

udp_add(Name, Val, R, Opts, As) ->
    case add_opt(Name, Val, R#udp_opts.opts, As) of
	{ok, SOpts} ->
	    udp_opt(Opts, R#udp_opts { opts = SOpts }, As);
	Error -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Util to check and insert option in option list
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

add_opt(Name, Val, Opts, As) ->
    case member(Name, As) of
	true ->
	    case prim_inet:is_sockopt_val(Name, Val) of
		true ->
		    Opts1 = lists:keydelete(Name, 1, Opts),
		    {ok, [{Name,Val} | Opts1]};
		false -> {error, badarg}
	    end;
	false -> {error, badarg}
    end.
	

translate_ip(any,      inet) -> {0,0,0,0};
translate_ip(loopback, inet) -> {127,0,0,1};
translate_ip(any,      inet6) -> {0,0,0,0,0,0,0,0};
translate_ip(loopback, inet6) -> {0,0,0,0,0,0,0,1};
translate_ip(IP, _) -> IP.


getaddrs_tm({A,B,C,D} = IP, inet, _) ->
    if 
	?ip(A,B,C,D) -> {ok, [IP]};
	true -> {error,einval}
    end;
getaddrs_tm({A,B,C,D,E,F,G,H} = IP, inet6, _) ->
    if 
	?ip6(A,B,C,D,E,F,G,H) -> {ok, [IP]};
	true -> {error,einval}
    end;
getaddrs_tm(Address, Family, Timer) when atom(Address) ->
    getaddrs_tm(atom_to_list(Address), Family, Timer);
getaddrs_tm(Address, Family, Timer) ->
    Result = case inet_parse:visible_string(Address) of
		 true when Family == inet ->
		     inet_parse:ipv4_address(Address);
		 true when Family == inet6 -> 
		     inet_parse:ipv6_address(Address);
		 false ->
		     false
	     end,
    case Result of
	{ok,IP} -> {ok,[IP]};
	false -> {error, einval};
	_ -> 
	    case gethostbyname_tm(Address,Family,Timer) of
		{ok, Ent} -> {ok, Ent#hostent.h_addr_list};
		Error -> Error
	    end
    end;
getaddrs_tm(_,_,_) -> {error, einval}.


%%
%% gethostbyname with option search
%%
gethostbyname_tm(Name, Type, Timer, [dns | Opts]) ->
    Res = inet_res:gethostbyname_tm(Name, Type, Timer),
    case Res of
	{ok,_} -> Res;
	{error,timeout} -> Res;
	{error,formerr} -> {error,einval};
	{error,_} -> gethostbyname_tm(Name,Type,Timer,Opts)
    end;
gethostbyname_tm(Name, Type, Timer, [file | Opts]) ->
    case inet_hosts:gethostbyname(Name, Type) of
	{error,formerr} -> {error,einval};
	{error,_} -> gethostbyname_tm(Name,Type,Timer,Opts);
	Result -> Result
    end;
gethostbyname_tm(Name, Type, Timer, [yp | Opts]) ->
    gethostbyname_tm(Name, Type, Timer, [native|Opts]);
gethostbyname_tm(Name, Type, Timer, [nis | Opts]) ->
    gethostbyname_tm(Name, Type, Timer, [native|Opts]);
gethostbyname_tm(Name, Type, Timer, [nisplus | Opts]) ->
    gethostbyname_tm(Name, Type, Timer, [native|Opts]);
gethostbyname_tm(Name, Type, Timer, [wins | Opts]) ->
    gethostbyname_tm(Name, Type, Timer, [native|Opts]);
gethostbyname_tm(Name, Type, Timer, [native | Opts]) ->
    %% Fixme: add (global) timeout to gethost_native
    case inet_gethost_native:gethostbyname(Name, Type) of
	{error,formerr} -> {error, einval};
	{error,_} -> gethostbyname_tm(Name,Type,Timer,Opts);
	Result -> Result
    end;
gethostbyname_tm(Name, Type, Timer, [_ | Opts]) ->
    gethostbyname_tm(Name, Type, Timer, Opts);
gethostbyname_tm(Name, Type, Timer, []) ->
    case inet_parse:ipv4_address(Name) of
	{ok, IP4} ->
	    {ok, 
	     #hostent{
	       h_name = Name,
	       h_aliases = [],
	       h_addrtype = inet,
	       h_length = 4,
	       h_addr_list = [IP4]}};
	_ ->
	    case inet_parse:ipv6_address(Name) of
		{ok, IP6} ->
		    {ok, 
		     #hostent{
		       h_name = Name,
		       h_aliases = [],
		       h_addrtype = inet6,
		       h_length = 16,
		       h_addr_list = [IP6]}};
		_ ->
		    {error, nxdomain}
	    end
    end.

%%
%% gethostbyaddr with option search
%%
gethostbyaddr_tm(Addr, Timer, [dns | Opts]) ->
    Res = inet_res:gethostbyaddr_tm(Addr,Timer),
    case Res of
	{ok,_} -> Res;
	{error,timeout} -> Res;
	{error,formerr} -> {error, einval};
	{error,_} -> gethostbyaddr_tm(Addr,Timer,Opts)
    end;    
gethostbyaddr_tm(Addr, Timer, [file | Opts]) ->
    case inet_hosts:gethostbyaddr(Addr) of
	{error,formerr} -> {error, einval};
	{error,_} -> gethostbyaddr_tm(Addr,Timer,Opts);
	Result -> Result
    end;    
gethostbyaddr_tm(Addr, Timer, [yp | Opts]) ->
    gethostbyaddr_tm(Addr, Timer, [native | Opts]);
gethostbyaddr_tm(Addr, Timer, [nis | Opts]) ->
    gethostbyaddr_tm(Addr, Timer, [native | Opts]);
gethostbyaddr_tm(Addr, Timer,  [nisplus | Opts]) ->
    gethostbyaddr_tm(Addr, Timer, [native | Opts]);
gethostbyaddr_tm(Addr, Timer, [wins | Opts]) ->
    gethostbyaddr_tm(Addr, Timer, [native | Opts]);
gethostbyaddr_tm(Addr, Timer, [native | Opts]) ->
    %% Fixme: user timer for timeoutvalue
    case inet_gethost_native:gethostbyaddr(Addr) of
	{error,formerr} -> {error, einval};
	{error,_} -> gethostbyaddr_tm(Addr,Timer,Opts);
	Result -> Result
    end;    
gethostbyaddr_tm(Addr, Timer, [_ | Opts]) ->
    gethostbyaddr_tm(Addr, Timer, Opts);
gethostbyaddr_tm(Addr, Timer, []) ->
    {error, nxdomain}.


open(Fd, Addr, Port, Opts, Type, Family, Module) when Fd < 0 ->
    case prim_inet:open(Type,Family) of
	{ok,S} ->
	    case prim_inet:setopts(S, Opts) of
		ok ->
		    case prim_inet:bind(S, Addr, Port) of
			{ok, _} -> 
			    inet_db:register_socket(S, Module),
			    {ok,S};
			Error  -> prim_inet:close(S), Error
		    end;
		Error  -> prim_inet:close(S), Error
	    end;
	Error -> Error
    end;
open(Fd, _Addr, _Port, Opts, Type, Family, Module) ->
    fdopen(Fd, Opts, Type, Family, Module).


fdopen(Fd, Opts, Type, Family, Module) ->
    case prim_inet:fdopen(Type, Fd, Family) of
	{ok, S} ->
	    case prim_inet:setopts(S, Opts) of
		ok ->
		    inet_db:register_socket(S, Module),
		    {ok, S};
		Error ->
		    prim_inet:close(S), Error
	    end;
	Error -> Error
    end.
    


ip_to_bytes(IP) when size(IP) == 4 -> ip4_to_bytes(IP);
ip_to_bytes(IP) when size(IP) == 8 -> ip6_to_bytes(IP).


ip4_to_bytes({A,B,C,D}) ->
    [A band 16#ff, B band 16#ff, C band 16#ff, D band 16#ff].

ip6_to_bytes({A,B,C,D,E,F,G,H}) ->
    [?int16(A), ?int16(B), ?int16(C), ?int16(D),
     ?int16(E), ?int16(F), ?int16(G), ?int16(H)].

bytes_to_ip6(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16) ->
    { ?u16(X1,X2),?u16(X3,X4),?u16(X5,X6),?u16(X7,X8),
     ?u16(X9,X10),?u16(X11,X12),?u16(X13,X14),?u16(X15,X16)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  socket stat
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

i() -> i(tcp), i(udp).

i(Proto) -> i(Proto, [port, module, recv, sent, owner,
		      local_address, foreign_address, state]).

i(tcp, Fs) ->
    ii(tcp_sockets(), Fs, tcp);
i(udp, Fs) ->
    ii(udp_sockets(), Fs, udp).

ii(Ss, Fs, Proto) ->
    LLs = [h_line(Fs) | info_lines(Ss, Fs, Proto)],
    Maxs = foldl(
	     fun(Line,Max0) -> smax(Max0,Line) end, 
	     duplicate(length(Fs),0),LLs),
    Fmt = append(map(fun(N) -> "~-" ++ integer_to_list(N) ++ "s " end,
		     Maxs)) ++ "\n",
    foreach(
      fun(Line) -> io:format(Fmt, Line) end, LLs).

smax([Max|Ms], [Str|Strs]) ->
    N = length(Str),
    [ if N > Max -> N; true -> Max end | smax(Ms, Strs)];
smax([], []) -> [].

info_lines(Ss, Fs,Proto)  -> map(fun(S) -> i_line(S, Fs,Proto) end, Ss).
i_line(S, Fs, Proto)      -> map(fun(F) -> info(S, F, Proto) end, Fs).

h_line(Fs) -> map(fun(F) -> h_field(atom_to_list(F)) end, Fs).

h_field([C|Cs]) -> [upper(C) | hh_field(Cs)].

hh_field([$_,C|Cs]) -> [$\s,upper(C) | hh_field(Cs)];
hh_field([C|Cs]) -> [C|hh_field(Cs)];
hh_field([]) -> [].

upper(C) when C >= $a, C =< $z -> (C-$a) + $A;
upper(C) -> C.

    
info(S, F, Proto) ->
    case F of
	owner ->
	    case erlang:port_info(S, connected) of
		{connected, Owner} -> pid_to_list(Owner);
		_ -> " "
	    end;
	port ->
	    case erlang:port_info(S,id) of
		{id, Id}  -> integer_to_list(Id);
		undefined -> " "
	    end;
	sent ->
	    case prim_inet:getstat(S, [send_oct]) of
		{ok,[{send_oct,N}]} -> integer_to_list(N);
		_ -> " "
	    end;
	recv ->
	    case  prim_inet:getstat(S, [recv_oct]) of
		{ok,[{recv_oct,N}]} -> integer_to_list(N);
		_ -> " "
	    end;
	local_address ->
	    fmt_addr(prim_inet:sockname(S), Proto);
	foreign_address ->
	    fmt_addr(prim_inet:peername(S), Proto);
	state ->
	    case prim_inet:getstatus(S) of
		{ok,Status} -> fmt_status(Status);
		_ -> " "
	    end;
	packet ->
	    case prim_inet:getopt(S, packet) of
		{ok,Type} when atom(Type) -> atom_to_list(Type);
		{ok,Type} when integer(Type) -> integer_to_list(Type);
		_ -> " "
	    end;
	type ->
	    case prim_inet:gettype(S) of
		{ok,{_,stream}} -> "STREAM";
		{ok,{_,dgram}}  -> "DGRAM";
		_ -> " "
	    end;
	fd ->
	    case prim_inet:getfd(S) of
		{ok, Fd} -> integer_to_list(Fd);
		_ -> " "
	    end;
	module ->
	    case inet_db:lookup_socket(S) of
		{ok,Mod} -> atom_to_list(Mod);
		_ -> "prim_inet"
	    end
    end.
%% Possible flags: (sorted)
%% [accepting,bound,busy,connected,connecting,listen,listening,open]
%%
fmt_status(Flags) ->
    case lists:sort(Flags) of
	[accepting | _]               -> "ACCEPTING";
	[bound,busy,connected|_]      -> "CONNECTED*";
	[bound,connected|_]           -> "CONNECTED";
	[bound,listen,listening | _]  -> "LISTENING";
	[bound,listen | _]            -> "LISTEN";
	[bound,connecting | _]        -> "CONNECTING";
	[bound,open]                  -> "BOUND";
	[open]                        -> "IDLE";
	[]                            -> "CLOSED";
	_                             -> "????"
    end.

fmt_addr({error,enotconn}, _) -> "*:*";
fmt_addr({error,_}, _)        -> " ";
fmt_addr({ok,Addr}, Proto) ->
    case Addr of
	{0,0}            -> "*:*";
	{{0,0,0,0},Port} -> "*:" ++ fmt_port(Port, Proto);
	{{0,0,0,0,0,0,0,0},Port} -> "*:" ++ fmt_port(Port, Proto);
	{{127,0,0,1},Port} -> "localhost:" ++ fmt_port(Port, Proto);
	{{0,0,0,0,0,0,0,1},Port} -> "localhost:" ++ fmt_port(Port, Proto);
	{IP,Port} -> inet_parse:ntoa(IP) ++ ":" ++ fmt_port(Port, Proto)
    end.

fmt_port(N, Proto) ->
    case inet:getservbyport(N, Proto) of
	{ok, Name} -> Name;
	_ -> integer_to_list(N)
    end.

%% Return a list of all tcp sockets
tcp_sockets() -> port_list("tcp_inet").
udp_sockets() -> port_list("udp_inet").

%% Return all port having the name 'Name'
port_list(Name) ->
    filter(
      fun(Port) ->
	      case erlang:port_info(Port, name) of
		  {name, Name} -> true;
		  _ -> false
	      end
      end, erlang:ports()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_error(exbadport) -> "invalid port state";
format_error(exbadseq) ->  "bad command sequence";
format_error(Tag) ->
    erl_posix_msg:message(Tag).

%% Close a TCP socket.
tcp_close(S) when port(S) ->
    receive
	{tcp_closed, S} -> 
	    %% if exit_on_close is set we must force a close anyway!!!
	    prim_inet:close(S),
	    ok
    after 0 ->
	    prim_inet:close(S)
    end.

%% Close a UDP socket.
udp_close(S) when port(S) ->
    receive 
	{udp_closed, S} -> ok
    after 0 ->
	    prim_inet:close(S)
    end.

%% Set controlling process for TCP socket.
tcp_controlling_process(S, NewOwner) when port(S), pid(NewOwner) ->
    case erlang:port_info(S, connected) of
	{connected, Pid} when Pid /= self() ->
	    {error, not_owner};
	_ ->
	    {ok,A0} = prim_inet:getopt(S, active),
	    prim_inet:setopt(S, active, false),
	    case tcp_sync_input(S, NewOwner, false) of
		true ->
		    %%  %% socket already closed, 
		    ok;
		false ->
		    case catch erlang:port_connect(S, NewOwner) of
			true -> 
			    unlink(S), %% unlink from port
			    prim_inet:setopt(S, active, A0),
			    ok;
			{'EXIT', Reason} -> 
			    {error, Reason}
		    end
	    end
    end.

tcp_sync_input(S, Owner, Flag) ->
    receive
	{tcp, S, Data} ->
	    Owner ! {tcp, S, Data},
	    tcp_sync_input(S, Owner, Flag);
	{tcp_closed, S} ->
	    Owner ! {tcp_closed, S},
	    tcp_sync_input(S, Owner, true);
	{S, {data, Data}} ->
	    Owner ! {S, {data, Data}},
	    tcp_sync_input(S, Owner, Flag);	    
	{inet_async, S, Ref, Status} ->
	    Owner ! {inet_async, S, Ref, Status},
	    tcp_sync_input(S, Owner, Flag);
	{inet_reply, S, Status} ->
	    Owner ! {inet_reply, S, Status},
	    tcp_sync_input(S, Owner, Flag)
    after 0 -> 
	    Flag
    end.

%% Set controlling process for UDP socket.
udp_controlling_process(S, NewOwner) when port(S), pid(NewOwner) ->
    case erlang:port_info(S, connected) of
	{connected, Pid} when Pid /= self() ->
	    {error, not_owner};
	_ ->
	    {ok,A0} = prim_inet:getopt(S, active),
	    prim_inet:setopt(S, active, false),
	    case udp_sync_input(S, NewOwner, false) of
		false ->
		    case catch erlang:port_connect(S, NewOwner) of
			true -> 
			    unlink(S),
			    prim_inet:setopt(S, active, A0),
			    ok;
			{'EXIT', Reason} -> 
			    {error, Reason}
		    end;
		true ->
		    ok
	    end
    end.

udp_sync_input(S, Owner, Flag) ->
    receive
	{udp, S, IP, UP, Data} ->
	    Owner ! {udp, S, IP, UP, Data},
	    udp_sync_input(S, Owner, Flag);
	{udp_closed, S} ->
	    Owner ! {udp_closed, S},
	    udp_sync_input(S, Owner, true);
	{S, {data, Data}} ->
	    Owner ! {S, {data, Data}},
	    udp_sync_input(S, Owner, Flag);
	{inet_async, S, Ref, Status} ->
	    Owner ! {inet_async, S, Ref, Status},
	    udp_sync_input(S, Owner, Flag);
	{inet_reply, S, Status} ->
	    Owner ! {inet_reply, S, Status},
	    udp_sync_input(S, Owner, Flag)
    after 0 -> 
	    Flag
    end.

start_timer(infinity) -> false;
start_timer(Timeout) -> 
    erlang:start_timer(Timeout, self(), inet).

timeout(false) -> infinity;
timeout(Timer) ->
    case erlang:read_timer(Timer) of
	false -> 0;
	Time  -> Time
    end.

timeout(Time, false) -> Time;
timeout(Time, Timer) ->
    TimerTime = timeout(Timer),
    if TimerTime < Time -> TimerTime;
       true -> Time
    end.
    

stop_timer(false) -> false;
stop_timer(Timer) ->
    case erlang:cancel_timer(Timer) of
	false ->
	    receive
		{timeout,Timer,_} -> false
	    after 0 ->
		    false
	    end;
	T -> T
    end.
