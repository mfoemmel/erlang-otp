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

%% General udp/tcp queries

-include("inet.hrl").
-include("inet_int.hrl").

%% socket
-export([peername/1, sockname/1, name/1, port/1, send/2,
	 setopts/2, getopts/2, getif/1, getstat/1, getstat/2,
	 ip/1, stats/0, options/0, 
	 socket_options/1, socket_options/2,
	 pushf/3, popf/1, close/1, gethostname/0, gethostname/1]).

%% used by socks5
-export([setname/2, setpeername/2]).

%% low level inet_drv
-export([getll/1]).
-export([ll_open_set_bind/3, ll_open_accept_set/3, ll_accept/3]).
-export([ll_open/3, ll_fdopen/4, ll_close/1, ll_bind/3, ll_connect/3]).
-export([ll_setopts/2, ll_getopts/2, ll_name/1, ll_peer/1, ll_index/1]).
-export([handle_call/3]).

%% resolve
-export([gethostbyname/1, gethostbyname/2]).
-export([gethostbyaddr/1]).
-export([getservbyname/2, getservbyport/2]).
-export([getaddr/2]).

-export([bytes_to_ip6/16, ip6_to_bytes/1, ip4_to_bytes/1, ip_to_bytes/1]).

%% format error
-export([format_error/1]).

%% imports
-import(lists, [reverse/1, keysearch/3, keymember/3]).


%% Async (we do not expect an answer)
%% FIXME: when monitors arrive we should use it to this correct and sync
close(Socket) when record(Socket, socket) ->
    Socket#socket.pid ! {call, self(), none, close},
    ok.

peername(Socket) -> call(Socket, peer).

setpeername(Socket, {IP,Port}) ->
    call(Socket, {setpeername,{IP,Port}});
setpeername(Socket, undefined) ->
    call(Socket, {setpeername,undefined}).

setname(Socket, {IP,Port}) -> 
    call(Socket, {setname,{IP,Port}});
setname(Socket, undefined) ->
    call(Socket, {setname,undefined}).

sockname(Socket) -> call(Socket, name).

name(Socket) -> call(Socket, name).  %% OLD name (remove)

port(Socket) ->
    case call(Socket, name) of
	{ok, { _, Port}} -> {ok, Port};
	Error -> Error
    end.

send(Socket, Packet) -> 
    call(Socket, {send, Packet}).

setopts(Socket, Opts) -> 
    call(Socket, {setopts, Opts}).

getopts(Socket, Opts) ->
    call(Socket, {getopts, Opts}).
%%
%% Function protocol stack:
%%
%% Functions must be on the from
%% fun (input, Data, State) -> 
%%           {input, Data', State'}    %% input data
%%         | {output, Data', State'}   %% loop-out data
%%         | {false, State'}           %% consume data
%%     (output, Data, State) ->
%%           {input, Data', State'}    %% loop-in data
%%         | {output, Data', State'}   %% output
%%         | {false, State'}           %% consume data
%%     (event, Event, State) ->
%%           {input, Data', State'}    %% input data
%%         | {output, Data', State'}   %% output data
%%         | {event,  Event, State'}   %% change/propagte event
%%         | {false, State'}           %% consume event
%%     (option, Option, State) ->
%%         |  {option, Opttion, State'} %% change/accept option
%%         |  {false, State'}           %% consume option
%%         
%%
pushf(Socket, Fun, State) when function(Fun) ->
    call(Socket, {pushf, Fun, State}).

popf(Socket) ->
    call(Socket, popf).

getif(Socket) -> 
    call(Socket, getif).

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
    call(Socket, gethostname).

getstat(Socket) ->
    getstat(Socket, stats()).

getstat(Socket,What) when record(Socket,socket) ->
    if port(Socket#socket.port) ->
	    ctl_getstat(Socket#socket.port, What);
       true ->
	    call(Socket, {getstat, What})
    end.

gethostbyname(Name) ->
    gethostbyname(Name, inet, inet_db:res_option(lookup)).

gethostbyname(Name,Family) ->
    gethostbyname(Name, Family, inet_db:res_option(lookup)).

gethostbyaddr(Address) ->
    gethostbyaddr(Address, inet_db:res_option(lookup)).


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
getll(Socket) ->
    call(Socket, getll).

%%
%% Lookup an ip address
%%
getaddr(IP, inet) when tuple(IP), size(IP) == 4 ->
    {A,B,C,D} = IP,
    if 
	integer(A+B+C+D) -> {ok, IP};
	true -> {error,einval}
    end;
getaddr(IP, inet6) when tuple(IP), size(IP) == 8 ->
    {A,B,C,D,E,F,G,H} = IP,
    if 
	integer(A+B+C+D+E+F+G+H) -> {ok, IP};
	true -> {error,einval}
    end;
getaddr(Address, Family) when atom(Address) ->
    getaddr(atom_to_list(Address), Family);
getaddr(Address, Family) ->
    Result = case inet_parse:visible_string(Address) of
	true when Family == inet ->
	    inet_parse:ipv4_address(Address);
	true when Family == inet6 -> 
	    inet_parse:ipv6_address(Address);
	false ->
	    false
    end,
    case Result of
	{ok,_} -> Result;
	false -> {error, einval};
	_ -> getaddr1(Address, Family)
    end;
getaddr(_,_) -> {error, einval}.

getaddr1(Address, Family) ->
    case gethostbyname(Address,Family) of
	{ok, Ent} -> {ok, hd(Ent#hostent.h_addr_list)};
	Error -> Error
    end.


%% not ready yet 
getservbyport(Port, _) when integer(Port) ->
    {error, einval}.

%% partially ready
getservbyname(Name, _) when atom(Name) ->
    P = case Name of
	    echo ->   ?IPPORT_ECHO;
	    discard -> ?IPPORT_DISCARD;
	    systat  -> ?IPPORT_SYSTAT;
	    daytime -> ?IPPORT_DAYTIME;
	    netstat -> ?IPPORT_NETSTAT;
	    ftp -> ?IPPORT_FTP;
	    telnet -> ?IPPORT_TELNET;
	    smtp -> ?IPPORT_SMTP;
	    timeserver -> ?IPPORT_TIMESERVER;
	    nameserver -> ?IPPORT_NAMESERVER;
	    whois -> ?IPPORT_WHOIS;
	    mtp -> ?IPPORT_MTP;
	    tftp -> ?IPPORT_TFTP;
	    rje -> ?IPPORT_RJE;
	    finger -> ?IPPORT_FINGER;
	    ttylink -> ?IPPORT_TTYLINK;
	    supdup -> ?IPPORT_SUPDUP;
	    execserver -> ?IPPORT_EXECSERVER;
	    loginserver -> ?IPPORT_LOGINSERVER;
	    cmdserver -> ?IPPORT_CMDSERVER;
	    efsserver -> ?IPPORT_EFSSERVER;
	    biffudp -> ?IPPORT_BIFFUDP;
	    whoserver -> ?IPPORT_WHOSERVER;
	    routeserver -> ?IPPORT_ROUTESERVER;
	    _ -> -1
	end,
    if P == -1 -> {error, einval};
       true  -> {ok, P}
    end.


%% Return a list of available options
options() ->
    [reuseaddr, keepalive, dontroute, linger,
     broadcast, sndbuf, recbuf, nodelay,
     header, active, packet].

%% Return a list of statistics options
stats() ->
    [recv_cnt, recv_max, recv_avg, recv_dvi,
     send_cnt, send_max, send_avg, send_pend].

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
	{tcp_closed, Socket} ->
	    {error, closed};
	{udp_closed, Socket} ->
	    {error, closed}
    end.

%%
%% Socket options processing
%%
enc_opt(reuseaddr) ->  ?INET_OPT_REUSEADDR;
enc_opt(keepalive) ->  ?INET_OPT_KEEPALIVE;
enc_opt(dontroute) ->  ?INET_OPT_DONTROUTE;
enc_opt(linger) ->     ?INET_OPT_LINGER;
enc_opt(broadcast) ->  ?INET_OPT_BROADCAST;
enc_opt(sndbuf) ->     ?INET_OPT_SNDBUF;
enc_opt(recbuf) ->     ?INET_OPT_RCVBUF;
enc_opt(nodelay) ->    ?TCP_OPT_NODELAY;
enc_opt(multicast_if) -> ?UDP_OPT_MULTICAST_IF;
enc_opt(multicast_ttl) -> ?UDP_OPT_MULTICAST_TTL;
enc_opt(multicast_loop) -> ?UDP_OPT_MULTICAST_LOOP;
enc_opt(add_membership) -> ?UDP_OPT_ADD_MEMBERSHIP;
enc_opt(drop_membership) -> ?UDP_OPT_DROP_MEMBERSHIP;
enc_opt(buffer)  ->    ?INET_LOPT_BUFFER;
enc_opt(header)  ->    ?INET_LOPT_HEADER;
enc_opt(active)  ->    ?INET_LOPT_ACTIVE;
enc_opt(packet)  ->    ?INET_LOPT_PACKET;
enc_opt(_) ->          -1.

dec_opt(?INET_OPT_REUSEADDR) -> reuseaddr;
dec_opt(?INET_OPT_KEEPALIVE) -> keepalive;
dec_opt(?INET_OPT_DONTROUTE) -> dontroute;
dec_opt(?INET_OPT_LINGER)    -> linger;
dec_opt(?INET_OPT_BROADCAST) -> broadcast;
dec_opt(?INET_OPT_SNDBUF)    -> sndbuf;
dec_opt(?INET_OPT_RCVBUF)    -> recbuf;
dec_opt(?TCP_OPT_NODELAY)    -> nodelay;
dec_opt(?UDP_OPT_MULTICAST_IF) -> multicast_if;
dec_opt(?UDP_OPT_MULTICAST_TTL) -> multicast_ttl;
dec_opt(?UDP_OPT_MULTICAST_LOOP) -> multicast_loop;
dec_opt(?UDP_OPT_ADD_MEMBERSHIP) -> add_membership;
dec_opt(?UDP_OPT_DROP_MEMBERSHIP) -> drop_membership;
dec_opt(?INET_LOPT_BUFFER)   -> buffer;
dec_opt(?INET_LOPT_HEADER)   -> header;
dec_opt(?INET_LOPT_ACTIVE)   -> active;
dec_opt(?INET_LOPT_PACKET)   -> packet;
dec_opt(_)                   -> undefined.

type_opt(reuseaddr) -> bool;
type_opt(keepalive) -> bool;
type_opt(dontroute) -> bool;
type_opt(linger)    -> {bool, int};
type_opt(broadcast) -> bool;
type_opt(sndbuf)    -> int;
type_opt(recbuf)    -> int;
type_opt(nodelay)   -> bool;
%% multicast
type_opt(multicast_ttl) -> int;
type_opt(multicast_loop) -> bool;
type_opt(multicast_if)   -> ip;
type_opt(add_membership) -> {ip,ip};
type_opt(drop_membership) -> {ip,ip};
%% driver options
type_opt(header)    -> uint;
type_opt(buffer)    -> int;
type_opt(active)    -> bool;
type_opt(packet)        -> 
    {enum, [{0, ?TCP_PB_RAW},
	    {1, ?TCP_PB_1},
	    {2, ?TCP_PB_2},
	    {4, ?TCP_PB_4},
	    {raw,?TCP_PB_RAW},
	    {sunrm, ?TCP_PB_RM},
	    {asn1, ?TCP_PB_ASN1},
	    {cdr, ?TCP_PB_CDR},
	    {fcgi, ?TCP_PB_FCGI}]};
type_opt(_)         -> undefined.

type_value(bool, true) -> true;
type_value(bool, false) -> true;
type_value(int, X) when integer(X) -> true;
type_value(uint, X) when integer(X) -> true;
type_value(ip,{A,B,C,D}) when ?ip(A,B,C,D) -> true;
type_value({X,Y},{XV,YV}) -> type_value(X,XV) and type_value(Y,YV);
type_value({enum,List},Enum) -> keymember(Enum, 1, List);
type_value(_, _) -> false.

enc_value(bool, true) -> [0,0,0,1];
enc_value(bool, false) -> [0,0,0,0];
enc_value(int, Val) -> ?int32(Val);
enc_value(uint, Val) -> ?int32(Val);
enc_value(ip,{A,B,C,D}) -> [A,B,C,D];
enc_value({enum,List},Enum) ->
    {value,{_,Val}} = keysearch(Enum, 1, List),
    ?int32(Val);
enc_value({X,Y},{XV,YV}) -> enc_value(X,XV) ++ enc_value(Y,YV).


dec_value(bool, [0,0,0,0 | T]) -> {false, T};
dec_value(bool, [_,_,_,_ | T]) -> {true, T};
dec_value(int,  [X3,X2,X1,X0|T]) -> {?i32(X3,X2,X1,X0), T};
dec_value(uint, [X3,X2,X1,X0|T]) -> {?u32(X3,X2,X1,X0), T};
dec_value(ip, [A,B,C,D|T]) -> {{A,B,C,D}, T};
dec_value({enum,List}, [X3,X2,X1,X0|T]) ->
    Val = ?i32(X2,X2,X1,X0),
    case keysearch(Val, 2, List) of
	{value, {Enum,Val}} ->
	    {Enum, T};
	_ -> {undefined, T}
    end;
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
		  
is_sockopt_val(Opt,Val) ->
    case type_opt(Opt) of
	undefined -> false;
	Type -> type_value(Type,Val)
    end;
is_sockopt_val(_,_) ->
    false.

%%
%% Encode options in reversed order
%% socket options is given reversed order so the
%% driver will see them in the correct order
%%

encode_opt_val(Opts) ->
    enc_opt_val(Opts, []).

enc_opt_val([{Opt,Val} | Opts], Buf) ->
    case type_opt(Opt) of
	undefined -> {error, einval};
	Type ->
	    case type_value(Type, Val) of
		false -> {error, einval};
		true -> enc_opt_val(Opts, 
				    [enc_opt(Opt),enc_value(Type,Val),Buf])
	    end
    end;
enc_opt_val([], Buf) -> 
    {ok, Buf};
enc_opt_val(_, Buf) ->
    {error, einval}.

encode_opts(Opts) ->
    enc_opts(Opts, []).

enc_opts([Opt | Opts], Buf) ->
    case enc_opt(Opt) of
	-1 -> {error, einval};
	B -> enc_opts(Opts, [B | Buf])
    end;
enc_opts([], Buf) -> 
    {ok, Buf}.


decode_opt_val(Buf) ->
    dec_opt_val(Buf, []).

dec_opt_val([B | Buf], Opts) ->
    case dec_opt(B) of
	undefined -> {error, einval};
	Opt ->
	    {Val,T} = dec_value(type_opt(Opt), Buf),
	    dec_opt_val(T, [{Opt,Val} | Opts])
    end;
dec_opt_val(_, Opts) -> 
    {ok, Opts}.


socket_options(Opts) ->
    sock_opt(Opts, #sock { local_ip = ip_any(inet) }, inet).

socket_options(Opts, T) ->
    sock_opt(Opts, #sock { local_ip = ip_any(T)}, T).

ip_any(inet) -> {0,0,0,0};
ip_any(inet6) -> {0,0,0,0,0,0,0,0}.

ip_loopback(inet) -> {127,0,0,1};
ip_loopback(inet6) -> {0,0,0,0,0,0,0,1}.

sock_opt([Opt|Opts], St, T) ->
    case Opt of
	{port,P} when integer(P) -> 
	    sock_opt(Opts, St#sock { local_port = P }, T);
	{ip,any} ->
	    sock_opt(Opts, St#sock { local_ip = ip_any(T) }, T);
	{ip,loopback} ->
	    sock_opt(Opts, St#sock { local_ip = ip_loopback(T) }, T);
	{ip,{A,B,C,D}} when integer(A+B+C+D+D) ->
	    sock_opt(Opts, St#sock { local_ip = {A,B,C,D} }, T);
	{ip,{A,B,C,D,E,F,G,H}} when integer(A+B+C+D+E+F+G+H) ->
	    sock_opt(Opts, St#sock { local_ip = {A,B,C,D,E,F,G,H} }, T);
	{fd,Fd} when integer(Fd) ->
	    ins_other_opt(fd,Fd,Opts,St,T);
	{fake_name,{IP,Port}} ->
	    sock_opt(Opts, St#sock { fake_name = {IP,Port} }, T);
	{fake_peer,{IP,Port}} ->
	    sock_opt(Opts, St#sock { fake_peer = {IP,Port} }, T);
	binary -> 
	    sock_opt(Opts, St#sock { open_opts = [binary] }, T);
	list ->
	    sock_opt(Opts, St#sock { open_opts = [] }, T);
	{X,Val} when atom(X) ->
	    case is_sockopt_val(X,Val) of
		true -> ins_sock_opt(X,Val, Opts, St, T);
		false -> ins_other_opt(X,Val,Opts,St, T)
	    end;
	_ -> {error, badarg}
    end;
sock_opt([], St, T) -> {ok, St};
sock_opt(_, St, T) -> {error, badarg}.


%% Active MUST be flagged as a seprate field
ins_sock_opt(active, Stat, Opts, St, T) ->
    S0 = St#sock.sock_opts,
    sock_opt(Opts, St#sock { active = Stat, 
			     sock_opts = [{active,Stat}|S0]}, T);
ins_sock_opt(Opt, Val, Opts, St, T) ->
    SO = St#sock.sock_opts,
    sock_opt(Opts, St#sock { sock_opts = [{Opt,Val} | SO] }, T).

ins_other_opt(Opt, Val, Opts, St, T) ->
    SO = St#sock.other_opts,
    sock_opt(Opts, St#sock { other_opts = [{Opt,Val} | SO] }, T).

%%
%% Encode/Deocode socket statistics
%%

%% enumerate the statistics ops
enc_stat(recv_cnt)  -> ?INET_STAT_RECV_CNT;
enc_stat(recv_max)  -> ?INET_STAT_RECV_MAX;
enc_stat(recv_avg)  -> ?INET_STAT_RECV_AVG;
enc_stat(recv_dvi)  -> ?INET_STAT_RECV_DVI;
enc_stat(send_cnt)  -> ?INET_STAT_SEND_CNT;
enc_stat(send_max)  -> ?INET_STAT_SEND_MAX;
enc_stat(send_avg)  -> ?INET_STAT_SEND_AVG;
enc_stat(send_pend) -> ?INET_STAT_SEND_PEND;
enc_stat(_)         -> {error, einval}.

%% build a list of statitics ops (reverse)
enc_stats(Ws) ->
    enc_stats(Ws, []).

enc_stats([A | Ws], Acc) ->
    case enc_stat(A) of
	X when integer(X) -> enc_stats(Ws, [X | Acc]);
	Error -> Error
    end;
enc_stats([], Acc) -> {ok, Acc};
enc_stats(_, _) -> {error, einval}.

%% denumerate the ops
dec_stat(?INET_STAT_RECV_CNT)  -> recv_cnt;
dec_stat(?INET_STAT_RECV_MAX)  -> recv_max;
dec_stat(?INET_STAT_RECV_AVG)  -> recv_avg;
dec_stat(?INET_STAT_RECV_DVI)  -> recv_dvi;
dec_stat(?INET_STAT_SEND_CNT)  -> send_cnt;
dec_stat(?INET_STAT_SEND_MAX)  -> send_max;
dec_stat(?INET_STAT_SEND_AVG)  -> send_avg;
dec_stat(?INET_STAT_SEND_PEND) -> send_pend;
dec_stat(_)         -> {error, einval}.

%% build a list of reply paris (reverse i.e they are normal again)
dec_stats(Data) ->
    dec_stats(Data, []).

dec_stats([X,X3,X2,X1,X0 | R], Acc) ->
    case dec_stat(X) of
	A when atom(A) ->
	    dec_stats(R, [{A,?u32(X3,X2,X1,X0)} | Acc]);
	Error -> Error
    end;
dec_stats([], Acc) -> {ok, Acc};
dec_stats(_, _) -> {error, einval}.

%%
%% Low level stuff common for UDP/TCP 
%%

%% Common handle call dispatcher


handle_call(peer, Inet, St) ->
    if St#sock.fake_peer == undefined ->
	    ll_reply(ll_peer(Inet), St);
       true ->
	    ll_reply({ok, St#sock.fake_peer}, St)
    end;
handle_call(name, Inet, St) ->
    if St#sock.fake_name == undefined ->
	    ll_reply(ll_name(Inet), St);
       true ->
	    ll_reply({ok, St#sock.fake_name}, St)
    end;
handle_call({getstat,What}, Inet, St) ->
    case enc_stats(What, []) of
	{ok, Bytes} ->
	    ll_reply(ll_getstat(Inet,Bytes), St);
	Error ->
	    ll_reply(Error, St)
    end;

handle_call(getif, Inet, St) ->
    ll_reply(ll_getif(Inet), St);
handle_call({setopts,Opts}, Inet, St) -> 
    {Opts1,St1} = handle_opts(Opts, St),
    case ll_setopts(Inet, Opts1) of
	%% We want the active flag to reflect the correct status
	%% this code must be rewritten sooner or later.
	ok -> ll_reply(ok, update_active_hack(Opts1, St1));
	Error -> ll_reply(Error, St1)
    end;
handle_call({getopts,Opts}, Inet, St) ->
    ll_reply(ll_getopts(Inet, Opts), St);
handle_call(gethostname, Inet, St) ->
    ll_reply(ll_gethostname(Inet), St);
handle_call({pushf,Fun,State}, Inet, St) ->
    Fs = St#sock.fs,
    ll_reply(ok, St#sock { fs = [{Fun,State} | Fs]} );
handle_call(popf, Inet, St) ->
    Fs = St#sock.fs,
    St1 = if Fs == [] -> St; true -> St#sock { fs = tl(Fs) } end,
    ll_reply(ok, St1);
handle_call({setpeername, Name}, Inet, St) ->
    ll_reply(ok, St#sock { fake_peer = Name });
handle_call({setname, Name}, Inet, St) ->
    ll_reply(ok, St#sock { fake_name = Name });
handle_call(getll, Inet, St) ->
    ll_reply({ok, Inet}, St);

%% Sync set owner {owner, NewOwner, State} is returned
handle_call({set_owner, New}, Inet, St) ->
    {owner, New, St};
%% Close is special and return {stop, State} to caller
handle_call(close, Inet, St) ->
    ll_close(Inet),
    {stop, St};
handle_call(Req, Inet, St) ->
    {reply, {error, einval}, St}.

%%
%% Handle options (call functions)
%% Return reverse opts as input to ll_setopts !!!
%%
handle_opts(Opts, St) ->
    handle_opts(Opts, St, []).

handle_opts([Option | InOpts], St, OutOpts) ->
    case handle_option(St#sock.fs, Option, St) of
	{option, Option1, St1} ->
	    handle_opts(InOpts, St1, [Option1 | OutOpts]);
	{false, St1} ->
	    handle_opts(InOpts, St1, OutOpts)
    end;
handle_opts([], St, OutOpts) ->
    {OutOpts, St}.

%% Process a single option
handle_option([], Option, St) ->
    {option, Option, St};
handle_option(Fs, Option, St) ->
    handle_option(Fs, [], Option, St).

handle_option([], Called, Option, St) ->
    { option, Option, St#sock { fs = reverse(Called) }};
handle_option([{Fun,S} | Fs], Called, Option, St) ->
    case Fun(option, Option, S) of
	{option, Option1, S1} ->
	    handle_option(Fs, [{Fun,S1}|Called], Option1, St);
	{false, S1} ->
	    { false, St#sock { fs = reverse(Called) ++ [{Fun,S1} | Fs] }}
    end.

%%
%% Update the active flag status
%% Since the option list is REVERSE we are done when we find 
%% the first active flag value
%%
update_active_hack([{active,Stat} | _], St) ->
    St#sock { active = Stat };
update_active_hack([_ | Opts], St) ->
    update_active_hack(Opts, St);
update_active_hack([], St) ->
    St.
    
%%
%% Opts must be in reverse order
%%
ll_setopts(Inet, Opts) ->
    case encode_opt_val(Opts) of
	{ok, Buf} ->
	    case sync_cmd(Inet, [?INET_REQ_SETOPTS | Buf],
			  ?INET_REP_SETOPTS) of
		{ok, _} -> ok;
		Error -> Error
	    end;
	Error  -> Error
    end.

%% get internal index i inet_drv table (used by accept)
ll_index(Inet) ->
    case sync_cmd(Inet, [?INET_REQ_IX], ?INET_REP_IX) of
	{ok, [I1,I0]} -> {ok, ?u16(I1,I0)};
	Error -> Error
    end.


ll_getopts(Inet, Opts) ->
    case encode_opts(Opts) of
	{ok, Buf} ->
	    case sync_cmd(Inet, [?INET_REQ_GETOPTS | Buf],
			  ?INET_REP_GETOPTS) of
		{ok, Rep} -> 
		    decode_opt_val(Rep);
		Error -> Error
	    end;
	Error -> Error
    end.

build_if(L0, inet, Ls) when length(L0) >= 12 ->
    A = get_in(L0),  L1 = skip(4,L0),
    B = get_in(L1),  L2 = skip(4,L1),
    M = get_in(L2),  L3 = skip(4,L2),
    build_if(L3, inet, [{A,B,M}|Ls]);
build_if(L0, inet6, Ls) when length(L0) >= 48 ->
    A = get_in6(L0),  L1 = skip(16,L0),
    B = get_in6(L1),  L2 = skip(16,L1),
    M = get_in6(L2),  L3 = skip(16,L2),
    build_if(L3, inet6, [{A,B,M}|Ls]); 
build_if(_, _, Ls) -> Ls.


build_if(Data, Type) when binary(Data) ->
    build_if(binary_to_list(Data), Type, []);
build_if(Data, Type) ->
    build_if(Data, Type, []).


%% get all interfaces i.e list of {IP,SubNetMask}
ll_getif(Inet) ->
    case sync_cmd(Inet, [?INET_REQ_GETIF], ?INET_REP_GETIF) of
	{ok, [?INET_AF_INET | Data]} ->
	    {ok, build_if(Data, inet)};
	{ok, [?INET_AF_INET6 | Data]} ->
	    {ok, build_if(Data, inet6)};
	Error -> Error
    end.

%% ioctl version of ll_getstat
ctl_getstat(Port, Stats) ->
    case enc_stats(Stats) of
	{ok, Bytes} ->
	    case catch port_control(Port, ?INET_REQ_GETSTAT, Bytes) of
		{'EXIT',_} -> {error, einval};
		[?INET_REP_GETSTAT|Data] ->
		    dec_stats(Data);
		_ -> {error, einval}
	    end;
	Error -> Error
    end.

%%
%% infinity is encoded as max int32
%% this value must no be used for other timeout values!!!
%%
enc_time(infinity) -> [255,255,255,255];
enc_time(Time) -> ?int32(Time).


%% open/accept/setopts LIX = listen index, tcp only
ll_open_accept_set(LIX, St, Time) ->
    PortOpts = St#sock.open_opts,
    case catch open_port({spawn, tcp_inet}, PortOpts) of
	{'EXIT', Reason} ->
	    {error, Reason};
	Tcp when port(Tcp) -> 
	    case ll_accept(Tcp, LIX, Time) of
		ok ->
		    case ll_setopts(Tcp, St#sock.sock_opts) of
			ok -> {ok, Tcp};
			Error -> Error
		    end;
		Error -> Error
	    end;
	Error -> Error
    end.

%% accept LIX is the index of the listen port (get with ll_index)
ll_accept(Tcp, LIX, Time) ->
    case sync_cmd(Tcp,
		  [?TCP_REQ_ACCEPT,enc_time(Time),?int16(LIX)],
		  ?TCP_REP_ACCEPT) of
	{ok, _} -> ok;
	Error -> Error
    end.


%% combination of open-set-bind/fdopen
ll_open_set_bind(Name,Family,St) ->
    case lists:keysearch(fd, 1, St#sock.other_opts) of
	{value, {_,Fd}} ->
	    case ll_fdopen(Name,Family,St#sock.open_opts,Fd) of
		{ok, Inet} ->
		    ll_setopts(Inet, St#sock.sock_opts),
		    {ok, Inet};
		Error -> Error
	    end;
	_ ->
	    case ll_open(Name,Family, St#sock.open_opts) of
		{ok, Inet} ->
		    case ll_setopts(Inet, St#sock.sock_opts) of
			ok ->
			    case ll_bind(Inet,St#sock.local_ip,
					 St#sock.local_port) of
				{ok, Port2} -> {ok, Inet};
				Error -> Error
			    end;
			Error -> Error
		    end;
		Error -> Error
	    end
    end.

%% get statistics 
ll_getstat(Inet, Bytes) ->
    case sync_cmd(Inet, [?INET_REQ_GETSTAT|Bytes], ?INET_REP_GETSTAT) of
	{ok, Data} ->
	    dec_stats(Data);
	Error -> Error
    end.

ll_gethostname(Inet) ->
    sync_cmd(Inet, [?INET_REQ_GETHOSTNAME], ?INET_REP_GETHOSTNAME).

%% get local name
ll_name(Inet) ->
    case sync_cmd(Inet, [?INET_REQ_NAME], ?INET_REP_NAME) of
	{ok, [P1, P0 | Addr]} ->
	    {ok, { bytes_to_ip(Addr), ?u16(P1, P0) }};
	Error -> Error
    end.

%% get peer name
ll_peer(Inet) ->
    case sync_cmd(Inet, [?INET_REQ_PEER], ?INET_REP_PEER) of
	{ok, [P1,P0 | Addr]} ->
	    {ok, { bytes_to_ip(Addr), ?u16(P1, P0) }};
	Error -> Error
    end.

%% bind local address and port
ll_bind(Inet,IP,Port) ->
    case sync_cmd(Inet,
		  [?INET_REQ_BIND,?int16(Port), ip_to_bytes(IP)],
		  ?INET_REP_BIND) of
	{ok, [P1,P0]} ->
	    {ok, ?u16(P1, P0)};
	Error -> Error
    end.

%% connect 
ll_connect(Inet, IP, Port) ->
    case sync_cmd(Inet, 
		  [?INET_REQ_CONNECT, ?int16(Port), ip_to_bytes(IP)],
		  ?INET_REP_CONNECT) of
	{ok, _} -> ok;
	Error -> Error
    end.


%% open a driver (tcp/udp)
ll_open(Name, Family, PortOpts) ->
    case catch open_port({spawn, Name}, PortOpts) of
	{'EXIT', Reason} ->
	    {error, Reason};
	Inet ->
	    case sync_cmd(Inet, [?INET_REQ_OPEN,Family], ?INET_REP_OPEN) of
		{ok, _} -> {ok, Inet};
		Error -> Error
	    end
    end.

%% open a driver (tcp/udp) and set socket desriptor
ll_fdopen(Name, Family, PortOpts, Fd) ->
    case catch open_port({spawn, Name}, PortOpts) of
	{'EXIT', Reason} ->
	    {error, Reason};
	Inet ->
	    case sync_cmd(Inet, [?INET_REQ_FDOPEN,Family,?int32(Fd)],
			  ?INET_REP_FDOPEN) of
		{ok, _} -> {ok, Inet};
		Error -> Error
	    end
    end.

%% Close must be handled sending close to the port
%% and wait for closed reply
ll_close(Inet) ->
    Inet ! {self(), close},
    receive
	{Inet, closed} -> ok;
	{'EXIT', Inet, _} -> ok
    end.

%% contruct a reply or an exit 
ll_reply({exit,Reason}, St) -> {exit, Reason, St};
ll_reply(Reply, St) -> {reply, Reply, St}.


sync_cmd(Port, Cmd, Rep) ->
    Port ! {self(), {command, Cmd}},
    receive
	{Port, {data, [Rep | T]}} -> 
	    {ok, T};
	{Port, {data, [?INET_REP_ERROR | Err]}} -> 
	    {error, list_to_atom(Err)};
	{'EXIT', Port, badsig} -> {error, einval};
	{'EXIT', Port, Reason} -> {exit,Reason}
    end.

%%
%% gethostbyname with option search
%%
gethostbyname(Name, Type, [dns | Opts]) ->
    case inet_res:gethostbyname(Name, Type) of
	{error,formerr} -> exit(badarg);
	{error,_} -> gethostbyname(Name,Type,Opts);
	Result -> Result
    end;
gethostbyname(Name, Type, [file | Opts]) ->
    case inet_hosts:gethostbyname(Name, Type) of
	{error,formerr} -> exit(badarg);
	{error,_} -> gethostbyname(Name,Type,Opts);
	Result -> Result
    end;
gethostbyname(Name, Type, [yp | Opts]) ->
	gethostbyname(Name, Type,[native|Opts]);
gethostbyname(Name, Type, [nis | Opts]) ->
	gethostbyname(Name, Type,[native|Opts]);
gethostbyname(Name, Type, [nisplus | Opts]) ->
	gethostbyname(Name, Type,[native|Opts]);
gethostbyname(Name, Type, [wins | Opts]) ->
	gethostbyname(Name, Type,[native|Opts]);
gethostbyname(Name, Type, [native | Opts]) ->
    case inet_gethost_native:gethostbyname(Name, Type) of
	{error,formerr} -> exit(badarg);
	{error,_} -> gethostbyname(Name,Type,Opts);
	Result -> Result
    end;
gethostbyname(Name, Type, [_ | Opts]) ->
    gethostbyname(Name, Type, Opts);
gethostbyname(Name, Type, []) ->
    {error, nxdomain}.

%%
%% gethostbyaddr with option search
%%
gethostbyaddr(Addr, [dns | Opts]) ->
    case inet_res:gethostbyaddr(Addr) of
	{error,formerr} -> {error, einval};
	{error,_} -> gethostbyaddr(Addr,Opts);
	Result -> Result
    end;    
gethostbyaddr(Addr, [file | Opts]) ->
    case inet_hosts:gethostbyaddr(Addr) of
	{error,formerr} -> {error, einval};
	{error,_} -> gethostbyaddr(Addr,Opts);
	Result -> Result
    end;    
gethostbyaddr(Addr, [yp | Opts]) ->
	gethostbyaddr(Addr, [native | Opts]);
gethostbyaddr(Addr, [nis | Opts]) ->
	gethostbyaddr(Addr, [native | Opts]);
gethostbyaddr(Addr, [nisplus | Opts]) ->
	gethostbyaddr(Addr, [native | Opts]);
gethostbyaddr(Addr, [wins | Opts]) ->
	gethostbyaddr(Addr, [native | Opts]);
gethostbyaddr(Addr, [native | Opts]) ->
	case inet_gethost_native:gethostbyaddr(Addr) of
	{error,formerr} -> {error, einval};
	{error,_} -> gethostbyaddr(Addr,Opts);
	Result -> Result
    end;    
gethostbyaddr(Addr, [_ | Opts]) ->
    gethostbyaddr(Addr, Opts);
gethostbyaddr(Addr, []) ->
    {error, nxdomain}.



ip_to_bytes(IP) when size(IP) == 4 -> ip4_to_bytes(IP);
ip_to_bytes(IP) when size(IP) == 8 -> ip6_to_bytes(IP).

bytes_to_ip([A,B,C,D]) -> {A,B,C,D};
bytes_to_ip([X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16]) ->
    { ?u16(X1,X2),?u16(X3,X4),?u16(X5,X6),?u16(X7,X8),
     ?u16(X9,X10),?u16(X11,X12),?u16(X13,X14),?u16(X15,X16)}.

get_in([A,B,C,D | _]) -> {A,B,C,D}.

get_in6([X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16 | _]) ->
    { ?u16(X1,X2),?u16(X3,X4),?u16(X5,X6),?u16(X7,X8),
     ?u16(X9,X10),?u16(X11,X12),?u16(X13,X14),?u16(X15,X16)}.

skip(0, L) -> L;
skip(N, [_|T]) -> skip(N-1,T).

ip4_to_bytes({A,B,C,D}) ->
    [A band 16#ff, B band 16#ff, C band 16#ff, D band 16#ff].

ip6_to_bytes({A,B,C,D,E,F,G,H}) ->
    [?int16(A), ?int16(B), ?int16(C), ?int16(D),
     ?int16(E), ?int16(F), ?int16(G), ?int16(H)].

bytes_to_ip6(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16) ->
    { ?u16(X1,X2),?u16(X3,X4),?u16(X5,X6),?u16(X7,X8),
     ?u16(X9,X10),?u16(X11,X12),?u16(X13,X14),?u16(X15,X16)}.

format_error(Tag) ->
    erl_posix_msg:message(Tag).

    
