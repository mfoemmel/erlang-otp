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
-module(inet_res).

%% If the macro DEBUG is defined during compilation, 
%% debug printouts are done through erlang:display/1.
%% Activate this feature by starting the compiler 
%% with> erlc -DDEBUG ... 
%% or by> setenv ERL_COMPILER_FLAGS DEBUG 
%% before running make (in the OTP make system)
%% (the example is for tcsh)


-export([gethostbyname/1, gethostbyname/2, gethostbyname/3,
	 gethostbyname_tm/3]).
-export([gethostbyaddr/1, gethostbyaddr/2,
	 gethostbyaddr_tm/2]).
-export([getbyname/2, getbyname/3,
	 getbyname_tm/3]).

-export([nslookup/3, nslookup/4]).
-export([nnslookup/4, nnslookup/5]).

-import(inet_db, [res_option/1]).

-include_lib("kernel/include/inet.hrl").
-include("inet_res.hrl").
-include("inet_dns.hrl").
-include("inet_int.hrl").

-ifdef(DEBUG).
-define(dbg(Fmt, Args), io:format(Fmt, Args)).
-else.
-define(dbg(Fmd, Args), ok).
-endif.

%% --------------------------------------------------------------------------
%% nslookup:
%%
%% Do a general nameserver lookup
%%
%% Perform nslookup on standard config !!    

nslookup(Name, Class, Type) ->
    nslookup2(Name,Class,Type,false).

nslookup(Name, Class, Type, Timeout) when integer(Timeout), Timeout >= 0 ->
    Timer = inet:start_timer(Timeout),
    Res = nslookup2(Name,Class,Type,Timer),
    inet:stop_timer(Timer),
    Res;
nslookup(Name, Class, Type, Ns) ->              % For backwards compatibility
    nslookup1(Name,Class,Type,Ns,false).        % with OTP R6B only

nnslookup(Name, Class, Type, Ns) ->
    nslookup1(Name,Class,Type,Ns,false).    

nnslookup(Name, Class, Type, Ns, Timeout) ->
    Timer = inet:start_timer(Timeout),
    Res = nslookup1(Name,Class,Type,Ns,Timer),
    inet:stop_timer(Timer),
    Res.    

nslookup2(Name, Class, Type, Timer) ->
    case nslookup1(Name, Class, Type, res_option(nameserver), Timer) of
	{error, nxdomain} ->
	    case res_option(alt_nameserver) of
		[] ->
		    {error, nxdomain};
		AltNameserver ->
		    nslookup1(Name, Class, Type, AltNameserver, Timer)
	    end;
	Reply -> Reply
    end.
    

nslookup1(Name, Class, Type, Ns, Timer) ->
    case nsdname(Name) of
	{ok, Nm} ->
	    case res_mkquery(Nm, Class, Type) of
		{ok, Id, Buffer} ->
		    res_send2(Id, Buffer, Ns, Timer);
		Error -> Error
	    end;
	Error -> Error
    end.

		
%% --------------------------------------------------------------------------
%%
%% gethostbyaddr(ip_address()) => {ok, hostent()} | {error, Reason}
%%
%% where ip_address() is {A,B,C,D} ipv4 address
%%                    |  {A,B,C,D,E,F,G,H}  ipv6 address
%%                    | string versions of the above
%%                    | atom version
%%
%% --------------------------------------------------------------------------

gethostbyaddr(IP) -> gethostbyaddr_tm(IP,false).

gethostbyaddr(IP,Timeout) ->
    Timer = inet:start_timer(Timeout),
    Res = gethostbyaddr_tm(IP,Timer),
    inet:stop_timer(Timeout),
    Res.    

gethostbyaddr_tm({A,B,C,D}, Timer) when ?ip(A,B,C,D) ->
    IP = {A,B,C,D},
    case inet_db:gethostbyaddr(IP) of
	{ok, HEnt} -> {ok, HEnt};
	_ -> res_gethostbyaddr(dn_in_addr_arpa(A,B,C,D), IP, Timer)
    end;
%% ipv4  only ipv6 address
gethostbyaddr_tm({0,0,0,0,0,16#ffff,G,H},Timer) when integer(G+H) ->
    gethostbyaddr_tm({G div 256, G rem 256, H div 256, H rem 256},Timer);
gethostbyaddr_tm({A,B,C,D,E,F,G,H},Timer) when ?ip6(A,B,C,D,E,F,G,H) ->
    IP = {A,B,C,D,E,F,G,H},
    case inet_db:gethostbyaddr(IP) of
	{ok, HEnt} -> {ok, HEnt};
	_ -> res_gethostbyaddr(dn_ip6_int(A,B,C,D,E,F,G,H), IP, Timer)
    end;
gethostbyaddr_tm(Addr,Timer) when list(Addr) ->
    case inet_parse:address(Addr) of
	{ok, IP} -> gethostbyaddr_tm(IP,Timer);
	_Error -> {error, formerr}
    end;
gethostbyaddr_tm(Addr,Timer) when atom(Addr) ->
    gethostbyaddr_tm(atom_to_list(Addr),Timer);
gethostbyaddr_tm(_,_) -> {error, formerr}.

%%
%%  Send the gethostbyaddr query to:
%%  1. the list of normal names servers
%%  2. the list of alternaive name servers
%%
res_gethostbyaddr(Addr, IP, Timer) ->
    case res_mkquery(Addr, in, ptr) of
	{ok, Id, Buffer} ->
	    case res_send2(Id, Buffer, res_option(nameserver),Timer) of
		{ok, Rec} ->
		    if length(Rec#dns_rec.anlist) == 0 ->
			    alt_gethostbyaddr(Id, Buffer, IP, 
					      {error, nxdomain}, Timer);
		       true ->
			    inet_db:res_gethostbyaddr(IP, Rec)
		    end;
		{error, nxdomain} ->
		    alt_gethostbyaddr(Id, Buffer, IP, 
				      {error, nxdomain}, Timer);
		Error -> Error
	    end;
	Error -> Error
    end.

alt_gethostbyaddr(Id, Buffer, IP, PrevError, Timer) ->
    case res_option(alt_nameserver) of
	[] ->
	    PrevError;
	AltNameserver ->
	    case res_send2(Id, Buffer, AltNameserver, Timer) of
		{ok, Rec} ->
		    inet_db:res_gethostbyaddr(IP, Rec);
		Error -> Error
	    end
    end.

%% --------------------------------------------------------------------------
%%
%% gethostbyname(domain_name()[,family [,Timer]) 
%%      => {ok, hostent()} | {error, Reason}
%%
%% where domain_name() is domain string or atom
%%
%% Caches the answer.
%% --------------------------------------------------------------------------

gethostbyname(Name) ->
    case res_option(inet6) of
	true ->
	    gethostbyname_tm(Name, inet6, false);
	false ->
	    gethostbyname_tm(Name, inet, false)
    end.

gethostbyname(Name,Family) ->
    gethostbyname_tm(Name,Family,false).

gethostbyname(Name,Family,Timeout) ->
    Timer = inet:start_timer(Timeout),    
    Res = gethostbyname_tm(Name,Family,Timer),
    inet:stop_timer(Timeout),
    Res.
    
gethostbyname_tm(Name,inet,Timer) ->
    getbyname_tm(Name,?S_A,Timer);
gethostbyname_tm(Name,inet6,Timer) ->
    case getbyname_tm(Name,?S_AAAA,Timer) of
	{ok,HEnt} -> {ok,HEnt};
	{error,nxdomain} ->
	    case getbyname_tm(Name, ?S_A,Timer) of
		{ok, HEnt} ->
		    %% rewrite to a ipv4 only ipv6 address
		    {ok,
		     HEnt#hostent {
		       h_addrtype = inet6,
		       h_length = 16,
		       h_addr_list = 
		       lists:map(
			 fun({A,B,C,D}) ->
				 {0,0,0,0,0,16#ffff,A*256+B,C*256+D}
			 end, HEnt#hostent.h_addr_list)
		      }};
		Error ->
		    Error
	    end;
	Error ->
	    Error
    end;
gethostbyname_tm(_Name, _Family, _Timer) ->
    {error, einval}.
	    
%% --------------------------------------------------------------------------
%%
%% getbyname(domain_name(), Type) => {ok, hostent()} | {error, Reason}
%%
%% where domain_name() is domain string or atom and Type is ?S_A, ?S_MX ...
%%
%% Caches the answer.
%% --------------------------------------------------------------------------

getbyname(Name, Type) -> 
    getbyname_tm(Name,Type,false).

getbyname(Name, Type, Timeout) ->
    Timer = inet:start_timer(Timeout),
    Res = getbyname_tm(Name, Type, Timer),
    inet:stop_timer(Timeout),
    Res.

getbyname_tm(Name, Type, Timer) when list(Name) ->
    case type_p(Type) of
	true ->
	    case inet_parse:visible_string(Name) of
		false -> {error, formerr};
		true ->
		    case inet_db:getbyname(Name,Type) of
			{ok, HEnt} -> {ok, HEnt};
			_ -> res_getbyname(Name,Type,Timer)
		    end
	    end;
	false ->
	    {error, formerr}
    end;
getbyname_tm(Name,Type,Timer) when atom(Name) ->
    getbyname_tm(atom_to_list(Name), Type,Timer);
getbyname_tm(_, _, _) -> {error, formerr}.

type_p(Type) ->
    lists:member(Type, [?S_A, ?S_AAAA, ?S_MX, ?S_NS,
		        ?S_MD, ?S_MF, ?S_CNAME, ?S_SOA,
		        ?S_MB, ?S_MG, ?S_MR, ?S_NULL,
		        ?S_WKS, ?S_HINFO, ?S_TXT, ?S_SRV,
		        ?S_UINFO, ?S_UID, ?S_GID]).
	    
res_getbyname(Name,Type,Timer) ->
    case dots(Name) of
	{0, Dot} ->
	    res_getby_search(Name, Dot,
			     get_searchlist(),nxdomain,Type,Timer);
	{_,Dot} ->
	    case res_getby_query_alt(Name,Type,Timer) of
		{error,_Reason} ->
		    res_getby_search(Name, Dot,
				     get_searchlist(),nxdomain,Type,Timer);
		Other -> Other
	    end
    end.

%% returns number of dots and a string with a dot or an empty string
%% {Numdots_except_last,Dotstring}
%% Dotstring indicates wether a "." should be added before addition
%% of more components
dots(Name) ->
    case inet_parse:dots(Name) of
	{N,false} -> {N,"."}; % no dot we must add one
	{N,true} -> {N,""}    % trailing dot , no need to add one
    end.

get_searchlist() ->
    case res_option(search) of
	[] -> [res_option(domain)];
	L -> L
    end.

res_getby_search(Name, Dot, [Dom | Ds], _Reason, Type, Timer) ->
    case res_getby_query(Name ++ Dot ++ Dom,res_option(nameserver),
			 Type,Timer) of
	{ok, HEnt} -> {ok, HEnt};
	{error, formerr} -> {error, formerr};
	{error, NewReason} -> res_getby_search(Name,Dot,Ds,NewReason,
					       Type,Timer)
    end;
res_getby_search(_Name, _, [], Reason,_,_) ->
    {error, Reason}.


%% Try both name server and alternative name server
res_getby_query_alt(Name,Type,Timer) ->
    case res_getby_query(Name, res_option(nameserver),Type,Timer) of
	{error, noanswer} -> alt_query(Name, noanswer,Type,Timer);
	{error, nxdomain} -> alt_query(Name, nxdomain,Type,Timer);
	Other -> Other
    end.

alt_query(Name, ErrReason,Type,Timer) ->
    case res_option(alt_nameserver) of
	[] -> {error, ErrReason};
	Ns -> res_getby_query(Name, Ns,Type,Timer)
    end.

res_getby_query(Name, Ns, Type, Timer) ->
    case res_mkquery(Name, in, Type) of
	{ok, Id, Buffer} ->
	    case res_send2(Id, Buffer, Ns,Timer) of
		{ok, Rec} ->
		    inet_db:res_hostent_by_domain(Name, Type, Rec);
		Error -> Error
	    end;
	Error -> Error
    end.

res_mkquery(Dname, Class, Type) ->
    ID = res_option(next_id),
    Recurse = res_option(recurse),
    Rec = #dns_rec { header = #dns_header { id = ID, 
					   opcode = ?QUERY,
					   rd = Recurse,
					   rcode = ?NOERROR },
		    qdlist = [ #dns_query {domain = Dname, 
					   type = Type, 
					   class = Class } ] },
    ?dbg("Query: ~p~n", [Rec]),
    case inet_dns:encode(Rec) of
	{ok, Buffer} -> {ok, ID, Buffer};
	Error -> Error
    end.

%%
%% Send a query to the nameserver and return a reply
%% We first use socket server then we add the udp version
%%
%% Algorithm: (from manual page for dig)
%%  for i = 0 to retry - 1
%%     for j = 1 to num_servers
%%           send_query
%%           wait((time * (2**i)) / num_servers)
%%     end
%%  end
%%

res_send2(Id, Buffer, Ns, Timer) ->
    UseVc = res_option(usevc),
    Retry = res_option(retry),
    Tm = res_option(timeout),
    if
	length(Buffer) > ?PACKETSZ ->
	    res_send_tcp2(Id, Buffer, Retry, Tm, Timer, Ns);
	UseVc == true ->
	    %% XXX This seems to be a bug. Timeout is not used here,
	    %% but it should probably be used to change the timer.
	    Timeout = inet:timeout(Tm*5, Timer),
	    res_send_tcp2(Id, Buffer, Retry, Tm, Timer, Ns);
	true ->
	    res_send_udp2(Id, Buffer, Retry, Tm, Timer, Ns)
    end.
    
res_send_udp2(Id, Buffer, Retry, Tm, Timer, Ns) ->
    case inet_udp:open(0, [{active,false}]) of
	{ok,S} ->
	    Res = res_send_udp(S, Id, Buffer, 0, Retry, Tm, Timer, Ns),
	    inet_udp:close(S),
	    Res;
	Error -> Error
    end.

res_send_udp(_S, _Id, _Buffer, N, N, _Tm, _Timer, _Ns) -> 
    {error, timeout};
res_send_udp(S, Id, Buffer, I, N, Tm, Timer, Ns) ->
    Num = length(Ns),
    if Num == 0 ->
	    {error, timeout};
       true ->
	    case res_send_query_udp(S,Id,Buffer,I,Num,Tm,Timer,Ns,[]) of
		{noanswer, ErrNs} -> %% remove unreachable nameservers
		    res_send_udp(S, Id, Buffer, I+1, N, Tm,Timer,Ns--ErrNs);
		Result ->
		    Result
	    end
    end.

res_send_query_udp(S, Id, Buffer, I, N, Tm, Timer, [{IP, Port}|Ns],ErrNs) ->
    Timeout = inet:timeout( (Tm * (1 bsl I)) div N, Timer),
    ?dbg("Try UDP server : ~p:~p (timeout=~w)\n", [IP, Port,Timeout]),
    inet_udp:connect(S, IP, Port),
    inet_udp:send(S, IP, Port, Buffer),
    case res_recv_reply_udp(S, IP, Port, Id, Timeout) of
	{ok, Rec} -> 
	    {ok, Rec};
	{error, nxdomain} ->
	    {error, nxdomain};
	{error, enetunreach} ->
	    res_send_query_udp(S, Id, Buffer, I, N, Tm,Timer, 
			       Ns, [{IP,Port}|ErrNs]);
	{error, econnrefused} -> 
	    res_send_query_udp(S, Id, Buffer, I, N, Tm, Timer, 
			       Ns, [{IP,Port}|ErrNs]);
	{error, timeout} when Timeout == 0 ->
	    {error, timeout};
	_Error -> res_send_query_udp(S, Id, Buffer, I, N, Tm, Timer,
				    Ns, ErrNs)
    end;
res_send_query_udp(_S, _Id, _Buffer, _I, _N, _Tm, _Timer, [], ErrNs) ->
    {noanswer, ErrNs}.


res_recv_reply_udp(S, IP, Port, Id, Timeout) ->
    case inet_udp:recv(S, 0, Timeout) of
	{ok, {IP, Port, Answer}} ->
	    case decode_answer(Answer, Id) of
		{error, badid} ->
		    res_recv_reply_udp(S, IP, Port, Id, Timeout);
		Reply -> Reply
	    end;
	{ok, _} -> res_recv_reply_udp(S, IP, Port, Id, Timeout);
	Error -> 
	    ?dbg("Udp server error: ~p\n", [Error]),
	    Error
    end.


res_send_tcp2(Id, Buffer, Retry, Tm, Timer, [{IP,Port}|Ns]) ->
    Timeout = inet:timeout(Tm*5, Timer),
    ?dbg("Try TCP server : ~p:~p (timeout=~w)\n", [IP, Port, Timeout]),
    case catch inet_tcp:connect(IP, Port, 
				[{active,false},{packet,2}], 
				Timeout) of
	     {ok, S} ->
		 inet_tcp:send(S, Buffer),
		 case inet_tcp:recv(S, 0, Timeout) of
		     {ok, Answer} ->
			 inet_tcp:close(S),
			 case decode_answer(Answer, Id) of
			     {ok,Rec} -> {ok, Rec};
			     {error, nxdomain} -> {error, nxdomain};
			     {error, fmt} -> {error, einval};
			     _Error ->
				 res_send_tcp2(Id, Buffer, Retry, Tm, Timer, Ns)
			 end;
		     _Error ->
			 inet_tcp:close(S),
			 res_send_tcp2(Id, Buffer, Retry, Tm, Timer, Ns)
		 end;
	       _Error -> 
		 res_send_tcp2(Id, Buffer, Retry, Tm, Timer, Ns)
	 end;
res_send_tcp2(_Id, _Buffer, _Retry, _Tm, _Timer, []) ->
    {error, timeout}.



decode_answer(Answer, Id) ->
    case inet_dns:decode(Answer) of
	{ok, Rec} ->
	    ?dbg("Got reply: ~p~n", [Rec]),
	    H = Rec#dns_rec.header,
	    case H#dns_header.rcode of
		?NOERROR ->
		    if H#dns_header.id =/= Id ->
			    {error, badid};
		       length(Rec#dns_rec.qdlist) =/= 1 ->
			    {error, noquery};
		       true ->
			    {ok, Rec}
		    end;
		?FORMERR  -> {error, qfmterror};
		?SERVFAIL -> {error, servfail};
		?NXDOMAIN -> {error, nxdomain};
		?REFUSED  -> {error, refused};
		_ -> {error, unknown}
	    end;
	Error ->
	    ?dbg("Got reply: ~p~n", [Error]),
	    Error
    end.

%%
%% Transform domain name or address
%% 1.  "a.b.c"    => 
%%       "a.b.c"
%% 2.  "1.2.3.4"  =>  
%%       "4.3.2.1.in-addr.arpa"
%% 3.  "4321:0:1:2:3:4:567:89ab" =>
%%      "b.a.9.8.7.6.5.0.4.0.0.0.3.0.0.0.2.0.0.0.1.0.0.0.0.0.0.1.2.3.4.IP6.INT"
%% 4.  {1,2,3,4} => as 2.
%% 5.  {1,2,3,4,5,6,7,8} => as 3.
%%
nsdname({A,B,C,D}) -> 
    {ok, dn_in_addr_arpa(A,B,C,D)};
nsdname({A,B,C,D,E,F,G,H}) -> 
    {ok, dn_ip6_int(A,B,C,D,E,F,G,H)};
nsdname(Name) when list(Name) ->
    case inet_parse:visible_string(Name) of
	true ->
	    case inet_parse:address(Name) of
		{ok, Addr} -> 
		    nsdname(Addr);
		_ ->
		    {ok, Name}
	    end;
	_ -> {error, formerr}
    end;
nsdname(_) -> {error, formerr}.


dn_ip6_int(A,B,C,D,E,F,G,H) ->
    dnib(H) ++ dnib(G) ++ dnib(F) ++ dnib(E) ++ 
	dnib(D) ++ dnib(C) ++ dnib(B) ++ dnib(A) ++ "IP6.INT".

dn_in_addr_arpa(A,B,C,D) ->
    integer_to_list(D) ++ "." ++
	integer_to_list(C) ++ "." ++
	integer_to_list(B) ++ "." ++
	integer_to_list(A) ++ ".IN-ADDR.ARPA".


dnib(X) ->
    [ hex(X), $., hex(X bsr 4), $., hex(X bsr 8), $., hex(X bsr 12), $.].

hex(X) ->
    X4 = (X band 16#f),
    if X4 < 10 -> X4 + $0;
       true -> (X4-10) + $a
    end.
