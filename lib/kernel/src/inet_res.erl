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

%% DNS resolver 

-export([gethostbyname/1, gethostbyname/2]).
-export([gethostbyaddr/1]).
-export([nslookup/3, nslookup/4]).

-import(inet_db, [res_option/1]).
-import(lists, [foreach/2]).

-include("inet.hrl").
-include("inet_res.hrl").
-include("inet_dns.hrl").

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
nslookup(Name, Class, Type, Ns) ->
    case nsdname(Name) of
	{ok, Nm} ->
	    case res_mkquery(Nm, Class, Type) of
		{ok, Id, Buffer} ->
		    res_send(Id, Buffer, Ns);
		Error -> Error
	    end;
	Error -> Error
    end.
    
%% Perform nslookup on standard config !!    
nslookup(Name, Class, Type) ->
    case nsdname(Name) of
	{ok, Nm} ->
	    case res_mkquery(Nm, Class, Type) of
		{ok, Id, Buffer} ->
		    ns_send(Id, Buffer, res_option(nameserver));
		Error -> Error
	    end;
	Error -> Error
    end.

ns_send(Id, Buffer, Ns) ->
    case res_send(Id, Buffer, Ns) of
	{ok, Rec} ->
	    if length(Rec#dns_rec.anlist) == 0 ->
		    alt_ns_send(Id, Buffer, nxdomain,
				res_option(alt_nameserver));
	       true ->
		    {ok,Rec}
	    end;
	{error, nxdomain} ->
	    alt_ns_send(Id, Buffer, nxdomain, res_option(alt_nameserver));
	Error -> Error
    end.

alt_ns_send(Id, Buffer, ErrReason, []) -> 
    {error, ErrReason};
alt_ns_send(Id, Buffer, _, Ns) ->
    res_send(Id, Buffer, Ns).

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

gethostbyaddr({A,B,C,D}) when integer(A+B+C+D) ->
    IP = {A,B,C,D},
    case inet_db:gethostbyaddr(IP) of
	{ok, HEnt} -> {ok, HEnt};
	_ -> res_gethostbyaddr(dn_in_addr_arpa(A,B,C,D), IP)
    end;
%% ipv4  only ipv6 address
gethostbyaddr({0,0,0,0,0,16#ffff,G,H}) when integer(G+H) ->
    gethostbyaddr({G div 256, G rem 256, H div 256, H rem 256});
gethostbyaddr({A,B,C,D,E,F,G,H}) when integer(A+B+C+D+E+F+G+H) ->
    IP = {A,B,C,D,E,F,G,H},
    case inet_db:gethostbyaddr(IP) of
	{ok, HEnt} -> {ok, HEnt};
	_ -> res_gethostbyaddr(dn_ip6_int(A,B,C,D,E,F,G,H), IP)
    end;
gethostbyaddr(Addr) when list(Addr) ->
    case inet_parse:address(Addr) of
	{ok, IP} -> gethostbyaddr(IP);
	Error -> {error, formerr}
    end;
gethostbyaddr(Addr) when atom(Addr) ->
    gethostbyaddr(atom_to_list(Addr));
gethostbyaddr(_) -> {error, formerr}.

%%
%%  Send the gethostbyaddr query to:
%%  1. the list of normal names servers
%%  2. the list of alternaive name servers
%%
res_gethostbyaddr(Addr, IP) ->
    case res_mkquery(Addr, in, ptr) of
	{ok, Id, Buffer} ->
	    case res_send(Id, Buffer, res_option(nameserver)) of
		{ok, Rec} ->
		    if length(Rec#dns_rec.anlist) == 0 ->
			    alt_gethostbyaddr(Id, Buffer, IP);
		       true ->
			    NewAnlist = lists:foldl(
					  fun (RR,Ack) 
					      when RR#dns_rr.type =:= ptr ->
						  res_gethostbyname
						    (RR#dns_rr.data,inet),
						  Ack;
					      (RR,Ack) -> [RR | Ack]
					  end,
					  [],
					  Rec#dns_rec.anlist),
			    res_cache_answer(Rec#dns_rec{anlist = NewAnlist}),
			    inet_db:gethostbyaddr(IP)
		    end;
		{error, nxdomain} ->
		    alt_gethostbyaddr(Id, Buffer, IP);
		Error -> Error
	    end;
	Error -> Error
    end.

alt_gethostbyaddr(Id, Buffer, IP) ->
    case res_send(Id, Buffer, res_option(alt_nameserver)) of
	{ok, Rec} ->
	    res_cache_answer(Rec),
	    inet_db:gethostbyaddr(IP);
	Error -> Error
    end.

%% --------------------------------------------------------------------------
%%
%% gethostbyname(domain_name()) => {ok, hostent()} | {error, Reason}
%%
%% where domain_name() is domain string or atom
%%
%% --------------------------------------------------------------------------

gethostbyname(Name) ->
    gethostbyname(Name, inet).

gethostbyname(Name,Type) when list(Name) ->
    case inet_parse:visible_string(Name) of
	false -> {error, formerr};
	true ->
	    case inet_db:gethostbyname(Name,Type) of
		{ok, HEnt} -> {ok, HEnt};
		_ -> res_gethostbyname(Name,Type)
	    end
    end;
gethostbyname(Name,Type) when atom(Name) ->
    gethostbyname(atom_to_list(Name),Type);
gethostbyname(_,_) -> {error, formerr}.

	    
res_gethostbyname(Name,Type) ->
    case dots(Name) of
	{0, Dot} ->
	    res_gethostby_search(Name, Dot,
				 get_searchlist(),nxdomain,Type);
	{_,Dot} ->
	    case res_gethostby_query_alt(Name,Type) of
		{error,Reason} ->
		    res_gethostby_search(Name, Dot,
					 get_searchlist(),nxdomain,Type);
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

res_gethostby_search(Name, Dot, [Dom | Ds], Reason,Type) ->
    case res_gethostby_query(Name ++ Dot ++ Dom,res_option(nameserver),Type) of
	{ok, HEnt} -> {ok, HEnt};
	{error, formerr} -> {error, formerr};
	{error, NewReason} -> res_gethostby_search(Name,Dot,Ds,NewReason,Type)
    end;
res_gethostby_search(Name, _, [], Reason,_) ->
    {error, Reason}.


%% Try both name server and alternative name server
res_gethostby_query_alt(Name,Type) ->
    case res_gethostby_query(Name, res_option(nameserver),Type) of
	{error, noanswer} -> alt_query(Name, noanswer,Type);
	{error, nxdomain} -> alt_query(Name, nxdomain,Type);
	Other -> Other
    end.

alt_query(Name, ErrReason,Type) ->
    case res_option(alt_nameserver) of
	[] -> {error, ErrReason};
	Ns -> res_gethostby_query(Name, Ns,Type)
    end.

res_gethostby_query(Name, Ns, Type) ->
    case res_mkquery(Name, in, if Type == inet6 -> aaaa; true -> a end) of
	{ok, Id, Buffer} ->
	    case res_send(Id, Buffer, Ns) of
		{ok, Rec} ->
		    res_cache_answer(Rec),
		    inet_db:gethostbyname(Name,Type);
		Error -> Error
	    end;
	Error -> Error
    end.

%%
%% res_getanswer should caches all answer resource records
%% in the inet_db. Then it tries to resolve the question from
%% the cache
%%
res_cache_answer(Rec) ->
    foreach( fun(RR) -> inet_db:add_rr(RR) end, Rec#dns_rec.anlist).


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

%% This function is currently not used
%%res_mkiquery(?IQUERY, Address, Class, Type) ->
%%    ID = res_option(next_id),
%%    Recurse = res_option(recurse),
%%    Rec = #dns_rec  { header =  #dns_header { id = ID, opcode = ?IQUERY, 
%%					     rd = Recurse,
%%					     rcode = ?NOERROR },
%%		     anlist = [#dns_rr { domain = "",
%%					type = Type,
%%					class = Class,
%%					data = Address }] },
%%    ?dbg("IQuery: ~p~n", [Rec]),
%%    case inet_dns:encode(Rec) of
%%	{ok, Buffer} -> {ok, ID, Buffer};
%%	Error -> Error
%%    end.

%%
%% Send a query to the nameserver and return a reply
%% We first use socket server then we add the udp version
%%
res_send(Id, Buffer, Ns) ->
    res_send(Id, Buffer, Ns, nxdomain).

res_send(Id, Buffer, [], ErrReason) ->
    {error, ErrReason};
res_send(Id, Buffer, [NameServer | Ns], ErrReason) ->
    UseVc = res_option(usevc),
    Res = if
	      length(Buffer) > ?PACKETSZ ->
		  res_send_tcp(Id, Buffer, NameServer);
	      UseVc == true ->
		  res_send_tcp(Id, Buffer, NameServer);
	      true ->
		  res_send_udp(Id, Buffer, NameServer)
	  end,
    case Res of
	{error, nxdomain} ->
	    Res;
	{error, Reason} ->
	    res_send(Id, Buffer, Ns, Reason);
	{ok, Rec} ->
	    H = Rec#dns_rec.header,
	    if
		H#dns_header.rcode == ?NOERROR -> 
		    {ok, Rec};
		true ->
		    res_send(Id, Buffer, Ns, ErrReason)
	    end
    end.



res_send_udp(Id, Buffer, {IP,Port}) ->
    Tm = res_option(timeout),
    ReTry = res_option(retry),
    case catch inet_udp:open(0, []) of
	{ok, S} ->
	    ?dbg("Try UDP server : ~p:~p~n",[IP, Port]),
	    Res = try_udp(S, IP, Port, Id, Buffer, ReTry, Tm),
	    inet_udp:close(S),
	    Res;
	Error ->
	    Error
    end.

try_udp(S, IP, Port, Id, Buffer, 0, Tm) -> 
    {error, timeout};
try_udp(S, IP, Port, Id, Buffer, Count, Tm) ->
    inet_udp:send(S, IP, Port, Buffer),
    case receive_udp(S, IP, Port, Id, Tm) of
	{ok, Res} -> {ok, Res};
	{error, timeout} -> 
	    ?dbg("Retry UDP~n", []),
	    try_udp(S, IP, Port, Id, Buffer, Count-1, Tm*2);
	Error -> Error
    end.

%% receive udp answer
receive_udp(S, IP, Port, Id, Tm) ->
    receive
	{udp, S, IP, Port, Answer} ->
	    case decode_answer(Answer, Id) of
		{error, badid} -> receive_udp(S, IP, Port, Id, Tm);
		Other -> Other
	    end
    after Tm ->
	    ?dbg("Timeout~n", []),
	    {error, timeout}
    end.

res_send_tcp(Id, Buffer, {IP,Port}) ->
    ?dbg("Try TCP server : ~p:~p~n", [IP, Port]),
    Tm = res_option(timeout)*5,
    case catch inet_tcp:connect(IP, Port, [{packet,2}]) of
	{ok, S} ->
	    inet_tcp:send(S, Buffer),
	    receive
		{tcp, S, Answer} ->
		    inet_tcp:close(S),
		    decode_answer(Answer, Id);
		{tcp_closed, S} ->
		    {error, unknown}
	    after Tm ->
		    ?dbg("Timeout~n", []),
		    inet_tcp:close(S),
		    {error, timeout}
	    end;
	Error -> Error
    end.

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
