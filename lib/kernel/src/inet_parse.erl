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
-module(inet_parse).

%% Parser for all kinds of ineternet configuration files

-export([hosts/1, hosts/2]).
-export([hosts_vxworks/1]).
-export([protocols/1, protocols/2]).
-export([netmasks/1, netmasks/2]).
-export([networks/1, networks/2]).
-export([services/1, services/2]).
-export([rpc/1, rpc/2]).
-export([resolv/1, resolv/2]).
-export([host_conf_linux/1, host_conf_linux/2]).
-export([host_conf_freebsd/1, host_conf_freebsd/2]).
-export([host_conf_bsdos/1, host_conf_bsdos/2]).
-export([nsswitch_conf/1, nsswitch_conf/2]).

-export([ipv4_address/1, ipv6_address/1]).
-export([address/1]).
-export([visible_string/1, domain/1]).
-export([ntoa/1, dots/1]).
-export([split_line/1]).

-import(lists, [reverse/1]).

-record(fd, {port, number}).  %% XXX should include file_int.hrl ?

%% --------------------------------------------------------------------------
%% Parse services internet style
%% Syntax: 
%%      Name   Port/Protocol    [Aliases]  \n
%%      # comment
%% --------------------------------------------------------------------------

services(File) ->
    services(noname, File).

services(Fname, File) ->
    Fn = fun([Name, PortProto | Aliases]) ->
		 {Proto,Port} = port_proto(PortProto, 0),
		 {Name,Proto,Port,Aliases}
	 end,
    parse_file(Fname, File, Fn).

%% --------------------------------------------------------------------------
%% Parse rpc program names
%% Syntax:
%%      Name   Program  [Aliases]  \n |
%%      # comment
%% --------------------------------------------------------------------------

rpc(File) ->
    rpc(noname, File).

rpc(Fname, File) ->
    Fn = fun([Name,Program | Aliases]) ->
		 Prog = list_to_integer(Program),
		 {Name,Prog,Aliases}
	 end,
    parse_file(Fname, File, Fn).

%% --------------------------------------------------------------------------
%% Parse hosts file unix style
%% Syntax:
%%      IP Name [Aliases]  \n |
%%      # comment
%% --------------------------------------------------------------------------
hosts(File) ->
    hosts(noname,File).

hosts(Fname,File) ->
    Fn = fun([Address, Name | Aliases]) ->
		 {ok,IP} = address(Address),
		 {IP, Name, Aliases}
	 end,
    parse_file(Fname, File, Fn).

%% --------------------------------------------------------------------------
%% Parse hostShow vxworks style
%% Syntax:
%%      Name          IP                [Aliases]  \n 
%% --------------------------------------------------------------------------
hosts_vxworks(Hosts) ->
    Fn = fun([Name, Address | Aliases]) ->
		 {ok,IP} = address(Address),
		 {IP, Name, Aliases}
	 end,
    parse_file(Hosts, Fn).

%% --------------------------------------------------------------------------
%% Parse resolv file unix style
%% Syntax:
%%      domain Domain \n
%%      nameserver IP \n
%%      search Dom1 Dom2 ... \n
%%      lookup Method1 Method2 Method3 \n
%%      # comment
%% --------------------------------------------------------------------------

resolv(File) ->
    resolv(noname,File).

resolv(Fname, File) ->
    Fn = fun(["domain", Domain]) ->
		 {domain, Domain};
	    (["nameserver", Address]) ->
		 {ok,IP} = address(Address),
		 {nameserver,IP};
	    (["search" | List]) ->
		 {search, List};
	    (["lookup" | Types]) ->
		 {lookup, Types};
	    (_) ->
		 skip  %% there are too many local options, we MUST skip
	 end,
    parse_file(Fname, File, Fn).

%% --------------------------------------------------------------------------
%%
%% Parse Linux host.conf file
%% find "order" only.
%%
%% --------------------------------------------------------------------------
host_conf_linux(File) ->
    host_conf_linux(noname,File).

host_conf_linux(Fname, File) ->
    Fn = fun(["order" | Order]) ->
		 %% XXX remove ',' between entries
		 {lookup, split_comma(Order)}; 
	    (_) ->
		 skip
	 end,
    parse_file(Fname, File, Fn).

%% --------------------------------------------------------------------------
%%
%% Parse Freebsd/Netbsd host.conf file
%% find "order" only.
%%
%% --------------------------------------------------------------------------
host_conf_freebsd(File) ->
    host_conf_freebsd(noname,File).

host_conf_freebsd(Fname, File) ->
    Fn = fun([Type]) -> Type end,
    case parse_file(Fname, File, Fn) of
	{ok, Ls} -> {ok, [{lookup, Ls}]};
	Error -> Error
    end.



%% --------------------------------------------------------------------------
%%
%% Parse BSD/OS irs.conf file
%% find "hosts" only and ignore options.
%%
%% Syntax: 
%%      Map AccessMethod [,AccessMethod] [continue|merge [,merge|,continue]] \n
%%      # comment

%% --------------------------------------------------------------------------
host_conf_bsdos(File) ->
    host_conf_bsdos(noname,File).

host_conf_bsdos(Fname, File) ->
    Fn = fun(["hosts" | List]) ->
		 delete_options(split_comma(List));
	    (_) ->
		 skip
	 end,
    case parse_file(Fname, File, Fn) of
	{ok, Ls} ->
	    {ok, [{lookup, lists:append(Ls)}]};
	Error -> Error
    end.

delete_options(["continue"|T]) ->
    delete_options(T);
delete_options(["merge"|T]) ->
    delete_options(T);
delete_options([H|T]) ->
    [H|delete_options(T)];
delete_options([]) ->
    [].


%% --------------------------------------------------------------------------
%%
%% Parse Solaris nsswitch.conf
%% find "hosts:" only
%%
%% --------------------------------------------------------------------------

nsswitch_conf(File) ->
    nsswitch_conf(noname,File).

nsswitch_conf(Fname, File) ->
    Fn = fun(["hosts:" | Types]) ->
		 {lookup, Types};
	    (_) -> skip
	 end,
    parse_file(Fname, File, Fn).

%% --------------------------------------------------------------------------
%% Parse protocol file unix style
%% Syntax:
%%      name protocol number name \n
%%      # comment
%% --------------------------------------------------------------------------

protocols(File) ->
    protocols(noname,File).

protocols(Fname, File) ->
    Fn = fun([Name, Number, DName]) ->
		 {list_to_atom(Name), list_to_integer(Number), DName}
	 end,
    parse_file(Fname, File, Fn).

%% --------------------------------------------------------------------------
%% Parse netmasks file unix style
%% Syntax:
%%      Network  Subnetmask
%%      # comment
%% --------------------------------------------------------------------------

netmasks(File) ->
    netmasks(noname, File).

netmasks(Fname, File) ->
    Fn = fun([Net, Subnetmask]) ->
		 {ok, NetIP} = address(Net),
		 {ok, Mask} =  address(Subnetmask),
		 {NetIP, Mask}
	 end,
    parse_file(Fname, File, Fn).

%% --------------------------------------------------------------------------
%% Parse networks file unix style
%% Syntax:
%%      network-name  network-number aliases ...
%%      # comment
%% --------------------------------------------------------------------------

networks(File) ->
    networks(noname, File).

networks(Fname, File) ->
    Fn = fun([NetName, NetNumber]) ->
		 Number = list_to_integer(NetNumber),
		 {NetName, Number}
	 end,
    parse_file(Fname, File, Fn).

%% --------------------------------------------------------------------------
%%
%% Simple Line by Line parser
%%
%% --------------------------------------------------------------------------

parse_file(File, Fn) ->
    parse_file(noname, File, Fn).

parse_file(Fname, {fd,Fd}, Fn) ->
    parse_fd(Fname,Fd, 1, Fn, []);
parse_file(Fname, {chars,Cs}, Fn) when list(Cs) ->
    parse_cs(Fname, Cs, 1, Fn, []);
parse_file(Fname, {chars,Cs}, Fn) when binary(Cs) ->
    parse_cs(Fname, binary_to_list(Cs), 1, Fn, []);
parse_file(_, File, Fn) ->
    case file:open(File, [read]) of
	{ok, Fd} ->
	    Result = parse_fd(File,Fd, 1, Fn, []),
	    file:close(Fd),
	    Result;
	Error -> Error
    end.

parse_fd(Fname,Fd, Line, Fun, Ls) ->
    case read_line(Fd) of
	eof -> {ok, reverse(Ls)};
	Cs ->
	    case split_line(Cs) of
		[] -> parse_fd(Fname, Fd, Line+1, Fun, Ls);
		Toks ->
		    case catch Fun(Toks) of
			{'EXIT',_} ->
			    error("~p:~p: erroneous line, SKIPPED~n",[Fname,Line]),
			    parse_fd(Fname, Fd,Line+1,Fun,Ls);
			{warning,Wlist,Val} ->
			    warning("~p:~p: warning! strange domain name(s) ~p ~n",[Fname,Line,Wlist]),
			    parse_fd(Fname, Fd,Line+1,Fun,[Val|Ls]);

			skip -> 
			    parse_fd(Fname, Fd, Line+1, Fun, Ls);
			Val -> parse_fd(Fname, Fd, Line+1, Fun, [Val|Ls])
		    end
	    end
    end.

parse_cs(Fname, Chars, Line, Fun, Ls) ->
    case get_line(Chars) of
	eof -> {ok, reverse(Ls)};
	{Cs,Chars1} ->
	    case split_line(Cs) of
		[] -> parse_cs(Fname, Chars1, Line+1, Fun, Ls);
		Toks ->
		    case catch Fun(Toks) of
			{'EXIT',_} -> 
			    error("~p:~p: erroneous line, SKIPPED~n",[Fname,Line]),
 			    parse_cs(Fname, Chars1, Line+1, Fun, Ls);
			{warning,Wlist,Val} ->
			    warning("~p:~p: warning! strange domain name(s) ~p ~n",[Fname,Line,Wlist]),
			    parse_cs(Fname, Chars1, Line+1, Fun, [Val|Ls]);

			skip -> parse_cs(Fname, Chars1, Line+1, Fun, Ls);
			Val -> parse_cs(Fname, Chars1, Line+1, Fun, [Val|Ls])
		    end
	    end
    end.

get_line([]) -> eof;
get_line(Chars) -> get_line(Chars,[]).

get_line([], Acc) -> {reverse(Acc), []};
get_line([$\r, $\n | Cs], Acc) -> {reverse([$\n|Acc]), Cs};
get_line([$\n | Cs], Acc) -> {reverse([$\n|Acc]), Cs};
get_line([C | Cs], Acc) -> get_line(Cs, [C|Acc]).

%%
%% Read a line
%%
read_line(Fd) when pid(Fd) -> io:get_line(Fd, '');
read_line(Fd) when record(Fd, fd) ->
    collect_line(Fd, []).

collect_line(Fd, Cs) ->
    case file:read(Fd, 80) of
	{ok, Line} when binary(Line) ->
	    collect_line(Fd, size(Line), binary_to_list(Line), Cs);
	{ok, Line} ->
	    collect_line(Fd, length(Line), Line, Cs);
	eof when Cs == [] ->
	    eof;
	eof -> reverse(Cs)
    end.    

collect_line(Fd, N, [$\r, $\n|_], Cs) ->
    file:position(Fd, {cur,-(N-2)}),
    reverse([$\n|Cs]);
collect_line(Fd, N, [$\n|_], Cs) ->
    file:position(Fd, {cur,-(N-1)}),
    reverse([$\n|Cs]);
collect_line(Fd, _, [], Cs) ->
    collect_line(Fd, Cs);
collect_line(Fd, N, [X|Xs], Cs) ->
    collect_line(Fd, N-1, Xs, [X|Cs]).


%% split Port/Proto -> {Port, Proto}
port_proto([X|Xs], N) when X >= $0, X =< $9 -> 
    port_proto(Xs, N*10 + (X - $0));
port_proto([$/ | Proto], Port) when Port =/= 0 -> 
    {list_to_atom(Proto), Port}.

%%
%% Check if a String is a string with visible characters #21..#7E
%% visible_string(String) -> Bool
%%
visible_string([H|T]) ->
    is_vis1([H|T]);
visible_string(_) ->
    false.

is_vis1([C | Cs]) when C >= 16#21, C =< 16#7e -> is_vis1(Cs);
is_vis1([]) -> true;
is_vis1(_) -> false.

%%
%% Check if a String is a domain name according to RFC XXX.
%% domain(String) -> Bool
%%
domain([H|T]) ->
    is_dom1([H|T]);
domain(_) ->
    false.

is_dom1([C | Cs]) when C >= $a, C =< $z -> is_dom_ldh(Cs);
is_dom1([C | Cs]) when C >= $A, C =< $Z -> is_dom_ldh(Cs);
is_dom1([C | Cs]) when C >= $0, C =< $9 -> 
    case is_dom_ldh(Cs) of
	true  -> is_dom2(string:tokens([C | Cs],"."));
	false -> false
    end;
is_dom1(_) -> false.

is_dom_ldh([C | Cs]) when C >= $a, C =< $z -> is_dom_ldh(Cs);
is_dom_ldh([C | Cs]) when C >= $A, C =< $Z -> is_dom_ldh(Cs);
is_dom_ldh([C | Cs]) when C >= $0, C =< $9 -> is_dom_ldh(Cs);
is_dom_ldh([$-,$. | _]) -> false;
is_dom_ldh([$_,$. | _]) -> false;
is_dom_ldh([$_ | Cs]) -> is_dom_ldh(Cs);
is_dom_ldh([$- | Cs]) -> is_dom_ldh(Cs);
is_dom_ldh([$. | Cs]) -> is_dom1(Cs);
is_dom_ldh([]) -> true;
is_dom_ldh(_) -> false.

%%% Check that we don't get a IP-address as a domain name.

-define(L2I(L), (catch list_to_integer(L))).

is_dom2([A,B,C,D]) ->
    case ?L2I(D) of
	Di when integer(Di) ->
	    case {?L2I(A),?L2I(B),?L2I(C)} of
		{Ai,Bi,Ci} when integer(Ai),integer(Bi),integer(Ci) -> false;
		_ -> true
	    end;
	_ -> true
    end;
is_dom2(_) ->
    true.



%%
%% Test ipv4 address or ipv6 address
%% Return {ok, Address} | {error, Reason}
%%
address(Cs) when list(Cs) ->
    case ipv4_address(Cs) of
	{ok,IP} -> {ok,IP};
	_ ->
	    case ipv6_address(Cs) of
		{ok, IP} -> {ok, IP};
		Error -> Error
	    end
    end;
address(_) -> 
    {error, einval}.

%%
%% Parse Ipv4 address: d1.d2.d3.d4
%%
%% Return {ok, IP} | {error, einval}
%%
ipv4_address(Cs) ->
    case catch ipv4_addr(Cs, []) of
	{'EXIT',_} -> {error,einval};
	Addr -> {ok,Addr}
    end.

ipv4_addr([C | Cs], IP) when C >= $0, C =< $9 -> ipv4_addr(Cs, C-$0, IP).

ipv4_addr([$.|Cs], N, IP) when N < 256 -> ipv4_addr(Cs, [N|IP]);
ipv4_addr([C|Cs], N, IP) when C >= $0, C =< $9 ->
    ipv4_addr(Cs, N*10 + (C-$0), IP);
ipv4_addr([], D, [C,B,A]) when D < 256 -> {A,B,C,D}.

%%
%% ipv6 addresses:
%%
%% Syntax:
%% addr ->
%%       :: addr2
%%       addr1
%%
%% addr1 ->
%%       x4  : addr1
%%       x4 :: addr2
%%       d3  . addr3
%%       x4
%%
%% addr2 ->
%%       x4 : addr2
%%       d3 . addr3
%%       x4
%%       empty
%%
%% addr3 ->
%%       d3 . addr3
%%       d3
%%
%%  return {ok, Addr} | {error, einval}
%%
%%
ipv6_address([$:,$: | Cs]) ->
    case catch ipv6_addr2(Cs, [], []) of
	{'EXIT',_} -> {error, einval};
	Addr -> {ok, Addr}
    end;
ipv6_address(Cs) ->
    case catch ipv6_addr1(Cs, []) of
	{'EXIT',_} -> {error, einval};
	Addr -> {ok, Addr}
    end.

ipv6_addr1(Cs, A0) ->
    case x4(Cs) of
	{_,Ds,[$:,$: | Cs1]} -> ipv6_addr2(Cs1,[],[tox(Ds) | A0]);
	{_,Ds,[$: | Cs1]}    -> ipv6_addr1(Cs1,[tox(Ds)|A0]);
	{_,Ds,[$. | Cs1]}    -> ipv6_addr3(Cs1,[tod(Ds)],A0,[],false);
	{_,Ds,[]}            -> mkaddr([tox(Ds)|A0], [], [], false)
    end.

ipv6_addr2([],A0,A2) ->
    mkaddr(A2, A0, [], true);
ipv6_addr2(Cs,A0,A2) ->
    case x4(Cs) of
	{_,Ds,[$:|Cs1]}   -> ipv6_addr2(Cs1,[tox(Ds)|A0],A2);
	{_,Ds,[]}         -> mkaddr(A2, [tox(Ds)|A0],[],true);
	{dec,Ds,[$.|Cs1]} -> ipv6_addr3(Cs1,[tod(Ds)],A2,A0,true)
    end.

ipv6_addr3(Cs,A0,A2,A3,Expand) ->
    case x4(Cs) of
	{dec,Ds,[$.|Cs1]} -> ipv6_addr3(Cs1,[tod(Ds)|A0],A2,A3,Expand);
	{dec,Ds,[]} -> mkaddr(A2,A3,[tod(Ds)|A0],Expand)
    end.

x4(Cs) ->
    x4(Cs, 4, [], dec).

x4(Cs, 0, Ds, Type) -> {Type,Ds,Cs};
x4([C|Cs],N,Ds,Type) ->
    if
	C >= $0, C =< $9 -> x4(Cs,N-1,[(C-$0)|Ds],Type);
	C >= $a, C =< $f -> x4(Cs,N-1,[((C-$a)+10)|Ds],Type);
	C >= $A, C =< $F -> x4(Cs,N-1,[((C-$A)+10)|Ds],hex);
	true -> {Type, ds4(Ds), [C|Cs]}
    end;
x4([],N,Ds,Type) ->
    {Type, ds4(Ds), []}.

tox([X0,X1,X2,X3]) -> X0 + (X1 bsl 4) + (X2 bsl 8) + (X3 bsl 12).

tod([D0,D1,D2,0]) ->
    D = D0 + D1*10 + D2*100,
    if D =< 255 -> D end.

%% mkaddr([], [], [D,C,B,A], false) ->  (add this clause to support old ipv4)
%%    {A,B,C,D};
mkaddr(E, F, [], false) when length(E) + length(F) == 8 ->
    list_to_tuple(reverse(E) ++ reverse(F));
mkaddr(E, F, [D,C,B,A], false) when length(E) + length(F) == 6 ->
    list_to_tuple(reverse(E) ++ reverse(F) ++ [A*256+B, C*256+D]);
mkaddr(E, F, [], true) when length(E)+length(F) =< 8 ->
    list_to_tuple(reverse(E) ++ zero(8-(length(E)+length(F))) ++ reverse(F));
mkaddr(E, F, [D,C,B,A], true) when length(E)+length(F) =< 6 -> 
    list_to_tuple(reverse(E) ++ zero(6-(length(E)+length(F))) ++ reverse(F) ++
		  [A*256+B, C*256+D]).

ds4([D0]) -> [D0,0,0,0];
ds4([D0,D1]) -> [D0,D1,0,0];
ds4([D0,D1,D2]) -> [D0,D1,D2,0].

zero(0) -> [];
zero(N) -> [0 | zero(N-1)].


%% Convert IPv4 adress to ascii
%% Convert IPv6 / IPV4 adress to ascii (plain format)
ntoa({A,B,C,D}) ->
    integer_to_list(A) ++ "." ++ integer_to_list(B) ++ "." ++ 
	integer_to_list(C) ++ "." ++ integer_to_list(D);
%% ANY
ntoa({0,0,0,0,0,0,0,0}) -> "::";
%% LOOPBACK
ntoa({0,0,0,0,0,0,0,1}) -> "::1";
%% IPV4 ipv6 host address
ntoa({0,0,0,0,0,0,A,B}) -> "::" ++ dig_to_dec(A) ++ "." ++ dig_to_dec(B);
%% IPV4 non ipv6 host address
ntoa({0,0,0,0,0,16#ffff,A,B}) -> 
    "::FFFF:" ++ dig_to_dec(A) ++ "." ++ dig_to_dec(B);
%% general XXX fix :: when possible
ntoa({A,B,C,D,E,F,G,H}) ->
    dig_to_hex(A) ++ ":" ++
	dig_to_hex(B) ++ ":" ++
	dig_to_hex(C) ++ ":" ++
	dig_to_hex(D) ++ ":" ++
	dig_to_hex(E) ++ ":" ++
	dig_to_hex(F) ++ ":" ++
	dig_to_hex(G) ++ ":" ++
	dig_to_hex(H).

%% convert to A.B decimal form
dig_to_dec(0) -> [$0,$.,$0];
dig_to_dec(X) -> 
    integer_to_list((X bsr 8) band 16#ff) ++ "." ++
	integer_to_list(X band 16#ff).

%% Convert a integer to hex string
dig_to_hex(X) ->
    shex(hex(X bsr 12), hex(X bsr 8), hex(X bsr 4), hex(X)).

% short hex form
shex($0,$0,$0,D) -> [D];
shex($0,$0,C,D) -> [C,D];
shex($0,B,C,D) -> [B,C,D];
shex(A,B,C,D) -> [A,B,C,D].

hex(X) ->
    X4 = (X band 16#f),
    if X4 < 10 -> X4 + $0;
       true -> (X4-10) + $A
    end.

%%
%% Count number of '.' in a name
%% return {Number of non-terminating dots, has-terminating dot?}
%%        {integer, bool}
%%
dots(Name) -> dots(Name, 0).

dots([$.], N) -> {N, true};
dots([$. | T], N) -> dots(T, N+1);
dots([C | T], N) -> dots(T, N);
dots([], N) -> {N, false}.


split_line(Line) ->
    split_line(Line, []).

split_line([$# | Xs], Tokens) -> reverse(Tokens);
split_line([$\s| L], Tokens) ->   split_line(L, Tokens);
split_line([$\t | L], Tokens) -> split_line(L, Tokens);
split_line([$\n | L], Tokens) -> split_line(L, Tokens);
split_line([], Tokens) -> reverse(Tokens);
split_line([C|Cs], Tokens) -> split_mid(Cs, [C], Tokens).

split_mid([$# | Cs], Acc, Tokens) -> split_end(Acc, Tokens);
split_mid([$\s | Cs], Acc, Tokens) -> split_line(Cs, [reverse(Acc) | Tokens]);
split_mid([$\t | Cs], Acc, Tokens) -> split_line(Cs, [reverse(Acc) | Tokens]);
split_mid([$\r, $\n | Cs], Acc, Tokens) -> split_line(Cs, [reverse(Acc) | Tokens]);
split_mid([$\n | Cs], Acc, Tokens) -> split_line(Cs, [reverse(Acc) | Tokens]);
split_mid([], Acc, Tokens) -> split_end(Acc, Tokens);
split_mid([C|Cs], Acc, Tokens) -> split_mid(Cs, [C|Acc], Tokens).

split_end([], Tokens) -> reverse(Tokens);
split_end(Acc, Tokens) -> reverse([reverse(Acc) | Tokens]).


%% Split a comma separated tokens. Because we already have split on
%% spaces we may have the cases
%%
%%        ",foo"
%%        "foo,"
%%        "foo,bar..."
 
split_comma([]) ->
    [];
split_comma([Token | Tokens]) ->
    split_comma(Token, []) ++ split_comma(Tokens).
 
split_comma([], Tokens) ->       reverse(Tokens);
split_comma([$, | L], Tokens) -> split_comma(L, Tokens);
split_comma([C|Cs], Tokens) ->   split_mid_comma(Cs, [C], Tokens).
 
split_mid_comma([$, | Cs], Acc, Tokens) ->
    split_comma(Cs, [reverse(Acc) | Tokens]);
split_mid_comma([], Acc, Tokens) ->
    split_end(Acc, Tokens);
split_mid_comma([C|Cs], Acc, Tokens) ->
    split_mid_comma(Cs, [C|Acc], Tokens).

%%

warning(Fmt, Args) ->
    case application:get_env(kernel,inet_warnings) of
	{ok,on} -> 
	    error_logger:info_msg("inet_parse:" ++ Fmt, Args);
	_ ->
	    ok
    end.

error(Fmt, Args) ->
    error_logger:info_msg("inet_parse:" ++ Fmt, Args).

