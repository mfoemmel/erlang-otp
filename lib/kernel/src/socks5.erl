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
-module(socks5).

%% Socks5 general interface

-include("inet_int.hrl").
-include("socks5.hrl").

-export([open/0, open/1, open/2, associate/3, 
	 connect/3, connect/4, bind/3, 
	 accept/1, accept/2]).
-export([is_direct/1]).

%%
%% Open a connection to socks server
%% Use for assoicate, bind or connect
%%
open() ->
    do_open([], infinity).

open(Opts) ->
    do_open(Opts, infinity).

open(Opts, infinity) -> 
    do_open(Opts, infinity);
open(Opts, TimeOut) when integer(TimeOut), TimeOut >= 0 ->
    do_open(Opts, TimeOut).

do_open(Opts, TimeOut) ->
    case inet_db:socks_option(server) of
	[] -> {error, "no socks5 server"};
	Server ->
	    Port = inet_db:socks_option(port),
	    case inet_tcp:connect(
		   Server, Port,
		   Opts ++ [{active,false},{header,-1}], TimeOut) of
		{ok, S} -> auth(S);
		Error -> Error
	    end;
	Error -> Error
    end.

auth(S) ->
    Methods = inet_db:socks_option(methods),
    Ms = encode_methods(Methods),
    inet_tcp:send(S, [?SOCKS5_VER, length(Methods) | Ms]),
    case inet_tcp:recv(S, 2) of
	{ok, [?SOCKS5_VER, M]} ->
	    socks5_auth:negotiate(S, decode_method(M));
	_ ->
	    {error, unknown}
    end.

%%
%% Associate socks with Udp IP and port
%%
associate(S, IP, Port) when integer(Port) -> 
    case enc_address(IP) of
	{ok, Address} ->
	    cmd(S, ?SOCKS5_REQ_UDP_ASSOC, Address, Port, infinity);
	Error  -> Error
    end.
%%
%% Connect to remote address
%%
connect(S, IP, Port) when integer(Port) -> 
    do_connect(S, IP, Port, infinity).

connect(S, IP, Port,infinity) when integer(Port) -> 
    do_connect(S, IP, Port, infinity);
connect(S, IP, Port, Time) when integer(Port),integer(Time),Time >=0 -> 
    do_connect(S, IP, Port, Time).

do_connect(S, IP, Port, Time) -> 
    case enc_address(IP) of
	{ok, Address} ->
	    cmd(S, ?SOCKS5_REQ_CONNECT, Address, Port, Time);
	Error -> Error
    end.

%% Bind port on socks server    
bind(S, IP, Port) when integer(Port) ->
    case enc_address(IP) of
	{ok, Address} ->
	    cmd(S, ?SOCKS5_REQ_BIND, Address, Port, infinity);
	Error -> Error
    end.

%%
%% Accept on listning socket
%%
accept(S) ->
    rep(S, infinity).    

accept(S, infinity) ->
    rep(S, infinity);
accept(S, Time) when integer(Time), Time >= 0 ->
    rep(S, Time).

%%
%% Encode socks5 address format
%% return {ok, AddressString} or {error,Reason}
%%
enc_address(IPV4) when tuple(IPV4), size(IPV4) == 4 ->
    Addr = tuple_to_list(IPV4),
    case is_byte_list(Addr) of
	false -> {error, einval};
	true  -> {ok, [?SOCKS5_ATYP_V4 | Addr]}
    end;
enc_address(IPV6) when tuple(IPV6), size(IPV6) == 8 ->
    Addr = lists:flatmap(fun(X) when X>=0, X=<16#ffff ->
				 [(X bsr 8) band 16#ff, X band 16#ff];
			    (_) -> error
			 end, tuple_to_list(IPV6)),
    case is_byte_list(Addr) of
	false -> {error, einval};
	true  -> {ok, [?SOCKS5_ATYP_V6 | Addr]}
    end;    
enc_address(Dom) when atom(Dom) ->
    Addr = atom_to_list(Dom),
    {ok, [?SOCKS5_ATYP_DOM, length(Addr), Addr]};
enc_address(Dom) when list(Dom) ->
    case is_byte_list(Dom) of
	false -> {error, einval};
	true -> {ok, [?SOCKS5_ATYP_DOM, length(Dom), Dom]}
    end;
enc_address(_) -> 
    {error, einval}.

%%
%% Decode address format and read rest of reply
%%
dec_address(S, ?SOCKS5_ATYP_V4, A) ->
    case inet_tcp:recv(S, 5) of
	{ok, [B,C,D,P1,P0]} ->
	    {ok, {{A,B,C,D}, ?u16(P1,P0)}};
	_ -> dec_error(S)
    end;
dec_address(S, ?SOCKS5_ATYP_V6, X1) ->
    case inet_tcp:recv(S, 17) of
	{ok, As} ->
	    [X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,P1,P0] = As,
	    {ok, { { ?u16(X1,X2),?u16(X3,X4),?u16(X5,X6),?u16(X7,X8),
		    ?u16(X9,X10),?u16(X11,X12),?u16(X13,X14),?u16(X15,X16)},
		  ?u16(P1,P0)}};
	_ -> dec_error(S)
    end;
dec_address(S, ?SOCKS5_ATYP_DOM, Len) ->
    Len1 = Len+2,
    case inet_tcp:recv(S, Len1) of
	{ok, Addr} ->
	    Dom = lists:sublist(Addr, 1, Len),
	    [P1,P0] = lists:sublist(Addr, Len+1, 2),
	    {ok, {Dom, ?u16(P1, P0)}};
	_ -> dec_error(S)
    end;
dec_address(S, _, _) ->
    dec_error(S).


dec_error(S) ->
    inet_tcp:close(S),
    {error, "Unknown error"}.

	    

%% Send command to socks server
cmd(S, Cmd, Address, Port, TimeOut) ->
    inet_tcp:send(S, [?SOCKS5_VER, Cmd, 0, Address, ?int16(Port)]),
    rep(S, TimeOut).
    
%% Read from socks server
rep(S, TimeOut) ->
    case inet_tcp:recv(S, 5, TimeOut) of
	{ok, [?SOCKS5_VER, ?SOCKS5_REP_OK, _, AType, A1]} ->
	    dec_address(S, AType, A1);
	{ok, [?SOCKS5_VER, Rep | _]} ->
	    inet_tcp:close(S),
	    case Rep of
		?SOCKS5_REP_FAIL ->
		    {error, "General SOCKS server failure"};
		?SOCKS5_REP_NOT_ALLOWED -> 
		    {error, "Connection not allowed by rule set"};
		?SOCKS5_REP_NET_UNREACHABLE ->
		    {error, "Network unreachable"};
		?SOCKS5_REP_HOST_UNREACHABLE ->
		    {error, "Host unreachable"};
		?SOCKS5_REP_REFUSED ->
		    {error, "Connection refused"};
		?SOCKS5_REP_TTL_EXPIRED ->
		    {error, "TTL expired"};
		?SOCKS5_REP_CMD_NOT_SUPPORTED ->
		    {error, "Command not supported"};
		?SOCKS5_REP_ATYP_NOT_SUPPORTED ->
		    {error, "Address type not supported"};
		_ ->
		    {error, "Unknown error"}
	    end;
	{error,timeout} ->
	    {error, timeout};
	_ ->
	    dec_error(S)
    end.


encode_methods([none | Ms]) ->
    [?SOCKS5_AUTH_NONE | encode_methods(Ms)];
encode_methods([gssapi | Ms]) ->
    [?SOCKS5_AUTH_GSSAPI | encode_methods(Ms)];
encode_methods([user | Ms]) ->
    [?SOCKS5_AUTH_USER | encode_methods(Ms)];
encode_methods([Other | Ms]) when integer(Other), Other < 16#ff ->
    [Other | encode_methods(Ms)];
encode_methods([]) -> [].


decode_method(?SOCKS5_AUTH_NONE) -> none;
decode_method(?SOCKS5_AUTH_GSSAPI) -> gssapi;
decode_method(?SOCKS5_AUTH_USER) -> user;
decode_method(?SOCKS5_AUTH_ERR) -> error;
decode_method(Other) -> Other.

is_byte_list([H | T]) when H >= 0, H =< 255 ->
    is_byte_list(T);
is_byte_list([]) -> true;
is_byte_list(_) -> false.

%%
%% Check if an ip address is direct i.e not via the socks server
%%
% AOJ: added first clause, so gen_tcp:listen(Port) works.
is_direct({0,0,0,0}) ->
    true;
is_direct(IP) ->
    is_direct(inet_db:socks_option(noproxy), IP).

is_direct([{Net,Mask} | Ms], IP) ->
    case is_net(Net,Mask,IP) of
	true -> true;
	false -> is_direct(Ms, IP)
    end;
is_direct([], _) -> false.

is_net({NA,NB,NC,ND},{MA,MB,MC,MD},{A,B,C,D}) ->
    if A band MA == NA,
       B band MB == NB,
       C band MC == NC,
       D band MD == ND -> true;
       true -> false
    end.
