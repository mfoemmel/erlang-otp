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
-module(inet_hosts).

%% Implement gethostbyname gethostbyaddr for inet_hosts table

-export([gethostbyname/1, gethostbyname/2, gethostbyaddr/1]).

-include("inet.hrl").

%% simply scan each entry for a match
gethostbyname(Name) when is_list(Name) ->
    case find_name(Name, any) of
	false -> {error, nxdomain};
	{true, {IP, Type, [Nm | As]}} ->
	    {ok, make_hostent(Nm,[IP],As,Type)}
    end;
gethostbyname(Name) when is_atom(Name) ->
    gethostbyname(atom_to_list(Name));
gethostbyname(_) -> {error, formerr}.


gethostbyname(Name, Type) when is_list(Name), is_atom(Type) ->
    case find_name(Name, Type) of
	false -> {error, nxdomain};
	{true, {IP, Type, [Nm | As]}} -> {ok,make_hostent(Nm,[IP],As,Type)}
    end;
gethostbyname(Name, Type) when is_atom(Name), is_atom(Type) ->
    gethostbyname(atom_to_list(Name), Type);
gethostbyname(_, _) -> {error, formerr}.


gethostbyaddr({A,B,C,D}) when is_integer(A+B+C+D) ->
    IP = {A,B,C,D},
    case ets:lookup(inet_hosts, IP) of
	[] -> {error, nxdomain};
	[{IP,Type,[Nm | As]}] -> {ok, make_hostent(Nm,[IP],As,Type)}
    end;
%% ipv4  only ipv6 address
gethostbyaddr({0,0,0,0,0,16#ffff,G,H}) when is_integer(G+H) ->
    gethostbyaddr({G div 256, G rem 256, H div 256, H rem 256});
gethostbyaddr({A,B,C,D,E,F,G,H}) when is_integer(A+B+C+D+E+F+G+H) ->
    IP = {A,B,C,D,E,F,G,H},
    case ets:lookup(inet_hosts, IP) of
	[] -> {error, nxdomain};
	[{IP,Type,[Nm | As]}] -> {ok, make_hostent(Nm,[IP],As,Type)}
    end;
gethostbyaddr(Addr) when is_list(Addr) ->
    case inet_parse:address(Addr) of
	{ok, IP} -> gethostbyaddr(IP);
	_Error -> {error, formerr}
    end;
gethostbyaddr(Addr) when is_atom(Addr) ->
    gethostbyaddr(atom_to_list(Addr));
gethostbyaddr(_) -> {error, formerr}.




find_name(Name, Type) ->
    find_name(Name, ets:first(inet_hosts), Type).

find_name(_Name, '$end_of_table', _Type) -> false;
find_name(Name, Key, Type) ->
    [{IP, T, Ns}] = ets:lookup(inet_hosts, Key),
    LowerName = inet_db:tolower(Name),
    case lists:member(LowerName, Ns) of
	true when Type =:= any ->
	    Ns2 = lists:map(fun(E)->
				    case E of LowerName -> Name; _ -> E end 
			    end, Ns),
	    {true, {IP,T,Ns2}};
	true when Type =:= T   ->
	    Ns2 = lists:map(fun(E)->
				    case E of LowerName -> Name; _ -> E end 
			    end, Ns),
	    {true, {IP,T,Ns2}};
	_ -> find_name(Name, ets:next(inet_hosts, Key), Type)
    end.

make_hostent(Name, Addrs, Aliases, inet) ->
    #hostent {
	      h_name = Name,
	      h_addrtype = inet,
	      h_length = 4,
	      h_addr_list = Addrs,
	      h_aliases = Aliases
	     };
make_hostent(Name, Addrs, Aliases, inet6) ->
    #hostent {
	      h_name = Name,
	      h_addrtype = inet6,
	      h_length = 16,
	      h_addr_list = Addrs,
	      h_aliases = Aliases
	     }.


