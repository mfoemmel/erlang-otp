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

-module(inet_db).

%% Store info about ip addresses, names, aliases host files resolver
%% options

%% If the macro DEBUG is defined during compilation, 
%% debug printouts are done through erlang:display/1.
%% Activate this feature by starting the compiler 
%% with> erlc -DDEBUG ... 
%% or by> setenv ERL_COMPILER_FLAGS DEBUG 
%% before running make (in the OTP make system)
%% (the example is for tcsh)

%% External exports
-export([start/0, start_link/0, stop/0, reset/0, clear_cache/0]).
-export([add_rr/1,add_rr/5,del_rr/4]).
-export([add_ns/1,add_ns/2, ins_ns/1, ins_ns/2, del_ns/1, del_ns/0]).
-export([add_alt_ns/1,add_alt_ns/2, ins_alt_ns/1,ins_alt_ns/2, 
	 del_alt_ns/1, del_alt_ns/0]).
-export([add_search/1,ins_search/1,del_search/1, del_search/0]).
-export([set_lookup/1, set_recurse/1]).
-export([set_socks_server/1, set_socks_port/1, add_socks_methods/1,
	 del_socks_methods/1, del_socks_methods/0,
	 add_socks_noproxy/1, del_socks_noproxy/1]).
-export([set_cache_size/1, set_cache_refresh/1]).
-export([set_timeout/1, set_retry/1, set_inet6/1, set_usevc/1]).
-export([tcp_module/0, set_tcp_module/1]).
-export([udp_module/0, set_udp_module/1]).
-export([register_socket/2, unregister_socket/1, lookup_socket/1]).

%% Host name & domain
-export([set_hostname/1, set_domain/1]).
-export([gethostname/0]).

%% file interface
-export([add_host/2, del_host/1, clear_hosts/0, add_hosts/1]).
-export([add_resolv/1]).
-export([add_rc/1, add_rc_bin/1, add_rc_list/1, get_rc/0]).

-export([res_option/1]).
-export([socks_option/1]).
-export([getbyname/2]).
-export([gethostbyaddr/1]).
-export([res_gethostbyaddr/2,res_hostent_by_domain/3]).
%% inet help functions
-export([tolower/1]).
-ifdef(DEBUG).
-define(dbg(Fmt, Args), io:format(Fmt, Args)).
-else.
-define(dbg(Fmd, Args), ok).
-endif.

-import(lists, [foreach/2, reverse/1, keydelete/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, 
	{
	 db,          %% resolver data
	 cache,       %% bag of resource records
	 hosts,       %% hosts table
	 cache_timer  %% timer reference for refresh
	}).

-include_lib("kernel/include/inet.hrl").
-include("inet_int.hrl").
-include("inet_res.hrl").
-include("inet_dns.hrl").
-include("inet_config.hrl").


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start() ->
    case gen_server:start({local, inet_db}, inet_db, [], []) of
	{ok,Pid} -> inet_config:init(), {ok,Pid};
	Error -> Error
    end.


start_link() ->
    case gen_server:start_link({local, inet_db}, inet_db, [], []) of
	{ok,Pid} -> inet_config:init(), {ok,Pid};
	Error -> Error
    end.
	       
call(Req) -> 
    gen_server:call(inet_db, Req, infinity).

stop() ->
    call(stop).

reset() ->
    call(reset).


%% insert all resolve options from this file (MAY GO)
add_resolv(File) ->
    case inet_parse:resolv(File) of
	{ok, Res} -> add_rc_list(Res);
	Error -> Error
    end.

%% add all aliases from this hosts file (MAY GO)
add_hosts(File) ->
    case inet_parse:hosts(File) of
	{ok, Res} ->
	    foreach(
	      fun({IP, Name, Aliases}) -> add_host(IP, [Name|Aliases]) end,
	      Res);
	Error -> Error
    end.


add_host(IP, Names) -> call({add_host, IP, Names}).

del_host(IP) ->  call({del_host, IP}).

clear_hosts() -> call(clear_hosts).

%% add to the end of name server list
add_ns(IP) -> 
    add_ns(IP,?NAMESERVER_PORT).
add_ns(IP,Port) ->
    call({add_ns, IP, Port}).

%% insert at head of name server list
ins_ns(IP) ->
    call({ins_ns, IP, ?NAMESERVER_PORT}).
ins_ns(IP,Port) ->
    call({ins_ns, IP,Port}).

%% delete this name server entry (delete all ns having this ip)
del_ns(IP) -> 
    call({del_ns, IP}).

del_ns() -> 
    call(del_ns).

%% ALTERNATIVE NAME SERVER
%% add to the end of name server list
add_alt_ns(IP) -> 
    add_alt_ns(IP, ?NAMESERVER_PORT).
add_alt_ns(IP,Port) ->
    call({add_alt_ns, IP,Port}).

%% insert at head of name server list
ins_alt_ns(IP) -> 
    ins_alt_ns(IP, ?NAMESERVER_PORT).
ins_alt_ns(IP,Port) ->
    call({ins_alt_ns, IP,Port}).

%% delete this name server entry
del_alt_ns(IP) ->
    call({del_alt_ns, IP}).

del_alt_ns() -> 
    call(del_alt_ns).

%% add this domain to the search list
add_search(Domain) when list(Domain) -> 
    call({add_search, Domain}).

ins_search(Domain) when list(Domain) ->
    call({ins_search, Domain}).

del_search(Domain) ->
    call({del_search, Domain}).

del_search() ->
    call(del_search).

%% set host name used by inet
%% Should only be used by inet_config at startup!
set_hostname(Name) ->
    call({set_hostname, Name}).

%% set default domain
set_domain(Domain) ->
    call({set_domain, Domain}).

%% set lookup methods
set_lookup(Methods) ->
    call({set_lookup, Methods}).

%% resolver
set_recurse(Flag) -> call({set_recurse, Flag}).

set_timeout(Time) -> call({set_timeout, Time}).

set_retry(N) -> call({set_retry, N}).

set_inet6(Bool) -> call({set_inet6,Bool}).

set_usevc(Bool) -> call({set_usevc,Bool}).

%% set socks options
set_socks_server(Server) -> call({set_socks_server, Server}).

set_socks_port(Port) -> call({set_socks_port, Port}).

add_socks_methods(Ms) -> call({add_socks_methods,Ms}).

del_socks_methods(Ms) -> call({del_socks_methods,Ms}).

del_socks_methods() -> call(del_socks_methods).

add_socks_noproxy({Net,Mask}) -> call({add_socks_noproxy, {Net,Mask}}).

del_socks_noproxy(Net) -> call({del_socks_noproxy, Net}).

%% cache options
set_cache_size(Limit) -> call({set_cache_size, Limit}).

set_cache_refresh(Time) -> call({set_cache_refresh, Time}).

clear_cache() -> call(clear_cache).


set_tcp_module(Module) -> call({set_tcp_module, Module}).

tcp_module() -> db_get(tcp_module).

set_udp_module(Module) -> call({set_udp_module, Module}).

udp_module() -> db_get(udp_module).


%% Add a .inetrc file
add_rc(File) -> 
    case file:consult(File) of
	{ok, List} -> add_rc_list(List);
	Error -> Error
    end.

%% Add a .inetrc binary term must be a rc list
add_rc_bin(Bin) ->
    case catch binary_to_term(Bin) of
	List when list(List) ->
	    add_rc_list(List);
	_ ->
	    {error, badarg}
    end.

add_rc_list(List) ->
    case catch add_rc_list_int(List) of
	{'EXIT', Reason} -> {error, Reason};
	Value -> Value
    end.

add_rc_list_int(List) ->
    foreach(
      fun({domain, Name}) ->
	      set_domain(Name);
	 ({nameserver,Ns}) -> 
	      add_ns(Ns);
	 ({nameserver,Ns,Port}) ->
	      add_ns(Ns,Port);
	 ({alt_nameserver,Ns}) ->
	      add_alt_ns(Ns);
	 ({alt_nameserver,Ns,Port}) ->
	      add_alt_ns(Ns,Port);
	 ({socks5_server,Server}) ->
	      set_socks_server(Server);
	 ({socks5_port,Port}) ->
	      set_socks_port(Port);
	 ({socks5_methods, Ms}) ->
	      add_socks_methods(Ms);
	 ({socks5_noproxy, IP, Mask}) ->
	      add_socks_noproxy({IP,Mask});
	 ({search, Ds}) -> 
	      foreach(fun(Domain) -> add_search(Domain) end, Ds);
	 ({host, IP, Aliases}) ->
	      add_host(IP, Aliases);
	 ({cache_refresh, Time}) ->
	      set_cache_refresh(Time);
	 ({cache_size, Size}) ->
	      set_cache_size(Size);
	 ({timeout, Time}) ->
	      set_timeout(Time);
	 ({retry, N}) ->
	      set_retry(N);
	 ({inet6, Bool}) ->
	      set_inet6(Bool);
	 ({udp, Module}) ->
	      set_udp_module(Module);
	 ({tcp, Module}) ->
	      set_tcp_module(Module);
	 (reset) ->
	      reset();       %% reset before loading new defs
	 (clear_cache) ->
	      clear_cache();
	 (clear_hosts) ->
	      clear_hosts();
	 ({lookup, Ls}) ->
	      set_lookup(translate_lookup(Ls));
	 (Opt) -> 
	      throw({error, {badopt, Opt}})
      end, List),
    ok.

%% All kind of flavors !
translate_lookup(["bind" | Ls]) -> [dns | translate_lookup(Ls)];
translate_lookup(["dns" | Ls]) -> [dns | translate_lookup(Ls)];
translate_lookup(["hosts" | Ls]) -> [file | translate_lookup(Ls)];
translate_lookup(["files" | Ls]) -> [file | translate_lookup(Ls)];
translate_lookup(["file"  | Ls]) -> [file | translate_lookup(Ls)];
translate_lookup(["yp" | Ls]) -> [yp | translate_lookup(Ls)];
translate_lookup(["nis" | Ls]) -> [nis | translate_lookup(Ls)];
translate_lookup(["nisplus" | Ls]) -> [nisplus | translate_lookup(Ls)];
translate_lookup(["native" | Ls]) -> [native | translate_lookup(Ls)];
translate_lookup([_ | Ls]) -> translate_lookup(Ls);
translate_lookup([]) -> [].

%% Reconstruct a .inetrc sturcture from inet_db
get_rc() -> 
    get_rc([domain, nameserver, search, alt_nameserver,
	    timeout, retry, inet6, usevc,
	    socks5_server, socks5_port, socks5_methods, socks5_noproxy,
	    udp, tcp, host, cache_size, cache_refresh], []).

get_rc([K | Ks], Ls) ->
    case K of
	domain     -> get_rc(domain, res_domain, "", Ks, Ls);
	nameserver -> get_rc_ns(db_get(res_ns),nameserver,Ks,Ls);
	alt_nameserver -> get_rc_ns(db_get(res_alt_ns),alt_nameserver,Ks,Ls);
	search -> get_rc(search, res_search, [], Ks, Ls);
	timeout -> get_rc(timeout,res_timeout,?RES_TIMEOUT, Ks,Ls);
	retry   -> get_rc(rety, res_retry, ?RES_RETRY, Ks, Ls);
	inet6   -> get_rc(inet6, res_inet6, false, Ks, Ls);
	usevc   -> get_rc(usevc, res_usevc, false, Ks, Ls);
	tcp     -> get_rc(tcp, tcp_module, ?DEFAULT_TCP_MODULE, Ks, Ls); 
	udp     -> get_rc(udp, udp_module, ?DEFAULT_UDP_MODULE, Ks, Ls); 
	cache_size -> get_rc(cache_size, cache_size, ?CACHE_LIMIT, Ks, Ls);
	cache_refresh ->
	    get_rc(cache_refresh, cache_refresh_interval,?CACHE_REFRESH,Ks,Ls);
	socks5_server -> get_rc(socks5_server, socks5_server, "", Ks, Ls);
	socks5_port    -> get_rc(socks5_port,socks5_port,?IPPORT_SOCKS,Ks,Ls);
	socks5_methods -> get_rc(socks5_methods,socks5_methods,[none],Ks,Ls);
	socks5_noproxy ->
	    case db_get(socks5_noproxy) of
		[] -> get_rc(Ks, Ls);
		NoProxy -> get_rc_noproxy(NoProxy, Ks, Ls)
	    end;
	_ ->
	    get_rc(Ks, Ls)
    end;
get_rc([], Ls) -> 
    reverse(Ls).

get_rc(Name, Key, Default, Ks, Ls) ->
    case db_get(Key) of
	Default -> get_rc(Ks, Ls);
	Value -> get_rc(Ks, [{Name, Value} | Ls])
    end.
	    
get_rc_noproxy([{Net,Mask} | Ms], Ks, Ls) ->
    get_rc_noproxy(Ms, Ks, [{socks5_noproxy, Net, Mask} | Ls]);
get_rc_noproxy([], Ks, Ls) -> get_rc(Ks, Ls).

get_rc_ns([{IP,?NAMESERVER_PORT} | Ns], Tag, Ks, Ls) ->
    get_rc_ns(Ns, Tag, Ks, [{Tag, IP} | Ls]);
get_rc_ns([{IP,Port} | Ns], Tag, Ks, Ls) ->
    get_rc_ns(Ns, Tag, Ks, [{Tag, IP, Port} | Ls]);
get_rc_ns([], Tag, Ks, Ls) ->
    get_rc(Ks, Ls).

%%
%% Resolver options
%%
res_option(nameserver)     -> db_get(res_ns);
res_option(alt_nameserver) -> db_get(res_alt_ns);
res_option(domain)         -> db_get(res_domain);
res_option(lookup)         -> db_get(res_lookup);
res_option(recurse)        -> db_get(res_recurse);
res_option(search)         -> db_get(res_search);
res_option(retry)          -> db_get(res_retry);
res_option(timeout)        -> db_get(res_timeout);
res_option(inet6)          -> db_get(res_inet6);
res_option(next_id)        -> 
    Cnt = ets:update_counter(inet_db, res_id, 1),
    case Cnt band 16#ffff of
	0 ->
	    ets:update_counter(inet_db, res_id, -Cnt),
	    0;
	Id ->
	    Id
    end;
res_option(usevc)          -> db_get(res_usevc).

socks_option(server)       -> db_get(socks5_server);
socks_option(port)         -> db_get(socks5_port);
socks_option(methods)      -> db_get(socks5_methods);
socks_option(noproxy)      -> db_get(socks5_noproxy).

gethostname()              -> db_get(hostname).


db_get(Name) ->
    case ets:lookup(inet_db, Name) of
	[] -> undefined;
	[{_,Val}] -> Val
    end.

add_rr(RR) ->
    call({add_rr, RR}).

add_rr(Domain, Class, Type, TTL, Data) ->
    call({add_rr, #dns_rr { domain = Domain, class = Class,
		       type = Type, ttl = TTL, data = Data}}).

del_rr(Domain, Class, Type, Data) ->
    call({del_rr, #dns_rr { domain = Domain, class = Class,
		       type = Type, cnt = '_', tm = '_', ttl = '_',
		       bm = '_', func = '_', data = Data}}).

res_cache_answer(Rec) ->
    foreach( fun(RR) -> add_rr(RR) end, Rec#dns_rec.anlist).


%%
%% getbyname (cache version)
%%
getbyname(Name,Type) ->
    case inet_parse:dots(Name) of
	{0, false} ->
	    getbysearch(Name, ".", res_option(search),Type);
	{0, true} ->
	    getbysearch(Name, "", res_option(search),Type);
	_ ->
	    hostent_by_domain(Name,Type)
    end.

getbysearch(Name, Dot, [Dom | Ds], Type) ->
    case hostent_by_domain(Name ++ Dot ++ Dom, Type) of
	{ok, HEnt} -> {ok, HEnt};
	Error -> getbysearch(Name, Dot, Ds, Type)
    end;
getbysearch(Name, Dot, [], Type) ->
    hostent_by_domain(Name, Type).


make_hostent(Name, Addrs, Aliases, ?S_A) ->
    #hostent {
	      h_name = Name,
	      h_addrtype = inet,
	      h_length = 4,
	      h_addr_list = Addrs,
	      h_aliases = Aliases
	     };
make_hostent(Name, Addrs, Aliases, ?S_AAAA) ->
    #hostent {
	      h_name = Name,
	      h_addrtype = inet6,
	      h_length = 16,
	      h_addr_list = Addrs,
	      h_aliases = Aliases
	     };
make_hostent(Name, Datas, Aliases, Type) ->
    %% Use #hostent{} for other Types as well !
    #hostent {
	      h_name = Name,
	      h_addrtype = Type,
	      h_length = length(Datas),
	      h_addr_list = Datas,
	      h_aliases = Aliases
	     }.

hostent_by_domain(Domain,Type) ->
    ?dbg("hostent_by_domain: ~p~n", [Domain]),
    hostent_by_domain(Domain, [], Type).

hostent_by_domain(Domain, Aliases,Type) ->
    case lookup_type(Domain,Type) of
	[] ->
	    case lookup_cname(Domain) of
		[] ->  {error, nxdomain};
		[CName | _] ->
		    hostent_by_domain(CName, [Domain | Aliases], Type)
	    end;
	Addrs ->
	    {ok, make_hostent(Domain, Addrs, Aliases,Type)}
    end.

%% lookup address record
lookup_type(Domain,Type) ->
    [R#dns_rr.data || R <- lookup_rr(Domain, in, Type) ].

%% lookup canonical name
lookup_cname(Domain) ->
    [R#dns_rr.data || R <- lookup_rr(Domain, in, ?S_CNAME) ].

%% lookup domain pointers
lookup_ptr(Domain) ->
    [R#dns_rr.data || R <- lookup_rr(Domain, in, ?S_PTR) ].

%% Have to do all lookups (changes to the db) in the
%% process in order to make it possible to refresh the cache.
lookup_rr(Domain, Class, Type) ->
    call({lookup_rr, Domain, Class, Type}).

%%
%% hostent_by_domain (newly resolved version)
%% match data field directly and cache RRs.
%%
res_hostent_by_domain(Domain, Type, Rec) ->
    res_cache_answer(Rec),
    RRs = Rec#dns_rec.anlist,
    ?dbg("res_hostent_by_domain: ~p - ~p~n", [Domain, RRs]),
    res_hostent_by_domain(Domain, [], Type, RRs).

res_hostent_by_domain(Domain, Aliases,Type,RRs) ->
    case res_lookup_type(Domain,Type,RRs) of
	[] ->
	    case res_lookup_type(Domain,?S_CNAME,RRs) of
		[] ->  {error, nxdomain};
		[CName | _] ->
		    res_hostent_by_domain(CName, [Domain | Aliases], Type, RRs)
	    end;
	Addrs ->
	    {ok, make_hostent(Domain, Addrs, Aliases,Type)}
    end.

%% newly resolved lookup address record
res_lookup_type(Domain,Type,RRs) ->
    [R#dns_rr.data || R <- RRs,
		      R#dns_rr.domain == Domain,
		      R#dns_rr.type == Type].

%%
%% gethostbyaddr (cache version)
%% match data field directly
%%
gethostbyaddr(IP) ->
    case dnip(IP) of
	{ok, {IP1, HType, HLen, DnIP}} ->
	    RRs = match_rr(#dns_rr { domain = DnIP, class = in, type = ptr,
				     cnt = '_', tm = '_', ttl = '_',
				     bm = '_', func = '_', data = '_' }),
	    ent_gethostbyaddr(RRs,  IP1, HType, HLen);
	Error -> Error
    end.

%%
%% res_gethostbyaddr (newly resolved version)
%% match data field directly and cache RRs.
%%
res_gethostbyaddr(IP, Rec) ->
    {ok, {IP1, HType, HLen}} = dnt(IP),
    res_cache_answer(Rec),
    ent_gethostbyaddr(Rec#dns_rec.anlist, IP1, HType, HLen).

ent_gethostbyaddr(RRs, IP, AddrType, Length) ->
    case RRs of
	[] -> {error, nxdomain};
	[RR|TR] ->
	    %% debug
	    if TR /= [] ->
		    ?dbg("gethostbyaddr found extra=~p~n", [TR]);
	       true -> ok
	    end,
	    Domain = RR#dns_rr.data,
	    H = #hostent { h_name = Domain,
			   h_aliases = lookup_cname(Domain),
			   h_addr_list = [IP],
			   h_addrtype = AddrType,
			   h_length = Length },
	    {ok, H}
    end.

dnip(IP) ->
    case dnt(IP) of
	{ok,{IP1 = {A,B,C,D}, inet, HLen}} ->
	    {ok,{IP1, inet, HLen, dn_in_addr_arpa(A,B,C,D)}};
	{ok,{IP1 = {A,B,C,D,E,F,G,H}, inet6, HLen}} ->
	    {ok,{IP1, inet6, HLen, dn_ip6_int(A,B,C,D,E,F,G,H)}};
	_ ->
	    {error, formerr}
    end.


dnt(IP = {A,B,C,D}) when ?ip(A,B,C,D) ->
    {ok, {IP, inet, 4}};
dnt({0,0,0,0,0,16#ffff,G,H}) when integer(G+H) ->
    A = G div 256, B = G rem 256, C = H div 256, D = H rem 256,
    {ok, {{A,B,C,D}, inet, 4}};
dnt(IP = {A,B,C,D,E,F,G,H}) when ?ip6(A,B,C,D,E,F,G,H) ->
    {ok, {IP, inet6, 16}};
dnt(_) ->
    {error, formerr}.

%%
%% Register socket Modules
%%
register_socket(Socket, Module) when port(Socket), atom(Module) ->
    call({register_socket, Socket, Module}).

unregister_socket(Socket) when port(Socket) ->
    ets:delete(inet_db, {socket,Socket}).

lookup_socket(Socket) when port(Socket) ->
    case catch ets:lookup_element(inet_db, {socket,Socket}, 2) of
	Module when atom(Module) ->
	    {ok, Module};
	_ ->
	    {error, closed}
    end.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          {stop, Reason}
%%----------------------------------------------------------------------

%% INET DB ENTRY TYPES:
%%
%% KEY            VALUE           - DESCRIPTION
%%
%% hostname       String          - SHORT host name
%%
%% Resolver options
%% ----------------
%% res_ns         [Nameserver]    - list of name servers
%% res_alt_ns     [AltNameServer] - list of alternate name servers (nxdomain)
%% res_search     [Domain]        - list of domains for short names
%% res_domain     Domain          - local domain for short names
%% res_recurse    Bool            - recursive query 
%% res_usevc      Bool            - use tcp only
%% res_id         Integer         - NS query identifier
%% res_retry      Integer         - Retry count for UDP query
%% res_timeout    Integer         - UDP query timeout before retry
%%
%% Socks5 options
%% --------------
%% socks5_server  Server          - IP address of the socks5 server
%% socks5_port    Port            - TCP port of the socks5 server
%% socks5_methods Ls              - List of authication methdos
%% socks5_noproxy IPs             - List of {Net,Subnetmask}
%%
%% Generic tcp/udp options
%% -----------------------
%% tcp_module     Module          - The default gen_tcp module
%% udp_module     Module          - The default gen_udp module 
%%
%% Distribution options
%% --------------------
%% {node_auth,N}  Ls              - List of authentication for node N
%% {node_crypt,N} Ls              - List of encryption methods for node N
%% node_auth      Ls              - Default authenication
%% node_crypt     Ls              - Default encryption
%%
init([]) ->
    process_flag(trap_exit, true),
    Db = ets:new(inet_db, [public, named_table]),
    reset_db(Db),
    Cache = ets:new(inet_cache, [public, bag, {keypos,2}, named_table]),
    Hosts = ets:new(inet_hosts, [public, named_table]),
    {ok, #state{
		db = Db,
		cache = Cache,
		hosts = Hosts,
		cache_timer = init_timer() }}.

reset_db(Db) ->
    ets:insert(Db, {hostname, []}),
    ets:insert(Db, {res_ns, []}),
    ets:insert(Db, {res_alt_ns, []}),
    ets:insert(Db, {res_search, []}),
    ets:insert(Db, {res_domain, ""}),
    ets:insert(Db, {res_lookup, []}),
    ets:insert(Db, {res_recurse, 1}),
    ets:insert(Db, {res_usevc, false}),
    ets:insert(Db, {res_id, 0}),
    ets:insert(Db, {res_retry, ?RES_RETRY}),
    ets:insert(Db, {res_timeout, ?RES_TIMEOUT}),
    ets:insert(Db, {res_inet6, false}),
    ets:insert(Db, {cache_size, ?CACHE_LIMIT}),
    ets:insert(Db, {cache_refresh_interval,?CACHE_REFRESH}),
    ets:insert(Db, {socks5_server, ""}),
    ets:insert(Db, {socks5_port, ?IPPORT_SOCKS}),
    ets:insert(Db, {socks5_methods, [none]}),
    ets:insert(Db, {socks5_noproxy, []}),
    ets:insert(Db, {tcp_module, ?DEFAULT_TCP_MODULE}),
    ets:insert(Db, {udp_module, ?DEFAULT_UDP_MODULE}).
	       

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, Reply, State}     (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(Request, From, State) ->
    Db = State#state.db,
    case Request of
	%% temporary special case used only by inet_config to
	%% add the own hostname as an alias for localhost if the real 
	%% IP adress is not specified anywhere
	%% This code will be changed when the hosts-table is reorganized
	%% to use names as keys
	{add_host,{127,0,0,1},[TName|TAs]} when list(TName), list(TAs) ->
	    [Name|As] = lists:map(fun tolower/1,[TName|TAs]),
	    NameList = case ets:lookup(State#state.hosts,{127,0,0,1}) of
			   [{IP,_,NList}] -> NList;
			   _ -> []
		       end,
	    ets:insert(State#state.hosts, {{127,0,0,1},inet,NameList ++ [Name|As]}),
	    {reply, ok, State};

	{add_host,IP,[TName|TAs]} when tuple(IP), list(TName), list(TAs) ->
	    [Name|As] = lists:map(fun tolower/1,[TName|TAs]),
	    if size(IP) == 4 ->
		    %% temporary special as above
		    case ets:lookup(State#state.hosts,{127,0,0,1}) of
			[{IP,_,[N|AList]}] ->
			    ets:insert(State#state.hosts, 
				       {{127,0,0,1},inet,
					lists:foldl(fun(A,L) -> 
							    lists:delete(A,L) 
						    end, [N|AList], [Name|As])});
			_ -> true
		    end,
		    %% end temporary special
		    ets:insert(State#state.hosts, {IP,inet,[Name|As]}),
		    {reply, ok, State};
	       size(IP) == 8 ->
		    ets:insert(State#state.hosts, {IP,inet6,[Name|As]}),
		    {reply, ok, State};
	       true ->
		    {reply, error, State}
	    end;

	{del_host, IP} when tuple(IP) ->
	    ets:delete(State#state.hosts, IP),
	    {reply, ok, State};
		    
	{add_rr, RR} when record(RR, dns_rr) ->
	    RR1 = lower_rr(RR),
	    ?dbg("add_rr: ~p~n", [RR1]),
	    do_add_rr(RR1, Db, State),
	    {reply, ok, State};

	{del_rr, RR} when record(RR, dns_rr) ->
	    RR1 = lower_rr(RR),
	    %% note. del_rr will handle wildcards !!!
	    Cache = State#state.cache,
	    ets:match_delete(Cache, RR1),
	    {reply, ok, State};

	{lookup_rr, Domain, Class, Type} ->
	    {reply, do_lookup_rr(Domain, Class, Type), State};

	{register_socket, Socket, Module} ->
	    ets:insert(inet_db, {{socket,Socket}, Module}),
	    link(Socket),
	    {reply, ok, State};

	{unregister_socket, Socket} ->
	    ets:delete(inet_db, {socket,Socket}),
	    unlink(Socket),
	    {reply, ok, State};

	%% XXX Fix IPv6 nameservers
	{ins_ns, {A,B,C,D},Port} when ?ip(A,B,C,D), integer(Port) ->
	    [{_,Ns}] = ets:lookup(Db, res_ns),
	    ets:insert(Db, {res_ns, [{{A,B,C,D},Port} | Ns]}),
	    {reply, ok, State};

	{add_ns, {A,B,C,D}, Port} when ?ip(A,B,C,D), integer(Port) ->
	    [{_,Ns}] = ets:lookup(Db, res_ns),
	    ets:insert(Db, {res_ns, Ns ++ [{{A,B,C,D},Port}]}),
	    {reply, ok, State};

	{del_ns, {A,B,C,D}} when ?ip(A,B,C,D) ->
	    [{_,Ns}] = ets:lookup(Db, res_ns),
	    ets:insert(Db, {res_ns, keydelete({A,B,C,D}, 1, Ns)}),
	    {reply, ok, State};

	del_ns ->
	    ets:insert(Db, {res_ns, []}),
	    {reply, ok, State};

	%% Fix IPv6 nameservers
	{ins_alt_ns, {A,B,C,D}, Port} when ?ip(A,B,C,D), integer(Port) ->
	    [{_,Ns}] = ets:lookup(Db, res_alt_ns),
	    ets:insert(Db, {res_alt_ns, [{{A,B,C,D},Port} | Ns]}),
	    {reply, ok, State};

	{add_alt_ns, {A,B,C,D}, Port} when ?ip(A,B,C,D), integer(Port) ->
	    [{_,Ns}] = ets:lookup(Db, res_alt_ns),
	    ets:insert(Db, {res_alt_ns, Ns ++ [{{A,B,C,D},Port}]}),
	    {reply, ok, State};

	{del_alt_ns, {A,B,C,D}} when ?ip(A,B,C,D) ->
	    [{_,Ns}] = ets:lookup(Db, res_alt_ns),
	    ets:insert(Db, {res_alt_ns, keydelete({A,B,C,D}, 1, Ns)}),
	    {reply, ok, State};

	del_alt_ns ->
	    ets:insert(Db, {res_alt_ns, []}),
	    {reply, ok, State};

	{ins_search, Dom} ->
	    case inet_parse:visible_string(Dom) of
		true ->
		    [{_,Ds}] = ets:lookup(Db, res_search),
		    Ds1 = lists:delete(Dom, Ds),
		    ets:insert(Db, {res_search, [Dom | Ds1]}),
		    {reply, ok, State};
		false ->
		    {reply, error, State}
	    end;

	{add_search, Dom} ->
	    case inet_parse:visible_string(Dom) of
		true ->
		    [{_,Ds}] = ets:lookup(Db, res_search),
		    case lists:member(Dom, Ds) of
			true ->
			    {reply, ok, State};
			false ->
			    ets:insert(Db, {res_search, Ds ++ [Dom]}),
			    {reply, ok, State}
		    end;
		false ->
		    {reply, error, State}
	    end;

	{del_search, Dom} ->
	    case inet_parse:visible_string(Dom) of
		true ->
		    [{_,Ds}] = ets:lookup(Db, res_search),
		    ets:insert(Db, {res_search, Ds -- [Dom]}),
		    {reply, ok, State};
		false ->
		    {reply, ok, State}
	    end;

	del_search ->
	    ets:insert(Db, {res_search, []}),
	    {reply, ok, State};

	{set_hostname, Name} ->
	    case inet_parse:visible_string(Name) of
		true ->
		    ets:insert(Db, {hostname, Name}),
		    {reply, ok, State};
		false ->
		    {reply, error, State}
	    end;

	{set_domain, Dom} ->
	    case inet_parse:visible_string(Dom) of
		true ->
		    ets:insert(Db, {res_domain, Dom}),
		    {reply, ok, State};
		false ->
		    {reply, error, State}
	    end;

	{set_lookup, Methods} ->
	    ets:insert(Db, {res_lookup, Methods}),
	    {reply, ok, State};

	{set_recurse, 1} ->
	    ets:insert(Db, {res_recurse, 1}),
	    {reply, ok, State};

	{set_recurse, 0} ->
	    ets:insert(Db, {res_recurse, 0}),
	    {reply, ok, State};

	{set_timeout, Time} when integer(Time), Time > 0 ->
	    ets:insert(Db, {res_timeout, Time}),
	    {reply, ok, State};

	{set_retry, N} when integer(N), N > 0 ->
	    ets:insert(Db, {res_retry, N}),
	    {reply, ok, State};

	{set_inet6, true} ->
	    ets:insert(Db, {res_inet6, true}),
	    {reply, ok, State};

	{set_inet6, false} ->
	    ets:insert(Db, {res_inet6, false}),
	    {reply, ok, State};

	{set_usevc, true} ->
	    ets:insert(Db, {res_usevc, true}),
	    {reply, ok, State};

	{set_usevc, false} ->
	    ets:insert(Db, {res_usevc, false}),
	    {reply, ok, State};

	{set_socks_server, {A,B,C,D}} when ?ip(A,B,C,D) ->
	    ets:insert(Db, {socks5_server, {A,B,C,D}}),
	    {reply, ok, State};

	{set_socks_port, Port} when integer(Port) ->
	    ets:insert(Db, {socks5_port, Port}),
	    {reply, ok, State};

	{add_socks_methods, Ls} -> 
	    [{_,As}] = ets:lookup(Db, socks5_methods),
	    As1 = As -- Ls,
	    ets:insert(Db, {socks5_methods, As1 ++ Ls}),
	    {reply, ok, State};
	    
	{del_socks_methods, Ls} ->
	    [{_,As}] = ets:lookup(Db, socks5_methods),
	    As1 = As -- Ls,
	    case lists:member(none, As1) of
		false -> ets:insert(Db, {socks5_methods, As1 ++ [none]});
		true  -> ets:insert(Db, {socks5_methods, As1})
	    end,
	    {reply, ok, State};
	
	del_socks_methods ->
	    ets:insert(Db, {socks5_methods, [none]}),
	    {reply, ok, State};

	{add_socks_noproxy, {{A,B,C,D},{MA,MB,MC,MD}}} 
	when ?ip(A,B,C,D), ?ip(MA,MB,MC,MD) ->
	    [{_,As}] = ets:lookup(Db, socks5_noproxy),
	    ets:insert(Db, {socks5_noproxy, As++[{{A,B,C,D},{MA,MB,MC,MD}}]}),
	    {reply, ok, State};

	{del_socks_noproxy, {A,B,C,D}} when ?ip(A,B,C,D) ->
	    [{_,As}] = ets:lookup(Db, socks5_noproxy),
	    ets:insert(Db, {socks5_noproxy, keydelete({A,B,C,D},1,As)}),
	    {reply, ok, State};

	{set_tcp_module, Mod} when atom(Mod) ->
	    ets:insert(Db, {tcp_module, Mod}), %% check/load module ?
	    {reply, ok, State};

	{set_udp_module, Mod} when atom(Mod) ->
	    ets:insert(Db, {udp_module, Mod}), %% check/load module ?
	    {reply, ok, State};

	{set_cache_size, Size} when integer(Size), Size >= 0 ->
	    ets:insert(Db, {cache_size, Size}),
	    {reply, ok, State};
	
	{set_cache_refresh, Time} when integer(Time), Time > 0 ->
	    Time1 = ((Time+999) div 1000)*1000, %% round up
	    ets:insert(Db, {cache_refresh_interval, Time1}),
	    stop_timer(State#state.cache_timer),
	    {reply, ok, State#state{cache_timer = init_timer()}};

	clear_hosts ->
	    ets:match_delete(State#state.hosts, '_'),
	    {reply, ok, State};

	clear_cache ->
	    ets:match_delete(State#state.cache, '_'),
	    {reply, ok, State};

	reset ->
	    reset_db(Db),
	    stop_timer(State#state.cache_timer),
	    {reply, ok, State#state{cache_timer = init_timer()}};

	stop ->
	    {stop, normal, ok, State};

	_ ->
	    {reply, error, State}
    end.


%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info(refresh_timeout, State) ->
    do_refresh_cache(State#state.cache),
    {noreply, State#state{cache_timer = init_timer()}};

handle_info({'EXIT', Socket, _}, State) when port(Socket) ->
    ets:delete(inet_db, {socket,Socket}),
    {noreply, State};

handle_info(Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    stop_timer(State#state.cache_timer),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%% Add a resource record to the cache if there are space left.
%% If the cache is full this function first deletes old entries,
%% i.e. entries with oldest latest access time.
%% #dns_rr.cnt is used to store the access time instead of number of
%% accesses.
do_add_rr(RR, Db, State) ->
    CacheDb = State#state.cache,
    TM = times(),
    case alloc_entry(Db, CacheDb, TM) of
	true ->
	    cache_rr(Db, CacheDb, RR#dns_rr { tm = TM,
					      cnt = TM });
	_ ->
	    false
    end.

cache_rr(Db, Cache, RR) ->
    %% delete possible old entry
    ets:match_delete(Cache, RR#dns_rr { cnt = '_', tm = '_', ttl = '_',
				        bm = '_', func = '_'}),
    ets:insert(Cache, RR).

times() ->
    {Mega,Secs,_} = erlang:now(),
    Mega*1000000 + Secs.

%% lookup and remove old entries

do_lookup_rr(Domain, Class, Type) ->
    match_rr(#dns_rr { domain = tolower(Domain), class = Class,type = Type, 
		      cnt = '_', tm = '_', ttl = '_',
		      bm = '_', func = '_', data = '_'}).

match_rr(RR) ->
    filter_rr(ets:match_object(inet_cache, RR), times()).


%% filter old resource records and update access count

filter_rr([RR | RRs], Time) when RR#dns_rr.ttl == 0 -> %% at least once
    ets:match_delete(inet_cache, RR),
    [RR | filter_rr(RRs, Time)];
filter_rr([RR | RRs], Time) when RR#dns_rr.tm + RR#dns_rr.ttl < Time ->
    ets:match_delete(inet_cache, RR),
    filter_rr(RRs, Time);
filter_rr([RR | RRs], Time) ->
    ets:match_delete(inet_cache, RR),
    ets:insert(inet_cache, RR#dns_rr { cnt = Time }),
    [RR | filter_rr(RRs, Time)];
filter_rr([], Time) ->  [].


%%
%% Lower case the domain name before storage 
%%
lower_rr(RR) ->
    Dn = RR#dns_rr.domain,
    if list(Dn) ->
	    RR#dns_rr { domain = tolower(Dn) };
       true -> RR
    end.

%%
%% Map upper-case to lower-case 
%% NOTE: this code is in kernel and we don't want to relay
%% to much on stdlib
%%
tolower([C|Cs]) when C >= $A, C =< $Z -> [(C-$A)+$a | tolower(Cs)];
tolower([C|Cs]) -> [C | tolower(Cs)];
tolower([]) -> [].

%% reverse of inet_res:dn_in_addr_arpa()
arpa_addr_in_dn(Cs) ->
    arpa_addr_in_dn(Cs, [], []).

arpa_addr_in_dn([X | Xs], Ds, Acc) when X >= $0, X =< $9 ->
    arpa_addr_in_dn(Xs, [X|Ds], Acc);
arpa_addr_in_dn([$. | Xs], Ds, Acc) ->
    arpa_addr_in_dn(Xs, [], [list_to_integer(Ds) | Acc]);
arpa_addr_in_dn([], Ds, Acc) -> 
    list_to_tuple(reverse([list_to_integer(Ds) | Acc])).

%% reverse of inet_res:dn_ip6_int nibble dot list to {A,B,C,D,E,F,G,H}
int_ip6_dn(Cs) ->
    int_ip6_dn(Cs, []).

int_ip6_dn([$.,X3,$.,X2,$.,X1,$.,X0 | T], Acc) ->
    X = (dig(X3) bsl 12) + (dig(X2) bsl 8) + (dig(X1) bsl 4) + dig(X0),
    int_ip6_dn(T, [X | Acc]);
int_ip6_dn([], Acc) -> list_to_tuple(reverse(Acc)).


dig(X) when X >= $A, X =< $F -> (X - $A) + 10;
dig(X) when X >= $a, X =< $f -> (X - $a) + 10;
dig(X) when X >= $0, X =< $9 -> (X - $0).

dn_ip6_int(A,B,C,D,E,F,G,H) ->
    dnib(H) ++ dnib(G) ++ dnib(F) ++ dnib(E) ++ 
	dnib(D) ++ dnib(C) ++ dnib(B) ++ dnib(A) ++ "ip6.int".

dn_in_addr_arpa(A,B,C,D) ->
    integer_to_list(D) ++ "." ++
	integer_to_list(C) ++ "." ++
	integer_to_list(B) ++ "." ++
	integer_to_list(A) ++ ".in-addr.arpa".

dnib(X) ->
    [ hex(X), $., hex(X bsr 4), $., hex(X bsr 8), $., hex(X bsr 12), $.].

hex(X) ->
    X4 = (X band 16#f),
    if X4 < 10 -> X4 + $0;
       true -> (X4-10) + $a
    end.

%% -------------------------------------------------------------------
%% Refresh cache at regular intervals, i.e. delete expired #dns_rr's.
%% -------------------------------------------------------------------
init_timer() ->
    erlang:send_after(cache_refresh(), self(), refresh_timeout).

stop_timer(undefined) ->
    undefined;
stop_timer(Timer) ->
    erlang:cancel_timer(Timer).

cache_refresh() ->
    case db_get(cache_refresh_interval) of
	undefined -> ?CACHE_REFRESH;
	Val       -> Val
    end.

%% Delete all entries with expired TTL.
%% Returns the access time of the entry with the oldest access time
%% in the cache.
do_refresh_cache(CacheDb) ->
    Now = times(),
    do_refresh_cache(ets:first(CacheDb), CacheDb, Now, Now).

do_refresh_cache('$end_of_table', _, _, OldestT) ->
    OldestT;
do_refresh_cache(Key, CacheDb, Now, OldestT) ->
    Fun = fun(RR, T) when RR#dns_rr.tm + RR#dns_rr.ttl < Now ->
		  ets:match_delete(CacheDb, RR),
		  T;
	     (#dns_rr{cnt = C}, T) when C < T ->
		  C;
	     (_, T) ->
		  T
	  end,
    Next = ets:next(CacheDb, Key),
    OldT = lists:foldl(Fun, OldestT, ets:lookup(CacheDb, Key)),
    do_refresh_cache(Next, CacheDb, Now, OldT).

%% -------------------------------------------------------------------
%% Allocate room for a new entry in the cache.
%% Deletes entries with expired TTL and all entries with latest
%% access time older than
%% trunc((TM - OldestTM) * 0.3) + OldestTM from the cache if it
%% is full. Does not delete more than 10% of the entries in the cache
%% though, unless they there deleted due to expired TTL.
%% Returns: true if space for a new entry otherwise false.
%% -------------------------------------------------------------------
alloc_entry(Db, CacheDb, TM) ->
    CurSize = ets:info(CacheDb, size),
    case ets:lookup(Db, cache_size) of
	[{cache_size, Size}] when Size =< CurSize, Size > 0 ->
	    alloc_entry(CacheDb, CurSize, TM, trunc(Size * 0.1) + 1);
	[{cache_size, Size}] when Size =< 0 ->
	    false;
	_ ->
	    true
    end.

alloc_entry(CacheDb, OldSize, TM, N) ->
    OldestTM = do_refresh_cache(CacheDb),     % Delete timedout entries
    case ets:info(CacheDb, size) of
	OldSize ->
	    %% No entrys timedout
	    delete_n_oldest(CacheDb, TM, OldestTM, N);
	_ ->
	    true
    end.

delete_n_oldest(CacheDb, TM, OldestTM, N) ->
    DelTM = trunc((TM - OldestTM) * 0.3) + OldestTM,
    case delete_older(CacheDb, DelTM, N) of
	0 ->
	    false;
	_ ->
	    true
    end.

%% Delete entries with latest access time older than TM.
%% Delete max N number of entries.
%% Returns the number of deleted entries.
delete_older(CacheDb, TM, N) ->
    delete_older(ets:first(CacheDb), CacheDb, TM, N, 0).

delete_older('$end_of_table', _, _, _, M) ->
    M;
delete_older(_, _, _, N, M) when N =< M ->
    M;
delete_older(Domain, CacheDb, TM, N, M) ->
    Next = ets:next(CacheDb, Domain),
    Fun = fun(RR, MM) when RR#dns_rr.cnt =< TM ->
		  ets:match_delete(CacheDb, RR),
		  MM + 1;
	     (_, MM) ->
		  MM
	  end,
    M1 = lists:foldl(Fun, M, ets:lookup(CacheDb, Domain)),
    delete_older(Next, CacheDb, TM, N, M1).


