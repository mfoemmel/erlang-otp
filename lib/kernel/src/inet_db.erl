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
-export([tcp_module/0, set_tcp_module/1]).
-export([udp_module/0, set_udp_module/1]).

%% Host name & domain
-export([set_hostname/1, set_domain/1]).
-export([gethostname/0]).

%% file interface
-export([add_host/2, del_host/1, clear_hosts/0, add_hosts/1]).
-export([add_resolv/1]).
-export([add_rc/1, add_rc_bin/1, add_rc_list/1, get_rc/0]).

-export([res_option/1]).
-export([socks_option/1]).
-export([gethostbyname/1, gethostbyname/2]).
-export([gethostbyaddr/1]).
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

-include("inet.hrl").
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

set_recurse(Flag) -> 
    call({set_recurse, Flag}).

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
translate_lookup(["file" | Ls]) -> [file | translate_lookup(Ls)];
translate_lookup(["yp" | Ls]) -> [yp | translate_lookup(Ls)];
translate_lookup(["nis" | Ls]) -> [nis | translate_lookup(Ls)];
translate_lookup(["nisplus" | Ls]) -> [nisplus | translate_lookup(Ls)];
translate_lookup(["native" | Ls]) -> [native | translate_lookup(Ls)];
translate_lookup([_ | Ls]) -> translate_lookup(Ls);
translate_lookup([]) -> [].

%% Reconstruct a .inetrc sturcture from inet_db
get_rc() -> 
    get_rc([domain, nameserver, search, alt_nameserver,
	    socks5_server, socks5_methods, socks5_noproxy,
	    udp, tcp, host, cache_size, cache_refresh], []).

get_rc([domain | Ks], Ls) ->
    case db_get(res_domain) of
	"" -> get_rc(Ks, Ls);
	Dom -> get_rc(Ks, [{domain, Dom} | Ls])
    end;
get_rc([nameserver | Ks], Ls) ->
    get_rc_ns(db_get(res_ns), nameserver, Ks, Ls);
get_rc([search | Ks], Ls) ->
    case db_get(res_search) of
	[] -> get_rc(Ks, Ls);
	List -> get_rc(Ks, [{search, List} | Ls])
    end;
get_rc([alt_nameserver | Ks], Ls) ->
    get_rc_ns(db_get(res_alt_ns), alt_nameserver, Ks, Ls); 
get_rc([socks5_server | Ks], Ls) ->
    case db_get(socks5_server) of
	"" -> get_rc(Ks, Ls);
	Srv -> get_rc(Ks, [{socks5_server,Srv} | Ls])
    end;
get_rc([socks5_port | Ks], Ls) ->
    case db_get(socks5_port) of
	?IPPORT_SOCKS -> get_rc(Ks, Ls);
	Port -> get_rc(Ks, [{socks5_port,Port} | Ls])
    end;
get_rc([socks5_methods | Ks], Ls) ->
    case db_get(socks5_methods) of
	[none] -> get_rc(Ks, Ls);
	Methods -> get_rc(Ks, [{socks5_methods,Methods} | Ls])
    end;
get_rc([socks5_noproxy | Ks], Ls) ->
    case db_get(socks5_noproxy) of
	[] -> get_rc(Ks, Ls);
	NoProxy -> get_rc_noproxy(NoProxy, Ks, Ls)
    end;
get_rc([udp | Ks], Ls) ->
    case db_get(udp_module) of
	?DEFAULT_UDP_MODULE -> get_rc(Ks, Ls);
	Mod -> get_rc(Ks, [{udp, Mod} | Ls])
    end;
get_rc([tcp | Ks], Ls) ->
    case db_get(tcp_module) of
	?DEFAULT_TCP_MODULE -> get_rc(Ks, Ls);
	Mod -> get_rc(Ks, [{tcp, Mod} | Ls])
    end;
get_rc([cache_size | Ks], Ls) ->
    case db_get(cache_size) of
	?CACHE_LIMIT -> get_rc(Ks, Ls);
	Size -> get_rc(Ks, [{cache_size, Size} | Ls])
    end;
get_rc([cache_refresh | Ks], Ls) ->
    Default = ?CACHE_REFRESH,
    case db_get(cache_refresh_interval) of
	Default -> get_rc(Ks, Ls);
	Interval -> get_rc(Ks, [{cache_refresh, Interval} | Ls])
    end;
get_rc([_ | Ks], Ls) ->
    get_rc(Ks, Ls);
get_rc([], Ls) -> reverse(Ls).

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
res_option(next_id)        -> ets:update_counter(inet_db, res_id, 1);
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

%%
%% gethostbyname (cache version)
%%
gethostbyname(Name) ->
    gethostbyname1(Name, a).

gethostbyname(Name, inet) ->
    gethostbyname1(Name, a);
gethostbyname(Name, inet6) ->
    gethostbyname1(Name, aaaa);
gethostbyname(_, _) ->
    {error, formerr}.

gethostbyname1(Name,Type) ->
    case inet_parse:dots(Name) of
	{0, false} ->
	    gethostbysearch(Name, ".", res_option(search),Type);
	{0, true} ->
	    gethostbysearch(Name, "", res_option(search),Type);
	_ ->
	    hostent_by_domain(Name,Type)
    end.

gethostbysearch(Name, Dot, [Dom | Ds], Type) ->
    case hostent_by_domain(Name ++ Dot ++ Dom, Type) of
	{ok, HEnt} -> {ok, HEnt};
	Error -> gethostbysearch(Name, Dot, Ds, Type)
    end;
gethostbysearch(Name, Dot, [], Type) ->
    hostent_by_domain(Name, Type).


make_hostent(Name, Addrs, Aliases, a) ->
    #hostent {
	      h_name = Name,
	      h_addrtype = inet,
	      h_length = 4,
	      h_addr_list = Addrs,
	      h_aliases = Aliases
	     };
make_hostent(Name, Addrs, Aliases, aaaa) ->
    #hostent {
	      h_name = Name,
	      h_addrtype = inet6,
	      h_length = 16,
	      h_addr_list = Addrs,
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
    [R#dns_rr.data || R <- lookup_rr(Domain, in, cname) ].

%%
%% gethostbyaddr (cache version)
%% match data field directly
%%
gethostbyaddr(IP) when tuple(IP) ->
    Type = if size(IP) == 4 -> a;
	      size(IP) == 8 -> aaaa;
	      true -> null
	   end,
    case match_rr(#dns_rr { domain = '_', class = in, type = Type,
			   cnt = '_', tm = '_', ttl = '_',
			   bm = '_', func = '_', data = IP }) of
	[] -> {error, nxdomain};
	[RR | TR] ->
	    %% debug
	    if TR /= [] ->
		    ?dbg("gethostbyaddr found extra=~p~n", [TR]);
	       true -> ok
	    end,
	    H = #hostent {
			  h_name = RR#dns_rr.domain,
			  h_aliases = lookup_cname(RR#dns_rr.domain),
			  h_addr_list = [IP]
			 },
	    case Type of
		a -> 
		    {ok, H#hostent { h_addrtype = inet, h_length = 4}};
		aaaa ->
		    {ok, H#hostent { h_addrtype = inet6, h_length = 16}};
		_ ->
		    {error, formerr}
	    end
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
    Db = ets:new(inet_db, [public, named_table]),
    reset_db(Db),
    Cache = ets:new(inet_cache, [public, bag, {keypos,2}, named_table]),
    Hosts = ets:new(inet_hosts, [public, named_table]),
    {ok, #state{
		db = Db,
		cache = Cache,
		hosts = Hosts,
		cache_timer = undefined }}.


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
	    RR2 = transform_rr(RR1),
	    ?dbg("add_rr: ~p~n", [RR2]),
	    cache_rr(Db, State#state.cache, RR2#dns_rr { tm = times() }),
	    {reply, ok, State};

	{del_rr, RR} when record(RR, dns_rr) ->
	    RR1 = lower_rr(RR),
	    %% note. del_rr will handle wildcards !!!
	    Cache = State#state.cache,
	    ets:match_delete(Cache, RR1),
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
		    ets:insert(Db, {res_search, [Dom | Ds]}),
		    {reply, ok, State};
		false ->
		    {reply, error, State}
	    end;

	{add_search, Dom} ->
	    case inet_parse:visible_string(Dom) of
		true ->
		    [{_,Ds}] = ets:lookup(Db, res_search),
		    ets:insert(Db, {res_search, Ds ++ [Dom]}),
		    {reply, ok, State};
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
	    {reply, ok, State};

	clear_hosts ->
	    ets:match_delete(State#state.hosts, '_'),
	    {reply, ok, State};

	clear_cache ->
	    ets:match_delete(State#state.cache, '_'),
	    {reply, ok, State};

	reset ->
	    reset_db(Db),
	    {reply, ok, State};

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
handle_info(Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

times() ->
    {Mega,Secs,_} = erlang:now(),
    Mega*1000000 + Secs.

cache_rr(Db, Cache, RR) ->
    %% delete possible old entry
    ets:match_delete(Cache, RR#dns_rr { cnt = '_', tm = '_', ttl = '_',
				        bm = '_', func = '_'}),
    ets:insert(Cache, RR).

%% lookup and remove old entries

lookup_rr(Domain, Class, Type) ->
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
    Cnt = RR#dns_rr.cnt,
    ets:match_delete(inet_cache, RR),
    ets:insert(inet_cache, RR#dns_rr { cnt = Cnt+1 }),
    [RR | filter_rr(RRs, Time)];
filter_rr([], Time) ->  [].


%%
%% Hack to transform query result on form:
%% "D.C.B.A.IN-ADDR.ARPA" in ptr => {A,B,C,D} in a
%% "x1.x2.x3.x4....IPV6.INT"  in ptr => {A,B,C,D,E,F,G,H} in aaa
%%
%%
transform_rr(RR) when RR#dns_rr.class == in, RR#dns_rr.type == ptr ->
    case reverse(RR#dns_rr.domain) of
	["apra.rdda-ni." ++ T] ->
	    case catch arpa_addr_in_dn(T) of
		{'EXIT', _} -> RR;
		IP ->
		    #dns_rr { domain = tolower(RR#dns_rr.data),
			      class = in, type = a,
			     ttl = RR#dns_rr.ttl,
			     data = IP }
	    end;
	["tni.6pi" ++ T] when length(T) == 64 ->
	    case catch int_ip6_dn(T) of
		{'EXIT',_} -> RR;
		IP ->
		    #dns_rr { domain = tolower(RR#dns_rr.data),
			     class = in, type = aaaa,
			     ttl = RR#dns_rr.ttl,
			     data = IP }
	    end;
	_ -> RR
    end;
transform_rr(RR) -> RR.

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



