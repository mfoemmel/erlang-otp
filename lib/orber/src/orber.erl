%%--------------------------------------------------------------------
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
%%-----------------------------------------------------------------
%% File: orber.erl
%% 
%% Description:
%%    This file contains the Orber application interface
%%
%% Creation date: 970407
%%
%%-----------------------------------------------------------------
-module(orber).

-include_lib("orber/include/corba.hrl").
-include_lib("orber/src/orber_iiop.hrl").
-include_lib("orber/src/ifr_objects.hrl").
%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/0, start/1, stop/0, install/1, install/2, orber_nodes/0, iiop_port/0,
	 domain/0, bootstrap_port/0, iiop_ssl_port/0, iiop_out_ports/0,
	 ssl_server_certfile/0, ssl_client_certfile/0, set_ssl_client_certfile/1,
	 ssl_server_verify/0, ssl_client_verify/0, set_ssl_client_verify/1,
	 ssl_server_depth/0, ssl_client_depth/0, set_ssl_client_depth/1,
	 ssl_server_cacertfile/0,ssl_client_cacertfile/0, set_ssl_client_cacertfile/1,
	 ssl_client_keyfile/0, ssl_client_password/0, ssl_server_keyfile/0, ssl_server_password/0, 
	 ssl_client_ciphers/0, ssl_server_ciphers/0, ssl_client_cachetimeout/0, ssl_server_cachetimeout/0,
	 uninstall/0, giop_version/0, info/0, info/1, is_running/0, add_node/2, 
	 remove_node/1, iiop_timeout/0, iiop_connection_timeout/0, 
	 iiop_setup_connection_timeout/0, objectkeys_gc_time/0,
	 is_lightweight/0, get_lightweight_nodes/0,
	 start_lightweight/0, start_lightweight/1,
	 get_ORBDefaultInitRef/0, get_ORBInitRef/0,
	 get_interceptors/0, get_local_interceptors/0, 
	 get_cached_interceptors/0, set_interceptors/1,
	 jump_start/0, jump_start/1, jump_start/2, jump_stop/0,
	 iiop_connections/0, iiop_connections/1, iiop_connections_pending/0, 
	 typechecking/0,
	 exclude_codeset_ctx/0, exclude_codeset_component/0, bidir_context/0, use_FT/0,
	 use_CSIv2/0, get_flags/0, secure/0, multi_jump_start/1, multi_jump_start/2, 
	 multi_jump_start/3, get_tables/0, iiop_in_connection_timeout/0, 
	 partial_security/0, nat_iiop_ssl_port/0, nat_iiop_port/0, ip_version/0,
	 light_ifr/0, iiop_max_in_requests/0, iiop_max_in_connections/0, 
	 iiop_max_fragments/0, iiop_backlog/0, iiop_ssl_backlog/0]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([nat_host/0, host/0, ip_address_variable_defined/0, start/2, init/1,
	 get_debug_level/0, debug_level_print/3, dbg/3, error/3,
	 exception_info/1, configure/2, configure_override/2, multi_configure/1,
	 mjs/1, mjs/2, js/0, js/1]).

%%-----------------------------------------------------------------
%% Internal definitions
%%-----------------------------------------------------------------
%% Defines possible configuration parameters a user can add when,
%% for example, installing Orber.
-record(options, {ifr_storage_type = disc_copies,
		  install_timeout = infinity,
		  local_content = false,
		  nameservice_storage_type = ram_copies,
		  initialreferences_storage_type = ram_copies,
		  type = temporary}).

-define(ORBER_TABS, [orber_CosNaming, orber_objkeys, orber_references]).

-define(DEBUG_LEVEL, 5).

-define(FORMAT(_F, _A), lists:flatten(io_lib:format(_F, _A))).
-define(EFORMAT(_F, _A), exit(lists:flatten(io_lib:format(_F, _A)))).


%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------

jump_stop() ->
    stop(),
    uninstall(),
    mnesia:stop().

js() ->
    application:load(orber),
    jump_start(iiop_port(), [{interceptors, {native, [orber_iiop_tracer_silent]}},
			     {orber_debug_level, 10}, 
			     {flags, (?ORB_ENV_LOCAL_TYPECHECKING bor get_flags())}]).

js(Port) ->
    application:load(orber),
    jump_start(Port, [{interceptors, {native, [orber_iiop_tracer_silent]}},
		      {orber_debug_level, 10}, 
		      {flags, (?ORB_ENV_LOCAL_TYPECHECKING bor get_flags())}]).

jump_start() ->
    application:load(orber),
    jump_start(iiop_port(), []).


jump_start(Port) ->
    application:load(orber),
    jump_start(Port, []).

jump_start(Port, InitOptions) when integer(Port), list(InitOptions) ->
    Domain = ip_address() ++ [$:|integer_to_list(Port)],
    Options = lists:keydelete(iiop_port, 1, InitOptions),
    mnesia:start(),
    corba:orb_init([{iiop_port, Port}, {domain, Domain}|Options]),
    install([node()], [{ifr_storage_type, ram_copies}]),
    start(),
    info();
jump_start(Port, Options) ->
    exit({error, Port, Options}). 


mjs(Nodes) ->
    application:load(orber),
    multi_js_helper(Nodes, iiop_port(),
		    [{interceptors, {native, [orber_iiop_tracer_silent]}},
		     {orber_debug_level, 10}, 
		     {flags, (?ORB_ENV_LOCAL_TYPECHECKING bor get_flags())}]).

mjs(Nodes, Port) ->
    application:load(orber),
    multi_js_helper(Nodes, Port, 
		    [{interceptors, {native, [orber_iiop_tracer_silent]}},
		     {orber_debug_level, 10},
		     {flags, (?ORB_ENV_LOCAL_TYPECHECKING bor get_flags())}]).


multi_jump_start(Nodes) ->
    application:load(orber),
    multi_js_helper(Nodes, iiop_port(), []).

multi_jump_start(Nodes, Port) ->
    multi_js_helper(Nodes, Port, []).

multi_jump_start(Nodes, Port, Options) ->
    multi_js_helper(Nodes, Port, Options).

multi_js_helper(Nodes, Port, InitOptions) when list(Nodes), integer(Port), 
					       list(InitOptions) ->
    Domain = ip_address() ++ [$:|integer_to_list(Port)],
    %% We MUST delete the option iiop_port.
    InitOptions2 = lists:keydelete(iiop_port, 1, InitOptions),
    Options = [{domain, Domain}|InitOptions2],
    case node() of
	nonode@nohost ->
	    {error, "The distribution is not started"};
	_ ->
	    mnesia:start(),
	    corba:orb_init([{iiop_port, Port}|Options]),
	    install([node()], [{ifr_storage_type, ram_copies}]),
	    start(),
	    case jump_start_slaves(Nodes, Port, Options, [], []) of
		{ok, NodeData} ->
		    info(),
		    {ok, [{node(), Port}|NodeData]};
		Other ->
		    Other
	    end
    end.

jump_start_slaves([], _, _, [], NodeData) ->
    rpc:multicall([node() | nodes()], global, sync, []),
    {ok, NodeData};
jump_start_slaves([], _, _, Errors, _) ->
    {error, Errors};
jump_start_slaves([{Host, N}|T], Port, Options, Errors, NodeData) ->
    case create_nodes(Host, N, Port, Options, Errors, NodeData) of
	{ok, NewNodeData} ->
	    jump_start_slaves(T, Port, Options, Errors, NewNodeData);
	{error, NewErrors} ->
	    jump_start_slaves(T, Port, Options, NewErrors, NodeData)
    end;
jump_start_slaves([Host|T], Port, Options, Errors, NodeData) ->
    case catch create_node(Host, Port+1, Options) of
	{ok, NewNode} ->
	    jump_start_slaves(T, Port, Options, Errors, [{NewNode, Port+1}|NodeData]);
	{error, Reason} ->
	    jump_start_slaves(T, Port, Options, [{Host, Port, Reason}|Errors], 
			      NodeData);
	Other ->
	    jump_start_slaves(T, Port, Options, [{Host, Port, Other}|Errors], 
			      NodeData)
    end.

create_nodes(_, 0, _, _, [], NodeData) ->
    {ok, NodeData};
create_nodes(_, 0, _, _, Errors, _) ->
    {error, Errors};
create_nodes(Host, N, Port, Options, Errors, NodeData) ->
    case catch create_node(Host, Port+N, Options) of
	{ok, NewNode} ->
	    create_nodes(Host, N-1, Port, Options, Errors, 
			 [{NewNode, Port+N}|NodeData]);
	{error, Reason} ->
	    create_nodes(Host, N-1, Port, Options, 
			 [{Host, Port+N, Reason}|Errors], NodeData);
	Other ->
	    create_nodes(Host, N-1, Port, Options, 
			 [{Host, Port+N, Other}|Errors], NodeData)
    end.
    

create_node(Host, Port, Options) ->
    case slave:start_link(Host, Port) of
	{ok, NewNode} ->
	    case net_adm:ping(NewNode) of
		pong ->
		    ok = rpc:call(NewNode, mnesia, start, []),
		    {ok,_} = rpc:call(NewNode, mnesia, change_config, [extra_db_nodes, [node()]]),
		    ok = rpc:call(NewNode, corba, orb_init, [[{iiop_port, Port}|Options]]),
		    ok = rpc:call(NewNode, orber, add_node, [NewNode, ram_copies]),
		    {ok, NewNode};
		_ ->
		    {error, "net_adm:ping(Node) failed"}
	    end;
        {error, Reason} ->
            {error, Reason}
    end.


start() ->
    start(temporary).

start(Type) when Type == permanent; Type == temporary ->
    application:start(mnesia),
    TableTest = test_tables(),
    case lists:member(not_member, TableTest) of
	true ->
	    exit({error,"Orber Mnesia Table(s) missing. Orber not properly installed."});
	_->
	    try_starting(Type, false)
    end.

start_lightweight() ->
    application:start(orber).

start_lightweight(Nodes) when list(Nodes) ->
    %%------- WARNING!!! -------
    %% Here we use an undocumented function, i.e., set_env/3. IF it stops
    %% being exported we must solve this problem in another way!!!
    case is_loaded() of
	false ->
	    application:load(orber),
	    application_controller:set_env(orber, lightweight, Nodes),
	    application:start(orber);
	true ->
	    application_controller:set_env(orber, lightweight, Nodes),
	    application:start(orber)
    end;
start_lightweight(_) ->
    exit({error,"Argument not correct; must be a list of nodes."}).

stop() ->
    application:stop(orber).


get_tables() ->
    case light_ifr() of
	false ->
	    ?ifr_object_list++?ORBER_TABS;
	true ->
	    ?ifr_light_object_list ++?ORBER_TABS
    end.

iiop_port() ->
    case application:get_env(orber, iiop_port) of
	{ok, Port} when integer(Port), Port >= 0 ->
	    Port;
	_ ->
	    4001
    end.

nat_iiop_port() ->
    case application:get_env(orber, nat_iiop_port) of
	{ok, Port} when integer(Port), Port > 0 ->
	    Port;
	_ ->
	    iiop_port()
    end.

iiop_out_ports() ->
    case application:get_env(orber, iiop_out_ports) of
	{ok, {Min, Max}} when integer(Min), integer(Max), Min =< Max ->
	    {Min, Max};
	{ok, {Max, Min}} when integer(Min), integer(Max), Min < Max ->
	    {Min, Max};
	_ ->
	    0
    end.

bootstrap_port() ->
    case application:get_env(orber, bootstrap_port) of
	{ok, Port} when integer(Port), Port > 0 ->
	    Port;
	_ ->
	    iiop_port()
    end.

orber_nodes() ->
    case catch mnesia:table_info(orber_objkeys,ram_copies) of
	Nodes when list(Nodes) ->
	    Nodes;
	_ ->
	    [node()]
    end.

domain() -> 
    case application:get_env(orber, domain) of
	{ok, Domain} when list(Domain) ->
	    Domain;
	{ok, Domain} when atom(Domain) ->
	    atom_to_list(Domain);
	_ ->
	    "ORBER"
    end.

ip_address_variable_defined() ->
    case application:get_env(orber, ip_address) of
	undefined ->
	    false;
	{ok,{multiple, _}} ->
	    false;
	_ ->
	    [Host] = host(),
	    Host
    end.

nat_host() ->
    case application:get_env(orber, nat_ip_address) of
	{ok,I} when list(I) ->
	    [I];
	{ok,{multiple, [I|_] = IList}} when list(I) ->
	    IList;
	_ ->
	    host()
    end.

host() ->
    case application:get_env(orber, ip_address) of
	{ok,I} when list(I) ->
	    [I];
	{ok,{multiple, [I|_] = IList}} when list(I) ->
	    IList;
	%% IPv4. For IPv6 we only accept a string, but we must support this format
	%% for IPv4 
	{ok, {A1, A2, A3, A4}} when integer(A1+A2+A3+A4) ->
	    [integer_to_list(A1) ++ "." ++ integer_to_list(A2) ++ "." ++ integer_to_list(A3)
	     ++ "." ++ integer_to_list(A4)];
	_ ->
	    Flags = get_flags(),
	    case ?ORB_FLAG_TEST(Flags, ?ORB_ENV_HOSTNAME_IN_IOR) of
		true ->
		    {ok, Hostname} = inet:gethostname(),
		    [Hostname];
		_ ->
		    case ?ORB_FLAG_TEST(Flags, ?ORB_ENV_USE_IPV6) of
			false ->
			    [ip_address(inet)];
			true ->
			    [ip_address(inet6)]
		    end
	    end
    end.

ip_address() ->
    ip_address(ip_version()).

ip_address(inet) ->
    {ok, Hostname} = inet:gethostname(),
    {ok, {A1, A2, A3, A4}} = inet:getaddr(Hostname, inet),
    integer_to_list(A1) ++ "." ++ integer_to_list(A2) ++ "." ++ integer_to_list(A3)
	++ "." ++ integer_to_list(A4);
ip_address(inet6) ->
    {ok, Hostname} = inet:gethostname(),
    {ok, {A1, A2, A3, A4, A5, A6, A7, A8}} = inet:getaddr(Hostname, inet6),
    integer_to_list(A1) ++ ":" ++ integer_to_list(A2) ++ ":" ++ integer_to_list(A3)
	++ ":" ++ integer_to_list(A4) ++ ":" ++ integer_to_list(A5) ++ ":" ++ 
	integer_to_list(A6) ++ ":" ++ integer_to_list(A7) ++ ":" ++ 
	integer_to_list(A8).

giop_version() ->
    case application:get_env(orber, giop_version) of
	{ok, {Major, Minor}} ->
	    {Major, Minor};
	_ ->
	    {1, 1}
    end.

iiop_timeout() ->
    case application:get_env(orber, iiop_timeout) of
	{ok, Int} when integer(Int) ->
	    if
		Int > 1000000 ->
		    error_logger:error_msg("Orber 'iiop_timeout' badly configured.~n"
					   "Time to large (>1000000 sec), swithed to 'infinity'~n"),
		    infinity;
		true ->
		    %% Convert to msec.
		    Int*1000
	    end;
	_ ->
	    infinity
    end.

iiop_connection_timeout() ->
    case application:get_env(orber, iiop_connection_timeout) of
	{ok, Int} when integer(Int) ->
	    if
		Int > 1000000 ->
		    error_logger:error_msg("Orber 'iiop_connection_timeout' badly configured.~n"
					   "Time to large (>1000000 sec), swithed to 'infinity'~n"),
		    infinity;
		true ->
		    %% Convert to msec.
		    Int*1000
	    end;
	_ ->
	    infinity
    end.

iiop_setup_connection_timeout() ->
    case application:get_env(orber, iiop_setup_connection_timeout) of
	{ok, Int} when integer(Int) ->
            %% Convert to msec.
	    Int*1000;
	_ ->
	    infinity
    end.

iiop_in_connection_timeout() ->
    case application:get_env(orber, iiop_in_connection_timeout) of
	{ok, Int} when integer(Int) ->
	    if
		Int > 1000000 ->
		    error_logger:error_msg("Orber 'iiop_connection_timeout' badly configured.~n"
					   "Time to large (>1000000 sec), swithed to 'infinity'~n"),
		    infinity;
		true ->
		    %% Convert to msec.
		    Int*1000
	    end;
	_ ->
	    infinity
    end.
    


iiop_connections() ->
    iiop_connections(inout).

iiop_connections(inout) ->
    orber_iiop_pm:list_existing_connections() ++ orber_iiop_net:connections();
iiop_connections(in) ->
    orber_iiop_net:connections();
iiop_connections(out) ->
    orber_iiop_pm:list_existing_connections().

iiop_connections_pending() ->
    orber_iiop_pm:list_setup_connections().

iiop_max_fragments() ->
    case application:get_env(orber, iiop_max_fragments) of
	{ok, Max} when integer(Max), Max > 0 ->
	    Max;
	_ ->
	    infinity
    end.
    
iiop_max_in_requests() ->
    case application:get_env(orber, iiop_max_in_requests) of
	{ok, Max} when integer(Max), Max > 0 ->
	    Max;
	_ ->
	    infinity
    end.

iiop_max_in_connections() ->
    case application:get_env(orber, iiop_max_in_connections) of
	{ok, Max} when integer(Max), Max > 0 ->
	    Max;
	_ ->
	    infinity
    end.

iiop_backlog() ->
    case application:get_env(orber, iiop_backlog) of
	{ok, Int} when integer(Int), Int >= 0 ->
	    Int;
	_ ->
	    5
    end.


get_flags() ->
    case get(oe_orber_flags) of
	undefined ->
	    case application:get_env(orber, flags) of
		undefined ->
		    put(oe_orber_flags, ?ORB_ENV_INIT_FLAGS),
		    ?ORB_ENV_INIT_FLAGS;
		{ok, Flags} ->
		    put(oe_orber_flags, Flags),
		    Flags
	    end;
	Flags when integer(Flags) ->
	    Flags
    end.

typechecking() ->
    ?ORB_FLAG_TEST(get_flags(), ?ORB_ENV_LOCAL_TYPECHECKING).

exclude_codeset_ctx() ->
    ?ORB_FLAG_TEST(get_flags(), ?ORB_ENV_EXCLUDE_CODESET_CTX).

exclude_codeset_component() ->
    ?ORB_FLAG_TEST(get_flags(), ?ORB_ENV_EXCLUDE_CODESET_COMPONENT).

partial_security() ->
    ?ORB_FLAG_TEST(get_flags(), ?ORB_ENV_PARTIAL_SECURITY).

use_CSIv2() ->
    ?ORB_FLAG_TEST(get_flags(), ?ORB_ENV_USE_CSIV2).

use_FT() ->
    ?ORB_FLAG_TEST(get_flags(), ?ORB_ENV_USE_FT).

ip_version() ->
    case ?ORB_FLAG_TEST(get_flags(), ?ORB_ENV_USE_IPV6) of
	false ->
	    inet;
	true ->
	    inet6
    end.

light_ifr() ->
    ?ORB_FLAG_TEST(get_flags(), ?ORB_ENV_LIGHT_IFR).

bidir_context() ->
    Flags = get_flags(),
    if
	?ORB_FLAG_TEST(Flags, ?ORB_ENV_USE_BI_DIR_IIOP) ->
	    [#'IOP_ServiceContext'
	     {context_id=?IOP_BI_DIR_IIOP,
	      context_data = 
	      #'IIOP_BiDirIIOPServiceContext'{listen_points = 
					      [#'IIOP_ListenPoint'{host=host(), 
								   port=iiop_port()}]}}];
	true ->
	    []
    end.


objectkeys_gc_time() ->
    case application:get_env(orber, objectkeys_gc_time) of
	{ok, Int} when integer(Int) ->
	    if
		Int > 1000000 ->
		    error_logger:error_msg("Orber 'objectkeys_gc_time' badly configured.~n"
					   "Time to large (>1000000 sec), swithed to 'infinity'~n"),
		    infinity;
		true ->
		    Int
	    end;
	_ ->
	    infinity
    end.

%%-----------------------------------------------------------------
%% CosNaming::NamingContextExt operations
%%-----------------------------------------------------------------
get_ORBInitRef() ->
    case application:get_env(orber, orbInitRef) of
	{ok, Ref} when list(Ref) ->
	    Ref;
	_ ->
	    undefined
    end.

get_ORBDefaultInitRef() ->
    case application:get_env(orber, orbDefaultInitRef) of
	{ok, Ref} when list(Ref) ->
	    Ref;
	_ ->
	    undefined
    end.

%%-----------------------------------------------------------------
%% Interceptor opertaions (see orber_pi.erl)
%%-----------------------------------------------------------------
get_interceptors() ->
    case application:get_env(orber, interceptors) of
	{ok, {native, PIs}} when list(PIs) ->
	    {native, PIs};
	{ok, {portable, PIs}} when list(PIs) ->
	    {portable, PIs};
	_ ->
	    false
    end.

get_local_interceptors() ->
    case application:get_env(orber, local_interceptors) of
	{ok, {native, PIs}} when list(PIs) ->
	    {native, PIs};
	{ok, {portable, PIs}} when list(PIs) ->
	    {portable, PIs};
	_ ->
	    false
    end.


get_cached_interceptors() ->
    case get(oe_orber_interceptor_cache) of
	undefined ->
	    PIs = case application:get_env(orber, local_interceptors) of
		      {ok, {native, LPIs}} when list(LPIs) ->
			  {native, LPIs};
		      {ok, {portable, LPIs}} when list(LPIs) ->
			  {portable, LPIs};
		      _ ->
			  get_interceptors()
		  end,
	    put(oe_orber_interceptor_cache, PIs),
	    PIs;
	PIs ->
	    PIs
    end.


set_interceptors({Type, InterceptorList}) when list(InterceptorList) ->
    configure(interceptors, {Type, InterceptorList});
set_interceptors(_) ->
    exit({error, "Usage: {Type, ModuleList}"}).


%%-----------------------------------------------------------------
%% Light weight Orber operations
%%-----------------------------------------------------------------
is_lightweight() ->
    case application:get_env(orber, lightweight) of
	{ok, L} when list(L) ->
	    true;
	_ ->
	    false
    end.
get_lightweight_nodes() ->
    case application:get_env(orber, lightweight) of
	{ok, L} when list(L) ->
	    L;
	_ ->
	    false
    end.
   

%%-----------------------------------------------------------------
%% Security access operations (SSL)
%%-----------------------------------------------------------------
secure() ->
    case application:get_env(orber, secure) of
	{ok, V} ->
	    V;
	_ ->
	    no
    end.
 
iiop_ssl_backlog() ->
    case application:get_env(orber, iiop_ssl_backlog) of
	{ok, Int} when integer(Int), Int >= 0 ->
	    Int;
	_ ->
	    5
    end.


iiop_ssl_port() ->
    case application:get_env(orber, secure) of
	{ok, ssl} ->
	        case application:get_env(orber, iiop_ssl_port) of
		    {ok, Port} when integer(Port) ->
			Port;
		    _ ->
			4002
		end;
	_ ->
	    -1
    end.

nat_iiop_ssl_port() ->
    case application:get_env(orber, secure) of
	{ok, ssl} ->
	        case application:get_env(orber, nat_iiop_ssl_port) of
		    {ok, Port} when integer(Port), Port > 0 ->
			Port;
		    _ ->
			iiop_ssl_port()
		end;
	_ ->
	    -1
    end.

ssl_server_certfile() ->
    case application:get_env(orber, ssl_server_certfile) of
	{ok, V1}  when list(V1) ->
	    V1;
	{ok, V2}  when atom(V2) ->
	    atom_to_list(V2);
	_ ->
	    []
    end.
    
ssl_client_certfile() ->
    case get(ssl_client_certfile) of
	undefined ->
	    case application:get_env(orber, ssl_client_certfile) of
		{ok, V1}  when list(V1) ->
		    V1;
		{ok, V2}  when atom(V2) ->
		    atom_to_list(V2);
		_ ->
		    []
	    end;
	V ->
	    V
    end.

set_ssl_client_certfile(Value) when list(Value) ->
    put(ssl_client_certfile, Value).
    
ssl_server_verify() ->
    Verify = case application:get_env(orber, ssl_server_verify) of
	{ok, V} when integer(V) ->
	    V;
	_ ->
	    0
    end,
    if
	Verify =< 2, Verify >= 0 ->
	    Verify;
	true ->
	   0
    end.
    
ssl_client_verify() ->
    Verify = case get(ssl_client_verify) of
		 undefined ->
		     case application:get_env(orber, ssl_client_verify) of
			 {ok, V1} when integer(V1) ->
			     V1;
			 _ ->
			     0
		     end;
		 V2 ->
		     V2
	     end,
    if
	Verify =< 2, Verify >= 0 ->
	    Verify;
	true ->
	   0
    end.

set_ssl_client_verify(Value) when integer(Value), Value =< 2, Value >= 0 ->
    put(ssl_client_verify, Value), ok.
    
ssl_server_depth() ->
    case application:get_env(orber, ssl_server_depth) of
	{ok, V1} when integer(V1) ->
	    V1;
	_ ->
	    1
    end.
    
ssl_client_depth() ->
    case get(ssl_client_depth) of
	undefined ->
	    case application:get_env(orber, ssl_client_depth) of
		{ok, V1} when integer(V1) ->
		    V1;
		_ ->
		    1
	    end;
	V2 ->
	    V2
    end.

set_ssl_client_depth(Value) when integer(Value) ->
    put(ssl_client_depth, Value), ok.
    


ssl_server_cacertfile() ->
    case application:get_env(orber, ssl_server_cacertfile) of
	{ok, V1}  when list(V1) ->
	    V1;
	{ok, V2}  when atom(V2) ->
	    atom_to_list(V2);
	_ ->
	    []
    end.
    
ssl_client_cacertfile() ->
    case get(ssl_client_cacertfile) of
	undefined ->
	    case application:get_env(orber, ssl_client_cacertfile) of
		{ok, V1}  when list(V1) ->
		    V1;
		{ok, V2}  when atom(V2) ->
		    atom_to_list(V2);
		_ ->
		    []
	    end;
	V3 ->
	    V3
    end.

set_ssl_client_cacertfile(Value) when list(Value) ->
    put(ssl_client_cacertfile, Value), ok.
    

ssl_client_password() ->
    case application:get_env(orber, ssl_client_password) of
	{ok, V1} when list(V1) ->
	    V1;
	_ ->
	    []
    end.

ssl_server_password() ->
    case application:get_env(orber, ssl_server_password) of
	{ok, V1} when list(V1) ->
	    V1;
	_ ->
	    []
    end.

ssl_client_keyfile() ->
    case application:get_env(orber, ssl_client_keyfile) of
	{ok, V1} when list(V1) ->
	    V1;
	_ ->
	    []
    end.

ssl_server_keyfile() ->
    case application:get_env(orber, ssl_server_keyfile) of
	{ok, V1} when list(V1) ->
	    V1;
	_ ->
	    []
    end.

ssl_client_ciphers() ->
    case application:get_env(orber, ssl_client_ciphers) of
	{ok, V1} when list(V1) ->
	    V1;
	_ ->
	    []
    end.

ssl_server_ciphers() ->
    case application:get_env(orber, ssl_server_ciphers) of
	{ok, V1} when list(V1) ->
	    V1;
	_ ->
	    []
    end.

ssl_client_cachetimeout() ->
    case application:get_env(orber, ssl_client_cachetimeout) of
	{ok, V1} when integer(V1) ->
	    V1;
	_ ->
	    infinity
    end.

ssl_server_cachetimeout() ->
    case application:get_env(orber, ssl_server_cachetimeout) of
	{ok, V1} when integer(V1) ->
	    V1;
	_ ->
	    infinity
    end.

%%-----------------------------------------------------------------
%% Configuration settings
%%-----------------------------------------------------------------
info() ->
    info(info_msg).

info(IoDevice) ->
    Info = 
	case is_running() of
	    true ->
		Info1 = create_main_info(),
		Info2 = create_flag_info(Info1),
		create_security_info(secure(), Info2);
	    _ ->
		?FORMAT("=== Orber-~-9s System Information ===~n"
			"       *** Orber is not running ***~n"
			"==========================================~n",
			[?ORBVSN])
	end,
    case IoDevice of
	info_msg ->
	    error_logger:info_msg(Info);
	string ->
	    Info;
	io ->
	    io:format(Info); 
	{io, Dev} ->
	    io:format(Dev, Info, []);
	_ ->
	    exit("Bad parameter")
    end.

create_main_info() ->
    {Major, Minor} = giop_version(),
    [io_lib:format("=== Orber-~-9s System Information ===~n"
		   "Orber domain..................: ~s~n"
		   "IIOP port number..............: ~p~n"
		   "IIOP NAT port number..........: ~p~n"
		   "Interface(s)..................: ~p~n"
		   "Interface(s) NAT..............: ~p~n"
		   "Nodes in domain...............: ~p~n"
		   "GIOP version (default)........: ~p.~p~n"
		   "IIOP out timeout..............: ~p msec~n"
		   "IIOP out connection timeout...: ~p msec~n"
		   "IIOP setup connection timeout.: ~p msec~n"
		   "IIOP out ports................: ~p~n"
		   "IIOP out connections..........: ~p~n"
		   "IIOP out connections (pending): ~p~n"
		   "IIOP in connections...........: ~p~n"
		   "IIOP in connection timeout....: ~p msec~n"
		   "IIOP max fragments............: ~p~n"
		   "IIOP max in requests..........: ~p~n"
		   "IIOP max in connections.......: ~p~n"
		   "IIOP backlog..................: ~p~n"
		   "Object Keys GC interval.......: ~p~n"
		   "Using Interceptors............: ~p~n"
		   "Using Local Interceptors......: ~p~n"
		   "Debug Level...................: ~p~n"
		   "orbInitRef....................: ~p~n"
		   "orbDefaultInitRef.............: ~p~n",
		   [?ORBVSN, domain(), iiop_port(), nat_iiop_port(), host(), 
		    nat_host(), orber_nodes(), Major, Minor,
		    iiop_timeout(), iiop_connection_timeout(), 
		    iiop_setup_connection_timeout(), iiop_out_ports(), 
		    iiop_connections(out), iiop_connections_pending(), 
		    iiop_connections(in), iiop_in_connection_timeout(), 
		    iiop_max_fragments(), iiop_max_in_requests(), 
		    iiop_max_in_connections(), iiop_backlog(),
		    objectkeys_gc_time(), get_interceptors(), 
		    get_local_interceptors(), get_debug_level(), get_ORBInitRef(),
		    get_ORBDefaultInitRef()])].

create_flag_info(Info) ->
    case application:get_env(orber, flags) of
	undefined ->
	    [Info, "System Flags Set..............: -\n"];
	{ok, Flags} ->
	    FlagData = check_flags(?ORB_ENV_FLAGS, Flags, []),
	    [Info, "System Flags Set..............: \n", FlagData, "\n"]
    end.
  
check_flags([], _, Acc) ->
    Acc;
check_flags([{Flag, Txt}|T], Flags, Acc) when ?ORB_FLAG_TEST(Flags, Flag) ->
    check_flags(T, Flags, ["   - ", Txt, "\n"|Acc]);
check_flags([_|T], Flags, Acc) ->
    check_flags(T, Flags, Acc).


create_security_info(no, Info) ->
    lists:flatten([Info, "=========================================\n"]);
create_security_info(ssl, Info) ->
    lists:flatten([Info, 
		   io_lib:format("ORB security..................: ssl~n"
				 "SSL IIOP port number..........: ~p~n"
				 "SSL IIOP NAT port number......: ~p~n"
				 "SSL IIOP backlog..............: ~p~n"
				 "SSL server certfile...........: ~p~n"
				 "SSL server verification type..: ~p~n"
				 "SSL server verification depth.: ~p~n"
				 "SSL server cacertfile.........: ~p~n"
				 "SSL server keyfile............: ~p~n"
				 "SSL server password...........: ~p~n"
				 "SSL server ciphers............: ~p~n"
				 "SSL server cachetimeout.......: ~p~n"
				 "SSL client certfile...........: ~p~n"
				 "SSL client verification type..: ~p~n"
				 "SSL client verification depth.: ~p~n"
				 "SSL client cacertfile.........: ~p~n"
				 "SSL client keyfile............: ~p~n"
				 "SSL client password...........: ~p~n"
				 "SSL client ciphers............: ~p~n"
				 "SSL client cachetimeout.......: ~p~n"
				 "=========================================~n",
				 [iiop_ssl_port(), nat_iiop_ssl_port(), 
				  iiop_ssl_backlog(), 
				  ssl_server_certfile(), ssl_server_verify(),
				  ssl_server_depth(), ssl_server_cacertfile(), 
				  ssl_server_keyfile(), ssl_server_password(), 
				  ssl_server_ciphers(), ssl_server_cachetimeout(),
				  ssl_client_certfile(), ssl_client_verify(), 
				  ssl_client_depth(), ssl_client_cacertfile(), 
				  ssl_client_keyfile(), ssl_client_password(),
				  ssl_client_ciphers(), ssl_client_cachetimeout()])]).


%%-----------------------------------------------------------------
%% EXCEPTION mapping
%%-----------------------------------------------------------------
exception_info(Exc) ->
    orber_exceptions:dissect(Exc).

%%-----------------------------------------------------------------
%% Installation interface functions
%%-----------------------------------------------------------------
install(Nodes) ->
    install(Nodes, []).

install([], Options) ->
    install([node()], Options);
install(Nodes, Options) when list(Nodes), list(Options)->
    case is_running() of
	false ->
	    application:load(orber),
	    case mnesia:system_info(is_running) of
		no ->
		    application:start(mnesia),
		    Outcome = install_orber(Nodes, Options),
		    application:stop(mnesia),
		    Outcome;
		yes ->
		    install_orber(Nodes, Options)
	    end;
	_ ->
	    exit({error, "Orber is already running on this node."})
    end.



install_orber(Nodes, Options) ->
    #options{ifr_storage_type = IFRType, install_timeout = Timeout,
	     local_content = LocalContent, nameservice_storage_type = NSType,
	     initialreferences_storage_type = InitType}
	= check_options(Options, #options{}),
    MnesiaOptions = [{local_content, LocalContent}],
    TableTest = test_tables(),
    case lists:member(is_member, TableTest) of
	true ->
	    case LocalContent of
		true ->
		    orber_ifr:initialize(Timeout, {localCopy,IFRType}, 
					 light_ifr());
		_->
		    exit("Orber Mnesia Table(s) already exist. Cannot install Orber.")
	    end;
	_ ->
	    orber_ifr:initialize(Timeout, [{IFRType, Nodes} |MnesiaOptions], 
				 light_ifr())
    end,
    orber_objectkeys:install(Timeout, [{ram_copies, Nodes} |MnesiaOptions]),
    'CosNaming_NamingContextExt_impl':install(Timeout, [{NSType, Nodes} |MnesiaOptions]),
    orber_initial_references:install(Timeout, [{InitType, Nodes} |MnesiaOptions]),
    oe_cos_naming:oe_register(),
    oe_cos_naming_ext:oe_register(),
    oe_erlang:oe_register(),
    oe_OrberIFR:oe_register(),
    oe_CORBA:oe_register(),
    case NSType of
	ram_copies ->
	    case mnesia:dump_tables(['orber_CosNaming']) of
		{atomic, ok} ->
		    ok;
		{aborted, {has_no_disc,_}} ->
		    ok;
		{aborted, Reason} ->
		    ?EFORMAT("Unable to dump mnesia tables: ~p", [Reason])
	    end;
	_ ->
	    ok
    end.

check_options([], Options) ->
    Options;
check_options([{ifr_storage_type, Type}|T], Options) 
  when Type == disc_copies; Type == ram_copies ->
    check_options(T, Options#options{ifr_storage_type = Type}); 
check_options([{nameservice_storage_type, Type}|T], Options) 
  when Type == disc_copies; Type == ram_copies ->
    check_options(T, Options#options{nameservice_storage_type = Type}); 
check_options([{initialreferences_storage_type, Type}|T], Options) 
  when Type == disc_copies; Type == ram_copies ->
    check_options(T, Options#options{initialreferences_storage_type = Type}); 
check_options([{install_timeout, Timeout}|T], Options) 
  when Timeout == infinity; integer(Timeout) ->
    check_options(T, Options#options{install_timeout = Timeout});
check_options([{local_content, Bool}|T], Options) 
  when Bool == true; Bool == false ->
    check_options(T, Options#options{local_content = Bool});
check_options([{type, Type}|T], Options) 
  when Type == temporary; Type == permanent ->
    check_options(T, Options#options{type = Type});
check_options([H|_], _) ->
    ?EFORMAT("Option unknown or incorrect value: ~w", [H]).

  

try_starting(Type, Exit) ->
    case application:start(orber, Type) of
	ok ->
	    case partial_security() of
		true ->
		    error_logger:error_msg("=================== Orber =================
*******************************************
**** WARNING - WARNING - WARNING **********
**** WARNING - WARNING - WARNING **********
**** WARNING - WARNING - WARNING **********
**** WARNING - WARNING - WARNING **********
*******************************************
  ORBER STARTED WITH AN INSECURE OPTION:

             {flags, ~p}

 THIS OPTION MAY ONLY BE USED DURING TESTS

===========================================~n", [?ORB_ENV_PARTIAL_SECURITY]),
		    ok;
		false ->
		    ok
	    end;
	{error,{already_started,orber}} when Exit == true ->
	    exit("Orber already started on this node.");
	Reason when Exit == true ->
	    dbg("[~p] orber:try_starting(~p) failed: ~n~p", 
		[?LINE, Type, Reason], ?DEBUG_LEVEL),
	    exit("Unable to start Orber. Is the listen port vacant?");
	{error,{already_started,orber}} ->
	    {error,{already_started,orber}};
	Reason ->
	    dbg("[~p] orber:try_starting(~p) failed: ~n~p", 
		[?LINE, Type, Reason], ?DEBUG_LEVEL),
	    {error, "Unable to start Orber. Is the listen port vacant?"}
	end.

test_tables() ->
    AllTabs = mnesia:system_info(tables),
    lists:map(fun(Tab) ->
		      case lists:member(Tab,AllTabs) of
			  false ->
			      not_member;
			  _ ->
			      is_member
		      end
	      end,
	      get_tables()).

%%-----------------------------------------------------------------
%% UnInstallation interface functions
%%-----------------------------------------------------------------
uninstall() ->
    orber_objectkeys:stop_all(),
    application:stop(orber),
    delete_orber_tables(get_tables()).

delete_orber_tables([]) -> ok;
delete_orber_tables([Tab1|Rest]) ->
    mnesia:delete_table(Tab1),
    delete_orber_tables(Rest).

%%-----------------------------------------------------------------
%% Add and remove node interface functions
%%-----------------------------------------------------------------
add_node(Node, StorageType) when atom(Node), atom(StorageType) ->
    add_node(Node, [{ifr_storage_type, StorageType}]);
add_node(Node, OptionList) when atom(Node), list(OptionList) ->
    case rpc:call(Node, mnesia, system_info, [is_running]) of
	{badrpc, Reason} ->
	    ?EFORMAT("Node ~p do not respond. add_node/2 failed: ~p", 
		     [Node, Reason]);
	yes ->
	    case rpc:call(Node, orber, is_running, []) of
		false ->
		    %% We need to "load" orber to make sure that
		    %% application environment variables is loaded.
		    rpc:call(Node, application, load, [orber]),
		    Options = check_options(OptionList, #options{}),
		    case rpc:call(Node, orber, light_ifr, []) of
			false ->
			    copy_tables(?ifr_object_list, Node, Options);
			true ->
			    copy_tables(?ifr_light_object_list, Node, Options)
		    end;
		true ->
		    ?EFORMAT("Orber is already running on ~p. add_node failed.",
			     [Node]);
		Reason ->
		    ?EFORMAT("Unable to reach node ~p. add_node/1 failed: ~p",
			     [Node, Reason])
	    end;
	no ->
	    ?EFORMAT("Mnesia not running on node ~p. add_node/2 failed.",
		     [Node]);
	starting ->
	    ?EFORMAT("Mnesia not fully started on node ~p. add_node/2 failed.",
		     [Node]);
	stopping ->
	    ?EFORMAT("Mnesia stopping  on node ~p. add_node/2 failed.", [Node])
    end.

%% We have to copy the tables in two steps, i.e., orber tables should be ram_copies
%% while the user may choose to install the rest as disc_copies.
copy_tables([], Node, Options) ->
    copy_orber_tables(?ORBER_TABS, Node, Options);
copy_tables([T1|Trest], Node, Options) ->
    case mnesia:add_table_copy(T1, Node, Options#options.ifr_storage_type) of
	{atomic, ok} ->
	    copy_tables(Trest, Node, Options);
	{aborted, Reason} ->
	    ?EFORMAT("orber:add_node/2 failed: ~p. Unable to copy IFR table(s): ~p", 
		     [mnesia:error_description(Reason), [T1|Trest]])
    end.

copy_orber_tables([], Node, Options) ->
    case rpc:call(Node, application, start, [orber, Options#options.type]) of
	ok ->
	    ok;
	Reason ->
	    ?EFORMAT("All tables installed but failed to start orber on node ~p: ~p",
		     [Node, Reason])
    end;
copy_orber_tables([orber_CosNaming|TTail], Node, Options) ->
    case mnesia:add_table_copy(orber_CosNaming, Node, 
			       Options#options.nameservice_storage_type) of
	{atomic, ok} ->
	    copy_orber_tables(TTail, Node, Options);
	{aborted, Reason} ->
	    ?EFORMAT("orber:add_node/2 failed: ~p. Unable to copy system table(s): ~p",
		     [mnesia:error_description(Reason), [orber_CosNaming|TTail]])
    end;
copy_orber_tables([orber_references|TTail], Node, Options) ->
    case mnesia:add_table_copy(orber_references, Node, 
			       Options#options.initialreferences_storage_type) of
	{atomic, ok} ->
	    copy_orber_tables(TTail, Node, Options);
	{aborted, Reason} ->
	    ?EFORMAT("orber:add_node/2 failed: ~p. Unable to copy system table(s): ~p",
		     [mnesia:error_description(Reason), [orber_references|TTail]])
    end;
copy_orber_tables([THead|TTail], Node, Options) ->
    case mnesia:add_table_copy(THead, Node, ram_copies) of
	{atomic, ok} ->
	    copy_orber_tables(TTail, Node, Options);
	{aborted, Reason} ->
	    ?EFORMAT("orber:add_node/2 failed: ~p. Unable to copy system table(s): ~p",
		     [mnesia:error_description(Reason), [THead|TTail]])
    end.

remove_node(Node) when atom(Node) ->
    case rpc:call(Node, mnesia, system_info, [is_running]) of
	yes ->
	    case rpc:call(Node, orber, is_running, []) of
		true ->
		    rpc:call(Node, orber, stop, []),
		    remove_tables(get_tables(), Node);
		false ->
		    remove_tables(get_tables(), Node);
		Reason ->
		    ?EFORMAT("Unable to reach node: ~p. remove_node/1 failed: ~p",
			     [Node, Reason])
	    end;
	no ->
	    case rpc:call(Node, mnesia, start, []) of
		ok ->
		    remove_tables(get_tables(), Node),
		    rpc:call(Node, mnesia, stop, []);
		Reason ->
		    ?EFORMAT("Unable to reach node: ~p. remove_node/1 failed: ~p",
			     [Node, Reason])
	    end;
	Reason ->
	    ?EFORMAT("Problem with ~p. remove_node/1 failed: ~p", [Node, Reason])
    end.


remove_tables(Tables, Node) ->
    remove_tables(Tables, Node, []).

remove_tables([], _, []) -> ok;
remove_tables([], Node, Failed) ->
    ?EFORMAT("orber:remove_node(~p) failed. Unable to remove table(s): ~p", 
	     [Node, Failed]);
remove_tables([T1|Trest], Node, Failed) ->
    case mnesia:del_table_copy(T1, Node) of
	{atomic, ok} ->
	    remove_tables(Trest, Node, Failed);
	{aborted, Reason} ->
	    remove_tables(Trest, Node, [{T1, Reason}|Failed])
    end.



%%-----------------------------------------------------------------
%% Internal interface functions
%%-----------------------------------------------------------------
%%----------------------------------------------------------------------
%% Function   : is_loaded
%% Arguments  : 
%% Returns    : 
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
is_loaded() ->
    is_running(application:loaded_applications()).
is_running() ->
    is_running(application:which_applications()).

%%----------------------------------------------------------------------
%% Function   : is_running
%% Arguments  : 
%% Returns    : 
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
is_running([]) ->
    false;
is_running([{orber, _, _} |_]) ->
     true;
is_running([_ |As]) ->
    is_running(As).

%%----------------------------------------------------------------------
%% Function   : check_giop
%% Arguments  : 
%% Returns    : 
%% Raises     : 
%% Description: 
%%----------------------------------------------------------------------
check_giop_version() ->
    case giop_version() of
	{1,0} ->
	    ok;
	{1,1} ->
	    ok;
	{1,2} ->
	    ok;
	X ->
	    X
    end.

%%----------------------------------------------------------------------
%% Function   : dbg
%% Arguments  : 
%% Returns    : 
%% Raises     : 
%% Description: Note, dbg replaces debug_level_print.
%%              
%%              The following levels are used (0-10):
%%              10: cdrlib.erl
%%               9: cdr_encode.erl cdr_decode.erl orber_ifr.erl orber_pi.erl
%%               8: orber_iiop_outrequest.erl orber_iiop_inrequest.erl
%%               7: orber_iiop_outproxy.erl orber_iiop_inproxy.erl
%%               6: iop_ior.erl, orber_objectkeys.erl, Orber_IFR_impl.erl orber_socket.erl
%%               5: corba.erl, corba_boa.erl, corba_object.erl
%%               4: Reserved for Cos-services!
%%               3: Reserved for Cos-services!
%%               2: Reserved for client applications!
%%               1: Reserved for client applications!
%%               0: No logging!
%%
%%              A higher value will result in a finer granularity.
%%----------------------------------------------------------------------
get_debug_level() ->
    case application:get_env(orber, orber_debug_level) of
	{ok, Level} when integer(Level)  ->
	    Level;
	_ ->
	    0
    end.

debug_level_print(Format, Data, RequestedLevel) ->
    dbg(Format, Data, RequestedLevel).

dbg(Format, Data, RequestedLevel) ->
    case application:get_env(orber, orber_debug_level) of
	{ok, Level} when integer(Level), Level >= RequestedLevel ->
	    if
		RequestedLevel > 4 ->
		    %% Use catch if incorrect format used somewhere.
		    catch error_logger:error_msg("=================== Orber =================~n"++
						 Format++
						 "~n===========================================~n",
						 Data);
		RequestedLevel > 2 ->
		    %% Use catch if incorrect format used somewhere.
		    catch error_logger:error_msg("=========== Orber COS Application =========~n"++
						 Format++
						 "~n===========================================~n",
						 Data);
		true ->
		    %% Use catch if incorrect format used somewhere.
		    catch error_logger:error_msg("========== Orber Client Application =======~n"++
						 Format++
						 "~n===========================================~n",
						 Data)
	    end,
	    ok;
	_ ->
	    ok
    end.

error(Format, Data, RequestedLevel) ->
    if
	RequestedLevel > 4 ->
	    %% Use catch if incorrect format used somewhere.
	    catch error_logger:error_msg("=================== Orber =================~n"++
					 Format++
					 "~n===========================================~n",
					 Data);
	RequestedLevel > 2 ->
	    %% Use catch if incorrect format used somewhere.
	    catch error_logger:error_msg("=========== Orber COS Application =========~n"++
					 Format++
					 "~n===========================================~n",
					 Data);
	true ->
	    %% Use catch if incorrect format used somewhere.
	    catch error_logger:error_msg("========== Orber Client Application =======~n"++
					 Format++
					 "~n===========================================~n",
					 Data)
    end,
    ok.

configure(Key, Value) when atom(Key) ->
    configure(Key, Value, check);
configure(Key, _) ->
    ?EFORMAT("Given key (~p) not an atom.", [Key]).

configure_override(Key, Value)  when atom(Key) ->
    configure(Key, Value, loaded);
configure_override(Key, _) ->
    ?EFORMAT("Given key (~p) not an atom.", [Key]).

multi_configure(KeyValueList) when list(KeyValueList) ->
    case is_loaded() of
	false ->
	    application:load(orber),
	    multi_configure_helper(KeyValueList, loaded);
	true ->
	    case is_running() of
		false ->
		    multi_configure_helper(KeyValueList, loaded);
		true ->
		    multi_configure_helper(KeyValueList, running)
	    end
    end;
multi_configure(KeyValueList) ->
    ?EFORMAT("Given configuration parameters not a Key-Value-pair list: ~p", 
	     [KeyValueList]).

multi_configure_helper([], _) ->
    ok;
multi_configure_helper([{Key, Value}|T], Status) ->
    configure(Key, Value, Status),
    multi_configure_helper(T, Status);
multi_configure_helper([What|_], _) ->
    ?EFORMAT("Incorrect configuration parameters supplied: ~p", [What]).

%%------ Keys we can update at any time -----
%% Initial Services References
configure(orbDefaultInitRef, String, Status) when list(String) ->
    do_configure(orbDefaultInitRef, String, Status);
configure(orbDefaultInitRef, undefined, Status) ->
    do_configure(orbDefaultInitRef, undefined, Status);
configure(orbInitRef, String, Status) when list(String) ->
    do_configure(orbInitRef, String, Status);
configure(orbInitRef, undefined, Status) ->
    do_configure(orbInitRef, undefined, Status);
%% IIOP-version
configure(giop_version, {1, 0}, Status) ->
    do_configure(giop_version, {1, 0}, Status);
configure(giop_version, {1, 1}, Status) ->
    do_configure(giop_version, {1, 1}, Status);
configure(giop_version, {1, 2}, Status) ->
    do_configure(giop_version, {1, 2}, Status);
%% configure 'iiop_timout' will only have effect on new requests.
configure(iiop_timeout, infinity, Status) ->
    do_configure(iiop_timeout, infinity, Status);
configure(iiop_timeout, Value, Status) when integer(Value), Value =< 1000000 ->
    do_configure(iiop_timeout, Value, Status);
%% Backlog
configure(iiop_backlog, Value, Status) when integer(Value), Value >= 0 ->
    do_configure(iiop_backlog, Value, Status);
%% configure 'iiop_connection_timout' will only have effect on new connections.
configure(iiop_connection_timeout, infinity, Status) ->
    do_configure(iiop_connection_timeout, infinity, Status);
configure(iiop_connection_timeout, Value, Status) when integer(Value), Value =< 1000000 ->
    do_configure(iiop_connection_timeout, Value, Status);
%% configure 'iiop_in_connection_timout' will only have effect on new connections.
configure(iiop_in_connection_timeout, infinity, Status) ->
    do_configure(iiop_in_connection_timeout, infinity, Status);
configure(iiop_in_connection_timeout, Value, Status) when integer(Value), Value =< 1000000 ->
    do_configure(iiop_in_connection_timeout, Value, Status);
%% configure 'iiop_setup_connection_timeout' will only have effect on new connections.
configure(iiop_setup_connection_timeout, infinity, Status) ->
    do_configure(iiop_setup_connection_timeout, infinity, Status);
configure(iiop_setup_connection_timeout, Value, Status) when integer(Value) ->
    do_configure(iiop_setup_connection_timeout, Value, Status);
%% configure 'iiop_max_fragments' will only have effect on new connections.
configure(iiop_max_fragments, infinity, Status) ->
    do_configure(iiop_max_fragments, infinity, Status);
configure(iiop_max_fragments, Value, Status) when integer(Value), Value > 0 ->
    do_configure(iiop_max_fragments, Value, Status);
%% configure 'iiop_max_in_requests' will only have effect on new connections.
configure(iiop_max_in_requests, infinity, Status) ->
    do_configure(iiop_max_in_requests, infinity, Status);
configure(iiop_max_in_requests, Value, Status) when integer(Value), Value > 0 ->
    do_configure(iiop_max_in_requests, Value, Status);
%% configure 'iiop_max_in_connections' will only have effect on new connections.
configure(iiop_max_in_connections, infinity, Status) ->
    do_configure(iiop_max_in_connections, infinity, Status);
configure(iiop_max_in_connections, Value, Status) when integer(Value), Value > 0 ->
    do_configure(iiop_max_in_connections, Value, Status);
%% Garbage Collect the object keys DB.
configure(objectkeys_gc_time, infinity, Status) ->
    do_configure(objectkeys_gc_time, infinity, Status);
configure(objectkeys_gc_time, Value, Status) when integer(Value), Value =< 1000000 ->
    do_configure(objectkeys_gc_time, Value, Status);
%% Orber debug printouts
configure(orber_debug_level, Value, Status) when integer(Value) ->
    do_configure(orber_debug_level, Value, Status);

%%------ Keys we cannot change if Orber is running -----
%% Set the bootstrap port
configure(bootstrap_port, Value, Status) when integer(Value), Value > 0 ->
    do_safe_configure(bootstrap_port, Value, Status);
%% Set the listen port
configure(iiop_port, Value, Status) when integer(Value) ->
    do_safe_configure(iiop_port, Value, Status);
%% Set the NAT listen port
configure(nat_iiop_port, Value, Status) when integer(Value), Value > 0 ->
    do_safe_configure(nat_iiop_port, Value, Status);
%% IIOP interceptors
configure(interceptors, Value, Status) when tuple(Value) ->
    do_safe_configure(interceptors, Value, Status);
%% Local interceptors
configure(local_interceptors, Value, Status) when tuple(Value) ->
    do_safe_configure(local_interceptors, Value, Status);
%% Orber Domain
configure(domain, Value, Status) when list(Value) ->
    do_safe_configure(domain, Value, Status);
%% Set the IP-address we should use
configure(ip_address, Value, Status) when list(Value) ->
    do_safe_configure(ip_address, Value, Status);
configure(ip_address, {multiple, Value}, Status) when list(Value) ->
    do_safe_configure(ip_address, {multiple, Value}, Status);
%% Set the NAT IP-address we should use
configure(nat_ip_address, Value, Status) when list(Value) ->
    do_safe_configure(nat_ip_address, Value, Status);
configure(nat_ip_address, {multiple, Value}, Status) when list(Value) ->
    do_safe_configure(nat_ip_address, {multiple, Value}, Status);
%% Set the range of ports we may use on this machine when connecting to a server.
configure(iiop_out_ports, {Min, Max}, Status) when integer(Min), integer(Max) ->
    do_safe_configure(iiop_out_ports, {Min, Max}, Status);
%% Set the lightweight option.
configure(lightweight, Value, Status) when list(Value) ->
    do_safe_configure(lightweight, Value, Status);
%% Configre the System Flags
configure(flags, Value, Status) when integer(Value) ->
    do_safe_configure(flags, Value, Status);

%% SSL settings
configure(secure, ssl, Status) ->
    do_safe_configure(secure, ssl, Status);
configure(iiop_ssl_backlog, Value, Status) when integer(Value), Value >= 0 ->
    do_safe_configure(iiop_ssl_backlog, Value, Status);
configure(nat_iiop_ssl_port, Value, Status) when integer(Value), Value > 0 ->
    do_safe_configure(nat_iiop_ssl_port, Value, Status);
configure(iiop_ssl_port, Value, Status) when integer(Value) ->
    do_safe_configure(iiop_ssl_port, Value, Status);
configure(ssl_server_certfile, Value, Status) when list(Value) ->
    do_safe_configure(ssl_server_certfile, Value, Status);
configure(ssl_server_certfile, Value, Status) when atom(Value) ->
    do_safe_configure(ssl_server_certfile, atom_to_list(Value), Status);
configure(ssl_client_certfile, Value, Status) when list(Value) ->
    do_safe_configure(ssl_client_certfile, Value, Status);
configure(ssl_client_certfile, Value, Status) when atom(Value) ->
    do_safe_configure(ssl_client_certfile, atom_to_list(Value), Status);
configure(ssl_server_verify, Value, Status) when integer(Value) ->
    do_safe_configure(ssl_server_verify, Value, Status);
configure(ssl_client_verify, Value, Status) when integer(Value) ->
    do_safe_configure(ssl_client_verify, Value, Status);
configure(ssl_server_depth, Value, Status) when integer(Value) ->
    do_safe_configure(ssl_server_depth, Value, Status);
configure(ssl_client_depth, Value, Status) when integer(Value) ->
    do_safe_configure(ssl_client_depth, Value, Status);
configure(ssl_server_cacertfile, Value, Status) when list(Value) ->
    do_safe_configure(ssl_server_cacertfile, Value, Status);
configure(ssl_server_cacertfile, Value, Status) when atom(Value) ->
    do_safe_configure(ssl_server_cacertfile, atom_to_list(Value), Status);
configure(ssl_client_cacertfile, Value, Status) when list(Value) ->
    do_safe_configure(ssl_client_cacertfile, Value, Status);
configure(ssl_client_cacertfile, Value, Status) when atom(Value) ->
    do_safe_configure(ssl_client_cacertfile, atom_to_list(Value), Status);
configure(ssl_client_password, Value, Status) when list(Value) ->
    do_safe_configure(ssl_client_password, Value, Status);
configure(ssl_client_password, Value, Status) when atom(Value) ->
    do_safe_configure(ssl_client_password, atom_to_list(Value), Status);
configure(ssl_client_keyfile, Value, Status) when list(Value) ->
    do_safe_configure(ssl_client_keyfile, Value, Status);
configure(ssl_client_keyfile, Value, Status) when atom(Value) ->
    do_safe_configure(ssl_client_keyfile, atom_to_list(Value), Status);
configure(ssl_server_password, Value, Status) when list(Value) ->
    do_safe_configure(ssl_server_password, Value, Status);
configure(ssl_client_password, Value, Status) when atom(Value) ->
    do_safe_configure(ssl_server_password, atom_to_list(Value), Status);
configure(ssl_server_keyfile, Value, Status) when list(Value) ->
    do_safe_configure(ssl_server_keyfile, Value, Status);
configure(ssl_server_keyfile, Value, Status) when atom(Value) ->
    do_safe_configure(ssl_server_keyfile, atom_to_list(Value), Status);
configure(ssl_server_ciphers, Value, Status) when list(Value) ->
    do_safe_configure(ssl_server_ciphers, Value, Status);
configure(ssl_server_ciphers, Value, Status) when atom(Value) ->
    do_safe_configure(ssl_server_ciphers, atom_to_list(Value), Status);
configure(ssl_client_ciphers, Value, Status) when list(Value) ->
    do_safe_configure(ssl_client_ciphers, Value, Status);
configure(ssl_client_ciphers, Value, Status) when atom(Value) ->
    do_safe_configure(ssl_client_ciphers, atom_to_list(Value), Status);
configure(ssl_client_cachetimeout, Value, Status) when integer(Value), Value > 0 ->
    do_safe_configure(ssl_client_cachetimeout, Value, Status);
configure(ssl_server_cachetimeout, Value, Status) when integer(Value), Value > 0 ->
    do_safe_configure(ssl_server_cachetimeout, Value, Status);

configure(Key, Value, _) ->
    ?EFORMAT("Bad configuration parameter: {~p, ~p}", [Key, Value]).

%% This function may be used as long as it is safe to change a value at any time.
do_configure(Key, Value, check) ->
    case is_loaded() of
	false ->
	    application:load(orber),
	    application_controller:set_env(orber, Key, Value);
	true ->
	    application_controller:set_env(orber, Key, Value)
    end;
do_configure(Key, Value, _) ->
    application_controller:set_env(orber, Key, Value).

%% This function MUST(!!) be used when we cannot change a value if Orber is running.
do_safe_configure(_, _, running) ->
    exit("Orber already running, the given key may not be updated!");
do_safe_configure(Key, Value, check) ->
    case is_loaded() of
	false ->
	    application:load(orber),
	    application_controller:set_env(orber, Key, Value);
	true ->
	    case is_running() of
		false ->
		    application_controller:set_env(orber, Key, Value);
		true ->
		    ?EFORMAT("Orber already running. {~p, ~p} may not be updated!",
			     [Key, Value])
	    end
    end;
do_safe_configure(Key, Value, loaded) ->
    application_controller:set_env(orber, Key, Value).


%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
start(_, _) ->
    supervisor:start_link({local, orber_sup}, orber, orb_init).

init(orb_init) ->
    case check_giop_version() of
	ok ->
	    case is_lightweight() of
		true ->
		    SupFlags = {one_for_one, 5, 1000},  
		    ChildSpec = [
				 {orber_iiop_sup, {orber_iiop, start_sup, [[]]},
				  permanent, 
				  10000, supervisor, [orber_iiop]},
				 {orber_reqno, {orber_request_number, start,
						[[]]},
				  permanent, 
				  10000, worker, [orber_request_number]}
				],
		    {ok, {SupFlags, ChildSpec}};
		false ->
		    case mnesia:wait_for_tables(get_tables(),
						infinity) of
			ok ->
			    orber_objectkeys:remove_old_keys(),
			    SupFlags = {one_for_one, 5, 1000},  
			    ChildSpec = [
					 {orber_iiop_sup, {orber_iiop, start_sup, [[]]},
					  permanent, 
					  10000, supervisor, [orber_iiop]},
					 {orber_init, {orber_initial_references, start,
						       [[]]},
					  permanent, 
					  10000, worker, [orber_initial_references]},
					 {orber_reqno, {orber_request_number, start,
							[[]]},
					  permanent, 
					  10000, worker, [orber_request_number]},
					 {orber_objkeyserver, {orber_objectkeys, start,
							       [[orber_nodes(), 0]]},
					  permanent, 
					  10000, worker, [orber_objectkeys]}
					],
			    {ok, {SupFlags, ChildSpec}};
			StopReason ->
			    {stop, StopReason}
		    end
	    end;
	X ->
	    {stop, ?FORMAT("GIOP ~p not an implemeted version", [X])}
    end.

