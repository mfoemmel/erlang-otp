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
-include_lib("orber/src/orber_debug.hrl").
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
	 uninstall/0, giop_version/0, info/0, is_running/0, add_node/2, 
	 remove_node/1, iiop_timeout/0, iiop_connection_timeout/0, 
	 iiop_setup_connection_timeout/0, objectkeys_gc_time/0,
	 is_lightweight/0, get_lightweight_nodes/0,
	 start_lightweight/0, start_lightweight/1,
	 get_ORBDefaultInitRef/0, get_ORBInitRef/0,
	 get_interceptors/0, set_interceptors/1,
	 jump_start/0, jump_start/1, jump_start/2, jump_stop/0,
	 iiop_connections/0, iiop_connections_pending/0, typechecking/0,
	 exclude_codeset_ctx/0, exclude_codeset_component/0, bidir_context/0, use_FT/0,
	 use_CSIv2/0, get_flags/0, secure/0, multi_jump_start/1, multi_jump_start/2, 
	 multi_jump_start/3, get_tables/0, iiop_in_connection_timeout/0, 
	 partial_security/0]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([host/0, ip_address_variable_defined/0, start/2, init/1,
	 get_debug_level/0, debug_level_print/3, dbg/3, error/3,
	 exception_info/1, configure/2, multi_configure/1,
	 mjs/1, mjs/2, js/0, js/1]).

%%-----------------------------------------------------------------
%% Internal definitions
%%-----------------------------------------------------------------
%% Defines possible configuration parameters a user can add when
%% installing Orber.
-define(INSTALL_DEF_OPT, [{ifr_storage_type, disc_copies},
			  {install_timeout, infinity},
			  {local_content, false},
			  {nameservice_storage_type, ram_copies},
			  {initialreferences_storage_type, ram_copies}]).

-define(ORBER_TABS, [orber_CosNaming, orber_objkeys, corba_policy,
		     corba_policy_associations, orber_references]).

-define(DEBUG_LEVEL, 5).


%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------

jump_stop() ->
    stop(),
    uninstall(),
    mnesia:stop().

js() ->
    jump_start(iiop_port(), [{interceptors, {native, [orber_iiop_tracer]}},
			     {orber_debug_level, 10}, 
			     {flags, (?ORB_ENV_LOCAL_TYPECHECKING bor get_flags())}]).

js(Port) ->
    jump_start(Port, [{interceptors, {native, [orber_iiop_tracer]}},
		      {orber_debug_level, 10}, 
		      {flags, (?ORB_ENV_LOCAL_TYPECHECKING bor get_flags())}]).

jump_start() ->
    jump_start(iiop_port(), []).

jump_start(Port) ->
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
    multi_js_helper(Nodes, iiop_port(),
		    [{interceptors, {native, [orber_iiop_tracer]}},
		     {orber_debug_level, 10}, 
		     {flags, (?ORB_ENV_LOCAL_TYPECHECKING bor get_flags())}]).

mjs(Nodes, Port) ->
    multi_js_helper(Nodes, Port, 
		    [{interceptors, {native, [orber_iiop_tracer]}},
		     {orber_debug_level, 10},
		     {flags, (?ORB_ENV_LOCAL_TYPECHECKING bor get_flags())}]).


multi_jump_start(Nodes) ->
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

jump_start_slaves([], Port, Options, [], NodeData) ->
    rpc:multicall([node() | nodes()], global, sync, []),
    {ok, NodeData};
jump_start_slaves([], Port, Options, Errors, _) ->
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

create_nodes(Host, 0, Port, Options, [], NodeData) ->
    {ok, NodeData};
create_nodes(Host, 0, Port, Options, Errors, _) ->
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
	
start_lightweight(Nodes) ->
    exit({error,"Argument not correct; must be a list of nodes."}).

stop() ->
    application:stop(orber).


get_tables() ->
    ?ifr_object_list++?ORBER_TABS.

iiop_port() ->
    case application:get_env(orber, iiop_port) of
	{ok, Port} when integer(Port), Port > 0 ->
	    Port;
	_ ->
	    4001
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
	_ ->
	    true
    end.

host() ->
    case application:get_env(orber, ip_address) of
	{ok,I} when list(I) ->
	    I;
	{ok, {A1, A2, A3, A4}} when integer(A1+A2+A3+A4) ->
	    integer_to_list(A1) ++ "." ++ integer_to_list(A2) ++ "." ++ integer_to_list(A3)
		++ "." ++ integer_to_list(A4);
	_ ->
	    case hostname_in_IOR() of
		true ->
		    {ok, Hostname} = inet:gethostname(),
		    Hostname;
		_ ->
		    ip_address()
	    end
    end.

ip_address() ->
    {ok, Hostname} = inet:gethostname(),
    {ok, {A1, A2, A3, A4}} = inet:getaddr(Hostname, inet),
    integer_to_list(A1) ++ "." ++ integer_to_list(A2) ++ "." ++ integer_to_list(A3)
	++ "." ++ integer_to_list(A4).


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
		    error_logger:error_msg("Orber 'iiop_timeout' badly configured.
Time to large (>1000000 sec), swithed to 'infinity'~n"),
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
		    error_logger:error_msg("Orber 'iiop_connection_timeout' badly configured.
Time to large (>1000000 sec), swithed to 'infinity'~n"),
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
		    error_logger:error_msg("Orber 'iiop_connection_timeout' badly configured.
Time to large (>1000000 sec), swithed to 'infinity'~n"),
		    infinity;
		true ->
		    %% Convert to msec.
		    Int*1000
	    end;
	_ ->
	    infinity
    end.
    


iiop_connections() ->
    orber_iiop_pm:list_existing_connections().

iiop_connections_pending() ->
    orber_iiop_pm:list_setup_connections().


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

hostname_in_IOR() ->
    ?ORB_FLAG_TEST(get_flags(), ?ORB_ENV_HOSTNAME_IN_IOR).

partial_security() ->
    ?ORB_FLAG_TEST(get_flags(), ?ORB_ENV_PARTIAL_SECURITY).

use_CSIv2() ->
    ?ORB_FLAG_TEST(get_flags(), ?ORB_ENV_USE_CSIV2).

use_FT() ->
    ?ORB_FLAG_TEST(get_flags(), ?ORB_ENV_USE_FT).


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
		    error_logger:error_msg("Orber 'objectkeys_gc_time' badly configured.
Time to large (>1000000 sec), swithed to 'infinity'~n"),
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
 
iiop_ssl_port() ->
    case application:get_env(orber, secure) of
	{ok, ssl} ->
	        case application:get_env(orber, iiop_ssl_port) of
		    {ok, Port} when integer(Port), Port > 0 ->
			Port;
		    _ ->
			4002
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
    case is_running() of
	true ->
	    Info1 = create_main_info(),
	    Info2 = create_flag_info(Info1),
	    Info3 = create_security_info(secure(), Info2),
	    error_logger:info_msg(Info3);
	_ ->
	    error_logger:info_msg("=== Orber-~-9s System Information ===
       *** Orber is not running ***
==========================================~n",[?ORBVSN])
    end.

create_main_info() ->
    [io_lib:format("=== Orber-~-9s System Information ===
Orber domain..................: ~s
IIOP port number..............: ~p
Bootstrap port number.........: ~p
Nodes in domain...............: ~p
GIOP version..................: ~p
IIOP out timeout..............: ~p
IIOP out connection timeout...: ~p
IIOP setup connection timeout.: ~p
IIOP out ports................: ~p
IIOP out connections..........: ~p
IIOP out connections (pending): ~p
Object Keys GC interval.......: ~p
Using Interceptors............: ~p
Debug Level...................: ~p
orbInitRef....................: ~p
orbDefaultInitRef.............: ~p~n",
[?ORBVSN, domain(), iiop_port(), bootstrap_port(), orber_nodes(), giop_version(),
 iiop_timeout(), iiop_connection_timeout(), iiop_setup_connection_timeout(),
 iiop_out_ports(), iiop_connections(), iiop_connections_pending(),
 objectkeys_gc_time(), get_interceptors(), get_debug_level(), get_ORBInitRef(),
 get_ORBDefaultInitRef()])].

create_flag_info(Info) ->
    case application:get_env(orber, flags) of
	undefined ->
	    [Info, "System Flags Set..............: -~n"];
	{ok, Flags} ->
	    FlagData = check_flags(?ORB_ENV_FLAGS, Flags, []),
	    [Info, "System Flags Set..............:~n", FlagData, "~n"]
    end.
  
check_flags([], _, Acc) ->
    Acc;
check_flags([{Flag, Txt}|T], Flags, Acc) when ?ORB_FLAG_TEST(Flags, Flag) ->
    check_flags(T, Flags, ["   - ", Txt, "~n"|Acc]);
check_flags([_|T], Flags, Acc) ->
    check_flags(T, Flags, Acc).

	    


create_security_info(no, Info) ->
    lists:flatten([Info, "=========================================~n"]);
create_security_info(ssl, Info) ->
    lists:flatten([Info, io_lib:format("ORB security..................: ssl
SSL IIOP port number..........: ~p
SSL server certfile...........: ~p
SSL server verification type..: ~p
SSL server verification depth.: ~p
SSL server cacertfile.........: ~p
SSL server keyfile............: ~p
SSL server password...........: ~p
SSL server ciphers............: ~p
SSL server cachetimeout.......: ~p
SSL client certfile...........: ~p
SSL client verification type..: ~p
SSL client verification depth.: ~p
SSL client cacertfile.........: ~p
SSL client keyfile............: ~p
SSL client password...........: ~p
SSL client ciphers............: ~p
SSL client cachetimeout.......: ~p
=========================================~n",
[iiop_ssl_port(), ssl_server_certfile(), ssl_server_verify(),
 ssl_server_depth(), ssl_server_cacertfile(), ssl_server_keyfile(), 
 ssl_server_password(), ssl_server_ciphers(), ssl_server_cachetimeout(),
 ssl_client_certfile(), ssl_client_verify(), ssl_client_depth(), 
 ssl_client_cacertfile(), ssl_client_keyfile(), ssl_client_password(),
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
    Local_content_tables = get_option(local_content, Options),
    MnesiaOptions = [Local_content_tables],
    {_, IFR_storage_type} = get_option(ifr_storage_type, Options),
    {_, Timeout} = get_option(install_timeout, Options),
    {_, NSType} = get_option(nameservice_storage_type, Options),
    {_, InitType} = get_option(initialreferences_storage_type, Options),
    TableTest = test_tables(),
    case lists:member(is_member, TableTest) of
	true ->
	    case Local_content_tables of
		{local_content, true} ->
		    orber_ifr:init(Timeout, {localCopy,IFR_storage_type});
		_->
		    exit({error, "Orber Mnesia Table(s) already exist. Cannot install Orber."})
	    end;
	_ ->
	    orber_ifr:init(Timeout, [{IFR_storage_type, Nodes} |MnesiaOptions])
    end,
    add_table_index(),
    orber_objectkeys:install(Timeout, [{ram_copies, Nodes} |MnesiaOptions]),
    orber_policy_server:install(Timeout, [{ram_copies, Nodes} |MnesiaOptions]),
    'CosNaming_NamingContextExt_impl':install(Timeout, [{NSType, Nodes} |MnesiaOptions]),
    orber_initial_references:install(Timeout, [{InitType, Nodes} |MnesiaOptions]),
    try_starting(temporary, true),
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
		    application:stop(orber),
		    exit({error, {"Unable to dump mnesia tables.", Reason}})
	    end;
	_ ->
	    ok
    end,
    application:stop(orber).

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
	    exit("Unable to start Orber. Is the listen port vacant?");
	{error,{already_started,orber}} ->
	    {error,{already_started,orber}};
	_ ->
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
	      ?ifr_object_list++?ORBER_TABS).

check_mnesia_result({atomic, ok}, _) ->
    ok;
check_mnesia_result({aborted, R}, Reason) ->
    exit({error, {Reason, R}}).

%%-----------------------------------------------------------------
%% UnInstallation interface functions
%%-----------------------------------------------------------------
uninstall() ->
    orber_objectkeys:stop_all(),
    application:stop(orber),
    delete_orber_tables(?ifr_object_list++?ORBER_TABS).

delete_orber_tables([]) -> ok;
delete_orber_tables([Tab1|Rest]) ->
    mnesia:delete_table(Tab1),
    delete_orber_tables(Rest).

get_option(Key, OptionList) ->
    case lists:keysearch(Key, 1, OptionList) of
	{value,{Key,Value}} ->
	    {Key,Value};
	_ ->
	    case lists:keysearch(Key, 1, ?INSTALL_DEF_OPT) of
		{value,{Key,Value}} ->
		    {Key,Value};
		_->
		    exit(io_lib:format("Option ~w not found in optionlist", 
				       [Key]))
	    end
    end.

%%-----------------------------------------------------------------
%% Add and remove node interface functions
%%-----------------------------------------------------------------
add_node(Node, StorageType) when atom(Node), atom(StorageType)  ->
    case rpc:call(Node, mnesia, system_info, [is_running]) of
	{badrpc, Reason} ->
	    exit("Node '"++atom_to_list(Node)++
		 "' do not respond. add_node/2 failed.");
	yes ->
	    case rpc:call(Node, orber, is_running, []) of
		false ->
		    copy_tables(?ifr_object_list, Node, StorageType);
		true ->
		    exit("Unable to add node '" ++ 
			 atom_to_list(Node) ++ 
			 "' since Orber already running.");
		_ ->
		    exit("Unable to reach node: '" ++ 
			 atom_to_list(Node) ++ 
			 " add_node/1 failed")
	    end;
	no ->
	    exit("Mnesia not running on node '"++atom_to_list(Node)++ 
		 "' add_node/2 failed.") 
    end.

%% We have to copy the tables in two steps, i.e., orber tables should be ram_copies
%% while the user may choose to install the rest as disc_copies.
copy_tables([], Node, StorageType) ->
    copy_orber_tables(?ORBER_TABS, Node);
copy_tables([T1|Trest], Node, StorageType) ->
    case mnesia:add_table_copy(T1, Node, StorageType) of
	{atomic, ok} ->
	    copy_tables(Trest, Node, StorageType);
	{aborted, Reason} ->
	    exit({"orber:add_node/2 failed. Unable to copy IFR table(s): ", 
		  [T1|Trest],
		  mnesia:error_description(Reason)})
    end.

copy_orber_tables([], Node) ->
    case rpc:call(Node, application, start, [orber]) of
	ok ->
	    ok;
	Reason ->
	    exit("All tables installed but failed to start orber on node: '"++
		 atom_to_list(Node))
    end;
copy_orber_tables([THead|TTail], Node) ->
    case mnesia:add_table_copy(THead, Node, ram_copies) of
	{atomic, ok} ->
	    copy_orber_tables(TTail, Node);
	{aborted, Reason} ->
	    exit({"orber:add_node/2 failed. Unable to copy system table(s):",
		  [THead|TTail],
		  mnesia:error_description(Reason)})
    end.

remove_node(Node) when atom(Node) ->
    case rpc:call(Node, mnesia, system_info, [is_running]) of
	yes ->
	    case rpc:call(Node, orber, is_running, []) of
		true ->
		    rpc:call(Node, application, stop, [orber]),
		    remove_tables(?ifr_object_list ++ ?ORBER_TABS, Node);
		false ->
		    remove_tables(?ifr_object_list ++ ?ORBER_TABS, Node);
		_ ->
		    exit("Unable to reach node: '"++atom_to_list(Node)++ 
			 "' remove_node/1 failed")
	    end;
	no ->
	    case rpc:call(Node, mnesia, start, []) of
		ok ->
		    remove_tables(?ifr_object_list ++ ?ORBER_TABS, Node),
		    rpc:call(Node, mnesia, stop, []);
		_->
		    exit("Unable to reach node: '"++atom_to_list(Node)++ 
			 "' remove_node/1 failed")
	    end;
	{badrpc, Reason} ->
	    exit("Unable to contact node '"++atom_to_list(Node)++ 
		 "' remove_node/1 failed.")
    end.


remove_tables(Tables, Node) ->
    remove_tables(Tables, Node, []).

remove_tables([], Node, []) -> ok;
remove_tables([], Node, Failed) ->
    exit({"orber:remove_node/1 failed. Unable to remove table(s):", Failed});
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

is_loaded() ->
    is_running(application:loaded_applications()).
is_running() ->
    is_running(application:which_applications()).

is_running([]) ->
    false;
is_running([{orber, _, _} |As]) ->
     true;
is_running([_ |As]) ->
    is_running(As).

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


get_debug_level() ->
    case application:get_env(orber, orber_debug_level) of
	{ok, Level} when integer(Level)  ->
	    Level;
	_ ->
	    0
    end.

%% NOTE!!
%% The following levels are used (0-10):
%% 10: cdrlib.erl
%%  9: cdr_encode.erl cdr_decode.erl orber_ifr.erl orber_pi.erl
%%  8: orber_iiop_outrequest.erl orber_iiop_inrequest.erl
%%  7: orber_iiop_outproxy.erl orber_iiop_inproxy.erl
%%  6: iop_ior.erl, orber_objectkeys.erl, Orber_IFR_impl.erl orber_socket.erl
%%  5: corba.erl, corba_boa.erl, corba_object.erl
%%  4: Reserved for Cos-services!
%%  3: Reserved for Cos-services!
%%  2: Reserved for client applications!
%%  1: Reserved for client applications!
%%  0: No logging!
%%
%% A higher value will result in a finer granularity.

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
configure(Key, Value) ->
    exit("Given key not an atom.").

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
    exit({"Given configuration parameters not a Key-Value-pair list.", KeyValueList}).

multi_configure_helper([], _) ->
    ok;
multi_configure_helper([{Key, Value}|T], Status) ->
    configure(Key, Value, Status),
    multi_configure_helper(T, Status);
multi_configure_helper([What|T], _) ->
    exit({"Incorrect configuration parameters supplied:", What}).

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
configure(iiop_port, Value, Status) when integer(Value), Value > 0 ->
    do_safe_configure(iiop_port, Value, Status);
%% IIOP interceptors
configure(interceptors, Value, Status) when tuple(Value) ->
    do_safe_configure(interceptors, Value, Status);
%% Orber Domain
configure(domain, Value, Status) when list(Value) ->
    do_safe_configure(domain, Value, Status);
%% Set the IP-address we should use
configure(ip_address, Value, Status) when list(Value) ->
    do_safe_configure(ip_address, Value, Status);
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
configure(iiop_ssl_port, Value, Status) when integer(Value), Value > 0 ->
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
    dbg("[~p] orber:configure(~p, ~p); Bad key or value.", 
	[?LINE, Key, Value], ?DEBUG_LEVEL),
    exit("Bad configuration parameter(s)").

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
do_safe_configure(Key, Value, running) ->
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
		    exit("Orber already running, the given key may not be updated!")
	    end
    end;
do_safe_configure(Key, Value, loaded) ->
    application_controller:set_env(orber, Key, Value).


add_table_index() ->
    check_mnesia_result(mnesia:add_table_index(ir_ModuleDef, id),
			"Unable to index ir_ModuleDef mnesia table."),
    check_mnesia_result(mnesia:add_table_index(ir_InterfaceDef, id),
			"Unable to index ir_InterfaceDef mnesia table."),
    check_mnesia_result(mnesia:add_table_index(ir_StructDef, id),
			"Unable to index ir_StructDef mnesia table."),
    check_mnesia_result(mnesia:add_table_index(ir_UnionDef, id),
			"Unable to index ir_UnionDef mnesia table."),
    check_mnesia_result(mnesia:add_table_index(ir_ExceptionDef, id),
			"Unable to index ir_ExceptionDef mnesia table."),
    check_mnesia_result(mnesia:add_table_index(ir_ConstantDef, id),
			"Unable to index ir_ConstantDef mnesia table."),
    check_mnesia_result(mnesia:add_table_index(ir_EnumDef, id),
			"Unable to index ir_EnumDef mnesia table."),
    check_mnesia_result(mnesia:add_table_index(ir_AliasDef, id),
			"Unable to index ir_AliasDef mnesia table."),
    check_mnesia_result(mnesia:add_table_index(ir_AttributeDef, id),
			"Unable to index ir_AttributeDef mnesia table."),
    check_mnesia_result(mnesia:add_table_index(ir_OperationDef, id),
			"Unable to index ir_OperationDef mnesia table."),
    check_mnesia_result(mnesia:add_table_index(ir_Contained, id),
			"Unable to index ir_Contained mnesia table."),
    check_mnesia_result(mnesia:add_table_index(ir_TypedefDef, id),
			"Unable to index ir_TypedefDef mnesia table.").

%%-----------------------------------------------------------------
%% Server functions
%%-----------------------------------------------------------------
start(_, _) ->
    supervisor:start_link({local, orber_sup}, orber, orb_init).

init(orb_init) ->
    case check_giop_version() of
	ok ->
	    ?PRINTDEBUG("init orber supervisor"),
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
		    case mnesia:wait_for_tables(?ifr_object_list, infinity) of
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
					  10000, worker, [orber_objectkeys]},
					 {orber_policyserver, {orber_policy_server, start,
							       [[]]},
					  permanent, 
					  10000, worker, [orber_policy_server]}
					],
			    {ok, {SupFlags, ChildSpec}};
			StopReason ->
			    {stop, StopReason}
		    end
	    end;
	X ->
	    {stop, io_lib:format("GIOP ~p not an implemeted version", [X])}
    end.

