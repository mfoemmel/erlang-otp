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
-include_lib("orber/src/orber_debug.hrl").
-include_lib("orber/src/ifr_objects.hrl").
%%-----------------------------------------------------------------
%% External exports
%%-----------------------------------------------------------------
-export([start/0, stop/0, install/1, install/2, orber_nodes/0, iiop_port/0,
	 domain/0, bootstrap_port/0, iiop_ssl_port/0, 
	 ssl_server_certfile/0, ssl_client_certfile/0, set_ssl_client_certfile/1,
	 ssl_server_verify/0, ssl_client_verify/0, set_ssl_client_verify/1,
	 ssl_server_depth/0, ssl_client_depth/0, set_ssl_client_depth/1,
	 ssl_server_cacertfile/0,ssl_client_cacertfile/0, set_ssl_client_cacertfile/1,
	 uninstall/0, giop_version/0, info/0, is_running/0, add_node/2, 
	 remove_node/1, iiop_timeout/0, iiop_connection_timeout/0, 
	 iiop_setup_connection_timeout/0, objectkeys_gc_time/0,
	 is_lightweight/0, get_lightweight_nodes/0,
	 start_lightweight/0, start_lightweight/1,
	 get_ORBDefaultInitRef/0, get_ORBInitRef/0,
	 get_interceptors/0, set_interceptors/1]).

%%-----------------------------------------------------------------
%% Internal exports
%%-----------------------------------------------------------------
-export([host/0, ip_address_variable_defined/0, start/2, init/1, start_naming_service/1,
	 get_debug_level/0, debug_level_print/3, configure/2, throw_helper/2]).

%%-----------------------------------------------------------------
%% Internal definitions
%%-----------------------------------------------------------------
%% Defines possible configuration parameters a user can add when
%% installing Orber.
-define(INSTALL_DEF_OPT, [{ifr_storage_type, disc_copies},
			  {install_timeout, infinity},
			  {local_content, false}]).

-define(ORBER_TABS, [orber_CosNaming, orber_objkeys, corba_policy,
		     corba_policy_associations]).

-define(DEBUG_LEVEL, 5).


%%-----------------------------------------------------------------
%% External interface functions
%%-----------------------------------------------------------------
start() ->
    application:start(mnesia),
    TableTest = test_tables(),
    case lists:member(not_member, TableTest) of
	true ->
	    exit({error,"Orber Mnesia Table(s) missing. Orber not properly installed."});
	_->
	    application:start(orber)
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


iiop_port() ->
    case application:get_env(orber, iiop_port) of
	{ok, Port} when integer(Port) ->
	    Port;
	_ ->
	    4001
    end.

bootstrap_port() ->
    case application:get_env(orber, bootstrap_port) of
	{ok, Port} when integer(Port) ->
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
	    ip_address()
    end.

ip_address() ->
    %% We used to call inet:gethostname() instead of net_adm:localhost() but if the 
    %% /etc/hosts don't contain the short name the result would be 127.0.0.1. In an
    %% IOR this would cause problems.
    Hostname = net_adm:localhost(),
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
		    {ok, Port} when integer(Port) ->
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
	    {ok, Cwd} = file:get_cwd(),
	    filename:join(Cwd,"ssl_server_cert.pem")
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
		    {ok, Cwd} = file:get_cwd(),
		    filename:join(Cwd,"ssl_client_cert.pem")
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
    

%%-----------------------------------------------------------------
%% Configuration settings
%%-----------------------------------------------------------------
info() ->
    case is_running() of
	true ->
	    io:format("==== Orber System Information ====\n",[]),
	    io:format("Orber domain: ~p\n",[domain()]),
	    io:format("IIOP port number: ~p\n",[iiop_port()]),
	    io:format("Bootstrap port number: ~p\n",[bootstrap_port()]),
	    io:format("Nodes in domain: ~p\n",[orber_nodes()]),
	    io:format("GIOP version: ~p\n",[giop_version()]),
	    io:format("IIOP timeout: ~p\n",[iiop_timeout()]),
	    io:format("IIOP connection timeout: ~p\n",[iiop_connection_timeout()]),
	    io:format("IIOP setup connection timeout: ~p\n",[iiop_setup_connection_timeout()]),
	    io:format("Object Keys GC interval: ~p\n",[objectkeys_gc_time()]),
	    io:format("Using Interceptors: ~p\n",[get_interceptors()]),
	    io:format("Debug Level: ~p\n",[get_debug_level()]),
	    io:format("orbInitRef: ~p\n",[get_ORBInitRef()]),
	    io:format("orbDefaultInitRef: ~p\n",[get_ORBDefaultInitRef()]),
	    Sec = secure(),
	    io:format("ORB security: ~p\n",[Sec]),
	    case Sec of
		ssl ->
		    io:format("SSL IIOP port number ~p\n",[iiop_ssl_port()]),
		    io:format("SSL server certfile ~p\n",[ssl_server_certfile()]),
		    io:format("SSL server verification type ~p\n",[ssl_server_verify()]),
		    io:format("SSL server verification depth ~p\n",[ssl_server_depth()]),
		    io:format("SSL server cacertfile ~p\n",[ssl_server_cacertfile()]),
		    io:format("SSL client certfile ~p\n",[ssl_client_certfile()]),
		    io:format("SSL client verification type ~p\n",[ssl_client_verify()]),
		    io:format("SSL client verification depth ~p\n",[ssl_client_depth()]),
		    io:format("SSL client cacertfile ~p\n",[ssl_client_cacertfile()]);
		no ->
		    ok
	    end; 
	_ ->
	    io:format("Orber is not running\n",[])
    end.

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
    'CosNaming_NamingContext_impl':install(Timeout, [{ram_copies, Nodes} |MnesiaOptions]),
    application:start(orber),
    oe_cos_naming:oe_register(),
    oe_cos_naming_ext:oe_register(),
    oe_erlang:oe_register(),
    oe_OrberIFR:oe_register(),
    oe_CORBA:oe_register(),
    check_mnesia_result(mnesia:dump_tables(['orber_CosNaming']),
			"Unable to dump mnesia tables."),
    application:stop(orber).


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
	    exit({error, "Node '"++atom_to_list(Node)++
		  "' do not respond. add_node/2 failed."});
	yes ->
	    case rpc:call(Node, orber, is_running, []) of
		false ->
		    copy_tables(?ifr_object_list, Node, StorageType);
		true ->
		    exit({error, "Unable to add node '" ++ 
			  atom_to_list(Node) ++ 
			  "' since Orber already running."});
		_ ->
		    exit({error, "Unable to reach node: '" ++ 
			  atom_to_list(Node) ++ 
			  " add_node/1 failed"})
	    end;
	no ->
	    exit({error, "Mnesia not running on node '"++atom_to_list(Node)++ 
		  "' add_node/2 failed."}) 
    end.

%% We have to copy the tables in two steps, i.e., orber tables should be ram_copies
%% while the user may choose to install the rest as disc_copies.
copy_tables([], Node, StorageType) ->
    copy_orber_tables(?ORBER_TABS, Node);
copy_tables([T1|Trest], Node, StorageType) ->
    case mnesia:add_table_copy(T1, Node, StorageType) of
	{atomic, ok} ->
	    copy_tables(Trest, Node, StorageType);
	_ ->
	    exit({error, "Unable to copy table(s). add_node/2 failed"})
    end.
copy_orber_tables([], Node) ->
    case rpc:call(Node, application, start, [orber]) of
	ok ->
	    ok;
	_->
	    exit({error, "All tables installed but failed to start orber on node: '"++
		  atom_to_list(Node)})
    end;
copy_orber_tables([THead|TTail], Node) ->
    case mnesia:add_table_copy(THead, Node, ram_copies) of
	{atomic, ok} ->
	    copy_orber_tables(TTail, Node);
	_ ->
	    exit({error, "Unable to copy table(s). add_node/2 failed"})
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
		    exit({error, "Unable to reach node: '"++atom_to_list(Node)++ 
			  "' remove_node/1 failed"})
	    end;
	no ->
	    case rpc:call(Node, mnesia, start, []) of
		ok ->
		    remove_tables(?ifr_object_list ++ ?ORBER_TABS, Node),
		    rpc:call(Node, mnesia, stop, []);
		_->
		    exit({error, "Unable to reach node: '"++atom_to_list(Node)++ 
			  "' remove_node/1 failed"})
	    end;
	{badrpc, Reason} ->
	    exit({error, "Unable to contact node '"++atom_to_list(Node)++ 
		  "' remove_node/1 failed."})
    end.


remove_tables(Tables, Node) ->
    remove_tables(Tables, Node, []).

remove_tables([], Node, []) -> ok;
remove_tables([], Node, Failed) ->
    exit({error, "Unable to remove table(s). Remove_node/1 failed."});
remove_tables([T1|Trest], Node, Failed) ->
    case mnesia:del_table_copy(T1, Node) of
	{atomic, ok} ->
	    remove_tables(Trest, Node, Failed);
	_ ->
	    remove_tables(Trest, Node, [T1|Failed])
    end.



%%-----------------------------------------------------------------
%% Internal interface functions
%%-----------------------------------------------------------------
%% This a function requested by IC.
throw_helper('INTF_REPOS', 'COMPLETED_YES') ->
    corba:raise(#'INTF_REPOS'{completion_status=?COMPLETED_YES});
throw_helper('INTF_REPOS', 'COMPLETED_NO') ->
    corba:raise(#'INTF_REPOS'{completion_status=?COMPLETED_NO});
throw_helper('INTF_REPOS', 'COMPLETED_MAYBE') ->
    corba:raise(#'INTF_REPOS'{completion_status=?COMPLETED_MAYBE}).




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
    case application:get_env(orber, orber_debug_level) of
	{ok, Level} when integer(Level), Level >= RequestedLevel ->
	    %% Use the catch if incorrect format used somewhere.
	    catch error_logger:error_msg("=================== Orber =================~n"++
					 Format++
					 "~n===========================================~n",
					 Data),
	    ok;
	_ ->
	    ok
    end.

%%------ Keys we can update at any time -----
%% Initial Services References
configure(orbDefaultInitRef, String) when list(String) ->
    do_configure(orbDefaultInitRef, String);
configure(orbDefaultInitRef, undefined) ->
    do_configure(orbDefaultInitRef, undefined);
configure(orbInitRef, String) when list(String) ->
    do_configure(orbInitRef, String);
configure(orbInitRef, undefined) ->
    do_configure(orbInitRef, undefined);
%% IIOP-version
configure(giop_version, {1, 0}) ->
    do_configure(giop_version, {1, 0});
configure(giop_version, {1, 1}) ->
    do_configure(giop_version, {1, 1});
configure(giop_version, {1, 2}) ->
    do_configure(giop_version, {1, 2});
%% configure 'iiop_timout' will only have effect on new requests.
configure(iiop_timeout, infinity) ->
    do_configure(iiop_timeout, infinity);
configure(iiop_timeout, Value) when integer(Value), Value =< 1000000 ->
    do_configure(iiop_timeout, Value);
%% configure 'iiop_connection_timout' will only have effect on new connections.
configure(iiop_connection_timeout, infinity) ->
    do_configure(iiop_connection_timeout, infinity);
configure(iiop_connection_timeout, Value) when integer(Value), Value =< 1000000 ->
    do_configure(iiop_connection_timeout, Value);
%% configure 'iiop_setup_connection_timeout' will only have effect on new connections.
configure(iiop_setup_connection_timeout, infinity) ->
    do_configure(iiop_setup_connection_timeout, infinity);
configure(iiop_setup_connection_timeout, Value) when integer(Value) ->
    do_configure(iiop_setup_connection_timeout, Value);
%% Garbage Collect the object keys DB.
configure(objectkeys_gc_time, infinity) ->
    do_configure(objectkeys_gc_time, infinity);
configure(objectkeys_gc_time, Value) when integer(Value), Value =< 1000000 ->
    do_configure(objectkeys_gc_time, Value);
%% Orber debug printouts
configure(orber_debug_level, Value) when integer(Value) ->
    do_configure(orber_debug_level, Value);

%%------ Keys we cannot change if Orber is running -----
%% Set the listen port
configure(iiop_port, Value) when integer(Value) ->
    do_safe_configure(iiop_port, Value);
%% IIOP interceptors
configure(interceptors, Value) when tuple(Value) ->
    do_safe_configure(interceptors, Value);

configure(Key, Value) ->
    orber:debug_level_print("[~p] orber:configure(~p, ~p); Bad key or value.", 
			    [?LINE, Key, Value], ?DEBUG_LEVEL),
    exit("Bad configure parameter(s)").

%% This function may be used as long as it is safe to change a value at any time.
do_configure(Key, Value) ->
    case is_loaded() of
	false ->
	    application:load(orber),
	    application_controller:set_env(orber, Key, Value);
	true ->
	    application_controller:set_env(orber, Key, Value)
    end.

%% This function MUST(!!) be used when we cannot change a value if Orber is running.
do_safe_configure(Key, Value) ->
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
    end.


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

del_table_index() ->
    check_mnesia_result(mnesia:del_table_index(ir_ModuleDef, id),
			"Unable to remove index for the mnesia table ir_ModuleDef."),
    check_mnesia_result(mnesia:del_table_index(ir_InterfaceDef, id),
			"Unable to remove index for the mnesia table ir_InterfaceDef."),
    check_mnesia_result(mnesia:del_table_index(ir_StructDef, id),
			"Unable to remove index for the mnesia table ir_StructDef."),
    check_mnesia_result(mnesia:del_table_index(ir_UnionDef, id),
			"Unable to remove index for the mnesia table ir_UnionDef."),
    check_mnesia_result(mnesia:del_table_index(ir_ExceptionDef, id),
			"Unable to remove index for the mnesia table ir_ExceptionDef."),
    check_mnesia_result(mnesia:del_table_index(ir_ConstantDef, id),
			"Unable to remove index for the mnesia table ir_ConstantDef."),
    check_mnesia_result(mnesia:del_table_index(ir_EnumDef, id),
			"Unable to remove index for the mnesia table ir_EnumDef."),
    check_mnesia_result(mnesia:del_table_index(ir_AliasDef, id),
			"Unable to remove index for the mnesia table ir_AliasDef."),
    check_mnesia_result(mnesia:del_table_index(ir_AttributeDef, id),
			"Unable to remove index for the mnesia table ir_AttributeDef."),
    check_mnesia_result(mnesia:del_table_index(ir_OperationDef, id),
			"Unable to remove index for the mnesia table ir_OperationDef."),
    check_mnesia_result(mnesia:del_table_index(ir_Contained, id),
			"Unable to remove index for the mnesia table ir_Contained."),
    check_mnesia_result(mnesia:del_table_index(ir_TypedefDef, id),
			"Unable to remove index for the mnesia table ir_TypedefDef.").

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
					  10000, worker, [orber_policy_server]},
					 {orber_nameservice, {orber, start_naming_service, 
							      [[]]},
					  permanent, 
					  10000, worker, ['CosNaming_NamingContext']}
					],
			    {ok, {SupFlags, ChildSpec}};
			StopReason ->
			    {stop, StopReason}
		    end
	    end;
	X ->
	    {stop, io_lib:format("GIOP ~p not an implemeted version", [X])}
    end.

start_naming_service(_) ->
    case catch 'CosNaming_NamingContext':oe_create_link([],
							[{regname, {local, orber_nameservice}},
							 {sup_child, true}]
						       ) of 
	{'EXCEPTION', E} ->
	    {error, {'EXCEPTION', E}};
	{'EXIT', R} ->
	    {error, {'EXIT', R}};
	{ok, Pid, Key} ->
	    {ok, Pid}
    end.



