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
-module(httpd_listener).
-include("httpd.hrl").
-behaviour(gen_server).

%% External API
-export([stop/1, restart/1]).

%% Internal API
-export([destroy/1, create/2]).

%% Module API
-export([config_lookup/2, config_multi_lookup/2, config_match/2]).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% Other exports (for spawn's etc.)
-export([heavy_load/4, connection/4]).


%%
%% External API
%%

%% stop

stop(ServerRef) ->
    gen_server:call(ServerRef, stop).

%% restart

restart(ServerRef) ->
    gen_server:cast(ServerRef, restart).



%%
%% Internal API
%%

%% destroy

destroy(ServerRef) ->
    gen_server:cast(ServerRef, destroy).

%% create

create(ServerRef,Pid) ->
    gen_server:cast(ServerRef, {create,Pid}).


%%
%% Module API. Theese functions are intended for use from modules only.
%%

config_lookup(Port, Query) ->
    Name = list_to_atom(lists:flatten(io_lib:format("httpd~w",[Port]))),
    gen_server:call(whereis(Name), {config_lookup, Query}).

config_multi_lookup(Port, Query) ->
    Name = list_to_atom(lists:flatten(io_lib:format("httpd~w",[Port]))),
    gen_server:call(whereis(Name), {config_multi_lookup, Query}).

config_match(Port, Pattern) ->
    Name = list_to_atom(lists:flatten(io_lib:format("httpd~w",[Port]))),
    gen_server:call(whereis(Name), {config_match, Pattern}).


%%
%% Server call-back functions
%%

%% init

init([ConfigFile, ConfigList, Port]) ->
    process_flag(trap_exit, true),
    case httpd_conf:store(ConfigList) of
	{ok, ConfigDB} ->
	    SocketType = httpd_socket:config(ConfigDB),
	    httpd_socket:start(SocketType),
	    ListenAddr = httpd_util:lookup(ConfigDB, bind_address),
	    ListenSocket = httpd_socket:listen(SocketType, ListenAddr, Port),
	    case ListenSocket of
		{error, Reason} ->
		    {stop, {error, {listen, Reason}}};
		_Else ->
		    proc_lib:spawn_link(httpd_listener, connection,
					[self(), SocketType, 
					 ListenSocket, ConfigDB]),
		    {ok,[SocketType, ListenSocket, ConfigFile, ConfigDB, 0]}
	    end;
	{error, Reason} ->
	    {stop, Reason}
    end.

%% handle_call

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({config_lookup, Query}, _From, 
	    [SocketType,ListenSocket,ConfigFile,ConfigDB,Processes]) ->
    Res = httpd_util:lookup(ConfigDB, Query),
    {reply, Res, [SocketType,ListenSocket,ConfigFile,ConfigDB,Processes]};

handle_call({config_multi_lookup, Query}, _From, 
	    [SocketType,ListenSocket,ConfigFile,ConfigDB,Processes]) ->
    Res = httpd_util:multi_lookup(ConfigDB, Query),
    {reply, Res, [SocketType,ListenSocket,ConfigFile,ConfigDB,Processes]};

handle_call({config_match, Query}, _From, 
	    [SocketType,ListenSocket,ConfigFile,ConfigDB,Processes]) ->
    Res = ets:match_object(ConfigDB, Query),
    {reply, Res, [SocketType,ListenSocket,ConfigFile,ConfigDB,Processes]};

handle_call(Request, _From, State) ->
    {reply, ok, State}.

%% handle_cast

handle_cast(destroy, [SocketType,ListenSocket,ConfigFile,ConfigDB,Processes]) ->
    {noreply, [SocketType,ListenSocket,ConfigFile,ConfigDB,Processes-1]};
handle_cast({create, Pid}, [SocketType,ListenSocket,ConfigFile,ConfigDB,Processes]) ->
    case httpd_util:lookup(ConfigDB, max_clients, 150) of
	Processes ->
	    proc_lib:spawn_link(httpd_listener, heavy_load,
				[self(), SocketType, ListenSocket, ConfigDB]),
	    {noreply, [SocketType,ListenSocket,ConfigFile,ConfigDB,Processes]};
	_ ->
	    proc_lib:spawn_link(httpd_listener, connection,
				[self(), SocketType, ListenSocket, ConfigDB]),
	    {noreply, [SocketType,ListenSocket,ConfigFile,ConfigDB,Processes+1]}
    end.

%% handle_info

handle_info(Info, State) ->
    case Info of
	{'EXIT', Pid, normal} ->
	    do_nothing;
	{'EXIT', Pid, {accept_failed, Err}} ->
	    %% Accept failed. Start a new connection process.
	    create(self(), self());
	{'EXIT', Pid, {error, normal}} ->
	    %% Bug in gen_tcp
	    create(self(), self());
	_ ->
	    String = lists:flatten(io_lib:format("Error: ~p", [Info])),
	    error_logger:error_report(String)
    end,
    ?DEBUG("handle_info: Info: ~p State: ~p", [Info, State]),
    {noreply, State}.

%% terminate

terminate(Reason, [SocketType,ListenSocket,ConfigFile,ConfigDB,Processes]) -> 
    httpd_socket:close(SocketType, ListenSocket),
    httpd_conf:remove_all(ConfigDB),
    ok.

%% heavy_load

%% The 30000 ms timeout here and in connection/4 serves the purpose of
%% making old code not stick around indefinitely when upgrading.

heavy_load(Listener,SocketType,ListenSocket,ConfigDB) ->
    case httpd_socket:accept(SocketType,ListenSocket, 30000) of
	{error, timeout} ->
	    ?MODULE:heavy_load(Listener,SocketType,ListenSocket,ConfigDB);
	Socket ->
	    create(Listener,self()),
	    MaxClients = httpd_util:lookup(ConfigDB,max_clients,150),
	    String = io_lib:format("heavy load (>~w processes)",[MaxClients]),
	    httpd_response:send_status(SocketType,Socket,504,String,ConfigDB),
	    destroy(Listener),
	    httpd_socket:close(SocketType,Socket)
    end.

%% connection

connection(Listener,SocketType,ListenSocket,ConfigDB) ->
    case catch httpd_socket:accept(SocketType,ListenSocket, 30000) of
	{error, timeout} ->
	    ?MODULE:connection(Listener,SocketType,ListenSocket,ConfigDB);
	{error, {enfile, _}} ->
	    %% Out of sockets...
	    receive after 200 -> ok end,
	    ?MODULE:connection(Listener, SocketType, ListenSocket, ConfigDB);
	{error, closed} ->
	    exit(normal);
	{error, Reason} ->
	    accept_failed(SocketType, ConfigDB, Reason);
	{'EXIT', Reason} ->
	    accept_failed(SocketType, ConfigDB, Reason);
	Socket ->
	    handle_connection(Listener, SocketType, Socket, ConfigDB)
    end.

handle_connection(Listener, SocketType, Socket, ConfigDB) ->
    Resolve = httpd_socket:resolve(SocketType),
    Peername = httpd_socket:peername(SocketType, Socket),
    InitData = #init_data{peername=Peername, resolve=Resolve},
    create(Listener,self()),
    MaxRequests = httpd_util:lookup(ConfigDB, keep_alive, 1),
    do_next_connection(InitData, SocketType, Socket, ConfigDB, 
		       MaxRequests, 60000), % XXX Was infinity
    destroy(Listener),
    httpd_socket:close(SocketType,Socket).

do_next_connection(_InitData, _SocketType, _Socket, _ConfigDB, 0, _Timeout) ->
    ok;
do_next_connection(InitData, SocketType, Socket, ConfigDB, MaxRequests, Timeout) ->
    Peername = InitData#init_data.peername,
    case catch httpd_request:read(SocketType,
				  Socket,
				  ConfigDB,
				  InitData, 
				  Timeout) of
	{'EXIT',Reason} ->
	    error_logger:error_report({'EXIT',Reason}),
	    mod_log:error_log(SocketType,Socket,ConfigDB,Peername,Reason),
	    mod_disk_log:error_log(SocketType,Socket,ConfigDB,Peername,Reason);
	Info when record(Info, mod) ->
	    case Info#mod.connection of
		keep_alive ->
		    RequestTimeout = httpd_util:lookup(ConfigDB, 
						      keep_alive_timeout, 
						      15000),
		    do_next_connection(InitData,
				       SocketType,
				       Socket, 
				       ConfigDB, 
				       MaxRequests-1, 
				       RequestTimeout);
		_ ->
		    ok
	    end;
	_ ->
	    ok
    end.

accept_failed(SocketType, ConfigDB, Error) ->
    String = lists:flatten(io_lib:format("Accept failed: ~p", [Error])),
    error_logger:error_report(String),
    mod_log:error_log(SocketType, undefined, ConfigDB, {0, "unknown"}, String),
    mod_disk_log:error_log(SocketType, undefined, ConfigDB, {0, "unknown"}, String),
    exit({accept_failed, String}).
