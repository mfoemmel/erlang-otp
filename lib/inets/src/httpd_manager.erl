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

-module(httpd_manager).

-include("httpd.hrl").
-include("httpd_verbosity.hrl").

-behaviour(gen_server).

%% External API
-export([start/2, start_link/2, stop/1, restart/1]).

%% Internal API
-export([new_connection/2, done_connection/2]).

%% Module API
-export([config_lookup/2, config_lookup/3, 
	 config_multi_lookup/2, config_multi_lookup/3, 
	 config_match/2, config_match/3]).

%% gen_server exports
-export([init/1, 
	 handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2,
         code_change/3]).

%% Management exports
-export([get_status/1,get_status/2,is_busy/1,is_busy/2]).
-export([verbosity/2,verbosity/3]).


-record(state,{socket_type = ip_comm,
	       socket,
	       config_file,
	       config_db = null,
	       connection_count = 0,
	       listener,
	       status = []}).


%% ----
%% This (and the report_error/4 function) is just temporary
%% and should be removed eventually.

%%-define(httpd_verbose,true).
-ifdef(httpd_verbose).
-define(REPORT_ERROR(Db,FS,A),report_error(Db,FS,A,?LINE)).
-else.
-define(REPORT_ERROR(Db,FS,A),ok).
-endif.


%%
%% External API
%%

start(ConfigFile,ConfigList) ->
    Port = httpd_util:key1search(ConfigList,port,80),
    Addr = httpd_util:key1search(ConfigList,bind_address),
    Name = make_name(Addr,Port),
    ?LOG("start -> Name = ~p",[Name]),
    gen_server:start({local,Name},?MODULE,
		     [ConfigFile,ConfigList,Addr,Port],[]).
    
start_link(ConfigFile,ConfigList) ->
    Port = httpd_util:key1search(ConfigList,port,80),
    Addr = httpd_util:key1search(ConfigList,bind_address),
    Name = make_name(Addr,Port),
    ?LOG("start_link -> Name = ~p",[Name]),
    gen_server:start_link({local,Name},?MODULE,
			  [ConfigFile,ConfigList,Addr,Port],[]).
    
%% stop

stop(ServerRef) ->
    gen_server:call(ServerRef, stop).

%% restart

restart(ServerRef) ->
    gen_server:cast(ServerRef, restart).

%% get_status

get_status(ServerRef) ->
    gen_server:call(ServerRef,get_status).

get_status(ServerRef,Timeout) ->
    gen_server:call(ServerRef,get_status,Timeout).


verbosity(ServerRef,Verbosity) ->
    verbosity(ServerRef,all,Verbosity).

verbosity(ServerRef,all,Verbosity) ->
    gen_server:call(ServerRef,{verbosity,all,Verbosity});
verbosity(ServerRef,manager,Verbosity) ->
    gen_server:call(ServerRef,{verbosity,manager,Verbosity});
verbosity(ServerRef,listener,Verbosity) ->
    gen_server:call(ServerRef,{verbosity,listener,Verbosity});
verbosity(ServerRef,manager,Verbosity) ->
    gen_server:call(ServerRef,{verbosity,security,Verbosity}).


%%
%% Internal API
%%

%% new_connection

new_connection(Manager,Action) ->
    gen_server:cast(Manager,{new_connection,Action,self()}).

%% done

done_connection(Manager,Action) ->
    gen_server:cast(Manager,{done_connection,Action,self()}).


%% is_busy(ServerRef) -> true | false
%% 
%% Tests if the server is (in usage state) busy, 
%% i.e. has rached the heavy load limit.
%% 

is_busy(ServerRef) ->
    gen_server:call(ServerRef,is_busy).
    
is_busy(ServerRef,Timeout) ->
    gen_server:call(ServerRef,is_busy,Timeout).


%%
%% Module API. Theese functions are intended for use from modules only.
%%

config_lookup(Port, Query) ->
    config_lookup(undefined, Port, Query).
config_lookup(Addr, Port, Query) ->
    Name = httpd_util:make_name("httpd",Addr,Port),
    gen_server:call(whereis(Name), {config_lookup, Query}).

config_multi_lookup(Port, Query) ->
    config_multi_lookup(undefined,Port,Query).
config_multi_lookup(Addr,Port, Query) ->
    Name = httpd_util:make_name("httpd",Addr,Port),
    gen_server:call(whereis(Name), {config_multi_lookup, Query}).

config_match(Port, Pattern) ->
    config_match(undefined,Port,Pattern).
config_match(Addr, Port, Pattern) ->
    Name = httpd_util:make_name("httpd",Addr,Port),
    gen_server:call(whereis(Name), {config_match, Pattern}).


%%
%% Server call-back functions
%%

%% init

init([ConfigFile, ConfigList, Addr, Port]) ->
    init([ConfigFile, ConfigList, Addr, Port, ?default_verbosity]);

init([ConfigFile, ConfigList, Addr, Port, Verbosity]) ->
    ?LOG("init -> ~n"
	 "     Addr: ~p~n"
	 "     Port: ~p", [Addr,Port]),
    process_flag(trap_exit, true),
    put(sname,man),
    put(verbosity,?vvalidate(Verbosity)),
    ?vlog("starting",[]),
    case httpd_conf:store(ConfigList) of
	{ok, ConfigDB} ->
	    SocketType = httpd_socket:config(ConfigDB),
	    case httpd_socket:start(SocketType) of
		ok ->
		    ListenSocket = httpd_socket:listen(SocketType,Addr,Port),
		    ?DEBUG("init -> ListenSocket: ~p", [ListenSocket]),
		    case ListenSocket of
			{error, Reason} ->
			    ?vinfo("failed socket listen operation: ~p",
				   [Reason]),
			    {stop, {error, {listen, Reason}}};
			_Else ->
			    %% Create the first (active) listener
			    ?vdebug("create listener",[]),
			    Pid = httpd_listener:start_link(SocketType,
							    ListenSocket,
							    ConfigDB),

			    Status = [{max_conn,0},
				      {last_heavy_load,never},
				      {last_connection,never}],

			    State = #state{socket_type      = SocketType,
					   socket           = ListenSocket,
					   config_file      = ConfigFile,
					   config_db        = ConfigDB,
					   connection_count = 0,
					   listener         = Pid,
					   status           = Status},
			    {ok,State}
		    end;
		{error,Reason} ->
		    ?LOG("init -> socket start failed: ~p", [Reason]),
		    ?vinfo("failed socket start: ~p",[Reason]),
		    {stop,{socket_start_failed,Reason}}
	    end;
	{error, Reason} ->
	    ?LOG("init -> storage error: ~p", [Reason]),
	    ?vinfo("failed storing coniguration: ~p",[Reason]),
	    {stop, Reason}
    end.
   

%% handle_call

handle_call(stop, _From, State) ->
    ?LOG("handle_call -> stop", []),
    ?vlog("stop",[]),
    {stop, normal, ok, State};

handle_call({config_lookup, Query}, _From, State) ->
    ?LOG("handle_call -> config_lookup: Query = ~p", [Query]),
    ?vlog("config lookup: Query = ~p",[Query]),
    Res = httpd_util:lookup(State#state.config_db, Query),
    ?DEBUG("handle_call -> config_lookup: Res   = ~p", [Res]),
    ?vdebug("config lookup result: ~p",[Res]),
    {reply, Res, State};

handle_call({config_multi_lookup, Query}, _From, State) ->
    ?LOG("handle_call -> config_multi_lookup: Query = ~p", [Query]),
    ?vlog("multi config lookup: Query = ~p",[Query]),
    Res = httpd_util:multi_lookup(State#state.config_db, Query),
    ?DEBUG("handle_call -> config_multi_lookup: Res   = ~p", [Res]),
    ?vdebug("multi config lookup result: ~p",[Res]),
    {reply, Res, State};

handle_call({config_match, Query}, _From, State) ->
    ?LOG("handle_call -> config_match: Query = ~p", [Query]),
    ?vlog("config match: Query = ~p",[Query]),
    Res = ets:match_object(State#state.config_db, Query),
    ?DEBUG("handle_call -> config_match: Res   = ~p", [Res]),
    ?vdebug("config match result: ~p",[Res]),
    {reply, Res, State};

handle_call(get_status, _From, State) ->
    ?LOG("handle_call -> get_status", []),
    ?vdebug("get status",[]),
    ManagerStatus  = manager_status(self()),
    ListenerStatus = listener_status(State#state.listener),
    S1 = [{current_conn,State#state.connection_count}|State#state.status]++
	[ManagerStatus,ListenerStatus],
    ?vtrace("status = ~p",[S1]),
    {reply,S1,State};

handle_call(is_busy, From, State) ->
    ?LOG("handle_call -> is_busy(~p) when connection count: ~p", 
	 [From,State#state.connection_count]),
    Reply = case get_usage_state(State) of
		busy ->
		    true;
		_ ->
		    false
	  end,
    ?DEBUG("handle_call -> is_busy: ~p", [Reply]),
    ?vlog("is busy = ~p",[Reply]),
    {reply,Reply,State};

handle_call({verbosity,Who,Verbosity}, From, State) ->
    V = ?vvalidate(Verbosity),
    ?vlog("~n   Set new verbosity to ~p for ~p",[V,Who]),
    Reply = set_verbosity(Who,V,State),
    {reply,Reply,State};

handle_call(Request, From, State) ->
    ?ERROR("handle_call -> ~n"
	   "\tUnknown request: ~p~n"
	   "\tFrom:            ~p", 
	   [Request,From]),
    ?vinfo("~n   unknown call '~p' from ~p"
	   "~n   when listener = ~p",[Request,From,State#state.listener]),
    String = 
	lists:flatten(
	  io_lib:format("Unexpected (call) request to ~p from ~p when ~n" ++
			"\tListener: ~p~n" ++
			"\tSocket: ~p~n" ++
			"\t=> ~p",
			[self(),From,State#state.listener,
			 State#state.socket,Request])),
    error_logger:error_report(String),
    ?REPORT_ERROR(State#state.config_db,String,[]),
    {reply, ok, State}.


%% handle_cast

handle_cast({new_connection,reject,OldListener}, State) ->
    ?LOG("handle_cast -> new_connection(reject,~p) when connection count: ~p",
	 [OldListener,State#state.connection_count]),
    ?vlog("~n   New connection rejected by ~p when connection count = ~p",
	  [OldListener,State#state.connection_count]),
    Status   = update_heavy_load_status(State#state.status),
    Listener = create_listener(State),
    %% ?DEBUG("handle_cast -> new_connection(reject,~p): New Listener: ~p", 
    ?DEBUG("handle_cast -> new_connection(reject,~p): New Listener: ~p", 
	   [OldListener,Listener]),
    ?vdebug("New listener = ~p",[Listener]),
    {noreply,State#state{listener = Listener, status = Status}};

handle_cast({new_connection,accept,OldListener}, State) ->
    Cnt = State#state.connection_count,
    ?LOG("handle_cast -> new_connection(accept,~p) when connection count: ~p",
	 [OldListener,Cnt]),
    ?vlog("~n   New connection accepted by ~p when connection count = ~p",
	  [OldListener,Cnt]),
    S1       = State#state{connection_count = Cnt+1},
    Status   = update_connection_status(State#state.status,Cnt+1),
    Listener = create_listener(S1),
    %%?DEBUG("handle_cast -> new_connection(accept,~p): New listener: ~p",
    ?DEBUG("handle_cast -> new_connection(accept,~p): New listener: ~p",
	   [OldListener,Listener]),
    ?vdebug("New listener = ~p",[Listener]),
    {noreply,S1#state{listener = Listener, status = Status}};

handle_cast({done_connection,reject,Pid},State) ->
    ?LOG("handle_cast -> done_connection(reject,~p)", [Pid]),
    ?vlog("~n   Done rejected connection (~p)",[Pid]),
    {noreply, State};

handle_cast({done_connection,accept,Pid},State) ->
    Cnt = State#state.connection_count,
    ?LOG("handle_cast -> done_connection(accept,~p): Connection count: ~p", 
	 [Pid,Cnt]),
    ?vlog("~n   Done accepted connection (~p) when connection count = ~p",
	  [Pid,Cnt]),
    S1 = State#state{connection_count = Cnt-1},
    {noreply, S1};

handle_cast(Message,State) ->
    ?ERROR("handle_cast -> Unknown message: ~p", [Message]),
    ?vinfo("~n   unknown cast '~p'",[Message]),
    String = 
	lists:flatten(
	  io_lib:format("Unexpected (cast) message to ~p when ~n" ++
			"\tListener: ~p~n" ++
			"\tSocket: ~p~n" ++
			"\t=> ~p",
			[self(),State#state.listener,
			 State#state.socket,Message])),
    error_logger:error_report(String),
    ?REPORT_ERROR(State#state.config_db,String,[]),
    {noreply, State}.

%% handle_info

handle_info({'EXIT', Pid, normal}, State) ->
    ?vdebug("~n   Normal exit message from ~p",[Pid]),
    {noreply, State};

handle_info({'EXIT', Pid, {accept_failed, Err}},State) ->
    %% Accept failed. Start a new connection process.
    ?ERROR("handle_info -> accept failed: ~n" ++ 
	   "      Listener: ~p~n" ++ 
	   "      Pid:      ~p~n" ++ 
	   "      Err:      ~p", 
	   [State#state.listener,Pid,Err]),
    ?vlog("~n   Accept failed exit message from ~p"
	  "~n   with reason ~p",[Pid,Err]),
    L = create_listener(State),
    ?vdebug("New listener: ~p",[L]),
    {noreply, State#state{listener = L}};

handle_info({'EXIT', Pid, {error, normal}}, State) ->
    %% Bug in gen_tcp
    ?ERROR("handle_info -> 'normal' error: ~n" ++
	   "      Listener: ~p~n" ++ 
	   "      Pid:      ~p",
	   [State#state.listener,Pid]),
    ?vlog("~n   Error(normal) exit message from ~p",[Pid]),
    L = create_listener(State),
    ?vdebug("New listener: ~p",[L]),
    {noreply, State#state{listener = L}};

handle_info(Info, State) ->
    ?ERROR("handle_info -> Info: ~p", [Info]),
    ?vinfo("~n   unknown info '~p'",[Info]),
    String = 
	lists:flatten(
	  io_lib:format("Unexpected message to ~p when ~n" ++
			"\tListener: ~p~n" ++
			"\tSocket: ~p~n" ++
			"\t=> ~p",
			[self(),State#state.listener,
			 State#state.socket,Info])),
    error_logger:error_report(String),
    ?REPORT_ERROR(State#state.config_db,String,[]),
    {noreply, State}.

%% terminate

terminate(Reason, State) -> 
    ?LOG("terminate -> \n"
	 "\tReason:    ~p\n"
	 "\tProcesses: ~p\n"
	 "\tStatus:    ~p",
	 [Reason,State#state.connection_count,State#state.status]),
    ?vlog("~n   Terminating for reason: ~p",[Reason]),
    Res = httpd_socket:close(State#state.socket_type,State#state.socket),
    ?DEBUG("terminate -> socket close result: ~p", [Res]),
    httpd_conf:remove_all(State#state.config_db),
    ok.


%% code_change({down,ToVsn}, State, Extra)
%% 
%% NOTE:
%% Actually upgrade from 2.5.1 to 2.5.3 and downgrade from 
%% 2.5.3 to 2.5.1 is done with an application restart, so 
%% these function is actually never used. The reason for keeping
%% this stuff is only for future use.
%%
code_change({down,103432255266931353046884123370331229687},State,Extra) ->
    ?vlog("~n   Downgrade to 2.5.1"
	  "~n   when state '~p'",[State]),
    {ok,downgrade_to_2_5_1(State)};

%% code_change(FromVsn, State, Extra)
%%
code_change(103432255266931353046884123370331229687,State,Extra) ->
    ?vlog("~n   Upgrade from 2.5.1"
	  "~n   when state '~p'",[State]),
    {ok,upgrade_from_2_5_1(State)}.


upgrade_from_2_5_1([SocketType,ListenSocket,ConfigFile,ConfigDB,Processes]) ->
    #state{socket_type      = SocketType,
	   socket           = ListenSocket,
	   config_file      = ConfigFile,
	   config_db        = ConfigDB,
	   connection_count = Processes,
	   listener         = upgrade_undefined,
	   status           = []}.

downgrade_to_2_5_1(State) ->
    [State#state.socket_type,
     State#state.socket,
     State#state.config_file,
     State#state.config_db,
     State#state.connection_count].


%% create_listener

create_listener(State) ->
    UsageState    = get_usage_state(State),
    ?DEBUG("create_listener -> Connection count: ~p => ~p",
	   [State#state.connection_count,UsageState]), 
    ?vtrace("Create listener whehn connection count = ~p and usage state = ~p",
	    [State#state.connection_count,UsageState]),
    ConfigDB      = State#state.config_db,
    SocketType    = State#state.socket_type,
    ListenSocket  = State#state.socket,
    create_listener(UsageState,SocketType,ListenSocket,ConfigDB).

create_listener(UsageState,SocketType,ListenSocket,ConfigDB) ->
    httpd_listener:start_link(UsageState,SocketType,ListenSocket,ConfigDB).


%% get_usage_state(State) -> idle | active | busy
%%
%% Retrieve the usage state of the HTTP server:
%%   0 active connection          -> idle
%%   max_count active connections -> busy
%%   Otherwise                    -> active
%%
get_usage_state(State) ->
    get_usage_state(State#state.connection_count,State).

get_usage_state(0,_State) ->
    idle;
get_usage_state(ConnectionCnt,State) ->
    ConfigDB = State#state.config_db,
    case httpd_util:lookup(ConfigDB, max_clients, 150) of
	ConnectionCnt ->
	    busy;
	_ ->
	    active
    end.


%% Some status utility functions

update_heavy_load_status(Status) ->
    update_status_with_time(Status,last_heavy_load).

update_connection_status(Status,ConnCount) ->
    S1 = case lists:keysearch(max_conn,1,Status) of
	     {value,{max_conn,C1}} when ConnCount > C1 ->
		 lists:keyreplace(max_conn,1,Status,{max_conn,ConnCount});
	     {value,{max_conn,C2}} ->
		 Status;
	     false ->
		 [{max_conn,ConnCount}|Status]
	 end,
    update_status_with_time(S1,last_connection).

update_status_with_time(Status,Key) ->
    lists:keyreplace(Key,1,Status,{Key,universal_time()}).

universal_time() -> calendar:universal_time().


listener_status(P) ->
    Items = [status,message_queue_len,reductions,
	     heap_size,stack_size,current_function],
    {listener_status,process_status(P,Items,[])}.

manager_status(P) ->
    Items = [status,message_queue_len,reductions,
	     heap_size,stack_size],
    {manager_status,process_status(P,Items,[])}.


process_status(P,[],L) ->
    [{pid,P}|lists:reverse(L)];
process_status(P,[H|T],L) ->
    case (catch process_info(P,H)) of
	{H,Value} ->
	    process_status(P,T,[{H,Value}|L]);
	_ ->
	    process_status(P,T,[{H,undefined}|L])
    end.
	
make_name(Addr,Port) ->
    httpd_util:make_name("httpd",Addr,Port).

-ifdef(httpd_verbose).
report_error(ConfigDB,FStr,Args,Line) ->
    String = lists:flatten(io_lib:format("Error at line ~w: " ++ FStr, 
					 [Line|Args])),
    error_logger:error_report(String),
    mod_log:report_error(ConfigDB,String),
    mod_disk_log:report_error(ConfigDB,String).
-endif.


set_verbosity(manager,V,_S) ->
    OldVerbosity = get(verbosity),
    put(verbosity,V),
    OldVerbosity;
set_verbosity(listener,V,_S) ->
    OldVerbosity = get(listener_verbosity),
    put(listener_verbosity,V),
    OldVerbosity;
set_verbosity(security,V,S) ->
    put(security_verbosity,V),
    Addr = httpd_util:lookup(S#state.config_db, bind_address),
    Port = httpd_util:lookup(S#state.config_db, port),
    mod_security:verbosity(Addr,Port,V);
set_verbosity(all,V,S) ->
    OldMv = get(verbosity),
    put(verbosity,V),
    OldLv = get(listener_verbosity),
    put(listener_verbosity,V),
    put(security_verbosity,V),
    Addr = httpd_util:lookup(S#state.config_db, bind_address),
    Port = httpd_util:lookup(S#state.config_db, port),
    OldSv = mod_security:verbosity(Addr,Port,V),
    [{manager,OldMv},{listener,OldLv},{security,OldSv}].
    
    
