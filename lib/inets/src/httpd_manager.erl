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
-export([start/2, start/3, start_link/2, start_link/3, stop/1, restart/1]).

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
-export([block/2, block/3, unblock/1]).
-export([get_admin_state/1, get_usage_state/1]).
-export([is_busy/1, is_busy/2, is_busy_or_blocked/1, is_blocked/1]).
-export([get_status/1, get_status/2]).
-export([verbosity/2, verbosity/3]).


-record(state,{socket_type  = ip_comm,
	       socket,
	       config_file,
	       config_db    = null,
	       connections, %% Previous listeners now handling requests
	       listener,    %% Current listener
	       admin_state  = unblocked,
	       blocker_ref  = undefined,
	       blocking_tmr = undefined,
	       status       = []}).


%%
%% External API
%%

start(ConfigFile, ConfigList) ->
    start(ConfigFile, ConfigList, []).

start(ConfigFile, ConfigList, Verbosity) ->
    Port = httpd_util:key1search(ConfigList, port, 80),
    Addr = httpd_util:key1search(ConfigList, bind_address),
    Name = make_name(Addr,Port),
    ?LOG("start -> Name = ~p",[Name]),
    gen_server:start({local,Name},?MODULE,
		     [ConfigFile, ConfigList, Addr, Port, Verbosity],[]).
    
start_link(ConfigFile, ConfigList) ->
    start_link(ConfigFile, ConfigList, []).
    
start_link(ConfigFile, ConfigList, Verbosity) ->
    Port = httpd_util:key1search(ConfigList, port, 80),
    Addr = httpd_util:key1search(ConfigList, bind_address),
    Name = make_name(Addr,Port),
    ?LOG("start_link -> Name = ~p",[Name]),
    gen_server:start_link({local,Name},?MODULE,
			  [ConfigFile, ConfigList, Addr, Port, Verbosity],[]).
    
%% stop

stop(ServerRef) ->
    gen_server:call(ServerRef, stop).

%% restart

restart(ServerRef) ->
    gen_server:call(ServerRef, restart).


%%%----------------------------------------------------------------

block(ServerRef, disturbing) ->
    call(ServerRef,block);

block(ServerRef, non_disturbing) ->
    do_block(ServerRef, non_disturbing, infinity).

block(ServerRef, Method, Timeout) ->
    do_block(ServerRef, Method, Timeout).


%% The reason for not using call here, is that the manager cannot
%% _wait_ for completion of the requests. It must be able to do
%% do other things at the same time as the blocking goes on.
do_block(ServerRef, Method, infinity) ->
    Ref = make_ref(),
    cast(ServerRef, {block, Method, infinity, self(), Ref}),
    receive
	{block_reply, Reply, Ref} ->
	    Reply
    end;
do_block(ServerRef,Method,Timeout) when Timeout > 0 ->
    Ref = make_ref(),
    cast(ServerRef,{block,Method,Timeout,self(),Ref}),
    receive
	{block_reply,Reply,Ref} ->
	    Reply
    end.


%%%----------------------------------------------------------------

%% unblock

unblock(ServerRef) ->
    call(ServerRef,unblock).

%% get admin/usage state

get_admin_state(ServerRef) ->
    call(ServerRef,get_admin_state).

get_usage_state(ServerRef) ->
    call(ServerRef,get_usage_state).


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
verbosity(ServerRef,security,Verbosity) ->
    gen_server:call(ServerRef,{verbosity,security,Verbosity});
verbosity(ServerRef,auth,Verbosity) ->
    gen_server:call(ServerRef,{verbosity,auth,Verbosity}).

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


%% is_busy_or_blocked(ServerRef) -> busy | blocked | false
%% 
%% Tests if the server is busy (usage state), i.e. has rached,
%% the heavy load limit, or blocked (admin state) .
%% 

is_busy_or_blocked(ServerRef) ->
    gen_server:call(ServerRef,is_busy_or_blocked).
    

%% is_blocked(ServerRef) -> true | false
%% 
%% Tests if the server is blocked (admin state) .
%% 

is_blocked(ServerRef) ->
    gen_server:call(ServerRef,is_blocked).
    

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


init([ConfigFile, ConfigList, Addr, Port, Verbosity]) ->
    ?LOG("init -> ~n"
	 "     Addr: ~p~n"
	 "     Port: ~p", [Addr,Port]),
    process_flag(trap_exit, true),
    put(sname,man),
    set_verbosity(Verbosity),
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

			    State = #state{socket_type = SocketType,
					   socket      = ListenSocket,
					   config_file = ConfigFile,
					   config_db   = ConfigDB,
					   connections = [],
					   listener    = Pid,
					   status      = Status},
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
    ?vlog("stop",[]),
    {stop, normal, ok, State};

handle_call({config_lookup, Query}, _From, State) ->
    ?vlog("config lookup: Query = ~p",[Query]),
    Res = httpd_util:lookup(State#state.config_db, Query),
    ?vdebug("config lookup result: ~p",[Res]),
    {reply, Res, State};

handle_call({config_multi_lookup, Query}, _From, State) ->
    ?vlog("multi config lookup: Query = ~p",[Query]),
    Res = httpd_util:multi_lookup(State#state.config_db, Query),
    ?vdebug("multi config lookup result: ~p",[Res]),
    {reply, Res, State};

handle_call({config_match, Query}, _From, State) ->
    ?vlog("config match: Query = ~p",[Query]),
    Res = ets:match_object(State#state.config_db, Query),
    ?vdebug("config match result: ~p",[Res]),
    {reply, Res, State};

handle_call(get_status, _From, State) ->
    ?vdebug("get status",[]),
    ManagerStatus  = manager_status(self()),
    ListenerStatus = listener_status(State#state.listener),
    AuthStatus     = auth_status(get(auth_server)),
    SecStatus      = sec_status(get(sec_server)),
    S1 = [{current_conn,length(State#state.connections)}|State#state.status]++
	[ManagerStatus, ListenerStatus, AuthStatus, SecStatus],
    ?vtrace("status = ~p",[S1]),
    {reply,S1,State};

handle_call(is_busy, From, State) ->
    Reply = case get_ustate(State) of
		busy ->
		    true;
		_ ->
		    false
	  end,
    ?vlog("is busy: ~p",[Reply]),
    {reply,Reply,State};

handle_call(is_busy_or_blocked, From, State) ->
    Reply = 
	case get_astate(State) of
	    unblocked ->
		case get_ustate(State) of
		    busy ->
			busy;
		    _ ->
			false
		end;
	    _ ->
		blocked
	  end,
    ?vlog("is busy or blocked: ~p",[Reply]),
    {reply,Reply,State};

handle_call(is_blocked, From, State) ->
    Reply = 
	case get_astate(State) of
	    unblocked ->
		false;
	    _ ->
		true
	  end,
    ?vlog("is blocked: ~p",[Reply]),
    {reply,Reply,State};

handle_call(get_admin_state, From, State) ->
    Reply = get_astate(State),
    ?vlog("admin state: ~p",[Reply]),
    {reply,Reply,State};

handle_call(get_usage_state, From, State) ->
    Reply = get_ustate(State),
    ?vlog("usage state: ~p",[Reply]),
    {reply,Reply,State};

handle_call({verbosity,Who,Verbosity}, From, State) ->
    V = ?vvalidate(Verbosity),
    ?vlog("~n   Set new verbosity to ~p for ~p",[V,Who]),
    Reply = set_verbosity(Who,V,State),
    {reply,Reply,State};

handle_call(restart, From, State) when State#state.admin_state == blocked ->
    ?vlog("restart",[]),
    case handle_restart(State) of
	{stop, Reply, S1} ->
	    {stop, Reply, S1};
	{_, Reply, S1} ->
	    {reply, Reply, S1}
    end;

handle_call(restart, From, State) ->
    ?vlog("restart(~p)",[State#state.admin_state]),
    {reply,{error,{invalid_admin_state,State#state.admin_state}},State};

handle_call(block, From, State) ->
    ?vlog("block(disturbing)",[]),
    {Reply,S1} = handle_block(State),
    {reply,Reply,S1};

handle_call(unblock, {From,_Tag}, State) ->
    ?vlog("unblock",[]),
    {Reply,S1} = handle_unblock(State,From),
    {reply,Reply,S1};

handle_call(Request, From, State) ->
    ?ERROR("handle_call -> ~n"
	   "\tUnknown request: ~p~n"
	   "\tFrom:            ~p", 
	   [Request,From]),
    ?vinfo("~n   unknown call '~p' from ~p"
	   "~n   when listener = ~p",[Request,From,State#state.listener]),
    String = 
	lists:flatten(
	  io_lib:format("Unknown request to manager (~p) from ~p when ~n" ++
			"\tListener: ~p~n" ++
			"\tSocket: ~p~n" ++
			"\t=> ~p",
			[self(),From,State#state.listener,
			 State#state.socket,Request])),
    report_error(State,String),
    {reply, ok, State}.


%% handle_cast

handle_cast({new_connection,reject,OldListener}, State) ->
    ?vlog("~n   New connection rejected by ~p when connection count = ~p",
	  [OldListener,length(State#state.connections)]),
    Status   = update_heavy_load_status(State#state.status),
    Listener = create_listener(State),
    ?vdebug("New listener = ~p",[Listener]),
    {noreply,State#state{listener = Listener, status = Status}};

handle_cast({new_connection,accept,OldListener}, State) ->
    Connections = State#state.connections,
    ?vlog("~n   New connection accepted by ~p when connection count = ~p",
	  [OldListener,length(Connections)]),
    S1       = State#state{connections = [OldListener|Connections]},
    Status   = update_connection_status(State#state.status,
					length(Connections)+1),
    Listener = create_listener(S1),
    ?vdebug("New listener = ~p",[Listener]),
    {noreply,S1#state{listener = Listener, status = Status}};

handle_cast({done_connection,reject,Pid},State) ->
    ?vlog("~n   Done rejected connection (~p)",[Pid]),
    {noreply, State};

handle_cast({done_connection,accept,Pid},State) ->
    Connections = State#state.connections,
    ?vlog("~n   Done accepted connection (~p) when connection count = ~p",
	  [Pid,length(Connections)]),
    S1 = 
	case {State#state.admin_state,lists:delete(Pid,Connections)} of
	    {shutting_down,[]} ->
		%% Ok, block complete
		?vlog("block complete",[]),
		demonitor_blocker(State#state.blocker_ref),
		{Tmr,From,Ref} = State#state.blocking_tmr,
		?vlog("(possibly) stop block timer",[]),
		stop_block_tmr(Tmr),
		?vlog("and send the reply",[]),
		From ! {block_reply,ok,Ref},
		State#state{admin_state = blocked, connections = [],
			    blocker_ref = undefined};
	    {_AdminState,Connections1} ->
		State#state{connections = Connections1}
	end,
    {noreply, S1};

handle_cast({block,disturbing,Timeout,From,Ref}, State) ->
    ?vlog("block(disturbing,~p)",[Timeout]),
    S1 = handle_block(State,Timeout,From,Ref),
    {noreply,S1};

handle_cast({block,non_disturbing,Timeout,From,Ref}, State) ->
    ?vlog("block(non-disturbing,~p)",[Timeout]),
    S1 = handle_nd_block(State,Timeout,From,Ref),
    {noreply,S1};

handle_cast(Message,State) ->
    ?ERROR("handle_cast -> Unknown message: ~p", [Message]),
    ?vinfo("~n   unknown cast '~p'",[Message]),
    String = 
	lists:flatten(
	  io_lib:format("Unknown message to manager (~p) when ~n" ++
			"\tListener: ~p~n" ++
			"\tSocket: ~p~n" ++
			"\t=> ~p",
			[self(),State#state.listener,
			 State#state.socket,Message])),
    report_error(State,String),
    {noreply, State}.

%% handle_info

handle_info({block_timeout,Method}, State) ->
    ?vlog("received block_timeout event",[]),
    S1 = handle_block_timeout(State,Method),
    {noreply, S1};

handle_info({'DOWN', Ref, process, _Object, Info}, State) ->
    ?vlog("~n   down message for ~p",[Ref]),
    S1 = 
	case State#state.blocker_ref of
	    Ref ->
		handle_blocker_exit(State);
	    _ ->
		%% Not our blocker, so ignore
		State
	end,
    {noreply, S1};

handle_info({'EXIT', Pid, normal}, #state{listener = Pid} = State) ->
    %% The only reason why the current listener dies normally is when the 
    %% listen socket has closed. This (normally) happens when the application
    %% is stoppen (closed by the manager). But since we get it here we are not
    %% stopping, and therefor we first must create a new listenen socket and 
    %% the create a new listener.
    ?vinfo("~n   Normal exit message from current listener ~p",[Pid]),
    report_crasher(Pid, "current listener", normal, State),
    #state{config_db = Db, socket_type = SocketType, socket = Sock} = State,
    SocketType = httpd_socket:config(Db),
    Port       = httpd_util:lookup(Db, port),
    Addr       = httpd_util:lookup(Db, bind_address),
    case httpd_socket:listen(SocketType, Addr, Port) of
	{error, eaddrinuse} ->
	    ?vlog("listen already exist, create new listener", []),
	    Listener = create_listener(State),
	    ?vdebug("new listener: ~p",[Pid]),
	    {noreply, State#state{listener = Listener}};
	{error, Reason} ->
	    ?vinfo("failed socket listen operation: ~p", [Reason]),
	    String = 
		lists:flatten(
		  io_lib:format("failed creating new listen socket: ~p", 
				[Reason])),
	    report_error(State, String),
	    {stop, {error, {listen, Reason}}};
	ListenSocket ->
	    ?vlog("new listen socket: ~p", [ListenSocket]),
	    State1 = State#state{socket = ListenSocket},
	    Listener = create_listener(State1),
	    ?vdebug("new listener: ~p",[Listener]),
	    {noreply, State1#state{listener = Listener}}
    end;

handle_info({'EXIT', Pid, normal}, State) ->
    ?vdebug("~n   Normal exit message from ~p",[Pid]),
    {noreply, State};

handle_info({'EXIT', Pid, {accept_failed, Err}},State) ->
    %% Accept failed. Start a new connection process.
    ?ERROR("handle_info -> accept failed:"
	   "~n    Listener: ~p"
	   "~n    Pid:      ~p"
	   "~n    Err:      ~p", 
	   [State#state.listener,Pid,Err]),
    ?vlog("~n   Accept failed exit message from ~p"
	  "~n   with reason ~p",[Pid,Err]),
    L = create_listener(State),
    ?vdebug("New listener: ~p",[L]),
    {noreply, State#state{listener = L}};

handle_info({'EXIT', Pid, blocked}, S) ->
    ?vdebug("handle_info -> blocked exit signal from connection handler (~p)",
	    [Pid]),
    {noreply, S};

handle_info({'EXIT', Pid, Reason}, State) ->
    ?ERROR("handle_info -> exit signal:"
	   "~n    Listener: ~p"
	   "~n    Pid:      ~p"
	   "~n    Reason:   ~p",
	   [State#state.listener,Pid,Reason]),
    ?vlog("~n   Exit message from ~p for reason ~p",[Pid,Reason]),
    L = check_crasher(Pid,State,Reason),
    {noreply, State#state{listener = L}};

handle_info(Info, State) ->
    ?ERROR("handle_info -> Info: ~p", [Info]),
    ?vinfo("~n   unknown info '~p'",[Info]),
    String = 
	lists:flatten(
	  io_lib:format("Unknown info to manager (~p) when ~n" ++
			"\tListener: ~p~n" ++
			"\tSocket: ~p~n" ++
			"\t=> ~p",
			[self(),State#state.listener,
			 State#state.socket,Info])),
    report_error(State,String),
    {noreply, State}.

%% terminate

terminate(Reason, #state{config_db   = Db, 
			 socket_type = Type, 
			 socket      = Socket}) -> 
    ?vlog("~n   Terminating for reason: ~p",[Reason]),
    Res = httpd_socket:close(Type, Socket),
    ?vtrace("terminate -> socket close result: ~p", [Res]),
    httpd_conf:remove_all(Db),
    ok.


%% code_change({down,ToVsn}, State, Extra)
%% 
%% NOTE:
%% Actually upgrade from 2.5.1 to 2.5.3 and downgrade from 
%% 2.5.3 to 2.5.1 is done with an application restart, so 
%% these function is actually never used. The reason for keeping
%% this stuff is only for future use.
%%
code_change({down,ToVsn},State,Extra) ->
    {ok,State};

%% code_change(FromVsn, State, Extra)
%%
code_change(FromVsn,State,Extra) ->
    {ok,State}.



%% Check and (if needed) Create new listener, in any case report
%% the event
check_crasher(Crasher, #state{listener = Listener} = State, Reason) ->
    AuthServer = get(auth_server),      % This is only tmp
    SecServer  = get(security_server),  % This is only tmp
    check_crasher(Crasher, Listener, AuthServer, SecServer, State, Reason).

check_crasher(Listener, Listener, _AuthServer, _SecServer, State, Reason) ->
    ?vtrace("check_crasher -> entry when listener crashed",[]),
    report_crasher(Listener, "Listener", Reason, State),
    ?vtrace("check_and_create -> create new listener",[]),
    create_listener(State);
check_crasher(AuthServer, Listener, AuthServer, _SecServer, State, Reason) ->
    ?vtrace("check_and_create -> entry with auth server crashed:"
	    "~n   AuthServer: ~p"
	    "~n   Reason:     ~p",[AuthServer, Reason]),
    report_crasher(AuthServer, "Auth server", Reason, State),
    Listener;
check_crasher(SecServer, Listener, _AuthServer, SecServer, State, Reason) ->
    ?vtrace("check_and_create -> entry with security server crashed:"
	    "~n   SecServer: ~p"
	    "~n   Reason:    ~p",[SecServer, Reason]),
    report_crasher(SecServer, "Security server", Reason, State),
    Listener;
check_crasher(Crasher, Listener, _AuthServer, _SecServer, State, Reason) ->
    report_crasher(Crasher, "Unknown", Reason, State),
    Listener.


report_crasher(Crasher, Desc, Reason, State) ->
    String = 
	lists:flatten(
	  io_lib:format("~s process (~p) crashed ~n\t=> ~p",
			[Desc, Crasher, Reason])),
    report_error(State, String).


%% create_listener

create_listener(State) ->
    create_listener(State,get_astate(State)).

create_listener(State,AdminState) ->
    UsageState = get_ustate(State),
    ?vtrace("Create listener when "
	    "~n   connection count = ~p"
	    "~n   admin state      = ~p"
	    "~n   usage state      = ~p",
	    [length(State#state.connections),AdminState,UsageState]),
    ConfigDB      = State#state.config_db,
    SocketType    = State#state.socket_type,
    ListenSocket  = State#state.socket,
    create_listener(AdminState,UsageState,SocketType,ListenSocket,ConfigDB).

create_listener(AdminState,UsageState,SocketType,ListenSocket,ConfigDB) ->
    ?vtrace("create_listener/5 -> entry",[]),
    httpd_listener:start_link(AdminState,UsageState,
			      SocketType,ListenSocket,ConfigDB).


handle_block(S) ->
    handle_block(S,S#state.admin_state).

handle_block(S,unblocked) ->
    %% Kill all connections
    ?vtrace("handle_block -> kill all connection handlers",[]),
    [exit(Pid,blocked) || Pid <- S#state.connections],
    {ok,S#state{connections = [], admin_state = blocked}};
handle_block(S,blocked) ->
    {ok,S};
handle_block(S,shutting_down) ->
    {{error,shutting_down},S}.
    
handle_block(S,Timeout,From,Ref) when Timeout >= 0 ->
    do_block(S,Timeout,From,Ref);

handle_block(S,Timeout,From,Ref) ->
    Reply = {error,{invalid_block_request,Timeout}},
    From ! {block_reply,Reply,Ref},
    S.

do_block(S,Timeout,From,Ref) ->
    case S#state.connections of
	[] ->
	    %% Already in idle usage state => go directly to blocked
	    ?vdebug("do_block -> already in idle usage state",[]),
	    From ! {block_reply,ok,Ref},
	    S#state{admin_state = blocked};
	_ ->
	    %% Active or Busy usage state => go to shutting_down
	    ?vdebug("do_block -> active or busy usage state",[]),
	    %% Make sure we get to know if blocker dies...
	    ?vtrace("do_block -> create blocker monitor",[]),
	    MonitorRef = monitor_blocker(From),
	    ?vtrace("do_block -> (possibly) start block timer",[]),
	    Tmr = {start_block_tmr(Timeout,disturbing),From,Ref},
	    S#state{admin_state = shutting_down, 
		    blocker_ref = MonitorRef, blocking_tmr = Tmr}
    end.

handle_nd_block(S,infinity,From,Ref) ->
    do_nd_block(S,infinity,From,Ref);

handle_nd_block(S,Timeout,From,Ref) when Timeout >= 0 ->
    do_nd_block(S,Timeout,From,Ref);

handle_nd_block(S,Timeout,From,Ref) ->
    Reply = {error,{invalid_block_request,Timeout}},
    From ! {block_reply,Reply,Ref},
    S.

do_nd_block(S,Timeout,From,Ref) ->
    case S#state.connections of
	[] ->
	    %% Already in idle usage state => go directly to blocked
	    ?vdebug("do_nd_block -> already in idle usage state",[]),
	    From ! {block_reply,ok,Ref},
	    S#state{admin_state = blocked};
	_ ->
	    %% Active or Busy usage state => go to shutting_down
	    ?vdebug("do_nd_block -> active or busy usage state",[]),
	    %% Make sure we get to know if blocker dies...
	    ?vtrace("do_nd_block -> create blocker monitor",[]),
	    MonitorRef = monitor_blocker(From),
	    ?vtrace("do_nd_block -> (possibly) start block timer",[]),
	    Tmr = {start_block_tmr(Timeout,non_disturbing),From,Ref},
	    S#state{admin_state = shutting_down, 
		    blocker_ref = MonitorRef, blocking_tmr = Tmr}
    end.

handle_block_timeout(S,Method) ->
    %% Time to take this to the road...
    demonitor_blocker(S#state.blocker_ref),
    handle_block_timeout1(S,Method,S#state.blocking_tmr).

handle_block_timeout1(S,non_disturbing,{_,From,Ref}) ->
    ?vdebug("handle_block_timeout1(non-disturbing) -> send reply: timeout",[]),
    From ! {block_reply,{error,timeout},Ref},
    S#state{admin_state = unblocked, 
	    blocker_ref = undefined, blocking_tmr = undefined};

handle_block_timeout1(S,disturbing,{_,From,Ref}) ->
    ?vdebug("handle_block_timeout1(disturbing) -> kill all connections",[]),
    [exit(Pid,blocked) || Pid <- S#state.connections],

    ?vdebug("handle_block_timeout1 -> send reply: ok",[]),
    From ! {block_reply,ok,Ref},
    S#state{admin_state = blocked,    connections = [], 
	    blocker_ref = undefined, blocking_tmr = undefined};

handle_block_timeout1(S,Method,{_,From,Ref}) ->
    ?vinfo("received block timeout with unknown block method:"
	   "~n   Method:  ~p",[Method]),
    From ! {block_reply,{error,{unknown_block_method,Method}},Ref},
    S#state{admin_state = blocked,    connections = [], 
	    blocker_ref = undefined, blocking_tmr = undefined};

handle_block_timeout1(S,Method,TmrInfo) ->
    ?vinfo("received block timeout with erroneous timer info:"
	   "~n   Method:  ~p"
	   "~n   TmrInfo: ~p",[Method,TmrInfo]),
    S#state{admin_state = unblocked,
	    blocker_ref = undefined, blocking_tmr = undefined}.

handle_unblock(S,FromA) ->
    handle_unblock(S,FromA,S#state.admin_state).

handle_unblock(S,_FromA,unblocked) ->
    {ok,S};
handle_unblock(S,FromA,_AdminState) ->
    ?vtrace("handle_unblock -> (possibly) stop block timer",[]),
    stop_block_tmr(S#state.blocking_tmr),
    case S#state.blocking_tmr of
	{Tmr,FromB,Ref} ->
	    %% Another process is trying to unblock
	    %% Inform the blocker
	    FromB ! {block_reply, {error,{unblocked,FromA}},Ref};
	_ ->
	    ok
    end,
    {ok,S#state{admin_state = unblocked, blocking_tmr = undefined}}.
    
%% The blocker died so we give up on the block.
handle_blocker_exit(S) ->
    {Tmr,_From,_Ref} = S#state.blocking_tmr,
    ?vtrace("handle_blocker_exit -> (possibly) stop block timer",[]),
    stop_block_tmr(Tmr),
    S#state{admin_state = unblocked,
	    blocker_ref = undefined, blocking_tmr = undefined}.
    


%% -------------------------------------------------------------------------
%% handle_restart
%%
%%
%%
%%
handle_restart(State) ->
    handle_restart(State, State#state.config_file).

handle_restart(#state{config_db = Db} = State, ConfigFile) ->
    ?vtrace("load new configuration",[]),
    {ok, Config} = httpd_conf:load(ConfigFile),
    ?vtrace("check for illegal changes (addr, port and socket-type)",[]),
    case (catch check_constant_values(Db,Config)) of
	ok ->
	    %% If something goes wrong between the remove 
	    %% and the store where fu-ed
	    ?vtrace("remove old configuration, now hold you breath...",[]),
	    httpd_conf:remove_all(Db),
	    ?vtrace("store new configuration",[]),
	    case httpd_conf:store(Config) of
		{ok, NewConfigDB} ->
		    ?vtrace("restart done, puh!",[]),
		    {continue, ok, State#state{config_db = NewConfigDB}};
		Error ->
		    ?vlog("failed store new config: ~n   ~p",[Error]),
		    {stop, Error, State}
	    end;
	Error ->
	    ?vlog("restart NOT performed due to:"
		  "~n   ~p",[Error]),
	    {continue, Error, State}
    end.


check_constant_values(Db, Config) ->
    %% Check port number
    ?vtrace("check_constant_values -> check port number",[]),
    Port = httpd_util:lookup(Db, port),
    case httpd_util:key1search(Config, port) of  %% MUST be equal
	Port ->
	    ok;
	OtherPort ->
	    throw({error,{port_number_changed, Port, OtherPort}})
    end,

    %% Check bind address
    ?vtrace("check_constant_values -> check bind address",[]),
    Addr = httpd_util:lookup(Db, bind_address),
    case httpd_util:key1search(Config, bind_address) of  %% MUST be equal
	Addr ->
	    ok;
	OtherAddr ->
	    throw({error,{addr_changed, Addr, OtherAddr}})
    end,

    %% Check socket type
    ?vtrace("check_constant_values -> check socket type",[]),
    SockType = httpd_util:lookup(Db, com_type),
    case httpd_util:key1search(Config, com_type) of  %% MUST be equal
	SockType ->
	    ok;
	OtherSockType ->
	    throw({error,{sock_type_changed, SockType, OtherSockType}})
    end,
    ?vtrace("check_constant_values -> done",[]),
    ok.


%% get_ustate(State) -> idle | active | busy
%%
%% Retrieve the usage state of the HTTP server:
%%   0 active connection            -> idle
%%   max_clients active connections -> busy
%%   Otherwise                      -> active
%%
get_ustate(State) ->
    get_ustate(length(State#state.connections),State).

get_ustate(0,_State) ->
    idle;
get_ustate(ConnectionCnt,State) ->
    ConfigDB = State#state.config_db,
    case httpd_util:lookup(ConfigDB, max_clients, 150) of
	ConnectionCnt ->
	    busy;
	_ ->
	    active
    end.


get_astate(S) -> S#state.admin_state.


%% Timer handling functions
start_block_tmr(infinity,_) ->
    undefined;
start_block_tmr(T,M) ->
    erlang:send_after(T,self(),{block_timeout,M}).

stop_block_tmr(undefined) ->
    ok;
stop_block_tmr(Ref) ->
    erlang:cancel_timer(Ref).


%% Monitor blocker functions
monitor_blocker(Pid) when pid(Pid) ->
    case (catch erlang:monitor(process,Pid)) of
	MonitorRef ->
	    MonitorRef;
	{'EXIT',Reason} ->
	    undefined
    end;
monitor_blocker(_) ->
    undefined.

demonitor_blocker(undefined) ->
    ok;
demonitor_blocker(Ref) ->
    (catch erlang:demonitor(Ref)).


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


auth_status(P) when pid(P) ->
    Items = [status,message_queue_len,reductions,
	     heap_size,stack_size,current_function],
    {auth_status, process_status(P,Items,[])};
auth_status(_) ->
    {auth_status, undefined}.

sec_status(P) when pid(P) ->
    Items = [status,message_queue_len,reductions,
	     heap_size,stack_size,current_function],
    {security_status, process_status(P,Items,[])};
sec_status(_) ->
    {security_status, undefined}.

listener_status(P) ->
    Items = [status,message_queue_len,reductions,
	     heap_size,stack_size,current_function],
    {listener_status, process_status(P,Items,[])}.

manager_status(P) ->
    Items = [status,message_queue_len,reductions,
	     heap_size,stack_size],
    {manager_status, process_status(P,Items,[])}.


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


report_error(State,String) ->
    Cdb = State#state.config_db,
    error_logger:error_report(String),
    mod_log:report_error(Cdb,String),
    mod_disk_log:report_error(Cdb,String).
    

set_verbosity(V) ->
    Units = [manager_verbosity, listener_verbosity, 
	     security_verbosity, auth_verbosity],
    case httpd_util:key1search(V, all) of
	undefined ->
	    set_verbosity(V, Units);
	Verbosity when atom(Verbosity) ->
	    V1 = [{Unit, Verbosity} || Unit <- Units],
	    set_verbosity(V1, Units)
    end.

set_verbosity(_V, []) ->
    ok;
set_verbosity(V, [manager_verbosity = Unit|Units]) ->
    Verbosity = httpd_util:key1search(V, Unit, ?default_verbosity),
    put(verbosity, ?vvalidate(Verbosity)),
    set_verbosity(V, Units);
set_verbosity(V, [Unit|Units]) ->
    Verbosity = httpd_util:key1search(V, Unit, ?default_verbosity),
    put(Unit, ?vvalidate(Verbosity)),
    set_verbosity(V, Units).

    
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
set_verbosity(auth,V,S) ->
    put(auth_verbosity,V),
    Addr = httpd_util:lookup(S#state.config_db, bind_address),
    Port = httpd_util:lookup(S#state.config_db, port),
    mod_auth:verbosity(Addr,Port,V);


set_verbosity(all,V,S) ->
    OldMv = put(verbosity,V),
    OldLv = put(listener_verbosity,V),
    OldSv = put(security_verbosity,V),
    OldAv = put(auth_verbosity,V),
    Addr  = httpd_util:lookup(S#state.config_db, bind_address),
    Port  = httpd_util:lookup(S#state.config_db, port),
    mod_security:verbosity(Addr,Port,V),
    mod_auth:verbosity(Addr,Port,V),
    [{manager,OldMv}, {listener,OldLv}, {security,OldSv}, {auth, OldAv}].
    
    
%%
call(ServerRef,Request) ->
    gen_server:call(ServerRef,Request).

cast(ServerRef,Message) ->
    gen_server:cast(ServerRef,Message).

