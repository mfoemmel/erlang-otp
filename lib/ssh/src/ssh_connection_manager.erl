%%<copyright>
%% <year>2008-2008</year>
%% <holder>Ericsson AB, All Rights Reserved</holder>
%%</copyright>
%%<legalnotice>
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%%----------------------------------------------------------------------
%% Purpose: Handles multiplexing to ssh channels and global
%% connection requests e.i. the SSH Connection Protocol (RFC 4254), 
%% that provides interactive login sessions, remote execution of commands, 
%% forwarded TCP/IP connections, and forwarded X11 connections. Details of the
%% protocol is implemented in ssh_connection.erl
%% ----------------------------------------------------------------------
-module(ssh_connection_manager).

-behaviour(gen_server).

-include("ssh.hrl").
-include("ssh_connect.hrl").

-export([start_link/1]).

-export([info/1, info/2, attach/2, attach/3, detach/2, renegotiate/1,
	 peer_addr/1, send_window/3, recv_window/3, adjust_window/3,
	 close/2, stop/1, send/5,
	 send_eof/2, controlling_process/4]).

-export([open_channel/6, request/6, global_request/4, event/2,
	 cast/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(DBG_MESSAGE, true).

-record(state,
	{
	  role,
	  client,
	  starter,
	  connection,       % pid()
	  connection_state, % #connection{} 
	  latest_channel_id = 0, 
	  opts,
	  channel_args,
	  connected 
	 }).

%%====================================================================
%% API
%%====================================================================

start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

open_channel(ConnectionManager,	ChannelType, ChannelSpecificData,
	     InitialWindowSize, MaxPacketSize, Timeout) ->
    case (catch call(ConnectionManager, {open, self(), ChannelType,
					 InitialWindowSize, 
					 MaxPacketSize, ChannelSpecificData},
	      Timeout)) of
	{open, Channel} -> 
	    {ok, Channel}; 
	Error -> 
	    %% TODO: Best way?
	    Error
    end.

request(ConnectionManager, Channel, Type, true, Data, Timeout) ->
    call(ConnectionManager, {request, Channel, Type, Data}, Timeout);
request(ConnectionManager, Channel, Type, false, Data, _) ->
    cast(ConnectionManager, {request, Channel, Type, Data}).

global_request(ConnectionManager, Type, true = Reply, Data) ->
    case call(ConnectionManager, 
	      {global_request, self(), Type, Reply, Data}) of
	{ssh_cm, ConnectionManager, {success, _}} ->
	    ok;
	{ssh_cm, ConnectionManager, {failure, _}} ->
	    error
    end;

global_request(ConnectionManager, Type, false = Reply, Data) ->
    cast(ConnectionManager, {global_request, self(), Type, Reply, Data}).
 
event(ConnectionManager, BinMsg) -> 
    cast(ConnectionManager, {ssh_msg, self(), BinMsg}).

info(ConnectionManager) ->
    info(ConnectionManager, {info, all}).

info(ConnectionManager, ChannelProcess) ->
    call(ConnectionManager, {info, ChannelProcess}).

attach(ConnectionManager, TimeOut) ->
    call(ConnectionManager, {attach, self()}, TimeOut).

attach(ConnectionManager, ChannelPid, TimeOut) ->
    call(ConnectionManager, {attach, ChannelPid}, TimeOut).

detach(ConnectionManager, TimeOut) ->
    call(ConnectionManager, {detach, self()}, TimeOut).

renegotiate(ConnectionManager) ->
    cast(ConnectionManager, renegotiate).

peer_addr(ConnectionManager) ->
    call(ConnectionManager, {peer_addr, self()}).

send_window(ConnectionManager, Channel, TimeOut) ->
    call(ConnectionManager, {send_window, Channel}, TimeOut).

recv_window(ConnectionManager, Channel, TimeOut) ->
    call(ConnectionManager, {recv_window, Channel}, TimeOut).

adjust_window(ConnectionManager, Channel, Bytes) ->
    cast(ConnectionManager, {adjust_window, Channel, Bytes}).

close(ConnectionManager, ChannelId) ->
    cast(ConnectionManager, {close, ChannelId}).

stop(ConnectionManager) ->
    Opts = call(ConnectionManager, opts),
    Address =  proplists:get_value(address, Opts),
    Port = proplists:get_value(port, Opts),
    stop(Address, Port, ConnectionManager),
    ok.

send(ConnectionManager, ChannelId, Type, Data, Timeout) ->
    call(ConnectionManager, {data, ChannelId, Type, Data}, Timeout).

send_eof(ConnectionManager, ChannelId) ->
    cast(ConnectionManager, {eof, ChannelId}).

controlling_process(ConnectionManager, ChannelId, NewPid, OldPid) -> 
    call(ConnectionManager, 
	 {controlling_process, ChannelId, NewPid, OldPid}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([server, _Socket, Opts]) ->  
    process_flag(trap_exit, true),
    ssh_bits:install_messages(ssh_connection:messages()),
    Cache = ssh_channel:cache_create(), 
    {ok, #state{role = server, 
		connection_state = #connection{channel_cache = Cache,
					       channel_id_seed = 0,
					       port_bindings = [],
					       requests = [],
					       channel_pids = []},
		opts = Opts,
		connected = false}};

init([client, Opts]) ->  
    process_flag(trap_exit, true),
    {links, [Parent]} = process_info(self(), links),
    ssh_bits:install_messages(ssh_connection:messages()),
    Cache = ssh_channel:cache_create(),
    Address =  proplists:get_value(address, Opts),
    Port = proplists:get_value(port, Opts),
    SocketOpts = proplists:get_value(socket_opts, Opts),
    Options = proplists:get_value(ssh_opts, Opts),
    ChannelPid = proplists:get_value(channel_pid, Opts),
    self() ! 
	{start_connection, client, [Parent, Address, Port, 
				    ChannelPid, SocketOpts, Options]},
    {ok, #state{role = client, 
		client = ChannelPid,
		connection_state = #connection{channel_cache = Cache,
					       channel_id_seed = 0,
					       port_bindings = [],
					       requests = [],
					       channel_pids = []},
		opts = Opts,
		connected = false}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({request, ChannelId, Type, Data}, From, State0) ->
    {{replies, Replies}, State} = handle_request(ChannelId, Type, Data, 
						 true, From, State0),
    %% Sends message to the connection handler process, reply to
    %% channel is sent later when reply arrives from the connection
    %% handler.
    lists:foreach(fun send_msg/1, Replies),
    {noreply, State};

handle_call({global_request, Pid, _, _, _} = Request, From, 
	    #state{connection_state = 
		   #connection{channel_cache = Cache}} = State0) ->
    State1 = handle_global_request(Request, State0),
    Channel = ssh_channel:cache_find(Pid, Cache),
    State = add_request(true, Channel#channel.local_id, From, State1),
    {noreply, State};

handle_call({data, ChannelId, Type, Data}, From, 
	    #state{connection_state = Connection0,
		   connection = ConnectionPid} = State) ->
    case ssh_connection:channel_data(ChannelId, Type, Data, Connection0,
				     ConnectionPid, From) of
	{{replies, Replies}, Connection} ->
	    lists:foreach(fun send_msg/1, Replies),
	    {noreply, State#state{connection_state = Connection}};
	{noreply, Connection} ->
	    {noreply, State#state{connection_state = Connection}}
    end;
handle_call({info, ChannelPid}, _From, 
	    #state{connection_state = 
		   #connection{channel_cache = Cache}} = State) ->
    Result = ssh_channel:cache_foldl(
	       fun(Channel, Acc) when ChannelPid == all; 
		  Channel#channel.user == ChannelPid ->
		       [Channel | Acc];
		  (_, Acc) ->
		       Acc
	       end, [], Cache),
    {reply, {ok, Result}, State};

handle_call({open, ChannelPid, Type, InitialWindowSize, MaxPacketSize, Data}, 
	    From, #state{connection = Pid,
			 connection_state = 
			 #connection{channel_cache = Cache}} = State0) ->
    try is_attched_channel(ChannelPid, State0) of
	false -> 
	    {reply, {error, einval}, State0};
	true ->
	    {ChannelId, State1}  = new_channel_id(State0),
	    Msg = ssh_connection:channel_open_msg(Type, ChannelId, 
						  InitialWindowSize, 
						  MaxPacketSize, Data),
	    send_msg({connection_reply, Pid, Msg}),
	    Channel = #channel{type = Type,
			       sys = "none",
			       user = ChannelPid,
			       local_id = ChannelId,
			       recv_window_size = InitialWindowSize,
			       recv_packet_size = MaxPacketSize},
	    ssh_channel:cache_update(Cache, Channel),
	    State = add_request(true, ChannelId, From, 
				State1),
	    {noreply, State}
    catch
	error:Reason ->
	    exit({session_open, Reason})
    
    end;

handle_call({attach, ChannelPid}, _From, State) ->
    {reply, ok, add_channel_ref(ChannelPid, State)};

handle_call({detach, ChannelPid}, _From, State) ->
    {reply, ok, del_channel_ref(ChannelPid, State)};

handle_call({send_window, ChannelId}, _From, 
	    #state{connection_state = 
		   #connection{channel_cache = Cache}} = State) ->
     Reply = case ssh_channel:cache_lookup(Cache, ChannelId) of
		#channel{send_window_size = WinSize,
			 send_packet_size = Packsize} ->
		    {ok, {WinSize, Packsize}};
		undefined -> 
		    {error, einval}
	    end,
    {reply, Reply, State};

handle_call({recv_window, ChannelId}, _From, 
	    #state{connection_state = #connection{channel_cache = Cache}} 
	     = State) ->
  
    Reply = case ssh_channel:cache_lookup(Cache, ChannelId) of
		#channel{recv_window_size = WinSize,
			 recv_packet_size = Packsize} ->
		    {ok, {WinSize, Packsize}};
		undefined -> 
		    {error, einval}
	    end,
    {reply, Reply, State};

handle_call(peer_addr, _From, #state{connection = Pid} = State) ->
    Reply = ssh_connection_handler:peer_address(Pid),
    {reply, Reply, State};

handle_call(opts, _, #state{opts = Opts} = State) ->
    {reply, Opts, State};

handle_call({controlling_process, ChannelId, NewPid, OldPid}, _,  
	    #state{connection_state = 
		   #connection{channel_cache = Cache}} = State0) ->
    
    State1 = del_channel_ref(OldPid, State0),
    State = add_channel_ref(NewPid, State1),
    Channel = ssh_channel:cache_lookup(Cache, ChannelId),
    ChannelDataQueue = Channel#channel.subsys_queue,
    lists:foreach(fun(Data) -> 
			  send_msg({channel_data, NewPid, Data})
		  end,
		  queue:to_list(ChannelDataQueue)),
    ssh_channel:cache_update(Cache, Channel#channel{user = NewPid,
						    passive_subsys = false,
						    subsys_queue = queue:new()}),
    {reply, ok, State};

%%TODO: !
handle_call(_, _From, State) ->
    {reply, ok, State}.

stop(_, _, Pid) ->
    exit(Pid, normal).

stop(Role, Address, Port, Pid) ->
    case Role of
	client ->
	    exit(Pid, normal);
	server ->
	    ssh_system_sup:restart_subsystem(Address, Port)
    end.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

%% Message from ssh_connection_handler
handle_cast({ssh_msg, Pid, Msg}, #state{%%connection = Pid,
					   connection_state = Connection0,
	      role = Role, opts = Opts, connected = IsConnected,
	      client = ClientPid}
	    = State) ->
    ConnectionMsg = decode_ssh_msg(Msg),
    case ssh_connection:handle_msg(ConnectionMsg, Connection0, Pid, Role) of
	{{replies, Replies}, Connection} ->
	    lists:foreach(fun send_msg/1, Replies),
	    {noreply, State#state{connection_state = Connection}};
	{noreply, Connection} ->
	    {noreply, State#state{connection_state = Connection}};
	{disconnect, {_, Reason}, {{replies, Replies}, Connection}} 
	when Role == client andalso (not IsConnected) ->
	    lists:foreach(fun send_msg/1, Replies),
	    ClientPid ! {self(), not_connected, Reason},
	    {stop, normal, State#state{connection = Connection}};
	{disconnect, Reason, {{replies, Replies}, Connection}} ->
	    lists:foreach(fun send_msg/1, Replies),
	    SSHOpts = proplists:get_value(ssh_opts, Opts),
	    disconnect_fun(Reason, SSHOpts),
	    Address =  proplists:get_value(address, Opts),
	    Port = proplists:get_value(port, Opts),
	    stop(Role, Address, Port, self()),
	    {noreply, State#state{connection_state = Connection}}
    end;


handle_cast({request, ChannelId, Type, Data}, State0) ->
    {{replies, Replies}, State} = handle_request(ChannelId, Type, Data, 
						 false, none, State0),
    lists:foreach(fun send_msg/1, Replies),
    {noreply, State};

handle_cast({global_request, _, _, _, _} = Request, State0) ->
    State = handle_global_request(Request, State0),
    {noreply, State};

handle_cast(renegotiate, #state{connection = Pid} = State) ->
    ssh_connection_handler:renegotiate(Pid),
    {noreply, State};

handle_cast({adjust_window, ChannelId, Bytes}, 
	    #state{connection = Pid, connection_state =
		   #connection{channel_cache = Cache}} = State) ->
    case ssh_channel:cache_lookup(Cache, ChannelId) of
	#channel{recv_window_size = WinSize, remote_id = Id} = Channel ->
	    ssh_channel:cache_update(Cache, Channel#channel{recv_window_size = 
					       WinSize + Bytes}),
	    Msg = ssh_connection:channel_adjust_window_msg(Id, Bytes),
	    send_msg({connection_reply, Pid, Msg});
	undefined -> 
	    ignore
    end,
    {noreply, State};

handle_cast({close, ChannelId}, 
	    #state{connection = Pid, connection_state = 
		   #connection{channel_cache = Cache}} = State0) ->
    case ssh_channel:cache_lookup(Cache, ChannelId) of			 
	#channel{remote_id = Id, user = ChannelPid} ->
	    send_msg({connection_reply, Pid, 
		      ssh_connection:channel_close_msg(Id)}),
	    State = del_channel_ref(ChannelPid, State0),
	    {noreply, State};
	undefined -> 
	    {noreply, State0}
    end;

handle_cast({success, ChannelId},  #state{connection = Pid} = State) ->
    Msg = ssh_connection:channel_success_msg(ChannelId),
    send_msg({connection_reply, Pid, Msg}),
    {noreply, State};

handle_cast({failure, ChannelId},  #state{connection = Pid} = State) ->
    Msg = ssh_connection:channel_failure_msg(ChannelId),
    send_msg({connection_reply, Pid, Msg}),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({start_connection, server, 
	     [Address, Port, Socket, Options]}, State) ->
    
    {ok, Connection} = ssh_transport:accept(Address, Port, Socket, Options),
    Shell = proplists:get_value(shell, Options),
    %% Wait for 'ssh_connected' message from the connection
    %% handling process then use channel_arg to start the
    %% channel
    {noreply, State#state{connection = Connection, 
			  channel_args = {Address, Port, 
					  Shell, Options}}};
	    
handle_info({start_connection, client, 
	     [Parent, Address, Port, ChannelPid, SocketOpts, Options]},
	    #state{client = Pid} = State0) ->
    case (catch ssh_transport:connect(Parent, Address, 
				      Port, SocketOpts, Options)) of
	{ok, Connection} ->
	    State = add_channel_ref(ChannelPid, State0),
	    {noreply, State#state{connection = Connection}};
	Reason ->
	    Pid ! {self(), not_connected, Reason},
	    {stop, normal, State0}	
    end;

handle_info({ssh_cm, Sender, Msg}, State) ->
    %% Backwards compatibility!
    NewState = case is_attched_channel(Sender, State) of
		   false ->
		       State;
		   true ->
		       cm_message(Msg, State)
	       end,
    {noreply, NewState};

%% Nop backwards compatibility
handle_info({same_user, _}, State) ->
    {noreply, State};

handle_info(ssh_connected, #state{role = client, client = Pid} 
	    = State) ->
    Pid ! {self(), is_connected},
    {noreply, State#state{connected = true}};

handle_info(ssh_connected, 
	    #state{role = server, channel_args = {Address, Port, Shell, Options}} 
	    = State0) ->
    ChannelSpec = ssh_cli:child_spec(Shell, Address, Port, Options),
    ChannelPid = start_channel(Address, Port, ChannelSpec),
    State = add_channel_ref(ChannelPid, State0),
    {noreply, State#state{connected = true}};

handle_info({'DOWN', _Ref, process, ChannelPid, normal}, State0) ->
    {{replies, Replies}, State} = handle_channel_down(ChannelPid, State0),
    lists:foreach(fun send_msg/1, Replies),
    {noreply, State};

handle_info({'DOWN', _Ref, process, ChannelPid, shutdown}, State0) ->
    {{replies, Replies}, State} = handle_channel_down(ChannelPid, State0),
    lists:foreach(fun send_msg/1, Replies),
    {noreply, State};

handle_info({'DOWN', _Ref, process, ChannelPid, Reason}, State0) ->
    Report = io_lib:format("Pid ~p DOWN ~p\n", [ChannelPid, Reason]),
    error_logger:error_report(Report),
    {{replies, Replies}, State} = handle_channel_down(ChannelPid, State0),
    lists:foreach(fun send_msg/1, Replies),
    {noreply, State};

handle_info({'EXIT', _, _}, State) ->
    %% Handled in 'DOWN'
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(Reason, #state{connection_state =  
			  #connection{requests = Requests},
			 opts = Opts}) ->
    SSHOpts = proplists:get_value(ssh_opts, Opts),
    disconnect_fun(Reason, SSHOpts),
    lists:foreach(fun({_, From}) -> 
			  gen_server:reply(From, {error, connection_closed})
		  end, Requests).

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
call(Pid, Msg) ->
    call(Pid, Msg, infinity).
call(Pid, Msg, Timeout) ->
    try gen_server:call(Pid, Msg, Timeout) of
	Result ->
	    Result
    catch
	exit:{timeout, _} ->
	    {error, timeout}
    end.

cast(Pid, Msg) ->
    gen_server:cast(Pid, Msg).

decode_ssh_msg(BinMsg) when is_binary(BinMsg)->
    Msg = ssh_bits:decode(BinMsg),
    ?dbg(?DBG_MESSAGE, "RECV_MSG: ~p\n", [Msg]),
    Msg;
decode_ssh_msg(Msg) ->
    Msg.

add_channel_ref(ChannelPid, #state{connection_state = 
			       #connection{channel_pids = ChannelPids} 
			       = Connection0} = State0) ->
    case lists:keymember(ChannelPid, 1, ChannelPids) of
	false ->
	    Ref = erlang:monitor(process, ChannelPid),
	    Connection = 
		Connection0#connection{channel_pids = [{ChannelPid, Ref} 
						       | ChannelPids]},
	    State0#state{connection_state = Connection};
	true ->
	    State0
    end.

del_channel_ref(ChannelPid, #state{connection_state =
			       #connection{channel_pids = ChannelPids} = 
			       Connection0} = State0) ->
    case lists:keysearch(ChannelPid, 1, ChannelPids) of
	false ->
	    State0;
	{value, {ChannelPid, Ref}} ->
	    erlang:demonitor(Ref),
	    Connection1 = 
		ssh_connection:unbind_channel(ChannelPid, Connection0),
	    Connection = 
		Connection1#connection{channel_pids =  
				       lists:keydelete(ChannelPid, 
						       1, ChannelPids)},
	    State0#state{connection_state = Connection}	    
    end.

is_attched_channel(ChannelPid, 
		   #state{connection_state = 
			  #connection{channel_pids = ChannelPids}}) ->
    lists:keymember(ChannelPid, 1, ChannelPids).

send_msg({channel_data, Pid, Data}) ->
    Pid ! {ssh_cm, self(), Data};
send_msg({channel_requst_reply, From, Data}) ->
    gen_server:reply(From, Data);
send_msg({connection_reply, Pid, Data}) ->
    ?dbg(?DBG_MESSAGE, "SEND_MSG: ~70p\n", [Data]),
    Msg = ssh_bits:encode(Data),
    ssh_connection_handler:send(Pid, Msg);
send_msg({flow_control, Cache, Channel, From, Msg}) ->
    ssh_channel:cache_update(Cache, Channel#channel{flow_control = undefined}),
    gen_server:reply(From, Msg).

handle_request(ChannelId, Type, Data, WantReply, From, 
	       #state{connection = Pid,
		      connection_state = 
		      #connection{channel_cache = Cache}} = State0) ->
    case ssh_channel:cache_lookup(Cache, ChannelId) of
	#channel{local_id = Id} = Channel ->
	    update_sys(Cache, Channel, Type),
	    Msg = ssh_connection:channel_request_msg(ChannelId, Type, 
						     WantReply, Data),
	    Replies = [{connection_reply, Pid, Msg}],
	    State = add_request(WantReply, Id, From, State0),
	    {{replies, Replies}, State};
	undefined ->
	    {noreply, State0}
    end.

handle_channel_down(ChannelPid, #state{connection = Pid,
				       connection_state = 
				       #connection{channel_cache = Cache}} = 
		    State0) ->
    case is_attched_channel(ChannelPid, State0) of
	true ->
	    Replies = 
		ssh_channel:cache_foldl(
		  fun(Channel, Acc) when Channel#channel.user == ChannelPid ->
			  ssh_channel:cache_delete(Cache, 
						   Channel#channel.local_id),
			  case Channel#channel.remote_id of
			      undefined ->
				  Acc;
			      _ ->
				  [{connection_reply, Pid,
				    ssh_connection:channel_close_msg(
				      Channel#channel.remote_id)} | Acc]
			  end;
		     (_, Acc) ->
			  Acc
		  end, [], Cache),
	    State = del_channel_ref(ChannelPid, State0),
	    {{replies, Replies}, State};
	false ->
	    {{replies, []}, State0}
    end.

update_sys(Cache, Channel, Type) ->
    case Type of
	"subsystem" ->
	    ssh_channel:cache_update(Cache, 
				     Channel#channel{sys = "subsystem"});
	"exec" -> 
	    ssh_channel:cache_update(Cache, 
				     Channel#channel{sys = "exec"});
	"shell" -> 
	    ssh_channel:cache_update(Cache, 
				     Channel#channel{sys = "shell"});
	_ ->
	    ok
    end.

add_request(false, _Channel, _From, State) ->
    State;
add_request(true, ChannelId, From, #state{connection_state = 
					#connection{requests = Requests0} = 
					Connection} = State) ->
    Requests = [{ChannelId, From} | Requests0],
    State#state{connection_state = Connection#connection{requests = Requests}}.
 
new_channel_id(#state{connection_state = #connection{channel_id_seed = Id} =
		      Connection}
	       = State) ->
    {Id, State#state{connection_state = 
		     Connection#connection{channel_id_seed = Id + 1}}}.
   
handle_global_request({global_request, ChannelPid, 
		       "tcpip-forward" = Type, WantReply, 
		       <<?UINT32(IPLen), 
			IP:IPLen/binary, ?UINT32(Port)>> = Data}, 
		      #state{connection = ConnectionPid, 
			     connection_state = 
			     #connection{channel_cache = Cache}
			     = Connection0} = State0) ->
    
    %% auto attach channel process
    State = add_channel_ref(ChannelPid, State0),  
    %% TODO:
    ssh_channel:cache_update(Cache, #channel{user = ChannelPid,
					     type = "forwarded-tcpip",
 					     sys = none}),
    Connection = ssh_connection:bind(IP, Port, ChannelPid, Connection0),
    Msg = ssh_connection:global_request_msg(Type, WantReply, Data),
    send_msg({connection_reply, ConnectionPid, Msg}),
    State#state{connection_state = Connection};

handle_global_request({global_request, _Pid, "cancel-tcpip-forward" = Type, 
		       WantReply, <<?UINT32(IPLen), 
				   IP:IPLen/binary, ?UINT32(Port)>> = Data}, 
		      #state{connection = Pid,
			     connection_state = Connection0} = State) ->
    %% OBS! Can not auto erase a channel process, it may have more tasks
    Connection = ssh_connection:unbind(IP, Port, Connection0),
    Msg = ssh_connection:global_request_msg(Type, WantReply, Data),
    send_msg({connection_reply, Pid, Msg}),
    State#state{connection_state = Connection};

handle_global_request({global_request, _Pid, "cancel-tcpip-forward" = Type, 
		       WantReply,  Data}, #state{connection = Pid} = State) ->
    Msg = ssh_connection:global_request_msg(Type, WantReply, Data),
    send_msg({connection_reply, Pid, Msg}),
    State.

start_channel(_, _, Fun) when is_function(Fun) ->
    Fun();
start_channel(Address, Port, ChildSpec) when is_tuple(ChildSpec)->
    SystemSup = ssh_system_sup:system_supervisor(Address, Port),
    ChannelSup = ssh_system_sup:channel_supervisor(SystemSup),
    {ok, Pid} = ssh_channel_sup:start_child(ChannelSup, ChildSpec),
    Pid.
    
cm_message(Msg, State) ->
    {noreply, NewState} =  handle_cast(Msg, State),
    NewState.

disconnect_fun(Reason, Opts) ->
    case proplists:get_value(disconnectfun, Opts) of
 	undefined ->
 	    ok;
 	Fun ->
 	    catch Fun(Reason)
     end.
