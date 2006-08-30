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

%%% Description : SSH connection protocol manager

-module(ssh_cm).

-include("ssh.hrl").
-include("ssh_connect.hrl").

-define(DEFAULT_PACKET_SIZE, 32768).
-define(DEFAULT_WINDOW_SIZE, 2*?DEFAULT_PACKET_SIZE).
-define(DEFAULT_TIMEOUT, 5000).

-behaviour(gen_server).

-import(lists, [reverse/1, foreach/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-export([connect/1, connect/2, connect/3]).
-export([listen/2, listen/3, listen/4, stop_listener/1]).
-export([stop/1]).
%%-export([dist_start/1, dist_start/2]).
-export([encode_ip/1]).

%% API
-export([adjust_window/3, attach/2, detach/2,
	 tcpip_forward/3, cancel_tcpip_forward/3, direct_tcpip/6,
	 direct_tcpip/8, 
	 close/2,
	 shell/2, exec/4, i/1, i/2, info/1, info/2, 
	 recv_window/3, send/3, send/4, renegotiate/1, renegotiate/2,
	 request_success/2, send_ack/3, send_ack/4, send_ack/5, send_eof/2,
	 send_window/3, session_open/2, session_open/4, subsystem/4,
	 open_pty/3, open_pty/7, open_pty/9,
	 set_user_ack/4, set_user/4,
	 setenv/5, signal/3, winch/4,
	 get_authhandle/1,
	 get_peer_addr/1]).

%% Special for ssh_userauth (and similar)
%%-export([set_ssh_msg_handler/2, reset_ssh_msg_handler/1]).

%% internal exports
%% -export([listen_init/7, connect_init/6]).

-define(DBG_SSHMSG, true).
-define(DBG_SSHCM,  true).
-define(DBG_USER,   true).

-record(channel,
	{
	  type,          %% "session", "x11", "forwarded-tcpip", "direct-tcpip"
	  sys,           %% "none", "shell", "exec" "subsystem"
	  user,          %% "user" process id (default to cm user)
	  user_ack = false,   %% user want ack packet when data is sent

	  local_id,           %% local channel id

	  recv_window_size,
	  recv_packet_size,
	  %%recv_eof = false,
	  recv_close = false,

	  remote_id,          %% remote channel id
	  send_window_size,
	  send_packet_size,
	  %%sent_eof = false,
	  sent_close = false,
	  send_buf = []
	 }).

-record(state,
	{
	  role,
	  %%ssh_msg_handler,
	  ssh,
	  ctab,
	  binds = [],
	  users = [],
	  channel_id = 0,
	  opts,
	  requests = [], %% [{Channel, Pid}...] awaiting reply on request
	  authhandle     %% for session termination
	 }).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% Function: connect(...) -> {ok,Pid} | {error,Error}
%% Description: Starts the server (as an ssh-client)
%%--------------------------------------------------------------------
connect(Host) ->
    connect(Host, []).
connect(Host, Opts) ->
    connect(Host, ?SSH_DEFAULT_PORT, Opts).
connect(Host, Port, Opts) ->
    gen_server:start_link(?MODULE, [client, self(), Host, Port, Opts], []).

%%--------------------------------------------------------------------
%% Function: listen(...) -> Pid | {error,Error}
%% Description: Starts a listening server (as an ssh-server)
%%--------------------------------------------------------------------
listen(UserFun, Port) ->
    listen(UserFun, Port, []).
listen(UserFun, Port, Opts) ->
    listen(UserFun, any, Port, Opts).
listen(UserFun, Addr, Port, Opts) ->
    Self = self(),
    ssh_userauth:reg_user_auth_server(),
    ssh_transport:listen(
      fun(SSH) ->
	      {ok, CM} =
		  gen_server:start_link(
		    ?MODULE, [server, Self, UserFun, SSH, Opts], []),
	      CM
      end, Addr, Port, Opts).

%%--------------------------------------------------------------------
%% Function: stop_listener(Pid) -> ok
%% Description: Stops the listener
%%--------------------------------------------------------------------
stop_listener(Pid) ->
    ssh_transport:stop_listener(Pid).

%% %%
%% %% special ssh distribution version
%% %%
%% dist_start(Node) ->
%%     Opts1 = case init:get_argument('ssh_password') of
%% 	       {ok, [[Passwd]]} -> [{password, Passwd}];
%% 	       error -> []
%% 	   end,
%%     Opts2 = case init:get_argument('ssh_user') of
%% 		{ok, [[User]]} -> [{user, User}];
%% 		error -> []
%% 	    end,
%%     dist_start(Node, Opts1++Opts2).
    
%% dist_start(Node, Opts) when atom(Node), list(Opts) ->
%%     case string:tokens(atom_to_list(Node), "@") of
%% 	[_, "ssh:"++Host] ->
%% 	    CMHost = list_to_atom(Host),
%% 	    case whereis(CMHost) of
%% 		undefined ->
%% 		    start(CMHost, Host, Opts);
%% 		Pid when pid(Pid) ->
%% 		    {ok,Pid};
%% 		_ ->
%% 		    {error, einval}
%% 	    end;
%% 	_ ->
%% 	    {error, einval}
%%     end;
%% dist_start(_, _) ->
%%     {error, einval}.


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
init([server, _Caller, UserFun, SSH, Opts]) ->
    SSH ! {ssh_install, connect_messages()},
    process_flag(trap_exit, true),
    User = UserFun(),
    %% Caller ! {self(), {ok, self()}},
    CTab = ets:new(cm_tab, [set,{keypos, #channel.local_id}]),
    State = #state{role = server, ctab = CTab, ssh = SSH, opts = Opts,
		   requests = []},
    NewState = add_user(User, State),  %% add inital user
    {ok, NewState};
init([client, User, Host, Port, Opts]) ->
    case ssh_transport:connect(Host, Port, Opts) of
	{ok, SSH} ->
	    case user_auth(SSH, Opts) of
		ok ->
		    SSH ! {ssh_install, connect_messages()},
 		    process_flag(trap_exit, true),
		    CTab = ets:new(cm_tab, [set,{keypos,#channel.local_id}]),
		    State = #state{role = client, ctab = CTab, ssh = SSH,
				   opts = Opts, requests = []},
		    NewState = add_user(User, State),  %% add inital user
		    {ok, NewState};
		Error ->
		    ssh_transport:disconnect(
		      SSH, ?SSH_DISCONNECT_BY_APPLICATION),
		    {stop, Error}
	    end;
	Error ->
	    {stop, Error}
    end.

i(CM) ->
    i(CM, all).

i(CM, User) ->
    case info(CM, User) of
	{ok, Cs} ->
	    Cs1 = lists:keysort(#channel.user, Cs),
	    foreach(
	      fun(C) ->
		      io:format("~10p ~w ~s/~s ~w/~w ~w/~w\n",
				[C#channel.user,
				 C#channel.local_id,
				 C#channel.type, C#channel.sys,
				 C#channel.recv_window_size,
				 C#channel.recv_packet_size,
				 C#channel.send_window_size,
				 C#channel.send_packet_size])
	      end, Cs1);
	Error ->
	    Error
    end.    

info(CM) ->
    info(CM, all).

info(CM, User) ->
    gen_server:call(CM, {info, User}).

%% CM Client commands
session_open(CM, TMO) ->
    session_open(CM, ?DEFAULT_WINDOW_SIZE, ?DEFAULT_PACKET_SIZE, TMO).

session_open(CM, InitialWindowSize, MaxPacketSize, TMO) ->
    case gen_server:call(CM, {open, self(), "session",
			      InitialWindowSize, MaxPacketSize, <<>>}, TMO) of
	{open, C} -> {ok, C};
	Error -> Error
    end.

direct_tcpip(CM, RemoteHost, RemotePort, OrigIP, OrigPort, TMO) ->
    direct_tcpip(CM, RemoteHost, RemotePort, OrigIP, OrigPort,
		 ?DEFAULT_WINDOW_SIZE, ?DEFAULT_PACKET_SIZE, TMO).

direct_tcpip(CM, RemoteIP, RemotePort, OrigIP, OrigPort,
	     InitialWindowSize, MaxPacketSize, TMO) ->
    case {encode_ip(RemoteIP), encode_ip(OrigIP)} of
	{false, _} -> {error, einval};
	{_, false} -> {error, einval};
	{RIP, OIP} ->
	    gen_server:call(CM, {open, self(), "direct-tcpip", 
				 InitialWindowSize, MaxPacketSize,
				 [?string(RIP), ?uint32(RemotePort),
				  ?string(OIP), ?uint32(OrigPort)] }, TMO)
%%% 	    receive
%%% 		{ssh_cm, CM, {open, Channel}} ->
%%% 		    {ok, Channel};
%%% 		{ssh_cm, CM, {open_error, _Reason, Descr, _Lang}} ->
%%% 		    {error, Descr}
%%% 	    end
    end.

tcpip_forward(CM, BindIP, BindPort)                                        ->
    case encode_ip(BindIP) of
	false -> {error, einval};
	IPStr ->
	    global_request(CM, "tcpip-forward", true,
			   [?string(IPStr),
			    ?uint32(BindPort)])
    end.

cancel_tcpip_forward(CM, BindIP, Port) ->
    case encode_ip(BindIP) of
	false -> {error, einval};
	IPStr ->
	    global_request(CM, "cancel-tcpip-forward", true,
			   [?string(IPStr),
			    ?uint32(Port)])
    end.

open_pty(CM, Channel, TMO) ->
    open_pty(CM, Channel, os:getenv("TERM"), 80, 24, [], TMO).

open_pty(CM, Channel, Term, Width, Height, PtyOpts, TMO) ->
    open_pty(CM, Channel, Term, Width, Height, 0, 0, PtyOpts, TMO).


open_pty(CM, Channel, Term, Width, Height, PixWidth, PixHeight, PtyOpts, TMO) ->
    request(CM, Channel, "pty-req", true, 
	    [?string(Term),
	     ?uint32(Width), ?uint32(Height),
	     ?uint32(PixWidth),?uint32(PixHeight),
	     encode_pty_opts(PtyOpts)], TMO).

setenv(CM, Channel, Var, Value, TMO) ->
    request(CM, Channel, "env", true, [?string(Var), ?string(Value)], TMO).

shell(CM, Channel) ->
    request(CM, Channel, "shell", false, <<>>, 0).

exec(CM, Channel, Command, TMO) ->
    request(CM, Channel, "exec", true, [?string(Command)], TMO).

subsystem(CM, Channel, SubSystem, TMO) ->
    request(CM, Channel, "subsystem", true, [?string(SubSystem)], TMO).

winch(CM, Channel, Width, Height) ->
    winch(CM, Channel, Width, Height, 0, 0).
winch(CM, Channel, Width, Height, PixWidth, PixHeight)                     ->
    request(CM, Channel, "window-change", false, 
	    [?uint32(Width), ?uint32(Height),
	     ?uint32(PixWidth), ?uint32(PixHeight)], 0).

signal(CM, Channel, Sig)                                                   ->
    request(CM, Channel, "signal", false,
	    [?string(Sig)], 0).

attach(CM, TMO) ->
    gen_server:call(CM, {attach, self()}, TMO).

detach(CM, TMO) ->
    gen_server:call(CM, {detach, self()}, TMO).


renegotiate(CM) ->
    renegotiate(CM,[]).
renegotiate(CM,Opts) ->
    gen_server:cast(CM, {renegotiate,Opts}).

%% Setup user ack on data messages (i.e signal when the data has been sent)
set_user_ack(CM, Channel, Ack, TMO) ->
    gen_server:call(CM, {set_user_ack, Channel, Ack}, TMO).

get_authhandle(CM) ->
    gen_server:call(CM, get_authhandle).

get_peer_addr(CM) ->
    gen_server:call(CM, get_peer_addr).

set_user(CM, Channel, User, TMO) ->
    gen_server:call(CM, {set_user, Channel, User}, TMO).

send_window(CM, Channel, TMO) ->
    gen_server:call(CM, {send_window, Channel}, TMO).

recv_window(CM, Channel, TMO) ->
    gen_server:call(CM, {recv_window, Channel}, TMO).

adjust_window(CM, Channel, Bytes) ->
    gen_server:cast(CM, {adjust_window, Channel, Bytes}).

close(CM, Channel) ->
    gen_server:cast(CM, {close, Channel}).

stop(CM) ->
    gen_server:call(CM, stop).

send_eof(CM, Channel)                                                      ->
    gen_server:cast(CM, {eof, Channel}).

send(CM, Channel, Data)                                                    ->
    CM ! {ssh_cm, self(), {data, Channel, 0, Data}}.

send(CM, Channel, Type, Data)                                              ->
    CM ! {ssh_cm, self(), {data, Channel, Type, Data}}.

send_ack(CM, Channel, Data) ->
    send_ack(CM, Channel, 0, Data, infinity).

send_ack(CM, Channel, Type, Data) ->
    send_ack(CM, Channel, Type, Data, infinity).

send_ack(CM, Channel, Type, Data, Timeout) ->
    send(CM, Channel, Type, Data),
    receive
	{ssh_cm, CM, {ack, Channel}} ->
	    ok
    after Timeout ->
	    {error, timeout}
    end.

request(CM, Channel, Type, Reply, Data, TMO) ->
    case Reply of
	true -> gen_server:call(CM, {request, Channel, Type, Data}, TMO);
	false -> gen_server:cast(CM, {request, Channel, Type, Data})
    end.

global_request(CM, Type, Reply, Data) ->
    CM ! {ssh_cm, self(), {global_request,self(),Type,Reply,Data}},
    if Reply == true ->
	    receive
		{ssh_cm, CM, {success, _Channel}} ->
		    ok;
		{ssh_cm, CM, {failure, _Channel}} ->
		    error
	    end;
       true ->
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% CM command encode/decode table
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

connect_messages() ->
    [ {ssh_msg_global_request, ?SSH_MSG_GLOBAL_REQUEST,
       [string, 
	boolean,
	'...']},

      {ssh_msg_request_success, ?SSH_MSG_REQUEST_SUCCESS,
       ['...']},

      {ssh_msg_request_failure, ?SSH_MSG_REQUEST_FAILURE,
       []},
      
      {ssh_msg_channel_open, ?SSH_MSG_CHANNEL_OPEN,
       [string,
	uint32,
	uint32,
	uint32,
	'...']},

      {ssh_msg_channel_open_confirmation, ?SSH_MSG_CHANNEL_OPEN_CONFIRMATION,
       [uint32,
	uint32,
	uint32,
	uint32,
	'...']},

      {ssh_msg_channel_open_failure, ?SSH_MSG_CHANNEL_OPEN_FAILURE,
       [uint32,
	uint32,
	string,
	string]},

      {ssh_msg_channel_window_adjust, ?SSH_MSG_CHANNEL_WINDOW_ADJUST,
       [uint32,
	uint32]},

      {ssh_msg_channel_data, ?SSH_MSG_CHANNEL_DATA,
       [uint32,
	binary]},

      {ssh_msg_channel_extended_data, ?SSH_MSG_CHANNEL_EXTENDED_DATA,
       [uint32,
	uint32,
	binary]},

      {ssh_msg_channel_eof, ?SSH_MSG_CHANNEL_EOF,
       [uint32]},

      {ssh_msg_channel_close, ?SSH_MSG_CHANNEL_CLOSE,
       [uint32]},

      {ssh_msg_channel_request, ?SSH_MSG_CHANNEL_REQUEST,
       [uint32,
	string,
	boolean,
	'...']},

      {ssh_msg_channel_success, ?SSH_MSG_CHANNEL_SUCCESS,
       [uint32]},

      {ssh_msg_channel_failure, ?SSH_MSG_CHANNEL_FAILURE,
       [uint32]}
     ].


%%--------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------
handle_cast({request, Channel, Type, Data}, State) ->
    {noreply, do_request(Channel, Type, Data, false, undefined, State)};
handle_cast({renegotiate, Opts}, State) ->
    State#state.ssh ! {ssh_renegotiate, false, Opts},
    {noreply, State};
handle_cast({adjust_window, Channel, Bytes}, State) ->
    #state{ssh = SSH, ctab = CTab} = State,
    with_channel(
      State, Channel,
      fun(C) ->
	      WSz = C#channel.recv_window_size + Bytes,
	      channel_adjust_window(SSH, C#channel.remote_id, Bytes),
	      ets:insert(CTab, C#channel { recv_window_size = WSz})
      end),
    {noreply, State};
handle_cast({close, Channel}, State) ->
    #state{ssh = SSH, ctab = CTab} = State,
    with_channel(State, Channel,
		 fun(C) ->
			 channel_close(SSH, C#channel.remote_id),
			 ets:insert(CTab, C#channel{sent_close = true})
		 end),
    {noreply, State};
handle_cast({eof, Channel}, State) ->
    %%#state{ssh = SSH, ctab = _CTab} = State,
    SSH = State#state.ssh,
    with_channel(State, Channel,
		 fun(C) ->
			 channel_eof(SSH,  C#channel.remote_id)%,
			 %%ets:insert(CTab, C#channel{sent_eof = true})
		 end),
    {noreply, State};
handle_cast(_Cast, State) ->
    ?dbg(true, "handle_cast: BAD cast ~p\n(State ~p)\n", [_Cast, State]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({ssh_msg,SSH,#ssh_msg_service_request{name="ssh-userauth"}},
	    State) when State#state.role == server->
    case ssh_userauth:auth_remote(SSH, "ssh-connection", State#state.opts) of
	{ok, Handle} ->
	    {noreply, State#state{authhandle = Handle}};
	_Error ->
	    ssh_transport:disconnect(SSH, ?SSH_DISCONNECT_BY_APPLICATION),
	    %{stop, {error, Error}, State}
	    {stop, shutdown, State}
    end;
handle_info({ssh_msg, SSH, Msg}, State)                                    ->
    %%SSH = State#state.ssh,
    ?dbg(?DBG_SSHMSG, "handle_info<~p>: ssh_msg ~p\n", [SSH, Msg]),
    case ssh_message(SSH, Msg, State) of
	{disconnected, _Reason} ->
	    ssh_userauth:disconnect(State#state.authhandle, State#state.opts),
	    %{stop, {error, {disconnected, Reason}}, State};
	    {stop, shutdown, State};
	NewState ->
	    {noreply, NewState}
    end;
handle_info({ssh_cm, Sender, Msg}, State)                                  ->
    SSH = State#state.ssh,
    ?dbg(?DBG_SSHCM, "handle_info<~p>: sender=~p, ssh_cm ~p\n", [SSH, Sender, Msg]),
    %% only allow attached users (+ initial user)
    NewState = case is_user(Sender, State) of
		   false -> 
		       State;
		   true ->
		       cm_message(SSH, Msg, State)
	       end,
    {noreply, NewState};
handle_info({'EXIT', SSH, Reason}, State) when SSH == State#state.ssh      ->
    error_logger:format("SSH_CM ~p EXIT ~p\n", [SSH, Reason]),
    {noreply, State};
handle_info({'EXIT', _Pid, normal}, State)                                  ->
    {noreply, State};
handle_info({'EXIT', _Pid, shutdown}, State)                                  ->
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, State)                                  ->
    error_logger:format("ssh_cm: Pid ~p EXIT ~p\n", [Pid, Reason]),
    {noreply, State};

handle_info({'DOWN', _Ref, process, Pid, normal}, State)                   ->
    NewState = down_user(Pid, State),
    {noreply, NewState};
handle_info({'DOWN', _Ref, process, Pid, Reason}, State)                   ->
    error_logger:format("Pid ~p DOWN ~p\n", [Pid, Reason]),
    NewState = down_user(Pid, State),
    {noreply, NewState};


handle_info(_Info, State)                                                   ->
    ?dbg(true, "ssh_cm:handle_info: BAD info ~p\n(State ~p)\n", [_Info, State]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({attach, User}, _From, State) ->
    {reply, ok, add_user(User, State)};
handle_call({detach, User}, _From, State) ->
    {reply, ok, del_user(User, State)};
handle_call({send_window, Channel}, _From, State)                          ->
    Reply = case ets:lookup(State#state.ctab, Channel) of
		[C] ->
		    {ok, {C#channel.send_window_size,
			  C#channel.send_packet_size}};
		[] -> 
		    {error, einval}
	    end,
    {reply, Reply, State};
handle_call({request, Channel, Type, Data}, From, State) ->
    {noreply, do_request(Channel, Type, Data, true, From, State)};
handle_call({recv_window, Channel}, _From, State) ->
    Reply = case ets:lookup(State#state.ctab, Channel) of
		[C] ->
		    {ok, {C#channel.recv_window_size,
			  C#channel.recv_packet_size}};
		[] -> 
		    {error, einval}
	    end,
    {reply, Reply, State};
handle_call({set_user, Channel, User}, _From, State) ->
    Reply = case is_user(User, State) of
		false -> {error, einval};
		true ->
		    CTab = State#state.ctab,
		    case ets:lookup(CTab, Channel) of
			[C] ->
			    ets:insert(CTab, C#channel { user = User }),
			    ok;
			[] -> 
			    {error, einval}
		    end
	    end,
    {reply, Reply, State};
handle_call(get_authhandle, _From, State) ->
    {reply, {State#state.authhandle,State#state.ssh}, State};
handle_call(get_peer_addr, _From, State) ->
    {reply, ssh_transport:peername(State#state.ssh), State};
handle_call({set_user_ack, Channel,Ack}, _From, State) ->
    CTab = State#state.ctab,
    Reply = case ets:lookup(CTab, Channel) of
		[C] ->
		    ets:insert(CTab, C#channel { user_ack = Ack }),
		    ok;
		[] -> 
		    {error, einval}
	    end,
    {reply, Reply, State};
handle_call({info,User}, _From, State) ->
    Result = ets:foldl(
	       fun(C, Acc) when User == all; C#channel.user == User ->
		       [C | Acc];
		  (_, Acc) ->
		       Acc
	       end, [], State#state.ctab),
    {reply, {ok, Result}, State};
handle_call({open, User, Type, InitialWindowSize, MaxPacketSize, Data}, From, State) ->
	case is_user(User, State) of
	    false -> 
		{reply, {error, einval}, State};
	    true ->
		{Channel, State1}  = new_channel_id(State),
		channel_open(State#state.ssh, Type, Channel, InitialWindowSize,
			     MaxPacketSize, Data),
		C = #channel { type = Type,
			       sys = "none",
			       user = User,
			       local_id = Channel,
			       recv_window_size = InitialWindowSize,
			       recv_packet_size = MaxPacketSize },
		ets:insert(State#state.ctab, C),
		State2 = add_request(true, Channel, From, State1),
		{noreply, State2}
	end;
handle_call(stop, _From, State) ->
    ssh_userauth:disconnect(State#state.authhandle, State#state.opts),
    ssh_transport:close(State#state.ssh),
    {stop, normal, ok, State};
handle_call(_Call, _From, State) ->
    ?dbg(true, "handle_call: BAD call ~p\n(State ~p)\n", [_Call, State]),
    {reply, {error, bad_call}, State}.


%%--------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
ssh_message(SSH, Msg, State) ->
    CTab = State#state.ctab,
    case Msg of
	#ssh_msg_channel_open_confirmation { recipient_channel = Channel,
					     sender_channel = RID,
					     initial_window_size = WindowSz,
					     maximum_packet_size = PacketSz
					    } ->
	    with_channel(State, Channel,
			 fun(C) ->
				 if C#channel.remote_id == undefined ->
					 ets:insert(CTab, C#channel {
							    remote_id = RID,
							    send_window_size = WindowSz,
							    send_packet_size = PacketSz });
				    true ->
					 ignore
				 end
			 end),
	    reply_request(Channel, {open, Channel}, State);

	#ssh_msg_channel_open_failure { recipient_channel = Channel,
					reason = Reason,
					description = Descr,
					lang = Lang } ->
	    with_channel(State, Channel,
			 fun() ->
				 ets:delete(CTab, Channel)
			 end),
	    reply_request(Channel, {open_error, Reason, Descr, Lang}, State);

	#ssh_msg_channel_success { recipient_channel = Channel } ->
	    reply_request(Channel, success, State);

	#ssh_msg_channel_failure { recipient_channel = Channel} ->
	    reply_request(Channel, failure, State);
	    
	#ssh_msg_channel_eof { recipient_channel = Channel} ->
	    with_channel(State, Channel,
			 fun(C) ->
				 send_user(C, {eof, Channel})
%% 				 if C#channel.sent_eof == true ->
%% 					 reply_request(C#channel.local_id,
%% 						       closed, State),
%% 					 io:format("delete Channel b ~p\n", [Channel]),
%% 					 ets:delete(CTab, Channel);
%% 				    true ->
%% 					 ets:insert(CTab, C#channel { recv_eof = true })
%% 				 end
			 end);

	#ssh_msg_channel_close { recipient_channel = Channel } ->
	    with_channel(State, Channel,
			 fun(C) ->
				 if C#channel.sent_close == false ->
					 channel_close(SSH,C#channel.remote_id),
 					 reply_request(C#channel.local_id,
						       closed, State);
				    true -> 
					 ok
				 end,
				 ets:delete(CTab, Channel)
			 end),
	    reply_request(Channel, closed, State);

	#ssh_msg_channel_data { recipient_channel = Channel,
				data = Data } ->
	    with_channel(State, Channel,
			 fun(C) ->
				 WSz = C#channel.recv_window_size - size(Data),
				 send_user(C, {data, Channel, 0, Data}),
				 ets:insert(CTab, C#channel { recv_window_size = WSz})
			 end);

	#ssh_msg_channel_extended_data { recipient_channel = Channel,
					 data_type_code = DataType,
					 data = Data} ->
	    with_channel(State, Channel,
			 fun(C) ->
				 WSz = C#channel.recv_window_size - size(Data),
				 send_user(C, {data, Channel, DataType, Data}),
				 ets:insert(CTab, C#channel { recv_window_size = WSz})
			 end);

	#ssh_msg_channel_window_adjust { recipient_channel = Channel,
					 bytes_to_add = Add } ->
	    with_channel(State, Channel, 
			 fun(C) -> 
				 update_send_window(SSH, CTab, C, Add) 
			 end);

	#ssh_msg_channel_open { channel_type = Type,
				sender_channel = RID,
				initial_window_size = RWindowSz,
				maximum_packet_size = RPacketSz,
				data = Data } ->
	    case Type of
		"session" ->
		    %% FIXME: check that we requested this !
		    %% (install a listener & user somehow)
%		    <<?UINT32(ALen), Address:ALen/binary, ?UINT32(Port),
%		     ?UINT32(OLen), Orig:OLen/binary, ?UINT32(OrigPort)>> = Data,
		    case State#state.users of
			[{User,_}|_] ->
			    {Channel, NewState} = new_channel_id(State),
			    LWindowSz = ?DEFAULT_WINDOW_SIZE,
			    LPacketSz = ?DEFAULT_PACKET_SIZE,
			    C = #channel { type = Type,
					   sys = "ssh",
					   user = User,
					   local_id = Channel,
					   recv_window_size = LWindowSz,
					   recv_packet_size = LPacketSz,
					   send_window_size = RWindowSz,
					   send_packet_size = RPacketSz,
					   remote_id = RID},
			    ets:insert(CTab, C),
			    channel_open_confirmation(SSH, RID, Channel,
						      LWindowSz, LPacketSz),
			    send_user(C, {open, Channel, RID, {session}}),
			    NewState;
			_ ->
			    channel_open_failure(SSH, RID, 
						 ?SSH_OPEN_CONNECT_FAILED,
						 "Connection refused", "en"),
			    State
		    end;
		    
		"forwarded-tcpip" ->
		    %% FIXME: check that we requested this !
		    %% (install a listener & user somehow)
		    <<?UINT32(ALen), Address:ALen/binary, ?UINT32(Port),
		     ?UINT32(OLen), Orig:OLen/binary, ?UINT32(OrigPort)>> = Data,
		    case get_bind(Address, Port, State) of
			undefined ->
			    channel_open_failure(SSH, RID, 
						 ?SSH_OPEN_CONNECT_FAILED,
						 "Connection refused", "en"),
			    State;
			User ->
			    {Channel, NewState} = new_channel_id(State),
			    LWindowSz = ?DEFAULT_WINDOW_SIZE,
			    LPacketSz = ?DEFAULT_PACKET_SIZE,
			    C = #channel { type = Type,
					   sys = "none",
					   user = User,
					   local_id = Channel,
					   recv_window_size = LWindowSz,
					   recv_packet_size = LPacketSz,
					   send_window_size = RWindowSz,
					   send_packet_size = RPacketSz },
			    ets:insert(CTab, C),
			    channel_open_confirmation(SSH, RID, Channel,
						      LWindowSz, LPacketSz),
			    send_user(C, {open, Channel, {forwarded_tcpip,
							  decode_ip(Address), Port,
							  decode_ip(Orig), OrigPort}}),
			    NewState
		    end;
		_ ->
		    channel_open_failure(SSH, RID, 
					 ?SSH_OPEN_ADMINISTRATIVELY_PROHIBITED,
					 "Not allowed", "en"),
		    State
	    end;

	#ssh_msg_channel_request { recipient_channel = Channel,
				   request_type = Type,
				   want_reply = WantReply,
				   data = Data } ->
	    case Type of
		"exit-status" ->
		    <<?UINT32(Status)>> = Data,
		    send_user(CTab, Channel, {exit_status,Channel,Status});
		"exit-signal" ->
		    <<?UINT32(SigLen), SigName:SigLen/binary,
		     ?BOOLEAN(_Core), 
		     ?UINT32(ErrLen), Err:ErrLen/binary,
		     ?UINT32(LangLen), Lang:LangLen/binary>> = Data,
		    send_user(CTab, Channel, {exit_signal, Channel,
					      binary_to_list(SigName),
					      binary_to_list(Err),
					      binary_to_list(Lang)});
		"xon-xoff" ->
		    <<?BOOLEAN(CDo)>> = Data,
		    send_user(CTab, Channel, {xon_xoff, Channel, CDo=/= 0});
		
		"window-change" ->
		    <<?UINT32(Width),?UINT32(Height),
		     ?UINT32(PixWidth), ?UINT32(PixHeight)>> = Data,
		    send_user(CTab, Channel, {window_change,Channel,
					      Width, Height,
					      PixWidth, PixHeight});
		"signal" ->
		    <<?UINT32(SigLen), SigName:SigLen/binary>> = Data,
		    send_user(CTab, Channel, {signal,Channel,
					      binary_to_list(SigName)});
		"subsystem" ->
		    <<?UINT32(SsLen), SsName:SsLen/binary>> = Data,
		    send_user(CTab, Channel, {subsystem,Channel, WantReply,
					      binary_to_list(SsName)});
		"pty-req" ->
		    <<?UINT32(TermLen), BTermName:TermLen/binary,
		     ?UINT32(Width),?UINT32(Height),
		     ?UINT32(PixWidth), ?UINT32(PixHeight),
		     Modes/binary>> = Data,
		    TermName = binary_to_list(BTermName),
		    ?dbg(?DBG_USER, "ssh_msg pty-req: TermName=~p Modes=~p\n", [TermName, Modes]),
		    Pty = #ssh_pty{term = TermName,
				   width = not_zero(Width, 80),
				   height = not_zero(Height, 24),
				   pixel_width = PixWidth,
				   pixel_height = PixHeight,
				   modes = decode_pty_opts(Modes)},
		    send_user(CTab, Channel, {pty, Channel, WantReply, Pty});

		"shell" ->
		    send_user(CTab, Channel, {shell});

		"exec" ->
		    <<?UINT32(Len), Command:Len/binary>> = Data,
		    send_user(CTab, Channel, {exec, binary_to_list(Command)});

		_Other ->
		    ?dbg(true, "ssh_msg ssh_msg_channel_request: Other=~p\n",
			 [_Other]),
		    if WantReply == true ->
			    channel_failure(SSH, Channel);
		       true ->
			    ignore
		    end
	    end,
	    State;
	    
	#ssh_msg_global_request { name = _Type,
				  want_reply = WantReply,
				  data = _Data } ->
	    if WantReply == true ->
		    request_failure(SSH);
	       true ->
		    ignore
	    end,
	    State;
	

	#ssh_msg_disconnect { code = Code,
			      description = Description,
			      language = _Lang } ->
	    %% close all channels
	    ets:foldl(
	      fun(C, _) ->
		      reply_request(C#channel.local_id, closed, State)
	      end, ok, CTab),
	    ets:delete(CTab),
	    {disconnected, {Code, Description}};

	_ ->
	    ?dbg(true, "ssh_connection: ~p\n", [Msg]),
	    State
    end.

cm_message(SSH, Msg, State) ->
    CTab = State#state.ctab,
    case Msg of
	{data, Channel, Type, Data} ->
	    send_data(SSH, CTab, Channel, Type, Data),
	    State;

	{global_request, User, Type, WantReply, Data} ->
	    case Type of
		"tcpip-forward" ->
		    <<?UINT32(IPLen), IP:IPLen/binary, ?UINT32(Port)>> = Data,
		    State1 = add_user(User, State),  %% auto attach user
		    State2 = put_bind(IP, Port, User, State1),
		    send_global_request(SSH, Type, WantReply, Data),
		    State2;
		"cancel-tcpip-forward" ->
		    <<?UINT32(IPLen), IP:IPLen/binary, ?UINT32(Port)>> = Data,
		    %% note can not erase user!
		    send_global_request(SSH, Type, WantReply, Data),
		    del_bind(IP, Port, State);
		_ ->
		    send_global_request(SSH, Type, WantReply, Data),
		    State
	    end;

	{success, Channel} ->
	    channel_success(SSH, Channel),
	    State;

	_ ->
	    ?dbg(true, "ssh_connection: ~p\n", [Msg]),
	    State
    end.

update_sys(CTab, C, Type) ->
    case Type of
	"subsystem" ->
	    ets:insert(CTab, C#channel { sys = "subsystem" });
	"exec" -> 
	    ets:insert(CTab, C#channel { sys = "subsystem" });
	"shell" -> 
	    ets:insert(CTab, C#channel { sys = "shell" });
	_ ->
	    ok
    end.

user_auth(SSH, Opts) ->
    case ssh_transport:service_request(SSH, "ssh-userauth") of
	ok ->
	    ssh_userauth:auth(SSH, "ssh-connection", Opts);
	Error ->
	    Error
    end.

%% User is down
down_user(User, State) ->
    #state{ctab = CTab, ssh = SSH} = State,
    case is_user(User, State) of
	true ->
	    ets:foldl(
	      fun(C, _) when C#channel.user == User ->
		      channel_close(SSH,  C#channel.remote_id),
		      ets:delete(CTab, C#channel.local_id);
		 (_C, _) ->
		      ok
	      end, ok, CTab),
	    del_user(User, State);
	false ->
	    State
    end.

%% reply to request, or send to user, depending on whether the
%% Channel is in requests
reply_request(Channel, Reply, State) ->
    #state{ctab = CTab, requests = Requests} = State,
    case lists:keysearch(Channel, 1, Requests) of
	{value, {Channel, From}} ->
	    gen_server:reply(From, Reply),
	    State#state{requests = lists:keydelete(Channel, 1, Requests)};
	false ->
	    send_user(CTab, Channel, {Reply, Channel}),
	    State
    end.

%% Send ssh_cm messages to the 'user'
send_user(C, Msg) when record(C, channel) ->
    C#channel.user ! {ssh_cm, self(), Msg}.

send_user(CTab, Channel, Msg) ->
    case ets:lookup(CTab, Channel) of
	[C] ->
	    send_user(C, Msg);
	[] ->
	    ignore
    end.

%% Update the send window with Data
%% adjust output window
%%
%% buffer is on form [{DataType,UserAck,User,Data}]
%% DataType = 0   regular data
%%            1   stderr data
%% UserAck = true if "User" wants ack when data was sent
%% Data = io-list
%%
send_data(SSH, CTab, LID, DataType, Data0) ->
    case ets:lookup(CTab, LID) of
	[C] ->
	    Data = if binary(Data0) -> Data0;
		      list(Data0) -> list_to_binary(Data0)
		   end,
	    send_window(SSH,CTab,C, DataType,
			C#channel.user_ack, C#channel.user,
			Data);
	[] ->
	    ignore
    end.

update_send_window(SSH, CTab, C, Bytes) ->
    WSz0 = C#channel.send_window_size,
    send_window(SSH, CTab, C#channel { send_window_size = WSz0+Bytes},
		0, false, undefined, <<>>).


send_window(SSH, CTab, C, DataType, UserAck, User, Data) ->
    foreach(
      fun({Type,Ack,Usr,Data1}) ->
	      channel_data(SSH, C#channel.remote_id, Type, Data1),
	      if Ack == true ->
		      Usr ! {ssh_cm, self(), {ack, C#channel.local_id}};
		 true ->
		      ok
	      end
      end, remove_from_send_window(CTab, C, DataType, UserAck, User, Data)).


%% Get data from the send buffer 
%% each buffer sent must be less than packet size
remove_from_send_window(CTab, C, DataType, UserAck, User, Data) ->
    Buf0 = if Data == <<>> ->
		   C#channel.send_buf;
	      true ->
		   C#channel.send_buf ++ [{DataType,UserAck,User,Data}]
	   end,
    {Buf1,NewSz,Buf2} = get_window(Buf0, 
				   C#channel.send_packet_size,
				   C#channel.send_window_size),
    ets:insert(CTab, C#channel { send_window_size = NewSz,
				 send_buf = Buf2}),
    Buf1.

get_window(Bs, PSz, WSz) ->
    get_window(Bs, PSz, WSz, []).

get_window(Bs, _PSz, 0, Acc) ->
    {reverse(Acc), 0, Bs};
get_window([B0 = {DataType,_UserAck,_User,Bin} | Bs], PSz, WSz, Acc) ->
    BSz = size(Bin),
    if BSz =< WSz ->  %% will fit into window
	    if BSz =< PSz ->  %% will fit into a packet
		    get_window(Bs, PSz, WSz-BSz, [B0|Acc]);
	       true -> %% split into packet size
		    <<Bin1:PSz/binary, Bin2/binary>> = Bin,
		    get_window([setelement(4, B0, Bin2) | Bs],
			       PSz, WSz-PSz, 
			       [{DataType,false,undefined,Bin1}|Acc])
	    end;
       WSz =< PSz ->  %% use rest of window
	    <<Bin1:WSz/binary, Bin2/binary>> = Bin,
	    get_window([setelement(4, B0, Bin2) | Bs],
		       PSz, WSz-WSz, 
		       [{DataType,false,undefined,Bin1}|Acc]);
       true -> %% use packet size
	    <<Bin1:PSz/binary, Bin2/binary>> = Bin,
	    get_window([setelement(4, B0, Bin2) | Bs],
		       PSz, WSz-PSz, 
		       [{DataType,false,undefined,Bin1}|Acc])
    end;
get_window([], _PSz, WSz, Acc) ->
    {reverse(Acc), WSz, []}.


%%
%% CHANNEL Commands
%%
channel_eof(SSH, Channel) ->
    SSH ! {ssh_msg, self(), 
	   #ssh_msg_channel_eof { recipient_channel = Channel }}.

channel_close(SSH, Channel) ->
    SSH ! {ssh_msg, self(), 
	   #ssh_msg_channel_close { recipient_channel = Channel }}.

channel_success(SSH, Channel) ->
    SSH ! {ssh_msg, self(),
	   #ssh_msg_channel_success { recipient_channel = Channel }}.

channel_failure(SSH, Channel) ->
    SSH ! {ssh_msg, self(),
	   #ssh_msg_channel_failure { recipient_channel = Channel }}.


channel_adjust_window(SSH, Channel, Bytes) ->
    SSH ! {ssh_msg, self(), 
	   #ssh_msg_channel_window_adjust { recipient_channel = Channel,
					    bytes_to_add = Bytes }}.


channel_data(SSH, Channel, 0, Data) ->
    SSH ! {ssh_msg, self(),
	   #ssh_msg_channel_data { recipient_channel = Channel,
				   data = Data }};
channel_data(SSH, Channel, Type, Data) ->
    SSH ! {ssh_msg, self(),
	   #ssh_msg_channel_extended_data { recipient_channel = Channel,
					    data_type_code = Type,
					    data = Data }}.

channel_open(SSH, Type, Channel, WindowSize, MaxPacketSize, Data) ->
    SSH ! {ssh_msg, self(), 
	   #ssh_msg_channel_open { channel_type = Type,
				   sender_channel = Channel,
				   initial_window_size = WindowSize,
				   maximum_packet_size = MaxPacketSize,
				   data = Data
				  }}.

channel_open_confirmation(SSH, RID, LID, WindowSize, PacketSize) ->
    SSH ! {ssh_msg, self(),
	   #ssh_msg_channel_open_confirmation { recipient_channel = RID,
						sender_channel = LID,
						initial_window_size = WindowSize,
						maximum_packet_size = PacketSize}}.

channel_open_failure(SSH, RID, Reason, Description, Lang) ->
    SSH ! {ssh_msg, self(),
	   #ssh_msg_channel_open_failure { recipient_channel = RID,
					   reason = Reason,
					   description = Description,
					   lang = Lang }}.
					   
    

channel_request(SSH, Channel, Type, WantReply, Data) ->
    SSH ! {ssh_msg, self(),
	   #ssh_msg_channel_request { recipient_channel = Channel,
				      request_type = Type,
				      want_reply = WantReply,
				      data = Data }}.

send_global_request(SSH, Type, WantReply, Data) ->
    SSH ! {ssh_msg, self(),
	   #ssh_msg_global_request { name = Type,
				     want_reply = WantReply,
				     data = Data }}.

request_failure(SSH) ->
    SSH ! {ssh_msg, self(), #ssh_msg_request_failure {}}.

request_success(SSH,Data) ->
    SSH ! {ssh_msg, self(), #ssh_msg_request_success { data=Data }}.



decode_pty_opts(<<>>) ->		     
    [];
decode_pty_opts(<<0, 0, 0, 0>>) ->
    [];
decode_pty_opts(<<?UINT32(Len), Modes:Len/binary>>) ->
    decode_pty_opts2(Modes);
decode_pty_opts(Binary) ->
    decode_pty_opts2(Binary).

decode_pty_opts2(<<?TTY_OP_END>>) ->		     
    [];
decode_pty_opts2(<<Code, ?UINT32(Value), Tail/binary>>) ->
    Op = case Code of
	     ?VINTR -> vintr;
	     ?VQUIT -> vquit;
	     ?VERASE -> verase;
	     ?VKILL -> vkill;
	     ?VEOF -> veof;
	     ?VEOL -> veol;
	     ?VEOL2 -> veol2;
	     ?VSTART -> vstart;
	     ?VSTOP -> vstop;
	     ?VSUSP -> vsusp;
	     ?VDSUSP -> vdsusp;
	     ?VREPRINT -> vreprint;
	     ?VWERASE -> vwerase;
	     ?VLNEXT -> vlnext;
	     ?VFLUSH -> vflush;
	     ?VSWTCH -> vswtch;
	     ?VSTATUS -> vstatus;
	     ?VDISCARD -> vdiscard;
	     ?IGNPAR -> ignpar;
	     ?PARMRK -> parmrk;
	     ?INPCK -> inpck;
	     ?ISTRIP -> istrip;
	     ?INLCR -> inlcr;
	     ?IGNCR -> igncr;
	     ?ICRNL -> icrnl;
	     ?IUCLC -> iuclc;
	     ?IXON -> ixon;
	     ?IXANY -> ixany;
	     ?IXOFF -> ixoff;
	     ?IMAXBEL -> imaxbel;
	     ?ISIG -> isig;
	     ?ICANON -> icanon;
	     ?XCASE -> xcase;
	     ?ECHO -> echo;
	     ?ECHOE -> echoe;
	     ?ECHOK -> echok;
	     ?ECHONL -> echonl;
	     ?NOFLSH -> noflsh;
	     ?TOSTOP -> tostop;
	     ?IEXTEN -> iexten;
	     ?ECHOCTL -> echoctl;
	     ?ECHOKE -> echoke;
	     ?PENDIN -> pendin;
	     ?OPOST -> opost;
	     ?OLCUC -> olcuc;
	     ?ONLCR -> onlcr;
	     ?OCRNL -> ocrnl;
	     ?ONOCR -> onocr;
	     ?ONLRET -> onlret;
	     ?CS7 -> cs7;
	     ?CS8 -> cs8;
	     ?PARENB -> parenb;
	     ?PARODD -> parodd;
	     ?TTY_OP_ISPEED -> tty_op_ispeed;
	     ?TTY_OP_OSPEED -> tty_op_ospeed
	 end,    
    [{Op, Value} | decode_pty_opts2(Tail)].



encode_pty_opts([{Opt,Value} | Opts]) ->
    Code = case Opt of
	       vintr -> ?VINTR;
	       vquit -> ?VQUIT;
	       verase -> ?VERASE;
	       vkill -> ?VKILL;
	       veof -> ?VEOF;
	       veol -> ?VEOL;
	       veol2 -> ?VEOL2;
	       vstart -> ?VSTART;
	       vstop -> ?VSTOP;
	       vsusp -> ?VSUSP;
	       vdsusp -> ?VDSUSP;
	       vreprint -> ?VREPRINT;
	       vwerase -> ?VWERASE;
	       vlnext -> ?VLNEXT;
	       vflush -> ?VFLUSH;
	       vswtch -> ?VSWTCH;
	       vstatus -> ?VSTATUS;
	       vdiscard -> ?VDISCARD;
	       ignpar -> ?IGNPAR;
	       parmrk -> ?PARMRK;
	       inpck -> ?INPCK;
	       istrip -> ?ISTRIP;
	       inlcr -> ?INLCR;
	       igncr -> ?IGNCR;
	       icrnl -> ?ICRNL;
	       iuclc -> ?IUCLC;
	       ixon -> ?IXON;
	       ixany -> ?IXANY;
	       ixoff -> ?IXOFF;
	       imaxbel -> ?IMAXBEL;
	       isig -> ?ISIG;
	       icanon -> ?ICANON;
	       xcase -> ?XCASE;
	       echo -> ?ECHO;
	       echoe -> ?ECHOE;
	       echok -> ?ECHOK;
	       echonl -> ?ECHONL;
	       noflsh -> ?NOFLSH;
	       tostop -> ?TOSTOP;
	       iexten -> ?IEXTEN;
	       echoctl -> ?ECHOCTL;
	       echoke -> ?ECHOKE;
	       pendin -> ?PENDIN;
	       opost -> ?OPOST;
	       olcuc -> ?OLCUC;
	       onlcr -> ?ONLCR;
	       ocrnl -> ?OCRNL;
	       onocr -> ?ONOCR;
	       onlret -> ?ONLRET;
	       cs7 -> ?CS7;
	       cs8 -> ?CS8;
	       parenb -> ?PARENB;
	       parodd -> ?PARODD;
	       tty_op_ispeed -> ?TTY_OP_ISPEED;
	       tty_op_ospeed -> ?TTY_OP_OSPEED
	   end,
    [Code, ?uint32(Value) | encode_pty_opts(Opts)];
encode_pty_opts([]) -> 
    [?TTY_OP_END].


decode_ip(Addr) when tuple(Addr) ->
    Addr;
decode_ip(Addr) when binary(Addr) ->
    decode_ip(binary_to_list(Addr));
decode_ip(Addr) when list(Addr) ->
    case inet_parse:address(Addr) of
	{error,_} -> Addr;
	{ok,A}    -> A
    end.

%% return string() | false
encode_ip(Addr) when tuple(Addr) ->
    case catch inet_parse:ntoa(Addr) of
	{'EXIT',_} -> false;
	A -> A
    end;
encode_ip(Addr) when list(Addr) ->
    case inet_parse:address(Addr) of
	{ok, _} -> Addr;
	Error ->
	    case inet:getaddr(Addr, inet) of
		{ok, A} ->
		    inet_parse:ntoa(A);
		Error -> false
	    end
    end.


%% requests

do_request(Channel, Type, Data, WantReply, From, State) ->
    #state{ctab = CTab, ssh = SSH} = State,
    with_channel(
      State, Channel,
      fun(C) ->
	      update_sys(CTab, C, Type),
	      channel_request(SSH, C#channel.remote_id,
			      Type, WantReply, Data)
      end),
    add_request(WantReply, Channel, From, State).

add_request(false, _Channel, _From, State) ->
    State;
add_request(true, Channel, From, State) ->
    Requests = [{Channel, From} | State#state.requests],
    State#state{requests = Requests}.
    
    


%% state functions


put_bind(IP, Port, User, State) ->
    Binds = [{{IP, Port}, User}
	     | lists:keydelete({IP, Port}, 1, State#state.binds)],
    State#state{binds = Binds}.

del_bind(IP, Port, State) ->
    State#state{binds = lists:keydelete({IP, Port}, 1, State#state.binds)}.

del_binds_by_user(User, State) ->
    Binds = [{B, U} || {B, U} <- State#state.binds, U =/= User],
    State#state{binds = Binds}.

get_bind(IP, Port, State) ->
    case lists:keysearch({IP, Port}, 1, State) of
	{value, User} -> User;
	_ -> undefined
    end.

with_channel(State, Channel, Fun) ->
    case ets:lookup(State#state.ctab, Channel) of
	[C] ->
	    Fun(C);
	[] ->
	    ignore
    end,
    State.

%% Add a user
add_user(User, State) ->
    Users = State#state.users,
    case lists:keymember(User, 1, Users) of
	false ->
	    Ref = erlang:monitor(process, User),
	    State#state{users = [{User, Ref} | Users]};
	true ->
	    State
    end.

%% Remove user
del_user(User, State) ->
    #state{users = Users, ssh = SSH} = State,
    ?dbg(false, "del user: ~p\n",[User]),
    case lists:keysearch(User, 1, Users) of
	false ->
	    {{error, einval}, State};
	{value, {User, Ref}} ->
	    erlang:demonitor(Ref),
	    State1 = del_binds_by_user(User, State),
	    State2 = State1#state{users = lists:keydelete(User, 1, Users)},
	    %% exit if no more users and we are unregistered
	    if Users == [] ->
		    case process_info(self(), registered_name) of
			[] ->
			    ssh_transport:disconnect(SSH, 0);
			{registered_name,_Name} ->
			    ok
		    end;
	       true ->
		    ok
	    end,
	    State2	    
    end.

is_user(User, State) ->
    lists:keymember(User, 1, State#state.users).	    

%% Allocate channel ID 
new_channel_id(State) ->
    ID = State#state.channel_id,
    {ID, State#state{channel_id = ID + 1}}.

not_zero(0, B) -> B;
not_zero(A, _) -> A.
