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

-module(ssh_connection).

-behaviour(gen_server).

-include("ssh.hrl").

-export([
	 open/1,
	 open/2,
	 user/3,
	 start_subsystem/2,
	 exec/2,
	 shell/1,
	 print_state/1,
	 send_msg/3,
	 set_active_once/2,
	 close_session/2,
	 close/1
	]).

-export([
	 init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 code_change/3,
	 terminate/2
	]).

-record(ssh_connection, {transport = '_',
			 sessions = []}).

-record(ssh_session, {recipient_channel = '_',
		      sender_channel =	'_',
		      window_size = 	'_',
		      max_packet_size =	'_',
		      owner =		'_',
		      owner_pid =	'_',
		      recv_window =	'_',
		      recv_packet_size =	'_',
		      msgq = 		[],
		      recvq =		0,
		      sendq =		[],
		      pending_close =	'_'}).

-define(DEFAULT_TIMEOUT, 60000).

open(Host) ->
    open(Host, []).

open(Host, Opts) ->
    {ok, Pid} = gen_server:start_link(?MODULE, ok, []),
    case gen_server:call(Pid, {post_init, Host, Opts},
			 ?DEFAULT_TIMEOUT) of
	ok ->
	    {ok, Pid};
	Other ->
	    Other
    end.

user(Pid, User, Password) ->
    gen_server:call(Pid, {user, User, Password}, ?DEFAULT_TIMEOUT).

start_subsystem(Transport, Subsystem) ->
    gen_server:call(Transport, {start_subsystem, Subsystem, self()},
		    ?DEFAULT_TIMEOUT).

exec(Transport, Command) ->
    gen_server:call(Transport, {exec, Command, self()}, ?DEFAULT_TIMEOUT).

shell(Transport) ->
    gen_server:call(Transport, {shell, self()}, ?DEFAULT_TIMEOUT).

send_msg(Pid, RecipientChannel, {Size, Msg}) ->
    gen_server:call(Pid, {send_msg, RecipientChannel, {Size, Msg}}, ?DEFAULT_TIMEOUT);
send_msg(Pid, RecipientChannel, Msg) ->
    send_msg(Pid, RecipientChannel, {ssh_transport:flatlength(Msg), Msg}).

set_active_once(Pid, RecipientChannel) ->
    gen_server:call(Pid, {active_once, self(), RecipientChannel}).

close_session(Pid, RecipientChannel) ->
    gen_server:call(Pid, {close_session, RecipientChannel}, ?DEFAULT_TIMEOUT).

close(Pid) ->
    gen_server:call(Pid, close, ?DEFAULT_TIMEOUT).

print_state(Pid) ->
    gen_server:cast(Pid, print_state).

init(_) ->
    process_flag(trap_exit, true),
    {ok, #ssh_connection{}}.

handle_call({post_init, Host, Opts}, From, State) ->
    case ssh_transport:open(Host, Opts) of
	{ok, Transport} ->
	    {reply, ok, State#ssh_connection{transport = Transport}};
	{error, Reason} ->
	    {stop, normal, {error, Reason}, State};
	Other ->
	    {stop, Other, State}
    end;
handle_call({user, User, Password}, From, State) ->
    case ssh_userauth:passwd(State#ssh_connection.transport,
			     User,
			     Password,
			     "ssh-connection") of
	ok ->
	    {reply, ok, State};
	Other ->
	    {reply, Other, State}
    end;
handle_call({start_subsystem, Subsystem, OwnerPid}, From, State) ->
    start_session(subsystem,
		  length(State#ssh_connection.sessions),
		  From,
		  Subsystem,
		  OwnerPid,
		  State);
handle_call({exec, Command, OwnerPid}, From, State) ->
    start_session(exec,
		  length(State#ssh_connection.sessions),
		  From,
		  Command,
		  OwnerPid,
		  State);
handle_call({shell, OwnerPid}, From, State) ->
    start_session(shell,
		  length(State#ssh_connection.sessions),
		  From,
		  OwnerPid,
		  State);
handle_call({send_msg, RecipientChannel, {Size, Msg}}, From, State) ->
    case extract_session(RecipientChannel,
			 State#ssh_connection.sessions) of
	{ok, Session = #ssh_session{sender_channel = SenderChannel,
				    window_size = WindowSize,
				    max_packet_size = PacketSize,
				    msgq = Msgs},
	 Rest} ->
	    case Size of
		Size when Size > PacketSize, PacketSize =< WindowSize ->
		    [MsgH, MsgT] = ssh_transport:split_data([PacketSize],
							    Msg),
		    case lists:member({error, close}, Msgs) of
			true ->
			    {reply, {error, closed}, State};
			false ->
			    CompleteSize = 9 + PacketSize,
			    case ssh_transport:send_msg(
				   State#ssh_connection.transport,
				   {CompleteSize,
				    [?SSH_MSG_CHANNEL_DATA,
				     <<SenderChannel:32/integer>>,
				     <<PacketSize:32/integer>>, MsgH]}) of
				ok ->
				    handle_call({send_msg, RecipientChannel,
						 {Size - PacketSize, MsgT}},
						From,
						State#ssh_connection{
						  sessions =
						  [Session#ssh_session{
						     window_size =
						     WindowSize - PacketSize
						    }|Rest]
						 });
				Other ->
				    {reply, Other, State}
			    end
		    end;
		Size when Size =< WindowSize ->
		    case lists:member({error, closed}, Msgs) of
			true ->
			    {reply, {error, closed}, State};
			false ->
			    CompleteSize = 9 + Size,
			    case ssh_transport:send_msg(
				   State#ssh_connection.transport,
				   {CompleteSize,
				    [?SSH_MSG_CHANNEL_DATA,
				     <<SenderChannel:32/integer>>,
				     <<Size:32/integer>>, Msg]}) of
				ok ->
				    {reply,
				     ok,
				     State#ssh_connection{
				       sessions =
				       [Session#ssh_session{
					  window_size =
					  WindowSize - Size
					 }|Rest]
				      }
				    };
				Other ->
				    {reply, Other, State}
			    end
		    end;
		Size ->
		    case lists:member({error, closed}, Msgs) of
			true ->
			    {reply, {error, closed}, State};
			false ->
			    {noreply, State#ssh_connection{
					sessions =
					[Session#ssh_session{
					   sendq = {From, {Size, Msg}}}|Rest]}}
		    end
	    end;
	Other ->
	    {reply, no_such_session, State}
    end;
handle_call({active_once, Pid, RecipientChannel}, From, State) ->
    case listextract(fun(#ssh_session{recipient_channel = R})
			when R == RecipientChannel ->
			     true;
			(_) ->
			     false
		     end, State#ssh_connection.sessions) of
	{ok, Session = #ssh_session{window_size = WindowSize,
				    sender_channel = SenderChannel,
				    msgq = Msgs,
				    recvq = Count}, Rest} ->
	    case deliver_msgs(Pid, RecipientChannel, Count + 1, Msgs) of
		closed ->
		    {reply, ok, State#ssh_connection{sessions = Rest}};
		{NewCount, NewMsgs} ->
		    {reply, ok, State#ssh_connection{
				  sessions =
				  [handle_receive_window(Session#ssh_session{
							   msgq = NewMsgs,
							   recvq = NewCount
							  }, 0, State)|Rest]
				 }
		    }
	    end;
	Other ->
	    {reply, {error, no_such_session}}
    end;
handle_call({close_session, RecipientChannel},
	    From, State) ->
    case listextract(fun(#ssh_session{recipient_channel = R})
			when R == RecipientChannel ->
			     true;
			(_) ->
			     false
		     end, State#ssh_connection.sessions) of
	{ok, Session = #ssh_session{sender_channel = SenderChannel,
				    pending_close = Remote}, Rest}
	when Remote == remote ->
	    ssh_transport:send_msg(State#ssh_connection.transport,
				   [?SSH_MSG_CHANNEL_EOF,
				    <<SenderChannel:32/integer>>]),
	    ssh_transport:send_msg(State#ssh_connection.transport,
				   [?SSH_MSG_CHANNEL_CLOSE,
				    <<SenderChannel:32/integer>>]),
	    {reply,
	     ok,
	     State#ssh_connection{
	       sessions = Rest}
	    };
	{ok, Session = #ssh_session{sender_channel = SenderChannel}, Rest} ->
	    ssh_transport:send_msg(State#ssh_connection.transport,
				   [?SSH_MSG_CHANNEL_EOF,
				    <<SenderChannel:32/integer>>]),
	    ssh_transport:send_msg(State#ssh_connection.transport,
				   [?SSH_MSG_CHANNEL_CLOSE,
				    <<SenderChannel:32/integer>>]),
	    {noreply,
	     State#ssh_connection{
	       sessions = [Session#ssh_session{pending_close = From}|Rest]}
	    };
	Other ->
	    {reply, {error, no_such_session}, State}
    end;
handle_call(close, From, State) ->
    [begin
	 SenderChannel = X#ssh_session.sender_channel,
	 ssh_transport:send_msg(State#ssh_connection.transport,
				[?SSH_MSG_CHANNEL_EOF,
				 <<SenderChannel:32/integer>>]),
	 ssh_transport:send_msg(State#ssh_connection.transport,
				[?SSH_MSG_CHANNEL_CLOSE,
				 <<SenderChannel:32/integer>>])
     end || X <- State#ssh_connection.sessions],
    ssh_transport:close(State#ssh_connection.transport),
    {stop, normal, ok, State#ssh_connection{
			 transport = '_',
			 sessions = '_'}}.

handle_cast(print_state, State) ->
    io:format("~p, State = ~p~n", [{?MODULE, self()}, State]),
    {noreply, State}.

handle_info({ssh_transport, Transport,
	     <<?SSH_MSG_CHANNEL_OPEN_CONFIRMATION,
	      RecipientChannel:32/integer,
	      SenderChannel:32/integer,
	      InitialWindowSize:32/integer,
	      MaxPacketSize:32/integer>>}, State) ->
    case extract_session(RecipientChannel,
			 State#ssh_connection.sessions) of
	{ok,
	 Session = #ssh_session{recipient_channel = RecipientChannel,
				sender_channel = {subsystem, Subsystem}},
	 Rest} ->
	    NewSession = Session#ssh_session{
			   recipient_channel = RecipientChannel,
			   sender_channel = SenderChannel,
			   window_size = InitialWindowSize,
			   max_packet_size = MaxPacketSize},
	    do_start_subsystem(NewSession, Subsystem,
			       State#ssh_connection{sessions = Rest});
	{ok,
	 Session = #ssh_session{recipient_channel = RecipientChannel,
				sender_channel = {exec, Command}},
	 Rest} ->
	    NewSession = Session#ssh_session{
			   recipient_channel = RecipientChannel,
			   sender_channel = SenderChannel,
			   window_size = InitialWindowSize,
			   max_packet_size = MaxPacketSize},
	    do_start_exec(NewSession, Command,
			  State#ssh_connection{sessions = Rest});
	{ok,
	 Session = #ssh_session{recipient_channel = RecipientChannel,
				sender_channel = shell},
	 Rest} ->
	    NewSession = Session#ssh_session{
			   recipient_channel = RecipientChannel,
			   sender_channel = SenderChannel,
			   window_size = InitialWindowSize,
			   max_packet_size = MaxPacketSize},
	    do_start_shell(NewSession, State#ssh_connection{sessions = Rest});
	_ ->
	    ssh_transport:set_active_once(Transport),
	    {noreply, State}
    end;
handle_info({ssh_transport, Transport,
	     <<?SSH_MSG_CHANNEL_OPEN_FAILURE,
	      RecipientChannel:32/integer,
	      ReasonCode:32/integer,
	      TextualInformationLength:32/integer,
	      TextualInformation:TextualInformationLength/binary>>}, State) ->
    case extract_session(RecipientChannel,
			 State#ssh_connection.sessions) of
	{ok, Session = #ssh_session{recipient_channel = RecipientChannel,
				    owner = Owner}, Rest} ->
	    gen_server:reply(Owner, {error, ReasonCode, binary_to_list(TextualInformation)}),
	    ssh_transport:set_active_once(Transport),
	    {noreply, State#ssh_connection{sessions = Rest}};
	_ ->
	    ssh_transport:set_active_once(Transport),
	    {noreply, State}
    end;
handle_info({ssh_transport, Transport,
	     <<?SSH_MSG_CHANNEL_WINDOW_ADJUST, RecipientChannel:32/integer,
	      Amount:32/integer>>},
	    State) ->
    case extract_session(RecipientChannel,
			 State#ssh_connection.sessions) of
	{ok, Session = #ssh_session{window_size = OldWindowSize}, Rest} ->
	    ssh_transport:set_active_once(Transport),
	    NewSession = Session#ssh_session{
			   window_size = OldWindowSize + Amount},
	    case Session#ssh_session.sendq of
		[] ->
		    {noreply, State#ssh_connection{sessions =
						   [NewSession|Rest]}};
		{From, Msg} ->
		    case handle_call(
			   {send_msg, RecipientChannel, Msg},
			   From, State#ssh_connection{
				   sessions =
				   [NewSession#ssh_session{sendq = []}|Rest]
				  }) of
			{reply, Reply, NewState1} ->
			    gen_server:reply(From, Reply),
			    {noreply, NewState1};
			Other ->
			    Other
		    end
	    end;
	Other ->
	    io:format("When trying to find channel to adjust window_size: ~p~n", [Other]),
	    ssh_transport:set_active_once(Transport),
	    {noreply, State}
    end;
handle_info({ssh_transport, Transport,
	     <<?SSH_MSG_CHANNEL_SUCCESS, RecipientChannel:32/integer>>},
	    State) ->
    case extract_session(RecipientChannel,
			 State#ssh_connection.sessions) of
	{ok, Session = #ssh_session{owner = Owner}, Rest} ->
	    gen_server:reply(Owner,
			     {ok, Session#ssh_session.recipient_channel}),
	    ssh_transport:set_active_once(Transport),
	    {noreply, State#ssh_connection{sessions =
					   [Session|Rest]}};
	Other ->
	    io:format("When receiving channel_success message: ~p~n", [Other]),
	    ssh_transport:set_active_once(Transport),
	    {noreply, State}
    end;
handle_info({ssh_transport, Transport,
	     <<?SSH_MSG_CHANNEL_FAILURE, RecipientChannel:32/integer>>},
	    State) ->
    case extract_session(RecipientChannel,
			 State#ssh_connection.sessions) of
	{ok, Session = #ssh_session{owner = Owner}, Rest} ->
	    gen_server:reply(Owner, nok),
	    ssh_transport:set_active_once(Transport),
	    {noreply, State#ssh_connection{sessions =
					   [Session|Rest]}};
	Other ->
	    io:format("When receiving channel_failure message: ~p~n", [Other]),
	    ssh_transport:set_active_once(Transport),
	    {noreply, State}
    end;
handle_info({ssh_transport, Transport,
	     <<?SSH_MSG_CHANNEL_DATA, RecipientChannel:32/integer,
	      DataLen:32/integer, Data:DataLen/binary>>},
	    State) ->
    case extract_session(RecipientChannel,
			 State#ssh_connection.sessions) of
	%%	{ok, Session = #ssh_session{owner = Owner,
	%%				    recvq = Msgs}, Rest} when Msgs == [waiting] ->
	%%	    gen_server:reply(Owner, {ok, Data}),
	%%	    ssh_transport:set_active_once(Transport),
	%%	    {noreply, State#ssh_connection{
	%%		       sessions =
	%%		       [handle_receive_window(Session#ssh_session{
	%%						recvq = []},
	%%					      DataLen, State)|
	%%			Rest]}};
	{ok, Session = #ssh_session{owner_pid = Pid,
				    recvq = Count,
				    msgq = Msgs}, Rest} ->
	    case deliver_msgs(Pid, RecipientChannel, Count, Msgs ++
			      [Data]) of
		closed ->
		    ssh_transport:set_active_once(Transport),
		    {noreply, State#ssh_connection{
				sessions =
				Rest}};
		{NewCount, NewMsgs} ->
		    ssh_transport:set_active_once(Transport),
		    NewSession = Session#ssh_session{msgq = NewMsgs,
						     recvq = NewCount},
		    {noreply, State#ssh_connection{
				sessions =
				[handle_receive_window(NewSession,
						       DataLen, State)|Rest]}}
	    end;
	Other ->
	    io:format("ChannelData for non-existing channel: ~p~n",
		      [RecipientChannel]),
	    ssh_transport:set_active_once(Transport),
	    {noreply, State}
    end;
handle_info({ssh_transport, Transport,
	     <<?SSH_MSG_CHANNEL_EXTENDED_DATA, RecipientChannel:32/integer,
	      DataTypeCode:32/integer,
	      DataLen:32/integer, Data:DataLen/binary>>},
	    State) ->
    case extract_session(RecipientChannel,
			 State#ssh_connection.sessions) of
	{ok, Session = #ssh_session{owner_pid = Pid,
				    recvq = Count,
				    msgq = Msgs}, Rest} ->
	    case deliver_msgs(Pid, RecipientChannel, Count, Msgs ++
			      [{extended, Data}]) of
		closed ->
		    ssh_transport:set_active_once(Transport),
		    {noreply, State#ssh_connection{
				sessions =
				Rest}};
		{NewCount, NewMsgs} ->
		    ssh_transport:set_active_once(Transport),
		    NewSession = Session#ssh_session{msgq = NewMsgs,
						     recvq = NewCount},
		    {noreply, State#ssh_connection{
				sessions =
				[handle_receive_window(NewSession,
						       DataLen, State)|Rest]}}
	    end;
	Other ->
	    io:format("Extended ChannelData for non-existing channel: ~p~n",
		      [RecipientChannel]),
	    ssh_transport:set_active_once(Transport),
	    {noreply, State}
    end;
handle_info({ssh_transport, Transport,
	     <<?SSH_MSG_CHANNEL_CLOSE, RecipientChannel:32/integer>>},
	    State) ->
    case extract_session(RecipientChannel,
			 State#ssh_connection.sessions) of
	{ok, Session = #ssh_session{owner_pid = Pid,
				    recvq = Count,
				    msgq = Msgs,
				    pending_close = PendingClose}, Rest} ->
	    MsgsToDeliver =
		case PendingClose of
		    _ when tuple(PendingClose) ->
			gen_server:reply(PendingClose, ok),
			Msgs;
		    _ ->
			Msgs ++ [{error, closed}]
		end,
	    case deliver_msgs(Pid, RecipientChannel, Count,
			      MsgsToDeliver) of
		closed ->
		    ssh_transport:set_active_once(Transport),
		    {noreply, State#ssh_connection{sessions = Rest}};
		{NewCount, []} when tuple(PendingClose) ->
		    {noreply, State#ssh_connection{sessions =
						   Rest}};
		{0, NewMsgs} when tuple(PendingClose) ->
		    ssh_transport:set_active_once(Transport),
		    {noreply, State#ssh_connection{sessions =
						   [Session#ssh_session{
						      msgq = NewMsgs,
						      pending_close = closed}|
						    Rest]}};
		{0, NewMsgs} ->
		    ssh_transport:set_active_once(Transport),
		    {noreply, State#ssh_connection{sessions =
						   [Session#ssh_session{
						      msgq = NewMsgs,
						      pending_close = remote}|
						    Rest]}}
	    end;
	Other -> %% Already closed.
	    ssh_transport:set_active_once(Transport),
	    {noreply, State}
    end;
handle_info({ssh_transport, Transport,
	     <<?SSH_MSG_CHANNEL_REQUEST, RecipientChannel:32/integer,
	      11:32/integer, "exit-status",
	      0, %FALSE
	      ExitStatus:32/integer>>},
	    State) ->
    case ExitStatus of
	0 ->
	    ok;
	_ ->
	    io:format("~p: Received exit-status ~p on channel ~p~n",
		      [{?MODULE, self()}, ExitStatus, RecipientChannel])
    end,
    ssh_transport:set_active_once(Transport),
    {noreply, State};
handle_info({ssh_transport, Transport,
	     <<?SSH_MSG_CHANNEL_REQUEST, RecipientChannel:32/integer,
	      11:32/integer, "exit-signal",
	      0,  %FALSE
	      SignalStrSize:32/integer, SignalStr:SignalStrSize/binary,
	      CoreDumped, % Boolean
	      ErrorMsgSize:32/integer, ErrorMsg:ErrorMsgSize/binary,
	      LanguageTagSize:32/integer, LanguageTag:LanguageTagSize/binary>>},
	    State) ->
    io:format("~p: Received exit-signal ~p on channel ~p~n\tmsg: ~p",
	      [{?MODULE, self()}, binary_to_list(SignalStr), RecipientChannel,
	       binary_to_list(ErrorMsg)]),
    ssh_transport:set_active_once(Transport),
    {noreply, State};  %%%FIXME
handle_info({ssh_transport, Transport,
	     <<?SSH_MSG_CHANNEL_EOF, _/binary>>}, State) ->
    ssh_transport:set_active_once(Transport),
    {noreply, State};
handle_info({ssh_transport, Transport, Msg}, State) ->
    ssh_transport:set_active_once(Transport),
    io:format("Got ~p from ~p~n", [Msg, Transport]), %%%FIXME
    {noreply, State};
handle_info({ssh_transportError, Transport, Reason}, State) ->
    handle_info({'EXIT', Transport, Reason}, State);
handle_info({'EXIT', Pid, Reason},
	    State = #ssh_connection{transport = Transport})
  when Pid == Transport ->
    {stop, Reason, #ssh_connection{transport = '_',
				   sessions = []}}.

terminate(Why, State) ->
    {ok, State}.

code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% #             start_session
%%%
%%% Input:
%%% Output:
%%%
%%% Description:
%%%----------------------------------------------------------------------

start_session(exec, RecipientChannel, From, Command, OwnerPid, State) ->
    case ssh_transport:send_msg(State#ssh_connection.transport,
				[?SSH_MSG_CHANNEL_OPEN,
				 ?SSH_STRING("session"),
				 ?SSH_UINT_32(RecipientChannel),
				 ?SSH_UINT_32(1 bsl 17),
				 ?SSH_UINT_32(1 bsl 15)]) of
	ok ->
	    ssh_transport:set_active_once(State#ssh_connection.transport),
	    ssh_transport:set_active_once(State#ssh_connection.transport),
	    {noreply, State#ssh_connection{
			sessions = [#ssh_session{recipient_channel = RecipientChannel,
						 sender_channel =
						 {exec, Command},
						 owner = From,
						 owner_pid = OwnerPid,
						 recv_window = 1 bsl 17,
						 recv_packet_size = 1 bsl 15}|
				    State#ssh_connection.sessions]}};
	Other ->
	    {reply, Other, State}
    end;
start_session(subsystem, RecipientChannel, From, Subsystem, OwnerPid, State) ->
    case ssh_transport:send_msg(State#ssh_connection.transport,
				[?SSH_MSG_CHANNEL_OPEN,
				 ?SSH_STRING("session"),
				 ?SSH_UINT_32(RecipientChannel),
				 ?SSH_UINT_32(1 bsl 17),
				 ?SSH_UINT_32(1 bsl 15)]) of
	ok ->
	    ssh_transport:set_active_once(State#ssh_connection.transport),
	    ssh_transport:set_active_once(State#ssh_connection.transport),
	    {noreply, State#ssh_connection{
			sessions = [#ssh_session{recipient_channel =
						 RecipientChannel,
						 sender_channel =
						 {subsystem, Subsystem},
						 owner = From,
						 owner_pid = OwnerPid,
						 recv_window = 1 bsl 17,
						 recv_packet_size = 1 bsl 15}|
				    State#ssh_connection.sessions]}};
	Other ->
	    {reply, Other, State}
    end.

start_session(shell, RecipientChannel, From, OwnerPid, State) ->
    case ssh_transport:send_msg(State#ssh_connection.transport,
				[?SSH_MSG_CHANNEL_OPEN,
				 ?SSH_STRING("session"),
				 ?SSH_UINT_32(RecipientChannel),
				 ?SSH_UINT_32(1 bsl 17),
				 ?SSH_UINT_32(1 bsl 15)]) of
	ok ->
	    ssh_transport:set_active_once(State#ssh_connection.transport),
	    ssh_transport:set_active_once(State#ssh_connection.transport),
	    {noreply, State#ssh_connection{
			sessions = [#ssh_session{recipient_channel = RecipientChannel,
						 sender_channel = shell,
						 owner = From,
						 owner_pid = OwnerPid,
						 recv_window = 1 bsl 17,
						 recv_packet_size = 1 bsl 15}|
				    State#ssh_connection.sessions]}};
	Other ->
	    {reply, Other, State}
    end.

do_start_subsystem(Session, Subsystem, State) ->
    case ssh_transport:send_msg(
	   State#ssh_connection.transport,
	   [?SSH_MSG_CHANNEL_REQUEST,
	    ?SSH_UINT_32(Session#ssh_session.sender_channel),
	    ?SSH_STRING("subsystem"),
	    ?SSH_TRUE,
	    ?SSH_STRING(Subsystem)]) of
	ok ->
	    ssh_transport:set_active_once(State#ssh_connection.transport),
	    {noreply,
	     State#ssh_connection{
	       sessions = [Session|State#ssh_connection.sessions]}};
	Other ->
	    gen_server:reply(Session#ssh_session.owner, {error, Other}),
	    {noreply, State}
    end.

do_start_exec(Session, Command, State) ->
    case ssh_transport:send_msg(
	   State#ssh_connection.transport,
	   [?SSH_MSG_CHANNEL_REQUEST,
	    ?SSH_UINT_32(Session#ssh_session.sender_channel),
	    ?SSH_STRING("exec"),
	    ?SSH_TRUE,
	    ?SSH_STRING(Command)]) of
	ok ->
	    ssh_transport:set_active_once(State#ssh_connection.transport),
	    {noreply,
	     State#ssh_connection{
	       sessions = [Session|State#ssh_connection.sessions]}};
	Other ->
	    gen_server:reply(Session#ssh_session.owner, {error, Other}),
	    {noreply, State}
    end.

do_start_shell(Session, State) ->
    case ssh_transport:send_msg(
	   State#ssh_connection.transport,
	   [?SSH_MSG_CHANNEL_REQUEST,
	    ?SSH_UINT_32(Session#ssh_session.sender_channel),
	    ?SSH_STRING("shell"),
	    ?SSH_TRUE]) of
	ok ->
	    ssh_transport:set_active_once(State#ssh_connection.transport),
	    {noreply,
	     State#ssh_connection{
	       sessions = [Session|State#ssh_connection.sessions]}};
	Other ->
	    gen_server:reply(Session#ssh_session.owner, {error, Other}),
	    {noreply, State}
    end.

handle_receive_window(Session, DataLen, State) ->
    OldReceiveWindow = Session#ssh_session.recv_window,
    Msgs = Session#ssh_session.msgq,
    case Msgs of
	Msgs when length(Msgs) > 4 ->
	    Session#ssh_session{recv_window = OldReceiveWindow - DataLen};
	_ ->
	    case OldReceiveWindow - DataLen of
		NewWindow when NewWindow > (1 bsl 16) ->
		    Session#ssh_session{recv_window = NewWindow};
		NewWindow ->
		    Incr = (1 bsl 17) - NewWindow,
		    ssh_transport:send_msg(
		      State#ssh_connection.transport,
		      [?SSH_MSG_CHANNEL_WINDOW_ADJUST,
		       <<(Session#ssh_session.sender_channel):32/integer,
			Incr:32/integer>>]),
		    Session#ssh_session{recv_window = NewWindow + Incr}
	    end
    end.

extract_session(RecipientChannel, Sessions) ->
    listextract(fun(#ssh_session{recipient_channel = R})
		   when R == RecipientChannel ->
			true;
		   (_) ->
			false
		end, Sessions).

listextract(Fun, List) ->
    listextract(Fun, List, []).

listextract(Fun, [], Acc) ->
    {error, not_found};
listextract(Fun, [Session|Sessions], Acc) ->
    case Fun(Session) of
	true ->
	    {ok, Session, lists:reverse(Acc) ++ Sessions};
	false ->
	    listextract(Fun, Sessions, [Session|Acc])
    end.

deliver_msgs(Owner, RecipientChannel, 0, Msgs) ->
    {0, Msgs};
deliver_msgs(Owner, RecipientChannel, Count, []) ->
    {Count, []};
deliver_msgs(Owner, RecipientChannel, Count, [{error, closed}]) ->
    Owner ! {ssh_connection_error, self(), RecipientChannel, closed},
    closed;
deliver_msgs(Owner, RecipientChannel, Count, [Msg|Msgs]) ->
    Owner ! {ssh_connection, self(), RecipientChannel, Msg},
    deliver_msgs(Owner, RecipientChannel, Count - 1, Msgs).
