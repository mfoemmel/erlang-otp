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
-module(inet6_tcp_dist).

%% Handles the connection setup phase with other Erlang nodes.

-export([listen/0,accept/1,accept_connection/6,
	 setup/5, close/1, reg/2, select/1]).

%% internal exports

-export([accept_loop/2, do_accept/7, do_setup/6, setup_timer/2]).

-import(error_logger,[error_msg/2]).

-include("net_address.hrl").

-define(to_port(Socket, Data),
	case inet6_tcp:send(Socket, Data) of
	    {error, closed} ->
		self() ! {tcp_closed, Socket},
	        {error, closed};
	    R ->
	        R
        end).


-define(int16(X), [((X) bsr 8) band 16#ff, (X) band 16#ff]).

-define(int32(X), 
	[((X) bsr 24) band 16#ff, ((X) bsr 16) band 16#ff,
	 ((X) bsr 8) band 16#ff, (X) band 16#ff]).

-define(i16(X1,X0),
        (?u16(X1,X0) - 
         (if (X1) > 127 -> 16#10000; true -> 0 end))).

-define(u16(X1,X0),
        (((X1) bsl 8) bor (X0))).

-record(tick, {read = 0,
	       write = 0,
	       tick = 0,
	       ticked = 0
	       }).

%% ------------------------------------------------------------
%%  Select this protocol based on node name
%%  select(Node) => Bool
%% ------------------------------------------------------------

select(Node) ->
    case split_node(atom_to_list(Node), $@, []) of
	[_, Host] ->
	    case inet:getaddr(Host,inet6) of
		{ok,_} -> true;
		_ -> false
	    end;
	_ -> false
    end.

%% ------------------------------------------------------------
%% Register the node with epmd
%% ------------------------------------------------------------

reg(Name, Address) ->
    {_,Port} = Address#net_address.address,
    erl_epmd:register_node(Name, Port).

%% ------------------------------------------------------------
%% Create the listen socket, i.e. the port that this erlang
%% node is accessible through.
%% ------------------------------------------------------------

listen() ->
    case inet6_tcp:listen(0, [{active, false}]) of
	{ok, Socket} ->
	    TcpAddress = get_tcp_address(Socket),
	    {ok, {Socket, TcpAddress}};
	Error ->
	    Error
    end.

%% ------------------------------------------------------------
%% Accepts new connection attempts from other Erlang nodes.
%% ------------------------------------------------------------

accept(Listen) ->
    spawn_link(?MODULE, accept_loop, [self(), Listen]).

accept_loop(Kernel, Listen) ->
    case inet6_tcp:accept(Listen) of
	{ok, Socket} ->
	    Kernel ! {accept, self(), Socket, tcp},
	    controller(Kernel, Socket),
	    accept_loop(Kernel, Listen);
	Error ->
	    exit(Error)
    end.

controller(Kernel, Socket) ->
    receive
	{Kernel, controller, Pid} ->
	    flush_controller(Pid, Socket),
	    inet6_tcp:controlling_process(Socket, Pid),
	    flush_controller(Pid, Socket),
	    Pid ! {self(), controller};
	{Kernel, unsupported_protocol} ->
	    exit(unsupported_protocol)
    end.

flush_controller(Pid, Socket) ->
    receive
	{tcp, Socket, Data} ->
	    Pid ! {tcp, Socket, Data},
	    flush_controller(Pid, Socket);
	{tcp_closed, Socket} ->
	    Pid ! {tcp_closed, Socket},
	    flush_controller(Pid, Socket)
    after 0 ->
	    ok
    end.

%% ------------------------------------------------------------
%% Accepts a new connection attempt from another Erlang node.
%% Performs the handshake with the other side.
%% ------------------------------------------------------------

accept_connection(AcceptPid, Socket, Cookie, MyNode, Allowed, SetupTime) ->
    spawn_link(?MODULE, do_accept,
	       [self(), AcceptPid, Socket, Cookie, MyNode,
		Allowed, SetupTime]).

do_accept(Kernel, AcceptPid, Socket, Cookie, MyNode, Allowed, SetupTime) ->
    receive
	{AcceptPid, controller} ->
	    Timer = start_timer(SetupTime),
	    shakehand(Kernel, Socket, Cookie, MyNode, Allowed, Timer)
    end.

shakehand(Kernel, Socket, MyInCookie, MyNode, Allowed, Timer) ->
    case inet6_tcp:recv(Socket, 2) of
	{ok, [L1,L0]} ->
	    Length = ?i16(L1,L0),
	    case inet6_tcp:recv(Socket, Length) of
		{ok, Data} ->
		    {Node, Type} = get_name(Data, Socket),
		    Address = get_remote_id(Socket, Node),
		    mark_pending(Kernel, Node, Type, Socket, Address,
				 MyInCookie, MyNode, Allowed, Type,
				 Timer);
		_ ->
		    shutdown()
	    end;
	_ ->
	    shutdown()
    end.

%%
%% No nodedown will be sent if we fail before this process has
%% succeeded to mark the node as pending !
%%
mark_pending(Kernel, Node, Type, Socket, Address, MyInCookie,
	     MyNode, Allowed, Type, Timer) ->
    case lists:member(Node, Allowed) of
	false when Allowed /= [] ->
	    error_msg("** Connection attempt from "
		      "disallowed node ~w ** ~n", [Node]),
	    %% tell_name needed to be backward compatible !
	    catch tell_name(Socket, MyNode), 
	    shutdown(); 
	_ ->
	    mark_pending(Kernel, Node, Type, Socket, Address,
			 MyInCookie, MyNode, Type, Timer)
    end.

mark_pending(Kernel, Node, Type, Socket, Address, MyInCookie,
	     MyNode, Type, Timer) ->
    case do_mark_pending(Kernel,Node,Address,Type) of
	ok ->
	    reset_timer(Timer),
	    case catch tell_name(Socket, MyNode) of
		ok ->
		    connection(Kernel, Node, Socket, Address,
			       {MyInCookie, MyInCookie}, MyNode,
			       Type, Timer);
		_ ->
		    shutdown()
	    end;
	{pending, SetupPid} ->
	    simultan_cnct(Kernel, Socket, Address, SetupPid,
			  Node, MyInCookie, MyNode, Type, Timer);
	up ->
	    %% If our socket already is closed this was a
	    %% simultaneous attempt there the accept process
	    %% at the other side killed the setup process !!
	    case is_closed(Socket) of
		true ->
		    shutdown();
		_ ->
		    disconnect_old(Node),
		    %% Try again !
		    mark_pending(Kernel, Node, Type, Socket, Address,
				 MyInCookie, MyNode, Type, Timer)
	    end
    end.

%%
%% We are sure that the other side has not told us anything as
%% we have not told our name yet ! Thus, the inet_tcp:recv call
%% just tells us if the socket as closed.
%% Have to do this stuff as the socket is in passive mode, i.e.
%% the inet_drv.c has no READ select on it !!
%%
is_closed(Socket) ->
    inet6_tcp:recv(Socket, 0, 100),
    receive
	{tcp_closed, Socket} ->
	    true
    after 0 ->
	    false
    end.

disconnect_old(Node) ->
    erlang:monitor_node(Node, true),
    net_kernel:disconnect(Node),
    receive
	{nodedown, Node} -> ok
    end.

do_mark_pending(Kernel,Node,Address,Type) ->
    Kernel ! {self(), {accept_pending,Node,Address,Type}},
    receive
	{Kernel, {accept_pending, Ret}} ->
	    Ret
    end.

simultan_cnct(_, _, _, _, Node, _, MyNode, _, _) when MyNode > Node ->
    shutdown();
simultan_cnct(Kernel, Socket, Address, SetupPid, Node,
	      InCookie, MyNode, Type, Timer) ->
    case mark_new_pending(Kernel, Node) of
	ok ->
	    exit(SetupPid, shutdown),
	    reset_timer(Timer),
	    case catch tell_name(Socket, MyNode) of
		ok ->
		    connection(Kernel, Node, Socket, Address,
			       {InCookie, InCookie}, MyNode, Type, Timer);
		_ ->
		    shutdown()
	    end;
	bad_request ->
	    %% Can not occur !!
	    error_msg("net_connect: ** Simultaneous connect failed for ~p~n",
		      [Node]),
	    shutdown()
    end.

mark_new_pending(Kernel, Node) ->
    Kernel ! {self(), {remark_pending, Node}},
    receive
	{Kernel, {remark_pending, Resp}} ->
	    Resp
    end.

%% ------------------------------------------------------------
%% Get remote information about a Socket.
%% ------------------------------------------------------------
get_remote_id(Socket, Node) ->
    {ok, Address} = inet:peername(Socket),
    [_, Host] = split_node(atom_to_list(Node), $@, []),
    #net_address{
		 address = Address,
		 host = Host,
		 protocol = tcp,
		 family = inet6 }.

%% ------------------------------------------------------------
%% Setup a new connection to another Erlang node.
%% Performs the handshake with the other side.
%% ------------------------------------------------------------

setup(Node,Cookie,MyNode,LongOrShortNames,SetupTime) ->
    spawn_link(?MODULE, do_setup, [self(),
				   Node,
				   Cookie,
				   MyNode,
				   LongOrShortNames,
				   SetupTime]).

do_setup(Kernel,Node,Cookie,MyNode,LongOrShortNames,SetupTime) ->
    [Name, Address] = splitnode(Node, LongOrShortNames),
    case inet:getaddr(Address, inet6) of
	{ok, Ip} ->
	    Timer = start_timer(SetupTime),
	    case erl_epmd:port_please(Name, Ip) of
		noport ->
		    shutdown();
		{port, TcpPort} ->
		    reset_timer(Timer),
		    case inet6_tcp:connect(Ip, TcpPort, [{active, false}]) of
			{ok, Socket} ->
			    handshake(Kernel, Node, Socket,
				      #net_address {
						    address = {Ip,TcpPort},
						    host = Address,
						    protocol = tcp,
						    family = inet6},
				      Cookie,
				      MyNode,
				      Timer);
			_ ->
			    %% Other Node may have closed since port_please !
			    shutdown()
		    end
	    end;
	Other ->
	    shutdown()
    end.

%%
%% This will tell the net_kernel about the nodedown as it
%% recognizes the exit signal.
%% Terminate with reason shutdown so inet processes want
%% generate crash reports.
%% The termination of this process does also imply that the Socket
%% is closed in a controlled way by inet_drv.
%%
shutdown() ->
    flush_down(),
    exit(shutdown).

flush_down() ->
    receive
	{From, get_status} ->
	    From ! {self(), get_status, error},
	    flush_down()
    after 0 ->
	    ok
    end.

%% If Node is illegal terminate the connection setup!!
splitnode(Node, LongOrShortNames) ->
    case split_node(atom_to_list(Node), $@, []) of
	[Name|Tail] when Tail /= [] ->
	    Host = lists:append(Tail),
	    case split_node(Host, $., []) of
		[_] when LongOrShortNames == longnames ->
		    error_msg("** System running to use fully qualified "
			      "hostnames **~n"
			      "** Hostname ~s is illegal **~n",
			      [Host]),
		    shutdown();
		L when length(L) > 1, LongOrShortNames == shortnames ->
		    error_msg("** System NOT running to use fully qualified "
			      "hostnames **~n"
			      "** Hostname ~s is illegal **~n",
			      [Host]),
		    shutdown();
		_ ->
		    [Name, Host]
	    end;
	[_] ->
	    error_msg("** Nodename ~p illegal, no '@' character **~n",
		      [Node]),
	    shutdown();
	_ ->
	    error_msg("** Nodename ~p illegal **~n", [Node]),
	    shutdown()
    end.

split_node([Chr|T], Chr, Ack) -> [lists:reverse(Ack)|split_node(T, Chr, [])];
split_node([H|T], Chr, Ack)   -> split_node(T, Chr, [H|Ack]);
split_node([], _, Ack)        -> [lists:reverse(Ack)].

handshake(Kernel, Node, Socket, TcpAddress, Cookie, MyNode, Timer) ->
    SessionCookie = Cookie,
    reset_timer(Timer),
    case catch tell_name(Socket, MyNode) of
	ok ->
	    handshake1(Kernel, Node, Socket, TcpAddress,
		       {SessionCookie, Cookie}, MyNode, Timer);
	_ ->
	    wait_for_acceptpid()
    end.

handshake1(Kernel, Node, Socket, TcpAddress, {InCookie, OldOutCookie},
	   MyNode, Timer) ->
  reset_timer(Timer),
  case inet6_tcp:recv(Socket, 2) of
    {ok, [L1,L0]} ->
      Length = ?i16(L1,L0),
      case inet6_tcp:recv(Socket, Length) of
	{ok, Data} ->
	  {Node, Type} = get_name(Data, Socket), %% crash if badnode
	  connection(Kernel, Node, Socket, TcpAddress,
		     {InCookie, OldOutCookie}, MyNode, Type,
		     Timer);
	_ ->
	  wait_for_acceptpid()
      end;
    _ ->
      wait_for_acceptpid()
  end.

%% Handle simultaneous connection attempt!
%% We must keep this process either until the accept process
%% kills us (it has succeeded to remark itself as pending)
%% or until the setup timer process triggers.
%% This is needed if we simultaneously connects to a node
%% running the old tcp_drv.c as it may close the connection
%% immediately (checks the IP address instead of nodename)
%% and we do not want to generate a nodedown yet.
wait_for_acceptpid() ->
    receive after infinity -> shutdown() end.

%% --------------------------------------------------------------
%% The connection has been established.
%% --------------------------------------------------------------

connection(Kernel, Node, Socket, TcpAddress, Cookies,
	   MyNode, Type, Timer) ->
    cancel_timer(Timer),
    case do_setnode(Node, Socket, Type) of
	error ->
	    shutdown();
	ok ->
	    case inet:setopts(Socket, [{active, true},
				       {packet, 4},
				       {nodelay, true}]) of
		ok ->
		    mark_nodeup(Kernel,Node,TcpAddress,Cookies,Type),
		    {InCookie,OutCookie} = Cookies,
		    con_loop(Kernel, Node, Socket, TcpAddress, InCookie,
			     OutCookie, MyNode, Type, #tick{});
		_ ->
		    shutdown()
	    end
    end.

do_setnode(Node, Socket, Type) ->
    case inet:getll(Socket) of
	{ok,Port} ->
	    erlang:setnode(Node, Port, Type),
	    ok;
	_ ->
	    error
    end.

mark_nodeup(Kernel,Node,Address,Cookies,Type) ->
    Kernel ! {self(), {nodeup,Node,Address,Cookies,Type}},
    receive
	{Kernel, inserted} ->
	    ok;
	{Kernel, bad_request} ->
	    error_msg("Uuugh, we were not allowed to send {nodeup, ~p} !!~n",
		      [Node]),
	    shutdown()
    end.

con_loop(Kernel, Node, Socket, TcpAddress, InCookie, OutCookie,
	 MyNode, Type, Tick) ->
    receive
	{tcp_closed, Socket} ->
	    shutdown();
	{Kernel, disconnect} ->
	    shutdown();
	{Kernel, tick} ->
	    case send_tick(Socket, Tick, Type) of
		{ok, NewTick} ->
		    con_loop(Kernel, Node, Socket, TcpAddress, InCookie,
			     OutCookie, MyNode, Type, NewTick);
		{error, not_responding} ->
		    error_msg("** Node ~p not responding **~n"
			      "** Removing (timedout) connection **~n",
			      [Node]),
		    shutdown();
		_ ->
		    shutdown()
	    end;
	{From, get_status} ->
	    case getstat(Socket) of
		{ok, Read, Write, _} ->
		    From ! {self(), get_status, {ok, Read, Write}},
		    con_loop(Kernel, Node, Socket, TcpAddress, InCookie,
			     OutCookie, MyNode, Type, Tick);
		_ ->
		    shutdown()
	    end
    end.

%% ------------------------------------------------------------
%% Fetch local information about a Socket.
%% ------------------------------------------------------------
get_tcp_address(Socket) ->
    {ok, Address} = inet:sockname(Socket),
    {ok, Host} = inet:gethostname(),
    #net_address {
		  address = Address,
		  host = Host,
		  protocol = tcp,
		  family = inet6 
		 }.

%% ------------------------------------------------------------
%% Misc. functions.
%% ------------------------------------------------------------

%%
%% Get the name of the other side.
%% Close the connection if invalid data.
%% The IP address sent is not interesting (as in the old
%% tcp_drv.c which used it to detect simultaneous connection
%% attempts).
%%
get_name([$M,_Ip1,_Ip2,_Ip3,_Ip4|OtherNode], Socket) ->
    {list_to_atom(OtherNode), normal};
get_name([$H,_Ip1,_Ip2,_Ip3,_Ip4|OtherNode], Socket) ->
    {list_to_atom(OtherNode), hidden};
get_name(_, Socket) ->
    shutdown().

%%
%% Tell our name to the other side.
%% We have to send a correct IP address in case we
%% connects to an old Erlang node (running old tcp_drv.c).
%%
tell_name(Socket, MyNode0) ->
    MyNode = atom_to_list(MyNode0),
    L = ?int16(length(MyNode) + 5),
    %% write 0,0,0,0 as address receriver will ignore it anyway
    ?to_port(Socket, L ++ [$M,0,0,0,0] ++ MyNode),
    ok.

%%
%% Send a TICK to the other side.
%%
%% This will happen every 15 seconds (by default) 
%% The idea here is that everey 15 secs, we write a little 
%% something on the connection if we haven't written anything for 
%% the last 15 secs.
%% This will ensure that nodes that are not responding due to 
%% hardware errors (Or being suspended by means of ^Z) will 
%% be considered to be down. If we do not want to have this  
%% we must start the net_kernel (in erlang) without its 
%% ticker process, In that case this code will never run 

%% And then every 60 seconds we also check the connection and 
%% close it if we havn't received anything on it for the 
%% last 60 secs. If ticked == tick we havn't received anything 
%% on the connection the last 60 secs. 

%% The detection time interval is thus, by default, 45s < DT < 75s 

%% A HIDDEN node is always (if not a pending write) ticked if 
%% we haven't read anything as a hidden node only ticks then it receives 
%% a TICK !! 
	
send_tick(Socket, Tick, Type) ->
    #tick{tick = T0,
	  read = Read,
	  write = Write,
	  ticked = Ticked} = Tick,
    T = T0 + 1,
    T1 = T rem 4,
    case getstat(Socket) of
	{ok, Read, _, _} when  Ticked == T ->
	    {error, not_responding};
	{ok, Read, W, Pend} when Type == hidden ->
	    send_tick(Socket, Pend),
	    {ok, Tick#tick{write = W + 1,
			   tick = T1}};
	{ok, Read, Write, Pend} ->
	    send_tick(Socket, Pend),
	    {ok, Tick#tick{write = Write + 1,
			   tick = T1}};
	{ok, R, Write, Pend} ->
	    send_tick(Socket, Pend),
	    {ok, Tick#tick{write = Write + 1,
			   read = R,
			   tick = T1,
			   ticked = T}};
	{ok, Read, W, _} ->
	    {ok, Tick#tick{write = W,
			   tick = T1}};
	{ok, R, W, _} ->
	    {ok, Tick#tick{write = W,
			   read = R,
			   tick = T1,
			   ticked = T}};
	Error ->
	    Error
    end.


send_tick(Socket, 0) ->
    ?to_port(Socket, []);
send_tick(_, Pend) ->
    %% Dont send tick if pending write.
    ok.

getstat(Socket) ->
    case inet:getstat(Socket, [recv_cnt, send_cnt, send_pend]) of
	{ok, Stat} ->
	    split_stat(Stat,0,0,0);
	Error ->
	    Error
    end.

split_stat([{recv_cnt, R}|Stat], _, W, P) ->
    split_stat(Stat, R, W, P);
split_stat([{send_cnt, W}|Stat], R, _, P) ->
    split_stat(Stat, R, W, P);
split_stat([{send_pend, P}|Stat], R, W, _) ->
    split_stat(Stat, R, W, P);
split_stat([], R, W, P) ->
    {ok, R, W, P}.

%%
%% Close a socket.
%%
close(Socket) ->
    inet6_tcp:close(Socket).

%% ------------------------------------------------------------
%% Connection setup timeout timer.
%% After Timeout milliseconds this process terminates
%% which implies that the owning setup/accept process terminates.
%% The timer is reset before every network operation during the
%% connection setup !
%% ------------------------------------------------------------

start_timer(Timeout) ->
    spawn_link(?MODULE, setup_timer, [self(), Timeout]).

setup_timer(Pid, Timeout) ->
    receive
	{Pid, reset} ->
	    setup_timer(Pid, Timeout)
    after Timeout ->
	    exit(shutdown)
    end.

reset_timer(Timer) ->
    Timer ! {self(), reset}.

cancel_timer(Timer) ->
    unlink(Timer),
    exit(Timer, shutdown).

