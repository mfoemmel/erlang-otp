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
-module(erl_sim_tcp_dist).

%% Handles the connection setup phase with other Erlang nodes.
%% Handles the connection completely in Erlang.  argument: -sim_dist
%% [debug cache]


-export([listen/0, accept/1, accept_connection/6,
	 setup/5, close/1, reg/2, select/1]).

%% internal exports

-export([accept_loop/2,do_accept/7,do_setup/6,setup_timer/2]).

-import(error_logger,[error_msg/2]).

-include("net_address.hrl").

-define(shutdown(Data), shutdown(?LINE, Data)).
-define(shutdown_pid(Pid, Data), shutdown_pid(?LINE, Pid, Data)).

-define(to_port(Socket, Data),
	case inet_tcp:send(Socket, Data) of
	    {error, closed} ->
		self() ! {tcp_closed, Socket},
	        {error, closed};
	    R ->
	        R
        end).

-define(DOP_LINK, 1).
-define(DOP_SEND, 2).
-define(DOP_EXIT, 3).
-define(DOP_UNLINK, 4).
-define(DOP_NODE_LINK, 5).
-define(DOP_REG_SEND, 6).
-define(DOP_GROUP_LEADER, 7).
-define(DOP_EXIT2, 8).

-define(PASS_THROUGH, $p).

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
	    case inet:getaddr(Host,inet) of
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
    case inet_tcp:listen(0, [{active, false}]) of
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
    process_flag(priority, max),
    case inet_tcp:accept(Listen) of
	{ok, Socket} ->
	    Kernel ! {accept,self(),Socket,inet,tcp},
	    controller(Kernel, Socket),
	    accept_loop(Kernel, Listen);
	Error ->
	    exit(Error)
    end.

controller(Kernel, Socket) ->
    receive
	{Kernel, controller, Pid} ->
	    flush_controller(Pid, Socket),
	    inet_tcp:controlling_process(Socket, Pid),
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
    process_flag(priority, max),
    receive
	{AcceptPid, controller} ->
	    Timer = start_timer(SetupTime),
	    shakehand(Kernel, Socket, Cookie, MyNode, Allowed, Timer)
    end.

shakehand(Kernel, Socket, MyInCookie, MyNode, Allowed, Timer) ->
    case inet_tcp:recv(Socket, 2) of
	{ok, [L1,L0]} ->
	    Length = ?i16(L1,L0),
	    case inet_tcp:recv(Socket, Length) of
		{ok, Data} ->
		    {Node, Type, Version} = get_name(Data, Socket),
		    Address = get_remote_id(Socket, Node),
		    mark_pending(Kernel, Node, Type, Socket, Address,
				 MyInCookie, MyNode, Allowed, Type,
				 Version, Timer);
		_ ->
		    ?shutdown(no_node)
	    end;
	_ ->
	    ?shutdown(no_node)
    end.

%%
%% No nodedown will be sent if we fail before this process has
%% succeeded to mark the node as pending !
%%
mark_pending(Kernel, Node, Type, Socket, Address, MyInCookie,
	     MyNode, Allowed, Type, Version, Timer) ->
    case lists:member(Node, Allowed) of
	false when Allowed /= [] ->
	    error_msg("** Connection attempt from "
		      "disallowed node ~w ** ~n", [Node]),
	    %% tell_name needed to be backward compatible !
	    catch tell_name(Socket, MyNode, Version), 
	    ?shutdown(Node); 
	_ ->
	    mark_pending(Kernel, Node, Type, Socket, Address,
			 MyInCookie, MyNode, Type, Version, Timer)
    end.

mark_pending(Kernel, Node, Type, Socket, Address, MyInCookie,
	     MyNode, Type, Version, Timer) ->
    case do_mark_pending(Kernel,Node,Address,Type) of
	ok ->
	    reset_timer(Timer),
	    case catch tell_name(Socket, MyNode, Version) of
		ok ->
		    connection(Kernel, Node, Socket, Address,
			       {MyInCookie, MyInCookie}, MyNode,
			       Type, Version, Timer);
		_ ->
		    ?shutdown(Node)
	    end;
	{pending, SetupPid} ->
	    simultan_cnct(Kernel, Socket, Address, SetupPid,
			  Node, MyInCookie, MyNode, Type, Version, Timer);
	up ->
	    %% If our socket already is closed this was a
	    %% simultaneous attempt where the accept process
	    %% at the other side killed the setup process !!
	    case is_closed(Socket) of
		true ->
		    ?shutdown(Node);
		_ ->
		    %% This can happen if the other node goes down,
		    %% and goes up again and contact us before we have
		    %% detected that the socket was closed.  Force the
		    %% cleanup here.
		    disconnect_old(Node),
		    %% Try again !
		    mark_pending(Kernel, Node, Type, Socket, Address,
				 MyInCookie, MyNode, Type, Version, Timer)
	    end
    end.

%%
%% We are sure that the other side has not told us anything as
%% we have not told our name yet ! Thus, the inet_tcp:recv call
%% just tells us if the socket is closed.
%% Have to do this stuff as the socket is in passive mode, i.e.
%% the inet_drv.c has no READ select on it !!
%%
is_closed(Socket) ->
    %% Uack!  What if the other end is slow and doesn't close the socket
    %% within 100 ms?  Unfortunatley, this one needs a protocol change -
    %% We need an explicit message to wait for instead of rely on the
    %% timeout.
    case inet_tcp:recv(Socket, 0, 100) of
	{error, _} -> true;
	_ -> false
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

simultan_cnct(_, _, _, _, Node, _, MyNode, _, _, _) when MyNode > Node ->
    ?shutdown(Node);
simultan_cnct(Kernel, Socket, Address, SetupPid, Node,
	      InCookie, MyNode, Type, Version, Timer) ->
    case mark_new_pending(Kernel, Node) of
	ok ->
	    ?shutdown_pid(SetupPid, Node),
	    reset_timer(Timer),
	    case catch tell_name(Socket, MyNode, Version) of
		ok ->
		    connection(Kernel, Node, Socket, Address,
			       {InCookie, InCookie}, MyNode,
			       Type, Version, Timer);
		_ ->
		    ?shutdown(Node)
	    end;
	bad_request ->
	    %% Can not occur !!
	    error_msg("net_connect: ** Simultaneous connect failed for ~p~n",
		      [Node]),
	    ?shutdown(Node)
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
    #net_address {
		  address = Address,
		  host = Host,
		  protocol = tcp,
		  family = inet }.

%% ------------------------------------------------------------
%% Setup a new connection to another Erlang node.
%% Performs the handshake with the other side.
%% ------------------------------------------------------------

setup(Node, Cookie, MyNode, LongOrShortNames,SetupTime) ->
    spawn_link(?MODULE, do_setup, [self(),
				   Node,
				   Cookie,
				   MyNode,
				   LongOrShortNames,
				   SetupTime]).

do_setup(Kernel, Node, Cookie, MyNode, LongOrShortNames,SetupTime) ->
    process_flag(priority, max),
    [Name, Address] = splitnode(Node, LongOrShortNames),
    case inet:getaddr(Address, inet) of
	{ok, Ip} ->
	    Timer = start_timer(SetupTime),
	    case erl_epmd:port_please(Name, Ip) of
		noport ->
		    ?shutdown(Node);
		{port, TcpPort, Version} ->
%		    io:format("please -> version ~p~n", [Version]),
		    reset_timer(Timer),
		    case inet_tcp:connect(Ip, TcpPort, [{active, false}]) of
			{ok, Socket} ->
			    handshake(Kernel, Node, Socket,
				      #net_address {
						    address = {Ip,TcpPort},
						    host = Address,
						    protocol = tcp,
						    family = inet},
				      Cookie,
				      MyNode,
				      Version,
				      Timer);
			_ ->
			    %% Other Node may have closed since port_please !
			    ?shutdown(Node)
		    end
	    end;
	Other ->
	    ?shutdown(Node)
    end.

%%
%% This will tell the net_kernel about the nodedown as it
%% recognizes the exit signal.
%% Terminate with reason shutdown so inet processes want
%% generate crash reports.
%% The termination of this process does also imply that the Socket
%% is closed in a controlled way by inet_drv.
%%
shutdown(Line, Data) ->
    flush_down(),
    exit(shutdown).
% Use this line to debug connection.  Set net_kernel verbose = 1 as well.
%    exit({shutdown, ?MODULE, Line, Data, erlang:now()}).

shutdown_pid(Line, Pid, Data) ->
    exit(Pid, shutdown).
% Use this line to debug connection.  Set net_kernel verbose = 1 as well.
%    exit(Pid, {shutdown, ?MODULE, Line, Data, erlang:now()}).

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
		    ?shutdown(Node);
		L when length(L) > 1, LongOrShortNames == shortnames ->
		    error_msg("** System NOT running to use fully qualified "
			      "hostnames **~n"
			      "** Hostname ~s is illegal **~n",
			      [Host]),
		    ?shutdown(Node);
		_ ->
		    [Name, Host]
	    end;
	[_] ->
	    error_msg("** Nodename ~p illegal, no '@' character **~n",
		      [Node]),
	    ?shutdown(Node);
	_ ->
	    error_msg("** Nodename ~p illegal **~n", [Node]),
	    ?shutdown(Node)
    end.

split_node([Chr|T], Chr, Ack) -> [lists:reverse(Ack)|split_node(T, Chr, [])];
split_node([H|T], Chr, Ack)   -> split_node(T, Chr, [H|Ack]);
split_node([], _, Ack)        -> [lists:reverse(Ack)].

handshake(Kernel, Node, Socket, TcpAddress, Cookie, MyNode, Version, Timer) ->
    SessionCookie = Cookie,
    reset_timer(Timer),
    case catch tell_name(Socket, MyNode, Version) of
	ok ->
	    handshake1(Kernel, Node, Socket, TcpAddress,
		       {SessionCookie, Cookie}, MyNode, Version, Timer);
	_ ->
	    wait_for_acceptpid()
    end.

handshake1(Kernel, Node, Socket, TcpAddress, {InCookie, OldOutCookie},
	   MyNode, Version, Timer) ->
    reset_timer(Timer),
    case inet_tcp:recv(Socket, 2) of
	{ok, [L1,L0]} ->
	    Length = ?i16(L1,L0),
	    case inet_tcp:recv(Socket, Length) of
		{ok, Data} ->
		    {Node, Type, Version} = get_name(Data, Socket), %% crash if badnode
		    connection(Kernel, Node, Socket, TcpAddress,
			       {InCookie, OldOutCookie}, MyNode, Type,
			       Version, Timer);
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
    receive after infinity -> ok end.

%% --------------------------------------------------------------
%% The connection has been established.
%% --------------------------------------------------------------

connection(Kernel, Node, Socket, TcpAddress, Cookies,
	   MyNode, Type, Version, Timer) ->
    cancel_timer(Timer),
    case do_setnode(Node,Socket,Type,Version) of
	error ->
	    ?shutdown(Node);
	Sim ->
	    case inet:setopts(Socket, [{active, true},
				       {packet, 4},
				       {nodelay, true}]) of
		ok -> 
		    mark_nodeup(Kernel,Node,TcpAddress,Cookies,Type),
		    {InCookie,OutCookie} = Cookies,
		    con_loop(Kernel, Node, Socket, TcpAddress, InCookie,
			     OutCookie, MyNode, Type, Sim, #tick{});
		_ ->
		    ?shutdown(Node)
	    end
    end.

do_setnode(Node, Socket, Type, Version) ->
    erlang:setnode(Node, self(), {Type, Version}),
    case init:get_argument(sim_dist) of
	{ok, [Flags]} ->
	    Debug = lists:member("debug", Flags),
	    case lists:member("cache", Flags) of
		true ->
		    erl_atom_cache:add_node(Node, self()),
		    {atom_cache, Debug};
		_ ->
		    {true, Debug}
	    end;
	_ ->
	    {true, false}
    end.


mark_nodeup(Kernel,Node,Address,Cookies,Type) ->
    Kernel ! {self(), {nodeup,Node,Address,Cookies,Type}},
    receive
	{Kernel, inserted} ->
	    ok;
	{Kernel, bad_request} ->
	    error_msg("Uuugh, we were not allowed to send {nodeup, ~p} !!~n",
		      [Node]),
	    ?shutdown(Node)
    end.

con_loop(Kernel, Node, Socket, TcpAddress, InCookie, OutCookie,
	 MyNode, Type, Sim, Tick) ->
    receive
	{tcp_closed, Socket} ->
	    ?shutdown(Node);
	{Kernel, disconnect} ->
	    ?shutdown(Node);
	{Kernel, tick} ->
	    case send_tick(Socket, Tick, Type) of
		{ok, NewTick} ->
		    con_loop(Kernel, Node, Socket, TcpAddress, InCookie,
			     OutCookie, MyNode, Type, Sim, NewTick);
		{error, not_responding} ->
		    error_msg("** Node ~p not responding **~n"
			      "** Removing (timedout) connection **~n",
			      [Node]),
		    ?shutdown(Node);
		Other ->
		    ?shutdown(Node)
	    end;
	{From, get_status} ->
	    case getstat(Socket) of
		{ok, Read, Write, _} ->
		    From ! {self(), get_status, {ok, Read, Write}},
		    con_loop(Kernel, Node, Socket, TcpAddress, InCookie,
			     OutCookie, MyNode, Type, Sim, Tick);
		_ ->
		    ?shutdown(Node)
	    end;

	%% This is for a simulated connection !
	
	{tcp, Socket, Data} ->
	    got_data(Data, Kernel, Node, InCookie, Type, Sim),
	    con_loop(Kernel, Node, Socket, TcpAddress, InCookie,
		     OutCookie, MyNode, Type, Sim, Tick);
	{dist, Data} ->
	    send_data(Data, Node, Socket, OutCookie, Type, Sim),
	    con_loop(Kernel, Node, Socket, TcpAddress, InCookie,
		     OutCookie, MyNode, Type, Sim, Tick)
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
		  family = inet
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
    {list_to_atom(OtherNode), normal, 0};
get_name([$H,_Ip1,_Ip2,_Ip3,_Ip4|OtherNode], Socket) ->
    {list_to_atom(OtherNode), hidden, 0};
get_name([$m,VersionA,VersionB,_Ip1,_Ip2,_Ip3,_Ip4|OtherNode], Socket) ->
    {list_to_atom(OtherNode), normal, ?u16(VersionA,VersionB)};
get_name([$h,VersionA,VersionB,_Ip1,_Ip2,_Ip3,_Ip4|OtherNode], Socket) ->
    {list_to_atom(OtherNode), hidden, ?u16(VersionA,VersionB)};
get_name(Data, Socket) ->
    ?shutdown(Data).

%%
%% Tell our name to the other side.
%% We have to send a correct IP address in case we
%% connects to an old Erlang node (running old tcp_drv.c).
%%
tell_name(Socket, MyNode0, Version) when Version == 0 ->
    MyNode = atom_to_list(MyNode0),
    L = ?int16(length(MyNode) + 5),
    {ok, {{Ip1,Ip2,Ip3,Ip4}, _}} = inet:sockname(Socket),
    ?to_port(Socket, L ++ [$M,Ip1,Ip2,Ip3,Ip4] ++ MyNode),
    ok;
tell_name(Socket, MyNode0, Version) ->
    MyNode = atom_to_list(MyNode0),
    L = ?int16(length(MyNode) + 7),
    {ok, {{Ip1,Ip2,Ip3,Ip4}, _}} = inet:sockname(Socket),
    ?to_port(Socket, L ++ [$m,?int16(Version),Ip1,Ip2,Ip3,Ip4] ++ MyNode),
    ok.

%%
%% Send a TICK to the other side.
%%
%% This will happen every 15 seconds (by default) 
%% The idea here is that every 15 secs, we write a little 
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
%% we haven't read anything as a hidden node only ticks when it receives 
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
    inet_tcp:close(Socket).


%% -------------------------------------------------------
%% Simulates a connection.
%% -------------------------------------------------------

got_data([$p|What], Kernel, Node, InCookie, Type, {Cache, false}) ->
    decode(split_and_decode(What, Node, Type, Cache), Kernel, Node);
got_data([$p|What], Kernel, Node, InCookie, Type, {Cache, _}) ->
    D = split_and_decode(What, Node, Type, Cache),
    io:format("<== ~p : ~p~n", [Node, D]),
    decode(D, Kernel, Node);
got_data([], _, _, _, _, _) ->  %% The TICK !
    ok.

decode([{?DOP_LINK, FromPid, ToPid}], _, _) ->
    erlang:dist_link(ToPid, FromPid);
decode([{?DOP_SEND, InCookie, ToPid}, Msg], _, _) ->
    ToPid ! Msg;
decode([{?DOP_SEND, BadCookie, ToPid}, Msg], Kernel, Node) ->
    Kernel ! {Node, badcookie, ToPid, Msg};
decode([{?DOP_EXIT, FromPid, ToPid, Reason}], _, _) ->
    erlang:dist_exit(ToPid, Reason, FromPid);
decode([{?DOP_UNLINK, FromPid, ToPid}], _, _) ->
    erlang:dist_unlink(ToPid, FromPid);
decode([{?DOP_NODE_LINK}], _, _) ->
    ok;
decode([{?DOP_REG_SEND, FromPid, InCookie, ToName}, Msg], Kernel, _) ->
    whereis(ToName) ! Msg;
%% XXX fix this no cookie check !!!
decode([{?DOP_REG_SEND, FromPid, BadCookie, ToName}, Msg], Kernel, _) ->
    Kernel ! {FromPid, badcookie, ToName, Msg};
decode([{?DOP_GROUP_LEADER, FromPid, ToPid}], _, _) ->
    group_leader(FromPid, ToPid);
decode([{?DOP_EXIT2, FromPid, ToPid, Reason}], _, _) ->
    erlang:dist_exit(ToPid, Reason, FromPid).

split_and_decode(Ext, _, _, true) ->
    erlang:binary_to_dist(list_to_binary(Ext));
split_and_decode(Ext, Node, Type, _) ->
    case erl_external:from_external(Ext, Node, Type) of
	{Control, []} ->
	    [Control];
	{Control, MsgExt} ->
	    {Msg, _} = erl_external:from_external(MsgExt, Node, Type),
	    [Control, Msg]
    end.

send_data(Data, Node, Socket, OutCookie, Type, {Cache, false}) ->
    do_send_data(Data, Node, Socket, OutCookie, Type, Cache);
send_data(Data, Node, Socket, OutCookie, Type, {Cache, _}) ->
    io:format("==> ~p : ~p~n", [Node, Data]),
    do_send_data(Data, Node, Socket, OutCookie, Type, Cache).

do_send_data({?DOP_SEND, _, ToPid, Msg}, Node, Socket, OutCookie,
	     Type, true) ->
    Bin = erlang:dist_to_binary([{?DOP_SEND, OutCookie, ToPid},Msg]),
    pass_through(Socket, binary_to_list(Bin));
do_send_data({?DOP_SEND, _, ToPid, Msg}, Node, Socket, OutCookie, Type, _) ->
    CExt = erl_external:to_external({?DOP_SEND, OutCookie, ToPid}, Node, Type),
    CMsg = erl_external:to_external(Msg, Node, Type),
    pass_through(Socket, CExt ++ CMsg);
do_send_data({?DOP_REG_SEND, FromPid, _, ToName, Msg},
	  Node, Socket, OutCookie, Type, true) ->
    Bin = erlang:dist_to_binary([{?DOP_REG_SEND, FromPid, OutCookie, ToName},
				 Msg]),
    pass_through(Socket, binary_to_list(Bin));
do_send_data({?DOP_REG_SEND, FromPid, _, ToName, Msg},
	  Node, Socket, OutCookie, Type, _) ->
    CExt = erl_external:to_external({?DOP_REG_SEND, FromPid,
				     OutCookie, ToName},
				    Node, Type),
    CMsg = erl_external:to_external(Msg, Node, Type),
    pass_through(Socket, CExt ++ CMsg);
do_send_data(Control, Node, Socket, OutCookie, Type, true) ->
    Bin = erlang:dist_to_binary([Control]),
    pass_through(Socket, binary_to_list(Bin));
do_send_data(Control, Node, Socket, OutCookie, Type, _) ->
    Ext = erl_external:to_external(Control, Node, Type),
    pass_through(Socket, Ext).

pass_through(Socket, Data) ->
    ?to_port(Socket, [?PASS_THROUGH] ++ Data).

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
	    ?shutdown(timer)
    end.

reset_timer(Timer) ->
    Timer ! {self(), reset}.

cancel_timer(Timer) ->
    unlink(Timer),
    exit(Timer, shutdown).

