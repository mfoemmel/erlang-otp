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
-module(global).
-behaviour(gen_server).

%%  A Global register that allows the global registration of pid's and
%% name's, that dynamically keeps up to date with the entire network.
%% global can operate in two modes; in a fully connected network, or
%% in a non-fully connected network.  In the latter case, the name
%% registration mechanism won't work.
%%

%% External exports
-export([start/0, start_link/0, stop/0, sync/0, sync/1,
	 safe_whereis_name/1, whereis_name/1,  register_name/2, register_name/3,
	 register_name_external/2, register_name_external/3, unregister_name_external/1, 
	 re_register_name/2, re_register_name/3,
	 unregister_name/1, registered_names/0, send/2, node_disconnected/1,
	 set_lock/1, set_lock/2, set_lock/3,
	 del_lock/1, del_lock/2,
	 trans/2, trans/3, trans/4,
	 random_exit_name/3, random_notify_name/3, notify_all_name/3, cnode/3]).

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 del_name/2, timer/2, sync_init/2, init_locker/3, resolve_it/4]).

-export([info/0]).


%% This is the protocol version
%% Vsn 1 is the original protocol.
%% Vsn 2 is enhanced with code to take care of registration of names from 
%%       non erlang nodes, e.g. c-nodes.
%% Vsn 3 is enhanced with a tag in the synch messages to distinguish 
%%       different synch sessions from each other, see OTP-2766.
%%       Note: This requires also that the ticket OTP-2928 is fixed on the nodes 
%%             running vsn 1 or 2; if such nodes will coexist with vsn 3 nodes.
-define(vsn, 3).

%%-----------------------------------------------------------------
%% connect_all = bool() - true if we are supposed to set up a
%%                        fully connected net
%% known       = [Node] - all nodes known to us
%% known_v2    = [Node] - all vsn2 nodes known to us, subset of known
%% synced      = [Node] - all nodes that have the same names as us
%% lockers     = [{Node, MyLockerPid}] - the pid of the locker
%%                         process for each Node
%% syncers     = [pid()] - all current syncers processes
%% node_name   = atom()  - our node name (can change if distribution
%%                         is started/stopped dynamically)
%%
%% In addition to these, we keep info about messages arrived in
%% the process dictionary:
%% {pre_connect, Node} = {Vsn, InitMsg} - init_connect msgs that
%%                         arrived before nodeup
%% {wait_lock, Node}   = {exchange, NameList} | lock_is_set
%%                        - see comment below (handle_cast)
%% {save_ops, Node}    = [operation()] - save the ops between
%%                          exchange and resolved
%% {prot_vsn, Node}    = Vsn - the exchange protocol version
%% {sync_tag_my, Node} =  My tag, used at synchronization with Node
%% {sync_tag_his, Node} = The Node's tag, used at synchronization
%%-----------------------------------------------------------------
-record(state, {connect_all, known = [], known_v2 = [], synced = [],
		lockers = [], syncers = [], node_name = node()}).

start() -> gen_server:start({local, global_name_server}, global, [], []).
start_link() -> gen_server:start_link({local, global_name_server},global,[],[]).
stop()  -> gen_server:call(global_name_server, stop, infinity).

sync() -> 
    case check_sync_nodes() of
	{error, Error} ->
	    {error, Error};
	SyncNodes ->
	    gen_server:call(global_name_server, {sync, SyncNodes}, infinity)
    end.
sync(Nodes) -> 
    case check_sync_nodes(Nodes) of
	{error, Error} ->
	    {error, Error};
	SyncNodes ->
	    gen_server:call(global_name_server, {sync, SyncNodes}, infinity)
    end.
    

send(Name, Msg) -> 
    case whereis_name(Name) of
	Pid when pid(Pid) ->
	    Pid ! Msg,
	    Pid;
	undefined ->
	    exit({badarg, {Name, Msg}})
    end.

whereis_name(Name) ->
    %% First, check if a lock is set.  If not - use ets direct, otherwise
    %% use a call.  This one is Quick and Dirty.  The Pid may have died,
    %% but global has not yet handled the EXIT message.  This behaviour is
    %% ok for clients to the Pid, but not for supervisors (and simliar
    %% processes). These should use safe_whereis_name.
    case is_lock_set(global) of
	false ->
	    where(Name);
	true ->
	    gen_server:call(global_name_server, {whereis, Name}, infinity)
    end.

safe_whereis_name(Name) ->
    gen_server:call(global_name_server, {whereis, Name}, infinity).


node_disconnected(Node) ->
    global_name_server ! {nodedown, Node}.


%%-----------------------------------------------------------------
%% Method = function(Name, Pid1, Pid2) -> Pid | Pid2 | none
%% Method is called if a name conflict is detected when two nodes
%% are connecting to each other.  It is supposed to return one of
%% the Pids or 'none'.  If a pid is returned, that pid is
%% registered as Name on all nodes.  If 'none' is returned, the
%% Name is unregistered on all nodes.  If anything else is returned,
%% the Name is unregistered as well.
%% Method is called once at one of the nodes where the processes reside
%% only.  If different Methods are used for the same name, it is
%% undefined which one of them is used.
%% Method is blocking, i.e. when it is called, no calls to whereis/
%% send is let through until it has returned.
%%-----------------------------------------------------------------
register_name(Name, Pid) when pid(Pid) ->
    register_name(Name, Pid, {global, random_exit_name}).
register_name(Name, Pid, Method) when pid(Pid) ->
    Nodes = [node() | gen_server:call(global_name_server, get_known)],
    trans({global, self()},
	  fun() ->
		  case where(Name) of
		      undefined ->
			  gen_server:multi_call(Nodes, 
						global_name_server,
						{register, Name, Pid, Method}),
			  yes;
		      _Pid -> no
		  end
	  end,
	  Nodes).

unregister_name(Name) ->
    case where(Name) of
	undefined ->
	    ok;
	_ ->
	    Nodes = [node() | gen_server:call(global_name_server, get_known)],
	    trans({global, self()},
		  fun() ->
			  gen_server:multi_call(Nodes, 
						global_name_server,
						{unregister, Name}),
			  ok
		  end,
		  Nodes)
    end.

re_register_name(Name, Pid) when pid(Pid) ->
    re_register_name(Name, Pid, {global, random_exit_name}).
re_register_name(Name, Pid, Method) when pid(Pid) ->
    Nodes = [node() | gen_server:call(global_name_server, get_known)],
    trans({global, self()},
	  fun() ->
		  gen_server:multi_call(Nodes,
					global_name_server,
					{register, Name, Pid, Method}),
		  yes
	  end,
	  Nodes).

%% Returns all globally registered names 
registered_names() -> lists:map(fun({Name, Pid, _Method}) -> Name end, 
				ets:tab2list(global_names)).

%%-----------------------------------------------------------------
%% An external node (i.e not an erlang node) (un)registers a name.
%% If the registered Pid crashes the name is to be removed from global.
%% If the external node crashes the name is to be removed from global.
%% If the erlang node which registers the name crashes the name is also to be
%% removed, because the registered process is not supervised any more,
%% (i.e there is no link to the registered Pid).
%%-----------------------------------------------------------------
register_name_external(Name, Pid) when pid(Pid) ->
    register_name_external(Name, Pid, {global, random_exit_name}).
register_name_external(Name, Pid, Method) when pid(Pid) ->
    Nodes = [node() | gen_server:call(global_name_server, get_known)],
    Nodes_v2 = [node() | gen_server:call(global_name_server, get_known_v2)],
    trans({global, self()},
	  fun() ->
		  case where(Name) of
		      undefined ->
			  gen_server:multi_call(Nodes, 
						global_name_server,
						{register, Name, Pid, Method}),
			  gen_server:multi_call(Nodes_v2, 
						global_name_server,
						{register_ext, Name, Pid, node()}),
			  yes;
		      _Pid -> no
		  end
	  end,
	  Nodes).




unregister_name_external(Name) ->
    case where(Name) of
	undefined ->
	    ok;
	_ ->
	    Nodes = [node() | gen_server:call(global_name_server, get_known)],
	    Nodes_v2 = [node() | gen_server:call(global_name_server, get_known_v2)],
	    trans({global, self()},
		  fun() ->
			  gen_server:multi_call(Nodes, 
						global_name_server,
						{unregister, Name}),
			  gen_server:multi_call(Nodes_v2, 
						global_name_server,
						{unregister_ext, Name}),
			  ok
		  end,
		  Nodes)
    end.





%%-----------------------------------------------------------------
%% Args: Id = id()
%%       Nodes = [node()]
%%       id() = {ResourceId, LockRequesterId}
%%       Retries = infinity | int() > 0
%% Purpose: Sets a lock on the specified nodes (or all nodes if
%%          none are specified) on ResourceId for LockRequesterId.  If there
%%          already exists a lock on ResourceId for another owner
%%          than LockRequesterId, false is returned, otherwise true.
%% Returns: bool()
%%-----------------------------------------------------------------
set_lock(Id) ->
    set_lock(Id, [node() | nodes()], infinity, 1).
set_lock(Id, Nodes) ->
    set_lock(Id, Nodes, infinity, 1).
set_lock(Id, Nodes, Retries) when Retries > 0 ->
    set_lock(Id, Nodes, Retries, 1);
set_lock(Id, Nodes, infinity) ->
    set_lock(Id, Nodes, infinity, 1).
set_lock(Id, Nodes, 0, _) -> false;
set_lock({ResourceId, LockRequesterId}, Nodes, Retries, Times) ->
    Id = {ResourceId, LockRequesterId},
    Msg = {set_lock, Id},
    {Replies, _} =
	gen_server:multi_call(Nodes, global_name_server, Msg),
    case check_replies(Replies, Id, Nodes) of
	true -> true;
	false ->
	    random_sleep(Times),
	    set_lock(Id, Nodes, dec(Retries), Times+1)
    end.

check_replies([{_Node, true} | T], Id, Nodes) ->
    check_replies(T, Id, Nodes);
check_replies([{_Node, false} | T], Id, Nodes) ->
    gen_server:multi_call(Nodes, global_name_server, {del_lock, Id}),
    false;
check_replies([], _Id, _Nodes) ->
    true.

del_lock(Id) ->
    del_lock(Id, [node() | nodes()]).
del_lock({ResourceId, LockRequesterId}, Nodes) ->
    Id = {ResourceId, LockRequesterId},
    gen_server:multi_call(Nodes, global_name_server, {del_lock, Id}),
    true.

%%-----------------------------------------------------------------
%% Args: Id = id()
%%       Fun = fun() | {M,F}
%%       Nodes = [node()]
%%       Retries = infinity | int() > 0
%% Purpose: Sets a lock on Id (as set_lock), and evaluates
%%          Res = Fun() on success.
%% Returns: Res | aborted  (note, if Retries is infinity, the
%%          transaction won't abort)
%%-----------------------------------------------------------------
trans(Id, Fun) -> trans(Id, Fun, [node() | nodes()], infinity).
trans(Id, Fun, Nodes) -> trans(Id, Fun, Nodes, infinity).
trans(Id, Fun, Nodes, 0) -> aborted;
trans(Id, Fun, Nodes, Retries) ->
    case set_lock(Id, Nodes, Retries) of
	true ->
	    case catch Fun() of
		{'EXIT', R} ->
		    del_lock(Id, Nodes),
		    exit(R);
		Res ->
		    del_lock(Id, Nodes),
		    Res
	    end;
	false ->
	    aborted
    end.


info() ->
    gen_server:call(global_name_server, info).

%%%-----------------------------------------------------------------
%%% Call-back functions from gen_server
%%%-----------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    ets:new(global_locks, [set, named_table, protected]),
    ets:new(global_names, [set, named_table, protected]),
    ets:new(global_names_ext, [set, named_table, protected]),
    case init:get_argument(connect_all) of
	{ok, [["false"]]} ->
	    {ok, #state{connect_all = false}};
	_ ->
	    {ok, #state{connect_all = true}}
    end.

%%-----------------------------------------------------------------
%% Connection algorithm
%% ====================
%% This alg solves the problem with partitioned nets as well.
%%
%% The main idea in the alg is that when two nodes connect, they
%% try to set a lock in their own partition (i.e. all nodes already
%% known to them).  When the lock is set in each partition, these
%% two nodes send each other a list with all registered names in
%% resp partition(*).  If no conflict is found, the name tables are
%% just updated.  If a conflict is found, a resolve function is
%% called once for each conflict.  The result of the resolving
%% is sent to the other node.  When the names are exchanged, all
%% other nodes in each partition are informed of the other nodes,
%% and they ping each other to form a fully connected net.
%%
%% Here's the flow:
%% Suppose nodes A and B connect, and C is connected to A.
%% 
%% Node A
%% ------
%% << {nodeup, B}
%%   [spawn locker]
%% B ! {init_connect, MyLocker}
%% << {init_connect, MyLocker}
%%   [The lockers try to set the lock]
%% << {lock_is_set, B}
%%   [Now, lock is set in both partitions]
%% B ! {exchange, Names}
%% << {exchange, Names}
%%   [solve conflict]
%% B ! {resolved, Resolved}
%% << {resolved, Resolved}
%% C ! {new_nodes, Resolved, [B]}
%%
%% Node C
%% ------
%% << {new_nodes, ResolvedOps, NewNodes}
%%   [insert Ops]
%% ping(NewNodes)
%% << {nodeup, B}
%% <ignore this one>
%%
%% Several things can disturb this picture.
%%
%% First, the got_names message may arrive *before* the nodeup
%% message, due to delay in net_kernel and an optimisation in the
%% emulator.  We handle this by keeping track of these messages in the
%% pre_connect and lockers variables in our state.
%%
%% The most common situation is when a new node connects to an
%% existing net.  In this case there's no need to set the lock on
%% all nodes in the net, as we know that there won't be any conflict.
%% This is optimised by sending {first_contact, Node} instead of got_names.
%% This implies that first_contact may arrive before nodeup as well.
%%
%% Of course we must handle that some node goes down during the
%% connection.
%%
%% (*) When this information is being exchanged, no one is allowed
%% to change the global register table.  All calls to register etc
%% are protected by a lock.  If a registered process dies
%% during this phase, the deregistration is done as soon as possible
%% on each node (i.e. when the info about the process has arrived).
%%-----------------------------------------------------------------
%% Messages in the protocol
%% ========================
%% 1. Between connecting nodes  (gen_server:casts)
%%    {init_connect, Vsn, Node, InitMsg}
%%         InitMsg = {locker, LockerPid}
%%    {exchange, Node, ListOfNames}
%%    {resolved, Node, Ops, Known}
%%         Known = list of nodes in Node's partition
%% 2. Between lockers on connecting nodes  (!s)
%%    {his_locker, Pid} (from our global)
%%         lockers link to each other
%%    {lock, Bool} loop until both lockers have lock = true,
%%          then send to global {lock_is_set, Node}
%% 3. From connecting node to other nodes in the partition
%%    {new_nodes, Node, Ops, NewNodes}
%% 4. sync protocol
%%    {in_sync, Node, IsKnown}
%%       - sent by each node to all new nodes
%%-----------------------------------------------------------------

handle_call({whereis, Name}, From, S) ->
    do_whereis(Name, From, S),
    {noreply, S};
    
handle_call({register, Name, Pid, Method}, _From, S) ->
    ins_name(S, Name, Pid, Method),
    {reply, yes, S};

handle_call({unregister, Name}, _From, S) ->
    case ets:lookup(global_names, Name) of
	[{_, Pid, _}] ->
	    ets:delete(global_names, Name),
	    dounlink(Pid);
	_ -> ok
    end,
    {reply, ok, S};

handle_call({register_ext, Name, Pid, RegNode}, _F, S) ->
    ins_name_ext(S, Name, Pid, RegNode),
    {reply, yes, S};

handle_call({unregister_ext, Name}, _From, S) ->
    ets:delete(global_names_ext, Name),
    {reply, ok, S};


handle_call({set_lock, Lock}, {Pid, _Tag}, S) ->
    Reply = handle_set_lock(Lock, Pid),
    {reply, Reply, S};

handle_call({del_lock, Lock}, {Pid, _Tag}, S) ->
    handle_del_lock(Lock, Pid, S),
    {reply, true, S};

handle_call(get_known, _From, S) ->
    {reply, S#state.known, S};

handle_call(get_known_v2, _From, S) ->
    {reply, S#state.known_v2, S};

handle_call({sync, Nodes}, From, S) ->
    Pid = start_sync(lists:delete(node(), Nodes) -- S#state.synced, From),
    {noreply, S#state{syncers = [Pid | S#state.syncers]}};

handle_call(get_protocol_version, _From, S) ->
    {reply, ?vsn, S};

handle_call(get_names_ext, _From, S) ->
    {reply, get_names_ext(), S};

handle_call(info, _From, S) ->
    {reply, S, S};

handle_call(stop, _From, S) ->
    {stop, normal, stopped, S}.


%%=======================================================================================
%% init_connect
%%
%% Vsn 1 is the original protocol.
%% Vsn 2 is enhanced with code to take care of registration of names from 
%%       non erlang nodes, e.g. c-nodes.
%% Vsn 3 is enhanced with a tag in the synch messages to distinguish 
%%       different synch sessions from each other, see OTP-2766.
%%       Note: This requires also that the ticket OTP-2928 is fixed on the nodes 
%%             running vsn 1 or 2; if such nodes will coexist with vsn 3 nodes.
%%=======================================================================================
handle_cast({init_connect, Vsn, Node, InitMsg}, S) ->
%    io:format("~p #### init_connect  Vsn ~p, Node ~p, InitMsg ~p~n",[node(), Vsn, Node, InitMsg]),
    case Vsn of
	%% It is always the responsibility of newer versions to understand
	%% older versions of the protocol.  
	{HisVsn, HisTag} when HisVsn > ?vsn ->
	    init_connect(?vsn, Node, InitMsg, HisTag, S#state.lockers);
	{HisVsn, HisTag} ->
	    init_connect(HisVsn, Node, InitMsg, HisTag, S#state.lockers);
	%% To be future compatible
	Tuple when tuple(Tuple) ->
	    List = tuple_to_list(Tuple),
	    [HisVsn, HisTag | Rem] = List,
	    %% use own version handling if his is newer.
	    init_connect(?vsn, Node, InitMsg, HisTag, S#state.lockers);
	_ when Vsn < 3 ->
	    init_connect(Vsn, Node, InitMsg, undef, S#state.lockers);
	_ ->
	    Txt = io_lib:format("Illegal global protocol version ~p Node: ~p",[Vsn, Node]),
	    error_logger:info_report(lists:flatten(Txt))
    end,
    {noreply, S};

%%=======================================================================================
%% lock_is_set
%%
%% Ok, the lock is now set on both partitions. Send our names to other node.
%%=======================================================================================
handle_cast({lock_is_set, Node, MyTag}, S) ->
%    io:format("~p #### lock_is_set  Node ~p~n",[node(), Node]),
    Sync_tag_my = get({sync_tag_my, Node}),
    PVsn =  get({prot_vsn, Node}),
    case {MyTag, PVsn} of
	{Sync_tag_my, undefined} -> 
	    %% Patch for otp-2728, the connection to the Node is flipping up and down
	    %% the messages from the 'older' sync tries can disturb the 'new' sync try
	    %% therefor all messages are discarded if the protocol vsn is not defined.
	    Txt = io_lib:format("undefined global protocol version Node: ~p",[Node]),
	    error_logger:info_report(lists:flatten(Txt)),
	    {noreply, S};
	{Sync_tag_my, _} when PVsn > 3 -> 
	    %% Patch for otp-2728, the connection to the Node is flipping up and down
	    %% the messages from the 'older' sync tries can disturb the 'new' sync try
	    %% therefor all messages are discarded if the protocol vsn is not defined.
	    Txt = io_lib:format("illegal global protocol version ~p Node: ~p",[PVsn, Node]),
	    error_logger:info_report(lists:flatten(Txt)),
	    {noreply, S};
	{Sync_tag_my, _} ->
	    %% Check that the Node is still not known
	    case lists:member(Node, S#state.known) of
		false ->
		    lock_is_set(Node, S#state.known, S#state.known_v2),
		    {noreply, S};
		true ->
		    erase({wait_lock, Node}),
		    NewS = cancel_locker(Node, S),
		    {noreply, NewS}
	    end;
	_ ->
	    %% Illegal tag, delete the locker.
	    erase({wait_lock, Node}),
	    NewS = cancel_locker(Node, S),
	    {noreply, NewS}
    end;

%%=======================================================================================
%% exchange
%%
%% Here the names are checked to detect name clashes.
%%=======================================================================================
%% Vsn 1 of the protocol
handle_cast({exchange, Node, NameList}, S) ->
%    io:format("~p #### handle_cast 1 exchange ~p~n",[node(),{Node, NameList}]),
    PVsn =  get({prot_vsn, Node}),
    exchange(PVsn, Node, NameList, S#state.known),
    {noreply, S};

%% Vsn 2 of the protocol
handle_cast({exchange, Node, NameList, NameExtList}, S) ->
%    io:format("~p #### handle_cast 2 lock_is_set  exchange ~p~n",
%	      [node(),{Node, NameList, NameExtList}]),
    PVsn =  get({prot_vsn, Node}),
    case PVsn of
	undefined -> 
	    %% Patch for otp-2728, the connection to the Node is flipping up and down
	    %% the messages from the 'older' sync tries can disturb the 'new' sync try
	    %% therefor all messages are discarded if the protocol vsn is not defined.
	    Txt = lists:flatten(io_lib:format("undefined global protocol version Node: ~p",[Node])),
	    error_logger:info_report(Txt),
	    {noreply, S};
	_ ->
	    exchange(PVsn, Node, {NameList, NameExtList}, {S#state.known, S#state.known_v2}),
	    {noreply, S}
    end;


%% Vsn 3 of the protocol
handle_cast({exchange, Node, NameList, NameExtList, MyTag}, S) ->
%    io:format("~p #### handle_cast 3 lock_is_set  exchange ~p~n",
%	      [node(),{Node, NameList, NameExtList, MyTag}]),
    Sync_tag_my = get({sync_tag_my, Node}),
    PVsn =  get({prot_vsn, Node}),
    case {MyTag, PVsn} of
	{Sync_tag_my, undefined} -> 
	    %% Patch for otp-2728, the connection to the Node is flipping up and down
	    %% the messages from the 'older' sync tries can disturb the 'new' sync try
	    %% therefor all messages are discarded if the protocol vsn is not defined.
	    Txt = lists:flatten(io_lib:format(
				  "undefined global protocol version Node: ~p",[Node])),
	    error_logger:info_report(Txt),
	    {noreply, S};
	{Sync_tag_my, _} ->
	    exchange(PVsn, Node, {NameList, NameExtList}, {S#state.known, S#state.known_v2}),
	    {noreply, S};
	_ ->
	    %% Illegal tag, delete the locker.
	    erase({wait_lock, Node}),
	    NewS = cancel_locker(Node, S),
	    {noreply, NewS}
    end;



%%=======================================================================================
%% resolved
%%
%% Here the name clashes are resolved.
%%=======================================================================================
%% Vsn 1 of the protocol
handle_cast({resolved, Node, Resolved, HisKnown}, S) ->
%    io:format("~p #### 1 resolved ~p~n",[node(),{Node, Resolved, HisKnown}]),
    %% Check if any of his nodes has vs2 protocol, remove the nodes which are not
    %% contacteable (must went down after the resolved message was sent)
    {HisKnown_v2, Unknown} = check_prot_vsn(HisKnown),
    Names_ext = get_names_ext(HisKnown_v2),
    NewS = resolved(Node, Resolved, {HisKnown, HisKnown_v2}, Names_ext, S),
    {noreply, NewS};
%    handle_cast({resolved, Node, Resolved, HisKnown -- Unknown, HisKnown_v2, Names_ext}, S);

%% Vsn 2 of the protocol
handle_cast({resolved, Node, Resolved, HisKnown, HisKnown_v2, Names_ext}, S) ->
%    io:format("~p #### 2 resolved ~p~n",[node(),{Node, Resolved, HisKnown, HisKnown_v2, Names_ext}]),
    PVsn =  get({prot_vsn, Node}),
    case PVsn of
	undefined -> 
	    %% Patch for otp-2728, the connection to the Node is flipping up and down
	    %% the messages from the 'older' sync tries can disturb the 'new' sync try
	    %% therefor all messages are discarded if the protocol vsn is not defined.
	    Txt = lists:flatten(io_lib:format("undefined global protocol version Node: ~p",[Node])),
	    error_logger:info_report(Txt),
	    {noreply, S};
	_ ->
	    NewS = resolved(Node, Resolved, {HisKnown, HisKnown_v2}, Names_ext, S),
	    {noreply, NewS}
    end;

%% Vsn 3 of the protocol
handle_cast({resolved, Node, Resolved, HisKnown, HisKnown_v2, Names_ext, MyTag}, S) ->
%    io:format("~p #### 2 resolved ~p~n",[node(),{Node, Resolved, HisKnown, HisKnown_v2, Names_ext}]),
    Sync_tag_my = get({sync_tag_my, Node}),
    PVsn =  get({prot_vsn, Node}),
    case {MyTag, PVsn} of
	{Sync_tag_my, undefined} -> 
		%% Patch for otp-2728, the connection to the Node is flipping up and down
		%% the messages from the 'older' sync tries can disturb the 'new' sync try
		%% therefor all messages are discarded if the protocol vsn is not defined.
		Txt = lists:flatten(io_lib:format(
				      "undefined global protocol version Node: ~p",[Node])),
		error_logger:info_report(Txt),
		{noreply, S};
	{Sync_tag_my, _} ->
	    NewS = resolved(Node, Resolved, {HisKnown, HisKnown_v2}, Names_ext, S),
	    {noreply, NewS};
	_ ->
	    %% Illegal tag, delete the locker.
	    erase({wait_lock, Node}),
	    NewS = cancel_locker(Node, S),
	    {noreply, NewS}
    end;






%%=======================================================================================
%% new_nodes
%%
%% We get to know the other node's known nodes.
%%=======================================================================================
%% Vsn 1 of the protocol
handle_cast({new_nodes, Node, Ops, Nodes}, S) ->
%    io:format("~p #### 1 new_nodes  ~p~n",[node(),{Ops, Nodes}]),
    %% Check if any of the nodes has vs2 protocol, remove the nodes which are not
    %% contacteable (must went down after the new_nodes message was sent)
    {Nodes_v2, Unknown} = check_prot_vsn(Nodes),
    Names_ext = get_names_ext(Nodes_v2),
    NewS = new_nodes(Ops, Names_ext, Nodes -- Unknown, Nodes_v2, S),
    {noreply, NewS};

    
%% Vsn 2 of the protocol
handle_cast({new_nodes, _Node, Ops, Names_ext, Nodes, Nodes_v2}, S) ->
%    io:format("~p #### 2 new_nodes  ~p~n",[node(),{Ops, Names_ext, Nodes, Nodes_v2}]),
    NewS = new_nodes(Ops, Names_ext, Nodes, Nodes_v2, S),
    {noreply, NewS};
    



%%=======================================================================================
%% in_sync
%%
%% We are in sync with this node (from the other node's known world).
%%=======================================================================================
handle_cast({in_sync, Node, IsKnown}, S) ->
%    io:format("~p #### in_sync  ~p~n",[node(),{Node, IsKnown}]),
    lists:foreach(fun(Pid) -> Pid ! {synced, [Node]} end, S#state.syncers),
    erase({wait_lock, Node}),
    erase({pre_connect, Node}),
    erase({sync_tag_my, Node}),
    erase({sync_tag_his, Node}),
    NewS = cancel_locker(Node, S),
    NKnown = case lists:member(Node, Known = NewS#state.known) of
		 false when IsKnown == true ->
		     gen_server:cast({global_name_server, Node},
				     {in_sync, node(), false}),
		     [Node | Known];
		 _ ->
		     Known
	     end,
    NKnown_v2 = case lists:member(Node, Known_v2 = NewS#state.known_v2) of
		  true -> Known_v2;
		  false -> [Node | Known_v2]
	      end,
    NSynced = case lists:member(Node, Synced = NewS#state.synced) of
		  true -> Synced;
		  false -> [Node | Synced]
	      end,
    {noreply, NewS#state{known = NKnown, synced = NSynced}};




%% Called when Pid on other node crashed
handle_cast({async_del_name, Name, Pid}, S) ->
    case ets:lookup(global_names, Name) of
	[{Name, Pid, _}] ->
	    ets:delete(global_names, Name),
	    dounlink(Pid);
	_ -> ok
    end,
    ets:delete(global_names_ext, Name),
    {noreply, S};

handle_cast({async_del_lock, ResourceId, Pid}, S) ->
    del_locks2(ets:tab2list(global_locks), Pid),
%    ets:match_delete(global_locks, {ResourceId, '_', Pid}),
    {noreply, S}.


handle_info({'EXIT', Pid, Reason}, S) when pid(Pid) ->
    check_exit(Pid),
    Syncers = lists:delete(Pid, S#state.syncers),
    Lockers = lists:keydelete(Pid, 2, S#state.lockers),
    {noreply, S#state{syncers = Syncers, lockers = Lockers}};

handle_info({nodedown, Node}, S) when Node == S#state.node_name ->
    %% Somebody stopped the distribution dynamically - change
    %% references to old node name (Node) to new node name ('nonode@nohost')
    {noreply, change_our_node_name(node(), S)};

handle_info({nodedown, Node}, S) ->
%    X = lists:flatten(io_lib:format("~p #### nodedown 1 ####### Node ~p",[node(),Node])),
%    erlang:display(list_to_atom(X)),
    erase({wait_lock, Node}),
    erase({save_ops, Node}),
    erase({pre_connect, Node}),
    erase({prot_vsn, Node}),
    erase({sync_tag_my, Node}),
    erase({sync_tag_his, Node}),
    do_node_down(Node),
    #state{known = Known, known_v2 = Known_v2, synced = Syncs, lockers = Ls} = S,
    NewS = cancel_locker(Node, S),
    {noreply, NewS#state{known = lists:delete(Node, Known),
			 known_v2 = lists:delete(Node, Known_v2),
			 synced = lists:delete(Node, Syncs)}};



handle_info({nodeup, Node}, S) when Node == node() ->
%    io:format("~p ####  nodeup S ####### Node ~p~n",[node(), Node]),
    %% Somebody started the distribution dynamically - change
    %% references to old node name ('nonode@nohost') to Node.
    {noreply, change_our_node_name(Node, S)};

handle_info({nodeup, Node}, S) when S#state.connect_all == true ->
%    X = lists:flatten(io_lib:format("~p #### nodeup 1 ####### Node ~p",[node(),Node])),
%    erlang:display(list_to_atom(X)),
    IsKnown = lists:member(Node, S#state.known) or
              %% This one is only for double nodeups (shouldn't occur!)
              lists:keymember(Node, 1, S#state.lockers),
    case IsKnown of
	true ->
	    {noreply, S};
	false ->
	    %% now() is used as a tag to separate different sycnh sessions
	    %% from each others. Global could be confused at bursty nodeups
	    %% because it couldn't separate the messages between the different
	    %% synch sessions started by a nodeup.
	    MyTag = now(),
	    resend_pre_connect(Node),
	    Pid = start_locker(Node, S#state.known, MyTag),
	    Ls = S#state.lockers,
	    InitC = {init_connect, {?vsn, MyTag}, node(), {locker, Pid, S#state.known}},
	    put({sync_tag_my, Node}, MyTag),
	    gen_server:cast({global_name_server, Node}, InitC),
	    {noreply, S#state{lockers = [{Node, Pid} | Ls]}}
    end;


%% This message is only to test otp-2766 Global may be confused at bursty nodeup/nodedowns
handle_info({test_vsn_tag_nodeup, Node}, S) when S#state.connect_all == true ->
%    io:format("~p #### test_nodeup 1 ####### Node ~p~n",[node(), Node]),
    MyTag = now(),
    resend_pre_connect(Node),
    Pid = start_locker(Node, S#state.known, MyTag),
    Ls = S#state.lockers,
    InitC = {init_connect, {?vsn, MyTag}, node(), {locker, Pid, S#state.known}},
    put({sync_tag_my, Node}, MyTag),
    gen_server:cast({global_name_server, Node}, InitC),
    {noreply, S#state{lockers = [{Node, Pid} | Ls]}};


handle_info({whereis, Name, From}, S) ->
    do_whereis(Name, From, S),
    {noreply, S};

handle_info(known, S) ->
    io:format(">>>> ~p~n",[S#state.known]),
    {noreply, S};

handle_info(known_v2, S) ->
    io:format(">>>> ~p~n",[S#state.known_v2]),
    {noreply, S};
    
handle_info(_, S) ->
    {noreply, S}.




%%=======================================================================================
%%=======================================================================================
%%=============================== Internal Functions ====================================
%%=======================================================================================
%%=======================================================================================



%%=======================================================================================
%% Another node wants to synchronize its registered names with us.
%% Start a locker process. Both nodes must have a lock before they are
%% allowed to continue.
%%=======================================================================================
init_connect(Vsn, Node, InitMsg, HisTag, Lockers) ->
%    io:format("~p #### init_connect  Vsn, Node, InitMsg ~p~n",[node(),{Vsn, Node, InitMsg}]),
    %% It is always the responsibility of newer versions to understand
    %% older versions of the protocol.  
    put({prot_vsn, Node}, Vsn),
    put({sync_tag_his, Node}, HisTag),
    case lists:keysearch(Node, 1, Lockers) of
	{value, {_Node, MyLocker}} ->
	    %% We both have lockers; let them set the lock
	    case InitMsg of
		{locker, HisLocker} -> %% old version
		    MyLocker ! {his_locker, HisLocker};
		{locker, HisLocker, HisKnown} -> %% current version
		    MyLocker ! {his_locker, HisLocker, HisKnown}
	    end;
	false ->
	    put({pre_connect, Node}, {Vsn, InitMsg, HisTag})
    end.



%%=======================================================================================
%% In the simple case, we'll get lock_is_set before we get exchange,
%% but we may get exchange before we get lock_is_set from our locker.
%% If that's the case, we'll have to remember the exchange info, and
%% handle it when we get the lock_is_set.  We do this by using the
%% process dictionary - when the lock_is_set msg is received, we store
%% this info.  When exchange is received, we can check the dictionary
%% if the lock_is_set has been received.  If not, we store info about
%% the exchange instead.  In the lock_is_set we must first check if
%% exchange info is stored, in that case we take care of it.
%%=======================================================================================
lock_is_set(Node, Known, Known_v2) ->
%    io:format("~p ####  lock_is_set ~p~n",[node(),{Node, Node, Known, Known_v2}]),
    PVsn = get({prot_vsn, Node}),
    case PVsn of 
	1 ->
	    gen_server:cast({global_name_server, Node},
			    {exchange, node(), get_names()});
	2 ->
	    gen_server:cast({global_name_server, Node},
			    {exchange, node(), get_names(), get_names_ext()});
	3 ->
	    gen_server:cast({global_name_server, Node},
			    {exchange, node(), get_names(), get_names_ext(), 
			     get({sync_tag_his, Node})})
    end,
    %% If both have the lock, continue with exchange
    case get({wait_lock, Node}) of
	{exchange, NameList} ->
	    %% vsn 1
	    put({wait_lock, Node}, lock_is_set),
	    exchange(PVsn, Node, NameList, Known);
	{exchange, NameList, NameExtList} ->
	    %% vsn 2, 3
	    put({wait_lock, Node}, lock_is_set),
	    exchange(PVsn, Node, {NameList, NameExtList}, {Known, Known_v2});
	undefined ->
	    put({wait_lock, Node}, lock_is_set)
    end.



%%=======================================================================================
%% exchange
%%=======================================================================================
%% Vsn 1 of the protocol
exchange(1, Node, NameList, Known) ->
%    io:format("~p #### 1 exchange ~p~n",[node(),{Node, NameList}]),
    case erase({wait_lock, Node}) of
	lock_is_set ->
	    {Ops, Resolved} = exchange_names(NameList, Node, [], []),
	    put({save_ops, Node}, Ops),
	    gen_server:cast({global_name_server, Node},
			    {resolved, node(), Resolved, Known});
	undefined ->
	    put({wait_lock, Node}, {exchange, NameList})
    end;
%% Vsn 2 of the protocol
exchange(2, Node, {NameList, NameExtList}, {Known, Known_v2}) ->
%    io:format("~p #### 2 lock_is_set  exchange ~p~n",[node(),{Node, NameList, NameExtList}]),
    case erase({wait_lock, Node}) of
	lock_is_set ->
	    {Ops, Resolved} = exchange_names(NameList, Node, [], []),
	    put({save_ops, Node}, Ops),
	    gen_server:cast({global_name_server, Node},
			    {resolved, node(), Resolved, Known, 
			     Known_v2, get_names_ext()});
	undefined ->
	    put({wait_lock, Node}, {exchange, NameList, NameExtList})
    end;
%% Vsn 3 of the protocol
exchange(3, Node, {NameList, NameExtList}, {Known, Known_v2}) ->
%    io:format("~p #### 3 lock_is_set  exchange ~p~n",[node(),{Node, NameList, NameExtList}]),
    case erase({wait_lock, Node}) of
	lock_is_set ->
	    {Ops, Resolved} = exchange_names(NameList, Node, [], []),
	    put({save_ops, Node}, Ops),
	    gen_server:cast({global_name_server, Node},
			    {resolved, node(), Resolved, Known, 
			     Known_v2, get_names_ext(), get({sync_tag_his, Node})});
	undefined ->
	    put({wait_lock, Node}, {exchange, NameList, NameExtList})
    end.





resolved(Node, Resolved, {HisKnown, HisKnown_v2}, Names_ext, S) ->
%    io:format("~p #### 2 resolved ~p~n",[node(),{Node, Resolved, HisKnown, HisKnown_v2, Names_ext}]),
    Vsn = erase({prot_vsn, Node}),
    Ops = erase({save_ops, Node}) ++ Resolved,
    Known = S#state.known,
    Known_v2 = S#state.known_v2,
    Synced = S#state.synced,
    NewNodes = [Node | HisKnown],
    NewNodes_v2 = case Vsn of
		      1 ->
			  HisKnown_v2;
		      2 ->
			  [Node | HisKnown_v2];
		      3 ->
			  [Node | HisKnown_v2]
		  end,
    do_ops(Ops),
    do_ops_ext(Ops,Names_ext),
    gen_server:abcast(Known -- Known_v2, global_name_server,
		      {new_nodes, node(), Ops, NewNodes}),
    gen_server:abcast(Known_v2, global_name_server,
		      {new_nodes, node(), Ops, Names_ext, NewNodes, NewNodes_v2}),
    %% I am synced with Node, but not with HisKnown yet
    lists:foreach(fun(Pid) -> Pid ! {synced, [Node]} end, S#state.syncers),
    gen_server:abcast(HisKnown, global_name_server, {in_sync, node(), true}),
    NewS = lists:foldl(fun(Node1, S1) -> cancel_locker(Node1, S1) end,
		       S,
		       NewNodes),
    %% See (*) below... we're node b in that description
    NewKnown = Known ++ (NewNodes -- Known),
    NewKnown_v2 = Known_v2 ++ (NewNodes_v2 -- Known_v2),
    NewS#state{known = NewKnown, known_v2 = NewKnown_v2, synced = [Node | Synced]}.



    
new_nodes(Ops, Names_ext, Nodes, Nodes_v2, S) ->
%    io:format("~p #### 2 new_nodes  ~p~n",[node(),{Ops, Names_ext, Nodes, Nodes_v2}]),
    do_ops(Ops),
    do_ops_ext(Ops,Names_ext),
    Known = S#state.known,
    Known_v2 = S#state.known_v2,
    %% (*) This one requires some thought...
    %% We're node a, other nodes b and c:
    %% The problem is that {in_sync, a} may arrive before {resolved, [a]} to
    %% b from c, leading to b sending {new_nodes, [a]} to us (node a).
    %% Therefore, we make sure we never get duplicates in Known.
    NewNodes = lists:delete(node(), Nodes -- Known),
    NewNodes_v2 = lists:delete(node(), Nodes_v2 -- Known_v2),
    gen_server:abcast(NewNodes, global_name_server, {in_sync, node(), true}),
    S#state{known = Known ++ NewNodes, known_v2 = Known_v2 ++ NewNodes_v2}.
    




do_whereis(Name, From, S) ->
    case is_lock_set(global) of
	false ->
	    gen_server:reply(From, where(Name));
	true ->
	    send_again({whereis, Name, From})
    end.

terminate(_Reason, S) ->
    ets:delete(global_names),
    ets:delete(global_names_ext),
    ets:delete(global_locks).


%% Resend init_connect to ourselves.
resend_pre_connect(Node) ->
    case erase({pre_connect, Node}) of
	{Vsn, InitMsg, undef} ->
	    %% Vsn 1 & 2
	    gen_server:cast(self(), {init_connect, Vsn, Node, InitMsg});
	{Vsn, InitMsg, HisTag} ->
	    %% Vsn 3
	    gen_server:cast(self(), {init_connect, {Vsn, HisTag}, Node, InitMsg});
	_ ->
	    ok
    end.

ins_name(S, Name, Pid, Method) ->
    case ets:lookup(global_names, Name) of
	[{Name, Pid2, _}] ->
	    dounlink(Pid2);
	[] ->
	    ok
    end,
    dolink(Pid),
    ets:insert(global_names, {Name, Pid, Method}).

ins_name_ext(S, Name, Pid, RegNode) ->
    case ets:lookup(global_names_ext, Name) of
	[{Name, Pid2, _}] ->
	    dounlink(Pid2);
	[] ->
	    ok
    end,
    dolink_ext(Pid, RegNode),
    ets:insert(global_names_ext, {Name, Pid, RegNode}).

where(Name) ->
    case ets:lookup(global_names, Name) of
	[{_, Pid, _}] -> Pid;
	[] -> undefined
    end.

handle_set_lock({ResourceId, LockRequesterId}, Pid) ->
    case ets:lookup(global_locks, ResourceId) of
	[{ResourceId, LockRequesterId, Pids}] -> 
	    case lists:member(Pid, Pids) of
		true ->
		    true;
		false ->
		    dolink(Pid),
		    ets:insert(global_locks, {ResourceId, LockRequesterId, [Pid | Pids]}),
		    true
	    end;
	[{ResourceId, LockRequesterId2, Pid2}] -> 
	    false;
	[] ->
	    dolink(Pid),
	    ets:insert(global_locks, {ResourceId, LockRequesterId, [Pid]}),
	    true
    end.

is_lock_set(ResourceId) ->
    case ets:lookup(global_locks, ResourceId) of
	[_Lock] -> true;
	[] -> false
    end.

handle_del_lock({ResourceId, LockRequesterId}, Pid, S) ->
    case ets:lookup(global_locks, ResourceId) of
	[{ResourceId, LockRequesterId, Pids}] when [Pid] == Pids ->
	    ets:delete(global_locks, ResourceId),
	    dounlink(Pid);
	[{ResourceId, LockRequesterId, Pids}] ->
	    NewPids = lists:delete(Pid, Pids),
	    ets:insert(global_locks, {ResourceId, LockRequesterId, NewPids}),
	    dounlink(Pid);
	_ -> ok
    end.

do_ops(Ops) ->
    lists:foreach(fun({insert, Item}) -> ets:insert(global_names, Item);
		     ({delete, Name}) ->
			  case ets:lookup(global_names, Name) of
			      [{Name, Pid, _}] ->
				  ets:delete(global_names, Name),
				  dounlink(Pid);
			      [] ->
				  ok
			  end
		  end, Ops).

%% If a new name, then it must be checked if it is an external name
%% If delete a name it is always deleted from global_names_ext
do_ops_ext(Ops, Names_ext) ->
    lists:foreach(fun({insert, {Name, Pid, Method}}) -> 
			  case lists:keysearch(Name, 1, Names_ext) of
			      {value, {Name, Pid, RegNode}} ->
				  ets:insert(global_names_ext, {Name, Pid, RegNode});
			      _ ->
				  ok
			  end;
		     ({delete, Name}) ->
			  ets:delete(global_names_ext, Name)
		  end, Ops).

%%-----------------------------------------------------------------
%% A locker is a process spawned by global_name_server when a
%% nodeup is received from a new node.  Its purpose is to try to
%% set a lock in our partition, i.e. on all nodes known to us.
%% When the lock is set, it tells global about it, and keeps
%% the lock set.  global sends a cancel message to the locker when
%% the partitions are connected.
%%-----------------------------------------------------------------
start_locker(Node, Known, MyTag) ->
    %% No link here!  The del_lock call would delete the link anyway.
    %% global_name_server has control of these processes anyway...
    spawn(?MODULE, init_locker, [Node, Known, MyTag]).

init_locker(Node, Known, MyTag) ->
    process_flag(trap_exit, true),
    receive
	{his_locker, Pid, HisKnown} ->
	    link(Pid),
	    %% If two nodes in a group of nodes first disconnect
	    %% and then reconnect, this causes global to deadlock. 
	    %% This because both of the reconnecting nodes
	    %% tries to set lock on the other nodes in the group.
	    %% This is solved by letting only one of the reconneting nodes set the lock.
	    BothsKnown = HisKnown -- (HisKnown -- Known),
	    if
		node() < Node ->
		    loop_locker(Node, Pid, [node() | Known], 1, MyTag);
		true ->
		    loop_locker(Node, Pid, [node() | Known] -- BothsKnown, 1, MyTag)
	    end;
	cancel ->
	    exit(normal)
    end.

loop_locker(Node, Pid, Known, Try, MyTag) ->
    LockId = {global, {locker, self()}},
    IsLockSet = set_lock(LockId, Known, 1),
    %% Tell other node that we managed to get the lock.
    Pid ! {lock, IsLockSet},
    %% Wait for other node's result.
    receive
	{lock, true} when IsLockSet == true ->
	    %% Now we got the lock in both partitions.  Tell
	    %% global, and let him resolve name conflict.
	    gen_server:cast(global_name_server, {lock_is_set, Node, MyTag}),
	    %% Wait for global to tell us to remove lock.
	    receive
		cancel ->
		    %% All conflicts are resolved, remove lock.
		    del_lock(LockId, Known);
		{'EXIT', Pid, _} ->
		    %% Other node died; remove lock and ignore him.
		    del_lock(LockId, Known)
	    end;
	{lock, _} ->
	    %% Some of us failed to get the lock; try again
	    d_lock(IsLockSet, LockId, Known),
	    try_again_locker(Node, Pid, Known, Try, MyTag);
	{'EXIT', Pid, _} ->
	    %% Other node died; remove lock and ignore him.
	    d_lock(IsLockSet, LockId, Known);
	cancel ->
	    d_lock(IsLockSet, LockId, Known)
    end.

d_lock(true, LockId, Known) -> del_lock(LockId, Known);
d_lock(false, _, _) -> ok.

try_again_locker(Node, Pid, Known, Try, MyTag) ->
    random_sleep(Try),
    NewKnown = gen_server:call(global_name_server, get_known),
    case lists:member(Node, NewKnown) of
	false ->
	    loop_locker(Node, Pid, [node() | NewKnown], Try+1, MyTag);
	true ->
	    %% Node is already handled, we are ready.
	    ok
    end.
	    
cancel_locker(Node, S) ->
    Lockers = S#state.lockers,
    case lists:keysearch(Node, 1, Lockers) of
	{value, {_, Pid}} ->
	    Pid ! cancel,
	    S#state{lockers = lists:keydelete(Node, 1, Lockers)};
	_ ->
	    S
    end.

%% A node sent us his names. When a name clash is found, the resolve
%% function is called from the smaller node => all resolve funcs are called
%% from the same partition.
exchange_names([{Name, Pid, Method} |Tail], Node, Ops, Res) ->
    case ets:lookup(global_names, Name) of
	[{Name, Pid, _}] -> 
	    exchange_names(Tail, Node, Ops, Res);
	[{Name, Pid2, Method2}] when node() < Node ->
	    %% Name clash!  Add the result of resolving to Res(olved).
	    %% We know that node(Pid) /= node(), so we don't
	    %% need to link/unlink to Pid.
	    Node2 = node(Pid2), %%&&&&&& check external node???
	    case rpc:call(Node2, ?MODULE, resolve_it, 
			  [Method2, Name, Pid, Pid2]) of
		Pid ->
		    dounlink(Pid2),
		    ets:insert(global_names, {Name, Pid, Method}),
		    Op = {insert, {Name, Pid, Method}},
		    exchange_names(Tail, Node, [Op | Ops], [Op | Res]);
		Pid2 ->
		    Op = {insert, {Name, Pid2, Method2}},
		    exchange_names(Tail, Node, Ops, [Op | Res]);
		none ->
		    dounlink(Pid2),
		    ets:delete(global_names, Name),
		    Op = {delete, Name},
		    exchange_names(Tail, Node, [Op | Ops], [Op | Res]);
		{badrpc, Badrpc} ->
		    error_logger:info_msg("global: badrpc ~w received when "
					  "conflicting name ~w was found",
					  [Badrpc, Name]),
		    dounlink(Pid2),
		    ets:delete(global_names, Name),
		    Op = {delete, Name},
		    exchange_names(Tail, Node, [Op | Ops], [Op | Res]);
		Else ->
		    error_logger:info_msg("global: Resolve method ~w for "
					  "conflicting name ~w returned ~w~n",
					  [Method, Name, Else]),
		    dounlink(Pid2),
		    ets:delete(global_names, Name),
		    Op = {delete, Name},
		    exchange_names(Tail, Node, [Op | Ops], [Op | Res])
	    end;
	[{Name, Pid2, _}] ->
	    %% The other node will solve the conflict.
	    exchange_names(Tail, Node, Ops, Res);
	_ ->
	    %% Entirely new name.
	    ets:insert(global_names, {Name, Pid, Method}),
	    exchange_names(Tail, Node,
			   [{insert, {Name, Pid, Method}} | Ops], Res)
    end;
exchange_names([], _, Ops, Res) ->
    {Ops, Res}.

resolve_it(Method, Name, Pid1, Pid2) ->
    catch Method(Name, Pid1, Pid2).

minmax(P1,P2) ->
    if node(P1) < node(P2) -> {P1, P2}; true -> {P2, P1} end.

random_exit_name(Name, Pid, Pid2) ->
    {Min, Max} = minmax(Pid, Pid2),
    error_logger:info_msg("global: Name conflict terminating ~w~n",
			  [{Name, Max}]),
    exit(Max, kill),
    Min.

random_notify_name(Name, Pid, Pid2) ->
    {Min, Max} = minmax(Pid, Pid2),
    Max ! {global_name_conflict, Name},
    Min.

notify_all_name(Name, Pid, Pid2) ->
    Pid ! {global_name_conflict, Name, Pid2},
    Pid2 ! {global_name_conflict, Name, Pid},
    none.

cnode(Name, Pid, Pid2) ->
    {Min, Max} = minmax(Pid, Pid2),
    error_logger:info_msg("global: Name conflict terminating ~w~n",
			  [{Name, Max}]),
    Max ! {global_name_conflict, Name},
    Min.

%% Only link to pids on our own node
dolink(Pid) when node(Pid) == node() -> 
    link(Pid);
dolink(_) -> ok.

%% Only link to pids on our own node
dolink_ext(Pid, RegNode) when RegNode == node() -> link(Pid);
dolink_ext(_, _) -> ok.

dounlink(Pid) when node(Pid) == node() ->
    case ets:match(global_names, {'_', Pid, '_'}) of
	[] ->
	    case is_pid_used(Pid) of
		false -> 
		    unlink(Pid);
		true -> ok
	    end;
	_ -> ok
    end;
dounlink(_Pid) ->
    ok.

is_pid_used(Pid) ->
    is_pid_used(ets:tab2list(global_locks), Pid).

is_pid_used([],Pid) -> 
    false;
is_pid_used([{ResourceId, LockReqId, Pids} | Tail], Pid) ->
    case lists:member(Pid, Pids) of
	true ->
	    true;
	false ->
	    is_pid_used(Tail, Pid)
    end.
	       
    

%% check_exit/3 removes the Pid from affected tables.
%% This function needs to abcast the thingie since only the local
%% server is linked to the registered process (or the owner of the
%% lock).  All the other servers rely on the nodedown mechanism.
check_exit(Pid) ->
    del_names(ets:tab2list(global_names), Pid),
    del_locks(ets:tab2list(global_locks), Pid).

del_names([{Name, Pid, _Method} | Tail], Pid) ->
    %% First, delete the Pid from the local ets; then send to other nodes
    ets:delete(global_names, Name),
    ets:delete(global_names_ext, Name),
    dounlink(Pid),
    spawn_link(?MODULE, del_name, [Name, Pid]),
    del_names(Tail, Pid);
del_names([_|T], Pid) ->
    del_names(T, Pid);
del_names([], Pid) -> done.

del_name(Name, Pid) ->
    trans({global, self()},
	  fun() ->
		  gen_server:abcast(nodes(), global_name_server,
				    {async_del_name, Name, Pid})
	  end,
	  nodes()).

del_locks([{ResourceId, LockReqId, Pids} | Tail], Pid) ->
    case {lists:member(Pid, Pids), Pids} of
	{true, [Pid]} ->
	    ets:delete(global_locks, ResourceId),
	    gen_server:abcast(nodes(), global_name_server,
			      {async_del_lock, ResourceId, Pid});
	{true, _} ->
	    NewPids = lists:delete(Pid, Pids),
	    ets:insert(global_locks, {ResourceId, LockReqId, NewPids}),
	    gen_server:abcast(nodes(), global_name_server,
			      {async_del_lock, ResourceId, Pid});
	_ ->
	    continue
    end,
    del_locks(Tail, Pid);
del_locks([],Pid) -> done.

del_locks2([{ResourceId, LockReqId, Pids} | Tail], Pid) ->
    case {lists:member(Pid, Pids), Pids} of
	{true, [Pid]} ->
	    ets:delete(global_locks, ResourceId);
	{true, _} ->
	    NewPids = lists:delete(Pid, Pids),
	    ets:insert(global_locks, {ResourceId, LockReqId, NewPids});
	_ ->
	    continue
    end,
    del_locks2(Tail, Pid);
del_locks2([],Pid) -> 
    done.
	       


%% Unregister all Name/Pid pairs such that node(Pid) == Node
%% and delete all locks where node(Pid) == Node
do_node_down(Node) ->
    do_node_down_names(Node, ets:tab2list(global_names)),
    do_node_down_names_ext(Node, ets:tab2list(global_names_ext)),
    do_node_down_locks(Node, ets:tab2list(global_locks)).

do_node_down_names(Node, [{Name, Pid, _Method} | T]) when node(Pid) == Node ->
    ets:delete(global_names, Name),
    do_node_down_names(Node, T);
do_node_down_names(Node, [_|T]) ->
    do_node_down_names(Node, T);
do_node_down_names(_, []) -> ok.

%%remove all external names registered on the crashed node
do_node_down_names_ext(Node, [{Name, Pid, Node} | T])  ->
    ets:delete(global_names, Name),
    ets:delete(global_names_ext, Name),
    do_node_down_names_ext(Node, T);
do_node_down_names_ext(Node, [_|T]) ->
    do_node_down_names_ext(Node, T);
do_node_down_names_ext(_, []) -> ok.

do_node_down_locks(Node, [{ResourceId, LockReqId, Pids} | T]) ->
    case do_node_down_locks2(Pids, Node) of
	[] ->
	    continue;
	RemovePids ->
	    case Pids -- RemovePids of
		[] ->
		    ets:delete(global_locks, ResourceId);
		NewPids ->
		    ets:insert(global_locks, {ResourceId, LockReqId, NewPids})
	    end
    end,
    do_node_down_locks(Node, T);
do_node_down_locks(Node, [_|T]) ->
    do_node_down_locks(Node, T);
do_node_down_locks(_, []) -> done.


do_node_down_locks2(Pids, Node) ->
    do_node_down_locks2(Pids, Node, []).

do_node_down_locks2([], Node, Res) -> 
    Res;
do_node_down_locks2([Pid | Pids], Node, Res) when node(Pid) == Node ->
    do_node_down_locks2(Pids, Node, [Pid | Res]);
do_node_down_locks2([_ | Pids], Node, Res) ->
    do_node_down_locks2(Pids, Node, Res).


get_names() ->
    ets:tab2list(global_names).

get_names_ext() ->
    ets:tab2list(global_names_ext).

%% get global_names_ext from another node.
get_names_ext([]) ->
    [];
get_names_ext([Node | T]) ->
    case rpc:call(Node, ets, tab2list, [global_names_ext]) of
	{badrpc,nodedown} ->
	    get_names_ext(T);
	List ->
	    List
    end.

random_sleep(Times) ->
    case (Times rem 10) of
	0 -> erase(random_seed);
	_ -> ok
    end,
    case get(random_seed) of
	undefined ->
	    {A1, A2, A3} = now(),
	    random:seed(A1, A2, A3 + erlang:hash(node(), 100000));
	_ -> ok
    end,
    receive after random:uniform(1000*Times) -> ok end.

dec(infinity) -> infinity;
dec(N) -> N-1.

send_again(Msg) ->
    spawn_link(?MODULE, timer, [self(), Msg]).

timer(Pid, Msg) ->
    random_sleep(5),
    Pid ! Msg.

change_our_node_name(NewNode, S) ->
    S#state{node_name = NewNode}.




%%-----------------------------------------------------------------
%% Check protocol version of the Nodes
%% This must be done by checking if the global_names_ext exists
%% because of deadlock if 'get_protocol_version' is called
%% global_names_ext exists => vsn 2
%% global_names_ext does not exist => vsn 1
%%-----------------------------------------------------------------
check_prot_vsn(Nodes) ->
    check_prot_vsn(Nodes,[], []).

check_prot_vsn([], V2, Vx) ->
    {V2, Vx};
check_prot_vsn([N|Nodes], V2, Vx) ->
    case rpc:call(N, ets, all, []) of
	{badrpc,nodedown} ->
	    check_prot_vsn(Nodes, V2, [N|Vx]);
	Tabs ->
	    case lists:keymember(global_names_ext, 1, Tabs) of
		true ->
		    check_prot_vsn(Nodes, [N|V2], Vx);
		false ->
		    check_prot_vsn(Nodes, V2, Vx)
	    end
    end.


%%-----------------------------------------------------------------
%% Each sync process corresponds to one call to sync.  Each such
%% process asks the global_name_server on all Nodes if it is in sync
%% with Nodes.  If not, that (other) node spawns a syncer process that
%% waits for global to get in sync with all Nodes.  When it is in
%% sync, the syncer process tells the original sync process about it.
%%-----------------------------------------------------------------
start_sync(Nodes, From) ->
    spawn_link(?MODULE, sync_init, [Nodes, From]).

sync_init(Nodes, From) ->
    lists:foreach(fun(Node) -> monitor_node(Node, true) end, Nodes),
    sync_loop(Nodes, From).

sync_loop([], From) ->
    gen_server:reply(From, ok);
sync_loop(Nodes, From) ->
    receive
	{nodedown, Node} ->
	    monitor_node(Node, false),
	    sync_loop(lists:delete(Node, Nodes), From);
	{synced, SNodes} ->
	    lists:foreach(fun(N) -> monitor_node(N, false) end, SNodes),
	    sync_loop(Nodes -- SNodes, From)
    end.


%%%====================================================================================
%%% Get the current global_groups definition
%%%====================================================================================
check_sync_nodes() ->
    case get_own_nodes() of
	{ok, all} ->
	    nodes();
	{ok, NodesNG} ->
	    %% global_groups parameter is defined, we are not allowed to sync
	    %% with nodes not in our own global group.
	    (nodes() -- (nodes() -- NodesNG));
	{error, Error} ->
	    {error, Error}
    end.

check_sync_nodes(SyncNodes) ->
    case get_own_nodes() of
	{ok, all} ->
	    SyncNodes;
	{ok, NodesNG} ->
	    %% global_groups parameter is defined, we are not allowed to sync
	    %% with nodes not in our own global group.
	    OwnNodeGroup = (nodes() -- (nodes() -- NodesNG)),
	    IllegalSyncNodes = (SyncNodes -- [node() | OwnNodeGroup]),
	    case IllegalSyncNodes of
		[] -> SyncNodes;
		_ -> {error, {"Trying to sync nodes not defined in the own global group", 
			      IllegalSyncNodes}}
	    end;
	{error, Error} ->
	    {error, Error}
    end.

get_own_nodes() ->
    case application:get_env(kernel, global_groups) of
	undefined ->
	    {ok, all};
	{ok, []} ->
	    {ok, all};
	{ok, NodeGrps} ->
	    case catch global_group:config_scan(NodeGrps) of
		{error, Error2} ->
		    {error, {"global_groups definition error", Error2}};
		{Group_NameDef, NodesDef, OtherDef} ->
		    {ok, NodesDef}
	    end
    end.

