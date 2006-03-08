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
%%     $Id $
%%
-module(global).
-behaviour(gen_server).

%% Global provides global registration of process names. The names are
%% dynamically kept up to date with the entire network. Global can
%% operate in two modes: in a fully connected network, or in a
%% non-fully connected network. In the latter case, the name
%% registration mechanism won't work. 
%% As a separate service Global also provides global locks.

%% External exports
-export([start/0, start_link/0, stop/0, sync/0, sync/1,
	 safe_whereis_name/1, whereis_name/1,  register_name/2, 
         register_name/3, register_name_external/2, register_name_external/3,
         unregister_name_external/1,re_register_name/2, re_register_name/3,
	 unregister_name/1, registered_names/0, send/2, node_disconnected/1,
	 set_lock/1, set_lock/2, set_lock/3,
	 del_lock/1, del_lock/2,
	 trans/2, trans/3, trans/4,
	 random_exit_name/3, random_notify_name/3, notify_all_name/3]).

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3, resolve_it/4]).

-export([info/0]).

-include_lib("stdlib/include/ms_transform.hrl").

-define(WARN_DUPLICATED_NAME, global_multi_name_action).

%% Undocumented Kernel variable. Set this to 0 (zero) to get the old
%% behaviour.
-define(N_CONNECT_RETRIES, global_connect_retries).
-define(DEFAULT_N_CONNECT_RETRIES, 5).

%%% In certain places in the server, calling io:format hangs everything,
%%% so we'd better use erlang:display/1.
%%% my_tracer is used in testsuites
-define(trace(_), ok).

%-define(trace(T), (catch my_tracer ! {node(), {line,?LINE}, T})).

%-define(trace(T), erlang:display({format, node(), cs(), T})).
%cs() ->
%    {_Big, Small, Tiny} = now(),
%    (Small rem 100) * 100 + (Tiny div 10000).

%% These are the protocol versions:
%% Vsn 1 is the original protocol.
%% Vsn 2 is enhanced with code to take care of registration of names from
%%       non erlang nodes, e.g. C-nodes.
%% Vsn 3 is enhanced with a tag in the synch messages to distinguish
%%       different synch sessions from each other, see OTP-2766.
%% Vsn 4 uses a single, permanent, locker process, but works like vsn 3
%%       when communicating with vsn 3 nodes. Current version of global does
%%       not support vsn 3 nodes.
%% Vsn 5 uses an ordered list of self() and HisTheLocker when locking
%%       nodes in the own partition.

-define(vsn, 5).

%%-----------------------------------------------------------------
%% connect_all = bool() - true if we are supposed to set up a
%%                        fully connected net
%% known       = [Node] - all nodes known to us
%% synced      = [Node] - all nodes that have the same names as us
%% resolvers   = [{Node, MyTag, Resolver}] - 
%%                        the tag separating different synch sessions, 
%%                        and the pid of the name resolver process
%% syncers     = [pid()] - all current syncers processes
%% node_name   = atom()  - our node name (can change if distribution
%%                         is started/stopped dynamically)
%%
%% In addition to these, we keep info about messages arrived in
%% the process dictionary:
%% {pre_connect, Node} = {Vsn, InitMsg} - init_connect msgs that
%%                         arrived before nodeup
%% {wait_lock, Node}   = {exchange, NameList, _NamelistExt} | lock_is_set
%%                        - see comment below (handle_cast)
%% {save_ops, Node}    = {resolved, HisKnown, NamesExt, Res} | [operation()] 
%%                        - save the ops between exchange and resolved
%% {prot_vsn, Node}    = Vsn - the exchange protocol version (not used now)
%% {sync_tag_my, Node} =  My tag, used at synchronization with Node
%% {sync_tag_his, Node} = The Node's tag, used at synchronization
%%-----------------------------------------------------------------
-record(state, {connect_all, known = [], synced = [],
		resolvers = [], syncers = [], node_name = node(),
		the_locker, the_deleter}).

%%% There are also ETS tables used for bookkeeping of locks and names
%%% (the first position is the key):
%%%
%%% global_locks (set): {ResourceId, LockRequesterId, Pids}
%%%   Pids is a list of all pids locking ResourceId.
%%% global_names (set):  {Name, Pid, Method}
%%%   Registered names.
%%% global_names_ext (set): {Name, Pid, RegNode}
%%%   External registered names (C-nodes).
%%% 
%%% Helper tables:
%%% global_pid_names (bag): {Pid, Name}
%%%   Name(s) registered for Pid.
%%% global_pid_ids (bag): {Pid, ResourceId}
%%%   Resources locked by Pid.
%%% global_node_pids (duplicated_bag): {{lock, node(Pid)}, Pid} or 
%%%                                    {{name, Node},Pid}, 
%%%                     where Node is a node where some name is registered
%%%   Pids affected by a nodedown.
%%%
%%% global_pid_names is a 'bag' for backward compatibility.
%%% global_node_pids is a 'duplicate_bag' for the same reason.
%%% (Before vsn 5 more than one name could be registered for a process.)

start() -> 
    gen_server:start({local, global_name_server}, ?MODULE, [], []).

start_link() -> 
    gen_server:start_link({local, global_name_server}, ?MODULE, [], []).

stop() -> 
    gen_server:call(global_name_server, stop, infinity).

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
	Pid when is_pid(Pid) ->
	    Pid ! Msg,
	    Pid;
	undefined ->
	    exit({badarg, {Name, Msg}})
    end.

%% See OTP-3737.
whereis_name(Name) ->
    where(Name).

safe_whereis_name(Name) ->
    gen_server:call(global_name_server, {whereis, Name}, infinity).

node_disconnected(Node) ->
    global_name_server ! {nodedown, Node}.

%%-----------------------------------------------------------------
%% Method = function(Name, Pid1, Pid2) -> Pid | Pid2 | none
%% Method is called if a name conflict is detected when two nodes
%% are connecting to each other. It is supposed to return one of
%% the Pids or 'none'. If a pid is returned, that pid is
%% registered as Name on all nodes. If 'none' is returned, the
%% Name is unregistered on all nodes. If anything else is returned,
%% the Name is unregistered as well.
%% Method is called once at one of the nodes where the processes reside
%% only. If different Methods are used for the same name, it is
%% undefined which one of them is used.
%% Method blocks the name registration, but does not affect global locking.
%%-----------------------------------------------------------------
register_name(Name, Pid) when is_pid(Pid) ->
    register_name(Name, Pid, {?MODULE, random_exit_name}).

register_name(Name, Pid, Method) when is_pid(Pid) ->
    trans_all_known(fun(Nodes) ->
		  case where(Name) of
		      undefined ->
			  gen_server:multi_call(Nodes,
						global_name_server,
						{register, Name, Pid, Method}),
			  yes;
		      _Pid -> no
		  end
	  end).

unregister_name(Name) ->
    case where(Name) of
	undefined ->
	    ok;
	_ ->
	    trans_all_known(fun(Nodes) ->
			  gen_server:multi_call(Nodes,
						global_name_server,
						{unregister, Name}),
			  ok
		  end)
    end.

re_register_name(Name, Pid) when is_pid(Pid) ->
    re_register_name(Name, Pid, {?MODULE, random_exit_name}).

re_register_name(Name, Pid, Method) when is_pid(Pid) ->
    trans_all_known(fun(Nodes) ->
		  gen_server:multi_call(Nodes,
					global_name_server,
					{register, Name, Pid, Method}),
		  yes
	  end).

registered_names() ->
    ets:select(global_names, ets:fun2ms(fun({Name,_Pid,_Meth}) -> Name end)).

%%-----------------------------------------------------------------
%% The external node (e.g. a C-node) registers the name on an Erlang
%% node which links to the process (an Erlang node has to be used
%% since there is no global_name_server on the C-node). If the Erlang
%% node dies the name is to be unregistered on all nodes. Normally
%% node(Pid) is compared to the node that died, but that does not work
%% for external nodes (the process does not run on the Erlang node
%% that died). Therefore a table of all names registered by external
%% nodes is kept up-to-date on all nodes.
%%
%% Note: if the Erlang node dies an EXIT signal is also sent to the
%% C-node due to the link between the global_name_server and the
%% registered process.
%%-----------------------------------------------------------------
register_name_external(Name, Pid) when is_pid(Pid) ->
    register_name_external(Name, Pid, {?MODULE, random_exit_name}).

register_name_external(Name, Pid, Method) when is_pid(Pid) ->
    trans_all_known(fun(Nodes) ->
		  case where(Name) of
		      undefined ->
			  gen_server:multi_call(Nodes,
						global_name_server,
						{register_ext, Name, Pid, 
                                                 Method, node()}),
			  yes;
		      _Pid -> no
		  end
	  end).

unregister_name_external(Name) ->
    unregister_name(Name).

set_lock(Id) ->
    set_lock(Id, [node() | nodes()], infinity, 1).

set_lock(Id, Nodes) ->
    set_lock(Id, Nodes, infinity, 1).

set_lock(Id, Nodes, Retries) when is_integer(Retries), Retries >= 0 ->
    set_lock(Id, Nodes, Retries, 1);
set_lock(Id, Nodes, infinity) ->
    set_lock(Id, Nodes, infinity, 1).

set_lock({_ResourceId, _LockRequesterId}, [], _Retries, _Times) ->
    true;
set_lock({_ResourceId, _LockRequesterId} = Id, Nodes, Retries, Times) ->
    ?trace({set_lock,{me,self()},Id,{nodes,Nodes},
            {retries,Retries}, {times,Times}}),
    case set_lock_on_nodes(Id, Nodes) of
	true -> 
            ?trace({set_lock_true, Id}),
            true;
        false=Reply when Retries =:= 0 ->
            Reply;
	false ->
	    random_sleep(Times),
	    set_lock(Id, Nodes, dec(Retries), Times+1)
    end.

del_lock(Id) ->
    del_lock(Id, [node() | nodes()]).

del_lock({ResourceId, LockRequesterId}, Nodes) ->
    Id = {ResourceId, LockRequesterId},
    ?trace({del_lock, {me,self()}, {ResourceId,LockRequesterId}, 
            {nodes,Nodes}}),
    gen_server:multi_call(Nodes, global_name_server, {del_lock, Id}),
    true.

trans(Id, Fun) -> trans(Id, Fun, [node() | nodes()], infinity).

trans(Id, Fun, Nodes) -> trans(Id, Fun, Nodes, infinity).

trans(Id, Fun, Nodes, Retries) ->
    case set_lock(Id, Nodes, Retries) of
	true ->
            try 
                Fun()
            after
                del_lock(Id, Nodes)
            end;
	false ->
	    aborted
    end.

info() ->
    gen_server:call(global_name_server, info, infinity).

%%%-----------------------------------------------------------------
%%% Call-back functions from gen_server
%%%-----------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    _ = ets:new(global_locks, [set, named_table, protected]),
    _ = ets:new(global_names, [set, named_table, protected]),
    _ = ets:new(global_names_ext, [set, named_table, protected]),

    _ = ets:new(global_pid_names, [bag, named_table, protected]),
    _ = ets:new(global_pid_ids, [bag, named_table, protected]),
    _ = ets:new(global_node_pids, [duplicate_bag, named_table, protected]),

    S = #state{the_locker = start_the_locker(),
	       the_deleter = start_the_deleter(self())},

    case init:get_argument(connect_all) of
	{ok, [["false"]]} ->
	    {ok, S#state{connect_all = false}};
	_ ->
	    {ok, S#state{connect_all = true}}
    end.

%%-----------------------------------------------------------------
%% Connection algorithm
%% ====================
%% This algorithm solves the problem with partitioned nets as well.
%%
%% The main idea in the algorithm is that when two nodes connect, they
%% try to set a lock in their own partition (i.e. all nodes already
%% known to them; partitions are not necessarily disjoint). When the
%% lock is set in each partition, these two nodes send each other a
%% list with all registered names in resp partition (*). If no conflict
%% is found, the name tables are just updated. If a conflict is found,
%% a resolve function is called once for each conflict. The result of
%% the resolving is sent to the other node. When the names are
%% exchanged, all other nodes in each partition are informed of the
%% other nodes, and they ping each other to form a fully connected
%% net.
%%
%% A few remarks:
%% 
%% (*) When this information is being exchanged, no one is allowed to
%%     change the global register table. All calls to register etc are
%%     protected by a lock. If a registered process dies during this
%%     phase the name is unregistered on the local node immediately,
%%     but the unregistration on other nodes will take place when the
%%     deleter manages to acquire the lock. This is necessary to
%%     prevent names from spreading to nodes where they cannot be
%%     deleted.
%%
%% - It is assumed that nodeups and nodedowns arrive in an orderly
%%   fashion: for every node, nodeup is followed by nodedown, and vice
%%   versa. "Double" nodeups and nodedowns must never occur. It is
%%   the responsibility of net_kernel to assure this.
%%
%% - There is always a delay between the termination of a registered
%%   process and the removal of the name from Global's tables. This
%%   delay can sometimes be quite substantial. Global guarantees that
%%   the name will eventually be removed, but there is no
%%   synchronization between nodes; the name can be removed from some
%%   node(s) long before it is removed from other nodes. Using
%%   safe_whereis_name is no cure.
%%
%% - Global cannot handle problems with the distribution very well.
%%   Depending on the value of the kernel variable 'net_ticktime' long
%%   delays may occur. This does not affect the handling of locks but
%%   will block name registration.
%% 
%% - Old synch session messages may linger on in the message queue of
%%   global_name_server after the sending node has died. The tags of
%%   such messages do not match the current tag (if there is one),
%%   which makes it possible to discard those messages and cancel the
%%   corresponding lock.
%%
%% Suppose nodes A and B connect, and C is connected to A.
%% Here's the algorithm's flow:
%%
%% Node A
%% ------
%% << {nodeup, B}
%%   TheLocker ! {nodeup, ..., Node, ...} (there is one locker per node)
%% B ! {init_connect, ..., {..., TheLockerAtA, ...}}
%% << {init_connect, TheLockerAtB}
%%   [The lockers try to set the lock]
%% << {lock_is_set, B, ...}
%%   [Now, lock is set in both partitions]
%% B ! {exchange, A, Names, ...}
%% << {exchange, B, Names, ...}
%%   [solve conflict]
%% B ! {resolved, A, ResolvedA, KnownAtA, ...}
%% << {resolved, B, ResolvedB, KnownAtB, ...}
%% C ! {new_nodes, ResolvedAandB, [B]}
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
%% First, the init_connect message may arrive _before_ the nodeup
%% message due to delay in net_kernel. We handle this by keeping track
%% of these messages in the pre_connect variable in our state.
%%
%% Of course we must handle that some node goes down during the
%% connection.
%%
%%-----------------------------------------------------------------
%% Messages in the protocol
%% ========================
%% 1. Between global_name_servers on connecting nodes
%%    {init_connect, Vsn, Node, InitMsg}
%%         InitMsg = {locker, _Unused, HisKnown, HisTheLocker}
%%    {exchange, Node, ListOfNames, _ListOfNamesExt, Tag}
%%    {resolved, Node, HisOps, HisKnown, _Unused, ListOfNamesExt, Tag}
%%         HisKnown = list of known nodes in Node's partition
%% 2. Between lockers on connecting nodes
%%    {his_locker, Pid} (from our global)
%%    {lock, Bool} loop until both lockers have lock = true,
%%          then send to global_name_server {lock_is_set, Node, Tag}
%% 3. Connecting node's global_name_server informs other nodes in the same 
%%    partition about hitherto unknown nodes in the other partition
%%    {new_nodes, _Node, Ops, ListOfNamesExt, NewNodes, _Unused}
%% 4. Between global_name_server and resolver
%%    {resolve, NameList, Node} to resolver
%%    {exchange_ops, Node, Tag, Ops, Resolved} from resolver
%% 5. sync protocol, between global_name_servers in different partitions
%%    {in_sync, Node, IsKnown}
%%          sent by each node to all new nodes (Node becomes known to them)
%%-----------------------------------------------------------------

handle_call({whereis, Name}, From, S) ->
    do_whereis(Name, From),
    {noreply, S};

handle_call({register, Name, Pid, Method}, _From, S) ->
    ins_name(Name, Pid, Method),
    {reply, yes, S};

handle_call({unregister, Name}, _From, S) ->
    delete_global_name(Name),
    {reply, ok, S};

handle_call({register_ext, Name, Pid, Method, RegNode}, _F, S) ->
    ins_name_ext(Name, Pid, Method, RegNode),
    {reply, yes, S};

handle_call({set_lock, Lock}, {Pid, _Tag}, S) ->
    Reply = handle_set_lock(Lock, Pid),
    {reply, Reply, S};

handle_call({del_lock, Lock}, {Pid, _Tag}, S) ->
    handle_del_lock(Lock, Pid),
    {reply, true, S};

handle_call(get_known, _From, S) ->
    {reply, S#state.known, S};

handle_call(get_synced, _From, S) ->
    {reply, S#state.synced, S};

handle_call({sync, Nodes}, From, S) ->
    %% If we have several global groups, this won't work, since we will
    %% do start_sync on a nonempty list of nodes even if the system
    %% is quiet.
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

%%========================================================================
%% init_connect
%%
%%========================================================================
handle_cast({init_connect, Vsn, Node, InitMsg}, S) ->
    %% Sent from global_name_server at Node.
    ?trace({'####', init_connect, {vsn, Vsn}, {node,Node},{initmsg,InitMsg}}),
    case Vsn of
	%% It is always the responsibility of newer versions to understand
	%% older versions of the protocol.
	{HisVsn, HisTag} when HisVsn > ?vsn ->
	    init_connect(?vsn, Node, InitMsg, HisTag, S#state.resolvers, S);
	{HisVsn, HisTag} ->
	    init_connect(HisVsn, Node, InitMsg, HisTag, S#state.resolvers, S);
	%% To be future compatible
	Tuple when is_tuple(Tuple) ->
	    List = tuple_to_list(Tuple),
	    [_HisVsn, HisTag | _] = List,
	    %% use own version handling if his is newer.
	    init_connect(?vsn, Node, InitMsg, HisTag, S#state.resolvers, S);
	_ ->
	    Txt = io_lib:format("Illegal global protocol version ~p Node: ~p\n",
                                [Vsn, Node]),
	    error_logger:info_report(lists:flatten(Txt))
    end,
    {noreply, S};

%%=======================================================================
%% lock_is_set
%%
%% Ok, the lock is now set on both partitions. Send our names to other node.
%%=======================================================================
handle_cast({lock_is_set, Node, MyTag}, S) ->
    %% Sent from the_locker at node().
    ?trace({'####', lock_is_set , {node,Node}}),
    case get({sync_tag_my, Node}) of
	MyTag ->
            lock_is_set(Node, S#state.resolvers),
            {noreply, S};
	_ -> %% Illegal tag, delete the old sync session.
	    NewS = cancel_locker(Node, S, MyTag),
	    {noreply, NewS}
    end;

%%========================================================================
%% exchange
%%
%% Here the names are checked to detect name clashes.
%%========================================================================
handle_cast({exchange, Node, NameList, _NameExtList, MyTag}, S) ->
    %% Sent from global_name_server at Node.
    case get({sync_tag_my, Node}) of
	MyTag ->
	    exchange(Node, NameList, S#state.resolvers),
	    {noreply, S};
	_ -> %% Illegal tag, delete the old sync session.
	    NewS = cancel_locker(Node, S, MyTag),
	    {noreply, NewS}
    end;

%% {exchange_ops, ...} is sent by the resolver process (which then
%% dies). It could happen that {resolved, ...} has already arrived
%% from the other node. In that case we can go ahead and run the
%% resolve operations. Otherwise we have to save the operations and
%% wait for {resolve, ...}. This is very much like {lock_is_set, ...}
%% and {exchange, ...}.
handle_cast({exchange_ops, Node, MyTag, Ops, Resolved}, S) ->
    %% Sent from the resolver for Node at node().
    ?trace({exchange_ops, {node,Node}, {ops,Ops},{resolved,Resolved},
            {mytag,MyTag}}),
    case get({sync_tag_my, Node}) of
	MyTag ->
            Known = S#state.known,
	    gen_server:cast({global_name_server, Node},
			    {resolved, node(), Resolved, Known,
			     Known,get_names_ext(),get({sync_tag_his,Node})}),
            case get({save_ops, Node}) of
                {resolved, HisKnown, Names_ext, HisResolved} ->
                    put({save_ops, Node}, Ops),
                    NewS = resolved(Node, HisResolved, HisKnown, Names_ext,S),
                    {noreply, NewS};
                undefined -> 
                    put({save_ops, Node}, Ops),
                    {noreply, S}
            end;
	_ -> %% Illegal tag, delete the old sync session.
	    NewS = cancel_locker(Node, S, MyTag),
	    {noreply, NewS}
    end;

%%========================================================================
%% resolved
%%
%% Here the name clashes are resolved.
%%========================================================================
handle_cast({resolved, Node, HisResolved, HisKnown, _HisKnown_v2, 
             Names_ext, MyTag}, S) ->
    %% Sent from global_name_server at Node.
    ?trace({'####', resolved, {his_resolved,HisResolved}, {node,Node}}),
    case get({sync_tag_my, Node}) of
	MyTag -> 
            %% See the comment at handle_case({exchange_ops, ...}).
            case get({save_ops, Node}) of
                Ops when is_list(Ops) ->
                    NewS = resolved(Node, HisResolved, HisKnown, Names_ext, S),
                    {noreply, NewS};
                undefined ->
                    Resolved = {resolved, HisKnown, Names_ext, HisResolved},
                    put({save_ops, Node}, Resolved),
                    {noreply, S}
            end;
	_ -> %% Illegal tag, delete the old sync session.
	    NewS = cancel_locker(Node, S, MyTag),
	    {noreply, NewS}
    end;

%%========================================================================
%% new_nodes
%%
%% We get to know the other node's known nodes.
%%========================================================================
handle_cast({new_nodes, _Node, Ops, Names_ext, Nodes, _Nodes_v2}, S) ->
    %% Sent from global_name_server at _Node.
    ?trace({new_nodes, {node,_Node}, {ops,Ops}, {nodes,Nodes}}),
    NewS = new_nodes(Ops, Names_ext, Nodes, S),
    {noreply, NewS};

%%========================================================================
%% in_sync
%%
%% We are in sync with this node (from the other node's known world).
%%========================================================================
handle_cast({in_sync, Node, _IsKnown}, S) ->
    %% Sent from global_name_server at Node (in the other partition).
    ?trace({'####', in_sync, {Node, _IsKnown}}),
    lists:foreach(fun(Pid) -> Pid ! {synced, [Node]} end, S#state.syncers),
    NewS = cancel_locker(Node, S, get({sync_tag_my, Node})),
    reset_node_state(Node),
    NSynced = case lists:member(Node, Synced = NewS#state.synced) of
		  true -> Synced;
		  false -> [Node | Synced]
	      end,
    {noreply, NewS#state{synced = NSynced}};

%% Called when Pid on other node crashed
handle_cast({async_del_name, Name, Pid}, S) ->
    %% Sent from the_deleter at some node in the partition but node().
    case ets:lookup(global_names, Name) of
	[{Name, Pid, _}] ->
            delete_global_name(Name, Pid);
	_ -> ok
    end,
    {noreply, S};

handle_cast({async_del_lock, _ResourceId, Pid}, S) ->
    %% Sent from global_name_server at some node in the partition but node().
    async_del_lock(Pid),
    {noreply, S}.

handle_info({'EXIT', Deleter, _Reason}=Exit, #state{the_deleter=Deleter}=S) ->
    {stop, {deleter_died,Exit}, S#state{the_deleter=undefined}};
handle_info({'EXIT', Pid, _Reason}, S) when is_pid(Pid) ->
    ?trace({global_EXIT,Pid}),
    %% The process that died was either a synch process started by
    %% start_sync or a registered process.
    #state{the_deleter=Deleter, known = Known} = S,
    check_exit(Deleter, Pid, Known),
    Syncers = lists:delete(Pid, S#state.syncers),
    {noreply, S#state{syncers = Syncers}};

handle_info({nodedown, Node}, S) when Node =:= S#state.node_name ->
    %% Somebody stopped the distribution dynamically - change
    %% references to old node name (Node) to new node name ('nonode@nohost')
    {noreply, change_our_node_name(node(), S)};

handle_info({nodedown, Node}, S) ->
    ?trace({'####', nodedown, {node,Node}}),
    do_node_down(Node),
    #state{known = Known, synced = Syncs} = S,
    NewS = cancel_locker(Node, S, get({sync_tag_my, Node})),
    NewS#state.the_locker ! {remove_from_known, Node},
    reset_node_state(Node),
    {noreply, NewS#state{known = lists:delete(Node, Known),
			 synced = lists:delete(Node, Syncs)}};

handle_info({nodeup, Node}, S) when Node =:= node() ->
    ?trace({'####', local_nodeup, {node, Node}}),
    %% Somebody started the distribution dynamically - change
    %% references to old node name ('nonode@nohost') to Node.
    {noreply, change_our_node_name(Node, S)};

handle_info({nodeup, Node}, S) when S#state.connect_all ->
    IsKnown = lists:member(Node, S#state.known) or
              %% This one is only for double nodeups (shouldn't occur!)
              lists:keymember(Node, 1, S#state.resolvers),
    ?trace({'####', nodeup, {node,Node}, {isknown,IsKnown}}),
    case IsKnown of
	true ->
	    {noreply, S};
	false ->
	    resend_pre_connect(Node),

	    %% now() is used as a tag to separate different sycnh sessions
	    %% from each others. Global could be confused at bursty nodeups
	    %% because it couldn't separate the messages between the different
	    %% synch sessions started by a nodeup.
	    MyTag = now(),
	    put({sync_tag_my, Node}, MyTag),
            ?trace({sending_nodeup_to_locker, {node,Node},{mytag,MyTag}}),
	    S#state.the_locker ! {nodeup, Node, MyTag},

            %% In order to be compatible with unpatched R7 a locker
            %% process was spawned. Vsn 5 is no longer comptabible with
            %% vsn 3 nodes, so the locker process is no longer needed.
            %% The permanent locker takes its place.
            NotAPid = no_longer_a_pid,
            Locker = {locker, NotAPid, S#state.known, S#state.the_locker},
	    InitC = {init_connect, {?vsn, MyTag}, node(), Locker},
	    Rs = S#state.resolvers,
            ?trace({casting_init_connect, {node,Node},{initmessage,InitC},
                    {resolvers,Rs}}),
	    gen_server:cast({global_name_server, Node}, InitC),
            Resolver = start_resolver(Node, MyTag),
	    {noreply, S#state{resolvers = [{Node, MyTag, Resolver} | Rs]}}
    end;

handle_info({whereis, Name, From}, S) ->
    do_whereis(Name, From),
    {noreply, S};

handle_info(known, S) ->
    io:format(">>>> ~p\n",[S#state.known]),
    {noreply, S};

handle_info(_Message, S) ->
    {noreply, S}.

%%========================================================================
%%========================================================================
%%=============================== Internal Functions =====================
%%========================================================================
%%========================================================================

-define(GLOBAL_RID, global).

%% Similar to trans(Id, Fun), but always uses global's own lock
%% on all nodes known to global, making sure that no new nodes have
%% become known while we got the list of known nodes.
trans_all_known(Fun) ->
    Id = {?GLOBAL_RID, self()},
    Nodes = set_lock_known(Id, 0),
    try
        Fun(Nodes)
    after
        del_lock(Id, Nodes)
    end.

set_lock_known(Id, Times) -> 
    Known = get_known(),
    Nodes = [node() | Known],
    Boss = lists:max(Nodes),
    %% Use the  same convention (a boss) as lock_nodes_safely. Optimization.
    case set_lock_on_nodes(Id, [Boss]) of
        true ->
            case lock_on_known_nodes(Id, Known, Nodes) of
                true ->
                    Nodes;
                false -> 
                    del_lock(Id, [Boss]),
                    random_sleep(Times),
                    set_lock_known(Id, Times+1)
            end;
        false ->
            random_sleep(Times),
            set_lock_known(Id, Times+1)
    end.

lock_on_known_nodes(Id, Known, Nodes) ->
    case set_lock_on_nodes(Id, Nodes) of
        true ->
            (get_known() -- Known) =:= [];
        false ->
            false
    end.

set_lock_on_nodes(_Id, []) ->
    true;
set_lock_on_nodes(Id, Nodes) ->
    case local_lock_check(Id, Nodes) of 
        true ->
            Msg = {set_lock, Id},
            {Replies, _} = 
                gen_server:multi_call(Nodes, global_name_server, Msg),
            ?trace({set_lock,{me,self()},Id,{nodes,Nodes},{replies,Replies}}),
            check_replies(Replies, Id, Replies);
        false=Reply ->
            Reply
    end.

%% Probe lock on local node to see if one should go on trying other nodes.
local_lock_check(_Id, [_] = _Nodes) ->
    true;
local_lock_check(Id, Nodes) ->
    not lists:member(node(), Nodes) orelse (can_set_lock(Id) =/= false).

check_replies([{_Node, true} | T], Id, Replies) ->
    check_replies(T, Id, Replies);
check_replies([{_Node, false=Reply} | _T], _Id, [_]) ->
    Reply;
check_replies([{_Node, false=Reply} | _T], Id, Replies) ->
    TrueReplyNodes = [N || {N, true} <- Replies],
    ?trace({check_replies, {true_reply_nodes, TrueReplyNodes}}),
    gen_server:multi_call(TrueReplyNodes, global_name_server, {del_lock, Id}),
    Reply;
check_replies([], _Id, _Replies) ->
    true.

%%========================================================================
%% Another node wants to synchronize its registered names with us.
%% Both nodes must have a lock before they are allowed to continue.
%%========================================================================
init_connect(Vsn, Node, InitMsg, HisTag, Resolvers, S) ->
    %% It is always the responsibility of newer versions to understand
    %% older versions of the protocol.
    put({prot_vsn, Node}, Vsn),
    put({sync_tag_his, Node}, HisTag),
    case lists:keysearch(Node, 1, Resolvers) of
        {value, {Node, MyTag, _Resolver}} ->
            MyTag = get({sync_tag_my, Node}), % assertion
            case InitMsg of
                {locker, HisLocker, HisKnown} -> %% before vsn 5
                    ?trace({old_init_connect,{histhelocker,HisLocker}}),
                    HisLocker ! {his_locker_new, S#state.the_locker,
                                 {HisKnown, S#state.known}};
                
                {locker, _NoLongerAPid, HisKnown, HisTheLocker} -> %% vsn 5
                    ?trace({init_connect,{histhelocker,HisTheLocker}}),
                    S#state.the_locker ! {his_the_locker, HisTheLocker,
                                          {Vsn,HisKnown}, S#state.known}
            end;
        false ->
            ?trace({init_connect,{pre_connect,Node},{histag,HisTag}}),
            put({pre_connect, Node}, {Vsn, InitMsg, HisTag})
    end.

%%========================================================================
%% In the simple case, we'll get lock_is_set before we get exchange,
%% but we may get exchange before we get lock_is_set from our locker.
%% If that's the case, we'll have to remember the exchange info, and
%% handle it when we get the lock_is_set. We do this by using the
%% process dictionary - when the lock_is_set msg is received, we store
%% this info. When exchange is received, we can check the dictionary
%% if the lock_is_set has been received. If not, we store info about
%% the exchange instead. In the lock_is_set we must first check if
%% exchange info is stored, in that case we take care of it.
%%========================================================================
lock_is_set(Node, Resolvers) ->
    gen_server:cast({global_name_server, Node},
                    {exchange, node(), get_names(), _ExtNames = [],
                     get({sync_tag_his, Node})}),
    %% If both have the lock, continue with exchange.
    case get({wait_lock, Node}) of
	{exchange, NameList} ->
	    put({wait_lock, Node}, lock_is_set),
	    exchange(Node, NameList, Resolvers);
	undefined ->
	    put({wait_lock, Node}, lock_is_set)
    end.

%%========================================================================
%% exchange
%%========================================================================
exchange(Node, NameList, Resolvers) ->
    ?trace({'####', exchange, {node,Node}, {namelist,NameList}, 
            {resolvers, Resolvers}}),
    case erase({wait_lock, Node}) of
	lock_is_set ->
            {value, {Node, _Tag, Resolver}} = 
                lists:keysearch(Node, 1, Resolvers),
            Resolver ! {resolve, NameList, Node};
	undefined ->
	    put({wait_lock, Node}, {exchange, NameList})
    end.

resolved(Node, HisResolved, HisKnown, Names_ext, S) ->
    Ops = erase({save_ops, Node}) ++ HisResolved,
    %% Known may have shrunk since the lock was taken (due to nodedowns).
    Known = S#state.known,
    Synced = S#state.synced,
    NewNodes = [Node | HisKnown],
    sync_others(HisKnown),
    do_ops(Ops, Names_ext),
    gen_server:abcast(Known, global_name_server,
		      {new_nodes, node(), Ops, Names_ext, NewNodes,NewNodes}),
    %% I am synced with Node, but not with HisKnown yet
    lists:foreach(fun(Pid) -> Pid ! {synced, [Node]} end, S#state.syncers),
    NewS = lists:foldl(fun(Node1, S1) -> 
                               Tag1 = get({sync_tag_my, Node1}),
                               ?trace({calling_cancel_locker,Tag1,get()}),
                               S2 = cancel_locker(Node1, S1, Tag1),
                               reset_node_state(Node1),
                               S2
                       end, S, NewNodes),
    %% See (*) below... we're node b in that description
    AddedNodes = (NewNodes -- Known),
    NewKnown = Known ++ AddedNodes,
    NewS#state.the_locker ! {add_to_known, AddedNodes},
    NewS#state{known = NewKnown, synced = [Node | Synced]}.

new_nodes(Ops, Names_ext, Nodes, S) ->
    Known = S#state.known,
    %% (*) This one requires some thought...
    %% We're node a, other nodes b and c:
    %% The problem is that {in_sync, a} may arrive before {resolved, [a]} to
    %% b from c, leading to b sending {new_nodes, [a]} to us (node a).
    %% Therefore, we make sure we never get duplicates in Known.
    NewNodes = lists:delete(node(), Nodes -- Known),
    sync_others(NewNodes),
    do_ops(Ops, Names_ext),
    ?trace({new_nodes_in_sync,{new_nodes,NewNodes}}),
    S#state.the_locker ! {add_to_known, NewNodes},
    S#state{known = Known ++ NewNodes}.

do_whereis(Name, From) ->
    case is_lock_set(?GLOBAL_RID) of
	false ->
	    gen_server:reply(From, where(Name));
	true ->
	    send_again({whereis, Name, From})
    end.

terminate(_Reason, _S) ->
    true = ets:delete(global_names),
    true = ets:delete(global_names_ext),
    true = ets:delete(global_locks),
    true = ets:delete(global_pid_names),
    true = ets:delete(global_pid_ids),
    true = ets:delete(global_node_pids).

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%% The resolver runs exchange_names in a separate process. The effect
%% is that locks can be used at the same time as name resolution takes
%% place.
start_resolver(Node, MyTag) ->
    spawn(fun() -> resolver(Node, MyTag) end).

resolver(Node, Tag) ->
    receive 
        {resolve, NameList, Node} ->
            ?trace({resolver, {me,self()}, {node,Node}, {namelist,NameList}}),
            {Ops, Resolved} = exchange_names(NameList, Node, [], []),
            Exchange = {exchange_ops, Node, Tag, Ops, Resolved},
            gen_server:cast(global_name_server, Exchange),
            exit(normal);
        _ -> % Ignore garbage.
            resolver(Node, Tag)
    end.

resend_pre_connect(Node) ->
    case erase({pre_connect, Node}) of
	{Vsn, InitMsg, HisTag} ->
	    gen_server:cast(self(), 
                            {init_connect, {Vsn, HisTag}, Node, InitMsg});
	_ ->
	    ok
    end.

ins_name(Name, Pid, Method) ->
    ?trace({ins_name,insert,{name,Name},{pid,Pid}}),
    delete_global_name(Name),
    dolink(Pid),
    insert_global_name_with_check(Name, Pid, Method, node(Pid)).

ins_name_ext(Name, Pid, Method, RegNode) ->
    ?trace({ins_name_ext, {name,Name}, {pid,Pid}}),
    delete_global_name(Name),
    dolink_ext(Pid, RegNode),
    insert_global_name_with_check(Name, Pid, Method, RegNode),
    true = ets:insert(global_names_ext, {Name, Pid, RegNode}).

where(Name) ->
    case ets:lookup(global_names, Name) of
	[{_, Pid, _}] -> Pid;
	[] -> undefined
    end.

handle_set_lock(Id, Pid) ->
    ?trace({handle_set_lock, Id, Pid}),
    case can_set_lock(Id) of
        {true, Pids} ->
	    case lists:member(Pid, Pids) of
		true -> true;
		false -> insert_lock(Id, Pid, Pids)
	    end;
        false=Reply ->
            Reply
    end.

can_set_lock({ResourceId, LockRequesterId}) ->
    case ets:lookup(global_locks, ResourceId) of
	[{ResourceId, LockRequesterId, Pids}] ->
            {true, Pids};
	[{ResourceId, _LockRequesterId2, _Pids}] ->
            false;
	[] ->
            {true, []}
    end.

insert_lock({ResourceId, LockRequesterId}, Pid, Pids) ->
    dolink(Pid),
    true = ets:insert(global_pid_ids, {Pid, ResourceId}),
    true = ets:insert(global_node_pids, {{lock,node(Pid)}, Pid}),
    Lock = {ResourceId, LockRequesterId, [Pid | Pids]},
    true = ets:insert(global_locks, Lock).

is_lock_set(ResourceId) ->
    ets:member(global_locks, ResourceId).

handle_del_lock({ResourceId, LockRequesterId}, Pid) ->
    ?trace({handle_del_lock, {pid,Pid},{id,{ResourceId, LockRequesterId}}}),
    case ets:lookup(global_locks, ResourceId) of
	[{ResourceId, LockRequesterId, Pids}]->
            remove_lock(ResourceId, LockRequesterId, Pid, Pids),
	    dounlink(Pid);
	_ -> ok
    end.

remove_lock(ResourceId, _LockRequesterId, Pid, [Pid]) ->
    ?trace({remove_lock_1, {id,ResourceId},{pid,Pid}}),
    true = ets:delete(global_locks, ResourceId),
    true = ets:delete_object(global_node_pids, {{lock,node(Pid)}, Pid}),
    true = ets:delete_object(global_pid_ids, {Pid, ResourceId});
remove_lock(ResourceId, LockRequesterId, Pid, Pids) ->
    ?trace({remove_lock_2, {id,ResourceId},{pid,Pid}}),
    Lock = {ResourceId, LockRequesterId, lists:delete(Pid, Pids)},
    true = ets:insert(global_locks, Lock),
    true = ets:delete_object(global_node_pids, {{lock,node(Pid)}, Pid}),
    true = ets:delete_object(global_pid_ids, {Pid, ResourceId}).

do_ops(Ops, Names_ext) ->
    ?trace({do_ops, {ops,Ops}}),

    XInserts = [{Name, Pid, RegNode, Method} || 
                   {Name2, Pid2, RegNode} <- Names_ext,
                   {insert, {Name, Pid, Method}} <- Ops,
                   Name =:= Name2, Pid =:= Pid2],
    lists:foreach(fun({Name, Pid, RegNode, Method}) ->
                     ins_name_ext(Name, Pid, Method, RegNode)
                  end, XInserts),

    XNames = [Name || {Name, _Pid, _RegNode, _Method} <- XInserts],
    Inserts = [{Name, Pid, node(Pid), Method} || 
                  {insert, {Name, Pid, Method}} <- Ops,
                  not lists:member(Name, XNames)],
    lists:foreach(fun({Name, Pid, _RegNode, Method}) ->
                     ins_name(Name, Pid, Method)
                  end, Inserts),

    DelNames = [Name || {delete, Name} <- Ops],
    lists:foreach(fun(Name) -> delete_global_name(Name) end, DelNames).

%% It is possible that a node that was up and running when the
%% operations were assembled has since died. The final {in_sync,...}
%% messages do not generate nodedown messages for such nodes. To
%% compensate "artificial" nodedown messages are created which will
%% remove the registered names that were inserted (as well as removing
%% the node names from 'known'). Since monitor_node may take some time
%% processes are spawned to avoid locking up the global_name_server.
%% Should somehow double nodedown messages occur (one of them
%% artificial), nothing bad can happen (the second nodedown is a
%% no-op). It is assumed that there cannot be a nodeup before the
%% artificial nodedown.
%%
%% The extra nodedown messages generated here also take care of the
%% case that a nodedown message is received _before_ the operations
%% are run.
sync_others(Nodes) ->
    N = case application:get_env(kernel, ?N_CONNECT_RETRIES) of
            {ok, NRetries} when NRetries >= 0 -> NRetries;
            _ -> ?DEFAULT_N_CONNECT_RETRIES
        end,
    lists:foreach(fun(Node) -> 
                          spawn(fun() -> sync_other(Node, N) end)
                  end, Nodes).

sync_other(Node, N) ->
    monitor_node(Node, true),
    receive
        {nodedown, Node} when N > 0 ->
            sync_other(Node, N - 1);
        {nodedown, Node} ->
            ?trace({missing_nodedown, {node, Node}}),
            error_logger:warning_msg("global: ~w failed to connect to ~w\n",
                                     [node(), Node]),
            global_name_server ! {nodedown, Node}
    after 0 ->
            gen_server:cast({global_name_server,Node}, {in_sync,node(),true})
    end.
    % monitor_node(Node, false),
    % exit(normal).

insert_global_name_with_check(Name, Pid, Method, Node) ->
    true = ets:insert(global_pid_names, {Pid, Name}),
    true = ets:insert(global_names, {Name, Pid, Method}),
    true = ets:insert(global_node_pids, {{name,Node}, Pid}),
    case application:get_env(kernel, ?WARN_DUPLICATED_NAME) of
        {ok, How} ->
            Spec = ets:fun2ms(fun({Name,Pid0,_}) when Pid0 =:= Pid -> Name 
                              end),
            case ets:select(global_names, Spec) of
                Names when length(Names) > 1 -> dupname(How, Pid, Names);
                _ -> ok
            end;
        _ -> ok
    end.

dupname(How, Pid, Names) ->
    S = "global: ~w registered under several names: ~w~n",
    As = [Pid, Names],
    case How of
        info -> error_logger:info_msg(S, As);
        warning -> error_logger:warning_msg(S, As);
        error -> error_logger:error_msg(S, As);
        _ -> ok
    end.

delete_global_name(Name) ->
    case ets:lookup(global_names, Name) of
        [{Name, Pid, _}] ->
            delete_global_name(Name, Pid);
        [] ->
            ok
    end.

delete_global_name(Name, Pid) ->
    ?trace({delete_global_name,{item,Name},{pid,Pid}}),
    true = ets:delete(global_names, Name),
    true = ets:delete_object(global_pid_names, {Pid, Name}),
    case ets:lookup(global_names_ext, Name) of
	[{Name, Pid, RegNode}] ->
            true = ets:delete(global_names_ext, Name),
            ?trace({delete_global_name, {name,Name,{pid,Pid},{RegNode,Pid}}}),
            true = ets:delete_object(global_node_pids, {{name,RegNode}, Pid}),
	    dounlink_ext(Pid, RegNode);
	[] ->
            ?trace({delete_global_name,{name,Name,{pid,Pid},{node(Pid),Pid}}}),
            true = ets:delete_object(global_node_pids,{{name,node(Pid)},Pid}),
            dounlink(Pid)
    end.

%%-----------------------------------------------------------------
%% The locker is a satellite process to global_name_server. When a
%% nodeup is received from a new node the global_name_server sends a
%% message to the locker. The locker tries to set a lock in our
%% partition, i.e. on all nodes known to us. When the lock is set, it
%% tells global_name_server about it, and keeps the lock set.
%% global_name_server sends a cancel message to the locker when the
%% partitions are connected.

%% There are two versions of the protocol between lockers on two nodes:
%% Version 1: used by unpatched R7.
%% Version 2: the messages exchanged between the lockers include the known 
%%            nodes (see OTP-3576).
%%-----------------------------------------------------------------

-define(locker_vsn, 2).

-record(multi, 
        {local = [],         % Requests from nodes on the local host.
         remote = [],        % Other requests.
         known = [],         % Copy of global_name_server's known nodes. It's
                             % faster to keep a copy of known than to asking 
                             % for it when needed.
         the_boss,           % max([node() | 'known'])
         just_synced = false % true if node() synced just a moment ago
        }).

-record(him, {node, locker, vsn, my_tag}).

start_the_locker() ->
    spawn_link(fun() -> init_the_locker() end).

init_the_locker() ->
    process_flag(trap_exit, true),    % needed?
    S1 = update_locker_known({add, get_known()}, #multi{}),
    loop_the_locker(S1),
    erlang:error(locker_exited).

loop_the_locker(S) ->
    ?trace({loop_the_locker,S}),
    receive 
        Message when element(1, Message) =/= nodeup ->
            the_locker_message(Message, S)
    after 0 ->
            Timeout = 
                case {S#multi.local, S#multi.remote} of
                    {[],[]} ->
                        infinity;
                    _ ->
                        %% It is important that the timeout is greater
                        %% than zero, or the chance that some other node
                        %% in the partition sets the lock once this node
                        %% has failed after setting the lock is very slim.
                        if
                            S#multi.just_synced ->
                                0; % no reason to wait after success
                            S#multi.known =:= [] ->
                                200; % just to get started 
                            true ->
                                lists:min([1000 + 100*length(S#multi.known), 
                                           3000])
                        end
                end,
            S1 = S#multi{just_synced = false},
            receive 
                Message when element(1, Message) =/= nodeup ->
                    the_locker_message(Message, S1)
            after Timeout ->
                    case is_lock_set(?GLOBAL_RID) of
                        true -> 
                            loop_the_locker(S1);
                        false -> 
                            select_node(S1)
                    end
            end
    end.

the_locker_message({his_the_locker, HisTheLocker, HisKnown0, _MyKnown}, S) ->
    ?trace({his_the_locker, HisTheLocker, {node,node(HisTheLocker)}}),
    HisVsn = 
        case HisKnown0 of
            {Vsn0, _} when Vsn0 > 4 ->
                Vsn0;
            _ when is_list(HisKnown0) ->
                4
        end,
    receive
        {nodeup, Node, MyTag} when node(HisTheLocker) =:= Node ->
            ?trace({the_locker_nodeup, {node,Node},{mytag,MyTag}}),
            Him = #him{node = node(HisTheLocker), my_tag = MyTag,
                       locker = HisTheLocker, vsn = HisVsn},
            loop_the_locker(add_node(Him, S));
        {cancel, Node, _Tag} when node(HisTheLocker) =:= Node ->
            loop_the_locker(S)
    after 60000 ->
            ?trace({nodeupnevercame, node(HisTheLocker)}),
            error_logger:error_msg("global: nodeup never came ~w ~w\n",
                                   [node(), node(HisTheLocker)]),
            loop_the_locker(S#multi{just_synced = false})
    end;
the_locker_message({cancel, _Node, undefined}, S) ->
    ?trace({cancel_the_locker, undefined, {node,_Node}}),
    %% If we actually cancel something when a cancel message with the
    %% tag 'undefined' arrives, we may be acting on an old nodedown,
    %% to cancel a new nodeup, so we can't do that.
    loop_the_locker(S);
the_locker_message({cancel, Node, Tag}, S) ->
    ?trace({the_locker, cancel, {multi,S}, {tag,Tag},{node,Node}}),
    receive
        {nodeup, Node, Tag} ->
            ?trace({cancelnodeup2, {node,Node},{tag,Tag}}),
            ok
    after 0 ->
            ok
    end,
    loop_the_locker(remove_node(Node, S));
the_locker_message({lock_set, _Pid, false, _}, S) ->
    ?trace({the_locker, spurious, {node,node(_Pid)}}),
    loop_the_locker(S);
the_locker_message({lock_set, Pid, true, _HisKnown}, S) ->
    Node = node(Pid),
    ?trace({the_locker, self(), spontaneous, {node,Node}}),
    case find_node_tag(Node, S) of
        {true, MyTag, HisVsn} ->
            LockId = locker_lock_id(Pid, HisVsn),
            {IsLockSet, S1} = lock_nodes_safely(LockId, [], S),
            Pid ! {lock_set, self(), IsLockSet, S1#multi.known},
            Known2 = [node() | S1#multi.known],
            ?trace({the_locker, spontaneous, {known2, Known2},
                    {node,Node}, {is_lock_set,IsLockSet}}),
            case IsLockSet of
                true ->
                    gen_server:cast(global_name_server, 
                                    {lock_is_set, Node, MyTag}),
                    ?trace({lock_sync_done, {pid,Pid}, 
                            {node,node(Pid)}, {me,self()}}),
                    %% Wait for global to tell us to remove lock.
                    %% Should the other locker's node die,
                    %% global_name_server will receive nodedown, and
                    %% then send {cancel, Node, Tag}.
                    receive
                        {cancel, Node, _Tag} ->
                            ?trace({cancel_the_lock,{node,Node}}),
                            del_lock(LockId, Known2)
                    end,
                    S2 = S1#multi{just_synced = true},
                    loop_the_locker(remove_node(Node, S2));
                false ->
                    loop_the_locker(S1#multi{just_synced = false})
            end;
        false ->
            ?trace({the_locker, not_there, {node,Node}}),
            Pid ! {lock_set, self(), false, S#multi.known},
            loop_the_locker(S)
    end;
the_locker_message({add_to_known, Nodes}, S) ->
    S1 = update_locker_known({add, Nodes}, S),
    loop_the_locker(S1);
the_locker_message({remove_from_known, Node}, S) ->
    S1 = update_locker_known({remove, Node}, S),
    loop_the_locker(S1);
the_locker_message(_Other, S) ->
    ?trace({the_locker, {other_msg, _Other}}),
    loop_the_locker(S).

%% Requests from nodes on the local host are chosen before requests
%% from other nodes. This should be a safe optimization.
select_node(S) ->
    UseRemote = S#multi.local =:= [],
    Others1 = if UseRemote -> S#multi.remote; true -> S#multi.local end,
    Others2 = exclude_known(Others1, S#multi.known),
    S1 = if 
             UseRemote -> S#multi{remote = Others2}; 
             true -> S#multi{local = Others2} 
         end,
    if 
        Others2 =:= [] ->
            loop_the_locker(S1);
        true -> 
            Him = random_element(Others2),
            #him{locker = HisTheLocker, vsn = HisVsn,
                 node = Node, my_tag = MyTag} = Him,
            HisNode = if
                           HisVsn < 5 -> [];
                           true -> [Node] % prevents deadlock; optimization
                       end,
            Us = [node() | HisNode],
            LockId = locker_lock_id(HisTheLocker, HisVsn),
            ?trace({select_node, self(), {us, Us}}),
            {IsLockSet, S2} = lock_nodes_safely(LockId, HisNode, S1),
            case IsLockSet of
                true -> 
                    Known1 = Us ++ S2#multi.known,
                    ?trace({sending_lock_set, self(), {his,HisTheLocker}}),
                    HisTheLocker ! {lock_set, self(), true, S2#multi.known},
                    %% OTP-4902
                    S3 = lock_set_loop(S2, Him, MyTag, Known1, LockId),
                    loop_the_locker(S3);
                false ->
                    loop_the_locker(S2)
            end
    end.

%% Version 5: Both sides use the same requester id. Thereby the nodes
%% common to both sides are locked by both locker processes. This
%% means that the lock is still there when the 'new_nodes' message is
%% received even if the other side has deleted the lock. Before R11 it
%% could be that the lock had been deleted (by the other side) at the
%% time 'new_nodes' was sent.
locker_lock_id(Pid, 4) ->
    %% if node() > Node then Node locks common nodes with {global, Pid}
    {?GLOBAL_RID, Pid};
locker_lock_id(Pid, Vsn) when Vsn > 4 ->
    {?GLOBAL_RID, lists:sort([self(), Pid])}.

lock_nodes_safely(LockId, Extra, S0) ->
    %% Locking the boss first is an optimization.
    First = lists:usort([node(), S0#multi.the_boss]) -- [nonode@nohost],
    case set_lock(LockId, First, 0) of
        true ->
            S = update_locker_known(S0),
            %% The boss may have changed, but don't bother.
            case set_lock(LockId, Extra, 0) of
                true ->
                    case set_lock(LockId, S#multi.known, 0) of
                        true ->
                            {true, S};
                        false ->
                            %% Since the boss is locked we should have
                            %% gotten the lock, at least if there are
                            %% no version 4 nodes in the partition or
                            %% someone else is locking 'global'. 
                            %% Calling set_lock with Retries > 0 does
                            %% not seem to speed things up.
                            del_lock(LockId, Extra ++ First),
                            {false, S}
                    end;
                false ->
                    del_lock(LockId, First),
                    {false, S}
            end;
        false ->
            {false, S0}
    end.

update_locker_known(S) ->
    receive
        {add_to_known, Nodes} ->
            S1 = update_locker_known({add, Nodes}, S),
            update_locker_known(S1);
        {remove_from_known, Node} ->
            S1 = update_locker_known({remove, Node}, S),
            update_locker_known(S1)
    after 0 ->
            S
    end.

update_locker_known(Upd, S) ->
    Known = case Upd of
                {add, Nodes} -> Nodes ++ S#multi.known;
                {remove, Node} -> lists:delete(Node, S#multi.known)
            end,
    TheBoss = lists:max([node() | Known]), 
    S#multi{known = Known, the_boss = TheBoss}.

random_element(L) ->
    {A,B,C} = now(),
    E = (A+B+C) rem length(L),
    lists:nth(E+1, L).

exclude_known(Others, Known) ->
    [N || N <- Others, not lists:member(N#him.node, Known)].

lock_set_loop(S, Him, MyTag, Known1, LockId) ->
    Node = Him#him.node,
    Timeout = if
                  Him#him.vsn < 5 -> 5000;
                  true -> infinity
              end,
    receive
	{lock_set, P, true, _} when node(P) =:= Node ->
	    gen_server:cast(global_name_server, {lock_is_set, Node, MyTag}),
	    ?trace({lock_sync_done, {p,P, node(P)}, {me,self()}}),

	    %% Wait for global to tell us to remove lock. Should the
            %% other locker's node die, global_name_server will
            %% receive nodedown, and then send {cancel, Node, Tag}.
	    receive
		{cancel, Node, _} ->
                    ?trace({lock_set_loop, {known1,Known1}}),
		    del_lock(LockId, Known1)
	    end,
            S#multi{just_synced = true,
                    local = lists:delete(Him, S#multi.local),
                    remote = lists:delete(Him, S#multi.remote)};
	{lock_set, P, false, _} when node(P) =:= Node ->
            ?trace({not_both_set, {node,Node},{p, P},{known1,Known1}}),
	    del_lock(LockId, Known1),
	    S;
	{cancel, Node, _} ->
	    ?trace({the_locker, cancel2, {node,Node}}),
	    del_lock(LockId, Known1),
            remove_node(Node, S);
	{'EXIT', _, _} ->
	    ?trace({the_locker, exit, {node,Node}}),
	    del_lock(LockId, Known1),
	    S
    after
	%% OTP-4902
	%% A cyclic deadlock could occur in rare cases where three or
	%% more nodes waited for a reply from each other.
	%% Therefore, reject lock_set attempts in this state from
	%% nodes < this node (its enough if at least one node in
	%% the cycle rejects and thus breaks the deadlock)
        %%
        %% OTP-5770. Version 5 of the protocol. Deadlock can no longer
        %% occur due to the fact that if a partition is locked, one
        %% node in the other partition is also locked with the same
        %% lock-id, which makes it impossible for any node in the
        %% other partition to lock its partition unless it negotiates
        %% with the first partition.
        Timeout -> 
	    reject_lock_set(),
	    lock_set_loop(S, Him, MyTag, Known1, LockId)
    end.

reject_lock_set() ->
    receive
	{lock_set, P, true, _} when node(P) < node() ->
	    P ! {lock_set, self(), false, []},
	    reject_lock_set()
    after
	0 ->
	    true
    end.

find_node_tag(Node, S) ->
    case find_node_tag2(Node, S#multi.local) of
        false -> 
            find_node_tag2(Node, S#multi.remote);
        Reply ->
            Reply
    end.

find_node_tag2(_Node, []) ->
    false;
find_node_tag2(Node, [#him{node = Node, my_tag = MyTag, vsn = HisVsn} | _]) ->
    {true, MyTag, HisVsn};
find_node_tag2(Node, [_E | Rest]) ->
    find_node_tag2(Node, Rest).

remove_node(Node, S) ->
    S#multi{local = remove_node2(Node, S#multi.local),
            remote = remove_node2(Node, S#multi.remote)}.

remove_node2(_Node, []) ->
    [];
remove_node2(Node, [#him{node = Node} | Rest]) ->
    Rest;
remove_node2(Node, [E | Rest]) ->
    [E | remove_node2(Node, Rest)].

add_node(Him, S) ->
    case is_node_local(Him#him.node) of
        true ->
            S#multi{local = [Him | S#multi.local]};
        false ->
            S#multi{remote = [Him | S#multi.remote]}
    end.

is_node_local(Node) ->
    {ok, Host} = inet:gethostname(),
    case catch split_node(atom_to_list(Node), $@, []) of
	[_, Host] -> 
            true;
	_ ->
            false
    end.

split_node([Chr|T], Chr, Ack) -> [lists:reverse(Ack)|split_node(T, Chr, [])];
split_node([H|T], Chr, Ack)   -> split_node(T, Chr, [H|Ack]);
split_node([], _, Ack)        -> [lists:reverse(Ack)].

cancel_locker(Node, S, Tag) ->
    S#state.the_locker ! {cancel, Node, Tag},
    Resolvers = S#state.resolvers,
    ?trace({cancel_locker, {node,Node},{tag,Tag},
            {sync_tag_my, get({sync_tag_my, Node})},{resolvers,Resolvers}}),
    case lists:keysearch(Node, 1, Resolvers) of
	{value, {_, Tag, Resolver}} ->
            ?trace({{resolver, Resolver}}),
            exit(Resolver, kill),
	    S#state{resolvers = lists:keydelete(Node, 1, Resolvers)};
	_ ->
	    S
    end.

reset_node_state(Node) ->
    ?trace({{node,Node}, reset_node_state,get()}),
    erase({wait_lock, Node}),
    erase({save_ops, Node}),
    erase({pre_connect, Node}),
    erase({prot_vsn, Node}),
    erase({sync_tag_my, Node}),
    erase({sync_tag_his, Node}).

%% Some node sent us his names. When a name clash is found, the resolve
%% function is called from the smaller node => all resolve funcs are called
%% from the same partition.
exchange_names([{Name, Pid, Method} | Tail], Node, Ops, Res) ->
    case ets:lookup(global_names, Name) of
	[{Name, Pid, _}] ->
	    exchange_names(Tail, Node, Ops, Res);
	[{Name, Pid2, Method2}] when node() < Node ->
	    %% Name clash!  Add the result of resolving to Res(olved).
	    %% We know that node(Pid) =/= node(), so we don't
	    %% need to link/unlink to Pid.
	    Node2 = node(Pid2), %% Node2 is connected to node().
	    case rpc:call(Node2, ?MODULE, resolve_it,
			  [Method2, Name, Pid, Pid2]) of
		Pid ->
		    Op = {insert, {Name, Pid, Method}},
		    exchange_names(Tail, Node, [Op | Ops], Res);
		Pid2 ->
		    Op = {insert, {Name, Pid2, Method2}},
		    exchange_names(Tail, Node, Ops, [Op | Res]);
		none ->
		    Op = {delete, Name},
		    exchange_names(Tail, Node, [Op | Ops], [Op | Res]);
		{badrpc, Badrpc} ->
		    error_logger:info_msg("global: badrpc ~w received when "
					  "conflicting name ~w was found\n",
					  [Badrpc, Name]),
		    Op = {insert, {Name, Pid, Method}},
		    exchange_names(Tail, Node, [Op | Ops], Res);
		Else ->
		    error_logger:info_msg("global: Resolve method ~w for "
					  "conflicting name ~w returned ~w\n",
					  [Method, Name, Else]),
		    Op = {delete, Name},
		    exchange_names(Tail, Node, [Op | Ops], [Op | Res])
	    end;
	[{Name, _Pid2, _}] ->
	    %% The other node will solve the conflict.
	    exchange_names(Tail, Node, Ops, Res);
	_ ->
	    %% Entirely new name.
	    exchange_names(Tail, Node,
			   [{insert, {Name, Pid, Method}} | Ops], Res)
    end;
exchange_names([], _, Ops, Res) ->
    ?trace({exchange_names_finish,{ops,Ops},{res,Res}}),
    {Ops, Res}.

resolve_it(Method, Name, Pid1, Pid2) ->
    catch Method(Name, Pid1, Pid2).

minmax(P1,P2) ->
    if node(P1) < node(P2) -> {P1, P2}; true -> {P2, P1} end.

random_exit_name(Name, Pid, Pid2) ->
    {Min, Max} = minmax(Pid, Pid2),
    error_logger:info_msg("global: Name conflict terminating ~w\n",
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

%% Only link to pids on our own node
dolink(Pid) when node(Pid) =:= node() ->
    link(Pid);
dolink(_) -> ok.

%% Only link to pids on our own node
dolink_ext(Pid, RegNode) when RegNode =:= node() -> 
    link(Pid);
dolink_ext(_, _) -> 
    ok.

dounlink(Pid) when node(Pid) =:= node() ->
    unlink_pid(Pid);
dounlink(_Pid) ->
    ok.

dounlink_ext(Pid, RegNode) when RegNode =:= node() ->
    unlink_pid(Pid);
dounlink_ext(_Pid, _RegNode) ->
    ok.

unlink_pid(Pid) ->
    case ets:member(global_pid_names, Pid) of
	false ->
            case ets:member(global_pid_ids, Pid) of
                false -> 
		    unlink(Pid);
		true -> 
                    ok
	    end;
	true -> 
            ok
    end.

%% check_exit/3 removes the Pid from affected tables.
%% This function needs to abcast the thingie since only the local
%% server is linked to the registered process (or the owner of the
%% lock). All the other servers rely on the nodedown mechanism.
check_exit(Deleter, Pid, KnownNodes) ->
    del_names(Deleter, Pid),
    del_locks(pid_locks(Pid), Pid, KnownNodes).

del_names(Deleter, Pid) ->
    lists:foreach(fun({_Pid,Name}) ->
                          %% The local name is deleted immediately,
                          %% before the lock is taken. It is not known
                          %% exactly why, but it may have something to
                          %% do with supervisors...
                          ?trace({del_names, {pid,Pid}, {name,Name}}),
                          delete_global_name(Name, Pid),
                          Deleter ! {delete_name, self(), Name, Pid}
                  end, ets:lookup(global_pid_names, Pid)).

pid_locks(Pid) ->
    L = lists:flatmap(fun({_Pid, ResourceId}) ->
                              ets:lookup(global_locks, ResourceId)
                      end, ets:lookup(global_pid_ids, Pid)),
    [Lock || Lock = {_Id, _Req, Pids} <- L, lists:member(Pid, Pids)].

del_locks([{ResourceId, LockReqId, Pids} | Tail], Pid, KnownNodes) ->
    remove_lock(ResourceId, LockReqId, Pid, Pids),
    gen_server:abcast(KnownNodes, global_name_server,
                      {async_del_lock, ResourceId, Pid}),
    del_locks(Tail, Pid, KnownNodes);
del_locks([], _Pid, _KnownNodes) -> done.

%% Unregister all Name/Pid pairs such that node(Pid) =:= Node
%% and delete all locks where node(Pid) =:= Node
do_node_down(Node) ->
    ?trace({do_node_down, Node}),
    do_node_down_names(Node),
    do_node_down_locks(Node).

do_node_down_names(Node) ->
    [[] || {_, Pid} <- ets:lookup(global_node_pids, {name,Node}),
           {_, Name} <- ets:lookup(global_pid_names, Pid),
           ok =/= delete_global_name(Name, Pid)].

do_node_down_locks(Node) ->
    NodePids = ets:lookup(global_node_pids, {lock, Node}),
    ?trace({do_node_down_locks, {node_pids, NodePids}}),
    Pids = lists:usort([Pid || {_Node, Pid} <- NodePids]),
    lists:foreach(fun async_del_lock/1, Pids).

async_del_lock(Pid) ->
    lists:foreach(fun({ResourceId, LockRequesterId, Pids}) -> 
                          remove_lock(ResourceId, LockRequesterId, Pid, Pids)
                  end, pid_locks(Pid)).

get_names() ->
    ets:tab2list(global_names).

get_names_ext() ->
    ets:tab2list(global_names_ext).

get_known() ->
    gen_server:call(global_name_server, get_known, infinity).

random_sleep(Times) ->
    case (Times rem 10) of
	0 -> erase(random_seed);
	_ -> ok
    end,
    case get(random_seed) of
	undefined ->
	    {A1, A2, A3} = now(),
	    random:seed(A1, A2, A3 + erlang:phash(node(), 100000));
	_ -> ok
    end,
    %% First time 1/4 seconds, then doubling each time up to 8 seconds max.
    Tmax = if Times > 5 -> 8000;
	      true -> ((1 bsl Times) * 1000) div 8
	   end,
    T = random:uniform(Tmax),
    ?trace({random_sleep, {me,self()}, {times,Times}, {t,T}, {tmax,Tmax}}),
    receive after T -> ok end.

dec(infinity) -> infinity;
dec(N) -> N - 1.

send_again(Msg) ->
    Me = self(),
    spawn_link(fun() -> timer(Me, Msg) end).

timer(Pid, Msg) ->
    random_sleep(5),
    Pid ! Msg.

change_our_node_name(NewNode, S) ->
    S#state{node_name = NewNode}.

%%-----------------------------------------------------------------
%% Each sync process corresponds to one call to sync. Each such
%% process asks the global_name_server on all Nodes if it is in sync
%% with Nodes. If not, that (other) node spawns a syncer process that
%% waits for global to get in sync with all Nodes. When it is in
%% sync, the syncer process tells the original sync process about it.
%%-----------------------------------------------------------------
start_sync(Nodes, From) ->
    spawn_link(fun() -> sync_init(Nodes, From) end).

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

%%%=======================================================================
%%% Get the current global_groups definition
%%%=======================================================================
check_sync_nodes() ->
    case get_own_nodes() of
	{ok, all} ->
	    nodes();
	{ok, NodesNG} ->
	    %% global_groups parameter is defined, we are not allowed to sync
	    %% with nodes not in our own global group.
	    intersection(nodes(), NodesNG);
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
	    OwnNodeGroup = intersection(nodes(), NodesNG),
	    IllegalSyncNodes = (SyncNodes -- [node() | OwnNodeGroup]),
	    case IllegalSyncNodes of
		[] -> SyncNodes;
		_ -> {error, {"Trying to sync nodes not defined in "
                              "the own global group", IllegalSyncNodes}}
	    end;
	{error, Error} ->
	    {error, Error}
    end.

get_own_nodes() ->
    case global_group:get_own_nodes_with_errors() of
        {error, Error} ->
            {error, {"global_groups definition error", Error}};
        OkTup ->
            OkTup
    end.

%%-----------------------------------------------------------------
%% The deleter process is a satellite process to global_name_server
%% that does background batch deleting of names when a process
%% that had globally registered names dies. It is started by and 
%% linked to global_name_server.
%%-----------------------------------------------------------------

start_the_deleter(Global) ->
    spawn_link(
      fun () -> 
	      loop_the_deleter(Global)
      end).

loop_the_deleter(Global) ->
    Deletions = collect_deletions(Global, []),
    ?trace({loop_the_deleter, self(), {deletions,Deletions}, 
            {names,get_names()}}),
    %% trans_all_known is called rather than trans/3 with nodes() as
    %% third argument. The reason is that known gets updated by
    %% new_nodes when the lock is still set. nodes() on the other hand
    %% could be updated later (if in_sync is received after the lock
    %% is gone). It is not likely that in_sync would be received after
    %% the lock has been taken here, but using trans_all_known makes it
    %% even less likely.
    trans_all_known(
      fun(Known) ->
              lists:map(
                fun ({Name,Pid}) ->
                        gen_server:abcast(Known, global_name_server,
                                          {async_del_name, Name, Pid})
                end, Deletions)
      end),
    loop_the_deleter(Global).

collect_deletions(Global, Deletions) ->
    receive
	{delete_name, Global, Name, Pid} ->
	    collect_deletions(Global, [{Name,Pid} | Deletions]);
	Other ->
	    error_logger:error_msg("The global_name_server deleter process "
                                   "received an unexpected message:\n~p\n", 
                                   [Other]),
	    collect_deletions(Global, Deletions)
    after case Deletions of
	      [] -> infinity;
	      _  -> 0
	  end ->
	    lists:reverse(Deletions)
    end.

%%% Utilities

intersection(_, []) -> 
    [];
intersection(L1, L2) ->
    L1 -- (L1 -- L2).
