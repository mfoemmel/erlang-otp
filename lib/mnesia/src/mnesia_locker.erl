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
-module(mnesia_locker).

-export([
	 get_held_locks/0,
	 get_lock_queue/0,
	 global_lock/5,
	 ixrlock/5,
	 init/1,
	 mnesia_down/2,
	 release_tid/1,
	 async_release_tid/2,
	 send_release_tid/2,
	 receive_release_tid_acc/2,
	 rlock/3,
	 rlock_table/3,
	 rwlock/3,
	 start/0,
	 sticky_wlock/3,
	 wlock/3,
	 wlock_no_exist/4,
	 wlock_table/3
	]).

%% sys callback functions
-export([system_continue/3,
	 system_terminate/4,
	 system_code_change/4
	]).

-include("mnesia.hrl").
-import(mnesia_lib, [dbg_out/2, error/2, verbose/2]).
-define(ALL, '______WHOLETABLE_____').
-define(STICK, '______STICK_____').
-define(GLOBAL, '______GLOBAL_____').

-record(state, {supervisor}).

start() ->
    mnesia_monitor:start_proc(?MODULE, ?MODULE, init, [self()]).

init(Parent) ->
    register(?MODULE, self()),
    process_flag(trap_exit, true),
    proc_lib:init_ack(Parent, {ok, self()}),
    loop(#state{supervisor = Parent}).


val(Var) ->
    case ?catch_val(Var) of
	{'EXIT', _ReASoN_} -> mnesia_lib:other_val(Var, _ReASoN_); 
	_VaLuE_ -> _VaLuE_ 
    end.

reply(From, R, State) ->
    From ! {?MODULE, node(), R},
    loop(State).

l_request(Node, X, Store) ->
    {?MODULE, Node} ! {self(), X},
    l_req_rec(Node, Store).

l_req_rec(Node, Store) ->
    ?ets_insert(Store, {nodes, Node}),
    receive 
	{?MODULE, Node, {switch, Node2, Req}} ->
	    ?ets_insert(Store, {nodes, Node2}),
	    {?MODULE, Node2} ! Req,
	    {switch, Node2, Req};
	{?MODULE, Node, Reply} -> 
	    Reply;
	{mnesia_down, Node} -> 
	    {not_granted, {node_not_running, Node}}
    end.

release_tid(Tid) ->
    ?MODULE ! {release_tid, Tid}.

async_release_tid(Nodes, Tid) ->
    rpc:abcast(Nodes, ?MODULE, {release_tid, Tid}).

send_release_tid(Nodes, Tid) ->
    rpc:abcast(Nodes, ?MODULE, {self(), {sync_release_tid, Tid}}).

receive_release_tid_acc([Node | Nodes], Tid) ->
    receive 
	{?MODULE, Node, {tid_released, Tid}} -> 
	    receive_release_tid_acc(Nodes, Tid);
	{mnesia_down, Node} -> 
	    receive_release_tid_acc(Nodes, Tid)
    end;
receive_release_tid_acc([], Tid) ->
    ok.

%% mnesia_held_locks: contain    {Oid, Op, Tid} entries  (bag)
%% mnesia_tid_locks: contain {Tid, Oid, Op} entries  (bag) 
%% mnesia_sticky_locks: contain   {Oid, Node} entries and {Tab, Node} entries (set)
%% mnesia_lock_queue: contain    {Oid, Op, ReplyTo, Tid} entries (bag)
%% mnesia_lock_counter:        {{write, Tab}, Number} &&
%%                   {{read, Tab}, Number} entries  (set)

loop(State) ->
    receive
	{From, {write, Tid, Oid}} ->
	    try_lock(Tid, write, From, Oid, State);

	%% If Key == ?ALL it's a request to lock the entire table
	%%

	{From, {read, Tid, Oid}} ->
	    try_sticky_lock(Tid, read, From, Oid, State);

	%% Really do a  read, but get hold of a write lock
	%% used by mnesia:wread(Oid).
	
	{From, {read_write, Tid, Oid}} ->
	    try_sticky_lock(Tid, read_write, From, Oid, State);
	
	%% Tid has somehow terminated, clear up everything
	%% and pass locks on to queued processes.
	%% This is the purpose of the mnesia_tid_locks table
	
	{release_tid, Tid} ->
	    do_release_tid(Tid),
	    loop(State);

	%% stick lock, first tries this to the where_to_read Node
	{From, {test_set_sticky, Tid, Oid}} ->
	    Tab = element(1, Oid),
	    case catch ?ets_lookup_element(mnesia_sticky_locks, Oid, 2) of
		{'EXIT', _} -> 
		    reply(From, not_stuck, State);
		Node when Node == node() ->
		    %% Lock is stuck here, see now if we can just set 
		    %% a regular write lock
		    try_lock(Tid, write, From, Oid, State);
		Node ->
		   reply(From, {stuck_elsewhere, Node}, State) 
	    end;

	%% If test_set_sticky fails, we send this to all nodes
	%% after aquiring a real write lock on Oid

	{stick, Oid, N} ->
	    Tab = element(1, Oid),
	    ?ets_insert(mnesia_sticky_locks, {Oid, N}),
	    ?ets_insert(mnesia_sticky_locks, {Tab, N}),
	    loop(State);

	%% The caller which sends this message, must have first 
	%% aquired a write lock on the entire table
	{unstick, Tab} ->
	    ?ets_delete(mnesia_sticky_locks, Tab),
	    ?ets_match_delete(mnesia_sticky_locks, {{Tab, '_'}, '_'}),
	    loop(State);

	{From, {ix_read, Tid, Tab, IxKey, Pos}} ->
	    case catch mnesia_index:get_index_table(Tab, Pos) of
		{'EXIT', _} ->
		    reply(From, {not_granted, {no_exists, Tab, {index, [Pos]}}}, State);
		Index ->
		    Rk = mnesia_lib:elems(2,mnesia_index:db_get(Index, IxKey)),
		    %% list of real keys
		    case catch ?ets_lookup_element(mnesia_sticky_locks, Tab, 2) of
			{'EXIT',_} ->
			    set_read_lock_on_all_keys(Tid, From,Tab,Rk,Rk, [],State);
			N when N == node() ->
			    set_read_lock_on_all_keys(Tid, From,Tab,Rk,Rk, [], State);
			N ->
			    Req = {From, {ix_read, Tid, Tab, IxKey, Pos}},
			    From ! {?MODULE, node(), {switch, N, Req}},
			    loop(State)
		    end
	    end;

	{From, {sync_release_tid, Tid}} ->
	    do_release_tid(Tid),
	    reply(From, {tid_released, Tid}, State);
	
	{release_remote_non_pending, Node, Pending} ->
	    release_remote_non_pending(Node, Pending),
	    mnesia_monitor:mnesia_down(?MODULE, Node),
	    loop(State);

	{'EXIT', Pid, _} when Pid == State#state.supervisor ->
	    do_stop();

	{system, From, Msg} ->
	    dbg_out("~p got {system, ~p, ~p}~n", [?MODULE, From, Msg]),
	    Parent = State#state.supervisor,
	    sys:handle_system_msg(Msg, From, Parent, ?MODULE, [], State);
	    
	Msg ->
	    error("~p got unexpected message: ~p~n", [?MODULE, Msg]),
	    loop(State)
    end.

set_lock(Tid, Oid, Op) ->
    ?ets_insert(mnesia_held_locks, {Oid, Op, Tid}),
    ?ets_insert(mnesia_tid_locks, {Tid, Oid, Op}),
    Tab = element(1, Oid),
    incr_counter({Op, Tab}).

incr_counter(Counter) ->
    case catch ?ets_update_counter(mnesia_lock_counter, Counter, 1) of
	{'EXIT', _} ->
	    ?ets_insert(mnesia_lock_counter, {Counter, 1});
	_ -> 
	    ok
    end.

decr_counter(Counter) ->
    catch ?ets_update_counter(mnesia_lock_counter, Counter, -1).

get_counter(Counter) ->
    case catch ?ets_lookup_element(mnesia_lock_counter, Counter, 2) of
	{'EXIT', _} -> 0;
	Val -> Val
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Acquire locks

try_sticky_lock(Tid, Op, Pid, Oid, State) ->
    StickyKey = sticky_key(Oid),
    case catch ?ets_lookup_element(mnesia_sticky_locks, StickyKey, 2) of
	{'EXIT', _} ->
	    try_lock(Tid, Op, Pid, Oid, State);
	N when N == node() ->
	    try_lock(Tid, Op, Pid, Oid, State);
	N ->
	    Req = {Pid, {Op, Tid, Oid}},
	    Pid ! {?MODULE, node(), {switch, N, Req}},
	    loop(State)
   end.

sticky_key({Tab, Key}) when Key /= ?ALL ->
    {Tab, Key};
sticky_key({Tab, Key}) ->
    Tab.

try_lock(Tid, read_write, Pid, Oid, State) ->
    try_lock(Tid, read_write, read, write, Pid, Oid, State);
try_lock(Tid, Op, Pid, Oid, State) ->
    try_lock(Tid, Op, Op, Op, Pid, Oid, State).

try_lock(Tid, Op, SimpleOp, Lock, Pid, Oid, State) ->
    case catch can_lock(Tid, Lock, Oid, {no, bad_luck}) of
	yes ->
	    Reply = grant_lock(Tid, Pid, SimpleOp, Lock, Oid),
	    reply(Pid, Reply, State);
	{no, Lucky} ->
	    C = #cyclic{op = SimpleOp, lock = Lock, oid = Oid, lucky = Lucky},
	    if
		Tid#tid.pid == Lucky#tid.pid ->
		    verbose("Detected spurious lock conflict ~w: ~w -> ~w~n",
			    [Tid#tid.counter == Lucky#tid.counter, Tid, C]);
		true ->
		    ignore
	    end,
	    reply(Pid, {not_granted, C}, State);
	{queue, Lucky} ->
	    %% Append to queue: Nice place for trace output
	    ?ets_insert(mnesia_lock_queue, {Oid, Op, Pid, Tid}),
	    loop(State)
    end.

grant_lock(Tid, ClientPid, read, Lock, {Tab, Key})
  when Key /= ?ALL, Tab /= ?GLOBAL ->
    case node(Tid#tid.pid) == node() of
	true ->
	    set_lock(Tid, {Tab, Key}, Lock),
	    {granted, lookup_in_client};
	false ->
	    case catch mnesia_lib:db_get(Tab, Key) of %% lookup as well
		{'EXIT', _Reason} ->
		    %% Table has been deleted from this node,
		    %% restart the transaction.
		    C = #cyclic{op = read, lock = Lock, oid = {Tab, Key},
				lucky = nowhere},
		    {not_granted, C};
		Val -> 
		    set_lock(Tid, {Tab, Key}, Lock),
		    {granted, Val}
	    end
    end;
grant_lock(Tid, _ClientPid, read, Lock, Oid) ->
    set_lock(Tid, Oid, Lock),
    {granted, ok};
grant_lock(Tid, _ClientPid, write, Lock, Oid) ->
    set_lock(Tid, Oid, Lock),
    granted.

%% Impose an ordering on all transactions 
%% favour old transactions
%% newer transactions may never wait on older ones

can_lock(Tid, read, {Tab, Key}, AlreadyQ) when Key /= ?ALL ->
    %% The key is bound, no need for the other BIF
    Oid = {Tab, Key}, 
    ObjLocks = ?ets_match_object(mnesia_held_locks, {Oid, write, '_'}),
    TabLocks = ?ets_match_object(mnesia_held_locks, {{Tab, ?ALL}, write, '_'}),
    check_lock(Tid, Oid, ObjLocks, TabLocks, yes, AlreadyQ);


can_lock(Tid, read, Oid, AlreadyQ) -> % Whole tab
    Tab = element(1, Oid),
    ObjLocks =
	case get_counter({write, Tab}) of
	    0 ->
		[];
	    _ ->
		?ets_match_object(mnesia_held_locks, {{Tab, '_'}, write, '_'})
	end,
    check_lock(Tid, Oid, ObjLocks, [], yes, AlreadyQ);

can_lock(Tid, write, {Tab, Key}, AlreadyQ) when Key /= ?ALL, Tab /= ?GLOBAL ->
    Oid = {Tab, Key},
    ObjLocks = ?ets_lookup(mnesia_held_locks, Oid),
    TabLocks = ?ets_lookup(mnesia_held_locks, {Tab, ?ALL}),
    check_lock(Tid, Oid, ObjLocks, TabLocks, yes, AlreadyQ);

can_lock(Tid, write, Oid, AlreadyQ) -> % Whole tab
    Tab = element(1, Oid),
    ObjLocks = ?ets_match_object(mnesia_held_locks, {{Tab, '_'}, '_', '_'}),
    check_lock(Tid, Oid, ObjLocks, [], yes, AlreadyQ).

%% Check held locks for conflicting locks
check_lock(Tid, Oid, [Lock | Locks], TabLocks, X, AlreadyQ) ->
    case element(3, Lock) of
	Tid ->
	    check_lock(Tid, Oid, Locks, TabLocks, X, AlreadyQ);
	WaitForTid when WaitForTid > Tid -> % Important order
	    check_lock(Tid, Oid, Locks, TabLocks, {queue, WaitForTid}, AlreadyQ);
	WaitForTid when Tid#tid.pid == WaitForTid#tid.pid ->
	    verbose("Spurious lock conflict ~w ~w: ~w -> ~w~n",
		    [Oid, Lock, Tid, WaitForTid]),
	    check_lock(Tid, Oid, Locks, TabLocks, {queue, WaitForTid}, AlreadyQ);
	WaitForTid ->
	    {no, WaitForTid}
    end;

check_lock(Tid, Oid, [], [], X, AlreadyQ) ->
    {Tab, Key} = Oid,
    if
	Key /= ?ALL, Tab /= ?GLOBAL ->
	    ObjLocks = ?ets_lookup(mnesia_lock_queue, Oid),
	    TabLocks = ?ets_lookup(mnesia_lock_queue, {Tab, ?ALL}),
	    check_queue(Tid, ObjLocks, TabLocks, X, AlreadyQ);
	true ->
	    TabLocks = ?ets_lookup(mnesia_lock_queue, {Tab, ?ALL}),
	    check_queue(Tid, [], TabLocks, X, AlreadyQ)
    end;

check_lock(Tid, Oid, [], TabLocks, X, AlreadyQ) ->
    check_lock(Tid, Oid, TabLocks, [], X, AlreadyQ).

%% Check queue for conflicting locks
%% Assume that all queued locks belongs to other tid's
check_queue(Tid, [Lock | Locks], TabLocks, X, AlreadyQ) ->
    case element(4, Lock) of
	Tid ->
	    X;
	WaitForTid when WaitForTid > Tid -> % Important order
	    check_queue(Tid, Locks, TabLocks, {queue, WaitForTid}, AlreadyQ);
	WaitForTid ->
	    %% Do not bother with the remaining queue since
	    %% the rest of the queue only may contain older
	    %% transactions.
	    case AlreadyQ of
		{no, bad_luck} -> {no, WaitForTid};
		_ -> AlreadyQ
	    end
    end;
check_queue(Tid, [], [], X, AlreadyQ) ->
    X;
check_queue(Tid, [], TabLocks, X, AlreadyQ) ->
    check_queue(Tid, TabLocks, [], X, AlreadyQ).

%% We can't queue the ixlock requests since it
%% becomes to complivated for little me :-)
%% If we encounter an object with a wlock we reject the
%% entire lock request
%% 
%% BUGBUG: this is actually a bug since we may starve

set_read_lock_on_all_keys(Tid, From, Tab, [RealKey | Tail], Orig, Ack, S) ->
    Oid = {Tab, RealKey},
    case catch can_lock(Tid, read, Oid, no) of
	yes ->
	    {granted, Val} = grant_lock(Tid, from, read, read, Oid),
	    Val2 = opt_lookup_in_client(Val, Oid, read), % Ought to be invoked
							 % in the client
	    Ack2 = lists:append(Val2, Ack),
	    set_read_lock_on_all_keys(Tid, From, Tab, Tail, Orig, Ack2, S);
	{no, Lucky} ->
	    C = #cyclic{op = read, lock = read, oid = Oid, lucky = Lucky},
	    if
		Tid#tid.pid == Lucky#tid.pid ->
		    verbose("Detected spurious lock conflict ~w: ~w -> ~w~n",
			    [Tid#tid.counter == Lucky#tid.counter, Tid, C]);
		true ->
		    ignore
	    end,
	    reply(From, {not_granted, C}, S);
	{queue, Lucky} ->
	    C = #cyclic{op = read, lock = read, oid = Oid, lucky = Lucky},
	    reply(From, {not_granted, C}, S)
    end;
set_read_lock_on_all_keys(Tid, From, Tab, [], Orig, Ack, State) ->
    reply(From, {granted, Ack, Orig}, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Release of locks

%% Release remote non-pending nodes
release_remote_non_pending(Node, Pending) ->
    %% Clear the mnesia_sticky_locks table first, to avoid
    %% unnecessary requests to the failing node
    ?ets_match_delete(mnesia_sticky_locks, {'_' , Node}),

    %% Then we have to release all locks held by processes
    %% running at the failed node and also simply remove all
    %% queue'd requests back to the failed node

    AllTids = ?ets_match(mnesia_tid_locks, {'$1', '_', '_'}),
    Tids = [T || [T] <- AllTids, Node == node(T#tid.pid), not lists:member(T, Pending)],
    do_release_tids(Tids),

    Queue = ?ets_match_object(mnesia_lock_queue, '_'),
    QueuedLocks = filter_q(Queue, Node, Pending),
    rearrange_queue(QueuedLocks).

%% Pick remote locks and make them into mnesia_tid_locks format
filter_q([Item |  Queue], Node, Pending) ->
    Tid = element(4, Item),
    if
	Node == node(Tid#tid.pid) ->
	    case lists:member(Tid, Pending) of
		false ->
		    %% A remote non-pending transaction
		    ?ets_match_delete(mnesia_tid_locks, Item),
		    case Item of
			{Oid, read_write, ReplyTo, Tid} ->
			    [{Tid, Oid, write} | filter_q(Queue, Node, Pending)];
			{Oid, Op, ReplyTo, Tid} ->
			    [{Tid, Oid, Op} | filter_q(Queue, Node, Pending)]
		    end;
		true ->
		    filter_q(Queue, Node, Pending)
	    end;
	true ->
	    filter_q(Queue, Node, Pending)
    end;
filter_q([], _, Pending) ->
    [].

do_release_tids([Tid | Tids]) ->
    do_release_tid(Tid),
    do_release_tids(Tids);
do_release_tids([]) ->
    ok.

do_release_tid(Tid) ->
    Locks = ?ets_lookup(mnesia_tid_locks, Tid),
    ?ets_delete(mnesia_tid_locks, Tid),
    release_locks(Locks),
    rearrange_queue(Locks).

release_locks([Lock | Locks]) ->
    release_lock(Lock),
    release_locks(Locks);
release_locks([]) ->
    ok.

release_lock({Tid, Oid, Op}) ->
    if
	Op == write ->
	    ?ets_delete(mnesia_held_locks, Oid);
	Op == read ->
	    ?ets_match_delete(mnesia_held_locks, {Oid, Op, Tid})
    end,
    Tab = element(1, Oid),
    decr_counter({Op, Tab}).

rearrange_queue([{Tid, {Tab, Key}, Op} | Locks]) ->
    if
	Key /= ?ALL, Tab /= ?GLOBAL ->
	    ObjWaiters = ?ets_lookup(mnesia_lock_queue, {Tab, Key}),
	    TabWaiters = ?ets_lookup(mnesia_lock_queue, {Tab, ?ALL}),
	    try_waiters(ObjWaiters, TabWaiters, Op);
	true ->
	    Pat = {{Tab, '_'}, '_' ,'_', '_'},
	    Waiters = ?ets_match_object(mnesia_lock_queue, Pat),
	    try_waiters(Waiters, Op)
    end,
    rearrange_queue(Locks);
rearrange_queue([]) ->
    ok.

try_waiters([OW | ObjWaiters], [TW | TabWaiters], OldLock) ->
    if
	element(4, OW) > element(4, TW) -> % Important order
	    try_waiter(OW, OldLock),
	    try_waiters(ObjWaiters, [TW | TabWaiters], OldLock);
	true ->
	    try_waiter(TW, OldLock),
	    try_waiters([OW | ObjWaiters], TabWaiters, OldLock)
    end;
try_waiters(ObjWaiters, TabWaiters, OldLock) ->
    try_waiters(ObjWaiters, OldLock),
    try_waiters(TabWaiters, OldLock).

try_waiters([W | Waiters], OldLock) ->
    try_waiter(W, OldLock),
    try_waiters(Waiters, OldLock);
try_waiters([], _) ->
    ok.

try_waiter({Oid, read_write, ReplyTo, Tid}, OldLock) ->
    try_waiter(Oid, read_write, read, write, ReplyTo, Tid, OldLock);
try_waiter({Oid, Op, ReplyTo, Tid}, OldLock) ->
    try_waiter(Oid, Op, Op, Op, ReplyTo, Tid, OldLock).

try_waiter(Oid, Op, SimpleOp, Lock, ReplyTo, Tid, OldLock) ->
    case catch can_lock(Tid, Lock, Oid, {queue, bad_luck}) of
	yes ->
	    %% Delete from queue: Nice place for trace output
	    ?ets_match_delete(mnesia_lock_queue, {Oid, Op, ReplyTo, Tid}),
	    Reply = grant_lock(Tid, ReplyTo, SimpleOp, Lock, Oid),
	    ReplyTo ! {?MODULE, node(), Reply};
	{queue, _} ->
	    ignore; % Keep waiter in queue
	{no, Lucky} ->
	    C = #cyclic{op = SimpleOp, lock = Lock, oid = Oid, lucky = Lucky},
	    verbose("** ERROR ** Possible deadlock in lock queue ~w ~w ~w: old = ~w~n",
		    [OldLock, Tid, ReplyTo, C]),
	    ?ets_match_delete(mnesia_lock_queue, {Oid, Op, ReplyTo, Tid}),
	    Reply = {not_granted, C},
	    ReplyTo ! {?MODULE, node(), Reply}
    end.

%% ********************* end server code ********************
%% The following code executes at the client side of a transaction

mnesia_down(N, Pending) ->
    case whereis(?MODULE) of
	undefined ->
	    %% Takes care of mnesia_down's in early startup
	    mnesia_monitor:mnesia_down(?MODULE, N);
	Pid ->
	    %% Syncronously call needed in order to avoid
	    %% race with mnesia_tm's coordinator processes
	    %% that may restart and acquire new locks.
	    %% mnesia_monitor ensures the sync.
	    Pid ! {release_remote_non_pending, N, Pending}
    end.

%% Aquire a write lock, but do a read, used by 
%% mnesia:wread/1

rwlock(Tid, Store, Oid) ->
    {Tab, Key} = Oid,
    case val({Tab, where_to_read}) of
	nowhere ->
	    mnesia:abort({no_exists, Tab});
	Node ->
	    Lock = write,
	    case need_lock(Store, Tab, Key, Lock)  of
		yes ->
		    Ns = w_nodes(Tab),
		    Res = get_rwlocks_on_nodes(Ns, Ns, Node, Store, Tid, Oid),
		    ?ets_insert(Store, {{locks, Tab, Key}, Lock}),
		    Res;
		no ->
		    if
			Key == ?ALL ->
			    w_nodes(Tab);
			Tab == ?GLOBAL ->
			    w_nodes(Tab);
			true ->
			    dirty_rpc(Node, Tab, Key, Lock)
		    end
	    end
    end.

get_rwlocks_on_nodes([Node | Tail], Orig, Node, Store, Tid, Oid) ->
    Op = {self(), {read_write, Tid, Oid}},
    {?MODULE, Node} ! Op,
    ?ets_insert(Store, {nodes, Node}),
    get_rwlocks_on_nodes(Tail, Orig, Node, Store, Tid, Oid);
get_rwlocks_on_nodes([Node | Tail], Orig, OtherNode, Store, Tid, Oid) ->
    Op = {self(), {write, Tid, Oid}},
    {?MODULE, Node} ! Op,
    ?ets_insert(Store, {nodes, Node}),
    get_rwlocks_on_nodes(Tail, Orig, OtherNode, Store, Tid, Oid);
get_rwlocks_on_nodes([], Orig, Node, Store, Tid, Oid) ->
    receive_wlocks(Orig, Orig, Store, Oid).

%% Return a list of nodes or abort transaction
%% WE also insert any additional where_to_write nodes
%% in the local store under the key == nodes

w_nodes(Tab) ->
    Nodes = ?catch_val({Tab, where_to_write}),
    case Nodes of
	[_ | _] -> Nodes;
	_ ->  mnesia:abort({no_exists, Tab})
    end.

%% aquire a sticky wlock, a sticky lock is a lock
%% which remains at this node after the termination of the
%% transaction.

sticky_wlock(Tid, Store, Oid) ->
    Tab = element(1, Oid),
    N = val({Tab, where_to_read}), 
    if
	node() == N ->
	    ?MODULE ! {self(), {test_set_sticky, Tid, Oid}},
	    receive
		{?MODULE, N, granted} ->
		    granted;
		{?MODULE, N, {not_granted, Reason}} ->
		    exit({aborted, Reason});
		{?MODULE, N, not_stuck} ->
		    wlock(Tid, Store, Oid),           %% pefect sync
		    wlock(Tid, Store, {Tab, ?STICK}), %% max one sticker/table
		    Ns = val({Tab, where_to_write}),
		    rpc:abcast(Ns, ?MODULE, {stick, Oid, N});
		{mnesia_down, N} ->
		    exit({aborted, {node_not_running, N}});
		{?MODULE, N, {stuck_elsewhere, N2}} ->
		    rlock(Tid, Store, {Tab, ?ALL}),
		    wlock(Tid, Store, Oid),           %% pefect sync
		    wlock(Tid, Store, {Tab, ?STICK}), %% max one sticker/table
		    Ns = val({Tab, where_to_write}),
		    rpc:abcast(Ns, ?MODULE, {unstick, Tab})
	    end;
	true ->
	    mnesia:abort({not_local, Tab})
    end.

%% aquire a wlock on Oid
%% We store a {Tabname, write, Tid} in all locktables
%% on all nodes containing a copy of Tabname
%% We also store an item {{locks, Tab, Key}, write} in the 
%% local store when we have aquired the lock.
%% 
wlock(Tid, Store, Oid) ->
    {Tab, Key} = Oid,
    case need_lock(Store, Tab, Key, write) of
	yes ->
	    Ns = w_nodes(Tab),
	    Op = {self(), {write, Tid, Oid}},
	    ?ets_insert(Store, {{locks, Tab, Key}, write}),
	    get_wlocks_on_nodes(Ns, Ns, Store, Op, Oid);
	no when Key /= ?ALL, Tab /= ?GLOBAL ->
	    [];
	no ->
	    w_nodes(Tab)
    end.

wlock_table(Tid, Store, Tab) ->
    wlock(Tid, Store, {Tab, ?ALL}).

%% Write lock even if the table does not exist

wlock_no_exist(Tid, Store, Tab, Ns) ->
    Oid = {Tab, ?ALL},
    Op = {self(), {write, Tid, Oid}},
    get_wlocks_on_nodes(Ns, Ns, Store, Op, Oid).



need_lock(Store, Tab, Key, LockPattern) ->
    TabL = ?ets_match_object(Store, {{locks, Tab, ?ALL}, LockPattern}),
    if 
	TabL == [] ->
	    KeyL = ?ets_match_object(Store, {{locks, Tab, Key}, LockPattern}),
	    if 
		KeyL == [] ->
		    yes;
		true  ->
		    no
	    end;
	true ->
	    no
    end.

%% We first send lock requests to the lockmanagers on all 
%% nodes holding a copy of the table

get_wlocks_on_nodes([Node | Tail], Orig, Store, Request, Oid) ->
    {?MODULE, Node} ! Request,
    ?ets_insert(Store, {nodes, Node}),
    get_wlocks_on_nodes(Tail, Orig, Store, Request, Oid);
get_wlocks_on_nodes([], Orig, Store, Request, Oid) ->
    receive_wlocks(Orig, Orig, Store, Oid).

receive_wlocks([Node | Tail], Res, Store, Oid) ->
    receive
	{?MODULE, Node, granted} ->
	    receive_wlocks(Tail, Res, Store, Oid);
	{?MODULE, Node, {granted, Val}} -> %% for rwlocks
	    Val2 = opt_lookup_in_client(Val, Oid, write),
	    receive_wlocks(Tail, Val2, Store, Oid);
	{?MODULE, Node, {not_granted, Reason}} ->
	    Reason1 = {aborted, Reason},
	    flush_remaining(Tail, Node, Reason1);
	{mnesia_down, Node} ->
	    Reason1 = {aborted, {node_not_running, Node}},
	    flush_remaining(Tail, Node, Reason1);
	{?MODULE, Node, {switch, Node2, Req}} -> %% for rwlocks
	    ?ets_insert(Store, {nodes, Node2}),
	    {?MODULE, Node2} ! Req,
	    receive_wlocks([Node2 | Tail], Res, Store, Oid)
    end;
receive_wlocks([], Res, _Store, _Oid) ->
    Res.
 
flush_remaining([], SkipNode, Res) ->
    exit(Res);
flush_remaining([SkipNode | Tail ], SkipNode, Res) ->
    flush_remaining(Tail, SkipNode, Res);
flush_remaining([Node | Tail], SkipNode, Res) ->
    receive
	{?MODULE, Node, _} ->
	    flush_remaining(Tail, SkipNode, Res);
	{mnesia_down, Node} ->
	    flush_remaining(Tail, SkipNode, {aborted, {node_not_running, Node}})
    end.

opt_lookup_in_client(lookup_in_client, Oid, Lock) ->
    {Tab, Key} = Oid,
    case catch mnesia_lib:db_get(Tab, Key) of
	{'EXIT', _} ->
	    %% Table has been deleted from this node,
	    %% restart the transaction.
	    C = #cyclic{op = read, lock = Lock, oid = Oid, lucky = nowhere},
	    mnesia:abort(C);
	Val -> 
	    Val
    end;
opt_lookup_in_client(Val, Oid, Lock) ->
    Val.

return_granted_or_nodes({_, ?ALL}   , Nodes) -> Nodes;
return_granted_or_nodes({?GLOBAL, _}, Nodes) -> Nodes;
return_granted_or_nodes(_           , Nodes) -> granted.
    
%% We store a {Tab, read, From} item in the 
%% locks table on the node where we actually do pick up the object
%% and we also store an item {lock, Oid, read} in our local store
%% so that we can release any locks we hold when we commit.
%% This function not only aquires a read lock, but also reads the object

%% Oid's are always {Tab, Key} tuples
rlock(Tid, Store, Oid) ->
    {Tab, Key} = Oid,
    case val({Tab, where_to_read}) of
	nowhere ->
	    mnesia:abort({no_exists, Tab});
	Node ->
	    case need_lock(Store, Tab, Key, '_') of
		yes ->
		    R = l_request(Node, {read, Tid, Oid}, Store),
		    rlock_get_reply(Node, Store, Oid, R);
		no ->
		    if
			Key == ?ALL ->
			    [Node];
			Tab == ?GLOBAL ->
			    [Node];
			true ->
			    dirty_rpc(Node, Tab, Key, read)
		    end
	    end
    end.

dirty_rpc(nowhere, Tab, Key, Lock) ->
    mnesia:abort({no_exists, {Tab, Key}});
dirty_rpc(Node, _Tab, ?ALL, _Lock) ->
    [Node];
dirty_rpc(Node, ?GLOBAL, _Key, _Lock) ->
    [Node];
dirty_rpc(Node, Tab, Key, Lock) ->
    Args = [Tab, Key],
    case rpc:call(Node, mnesia_lib, db_get, Args) of
	{badrpc, Reason} ->
	    case val({Tab, where_to_read}) of
		Node ->
		    ErrorTag = mnesia_lib:dirty_rpc_error_tag(Reason),
		    mnesia:abort({ErrorTag, Args});
		NewNode ->
		    %% Table has been deleted from the node,
		    %% restart the transaction.
		    C = #cyclic{op = read, lock = Lock, oid = {Tab, Key}, lucky = nowhere},
		    exit({aborted, C})
	    end;
	Other ->
	    Other
    end.

rlock_get_reply(Node, Store, Oid, {granted, V}) ->
    {Tab, Key} = Oid,
    ?ets_insert(Store, {{locks, Tab, Key}, read}),
    ?ets_insert(Store, {nodes, Node}),
    opt_lookup_in_client(V, Oid, read);
rlock_get_reply(Node, Store, Oid, granted) ->
    {Tab, Key} = Oid,
    ?ets_insert(Store, {{locks, Tab, Key}, read}),
    ?ets_insert(Store, {nodes, Node}),
    return_granted_or_nodes(Oid, [Node]);
rlock_get_reply(Node, Store, Tab, {granted, V, RealKeys}) ->
    L = fun(K) -> ?ets_insert(Store, {{locks, Tab, K}, read}) end,
    lists:foreach(L, RealKeys),
    ?ets_insert(Store, {nodes, Node}),
    V;
rlock_get_reply(Node, Store, Oid, {not_granted , Reason}) ->
    exit({aborted, Reason});

rlock_get_reply(Node, Store, Oid, {switch, N2, Req}) ->
    ?ets_insert(Store, {nodes, N2}),
    {?MODULE, N2} ! Req,
    rlock_get_reply(N2, Store, Oid, l_req_rec(N2, Store)).


rlock_table(Tid, Store, Tab) ->
    rlock(Tid, Store, {Tab, ?ALL}).

ixrlock(Tid, Store, Tab, IxKey, Pos) ->
    case val({Tab, where_to_read}) of
	nowhere ->
	    mnesia:abort({no_exists, Tab});
	Node ->
	    R = l_request(Node, {ix_read, Tid, Tab, IxKey, Pos}, Store),
	    rlock_get_reply(Node, Store, Tab, R)
    end.

%% Grabs the locks or exits
global_lock(Tid, Store, Item, write, Ns) ->
    Oid = {?GLOBAL, Item},
    Op = {self(), {write, Tid, Oid}},
    get_wlocks_on_nodes(Ns, Ns, Store, Op, Oid);
global_lock(Tid, Store, Item, read, Ns) ->
    Oid = {?GLOBAL, Item},
    send_requests(Ns, {read, Tid, Oid}),
    rec_requests(Ns, Oid, Store),
    Ns.

send_requests([Node | Nodes], X) ->
    {?MODULE, Node} ! {self(), X},
    send_requests(Nodes, X);
send_requests([], X) ->
    ok.

rec_requests([Node | Nodes], Oid, Store) ->
    Res = l_req_rec(Node, Store),
    case catch rlock_get_reply(Node, Store, Oid, Res) of
	{'EXIT', Reason} ->
	    flush_remaining(Nodes, Node, Reason);
	_ ->
	    rec_requests(Nodes, Oid, Store)
    end;
rec_requests([], Oid, Store) ->
    ok.

get_held_locks() ->
    mnesia_lib:safe_match_object(ets, mnesia_held_locks, '_').

get_lock_queue() ->
    mnesia_lib:safe_match_object(ets, mnesia_lock_queue, '_').

do_stop() ->
    exit(shutdown).

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% System upgrade

system_continue(Parent, Debug, State) ->
    loop(State).

system_terminate(Reason, Parent, Debug, State) ->
    do_stop().

system_code_change(State, Module, OldVsn, Extra) ->
    exit(not_supported).
