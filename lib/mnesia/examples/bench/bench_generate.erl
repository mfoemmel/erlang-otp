%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% File    : bench_generate.hrl
%%% Author  : Hakan Mattsson <hakan@cslab.ericsson.se>
%%% Purpose : Start request generators and collect statistics
%%% Created : 21 Jun 2001 by Hakan Mattsson <hakan@cslab.ericsson.se>
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(bench_generate).
-author('hakan@cslab.ericsson.se').

-include("bench.hrl").

%% Public
-export([start/1]).

%% Internal
-export([
	 generator_init/2,
	 worker_loop/1,
	 remote_spawn_link_opt/5,
	 link_migrator/5
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The traffic generator
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% -------------------------------------------------------------------
%% Start request generators
%% -------------------------------------------------------------------

start(C) when record(C, config) ->
    process_flag(trap_exit, true),
    Nodes     = C#config.generator_nodes,
    PerNode   = C#config.n_generators_per_node,
    Timer     = C#config.generator_warmup,
    ?d("~n", []),
    ?d("Start ~p request generators each at ~p nodes...~n",
       [PerNode, length(Nodes)]),
    ?d("~n", []),
    warmup_sticky(C),
    ?d("    ~p seconds warmup...~n", [Timer div 1000]),
    Monitor = self(),
    Options = [{fullsweep_after, 0}],
    GeneratorPids =
	[spawn_link_opt(Node, ?MODULE, generator_init, [Monitor, C], Options) ||
	    Node <- Nodes,
	    _    <- lists:seq(1, PerNode)],
    erlang:send_after(Timer, self(), warmup_done),
    monitor_generators(C, GeneratorPids).

warmup_sticky(C) ->
    %% Select one node per fragment as master node
    Tabs = [subscriber, session, server, suffix],
    Fun = fun(S) ->
		  {Node, _, Wlock} = nearest_node(S, transaction, C),
		  Stick = fun() -> [mnesia:read({T, S}, S, Wlock) || T <- Tabs] end,
		  Args = [transaction, Stick, [], mnesia_frag],
		  rpc:call(Node, mnesia, activity, Args)
	  end,
    Suffixes = lists:seq(0, C#config.n_fragments - 1), % Assume even distrib.
    lists:foreach(Fun, Suffixes).

%% Main loop for benchmark monitor
monitor_generators(C, GeneratorPids) ->
    receive
        warmup_done ->
            multicall(GeneratorPids, reset_statistics),	    
	    Timer = C#config.generator_duration,
	    ?d("    ~p seconds actual benchmarking...~n", [Timer div 1000]),
	    erlang:send_after(Timer, self(), measurement_done),
	    monitor_generators(C, GeneratorPids);
        measurement_done ->
            Stats = multicall(GeneratorPids, get_statistics),	    
	    Timer = C#config.generator_cooldown,
	    ?d("    ~p seconds cooldown...~n", [Timer div 1000]),
	    erlang:send_after(Timer, self(), {cooldown_done, Stats}),
	    monitor_generators(C, GeneratorPids);
        {cooldown_done, Stats} ->
            multicall(GeneratorPids, stop),
            display_statistics(Stats, C, GeneratorPids),
            ok;
        {'EXIT', Pid, Reason} = Bad ->
            case lists:member(Pid, GeneratorPids) of
                true ->
                    error_logger:format("Generator on node ~p died: ~p~n",
					[node(Pid), Bad]),
                    multicall(GeneratorPids, stop),
                    {error, [{Pid, {'EXIT', Reason}}]};
                false ->
                    monitor_generators(C, GeneratorPids)
            end
    end.

%% Send message to a set of processes and wait for their replies
multicall(Pids, Message) ->
    Send =
        fun(Pid) -> 
                Ref = erlang:monitor(process, Pid),
                Pid ! {self(), Ref, Message},
                {Pid, Ref}
        end,
    PidRefs = lists:map(Send, Pids),
    Collect =
        fun({Pid, Ref}) ->
                receive
                    {'DOWN', Ref, process, Pid, Reason} ->
                        {Pid, {'EXIT', Reason}};
                    {Pid, Ref, Reply} ->
                        erlang:demonitor(Ref),
                        {Pid, Reply}
                end
        end,
    lists:map(Collect, PidRefs).

%% Initialize a traffic generator
generator_init(Monitor, C) ->
    process_flag(trap_exit, true),
    {_Mega, Sec, Micro} = erlang:now(),
    Uniq = lists:sum(binary_to_list(term_to_binary(make_ref()))),
    random:seed(Uniq, Sec, Micro),
    %% net_kernel:monitor_nodes(true), BUGBUG: needed to reinit workers
    init_workers(C#config.table_nodes),
    Counters = reset_counters(C, C#config.statistics_detail),
    SessionTab = ets:new(bench_sessions, [public, {keypos, 1}]),
    generator_loop(Monitor, C, SessionTab, Counters).

%% Main loop for traffic generator
generator_loop(Monitor, C, SessionTab, Counters) ->
    receive
        {ReplyTo, Ref, get_statistics} ->
	    Stats = get_counters(C, Counters),
	    ReplyTo ! {self(), Ref, Stats},
	    generator_loop(Monitor, C, SessionTab, Counters);
        {ReplyTo, Ref, reset_statistics} ->
	    Stats = get_counters(C, Counters),
	    Counters2 = reset_counters(C, Counters),
	    ReplyTo ! {self(), Ref, Stats},
	    generator_loop(Monitor, C, SessionTab, Counters2);
        {ReplyTo, Ref, stop} ->
	    exit(shutdown);
        {'EXIT', Monitor, Reason} ->
	    exit(Reason);
	{'EXIT', Pid, Reason} ->
	    Node = node(Pid),
	    ?d("Worker on node ~p died: ~p~n", [Node, Reason]),
	    Key = {worker,Node},
	    case get(Key) of
		undefined -> ignore;
		Pid       -> erase(Key);
		_         -> ignore
	    end,
	    generator_loop(Monitor, C, SessionTab, Counters);
	{nodedown, Node} ->
	    %% Only bother about node up's
	    generator_loop(Monitor, C, SessionTab, Counters);
	{nodeup, Node} ->
	    ?d("Worker on node ~p restarted: ~p~n", [Node, nodeup]),
	    Key = {worker,Node},
	    case get(Key) of
		undefined -> init_workers([Node]);
		_         -> ignore
	    end,
	    generator_loop(Monitor, C, SessionTab, Counters)
    after 0 ->
	    {Name, {Node, Activity, Wlock}, Fun, CommitSessions} =
		gen_trans(C, SessionTab),
	    Before = erlang:now(),
	    Args = [Activity, Fun, [Wlock], mnesia_frag],
	    %% Res  = (catch rpc:call(Node, mnesia, activity, Args)),
	    Res  = call_worker(Node, Args),
	    After = erlang:now(),
	    Elapsed = elapsed(Before, After),
	    post_eval(Monitor, C, Elapsed, Res, Name, Node, CommitSessions, SessionTab, Counters)
    end.

%% Perform a transaction on a node near the data
call_worker(Node, [Activity, Fun, Extra, Mod]) when Node == node() ->
    catch mnesia:activity(Activity, Fun, Extra, Mod);
call_worker(Node, Args) ->
    case get({worker, Node}) of
	Pid when pid(Pid) ->
	    Pid ! {activity, self(), Args},
	    receive
		{'EXIT', Pid, Reason} ->
		    {'EXIT', Pid, Reason};
		{activity_result, Pid, Result} ->
		    Result
	    end;
	undefined ->
	    {aborted, please_retry_on_other_node}
    end.

%% Spawn one worker process per (table) node
init_workers(Nodes) ->
    GenPid = self(),
    Options = [{fullsweep_after, 0}],
    [put({worker, N}, spawn_link_opt(N, ?MODULE, worker_loop, [GenPid], Options))
     || N <- Nodes, N /= node()].

spawn_link_opt(Node, M, F, A, Options) ->
    case rpc:call(Node, ?MODULE, remote_spawn_link_opt, [self(), M, F, A, Options]) of
	{ok, Pid} -> 
	    Pid;
	Bad ->
	    exit({?MODULE, spawn_link_opt, [Node, M, F, A, Options], Bad})
    end.

remote_spawn_link_opt(RemoteParent, M, F, A, Options) ->
    Pid = spawn_opt(?MODULE, link_migrator, [self(), RemoteParent, M, F, A], [link | Options]),
    receive
	{link_migrated, Pid} ->
	    unlink(Pid),
	    {ok, Pid}
    end.

link_migrator(LocalParent, RemoteParent, M, F, A) ->
    %% Do not trap exit
    link(RemoteParent),
    LocalParent ! {link_migrated, self()},
    erlang:apply(M, F, A).

%% Main loop for remote workers
worker_loop(Parent) ->
    receive
	{activity, Parent, [Activity, Fun, Extra, Mod]} ->
	    Result = (catch mnesia:activity(Activity, Fun, Extra, Mod)),
	    Parent ! {activity_result, self(), Result},
	    worker_loop(Parent)
    end.


elapsed({Before1, Before2, Before3}, {After1, After2, After3}) ->
    After  = After1  * 1000000000000  + After2  * 1000000 + After3,
    Before = Before1 * 1000000000000  + Before2 * 1000000 + Before3,
    After - Before.

%% Lookup counters
get_counters(C, {table, Tab}) ->
    ets:match_object(Tab, '_');
get_counters(C, {NM, NC, NA, NB}) ->
    Trans = any,
    Node  = somewhere,
    [{{Trans, n_micros, Node}, NM},
     {{Trans, n_commits, Node}, NC},
     {{Trans, n_aborts, Node}, NA},
     {{Trans, n_branches_executed, Node}, NB}].

% Clear all counters    
reset_counters(C, normal) ->
    {0, 0, 0, 0};
reset_counters(C, {_, _, _, _}) ->
    reset_counters(C, normal);
reset_counters(C, debug) ->
    CounterTab = ets:new(bench_pending, [public, {keypos, 1}]),
    reset_counters(C, {table, CounterTab});
reset_counters(C, debug2) ->
    CounterTab = ets:new(bench_pending, [public, {keypos, 1}]),
    reset_counters(C, {table, CounterTab});
reset_counters(C, {table, Tab} = Counters) ->
    Names = [n_micros, n_commits, n_aborts, n_branches_executed],
    Nodes = C#config.generator_nodes ++ C#config.table_nodes,
    TransTypes = [t1, t2, t3, t4, t5, ping],
    [ets:insert(Tab, {{Trans, Name, Node}, 0}) || Name <- Names,
						  Node <- Nodes,
						  Trans <- TransTypes],
    Counters.

%% Determine the outcome of a transaction and increment the counters
post_eval(Monitor, C, Elapsed, {badrpc, Res}, Name, Node, CommitSessions, SessionTab, Counters) ->
    post_eval(Monitor, C, Elapsed, Res, Name, Node, CommitSessions, SessionTab, Counters);
post_eval(Monitor, C, Elapsed, Res, Name, Node, CommitSessions, SessionTab, {table, Tab} = Counters) ->
    case Res of
	{do_commit, BranchExecuted, _} ->
	    incr(Tab, {Name, n_micros, Node}, Elapsed),
	    incr(Tab, {Name, n_commits, Node}, 1),
	    case BranchExecuted of
		true  ->
		    incr(Tab, {Name, n_branches_executed, Node}, 1),
		    commit_session(CommitSessions),
		    generator_loop(Monitor, C, SessionTab, Counters);
		false ->
		    generator_loop(Monitor, C, SessionTab, Counters)
	    end;
	{'EXIT', {aborted, {do_rollback, BranchExecuted, _}}} ->
	    incr(Tab, {Name, n_micros, Node}, Elapsed),
	    incr(Tab, {Name, n_aborts, Node}, 1),
	    case BranchExecuted of
		true  ->
		    incr(Tab, {Name, n_branches_executed, Node}, 1),
		    generator_loop(Monitor, C, SessionTab, Counters);
		false ->
		    generator_loop(Monitor, C, SessionTab, Counters)
	    end;
	_ ->
	    %% BUGBUG: Retry trans on some alternate node
	    incr(Tab, {Name, n_micros, Node}, Elapsed),
	    incr(Tab, {Name, n_aborts, Node}, 1),
	    generator_loop(Monitor, C, SessionTab, Counters)
    end;
post_eval(Monitor, C, Elapsed, Res, Name, Node, CommitSessions, SessionTab, {NM, NC, NA, NB}) ->
    case Res of
	{do_commit, BranchExecuted, _} ->
	    case BranchExecuted of
		true  ->
		    commit_session(CommitSessions),
		    generator_loop(Monitor, C, SessionTab, {NM + Elapsed, NC + 1, NA, NB + 1});
		false ->
		    generator_loop(Monitor, C, SessionTab, {NM + Elapsed, NC + 1, NA, NB})
	    end;
	{'EXIT', {aborted, {do_rollback, BranchExecuted, _}}} ->
	    case BranchExecuted of
		true  ->
		    generator_loop(Monitor, C, SessionTab, {NM + Elapsed, NC, NA + 1, NB + 1});
		false ->
		    generator_loop(Monitor, C, SessionTab, {NM + Elapsed, NC, NA + 1, NB})
	    end;
	_ ->
	    %% BUGBUG: Retry trans on some alternate node
	    generator_loop(Monitor, C, SessionTab, {NM + Elapsed, NC, NA + 1, NB})
    end.

incr(Tab, Counter, Incr) ->
    ets:update_counter(Tab, Counter, Incr).

commit_session(no_fun) ->
    ignore;
commit_session(Fun) when function(Fun) ->
    Fun().

%% Randlomly choose a transaction type according to benchmar spec
gen_trans(C, SessionTab) when C#config.generator_profile == random ->
    case random:uniform(100) of
        Rand when Rand >   0, Rand =<  25 -> gen_t1(C, SessionTab);
        Rand when Rand >  25, Rand =<  50 -> gen_t2(C, SessionTab);
        Rand when Rand >  50, Rand =<  70 -> gen_t3(C, SessionTab);
        Rand when Rand >  70, Rand =<  85 -> gen_t4(C, SessionTab);
        Rand when Rand >  85, Rand =< 100 -> gen_t5(C, SessionTab)
    end;
gen_trans(C, SessionTab) ->
    case C#config.generator_profile of
        t1   -> gen_t1(C, SessionTab);
        t2   -> gen_t2(C, SessionTab);
        t3   -> gen_t3(C, SessionTab);
        t4   -> gen_t4(C, SessionTab);
        t5   -> gen_t5(C, SessionTab);
	ping -> gen_ping(C, SessionTab)
    end.
        
gen_t1(C, SessionTab) ->
    SubscrId    = random:uniform(C#config.n_subscribers) - 1,
    SubscrKey   = bench_trans:number_to_key(SubscrId, C),
    Location    = 4711,
    ChangedBy   = <<4711:(8*25)>>,
    ChangedTime = <<4711:(8*25)>>,
    {t1,
     nearest_node(SubscrId, transaction, C),
     fun(Wlock) -> bench_trans:update_current_location(Wlock, SubscrKey, Location, ChangedBy, ChangedTime) end,
     no_fun
    }.

gen_t2(C, SessionTab) ->
    SubscrId  = random:uniform(C#config.n_subscribers) - 1,
    SubscrKey = bench_trans:number_to_key(SubscrId, C),
    {t2,
     nearest_node(SubscrId, sync_dirty, C),
     %%nearest_node(SubscrId, transaction, C),
     fun(Wlock) -> bench_trans:read_current_location(Wlock, SubscrKey) end,
     no_fun
    }.

gen_t3(C,  SessionTab) ->
    case ets:first(SessionTab) of
	'$end_of_table' ->
	    %% This generator does not have any session,
	    %% try reading someone elses session details
	    SubscrId  = random:uniform(C#config.n_subscribers) - 1,
	    SubscrKey = bench_trans:number_to_key(SubscrId, C),
	    ServerId  = random:uniform(C#config.n_servers) - 1,
	    ServerBit = 1 bsl ServerId,
	    {t3,
	     nearest_node(SubscrId, transaction, C),
	     fun(Wlock) -> bench_trans:read_session_details(Wlock, SubscrKey, ServerBit, ServerId) end,
	     no_fun
	    };
	{SubscrId, SubscrKey, ServerId}  ->
	    %% This generator do have a session,
	    %% read its session details
	    ServerBit = 1 bsl ServerId,
	    {t3,
	     nearest_node(SubscrId, transaction, C),
	     fun(Wlock) -> bench_trans:read_session_details(Wlock, SubscrKey, ServerBit, ServerId) end,
	     no_fun
	    }
    end.

gen_t4(C, SessionTab) ->
    %% This generator may already have sessions,
    %% create a new session and hope that no other
    %% generator already has occupied it
    SubscrId   = random:uniform(C#config.n_subscribers) - 1,
    SubscrKey  = bench_trans:number_to_key(SubscrId, C),
    ServerId   = random:uniform(C#config.n_servers) - 1,
    ServerBit  = 1 bsl ServerId,
    Details    = <<4711:(8*2000)>>,
    DoRollback = (random:uniform(100) =< 2),
    Insert     = fun() -> ets:insert(SessionTab, {{SubscrId, SubscrKey, ServerId}, self()}) end,
    {t4,
     nearest_node(SubscrId, transaction, C),
     fun(Wlock) -> bench_trans:create_session_to_server(Wlock, SubscrKey, ServerBit, ServerId, Details, DoRollback) end,
     Insert
    }.

gen_t5(C, SessionTab) ->
    case ets:first(SessionTab) of
	'$end_of_table' ->
	    %% This generator does not have any session,
	    %% try to delete someone elses session details
	    SubscrId   = random:uniform(C#config.n_subscribers) - 1,
	    SubscrKey  = bench_trans:number_to_key(SubscrId, C),
	    ServerId   = random:uniform(C#config.n_servers) - 1,
	    ServerBit  = 1 bsl ServerId,
	    DoRollback = (random:uniform(100) =< 2),
	    {t5,
	     nearest_node(SubscrId, transaction, C),
	     fun(Wlock) -> bench_trans:delete_session_from_server(Wlock, SubscrKey, ServerBit, ServerId, DoRollback) end,
	     no_fun
	    };
	{SubscrId, SubscrKey, ServerId}  ->
	    %% This generator do have at least one session,
	    %% delete it.
	    ServerBit  = 1 bsl ServerId,	
	    DoRollback = (random:uniform(100) =< 2),
	    Delete     = fun() -> ets:delete(SessionTab, {SubscrId, SubscrKey, ServerId}) end,
	    {t5,
	     nearest_node(SubscrId, transaction, C),
	     fun(Wlock) -> bench_trans:delete_session_from_server(Wlock, SubscrKey, ServerBit, ServerId, DoRollback) end,
	     Delete
	    }
    end.

gen_ping(C, SessionTab) ->
    SubscrId   = random:uniform(C#config.n_subscribers) - 1,
    {ping,
     nearest_node(SubscrId, transaction, C),
     fun(Wlock) -> {do_commit, true, []} end,
     no_fun
    }.    

%% Select a node as near as the subscriber data as possible
nearest_node(SubscrId, Activity, C) ->
    Suffix = bench_trans:number_to_suffix(SubscrId),
    case mnesia_frag:table_info(t, s, {suffix, Suffix}, where_to_write) of
	[] ->
	    {node(), Activity, write};
	[Node] ->
	    {Node, Activity, write};
	Nodes ->
	    Wlock = C#config.write_lock_type,
	    case Wlock of
		sticky_write ->
		    Node = pick_node(Suffix, C, Nodes),
		    {Node, Activity, Wlock};
		write ->
		    case lists:member(node(), Nodes) of
			true ->
			    {node(), Activity, Wlock};
			false ->
			    Node = pick_node(Suffix, C, Nodes),
			    {Node, Activity, Wlock}
		    end
	    end
    end.
  
pick_node(Suffix, C, Nodes) ->
    Ordered = lists:sort(Nodes),
    NumberOfActive = length(Ordered),
    PoolSize = length(C#config.table_nodes),
    Suffix2 = 
	case PoolSize rem NumberOfActive of
	    0 -> Suffix div (PoolSize div NumberOfActive);
	    _ -> Suffix
	end,
    N = (Suffix2 rem NumberOfActive) + 1,
    lists:nth(N, Ordered).

display_statistics(Stats, C, GeneratorPids) ->
    FlatStats = [{Type, Name, EvalNode, node(GenPid), Count} ||
                    {GenPid, GenStats} <- Stats,
                    {{Type, Name, EvalNode}, Count} <- GenStats],
    TotalStats = calc_stats_per_tag(lists:keysort(2, FlatStats), 2, []),
    {value, {n_aborts, 0, NA, 0, 0}} =
     lists:keysearch(n_aborts, 1, TotalStats ++ [{n_aborts, 0, 0, 0, 0}]),
    {value, {n_commits, NC, 0, 0, 0}} =
	lists:keysearch(n_commits, 1, TotalStats ++ [{n_commits, 0, 0, 0, 0}]),
    {value, {n_branches_executed, 0, 0, NB, 0}} =
	lists:keysearch(n_branches_executed, 1, TotalStats ++ [{n_branches_executed, 0, 0, 0, 0}]),
    {value, {n_micros, 0, 0, 0, AccMicros}} =
	lists:keysearch(n_micros, 1, TotalStats ++ [{n_micros, 0, 0, 0, 0}]),
    NT = NA + NC,
    NG = length(GeneratorPids),
    NTN = length(C#config.table_nodes),
    WallMicros = C#config.generator_duration * 1000 * NG,
    Overhead = (catch (WallMicros - AccMicros) / WallMicros),
    ?d("~n", []),
    ?d("Benchmark result...~n", []),
    ?d("~n", []),
    ?d("    ~p transactions per second (TPS).~n", [catch ((NT * 1000000 * NG) div AccMicros)]),
    ?d("    ~p TPS per table node.~n", [catch ((NT * 1000000 * NG) div (AccMicros * NTN))]),
    ?d("    ~p micro seconds in average per transaction, including latency.~n",
       [catch (AccMicros div NT)]),
    ?d("    ~p transactions. ~f% generator overhead.~n", [NT, Overhead]),

    TypeStats = calc_stats_per_tag(lists:keysort(1, FlatStats), 1, []),
    EvalNodeStats = calc_stats_per_tag(lists:keysort(3, FlatStats), 3, []),
    GenNodeStats = calc_stats_per_tag(lists:keysort(4, FlatStats), 4, []),
    if
	C#config.statistics_detail == normal ->
	    ignore;
	true ->
	    ?d("~n", []),
	    ?d("Statistics per transaction type...~n", []),
	    ?d("~n", []),
	    display_type_stats("    ", TypeStats, NT, AccMicros),

	    ?d("~n", []),
	    ?d("Transaction statistics per table node...~n", []),
	    ?d("~n", []),
	    display_calc_stats("    ", EvalNodeStats, NT, AccMicros),

	    ?d("~n", []),
	    ?d("Transaction statistics per generator node...~n", []),
	    ?d("~n", []),
	    display_calc_stats("    ", GenNodeStats, NT, AccMicros)
    end,
    if
	C#config.statistics_detail /= debug2 ->
	    ignore;
	true ->
	    io:format("~n", []),
	    io:format("------ Test Results ------~n", []),
	    io:format("Length        : ~p sec~n", [C#config.generator_duration div 1000]),
	    Host = lists:nth(2, string:tokens(atom_to_list(node()), [$@])),
	    io:format("Processor     : ~s~n", [Host]),
	    io:format("Number of Proc: ~p~n", [NG]),
	    io:format("~n", []),
	    display_trans_stats("    ", TypeStats, NT, AccMicros, NG),
	    io:format("~n", []),
	    io:format("  Overall Statistics~n", []),
	    io:format("     Transactions: ~p~n", [NT]),
	    io:format("     Inner       : ~p TPS~n", [catch ((NT * 1000000 * NG) div AccMicros)]),
	    io:format("     Outer       : ~p TPS~n", [catch ((NT * 1000000 * NG) div WallMicros)]),
	    io:format("~n", [])
    end.
    
 
display_calc_stats(Prefix, [{Tag, 0, 0, 0, 0} | Rest], NT, Micros) ->
    display_calc_stats(Prefix, Rest, NT, Micros);   
display_calc_stats(Prefix, [{Tag, NC, NA, NB, NM} | Rest], NT, Micros) ->
    ?d("~s~s n=~s%\ttime=~s%~n",
       [Prefix, left(Tag), percent(NC + NA, NT), percent(NM, Micros)]),
    display_calc_stats(Prefix, Rest, NT, Micros);   
display_calc_stats(_, [], _, _) ->
    ok.

display_type_stats(Prefix, [{Tag, 0, 0, 0, 0} | Rest], NT, Micros) ->
    display_type_stats(Prefix, Rest, NT, Micros);   
display_type_stats(Prefix, [{Tag, NC, NA, NB, NM} | Rest], NT, Micros) ->
    ?d("~s~s n=~s%\ttime=~s%\tavg micros=~p~n",
       [
	Prefix,
	left(Tag),
	percent(NC + NA, NT),
	percent(NM, Micros),
	catch (NM div (NC + NA))
       ]),
    case NA /= 0 of
	true  -> ?d("~s    ~s% aborted~n", [Prefix, percent(NA, NC + NA)]);
	false -> ignore
    end,
    case NB /= 0 of
	true  -> ?d("~s    ~s% branches executed~n", [Prefix, percent(NB, NC + NA)]);
	false -> ignore
    end,
    display_type_stats(Prefix, Rest, NT, Micros);   
display_type_stats(_, [], _, _) ->
    ok.

left(Term) ->
    string:left(lists:flatten(io_lib:format("~p", [Term])), 27, $.).

percent(Part, 0)     -> "infinity";
percent(Part, Total) -> io_lib:format("~8.4f", [(Part * 100) / Total]).

calc_stats_per_tag([], Pos, Acc) ->
    lists:sort(Acc);
calc_stats_per_tag([Tuple | _] = FlatStats, Pos, Acc) when size(Tuple) == 5 ->
    Tag = element(Pos, Tuple),
    do_calc_stats_per_tag(FlatStats, Pos, {Tag, 0, 0, 0, 0}, Acc).

do_calc_stats_per_tag([Tuple | Rest], Pos, {Tag, NC, NA, NB, NM}, Acc)
  when element(Pos, Tuple) == Tag ->
    Val = element(5, Tuple),
    case element(2, Tuple) of
        n_commits ->
            do_calc_stats_per_tag(Rest, Pos, {Tag, NC + Val, NA, NB, NM}, Acc);
        n_aborts ->
            do_calc_stats_per_tag(Rest, Pos, {Tag, NC, NA + Val, NB, NM}, Acc);
        n_branches_executed ->
            do_calc_stats_per_tag(Rest, Pos, {Tag, NC, NA, NB + Val, NM}, Acc);
        n_micros ->
            do_calc_stats_per_tag(Rest, Pos, {Tag, NC, NA, NB, NM + Val}, Acc)
    end;
do_calc_stats_per_tag(GenStats, Pos, CalcStats, Acc) ->
    calc_stats_per_tag(GenStats, Pos, [CalcStats | Acc]).

display_trans_stats(Prefix, [{Tag, 0, 0, 0, 0} | Rest], NT, Micros, NG) ->
    display_trans_stats(Prefix, Rest, NT, Micros, NG);   
display_trans_stats(Prefix, [{Tag, NC, NA, NB, NM} | Rest], NT, Micros, NG) ->
    Common =
	fun(Name) ->
           Sec = NM / (1000000 * NG),
	   io:format("  ~s: ~p (~p%) Time: ~p sec TPS = ~p~n",
		     [Name,
		      NC + NA,
		      round(((NC + NA) * 100) / NT),
		      round(Sec),
		      round((NC + NA) / Sec)])
        end,
    Branch = 
	fun() -> 
            io:format("      Branches Executed: ~p (~p%)~n",
		      [NB, round((NB * 100) / (NC + NA))])
	end,
    Rollback =
	fun() -> 	
            io:format("      Rollback Executed: ~p (~p%)~n",
		      [NA, round((NA * 100) / (NC + NA))])
	end,
    case Tag of
	t1 ->
	    Common("T1");
	t2 ->
	    Common("T2");
	t3 ->
	    Common("T3"),
	    Branch();
	t4 ->
	    Common("T4"),
	    Branch(),
	    Rollback();
	t5 ->
	    Common("T5"),
	    Branch(),
	    Rollback();
	_ ->
	    Common(io_lib:format("~p", [Tag]))
    end,
    display_trans_stats(Prefix, Rest, NT, Micros, NG);   
display_trans_stats(_, [], _, _, _) ->
    ok.

