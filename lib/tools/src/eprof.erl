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
%% Purpose: Profile a system in order to figure out where the 
%% time goes.
%%

-module(eprof).
-behaviour(gen_server).

-export([start/0, stop/0, dump/0, total_analyse/0,
	 start_profiling/1, profile/4, profile/1,
	 stop_profiling/0, analyse/0, log/1]).

%% Internal exports 
-export([init/1,
	 call/4,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 echo/0]).

-import(lists, [flatten/1,reverse/1,keysort/2, member/2,keysearch/3]).


-record(state, {table = notable, 
		proc = noproc, 
		profiling = false, 
		pfunc = nofunc,
		ptime = 0,
		acktime = 0,
		pop = running,
		rootset = []}).

%%%%%%%%%%%%%%

start() -> gen_server:start({local, eprof}, eprof, [], []).
stop()  -> gen_server:call(eprof, stop, infinity).


profile(Pids,M,F,A) ->
    start(),
    gen_server:call(eprof, {profile,Pids, M,F,A},infinity).

dump() -> 
    gen_server:call(eprof, dump, infinity).

analyse() ->
    gen_server:call(eprof, analyse, infinity).

log(File) ->
    gen_server:call(eprof, {logfile, File}, infinity).

total_analyse() ->
    gen_server:call(eprof, total_analyse, infinity).

start_profiling(Rootset) ->
    start(),
    gen_server:call(eprof, {profile, Rootset}, infinity).

stop_profiling() ->
    gen_server:call(eprof, stop_profiling, infinity).

profile(Rs) ->
    start_profiling(Rs).

%%%%%%%%%%%%%%%%

init(_) ->
    process_flag(trap_exit, true),
    process_flag(priority, max), 
    put(oct, onecalltime()),
    put(sched_time, sched_time()),
    {ok, #state{}}.

subtr({X1,Y1,Z1}, {X1,Y1,Z2}) ->
    Z1 - Z2;
subtr({X1,Y1,Z1}, {X1,Y2,Z2}) ->
    ((Y1 * 1000000) + Z1) - ((Y2 * 1000000) + Z2).

collect_trace_messages() ->
    receive
	X when tuple(X), element(1,X) == trace_ts ->
	    [X | collect_trace_messages()];
	X when tuple(X), element(1,X) == trace ->
	    [X | collect_trace_messages()]
    after 0 -> 
	    []
    end.

into_tab(Tab, Pfunc, Pop, AckTime) ->
    case ets:lookup(Tab, Pfunc) of
	[] when Pop == call ->
	    ets:insert(Tab, {Pfunc,AckTime,1});
	[] -> %% Pop == return | running
	    ets:insert(Tab, {Pfunc,AckTime,0});
	[{_,Ack0, Calls}] when Pop == call ->
	    ets:insert(Tab, {Pfunc,AckTime + Ack0, Calls+1});
	[{_,Ack0, Calls}]  ->
	    ets:insert(Tab, {Pfunc,AckTime + Ack0, Calls})
    end.

do_messages([{trace_ts, From, Op, Func, Time0}|Tail], Tab,Pf,Pt,Pop,Ct,Ept) ->
    Time = convert_time(Time0),
    VirtualTime = Time - Ept,
    DiffTime = VirtualTime - Pt,
    Ack = DiffTime  - Ct,
    into_tab(Tab, Pf, Pop, Ack),
    case Tail of
	[] ->
	    {{From,Func}, Op, VirtualTime};
	_ ->
	    do_messages(Tail,Tab,{From,Func},VirtualTime, Op, Ct,Ept)
    end.

%%%%%%%%%%%%%%%%%%

handle_cast(Req, S) -> {noreply, S}.

terminate(Reason, S) -> normal.

%%%%%%%%%%%%%%%%%%

handle_call({logfile, F}, _FromTag, Status) ->
    case file:open(F, write) of
	{ok, Fd} ->
	    case get(fd) of
		undefined -> ok;
		FdOld -> file:close(FdOld)
	    end,
	    put(fd, Fd),
	    {reply, ok, Status};
	{error, _} ->
	    {reply, error, Status}
    end;

handle_call({profile, Rootset}, {From, _Tag}, S) ->
    link(From),
    maybe_delete(S#state.table),
    io:format("eprof: Starting profiling ..... ~n",[]),
    load_check(),
    ptrac(S#state.rootset, false, all()),
    flush_receive(),
    Tab = ets:new(eprof, [set, public]),
    case ptrac(Rootset, true, all()) of
	false ->
	    {reply, error,  #state{}};
	true ->
	    erase(replyto),
	    {reply, profiling, #state{table = Tab,
				      proc = From,
				      profiling = true,
				      rootset = Rootset}}
    end;

handle_call(stop_profiling, _FromTag, S) when S#state.profiling == true ->
    ptrac(S#state.rootset, false, all()),
    io:format("eprof: Stop profiling~n",[]),
    ets:delete(S#state.table, nofunc),
    {reply, profiling_stopped, S#state{profiling = false}};

handle_call(stop_profiling, _FromTag, S) ->
    {reply, profiling_already_stopped, S};

handle_call({profile, Rootset, M, F, A}, FromTag, S) ->
    io:format("eprof: Starting profiling ..... ~n",[]),
    load_check(),
    maybe_delete(S#state.table),
    ptrac(S#state.rootset, false, all()),
    flush_receive(),
    put(replyto, FromTag),
    Tab = ets:new(eprof, [set, public]),
    P = spawn_link(eprof, call, [self(), M, F, A]),
    case ptrac([P|Rootset], true, all()) of
	true ->
	    {noreply, #state{table     = Tab, 
			     profiling = true,
			     rootset   = [P|Rootset]}};
	false ->
	    erase(replyto),
	    {reply, error, #state{}}
    end;

handle_call(dump, _FromTag, S) ->
    {reply, dump(S#state.table), S};

handle_call(analyse, _FromTag, S) ->
    {reply, analyse(S#state.table), S};

handle_call(total_analyse, _FromTag, S) ->
    {reply, total_analyse(S#state.table), S};

handle_call(stop, _FromTag, S) ->
    {stop, normal, stopped, S}.

%%%%%%%%%%%%%%%%%%%

handle_info({trace_ts, From, Op, Func, Time}, S) when S#state.profiling == true ->
    put(start, convert_time(erlang:now())),
    Tmsgs = [{trace_ts, From, Op, Func, Time} | collect_trace_messages()],
    Pfunc0 = S#state.pfunc,
    Ptime0 = S#state.ptime,
    Pop0 = S#state.pop,
    Ect0 = S#state.acktime,
    {Pfunc, Pop, Ptime} = 
	do_messages(Tmsgs, S#state.table, Pfunc0, Ptime0, Pop0, get(oct), Ect0),
    Ect = get(sched_time) + Ect0 + (convert_time(erlang:now()) - get(start)),
    S2 = S#state{pfunc = Pfunc, ptime = Ptime, pop=Pop, acktime= Ect},
    {noreply, S2};

handle_info({trace_ts, From, _, _, _}, S) when S#state.profiling == false ->
    ptrac([From], false, all()),
    {noreply, S};

handle_info({P, {answer, A}}, S) ->
    ptrac(S#state.rootset, false, all()),
    io:format("eprof: Stop profiling~n",[]),
    {From, Tag} = get(replyto),
    catch unlink(From),
    ets:delete(S#state.table,nofunc),
    gen_server:reply(erase(replyto), {ok, A}),
    {noreply, S#state{profiling = false,
		      rootset = []}};

handle_info({'EXIT', P, Reason}, S) when S#state.profiling == true,
                                         S#state.proc == P  ->
    maybe_delete(S#state.table),
    ptrac(S#state.rootset, false, all()),
    io:format("eprof: Fail profiling~n",[]),
    case erase(replyto) of
	undefined ->
	    {noreply, #state{}};
	FromTag ->
	    gen_server:reply(FromTag, {error, Reason}),
	    {noreply, #state{}}
    end;

handle_info({'EXIT', P, Reason}, S) ->
    {noreply, S}.

%%%%%%%%%%%%%%%%%%

call(Top, M, F, A) ->
    Top ! {self(), {answer, apply(M,F,A)}}.

ptrac([P|T], How, Flags) when pid(P) ->
    case dotrace(P, How, Flags) of
	true ->
	    ptrac(T, How, Flags);
	false when How == true ->
	    false;
	false ->
	    ptrac(T, How, Flags)
    end;

ptrac([P|T], How, Flags) when atom(P) ->
    case whereis(P) of
	undefined when How == true ->
	    false;
	undefined when How == false ->
	    ptrac(T, How, Flags);
	Pid ->
	    ptrac([Pid|T], How, Flags)
    end;

ptrac([H|T], How, Flags) ->
    io:format("** eprof bad process ~w~n",[H]),
    false;

ptrac([],_,_) -> true.

dotrace(P,How,What) ->
    case (catch erlang:trace(P, How, What)) of
	1 ->
	    true;
	Other when How == false ->
	    true;
	Other ->
	    io:format("** eprof: bad process ~w~n",[P]),
	    false
    end.

all() -> [old_call_trace, running, timestamp, set_on_spawn].

total_analyse(notable) -> 
    nothing_to_analyse;
total_analyse(T) ->
    Pcalls = reverse(keysort(2, replicas(ets:tab2list(T)))),
    Time = collect_times(Pcalls),
    format("FUNCTION~44s      TIME ~n", ["CALLS"]),   
    printit(Pcalls, Time).

analyse(notable) -> 
    nothing_to_analyse;
analyse(T) ->
    Pids = ordsets:list_to_set(flatten(ets:match(T, {{'$1','_'},'_', '_'}))),
    Times = sum(ets:match(T, {'_','$1', '_'})),
    format("FUNCTION~44s      TIME ~n", ["CALLS"]),     
    do_pids(Pids, T, 0, Times).

do_pids([Pid|Tail], T, AckTime, Total) ->
    Pcalls = 
     reverse(keysort(2, to_tups(ets:match(T, {{Pid,'$1'}, '$2','$3'})))),
    Time = collect_times(Pcalls),
    PercentTotal = 100 * (divide(Time, Total)),
    format("~n****** Process ~w    -- ~s % of profiled time *** ~n", 
	   [Pid, fpf(PercentTotal)]),
    printit(Pcalls, Time),
    do_pids(Tail, T, AckTime + Time, Total);
do_pids([], _, _, _) -> 
    ok.

printit([],_) -> ok;
printit([{{Mod,Fun,Arity}, Time, Calls} |Tail], ProcTime)  ->
    format("~s  ~s ~s % ~n", [ff(Mod,Fun,Arity), fint(Calls),
			      fpf(100*(divide(Time,ProcTime)))]),
    printit(Tail, ProcTime);
printit([{{_,{Mod,Fun,Arity}}, Time, Calls} |Tail], ProcTime)  ->
    format("~s  ~s ~s % ~n", [ff(Mod,Fun,Arity), fint(Calls),
			      fpf(100*(divide(Time,ProcTime)))]),
    printit(Tail, ProcTime); 
printit([_|T], Time) ->
    printit(T, Time).

ff(Mod,Fun,Arity) ->
    pad(flatten(io_lib:format("~w:~w/~w", [Mod,Fun, Arity])),45).

pad(Str, Len) -> 
    Strlen = length(Str),
    if
	Strlen > Len -> strip_tail(Str, 45);
	true -> lists:append(Str, mklist(Len-Strlen))
    end.

strip_tail([H|T], 0) ->[];
strip_tail([H|T], I) -> [H|strip_tail(T, I-1)];
strip_tail([], I) -> [].

fpf(F) -> strip_tail(flatten(io_lib:format("~w", [round(F)])), 5).
fint(Int) -> pad(flatten(io_lib:format("~w",[Int])), 10).

mklist(0) -> [];
mklist(I) -> [$ |mklist(I-1)].

to_tups(L) -> lists:map(fun(List) -> erlang:list_to_tuple(List) end, L).

divide(X,Y) -> X / Y.

collect_times([]) -> 0;
collect_times([Tup|Tail]) -> element(2, Tup) + collect_times(Tail).

dump(T) ->
    L = ets:tab2list(T),
    format(L).

format([H|T]) -> 
    format("~p~n", [H]), format(T);
format([]) -> ok.

format(F, A) ->
    io:format(F,A),
    case get(fd) of
	undefined -> ok;
	Fd -> io:format(Fd, F,A)
    end.

maybe_delete({T,Ref}) when reference(Ref) ->
    ets:delete({T, Ref});
maybe_delete(_) -> ok.

onecalltime() -> hd(lists:sort([oct(), oct(), oct()])).

oct() ->
    garbage_collect(),  %% ehhh
    N = erlang:now(),
    call_loop(100,time),
    Time1 = subtr(erlang:now(), N) div 100,

    garbage_collect(),  %% ehhh
    N2 = erlang:now(),
    call_loop(100,notime),
    Time2 = subtr(erlang:now(), N2) div 100,
    
    (Time1 - Time2) div 2.

sched_time() ->
    P = spawn(eprof, echo, []),
    X = erlang:now(),
    P ! self(),
    receive P -> ok end,
    subtr(erlang:now(), X).

echo() ->
    receive P -> P ! self() end.

call_loop(0,_) -> ok;
call_loop(I,time) ->
    erlang:now(), call_loop(I-1,time);
call_loop(I, notime) ->
    call_loop(I-1, notime).

convert_time({Msecs,Secs,Mysecs}) ->
    (1000000000000 * Msecs) + (1000000 * Secs) + Mysecs.

sum([[H]|T]) -> H + sum(T);
sum([]) -> 0.

replicas(L) ->
    replicas(L, []).

replicas([{{Pid, {Mod,Fun,Arity}}, Ack,Calls} |Tail], Result) ->
    case search({Mod,Fun,Arity},Result) of
	false ->
	    replicas(Tail, [{{Pid, {Mod,Fun,Arity}}, Ack,Calls} |Result]);
	{Ack2, Calls2} ->
	    Result2 = del({Mod,Fun,Arity}, Result),
	    replicas(Tail, [{{Pid, {Mod,Fun,Arity}}, 
			     Ack+Ack2,Calls+Calls2} |Result2])
    end;

replicas([_|T], Ack) ->  %% Whimpy
    replicas(T, Ack);

replicas([], Res) -> Res.

search(Key, [{{_,Key}, Ack, Calls}|_]) -> 
    {Ack, Calls};
search(Key, [_|T]) -> 
    search(Key, T);
search(Key, []) -> false.

del(Key, [{{_,Key}, Ack, Calls}|T]) ->
    T;
del(Key, [H | Tail]) ->
    [H|del(Key, Tail)];
del(Key, []) -> [].

flush_receive() ->
    receive 
	{trace_ts, From, _, _, _} when pid(From) ->
	    ptrac([From], false, all()),
	    flush_receive();
	_ ->
	    flush_receive()
    after 0 ->
	    ok
    end.

load_check() ->
    load_check(code:all_loaded()).
load_check([{Mod, File} |Tail]) ->
    load_check_mod(Mod, keysearch(options, 1,
				  apply(Mod, module_info,[compile]))),
    load_check(Tail);
load_check([]) -> done.

load_check_mod(Mod, {value, {_, Opts}}) ->
    case member(trace, Opts) of
	true -> true;
	false -> 
	    io:format("** eprof: Warning module ~w not trace compiled ~n", 
		      [Mod])
    end;
load_check_mod(Mod, _) ->
    io:format("** eprof: No compile_opts found in ~w:module_info()~n", [Mod]).

