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
%%----------------------------------------------------------------------
%% Purpose: Collect trace events and provide a backing storage
%%          appropriate for iteration 
%%----------------------------------------------------------------------

-module(et_collector).

-behaviour(gen_server).

%% External exports
-export([
	 start_link/1, 
	 stop/1,

	 report/2, 
	 report/6, 
	 report/7, 

	 iterate/3,
	 iterate/5,

         start_trace_client/3, 
	 start_trace_port/1, 
	 %% load_event_file/2, 
	 save_event_file/3,
	 clear_table/1,

	 get_global_pid/0, 
	 get_table_handle/1,
	 change_pattern/2,
	 make_key/2,

	 dict_insert/3, 
	 dict_delete/2, 
	 dict_lookup/2, 
	 dict_match/2,
	 multicast/2
	]).

%% gen_server callbacks
-export([init/1,terminate/2, code_change/3,
	 handle_call/3, handle_cast/2, handle_info/2
	]).

-include("et.hrl").

-record(state, {parent_pid,
		event_tab,
		dict_tab,
		event_order,
		subscribers,
		file, 
		trace_pattern,
		trace_port,
		trace_max_queue,
		trace_nodes,
		trace_global}).

-record(file, {name, desc, event_opt, file_opt, table_opt}).

-record(table_handle, {collector_pid, event_tab, event_order, filter}).

-record(trace_ts, {trace_ts, event_ts}).
-record(event_ts, {event_ts, trace_ts}).

%%%----------------------------------------------------------------------
%%% Client side
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Start a collector process
%%
%% The collector collects trace events and keeps them ordered by their
%% timestamp. The timestamp may either reflect the time when the
%% actual trace data was generated (trace_ts) or when the trace data
%% was transformed into an event record (event_ts).
%% 
%% Events are reported to the collector directly with the report
%% function or indirectly via one or more trace clients. All reported
%% events are first filtered thru the collector filter before they are
%% stored by the collector. By replacing the default collector filter
%% with a customized dito it is possible to allow any trace data as
%% input. The collector filter is a dictionary entry with the
%% predefined key {filter, collectorr} and the value is a fun of
%% arity 1. See et_selector:make_event/1 for interface details.
%%
%% The collector has a built-in dictionary service. Any term may be
%% stored as value in the dictionary and bound to a unique key. When
%% new values are inserted with an existing key, the new values will
%% overwrite the existing ones. Processes may subscribe on dictionary
%% updates by using {subscriber, pid()} as dictionary key. All
%% dictionary updates will be propagated to the subscriber processes
%% matching the pattern {{subscriber, '_'}, '_'} where the first '_'
%% is interpreted as a pid().
%%
%% In global trace mode, the collector will automatically start
%% tracing on all connected Erlang nodes. When a node connects, a port
%% tracer will be started on that node and a corresponding trace
%% client on the local node. By default the global trace pattern is 'max'.
%% 
%% Options = [option()]
%% 
%% option() =
%% 
%%   {parent_pid,      pid()}        |
%%   {event_order,     order()}      |
%%   {dict_insert,     dict_key(), dict_val()} |
%%   {dict_delete,     dict_key()}        |
%%   {trace_client,    client()}     |
%%   {trace_global,    boolean()}    | 
%%   {trace_pattern,   pattern()}    |
%%   {trace_port,      integer()}    | 
%%   {trace_max_queue, integer()}
%%   
%% order()    = 'trace_ts' | 'event_ts'
%% pattern()  = 'min' | 'max' | dbg_match_spec()
%% client()   = {event_file, FileName} |
%%              {dbg_trace_type(), dbg_trace_parameters()}
%% dict_key() = term()
%% dict_ val() = term()
%% 
%% Returns {ok, Pid} | {error, Reason}
%%----------------------------------------------------------------------

start_link(Options) ->
    Dict = [{dict_insert, {filter, collector}, fun(E) -> et:make_event(E) end}],
    Default = #state{parent_pid      = self(),
                     event_order     = trace_ts,
                     subscribers     = [],
		     trace_global    = false,
                     trace_pattern   = max,
		     trace_nodes     = [],
		     trace_port      = 4711,
		     trace_max_queue = 50},
    case parse_opt(Options, Default, Dict, []) of
        {ok, S, Dict2, Clients} when S#state.trace_global == false ->
            case gen_server:start_link(?MODULE, [S, Dict2], []) of
		{ok, Pid} when S#state.parent_pid /= self() ->
		    unlink(Pid),
		    start_clients(Pid, Clients);
		{ok,Pid} ->
		    start_clients(Pid, Clients);
		{error, Reason} ->
		    {error, Reason}
	    end;
        {ok, S, Dict2, Clients} when S#state.trace_global == true ->
            case gen_server:start_link({global, ?MODULE}, ?MODULE, [S, Dict2], []) of
		{ok, Pid} when S#state.parent_pid /= self() ->
		    unlink(Pid),
		    start_clients(Pid, Clients);
		{ok,Pid} ->
		    start_clients(Pid, Clients);
		{error, Reason} ->
		    {error, Reason}
	    end;
        {error, Reason} ->
            {error, Reason}
    end.

parse_opt([], S, Dict, Clients) ->
    {ok, S, Dict, Clients};
parse_opt([H | T], S, Dict, Clients) ->
    case H of
        {parent_pid, Parent} when pid(Parent) ->
            parse_opt(T, S#state{parent_pid = Parent}, Dict, Clients);
        {event_order, Order} when Order == trace_ts ->
            parse_opt(T, S#state{event_order = Order}, Dict, Clients);
        {event_order, Order}  when Order == event_ts ->
            parse_opt(T, S#state{event_order = Order}, Dict, Clients);
        {dict_insert, Key, Val} ->
            parse_opt(T, S, Dict ++ [H], Clients);
        {dict_delete, Key} ->
            parse_opt(T, S, Dict ++ [H], Clients);
        {trace_client, Client = {_, _}} ->
            parse_opt(T, S, Dict, Clients ++ [Client]);
        {trace_global, Bool} when Bool == false ->
            parse_opt(T, S#state{trace_global = Bool}, Dict, Clients);
        {trace_global, Bool} when Bool == true ->
            parse_opt(T, S#state{trace_global = Bool}, Dict, Clients);
        {trace_pattern, Pattern} ->
            parse_opt(T, S#state{trace_pattern = Pattern}, Dict, Clients);
        {trace_port, Port} when integer(Port) ->
            parse_opt(T, S#state{trace_port = Port}, Dict, Clients);
        {trace_max_queue, MaxQueue} when integer(MaxQueue) ->
            parse_opt(T, S#state{trace_port = MaxQueue}, Dict, Clients);
        Bad ->
            {error, {bad_option, Bad}}
    end;
parse_opt(BadList, S, Dict, Clients) ->
    {error, {bad_option_list, BadList}}.

start_clients(CollectorPid, [{Type, Parameters} | T]) ->
    start_trace_client(CollectorPid, Type, Parameters),
    start_clients(CollectorPid, T);
start_clients(CollectorPid, []) ->
    {ok, CollectorPid}.

%%----------------------------------------------------------------------
%% Stop a collector
%% 
%% Returns: ok
%%----------------------------------------------------------------------

stop(CollectorPid) ->
    call(CollectorPid, stop).

%%----------------------------------------------------------------------
%% Save the event table to a file
%% 
%% Returns ok | {error, Reason}
%%----------------------------------------------------------------------

save_event_file(CollectorPid, FileName, Options) ->
    call(CollectorPid, {save_event_file, FileName, Options}).

%% Load the event table from a file
%% Returns {ok, BadBytes} | exit(Reason)

load_event_file(CollectorPid, FileName) ->
    Fd = make_ref(),
    Args = [{file, FileName}, {name, Fd}, {repair, true}, {mode, read_only}],
    Fun = fun(Event, {ok, TH}) -> report(TH, Event) end,
    case disk_log:open(Args) of
	{ok, _} ->
	    do_load_event_file(Fun, Fd, start, {ok, CollectorPid}, FileName, 0);
	{repaired, _, _, BadBytes} ->
	    do_load_event_file(Fun, Fd, start, {ok, CollectorPid}, FileName, BadBytes);
	{error, Reason} ->
	    exit({disk_log_open, FileName, Reason})
    end.

do_load_event_file(Fun, Fd, Cont, Acc, FileName, BadBytes) ->
    case disk_log:chunk(Fd, Cont) of
        eof ->
            {ok, BadBytes};
        {error, Reason} ->
            exit({bad_disk_log_chunk, FileName, Reason});
        {Cont2, Events} ->
            Acc2 = lists:foldl(Fun, Acc, Events),
            do_load_event_file(Fun, Fd, Cont2, Acc2, FileName, BadBytes);
        {Cont2, Events, More} ->
            Acc2 = lists:foldl(Fun, Acc, Events),
            do_load_event_file(Fun, Fd, Cont2, Acc2, FileName, BadBytes + More)
    end.

%%----------------------------------------------------------------------
%% Collects an event
%%
%% All events are filtered thru the collector filter, which
%% optionally may transform or discard the event.
%% 
%% Returns {ok, NewReportHandle} | exit(Reason)
%%----------------------------------------------------------------------

report(CollectorPid, TraceOrEvent) when pid(CollectorPid) ->
    case get_table_handle(CollectorPid) of
	{ok, TH} when record(TH, table_handle) ->
	    report(TH, TraceOrEvent);
	{error, Reason} ->
	    exit(Reason)
    end;
report(TH, TraceOrEvent) when record(TH, table_handle) ->
    Fun = TH#table_handle.filter,
    case Fun(TraceOrEvent) of
	false ->
	    {ok, TH};
	true when record(TraceOrEvent, event) ->
	    Key = make_key(TH, TraceOrEvent),
	    case catch ets:insert(TH#table_handle.event_tab, {Key, TraceOrEvent}) of
		true ->
		    {ok, TH};
		{'EXIT', Reason} ->
		    %% Refresh the report handle and try again
		    report(TH#table_handle.collector_pid, TraceOrEvent)
	    end;
	{true, Event} when record(Event, event) ->
	    Key = make_key(TH, Event),
	    case catch ets:insert(TH#table_handle.event_tab, {Key, Event}) of
		true ->
		    {ok, TH};
		{'EXIT', Reason} ->
		    %% Refresh the report handle and try again
		    report(TH#table_handle.collector_pid, TraceOrEvent)
	    end;
	BadEvent ->
	    TS = erlang:now(),
	    Contents = [{trace, TraceOrEvent}, {reason, BadEvent}, {filter, Fun}],
	    Event = #event{detail_level = 0,
			   trace_ts     = TS,
			   event_ts     = TS,
			   from         = bad_filter,
			   to           = bad_filter,
			   label        = bad_filter,
			   contents     = Contents},
	    Key = make_key(TH, Event),
	    case catch ets:insert(TH#table_handle.event_tab, {Key, Event}) of
		true ->
		    {ok, TH};
		{'EXIT', Reason} ->
		    %% Refresh the report handle and try again
		    report(TH#table_handle.collector_pid, TraceOrEvent)
	    end
    end;
report(TH, end_of_trace) when record(TH, table_handle) ->
    {ok, TH};
report(_, Bad) ->
    exit({bad_event, Bad}).

report(CollectorPid, DetailLevel, From, To, Label, Contents) ->
    report(CollectorPid, DetailLevel, erlang:now(), From, To, Label, Contents).

report(CollectorPid, DetailLevel, TS, From, To, Label, Contents)
  when integer(DetailLevel), DetailLevel >= 0, DetailLevel =< 100, list(Contents) ->
    E = #event{detail_level = DetailLevel,
	       trace_ts     = TS,
	       event_ts     = TS,
	       from         = From,
	       to           = To, 
	       label        = Label, 
	       contents     = Contents},
    report(CollectorPid, E).

%%----------------------------------------------------------------------
%% Makes a key out of an event record or an old key
%%----------------------------------------------------------------------

make_key(TH, Stuff) when record(TH, table_handle) ->
    make_key(TH#table_handle.event_order, Stuff);
make_key(trace_ts, Stuff) ->
    if
	record(Stuff, event) ->
	    #event{trace_ts = R, event_ts = P} = Stuff,
	    #trace_ts{trace_ts = R, event_ts = P};
	record(Stuff, trace_ts) ->
	    Stuff;
	record(Stuff, event_ts) ->
	    #event_ts{trace_ts = R, event_ts = P} = Stuff,
	    #trace_ts{trace_ts = R, event_ts = P}
    end;
make_key(event_ts, Stuff) ->
    if
	record(Stuff, event) ->
	    #event{trace_ts = R, event_ts = P} = Stuff,
	    #event_ts{trace_ts = R, event_ts = P};
	record(Stuff, event_ts) ->
	    Stuff;
	record(Stuff, trace_ts) ->
	    #trace_ts{trace_ts = R, event_ts = P} = Stuff,
	    #event_ts{trace_ts = R, event_ts = P}
    end.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------

get_table_handle(CollectorPid) when pid(CollectorPid) ->
    call(CollectorPid, get_table_handle).

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------

get_global_pid() ->
    case global:whereis_name(?MODULE) of
	CollectorPid when pid(CollectorPid) ->
            CollectorPid;
        undefined ->
            exit(global_collector_not_started)
    end.

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------
change_pattern(CollectorPid, RawPattern) ->
    Pattern = et:make_pattern(RawPattern),
    call(CollectorPid, {change_pattern, Pattern}).

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------

dict_insert(CollectorPid, Key, Val) ->
    call(CollectorPid, {dict_insert, Key, Val}).

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------

dict_lookup(CollectorPid, Key) ->
    call(CollectorPid, {dict_lookup, Key}).

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------

dict_delete(CollectorPid, Key) ->
    call(CollectorPid, {dict_delete, Key}).

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------

dict_match(CollectorPid, Pattern)  ->
    call(CollectorPid, {dict_match, Pattern}).

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------

multicast(CollectorPid, Msg = {subscriber, _}) ->
    exit({badarg, Msg});
multicast(CollectorPid, Msg) ->
    call(CollectorPid, {multicast, Msg}).

%%----------------------------------------------------------------------
%% Load raw Erlang trace from a file, port or process.
%% 
%% Type       = dbg_trace_client_type()
%% Parameters = dbg_trace_client_parameters()
%% Pid        = dbg_trace_client_pid()
%%
%% Returns: file_loaded | {trace_client_pid, Pid} | exit(Reason)
%%----------------------------------------------------------------------

start_trace_client(CollectorPid, Type, FileName) when Type == event_file ->
    load_event_file(CollectorPid, FileName);
start_trace_client(CollectorPid, Type, FileName) when Type == file -> 
   WaitFor = {make_ref(), end_of_trace},
    EventFun = fun(E, {ReplyTo, {ok, TH}}) -> {ReplyTo, report(TH, E)} end,
    EndFun = fun({ReplyTo, {ok, TH}}) -> ReplyTo ! WaitFor, ReplyTo  end,
    Spec = trace_spec_wrapper(EventFun, EndFun, {self(), {ok, CollectorPid}}),
    Pid = dbg:trace_client(Type, FileName, Spec),
    receive
	WaitFor -> 
	    file_loaded;
	{'EXIT', Pid, Reason} ->
	    exit(Reason)
    end;
start_trace_client(CollectorPid, Type, Parameters) ->
    EventFun = fun(Event, {ok, TH}) -> report(TH, Event) end,
    EndFun   = fun(Acc) -> Acc end,
    Spec = trace_spec_wrapper(EventFun, EndFun, {ok, CollectorPid}),
    {trace_client_pid, dbg:trace_client(Type, Parameters, Spec)}.
    

trace_spec_wrapper(EventFun, EndFun, EventInitialAcc)
  when function(EventFun), function(EndFun) ->
    {fun(Trace, Acc) -> 
	     case Trace == end_of_trace of
		 true  -> EndFun(Acc);
		 false -> EventFun(Trace,  Acc)
	     end
     end,
     EventInitialAcc}.

start_trace_port(Parameters) ->
    dbg:tracer(port, dbg:trace_port(ip, Parameters)).

%%----------------------------------------------------------------------
%%----------------------------------------------------------------------

iterate(Handle, Prev, Max) ->
    iterate(Handle, Prev, Max, undefined, Prev).

iterate(_, _, Max, _, Acc) when Max == 0 ->
    Acc;
iterate(CollectorPid, Prev, Max, Fun, Acc) when pid(CollectorPid) ->
    case get_table_handle(CollectorPid) of
	{ok, TH} when record(TH, table_handle) ->
	    iterate(TH, Prev, Max, Fun, Acc);
	{error, Reason} ->
    	    exit(Reason)
    end;
iterate(TH, Prev, Max, Fun, Acc) when record(TH, table_handle) ->
    if
	Max == infinity ->
	    next_iterate(TH, Prev, Max, Fun, Acc);
	integer(Max), Max > 0 ->
	    next_iterate(TH, Prev, Max, Fun, Acc);
	integer(Max), Max < 0 ->
	    prev_iterate(TH, Prev, Max, Fun, Acc)
    end.    
    
next_iterate(TH, Prev = first, Max, Fun, Acc) ->
    Tab = TH#table_handle.event_tab,
    case catch ets:first(Tab) of
	'$end_of_table' ->
	    Acc;
	{'EXIT', _} = Error ->
	    io:format("~p(~p): First ~p~n", [?MODULE, ?LINE, Error]),
	    iterate(TH#table_handle.collector_pid, Prev, Max, Fun, Acc);
	First ->
	    lookup_and_apply(TH, Prev, First, Max, -1, Fun, Acc)
    end;
next_iterate(TH, Prev = last, Max, Fun, Acc) ->
    Tab = TH#table_handle.event_tab,
    case catch ets:last(Tab) of
	'$end_of_table' ->
	    Acc;
	{'EXIT', _} = Error ->
	    io:format("~p(~p): Last ~p~n", [?MODULE, ?LINE, Error]),
	    iterate(TH#table_handle.collector_pid, Prev, Max, Fun, Acc);
	Last ->
	    lookup_and_apply(TH, Prev, Last, Max, -1, Fun, Acc)
    end;
next_iterate(TH, Prev, Max, Fun, Acc) ->
    Tab = TH#table_handle.event_tab,
    Key = make_key(TH, Prev),
    case catch ets:next(Tab, Key) of
	'$end_of_table' ->
	    Acc;
	{'EXIT', _} = Error ->
	    io:format("~p(~p): Next ~p -> ~p~n", [?MODULE, ?LINE, Key, Error]),
	    iterate(TH#table_handle.collector_pid, Prev, Max, Fun, Acc);
	Next ->
	    lookup_and_apply(TH, Prev, Next, Max, -1, Fun, Acc)
    end.

prev_iterate(TH, Prev = first, Max, Fun, Acc) ->
    Tab = TH#table_handle.event_tab,
    case catch ets:first(Tab) of
	'$end_of_table' ->
	    Acc;
	{'EXIT', _} = Error ->
	    io:format("~p(~p): First ~p~n", [?MODULE, ?LINE, Error]),
	    iterate(TH#table_handle.collector_pid, Prev, Max, Fun, Acc);
	First ->
	    lookup_and_apply(TH, Prev, First, Max, 1, Fun, Acc)
    end;
prev_iterate(TH, Prev = last, Max, Fun, Acc) ->
    Tab = TH#table_handle.event_tab,
    case catch ets:last(Tab) of
	'$end_of_table' ->
	    Acc;
	{'EXIT', _} = Error ->
	    io:format("~p(~p): Last ~p~n", [?MODULE, ?LINE, Error]),
	    iterate(TH#table_handle.collector_pid, Prev, Max, Fun, Acc);
	Last ->
	    lookup_and_apply(TH, Prev, Last, Max, 1, Fun, Acc)
    end;
prev_iterate(TH, Prev, Max, Fun, Acc) ->
    Tab = TH#table_handle.event_tab,
    Key = make_key(TH, Prev),
    case catch ets:prev(Tab, Key) of
	'$end_of_table' ->
	    Acc;
	{'EXIT', _} = Error ->
	    io:format("~p(~p): Prev ~p -> ~p~n", [?MODULE, ?LINE, Key, Error]),
	    iterate(TH#table_handle.collector_pid, Prev, Max, Fun, Acc);
	Next ->
	    lookup_and_apply(TH, Prev, Next, Max, 1, Fun, Acc)
    end.

lookup_and_apply(TH, Prev, Next, Max, Incr, Fun, Acc) when Fun == undefined ->
    Max2 = incr(Max, Incr),
    iterate(TH, Next, Max2, Fun, Next);   
lookup_and_apply(TH, Prev, Next, Max, Incr, Fun, Acc) ->
    Tab = TH#table_handle.event_tab,
    case catch ets:lookup_element(Tab, Next, 2) of
	{'EXIT', _} ->
	    iterate(TH#table_handle.collector_pid, Prev, Max, Fun, Acc);
	E when record(E, event) ->
	    Acc2 = Fun(E, Acc),
	    Max2 = incr(Max, Incr),
	    iterate(TH, Next, Max2, Fun, Acc2)
    end.

incr(infinity, Incr) -> infinity;
incr(Int, Incr)      -> Int + Incr.

%%----------------------------------------------------------------------
%% Clear the event table
%% 
%% Returns ok
%%----------------------------------------------------------------------

clear_table(CollectorPid) when pid(CollectorPid) ->
    call(CollectorPid, clear_table);
clear_table(TH) when record(TH, table_handle) ->
    clear_table(TH#table_handle.collector_pid).

call(CollectorPid, Request) ->
    gen_server:call(CollectorPid, Request, infinity).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%%----------------------------------------------------------------------

init([InitialS, Dict]) ->
    process_flag(trap_exit, true),
    Funs = [fun init_tables/1,
	    fun init_pattern/1, 
	    fun init_global/1,
	    fun(S) -> lists:foldl(fun do_dict_insert/2, S, Dict) end],
    {ok, lists:foldl(fun(F, S) -> F(S) end, InitialS, Funs)}.

init_tables(S) -> 
    EventTab = ets:new(et_events, [ordered_set, {keypos, 1}, public]),
    DictTab  = ets:new(et_dict,   [ordered_set, {keypos, 1}, public]),
    S#state{event_tab = EventTab, dict_tab = DictTab}.

init_pattern(S) ->
    Pattern = et:make_pattern(S#state.trace_pattern),
    S#state{trace_pattern = Pattern}.

init_global(S) -> 
    case S#state.trace_global of
	true ->
	    EventFun = fun(Event, {ok, TH}) -> report(TH, Event) end,
	    EndFun = fun(Acc) -> Acc end,
	    Spec = trace_spec_wrapper(EventFun, EndFun, {ok, self()}),
	    dbg:tracer(process, Spec),
	    et:change_pattern(S#state.trace_pattern),
	    net_kernel:monitor_nodes(true),
	    lists:foreach(fun(N) -> self() ! {nodeup, N} end, nodes()),
	    S#state{trace_nodes = [node()]};
	false ->
	    S
    end.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_call({multicast, Msg}, From, S) ->
    do_multicast(S#state.subscribers, Msg),
    {reply, ok, S};

handle_call(Msg = {dict_insert, Key, Val}, From, S) ->
    S2 = do_dict_insert(Msg, S),
    {reply, ok, S2};

handle_call(Msg = {dict_delete, {subscriber, Pid}}, From, S) ->
    S2 = do_dict_delete(Msg, S),
    {reply, ok, S2};

handle_call({dict_lookup, Key}, From, S) ->
    Reply = ets:lookup(S#state.dict_tab, Key),
    {reply, Reply, S};

handle_call({dict_match, Pattern}, From, S) ->
    case catch ets:match_object(S#state.dict_tab, Pattern) of
	{'EXIT', Reason} -> {reply, [], S};
	Matching         -> {reply, Matching, S}
    end;

handle_call(get_table_handle, From, S) ->
    [{_, TableFilter}] = ets:lookup(S#state.dict_tab, {filter, collector}),
    TH = #table_handle{collector_pid = self(),
		       event_tab     = S#state.event_tab,
		       event_order   = S#state.event_order,
		       filter        = TableFilter},
    {reply, {ok, TH}, S};

handle_call(close, From, S) ->
    case S#state.file of
        undefined ->
            {reply, {error, file_not_open}, S};
        F ->
            Reply = disk_log:close(F#file.desc),
            S2 = S#state{file = undefined},
            {reply, Reply, S2}
    end;
handle_call({save_event_file, FileName, Options}, From, S) ->
    Default = #file{name      = FileName,
                    event_opt = existing,
                    file_opt  = write,
                    table_opt = keep},
    case parse_file_options(Default, Options) of
        {ok, F} when record(F, file) ->
            case file_open(F) of
                {ok, Fd} ->
                    F2 = F#file{desc = Fd},
                    {Reply2, S3} = 
                        case F2#file.event_opt of
                            %% new ->
                            %% 	   Reply = ok,
                            %% 	   S2 = S#state{file = F},
                            %% 	   {Reply, S2};
                            %%
    			    %% insert() ->
                            %% 	 case S2#state.file of    
 			    %% 	     undefined ->
 			    %% 		 ignore;
 			    %% 	     F  ->
 			    %% 		 Fd = F#file.desc,
 			    %% 		 ok = disk_log:log(Fd, Event)
    			    %% 	 end.
                            existing ->
				Fun = fun({_, E}, A) -> ok = disk_log:log(Fd, E), A end,
				Tab = S#state.event_tab,
				Reply = tab_iterate(Fun, Tab, ets:first(Tab), ok),
                                disk_log:close(Fd),
                                {Reply, S}
                            %% all ->
                            %% 	   Reply = tab_iterate(WriteFun, Tab, ok),
                            %% 	   S2 = S#state{file = F},
                            %% 	   {Reply, S2}
                        end,
                    case F2#file.table_opt of
                        keep ->
			    {reply, Reply2, S3};
                        clear ->
			    S4 = do_clear_table(S3),
			    {reply, Reply2, S4}
                    end;
                {error, Reason} ->
                    {reply, {error, {file_open, Reason}}, S}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, S}
    end;

handle_call({change_pattern, Pattern}, From, S) ->
    Ns = S#state.trace_nodes,
    rpc:multicall(Ns, ?MODULE, change_pattern, [Pattern]),
    Reply = {old_pattern, S#state.trace_pattern},
    S2 = S#state{trace_pattern = Pattern},
    {reply, Reply, S2};

handle_call(clear_table, From, S) ->
    S2 = do_clear_table(S),
    {reply, ok, S2};

handle_call(stop, From, S) ->
    unlink(S#state.parent_pid),
    do_multicast(S#state.subscribers, close),
    case S#state.trace_global of
	true  -> rpc:multicall(S#state.trace_nodes, dbg, stop, []);
	false -> ignore
    end,
    {stop, shutdown, S};
handle_call(Request, From, S) ->
    ok = error_logger:format("~p(~p): handle_call(~p, ~p, ~p)~n",
                             [?MODULE, self(), Request, From, S]),
    {reply, {error, {bad_request, Request}}, S}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_cast(Msg, S) ->
    ok = error_logger:format("~p(~p): handle_cast(~p, ~p)~n",
                             [?MODULE, self(), Msg, S]),
    {noreply, S}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_info({nodeup, Node}, S) ->
    Port     = S#state.trace_port,
    MaxQueue = S#state.trace_max_queue,
    case rpc:call(Node, ?MODULE, start_trace_port, [{Port, MaxQueue}]) of
        {ok, _} ->
	    listen_on_trace_port(Node, Port, S);
        {error, Reason} when Reason == already_started->
            ok = error_logger:format("~p(~p): producer ignored(~p:~p):~n    ~p~n",
                                     [?MODULE, self(), Node, Port, Reason]),
            S2 = S#state{trace_port = Port + 1},
            {noreply, S2};
        {badrpc, Reason} ->
            ok = error_logger:format("~p(~p): producer ignored(~p:~p):~n    ~p~n",
                                     [?MODULE, self(), Node, Port, Reason]),
            S2 = S#state{trace_port = Port + 1},
            {noreply, S2};
        {error, Reason} ->
            self() ! {nodeup, Node},
            ok = error_logger:format("~p(~p): producer retry(~p:~p):~n     ~p~n",
                                     [?MODULE, self(), Node, Port, Reason]),
            S2 = S#state{trace_port = Port + 1},
            {noreply, S2}
    end;

handle_info({nodedown, Node}, S) ->
    {noreply, S#state{trace_nodes = S#state.trace_nodes -- [Node]}};

handle_info({'EXIT', Pid, Reason}, S) when Pid == S#state.parent_pid ->
    {stop, Reason, S};
handle_info(Info = {'EXIT', Pid, Reason}, S) ->
    OldSubscribers = S#state.subscribers,
    case lists:member(Pid, OldSubscribers) of
	true  ->
	    S2 = do_dict_delete({dict_delete, {subscriber, Pid}}, S),
	    {noreply, S2};
	false ->
	    ok = error_logger:format("~p(~p): handle_info(~p, ~p)~n",
				     [?MODULE, self(), Info, S]),
	    {noreply, S}
    end;
handle_info(Info, S) ->
    ok = error_logger:format("~p(~p): handle_info(~p, ~p)~n",
			     [?MODULE, self(), Info, S]),
    {noreply, S}.

listen_on_trace_port(Node, Port, S) ->
    [Name, Host] = string:tokens(atom_to_list(Node), [$@]),
    case catch start_trace_client(self(), ip, {Host, Port}) of
	{trace_client_pid, RemotePid} ->
	    rpc:call(Node, ?MODULE, change_pattern, [S#state.trace_pattern]),
	    link(RemotePid),
	    S2 = S#state{trace_nodes = [Node | S#state.trace_nodes],
			 trace_port  = Port + 1},
	    {noreply, S2};
	{'EXIT', Reason} when Reason == already_started->
	    ok = error_logger:format("~p(~p): consumer ignored(~p:~p): ~p~n",
				     [?MODULE, self(), Node, Port, Reason]),
	    S2 = S#state{trace_port = Port + 1},
	    {noreply, S2};
	{'EXIT', Reason} ->
	    self() ! {nodeup, Node},
	    ok = error_logger:format("~p(~p): consumer retry(~p:~p):~n     ~p~n",
				     [?MODULE, self(), Node, Port, Reason]),
	    S2 = S#state{trace_port = Port + 1},
	    {noreply, S2}
    end.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------

terminate(Reason, S) ->
    [exit(Sub#subscriber.pid, Reason) || Sub <- S#state.subscribers].

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------

code_change(OldVsn, S, Extra) ->
    {ok, S}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

do_clear_table(S) ->
    OldTab = S#state.event_tab,
    ets:delete(OldTab),
    NewTab = ets:new(et_events, [ordered_set, {keypos, 1}, public]),
    S#state{event_tab = NewTab}.

do_dict_insert(Msg = {dict_insert, Key = {subscriber, Pid}, Val}, S) ->
    OldSubscribers = S#state.subscribers,
    NewSubscribers =
	case lists:member(Pid, OldSubscribers) of
	    true  ->
		OldSubscribers;
	    false ->
		link(Pid),
		All = ets:match_object(S#state.dict_tab, '_'),
		lists:foreach(fun({K, V}) -> Pid ! {et, {dict_insert, K, V}} end, All),
		[Pid | OldSubscribers]
	end,
    do_multicast(NewSubscribers, Msg),
    ets:insert(S#state.dict_tab, {Key, Val}),
    S#state{subscribers = NewSubscribers};
do_dict_insert(Msg = {dict_insert, Key, Val}, S) ->
    do_multicast(S#state.subscribers, Msg),
    ets:insert(S#state.dict_tab, {Key, Val}),
    S.

do_dict_delete(Msg = {dict_delete, Key = {subscriber, Pid}}, S) ->
    OldSubscribers = S#state.subscribers,
    NewSubscribers =
	case lists:member(Pid, OldSubscribers) of
	    true  ->
		unlink(Pid),
		OldSubscribers -- [Pid];
	    false ->
		OldSubscribers
	end,
    do_multicast(NewSubscribers, Msg),
    ets:delete(S#state.dict_tab, Key),
    S#state{subscribers = NewSubscribers};
do_dict_delete(Msg = {dict_delete, {filter, collector}}, S) ->
    S;
do_dict_delete(Msg = {dict_delete, Key}, S) ->
    do_multicast(S#state.subscribers, Msg),
    ets:delete(S#state.dict_tab, Key),
    S.

tab_iterate(Fun, Tab, '$end_of_table', Acc) ->
    Acc;
tab_iterate(Fun, Tab, Key, Acc) ->
    Acc2 = lists:foldl(Fun, Acc, ets:lookup(Tab, Key)),
    tab_iterate(Fun, Tab, ets:next(Tab, Key), Acc2).

file_open(F) ->
    Fd = make_ref(),
    case F#file.file_opt of
        write  -> file:rename(F#file.name, F#file.name ++ ".OLD");
        append -> ignore
    end,
    Args = [{file, F#file.name}, {name, Fd},
            {repair, true}, {mode, read_write}],
    case disk_log:open(Args) of
        {ok, _} ->
            {ok, Fd};
        {repaired, _, _, BadBytes} ->
            ok = error_logger:format("~p: Skipped ~p bad bytes in file: ~p~n",
                                     [?MODULE, BadBytes, F#file.name]),
            {ok, Fd};
        {error,Reason} ->
            {error,Reason}
    end.

parse_file_options(F, [H | T]) ->
    case H of
        existing -> parse_file_options(F#file{event_opt = existing} , T);
        %%new      -> parse_file_options(F#file{event_opt = new} , T);
        all      -> parse_file_options(F#file{event_opt = all} , T);
        write    -> parse_file_options(F#file{file_opt  = write} , T);
        append   -> parse_file_options(F#file{file_opt  = append} , T);
        keep     -> parse_file_options(F#file{table_opt = keep} , T);
        clear    -> parse_file_options(F#file{table_opt = clear} , T);
        Bad      -> {error, {bad_file_option, Bad}}
    end;
parse_file_options(F, []) ->
    {ok, F}.

do_multicast([Pid | Pids], Msg) ->
    Pid ! {et, Msg},
    do_multicast(Pids, Msg);
do_multicast([], Msg) ->
    ok.
