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
%% Purpose: Collect trace events and forwards them to subscribers
%%----------------------------------------------------------------------

-module(event_collector).

-behaviour(gen_server).

%% External exports
-export([start_link/1, stop/1, start_global/1, stop_global/0,
         save/3, load/2, clear/1,
         subscribe/3, unsubscribe/3, subscribers/1,
         register_filter/3, unregister_filter/2, filters/1,
         report/2, refresh/2, lookup/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("event_tracer.hrl").

-record(state, {parent_pid, table_id, keypos, subscribers, filters, seq_n,
                file, ensure_tracer, trace_pattern, trace_spec}).
-record(file,  {name, desc, event_opt, file_opt, table_opt}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% Start a collector
%% Returns {ok, Pid} | {error, Reason}
start_link(Options) ->
    AllEvents = #filter{name = unfiltered, function = fun(_) -> true end},
    Default = #state{parent_pid    = self(),
                     keypos        = reported_ts,
		     seq_n         = 0,
                     subscribers   = [],
                     filters       = [AllEvents],
                     ensure_tracer = false,
                     trace_pattern = max},
    case parse_opt(Options, Default) of
        {ok, S} ->
            gen_server:start_link(?MODULE, [S], []);
        {error, Reason} ->
            {error, Reason}
    end.

start_global(Options) ->
    case global:whereis_name(?MODULE) of
        undefined ->
            case start_link(Options) of
                {ok, CollectorPid} ->
                    case global:register_name(?MODULE, CollectorPid) of
                        yes ->
                            {ok, CollectorPid};
                        no ->
                            %% Bad luck
                            stop(CollectorPid),
                            start_global(Options)
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        CollectorPid when pid(CollectorPid)->
            {ok, CollectorPid}
    end.

%% Stop a collector
%% Returns ok
stop(CollectorPid) ->
    call(CollectorPid, stop).

stop_global() ->
    case global:whereis_name(?MODULE) of
        CollectorPid when pid(CollectorPid) ->
            stop(CollectorPid);
        undefined ->
            {error, not_started}
    end.
%% Save the event table to a file
%% Returns ok | {error, Reason}
save(CollectorPid, FileName, Options) ->
    call(CollectorPid, {save, FileName, Options}).

%% Load the event table from a file
%% Returns ok | {error, Reason}
load(CollectorPid, FileName) ->
    call(CollectorPid, {load, FileName}).

%% Clear the event table
%% Returns ok
clear(CollectorPid) ->
    call(CollectorPid, clear).

%% Activate a subscription of collector events
%%
%% When an Event is reported, it is stored in the event table
%% of the collector, and optionally forwarded to its subscribers
%% depending of the outcome of their respective FilterFun.
%% A FilterFun may either a anonymous fun or an atom associated 
%% with a named filter fun.

%% The behaviour is like this:
%% 
%%     case FilterFun(Event) of
%%         false             -> ignore;
%%         true              -> SubscriberPid ! {event_collector, Event};
%%         {true, NewEvent}  -> SubscriberPid ! {event_collector, NewEvent}
%%     end.
%%
%% Refresh ('all' | 'new') controls whether the Subscriber
%% should get all old events redelivered or not. It may be useful
%% if the same process already is subscribing, but needs to
%% replace its FilterFun with a new one.
%%
%% Returns {ok, FilterName, FilterFun} | {error, Reason}
subscribe(_, _, Refresh) when Refresh /= all, Refresh /= new ->
    {error, {bad_type, Refresh}};
subscribe(CollectorPid, FilterFun, Refresh) ->
    call(CollectorPid, {subscribe, self(), FilterFun, Refresh}).

%% Deacivate a subscription of collector events
%% Returns ok | {error, Reason}
unsubscribe(CollectorPid, SubPid, Reason) when  pid(SubPid) ->
    call(CollectorPid, {unsubscribe, SubPid, Reason});
unsubscribe(CollectorPid, SubPid, Reason) ->
    {error, {bad_type, SubPid}}.

%% Returns a list of all subscribers as subscriber records.
subscribers(CollectorPid) ->
    call(CollectorPid, subscribers).

%% Reports an event
%%
%% See subscriber/4 for more info.
%% Returns ok
report(CollectorPid, Event) -> 
    tell(CollectorPid, Event),
    ok.

%% Refresh the event table
%%
%% Redelivers all old events to its subscriber.
%% 
%% Returns ok
refresh(CollectorPid, SubPid) -> 
    call(CollectorPid, {refresh, SubPid}).

%% Looks up an event, given its key (reported_ts | parsed_ts)
%% 
%% Returns [] | [Event]
lookup(CollectorPid, EventKey) ->
    call(CollectorPid, {lookup, EventKey}).

%% Add a named filter fun
%%
%% The named filter fun is also forwarded to all subscribers
%%
%% Returns ok | {error, Reason}
register_filter(CollectorPid, Name, Fun) -> 
    call(CollectorPid, {register_filter, Name, Fun}).

%% Delete a named filter fun
%% 
%% The name of the deleted filter fun is also forwarded to all subscribers
%%
%% Returns ok | {error, Reason}
unregister_filter(CollectorPid, Name) -> 
    call(CollectorPid, {unregister_filter, Name}).

%% Returns a list of all named filter funs
filters(CollectorPid) -> 
    call(CollectorPid, filters).

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
init([S]) ->
    process_flag(trap_exit, true),
    Self = self(), 
    Spec = {fun(Event, Acc) -> report(Self, Event), Acc end, ok},
    S2   = case S#state.trace_spec == undefined of
	       true  -> S#state{trace_spec = Spec};
	       false -> S
	   end,
    case ensure_tracer(S2) of
        ok ->
            Tab = ets:new(?MODULE, [ordered_set, {keypos, 1}, public]),
            {ok, S2#state{table_id = Tab}};
        {error, Reason} ->
            {error, Reason}
    end.

make_key(Event, S) ->
    N = S#state.seq_n + 1,
    S2 = S#state{seq_n = N},
    case S#state.keypos of
	reported_ts -> {{Event#event.reported_ts, N}, S2};
	parsed_ts   -> {{Event#event.parsed_ts,   N}, S2}
    end.

ensure_tracer(S) ->
    case S#state.ensure_tracer of
        true ->
            Pattern = S#state.trace_pattern,
            Spec    = S#state.trace_spec,
            case event_tracer:start_link(Pattern, Spec) of
                {ok, _TracerPid} -> ok;
                {error, Reason}  -> {error, {event_tracer, Reason}}
            end;
        false ->
            ok
    end.

parse_opt([], S) ->
    {ok, S};
parse_opt([H | T], S) ->
    case H of
        {parent_pid, Parent} when pid(Parent) ->
            parse_opt(T, S#state{parent_pid = Parent});
        {keypos, reported_ts} ->
            parse_opt(T, S#state{keypos = reported_ts});
        {keypos, parsed_ts} ->
            parse_opt(T, S#state{keypos = parsed_ts});
        {ensure_tracer, true} ->
            parse_opt(T, S#state{ensure_tracer = true});
        {ensure_tracer, false} ->
            parse_opt(T, S#state{ensure_tracer = false});
        {trace_pattern, Pattern} ->
            parse_opt(T, S#state{trace_pattern = Pattern});
        {trace_spec, Spec} ->
            parse_opt(T, S#state{trace_spec = Spec});
        Bad ->
            {error, {bad_option, Bad}}
    end;
parse_opt(BadList, S) ->
    {error, {bad_option_list, BadList}}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(close, From, S) ->
    case S#state.file of
        undefined ->
            {reply, {error, file_not_open}, S};
        F ->
            Reply = disk_log:close(F#file.desc),
            S2 = S#state{file = undefined},
            {reply, Reply, S2}
    end;
handle_call({save, FileName, Options}, From, S) ->
    Default = #file{name = FileName,
                    event_opt = existing,
                    file_opt  = write,
                    table_opt = keep},
    case parse_file_options(Default, Options) of
        {ok, F} ->
            case file_open(F) of
                {ok, Fd} ->
                    F2 = F#file{desc = Fd},
                    Tab = S#state.table_id,
                    WriteFun = fun(E, A) -> ok = disk_log:log(Fd, E), A end,
                    Res = 
                        case F2#file.event_opt of
                            new ->
                                Reply = ok,
                                S2 = S#state{file = F},
                                {reply, Reply, S2};
                            existing ->
                                Reply = tab_iterate(WriteFun, Tab, ok),
                                disk_log:close(Fd),
                                {reply, Reply, S};
                            all ->
                                Reply = tab_iterate(WriteFun, Tab, ok),
                                S2 = S#state{file = F},
                                {reply, Reply, S2}
                        end,
                    case F2#file.table_opt of
                        keep ->  Res;
                        clear -> do_clear(S), Res
                    end;
                {error, Reason} ->
                    {error, {file_open, Reason}}
            end;
        {error, Reason} ->
            {error, Reason}
    end;
handle_call({load, FileName}, From, S) ->
    LoadFun = fun(Event, {_, State}) -> insert(Event, State) end,
    {Reply, S2} = file_iterate(LoadFun, FileName, {ok,S}),
    {reply, Reply, S2};
handle_call(clear, From, S) ->
    do_clear(S),
    Reply = ok,
    {reply, Reply, S};
handle_call({subscribe, Pid, Filter, Refresh}, From, S) ->
    if
        atom(Filter) ->
            case lookup_filter(Filter, S#state.filters) of
                {value, F} ->
                    Sub = #subscriber{pid = Pid, filter = F},
                    do_subscribe(Sub, Refresh, S);
                false ->
                    Reply = {error, {no_such_fun, Filter}},
                    {reply, Reply, S}
            end;
        function(Filter) ->
            F = #filter{name = anonymous, function = Filter},
            Sub = #subscriber{pid = Pid, filter = F},
            do_subscribe(Sub, Refresh, S);
        record(Filter, filter) ->
            Sub = #subscriber{pid = Pid, filter = Filter},
            do_subscribe(Sub, Refresh, S);
        true ->
            {error, {bad_type, Filter}}
    end;
handle_call({unsubscribe, SubPid, Reason}, From, S) ->
    S2 = do_unsubscribe(SubPid, Reason, S, true),
    {reply, ok, S2};
handle_call(subscribers, From, S) ->
    {reply, S#state.subscribers, S};
handle_call({register_filter, Name, Fun}, From, S)
  when atom(Name), function(Fun) ->
    Funs = S#state.filters,
    case lookup_filter(Name, Funs) of
        {value, _} ->
            {reply, {error, {already_exists, Name}}, S};
        false when Name == anonymous ->
            {reply, {error, {already_exists, Name}}, S};
        false ->
            F = #filter{name = Name, function = Fun},
            S2 = S#state{filters = [F | Funs]},
            Msg = {register_filter, F},
            Tell = fun(Sub) -> tell(Sub#subscriber.pid, Msg) end,
            lists:foreach(Tell, S#state.subscribers),
            {reply, ok, S2}
    end;
handle_call({unregister_filter, Name}, From, S) when atom(Name)->
    Funs = S#state.filters,
    case lookup_filter(Name, Funs) of
        {value, F} ->
            S2 = S#state{filters = Funs -- [F]},
            Msg = {unregister_filter, F},
            Tell = fun(Sub) -> tell(Sub#subscriber.pid, Msg) end,
            lists:foreach(Tell, S#state.subscribers),
            {reply, ok, S2};
        false ->
            {reply, {error, {no_such_fun, Name}}, S}
    end;
handle_call(filters, From, S) ->
    {reply, S#state.filters, S};
handle_call({refresh, all}, From, S) ->
    {Reply, S2} = redeliver_all(S#state.subscribers, S),
    {reply, Reply, S2};
handle_call({refresh, SubPid}, From, S) when pid(SubPid) ->
    case lists:keysearch(SubPid, #subscriber.pid, S#state.subscribers) of
        {value, Sub} ->
            {Reply, S2} = redeliver_all([Sub], S),
            {reply, Reply, S2};
        false ->
            Reply = {error, {unknown_subscriber, SubPid}},
            {reply, Reply, S}
    end;
handle_call({lookup, EventKey}, From, S) ->
    Reply = do_lookup(S#state.table_id, EventKey),
    {reply, Reply, S};
handle_call(stop, From, S) ->
    unlink(S#state.parent_pid),
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
handle_info({?MODULE, Event}, S) when record(Event, event) ->
    {_Reply, S2} = insert(Event, S),
    {noreply, S2};
handle_info({'EXIT', Pid, Reason}, S) when Pid == S#state.parent_pid ->
    {stop, Reason};
handle_info({'EXIT', Pid, Reason}, S) ->
    S2 = do_unsubscribe(Pid, Reason, S, false),
    {noreply, S2};
handle_info(Info, S) ->
    ok = error_logger:format("~p(~p): handle_info(~p, ~p)~n",
                             [?MODULE, self(), Info, S]),
    {noreply, S}.

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

tab_iterate(Fun, Tab, Acc) ->
    case catch do_tab_iterate(Fun, Tab, ets:first(Tab), Acc) of
        {'EXIT', Reason} ->
            {error, {tab_iterate, Tab, {'EXIT', Reason}}};
        Acc2 ->
            Acc2
    end.

do_tab_iterate(Fun, Tab, '$end_of_table', Acc) ->
    Acc;
do_tab_iterate(Fun, Tab, {EventKey, _} = Key, Acc) ->
    Events = do_lookup(Tab, EventKey),
    Acc2 = lists:foldl(Fun, Acc, Events),
    do_tab_iterate(Fun, Tab, ets:next(Tab, Key), Acc2).

do_lookup(Tab, EventKey) ->
    Objs = ets:match_object(Tab, {{EventKey, '_'}, '_'}),
    [Event || {_, Event} <- Objs].

file_iterate(Fun, FileName, {Res, S})
  when record(S#state.file, file), (S#state.file)#file.name == FileName->
    {{error, {already_opened_for_write, FileName}}, S};
file_iterate(Fun, FileName, {Res, S}) ->
    Fd = make_ref(),
    Args = [{file, FileName}, {name, Fd},
            {repair, true}, {mode, read_only}],
    InitAcc = {Res, S},
    case disk_log:open(Args) of
        {ok, _} ->
            do_file_iterate(Fun, Fd, FileName, InitAcc);
        {repaired, _, _, BadBytes} ->
            ok = error_logger:format("~p: Skipped ~p bad bytes in file: ~p~n",
                                     [?MODULE, BadBytes, FileName]),
            do_file_iterate(Fun, Fd, FileName, InitAcc);
        {error,Reason} ->
            {{error, {file_open, FileName, Reason}}, S}
    end.


do_file_iterate(Fun, Fd, FileName, Acc) ->
    Wrapper = fun(E, A) when record(E, event) ->
                      Fun(E, A);
                 (Bad, A) ->
                      exit({bad_event, Bad})
              end,
    {Res2, S2} = (catch do_file_iterate(Wrapper, Fd, FileName, start, Acc)),
    disk_log:close(Fd),
    case Res2 of
        {'EXIT', _} ->
            {{error, {file_iterate, FileName, Res2}}, S2};
        _ ->
            {Res2, S2}
    end.

do_file_iterate(Fun, Fd, FileName, Cont, Acc) ->
    case disk_log:chunk(Fd, Cont) of
        eof ->
            Acc;
        {error, Reason} ->
            {error, {bad_chunk, Reason}};
        {Cont2, Events} ->
            Acc2 = lists:foldl(Fun, Acc, Events),
            do_file_iterate(Fun, Fd, FileName, Cont2, Acc2);
        {Cont2, Events, BadBytes} ->
            ok = error_logger:format("~p: Skipped ~p bad bytes in file: ~p~n",
                                     [?MODULE, BadBytes, FileName]),
            Acc2 = lists:foldl(Fun, Acc, Events),
            do_file_iterate(Fun, Fd, FileName, Cont2, Acc2)
    end.

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
        new      -> parse_file_options(F#file{event_opt = new} , T);
        all      -> parse_file_options(F#file{event_opt = all} , T);
        write    -> parse_file_options(F#file{file_opt  = write} , T);
        append   -> parse_file_options(F#file{file_opt  = append} , T);
        keep     -> parse_file_options(F#file{table_opt = keep} , T);
        clear    -> parse_file_options(F#file{table_opt = clear} , T);
        Bad      -> {error, {bad_file_option, Bad}}
    end;
parse_file_options(F, []) ->
    {ok, F}.

insert(Event, S) ->
    {Key, S2} = make_key(Event, S),
    ets:insert(S#state.table_id, {Key, Event}),
    case S2#state.file of
            undefined ->
	    ignore;
	F  ->
	    Fd = F#file.desc,
	    ok = disk_log:log(Fd, Event)
    end,
    deliver(S2#state.subscribers, Event, S2, ok).

deliver([Sub | Tail], Event, S, Result) ->
    Filter = Sub#subscriber.filter,
    FilterFun = Filter#filter.function,
    SubPid = Sub#subscriber.pid,
    case catch FilterFun(Event) of
        false ->
            deliver(Tail, Event, S, Result);
        true ->
            tell(SubPid, Event),
            deliver(Tail, Event, S, Result);
        {true, CustomizedEvent} when record(CustomizedEvent, event) ->
            tell(SubPid, CustomizedEvent),
            deliver(Tail, Event, S, Result);
        Bad ->
	    Name     = Filter#filter.name,
	    Contents = {bad_filter, Name, Bad, Event},
	    BadEvent = Event#event{contents = Contents,
				   from = bad_filter, to = bad_filter},
            tell(SubPid, BadEvent),
            deliver(Tail, Event, S, Result)
    end;
deliver([], Event, S, Result) ->
    {Result, S}.

redeliver_all(Subs, S) ->
    Tab = S#state.table_id,
    tell_clear(Subs, Tab),
    Fun = fun(Event, {R, State}) -> deliver(Subs, Event, State, R) end,
    tab_iterate(Fun, Tab, {ok, S}).

do_subscribe(Sub, Refresh, S) ->
    SubPid = Sub#subscriber.pid,
    Pos = #subscriber.pid,
    Subs = S#state.subscribers,
    S2 = 
        case lists:keymember(SubPid, Pos, Subs) of
            true  ->
                S#state{subscribers =
                        [Sub | lists:keydelete(SubPid, Pos, Subs)]};
            false -> 
                link(SubPid),
                Tell = fun(F) -> tell(SubPid, {register_filter, F}) end,
                lists:foreach(Tell, S#state.filters),
                S#state{subscribers = [Sub | Subs]}
        end,
    case Refresh of
        all ->
            case redeliver_all([Sub], S2) of
                {ok, S3} ->
                    {reply, {ok, Sub#subscriber.filter}, S3};
                Error ->
                    {reply, Error, S2}
            end;
        new ->
            {reply, {ok, Sub#subscriber.filter}, S2}
    end.

do_unsubscribe(SubPid, Reason, S, Tell) ->
    Pos = #subscriber.pid,
    Subs = S#state.subscribers,
    case lists:keymember(SubPid, Pos, Subs) of
        true ->
            case Tell of
                true  ->
                    unlink(SubPid),
                    exit(SubPid, Reason);
                false ->
                    ignore
            end,
            New = lists:keydelete(SubPid, Pos, Subs),
            S#state{subscribers = New};
        false ->
            S
    end.

do_clear(S) ->
    Tab = S#state.table_id,
    ets:match_delete(Tab, '_'),
    tell_clear(S#state.subscribers, Tab).

tell_clear(Subs, Tab) ->
    TabSize = ets:info(Tab, size),
    Fun = fun(S) -> tell(S#subscriber.pid, {clear, TabSize}) end,
    lists:foreach(Fun, Subs).

tell(SubPid, What) ->
    SubPid ! {?MODULE, What}.

lookup_filter(Name, Funs) ->
    lists:keysearch(Name, #filter.name, Funs).
