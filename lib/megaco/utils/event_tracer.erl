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
%% Purpose: Tracing of events
%%----------------------------------------------------------------------

-module(event_tracer).

-behaviour(gen_server).

%% External exports
-export([
         start_link/0,
         start_link/2,
         stop/0,
         set_global_pattern/1,

         report/4,
         report/5,
         get_pattern/1,
         set_pattern/1,
         start_producer/1,
         start_consumer/2,

         get_show_label_spec/0,
         get_show_contents_spec/0,
         get_filter/1,
         show/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("event_tracer.hrl").

-record(state, {parent_pid, pattern, spec, local_pid, nodes, next_port}).

-define(DETAIL_SEQ_PRINT,      5).
-define(DETAIL_SEQ_RECEIVE,   10).
-define(DETAIL_SEQ_SEND,      15).
-define(DETAIL_DROP,          20).
-define(DETAIL_SPAWN,         25).
-define(DETAIL_EXIT,          30).
-define(DETAIL_RECEIVE,       35).
-define(DETAIL_SEND_NO_EXIST, 40).
-define(DETAIL_SEND,          45).
-define(DETAIL_LINK,          50).
-define(DETAIL_UNLINK,        55).
-define(DETAIL_GETTING_LINKED,60).
-define(DETAIL_CALL,          65).
-define(DETAIL_GC_START,      70).
-define(DETAIL_GC_END,        75).
-define(DETAIL_RETURN_FROM,   80).
-define(DETAIL_RETURN_TO,     85).
-define(DETAIL_IN,            90).
-define(DETAIL_OUT,           95).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------


%%----------------------------------------------------------------------
%% Reports an action, such as a message
%%
%% DetailLevel is an integer and Action is a tuple
%%----------------------------------------------------------------------

report(DetailLevel, Who, Label, Contents) ->
    ?MODULE:report(DetailLevel, Who, Who, Label, Contents). % N.B. External call

report(DetailLevel, From, To, Label, Contents) when integer(DetailLevel) ->
    hopefully_traced.

%%----------------------------------------------------------------------
%% Activates/deactivates tracing by changing the current trace Pattern.
%%
%% The final Pattern must be a valid MatchSpec according to
%% erlang:trace_pattern/2.
%%
%% For conveniance reasons the pattern can be replaced with the
%% shorter notion of a trace level. The trace level is internally
%% converted to an appropriate Pattern with get_pattern/1.
%%
%% set_pattern(min) ->
%%   deactivates tracing of calls to report/4,5
%%   
%% set_pattern(max) ->
%%   activates tracing of all calls to report/4,5
%%   
%% set_pattern(Integer) ->
%%   activates tracing of all calls to report/4,5 with
%%   DetailLevel >= Integer
%%----------------------------------------------------------------------

get_pattern(min) ->
    [];
get_pattern(max) ->
    Head = ['$1', '_', '_', '_', '_'],
    Body = [],
    Cond = [],
    [{Head, Cond, Body}];
get_pattern(Int) when integer(Int) ->
    Head = ['$1', '_', '_', '_', '_'],
    Body = [],
    Cond = [{ '<', '$1', Int}],
    [{Head, Cond, Body}].

set_pattern(Pattern) ->
    MFA = {?MODULE, report, 5},
    case Pattern of
        [] ->
            error_to_exit(dbg:ctp({?MODULE, report, 5})),
            error_to_exit(dbg:p(all, clear));
        List when list(List) ->
            error_to_exit(dbg:ctp(MFA)),
            error_to_exit(dbg:tp(MFA, Pattern)),
            error_to_exit(dbg:p(all, [call, timestamp]));
        Other ->
            set_pattern(get_pattern(Other))
    end.

error_to_exit({error, Reason}) ->
    exit(Reason);
error_to_exit({ok, Res}) ->
    Res.

%%----------------------------------------------------------------------
%% start_producer(Target)
%% 
%% Start producing trace events and send them to a port or file
%%----------------------------------------------------------------------
start_producer(Port) when integer(Port) ->
    start_producer({Port, 50});
start_producer({Port, MaxQueue}) when integer(Port), integer(MaxQueue) ->
    dbg:tracer(port, dbg:trace_port(ip, {Port, MaxQueue}));
start_producer(FileName) when list(FileName) ->
    dbg:tracer(port, dbg:trace_port(file, FileName)).

%%----------------------------------------------------------------------
%% start_consumer(Source, {Fun, Acc} = HandlerSpec)
%% 
%% Start consuming trace events by reading them from the Source
%% and invoke Fun(Event, Acc) -> NewAcc, for each Event.
%%
%% The Source may be a port, file or a local process which
%% in fact is a combined producer and consumer.
%%
%% The Source may also be a global consumer 
%%----------------------------------------------------------------------
start_consumer(local, HandlerSpec) ->
    dbg:tracer(process, spec_wrapper(HandlerSpec));
start_consumer(Port, HandlerSpec) when integer(Port) ->
    {ok, dbg:trace_client(ip, Port, spec_wrapper(HandlerSpec))};
start_consumer({Host, Port}, HandlerSpec) when list(Host), integer(Port) ->
    {ok, dbg:trace_client(ip, {Host, Port}, spec_wrapper(HandlerSpec))};
start_consumer(FileName, HandlerSpec) when list(FileName) ->
    {ok, dbg:trace_client(file, FileName, spec_wrapper(HandlerSpec))}.

spec_wrapper({EventFun, EventInitialAcc}) when function(EventFun) ->
    {fun(Trace, Acc) ->
             case catch parse_event(Trace) of
                 Event when record(Event, event) ->
                     EventFun(Event, Acc);
                 Bad ->
                     ok = error_logger:format("~p(~p): ~p~n     -> ~p ~n",
                                              [?MODULE, ?LINE, Trace, Bad])
             end
     end,
     EventInitialAcc
    }.

%%----------------------------------------------------------------------
%% Usage of a global producer/consumer broker
%% 
%% The global broker runs as a globally registered process.  On all
%% connected Erlang nodes, it starts a producer which sends its events
%% to a local port (the port number is generated). On the node where
%% the global broker is running, corresponding consumer processes are
%% started, configured to use the given HandlerSpec.
%% 
%% Whenever new nodes are (dis)connected, this is monitored and new
%% producer/consumers are automatically started and eventually
%% the trace Pattern is set on these nodes.
%%
%% start_global(Pattern, HandlerSpec) -> starts the global broker
%% set_global_pattern(Pattern) -> changes the Pattern on all nodes
%% stop_global() -> stops the global broker and all producers/consumers
%%----------------------------------------------------------------------
start_link() ->
    start_link(max, get_show_contents_spec()).

start_link(Pattern, HandlerSpec) ->
    gen_server:start_link({global, ?MODULE}, ?MODULE,
                          [self(), Pattern, HandlerSpec], []).

stop() ->
    rpc:multicall(dbg, stop, []),
    case global:whereis_name(?MODULE) of
        undefined ->
            ok;
        Pid when pid(Pid) ->
            unlink(Pid),
            exit(Pid, shutdown),
            ok
    end.

set_global_pattern(Pattern) ->
    gen_server:call({global, ?MODULE}, {set_pattern, Pattern}, infinity).

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
init([Parent, Pattern, Spec]) ->
    process_flag(trap_exit, true),
    net_kernel:monitor_nodes(true),
    lists:foreach(fun(N) -> self() ! {nodeup, N} end, nodes()),
    case start_consumer(local, Spec) of
        {ok, Local} ->
            link(Local),
            set_pattern(Pattern),
            State = #state{parent_pid = Parent,
                           pattern    = Pattern,
                           spec       = Spec,
                           local_pid  = Local,
                           nodes      = [node()],
                           next_port  = 4711},
            {ok, State};
        {error, Reason} ->
            {stop, Reason}
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

handle_call({set_pattern, RawPattern}, From, State) ->
    case catch get_pattern(RawPattern) of
        {'EXIT', Reason} ->
            Reply = {bad_pattern, Reason},
            {reply, Reply, State};
        NewPattern ->
            Ns = State#state.nodes,
            rpc:multicall(Ns, ?MODULE, set_pattern, [NewPattern]),
            Reply = {old_pattern, State#state.pattern},
            State2 = State#state{pattern = NewPattern},
            {reply, Reply, State2}
    end;
handle_call(stop, From, State) ->
    {stop, shutdown, State};
handle_call(Request, From, State) ->
    ok = error_logger:format("~p(~p): handle_call(~p, ~p, ~p)~n",
                             [?MODULE, self(), Request, From, State]),
    {reply, {error, {bad_request, Request}}, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, State) ->
    ok = error_logger:format("~p(~p): handle_cast(~p, ~p)~n",
                             [?MODULE, self(), Msg, State]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({nodeup, Node}, State) ->
    Port = State#state.next_port,
    case rpc:call(Node, ?MODULE, start_producer, [Port]) of
        {ok, _} ->
            [Name, Host] = string:tokens(atom_to_list(Node), [$@]),
            case start_consumer({Host, Port}, State#state.spec) of
                {ok, Remote} ->
                    rpc:call(Node, ?MODULE, set_pattern, [State#state.pattern]),
                    link(Remote),
                    State2 = State#state{nodes     = [Node | State#state.nodes],
                                         next_port = Port + 1},
                    {noreply, State2};
                {error, Reason} when Reason == already_started->
                    ok = error_logger:format("~p(~p): consumer ignored(~p:~p): ~p~n",
                                             [?MODULE, self(), Node, Port, Reason]),
                    State2 = State#state{next_port = Port + 1},
                    {noreply, State2};
                {error, Reason} ->
                    self() ! {nodeup, Node},
                    ok = error_logger:format("~p(~p): consumer retry(~p:~p):~n     ~p~n",
                                             [?MODULE, self(), Node, Port, Reason]),
                    State2 = State#state{next_port = Port + 1},
                    {noreply, State2}
            end;
        {error, Reason} when Reason == already_started->
            ok = error_logger:format("~p(~p): producer ignored(~p:~p):~n    ~p~n",
                                     [?MODULE, self(), Node, Port, Reason]),
            State2 = State#state{next_port = Port + 1},
            {noreply, State2};
        {badrpc, Reason} ->
            ok = error_logger:format("~p(~p): producer ignored(~p:~p):~n    ~p~n",
                                     [?MODULE, self(), Node, Port, Reason]),
            State2 = State#state{next_port = Port + 1},
            {noreply, State2};
        {error, Reason} ->
            self() ! {nodeup, Node},
            ok = error_logger:format("~p(~p): producer retry(~p:~p):~n     ~p~n",
                                     [?MODULE, self(), Node, Port, Reason]),
            State2 = State#state{next_port = Port + 1},
            {noreply, State2}
    end;
handle_info({nodedown, Node}, State) ->
    State2 = State#state{nodes = lists:delete(Node, State#state.nodes)},
    {noreply, State2};
handle_info({'EXIT', Pid, Reason} = Info, State) when Pid == State#state.local_pid ->
    ok = error_logger:format("~p(~p): ~p -> stopped by dbg:stop()~n",
                             [?MODULE, self(), Info]),
    {stop, Reason, State};
handle_info({'EXIT', Pid, Reason} = Info, State)
  when Pid == State#state.parent_pid ->
    {stop, Reason, State};
handle_info(Info, State) ->
    ok = error_logger:format("~p(~p): handle_info(~p, ~p)~n",
                             [?MODULE, self(), Info, State]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    ignore.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Parse a trace event and make an event record out of it
%%----------------------------------------------------------------------

parse_event(Trace) ->
    ParsedTS = erlang:now(),
    case Trace of
        {trace, Pid, What, Info} ->
            Event = make_event(Pid, What, Info),
            Event#event{reported_ts = ParsedTS, parsed_ts = ParsedTS};
        {trace, Pid, What, Info, Extra} ->
            Event = make_event2(Pid, What, Info, Extra),
            Event#event{reported_ts = ParsedTS, parsed_ts = ParsedTS};
        {trace_ts, Pid, What, Info, ReportedTS} ->
            Event = make_event(Pid, What, Info),
            Event#event{reported_ts = ReportedTS, parsed_ts = ParsedTS};
        {trace_ts, Pid, What, Info, Extra, ReportedTS} ->
            Event = make_event2(Pid, What, Info, Extra),
            Event#event{reported_ts = ReportedTS, parsed_ts = ParsedTS};
        {drop, N} ->
            Event = make_drop_event(N),
            Event#event{reported_ts = ParsedTS, parsed_ts = ParsedTS};
        {drop, N} ->
            Event = make_drop_event(N),
            Event#event{reported_ts = ParsedTS, parsed_ts = ParsedTS};
        {seq_trace, SeqLabel, SeqTrace, ReportedTS} ->
            Event = make_seq_event(SeqLabel, SeqTrace),
            Event#event{reported_ts = ReportedTS, parsed_ts = ParsedTS};
        {seq_trace, SeqLabel, SeqTrace} ->
            Event = make_seq_event(SeqLabel, SeqTrace),
            Event#event{reported_ts = ParsedTS, parsed_ts = ParsedTS}
    end.

make_drop_event(N) ->
    #event{from = undefined, to = undefined, detail_level = ?DETAIL_DROP,
           label = {drop, N}, contents = {drop, N}}.

make_seq_event(SeqLabel, SeqTrace) ->
    case SeqTrace of
        {send, Serial, From, To, Msg} ->
            #event{from = From, to = To, detail_level = ?DETAIL_SEQ_SEND,
                   label = {send, SeqLabel, Serial}, contents = Msg};
        {'receive', Serial, From, To, Msg} ->
            #event{from = From, to = To, detail_level = ?DETAIL_SEQ_RECEIVE,
                   label = {'receive', SeqLabel, Serial}, contents = Msg};
        {print, Serial, From, To, UserInfo} ->
            #event{from = From, to = To, detail_level = ?DETAIL_SEQ_PRINT, 
                   label = {print, SeqLabel, Serial}, contents = UserInfo}
    end.

make_event2(From, Label = send, Msg, To) ->
    #event{from = From, to = To, detail_level = ?DETAIL_SEND,
           label = Label, contents = Msg};
make_event2(From, Label = send_to_non_existing_process, Msg, To) ->
    #event{from = From, to = To, detail_level = ?DETAIL_SEND_NO_EXIST,
           label = Label, contents = Msg};
make_event2(Pid, Label = return_from, {M,F,A} , ReturnValue) ->
    #event{from = Pid, to = Pid, detail_level = ?DETAIL_RETURN_FROM,
           label = Label, contents = {M,F,A, ReturnValue}}.

make_event(Pid, Label = call, MFA) ->
    case MFA of
        {?MODULE, report, [DetailLevel, From, To, Label2, Contents]} ->
            #event{from = From, to = To, detail_level = DetailLevel,
                   label = Label2, contents = Contents};
        {M, F, A} ->
            #event{from = Pid, to = Pid, detail_level = ?DETAIL_CALL,
                   label = Label, contents = MFA}
    end;
make_event(Pid, Label = 'receive', Msg) ->
    #event{from = Pid, to = Pid, detail_level = ?DETAIL_RECEIVE,
           label = Label, contents = Msg};
make_event(Pid, Label = return_to, {M, F, A}) -> 
    #event{from = Pid, to = Pid, detail_level = ?DETAIL_RETURN_TO,
           label = Label, contents = {M, F, A}};
make_event(Pid, Label = in, {M, F, A}) ->
    #event{from = Pid, to = Pid, detail_level = ?DETAIL_IN,
           label = Label, contents = {M, F, A}};
make_event(Pid, Label = out, {M, F, A}) ->
    #event{from = Pid, to = Pid, detail_level = ?DETAIL_OUT,
           label = Label, contents = {M, F, A}};
make_event(Pid, Label = gc_start, TagList) when list(TagList) ->
    #event{from = Pid, to = Pid, detail_level = ?DETAIL_GC_START,
           label = Label, contents = TagList};
make_event(Pid, Label = gc_end, TagList) when list(TagList) ->
    #event{from = Pid, to = Pid, detail_level = ?DETAIL_GC_END,
           label = Label, contents = TagList};
make_event(Pid, Label = spawn, Pid2) ->
    #event{from = Pid, to = Pid2, detail_level = ?DETAIL_SPAWN,
           label = Label, contents = Pid2};
make_event(Pid, Label = exit, Reason) ->
    #event{from = Pid, to = Pid, detail_level = ?DETAIL_EXIT,
           label = Label, contents = Reason};
make_event(Pid, Label = link, Pid2) ->
    #event{from = Pid, to = Pid, detail_level = ?DETAIL_LINK,
           label = Label, contents = Pid2};
make_event(Pid, Label = unlink, Pid2) ->
    #event{from = Pid, to = Pid, detail_level = ?DETAIL_UNLINK,
           label = Label, contents = Pid2};
make_event(Pid, Label = getting_linked, Pid2) ->
    #event{from = Pid, to = Pid, detail_level = ?DETAIL_GETTING_LINKED,
           label = Label, contents = Pid2}.

%%----------------------------------------------------------------------
%% Simple text viewer with some possibilites for custimization. 
%%
%% show(Event, {FilterFun, ShowContents}) ->
%%     Invokes FilterFun(Event) in order to determine if
%%     the event should be displayed or silently ignored.
%%     ShowContents is a boolean which determines whether
%%     the detailed field of the Event record should be
%%     displayed or not.
%%----------------------------------------------------------------------

get_show_label_spec() ->
    {fun show/2, {get_filter(max), false}}.

get_show_contents_spec() ->
    {fun show/2, {get_filter(max), true}}.

get_filter(min) ->
    fun(_) -> false end;
get_filter(max) ->
    fun(_) -> true end;
get_filter(TraceLevel) when integer(TraceLevel) ->
    fun(Event) -> Event#event.detail_level >= TraceLevel end.

show(Event, {FilterFun, ShowContents}) ->
    case FilterFun(Event) of
        false ->
            ignore;
        true ->
            do_show(Event, ShowContents);
        {true, NewEvent} ->
            do_show(NewEvent, ShowContents)
    end,
    FilterFun.

do_show(Event, ShowContents) ->
    F = Event#event.from,
    T = Event#event.to,
    L = Event#event.label,
    case F == T of
        true ->
            ok = io:format(user,  "?p(~p):\t~p",
                           [?MODULE, F, L]);
        false ->
            ok = io:format(user,  "~p(~p -> ~p ):\t~p\n\t~p\n",
                           [?MODULE, F, T, L])
    end,
    case ShowContents of
        true ->
            ok = io:format(user,  "\t~p\n", [Event#event.contents]);
        false ->
            ignore
    end.
