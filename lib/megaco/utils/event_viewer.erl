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
%% Purpose: Displays a sequence chart for trace events (messages/actions)
%%----------------------------------------------------------------------

-module(event_viewer).

-behaviour(gen_server).

%% External exports
-export([start_global/1, start_link/1, stop/0, stop/1, start/0,
         collector_pid/1, report/2, refresh/1, view_contents/2,
         suspend/1, resume/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("event_tracer.hrl").

-record(state,
        {parent_pid, collector_pid, refresh, filter, filters,
         keypos, trace_pattern, display, actors, selected_actor,
         detail_level, hide_actions,
         keys, last_key, visible_n, hidden_n, is_suspended, queue,
         title, win, packer, width, height, scale, font, 
         canvas_height, canvas, y_pos}).
-record(actor, {name, length, string}).

-define(initial_x, 10).
-define(incr_x,    60).
-define(initial_y, 15).
-define(incr_y,    15).
-define(detail_level_min, 0).
-define(detail_level_max, 100).

null_ts() ->
    {{0, 0, 0}, {0, 0, 0}}.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

start() ->
    {ok, Pid} = start_global([]),
    unlink(Pid).

start_global(Options) ->
    start_link([{trace_pattern, max}, {collector_pid, ensure} | Options]).

%% Returns {ok, Pid} | {error, Reason}
start_link(Options) -> 
    case parse_opt(Options, default_state()) of
        {ok, S} ->
            gen_server:start_link(?MODULE, [S], []);
        {error, Reason} ->
            {error, Reason}
    end.

default_state() ->
    #state{parent_pid    = self(),
           display       = graphic,
           trace_pattern = undefined,
           collector_pid = undefined,
           refresh       = all,
           detail_level  = max,
           filter        = unfiltered,
           filters       = [],
           keypos        = reported_ts,
           is_suspended  = false,
           queue         = queue_new(),
           actors        = [],
           selected_actor= make_ref(),
           scale         = 2,
           width         = 800,
           height        = 600}.

parse_opt([], S) ->
    case S#state.detail_level of
        max ->
            {ok, S#state{detail_level = ?detail_level_max}};
        min ->
            {ok, S#state{detail_level = ?detail_level_min}};
        Int when integer(Int) ->
            {ok, S}
    end;
parse_opt([H | T], S) ->
    case H of
        {parent_pid, Parent} when pid(Parent) ->
            parse_opt(T, S#state{parent_pid = Parent});
        {title, Title} ->
            parse_opt(T, S#state{title = name_to_string(Title)});
        {display, graphic} ->
            parse_opt(T, S#state{display = graphic});
        {display, text} ->
            parse_opt(T, S#state{display = text});
        {trace_pattern, Pattern} -> 
            parse_opt(T, S#state{trace_pattern = Pattern});
        {collector_pid, undefined} -> 
            parse_opt(T, S#state{collector_pid = undefined});
        {collector_pid, ensure} -> 
            parse_opt(T, S#state{collector_pid = ensure});
        {collector_pid, C} when pid(C) -> 
            parse_opt(T, S#state{collector_pid = C});
        {detail_level, Level} when integer(Level),
                                   Level >= ?detail_level_min,
                                   Level =< ?detail_level_max -> 
            parse_opt(T, S#state{detail_level = Level});
        {detail_level, max} ->
            parse_opt(T, S#state{detail_level = ?detail_level_max});
        {detail_level, min} ->
            parse_opt(T, S#state{detail_level = ?detail_level_min});
        {filter, Fun} when function(Fun) -> 
            parse_opt(T, S#state{filter = Fun});
        {filter, Name} when atom(Name) -> 
            parse_opt(T, S#state{filter = Name});
        {filter, F} when record(F, filter) -> 
            parse_opt(T, S#state{filter = F});
        {refresh, new} ->
            parse_opt(T, S#state{refresh = new});
        {refresh, all} ->
            parse_opt(T, S#state{refresh = all});
        {is_suspended, true} ->
            parse_opt(T, S#state{is_suspended = true});
        {is_suspended, false} ->
            parse_opt(T, S#state{is_suspended = false});
        {keypos, reported_ts} ->
            parse_opt(T, S#state{keypos = reported_ts});
        {keypos, parsed_ts} ->
            parse_opt(T, S#state{keypos = parsed_ts});
        {actors, ActorNames} when list(ActorNames) ->
            Actors = [init_actor(A) || A <- ActorNames],
            parse_opt(T, S#state{actors = Actors});
        {scale, Scale} when integer(Scale), Scale > 0 ->
            parse_opt(T, S#state{scale = Scale});
        {width, Width} when integer(Width), Width > 0 ->
            parse_opt(T, S#state{width = Width});
        {height, Height} when integer(Height), Height > 0 ->
            parse_opt(T, S#state{height = Height});
        Bad ->
            {error, {bad_option, Bad}}
    end;
parse_opt(BadList, S) ->
    {error, {bad_option_list, BadList}}.

%% pick_opt(Tag, List) when list(List) ->
%%     case lists:keysearch(Tag, 1, List) of
%%         {value, Value} ->
%%             Value;
%%         false ->
%%             undefined
%%     end;
%% pick_opt(Tag, List) ->
%%     undefined.

%% Reports an event
%%
%% See subscriber/4 for more info.
%% Returns ok
report(ToolPid, Event) when record(Event, event) ->
    %% Use same message format as event_collector module
    ToolPid ! {event_collector, Event},
    ok.

%% Refresh the view of the event table
%%
%% Returns ok
refresh(ToolPid) -> 
    call(ToolPid, refresh).

%% Show an contents of the event
%% 
%% Returns ok | {error, Reason}
view_contents(ToolPid, EventKey) ->
    call(ToolPid, {view_contents, EventKey}).

collector_pid(ToolPid) ->
    call(ToolPid, collector_pid).

suspend(ToolPid) ->
    call(ToolPid, suspend).

resume(ToolPid) ->
    call(ToolPid, resume).

stop() ->
    event_collector:stop_global().

stop(ToolPid) ->
    call(ToolPid, stop).

call(ToolPid, Request) ->
    gen_server:call(ToolPid, Request, infinity).

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
init([S]) when record(S, state) ->
    process_flag(trap_exit, true),
    case S#state.collector_pid of
        undefined  ->
            case global:whereis_name(event_collector) of
                undefined ->
                    {stop, {badarg, no_collector_pid}};
                CollectorPid when pid(CollectorPid) ->
                    do_init(S#state{collector_pid = CollectorPid})
            end;
        ensure ->
            Opt =
                case S#state.trace_pattern of
                    undefined ->
                        [{ensure_tracer, false}];
                    Pattern ->
                        [{ensure_tracer, true},
                         {trace_pattern, Pattern}]
                end,
            case event_collector:start_global([{keypos, S#state.keypos} | Opt]) of
                {ok, CollectorPid} ->
                    do_init(S#state{collector_pid = CollectorPid});
                {error, Reason} ->
                    {stop, {event_collector, Reason}}
            end;
        CollectorPid when pid(CollectorPid) ->
            do_init(S)
    end.

do_init(S) ->
    C = S#state.collector_pid,
    case event_collector:subscribe(C, S#state.filter, S#state.refresh) of
        {ok, F} ->
	    Name = F#filter.name,
            Title = 
                case S#state.title of
                    undefined -> atom_to_list(?MODULE);
                    Explicit  -> Explicit
                end,
            S2 = S#state{filter = F, title = Title},
            display_init(S2);
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
handle_call(refresh, From, S) ->
    Reply = ok,
    S2 = do_refresh(S),
    reply(Reply, S2);
handle_call({view, EventKey}, From, S) ->
    case event_collector:lookup(S#state.collector_pid, EventKey) of
        [] ->
            Reply = {error, {no_such_event, EventKey}},
            reply(Reply, S);
        [Event] ->
            S2 = display_contents(Event, S),
            Reply = ok,
            reply(Reply, S2)
    end;
handle_call(collector_pid, From, S) ->
    Reply = S#state.collector_pid,
    reply(Reply, S);
handle_call(suspend, From, S) ->
    Reply = ok,
    S2 = do_suspend(S),
    reply(Reply, S2);
handle_call(resume, From, S) ->
    Reply = ok,
    S2 = do_resume(S),
    reply(Reply, S2);
handle_call(stop, From, S) ->
    unlink(S#state.parent_pid),
    {stop, shutdown, S};
handle_call(Request, From, S) ->
    ok = error_logger:format("~p(~p): handle_call(~p, ~p, ~p)~n",
                             [?MODULE, self(), Request, From, S]),
    Reply = {error, {bad_request, Request}},
    reply(Reply, S).

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(Msg, S) ->
    ok = error_logger:format("~p(~p): handle_cast(~p, ~p)~n",
                             [?MODULE, self(), Msg, S]),
    noreply(S).

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({event_collector, Event}, S) when record(Event, event) ->
    if
        Event#event.detail_level > S#state.detail_level ->
            %% Ignore event
            noreply(S);
        Event#event.from == Event#event.to,
        S#state.hide_actions == true ->
            %% Ignore event
            noreply(S);
        true ->
            case S#state.is_suspended of
                false ->
                    S2 = display_named_arrow(Event, S),
                    noreply(S2);
                true ->
                    S2 = S#state{queue = queue_in(Event, S#state.queue)},
                    noreply(S2)
            end
    end;
handle_info({event_collector, {register_filter, F}}, S) when record(F, filter) ->
    Filters = lists:keydelete(F#filter.name, #filter.name, S#state.filters),
    S2 = S#state{filters = [F | Filters]},
    noreply(S2);
handle_info({event_collector, {unregister_filter, F}}, S) when record(F, filter) ->
    S2 = S#state{filters = S#state.filters -- [F]},
    noreply(S2);
handle_info({event_collector, {clear, TabSize}}, S) ->
    S2 = S#state{visible_n = 0, hidden_n = 0, last_key = null_ts(),
		 keys = queue_new(), queue = queue_new()},
    S3 = display_clear(TabSize, S2),
    noreply(S3);
handle_info({gs, Button, click, Data, Other} = Click, S) ->
    CollectorPid = S#state.collector_pid,
    case Button of
        close ->
            unlink(S#state.parent_pid),
            gs:destroy(S#state.win),
            {stop, normal, S};
        suspended ->
            case Other of
                [Text, Group, Bool | _] when Bool == true ->
                    S2 = do_suspend(S),
                    noreply(S2);    
                [Text, Group, Bool | _] when Bool == false ->
                    S2 = do_resume(S),
                    noreply(S2);
                Bad ->
                    click_error(Click),
                    noreply(S)
            end;
        hide_actions ->
            case Other of
                [Text, Group, Bool | _] when Bool == true ->
                    S2 = S#state{hide_actions = Bool},
                    noreply(S2);    
                [Text, Group, Bool | _] when Bool == false ->
                    S2 = S#state{hide_actions = Bool},
                    noreply(S2);
                Bad -> 
                    click_error(Click),
                    noreply(S)
            end;
        clear ->
            S2 = S#state{visible_n = 0,
			 last_key = null_ts(),
			 keys = queue_new(),
                         hidden_n  = S#state.visible_n},
            S3 = clear_canvas(queue_length(S#state.queue), S2),
            noreply(S3);
        refresh ->
            S2 = do_refresh(S),
            noreply(S2);
        close_global ->
            unlink(S#state.parent_pid),
            unlink(S#state.collector_pid),
            spawn(event_collector, stop_global, []),
            gs:destroy(S#state.win),
            {stop, normal, S};
        close_all ->
            unlink(S#state.parent_pid),
            unlink(S#state.collector_pid),
            spawn(event_collector, stop, [CollectorPid]),
            {stop, normal, S};
        close_all_others ->
	    C = CollectorPid,
            Fun =
                fun(Sub) ->
                        P = Sub#subscriber.pid,
                        if
                            P == S#state.parent_pid ->
                                unlink(P),
				event_collector:unsubscribe(C, P, shutdown);
                            P == self() ->
                                ignore;
                            true ->
				event_collector:unsubscribe(C, P, shutdown)
                        end
                end,
            Subs = event_collector:subscribers(CollectorPid),
            lists:foreach(Fun, Subs),
            noreply(S);
        clear_all ->
            event_collector:clear(CollectorPid),
            noreply(S);
        refresh_all ->
            All = event_collector:subscribers(CollectorPid),
            [event_collector:refresh(CollectorPid, Sub#subscriber.pid) ||
                Sub <- All, Sub#subscriber.pid /= self()],
            S2 = do_refresh(S),
            noreply(S2);
        load_all ->
            event_collector:load(CollectorPid, "event_viewer.log"),
            noreply(S);
        save_all ->
            event_collector:save(S#state.collector_pid,
                                 "event_viewer.log",
                                 [existing, write, keep]),
            noreply(S);
        select_filter ->
	    create_filter_menu(S#state.win, S#state.filter, S#state.filters),
            noreply(S);
        {open_tool, Display, Scale} ->
	    open_tool(Display, Scale, S#state.filter, S#state.actors, S),
            noreply(S);
        _Level when Data == detail_level, integer(hd(Other)),
                    hd(Other) >= ?detail_level_min,
                    hd(Other) =< ?detail_level_max ->
            S2 = S#state{detail_level = hd(Other)},
            noreply(S2);
        _PopupMenuItem when record(Data, filter) ->
	    open_tool(graphic, S#state.scale, Data, [], S),
            noreply(S);
        Nyi ->
            click_error(Click),
            noreply(S)
    end;
handle_info({gs, Obj, destroy,_, _}, S) ->
    unlink(S#state.parent_pid),
    gs:destroy(S#state.win),
    {stop, normal, S};
handle_info({gs, Obj, buttonpress, _, [Button, X, Y | _]}, S) ->
    S2 =
        case y_to_n(Y, S) of
            0 ->
                %% Actor click
                case S#state.actors of
                    [] ->
                        S;
                    Actors ->
                        N = x_to_n(X, S),
                        A = lists:nth(N, S#state.actors),
                        S#state{selected_actor = A}
                end;
            N when integer(N), N > 0 ->
                %% Event click
		display_n(N, S)
        end,
    noreply(S2);
handle_info({gs, Obj, buttonrelease, _, [Button, X, Y | _]}, S) ->
    CollectorPid = S#state.collector_pid,
    S2 =
        case y_to_n(Y, S) of
            0 ->
                %% Actor click
                case S#state.actors of
                    [] ->
                        S;
                    Actors ->
                        N = x_to_n(X, S),
                        New = lists:nth(N, S#state.actors),
                        Old = S#state.selected_actor,
                        case New#actor.name == Old#actor.name of
                            true ->
                                ok = io:format("~p: ~p~n",
                                               [?MODULE,
                                                New#actor.string]),
                                S#state{selected_actor = make_ref()};
                            false ->
                                ok = io:format("~p: ~p -> ~p~n",
                                               [?MODULE,
                                                Old#actor.string,
                                                New#actor.string]),
                                Actors2 = move_actor(Old, New, Actors, CollectorPid),
                                S#state{selected_actor = make_ref(),
                                        actors = Actors2}
                        end
                end;
            N when integer(N), N > 0 ->
                %% Event click ignored
		S
        end,
    noreply(S2);
handle_info({gs, Obj, keypress, _, [KeySym, Keycode, Shift, Control | _]} = Key, S) ->
    io:format("KEY: ~p~n", [Key]),
    noreply(S);
handle_info({gs, _Obj,configure, [], [W, H | _]}, S) ->
    gs:config(S#state.packer, [{width, W},{height, H}]),
    S2 = S#state{width = W, height = H},
    noreply(S2);
handle_info(timeout, S) ->
    S2 = display_queue(S),
    noreply(S2);
handle_info({'EXIT', Pid, Reason}, S) ->
    if
        Pid == S#state.collector_pid ->
            unlink(S#state.parent_pid),
            unlink(Pid),
            {stop, Reason, S};
        Pid == S#state.parent_pid ->
            unlink(Pid),
            {stop, Reason, S};
        true ->
            noreply(S)
    end;
handle_info(Info, S) ->
    ok = error_logger:format("~p(~p): handle_info(~p, ~p)~n",
                             [?MODULE, self(), Info, S]),
    noreply(S).

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, S) ->
    ignore.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(OldVsn, S, Extra) ->
    {ok, S}.

%%%----------------------------------------------------------------------
%%% Handle suspend/resume
%%%----------------------------------------------------------------------

reply(Reply, S) ->
    case S#state.is_suspended of
        true ->
            {reply, Reply, S, infinity};
        false ->
            case queue_length(S#state.queue) of
                0 ->
                    {reply, Reply, S, infinity};
                1 ->
                    S2 = display_queue(S),
                    {reply, Reply, S2, infinity};
                _ ->
                    S2 = display_queue(S),
                    {reply, Reply, S2, 0}
            end
    end.

noreply(S) ->
    case S#state.is_suspended of
        true ->
            {noreply, S, infinity};
        false ->
            case queue_length(S#state.queue) of
                0 ->
                    {noreply, S, infinity};
                1 ->
                    S2 = display_queue(S),
                    {noreply, S2, infinity};
                _ ->
                    S2 = display_queue(S),
                    {noreply, S2, 0}
            end
    end.

do_suspend(S) ->
    config_suspend(S#state{is_suspended = true}).

do_resume(S) ->
    config_suspend(S#state{is_suspended = false}).

config_suspend(S) ->
    Suspended = S#state.is_suspended,
    gs:config(refresh, [{enable, not Suspended}]),
    gs:config(refresh_all, [{enable, not Suspended}]),
    S.

do_refresh(S) ->
    event_collector:refresh(S#state.collector_pid, self()),
    S.

%%%----------------------------------------------------------------------
%%% String padding of actors
%%%----------------------------------------------------------------------

new_actors(Event, S) ->
    From = Event#event.from,
    To = Event#event.to,
    Actors = S#state.actors,
    case From == To of
        true ->
            new_actor(From, Actors);
        false ->
            case new_actor(From, Actors) of
                [] -> new_actor(To, Actors);
                [New] -> [New | new_actor(To, Actors)]
            end
    end.

new_actor(Name, Actors) ->
    case lists:keymember(Name, #actor.name, Actors) of
        true -> [];
        false -> [init_actor(Name)]
    end.

init_actor(Name) ->
    String = name_to_string(Name),
    PaddedString = pad_string(String, 8),
    #actor{name = Name, length = length(PaddedString), string = PaddedString}.

name_to_string(Name) ->
    case catch io_lib:format("~s", [Name]) of
        {'EXIT', _} -> lists:flatten(io_lib:format("~w", [Name]));
        GoodString  -> lists:flatten(GoodString)
    end.

pad_string(String, MinLen) ->
    Len = length(String),
    case Len >= MinLen of
        true ->
            String;
        false ->
            String ++ lists:duplicate(MinLen - Len, $ )
    end.

%%%----------------------------------------------------------------------
%%% Queue management
%%%----------------------------------------------------------------------

queue_new() ->
    {[], []}.

queue_in(X, {In, Out}) ->
    {[X | In], Out}.

queue_out(Q) ->
    case Q of
        {In, [H | Out]} -> {{value, H}, {In, Out}};
        {[], []}        -> {empty, {[], []}};
        {In, _}         -> queue_out({[], lists:reverse(In)})
    end.

queue_to_list({In, Out}) ->
    Out ++ lists:reverse(In).

queue_length({In, Out}) ->
    length(In) + length(Out).

list_to_queue(List) when list(List) ->
    {[], List}.

%%%----------------------------------------------------------------------
%%% Display either as graphics or text
%%%----------------------------------------------------------------------

display_queue(S) ->
    {{value, Event}, Q} = queue_out(S#state.queue),
    S2 = S#state{queue = Q},
    display_named_arrow(Event, S2).

display_init(S) ->
    case S#state.display of
        graphic -> 
            S2 = create_main_window(S),
            {ok, S2};
        text ->
            Prefix = init_actor(pad_string("EventKey", 22)),
            S2 = S#state{actors = [Prefix | S#state.actors]},
            output_actors(S2),
            {ok, S2}
    end.

display_clear(MinN, S) ->
    case S#state.display of
        graphic -> clear_canvas(MinN, S);
        text    -> output_actors(S)
    end.

display_named_arrow(Event, S) ->
    case S#state.display of
        graphic -> draw_named_arrow(Event, S);
        text    -> output_named_arrow(Event, S)
    end.

display_contents(Event, S) ->
    case S#state.display of
        graphic -> create_contents_window(Event, S);
        text    -> output_contents(Event, S)                  
    end.

display_n(N, S) ->
    List = queue_to_list(S#state.keys),
    S2 = S#state{keys = list_to_queue(List)},
    case catch lists:nth(N, List) of
	{'EXIT', _} ->
	    S2;
	Key ->
	    Events = event_collector:lookup(S#state.collector_pid, Key),
	    lists:foldl(fun display_contents/2, S2, Events)
    end.

%%%----------------------------------------------------------------------
%%% Handle graphics
%%%----------------------------------------------------------------------

create_main_window(S) ->
    Scale   = S#state.scale,
    Font    = select_font(Scale),
    GS      = gs:start(),
    W       = S#state.width,
    H       = S#state.height,
    Name    = name_to_string((S#state.filter)#filter.name),
    Title   = name_to_string(S#state.title) ++ " (filter: " ++ Name ++  ")",

    WinOpt  = [{title, Title}, {configure, true}, {width, W}, {height, H}],
    Win     = gs:window(GS, WinOpt),
    Bar     = gs:menubar(Win, []),

    create_this_menu(Bar),
    create_all_menu(Bar),
    create_new_menu(Bar, S),
    config_suspend(S),

    PackerOpt = [{packer_x, [{fixed, 5}, {fixed, 40}, {fixed, 40},
			     {stretch, 1}, {fixed, 5}]},
		 {packer_y, [{fixed, 30}, {fixed, 30},
			     {stretch, 1}, {fixed, 30}]},
		 {x, 0}, {y, 30}],
    Packer = gs:frame(Win, PackerOpt),
    gs:checkbutton(suspended, Packer, [{label,{text,"Freeze"}},
                                       {x, 10}, {y, 0},
                                       {width, 120}, {align, w},
                                       {select, S#state.is_suspended}]),
    gs:checkbutton(hide_actions, Packer, [{label,{text,"Hide Actions"}},
                                          {x, 10}, {y, 20},
                                          {width, 120}, {align, w}]),
    gs:scale(Packer, [{text,"Detail Level"},
                      {range, {?detail_level_min, ?detail_level_max}},
                      {orient, horizontal},
                      {x, 150}, {y, 0}, {height, 65}, {width, 200},
                      {pos, S#state.detail_level}, {data, detail_level}]),
    CanvasW = calc_canvas_width(W, S#state.actors, Scale),
    CanvasH = calc_canvas_height(H, queue_length(S#state.queue), Scale),
    CanOpt = [{pack_xy, {{2, 4}, 3}}, {vscroll, right}, {hscroll, bottom},
              {scrollregion, {2, 2, CanvasW, CanvasH}}],
    Canvas = gs:canvas(Packer, CanOpt),
    gs:config(Canvas, [{buttonpress, true}, {buttonrelease, true}, {keypress, true}]),
    gs:config(Packer, [{width, W}, {height, H}]),
    gs:config(Win, {map, true}),
    S2 = S#state{win = Win, font = Font, packer = Packer,
                 canvas_height = CanvasH, canvas = Canvas,
                 visible_n = 0, hidden_n = 0,
		 last_key = null_ts(), keys = queue_new(),
		 y_pos = ?initial_y * Scale},
    draw_actors(S2).

create_this_menu(Bar) ->
    Button = gs:menubutton(Bar,  [{label, {text, "This"}}]),
    Menu   = gs:menu(Button, []),
    gs:menuitem(close, Menu, [{label, {text, "Close"}}]),
    gs:menuitem(Menu, [{itemtype, separator}]),
    gs:menuitem(clear, Menu, [{label, {text, "Clear   (hide)"}}]),
    gs:menuitem(refresh, Menu, [{label, {text, "Refresh (show)"}}]).

create_all_menu(Bar) ->
    Button = gs:menubutton(Bar,  [{label, {text, "All"}}]),
    Menu = gs:menu(Button, []),
    gs:menuitem(close_global, Menu, [{label, {text, "Close Global"}}]),
    gs:menuitem(Menu, [{itemtype, separator}]),
    gs:menuitem(close_all, Menu, [{label, {text, "Close All"}}]),
    gs:menuitem(close_all_others, Menu, [{label, {text, "Close All Others"}}]),
    gs:menuitem(Menu, [{itemtype, separator}]),
    gs:menuitem(clear_all, Menu, [{label, {text, "Clear All"}}]),
    gs:menuitem(refresh_all, Menu, [{label, {text, "Refresh All"}}]),
    gs:menuitem(Menu, [{itemtype, separator}]),
    gs:menuitem(load_all, Menu, [{label, {text, "Load All (no clear)"}}]),
    gs:menuitem(save_all, Menu, [{label, {text, "Save All"}}]).

create_new_menu(Bar, S) ->
    Scale   = S#state.scale,
    Button = gs:menubutton(Bar,  [{label, {text, "New"}}]),
    Menu   = gs:menu(Button, []),
    N = lists:concat([" (", (S#state.filter)#filter.name, ")"]),
    gs:menuitem(select_filter, Menu,
		[{label, {text, "Open Graphic Tool (select new filter in popup)"}}]),
    gs:menuitem(Menu, [{itemtype, separator}]),
    gs:menuitem({open_tool, graphic, Scale}, Menu,
                [{label, {text, "Open Graphic Tool" ++ N ++ " same scale"}}]),
    gs:menuitem({open_tool, graphic, Scale + 1}, Menu,
                [{label, {text, "Open Graphic Tool" ++ N ++ " larger scale"}}]),
    gs:menuitem({open_tool, graphic, lists:max([1, Scale - 1])}, Menu,
                [{label, {text, "Open Graphic Tool" ++ N ++ " smaller scale"}}]),
    gs:menuitem(Menu, [{itemtype, separator}]),
    gs:menuitem({open_tool, text, Scale}, Menu,
                [{label, {text, "Open Text Tool   " ++ N}}]).

create_filter_menu(Win, Filter, Filters) ->
    Menu = gs:menu(Win, []),
    Filters2 = lists:keysort(#filter.name, Filters),
    Item = fun(F) ->
		   Name = F#filter.name,
		   Label = atom_to_list(Name),
		   gs:menuitem(Menu, [{label, {text, Label}}, {data,F}])
	   end,
    I = Item(Filter),
    gs:config(I, [{enable, false}]),
    gs:menuitem(Menu, [{itemtype, separator}]),
    lists:foreach(Item, Filters2),
    gs:config(Menu, [{post_at, {85, 30}}]),
    Menu.

open_tool(Display, Scale, Filter, Actors, S) ->
    Options = 
	[{parent_pid, S#state.parent_pid},
	 {title, S#state.title},
	 {display, Display},
	 {collector_pid, S#state.collector_pid},
	 {refresh, new},
	 {is_suspended, S#state.is_suspended},
	 {detail_level, S#state.detail_level},
	 {filter, Filter},
	 {keypos, S#state.keypos},
	 {actors, [A#actor.name || A <- Actors]},
	 {scale, Scale},
	 {width,  S#state.width},
	 {height, S#state.height}],
    case start_link(Options) of
	{ok, ToolPid} ->
	    unlink(ToolPid),
	    ok;
	{error, Reason} ->
	    ok = error_logger:format("~p: Failed to start a new window: ~p~n",
				     [?MODULE, Reason])
    end.

clear_canvas(MinN, S) ->
    Scale = S#state.scale,
    W     = S#state.width,
    H     = S#state.height,
    N     = S#state.visible_n + S#state.hidden_n + MinN + queue_length(S#state.queue),
    gs:destroy(S#state.canvas),
    CanvasW = calc_canvas_width(W, S#state.actors, Scale),
    CanvasH = calc_canvas_height(H, N, Scale),
    CanOpt = [{pack_xy, {{2, 4}, 3}}, {vscroll, right}, {hscroll, bottom},
              {scrollregion, {2, 2, CanvasW, CanvasH}}],
    Canvas = gs:canvas(S#state.packer, CanOpt),
    gs:config(S#state.packer, [{width, W}, {height, H}]), 
    gs:config(Canvas, [{buttonpress, true}, {buttonrelease, true}, {keypress, true}]),
    S2 = S#state{y_pos = ?initial_y * Scale,
                 canvas_height = CanvasH, canvas = Canvas},
    draw_actors(S2).

calc_canvas_width(WinW, Actors, Scale) ->
    CanvasW = ((2 * ?initial_x) + (length(Actors) * ?incr_x)) * Scale,
    lists:max([CanvasW, WinW - (15 * Scale)]).

calc_canvas_height(WinH, N, Scale) ->
    CanvasH = ((2 * ?initial_y) + (N * ?incr_y)) * Scale,
    lists:max([CanvasH, WinH - (15 * Scale)]) * 3.

draw_actors(S) ->
    Scale = S#state.scale,
    Y = ?initial_y * Scale,
    draw_actors(S#state.actors, ?initial_x * Scale, Y, Y + (?incr_y * Scale / 2), S).

draw_actors([H | T], X, Y, LineY, S) ->
    Scale  = S#state.scale,
    X2 = X - (5 * Scale),
    write_text(H#actor.string, X2, Y, red, S),
    CanOpt = [{coords, [{X, LineY},
                        {X, LineY + (10 * S#state.canvas_height * Scale)}]},
              {width, 1}, {fg, blue}],
    gs:line(S#state.canvas, CanOpt),
    draw_actors(T, X + (?incr_x * Scale), Y, LineY, S);
draw_actors([], X, Y, LineY, S) ->
    S#state{y_pos = LineY}.

write_text(Text, X, Y, Colour, S) ->
    Opt = [{coords, [{X, Y - (?incr_y * S#state.scale / 2)}]},
           {font, S#state.font}, {fg, Colour}, {text, Text}],
    gs:text(S#state.canvas, Opt).

select_font(Scale) when integer(Scale) ->
    case Scale of
        1 -> {courier,  7};
        2 -> {courier, 10};
        3 -> {courier, 12};
        4 -> {courier, 14};
        S -> {courier, S * 4}
    end.

draw_named_arrow(Event, S) ->
    Scale   = S#state.scale,
    Actors  = S#state.actors,
    FromPos = calc_pos(Event#event.from, Actors, ?initial_x * Scale, Scale),
    ToPos   = calc_pos(Event#event.to,   Actors, ?initial_x * Scale, Scale),
    N       = S#state.visible_n + 1,
    LastKey = S#state.last_key,
    Key     = element(key_to_pos(S#state.keypos), Event),
    S2      = S#state{y_pos     = S#state.y_pos + (?incr_y * Scale),
		      last_key  = lists:max([Key, LastKey]),
		      keys      = queue_in(Key, S#state.keys),
		      visible_n = N},
    case new_actors(Event, S2) of
        [] ->
            Name = name_to_string(Event#event.label),
            draw_arrow(FromPos, ToPos, S2),
            draw_name(Name, FromPos, ToPos, S2, Key == LastKey);
        NewActors ->
            event_collector:refresh(S2#state.collector_pid, self()),
            S2#state{actors = S2#state.actors ++ NewActors}
    end.

calc_pos(Name, [H | T], Pos, Scale) ->
    case H#actor.name == Name of
        true  -> Pos;
        false -> calc_pos(Name, T, Pos + (?incr_x * Scale), Scale)
    end;
calc_pos(Name, [], Pos, Scale) ->
    Pos.

move_actor(From, To, Actors, CollectorPid) ->
    Pos      = #actor.name,
    ToName   = To#actor.name,
    FromName = From#actor.name,
    ToIx     = keyindex(ToName, Pos, Actors),
    FromIx   = keyindex(FromName, Pos, Actors),
    if
        FromIx /= 0, ToIx /= 0, ToIx > FromIx ->
            event_collector:refresh(CollectorPid, self()),
            Actors2  = lists:keydelete(FromName, Pos, Actors),
            move_actor_after(From, To, Actors2);
        FromIx /= 0, ToIx /= 0 ->
            event_collector:refresh(CollectorPid, self()),
            Actors2  = lists:keydelete(FromName, Pos, Actors),
            move_actor_before(From, To, Actors2);
        true ->
            %% Ignore
            Actors
    end.

move_actor_after(From, To, [H | T]) ->
    case To#actor.name == H#actor.name of
        true  -> [H, From | T];
        false -> [H | move_actor_after(From, To, T)]
    end;
move_actor_after(From, To, []) ->
    [].

move_actor_before(From, To, [H | T]) ->
    case To#actor.name == H#actor.name of
        true  -> [From, H | T];
        false -> [H | move_actor_before(From, To, T)]
    end;
move_actor_before(From, To, []) ->
    [].

keyindex(Key, Pos, []) ->
    0;
keyindex(Key, Pos, [H | T]) ->
    case Key == element(Pos, H) of
        false -> keyindex(Key, Pos, T) + 1;
        true -> 1
    end.

draw_arrow(Pos, Pos, S) ->
    S;
draw_arrow(FromPos, ToPos, S) ->
    Y = S#state.y_pos,
    CanOpts = [{coords, [{FromPos , Y}, {ToPos, Y}]},
               {arrow, last},{width, 1}, {fg, black}],
    gs:line(S#state.canvas, CanOpts),
    S.

draw_name(Name, FromPos, ToPos, S, true) ->
    do_draw_name(Name, FromPos, ToPos, S, green);
draw_name(Name, Pos, Pos, S, false) ->
    do_draw_name(Name, Pos, Pos, S, blue);
draw_name(Name, FromPos, ToPos, S, false) ->
    do_draw_name(Name, FromPos, ToPos, S, red).

do_draw_name(Name, FromPos, ToPos, S, Colour) ->
    Scale = S#state.scale,
    X = lists:min([FromPos, ToPos]) + (6 * Scale),
    Y = S#state.y_pos,
    write_text(Name, X, Y, Colour, S),
    S.

y_to_n(Y, S) ->
    Scale   = S#state.scale,
    Y2 = Y - (?initial_y * Scale) - (?incr_y * Scale / 2),
    N  = Y2 / (?incr_y * Scale),
    N2 = trunc(N + 0.5) + S#state.hidden_n,
    MaxN = S#state.visible_n + S#state.hidden_n,
    if
        N2 > MaxN -> 0;
        true      -> N2
    end.

x_to_n(X, S) ->
    Scale   = S#state.scale,
    Len = length(S#state.actors),
    X2 = X - (?initial_x * Scale),
    N  = X2 / (?incr_x * Scale),
    N2 = trunc(N + 1.5),
    if
        N2 > Len -> Len;
        N2 < 1   -> 1;
        true     -> N2
    end.

create_contents_window(Event, S) ->
    Options = [{event, Event},
               {keypos, S#state.keypos},
               {filter, S#state.filter},
               {filters, S#state.filters}],
    case event_contents_viewer:start_link(Options) of
        {ok, Pid} ->
            S;
        {error, Reason} ->
            ok = error_logger:format("~p(~p): create_contents_window(~p) ->~n     ~p~n",
                                     [?MODULE, self(), Options, Reason]),
            S
    end.

output_actors(S) ->
    Delim = "========================================",
    ok = io:format(user, "~n~s~s~n~p(~p): ~p~n",
                   [Delim, Delim, ?MODULE, self(), S#state.title]),
    DashedLine = do_output_actors(S#state.actors),
    ok = io:format(user, "~n+~s~n", [DashedLine]),
    S.

do_output_actors([H | T]) ->
    ok = io:format(user, "~s ", [H#actor.string]),
    lists:duplicate(H#actor.length,  $-) ++ [$+ | do_output_actors(T)];
do_output_actors([]) ->
    [].

output_named_arrow(Event, S) ->
    Label = Event#event.label,
    S3 = 
        case new_actors(Event, S) of
            [] ->
                S;
            NewActors ->
                S2 = S#state{actors = S#state.actors ++ NewActors},
                output_actors(S2),
                S2
        end,
    Actors = S3#state.actors,
    Prefix = hd(Actors),
    FromPos = calc_output_pos(Event#event.from, Actors, 1),
    ToPos   = calc_output_pos(Event#event.to, Actors, 1),
    LeftPos = lists:min([FromPos, ToPos]),
    Key = name_to_string(element(key_to_pos(S3#state.keypos), Event)),
    PaddedKey  = pad_string(Key, Prefix#actor.length),
    PaddedKey2 = pad_string(PaddedKey ++ " +", LeftPos),
    ok = io:format(user, "~s~p~n", [PaddedKey2, Event#event.label]),
    output_arrow(FromPos, ToPos),
    S3.

key_to_pos(reported_ts) -> #event.reported_ts;
key_to_pos(parsed_ts)   -> #event.parsed_ts.

calc_output_pos(Name, [H | T], Pos) ->
    case H#actor.name == Name of
        true  -> Pos;
        false -> calc_output_pos(Name, T, Pos + H#actor.length + 1)
    end;
calc_output_pos(Name, [], Pos) ->
    Pos.

output_contents(Event, S) ->
    String = event_contents_viewer:event_to_string(Event, S#state.keypos),
    ok = io:format(user, "~n~s~n", String),
    S.

output_arrow(FromPos, ToPos) ->
    if
        FromPos < ToPos ->
            output_blanks(FromPos - 1),
            output_chars("-", ToPos-FromPos),
            output_chars(">");
        FromPos == ToPos ->
            output_blanks(FromPos - 1),
            output_chars("<->");
        FromPos > ToPos ->
            output_blanks(ToPos - 1),
            output_chars("<"),
            output_chars("-", FromPos - ToPos)
    end,
    output_newline().

output_chars(C) ->
    output_chars(C, 1).

output_chars([C | Cs], N) when N =< 0 ->
    output_chars(Cs, N + 1);
output_chars([], _) ->
    ok;
output_chars(C, N) ->
    io:put_chars(user, lists:duplicate(N, C)).

output_blanks(N) ->
    output_chars($ , N).

output_newline() ->
    io:nl(user).

click_error(Click) ->
    ok = error_logger:format("~p: click ignored (nyi): ~p~n",
                             [?MODULE, Click]).

