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
%% Purpose: Displays details of a trace event
%%----------------------------------------------------------------------

-module(event_contents_viewer).

-behaviour(gen_server).

%% External exports
-export([start_link/1, stop/1, event_to_string/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("event_tracer.hrl").

-record(state, {parent_pid, keypos, event, filter, filters,
                win, packer, width, height}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% Start a detail viewer
%% Returns {ok, Pid} | {error, Reason}
start_link(Options) -> 
    case parse_opt(Options, default_state()) of
        {ok, S} ->
            gen_server:start_link(?MODULE, [S], []);
        {error, Reason} ->
            {error, Reason}
    end.

default_state() ->
    #state{parent_pid = self(),
           filter     = unfiltered,
           filters    = [],
           width      = 600,
           height     = 300}.

parse_opt([], S) ->
    F = S#state.filter,
    Filters = S#state.filters,
    case S#state.event == undefined of
        true  ->
	    {error, {badarg, no_event}};
        false when atom(F) ->
	    case lists:keysearch(F, #filter.name, Filters) of
		{value, Filter} ->
		    {ok, S#state{filter = Filter}};
		false ->
		    {badarg, {no_such_filter, S#state.filter}}
	    end;
	false when record(F, filter),
		   atom(F#filter.name),
		   function(F#filter.function) ->
	    {ok, S}
    end;
parse_opt([H | T], S) ->
    case H of
        {parent_pid, Parent} when pid(Parent) ->
            parse_opt(T, S#state{parent_pid = Parent});
        {keypos, reported_ts} ->
            parse_opt(T, S#state{keypos = reported_ts});
        {keypos, parsed_ts} ->
            parse_opt(T, S#state{keypos = parsed_ts});
        {event, Event} when record(Event, event) ->
            parse_opt(T, S#state{event = Event});
        {filter, Name} when atom(Name) ->
            parse_opt(T, S#state{filter = Name});
        {filter, F} when record(F, filter),
			 atom(F#filter.name),
			 function(F#filter.function) ->
            parse_opt(T, S#state{filter = F});
        {filters, Filters} when list(Filters) ->
	    Bad = fun(F) when record(F, filter),
			      atom(F#filter.name),
			      function(F#filter.function) ->
			  false;
		     (_) ->
			  true
		  end,
	    case [Filter || Filter <- Filters, Bad(Filter)] of
		[] ->
		    parse_opt(T, S#state{filters = Filters});
		BadFilters ->
		    {error, {bad_option, BadFilters}}
	    end;
        {width, Width} when integer(Width), Width > 0 ->
            parse_opt(T, S#state{width = Width});
        {height, Height} when integer(Height), Height > 0 ->
            parse_opt(T, S#state{height = Height});
        Bad ->
            {error, {bad_option, Bad}}
    end;
parse_opt(BadList, S) ->
    {error, {bad_option_list, BadList}}.

stop(ViewerPid) ->
    call(ViewerPid, stop).

call(ViewerPid, Request) ->
    gen_server:call(ViewerPid, Request, infinity).

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
    S2 = create_window(S),
    {ok, S2}.

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(stop, From, S) ->
    unlink(S#state.parent_pid),
    {stop, shutdown, S};
handle_call(Request, From, S) ->
    ok = error_logger:format("~p(~p): handle_call(~p, ~p, ~p)~n",
                             [?MODULE, self(), Request, From, S]),
    Reply = {error, {bad_request, Request}},
    {reply, Reply, S}.

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
handle_info({gs, Button, click, Data, Other}, S) ->
    case Button of
        close ->
            gs:destroy(S#state.win),
            {stop, normal, S};
        save ->
            Event = S#state.event,
            Bin = list_to_binary(event_to_string(Event, S#state.keypos)),
            TimeStamp = 
                case S#state.keypos of
                    reported_ts -> Event#event.reported_ts;
                    parsed_ts   -> Event#event.parsed_ts
                end,
	    FileName = ["event_contents_viewer_", now_to_string(TimeStamp), ".save"],
            file:write_file(lists:flatten(FileName), Bin),
            {noreply, S};
        _PopupMenuItem when record(Data, filter) ->
	    F = Data,
	    OldF = S#state.filter,
	    Pos = #filter.name,
	    Filters = lists:keydelete(F#filter.name, Pos, S#state.filters),
	    %% Filters2 = lists:keydelete(OldF#filter.name, Pos, Filters),
	    %% S2 = S#state{filter = F, filters = [OldF | Filters2]},
	    S2 = S#state{filter = F},
	    gen_server:start_link(?MODULE, [S2], []),
            {noreply, S2};
        Nyi ->
            ok = error_logger:format("~p: click ~p ignored (nyi)~n",
                                     [?MODULE, Nyi]),
            {noreply, S}
    end;
handle_info({gs, Obj, destroy,_, _}, S) ->
    unlink(S#state.parent_pid),
    gs:destroy(S#state.win),
    {stop, normal, S};
handle_info({gs, _Obj,configure, [], [W, H | _]}, S) ->
    gs:config(S#state.packer, [{width, W},{height, H}]),
    S2 = S#state{width = W, height = H},
    {noreply, S2};
handle_info({'EXIT', Pid, Reason}, S) ->
    if
        Pid == S#state.parent_pid ->
            unlink(Pid),
            {stop, Reason, S};
        true ->
            {noreply, S}
    end;
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
    ignore.

%%----------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%%----------------------------------------------------------------------
code_change(OldVsn, S, Extra) ->
    {ok, S}.

%%%----------------------------------------------------------------------
%%% Handle graphics
%%%----------------------------------------------------------------------

create_window(S) ->
    H = S#state.height,
    W = S#state.width,
    FilterName = (S#state.filter)#filter.name,
    Title = lists:concat([?MODULE, " (filter: ", FilterName, ")"]),
    WinOpt = [{title, Title}, {configure, true},
	      {width, W}, {height, H}],
    GS  = gs:start(),
    Win = gs:window(GS, WinOpt),
    Bar = gs:menubar(Win, []),
    create_file_menu(Bar),
    create_filter_menu(Bar, S#state.filter, S#state.filters),
    PackerOpt = [{packer_x, [{stretch, 1}]},
		 {packer_y, [{stretch, 1}, {fixed, 25}]},
		 {x, 0}, {y, 25}],
    Packer = gs:frame(Win, PackerOpt),
    EditorOpt = [{pack_xy, {1, 1}}, {vscroll, right}, {hscroll, bottom},
		 {wrap, none},
                 {bg, lightblue},  {font, {courier, 12}}],
    Editor = gs:editor(Packer, EditorOpt),
    config_editor(Editor, S),
    gs:config(Packer, [{width, W}, {height, H}]),
    gs:config(Win, {map,true}),
    S#state{win = Win, packer = Packer}.

create_file_menu(Bar) ->
    Button = gs:menubutton(Bar,  [{label, {text, "File"}}]),
    Menu  = gs:menu(Button, []),
    gs:menuitem(close, Menu, [{label, {text,"Close"}}]),
    gs:menuitem(save, Menu, [{label, {text,"Save"}}]).

create_filter_menu(Bar, Filter, Filters) ->
    Filters2 = lists:keysort(#filter.name, Filters),
    Button = gs:menubutton(Bar, [{label, {text, "Filters"}}]),
    Menu  = gs:menu(Button, []),
    Item = fun(F) ->
		   Name = F#filter.name,
		   Label = atom_to_list(Name),
		   gs:menuitem(Menu, [{label, {text, Label}}, {data,F}])
	   end,
    I = Item(Filter),
    gs:config(I, [{enable, false}]),
    gs:menuitem(Menu, [{itemtype, separator}]),
    lists:foreach(Item, Filters2),
    Menu.

config_editor(Editor, S) ->
    Event = S#state.event,
    F = S#state.filter,
    FilterFun = F#filter.function,
    case catch FilterFun(Event) of
	true ->
	    do_config_editor(Editor, Event, lightblue, S#state.keypos);
	{true, Event2} when record(Event2, event) ->
	    do_config_editor(Editor, Event2, lightblue, S#state.keypos);
	Bad ->
	    Name = F#filter.name,
	    BadEvent = Event#event{contents = {bad_filter, Name, Bad}},
	    do_config_editor(Editor, BadEvent, red, S#state.keypos)
    end.

do_config_editor(Editor, Event, Colour, KeyPos) ->
    String = event_to_string(Event, KeyPos),
    gs:config(Editor, {insert, {'end', String}}),
    gs:config(Editor, {enable, false}),
    gs:config(Editor, {bg, Colour}).

%%%----------------------------------------------------------------------
%%% String handling
%%%----------------------------------------------------------------------

term_to_string(Term) ->
    case catch io_lib:format("~s", [Term]) of
        {'EXIT', _} -> io_lib:format("~p", [Term]);
        GoodString  -> GoodString
    end.

now_to_string({Mega, Sec, Micro} = Now)
  when integer(Mega), integer(Sec), integer(Micro) ->
    {{Y, Mo, D}, {H, Mi, S}} = calendar:now_to_universal_time(Now),
    lists:concat([Y, "-", Mo, "-", D, " ", H, ".", Mi, ".", S, ".", Micro]);
now_to_string(Other) ->
    term_to_string(Other).

event_to_string(Event, KeyPos) ->
    ReportedTs = Event#event.reported_ts,
    ParsedTs   = Event#event.parsed_ts,
    Deep = 
        ["DETAIL LEVEL: ", term_to_string(Event#event.detail_level),
         "\nLABEL:        ", term_to_string(Event#event.label),
         case Event#event.from == Event#event.to of
             true ->
                 ["\nACTOR:        ", term_to_string(Event#event.from)];
             false ->
                 ["\nFROM:         ", term_to_string(Event#event.from),
                  "\nTO:           ", term_to_string(Event#event.to)]
         end,
         case ReportedTs == ParsedTs of
             true ->
                 ["\nPARSED:       ", now_to_string(ParsedTs)];
             false ->
                 case KeyPos of
                     reported_ts ->
                         ["\nREPORTED:     ", now_to_string(ReportedTs),
                          "\nPARSED:       ", now_to_string(ParsedTs)];
                     parsed_ts ->
                         ["\nPARSED:       ", now_to_string(ParsedTs),
                          "\nREPORTED:     ", now_to_string(ReportedTs)]
                 end
         end,
         "\nCONTENTS:\n\n", term_to_string(Event#event.contents)],
    lists:flatten(Deep).

