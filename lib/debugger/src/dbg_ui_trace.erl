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
-module(dbg_ui_trace).

%% External exports
-export([start/1, start/3]).
-export([title/1]).

-define(TRACEWIN, ['Button Area', 'Evaluator Area', 'Bindings Area']).
-define(BACKTRACE, 100).

-record(state, {gs,                % term() Graphics system id
		win,               % term() Attach process window data
		coords,            % {X,Y} Mouse point position

		pid,               % pid() Debugged process
		meta,              % pid() Meta process
		status,            % {St,Mod,Line} | idle | running
		                   % St = exit | break | wait

		cm,                % atom() | undefined Current module
		cm_obsolete=false, % boolean() Curr module needs reloading

		stack,             % {Cur,Max}

		trace,             % boolean()
		stack_trace,       % all | no_tail | false
		backtrace          % integer() No. of call frames to fetch
	       }).

%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% start(Pid)
%% start(Pid, TraceWin, BackTrace)
%%   Pid = pid()
%%   TraceWin = [WinArea]
%%     WinArea = 'Button|Evaluator|Bindings|Trace Area'
%%   Backtrace = integer()
%%--------------------------------------------------------------------
start(Pid) -> % Used by debugger:quick/3 (no monitor)
    start(Pid, ?TRACEWIN, ?BACKTRACE).
start(Pid, TraceWin, BackTrace) ->

    %% Inform int about my existence and get the meta pid back
    case int:attached(Pid) of
	{ok, Meta} ->
	    init(Pid, Meta, TraceWin, BackTrace);
	error ->
	    ignore
    end.

%%--------------------------------------------------------------------
%% title(Pid) -> string()
%% By exporting this function, dbg_ui_mon may check with dbg_ui_winman
%% if there already is an attach window for a given pid and thus avoid
%% spawning processes unnecessarily.
%%--------------------------------------------------------------------
title(Pid) ->
    "Attach Process " ++ pid_to_list(Pid).


%%====================================================================
%% Main loop and message handling
%%====================================================================

init(Pid, Meta, TraceWin, BackTrace) ->

    %% Start necessary stuff
    GS = dbg_ui_trace_win:init(),               % Graphics system

    %% Create attach process window
    Title = title(Pid),
    Win = dbg_ui_trace_win:create_win(GS, Title, TraceWin, menus()),
    Window = dbg_ui_trace_win:get_window(Win),
    dbg_ui_winman:insert(Title, Window),

    %% Initial process state
    State1 = #state{gs=GS, win=Win, coords={0,0}, pid=Pid, meta=Meta,
		    status={idle,null,null},
		    stack={1,1}},

    State2 = init_options(TraceWin,
			  int:stack_trace(),    % Stack Trace
			  BackTrace,            % Back trace size
			  State1),

    State3 = init_contents(int:all_breaks(),    % Breakpoints
			   State2),

    int:meta(Meta, trace, State3#state.trace),

    gui_enable_functions({first,null,null}, {idle,null,null}),
    gui_enable_updown(State3#state.stack_trace, State3#state.stack, bottom),
    gui_enable_btrace(State3#state.trace, State3#state.stack_trace),
    dbg_ui_trace_win:display(idle),
    
    loop(State3).

init_options(TraceWin, StackTrace, BackTrace, State) ->
    lists:foreach(fun(Area) -> dbg_ui_trace_win:select(Area, true) end,
		  TraceWin),

    Trace = lists:member('Trace Area', TraceWin),

    dbg_ui_trace_win:select(map(StackTrace), true),

    %% Backtrace size is (currently) not shown in window

    State#state{trace=Trace, stack_trace=StackTrace, backtrace=BackTrace}.

init_contents(Breaks, State) ->
    Win =
	lists:foldl(fun(Break, Win) ->
			    dbg_ui_trace_win:add_break(Win, 'Break', Break)
		    end,
		    State#state.win,
		    Breaks),

    State#state{win=Win}.

loop(#state{meta=Meta} = State) ->
    receive

	%% From the GUI main window
	GuiEvent when tuple(GuiEvent), element(1, GuiEvent)==gs ->
	    Cmd = dbg_ui_trace_win:handle_event(GuiEvent, State#state.win),
	    State2 = gui_cmd(Cmd, State),
	    loop(State2);

	%% From the GUI help windows
	{gui, Cmd} ->
	    State2 = gui_cmd(Cmd, State),
	    loop(State2);

	%% From the interpreter
	{int, Cmd} ->
	    State2 = int_cmd(Cmd, State),
	    loop(State2);

	%% From the meta process
	{Meta, Cmd} ->
	    State2 = meta_cmd(Cmd, State),
	    gui_enable_functions(State#state.status, State2#state.status),
	    loop(State2);
	{NewMeta, {exit_at, Where, Reason, Cur}} ->
	    State2 = meta_cmd({exit_at, Where, Reason, Cur},
			      State#state{meta=NewMeta}),
	    gui_enable_functions(State#state.status, State2#state.status),
	    loop(State2);

	%% From the dbg_ui_edit process
	{dbg_ui_edit, 'Backtrace:', BackTrace}  ->
	    loop(State#state{backtrace=BackTrace});
	{dbg_ui_edit, Var, Val} ->
	    Cmd = atom_to_list(Var)++"="++io_lib:format("~p", [Val]),
	    State2 = gui_cmd({user_command, lists:flatten(Cmd)}, State),
	    loop(State2);

	%% From the dbg_ui_winman process (Debugger window manager)
	{dbg_ui_winman, update_windows_menu, Data} ->
	    dbg_ui_winman:update_windows_menu(Data),
	    loop(State);
	{dbg_ui_winman, destroy} ->
	    exit(stop)
    end.

%%--Commands from the GUI---------------------------------------------

gui_cmd(ignore, State) ->
    State;
gui_cmd({win, Win}, State) ->
    State#state{win=Win};
gui_cmd(stopped, State) ->
    exit(stop);
gui_cmd({coords, Coords}, State) ->
    State#state{coords=Coords};

gui_cmd({shortcut, Key}, State) ->
    case shortcut(Key) of
	{always, Cmd} -> gui_cmd(Cmd, State);
	{if_enabled, Cmd} ->
	    case dbg_ui_trace_win:is_enabled(Cmd) of
		true -> gui_cmd(Cmd, State);
		false -> State
	    end;
	false -> State
    end;

%% File menu
gui_cmd('Close', State) ->
    gui_cmd(stopped, State);

%% Edit menu
gui_cmd('Go To Line...', State) ->
    %% Will result in message handled below: {gui, {gotoline, Line}}
    dbg_ui_trace_win:helpwin(gotoline, State#state.win,
			      State#state.gs, State#state.coords),
    State;
gui_cmd({gotoline, Line}, State) ->
    Win = dbg_ui_trace_win:select_line(State#state.win, Line),
    State#state{win=Win};
gui_cmd('Search...', State) ->
    dbg_ui_trace_win:helpwin(search, State#state.win,
			     State#state.gs, State#state.coords),
    State;

%% Process menu
gui_cmd('Step', State) ->
    int:meta(State#state.meta, step),
    State;
gui_cmd('Next', State) ->
    int:meta(State#state.meta, next),
    State;
gui_cmd('Continue', State) ->
    int:meta(State#state.meta, continue),
    State;
gui_cmd('Finish', State) ->
    int:meta(State#state.meta, finish),
    State;
gui_cmd('Skip', State) ->
    int:meta(State#state.meta, skip),
    State;
gui_cmd('Time Out', State) ->
    int:meta(State#state.meta, timeout),
    State;
gui_cmd('Stop', State) ->
    int:meta(State#state.meta, stop),
    State;
gui_cmd('Where', State) ->
    {Cur, Max} = State#state.stack,
    gui_cmd('Down', State#state{stack={Max, Max}});
gui_cmd('Kill', State) ->
    exit(State#state.pid, kill),
    State;
gui_cmd('Messages', State) ->
    case int:meta(State#state.meta, messages) of
	[] ->
	    dbg_ui_trace_win:eval_output("< No Messages!\n", bold);
	Messages ->
	    dbg_ui_trace_win:eval_output("< --- Current Messages ---\n",
					 bold),
	    lists:foldl(
	      fun(Msg, N) ->
		      Str1 = io_lib:format(" ~w:", [N]),
		      dbg_ui_trace_win:eval_output(Str1, bold),
		      Str2 = io_lib:format(" ~s~n", [io_lib:print(Msg)]),
		      dbg_ui_trace_win:eval_output(Str2, normal),
		      N+1
	      end,
	      1,
	      Messages)
    end,
    State;
gui_cmd('Back Trace', State) ->
    lists:foreach(fun({Lev, {Mod, {Func,Arity}, Line, Bs}}) ->
			  Str = io_lib:format("~p > ~p:~p/~p~n",
					      [Lev, Mod, Func, Arity]),
			  dbg_ui_trace_win:trace_output(Str);
		     (_) -> ignore
		  end,
		  int:meta(State#state.meta,
			   backtrace, State#state.backtrace)),
    dbg_ui_trace_win:trace_output("\n"),
    State;
gui_cmd('Up', State) ->
    {Cur, Max} = State#state.stack,
    case int:meta(State#state.meta, stack_frame, {up, Cur}) of
	{New, Mod, Line} ->
	    Stack = {New, Max},
	    Win = gui_show_module(State#state.win, Mod, Line,
				  State#state.cm, State#state.pid, where),
	    gui_update_bindings(State#state.meta, Stack),
	    gui_enable_updown(State#state.stack_trace, Stack, stack),
	    dbg_ui_trace_win:display({New, Mod, Line}),
	    State#state{win=Win, cm=Mod, stack=Stack};
	top -> State
    end;
gui_cmd('Down', State) ->
    {Cur, Max} = State#state.stack,
    {Stack, {Status, Mod, Line}, Where, How} = 
	case int:meta(State#state.meta, stack_frame, {down, Cur}) of
	    {New, M, L} -> {{New, Max}, {New, M, L}, stack, where};
	    bottom -> {{Max, Max}, State#state.status, bottom, break}
	end,
    Win = gui_show_module(State#state.win, Mod, Line,
			  State#state.cm, State#state.pid, How),
    gui_update_bindings(State#state.meta, Stack),
    gui_enable_updown(State#state.stack_trace, Stack, Where),
    dbg_ui_trace_win:display({Status, Mod, Line}),
    State#state{win=Win, cm=Mod, stack=Stack};

%% Break menu
gui_cmd('Line Break...', State) ->
    add_break(State#state.gs, State#state.coords, line,
	      State#state.cm,
	      dbg_ui_trace_win:selected_line(State#state.win)),
    State;
gui_cmd('Conditional Break...', State) ->
    add_break(State#state.gs, State#state.coords, conditional,
	      State#state.cm,
	      dbg_ui_trace_win:selected_line(State#state.win)),
    State;
gui_cmd('Function Break...', State) ->
    add_break(State#state.gs, State#state.coords, function,
	      State#state.cm, undefined),
    State;
gui_cmd('Delete All', State) ->
    int:no_break(State#state.cm),
    State;
gui_cmd({break, {Mod, Line}, What}, State) ->
    case What of
	add -> int:break(Mod, Line);
	delete -> int:delete_break(Mod, Line);
	{status, inactive} -> int:disable_break(Mod, Line);
	{status, active} -> int:enable_break(Mod, Line);
	{trigger, Action} -> int:action_at_break(Mod, Line, Action)
    end,
    State;

%% Options menu
gui_cmd({'Trace Window', TraceWin}, State) ->
    Trace = lists:member('Trace Area', TraceWin),
    int:meta(State#state.meta, trace, Trace),
    Win = dbg_ui_trace_win:configure(State#state.win, TraceWin),
    gui_enable_btrace(Trace, State#state.stack_trace),
    State#state{win=Win, trace=Trace};
gui_cmd({'Stack Trace', [Name]}, State) ->
    int:meta(State#state.meta, stack_trace, map(Name)),
    State;
gui_cmd('Back Trace Size...', State) ->
    dbg_ui_edit:start(State#state.gs, State#state.coords, "Backtrace",
		      'Backtrace:', {integer, State#state.backtrace}),
    State;

%% Help menu
gui_cmd('Debugger', State) ->
    HelpFile = filename:join([code:lib_dir(debugger),"doc","index.html"]),
    tool_utils:open_help(State#state.gs, HelpFile),
    State;

gui_cmd({user_command, Cmd}, State) ->
    {Status, _Mod, _Line} = State#state.status,
    if
	Status==break;
	Status==wait ->
	    Cm = State#state.cm,
	    Arg = case State#state.stack of
		      {Cur, Max} when Cur<Max -> {Cm, Cmd, Cur};
		      _Stack -> {Cm, Cmd}
		  end,

	    %% Reply will be received as {Meta, {eval_rsp, Res}}
	    int:meta(State#state.meta, eval, Arg);
	true ->
	    Str = "Commands not allowed",
	    dbg_ui_trace_win:eval_output([$<,Str,10], normal)
    end,
    State;

gui_cmd({edit, {Var, Val}}, State) ->
    dbg_ui_edit:start(State#state.gs, State#state.coords,
		      "Edit variable", Var, {term, Val}),
    State.

add_break(GS, Coords, Type, undefined, _Line) ->
    dbg_ui_break:start(GS, Coords, Type);
add_break(GS, Coords, Type, Mod, undefined) ->
    dbg_ui_break:start(GS, Coords, Type, Mod);
add_break(GS, Coords, Type, Mod, Line) ->
    dbg_ui_break:start(GS, Coords, Type, Mod, Line).

%%--Commands from the interpreter-------------------------------------

int_cmd({interpret, Mod}, State) ->
    if
	Mod==State#state.cm ->
	    State#state{cm_obsolete=true};
	true ->
	    State
    end;
int_cmd({no_interpret, Mod}, State) ->
    if
	Mod==State#state.cm ->
	    State#state{cm_obsolete=true};
	true ->
	    Win = dbg_ui_trace_win:remove_code(State#state.win, Mod),
	    State#state{win=Win}
    end;

int_cmd({new_break, Break}, State) ->
    Win = dbg_ui_trace_win:add_break(State#state.win, 'Break', Break),
    State#state{win=Win};
int_cmd({delete_break, Point}, State) ->
    Win = dbg_ui_trace_win:delete_break(State#state.win, Point),
    State#state{win=Win};
int_cmd({break_options, Break}, State) ->
    Win = dbg_ui_trace_win:update_break(State#state.win, Break),
    State#state{win=Win};
int_cmd(no_break, State) ->
    Win = dbg_ui_trace_win:clear_breaks(State#state.win),
    State#state{win=Win};
int_cmd({no_break, Mod}, State) ->
    Win = dbg_ui_trace_win:clear_breaks(State#state.win, Mod),
    State#state{win=Win}.

%%--Commands from the meta process------------------------------------

%% Message received when first attached to a living process
%% '_Trace' is a boolean indicating if the process is traced or not --
%% ignore this as we already have ordered tracing or not depending on if
%% the Trace Area is shown or not.
meta_cmd({attached, Mod, Line, _Trace}, State) ->
    Win = if
	      Mod/=null ->
		  gui_show_module(State#state.win, Mod, Line,
				  State#state.cm, State#state.pid);
	      true -> State#state.win
	  end,
    State#state{win=Win, cm=Mod};

%% Message received when returning to interpreted code
meta_cmd({re_entry, Mod, Func}, State) ->
    Obs = State#state.cm_obsolete,
    case State#state.cm of
	Mod when Obs==true ->
	    Win = gui_load_module(State#state.win, Mod, State#state.pid),
	    State#state{win=Win, cm_obsolete=false};
	Mod -> State;
	Cm ->
	    Win = gui_show_module(State#state.win, Mod, 0,
				  Cm, State#state.pid),
	    State#state{win=Win, cm=Mod}
    end;

%% Message received when attached to a terminated process
meta_cmd({exit_at, Where, Reason, Cur}, State) ->
    Stack = {Cur, Cur},
    case Where of
	null ->
	    gui_enable_updown(false, {Cur,Cur}, bottom),
	    dbg_ui_trace_win:display({exit, Reason}),
	    State#state{status={exit, null, null}, stack=Stack};
	{Mod, Line} ->
	    Win = gui_show_module(State#state.win, Mod, Line,
				  State#state.cm, State#state.pid),
	    gui_update_bindings(State#state.meta),
	    gui_enable_updown(State#state.stack_trace, {Cur,Cur}, bottom),
	    dbg_ui_trace_win:display({exit, Mod, Line, Reason}),
	    State#state{win=Win, cm=Mod, status={exit, Mod, Line},
			stack=Stack}
    end;

meta_cmd({break_at, Mod, Line, Cur}, State) ->
    Stack = {Cur,Cur},
    Win = gui_show_module(State#state.win, Mod, Line,
			  State#state.cm, State#state.pid),
    gui_update_bindings(State#state.meta),
    gui_enable_updown(State#state.stack_trace, Stack, bottom),
    dbg_ui_trace_win:display({break, Mod, Line}),
    State#state{win=Win, cm=Mod, status={break, Mod, Line}, stack=Stack};
meta_cmd({func_at, Mod, Line, Cur}, State) ->
    Stack = {Cur,Cur},
    Win = gui_show_module(State#state.win, Mod, Line,
			  State#state.cm, State#state.pid),
    State#state{win=Win, cm=Mod, stack=Stack};
meta_cmd({wait_at, Mod, Line, Cur}, State) ->
    Stack = {Cur,Cur},
    Win = gui_show_module(State#state.win, Mod, Line,
			  State#state.cm, State#state.pid),
    gui_update_bindings(State#state.meta),
    dbg_ui_trace_win:display({wait, Mod, Line}),
    State#state{win=Win, cm=Mod, status={wait, Mod, Line}, stack=Stack};

meta_cmd({wait_after_at, Mod, Line, Sp}, State) ->
    dbg_ui_trace_win:enable('Time Out', true),
    meta_cmd({wait_at, Mod, Line, Sp}, State);

meta_cmd(running, State) ->
    Win = dbg_ui_trace_win:unmark_line(State#state.win),
    dbg_ui_trace_win:update_bindings([]),
    dbg_ui_trace_win:display({running, State#state.cm}),
    State#state{win=Win, status={running,null,null}, stack={1,1}};
meta_cmd(idle, State) ->
    Win = dbg_ui_trace_win:show_no_code(State#state.win),
    dbg_ui_trace_win:update_bindings([]),
    dbg_ui_trace_win:display(idle),
    State#state{win=Win, status={idle,null,null}, cm=undefined, stack={1,1}};

%% Message about changed trace option can be ignored, the change must have
%% been ordered by this process. (In theory, the change could have been
%% ordered by another attached process. Debugger though, allows max one
%% attached process per debugged process).
meta_cmd({trace, Bool}, State) ->
    State;

meta_cmd({stack_trace, Flag}, State) ->
    dbg_ui_trace_win:select(map(Flag), true),
    {_Cur,Max} = State#state.stack,
    gui_enable_updown(Flag, {Max,Max}, bottom),
    gui_enable_btrace(State#state.trace, Flag),
    State#state{stack_trace=Flag};

meta_cmd({trace_output, Str}, State) ->
    dbg_ui_trace_win:trace_output(Str),
    State;

%% Reply on a user command
meta_cmd({eval_rsp, Res}, State) ->
    Str = io_lib:print(Res),
    dbg_ui_trace_win:eval_output([$<,Str,10], normal),
    State.


%%====================================================================
%% GUI auxiliary functions
%%====================================================================

menus() ->
    [{'File', [{'Close', no}]},
     {'Edit', [{'Go To Line...', no},
	       {'Search...', no}]},
     {'Process', [{'Step', 0},
		  {'Next', 0},
		  {'Continue', 0},
		  {'Finish', 0},
		  {'Skip', no},
		  {'Time Out', no},
		  {'Stop', no},
		  separator,
		  {'Kill', no},
		  separator,
		  {'Messages', 0},
		  {'Back Trace', no},
		  separator,
		  {'Where', 0},
		  {'Up', no},
		  {'Down', no}]},
     {'Break', [{'Line Break...', 5},
		{'Conditional Break...', no},
		{'Function Break...', no},
		separator,
		{'Delete All', 0},
		separator]},
     {'Options', [{'Trace Window', no, cascade,
		   [{'Button Area', no, check},
		    {'Evaluator Area', no, check},
		    {'Bindings Area', no, check},
		    {'Trace Area', no, check}]},
		  {'Stack Trace', no, cascade,
		   [{'Stack On, Tail', no, radio},
		    {'Stack On, No Tail', no, radio},
		    {'Stack Off', no, radio}]},
		  {'Back Trace Size...', no}]},
     {'Help', [{'Debugger', no}]}].

enable(new, break) ->
    ['Step','Next','Continue','Finish','Skip', 'Messages'];
enable(new, exit) -> [];
enable(new, running) -> ['Stop'];
enable(new, wait) -> ['Stop'];
enable(new, _Status) -> [];
enable(old, _Status) -> [].

disable(new, exit) -> ['Kill'];
disable(new, _Status) -> [];
disable(old, first) ->
    ['Step','Next','Continue','Finish','Skip', 'Time Out','Stop',
     'Messages'];
disable(old, break) -> enable(new, break) ++ ['Up', 'Down', 'Where'];
disable(old, running) -> enable(new, running);
disable(old, wait) -> ['Time Out', 'Stop'];
disable(old, _Status) -> [].

shortcut(s) -> {if_enabled, 'Step'};
shortcut(n) -> {if_enabled, 'Next'};
shortcut(c) -> {if_enabled, 'Continue'};
shortcut(f) -> {if_enabled, 'Finish'};
shortcut(m) -> {if_enabled, 'Messages'};
shortcut(w) -> {if_enabled, 'Where'};

shortcut(b) -> {always, 'Line Break...'};
shortcut(d) -> {always, 'Delete All'};

shortcut(_) -> false.

map('Stack On, Tail')    -> all;               % Stack trace
map('Stack On, No Tail') -> no_tail;
map('Stack Off')         -> false;
map(all)                 -> 'Stack On, Tail';
map(true)                -> 'Stack On, Tail';
map(no_tail)             -> 'Stack On, No Tail';
map(false)               -> 'Stack Off'.


%% gui_show_module(Win, Mod, Line, Cm, Pid, How) -> Win
%% Show contents of a module in code area
gui_show_module(Win, Mod, Line, Cm, Pid) ->
    gui_show_module(Win, Mod, Line, Cm, Pid, break).

gui_show_module(Win, Mod, Line, Mod, _Pid, How) ->
    dbg_ui_trace_win:mark_line(Win, Line, How);
gui_show_module(Win, Mod, Line, Cm, Pid, How) ->
    Win2 = case dbg_ui_trace_win:is_shown(Win, Mod) of
	       {true, Win3} -> Win3;
	       false -> gui_load_module(Win, Mod, Pid)
	   end,
    dbg_ui_trace_win:mark_line(Win2, Line, How).

gui_load_module(Win, Mod, Pid) ->
    dbg_ui_trace_win:display({text, "Loading module..."}),
    Contents = int:contents(Mod, Pid),
    Win2 = dbg_ui_trace_win:show_code(Win, Mod, Contents),
    dbg_ui_trace_win:display({text, ""}),
    Win2.

gui_update_bindings(Meta) ->
    gui_update_bindings(Meta, nostack).
gui_update_bindings(Meta, Stack) ->
    Sp = if
	     Stack==nostack -> nostack;
	     true -> {Cur,Max} = Stack, Cur
	 end,
    Bs = int:meta(Meta, bindings, Sp),
    dbg_ui_trace_win:update_bindings(Bs).

gui_enable_functions({Status,_,_}, {Status,_,_}) ->
    ignore;
gui_enable_functions({OldStatus,_,_}, {NewStatus,_,_}) ->
    Enable = enable(old, OldStatus) ++ enable(new, NewStatus),
    Disable = disable(old, OldStatus) ++ disable(new, NewStatus),
    dbg_ui_trace_win:enable(Enable, true),
    dbg_ui_trace_win:enable(Disable, false).

gui_enable_updown(Flag, Stack, Where) ->
    {Enable, Disable} =
	if
	    Flag==false -> {[], ['Up', 'Down']};
	    true ->
		case Stack of
		    {0,0} -> {[], ['Up', 'Down']};
		    {1,1} -> {[], ['Up', 'Down']};
		    {1,_} -> {['Down'], ['Up']};
		    {2,_} when Where==bottom -> {[], ['Up', 'Down']};
		    {2,_} -> {['Down'], ['Up']};
		    {_,_} when Where==bottom -> {['Up'], ['Down']};
		    {C,M} when C>M -> {['Down'], ['Up']};
		    {_,_} -> {['Up', 'Down'], []}
		end
	end,
    dbg_ui_trace_win:enable(Enable, true),
    dbg_ui_trace_win:enable(Disable, false),
    case Enable of
	[] -> dbg_ui_trace_win:enable(['Where'], false);
	_L -> dbg_ui_trace_win:enable(['Where'], true)
    end.

gui_enable_btrace(Trace, StackTrace) ->
    Bool = if
	       Trace==false -> false;
	       StackTrace==false -> false;
	       true -> true
	   end,
    dbg_ui_trace_win:enable(['Back Trace'], Bool).
