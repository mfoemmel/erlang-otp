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
%%%----------------------------------------------------------------------
%%% Purpose : dbg_ui_aux contains the functions shared by the modules 
%%%           dbg_ui_view and dbg_ui_trace.
%%%----------------------------------------------------------------------

-module (dbg_ui_aux).

-include ("dbg_ui_data_struct.hrl").

-export ([
	  add_break/4,
	  back_trace_button/1,
	  break/2,
	  delete_break/3,
	  delete_breaks/2,
	  execute_cmd/2,
	  flush_motion/3,
	  get_flags/1,
	  gs_cmd/2,	
	  insert_breaks/2,
	  is_loaded/6,
	  key/1,
	  load_file/5,
	  module_menu/1,
	  new_break_options/3,
	  new_mod/2,
	  new_mods/2,
	  request_backtrace/2,
	  request_bindings/2,
	  show_line/5,
	  stack_buttons/4,
	  switch/1,
	  to_atom/1,
	  update_windows/1,
	  get_att_pid/2,
	  term_edit/2
	 ]).

-export([
	 add_menu/2,
	 remove_menu/1,
	 enable_menus/1,
	 disable_menus/1,
	 select_menus/2,
	 message_window/3,
	 back_trace_window/1,
	 back_trace/2,
	 init_msg_win/4,
	 back_trace_window/1,
	 mark_busy/1,
	 mark_nonbusy/1,
	 no_interpret/2
	]).

-export([
	 start_busy_window/2,
	 stop_busy_window/1,
	 busy_window/1
	]).


%%% Internal exports
-export([get_bt/3,get_bs/3]).



%%% ----------------------------------------------------------
%%% Load the active version of the source code, used by the
%%% interpreted process, into the window.
%%% ----------------------------------------------------------

is_loaded(Mod,Mod,Breaks,_,_Pid,_Win) ->
    {Mod,Breaks};
is_loaded(Mod,Cm,Breaks,Line,Pid,Win) ->
    load_file(Mod,Cm,Breaks,Pid,Win).



%%% load_file (Module, Cm, Breaks, Pid, Win)
%%%
%%%

load_file (Module, Cm, Breaks, Pid, Win) ->
    OldBreaks = int:all_breaks (Cm),
    delete_breaks (OldBreaks, 0),

    case dbg_idb:which_db (Pid, Module) of
	not_found ->
	    Text = io_lib:format ("Could not find module ~p", [Module]),
	    dbg_ui_trace_win:put_text (Text, 'CodeEditor'),
	    gs:config ('CodeEditor', raise),
	    disable_menus (['Next', 'NextMenu', 'Step',
			    'StepMenu', 'Finish', 
			    'FinishMenu', 'Continue', 
			    'ContinueMenu', 'SkipMenu',
			    'Up', 'UpMenu', 'Down', 
			    'DownMenu', 'Messages', 
			    'MessagesMenu', 'Time Out',
			    'Time OutMenu', 'Stop', 
			    'StopMenu', 'Skip', 'Kill', 
			    'Where', 'WhereMenu', 'Break']),
	    {not_found,[]};

	DB ->
	    mark_busy (Win),
	    InfoText = io_lib:format ("Loading Module ~s...", [Module]),
	    dbg_ui_trace_win:update_label ({text, InfoText}, dummy, dummy),

	    NewBreaks = int:all_breaks (Module),

	    Mode = case Pid of
		       [] ->
			   view;
		       _ -> 
			   trace
		   end,

	    Editor = 
		case dbg_ui_cache:get_editor (self (), Module) of

		    %% The module is not in dbg_ui_cache server.
		    %% 
		    exists_not ->
			E = dbg_ui_trace_win:trace_editor (Mode),
			gs:config (E, raise),
			dbg_ui_cache:insert_editor (self (), Module, E),
			BinFile = read_file (DB, Module),
			Text = binary_to_list (BinFile),
			dbg_ui_trace_win:put_text (Text, E),
			E;

		    %% The module exists in dbg_ui_cache server.
		    %% 
		    GsEditor ->
			gs:config (GsEditor, raise),
			GsEditor
		end,

	    write_breaks (NewBreaks, Module, 0, Pid, Editor),
	    dbg_ui_trace_win:update_label ({text, ""}, dummy, dummy),
	    mark_nonbusy (Win),

	    {Module, NewBreaks}
    end.






%%% view_mod(Mod,Trace) -> #trace
%%% View a module in the code editor window, using the same
%%% fun as in the trace window.

view_mod(Mod,Trace) ->
    {NMod,Breaks} = is_loaded(Mod,Trace#trace.cm, Trace#trace.breaks,
			      dummy, Trace#trace.pid, Trace#trace.win),
    Trace#trace{cm=NMod,breaks=Breaks}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BREAK POINTS

%%% break_points(Trace,Mod,Line,Action) -> #trace
%%% Handle all change of states in breaks requested by the user.
%%% These calls are asyncronius, and thus do not require visual
%%% feed back. An event will be sent to the main process upon
%%% completion.

break_points(Trace,Module,Line,delete) ->
    int:delete_break(Module,Line),Trace;
break_points(Trace,Module,Line,inactive) ->
    int:disable_break(Module,Line),Trace;
break_points(Trace,Module,Line,active) ->
    int:enable_break(Module,Line),Trace;
break_points(Trace,Module,Line,trigger_disable) ->
    int:action_at_break(Module,Line,disable),Trace;
break_points(Trace,Module,Line,trigger_enable) ->
    int:action_at_break(Module,Line,enable),Trace;
break_points(Trace,Module,Line,trigger_delete) ->
    int:action_at_break(Module,Line,delete),Trace;
break_points(Trace,Module,Line,Action) ->
    Trace.



%%% break(Trace,Action) -> #trace
%%% break is called whenever an event concerning a break is
%%% received. This event will result in changes in the menus.

%%% A new break must be added to the menus

break (Trace, Action) ->
    case dbg_ui_cache:current_editor (self ()) of
	exists_not ->
	    exit (editor_error);
	
	%% Use an already loaded editor from the dbg_ui_cache
	%%
	Editor ->
	    break (Trace, Action, Editor)
    end.
		

break (Trace, {new_break, Break}, Editor) ->
    Cm = Trace#trace.cm,
    {Mod, Line, Status} = Break,

    case Cm of
	Mod ->
	    Breaks = Trace#trace.breaks,

	    case lists:keysearch ({Mod, Line}, 1, Breaks) of
		false ->
		    NewBreak = {{Mod, Line}, Status},
		    Br_num = length (Breaks), 
		    Pid = Trace#trace.pid,
		    write_breaks ([NewBreak], Cm, Br_num, Pid, Editor),
		    Trace#trace{breaks = [NewBreak | Breaks]};

		_ ->
		    dbg_ui_trace_win:break_colour (Line, hd(Status), Editor),
		    new_break_options (Mod, Line, Status),
		    Trace
	    end;
	_ ->
	    Trace
    end;

%%% All breaks belonging to Module Mod must be deleted from the menus

break (Trace, {no_break, {Mod}}, Editor) ->
    Cm = Trace#trace.cm,
    case Cm of

	Mod -> 
	    break (Trace, {no_break, {}}, Editor);

	_   -> 
	    Trace
    end;

%%% All the breaks have been deleted and must be removed from Menus

break (Trace, {no_break, {}}, Editor) ->
    delete_breaks (Trace#trace.breaks, 0), %%Menus
    dbg_ui_trace_win:remove_breaks (Trace#trace.breaks, Trace#trace.cm, Editor),
    Trace#trace{breaks = []};

%%% Delete a specific break from the Menus

break (Trace, {delete_break, {Mod, Line}}, Editor) ->
    Cm = Trace#trace.cm,

    case Cm of

	Mod ->
	    Breaks = Trace#trace.breaks,
	    delete_break (Mod, Line, length (Breaks) - 1), %%Menu
	    dbg_ui_trace_win:remove_break (Line, Editor), %%Editor
	    Trace#trace {breaks = lists:keydelete ({Mod, Line}, 1, Breaks)};

	_ ->
	    Trace
    end;

%%% Change the break options in the Menu

break (Trace, {new_break_options, {{Mod, Line}, Status}}, Editor) ->
    Cm = Trace#trace.cm,

    case Cm of

	Mod ->
	    dbg_ui_trace_win:break_colour (Line, hd (Status), Editor),
	    new_break_options (Mod, Line, Status);

	_ ->
	    ok
    end,
    Trace.



%%% text_command(String,Trace).
%%% text_command is called whenever Return is pressed in the
%%% evaluator entry box. The command is echoed in the Editor
%%% and sent off to the Meta process to be evaluated. Various
%%% Stack levels yield different results.
%%% Handle requests coming from the Evaluator

text_command([10],_) ->
    dbg_ui_trace_win:print_shell("\n",normal);
text_command(Cmd,Trace) ->
    dbg_ui_trace_win:print_shell([$>,Cmd,10],normal),
    case Trace#trace.state of
	{running,_,_} ->
	    Str = "< You May not Execute commands while running\n",
	    dbg_ui_trace_win:print_shell(Str,bold);
	_  ->
	    Meta = Trace#trace.meta,
	    Cm = Trace#trace.cm,
	    CmdNo = Trace#trace.cmd_no,
	    case Trace#trace.stack of 
		{Cur,Max} when Cur < Max ->
		    dbg_icmd:command(Meta,Cm,Cmd,CmdNo,Cur);
		_ ->
		    dbg_icmd:command(Meta,Cm,Cmd,CmdNo)
	    end
    end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Execute user commands not handled directly in execute_cmd/3

%%% trace_step(Cmd,Meta,Cm,{break_at,_,_}).
%%% Cmd: finish|step|next
%%% Meta: Process attached to the traced process
%%% Cm: Current Module

trace_step(Cmd,Meta,Cm,{break_at,_,_}) ->  apply(dbg_icmd,Cmd,[Meta]);
trace_step(  _,   _, _,         _)     -> true.

%%% user_up(Trace) -> #trace
%%% Move up in the stack when allowed, updating visual feed back

user_up(Trace) ->
    case up(Trace#trace.meta,Trace#trace.stack) of
	{CurLev,MaxLev,Mod,Line} ->
	    Stack = {CurLev,MaxLev},
	    request_bindings(Trace#trace.meta, Stack),
	    stack_buttons(Trace#trace.stack_flag,CurLev,MaxLev,stack),
	    stack_action(Trace#trace{stack=Stack},CurLev,Mod,Line);
	_ ->
	    Trace
    end.

up(Meta,{Cur,Max}) when Cur > 0 ->
    case dbg_icmd:up_stack(Meta,Cur) of
	{ok,{NewLev,Mod,Line}} ->
	    {NewLev,Max,Mod,Line};
	_ ->
	    false
    end;
up(_,_) ->
    false.

%%% user_down(Trace) -> #trace
%%% Move down in the stack when allowed, updating visual feed back
%%% When the bottom is reached, refresh the exit/break at label

user_down(Trace) ->
    case down(Trace#trace.meta,Trace#trace.stack) of 
	{CurLev,MaxLev,Mod,Line} ->
	    Stack = {CurLev,MaxLev},
	    stack_buttons(Trace#trace.stack_flag,CurLev,MaxLev,stack),
	    request_bindings(Trace#trace.meta,Stack),
	    stack_action(Trace#trace{stack=Stack},CurLev,Mod,Line);
	bottom ->
	    NewStack = {Sp,Sp} = reset_stack(Trace#trace.stack),
	    stack_buttons(Trace#trace.stack_flag,Sp,Sp,bottom),
	    {Status,Mod,Line} = Trace#trace.state,
	    {_Mod,Breaks} = is_loaded(Mod, Trace#trace.cm,
				      Trace#trace.breaks,Line,
				      Trace#trace.pid,
				      Trace#trace.win),
	    show_line(Mod,Line,Trace#trace.line,Status,Trace#trace.pid),
	    dbg_ui_trace_win:update_label(Status,Line,Mod),
	    request_bindings(Trace#trace.meta,NewStack),
	    Trace#trace{cm=Mod,breaks=Breaks,stack=NewStack,line=Line};
	_ ->
	    Trace
    end.


down(Meta,{Cur,Max}) when Cur =< Max ->
    case dbg_icmd:down_stack(Meta,Cur) of
	{ok,{NewLev,Mod,Line}} ->
	    {NewLev,Max,Mod,Line};
	{error,bottom} ->
	    bottom;
	_other ->
	    false
    end;

down(_,_) ->
    false.



%%% show_line(Mod,LineNo,Old_line,Type,Pid).
%%% Mod: Current Module
%%% LineNo: The new line to be put in Focus
%%% Old_line: The old line in FOcus
%%% Type: The type of action, ie break_at|wait_at|break
%%% Show current line in the code editor, deleting
%%% Previous bold fonts and text in the old line.

show_line(Mod,LineNo,Old_line,Type,Pid) ->
    case dbg_idb:which_db(Pid,Mod) of
	not_found ->
	    false;
	DB ->
	    case dbg_idb:lookup(DB,LineNo) of
		{ok,_} ->
		    Editor = dbg_ui_cache:current_editor (self()),
		    dbg_ui_trace_win:select_line(Editor,Type,
						 LineNo,Old_line);
		_ ->
		    false
	    end
    end.



%%% write_messages(Messages,Number) -> true.
%%% Pretty print and write the traced process messages
%%% in the evaluator Window

write_messages([Msg|Msgs],N) ->
    FormValue = dbg_pretty:term(Msg),
    dbg_ui_trace_win:print_shell(io_lib:format(" ~w:",[N]),bold),
    dbg_ui_trace_win:print_shell(io_lib:format(" ~s~n",[FormValue]),normal),
    write_messages(Msgs,N+1);
write_messages([],_) ->
    true.





%%% flush_motion(X,Y,Trace) -> #trace
%%% X,Y: Lask known mouse cursor position relative to the Window
%%% Read the position of the mouse cursor relative to the
%%% Window. Clear the message queue of all motion events,
%%% saving the data included in the last one.

flush_motion (X, Y, Trace) ->
    receive
	{gs, _, motion, _, [NX, NY]} -> 
	    flush_motion (NX, NY, Trace)
    after 0 ->
	    Trace#trace{coords = {X, Y}}
    end.


%%% Enable and disable back trace button  depending on the Frame status.

back_trace_button(#trace{trace=open}) ->
    enable_menus(['BackTraceMenu']);
back_trace_button(#trace{trace=close}) ->
    disable_menus(['BackTraceMenu']).


%%% get_cursor_pos(Win,{X,Y}) -> {X',Y'}.
%%% X,Y: The position of the Monitor Window relative to the screen.
%%% Get the last known position of the cursor in the trace Window,
%%% Reducing the value somewhat (Due to various window Managers),
%%% So that new windows can be placed on the mouse cursor and be in
%%% focus.

get_cursor_pos(Win,{X,Y}) ->
    WinX = gs:read(Win,x),
    WinY = gs:read(Win,y),
    {WinX+X-5,WinY+Y-5}.

%%% Switch the status of the frames

switch(open)  -> close;
switch(close) -> open.



%%% gs_cmd(Gs_Cmd,Trace) -> #trace
%%% Handle all user commands here, directing them to the
%%% right functions. 

gs_cmd(Gs_Cmd,Trace) ->
    case Gs_Cmd of 

	%%Resize window

	{gs,_W,configure ,_,[W,H|_]} ->
	    Flags = get_flags(Trace),
	    dbg_ui_trace_win:configure(Trace#trace.size,{W,H},get_flags(Trace)),
	    Trace#trace{size = {W,H}};


	%%Key press events

	{gs,'Shell',keypress,_,['Return'|_]} ->
	    execute_cmd('Shell', Trace);

	{gs,_,keypress,_,[Key,_,_,1]} ->
	    execute_cmd(key(Key), Trace);


	%%Menu and Button Events

	%% manages the Windows menu - puts the choosen window on top
        {gs, _, click, [win_menu, Win_Pid], _}     ->
	    dbg_ui_winman:on_top(Win_Pid),
	    Trace;

	{gs,Id, click,{break,{Module,Line,Action}}, _A} ->
	    execute_cmd({break,{Module,Line,Action,Id}}
			,Trace);

	{gs,Id,click,{menu,Command},_} -> 
	    execute_cmd(Command, Trace);

	{gs,Id,click,{view,Mod},_} ->
	    execute_cmd({view,Mod},Trace);

	{gs,Command,click,_,_} -> 
	    execute_cmd(Command, Trace);


	%%Frame Resize Event

	{gs,RB,buttonpress,resizebar,_} ->
	    dbg_ui_trace_win:resize(Trace#trace.win,RB,get_flags(Trace)), Trace;


	{gs, Editor, buttonpress, {trace_editor, Mode}, _} ->
	    ButtonPress = now (),
	    NTrace = handle_editor_action (Editor, ButtonPress, Trace, Mode),
	    NTrace#trace{btn_press = ButtonPress};

	_ ->
	    Trace
    end. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% execute_cmd(Command,Trace) -> #trace
%%% Command: Command|{Command,Data}
%%% Trace: #trace
%%% Here are the functions handling all the user commands, divided up
%%% in their respective menus.

%%% Start of additions made by Fredrik Gustafson
execute_cmd('Help',Trace) -> 
    HelpFile = filename:join(code:priv_dir(debugger), "../doc/index.html"),
    tool_utils:open_help(gs:start([{kernel, true}]), HelpFile),
    Trace;

%%% End of additions made by Fredrik Gustafson




%%% File Menu

execute_cmd('Close', Trace) ->
    dbg_ui_winman:delete_win (Trace#trace.win),
    exit(close);

%%% Edit Menu

execute_cmd ('GoToLine', Trace) ->
    Editor = dbg_ui_cache:current_editor (self()),
    Size = gs:read (Editor, size),
    dbg_ui_gotoline:start (self (), Editor, Trace#trace.win, Size),
    Trace;

execute_cmd ('Search', Trace) ->
    Editor = dbg_ui_cache:current_editor (self()),
    dbg_ui_search:start (self (), Editor, Trace#trace.win),
    Trace;


execute_cmd({view,Mod}, Trace) ->
    view_mod(Mod,Trace);

%%% Module Menu

execute_cmd('Shell',Trace) ->
    text_command(gs:read('Shell',text),Trace),
    gs:config('Shell',[{text,""},{focus,false}]),
    Trace;

%%% Process Menu

execute_cmd('Next', Trace) ->
    trace_step(next,Trace#trace.meta,Trace#trace.cm,
	       Trace#trace.state), Trace;
execute_cmd('Step', Trace) ->
    trace_step(step,Trace#trace.meta,Trace#trace.cm,
	       Trace#trace.state), Trace;
execute_cmd('Finish', Trace) ->
    trace_step(finish,Trace#trace.meta,Trace#trace.cm,
	       Trace#trace.state), Trace;
execute_cmd('Kill',Trace) -> 
    exit(Trace#trace.pid,kill),
    Trace;
execute_cmd('Time Out',Trace) -> 
    dbg_icmd:timeout(Trace#trace.meta),Trace;
execute_cmd('Skip',Trace) -> 
    dbg_icmd:skip(Trace#trace.meta), Trace;
execute_cmd('Continue',Trace) -> 
    dbg_icmd:continue(Trace#trace.meta), Trace;
execute_cmd('Stop',Trace) -> 
    int:attach(Trace#trace.pid,Trace#trace.meta),Trace;
execute_cmd('Up',Trace) ->
    user_up(Trace);
execute_cmd('Down',Trace) ->
    user_down(Trace);
execute_cmd('Back', Trace) ->  %%Dummy, for Where Is button
    Trace;
execute_cmd('Messages',Trace) ->
    case dbg_icmd:messages(Trace#trace.meta) of
	[] ->
	    dbg_ui_trace_win:print_shell("< No Messages ! \n",bold),
	    Trace;
	Messages ->
	    dbg_ui_trace_win:print_shell("< --- Current Messages --- \n",bold),
	    write_messages(Messages,1),
	    Trace
    end;
execute_cmd('Stack Tail',Trace) ->
    dbg_icmd:stack_trace_pid(Trace#trace.meta,true),
    select_menus(['Stack Tail'],true),
    Trace#trace{stack_flag = true};

execute_cmd('Stack',Trace) ->
    dbg_icmd:stack_trace_pid(Trace#trace.meta,no_tail),
    select_menus(['Stack'],true),
    Trace#trace{stack_flag = no_tail};

execute_cmd('Stack Off',Trace) ->
    dbg_icmd:stack_trace_pid(Trace#trace.meta,false),
    select_menus(['Stack Off'],true),
    Trace#trace{stack_flag = false};

%%% Break Menus

execute_cmd('Normal Break', Trace) ->
    Pos = get_cursor_pos(Trace#trace.win,Trace#trace.coords),
    dbg_ui_break:start(normal,{Trace#trace.cm,line()},Pos),
    Trace;
execute_cmd('Functional Break',Trace) ->
    Pos = get_cursor_pos(Trace#trace.win,Trace#trace.coords),
    dbg_ui_break:start(functional,Trace#trace.cm,Pos),
    Trace;
execute_cmd('Conditional Break',Trace) ->
    Pos = get_cursor_pos(Trace#trace.win,Trace#trace.coords),
    dbg_ui_break:start(conditional,{Trace#trace.cm,line()},Pos),
    Trace;
execute_cmd('Delete All Breaks',Trace) -> 
    int:no_break(Trace#trace.cm),
    Trace;
execute_cmd({break,{Module,Line,Action,ID}},Trace) ->
    break_points(Trace,Module,Line,Action);

%%% Options Menus

execute_cmd('Trace Flag',Trace) ->
    Flags = get_flags(Trace),
    NTrace= Trace#trace{trace=switch(Trace#trace.trace)},
    NFlags = get_flags(NTrace),
    back_trace_button(NTrace),
    case NTrace#trace.trace of
	open  ->  dbg_icmd:trace_pid(NTrace#trace.meta,on);
        close -> dbg_icmd:trace_pid(NTrace#trace.meta,off)
    end,
    NewSize = dbg_ui_trace_win:configure(Trace#trace.win,function,
					 switch(Trace#trace.trace),
					 Flags,NFlags),
    NTrace#trace{size=NewSize};

execute_cmd('Back Trace',Trace) ->
    Pos = get_cursor_pos(Trace#trace.win,Trace#trace.coords),
    back_trace_window(Pos),
    Trace;

execute_cmd('Bindings Frame',Trace) -> 
    Flags = get_flags(Trace),
    NTrace = Trace#trace{bind = switch(Trace#trace.bind)},
    NFlags = get_flags(NTrace),
    NewSize = dbg_ui_trace_win:configure(Trace#trace.win,bind,
					 switch(Trace#trace.bind),
					 Flags,NFlags),
    NTrace#trace{size=NewSize};
execute_cmd('Evaluator Frame',Trace) -> 
    Flags = get_flags(Trace),
    NTrace = Trace#trace{eval = switch(Trace#trace.eval)},
    NFlags = get_flags(NTrace),
    NewSize = dbg_ui_trace_win:configure(Trace#trace.win,shell,
					 switch(Trace#trace.eval),
					 Flags,NFlags),
    NTrace#trace{size=NewSize};
execute_cmd('Buttons Frame',Trace) -> 
    Flags = get_flags(Trace),
    NTrace = Trace#trace{button = switch(Trace#trace.button)},
    NFlags = get_flags(NTrace),
    NewSize = dbg_ui_trace_win:configure(Trace#trace.win,button,
					 switch(Trace#trace.button),
					 Flags,NFlags),
    dbg_idb:insert(frame_defs,NFlags),
    NTrace#trace{size=NewSize};
execute_cmd('Where',Trace) ->   %%Dummy commands
    {Cur,Max} = Trace#trace.stack,
    user_down(Trace#trace{stack = {Max,Max}});

execute_cmd('BackTraceMenu',Trace) ->
    request_backtrace(Trace#trace.meta,Trace#trace.btrace),
    Trace;

execute_cmd('Break', Trace) ->
    gs:config ('Break', [{label, {text, 'Break'}}]),
    Row = gs:read ('Break', data), 
    set_break (Trace#trace.cm, Row),
    gs:config ('Break', {enable, false}),
    Trace;

execute_cmd(_CMD,Trace) ->   %%Dummy commands
    Trace.



%%% update_windows
%%%

update_windows (Data) ->
    dbg_ui_winman:update_windows_menu (Data).



%%% Fetch the binary contents of the source code file 
%%% corresponding to Module.

read_file (DB, Module) ->
    case dbg_idb:lookup (DB, mod_bin) of

	{ok, Bin} -> 
	    Bin;
	What -> 
	    throw (What)
    end.


%%% write_breaks(Breaks,Cm,Num,Pid).
%%% Breaks: List of breaks from the attach process
%%% Cm: The current module in the code editor

write_breaks (Breaks, Cm, Num, Pid, Editor) ->
    case dbg_idb:which_db (Pid, Cm) of

	not_found -> 
	    false;
	DB -> 
	    write_breaks2 (DB, Breaks, Cm, Num, Editor)
    end.



write_breaks2 (DB, [{Break, Options} | Breaks], Cm, Num, Editor) ->
    NewNum = write_break (DB, Break, Options, Cm, Num, Editor),
    write_breaks2 (DB, Breaks, Cm, NewNum, Editor);

write_breaks2 (_, _, _, _, _) ->
    true.



%%% write_break(DB,{Mod,LineNo},Opts,Cm).
%%% DB: The attach database of the current module
%%% Mod: The module the break is in
%%% LineNo: The line number of the Break
%%% Opts: The Status Options of the break (ie, enable, action on trigger)
%%% Cm: The current module in the code editor

write_break (DB, {Cm, Line}, Status, Cm, Num, Editor) ->
    case dbg_idb:lookup (DB, Line) of

	{ok, _} ->
	    dbg_ui_trace_win:add_break (Line, hd (Status), Editor),
	    add_break (Cm, Line, Status, Num),
	    Num + 1;
	_  -> 
	    Num
    end;

write_break( _, _, _, _, _, _) ->
    true.



%%% get_flags
%%%
%%% Return the flags regarding the Frame options

%%% get_flags (#trace{trace = Trace, bind = Bind, button = Button, eval = Eval}) ->
%%%    {Button, Eval, Bind, Trace}.

get_flags (Trace) ->
    {Trace#trace.button, Trace#trace.eval, Trace#trace.bind, Trace#trace.trace}. 



%%% line() -> LineNo|""
%%% Return the line number last clicked by the user in
%%% The editor

line() ->
    Editor = dbg_ui_cache:current_editor (self()),
    {Y,_X} = dbg_ui_trace_win:get_position(Editor),
    Last = dbg_ui_trace_win:get_end(Editor),
    case Last of 
	Y -> "";
	_ -> Y
    end.




%%% stack_action(Trace,Lev,Mod,Line) -> #trace
%%% Lev:  The level in the stack we are at
%%% Mod:  Current Mocule
%%% Line: Line to be shown in the traversal of the stack
%%% User feed back when going up and down in the stack

stack_action(Trace,Lev,_,-1) ->
    Txt=io_lib:format("****:Call level #~w: external (not interpreted) call",
		      [Lev]),
    dbg_ui_trace_win:update_label({text,Txt},dummy,dummy),
    Trace;

stack_action(Trace,Lev,Mod,Line) ->
    {_Mod,Breaks} = is_loaded(Mod, Trace#trace.cm, Trace#trace.breaks,
			      Line, Trace#trace.pid, Trace#trace.win),
    show_line(Mod,Line,Trace#trace.line,stack_at,Trace#trace.pid),
    Txt = io_lib:format("***Call level #~w: in module ~w at line ~w",
			[Lev,Mod,Line]),
    dbg_ui_trace_win:update_label({text,Txt},dummy,dummy),
    Trace#trace{breaks=Breaks,cm=Mod, line= Line}.



%%% Enable and disable stack buttons depending on the level.

stack_buttons(false,_,_,_) ->
    disable_menus(['Up','UpMenu','Down','DownMenu']);
stack_buttons(_,0,0,_) ->
    disable_menus(['Up','UpMenu','Down','DownMenu']);
stack_buttons(_,1,1,_) ->
    disable_menus(['Up','UpMenu','Down','DownMenu']);
stack_buttons(_,1,_,_) ->
    disable_menus(['Up','UpMenu']),
    enable_menus(['Down','DownMenu']);
stack_buttons(_,2,_,bottom) ->
    disable_menus(['Up','UpMenu','Down','DownMenu']);
stack_buttons(_,2,_,_) ->
    disable_menus(['Up','UpMenu']),
    enable_menus(['Down','DownMenu']);
stack_buttons(_,_,_,bottom) ->
    enable_menus(['Up','UpMenu']),
    disable_menus(['Down','DownMenu']);
stack_buttons(_,Cur,Max,_) when Cur > Max ->
    disable_menus(['Up','UpMenu']),
    enable_menus(['Down','DownMenu']);
stack_buttons(_,_,_,_) ->
    enable_menus(['Up','UpMenu','Down','DownMenu']).



%%% Reset the stack

reset_stack(Stack) ->
    {_,MaxLev} = Stack,
    {MaxLev,MaxLev}.




%%% request_bindings/2 spawns the query for the bindings,
%%% the result will later be sent to the calling process.
%%% on the form
%%% {bs, Bindings}
%%% where Bindings is a list of {Variable, Value}-tuples.
%%% FIXME: Describe why this thing is *spawned*.

request_bindings(Meta, Flag) ->
    spawn(?MODULE,get_bs,[self(),  Meta, Flag]).

get_bs(Parent, Meta, Stack) ->
    Bindings =
	case Stack of
	    false ->
		get_bindings(Meta, nostack);
	    {SP,_} ->
		get_bindings(Meta, SP)
	end,
    Parent ! {bs, Bindings}.






%%% FIXME: This is an interpreter Meta process API function.
%%% It should be placed in the appropriate module.

%%%-- Get all current bindings.
%%%-- This function shall only be executed then the
%%%-- interpreted process is in state break.
%%%-- Otherwise we will be hanging here ...


get_bindings(Meta, SP) ->
    Meta ! {cmd, get_bindings, {self(), SP}},
    receive
	{Meta, bindings, Bs} ->
	    Bs
    end.


%%% request_backtrace/2 spawns the query for the backtrace,
%%% the result will later be sent to the calling process.
%%% on the form
%%% {back_trace, Txt}

request_backtrace(Meta, Size) ->
    spawn(?MODULE,get_bt,[self(), Meta, Size]). 



get_bt(Parent,Meta,Size) ->
    case  dbg_icmd:backtrace(Meta,Size) of
	{ok,Txt} -> Parent!{back_trace,Txt};
	_ -> ok
    end.



%%% to_atom  /1
%%%
%%% to_atom returns an atom
%%%
%%% Pre:
%%%    X  ==  atom  ||  string
%%%
%%% Def:
%%%    to_atom (X)  ==  atom
%%%

to_atom (X) when atom (X) ->
    X;

to_atom (X) ->
    list_to_atom (lists:flatten (X)).



%%% handle_editor_action  /4
%%%
%%% handle_editor_action handles buttonpress actions in the editor.
%%%
%%% Attach (trace) window: 
%%% If it's a single click on a unmarked row, mark it and set the 
%%% 'Break' button to 'Break: ' + #row. If the row was marked, 
%%% unmark the line. 
%%% If it's a double click a unmarked row, mark it and set
%%% a normal break at that row. If the row had a break point (anyone),
%%% remove the breakpoint.
%%%
%%% View window:
%%% The same as above but without the break button.
%%%

%%% Attach window
%%%

handle_editor_action (Editor, New_pressed, Trace, trace) ->

    %% Get the row and column, 
    %% see if it's a marked row and
    %% if it's a break row

    {Row, Col} = gs:read(Editor, insertpos),                
    Marked_row = marked_row (Row, Trace#trace.edt_marked),  
    Is_break_row = is_break_row ({Trace#trace.cm, Row}, Trace#trace.breaks),

    case double_clicked (500, New_pressed, Trace#trace.btn_press) of

	%% double click, set/remove a breakpoint   

	true ->     
	    gs:config(Editor, {selection, {clear}}),

	    %% Is the #row marked or a break row

	    case {Marked_row, Is_break_row} of

		%% The double click was on two different rows
		%% and the last one had no break point.
		%% Mark the last row and set the break button to #row. 

		{{other_row, _Other_row}, false} ->
		    Str = io_lib:format ("Break: ~w", [Row]),
		    gs:config('Break', [{label, {text, Str}}, {data, Row}]),
		    gs:config ('Break', {enable, true}),
		    gs:config(Editor, {selection, {{Row, 0}, {Row, lineend}}}), 
		    Trace#trace{edt_marked = Row};

		%% The double click was on two different rows
		%% and the last one had a break point.
		%% Unmark the other row and reset the break button.

		{{other_row, _Other_row}, true} ->
		    gs:config ('Break', [{label, {text, 'Break'}}]),
		    gs:config ('Break', {enable, false}),
		    Trace#trace{edt_marked = undefined};

		%% Set a break point at this row.

		{_, false}  ->       
		    gs:config ('Break', [{label, {text, 'Break'}}]),
		    gs:config ('Break', {enable, false}),
		    set_break (Trace#trace.cm, Row),
		    gs:config(Editor, {selection, {{Row, 0}, {Row, lineend}}}), 
		    Trace#trace{edt_marked = undefined};

		%% This row had a break point, remove it

		{_, true} ->     
		    gs:config ('Break', [{label, {text, 'Break'}}]),
		    gs:config ('Break', {enable, false}),
		    int:delete_break (Trace#trace.cm, Row),
		    Trace#trace{edt_marked = undefined};

		_ ->
		    Trace
	    end;

	%% single click, mark/unmark the #row

	_ ->
	    gs:config(Editor, {selection, {clear}}),

	    case {Marked_row, Is_break_row} of

		%% This row was marked before,
		%% unmark it.

		{this_row, _} ->        
		    gs:config ('Break', [{label, {text, 'Break'}}]),
		    gs:config ('Break', {enable, false}),
		    Trace#trace{edt_marked = undefined};

		%% Not a break point,
		%% mark this row.

		{_, false} ->     
		    Str = io_lib:format ("Break: ~w", [Row]),
		    gs:config('Break', [{label, {text, Str}}, {data, Row}]),
		    gs:config ('Break', {enable, true}),
		    gs:config(Editor, {selection, {{Row, 0}, {Row, lineend}}}), 
		    Trace#trace{edt_marked = Row};

		%% This is a break row,
		%% unmark all others

		{_, true} ->        
		    gs:config ('Break', [{label, {text, 'Break'}}]),
		    gs:config ('Break', {enable, false}),
		    Trace#trace{edt_marked = undefined};

		_ ->
		    Trace
	    end
    end;



%%% View window
%%%

handle_editor_action (Editor, New_pressed, Trace, view) ->
    {Row, Col} = gs:read(Editor, insertpos),
    Marked_row = marked_row (Row, Trace#trace.edt_marked), 
    Is_break_row = is_break_row ({Trace#trace.cm, Row}, Trace#trace.breaks),

    case double_clicked (500, New_pressed, Trace#trace.btn_press) of
	true ->
	    gs:config(Editor, {selection, {clear}}),
	    case {Marked_row, Is_break_row} of
		{{other_row, _Other_row}, false} ->
		    gs:config(Editor, {selection, {{Row, 0}, {Row, lineend}}}), 
		    Trace#trace{edt_marked = Row};

		{_, false}  ->       
		    set_break (Trace#trace.cm, Row),
		    Trace#trace{edt_marked = undefined};

		{{other_row, _Other_row}, true} ->
		    Trace#trace{edt_marked = undefined};

		{_, true} ->     
		    int:delete_break (Trace#trace.cm, Row),
		    Trace#trace{edt_marked = undefined};
		
		_ ->
		    Trace
	    end;

	_ ->
	    gs:config(Editor, {selection, {clear}}),
	    case {Marked_row, Is_break_row} of
		{this_row, _} ->        
		    Trace#trace{edt_marked = undefined};

		{_, false} ->     
		    gs:config(Editor, {selection, {{Row, 0}, {Row, lineend}}}),
		    Trace#trace{edt_marked = Row};

		{_, true} ->        
		
		    Trace#trace{edt_marked = undefined};

		_ ->
		    Trace
	    end
    end.



%%% is_break_row  /2
%%%
%%% is_break_row returns true if the given row is a break row
%%%

is_break_row (_, []) ->
    false;


is_break_row ({Module, Row}, [{{Module, Row}, _} | T]) ->
    true;


is_break_row ({Module, Row}, [_ | T]) ->
    is_break_row ({Module, Row}, T).



%%% marked_row  /2
%%%
%%% marked_row returns this_row if the given row is  
%%% the previous marked row, other_row if it was some 
%%% one else and false if no row was marked
%%%
%%% Pre:
%%%    Row       ==  integer
%%%    Prev_row  ==  integer  ||  undefined
%%%
%%% Def:
%%%    marked_row  ==  this_row  ||  {other_row, Other_row}  ||  false
%%%

marked_row (Row, Prev_row) when Row == Prev_row ->
    this_row;


marked_row (_, Prev_row) ->
    case Prev_row of
	undefined ->
	    false;

	Other_row ->
	    {other_row, Other_row}
    end.



%%% double_clicked  /3
%%%
%%% double_clicked returns true if the difference between the two times
%%% is less than the limit.
%%%
%%% Pre:
%%%    Limit  ==  integer
%%%    The time variables is from the bif now()  ==  {A, B, C}
%%%    where C  ==  0..999999

double_clicked (Limit, {_A, Now_B, Now_C}, {_A, Old_B, Old_C}) ->
    Diff_B = Now_B - Old_B,
    Now_C1 = Diff_B * 1000000,
    Diff = Now_C1 - Old_C,

    Limit > Diff;     % true  ||  false


double_clicked (_, _, _) ->
    false.



%%% set_break  /2
%%%

set_break (Module, Row) ->
    int:break (Module, Row),
    int:action_at_break (Module, Row, enable).




%%% key (K)
%%%

key (K) ->
    Command =  key_acc (K),

    case catch gs:read (Command, enable) of

	true -> 
	    Command;

	_ -> 
	    false
    end.



%%% key_acc
%%%
%%%

key_acc (a) -> 'BackTraceMenu';
key_acc (b) -> 'Normal Break';
key_acc (c) -> 'Close';
key_acc (d) -> 'Delete All Breaks';
key_acc (h) -> 'Finish';
key_acc (k) -> 'Kill';
key_acc (l) -> 'Skip';
key_acc (m) -> 'Messages';
key_acc (n) -> 'Down';
key_acc (o) -> 'Continue';
key_acc (p) -> 'Up';
key_acc (r) -> 'Conditional Break';
key_acc (s) -> 'Step';
key_acc (t) -> 'Time Out';
key_acc (w) -> 'Where';
key_acc (x) -> 'Next';
key_acc (z) -> 'Stop';
key_acc (_) -> false.




%%% new_mods (Trace, Mods) -> #trace
%%%
%%% Update the menus for all the new mods which have been trace compiled,
%%% Making sure that menu entries do not already exist.

new_mods (Trace, []) -> 
    Trace;
new_mods (Trace, [Mod | Mods]) ->
    new_mods (new_mod (Trace, Mod), Mods).



%%% new_mod (Trace, Mod)
%%%
%%%
new_mod (Trace, Mod) ->
    case lists:member (Mod, Trace#trace.mods) of

	true  -> 
	    Trace;

	false ->
	    add_menu_item ('MenuModule', Mod, view),

	    case length (Trace#trace.mods) of

		0 -> 
		    enable_menus (['DeleteMenu', 'ViewMenu',
				   'Delete All Modules']);

		_ -> 
		    ok
	    end,

	    Trace#trace{mods = [Mod | Trace#trace.mods]}
    end.



%%% ---------------------------------------------------------------
%%% INTERFACE: delete_breaks(Breaks,Amount)
%%% INTERFACE: delete_break(Mod,Line,Amount)
%%%
%%% Delete all breaks hasbeen called. remove them all, 
%%% with the separator.
%%% ---------------------------------------------------------------

delete_breaks(Breaks,Amount) ->
    break_separator(Amount,false),
    delete_breaks(Breaks).

delete_breaks([]) -> ok;
delete_breaks([{{Mod,Line},_}|Rest]) ->
    delete_break(Mod,Line),
    delete_breaks(Rest);
delete_breaks(_) -> ignore.  %%usualy bad arg.

delete_break(Mod,Line,Amount) ->
    break_separator(Amount,false),
    delete_break(Mod,Line).

delete_break(Mod,Line) ->
    Daddy = format_atom("Break ~w ~5w",[Mod,Line]),
    gs:destroy(Daddy).

%%% INTERFACE:
module_menu(MenuBar) ->
    MenuButtModule= gs:menubutton(MenuBar, [{label, {text, " Module "}},
					    {underline, 1}]),
    MenuModule = gs:menu(MenuButtModule, []),
    gs:menuitem('Interpret',MenuModule,
		[{label,{text, "Interpret..."}},{underline,0}]),
    gs:menuitem('Delete All Modules',MenuModule ,
		[{label, {text, "Delete All"}},{underline,7},{enable,false}]),
    gs:menuitem('DeleteMenu',MenuModule,
		[{label,{text,"Delete"}}, {underline, 0},
		 {itemtype, cascade},{enable,false}]),
    gs:menu('Delete','DeleteMenu',[]),
    gs:menuitem('ViewMenu',MenuModule,
		[{label,{text,"View"}}, {underline,0},
		 {itemtype, cascade},{enable,false}]),
    gs:menu('View','ViewMenu',[]).



%%% ---------------------------------------------------------------
%%% INTERFACE:
%%%
%%% Change the break options in an already existing break.
%%% Options can include status on trigger (delete/active/inactive) or
%%% the current status (active/inactive)
%%% ---------------------------------------------------------------

new_break_options(Mod,Line,Status)->
    Text = lists:flatten(io_lib:format("~w ~5w",[Mod,Line])),
    Name = lists:flatten(io_lib:format("Break ~s",[Text])),
    Daddy = list_to_atom(Name),
    Statustxt = lists:flatten(io_lib:format("Status ~s",[Text])),
    Statusname = list_to_atom(Statustxt),
    [Activation,Action|_] = Status,
    case Activation of
	active ->
	    gs:config(Daddy,{fg,break_col(false)}),
	    gs:config(Statusname,[{data,{break,{Mod,Line,inactive}}},
				  {label,{text,"Disable"}}]);
	inactive ->
	    gs:config(Daddy,{fg,break_col(true)}),
	    gs:config(Statusname,[{data,{break,{Mod,Line,active}}},
				  {label,{text,"Enable"}}])
    end,
    case Action of
	delete  ->
	    Del = format_atom("~s~w",[Text,trigdel]),
	    gs:config(Del,[{select,true}]);
	enable  ->
	    Ena = format_atom("~s~w",[Text,trigena]),
	    gs:config(Ena,[{select,true}]);
	disable ->
	    Dis = format_atom("~s~w",[Text,trigdis]),
	    gs:config(Dis,[{select,true}])
    end.


format_atom(Format,Args) ->
    list_to_atom(lists:flatten(io_lib:format(Format,Args))).

%%% Choose the colour of the text describing the break, depending on the
%%% state. If the break is disabled, choose blue, if enabled, choose red.

break_col(true)  -> blue;
break_col(false) -> red.



%%% ---------------------------------------------------------------
%%% INTERFACE: ????
%%% ---------------------------------------------------------------

add_break(Mod,Line,Status,Amount) ->
    break_separator(Amount,true),
    add_break(Mod,Line,Status).

add_break(Mod,Line,Status) ->
    Text = io_lib:format("~w ~5w",[Mod,Line]),
    Name = lists:flatten(io_lib:format("Break ~s",[Text])),
    Daddy = list_to_atom(Name),
    Group = list_to_atom(pid_to_list(self()) ++ Name),
    Id = gs:menuitem(Daddy,'MenuBreaks',
		     [{label,{text,Text}},{itemtype, cascade}]),
    Id2 = gs:menu(Id,[]),
    Statusname = format_atom("Status ~s",[Text]),
    gs:menuitem(Statusname, Id2,[]),
    gs:menuitem(Id2,[{label,{text,"Delete"}},
		     {data,{break,{Mod,Line,delete}}}]),
    Id3 = gs:menuitem(Id2,[{label,{text,"Trigger Action"}},
			   {itemtype,cascade}]),
    Id4 = gs:menu(Id3,[]),
    Del = format_atom("~s~w",[Text,trigdel]),
    gs:menuitem(Del, Id4,[{label,{text,"Delete"}},{itemtype, radio},
			  {data,{break,{Mod,Line,trigger_delete}}},
			  {group,Group}]),
    Ena = format_atom("~s~w",[Text,trigena]),
    gs:menuitem(Ena, Id4,[{label,{text,"Enable"}},{itemtype, radio},
			  {data,{break,{Mod,Line,trigger_enable}}},
			  {select,true}, {group,Group}]),
    Dis = format_atom("~s~w",[Text,trigdis]),
    gs:menuitem(Dis, Id4,[{label,{text,"Disable"}},{itemtype, radio},
			  {data,{break,{Mod,Line,trigger_disable}}}, 
			  {group,Group}]),
    new_break_options(Mod,Line,Status).



%%% ---------------------------------------------------------------
%%% The following functions are used to monitor the state of the
%%% Breaks menus, enabling/disabling/creating and deleting the
%%% menus when such is needed.
%%%
%%%   break_separator(NumOfBreaks,Bool)
%%%     Bool: true, we are adding a break
%%%           false we are deleting one.
%%%
%%% If there are no breaks, and one of the above actions takes
%%% place, we need to hide/show the break separator,
%%% ---------------------------------------------------------------

break_separator(0,true) ->
    gs:config('Delete All Breaks',{enable,true}),
    gs:menuitem('BreakSeparator','MenuBreaks',[{itemtype,separator}]);
break_separator(0,false) ->
    gs:config('Delete All Breaks',{enable,false}),
    gs:destroy('BreakSeparator');
break_separator(_,_) ->   ok.


%%% ---------------------------------------------------------------
%%% INTERFACE: Insert new breaks in the breaks menu
%%% ---------------------------------------------------------------

insert_breaks([],Amount)  -> ok;
insert_breaks(Breaks,Amount)  ->
    break_separator(Amount,true),
    insert_breaks(Breaks).

insert_breaks([]) -> ok;
insert_breaks([{{Mod,Line},Status}|Rest]) ->
    add_break(Mod,Line,Status),
    insert_breaks(Rest).



%%% get_att_pid  /2
%%%
%%% get_att_pid returns the attached process from the given name
%%% in the attached window title
%%%
%%% Pre:
%%%    Type  ==  monitor  ||  trace  ||  view
%%%

get_att_pid (Type, Name) ->
    case {Type, string:chr (Name, $<)} of
	{trace, Index} when Index > 0 ->
	    Att_pid = string:substr (Name, Index),
	    list_to_pid (Att_pid);

	_ ->
	    null
    end.




%%% Change Cursor to Busy/Not Busy when on window ID
mark_busy(Id) ->
    gs:config(Id, [{cursor, busy}]).
mark_nonbusy(Id) ->
    gs:config(Id, [{cursor, arrow}]).


%%% Add a busy window with String as a message
start_busy_window (Parent, String) ->
    Pid = spawn (?MODULE, busy_window, [gs:start ()]),
    Pid ! {start, {Parent, String}},
    Pid.

stop_busy_window (Pid) ->
    Pid ! stop.

busy_window (Win) ->
    receive
	{start, {Parent, String}} ->
	    receive
		stop ->
		    self () ! stop
	    after 2000 ->
		    Win_Id = dbg_ui_trace_win:progress_window (Parent, 
							       String),
		    mark_busy (Win_Id),
		    busy_window (Win_Id)
	    end;
	stop ->
	    mark_nonbusy (Win)
	end.


%%% Add a menu
add_menu(Menu, Name) ->
    Atom = list_to_atom(lists:flatten(io_lib:format("~p~p",[Menu,Name]))),
    gs:create(menuitem, Atom, Menu,[{label,{text,Name}},{data,{Menu, Name}}]).


%%% Add a menu item
add_menu_item (Menu, Item, Func) ->
    Item1 = list_to_atom(lists:flatten(io_lib:format("View ~p",[Item]))),
    Name = list_to_atom(lists:flatten(io_lib:format("~p~p",[Menu, Item]))),
    gs:create(menuitem, Name, Menu,[{label,{text,Item1}},{data,{Func, Item}}]).


%%% Remove one
remove_menu(Menu) ->
    gs:destroy(Menu).


%%% Remove an uninterpreted module
no_interpret(Trace, Mod) ->
    Module  = list_to_atom(lists:flatten(io_lib:format("~p~p",['MenuModule',
							       Mod]))),
    remove_menu(Module),
    Trace#trace{mods = lists:delete(Mod,Trace#trace.mods)}.


%%% Select radio button/select Menus
select_menus([],_) -> ok;
select_menus([Menu|Menus],Flag) ->
    gs:config(Menu,{select,Flag}),
    select_menus(Menus,Flag).

%%% Enable and disable Menus

enable_menus(Menus)  -> menus(Menus, true).
disable_menus(Menus) -> menus(Menus, false).
menus([],_) -> ok;
menus([Menu|Menus],Flag) -> 
    gs:config(Menu,{enable,Flag}),
    menus(Menus,Flag).


%%% -----------------------------------------------------------------
%%% Create a window on which short messages can be displayed.

message_window(Text,Parent,{X,Y}) ->
    spawn(?MODULE,init_msg_win,[Text,Parent,X,Y]).
init_msg_win(Text,Parent,X,Y) ->
    link(Parent),
    {Text_obj,Win} = create_text_win(Text,X,Y),
    dismiss(Text_obj,Win).
create_text_win(Text,X,Y) ->
    Gs = gs:start([{kernel,true}]),
    Win = gs:create(window,Gs,[{width,200},{height,75},{destroy,true},
                               {title,"Debug Message"},{keypress,true},
			       {x,X},{y,Y}]),
    T = gs:create(label,Win,
                  [{label,{text,Text}},{x,0},{y,5},{width,200},{justify,center},
		   {align,center}]),
    {T,Win}.


dismiss(Text_Obj,Win) ->
    gs:create(button,quit,Win,[{label,{text,"Dismiss"}},{width,100},
			       {x,50},{y,40}]),
    gs:config(Win,{map,true}),
    receive
	{gs, _, destroy,_ ,_} -> ok;
	{gs, quit, click, _, _} -> ok;
	{gs,_,keypress,[],['Return'|_]} -> ok;
	{gs,_,keypress,[],[c,_,_,1]} -> ok
    end.



%%%------------------------------------------------------------
%%% The entry box for the back trace option

back_trace_window(Pos) ->
    spawn_link(?MODULE,back_trace,[self(),Pos]).

back_trace(Pid,{X,Y}) ->
    Gs = gs:start([{kernel,true}]),
    Win = gs:create(window,Gs,[{width,190},{height,75},{destroy,true},
                               {title,"back trace"},{keypress,true},
			       {x,X},{y,Y},{configure,true}]),
    field(5,f1,"New size:",true,Win),
    buttons(Win,40),
    gs:config(Win,{map,true}),
    loop(Pid,Win,{190,75}).

field(Y,Name,Label,Focus,Win) ->
    gs:create(label,Win,[{x,83},{y,Y},{width,63},{label,{text,Label}},
			 {anchor,ne},{align,e}]),
    gs:entry(Name,Win,[{x,83},{y,Y},{width,97},{keypress,true},
		       {setfocus,Focus},{anchor,nw}]).
buttons(Win,Y) ->
    gs:button(ok,Win,[{width,80},{y,Y},{x,10},{label,{text,"Ok"}},
		      {anchor,nw}]),
    gs:button(cancel,Win,[{width,80},{y,Y},{x,100},{label,{text,"Cancel"}},
			  {anchor,nw}]).


loop(Pid,Win,Size) ->
    receive 
	{gs,f1,keypress,_,['Return'|_]} -> read_backtrace(Pid,Win,Size);
	{gs,ok,click,_,_}               -> read_backtrace(Pid,Win,Size);
	{gs,_,destroy,_,_}            -> exit(normal);
	{gs,cancel,click,_,_}         -> exit(normal);
	{gs,_,keypress,[],[c,99,0,1]} -> exit(normal);
	{gs,_,configure,_,[Dx,Dy|_]}-> resize({Dx,Dy},Size),
				       loop(Pid,Win,Size);
	_Other                        -> loop(Pid,Win,Size)
    end.

resize(Size,Size) -> ok;
resize({Dx,Dy},_) ->
    gs:config(f1,{width,Dx-93}).


read_backtrace(Parent,Win,Size) ->
    case catch convert() of
	{ok,BT_Size} ->
	    Parent!{new_backtrace,BT_Size},
	    exit(normal);
	Error ->
	    X = gs:read(Win,x),
	    Y = gs:read(Win,y),
	    Str = "Error:Bad Input.",
	    message_window(Str,Parent,{X,Y}),
	    loop(Parent,Win,Size)
    end.

convert() ->
    Size = string:strip(gs:read(f1,text)),
    {ok,parse({int,Size})}.


parse({_,       []})      -> throw({error,empty});
parse({int, String})      -> list_to_integer(String).



%%% command from record editor
%%%

term_edit (Cmd, Trace) ->
    text_command (Cmd, Trace).

