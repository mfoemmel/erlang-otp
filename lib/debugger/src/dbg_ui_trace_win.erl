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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% File    : dbg_ui_trace_win.erl
%% Purpose : GS interface for attach windows in the debugger
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(dbg_ui_trace_win).

-export([create_attach_window/3,
	 select_line/4,
	 put_text/2,
	 update_label/3,
	 get_end/1, 
	 get_position/1,
	 put_trace/2,
	 update_bindings/1,
	 print_shell/2,
	 configure_view/2,
	 remove_breaks/3,
	 remove_break/2,
	 add_break/3,
	 add_breaks/3,
	 resize/3,
	 configure/3,
	 break_colour/3,
	 update_backtrace_menu/1,
	 configure/5,
	 progress_window/2,
	 trace_editor/1]).

-define(STRLEN,1500).  %%Max num of characters sent to the Editor at 1 time


% Start of additions made by Fredrik Gustafson

%%% Create the file Menu in the Window

help_menu(MenuBar) ->
    MenuButtHelp = gs:menubutton(MenuBar, [{label, {text, " Help "}},
					   {underline, 1},{side, right}]),
    MenuHelp = gs:menu(MenuButtHelp, []),
    gs:menuitem('Help', MenuHelp,
		[{label, {text, "Help"}}, {underline, 0}]).

% End of additions made by Fredrik Gustafson



%%% Create the file Menu in the Window

file_menu(MenuBar) ->    
    MenuButtFile = gs:menubutton(MenuBar,
				 [{label,{text," File "}}, {underline, 1}]),
    MenuFile = gs:menu(MenuButtFile, []),
    gs:menuitem('Close', MenuFile,
		[{label, {text, "Close"}},{underline,0}]).


%%% Create the edit Menu in the Window

edit_menu(MenuBar) ->
    MenuButtEdit = gs:menubutton (MenuBar, [{label, {text, " Edit "}},
					    {underline, 1}]),
    MenuEdit = gs:menu (MenuButtEdit, []),
    gs:menuitem ('GoToLine', MenuEdit, [{label, {text, "Go to line"}}, 
					{underline, 0}]),
    gs:menuitem ('Search', MenuEdit, [{label, {text, "Search"}}, 
				      {underline, 0}]).


%%% Create the Module Menu in the Window

module_menu(MenuBar) ->
    MenuButtModule= gs:menubutton(MenuBar,
				  [{label, {text, " Module "}}, {underline, 1}]),
    gs:menu('MenuModule',MenuButtModule, []).


%%% Process Menu in the trace Window

process_menu(MenuBar) ->
    MenuButtProcess = gs:menubutton(MenuBar,
				    [{label,{text," Process "}}, {underline, 1}]),
    MenuProcess = gs:menu(MenuButtProcess, []),
    gs:menuitem('StepMenu',MenuProcess,
		[{label, {text, "Step"}},{underline,0},
		 {enable,false},{data,{menu,'Step'}}]),
    gs:menuitem('NextMenu',MenuProcess,
		[{label, {text, "Next"}}, {underline,0},
		 {enable,false},{data,{menu,'Next'}}]),
    gs:menuitem('ContinueMenu',MenuProcess,
		[{label, {text, "Continue"}},{underline,0},
		 {enable,false},{data,{menu,'Continue'}}]),
    gs:menuitem('FinishMenu',MenuProcess,
		[{label, {text, "Finish"}},{underline,0},
	       {enable,false},{data,{menu,'Finish'}}]),

    gs:menuitem(MenuProcess,[{itemtype,separator}]),
    gs:menuitem('Time OutMenu',MenuProcess,
		[{label, {text, "Time Out"}},{underline,0},
		 {enable,false},{data,{menu,'Time Out'}},
		 {data,{menu,'Time Out'}}]),
    gs:menuitem('SkipMenu',MenuProcess,
		[{label, {text, "Skip (Ctl-L)"}},{underline,2},
		 {enable,false},{data,{menu,'Skip'}}]),
    gs:menuitem('StopMenu',MenuProcess,
		[{label, {text, "Stop (Ctl-z)"}},{underline,2},
		 {enable,false},{data,{menu,'Stop'}}]),
    gs:menuitem('UpMenu',MenuProcess,
		[{label, {text, "Up"}},{underline,0},
		 {enable,false},{data,{menu,'Up'}}]),
    gs:menuitem('DownMenu',MenuProcess,
		[{label, {text, "Down"}},{underline,0},
		 {enable,false},{data,{menu,'Down'}}]),
    gs:menuitem('WhereMenu',MenuProcess,
		[{label, {text, "Where"}},{underline,0},
		 {enable,false},{data,{menu,'Where'}}]),
    gs:menuitem('MessagesMenu',MenuProcess,
		[{label, {text, "Messages"}},{underline,0},
		 {enable,false},{data,{menu,'Messages'}}]),
     gs:menuitem('BackTraceMenu',MenuProcess,
		[{label, {text, "Back Trace"}},{underline,0},
		 {enable,false}]),
    gs:menuitem('Kill',MenuProcess,
		[{label, {text, "Kill"}},{underline,0},
		 {enable,false},{data,{menu,'Kill'}}]).
    
%%% Breaks Menu in the Window. Individual breaks are attached
%%% To the end of the menu later on, by the application.

breaks_menu(MenuBar) ->
    MenuButtBreaks = gs:menubutton(MenuBar,
				   [{label, {text, " Breaks "}}, {underline, 1}]),
    gs:menu('MenuBreaks', MenuButtBreaks, []),
    gs:menuitem('Normal Break','MenuBreaks',
		[{label,{text, "Line Break..."}},{underline,0}]),
    gs:menuitem('Conditional Break','MenuBreaks',
		[{label,{text, "Conditional Break..."}},
		 {underline,0}]),    
    gs:menuitem('Functional Break','MenuBreaks',
		[{label,{text,"Function Break..."}}, {underline,0}]),
    gs:menuitem('MenuBreaks',[{itemtype,separator}]),
    gs:menuitem('Delete All Breaks','MenuBreaks',
		[{label,{text, "Delete All"}},{underline,0},
		 {enable,false}]).

%%% Options Menu, telling which frames are to be shown or not.

options_menus(MenuBar) ->
    MenuButtFrame = gs:menubutton('ShowMenu',MenuBar,
				  [{label,{text," Options "}}, {underline, 1},
				   {side,left}]), 
    MenuTrace = gs:menu(MenuButtFrame, []),
    gs:menuitem('Buttons Frame',MenuTrace,
		[{label, {text, "Button Frame"}},{underline,1},
		 {itemtype, check},
		 {select,true}]),
    gs:menuitem('Evaluator Frame',MenuTrace,
		[{label, {text, "Evaluator Frame"}},{underline, 0},
		 {itemtype, check},
		 {select,true}]),
    gs:menuitem('Bindings Frame',MenuTrace,
		[{label, {text, "Bindings Frame"}},{underline, 0},
		 {itemtype, check},
		 {select,true}]),
    gs:menuitem('Trace Flag',MenuTrace,
		[{label, {text, "Trace Frame"}},{underline,0},
		 {itemtype, check},
		 {select,true}]),
    gs:menuitem('Back Trace',MenuTrace,
		[{label,{text, "Back Trace Size:"}}, {underline, 13}]),
    StackButt = gs:menuitem('StackMenu',MenuTrace,
			    [{label,{text,"Stack Options"}},{underline,0},
			     {itemtype, cascade}]),
    StackMenu = gs:menu(StackButt,[]),
    Group = list_to_atom(pid_to_list(self()) ++ "Stack"),
    gs:menuitem('Stack Tail',StackMenu,
		[{label,{text,"Stack On, Tail"}}, {underline, 10},
		 {select,false},
		 {itemtype, radio},{group,Group},{data,{menu,'Stack Tail'}}]),
    gs:menuitem('Stack',StackMenu,
		[{label,{text, "Stack On, no Tail"}},{underline, 10},
		 {select,false},
		 {itemtype, radio},{group,Group},{data,{menu,'Stack'}}]),
    gs:menuitem('Stack Off',StackMenu,
		[{label,{text, "Stack Off"}},{underline,6},
		 {select,false},
		 {itemtype, radio},{group,Group},{data,{menu,'Stack Off'}}]).



update_backtrace_menu(Size) ->
    Text = io_lib:format("Back Trace Size: ~p...",[Size]),
    gs:config('Back Trace',{label,{text,Text}}).

%%% Creation of the Function Frame, where all function traces are
%%% Printed out.

function_frame(Flag,X,Y,FrameOpts,Win) ->
    {W,H} = case Flag of
		open -> {546,200};
		close -> {0,0}
	    end,
    gs:frame('FunctionFrame',Win,
	     [{x,X},{y,Y},{height,H},{width,W}|FrameOpts]),
    Editor = gs:editor('FunctionEditor','FunctionFrame',
		       [{x,5},{y,5},{width, 536},{height, 190},
			{keypress,false}]),
    config_editor(Editor,
		  [{vscroll, right}, {hscroll, bottom},
		   {wrap,none},{fg,{{{1,0},'end'},black}},
		   {font_style,{{{1,0},'end'},{screen,[],12}}}]).

%%% Trace Frame, where the code is displayed, along with an information
%%% Label. This is the only frame which can not be hidden.

trace_frame(X, Y, FrameOpts, Win) ->
    gs:frame('TraceFrame',Win,
	     [{x,X},{y,Y},{height,200},{width,546}|FrameOpts]),
    gs:label(info_window,'TraceFrame',
	     [{x,5},{y,10},{label,{text,""}},{anchor,nw},
	      {height,15},{align,w},{width,406}]).



%%% trace_editor
%%%
%%% creates a Gs editor object with the given name in the given Frame.
%%%

trace_editor (Editor, Mode) ->
    gs:editor (Editor, 'TraceFrame',
	       [{x, 5}, {y, 30}, {width, 536}, {height, 165},
		{keypress, false}, {buttonpress, true}, 
		{data, {trace_editor, Mode}}]),
    config_editor (Editor, [{vscroll, right}, {hscroll, bottom}]),
    config_editor (Editor, [{wrap, none}, {fg, {{{1, 0}, 'end'}, black}},
			    {font_style, {{{1, 0}, 'end'},{screen, [], 12}}}]).

trace_editor (Mode) ->
    W = gs:read ('CodeEditor', width),
    H = gs:read ('CodeEditor', height),

    Editor = gs:editor ('TraceFrame',
			[{x, 5}, {y, 30}, {width, W}, {height, H},
			 {keypress, false}, {buttonpress, true}, 
			 {data, {trace_editor, Mode}}]),
    config_editor (Editor, [{vscroll, right}, {hscroll, bottom}]),
    config_editor (Editor, [{wrap, none}, {fg, {{{1, 0}, 'end'}, black}},
			    {font_style, {{{1, 0}, 'end'},{screen, [], 12}}}]),
    Editor.



%%% The Shell Frame allows the user to enter expressions evaluated in
%%% the context of the process being Traced. It is also used for system
%%% information.

shell_frame({S,Bi},X,Y,FrameOpts,Win) ->
     {W,H} = if
		S==open -> {289,200};
		true -> {0,0}
	    end,
    gs:frame('ShellFrame',Win,
	     [{x,X},{y,Y},{height,H},{width,W}|FrameOpts]),
    gs:label('ShellFrame',
	     [{x,5},{y,35},{label,{text,"Evaluator:"}},{anchor,sw},
	      {height,25},{align,center},{width,80}]),
    gs:entry('Shell','ShellFrame',
	     [{x,80},{y,35},{width,185},{keypress,true},
	      {anchor,sw},{height,25}]),
    gs:editor('ShellEditor','ShellFrame',
		       [{x,5},{y,35},{width, 280},{height, 160},
			{vscroll, right},{hscroll, bottom},{wrap,none},
			{fg,{{{1,0},'end'},black}},{keypress,false},
			{font_style,{{{1,0},'end'},{screen,[],12}}}]),
    gs:config('ShellEditor',{enable, false}),
     if
	S==open,Bi==close ->
	    resize_shell(S,width,257);
	true ->
	    true
    end.

%%% Frame where bindings are shown.

bind_frame({S,Bi},X,Y,FrameOpts,Win) ->
     {W,H} = if
		Bi==open -> {249,200};
		true -> {0,0}
	    end,
    gs:frame('BindFrame',Win,
	     [{x,X},{y,Y},{height,H},{width,W}|FrameOpts]),
    BindGridOpts = [{x,2}, {y,2}, {height,193}, {width,241},
		    {fg,black}, {vscroll,right},
		    {font,{screen,[bold],12}}, 
		    {hscroll,bottom},
		    calc_columnwidths(241), {rows, {1,50}}], 

    gs:grid('BindGrid','BindFrame',BindGridOpts),
    gs:gridline('BindGrid',
		[{row,1},{fg,blue},{text,{1,"Name"}},
		 {text,{2,"Value"}},{height,14}]),
    gs:config('BindGrid',{rows,{1,1}}),
    if
	Bi==open,S==close ->
	    resize_bind(Bi,width,297);
	true ->
	    true
    end.

%%% Frame for the ones who prefer the mouse to the key accellerators.

button_frame(Bu,X,Y,FrameOpts,Win) ->
     {W,H} = case Bu of
		open -> {710,30};
		close -> {0,0}
	    end,
    gs:frame('ButtFrame',Win,
	     [{x,X},{y,Y},{height,H},{width,W}|FrameOpts]),
    Buttons = ['Next', 'Step', 'Finish', 'Continue', 'Up', 'Down', 'Where', 'Break'],
    create_buttons('ButtFrame',Buttons).



%%% progress_window (Win, String)
%%% A progress window when loading large files

progress_window (Parent, String) ->
    W = 300,
    H = 50,

    %% Open a new window
    Win = gs:create (window, gs:start (), 
		     [{x, gs:read (Parent, x) + 50}, 
		      {y, gs:read (Parent, y) + 50},
		      {width, W}, {height, H},
		      {title, "Progress Window"}]),

    %% Top frame containing a label
    Top = gs:create (frame, Win, [{width, W}, {height, H}, {x, 0}, {y, 0}]),
    Lbl = gs:create (label, Top, [{width, W}, {height, H}, 
				  {x, 0}, {y, 0},
				  {align, c}, {justify, center}]),
  
    gs:config (Lbl, {label, {text, String}}),
    gs:config (Win, {map, true}),
    Win.



%%% create_attach_window(Title,view,_) -> {WinId,EditorId}
%%% Creates a display Window, with the modules menu from the monitor
%%% window, the File Menu and the Breaks Menu.

create_attach_window(Title, view, _) ->
    GS = gs:start([{kernel,true}]),
    Win_Options = [{title,Title},{width,550},{destroy,true},
		   {height, 400},{motion,true},
		   {keypress,true}],
    Win = gs:window(view_window, GS,Win_Options),
    MenuBar = gs:menubar(Win, []),

% Start of additions made by Fredrik Gustafson

    help_menu(MenuBar),

% End of additions made by Fredrik Gustafson
    
    file_menu(MenuBar),
    edit_menu(MenuBar),
    breaks_menu(MenuBar),
    dbg_ui_winman:windows_menu (MenuBar),

    gs:config(Win, {configure,true}),
    FrameOpts = [{anchor,nw},{relief,raised}, {bw,2}, {buttonpress, true}],
    trace_frame(2, 25, FrameOpts, Win),
    trace_editor ('CodeEditor', view),   % remove ???
    
    configure_view(Win,{550,400}),
    gs:config(Win, [{map, true}]),
    Win;

%%% create_attach_window(Title, What, Flags) -> {WinId,EditorId}
%%% Title: String
%%% What: {trace,Pid}
%%% Flags: {ButtonFlag,EvalFlag,BindFlag,TraceFlag} Flag=open|close
%%%        The flags describing the different frame statuses.
%%% Creates an attach Window with a set of frames showing the environment
%%% of the traced process.

create_attach_window (Title, What, Flags) ->
    {Bu,S,Bi,F} = Flags,
    GS = gs:start([{kernel,true}]),
    
    Win_Options=[{title,Title},{width,550},{keypress,true},
		 {motion,true},{destroy,true}],
    Win =gs:window(trace_window, GS,Win_Options),

    %%Menus
    MenuBar = gs:menubar(Win, []),

% Start of additions made by Fredrik Gustafson

    help_menu(MenuBar),

% End of additions made by Fredrik Gustafson

    file_menu(MenuBar),
    edit_menu(MenuBar),
    module_menu(MenuBar),
    process_menu(MenuBar),
    breaks_menu(MenuBar),
    options_menus(MenuBar),
    dbg_ui_winman:windows_menu (MenuBar),


    %%Frames
    if Bu==close -> gs:config('Buttons Frame',{select,false}); true -> t end,
    if S==close -> gs:config('Evaluator Frame',{select,false}); true -> t end,
    if Bi==close -> gs:config('Bindings Frame',{select,false}); true -> t end,
    if F==close -> gs:config('Trace Flag',{select,false}); true -> t end,
    
    FrameOpts = [{keypress,true}, {anchor,nw},{relief,raised},{bw,2}],
    trace_frame(2, 25, FrameOpts, Win),
    trace_editor ('CodeEditor', trace),   % remove ???

    configure_view(Win,{550,400}),

    button_frame(Bu,2,235,FrameOpts,Win),
    resize_button(Bu,width,-164),
    shell_frame({S,Bi},2,265,FrameOpts,Win),
    bind_frame({S,Bi},300,265,FrameOpts,Win),
    function_frame(F,2,475,FrameOpts,Win),

    %%Resize Bars
    resizebar(rb1(Flags),'RB1',2,225,710,10,Win),
    resizebar(rb2(Flags),'RB2',2,465,710,10,Win),
    resizebar(rb3(Flags),'RB3',290,265,10,200,Win),
    config_v(),
    config_h(),
    
    gs:config(Win,{height,
		   25 +
		   gs:read('TraceFrame',height) +
		   gs:read('RB1',height) +
		   gs:read('ButtFrame',height) +
		   max(gs:read('ShellFrame',height),
		       gs:read('BindFrame',height)) +
		   gs:read('RB2',height) +
		   gs:read('FunctionFrame',height)}),
    
    gs:config(Win, [{map, true}]),
    gs:config(Win, [{configure,true}]),
    Win.

%%% update_bindings(Bindings).
%%% Bindings: [{'Name',Value}|..]
%%% Updates all the bindings shown in the Grid in the Bind Frame.
%%% If there aren't enough rows, new ones are created, and if there
%%% are too many, the scroll bars are adapted so that they are not
%%% visible.

update_bindings(Bindings) when list(Bindings)->
    Rows = length(Bindings) + 1,
    gs:config('BindGrid',{rows,{1,length(Bindings)+1}}),
    update('BindGrid',Bindings,2).

update(Grid,[{Name,Val}|Rest],Row) ->
    Opts = [{text,{1,atom_to_list(Name)}},
	    {text,{2,io_lib:format("~P",[Val,4])}},
	    {doubleclick, true},
	    {data,{print,{Name,Val}}}],
    case gs:read(Grid, {obj_at_row, Row}) of
	undefined ->
	    gs:gridline(Grid,[{row, Row},{height,14}|Opts]);
	GridLine ->
	    gs:config(GridLine,Opts)
    end,
    update(Grid, Rest, Row+1);
update(Grid, [], Row) ->
    delete_items(Grid, Row).

%%% Remove any text from the old lines which have not been reused.

delete_items(Grid, Row) -> 
    case gs:read(Grid, {obj_at_row, Row}) of
	undefined ->
	    ok;
	GridLine ->
	    gs:destroy(GridLine),
	    delete_items(Grid, Row+1)
    end.

%%% calc_columnwidths(Width) -> {columnwdths,[NameWidth,ValueWidth]}
%%% Width: The Size of the Grid.
%%% Given the new width of a grid, calculates two proportional
%%% column widths. There is a catch however. If the grid is too
%%% small, a default minimum size is returned.

calc_columnwidths(Width) ->    
    if Width =< 291 -> 
	    {columnwidths,[90,198]};
       true -> 
	    S = (Width)/(90+198),
	    {columnwidths,[round(90*S),round(198*S)]}
    end.

%%% create_buttons(Daddy,Names).
%%% Daddy: The Id For the Frame or Window the buttons are to be placed in.
%%% Names: A list of names and Ids (Name==Id) for the buttons.
%%% Create the Buttons for the Button Frame.

create_buttons(Daddy,Names) ->
    create_buttons(Daddy,Names,0).

create_buttons(Daddy,[Name|Names],X) ->
    gs:button(Name,Daddy,[{label,{text,Name}},
			  {width,67},
			  {height,24},{x,X},{y,1},{enable,false}]),
    create_buttons(Daddy,Names,X+70);
create_buttons(_,[],_) -> ok.
    
%%% Tests for the possibility of a text document not being
%%% found, patching the atom.

format_text(no_file) -> "File not available or unreadable";
format_text(Text) -> Text.

%%% select_line(Editor,Mode,NewLine,LineNo).
%%% Mode: exit|wait_at|break_at|stack_at
%%% LineNo: Int, the last line an event occured in
%%% NewLine: The Line the new event has occured in.
%%% An event has occured with a certain line. Undo what
%%% had been done in the old line, and update the new one.

select_line(Editor, clear,_Line,OldLine) ->
    refresh_line(Editor, OldLine);
select_line(Editor, Mode,Line,OldLine) ->
    refresh_line(Editor, OldLine),
    Scroll = {vscrollpos,Line-5},
    Text = {overwrite,{{Line,5},mode(Mode)}},
    {Fo,_,Siz} = gs:read(Editor,{font_style,{Line,0}}),
    NewCfg = {font_style,{{{Line,0},{Line,lineend}},{Fo,[bold],Siz}}},
    config_editor(Editor,[Text,NewCfg,Scroll]).


%%% The editor is disabled, so inorder to make changes, the
%%% following function must be used.

config_editor(Editor,Opts) ->
    gs:config(Editor,{enable,true}),
    gs:config(Editor,Opts),
    gs:config(Editor,{enable,false}).


%%% Remove the colour from a line where a break has been
%%% removed.

break_colour(Line,Status, Editor) ->
    config_editor(Editor,{fg,{{{Line,0},{Line,lineend}},
			      br_col(Status)}}).



%%% Remove the text symbols for the breaks in the editor

remove_breaks([{{Mod,Line},_}|Breaks],Mod, Editor) -> 
    remove_break(Line, Editor),
    remove_breaks(Breaks,Mod, Editor);

remove_breaks([_|Breaks],Mod, Editor) -> %%Other module 
    remove_breaks(Breaks,Mod, Editor);
remove_breaks([],_Mod, _) ->
     ok.

remove_break(Line, Editor) ->
    break_colour(Line,no_break, Editor),
    Text = lists:flatten(io_lib:format("~w:   ",[Line])),
    NText = {overwrite,{{Line,0},string:substr(Text,1,5)}},
    config_editor(Editor,[NText]).


add_breaks([{{Mod,Line},[Status|_]}|Breaks],Mod, Editor) -> 
    add_break(Line,Status, Editor),
    add_breaks(Breaks,Mod, Editor);
add_breaks([_|Breaks],Mod, Editor) -> %%Other module 
    add_breaks(Breaks,Mod, Editor);
add_breaks([],_Mod, _Editor) ->
     ok.

add_break(Line,Status, Editor) ->
    config_editor(Editor,[{overwrite,{{Line,0},"-@-  "}}]),
    break_colour(Line,Status, Editor).

%%% Break colours used for colouring lines in the editor

br_col(no_break) -> black;  %%No Break
br_col(active)   -> red;    %%Active Break
br_col(inactive) -> blue.   %%Inactive Break

%%% Refresh the line
%%% FIXME: The second case in the case clause fixes a problem originating in
%%%        gtk_editor/read_option/5. It should of course be handled there
%%%        but I don't know how to do it. /olin

refresh_line(_Editor, 0) -> ok;
refresh_line(Editor, Line) ->

    case gs:read(Editor,{font_style,{Line,0}}) of
	{Fo,_,Siz} ->
	    config_editor(Editor,[{overwrite,{{Line,5},"   "}},
					{font_style,{{{Line,0},{Line,lineend}},
						     {Fo,[],Siz}}}]);
	Otherwise ->
	    config_editor(Editor,[{overwrite,{{Line,5},"   "}},
					{font_style,{{{Line,0},{Line,lineend}},
						     {screen,[],12}}}])
    end.
    



%%% The text symbols for the various modes for the lines in focus
%%% in the editor.

mode(   break) -> "-@-";
mode(    exit) -> "<->";
mode({exit,_}) -> "<->"; 
mode( wait_at) -> "-->";
mode(break_at) -> "-->";
mode(func_at)  -> "-->";
mode(stack_at) -> ">>>";
mode(_Other)   -> "---".
 
%%% Insert some new text in the Code Editor.  (Code Frame)
%%% Split up the string if it is larger than ?STRLEN characters, to avoid
%% blocking the gs port for incoming events.

put_text(Text, Editor) ->
    config_editor(Editor,clear),
    output_text(Text, Editor),
    config_editor(Editor,[{fg,{{{1,0},'end'},black}},
				{font_style,{{{1,0},'end'},{screen,[],12}}}]).
    
    
output_text(Text, Editor) ->
    if
	length(Text) > ?STRLEN ->
	    Str = string:sub_string(Text,1,?STRLEN),
	    config_editor(Editor,{insert,{'end', format_text(Str)}}),
	    output_text(string:sub_string(Text,?STRLEN+1), Editor);
	true ->
	    config_editor(Editor,{insert,{'end', format_text(Text)}})
    end.

%%% Add text in the Trace Editor (Trace Frame)

put_trace(Text,Editor) ->
    Op = [{insert,{'end', format_text(Text)}},
	  {fg,{{{1,0},'end'},black}},
	  {font_style,{{{1,0},'end'},{screen,[],12}}}],
    config_editor(Editor,Op).

%%% Print Text in different formats in the Evaluator
%%% Editor.

print_shell(Text,normal) -> print_shell(Text,[]);
print_shell(Text,bold) -> print_shell(Text,[bold]);
print_shell(Text,Type) ->
    {Fo,_,Siz} = gs:read('ShellEditor',{font_style,{1,0}}),
    Y1 = gs:read('ShellEditor',size),
    config_editor('ShellEditor',{insert, {'end', Text}}),
    Y2 = gs:read('ShellEditor',size),
    config_editor('ShellEditor',
		  [{font_style,{{{Y1,0},{Y2,lineend}},{Fo,Type,Siz}}},
		   {vscrollpos,Y2}] ).

%%% Update the label in the Code Frame.
update_label(Action,Line,Mod) ->
    Str = case Action of
	      idle ->
		  io_lib:format("State: not attached",[]);
	      running ->
		  io_lib:format("State: running [~w.erl]",[Mod]);
%		  io_lib:format("Module:~w   Action:running",[Mod]);
	      wait_at ->
		  io_lib:format("State: receive [~w.erl/~w]",[Mod, Line]);
%		  io_lib:format("Module:~w   Action:receive line:~w",
%				[Mod,Line]);
	      {no,R}   ->
		  gs:config (trace_window, raise),
		  io_lib:format("State: EXITED [uninterpreted], Reason:~w",
				[R]);
%		  io_lib:format("Exited in an uninterpreted module, Reason:~w",
%				[R]);
	      {exit,R}     ->
		  gs:config (trace_window, raise),
		  io_lib:format("State: EXITED [~w.erl/~w], Reason:~w",
				[Mod,Line,R]);
%		  io_lib:format("Module:~w  Exited at line ~w  Reason:~w",
%				[Mod,Line,R]);
	      {text,T}  ->
		  T;
	      _ ->
		  io_lib:format("State: ~w [~w.erl/~w]",
				[Action,Mod,Line])
	  
	  end,
    gs:config(info_window,{label,{text,lists:flatten(Str)}}).

%%% get_position(Object) -> {Row,Column}
%%% Get the cursor position, or if disabled, the last position which
%%% was clicked

get_position(Object) ->
    gs:read(Object,insertpos).

%%% Get the last line number in the Editor.

get_end(Editor) ->
    gs:config(Editor,{enable,true}),
    Pos =  gs:read(Editor,insertpos),
    gs:config(Editor,{insertpos,'end'}),
    {Y,_X} = gs:read(Editor,insertpos),
    gs:config(Editor,{insertpos,Pos}),
    gs:config(Editor,{enable,false}),
    Y.




%%% Special configure function for the View Module Window

configure_view(Window,{X,Y}) ->
    case {gs:read('TraceFrame',width) + 4,
	  gs:read('TraceFrame',height)+ 4} of
	{X,Y} ->
	     ok;
	_ ->
	    gs:config('TraceFrame',[{width,X-4},{height,Y-27}]),
	    configure_editors (X, Y),
	    gs:config(info_window,[{width,X-65}])
    end.


%%% configures the additional editors

configure_editors (X, Y) ->
    gs:config('CodeEditor',[{width,X-14},{height,Y-64}]),
    case catch dbg_ui_cache:get_editors (self ()) of
	[] ->
	    ok;

	Editors when list (Editors) ->
	    configure_editors (Editors, X, Y);
	
	_error ->
	    ok
    end.

configure_editors ([Editor | T], X, Y) ->
    gs:config(Editor, [{width, X - 14}, {height, Y - 64}]),
    configure_editors (T, X, Y);

configure_editors ([], _,_) ->
    ok.


%%%----------------------------------------
%%% resizebar(Flag,Name,X,Y,W,H,Obj) => ResizeBar
%%%   Flag      - open | close
%%%   Name      - atom()
%%%   X, Y      - integer()  Coordinates relative to Obj
%%%   W, H      - integer()  Width and height
%%%   Obj       - GS object
%%%   ResizeBar - GS frame object
%%% Creates a 'resize bar', a frame object over which the cursor will
%%% be of the 'resize' type.
%%%----------------------------------------
resizebar(Flag,Name,X,Y,W,H,Obj) ->
    {W2,H2} = case Flag of
		  open  -> {W,H};
		  close -> {0,0}
	      end,
    gs:create(frame,Name,Obj,[{x,X},{y,Y},{width,W2},{height,H2},
			      {bw,2},
			      {cursor,resize},
			      {data,resizebar},
			      {buttonpress,true},{buttonrelease,true}]).

%%%----------------------------------------
%%% resize(W,ResizeBar,Flags)
%%%   W         - GS window object
%%%   ResizeBar - GS frame object
%%%   Flags     - window flags
%%% Motions event will move the ResizeBar accordingly in W
%%% for a number of HARDWIRED resize bars.
%%% Releasing the mouse button will cause a reconfiguration of the
%%% window, resizing its contents according to the new position of the
%%% resize bar. HARDWIRED as well...
%%%----------------------------------------
resize(W,ResizeBar,Flags) ->		

    %% Listen to motion events
%%    gs:config(W,{motion,true}),
    
    %% Get window dimensions
    Width = gs:read(W,width),
    Height = gs:read(W,height),
    
    %% Call resize loop with min and max for the resize bars derived
    %% from the window dimensions
    resizeloop(W,ResizeBar,Flags,null,rblimits('RB1',Width,Height),
	                              rblimits('RB2',Width,Height),
	                              rblimits('RB3',Width,Height)).

%%%----------------------------------------
%%% resizeloop(W,ResizeBar,Flags,Prev,Limits1,Limits2,Limits3)
%%%   W          - GS window object
%%%   ResizeBar  - GS frame object
%%%   Flags     - window flags
%%%   Prev       - integer()  Previous X/Y coordinate
%%%   LimitsN    - {Min,Max}  Max limits for moving for some HARDWIRED bars
%%%     Min, Max - integer()
%%% Receive motion events and move ResizeBar accordingly.
%%% Call resize_win/2 at mouse button release.
%%%----------------------------------------
resizeloop(W,ResizeBar,Flags,Prev,{Min1,Max1},{Min2,Max2},{Min3,Max3}) ->
    receive
	{gs,_,motion,_,[X,_]} when ResizeBar=='RB3',
				   X>Min3,X<Max3 ->
	    gs:config('RB3',{x,X}),
	    resizeloop(W,'RB3',Flags,X,{Min1,Max1},{Min2,Max2},{Min3,Max3});
	{gs,_,motion,_,_} when ResizeBar=='RB3' ->
	    resizeloop(W,ResizeBar,Flags,
		       Prev,{Min1,Max1},{Min2,Max2},{Min3,Max3});
	
	{gs,_,motion,_,[_,Y]} when ResizeBar=='RB1',
				   Y>Min1,Y<Max1 ->
	    gs:config('RB1',{y,Y}),
	    resizeloop(W,'RB1',Flags,Y,{Min1,Max1},{Min2,Max2},{Min3,Max3});
	{gs,_,motion,_,_} when ResizeBar=='RB1' ->
	    resizeloop(W,'RB1',Flags,
		       Prev,{Min1,Max1},{Min2,Max2},{Min3,Max3});

	
	{gs,_,motion,_,[_,Y]} when ResizeBar=='RB2',
				   Y>Min2,Y<Max2 ->
	    gs:config('RB2',{y,Y}),
	    resizeloop(W,'RB2',Flags,Y,{Min1,Max1},{Min2,Max2},{Min3,Max3});
	{gs,_,motion,_,_} when ResizeBar=='RB2' ->
	    resizeloop(W,'RB2',Flags,
		       Prev,{Min1,Max1},{Min2,Max2},{Min3,Max3});
	 
	{gs,_,buttonrelease,_,_} ->
%%	    gs:config(W,{motion,false}),
	    resize_win(ResizeBar,Prev,Flags)
    end.

%%%----------------------------------------
%%% resize_win(ResizeBar,NewVal,Flags)
%%%   ResizeBar - GS frame object
%%%   NewVal    - integer()
%%%   Flags     - window flags
%%% Resize the window according to the new position NewVal of one of
%%% the HARDWIRED resize bars.
%%%----------------------------------------
resize_win(_RB,null,_Flags) ->
    true;
resize_win('RB1',Y,Flags) ->
    {Bu,S,Bi,F} = Flags,
    H = gs:read('TraceFrame',height),
    Diff = H-(Y-25),

    %% Resize Trace, Shell and Bind frames
    resize_trace(height,-Diff),
    if
	S==close,Bi==close,F==open ->
	    resize_function(open,height,Diff);
	true ->
	    resize_shell(S,height,Diff),
	    resize_bind(Bi,height,Diff)
    end,

    %% Resize vertical resize bar
    case rb3(Flags) of
	open ->
	    gs:config('RB3',{height,gs:read('RB3',height)+Diff});
	close ->
	    true
    end,
    
    %% Adjust the frames' y coordinates
    config_v();

resize_win('RB2',Y,Flags) ->
    {Bu,S,Bi,F} = Flags,
    Prev = gs:read('FunctionFrame',y),
    Diff = Prev-(Y+10),
    
    %% Resize Function, Shell and Bind frames
    resize_function(F,height,Diff),
    resize_shell(S,height,-Diff),
    resize_bind(Bi,height,-Diff),

    %% Resize vertical resize bar
    case rb3(Flags) of
	open ->
	    gs:config('RB3',{height,gs:read('RB3',height)-Diff});
	close ->
	    true
    end,

    %% Adjust the frames' y coordinates
    config_v();

resize_win('RB3',X,Flags) ->
    {Bu,S,Bi,F} = Flags,
    Prev = gs:read('BindFrame',x),
    Diff = Prev-(X+10),
    
    %% Resize Bind and Shell frames
    resize_bind(Bi,width,Diff),
    gs:config('BindGrid',calc_columnwidths(gs:read('BindGrid',width))),
    resize_shell(S,width,-Diff),

    %% Adjust the frames' x coordinates
    config_h().

%%%----------------------------------------
%%% rblimits(ResizeBar,W,H) => {Min,Max}
%%%   ResizeBar - GS frame object
%%%   W, H      - integer()  Window dimensions
%%% Given the window dimensions, return the limits for a HARDWIRED
%%% resize bar.
%%%----------------------------------------
rblimits('RB1',W,H) ->
    
    %% Trace frame should not have height <100
    Min = 126,
    
    %% Max is decided by a minimum distance to 'RB2'
    %% unless 'RB2' is invisible and 'FunctionFrame' is visible
    %% (=> ShellFrame and BindFrame invisible) in which case
    %% the Function frame should not have height <100
    RB2 = gs:read('RB2',height),
    FF = gs:read('FunctionFrame',height),
    Max = case RB2 of
	      0 when FF/=0 ->
		  H-112;
	      _ ->
		  Y = gs:read('RB2',y),
		  max(Min,Y-140)
	  end,
    
    {Min,Max};

rblimits('RB2',W,H) ->

    %% Function frame should not have height <100
    Max = H-112,
    
    %% Min is decided by a minimum distance to 'RB1'
    Y = gs:read('RB1',y),
    Min = min(Max,Y+140),
    
    {Min,Max};

rblimits('RB3',W,H) ->
    
    %% Neither the Shell frame nor the Bind frame should occupy 
    %% less than 1/3 of the total window width and the Shell Frame should
    %% be at least 289 pixels wide
    {max(round(W/3),289),round(2*W/3)}.

%%%----------------------------------------
%%% config_v()
%%% Reconfigure the window vertically.
%%%----------------------------------------
config_v() ->
    Y1 = 25+gs:read('TraceFrame',height),
    gs:config('RB1',{y,Y1}),
    
    Y2 = Y1+gs:read('RB1',height),
    gs:config('ButtFrame',{y,Y2}),
    
    Y3 = Y2+gs:read('ButtFrame',height),
    gs:config('ShellFrame',{y,Y3}),
    gs:config('RB3',{y,Y3}),
    gs:config('BindFrame',{y,Y3}),
    
    Y4 = Y3 + max(gs:read('ShellFrame',height),gs:read('BindFrame',height)),
    gs:config('RB2',{y,Y4}),
    
    Y5 = Y4 + gs:read('RB2',height),
    gs:config('FunctionFrame',{y,Y5}).

%%%----------------------------------------
%%% config_h()
%%% Reconfigure the window horizontally.
%%%----------------------------------------
config_h() ->
    X1 = 2+gs:read('ShellFrame',width),
    gs:config('RB3',{x,X1}),
    
    X2 = X1+gs:read('RB3',width),
    gs:config('BindFrame',{x,X2}).

%%%----------------------------------------
%%% configure(OldSize,NewSize,Flags)
%%%   OldSize, NewSize - {W,H}      Window dimensions
%%%   W, H             - integer()  Width & height
%%%   Flags            - window flags
%%% The window has been resized, now its contents must be resized too.
%%%----------------------------------------
configure({W,H},{W,H},Flags) ->
    true;
configure({_,_},{NewW,NewH},Flags) ->
    {Bu,S,Bi,F} = Flags,
    
    OldW = gs:read('TraceFrame',width) + 4,
    OldH = 25 +
	gs:read('TraceFrame',height) +
	gs:read('RB1',height) +
	gs:read('ButtFrame',height) +
	max(gs:read('ShellFrame',height),
	    gs:read('BindFrame',height)) +
	gs:read('RB2',height) +
	gs:read('FunctionFrame',height),

    %% Adjust width unless it is unchanged or less than minimum width
    if
	OldW/=NewW ->
	    {Dtrace,Dshell,Dbind} = configure_widths(OldW,NewW,Flags),
	    resize_trace(width,Dtrace),
	    case rb1(Flags) of
		open ->
		    gs:config('RB1',{width,gs:read('RB1',width)+Dtrace});
		close ->
		    true
	    end,
	    resize_button(Bu,width,Dtrace),
	    resize_shell(S,width,Dshell),
	    resize_bind(Bi,width,Dbind),
	    if
		Bi==open ->
		    Cols = calc_columnwidths(gs:read('BindGrid',width)),
		    gs:config('BindGrid',Cols);
		true ->
		    true
	    end,
	    case rb2(Flags) of
		open ->
		    gs:config('RB2',{width,gs:read('RB2',width)+Dtrace});
		close ->
		    true
	    end,
	    resize_function(F,width,Dtrace),
	    config_h();
	true ->
	    true
    end,
    
    %% Adjust height unless it is unchanged or less than minimum height
    if
	OldH/=NewH ->
	    {Dtrace2,Dshell2,Dfunction} = configure_heights(OldH,NewH,Flags),
	    resize_trace(height,Dtrace2),
	    resize_shell(S,height,Dshell2),
	    case rb3(Flags) of
		open ->
		    gs:config('RB3',{height,gs:read('RB3',height)+Dshell2});
		close ->
		    true
	    end,
	    resize_bind(Bi,height,Dshell2),
	    resize_function(F,height,Dfunction),
	    config_v();
	true ->
	    true
    end.

%%%----------------------------------------
%%% configure_heights(OldH,NewH,Flags) => {Dtrace,Dshell,Dfunction}
%%%   OldH, NewH                - integer()  Old & new window height
%%%   Dtrace, Dshell, Dfunction - integer()
%%%   Flags                     - window flags
%%% Compute how much the height of each frame must be increased or
%%% decreased in order to adjust to the new window height.
%%%----------------------------------------
configure_heights(OldH,NewH,Flags) ->
    {Bu,S,Bi,F} = Flags,

    %% Difference between old and new height, considering min window height
    MinH = min_height(Flags),
    Diff = abs(max(OldH,MinH)-max(NewH,MinH)),
    
    %% Check how much the frames can be resized in reality
    {T,Sf,Ff} = if
		  %% Window larger
		  NewH>OldH ->
		      {Diff,
		       if
			   S==close,Bi==close ->
			       0;
			   true ->
			       Diff
		       end,
		       if
			   F==open ->
			       Diff;
			   true ->
			       0
		       end};

		  %% Window smaller; get difference between min size
		  %% and current size
		  OldH>NewH ->
		      {gs:read('TraceFrame',height)-100,
		       if
			   S==close,Bi==close ->
			       0;
			   true ->
			       if
				   S==open ->
				       gs:read('ShellFrame',height)-100;
				   Bi==open ->
				       gs:read('BindFrame',height)-100
			       end
		       end,
		       if
			   F==open ->
			       gs:read('FunctionFrame',height)-100;
			   true ->
			       0
		       end}
	      end,
    
    if
	%% Window larger; divide Diff among the frames and return result
	NewH>OldH ->
	    divide([{0,T},{0,Sf},{0,Ff}],Diff);

	%% Window smaller; divide Diff among the frames and return
	%% the inverted result (the frames should shrink)
	OldH>NewH ->
	    {T2,Sf2,Ff2} = divide([{0,T},{0,Sf},{0,Ff}],Diff),
	    {-T2,-Sf2,-Ff2}
    end.

%%%----------------------------------------
%%% configure_widths(OldH,NewH,Flags) => {Dtrace,Dshell,Dbind}
%%%   OldH, NewH                - integer()  Old & new window width
%%%   Dtrace, Dshell, Dbind     - integer()
%%%   Flags                     - window flags
%%% Compute how much the height of each frame must be increased or
%%% decreased in order to adjust to the new window width.
%%%----------------------------------------
configure_widths(OldW,NewW,Flags) ->
    {Bu,S,Bi,F} = Flags,

    %% Difference between old and new width, considering min window width
    Diff = abs(max(OldW,330)-max(NewW,330)),
    
    %% Check how much the frames can be resized in reality
    Limits = if
		 %% Window larger
		 NewW>OldW ->
		     if
			 S==open,Bi==open ->
			     {0,Diff,Diff};
			 S==open ->
			     {0,Diff,0};
			 Bi==open ->
			     {0,0,Diff};
			 true ->
			     {Diff,0,0}
		     end;
		 
		 %% Window smaller; get difference between min size
		 %% and current size
		 OldW>NewW ->
		     if
			 S==open,Bi==open ->
			     {0,
			      gs:read('ShellFrame',width)-204,
			      gs:read('BindFrame',width)-112};
			 S==open ->
			     {0,Diff,0};
			 Bi==open ->
			     {0,0,Diff};
			 true ->
			     {Diff,0,0}
		     end
	     end,
    
    case Limits of

	%% No Shell or Bind frame, larger window
	{T,0,0} when NewW>OldW ->
	    {T,0,0};
	
	%% No Shell or Bind frame, smaller window
	{T,0,0} when OldW>NewW ->
	    {-T,0,0};

	%% Window larger; divide Diff among the frames and return result
	{_,Sf,B} when NewW>OldW ->
	    {_,Sf2,B2} = divide([{0,0},{0,Sf},{0,B}],Diff),
	    {Sf2+B2,Sf2,B2};

	%% Window smaller; divide Diff among the frames and return
	%% the inverted result (the frames should shrink)
	{_,Sf,B} when OldW>NewW ->
	    {_,Sf2,B2} = divide([{0,0},{0,Sf},{0,B}],Diff),
	    {-(Sf2+B2),-Sf2,-B2}
    end.

%%%----------------------------------------
%%% divide([E1,E2,E3],Diff) => {D1,D2,D3}
%%%   E1, E2, E3  - {Curr,Max}
%%%     Curr, Max - integer()
%%%   Diff        - integer()
%%%   D1, D2, D3  - integer()
%%% Try to distribute Diff as evenly as possible between E1, E2 and E3.
%%%----------------------------------------
%%% All elements are 'full'
divide([{T,T},{S,S},{F,F}],_Diff) ->
    {T,S,F};
divide(L,Diff) ->
    [{T,Tmax},{S,Smax},{F,Fmax}] = L,

    %% Count how many elements in L can still be filled
    Rem = remaining(L),
    
    %% Divide Diff by Rem
    D = Diff div Rem,

    if
	%% All of Diff has been distributed
	D==0 ->
	    {T,S,F};
	
	true ->
    
	    %% For each element, try to add as much as possible of D
	    {NewT,Dt} = divide2(D,T,Tmax),
	    {NewS,Ds} = divide2(D,S,Smax),
	    {NewF,Df} = divide2(D,F,Fmax),
    
	    %% Recur with a list of elements with new current values
	    %% and decreased Diff
	    divide([{NewT,Tmax},{NewS,Smax},{NewF,Fmax}],(Diff rem Rem)+Dt+Ds+Df)
    end.

%%%----------------------------------------
%%% remaining(L) => N
%%%   L, NewL     - [{Curr,Max}]
%%%     Curr, Max - integer()
%%%   N           - integer()
%%% Count the number of 'non-filled' elements in L, ie elements where
%%% Curr<Max.
%%%----------------------------------------
remaining([]) ->
    0;
remaining([{Max,Max}|T]) ->
    remaining(T);
remaining([_H|T]) ->
    1 + remaining(T).

%%%----------------------------------------
%%% divide2(Diff,Curr,Max) => {New,Rem}
%%%   Diff,Curr,Max,New,Rem - integer()
%%% New = min(Curr+Diff,Max),
%%% Rem = Max-(Curr+Diff) if Max>(Curr+Diff)
%%%----------------------------------------
divide2(Diff,Max,Max) ->
    {Max,0};
divide2(Diff,Curr,Max) ->
    New = Curr+Diff,
    if
	New>Max ->
	    {Max,New-Max};
	true ->
	    {New,0}
    end.

%%%----------------------------------------
%%% resize_trace(Key,Diff)
%%%   Key  - width | height
%%%   Diff - integer()
%%% Resize the Trace frame and its contents, adding Diff to their width
%%% or height.
%%%----------------------------------------
resize_trace(Key,Diff) ->
    gs:config('TraceFrame',{Key,gs:read('TraceFrame',Key)+Diff}),
    case Key of
	width ->
	    gs:config(info_window,{Key,gs:read(info_window,Key)+Diff});
	_ ->
	    true
    end,
    resize_editors (Key, Diff).


resize_editors (Key, Diff) ->
    gs:config('CodeEditor', {Key, gs:read ('CodeEditor', Key) + Diff}),
    Value = gs:read('CodeEditor', Key),
    case catch dbg_ui_cache:get_editors (self ()) of
	[] ->
	    ok;
	
	Editors when list (Editors) ->
	    resize_editors (Key, Value, Editors);
	
	_error ->
	    ok
    end.

resize_editors (Key, Value, [Editor | T]) ->
    gs:config(Editor, {Key, Value}),
    resize_editors (Key, Value, T);

resize_editors (_Key, _Value, []) ->
    ok.


%%%----------------------------------------
%%% resize_shell(Flag,Key,Diff)
%%%   Flag - open | close
%%%   Key  - width | height
%%%   Diff - integer()
%%% Resize the Shell frame and its contents, adding Diff to their width
%%% or height.
%%%----------------------------------------
resize_shell(close,Key,Diff) ->
    true;
resize_shell(open,Key,Diff) ->
    New = gs:read('ShellFrame',Key)+Diff,
    gs:config('ShellFrame',{Key,New}),
    case Key of
	width ->
	    gs:config('Shell',{width,New-104});
	_ ->
	    true
    end,
    case Key of
	width ->
	    gs:config('ShellEditor',{width,New-9});
	height ->
	    gs:config('ShellEditor',{height,New-40})
    end.

%%%----------------------------------------
%%% resize_bind(Flag,Key,Diff)
%%%   Flag - open | close
%%%   Key  - width | height
%%%   Diff - integer()
%%% Resize the Bind frame and its contents, adding Diff to their width
%%% or height.
%%%----------------------------------------
resize_bind(close,Key,Diff) ->
    true;
resize_bind(open,Key,Diff) ->
    New = gs:read('BindFrame',Key)+Diff,
    gs:config('BindFrame',{Key,New}),
    case Key of
	width ->
	    gs:config('BindGrid',{width,New-8}),
	    Cols = calc_columnwidths(New-8),
	    gs:config('BindGrid',Cols);
	height ->
	    gs:config('BindGrid',{height,New-7})
    end.
    
%%%----------------------------------------
%%% resize_function(Flag,Key,Diff)
%%%   Flag - open | close
%%%   Key  - width | height
%%%   Diff - integer()
%%% Resize the Function frame and its contents, adding Diff to their width
%%% or height.
%%%----------------------------------------
resize_function(close,Key,Diff) ->
    true;
resize_function(open,Key,Diff) ->
    New = gs:read('FunctionFrame',Key)+Diff,
    gs:config('FunctionFrame',{Key,New}),
    case Key of
	width ->
	    gs:config('FunctionEditor',{width,New-10});
	height ->
	    gs:config('FunctionEditor',{height,New-10})
    end.

%%%----------------------------------------
%%% resize_button(Flag,width,Diff)
%%%   Flag - open | close
%%%   Diff - integer()
%%% Resize the Button frame and its contents adding Diff to the width.
%%%----------------------------------------
resize_button(close,width,Diff) ->
    true;
resize_button(open,width,Diff) ->
    Buttons = ['Next', 'Step', 'Finish', 'Continue', 'Up', 'Down', 'Where', 'Break'],
    W = gs:read('ButtFrame', width) + Diff,
    BW = W div length(Buttons),
    resize_buttons(Buttons, BW, 0),
    gs:config('ButtFrame', {width, W}).

%%%----------------------------------------
%%% resize_buttons(Buttons,W,X)
%%%   Buttons  - [Button]
%%%     Button - GS button object
%%%  W, X      - integer()
%%% Configure all buttons in Buttons to a new width W and an x coordinate X
%%% updated accordingly.
%%%----------------------------------------
resize_buttons([Name | T], W, X) ->
    gs:config(Name, [{label, {text, Name}}, {font, {helvetica, 12}}]),
    gs:config(Name, {width, W - 3}),
    gs:config(Name, {x, X}),
    resize_buttons(T, W, X + W);
resize_buttons([], _, _) ->
    true.
    
%%%----------------------------------------
%%% max(A,B) => integer()
%%%   A, B - integer()
%%% Return the max of A and B.
%%%----------------------------------------
max(A,B) ->
    if
	A>B ->
	    A;
	true ->
	    B
    end.

%%%----------------------------------------
%%% min(A,B) => integer()
%%%   A, B - integer()
%%% Return the min of A and B.
%%%----------------------------------------
min(A,B) ->
    if
	A<B ->
	    A;
	true ->
	    B
    end.

%%%----------------------------------------
%%% rb1(Flags) => open | close
%%% Check if 'RB1' should be visible or not.
%%%----------------------------------------
rb1({Bu,S,Bi,F}) ->
    if
	S==close,
	Bi==close,
	F==close ->
	    close;
	true ->
	    open
    end.
    
%%%----------------------------------------
%%% rb2(Flags) => open | close
%%% Check if 'RB3' should be visible or not.
%%%----------------------------------------
rb2({Bu,S,Bi,F}) ->
    if
	F==open ->
	    if
		S==close,
		Bi==close ->
		    close;
		true ->
		    open
	    end;
	true ->
	    close
    end.
    
%%%----------------------------------------
%%% rb3(Flags) => open | close
%%% Check if 'RB3' should be visible or not.
%%%----------------------------------------
rb3({Bu,S,Bi,F}) ->
    if
	S==open,
	Bi==open ->
	    open;
	true ->
	    close
    end.

%%%----------------------------------------
%%% min_height(Flags) => integer()
%%%   Flags - window flags
%%% Compute minimum window height
%%%----------------------------------------
min_height(Flags) ->
    {Bu,S,Bi,F} = Flags,
    H1 = 25 + 100 + 2, % Upper pad + Trace frame + lower pad
    H2 = H1 + bu(Bu) + s_bi(S,Bi) + f(F),
    H3 = case rb1(Flags) of
	     open ->
		 H2+10;
	     close ->
		 H2
	 end,
    H4 = case rb2(Flags) of
	     open ->
		 H3+10;
	     close ->
		 H3
	 end.

bu(close) -> 0;
bu(open)  -> 30.

s_bi(close,close) -> 0;
s_bi(_,_) -> 100.

f(close) -> 0;
f(open)  -> 100.


%%%----------------------------------------
%%% configure(Win,,Frame,State,OldFlags,NewFlags) => {W,H}
%%%   Win      - GS window object
%%%   Frame    - button | shell | bind | function
%%%   State    - open | close
%%%   OldFlags - window flags
%%%   NewFlags - window flags
%%%   W, H     - integer()
%%% A frame should be opened or closed.
%%%----------------------------------------
configure(Win,Frame,open,OldFlags,NewFlags) ->
    {Bu,S,Bi,F} = OldFlags,
    W = gs:read(Win,width),
    H = gs:read(Win,height),
    case Frame of
	button ->
	    
	    %% Adjust Button frame width and height
	    resize_button(open,width,W-4),
	    gs:config('ButtFrame',{height,30}),

	    %% Increase window height
	    gs:config(Win,{height,H+30});
	
	shell when Bi==open ->
	    
	    %% Compute Shell frame width (half window - resize bar - pads)
	    Wnew = round((W-10-4)/2),

	    %% Shell frame height = Bind frame height
	    Hbi = gs:read('BindFrame',height),

	    %% Adjust Shell frame width and height
	    resize_shell(open,width,Wnew),
	    resize_shell(open,height,Hbi),
	    
	    %% Adjust RB3 width and height
	    gs:config('RB3',{width,10}),
	    gs:config('RB3',{height,Hbi}),
	    
	    %% Decrease Bind frame width
	    resize_bind(open,width,Wnew-gs:read('BindFrame',width));
						
	shell ->

	    %% Adjust Shell frame width and height
	    resize_shell(open,width,W-4),
	    resize_shell(open,height,200),

	    %% Adjust window height
	    gs:config(Win,{height,H+200});

	bind when S==open ->

	    %% Compute Bind frame width (half window - resize bar - pads)
	    Wnew = round((W-10-4)/2),

	    %% Bind frame height = Shell frame height
	    Hs = gs:read('ShellFrame',height),

	    %% Adjust Bind frame width and height
	    resize_bind(open,width,Wnew),
	    resize_bind(open,height,Hs),
	    
	    %% Adjust RB3 width and height
	    gs:config('RB3',[{width,10},{height,Hs}]),
	    
	    %% Decrease Shell frame width
	    resize_shell(open,width,Wnew-gs:read('ShellFrame',width));
	    
	bind ->

	    %% Adjust Bind frame width and height
	    resize_bind(open,width,W-4),
	    resize_bind(open,height,200),

	    %% Adjust window height
	    gs:config(Win,{height,H+200});

	function ->
	    
	    %% Adjust Function frame width and height
	    resize_function(open,width,W-4),
	    resize_function(open,height,200),

	    %% Increase window height
	    gs:config(Win,{height,H+200})
    end,

    %% Add RB1 unless its already there
    RB1old = rb1(OldFlags), RB1new = rb1(NewFlags),
    if
	RB1old==close,RB1new==open ->
	    gs:config('RB1',[{width,W-4},{height,10}]),
	    gs:config(Win,{height,gs:read(Win,height)+10});
	true ->
	    true
    end,

    %% Add RB2 unless its already there
    RB2old = rb2(OldFlags), RB2new = rb2(NewFlags),
    if
	RB2old==close,RB2new==open ->
	    gs:config('RB2',[{width,W-4},{height,10}]),
	    gs:config(Win,{height,gs:read(Win,height)+10});
	true ->
	    true
    end,
    
    config_v(),
    config_h(),
    
    {gs:read(Win,width),gs:read(Win,height)};

configure(Win,Frame,close,OldFlags,NewFlags) ->
    {Bu,S,Bi,F} = OldFlags,
    W = gs:read(Win,width),
    H = gs:read(Win,height),
    case Frame of
	button ->
	    
	    %% Adjust Button frame width and height
	    gs:config('ButtFrame',[{width,0},{height,0}]),

	    %% Decrease window height
	    gs:config(Win,{height,H-30});
	
	shell when Bi==open ->

	    %% Adjust Shell frame width and height
	    gs:config('ShellFrame',[{width,0},{height,0}]),
	    
	    %% Adjust RB3 width and height
	    gs:config('RB3',[{width,0},{height,0}]),
	    
	    %% Compute Bind frame width (window - pads)
	    Wnew = W-4,

	    %% Increase Bind frame width
	    resize_bind(open,width,Wnew-gs:read('BindFrame',width));
	    
	shell ->
	    Hs = gs:read('ShellFrame',height),

	    %% Adjust Shell frame width and height
	    gs:config('ShellFrame',[{width,0},{height,0}]),

	    %% Adjust window height
	    gs:config(Win,{height,H-Hs});

	bind when S==open ->

	    %% Adjust Bind frame width and height
	    gs:config('BindFrame',[{width,0},{height,0}]),
	    
	    %% Adjust RB3 width and height
	    gs:config('RB3',[{width,0},{height,0}]),
	    
	    %% Compute Bind frame width (window - pads)
	    Wnew = W-4,

	    %% Increase Shell frame width
	    resize_shell(open,width,Wnew-gs:read('ShellFrame',width));
	    
	bind ->
	    Hbi = gs:read('BindFrame',height),

	    %% Adjust Bind frame width and height
	    gs:config('BindFrame',[{width,0},{height,0}]),

	    %% Adjust window height
	    gs:config(Win,{height,H-Hbi});

	function ->
	    Hf = gs:read('FunctionFrame',height),
	    
	    %% Adjust Function frame width and height
	    gs:config('FunctionFrame',[{width,0},{height,0}]),

	    %% Adjust window height
	    gs:config(Win,{height,H-Hf})
    end,
    
    %% Remove RB1 if it is visible
    RB1old = rb1(OldFlags), RB1new = rb1(NewFlags),
    if
	RB1old==open,RB1new==close ->
	    gs:config('RB1',[{width,0},{height,0}]),
	    gs:config(Win,{height,gs:read(Win,height)-10});
	true ->
	    true
    end,

    %% Remove RB2 if it is visible
    RB2old = rb2(OldFlags), RB2new = rb2(NewFlags),
    if
	RB2old==open,RB2new==close ->		
	    gs:config('RB2',[{width,0},{height,0}]),
	    gs:config(Win,{height,gs:read(Win,height)-10});
	true ->
	    true
    end,

    config_v(),
    config_h(),
    
    {gs:read(Win,width),gs:read(Win,height)}.
