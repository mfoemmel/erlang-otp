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
% ------------------------------------------------------------
%% Purpose:  window management and the gs interface
%%           for the monitor window in the debugger

-module(dbg_ui_mon_win).
-export([
	 change_colour/3,
	 config_grid/2,
	 config_win/3,
	 delete_items/2,
	 mon_title/0,
	 mon_window/1,
	 update_backtrace_menu/1,
	 update_field/4,
	 update_pids/4
	]).

%%% -----------------------------------------------------------------
%%% INTERFACE: mon_window(Processes) -> {WinId,GridId}
%%%
%%%   Processes: The attached processes & their status information
%%%              Start the monitor Window, iserting the info in
%%%              Processes in the grid.
%%%
%%% Create a monitor window with menus and grid and call 'update_pids'
%%% to fill it with data. Returns GS references {Window,Grid}
%%% -----------------------------------------------------------------

mon_window(Processes) ->
    GS = gs:start([{kernel,true}]),
   
    Win_Options =
	[{title,mon_title()},
	 {width, 745},
	 {height,390},
	 {configure,true},
	 {motion,true},
	 {destroy,true},
	 {keypress,true}],

    Win = gs:window(GS,Win_Options),

    MenuBar = gs:menubar(Win, []),

    help_menu(MenuBar),
    file_menu(MenuBar),
    edit_menu(MenuBar),
    dbg_ui_aux:module_menu(MenuBar),
    process_menu(MenuBar),
    breaks_menu(MenuBar),
    options_menu(MenuBar),
        
    dbg_ui_winman:windows_menu (MenuBar),

    Grid_Options = 
	[{x,3},
	 {y,40},
	 {height,348},
	 {width,739},
	 {fg,black},
	 {bg,grey},
	 {vscroll,right},
	 %%{font,{screen,[bold],14}},
	 {hscroll,bottom},
	 calc_columnwidths(739), 
	 {rows, {1,length(Processes)+50}}], 

    Grid = gs:grid(Win,Grid_Options),

    update_pids(Grid,Processes,length(Processes),1),

    gs:config(Win, {map, true}),
    {Win, Grid}.


%% ---------------------------------------------------------------
%% Create the menus
%% ---------------------------------------------------------------

help_menu(MenuBar) ->
    MenuButtHelp = gs:menubutton(MenuBar, [{label, {text, " Help "}},
					   {underline, 1},
					   {side, right}]),
    MenuHelp = gs:menu(MenuButtHelp, []),
    gs:menuitem('Help', MenuHelp,
		[{label, {text, "Help"}},
		 {underline, 0}]).


file_menu(MenuBar) ->
    MenuButtFile = gs:menubutton(MenuBar, [{label, {text, " File "}},
					   {underline, 1}]),
    MenuFile = gs:menu(MenuButtFile, []),
    gs:menuitem('Load',MenuFile,
		[{label, {text, "Load Settings"}},{underline,0}]),
    gs:menuitem('Save',MenuFile,
		[{label, {text, "Save Settings"}},{underline,0}]),
    gs:menuitem(MenuFile, [{itemtype,separator}]),
    gs:menuitem('Exit', MenuFile,[{label, {text, "Exit"}},{underline,1}]).


edit_menu(MenuBar) ->
    MenuButtEdit= gs:menubutton(MenuBar, [{label, {text, " Edit "}},
					  {underline, 1}]),
    MenuEdit = gs:menu(MenuButtEdit, []),
    gs:menuitem('Clear',MenuEdit, [{label, {text, "Clear"}},{underline,0}]),
    gs:menuitem('Kill All',MenuEdit,[{label, {text, "Kill All"}},
				     {underline, 0},
				     {enable,true}]).


process_menu(MenuBar) ->
    MenuButtProcess = gs:menubutton(MenuBar,[{label, {text, " Process "}},
					     {underline, 1}]),
    MenuProcess = gs:menu(MenuButtProcess, []),
    gs:menuitem('Step',MenuProcess,
		[{label, {text, "Step ctrl-z"}},{underline,0},{enable,true}]),
    gs:menuitem('Next',MenuProcess,
		[{label, {text, "Next"}},{underline,0},{enable,true}]),
    gs:menuitem('Continue',MenuProcess,
		[{label, {text, "Continue"}},{underline,0},{enable,true}]),
    gs:menuitem('Finish',MenuProcess,
		[{label, {text, "Finish"}},{underline,0},{enable,true}]),

    gs:menuitem(MenuProcess,[{itemtype,separator}]),
    gs:menuitem('Trace',MenuProcess,
		[{label,{text, "Attach"}},{underline,0}]),
    gs:menuitem('Kill',MenuProcess,
		[{label, {text, "Kill"}},{underline,0},{enable,true}]).

breaks_menu(MenuBar) ->
    MenuButtBreaks = gs:menubutton(MenuBar, [{label, {text, " Breaks "}},
					     {underline, 1}]),
    gs:menu('MenuBreaks',MenuButtBreaks, []),
    gs:menuitem('Normal Break','MenuBreaks',
		[{label,{text, "Line Break..."}},
		 {underline,0}]),
    gs:menuitem('Conditional Break','MenuBreaks',
		[{label,{text, "Conditional Break..."}},
		 {underline,0}]),    
    gs:menuitem('Functional Break','MenuBreaks',
		[{label,{text, "Function Break..."}},
		{underline,0}]),
    gs:menuitem('MenuBreaks',[{itemtype,separator}]),
    gs:menuitem('Delete All Breaks','MenuBreaks',
		[{label,{text, "Delete All"}},{underline,0},
		 {enable,false}]).

options_menu(MenuBar) ->
    MenuButtOptions = gs:menubutton(MenuBar, [{label, {text, " Options "}},
					      {underline, 1}]),
    MenuOptions = gs:menu(MenuButtOptions, []),
    gs:menuitem('Reset Options',MenuOptions, 
		[{label, {text, "Reset Options"}}, {underline, 0}]),
    gs:menuitem(MenuOptions, [{itemtype,separator}]),
    gs:menuitem('Button Frame',MenuOptions,
		[{label,{text,"Button Frame"}},
		 {underline, 1},
		 {select,false},
		 {itemtype,check}]),
    gs:menuitem('Evaluator Frame',MenuOptions,
		[{label,{text, "Evaluator Frame"}},
		 {underline,0},
		 {select,true},
		 {itemtype, check}]),
    gs:menuitem('Bindings Frame',MenuOptions,
		[{label,{text, "Bindings Frame"}},
		 {underline, 0},
		 {select,true},
		 {itemtype, check}]),
    gs:menuitem('Trace Flag',MenuOptions,
	      [{label,{text, "Trace Frame"}},
	       {underline,0},
	       {select,false},
	       {itemtype, check}]),
    
    gs:menuitem(MenuOptions,[{itemtype,separator}]),
    gs:menuitem('Back Trace',MenuOptions,
		[{label,{text, "Back Trace Size:"}},
		 {underline, 13}]),
    AttachButt = gs:menuitem(MenuOptions,
			     [{label,{text, "Attach"}},
			      {underline,0},
			      {itemtype, cascade}]),
    AttachMenu = gs:menu(AttachButt,[]),
    gs:menuitem('First Call',AttachMenu,
		[{label,{text,"First Call"}},
		 {underline, 0},
		 {select,false},
		 {itemtype, check}]),
    gs:menuitem('On Exit',AttachMenu,
		[{label,{text,"On Exit"}},
		 {underline, 4},
		 {select,false},{itemtype, check}]),
    gs:menuitem('On Break',AttachMenu,
		[{label,{text,"On Break"}},
		 {underline, 3},
		 {select,false},{itemtype, check}]),
    gs:menuitem('Attach',AttachMenu,[{label,{text,"All"}},
				    {underline, 0}]),
    gs:menuitem('Never Attach',AttachMenu,[{label,{text,"Never"}},
					  {underline, 0}]),

    StackButt = gs:menuitem('StackMenu',MenuOptions,
			    [{label,{text,"Stack Options"}},
			     {underline,0},
			     {itemtype, cascade}]),
    StackMenu = gs:menu(StackButt,[]),
    gs:menuitem('Stack Tail',StackMenu,
		[{label,{text,"Stack On, Tail"}},
		 {underline, 10},
		 {select,false},
		 {itemtype, radio},{group,stack}]),
    gs:menuitem('Stack',StackMenu,
		[{label,{text, "Stack On, no Tail"}},
		 {underline, 10},
		 {select,false},
		 {itemtype, radio},{group,stack}]),
    gs:menuitem('Stack Off',StackMenu,
		[{label,{text, "Stack Off"}},
		 {underline, 6},
		 {select,false},
		 {itemtype, radio},{group,stack}]).



%%% windows_menu
%%%
% FIXME: Remove this.
%windows_menu (MenuBar, Win) ->
%    MenuButtWindows = gs:menubutton(MenuBar, [{label, {text, " Windows "}},
%					      {underline, 1}]),
%    gs:menu('WindowsMenu', MenuButtWindows, []).
    


%%% -----------------------------------------------------------------
%%% INTERFACE: ?????
%%% -----------------------------------------------------------------

update_backtrace_menu(Size) ->
    Text = io_lib:format("Back Trace Size: ~p...",[Size]),
    gs:config('Back Trace',{label,{text,Text}}).

%%% Create the title of the window, depended on if we are 
%%% distributed or not.

mon_title() ->
    case node() of
	nonode@nohost -> "Monitor";
	Name          -> io_lib:format("Monitor on ~p",[Name])
    end.

%%% -----------------------------------------------------------------
%%% INTERFACE: update_pids(Grid,Items,Amount,Curr)
%%%
%%% Update the monitor grid with fresh information on all the pids.
%%% Creates new rows if there are not enough available, and calls
%%% update which handles the actual writing in the grid.
%%% Data representing the state is formatted accordingly.
%%% -----------------------------------------------------------------
	
update_pids(Grid,Items,Amount,Curr) -> 
    gs:config(Grid,{rows,{1,Amount+1}}),
    update(Grid,Items,1,Curr).



update(Grid,[Item|Items],Last,Curr) ->
    {Row,Pid,_,Func,Status,Info,_,Name} = Item,
    Colour = colour(Row,Curr),
    Ninfo = case Info of
		{} ->
		    "";
		_  ->
		    Info
	    end,
    Conf = [{text,{1,Pid}},
	    {text,{2,Func}},
	    {text,{3,Name}},
	    {text,{4,Status}},
	    {text,{5,Ninfo}},
	    {fg,Colour},
	    {bw,5}],

    GLRet = gs:read(Grid,{obj_at_row,Row}),
    
    %% Set GridLine to old gridline, or a newly created one.
    GridLine =
	case GLRet of

	    %% No gridline at Row of the Grid. Create one.
	    undefined ->
		gs:gridline(Grid,[{row,Row},
				  {doubleclick,true},
				  {click,true}]);

	    %% There is a gridline at Row, simply reconfigure it.
	    GL ->
		GL
    end,
    DataConf = {data,{pidfunc,Pid,Status}},
    ok = gs:config(GridLine,DataConf),
    ok = gs:config(GridLine,Conf),


    update(Grid,Items,Row+1,Curr);

update(Grid,[],Row,_Curr) ->

    delete_items(Grid,Row).

%% -----------------------------------------------------------------
%% INTERFACE: ?????
%% -----------------------------------------------------------------

update_field(Grid,{Col,Row},Txt,Data) ->
    GridLine = gs:read(Grid,{obj_at_row,Row}),
    gs:config(GridLine,[{text,{Col,Txt}},{data,Data}]).

%% -----------------------------------------------------------------
%% Colour for the item in focus in the grid.
%% -----------------------------------------------------------------

colour(1,   _) -> blue;
colour(Row, Row) -> white;
colour(From, To) -> black.

%% ---------------------------------------------------------------
%% INTERFACE: Interchange colours between two rows
%% ---------------------------------------------------------------

change_colour(Grid,From,To) ->
    Gitem_to = gs:read(Grid,{obj_at_row,To}),
    Gitem_fr = gs:read(Grid,{obj_at_row,From}),
    gs:config(Gitem_to,{fg,colour(To,To)}),
    gs:config(Gitem_fr,{fg,colour(From,To)}).


%% ---------------------------------------------------------------
%% INTERFACE: Delete items in the grid
%% ---------------------------------------------------------------

delete_items(Grid,Row) ->
    case gs:read(Grid, {obj_at_row, Row}) of
	undefined ->
	    ok;
	GridLine ->
	    gs:destroy(GridLine),
	    delete_items(Grid,Row+1)
    end.






%% ---------------------------------------------------------------
%% INTERFACE: Resize the window if the size has changed.
%% ---------------------------------------------------------------

config_win(Window, Object, {X, Y}) ->
    Dx = abs(X - gs:read(Object,width) - 4),
    Dy = abs(Y - gs:read(Object,height) - 42),
    if Dx + Dy =/= 0 ->
	    gs:config(Object,[{width,X-4},{height,Y-42}]);
       true ->
	    ok
    end.

%% ---------------------------------------------------------------
%% INTERFACE:
%% Resize the grid if the window size has changed. Columns must be
%% resized proportionaly too.
%% ---------------------------------------------------------------

config_grid(Grid,{X,Y}) ->
    case abs(X - gs:read(Grid,width) - 6) of
	0 -> 
	    ok;   %%Avoid refreshig width. Takes lots of processos power
	_ -> Cols = calc_columnwidths(X-6),
	     gs:config(Grid,Cols)
    end.

calc_columnwidths(Width) ->
    if Width =< 739 -> 
	    {columnwidths,[75,215,146,150,150]};
       true -> 
	    S = (Width - 75)/(215+146+150+150),
	    {columnwidths,[75,round(215*S),round(146*S), round(150*S),
			   round(150*S)]}
    end.

