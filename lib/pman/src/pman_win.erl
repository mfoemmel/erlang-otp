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
%% ------------------------------------------------------------
%% Purpose:  window management and the gs interface
%% ------------------------------------------------------------

-module(pman_win).

%% ---------------------------------------------------------------
%% The user interface exports 
%% ---------------------------------------------------------------

-export([pman_window/3, window/1, module_data/1, display/1, format/2,
	 addrows/2,
	 dialog_window/2, read_flags/0, configeditor/2, configwin/3,
	 info/2, info/4,
	 uppdate/3,
	 msg_win/2, msg_win/1, title/1,
	 remove_menu/1, add_menu/3,
	 change_colour/3,links_menus/1,calc_columnwidths/1]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Constants
%%
-include("pman_win.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% pman_window/3 - Create a GS window and components for the
%%   PMAN overview window, the main window.
%%
%% Arguments:
%%   Size		number of processes
%%   HiddenModules	list of modules 
%%   Nodes		list of supervised nodes
%%
%% Return: 
%%   {Win, Grid, Procnum} where
%%   Win	The GS top window
%%   Grid	The GS grid
%%   Procnum	Number of displayed processes
%%

pman_window(Size, HiddenModules, Nodes) ->
    GS = gs:start([{kernel,true}]),
    Win_Options = [{title, lists:concat(["PMAN:Overview on ", node()])},
		   {width, ?WIN_WIDTH}, {height, ?WIN_HEIGHT},
		   {destroy, true},
		   {keypress,true}],
    Win = gs:create(window, GS,Win_Options),


    %% Menu bar 
    MenuBar = gs:create(menubar, Win, []),
    MenuButtFile =gs:create(menubutton, MenuBar,[{label,{text, " File "}},
						 {underline,1}]),
    MenuButtView=gs:create(menubutton, MenuBar,[{label,{text, " View "}},
						{underline,1}]),
    MenuButtTrace=gs:create(menubutton, MenuBar,[{label,{text, " Trace "}},
						 {underline,1}]),
    MenuButtHelp=gs:create(menubutton, MenuBar, [{label, {text, " Help "}},
						 {side,right},
						 {underline,1}]),

    %% Addition of a menu for distribution
    add_node_menu(MenuBar,Nodes),

    %% All menu buttons
    MenuFile = gs:create(menu, MenuButtFile, []),
    MenuView = gs:create(menu, MenuButtView, []),
    MenuTrace = gs:create(menu, MenuButtTrace, []),
    MenuHelp = gs:create(menu, MenuButtHelp, []),

    %% File menu
    
    gse:named_menuitem('Default Options',MenuFile,  % Keeping Def. Opts. So it won't
		       [{label,{text,"Options..."}},% interfere with Trace's Opts.
			{underline,0}]),
    gse:named_menuitem('Save Options',MenuFile,
		       [{label,{text,"Save Options"}},
			{underline,0}]),
    gse:named_menuitem('Exit', MenuFile,
		       [{label,{text,"Exit"}},
			{underline,0}]),

    %% View menu

    gse:named_menuitem('Hide All',MenuView,
		       [{label, {text, "Hide All Processes"}},
			{underline,5}]),

    gse:named_menuitem('Hide Modules', MenuView,
		       [{label, {text, "Hide Modules..."}},
			{underline,5}]),

    gse:named_menuitem('Hide Selected Process', MenuView,
		       [{label, {text, "Hide Selected Process"}},
			{underline,0}]),

    gse:named_menuitem('Module',MenuView,
		       [{label, {text, "Module info"}},
			{underline,7}]),

    gse:named_menuitem('Refresh', MenuView,
		       [{label, {text, "Refresh"}},
			{underline,0}]),

    gse:named_menuitem('Show All',MenuView,
		       [{label, {text, "Show All Processes"}},
			{underline,0}]),

    gse:named_menuitem('Show Selected',MenuView,
		       [{label, {text, "Show Processes..."}},
			{underline,5}]),

    

    %% Trace menu
    gs:create(menuitem,'Kill', MenuTrace,[{label, {text, "Kill"}},
					 {underline,0}]),

    gs:create(menuitem,'Trace Process',
	      MenuTrace,[{label, {text, "Selected Process"}},
					   {underline,9}]),

    gs:create(menuitem,'Trace Shell',
	      MenuTrace,[{label, {text,"Shell Process"}},
					   {underline,0}]),


    %% Help menu
    gs:create(menuitem,'Help', MenuHelp,  [{label, {text, "Help" }},
					   {underline,0}]),

    %% Window contents

    %% Geometry managing frame

    Frame = gse:frame(Win, [{y,?MENU_HEIGHT},
			    {packer_x,[{stretch, 1}]},
			    {packer_y,[{stretch,10},
				       {fixed,?CHECKBAR_HEIGHT}]}]),



    %% Grid 
    Grid_Options = [
%		    {x,3}, {y,?MENU_HEIGHT},
%		    {height,?WIN_HEIGHT-?MENU_HEIGHT-?CHECKBAR_HEIGHT},
%		    {width,?WIN_WIDTH-6},
		    {pack_x,1}, {pack_y,1},
		    {fg,black}, 
		    {vscroll,right},{hscroll,bottom},
		    calc_columnwidths(739),
		    {rows, {1,Size}}], 
    Grid = gse:grid(Frame,Grid_Options),


    %% Checkbutton bar at the bottom of the window

    CheckBar = gse:frame(Frame, [{pack_x,1},
				 {pack_y,2},
				 {packer_x,[{stretch, 2, 100,300},
					    {stretch, 2, 100,300},
					    {stretch,1},
					    {stretch, 2,100,300}]},
				 {packer_y,[{stretch,1}]}]),
    gse:named_checkbutton('Hide System',CheckBar,
			  [{pack_xy,{1,1}},
			   {justify, left},
			   {align,w},
			   {width, 200},
			   {label, {text, "Hide System processes" }}]),
    
    gse:named_checkbutton('Auto Hide New',CheckBar,
			  [{pack_xy,{2,1}},
			   {width, 200},
			   {justify, left},
			   {align,w},
			   {label, {text, "Auto-Hide New" }}]),
    
    gse:named_label('Number Hidden',CheckBar,
			  [{pack_xy,{4,1}},
			   {justify, left},
			   {align,w},
			   {width, 200},
			   {label, {text, ?CPIDHIDDENTEXT }}]),
    

    

    %% Finalize it!
    gse:map(Win),
    gse:config(Win,[raise]),
    gse:config(Win,[{configure,true}]),


    {Win, Grid, Frame, length(processes()) + 1}.


%%Calculate columnwidths in respect to the size of the window.

calc_columnwidths(Width) ->
    if Width =< 739 -> 
	    {columnwidths,[75,215,146,90,105,105]};
       true -> 
	    S = (Width - 75)/(215+146+90+105+105),
	    {columnwidths,[75,round(215*S),round(146*S),round(90*S),
			   round(105*S),round(105*S)]}
    end.

%% ---------------------------------------------------------------
%% Create a trace window
%%
%% Process, a process id or an atom
%%
%% Return: A window and a editor
%% ---------------------------------------------------------------


window(Process) ->
    GS = gs:start([{kernel,true}]),
    Win_Options = [{title,title(Process)}, {width,550}, {keypress,true}, 
		   {configure,true},
		   {destroy,true},{height, 400}],
    Win = gs:create(window,GS,Win_Options),
    
    MenuBar = gs:create(menubar, Win, []),

    %% File menu
    MenuButtFile =  gs:create(menubutton,MenuBar,[{label,{text," File "}},
						 {underline, 1}]),
    MenuFile = gs:create(menu, MenuButtFile, []),
    make_menus(pman_process:is_running(Process),MenuBar,MenuFile),

    gse:named_menuitem('Save buffer',MenuFile,
		       [{label,{text, "Save buffer..."}},
			{underline,0}]),
    gse:named_menuitem('Close',MenuFile,
		       [{label, {text, "Close"}},
			{underline,0}]),


    Editor = gs:create(editor,Win,[{x,3},{y,40},{width, 546}, {height, 348}]),
    gs:config(Editor, [{keypress, true},{insert, {'end', display(Process)}}]),
    gs:config(Editor, [{enable, false},{vscroll, right}, {hscroll, bottom},
		       {wrap,none}]),
    gs:config(Win, [{map, true}]),
    {Win, Editor}.

%% ---------------------------------------------------------------------
%% Menu Help Fuctions
%% ---------------------------------------------------------------------


links_menus(Links) ->
    gs:destroy('Links'),
    gs:create(menu,'Links','LinksMenu',[]),
    Flag =  case links_menus(Links,[]) of
		[] -> false;
		Pids -> add_menu('Links',Pids,"Trace"),
			true
	    end,
    gse:config('LinksMenu',[{enable,Flag}]).
	    
links_menus([],Pids) -> Pids;
links_menus([Pid|Links],Pids) when pid(Pid) ->
    links_menus(Links,[Pid|Pids]);
links_menus([_Port|Links],Pids) ->
    links_menus(Links,Pids).


%% Create the node menu. 

add_node_menu(MenuBar,Nodes) ->
    MenuButtNode=gs:create(menubutton, MenuBar, [{label, {text, " Nodes "}},
						{underline, 1}]),
    MenuNode = gs:create(menu, node, MenuButtNode, []),
    add_menu(node,Nodes,"Show"),
    gse:disable(node()).


%% ----------------------------------------------------------------------
%% Add Menus in the list under Menu menuitem.


add_menu(_,      [], _) -> ok;
add_menu(Menu,[Name|Names], Tag) ->
    Title = io_lib:format("~s ~p",[Tag, Name]),
    gs:create(menuitem,Name,Menu,[{label,{text,Title}},{data,{Menu,Name}}]),
    add_menu(Menu,Names,Tag);
add_menu(_, _, _) -> ok.  %%Not a list, ie allmods == do nothing


%% -----------------------------------------------------------------------
%% Remove a specific menu item, or a whole menu, or a list of both.
%%

remove_menu(List) when list(List)->
    lists:foreach({gs,destroy}, List);

remove_menu(Object) ->
    gse:destroy(Object).

%%remove_menu([]) ->  ok;
%%remove_menu([Id|Ids]) ->
%%    gs:destroy(Id),
%%    remove_menu(Ids);
%%remove_menu(Id) ->
%%    gs:destroy(Id).


%% -----------------------------------------------------------------------
%% Enable and disable menus Items

%enable_list(List) ->
%    lists:foreach({gse,enable},List).
%
%disable_list(List) ->
%    lists:foreach({gse,disable},List).

menus_enable([],_) -> ok;
menus_enable([Menu|Menus],Bool) -> 
    menu_enable(Menu,Bool),
    menus_enable(Menus,Bool).

menu_enable(Menu_id,Bool) ->
    gs:config(Menu_id,{enable,Bool}).


%% -----------------------------------------------------------------------
%% If the trace window opened is supposed to trace a real pid, let us add 
%% the trace menu, and other items specific to tracing. If not, the only
%% menus available are the ones in the default defined in window(Pid).

make_menus(false,_,_) -> ok;
make_menus({true,Pid},MenuBar,MenuFile) ->
    MenuButtView = gs:create(menubutton,'ViewMenu',MenuBar,
				[{underline,1},
				 {label,{text," View "}},
				 {side,left}]),
    MenuView = gs:create(menu, MenuButtView, []),

    MenuButtTrace = gs:create(menubutton,'TraceMenu',MenuBar,
			      [{underline,1},
			       {label,{text," Trace "}},
			       {side,left}]),
    MenuTrace = gs:create(menu, MenuButtTrace, []),


    MenuButtHelp =  gs:create(menubutton,'HelpMenu',MenuBar,
			      [{underline,1},
			       {label,{text," Help "}},
			       {side,right}]),
    MenuHelp = gs:create(menu, MenuButtHelp, []),

    %% File menu
    gse:named_menuitem('Options', MenuFile,
		       [{label, {text, "Options..."}},{underline,0}]),

    %% Trace menu

    gse:named_menuitem('All Links', MenuTrace,
		       [{label, {text, "All Linked Processes"}},
			{underline,0}]),
    gse:named_menuitem('LinksMenu', MenuTrace,
		       [{underline,0},
			{label, {text, "Linked Process..."}},
			{itemtype, cascade},
			{enable,false}]),
    gs:create(menu,'Links','LinksMenu',[]),
    case  catch pman_process:pinfo(Pid,links) of
	{links,Links} ->
	    links_menus(Links);
	dead ->
	    lists:foreach({gse, disable},['LinksMenu'])
    end,
    gse:named_menuitem('Kill', MenuTrace,
		       [{label, {text, "Kill"}},{underline,0}]),


    %% View menu

    gse:named_menuitem('Clear', MenuView,
		       [{label, {text, "Clear buffer"}},{underline,0}]),

    gse:named_menuitem('Module', MenuView,
		       [{label, {text, "Module Info"}},{underline,0}]),



    %% Help menu

    gse:named_menuitem('Help', MenuHelp,
		       [{label, {text, "Help"}},{underline,0}]).

%% ---------------------------------------------------------------------
%% Configurate the actual editor
%%
%% Editor, actual editor
%% Options, actual options for the editor
%%
%% Return: A configurated editor with the actual options
%% ---------------------------------------------------------------------

configeditor(Editor, Options) ->
    gs:config(Editor, Options).

%% ---------------------------------------------------------------------
%% Configure the actual window after it has been resized.
%% ---------------------------------------------------------------------

configwin(Object, W, H) ->
    Dx = abs(W - gs:read(Object,width) - 4),
    Dy = abs(H - gs:read(Object,height) - 42),
    if Dx + Dy =/= 0 ->
	    gs:config(Object,[{width,W - 4}]);

    true ->
	    ok
    end.


%% ---------------------------------------------------------------------
%% Create process and module information for the actual grid
%%
%% Arguments:
%%   Procs		A list of (active) processes.
%%   
%%   ExcludedModules	A list of modules. We do not want information
%% 			about processes running code from these modules.
%%
%% Returns:
%%   A list of process and information about them info
%%

info(Procs,  ExcludedModules) ->
    info(Procs, [],ExcludedModules, []).
					 
%% Add the heading of the process overview grid.
%% And 

info([], Mods, _ExcludedModules, Ack) ->
    addrows(1,[{'Pid','Current Function' ,'Name',
		'Msgs', 'Reds', 'Size'} | Ack]);


%% Recurse through all 
info([P |Tail], Mods, ExcludedModules,Ack) ->
    case catch compile_info(P,Mods,ExcludedModules) of
	{true,T} ->
	    info(Tail,Mods, ExcludedModules,[T|Ack]);
	_Otherwise ->
	    info(Tail,Mods, ExcludedModules,Ack)
    end.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% compile_info/2 - Returns a tuple with procecss information,
%%   if the specified process is to be shown.
%%
%% Arguments:
%%   P		Process specifier
%%   Mods	(???)
%%   ExcludedModules	A list of modules. Processes executing code from
%%             		these modules are excluded.
%%
%% Returns:
%%   {true, Result}  	If the process is to be shown
%%   false  		If the process is not to be shown
%% 

compile_info(P,Mods,ExcludedModules) ->
    Call = pman_process:function_info(P),
    Reds = pman_process:pinfo_notag(P,reductions),
    Name = pman_process:get_name(P),
    Msg = pman_process:msg(P),
    Size = pman_process:psize(P),
    case pman_primitive:seemod(catch(element(1, Call)), Mods, ExcludedModules) of
	true ->
	    {true,{P,Call, Name, Msg, Reds, Size}};
	false ->
	    false
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% uppdate/2 - Updates the list of processes in the grid.
%%
%% Arguments:
%%   Grid		The GS grid object
%%   OSPidDisplay	An Ordset of Pid:s to display
%%   CPidHidden		Number of Pid:s that are not shown
%%
%% Returns:
%%   Rows		The number of gridlines in the GS grid object
%%

uppdate(Grid,OSPidDisplay, CPidHidden) ->

    %% We reverse the list because we want the processes to appear with the
    %% newest (=highest) PID first in the list.
    OSPidDisplayRev = lists:reverse(OSPidDisplay),

    %% Set the length of the grid
    CGridline = length(OSPidDisplayRev) + 1,
    gs:config(Grid, [{rows, {1,CGridline}}]),

    %% Add the header line 
    add_gridline(Grid,
		 1,
		 {'Pid',
		  'Current Function' ,
		  'Name',
		  'Msgs',
		  'Reds',
		  'Size'},
		 []),

    %% Recurse through the ordset of PIDs
    uppdate_r(Grid,OSPidDisplayRev,2),
    
    %% Update the Hidden field

    STHidden =
	lists:flatten(io_lib:format(?CPIDHIDDENTEXT ++ "~w", [CPidHidden])),
    gse:config('Number Hidden', [{label, {text, STHidden}}]),


    
    CGridline.

%% (???) Terminal case, end of pid list
uppdate_r(Grid, [], Row) -> 
    delete_items(Grid, Row);


uppdate_r(Grid, [Pid|OSPid], Row)  -> 
    case catch addrow(Pid, Row, Grid) of
	
	%% The process we want to display information about may
	%% have died since we found out about it's existence.
	%% In that case the pman_process-functions called in addrow/3
	%% will 'EXIT'. 
	{'EXIT', Reason} ->
	    uppdate_r(Grid, OSPid, Row);

	%% Normal return from addrow/3.

	_Otherwise ->
	     uppdate_r(Grid, OSPid, Row+1)
    end.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% addrow/3 - Adds a gridline with process data to the grid.
%% 
%% Arguments:
%%   Pid	The process to show data for
%%   Row	The row to place the info in
%%   Grid	The GS grid object.
%% 
%% Returns:
%%   Undefined return value.
%%   

addrow(Pid, Row, Grid) when pid(Pid) ->

    TupleProcinfo = show(Pid),

    {_Pid,Func,Name,Msgs,Reds,Psize} = TupleProcinfo,

    FuncText =
	case Func of
	    {Module, Function, Arity} ->
		lists:flatten(io_lib:format("~w:~w/~w",[Module,
							Function,
							Arity]));
	    Anything -> Anything
	end,

    %% 
    LIOptSpec = [{data,{pidfunc,Pid,Func}}],
    
    add_gridline(Grid,
		 Row, 
		 {Pid, FuncText, Name, Msgs, Reds, Psize},
		LIOptSpec).


%% add_gridline/4 - Conditionally adds a gridline at the specified row
%%
%% Arguments: 
%%   Grid		The GS grid object.
%%   Row		Row of the GS grid object to add the gridline at
%%   Tuple		Tuple of fields for each column in the gridline
%%   LIOptSpec		List of specific options for this gridline
%%
%% Returns:
%%   Undefined return value.

add_gridline(Grid, Row, Tuple, LIOptSpec) ->

    {Pid, FuncText, Name, Msgs, Reds, Psize} = Tuple,
    
    LIOpt = [{click,true},
	     {doubleclick,true},
	     {fg, colour(Row)},
	     {text,{1,Pid}},
	     {text,{2,FuncText}},
	     {text,{3,Name}},
	     {text,{4,Msgs}},
	     {text,{5,Reds}},
	     {text,{6,Psize}} |LIOptSpec],


    case gs:read(Grid, {obj_at_row, Row}) of
	undefined ->
	    gse:gridline(Grid,[{row, Row}|LIOpt]);
	GridLine ->
	    gs:config(GridLine,LIOpt)
    end.
    


show(Process) ->
    Call = pman_process:function_info(Process),
    Reds = pman_process:pinfo_notag(Process, reductions),
    Name = pman_process:get_name(Process),
    Msg = pman_process:msg(Process),
    Size = pman_process:psize(Process),

    {Process,Call, Name, Msg, Reds, Size}.


%% -----------------------------------------------------------------
%% Colour 

colour(1) ->
    ?HEADER_COLOUR;

colour(Row) ->
    ?UNSELECTED_COLOUR.



%% ---------------------------------------------------------------
%% Interchange colours between two rows
%% ---------------------------------------------------------------
 
change_colour(Grid,1,1) ->
    ok;

change_colour(Grid,From,To) ->
    Gitem_to = gs:read(Grid,{obj_at_row,To}),
    Gitem_fr = gs:read(Grid,{obj_at_row,From}),
    gs:config(Gitem_to,{fg,?SELECTED_COLOUR}),
    
    case Gitem_fr == Gitem_to of
	false ->
	    gs:config(Gitem_fr,{fg,colour(From)});

	true ->
	    ok
    end.

    
%% ---------------------------------------------------------------
%% Delete items in the grid
%% ---------------------------------------------------------------

delete_items(Grid, Row) -> 
    case gs:read(Grid, {obj_at_row, Row}) of
	undefined ->
	    ok;
	GridLine ->
	    gs:destroy(GridLine),
	    delete_items(Grid, Row+1)
    end.
    

%% addrows/2
%%
%% 
addrows(_Any, []) -> [];
addrows(I, [H|T]) -> [{I, H} | addrows(I+1, T)].



%% --------------------------------------------------------------
%% Create a title for the window
%% Return: the title
%% --------------------------------------------------------------

title({module, Mod}) ->
    lists:flatten([io_lib:format("PMAN:Module info ~p", [Mod])]);

title({shell,  Sh} ) ->
    lists:flatten([io_lib:format("PMAN:Shell process ~p on Node ~p",
				 [Sh,node(Sh)])]);

title(Sh) ->
    lists:flatten([io_lib:format("PMAN:Process ~p on Node ~p",
				 [Sh, node(Sh)]),name(Sh)]).
name(Pid) ->
    case pman_process:pinfo(Pid, registered_name) of
	{registered_name, Name} ->
	    lists:flatten([io_lib:format("[~p]", [Name])]);
	_Anything -> ""
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% module_data/1 - %% Returns the module information for a
%%   module, on a format suitable to insert into a GS editor.
%%
%% Arguments:
%%   ModuleName		The module
%%
%% Returns:
%%   A string with module information.
%%

module_data(ModuleName) ->
    vformat("", catch apply({ModuleName, module_info},[])).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% display/1 - 
%%

display({_,Pid,_}) -> display(Pid);
display({_,Pid}) -> display(Pid);
display(Pid) when pid(Pid) ->
    case pman_process:pinfo(Pid) of
	undefined ->
	    format('Process is dead~n',[]);
	Other ->
	    proc_format(Other)
    end.

%% --------------------------------------------------------------
%% Format functions for the shell and help window.
%% --------------------------------------------------------------

vformat(Pad, {M,F,A}) when atom(F) -> 
    Pad2 = lists:append(Pad,mkpad(io_lib:format("~w:~w",[M,F]))),
    lists:flatten([format("~p:~p", [M,F]),argformat(Pad2, A)]);

vformat(Pad, [H|T]) ->
    kvformat(Pad, [H|T],"[");

vformat(Pad, X) -> format("~p~n", [X]).

format(Format) -> format(Format, []).

format(Format, Args) ->
   io_lib:format(Format, Args).
   

format1([]) -> "";
format1([H|T]) ->
    lists:flatten(io_lib:format("~p~n", [H]), format1(T)).

kvformat(S, [Item],Buff) ->
    lists:reverse([format("\n~s~p]\n",[S,Item])|Buff]);

kvformat(S,[H|T],Buff) ->
    kvformat(S,T,[format("\n~s~p, ",[S,H])|Buff]);

kvformat(_,[],Buff) -> 
    lists:reverse(["]\n"|Buff]).

argformat(Pad,A) when integer(A) ->
    format("/~p\n", [A]);
argformat(_,A) ->
    lists:flatten([format("/~p\n", [length(A)]),
		   format("args: \n"),
		   argformat2("      ", A)]).

argformat2(Pad, Arglist) ->
    Chars = lists:flatten(io_lib:format("~p",[Arglist])),
    if
	length(Chars) < (70 - length(Pad)) ->
	    format("~s~s\n", [Pad, Chars]);
	true ->
	    argformat3(Pad, Arglist)
    end.

argformat3(_,[]) -> format("\n");
argformat3(Pad, [H|T]) ->
    Chars = truncate(65,io_lib:format("~s~p",[Pad, H])),
    format("~s,\n", [Chars]),
     argformat3(Pad, T).

pformat(false) -> [];
pformat({value,{_, 0}}) -> [];
pformat({value,{_, []}}) -> [];
pformat({value, {Key, Vals}}) ->
    Pad = mkpad(io_lib:format("~p ",[Key])),
    format(lists:flatten(["~p: " ,vformat(Pad, Vals), "~n"]), [Key]).
   
truncate(0, Chars) -> ".....";
truncate(I, [H|T]) -> [H|truncate(I-1, T)];
truncate(I, []) -> [].

mkpad([_|T]) -> [32|mkpad(T)];
mkpad([]) -> [].

proc_format(Pi) ->  %% process_info struct
    X1 = pformat(lists:keysearch(initial_call, 1, Pi)),
    X2 = pformat(lists:keysearch(current_function, 1,Pi)),
    X3 = pformat(lists:keysearch(messages, 1,Pi)),
    X4 = pformat(lists:keysearch(dictionary,1, Pi)),
    X5 = pformat(lists:keysearch(heap_size, 1,Pi)),
    X6 = pformat(lists:keysearch(stack_size, 1,Pi)),
    X7 = pformat(lists:keysearch(reductions, 1,Pi)),
    X8 = pformat(lists:keysearch(links, 1,Pi)),
    X9 = pformat(lists:keysearch(trap_exit, 1,Pi)),
    Str = lists:flatten([X1, X2, X3, X4, X5,X6,X7,X8,X9]).


     
    
    
read_flags() ->
    read_flags(pman_shell:flags(),[]).

read_flags([],Chosen) ->
    Chosen;
read_flags([Flag|Flags],Chosen) ->
    case gs:read(Flag,select) of
	true ->
	    read_flags(Flags,[Flag|Chosen]);
	false ->
	    read_flags(Flags,Chosen)
    end.
	    


%% -----------------------------------------------------------------
%% Create a window on which short messages can be displayed.

create_text_win(Text) ->
    Gs = gs:start([{kernel,true}]),
    Win = gs:create(window,Gs,[{width,200},{height,75},{destroy,true},
			       {title,"Pman Message"}]),
    Can = gs:create(canvas,Win,[{width,200},{height, 75},{x,0},{y,0}]),
    T = gs:create(text,Can,[{text,Text},{coords,[{10,0}]},{justify,center}]),
    {T,Win}.


%%
%% Using the tool_utils function for presenting messages.
%%

dialog_window(GSParent, Text) ->
    spawn_link(tool_utils,notify,[GSParent, Text]).



%% Create a window with a dismiss button

    

msg_win(Text) ->
    spawn_link(pman_win,msg_win,[Text,self()]).

msg_win({Text,nolink},Parent) ->
    {Text_obj,Win} = create_text_win(Text),
    dismiss(Text_obj,Win);

msg_win(Text,Parent) ->
    link(Parent),
    {Text_obj,Win} = create_text_win(Text),
    dismiss(Text_obj,Win).

dismiss(Text_Obj,Win) ->
    gs:create(button,quit,Win,[{label,{text,"Dismiss"}},{width,100},
			       {x,50},{y,40}]),
    gs:config(Win,{map,true}),
    receive
	{gs, quit, click, _, _} ->
	    exit(normal)
    end.
    




