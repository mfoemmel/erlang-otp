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
%% Interpretate File Window
%%
%% This module is a simple file browser which displays directories
%% and files ending in .erl. Upon choosing the file, an Action
%% passed on to it through start is applied to the file name. 
%% ------------------------------------------------------------

-module(dbg_ui_interpret).

-include_lib("kernel/include/file.hrl").

-export([start/5,fs_init/6,get_event/2]).

-define(BUTTONX,70).
-define(LISTBOXY,75).
-define(SYMBOL, "*").   % interpreted symbol, attached to the file

-define(INT_MSG_TAG, ?MODULE).


%%%start(Parent,Action,Pos) ->ok. Starts the window.
%%%Parent: The pid of the parent we are to link
%%%        ourselvs to inorder to exit when the
%%%        program is terminated.
%%%Action: {Mod,Func,Arg}: The action to be taken with
%%%        the file name once it has been picked.
%%%Pos:    The position to display the window in the screen,
%%%        so that it is in focus with the mouse pointer, allowing
%%%        the user to use key accellerators with out positioning the
%%%        mouse.

start(Action,Pos,Title,Dir, Table) ->
    Dir0 = filename:join(filename:split(Dir)),
    spawn_link(?MODULE,fs_init,[Dir0,self(),Pos,Title,Action,Table]).	

%%%get_event(Parent,Action) -> ok. Awaits the file name to be chosen,
%%%                                and applies Action to it.

get_event(Parent,{Mod,Func,Args}) ->
    receive
	{?INT_MSG_TAG,Pid,Dir,done} ->
	    Parent!{new_dir,Dir};
	{?INT_MSG_TAG,Pid,{ok,Dir,Name,Opts}} ->
	    File = Dir ++ "/" ++ Name,
	    Pid ! apply(Mod,Func,[File,Opts|Args]),
	    get_event(Parent,{Mod,Func,Args})
        end.



%%% fs_init(Dir,Owner,Pos). Initializes the file dialog
%%% Dir:   The current working directory
%%% Owner: The process we send events to.
%%% Pos:   The position to display the window in the screen,

fs_init(Dir,Owner,Pos,Title,Action,Table) ->
    
    case dbg_ui_winman:win_started (self (), Title) of
	true ->
	    exit (self (), kill),
            exit (already_exists);

	_false ->
	    ok
    end,

    dbg_ui_aux:mark_busy(win),
    Pid = spawn_link(?MODULE,get_event,[Owner,Action]),
    process_flag(trap_exit, true),
    {Win, Size} = init_win([],Dir,Pos,Title),
    dbg_ui_winman:insert_win (interpret, Win, self ()),
    Interpreted = get_interpreted (),
    Items =  refresh(Dir, Interpreted),
    gs:config(info,{label,{text,["Dir: "|Dir]}}),
    dbg_ui_aux:mark_nonbusy(win),
    fs_loop(Win,Dir, Pid, Size, Table, Interpreted, undefined, undefined).

%%% init_win(Items,Dir,{X,Y}) -> {Width,Height}. Creates the window

%%% Items: Items to be inserted in the list box. (default []).
%%% Dir:   The current working directory
%%% {X,Y} : Position to display the window in the Screen.

init_win(Items,Dir,{X,Y},Title) ->
    Width = 400, 
    Height = 320,
    S=gs:start([{kernel,true}]),
    Win = gs:window(win,S,[{width,Width},
		     {height,Height},
		     {title,Title},
		     {configure,true},
		     {keypress,true},
		     {x,X},
		     {y,Y},
		     {destroy,true},
		     {cursor,arrow}
		    ]),
    MenuBar = gs:menubar(menubar, win, []),
    gs:menubutton(menubtn_file, menubar, [{label, {text, " File "}},
					  {underline, 1},
					  {side, left}
					 ]),
    gs:menubutton(menubtn_options, menubar, [{label, {text, " Options "}},
					  {underline, 1},
					  {side, left}
					 ]),

    dbg_ui_winman:windows_menu (MenuBar),
    
    gs:menubutton(menubtn_help, menubar, [{label, {text, " Help "}},
					  {underline, 1},
					  {side, right}
					 ]),
    gs:menu(menu_file, menubtn_file, []), 
    gs:menu(menu_options, menubtn_options, []), 
    gs:menu(menu_help, menubtn_help, []), 
    
    gs:menuitem(menu_file, [{label, {text, "Close"}},
			    {underline, 0},
			    {data, close}
			   ]),
    gs:menuitem(menu_options, [{label, {text, "Macro Definitions..."}},
			       {underline, 0},
			       {data, macro}
			      ]),
    gs:menuitem(menu_options, [{label, {text, "Include Directories..."}},
			       {underline, 0},
			       {data, incdir}
			      ]),
    gs:menuitem(menu_help, [{label, {text, "Help"}},
			       {underline, 0},
			       {data, help}
			      ]),
    


    gs:label(win,[{y,17+30},
		  {x,0},
		  {label, {text,"File:"}},
		  {align,e},
		  {height,15},
		  {width,40}]),
    gs:entry(entry,win,[{y,10+30},
			{width,Width-50},
			{x,40},
			{keypress,true},
			{focus,true}]),
    gs:listbox(lb,win,[{x,5},
		       {y,?LISTBOXY + 30},
		       {width,Width-?BUTTONX-10},
		       {height,Height-?LISTBOXY-5-30},
		       {selectmode,single},
		       {vscroll,right},
		       {click,true},
		       {doubleclick,true}]),
    gs:button(all,win,[{label,{text,"All"}},
		       {width,65},
		       {x,Width-?BUTTONX},
		       {y,Height-105}]),
    gs:button(interp,win,[{label,{text,"Choose"}},
			  {width,65},
			  {x,Width-?BUTTONX},
			  {y,Height-70}]),
    gs:button(done,win,[{label, {text,"Done"}},
			{x,Width-?BUTTONX},
			{y,Height-35},
			{width,65}]),
    gs:label(info,win,[{x,5},
		       {y,?LISTBOXY-30+30},
		       {align,w},
		       {height,20},
		       {width,Width}]),
    gs:config(win,[{map,true}]),
    {Win, {Width,Height}}.

%%% update_window(X,Y,Size) -> NewSize. Resizes the objects in the window.
%%% X,Y: The new height and with of the window.
%%% {OldX,OldY} : The old width and height

update_window(X,Y,{OldX, OldY}) ->
    Dx = abs(X - OldX),
    Dy = abs(Y - OldY),
    if Dx + Dy =/= 0 ->
	    gs:config(lb,[{width,X-?BUTTONX-10},{height,Y-?LISTBOXY-5-30}]),
	    gs:config(done,[{x,X-?BUTTONX},{y,Y-35}]),
	    gs:config(interp,[{x,X-?BUTTONX},{y,Y-70}]),
	    gs:config(all,[{x,X-?BUTTONX},{y,Y-105}]),
	    gs:config(entry,{width,X-45}),
	    gs:config(info,{width,X}),
	    {X,Y};
    true ->
	    {X,Y}
    end.

%%% fs_loop(Win,Dir,Owner,Size,Interpreted). The window event loop.
%%% Win: window pid
%%% Dir: Current working directory
%%% Owner: The process we send events to.
%%% Size: WIndow Size in case of configure.
%%% Interpreted: File names on which actions have been taken, not to
%%%         be shown in the list box.

fs_loop(Win, Dir, Owner, Size, Table, Interpreted, MPid, IPid) ->
    receive
	{update_windows, Data} ->
	    dbg_ui_winman:update_windows_menu (Data),
	    fs_loop (Win, Dir, Owner, Size, Table, Interpreted, MPid, IPid);

        %% manages the Windows menu - puts the choosen window on top
        {gs, _, click, [win_menu, Win_Pid], _}     ->
            dbg_ui_winman:on_top(Win_Pid),
	    fs_loop (Win, Dir, Owner, Size, Table, Interpreted, MPid, IPid);

	{gs, _, destroy, _, _} ->             %%Exit
	    send_to(MPid, destroy),
	    send_to(IPid, destroy),
	    dbg_ui_winman:delete_win (Win),
	    Owner ! {?INT_MSG_TAG, self(), Dir, done};

	{gs, win, keypress, [], ['Up'|_]} ->    %%Up
	    up(Dir, Owner),
	    fs_loop(Win, Dir, Owner, Size, Table, Interpreted, MPid, IPid);

	{gs, win, keypress, [], ['Down'|_]} ->  %%Down
	    down(Dir, Owner),
	    fs_loop(Win, Dir, Owner, Size, Table, Interpreted, MPid, IPid);

	{gs, win, keypress, _, ['Return'|_]} -> %%Compile
	    {NDir, NInterpreted} = entered_name(Dir, Owner, Table, Interpreted),
	    case NDir of
		Dir ->
		    done;
		_Other ->
		    send_to(MPid, {new_dir,NDir}),
		    send_to(IPid, {new_dir,NDir})
	    end,
	    fs_loop(Win, NDir, Owner, Size, Table, NInterpreted, MPid, IPid);

	{gs, win, keypress, _, [c,_,_,1]} ->    %%Exit
	    send_to(MPid, destroy),
	    send_to(IPid, destroy),
	    dbg_ui_winman:delete_win (Win),
	    Owner ! {?INT_MSG_TAG, self(), Dir, done};

	{gs, win, keypress, _, [p,_,_,1]} ->    %%Up
	    up(Dir, Owner),
	    fs_loop(Win, Dir, Owner, Size, Table, Interpreted, MPid, IPid);

	{gs, win, keypress, _, [n,_,_,1]} ->    %%Down
	    down(Dir, Owner),
	    fs_loop(Win, Dir, Owner, Size, Table, Interpreted, MPid, IPid);

	{gs, interp, click, _, _} ->          %%Action
	    gs:config(win, [{cursor,busy}]),
	    {NDir, NInterpreted} = double_clicked(Dir, Owner, Table, Interpreted),
	    case NDir of
		Dir ->
		    done;
		_Other ->
		    send_to(MPid, {new_dir,NDir}),
		    send_to(IPid, {new_dir,NDir})
	    end,
	    gs:config(win, [{cursor,arrow}]),
	    fs_loop(Win, NDir, Owner, Size, Table, NInterpreted, MPid, IPid);

	{gs, all, click, _, _} ->             %%Action on all
	    NInterpreted = all(Dir, Owner, Table, Interpreted),
	    fs_loop(Win, Dir, Owner, Size, Table, NInterpreted, MPid, IPid);

	{gs, done, click, _, _} ->            %%Exit
	    send_to(MPid, destroy),
	    send_to(IPid, destroy),
	    dbg_ui_winman:delete_win (Win),
	    Owner ! {?INT_MSG_TAG, self(), Dir, done};

	{gs, lb, click, _, _} ->              %%Choose
	    clicked(Dir, Owner),
	    fs_loop(Win, Dir, Owner, Size, Table, Interpreted, MPid, IPid);

	{gs, lb, doubleclick, _, _} ->        %%Compile
	    gs:config(win, [{cursor,busy}]),
	    {NDir, NInterpreted} = double_clicked(Dir, Owner, Table, Interpreted),
	    case NDir of
		Dir ->
		    done;
		_Other ->
		    send_to(MPid, {new_dir,NDir}),
		    send_to(IPid, {new_dir,NDir})
	    end,
	    gs:config(win, [{cursor,arrow}]),
	    fs_loop(Win, NDir, Owner, Size, Table, NInterpreted, MPid, IPid);

	{gs, win, configure, _, [X,Y|_]} ->   %%Resize
	    New_size = update_window(X, Y, Size),
	    fs_loop(Win, Dir, Owner, New_size, Table, Interpreted, MPid, IPid);

	{gs, _Id, click, close, _Args} ->
	    send_to(MPid, destroy),
	    send_to(IPid, destroy),
	    dbg_ui_winman:delete_win (Win),
	    Owner ! {?INT_MSG_TAG, self(), Dir, done};

	{gs, _Id, click, macro, _Args} ->
	    NewMPid = dbg_ui_compilerdefs:start(macro, Table, Dir, MPid),
	    fs_loop(Win, Dir, Owner, Size, Table, Interpreted, NewMPid, IPid);

	{gs, _Id, click, incdir, _Args} ->
	    NewIPid = dbg_ui_compilerdefs:start(incdir, Table, Dir, undefined),
	    fs_loop(Win, Dir, Owner, Size, Table, Interpreted, MPid, NewIPid);

	{gs, _Id, click, help, _Args} ->
	    HelpFile = filename:join(code:priv_dir(debugger), "../doc/index.html"),
	    tool_utils:open_help(gs:start([{kernel,true}]), HelpFile),
	    fs_loop(Win, Dir, Owner, Size, Table, Interpreted, MPid, IPid);


	{'EXIT', MPid, Reason} ->
	    fs_loop(Win, Dir, Owner, Size, Table, Interpreted, undefined, IPid);

	{'EXIT', IPid, Reason} ->
	    fs_loop(Win, Dir, Owner, Size, Table, Interpreted, MPid, undefined);

	{'EXIT', _AnyPid, Reason} ->
	    send_to(MPid, destroy),
	    send_to(IPid, destroy),
	    dbg_ui_winman:delete_win (Win),
	    Owner ! {?INT_MSG_TAG, self(), Dir, done};

	_Other ->                              %%Ignore
	    fs_loop(Win, Dir, Owner, Size, Table, Interpreted, MPid, IPid)
    end.

%% up(Dir,Owner) -> ok.
%% down(Dir,Owner) -> ok.
%% The user wants to move up (or down) one pos in the list box.
%% Dir: Current working directory
%% Dir,Owner used when clicked is called.

up(Dir, Owner) ->
    move({Dir, Owner}, lb,
	 fun(0, Last) -> Last;
	    (Pos, _) -> Pos-1
	 end).
		   
down(Dir, Owner) ->
    move({Dir, Owner}, lb,
	 fun(Last, Last) -> 0;
	    (Pos, _) -> Pos+1
	 end).
		   
%% move({Dir, Owner}, ObjId, StepFun) -> ok.

%% Move the selection in the list box ObjId to
%% StepFun(CurrentIndex, LastIndex). Also move the list box view so
%% the selection stays visible. Finally call clicked/2 (if anything
%% *was* selected, that is).

%% FIXME: If we leave via clicked/2, `true' might be returned instead
%% of `ok'. As the return value isn't actually used anywhere, this
%% should only be fixed for general neatness.
move({Dir, Owner}, ObjId, StepFun) ->
    case gs:read(lb,selection) of
	[Pos|_] when integer(Pos) ->		%at least one selection
	    LastPos = gs:read(ObjId, size)-1,
	    NewPos = StepFun(Pos, LastPos),
	    gs:config(ObjId, {selection,clear}),
	    gs:config(ObjId, {selection,NewPos}),
	    gs:config(ObjId, {see,NewPos}),
	    clicked(Dir, Owner);
	_ ->
	    ok
    end.

%%% refresh(Dir,Interpretedd) -> Items. Update the elements in the
%%%                                entry box.
%%% Items: Elements in the list box.
%%% Dir: Current working directory
%%% Interpretedd: Files not to be shown

refresh(Dir,Interpreted) ->
    Index = get_index (),

    %% Clear the entry field first thing, otherwise the user has time
    %% to enter something while the 'get_files' is running.
    gs:config(entry,{text, ""}),

    Items = case catch get_files (Dir, Interpreted) of
		{'EXIT', _} -> 
		    ["../"];

		Files -> 
		    Files
	    end,

    gs:config(lb,clear),
    gs:config(lb,[{items,Items}]),
    gs:config(lb,{selection,clear}),
%    gs:config(lb,{see, Index}),
    Items.

%%% all(Dir,Owner,Interpreted) -> NewInterpreted.
%%% Apply action to all files in the list box.
%%% NewInterpreted: New files not to be shown.
%%% Dir: Current working directory
%%% Owner: Process which will execute the action.
%%% Interpreted: Files not to be shown

all(Dir,Owner,Table,Interpreted) ->
    dbg_ui_aux:mark_busy(win),
    NewInterpreted =  interpret_all(Dir,Owner,Table,1,gs:read(lb,size),Interpreted),
    refresh(Dir,NewInterpreted),
    gs:config(info,{label,{text,["Compiled all"]}}),
    dbg_ui_aux:mark_nonbusy(win),
    NewInterpreted.

%%% interpret_all(Dir,Owner,Num,Size,Interpreted) -> NewInterpreted;
%%% Checks every entry in the list box, and sends the
%%% files ending in .erl to the process which will apply
%%% the action.

interpret_all(Dir,Owner,Table,Size,Size,Interpreted) ->
    Interpreted;
interpret_all(Dir,Owner,Table,Num,Size,Interpreted) ->
    File = gs:read(lb,{get,Num}),
    File1 = strip_file (File),
    case check_file(Dir,filename:join(Dir,File1)) of
	{file,Dir2,File2} ->
	    gs:config(info,{label,{text,["Compiling: " | File2]}}),
	    Opts = dbg_ui_compilerdefs:get_opts(Table, Dir2),
	    Owner !  {?INT_MSG_TAG,self(),{ok,Dir2,File2,Opts}},
	    receive
		error ->
		    Module = filename:basename(File2, ".erl"),
		    gs:config(info, [beep]),
		    gs:config(win, [{cursor,arrow}]),
		    tool_utils:notify(win, ["Cannot compile module '" ++ Module ++ "'!"]),
		    interpret_all(Dir,Owner,Table,Num+1,Size,Interpreted);
		{module, Module} ->
		    refresh(Dir2,[File2|Interpreted]),
		    gs:config(lb,{see, Num}),
		    interpret_all(Dir,Owner,Table,Num+1,Size,[File2|Interpreted])
	    end;
	_ ->
	    interpret_all(Dir,Owner,Table,Num+1,Size,Interpreted)
    end.

%%% clicked(Dir,Owner). The entry in the list box has been
%%% changed, either manualy or by clicking on the item.

clicked(Dir,Owner) ->
    case gs:read(lb,selection) of
	[] -> 
	    true;
	[0] ->
	    gs:config(entry,{text,[filename:dirname(Dir)]}); 
	[Idx|_] ->
	    File=gs:read(lb,{get,Idx}),
	    File1 = strip_file (File),
	    gs:config(entry,{text, File1})
    end.


%%% double_clicked(Dir,Owner,Interpreted) -> {NewDir,NewInterpreted}.
%%% Called whenever a user chooses a file, either by double
%%% clicking on it, or by pressing return. If a file, it is sent
%%% off to the owner, and the predefined action is applied to it.
%%% NewDir: is the new directory.
%%% DNewInterpreted: The new list of files not to be displayed.
%%% Dir: Current working directory
%%% Interpreted: Files not to be shown in the list box

double_clicked(Dir,Owner,Table,Interpreted) ->
    case gs:read(lb,selection) of
	[] ->
	    {Dir,Interpreted};
	[Idx|_] ->
	    clicked(Dir,Owner),
	    entered_name(Dir,Owner,Table,Interpreted)
    end.

%%% entered_name(Dir,Owner,Interpreted) -> {NewDiw,NewInterpreted}|Error.
%%% reads the entry chosen by the user, analizes it, and handles
%%% side effects accordingly depending on if it is a file, directory
%%% or an error.
%%% Dir: last chosen directory
%%% Owner: Process handling the action events
%%% Interpreted: Modules not to be displaied.

entered_name(Dir,Owner,Table,Interpreted) ->
    File=gs:read(entry,text),
    Index = get_index (),
    case check_file(Dir,File) of
	{file,Dir2,File2} ->
	    gs:config(info,{label,{text,["Compiling: " | File2]}}),
	    Opts = dbg_ui_compilerdefs:get_opts(Table, Dir2),
	    Owner ! {?INT_MSG_TAG, self(), {ok,Dir2,File2,Opts}},
	    receive
		error ->
		    Module = filename:basename(File2, ".erl"),
		    gs:config(info,[beep]),
		    gs:config(win, [{cursor,arrow}]),
		    tool_utils:notify(win, ["Cannot compile module '" ++ Module ++ "'!"]),
		    {Dir2, Interpreted};
		{module, Module} ->
		    dbg_ui_aux:mark_busy(win),
		    refresh(Dir2,[File2|Interpreted]),
		    gs:config(lb,{see, Index}),
		    Txt = io_lib:format ("Compiled: ~p.erl", [Module]),
		    gs:config(info,{label,{text,[Txt]}}),
		    dbg_ui_aux:mark_nonbusy(win),
		    {Dir2,[File2|Interpreted]}
	    end;
	{dir,Dir2} ->
	    dbg_ui_aux:mark_busy(win),
	    refresh(Dir2,Interpreted),
	    gs:config(info,{label,{text,["Dir: "|Dir2]}}),
	    dbg_ui_aux:mark_nonbusy(win),
	    {Dir2,Interpreted};
	{error,What} ->
	    gs:config(info,{label,{text,["Error: ",atom_to_list(What)]}}),
	    {Dir,Interpreted}
    end.






%%% check_file(Dir,File) -> {file,Dir,File}|{dir,Dir}|Error
%%% checks if a file entered exists, and handles absolute/relative
%%% paths.
%%% returns {file,Dir,File}
%%%         {dir,Dir}
%%%    or   {error,What}

check_file(_,[]) ->
     {error,'no file'};
check_file(Dir,File) ->	
    Last = lists:last(File),
    Path = case filename:pathtype(File) of
	       absolute ->
		   File;
	       relative ->
		   filename:join(Dir,File);
	       volumerelative ->
		   File ++ "/"
	   end,
    case file:read_file_info(Path) of
	{ok,#file_info{type=directory}} ->
	    {dir,Path};
	{ok,#file_info{type=regular}} ->
	    case filename:extension(Path) of
		".erl" ->
		    FileName = filename:basename(Path),
		    {file,filename:dirname(Path),FileName}
		end;
	_ ->
	    {error,'unknown file or directory'}
    end.



%%% get_files(Dir,Interpreted) -> [Files/Dirs]
%%% Returns a list with the interpreted files marked with 
%%% an attached symbol.
%%%

get_files (Dir, Interpreted) -> 
    {ok, Files} = file:list_dir (Dir),

    Up = case filename:dirname (Dir) of
	     Dir -> 
		 ".";
	     _  ->
		 " ../"
	 end,

    [Up | get_files (Dir, lists:sort (Files), Interpreted, [])].


get_files (Dir, [H | T], Interpreted, Buffer) ->
    case file:read_file_info (filename:join (Dir, H)) of
        {ok, #file_info{type=directory}} ->
	    H1 = lists:append (" ", H), 
	    H2 = lists:append (H1, "/"),
	    get_files (Dir, T, Interpreted, [H2 | Buffer]);

        {ok, #file_info{type=regular}} ->
	    case lists:member (H, Interpreted) of
		true ->
		    H1 = lists:append (?SYMBOL, H),
		    get_files (Dir, T, Interpreted, [H1 | Buffer]);

		false ->
		    case filename:extension (H) of
			".erl" ->
			    H1 = lists:append (" ", H), 
			    get_files (Dir, T, Interpreted, [H1 | Buffer]);
			_ ->
			    get_files(Dir,T,Interpreted,Buffer)
		    end
	    end;
	_  ->
	    get_files(Dir,T,Interpreted,Buffer)
    end;
get_files(_,[],_,Buffer) -> lists:reverse(Buffer).




send_to(undefined, _Msg) ->
    done;
send_to(Pid, Msg) ->
    Pid ! Msg.




%%% get_interpreted  /0
%%%
%%% get_interpreted returns the interpreted modules.
%%% The modules are strings with the extension ".erl"
%%%

get_interpreted () ->
    L = lists:map (fun atom_to_list/1, int:interpreted ()),

    Fun  = fun (X) -> 
		   lists:append (X, ".erl") 
	   end,

    lists:map (Fun, L).



%%% get_index  /0
%%%
%%% get_index returns the current selection in the listbox
%%%

get_index () ->
    case gs:read (lb, selection) of
	[] ->
	    0;
	
	[Index | _] ->
	    Index
    end.



%%% strip_file  /1
%%%
%%% strip_file returns the given file without the added 
%%% characters in front.
%%%
%%% Pre:
%%%    File  ==  string
%%%

strip_file (File) ->
    string:sub_string (File, 2).
