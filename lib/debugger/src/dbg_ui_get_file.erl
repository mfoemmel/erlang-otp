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
-module (dbg_ui_get_file).

-export ([get_file/2]).

-define (BUTTONX,70).
-define (LISTBOXY,75).
-define (EXTENSION, ".state").




get_file (Mode, Pos) ->
    init (Mode, Pos).



init (Mode, Pos) ->
    Dir = get_dir (),
    {Win, Size} = init_win (Mode, Pos),
    refresh (Dir),
    no_name (Mode),    % NoName.state as default when saving settings
    loop (Mode, Dir, Win, Size).



loop (Mode, Dir, Win, Size) ->
    receive
	{gs, _, destroy, _, _} ->               %% Exit
	    gs:destroy (Win),
            no_file;

	{gs, win, keypress, [], ['Up'|_]} ->    %% Up
	    up (Dir),
	    loop (Mode, Dir, Win, Size);

	{gs, win, keypress, [], ['Down'|_]} ->  %% Down
	    down (Dir),
	    loop (Mode, Dir, Win, Size);

	{gs, win, keypress, _, ['Return'|_]} -> %% Action
	    Data = get_value (Mode, Dir),
	    
	    case Data of
		{file, _, File} ->
		    gs:destroy (Win),
		    File;              % Return value
		
		{dir, New_Dir} ->
		    loop (Mode, New_Dir, Win, Size)
		
	    end;

	{gs, win, keypress, _, [c, _, _, 1]} ->    %%Exit
	    gs:destroy (Win),
            no_file;

	{gs, win, keypress, _, [p, _, _, 1]} ->    %%Up
	    up (Dir),
	    loop (Mode, Dir, Win, Size);

	{gs, win, keypress, _, [n, _, _, 1]} ->    %%Down
	    down (Dir ),
	    loop (Mode, Dir, Win, Size);

	{gs, mode, click, What_Mode, _} ->          %%Action
	    Data = get_value (What_Mode, Dir),
	    
	    case Data of
		{file, _, File} ->
		    gs:destroy (Win),
		    File;
		
		{dir, New_Dir} ->
		    loop (Mode, New_Dir, Win, Size)
		
	    end;

	{gs, cancel, click, _, _} ->            %%Exit
	    gs:destroy (Win),
            no_file;

	{gs, lb, click, _, _} ->              %%Choose
	    clicked (Dir),
	    loop (Mode, Dir, Win, Size);

	{gs, lb, doubleclick, _, _} ->        %% Action
	    Data = get_value (Mode, Dir),
	    
	    case Data of
		{file, _, File} ->
		    gs:destroy (Win),
		    File;                     % Return value
		
		{dir, New_Dir} ->
		    loop (Mode, New_Dir, Win, Size)
		
	    end;

	{gs, win, configure, _, [X,Y|_]} ->   %%Resize
	    New_Size = update_window (X, Y, Size, Mode),
	    loop (Mode, Dir, Win, New_Size);

	{gs, _, click, cancel, _} ->
	    gs:destroy (Win),
            no_file;

	_Other ->                              %%Ignore
	    loop (Mode, Dir, Win, Size)
 
    end.



%%% get_dir  /0
%%%
%%% get_dir returns the default directory for the setting files.
%%%

get_dir () ->
    tool_utils:appstate_filename (debugger, "").

    




%%% init_win
%%%

init_win (Mode, {X, Y}) ->
    Width = 400, 
    Height = 320,

    Title = io_lib:format ("~s Settings Dialog", [Mode]),
    
    S = gs:start ([{kernel, true}]),

    Win = gs:window (win, S, [{width, Width},
			      {height, Height},
			      {title, Title},
			      {configure, true},
			      {keypress, true},
			      {x, X},
			      {y, Y},
			      {destroy, true},
			      {cursor, arrow},
			      {setfocus, true}]),

    gs:label (file, win, [{y, 17 + 30},
			  {x, 0},
			  {label, {text, "File:"}},
			  {align, e},
			  {height, 15},
			  {width, 40}]),

    gs:entry (entry, win, [{y, 10 + 30},
			   {width, Width - 50},
			   {x, 40},
			   {keypress, true},
			   {focus, true}]),

    gs:label (dir, win, [{x, 5},
			 {y, ?LISTBOXY},
			 {align, w},
			 {label, {text, " Dir:"}},
			 {height, 20},
			 {width, Width - 10}]),

    gs:listbox (lb, win, [{x, 5},
			  {y, ?LISTBOXY + 30},
			  {width, Width - ?BUTTONX - 10},
			  {height, Height - ?LISTBOXY - 5 - 30},
			  {selectmode, single},
			  {vscroll, right},
			  {click, true},
			  {doubleclick, true}]),

    gs:button (mode, win, [{label, {text, Mode}},
			   {width, 65},
			   {data, Mode},
			   {x, Width - ?BUTTONX},
			   {y, Height - 70}]),

    gs:button (cancel, win, [{label, {text, "Cancel"}},
			    {x, Width - ?BUTTONX},
			    {y, Height - 35},
			    {width, 65}]),

    gs:config (win, [{map, true}]),

    {Win, {Width,Height}}.





%%% get_value
%%%

get_value ('Load', Dir) ->
    dbg_ui_aux:mark_busy (win),
    File = gs:read (entry, text),
    Answ = control_file (Dir, File),
    dbg_ui_aux:mark_nonbusy (win),
    Answ;



get_value ('Save', Dir) ->
    dbg_ui_aux:mark_busy (win),
    File = gs:read (entry, text),
    Files = gs:read (lb, items),

    Answ = case check_file (Dir, File) of
	       {error, _} ->
		   check_file_to_save (Dir, File);

	       _ ->
		   control_file (Dir, File)
	   end,
    
    dbg_ui_aux:mark_nonbusy (win),
    Answ.



%%% check_file_to_save  /2
%%%
%%% check_file_to_save returns file, dir and/or prints an error message
%%% if the file is not correct.
%%%

check_file_to_save (Dir, File) ->
    case filename:extension (File) of
	?EXTENSION ->
	    {file, Dir, filename:join (Dir, File)};

	_ ->
	    gs:config (dir, {label, 
			     {text, 
			      ["Error: the extension should be '", ?EXTENSION, "'"]}}),
	    {dir, Dir}
    end.


%%% update_window(X,Y,Size) -> NewSize. Resizes the objects in the window.
%%% X,Y: The new width and height of the window.
%%% {OldX,OldY} : The old width and height

update_window (X, Y, {OldX, OldY}, Mode) ->
    Dx = abs (X - OldX),
    Dy = abs (Y - OldY),
    if Dx + Dy =/= 0 ->
	    gs:config (lb, [{width, X - ?BUTTONX - 10},
		            {height, Y - ?LISTBOXY - 5 - 30}]),
	    gs:config (cancel, [{x, X - ?BUTTONX}, {y, Y - 35}]),
	    gs:config (mode, [{x, X - ?BUTTONX}, {y, Y - 70}]),
	    gs:config (entry, {width, X - 45}),
	    gs:config (dir, {width, X - 10}),
	    {X, Y};
    true ->
	    {X, Y}
    end.



%%% up(Dir) -> ok.
%%% The user wants to move up one pos in the list box.
%%% Dir: Current working directory
%%% Dir used when clicked is called.

up (Dir) ->
    Last = gs:read(lb,size) -1,
    case gs:read(lb,selection) of
	[0] ->
	    gs:config(lb,{selection,clear}),
	    gs:config(lb,{selection,Last});
	[Pos]  ->
	    gs:config(lb,{selection,clear}),
	    gs:config(lb,{selection,Pos-1});
	_ ->
	    ok
    end,
    clicked (Dir).



%%% down(Dir) -> ok.
%%% The user wants to move down one pos in the list box.
%%% Dir: Current working directory
%%% Dir used when clicked is called.

down (Dir) ->
    Last = gs:read(lb,size) -1,
    case gs:read(lb,selection) of
	[Last] -> 
	    gs:config(lb,{selection,clear}),
	    gs:config(lb,{selection,0});
	[Pos]  ->
	    gs:config(lb,{selection,clear}),
	    gs:config(lb,{selection,Pos+1});
	_ -> 
	    ok
    end,
    clicked (Dir).  



%%% refresh(Dir,Removed) -> Items. Update the elements in the
%%%                                entry box.
%%% Items: Elements in the list box.
%%% Dir: Current working directory

refresh (Dir) ->
    gs:config (dir, {label ,{text, ["Working..."]}}),

    Items = case catch get_files (Dir) of
		{'EXIT', _} -> 
	             ["../"];

		Files -> 
                     Files
	    end,

    gs:config (lb, clear),
    gs:config (entry, {text, ""}),
    gs:config (lb, [{items, Items}]),
    gs:config (lb, {selection, clear}),
    gs:config (dir, {label ,{text, ["Dir: " | Dir]}}),
    Items.




%%% clicked(Dir,Owner). The entry in the list box has been
%%% changed, either manualy or by clicking on the item.

clicked (Dir) ->
    case gs:read (lb, selection) of
	[] -> 
	    no_file;

	[0] ->
	    gs:config (entry, {text, [filename:dirname (Dir)]}),
	    no_file; 

	[Index|_] ->
	    File = gs:read(lb, {get, Index}),
	    gs:config (entry, {text, File}),
	    File
    end.



%%% control_file  /2
%%%
%%% control_file returns a file, dir or an error message 
%%% depending of the given file
%%%

control_file (Dir, File) ->
    
    case check_file (Dir, File) of
	{file, Dir2, File2} ->
	    {file, Dir2, File2};
	
	{dir, Dir2} ->
	    refresh (Dir2),
	    gs:config (dir, {label, {text, ["Dir: " | Dir2]}}),
	    {dir, Dir2};

	{error, What} ->
	    gs:config (dir, {label, {text, ["Error: ", atom_to_list (What)]}}),
	    {dir, Dir}
    end.



%%% check_file(Dir,File) -> {file,Dir,File}|{dir,Dir}|Error
%%% checks if a file entered exists, and handles absolute/relative
%%% paths.
%%% returns {file,Dir,File}
%%%         {dir,Dir}
%%%    or   {error,What}

check_file (_, []) ->
     {error, 'no file'};

check_file (Dir, File) ->	
    Path = case filename:pathtype (File) of
	       absolute ->
		   File;

	       relative ->
		   filename:join (Dir, File);

	       volumerelative ->
		   File ++ "/"
	   end,

    case file:file_info (Path) of
	{ok, {_, directory, _, _, _, _, _}} ->
	    {dir, Path};

	{ok, {_, regular, _, _, _, _, _}} ->
	    case filename:extension (Path) of
		?EXTENSION ->
		    FileName = filename:basename (Path),
		    {file, filename:dirname (Path), Path};

		 _ ->
		    {error, 'wrong type of file'}
	    end;
	
	_ ->    
	    {error, 'unknown file or directory'}
    end.



%%% get_files(Dir,Remove) -> [Files/Dirs]
%%% Returns a list of files we want to display in the list box,
%%% removing non *.erl files and files in Remove

get_files (Dir) -> 
    {ok, Files} = file:list_dir (Dir),

    Up = case filename:dirname (Dir) of
	     Dir -> 
                 ".";
	     _ -> 
                 "../"
	 end,

    [Up | get_files (Dir, lists:sort (Files), [])].



get_files (Dir, [H | T], Buffer) ->

    case file:file_info (filename:join (Dir, H)) of
        {ok, {_, directory, _, _, _, _, _}} ->
	    H1 = lists:flatten (io_lib:format ("~s/", [H])),
	    get_files (Dir, T, [H1 | Buffer]);

        {ok, {_, regular, _, _, _, _, _}} ->
	    case filename:extension (H) of
		?EXTENSION ->
		    get_files (Dir, T, [H | Buffer]);

		_ ->
		    get_files (Dir, T, Buffer)
	    end;
	
	_  ->
	    get_files (Dir, T, Buffer)
    end;


get_files (_, [], Buffer) -> 
    lists:reverse (Buffer).




%%% no_name  /1
%%%

no_name (Mode) ->
    case Mode of
	'Save' ->
	    gs:config (entry, {text, "NoName.state"});

	Else ->
	    true
    end.

