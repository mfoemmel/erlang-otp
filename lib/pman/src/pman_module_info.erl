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

-module(pman_module_info).


%% External exports
-export([start/1]).

%%Internal exports
-export([init/2]).


%% Record for keeping the loop state for the 
%% module info process.

-record(module_state,{topwin,			%GS identifier for top window
		      editor,			%GS identifier for editor
		      modulename,		%Name of the viewed module
		      callingpid}).		%PID of the calling process




%%
%% Function for opening a window with module information
%%
%% start/1 will spawn a linked process that handles the
%% opened window.
%% 
%% The calling process must register for receiving 'EXIT' messages, since
%% an exiting module_info process will send an 'EXIT' message to notify
%% others of it's death.
%%
%%

start(ModuleName) ->
    spawn_link(?MODULE, init,[ModuleName,self()]).


init(ModuleName, CallingPid) ->

    %% Setup for handling the death of the caller in a graceful way
    process_flag(trap_exit, true),

    GS = gs:start([{kernel,true}]),

    WinTitle = lists:flatten(io_lib:format("PMAN:Module info: ~p",
					   [ModuleName])),

    Win_Options = [{title,WinTitle},
		   {width,550} ,{height, 400},
		   {configure,true}, {keypress,true}, {destroy,true}],
    TopWindow = gse:window(GS,Win_Options),
    
    MenuBar = gse:menubar(TopWindow, []),

    %% File menu
    MenuButtFile =  gse:menubutton(MenuBar,[{label,{text," File "}},
						 {underline, 1}]),
    MenuFile = gse:menu(MenuButtFile, []),

    gse:named_menuitem('Save buffer',MenuFile,
		       [{label,{text, "Save buffer..."}},
			{underline,0}]),
    gse:named_menuitem('Close',MenuFile,
		       [{label, {text, "Close"}},
			{underline,0}]),


    %% Output part of window
    Editor = gse:editor(TopWindow,
			[{x,3},{y,40},{width, 546}, {height, 348}]),
    gse:config(Editor, [{keypress, true},
			{insert, {'end', pman_win:module_data(ModuleName)}}]),
    gse:config(Editor, [{enable, false},{vscroll, right}, {hscroll, bottom},
		       {wrap,none}]),
    gse:map(TopWindow),
    Loopstate = #module_state{topwin=TopWindow,
			      editor=Editor,
			      modulename=ModuleName,
			      callingpid=CallingPid},
    
    loop(Loopstate).


loop(LoopState) ->

    receive
	%% Death of a linked process (i.e. the caller)
	{'EXIT', _Reason} ->
	    gse:destroy(LoopState#module_state.topwin);

	%% Destroy - destroy window and exit process.
	{gs, _TopWindow, destroy, [], []} ->
	    unlink(LoopState#module_state.callingpid),
	    gse:destroy(LoopState#module_state.topwin);


        {gs, TopWindow, configure ,_Data,[W,H,_X,_Y|_]} ->
	    configure (TopWindow, LoopState#module_state.editor, W, H),
	    loop (LoopState);

	%% Close - destroy window and exit process.
	{gs, 'Close', click, _Data, _Args} ->
	    unlink(LoopState#module_state.callingpid),
	    gse:destroy(LoopState#module_state.topwin);


	%% Save buffer - make filename, open dialog, and save buffer to file.
	{gs, 'Save buffer', click, _Data, _Args} ->

    	    DefaultFile =
		atom_to_list(LoopState#module_state.modulename) ++
		".module_info",
	    Result = tool_utils:file_dialog([{type,save},
					     {file,DefaultFile}]),
	    case Result of
		%% User selected a file, now save the result
		{ok, UserFile, _State} ->	    
		    gs:config(LoopState#module_state.editor,
			      {save,UserFile}),
		    Msg = "Module information saved in file\n" ++ UserFile,
		    pman_win:dialog_window(gs:start(),Msg);

		%% File dialog was cancelled in some way.
		{error,_Reason} ->
		    true
	    end,

	    loop(LoopState);
	
	{'EXIT', _Pid, Reason} ->
	    exit(Reason)

    end.

	

configure (_Win, Editor, W, H) ->
    gs:config (Editor, [{width, W - 3},
			{height, H - 40}]).
    
