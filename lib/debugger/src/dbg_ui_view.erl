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
%%% Purpose : dbg_ui_view is the user interface to the view window in the
%%%           new debugger. It starts the view window and then stays in
%%%           a receive loop.
%%% History : The int_show_new module has been divided into two seperate 
%%%           files for the view and trace (attach) windows in the new 
%%%           debugger.
%%%----------------------------------------------------------------------

-module (dbg_ui_view).


-include ("dbg_ui_data_struct.hrl").
-export ([start/1, init/1]).



%%% start (Module)
%%%
%%%

start (Module) ->
    spawn_link (?MODULE, init, [Module]).



%%% init (Module)
%%%
%%%

init (Module) ->
    Title = lists:flatten (io_lib:format ("View Module ~p", [Module])),

    case dbg_ui_winman:win_started (self (), Title) of
	true ->
	    exit (self (), kill),
            exit (already_exists);

	_false ->
	    ok
    end,

    dbg_ui_cache:start (self ()),   % initiate this window at dbg_ui_cache
    Win = dbg_ui_trace_win:create_attach_window (Title, view, dummy),
    dbg_ui_winman:insert_win (view, Win, self ()),

    start_view_loop (Module, Win).



%%% start_view_loop(Module,Win).
%%%
%%% Module: The module we want to see
%%% Initialize the environment for the view window, including the
%%% Trace records. Only some of the fields will be used.

start_view_loop (Module, Win) ->
    process_flag(trap_exit,true),
    int:start (),
    int:refresh (),
    X = gs:read (Win, width),
    Y = gs:read (Win, height),
    {Name, Breaks} = dbg_ui_aux:load_file (Module, [], [], undefined, Win),
    Mods = lists:sort(int:interpreted()),
    {ok, Dir} = file:get_cwd (),

    Trace = #trace {cm = Name,
		    size = {X, Y}, 
		    win = Win,
		    coords = {0, 0},
		    breaks = Breaks,
		    mods = [],
		    line = 0,
		    dir = Dir},
    
    view_loop (Trace).
    


%%% view_loop (Trace)
%%%
%%% The main loop of the view window.

view_loop (Trace) ->
    receive

	%% Windows menu

	{update_windows, Data} ->
	    dbg_ui_aux:update_windows (Data),
	    view_loop (Trace);

	
	%% Change of states in the system

	{new_dir, Dir} ->
	    view_loop (Trace#trace{dir = Dir});

	{P, new_break, {{Mod, Line}, Status}} ->
	    view_loop (dbg_ui_aux:break (Trace, {new_break, {Mod, Line, Status}}));

	{P, no_break, Br} ->
	    view_loop (dbg_ui_aux:break (Trace, {no_break, Br}));

	{P, delete_break, Br} ->
	    view_loop (dbg_ui_aux:break (Trace, {delete_break, Br}));

	{P, new_break_options, I} ->
	    view_loop (dbg_ui_aux:break (Trace, {new_break_options, I}));

	{P, interpret, Mod} -> 
  	    view_loop (Trace);

	{P, no_interpret, Mod} -> 
	    view_loop (Trace);

	
	%% Search calls

	{editor_search, CallingPid, String, Pos, CaseS} ->
	    Editor = dbg_ui_cache:current_editor (self()),
	    dbg_ui_search:editor_search (CallingPid, Editor,
					 String, Pos, CaseS),
	    view_loop (Trace);

	{unmark_string, Editor, Row} ->
	    dbg_ui_search:unmark_string (Editor, Row),
	    view_loop (Trace);

	{gotoline, Editor, Line, OldLine} ->
	    dbg_ui_trace_win:select_line (Editor, undefined_mode, 
					  Line, OldLine),
	    view_loop (Trace);

	{clear_oldline, Editor, OldLine} ->
	    dbg_ui_trace_win:select_line (Editor, clear, 0, OldLine),
	    view_loop (Trace);


	%% User interaction
	
	{gs, _W, configure , _, [X, Y|_]} ->
	    dbg_ui_trace_win:configure_view (Trace#trace.win, {X, Y}),
	    view_loop (Trace);		

	{gs, _, motion, _, [X, Y]} ->
	    view_loop (dbg_ui_aux:flush_motion (X, Y, Trace));

	{gs, _W, destroy, _, _} -> 
	    dbg_ui_winman:delete_win (Trace#trace.win),
	    exit (destroy);

	
	%%Some commands which must be handled separately from the trace menu	     
	
	{gs, _, keypress, _, [Key, _, _, 1]} ->
	    execute_view_cmd (Key, Trace);				     

        {gs, 'Delete All Modules', _, _, _} ->
	    view_loop (execute_view_cmd (a,  Trace));	

	{gs, Id, click, {'Delete', Mod}, _} ->
  	    execute_view_cmd ({'Delete', Mod},  Trace);			   

	{gs, Id, click, {'View', Mod}, _}  ->
	    execute_view_cmd ({'View', Mod},  Trace);	

	Gs_Cmd when tuple (Gs_Cmd),  element (1, Gs_Cmd) == gs ->
	    view_loop (dbg_ui_aux:gs_cmd (Gs_Cmd, Trace));


	%% Exit events

	{'EXIT', _, _} ->
	    view_loop (Trace);
	

	_ ->
	    view_loop (Trace)
    end.



%%% execute_view_cmd
%%%
%%% Execute user commands specific to the view Window

execute_view_cmd (a, Trace) ->
    lists:map ({int, nn}, Trace#trace.mods),
    Trace;

execute_view_cmd ({'Delete', Mod}, Trace) ->
    int:nn (Mod),
    view_loop (Trace);

execute_view_cmd ({'View', Mod}, Trace) ->
    {NMod, Breaks} = dbg_ui_aux:load_file (Mod, Trace#trace.cm, [],
					   Trace#trace.pid, Trace#trace.win),
    view_loop (Trace#trace{cm = NMod, breaks = Breaks, line = 0});

execute_view_cmd (Key, Trace) ->
    case lists:member (Key, [i, a, b, r, c]) of  %% Ugly, but quick. Only key
	false  -> 
	    view_loop (Trace);              %% accellerators allowed.
	true -> 
	    view_loop (dbg_ui_aux:execute_cmd (dbg_ui_aux:key (Key), Trace))
    end.





