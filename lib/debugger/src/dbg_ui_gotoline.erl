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
-module(dbg_ui_gotoline).

-export ([
	  start/4
	 ]).

-export ([init/4]).



%%% start  /4
%%%
%%% start spawns the init function with the given editor to search in.
%%%
%%% Pre:
%%%    ParentPid  ==  pid ()
%%%    Editor     ==  gs object
%%%    ParentWin  ==  gs object
%%%    Size       ==  integer (), #rows in Editor
%%%

start (ParentPid, Editor, ParentWin, Size) ->
    spawn_link (?MODULE, init, [ParentPid, Editor, ParentWin, Size]).



%%% init  /4
%%%
%%% init calls the win_init function to create the search window
%%%
%%% Pre:
%%%    ParentPid  ==  pid ()
%%%    Editor     ==  gs object
%%%    ParentWin  ==  gs object
%%%    Size       ==  integer (), #rows in Editor
%%%

init (ParentPid, Editor, ParentWin, Size) ->
    win_init (ParentWin),
    loop (ParentPid, Editor, 0, Size).



loop (ParentPid, Editor, OldLine, Size) ->
    receive
	{gs, gotoline_entry, keypress, _, ['Return' | _]} ->
	    NewOldLine = gotoline_action (ParentPid, Editor, OldLine, Size),
	    loop (ParentPid, Editor, NewOldLine, Size);
	

	{gs, _button, click, _, ["Go"]} ->
	    NewOldLine = gotoline_action (ParentPid, Editor, OldLine, Size),
	    loop (ParentPid, Editor, NewOldLine, Size);

	{gs, _button, click, _, ["Clear"]} ->
	    gs:config (gotoline_entry, {delete, {0, last}}),
	    clear_oldline (ParentPid, Editor, OldLine),
	    loop (ParentPid, Editor, 0, Size);


	{gs, _button, click, _, ["Close"]} ->
	    clear_oldline (ParentPid, Editor, OldLine),
	    exit (normal);


	Other ->
	    loop (ParentPid, Editor, OldLine, Size)
    end.



%%% gotoline_action  /3
%%%

gotoline_action (ParentPid, Editor, OldLine, Size) ->
    case get_line (Editor) of
	{ok, Line} when Line < Size ->
	    gs:config (gotoline_button, flash),
	    ParentPid ! {gotoline, Editor, Line, OldLine},
	    gs:config (gotoline_entry, {delete, {0, last}}),
	    Line;
	
	_False ->
	    OldLine
    end.



%%% get_line  /1
%%%

get_line (Editor) ->
    case catch list_to_integer (gs:read (gotoline_entry, text)) of
	Line when integer (Line) ->
	    {ok, Line};
	
	_False ->
	    false
    end.



%%% clear_oldline  /3
%%%

clear_oldline (ParentPid, Editor, OldLine) ->
    ParentPid ! {clear_oldline, Editor, OldLine}.



%%% win_init  /1
%%%
%%% win_init creates the search window.
%%%
%%% Pre:
%%%    ParentWin  ==  gs object
%%%                   view or attach window
%%%

win_init (ParentWin) ->
    W = 200,  % Window width
    H = 100,  % Window height
    BW = 50,  % Button width

    GS = gs:start (),

    gs:window (gotoline_win, GS, [{width, W}, {height, H},
				  {x, gs:read (ParentWin, x) + 50}, 
				  {y, gs:read (ParentWin, y) + 50},
				  {title, "Go to line"},
				  {configure, true}]),

    gs:entry (gotoline_entry, gotoline_win, [{width, W - 10},
					     {x, 5}, {y, 5},
					     {keypress, true}]),

    gs:button (gotoline_button, gotoline_win, [{width, BW},
					       {x, W/2 - 3/2 * BW - 10},
					       {y, 50},
					       {label, {text, "Go"}}]),

    gs:button (clear_button, gotoline_win, [{width, BW},
					    {x, W/2 - 1/2 * BW},
					    {y, 50},
					    {label, {text, "Clear"}}]),

    gs:button (close_button, gotoline_win, [{width, BW},
					    {x, W/2 + 1/2 * BW + 10},
					    {y, 50},
					    {label, {text, "Close"}}]),

    gs:config (gotoline_win, [{map, true}, raise]).


