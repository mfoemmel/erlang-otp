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
%%------------------------------------------------------------
%%
%% Simple text viewer
%%
%%------------------------------------------------------------
%%
%%
%%		INTRODUCTION
%%		------------
%%
%%------------------------------------------------------------



-module(appmon_txt).


-export([start/0, start/1, print/1, fprint/1]).

%% gen_server stuff
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).


-define(LOADTXT, "Load file").
-define(SAVETXT, "Save file").
-define(SAVEASTXT, "Save as").
-define(CLOSETXT, "Close").
-define(HELPTXT, "Help").

%%------------------------------------------------------------
%%
%% start/0 starts an open text viewer that can be filled with
%% whatever.
%%
%%------------------------------------------------------------



%%------------------------------------------------------------
%%
%% start(ListOfOptions) starts an open text viewer with options
%%
%% Options can be
%% {file, FileName}	- insert contents of file
%% locked		- the text cannot be edited
%% {text, Text}		- insert text at startup
%%
%%------------------------------------------------------------


start() ->
    start([]).

start(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% Start a text viewer if necessary
print(Txt) ->
%    case whereis(?MODULE) of
%	Pid when pid(Pid) ->
%	    ok;
%	Other ->
%	    start()
%    end,
    catch start(),
    gen_server:call(?MODULE, {add_txt, Txt}),
    ok.

fprint(File) ->
    catch start(),
    gen_server:call(?MODULE, {add_file, File}),
    ok.


%%------------------------------------------------------------
%% gen server admin

init(Opts) ->
    process_flag(trap_exit, true),
    %%io:format("Starting editor w args: ~p~n", [Opts]),
    setup_base_win(),
    default_status(),
    setup_opts(Opts),
%%    receive
%%    after 6000 -> ok end,
    {ok, []}.

terminate(Reason, State) ->
    %%io:format("Terminating editor:~n", []),
    ok.

%%------------------------------------------------------------
%% gen server stuff
handle_call({add_txt, Txt}, From, State) ->
    %%io:format("~p got call: ~p~n", [self(), Request]),
    do_insert_text(Txt),
    scroll_to_last_line(),
    {reply, ok, State};
handle_call({add_file, FileName}, From, State) ->
    %%io:format("~p got call: ~p~n", [self(), Request]),
    do_load_file(FileName),
    {reply, ok, State};
handle_call(Request, From, State) ->
    %%io:format("~p got call: ~p~n", [self(), Request]),
    {reply, ok, State}.
handle_cast(Request, State) ->
    %%io:format("~p got cast: ~p~n", [self(), Request]),
    {noreply, State}.
handle_info({gs, _, click, _, [?CLOSETXT|_]}, State) ->
    %%d:d("handle_info: exit: Close button pressed~n", []),
    {stop, normal, State};
handle_info({gs, _, click, _, [?LOADTXT|_]}, State) ->
    ui_load(),
    {noreply, State};
handle_info({gs, _, configure, _, [W, H | _]}, State) ->
    resize(W, H),
    {noreply, State};

handle_info({gs, _, destroy, _, _}, State) ->
%%    ?D("handle_info: exit: window killed~n", []),
    {stop, normal, State};
%%%handle_info({gs, _, click, _, [?HELPTXT|_]}, State) ->
%%%    d:d("~p: help:~n", [node()]),
%%%    print_help(),
%%%    {noreply, State};
%%%handle_info({gs, Id, click, {mode, Mode}, _}, State) ->
%%%    %%io:format("handle_info: Setting mode: ~p~n", [Mode]),
%%%    set_mode(Id, Mode),
%%%    {noreply, State};
handle_info(Request, State) ->
    io:format("~p got info: ~p~n", [self(), Request]),
    print_status("Not implemented"),
    {noreply, State}.


%%------------------------------------------------------------
%% Handle options

setup_opts([Opt|Opts]) -> 
    setup_opt(Opt),
    setup_opts(Opts);
setup_opts([]) -> ok.

setup_opt(Opt) ->
    case Opt of
	{file, FileName} ->
	    do_load_file(FileName);
	locked ->
	    do_lock();
	{text, Text} ->
	    do_insert_text(Text);
	Other ->
	    ok
    end.

do_load_file(FileName) ->
    Msg = case catch i_load_file(FileName) of
	      ok ->
		  default_status();
	      Other -> 
		  print_status(lists:append(["File not found: ", FileName]))
	  end.


i_load_file(FileName) ->
    {ok, Bin} = file:read_file(FileName),
    L = binary_to_list(Bin),
    i_do_clear(),
    do_insert_text(L),
    ok.

ui_load() ->
    Title = "Load file",
    Files = get_file_list(),
    case catch ui_list_dialog(Title, "File: ", Files) of
	{ok, FileName} ->
	    do_load_file(FileName);
	Other ->
	    print_status("Load cancelled")
    end.

get_file_list() ->
    case file:list_dir(".") of
	{ok, FileList} -> lists:sort(FileList);
	Other -> []
    end.



do_insert_text(Text) ->
    gs:config(editor(), {insert, {'end', Text}}),
    ok.

%% Scrolls editor to show the last rows
scroll_to_last_line() ->
    H = gs:read(editor(), size),
    R = gs:read(editor(), height),
    TopRow = H-R/15,
%%    io:format("Top: ~p, ~p - ~p~n", [TopRow, H, R]),
    if  TopRow > 0 -> gs:config(editor(), {vscrollpos, TopRow});
	true       -> gs:config(editor(), {vscrollpos, 0})
	end,
    ok.


do_lock() ->    
    gs:config(editor(), {enable, false}).



i_do_clear() ->
    gs:config(editor(), clear).


%%------------------------------------------------------------
%% Graphical stuff

label_h() -> 20.
menu_h() -> 29.

setup_base_win() ->
    H = 400, W=580,
    LabelHeight=label_h(), MenuHeight=menu_h(),

    F = gs:start([{kernel,true}]),
    set_winroot(F),

    Win = gs:create(window, F, [{width, W}, {height, H}, 
				{title, "APPMON: Process Information"}]),

    E = gs:create(editor, Win, [{x, 0}, {y, MenuHeight}, 
				{width, W}, 
				{height, H-MenuHeight-LabelHeight-1}, 
				{vscroll, right}]),
    set_editor(E),

    L = gs:create(label, Win, [{x, 0}, {y, H-LabelHeight}, 
			       {height,LabelHeight }, {width, W}, 
			       {align, w}]),
    set_status(L),
    print_status("Loading"),

    gs:config(Win, {map, true}),

    MB = gs:create(menubar, Win, []),

    FMB = gs:create(menubutton, MB, [{label, {text, "File"}}]),
    FM = gs:create(menu, FMB, []),
    %%gs:create(menuitem, FM, [{label, {text, ?LOADTXT}}]),
    %%gs:create(menuitem, FM, [{label, {text, ?SAVETXT}}]),
    %%gs:create(menuitem, FM, [{label, {text, ?SAVEASTXT}}]),
    %%gs:create(menuitem, FM, [{itemtype, separator}]),
    gs:create(menuitem, FM, [{label, {text, ?CLOSETXT}}]),

%%    EMB = gs:create(menubutton, MB, [{label, {text, "Edit"}}]),
%%    EM = gs:create(menu, EMB, []),
    
    
%    HMB = gs:create(menubutton, MB, [{label, {text, "Help"}}, {side, right}]),
%    HM = gs:create(menu, HMB, []),
%    gs:create(menuitem, HM, [{label, {text, ?HELPTXT}}]),

    gs:config(Win, {configure, true}),
    ok.

resize(W, H) ->
%%    io:format("Resizing to: ~p, ~p~n", [W, H]),
    %% resize editor
    gs:config(editor(), [{width, W}, {height, H-label_h()-menu_h()}]),
    %% resize move label
    gs:config(status(), [{y, H-label_h()}, {width, W}]),
    ok.



%%------------------------------------------------------------
%% ui_list_dialog(
%%
%%	Traditional dialog with a list box and a selection field that
%%	is updated from the list box.
%%
%%	Returns {ok, String} if successful and something else if not
%%
%%	Title - the name of the window
%%	LeadText - the lead text on the selection field
%%	List - a list of items that will be displayed in the list box
%%
%%------------------------------------------------------------

ui_list_dialog(Title, LeadText, TxtList) ->
    W = 200, H = 300,
    
    Win = gs:create(window, winroot(), [{title, Title}, 
					{width, W},{height, H}]),
    Ok = gs:create(button, Win, [{x, 10}, {y,10}, {width, 50}, {height, 20},
				 {label, {text, "Ok"}}]),
    Cn = gs:create(button, Win, [{x, 70}, {y,10}, {width, 50}, {height, 20},
				 {label, {text, "Cancel"}}]),

    gs:create(label, Win, [{x, 10}, {y, 50}, {width, 60}, {height, 20},
			   {label, {text, LeadText}}]),
    Box = gs:create(entry, Win, [{x, 10}, {y, 70}, {width, 160}, {height, 20},
				 {keypress, true}]),
    List = gs:create(listbox, Win, [{x, 10}, {y, 100}, {width, 180}, 
				    {height, 190},
				    {items, TxtList}, {vscroll, right},
				    {hscroll, false}, {click, true},
				    {doubleclick, true}, {keypress, true}]),
    gs:config(Win, {map, true}),

    RetVal = ui_load_loop(Box, List, Ok, Cn),

    gs:destroy(Win),
    
    RetVal.

ui_load_loop(Box, List, Ok, Cn) ->    
    receive
	{gs, Box, keypress, _, ['Return'|_]} ->
	    {ok, gs:read(Box, text)};
	{gs, Box, keypress, _, _} ->
	    ui_load_loop(Box, List, Ok, Cn);
	{gs, Ok, click, _, _} ->
	    {ok, gs:read(Box, text)};
	{gs, List, doubleclick, _, [Idx, Txt|_]} ->
	    {ok, Txt};
	{gs, List, click, _, [Idx, Txt|_]} ->
	    gs:config(Box, {text, Txt}),
	    ui_load_loop(Box, List, Ok, Cn);
	Other -> 
	    something_else
    end.




%% The status row at the bottom of the window
set_status(Id) -> put(status_row, Id).
status() -> get(status_row).
print_status(Msg) -> gs:config(get(status_row), {label, {text, Msg}}).
default_status() -> print_status("Done").

set_editor(X) -> put(editor, X).
editor() -> get(editor).

winroot() -> get(winroot).
set_winroot(X) -> put(winroot, X).

%%destroy(undefined) -> ok;
%%destroy(Id) -> gs:destroy(Id).




