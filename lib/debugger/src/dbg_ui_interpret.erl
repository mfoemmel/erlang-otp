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
-module(dbg_ui_interpret).

-include_lib("kernel/include/file.hrl").

%% External exports
-export([start/4]).

%% Internal exports
-export([init/6]).

-record(state, {gs,      % term() Graphics system id
		win,     % term() Interpret dialog window data
		monitor, % pid() Monitor pid
		mode     % local | global
	       }).

%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% start(GS, Pos, Dir, Mode)
%%   GS  = Graphics system id
%%   Dir = string()
%%   Pos = {X,Y}
%%   Mode = local | global
%%--------------------------------------------------------------------
start(GS, Pos, Dir, Mode) ->
    Title = "Interpret Dialog",
    case dbg_ui_winman:is_started(Title) of
	true -> ignore;
	false ->
	    spawn(?MODULE, init, [self(), GS, Pos, Title, Dir, Mode])
    end.

%%====================================================================
%% Internal exports
%%====================================================================

init(Monitor, GS, Pos, Title, Dir, Mode) ->
    Filter = filename:join(Dir, "*.erl"),
    Extra = fun(File) ->
		    case int:interpretable(File) of
			true ->
			    ModS = filename:basename(File, ".erl"),
			    Mod = list_to_atom(ModS),
			    case int:file(Mod) of
				File -> {true, tag};
				_ -> true % {error,not_loaded} | File2
			    end;
			Error -> {error, Error}
		    end
	    end,
			
    %% Create interpret dialog window
    Win = dbg_ui_filedialog_win:create_win(GS, Title, Pos, multiselect,
					   Filter, Extra),
    Window = dbg_ui_filedialog_win:get_window(Win),
    dbg_ui_winman:insert(Title, Window),

    State = #state{gs=GS, win=Win, monitor=Monitor, mode=Mode},
    loop(State).


%%====================================================================
%% Main loop and message handling
%%====================================================================

loop(State) ->
    receive

	%% From the GUI
	GuiEvent when tuple(GuiEvent), element(1, GuiEvent)==gs ->
	    Cmd = dbg_ui_filedialog_win:handle_event(GuiEvent,
						    State#state.win),
	    State2 = gui_cmd(Cmd, State),
	    loop(State2);

	%% From the dbg_ui_winman process (Debugger window manager)
	{dbg_ui_winman, update_windows_menu, Data} ->
	    loop(State);
	{dbg_ui_winman, destroy} ->
	    exit(normal)
    end.

gui_cmd(ignore, State) ->
    State;
gui_cmd({stopped, Dir}, State) ->	
    State#state.monitor ! {dbg_ui_interpret, Dir},
    exit(normal);
gui_cmd({win, Win}, State) ->
    State#state{win=Win};
gui_cmd({select, File}, State) ->
    Res = case State#state.mode of
	      local -> int:i(File);
	      global -> int:ni(File)
	  end,

    case Res of
	%% Interpretation succeeded, tag the file name
	{module, Mod} ->
	    FileName = filename:basename(File),
	    dbg_ui_filedialog_win:tag(State#state.win, File);

	%% Interpretation failed
	error ->
	    Error = format_error(int:interpretable(File)),
	    Msg = ["Error when interpreting:", File, Error],
	    tool_utils:notify(State#state.gs, Msg)
    end,
    State;
gui_cmd({multiselect, Dir, FileNames}, State) ->
    interpret_all(State, Dir, FileNames),
    State.

interpret_all(State, Dir, [File0|Files]) ->
    File = filename:join(Dir, File0),
    Res = case State#state.mode of
	      local -> int:i(File);
	      global -> int:ni(File)
	  end,
    case Res of
	{module, Mod} ->
	    dbg_ui_filedialog_win:tag(State#state.win, File),
	    interpret_all(State, Dir, Files);
	error ->
	    Error = format_error(int:interpretable(File)),
	    Msg = ["Error when interpreting:", File, Error,
		   "Ok to continue?"],
	    case tool_utils:confirm(State#state.gs, Msg) of
		ok -> interpret_all(State, Dir, Files);
		cancel -> true
	    end
    end;
interpret_all(_State, _Dir, []) ->
    true.

format_error(no_beam) -> "No BEAM file";
format_error(no_debug_info) -> "No debug_info in BEAM file";
format_error(badarg) -> "Not an Erlang module".
