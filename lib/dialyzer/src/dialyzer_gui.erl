%% -*- erlang-indent-level: 2 -*-
%%------------------------------------------------------------------------
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
%% Copyright 2006, Tobias Lindahl and Kostis Sagonas
%% 
%%     $Id$
%%

%%%-----------------------------------------------------------------------
%%% File    : dialyzer_gui.erl
%%% Authors : Tobias Lindahl <tobiasl@csd.uu.se>
%%%           Kostis Sagonas <kostis@it.uu.se>
%%% Description : The graphical user interface for the Dialyzer tool.
%%%
%%% Created : 27 Apr 2004 by Tobias Lindahl <tobiasl@csd.uu.se>
%%%-----------------------------------------------------------------------

-module(dialyzer_gui).

-export([start/1]).

-include("dialyzer.hrl").

-record(gui_state, {add_all, add_file, add_rec,
		    chosen_box, analysis_pid, del_file,
		    doc_plt, clear_chosen, clear_log, clear_warn, 
		    empty_plt, fixpoint, init_plt, 
		    user_plt, dir_entry, file_box, file_wd, gs, log, menu, mode,
		    options, packer, run, stop, top, warnings_box,
		    backend_pid}).

-record(menu, {file_save_log, file_save_warn, file_quit, 
	       help_about, help_manual, help_overview, help_warnings, 
	       opts_supress_inline, opts_macros, opts_includes,
	       plt_empty, plt_search_doc, plt_show_doc,
	       warnings}).
	       
-record(mode, {gran_all, start_byte_code, iter_fixpoint, gran_module,
	       start_src_code, iter_qad, dataflow, succ_typings}).


start(DialyzerOptions = #options{}) ->
  process_flag(trap_exit, true),

  GS = gs:start(),
  code:add_pathsa(["."]),
  WH = [{width, 1000}, {height, 550}],

  {ok,Host} = inet:gethostname(),
  %% --------- Top Window --------------
  TopWin = gs:window(GS, [{title, "Dialyzer "++?VSN++" @ "++Host},
			  {configure, true},
			  {default, listbox, {bg, white}},
			  {default, editor, {bg, white}},
			  {default, entry, {bg, white}},
			  {default, button, {font, {helvetica, bold, 12}}},
			  {default, label, {font, {helvetica, bold, 12}}}
			  |WH]),
  Packer = gs:frame(TopWin, [{packer_x, [{stretch, 3},{fixed, 200},
					 {stretch, 7}]},
			     {packer_y, [{fixed, 25}, {fixed, 20},
					 {stretch, 1, 50},
					 {fixed, 25}, {fixed, 20},
					 {stretch, 1, 50},
					 {fixed, 25}]}]),

  %% --------- Chosen box --------------
  gs:label(Packer, [{label, {text, "Directories or modules to analyze"}}, 
		    {height, 20}, {pack_xy, {1,2}}]),
  ChosenBox = gs:listbox(Packer, [{pack_xy, {1,3}},{vscroll, right},
				{selectmode, multiple}]),

  %% --------- File box --------------
  gs:label(Packer, [{label, {text, "File"}}, {height, 20}, {pack_xy, {1,5}}]),
  FilePacker = gs:frame(Packer, [{packer_x, [{fixed, 30}, {stretch, 1, 100}]},
				 {packer_y, [{fixed, 25}, {stretch, 1, 25}]},
				 {pack_xy, {1, 6}}]),
  gs:label(FilePacker, [{label, {text, "Dir:"}}, {pack_xy, {1,1}}]),
  DirEntry = gs:entry(FilePacker, [{height, 30}, {pack_xy, {2,1}},
				   {keypress, true}]),
  File = gs:listbox(FilePacker, [{pack_x, {1,2}}, {pack_y, 2},
				 {selectmode, multiple}, {doubleclick, true}, 
				 {vscroll, right}]),

  %% --------- Options --------------
  gs:label(Packer, [{label, {text, "Analysis Options"}}, 
		    {height, 20}, {pack_xy, {2, 2}}]),
  ModePacker = gs:frame(Packer, [{packer_x, [{fixed, 75},{fixed, 120}]},
				 {packer_y, [{fixed, 20},{fixed, 20},
					     {stretch, 1}, % empty space
					     {fixed, 20}, {fixed, 20},
					     {stretch, 1}, % empty space
					     {fixed, 20}, {fixed, 20},
					     {stretch, 1}, % empty space
					     {fixed, 20}, {fixed, 20},
					     {stretch, 1}, % empty space
					     {fixed, 15}, {stretch, 1}]},
				 {bw, 10}, {relief, flat},
				 {default, {radiobutton, {align, w}}},
				 {default, {label, {align, w}}},
				 {pack_xy, {2, 3}}]),

  %% Bytecode vs. Source code
  gs:label(ModePacker, [{label, {text, "Analyze:"}},
			{height, 20}, {pack_xy, {1,1}}]),
  ModeByteCode = gs:radiobutton(ModePacker,
				[{group, start_from},
				 {label, {text,"ByteCode"}},
				 {select, true},
				 {pack_xy, {2,1}}]),
  ModeSrcCode = gs:radiobutton(ModePacker,
			       [{group, start_from},
				{label, {text,"SourceCode"}},
				{pack_xy, {2,2}}]),

  %% Analyze modules or all chosen
%%  gs:label(ModePacker, [{label, {text, "Granularity:"}},
%%			{height, 20}, {pack_xy, {1,4}}]),
%%  ModeFileModule = gs:radiobutton(ModePacker,
%%				  [{group, granularity},
%%				   {label, {text,"Module-local"}}, 
%%				   {pack_xy, {2,4}}]),
%%  ModeFileAll = gs:radiobutton(ModePacker,
%%			       [{group, granularity},
%%				{select, true},
%%				{label, {text,"Global"}}, 
%%				{pack_xy, {2,5}}]),

  %% Iteration Mode
%%  gs:label(ModePacker, [{label, {text, "Iteration:"}}, 
%%			{height, 20}, {pack_xy, {1,7}}]),
%%  ModeAnalysisQaD = gs:radiobutton(ModePacker,
%%				   [{group, analysis},
%%				    {label,{text,"One pass"}}, 
%%				    {pack_xy, {2,7}}]),
%%  ModeAnalysisFixpoint = gs:radiobutton(ModePacker,
%%					[{group, analysis},
%%					 {label,{text,"Fixpoint"}}, 
%%					 {select, true},
%%					 {pack_xy, {2,8}}]),

  %% Core transform
%%  gs:label(ModePacker, [{label, {text, "Analysis:"}}, 
%%			{height, 20}, {pack_xy, {1,10}}]),
%%  ModeDataflow = gs:radiobutton(ModePacker,
%%			      [{group, core},
%%			       {label,{text,"Quick & dirty"}}, 
%%			       {select, true},
%%			       {pack_xy, {2,10}}]),
%%  ModeSuccTypings = gs:radiobutton(ModePacker,
%%			       [{group, core},
%%				{label,{text,"More precise"}}, 
%%				{pack_xy, {2,11}}]),

  Mode = #mode{start_byte_code=ModeByteCode,
	       start_src_code=ModeSrcCode
	       %% gran_all=ModeFileAll,
	       %% gran_module=ModeFileModule, 
	       %% iter_fixpoint=ModeAnalysisFixpoint, 
	       %% iter_qad=ModeAnalysisQaD,
	       %% dataflow=ModeDataflow,
	       %% succ_typings=ModeSuccTypings
	      },

  %% --------- Log box --------------
  gs:label(Packer, [{label, {text, "Log"}}, {height, 20}, {pack_xy, {3,2}}]),
  Log = gs:editor(Packer, [{pack_x, 3}, {pack_y, 3}, {enable, false},
			   {font, {courier, 12}}, {vscroll, right},
			   {wrap, word}]),

  %% --------- Warnings box --------------
  gs:label(Packer, [{label, {text, "Warnings"}},{height, 20},{pack_xy, {3,5}}]),
  WarningsBox = gs:editor(Packer, [{pack_x, {2,3}}, {pack_y, 6},
				   {enable, false},
				   {font, {courier, 12}},{vscroll, right},
				   {wrap, word}]),

  %% --------- Buttons --------------
  ButtonPackerHighLeft = 
    gs:frame(Packer, [{packer_x, [{fixed, 50},
				  {fixed, 65},
				  {stretch,1}]}, % empty space
		      {pack_xy, {1,4}}]),
  ButtonPackerHighRight = 
    gs:frame(Packer, [{packer_x, [{fixed, 70},
				  {fixed, 70},
				  {stretch,1}]}, % empty space
		      {pack_xy, {3,4}}]),
  ButtonPackerLowLeft = 
    gs:frame(Packer, [{packer_x, [{fixed, 50},
				  {fixed, 60},
				  {fixed, 110},
				  {stretch,1}]}, % empty space
		      {pack_xy, {1,7}}]),
  ButtonPackerLowRight = 
    gs:frame(Packer, [{packer_x, [{fixed, 70},
				  {fixed, 70},
				  {stretch,1},% empty space
				  {fixed, 70},
				  {fixed, 70}]}, 
		      {pack_x, {2,3}}, {pack_y, 7}]),

  WHButton = [{width, 60},{height, 20}],
  AddFile = gs:button(ButtonPackerLowLeft, [{pack_xy, {1,1}}, 
					    {label, {text,"Add"}}|WHButton]),
  AddAll = gs:button(ButtonPackerLowLeft, [{pack_xy, {2,1}}, 
					   {label, {text,"Add All"}}|WHButton]),
  AddRec = gs:button(ButtonPackerLowLeft, [{pack_xy, {3,1}}, 
                                           {label, {text,"Add Recursively"}}
                                           |WHButton]),
  DelFile = gs:button(ButtonPackerHighLeft, [{pack_xy, {1,1}}, 
					    {label, {text,"Delete"}}|WHButton]),
  ClearChosen = gs:button(ButtonPackerHighLeft, [{pack_xy, {2,1}},
					         {label, {text,"Delete All"}}
					         |WHButton]),
  ClearLog = gs:button(ButtonPackerHighRight, [{pack_xy, {1,1}}, 
					       {label, {text,"Clear Log"}}
					       |WHButton]),
  ClearWarn = gs:button(ButtonPackerLowRight, [{pack_xy, {1,1}}, 
					       {label, {text,"Clear Warn"}}
					       |WHButton]),

  Run = gs:button(ButtonPackerLowRight, [{pack_xy, {4,1}},
					 {label, {text,"Run"}}|WHButton]),
  Stop = gs:button(ButtonPackerLowRight, [{pack_xy, {5,1}}, {enable, false}, 
					  {label, {text,"Stop"}}|WHButton]),

  %% --------- Menu --------------  
  MenuBar = gs:menubar(TopWin, []),

  %% File Menu
  MenuBarFile = gs:menubutton(MenuBar, [{label,{text,"File"}}]),
  MenuFile = gs:menu(MenuBarFile, []),
  MenuFileSaveWarn = gs:menuitem(MenuFile, [{label,{text,"Save Warnings"}}]),
  MenuFileSaveLog = gs:menuitem(MenuFile, [{label,{text,"Save Log"}}]),
  MenuFileQuit = gs:menuitem(MenuFile, [{label,{text,"Quit"}}]),

  %% Warnings Menu
  MenuBarWarn = gs:menubutton(MenuBar, [{label,{text,"Warnings"}}]),
  MenuWarn = gs:menu(MenuBarWarn, []),

  MenuWarnMatch = gs:menuitem(MenuWarn, [{label, {text, "Match failures"}}, 
					 {itemtype, check}, {select, true}]),
  MenuWarnFailingCall = gs:menuitem(MenuWarn, 
				    [{label, {text, "Failing function calls"}}, 
				     {itemtype, check}, {select, true}]),
  MenuWarnFunApp = gs:menuitem(MenuWarn, [{label, 
					   {text, "Bad fun-applications"}}, 
					  {itemtype, check}, {select, true}]),
  MenuWarnLists = gs:menuitem(MenuWarn, [{label, {text, "Improper list constructions"}}, 
					 {itemtype, check}, {select, true}]),
  MenuWarnGuards = gs:menuitem(MenuWarn, [{label, {text, "Failing guards"}}, 
					  {itemtype, check}, {select, true}]),
  MenuWarnEq = gs:menuitem(MenuWarn, [{label, {text, "Failing term comparisons"}}, 
				      {itemtype, check}, {select, true}]),
  MenuWarnNotCalled = gs:menuitem(MenuWarn, 
				  [{label, {text, "Unused functions"}}, 
				   {itemtype, check}, {select, true}]),
  MenuWarnTupleAsFun = gs:menuitem(MenuWarn, [{label, 
					       {text, "Tuple used as fun"}}, 
					      {itemtype, check}, 
					      {select, true}]),
  MenuWarnBeam = gs:menuitem(MenuWarn, [{label, {text, "Unsafe BEAM code"}}, 
					{itemtype, check}, {select, true}]),
  MenuWarnReturnOnlyExit = gs:menuitem(MenuWarn,
				       [{label, 
					 {text, "Error handling functions"}},
					{itemtype, check}, {select, false}]),
  MenuWarnReturnNoReturn = gs:menuitem(MenuWarn,
				       [{label, 
					 {text, "Functions of no return"}},
					{itemtype, check}, {select, true}]),  
  MenuWarnCallNonExported = gs:menuitem(MenuWarn,
					[{label, 
					  {text,"Call to unexported function"}},
					 {itemtype, check}, {select, true}]),  
  
  %% PLT Menu
  MenuBarPLT = gs:menubutton(MenuBar, [{label, {text,"PLT"}}]),
  MenuPLT = gs:menu(MenuBarPLT, []),
  MenuPLTEmpty = gs:menuitem(MenuPLT, [{label,{text,"Init with empty PLT"}},
				       {itemtype, check}, {select, false}]),
  MenuPLTShow = gs:menuitem(MenuPLT, [{label,{text,"Show contents"}}]),
  MenuPLTSearch = gs:menuitem(MenuPLT, [{label,{text,"Search contents"}}]),

  %% Options Menu
  MenuBarOpts = gs:menubutton(MenuBar, [{label,{text,"Options"}}]),
  MenuOpts = gs:menu(MenuBarOpts, []),
  MenuOptsCheckInline = gs:menuitem(MenuOpts, 
				    [{label, {text, "Ignore inline-compiled bytecode"}}, 
				     {itemtype, check}, {select, false}]),
  MenuOptsMacros = gs:menuitem(MenuOpts,[{label, {text, "Manage Macro Definitions"}}]),
  MenuOptsIncludes = gs:menuitem(MenuOpts, [{label, {text, "Manage Include Directories"}}]),
  
  %% Help
  MenuBarHelp = gs:menubutton(MenuBar, [{label,{text,"Help"}}, {side, right}]),
  MenuHelp = gs:menu(MenuBarHelp, []),
  MenuHelpOverview = gs:menuitem(MenuHelp, [{label,{text,"Overview"}}]),
  MenuHelpManual = gs:menuitem(MenuHelp, [{label,{text,"Manual"}}]),
  MenuHelpWarnings = gs:menuitem(MenuHelp, [{label,{text,"Warnings"}}]),
  MenuHelpAbout = gs:menuitem(MenuHelp, [{label,{text,"About"}}]),
  
  Warnings = [{?WARN_RETURN_NO_RETURN, MenuWarnReturnNoReturn},
	      {?WARN_RETURN_ONLY_EXIT, MenuWarnReturnOnlyExit},
	      {?WARN_NOT_CALLED, MenuWarnNotCalled},
	      {?WARN_NON_PROPER_LIST, MenuWarnLists},
	      {?WARN_TUPLE_AS_FUN, MenuWarnTupleAsFun},
	      {?WARN_FUN_APP, MenuWarnFunApp},
	      {?WARN_MATCHING, MenuWarnMatch},
	      {?WARN_COMP, MenuWarnEq},
	      {?WARN_GUARDS, MenuWarnGuards},
	      {?WARN_OLD_BEAM, MenuWarnBeam},
	      {?WARN_FAILING_CALL, MenuWarnFailingCall},
	      {?WARN_CALLGRAPH, MenuWarnCallNonExported}
	     ],

  init_warnings(Warnings, DialyzerOptions#options.legal_warnings),

  Menu = #menu{file_quit=MenuFileQuit,
	       plt_empty=MenuPLTEmpty,
	       help_overview=MenuHelpOverview,
	       help_manual=MenuHelpManual,
	       help_about=MenuHelpAbout,
	       help_warnings=MenuHelpWarnings,
	       opts_supress_inline=MenuOptsCheckInline,
	       opts_macros=MenuOptsMacros,
	       opts_includes=MenuOptsIncludes,
	       plt_search_doc=MenuPLTSearch,
	       plt_show_doc=MenuPLTShow,
	       file_save_log=MenuFileSaveLog,
	       file_save_warn=MenuFileSaveWarn,
	       warnings=Warnings},


  %% --------- Init --------------
  gs:config(TopWin, {map, true}),
  gs:config(Packer, WH),
  {ok, CWD} = file:get_cwd(),
  
  EmptyPlt = dialyzer_plt:new(dialyzer_empty_plt),
  InitPlt0 = DialyzerOptions#options.init_plt,
  InitPlt = dialyzer_plt:from_file(dialyzer_init_plt, InitPlt0),

  State = #gui_state{add_all=AddAll,
		     add_file=AddFile, 
		     add_rec=AddRec,
		     chosen_box=ChosenBox, 
		     clear_chosen=ClearChosen, 
		     clear_log=ClearLog, 
		     clear_warn=ClearWarn, 
		     del_file=DelFile, 
		     dir_entry=DirEntry,
		     empty_plt=EmptyPlt,
		     file_box=File, 
		     file_wd=CWD,
		     gs=GS, 
		     init_plt=InitPlt,
		     log=Log, 
		     menu=Menu,
		     mode=Mode,
		     options=DialyzerOptions,
		     packer=Packer, 
		     run=Run,
		     stop=Stop,
		     top=TopWin, 
		     warnings_box=WarningsBox},
  NewState = change_dir_or_add_file(State, "."),
  gui_loop(NewState).


%% ----------------------------------------------------------------
%%
%%  Main GUI Loop
%%

gui_loop(State = #gui_state{}) ->
  TopWin = State#gui_state.top,
  Packer = State#gui_state.packer,
  ChosenBox = State#gui_state.chosen_box,
  File = State#gui_state.file_box,
  DirEntry = State#gui_state.dir_entry,
  Run = State#gui_state.run,
  AddFile = State#gui_state.add_file,
  AddAll = State#gui_state.add_all,
  AddRec = State#gui_state.add_rec,
  DelFile = State#gui_state.del_file,
  ClearChosen = State#gui_state.clear_chosen,
  ClearLog = State#gui_state.clear_log,
  ClearWarn = State#gui_state.clear_warn,
  Stop = State#gui_state.stop,
  Log = State#gui_state.log,
  BackendPid = State#gui_state.backend_pid,
  Options = State#gui_state.options,

  %% --- Menu ---
  Menu = State#gui_state.menu,
  Quit = Menu#menu.file_quit,
  Overview = Menu#menu.help_overview,
  Manual = Menu#menu.help_manual,
  HelpWarnings = Menu#menu.help_warnings,
  About = Menu#menu.help_about,
  SaveLog = Menu#menu.file_save_log,
  SaveWarn = Menu#menu.file_save_warn,
  SearchPlt = Menu#menu.plt_search_doc,
  ShowPlt = Menu#menu.plt_show_doc,
  Macros = Menu#menu.opts_macros,
  Includes = Menu#menu.opts_includes,
  
  receive
    {gs, TopWin, configure, _Data, [W, H|_]} ->
      gs:config(Packer, [{width, W}, {height, H}]),
      gui_loop(State);
    {gs, TopWin, destroy, _Data, _Args} ->
      ?RET_NOTHING_SUSPICIOUS;
    {gs, File, doubleclick, _, [_Id, Text|_]} ->
      NewState = change_dir_or_add_file(State, Text),
      gui_loop(NewState);
    {gs, DirEntry, keypress, _, ['Return'|_]} ->
      gs:config(TopWin, {setfocus, true}),
      NewState = change_dir_absolute(State, gs:read(DirEntry, text)),
      gui_loop(NewState);
    {gs, DirEntry, keypress, _, _} ->
      gui_loop(State);
    %% ----- Buttons -----
    {gs, AddFile, click, _, _} ->
      handle_add_files(State),
      gui_loop(State);
    {gs, AddAll, click, _, _} ->
      handle_add_all_click(State),
      gui_loop(State);
    {gs, AddRec, click, _, _} ->
      handle_add_rec_click(State),
      gui_loop(State);
    {gs, DelFile, click, _, _} ->
      handle_file_delete(State),
      gui_loop(State);
    {gs, ClearChosen, click, _, _} ->
      gs:config(ChosenBox, clear),
      gui_loop(State);
    {gs, ClearLog, click, _, _} ->
      Log = State#gui_state.log,
      gs:config(Log, [{enable, true}]),
      gs:config(Log, clear),
      gs:config(Log, [{enable, false}]),
      gui_loop(State);
    {gs, ClearWarn, click, _, _} ->
      Warn = State#gui_state.warnings_box,
      gs:config(Warn, [{enable, true}]),
      gs:config(Warn, clear),
      gs:config(Warn, [{enable, false}]),
      gui_loop(State);
    {gs, Run, click, _, _} ->
      NewState = start_analysis(State),
      gui_loop(NewState);
    {gs, Stop, click, _, _} ->
      dialyzer_plt:delete(State#gui_state.user_plt),
      config_gui_stop(State),
      BackendPid ! {self(), stop},
      update_editor(State#gui_state.log, "\n***** Analysis stopped ****\n"),
      gui_loop(State);
    %% ----- Menu -----
    {gs, Quit, click, _, _} ->
      case maybe_quit(State) of
	true -> ?RET_NOTHING_SUSPICIOUS;
	false -> gui_loop(State)
      end;
    {gs, Overview, click, _, _} ->
      spawn_link(fun() -> overview(State) end),
      gui_loop(State);
    {gs, Manual, click, _, _} ->
      spawn_link(fun() -> manual(State) end),
      gui_loop(State);
    {gs, HelpWarnings, click, _, _} ->
      spawn_link(fun() -> help_warnings(State) end),
      gui_loop(State);
    {gs, About, click, _, _} ->
      spawn_link(fun() -> about(State) end),
      gui_loop(State);
    {gs, SaveLog, click, _, _} ->
      save_log(State),
      gui_loop(State);
    {gs, SaveWarn, click, _, _} ->
      save_warn(State),
      gui_loop(State);
    {gs, SearchPlt, click, _, _} ->
      spawn_link(fun() -> search_doc_plt(State) end),
      gui_loop(State);
    {gs, ShowPlt, click, _, _} ->
      spawn_link(fun() -> show_doc_plt(State) end),
      gui_loop(State);
    {gs, Macros, click, _, _} ->
      Self = self(),
      spawn_link(fun() -> macro_dialog(State, Self) end),
      gui_loop(State);
    {gs, Includes, click, _, _} ->
      Self = self(),
      spawn_link(fun() -> include_dialog(State, Self) end),
      gui_loop(State);
    {new_options, NewOptions} ->
      NewState = State#gui_state{options=NewOptions},
      gui_loop(NewState);
    %% ----- Analysis -----
    {BackendPid, ext_calls, ExtCalls} ->
      Msg = io_lib:format("The following functions are called "
			  "but type information about them is not available.\n"
			  "The analysis might get more precise by including "
			  "the modules containing these functions:\n\n\t~p\n", 
			  [ExtCalls]),
      free_editor(State, Msg, "Analysis done"),
      gui_loop(State);
    {BackendPid, log, LogMsg} ->
      update_editor(State#gui_state.log, LogMsg),
      gui_loop(State);
    {BackendPid, warnings, Warnings} ->
      WarningString = 
	case Options#options.core_transform of
	  dataflow ->
	    lists:flatten([io_lib:format("~w: ~s", [Fun, W])
			   || {Fun, W} <- Warnings]);
	  core_warnings ->
	    lists:flatten([io_lib:format("~s:~w: ~s", 
					 [filename:basename(File), Line, W])
			   || {{File, Line}, W} <- Warnings])
	end,
      update_editor(State#gui_state.warnings_box, WarningString),
      gui_loop(State);
    {BackendPid, inline_warnings, Warnings} ->
      update_editor(State#gui_state.warnings_box, Warnings),
      gui_loop(State);
    {BackendPid, error, Msg} ->
      update_editor(State#gui_state.warnings_box, Msg),
      update_editor(State#gui_state.log, 
		    "*** Analysis failed! See warnings for details\n"),
      gui_loop(State);
    {BackendPid, done} ->
      dialyzer_plt:delete(State#gui_state.user_plt),
      message(State, "Analysis done"),
      config_gui_stop(State),
      gui_loop(State);
    {'EXIT', BackendPid, {error, Reason}} ->
      dialyzer_plt:delete(State#gui_state.user_plt),
      error(State, Reason),
      config_gui_stop(State),
      gui_loop(State);
    {'EXIT', BackendPid, Reason} when Reason =/= 'normal' ->
      dialyzer_plt:delete(State#gui_state.user_plt),
      error(State, io_lib:format("~p", [Reason])),
      config_gui_stop(State),
      gui_loop(State);
    _Other ->
      %io:format("Received ~p\n", [Other]),
      gui_loop(State)
  end.

%% ----------------------------------------------------------------
%%
%%  Main window actions
%%

%% ---- Adding and deleting files ----

handle_add_all_click(#gui_state{chosen_box=ChosenBox, file_box=File, 
				file_wd=FWD, mode=Mode}) ->
  case gs:read(File, items) of
    [] ->
      ok;
    Add0 ->
      gs:config(File, [{selection, clear}]),
      Add1 = ordsets:subtract(Add0, [".."]),
      Add = ordsets:from_list([filename:join(FWD, X) || X <- Add1]),
      case gs:read(Mode#mode.start_byte_code, select) of
	true ->
	  add_files(filter_mods(Add, ".beam"), ChosenBox, byte_code);
	false ->
	  add_files(filter_mods(Add, ".erl"), ChosenBox, src_code)
      end  
  end.

all_subdirs(Dirs) ->
  all_subdirs(Dirs, []).

all_subdirs([Dir|T], Acc) ->
  {ok, Files} = file:list_dir(Dir),
  SubDirs = lists:zf(fun(F) ->
                       SubDir = filename:join(Dir, F),
                       case filelib:is_dir(SubDir) of
                         true -> {true, SubDir};
                         false -> false
                       end
                   end, Files),
  NewAcc = ordsets:union(ordsets:from_list(SubDirs), Acc),
  all_subdirs(T++SubDirs, NewAcc);
all_subdirs([], Acc) ->
  Acc.

handle_add_rec_click(#gui_state{chosen_box=ChosenBox, file_box=File, 
				file_wd=FWD, mode=Mode}) ->
  case gs:read(File, selection) of
    [] ->
      ok;
    List ->
      gs:config(File, [{selection, clear}]),
      Dirs1 = [gs:read(File, {get, X}) || X <- List],
      Dirs2 = ordsets:from_list([filename:join(FWD, X) || X <- Dirs1]),
      Dirs3 = ordsets:filter(fun(X) -> filelib:is_dir(X) end, Dirs2),
      TargetDirs = ordsets:union(Dirs3, all_subdirs(Dirs3)),
      case gs:read(Mode#mode.start_byte_code, select) of
      true -> 
        Code = byte_code,
        Ext = ".beam";
      false ->
        Code = src_code,
        Ext = ".erl"
      end,
      add_files(filter_mods(TargetDirs, Ext), ChosenBox, Code)
  end.

handle_add_files(#gui_state{chosen_box=ChosenBox, file_box=File, 
			    file_wd=FWD, mode=Mode}) ->
  case gs:read(File, selection) of
    [] ->
      ok;
    List ->
      gs:config(File, [{selection, clear}]),
      Add0 = [gs:read(File, {get, X}) || X <- List],
      Add = ordsets:from_list([filename:join(FWD, X) || X <- Add0]),
      case gs:read(Mode#mode.start_byte_code, select) of
	true -> 
	  add_files(filter_mods(Add, ".beam"), ChosenBox, byte_code);
	false ->
	  add_files(filter_mods(Add, ".erl"), ChosenBox, src_code)
      end
  end.

filter_mods(Mods, Extension) ->
  Fun = fun(X) -> 
	    filename:extension(X) =:= Extension
	      orelse 
		(filelib:is_dir(X) andalso
		 contains_files(X, Extension))
	end,
  lists:filter(Fun, Mods).

contains_files(Dir, Extension) ->
  {ok, Files} = file:list_dir(Dir),
  lists:any(fun(X) -> filename:extension(X) =:= Extension end,Files).

add_files(Add, ChosenBox, Type) ->
  Set = gs:read(ChosenBox, items),
  Set1 = 
    case Type of
      byte_code -> filter_mods(Set, ".beam");
      src_code -> filter_mods(Set, ".erl")
    end,
  Files = ordsets:union(Add, Set1),
  gs:config(ChosenBox, [{items, Files}]),
  ok.

handle_file_delete(#gui_state{chosen_box=ChosenBox}) ->
  case gs:read(ChosenBox, selection) of
    [] ->
      ok;
    List ->
      [gs:config(ChosenBox, {del, X}) || X <- lists:reverse(lists:sort(List))],
      ok
  end.


%% ---- Other ----

change_dir_or_add_file(S = #gui_state{file_wd=FWD, mode=Mode}, 
		       Text) ->
  NewWDorFile =
    case Text of
      ".." -> filename:join(butlast(filename:split(FWD)));
      "." -> FWD;
      _ -> filename:join(FWD, Text)
    end,
  case filelib:is_dir(NewWDorFile) of
    true ->
      gs:config(S#gui_state.dir_entry, [{text, NewWDorFile}]),
      {ok, List} = file:list_dir(NewWDorFile),
      gs:config(S#gui_state.file_box, [{items, [".."|lists:sort(List)]}]),
      S#gui_state{file_wd=NewWDorFile};
    false ->
      case gs:read(Mode#mode.start_byte_code, select) of
	true -> 
	  case filter_mods([NewWDorFile], ".beam") of
	    [] -> ok;
	    RealFiles ->
	      add_files(RealFiles, S#gui_state.chosen_box, byte_code)
	  end;
	false -> 
	  case filter_mods([NewWDorFile], ".erl") of
	    [] -> ok;
	    RealFiles ->
	      add_files(RealFiles, S#gui_state.chosen_box, src_code)
	  end
      end,
      S
  end.

butlast([H1, H2 | T]) ->
  [H1 | butlast([H2|T])];
butlast([_]) ->
  [];
butlast([]) ->
  ["/"].

change_dir_absolute(S = #gui_state{file_wd=FWD, dir_entry=DirEntry,
				   file_box=File}, 
		    Text) ->
  case filelib:is_dir(Text) of
    true ->
      WD = filename:join(FWD, Text),
      gs:config(DirEntry, [{text, WD}]),
      {ok, List} = file:list_dir(WD),
      gs:config(File, [{items, [".."|lists:sort(List)]}]),
      S#gui_state{file_wd=WD};
    false ->
      S
  end.

init_warnings([{Tag, GSItem}|Left], LegalWarnings) ->
  Select = ordsets:is_element(Tag, LegalWarnings),
  gs:config(GSItem, [{select, Select}]),
  init_warnings(Left, LegalWarnings);
init_warnings([], _LegalWarnings) ->
  ok.
  

config_gui_start(State) ->
  gs:config(State#gui_state.stop, {enable, true}),
  gs:config(State#gui_state.run, {enable, false}),
  gs:config(State#gui_state.del_file, {enable, false}),
  gs:config(State#gui_state.clear_chosen, {enable, false}),
  gs:config(State#gui_state.add_file, {enable, false}),
  gs:config(State#gui_state.add_all, {enable, false}),
  gs:config(State#gui_state.add_rec, {enable, false}),
  gs:config(State#gui_state.clear_warn, {enable, false}),
  gs:config(State#gui_state.clear_log, {enable, false}),

  Menu = State#gui_state.menu,
  gs:config(Menu#menu.file_save_warn, {enable, false}),
  gs:config(Menu#menu.file_save_log, {enable, false}),
  gs:config(Menu#menu.opts_supress_inline, {enable, false}),
  gs:config(Menu#menu.opts_macros, {enable, false}),
  gs:config(Menu#menu.opts_includes, {enable, false}),
  gs:config(Menu#menu.plt_empty, {enable, false}),
  gs:config(Menu#menu.plt_search_doc, {enable, false}),
  gs:config(Menu#menu.plt_show_doc, {enable, false}),

  Mode = State#gui_state.mode,
  gs:config(Mode#mode.gran_all, {enable, false}),
  gs:config(Mode#mode.start_byte_code, {enable, false}),
  gs:config(Mode#mode.iter_fixpoint, {enable, false}),
  gs:config(Mode#mode.gran_module, {enable, false}),
  gs:config(Mode#mode.start_src_code, {enable, false}),
  gs:config(Mode#mode.iter_qad, {enable, false}),  
  gs:config(Mode#mode.dataflow, {enable, false}),
  gs:config(Mode#mode.succ_typings, {enable, false}).


config_gui_stop(State) ->
  gs:config(State#gui_state.stop, {enable, false}),
  gs:config(State#gui_state.run, {enable, true}),
  gs:config(State#gui_state.del_file, {enable, true}),
  gs:config(State#gui_state.clear_chosen, {enable, true}),
  gs:config(State#gui_state.add_file, {enable, true}),
  gs:config(State#gui_state.add_all, {enable, true}),
  gs:config(State#gui_state.add_rec, {enable, true}),
  gs:config(State#gui_state.clear_warn, {enable, true}),
  gs:config(State#gui_state.clear_log, {enable, true}),

  Menu = State#gui_state.menu,
  gs:config(Menu#menu.file_save_warn, {enable, true}),
  gs:config(Menu#menu.file_save_log, {enable, true}),
  gs:config(Menu#menu.opts_supress_inline, {enable, true}),
  gs:config(Menu#menu.opts_macros, {enable, true}),
  gs:config(Menu#menu.opts_includes, {enable, true}),
  gs:config(Menu#menu.plt_empty, {enable, true}),
  gs:config(Menu#menu.plt_search_doc, {enable, true}),
  gs:config(Menu#menu.plt_show_doc, {enable, true}),

  Mode = State#gui_state.mode,
  gs:config(Mode#mode.gran_all, {enable, true}),
  gs:config(Mode#mode.start_byte_code, {enable, true}),
  gs:config(Mode#mode.iter_fixpoint, {enable, true}),
  gs:config(Mode#mode.gran_module, {enable, true}),
  gs:config(Mode#mode.start_src_code, {enable, true}),
  gs:config(Mode#mode.iter_qad, {enable, true}),
  gs:config(Mode#mode.dataflow, {enable, true}),
  gs:config(Mode#mode.succ_typings, {enable, true}).



%% ----------------------------------------------------------------
%%
%%  Messages
%%

message(State, Message) ->
  message(State, "Dialyzer Message", Message).  

error(State, Message) ->
  message(State, "Dialyzer Error", Message).

message(#gui_state{gs=GS, top=TopWin}, Title, Message) ->
  WH = [{width, 400}, {height, 100}],

  MessageWin = gs:window(GS, [{title, Title},
			      {default, button, {font, {helvetica, bold, 12}}}
			      |WH]),

  MessagePacker = gs:frame(MessageWin, [{packer_y, [{fixed, 75}, {fixed, 25}]},
					{packer_x, [{fixed, 175},{fixed, 50},
						    {fixed, 175}]}]),
  gs:label(MessagePacker, [{pack_x, {1,3}}, {pack_y, 1}, 
			   {label, {text, Message}}]),
  Ok = gs:button(MessagePacker, [{label, {text, "Ok"}}, 
				 {pack_xy, {2,2}}]),
  gs:config(MessageWin, {map, true}),
  gs:config(MessagePacker, WH),
  message_loop(Ok, MessageWin, TopWin).

message_loop(Ok, Win, TopWin) ->
  receive
    {gs, Ok, click, _, _} ->
      gs:destroy(Win);
    {gs, Win, destroy, _, _} ->
      ok;
    {gs, TopWin, destroy, _, _} ->
      exit(normal);
    {gs, _, _, _, _} ->
      message_loop(Ok, Win, TopWin)
  end.

dialog(#gui_state{gs=GS, top=TopWin}, Message, OkLabel, CancelLabel) ->
  WH = [{width, 400}, {height, 100}],
  WHButton = [{width, 70}, {height, 20}],

  DialogWin = gs:window(GS, [{title, "Dialyzer Message"},
			     {default, button, {font, {helvetica, bold, 12}}}
			     |WH]),
  DialogPacker = gs:frame(DialogWin, [{packer_y, [{fixed, 75}, {fixed, 25}]},
				      {packer_x, [{fixed, 150},{fixed, 50},
						  {fixed, 50},{fixed, 150}]}]),
  gs:label(DialogPacker, [{pack_x, {1,4}}, {pack_y, 1}, 
			  {label, {text, Message}}]),
  Ok = gs:button(DialogPacker, [{label, {text, OkLabel}}, 
				{pack_xy, {2,2}}|WHButton]),
  Cancel = gs:button(DialogPacker, [{label, {text, CancelLabel}}, 
				    {pack_xy, {3,2}}|WHButton]),

  gs:config(DialogWin, {map, true}),
  gs:config(DialogPacker, WH),
  dialog_loop(Ok, Cancel, DialogWin, TopWin).

dialog_loop(Ok, Cancel, Win, TopWin) ->
  receive
    {gs, Ok, click, _, _} ->
      gs:destroy(Win),
      true;
    {gs, Cancel, click, _, _} ->
      gs:destroy(Win),
      false;
    {gs, Win, destroy, _, _} ->
      false;
    {gs, TopWin, destroy, _, _} ->
      exit(normal);
    {gs, _, _, _, _} ->
      dialog_loop(Ok, Cancel, Win, TopWin)
  end.

maybe_quit(State=#gui_state{top=TopWin}) ->
  case dialog(State, "Do you really want to quit?", "Yes", "No") of
    true ->
      flush(),
      gs:destroy(TopWin),
      gs:stop(),
      true;
    false ->
      false
  end.


%% ----------------------------------------------------------------
%%
%%  Menu actions
%%

%% ---- Help Menu ----

overview(State) ->
  GS = State#gui_state.gs,
  WH = [{width, 600}, {height, 360}],
  Win = gs:window(GS, [{title, "Dialyzer Overview"}, {configure, true},
		       {default, editor, {bg, white}} | WH]),
  Frame = gs:frame(Win, [{packer_x, [{stretch, 1}, {fixed, 60}, {stretch, 1}]}, 
			 {packer_y, [{stretch, 1}, {fixed, 30}]}
			 | WH]),
  Editor = gs:editor(Frame, [{pack_x, {1,3}}, {pack_y, 1},
			     {font, {courier, 12}}, {vscroll, right},
			     {wrap, word}]),
  Button = gs:button(Frame, [{label, {text, "Ok"}}, {pack_xy, {2,2}}]),
  gs:config(Win, {map, true}),
  gs:config(Frame, WH),

  AboutFile = filename:join([code:lib_dir(dialyzer), "doc", "overview.txt"]),
  case gs:config(Editor, {load, AboutFile}) of
    {error, Reason} ->
      gs:destroy(Win),
      error(State, 
	    io_lib:format("Could not find doc/overview.txt file!\n\n ~p", 
			  [Reason]));
    ok ->
      gs:config(Editor, {enable, false}),
      TopWin = State#gui_state.top,
      show_info_loop(TopWin, Win, Frame, Button)
  end.

manual(State) ->
  GS = State#gui_state.gs,
  WH = [{width, 600}, {height, 500}],
  Win = gs:window(GS, [{title, "Dialyzer Manual"}, {configure, true},
		       {default, editor, {bg, white}} | WH]),
  Frame = gs:frame(Win, [{packer_x, [{stretch, 1}, {fixed, 60}, {stretch, 1}]}, 
			 {packer_y, [{stretch, 1}, {fixed, 30}]}
			 | WH]),
  Editor = gs:editor(Frame, [{pack_x, {1,3}}, {pack_y, 1},
			     {font, {courier, 12}}, {vscroll, right},
			     {wrap, word}]),
  Button = gs:button(Frame, [{label, {text, "Ok"}}, {pack_xy, {2,2}}]),
  gs:config(Win, {map, true}),
  gs:config(Frame, WH),

  AboutFile = filename:join([code:lib_dir(dialyzer), "doc", "manual.txt"]),
  case gs:config(Editor, {load, AboutFile}) of
    {error, Reason} ->
      gs:destroy(Win),
      error(State, 
	    io_lib:format("Could not find doc/manual.txt file!\n\n ~p", 
			  [Reason]));
    ok ->
      gs:config(Editor, {enable, false}),
      TopWin = State#gui_state.top,
      show_info_loop(TopWin, Win, Frame, Button)
  end.

help_warnings(State) ->
  GS = State#gui_state.gs,
  WH = [{width, 600}, {height, 500}],
  Win = gs:window(GS, [{title, "Dialyzer Warnings"}, {configure, true},
		       {default, editor, {bg, white}} | WH]),
  Frame = gs:frame(Win, [{packer_x, [{stretch, 1}, {fixed, 60}, {stretch, 1}]}, 
			 {packer_y, [{stretch, 1}, {fixed, 30}]}
			 | WH]),
  Editor = gs:editor(Frame, [{pack_x, {1,3}}, {pack_y, 1},
			     {font, {courier, 12}}, {vscroll, right},
			     {wrap, word}]),
  Button = gs:button(Frame, [{label, {text, "Ok"}}, {pack_xy, {2,2}}]),
  gs:config(Win, {map, true}),
  gs:config(Frame, WH),

  AboutFile = filename:join([code:lib_dir(dialyzer), "doc", "warnings.txt"]),
  case gs:config(Editor, {load, AboutFile}) of
    {error, Reason} ->
      gs:destroy(Win),
      error(State, 
	    io_lib:format("Could not find doc/warnings.txt file!\n\n ~p", 
			  [Reason]));
    ok ->
      gs:config(Editor, {enable, false}),
      TopWin = State#gui_state.top,
      show_info_loop(TopWin, Win, Frame, Button)
  end.

about(State) ->
  GS = State#gui_state.gs,
  WH = [{width, 600}, {height, 160}],
  Win = gs:window(GS, [{title, "About Dialyzer"}, {configure, true},
		       {default, editor, {bg, yellow}} | WH]),
  Frame = gs:frame(Win, [{packer_x, [{stretch, 1}, {fixed, 60}, {stretch, 1}]}, 
			 {packer_y, [{stretch, 1}, {fixed, 30}]}
			 | WH]),
  Editor = gs:editor(Frame, [{pack_x, {1,3}}, {pack_y, 1},
			     {font, {courier, 12}}, {vscroll, right},
			     {wrap, word}]),
  Button = gs:button(Frame, [{label, {text, "Ok"}}, {pack_xy, {2,2}}]),
  gs:config(Win, {map, true}),
  gs:config(Frame, WH),

  AboutFile = filename:join([code:lib_dir(dialyzer), "doc", "about.txt"]),
  case gs:config(Editor, {load, AboutFile}) of
    {error, Reason} ->
      gs:destroy(Win),
      error(State, 
	    io_lib:format("Could not find doc/about.txt file!\n\n ~p", 
			  [Reason]));
    ok ->
      gs:config(Editor, {enable, false}),
      TopWin = State#gui_state.top,
      show_info_loop(TopWin, Win, Frame, Button)
  end.

%% ---- File Menu ----

save_log(State) ->
  Log = State#gui_state.log,
  CWD = State#gui_state.file_wd,
  {Win, Entry, OkButton, CancelButton} = file_box(State, "Save Log", CWD),
  save_loop(State, OkButton, CancelButton, Entry, Win, Log).

save_warn(State) ->
  Warn = State#gui_state.warnings_box,
  CWD = State#gui_state.file_wd,
  {Win, Entry, OkButton, CancelButton} =
    file_box(State, "Save Warnings", CWD),
  save_loop(State, OkButton, CancelButton, Entry, Win, Warn).  

file_box(#gui_state{gs=GS}, Title, Default) ->
  WH = [{width, 400}, {height, 75}],
  Win = gs:window(GS, [{title, Title}|WH]),
  WinPacker = gs:frame(Win, [{packer_y, [{fixed, 25}, {fixed, 25},
					 {fixed, 25}]},
			     {packer_x, [{fixed, 75},{fixed, 75},
					 {fixed, 75},
					 {fixed, 175}]}]),
  gs:label(WinPacker, [{pack_xy, {1,2}},
		       {label, {text, "Enter file:"}}]),
  Entry = gs:entry(WinPacker, [{pack_x, {2,4}}, 
			       {pack_y, 2}, {keypress, true}]),
  OkButton = gs:button(WinPacker, [{label, {text, "Ok"}}, 
				   {pack_xy, {2,3}}]),
  CancelButton = gs:button(WinPacker, [{label, {text, "Cancel"}}, 
				       {pack_xy, {3,3}}]),
  gs:config(Entry, {text, Default}),
  gs:config(Win, {map, true}),
  gs:config(WinPacker, WH),
  {Win, Entry, OkButton, CancelButton}.

save_loop(State, OkButton, CancelButton, Entry, Save, Editor) ->
  TopWin = State#gui_state.top,
  receive
    {gs, OkButton, click, _, _} ->
      File = gs:read(Entry, text),
      case gs:config(Editor, {save, File}) of
	{error, _} ->
	  error(State, "Could not write to file "++File),
	  save_loop(State, OkButton, CancelButton, Entry, Save, Editor);
	_ ->
	  gs:destroy(Save)
      end;
    {gs, Entry, keypress, _, ['Return'|_]} ->
      File = gs:read(Entry, text),
      case gs:config(Editor, {save, File}) of
	{error, _} ->
	  error(State, "Could not write to file "++File),
	  save_loop(State, OkButton, CancelButton, Entry, Save, Editor);
	_ ->
	  gs:destroy(Save)
      end;
    {gs, Entry, keypress, _, _} ->
      save_loop(State, OkButton, CancelButton, Entry, Save, Editor);
    {gs, CancelButton, click, _, _} ->
      gs:destroy(Save);
    {gs, TopWin, destroy, _, _} ->
      exit(normal);
    {gs, Save, destroy, _, _} ->
      ok;
    {gs, _, _, _, _} ->
      save_loop(State, OkButton, CancelButton, Entry, Save, Editor)
  end.

%% ---- Plt Menu ----

search_doc_plt(State) ->
  case State#gui_state.doc_plt of
    undefined -> error(State, "No analysis has been made yet!\n");
    PLT ->
      search_doc_plt(State, PLT)
  end.

search_doc_plt(State, PLT) ->
  GS = State#gui_state.gs,
  TopWin = State#gui_state.top,
  WH = [{width, 400}, {height, 100}],
  WHB = [{width, 120}, {height, 30}],
  Title = io_lib:format("Search the PLT", []),
  Win = gs:window(GS, [{title, Title}, {configure, true},
		       {default, editor, {bg, white}} | WH]),
  Frame = gs:frame(Win, [{packer_x, [{stretch, 1}, {stretch, 1}, {stretch, 1}]},
			 {packer_y, [{fixed, 30}, {fixed, 30}, 
				     {stretch, 1}, {fixed, 30}]}
			 | WH]),
  gs:label(Frame, [{pack_xy, {1,1}}, {label, {text, "Module"}}]),
  ModEntry = gs:entry(Frame, [{pack_xy, {1,2}}]),
  gs:label(Frame, [{pack_xy, {2,1}}, {label, {text, "Function"}}]),
  FunEntry = gs:entry(Frame, [{pack_xy, {2,2}}]),
  gs:label(Frame, [{pack_xy, {3,1}}, {label, {text, "Arity"}}]),
  ArityEntry = gs:entry(Frame, [{pack_xy, {3,2}}]),

  ButtonPacker = gs:frame(Frame, [{pack_xy, {2,4}}, 
				  {packer_x, [{fixed, 60}, {fixed, 60}]},
				  {packer_y, {fixed, 30}}]),
  SearchButton = gs:button(ButtonPacker, [{label, {text, "Search"}}, 
					  {pack_xy, {1,1}}]),
  CancelButton = gs:button(ButtonPacker, [{label, {text, "Cancel"}}, 
					  {pack_xy, {2,1}}]),

  gs:config(Win, {map, true}),
  gs:config(Frame, WH),
  gs:config(ButtonPacker, WHB),
  search_doc_plt_loop(State, CancelButton, SearchButton, ModEntry, 
		      FunEntry, ArityEntry, PLT, Win, TopWin).


search_doc_plt_loop(State, CancelButton, SearchButton, ModEntry, 
		    FunEntry, ArityEntry, PLT, Win, TopWin) ->
  receive
    {gs, CancelButton, click, _, _} ->
      gs:destroy(Win),
      ok;
    {gs, TopWin, destroy, _, _} ->
      exit(normal);
    {gs, SearchButton, click, _, _} ->
      M = format_search(gs:read(ModEntry, text)),
      F = format_search(gs:read(FunEntry, text)),
      A = format_search(gs:read(ArityEntry, text)),
      case dialyzer_plt:to_edoc(PLT, M, F, A) of
	[] -> 
	  error(State, "No such function"),
	  search_doc_plt_loop(State, CancelButton, SearchButton, ModEntry, 
			      FunEntry, ArityEntry, PLT, Win, TopWin);
	String ->
	  gs:destroy(Win),
	  free_editor(State, String, "Content of PLT")
      end
  end.

format_search([]) ->
  '_';
format_search(String) ->
  case catch list_to_integer(String) of
    {'EXIT', _} -> list_to_atom(String);
    Int -> Int
  end.

show_doc_plt(State) ->
  case State#gui_state.doc_plt of
    undefined -> error(State, "No analysis has been made yet!\n");
    PLT ->
      String = dialyzer_plt:to_edoc(PLT),
      free_editor(State, String, "Content of PLT")
  end.

free_editor(State, Contents0, Title) ->
  Contents = lists:flatten(Contents0),
  Tokens = string:tokens(Contents, "\n"),
  NofLines = length(Tokens),
  LongestLine = lists:max([length(X)||X<-Tokens]),

  Height0 = NofLines * 25 + 80,
  if Height0 > 500 -> Height = 500;
     true -> Height = Height0
  end,

  Width0 = LongestLine * 7 + 60,
  if Width0 > 800 -> Width = 800;
     true -> Width = Width0
  end,

  GS = State#gui_state.gs,
  WH = [{width, Width}, {height, Height}],
  Win = gs:window(GS, [{title, Title}, {configure, true},
		       {default, editor, {bg, white}} | WH]),
  Frame = gs:frame(Win, [{packer_x, [{stretch, 1}, {fixed, 60}, {stretch, 1}]}, 
			 {packer_y, [{stretch, 1}, {fixed, 30}]}
			 | WH]),
  Editor = gs:editor(Frame, [{pack_x, {1,3}}, {pack_y, 1},
			     {font, {courier, 12}}, {vscroll, right},
			     {wrap, word}, {enable, true}]),
  Button = gs:button(Frame, [{label, {text, "Ok"}}, {pack_xy, {2,2}}]),

  gs:config(Editor, {insert, {insert, Contents}}),
  gs:config(Editor, {enable, false}),
  gs:config(Win, {map, true}),
  gs:config(Frame, WH),
  TopWin = State#gui_state.top,
  show_info_loop(TopWin, Win, Frame, Button).

%% ---- Common ----

show_info_loop(TopWin, Win, Frame, Button) ->
  receive
    {gs, Button, click, _, _} ->
      gs:destroy(Win);
    {gs, TopWin, destroy, _, _} ->
      exit(normal);
    {gs, Win, destroy, _, _} ->
      ok;
    {gs, Win, configure, _Data, [W, H|_]} ->
      gs:config(Frame, [{width, W}, {height, H}]),
      show_info_loop(TopWin, Win, Frame, Button)
  end.

include_dialog(State, Parent) ->
  GS = State#gui_state.gs,
  WH = [{width, 300}, {height, 400}],
  Title = io_lib:format("Include Directories", []),
  Win = gs:window(GS, [{title, Title}, {configure, true},
		       {default, entry, {bg, white}}| WH]),
  Frame = gs:frame(Win, [{packer_x, [{stretch, 1}]},
			 {packer_y, [{fixed, 30}, {fixed, 30}, 
				     {fixed, 30}, %Empty space
				     {stretch, 1}, {fixed, 30},{fixed, 30}]}
			 | WH]),
  gs:label(Frame, [{pack_xy, {1,1}}, {label, {text, "Directory"}}]),
  DirEntry = gs:entry(Frame, [{pack_xy, {1,2}}]),
  ButtonPacker1 = gs:frame(Frame, [{pack_xy, {1,3}},
				   {packer_x, [{fixed, 70},{fixed, 70},
					       {stretch,1}]},
				   {packer_y, {fixed, 30}}]),
  AddButton = gs:button(ButtonPacker1, [{label, {text, "Add"}}, 
					{pack_xy, {1,1}}]),
  
  Options = State#gui_state.options,
  Dirs = [io_lib:format("~s", [X])||X<-Options#options.include_dirs],
  DirBox = gs:listbox(Frame, [{pack_xy, {1,4}}, {vscroll, right},
				{bg, white}, {configure, true},
				{selectmode, multiple},
				{items, Dirs}]),
  
  ButtonPacker2 = gs:frame(Frame, [{pack_xy, {1,5}},
				   {packer_x, [{fixed, 60},{fixed, 70},
					       {stretch,1}]},%Empty space
				   {packer_y, {fixed, 30}}]),
  
  DeleteButton = gs:button(ButtonPacker2, [{label, {text, "Delete"}}, 
					   {pack_xy, {1,1}}]),
  DeleteAllButton = gs:button(ButtonPacker2, [{label, {text, "Delete All"}}, 
					      {pack_xy, {2,1}}]),
  ButtonPacker3 = gs:frame(Frame, [{pack_xy, {1,6}},
				   {packer_x, [{stretch,1},%Empty space
					       {fixed, 60},{fixed, 60}]},
				   {packer_y, {fixed, 30}}]),

  OkButton = gs:button(ButtonPacker3, [{label, {text, "Ok"}},
				       {pack_xy, {2,1}}]),
  CancelButton = gs:button(ButtonPacker3, [{label, {text, "Cancel"}},
					   {pack_xy, {3,1}}]),

  gs:config(Win, {map, true}),
  gs:config(Frame, WH),

  include_loop(Parent, Options, Frame, AddButton, DeleteAllButton, DeleteButton,
	       DirBox, DirEntry, OkButton, CancelButton, Win).

include_loop(Parent, Options, Frame, AddButton, DeleteAllButton, DeleteButton, 
	   DirBox, DirEntry, OkButton, CancelButton, Win) ->
  receive
    {gs, CancelButton, click, _, _} ->
      gs:destroy(Win),
      ok;
    {gs, OkButton, click, _, _} ->
      gs:destroy(Win),
      Parent ! {new_options, Options},
      ok;
    {gs, Win, configure, _Data, [W, H|_]} ->
      gs:config(Frame, [{width, W}, {height, H}]),
      include_loop(Parent, Options, Frame, AddButton, DeleteAllButton, 
		   DeleteButton, DirBox, DirEntry, OkButton, CancelButton, Win);
    {gs, AddButton, click, _, _} ->
      Dirs = Options#options.include_dirs,
      NewDirs =
	case gs:read(DirEntry, text) of
	  [] -> Dirs;
	  Add -> [Add|Dirs]
	end,
      NewOptions = Options#options{include_dirs=NewDirs},
      gs:config(DirBox, [{items, NewDirs}]),
      include_loop(Parent, NewOptions, Frame, AddButton, DeleteAllButton, 
		   DeleteButton, DirBox, DirEntry, OkButton, CancelButton, Win);
    {gs, DeleteAllButton, click, _, _} ->
      gs:config(DirBox, clear),
      NewOptions = Options#options{include_dirs=[]},      
      include_loop(Parent, NewOptions, Frame, AddButton, DeleteAllButton, 
		   DeleteButton, DirBox, DirEntry, OkButton, CancelButton, Win);
    {gs, DeleteButton, click, _, _} ->
      NewOptions =
	case gs:read(DirBox, selection) of
	  [] ->
	    Options;
	  List ->
	    [gs:config(DirBox, [{del, X}])||
	      X <- lists:reverse(lists:sort(List))],
	    NewDirs = gs:read(DirBox, items),
	    Options#options{include_dirs=NewDirs}
	end,
      include_loop(Parent, NewOptions, Frame, AddButton, DeleteAllButton, 
		   DeleteButton, DirBox, DirEntry, OkButton, CancelButton, Win);
    {gs, Win, destroy, _, _} ->
      ok
  end.
  
macro_dialog(State, Parent) ->
  GS = State#gui_state.gs,
  WH = [{width, 300}, {height, 400}],
  Title = io_lib:format("Macro Definitions", []),
  Win = gs:window(GS, [{title, Title}, {configure, true},
		       {default, entry, {bg, white}}| WH]),
  Frame = gs:frame(Win, [{packer_x, [{stretch, 1}, {stretch, 1}]},
			 {packer_y, [{fixed, 30}, {fixed, 30}, 
				     {fixed, 30}, %Empty space
				     {stretch, 1}, {fixed, 30},{fixed, 30}]}
			 | WH]),
  gs:label(Frame, [{pack_xy, {1,1}}, {label, {text, "Macro"}}]),
  MacroEntry = gs:entry(Frame, [{pack_xy, {1,2}}]),
  gs:label(Frame, [{pack_xy, {2,1}}, {label, {text, "Term"}}]),
  TermEntry = gs:entry(Frame, [{pack_xy, {2,2}}]),
  ButtonPacker1 = gs:frame(Frame, [{pack_x, {1,2}}, {pack_y, 3}, 
				   {packer_x, [{fixed, 70},{fixed, 70},
					       {stretch,1}]},
				   {packer_y, {fixed, 30}}]),
  AddButton = gs:button(ButtonPacker1, [{label, {text, "Add"}}, 
					{pack_xy, {1,1}}]),
  
  Options = State#gui_state.options,
  Macros = [io_lib:format("~p = ~p", [X,Y])||{X,Y}<-Options#options.defines],
  MacroBox = gs:listbox(Frame, [{pack_x, {1,2}}, {pack_y, 4}, {vscroll, right},
				{bg, white}, {configure, true},
				{selectmode, multiple},
				{items, Macros}]),
  
  ButtonPacker2 = gs:frame(Frame, [{pack_x, {1,2}}, {pack_y, 5}, 
				   {packer_x, [{fixed, 60},{fixed, 70},
					       {stretch,1}]},%Empty space
				   {packer_y, {fixed, 30}}]),
  
  DeleteButton = gs:button(ButtonPacker2, [{label, {text, "Delete"}}, 
					   {pack_xy, {1,1}}]),
  DeleteAllButton = gs:button(ButtonPacker2, [{label, {text, "Delete All"}}, 
					      {pack_xy, {2,1}}]),
  ButtonPacker3 = gs:frame(Frame, [{pack_x, {1,2}}, {pack_y, 6}, 
				   {packer_x, [{stretch,1},%Empty space
					       {fixed, 60},{fixed, 60}]},
				   {packer_y, {fixed, 30}}]),

  OkButton = gs:button(ButtonPacker3, [{label, {text, "Ok"}},
				       {pack_xy, {2,1}}]),
  CancelButton = gs:button(ButtonPacker3, [{label, {text, "Cancel"}},
					   {pack_xy, {3,1}}]),

  gs:config(Win, {map, true}),
  gs:config(Frame, WH),

  macro_loop(Parent, Options, Frame, AddButton, DeleteAllButton, DeleteButton, 
	     MacroBox, MacroEntry, TermEntry, OkButton, CancelButton, Win).
  
macro_loop(Parent, Options, Frame, AddButton, DeleteAllButton, DeleteButton, 
	   MacroBox, MacroEntry, TermEntry, OkButton, CancelButton, Win) ->
  receive
    {gs, CancelButton, click, _, _} ->
      gs:destroy(Win),
      ok;
    {gs, OkButton, click, _, _} ->
      gs:destroy(Win),
      Parent ! {new_options, Options},
      ok;
    {gs, Win, configure, _Data, [W, H|_]} ->
      gs:config(Frame, [{width, W}, {height, H}]),
      macro_loop(Parent, Options, Frame, AddButton, DeleteAllButton, 
		 DeleteButton, MacroBox, MacroEntry, TermEntry, OkButton, 
		 CancelButton, Win);
    {gs, AddButton, click, _, _} ->
      Defines = Options#options.defines,
      NewDefines = 
	case gs:read(MacroEntry, text) of
	  "" -> Defines;
	  Macro ->
	    case gs:read(TermEntry, text) of
	      "" -> 
		gs:config(MacroEntry, {text, ""}),
		orddict:store(list_to_atom(Macro), true, Defines);
	      String ->
		case parse(String) of
		  {ok, Term} ->
		    gs:config(MacroEntry, [{text, ""}]),
		    gs:config(TermEntry, [{text, ""}]),
		    orddict:store(list_to_atom(Macro), Term, Defines);
		  {error, _Reason} ->
		    Defines
		end
	    end
	end,
      NewOptions = Options#options{defines=NewDefines},		 
      NewEntries = [io_lib:format("~p = ~p", [X, Y])||{X, Y} <- NewDefines],
      gs:config(MacroBox, [{items, NewEntries}]),
      macro_loop(Parent, NewOptions, Frame, AddButton, DeleteAllButton, 
		 DeleteButton, MacroBox, MacroEntry, TermEntry, OkButton, 
		 CancelButton, Win);
    {gs, DeleteAllButton, click, _, _} ->
      gs:config(MacroBox, [clear]),
      NewOptions = Options#options{defines=[]},
      macro_loop(Parent, NewOptions, Frame, AddButton, DeleteAllButton, 
		 DeleteButton, MacroBox, MacroEntry, TermEntry, OkButton, 
		 CancelButton, Win);
    {gs, DeleteButton, click, _, _} ->
      NewOptions =
	case gs:read(MacroBox, selection) of
	  [] ->
	    Options;
	  List ->
	    gs:config(MacroBox, [{selection, clear}]),
	    Fun = 
	      fun(X) ->
		  {ok, [Macro|_]} =
		    regexp:split(gs:read(MacroBox, {get, X}), " "),
		  list_to_atom(Macro)
	      end,
	    Delete = [Fun(X) || X <- List],
	    [gs:config(MacroBox, [{del, X}])||
	      X <- lists:reverse(lists:sort(List))],
	    Defines = Options#options.defines,
	    NewDefines = lists:foldl(fun(X, Acc) -> 
					 orddict:erase(X, Acc)
				     end,
				     Defines, Delete),
	    Options#options{defines=NewDefines}
	end,
      macro_loop(Parent, NewOptions, Frame, AddButton, DeleteAllButton, 
		 DeleteButton, MacroBox, MacroEntry, TermEntry, OkButton, 
		 CancelButton, Win);
    {gs, Win, destroy, _, _} ->
      ok
  end.

parse(String) ->
  case erl_scan:string(String ++ ".", 1) of
    {ok, Ts, _} ->
      case erl_parse:parse_exprs(Ts) of
	{ok, [Expr]} ->	  
	  case catch erl_parse:normalise(Expr) of
	    {'EXIT', Reason} -> {error, Reason};
	    Term -> {ok, Term}
	  end;
	{error, E} ->
	  parse_error(E)
      end;
    {error, E, _} ->
      parse_error(E)
  end.

parse_error(E) ->
  S = io_lib:fwrite("Error parsing expression: ~P.",[E,15]),
  {error, S}.

%% ----------------------------------------------------------------
%%
%%  Run the analysis
%%

start_analysis(State) ->
  Analysis = build_analysis_record(State),
  case get_anal_files(State, Analysis#analysis.start_from) of
    error ->
      Msg = "You must choose one or more files or dirs\n"
	"before starting the analysis!",
      error(State, Msg),
      config_gui_stop(State),      
      State;
    {ok, Files} ->
      Msg = "\n========== Starting Analysis ==========\n\n",
      update_editor(State#gui_state.log, Msg),
      NewAnalysis = Analysis#analysis{files=Files},
      run_analysis(State, NewAnalysis)
  end.

build_analysis_record(#gui_state{mode=Mode, menu=Menu, options=Options,
				 init_plt=InitPlt0, empty_plt=EmptyPlt}) ->
  Fixpoint = first,
%%    case gs:read(Mode#mode.iter_fixpoint, select) of
%%      true -> first;
%%      false -> false
%%    end,
  Granularity = all,
%%    case gs:read(Mode#mode.gran_all, select) of
%%      true -> all;
%%      false -> module
%%    end,
  StartFrom =
    case gs:read(Mode#mode.start_byte_code, select) of
      true -> byte_code;
      false -> src_code
    end,
  CheckInline =
    case StartFrom of
      byte_code -> gs:read(Menu#menu.opts_supress_inline, select);
      src_code -> false
    end,
  InitPlt =
    case gs:read(Menu#menu.plt_empty, select) of
      true -> EmptyPlt;
      false -> InitPlt0
    end,
  CoreTransform = Options#options.core_transform,
%%    case gs:read(Mode#mode.dataflow, select) of
%%      true -> dataflow;
%%      false -> succ_typings
%%    end,
  
  #analysis{supress_inline=CheckInline,
	    core_transform=CoreTransform,
	    defines=Options#options.defines,
	    fixpoint=Fixpoint, 
	    granularity=Granularity,
	    include_dirs=Options#options.include_dirs,
	    init_plt=InitPlt,
	    start_from=StartFrom}.
	    

get_anal_files(#gui_state{chosen_box=ChosenBox}, StartFrom) ->
  Files = gs:read(ChosenBox, items),
  FilteredMods =
    case StartFrom of
      src_code -> filter_mods(Files, ".erl");
      byte_code -> filter_mods(Files, ".beam")
    end,
  FilteredDirs = [X || X <- Files, filelib:is_dir(X)],
  case FilteredMods ++ FilteredDirs of
    [] -> error;
    Set -> {ok, Set}
  end.

run_analysis(State, Analysis) ->
  config_gui_start(State),
  Self = self(),
  DocEts = 
    case State#gui_state.doc_plt of
      undefined -> ets:new(fun_info, [set, public, {keypos, 1}]);
      Ets -> 
	ets:delete_all_objects(Ets),
	Ets
    end,
  UserPlt = dialyzer_plt:new(dialyzer_user_plt),
  NewAnalysis = Analysis#analysis{doc_plt=DocEts, user_plt=UserPlt},
  LegalWarnings = find_legal_warnings(State),
  Fun = 
    fun() -> 
	dialyzer_analysis_callgraph:start(Self, LegalWarnings, NewAnalysis)
    end,
  BackendPid = spawn_link(Fun),
  State#gui_state{backend_pid=BackendPid,doc_plt=DocEts,user_plt=UserPlt}.


find_legal_warnings(#gui_state{menu=#menu{warnings=Warnings}}) ->
  [Tag || {Tag, GSItem} <- Warnings, gs:read(GSItem, select) =:= true].


flush() ->
  receive
    _ -> flush()
  after 
    0 -> ok
  end.

update_editor(Editor, Msg) ->
  gs:config(Editor, {enable, true}),
  NofRows = gs:read(Editor, size),
  gs:config(Editor, {insertpos, 'end'}),
  gs:config(Editor, {insert, {insert, Msg}}),
  NewNofRows = gs:read(Editor, size),
  ScrollPos = gs:read(Editor, vscrollpos),
  gs:config(Editor, {vscrollpos, ScrollPos + NewNofRows - NofRows}),
  gs:config(Editor, {enable, false}).

