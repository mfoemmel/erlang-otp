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
-module(dbg_ui_compilerdefs).



-export([start/4, init/4, get_opts/2, get_complete_opts/2]).



-define(BG, {217,217,217}).
-define(WIN_WIDTH, 478).
-define(WIN_HEIGHT, 292).





get_opts(Tab, Dir) ->
    AbsDir = filename:absname(Dir, Dir),
    get_prev_inserted(Tab, {incdir,all}) ++
	get_prev_inserted(Tab, {incdir,AbsDir}) ++
	get_prev_inserted(Tab, {macro,all}) ++
	get_prev_inserted(Tab, {macro,AbsDir}).



get_complete_opts(Tab, Dir) ->
    AbsDir = filename:absname(Dir, Dir),
    IncDir1 = get_prev_inserted(Tab, {incdir,all}),
    IncDir2 = get_prev_inserted(Tab, {incdir,AbsDir}),
    Macro1 = get_prev_inserted(Tab, {macro,all}),
    Macro2 = get_prev_inserted(Tab, {macro,AbsDir}),
    
    [{{incdir, all}, IncDir1}, {{incdir, AbsDir}, IncDir2},
     {{macro, all}, Macro1}, {{macro, AbsDir}, Macro2}].




%% Mode is one of 'macro' or 'incdir'
start(Mode, Table, Dir, undefined) ->
    Pid = spawn_link(?MODULE, init, [self(), Mode, Table, Dir]);
%    receive_loop(Pid);
start(Mode, Table, Dir, OldPid) ->
    OldPid ! raise,
    OldPid.

	    





init(Parent, Mode, Table, Dir) ->
    init_window(Mode, Table, Dir),
    update_listbox(Mode, Table, get_key(Mode, dir, Dir)),
    loop(Parent, Mode, Table, Dir, dir).








init_window(Mode, Table, Dir) ->
    WinFrameWidth  = ?WIN_WIDTH,
    WinFrameHeight = ?WIN_HEIGHT,
    FrameWidth     = WinFrameWidth - 115,
    FrameHeight    = 212,
    FrameX         = 10,
    FrameY         = 70,
    ListboxWidth   = FrameWidth - 23,
    BtnFrameWidth  = WinFrameWidth - FrameWidth - FrameX,
    BtnFrameHeight = FrameHeight,
    BtnFrameX      = FrameWidth + FrameX,
    BtnFrameY      = FrameY,
    BtnWidth       = 80,
    BtnHeight      = 30,
    BtnX           = 14,
    BtnY0          = 94,
    BtnDY          = 40,

    {WinTitle, LabelText, EntryLabelText, Width} =
	case Mode of
	    incdir ->
		{"Include Directories",
		 "Settings valid in:",
		 "Include directory:",
		 ListboxWidth
		};
	    macro ->
		{"Macro Definitions",
		 "Macros valid in:",
		 "Macro:",
		 (FrameWidth - 33) / 2
		}
	end,

    gs:window(win, gs:start([{kernel,true}]), [{width,?WIN_WIDTH},
				{height,?WIN_HEIGHT},
				{title,WinTitle},
				{bg,?BG},
				{destroy,true},
				{configure,true}
			       ]),
    gs:frame(winframe, win, [{width,WinFrameWidth},
			     {height,WinFrameHeight},
			     {x,0},
			     {y,0},
			     {bg,?BG}
			    ]),
    gs:label(winframe, [{width,125},
			{height,20},
			{x,10},
			{y,10},
			{bg,?BG},
			{fg,black},
			{label, {text,LabelText}},
			{align,w}
		       ]),
    gs:radiobutton(winframe, [{width,145},
			      {height,20},
			      {x,145},
			      {y,10},
			      {group,Mode},
			      {data,dir},
			      {bg,?BG},
			      {fg,black},
			      {align,w},
			      {label, {text,"current directory"}},
			      {select,true}
			     ]),
    gs:radiobutton(winframe, [{width,145},
			      {height,20},
			      {x,145},
			      {y,35},
			      {group,Mode},
			      {data,all},
			      {bg,?BG},
			      {fg,black},
			      {align,w},
			      {label, {text,"all directories"}}
			     ]),
    gs:frame(frame, winframe, [{width, FrameWidth},
			       {height, FrameHeight},
			       {x,FrameX},
			       {y,FrameY},
			       {bw,0},
			       {bg,?BG}
			      ]),
    gs:listbox(listbox, frame, [{width,ListboxWidth},
				{height,130},
				{x,10},
				{y,10},
				{bc,?BG},
				{bg,white},
				{fg,black},
				{hscroll,bottom},
				{vscroll,right},
				{selectmode,single},
				{click,true}
			       ]),
    gs:label(frame, [{width,Width},
		     {height,20},
		     {x,5},
		     {y,150},
		     {bg,?BG},
		     {fg,black},
		     {align,center},
		     {label, {text,EntryLabelText}}
		    ]),

    gs:entry(mentry, frame, [{width,Width},
			     {height,30},
			     {x,10},
			     {y,170},
			     {bw,2},
			     {bg,white},
			     {fg,black},
			     {keypress,true},
			     {setfocus,true}
			    ]),
    
    case Mode of
	incdir ->
	    done;
	macro ->
	    gs:label(frame, [{width,Width},
			     {height,20},
			     {x,Width+20},
			     {y,150},
			     {bg,?BG},
			     {fg,black},
			     {align,center},
			     {label, {text,"Value:"}}
			    ]),
	    gs:entry(ventry, frame, [{width,Width},
				     {height,30},
				     {x,Width+20},
				     {y,170},
				     {bw,2},
				     {bg,white},
				     {fg,black},
				     {keypress,true}
				    ])
    end,
    
    gs:frame(btnframe, winframe, [{width, BtnFrameWidth},
				  {height, BtnFrameHeight},
				  {x,BtnFrameX},
				  {y,BtnFrameY},
				  {bw,0},
				  {bg,?BG}
				 ]),
    gs:button(add, btnframe, [{width,BtnWidth},
			      {height,BtnHeight},
			      {x,BtnX},
			      {y,BtnY0-BtnFrameY},
			      {bg,?BG},
			      {fg,black},
			      {align,center},
			      {label, {text,"Add"}}
			     ]),
    gs:button(delete, btnframe, [{width,BtnWidth},
				 {height,BtnHeight},
				 {x,BtnX},
				 {y,BtnY0-BtnFrameY+BtnDY},
				 {bg,?BG},
				 {fg,black},
				 {align,center},
				 {label, {text,"Delete"}}
				]),
    gs:button(change, btnframe, [{width,BtnWidth},
				 {height,BtnHeight},
				 {x,BtnX},
				 {y,BtnY0-BtnFrameY+BtnDY*2},
				 {bg,?BG},
				 {fg,black},
				 {align,center},
				 {label, {text,"Change"}}
				]),
    gs:button(done, btnframe, [{width,BtnWidth},
			       {height,BtnHeight},
			       {x,BtnX},
			       {y,BtnY0-BtnFrameY+BtnDY*3},
			       {bg,?BG},
			       {fg,black},
			       {align,center},
			       {label, {text,"Done"}}
			      ]),
    gs:config(win, [{map,true}]).
    



loop(Parent, Mode, Table, Dir, Range) ->
    receive

	{gs, mentry, keypress, _Data, ['Tab' | _]} when Mode == incdir ->
	    loop(Parent, Mode, Table, Dir, Range);

	{gs, mentry, keypress, _Data, ['Tab' | _]} ->
	    gs:config(ventry, [{setfocus,true}, {select,{0,1000}}]),
	    loop(Parent, Mode, Table, Dir, Range);

	{gs, ventry, keypress, _Data, ['Tab' | _]} ->
	    gs:config(mentry, [{setfocus,true}, {select,{0,1000}}]),
	    loop(Parent, Mode, Table, Dir, Range);

	{gs, mentry, keypress, _Data, ['Return' | _]} ->
	    {Idx, Elem} = read_user_input(Mode, Table, Dir, Range, add),
	    mark_added_elem(Idx, Elem),
	    loop(Parent, Mode, Table, Dir, Range);

	{gs, ventry, keypress, _Data, ['Return' | _]} ->
	    {Idx, Elem} = read_user_input(Mode, Table, Dir, Range, add),
	    mark_added_elem(Idx, Elem),
	    loop(Parent, Mode, Table, Dir, Range);

	{gs, mentry, keypress, _Data, _Args} ->
	    loop(Parent, Mode, Table, Dir, Range);

	{gs, ventry, keypress, _Data, _Args} ->
	    loop(Parent, Mode, Table, Dir, Range);

	{gs, listbox, click, Data, [Idx | _]} ->
	    gs:config(mentry, [{delete, {0,1000}}]),
	    config_entries(Idx, Mode),
	    loop(Parent, Mode, Table, Dir, Range);

	{gs, add, click, _Data, _Args} ->
	    {Idx, Elem} = read_user_input(Mode, Table, Dir, Range, add),
	    mark_added_elem(Idx, Elem),
	    loop(Parent, Mode, Table, Dir, Range);

	{gs, delete, click, _Data, _Args} ->
	    {Idx,_Elem} = read_user_input(Mode, Table, Dir, Range, delete),
	    config_entries(Idx, Mode),
	    loop(Parent, Mode, Table, Dir, Range);

	{gs, change, click, _Data, _Args} ->
	    {Idx, _Elem} = read_user_input(Mode, Table, Dir, Range, change),
	    config_entries(Idx, Mode),
	    loop(Parent, Mode, Table, Dir, Range);

	{gs, _Id, click, Range, [_Text, range | _]} ->
	    loop(Parent, Mode, Table, Dir, Range);

	{gs, _Id, click, NewRange, [_Text, incdir | _]} ->
	    update_listbox(incdir, Table, get_key(incdir, NewRange, Dir)),
	    gs:config(mentry, [{delete, {0,1000}}]),
	    loop(Parent, incdir, Table, Dir, NewRange);

	{gs, _Id, click, NewRange, [_Text, macro | _]} ->
	    update_listbox(macro, Table, get_key(macro, NewRange, Dir)),
	    gs:config(mentry, [{delete, {0,1000}}, {setfocus,true}]),
	    gs:config(ventry, [{delete, {0,1000}}]),
	    loop(Parent, macro, Table, Dir, NewRange);

	{gs, done, click, _data, _args} ->
	    exit(normal);

	{gs, _Id, destroy, _Data, _Args} ->
	    exit(normal);

	{gs, _Id, configure, _Data, _Args} ->
	    gs:config(win, [{width, ?WIN_WIDTH}, {height, ?WIN_HEIGHT}]),
	    loop(Parent, Mode, Table, Dir, Range);

	{new_dir, NewDir} when Mode == incdir ->
	    update_listbox(incdir, Table, get_key(incdir, Range, NewDir)),
	    gs:config(mentry, [{delete, {0,1000}}, {setfocus,true}]),
	    loop(Parent, incdir, Table, NewDir, Range);

	{new_dir, NewDir} ->
	    update_listbox(macro, Table, get_key(macro, Range, NewDir)),
	    gs:config(mentry, [{delete, {0,1000}}, {setfocus,true}]),
	    gs:config(ventry, [{delete, {0,1000}}]),
	    loop(Parent, macro, Table, NewDir, Range);

	raise ->
	    gs:config(win, [raise]),
	    loop(Parent, Mode, Table, Dir, Range);

	destroy ->
	    exit(normal);

	Msg ->
%	    io:format("~p~n", [Msg]),
	    loop(Parent, Mode, Table, Dir, Range)

    end.



mark_added_elem(Idx, Elem) ->
    DataList = gs:read(listbox, data),
    NewIdx = case find_position(Elem, DataList) of
		 undefined ->
		     Idx;
		 N ->
		     N - 1
	     end,
    gs:config(listbox, [{selection, NewIdx}]),
    gs:config(mentry, [{setfocus,true}, {select, {0,1000}}]).
		 


find_position(Elem, DataList) ->
    find_position(Elem, DataList, 1).


find_position(Elem, [Elem | T], N) ->
    N;
find_position(Elem, [H | T], N) ->
    find_position(Elem, T, N + 1);
find_position(Elem, [], N) ->
    undefined.




config_entries(Idx, incdir) when integer(Idx) ->
    gs:config(listbox, [{selection,Idx}]),
    gs:config(mentry, [{delete, {0,1000}}]),
    ListboxData = gs:read(listbox, data),
    case gs:read(listbox, selection) of  %% What we tried to set may not be what really was set!
	[SetIdx] ->
	    gs:config(mentry, [{insert, {0, element(2, lists:nth(SetIdx + 1, ListboxData))}}]),
	    gs:config(mentry, [{select, {0,1000}}]);
	[] ->
	    gs:config(mentry, [{insert, {0,""}}]);
	[SetIdx | T] ->
	       %% This last case is a very strange one!
	       %% Sometimes it seems that we crash in this part of the code,
	       %% with the following reason:
	       %% {{case_clause,[0,1]},{dbg_ui_compilerdefs,config_entries,[0,incdir]}}
	       %% Can it be that we somehow for a moment have two rows selected,
	       %% even though singlemode is ordered? 
	       %% To be sure, this last case is added, where we clear all selections,
	       %% and then start all over again in the function, with the uppermost
	       %% selected row chosen...  :-/
	    gs:config(listbox, [{selection,clear}]),
	    config_entries(SetIdx, incdir)
	end;
config_entries(Idx, macro) when integer(Idx) ->
    gs:config(listbox, [{selection,Idx}]),
    gs:config(mentry, [{delete, {0,1000}}]),
    gs:config(ventry, [{delete, {0,1000}}]),
    ListboxData = gs:read(listbox, data),
    case gs:read(listbox, selection) of  %% What we tried to set may not be what really was set!
	[SetIdx] ->
	    {d,M,V} = lists:nth(SetIdx + 1, ListboxData),
	    gs:config(mentry, [{insert, {0,atom_to_list(M)}}]),
	    gs:config(ventry, [{insert, {0,atom_to_list(V)}}, {setfocus,true}]),
	    gs:config(ventry, [{select, {0,1000}}]);
	[] ->
	    gs:config(mentry, [{insert, {0,""}}, {setfocus,true}]),
	    gs:config(ventry, [{insert, {0,""}}]);
	[SetIdx | T] ->
	       %% This last case is a very strange one!
	       %% Sometimes it seems that we crash in this part of the code,
	       %% with the following reason:
	       %% {{case_clause,[0,1]},{dbg_ui_compilerdefs,config_entries,[0,incdir]}}
	       %% Can it be that we somehow for a moment have two rows selected,
	       %% even though singlemode is ordered? 
	       %% To be sure, this last case is added, where we clear all selections,
	       %% and then start all over again in the function, with the uppermost
	       %% selected row chosen...  :-/
	    gs:config(listbox, [{selection,clear}]),
	    config_entries(SetIdx, macro)
    end;
config_entries(_Idx, _Mode) ->
    done.







read_user_input(incdir, Table, Dir, Range, EditMode) ->
    MEntryText   = gs:read(mentry, text),
    {SelDir,Idx} = get_selected(incdir),
    handle_user_input(incdir, EditMode, Table, Dir, Range, {MEntryText, SelDir}),
    {Idx, {i,MEntryText}};
read_user_input(macro, Table, Dir, Range, EditMode) ->
    MacroName      = list_to_atom(gs:read(mentry, text)),
    MacroValue     = list_to_atom(gs:read(ventry, text)),
    {SelMacro,Idx} = get_selected(macro),
    UserInput      = {MacroName, MacroValue, SelMacro},
    handle_user_input(macro, EditMode, Table, Dir, Range, UserInput),
    {Idx, {d,MacroName,MacroValue}}.






handle_user_input(incdir, add, Table, Dir, Range, {"",_S}) ->
    gs:config(win, [beep]),
    tool_utils:notify(win, ["No directory specified!"]);
handle_user_input(incdir, delete, Table, Dir, Range, {_D,""}) ->
    gs:config(win, [beep]),
    tool_utils:notify(win, ["No directory selected!"]);
handle_user_input(incdir, change, Table, Dir, Range, {_D,""}) ->
    gs:config(win, [beep]),
    tool_utils:notify(win, ["No directory selected!"]);
handle_user_input(incdir, EditMode, Table, Dir, Range, {D,S}) ->
    case check_dir(D, Dir) of
	{error,Reason} ->
	    gs:config(win, [beep]),
	    tool_utils:notify(win, ["Not a valid include directory!"]);
	AbsDir ->
	    NewElem  = {i,AbsDir},
	    Key      = get_key(incdir, Range, Dir),
	    PrevDirs = get_prev_inserted(Table, Key),
	    NewDirs  =
		case EditMode of
		    add ->
			case lists:keymember(D, 2, PrevDirs) of
			    true ->
				gs:config(win, [beep]),
				tool_utils:notify(win, ["Directory already added!"]),
				PrevDirs;
			    false ->
				PrevDirs ++ [NewElem]
			end;
		    delete ->
			lists:keydelete(S, 2, PrevDirs);
		    change ->
			lists:map(fun({i,CDir}) when CDir == S ->
					  NewElem;
				     (X) ->
					  X
				  end, PrevDirs)
		end,
	    dbg_ets:insert(Table, {Key,NewDirs}),  %% Do not sort! Order may be important!
	    update_listbox(incdir, Table, Key)
    end;

%% When dealing with macros:
handle_user_input(macro, add, Table, Dir, Range, {'',_V,_S}) ->
    gs:config(win, [beep]),
    tool_utils:notify(win, ["Not a valid macro definition!"]);
handle_user_input(macro, delete, Table, Dir, Range, {_M,_V,''}) ->
    gs:config(win, [beep]),
    tool_utils:notify(win, ["No macro selected!"]);
handle_user_input(macro, EditMode, Table, Dir, Range, {M,'',S}) ->
    handle_user_input(macro, EditMode, Table, Dir, Range, {M,true,S});
handle_user_input(macro, change, Table, Dir, Range, {_M,_V,''}) ->
    gs:config(win, [beep]),
    tool_utils:notify(win, ["No macro selected!"]);
handle_user_input(macro, change, Table, Dir, Range, {M,_V,S}) when M /= S ->
    gs:config(win, [beep]),
    tool_utils:notify(win, ["Cannot change macro name!"]);
handle_user_input(macro, EditMode, Table, Dir, Range, {M,V,S}) ->
    NewElem = {d,M,V},
    Key     = get_key(macro, Range, Dir),
    PrevMacroDefs = get_prev_inserted(Table, Key),
    NewMacroDefs  =
	case EditMode of
	    add ->
		case lists:keymember(M, 2, PrevMacroDefs) of
		    true ->
			gs:config(win, [beep]),
			tool_utils:notify(win, ["Macro already defined!"]),
			PrevMacroDefs;
		    false ->
			[NewElem | PrevMacroDefs]
		end;
	    delete ->
		lists:keydelete(S, 2, PrevMacroDefs);
	    change ->
		NMD = lists:keydelete(S, 2, PrevMacroDefs),
		[NewElem | NMD]
	end,
    dbg_ets:insert(Table, {Key,lists:sort(NewMacroDefs)}),
    update_listbox(macro, Table, Key).

    
    


check_dir(UsrDir, Dir) ->
    AbsDir = filename:absname(UsrDir, Dir),
    case file:read_file_info(AbsDir) of
	{ok, _Data} ->
	    AbsDir;
	{error, _Reason} ->
	    {error, _Reason}
    end.





get_selected(incdir) ->
    case gs:read(listbox, selection) of
	[Idx] ->
	    {element(2, lists:nth(Idx + 1, gs:read(listbox,data))), Idx};
	[] ->
	    {"",undefined}
	end;
get_selected(macro) ->
    case gs:read(listbox, selection) of
	[Idx] ->
	    {element(2, lists:nth(Idx + 1, gs:read(listbox,data))), Idx};
	[] ->
	    {'',undefined}
    end.
    




get_key(Mode, all, Dir) ->
    {Mode,all};
get_key(Mode, dir, Dir) ->
    {Mode,Dir}.





update_listbox(macro, Tab, Key) ->
    MacroDefs = get_prev_inserted(Tab, Key),
    gs:config(listbox, [{items, lists:map(fun({d,M,V}) ->
						  atom_to_list(M) ++ " = " ++ atom_to_list(V)
					  end, MacroDefs)},
			{data,MacroDefs}
		       ]);
update_listbox(incdir, Tab, Key) ->
    IncDefs = get_prev_inserted(Tab, Key),
    gs:config(listbox, [{items, lists:map(fun({i,D}) ->
						  D
					  end, IncDefs)},
			{data,IncDefs}
		       ]).



get_prev_inserted(Tab, Key) ->
    case dbg_ets:lookup(Tab, Key) of
	[] ->
	    [];
	[{Key,List}] ->
	    List
    end.
