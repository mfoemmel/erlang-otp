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
-module(dbg_ui_filedialog_win).

%% External exports
-export([create_win/6, create_win/7, get_window/1,
	 tag/2,
	 handle_event/2]).

-record(winInfo, {window,        % gsobj()
		  extra,         % fun()
		  cwd,           % string()
		  pattern        % string()
		 }).

%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% create_win(GS, Title, Pos, Mode, Filter, Extra)
%% create_win(GS, Title, Pos, Mode, Filter, Extra, FileName) -> #winInfo{}
%%   GS  = term()
%%   Title = string()
%%   Pos = {X,Y}
%%   Mode = normal | multiselect
%%   Filter = string() File name that may include * symbols.
%%   Extra = fun(File) -> {true, tag} | true | {error, term()}
%%   FileName = string() Suggested file name when saving
%%--------------------------------------------------------------------
create_win(GS, Title, {X,Y}, Mode, Filter, Extra) ->
    create_win(GS, Title, {X,Y}, Mode, Filter, Extra, null).
create_win(GS, Title, {X,Y}, Mode, Filter, Extra, FileName) ->
    Pad = 8,
    W = 480,
    Hobj = 30,
    Wlbl = 80, Hlbl=20,
    Hlb = 130,
    Wbtn = 70,

    Win = gs:window(GS, [{title,Title},
			 {x, X}, {y,Y}, {width, 500}, {height, 320},
			 {destroy, true}]),

    gs:label(Win, [{label, {text,"Filter"}}, {align, sw},
		   {x, Pad+2}, {y, Pad}, {width, Wlbl}, {height, Hlbl}]),
    gs:entry('Filter', Win, [{x, Pad}, {y, Pad+Hlbl},
			     {width, W}, {height, Hobj},
			     {keypress, true}]),

    Xmid = Pad + W/2,
    Y2 = Pad+Hobj+Hobj+Pad,
    gs:label(Win, [{label, {text,"Directories"}}, {align, sw},
		   {x, Pad+2}, {y, Y2}, {width, Wlbl}, {height, Hlbl}]),
    gs:label(Win, [{label, {text,"Files"}}, {align, sw},
		   {x, Xmid+Pad/2+2}, {y, Y2},
		   {width,Wlbl}, {height,Hlbl}]),
    gs:listbox('Dirs', Win, [{x, Pad}, {y, Y2+Hlbl}, {vscroll,right},
			     {width, W/2-Pad/2}, {height, Hlb},
			     {click, true}, {doubleclick, true}]),
    gs:listbox('Files', Win, [{x, Xmid+Pad/2}, {y, Y2+Hlbl}, {vscroll,right},
			      {width, W/2-Pad/2}, {height, Hlb},
			      {click, true}, {doubleclick, true}]),

    Y3 = Y2+Hobj+Hlb,
    gs:label(Win, [{label, {text,"Selection"}}, {align, sw},
		   {x, Pad+2}, {y, Y3}, {width, Wlbl}, {height, Hlbl}]),
    gs:entry('Selection', Win, [{x, Pad}, {y, Y3+Hlbl},
				{width, W}, {height, Hobj},
				{keypress, true}]),

    Y4 = Y3+Hobj+Hobj+Pad,
    Wb = W-Wbtn,
    Opts = [{y, Y4}, {width, Wbtn}, {height, Hobj}],
    case Mode of
	normal ->
	    gs:button(Win, [{label, {text,"Ok"}}, {x, Pad},
			    {data, select} | Opts]),
	    gs:button(Win, [{label, {text,"Filter"}}, {x, W/2-Wbtn/2},
			    {data, filter} | Opts]),
	    gs:button(Win, [{label, {text,"Cancel"}}, {x, Pad+Wb},
			    {data, done} | Opts]);
	multiselect ->
	    gs:button(Win, [{label, {text,"Choose"}}, {x, Pad},
			    {data, select} | Opts]),
	    gs:button(Win, [{label, {text,"All"}}, {x, Pad+Wb/3},
			    {data, multiselect} | Opts]),
	    gs:button(Win, [{label, {text,"Filter"}}, {x, Pad+2*Wb/3},
			    {data, filter} | Opts]),
	    gs:button(Win, [{label, {text,"Done"}}, {x, Pad+Wb},
			    {data, done} | Opts])
    end,

    {ok, Home} = file:get_cwd(),
    {Cwd, Pattern} = update_win(Filter, Extra, Home),
    if
	list(FileName) ->
	    gs:config('Selection', {text, filename:join(Cwd, FileName)});
	true -> ignore
    end,

    gs:config(Win, [{height, Y4+Hobj+Pad}, {map, true}]),
    #winInfo{window=Win, extra=Extra, cwd=Cwd, pattern=Pattern}.

%%--------------------------------------------------------------------
%% get_window(WinInfo) -> Window
%%   WinInfo = #winInfo{}
%%   Window = gsobj()
%%--------------------------------------------------------------------
get_window(WinInfo) ->
    WinInfo#winInfo.window.

%%--------------------------------------------------------------------
%% tag(WinInfo, File)
%%   WinInfo = #winInfo{}
%%   File = string()
%%--------------------------------------------------------------------
tag(WinInfo, File0) ->
    File = relfile(WinInfo#winInfo.cwd, File0),
    case member(File, gs:read('Files', items)) of
	{true, Index} -> gs:config('Files', {change, {Index, tag(File)}});
	false -> ignore
    end.

tag(Str) -> [$*|Str].
untag([$*|Str]) -> Str;
untag([$(|Str]) -> [$)|Rts] = lists:reverse(Str),lists:reverse(Rts);
untag(Str) -> Str.

member(E, L) ->        member(E, L, 0).
member(E, [E|_], I) -> {true, I};
member(E, [_|T], I) -> member(E, T, I+1);
member(_E, [], _I) ->  false.

%%--------------------------------------------------------------------
%% handle_event(GSEvent, WinInfo) -> Command
%%   GSEvent = {gs, Id, Event, Data, Arg}
%%   WinInfo = #winInfo{}
%%   Command = ignore
%%           | {stopped, Dir}
%%           | {win, WinInfo}
%%           | {select, File} | {multiselect, Dir, FileNames}
%%--------------------------------------------------------------------
handle_event({gs, _Id, destroy, _Data, _Args}, WinInfo) ->
    {stopped, WinInfo#winInfo.cwd};

handle_event({gs, 'Filter', keypress, _Data, ['Return'|_]}, WinInfo) ->
    handle_event({gs, null, click, filter, null}, WinInfo);
handle_event({gs, 'Selection', keypress, _Data, ['Return'|_]}, WinInfo) ->
    handle_event({gs, null, click, select, null}, WinInfo);

handle_event({gs, 'Dirs', click, _Data, [0,"..",true|_]}, WinInfo) ->
    Filter = filename:join(filename:dirname(WinInfo#winInfo.cwd),
			   WinInfo#winInfo.pattern),
    gs:config('Filter', {text, Filter}),
    ignore;
handle_event({gs, 'Dirs', click, _Data, [Index,Str,true|_]}, WinInfo) ->
    Filter = filename:join([WinInfo#winInfo.cwd, Str,
			    WinInfo#winInfo.pattern]),
    gs:config('Filter', {text, Filter}),
    ignore;
handle_event({gs, 'Dirs', doubleclick, _Data, _Arg}, WinInfo) ->
    handle_event({gs, null, click, filter, null}, WinInfo);

handle_event({gs, 'Files', click, _Data, [Index,Str,true|_]}, WinInfo) ->
    Selection = filename:join(WinInfo#winInfo.cwd, untag(Str)),
    gs:config('Selection', {text, Selection}),
    ignore;
handle_event({gs, 'Files', doubleclick, _Data, _Arg}, WinInfo) ->
    handle_event({gs, null, click, select, null}, WinInfo);
  
handle_event({gs, _Id, click, select, _Arg}, WinInfo) ->
    {select, gs:read('Selection', text)};
handle_event({gs, _Id, click, multiselect, _Arg}, WinInfo) ->
    Files = lists:map(fun(File) -> untag(File) end,
		      gs:read('Files', items)),
    {multiselect, WinInfo#winInfo.cwd, Files};
handle_event({gs, _Id, click, filter, _Arg}, WinInfo) ->
    {Cwd, Pattern} = update_win(gs:read('Filter', text),
				WinInfo#winInfo.extra,
				WinInfo#winInfo.cwd),
    {win, WinInfo#winInfo{cwd=Cwd, pattern=Pattern}};
handle_event({gs, _Id, click, done, _Arg}, WinInfo) ->
    {stopped, WinInfo#winInfo.cwd};
    
handle_event(GSEvent, _WinInfo) ->
    ignore.

%%====================================================================
%% Internal functions
%%====================================================================

update_win(Filter, ExtraFilter, Prev) ->
    {Res, {Filter2, Cwd, FilePattern}} = check_filter(Filter, Prev),

    Dirs = [".." | get_subdirs(Cwd)],

    gs:config('Filter', {text, Filter2}),
    gs:config('Dirs', {items, Dirs}),
    gs:config('Selection', {text, Cwd}),

    case Res of
	ok ->
	    Matching = lists:sort(filelib:wildcard(Filter2)),
	    Files = extra_filter(Matching, Cwd, ExtraFilter),
	    gs:config('Files', {items, Files});
	error ->
	    gs:config('Files', beep)
    end,

    {Cwd, FilePattern}.

%% check_filter(Filter, Prev) -> {ok, Res} | {error, Res}
%%   Res = {Filter, Cwd, FilePattern}
%%   Filter = Prev = Cwd = FilePattern = string()
check_filter(Filter0, Prev) ->
    Filter = case filename:pathtype(Filter0) of
		 absolute -> Filter0;
		 _Relative -> filename:absname(Filter0, Prev)
	     end,
    Comps = filename:split(Filter),
    Last = lists:last(Comps),
    FilePattern = case is_pattern(Last) of
		      true -> Last;
		      false -> "*"
		  end,
    {Cwd, Rest} = max_existing(Comps),
    case Rest of
	[] ->
	    %% Filter = existing file or directory
	    Res = case filelib:is_dir(Filter) of
		      true -> {filename:join(Filter, "*"), Filter, "*"};
		      false -> {Filter, filename:dirname(Filter),
				filename:basename(Filter)}
		  end,
	    {ok, Res};
	[FilePattern] ->
	    %% Filter = existing dir and valid pattern
	    {ok, {Filter, Cwd, FilePattern}}; 
	Comps ->
	    %% Filter = garbage
	    {error, {Prev, Prev, "*"}};
	[Name|Names] ->
	    %% Filter = existing dir ++ pattern or non-existing file/dir
	    case is_pattern(Name) of
		true -> {ok, {Filter, Cwd, FilePattern}};
		false -> {error, {Cwd, Cwd, ""}}
	    end
    end.

max_existing([Name | Names]) ->
    case filelib:is_file(Name) of
	true -> max_existing(Name, Names);
	false -> {[], [Name | Names]}
    end.
max_existing(Dir, [Name | Names]) ->
    Dir2 = filename:join(Dir, Name),
    case filelib:is_file(Dir2) of
	true when Names==[] -> {Dir2, []};
	true -> max_existing(Dir2, Names);
	false -> {Dir, [Name | Names]}
    end.

is_pattern(Str) ->
    lists:member($*, Str).

extra_filter([File|Files], Dir, Fun) ->
    case Fun(File) of
	true ->
	    [relfile(Dir, File) | extra_filter(Files, Dir, Fun)];
	{true,tag} ->
	    [[$*|relfile(Dir,File)] | extra_filter(Files, Dir, Fun)];
	{true,disable} ->
	    [[$(|relfile(Dir,File)]++[$)] | extra_filter(Files, Dir, Fun)];
	{error, _Reason} -> extra_filter(Files, Dir, Fun)
    end;
extra_filter([], _Dir, _Fun) -> [].

get_subdirs(Dir) ->
    case file:list_dir(Dir) of
	{ok, FileNames} ->
	    X = lists:filter(fun(FileName) ->
				     File = filename:join(Dir, FileName),
				     filelib:is_dir(File)
			     end,
			     FileNames),
	    lists:sort(X);
	_Error -> []
    end.

%% Return the "remainder" of a file name relative a dir name, examples:
%%   relfile("/home/gunilla", "/home/gunilla/m.erl") -> "m.erl"
%%   relfile("/home/gunilla/dir", "/home/gunilla/dir/m.erl") -> "dir/m.erl"
%%   relfile("/home/gunilla", "/home/arne/m.erl") -> "/home/arne/m.erl"
relfile(Dir, File) ->
    case compare(Dir, File) of
	error -> File;
	RelFile -> RelFile
    end.

compare([X|Dir], [Y|File]) ->
    compare(Dir, File);
compare([], [$/|File]) ->
    File;
compare(_, _) ->
    error.
    
