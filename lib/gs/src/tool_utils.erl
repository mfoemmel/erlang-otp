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
-module(tool_utils).
-include_lib("kernel/include/file.hrl").

%%%----------------------------------------------------------------------
%%% Auxiliary functions to be used by the tools (internal module)
%%%----------------------------------------------------------------------

%% External exports
-export([open_help/2]).
-export([file_dialog/1]).
-export([notify/2, confirm/2, confirm_yesno/2, request/2]).

%%----------------------------------------------------------------------
%% open_help(GS, File)
%%   GS = gsobj()  (GS root object returned by gs:start/0,1)
%%   File = string() | nofile
%% View the help file File, which can be an URL, an HTML file or a text
%% file.
%% This function is OS dependant.
%% Unix: Assumes Netscape is up & running, and use Netscape remote commands
%%   to display the file.
%% NT: If File is a file, use the NT command 'start' which will open the
%%   default tool for viewing the file.
%%   If File is an URL, try to view it using Netscape.exe which requires that
%%   the path Netscape.exe must be in TBD.
%%   (TEMPORARY solution..., can be done better)
%%----------------------------------------------------------------------
open_help(S, nofile) ->
    notify(S, "Sorry, no help information exists");
open_help(S, File) ->
    case application:get_env(kernel, browser_cmd) of
	undefined ->
	    open_help_default(S, File);
	{ok, Cmd} when is_list(Cmd) ->
	    spawn(os, cmd, [Cmd ++ " " ++ File]);
	{ok, {M, F, A}} ->
	    apply(M, F, [File|A]);
	_Other ->
	    Str = ["Bad Kernel configuration parameter browser_cmd",
		   "Do not know how to display help file"],
	    notify(S, Str)
    end.

open_help_default(S, File) ->
    Cmd = case file_type(File) of

	      %% Local file
	      local ->
		  case os:type() of
		      {unix,Type} ->
                          case Type of
                               darwin -> "open " ++ File;
                               _Else -> "netscape -remote \"openURL(file:" ++ File ++ ")\""
			  end;
		      {win32,_AnyType} ->
			  "start " ++ filename:nativename(File);

		      _Other ->
			  unknown
		  end;

	      %% URL
	      remote ->
		  case os:type() of
		      {unix,Type} ->
                          case Type of
                               darwin -> "open " ++ File;
                               _Else -> "netscape -remote \"openURL(file:" ++ File ++ ")\""
			  end;
		      {win32,_AnyType} ->
			  "netscape.exe -h " ++ regexp:gsub(File,"\\\\","/");
		      _Other ->
			  unknown
		  end;

	      Error -> % {error,Reason}
		  Error
	  end,

    if
	is_list(Cmd) ->
	    spawn(os, cmd, [Cmd]);
	Cmd==unknown ->
	    Str = ["Sorry, do not know how to",
		   "display HTML files at this platform"],
	    notify(S, Str);
	true ->
	    {error, Reason} = Cmd,
	    Str = file:format_error(Reason),
	    notify(S, [File,Str])
    end.

%% file_type(File) -> local | remote | {error,Reason}
%%   File = string()
%%   Reason - see file(3)
%% Returns local if File is an existing, readable file
%% Returns remote if File is a remote URL (ie begins with 'http:')
file_type(File) ->
    case File of
	"http://"++_URL ->
	    remote;
	_ ->
	    %% HTML files can have a tag (<name>.html#tag), this must be
	    %% removed when checking if the file exists
	    File2 = case filename:extension(File) of
			".html#"++_Index ->
			    filename:rootname(File)++".html";
			_ ->
			    File
		    end,

            case file:read_file_info(File2) of
	        {ok, FileInfo} when FileInfo#file_info.type==regular,
				    FileInfo#file_info.access/=none ->
		    local;
		{ok, FileInfo} when FileInfo#file_info.type/=regular ->
		    {error,einval};
		{ok, FileInfo} when FileInfo#file_info.access==none ->
		    {error,eacces};
		Error ->
		    Error
	    end
    end.


%%----------------------------------------------------------------------
%% file_dialog(Options) -> tbd
%%----------------------------------------------------------------------
file_dialog(Options) ->
    tool_file_dialog:start(Options).


%%----------------------------------------------------------------------
%% notify(GS, Strings) -> ok
%% confirm(GS, Strings) -> ok | cancel
%% confirm_yesno(GS, Strings) -> yes | no | cancel
%% request(GS, Strings) -> {ok,string()} | cancel
%%   GS = gsobj()  (GS root object returned by gs:start/0,1)
%%   Strings = string() | [string()]
%% Opens a window with the specified message (Strings) and locks the GUI
%% until the user confirms the message.
%% A 'notify' window contains an 'Ok' button.
%% A 'confirm' window contains an 'Ok' and a 'Cancel' button.
%% A 'confirm_yesno' window contains a 'Yes', a 'No', and a 'Cancel'
%% button.
%% A 'request' window contains an entry, an 'Ok' and a 'Cancel' button.
%%----------------------------------------------------------------------
-define(Wlbl, 130).
-define(Hlbl, 30).
-define(Hent, 30).
-define(Wbtn, 50).
-define(Hbtn, 30).
-define(PAD,  10).

notify(S, Strings) ->
    help_win(notify, S, Strings).
confirm(S, Strings) ->
    help_win(confirm, S, Strings).
confirm_yesno(S, Strings) ->
    help_win(confirm_yesno, S, Strings).
request(S, Strings) ->
    help_win(request, S, Strings).

help_win(Type, S, Strings) ->
    GenOpts = [{data,Type}, {keypress,true}],
    GenOpts2 = [{font,{screen,12}} | GenOpts],
    Buttons = buttons(Type),
    Nbtn = length(Buttons),

    %% Create the window and its contents
    Win = gs:create(window, S, [{title,title(Type)} | GenOpts]),
    Top = gs:create(frame, Win, GenOpts),
    Lbl = gs:create(label, Top, [{align,c}, {justify,center} |GenOpts2]),
    Mid = if
	      Type==request -> gs:create(frame, Win, GenOpts);
	      true -> ignore
	  end,
    Ent = if
	      Type==request -> gs:create(entry, Mid, GenOpts2);
	      true -> ignore
	  end,
    Bot = gs:create(frame, Win, GenOpts),

    %% Find out minimum size required for label, entry and buttons
    Font = gs:read(S, {choose_font, {screen,12}}),
    Text = insert_newlines(Strings),
    {Wlbl0,Hlbl0} = gs:read(Lbl, {font_wh,{Font,Text}}),
    {_Went0,Hent0} = gs:read(Lbl, {font_wh,{Font,"Entry"}}),
    {Wbtn0,Hbtn0} = gs:read(Lbl, {font_wh,{Font,"Cancel"}}),
    
    %% Compute size of the objects and adjust the graphics accordingly
    Wbtn = max(Wbtn0+10, ?Wbtn),
    Hbtn = max(Hbtn0+10, ?Hbtn),
    Hent = max(Hent0+10, ?Hent),
    Wlbl = max(Wlbl0, max(Nbtn*Wbtn+(Nbtn-1)*?PAD, ?Wlbl)),
    Hlbl = max(Hlbl0, ?Hlbl),

    Wwin = ?PAD+Wlbl+?PAD,

    Htop = ?PAD+Hlbl,
    Hmid = if Type==request -> ?PAD+Hent; true -> 0 end,
    Hbot = ?PAD+Hbtn+?PAD,
    Hwin = Htop+Hmid+Hbot,

    gs:config(Win, [                        {width,Wwin},{height,Hwin}]),

    gs:config(Top, [{x,0},   {y,0},         {width,Wwin},{height,Htop}]),
    gs:config(Lbl, [{x,?PAD},{y,?PAD},      {width,Wlbl},{height,Hlbl}]),

    gs:config(Mid, [{x,0},   {y,Htop},      {width,Wwin},{height,Hmid}]),
    gs:config(Ent, [{x,?PAD},{y,?PAD},      {width,Wlbl},{height,Hent}]),

    gs:config(Bot, [{x,0},   {y,Htop+Hmid}, {width,Wwin},{height,Hbot}]),

    %% Insert the label text
    gs:config(Lbl, {label,{text,Text}}),

    %% Add the buttons
    Xbtns = xbuttons(Nbtn, Wbtn, Wwin, Wlbl),
    foreach2(fun(Button, X) ->
		     gs:create(button, Bot, [{x,X}, {y,?PAD},
					     {width,Wbtn}, {height,Hbtn},
					     {label,{text,Button}}
					     | GenOpts2])
	     end,
	     Buttons,
	     Xbtns),

    gs:config(Win, {map,true}),

    event_loop(Win, Ent).

title(notify) ->        "Notification";
title(confirm) ->       "Confirmation";
title(confirm_yesno) -> "Confirmation";
title(request) ->       "Request".

buttons(notify) ->        ["Ok"];
buttons(confirm) ->       ["Ok", "Cancel"];
buttons(confirm_yesno) -> ["Yes", "No", "Cancel"];
buttons(request) ->       ["Ok", "Cancel"].

max(X, Y) when X>Y -> X;
max(_X, Y) -> Y.

xbuttons(1, Wbtn, Wwin, _Wlbl) ->
    [round(Wwin/2-Wbtn/2)];
xbuttons(2, Wbtn, Wwin, Wlbl) ->
    Margin = (Wwin-Wlbl)/2,
    [round(Margin), round(Wwin-Margin-Wbtn)];
xbuttons(3, Wbtn, Wwin, Wlbl) ->
    Margin = (Wwin-Wlbl)/2,
    [round(Margin), round(Wwin/2-Wbtn/2), round(Wwin-Margin-Wbtn)].

foreach2(Fun, [H1|T1], [H2|T2]) ->
    Fun(H1, H2),
    foreach2(Fun, T1, T2);
foreach2(_Fun, [], []) ->
    true.

event_loop(Win,Entry) ->
    receive

	{gs,_Obj,_Event,Type,["Ok"|_]} when Type/=request ->
	    gs:destroy(Win),
	    ok;

	{gs,_Obj,_Event,request,["Ok"|_]} ->
	    case gs:read(Entry, text) of
		"" ->
		    event_loop(Win, Entry);
		Info ->
		    gs:destroy(Win),
		    {ok, Info}
	    end;

	{gs,_Obj,_Event,_Type,["Yes"|_]} ->
	    gs:destroy(Win),
	    yes;

	{gs,_Obj,_Event,_Type,["No"|_]} ->
	    gs:destroy(Win),
	    no;

	{gs,_Obj,_Event,_Type,["Cancel"|_]} ->
	    gs:destroy(Win),
	    cancel;

	{gs,_Obj,_Event,Type,['Return'|_]} when Type/=request ->
	    gs:destroy(Win),
	    if
		Type==notify -> ok;
		Type==confirm -> ok;
		Type==confirm_yesno -> yes
	    end;

	{gs,_Obj,_Event,request,['Return'|_]} ->
	    case gs:read(Entry, text) of
		"" ->
		    event_loop(Win, Entry);
		Info ->
		    gs:destroy(Win),
		    {ok, Info}
	    end;

	{gs,_Obj,destroy,Type,_Args} ->
	    if
		Type==notify -> ok;
		true -> cancel
	    end;

	%% Flush any other GS events
	{gs,_Obj,_Event,_Data,_Arg} ->
            event_loop(Win, Entry)
    end.

%% insert_newlines(Strings) => string()
%%   Strings - string() | [string()]
%% If Strings is a list of strings, return a string where all these
%% strings are concatenated with newlines in between,otherwise return
%% Strings.
insert_newlines([String|Rest]) when list(String),Rest/=[]->
    String ++ "\n" ++ insert_newlines(Rest);
insert_newlines([Last]) ->
    [Last];
insert_newlines(Other) ->
    Other.
