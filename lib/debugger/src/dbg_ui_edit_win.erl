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
-module(dbg_ui_edit_win).

%% External exports
-export([create_win/5, get_window/1,
	 handle_event/2]).

-record(winInfo, {window,   % gsobj()
		  entry,    % gsobj()
		  button,   % gsobj()
		  type      % atom()
		 }).

%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% create_win(GS, Pos, Title, Prompt, {Type, Value}) -> #winInfo{}
%%   GS = graphics system identifier
%%   Pos = {X, Y}
%%     X = Y = integer()
%%   Title = string()
%%   Prompt = atom()
%%   Type = term | atom | float | integer | string
%%   Value = term()
%%--------------------------------------------------------------------
create_win(GS, {X, Y}, Title, Prompt, {Type, Value}) ->
    Pad=10, Wbtn = 50,
    
    Win = gs:window(GS, [{title, Title}, {x, X}, {y, Y}, {destroy, true}]),

    Lbl = gs:label(Win, [{align, e}]),

    Font = gs:read(GS, {choose_font, {screen,12}}),
    Prompt2 = io_lib:format("~p", [Prompt]),
    Value2 = io_lib:format("~p", [Value]),
    {W1, _} = gs:read(Lbl, {font_wh, {Font, Prompt2}}),
    {W2, _} = gs:read(Lbl, {font_wh, {Font, Value2}}),
    Wlbl = min(50, W1),
    Went = min(50, W2),
    W = Pad + Wlbl + Went + Pad,

    gs:config(Lbl, [{label, {text, Prompt}},
		    {x, Pad}, {y, Pad}, {width, Wlbl}]),
    Ent = gs:entry(Win, [{text, Value2},
			 {x, Pad+Wlbl}, {y, Pad}, {width, Went},
			 {keypress, true}]),
    Hent = gs:read(Ent, height),
    
    Ybtn = Pad+Hent+Pad,
    Btn = gs:button(Win, [{label, {text,"Ok"}}, {x, Pad}, {y, Ybtn},
			  {width, Wbtn}, {height, Hent}]),
    gs:button(Win, [{label, {text,"Cancel"}}, {x, W-Pad-Wbtn}, {y, Ybtn},
		    {width, Wbtn}, {height, Hent}]),

    H = Ybtn+Hent+Pad,
    gs:config(Win, [{width, W}, {height, H}, {map, true}]),
    #winInfo{window=Win, entry=Ent, button=Btn, type=Type}.

min(X, Y) when X<Y -> Y;
min(X, Y) -> X.

%%--------------------------------------------------------------------
%% get_window(WinInfo) -> Window
%%   WinInfo = #winInfo{}
%%   Window = gsobj()
%%--------------------------------------------------------------------
get_window(WinInfo) ->
    WinInfo#winInfo.window.

%%--------------------------------------------------------------------
%% handle_event(GSEvent, WinInfo) -> Command
%% GSEvent = {gs, Id, Event, Data, Arg}
%% WinInfo = #winInfo{}
%% Command = ignore
%%         | stopped
%%         | {edit, Value}
%%--------------------------------------------------------------------
handle_event({gs, _Id, destroy, _Data, _Arg}, _WinInfo) ->
    stopped;
handle_event({gs, Id, keypress, Data, ['Return'|_]}, WinInfo) ->
    gs:config(WinInfo#winInfo.button, flash),
    handle_event({gs, Id, click, Data, ["Ok"]}, WinInfo);
handle_event({gs, _Id, click, _Data, ["Ok"|_]}, WinInfo) ->
    Ent = WinInfo#winInfo.entry,
    Str = gs:read(Ent, text),
    Type = WinInfo#winInfo.type,
    case erl_scan:string(Str) of
	{ok, Tokens, _EndLine} when Type==term ->
	    case erl_parse:parse_term(Tokens++[{dot, 1}]) of
		{ok, Value} -> {edit, Value};
		Error -> ignore
	    end;
	{ok, [{Type, _Line, Value}], _EndLine} when Type/=term ->
	    {edit, Value};
	_ ->
	    ignore
    end;
handle_event({gs, _Id, click, _Data, ["Cancel"|_]}, _WinInfo) ->
    stopped;
handle_event(GSEvent, _WinInfo) ->
    ignore.
