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
-module(erltk_demo).

-compile(export_all).

-import(lists, [reverse/1, map/2, foreach/2]).
 
go() ->
    etk:start(),
    fig16_1(),
    fig16_2(),
    fig16_6(),
    fig16_7(),
    fig16_8(),
    fig16_9(),
    true.

fig16_1() ->
    Top = etk:toplevel([]),
    tk:wm([title,Top,"Figure 16.1"]),
    W = map(fun(Relief) ->
		    etk:frame(Top, [{width,"15m"},{height,"10m"},
				    {relief,Relief},{borderwidth,4}])
	    end, [raised, sunken, flat, groove, ridge]),
    tk:pack(W ++ [{side,left},{padx,"2m"},{pady,"2m"}]),
    tk:cmd(lists:nth(3,W),[configure, {background,black}]).

fig16_2() ->
    Top = etk:toplevel([]),
    tk:wm([title,Top,"Figure 16.2"]),
    Bitmap = etk:label(Top, 
		       [{bitmap,"@flagdown.bmp"}]),
    Label  = etk:label(Top,[{text,"No new mail"}]),
    tk:pack([Bitmap, Label]).

fig16_6() ->
    Top    = etk:toplevel([]),
    tk:wm([title,Top,"Figure 16.6"]),
    Ok     = etk:button(Top, [{text,"Ok"},cmd(ok)]),
    Apply  = etk:button(Top, [{text,"Apply"},cmd(apply)]),
    Cancel = etk:button(Top, [{text,"Cancel"},cmd(cancel)]),
    Help   = etk:button(Top, [{text,"Help"},cmd(help)]),
    tk:pack([Ok,Apply,Cancel,Help] ++  [{side,"left"}]).

fig16_7() ->
    Top    = etk:toplevel([]),
    tk:wm([title,Top,"Figure 16.7"]),
    Bold      = etk:checkbutton(Top, [{text,"Bold"},{anchor,"w"}]),
    tk:cmd(Bold, [select]),
    Italic    = etk:checkbutton(Top, [{text,"Italic"},{anchor,"w"}]),
    Underline = etk:checkbutton(Top, [{text,"Underline"},{anchor,"w"}]),
    tk:cmd(Underline, [select]),
    tk:pack([Bold,Italic,Underline] ++ [{side,"top"},{fill,"x"}]).
 
fig16_8() ->
    Top    = etk:toplevel([]),
    tk:wm([title,Top,"Figure 16.8"]),
    Times   = etk:radiobutton(Top, [{text,"Times"},{variable,"font"},
				    {value,times},{anchor,"w"}]),
    Helv    = etk:radiobutton(Top, [{text,"Helvetica"},{variable,"font"},
				    {value,helvetica},{anchor,"w"}]),
    tk:cmd(Helv, [select]),
    tk:pack([Times,Helv] ++ [{side,"top"},{fill,"x"}]).

fig16_9() ->
    Top    = etk:toplevel([]),
    tk:wm([title,Top,"Figure 16.9"]),
    Opts = [{width,"8c"},{justify,left},{relief,raised},{bd,2},
	    {font,"-Adobe-Helvetica-Medium-R-Normal--*-180-*"},
	    {text,"You have made changes to this document "
	          "since the last time it was saved. Is it OK to "
	          "discard the changes?"}],
    Mess = etk:message(Top, Opts),
    tk:pack([Mess]).

fig16_10() ->
    Top    = etk:toplevel([]),
    tk:wm([title,Top,"Figure 16.10"]),
    ListBox = etk:listbox(Top, []),
    tk:pack([ListBox]),
    {ok, Colors} = file:consult("rgb.e"),
    etk:bind(ListBox, "<Button-1>", ['%w'],
	     fun(W) ->
		    Val = tk:selection([get]),
		    io:format("Here:~p~n", [Val])
	    end),
    foreach(fun({_,_,_,Col}) ->
		    tk:cmd(ListBox, [insert,'end',Col])
	    end, Colors).

cmd(X) -> {command, fun() -> io:format("~w~n",[X]) end}.


