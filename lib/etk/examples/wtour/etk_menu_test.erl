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
-module(etk_menu_test).

-compile(export_all).

%%
%% Create the browser window
%%

start(N) ->
    etk:start(),
    W = etk:toplevel([]),
    tk:wm([maxsize, W, 10000, 10000]),
    tk:wm([geometry, W, "550x100+100+200"]),
    tk:wm([title, W, "Escape"]),
    MenuBar = etk_menu:create_menubar(W, menu(N)),
    tk:pack(MenuBar, [{side, "top"}, {anchor, "w"}, {fill, "x"}]),
    W.

menu(0) ->
    {menubar, 
     [{menu, "File", "left", 
       [{button, [{label, "One"}]}]}]};
menu(1) ->
    {menubar, 
     [{menu, "File", "left", 
       [{button, [{label, "New Web Browser"}, {underline, 8}, 
		   {accel, "Alt+N"}]},
	{button, [{label, "Do this thing..."},
		 {func, fun() -> io:format("Click\n") end}]},
	separator,
	{button, [{label, "Open Location..."}]}]}]};
menu(2) ->
    {menubar,
     [{menu, "File", "left", 
       [{button, [{label, "New Web Browser"},{underline, 8}, 
		   {accel, "Alt+N"}]},
	separator,
	{button, [{label, "Open Location..."}]}]},
      {menu, "Options", "left",
       [{button, [{label, "General Preferences..."}]},
	{button, [{label, "Mail and News Preferences..."}]},
	separator,
	{button, [{label,"Show Toolbar"}]}]}]};
menu(3) ->
    {menubar, 
     [{menu, "Super", "left", 
       [{submenu, "Options ...",
	 [{button, [{label, "One"}]},
	  separator,
	  {button, [{label, "Two"}]}]}]}]};
menu(4) ->
    {menubar, 
     [{menu, "File", "left", 
       [{checkbutton, {0,1},[{label, "What"}]},
	{checkbutton, {0,1},[{label, "Why"}]}]}]};
menu(5) ->
    {menubar, 
     [{menu, "File", "left", 
       [{radiobutton, 1,[{label, "What"}]},
	{radiobutton, 2,[{label, "Why"}]}]}]}.








