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
-module(etk_menu).

-export([create_menubar/2, add_menu/2]).

-import(lists, [foreach/2, flatmap/2]).

%%
%% Create menu
%%
%%   create_menubar(Win, Menu) -> MenuBar


%% MenuBar   = {menubar, [Menu]}
%% Menu      = {menu, Name, Side, [MenuItems]}
%% MenuItems = Button | radioButton | SubMenu | CheckButton
%% Button    = {button, [Opts]}
%% SubMenu   = {subMenu, Id, Title, [MenuItems]}
%% ..

create_menubar(Parent, {menubar, Menus}) ->
    MenuBar = etk:frame(Parent, []),
    foreach(fun(M) -> add_menu(MenuBar, M) end, Menus),
    MenuBar.

add_menu(Parent, {menu, Name, Side, Children}) ->
    MenuButton = etk:menubutton(Parent, []),
    Menu = name(MenuButton, m),
    tk:menu(Menu, [{tearoff, 0}]),
    foreach(fun(C) -> add_menu_item(Menu, C) end, Children),
    tk_cmd(MenuButton, ["configure",{menu, Menu}, {text, Name},
			{font, menu_font()}]),
    tk_pack(MenuButton, [{side, Side}]),
    Menu.

add_menu_item(Parent, {submenu, Name, Children}) ->
    Menu = etk:menu(Parent, [{tearoff, 0}]),
    foreach(fun(C) -> add_menu_item(Menu, C) end, Children),
    tk_cmd(Parent, ["add", "cascade", {menu, Menu}, {label, Name}]);
add_menu_item(Parent, {button, Opts}) ->
    tk_cmd(Parent, ["add", "command" | button_opts(Opts)]);
add_menu_item(Parent, {radiobutton, Value, Opts}) ->
    tk_cmd(Parent, ["add", "radiobutton", {value, Value} | button_opts(Opts)]);
add_menu_item(Parent, {checkbutton, Value, Opts}) ->
    {Off, On} = Value,
    tk_cmd(Parent, ["add", "checkbutton", {onvalue, On}, {offvalue, Off}
		    | button_opts(Opts)]);
add_menu_item(Parent, separator) ->
    tk_cmd(Parent, ["add", "separator"]).

button_opts(Opts) -> flatmap(fun buttonopt/1, Opts).

buttonopt({underline, N})      -> [{underline, N}];
buttonopt({accelerator, Str})  -> [{accelerator, Str}];
buttonopt({label, Text})       -> [{label, Text}, {font, menu_font()}];
buttonopt({bitmap, Bitmap})    -> [{bitmap, Bitmap}];
buttonopt({image, Image})      -> [{image, Image}];
buttonopt({func, Fun})         -> [{command,Fun}];
buttonopt(X)                   -> io:format("invalid buttonopt ~p\n", [X]), [].

menu_font() -> "-*-helvetica-bold-r-*-*-12-*-*-*-*-*-*-*".

name(Parent, Id) when atom(Id) ->  Parent ++ "." ++ atom_to_list(Id).

tk_cmd(Parent, Opts) ->
    %% io:format("tk:cmd(~p,~p)\n", [Parent, Opts]),
    tk:cmd(Parent, Opts).

tk_pack(Parent, Opts) ->
    %% io:format("tk:pack(~p,~p)\n", [Parent, Opts]),
    tk:pack(Parent, Opts).
