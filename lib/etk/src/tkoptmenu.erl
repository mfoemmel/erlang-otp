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
%% This file was derived from:
%%
%% optMenu.tcl --
%%
%% This file defines the procedure tk_optionMenu, which creates
%% an option button and its associated menu.
%%
%% SCCS: @(%%) optMenu.tcl 1.9 96/02/16 10:48:26
%%
%% Copyright (c) 1994 The Regents of the University of California.
%% Copyright (c) 1994 Sun Microsystems, Inc.
%%
%% See the file "license.terms" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
-module(tkoptmenu).

-export([tk_optionMenu/4]).

%% tk_optionMenu --
%% This procedure creates an option button named $w and an associated
%% menu.  Together they provide the functionality of Motif option menus:
%% they can be used to select one of many values, and the current value
%% appears in the global variable varName, as well as in the text of
%% the option menubutton.  The name of the menu is returned as the
%% procedure's result, so that the caller can use it to change configuration
%% options on the menu or otherwise manipulate it.
%%
%% Arguments:
%% w -			The name to use for the menubutton.
%% varName -		Global variable to hold the currently selected value.
%% firstValue -		First of legal values for option (must be >= 1).
%% args -		Any number of additional values.

tk_optionMenu(W, VarName, FirstValue, Args) ->
    Menu = W ++ ".menu",
    tk:setvar(VarName, FirstValue),
    tk:menubutton(W, [{textvariable, VarName}, 
		      {indicatoron, 1}, 
		      {menu, Menu},
		      {relief, "raised"},
		      {bd, 2},
		      {highlightthickness, 2},
		      {anchor,"c"}]),
    tk:menu(Menu, [{tearoff, 0}]),
    tk:cmd(Menu, [add, command, 
		  {label, FirstValue},
		  {command, fun() ->
				    tk:setvar(VarName,FirstValue)
			    end}]),
    lists:foreach(
      fun(I) ->
	      tk:cmd(Menu,
		     [add, command,
		      {label, I},
		      {command, fun() ->
				       tk:setvar(VarName, I)
				end}])
      end, Args),
    Menu.
