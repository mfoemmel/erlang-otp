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
%% tearoff.tcl --
%%
%% This file contains procedures that implement tear-off menus.
%%
%% SCCS: @(%%) tearoff.tcl 1.9 96/02/23 15:31:30
%%
%% Copyright (c) 1994 The Regents of the University of California.
%% Copyright (c) 1994-1995 Sun Microsystems, Inc.
%%
%% See the file "license.terms" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
-module(tktearoff).

-include("tk.hrl").

-export([tkTearOffMenu/1]).

%% tkTearoffMenu --
%% Given the name of a menu, this procedure creates a torn-off menu
%% that is identical to the given menu (including nested submenus).
%% The new torn-off menu exists as a toplevel window managed by the
%% window manager.  The return value is the name of the new menu.
%%
%% Arguments:
%% w -			The menu to be torn-off (duplicated).

tkTearOffMenu(W) ->
    %% Find a unique name to use for the torn-off menu.  Find the first
    %% ancestor of w that is a toplevel but not a menu, and use this as
    %% the parent of the new menu.  This guarantees that the torn off
    %% menu will be on the same screen as the original menu.  By making
    %% it a child of the ancestor, rather than a child of the menu, it
    %% can continue to live even if the menu is deleted;  it will go
    %% away when the toplevel goes away.

    Parent = case menuParent(W) of
		 "." -> "";
		 W1 -> W1
	     end,
    Menu = mkTearOff(Parent, 1),
    tkMenuDup(W, Menu),
    tk:cmd(Menu, [configure, {transient, 0}]),

    %% Pick a title for the new menu by looking at the parent of the
    %% original: if the parent is a menu, then use the text of the active
    %% entry.  If it's a menubutton then use its text.

    Parent1 = tk:parentof(W),
    case tk:classof(Parent1) of
	"Menubutton" ->
	    tk:wm([title, Menu, tk:rcget(Parent1, text)]);
	"Menu" ->
	    tk:wm([title, Menu, tk:rcmd(Parent1, 
					[entrycget,active,{label}])])
    end,
    tk:cmd(Menu, [configure, {tearoff, 0}]),
    tk:cmd(Menu, [post, tk:winfo([x, W]), tk:winfo([y,W])]),

    %% Set tkPriv(focus) on entry:  otherwise the focus will get lost
    %% after keyboard invocation of a sub-menu (it will stay on the
    %% submenu).

    tk:bind(Menu, "<Enter>", ['%W'],
	    fun(M) -> ?tkset(focus, M) end),

    %% If there is a -tearoffcommand option for the menu, invoke it
    %% now.
    case tk:cget(W, tearoffcommand) of
	"" -> false;
	["invoke", Id | _] ->
	    Fun = tk:tkfun(Id),
	    Fun(W, Menu)
    end.


menuParent(W) ->
    Parent = tk:parentof(W),
    case tk:toplevelof(Parent) of
	Parent -> Parent;
	_ ->
	    case tk:classof(Parent) of
		"Menu" -> menuParent(Parent);
		_ -> Parent
	    end
    end.

mkTearOff(W, I) ->
    Menu = W ++ ".tearoff" ++ integer_to_list(I),
    case tk:winfo([exists, Menu]) of
	0 -> Menu;
	1 -> mkTearOff(W, I+1)
    end.
	    

%% tkMenuDup --
%% Given a menu (hierarchy), create a duplicate menu (hierarchy)
%% in a given window.
%%
%% Arguments:
%% src -			Source window.  Must be a menu.  It and its
%%			menu descendants will be duplicated at dst.
%% dst -			Name to use for topmost menu in duplicate
%%			hierarchy.

tkMenuDup(Src, Dst) ->
    Opts = lists:map(
	     fun(Option)  ->
		     if length(Option) > 2 ->
			     [tklib:index(0, Option), tklib:index(4, Option)];
			true ->
			     []
		     end
	     end,
	     tk:cmd(Src, [configure])),
    tk:menu(Dst, lists:append(Opts)),
    case tk:cmd(Src, [index, last]) of
	"none" -> false;
	Last ->
	    I = tk:cget(Src, tearoff),
	    tkMenuDup(Src, Dst, I, Last),

	    %% Duplicate the binding tags and bindings from the source menu.
	    Tags = tk:bindtags([Src]),
	    DstTags = lists:map(
			fun(X) when X == Src -> Dst;
			   (X) -> X
			end, tk:bindtags([Src])),
	    tk:bindtags(Dst, DstTags),
	    lists:foreach(
	      fun(Event) ->
		      case tk:bind(Src, Event) of
			  "" -> "";
			  ["event",Id | As] ->
			      Fun = tk:tkfun(Id),
			      tk:bind(Dst, Event, As, Fun)
		      end
	      end,
	      tk:bind(Src))
    end.


tkMenuDup(Src, Dst, I, Last) when I =< Last ->
    Opts = lists:map(
	     fun(Option) ->
		     {tklib:index(0, Option), tklib:index(4, Option)}
	     end,
	     tk:cmd(Src, [entryconfigure, I])),
    tk:cmd(Dst, [add,type, I | Opts]),
    case tk:cmd(Src, [type, I]) of
	"cascade" ->
	    SubMenu =  Dst ++ ".m" ++ integer_to_list(I),
	    tkMenuDup(tk:cmd(Src, [entrycget,I, {menu}]), SubMenu),
	    tk:cmd(Dst, [entryconfigure, I, {menu, SubMenu}]);
	_ ->
	    false
    end.
