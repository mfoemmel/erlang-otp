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
%% menu.tcl --
%%
%% This file defines the default bindings for Tk menus and menubuttons.
%% It also implements keyboard traversal of menus and implements a few
%% other utility procedures related to menus.
%%
%% SCCS: @(%%) menu.tcl 1.65 96/04/16 09:02:01
%%
%% Copyright (c) 1992-1994 The Regents of the University of California.
%% Copyright (c) 1994-1996 Sun Microsystems, Inc.
%%
%% See the file "license.terms" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
-module(tkmenu).

-include("tk.hrl").

-export([init/0, tk_popup/3, tk_popup/4]).

%%-------------------------------------------------------------------------
%% Elements of tkPriv that are used in this file:
%%
%% cursor -		Saves the -cursor option for the posted menubutton.
%% focus -		Saves the focus during a menu selection operation.
%%			Focus gets restored here when the menu is unposted.
%% grabGlobal -		Used in conjunction with tkPriv(oldGrab):  if
%%			tkPriv(oldGrab) is non-empty, then tkPriv(grabGlobal)
%%			contains either an empty string or "-global" to
%%			indicate whether the old grab was a local one or
%%			a global one.
%% inMenubutton -	The name of the menubutton widget containing
%%			the mouse, or an empty string if the mouse is
%%			not over any menubutton.
%% oldGrab -		Window that had the grab before a menu was posted.
%%			Used to restore the grab state after the menu
%%			is unposted.  Empty string means there was no
%%			grab previously set.
%% popup -		If a menu has been popped up via tk_popup, this
%%			gives the name of the menu.  Otherwise this
%%			value is empty.
%% postedMb -		Name of the menubutton whose menu is currently
%%			posted, or an empty string if nothing is posted
%%			A grab is set on this widget.
%% relief -		Used to save the original relief of the current
%%			menubutton.
%% window -		When the mouse is over a menu, this holds the
%%			name of the menu;  it's cleared when the mouse
%%			leaves the menu.
%%-------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% Overall note:
%% This file is tricky because there are four different ways that menus
%% can be used:
%%
%% 1. As a pulldown from a menubutton.  This is the most common usage.
%%    In this style, the variable tkPriv(postedMb) identifies the posted
%%    menubutton.
%% 2. As a torn-off menu copied from some other menu.  In this style
%%    tkPriv(postedMb) is empty, and the top-level menu is no
%%    override-redirect.
%% 3. As an option menu, triggered from an option menubutton.  In thi
%%    style tkPriv(postedMb) identifies the posted menubutton.
%% 4. As a popup menu.  In this style tkPriv(postedMb) is empty and
%%    the top-level menu is override-redirect.
%%
%% The various binding procedures use the  state described above to
%% distinguish the various cases and take different actions in each
%% case.
%%-------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% The code below creates the default class bindings for menus
%% and menubuttons.
%%-------------------------------------------------------------------------

mkpos(X) ->
    "@" ++ integer_to_list(X).

init() ->
    tk:bind("Menubutton", "<FocusIn>", [], fun() -> false end),
    tk:bind("Menubutton", "<Enter>", ['%W'], fun tkMbEnter/1),
    tk:bind("Menubutton", "<Leave>", ['%W'], fun tkMbLeave/1),
    tk:bind("Menubutton", "<1>", ['%W','%X','%Y'],
	    fun(W,X,Y) ->
		    case ?tkget(inMenubutton) of
			"" -> false;
			But ->
			    tkMbPost(But, X, Y)
		    end
	    end),
    tk:bind("Menubutton", "<Motion>", ['%W','%X','%Y'],
	    fun(W, X, Y) -> tkMbMotion(W,"up",X,Y) end),
    tk:bind("Menubutton", "<B1-Motion>", ['%W','%X','%Y'],
	    fun(W,X,Y) ->  tkMbMotion(W,"down",X,Y) end),
    tk:bind("Menubutton","<ButtonRelease-1>", ['%W'],
	    fun(W) -> tkMbButtonUp(W) end),
    tk:bind("Menubutton","<space>", ['%W'],
	    fun(W) -> tkMbPost(W),
		      tkMenuFirstEntry(tk:cget(W, menu))
	    end),

%% Must set focus when mouse enters a menu, in order to allow
%% mixed-mode processing using both the mouse and the keyboard.
%% Don't set the focus if the event comes from a grab release,
%% though:  such an event can happen after as part of unposting
%% a cascaded chain of menus, after the focus has already been
%% restored to wherever it was before menu selection started.

    tk:bind("Menu", "<FocusIn>", [], fun() -> false end),

    tk:bind("Menu", "<Enter>", ['%W', '%m'],
	    fun(W, M) ->
		  ?tkset(window, W),
		  case M of
		      "NotifyUngrab" ->
			  tk:focus([W]);
		      _ -> false
		  end
	    end),

    tk:bind("Menu", "<Leave>", ['%W','%X','%Y','%s'], fun tkMenuLeave/4),
    tk:bind("Menu", "<Motion>", ['%W','%y','%s'],fun tkMenuMotion/3),

    tk:bind("Menu", "<ButtonPress>",['%W'], fun tkMenuButtonDown/1),
    tk:bind("Menu", "<ButtonRelease>", ['%W'],
	    fun(W) ->  tkMenuInvoke(W, 1) end),
    tk:bind("Menu", "<space>", ['%W'],
	    fun(W) ->  tkMenuInvoke(W, 0) end),
    tk:bind("Menu", "<Return>", ['%W'],
	    fun(W) ->  tkMenuInvoke(W, 0) end),
    tk:bind("Menu", "<Escape>", ['%W'], fun tkMenuEscape/1),
    tk:bind("Menu", "<Left>",  ['%W'],
	    fun(W) -> tkMenuLeftRight(W, left) end),
    tk:bind("Menu", "<Right>", ['%W'],
	    fun(W) -> tkMenuLeftRight(W, right) end),
    tk:bind("Menu", "<Up>", ['%W'],
	    fun(W) -> tkMenuNextEntry(W, -1) end),
    tk:bind("Menu", "<Down>", ['%W'],
	    fun(W) -> tkMenuNextEntry(W, 1) end),
    tk:bind("Menu", "<KeyPress>", ['%W','%A'],
	    fun(W,A) -> tkTraverseWithinMenu(W,A) end),


%% The following bindings apply to all windows, and are used to
%% implement keyboard menu traversal.

    tk:bind("all", "<Alt-KeyPress>", ['%W','%A'], fun tkTraverseToMenu/2),
    tk:bind("all", "<F10>", ['%W'], fun tkFirstMenu/1).


%% tkMbEnter --
%% This procedure is invoked when the mouse enters a menubutton
%% widget.  It activates the widget unless it is disabled.  Note:
%% this procedure is only invoked when mouse button 1 is *not* down.
%% The procedure tkMbB1Enter is invoked if the button is down.
%%
%% Arguments:
%% w -			The  name of the widget.

tkMbEnter(W) ->
    tkMbLeave(?tkget(inMenubutton)),
    ?tkset(inMenubutton, W),
    case tk:cget(W, state) of
	"disabled" -> false;
	_ ->
	    tk:cmd(W, [configure, {state, active}])
    end.

%% tkMbLeave --
%% This procedure is invoked when the mouse leaves a menubutton widget.
%% It de-activates the widget, if the widget still exists.
%%
%% Arguments:
%% w -			The  name of the widget.

tkMbLeave("") ->
    false;
tkMbLeave(W) ->
    ?tkset(inMenubutton, ""),
    case tk:winfo([exists, W]) of
	1 ->
	    case tk:cget(W, state) of
		"active" ->
		    tk:cmd(W, [configure, {state, normal}]);
		_ -> false
	    end;
	0 ->
	    false
    end.

%% tkMbPost --
%% Given a menubutton, this procedure does all the work of posting
%% its associated menu and unposting any other menu that is currently
%% posted.
%%
%% Arguments:
%% w -			The name of the menubutton widget whose menu
%%			is to be posted.
%% x, y -		Root coordinates of cursor, used for positioning
%%			option menus.  If not specified, then the center
%%			of the menubutton is used for an option menu.
tkMbPost(W) ->
    mbPost1(W, "", "").

tkMbPost(W, X, Y)  ->
    mbPost1(W, X, Y).

mbPost1(W, X, Y) ->
    Cur = ?tkget(postedMb),
    case tk:cget(W, state) of
	"disabled" -> false;
	_ when Cur == W -> false;
	_ ->
	    case tk:cget(W, menu) of
		"" -> false;
		Menu ->
		    case lists:prefix(W, Menu) of
			false ->
			    exit({error , "can't post "++Menu ++
				  "it isn't a descendant of " ++ W});
			true -> 
			    mbPost2(W, X, Y, Cur, Menu)
		    end
	    end
    end.

mbPost2(W, X, Y, Cur, Menu) ->
    if Cur =/= "" ->
	    tkMenuUnpost("");
       true -> false
    end,
    ?tkset(cursor, tk:cget(W, cursor)),
    ?tkset(relief, tk:cget(W, relief)),
    tk:cmd(W, [configure, {cursor, "arrow"}]),
    tk:cmd(W, [configure, {relief, "raised"}]),
    ?tkset(postedMb, W),
    ?tkset(focus, tk:focus([])),
    tk:cmd(Menu, [activate, none]),

    %% If this looks like an option menubutton then post the menu so
    %% that the current entry is on top of the mouse.  Otherwise post
    %% the menu just below the menubutton, as for a pull-down.

    case tk:cget(W, indicatoron) of
	1 when Y == "" ->
	    X1 = tk:winfo([rootx, W]) + tk:winfo([width, W])/2,
	    Y1 = tk:winfo([rooty, W]) + tk:winfo([height,W])/2,
	    Name = tkMenuFindName(Menu, tk:rcget(W, text)),
	    tkPostOverPoint(Menu, X1,Y1, Name);
	1 ->
	    Name = tkMenuFindName(Menu, tk:rcget(W, text)),
	    tkPostOverPoint(Menu, X,Y, Name);
	0 ->
	    tk:cmd(Menu,[post, 
			    tk:winfo([rootx,W]),
			    tk:winfo([rooty,W]) +
			    tk:winfo([height,W])])
    end,
    tk:focus([Menu]),
    tkSaveGrabInfo(W),
    tk:grab([{global,W}]).

%% tkMenuUnpost --
%% This procedure unposts a given menu, plus all of its ancestors up
%% to (and including) a menubutton, if any.  It also restores various
%% values to what they were before the menu was posted, and releases
%% a grab if there's a menubutton involved.  Special notes:
%% 1. It's important to unpost all menus before releasing the grab, so
%%    that any Enter-Leave events (e.g. from menu back to main
%%    application) have mode NotifyGrab.
%% 2. Be sure to enclose various groups of commands in "catch" so that
%%    the procedure will complete even if the menubutton or the menu
%%    or the grab window has been deleted.
%%
%% Arguments:
%% menu -		Name of a menu to unpost.  Ignored if there
%%			is a posted menubutton.

tkMenuUnpost(Menu) ->
    Mb = ?tkget(postedMb),

    %% Restore focus right away (otherwise X will take focus away when
    %% the menu is unmapped and under some window managers (e.g. olvwm)
    %% we'll lose the focus completely).

    catch tk:focus([?tkget(focus)]),
    ?tkset(focus, ""),

    %% Unpost menu(s) and restore some stuff that's dependent on
    %% what was posted.

    Res =
	(catch
	    begin
		if
		    Mb =/= "" ->
			Res0 = tk:cget(Mb, menu),
			tk:cmd(Res0, ["unpost"]),
			?tkset(postedMb, ""),
			tk:cmd(Mb,[configure,{cursor,?tkget(cursor)}]),
			tk:cmd(Mb,[configure,{relief,?tkget(relief)}]),
			Res0;
		    true ->
			Popup = ?tkget(popup),
			if
			    Popup =/= "" ->
				tk:cmd(Popup, [unpost]),
				?tkset(popup, ""),
				Menu;
			    true ->
				case tk:wm([overrideredirect,Menu]) of
				    1 ->
					Res0 = unpostCascade(Menu),
					tk:cmd(Res0, [unpost]),
					Res0;
				    0 ->
					Menu
				end
			end
		end
	    end),
    Menu1 = 
	case Res of
	    {'EXIT', _} -> Menu;
	    _ -> Res
	end,

    %% Release grab, if any, and restore the previous grab, if there
    %% was one.

    if
	Menu1 =/= "" ->
	    case tk:grab([current, Menu1]) of
		"" -> false;
		Grab ->
		    tk:grab([release, Grab])
	    end;
	true ->
	    false
    end,
    case  ?tkget(oldGrab) of
	"" -> false;
	OldGrab ->
	    %% Be careful restoring the old grab, since it's window may not
	    %% be visible anymore.
	    catch 
		begin
		    case ?tkget(grabStatus) of
			"global" ->
			    tk:grab([set, {global,OldGrab}]);
			_ ->
			    tk:grab([set, OldGrab])
		    end
		end,
	    ?tkset(oldGrab, "")
    end.

%% We're in a cascaded sub-menu from a torn-off menu or popup.
%% Unpost all the menus up to the toplevel one (but not
%% including the top-level torn-off one) and deactivate the
%% top-level torn off menu if there is one.

unpostCascade(Menu) ->
    Parent = tk:parentof(Menu),
    case tk:classof(Parent) of
	"Menu" ->
	    case tk:winfo([ismapped, Parent]) of
		0 -> Menu;
		1 ->
		    tk:cmd(Parent, [activate, none]),
		    tk:cmd(Parent, [postcascade, none]),
		    case tk:wm([overrideredirect, Parent]) of
			0 -> Menu;
			1 -> unpostCascade(Parent)
		    end
	    end;
	_ ->
	    Menu
    end.

%% tkMbMotion --
%% This procedure handles mouse motion events inside menubuttons, and
%% also outside menubuttons when a menubutton has a grab (e.g. when a
%% menu selection operation is in progress).
%%
%% Arguments:
%% w -			The name of the menubutton widget.
%% upDown - 		"down" means button 1 is pressed, "up" means
%%			it isn't.
%% rootx, rooty -	Coordinates of mouse, in (virtual?) root window.

tkMbMotion(W, UpDown, Rootx, Rooty) ->
    case ?tkget(inMenubutton) of
	W -> false;
	Mb ->
	    mbMotion1(W, Mb, UpDown, Rootx, Rooty)
    end.

mbMotion1(W, Mb, UpDown,Rootx,Rooty) ->
    New = tk:winfo([containing, Rootx, Rooty]),
    if New == Mb -> false;
       New == "" -> tkMbLeave(Mb);
       true ->
	    NT = tk:toplevelof(New),
	    WT = tk:toplevelof(W),
	    if
		NT == WT ->
		    if Mb == "" -> false;
		       true -> tkMbLeave(Mb)
		    end,
		    mbMotion2(W, New, UpDown,Rootx,Rooty);
		true ->
		    false
	    end
    end.
		
mbMotion2(W, New, UpDown,Rootx,Rooty) ->
    case tk:classof(New) of
	"Menubutton" ->
	    IN = tk:cget(New, indicatoron),
	    IW =  tk:cget(W, indicatoron),
	    if IN == 0, IW == 0 ->
		    if UpDown == "down" ->
			    tkMbPost(New,Rootx,Rooty);
		       true ->
			    tkMbEnter(New)
		    end;
	       true ->
		    false
	    end;
	_ ->
	    false
    end.

%% tkMbButtonUp --
%% This procedure is invoked to handle button 1 releases for menubuttons.
%% If the release happens inside the menubutton then leave its menu
%% posted with element 0 activated.  Otherwise, unpost the menu.
%%
%% Arguments:
%% w -			The name of the menubutton widget.

tkMbButtonUp(W) ->
    PostedMb = ?tkget(postedMb),
    Mb = ?tkget(inMenubutton),
    if
	PostedMb == W, Mb == W ->
	    tkMenuFirstEntry(tk:cget(PostedMb, menu));
	true ->
	    tkMenuUnpost("")
    end.

%% tkMenuMotion --
%% This procedure is called to handle mouse motion events for menus.
%% It does two things.  First, it resets the active element in the
%% menu, if the mouse is over the menu.  Second, if a mouse button
%% is down, it posts and unposts cascade entries to match the mouse
%% position.
%%
%% Arguments:
%% menu -		The menu window.
%% y -			The y position of the mouse.
%% state -		Modifier state (tells whether buttons are down).

tkMenuMotion(Menu,Y,State) ->
    Window = ?tkget(window),
    if 
	Menu == Window ->
	    tk:cmd(Menu, [activate, mkpos(Y)]);
	true  -> false
    end,
    if
	State band 16#1f00 =/= 0 ->
	    tk:cmd(Menu, [postcascade, active]);
	true -> false
    end.

%% tkMenuButtonDown --
%% Handles button presses in menus.  There are a couple of tricky things
%% here:
%% 1. Change the posted cascade entry (if any) to match the mouse position.
%% 2. If there is a posted menubutton, must grab to the menubutton;  this
%%    overrrides the implicit grab on button press, so that the menu
%%    button can track mouse motions over other menubuttons and change
%%    the posted menu.
%% 3. If there's no posted menubutton (e.g. because we're a torn-off menu
%%    or one of its descendants) must grab to the top-level menu so that
%%    we can track mouse motions across the entire menu hierarchy.
%%
%% Arguments:
%% menu -		The menu window.

tkMenuButtonDown(Menu) ->
    tk:cmd(Menu, [postcascade, active]),
    case ?tkget(postedMb) of
	"" -> 
	    Menu1 = menuDown(Menu),
	    case tk:grab([current, Menu1]) of
		Menu1 -> false;
		_ -> tkSaveGrabInfo(Menu1)
	    end,
	    tk:grab([{global, Menu1}]);
	Posted ->
	    tk:grab([{global, Posted}])
    end.

menuDown(Menu) ->
    case tk:wm([overrideredirect, Menu]) of
	1 ->
	    Parent = tk:parentof(Menu),
	    case tk:classof(Parent) of
		"Menu" ->
		    case tk:winfo([ismapped, Parent]) of
			1 -> menuDown(Menu);
			0 -> Menu
		    end;
		_ ->
		    Menu
	    end;
	0 ->
	    Menu
    end.

%% tkMenuLeave --
%% This procedure is invoked to handle Leave events for a menu.  It
%% deactivates everything unless the active element is a cascade element
%% and the mouse is now over the submenu.
%%
%% Arguments:
%% menu -		The menu window.
%% rootx, rooty -	Root coordinates of mouse.
%% state -		Modifier state.

tkMenuLeave(Menu,Rootx,Rooty,State) ->
    ?tkset(window, ""),
    case tk:cmd(Menu, [index, active]) of
	"none" -> false;
	_ ->
	    case tk:cmd(Menu, [type, active]) of
		"cascade" ->
		    Cont = tk:winfo([containing, Rootx, Rooty]),
		    M = tk:cmd(Menu, [entrycget, active, {menu}]),
		    if
			M == Cont -> false;
			true ->
			    tk:cmd(Menu, [activate, none])
		    end;
		_ ->
		    tk:cmd(Menu, [activate, none])
	    end
    end.

%% tkMenuInvoke --
%% This procedure is invoked when button 1 is released over a menu.
%% It invokes the appropriate menu action and unposts the menu if
%% it came from a menubutton.
%%
%% Arguments:
%% w -			Name of the menu widget.
%% buttonRelease -	1 means this procedure is called because of
%%			a button release;  0 means because of keystroke.

tkMenuInvoke(W, ButtonRelease) ->
    Win = ?tkget(window),
    if
	ButtonRelease == 1, Win == "" ->
	    %% Mouse was pressed over a menu without a menu button, then
	    %% dragged off the menu (possibly with a cascade posted) and
	    %% released.  Unpost everything and quit.
	    tk:cmd(W, [postcascade, none]),
	    tk:cmd(W, [activate, none]),
	    tkMenuUnpost(W);
	true ->
	    case tk:cmd(W, [type, active]) of
		"cascade" ->
		    tk:cmd(W, [postcascade, active]),
		    Menu = tk:cmd(W, [entrycget, active,{menu}]),
		    tkMenuFirstEntry(Menu);
		"tearoff" ->
		    tkMenuUnpost(W),
		    tktearoff:tkTearOffMenu(W);
	       _ ->
		    tkMenuUnpost(W),
		    tk:cmd(W, [invoke, active])
	    end
    end.


%% tkMenuEscape --
%% This procedure is invoked for the Cancel (or Escape) key.  It unposts
%% the given menu and, if it is the top-level menu for a menu button,
%% unposts the menu button as well.
%%
%% Arguments:
%% menu -		Name of the menu window.

tkMenuEscape(Menu) ->
    Parent = tk:parentof(Menu),
    case tk:classof(Parent) of
	"Menu" ->
	    tkMenuLeftRight(Menu, left);   %% Fix a BUG "left" was -1
	_ ->
	    tkMenuUnpost(Menu)
    end.

%% tkMenuLeftRight --
%% This procedure is invoked to handle "left" and "right" traversal
%% motions in menus.  It traverses to the next menu in a menu bar,
%% or into or out of a cascaded menu.
%%
%% Arguments:
%% menu -		The menu that received the keyboard
%%			event.
%% direction -		Direction in which to move: "left" or "right"
tkMenuLeftRight(Menu, right) ->
    case tk:cmd(Menu, [type, active]) of
	"cascade" ->
	    tk:cmd(Menu, [postcascade, active]),
	    case tk:cmd(Menu, [entrycget,active,{menu}]) of
		"" -> 
		    false;
		M2 ->
		    tkMenuFirstEntry(M2)
	    end;
	_ ->
	    leftRight(Menu, 1)
    end;
tkMenuLeftRight(Menu, left) ->
    %% First handle traversals into and out of cascaded menus.
    M2 = tk:parentof(Menu),
    case tk:classof(M2) of
	"Menu" ->
	    tk:cmd(Menu, [activate, none]),
	    tk:focus([M2]),
	    Tmp = tk:cmd(M2, [index, active]),
	    tk:cmd(M2, [activate, none]),
	    tk:cmd(M2, [activate, Tmp]);
	_ ->
	    leftRight(Menu, -1)
    end.

mod(X, N) ->
    ((X rem N) + N) rem N.

leftRight(Menu, Count) ->
    case ?tkget(postedMb) of
	"" -> false;
	W -> leftRight(W, Menu, Count)
    end.

leftRight(W, Menu, Count) ->
    %% Can't traverse into or out of a cascaded menu.  Go to the next
    %% or previous menubutton, if that makes sense.
    Parent = tk:parentof(W),
    Buttons = tk:childrenof(Parent),
    N = length(Buttons),
    I = tklib:search(Buttons, W) + Count,
    case leftRight(W, Buttons, mod(I, N), N, Count) of
	"" ->
	    false;
	Mb ->
	    tkMbPost(Mb),
	    tkMenuFirstEntry(tk:cget(Mb, menu))
    end.
			
leftRight(W, Buttons, I, N, Count) ->
    Mb = tklib:index(I, Buttons),
    case tk:classof(Mb) of
	"Menubutton" ->
	    case tk:cget(Mb, state) of
		"disabled" ->
		    leftRightNext(W,Mb,Buttons,I,N,Count);
		_ ->
		    case tk:cget(Mb, menu) of
			"" ->
			    leftRightNext(W,Mb,Buttons,I,N,Count);
			_ ->
			    Menu = tk:cget(Mb, menu),
			    case tk:cmd(Menu, [index, last]) of
				"none" ->
				    leftRightNext(W,Mb,Buttons,I,N,Count);
				_ ->
				    Mb
			    end
		    end
	    end;
	_ -> ""
    end.

leftRightNext(W, W, _, _, _,_) -> 
    "";
leftRightNext(W, _, Buttons, I, N,Count) ->
    leftRight(W, Buttons, mod(I+Count,N), N, Count).

%% tkMenuNextEntry --
%% Activate the next higher or lower entry in the posted menu,
%% wrapping around at the ends.  Disabled entries are skipped.
%%
%% Arguments:
%% menu -			Menu window that received the keystroke.
%% count -			1 means go to the next lower entry,
%%				-1 means go to the next higher entry.

tkMenuNextEntry(Menu, Count) ->
    case tk:cmd(Menu, [index, last]) of
	"none" -> false;
	N0 ->
	    N = N0 + 1,
	    QuitAfter = N,
	    Active = tk:cmd(Menu, [index, active]),
	    I = if Active == "none" -> 0;
		   true -> Active + Count
		end,
	    case menuNext(Menu, mod(I,N), Count, Active, QuitAfter) of
		-1 -> false;
		I1 ->
		    tk:cmd(Menu, [activate, I1]),
		    tk:cmd(Menu, [postcascade, I1])
	    end
    end.

menuNext(Menu, I, Count, A, N) when N =< 0 -> 
    -1;
menuNext(Menu, I, Count, A, N) ->
    case catch tk:cmd(Menu, [entrycget, I, {state}]) of
	State when list(State), State =/= "disabled" ->
	    I;
	_ ->
	    if
		A == I -> -1;
		true ->
		    menuNext(Menu, mod(I+Count,N), Count, A, N-1)
	    end
    end.

%% tkMenuFind --
%% This procedure searches the entire window hierarchy under w for
%% a menubutton that isn't disabled and whose underlined character
%% is "char".  It returns the name of that window, if found, or an
%% empty string if no matching window was found.  If "char" is an
%% empty string then the procedure returns the name of the first
%% menubutton found that isn't disabled.
%%
%% Arguments:
%% w -				Name of window where key was typed.
%% char -			Underlined character to search for;
%%				may be either upper or lower case, and
%%				will match either upper or lower case.
tkMenuFind(W, Char0) ->
    tkMenuFind1(W, tklib:tolower(Char0)).

tkMenuFind1(W, Char) ->
    tklib:while(
      fun(Child) ->
	      case tk:classof(Child) of
		  "Menubutton" ->
		      U = tk:cget(Child, underline),
		      Text = tk:rcget(Child, text),
		      Char2 = tklib:tolower(tklib:index(U, Text)),
		      if
			  Char =/= Char2, Char =/= "" -> true;
			  true ->
			      case tk:cget(Child, state) of
				  "disabled" -> true;
				  _ -> {false, Child}
			      end
		      end;
		  _ ->
		      case tkMenuFind(Child, Char) of
			  "" -> true;
			  Match -> {false, Match}
		      end
	      end
      end,
      tk:childrenof(W)).


%% tkTraverseToMenu --
%% This procedure implements keyboard traversal of menus.  Given an
%% ASCII character "char", it looks for a menubutton with that character
%% underlined.  If one is found, it posts the menubutton's menu
%%
%% Arguments:
%% w -				Window in which the key was typed (selects
%%				a toplevel window).
%% char -			Character that selects a menu.  The case
%%				is ignored.  If an empty string, nothing
%%				happens.
tkTraverseToMenu(W, "") ->
    false;
tkTraverseToMenu(W, Char) ->
    case traverseToMenu(W) of
	false -> false;
	W1 ->
	    case tkMenuFind(tk:toplevelof(W1), Char) of
		"" -> false;
		W2 ->
		    tkMbPost(W2),
		    tkMenuFirstEntry(tk:cget(W, menu))
	    end
    end.

traverseToMenu(W) ->
    case tk:classof(W) of
	"Menu" ->
	    case ?tkget(postedMb) of
		"" -> 
		    false;
		_ ->
		    traverseToMenu(tk:parentof(W))
	    end;
	_ -> W
    end.

%% tkFirstMenu --
%% This procedure traverses to the first menubutton in the toplevel
%% for a given window, and posts that menubutton's menu.
%%
%% Arguments:
%% w -				Name of a window.  Selects which toplevel
%%				to search for menubuttons.

tkFirstMenu(W) ->
    case  tkMenuFind(tk:toplevelof(W), "") of
	"" -> false;
	W1 ->
	    tkMbPost(W1),
	    tkMenuFirstEntry(tk:cget(W1, menu))
    end.

%% tkTraverseWithinMenu
%% This procedure implements keyboard traversal within a menu.  It
%% searches for an entry in the menu that has "char" underlined.  If
%% such an entry is found, it is invoked and the menu is unposted.
%%
%% Arguments:
%% w -				The name of the menu widget.
%% char -			The character to look for;  case is
%%				ignored.  If the string is empty then
%%				nothing happens.

tkTraverseWithinMenu(W, "") ->
    false;
tkTraverseWithinMenu(W, Char0) ->
    Char = tklib:tolower(Char0),
    case tk:cmd(W, [index,last]) of
	"none" -> false;
	Last ->
	    tklib:while(
	      fun(I) ->
		      case catch
			  tklib:index(tk:cmd(W, [entrycget,I,{underline}]),
				      tk:rcmd(W, [entrycget,I,{label}])) of
			  {'EXIT', _} ->
			      true;
			  Char2 ->
			      case tklib:tolower(Char2) of
				  Char ->
				      case tk:cmd(W, [type, I]) of
					  "cascade" ->
					      tk:cmd(W, [postcascade,I]),
					      tk:cmd(W, [activate, I]),
					      case tk:cmd(W,[entrycget,I,{menu}]) of
						  "" ->
						      false;
						  M2 ->
						      tkMenuFirstEntry(M2),
						      false
					      end;
					  _ ->
					      tkMenuUnpost(W),
					      tk:cmd(W, [invoke, I]),
					      false
				      end;
				  _ ->
				      true
			      end
		      end
	      end,
	      lists:seq(0, Last))
    end.

%% tkMenuFirstEntry --
%% Given a menu, this procedure finds the first entry that isn't
%% disabled or a tear-off or separator, and activates that entry.
%% However, if there is already an active entry in the menu (e.g.,
%% because of a previous call to tkPostOverPoint) then the active
%% entry isn't changed.  This procedure also sets the input focus
%% to the menu.
%%
%% Arguments:
%% menu -		Name of the menu window (possibly empty).

tkMenuFirstEntry("") -> 
    false;
tkMenuFirstEntry(Menu) ->
    tk:focus([Menu]),
    case tk:cmd(Menu, [index, active]) of
	"none" -> false;
	_ ->
	    case tk:cmd(Menu,[index, last]) of
		"none" -> false;
		Last -> 
		    menuFirst(0, Last, Menu)
	    end
    end.
	    
menuFirst(I, Last, Menu) when I > Last -> 
    false;
menuFirst(I, Last, Menu) ->
    case catch tk:cmd(Menu, [entrycget, I, {state}]) of
	{'EXIT', _} ->
	    menuFirst(I+1, Last, Menu);
	"disabled" ->
	    menuFirst(I+1, Last, Menu);
	State ->
	    case tk:cmd(Menu, [type, I]) of
		"tearoff" ->
		    menuFirst(I+1, Last, Menu);
		_ ->
		    tk:cmd(Menu, [activate, I]),
		    true
	    end
    end.

%% tkMenuFindName --
%% Given a menu and a text string, return the index of the menu entry
%% that displays the string as its label.  If there is no such entry,
%% return an empty string.  This procedure is tricky because some names
%% like "active" have a special meaning in menu commands, so we can't
%% always use the "index" widget command.
%%
%% Arguments:
%% menu -		Name of the menu widget.
%% s -			String to look for.

tkMenuFindName(Menu, S) ->
    I = case regexp:match(S, "^active$|^last$|^none$|^[0-9]|^@") of
	    nomatch ->
		case catch tk:cmd(Menu, [index, S]) of
		    {'EXIT', _} -> "";
		    Ix -> Ix
		end;
	    _ -> ""
	end,
    case tk:cmd(Menu, [index, last]) of
	"none" -> false;
	Last ->
	    menuFind(0, Last, Menu, S)
    end.

menuFind(I, Last, Menu, S) when I > Last -> 
    "";
menuFind(I, Last, Menu, S) ->
    case catch tk:cmd(Menu, [entrycget, I]) of
	S -> I;
	_ -> menuFind(I+1, Last, Menu, S)
    end.

%% tkPostOverPoint --
%% This procedure posts a given menu such that a given entry in the
%% menu is centered over a given point in the root window.  It also
%% activates the given entry.
%%
%% Arguments:
%% menu -		Menu to post.
%% x, y -		Root coordinates of point.
%% entry -		Index of entry within menu to center over (x,y).
%%			If omitted or specified as {}, then the menu's
%%			upper-left corner goes at (x,y).
tkPostOverPoint(Menu,X,Y,"") ->
    tk:cmd(Menu, [post, X, Y]);
tkPostOverPoint(Menu,X0,Y0,Entry) ->
    Y = case tk:cmd(Menu, [index, last]) of
	    Entry ->
		Y0 + -tk:cmd(Menu, [yposition, Entry]) +
		    tk:winfo([reqheight, Menu])/2;
	     _ ->
		Y0 + -tk:cmd(Menu, [yposition, Entry]) +
		    tk:cmd(Menu, [yposition, Entry+1]) / 2
	end,
    X = X0 + -tk:winfo([reqwidth, Menu]) / 2,
    tk:cmd(Menu, [post, X, Y]),
    case tk:cmd(Menu, [entrycget, Entry, {state}]) of
	"disabled" -> false;
	_ ->
	    tk:cmd(Menu, [activate, Entry])
    end.

%% tkSaveGrabInfo --
%% Sets the variables tkPriv(oldGrab) and tkPriv(grabStatus) to record
%% the state of any existing grab on the w's display.
%%
%% Arguments:
%% w -			Name of a window;  used to select the display
%%			whose grab information is to be recorded.

tkSaveGrabInfo(W) ->
    case tk:grab([current, W]) of
	"" -> false;
	OldGrab ->
	    ?tkset(grabStatus, tk:grab([status, OldGrab]))
    end.

%% tk_popup --
%% This procedure pops up a menu and sets things up for traversing
%% the menu and its submenus.
%%
%% Arguments:
%% menu -		Name of the menu to be popped up.
%% x, y -		Root coordinates at which to pop up the
%%			menu.
%% entry -		Index of a menu entry to center over (x,y).
%%			If omitted or specified as {}, then menu's
%%			upper-left corner goes at (x,y).

tk_popup(Menu,X,Y) ->
    tk_popup(Menu,X,Y,"").

tk_popup(Menu,X,Y,Entry) ->
    case ?tkget(popup) of
	"" ->
	    case ?tkget(postedMb) of
		"" -> false;
		_  -> tkMenuUnpost("")
	    end;
	_ -> 
	    tkMenuUnpost("")
    end,
    tkPostOverPoint(Menu, X, Y, Entry),
    tkSaveGrabInfo(Menu),
    tk:grab([{global,Menu}]),
    ?tkset(popup, Menu),
    ?tkset(focus, tk:focus([])),
    tk:focus([Menu]).



