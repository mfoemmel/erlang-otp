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
%% entry.tcl --
%%
%% This file defines the default bindings for Tk entry widgets and provides
%% procedures that help in implementing those bindings.
%%
%% @(#) entry.tcl 1.36 95/06/17 17:47:29
%%
%% Copyright (c) 1992-1994 The Regents of the University of California.
%% Copyright (c) 1994-1995 Sun Microsystems, Inc.
%%
%% See the file "license.terms" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

%%-------------------------------------------------------------------------
%% Elements of tkPriv that are used in this file:
%%
%% afterId -		If non-null, it means that auto-scanning is underway
%%			and it gives the "after" id for the next auto-scan
%%			command to be executed.
%% mouseMoved -		Non-zero means the mouse has moved a significant
%%			amount since the button went down (so, for example,
%%			start dragging out a selection).
%% pressX -		X-coordinate at which the mouse button was pressed.
%% selectMode -		The style of selection currently underway:
%%			char, word, or line.
%% x, y -		Last known mouse coordinates for scanning
%%			and auto-scanning.
%%-------------------------------------------------------------------------

-module(tkentry).

-export([init/0]).

-include("tk.hrl").

bind(Event, Template, Fun) ->
    tk:bind("Entry", Event, Template, Fun).

bindw(Event, Fun) ->
    tk:bind("Entry", Event, ['%W'], Fun).

bindwx(Event, Fun) ->
    tk:bind("Entry", Event, ['%W','%x'], Fun).

strict_bindw(Event, Fun) ->
    tk:bind("Entry", Event, ['%W'],
	    fun(W) ->
		    case ?tkget(tk_strictMotif) of
			false -> Fun(W);
			true  -> false
		    end
	    end).

strict_bindwx(Event, Fun) ->
    tk:bind("Entry", Event, ['%W','%x'],
	    fun(W,X) ->
		    case ?tkget(tk_strictMotif) of
			false -> Fun(W,X);
			true  -> false
		    end
	    end).

mkpos(X) ->
    "@" ++ integer_to_list(X).

%%-------------------------------------------------------------------------
%% The code below creates the default class bindings for entries.
%%-------------------------------------------------------------------------

%% init Entry bindings

init() ->
    bindw("<<Cut>>", fun (W) -> tktext:tk_textCut(W) end),
    bindw("<<Copy>>", fun (W) -> tktext:tk_textCopy(W) end),
    bindw("<<Paste>>", fun (W) -> tktext:tk_textPaste(W) end),
    bindw("<<Clear>>",fun(W) -> tk:cmd(W,[delete,"sel.first","sel.last"]) end),

    bindwx("<1>",
	 fun(W,X) ->
		 tkEntryButton1(W, X),
		 tk:cmd(W, [selection, clear])
	 end),
    bindwx("<B1-Motion>",
	 fun(W,X) ->
		 ?tkset(x, X),
		 tkEntryMouseSelect(W, X)
	 end),
    bindwx("<Double-1>",
	 fun(W,X) ->
		 ?tkset(selectMode, word),
		 tkEntryMouseSelect(W, X),
		 catch tk:cmd(W, [icursor, "sel.first"])
	 end),
    bindwx("<Triple-1>",
	 fun(W,X) ->
		 ?tkset(selectMode, line),
		 tkEntryMouseSelect(W, X),
		 tk:cmd(W, [icursor, 0])
	 end),
    bindwx("<Shift-1>",
	 fun(W,X) ->
		 ?tkset(selectMode, char),
		 tk:cmd(W, [selection, adjust, mkpos(X)])
	 end),
    bindwx("<Double-Shift-1>",
	 fun(W,X) ->
		 ?tkset(selectMode, word),
		 tkEntryMouseSelect(W, X)
	 end),
    bindwx("<Triple-Shift-1>",
	 fun(W,X) ->
		 ?tkset(selectMode, line),
		 tkEntryMouseSelect(W, X)
	 end),
    bindwx("<B1-Leave>",
	 fun(W,X) ->
		 ?tkset(x, X),
		 tkEntryAutoScan(W)
	 end),

    bind("<B1-Enter>", [], fun() -> tk:tkCancelRepeat() end),
    bind("<ButtonRelease-1>", [], fun() -> tk:tkCancelRepeat() end),

    bindwx("<Control-1>",
	 fun(W,X) ->
		 tk:cmd(W,[icursor, mkpos(X)])
	 end),
    bindwx("<ButtonRelease-2>",
	 fun(W,X) ->
		 case ?tkget(mouseMoved) of
		     0 -> 
			 tkEntryPaste(W, X);
		     _ ->
			 case ?tkget(tk_strictMotif) of
			     true ->
				 tkEntryPaste(W, X);
			     _ -> false
			 end
		 end
	 end),
    bindw("<Left>", 
	 fun(W) ->
		 Ix = tk:cmd(W, [index, insert]),
		 tkEntrySetCursor(W, Ix - 1)
	 end),
    bindw("<Right>", 
	 fun(W) ->
		 Ix = tk:cmd(W, [index, insert]),
		 tkEntrySetCursor(W, Ix + 1)
	 end),
    bindw("<Shift-Left>", 
	 fun(W) ->
		 Ix = tk:cmd(W, [index, insert]),
		 tkEntrySetCursor(W, Ix - 1),
		 tkEntrySeeInsert(W)
	 end),
    bindw("<Shift-Right>", 
	 fun(W) ->
		 Ix = tk:cmd(W, [index, insert]),
		 tkEntrySetCursor(W, Ix + 1),
		 tkEntrySeeInsert(W)
	 end),
    bindw("<Control-Left>", 
	 fun(W) ->
		 String = tk:rcmd(W, [get]),
		 Ix = tk:cmd(W, [index, insert]),
		 WIx = tklib:wordstart(String, Ix - 1),
		 tkEntrySetCursor(W, WIx)
	 end),
    bindw("<Control-Right>", 
	 fun(W) ->
		 String = tk:rcmd(W, [get]),
		 Ix = tk:cmd(W, [index, insert]),
		 WIx = tklib:wordend(String, Ix),
		 tkEntrySetCursor(W, WIx)
	 end),
    bindw("<Shift-Control-Left>", 
	 fun(W) ->
		 String = tk:rcmd(W, [get]),
		 Ix = tk:cmd(W, [index, insert]),
		 WIx = tklib:wordstart(String, Ix - 1),
		 tkEntryKeySelect(W, WIx),
		 tkEntrySeeInsert(W)
	 end),
    bindw("<Shift-Control-Right>", 
	 fun(W) ->
		 String = tk:rcmd(W, [get]),
		 Ix = tk:cmd(W, [index, insert]),
		 WIx = tklib:wordend(String, Ix),
		 tkEntryKeySelect(W, WIx),
		 tkEntrySeeInsert(W)
	 end),
    bindw("<Home>",
	    fun(W) ->
		    tkEntrySetCursor(W, 0)
	    end),
    bindw("<Shift-Home>",
	    fun(W) ->
		    tkEntrySetCursor(W, 0),
		    tkEntrySeeInsert(W)
	    end),
    bindw("<End>",
	    fun(W) ->
		    tkEntrySetCursor(W, "end")
	    end),
    bindw("<Shift-End>",
	    fun(W) ->
		    tkEntrySetCursor(W, "end"),
		    tkEntrySeeInsert(W)
	    end),
    bindw("<Delete>",
	    fun(W) ->
		    case tk:cmd(W, [selection, present]) of
			1 ->
			    tk:cmd(W, [delete, "sel.first", "sel.last"]);
			0 ->
			    tk:cmd(W, [delete, insert])
		    end
	    end),
    bindw("<BackSpace>",
	    fun(W) ->
		    tkEntryBackspace(W)
	    end),
    bindw("<Control-space>",
	    fun(W) ->
		    tk:cmd(W,[selection,from,insert])
	    end),
    bindw("<Select>",
	    fun(W) ->
		    tk:cmd(W,[selection, from, insert])
	    end),
    bindw("<Control-Shift-space>",
	    fun(W) ->
		    tk:cmd(W,[selection, adjust, insert])
	    end),
    bindw("<Shift-Select>",
	    fun(W) ->
		    tk:cmd(W,[selection, adjust, insert])
	    end),
    bindw("<Control-slash>",
	    fun(W) ->
		    tk:cmd(W,[selection, range, 0, "end"])
	    end),
    bindw("<Control-backslash>",
	    fun(W) ->
		    tk:cmd(W,[selection, clear])
	    end),

    bind("<KeyPress>", ['%W','%A'], fun(W,A) -> tkEntryInsert(W, A) end),

%% Ignore all Alt, Meta, and Control keypresses unless explicitly bound.
%% Otherwise, if a widget binding for one of these is defined, the
%% <KeyPress> class binding will also fire and insert the character,
%% which is wrong.  Ditto for Escape, Return, and Tab.

    bind("<Alt-KeyPress>", [], fun() -> false end),
    bind("<Meta-KeyPress>", [], fun() -> false end),
    bind("<Control-KeyPress>", [], fun() -> false end),
    bind("<Escape>", [], fun() -> false end),
    bind("<Return>", [], fun() -> false end),
    bind("<KP_Enter>", [], fun() -> false end),
    bind("<Tab>", [], fun() -> false end),
    bindw("<Insert>",
	    fun(W) ->
		    catch
			tkEntryInsert(W, tk:rselection(["get",{displayof,W}]))
	    end),

    strict_bindw("<Control-a>", 
	 fun(W) -> tkEntrySetCursor(W, 0) end),
    strict_bindw("<Control-b>",
	 fun(W) ->
		 Ix = tk:cmd(W, [index, insert]),
		 tkEntrySetCursor(W, Ix -1)
	 end),
    strict_bindw("<Control-d>",
	 fun(W) ->
		 tk:cmd(W,[delete, insert])
	 end),
    strict_bindw("<Control-e>",
	 fun(W) ->
		 tkEntrySetCursor(W,"end")
	 end),
    strict_bindw("<Control-f>",
	 fun(W) ->
		 Ix = tk:cmd(W, [index, insert]),
		 tkEntrySetCursor(W, Ix+1)
	 end),
    strict_bindw("<Control-h>", 
	 fun(W) ->
		 tkEntryBackspace(W)
	 end),
    strict_bindw("<Control-k>",
	 fun(W) ->
		 tk:cmd(W, [delete, insert,"end"])
	 end),
    strict_bindw("<Control-t>",
	 fun(W) ->
		 tkEntryTranspose(W)
	 end),
    strict_bindw("<Meta-b>",
	 fun(W) ->
		 Ix = tk:cmd(W, [index, insert]),
		 String = tk:rcmd(W, [get]),
		 WIx = tklib:wordstart(String, Ix-1),
		 tkEntrySetCursor(W, WIx)
	 end),
    strict_bindw("<Meta-d>",
	 fun(W) ->
		 Ix = tk:cmd(W, [index, insert]),
		 String = tk:rcmd(W, [get]),
		 WIx = tklib:wordend(String, Ix),
		 tk:cmd(W, [delete, insert, WIx])
	 end),
    strict_bindw("<Meta-f>",
	 fun(W) ->
		 Ix = tk:cmd(W, [index, insert]),
		 String = tk:rcmd(W, [get]),
		 WIx = tklib:wordend(String, Ix),
		 tkEntrySetCursor(W, WIx)
	 end),
    strict_bindw("<Meta-BackSpace>",
	 fun(W) ->
		 Ix = tk:cmd(W, [index, insert]),
		 String = tk:rcmd(W, [get]),
		 WIx = tklib:wordstart(String, Ix-1),
		 tk:cmd(W, [delete, WIx, insert])
	 end),

    %% A few additional bindings of my own.

    strict_bindwx("<2>",
	 fun(W,X) ->
		 tk:cmd(W, [scan, mark, X]),
		 ?tkset(x, X),
		 ?tkset(mouseMoved, 0)
	 end),
    strict_bindwx("<B2-Motion>", 
	 fun(W,X) ->
		 OldX = ?tkget(x),
		 if
		     abs(X - OldX) > 2 ->
			 ?tkset(mouseMoved, 1);
		     true ->
			  false
		 end,
		 tk:cmd(W, [scan, dragto, X])
	 end).


%% tkEntryClosestGap --
%% Given x and y coordinates, this procedure finds the closest boundary
%% between characters to the given coordinates and returns the index
%% of the character just after the boundary.
%%
%% Arguments:
%% w -		The entry window.
%% x -		X-coordinate within the window.

tkEntryClosestGap(W, X) ->
    Pos = tk:cmd(W, [index, mkpos(X)]),
    [B0,_,B1|_] = tk:cmd(W, [bbox, Pos]),
    if 
	X - B0 < B1 / 2 -> Pos;
	true  -> Pos + 1
    end.

%% tkEntryButton1 --
%% This procedure is invoked to handle button-1 presses in entry
%% widgets.  It moves the insertion cursor, sets the selection anchor,
%% and claims the input focus.
%%
%% Arguments:
%% w -		The entry window in which the button was pressed.
%% x -		The x-coordinate of the button press.

tkEntryButton1(W, X) ->
    ?tkset(selectMode, char),
    ?tkset(mouseMoved, 0),
    ?tkset(pressX, X),
    tk:cmd(W, [icursor, tkEntryClosestGap(W, X)]),
    tk:cmd(W, [selection, from, insert]),
    case tklib:index(4, tk:cmd(W, [configure, {state}])) of
	"normal" ->
	    tk:focus([W]);
	_ ->
	    false
    end.

%% tkEntryMouseSelect --
%% This procedure is invoked when dragging out a selection with
%% the mouse.  Depending on the selection mode (character, word,
%% line) it selects in different-sized units.  This procedure
%% ignores mouse motions initially until the mouse has moved from
%% one character to another or until there have been multiple clicks.
%%
%% Arguments:
%% w -		The entry window in which the button was pressed.
%% x -		The x-coordinate of the mouse.

tkEntryMouseSelect(W, X) ->
    Cur = tkEntryClosestGap(W, X),
    Anchor = tk:cmd(W, [index, anchor]),
    if
	Cur =/= Anchor ->
	    ?tkset(mouseMoved, 1);
	true ->
	    Tmp = abs(?tkget(pressX) - X),
	    if
		Tmp >= 3 ->
		    ?tkset(mouseMoved, 1);
		true ->
		    false
	    end
    end,
    case ?tkget(selectMode) of
	char ->
	    case ?tkget(mouseMoved) of
		1 ->
		    if
			Cur < Anchor ->
			    tk:cmd(W, [selection,to,Cur]);
			true ->
			    tk:cmd(W, [selection,to,Cur+1])
		    end;
		_ ->
		    false
	    end;

	word ->
	    String = tk:rcmd(W, [get]),
	    if 
		Cur < Anchor ->
		    tk:cmd(W, [selection, range,
			       tklib:wordstart(String, Cur),
			       tklib:wordend(String, Anchor-1)]);
		true ->
		    tk:cmd(W, [selection,range,
			       tklib:wordstart(String, Anchor),
			       tklib:wordend(String, Cur)])
	    end;
	line ->
	    tk:cmd(W,[selection, range, 0, "end"])
    end,
    tk:update(idletasks).

%% tkEntryPaste --
%% This procedure sets the insertion cursor to the current mouse position,
%% pastes the selection there, and sets the focus to the window.
%%
%% Arguments:
%% w -		The entry window.
%% x -		X position of the mouse.

tkEntryPaste(W,X) ->
    tk:cmd(W, [icursor, tkEntryClosestGap(W, X)]),
    catch tk:cmd(W, [insert, insert,
			tk:rselection([get, {displayof, W}])]),
    case tklib:index(4, tk:cmd(W, [configure, {state}])) of
	"normal" -> tk:focus([W]);
	_ -> false
    end.


%% tkEntryAutoScan --
%% This procedure is invoked when the mouse leaves an entry window
%% with button 1 down.  It scrolls the window left or right,
%% depending on where the mouse is, and reschedules itself as an
%% "after" command so that the window continues to scroll until the
%% mouse moves back into the window or the mouse button is released.
%%
%% Arguments:
%% w -		The entry window.

tkEntryAutoScan(W) ->
    case tk:winfo([exists, W]) of
	1 ->
	    X = ?tkget(x),
	    Width = tk:winfo([width, W]),
	    if
		X >= Width ->
		    tk:cmd(W, [xview, scroll, 2, units]),
		    tkEntryMouseSelect(W, X);
		X < 0 ->
		    tk:cmd(W, [xview, scroll, -2, units]),
		    tkEntryMouseSelect(W, X);
		true ->
		    false
	    end,
	    tk:tkRepeat(50, fun() -> tkEntryAutoScan(W) end);
	_ ->
	    false
    end.
    

%% tkEntryKeySelect --
%% This procedure is invoked when stroking out selections using the
%% keyboard.  It moves the cursor to a new position, then extends
%% the selection to that position.
%%
%% Arguments:
%% w -		The entry window.
%% new -		A new position for the insertion cursor (the cursor hasn't
%%		actually been moved to this position yet).

tkEntryKeySelect(W, New) ->
    case tk:cmd(W, [selection, present]) of
	0 ->
	    tk:cmd(W, [selection,from, insert]),
	    tk:cmd(W, [selection, to, New]);
	1 ->
	    tk:cmd(W, [selection, adjust, New])
    end,
    tk:cmd(W, [icursor, New]).

%% tkEntryInsert --
%% Insert a string into an entry at the point of the insertion cursor.
%% If there is a selection in the entry, and it covers the point of the
%% insertion cursor, then delete the selection before inserting.
%%
%% Arguments:
%% w -		The entry window in which to insert the string
%% s -		The string to insert (usually just a single character)

tkEntryInsert(W, "") -> false;
tkEntryInsert(W, S) ->
    catch
	begin
	    Insert = tk:cmd(W, [index, insert]),
	    SelFirst =  tk:cmd(W, [index, "sel.first"]),
	    SelLast =  tk:cmd(W, [index, "sel.last"]),
	    if
		SelFirst =< Insert, SelLast >= Insert ->
		    tk:cmd(W, [delete,"sel.first","sel.last"]);
		true ->
		    false
	    end
	end,
    tk:cmd(W, [insert, insert, S]),
    tkEntrySeeInsert(W).

%% tkEntryBackspace --
%% Backspace over the character just before the insertion cursor.
%% If backspacing would move the cursor off the left edge of the
%% window, reposition the cursor at about the middle of the window.
%%
%% Arguments:
%% w -		The entry window in which to backspace.

tkEntryBackspace(W) ->
    case tk:cmd(W, [selection, present]) of
	1 ->
	    tk:cmd(W, [delete, "sel.first", "sel.last"]);
	0 ->
	    X = tk:cmd(W, [index,"insert"]),
	    if
		X >= 1 ->
		    tk:cmd(W, [delete, X-1]);
		true ->
		    false
	    end,
	    Index0 = tk:cmd(W, [index, "@0"]),
	    if
		Index0 >= X ->
		    [Left,Right | _] = tk:cmd(W, [xview]),
		    tk:cmd(W, [xview, moveto,
				  Left - (Right - Left)/2]);
		true  ->
		    false
	    end
    end.

%% tkEntrySeeInsert --
%% Make sure that the insertion cursor is visible in the entry window.
%% If not, adjust the view so that it is.
%%
%% Arguments:
%% w -		The entry window.

tkEntrySeeInsert(W) ->
    C = tk:cmd(W, [index, insert]),
    Left = tk:cmd(W, [index, "@0"]),
    if
	Left > C ->
	    tk:cmd(W, [xview, C]);
	true ->
	    X = tk:winfo( [width, W]),
	    Right = tk:cmd(W, [index, mkpos(X)]),
	    if
		Right > C ->
		    false;
		true ->
		    tk:cmd(W, [xview, Left+(C-Left)])
	    end
    end.

%% tkEntrySetCursor -
%% Move the insertion cursor to a given position in an entry.  Also
%% clears the selection, if there is one in the entry, and makes sure
%% that the insertion cursor is visible.
%%
%% Arguments:
%% w -		The entry window.
%% pos -		The desired new position for the cursor in the window.

tkEntrySetCursor(W, Pos) ->
    tk:cmd(W, [icursor, Pos]),
    tk:cmd(W, [selection, clear]),
    tkEntrySeeInsert(W).

%% tkEntryTranspose -
%% This procedure implements the "transpose" function for entry widgets.
%% It tranposes the characters on either side of the insertion cursor,
%% unless the cursor is at the end of the line.  In this case it
%% transposes the two characters to the left of the cursor.  In either
%% case, the cursor ends up to the right of the transposed characters.
%%
%% Arguments:
%% w -		The entry window.

tkEntryTranspose(W) ->
    Ins = tk:cmd(W, [index, insert]),
    End = tk:cmd(W, [index, 'end']),
    I = if 
	    Ins < End ->
		Ins+1;
	    true ->
		 Ins
	end,
    First = I - 2,
    if 
	First < 0 ->
	    false;
	true ->
	    String = tk:rcmd(W, [get]),
	    New = [tklib:index(I-1,String),tklib:index(First,String)],
	    tk:cmd(W, [delete, First, I]),
	    tk:cmd(W, [insert, insert, New]),
	    tkEntrySeeInsert(W)
    end.

