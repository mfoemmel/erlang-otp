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
%% text.tcl --
%%
%% This file defines the default bindings for Tk text widgets and provides
%% procedures that help in implementing the bindings.
%%
%% @(#) text.tcl 1.36 95/06/28 10:24:23
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
%% char -		Character position on the line;  kept in order
%%			to allow moving up or down past short lines while
%%			still remembering the desired position.
%% mouseMoved -		Non-zero means the mouse has moved a significant
%%			amount since the button went down (so, for example,
%%			start dragging out a selection).
%% prevPos -		Used when moving up or down lines via the keyboard.
%%			Keeps track of the previous insert position, so
%%			we can distinguish a series of ups and downs, all
%%			in a row, from a new up or down.
%% selectMode -		The style of selection currently underway:
%%			char, word, or line.
%% x, y -		Last known mouse coordinates for scanning
%%			and auto-scanning.
%%-------------------------------------------------------------------------

-module(tktext).

-export([init/0]).
-export([tkTextSetCursor/2]).

-include("tk.hrl").

-export([tk_textCopy/1, tk_textCut/1, tk_textPaste/1]).

bind(Event, Template, Fun) ->
    tk:bind("Text", Event, Template, Fun).

bindw(Event, Fun) ->
    tk:bind("Text", Event, ['%W'], Fun).

bindwxy(Event, Fun) ->
    tk:bind("Text", Event, ['%W','%x','%y'], Fun).

strict_bindw(Event, Fun) ->
    tk:bind("Text", Event, ['%W'],
	    fun(W) ->
		    case ?tkget(tk_strictMotif) of
			false -> Fun(W);
			true  -> false
		    end
	    end).

strict_bindwxy(Event, Fun) ->
    tk:bind("Text", Event, ['%W','%x','%y'],
	    fun(W,X,Y) ->
		    case ?tkget(tk_strictMotif) of
			false -> Fun(W,X,Y);
			true  -> false
		    end
	    end).

mkpos(X,Y) ->
    [$@ | integer_to_list(X) ++ "," ++ integer_to_list(Y)].

index(W, Where) ->
    tk:rcmd(W, [index, Where]).

%%-------------------------------------------------------------------------
%% The code below creates the default class bindings for entries.
%%-------------------------------------------------------------------------

init() ->
    bindwxy("<1>",
	 fun(W,X,Y) ->
		 tkTextButton1(W, X, Y),
		 tk:cmd(W, [tag,remove,sel,"1.0",'end'])
	 end),
    
    bindwxy("<B1-Motion>",
	 fun(W,X,Y) ->
		 ?tkset(x, X),
		 ?tkset(y, Y),
		 tkTextSelectTo(W, X, Y)
	 end),

    bindwxy("<Double-1>",
	 fun(W,X,Y) ->
		 ?tkset(selectMode, word),
		 tkTextSelectTo(W, X, Y),
		 catch 
		     begin 
			 tk:cmd(W, [mark, set, insert, "sel.first"])
		     end
	 end),

    bindwxy("<Triple-1>",
	 fun(W,X,Y) ->
		 ?tkset(selectMode, line),
		 tkTextSelectTo(W, X, Y),
		 catch 
		     begin 
			 tk:cmd(W, [mark, set, insert, "sel.first"])
		     end
	 end),

    bindwxy("<Shift-1>", 
	    fun(W,X,Y) ->
		 tkTextResetAnchor(W, mkpos(X,Y)),
		 ?tkset(selectMode, char),
		 tkTextSelectTo(W, X, Y)
	 end),

    bindwxy("<Double-Shift-1>",
	 fun(W,X,Y) ->
		 ?tkset(selectMode, word),
		 tkTextSelectTo(W, X, Y)
	 end),

    bindwxy("<Triple-Shift-1>",
	 fun(W,X,Y) ->
		 ?tkset(selectMode, line),
		 tkTextSelectTo(W, X, Y)
	 end),
    bindwxy("<B1-Leave>",
	 fun(W,X,Y) ->
		 ?tkset(x, X),
		 ?tkset(y, Y),
		 tkTextAutoScan(W)
	 end),
    bind("<B1-Enter>", [], fun() -> tk:tkCancelRepeat() end),
    bind("<ButtonRelease-1>", [], fun() -> tk:tkCancelRepeat() end),

    bindwxy("<Control-1>",
	 fun(W,X,Y) ->
		 tk:cmd(W,[mark,set,insert, mkpos(X,Y)])
	 end),
    bindwxy("<ButtonRelease-2>",
         fun(W,X,Y) ->
              case ?tkget(mouseMoved) of
                1 -> 
		      tkTextPaste(W,X,Y);
		_ -> 
		      case ?tkget(tk_strictMotif) of
			  false ->
			      tkTextPaste(W, X, Y);
			  true -> false
		      end
	      end
	 end),

    bindw("<Left>",
	 fun(W) ->
		 tkTextSetCursor(W, index(W, "insert - 1c"))
	 end),
    bindw("<Right>", 
	 fun(W) ->
		 tkTextSetCursor(W, index(W, "insert + 1c"))
	 end),
    bindw("<Up>",
	 fun(W) ->
		 tkTextSetCursor(W, tkTextUpDownLine(W, -1))
	 end),
    bindw("<Down>", 
	 fun(W) ->
		 tkTextSetCursor(W, tkTextUpDownLine(W, 1))
	 end),
    bindw("<Shift-Left>",
	 fun(W) ->
		 tkTextKeySelect(W, index(W,"insert - 1c"))
	 end),
    bindw("<Shift-Right>",
	 fun(W) ->
		 tkTextKeySelect(W, index(W, "insert + 1c"))
	 end),
    bindw("<Shift-Up>",
	 fun(W) ->
		 tkTextKeySelect(W, tkTextUpDownLine(W, -1))
	 end),

    bindw("<Shift-Down>", 
	 fun(W) ->
		 tkTextKeySelect(W, tkTextUpDownLine(W, 1))
	 end),
    bindw("<Control-Left>",
	 fun(W) ->
		 tkTextSetCursor(W, index(W, "insert - 1c wordstart"))
	 end),
    bindw("<Control-Right>",
	 fun(W) ->
		 tkTextSetCursor(W, index(W, "insert wordend"))
	 end),
    bindw("<Control-Up>",
	 fun(W) ->
		 tkTextSetCursor(W, tkTextPrevPara(W, "insert"))
	 end),
    bindw("<Control-Down>",
	 fun(W) ->
		 tkTextSetCursor(W, tkTextNextPara(W, "insert"))
	 end),

    bindw("<Shift-Control-Left>",
	 fun(W) ->
		 tkTextKeySelect(W, index(W, "insert - 1c wordstart"))
	 end),
    bindw("<Shift-Control-Right>",
	 fun(W) ->
		 tkTextKeySelect(W, index(W, "insert wordend"))
	 end),
    bindw("<Shift-Control-Up>",
	 fun(W) ->
		 tkTextKeySelect(W, tkTextPrevPara(W, "insert"))
	 end),
    bindw("<Shift-Control-Down>",
	 fun(W) ->
		 tkTextKeySelect(W, tkTextNextPara(W, "insert"))
	 end),
    bindw("<Prior>",
	 fun(W) ->
		 tkTextSetCursor(W, tkTextScrollPages(W, -1))
	 end),
    bindw("<Shift-Prior>",
	 fun(W) ->
		 tkTextKeySelect(W, tkTextScrollPages(W, -1))
	 end),
    bindw("<Next>",
	 fun(W) ->
		 tkTextSetCursor(W, tkTextScrollPages(W, 1))
	 end),
    bindw("<Shift-Next>",
	 fun(W) ->
		 tkTextKeySelect(W, tkTextScrollPages(W, 1))
	 end),
    bindw("<Control-Prior>",
	 fun(W) ->
	      tk:cmd(W,[xview,scroll,-1,page])
	 end),
    bindw("<Control-Next>",
	 fun(W) ->
	      tk:cmd(W,[xview,scroll,1,page])
	 end),
    bindw("<Home>",
	 fun(W) ->
		 tkTextSetCursor(W, "insert linestart")
	 end),
    bindw("<Shift-Home>",
	 fun(W) ->
		 tkTextKeySelect(W, "insert linestart")
	 end),
    bindw("<End>",
	 fun(W) ->
		 tkTextSetCursor(W, "insert lineend")
	 end),
    bindw("<Shift-End>",
	 fun(W) ->
		 tkTextKeySelect(W, "insert lineend")
	 end),
    bindw("<Control-Home>",
	 fun(W) ->
		 tkTextSetCursor(W, "1.0")
	 end),
    bindw("<Control-Shift-Home>",
	 fun(W) ->
		 tkTextKeySelect(W, "1.0")
	 end),
    bindw("<Control-End>",
	 fun(W) ->
		 tkTextSetCursor(W, "end - 1 char")
	 end),
    bindw("<Control-Shift-End>",
	 fun(W) ->
		 tkTextKeySelect(W, "end -1 char")
	 end),
    bindw("<Tab>",
	 fun(W) ->
		 tkTextInsert(W, "\t"),
		 tk:focus([W]),
		 break
	 end),
    bind("<Shift-Tab>", [],
	 fun() ->
		 %% Needed only to keep <Tab> binding from triggering;  doesn't
		 %% have to actually do anything.
		 true
	 end),
    bindw("<Control-Tab>",
	 fun(W) ->
		 tk:focus([tkfocus:tk_focusNext(W)])
	 end),
    bindw("<Control-Shift-Tab>",
	 fun(W) ->
		 tk:focus([tkfocus:tk_focusPrev(W)])
	 end),
    bindw("<Control-i>",
	 fun(W) ->
		 tkTextInsert(W,"\t")
	 end),
    bindw("<Return>",
	 fun(W) ->
		 tkTextInsert(W,"\n")
	 end),
    bindw("<Delete>",
	 fun(W) ->
		 case tk:cmd(W, [tag,nextrange,sel,"1.0","end"]) of
		     "" ->
			 tk:cmd(W, [delete, insert]),
			 tk:cmd(W, [see, insert]);
		     _ ->
			 tk:cmd(W, [delete,"sel.first","sel.last"])
		 end
	 end),
    bindw("<BackSpace>",
	 fun(W) ->
		 case tk:cmd(W, [tag,nextrange,sel,"1.0","end"]) of
		     "" ->
			 case tk:cmd(W, [compare,insert,"!=","1.0"]) of
			     1 ->
				 tk:cmd(W, [delete, "insert-1c"]),
				 tk:cmd(W, [see, insert]);
			     _ ->
				 true
			 end;
		     _ ->
			 tk:cmd(W, [delete,"sel.first","sel.last"])
		 end
	 end),
    bindw("<Control-space>",
	 fun(W) ->
		 tk:cmd(W,[mark,set,anchor,insert])
	 end),
    bindw("<Select>",
	 fun(W) ->
		 tk:cmd(W,[mark,set,anchor,insert])
	 end),
    bindw("<Control-Shift-space>",
	 fun(W) ->
		 ?tkset(selectMode, char),
		 tkTextKeyExtend(W,"insert")
	 end),
    bindw("<Shift-Select>",
	 fun(W) ->
		 ?tkset(selectMode, char),
		 tkTextKeyExtend(W,"insert")
	 end),
    bindw("<Control-slash>",
	 fun(W) ->
		 tk:cmd(W,[tag,add,sel,"1.0","end"])
	 end),
    bindw("<Control-backslash>",
	 fun(W) ->
		 tk:cmd(W,[tag,remove,sel,"1.0","end"])
	 end),

    bindw("<<Copy>>", fun tk_textCopy/1),
    bindw("<<Cut>>", fun tk_textCut/1),
    bindw("<<Paste>>", fun tk_textPaste/1),
    bindw("<<Clear>>", fun(W) -> tk:cmd(W,[delete,"sel.first","sel.last"])end),

    bindw("<Insert>",
	 fun(W) ->
		 catch
		     begin
			 tkTextInsert(W, tk:rselection([get,{displayof,W}]))
		     end
	 end),
    bind("<KeyPress>",['%W', '%A'],
	 fun(W, A) ->
		 tkTextInsert(W, A)
	 end),

%% Ignore all Alt, Meta, and Control keypresses unless explicitly bound.
%% Otherwise, if a widget binding for one of these is defined, the
%% <KeyPress> class binding will also fire and insert the character,
%% which is wrong.  Ditto for <Escape>.

    bind("<Alt-KeyPress>", [], fun() -> true end),
    bind("<Meta-KeyPress>", [], fun() -> true end),
    bind("<Control-KeyPress>", [], fun() -> true end),
    bind("<Escape>", [], fun() -> true end),
    bind("<KP_Enter>", [], fun() -> true end),

    strict_bindw("<Control-a>",
	 fun(W) ->
		 tkTextSetCursor(W, "insert linestart")
	 end),
    strict_bindw("<Control-b>",
	 fun(W) ->
		 tkTextSetCursor(W, "insert - 1c")
	 end),
    strict_bindw("<Control-d>",
	 fun(W) ->
		 tk:cmd(W, [delete, insert])
	 end),
    strict_bindw("<Control-e>",
	 fun(W) ->
		 tkTextSetCursor(W, "insert lineend")
	 end),
    strict_bindw("<Control-f>",
	 fun(W) ->
		 tkTextSetCursor(W, "insert + 1c")
	 end),
    strict_bindw("<Control-k>",
	 fun(W) ->
		 case tk:cmd(W,
				[compare,insert,"==","insert lineend"]) of
		     1 ->
			 tk:cmd(W, [delete, insert]);
		     _ ->
			 tk:cmd(W, [delete, insert, "insert lineend"])
		 end
	 end),
    strict_bindw("<Control-n>",
	 fun(W) ->
		 tkTextSetCursor(W,tkTextUpDownLine(W,1))
	 end),		     
    strict_bindw("<Control-o>",
	 fun(W) ->
		 tk:cmd(W,[insert, insert, "\n"]),
		 tk:cmd(W,[mark, set, insert, "insert-1c"])
	 end),
    strict_bindw("<Control-p>",
	 fun(W) ->
		 tkTextSetCursor(W, tkTextUpDownLine(W,-1))
	 end),
    strict_bindw("<Control-t>",
	 fun(W) ->
		 tkTextTranspose(W)
	 end),
    strict_bindw("<Control-v>",
	 fun(W) ->
		 tkTextSetCursor(W, tkTextScrollPages(W,1))
	 end),

    strict_bindw("<Meta-b>",
	 fun(W) ->
		 tkTextSetCursor(W, "insert - 1c wordstart")
	 end),
    strict_bindw("<Meta-d>",
	 fun(W) ->
		 tk:cmd(W,[delete, insert,"insert wordend"])
	 end),
    strict_bindw("<Meta-f>",
	 fun(W) ->
		 tkTextSetCursor(W, "insert wordend")
	 end),
    strict_bindw("<Meta-less>",
	 fun(W) ->
		 tkTextSetCursor(W, "1.0")
	 end),
    strict_bindw("<Meta-greater>",
	 fun(W) ->
		 tkTextSetCursor(W,"end-1c")
	 end),

    strict_bindw("<Meta-BackSpace>",
	 fun(W) ->
		 tk:cmd(W,[delete, "insert -1c wordstart", insert])
	 end),
    strict_bindw("<Meta-Delete>",
	 fun(W) ->
		 tk:cmd(W, [delete,"insert -1c wordstart", insert])
	 end),

    %% A few additional bindings of my own.

    strict_bindw("<Control-h>",
	 fun(W) ->
		 case tk:cmd(W, [compare, insert, "!=", "1.0"]) of
		     1 ->
			 tk:cmd(W, [delete,"insert -1c"]),
			 tk:cmd(W, [see, insert]);
		     0 ->
			 true
		 end
	 end),
    strict_bindwxy("<2>",
	 fun(W,X,Y) ->
		 tk:cmd(W, [scan,mark,X,Y]),
		 ?tkset(x, X),
		 ?tkset(y, Y),
		 ?tkset(mouseMoved, 0)
	 end),
    strict_bindwxy("<B2-Motion>",
	 fun(W,X,Y) ->
		 Moved = case ?tkget(x) of
			     X -> ?tkset(mouseMoved,1);
			     _ -> 0
			 end +
		     case ?tkget(y) of
			 Y -> ?tkset(mouseMoved,1);
			 _ -> 0
		     end,
		 if
		     Moved == 1 ->
			 tk:cmd(W, [scan, dragto, X, Y]);
		     true -> 
			 true
		 end
	 end),
    strict_bindwxy("<ButtonRelease-2>",
	 fun(W,X,Y) ->
		 case ?tkget(mouseMoved) of
		     0 ->
			 catch tk:cmd(W,
					 [insert, mkpos(X,Y),
					  tk:rselection([get,
							 {displayof,W}])]);
		     1 ->
			 false
		 end
	 end),
    true.

%% tkTextClosestGap --
%% Given x and y coordinates, this procedure finds the closest boundary
%% between characters to the given coordinates and returns the index
%% of the character just after the boundary.
%%
%% Arguments:
%% w -		The text window.
%% x -		X-coordinate within the window.
%% y -		Y-coordinate within the window.

tkTextClosestGap(W,X,Y) ->
    Pos = index(W, mkpos(X,Y)),
    case tk:cmd(W, [bbox, Pos]) of
	[B0,_,B2 | _] ->
	    if 
		X - B0 < B2/2 -> Pos;
		true -> index(W, Pos ++ " + 1 char")
	    end;
	_ ->
	    Pos
    end.

%% tkTextButton1 --
%% This procedure is invoked to handle button-1 presses in text
%% widgets.  It moves the insertion cursor, sets the selection anchor,
%% and claims the input focus.
%%
%% Arguments:
%% w -		The text window in which the button was pressed.
%% x -		The x-coordinate of the button press.
%% y -		The x-coordinate of the button press.

tkTextButton1(W, X, Y) ->
    ?tkset(selectMode, char),
    ?tkset(mouseMoved, 0),
    ?tkset(pressX, X),
    tk:cmd(W, [mark,set,insert, tkTextClosestGap(W,X,Y)]),
    tk:cmd(W, [mark,set,anchor,insert]),
    case tk:cget(W, state) of
	"normal" -> tk:focus([W]);
	_ -> true
    end.

%% tkTextSelectTo --
%% This procedure is invoked to extend the selection, typically when
%% dragging it with the mouse.  Depending on the selection mode (character,
%% word, line) it selects in different-sized units.  This procedure
%% ignores mouse motions initially until the mouse has moved from
%% one character to another or until there have been multiple clicks.
%%
%% Arguments:
%% w -		The text window in which the button was pressed.
%% x -		Mouse x position.
%% y - 		Mouse y position.

tkTextSelectTo(W, X, Y) ->
    Cur = tkTextClosestGap(W, X, Y),
    case catch index(W, anchor) of
	{'EXIT',_} ->
	    tk:cmd(W,[mark, set, anchor, Cur]);
	_ -> true
    end,
    Anchor = index(W, anchor),
    case tk:cmd(W, [compare, Cur, "!=", Anchor]) of
	0 ->
	    PX = ?tkget(pressX),
	    if
		abs(PX - X) >= 3 -> ?tkset(mouseMoved, 1);
		true -> false
	    end;
	_ -> 
	    ?tkset(mouseMoved, 1)
    end,
    Cmp = tk:cmd(W, [compare, Cur, "<", Anchor]),
    {First,Last} =
	case ?tkget(selectMode) of
	    char ->
		if Cmp == 1 -> 
			{Cur,anchor};
		   true ->
			{anchor, Cur}
		end;
	    word ->
		if Cmp == 1 ->
			{ index(W, Cur ++ " wordstart"),
			  index(W, "anchor - 1c wordend")};
		    true ->
			{index(W, "anchor wordstart"),
			 index(W, Cur ++ " -1c wordend")}
		end;
	    line ->
		if Cmp == 1 ->
			{index(W, Cur ++ " linestart"),
			 index(W, "anchor - 1c lineend + 1c")};
		    true ->
			{index(W, "anchor linestart"),
			 index(W, Cur ++ " lineend + 1c")}
		end
	end,
    case ?tkget(mouseMoved) of
	1 ->
	    selectTo(W,First,Last);
	0 ->
	    case ?tkget(selectMode) of
		line -> selectTo(W, First, Last);
		word -> selectTo(W, First, Last);
		_ -> false
	    end
    end.
%% tk:update(idletasks). 

%% tkTextKeyExtend --
%% This procedure handles extending the selection from the keyboard,
%% where the point to extend to is really the boundary between two
%% characters rather than a particular character.
%%
%% Arguments:
%% w -		The text window.
%% index -	The point to which the selection is to be extended.

tkTextKeyExtend(W, Index) ->
    Cur = index(W, Index),
    case catch index(W, anchor) of
	{'EXIT', _} -> 
	    tk:cmd(W, [mark,set,anchor,Cur]);
	_ -> false
    end,
    Anchor = index(W, anchor),
    case tk:cmd(W, [compare, Cur, "<", Anchor]) of
	1 -> selectTo(W,Cur,anchor);
	0 -> selectTo(W,anchor,Cur)
    end.

%% tkTextPaste --
%% This procedure sets the insertion cursor to the mouse position,
%% inserts the selection, and sets the focus to the window.
%%
%% Arguments:
%% w -		The text window.
%% x, y - 	Position of the mouse.

tkTextPaste(W, X, Y) ->
    tk:cmd(W, [mark,set,insert,tkTextClosestGap(W,X,Y)]),
    catch tk:cmd(W, [insert,insert, 
		     tk:rselection([get, {displayof, W}])]),
    case tk:cget(W, state) of
	"normal" -> tk:focus([W]);
	_ -> false
    end.

%% tkTextAutoScan --
%% This procedure is invoked when the mouse leaves a text window
%% with button 1 down.  It scrolls the window up, down, left, or right,
%% depending on where the mouse is (this information was saved in
%% tkPriv(x) and tkPriv(y)), and reschedules itself as an "after"
%% command so that the window continues to scroll until the mouse
%% moves back into the window or the mouse button is released.
%%
%% Arguments:
%% w -		The text window.

tkTextAutoScan(W) ->
    case tk:winfo([exists, W]) of
	0 -> false;
	1 ->
	    Y = ?tkget(y),
	    X = ?tkget(x),
	    Width = tk:winfo([height, W]),
	    Height = tk:winfo([width, W]),
	    Auto = if
		       Y >= Height ->
			   tk:cmd(W, [yview,scroll,2,units]),
			   true;
		       Y < 0 ->
			   tk:cmd(W, [yview,scroll,-2,units]),
			   true;
		       X >= Width ->
			   tk:cmd(W, [xview,scroll,2,units]),
			   true;
		       X < 0 ->
			   tk:cmd(W, [xview,scroll,-2,units]),
			   true;
		       true ->
			   false
		   end,
	    case Auto of
		true ->
		    tkTextSelectTo(W, X, Y),
		    tk:tkRepeat(50, fun() ->
					    tkTextAutoScan(W)
				    end);
		false -> false
	    end
    end.

%% tkTextSetCursor
%% Move the insertion cursor to a given position in a text.  Also
%% clears the selection, if there is one in the text, and makes sure
%% that the insertion cursor is visible.  Also, don't let the insertion
%% cursor appear on the dummy last line of the text.
%%
%% Arguments:
%% w -		The text window.
%% pos -		The desired new position for the cursor in the window.

tkTextSetCursor(W, Pos) ->
    Pos1 = case tk:cmd(W, [compare, Pos, "==", "end"]) of
	       1 -> "end - 1 chars";
	       0 -> Pos
	   end,
    tk:cmd(W, [mark,set,insert,Pos1]),
    tk:cmd(W, [tag,remove,sel,"1.0","end"]),
    tk:cmd(W, [see,insert]).

%% tkTextKeySelect
%% This procedure is invoked when stroking out selections using the
%% keyboard.  It moves the cursor to a new position, then extends
%% the selection to that position.
%%
%% Arguments:
%% w -		The text window.
%% new -		A new position for the insertion cursor (the cursor hasn't
%%		actually been moved to this position yet).

tkTextKeySelect(W, New) ->
    case tk:cmd(W, [tag,nextrange,sel,"1.0","end"]) of
	"" ->
	    case tk:cmd(W, [compare, New, "<", insert]) of
		1 ->
		    tk:cmd(W, [tag,add,sel,New,insert]);
		0 -> 
		    tk:cmd(W, [tag,add,sel,insert, New])
	    end,
	    tk:cmd(W, [mark,set,anchor,insert]);
	_ ->
	    {First,Last} = case tk:cmd(W,[compare,New,"<",insert]) of
			       1 -> {New, "anchor"};
			       0 -> {"anchor", New}
			   end,
	    selectTo(W,First,Last)
    end,
    tk:cmd(W, [mark,set,insert,New]),
    tk:cmd(W, [see,insert]).

%%    tk:update(idletasks).

selectTo(W, First, Last) ->
    tk:cmd(W, [tag,remove,sel,"1.0",First]),
    tk:cmd(W, [tag,add,sel,First,Last]),
    tk:cmd(W, [tag,remove,sel,Last,"end"]).


%% tkTextResetAnchor --
%% Set the selection anchor to whichever end is farthest from the
%% index argument.  One special trick: if the selection has two or
%% fewer characters, just leave the anchor where it is.  In this
%% case it doesn't matter which point gets chosen for the anchor,
%% and for the things like Shift-Left and Shift-Right this produces
%% better behavior when the cursor moves back and forth across the
%% anchor.
%%
%% Arguments:
%% w -		The text widget.
%% index -	Position at which mouse button was pressed, which determines
%%		which end of selection should be used as anchor point.

tkTextResetAnchor(W, Index) ->
    case tk:cmd(W, [tag, ranges, sel]) of
	"" ->
	    tk:cmd(W, [mark,set,anchor,Index]);
	_ ->
	    A = index(W, Index),
	    B = index(W, "sel.first"),
	    C = index(W, "sel.last"),
	    case tk:cmd(W, [compare,A,"<",B]) of
		1 ->
		    tk:cmd(W, [mark,set,anchor,"sel.last"]);
		0 ->
		    case tk:cmd(W, [compare,A,">",C]) of
			1 ->
			    tk:cmd(W, [mark, set, anchor, "sel.first"]);
			0 ->
			    {ok,[LineA,ChA],_} = io_lib:fread("~d.~d", A),
			    {ok,[LineB,ChB],_} = io_lib:fread("~d.~d", B),
			    {ok,[LineC,ChC],_} = io_lib:fread("~d.~d", C),
			    if
				LineB < LineC + 2 ->
				    TotalBC = length(tk:rcmd(W, [get,B,C])),
				    if 
					TotalBC > 2 ->
					    TotalBA = length(tk:rcmd(W, [get,B, A])),
					    if
						TotalBA < TotalBC / 2 ->
						    tk:cmd(W, [mark,set,anchor, "sel.last"]);
						true ->
						    tk:cmd(W, [mark,set,anchor, "sel.first"])
					    end
				    end;
				LineA-LineB < LineC-LineA ->
				    tk:cmd(W, [mark, set, anchor, "sel.last"]);
				true  ->
				    tk:cmd(W, [mark, set, anchor, "sel.first"])
			    end
		    end
	    end
    end.

%% tkTextInsert --
%% Insert a string into a text at the point of the insertion cursor.
%% If there is a selection in the text, and it covers the point of the
%% insertion cursor, then delete the selection before inserting.
%%
%% Arguments:
%% w -		The text window in which to insert the string
%% s -		The string to insert (usually just a single character)

tkTextInsert(W, S) ->
    case tk:cget(W, state) of
	"disabled" ->
	    false;
	_ when S == "" ->
	    false;
	_ ->
	    catch 
		begin
		    case tk:cmd(W, [compare,"sel.first","<=","insert"]) of
			1 ->
			    case tk:cmd(W, [compare,"sel.last",">=","insert"]) of
				1 ->
				    tk:cmd(W, [delete,
					       "sel.first", "sel.last"]);
				0 ->
				    false
			    end;
			0 ->
			    false
		    end
		end,
	    tk:cmd(W, [insert,insert,S]),
	    tk:cmd(W, [see,insert])
    end.

%% tkTextUpDownLine --
%% Returns the index of the character one line above or below the
%% insertion cursor.  There are two tricky things here.  First,
%% we want to maintain the original column across repeated operations,
%% even though some lines that will get passed through don't have
%% enough characters to cover the original column.  Second, don't
%% try to scroll past the beginning or end of the text.
%%
%% Arguments:
%% w -		The text window in which the cursor is to move.
%% n -		The number of lines to move: -1 for up one line,
%%		+1 for down one line.

tkTextUpDownLine(W, N) ->
    I = index(W, insert),
    {ok,[Line,Ch],_} = io_lib:fread("~d.~d", I),
    Char = case ?tkget(prevPos) of
	       I -> 
		   ?tkget(char);
	       _ ->
		   ?tkset(char, Ch),
		   Ch
	   end,
    New = index(W, integer_to_list(Line+N)++"."++integer_to_list(Char)),
    New1 = case tk:cmd(W, [compare, New, "==", "end"]) of
	       1 -> I;
	       0 ->
		   case tk:cmd(W, [compare, New, "==", "insert linestart"]) of
		       1 -> I;
		       0 -> New
		   end
	   end,
    ?tkset(prevPos, New1).

%% tkTextPrevPara --
%% Returns the index of the beginning of the paragraph just before a given
%% position in the text (the beginning of a paragraph is the first non-blank
%% character after a blank line).
%%
%% Arguments:
%% w -		The text window in which the cursor is to move.
%% pos -		Position at which to start search.

tkTextPrevPara(W, Pos) ->
    NPos = index(W, Pos ++ " linestart"),
    prevPara(W, NPos).

prevPara(W, Pos) ->
    Cond = 
	if Pos == "1.0" -> 
		true;
	   true ->
		case tk:rcmd(W, [get, Pos ++ " - 1 line"]) of
		    "\n" ->
			case tk:rcmd(W, [get, Pos]) of
			    C when C =/= "\n" -> true;
			    _ -> false
			end;
		    _ -> false
		end
	end,
    case Cond of
	true ->
	    Line = tk:rcmd(W, [get, Pos, Pos ++ " lineend"]),
	    Pos1 = case string:re_match(Line, "^[\t ]+(.)") of
		       nomatch -> Pos;
		       {match, _, Index} ->
			   index(W, Pos ++ " " ++ 
				 integer_to_list(Index-1) ++ " chars")
		   end,
	    case tk:cmd(W, [compare, Pos1, "!=", "insert"]) of
		0 when Pos1 == "1.0" -> 
		    Pos1;
		1 ->
		    Pos1;
		_ ->
		    prevPara(W, index(W, Pos1 ++ " - 1 line"))
	    end;
	false ->
	    prevPara(W, index(W, Pos ++ " - 1 line"))
    end.

%% tkTextNextPara --
%% Returns the index of the beginning of the paragraph just after a given
%% position in the text (the beginning of a paragraph is the first non-blank
%% character after a blank line).
%%
%% Arguments:
%% w -		The text window in which the cursor is to move.
%% start -	Position at which to start search.

tkTextNextPara(W, Start) ->
    Pos = index(W, Start ++ " linestart + 1 line"),
    case nextEol(W, Pos) of
	'end' ->
	    index(W, "end - 1c");
	Pos1 ->
	    case nextNonEol(W, Pos1) of
		'end' ->
		    index(W, "end - 1c");
		Pos2 ->
		    Line = tk:rcmd(W, [get, Pos2, Pos2 ++ " lineend"]),
		    case string:re_match(Line, "^[\t ]+(.)") of
			nomatch -> Pos2;
			{match,_,Index} ->
			    index(W, Pos2 ++ " " ++
				  integer_to_list(Index-1) ++ " chars")
		    end
	    end
    end.

nextEol(W, Pos) ->
    case tk:rcmd(W, [get, Pos]) of
	"\n" -> Pos;
	_ ->
	    case tk:cmd(W, [compare, Pos, "==", "end"]) of
		1 -> 'end';
		0 -> nextEol(W, index(W, Pos + " + 1 line"))
	    end
    end.

nextNonEol(W, Pos) ->
    case tk:rcmd(W, [get, Pos]) of
	"\n" ->
	    Pos1 = index(W, Pos ++ " + 1 line"),
	    case tk:cmd(W, [compare, Pos1, "==", "end"]) of
		1 -> 'end';
		0 -> nextNonEol(W, Pos1)
	    end;
	_ ->
	    Pos
    end.

%% tkTextScrollPages --
%% This is a utility procedure used in bindings for moving up and down
%% pages and possibly extending the selection along the way.  It scrolls
%% the view in the widget by the number of pages, and it returns the
%% index of the character that is at the same position in the new view
%% as the insertion cursor used to be in the old view.
%%
%% Arguments:
%% w -		The text window in which the cursor is to move.
%% count -	Number of pages forward to scroll;  may be negative
%%		to scroll backwards.

tkTextScrollPages(W, Count) ->
    Bbox = tk:cmd(W, [bbox, insert]),
    tk:cmd(W, [yview, scroll, Count, pages]),
    case Bbox of
	[X0,Y0 | _] ->
	    index(W, mkpos(X0, Y0));
	[] ->
	    index(W, mkpos(tk:winfo([height, W]) div 2, 0))
    end.

%% tkTextTranspose --
%% This procedure implements the "transpose" function for text widgets.
%% It tranposes the characters on either side of the insertion cursor,
%% unless the cursor is at the end of the line.  In this case it
%% transposes the two characters to the left of the cursor.  In either
%% case, the cursor ends up to the right of the transposed characters.
%%
%% Arguments:
%% w -		Text window in which to transpose.

tkTextTranspose(W) ->
    Pos = "insert",
    Pos1 = case tk:cmd(W, [compare, Pos, "!=", Pos++ " lineend"]) of
	       1 -> index(W, Pos ++ " + 1 char");
	       0 -> Pos
	   end,
    New = tk:rcmd(W, [get, Pos1 ++ " - 1 char"]) ++ 
	tk:rcmd(W, [get, Pos1 ++ " - 2 char"]),
    case tk:cmd(W, [compare, Pos1 ++ " - 1 char", "==", "1.0"]) of
	1 -> false;
	0 ->
	    tk:cmd(W, [delete, Pos1 ++ " - 2 char", Pos1]),
	    tk:cmd(W, [insert, insert, New]),
	    tk:cmd(W, [see, insert])
    end.

%% tk_textCopy --
%% This procedure copies the selection from a text widget into the
%% clipboard.
%%
%% Arguments:
%% w -		Name of a text widget.

tk_textCopy(W) ->
    case tk:selection([own, {displayof, W}]) of
	W ->
	    tk:clipboard([clear, {displayof,W}]),
	    catch
		begin
		    Clip = tk:rselection([get,{displayof,W}]),
		    tk:clipboard([append, {displayof,W}, '--', Clip])
		end;
	_ -> 
	    false
    end.

%% tk_textCut --
%% This procedure copies the selection from a text widget into the
%% clipboard, then deletes the selection (if it exists in the given
%% widget).
%%
%% Arguments:
%% w -		Name of a text widget.

tk_textCut(W) ->
    case tk:selection([own, {displayof, W}]) of
	W ->
	    tk:clipboard([clear, {displayof, W}]),
	    catch
		begin
		    Clip = tk:rselection([get, {displayof,W}]),
		    tk:clipboard([append, {displayof,W}, '--', Clip]),
		    tk:cmd(W, [delete, "sel.first", "sel.last"])
		end;
	_ ->
	    false
    end.

%% tk_textPaste --
%% This procedure pastes the contents of the clipboard to the insertion
%% point in a text widget.
%%
%% Arguments:
%% w -		Name of a text widget.

tk_textPaste(W) ->
    catch
	begin
	    Clip = tk:rselection([get,{displayof, W},
				  {selection, "CLIPBOARD"}]),
	    tk:cmd(W, [insert, insert, Clip])
	end.

