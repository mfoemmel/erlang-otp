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
%% listbox.tcl --
%%
%% This file defines the default bindings for Tk listbox widgets
%% and provides procedures that help in implementing those bindings.
%%
%% SCCS: @(#) listbox.tcl 1.16 96/04/16 11:42:22
%%
%% Copyright (c) 1994 The Regents of the University of California.
%% Copyright (c) 1994-1995 Sun Microsystems, Inc.
%%
%% See the file "license.terms" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.

-module(tklistbox).

-include("tk.hrl").

-export([init/0]).

%%--------------------------------------------------------------------------
%% tkPriv elements used in this file:
%%
%% afterId -		Token returned by "after" for autoscanning.
%% listboxPrev -		The last element to be selected or deselected
%%			during a selection operation.
%% listboxSelection -	All of the items that were selected before the
%%			current selection operation (such as a mouse
%%			drag) started;  used to cancel an operation.
%%--------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% The code below creates the default class bindings for listboxes.
%%-------------------------------------------------------------------------

%% Note: the check for existence of %W below is because this binding
%% is sometimes invoked after a window has been deleted (e.g. because
%% there is a double-click binding on the widget that deletes it).  Users
%% can put "break"s in their bindings to avoid the error, but this check
%% makes that unnecessary.

bind(Event, Template, Fun) ->
    tk:bind("Listbox", Event, Template, Fun).

bindw(Event, Fun) ->
    tk:bind("Listbox", Event, ['%W'], Fun).

bindwxy(Event, Fun) ->
    tk:bind("Listbox", Event, ['%W','%x','%y'], Fun).

mkpos(X,Y) ->
    "@" ++ integer_to_list(X) ++ "," ++ integer_to_list(Y).

index(W, Where) ->
    tk:cmd(W, [index, Where]).

init() ->
    bindwxy("<1>",
	 fun(W,X,Y) ->
		 case tk:winfo([exists, W]) of
		     1 ->
			 tkListboxBeginSelect(W, index(W, mkpos(X,Y)));
		     0 ->
			 false
		 end
	 end),

%% Ignore double clicks so that users can define their own behaviors.
%% Among other things, this prevents errors if the user deletes the
%% listbox on a double click.

    bind("<2>", [], fun() -> false end),

    bindwxy("<B1-Motion>", 
	 fun(W,X,Y) ->
		 ?tkset(x, X),
		 ?tkset(y, Y),
		 tkListboxMotion(W, index(W,mkpos(X,Y)))
	 end),

    bindwxy("<ButtonRelease-1>",
	 fun(W,X,Y) ->
		 tk:tkCancelRepeat(),
		 tk:cmd(W, [activate, mkpos(X,Y)])
	 end),

    bindwxy("<Shift-1>",
	 fun(W,X,Y) ->
		 tkListboxBeginExtend(W, index(W,mkpos(X,Y)))
	 end),
    bindwxy("<Control-1>",
	 fun(W,X,Y) ->
		 tkListboxBeginToggle(W, index(W,mkpos(X,Y)))
	 end),

    bindwxy("<B1-Leave>",
	 fun(W,X,Y) ->
		 ?tkset(x, X),
		 ?tkset(y, Y),
		 tkListboxAutoScan(W)
	 end),
    bind("<B1-Enter>", [], fun() -> tk:tkCancelRepeat() end),

    bindw("<Up>", fun(W) -> tkListboxUpDown(W, -1) end),

    bindw("<Shift-Up>",
	 fun(W) -> tkListboxExtendUpDown(W, -1) end),

    bindw("<Down>",
	 fun(W) -> tkListboxUpDown(W, 1) end),

    bindw("<Shift-Down>",
	 fun(W) -> tkListboxExtendUpDown(W, 1) end),

    bindw("<Left>",
	 fun(W) -> tk:cmd(W, [xview, scroll,-1, units]) end),
    bindw("<Control-Left>",
	  fun(W) -> tk:cmd(W,[xview, scroll,-1,pages])
	 end),

    bindw("<Right>",
	 fun(W) -> tk:cmd(W,[xview, scroll,1, units])
	 end),
    bindw("<Control-Right>",
	 fun(W) -> tk:cmd(W,[xview, scroll, 1, pages])
	 end),
    bindw("<Prior>",
	 fun(W) ->
		 tk:cmd(W, [yview, scroll, -1, pages]),
		 tk:cmd(W, [activate, "@0,0"])
	 end),
    bindw("<Next>",
	 fun(W) ->
		 tk:cmd(W, [yview, scroll, 1, pages]),
		 tk:cmd(W, [activate, "@0,0"])
	 end),

    bindw("<Control-Prior>",
	 fun(W) -> tk:cmd(W,[xview, scroll,-1,pages])
	 end),
    bindw("<Control-Next>",
	 fun(W) -> tk:cmd(W,[xview, scroll,1,pages])
	 end),
    bindw("<Home>",
	 fun(W) -> tk:cmd(W, [xview, moveto, 0]) end),
    bindw("<End>",
	 fun(W) -> tk:cmd(W, [xview, moveto, 1]) end),

    bindw("<Control-Home>",
	 fun(W) ->
		 tk:cmd(W, [activate, 0]),
		 tk:cmd(W, [see, 0]),
		 tk:cmd(W, [selection, clear, 0, 'end']),
		 tk:cmd(W, [selection, set, 0])
	 end),

    bindw("<Shift-Control-Home>", fun(W) -> tkListboxDataExtend(W, 0) end),

    bindw("<Control-End>",
	 fun(W) ->
		 tk:cmd(W, [activate, 'end']),
		 tk:cmd(W, [see, 'end']),
		 tk:cmd(W, [selection, clear, 0, 'end']),
		 tk:cmd(W, [selection, set, 'end'])
	 end),

    bindw("<Shift-Control-End>",
	 fun(W) -> tkListboxDataExtend(W, 'end') end),	 
    bindw("<<Copy>>", 
	 fun(W) ->
		 case tk:selection([own, {displayof, W}]) of
		     W ->
			 tk:clipboard([clear, {displayof, W}]),
			 tk:clipboard([append, 
				       {displayof, W}, 
				       tk:selection([get,{displayof,W}])]);
		     _ -> false
		 end
	 end),

    bindw("<space>",
	 fun(W) ->
		 tkListboxBeginSelect(W, index(W, activate))
	 end),

    bindw("<Select>",
	 fun(W) ->
		 tkListboxBeginSelect(W, index(W, activate))
	 end),

    bindw("<Control-Shift-space>",
	 fun(W) ->
		 tkListboxBeginExtend(W, index(W, activate))
	 end),

    bindw("<Shift-Select>",
	 fun(W) ->
		 tkListboxBeginExtend(W, index(W,activate))
	 end),

    bindw("<Escape>", fun(W) -> tkListboxCancel(W) end),
    bindw("<Control-slash>", fun(W) -> tkListboxSelectAll(W) end),
    bindw("<Control-backslash>",
	 fun(W) ->
		 case tk:cget(W, selectmode) of
		     "browse" -> 
			 false;
		     _ -> 
			 tk:cmd(W, [selection, clear, 0, 'end'])
		 end
	 end),

%% Additional Tk bindings that aren't part of the Motif look and feel:

    bindwxy("<2>", 
	 fun(W,X,Y) -> tk:cmd(W, [scan, dragto, X, Y]) end),
    bindwxy("<B2-Motion>",
	 fun(W,X,Y) -> tk:cmd(W, [scan, dragto, X, Y]) end).

%% tkListboxBeginSelect --
%%
%% This procedure is typically invoked on button-1 presses.  It begins
%% the process of making a selection in the listbox.  Its exact behavior
%% depends on the selection mode currently in effect for the listbox;
%% see the Motif documentation for details.
%%
%% Arguments:
%% w -		The listbox widget.
%% el -		The element for the selection operation (typically the
%%		one under the pointer).  Must be in numerical form.

tkListboxBeginSelect(W, El) ->
    case tk:cget(W, selectmode) of
	"multiple" ->
	    case tk:cmd(W, [selection, includes, El]) of
		1 ->
		    tk:cmd(W, [selection, clear, El]);
		0 ->
		    tk:cmd(W, [selection, set, El])
	    end;
	_ ->
	    tk:cmd(W, [selection, clear, 0, 'end']),
	    tk:cmd(W, [selection, set, El]),
	    tk:cmd(W, [selection, anchor, El]),
	    ?tkset(listboxSelection, ""),
	    ?tkset(listboxPrev, El)
    end.


%% tkListboxMotion --
%%
%% This procedure is called to process mouse motion events while
%% button 1 is down.  It may move or extend the selection, depending
%% on the listbox's selection mode.
%%
%% Arguments:
%% w -		The listbox widget.
%% el -		The element under the pointer (must be a number).

tkListboxMotion(W, El) ->
    case ?tkget(listboxPrev) of
	El -> 
	    false;
	_ ->
	    Anchor = index(W, anchor),
	    case tk:cget(W, selectmode) of
		"browse" ->
		    tk:cmd(W, [selection, clear, 0,'end']),
                    tk:cmd(W, [selection, set, El]),
		    ?tkset(listboxPrev, El);
		"extended" ->
		    I = ?tkget(listboxPrev),
		    tk:cmd(W, [selection,clear, I, El]),
		    case tk:cmd(W, [selection, includes, anchor]) of
			1 ->
			    tk:cmd(W, [selection,set,anchor, El]);
			0 ->
			    tk:cmd(W, [selection,clear,anchor,El])
		    end,
		    Sel = ?tkget(listboxSelection),
		    I1 = scanForward(W, I, El, Anchor, Sel),
		    scanBackward(W, I1, El, Anchor, Sel),
		    ?tkset(listboxPrev, El);
		_ ->
		    false
	    end
    end.


scanForward(W, I, El, Anchor, Sel) when I < El, I < Anchor ->
    case tklib:search(Sel, I) of
	-1 -> scanForward(W, I+1, El, Anchor, Sel);
	Ix ->
	    tk:cmd(W, [selection, set, I]),
	    scanForward(W, I+1, El, Anchor, Sel)
    end;
scanForward(_, I, _, _, _) -> I.

scanBackward(W, I, El, Anchor, Sel) when I > El, I > Anchor ->
    case tklib:search(Sel, I) of
	-1 -> scanBackward(W, I-1, El, Anchor, Sel);
	Ix ->
	    tk:cmd(W, [selection, set, I]),
	    scanBackward(W, I-1, El, Anchor, Sel)
    end;
scanBackward(_, I, _, _, _) -> I.


%% tkListboxBeginExtend --
%%
%% This procedure is typically invoked on shift-button-1 presses.  It
%% begins the process of extending a selection in the listbox.  Its
%% exact behavior depends on the selection mode currently in effect
%% for the listbox;  see the Motif documentation for details.
%%
%% Arguments:
%% w -		The listbox widget.
%% el -		The element for the selection operation (typically the
%%		one under the pointer).  Must be in numerical form.

tkListboxBeginExtend(W, El) ->
    case tk:cget(W, selectmode) of
	"extended" ->
	    case tk:cmd(W, [selection, includes, anchor]) of
		1 ->
		    tkListboxMotion(W, El);
		0 ->
		    false
	    end;
	_ ->
	    false
    end.

%% tkListboxBeginToggle --
%%
%% This procedure is typically invoked on control-button-1 presses.  It
%% begins the process of toggling a selection in the listbox.  Its
%% exact behavior depends on the selection mode currently in effect
%% for the listbox;  see the Motif documentation for details.
%%
%% Arguments:
%% w -		The listbox widget.
%% el -		The element for the selection operation (typically the
%%		one under the pointer).  Must be in numerical form.

tkListboxBeginToggle(W, El) ->
    case tk:cget(W, selectmode) of
	"extended" ->
	    ?tkset(listboxSelection, tk:cmd(W, ["curselection"])),
	    ?tkset(listboxPrev, El),
	    tk:cmd(W, [selection, anchor, El]),
	    case tk:cmd(W, [selection, includes, El]) of
		1 ->
		    tk:cmd(W, [selection, clear, El]);
		0 ->
		    tk:cmd(W, [selection, set, El])
	    end;
	_ ->
	    false
    end.


%% tkListboxAutoScan --
%% This procedure is invoked when the mouse leaves an entry window
%% with button 1 down.  It scrolls the window up, down, left, or
%% right, depending on where the mouse left the window, and reschedules
%% itself as an "after" command so that the window continues to scroll until
%% the mouse moves back into the window or the mouse button is released.
%%
%% Arguments:
%% w -		The entry window.

tkListboxAutoScan(W) ->
    case tk:winfo([exists, W]) of
	0 -> false;
	1 ->
	    X = ?tkget(x),
	    Y = ?tkget(y),
	    Height = tk:winfo([height, W]),
	    Width = tk:winfo([width, W]),
	    Auto = if
		       Y >= Height ->
			   tk:cmd(W, [yview, scroll, 1, units]),
			   true;
		       Y < 0 ->
			   tk:cmd(W, [yview, scroll, -1, units]),
			   true;		    
		       X >= Width ->
			   tk:cmd(W, [xview, scroll, 2, units]),
			   true;
		       X < 0 ->
			   tk:cmd(W, [xview, scroll, -2, units]),
			   true;
		       true  ->
			   false
		   end,
	    case Auto of
		true ->
		    tkListboxMotion(W, index(W, mkpos(X, Y))),
		    tk:tkRepeat(50, fun() -> tkListboxAutoScan(W)  end);
		false ->
		    false
	    end
    end.

%% tkListboxUpDown --
%%
%% Moves the location cursor (active element) up or down by one element,
%% and changes the selection if we're in browse or extended selection
%% mode.
%%
%% Arguments:
%% w -		The listbox widget.
%% amount -	+1 to move down one item, -1 to move back one item.

tkListboxUpDown(W, Amount) ->
    tk:cmd(W, [activate, index(W,active) + Amount]),
    tk:cmd(W, [see, active]),
    case tk:cget(W, selectmode) of
	"browse" ->
	    tk:cmd(W, [selection,clear,0,'end']),
	    tk:cmd(W, [selection,set,active]);
	"extended" ->
	    tk:cmd(W, [selection,clear,0,'end']),
	    tk:cmd(W, [selection,set,active]),
	    tk:cmd(W, [selection, anchor, active]),
	    ?tkset(listboxPrev, index(W, active)),
	    ?tkset(listboxSelection, "");
	_ ->
	    false
    end.


%% tkListboxExtendUpDown --
%%
%% Does nothing unless we're in extended selection mode;  in this
%% case it moves the location cursor (active element) up or down by
%% one element, and extends the selection to that point.
%%
%% Arguments:
%% w -		The listbox widget.
%% amount -	+1 to move down one item, -1 to move back one item.

tkListboxExtendUpDown(W, Amount) ->
    case tk:cget(W, selectmode) of
	"extended" ->
	    tk:cmd(W, [activate, index(W, active) + Amount]),
	    tk:cmd(W, [see, active]),
	    tkListboxMotion(W, index(W, active));
	_ -> 
	    false
    end.


%% tkListboxDataExtend
%%
%% This procedure is called for key-presses such as Shift-KEndData.
%% If the selection mode isn't multiple or extend then it does nothing.
%% Otherwise it moves the active element to el and, if we're in
%% extended mode, extends the selection to that point.
%%
%% Arguments:
%% w -		The listbox widget.
%% el -		An integer element number.

tkListboxDataExtend(W, El) ->
    Mode = tk:cget(W, selectmode),
    if
	Mode == "extended" ->
	    tk:cmd(W, [activate, El]),
	    tk:cmd(W, [see, El]),
	    case tk:cmd(W, [selection, includes ,anchor]) of
		1 ->
		    tkListboxMotion(W, El);
		0 ->
		    false
	    end;
	Mode == "multiple" ->
	    tk:cmd(W, [activate, El]),
	    tk:cmd(W, [see, El]);
	true ->
	    false
    end.

%% tkListboxCancel
%%
%% This procedure is invoked to cancel an extended selection in
%% progress.  If there is an extended selection in progress, it
%% restores all of the items between the active one and the anchor
%% to their previous selection state.
%%
%% Arguments:
%% w -		The listbox widget.

tkListboxCancel(W) ->
    case tk:cget(W, selectmode) of
	"extended" ->
	    First = index(W, anchor),
	    Last = ?tkget(listboxPrev),
	    {First1, Last1} = 
		if First > Last ->
			{Last, First};
		   true ->
			{First, Last}
		end,
	    tk:cmd(W, [selection, clear, First1, Last1]),
	    Sel = ?tkget(listboxSelection),
	    cancelForward(W, First1, Last1, Sel);
	_ ->
	    false
    end.

cancelForward(W, I, Last, Sel) when I =< Last ->
    case tklib:search(Sel, I) of
	-1 ->
	    cancelForward(W, I+1, Last, Sel);
	Ix ->
	    tk:cmd(W, [selection, set,  I]),
	    cancelForward(W, I+1, Last, Sel)
    end;
cancelForward(W, I, _, _) -> I.
    
%% tkListboxSelectAll
%%
%% This procedure is invoked to handle the "select all" operation.
%% For single and browse mode, it just selects the active element.
%% Otherwise it selects everything in the widget.
%%
%% Arguments:
%% w -		The listbox widget.

tkListboxSelectAll(W) ->
    Mode = tk:cget(W, selectmode),
    if 
	Mode == "single" ->
	    tk:cmd(W, [selection,clear, 0,'end']),
	    tk:cmd(W, [selection,set,active]);
	Mode == "browse" ->
	    tk:cmd(W, [selection,clear, 0,'end']),
	    tk:cmd(W, [selection,set,active]);
	true ->
	    tk:cmd(W, [selection,set, 0,'end'])
    end.
