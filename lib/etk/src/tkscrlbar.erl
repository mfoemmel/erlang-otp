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
%% scrlbar.tcl --
%%
%% This file defines the default bindings for Tk scrollbar widgets.
%% It also provides procedures that help in implementing the bindings.
%%
%% @(#) scrollbar.tcl 1.13 95/06/29 14:03:03
%%
%% Copyright (c) 1994 The Regents of the University of California.
%% Copyright (c) 1994-1995 Sun Microsystems, Inc.
%%
%% See the file "license.terms" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
-module(tkscrlbar).

-export([init/0]).

-include("tk.hrl").

%%-------------------------------------------------------------------------
%% The code below creates the default class bindings for scrollbars.
%%-------------------------------------------------------------------------

bind(Event, Template, Fun) ->
    tk:bind("Scrollbar", Event, Template, Fun).

bindw(Event, Fun) ->
    tk:bind("Scrollbar", Event, ['%W'], Fun).

bindwxy(Event, Fun) ->
    tk:bind("Scrollbar", Event, ['%W','%x','%y'], Fun).

%% Standard Motif bindings:

init() ->
    bindwxy("<Enter>",
	 fun(W,X,Y) ->
		 case ?tkget(tk_strictMotif) of
		     true ->
			 ?tkset(activeBg,
				tk:cget(W, activebackground)),
			 tk:cmd(W, [configure, 
				    {activebackground,
				     tk:cget(W, background)}]);
		     false ->
			 false
		 end,
		 tk:cmd(W, [activate, tk:cmd(W, [identify,X,Y])])
	 end),
    bindwxy("<Motion>",
	 fun(W,X,Y) ->
		 tk:cmd(W, [activate, tk:cmd(W, [identify,X,Y])])
	 end),
    bindw("<Leave>",
	 fun(W) ->
		 case ?tkget(tk_strictMotif) of
		     true ->
			 tk:cmd(W,[configure,
				   {activebackground,?tkget(activeBg)}]);
		     false ->
			 false
		 end,
		 tk:cmd(W, [activate, ""])
	 end),
    bindwxy("<1>", fun(W,X,Y) -> tkScrollButtonDown(W,X,Y) end),
    bindwxy("<B1-Motion>", fun(W,X,Y) -> tkScrollDrag(W,X,Y) end),
    bindwxy("<B1-B2-Motion>", fun(W,X,Y) -> tkScrollDrag(W,X,Y) end),
    bindwxy("<ButtonRelease-1>", fun(W,X,Y) -> tkScrollButtonUp(W,X,Y) end),
    bind("<B1-Leave>",[],
	 fun() ->
		 %% Prevents <Leave> binding from being invoked.
		 false
	 end),
    bind("<B1-Enter>",[],
	 fun() ->
		 %% Prevents <Enter> binding from being invoked.
		 false
	 end),
    bindwxy("<2>",
	 fun(W,X,Y) -> tkScrollButton2Down(W,X,Y) end),
    bind("<B1-2>", [],
	 fun() ->
		 %% Do nothing, since button 1 is already down
		 false
	 end),
    bind("<B2-1>", [],
	 fun() ->
		 %% Do nothing, since button 2 is already down
		 false
	 end),
    bindwxy("<B2-Motion>", fun(W,X,Y) -> tkScrollDrag(W,X,Y) end),
    bindwxy("<ButtonRelease-2>", fun(W,X,Y) -> tkScrollButtonUp(W,X,Y) end),
    bind("<B1-ButtonRelease-2>", [],
	 fun() -> %% Do nothing:  B1 release will handle it
		 false
	 end),
    bind("<B2-ButtonRelease-1>", [],
	 fun() -> %% Do nothing:  B2 release will handle it
		 false
	 end),
    bind("<B2-Leave>", [],
	 fun() ->
		 %% Prevents <Leave> binding from being invoked.
		 false
	 end),
    bind("<B2-Enter>", [],
	 fun() ->
		 %% Prevents <Enter> binding from being invoked.
		 false
	 end),
    bindwxy("<Control-1>", fun(W,X,Y) -> tkScrollTopBottom(W,X,Y) end),
    bindwxy("<Control-2>", fun(W,X,Y) -> tkScrollTopBottom(W,X,Y) end),
    bindw("<Up>", fun(W) -> tkScrollByUnits(W,"v", -1) end),
    bindw("<Down>", fun(W) -> tkScrollByUnits(W,"v", 1) end),
    bindw("<Control-Up>", fun(W) -> tkScrollByPages(W,"v", -1) end),
    bindw("<Control-Down>", fun(W) -> tkScrollByPages(W,"v", 1) end),
    bindw("<Left>", fun(W) -> tkScrollByUnits(W,"h", -1) end),
    bindw("<Right>", fun(W) -> tkScrollByUnits(W,"h", 1) end),
    bindw("<Control-Left>", fun(W) -> tkScrollByPages(W,"h", -1) end),
    bindw("<Control-Right>", fun(W) -> tkScrollByPages(W,"h", 1) end),
    bindw("<Prior>", fun(W) -> tkScrollByPages(W,"hv", -1) end),
    bindw("<Next>", fun(W) -> tkScrollByPages(W,"hv", 1) end),
    bindw("<Home>", fun(W) -> tkScrollToPos(W, 0) end),
    bindw("<End>", fun(W) -> tkScrollToPos(W, 1) end),
    true.

%% tkScrollButtonDown --
%% This procedure is invoked when a button is pressed in a scrollbar.
%% It changes the way the scrollbar is displayed and takes actions
%% depending on where the mouse is.
%%
%% Arguments:
%% w -		The scrollbar widget.
%% x, y -	Mouse coordinates.

tkScrollButtonDown(W,X,Y) ->
    ?tkset(relief, tk:cget(W,activerelief)),
    tk:cmd(W,[configure, {activerelief, sunken}]),
    Element = tk:cmd(W, [identify, X, Y]),
    if
	Element == "slider" ->
	    tkScrollStartDrag(W, X, Y);
	true ->
	    tkScrollSelect(W, Element, initial)
    end.

%% tkScrollButtonUp --
%% This procedure is invoked when a button is released in a scrollbar.
%% It cancels scans and auto-repeats that were in progress, and restores
%% the way the active element is displayed.
%%
%% Arguments:
%% w -		The scrollbar widget.
%% x, y -	Mouse coordinates.

tkScrollButtonUp(W,X,Y) ->
    tk:tkCancelRepeat(),
    tk:cmd(W, [configure, {activerelief, ?tkget(relief)}]),
    tkScrollEndDrag(W, X, Y),
    tk:cmd(W, [activate, tk:cmd(W, [identify, X, Y])]).

%% tkScrollSelect --
%% This procedure is invoked when button 1 is pressed over the scrollbar.
%% It invokes one of several scrolling actions depending on where in
%% the scrollbar the button was pressed.
%%
%% Arguments:
%% w -		The scrollbar widget.
%% element -	The element of the scrollbar that was selected, such
%%		as "arrow1" or "trough2".  Shouldn't be "slider".
%% repeat -	Whether and how to auto-repeat the action:  "noRepeat"
%%		means don't auto-repeat, "initial" means this is the
%%		first action in an auto-repeat sequence, and "again"
%%		means this is the second repetition or later.

tkScrollSelect(W, Element, Repeat) ->
    case tk:winfo([exists, W]) of
	1 ->
	    case Element of
		"arrow1" ->
		    tkScrollByUnits(W, "hv", -1),
		    setRepeat(W, Element, Repeat);
		"trough1" ->
		    tkScrollByPages(W, "hv", -1),
		    setRepeat(W, Element, Repeat);
		"trough2" ->
		    tkScrollByPages(W, "hv", 1),
		    setRepeat(W, Element, Repeat);
		"arrow2" ->
		    tkScrollByUnits(W, "hv",  1),
		    setRepeat(W, Element, Repeat);
		_ ->
		    false
	    end;
	_ ->
	    false
    end.

setRepeat(W, Element, again) ->
    Interval = tk:cget(W, repeatinterval),
    tk:tkRepeat(Interval, fun() -> tkScrollSelect(W,Element,again) end);
setRepeat(W, Element, initial) ->
    case tk:cget(W, repeatdelay) of
	Delay when Delay > 0 ->
	    tk:tkRepeat(Delay, fun() -> tkScrollSelect(W, Element, again) end);
	_ ->
	    false
    end;
setRepeat(W, Element, _) ->
    false.

%% tkScrollStartDrag --
%% This procedure is called to initiate a drag of the slider.  It just
%% remembers the starting position of the mouse and slider.
%%
%% Arguments:
%% w -		The scrollbar widget.
%% x, y -	The mouse position at the start of the drag operation.

tkScrollStartDrag(W, X, Y) ->
    case tk:cget(W, command) of
	"" -> false;
	_  -> 
	    ?tkset(pressX, X),
	    ?tkset(pressY, Y),
	    Ivalues = tk:cmd(W, [get]),
	    ?tkset(initValues, Ivalues),
	    [Iv0,Iv1|IvT] = Ivalues,
	    if
		IvT == [] ->
		    ?tkset(initPos, Iv0);
		Iv0 == 0 ->
		    ?tkset(initPos, 0.0);
		true ->
		    [Iv2 | _] = IvT,
		    ?tkset(initPos, Iv2 / Iv0)
	    end
    end.

%% tkScrollDrag --
%% This procedure is called for each mouse motion even when the slider
%% is being dragged.  It notifies the associated widget if we're not
%% jump scrolling, and it just updates the scrollbar if we are jump
%% scrolling.
%%
%% Arguments:
%% w -		The scrollbar widget.
%% x, y -	The current mouse position.

tkScrollDrag(W, X, Y) ->
    case ?tkget(initPos) of
	"" -> false;
	Ipos  ->
	    Delta = tk:cmd(W, [delta,
			       X - ?tkget(pressX),
			       Y - ?tkget(pressY)]),
	    case tk:cget(W, jump) of
		1 ->
		    [Iv0,Iv1 | IvT] = ?tkget(initValues),
		    if
			IvT == [] ->
			    tk:cmd(W, [set,
				       Iv0 + Delta,
				       Iv1 + Delta]);
			true ->
			    Delta1 = round(Delta * Iv0),
			    [Iv2, Iv3 | IvT1] = IvT,
			    tk:cmd(W, [set,
				       Iv0, Iv1,
				       Iv2 + Delta1,
				       Iv3 + Delta1])
		    end;
		0 ->
		    tkScrollToPos(W, Ipos + Delta)
	    end
    end.

%% tkScrollEndDrag --
%% This procedure is called to end an interactive drag of the slider.
%% It scrolls the window if we're in jump mode, otherwise it does nothing.
%%
%% Arguments:
%% w -		The scrollbar widget.
%% x, y -	The mouse position at the end of the drag operation.

tkScrollEndDrag(W, X, Y) ->
    case ?tkget(initPos) of
	"" -> false;
	Pos ->
	    case tk:cget(W, jump) of
		1 ->
		    Delta = tk:cmd(W, [delta, 
				       X - ?tkget(pressX),
				       Y - ?tkget(pressY)]),
		    tkScrollToPos(W, Pos + Delta);
		0 ->
		    false
	    end
    end,
    ?tkset(initPos, "").

%% tkScrollByUnits --
%% This procedure tells the scrollbar's associated widget to scroll up
%% or down by a given number of units.  It notifies the associated widget
%% in different ways for old and new command syntaxes.
%%
%% Arguments:
%% w -		The scrollbar widget.
%% orient -	Which kinds of scrollbars this applies to:  "h" for
%%		horizontal, "v" for vertical, "hv" for both.
%% amount -	How many units to scroll:  typically 1 or -1.

tkScrollByUnits(W, Orient, Amount) ->
    case tk:cget(W, command) of
	"" -> false;
	["invoke", Id | _] ->
	    WOrient = tk:cget(W, orient),
	    Fun = tk:tkfun(Id),
	    Ix = tklib:first(tklib:strindex(0, WOrient), Orient),
	    if
		Ix < 0 ->
		    false;
		true ->
		    case tk:cmd(W, [get]) of
			[_, _] ->
			    Fun([scroll, Amount, units]);
			[_, _,  V2 | _] ->
			    Fun(V2 + Amount)
		    end
	    end
    end.

%% tkScrollByPages --
%% This procedure tells the scrollbar's associated widget to scroll up
%% or down by a given number of screenfuls.  It notifies the associated
%% widget in different ways for old and new command syntaxes.
%%
%% Arguments:
%% w -		The scrollbar widget.
%% orient -	Which kinds of scrollbars this applies to:  "h" for
%%		horizontal, "v" for vertical, "hv" for both.
%% amount -	How many screens to scroll:  typically 1 or -1.

tkScrollByPages(W, Orient, Amount) ->
    case tk:cget(W, command) of
	"" -> false;
	["invoke", Id | _] ->
	    WOrient = tk:cget(W, orient),
	    Fun = tk:tkfun(Id),
	    Ix = tklib:first(tklib:strindex(0, WOrient), Orient),
	    if
		Ix < 0 ->
		    false;
		true ->
		    case tk:cmd(W, [get]) of
			[_, _] ->
			    Fun([scroll, Amount, pages]);
			[_, V2, V1 | _] ->
			    Fun(V1 + Amount*V2 - 1)
		    end
	    end
    end.

%% tkScrollToPos --
%% This procedure tells the scrollbar's associated widget to scroll to
%% a particular location, given by a fraction between 0 and 1.  It notifies
%% the associated widget in different ways for old and new command syntaxes.
%%
%% Arguments:
%% w -		The scrollbar widget.
%% pos -	A fraction between 0 and 1 indicating a desired position
%%		in the document.

tkScrollToPos(W, Pos) ->
    case tk:cget(W, command) of
	"" -> false;
	["invoke", Id | _]  ->
	    Fun = tk:tkfun(Id),
	    case tk:cmd(W, [get]) of
		[_, _] -> Fun([moveto, Pos]);
		[V0 | _] -> Fun(round(V0)*Pos)
	    end
    end.

%% tkScrollTopBottom
%% Scroll to the top or bottom of the document, depending on the mouse
%% position.
%%
%% Arguments:
%% w -		The scrollbar widget.
%% x, y -	Mouse coordinates within the widget.

tkScrollTopBottom(W, X, Y) ->
    case tk:cmd(W, [identify, X, Y]) of
	"" -> false;
	Element ->
	    case lists:last(Element) of
		$1 -> tkScrollToPos(W, 0);
		$2 -> tkScrollToPos(W, 1);
		_ -> false
	    end
    end,
    ?tkset(relief, tk:cget(W, activerelief)).


%% tkScrollButton2Down
%% This procedure is invoked when button 2 is pressed over a scrollbar.
%% If the button is over the trough or slider, it sets the scrollbar to
%% the mouse position and starts a slider drag.  Otherwise it just
%% behaves the same as button 1.
%%
%% Arguments:
%% w -		The scrollbar widget.
%% x, y -	Mouse coordinates within the widget.

tkScrollButton2Down(W,X,Y) ->
    case tk:cmd(W, [identify, X, Y]) of
	"arrow1" -> tkScrollButtonDown(W, X, Y);
	"arrow2" -> tkScrollButtonDown(W, X, Y);
	_ ->
	    tkScrollToPos(W, tk:cmd(W, [fraction, X, Y])),
	    ?tkset(relief, tk:cget(W, activerelief)),
	    %% Need the "update idletasks" below so that the widget calls us
	    %% back to reset the actual scrollbar position before we start the
	    %% slider drag.

	    %% tk:update(idletasks),
	    tk:cmd(W, [configure, {activerelief, sunken}]),
	    tk:cmd(W, [activate, slider]),
	    tkScrollStartDrag(W, X, Y)
    end.


