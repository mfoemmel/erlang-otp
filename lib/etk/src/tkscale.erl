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
%% scale.tcl --
%%
%% This file defines the default bindings for Tk scale widgets and provides
%% procedures that help in implementing the bindings.
%%
%% SCCS: @(%%) scale.tcl 1.12 96/04/16 11:42:25
%%
%% Copyright (c) 1994 The Regents of the University of California.
%% Copyright (c) 1994-1995 Sun Microsystems, Inc.
%%
%% See the file "license.terms" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
-module(tkscale).

-include("tk.hrl").

-export([init/0]).

%%-------------------------------------------------------------------------
%% The code below creates the default class bindings for entries.
%%-------------------------------------------------------------------------

bind(Event, Template, Fun) ->
    tk:bind("Scale", Event, Template, Fun).

bindw(Event, Fun) ->
    tk:bind("Scale", Event, ['%W'], Fun).

bindwxy(Event, Fun) ->
    tk:bind("Scale", Event, ['%W','%x','%y'], Fun).

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
		 tkScaleActivate(W, X, Y)
	 end),
    bindwxy("<Motion>",
	 fun(W,X,Y) ->
		 tkScaleActivate(W,X, Y)
	 end),
    bindw("<Leave>",
	 fun(W) ->
		 case ?tkget(tk_strictMotif) of
		     true ->
			 tk:cmd(W,[configure,
				   {activebackground,?tkget(activeBg)}]);
		     false -> false
		 end,
		 case tk:cget(W, state) of
		     "active" ->
			 tk:cmd(W, [configure,{state,normal}]);
		     _ -> false
		 end
	 end),

    bindwxy("<1>",
	 fun(W,X,Y) ->
		 tkScaleButtonDown(W, X, Y)
	 end),
    bindwxy("<B1-Motion>",
	 fun(W,X,Y) ->
		 tkScaleDrag(W, X, Y)
	 end),

    bind("<B1-Leave>", [], fun() -> false end),
    bind("<B1-Enter>", [], fun() -> false end),
    
    bindwxy("<ButtonRelease-1>",
	 fun(W,X,Y) ->
		 tk:tkCancelRepeat(),
		 tkScaleEndDrag(W),
		 tkScaleActivate(W,X,Y)
	 end),
    bindwxy("<2>",
	 fun(W,X,Y) ->
		 tkScaleButton2Down(W, X, Y)
	 end),
    bindwxy("<B2-Motion>",
	 fun(W,X,Y) ->
		 tkScaleDrag(W, X, Y)
	 end),
    bind("<B2-Leave>", [], fun() -> false end),
    bind("<B2-Enter>", [], fun() -> false end),
    bindwxy("<ButtonRelease-2>",
	 fun(W,X,Y) ->
		 tk:tkCancelRepeat(),
		 tkScaleEndDrag(W),
		 tkScaleActivate(W,X,Y)
	 end),
    bindwxy("<Control-1>",
	 fun(W,X,Y) ->
		 tkScaleControlPress(W, X, Y)
	 end),
    bindw("<Up>",
	 fun(W) ->
		 tkScaleIncrement(W, up, little, noRepeat)
	 end),
    bindw("<Down>",
	 fun(W) ->
		 tkScaleIncrement(W, down, little, noRepeat)
	 end),
    bindw("<Left>",
	 fun(W) ->
		 tkScaleIncrement(W, up, little, noRepeat)
	 end),
    bindw("<Right>",
	 fun(W) ->
		 tkScaleIncrement(W, down, little, noRepeat)
	 end),
    bindw("<Control-Up>",
	 fun(W) ->
		 tkScaleIncrement(W, up, big, noRepeat)
	 end),
    bindw("<Control-Down>",
	 fun(W) ->
		 tkScaleIncrement(W, down, big, noRepeat)
	 end),
    bindw("<Control-Left>",
	 fun(W) ->
		 tkScaleIncrement(W, up, big, noRepeat)
	 end),
    bindw("<Control-Right>",
	 fun(W) ->
		 tkScaleIncrement(W, down, big, noRepeat)
	 end),
    bindw("<Home>",
	 fun(W) ->
		 tk:cmd(W, [set, tk:cget(W, from)])
	 end),
    bindw("<End>",
	 fun(W) ->
		 tk:cmd(W, [set, tk:cget(W, to)])
	 end).

%% tkScaleActivate --
%% This procedure is invoked to check a given x-y position in the
%% scale and activate the slider if the x-y position falls within
%% the slider.
%%
%% Arguments:
%% w -		The scale widget.
%% x, y -	Mouse coordinates.

tkScaleActivate(W, X, Y) ->
    case tk:cget(W, state) of
	"disabled" -> false;
	_ ->
	    case tk:cmd(W, [identify, X, Y]) of
		"slider" ->
		    tk:cmd(W, [configure, {state, active}]);
		_ ->
		    tk:cmd(W, [configure, {state, normal}])
	    end
    end.
		    
%% tkScaleButtonDown --
%% This procedure is invoked when a button is pressed in a scale.  It
%% takes different actions depending on where the button was pressed.
%%
%% Arguments:
%% w -		The scale widget.
%% x, y -	Mouse coordinates of button press.

tkScaleButtonDown(W, X, Y) ->
    ?tkset(dragging,  0),
    case tk:cmd(W, [identify, X, Y]) of
	"trough1" ->
	    tkScaleIncrement(W, up, little, initial);
	"trough2" ->
	    tkScaleIncrement(W, down, little, initial);
	"slider" ->
	    ?tkset(dragging,  1),
	    ?tkset(initValue, tk:cmd(W, [get])),
	    [X0,Y0|_] = tk:cmd(W, [coords]),
	    ?tkset(deltaX, X - X0),
	    ?tkset(deltaY, Y - Y0),
	    tk:cmd(W, [configure, {sliderrelief, sunken}]);
	_ ->
	    true
    end.

%% tkScaleDrag --
%% This procedure is called when the mouse is dragged with
%% mouse button 1 down.  If the drag started inside the slider
%% (i.e. the scale is active) then the scale's value is adjusted
%% to reflect the mouse's position.
%%
%% Arguments:
%% w -		The scale widget.
%% x, y -	Mouse coordinates.

tkScaleDrag(W, X, Y) ->
    case ?tkget(dragging) of
	0 -> false;
	1 ->
	    tk:cmd(W, [set, 
		       tk:cmd(W, [get,
				  X - ?tkget(deltaX),
				  Y - ?tkget(deltaY)])])
    end.

%% tkScaleEndDrag --
%% This procedure is called to end an interactive drag of the
%% slider.  It just marks the drag as over.
%%
%% Arguments:
%% w -		The scale widget.

tkScaleEndDrag(W) ->
    ?tkset(dragging,  0),
    tk:cmd(W, [configure, {sliderrelief, raised}]).

%% tkScaleIncrement --
%% This procedure is invoked to increment the value of a scale and
%% to set up auto-repeating of the action if that is desired.  The
%% way the value is incremented depends on the "dir" and "big"
%% arguments.
%%
%% Arguments:
%% w -		The scale widget.
%% dir -		"up" means move value towards -from, "down" means
%%		move towards -to.
%% big -		Size of increments: "big" or "little".
%% repeat -	Whether and how to auto-repeat the action:  "noRepeat"
%%		means don't auto-repeat, "initial" means this is the
%%		first action in an auto-repeat sequence, and "again"
%%		means this is the second repetition or later.

tkScaleIncrement(W, Dir, Big, Repeat) ->
    case tk:winfo([exists, W]) of
	0 -> false;
	1 ->
	    From = tk:cget(W, from),
	    To = tk:cget(W, to),
	    Inc1 = case Big of
		       big ->
			   case tk:cget(W, bigincreament) of
			       0 ->
				   (To - From) / 10.0;
			       Inc0 ->
				   Res = tk:cget(W, resolution),
				   if
				       Inc0 < Res -> Res;
				       true -> Inc0
				   end
			   end;
		       _ ->
			   tk:cget(W, resolution)
		   end,
	    Inc2 = if
		       From > To, Dir =/= up -> -Inc1;
		       From =< To, Dir == up -> -Inc1;
		       true -> Inc1
		   end,
	    tk:cmd(W, [set, tk:cmd(W, [get]) + Inc2]),
	    case Repeat of
		again ->
		    Interval = tk:cget(W, repeatinterval),
		    tk:tkRepeat(Interval, 
				fun() -> tkScaleIncrement(W, Dir, Big, again) end);
		initial ->
		    Delay = tk:cget(W, repeatdelay),
		    if
			Delay > 0 ->
			    tk:tkRepeat(Delay, 
					fun() -> tkScaleIncrement(W, Dir, Big, again) end);
			true ->
			    false
		    end;
		_ ->
		    false
	    end
    end.


%% tkScaleControlPress --
%% This procedure handles button presses that are made with the Control
%% key down.  Depending on the mouse position, it adjusts the scale
%% value to one end of the range or the other.
%%
%% Arguments:
%% w -		The scale widget.
%% x, y -	Mouse coordinates where the button was pressed.

tkScaleControlPress(W, X, Y) ->
    case tk:cmd(W, [identify, X, Y]) of
	"trough1" ->
	    tk:cmd(W, [set, tk:cget(W, from)]);
	"trough2" ->
	    tk:cmd(W, [set, tk:cget(W, to)]);
	_ ->
	    false
    end.

%% tkScaleButton2Down
%% This procedure is invoked when button 2 is pressed over a scale.
%% It sets the value to correspond to the mouse position and starts
%% a slider drag.
%%
%% Arguments:
%% w -		The scrollbar widget.
%% x, y -	Mouse coordinates within the widget.

tkScaleButton2Down(W, X, Y) ->
    case tk:cget(W, state) of
	"disabled" -> false;
	_ ->
	    tk:cmd(W, [configure, {state, active}]),
	    tk:cmd(W, [set, tk:cmd(W, [get,X,Y])]),
	    ?tkset(dragging,  1),
	    ?tkset(initValue, tk:cmd(W, [get])),
	    ?tkset(deltaX, 0),
	    ?tkset(deltaY, 0)
    end.

