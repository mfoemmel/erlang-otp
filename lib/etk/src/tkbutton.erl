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
%%
%% tkbutton.erl
%%
-module(tkbutton).

-include("tk.hrl").

-export([init/0]).

bindb(Event, Fun) ->
    tk:bind("Button", Event, ['%W'], Fun).

bindc(Event, Fun) ->
    tk:bind("Checkbutton", Event, ['%W'], Fun).

bindr(Event, Fun) ->
    tk:bind("Radiobutton", Event, ['%W'], Fun).

strict_bindc(Event, Fun) ->
    tk:bind("Checkbutton", Event, ['%W'],
	    fun(W) ->
		    case ?tkget(tk_strictMotif) of
			false -> Fun(W);
			true  -> false
		    end
	    end).

strict_bindr(Event, Fun) ->
    tk:bind("Radiobutton", Event, ['%W'],
	    fun(W) ->
		    case ?tkget(tk_strictMotif) of
			false -> Fun(W);
			true  -> false
		    end
	    end).


init() ->
    bindb("<FocusIn>", fun(_) -> true end),
    bindb("<Enter>",  fun tkButtonEnter/1),
    bindb("<Leave>", fun tkButtonLeave/1),
    bindb("<1>",  fun tkButtonDown/1),
    bindb("<ButtonRelease-1>", fun tkButtonUp/1),
    bindb("<space>", fun tkButtonInvoke/1),
    bindc("<FocusIn>", fun(_) -> true end),
    bindc("<Enter>", fun tkButtonEnter/1),
    bindc("<Leave>", fun tkButtonLeave/1),
    bindc("<1>", fun tkCheckRadioInvoke/1),
    bindc("<space>", fun tkCheckRadioInvoke/1),
    strict_bindc("<Return>", fun tkCheckRadioInvoke/1),
    bindr("<FocusIn>", fun(_) -> true end),
    bindr("<Enter>", fun tkButtonEnter/1),
    bindr("<Leave>", fun tkButtonLeave/1),
    bindr("<1>", fun tkCheckRadioInvoke/1),
    bindr("<space>", fun tkCheckRadioInvoke/1),
    strict_bindr("<Return>",  fun tkCheckRadioInvoke/1).

%% tkButtonEnter --
%% The procedure below is invoked when the mouse pointer enters a
%% button widget.  It records the button we're in and changes the
%% state of the button to active unless the button is disabled.
%%
%% Arguments:
%% w -		The name of the widget.

tkButtonEnter(W) ->
    case tk:cget(W, state) of
	"disabled" -> true;
	_ ->
	    tk:configure(W, [{state,active}]),
	    case ?tkget(buttonWindow) of
		W ->
		    tk:configure(W, [{state, active}, {relief, sunken}]);
		_ ->
		    false
	    end
    end,
    ?tkset(window, W).


%% tkButtonLeave --
%% The procedure below is invoked when the mouse pointer leaves a
%% button widget.  It changes the state of the button back to
%% inactive.  If we're leaving the button window with a mouse button
%% pressed (tkPriv(buttonWindow) == $w), restore the relief of the
%% button too.
%%
%% Arguments:
%% w -		The name of the widget.

tkButtonLeave(W) ->
    case tk:cget(W, state) of
	"disabled" -> true;
	_ -> tk:configure(W, [{state,normal}])
    end,
    case ?tkget(buttonWindow) of
	W -> tk:configure(W, [{relief, ?tkget(relief)}]);
	_ -> true
    end,
    ?tkset(window, "").

%% tkButtonDown --
%% The procedure below is invoked when the mouse button is pressed in
%% a button widget.  It records the fact that the mouse is in the button,
%% saves the button's relief so it can be restored later, and changes
%% the relief to sunken.
%%
%% Arguments:
%% w -		The name of the widget.

tkButtonDown(W) ->
    ?tkset(relief, tklib:index(4, tk:cmd(W, [configure, {relief}]))),
    case tk:cget(W, state) of
	"disabled" -> true;
	_ ->
	    ?tkset(buttonWindow, W),
	    tk:configure(W, [{relief,sunken}])
    end.

%% tkButtonUp --
%% The procedure below is invoked when the mouse button is released
%% in a button widget.  It restores the button's relief and invokes
%% the command as long as the mouse hasn't left the button.
%%
%% Arguments:
%% w -		The name of the widget.

tkButtonUp(W) ->
    case ?tkget(buttonWindow) of
	W ->
	    ?tkset(buttonWindow, ""),
	    tk:cmd(W, [configure, {relief, ?tkget(relief)}]),
	    case ?tkget(window) of
		W ->
		    case tk:cget(W, state) of
			"disabled" -> true;
			_ ->
			    tk:cmd(W, [invoke])
		    end;
		_ ->
		    false
	    end;
	_ ->
	    false
    end.


%% tkButtonInvoke --
%% The procedure below is called when a button is invoked through
%% the keyboard.  It simulate a press of the button via the mouse.
%%
%% Arguments:
%% w -		The name of the widget.

tkButtonInvoke(W) ->
    case tk:cget(W, state) of
	"disabled" -> true;
	_ ->
	    OldRelief = tk:cget(W,relief),
	    OldState = tk:cget(W, state),
	    tk:configure(W, [{state, active}, {relief, sunken}]),
	    tk:update(idletasks),
	    receive
	    after 100 -> true
	    end,
	    tk:configure(W, [{state,OldState},{relief,OldRelief}]),
	    tk:cmd(W, [invoke])
    end.

%% tkCheckRadioInvoke --
%% The procedure below is invoked when the mouse button is pressed in
%% a checkbutton or radiobutton widget, or when the widget is invoked
%% through the keyboard.  It invokes the widget if it
%% isn't disabled.
%%
%% Arguments:
%% w -		The name of the widget.

tkCheckRadioInvoke(W) ->
    case tk:cget(W, state) of
	"disabled" -> true;
	_ -> tk:cmd(W, [invoke])
    end.

