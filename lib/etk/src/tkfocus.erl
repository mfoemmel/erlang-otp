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
%% focus.tcl --
%%
%% This file defines several procedures for managing the input
%% focus.
%%
%% @(#) focus.tcl 1.14 95/05/04 13:31:48
%%
%% Copyright (c) 1994-1995 Sun Microsystems, Inc.
%%
%% See the file "license.terms" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%

-module(tkfocus).

-export([tk_focusNext/1, tk_focusPrev/1,
	 tkFocusOK/1, tk_focusFollowsMouse/0]).
-import(lists, [reverse/1, append/1]).

-include("tk.hrl").

%% tk_focusNext --
%% This procedure returns the name of the next window after "w" in
%% "focus order" (the window that should receive the focus next if
%% Tab is typed in w).  "Next" is defined by a pre-order search
%% of a top-level and its non-top-level descendants, with the stacking
%% order determining the order of siblings.  The "-takefocus" options
%% on windows determine whether or not they should be skipped.
%%
%% Arguments:
%% w -		Name of a window.

tk_focusNext(W) ->
    fNext(tk:childrenof(W), W, W).

fNext([], Parent, Init) ->
    case tk:toplevelof(Parent) of
	Parent ->
	    fNext(tk:childrenof(Parent), Parent, Init);
	_ ->
	    Parent1 = tk:parentof(Parent),
	    fNext(membertl(tk:childrenof(Parent1), Parent), 
		  Parent1, Init)
    end;
fNext([Init|_], _, Init) ->
    Init;
fNext([W|Sibs], Parent, Init) ->
    case tk:toplevelof(W) of
	W -> 
	    fNext(Sibs, Parent, Init);
	_ ->
	    case tkFocusOK(W) of
		true -> 
		    W;
		false ->
		    fNext(tk:childrenof(W), W, Init)
	    end
    end.

%%
%% Return the tail after element X
%%
membertl([X|Xs], X) -> Xs;
membertl([_|Xs], X) -> membertl(Xs, X);
membertl([], _) -> [].


%% tk_focusPrev --
%% This procedure returns the name of the previous window before "w" in
%% "focus order" (the window that should receive the focus next if
%% Shift-Tab is typed in w).  "Next" is defined by a pre-order search
%% of a top-level and its non-top-level descendants, with the stacking
%% order determining the order of siblings.  The "-takefocus" options
%% on windows determine whether or not they should be skipped.
%%
%% Arguments:
%% w -		Name of a window.


tk_focusPrev(W) ->
    fPrev(reverse(tk:childrenof(W)), W, W).

fPrev([], Parent, Init) ->
    case tk:toplevelof(Parent) of
	Parent ->
	    fPrev(reverse(tk:childrenof(Parent)), Parent, Init);
	_ ->
	    Parent1 = tk:parentof(Parent),
	    fPrev(membertl(reverse(tk:childrenof(Parent1)), Parent),
		  Parent1, Init)
    end;
fPrev([Init|_], _, Init) ->
    Init;
fPrev([W|Sibs], Parent, Init) ->
    case tk:toplevelof(W) of
	W -> 
	    fPrev(Sibs, Parent, Init);
	_ ->
	    case tkFocusOK(W) of
		true -> 
		    W;
		false ->
		    fPrev(reverse(tk:childrenof(W)), W, Init)
	    end
    end.

%% tkFocusOK --
%%
%% This procedure is invoked to decide whether or not to focus on
%% a given window.  It returns 1 if it's OK to focus on the window,
%% 0 if it's not OK.  The code first checks whether the window is
%% viewable.  If not, then it never focuses on the window.  Then it
%% checks the -takefocus option for the window and uses it if it's
%% set.  If there's no -takefocus option, the procedure checks to
%% see if (a) the widget isn't disabled, and (b) it has some key
%% bindings.  If all of these are true, then 1 is returned.
%%
%% Arguments:
%% w -		Name of a window.

tkFocusOK(W) ->
    case tk:winfo([viewable, W]) of
	0 -> false;
	_ ->
	    case catch tk:cget(W, takefocus) of
		1 -> true;
		0 -> false;
		_ ->
		    case catch tk:cget(W, state) of
			"disabled" -> false;
			_ ->
			    Class = tk:winfo([class, W]),
			    B0 = tk:bind(Class),
			    B1 = tk:bind(W),
			    case regexp:match(append(B0) ++ append(B1), 
						 "Key|Focus") of
				nomatch -> false;
				_ -> true
			    end
		    end
	    end
    end.

%% tk_focusFollowsMouse --
%%
%% If this procedure is invoked, Tk will enter "focus-follows-mouse"
%% mode, where the focus is always on whatever window contains the
%% mouse.  If this procedure isn't invoked, then the user typically
%% has to click on a window to give it the focus.
%%
%% Arguments:
%% None.

tk_focusFollowsMouse() ->
    tk:bind("all", "<Enter>", ['%W','%d'],
		  fun(W, D) ->
			  case D of
			      "NotifyAncestor" -> 
				  setFocus(W);
			      "NotifyNonlinear" ->
				  setFocus(W);
			      "NotifyInferior" ->
				  setFocus(W);
			      _ -> 
				  false
			  end
		  end).


setFocus(W) ->
    case tkFocusOK(W) of
	true -> tk:focus([W]);
	_ -> false
    end.
