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
%% dialog.tcl --
%%
%% This file defines the procedure tk_dialog, which creates a dialog
%% box containing a bitmap, a message, and one or more buttons.
%%
%% @(%%) dialog.tcl 1.16 95/07/28 09:35:57
%%
%% Copyright (c) 1992-1993 The Regents of the University of California.
%% Copyright (c) 1994-1995 Sun Microsystems, Inc.
%%
%% See the file "license.terms" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%
-module(tkdialog).

-export([tk_dialog/6]).

%%
%% tk_dialog:
%%
%% This procedure displays a dialog box, waits for a button in the dialog
%% to be invoked, then returns the index of the selected button.
%%
%% Arguments:
%% w -		Window to use for dialog top-level.
%% title -	Title to display in dialog's decorative frame.
%% text -	Message to display in dialog.
%% bitmap -	Bitmap to display in dialog (empty string means none).
%% default -	Index of button that is to display the default ring
%%		(-1 means none).
%% args -	One or more strings to display in buttons across the
%%		bottom of the dialog box.

tk_dialog(W, Title, Text, Bitmap, Default, Args) ->
    %% 1. Create the top-level window and divide it into top
    %% and bottom parts.

    catch tk:destroy(W),
    tk:toplevel(W, [{class, "Dialog"}]),
    tk:wm([title, W, Title]),
    tk:wm([iconname, W, "Dialog"]),
    tk:wm([protocol, W, "WM_DELETE_WINDOW", ""]),
%%    tk:wm(["transient", W, tk:toplevelof(tk:parentof(W))]),
    tk:frame(W ++ ".bot", [{relief, raised}, {bd, 1}]),
    tk:pack(W ++ ".bot", [{side, bottom},{fill, both}]),
    tk:frame(W ++ ".top", [{relief, raised},{bd, 1}]),
    tk:pack(W ++ ".top", [{side, top}, {fill, both},{expand, 1}]),


    %% 2. Fill the top part with bitmap and message.
    tk:option(["add", "*Dialog.msg.wrapLength", "3i", "widgetDefault"]),
    tk:label(W ++ ".msg", 
	     [{justify,left},
	      {text, Text},
	      {font,"-Adobe-Times-Medium-R-Normal--*-180-*-*-*-*-*-*"}]),
    tk:pack(W ++ ".msg",
	    [{in, W ++ ".top"}, {side, right},
	     {expand, 1}, {fill, both},
	     {padx,"3m"}, {pady,"3m"}]),
    if
	Bitmap == "" ->
	    false;
	true ->
	    tk:label(W ++ ".bitmap", [{bitmap, Bitmap}]),
	    tk:pack(W ++ ".bitmap", [{in, W ++ ".top"},
				     {side,left},
				     {padx,"3m"},
				     {pady,"3m"}])
    end,

    %% 3. Create a row of buttons at the bottom of the dialog.

    dialog_buttons(W, Args, Default, 0, self()),

    %% 4. Withdraw the window, then update all the geometry information
    %% so we know how big it wants to be, then center the window in the
    %% display and de-iconify it.

    tk:wm([withdraw, W]),
    tk:update(idletasks),

    X = trunc(tk:winfo([screenwidth, W]) / 2 -
	      tk:winfo([reqwidth, W]) / 2 -
	      tk:winfo([vrootx, tk:parentof(W)])),
    
    Y = trunc(tk:winfo([screenheight, W]) / 2 -
	      tk:winfo([reqheight, W]) / 2 -
	      tk:winfo([vrooty, tk:parentof(W)])),
    tk:wm([geom, W, lists:concat(["+",X,"+",Y])]),

    tk:wm([deiconify, W]),

    %% 5. Set a grab and claim the focus too.

    OldFocus = tk:focus([]),
    OldGrab = tk:grab([current, W]),
    GrabStatus = if OldGrab == "" -> ""; 
		     true -> tk:grab([status, OldGrab])
		 end,
    tk:grab([W]),
    if
	Default >= 0 ->
	    tk:focus([W ++ ".button" ++ integer_to_list(Default)]);
	true ->
	    tk:focus([W])
    end,

    %% 6. Wait for the user to respond, then restore the focus and
    %% return the index of the selected button.  Restore the focus
    %% before deleting the window, since otherwise the window manager
    %% may take the focus away so we can't redirect it.  Finally,
    %% restore any grab that was in effect.
	
    %% tkwait variable tkPriv(button)
    Button = receive
		 {button, I} ->  
		     I
	     after 10000 ->  
		     -1
	     end,
    catch tk:focus([OldFocus]),
    tk:destroy(W),
    if OldGrab =/= "" ->
	    if GrabStatus == "global" ->
		    tk:grab([{global, OldGrab}]);
		true ->
		    tk:grab([OldGrab])
	    end;
	true ->
	    true
    end,
    Button.


dialog_buttons(W, [But | Bs], Default, I, Pid) ->
    WBut = W ++ ".button" ++ integer_to_list(I),
    tk:button(WBut, [{text,But},
		     {command, fun() -> Pid ! {button, I} end}]),
    if 
	I == Default ->
	    WDef = W ++ ".default",
	    tk:frame(WDef, [{relief,"sunken"},{bd, 1}]),
	    tk:raise(WBut, WDef),
	    tk:pack(WDef,[{in, W++".bot"}, {side, left},
			  {expand, 1}, {padx, "3m"}, {pady, "2m"}]),
	    tk:pack(WBut,[{in,WDef},{padx,"2m"},{pady,"2m"}]),
	    tk:bind(W, "<Return>",
		    fun(E) ->
			    tk:cmd(WBut, [flash]),
			    Pid ! {button, I}
		    end);
	true ->
	    tk:pack(WBut, [{in, W++".bot"},{side,left},{expand, 1},
			   {padx,"3m"},{pady,"2m"}])
    end,
    dialog_buttons(W, Bs, Default, I+1, Pid);
dialog_buttons(W, [], _, _, _) ->
    true.
