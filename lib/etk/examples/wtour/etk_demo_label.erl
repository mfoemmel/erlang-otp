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
-module(etk_demo_label).

-export([start/0]).


    %% set w .label
    %% catch {destroy $w}
    %% toplevel $w
    %% wm title $w "Label Demonstration"
    %% wm iconname $w "label"
    %% positionWindow $w

start() ->
    W = etk_demo_lib:start("Label Demonstration",
			   "label",
			   "Five labels are displayed below: three textual ones on the left, and a bitmap label and a text label on the right.  Labels are pretty boring because you can't do anything with them."),
    etk_demo_lib:dismiss_show_code(),
   
    %% frame $w.left
    %% frame $w.right
    %% pack $w.left $w.right -side left -expand yes -padx 10 -pady 10 -fill both

    W_left  = etk:frame(W, []),
    W_right = etk:frame(W, []),
    tk:pack([W_left, W_right, {side,left}, {expand,yes},{padx,10},
	    {pady,10},{fill,both}]),

    %% label $w.left.l1 -text "First label"
    %% label $w.left.l2 -text "Second label, raised" -relief raised
    %% label $w.left.l3 -text "Third label, sunken" -relief sunken
    %% pack $w.left.l1 $w.left.l2 $w.left.l3 -side top -expand yes -pady 2 -anchor w

    W_left_l1 = etk:label(W_left, [{text,"First label"}]),
    W_left_l2 = etk:label(W_left, [{text,"Second label, raised"},{relief,raised}]),
    W_left_l3 = etk:label(W_left, [{text, "Third label, sunken"},{relief, sunken}]),
    tk:pack([W_left_l1,W_left_l2,W_left_l3,{side,top},{expand,yes},
	     {pady,2},{anchor,w}]),

    %% label $w.right.bitmap -bitmap @$tk_library/demos/images/face -borderwidth 2 -relief sunken
    %% label $w.right.caption -text "Tcl/Tk Proprietor"
    %% pack $w.right.bitmap $w.right.caption -side top

    W_right_bitmap = etk:label(W_right, [{bitmap,"@face"},
					 {borderwidth,2},{relief,sunken}]),
    W_right_caption = etk:label(W_right, [{text,"Tcl/Tk Proprietor"}]),
    tk:pack([W_right_bitmap, W_right_caption, {side, top}]).
