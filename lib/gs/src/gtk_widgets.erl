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
%% ------------------------------------------------------------
%% Widget specific data
%% ------------------------------------------------------------
%%

-module(gtk_widgets).

-include("gtk.hrl").
%% ----- Exports -----
-compile(export_all).


%%
%% Map primitive types to modules or false (false should not be a module!)
%%
%% ordered for efficiency

type2mod(window)      -> gtk_window;
type2mod(frame)       -> gtk_frame;
type2mod(button)      -> gtk_button;
type2mod(canvas)      -> gtk_canvas;
type2mod(checkbutton) -> gtk_checkbutton;
type2mod(rectangle)   -> gtk_rectangle;
type2mod(gs)          -> gtk_gs;
type2mod(grid)        -> gtk_grid;
type2mod(gridline)    -> gtk_gridline;
type2mod(text)        -> gtk_text;
type2mod(image)       -> gtk_image;
type2mod(label)       -> gtk_label;
type2mod(line)        -> gtk_line;
type2mod(entry)       -> gtk_entry;
type2mod(listbox)     -> gtk_listbox;
type2mod(editor)      -> gtk_editor;
type2mod(menu)        -> gtk_menu;
type2mod(menubar)     -> gtk_menubar;
type2mod(menubutton)  -> gtk_menubutton;
type2mod(menuitem)    -> gtk_menuitem;
type2mod(message)     -> gtk_message;
type2mod(oval)        -> gtk_oval;
type2mod(polygon)     -> gtk_polygon;
type2mod(prompter)    -> gtk_prompter;
type2mod(radiobutton) -> gtk_radiobutton;
type2mod(scale)       -> gtk_scale;
type2mod(scrollbar)   -> gtk_scrollbar;
type2mod(arc)         -> gtk_arc;
type2mod(Type)        -> {error,{unknown_type, Type}}.

objmod(#gtkid{objtype=OT}) -> type2mod(OT).

%%
%% The suffix to add to the parent tk widget
%%
suffix(button)       -> ".b";
suffix(canvas)       -> ".c";
suffix(checkbutton)  -> ".cb";
suffix(editor)       -> ".ed";
suffix(entry)        -> ".e";
suffix(frame)        -> ".f";
suffix(label)        -> ".l";
suffix(listbox)      -> ".lb";
suffix(menu)         -> ".m";
suffix(menubar)      -> ".bar";
suffix(menubutton)   -> ".mb";
suffix(message)      -> ".ms";
suffix(prompter)     -> ".p";
suffix(radiobutton)  -> ".rb";
suffix(scale)        -> ".sc";
suffix(window)       -> ".w";
suffix(Objtype) -> apply(type2mod(Objtype), suffix, []).


