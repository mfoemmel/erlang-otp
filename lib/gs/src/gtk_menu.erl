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
%%-----------------------------------------------------------------------------
%%                         BASIC MENU TYPE
%%------------------------------------------------------------------------------

-module(gtk_menu).

%%------------------------------------------------------------------------------
%% 			    MENU OPTIONS
%%
%%  Attribute:
%%	activebg		Color
%%	activebw		Int
%%	activefg		Color
%%	bg			Color
%%	bw			Int
%%	data			Data
%%	disabledfg		Color
%%	fg			Color
%%	relief			Relief	[flat|raised|sunken|ridge|groove]
%%	selectcolor		Color
%%
%%  Commands:
%%	setfocus		[Bool | {Bool, Data}]
%%
%%  Events:
%%	buttonpress		[Bool | {Bool, Data}]
%%	buttonrelease		[Bool | {Bool, Data}]
%%	configure		[Bool | {Bool, Data}]
%%	destroy			[Bool | {Bool, Data}]
%%	enter			[Bool | {Bool, Data}]
%%	focus			[Bool | {Bool, Data}]
%%	keypress		[Bool | {Bool, Data}]
%%	keyrelease		[Bool | {Bool, Data}]
%%	leave			[Bool | {Bool, Data}]
%%	motion			[Bool | {Bool, Data}]
%%
%%  Read Options:
%%	children
%%	id
%%	parent
%%	type
%%
%%  Not Implemented:
%%	post			{X,Y}
%%	unpost
%%	align			n,w,s,e,nw,se,ne,sw,center
%%	anchor			n,w,s,e,nw,se,ne,sw,center
%%	cursor			??????
%%	focus			?????? (-takefocus)
%%	height			Int
%%	justify			left|right|center	(multiline text only)
%%	width			Int
%%	x			Int	(valid only for popup menus)
%%	y			Int	(valid only for popup menus)
%%

-export([create/3, config/3, read/3, delete/2, event/5,option/5,read_option/5]).
-export([delete_menuitem/3, insert_menuitem/4, lookup_menuitem_pos/3,
	 mk_create_opts_for_child/4]).

-include("gtk.hrl").

%%------------------------------------------------------------------------------
%%			MANDATORY INTERFACE FUNCTIONS
%%------------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: create/3
%% Purpose    	: Create a widget of the type defined in this module.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(DB, GtkId, Opts) ->
    #gtkid{parent=Parent,owner=Owner,objtype=Objtype}=GtkId,
    Pgtkid = gtk_db:lookup_gtkid(DB, Parent, Owner),
    Oref = gtk_db:counter(DB, Objtype),
    PF = gtk_widgets:suffix(Objtype),
    case Pgtkid#gtkid.objtype of
	menuitem ->
	    PMenu = Pgtkid#gtkid.parent,
	    PMgtkid = gtk_db:lookup_gtkid(DB, PMenu, Owner),
	    PMW = PMgtkid#gtkid.widget,
	    Index = gtk_menu:lookup_menuitem_pos(DB, PMgtkid, Pgtkid#gtkid.id),
	    TkW = lists:concat([PMW, PF, Oref]),
	    Gtkid=GtkId#gtkid{widget=TkW, widget_data=[]},
	    MPreCmd = ["menu ", TkW, " -tearoff 0 -relief raised -bo 2"],
	    MPostCmd = [$;,PMW," entryco ",gtk:to_ascii(Index)," -menu ",TkW],
	    case gtk_generic:make_command(Opts, Gtkid, TkW, "", "", DB) of
		{error,Reason} -> {error,Reason};
		Cmd when list(Cmd) ->
		    gtk:exec([MPreCmd,Cmd,MPostCmd]),
		    Gtkid
	    end;
	OtherParent ->
	    true = lists:member(OtherParent,
				%% grid+canvas har skumma coord system
				[menubutton,window,frame]),
	    PW = Pgtkid#gtkid.widget,
	    TkW = lists:concat([PW, PF, Oref]),
	    Gtkid=GtkId#gtkid{widget=TkW, widget_data=[]},
	    MPreCmd = ["menu ", TkW, " -tearoff 0 -relief raised -bo 2 "],
	    MPostCmd = if OtherParent == menubutton ->
			       [$;, PW, " conf -menu ", TkW];
			  true -> []
		       end,
	    case gtk_generic:make_command(Opts, Gtkid, TkW, "","", DB) of
		{error,Reason} -> {error,Reason};
		Cmd when list(Cmd) ->
		    gtk:exec([MPreCmd,Cmd,MPostCmd]),
		    Gtkid
	    end
    end.

mk_create_opts_for_child(DB,Cgtkid, Pgtkid, Opts) ->
    gtk_generic:mk_create_opts_for_child(DB,Cgtkid,Pgtkid,Opts).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: config/3
%% Purpose    	: Configure a widget of the type defined in this module.
%% Args        	: DB	  - The Database
%%		  Gtkid   - The gtkid of the widget
%%		  Opts    - A list of options for configuring the widget
%%
%% Return 	: [true | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
config(DB, Gtkid, Opts) ->
    TkW = Gtkid#gtkid.widget,
    PreCmd = [TkW, " conf"],
    gtk_generic:mk_cmd_and_exec(Opts, Gtkid, TkW, PreCmd, "", DB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: read/3
%% Purpose    	: Read one option from a widget
%% Args        	: DB	  - The Database
%%		  Gtkid   - The gtkid of the widget
%%		  Opt     - An option to read
%%
%% Return 	: [OptionValue | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read(DB, Gtkid, Opt) ->
    gtk_generic:read_option(DB, Gtkid, Opt).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: delete/2
%% Purpose    	: Delete widget from databas and return tkwidget to destroy
%% Args        	: DB	  - The Database
%%		  Gtkid   - The gtkid of the widget
%%
%% Return 	: TkWidget to destroy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete(DB, Gtkid) ->
    gtk_db:delete_widget(DB, Gtkid),
    Gtkid#gtkid.widget.

event(DB, Gtkid, Etype, Edata, Args) ->
    gtk_generic:event(DB, Gtkid, Etype, Edata, Args).

%%------------------------------------------------------------------------------
%%			MANDATORY FUNCTIONS
%%------------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: option/4
%% Purpose    	: Take care of options
%% Args        	: Option  - An option tuple
%%		  Gtkid   - The gtkid of the widget
%%		  TkW     - The  tk-widget
%%		  DB	  - The Database
%%
%% Return 	: A tuple {OptionType, OptionCmd}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
option(Option, Gtkid, TkW, DB,_) ->
    case Option of
	{activebw,      Int} -> {s, [" -activebo ", gtk:to_ascii(Int)]};
	{disabledfg,  Color} -> {s, [" -disabledf ", gtk:to_color(Color)]};
	{selectcolor, Color} -> {s, [" -selectc ", gtk:to_color(Color)]};
	{post_at,     {X,Y}} -> post_at(X,Y,Gtkid,TkW,DB);
	_                    -> invalid_option
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: read_option/5
%% Purpose    	: Take care of a read option
%% Args        	: DB	  - The Database
%%		  Gtkid   - The gtkid of the widget
%%		  Option  - An option
%%
%% Return 	: The value of the option or invalid_option
%%		  [OptionValue | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_option(Option, Gtkid, TkW, DB, AItem) ->
    case Option of
	activebw      -> tcl2erl:ret_int([TkW," cg -activebo"]);
	disabledfg    -> tcl2erl:ret_color([TkW," cg -disabledfo"]);
	selectcolor   -> tcl2erl:ret_color([TkW," cg -selectc"]);
	_ -> {error,{invalid_option,Option, Gtkid#gtkid.objtype}}
    end.

post_at(X,Y,Gtkid,TkW,DB) ->
    Pgtkid = gtk_db:lookup_gtkid(DB, Gtkid#gtkid.parent),
    PtkW = Pgtkid#gtkid.widget,
    RootX = tcl2erl:ret_int(["winfo rootx ",PtkW]),
    RootY = tcl2erl:ret_int(["winfo rooty ",PtkW]),
    {c,[" tk_popup ",TkW," ",gtk:to_ascii(RootX+X)," ",gtk:to_ascii(RootY+Y)]}.


%%-----------------------------------------------------------------------------
%%			       PRIMITIVES
%%-----------------------------------------------------------------------------
%%----------------------------------------------------------------------
%% gtk_db functions for menuitem handling
%% Tk menuitems are numbered from 0, thus we have to recalc the position.
%%----------------------------------------------------------------------
insert_menuitem(DB, MenuId, ItemId, Pos) ->
    Mgtkid = gtk_db:lookup_gtkid(DB, MenuId),
    Items = Mgtkid#gtkid.widget_data,
    NewItems = insert_at(ItemId, Pos+1, Items),
    gtk_db:update_widget(DB, Mgtkid#gtkid{widget_data=NewItems}).


delete_menuitem(DB, MenuId, ItemId) ->
    Mgtkid = gtk_db:lookup_gtkid(DB, MenuId),
    Items = Mgtkid#gtkid.widget_data,
    NewItems = lists:delete(ItemId, Items),
    gtk_db:insert_widget(DB, Mgtkid#gtkid{widget_data=NewItems}).


lookup_menuitem_pos(DB, Mgtkid, ItemId) ->
    Items = Mgtkid#gtkid.widget_data,
    
    find_pos(ItemId, Items) - 1.

%%----------------------------------------------------------------------
%% Generic list processing
%%----------------------------------------------------------------------
find_pos(ItemId, Items) ->
    find_pos(ItemId, Items, 1).

find_pos(ItemId, [], N) -> gs:error("Couldn't find item in menu~n", []);
find_pos(ItemId, [ItemId|Items], N) -> N;
find_pos(ItemId, [_|Items], N) ->
    find_pos(ItemId, Items, N + 1).

insert_at(Elem, 1, L) -> [Elem | L];
insert_at(Elem, N, [H|T]) ->
    [H|insert_at(Elem, N-1, T)].

%% ----- Done -----
