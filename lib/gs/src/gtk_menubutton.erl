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
%% Basic Menubutton Type
%% ------------------------------------------------------------

-module(gtk_menubutton).

%%------------------------------------------------------------------------------
%% 			    MENUBUTTON OPTIONS
%%
%%  Attributes:
%%	activebg		Color
%%	activefg		Color
%%	align			n,w,s,e,nw,se,ne,sw,center
%%	anchor			n,w,s,e,nw,se,ne,sw,center
%%	bg			Color
%%	bw			Int
%%	data			Data
%%	disabledfg		Color
%%	fg			Color
%%      font                    Font
%%	height			Int
%%	highlightbg		Color
%%	highlightbw		Int
%%	highlightfg		Color
%%	justify			left|right|center	(multiline text only)
%%	label			{text, String} | {image, BitmapFile}
%%	padx			Int   (Pixels)
%%	pady			Int   (Pixels)
%%	relief			Relief	[flat|raised| sunken | ridge | groove]
%%	side			left | right	(valid only in menubars)
%%	underline		Int
%%	width			Int
%%	wraplength		Int
%%	x			Int	(not valid in menubars)
%%	y			Int	(not valid in menubars)
%%
%%  Commands:
%%	enable			Bool
%%	setfocus		Bool
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
%%	activate		?????? (kontra enable, true)
%%	state			??????
%%	cursor			??????
%%	image			??????
%%	focus			?????? (-takefocus)
%%

-export([create/3,config/3,read/3,delete/2,event/5,option/5,read_option/5,
	 mk_create_opts_for_child/4]).

-include("gtk.hrl").

%%-----------------------------------------------------------------------------
%%			MANDATORY INTERFACE FUNCTIONS
%%-----------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: create/7
%% Purpose    	: Create a widget of the type defined in this module.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(DB, GtkId, Opts) ->
    TkW = gtk_generic:mk_tkw_child(DB,GtkId),
    NGtkId=GtkId#gtkid{widget=TkW},
    PlacePreCmd = [";place ", TkW],
    case gtk_generic:make_command(Opts, NGtkId, TkW, "", PlacePreCmd, DB) of
	{error,Reason} -> {error,Reason};
	Cmd when list(Cmd) ->
	    gtk:exec(["menubutton ", TkW," -padx 4 -pady 3",Cmd]),
	    NGtkId
    end.

mk_create_opts_for_child(DB,Cgtkid, Pgtkid, Opts) ->
    gtk_generic:mk_create_opts_for_child(DB,Cgtkid,Pgtkid,Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: config/3
%% Purpose    	: Configure a widget of the type defined in this module.
%% Args        	: DB	  - The Database
%%		  Gtkid   - The gtkid of the widget
%%		  Opts    - A list of options for configuring the widget
%%
%% Return 	: [true | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
config(DB, Gtkid, Opts) ->
    TkW = Gtkid#gtkid.widget,
    SimplePreCmd = [TkW, " conf"],
    PlacePreCmd = [";place ", TkW],
    gtk_generic:mk_cmd_and_exec(Opts,Gtkid,TkW,SimplePreCmd,PlacePreCmd,DB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: read/3
%% Purpose    	: Read one option from a widget
%% Args        	: DB	  - The Database
%%		  Gtkid   - The gtkid of the widget
%%		  Opt     - An option to read
%%
%% Return 	: [OptionValue | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read(DB, Gtkid, Opt) ->
    gtk_generic:read_option(DB, Gtkid, Opt).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: delete/2
%% Purpose    	: Delete widget from databas and return tkwidget to destroy
%% Args        	: DB	  - The Database
%%		  Gtkid   - The gtkid of the widget
%%
%% Return 	: TkWidget to destroy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete(DB, Gtkid) ->
    gtk_db:delete_widget(DB, Gtkid),
    Gtkid#gtkid.widget.

event(DB, Gtkid, Etype, Edata, Args) ->
    gtk_generic:event(DB, Gtkid, Etype, Edata, Args).

%%-----------------------------------------------------------------------------
%%			MANDATORY FUNCTIONS
%%-----------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: option/4
%% Purpose    	: Take care of options
%% Args        	: Option  - An option tuple
%%		  Gtkid   - The gtkid of the widget
%%		  TkW     - The  tk-widget
%%		  DB	  - The Database
%%
%% Return 	: A tuple {OptionType, OptionCmd}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
option(Option, Gtkid, TkW, DB,_) ->
    case Option of
	{anchor,        How} -> fix_anchor(How, Gtkid, TkW, DB);
	{disabledfg,  Color} -> {s, [" -disabledf ", gtk:to_color(Color)]};
	{height,     Height} -> {s, [" -he ", gtk:to_ascii(Height)]};
	{side,         Side} -> fix_side(Side, Gtkid, TkW, DB);
	{underline,     Int} -> {s, [" -und ", gtk:to_ascii(Int)]};
	{width,       Width} -> {s, [" -wi ", gtk:to_ascii(Width)]};
	{wraplength,    Int} -> {s, [" -wr ", gtk:to_ascii(Int)]};
	{x,               X} -> fix_placement(x, X, Gtkid, TkW, DB);
	{y,               Y} -> fix_placement(y, Y, Gtkid, TkW, DB);
	_                    -> invalid_option
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: read_option/3
%% Purpose    	: Take care of a read option
%% Args        	: DB	  - The Database
%%		  Gtkid   - The gtkid of the widget
%%		  Option  - An option
%%
%% Return 	: The value of the option or invalid_option
%%		  [OptionValue | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_option(Option,GtkId,TkW,DB,_) ->
    case Option of
	anchor        -> tcl2erl:ret_place(anchor, TkW);
	disabledfg    -> tcl2erl:ret_color([TkW," cg -disabledfo"]);
	height        -> tcl2erl:ret_int([TkW," cg -he"]);
	side          -> tcl2erl:ret_pack(side, TkW);
	underline     -> tcl2erl:ret_int([TkW," cg -underl"]);
	width         -> tcl2erl:ret_int([TkW," cg -wi"]);
	wraplength    -> tcl2erl:ret_int([TkW," cg -wr"]);
	x             -> tcl2erl:ret_place(x, TkW);
	y             -> tcl2erl:ret_place(y, TkW);
	_ -> {error,{invalid_option,Option, GtkId#gtkid.objtype}}
    end.

%%-----------------------------------------------------------------------------
%%			       PRIMITIVES
%%-----------------------------------------------------------------------------

fix_placement(Attr, Value, Gtkid, TkW, DB) ->
    Pgtkid = gtk_db:lookup_gtkid(DB, Gtkid#gtkid.parent),
    case Pgtkid#gtkid.objtype of
	menubar -> invalid_option;
	_       -> {p, [" -", atom_to_list(Attr), " ", gtk:to_ascii(Value)]}
    end.

	
fix_anchor(How, Gtkid, TkW, DB) ->
    Pgtkid = gtk_db:lookup_gtkid(DB, Gtkid#gtkid.parent),
    case {Pgtkid#gtkid.objtype, How} of
	menubar -> {c, ["pack ", TkW, " -an ", gtk:to_ascii(How)]};
	_       -> {p,   [" -anch ", atom_to_list(How)]}
    end.


fix_side(Side, Gtkid, TkW, DB) ->
    Pgtkid = gtk_db:lookup_gtkid(DB, Gtkid#gtkid.parent),
    case Pgtkid#gtkid.objtype of
	menubar -> {c, ["pack ", TkW, " -fill y -si ", gtk:to_ascii(Side)]};
        _       -> none
    end.


%% ----- Done -----

