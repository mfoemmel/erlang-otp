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
%% Basic Text Type
%% ------------------------------------------------------------

-module(gtk_text).

%%-----------------------------------------------------------------------------
%% 			    TEXT OPTIONS
%%
%%  Attributes:
%%	anchor			n|w|e|s|nw|sw|ne|se|center
%%	coords			[{X,Y}]
%%	data			Data
%%	fg			Color
%%	font			Font
%%	justify			left | center | right
%%	stipple			Bool
%%	text			String
%%	width			Int	(line length in characters)
%%
%%  Commands:
%%	lower
%%	move			{Dx, Dy}
%%	raise
%%	scale			{Xo, Yo, Sx, Sy}
%%	setfocus		Bool
%%
%%  Events:
%%	buttonpress		[Bool | {Bool, Data}]
%%	buttonrelease		[Bool | {Bool, Data}]
%%	enter			[Bool | {Bool, Data}]
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
%%	fontfamily		?????? Family
%%	fontsize		?????? Size
%%	style			?????? [bold,italic]
%%

-export([create/3, config/3, read/3, delete/2, destroy/3, event/5,
	 option/5,read_option/5]).

-include("gtk.hrl").

%%-----------------------------------------------------------------------------
%%			MANDATORY INTERFACE FUNCTIONS
%%----------------------------------------------------------------------------
create(DB, Gtkid, Opts) ->
    case gtk_canvas:pickout_coords(Opts, [],text,1) of
	{error, Error} ->
	    {bad_result, Error};
	{Coords, NewOpts} ->
	    Ngtkid=gtk_canvas:upd_gtkid(DB, Gtkid, Opts),
	    #gtkid{id=Id,widget=CanvasTkW}=Ngtkid,
	    MCmd = [CanvasTkW, " create te ", Coords],
	    gtk_canvas:mk_cmd_and_call(NewOpts,Ngtkid,CanvasTkW, MCmd, DB)
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: config/3
%% Purpose    	: Configure a widget of the type defined in this module.
%% Return 	: [true | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
config(DB, Gtkid, Opts) ->
    gtk_canvas:item_config(DB, Gtkid, Opts).


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
    Item = Gtkid#gtkid.widget_data,
    gtk_generic:read_option(DB,Gtkid,Opt,[gtk:to_ascii(Item)]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: delete/2
%% Purpose    	: Delete widget from databas and return tkwidget to destroy
%% Args        	: DB	  - The Database
%%		  Gtkid   - The gtkid of the widget
%%
%% Return 	: TkWidget to destroy | {Parent, Objmod, Args}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete(DB, Gtkid) ->
    gtk_canvas:item_delete_impl(DB,Gtkid).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: destroy/3
%% Purpose    	: Destroy a widget
%% Args        	: DB	  - The Database
%%		  Canvas  - The canvas tk widget
%%		  Item    - The item number to destroy
%%
%% Return 	: [true | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
destroy(DB, Canvas, Item) ->
    gtk:exec([Canvas, " delete ", gtk:to_ascii(Item)]).


event(DB, Gtkid, Etype, Edata, Args) ->
    gtk_generic:event(DB, Gtkid, Etype, Edata, Args).


%%-----------------------------------------------------------------------------
%%			MANDATORY FUNCTIONS
%%-----------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: option/5
%% Purpose    	: Take care of options
%% Args        	: Option  - An option tuple
%%		  MainW   - The main tk-widget
%%
%% Return 	: A tuple {OptionType, OptionCmd}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
option(Option, Gtkid, Canvas, DB, AItem) ->
    case Option of
	{anchor,         How} -> {s, [" -anchor ", gtk:to_ascii(How)]};
	{fg,          Color} -> {s, [" -fi ", gtk:to_color(Color)]};
	{font, Font} when tuple(Font) ->
	    gtk_db:insert_opt(DB,Gtkid,Option),
	    {s, [" -fo ", gtk_font:choose_ascii(DB,Font)]};
	{justify,       How} -> {s, [" -j ", gtk:to_ascii(How)]};
	{text,         Text} -> {s, [" -te ", gtk:to_ascii(Text)]};
	{width,       Width} -> {s, [" -w ", gtk:to_ascii(Width)]};
	_ -> invalid_option
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: read_option/5
%% Purpose    	: Take care of a read option
%% Args        	: DB	  - The Database
%%		  Gtkid   - The gtkid of the widget
%%		  Option  - An option
%%
%% Return 	: The value of the option or invalid_option
%%		  [OptionValue | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_option(Option, Gtkid, Canvas, DB, AItem) ->
    case Option of
	anchor   -> tcl2erl:ret_atom([Canvas, " itemcg ", AItem, " -anchor"]);
	fg       -> tcl2erl:ret_color([Canvas, " itemcg ", AItem, " -fi"]);
	font     -> gtk_db:opt(DB,Gtkid,font,undefined);
	justify  -> tcl2erl:ret_atom([Canvas, " itemcg ", AItem, " -j"]);
	stipple  -> tcl2erl:ret_stipple([Canvas," itemcg ",AItem," -stipple"]);
	text     -> tcl2erl:ret_str([Canvas, " itemcg ", AItem, " -te"]);
	width    -> tcl2erl:ret_int([Canvas, " itemcg ", AItem, " -w"]);
	_ -> {bad_result, {Gtkid#gtkid.objtype, invalid_option, Option}}
    end.


%% ----- Done -----
