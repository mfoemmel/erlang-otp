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
%% Basic Image Type
%% ------------------------------------------------------------

-module(gtk_image).

%%-----------------------------------------------------------------------------
%% 			    BITMAP OPTIONS
%%
%%  Attributes:
%%	anchor			n|w|e|s|nw|sw|ne|se|center
%%	bg			Color
%%	bitmap			String
%%	coords			[{X,Y}]
%%	data			Data
%%	fg			Color
%%
%% Attributes for gifs only:
%%      pix_val                 {{X,Y},Color}|{{{X1,Y1},{X2,Y2}},Color]
%%      save                    String
%%      refresh
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
%%      pix_val                 {X,Y}
%%	children
%%	id
%%	parent
%%	type
%%
%%  Not Implemented:
%%

-export([create/3, config/3, read/3, delete/2, destroy/3, event/5,
	 option/5,read_option/5]).

-include("gtk.hrl").

%%------------------------------------------------------------------------------
%%			MANDATORY INTERFACE FUNCTIONS
%%------------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: create/7
%% Purpose    	: Create a widget of the type defined in this module.
%% Args        	: DB	  - The Database
%%		  Objmod  - An atom, this module
%%		  Objtype - An atom, the logical widget type
%%		  Owner   - Pid of the creator
%%		  Name    - An atom naming the widget
%%		  Parent  - Gsid of the parent
%%		  Opts    - A list of options for configuring the widget
%%
%% Return 	: [Gsid_of_new_widget | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(DB, Gtkid, Opts) ->
    case pickout_type(Opts) of
	bitmap ->
	    create(bitmap,DB, Gtkid, Opts);
	_gif ->  %%Default gif
	    create(gif,DB, Gtkid, Opts)
    end.

create(gif,DB, Gtkid, Opts) ->
    case pickout_coords(Opts, []) of
	{error, Error} ->
	    {bad_result, Error};
	{Coords, NewOpts} ->
	    CCmd = "image create photo",
	    case tcl2erl:ret_atom(CCmd) of
		Photo_item when atom(Photo_item) ->
		    #gtkid{parent=Parent,owner=Owner,id=Id}=Gtkid,
		    Pgtkid = gtk_db:lookup_gtkid(DB, Parent, Owner),
		    SO = Pgtkid#gtkid.widget_data,
		    CanvasTkW = SO#so.object,
		    Photo_item_s = atom_to_list(Photo_item),
		    gtk_db:insert_opt(DB,Id,gs:pair(coords,Opts)),
		    Ngtkid=Gtkid#gtkid{widget=CanvasTkW,
				       widget_data={Photo_item_s,unknown}},
		    gtk_db:update_widget(DB,Ngtkid),
		    MCmd = [CanvasTkW," create image ",Coords," -image ", 
			   Photo_item_s," -anchor nw"],
		    case gtk_canvas:make_command(NewOpts, Ngtkid,
						 CanvasTkW, MCmd, DB) of
			{error,Reason} -> {error,Reason};
			Cmd when list(Cmd) ->
			    case tcl2erl:ret_int(Cmd) of
				Item when integer(Item) ->
				    %% buu, not nice
				    G2 = gtk_db:lookup_gtkid(DB,Id),
				    NewWidget = {Photo_item_s,Item},
				    NewGtkid = G2#gtkid{widget_data=NewWidget},
				    gtk_db:insert_widget(DB, NewGtkid),
				    NewGtkid;
				Bad_result ->
				    {error,Bad_result}
			    end
		    end;
		Bad_result ->
		    {error,Bad_result}
	    end
    end;

create(bitmap,DB, Gtkid, Opts) ->
    case pickout_coords(Opts, []) of
	{error, Error} ->
	    {bad_result, Error};
	{Coords, NewOpts} ->
	    #gtkid{parent=Parent,owner=Owner,id=Id}=Gtkid,
	    Pgtkid = gtk_db:lookup_gtkid(DB, Parent, Owner),
	    SO = Pgtkid#gtkid.widget_data,
	    CanvasTkW = SO#so.object,
	    gtk_db:insert_opt(DB,Id,gs:pair(coords,Opts)),
	    Ngtkid=Gtkid#gtkid{widget=CanvasTkW, widget_data=no_item},
	    gtk_db:update_widget(DB,Ngtkid),
	    MCmd = [CanvasTkW," create bi ", Coords],
	    gtk_canvas:mk_cmd_and_call(NewOpts,Ngtkid, CanvasTkW, MCmd,DB)
    end.


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
    {Canvas, Item} = get_widget(Gtkid),
    AItem = gtk:to_ascii(Item),
    SCmd = [Canvas, " itemconf ", AItem],
    gtk_canvas:mk_cmd_and_exec(Opts, Gtkid, Canvas, AItem, SCmd, DB).

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
    {_, Item} = get_widget(Gtkid),
    gtk_generic:read_option(DB,Gtkid,Opt,[gtk:to_ascii(Item)]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: delete/2
%% Purpose    	: Delete widget from databas and return tkwidget to destroy
%% Args        	: DB	  - The Database
%%		  Gtkid   - The gtkid of the widget
%%
%% Return 	: TkWidget to destroy | {Parent, Objmod, Args}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
delete(DB, Gtkid) ->
    gtk_db:delete_widget(DB, Gtkid),
    #gtkid{parent=P,id=ID}=Gtkid,
    {Canvas, Item} = get_widget(Gtkid),
    {P, ID, gtk_image, [Canvas, Item]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: destroy/3
%% Purpose    	: Destroy a widget
%% Args        	: DB	  - The Database
%%		  Canvas  - The canvas tk widget
%%		  Item    - The item number to destroy
%%
%% Return 	: [true | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
destroy(DB, Canvas, Item) ->
    gtk:exec([Canvas, " delete ", gtk:to_ascii(Item)]).


event(DB, Gtkid, Etype, Edata, Args) ->
    gtk_generic:event(DB, Gtkid, Etype, Edata, Args).

%%------------------------------------------------------------------------------
%%			MANDATORY FUNCTIONS
%%------------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: option/5
%% Purpose    	: Take care of options
%% Args        	: Option  - An option tuple
%%		  Gtkid   - The gtkid of the widget
%%		  MainW   - The main tk-widget
%%		  Canvas  - The canvas tk-widget
%%		  DB	  - The Database
%%
%% Return 	: A tuple {OptionType, OptionCmd}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
option(Option, Gtkid, Canvas, DB, AItem) ->
    case Option of
	{bitmap,     Bitmap} ->
	    {ok, BF,_} = regexp:gsub(Bitmap, [92,92], "/"),
	    {s, [" -bi @", BF]};
	{load_gif,       File} -> 
	    {ok, F2,_} = regexp:gsub(File, [92,92], "/"),
	    {Photo_item, _item} = Gtkid#gtkid.widget_data,
	    {c,[Photo_item, " configure -file ", gtk:to_ascii(F2)]};
	{pix_val,  {Coords,Color}} ->
	    {Photo_item, _item} = Gtkid#gtkid.widget_data,
	    {c, [Photo_item, " put ", gtk:to_color(Color), " -to ", 
		 coords(Coords)]};
	{save_gif, Name} ->
	    {Photo_item, _item} = Gtkid#gtkid.widget_data,
	    {c, [Photo_item, " write ", gtk:to_ascii(Name)]};
	{fg,          Color} -> {s, [" -fo ", gtk:to_color(Color)]};
	{bg,          Color} -> {s, [" -ba ", gtk:to_color(Color)]};
	{anchor,         How} -> {s, [" -anchor ", gtk:to_ascii(How)]};
	_ -> invalid_option
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: read_option/5
%% Return 	: The value of the option or invalid_option
%%		  [OptionValue | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_option(Option, Gtkid, Canvas, DB, AItem) ->
    case Option of
	anchor   -> tcl2erl:ret_atom([Canvas," itemcget ",AItem," -anchor"]);
	bg       -> tcl2erl:ret_color([Canvas, " itemcget ", AItem, " -ba"]);
	bitmap   -> tcl2erl:ret_file([Canvas, " itemcget ", AItem, " -bi"]);
	fg       -> tcl2erl:ret_color([Canvas, " itemcget ", AItem, " -fo"]);
	{pix_val,{X,Y}} ->
	    {Photo_item, _item} = Gtkid#gtkid.widget_data,
	    ret_photo_color([Photo_item," get ",coords({X,Y})]);
	_ -> {bad_result, {Gtkid#gtkid.objtype, invalid_option, Option}}
    end.

ret_photo_color(Cmd) ->
    case gtk:call(Cmd) of
	{result,Str} ->
	    {ok, [R,G,B],[]} = io_lib:fread("~d ~d ~d", Str),
	    {R,G,B};
	Bad_result -> Bad_result
    end.


%%------------------------------------------------------------------------------
%%			       PRIMITIVES
%%------------------------------------------------------------------------------
get_widget(#gtkid{widget=Canvas,widget_data={_Photo_item,Item}}) ->
    {Canvas,Item};
get_widget(#gtkid{widget=Canvas,widget_data=Item}) ->
    {Canvas,Item}.

pickout_coords([{coords,Coords} | Rest], Opts) when length(Coords) == 1 ->
    case coords(Coords) of
	invalid ->
	    {error, "An image must have two coordinates"};
	RealCoords ->
	    {RealCoords, lists:append(Rest, Opts)}
    end;
pickout_coords([Opt | Rest], Opts) ->
    pickout_coords(Rest, [Opt|Opts]);
pickout_coords([], Opts) ->
    {error, "An image must have two coordinates"}.

coords({X,Y}) when number(X),number(Y) ->
    [gtk:to_ascii(X), " ", gtk:to_ascii(Y), " "];
coords([{X,Y} | R]) when number(X),number(Y) ->
    [gtk:to_ascii(X), " ", gtk:to_ascii(Y), " ", coords(R)];
coords({{X1,Y1},{X2,Y2}}) when number(X1),number(Y1),number(X2),number(Y2) ->
    [gtk:to_ascii(X1), " ", gtk:to_ascii(Y1)," ",
     gtk:to_ascii(X2), " ", gtk:to_ascii(Y2)];
coords([_]) -> %% not a pair
    invalid;
coords([]) ->
    [].


pickout_type([{bitmap,Str}|Options]) ->
    bitmap;
pickout_type([{gif,Str}|Options])  ->
    gif;
pickout_type([]) ->
    none;
pickout_type([_|Tail]) ->
    pickout_type(Tail).

%% ----- Done -----

