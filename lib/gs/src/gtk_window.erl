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
%% Basic Window Type.
%% ------------------------------------------------------------

-module(gtk_window).

%%------------------------------------------------------------------------------
%% 			    WINDOW OPTIONS
%%
%%  Attributes:
%%	x			Int
%%	y			Int
%%	width			Int
%%	height			Int
%%	bg			Color
%%	bw			Int
%%	relief			Relief  [flat|raised|sunken|ridge|groove]
%%	highlightbw		Int
%%	highlightbg		Color
%%	highlightfg		Color
%%	map			Bool
%%	iconify 		Bool
%%	title			String
%%	iconname		String	
%%      iconbitmap      	Bitmap
%%      iconmask        	Bitmap
%%	data			Data
%%      cursor                  arrow|busy|cross|hand|help|resize|text
%%
%%  Commands:
%%      raise			
%%      lower			
%%	setfocus		Bool
%%
%%  Events:
%%      configure		[Bool | {Bool, Data}]
%%	enter			[Bool | {Bool, Data}]
%%	leave			[Bool | {Bool, Data}]
%%	motion			[Bool | {Bool, Data}]
%%	keypress		[Bool | {Bool, Data}]
%%	keyrelease		[Bool | {Bool, Data}]
%%	buttonpress		[Bool | {Bool, Data}]
%%	buttonrelease		[Bool | {Bool, Data}]
%%	focus			[Bool | {Bool, Data}]
%%	destroy			[Bool | {Bool, Data}]
%%
%%  Read options:
%%	children
%%	id
%%	parent
%%	type
%%
%%  Not Implemented:
%%	screen			?????????
%%	map			
%%	unmap			
%%	iconify
%%	deiconify
%%	focusmodel		[active|passive] (wm focusmodel)
%%

-export([create/3, config/3, read/3, delete/2, event/5,destroy_win/1]).
-export([option/5,read_option/5,mk_create_opts_for_child/4]).

-include("gtk.hrl").
% bind . <1> {puts "x: [expr %X - [winfo rootx .]] y: [expr %Y - [wi rooty .]]"}

%%-----------------------------------------------------------------------------
%%			MANDATORY INTERFACE FUNCTIONS
%%-----------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: create/3
%% Purpose    	: Create a widget of the type defined in this module.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(DB, Gtkid, Opts) ->
    TkW = gtk_generic:mk_tkw_child(DB,Gtkid),
    NGtkid=Gtkid#gtkid{widget=TkW},
    case gtk_generic:make_command(transform_geometry_opts(Opts),
				  NGtkid, TkW, "", ";", DB) of
	{error,Reason} -> {error,Reason};
	Cmd when list(Cmd) ->
	    BindCmd = gtk_generic:bind(DB, Gtkid, TkW, configure, true),
	    gtk:exec(["toplevel ", TkW,Cmd,$;,BindCmd]),
	    NGtkid
    end.

mk_create_opts_for_child(DB,Cgtkid, Pgtkid, Opts) ->
    gtk_generic:mk_create_opts_for_child(DB,Cgtkid,Pgtkid,Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: config/3
%% Purpose    	: Configure a widget of the type defined in this module.
%% Args        	: DB	  - The Database
%%		  Gtkid   - The gtkid of the widget
%%		  Opts    - A list of options for configuring the widget
%%
%% Return 	: [true | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
config(DB, Gtkid, Opts) ->
    TkW = Gtkid#gtkid.widget,
    SimplePreCmd = [TkW, " conf"],
    gtk_generic:mk_cmd_and_exec(transform_geometry_opts(Opts),
				Gtkid,TkW,SimplePreCmd,"",DB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: read/3
%% Purpose    	: Read one option from a widget
%% Args        	: DB	  - The Database
%%		  Gtkid   - The gtkid of the widget
%%		  Opt     - An option to read
%%
%% Return 	: [OptionValue | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: event/5
%% Purpose    	: Construct the event and send it to the owner of the widget
%% Args        	: DB	  - The Database
%%		  Gtkid   - The gtkid of the widget
%%		  Etype   - The event type
%%		  Edata   - The event data
%%		  Args    - The data from tcl/tk
%%		
%% Return 	: [true | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
event(DB, Gtkid, configure, Edata, Args) ->
    [W,H|_] = Args,
    gtk_db:insert_opt(DB,Gtkid,{width,W}),
    gtk_db:insert_opt(DB,Gtkid,{height,H}),
    case gtk_db:opt(DB,Gtkid,configure) of
	true ->
	    apply(gtk_generic,event,[DB,Gtkid,configure,Edata,Args]);
	false ->
	    ok
    end;
event(DB, Gtkid, destroy, Edata, Args) ->
    spawn(gtk_window,destroy_win,[gtk:make_extern_id(Gtkid#gtkid.id,DB)]),
    gtk_generic:event(DB, Gtkid, destroy, Edata, Args);
event(DB, Gtkid, Etype, Edata, Args) ->
    gtk_generic:event(DB, Gtkid, Etype, Edata, Args).

destroy_win(ID) ->
    gs:destroy(ID).
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
-define(REGEXP,"regexp {([0-9]+)x([0-9]+)\\+(\\-*[0-9]+)\\+(\\-*[0-9]+)} ").

option(Option, Gtkid, TkW, DB,_) ->
    case Option of
%% Bug in tcl/tk complicates setting of a single x,y,width,height.
	{x,               X} -> 
	    {c, 
	    [?REGEXP,"[wm ge ",TkW, "] g w h x y;wm ge ", TkW,
	     " $w\\x$h\\+", gtk:to_ascii(X), "\\+$y;update idletasks"]};
	{y,               Y} -> 
	    {c,[?REGEXP,"[wm ge ",TkW, "] g w h x y;wm ge ", TkW,
		" $w\\x$h\\+$x\\+",gtk:to_ascii(Y),"; update idletasks"]};
	{width,       Width} ->
	    case gtk_db:opt_or_not(DB,Gtkid,width) of
		{value,Width} -> none;
		Q ->
		    gtk_db:insert_opt(DB,Gtkid,{width,Width}),
		    {c,[?REGEXP,"[wm ge ",TkW, "] g w h x y;wm ge ", TkW,
			" ", gtk:to_ascii(Width),
			"\\x$h\\+$x\\+$y;update idletasks"]}
	    end;
    	{height,     Height} -> 
	    case gtk_db:opt_or_not(DB,Gtkid,height) of
		{value,Height} -> none;
		Q ->
		    gtk_db:insert_opt(DB,Gtkid,{height,Height}),
		    {c,
		     ["wm ge ",TkW," [winfo w ", TkW, "]x",gtk:to_ascii(Height),
		      ";update idletasks"]}
	    end;
	{width_height, {W,H}} ->
	    case {gtk_db:opt_or_not(DB,Gtkid,width),
		  gtk_db:opt_or_not(DB,Gtkid,height)} of
		{{value,W},{value,H}} ->
		    none;
		OtherSize -> 
		    gtk_db:insert_opt(DB,Gtkid,{height,H}),
		    gtk_db:insert_opt(DB,Gtkid,{width,W}),
		    {c, ["update idletasks;wm ge ", TkW, " ",
			 gtk:to_ascii(W),"x",gtk:to_ascii(H),
			 ";update idletasks"]}
	    end;
	{xy,             {X,Y}} -> 
	    {c, [?REGEXP,"[wm ge ",TkW, "] g w h x y;wm ge ", TkW,
		 " $w\\x$h\\+", signed(X),"\\+", signed(Y),
		 ";update idletasks"]};
	{bg,          Color} -> {s, [" -bg ", gtk:to_color(Color)]};
	{map,          true} -> {c, ["wm deiconify ", TkW]};
	{map,         false} -> {c, ["wm withdraw ", TkW]};
	{configure,      On} ->
	    gtk_db:insert_opt(DB,Gtkid,{configure,On}),
	    none;
	{iconify,      true} -> {c, ["wm iconify ", TkW]};
	{iconify,     false} -> {c, ["wm deiconify ", TkW]};
	{title,       Title} -> {c, ["wm title ", TkW, " " , 
					   gtk:to_ascii(Title)]};
	{iconname,     Name} -> {c, ["wm iconn ",TkW, " ",
					   gtk:to_ascii(Name)]};
	{iconbitmap, Bitmap} -> {c, ["wm iconb ",TkW, " ",
					   gtk:to_ascii(Bitmap)]};
	{iconmask,   Bitmap} -> {c, ["wm iconm ",TkW, " ",
					   gtk:to_ascii(Bitmap)]};
	raise		     -> {c, ["raise ", TkW]};
	lower		     -> {c, ["lower ", TkW]};
	{setfocus,     true} -> {c, ["focus ", TkW]};
	{setfocus,    false} -> {c, ["focus {}"]};
	{buttonpress,    On} ->
	    Eref = mk_eref(On, DB, Gtkid, buttonpress),
	    {c,["bind ",TkW," <ButtonPress> ",
	       event_onoff(["{erlsend ",Eref," %b ",xy_abs_str(TkW),"};"],On)]};
	{buttonrelease,  On} ->
	    Eref = mk_eref(On, DB, Gtkid, buttonrelease),
	    {c,["bind ",TkW," <ButtonRelease> ",
	       event_onoff(["{erlsend ",Eref," %b ",xy_abs_str(TkW),"};"],On)]};
	{motion,         On} ->
	    Eref = mk_eref(On, DB, Gtkid, motion),
	    {c,["bind ",TkW," <Motion> ",
	       event_onoff(["{erlsend ",Eref," ",xy_abs_str(TkW),"};"],On)]};
	_                    -> invalid_option
    end.

xy_abs_str(TkW) ->
    ["[expr %X-[winfo rootx ",TkW,"]] [expr %Y-[winfo rooty ",TkW,"]]"].

event_onoff(Str, true) -> Str;
event_onoff(_,false) -> "{}".

mk_eref(false, DB, Gtkid, Etype) ->
    gtk_db:delete_event(DB, Gtkid, Etype),
    dummy;
mk_eref(true,DB,Gtkid,Etype) ->
    Eref = gtk_db:insert_event(DB, Gtkid, Etype, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: read_option/3
%% Purpose    	: Take care of a read option
%% Args        	: DB	  - The Database
%%		  Gtkid   - The gtkid of the widget
%%		  Option  - An option
%%
%% Return 	: The value of the option or invalid_option
%%		  [OptionValue | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_option(Option, Gtkid, TkW, DB,_) ->
    case Option of
	x             -> tcl2erl:ret_x(geo_str(TkW));
	y             -> tcl2erl:ret_y(geo_str(TkW));
	width         -> tcl2erl:ret_width(geo_str(TkW));
	height        -> tcl2erl:ret_height(geo_str(TkW));
	configure     -> gtk_db:opt(DB,Gtkid,configure);
	bg            -> tcl2erl:ret_color([TkW," cg -bg"]);
	map           -> tcl2erl:ret_mapped(["winfo is ", TkW]);
	iconify       -> tcl2erl:ret_iconified(["wm st ", TkW]);
	title         -> tcl2erl:ret_str(["wm ti ", TkW]);
	iconname      -> tcl2erl:ret_str(["wm iconn ", TkW]);
	iconbitmap    -> tcl2erl:ret_str(["wm iconb ", TkW]);
	iconmask      -> tcl2erl:ret_str(["wm iconm ", TkW]);
	setfocus      -> tcl2erl:ret_focus(TkW, "focus");
	_ -> {bad_result, {Gtkid#gtkid.objtype, invalid_option, Option}}
    end.

geo_str(TkW) ->
    ["update idletasks;",?REGEXP,"[wm geometry ", TkW,
     "] g w h x y;set tmp \"$w $h $x $y\""].



%%------------------------------------------------------------------------------
%%			       PRIMITIVES
%%------------------------------------------------------------------------------

%% Return {+,-}Int  to be used in a geometry option
signed(X) when X>=0 ->
    [$+,integer_to_list(X)];
signed(X) when X<0 ->
    integer_to_list(X).

%%----------------------------------------------------------------------
%% Purpose: tcl/tk: wm .window geo sets WxH+x+y at one time.
%%          flushing every time is expensive. Do (almost) as much as
%%          possible in one operation.
%%----------------------------------------------------------------------
transform_geometry_opts(Opts) ->
    {Geo,RestOpts} = collect_geo_opts(Opts,[],[]),
    Geo2 = make_atomic(lists:sort(Geo)),
    lists:append(Geo2,RestOpts).

make_atomic([{height,H},{width,W},{x,X},{y,Y}]) ->
    [{width_height,{W,H}},{xy,{X,Y}}];
make_atomic([{height,H},{width,W}|XY]) ->
    [{width_height,{W,H}}|XY];
make_atomic([WH,{x,X},{y,Y}]) ->
    [WH,{xy,{X,Y}}];
make_atomic(L) -> L.

%%----------------------------------------------------------------------
%% Returns: {(list of x,y,width,height options),list of other opts}
%%----------------------------------------------------------------------
collect_geo_opts([{x,X}|Opts],Geo,Rest) ->
    collect_geo_opts(Opts,[{x,X}|Geo],Rest);
collect_geo_opts([{y,Y}|Opts],Geo,Rest) ->
    collect_geo_opts(Opts,[{y,Y}|Geo],Rest);
collect_geo_opts([{height,H}|Opts],Geo,Rest) ->
    collect_geo_opts(Opts,[{height,H}|Geo],Rest);
collect_geo_opts([{width,W}|Opts],Geo,Rest) ->
    collect_geo_opts(Opts,[{width,W}|Geo],Rest);
collect_geo_opts([Opt|Opts],Geo,Rest) ->
    collect_geo_opts(Opts,Geo,[Opt|Rest]);
collect_geo_opts([],Geo,Rest) -> {Geo,Rest}.
    
%%% ----- Done -----




