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
%% Basic Radiobutton Type
%% ------------------------------------------------------------

-module(gtk_radiobutton).

%%------------------------------------------------------------------------------
%% 			    RADIOBUTTON OPTIONS
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
%%	enable			Bool
%%	fg			Color
%%	group			Atom
%%	groupid			Groupid
%%	height			Int
%%	highlightbg		Color
%%	highlightbw		Int
%%	highlightfg		Color
%%	justify			left|right|center
%%	label			{text, String} | {image, BitmapFile}
%%	padx			Int   (Pixels)
%%	pady			Int   (Pixels)
%%	relief			Relief	[flat|raised|sunken|ridge|groove]
%%	selectbg		Color
%%	underline		Int
%%	value			Atom
%%	width			Int
%%	wraplength		Int
%%	x			Int
%%	y			Int
%%
%%  Commands:
%%	flash
%%	invoke			
%%	select			Bool
%%	setfocus		Bool
%%
%%  Events:
%%	buttonpress		[Bool | {Bool, Data}]
%%	buttonrelease		[Bool | {Bool, Data}]
%%	click			[Bool | {Bool, Data}]
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
%%	cursor			??????
%%	focus			?????? (-takefocus)
%%	font			??????
%%

-export([create/3,config/3,read/3,delete/2,event/5,option/5,read_option/5]).

-include("gtk.hrl").

%%------------------------------------------------------------------------------
%%			MANDATORY INTERFACE FUNCTIONS
%%------------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: create/3
%% Purpose    	: Create a widget of the type defined in this module.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(DB, GtkId, Opts) ->
    TkW = gtk_generic:mk_tkw_child(DB,GtkId),
    {G, GID, V, NOpts} = fix_group_and_value(Opts, DB, GtkId#gtkid.owner),
    NGtkId=GtkId#gtkid{widget=TkW,widget_data={G, GID, V}},
    PlacePreCmd = [";place ", TkW],
    case gtk_generic:make_command(NOpts, NGtkId, TkW, "", PlacePreCmd, DB) of
	{error,Reason} -> {error,Reason};
	Cmd when list(Cmd) ->
	    gtk:exec(["radiobutton ", TkW," -bo 2 -indi true ",Cmd]),
	    NGtkId
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
    TkW = Gtkid#gtkid.widget,
    {NOpts, NGtkid} = fix_group_and_value(Opts, DB, Gtkid#gtkid.owner, Gtkid),
    SimplePreCmd = [TkW, " conf"],
    PlacePreCmd = [";place ", TkW],
    gtk_generic:mk_cmd_and_exec(NOpts,NGtkid,TkW,SimplePreCmd,PlacePreCmd,DB).

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
    {_, Gid, _} = Gtkid#gtkid.widget_data,
    gtk_db:delete_bgrp(DB, Gid),
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
event(DB, Gtkid, Etype, Edata, Args) ->
    Arg2 = case Etype of
	      click ->
		  [Text, Grp | Rest] = Args,
		  {G, Gid, V} = Gtkid#gtkid.widget_data,
		  [Text, G, V | Rest];
	      Other3 ->
		  Args
	  end,
    gtk_generic:event(DB, Gtkid, Etype, Edata, Arg2).



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
	{disabledfg,  Color} -> {s, [" -disabledforegr ", gtk:to_color(Color)]};
	{group,       Group} -> {s, [" -var ", gtk:to_ascii(Group)]};
	{selectbg,    Color} -> {s, [" -selectc ", gtk:to_color(Color)]};
	{underline,     Int} -> {s, [" -un ", gtk:to_ascii(Int)]};
	{value,           V} -> {s, [" -val ", gtk:to_ascii(V)]};
	{wraplength,    Int} -> {s, [" -wr ", gtk:to_ascii(Int)]};
	flash                -> {c, [TkW, " f;"]};
	invoke               -> {c, [TkW, " i;"]};
	{select,       true} -> {c, [TkW, " se;"]};
	{select,      false} -> {c, [TkW, " des;"]};
	{click,          On} -> cbind(DB, Gtkid, click, On);
	_                    -> invalid_option
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: read_option/4
%% Purpose    	: Take care of a read option
%% Args        	: DB	  - The Database
%%		  Gtkid   - The gtkid of the widget
%%		  Option  - An option
%%
%% Return 	: The value of the option or invalid_option
%%		  [OptionValue | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_option(Option,Gtkid, TkW,DB,_) -> 
    case Option of
	disabledfg    -> tcl2erl:ret_color([TkW," cg -disabledforegr"]);
	group         -> {G, _, _} = Gtkid#gtkid.widget_data, G;
	groupid       -> {_, Gid, _} = Gtkid#gtkid.widget_data, Gid;
	selectbg      -> tcl2erl:ret_color([TkW," cg -selectc"]);
	underline     -> tcl2erl:ret_int([TkW," cg -un"]);
	value         -> {_, _, V} = Gtkid#gtkid.widget_data, V;
	wraplength    -> tcl2erl:ret_int([TkW," cg -wr"]);

	select        ->
	    Cmd = ["list [set x [",TkW," cg -var];global $x;set $x] [",
		   TkW," cg -val]"],
	    case tcl2erl:ret_tuple(Cmd) of
		{X, X} -> true;
		Other  -> false
	    end;

	click         -> gtk_db:is_inserted(DB, Gtkid, click);
	_ -> {bad_result, {Gtkid#gtkid.objtype, invalid_option, Option}}
    end.

%%------------------------------------------------------------------------------
%%			       PRIMITIVES
%%------------------------------------------------------------------------------

%% create version
fix_group_and_value(Opts, DB, Owner) ->
    {G, GID, V, NOpts} = fgav(Opts, erlNIL, erlNIL, erlNIL, []),
    RV = case V of
	     erlNIL -> list_to_atom(lists:concat([v,gtk_db:counter(DB,value)]));
	     Other0 -> Other0
	 end,
    NG = case G of
	       erlNIL -> rb;
	       Other1 -> Other1
	   end,
    RGID = case GID of
	       erlNIL -> {rbgrp, NG, Owner};
	       Other2 -> Other2
	   end,
    RG = gtk_db:insert_bgrp(DB, RGID),
    {NG, RGID, RV, [{group, RG}, {value, RV} | NOpts]}.
    
%% config version
fix_group_and_value(Opts, DB, Owner, Gtkid) ->
    {RG, RGID, RV} = Gtkid#gtkid.widget_data,
    {G, GID, V, NOpts} = fgav(Opts, RG, RGID, RV, []),
    case {G, GID, V} of
	{RG, RGID, RV} ->
	    {NOpts, Gtkid};
	{NG, RGID, RV} ->
	    NGID = {rbgrp, NG, Owner},
	    gtk_db:delete_bgrp(DB, RGID),
	    NRG = gtk_db:insert_bgrp(DB, NGID),
	    NGtkid = Gtkid#gtkid{widget_data={NG,NGID,RV}},
	    gtk_db:insert_widget(DB, NGtkid),
	    {[{group, NRG} | NOpts], NGtkid};
	{RG, RGID, NRV} ->
	    NGtkid = Gtkid#gtkid{widget_data={RG,RGID,NRV}},
	    gtk_db:insert_widget(DB, NGtkid),
	    {[{value,NRV} | NOpts], NGtkid};
	{_, NGID, RV} when NGID =/= RGID ->
	    gtk_db:delete_bgrp(DB, RGID),
	    NRG = gtk_db:insert_bgrp(DB, NGID),
	    NGtkid = Gtkid#gtkid{widget_data={RG,NGID,RV}},
	    gtk_db:insert_widget(DB, NGtkid),
	    {[{group, NRG} | NOpts], NGtkid};
	{_, NGID, NRV} when NGID =/= RGID ->
	    gtk_db:delete_bgrp(DB, RGID),
	    NRG = gtk_db:insert_bgrp(DB, NGID),
	    NGtkid = Gtkid#gtkid{widget_data={RG,NGID,NRV}},
	    gtk_db:insert_widget(DB, NGtkid),
	    {[{group, NRG}, {value,NRV} | NOpts], NGtkid};
	{NG, RGID, NRV} ->
	    NGID = {rbgrp, NG, Owner},
	    gtk_db:delete_bgrp(DB, RGID),
	    NRG = gtk_db:insert_bgrp(DB, NGID),
	    NGtkid = Gtkid#gtkid{widget_data={NG,NGID,NRV}},
	    gtk_db:insert_widget(DB, NGtkid),
	    {[{group, NRG}, {value,NRV} | NOpts], NGtkid}
    end.



fgav([{group, G} | Opts], _, GID, V, Nopts) ->
    fgav(Opts, G, GID, V, Nopts);

fgav([{groupid, GID} | Opts], G, _, V, Nopts) ->
    fgav(Opts, G, GID, V, Nopts);

fgav([{value, V} | Opts], G, GID, _, Nopts) ->
    fgav(Opts, G, GID, V, Nopts);

fgav([Opt | Opts], G, GID, V, Nopts) ->
    fgav(Opts, G, GID, V, [Opt | Nopts]);

fgav([], Group, GID, Value, Opts) ->
    {Group, GID, Value, Opts}.

%%
%% Config bind
%%
cbind(DB, Gtkid, Etype, On) ->
    TkW = Gtkid#gtkid.widget,
    Cmd = case On of
	      {true, Edata} ->
		  Eref = gtk_db:insert_event(DB, Gtkid, Etype, Edata),
		  [" -com {erlsend ", Eref,
		   " \\\"[", TkW, " cg -text]\\\" [", TkW, " cg -var]}"];
	      true ->
		  Eref = gtk_db:insert_event(DB, Gtkid, Etype, ""),
		  [" -com {erlsend ", Eref,
		   " \\\"[", TkW, " cg -text]\\\" [", TkW, " cg -var]}"];
	      Other ->
		  Eref = gtk_db:delete_event(DB, Gtkid, Etype),
		  " -com {}"
	  end,
    {s, Cmd}.

%% ----- Done -----

