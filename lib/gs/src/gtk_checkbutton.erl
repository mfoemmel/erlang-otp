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
%% Basic CheckButton Type
%% ------------------------------------------------------------

-module(gtk_checkbutton).

%%------------------------------------------------------------------------------
%% 			    CHECKBUTTON OPTIONS
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
%%	select			Bool
%%	selectbg		Color
%%	underline		Int
%%	width			Int
%%	wraplength		Int
%%	x			Int
%%	y			Int
%%
%%  Commands:
%%	enable			Bool
%%	flash
%%	invoke			
%%	setfocus		Bool
%%	toggle		
%%
%%  Events:
%%	buttonpress		[Bool | {Bool, Data}]
%%	buttonrelease		[Bool | {Bool, Data}]
%%	click			[Bool | {Bool, Data}]
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

%%-----------------------------------------------------------------------------
%%			MANDATORY INTERFACE FUNCTIONS
%%-----------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: create/7
%% Purpose    	: Create a widget of the type defined in this module.
%% Return 	: [Gsid_of_new_widget | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(DB, GtkId, Opts) ->
    TkW = gtk_generic:mk_tkw_child(DB,GtkId),
    {G, GID, NOpts} = fix_group(Opts, DB, GtkId#gtkid.owner),
    NGtkId=GtkId#gtkid{widget=TkW,widget_data={G, GID}},
    PlacePreCmd = [";place ", TkW],
    case gtk_generic:make_command(Opts,NGtkId,TkW,"",PlacePreCmd,DB) of
	{error,Reason} -> {error,Reason};
	Cmd when list(Cmd) ->
	    gtk:exec(["checkbutton ", TkW," -bo 2 -indi true ",Cmd]),
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
    {NOpts, NGtkid} = fix_group(Opts, DB, Gtkid#gtkid.owner, Gtkid),
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
    {_, Gid} = Gtkid#gtkid.widget_data,
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
%% Return 	: true
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
event(DB, Gtkid, Etype, Edata, Args) ->
    Arg2 = case Etype of
	       click ->
		   [Text, Bool | Rest] = Args,
		   RBool = case Bool of
			       1      -> true;
			       Other2 -> false
			   end,
		   {G, Gid} = Gtkid#gtkid.widget_data,
		   [Text, G, RBool | Rest];
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
	{wraplength,    Int} -> {s, [" -wr ", gtk:to_ascii(Int)]};

	flash                -> {c, [TkW, " f;"]};
	invoke               -> {c, [TkW, " i;"]};
	toggle               -> {c, [TkW, " to;"]};
	{select,       true} -> {c, [TkW, " se;"]};
	{select,      false} -> {c, [TkW, " de;"]};
	{click,          On} -> cbind(DB, Gtkid, click, On);
	_                    -> invalid_option
    end.

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
read_option(Option,Gtkid, TkW,DB,_) -> 
    case Option of
	disabledfg    -> tcl2erl:ret_color([TkW," cg -disabledforegr"]);
	group         -> {G, _} = Gtkid#gtkid.widget_data, G;
	selectbg      -> tcl2erl:ret_color([TkW," cg -selectc"]);
	groupid       -> {_, Gid} = Gtkid#gtkid.widget_data, Gid;
	underline     -> tcl2erl:ret_int([TkW," cg -un"]);
	wraplength    -> tcl2erl:ret_int([TkW," cg -wr"]);
	select        -> tcl2erl:ret_bool(["set x [", TkW,
					   " cg -va];global $x;set $x"]);
	
	click         -> gtk_db:is_inserted(DB, Gtkid, click);
	_ -> {bad_result, {Gtkid#gtkid.objtype, invalid_option, Option}}
    end.

%%------------------------------------------------------------------------------
%%			       PRIMITIVES
%%------------------------------------------------------------------------------
%% check button version
%% create version
fix_group(Opts, DB, Owner) ->
    {G, GID, NOpts} = fg(Opts, erlNIL, erlNIL, []),
    NG = case G of
	     erlNIL -> 
		 Vref = gtk_db:counter(DB, variable),
		 list_to_atom(lists:flatten(["cb", gtk:to_ascii(Vref)]));
	     Other1 -> Other1
	 end,
    RGID = case GID of
	       erlNIL -> {cbgrp, NG, Owner};
	       Other2 -> Other2
	   end,
    RG = gtk_db:insert_bgrp(DB, RGID),
    {NG, RGID, [{group, RG} | NOpts]}.
    
%% config version
fix_group(Opts, DB, Owner, Gtkid) ->
    {RG, RGID} = Gtkid#gtkid.widget_data,
    {G, GID, NOpts} = fg(Opts, RG, RGID, []),
    case {G, GID} of
	{RG, RGID} ->
	    {NOpts, Gtkid};
	{NG, RGID} ->
	    NGID = {cbgrp, NG, Owner},
	    gtk_db:delete_bgrp(DB, RGID),
	    NRG = gtk_db:insert_bgrp(DB, NGID),
	    NGtkid = Gtkid#gtkid{widget_data={NG,NGID}},
	    gtk_db:insert_widget(DB, NGtkid),
	    {[{group, NRG} | NOpts], NGtkid};
	{_, NGID} when NGID =/= RGID ->
	    gtk_db:delete_bgrp(DB, RGID),
	    NRG = gtk_db:insert_bgrp(DB, NGID),
	    NGtkid = Gtkid#gtkid{widget_data={RG,NGID}},
	    gtk_db:insert_widget(DB, NGtkid),
	    {[{group, NRG} | NOpts], NGtkid}
    end.



fg([{group, G} | Opts], _, GID, Nopts) ->
    fg(Opts, G, GID, Nopts);

fg([{groupid, GID} | Opts], G, _, Nopts) ->
    fg(Opts, G, GID, Nopts);

fg([Opt | Opts], G, GID, Nopts) ->
    fg(Opts, G, GID, [Opt | Nopts]);

fg([], Group, GID, Opts) ->
    {Group, GID, Opts}.


%%
%% Config bind
%%
cbind(DB, Gtkid, Etype, On) ->
    TkW = Gtkid#gtkid.widget,
    Cmd = case On of
	      {true, Edata} ->
		  Eref = gtk_db:insert_event(DB, Gtkid, Etype, Edata),
		  [" -com {erlsend  ", Eref, " \\\"[", TkW,
		   " cg -text]\\\" \[expr \$[", TkW, " cg -va]\]}"];
	      true ->
		  Eref = gtk_db:insert_event(DB, Gtkid, Etype, ""),
		  [" -com {erlsend  ", Eref, " \\\"[", TkW,
		   " cg -text]\\\" \[expr \$[", TkW, " cg -va]\]}"];
	      Other ->
		  Eref = gtk_db:delete_event(DB, Gtkid, Etype),
		  " -com {}"
	  end,
    {s, Cmd}.

%% ----- Done -----

