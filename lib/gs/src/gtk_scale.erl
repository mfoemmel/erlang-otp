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
%% Basic Scale Type
%% ------------------------------------------------------------

-module(gtk_scale).

%%-------------------------------------------------------------------------
%% 			    SCALE OPTIONS
%%
%%  Attributes:
%%	activebg		Color
%%	anchor			n,w,s,e,nw,se,ne,sw,center
%%	bg			Color
%%	bw			Int
%%	data			Data
%%	fg			Color
%%	height			Int
%%	highlightbg		Color
%%	highlightbw		Int
%%	highlightfg		Color
%%	orient			vertical | horizontal
%%	range			{From, To}
%%	relief			Relief	[flat|raised|sunken|ridge|groove]
%%	text			String
%%	width			Int
%%	x			Int
%%	y			Int
%%
%%  Commands:
%%	enable			Bool
%%	pos			Int
%%	setfocus		Bool
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

-export([create/3,config/3,read/3,delete/2,event/5,
	 option/5,read_option/5]).

-include("gtk.hrl").

%%------------------------------------------------------------------------------
%%			MANDATORY INTERFACE FUNCTIONS
%%------------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: create/7
%% Purpose    	: Create a widget of the type defined in this module.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(DB, GtkId, Opts) ->
    TkW = gtk_generic:mk_tkw_child(DB,GtkId),
    PlacePreCmd = [";place ", TkW],
    Ngtkid = GtkId#gtkid{widget=TkW},
    case gtk_generic:make_command(Opts, Ngtkid, TkW,"", PlacePreCmd, DB) of
	{error,Reason} -> {error,Reason};
	Cmd when list(Cmd) ->
	    gtk:exec(["scale ", TkW,Cmd,$;,TkW,
		      " conf -bo 2 -sliderrelief raised -highlightth 2"]),
	    Ngtkid
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%%		  TkW     - The  tk-widget
%%
%% Return 	: A tuple {OptionType, OptionCmd}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
option(Option, Gtkid, TkW, DB,_) ->
    case Option of
	{activebg,    Color} -> {s, [" -activeb ", gtk:to_color(Color)]};
	{orient,        How} -> {s, [" -or ", gtk:to_ascii(How)]};
	{range,  {From, To}} -> {s, [" -fr ", gtk:to_ascii(From),
				     " -to ", gtk:to_ascii(To)]};
        {relief,     Relief} -> {s, [" -rel ", gtk:to_ascii(Relief)]};
        {bw,            Wth} -> {s, [" -bd ", gtk:to_ascii(Wth)]};
	{text,       String} -> {s, [" -la ",gtk:to_ascii(String)]};
	{pos,           Pos} -> {c, [TkW, " set ", gtk:to_ascii(Pos)]};
	{click,          On} -> cbind(DB, Gtkid, click, On);
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_option(Option,Gtkid,TkW,DB,_) -> 
    case Option of
	activebg      -> tcl2erl:ret_color([TkW," cg -activeb"]);
	orient        -> tcl2erl:ret_atom([TkW," cg -ori"]);
	range         ->
	    tcl2erl:ret_tuple(["list [",TkW," cg -fr] [",TkW," cg -to]"]);
        bw            -> tcl2erl:ret_int([TkW," cg -bd"]);
        relief        -> tcl2erl:ret_atom([TkW, " cg -reli"]);
	text          -> tcl2erl:ret_str([TkW," cg -lab"]);
	pos           -> tcl2erl:ret_int([TkW," get"]);
	click         -> gtk_db:is_inserted(DB, Gtkid, click);
	_ -> {bad_result, {Gtkid#gtkid.objtype, invalid_option, Option}}
    end.

%%-----------------------------------------------------------------------------
%%			       PRIMITIVES
%%-----------------------------------------------------------------------------


%%
%% Config bind
%%
cbind(DB, Gtkid, Etype, On) ->
    TkW = Gtkid#gtkid.widget,
    Cmd = case On of
	      {true, Edata} ->
		  Eref = gtk_db:insert_event(DB, Gtkid, Etype, Edata),
		  [" -com {erlsend ", Eref, "}"];
	      true ->
		  Eref = gtk_db:insert_event(DB, Gtkid, Etype, ""),
		  [" -com {erlsend ", Eref, "}"];
	      Other ->
		  Eref = gtk_db:delete_event(DB, Gtkid, Etype),
		  " -com {}"
	  end,
    {s, Cmd}.

%% ----- Done -----
