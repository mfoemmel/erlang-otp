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
%% -----------------------------------------------------------
%% Basic Listbox Type
%% ------------------------------------------------------------

-module(gtk_listbox).

%%-----------------------------------------------------------------------------
%% 			    LISTBOX OPTIONS
%%
%%  Attributes:
%%	activebg		Color
%%	anchor			n,w,s,e,nw,se,ne,sw,center
%%	bc			Color
%%	bg			Color
%%	bw			Wth
%%	data			Data
%%	fg			Color
%%	height			Int
%%	highlightbg		Color
%%	highlightbw		Wth
%%	highlightfg		Color
%%	hscroll			Bool | top | bottom
%%	items			[String, String, ... String]
%%	relief			Relief
%%	scrollbg		Color
%%	scrollfg		Color
%%	selectbg		Color
%%	selectbw		Width
%%	selectfg		Color
%%	selection		Index | clear
%%	selectmode		single|browse|multiple|extended
%%	vscroll			Bool | left | right
%%	width			Int
%%	x			Int
%%	xselection		Bool	(Good name?????)
%%	y			Int
%%
%%  Commands:
%%	add			{Index, String} | String
%%	change			{Index, String}
%%	clear
%%	del			Index | {FromIdx, ToIdx}
%%	get			Index
%%	see			Index
%%	selection			=> [Idx1,Idx2,Idx3...]
%%	setfocus		Bool
%%	size			Int
%%
%%  Events:
%%	buttonpress		[Bool | {Bool, Data}]
%%	buttonrelease		[Bool | {Bool, Data}]
%%	click			[Bool | {Bool, Data}]
%%	configure		[Bool | {Bool, Data}]
%%	destroy			[Bool | {Bool, Data}]
%%	doubleclick		[Bool | {Bool, Data}]
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

-export([create/3,config/3,read/3,delete/2,event/5,wid_event/5,option/5,
	read_option/5]).

-include("gtk.hrl").

%%-----------------------------------------------------------------------------
%%			MANDATORY INTERFACE FUNCTIONS
%%-----------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: create/7
%% Purpose    	: Create a widget of the type defined in this module.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(DB, GtkId, Opts) ->
    MainW = gtk_generic:mk_tkw_child(DB,GtkId),
    Listbox = lists:append(MainW,".z"),
    {Vscroll, Hscroll, NewOpts} = gtk_generic:parse_scrolls(Opts),
    WidgetD = #so{main=MainW, object=Listbox,
		  hscroll=Hscroll, vscroll=Vscroll},
    Gtkid=GtkId#gtkid{widget=MainW, widget_data=WidgetD},
    MandatoryCmd = ["so_create listbox ", MainW],
    case gtk:call(MandatoryCmd) of
	{result, _} ->
	    SimplePreCmd = [MainW, " conf"],
	    PlacePreCmd = [";place ", MainW],
	    case gtk_generic:make_command(NewOpts, Gtkid, MainW,SimplePreCmd,
					  PlacePreCmd, DB,Listbox) of
		{error,Reason} -> {error,Reason};
		Cmd when list(Cmd) ->
		    gtk:exec(Cmd),
		    gtk:exec([MainW,".sy conf -rel sunken -bo 2;",
			      MainW,".pad.sx conf -rel sunken -bo 2;",Listbox,
			 " conf -bo 2 -relief sunken -highlightth 2 -expo 0;"]),
		    Gtkid
	    end;
	Bad_Result ->
	    {error, Bad_Result}
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: config/3
%% Purpose    	: Configure a widget of the type defined in this module.
%% Args        	: DB	  - The Database
%%		  Gtkid   - The gtkid of the widget
%%		  Opts    - A list of options for configuring the widget
%%
%% Return 	: [true | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
config(DB, Gtkid, Options) ->
    SO = Gtkid#gtkid.widget_data,
    MainW = Gtkid#gtkid.widget,
    Listbox = SO#so.object,
    NewOpts = gtk_generic:parse_scrolls(Gtkid, Options),
    SimplePreCmd = [MainW, " conf"],
    PlacePreCmd = [";place ", MainW],
    gtk_generic:mk_cmd_and_exec(NewOpts, Gtkid, MainW,
				SimplePreCmd, PlacePreCmd, DB,Listbox).

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
    SO = Gtkid#gtkid.widget_data,
    gtk_generic:read_option(DB, Gtkid, Opt,SO#so.object).


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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: event/5
%% Purpose    	: Construct the event and send it to the owner of the widget
%% Args        	: DB	  - The Database
%%		  Gtkid   - The gtkid of the widget
%%		  Etype   - The event type
%%		  Edata   - The event data
%%		  Args    - The data from tcl/tk
%%		
%% Return 	: [true | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
event(DB, Gtkid, click, Edata, Args) ->
    wid_event(DB, Gtkid, click, Edata, Args);
event(DB, Gtkid, doubleclick, Edata, Args) ->
    wid_event(DB, Gtkid, doubleclick, Edata, Args);
event(DB, Gtkid, Etype, Edata, Args) ->
    gtk_generic:event(DB, Gtkid, Etype, Edata, Args).


%% widget specific events
wid_event(DB, Gtkid, Etype, Edata, Args) ->
    SO = Gtkid#gtkid.widget_data,
    TkW = SO#so.object,
    CurIdx = tcl2erl:ret_int([TkW," index active;"]),
    CurTxt = tcl2erl:ret_str([TkW," get active;"]),
    CurSel = tcl2erl:ret_list([TkW," curselection;"]),
    Arg2 = [CurIdx,CurTxt,lists:member(CurIdx,CurSel)],
    gtk_generic:event(DB, Gtkid, Etype, Edata, Arg2).


%%-----------------------------------------------------------------------------
%%			MANDATORY FUNCTIONS
%%-----------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: option/5
%% Purpose    	: Take care of options
%% Args        	: Option  - An option tuple
%%		  Gtkid   - The gtkid of the widget
%%		  MainW   - The main tk-widget
%%		  Listbox  - The listbox tk-widget
%%		  DB	  - The Database
%%
%% Return 	: A tuple {OptionType, OptionCmd}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
option(Option, Gtkid, MainW,DB, Listbox) ->
    case Option of
	{items,          Items} when list(Items) ->
	    {c, [Listbox," del 0 end ;", Listbox," ins 0 ",item_list(Items)]};
	{selection, {From, To}} when integer(From),integer(To) ->
	    {c,[Listbox," sel set ",gtk:to_ascii(From)," " ,gtk:to_ascii(To)]};
	{font,            Font} when tuple(Font) ->
	    gtk_db:insert_opt(DB,Gtkid,Option),
	    {c, [Listbox," conf -font ",gtk_font:choose_ascii(DB,Font)]};
	{selection,   clear} ->
	    {c, [Listbox," sel clear 0 end"]};
	{selection,     Idx} when integer(Idx) ->
	    {c, [Listbox, " select set ", gtk:to_ascii(Idx)]};
	{selectmode,   Mode} ->
	    {c, [Listbox, " conf -selectm ", gtk:to_ascii(Mode)]};
	{xselection,   Bool} ->
	    {c, [Listbox, " conf -exportse ", gtk:to_ascii(Bool)]};
	{fg,          Color} ->
	    {c, [Listbox, " conf -fg ", gtk:to_color(Color)]};
	
	{del,   {From, To}} ->
	    {c, [Listbox, " del ", integer_to_list(From), " ",
		 integer_to_list(To)]};
	{del,          Idx} ->
	    {c, [Listbox, " del ", integer_to_list(Idx)]};
	clear               -> {c, [Listbox," del 0 end"]};
	{add,   {Idx, Str}} ->
	    {c, [Listbox, " ins ", integer_to_list(Idx), " ",
		 gtk:to_ascii(Str)]};
	{add,          Str} ->
	    {c, [Listbox," ins end ",gtk:to_ascii(Str)]};
	{change, {Idx, Str}} ->
	    {c, [Listbox, " del ", integer_to_list(Idx), $;,
		 Listbox, " ins ", integer_to_list(Idx), " " ,
		 gtk:to_ascii(Str)]};
	{see,        Idx} ->   
	    {c, [Listbox," see ",gtk:to_ascii(Idx)]};
	
	{setfocus,    true} -> {c, ["focus ", MainW]};
	{setfocus,   false} -> {c, ["focus ."]};
	
	{click,         On} -> cbind(DB, Gtkid, Listbox, click, On);
	{doubleclick,   On} -> cbind(DB, Gtkid, Listbox, doubleclick, On);
	_ -> invalid_option
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
read_option(Option,GtkId,MainW,DB,Listbox) ->
    case Option of
	fg            -> tcl2erl:ret_color([Listbox," cg -fg"]);
	font          -> gtk_db:opt(DB,GtkId,font,undefined);
	selection     -> tcl2erl:ret_list([Listbox, " curselection"]);
	setfocus      -> tcl2erl:ret_focus(Listbox, "focus");

	items         -> tcl2erl:ret_str_list([Listbox, " get 0 end"]);
	selectmode    -> tcl2erl:ret_atom([Listbox, " cg -selectmode"]);
	size          -> tcl2erl:ret_int([Listbox, " size"]);
	xselection    -> tcl2erl:ret_bool([Listbox, " cg -exportsel"]);
	{get, Idx}    -> tcl2erl:ret_str([Listbox, " get ",gtk:to_ascii(Idx)]);
        click         -> gtk_db:is_inserted(DB, GtkId, click);
        doubleclick   -> gtk_db:is_inserted(DB, GtkId, doubleclick);
	
	_ -> {bad_result, {GtkId#gtkid.objtype, invalid_option, Option}}
    end.


%%-----------------------------------------------------------------------------
%%			       PRIMITIVES
%%-----------------------------------------------------------------------------

item_list([H|T]) ->
    [gtk:to_ascii(H),$ |item_list(T)];
item_list([]) ->
    [].

cbind(DB, Gtkid, Listbox, Etype, {true, Edata}) ->
    Button = case Etype of
                 click       -> " <ButtonRelease-1> ";
                 doubleclick -> " <Double-ButtonRelease-1> "
             end,
    Eref = gtk_db:insert_event(DB, Gtkid, Etype, Edata),
    {c, ["bind " ,Listbox, Button, "{erlsend ", Eref," }"]};
 
cbind(DB, Gtkid, Listbox, Etype, true) ->    
    cbind(DB, Gtkid, Listbox, Etype, {true, []});
 
cbind(DB, Gtkid, Listbox, Etype, On) ->    
    Button = case Etype of
                 click       -> " <Button-1> {}";
                 doubleclick -> " <Double-Button-1> {}"
             end,
    gtk_db:delete_event(DB, Gtkid, Etype),
    {c, ["bind ",Listbox, Button]}.


%%% ----- Done -----
