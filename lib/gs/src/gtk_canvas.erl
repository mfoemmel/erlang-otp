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
%% Basic Canvas Type
%% ------------------------------------------------------------

-module(gtk_canvas).

%%-----------------------------------------------------------------------------
%% 			    CANVAS OPTIONS
%%
%%  Attributes:
%%	activebg		Color
%%	anchor			n,w,s,e,nw,se,ne,sw,center
%%	bc			Color
%%	bg			Color
%%	bw			Wth
%%	data			Data
%%	height			Int
%%	highlightbg		Color
%%	highlightbw		Wth
%%	highlightfg		Color
%%	hscroll			Bool | top | bottom
%%	relief			Relief
%%	scrollbg		Color
%%	scrollfg		Color
%%	scrollregion		{X1, Y1, X2, Y2}
%%	selectbg		Color
%%	selectbw		Width
%%	selectfg		Color
%%	vscroll			Bool | left | right
%%	width			Int
%%	x			Int
%%	y			Int
%%
%%
%%  Commands:
%%	find			{X, Y}	 =>    Item at pos X,Y or false
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
%%	fg			Color
%%

-export([create/3,config/3,read/3,delete/2,event/5,option/5,read_option/5]).
-export([make_command/5,make_command/6,pickout_coords/4, coords/1,
	item_config/3,mk_create_opts_for_child/4,
	upd_gtkid/3,item_delete_impl/2,mk_cmd_and_exec/6,mk_cmd_and_call/5]).

-include("gtk.hrl").

%%-----------------------------------------------------------------------------
%%			MANDATORY INTERFACE FUNCTIONS
%%-----------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: create/3
%% Return 	: [Gsid_of_new_widget | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(DB, Gtkid, Opts) ->
    MainW = gtk_generic:mk_tkw_child(DB,Gtkid),
    Canvas = lists:append(MainW,".z"),
    {Vscroll, Hscroll, NewOpts} = gtk_generic:parse_scrolls(Opts),
    WidgetD = #so{main=MainW, object=Canvas,
		  hscroll=Hscroll, vscroll=Vscroll},
    NGtkid=Gtkid#gtkid{widget=MainW, widget_data=WidgetD},
    MandatoryCmd = ["so_create canvas ", MainW],
    case gtk:call(MandatoryCmd) of
	{result, _} ->
	    SimplePreCmd = [MainW, " conf"],
	    PlacePreCmd = [";place ", MainW],
	    gtk_db:insert_opt(DB,Gtkid,gs:pair(scrollregion,Opts)),
	    case gtk_generic:make_command(NewOpts, NGtkid, MainW,
			       SimplePreCmd, PlacePreCmd, DB,Canvas) of
		{error,Reason} -> {error,Reason};
		Cmd when list(Cmd) ->
		    gtk:exec(Cmd),
		    gtk:exec([MainW,".sy conf -rel sunken -bo 2;",
			      MainW,".pad.sx conf -rel sunken -bo 2;"]),
		    NGtkid
	    end;
	Bad_Result ->
	    {bad_result, Bad_Result}
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
config(DB, Gtkid, Options) ->
    SO = Gtkid#gtkid.widget_data,
    MainW = Gtkid#gtkid.widget,
    Canvas = SO#so.object,
    NewOpts = gtk_generic:parse_scrolls(Gtkid, Options),
    SimplePreCmd = [MainW, " conf"],
    PlacePreCmd = [";place ", MainW],
    gtk_generic:mk_cmd_and_exec(NewOpts, Gtkid, MainW,
				SimplePreCmd, PlacePreCmd, DB,Canvas).

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

event(DB, Gtkid, Etype, Edata, Args) ->
    gtk_generic:event(DB, Gtkid, Etype, Edata, Args).


%%-----------------------------------------------------------------------------
%%			MANDATORY FUNCTIONS
%%-----------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: option/5
%% Purpose    	: Take care of options
%% Args        	: Option  - An option tuple
%%		  Gtkid   - The gtkid of the widget
%%		  MainW   - The main tk-widget
%%		  Canvas  - The canvas tk-widget
%%		  DB	  - The Database
%%
%% Return 	: A tuple {OptionType, OptionCmd}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
option(Option, Gtkid, MainW,DB,Canvas) ->
    case Option of
	{scrollregion, {X1, Y1, X2, Y2}} ->
	    gtk_db:insert_opt(DB,Gtkid,Option),
    	    {c, [Canvas, " conf -scrollr {",
		 gtk:to_ascii(X1), " ", gtk:to_ascii(Y1), " ",
		 gtk:to_ascii(X2), " ", gtk:to_ascii(Y2),"}"]};
	{yscrollpos, Y} ->
	    {_,Ymin,_,Ymax} = gtk_db:opt(DB,Gtkid,scrollregion),
	    K = 1/(Ymax-Ymin),
	    M = -K*Ymin,
	    PercentOffViewTop = K*Y+M,
	    {c, [Canvas," yvi mo ",gtk:to_ascii(PercentOffViewTop)]};
	{xscrollpos, X} ->
	    {Xmin,_,Xmax,_} = gtk_db:opt(DB,Gtkid,scrollregion),
	    K = 1/(Xmax-Xmin),
	    M = -K*Xmin,
	    PercentOffViewLeft = K*X+M,
	    {c, [Canvas," xvi mo ",gtk:to_ascii(PercentOffViewLeft)]};
	{buttonpress,    On} -> bind(DB, Gtkid, Canvas, buttonpress, On);
	{buttonrelease,  On} -> bind(DB, Gtkid, Canvas, buttonrelease, On);
	{configure,      On} -> bind(DB, Gtkid, Canvas, configure, On);
	{destroy,        On} -> bind(DB, Gtkid, Canvas, destroy, On);
	{enter,          On} -> bind(DB, Gtkid, Canvas, enter, On);
	{focus,          On} -> bind(DB, Gtkid, Canvas, focus, On);
	{keypress,       On} -> bind(DB, Gtkid, Canvas, keypress, On);
	{keyrelease,     On} -> bind(DB, Gtkid, Canvas, keyrelease, On);
	{leave,          On} -> bind(DB, Gtkid, Canvas, leave, On);
	{motion,         On} -> bind(DB, Gtkid, Canvas, motion, On);

	{secret_hack_gridit, GridGtkid} ->
	    CRef = gtk_db:insert_event(DB, GridGtkid, click, []),
	    ClickCmd = [Canvas, " bind all <ButtonRelease-1> {erlsend ", CRef, 
			" [",Canvas, " find withtag current]};"],
	    DRef = gtk_db:insert_event(DB, GridGtkid, doubleclick, []),
	    DclickCmd = [Canvas," bind all <Double-ButtonRelease-1> {erlsend ",
			 DRef," [",Canvas, " find withtag current]}"],
	    %% bind all at once for preformance reasons.
	    {c, [ClickCmd,DclickCmd]};
	{secret_forwarded_grid_event, {Event,On},GridGtkid} ->
	    bind(DB,GridGtkid,Canvas,Event,On);
	_ -> invalid_option
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: read_option/5
%% Purpose    	: Take care of a read option
%% Return 	: The value of the option or invalid_option
%%		  [OptionValue | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_option(Option,Gtkid,MainW,DB,Canvas) ->
    case Option of
	scrollregion  -> gtk_db:opt(DB,Gtkid,scrollregion);
	{hit, {X,Y}} ->
	    hit(DB,Canvas,X,Y,X,Y);
	{hit, [{X1,Y1},{X2,Y2}]} ->
	    hit(DB,Canvas,X1,Y1,X2,Y2);
	% {% hidden above, % of total area that is visible + % hidden above}
	yscrollpos ->
	    {PercentOffViewTop,_} = tcl2erl:ret_tuple([Canvas," yvi"]),
	    {_,Ymin,_,Ymax} = gtk_db:opt(DB,Gtkid,scrollregion),
	    K = 1/(Ymax-Ymin),
	    M = -K*Ymin,
	    Y = round((PercentOffViewTop - M)/K);
	xscrollpos ->
	    {PercentOffViewLeft,_} = tcl2erl:ret_tuple([Canvas," xvi"]),
	    {Xmin,_,Xmax,_} = gtk_db:opt(DB,Gtkid,scrollregion),
	    K = 1/(Xmax-Xmin),
	    M = -K*Xmin,
	    X = round((PercentOffViewLeft-M)/K);
	buttonpress   -> gtk_db:is_inserted(DB, Gtkid, buttonpress);
	buttonrelease -> gtk_db:is_inserted(DB, Gtkid, buttonrelease);
	configure     -> gtk_db:is_inserted(DB, Gtkid, configure);
	destroy       -> gtk_db:is_inserted(DB, Gtkid, destroy);
	enter         -> gtk_db:is_inserted(DB, Gtkid, enter);
	focus         -> gtk_db:is_inserted(DB, Gtkid, focus);
	keypress      -> gtk_db:is_inserted(DB, Gtkid, keypress);
	keyrelease    -> gtk_db:is_inserted(DB, Gtkid, keyrelease);
	leave         -> gtk_db:is_inserted(DB, Gtkid, leave);   
	motion        -> gtk_db:is_inserted(DB, Gtkid, motion);

	_ -> {bad_result, {Gtkid#gtkid.objtype, invalid_option, Option}}
    end.

hit(DB,Canvas,X1,Y1,X2,Y2) ->
    Ax1 = gtk:to_ascii(X1),
    Ay1 = gtk:to_ascii(Y1),
    Ax2 = gtk:to_ascii(X2),
    Ay2 = gtk:to_ascii(Y2),
    case tcl2erl:ret_list([Canvas," find overlapping ",
			   Ax1,$ ,Ay1,$ ,Ax2,$ ,Ay2]) of
	Items when list(Items) ->
	    [{_,Node}] = ets:lookup(DB,frontend_node),
	    fix_ids(Items,DB,Canvas,Node);
	Other ->
	    {bad_result, Other}
    end.

fix_ids([Item|Items],DB,Canvas,Node) ->
    [{gtk_db:lookup_item(DB,Canvas,Item),Node}|fix_ids(Items,DB,Canvas,Node)];
fix_ids([],_,_,_) -> [].

%%-----------------------------------------------------------------------------
%%			       PRIMITIVES
%%-----------------------------------------------------------------------------

%%
%% Event bind main function
%%
%% Should return a list of tcl commands or invalid_option
%%
%% WS = Widget suffix for c widgets
%%
bind(DB, Gtkid, TkW, Etype, On) ->
    case bind(DB, Gtkid, TkW, Etype, On, "") of
	invalid_option -> invalid_option;
	Cmd -> {c, Cmd}
    end.

bind(DB, Gtkid, TkW, Etype, On, WS) ->
    case On of
	true  -> ebind(DB, Gtkid, TkW, Etype, WS, "");
	false -> eunbind(DB, Gtkid, TkW, Etype, WS, "");
	{true, Edata} -> ebind(DB, Gtkid, TkW, Etype, WS, Edata);
	{false, Edata} -> eunbind(DB, Gtkid, TkW, Etype, WS, Edata);
	_     -> invalid_option
    end.


%%
%% Event bind on
%%
%% Should return a list of tcl commands or invalid_option
%%
%% WS = Widget suffix for complex widgets
%%
ebind(DB, Gtkid, TkW, Etype, WS, Edata) ->
    Eref = gtk_db:insert_event(DB, Gtkid, Etype, Edata),
    P = ["bind ", TkW, WS],
    Cmd = case Etype of
	      motion -> [P, " <Motion> {erlsend ", Eref, " [",
			 TkW, " canvasx %x] [", TkW, " canvasy %y] %x %y}"];
	      keypress ->
		  [P, " <Key> {erlsend ", Eref," %K %N 0 0 [",
		   TkW, " canvasx %x] [", TkW, " canvasy %y]};",
		   P, " <Shift-Key> {erlsend ", Eref, " %K %N 1 0 [",
		   TkW, " canvasx %x] [", TkW, " canvasy %y]};",
		   P, " <Control-Key> {erlsend ", Eref, " %K %N 0 1 [",
		   TkW, " canvasx %x] [", TkW, " canvasy %y]};",
		   P, " <Control-Shift-Key> {erlsend ", Eref," %K %N 1 1 [",
		   TkW, " canvasx %x] [", TkW, " canvasy %y]}"];
	      keyrelease ->
		  [P, " <KeyRelease> {erlsend ", Eref," %K %N 0 0 [",
		   TkW, " canvasx %x] [", TkW, " canvasy %y]};",
		   P, " <Shift-KeyRelease> {erlsend ", Eref, " %K %N 1 0 [",
		   TkW, " canvasx %x] [", TkW, " canvasy %y]};",
		   P, " <Control-KeyRelease> {erlsend ", Eref, " %K %N 0 1 [",
		   TkW, " canvasx %x] [", TkW, " canvasy %y]};",
		  P," <Control-Shift-KeyRelease> {erlsend ",Eref," %K %N 1 1[",
		   TkW, " canvasx %x] [", TkW, " canvasy %y]}"];
	      buttonpress ->
		  [P, " <Button> {erlsend ", Eref, " %b [",
		   TkW, " canvasx %x] [", TkW, " canvasy %y] %x %y}"];
	      buttonrelease ->
		  [P, " <ButtonRelease> {erlsend ", Eref, " %b [",
		   TkW, " canvasx %x] [", TkW, " canvasy %y] %x %y}"];
	      leave -> [P, " <Leave> {erlsend ", Eref, "}"];
	      enter -> [P, " <Enter> {erlsend ", Eref, "}"];
	      destroy ->
		  [P, " <Destroy> {if {\"%W\"==\"", [TkW, WS], 
		   "\"} {erlsend ", Eref, "}}"];
	      focus ->
		  [P, " <FocusIn> {erlsend ", Eref, " true};" ,
		   P, " <FocusOut> {erlsend ", Eref, " false}"];
	      configure ->
		  [P, " <Configure> {if {\"%W\"==\"", [TkW, WS], 
		   "\"} {erlsend ", Eref, " %w %h %x %y}}"]
	  end,
    Cmd.
		  

%%
%% Unbind event
%%
%% Should return a list of tcl commands
%% Already checked for validation in bind/5
%%
%% WS = Widget suffix for complex widgets
%%
eunbind(DB, Gtkid, TkW, Etype, WS, Edata) ->
    gtk_db:delete_event(DB, Gtkid, Etype),
    P = ["bind ", TkW, WS],
    Cmd = case Etype of
	      motion ->
		  [P, " <Motion> {}"];
	      keypress -> 
		   [P, " <KeyRelease> {};",
		    P, " <Shift-KeyRelease> {};",
		    P, " <Control-KeyRelease> {};",
		    P, " <Control-Shift-KeyRelease> {}"];
	      keyrelease -> 
		   [P, " <KeyRelease> {};",
		    P, " <Shift-KeyRelease> {};",
		    P, " <Control-KeyRelease> {};",
		    P, " <Control-Shift-KeyRelease> {}"];
	      buttonpress ->
		  [P, " <ButtonPress> {}"];
	      buttonrelease ->
		  [P, " <ButtonRelease> {}"];
	      leave ->
		  [P, " <Leave> {}"];
	      enter ->
		  [P, " <Enter> {}"];
	      destroy ->
		  [P, " <Destroy> {}"];
	      focus ->
		  [P, " <FocusIn> {};",
		   P, " <FocusOut> {}"];
	      configure ->
		  [P, " <Configure> {}"]
	  end,
    Cmd.

%%======================================================================
%% Item library
%%======================================================================

mk_cmd_and_exec(Options, Gtkid, Canvas, AItem, SCmd, DB) ->
    case make_command(Options, Gtkid, Canvas, AItem, SCmd, DB) of
	{error,Reason} -> {error,Reason};
	Cmd when list(Cmd) ->
	    gtk:exec(Cmd)
    end.

mk_cmd_and_call(Opts,Gtkid, CanvasTkW, MCmd, DB) ->
    case make_command(Opts,Gtkid, CanvasTkW, MCmd, DB) of
	{error,Reason} -> {error,Reason};
	Cmd when list(Cmd) ->
	    case tcl2erl:ret_int(Cmd) of
		Item when integer(Item) ->
		    G2 = gtk_db:lookup_gtkid(DB,Gtkid#gtkid.id), % buu, not nice
		    NewGtkid = G2#gtkid{widget_data=Item},
		    NewGtkid;
		Bad_result ->
		    {error,Bad_result}
	    end
    end.
	    

%%----------------------------------------------------------------------
%% MCmd = Mandatory command
%% Comment: The problem: Create everything in one async command and
%%          get the canvas obj integer id no back then.
%% The trick is to do:
%% set w [canvas create rectangle x1 y1 x2 y2 -Option Value ...];
%% canvas Action $w ;$w
%% Comment: no placer options (we don't have to consider all permutations)
%%----------------------------------------------------------------------
make_command(Options, Gtkid, Canvas, AItem, SCmd, DB) ->
    case gtk_generic:out_opts(Options,Gtkid,Canvas,DB,AItem, [],[],[]) of
	{[], [], []} -> [];
	{Si, [], []} -> [SCmd, Si];
	{[], [], Co} -> Co;
	{Si, [], Co} -> [SCmd, Si, $;, Co];
	{error,Reason} -> {error,Reason}
    end.

make_command(Options, Gtkid, Canvas, MCmd, DB) ->
    case gtk_generic:out_opts(Options,Gtkid,Canvas,DB,"$w",[],[],[]) of
	{[], [], []} -> MCmd;
	{Si, [], []} -> [MCmd, Si];
	{[], [], Co} -> ["set w [", MCmd, "];", Co, "set d $w"];
	{Si, [], Co} -> ["set w [", MCmd, Si, "];", Co, "set d $w"];
	{error,Reason} -> {error,Reason}
    end.

item_config(DB, Gtkid, Opts) ->
    #gtkid{widget=Canvas,widget_data=Item}=Gtkid,
    AItem = gtk:to_ascii(Item),
    SCmd = [Canvas, " itemconf ", AItem],
    case make_command(Opts, Gtkid, Canvas, AItem, SCmd, DB) of
	{error,Reason} -> {error,Reason};
	Cmd when list(Cmd) ->
	    gtk:exec(Cmd)
    end.

pickout_coords([{coords,Coords} | Rest], Opts, ObjType, NbrOfCoords) 
  when length(Coords) == NbrOfCoords ->
    case coords(Coords) of
	invalid ->
	    {error, io_lib:format("A ~w must have ~w coordinates",
				  [ObjType,NbrOfCoords])};
	RealCoords ->
	    {RealCoords, lists:append(Rest, Opts)}
    end;
pickout_coords([Opt | Rest], Opts, ObjType, NbrOfCoords) ->
    pickout_coords(Rest, [Opt|Opts], ObjType, NbrOfCoords);
pickout_coords([], Opts, ObjType, NbrOfCoords) ->
    {error, io_lib:format("A ~w must have ~w coordinates",
			  [ObjType,NbrOfCoords])}.

coords([{X,Y} | R]) when number(X),number(Y) ->
    [gtk:to_ascii(X), " ", gtk:to_ascii(Y), " ", coords(R)];
coords([_]) -> %% not a pair
    invalid;
coords([]) ->
    [].

item_delete_impl(DB,Gtkid) ->
    gtk_db:delete_widget(DB, Gtkid),
    #gtkid{widget=Canvas,widget_data=Item,parent=P,id=ID,objtype=Type}=Gtkid,
    {P,ID,gtk_widgets:type2mod(Type), [Canvas, Item]}.


upd_gtkid(DB, Gtkid, Opts) ->
    #gtkid{parent=Parent,owner=Owner}=Gtkid,
    Pgtkid = gtk_db:lookup_gtkid(DB, Parent, Owner),
    SO = Pgtkid#gtkid.widget_data,
    CanvasTkW = SO#so.object,
    gtk_db:insert_opt(DB,Gtkid,{coords,gs:val(coords,Opts)}),
    gtk_db:update_widget(DB,Gtkid#gtkid{widget=CanvasTkW,widget_data=no_item}).


%%% ----- Done -----


