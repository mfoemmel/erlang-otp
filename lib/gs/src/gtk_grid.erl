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
-module(gtk_grid).

-export([event/5,create/3,config/3,option/5,read/3,delete/2,destroy/2,
	 mk_create_opts_for_child/4,read_option/5]).

-include("gtk.hrl").

%%-----------------------------------------------------------------------------
%% 			    GRID OPTIONS
%%
%%	rows		{ViewFrom, ViewTo}
%%	columnwidths	[CW1, CW2, ..., CWn]
%%	vscroll		Bool | left | right
%%	hscroll		Bool | top | bottom
%%	x		Coord
%%	y		Coord
%%	width		Int
%%	height		Int
%%	fg		Color  (lines and default line color)
%%	bg		Color
%%-----------------------------------------------------------------------------

-record(state,{canvas,ncols,max_range,cell_id, cell_pos,ids,db,tkcanvas}).
-record(item,{text_id,rect_id,line_id}).

%%======================================================================
%% Interfaces
%%======================================================================

event(DB, Gtkid, Etype, Edata, Args) ->
    gtk_gridline:event(DB, Gtkid, Etype, Edata, Args).

create(DB, Gtkid, Options) ->
    WinParent=Gtkid#gtkid.parent,
    {OtherOpts,CanvasOpts} = parse_opts(Options,[],[]),
    %% Why this (secret) hack? Performance reasons.
    %% if we ".canvas bind all" once and for all, then we can
    %% create lines twice as fast since we don't have to bind each line.
    C = make_ref(),
    gtk:create_impl(DB,{a_grid, {canvas,C,WinParent,
				 [{secret_hack_gridit, Gtkid}
				  | CanvasOpts]}}),
    CanvasGtkid = gtk_db:lookup_gtkid(DB, C),
    Wid = CanvasGtkid#gtkid.widget,
    SO = CanvasGtkid#gtkid.widget_data,
    TkCanvas = SO#so.object,
    CI=ets:new(gtk_grid_cellid,[private,set]),
    CP=ets:new(gtk_grid_cellpos,[private,set]),
    IDs=ets:new(gtk_grid_id,[private,set]),
    S=#state{db=DB,ncols=length(gs:val(columnwidths,OtherOpts)),
	     canvas=C,cell_id=CI,tkcanvas=TkCanvas,cell_pos=CP,ids=IDs},
    Ngtkid = Gtkid#gtkid{widget=Wid,widget_data=S},
    gtk_db:insert_opts(DB,Ngtkid,OtherOpts),
    gtk_db:insert_widget(DB,Ngtkid),
    gtk_generic:mk_cmd_and_exec(lists:keydelete(columnwidths,1,OtherOpts),
				Ngtkid, TkCanvas,"","", DB,nop).

config(DB, Gtkid, Options) ->
    #gtkid{widget=TkW,widget_data=State}=Gtkid,
    {OtherOpts,CanvasOpts} = parse_opts(Options,[],[]),
    case gtk:config_impl(DB,State#state.canvas,CanvasOpts) of
	ok ->
	    SimplePreCmd = "nyi?",
	    PlacePreCmd = [";place ", TkW],
	    gtk_generic:mk_cmd_and_exec(OtherOpts,Gtkid,TkW,
					SimplePreCmd,PlacePreCmd,DB,State);
	Err -> Err
    end.
    

option(Option, Gtkid, TkW, DB,State) ->
    case Option of
	{rows,{From,To}} ->
	    Ngtkid = reconfig_rows(From,To,Gtkid),
	    gtk_db:insert_opt(DB,Gtkid,Option),
	    gtk_db:update_widget(DB,Ngtkid),
	    {none,Ngtkid};
	{fg,Color} ->
	    reconfig_grid(DB,Option,State),
	    gtk_db:insert_opt(DB,Gtkid,Option),
	    none;
	{bg,Color} ->
	    reconfig_grid(DB,Option,State),
	    gtk_db:insert_opt(DB,Gtkid,Option),
	    none;
	{font,Font} ->
	    reconfig_grid(DB,Option,State),
	    gtk_db:insert_opt(DB,Gtkid,Option),
	    none;
	{columnwidths,ColWs} ->
	    gtk_db:insert_opt(DB,Gtkid,Option),
	    Rows = gtk_db:opt(DB,Gtkid,rows),
	    CellHeight = gtk_db:opt(DB,Gtkid,cellheight),
	    gtk:config_impl(DB,State#state.canvas,
			    [calc_scrollregion(Rows,ColWs,CellHeight)]),
	    %% Crash upon an error msg (so we know WHY)
	    {result,_} = gtk:call(["resize_grid_cols ",State#state.tkcanvas,
				   " [list ",asc_tcl_colw(ColWs),"]"]),
	    none;
	{cellheight,Height} ->
	    gtk_db:insert_opt(DB,Gtkid,Option),
	    none;	
	Q ->
	    invalid_option
    end.

reconfig_grid(_,_,nop) -> done;
reconfig_grid(DB,Option,#state{tkcanvas=TkW,cell_pos=CP,
			       ncols=Ncols,max_range={From,To}}) ->
    reconfig_grid(DB,TkW,Option,From,To,CP,Ncols).
    
reconfig_grid(DB,TkW,Opt,Row,MaxRow,CellPos,Ncols) when Row =< MaxRow ->
    [{_,Item}] = ets:lookup(CellPos,{1,Row}),
    case Item#item.line_id of
	free -> empty_cell_config(DB,TkW,Row,1,Ncols,CellPos,Opt);
	GridLine ->
	    gtk_gridline:config(DB,gtk_db:lookup_gtkid(DB,GridLine),
				[Opt])
    end,
    reconfig_grid(DB,TkW,Opt,Row+1,MaxRow,CellPos,Ncols);
reconfig_grid(_,_,_,_,_,_,_) -> done.

%%----------------------------------------------------------------------
%% Purpose: Config an empty cell (i.e. has no gridline)
%%----------------------------------------------------------------------
empty_cell_config(DB,TkW,Row,Col,Ncols,CellPos,Opt) when Col =< Ncols ->
    [{_,Item}] = ets:lookup(CellPos,{Col,Row}),
    empty_cell_config(DB,TkW,Item,Opt),
    empty_cell_config(DB,TkW,Row,Col+1,Ncols,CellPos,Opt);
empty_cell_config(_,_,_,_,_,_,_) -> done.

empty_cell_config(_,TkW,#item{rect_id=Rid},{bg,Color}) ->
    gtk:exec([TkW," itemconf ",gtk:to_ascii(Rid)," -f ",gtk:to_color(Color)]);
empty_cell_config(_,TkW,#item{rect_id=Rid,text_id=Tid},{fg,Color}) ->
    Acolor = gtk:to_color(Color),
    Pre = [TkW," itemconf "],
    RectStr = [Pre, gtk:to_ascii(Rid)," -outline ",Acolor],
    TexdStr = [Pre,  gtk:to_ascii(Tid)," -fi ",Acolor],
    gtk:exec([RectStr,$;,TexdStr]);
empty_cell_config(DB,TkW,#item{text_id=Tid},{font,Font}) ->
    gtk:exec([TkW," itemconf ",gtk:to_ascii(Tid)," -font ",
	      gtk_font:choose_ascii(DB,Font)]);
empty_cell_config(_,_,_,_) -> done.



reconfig_rows(From, To, Gtkid) ->
    #gtkid{widget_data=State,id=Id} = Gtkid,
    #state{tkcanvas=TkCanvas,cell_pos=CP,cell_id=CI,
	   canvas=C,db=DB,max_range=Range}=State,
    NewRange =
	if Range == undefined ->
		mkgrid(DB,CP,CI,TkCanvas,Id,From,To),
		{From,To};
	   true ->
		{Top,Bot} = Range,
		if
		    From < Top -> % we need more rects above
			mkgrid(DB,CP,CI,TkCanvas,Id,From,Top-1);
		    true -> true
		end,
		if
		    To > Bot ->   % we need more rects below
			mkgrid(DB,CP,CI,TkCanvas,Id,Bot+1,To);
		    true -> true
		end,
		{lists:min([Top, From]), lists:max([Bot, To])}
	end,
    gtk:config_impl(DB,C,[calc_scrollregion({From,To},
					    gtk_db:opt(DB,Id,columnwidths),
					    gtk_db:opt(DB,Id,cellheight))]),
    S2 = State#state{max_range=NewRange},
    Gtkid#gtkid{widget_data=S2}.

read(DB,Gtkid,Opt) ->
    State = Gtkid#gtkid.widget_data,
    case lists:member(Opt,[x,y,width,height,hscroll,vscroll]) of
	true -> gtk:read_impl(DB,State#state.canvas,Opt);
	false ->
	    gtk_generic:read_option(DB, Gtkid, Opt,State)
      end.

read_option(Option,Gtkid,TkW,DB,State) -> 
    case Option of
	{obj_at_row,Row} ->
	    case ets:lookup(State#state.cell_pos,{1,Row}) of
		[{_pos,Item}] ->
		    case Item#item.line_id of
			free -> undefined;
			GridLine ->
			    gtk:make_extern_id(GridLine, DB)
		    end;
		Q -> undefined
	    end;
	Opt -> gtk_db:opt(DB,Gtkid#gtkid.id,Opt,undefined)
    end.


%%----------------------------------------------------------------------
%% Is always called.
%% Clean-up my specific side-effect stuff.
%%----------------------------------------------------------------------
delete(DB, Gtkid) ->
    gtk_db:delete_widget(DB, Gtkid),
    State = Gtkid#gtkid.widget_data,
    #state{canvas=C,cell_pos=CP,cell_id=CIs, ids=IDs} = State,
    ets:delete(CP),
    ets:delete(CIs),
    ets:delete(IDs),
    TkCanvas = Gtkid#gtkid.widget,
    {Gtkid#gtkid.parent, Gtkid#gtkid.id, gtk_grid, [C]}.

%%----------------------------------------------------------------------
%% Is called iff my parent is not also destroyed.
%%----------------------------------------------------------------------
destroy(DB, Canvas) ->
    gtk:destroy_impl(DB,gtk_db:lookup_gtkid(DB,Canvas)).

mk_create_opts_for_child(DB,Cgtkid, Pgtkid, Opts) ->
    gtk_generic:mk_create_opts_for_child(DB,Cgtkid,Pgtkid,Opts).

mkgrid(DB,CellPos,CellIds,TkCanvas,Id,From,To) ->
    ColWs = gtk_db:opt(DB,Id,columnwidths),
    AscColW = ["[list ",asc_tcl_colw(ColWs),"]"],
    Font = gtk_font:choose_ascii(DB,gtk_db:opt(DB,Id,font)),
    Fg = gtk:to_color(gtk_db:opt(DB,Id,fg)),
    Bg = gtk:to_color(gtk_db:opt(DB,Id,bg)),
    Objs = tcl2erl:ret_list(["mkgrid ",TkCanvas," ",AscColW," ",
			     gtk:to_ascii(From)," ",
			     gtk:to_ascii(To)," ",
			     gtk:to_ascii(gtk_db:opt(DB,Id,cellheight))," ",
			     Font," ",Fg," ",Bg]),
    insert_objs(CellPos,CellIds,From,To,1,length(ColWs)+1,Objs).

insert_objs(_,_,_,_,_,_,[]) -> done;
insert_objs(CP,CI,Row,T,MaxCol,MaxCol,Objs) ->
    insert_objs(CP,CI,Row+1,T,1,MaxCol,Objs);
insert_objs(CellPos,CellIds,Row,To,Col,Ncols,[RectId,TextId|Objs]) ->
    ets:insert(CellPos,{{Col,Row},
		   #item{text_id=TextId,rect_id=RectId,line_id=free}}),
    ets:insert(CellIds,{RectId,{Col,Row}}),
    ets:insert(CellIds,{TextId,{Col,Row}}),
    insert_objs(CellPos,CellIds,Row,To,Col+1,Ncols,Objs).

asc_tcl_colw([]) -> "";
asc_tcl_colw([Int|T]) -> [gtk:to_ascii(Int)," "|asc_tcl_colw(T)].

%%----------------------------------------------------------------------
%% Args: Cols  list of column sizes (measured in n-chars)
%%----------------------------------------------------------------------
calc_scrollregion({From, To}, Cols, Height) ->
    {scrollregion, {0, ((From-1) * Height) + From,
		    lists:sum(Cols)+length(Cols)+1, (To * Height)+ To+1}}.

parse_opts([],OtherOpts,CanvasOpts) -> {OtherOpts,CanvasOpts};
parse_opts([{Key,Val}|Opts],OtherOpts,CanvasOpts) ->
    case lists:member(Key,[x,y,width,height,vscroll,hscroll]) of
	true -> parse_opts(Opts,OtherOpts,[{Key,Val}|CanvasOpts]);
	false -> parse_opts(Opts,[{Key,Val}|OtherOpts],CanvasOpts)
    end;
parse_opts([Opt|Opts],OtherOpts,CanvasOpts) ->
    parse_opts(Opts,[Opt|OtherOpts],CanvasOpts).

