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
-module(gtk_gridline).

-export([event/5,create/3,config/3,option/5,read/3,delete/2,destroy/2,
	read_option/5]).

-include("gtk.hrl").
-record(state,{canvas,ncols,max_range,cell_id, cell_pos,ids,db,tkcanvas}).
-record(item,{text_id,rect_id,line_id}).

%%-----------------------------------------------------------------------------
%% 			    GRIDLINE OPTIONS
%%
%%	text		Text
%%	row             Row
%%	data		Data
%%	fg		Color (default is the same as grid fg)
%%	click         	Bool
%%
%%-----------------------------------------------------------------------------

create(DB, Gtkid, Options) ->
    Pgtkid = gtk_db:lookup_gtkid(DB,Gtkid#gtkid.parent),
    Id = Gtkid#gtkid.id,
    #gtkid{widget_data=State} = Pgtkid,
    #state{cell_pos=CP,tkcanvas=TkW,ncols=Ncols} = State,
    Row = gs:val(row,Options),
    case check_row(CP,Row) of
	{error,Reason} -> {error,Reason};
	ok ->
	    Ngtkid = Gtkid#gtkid{widget=TkW},
	    gtk_db:insert_opts(DB,Id,[{data,[]},{row,Row}]),
	    update_cp_db(Ncols,Row,Id,CP),
	    config_line(DB,Pgtkid,Ngtkid,Options),
	    Ngtkid
    end.

%%----------------------------------------------------------------------
%% Returns: ok|false
%%----------------------------------------------------------------------
check_row(CellPos,undefined) ->
    {error,{gridline,{row,undefined}}};
check_row(CellPos,Row) ->
    case ets:lookup(CellPos,{1,Row}) of
	[] ->
	    {error,{gridline,row_outside_range,Row}};
	[{_,Item}] ->
	    case Item#item.line_id of
		free -> ok;
		_    ->
		    {error,{gridline,row_is_occupied,Row}}
	    end
    end.

%%----------------------------------------------------------------------
%% s => text item
%% p => rect item
%%----------------------------------------------------------------------
option(Option, Gtkid, TkW, DB,_) ->
    case Option of
	{{bg,Item}, Color} -> {p,[" -f ", gtk:to_color(Color)]};
	{{text,Item},Text} -> {s, [" -te ", gtk:to_ascii(Text)]};
	{{fg,Item},Color} -> {sp,{[" -fi ", gtk:to_color(Color)],
				  [" -outline ", gtk:to_color(Color)]}};
	{{font,Item},Font} -> {s,[" -font ",gtk_font:choose_ascii(DB,Font)]};
	Q -> invalid_option
    end.

%%----------------------------------------------------------------------
%% Is always called.
%% Clean-up my specific side-effect stuff.
%%----------------------------------------------------------------------
delete(DB, Gtkid) ->
    gtk_db:delete_widget(DB, Gtkid),
    {Gtkid#gtkid.parent, Gtkid#gtkid.id, gtk_gridline,
     [Gtkid]}.

%%----------------------------------------------------------------------
%% Is called iff my parent is not also destroyed.
%%----------------------------------------------------------------------
destroy(DB, Lgtkid) ->
    Row = gtk_db:opt(DB,Lgtkid,row),
    Ggtkid = gtk_db:lookup_gtkid(DB,Lgtkid#gtkid.parent),
    #gtkid{widget_data=State} = Ggtkid,
    config_line(DB,Ggtkid,Lgtkid,
		[{bg,gtk_db:opt(DB,Ggtkid,bg)},
		 {fg,gtk_db:opt(DB,Ggtkid,fg)},{text,""}]),
    Ncols = State#state.ncols,
    update_cp_db(Ncols,Row,free,State#state.cell_pos).


config(DB, Gtkid, Opts) ->
    Pgtkid = gtk_db:lookup_gtkid(DB,Gtkid#gtkid.parent),
    case {gs:val(row,Opts,missing),gtk_db:opt(DB,Gtkid,row)} of
	{Row,Row} -> % stay here...
	    config_line(DB,Pgtkid,Gtkid,Opts);
	{missing,_} ->  % stay here...
	    config_line(DB,Pgtkid,Gtkid,Opts);
	{NewRow,OldRow} ->
	    config_line(DB,Pgtkid,Gtkid,Opts),
	    Ngtkid = gtk_db:lookup_gtkid(DB,Gtkid#gtkid.id),
	    case move_line(NewRow,OldRow,DB,Pgtkid#gtkid.widget_data,Ngtkid) of
		true -> 
		    gtk_db:insert_opt(DB,Ngtkid,{row,NewRow}),
		    ok;
		{error,Reason} -> {error,Reason}
	    end
    end,
    ok.

%%----------------------------------------------------------------------
%% Returns: true|false depending on if operation succeeded
%%----------------------------------------------------------------------
move_line(NewRow,OldRow,DB,State,Ngtkid) ->
    case ets:lookup(State#state.cell_pos,{1,NewRow}) of
	[] ->
	    {error,{gridline,row_outside_grid,NewRow}};
	[{_,#item{line_id=Lid}}] when Lid =/= free->
	    {error,{gridline,new_row_occupied,NewRow}};
	[{_,NewItem}] ->
	    #state{tkcanvas=TkW,ncols=Ncols,cell_pos=CP} = State,
	    swap_lines(TkW,OldRow,NewRow,1,Ncols,CP),
	    true
    end.

%%----------------------------------------------------------------------
%% Purpose: swaps an empty newrow with a (oldrow) gridline
%%----------------------------------------------------------------------
swap_lines(TkW,OldRow,NewRow,Col,MaxCol,CellPos) when Col =< MaxCol ->
    [{_,NewItem}] = ets:lookup(CellPos,{Col,NewRow}),
    [{_,OldItem}] = ets:lookup(CellPos,{Col,OldRow}),
    swap_cells(TkW,NewItem,OldItem),
    ets:insert(CellPos,{{Col,NewRow},OldItem}),
    ets:insert(CellPos,{{Col,OldRow},NewItem}),
    swap_lines(TkW,OldRow,NewRow,Col+1,MaxCol,CellPos);
swap_lines(_,_,_,_,_,_) -> done.

swap_cells(TkW,#item{rect_id=NewRectId,text_id=NewTextId},
	   #item{rect_id=OldRectId,text_id=OldTextId}) ->
    Aorid = gtk:to_ascii(OldRectId),
    Aotid = gtk:to_ascii(OldTextId),
    Anrid = gtk:to_ascii(NewRectId),
    Antid = gtk:to_ascii(NewTextId),
    Pre = [TkW," coords "],
    OldRectCoords = tcl2erl:ret_str([Pre,Aorid]),
    OldTextCoords = tcl2erl:ret_str([Pre,Aotid]),
    NewRectCoords = tcl2erl:ret_str([Pre,Anrid]),
    NewTextCoords = tcl2erl:ret_str([Pre,Antid]),
    gtk:exec([Pre,Aotid," ",NewTextCoords]),
    gtk:exec([Pre,Antid," ",OldTextCoords]),
    gtk:exec([Pre,Aorid," ",NewRectCoords]),
    gtk:exec([Pre,Anrid," ",OldRectCoords]).

%%----------------------------------------------------------------------
%% Pre: {row,Row} option is taken care of.
%%----------------------------------------------------------------------
config_line(DB,Pgtkid,Lgtkid, Opts) ->
    #gtkid{widget_data=State, widget=TkW} = Pgtkid,
    #state{cell_pos=CP,ncols=Ncols} = State,
    Row = gtk_db:opt(DB,Lgtkid,row),
    Ropts = transform_opts(Opts,Ncols),
    RestOpts = config_gridline(DB,CP,Lgtkid,Ncols,Row,Ropts),
    gtk_generic:mk_cmd_and_exec(RestOpts,Lgtkid,TkW,"","",DB).

%%----------------------------------------------------------------------
%% Returns: non-processed options
%%----------------------------------------------------------------------
config_gridline(DB,CP,Gtkid,0,Row,Opts) ->
    Opts;
config_gridline(DB,CP,Gtkid,Col,Row,Opts) ->
    case opts_for_col(Col,Opts,[],[]) of
	{[],OtherOpts} -> done;
	{ColOpts,OtherOpts} ->
	    [{_pos,Item}] =  ets:lookup(CP,{Col,Row}),
	    TkW = Gtkid#gtkid.widget,
	    TextPre = [TkW," itemconf ",gtk:to_ascii(Item#item.text_id)],
	    RectPre = [$;,TkW," itemconf ",gtk:to_ascii(Item#item.rect_id)],
	    case gtk_generic:make_command(ColOpts,Gtkid,TkW,
					  TextPre,RectPre,DB) of
		[] -> ok;
		{error,Reason} -> {error,Reason};
		Cmd -> gtk:exec(Cmd)
	    end
    end,
    config_gridline(DB,CP,Gtkid,Col-1,Row,OtherOpts).

opts_for_col(Col,[{{Key,Col},Val}|Opts],ColOpts,RestOpts) ->
    opts_for_col(Col,Opts,[{{Key,Col},Val}|ColOpts],RestOpts);
opts_for_col(Col,[Opt|Opts],ColOpts,RestOpts) ->
    opts_for_col(Col,Opts,ColOpts,[Opt|RestOpts]);
opts_for_col(Col,[],ColOpts,RestOpts) -> {ColOpts,RestOpts}.

%%----------------------------------------------------------------------
%% {Key,{Col,Val}} becomes {{Key,Col},Val}
%% {Key,Val} becomes {{Key,1},Val}...{{Key,Ncol},Val}
%%----------------------------------------------------------------------
transform_opts([], Ncols) -> [];
transform_opts([{{Key,Col},Val} | Opts],Ncols) ->
    [{{Key,Col},Val}|transform_opts(Opts,Ncols)];
transform_opts([{Key,{Col,Val}}|Opts],Ncols) when integer(Col) ->
    [{{Key,Col},Val}|transform_opts(Opts,Ncols)];
transform_opts([{Key,Val}|Opts],Ncols) ->
    case lists:member(Key,[fg,bg,text,font]) of
	true -> 
	    lists:append(expand_to_all_cols(Key,Val,Ncols),
			 transform_opts(Opts,Ncols));
	false ->
	    case lists:member(Key,[click,doubleclick,row]) of
		true ->
		    [{keep_opt,{Key,Val}}|transform_opts(Opts,Ncols)];
		false ->
		    [{Key,Val}|transform_opts(Opts,Ncols)]
	    end
    end;
transform_opts([Opt|Opts],Ncols) ->
    [Opt|transform_opts(Opts,Ncols)].

expand_to_all_cols(Key,Val,1) ->
    [{{Key,1},Val}];
expand_to_all_cols(Key,Val,Col) ->
    [{{Key,Col},Val}|expand_to_all_cols(Key,Val,Col-1)].
		     

read(DB, Gtkid, Opt) ->
    Pgtkid = gtk_db:lookup_gtkid(DB,Gtkid#gtkid.parent),
    gtk_generic:read_option(DB, Gtkid, Opt,Pgtkid).

read_option({font,Column},Gtkid, TkW,DB,Pgtkid) -> 
    case gtk_db:opt_or_not(DB,Gtkid,{font,Column}) of
	false -> gtk_db:opt(DB,Pgtkid,font);
	{value,V} -> V
    end;
read_option({Opt,Column},Gtkid, TkW,DB,#gtkid{widget_data=State}) -> 
    Row = gtk_db:opt(DB,Gtkid,row),
    [{_pos,Item}] = ets:lookup(State#state.cell_pos,{Column,Row}),
    Rid = gtk:to_ascii(Item#item.rect_id),
    Tid = gtk:to_ascii(Item#item.text_id),
    Pre = [TkW," itemcg "],
    case Opt of
	bg -> tcl2erl:ret_color([Pre,Rid," -f"]);
	fg -> tcl2erl:ret_color([Pre,Tid," -fi"]);
	text -> tcl2erl:ret_str([Pre,Tid," -te"]);
	Q -> {bad_result, {Gtkid#gtkid.objtype, invalid_option, {Opt,Column}}}
    end;
read_option(Option,Gtkid,TkW,DB,Pgtkid) -> 
    case lists:member(Option,[bg,fg,text]) of
	      true -> read_option({Option,1},Gtkid,TkW,DB,Pgtkid);
	      false -> gtk_db:opt(DB,Gtkid,Option,undefined)
    end.

update_cp_db(0,Row,_,_) -> ok;
update_cp_db(Col,Row,ID,CP) ->
    [{_,Item}] = ets:lookup(CP,{Col,Row}),
    ets:insert(CP,{{Col,Row},Item#item{line_id = ID}}),
    update_cp_db(Col-1,Row,ID,CP).


event(DB, GridGtkid, Etype, _Edata, [CanItem]) ->
    State = GridGtkid#gtkid.widget_data,
    #state{cell_pos=CP,cell_id=CIs,tkcanvas=TkW} = State,
    case ets:lookup(CIs,CanItem) of 
	[{_id,{Col,Row}}] ->
	    [{_pos,Item}] =  ets:lookup(CP,{Col,Row}),
	    case Item#item.line_id of
		free -> ok;
		Id   ->
		    Lgtkid = gtk_db:lookup_gtkid(DB,Id),
		    case gtk_db:opt_or_not(DB,Lgtkid,Etype) of
			{value,true}  ->
			    Txt = read_option({text,Col},Lgtkid,TkW,
					      DB,GridGtkid),
			    gtk_generic:event(DB,Lgtkid,Etype,dummy,
					      [Col,Row,Txt]);
			_ -> ok
		    end
	    end;
	_ -> ok
    end;
event(DB, Gtkid, _Etype, _Edata, Args) ->
    ok.
