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
%% Basic Menuitem Type
%% ------------------------------------------------------------

-module(gtk_menuitem).

%%-----------------------------------------------------------------------------
%% 			    MENUITEM OPTIONS
%%
%%  Attribute:
%%	accelerator		String
%%	activebg		Color
%%	activefg		Color
%%	bg			Color
%%	color			Color	(same as fg)
%%	data			Data
%%	fg			Color
%%      font                    Font
%%	group			Atom	(valid only for radio type)
%%	index			Int
%%	itemtype		normal|check|radio|separator|cascade (|tearoff)
%%	label			{text, String} | {image, BitmapFile}
%%	menu			Menu	(valid only for cascade type)
%%	selectbg		Color
%%	underline		Int
%%	value			Atom
%%
%%  Commands:
%%	activate
%%	enable			Bool
%%	invoke
%%
%%  Events:
%%	click			[Bool | {Bool, Data}]
%%
%%  Read Options:
%%	children
%%	id
%%	parent
%%	type
%%
%%  Not Implemented:
%%	font			Font
%%	read menu on cascades
%%

-export([create/3, config/3, read/3, delete/2, destroy/3, event/5,
	option/5,read_option/5,mk_create_opts_for_child/4]).
-include("gtk.hrl").

%%-----------------------------------------------------------------------------
%%			MANDATORY INTERFACE FUNCTIONS
%%-----------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: create/3
%% Purpose    	: Create a widget of the type defined in this module.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(DB, GtkId, Opts) ->
    #gtkid{parent=Parent,owner=Owner,id=Id}=GtkId,
    Pgtkid = gtk_db:lookup_gtkid(DB, Parent),
    TkMenu = Pgtkid#gtkid.widget,
    Widget = "",
    {Index, Type, Options} = parse_opts(Opts, TkMenu),
    PreCmd = [TkMenu, " insert ", gtk:to_ascii(Index)],
    InsertArgs = [DB, Parent,Id, Index],
    case Type of
	check ->
	    {G, GID, NOpts} = fix_group(Options, DB, Owner),
	    TypeCmd = " ch",
	    Ngtkid=GtkId#gtkid{widget=Widget,widget_data={Type, G, GID}},
	    GenArgs = [NOpts,Ngtkid,TkMenu,"","",DB,{Type,Index}],
	    CallArgs = [PreCmd,TypeCmd],
	    mk_it(GenArgs,CallArgs,InsertArgs,Ngtkid);
	radio ->
	    {G, GID, V, NOpts} = fix_group_and_value(Options, DB, Owner),
	    Ngtkid=GtkId#gtkid{widget=Widget, widget_data={Type,G,GID,V}},
	    TypeCmd = " ra",
	    GenArgs = [NOpts,Ngtkid,TkMenu,"", "",DB,{Type,Index}],
	    CallArgs = [PreCmd,TypeCmd],
	    mk_it(GenArgs,CallArgs,InsertArgs,Ngtkid);
	_ ->
	    Ngtkid=GtkId#gtkid{widget=Widget, widget_data=Type},
	    TypeCmd = case Type of
			  normal    -> " co";
			  separator -> " se";
			  cascade   -> " ca"
		      end,
	    GenArgs = [Options,Ngtkid,TkMenu,"","",DB,{Type,Index}],
	    CallArgs = [PreCmd,TypeCmd],
	    mk_it(GenArgs,CallArgs,InsertArgs,Ngtkid)
    end.

mk_it(GenArgs,CallArgs,InsertArgs,Ngtkid) ->
    case apply(gtk_generic,make_command,GenArgs) of
	{error,Reason} -> {error,Reason};
	Cmd when list(Cmd) ->
	    case apply(gtk,call,[[CallArgs|Cmd]]) of
		{result,_} ->
		    apply(gtk_menu,insert_menuitem,InsertArgs),
		    Ngtkid;
		Bad_Result -> {error,Bad_Result}
	    end
    end.

mk_create_opts_for_child(DB,Cgtkid, Pgtkid, Opts) ->
    gtk_generic:mk_create_opts_for_child(DB,Cgtkid,Pgtkid,Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: config/3
%% Purpose    	: Configure a widget of the type defined in this module.
%% Args        	: DB	  - The Database
%%		  Gtkid   - The gtkid of the widget
%%		  Options - A list of options for configuring the widget
%%
%% Return 	: [true | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
config(DB, Gtkid, Options) ->
    Parent = Gtkid#gtkid.parent,
    Pgtkid = gtk_db:lookup_gtkid(DB, Parent),
    TkMenu = Pgtkid#gtkid.widget,
    case Gtkid#gtkid.widget_data of
	{Type, _, _, _} ->
	    Owner = Gtkid#gtkid.owner,
	    {NOpts, NGtkid} = fix_group_and_value(Options, DB, Owner, Gtkid),
	    Index = gtk_menu:lookup_menuitem_pos(DB, Pgtkid, NGtkid#gtkid.id),
	    PreCmd = [TkMenu, " entryco ", gtk:to_ascii(Index)],
	    gtk_generic:mk_cmd_and_exec(NOpts,NGtkid,TkMenu,PreCmd,"",DB,
					{Type,Index});
	{Type, _, _} ->
	    Owner = Gtkid#gtkid.owner,
	    {NOpts, NGtkid} = fix_group(Options, DB, Owner, Gtkid),
	    Index = gtk_menu:lookup_menuitem_pos(DB, Pgtkid, NGtkid#gtkid.id),
	    PreCmd = [TkMenu, " entryco ", gtk:to_ascii(Index)],
	    gtk_generic:mk_cmd_and_exec(NOpts,NGtkid,TkMenu,PreCmd,"",DB,
					{Type,Index});
	Type ->
	    Index = gtk_menu:lookup_menuitem_pos(DB, Pgtkid, Gtkid#gtkid.id),
	    PreCmd = [TkMenu, " entryco ", gtk:to_ascii(Index)],
	    gtk_generic:mk_cmd_and_exec(Options,Gtkid,TkMenu,PreCmd,"",
					DB, {Type,Index})
    end.

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
    Parent = Gtkid#gtkid.parent,
    Id = Gtkid#gtkid.id,
    gtk_db:delete_widget(DB, Gtkid),
    case Gtkid#gtkid.widget_data of
	{radio, _, Gid, _} -> gtk_db:delete_bgrp(DB, Gid);
	{check, _, Gid}    -> gtk_db:delete_bgrp(DB, Gid);
	Other              -> true
    end,
   {Parent, Id, gtk_menuitem, [Id, Parent]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: destroy/3
%% Purpose    	: Destroy a widget
%% Args        	: Menu    - The menu tk widget
%%		  Item    - The index of the menuitem to destroy
%% Return 	: [true | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
destroy(DB, Id, Parent) ->
    Pgtkid = gtk_db:lookup_gtkid(DB, Parent),
    PW = Pgtkid#gtkid.widget,    
    Idx = gtk_menu:lookup_menuitem_pos(DB, Pgtkid, Id),
    gtk_menu:delete_menuitem(DB, Parent, Id),
    gtk:exec([PW, " delete ", gtk:to_ascii(Idx)]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: event/5
%% Purpose    	: Construct the event and send it to the owner of the widget
%% Args        	: Etype   - The event type
%%		  Edata   - The event data
%%		  Args    - The data from tcl/tk
%%		
%% Return 	: [true | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
event(DB, Gtkid, Etype, Edata, Args) ->
    Arg2 = 
	case Gtkid#gtkid.widget_data of
	    {radio, G, GID, V} ->
		[Grp, Text, Idx | Args1] = Args,
		[Text, Idx, G, V | Args1];
	    {check, G, Gid} ->
		[Bool, Text, Idx | Args1] = Args,
		RBool = case Bool of
			    0 -> false;
			    1 -> true
			end,
		[Text, Idx, G, RBool | Args1];
	    Other2 ->
		Args
	end,
    gtk_generic:event(DB, Gtkid, Etype, Edata, Arg2).



%%-----------------------------------------------------------------------------
%%			MANDATORY FUNCTIONS
%%-----------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: option/4
%% Purpose    	: Take care of options
%% Args        	: Option  - An option tuple
%%		  TkW     - The  tk-widget
%% Return 	: A tuple {OptionType, OptionCmd}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
option({click,true}, Gtkid, TkW, DB, {separator,Index}) ->
    none;  % workaround to be able to have {click,true} as default.
option(Option, Gtkid, TkW, DB, {separator,Index}) ->
    invalid_option;

option({menu,{Menu,_RestOfExternalId}}, Gtkid, TkW, DB, {cascade,Index}) ->
    Mgtkid = gtk_db:lookup_gtkid(DB, Menu),
    MenuW = Mgtkid#gtkid.widget,
    {s, [" -menu ", MenuW]};

option({select,false}, Gtkid, TkW, DB, {check,Index}) ->
    {c, ["set x [", TkW, " entrycg ", gtk:to_ascii(Index),
	 " -var];global $x;set $x 0"]};
option({select,true}, Gtkid, TkW, DB, {check,Index}) ->
    {c, ["set x [", TkW, " entrycg ", gtk:to_ascii(Index),
	 " -var];global $x;set $x 1"]};

option({value,Val}, Gtkid, TkW, DB, {radio,Index}) ->
    {s, [" -val ", gtk:to_ascii(Val)]};
option({select,false}, Gtkid, TkW, DB, {radio,Index}) ->
    {c, ["set x [", TkW, " entrycg ", gtk:to_ascii(Index),
	 " -var];global $x;set $x {}"]};
option({select,true}, Gtkid, TkW, DB, {radio,Index}) ->
    {c, ["set x [", TkW, " entrycg ", gtk:to_ascii(Index),
	 " -var]; set y [", TkW, " entrycg ", gtk:to_ascii(Index),
	 " -val]; global $x; set $x $y"]};

option(Option, Gtkid, TkW, DB, {Kind,Index}) ->
    case Option of
	activate  -> {c, [TkW, " act ", gtk:to_ascii(Index)]};
	invoke    -> {c, [TkW, " inv ", gtk:to_ascii(Index)]};
	{accelerator,   Acc} -> {s, [" -acc ", gtk:to_ascii(Acc)]};
	{click,          On} -> cbind(On, Gtkid, TkW, Index, Kind, DB);
	{font, Font} when tuple(Font) ->
	    gtk_db:insert_opt(DB,Gtkid,Option),
	    {s, [" -font ", gtk_font:choose_ascii(DB,Font)]};
	{label, {image,Img}} -> {s, [" -bitm @", Img, " -lab {}"]};
	{label, {text,Text}} -> {s, [" -lab ",gtk:to_ascii(Text)," -bitm {}"]};
	{underline,     Int} -> {s, [" -underl ", gtk:to_ascii(Int)]};
        {activebg,    Color} -> {s, [" -activeba ", gtk:to_color(Color)]};
        {activefg,    Color} -> {s, [" -activefo ", gtk:to_color(Color)]};
        {bg,          Color} -> {s, [" -backg ", gtk:to_color(Color)]};
        {enable,       true} -> {s, " -st normal"};
        {enable,      false} -> {s, " -st disabled"};
        {fg,          Color} -> {s, [" -foreg ", gtk:to_color(Color)]};
	Other -> 
	    case lists:member(Kind,[radio,check]) of
		true -> 
		    case Option of
			{group,Group} -> {s, [" -var ", gtk:to_ascii(Group)]};
			{selectbg,Col} -> {s,[" -selectc ",gtk:to_color(Col)]};
			_ -> invalid_option
		    end;
		_ -> invalid_option
	    end
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: read_option/5
%% Purpose    	: Take care of a read option
%% Return 	: The value of the option or invalid_option
%%		  [OptionValue | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_option(Option,GtkId,TkW,DB,_) ->
    ItemId = GtkId#gtkid.id,
    MenuId = GtkId#gtkid.parent,
    MenuGtkid = gtk_db:lookup_gtkid(DB, MenuId),
    MenuW = MenuGtkid#gtkid.widget,
    Idx = gtk_menu:lookup_menuitem_pos(DB, MenuGtkid, ItemId),
    PreCmd = [MenuW, " entrycg ", gtk:to_ascii(Idx)],
    case Option of
	accelerator   -> tcl2erl:ret_str([PreCmd, " -acc"]);
	activebg      -> tcl2erl:ret_color([PreCmd, " -activeba"]);
	activefg      -> tcl2erl:ret_color([PreCmd, " -activefo"]);
	bg            -> tcl2erl:ret_color([PreCmd, " -backg"]);
	fg            -> tcl2erl:ret_color([PreCmd, " -foreg"]);
	group         -> read_group(GtkId, Option);
	groupid       -> read_groupid(GtkId, Option);
	index         -> Idx;
	itemtype      -> case GtkId#gtkid.widget_data of
			     {Type, _, _, _} -> Type;
			     {Type, _, _} -> Type;
			     Type -> Type
			 end;
	enable        -> tcl2erl:ret_enable([PreCmd, " -st"]);
	font -> gtk_db:opt(DB,GtkId,font,undefined);
	label         -> tcl2erl:ret_label(["list [", PreCmd, " -lab] [",
					    PreCmd, " -bit]"]);
	selectbg      -> tcl2erl:ret_color([PreCmd, " -selectco"]);
	underline     -> tcl2erl:ret_int([PreCmd, " -underl"]);
	value         -> tcl2erl:ret_atom([PreCmd, " -val"]);
	select        -> read_select(MenuW, Idx, GtkId);
	click         -> gtk_db:is_inserted(DB, GtkId, click);
	_ -> {bad_result, {GtkId#gtkid.objtype, invalid_option, Option}}
    end.

read_group(Gtkid, Option) ->
    case Gtkid#gtkid.widget_data of
	{_, G, _, _} -> G;
	{_, G, _}    -> G;
	Other1 -> {bad_result,{Gtkid#gtkid.objtype, invalid_option, Option}}
    end.

read_groupid(Gtkid, Option) ->
    case Gtkid#gtkid.widget_data of
	{_, _, Gid, _} -> Gid;
	{_, _, Gid}    -> Gid;
	Other1 -> {bad_result,{Gtkid#gtkid.objtype, invalid_option, Option}}
    end.




read_select(TkMenu, Idx, Gtkid) ->
    case Gtkid#gtkid.widget_data of
	{radio, _, _, _} ->
	    Cmd = ["list [set x [", TkMenu, " entrycg ", gtk:to_ascii(Idx),
		   " -var];global $x;set $x] [", TkMenu,
		   " entrycg ", gtk:to_ascii(Idx)," -val]"],
	    case tcl2erl:ret_tuple(Cmd) of
		{X, X} -> true;
		Other  -> false
	    end;
	{check, _, _} ->
	    Cmd = ["set x [", TkMenu, " entrycg ", gtk:to_ascii(Idx),
		   " -var];global $x;set $x"],
	    tcl2erl:ret_bool(Cmd);
	Other ->
	    {error,{invalid_option,menuitem,select}}
    end.



%%-----------------------------------------------------------------------------
%%			       PRIMITIVES
%%-----------------------------------------------------------------------------

%% create version
fix_group_and_value(Opts, DB, Owner) ->
    {G, GID, V, NOpts} = fgav(Opts, erlNIL, erlNIL, erlNIL, []),
    RV = case V of
	     erlNIL ->
		 list_to_atom(lists:concat([v,gtk_db:counter(DB,value)]));
	     Other0 -> Other0
	 end,
    NG = case G of
	       erlNIL -> mrb;
	       Other1 -> Other1
	   end,
    RGID = case GID of
	       erlNIL -> {mrbgrp, NG, Owner};
	       Other2 -> Other2
	   end,
    RG = gtk_db:insert_bgrp(DB, RGID),
    {NG, RGID, RV, [{group, RG}, {value, RV} | NOpts]}.
    
%% config version
fix_group_and_value(Opts, DB, Owner, Gtkid) ->
    {Type, RG, RGID, RV} = Gtkid#gtkid.widget_data,
    {G, GID, V, NOpts} = fgav(Opts, RG, RGID, RV, []),
    case {G, GID, V} of
	{RG, RGID, RV} ->
	    {NOpts, Gtkid};
	{NG, RGID, RV} ->
	    NGID = {rbgrp, NG, Owner},
	    gtk_db:delete_bgrp(DB, RGID),
	    NRG = gtk_db:insert_bgrp(DB, NGID),
	    NGtkid = Gtkid#gtkid{widget_data={Type,NG,NGID,RV}},
	    gtk_db:insert_widget(DB, NGtkid),
	    {[{group, NRG} | NOpts], NGtkid};
	{RG, RGID, NRV} ->
	    NGtkid = Gtkid#gtkid{widget_data={Type,RG,RGID,NRV}},
	    gtk_db:insert_widget(DB, NGtkid),
	    {[{value,NRV} | NOpts], NGtkid};
	{_, NGID, RV} when NGID =/= RGID ->
	    gtk_db:delete_bgrp(DB, RGID),
	    NRG = gtk_db:insert_bgrp(DB, NGID),
	    NGtkid = Gtkid#gtkid{widget_data={Type,RG,NGID,RV}},
	    gtk_db:insert_widget(DB, NGtkid),
	    {[{group, NRG} | NOpts], NGtkid};
	{_, NGID, NRV} when NGID =/= RGID ->
	    gtk_db:delete_bgrp(DB, RGID),
	    NRG = gtk_db:insert_bgrp(DB, NGID),
	    NGtkid = Gtkid#gtkid{widget_data={Type,RG,NGID,NRV}},
	    gtk_db:insert_widget(DB, NGtkid),
	    {[{group, NRG}, {value,NRV} | NOpts], NGtkid};
	{NG, RGID, NRV} ->
	    NGID = {rbgrp, NG, Owner},
	    gtk_db:delete_bgrp(DB, RGID),
	    NRG = gtk_db:insert_bgrp(DB, NGID),
	    NGtkid = Gtkid#gtkid{widget_data={Type,NG,NGID,NRV}},
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


%% check button version
%% create version
fix_group(Opts, DB, Owner) ->
    {G, GID, NOpts} = fg(Opts, erlNIL, erlNIL, []),
    NG = case G of
	       erlNIL ->
		 Vref = gtk_db:counter(DB, variable),
		 list_to_atom(lists:flatten(["mcb", gtk:to_ascii(Vref)]));
	       Other1 -> Other1
	   end,
    RGID = case GID of
	       erlNIL -> {mcbgrp, NG, Owner};
	       Other2 -> Other2
	   end,
    RG = gtk_db:insert_bgrp(DB, RGID),
    {NG, RGID, [{group, RG} | NOpts]}.
    
%% config version
fix_group(Opts, DB, Owner, Gtkid) ->
    {Type, RG, RGID} = Gtkid#gtkid.widget_data,
    {G, GID, NOpts} = fg(Opts, RG, RGID, []),
    case {G, GID} of
	{RG, RGID} ->
	    {NOpts, Gtkid};
	{NG, RGID} ->
	    NGID = {cbgrp, NG, Owner},
	    gtk_db:delete_bgrp(DB, RGID),
	    NRG = gtk_db:insert_bgrp(DB, NGID),
	    NGtkid = Gtkid#gtkid{widget_data={Type,NG,NGID}},
	    gtk_db:insert_widget(DB, NGtkid),
	    {[{group, NRG} | NOpts], NGtkid};
	{_, NGID} when NGID =/= RGID ->
	    gtk_db:delete_bgrp(DB, RGID),
	    NRG = gtk_db:insert_bgrp(DB, NGID),
	    NGtkid = Gtkid#gtkid{widget_data={Type,RG,NGID}},
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



parse_opts(Opts, TkMenu) ->
    parse_opts(Opts, TkMenu, none, none, []).


parse_opts([Option | Rest], TkMenu, Idx, Type, Options) ->
    case Option of
	{index,    I} -> parse_opts(Rest, TkMenu, I, Type, Options);
	{itemtype, T} -> parse_opts(Rest, TkMenu, Idx, T, Options);
	Other         -> parse_opts(Rest, TkMenu, Idx, Type,[Option | Options])
    end;
parse_opts([], TkMenu, Index, Type, Options) ->
    RealIdx =
	case Index of
	    Idx when integer(Idx) -> Idx;
	    last  -> find_last_index(TkMenu);
	    Other -> gs:error("Invalid index ~p~n",[Other])
	end,
    {RealIdx, Type, Options}.

find_last_index(TkMenu) ->
    case tcl2erl:ret_int([TkMenu, " index last"]) of
	Last when integer(Last) -> Last+1;
	none  -> 0;
	Other -> gs:error("Couldn't find index ~p~n",[Other])
    end.

cbind({true, Edata}, Gtkid, TkMenu, Index, Type, DB) ->
    Eref = gtk_db:insert_event(DB, Gtkid, click, Edata),
    case Type of
	normal ->
	    Cmd = [" -com {set x [",TkMenu, " index active];erlsend ", Eref,
		   " \\\"[",TkMenu," entrycg $x -label]\\\" $x}"],
	    {s, Cmd};
	check ->
	    Cmd = [" -com {set x [",TkMenu, " index active];erlsend ", Eref,
		   " \[expr \$[", TkMenu, " entrycg $x -var]\] \\\"[",
		   TkMenu, " entrycg $x -label]\\\" $x}"],
	    {s, Cmd};
	radio ->
	    Cmd = [" -com {set x [",TkMenu, " index active];erlsend ", Eref,
		   " [", TkMenu, " entrycg $x -var] \\\"[",
		   TkMenu, " entrycg $x -label]\\\" $x}"],
	    {s, Cmd};
	Other ->
	    none
    end;

cbind({false, _}, Gtkid, TkMenu, Index, Type, DB) ->
    gtk_db:delete_event(DB, Gtkid, click),
    none;

cbind(On, Gtkid, TkMenu, Index, Type, DB) when atom(On) ->
    cbind({On, []}, Gtkid, TkMenu, Index, Type, DB).


%%% ----- Done -----

