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

-module(gtk_generic).

%%-export([bind/5,make_command/6,make_command/7]).
-compile(export_all).
-include("gtk.hrl").
-include("gtk_generic.hrl").

%%----------------------------------------------------------------------
%% Returns: a new unique TkWidget (string())
%%----------------------------------------------------------------------
mk_tkw_child(DB,#gtkid{parent=P,objtype=Ot}) ->
    Pgtkid = gtk_db:lookup_gtkid(DB, P),
    PW = Pgtkid#gtkid.widget,
    Oref = gtk_db:counter(DB, Ot),
    PF = gtk_widgets:suffix(Ot),
    TkW = lists:concat([PW, PF, Oref]).

%%----------------------------------------------------------------------
%% Purpose: Merges options. Opts have higher priority than BuiltIn
%%          (and ParentOpts have higher than BuiltIn)
%% Returns: A list of new options.
%%----------------------------------------------------------------------
merge_default_options(ParOpts, BuildInOpts, Opts) ->
    %% parents options first
    Tmp=merge_default_options(ParOpts, lists:sort(Opts)), 
    merge_default_options(BuildInOpts,Tmp).

merge_default_options([Def|Ds],[Opt|Os]) 
  when element(1,Def) < element(1,Opt) ->
    [Def | merge_default_options(Ds,[Opt|Os])];

merge_default_options([Def|Ds],[Opt|Os]) 
  when element(1,Def) > element(1,Opt) ->
    [Opt | merge_default_options([Def|Ds],Os)];

merge_default_options([Def|Ds],[Opt|Os]) 
  when element(1,Def) == element(1,Opt) ->
    [Opt | merge_default_options(Ds,Os)];

merge_default_options(Defs,[Opt|Os]) ->
    [Opt | merge_default_options(Defs,Os)];

merge_default_options([],Opts) -> Opts;
merge_default_options(Defs,[]) -> Defs.

opts_for_child(DB,Childtype,ParId) ->
    case gs_widgets:container(Childtype) of
	true -> 
	    gtk_db:default_container_opts(DB,ParId,Childtype);
	false ->
	    gtk_db:default_opts(DB,ParId,Childtype)
    end.

mk_create_opts_for_child(DB,#gtkid{objtype=ChildType}, Pgtkid, Opts) ->
    merge_default_options(
      opts_for_child(DB,ChildType,Pgtkid#gtkid.id),
      gs_widgets:default_options(ChildType),
      Opts).

mk_cmd_and_exec(Opts,Gtkid,Scmd,DB) ->
    TkW = Gtkid#gtkid.widget,
    mk_cmd_and_exec(Opts,Gtkid,TkW,Scmd,[";place ", TkW],DB,dummy).
mk_cmd_and_exec(Opts,Gtkid,Scmd,Pcmd,DB) ->
    mk_cmd_and_exec(Opts,Gtkid,Gtkid#gtkid.widget,Scmd,Pcmd,DB,dummy).
mk_cmd_and_exec(Options, Gtkid, TkW, SCmd, PCmd, DB) ->
    mk_cmd_and_exec(Options, Gtkid, TkW, SCmd, PCmd, DB,dummy).
mk_cmd_and_exec(Options, Gtkid, TkW, SCmd, PCmd, DB,ExtraArg) ->
    case gtk_generic:make_command(Options,Gtkid,TkW,SCmd,PCmd,DB,ExtraArg) of
	{error,Reason} -> {error,Reason};
	Cmd when list(Cmd) ->
	    gtk:exec(Cmd)
    end.

%%----------------------------------------------------------------------
%% SCmd: SimplePreCommand - prepended to simple (s) options
%% PCmd: PlacePreCommand - prepended to placer (p) options
%%       (should start with ';' (at least if preceeded with simple cmds))
%% Comment: If some function changes the gtkid,
%%          it's responsible for storing it in the DB.
%%----------------------------------------------------------------------
make_command(Opts,Gtkid,Scmd,DB) ->
    TkW = Gtkid#gtkid.widget,
    make_command(Opts,Gtkid,TkW,Scmd,[";place ", TkW],DB,dummy).
make_command(Opts,Gtkid,Scmd,Pcmd,DB) ->
    make_command(Opts,Gtkid,Gtkid#gtkid.widget,Scmd,Pcmd,DB,dummy).
make_command(Options, Gtkid, TkW, SCmd, PCmd, DB) ->
    make_command(Options, Gtkid, TkW, SCmd, PCmd, DB,dummy).
make_command(Options, Gtkid, TkW, SCmd, PCmd, DB,ExtraArg) ->
    case out_opts(Options, Gtkid, TkW, DB, ExtraArg, [], [], []) of
	{[], [], []} -> [];
	{Si, [], []} -> [SCmd, Si,$;];
	{[], Pl, []} -> [PCmd, Pl,$;];
	{[], [], Co} -> [$;,Co];
	{[], Pl, Co} -> [PCmd, Pl, $;, Co];
	{Si, [], Co} -> [SCmd, Si, $;, Co];
	{Si, Pl, []} -> [SCmd, Si, PCmd, Pl, $;];
	{Si, Pl, Co} -> [SCmd, Si, PCmd, Pl, $;, Co];
	{error,Reason} -> {error,Reason}
    end.

read_option(DB,Gtkid,Opt) ->
    read_option(DB,Gtkid,Gtkid#gtkid.widget,Opt,dummy).
read_option(DB,Gtkid,Opt,ExtraArg) ->
    read_option(DB,Gtkid,Gtkid#gtkid.widget,Opt,ExtraArg).

%%----------------------------------------------------------------------
%% Args: Args is [Gtkid, TkW, DB, ExtraArg]
%% Comment: An optimization:don't reconstruct the arg list for apply each time.
%%          This is the option-engine so we should optimize.
%%----------------------------------------------------------------------
handle_external_opt_call([Opt|Options],Gtkid,TkW,DB,ExtraArg,ExtRes,S,P,C) ->
    case ExtRes of
	{s, Cmd} ->
	    out_opts(Options,Gtkid, TkW,DB, ExtraArg, [Cmd|S], P, C);
	{p, Cmd} ->
	    out_opts(Options, Gtkid,TkW,DB, ExtraArg, S, [Cmd|P], C);
	{c, Cmd} ->
	    out_opts(Options, Gtkid,TkW,DB, ExtraArg,S, P, [Cmd,$;|C]);
	none ->
	    out_opts(Options, Gtkid,TkW,DB,ExtraArg, S, P, C);
%	{s, NGtkid, Cmd} ->
%	    out_opts(Options,NGtkid,TkW,DB,ExtraArg, [Cmd|S], P, C);
%	{p, NGtkid, Cmd} ->
%	    out_opts(Options,NGtkid,TkW,DB,ExtraArg, S, [Cmd|P], C);
	{c, NGtkid, Cmd} ->
	    out_opts(Options,NGtkid,TkW,DB, ExtraArg,S,P,[Cmd,$;|C]);
	{none, NGtkid} ->
	    out_opts(Options,NGtkid,TkW,DB, ExtraArg, S, P, C);
	{sp,{Scmd,Pcmd}} ->
	    out_opts(Options,Gtkid,TkW,DB,ExtraArg,[Scmd|S],[Pcmd|P],C);
	invalid_option ->
	    {error,{invalid_option,Gtkid#gtkid.objtype,Opt}};
	break -> % a hack. it is possible to abort generic option handling at
	    %% any time (without even inserting the gtkid inte to DB (for
	    %% performance reasons)).
	    {S, P, C}
    end.

handle_external_read(Res) ->
    case Res of 
	{bad_result,{Objtype,Reason,Option}} ->
	    {error,{Objtype,Reason,Option}};
	_ -> ok
    end,
    Res.

%%----------------------------------------------------------------------
%% Generic options
%%----------------------------------------------------------------------

gen_anchor(How,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,[" -anc ", gtk:to_ascii(How)|P],C).
gen_anchor(Opt,Gtkid,TkW,DB,ExtraArg) ->
	tcl2erl:ret_place(anchor, TkW).

gen_height(Height,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    gtk_db:insert_opt(DB,Gtkid,{height,Height}),
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,
	 [" -he ", gtk:to_ascii(Height)|P],C).
gen_height(Opt,Gtkid,TkW,DB,ExtraArg) ->
    gtk_db:opt(DB,Gtkid,height).

gen_width(Width,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    gtk_db:insert_opt(DB,Gtkid,{width,Width}),
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,
	 [" -wi ", gtk:to_ascii(Width)|P],C).
gen_width(Opt,Gtkid,TkW,DB,ExtraArg) ->
    gtk_db:opt(DB,Gtkid,width).

gen_x(X,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    gtk_db:insert_opt(DB,Gtkid,{x,X}),
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,
	 [" -x ", gtk:to_ascii(X)|P],C).
gen_x(Opt,Gtkid,TkW,DB,ExtraArg) ->
    gtk_db:opt(DB,Gtkid,x).

gen_y(Y,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    gtk_db:insert_opt(DB,Gtkid,{y,Y}),
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,
	 [" -y ", gtk:to_ascii(Y)|P],C).
gen_y(Opt,Gtkid,TkW,DB,ExtraArg) ->
    gtk_db:opt(DB,Gtkid,y).

gen_raise(_,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,["raise ", TkW,$;|C]).
gen_raise(Opt,Gtkid,TkW,DB,ExtraArg) ->
    undefined.

gen_lower(_,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,["lower ", TkW,$;|C]).
gen_lower(Opt,Gtkid,TkW,DB,ExtraArg) ->
    undefined.

gen_enable(true,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,[" -st normal"|S],P,C);
gen_enable(false,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,[" -st disabled"|S],P,C).
gen_enable(Opt,Gtkid,TkW,DB,ExtraArg) ->
    tcl2erl:ret_enable([TkW, " cg -st"]).

gen_align(How,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
   out_opts(Opts,Gtkid,TkW,DB,ExtraArg,[" -an ", gtk:to_ascii(How)|S],P,C).
gen_align(Opt,Gtkid,TkW,DB,ExtraArg) ->
    tcl2erl:ret_atom([TkW, " cg -anch"]).

gen_justify(How,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
   out_opts(Opts,Gtkid,TkW,DB,ExtraArg,[" -ju ", gtk:to_ascii(How)|S],P,C).
gen_justify(Opt,Gtkid,TkW,DB,ExtraArg) ->
    tcl2erl:ret_atom([TkW, " cg -ju"]).

gen_padx(Pad,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
   out_opts(Opts,Gtkid,TkW,DB,ExtraArg,[" -padx ", gtk:to_ascii(Pad)|S],P,C).
gen_padx(Opt,Gtkid,TkW,DB,ExtraArg) ->
    tcl2erl:ret_atom([TkW, " cg -padx"]).

gen_pady(Pad,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
   out_opts(Opts,Gtkid,TkW,DB,ExtraArg,[" -pady ", gtk:to_ascii(Pad)|S],P,C).
gen_pady(Opt,Gtkid,TkW,DB,ExtraArg) ->
    tcl2erl:ret_atom([TkW, " cg -pady"]).


gen_font(Font,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    gtk_db:insert_opt(DB,Gtkid,{font,Font}),
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,
	 [" -font ", gtk_font:choose_ascii(DB,Font)|S],P,C).
gen_font(Opt,Gtkid,TkW,DB,ExtraArg) ->
    gtk_db:opt(DB,Gtkid,font,undefined).

gen_label({text,Text},Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,[" -text ", gtk:to_ascii(Text), " -bi {}"|S],P,C);
gen_label({image,Img},Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    {ok, I2,_} = regexp:gsub(Img, [92,92], "/"),
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,[" -bi \"@", I2, "\" -text {}"|S],P,C).
gen_label(Opt,Gtkid,TkW,DB,ExtraArg) ->
    case gtk:call([TkW, " cg -bit"]) of
	{result, [$@|Image]} -> {image,Image};
	Nope ->
	    case gtk:call([TkW, " cg -text"]) of
		{result, Txt} -> {text, Txt};
		Bad_Result -> Bad_Result
	    end
    end.

gen_activebg(Color,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
   out_opts(Opts,Gtkid,TkW,DB,ExtraArg,[" -activeba ", gtk:to_color(Color)|S],P,C).
gen_activebg(Opt,Gtkid,TkW,DB,ExtraArg) ->
    tcl2erl:ret_color([TkW, " cg -activeba"]).

gen_activefg(Color,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
   out_opts(Opts,Gtkid,TkW,DB,ExtraArg,[" -activef ", gtk:to_color(Color)|S],P,C).
gen_activefg(Opt,Gtkid,TkW,DB,ExtraArg) ->
    tcl2erl:ret_color([TkW, " cg -activef"]).


gen_default(Opt,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    case Opt of
	{all, {font, Font}} ->
	    C2 = ["option a *",tl(TkW), % have to remove preceeding dot
		  "*font ",gtk_font:choose_ascii(DB, Font)],
	    gtk_db:insert_def(Gtkid,grid,{font,Font}),
	    gtk_db:insert_def(Gtkid,text,{font,Font}),
	    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,[C2,$;|C]);
	{buttons, {font, Font}} ->
	    C2 = ["option a *",tl(TkW), % have to remove preceeding dot
		  ".Button.font ",gtk_font:choose_ascii(DB, Font)],
	    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,[C2,$;|C]);
	{buttons,{Key,Val}} ->
	    gtk_db:insert_def(Gtkid,button,{Key,Val}),
	    gtk_db:insert_def(Gtkid,checkbutton,{Key,Val}),
	    gtk_db:insert_def(Gtkid,radiobutton,{Key,Val}),
	    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,C);
	{ObjType, {Key,Val}} ->
	    gtk_db:insert_def(Gtkid,ObjType,{Key,Val}),
	    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,C)
    end.


gen_relief(Relief,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,[" -reli ",gtk:to_ascii(Relief)|S],P,C).
gen_relief(Opt,Gtkid,TkW,DB,ExtraArg) ->
    tcl2erl:ret_atom([TkW, " cg -reli"]).

gen_bw(Wth,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
   out_opts(Opts,Gtkid,TkW,DB,ExtraArg,[" -bd ", gtk:to_ascii(Wth)|S],P,C).
gen_bw(Opt,Gtkid,TkW,DB,ExtraArg) ->
    tcl2erl:ret_int([TkW, " cg -bd"]).



gen_font_wh({font_wh,{Font, Txt}},Gtkid,TkW,DB,_) ->
    gtk_font:width_height(DB, gtk_font:choose(DB,Font), Txt).

gen_choose_font({choose_font,Font},Gtkid,TkW,DB,ExtraArg) ->
    gtk_font:choose(DB,Font).

gen_data(Data,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    gtk_db:insert_opt(DB,Gtkid,{data,Data}),
   out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,C).
gen_data(Opt,Gtkid,TkW,DB,ExtraArg) ->
    gtk_db:opt(DB,Gtkid,data).

gen_pack_x({Start,Stop},Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    gtk_db:insert_opt(DB,Gtkid,{pack_x,{Start,Stop}}),
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,C);
gen_pack_x(Col,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) when integer(Col) ->
    gtk_db:insert_opt(DB,Gtkid,{pack_x,{Col,Col}}),
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,C).
gen_pack_x(Opt,Gtkid,TkW,DB,ExtraArg) ->
    gtk_db:opt(DB,Gtkid,pack_x, undefined).

gen_pack_y({Start,Stop},Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    gtk_db:insert_opt(DB,Gtkid,{pack_y,{Start,Stop}}),
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,C);
gen_pack_y(Row,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) when integer(Row) ->
    gtk_db:insert_opt(DB,Gtkid,{pack_y,{Row,Row}}),
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,C).
gen_pack_y(Opt,Gtkid,TkW,DB,ExtraArg) ->
    gtk_db:opt(DB,Gtkid,pack_y, undefined).

gen_pack_xy({Col,Row},Opts,Gtkid,TkW,DB,ExtraArg,S,P,C)
  when integer(Col), integer(Row) ->
    gtk_db:insert_opt(DB,Gtkid,{pack_x,{Col,Col}}),
    gtk_db:insert_opt(DB,Gtkid,{pack_y,{Row,Row}}),
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,C);
gen_pack_xy({Col,{StartRow,StopRow}},Opts,Gtkid,TkW,DB,ExtraArg,S,P,C)
  when integer(Col) ->
    gtk_db:insert_opt(DB,Gtkid,{pack_x,{Col,Col}}),
    gtk_db:insert_opt(DB,Gtkid,{pack_y,{StartRow,StopRow}}),
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,C);
gen_pack_xy({{StartCol,StopCol},Row},Opts,Gtkid,TkW,DB,ExtraArg,S,P,C)
  when integer(Row) ->
    gtk_db:insert_opt(DB,Gtkid,{pack_x,{StartCol,StopCol}}),
    gtk_db:insert_opt(DB,Gtkid,{pack_y,{Row,Row}}),
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,C);
gen_pack_xy({Col,Row},Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    gtk_db:insert_opt(DB,Gtkid,{pack_x,Col}),
    gtk_db:insert_opt(DB,Gtkid,{pack_y,Row}),
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,C).


gen_flush(Opt,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C)  -> 
    tcl2erl:ret_int(["update idletasks;expr 1+1"]),
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,C).
gen_flush(Opt,Gtkid,TkW,DB,ExtraArg) ->
    tcl2erl:ret_int(["update idletasks;expr 1+1"]).

% a hidden impl option.
gen_keep_opt(Opt,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C)  -> 
    gtk_db:insert_opt(DB,Gtkid,Opt),
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,C).

gen_children(Opt,Gtkid,TkW,DB,ExtraArg) ->
    make_extern_id(gtk_db:lookup_kids(DB, Gtkid#gtkid.id), DB).

make_extern_id([Id|Ids], DB) ->
    [gtk:make_extern_id(Id, DB) | make_extern_id(Ids, DB)];
make_extern_id([], _) -> [].

gen_id(Opt,#gtkid{id=Id},TkW,DB,ExtraArg) ->
    gtk:make_extern_id(Id, DB).

gen_parent(Opt,Gtkid,TkW,DB,ExtraArg) ->
    gtk:make_extern_id(Gtkid#gtkid.parent, DB).

gen_type(Opt,Gtkid,TkW,DB,ExtraArg) ->
    Gtkid#gtkid.objtype.

gen_beep(_,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
   out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,["bell;",$;|C]).

gen_setfocus(true,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,["focus ", TkW,$;|C]);
gen_setfocus(false,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,["focus .",$;|C]).

gen_setfocus(Opt,Gtkid,TkW,DB,ExtraArg) ->
    tcl2erl:ret_focus(TkW, "focus").

gen_buttonpress(On,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    Cmd = bind(DB, Gtkid, TkW, buttonpress, On),
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,[Cmd,$;|C]).
gen_buttonpress(Opt,Gtkid,TkW,DB,ExtraArg) ->
    gtk_db:is_inserted(DB, Gtkid, buttonpress).

gen_buttonrelease(On,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    Cmd = bind(DB, Gtkid, TkW, buttonrelease, On),
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,[Cmd,$;|C]).
gen_buttonrelease(Opt,Gtkid,TkW,DB,ExtraArg) ->
    gtk_db:is_inserted(DB,Gtkid,buttonrelease).

gen_configure(On,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    Cmd = bind(DB, Gtkid, TkW, configure, On),
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,[Cmd,$;|C]).
gen_configure(Opt,Gtkid,TkW,DB,ExtraArg) ->
    gtk_db:is_inserted(DB,Gtkid,configure).

gen_destroy(On,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    Cmd = bind(DB, Gtkid, TkW, destroy, On),
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,[Cmd,$;|C]).
gen_destroy(Opt,Gtkid,TkW,DB,ExtraArg) ->
    gtk_db:is_inserted(DB,Gtkid,destroy).

gen_enter(On,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    Cmd = bind(DB, Gtkid, TkW, enter, On),
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,[Cmd,$;|C]).
gen_enter(Opt,Gtkid,TkW,DB,ExtraArg) ->
    gtk_db:is_inserted(DB,Gtkid,enter).

gen_focus_ev(On,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    Cmd = bind(DB, Gtkid, TkW, focus, On),
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,[Cmd,$;|C]).
gen_focus_ev(Opt,Gtkid,TkW,DB,ExtraArg) ->
    gtk_db:is_inserted(DB,Gtkid,focus).

gen_keypress(On,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    Cmd = bind(DB, Gtkid, TkW, keypress, On),
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,[Cmd,$;|C]).
gen_keypress(Opt,Gtkid,TkW,DB,ExtraArg) ->
    gtk_db:is_inserted(DB,Gtkid,keypress).

gen_keyrelease(On,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    Cmd = bind(DB, Gtkid, TkW, keyrelease, On),
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,[Cmd,$;|C]).
gen_keyrelease(Opt,Gtkid,TkW,DB,ExtraArg) ->
    gtk_db:is_inserted(DB,Gtkid,keyrelease).

gen_leave(On,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    Cmd = bind(DB, Gtkid, TkW, leave, On),
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,[Cmd,$;|C]).
gen_leave(Opt,Gtkid,TkW,DB,ExtraArg) ->
    gtk_db:is_inserted(DB,Gtkid,leave).

gen_motion(On,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    Cmd = bind(DB, Gtkid, TkW, motion, On),
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,[Cmd,$;|C]).
gen_motion(Opt,Gtkid,TkW,DB,ExtraArg) ->
    gtk_db:is_inserted(DB,Gtkid,motion).

gen_highlightbw(Wth,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
   out_opts(Opts,Gtkid,TkW,DB,ExtraArg,[" -highlightt ", gtk:to_ascii(Wth)|S],P,C).
gen_highlightbw(Opt,Gtkid,TkW,DB,ExtraArg) ->
    tcl2erl:ret_int([TkW, " cg -highlightt"]).

gen_highlightbg(Color,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
   out_opts(Opts,Gtkid,TkW,DB,ExtraArg,[" -highlightb ", gtk:to_color(Color)|S],P,C).
gen_highlightbg(Opt,Gtkid,TkW,DB,ExtraArg) ->
    tcl2erl:ret_color([TkW, " cg -highlightb"]).

gen_highlightfg(Color,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
   out_opts(Opts,Gtkid,TkW,DB,ExtraArg,[" -highlightc ", gtk:to_color(Color)|S],P,C).
gen_highlightfg(Opt,Gtkid,TkW,DB,ExtraArg) ->
    tcl2erl:ret_color([TkW,  " cg -highlightc"]).


gen_selectbw(Width,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,[TkW, " conf -selectbo ", gtk:to_ascii(Width),$;|C]).
gen_selectbw(Opt,Gtkid,TkW,DB,ExtraArg) ->
    tcl2erl:ret_int([TkW," cg -selectbo"]).

gen_selectfg(Color,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,[TkW, " conf -selectfo ", gtk:to_color(Color),$;|C]).
gen_selectfg(Opt,Gtkid,TkW,DB,ExtraArg) ->
    tcl2erl:ret_color([TkW," cg -selectfo"]).

gen_selectbg(Color,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,[TkW, " conf -selectba ", gtk:to_color(Color),$;|C]).
gen_selectbg(Opt,Gtkid,TkW,DB,ExtraArg) ->
    tcl2erl:ret_color([TkW," cg -selectba"]).

gen_fg(Color,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
   out_opts(Opts,Gtkid,TkW,DB,ExtraArg,[" -fg ", gtk:to_color(Color)|S],P,C).
gen_fg(Opt,Gtkid,TkW,DB,ExtraArg) ->
    tcl2erl:ret_color([TkW, " cg -fg"]).

gen_bg(Color,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
   out_opts(Opts,Gtkid,TkW,DB,ExtraArg,[" -bg ", gtk:to_color(Color)|S],P,C).
gen_bg(Opt,Gtkid,TkW,DB,ExtraArg) ->
    tcl2erl:ret_color([TkW, " cg -bg"]).

%%----------------------------------------------------------------------
%% Generic functions for scrolled objects
%%----------------------------------------------------------------------
gen_so_activebg(Color,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    Col = gtk:to_color(Color),
    C2 = [TkW, ".sy conf -activeba ", Col,$;,
	  TkW, ".pad.sx conf -activeba ", Col],
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,[C2,$;|C]).
gen_so_activebg(Opt,Gtkid,TkW,DB,ExtraArg) ->
    tcl2erl:ret_color([TkW,".sy cg -activeba"]).

gen_so_bc(Color,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    Col = gtk:to_color(Color),
    C2= [TkW, " conf -bg ", Col,$;,
	 TkW, ".sy conf -highlightba ", Col,$;,
	 TkW, ".pad.it conf -bg ", Col,$;,
	 TkW, ".pad.sx conf -highlightba ", Col],
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,[C2,$;|C]).
gen_so_bc(Opt,Gtkid,TkW,DB,ExtraArg) ->
    tcl2erl:ret_color([TkW," cg -bg"]).

gen_so_scrollfg(Color,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    Col = gtk:to_color(Color),
    C2=[TkW, ".sy conf -bg ", Col,$;,
	TkW, ".pad.sx conf -bg ", Col],
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,[C2,$;|C]).
gen_so_scrollfg(Opt,Gtkid,TkW,DB,ExtraArg) ->
    tcl2erl:ret_color([TkW,".sy cg -bg"]).


gen_so_scrollbg(Color,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    Col = gtk:to_color(Color),
    C2 = [TkW, ".sy conf -troughc ", Col, $;,
	  TkW, ".pad.sx conf -troughc ", Col],
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,[C2,$;|C]).

gen_so_scrollbg(Opt,Gtkid,TkW,DB,ExtraArg) ->
    tcl2erl:ret_color([TkW,".sy cg -troughc"]).

obj(#gtkid{widget_data=SO}) ->
    SO#so.object.

gen_so_bg(Color,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    C2= [obj(Gtkid), " conf -bg ", gtk:to_color(Color)],
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,[C2,$;|C]).
gen_so_bg(Opt,Gtkid,TkW,DB,ExtraArg) ->
    tcl2erl:ret_color([obj(Gtkid)," cg -bg"]).

gen_so_selectbw(Width,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    C2 = [obj(Gtkid), " conf -selectbo ", gtk:to_ascii(Width)],
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,[C2,$;|C]).
gen_so_selectbw(Opt,Gtkid,TkW,DB,ExtraArg) ->
    tcl2erl:ret_int([obj(Gtkid)," cg -selectbo"]).

gen_so_selectfg(Color,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    C2 = [obj(Gtkid), " conf -selectfo ", gtk:to_color(Color)],
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,[C2,$;|C]).
gen_so_selectfg(Opt,Gtkid,TkW,DB,ExtraArg) ->
    tcl2erl:ret_color([obj(Gtkid)," cg -selectfo"]).

gen_so_selectbg(Color,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    C2 = [obj(Gtkid), " conf -selectba ", gtk:to_color(Color)],
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,S,P,[C2,$;|C]).
gen_so_selectbg(Opt,Gtkid,TkW,DB,ExtraArg) ->
    tcl2erl:ret_color([obj(Gtkid)," cg -selectba"]).

gen_so_scrolls({Vscroll, Hscroll},Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    SO = Gtkid#gtkid.widget_data,
    NewSO = SO#so{hscroll=Hscroll, vscroll=Vscroll},
    C2 = scrolls_vh(TkW, Vscroll, Hscroll),
    Ngtkid = Gtkid#gtkid{widget_data=NewSO},
    gtk_db:update_widget(DB,Ngtkid),
    out_opts(Opts,Ngtkid,TkW,DB,ExtraArg,S,P,[C2,$;|C]).

% read-only
gen_so_hscroll(Opt,#gtkid{widget_data=SO},TkW,DB,_) ->
    SO#so.hscroll.

% read-only
gen_so_vscroll(Opt,#gtkid{widget_data=SO},TkW,DB,_) ->
    SO#so.vscroll.

cursors() -> [{arrow,"top_left_arrow"},{busy,"watch"},{cross,"X_cursor"},
	      {hand,"hand2"},{help,"question_arrow"},{resize,"fleur"},
	      {text,"xterm"}].

gen_cursor(parent,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    out_opts(Opts,Gtkid,TkW,DB,ExtraArg,[" -cur {}"|S],P,C);
gen_cursor(Cur,Opts,Gtkid,TkW,DB,ExtraArg,S,P,C) ->
    case gs:assq(Cur,cursors()) of
	{value, TxtCur} ->
	        out_opts(Opts,Gtkid,TkW,DB,ExtraArg,[" -cur ",TxtCur|S],P,C);
	Q ->
	    {error,{invalid_cursor,Gtkid#gtkid.objtype,Cur}}
    end.
gen_cursor(Opt,Gtkid,TkW,DB,ExtraArg) ->
    case tcl2erl:ret_str([TkW," cg -cur"]) of
	"" -> parent;
	Txt when list(Txt) ->
	    case lists:keysearch(Txt,2,cursors()) of
		{value,{Cur,_}} -> Cur;
		_ -> {bad_result, read_cursor}
	    end;
	Bad_Result -> Bad_Result
    end.

gen_citem_coords(Coords,Opts,Gtkid,TkW,DB,AItem,S,P,C) ->
    gtk_db:insert_opt(DB,Gtkid,{coords,Coords}),
    out_opts(Opts,Gtkid,TkW,DB,AItem,S,P,
	     [TkW, " coords ", AItem," ",gtk_canvas:coords(Coords),$;|C]).
gen_citem_coords(Opt,Gtkid,TkW,DB,ExtraArg) ->
    gtk_db:opt(DB,Gtkid, coords).

gen_citem_fill(none,Opts,Gtkid,TkW,DB,AItem,S,P,C) ->
    out_opts(Opts,Gtkid,TkW,DB,AItem,[" -f {}"|S],P,C);
gen_citem_fill(Color,Opts,Gtkid,TkW,DB,AItem,S,P,C) ->
    out_opts(Opts,Gtkid,TkW,DB,AItem,[" -f ",gtk:to_color(Color)|S],P,C).
gen_citem_fill(Opt,Gtkid,TkW,DB,AItem) ->
    tcl2erl:ret_color([TkW, " itemcg ", AItem, " -f"]).

gen_citem_lower(_,Opts,Gtkid,TkW,DB,AItem,S,P,C) ->
    out_opts(Opts,Gtkid,TkW,DB,AItem,S,P,
	     [TkW, " lower ", AItem,$;|C]).

gen_citem_raise(_,Opts,Gtkid,TkW,DB,AItem,S,P,C) ->
    out_opts(Opts,Gtkid,TkW,DB,AItem,S,P,
	     [TkW, " raise ", AItem,$;|C]).

gen_citem_move({Dx,Dy},Opts,Gtkid,TkW,DB,AItem,S,P,C) ->
    NewCoords = move_coords(Dx,Dy,gtk_db:opt(DB,Gtkid,coords)),
    gtk_db:insert_opt(DB,Gtkid,NewCoords),
    out_opts(Opts,Gtkid,TkW,DB,AItem,S,P,
	     [TkW, " move ", AItem, " ",
	      gtk:to_ascii(Dx), " ", gtk:to_ascii(Dy),$;|C]).

move_coords(Dx,Dy,Coords) ->
    Coords2 = add_to_coords(Dx,Dy, Coords),
    {coords,Coords2}.

add_to_coords(Dx,Dy,[{X,Y}|Coords]) ->
    [{X+Dx,Y+Dy}|add_to_coords(Dx,Dy,Coords)];
add_to_coords(_,_,[]) -> [].


gen_citem_setfocus(true,Opts,Gtkid,TkW,DB,AItem,S,P,C) ->
    out_opts(Opts,Gtkid,TkW,DB,AItem,S,P,
	     [TkW, " focus ", AItem,$;|C]);
gen_citem_setfocus(false,Opts,Gtkid,TkW,DB,AItem,S,P,C) ->
    out_opts(Opts,Gtkid,TkW,DB,AItem,S,P,
	     [TkW, " focus {}",$;|C]).
gen_citem_setfocus(Opt,Gtkid,TkW,DB,ExtraArg) ->
    tcl2erl:ret_focus(gtk:to_ascii(bug_aitem),[TkW, " focus"]).

gen_citem_buttonpress(On,Opts,Gtkid,TkW,DB,AItem,S,P,C) ->
    out_opts(Opts,Gtkid,TkW,DB,AItem,S,P,
	     [item_bind(DB, Gtkid, TkW, AItem,buttonpress, On),$;|C]).
gen_citem_buttonrelease(On,Opts,Gtkid,TkW,DB,AItem,S,P,C) ->
    out_opts(Opts,Gtkid,TkW,DB,AItem,S,P,
	     [item_bind(DB,Gtkid,TkW,AItem,buttonrelease, On),$;|C]).
gen_citem_enter(On,Opts,Gtkid,TkW,DB,AItem,S,P,C) ->
    out_opts(Opts,Gtkid,TkW,DB,AItem,S,P,
	     [item_bind(DB, Gtkid, TkW, AItem, enter, On),$;|C]).

gen_citem_keypress(On,Opts,Gtkid,TkW,DB,AItem,S,P,C) ->
    out_opts(Opts,Gtkid,TkW,DB,AItem,S,P,
	     [item_bind(DB, Gtkid, TkW, AItem, keypress, On),$;|C]).
gen_citem_keyrelease(On,Opts,Gtkid,TkW,DB,AItem,S,P,C) ->
    out_opts(Opts,Gtkid,TkW,DB,AItem,S,P,
	     [item_bind(DB, Gtkid, TkW, AItem, keyrelease, On),$;|C]).

gen_citem_leave(On,Opts,Gtkid,TkW,DB,AItem,S,P,C) ->
    out_opts(Opts,Gtkid,TkW,DB,AItem,S,P,
	     [item_bind(DB, Gtkid, TkW, AItem, leave, On),$;|C]).
gen_citem_motion(On,Opts,Gtkid,TkW,DB,AItem,S,P,C) ->
    out_opts(Opts,Gtkid,TkW,DB,AItem,S,P,
	     [item_bind(DB, Gtkid, TkW, AItem, motion, On),$;|C]).


scrolls_vh(W, V,       true) -> scrolls_vh(W, V, bottom);
scrolls_vh(W, true,       H) -> scrolls_vh(W, left, H);
scrolls_vh(W, left,  bottom) -> ["so_bottom_left ",W];
scrolls_vh(W, left,     top) -> ["so_top_left ",W];
scrolls_vh(W, left,       _) -> ["so_left ",W];
scrolls_vh(W, right, bottom) -> ["so_bottom_right ",W];
scrolls_vh(W, right,    top) -> ["so_top_right ",W];
scrolls_vh(W, right,      _) -> ["so_right ",W];
scrolls_vh(W, _,     bottom) -> ["so_bottom ",W];
scrolls_vh(W, _,        top) -> ["so_top ",W];
scrolls_vh(W, _,          _) -> ["so_plain ",W].

%% create version
parse_scrolls(Opts) ->
    {Vscroll, Hscroll, NewOpts} = parse_scrolls(Opts, false, false, []),
    {Vscroll, Hscroll, [{scrolls, {Vscroll, Hscroll}} | NewOpts]}.

%% config version
parse_scrolls(Gtkid, Opts) ->
    SO = Gtkid#gtkid.widget_data,
    Vscroll = SO#so.vscroll,
    Hscroll = SO#so.hscroll,
    case parse_scrolls(Opts, Vscroll, Hscroll, []) of
	{Vscroll, Hscroll, Opts} -> Opts;
	{NewVscroll, NewHscroll, NewOpts} -> 
	    [{scrolls, {NewVscroll, NewHscroll}} | NewOpts]
    end.
	      

parse_scrolls([Option | Rest], Vscroll, Hscroll, Opts) when tuple(Option) ->
    case element(1, Option) of
	vscroll ->
	    parse_scrolls(Rest, element(2, Option), Hscroll, Opts);
	hscroll ->
	    parse_scrolls(Rest, Vscroll, element(2, Option), Opts);
	_ ->
	    parse_scrolls(Rest, Vscroll, Hscroll, [Option | Opts])
    end;

parse_scrolls([Option | Rest], Vscroll, Hscroll, Opts) ->
    parse_scrolls(Rest, Vscroll, Hscroll, [Option | Opts]);

parse_scrolls([], Vscroll, Hscroll, Opts) ->
    {Vscroll, Hscroll, Opts}.


%%
%% Event bind main function
%%
%% Should return a list of tcl commands or invalid_option
%%
%% WS = Widget suffix for complex widgets
%%
bind(DB, Gtkid, TkW, Etype, On) ->
    WD = Gtkid#gtkid.widget_data,
    TkW2 = if record(WD, so) ->
		  WD#so.object;
	     true -> TkW
	  end,
    case bind(DB, Gtkid, TkW2, Etype, On, "") of
	invalid_option -> invalid_option;
	Cmd ->
	    Cmd
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
	      motion -> [P, " <Motion> {erlsend ", Eref, " %x %y}"];
	      keypress ->
		  [P, " <KeyPress> {erlsend ", Eref," %K %N 0 0};",
		   P, " <Shift-KeyPress> {erlsend ", Eref, " %K %N 1 0};",
		   P, " <Control-KeyPress> {erlsend ", Eref, " %K %N 0 1};",
		  P," <Control-Shift-KeyPress> {erlsend ", Eref," %K %N 1 1}"];
	      keyrelease ->
		  [P, " <KeyRelease> {erlsend ", Eref," %K %N 0 0};",
		   P, " <Shift-KeyRelease> {erlsend ", Eref, " %K %N 1 0};",
		   P, " <Control-KeyRelease> {erlsend ", Eref, " %K %N 0 1};",
		 P," <Control-Shift-KeyRelease> {erlsend ",Eref," %K %N 1 1}"];
	      buttonpress ->
		  [P, " <ButtonPress> {erlsend ", Eref, " %b %x %y}"];
	      buttonrelease ->
		  [P, " <ButtonRelease> {erlsend ", Eref, " %b %x %y}"];
	      leave -> [P, " <Leave> {erlsend ", Eref, "}"];
	      enter -> [P, " <Enter> {erlsend ", Eref, "}"];
	      destroy ->
		  [P, " <Destroy> {if {\"%W\"==\"", [TkW, WS], 
		   "\"} {erlsend ", Eref, "}}"];
	      focus ->
		  [P, " <FocusIn> {erlsend ", Eref, " 1};" ,
		   P, " <FocusOut> {erlsend ", Eref, " 0}"];
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
		   [P, " <KeyPress> {};",
		    P, " <Shift-KeyPress> {};",
		    P, " <Control-KeyPress> {};",
		    P, " <Control-Shift-KeyPress> {}"];
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


%%
%% Event item bind main function
%%
%% Should return a list of tcl commands or invalid_option
%%
item_bind(DB, Gtkid, Canvas, Item, Etype, On) ->
    case On of
	true          -> item_ebind(DB, Gtkid, Canvas, Item, Etype, "");
	{true, Edata} -> item_ebind(DB, Gtkid, Canvas, Item, Etype, Edata);
	Other         -> item_eunbind(DB, Gtkid, Canvas, Item, Etype)
    end.

%%
%% Event bind on
%%
%% Should return a list of tcl commands or invalid_option
%%
item_ebind(DB, Gtkid, Canvas, Item, Etype, Edata) ->
    Eref = gtk_db:insert_event(DB, Gtkid, Etype, Edata),
    P = [Canvas, " bind ", Item],
    case Etype of
	enter  -> [P, " <Enter> {erlsend ", Eref, "}"];
	leave  -> [P, " <Leave> {erlsend ", Eref, "}"];
	motion -> [P, " <Motion> {erlsend ", Eref, " [",
		   Canvas, " canvasx %x] [", Canvas, " canvasy %y] %x %y}"];
	keypress ->
	    [P, " <Key> {erlsend ", Eref," %K %N 0 0 [",
	     Canvas, " canvasx %x] [", Canvas, " canvasy %y]};",
	     P, " <Shift-Key> {erlsend ", Eref, " %K %N 1 0 [",
	     Canvas, " canvasx %x] [", Canvas, " canvasy %y]};",
	     P, " <Control-Key> {erlsend ", Eref, " %K %N 0 1 [",
	     Canvas, " canvasx %x] [", Canvas, " canvasy %y]};",
	     P, " <Control-Shift-Key> {erlsend ", Eref," %K %N 1 1 [",
	     Canvas, " canvasx %x] [", Canvas, " canvasy %y]}"];
	keyrelease ->
	    [P, " <KeyRelease> {erlsend ", Eref," %K %N 0 0 [",
	     Canvas, " canvasx %x] [", Canvas, " canvasy %y]};",
	     P, " <Shift-KeyRelease> {erlsend ", Eref, " %K %N 1 0 [",
	     Canvas, " canvasx %x] [", Canvas, " canvasy %y]};",
	     P, " <Control-KeyRelease> {erlsend ", Eref, " %K %N 0 1 [",
	     Canvas, " canvasx %x] [", Canvas, " canvasy %y]};",
	     P, " <Control-Shift-KeyRelease> {erlsend ", Eref," %K %N 1 1[",
	     Canvas, " canvasx %x] [", Canvas, " canvasy %y]}"];
	buttonpress ->
	    [P, " <Button> {erlsend ", Eref, " %b [",
	     Canvas, " canvasx %x] [", Canvas, " canvasy %y] %x %y}"];
	buttonrelease ->
	    [P, " <ButtonRelease> {erlsend ", Eref, " %b [",
	     Canvas, " canvasx %x] [", Canvas, " canvasy %y] %x %y}"]
    end.
		  

%%
%% Unbind event
%%
%% Should return a list of tcl commands
%% Already checked for validation in bind/5
%%
item_eunbind(DB, Gtkid, Canvas, Item, Etype) ->
    gtk_db:delete_event(DB, Gtkid, Etype),
    P = [Canvas, " bind ", Item],
    Cmd = case Etype of
	      enter         -> [P, " <Enter> {}"];
	      leave         -> [P, " <Leave> {}"];
	      motion        -> [P, " <Motion> {}"];
	      keypress -> 
		   [P, " <KeyPress> {};",
		    P, " <Shift-KeyPress> {};",
		    P, " <Control-KeyPress> {};",
		    P, " <Control-Shift-KeyPress> {}"];
	      keyrelease -> 
		   [P, " <KeyRelease> {};",
		    P, " <Shift-KeyRelease> {};",
		    P, " <Control-KeyRelease> {};",
		    P, " <Control-Shift-KeyRelease> {}"];
	      buttonpress   -> [P, " <Button> {}"];
	      buttonrelease -> [P, " <ButtonRelease> {}"]
	  end,
    Cmd.



event(DB, Gtkid, Etype, _Edata, Args) ->
    #gtkid{owner=Ow,id=Id} = Gtkid,
    Data = gtk_db:opt(DB,Gtkid,data),
    gs_frontend:event(get(gs_frontend),Ow,{gs,Id,Etype,Data,Args}).
