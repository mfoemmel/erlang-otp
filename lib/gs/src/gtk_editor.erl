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
%% Basic Editor Type
%% ------------------------------------------------------------

-module(gtk_editor).

%%------------------------------------------------------------------------------
%% 			    CANVAS OPTIONS
%%
%%  Attributes:
%%	activebg		Color
%%	anchor			n,w,s,e,nw,se,ne,sw,center
%%	bc			Color
%%	bg			Color
%%	bw			Wth
%%	data			Data
%%	fg			Color
%%      font                    Font
%%	height			Int
%%	highlightbg		Color
%%	highlightbw		Wth
%%	highlightfg		Color
%%	hscroll			Bool | top | bottom
%%	insertbg		Color
%%	insertbw		Wth
%%      insertpos               {Row,Col}|'end'  (Row: 1..Max, Col: 0..Max)
%%	justify			left|right|center
%%	padx			Int   (Pixels)
%%	pady			Int   (Pixels)
%%	relief			Relief
%%	scrollbg		Color
%%	scrollfg		Color
%%	selectbg		Color
%%	selectbw		Width
%%	selectfg		Color
%%	vscroll			Bool | left | right
%%	width			Int
%%	wrap			none | char | word
%%	x			Int
%%	y			Int
%%
%%
%%  Commands:
%%	clear
%%	del			{FromIdx, ToIdx} 
%%	enable			Bool
%%	file			String
%%	get			{FromIdx, ToIdx} => Text
%%	insert			{Index, Text}Index = [insert,{Row,lineend},end,{Row,Col}]
%%	setfocus		Bool
%%
%%  Events:
%%	buttonpress		[Bool | {Bool, Data}]
%%	buttonrelease		[Bool | {Bool, Data}]
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

%.t tag names 2.7 -> red blue (blue är färgen)
%.t tag add blue 2.1 2.10    tagga text
%.t tag configure blue -foregr blue skapa tag
% .t index end -> MaxRows.cols
% .t yview moveto (Row-1)/MaxRows

-export([create/3, config/3, read/3, delete/2,event/5,option/5,read_option/5]).

-include("gtk.hrl").

%%-----------------------------------------------------------------------------
%%			MANDATORY INTERFACE FUNCTIONS
%%-----------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: create/3
%% Purpose    	: Create a widget of the type defined in this module.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(DB, Gtkid, Opts) ->
    MainW = gtk_generic:mk_tkw_child(DB,Gtkid),
    Editor = lists:append(MainW,".z"),
    {Vscroll, Hscroll, NewOpts} = gtk_generic:parse_scrolls(Opts),
    WidgetD = #so{main=MainW, object=Editor,
		 hscroll=Hscroll, vscroll=Vscroll,misc=[{1,white}]},
    NGtkid=Gtkid#gtkid{widget=MainW, widget_data=WidgetD},
    gtk_db:insert_widget(DB,NGtkid),
    MandatoryCmd = ["so_create text ", MainW],
    case gtk:call(MandatoryCmd) of
	{result, _} ->
	    SimplePreCmd = [MainW, " conf"],
	    PlacePreCmd = [";place ", MainW],
	    case gtk_generic:make_command(NewOpts, NGtkid, MainW, SimplePreCmd,
					  PlacePreCmd, DB,Editor) of
		{error,Reason} -> {error,Reason};
		Cmd when list(Cmd) ->
		    gtk:exec(Cmd),
		    gtk:exec(
		      [Editor," conf -bo 2 -relief sunken -highlightth 2;",
		       MainW,".sy conf -rel sunken -bo 2;",
		       MainW,".pad.sx conf -rel sunken -bo 2;",
		       Editor, " tag co c1 -for white;"]),
		    ok;
		Bad_Result ->
		    {error, Bad_Result}
	    end
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
    Editor = SO#so.object,
    NewOpts =
	case {gs:assq(vscroll,Options),gs:assq(hscroll,Options)} of
	    {false,false} -> Options;
	    _ -> gtk_generic:parse_scrolls(Gtkid, Options)
	end,
    SimplePreCmd = [MainW, " conf"],
    PlacePreCmd = [";place ", MainW],
    gtk_generic:mk_cmd_and_exec(NewOpts, Gtkid, MainW, SimplePreCmd,
				PlacePreCmd, DB, Editor).


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
%%		  Editor  - The Editor tk-widget
%%		  DB	  - The Database
%%
%% Return 	: A tuple {OptionType, OptionCmd}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
option(Option, Gtkid, MainW,DB, Editor) ->
    case Option of
	{font,Font} when tuple(Font) ->   
	    gtk_db:insert_opt(DB,Gtkid,Option),
	    {c, [Editor, " conf -font ", gtk_font:choose_ascii(DB,Font)]};
	{font_style, {{Start,End},Font}} -> % should be only style
	    {Tag,Ngtkid} = get_style_tag(DB,Editor,Font,Gtkid),
	    gtk_db:update_widget(DB,Ngtkid),
	    {c, Ngtkid, [Editor, " tag ad ", Tag, " ", p_index(Start), " ",
			 p_index(End)]};
	{fg, {{Start,End},Color}} ->
	    {Tag,Ngtkid} = get_color_tag(Editor,Color,Gtkid),
	    gtk_db:update_widget(DB,Ngtkid),
	    {c, Ngtkid, [Editor, " tag ad ", Tag, " ", p_index(Start), " ",
			 p_index(End)]};
	{padx,          Pad} -> {c, [Editor," conf -padx ",gtk:to_ascii(Pad)]};
	{pady,          Pad} -> {c, [Editor," conf -pady ",gtk:to_ascii(Pad)]};
	{selection, {From, To}} ->
	    {c, [Editor," tag ad sel ",p_index(From)," ", p_index(To)]};
	{vscrollpos, Row} ->
	    {MaxRow,_Col} = ret_ed_index([Editor," ind end"]),
	    {c, [Editor, " yv mo ",gtk:to_ascii(Row/MaxRow)]};
	{wrap,          How} ->
	    {c, [Editor, " conf -wrap ", gtk:to_ascii(How)]};
	{fg,          Color} ->
	    {c, [Editor, " conf -fg ", gtk:to_color(Color)]};
	{insertbw,      Wth} ->
	    {c, [Editor, " conf -insertbo ", gtk:to_ascii(Wth)]};
	{insertbg,    Color} ->
	    {c, [Editor, " conf -insertba ", gtk:to_color(Color)]};
	{insertpos,    Index} ->
	    {c, [Editor, " m s insert ", p_index(Index)]};
	{insert, {Index, Text}} ->
	    {c, [Editor, " ins ", p_index(Index), " ", gtk:to_ascii(Text)]};
	{del,      {From, To}} ->
	    {c, [Editor, " del ", p_index(From), " ", p_index(To)]};
	{overwrite, {Index, Text}} ->
	    AI = p_index(Index),
	    Len = gtk:to_ascii(lists:flat_length(Text)),
	    {c, [Editor, " del ",AI," \"",AI,"+",Len,"c\";",
		 Editor, " ins ",AI," ", gtk:to_ascii(Text)]};
	{wrap,            How} ->
	    {c, [Editor, " conf -wrap ", gtk:to_ascii(How)]};
	clear       -> {c, [Editor, " delete 1.0 end"]};
	{load,        File} ->
	    {ok, F2,_} = regexp:gsub(File, [92,92], "/"),
	    case gtk:call(["ed_load ", Editor, " ", gtk:to_ascii(F2)]) of
		{result,    _} -> none;
		{bad_result,Re} -> 
		    {error,{no_such_file,editor,load,F2,Re}}
	    end;
	{save, File} ->
	    {ok, F2,_} = regexp:gsub(File, [92,92], "/"),
	    case gtk:call(["ed_save ",Editor," ",gtk:to_ascii(F2)]) of
		{result,    _} -> none;
		{bad_result,Re} -> 
		    {error,{no_such_file,editor,save,F2,Re}}
	    end;
	{enable,      true} -> {c, [Editor, " conf -sta normal"]};
	{enable,     false} -> {c, [Editor, " conf -sta disabled"]};
	
	{setfocus,     true} -> {c, ["focus ", Editor]};
	{setfocus,    false} -> {c, ["focus ."]};
	_ -> invalid_option
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: read_option/5
%% Purpose    	: Take care of a read option
%% Return 	: The value of the option or invalid_option
%%		  [OptionValue | {bad_result, Reason}]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
read_option(Option,GtkId,MainW,DB,Editor) ->
    case Option of
	font -> gtk_db:opt(DB,GtkId,font,undefined);
	padx          -> tcl2erl:ret_atom([Editor," cg -padx"]);
	pady          -> tcl2erl:ret_atom([Editor," cg -pady"]);
	enable        -> tcl2erl:ret_enable([Editor," cg -st"]);
	fg            -> tcl2erl:ret_color([Editor," cg -fg"]);
	{fg, Pos} ->
	    L=tcl2erl:ret_list([Editor," tag nam ", p_index(Pos)]),
	    SO = GtkId#gtkid.widget_data,
	    case last_tag_val(undefined, $c, L, SO#so.misc) of
		undefined -> tcl2erl:ret_color([Editor," cg -fg"]);
		Color -> Color
	    end;
	{font_style, Pos} ->
	    L=tcl2erl:ret_list([Editor," tag nam ", p_index(Pos)]),
	    SO = GtkId#gtkid.widget_data,
	    case last_tag_val(undefined, $f, L, SO#so.misc) of
		undefined -> 'my style? nyi';
		Style -> Style
	    end;
	selection -> ret_ed_indexes([Editor," tag ne sel 1.0"]);
	char_height   -> tcl2erl:ret_int([Editor, " cg -he"]);
	char_width   -> tcl2erl:ret_int([Editor, " cg -wi"]);
	insertbg      -> tcl2erl:ret_color([Editor," cg -insertba"]);
	insertbw      -> tcl2erl:ret_int([Editor," cg -insertbo"]);
	insertpos     -> ret_ed_index([Editor, " ind insert"]);
	setfocus      -> tcl2erl:ret_focus(Editor, "focus");
	wrap          -> tcl2erl:ret_atom([Editor," cg -wrap"]);
	size          -> {MaxRow,_Col} = ret_ed_index([Editor," ind end"]),
			 MaxRow-1;
	vscrollpos       ->
	    {MaxRow,_Col} = ret_ed_index([Editor," ind end"]),
	    [Top,_Bot] = tcl2erl:ret_list([Editor," yvi"]),
	    round(Top*(MaxRow-1))+1;
	{get, {From, To}} ->
	    tcl2erl:ret_str([Editor, " get ", p_index(From), " ", p_index(To)]);
	_ -> {bad_result, {GtkId#gtkid.objtype, invalid_option, Option}}
    end.


%%------------------------------------------------------------------------------
%%			       PRIMITIVES
%%------------------------------------------------------------------------------

p_index({Line, lineend}) -> [$",gtk:to_ascii(Line), ".1 lineend",$"];
p_index({Line, Char}) -> [gtk:to_ascii(Line), $., gtk:to_ascii(Char)];
p_index(insert)       -> "insert";
p_index('end')        -> "end";
p_index(Idx)          -> gs:error("bad index in editor: ~w~n",[Idx]),0.

ret_ed_index(Cmd) ->
    case gtk:call(Cmd) of
	{result, Val} ->
	    case io_lib:fread("~d.~d", Val) of
		{ok, [Row,Col], []} -> {Row, Col};
		Other -> {bad_result, Other}
	    end;
	Bad_result -> Bad_result
    end.

ret_ed_indexes(Cmd) ->
    case gtk:call(Cmd) of
	{result, ""} -> undefined;
	{result, Val} ->
	    case io_lib:fread("~d.~d ~d.~d", Val) of
		{ok, [Row1,Col1,Row2,Col2], []} -> {{Row1, Col1}, {Row2,Col2}};
		Other -> {bad_result, Other}
	    end;
	Bad_result -> Bad_result
    end.


%%----------------------------------------------------------------------
%% Returns: {Tag text(), NewGtkId}
%%----------------------------------------------------------------------
%% The misc field of the so record is a list of {ColorNo, Color|Font|...}
get_color_tag(Editor,Color,Gtkid) ->
    SO = Gtkid#gtkid.widget_data,
    Tags = SO#so.misc,
    case lists:keysearch(Color, 2, Tags) of
%	{value, {No, _}} -> {["c",gtk:to_ascii(No)], Gtkid};
%	false -> % don't reuse tags, priority order spoils that
	Any ->
	    {No,_} = lists:max(Tags),
	    N=No+1,
	    SO2 = SO#so{misc=[{N,Color}|Tags]},
	    TagStr=["c",gtk:to_ascii(N)],
	    gtk:exec([Editor," tag co ",TagStr," -for ", gtk:to_color(Color)]),
	    {TagStr,Gtkid#gtkid{widget_data=SO2}}
    end.

get_style_tag(DB,Editor,Style,Gtkid) ->
    SO = Gtkid#gtkid.widget_data,
    Tags = SO#so.misc,
    case lists:keysearch(Style, 2, Tags) of
%	{value, {No, _}} -> {["f",gtk:to_ascii(No)], Gtkid};
%	false -> % don't reuse tags, priority order spoils that
	Any -> 
	    {No,_} = lists:max(Tags),
	    N=No+1,
	    SO2 = SO#so{misc=[{N,Style}|Tags]},
	    TagStr=["f",gtk:to_ascii(N)],
	    gtk:exec([Editor," tag co ",TagStr," -font ",
		      gtk_font:choose_ascii(DB,Style)]), % should be style only
	    {TagStr,Gtkid#gtkid{widget_data=SO2}}
    end.

%%----------------------------------------------------------------------
%% Purpose: Given a list of tags for a char, return its visible color
%% (that is that last color tag in the list).
%%----------------------------------------------------------------------
last_tag_val(TagVal, _Chr, [], _TagDict) -> TagVal;
last_tag_val(TagVal, Chr, [Tag|Ts],TagDict) ->
    case atom_to_list(Tag) of
	[Chr|ANo] ->
	    No = list_to_integer(ANo),
	    last_tag_val(gs:val(No, TagDict),Chr,Ts,TagDict);
	NoAcolor ->
	    last_tag_val(TagVal,Chr, Ts,TagDict)
    end.
    
%%% ----- Done -----
