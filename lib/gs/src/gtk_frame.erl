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
%% Basic Frame Type.
%% ------------------------------------------------------------

-module(gtk_frame).

%%-----------------------------------------------------------------------------
%% 			    FRAME OPTIONS
%%
%%  Attributes:
%%	anchor			n,w,s,e,nw,se,ne,sw,center
%%	bg			Color
%%	bw			Int
%%	data			Data
%%	height			Int
%%	highlightbg		Color
%%	highlightbw		Int
%%	highlightfg		Color
%%	relief			Relief	[flat|raised|sunken|ridge|groove]
%%	width			Int
%%	x			Int
%%	y			Int
%%      cursor                  arrow|busy|cross|hand|help|resize|text
%%
%%  Commands:
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

-export([create/3,config/3,read/3,delete/2,event/5,option/5,read_option/5,
	mk_create_opts_for_child/4]).

-include("gtk.hrl").

%%-----------------------------------------------------------------------------
%%			MANDATORY INTERFACE FUNCTIONS
%%-----------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function   	: create/3
%% Purpose    	: Create a widget of the type defined in this module.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create(DB, GtkId, Opts) ->
    TkW = gtk_generic:mk_tkw_child(DB,GtkId),
    NGtkid=GtkId#gtkid{widget=TkW},
    PlacePreCmd = [";place ", TkW],
    case gtk_generic:make_command(Opts, NGtkid, TkW, "", PlacePreCmd, DB) of
	{error,Reason} -> {error,Reason};
	Cmd when list(Cmd) ->
	    gtk:exec(["frame ", TkW,
		      " -relief raised -bo 0",Cmd]),
	    NGtkid
    end.

mk_create_opts_for_child(DB,Cgtkid, Pgtkid, Opts) ->
    gtk_generic:mk_create_opts_for_child(DB,Cgtkid,Pgtkid,Opts).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
    Opts2 = atomic_width_height(false,false,Opts),
    gtk_generic:mk_cmd_and_exec(Opts2,Gtkid,TkW,SimplePreCmd,PlacePreCmd,DB).

atomic_width_height(false,false,[]) ->
    [];
atomic_width_height(false,Width,[]) ->
    [{width,Width}];
atomic_width_height(Height,false,[]) ->
    [{height,Height}];
atomic_width_height(H,W,[]) ->
    [{width_height,{W,H}}];
atomic_width_height(_,W,[{height,H}|Opts]) ->
    atomic_width_height(H,W,Opts);
atomic_width_height(H,_,[{width,W}|Opts]) ->
    atomic_width_height(H,W,Opts);
atomic_width_height(H,W,[Opt|Opts]) ->
    [Opt|atomic_width_height(H,W,Opts)].

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
%%		  Gtkid   - The gtkid of the widget
%%		  TkW     - The  tk-widget
%%		  DB	  - The Database
%%
%% Return 	: A tuple {OptionType, OptionCmd}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
option(Option, Gtkid, TkW, DB,_) ->
    case Option of
	{bg,          Color} -> {s, [" -bg ", gtk:to_color(Color)]};
	{packer_x, Pack} ->
            gtk_db:insert_opt(DB,Gtkid,Option),
	    none;
	{packer_y, Pack} ->
	    gtk_db:insert_opt(DB,Gtkid,Option),
	    none;
	{width, W} ->
	    execute_pack_cmds(DB,xpack(W,DB,Gtkid)),
	    {s,[" -wi ", gtk:to_ascii(W)]};
	{height, H} ->
	    execute_pack_cmds(DB,ypack(H,DB,Gtkid)),
	    {s,[" -he ", gtk:to_ascii(H)]};
	{width_height,{W,H}} ->
	    execute_pack_cmds(DB, merge_pack_cmds(xpack(W,DB,Gtkid),
						  ypack(H,DB,Gtkid))),
	    {s,[" -he ", gtk:to_ascii(H)," -wi ", gtk:to_ascii(W)]};
	_  -> invalid_option
    end.

xpack(W,DB,Gtkid) ->
    gtk_db:insert_opt(DB,Gtkid,{width,W}),
    case gtk_db:opt_or_not(DB,Gtkid,packer_x) of
	{value,Pack} when list(Pack) ->
	    ColSiz = gs_packer:pack(W,Pack),
	    pack_children(pack_x,x,width,DB,
			  gtk_db:lookup_kids(DB,Gtkid#gtkid.id),
			  ColSiz);
	Else -> []
    end.

ypack(H,DB,Gtkid) ->
    gtk_db:insert_opt(DB,Gtkid,{height,H}),
    case gtk_db:opt_or_not(DB,Gtkid,packer_y) of
	{value,Pack} when list(Pack) ->
	    ColSiz = gs_packer:pack(H,Pack),
	    pack_children(pack_y,y,height,DB,
			  gtk_db:lookup_kids(DB,Gtkid#gtkid.id),
			  ColSiz);
	Else -> []
    end.

merge_pack_cmds([{Id,Opts1}|Cmds1],[{Id,Opts2}|Cmds2]) ->
    [{Id,Opts1++Opts2}|merge_pack_cmds(Cmds1,Cmds2)];
merge_pack_cmds(L1,L2) ->
    L1++L2.

execute_pack_cmds(DB,[{Id,Opts}|Cmds]) ->
    gtk:config_impl(DB,Id,Opts),
    execute_pack_cmds(DB,Cmds);
execute_pack_cmds(_,[]) ->
    ok.

%%----------------------------------------------------------------------
%% Returns: list of {Id,Opts} to be executed (or merged with other first)
%%----------------------------------------------------------------------
pack_children(PackOpt,PosOpt,SizOpt,DB,Kids,Sizes) ->
    Schildren = keep_packed(Kids,PackOpt,DB),
    pack_children2(PackOpt,PosOpt,SizOpt,Schildren,Sizes).

pack_children2(PackOpt,PosOpt,SizOpt,[{StartStop,Id}|Childs],Sizes) ->
    [pack_child(Id,StartStop,SizOpt,PosOpt,Sizes)
     | pack_children2(PackOpt,PosOpt,SizOpt,Childs,Sizes)];
pack_children2(_,_,_,[],_) ->
    [].

pack_child(Id,{StartPos,StopPos},SizOpt,PosOpt,Sizes) ->
    {Pos,Size} = find_pos(StartPos,StopPos,1,0,0,Sizes),
    {Id,[{PosOpt,Pos},{SizOpt,Size}]}.

%%----------------------------------------------------------------------
%% Returns: {PixelPos,PixelSize}
%%----------------------------------------------------------------------
find_pos(_StartPos,Pos,Pos,AccPixelPos,AccPixelSize,[Size|_]) ->
    {AccPixelPos,Size+AccPixelSize};
find_pos(StartPos,StopPos,Pos,AccPixelPos,0,[Size|Sizes])
  when Pos < StartPos ->
    find_pos(StartPos,StopPos,Pos+1,Size+AccPixelPos,0,Sizes);
find_pos(StartPos,StopPos,Pos,AccPixelPos,AccPixelSize,[Size|Sizes])
  when Pos < StopPos ->
    find_pos(Pos,StopPos,Pos+1,AccPixelPos,Size+AccPixelSize,Sizes).

    

keep_packed([Id|Ids],PackOpt,DB) ->
    case gtk:read_impl(DB,Id,PackOpt) of
	undefined ->
	    keep_packed(Ids,PackOpt,DB);
	StartStop ->
	    [{StartStop,Id} | keep_packed(Ids,PackOpt,DB)]
    end;
keep_packed([],_,_) ->
    [].
    


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
read_option(Option,Gtkid,TkW,DB,_) -> 
    case Option of
	bg            -> tcl2erl:ret_color([TkW," cg -bg"]);
	_ -> {bad_result, {Gtkid#gtkid.objtype, invalid_option, Option}}
    end.

%% ----- Done -----
