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
%%% Purpose : The font model

-module(gstk_font).

-compile(export_all).

% option add *w.Button.font gallant
% option add *w.f*font 10x20


init() ->
    %% hack. the only way to find the size of a text seems to be to put
    %% it into a label in an unmappen window (DummyFontWindow)
    gstk:exec("toplevel .dfw;wm withdraw .dfw;" %deiconify
	     "label .dfw.l -text dummyinittxt -padx 0 -pady 0 -borderwidth 0;"
	     "pack .dfw.l").

%%----------------------------------------------------------------------
%% Returns: undefined if font doesn't exist
%%          {WidthPixels, HeightPixels}
%%----------------------------------------------------------------------
width_height(DB, {Fam,Siz}, Txt) ->
    width_height(DB, {Fam,[],Siz},Txt);
width_height(DB, {Fam,Styl,Siz}, Txt) ->
    case font_exist(DB,Fam,Styl,Siz) of
	false -> undefined;
	{variable,V} -> width_height(DB,V,Txt)
    end;
width_height(DB, Font, Txt) when list(Font) ->
    case gstk:call([".dfw.l co -font ", Font,
		   " -text ", gstk:to_ascii(Txt)]) of
	{result, _} ->
	    Width = tcl2erl:ret_int("update idletasks;winfo w .dfw.l"),
	    Height = tcl2erl:ret_int("winfo h .dfw.l"),
	    {Width,Height};
	Bad_Result -> undefined
    end.
    
%%----------------------------------------------------------------------
%% Returns: Font
%%----------------------------------------------------------------------
choose_ascii(DB, Font) ->
    {Fam,Styl,Siz} = choose(DB, Font),
    {variable,V} =gstk_db:lookup(DB,{font,Fam,Styl,Siz}),
    V.

choose(DB, {Fam,Siz}) ->
    choose(DB, Fam,[],Siz);
choose(DB, {Fam,Styl,Siz}) ->
    choose(DB, Fam, Styl, Siz).
choose(DB, Fam, Styl, Siz) ->
    Style = case Styl of
		italic -> [italic];
		bold -> [bold];
		[italic,bold] -> [bold,italic];
		Sorted -> Sorted
	    end,
    case font_exist(DB, Fam, Styl, Siz) of
	false -> 
	    case gstk_db:lookup(DB,{font,Fam, Styl, Siz}) of
		{replaced_by,{font,F,S1,S2}} -> {F,S1,S2};
		_ ->
		    {F,S1,S2} = choose_other(DB,Fam, Styl, Siz),
		    gstk_db:insert(DB,{font,Fam, Styl, Siz},
				  {replaced_by,{font,F,S1,S2}}),
		    {F,S1,S2}
	    end;
	_ -> {Fam, Styl, Siz}
    end.

%%----------------------------------------------------------------------
%% Returns: Font
%%----------------------------------------------------------------------
choose_other(DB, screen, [], Siz) ->
    case smaller(DB, screen, [], Siz) of
	false ->
	    choose(DB, courier, [], Siz);
	Fnt -> Fnt
    end;
choose_other(DB, screen, St, Si) ->
    case font_exist(DB, courier, St, Si) of
	false -> choose(DB, screen, [], Si);
	_ ->  {courier, St, Si}
    end;
choose_other(DB, Fam, St, Si) ->
    case smaller(DB, Fam, St, Si) of
	false -> choose(DB,standard,[],12);
	Fnt -> Fnt
    end.
	    
%% DB contains: {font,Fam,Style,Size} -> {replaced_by,{font,Fam,Style,Size}} or
%%                            {variable, TkVariableStrInclDollar}

%% Returns: false|{variable,V}
font_variable(DB,Fam,Style,Siz) ->
    case font_exist(DB, Fam,Style,Siz) of
	false -> false;
	{replaced_by,Font} ->
	    gstk_db:lookup(DB,Font)
    end.

%% Returns: false|{variable,V}|
font_exist(DB, Fam,Style,Siz) ->
    case gstk_db:lookup(DB,{font,Fam,Style,Siz}) of
	{variable,V} -> {variable,V};
	{replaced_by,_} -> false;
	undefined -> 
	    FontStr = mk_font(DB,Fam,Style,Siz),
	    case width_height(DB, FontStr,"a") of
		undefined -> false;
		WH ->
		    N = gstk_db:counter(DB,font),
		    Var=["f",gstk:to_ascii(N)],
		    gstk:exec(["global ",Var,";set ",Var," ",FontStr]),
		    %% should us variable syntax gs(f1) instead
		    %% have to recompile erlcall to define this global gs var
		    V2 = {variable,["[global ",Var,";set ",Var,$]]},
		    gstk_db:insert(DB,{font,Fam,Style,Siz},V2),
		    V2
	    end
    end.

%% Returns: false|Font
smaller(DB, Fam,Styl,Siz) ->
    Even = Siz - (Siz rem 2),
    Common = common_size(Siz),
    case font_exist(DB, Fam, Styl, Even) of
	false ->
	    case font_exist(DB, Fam, Styl, Common) of
		false -> false;
		_ -> {Fam, Styl, Common}
	    end;
	_ -> {Fam, Styl, Even}
    end.

common_size(Siz) ->
    find_smaller_size(6, [8,10,12,14,18,24,36,48],Siz).

find_smaller_size(Prev,[Cur|Sizes],Size) when Cur > Size -> Prev;
find_smaller_size(Prev,[Cur|Sizes],Size) ->
    find_smaller_size(Cur,Sizes,Size);
find_smaller_size(Prev,[],Size) -> 48.


%% ----- The Font Model -----
-define(SEVEN,"-*-*-*-*-*-*-*").
mk_font(DB, standard,Style,Sz) ->
    "fixed";
mk_font(DB, courier,Style,Sz) ->
    ["-*-courier-",fix_style(Style,"o"),"-*-*-",fix_size(Sz),?SEVEN];
mk_font(DB, times,Style,Sz) ->
    ["-*-times-",fix_style(Style,"i"),"-*-*-",fix_size(Sz),?SEVEN];
mk_font(DB, helvetica,Style,Sz) ->
    ["-*-helvetica-",fix_style(Style,"o"),"-*-*-",fix_size(Sz),?SEVEN];
mk_font(DB, symbol,Style,Sz) ->
    ["-*-symbol-",fix_style(Style,"o"),"-*-*-",fix_size(Sz),?SEVEN];
mk_font(DB, new_century_schoolbook,Style,Sz) ->
    ["\"-*-new century schoolbook-",fix_style(Style,"i"),"-*-*-",fix_size(Sz),
     ?SEVEN,"\""];
mk_font(DB, screen,[], Sz) ->
    Str2 = lists:append("screen", gstk:to_ascii(Sz)),
    case width_height(DB,Str2,"a") of
	undefined ->
	    %% not my problem
	    ["-*-screen-",fix_style([],"o"),"-*-*-",fix_size(Sz),?SEVEN];
	WH -> Str2
    end;
mk_font(DB, screen,[bold], Sz) ->
    Str2 = lists:append(["screen", gstk:to_ascii(Sz),"b"]),
    case width_height(DB,Str2,"a") of
	undefined ->
	    %% not my problem
	    ["-*-screen-",fix_style([bold],"o"),"-*-*-",fix_size(Sz),?SEVEN];
	WH -> Str2
    end;

mk_font(DB, screen,St, Sz) ->
    ["-*-screen-",fix_style(St,"o"),"-*-*-",fix_size(Sz),?SEVEN];
mk_font(DB, Other,Style,Sz) ->
    ["-*-",io_lib:format("~p",[Other]),"-",fix_style(Style,"o"),"-*-*-",
     fix_size(Sz),?SEVEN].

fix_style([bold,italic],Z) -> ["bold-",Z];
fix_style([bold],Z) -> "bold-r";
fix_style([italic],Z) -> ["medium-",Z];
fix_style(bold,Z) -> "bold-r";
fix_style(italic,Z) -> ["medium-",Z];
fix_style([],Z) -> ["medium-r"];
fix_style(Style,_) ->
    io:format("bad font style: ~w.~n",[Style]),
    "medium-r".

fix_size(N) when integer(N) -> io_lib:format("~w",[N]).

