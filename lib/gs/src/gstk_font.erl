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

%% ###########################################################################
%%
%% This module handle fonts. It was changed for Tcl 8.2 but it could
%% probably be simplified more.
%%
%% In Tcl 8.2 we can use named fonts. So the whe get a font request we
%% first check if it already exists and if not we name it and insert it
%% into the database.
%%
%% The font naming is also changedin Tcl 8.2.
%%
%% In Tcl 8.2 there is a way to find out the width of a string in
%% a specified font.
%%
%% ###########################################################################

-module(gstk_font).

%-compile(export_all).

-export([init/0,choose_ascii/2,choose/2,width_height/3]).


-ifndef(NEW_WIDTH_HEIGHT).
init() ->
    %% hack. the only way to find the size of a text seems to be to put
    %% it into a label in an unmappen window (DummyFontWindow)
    gstk:exec("toplevel .dfw;wm withdraw .dfw;" %deiconify
	     "label .dfw.l -text dummyinittxt -padx 0 -pady 0 -borderwidth 0;"
	     "pack .dfw.l").
-else.
init() -> true.
-endif.

%%----------------------------------------------------------------------
%% Returns: undefined if font doesn't exist
%%          {WidthPixels, HeightPixels}
%%----------------------------------------------------------------------
-ifndef(NEW_WIDTH_HEIGHT).
width_height(_DB, FontSpec, Txt) ->
    FontSpecStr = tk_font_spec(FontSpec),
    case gstk:call([".dfw.l co -font ", FontSpecStr,
		   " -text ", gstk:to_ascii(Txt)]) of
	{result, _} ->
	    Width = tcl2erl:ret_int("update idletasks;winfo w .dfw.l"),
	    Height = tcl2erl:ret_int("winfo h .dfw.l"),
%	    io:format("width_height(~p,~p) =>\n~p\n\n",[FontSpec,Txt,{Width,Height}]),
	    {Width,Height};
	_Bad_Result ->
%	    io:format("width_height(~p,~p) =>\nundefined\n\n",[FontSpec,Txt]),
	    undefined
    end.
-else.
%% This code should work but does't. Tk gives incorrect
%% values if asking to fast or something /kent
width_height(DB, FontSpec, Txt) when tuple(FontSpec) ->
    FontSpecStr = tk_font_spec(FontSpec),
    {Family,Size} = font_family_size(FontSpec),
    LineHeight =
	case cached_line_height(DB, {Family,Size}) of
	    undefined ->
		LineH = tcl2erl:ret_int(
			  ["font metrics ",FontSpecStr," -linespace"]),
		cache_line_height(DB, {Family,Size}, LineH),
		LineH;
	    LineH ->
		LineH
	end,
    EscapedText = gstk:to_ascii(Txt),
    Width = tcl2erl:ret_int(
	      ["font measure ",FontSpecStr," ",EscapedText]),
    Height = LineHeight * line_count(Txt),
    {Width,Height};

width_height(_DB, FontSpec, Txt) when list(FontSpec) ->
    EscapedText = gstk:to_ascii(Txt),
    Width =
	tcl2erl:ret_int(["font measure ",FontSpec," ",EscapedText]),
    LineHeight =
	tcl2erl:ret_int(["font metrics ",FontSpec," -linespace"]),
    Height = LineHeight * line_count(Txt),
    {Width,Height}.

font_family_size({Family,_Style,Size}) ->
    {Family,Size};
font_family_size({Family,Size}) ->
    {Family,Size}.

cached_line_height(DB,FontSpec) ->
    gstk_db:lookup(DB, {cached_line_height,FontSpec}).

cache_line_height(DB,FontSpec,Size) ->
    gstk_db:insert(DB, {cached_line_height,FontSpec}, Size).

line_count(Line) ->
    line_count(Line, 1).

line_count([H | T], Count) ->
    Count + line_count(H, 0) + line_count(T, 0);
line_count($\n, Count) -> Count + 1;
line_count(Char, Count) when integer(Char) -> Count;
line_count([], Count) -> Count.
-endif.
    
% "expr [font metrics ",FSpec," -linespace] * \
% [regsub -all \\n ",Txt," {} ignore]"

%%----------------------------------------------------------------------
%% Returns: Font specification string in Tk format
%%
%% The input is {Family,Size} or {Family,Style,Size} where Family and
%% Style are atoms ?! FIXME true???
%%----------------------------------------------------------------------
choose_ascii(DB, Font) ->
    {Fam,Styl,Siz} = choose(DB, Font),
    {variable,V} =gstk_db:lookup(DB,{font,Fam,Styl,Siz}),
%    io:format("choose_ascii(~p) =>\n~p\n\n",[Font,V]),
    V.

%% DB contains: {font,Fam,Style,Size} -> {replaced_by,{font,Fam,Style,Size}} or
%%                            {variable, TkVariableStrInclDollar}

%% ###########################################################################
%%
%% We create a new font name on the other side and store the name in the
%% database. We reorder the options so that they have a predefined order.
%% 
%% ###########################################################################

choose(DB, {Fam,Siz}) ->
    choose(DB, Fam,[],Siz);
choose(DB, {Fam,Styl,Siz}) ->
    choose(DB, Fam, Styl, Siz).
choose(DB, Fam, Styl, Siz) ->
    Style = case Styl of
		italic -> [italic];
		bold -> [bold];
		[italic,bold] -> [bold,italic];
		[bold,italic] -> [bold,italic];	% FIXME: optimize with "when"
		Any -> Any			% FIXME: Need catch all?
	    end,
    case gstk_db:lookup(DB,{font,Fam,Style,Siz}) of
	{variable,_OwnFontName} -> true;
	undefined -> 
	    N = gstk_db:counter(DB,font),   % FIXME: Can use "font create"
					    % without name to get unique name
	    NewName=["f",gstk:to_ascii(N)],
%	    io:format("~s\n\n",[lists:flatten(["font create ",NewName,tk_font_opt(Fam, Style, Siz)])]),
	    gstk:exec(["font create ",NewName,tk_font_opt(Fam, Style, Siz)]),
	    %% should us variable syntax gs(f1) instead
	    %% have to recompile erlcall to define this global gs var
	    V2 = {variable,NewName},
	    gstk_db:insert(DB,{font,Fam,Style,Siz},V2),
	    true
    end,
%    io:format("choose(~p,~p,~p) =>\n~p\n\n",[Fam,Styl,Siz,{Fam,Style,Siz}]),
    {Fam,Style,Siz}.

%% ----- The Font Model -----

% Guaranteed system fonts to exists in Tk 8.2 are:
%
%   Windows   : system systemfixed ansi ansifixed device oemfixed
%   Unix      : fixed
%
% Times, Courier and Helvetica always exists. Tk try to substitute
% others with the best matchin font.

tk_font_spec({Family,Size}) ->
    tk_font_spec({Family,[],Size});
tk_font_spec({Family,Style,Size}) when atom(Style) ->
    tk_font_spec({Family,[Style],Size});	% FIXME:  Case needed?
tk_font_spec({Family,Style,Size}) when list(Style) ->
    ["{",
     font_family(Family),
     " ",
     gstk:to_ascii(Size),
     " ",
     tk_font_spec_style(Style),
     "}"
    ].
     
tk_font_spec_style([]) ->
    "";
tk_font_spec_style([bold]) ->
    "bold";
tk_font_spec_style([italic]) ->
    "italic";
tk_font_spec_style([bold | Styles]) ->
    ["bold ",tk_font_spec_style(Styles)];
tk_font_spec_style([italic | Styles]) ->
    ["italic ",tk_font_spec_style(Styles)].


tk_font_opt(Fam, Styl, Siz) ->
    [" -family ",
     font_family(Fam),
     tk_font_opt(Styl, Siz)].

tk_font_opt(Styl, Siz) ->
    [" -size ",
     gstk:to_ascii(Siz),
     tk_font_opt(Styl)
    ].

tk_font_opt([]) ->
    "";
tk_font_opt([bold | Styl]) ->
    [" -weight bold",
     tk_font_opt(Styl)];
tk_font_opt([italic | Styl]) ->
    [" -slant italic",
     tk_font_opt(Styl)].


% We do some font family translations

font_family(new_century_schoolbook) ->
    "\"new century schoolbook\"";
font_family(Family) ->
    gstk:to_ascii(Family).
