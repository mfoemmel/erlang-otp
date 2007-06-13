%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999-2000, Ericsson 
%% Utvecklings AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(docb_html).

-export([rule/2, rule/3]).

rule([p, item, list|_], {_, _, _}) ->
    {"", "<BR>\n"};
rule([p, item, taglist|_], {_, _, _}) ->
    {"", "<BR>\n"};
rule([p|_], _) ->
    {"\n<P>", ""};

rule([pre|_], _) ->
    {"\n<PRE>\n", "\n</PRE>\n"};

rule([input|_], _) ->
    {"<STRONG>", "</STRONG>"};

rule([quote|_], _) ->
    {"\n<BLOCKQUOTE>\n", "\n</BLOCKQUOTE>\n"};

rule([i|_], _) ->
    {"<EM>", "</EM>"};

rule([b|_], _) ->
    {"<STRONG>", "</STRONG>"};

rule([c|_], _) ->
    {"<CODE>", "</CODE>"};

rule([em|_], _) ->
    {"<STRONG>", "</STRONG>"};

rule([sub|_], _) ->
    {"<SUB>", "</SUB>"};

rule([sup|_], _) ->
    {"<SUP>", "</SUP>"};

rule([termdef|_], _) ->
    {drop, ""};

rule([citedef|_], _) ->
    {drop, ""};

rule([br|_], _) ->
    {"<BR>\n", ""};

rule([digression|_], _) ->
    {"<TABLE>\n"
     "  <TR>\n"
     "    <TD WIDTH=\"23\"></TD>\n"
     "    <TD>\n"
     "      <FONT SIZE=-1>\n",
     "      </FONT>\n"
     "    </TD>\n"
     "  </TR>\n"
     "</TABLE>\n"};

rule([list, item, list|_], {_, ["ORDERED"], _}) ->
    {"\n<OL>\n", "\n</OL>\n"};
rule([list, item, taglist|_], {_, ["ORDERED"], _}) ->
    {"\n<OL>\n", "\n</OL>\n"};
rule([list|_], {_, ["ORDERED"], _}) ->
    {"\n<P>\n<OL>\n", "\n</OL>\n"};
rule([list, item, list|_], {_, ["BULLETED"], _}) ->
    {"\n<UL>\n", "\n</UL>\n"};
rule([list, item, taglist|_], {_, ["BULLETED"], _}) ->
    {"\n<UL>\n", "\n</UL>\n"};
rule([list|_], {_, ["BULLETED"], _}) ->
    {"\n<P>\n<UL>\n", "\n</UL>\n"};

rule([taglist, item, taglist|_], _) ->
    {"\n<DL>\n", "\n</DL>\n"};
rule([taglist, item, list|_], _) ->
    {"\n<DL>\n", "\n</DL>\n"};
rule([taglist|_], _) ->
    {"\n<P>\n<DL>\n", "\n</DL>\n"};

rule([tag|_], _) ->
    {"\n<DT>\n", "\n</DT>\n"};

rule([item, list|_], _) ->
    {"\n<LI>\n", "\n</LI>\n\n"};
rule([item, taglist|_], _) ->
    {"\n<DD>\n", "\n</DD>\n"};

rule([image|_], {_, [File], _}) ->
    {["\n<P>\n<CENTER>\n", "<IMG ALT=\"", File, "\" SRC=\"", File,
      ".gif\"><BR>\n"],
     "\n</CENTER>\n"};

rule([icaption|_], _) ->
    {"<EM>", "</EM>\n"};

rule([url|_], {_, [HREF], _}) ->
    URI = docb_html_util:make_uri(HREF),
    {io_lib:format("<A TARGET=\"_top\" HREF=\"~s\">", [URI]), "</A>"};

rule([marker|_], {_, [ID], _})   ->
    {ok, NewID, _} = regexp:sub(ID, "^[^#]*#", ""),
    {drop, ["<A NAME=\"", NewID, "\"><!-- Empty --></A>"]};

rule([table|_], {_, ["", ""], Ts}) ->
    {newargs,
     "\n<P><CENTER>\n"
     "<TABLE CELLSPACING=0 CELLPADDING=2 BORDER=1>\n",
     reorder_table(Ts),
     "\n</TABLE>\n"
     "</CENTER>\n"};
rule([table|_], {_, [Width, ""], Ts}) ->
    {newargs,
     ["\n<P>\n<CENTER>\n"
      "<TABLE CELLSPACING=0 CELLPADDING=2 BORDER=1 ",
      "WIDTH=\"", Width, "%\">\n"],
     reorder_table(Ts),
     "\n</TABLE>\n"
     "</CENTER>\n"};

%% The clauses above are for the report DTD. This one is for the other
%% DTDs.
rule([table|_], {_, _, Ts}) ->
    {newargs,
     "\n<P>\n<CENTER>\n"
     "<TABLE CELLSPACING=0 CELLPADDING=2 BORDER=1>\n",
     reorder_table(Ts),
     "\n</TABLE>\n"
     "</CENTER>\n"};

rule([row|_], _)   ->
    {"  <TR>\n", "\n  </TR>\n"};

rule([cell|_], {_, ["", ""], _})   ->
    {"    <TD>\n", "\n    </TD>\n"};
rule([cell|_], {_, [Align, ""], _})   ->
    {["    <TD ALIGN=\"", Align, "\">\n"], "\n    </TD>\n"};
rule([cell|_], {_, ["", VAlign], _})   ->
    {["    <TD VALIGN=\"", VAlign, "\">\n"], "\n    </TD>\n"};
rule([cell|_], {_, [Align, VAlign], _})   ->
    {["    <TD ALIGN=\"", Align, "\" VALIGN=\"", VAlign, "\">\n"],
     "\n    </TD>\n"};

rule([tcaption|_], _)   ->
    {"  <CAPTION ALIGN=BOTTOM><EM>", "</EM></CAPTION>\n"};

rule([codeinclude|_], {_, [File, Tag, "ERL"], _}) ->
    docb_html_util:code_include(File, Tag);
rule([codeinclude|_], {_, [File, Tag, "C"], _}) ->
    docb_html_util:code_include(File, Tag);
rule([codeinclude|_], {_, [File, Tag, "NONE"], _}) ->
    docb_html_util:code_include(File, Tag);

rule([erleval|_], {_, [Expr], _}) ->
    docb_html_util:erl_eval(Expr);

rule([pcdata, pre|_], {_, _, Data}) ->
    %% Do not remove leading spaces.
    {drop, docb_html_util:pcdata_to_html(Data, false)};

rule([pcdata|_], {_, _, Data}) ->
    {drop, docb_html_util:pcdata_to_html(Data)}.

rule([seealso|_], {_, [Marker], _}, Opts) ->
    Href =
	case docb_util:html_snippet(seealso, Marker, Opts) of
	    "" ->
		%% DocBuilder default behavior:
		%% Marker is of format "Path#Fragment", both optional.
		%% Translated to <A HREF="Path.html#Fragment">
		case string:chr(Marker, $#) of
		    0 -> % No Fragment
			Marker++".html";
		    1 -> % No Path
			Marker;
		    _ ->
			case string:tokens(Marker, "#") of
			    [Path] -> % # at end, remove it
				Path++".html";
			    [Path | Frag0] ->
				Path++".html#"++
				    docb_util:join(Frag0, "#")
			end
		end;
	    Href0 ->
		%% User defined behavior, use result as-is
		Href0
	end,
    {{["<A HREF=\"", Href, "\">"], "</A>"}, Opts};

rule([warning|_], _, Opts) ->
    docb_html_util:copy_pics("warning.gif", "warning.gif", Opts),
    {{"\n<P>\n"
      "<TABLE CELLPADDING=4>\n"
      "  <TR>\n"
      "    <TD VALIGN=TOP><IMG ALT=\"Warning!\" SRC=\"warning.gif\"></TD>\n"
      "    <TD>\n",
      "    </TD>\n"
      "  </TR>\n"
      "</TABLE>\n"}, Opts};

rule([note|_], _, Opts) ->
    docb_html_util:copy_pics("note.gif", "note.gif", Opts),
    {{"\n<P>\n"
      "<TABLE CELLPADDING=4>\n"
      "  <TR>\n"
      "    <TD VALIGN=TOP><IMG ALT=\"Note!\" SRC=\"note.gif\"></TD>\n"
      "    <TD>\n",
      "    </TD>\n"
      "  </TR>\n"
      "</TABLE>\n"}, Opts};

rule([path|_], {_, [UNIX, Windows], [{pcdata, _, Text}]}, Opts) ->
    UnixPart =
	docb_util:an_option({ptype,"unix"}, Opts) and (UNIX/=""),
    WinPart =
	docb_util:an_option({ptype,"windows"}, Opts) and (Windows/=""),
    if
	UnixPart, WinPart ->
	    {{drop, [docb_html_util:pcdata_to_html(Text),
		     " <FONT SIZE=\"-2\">(<CODE>UNIX: ", 
		     docb_html_util:attribute_cdata_to_html(UNIX),
		     ", ",
		     "Windows: ",
		     docb_html_util:attribute_cdata_to_html(Windows),
		     "</CODE>)</FONT>"]},
	     Opts};
	UnixPart ->
	    {{drop, [docb_html_util:pcdata_to_html(Text),
		     " <FONT SIZE=\"-1\">(<CODE>UNIX: ",
		     docb_html_util:attribute_cdata_to_html(UNIX),
		     "</CODE>)</FONT>"]},
	     Opts};
	WinPart ->
	    {{drop, [docb_html_util:pcdata_to_html(Text),
		     " <FONT SIZE=\"-1\">(<CODE>Windows: ",
		     docb_html_util:attribute_cdata_to_html(Windows),
		     "</CODE>)</FONT>"]},
	     Opts};
	true ->
	    {{drop, docb_html_util:pcdata_to_html(Text)}, Opts}
    end;

rule([term|_], {_, [ID], _}, Opts) ->
    case docb_util:an_option(dict, Opts) of
	false ->
	    case docb_util:lookup_option({defs, term}, Opts) of
		false ->
		    {{drop, ["<EM><STRONG>",
			    ID,
			    "</STRONG></EM> "]}, Opts};
		TermList ->
		    case lists:keysearch(ID, 1, TermList) of
			false ->
			    {{drop, ["<EM><STRONG>", ID,
				    "</STRONG></EM> "]},
			     Opts};
			{value, {ID, Name, _Description, _Resp}} ->
			    {{drop, ["<EM><STRONG>", Name,
				     "</STRONG></EM> "]},
			     Opts};
			{value, {ID, Name, _Description}} ->
			    {{drop, [ "<EM><STRONG>", Name,
				      "</STRONG></EM> "]},
			     Opts}
		    end
	    end;
	true ->
	    case docb_util:lookup_option({defs, term}, Opts) of
		false ->
		    {{drop, ["<EM><STRONG>",  ID,
			     "</STRONG></EM> "]}, Opts};
		TermList ->
		    PartApplication =
			docb_util:lookup_option(part_application, Opts),
		    case lists:keysearch(ID, 1, TermList) of
			false ->
			    {{drop, ["<A HREF=\"", PartApplication,
				    "_term.html#", ID, "\">", ID,
				    "</A> "]}, Opts};
			{value, {ID, Name, _Description, _Resp}} ->
			    {{drop, ["<A HREF=\"", PartApplication,
				    "_term.html#", ID, "\">", Name,
				    "</A> "]}, Opts};
			{value, {ID, Name, _Description}} ->
			    {{drop, ["<A HREF=\"", PartApplication,
				    "_term.html#", ID, "\">", Name,
				    "</A> "]}, Opts}
		    end
	    end
    end;

rule([cite|_], {_, [ID], _}, Opts) ->
    case docb_util:an_option(dict, Opts) of
	false ->
	    case docb_util:lookup_option({defs, cite}, Opts) of
		false ->
		    {{drop, ["<EM><STRONG>", ID, "</STRONG></EM> "]},
		     Opts};
		CiteList ->
		    case lists:keysearch(ID, 1, CiteList) of
			false ->
			    {{drop,
			      ["<EM><STRONG>", ID, "</STRONG></EM> "]},
			     Opts};
			{value, {ID, Name, _Description, _Resp}} ->
			    {{drop, ["<EM><STRONG>", Name,
				     "</STRONG></EM> "]},
			     Opts};
			{value, {ID, Name, _Description}} ->
			    {{drop, ["<EM><STRONG>", Name,
				     "</STRONG></EM> "]},
			     Opts}
		    end
	    end;
	true ->
	    case docb_util:lookup_option({defs, cite}, Opts) of
		false ->
		    {{drop, ["<EM><STRONG>", ID, "</STRONG></EM> "]},
		     Opts};
		CiteList ->
		    PartApp =
			docb_util:lookup_option(part_application, Opts),
		    case lists:keysearch(ID, 1, CiteList) of
			false ->
			    {{drop, ["<A HREF=\"", PartApp,
				     "_cite.html#", ID, "\">", ID,
				     "</A> "]},
			     Opts};
			{value, {ID, Name, _Description, _Resp}} ->
			    {{drop, ["<A HREF=\"", PartApp,
				    "_cite.html#", ID, "\">", Name,
				     "</A> "]},
			     Opts};
			{value, {ID, Name, _Description}} ->
			    {{drop, ["<A HREF=\"", PartApp,
				    "_cite.html#", ID, "\">", Name,
				     "</A> "]},
			     Opts}
		    end
	    end
    end;

rule([code|_], {_, ["ERL"], [{pcdata, _, Code}]}, Opts) ->
    {{drop, ["\n<PRE>\n", docb_html_util:element_cdata_to_html(Code),
	     "\n</PRE>\n"]}, Opts};
rule([code|_], {_, ["C"], [{pcdata, _, Code}]}, Opts) ->
    {{drop, ["\n<PRE>\n", docb_html_util:element_cdata_to_html(Code),
	     "\n</PRE>\n"]}, Opts};
rule([code|_], {_, ["NONE"], [{pcdata, _, Code}]}, Opts) ->
    {{drop, ["\n<PRE>\n", docb_html_util:element_cdata_to_html(Code),
	     "\n</PRE>\n"]}, Opts}.

reorder_table(TableContent) ->
    reorder_table(TableContent, [], []).
reorder_table([], Caption, NewTableContent) ->
    Caption ++ lists:reverse(NewTableContent);
reorder_table([{tcaption,_,_} = Caption | TableContent], _, NewTableContent) ->
    reorder_table(TableContent, [Caption], NewTableContent);
reorder_table([Row | TableContent], Caption, NewTableContent) ->
    reorder_table(TableContent, Caption, [Row | NewTableContent]).
