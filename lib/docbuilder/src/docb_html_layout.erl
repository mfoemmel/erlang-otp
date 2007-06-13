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
-module(docb_html_layout). 

-export([report_top/2, report_bot/1,
	 first_top/2, first_bot/1,
	 ref_top/2, ref_bot/1,
	 chapter_top/2, chapter_bot/1,
	 application_toc_top/3, application_toc_top/4,
	 part_toc_top/3, part_toc_top/4, part_toc_bot/0,
	 index_top/1, index_bot/0]).

%% Report

report_top(Data, Opts) ->
    [Title, Prepared, _Responsible, DocNo, _Approved, _Checked, _Date,
     Vsn0, _File] = Data,
    html_header(Title, Opts) ++
    docb_util:html_snippet(top, Opts) ++
"<CENTER>
<H1>" ++ Title ++ "</H1>
<BIG>
  " ++ DocNo ++ version(Opts, Vsn0) ++ "<BR>
  " ++ Prepared ++ "<BR>
</BIG>
</CENTER>
".

report_bot(Opts) ->
    docb_util:html_snippet(bottom, Opts) ++
"</BODY>
</HTML>
".

%% First

first_top(Data, Opts) ->
    [Title, _Prepared, _Responsible, DocNo, _Approved, _Checked, _Date,
     Vsn0, _File] = Data,
    html_header(Title, Opts) ++
    docb_util:html_snippet(top, Opts) ++
"<CENTER>
<H1>" ++ Title ++ "</H1>
<BIG>" ++ DocNo ++ version(Opts, Vsn0) ++ "<BR>
</BIG>
</CENTER>
".

first_bot(Opts) ->
    report_bot(Opts).

%% Reference

ref_top(Data, Opts) ->
    [Title, _Prepared, _Responsible, _DocNo, _Approved, _Checked,
     _Date, _Rev, _File] = Data,
    ref_html_header(Title, Opts) ++
"<!-- refpage -->\n" ++
    docb_util:html_snippet(top, Opts) ++
"<CENTER>
<H1>" ++ Title ++ "</H1>
</CENTER>".

ref_bot(Opts) ->
    docb_util:html_snippet(bottom, Opts) ++
"</BODY>
</HTML>
".

%% Chapter

chapter_top(Data, Opts) ->
    [Title, _Prepared, _Responsible, _DocNo, _Approved, _Checked,
     _Date, _Rev, _File] = Data,
    html_header(Title, Opts) ++
    docb_util:html_snippet(top, Opts).

chapter_bot(Opts) ->
    report_bot(Opts).

%% Application ToC

application_toc_top(Data, DocName, Opts) ->
    [Title, _Prepared, _Responsible, DocNo, _Approved, _Checked,
     _Date, Vsn0, _File] = Data,
    html_header(Title, []) ++
"<CENTER>
<STRONG>" ++ Title ++ "</STRONG>
<P>
<SMALL>
  " ++ DocNo ++ version(Opts, Vsn0) ++ "
</SMALL>
<P>
<SMALL>
  <A TARGET=\"document\" HREF=\"" ++ DocName ++	"_cite.html\">Bibliography</A> |
  <A TARGET=\"document\" HREF=\"" ++ DocName ++ "_term.html\">Glossary</A> |
  <A TARGET=\"document\" HREF=\"" ++ DocName ++ "_index.html\">Index</A> |
  <A TARGET=\"document\" HREF=\"" ++ DocName ++ "_first.html\">Cover</A>" ++ top_index(Opts) ++
"</SMALL>
</CENTER>
<P>
<SMALL>
<STRONG>Table of Contents</STRONG>
</SMALL>
".

application_toc_top(Data, DocName, Opts, HRefTexts) ->
    [Title, _Prepared, _Responsible, DocNo, _Approved, _Checked,
     _Date, Vsn0, _File] = Data,
    html_header(Title, []) ++
"<CENTER>
<SMALL>
" ++
	docb_util:join(
	  lists:map(
	    fun({HRef, Text}) ->
		    "<A TARGET=\"_top\" HREF=\"" ++ HRef ++ "\">" ++
			Text ++ "</A>"
	    end,
	    HRefTexts), " | ") ++ top_index(Opts) ++
"</SMALL>
<P>
<STRONG>" ++ Title ++ "</STRONG>
<P>
<SMALL>" ++ DocNo ++ version(Opts, Vsn0) ++ "<BR>
</SMALL>
<P>
<SMALL>
  <A TARGET=\"document\" HREF=\"" ++ DocName ++ "_cite.html\">Bibliography</A> |
  <A TARGET=\"document\" HREF=\"" ++ DocName ++ "_term.html\">Glossary</A> |
  <A TARGET=\"document\" HREF=\"" ++ DocName ++ "_index.html\">Index</A> |
  <A TARGET=\"document\" HREF=\"" ++ DocName ++	"_first.html\">Cover</A>
</SMALL>
</CENTER>
<P>
<SMALL>
<STRONG>Table of Contents</STRONG>
</SMALL>
".

%% Part ToC

part_toc_top(Data, DocName, Opts) ->
    [Title, _Prepared, _Responsible, DocNo, _Approved, _Checked,
     _Date, Vsn0, _File] = Data,
    html_header(Title, []) ++
"<CENTER>
<STRONG>" ++ Title ++ "</STRONG>
<P>
<SMALL>" ++ DocNo ++ version(Opts, Vsn0) ++ "<BR>
</SMALL>
<P>
<SMALL>
  <A TARGET=\"document\" HREF=\"" ++ DocName ++	"_cite.html\">Bibliography</A> |
  <A TARGET=\"document\" HREF=\"" ++ DocName ++ "_term.html\">Glossary</A> |
  <A TARGET=\"document\" HREF=\"" ++ DocName ++ "_first.html\">Cover</A>" ++
	top_index(Opts) ++
"</SMALL>
</CENTER>
<P>
<SMALL>
<STRONG>Table of Contents</STRONG>
</SMALL>
".

part_toc_top(Data, DocName, Opts, HRefTexts) ->
    [Title, _Prepared, _Responsible, DocNo, _Approved, _Checked,
     _Date, Vsn0, _File] = Data,
    html_header(Title, []) ++
"<CENTER>
<SMALL>
" ++
	docb_util:join(
	  lists:map(
	    fun({HRef, Text}) ->
		    "<A TARGET=\"_top\" HREF=\"" ++ HRef ++ "\">" ++
			Text ++ "</A>"
	    end,
	    HRefTexts), " | ") ++ top_index(Opts) ++
"</SMALL>
<P>
<STRONG>" ++ Title ++ "</STRONG>
<P>
<SMALL>
  " ++ DocNo ++ version(Opts, Vsn0) ++ "<BR>
</SMALL>
<P>
<SMALL>
  <A TARGET=\"document\" HREF=\"" ++ DocName ++ "_cite.html\">Bibliography</A> |
  <A TARGET=\"document\" HREF=\"" ++ DocName ++ "_term.html\">Glossary</A> |
  <A TARGET=\"document\" HREF=\"" ++ DocName ++ "_first.html\">Cover</A>
</SMALL>
</CENTER>
<P>
<SMALL>
<STRONG>Table of Contents</STRONG>
</SMALL>
".

part_toc_bot() ->
"</BODY>
</HTML>
".

%% Index

index_top(_Data) ->
    ref_html_header("INDEX", []) ++
"<H1>INDEX</H1>
<P><EM>Emphasized</EM> index entries refer to <EM>modules</EM>
and <CODE>Courier</CODE> ditos to <CODE>functions</CODE>.\n".

index_bot() ->
    part_toc_bot().

%% Internal functions

html_header(Title, Opts) ->
    Vsn = docb_util:version(),
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<!-- This document was generated using DocBuilder-" ++ Vsn ++ " -->
<HTML>
<HEAD>
  <TITLE>" ++ Title ++ "</TITLE>" ++
  docb_util:html_snippet(head, Opts) ++
"</HEAD>
<BODY BGCOLOR=\"#FFFFFF\" TEXT=\"#000000\" LINK=\"#0000FF\" VLINK=\"#FF00FF\" ALINK=\"#FF0000\">
".

ref_html_header(Title, Opts) ->
    Vsn = docb_util:version(),
"<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">
<!-- This document was generated using DocBuilder-" ++ Vsn ++ " -->
<HTML>
<HEAD>
  <TITLE>" ++ Title ++ "</TITLE>" ++
  docb_util:html_snippet(head, Opts) ++
"  <STYLE TYPE=\"text/css\">
<!--
    .REFBODY     { margin-left: 13mm }
    .REFTYPES    { margin-left: 8mm }
-->
  </STYLE>
</HEAD>
<BODY BGCOLOR=\"#FFFFFF\" TEXT=\"#000000\" LINK=\"#0000FF\" VLINK=\"#FF00FF\" ALINK=\"#FF0000\">
".

version(Opts, Vsn0) ->
    case docb_util:lookup_option(vsn, Opts, Vsn0) of
	"" -> "";
	Vsn -> " Version " ++ Vsn
    end.

top_index(Opts) ->
    case docb_util:lookup_option(top, Opts) of
	false -> "";
	TIFile ->
	    " | <A TARGET=\"_top\" HREF=\"" ++ TIFile ++ "\">Top</A>"
    end.
