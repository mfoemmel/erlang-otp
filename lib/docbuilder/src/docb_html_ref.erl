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
-module(docb_html_ref).

-export([rule/2, rule/3]).

rule([description|_],_) ->
    {"\n<H3>DESCRIPTION</H3>\n<DIV CLASS=REFBODY>\n","\n</DIV>\n"};

rule([funcs|_],_) ->
    {"\n<H3>EXPORTS</H3>\n",""};

rule([func|_],_) ->
    {"\n<P>",""};

rule([name, func, funcs, RefType|_], {_,_,[{pcdata,[],Name0}]}) ->
    Name1 = docb_html_util:make_anchor_name_short(Name0, RefType),
    {"<A NAME=\"" ++ Name1 ++ "\"><span class=\"bold_code\">",
     "</span></A><BR>\n"};

rule([fsummary|_],_) ->
    {drop, ""};

rule([type|_], _) ->
    {"\n<DIV CLASS=REFBODY><P>Types:\n  <DIV CLASS=REFTYPES>\n<P>\n",
     "\n  </DIV>\n</DIV>\n"};

rule([v|_], _) ->
    {"<span class=\"bold_code\">","</span><BR>\n"};

rule([d|_], _) ->
    {"\n<DIV CLASS=REFBODY>\n","\n</DIV>\n"};

rule([desc|_], _) ->
    {"\n<DIV CLASS=REFBODY>\n","\n</DIV>\n"};

rule([authors|_], _) -> 
    {"\n<H3>AUTHORS</H3>\n<DIV CLASS=REFBODY>\n","\n</DIV>\n"};

rule([aname|_], _) ->
    {"", " - "};

rule([section|_], {1,_,_}) ->
    {"", ""};
rule([section|_], {_N,_,_}) ->
    {"", "\n</DIV>\n"};

rule([title|_], _) ->
    {"\n<H3>", "</H3>\n<DIV CLASS=REFBODY>\n"};

rule(TagHistory, TagBody) ->
    docb_html:rule(TagHistory, TagBody).

rule([email|_], _, Opts) ->
    case docb_util:html_snippet(email, Opts) of
	"" ->
	    {{"","<BR>\n"}, Opts};
	Email ->
	    {{drop, Email++"<BR>\n"}, Opts}
    end;

rule(TagHistory, TagBody, Opts) ->
    docb_html:rule(TagHistory, TagBody, Opts).
