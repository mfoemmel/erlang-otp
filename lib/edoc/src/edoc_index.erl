%% =====================================================================
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%
%% $Id: edoc_index.erl,v 1.2 2004/04/04 14:34:40 richardc Exp $
%%
%% @copyright 2003 Johan Blom
%% @author Johan Blom <Johan.Blom@it.uu.se>
%% @see edoc
%% @end
%% =====================================================================

%% @doc Standard index generator

-module(edoc_index).

-export([toc/3]).

-define(DEFAULT_FILE_SUFFIX, ".html").
-define(INDEX_FILE, "index.html").
-define(OVERVIEW_FILE, "overview.edoc").
-define(MODULES_FRAME, "modules-frame.html").
-define(NL, "\n").
-define(CURRENT_DIR, ".").

-define(EDOC_DIR, "doc").


%% Creates a Table of Content from a list of Paths (ie paths to applications)
%% and an overview file.
toc(InDir,Paths,Options) ->
%    io:format("InDir=~p~n Paths=~p~n Options=~p~n",[InDir,Paths,Options]),
    Opts=Options++[
		   {overview, filename:join(try_subdir(InDir, "doc/index"),
					    ?OVERVIEW_FILE)},
		   {dir, filename:join(InDir, "doc/index")}
		  ],
    Dir = proplists:get_value(dir, Opts, ?CURRENT_DIR),
    Env = edoc_lib:get_doc_env('', [], [], Opts),
    app_index_file(Paths, Dir, Env, Opts).



app_index_file(Paths, Dir, Env, Options) ->
    Title = proplists:get_value(title, Options,"Overview"),
    Priv = proplists:get_bool(private, Options),
    CSS = edoc_doclet:stylesheet(Options),
    Apps1 = [{filename:dirname(A),filename:basename(A)} || A <- Paths],


    edoc_doclet:index_file(Dir, false, Title),
    application_frame(Dir, Apps1, Title, CSS),
%    edoc_doclet:modules_frame(Dir, [], Title, CSS),
    edoc_doclet:overview(Dir, Title, Env, Options),
%    edoc_lib:write_info_file(Prod, [], Modules1, Dir),
    edoc_doclet:copy_stylesheet(Dir, Options).


application_frame(Dir, Apps, Title, CSS) ->
    Body = [?NL,
	    {h2, ["Applications"]},
	    ?NL,
	    {table, [{width, "100%"}, {border, 0}],
	     [{tr, [{td, [], [{a, [{href,app_ref(Path,App)},
				   {target,"_top"}],
			       [App]}]}]}
	      || {Path,App} <- Apps]},
	    ?NL],
    XML = edoc_doclet:xhtml(Title, CSS, Body),
    Text = xmerl:export_simple([XML], xmerl_html, []),
    edoc_lib:write_file(Text, Dir, ?MODULES_FRAME).

app_ref(Path,M) ->
    filename:join([Path,M,?EDOC_DIR,?INDEX_FILE]).
    

%%% Note: Copied from edoc.erl
try_subdir(Dir, Subdir) ->
    D = filename:join(Dir, Subdir),
    case filelib:is_dir(D) of
	true -> D; 
	false -> Dir
    end.

