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
%% $Id: edoc_doclet.erl,v 1.11 2004/04/04 14:45:40 richardc Exp $
%%
%% @copyright 2003 Richard Carlsson
%% @author Richard Carlsson <richardc@csd.uu.se>
%% @see edoc
%% @end
%% =====================================================================

%% @doc Standard doclet module for EDoc.

%% Note that this is written so that it is *not* depending on edoc.hrl!

%% TODO: copy "doc-files" subdirectories, recursively.

-module(edoc_doclet).

-export([run/8]).

%% For edoc_index
-export([index_file/3,overview/4,copy_stylesheet/2,stylesheet/1,xhtml/3]).

-import(edoc_report, [report/2, warning/2]).

-define(EDOC_APP, edoc).
-define(DEFAULT_FILE_SUFFIX, ".html").
-define(INDEX_FILE, "index.html").
-define(OVERVIEW_FILE, "overview.edoc").
-define(PACKAGE_SUMMARY, "package-summary.html").
-define(OVERVIEW_SUMMARY, "overview-summary.html").
-define(PACKAGES_FRAME, "packages-frame.html").
-define(MODULES_FRAME, "modules-frame.html").
-define(STYLESHEET, "stylesheet.css").
-define(NL, "\n").

-include("xmerl.hrl").

%% Sources is the list of inputs in the order they were found.  Packages
%% and Modules are sorted lists of atoms without duplicates, and include
%% the things from the edoc-info file (if present) in the target
%% directory. The "empty package" is never included in Packages.

%% Note: this interface is not completely stable.

run(Sources, Dir, App, Packages, Modules, FileMap, Env, Options) ->
    Suffix = proplists:get_value(file_suffix, Options,
				 ?DEFAULT_FILE_SUFFIX),
    Title = proplists:get_value(title, Options,
				if App == [] ->
					"Overview";
				   true ->
					io_lib:fwrite("Application ~s",
						      [App])
				end),
    Priv = proplists:get_bool(private, Options),
    CSS = stylesheet(Options),
    Ms = lists:foldl(fun (Src, Set) ->
			     source(Src, Dir, Suffix, Env, Set,
				    Priv, Options)
		     end,
		     sets:new(), Sources),
    lists:foreach(fun (P) ->
			  package(P, Dir, FileMap, Env, Options)
		  end,
		  Packages),
    Modules1 = [M || M <- Modules, sets:is_element(M, Ms)],
    index_file(Dir, length(Packages) > 1, Title),
    packages_frame(Dir, Packages, Title, CSS),
    modules_frame(Dir, Modules1, Title, CSS),
    overview(Dir, Title, Env, Options),
    edoc_lib:write_info_file(App, Packages, Modules1, Dir),
    copy_stylesheet(Dir, Options),
    ok.

source({M, P, Name, Path}, Dir, Suffix, Env, Set, Priv, Options) ->
    File = filename:join(Path, Name),
    case catch {ok, edoc:get_doc(File, Env, Options)} of
	{ok, {Module, Doc}} ->
	    check_name(Module, M, P, File),
	    Text = edoc:layout(Doc, Options),
	    Name1 = packages:last(M) ++ Suffix,
	    edoc_lib:write_file(Text, Dir, Name1, P),
	    case Priv orelse not is_private(Doc) of
		true ->
		    sets:add_element(Module, Set);
		false ->
		    Set
	    end;
	R ->
	    report("skipping source file '~s': ~W.", [File, R, 15]),
	    Set
    end.

check_name(M, M0, P0, File) ->
    P = list_to_atom(packages:strip_last(M)),
    N = packages:last(M),
    N0 = packages:last(M0),
    if N =/= N0 ->
	    warning("file '~s' actually contains module '~s'.", [File, M]);
       true ->
	    ok
    end,
    if P =/= P0 ->
	    warning("file '~s' belongs to package '~s', not '~s'.",
		    [File, P, P0]);
       true ->
	    ok
    end.


package(P, Dir, FileMap, Env, Opts) ->
    Tags = case FileMap(P) of
	       "" ->
		   [];
	       File ->
		   read_file(File, package, Env, Opts)
	   end,
    Data = edoc_data:package(P, Tags, Env, Opts),
    Text = edoc_layout:package(Data, Opts),
    edoc_lib:write_file(Text, Dir, ?PACKAGE_SUMMARY, P).
    

index_file(Dir, Packages, Title) ->
    Frame1 = {frame, [{src,?PACKAGES_FRAME},
		      {name,"packagesFrame"},{title,""}],
	      []},
    Frame2 = {frame, [{src,?MODULES_FRAME},
		      {name,"modulesFrame"},{title,""}],
	      []},
    Frame3 = {frame, [{src,?OVERVIEW_SUMMARY},
		      {name,"overviewFrame"},{title,""}],
	      []},
    Frameset = {frameset, [{cols,"20%,80%"}],
		case Packages of
		    true ->
			[?NL,
			 {frameset, [{rows,"30%,70%"}],
			  [?NL, Frame1, ?NL, Frame2, ?NL]}
			];
		    false ->
 			[?NL, Frame2, ?NL]
		end
		++ [?NL, Frame3, ?NL,
		    {noframes,
		     [?NL,
		      {h2, ["Yo, man!"]},
		      ?NL,
		      {p, ["Your browser does not dig frames, man!",
			   ?NL, br,
			   "You should be going to the ",
			   {a, [{href, ?OVERVIEW_SUMMARY}],
			    ["non-frame version"]},
			   " instead.", ?NL]},
		      ?NL]},
		    ?NL]},
    XML = xhtml_1(Title, [], Frameset),
    Text = xmerl:export_simple([XML], xmerl_html, []),
    edoc_lib:write_file(Text, Dir, ?INDEX_FILE).

packages_frame(Dir, Ps, Title, CSS) ->
    Body = [?NL,
	    {h2, ["Packages"]},
	    ?NL,
	    {table, [{width, "100%"}, {border, 0}],
	     [{tr, [{td, [], [{a, [{href, package_ref(P)},
				   {target,"overviewFrame"}],
			       [atom_to_list(P)]}]}]}
	      || P <- Ps]},
	    ?NL],
    XML = xhtml(Title, CSS, Body),
    Text = xmerl:export_simple([XML], xmerl_html, []),
    edoc_lib:write_file(Text, Dir, ?PACKAGES_FRAME).

modules_frame(Dir, Ms, Title, CSS) ->
    Body = [?NL,
	    {h2, ["Modules"]},
	    ?NL,
	    {table, [{width, "100%"}, {border, 0}],
	     [{tr, [{td, [],
		     [{a, [{href, module_ref(M)},
			   {target,"overviewFrame"}],
		       [atom_to_list(M)]}]}]}
	      || M <- Ms]},
	    ?NL],
    XML = xhtml(Title, CSS, Body),
    Text = xmerl:export_simple([XML], xmerl_html, []),
    edoc_lib:write_file(Text, Dir, ?MODULES_FRAME).

module_ref(M) ->
    edoc_refs:relative_package_path(M, '') ++ ?DEFAULT_FILE_SUFFIX.

package_ref(P) ->
    edoc_lib:join_uri(edoc_refs:relative_package_path(P, ''),
		      ?PACKAGE_SUMMARY).

xhtml(Title, CSS, Content) ->
    xhtml_1(Title, CSS, {body, [{bgcolor, "white"}], Content}).

xhtml_1(Title, CSS, Body) ->
    {html, [?NL,
	    {head, [?NL, {title, [Title]}, ?NL] ++ CSS},
	    ?NL,
	    Body,
	    ?NL]
    }.

overview(Dir, Title, Env, Opts) ->
    File = proplists:get_value(overview, Opts, ?OVERVIEW_FILE),
    Tags = read_file(File, overview, Env, Opts),
    Data = edoc_data:overview(Title, Tags, Env, Opts),
    Text = edoc_layout:overview(Data, Opts),
    edoc_lib:write_file(Text, Dir, ?OVERVIEW_SUMMARY).

copy_stylesheet(Dir, Options) ->
    case proplists:get_value(stylesheet, Options) of
	undefined ->
	    From = case proplists:get_value(stylesheet_file, Options) of
		       File when list(File) ->
			   File;
		       _ ->
			   %% TODO: strengthen the search for priv-dir?
			   case code:priv_dir(?EDOC_APP) of
			       PrivDir when list(PrivDir) ->
				   filename:join(PrivDir, ?STYLESHEET);
			       _ ->
				   report("cannot find default "
					  "stylesheet file.", []),
				   exit(error)
			   end
		   end,
	    edoc_lib:copy_file(From, filename:join(Dir, ?STYLESHEET));
	_ ->
	    ok
    end.

stylesheet(Options) ->
    case proplists:get_bool(no_stylesheet, Options) of
	true ->
	    [];
	false ->
	    Ref = case proplists:get_value(stylesheet, Options) of
		      undefined ->
			  ?STYLESHEET;
		      S when list(S) ->
			  S;
		      _ ->
			  report("bad value for option 'stylesheet'.",
				 []),
			  exit(error)
		  end,
	    [{link, [{rel, "stylesheet"},
		     {type, "text/css"},
		     {href, Ref}], []},
	     ?NL]
    end.

is_private(E) ->
    case get_attrval(private, E) of
 	"yes" -> true;
 	_ -> false
    end.

get_attrval(Name, #xmlElement{attributes = As}) ->
    case get_attr(Name, As) of
	[#xmlAttribute{value = V}] ->
	    V;
	[] -> ""
    end.

get_attr(Name, [#xmlAttribute{name = Name} = A | As]) ->
    [A | get_attr(Name, As)];
get_attr(Name, [_ | As]) ->
    get_attr(Name, As);
get_attr(_, []) ->
    [].

%% Read external source file. Fails quietly, returning empty tag list.

read_file(File, Context, Env, Opts) ->
    case edoc_extract:file(File, Context, Env, Opts) of
	{ok, Tags} ->
	    Tags;
	{error, _} ->
	    []
    end.
