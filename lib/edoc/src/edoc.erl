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
%% $Id: edoc.erl,v 1.72 2004/04/05 17:12:38 richardc Exp $
%%
%% @copyright 2001-2003 Richard Carlsson
%% @author Richard Carlsson <richardc@csd.uu.se>
%%   [http://www.csd.uu.se/~richardc/]
%% @version 0.4 beta
%% @end
%% =====================================================================

%% TODO: fix frameset navigation problems
%% TODO: option for ignoring particular modules
%% TODO: intermediate-level packages: document even if no local sources.
%% TODO: document "references" (as in @see) in general.
%% TODO: make sure all options are documented

%% @doc EDoc - the Erlang program documentation generator.
%%
%% <p>This module provides the main user interface to EDoc.
%% <ul>
%%   <li><a href="overview-summary.html">EDoc User Manual</a></li>
%% </ul></p>

-module(edoc).

-export([packages/1, packages/2, files/1, files/2,
	 application/1, application/2, application/3,
	 run/3,
	 file/1, file/2,
	 read/1, read/2,
	 layout/1, layout/2,
	 get_doc/1, get_doc/2, get_doc/3,
	 read_comments/1, read_comments/2,
	 read_source/1, read_source/2]).

-import(edoc_report, [report/2, report/3, error/1, error/3]).

-include("edoc.hrl").


%% @spec (Name::filename()) -> ok
%% @equiv file(Name, [])

file(Name) ->
    file(Name, []).

%% @spec file(filename(), option_list()) -> ok 
%% @type filename() = //kernel/file:filename()
%% @type option_list() = [term()]
%%
%% @doc Reads a source code file and outputs formatted documentation to
%% a corresponding file.
%%
%% <p><b>Note</b>: This function is the old interface to EDoc and is mainly
%% kept for backwards compatibility. The preferred way of generating
%% documentation is through one of the functions {@link application/1},
%% {@link packages/2} and {@link files/2}.</p>
%% 
%% <p>Possible options are:
%% <dl>
%%  <dt>{@type {def, Macros@}}</dt>
%%    <dd>Specifies a set of macro definitions; see
%%    {@link get_doc/2} for details.</dd>
%%  <dt>{@type {dir, filename()@}}</dt>
%%    <dd>Specifies the output directory for the created file. (By
%%    default, the output is written to the directory of the source
%%    file.)</dd>
%%  <dt>{@type {file_suffix, string()@}}</dt>
%%    <dd>Specifies the suffix for the created file. The default value
%%    is `".html"'.</dd>
%%  <dt>{@type {layout, atom()@}}</dt>
%%    <dd>Specifies a callback module to be used for formatting the
%%    internal XML representation as text. See {@link layout/2} for
%%    details.</dd>
%%  <dt>{@type {preprocess, bool()@}}</dt>
%%    <dd>If the value is `true', the source file will be
%%    read via the Erlang preprocessor (`epp'). The default
%%    value is `false'. Normally, preprocessing is not
%%    necessary for EDoc to work, but if a file contains too exotic
%%    definitions or uses of macros, it will not be possible to read it
%%    without preprocessing. <em>Note: comments in included files will
%%    not be available to EDoc.</em></dd>
%%  <dt>{@type {includes, Path::[string()]@}}</dt>
%%    <dd>Specifies a list of directory names to be searched for include
%%    files, if the `preprocess' option is turned on. The
%%    default value is the empty list. The directory of the source file
%%    is always automatically appended to the search path.</dd>
%%  <dt>{@type {macros, [{atom(), term()@}]@}}</dt>
%%    <dd>Specifies a list of pre-defined Erlang preprocessor
%%    (`epp') macro definitions, used if the
%%    `preprocess' option is turned on. The default value is
%%    the empty list.</dd>
%% </dl></p>
%%
%% <p>See {@link get_doc/2} for further options.</p>
%%
%% <p>For running EDoc from a Makefile or similar, see
%% {@link edoc_run:file/1}.</p>
%%
%% @see read/2
%% @see layout/2
%% @see get_doc/2
%% @see //stdlib/epp

file(Name, Options) ->
    Text = read(Name, Options),
    SrcSuffix = proplists:get_value(source_suffix, Options,
				    ?DEFAULT_SOURCE_SUFFIX),
    BaseName = filename:basename(Name, SrcSuffix),
    Suffix = proplists:get_value(file_suffix, Options,
				 ?DEFAULT_FILE_SUFFIX),
    Dir = proplists:get_value(dir, Options, filename:dirname(Name)),
    edoc_lib:write_file(Text, Dir, BaseName ++ Suffix).


%% TODO: better documentation of files/1/2, packages/1/2, application/1/2/3

%% @spec (Files::[filename() | {package(), [filename()]}]) -> ok
%% @equiv packages(Packages, [])

files(Files) ->
    files(Files, []).

%% @spec (Files::[filename() | {package(), [filename()]}],
%%        Options::option_list()) -> ok
%% @doc Run EDoc on a given set of source files.
%% @equiv run([], Files, Options)
%% @see run/3

files(Files, Options) ->
    run([], Files, Options).

%% @spec (Packages::[package()]) -> ok
%%     package() = atom() | string()
%% @equiv packages(Packages, [])

packages(Packages) ->
    packages(Packages, []).

%% @spec (Packages::[package()], Options::option_list()) -> ok
%%     package() = atom() | string()
%% @doc Run EDoc on a set of packages. The source-path will be used to
%% find the files.
%% @equiv run(Packages, [], Options)
%% @see run/3

packages(Packages, Options) ->
    run(Packages, [], Options).

%% @spec (Application::atom()) -> ok
%% @equiv application(Application, [])

application(App) ->
    application(App, []).

%% @spec (Application::atom(), Options::option_list()) -> ok
%% @doc Run EDoc on an application in its default app-directory.
%% @see run/3
%% @see application/1
%% @see application/3

application(App, Options) when atom(App) ->
    case code:lib_dir(App) of
 	Dir when list(Dir) ->
 	    application(App, Dir, Options);
 	_ ->
 	    report("cannot find application directory for '~s'.",
 		   [App]),
 	    exit(error)
    end.

%% @spec (Application::atom(), Dir::filename(), Options::option_list())
%%        -> ok
%% @doc Run EDoc on an application located in the specified directory.
%% @see run/3
%% @see application/2

application(App, Dir, Options) when atom(App) ->
    Src = edoc_lib:try_subdir(Dir, ?SOURCE_DIR),
    Overview = filename:join(edoc_lib:try_subdir(Dir, ?EDOC_DIR),
			     ?OVERVIEW_FILE),
    Opts = Options ++ [{source_path, [Src]},
		       recursive,
		       {title, io_lib:fwrite("The ~s application", [App])},
		       {overview, Overview},
		       {dir, filename:join(Dir, ?EDOC_DIR)},
		       {includes, [filename:join(Dir, "include")]}],
    %% Recursively document all subpackages of '' - i.e., everything.
    run([''], [], [{application, App} | Opts]).

%% If no source files are found for a (specified) package, no package
%% documentation will be generated either (even if there is a
%% package-documentation file). This is the way it should be.  For
%% specified files, use empty package (unless otherwise specified). The
%% assumed package is always used for creating the output. If the actual
%% module or package of the source differs from the assumption gathered
%% from the path and file name, a warning should be issued (since links
%% are likely to be incorrect).

opt_defaults() ->
    [packages].

opt_negations() ->
    [{no_recursive, recursive},
     {no_packages, packages}].

%% @spec (Packages::[package()],
%%        Files::[filename() | {package(), [filename()]}],
%%        Options::option_list()) -> ok
%% @doc Run EDoc on a given set of source files and/or packages.
%% @see files/2
%% @see packages/2

run(Packages, Files, Opts0) ->
    Opts = proplists:substitute_negations(opt_negations(),
					  Opts0 ++ opt_defaults()),
    Module = case proplists:get_value(doclet, Opts, ?DEFAULT_DOCLET) of
		 M when atom(M) ->
		     M;
		 Other ->
		     report("bad value for option 'doclet': ~P.",
			    [Other, 10]),
		     exit(error)
	     end,
    Path = proplists:get_value(source_path, Opts, [?CURRENT_DIR]),
    Ss = sources(Path, Packages, Opts),
    {Ss1, Ms} = expand_sources(expand_files(Files) ++ Ss, Opts),
    Ps = [P || {_, P, _, _} <- Ss1],
    Dir = proplists:get_value(dir, Opts, ?CURRENT_DIR),
    App = proplists:get_value(application, Opts, ?NO_APP),
    {App1, Ps1, Ms1} = target_dir_info(Dir, App, Ps, Ms, Opts),
    %% The "empty package" is never included in the list of packages.
    Ps2 = edoc_lib:unique(lists:sort(Ps1)) -- [''],
    Ms2 = edoc_lib:unique(lists:sort(Ms1)),
    Fs = package_files(Path, Ps2),
    Env = edoc_lib:get_doc_env(App1, Ps2, Ms2, Opts),
    case catch {ok, Module:run(Ss1, Dir, App1, Ps2, Ms2, Fs, Env, Opts)} of
	{ok, _} ->
	    ok;
	R ->
	    report("error in doclet '~w': ~W.", [Module, R, 20]),
	    exit(error)
    end.

sources(Path, Packages, Opts) ->
    lists:foldl(fun (P, Xs) ->
			edoc_lib:find_sources(Path, P, Opts) ++ Xs
		end,
		[], Packages).

package_files(Path, Packages) ->
    Name = ?PACKAGE_FILE,    % this is hard-coded for now
    D = lists:foldl(fun (P, D) ->
			    F = edoc_lib:find_file(Path, P, Name),
			    dict:store(P, F, D)
		    end,
		    dict:new(), Packages),
    fun (P) ->
	    case dict:find(P, D) of
		{ok, F} -> F;
		error -> ""
	    end
    end.

%% Expand user-specified sets of files.

expand_files([{P, Fs1} | Fs]) ->
    [{P, filename:basename(F), filename:dirname(F)} || F <- Fs1]
	++ expand_files(Fs);
expand_files([F | Fs]) ->
    [{'', filename:basename(F), filename:dirname(F)} |
     expand_files(Fs)];
expand_files([]) ->
    [].

%% Create the (assumed) full module names. Keep only the first source
%% for each module, but preserve the order of the list.

expand_sources(Ss, Opts) ->
    Suffix = proplists:get_value(source_suffix, Opts,
				 ?DEFAULT_SOURCE_SUFFIX),
    Ss1 = case proplists:get_bool(packages, Opts) of
	      true -> Ss;
	      false -> [{'',F,D} || {_P,F,D} <- Ss]
	  end,
    expand_sources(Ss1, Suffix, sets:new(), [], []).

expand_sources([{P, F, D} | Fs], Suffix, S, As, Ms) ->
    M = list_to_atom(packages:concat(P, filename:rootname(F, Suffix))),
    case sets:is_element(M, S) of
	true ->
	    expand_sources(Fs, Suffix, S, As, Ms);
	false ->
	    S1 = sets:add_element(M, S),
	    expand_sources(Fs, Suffix, S1, [{M, P, F, D} | As],
			   [M | Ms])
    end;
expand_sources([], _Suffix, _S, As, Ms) ->
    {lists:reverse(As), lists:reverse(Ms)}.

target_dir_info(Dir, App, Ps, Ms, Opts) ->
    case proplists:get_bool(new, Opts) of
	true ->
	    {App, Ps, Ms};
	false ->
	    case edoc_lib:read_info_file(Dir) of
		{ok, {App1, Ps1, Ms1}} ->
		    {if App == ?NO_APP -> App1;
			true -> App
		     end,
		     Ps ++ Ps1,
		     Ms ++ Ms1};
		{error, _, _} ->
		    {App, Ps, Ms}
	    end
    end.


%% @spec read(File::filename()) -> string()
%% @equiv read(File, [])

read(File) ->
    read(File, []).

%% @spec read(File::filename(), Options::option_list()) ->
%%           string()
%%
%% @doc Like {@link file/2}, but returns the resulting text directly
%% instead of writing to a file. See {@link file/2} for options
%% pertaining to reading source code files.
%%
%% @see file/2
%% @see layout/2
%% @see get_doc/2

read(File, Opts) ->
    {_ModuleName, Doc} = get_doc(File, Opts),
    layout(Doc, Opts).


%% @spec layout(Doc::edoc_module()) -> string()
%% @equiv layout(Doc, [])

layout(Doc) ->
    layout(Doc, []).

%% @spec layout(Doc::edoc_module(), Options::option_list()) -> string()
%%
%% @doc Transforms EDoc documentation data to text. The default layout
%% creates an HTML document.
%%
%% <p>Options:
%% <dl>
%%  <dt>{@type {layout, Module::atom()@}}</dt>
%%    <dd>Specifies a callback module to be used for formatting. The
%%    module must export a function {@link edoc_layout:module/2.
%%    module(Doc, Options)}.  The default callback module is {@link
%%    edoc_layout}.</dd>
%% </dl></p>
%%
%% @see get_doc/2
%% @see edoc_layout:module/2

layout(Doc, Opts) ->
    Module = case proplists:get_value(layout, Opts, ?DEFAULT_LAYOUT) of
		 M when atom(M) ->
		     M;
		 Other ->
		     report("bad value for option 'layout': ~P.",
			    [Other, 10]),
		     exit(error)
	     end,
    case catch {ok, Module:module(Doc, Opts)} of
	{ok, Text} ->
	    Text;
	R ->
	    report("error in layout '~w': ~W.", [Module, R, 20]),
	    exit(error)
    end.


%% @spec (File) ->  [comment()]
%% @equiv read_comments(File, [])

read_comments(File) ->
    read_comments(File, []).

%% @spec read_comments(File::filename(), Options::option_list()) ->
%%           [comment()]
%%
%%	    comment() = {Line, Column, Indentation, Text}
%%	    Line = integer()
%%          Column = integer()
%%          Indentation = integer()
%%          Text = [string()]
%%
%% @doc Extracts comments from an Erlang source code file. See the
%% module {@link //syntax_tools/erl_comment_scan} for details on the
%% representation of comments. Currently, no options are avaliable.

read_comments(File, _Opts) ->
    erl_comment_scan:file(File).


%% @spec (File) -> [syntaxTree()]
%% @equiv read_source(File, [])

read_source(Name) ->
    read_source(Name, []).

%% @spec (File::filename(), Options::option_list()) -> [syntaxTree()]
%%     syntaxTree() = //syntax_tools/erl_syntax:syntaxTree()
%%
%% @doc Reads an Erlang source file and returns the list of "source code
%% form" syntax trees. See {@link file/2} for options pertaining to
%% reading source code files.
%%
%% @see file/2
%% @see //syntax_tools/erl_syntax

read_source(Name, Opts) ->
    case read_source_1(Name, Opts) of
	{ok, Forms} ->
	    check_forms(Forms, Name),
	    Forms;
	{error, R} ->
	    error({"error reading file '~s'.",
		   [edoc_lib:filename(Name)]}),
	    exit({error, R})
    end.

read_source_1(Name, Opts) ->
    case proplists:get_bool(preprocess, Opts) of
	true ->
	    read_source_2(Name, Opts);
	false ->
	    epp_dodger:parse_file(Name)
    end.

read_source_2(Name, Opts) ->
    Includes = proplists:append_values(includes, Opts)
	++ [filename:dirname(Name)],
    Macros = proplists:append_values(macros, Opts),
    epp:parse_file(Name, Includes, Macros).

check_forms(Fs, Name) ->
    Fun = fun (F) ->
	     case erl_syntax:type(F) of
		 error_marker ->
		     case erl_syntax:error_marker_info(F) of
			 {L, M, D} ->
			     error(L, Name, {format_error, M, D});

			 Other ->
			     report(Name, "unknown error in "
				    "source code: ~w.", [Other])
		     end,
		     exit(error);
		 _ ->
		     ok
	     end
	  end,
    lists:foreach(Fun, Fs).


%% @spec get_doc(File::filename()) -> {ModuleName, edoc_module()}
%% @equiv get_doc(File, [])

get_doc(File) ->
    get_doc(File, []).

%% @spec get_doc(File::filename(), Options::option_list()) ->
%%           {ModuleName, edoc_module()}
%%	ModuleName = atom()
%%
%% @type edoc_module(). The EDoc documentation data for a module,
%% expressed as an XML document in {@link //xmerl. XMerL} format. See the
%% file <a href="../priv/edoc.dtd">`edoc.dtd'</a> for details.
%%
%% @doc Reads a source code file and extracts EDoc documentation
%% data. Note that without an environment parameter (see {@link
%% get_doc/3}), hypertext links may not be correct.
%%
%% <p>Options:
%% <dl>
%%  <dt>{@type {def, Macros@}}</dt>
%%  <dd>where
%%    <ul>
%%      <li>`Macros' = {@type Macro | [Macro]}</li>
%%      <li>`Macro' = {@type {Name::atom(), Text::string()@}}</li>
%%    </ul>
%%    Specifies a set of macro definitions. See
%%    <a href="overview-summary.html#macros">Inline macro expansion</a>
%%    for details.
%%  </dd>
%% </dl></p>
%%
%% <p>See {@link file/2} for options pertaining to reading source code
%% files.</p>
%%
%% @see get_doc/3
%% @see read_source/2
%% @see read_comments/2
%% @see edoc_extract:source/4
%% @see layout/2
%% @see file/2
%% @see //xmerl

get_doc(File, Opts) ->
    Env = edoc_lib:get_doc_env(Opts),
    get_doc(File, Env, Opts).

%% @spec get_doc(File::filename(), Env::edoc_env(),
%%        Options::option_list()) -> term()
%%     edoc_env() = edoc_lib:edoc_env()
%%
%% @doc Like {@link get_doc/2}, but for a given environment
%% parameter. `Env' is an environment created by {@link
%% edoc_lib:get_doc_env/4}.

get_doc(File, Env, Opts) ->
    Forms = read_source(File, Opts),
    Comments = read_comments(File, Opts),
    edoc_extract:source(Forms, Comments, File, Env, Opts).
