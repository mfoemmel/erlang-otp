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
%%
%% Interface functions of exref.
%%
%% Possible options are:
%%   search:        Search code path for source code
%%   verbose:       Output modules while parsing
%%   auto:          Auto include modules referenced
%%   recursive:     Recursivly find load erlang files
%%   warnings:      Report warnings about hard references such as


-module(exref).

-export([start/0, stop/0]).
-export([module/1, module/2, get_modules/0]).
-export([directory/1, directory/2]).
-export([delete_module/1]).
-export([directory_module/2, directory_module/3]).
-export([includes/1, excludes/1, defs/1]).
-export([analyse/1, analyse/2]).
-export([pretty/1]).


% start/0
%
% Starts a new instance of the exref server.
%
% Usage:
% start() ->
%     {ok, ServerPid}
% Reason = {already_started, ServerPid} | term()
% ServerPid = pid(),
%
% Note:
% start/0 exits if any abnormalities are encountered.
%
start() ->
    case catch gen_server:start({local,exref}, exref_server, [], []) of
	{ok, Pid} ->
	    {ok, Pid};
	Other ->
	    exit({'EXREF INTERNAL ERROR: Unable to start exref server.', Other})
    end.



% stop/0
%
% Stops the exref server.
%
% Usage:
% stop() ->
%    stopped
% stopped = atom()
%
% Note:
% stop/0 exits if any abnormalities are encountered.
%
stop() -> 
    case catch gen_server:call(exref, stop) of
	stopped ->
	    stopped;
	Other ->
	    exit({'EXREF INTERNAL ERROR: Unable to stop exref server.', Other})
    end.



% module/1
%
% Loads one or more modules into the cross reference graph.
% Calling module(Modules) is equivalent to calling
% module(Modules, [search, verbose]). For a description of
% the options search and verbose, see module/2 below.
%
% Usage:
% module(Modules) ->
%    true
% Modules = atom() | [atom()]
%
% Note:
% module/1 exits if any abnormalities are encountered.
%
module(Modules) ->
    module(Modules, [verbose, search]).



% module/2
%
% Loads one or more modules into the cross reference graph.
%
% Usage:
% module(Modules, Options) ->
%    true
% Modules = atom() | [atom()]
% Options = [atom()]
%
% Options:
% search -     Search for source files in the load path (code:get_path())
%              if not found in CWD.
% verbose -    Output names of modules being successfully loaded.
% auto -       Transitively try to include all referenced modules.
% warnings -   Emit warnings (see ref. man).
%
% Note:
% module/2 exits if any abnormalities are encountered.
%
module(Modules, Options) when atom(Modules), list(Options) -> 
    case catch gen_server:call(exref, {module, Modules, Options}, infinity) of
	true ->
	    true;
	_Other ->
	    exit({'EXREF INTERNAL ERROR: Unable to load modules.', {Modules, Options}})
    end;
module(Modules, Options) when list(Modules), list(Options) -> 
    case catch gen_server:call(exref, {module, Modules, Options}, infinity) of
	true ->
	    true;
	_Other ->
	    exit({'EXREF INTERNAL ERROR: Unable to load modules.', {Modules, Options}})
    end.



% directory/1
%
% Loads all .erl-files in a directory. Calling directory(Directory)
% is equivalent to calling directory(Directory, [verbose]).
%
% Usage:
% directory(Directory) ->
%     true
% Directory = string()
%
% Note:
% directory/1 exits if any abnormalities are encountered.
%
directory(Directory) ->
    directory(Directory, [verbose]).



% directory/2
%
% Loads all .erl-files in a directory.
%
% Usage:
% directory(Directory, Options) ->
%     true
% Directory = string()
% Options = [atom()]
%
% Options:
% search -     Search for source files in the load path (code:get_path())
%              if not found in CWD.
% verbose -    Output names of modules being successfully loaded.
% auto -       Transitively try to include all referenced modules.
% warnings -   Emit warnings (see ref. man).
% recursive -  Recursively include all modules in the directory.
%
% Note:
% directory/2 exits if any abnormalities are encountered.
%
directory(Directory, Options) when list(Directory), list(Options)->
    case catch gen_server:call(exref, {directory, Directory, Options}, infinity) of
	true ->
	    true;
	_Other ->
	    exit({'EXREF INTERNAL ERROR: Unable to load directory.', {Directory, Options}})
    end.



% directory_module/2
%
% Loads modules from a directory.
% Calling directory_module(Directory, Modules) is
% equivalent to calling directory_module(Directory, Modules, [verbose]).
%
% Usage:
% directory_module(Directory, Modules) ->
%     true
% Directory = string()
% Modules = atom() | [atom()]
%
directory_module(Directory, Modules) ->
    directory_module(Directory, Modules, [verbose]).



% directory_module/3
%
% Loads modules from a directory.
%
% Usage:
% directory_module(Directory, Modules, Options) ->
%     true
% Directory = string()
% Modules = atom() | [atom()]
% Options = [atom()]
%
% Options:
% search -     Search for source files in the load path (code:get_path())
%              if not found in CWD.
% verbose -    Output names of modules being successfully loaded.
% auto -       Transitively try to include all referenced modules.
% warnings -   Emit warnings (see ref. man).
% recursive -  Recursively include all modules in the directory.
%
% Note:
% directory_module/3 exits if any abnormalities are encountered.
%
directory_module(Directory, Modules, Options) when
  list(Directory), atom(Modules), list(Options) ->
    case catch gen_server:call(exref,
			       {directory_module, Directory, Modules, Options},
			       infinity) of
	true ->
	    true;
	_Other ->
	    exit({'EXREF INTERNAL ERROR: Unable to load modules.',
		  {Directory,
		  Modules,
		  Options}})
    end;
directory_module(Directory, Modules, Options) when
  list(Directory), list(Modules), list(Options) ->
    case catch gen_server:call(exref,
			       {directory_module, Directory, Modules, Options},
			       infinity) of
	true ->
	    true;
	_Other ->
	    exit({'EXREF INTERNAL ERROR: Unable to load modules.',
		  {Directory,
		  Modules,
		  Options}})
    end.



% get_modules/0
%
% Returns all currently loaded modules.
%
% Usage:
% get_modules() ->
%     [atom()]
%
% Note:
% get_modules/0 exits if any abnormalities are encountered.
%
get_modules() ->
    case catch gen_server:call(exref, get_modules, infinity) of
	Modules when list(Modules) ->
	    Modules;
	_Other ->
	    exit({'EXREF INTERNAL ERROR: Unable to get modules.', {}})
    end.



% delete_module/1
%
% Removes one or more modules from the cross reference graph.
%
% Usage:
% delete_module(Modules) ->
%     true | {false, Modules}
% Modules = atom() | [atom()]
%
% Note:
% delete_module/1 exits if any abnormalities are encountered.
%
delete_module(Modules) when atom(Modules) ->
    case catch gen_server:call(exref, {delete_module, Modules}, infinity) of
	true ->
	    true;
	_Other ->
	    exit({'EXREF INTERNAL ERROR: Unable to delete modules.', {Modules}})
    end;
delete_module(Modules) when list(Modules) ->
    case catch gen_server:call(exref, {delete_module, Modules}, infinity) of
	true ->
	    true;
	_Other ->
	    exit({'EXREF INTERNAL ERROR: Unable to delete modules.', {Modules}})
    end.




% includes/1
%
% Appends directories to the include search path for Erlang
% include files.
%
% Usage:
% includes(Directories) ->
%     true
% Directories = [string()]
%
% Note:
% includes/1 exits if any abnormalities are encountered.
%
includes(Directories) when list(Directories) ->
    case catch gen_server:call(exref, {includes, Directories}, infinity) of
	ok ->
	    true;
	_Other ->
	    exit({'EXREF INTERNAL ERROR: Unable to add includes.', {Directories}})
    end.



% excludes/1
%
% Appends directories to the list of modules not to load
% into the cross reference graph.
%
% Usage:
% excludes(Modules) ->
%     true
% Modules = atom() | [atom()]
%
% Note:
% excludes/1 exits if any abnormalities are encountered.
% 
excludes(Modules) when atom(Modules)->
    case catch gen_server:call(exref, {excludes, [Modules]}, infinity) of
	ok ->
	    true;
	_Other ->
	    exit({'EXREF INTERNAL ERROR: Unable to add excludes.', {Modules}})
    end;
excludes(Modules) when list(Modules)->
    case catch gen_server:call(exref, {excludes, Modules}, infinity) of
	ok ->
	    true;
	_Other ->
	    exit({'EXREF INTERNAL ERROR: Unable to add excludes.', {Modules}})
    end.



% defs/1
%
% Appends definitions to the list of predefined macros used by
% epp:open (see module epp).
%
% Usage:
% defs(Definitions) ->
%     true
% Definitions = [{atom(), term()}]
%
% Note:
% defs/1 exits if any abnormalities are encountered.
%
defs(Definitions) when list(Definitions) ->
    case catch gen_server:call(exref, {defs, Definitions}, infinity) of
	ok ->
	    true;
	_Other ->
	    exit({'EXREF INTERNAL ERROR: Unable to add definitions.', {Definitions}})
    end.



% analyse/1
%
% Performs various types of analysis on the cross reference graph
% of loaded modules (see manual) and returns the result.
%
% Usage:
% analyse(What) ->
%     {What, Result1} | {What, Result1, Result2}
% What = atom()
% Result1 = term() (see ref. man.)
% Result2 = term() (see ref. man.)
%
% Note:
% The 'What' parameter must have one of the values stated below. The
% value of 'What' determines the form of the return value. For
% information on how to interpret the results in more detail, see
% the reference manual.
%
% call | use | module_call module_use | exports_not_called | locals_not_called |
% undefined_functions | recursive_modules | user_defined
%
% Note:
% analyse/1 exits if any abnormalities are encountered.
%
analyse(What) ->
    analyse(What, []).



% analyse/2
%
% Performs various types of analysis on the cross reference graph
% of loaded modules (see the reference manual) and returns the result.
%
% Usage:
% analyse(What, Arguments) ->
%     {What, Result1} | {What, Result1, Result2}
% What = atom()
% Arguments = term()
% Result1 = term() (see ref. man.)
% Result2 = term() (see ref. man.)
%
% Note:
% The 'What' parameter must have one of the values stated below. The
% value of 'What' determines the form of the return value. For
% information on how to interpret the results in more detail, see
% the reference manual.
%
% call | use | module_call module_use | exports_not_called | locals_not_called |
% undefined_functions | recursive_modules | user_defined
%
% Note:
% analyse/2 exits if any abnormalities are encountered.
%
analyse(What, Argument) when atom(What) ->
    case catch gen_server:call(exref, {analyse, What, Argument}, infinity) of
	{call, Result1, Result2} ->
	    {call, Result1, Result2};
	{use, Result1, Result2} ->
	    {use, Result1, Result2};
	{module_call, Result1, Result2} ->
	    {module_call, Result1, Result2};
	{module_use, Result1, Result2} ->
	    {module_use, Result1, Result2};
	{undefined_functions, Result1, Result2} ->
	    {undefined_functions, Result1, Result2};
	{exports_not_called, Result1, Result2} ->
	    {exports_not_called, Result1, Result2};
	{locals_not_called, Result1, Result2} ->
	    {locals_not_called, Result1, Result2};
	{recursive_modules, Result1} ->
	    {recursive_modules, Result1};
	{user_defined, Result1} ->
	    {user_defined, Result1};
	_Other ->
	    exit({'EXREF INTERNAL ERROR: Unable to analyse.', {What, Argument}})
    end.


% Pretty/1
%
% Prints the result of an analysis in a more readable way.
%
% Usage:
% pretty(AnalysisResult) ->
%     ok
% AnalysisResult = term()
%
% Precondition:
% AnalysisResult is returned by analyse/1 or analyse/2. The analysis
% made must not be of type 'user_defined'.
%
% Note:
% pretty/1 exits if any abnormalities are encountered.
%
pretty(AnalysisResult) ->
    case catch exref_pp:pp2_start(AnalysisResult) of
	ok ->
	    ok;
	_Other ->
	    exit({'EXREF INTERNAL ERROR: Unable to print.', {AnalysisResult}})
    end.
