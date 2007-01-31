%% -*- erlang-indent-level: 2 -*-
%%
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
%% Copyright 2006, Tobias Lindahl and Kostis Sagonas
%% 
%%     $Id$
%%

-module(dialyzer_cl_parse).

-export([start/0]).
-export([collect_args/1]).	% used also by typer_options.erl

-include("dialyzer.hrl").

start() ->
  init(),
  Args = init:get_plain_arguments(),
  try
    cl(Args)
  catch
    throw:{dialyzer_cl_parse_error, Msg} -> {error, Msg};
    _:R ->
      Msg = io_lib:format("~p\n~p\n", [R,erlang:get_stacktrace()]),
      {error, lists:flatten(Msg)}
  end.

cl(["--check_init_plt"|T]) ->
  put(dialyzer_options_gui, false),
  put(dialyzer_only_check_init_plt, true),
  cl(T);
cl(["-D"|_]) ->
  error("No defines specified after -D");
cl(["-D"++Defines|T]) ->
  {ok,Def} = regexp:split(Defines, "="),
  append_defines(Def),
  cl(T);
cl(["-h"|_]) ->
  help_message();
cl(["--help"|_]) ->
  help_message();
cl(["-I"]) ->
  error("no include directory specified after -I");
cl(["-I",Dir|T]) ->
  append_include(Dir),
  cl(T);
cl(["-I"++Dir|T]) ->
  append_include(Dir),
  cl(T);
cl(["-c"++_|T]) ->
  NewTail = command_line(T),
  cl(NewTail);
cl(["-r"++_|T0]) ->
  put(dialyzer_options_gui, false),
  {Args,T} = collect_args(T0),
  append_var(dialyzer_options_files_rec, Args),
  cl(T);
cl(["--com"++_|T]) ->
  NewTail = command_line(T),
  cl(NewTail);
cl(["--no_warn_on_inline"|T]) ->
  put(dialyzer_options_suppress_inline, true),
  cl(T);
cl(["--output"]) ->
  error("No outfile specified");
cl(["-o"]) ->
  error("No outfile specified");
cl(["--output",Output|T]) ->
  put(dialyzer_output, Output),
  cl(T);
cl(["--output_plt"]) ->
  error("No outfile specified for --output_plt");
cl(["--output_plt",Output|T]) ->
  put(dialyzer_output_plt, Output),
  cl(T);
cl(["-o",Output|T]) ->
  put(dialyzer_output, Output),
  cl(T);
cl(["-o"++Output|T]) ->
  put(dialyzer_output, Output),
  cl(T);
cl(["-pa",Path|T]) ->
  case code:add_patha(Path) of
    true -> cl(T);
    {error, _} -> error("Bad directory for -pa: "++Path)
  end;
cl(["--plt", Plt|T]) ->
  put(dialyzer_init_plt, Plt),
  cl(T);
cl(["--plt"]) ->
  error("No plt specified for --plt");
cl(["-q"|T]) ->
  put(dialyzer_options_quiet, true),
  cl(T);
cl(["--quiet"|T]) ->
  put(dialyzer_options_quiet, true),
  cl(T);
cl(["--src"|T]) ->
  put(dialyzer_options_from, src_code),
  cl(T);
cl(["-v"|_]) ->
  io:format("Dialyzer version "++?VSN++"\n"),
  erlang:halt(?RET_NOTHING_SUSPICIOUS);
cl(["--version"|_]) ->
  io:format("Dialyzer version "++?VSN++"\n"),
  erlang:halt(?RET_NOTHING_SUSPICIOUS);
cl(["--verbose"|T]) ->
  put(dialyzer_options_quiet, verbose),
  cl(T);
cl(["-W"|_]) ->
  error("-W given without warning");
cl(["-Whelp"|_]) ->
  help_warnings();
cl(["-W"++Warn|T]) ->
  append_var(dialyzer_warnings, [list_to_atom(Warn)]),
  cl(T);
cl([H|_]) ->
  error("Unknown option: "++H);
cl([]) ->
  {RetTag, Opts} = 
    case get(dialyzer_only_check_init_plt) of
      true -> 
	{check_init, common_options()};
      false -> 
	case get(dialyzer_options_gui) of
	  true -> {gui, common_options()};
	  false -> {cl, cl_opts()}
	end
    end,
  case dialyzer_options:build(Opts) of
    {error, Msg} -> error(Msg);
    OptsRecord -> {RetTag, OptsRecord}
  end.


command_line(T0) ->
  put(dialyzer_options_gui, false),
  {Args,T} = collect_args(T0),
  append_var(dialyzer_options_files, Args),
  %% if all files specified are ".erl" files, set the 'src' flag automatically
  case lists:all(fun(F) -> filename:extension(F) =:= ".erl" end, Args) of
    true -> put(dialyzer_options_from, src_code);
    false -> ok
  end,
  T.

error(Str) ->
  Msg = lists:flatten(Str),
  throw({dialyzer_cl_parse_error, Msg}).

init() ->
  InitPlt = filename:join([code:lib_dir(dialyzer), "plt","dialyzer_init_plt"]),
  put(dialyzer_init_plt, InitPlt),
  put(dialyzer_only_check_init_plt, false),
  put(dialyzer_options_gui, true),
  put(dialyzer_options_files_rec, []),
  put(dialyzer_options_quiet, false),
  put(dialyzer_options_libs, ?DEFAULT_LIBS),
  put(dialyzer_warnings, []),

  DefaultOpts = #options{},
  put(dialyzer_include,                 DefaultOpts#options.include_dirs),
  put(dialyzer_options_core_transform,  DefaultOpts#options.core_transform),
  put(dialyzer_options_defines,         DefaultOpts#options.defines),
  put(dialyzer_options_files,           DefaultOpts#options.files),
  put(dialyzer_options_suppress_inline, DefaultOpts#options.supress_inline),
  put(dialyzer_output,                  DefaultOpts#options.output_file),
  put(dialyzer_options_from,            DefaultOpts#options.from),
  ok.

append_defines([Def, Val]) ->
  {ok, Tokens, _} = erl_scan:string(Val++"."),
  {ok, ErlVal} = erl_parse:parse_term(Tokens),
  append_var(dialyzer_options_defines, [{list_to_atom(Def), ErlVal}]);
append_defines([Def]) ->
  append_var(dialyzer_options_defines, [list_to_atom(Def)]).

append_include(Dir) ->
  append_var(dialyzer_include, [Dir]).

append_var(Var, List) when is_list(List) ->
  put(Var, get(Var) ++ List).

collect_args(List) ->
  collect_args_1(List, []).

collect_args_1(["-"++_|_]=L, Acc) ->
  {lists:reverse(Acc),L};
collect_args_1([Arg|T], Acc) ->
  collect_args_1(T, [Arg|Acc]);
collect_args_1([], Acc) ->
  {lists:reverse(Acc),[]}.

cl_opts() ->
  [{core_transform, get(dialyzer_options_core_transform)},
   {files,get(dialyzer_options_files)},
   {files_rec,get(dialyzer_options_files_rec)},
   {output_file,get(dialyzer_output)}
   |common_options()].

common_options() ->
  [{defines, get(dialyzer_options_defines)},
   {from, get(dialyzer_options_from)},
   {include_dirs, get(dialyzer_include)},
   {init_plt, get(dialyzer_init_plt)},
   {output_plt, get(dialyzer_output_plt)},
   {quiet, get(dialyzer_options_quiet)},
   {supress_inline, get(dialyzer_options_suppress_inline)},
   {warnings, get(dialyzer_warnings)}].

help_warnings() ->
  S = "Warning options:
    -Wno_return
	Suppress warnings for functions of no return.
    -Wno_unused
	Suppress warnings for unused functions.
    -Wno_improper_lists
	Suppress warnings for construction of improper lists.
    -Wno_tuple_as_fun
	Suppress warnings for using tuples instead of funs.
    -Wno_fun_app
	Suppress warnings for fun applications that will fail.
    -Wno_match
	Suppress warnings for pattern matching operations that will never succeed.
    -Wno_comp
	Suppress warnings for term comparisons that will always return false.
    -Wno_guards
	Suppress warnings for guards that will always fail.
    -Wno_unsafe_beam
	Suppress warnings for unsafe BEAM code produced by an old BEAM compiler.
    -Werror_handling ***
	Include warnings for functions that only return by means of an exception.
Note:
  *** This is the only option that turns on warnings rather than turning them off.
",
  io:put_chars(S),
  erlang:halt(?RET_NOTHING_SUSPICIOUS).

help_message() ->
  S = "Usage: dialyzer [--help] [--version] [--shell] [--quiet] [--verbose]
		[-pa dir]* [--plt plt] [-Ddefine]* [-I include_dir]* 
		[--output_plt file] [-Wwarn]* [--src] 
		[-c applications] [-r applications] [-o outfile]

 Options: 
   -c applications (or --command-line applications)
       use Dialyzer from the command line (no GUI) to detect defects in the
       specified applications (directories or .erl or .beam files)
       Multiple applications, separated by spaces, can be specified
   -r applications
       same as -c only that directories are searched recursively for 
       subdirectories containing .erl or .beam files (depending on the 
       type of analysis)
   -o outfile (or --output outfile)
       when using Dialyzer from the command line, send the analysis
       results in the specified \"outfile\" rather than in stdout
   --src
       overwrite the default, which is to analyze BEAM bytecode, and
       analyze starting from Erlang source code instead
   -Dname (or -Dname=value)
       when analyzing from source, pass the define to Dialyzer (**)
   -I include_dir
       when analyzing from source, pass the include_dir to Dialyzer (**)
   -pa dir
       Include dir in the path for Erlang. Useful when analyzing files
       that have '-include_lib()' directives.
   -Wwarn
       a family of options which selectively turn on/off warnings.
       (for help on the names of warnings use dialyzer -Whelp)
   --no_warn_on_inline
       Suppress warnings when analyzing an inline compiled bytecode file.
   --plt plt
       Use the specified plt as the initial plt. If the plt was built 
       during setup the files will be checked for consistency.
   --output_plt file
       Store the plt at the specified file after building it
   --shell
       do not disable the Erlang shell while running the GUI
   --version (or -v)
       prints the Dialyzer version and some more information and exits
   --help (or -h)
       prints this message and exits
   --quiet (or -q)
       makes Dialyzer a bit more quiet
   --verbose
       makes Dialyzer a bit more verbose

Note:
  * denotes that multiple occurrences of these options are possible.
 ** options -D and -I work both from command-line and in the Dialyzer GUI;
    the syntax of defines and includes is the same as that used by \"erlc\".

The exit status of the command line version is:

    0 - No problems were encountered during the analysis and no
        warnings were emitted.
    1 - Problems were encountered during the analysis.
    2 - No problems were encountered, but warnings were emitted.
",
  io:put_chars(S),
  erlang:halt(?RET_NOTHING_SUSPICIOUS).
