%% -*- erlang-indent-level: 2 -*-
%%-----------------------------------------------------------------------
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
%% Copyright 2006, 2007 Tobias Lindahl and Kostis Sagonas
%% 
%%     $Id$
%%

-module(dialyzer_cl_parse).

-export([start/0]).
-export([collect_args/1]).	% used also by typer_options.erl

-include("dialyzer.hrl").

%%-----------------------------------------------------------------------

-type(dial_cl_parse_ret() :: {'check_init',#options{}}
                           | {'plt_info', #options{}}
                           | {'cl',#options{}}
                           | {'gui',#options{}} 
                           | {'error',string()}).

%%-----------------------------------------------------------------------

-spec(start/0 :: () -> dial_cl_parse_ret()).

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

cl(["--add_to_plt"|T]) ->
  put(dialyzer_options_mode, cl),
  put(dialyzer_options_analysis_type, plt_add),
  cl(T);
cl(["--build_plt"|T]) ->
  put(dialyzer_options_mode, cl),
  put(dialyzer_options_analysis_type, plt_build),
  cl(T);
cl(["--check_plt"|T]) ->
  put(dialyzer_options_mode, cl),
  put(dialyzer_options_analysis_type, plt_check),
  cl(T);
cl(["--plt_info"|T]) ->
  put(dialyzer_options_mode, cl),
  put(dialyzer_options_analysis_type, plt_info),
  cl(T);
cl(["--get_warnings"|T]) ->
  put(dialyzer_options_get_warnings, true),
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
  put(dialyzer_options_mode, cl),
  NewTail = command_line(T),
  cl(NewTail);
cl(["--dataflow"|_]) ->
  error("Option --dataflow is no longer supported");
cl(["--succ_typings"|T]) ->
  put(dialyzer_options_analysis_type, succ_typings),
  cl(T);
cl(["-r"++_|T0]) ->
  put(dialyzer_options_mode, cl),
  {Args,T} = collect_args(T0),
  append_var(dialyzer_options_files_rec, Args),
  cl(T);
cl(["--remove_from_plt"|T]) ->
  put(dialyzer_options_mode, cl),
  put(dialyzer_options_analysis_type, plt_remove),
  cl(T);
cl(["--com"++_|T]) ->
  NewTail = command_line(T),
  cl(NewTail);
cl(["--no_warn_on_inline"|_]) ->
  error("Option --no_warn_on_inline is no longer supported");
cl(["--output"]) ->
  error("No outfile specified");
cl(["-o"]) ->
  error("No outfile specified");
cl(["--old_style"|_]) ->
  error("Option --old_style is no longer supported");
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
cl(["--raw"|T]) ->
  put(dialyzer_output_format, raw),
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
  put(dialyzer_options_report_mode, quiet),
  cl(T);
cl(["--quiet"|T]) ->
  put(dialyzer_options_report_mode, quiet),
  cl(T);
cl(["--src"|T]) ->
  put(dialyzer_options_from, src_code),
  cl(T);
cl(["--no_spec"|T]) ->
  put(dialyzer_options_use_contracts, false),
  cl(T);
cl(["-v"|_]) ->
  io:format("Dialyzer version "++?VSN++"\n"),
  erlang:halt(?RET_NOTHING_SUSPICIOUS);
cl(["--version"|_]) ->
  io:format("Dialyzer version "++?VSN++"\n"),
  erlang:halt(?RET_NOTHING_SUSPICIOUS);
cl(["--verbose"|T]) ->
  put(dialyzer_options_report_mode, verbose),
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
    case get(dialyzer_options_analysis_type) =:= plt_info of
      true ->
	put(dialyzer_options_analysis_type, plt_check),
	{plt_info, cl_options()};
      false ->
	case get(dialyzer_options_mode) of
	  gui -> {gui, common_options()};
	  cl ->
	    case get(dialyzer_options_analysis_type) =:= plt_check of
	      true  -> {check_init, cl_options()};
	      false -> {cl, cl_options()}
	    end
	end
    end,
  case dialyzer_options:build(Opts) of
    {error, Msg} -> error(Msg);
    OptsRecord -> {RetTag, OptsRecord}
  end.


command_line(T0) ->
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
  put(dialyzer_options_mode, gui),
  put(dialyzer_options_files_rec, []),
  put(dialyzer_options_report_mode, normal),
  put(dialyzer_warnings, []),

  DefaultOpts = #options{},
  put(dialyzer_include,                 DefaultOpts#options.include_dirs),
  put(dialyzer_options_defines,         DefaultOpts#options.defines),
  put(dialyzer_options_files,           DefaultOpts#options.files),
  put(dialyzer_output_format,           formatted),
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
  put(Var, get(Var) ++ List),
  ok.

%%-----------------------------------------------------------------------

-spec(collect_args/1 :: ([string()]) -> {[string()],[string()]}).

collect_args(List) ->
  collect_args_1(List, []).

collect_args_1(["-"++_|_]=L, Acc) ->
  {lists:reverse(Acc),L};
collect_args_1([Arg|T], Acc) ->
  collect_args_1(T, [Arg|Acc]);
collect_args_1([], Acc) ->
  {lists:reverse(Acc),[]}.

%%-----------------------------------------------------------------------

cl_options() ->
  [{files,get(dialyzer_options_files)},
   {files_rec,get(dialyzer_options_files_rec)},
   {output_file,get(dialyzer_output)},
   {output_format,get(dialyzer_output_format)},
   {analysis_type, get(dialyzer_options_analysis_type)},
   {get_warnings, get(dialyzer_options_get_warnings)}
   |common_options()].

common_options() ->
  [{defines, get(dialyzer_options_defines)},
   {from, get(dialyzer_options_from)},
   {include_dirs, get(dialyzer_include)},
   {init_plt, get(dialyzer_init_plt)},
   {output_plt, get(dialyzer_output_plt)},
   {report_mode, get(dialyzer_options_report_mode)},
   {use_spec, get(dialyzer_options_use_contracts)},
   {warnings, get(dialyzer_warnings)}].

%%-----------------------------------------------------------------------

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
	Suppress warnings for patterns that are unused or cannot match.
    -Wunmatched_returns ***
	Include warnings for function calls which ignore a structured return value.
    -Werror_handling ***
	Include warnings for functions that only return by means of an exception.
Note:
  *** These are options that turn on warnings rather than turning them off.
",
  io:put_chars(S),
  erlang:halt(?RET_NOTHING_SUSPICIOUS).

help_message() ->
  S = "Usage: dialyzer [--help] [--version] [--shell] [--quiet] [--verbose]
		[-pa dir]* [--plt plt] [-Ddefine]* [-I include_dir]* 
		[--output_plt file] [-Wwarn]* [--src] 
		[-c applications] [-r applications] [-o outfile]
		[--build_plt] [--add_to_plt] [--remove_from_plt] [--check_plt]
                [--plt_info] [--get_warnings]
Options: 
   -c applications (or --command-line applications)
       Use Dialyzer from the command line (no GUI) to detect defects in the
       specified applications (directories or .erl or .beam files)
   -r applications
       Same as -c only that directories are searched recursively for 
       subdirectories containing .erl or .beam files (depending on the 
       type of analysis)
   -o outfile (or --output outfile)
       When using Dialyzer from the command line, send the analysis
       results to the specified \"outfile\" rather than to stdout
   --raw
       When using Dialyzer from the command line, output the raw analysis
       results (Erlang terms) instead of the formatted result.
       The raw format is easier to post-process (for instance, to filter
       warnings or to output HTML pages)
   --src
       Override the default, which is to analyze BEAM files, and
       analyze starting from Erlang source code instead
   -Dname (or -Dname=value)
       When analyzing from source, pass the define to Dialyzer (**)
   -I include_dir
       When analyzing from source, pass the include_dir to Dialyzer (**)
   --output_plt file
       Store the plt at the specified file after building it
   --plt plt
       Use the specified plt as the initial plt (if the plt was built 
       during setup the files will be checked for consistency)
   -pa dir
       Include dir in the path for Erlang (useful when analyzing files
       that have '-include_lib()' directives)
   -Wwarn
       A family of options which selectively turn on/off warnings
       (for help on the names of warnings use dialyzer -Whelp)
   --shell
       Do not disable the Erlang shell while running the GUI
   --version (or -v)
       Prints the Dialyzer version and some more information and exits
   --help (or -h)
       Prints this message and exits
   --quiet (or -q)
       Makes Dialyzer a bit more quiet
   --verbose
       Makes Dialyzer a bit more verbose
   --build_plt
       The analysis starts from an empty plt and creates a new one from the
       files specified with -c and -r. Only works for beam files.
       Use --plt or --output_plt to override the default plt location.
   --add_to_plt
       The plt is extended to also include the files specified with -c and -r.
       Use --plt to specify wich plt to start from, and --output_plt to 
       specify where to put the plt. Note that the analysis might include 
       files from the plt if they depend on the new files. 
       This option only works with beam files.
   --remove_from_plt
       The information from the files specified with -c and -r is removed
       from the plt. Note that this may cause a re-analysis of the remaining
       dependent files.
   --check_plt
       Checks the plt for consistency and rebuilds it if it is not up-to-date.
   --plt_info
       Makes Dialyzer print information about the plt and then quit. The plt 
       can be specified with --plt.
   --get_warnings
       Makes Dialyzer emit warnings even when manipulating the plt. Only 
       emits warnings for files that are actually analyzed.

Note:
  * denotes that multiple occurrences of these options are possible.
 ** options -D and -I work both from command-line and in the Dialyzer GUI;
    the syntax of defines and includes is the same as that used by \"erlc\".

Warning options:
  -Wno_return
     Suppress warnings for functions that will never return a value.
  -Wno_unused
     Suppress warnings for unused functions.
  -Wno_improper_lists
     Suppress warnings for construction of improper lists.
  -Wno_fun_app
     Suppress warnings for fun applications that will fail.
  -Wno_match
     Suppress warnings for patterns that are unused or cannot match.
  -Wunmatched_returns ***
     Include warnings for function calls which ignore the return value(s).
  -Werror_handling ***
     Include warnings for functions that only return by means of an exception.
  -Wunderspecs ***
     Warn about underspecified functions 
     (the -spec is strictly more allowing than the success typing)
  -Woverspecs ***
     Warn about overspecified functions 
     (the -spec is strictly less allowing than the success typing)
  -Wspecdiffs ***
     Warn when the -spec is different than the success typing

Note:
   *** These are options that turn on warnings rather than turning them off.


The exit status of the command line version is:

    0 - No problems were encountered during the analysis and no
        warnings were emitted.
    1 - Problems were encountered during the analysis.
    2 - No problems were encountered, but warnings were emitted.
",
  io:put_chars(S),
  erlang:halt(?RET_NOTHING_SUSPICIOUS).
