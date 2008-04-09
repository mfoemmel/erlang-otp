%% -*- erlang-indent-level: 2 -*-
%%===========================================================================
%% File        : typer_options.erl
%% Author      : Bingwen He <Bingwen.He@gmail.com>
%% Description : Handles all command-line options given to TypEr
%%===========================================================================

-module(typer_options).

-export([process/0, option_type/1]).

%%---------------------------------------------------------------------------

-include("typer.hrl").
-include("typer_options.hrl").

%%---------------------------------------------------------------------------
%% Exported functions
%%---------------------------------------------------------------------------

-spec(process/0 :: () -> {#args{}, #typer_analysis{}}).

process() ->
  ArgList = init:get_plain_arguments(),
  %% io:format("Args is ~p\n",[Args]),
  {Args, Analysis} = analyze_args(ArgList, #args{}, #typer_analysis{}),
  %% if the mode has not been set, set it to the default mode (show)
  {Args, case Analysis#typer_analysis.mode of
	   undefined -> Analysis#typer_analysis{mode=?SHOW};
	   Mode when is_atom(Mode) -> Analysis
	 end}.

-spec(option_type/1 :: (typer_option()) -> 'for_annotation' | 'for_show').

option_type(?SHOW) -> for_show;
option_type(?SHOW_EXPORTED) -> for_show;
option_type(?ANNOTATE) -> for_annotation;
option_type(?ANNOTATE_INC_FILES) -> for_annotation.

%%---------------------------------------------------------------------------
%% Internal functions
%%---------------------------------------------------------------------------

analyze_args([], Args, Analysis) ->
  {Args, Analysis};
analyze_args(ArgList, Args, Analysis) ->
  {Result, Rest} = cl(ArgList),
  {NewArgs, NewAnalysis} = analyze_result(Result, Args, Analysis),
  analyze_args(Rest, NewArgs, NewAnalysis).

cl(["-h"|_])     -> help_message();
cl(["--help"|_]) -> help_message();
cl(["-v"|_])        -> version_message();
cl(["--version"|_]) -> version_message();
cl(["--comments"|Opts]) -> {comments, Opts};
cl(["--show"|Opts]) -> {{mode,?SHOW}, Opts};
cl(["--show-exported"|Opts]) -> {{mode,?SHOW_EXPORTED}, Opts};
cl(["--annotate"|Opts]) -> {{mode,?ANNOTATE}, Opts};
cl(["--annotate-inc-files"|Opts]) -> {{mode,?ANNOTATE_INC_FILES}, Opts};
cl(["-D"++Defines|Opts]) ->
  case Defines of
    "" -> typer:error("no defines specified after -D");
    _ ->
      {ok,Result} = regexp:split(Defines, "="),
      Elem = collect_defines(Result),
      {{macros,Elem}, Opts}
  end;
cl(["-I",Dir|Opts]) -> {{inc,Dir}, Opts};
cl(["-I"++Dir|Opts]) ->
  case Dir of
    "" -> typer:error("no include directory specified after -I");
    _ -> {{inc,Dir}, Opts}
  end;
cl(["-T"|Opts]) ->
  {Files, RestOpts} = dialyzer_cl_parse:collect_args(Opts),
  case Files of
    [] -> typer:error("no file or directory specified after -T");
    [_|_] -> {{trust,Files}, RestOpts}
  end;
cl(["-r"|Opts]) ->
  {Files, RestOpts} = dialyzer_cl_parse:collect_args(Opts),
  {{a_dir_r,Files}, RestOpts};
cl(["-"++H|_]) -> typer:error("unknown option -"++H);
cl(Opts) -> 
  {Args, RestOpts} = dialyzer_cl_parse:collect_args(Opts),
  {{analyze,Args}, RestOpts}.

collect_defines(Result) ->
  case Result of
    [Def,Val] ->
      {ok,Tokens,_} = erl_scan:string(Val++"."),
      {ok,ErlVal} = erl_parse:parse_term(Tokens),
      {list_to_atom(Def), ErlVal};
    [Def] ->
      {list_to_atom(Def), true}
  end.

%% Get information about files that the user trusts and wants to analyze
analyze_result({analyze,Val}, Args, Analysis) -> 
  NewVal = Args#args.analyze ++ Val,
  {Args#args{analyze=NewVal}, Analysis};
analyze_result({a_dir_r,Val}, Args, Analysis) -> 
  NewVal = Args#args.analyzed_dir_r ++ Val,
  {Args#args{analyzed_dir_r=NewVal}, Analysis};
analyze_result({trust,Val}, Args, Analysis) -> 
  NewVal = Args#args.trust ++ Val,
  {Args#args{trust=NewVal}, Analysis};
analyze_result(comments, Args, Analysis) ->
  {Args, Analysis#typer_analysis{contracts=false}};
%% Get useful information for actual analysis
analyze_result({mode, Val}, Args, Analysis) -> 
  case Analysis#typer_analysis.mode of
    undefined -> {Args, Analysis#typer_analysis{mode=Val}};
    _ -> mode_error()
  end;
analyze_result({macros,Val}, Args, Analysis) ->
  NewVal = Analysis#typer_analysis.macros ++ [Val],
  {Args, Analysis#typer_analysis{macros=NewVal}};
analyze_result({inc,Val}, Args, Analysis) -> 
  NewVal = Analysis#typer_analysis.includes ++ [Val],
  {Args, Analysis#typer_analysis{includes=NewVal}}.

%%--------------------------------------------------------------------

-spec(mode_error/0 :: () -> no_return()).
mode_error() ->
  typer:error("can not do \"show\", \"show-exported\", \"annotate\", and \"annotate-inc-files\" at the same time").

-spec(version_message/0 :: () -> no_return()).
version_message() ->
  io:format("TypEr version "++?VSN++"\n"),
  erlang:halt(0).

-spec(help_message/0 :: () -> no_return()).
help_message() ->
  S = " Usage: typer [--help] [--version] [--comments]
              [--show | --show-exported | --annotate | --annotate-inc-files]
              [-Ddefine]* [-I include_dir]* [-T application]* [-r] file*

 Options:
   -r dir*
       search directories recursively for .erl files below them
   --show
       Prints type contracts for all functions on stdout.
       (this is the default behaviour; this option is not really needed)
   --show-exported
       Same as --show, but prints contracts for exported functions only
   --annotate
       Annotates the specified files with type contracts
   --annotate-inc-files
       Same as --annotate but annotates all -include() files as well as
       all .erl files (use this option with caution)
   --comments
       Print type information using comments, not type contracts
   -T file*
       The file(s) already contain type annotations and these annotations
       are to be trusted in order to print contracts for the rest of the files
       (Multiple files or dirs, separated by spaces, can be specified.)
   -Dname (or -Dname=value)
       pass the defined name(s) to TypEr
       (The syntax of defines is the same as that used by \"erlc\".)
   -I include_dir
       pass the include_dir to TypEr
       (The syntax of includes is the same as that used by \"erlc\".)
   --version (or -v)
       prints the Typer version and exits
   --help (or -h)
       prints this message and exits

 Note:
   * denotes that multiple occurrences of these options are possible.
",
  io:put_chars(S),
  erlang:halt(0).
