%% -*- erlang-indent-level: 2 -*-
%%--------------------------------------------------------------------
%% File        : typer_options.erl
%% Author      : Bingwen He <Bingwen.He@gmail.com>
%% Description : Handles all command-line options given to TypEr
%%--------------------------------------------------------------------

-module(typer_options).

-export([process/0, lookup/2]).

-include("typer.hrl").

process() ->
  Args = init:get_plain_arguments(),
  %% io:format("Args is ~p\n",[Args]),
  analyze_arg(Args, #args{}, #analysis{}).

lookup(Elem, Options) ->
  lists:member(Elem, Options).

analyze_arg([], Arg, Analysis) -> {Arg, Analysis};
analyze_arg(ArgList, Arg, Analysis) ->
  {Result,Rest} = cl(ArgList),
  {NewArg,NewAnalysis} = analyze_result(Result, Arg, Analysis),
  analyze_arg(Rest, NewArg, NewAnalysis).

cl(["-h"|_])     -> help_message();
cl(["--help"|_]) -> help_message();
cl(["-v"|_])        -> version_message();
cl(["--version"|_]) -> version_message();
cl(["--show"|Opts]) -> {{mode,show}, Opts};
cl(["--show-exported"|Opts]) -> {{mode,show_exported}, Opts};
cl(["--annotate-inc-files"|Opts]) -> {{mode,annotate_inc_files}, Opts};
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
%% Get useful information for actual analysis
analyze_result({mode,Val}, Args, Analysis) -> 
  case Analysis#analysis.mode of
    [] -> {Args, Analysis#analysis{mode=[Val]}};
    _  -> mode_error()
  end;
analyze_result({macros,Val}, Args, Analysis) ->
  NewVal = Analysis#analysis.macros ++ [Val],
  {Args, Analysis#analysis{macros=NewVal}};
analyze_result({inc,Val}, Args, Analysis) -> 
  NewVal = Analysis#analysis.includes ++ [Val],
  {Args, Analysis#analysis{includes=NewVal}}.

%%--------------------------------------------------------------------

mode_error() ->
  typer:error("can not do \"show\", \"show-exported\", and \"annotate-inc-files\" at the same time").
  
version_message() ->
  io:format("TypEr version "++?VSN++"\n"),
  erlang:halt(0).

help_message() ->
  S = " Usage: typer [--help] [--version]
              [--show | --show-exported | --annotate-inc-files]
              [-Ddefine]* [-I include_dir]* [-T application]* [-r] file*

 Options:
   -r
       type annotate application(s) searching their directories
       recursively for .erl files
   --show
       Prints on the current output the types of all functions in the
       file(s) that are analyzed (default is to annotate these files)
   --show-exported
       Same as --show, but only prints types of exported functions
   --annotate-inc-files
       Annotates all -include() files as well as all .erl files that
       are analyzed
   -T file*
       The file(s) already contain type annotations and these annotations
       are to be trusted in order to type annotate the rest of the files
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
