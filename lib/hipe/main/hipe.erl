%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%% Copyright (c) 1998 by Erik Johansson.  All Rights Reserved 
%% ====================================================================
%%  Filename : 	hipe.erl
%%  Module   :	hipe
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 1998-01-28 Erik Johansson (happi@csd.uu.se): Created.
%%  CVS      : $Id$
%% ====================================================================
%% Options:
%%     o0, 'O0', o1, 'O1', o2, 'O2', o3, 'O3': set optimization level
%%       (default 0)
%%     `{'O', N}': set optimization level to N.
%%     icode_rename: rename variables (optimizes stack->reg mapping)
%%     icode_prop: one pass of Icode optimization
%%     icode_bwd_cprop: backward copy propagation on Icode.
%%     rtl_prop_1: first pass of RTL optimization
%%     rtl_prop_2: second pass of RTL optimization
%%     rtl_cse: common subexpression elimination on RTL
%%        Also {rtl_cse, CSE_type}
%%        The CSE_types are the following:
%%        - true/false = normal CSE/no CSE
%%        - local = CSE per block; 
%%        - ebb = CSE per extended basic block; 
%%        - global = CSE by fixpoint iteration)
%%     sparc_profile: inserts profiling counters into code
%%         You get an annotated CFG by hipe_profile:annot(MFA)
%%         (and see also misc/hipe_profile.erl for more info)
%%        Also {sparc_profile, Prof_type}
%%        The Prof_types are the following:
%%        - true/false = normal profiling/no profiling
%%        - block = 
%%        - arc = 
%%     sparc_schedule: perform ILP scheduling on code (also annotates
%%         code with cycle estimates)
%%        Also {sparc_schedule, Sched_type}
%%        The Sched_types are the following:
%%        - true/false = normal scheduling/no scheduling
%%        - ultra =
%%     sparc_post_schedule: schedule after register allocation as well.
%%         There are two reasons for this:
%%         - spill code (rare)
%%         - reg.alloc may reuse registers in nearby instructions, which
%%           can destroy our careful schedule; post-scheduling tries to
%%           repair the damage (if any; I haven't seen it make a difference
%%           yet)
%%     sparc_estimate_block_times: do not perform scheduling, but
%%         annotate with cycle estimates
%%     load: load the module after compiling.
%%     {timeout, Time}: Where Time is time in ms or 'infinity', sets the
%%         time the compiler is allowed to use for the compilation.
%%         The default (DEFAULT_TIMEOUT) is set to 15 minutes.
%%     pp_beam, {pp_beam, {file, File}}:
%%     pp_icode, {pp_icode, {file, File}}, {pp_icode, {only, Functions}}:
%%     pp_opt_icode, {pp_opt_icode, {file, File}},
%%         {pp_opt_icode, {only, Functions}}:
%%     pp_rtl, {pp_rtl, {file, File}}, {pp_rtl, {only, Functions}}:
%%     pp_native, {pp_native, {file, File}}, {pp_native, {only, Functions}}:
%%     pp_all: [pp_beam,pp_icode,pp_rtl,pp_native]
%%     pp_asm: Prints the assembly listing with addresses and bytecode.
%%
%%     DEBUG Options: (Might require that some modules have been compiled
%%                     with the debug flag.)
%%     rtl_show_translation: Prints each step in the translation from
%%                           Icode to RTL
%%
%%   You can also write `{Option, false}' to turn `Option' off, or
%%   `{Option, true}' to force it on. Boolean-valued (true/false)
%%   options also have negative-form aliases, e.g. `no_sparc_profile' =
%%   `{sparc_profile, false}'.
%%
%%   *Note*: options are processed in the order they appear; an early
%%   option will 'shadow' a later one. (E.g.,
%%   property_lists:lookup([{foo,false}, foo]) => {foo, false}.)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe).

-export([c/1,
	 c/2,
	 c/3,
 	 f/1,
 	 f/2,
	 compile/1,
	 compile/2,
	 compile/3,
 	 file/1,
 	 file/2,
	 load/1,
	 load/2,
	 help/0,
	 help_options/0,
	 help_option/1,
	 help_debug_options/0,
	 version/0,
	 has_hipe_code/1,
	 expand_options/1]).

-ifndef(DEBUG).
-define(DEBUG,true).
-endif.
-include("hipe.hrl").

-define(COMPILE_DEFAULTS, [o2]).
-define(DEFAULT_TIMEOUT, 900000).  % 15 minutes


%% ---------------------------------------------------------------------
%% User interface for loading code into memory. The code can be given as
%% a native code binary or as the file name of a BEAM file which should
%% contain a native-code chunk. If only the module name is given, the
%% BEAM file is located automatically. Returns `{module, Mod}' or
%% `{error, Reason}'.

load(Mod) ->
    load(Mod, beam_file(Mod)).

load(Mod, Bin) when binary(Bin) ->
    do_load(Mod, Bin, false);
load(Mod, File) ->
   ChunkName =
	case erlang:system_info(hipe_architecture) of
	    ultrasparc -> "HS8P";
	    x86 -> "HX86"
	end,
    case beam_lib:chunks(File, [ChunkName]) of
	{ok,{_,[{_,Bin}]}} ->
	    do_load(Mod, Bin, File);
	Error -> {error, Error}
    end.


%% ---------------------------------------------------------------------
%% This is the user-friendly compiler interface, which by default loads
%% the compiled code directly (use the `no_load' or `{load, false}' flag
%% to turn this off). Returns `{ok, Name}' or `{error, Reason}'. The
%% `file' function takes a file path and deduces the module name from
%% that.

-define(USER_DEFAULTS, [load]).

c(Name) ->
  c(Name, []).

c(Name, Options) ->
  c(Name, beam_file(Name), Options).

c(Name, File, Opts) ->
  init(Opts),
  case compile(Name, File, user_compile_opts(Opts)) of
    {ok, _} ->
      {ok, Name};
    Other ->
      Other
  end.

f(File) ->
  f(File, []).

f(File, Opts) ->
  case file(File, user_compile_opts(Opts)) of
    {ok, Name, _} ->
      {ok, Name};
    Other ->
      Other
  end.

user_compile_opts(Opts) ->
  Opts ++ ?USER_DEFAULTS.


%% ---------------------------------------------------------------------
%% This is the direct compiler interface, which just compiles the named
%% function or module, returning `{ok, Binary}' or `{error, Reason}'. By
%% default, it does not load the binary to memory (use the 'load' flag
%% for this). `File' can be a file name *or* a binary containing the
%% BEAM code for the module.
%%
%% The 'file' function takes the module name directly from the file or
%% binary, regardless of the file name, and returns `{ok, Name, Binary}'
%% or `{error, Reason}'

compile(Name) ->
  compile(Name, []).

compile(Name, Options) ->
  compile(Name, beam_file(Name), Options).

compile(Name, File, Opts0) ->
  init(Opts0),
  Opts = expand_options(Opts0 ++ ?COMPILE_DEFAULTS),
  ?when_option(verbose, Opts, ?debug_msg("Options: ~p.\n",[Opts])),
  check_options(Opts),
  ?option_start_time("Compile", Opts),
  Res = compile_any(Name, File, Opts),
  ?option_stop_time("Compile",Opts),
  Res.

file(File) ->
  file(File, []).

file(File, Options) ->
  case beam_lib:info(File) of
    L when list(L) ->
      {value,{module,Mod}} = lists:keysearch(module,1,L),
      case compile(Mod, File, Options) of
	{ok, Bin} ->
	  {ok, Mod, Bin};
	Other ->
	  Other
      end;
    Error ->
      Error
  end.


%% The rest are internal functions:

compile_any({M,F,A} = MFA, File, Options) ->
  %% When compiling just one function then the emulated code
  %% for the module must be loaded.

  case M of
    hipe_internal ->
      ?error_msg("Nope can't do that\n",[]),
      ?EXIT({cant_compile_hipe_internal});
    _ ->
      ok
  end,
  code:ensure_loaded(M),

  Client = self(),
  CompFun = fun () ->
		compile_mfa(MFA, File, Client, Options)
	    end,
  run_compiler(CompFun, Options);
compile_any(Mod, File, Options) ->
 case Mod of
    hipe_internal ->
      ?error_msg("Nope can't do that\n",[]),
      ?EXIT({cant_compile_hipe_internal});
    _ ->
      ok
  end,
  Client = self(),
  CompFun = fun () ->
		compile_module(Mod, File, Client, Options)
	    end,
  run_compiler(CompFun, Options).

compile_mfa({M, F, A} = MFA, File, Client, Options) ->
  init(Options),
  %% DebugState = get(hipe_debug),
  put(hipe_debug,property_lists:get_bool(debug, Options)),
  ?option_time(Icode = (catch hipe_beam_to_icode:mfa(File, MFA, Options)),
	       "BEAM-to-Icode", Options),
  compile_finish(Icode, M, Client, false, Options).

compile_module(Mod, File, Client, Options) ->
  init(Options),
  %% DebugState = get(hipe_debug),
  put(hipe_debug,property_lists:get_bool(debug, Options)),
  ?option_time(Icode = (catch hipe_beam_to_icode:module(File, Options)),
	       "BEAM-to-Icode", Options),

  Bin = get_beam_code(File),

  compile_finish(Icode, Mod, Client, Bin, Options).

get_beam_code(Bin) when binary(Bin) -> Bin;
get_beam_code(FileName)->
  case erl_prim_loader:get_file(FileName) of
    {ok,Bin,_} ->
      Bin;
    error ->
      ?EXIT(no_beam_file)
  end.

compile_finish({'EXIT', Error}, Mod, Client, WholeModule, Options) ->
  compiler_return({error, Error}, Client);
compile_finish(Icode, Mod, Client, WholeModule, Options) ->
  Res = finalize(Icode, Mod, WholeModule, Options),
  FinalRes = post(Res, Options),
  compiler_return(FinalRes, Client).

run_compiler(CompFun, Options) ->
  process_flag(trap_exit, false),  %% so that the main process is not killed
  CompProc = spawn_link(CompFun),  %% in case the linked CompProc gets killed
  Timeout = case property_lists:get_value(timeout, Options) of
	      N when integer(N), N >= 0 -> N;
	      undefined -> ?DEFAULT_TIMEOUT;
	      infinity -> infinity;
	      Other ->
		?warning_msg("Bad timeout value: ~P\n"
			     "Using default timeout limit.\n",
			     [Other, 5]),
		?DEFAULT_TIMEOUT
	    end,
  receive 
    {CompProc, Result} -> Result;
    {'EXIT', CompProc, Reason} -> exit(Reason)
  after Timeout ->
      exit(CompProc, kill),  %% this kills the compilation process CompProc
      ?error_msg("ERROR: Compilation timed out.~n",[]),
      {error, timed_out}
  end.

compiler_return(Res, Client) ->
  Client ! {self(), Res}.


%% ---------------------------------------------------------------------
%% Finalize/3 compiles, assembles, and optionally loads a list of `{MFA,
%% Icode}' pairs, and returns `{ok, Binary}' or `{error, Reason}'.

finalize(List, Mod, WholeModule, Opts) ->
  {T1Compile,_} = erlang:statistics(runtime),
  CompiledCode = [finalize_fun({MFA, Icode}, Opts)
		  || {MFA, Icode} <- List],
  {T2Compile,_} = erlang:statistics(runtime),
  ?when_option(verbose, Opts,
	       ?debug_msg("Compiled ~p in ~.2f s\n",
			  [Mod,(T2Compile-T1Compile)/1000])),
  case property_lists:get_bool(to_rtl, Opts) of
    true ->
      {ok, CompiledCode};
    false ->
      Closures = 
	[MFA || {MFA, Icode} <- List,
		lists:member(closure,
			     hipe_icode:icode_info(Icode))],
      {T1,_} = erlang:statistics(runtime),
      ?when_option(verbose, Opts, ?debug_msg("Assembling ~w",[Mod])),
      case catch assemble(CompiledCode, Closures, Opts) of
	{'EXIT',Error} -> {error,Error};
	Bin ->
	  {T2,_} = erlang:statistics(runtime),
	  ?when_option(verbose, Opts,
		       ?debug_untagged_msg(" in ~.2f s\n",
					   [(T2-T1)/1000])),
	  maybe_load(Mod, Bin, WholeModule, Opts),
	  TargetArch = get(hipe_target_arch),
	  {ok, {TargetArch,Bin}}
      end
  end.

finalize_fun({MFA, Icode}, Opts) ->
  {T1,_} = erlang:statistics(runtime),
  ?when_option(verbose, Opts, ?debug_msg("Compiling ~w",[MFA])),
  DebugState = get(hipe_debug),
  
  case catch hipe_main:compile_icode(MFA, Icode, Opts, DebugState) of
    {native, Platform, {unprofiled,Code}} ->
      {T2,_} = erlang:statistics(runtime),
      ?when_option(verbose, Opts,
		   ?debug_untagged_msg(" in ~.2f s\n", [(T2-T1)/1000])),
      case Platform of
	ultrasparc -> {Entry,Rest,Ct} = Code, {MFA,{Entry,Rest},Ct};
	x86 -> {MFA, Code}
      end;
    {rtl, LinearRtl} ->
      {MFA, LinearRtl};
    {native, X} ->
      ?error_msg("ERROR: unknown native format: ~P.\n",[X,25]),
      ?EXIT(unknown_format);
	{'EXIT', Error} -> 
      ?when_option(verbose, Opts,?debug_untagged_msg("\n",[])),
      ?error_msg("ERROR: ~p~n",[Error]),
      ?EXIT(Error)
  end.

maybe_load(Mod, Bin, WholeModule, Opts) ->
  case property_lists:get_bool(load, Opts) of
	false ->
      ok;
    true ->
      ?when_option(verbose, Opts, ?debug_msg("Loading/linking\n", [])),
      do_load(Mod, Bin, WholeModule)
  end.

do_load(Mod, Bin, WholeModule) ->
  HostArch = get(hipe_host_arch),
  TargetArch = get(hipe_target_arch),

  %% Make sure we can do the load.
  if HostArch =/= TargetArch ->
      ?EXIT({host_and_target_arch_differ,HostArch,TargetArch});
     true -> ok
  end,

  case WholeModule of 
    false ->
      case get(hipe_host_arch) of
	ultrasparc ->
	  hipe_sparc_loader:load(Mod, Bin);
	x86 ->
	  hipe_x86_loader:load(Mod, Bin);
	Arch ->
	  ?EXIT({executing_on_an_unsupported_architecture,Arch})
      end;
    _ ->
      case code:is_sticky(Mod) of
	true ->
	  %% Don't load sticky mods.
	  %?error_msg("Can't load module that resides in sticky dir\n",[]),
	  %?EXIT({error,sticky_directory,Mod});

	  %% Just don't purge stick mods.
	  ok;
	_ ->
	  code:purge(Mod),
	  code:delete(Mod)
      end,
	  
      case get(hipe_host_arch) of
	ultrasparc ->
	  hipe_sparc_loader:load_module(Mod, Bin, WholeModule);
	x86 ->
	  hipe_x86_loader:load_module(Mod, Bin, WholeModule);
	Arch ->
	  ?EXIT({executing_on_an_unsupported_architecture,Arch})

      end
  end.
      

assemble(CompiledCode, Closures, Options) ->
  case get(hipe_target_arch) of
    ultrasparc ->
      %% XXX: The "saver" should really be called an assembler now
      hipe_sparc_saver:save(CompiledCode, Closures, Options);
    x86 ->
      x86_assemble(CompiledCode, Closures, Options);
    Arch ->
      ?EXIT({executing_on_an_unsupported_architecture,Arch})
  end.

%%% use option no_assemble_x86 to disable calling hipe_x86_assemble
x86_assemble(Code, Closures, Options) ->
  case property_lists:get_bool(no_assemble_x86, Options) of
    true ->
      Code;
    false ->
      hipe_x86_assemble:assemble(Code, Closures, Options)
  end.

%% ____________________________________________________________________
%% 
%% Prepare the compiler process for compiling MFA with Options.
%% Set up options and variables which are accessed globally.
%%
init(Options) ->
  %% `hipe_internal' must be loaded to compile gen_server
  code:ensure_loaded(hipe_internal),
  %% initialise host and target architecture target defaults to
  %% host, but can be overridden by passing in a {target,Target}
  %% option
  put(hipe_host_arch, erlang:system_info(hipe_architecture)),
  put(hipe_target_arch,
      property_lists:get_value(target,
			       Options,
			       get(hipe_host_arch))),
  %% Initialise some counters used for measurements and
  %% benchmarking.  If the option measure_regalloc is given the
  %% compilation will return a keylist with the counter values.
  [?set_hipe_timer_val(Timer,0) || Timer <- hipe_timers()],
  [case Counter of
     {CounterName, InitVal} -> put(CounterName, InitVal);
     CounterName  -> put(CounterName,0)
   end
   || Counter <- property_lists:get_value(counters,Options++[{counters,[]}])],

  put(regalloctime,0),
  put(totalspill,{0,0}),
  put(spilledtemps,0),
  put(pre_ra_instrs,0),
  put(post_ra_instrs,0),
  put(pre_ra_temps,0),
  put(post_ra_temps,0),
  put(noregs,0),
  put(bbs,0),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

post(Res, Options) ->
  TimerVals = 
    case property_lists:get_value(timers,Options) of
      Timers when list(Timers) ->
	[{Timer, ?get_hipe_timer_val(Timer)} || Timer <- Timers];
      _ -> []
    end,
  CounterVals = 
    case property_lists:get_value(counters,Options) of
      Counters when list(Counters) ->
	[case Counter of
	   {CounterName, InitVal} -> {CounterName, get(CounterName)};
	   CounterName -> {CounterName, get(CounterName)}
	 end
	 || Counter <- Counters];
      _ -> []
    end,
  Measures = 
    case property_lists:get_bool(measure_regalloc, Options) of
      true ->
	get();
      false -> []
    end,

  case TimerVals ++ CounterVals ++ Measures of
    [] ->
      Res;
    Info -> 
      {Res, Info}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

beam_file({M,F,A}) ->
  beam_file(M);
beam_file(Module) when atom(Module) ->
  case code:which(Module) of
    non_existing ->
      Module;
    File ->
      File
  end;
beam_file(Name) ->
  Name.


version() ->
  ?version().

%% ____________________________________________________________________
%% 
%% D O C U M E N T A T I O N   -   H E L P 
%%

help() ->
  {V1,V2,V3} = ?version(),
  io:format("The HiPE Compiler (Version ~w.~w.~w)\n" ++
	    " Functions:\n" ++
	    "   c(Name,Options)\n" ++ 
	    "     Compiles the module or function Name and loads\n" ++
	    "     it to memory. Name is an atom or a tuple {M,F,A}.\n" ++
	    "     Options is a list of option terms;\n" ++
	    "     use `help_options()' for details.\n" ++
	    "   c(Name)\n" ++
	    "     As above, but using only default options.\n" ++
	    "   c(Name,File,Options)\n" ++
	    "     As above, but reading BEAM code from File.\n" ++
	    "   f(File,Options)\n" ++ 
	    "     As c(Name,File,Options), but taking the module\n" ++
	    "     name from File.\n" ++
	    "   f(File)\n" ++ 
	    "     As above, but using only default options.\n" ++
	    "   compile(Name,Options)\n" ++
	    "     Compiles the module or function Name to a binary.\n" ++
	    "     By default, this does not load to memory.\n" ++
	    "   compile(Name)\n" ++ 
	    "     As above, but using only default options.\n" ++
	    "   compile(Name,File,Options)\n" ++
	    "     As above, but reading BEAM code from File.\n" ++
	    "   file(File,Options)\n" ++ 
	    "     As compile(Name,File,Options), but taking the\n" ++
	    "     module name from File.\n" ++
	    "   file(File)\n" ++ 
	    "     As above, but using only default options.\n" ++
	    "   load(Module)\n" ++
	    "     Loads the named module into memory.\n" ++
	    "   load(Module,Source)\n" ++
	    "     As above, but taking the code from Source.\n" ++
	    "     Source is either the name of a BEAM file\n" ++
	    "     or a binary containing native code.\n" ++
	    "   help()\n" ++
	    "     Prints this message.\n" ++
	    "   help_options()\n" ++
	    "     Prints a description of options used by the\n" ++
	    "     above functions.\n" ++
	    "   help_option(Option)\n" ++
	    "     Prints a description of that option.\n" ++
	    "   help_debug_options()\n" ++
	    "     Prints a description of debug options used by the\n" ++
	    "     above functions.\n" ++
	    "   version() ->\n" ++
	    "     Returns `{Major,Minor,Revision}'.\n",
	    [V1,V2,V3]),
  ok.

%% TODO: it should be possible to specify the target somehow when asking
%% for available options. Right now, you only see host machine options.

help_options() ->
  init([]), %% needed because of target-specific option expansion
  O1 = expand_options([o1]),
  O2 = expand_options([o2]),
  O3 = expand_options([o3]),
  io:format("HiPE Compiler Options\n" ++
	    " Boolean-valued options generally have corresponding " ++
	    "aliases `no_...',\n" ++
	    " and can also be specified as `{Name, true}' " ++
	    "or `{Name, false}.\n\n" ++
	    " General boolean options:\n" ++
	    "   ~p.\n\n" ++
	    " Non-boolean options:\n" ++
	    "   {'O', Level}, where 0 =< Level =< 3:\n" ++
	    "       Select optimization level (the default is 2).\n\n" ++
	    " Further options can be found below; " ++
	    "use `hipe:help_option(Name)' for details.\n\n" ++
	    " Aliases:\n" ++
	    "   pp_all = ~p,\n" ++
	    "   o0 = {'O',0},\n" ++
	    "   o1 = {'O',1} = ~p,\n" ++
	    "   o2 = {'O',2} = ~p ++ o1,\n" ++
	    "   o3 = {'O',3} = ~p ++ o2.\n",
	    [ordsets:from_list([verbose, debug, time, load, pp_beam,
				pp_icode, pp_rtl, pp_native, pp_x86,
				pp_sparc, pp_asm, timeout]),
	     expand_options([pp_all]),
	     O1 -- [{'O',1}],
	     (O2 -- O1) -- [{'O',2}],
	     (O3 -- O2) -- [{'O',3}]]),
  ok.

%% Documentation of the individual options.
%% If you add an option, please add help-text here.

option_text('O') ->
  "Specify optimization level. Used as {'O', LEVEL}.\n" ++
    "    At the moment levels 0 - 3 are implemented.\n" ++
    "    Aliases: o1, o2, o3, 'O1', 'O2', O3'.";
option_text(debug) ->
  "Outputs internal debugging information during compilation.";
option_text(fill_delayslot) ->
  "Try to optimize Sparc delay slots.";
option_text(icode_prop) ->
  "One pass of Icode optimization.";
option_text(icode_rename) ->
  "Rename variables (optimizes stack->reg mapping).";
option_text(load) ->
  "Automatically load the produced code into memory.\n" ++
    "    By default, this flag is `true'.";
option_text(pp_asm) ->
  "Displays assembly listing with addresses and bytecode.\n" ++
    "    Currently available for x86 only.";
option_text(pp_beam) ->
  "Display the input Beam code.";
option_text(pp_icode) ->
  "Display the intermediate HiPE-ICode.";
option_text(pp_native) ->
  "Display the generated native code.";
option_text(pp_rtl) ->
  "Display the intermediate HiPE-RTL code.";
option_text(regalloc) ->
  "Select register allocation algorithm. Used as {regalloc, METHOD}.\n" ++
    "    Currently available methods:\n" ++
    "        naive - spills everything (for debugging and testing).\n" ++
    "        linear_scan - fast; not so good if few registers available.\n" ++
    "        graph_color - slow, but gives better performance.\n" ++
    "        coalescing - tries hard to use registers.";
option_text(remove_comments) ->
  "Strip comments from intermediate code.";
option_text(rtl_prop_1) ->
  "Perform RTL-level optimization early.";
option_text(rtl_prop_2) ->
  "Perform RTL-level optimization late.";
option_text(sparc_peephole) ->
  "Perform Sparc peephole optimization.";
option_text(trim_frame) ->
  "Minimize stack frames on Sparc.";
option_text(time) ->
  "Reports the compilation times for the different\n" ++
    "    stages of the compiler.";
option_text(timeout) ->
  "Specify compilation time limit in ms. Used as {timeout, LIMIT}.\n" ++
    "    The limit must be a nonnegative integer or the atom 'infinity'.\n" ++
    "    The current default limit is 15 minutes (900000 ms).";
option_text(use_indexing) ->
  "Use indexing for multiple-choice branch selection.";
option_text(verbose) ->
  "Output information about what is being done.";
option_text(_) ->
  [].

help_option(Opt) ->
  init([]), %% needed because of target-specific option expansion
  case expand_options([Opt]) of
    [Opt] ->
      Name = if tuple(Opt), size(Opt) >= 1 -> element(1, Opt);
		true -> Opt
	     end,
      case option_text(Name) of
	[] ->  
	  case lists:member(Name, opt_keys()) of
	    true ->
	      io:format("~w - Sorry, this option is not documented yet.\n",
			[Name]);
	    _ -> 
	      io:format("Unknown option ~p.\n", [Name])
	  end;
	Txt ->
	  io:fwrite("~w - ~s\n", [Name, Txt])
      end;
    Opts ->
      io:fwrite("This is an alias for: ~p.\n", [Opts])
  end.

help_debug_options() ->
  io:format("HiPE compiler debug options:\n" ++
	    "Might require that some modules have been compiled " ++ 
	    "with the debug flag.\n" ++
	    "   rtl_show_translation - Prints each step in the\n" ++
	    "                          translation from Icode to RTL\n",
	    []),
  ok.

%% 
has_hipe_code(Atom) when atom(Atom) ->
  has_hipe_code(atom_to_list(Atom));
has_hipe_code(File) ->
  case catch 
    lists:member($H, 
		 [ hd(ChunkName) ||
		   {ChunkName,_} <- element(3,beam_lib:all_chunks(File))]
		) of
    {'EXIT',_} ->
      exit({bad_beam_file,File});
    R -> R
  end.


%%
hipe_timers() ->
  [time_icode_rename,
   time_ra].

%% ____________________________________________________________________
%% 
%% Option expansion

%% These are currently in use, but not documented:
%%
%%     count_instrs:
%%     fill_delayslot:
%%     {hot, Functions}:
%%     hotness:
%%     icode_type:
%%     iproc_debug:
%%     {ls_order, Order}:
%%     {regalloc, Algorithm}:
%%     remove_comments
%%     sparc_estimate_block_time
%%     sparc_opt_cfg:
%%     sparc_peephole:
%%     sparc_post_schedule
%%     sparc_profile:
%%     sparc_rename:
%%     sparc_schedule
%%     timeregalloc:
%%     timers
%%     trim_frame
%%     old_straighten:
%%     safe
%%     use_indexing
%%     rtl_add_gc
%%     rtl_cse

%% Definitions:

opt_keys() ->
    ['O',
     no_assemble_x86,
     counters,
     count_instrs,
     count_spills,
     count_temps,
     debug,
     fill_delayslot,
     finalise_x86,
     frame_x86,
     hot,
     hotness,
     icode_bwd_cprop,
     icode_prop,
     icode_rename,
     icode_type,
     iproc_debug,
     ls_order,
     load,
     measure_regalloc,
     old_straighten,
     pp_all,
     pp_beam,
     pp_icode,
     pp_rtl,
     pp_native,
     pp_x86,
     pp_sparc,
     pp_asm,
     regalloc,
     remove_comments,
     rtl_add_gc,
     rtl_cse,
     rtl_prop_1,
     rtl_prop_2,
     rtl_show_translation,
     safe,
     sparc_estimate_block_time,
     sparc_peephole,
     sparc_post_schedule,
     sparc_profile,
     sparc_schedule,
     sparc_opt_cfg,
     sparc_rename,
     target,
     time,
     timeout,
     timeregalloc,
     timers,
     to_rtl,
     trim_frame,
     use_indexing,
     use_inline_atom_search,
     use_clusters,
     use_jumptable,
     verbose].


o1_opts() ->
  case get(hipe_target_arch) of
    ultrasparc ->
      [rtl_prop_2, sparc_peephole, fill_delayslot];
    x86 ->
      [rtl_prop_2];
    Arch ->
      ?EXIT({executing_on_an_unsupported_architecture,Arch})
  end.

o2_opts() ->
  [use_indexing, icode_prop, remove_comments, trim_frame | o1_opts()].

o3_opts() ->
  case get(hipe_target_arch) of
    ultrasparc ->
      [icode_rename,{regalloc,graph_color}| o2_opts()];
    x86 ->
      [icode_rename,{regalloc,coalescing} | o2_opts()];
    Arch ->
      ?EXIT({executing_on_an_unsupported_architecture,Arch})
  end.

opt_aliases() ->
  [{'O0', o0},
   {'O1', o1},
   {'O2', o2},
   {'O3', o3},
   {pp_sparc, pp_native},
   {pp_x86, pp_native}].

opt_negations() ->
  [{assemble_x86, no_assemble_x86},
   {no_debug, debug},
   {no_fill_delayslot, fill_delayslot},
   {no_finalise_x86, finalise_x86},
   {no_frame_x86, frame_x86},
   {no_icode_bwd_cprop, icode_bwd_cprop},
   {no_icode_prop, icode_prop},
   {no_icode_rename, icode_rename},
   {no_load, load},
   {no_old_straighten, old_straighten},
   {no_pp_all, pp_all},
   {no_pp_beam, pp_beam},
   {no_pp_icode, pp_icode},
   {no_pp_rtl, pp_rtl},
   {no_pp_native, pp_native},
   {no_remove_comments, remove_comments},
   {no_rtl_add_gc, rtl_add_gc},
   {no_rtl_cse, rtl_cse},
   {no_rtl_prop_1, rtl_prop_1},
   {no_rtl_prop_2, rtl_prop_2},
   {no_rtl_show_translation, rtl_show_translation},
   {no_sparc_estimate_block_time, sparc_estimate_block_time},
   {no_sparc_peephole, sparc_peephole},
   {no_sparc_post_schedule, sparc_post_schedule},
   {no_sparc_profile, sparc_profile},
   {no_sparc_schedule, sparc_schedule},
   {no_time, time},
   {no_trim_frame, trim_frame},
   {no_use_clusters, use_clusters},
   {no_use_inline_atom_search, use_inline_atom_search},
   {no_use_indexing, use_indexing}].

opt_pre_expansions() ->
  [{o0, [{'O', 0}]},
   {o1, [{'O', 1}]},
   {o2, [{'O', 2}]},
   {o3, [{'O', 3}]}].

opt_expansions() ->
  [{{'O', 1}, [{'O', 1} | o1_opts()]},
   {{'O', 2}, [{'O', 2} | o2_opts()]},
   {{'O', 3}, [{'O', 3} | o3_opts()]},
   {safe, [{regalloc,graph_color},old_straighten, no_use_indexing,
	   rtl_add_gc, no_trim_frame]},
   {pp_all, [pp_beam, pp_icode, pp_rtl, pp_native]}].

expand_options(Opts) ->
  property_lists:normalize(Opts, [{aliases, opt_aliases()},
				  {expand, opt_pre_expansions()},
				  {expand, opt_expansions()},
				  {negations, opt_negations()}]).

check_options(Opts) ->
  Keys = ordsets:from_list(opt_keys()),
  Used = ordsets:from_list(property_lists:get_keys(Opts)),
  case ordsets:subtract(Used, Keys) of
    [] ->
      ok;
    L ->
      ?warning_msg("Unknown options: ~p.\n", [L]),
      ok
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
