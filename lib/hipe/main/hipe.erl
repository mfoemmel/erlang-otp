%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%% Copyright (c) 1998 by Erik Johansson.  All Rights Reserved 
%% ====================================================================
%%  Filename : hipe.erl
%%  Module   : hipe
%%  Purpose  :  
%%  Notes    : 
%%  History  : * 1998-01-28 Erik Johansson (happi@csd.uu.se): Created.
%%  CVS      : $Id$
%% ====================================================================
%% @doc This is the direct interface to the HiPE compiler.
%%
%% <h3>Normal use</h3>
%%
%% <p>The normal way to native-compile an Erlang module using HiPE is to
%% include the atom <code>native</code> in the Erlang compiler options,
%% as in:
%%
%% <pre>    1> c(my_module, [native]).</pre></p>
%%
%% <p>Options to the HiPE compiler are then passed as follows:
%%
%% <pre>    1> c(my_module, [native,{hipe,Options}]).</pre></p>
%%
%% <p>For on-line help in the Erlang shell, call <a
%% href="#help-0"><code>hipe:help()</code></a>. Details on HiPE compiler
%% options are given by <a
%% href="#help_options-0"><code>hipe:help_options()</code></a>.</p>
%%
%% <h3>Using the direct interface - for advanced users only</h3>
%%
%% To compile a module or a specific function to native code and
%% automatically load the code into memory, call <a
%% href="#c-1"><code>hipe:c(Module)</code></a> or <a
%% href="#c-2"><code>hipe:c(Module, Options)</code></a>. Note that all
%% options are specific to the HiPE compiler. See the <a
%% href="#index">function index</a> for other compiler functions.
%%
%% <h3>Main Options</h3>
%%
%% Options are processed in the order they appear in the list; an
%% early option will shadow a later one.
%% <dl>
%%   <dt><code>o0, 'O0', o1, 'O1', o2, 'O2', o3, 'O3'</code></dt>
%%     <dd>Set optimization level (default 2).</dd>
%%
%%   <dt><code>{'O', N}</code></dt>
%%     <dd>Set optimization level to <code>N</code>.</dd>
%%
%%   <dt><code>load</code></dt>
%%     <dd>Automatically load the code into memory after compiling.</dd>
%%
%%   <dt><code>time</code></dt>
%%     <dd>Reports the compilation times for the different stages
%%     of the compiler. Call <a
%%     href="#help_option-1"><code>hipe:help_option(time)</code></a> for
%%     details.</dd>
%%
%%   <dt><code>{timeout, Time}</code></dt>
%%     <dd>Sets the time the compiler is allowed to use for the
%%     compilation. <code>Time</code> is time in ms or the atom
%%     <code>infinity</code> (the default).</dd>
%%
%%   <dt><code>verbose</code></dt>
%%     <dd>Make the HiPE compiler output information about what it is
%%     being done.</dd>
%% </dl>
%% 
%% <h3>Advanced Options</h3>
%%
%% Note: You can also specify <code>{Option, false}</code> to turn a
%% particular option off, or <code>{Option, true}</code> to force it on.
%% Boolean-valued (<code>true</code>/<code>false</code>) options also
%% have negative-form aliases, e.g. <code>no_load</code> = <code>{load,
%% false}</code>.
%%
%% <p><dl>
%%   <dt><code>debug</code></dt>
%%     <dd>Outputs internal debugging information during
%%     compilation.</dd>
%%
%%   <dt><code>icode_prop</code></dt>
%%     <dd>One pass of Icode optimization.</dd>
%%
%%   <dt><code>icode_ssa</code></dt>
%%     <dd>Runs the compiler with Static Single Assignment (SSA) form on
%%     the Icode level.</dd>
%%
%%   <dt><code>icode_ssa_copy_prop</code></dt>
%%     <dd>Performs copy propagation on the SSA form on
%%     the Icode level. Needs <code>icode_ssa</code> to be turned on </dd>
%%
%%   <dt><code>icode_type</code></dt>
%%     <dd>A type propagator on the icode level. 
%%     Needs <code>icode_ssa</code> to be turned on </dd>
%%
%%   <dt><code>pp_all</code></dt>
%%     <dd>Equivalent to <code>[pp_beam, pp_icode, pp_rtl,
%%     pp_native]</code>.</dd>
%%
%%   <dt><code>pp_asm</code></dt>
%%     <dd>Prints the assembly listing with addresses and bytecode.
%%     Currently available for x86 only.</dd>
%%
%%   <dt><code>pp_beam, {pp_beam, {file, File}}</code></dt>
%%     <dd>Display the input Beam code to stdout or file.</dd>
%%
%%   <dt><code>pp_icode, {pp_icode, {file, File}},
%%       {pp_icode, {only, Functions}}</code></dt>
%%     <dd>Pretty-print Icode intermediate code to stdout or file.</dd>
%%
%%   <dt><code>pp_native, {pp_native, {file, File}},
%%       {pp_native, {only, Functions}}</code></dt>
%%     <dd>Pretty-print native code to stdout or file.</dd>
%%
%%   <dt><code>pp_opt_icode, {pp_opt_icode, {file, File}},
%%       {pp_opt_icode, {only, Functions}}</code></dt>
%%     <dd>Pretty-print optimized Icode to stdout or file.</dd>
%%
%%   <dt><code>pp_rtl, {pp_rtl, {file, File}},
%%       {pp_rtl, {only, Functions}}</code></dt>
%%     <dd>Pretty-print RTL intermediate code to stdout or file.</dd>
%%
%%   <dt><code>regalloc</code></dt>
%%     <dd>Select register allocation algorithm. Used as
%%     <code>{regalloc, Method}</code>.
%%
%%     <p><code>Method</code> is one of the following:
%%     <ul>
%%       <li><code>naive</code>: spills everything (for debugging and
%%       testing only).</li>
%%       <li><code>linear_scan</code>: fast compilation; not so good if
%%       only few registers available.</li>
%%       <li><code>graph_color</code>: slower, but gives better
%%       performance.</li>
%%       <li><code>coalescing</code>: tries hard to use registers; can be
%%	 very slow, but typically results in code with best performance.</li>
%%     </ul></p></dd>
%%
%%   <dt><code>remove_comments</code></dt>
%%     <dd>Remove comments from intermediate code.</dd>
%%
%%   <dt><code>rtl_cse</code></dt>
%%     <dd>Performs common subexpression elimination on RTL. Also
%%	<code>{rtl_cse,CSE_type}</code>.
%%
%%     <p><code>CSE_type</code> is one of the following:
%%     <ul>
%%       <li><code>true</code>/<code>false</code>: normal CSE/no
%%       CSE</li>
%%       <li><code>local</code>: CSE per block</li>
%%       <li><code>ebb</code>: CSE per extended basic block</li>
%%       <li><code>global</code>: CSE by fixpoint iteration</li>
%%     </ul></p></dd>
%%
%%   <dt><code>rtl_prop_1</code></dt>
%%     <dd>First pass of RTL optimization.</dd>
%%
%%   <dt><code>rtl_prop_2</code></dt>
%%     <dd>Second pass of RTL optimization.</dd>
%%
%%   <dt><code>sparc_estimate_block_times</code></dt>
%%     <dd>Do not perform scheduling, but annotate with cycle
%%     estimates</dd>
%%
%%   <dt><code>sparc_estimate_block_times</code></dt>
%%     <dd>Do not perform scheduling, but annotate with cycle
%%     estimates</dd>
%%
%%   <dt><code>sparc_post_schedule</code></dt>
%%   <dd>Schedule after register allocation as well. There are two
%%   reasons for this:
%%   <ol>
%%     <li>Spill code (rare).</li>
%%     <li>Register allocation may reuse registers in nearby
%%     instructions, which can destroy our careful schedule;
%%     post-scheduling tries to repair the damage (if any; no difference
%%     has been observed so far).</li>
%%   </ol></dd>
%%
%%   <dt><code>sparc_profile</code></dt>
%%     <dd>Inserts profiling counters into code. You get an annotated
%%     CFG by <code>hipe_profile:annot(MFA)</code> (see also
%%     <code>misc/hipe_profile.erl</code> for more info). Also
%%     <code>{sparc_profile, Prof_type}</code>.
%%
%%     <p><code>Prof_type</code> is one of the following:
%%     <ul>
%%       <li><code>true</code>/<code>false</code>: normal
%%       profiling/no profiling</li>
%%       <li><code>block</code>: </li>
%%       <li><code>arc</code>: </li>
%%     </ul></p>
%%   </dd>
%%
%%   <dt><code>sparc_schedule</code></dt>
%%     <dd>Perform ILP scheduling on code (also annotates code with
%%     cycle estimates). Also <code>{sparc_schedule, Sched_type}</code>.
%%
%%     <p><code>Sched_type</code> is one of are the following:
%%     <ul>
%%        <li><code>true</code>/<code>false</code>: normal
%%        scheduling/no scheduling</li>
%%         <li><code>ultra</code>:</li>
%%     </ul></p>
%%   </dd>
%%
%%   <dt><code>use_indexing</code></dt>
%%     <dd>Use indexing for multiple-choice branch selection.</dd>
%% </dl></p>
%%
%% <h3>Debugging Options</h3>
%% (May require that some modules have been
%% compiled with the <code>DEBUG</code> flag.)
%% <dl>
%%   <dt><code>rtl_show_translation</code></dt>
%%     <dd>Prints each step in the translation from Icode to RTL</dd>
%% </dl>
%%
%% @end
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
	 compile/4,
 	 file/1,
 	 file/2,
	 load/1,
	 load/2,
	 help/0,
	 help_options/0,
	 help_option/1,
	 help_debug_options/0,
	 version/0,
	 has_hipe_code/1]).

-ifndef(DEBUG).
-define(DEBUG,true).
-endif.
-include("hipe.hrl").
-include("../rtl/hipe_literals.hrl").

-define(COMPILE_DEFAULTS, [o2]).
-define(DEFAULT_TIMEOUT, infinity).

%% @spec load(Mod) -> {module, Mod} | {error, Reason}
%%     Mod = mod()
%%     Reason = term()
%% 
%% @doc Like load/2, but tries to locate a BEAM file automatically.
%%
%% @see load/2

load(Mod) ->
    load(Mod, beam_file(Mod)).

%% @spec load(Mod, Bin) -> {module, Mod} | {error, Reason}
%%     Mod = mod()
%%     Reason = term()
%%     Bin = binary() | filename()
%%     filename() = term()
%%
%% @type mod() = atom(). A module name.
%% 
%% @doc User interface for loading code into memory. The code can be
%% given as a native code binary or as the file name of a BEAM file
%% which should contain a native-code chunk. If only the module name is
%% given (see <code>load/1</code>), the BEAM file is located
%% automatically.
%%
%% @see load/1

load(Mod, Bin) when is_binary(Bin) ->
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


-define(USER_DEFAULTS, [load]).


%% @spec c(Name) -> {ok, Name} | {error, Reason}
%%       Name = mod() | mfa()
%%       Reason = term()
%%
%% @equiv c(Name, [])

c(Name) ->
  c(Name, []).

%% @spec c(Name, options()) -> {ok, Name} | {error, Reason}
%%     Name = mod() | mfa()
%%     options() = [option()]
%%     option() = term()
%%     Reason = term()
%%
%% @type mfa(M::mod(),F::fun(),A::arity()) = {M,F,A}.
%%       A fully qualified function name.
%%
%% @type fun() = atom(). A function identifier.
%%
%% @type arity() = integer(). A function arity; always nonnegative.
%% 
%% @doc User-friendly native code compiler interface. Reads BEAM code
%% from the corresponding "Module<code>.beam</code>" file in the system
%% path, and compiles either a single function or the whole module to
%% native code. By default, the compiled code is loaded directly. See
%% above for documentation of options.
%%
%% @see c/1
%% @see c/3
%% @see f/2
%% @see compile/2

c(Name, Options) ->
  c(Name, beam_file(Name), Options).

%% @spec c(Name, File, options()) -> {ok, Name} | {error, Reason}
%%     Name = mod() | mfa()
%%     File = filename() | binary()
%%     Reason = term()
%%
%% @doc Like <code>c/2</code>, but reads BEAM code from the specified
%% <code>File</code>.
%%
%% @see c/2
%% @see f/2

c(Name, File, Opts) ->
  case compile(Name, File, user_compile_opts(Opts)) of
    {ok, _} ->
      {ok, Name};
    Other ->
      Other
  end.

%% @spec f(File) -> {ok, Name} | {error, Reason}
%%     File = filename() | binary()
%%     Name = mod()
%%     Reason = term()
%%
%% @equiv f(File, [])

f(File) ->
  f(File, []).

%% @spec f(File, options()) -> {ok, Name} | {error, Reason}
%%     File = filename() | binary()
%%     Name = mod()
%%     Reason = term()
%%
%% @doc Like <code>c/3</code>, but takes the module name from the
%% specified <code>File</code>. This always compiles the whole module.
%%
%% @see c/3

f(File, Opts) ->
  case file(File, user_compile_opts(Opts)) of
    {ok, Name, _} ->
      {ok, Name};
    Other ->
      Other
  end.

user_compile_opts(Opts) ->
  Opts ++ ?USER_DEFAULTS.


%% @spec compile(Name) -> {ok, Binary} | {error, Reason}
%%       Name = mod() | mfa()
%%       Binary = binary()
%%       Reason = term()
%% 
%% @equiv compile(Name, [])

compile(Name) ->
  compile(Name, []).

%% @spec compile(Name, options()) -> {ok, Binary} | {error, Reason}
%%       Name = mod() | mfa()
%%       Binary = binary()
%%       Reason = term()
%%
%% @doc Direct compiler interface, for advanced use. This just compiles
%% the named function or module, reading BEAM code from the
%% corresponding "Module<code>.beam</code>" file in the system path.
%% Returns <code>{ok, Binary}</code> if successful, or <code>{error,
%% Reason}</code> otherwise. By default, it does <em>not</em> load the
%% binary to memory (the <code>load</code> option can be used to
%% activate automatic loading). <code>File</code> can be either a file
%% name or a binary containing the BEAM code for the module.
%%
%% @see c/2
%% @see compile/1
%% @see compile/3
%% @see file/2
%% @see load/2

compile(Name, Options) ->
  compile(Name, beam_file(Name), Options).

beam_file({M,_F,_A}) ->
  beam_file(M);
beam_file(Module) when is_atom(Module) ->
  case code:which(Module) of
    non_existing ->
      ?error_msg("Cannot find .beam file.",[]),
      ?EXIT({cant_find_beam_file});
    File ->
      File
  end.

%% @spec compile(Name, File, options()) -> {ok, Binary} | {error, Reason}
%%       Name = mod() | mfa()
%%       File = filename() | binary()
%%       Binary = binary()
%%       Reason = term()
%% 
%% @doc Like <code>compile/2</code>, but reads BEAM code from the
%% specified <code>File</code>.
%%
%% @see compile/2

compile(Name, {[], File}, Opts0) ->
  compile(Name, File, Opts0);
compile(Name, {Core, File}, Opts0) ->
  compile(Name, Core, File, Opts0);
compile(Name, File, Opts0) ->
  DisasmFun = fun (_) -> hipe_beam_to_icode:disasm(File) end,
  IcodeFun = fun (Code, Opts) ->
		 get_beam_icode(Name, Code, File, Opts)
	     end,
  compile_1(Name, DisasmFun, IcodeFun, Opts0).

compile(Name, Core, File, Opts0) when atom(Name) ->
  DisasmFun = fun (_) -> {false, []} end,
  IcodeFun = fun (_, Opts) ->
		 get_core_icode(Name, Core, File, Opts)
	     end,
  compile_1(Name, DisasmFun, IcodeFun, Opts0).

compile_1(Name, DisasmFun, IcodeFun, Opts0) ->
  check_module(Name),
  Opts = expand_basic_options(Opts0 ++ ?COMPILE_DEFAULTS),
  ?when_option(verbose, Opts, ?debug_msg("Compiling: ~p\n",[Name])),
  ?option_start_time("Compile", Opts),
  Res = run_compiler(DisasmFun, IcodeFun, Opts),
  ?option_stop_time("Compile",Opts),
  Res.

check_module(hipe_internal) ->
  ?error_msg("Nope, can't do that\n",[]),
  ?EXIT({cant_compile_hipe_internal});
check_module(_) ->
  ok.

%% @spec file(File) -> {ok, Name, Binary} | {error, Reason}
%%       File = filename() | binary()
%%       Name = mod() | mfa()
%%       Binary = binary()
%%       Reason = term()
%% 
%% @equiv file(File, [])

file(File) ->
  file(File, []).

%% @spec file(File, options()) -> {ok, Name, Binary} | {error, Reason}
%%       File = filename() | binary()
%%       Name = mod() | mfa()
%%       Binary = binary()
%%       Reason = term()
%% 
%% @doc Like <code>compile/2</code>, but takes the module name from the
%% specified <code>File</code>. Returns both the name and the final
%% binary if successful.
%%
%% @see file/1
%% @see compile/2

file(File, Options) ->
  case beam_lib:info(File) of
    L when is_list(L) ->
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

get_beam_icode({M,_F,_A} = MFA, BeamCode, _File, Options) ->
  %% When compiling just one function, then the emulated code for the
  %% module must be loaded.
  code:ensure_loaded(M),
  ?option_time({ok,Icode} = 
	       (catch {ok,hipe_beam_to_icode:mfa(BeamCode, MFA, Options)}),
	       "BEAM-to-Icode", Options),
  {Icode, M, false};
get_beam_icode(Mod, BeamCode, File, Options) ->
  ?option_time({ok, Icode} =
	       (catch {ok, hipe_beam_to_icode:module(BeamCode, Options)}),
	       "BEAM-to-Icode", Options),
  BeamBin = get_beam_code(File),
  {Icode, Mod, BeamBin}.

get_core_icode(Mod, Core, File, Options) ->
  ?option_time({ok, Icode} =
	       (catch {ok, cerl_to_icode:module(Core, Options)}),
	       "BEAM-to-Icode", Options),
  BeamBin = get_beam_code(File),
  {Icode, Mod, BeamBin}.

get_beam_code(Bin) when is_binary(Bin) -> Bin;
get_beam_code(FileName)->
  case erl_prim_loader:get_file(FileName) of
    {ok,Bin,_} ->
      Bin;
    error ->
      ?EXIT(no_beam_file)
  end.

%% Note that this function receives the "basic" options.  The DisasmFun
%% and IcodeFun only collect the Icode. Most of the real work is done in
%% the 'finalize' function.

run_compiler(DisasmFun, IcodeFun, Options) ->
  %% Spawn and link in case the linked CompProc gets killed. Make sure
  %% the main process catches the signal if this happens.
  process_flag(trap_exit, false),
  Parent = self(),
  CompProc = spawn_link(
	       fun () ->
		   %% Compiler process
		   set_architecture(Options),
		   pre_init(Options),
		   {Code, CompOpts} = DisasmFun(Options),
		   Opts = expand_options(Options ++ CompOpts),
		   check_options(Opts),
		   ?when_option(verbose, Options,
				?debug_msg("Options: ~p.\n",[Opts])),
		   init(Opts),
		   {Icode, Mod, BeamBin} = IcodeFun(Code, Opts),
		   Result = compile_finish(Icode, Mod, BeamBin, Opts),
		   compiler_return(Result, Parent)
	       end),
  Timeout = case proplists:get_value(timeout, Options) of
	      N when is_integer(N), N >= 0 -> N;
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

compile_finish({'EXIT',Error}, _Mod, _WholeModule, _Options) ->
  {error, Error};
compile_finish(Icode, Mod, WholeModule, Options) ->
  Res = finalize(Icode, Mod, WholeModule, Options),
  post(Res, Options).


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
  case proplists:get_bool(to_rtl, Opts) of
    true ->
      {ok, CompiledCode};
    false ->
      Closures = 
	[MFA || {MFA, Icode} <- List,
		hipe_icode:icode_is_closure(Icode)],
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
  case proplists:get_bool(load, Opts) of
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
	  hipe_unified_loader:load(Mod, Bin);
	x86 ->
	  hipe_unified_loader:load(Mod, Bin);
	Arch ->
	  ?EXIT({executing_on_an_unsupported_architecture,Arch})
      end;
    _ ->
      %% workaround for what may be an x86 bug
      case get(hipe_host_arch) of
	x86 ->
	  %% x86 must run the old code for now
	  case code:is_sticky(Mod) of
	    true ->
	      %% Don't load sticky mods.
	      %%?error_msg("Can't load module that resides in sticky dir\n",[]),
	      %%?EXIT({error,sticky_directory,Mod});
	      %% Just don't purge stick mods.
	      ok;
	    _ ->
	      code:purge(Mod),
	      code:delete(Mod)
	  end,
	  case get(hipe_host_arch) of
	    ultrasparc ->
	      hipe_unified_loader:load_module(Mod, Bin, WholeModule);
	    x86 ->
	      hipe_unified_loader:load_module(Mod, Bin, WholeModule);
	    Arch ->
	      ?EXIT({executing_on_an_unsupported_architecture,Arch})
	  end;
	ultrasparc ->
	  %% the new code, which alas trips a bug on x86
	  case code:is_sticky(Mod) of
	    true ->
	      %% Don't purge or register sticky mods.
	      case get(hipe_host_arch) of
		ultrasparc ->
		  hipe_unified_loader:load_module(Mod, Bin, WholeModule);
		x86 ->
		  hipe_unified_loader:load_module(Mod, Bin, WholeModule)
	      end;
	    _ -> %% Not a sticky module.
	      Chunkname = case get(hipe_host_arch) of
			    ultrasparc ->"HS8P";
			    x86 -> "HX86"
			  end,
	      {ok, _, Chunks0} = beam_lib:all_chunks(WholeModule),
	      Chunks = [{Chunkname,Bin}| lists:keydelete(Chunkname,1,Chunks0)],
	      {ok, BeamPlusNative} = beam_lib:build_module(Chunks),
	      Res = code:load_binary(Mod,code:which(Mod),
				     BeamPlusNative),
	      Res
	  end
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

%% use option no_assemble_x86 to disable calling hipe_x86_assemble
x86_assemble(Code, Closures, Options) ->
  case proplists:get_bool(no_assemble_x86, Options) of
    true ->
      Code;
    false ->
      hipe_x86_assemble:assemble(Code, Closures, Options)
  end.

%% ____________________________________________________________________

%% Initialise host and target architectures. Target defaults to host,
%% but can be overridden by passing an option {target, Target}.

set_architecture(Options) ->
  put(hipe_host_arch, erlang:system_info(hipe_architecture)),
  put(hipe_target_arch,
      proplists:get_value(target, Options, get(hipe_host_arch))),
  ok.

%% This sets up some globally accessed stuff that are needed by the
%% compiler process before it even gets the full list of options.
%% Therefore, this expands the current set of options for local use.

pre_init(Opts) ->
  Options = expand_options(Opts),
  %% Initialise some counters used for measurements and benchmarking. If
  %% the option 'measure_regalloc' is given the compilation will return
  %% a keylist with the counter values.

  put(hipe_time,
      case proplists:get_value(time, Options,false) of
	true -> [hipe,hipe_main];
	OptTime -> OptTime
      end),
  [?set_hipe_timer_val(Timer,0) || Timer <- hipe_timers()],
  [case Counter of
     {CounterName, InitVal} -> put(CounterName, InitVal);
     CounterName  -> put(CounterName,0)
   end
   || Counter <- proplists:get_value(counters, Options, [])],
  
  put(hipe_debug,proplists:get_bool(debug, Options)),  
  put(hipe_inline_fp, proplists:get_bool(inline_fp, Options)),
  put(hipe_inline_bs, proplists:get_bool(inline_bs, Options)),
  ok.

%% Prepare the compiler process by setting up variables which are
%% accessed globally. Options have been fully expanded at ths point.

init(_Options) ->
  %% `hipe_internal' must be loaded to compile gen_server
  code:ensure_loaded(hipe_internal),

  put(regalloctime,0),
  put(callersavetime,0),
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
    case proplists:get_value(timers,Options) of
      Timers when is_list(Timers) ->
	[{Timer, ?get_hipe_timer_val(Timer)} || Timer <- Timers];
      _ -> []
    end,
  CounterVals = 
    case proplists:get_value(counters,Options) of
      Counters when is_list(Counters) ->
	[case Counter of
	   {CounterName, _InitVal} -> {CounterName, get(CounterName)};
	   CounterName -> {CounterName, get(CounterName)}
	 end
	 || Counter <- Counters];
      _ -> []
    end,
  Measures = 
    case proplists:get_bool(measure_regalloc, Options) of
      true ->
	get();  % return whole process dictionary list (simplest way...)
      false -> []
    end,

  case TimerVals ++ CounterVals ++ Measures of
    [] ->
      Res;
    Info -> 
      {Res, Info}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @spec version() -> {Major, Minor, Increment}
%% Major = integer()
%% Minor = integer()
%% Increment  = integer() 
%% @doc Returns the current HiPE version.
version() ->
  ?version().

%% ____________________________________________________________________
%% 
%% D O C U M E N T A T I O N   -   H E L P 
%%

%% @spec () -> ok
%% @doc Prints on-line documentation to the standard output.
help() ->
  {V1,V2,V3} = ?version(),
  io:format("The HiPE Compiler (Version ~w.~w.~w)\n" ++
	    " The normal way to native-compile Erlang code " ++
	    "using HiPE is to\n" ++
	    " include `native' in the Erlang compiler " ++
	    "options, as in:\n" ++
	    "     1> c(my_module, [native]).\n" ++
	    " Options to the HiPE compiler must then be passed " ++
	    "as follows:\n" ++
	    "     1> c(my_module, [native,{hipe,Options}]).\n" ++
	    " Use `help_options()' for details.\n" ++
	    " Utility functions:\n" ++
	    "   help()\n" ++
	    "     Prints this message.\n" ++
	    "   help_options()\n" ++
	    "     Prints a description of options recognized by the\n" ++
	    "     HiPE compiler.\n" ++
	    "   help_option(Option)\n" ++
	    "     Prints a description of that option.\n" ++
	    "   help_debug_options()\n" ++
	    "     Prints a description of debug options.\n" ++
	    "   version() ->\n" ++
	    "     Returns `{Major,Minor,Revision}'.\n" ++
	    " The following functions are for advanced users only!\n" ++
	    " Note that all options are specific to the HiPE compiler.\n" ++
	    "   c(Name,Options)\n" ++ 
	    "     Compiles the module or function Name and loads it\n" ++
	    "     to memory. Name is an atom or a tuple {M,F,A}.\n" ++
	    "   c(Name)\n" ++
	    "     As above, but using only default options.\n" ++
	    "   c(Name,File,Options)\n" ++
	    "     As above, but reading BEAM code from File.\n" ++
	    "   f(File,Options)\n" ++ 
	    "     As c(Name,File,Options), but taking the module name\n" ++
	    "     from File.\n" ++
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
	    "     or a binary containing native code.\n",
	    [V1,V2,V3]),
  ok.

%% TODO: it should be possible to specify the target somehow when asking
%% for available options. Right now, you only see host machine options.

%% @spec () -> ok
%% @doc Prints documentation about options to the standard output.
help_options() ->
  set_architecture([]), %% needed for target-specific option expansion
  O1 = expand_options([o1]),
  O2 = expand_options([o2]),
  O3 = expand_options([o3]),
  io:format("HiPE Compiler Options\n" ++
	    " Boolean-valued options generally have corresponding " ++
	    "aliases `no_...',\n" ++
	    " and can also be specified as `{Option, true}' " ++
	    "or `{Option, false}.\n\n" ++
	    " General boolean options:\n" ++
	    "   ~p.\n\n" ++
	    " Non-boolean options:\n" ++
	    "   {'O', Level}, where 0 =< Level =< 3:\n" ++
	    "       Select optimization level (the default is 2).\n\n" ++
	    " Further options can be found below; " ++
	    "use `hipe:help_option(Name)' for details.\n\n" ++
	    " Aliases:\n" ++
	    "   pp_all = ~p,\n" ++
	    "   pp_sparc = pp_native,\n" ++
	    "   pp_x86 = pp_native,\n" ++
	    "   o0 = {'O',0},\n" ++
	    "   o1 = {'O',1} = ~p,\n" ++
	    "   o2 = {'O',2} = ~p ++ o1,\n" ++
	    "   o3 = {'O',3} = ~p ++ o2.\n",
	    [ordsets:from_list([verbose, debug, time, load, pp_beam,
				pp_icode, pp_rtl, pp_native, pp_asm,
				timeout]),
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
option_text(icode_ssa) ->
  "Runs the compiler with Static Single Assignment (SSA) Form\n"++
  "on the Icode level.";
option_text(icode_ssa_check) ->
  "Checks wheter Icode is on SSA form or not\n";
option_text(load) ->
  "Automatically load the produced code into memory.";
option_text(pp_asm) ->
  "Displays assembly listing with addresses and bytecode.\n" ++
  "Currently available for x86 only.";
option_text(pp_beam) ->
  "Display the input Beam code.";
option_text(pp_icode) ->
  "Display the intermediate HiPE-ICode.";
option_text(pp_rtl) ->
  "Display the intermediate HiPE-RTL code.";
option_text(pp_native) ->
  "Display the generated native code.";
option_text(regalloc) ->
  "Select register allocation algorithm. Used as {regalloc, METHOD}.\n" ++
  "    Currently available methods:\n" ++
  "        naive - spills everything (for debugging and testing).\n" ++
  "        linear_scan - fast; not so good if few registers available.\n" ++
  "        graph_color - slow, but gives better performance.\n" ++
  "        coalescing - tries hard to use registers.";
option_text(remove_comments) ->
  "Strip comments from intermediate code.";
option_text(rtl_cse) ->
  "Common Subexpression Elimination on RTL.";
option_text(rtl_prop_1) ->
  "Perform RTL-level optimization early.";
option_text(rtl_prop_2) ->
  "Perform RTL-level optimization late.";
option_text(sparc_peephole) ->
  "Perform Sparc peephole optimization.";
option_text(sparc_prop) ->
  "Perform Sparc-level optimization.";
option_text(time) ->
  "Reports the compilation times for the different stages\n" ++
  "of the compiler.\n" ++
  "    {time, Module}       reports timings for the module Module.\n" ++
  "    {time, [M1, M2, M3]} reports timings for the specified modules.\n" ++
  "    {time, all}          reports timings all modules.\n" ++
  "    time                 reports timings for the main module.\n";
option_text(timeout) ->
  "Specify compilation time limit in ms. Used as {timeout, LIMIT}.\n" ++
  "    The limit must be a non-negative integer or the atom 'infinity'.\n" ++
  "    The current default limit is 15 minutes (900000 ms).";
option_text(use_indexing) ->
  "Use indexing for multiple-choice branch selection.";
option_text(verbose) ->
  "Output information about what is being done.";
option_text(_) ->
  [].

%% @spec (option()) -> ok
%% @doc Prints documentation about a specific option to the standard
%% output.
help_option(Opt) ->
  set_architecture([]), %% needed for target-specific option expansion
  case expand_options([Opt]) of
    [Opt] ->
      Name = if is_tuple(Opt), size(Opt) >= 1 -> element(1, Opt);
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

%% @spec () -> ok
%% @doc Prints documentation about debugging options to the standard
%% output.
help_debug_options() ->
  io:format("HiPE compiler debug options:\n" ++
	    "Might require that some modules have been compiled " ++ 
	    "with the debug flag.\n" ++
	    "   rtl_show_translation - Prints each step in the\n" ++
	    "                          translation from Icode to RTL\n",
	    []),
  ok.

%% @spec (Name) -> bool() 
%% Name = mod() | string()
%% @doc Returns true if the module or file contains native HiPE code.
has_hipe_code(Atom) when is_atom(Atom) ->
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
  [time_ra].

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
%%     safe
%%     use_indexing
%%     rtl_add_gc
%%     rtl_cse

%% Valid option keys. (Don't list aliases or negations - the check is
%% done after the options have been expanded to normal form.)

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
     icode_prop,
     icode_ssa,
     icode_ssa_check,
     icode_ssa_copy_prop,
     icode_type,
     inline_bs,
     inline_fp,
     iproc_debug,
     ls_order,
     load,
     measure_regalloc,
     pp_asm,
     pp_beam,
     pp_icode,
     pp_opt_icode,
     pp_typed_icode,
     pp_native,
     pp_rtl,
     regalloc,
     remove_comments,
     rtl_add_gc,
     rtl_cse,
     rtl_prop_1,
     rtl_prop_2,
     rtl_prop_3,
     rtl_show_translation,
     safe,
     sparc_estimate_block_time,
     sparc_peephole,
     sparc_post_schedule,
     sparc_profile,
     sparc_prop,
     sparc_schedule,
     sparc_opt_cfg,
     sparc_rename,
     target,
     time,
     timeout,
     timeregalloc,
     timers,
     to_rtl,
     use_indexing,
     use_inline_atom_search,
     use_clusters,
     use_jumptable,
     verbose].

%% Definitions: 

o1_opts() ->
  Common = [rtl_prop_2, inline_fp, inline_bs],
  case get(hipe_target_arch) of
    ultrasparc ->
      [sparc_peephole, fill_delayslot | Common];
    x86 ->
      Common;
    Arch ->
      ?EXIT({executing_on_an_unsupported_architecture,Arch})
  end.

o2_opts() ->
  Common = [icode_ssa, icode_type, icode_prop, icode_ssa_copy_prop,
	    use_indexing, remove_comments | o1_opts()],
  %% KOSTIS: The following do not work currently -- why?
  % Common = [rtl_cse | Common],
  % Common = [{rtl_cse,global} | Common],
  case get(hipe_target_arch) of
    ultrasparc ->
      [sparc_prop | Common];
    x86 ->
      Common;
    Arch -> 
      ?EXIT({executing_on_an_unsupported_architecture,Arch})
  end.

o3_opts() ->
  Common = [{regalloc,coalescing}, rtl_prop_3 | o2_opts()],
  case get(hipe_target_arch) of
    ultrasparc ->
      Common;
    x86 ->
      Common;
    Arch ->
      ?EXIT({executing_on_an_unsupported_architecture,Arch})
  end.

%% Note that in general, the normal form for options should be positive.
%% This is a good programming convention, so that tests in the code say
%% "if 'x' ..." instead of "if not 'no_x' ...".

opt_negations() ->
  [{assemble_x86, no_assemble_x86},    % normal form is negative!
   {no_debug, debug},
   {no_fill_delayslot, fill_delayslot},
   {no_finalise_x86, finalise_x86},
   {no_frame_x86, frame_x86},
   {no_icode_prop, icode_prop},
   {no_icode_ssa, icode_ssa},
   {no_icode_ssa_check, icode_ssa_check},
   {no_icode_ssa_copy_prop, icode_ssa_copy_prop},
   {no_icode_type, icode_type},
   {no_inline_bs, inline_bs},
   {no_inline_fp, inline_fp},
   {no_load, load},
   {no_pp_beam, pp_beam},
   {no_pp_icode, pp_icode},
   {no_pp_opt_icode, pp_opt_icode},
   {no_pp_typed_icode, pp_typed_icode},
   {no_pp_rtl, pp_rtl},
   {no_pp_native, pp_native},
   {no_remove_comments, remove_comments},
   {no_rtl_add_gc, rtl_add_gc},
   {no_rtl_cse, rtl_cse},
   {no_rtl_prop_1, rtl_prop_1},
   {no_rtl_prop_2, rtl_prop_2},
   {no_rtl_prop_3, rtl_prop_3},
   {no_rtl_show_translation, rtl_show_translation},
   {no_sparc_estimate_block_time, sparc_estimate_block_time},
   {no_sparc_peephole, sparc_peephole},
   {no_sparc_post_schedule, sparc_post_schedule},
   {no_sparc_profile, sparc_profile},
   {no_sparc_prop, sparc_prop},
   {no_sparc_schedule, sparc_schedule},
   {no_time, time},
   {no_use_clusters, use_clusters},
   {no_use_inline_atom_search, use_inline_atom_search},
   {no_use_indexing, use_indexing}].

%% Don't use negative forms in right-hand sides of aliases and
%% expansions! We only expand negations once, before the other
%% expansions are done.

opt_aliases() ->
  [{'O0', o0},
   {'O1', o1},
   {'O2', o2},
   {'O3', o3},
   {pp_sparc, pp_native},
   {pp_x86, pp_native}].

opt_basic_expansions() ->
  [{pp_all, [pp_beam, pp_icode, pp_rtl, pp_native]}].

opt_pre_expansions() ->
  [{o0, [{'O', 0}]},
   {o1, [{'O', 1}]},
   {o2, [{'O', 2}]},
   {o3, [{'O', 3}]}].

opt_expansions() ->
  [{{'O', 1}, [{'O', 1} | o1_opts()]},
   {{'O', 2}, [{'O', 2} | o2_opts()]},
   {{'O', 3}, [{'O', 3} | o3_opts()]},
   {safe, [{regalloc,graph_color}, no_use_indexing, rtl_add_gc]}
   ].

%% This expands "basic" options, which may be tested early and cannot be
%% in conflict with options found in the source code.

expand_basic_options(Opts) ->
  proplists:normalize(Opts, [{negations, opt_negations()},
			     {aliases, opt_aliases()},
			     {expand, opt_basic_expansions()}]).

%% Note that set_architecture/1 must be called first, and that the given
%% list should contain the total set of options, since things like 'o2'
%% are expanded here. Basic expansions are processed here also, since
%% this function is called from the help-functions.

expand_options(Opts) ->
  proplists:normalize(Opts, [{negations, opt_negations()},
			     {aliases, opt_aliases()},
			     {expand, opt_basic_expansions()},
			     {expand, opt_pre_expansions()},
			     {expand, opt_expansions()}]).

check_options(Opts) ->
  Keys = ordsets:from_list(opt_keys()),
  Used = ordsets:from_list(proplists:get_keys(Opts)),
  case ordsets:subtract(Used, Keys) of
    [] ->
      ok;
    L ->
      ?warning_msg("Unknown options: ~p.\n", [L]),
      ok
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
