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
%%   <dt><code>icode_ssa_copy_prop</code></dt>
%%     <dd>Performs copy propagation on the SSA form on the Icode
%%     level.</dd>
%%
%%   <dt><code>icode_ssa_const_prop</code></dt>
%%     <dd>Performs sparse conditional constant propagation on the SSA
%%     form on the Icode level. </dd>
%%
%%   <dt><code>icode_type</code></dt>
%%     <dd>A type propagator on the Icode level. </dd>
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
%%   <dt><code>rtl_ssa_const_prop</code></dt>
%%     <dd>Performs sparse conditional constant propagation on the SSA
%%     form on the RTL level. </dd>
%%
%%   <dt><code>rtl_cse</code></dt>
%%     <dd>Performs common subexpression elimination on RTL. Also
%%	<code>{rtl_cse,CSE_type}</code>.
%%      <p><code>CSE_type</code> is one of the following:
%%      <ul>
%%        <li><code>true</code>/<code>false</code>: normal CSE/no CSE</li>
%%        <li><code>local</code>: CSE per block</li>
%%        <li><code>ebb</code>: CSE per extended basic block</li>
%%        <li><code>global</code>: CSE by fixpoint iteration</li>
%%      </ul></p></dd>
%%
%%   <dt><code>rtl_prop</code></dt>
%%     <dd>Propagation of constants on RTL.</dd>
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
%%
%%   <dt><code>use_callgraph</code></dt>
%%     <dd>Use a static call-graph for determining the order in which
%%         the functions of a module should be compiled (in reversed 
%%         topological sort order).</dd>
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
%% ====================================================================

-module(hipe).

-export([c/1,
	 c/2,
 	 f/1,
 	 f/2,
	 compile/1,
	 compile/2,
	 compile/4,
	 compile_core/4,
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
  Architecture = erlang:system_info(hipe_architecture),
  ChunkName = hipe_unified_loader:chunk_name(Architecture),
  case beam_lib:chunks(File, [ChunkName]) of
    {ok,{_,[{_,Bin}]}} -> do_load(Mod, Bin, File);
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
%% @type mfa() = {M::mod(),F::fun(),A::arity()}.
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
      ?error_msg("Cannot find ~w.beam file.",[Module]),
      ?EXIT({cant_find_beam_file,Module});
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

compile(Name, File, Opts) ->
  case proplists:get_value(core, Opts) of
    true when is_binary(File) ->
      ?error_msg("Cannot get Core Erlang code from BEAM binary.",[]),
      ?EXIT({cant_compile_core_from_binary});
    true ->
      case filename:find_src(filename:rootname(File, ".beam")) of
	{error, _} ->
	  ?error_msg("Cannot find source code for ~p.",[File]),
	  ?EXIT({cant_find_source_code});
	{Source, CompOpts} ->
	  CoreOpts = [X || X = {core_transform, _} <-Opts],
	  %%io:format("Using: ~w\n", [CoreOpts]),
	  case compile:file(Source, CoreOpts ++ [to_core, binary | CompOpts]) of
	    {ok, _, Core} ->
	      compile_core(Name, Core, File, Opts);
	    Error ->
	      ?error_msg("Error compiling ~p:\n~p.",[File, Error]),
	      ?EXIT({cant_compile_source_code})
	  end
      end;
    {src_file, Source} ->
      CoreOpts1 = [X || X = {core_transform, _} <-Opts],
      CoreOpts2 = [report_errors, to_core, binary, {i, "../include"}|CoreOpts1],
      %%io:format("Using: ~w\n", [CoreOpts2]),
      case compile:file(Source, CoreOpts2) of
	{ok, _, Core} ->
	  compile_core(Name, Core, File, Opts);
	Error ->
	  ?error_msg("Error compiling ~p:\n~p\n",[Source, Error]),
	  ?EXIT({cant_compile_source_code, Error})
      end;
    Other when Other == false; Other == undefined ->
      NewOpts =
	case proplists:get_value(use_callgraph, Opts) of
	  No when No == false; No == undefined -> Opts;
	  _ ->
	    case Name of
	      {_M,_F,_A} ->
		?WARNING_MSG("The option use_callgraph cannot be used when "
			     "compiling a single function\n", []),
		[no_use_callgraph|Opts];
	      _ -> Opts
	    end
	end,
      DisasmFun = fun (_) -> disasm(File) end,
      IcodeFun = fun (Code, Opts_) ->
		     get_beam_icode(Name, Code, File, Opts_)
		 end,
      run_compiler(Name, DisasmFun, IcodeFun, NewOpts)
  end.

compile_core(Name, Core0, File, Opts) ->
  Core = cerl:from_records(Core0),
  Core1 = case (erlang:system_info(heap_type) == hybrid)
	    andalso proplists:get_bool(hybrid, Opts) of
	    true -> cerl_hybrid_transform:transform(Core, Opts);
	    false -> Core
	  end,
  compile(Name, Core1, File, Opts).

%% @spec compile(Name, Core, File, options()) ->
%%           {ok, Binary} | {error, Reason}
%%       Name = mod()
%%       Core = coreErlang() | []
%%       File = filename() | binary()
%%       Binary = binary()
%%       Reason = term()
%% 
%% @doc Like <code>compile/3</code>, but unless <code>Core</code> is
%% <code>[]</code>, low-level code is generated from the given Core
%% Erlang code instead of from the BEAM code.
%%
%% <p>Note that only whole modules can be compiled with this
%% function.</p>
%%
%% @see compile/3

compile(Name, [], File, Opts) ->
  compile(Name, File, Opts);
compile({M,F,A}, _Core, _File, _Opts) ->
  ?WARNING_MSG("Cannot compile single functions from source code. (~p}.\n",[{M,F,A}]),
  {ok, <<>>};  % dummy
%  compile(M, Core, File, Opts);
compile(Name, Core, File, Opts) when atom(Name) ->
  DisasmFun = fun (_) -> {false, []} end,
  IcodeFun = fun (_, Opts) ->
		 get_core_icode(Name, Core, File, Opts)
	     end,
  run_compiler(Name, DisasmFun, IcodeFun, Opts).

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


%%-----------------------------------------------------------------------
%% The rest are internal functions:
%%-----------------------------------------------------------------------

%% @doc
%% Get BEAM code from `.beam' files or direct from binaries.
%%   File is either a file name or a binary containing the BEAM code.

disasm(File) ->
  case beam_disasm:file(File) of
    {beam_file,DisasmBeam} ->
      {value,{code,BeamCode}} = lists:keysearch(code,1,DisasmBeam),
      {value,{exports,BeamExports}} = lists:keysearch(exports,1,DisasmBeam),
      {value,{comp_info,CompInfo}} = lists:keysearch(comp_info,1,DisasmBeam),
      {value,{options,CompOpts}} = lists:keysearch(options,1,CompInfo),
      HCompOpts = case lists:keysearch(hipe,1,CompOpts) of
		    {value,{hipe,L}} when is_list(L) -> L;
		    {value,{hipe,X}} -> [X];
		    _ -> []
		  end,
      %% XXX: get module name too somehow
      Exports = fix_beam_exports(BeamExports),
      {{BeamCode, Exports}, HCompOpts};
    Error ->
      io:format("~s\n", [beam_lib:format_error(Error)]),
      ?EXIT(no_beam_code)
  end.

fix_beam_exports(BeamExports) ->
  fix_beam_exports(BeamExports, []).
fix_beam_exports([{F,A,_}|BeamExports], Exports) ->
  fix_beam_exports(BeamExports, [{F,A} | Exports]);
fix_beam_exports([], Exports) ->
  Exports.

get_beam_icode({M,_F,_A} = MFA, {BeamCode, Exports}, _File, Options) ->
  ?option_time({ok,Icode} = 
	       (catch {ok,hipe_beam_to_icode:mfa(BeamCode, MFA, Options)}),
	       "BEAM-to-Icode", Options),
  {{M, Exports, Icode}, false};
get_beam_icode(Mod, {BeamCode, Exports}, File, Options) ->
  ?option_time({ok, Icode} =
	       (catch {ok, hipe_beam_to_icode:module(BeamCode, Options)}),
	       "BEAM-to-Icode", Options),
  BeamBin = get_beam_code(File),
  {{Mod, Exports, Icode}, BeamBin}.

get_core_icode(Mod, Core, File, Options) ->
  ?option_time({ok, Icode} =
	       (catch {ok, cerl_to_icode:module(Core, Options)}),
	       "BEAM-to-Icode", Options),
  NeedBeamCode = 
    proplists:get_bool(type_only, Options) 
    orelse (not proplists:get_bool(load, Options)),

  BeamBin = 
    case NeedBeamCode of
      true -> [];
      false -> get_beam_code(File)
    end,
  Exports = [],   %% FIXME: get list of exports from core code
  {{Mod, Exports, Icode}, BeamBin}.

get_beam_code(Bin) when is_binary(Bin) -> Bin;
get_beam_code(FileName) ->
  case erl_prim_loader:get_file(FileName) of
    {ok,Bin,_} ->
      Bin;
    error ->
      ?EXIT(no_beam_file)
  end.


%% ---------------------------------------------------------------------
%% All compilations go through this function. Note that it receives only
%% "basic" options. Name is just used for verbosity. The DisasmFun and
%% IcodeFun only collect the Icode; most of the real work is done in the
%% 'finalize' function.

run_compiler(Name, DisasmFun, IcodeFun, Opts0) ->
  Opts = expand_basic_options(Opts0 ++ ?COMPILE_DEFAULTS),
  ?when_option(verbose, Opts, ?debug_msg("Compiling: ~p\n",[Name])),
  ?option_start_time("Compile", Opts),
  Res = run_compiler_1(DisasmFun, IcodeFun, Opts),
  ?option_stop_time("Compile", Opts),
  Res.

run_compiler_1(DisasmFun, IcodeFun, Options) ->
  Parent = self(),
  {trap_exit,TrapExit} = process_info(Parent, trap_exit),
  %% Spawn a compilation process CompProc. In case this process gets
  %% killed, the trap_exit flag is restored to that of the Parent process.
  process_flag(trap_exit, true),
  CompProc = spawn_link(fun () ->
			    %% Compiler process
			    set_architecture(Options),
			    pre_init(Options),
			    %% The full option expansion is not done
			    %% until the DisasmFun returns.
			    {Code, CompOpts} = DisasmFun(Options),
			    Opts = expand_options(Options ++ CompOpts),
			    check_options(Opts),
			    ?when_option(verbose, Options,
					 ?debug_msg("Options: ~p.\n",[Opts])),
			    init(Opts),
			    {Icode, WholeModule} = IcodeFun(Code, Opts),
			    CompRes = compile_finish(Icode, WholeModule, Opts),
			    compiler_return(CompRes, Parent)
			end),
  Timeout = case proplists:get_value(timeout, Options) of
	      N when is_integer(N), N >= 0 -> N;
	      undefined -> ?DEFAULT_TIMEOUT;
	      infinity -> infinity;
	      Other ->
		?WARNING_MSG("Bad timeout value: ~P\n"
			     "Using default timeout limit.\n",	
			     [Other, 5]),
		?DEFAULT_TIMEOUT
	    end,
  receive
    {'EXIT', CompProc, normal} -> ok;
    {'EXIT', CompProc, Reason} -> exit(Reason)
  after Timeout ->
      %% Kill the compilation process
      exit(CompProc, kill),
      receive {'EXIT', CompProc, _} -> ok end,
      flush(),
      ?error_msg("ERROR: Compilation timed out.\n",[]),
      exit(timed_out)
  end,
  Result = receive {CompProc, Res} -> Res end,
  process_flag(trap_exit, TrapExit),
  Result.

flush() ->
  receive
    _ -> flush()
  after 0 ->
      ok
  end.

compiler_return(Res, Client) ->
  Client ! {self(), Res}.

compile_finish({Mod, Exports, Icode}, WholeModule, Options) ->
  Res = finalize(Icode, Mod, Exports, WholeModule, Options),
  post(Res, Icode, Options).


%% ---------------------------------------------------------------------
%% finalize/4 compiles, assembles, and optionally loads a list of `{MFA,
%% Icode}' pairs, and returns `{ok, Binary}' or `{error, Reason}'.

%% TODO: make the Exports info accessible to the compilation passes.
finalize(List, Mod, Exports, WholeModule, Opts) ->
  {T1Compile,_} = erlang:statistics(runtime),
  CompiledCode =
    case proplists:get_value(use_callgraph, Opts) of
      true -> 
	%% Compiling the functions bottom-up by using a call graph
	CallGraph = hipe_icode_callgraph:construct(List),
	OrdList = hipe_icode_callgraph:to_list(CallGraph),
	[finalize_fun({MFA, Icode}, Opts)
	 || {MFA, Icode} <- OrdList];
      fixpoint ->
	CallGraph = hipe_icode_callgraph:construct(List),
	finalize_fun_fixpoint(CallGraph, Opts);
      _ -> 
	%% Compiling the functions bottom-up by reversing the list
	OrdList = lists:reverse(List),
	[finalize_fun({MFA, Icode}, Opts)
	 || {MFA, Icode} <- OrdList]
    end,
  {T2Compile,_} = erlang:statistics(runtime),
  ?when_option(verbose, Opts,
	       ?debug_msg("Compiled ~p in ~.2f s\n",
			  [Mod,(T2Compile-T1Compile)/1000])),
  case proplists:get_bool(type_only, Opts) of
    true ->
      {ok, Mod};
    false ->
      case proplists:get_bool(to_rtl, Opts) of
	true ->
	  {ok, CompiledCode};
	false ->
	  Closures = 
	    [MFA || {MFA, Icode} <- List,
		    hipe_icode:icode_is_closure(Icode)],
	  {T1,_} = erlang:statistics(runtime),
	  ?when_option(verbose, Opts, ?debug_msg("Assembling ~w",[Mod])),
	  case catch assemble(CompiledCode, Closures, Exports, Opts) of
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
      end
  end.

finalize_fun({MFA, Icode}, Opts) ->
  {T1,_} = erlang:statistics(runtime),
  ?when_option(verbose, Opts, ?debug_msg("Compiling ~w",[MFA])),
  case catch hipe_main:compile_icode(MFA, Icode, Opts) of
    {native, Platform, {unprofiled,Code}} ->
      {T2,_} = erlang:statistics(runtime),
      ?when_option(verbose, Opts,
		   ?debug_untagged_msg(" in ~.2f s\n", [(T2-T1)/1000])),
      case Platform of
	ultrasparc -> {Entry,Ct} = Code, {MFA,Entry,Ct};
	powerpc -> {MFA, Code};
	x86 -> {MFA, Code};
        amd64 -> {MFA, Code}
      end;
    {rtl, LinearRtl} ->
      io:format("~p~n", [LinearRtl]),
      {MFA, LinearRtl};
    {type_only, Fixpoint} ->
      {MFA, Fixpoint};
    {native, X} ->
      ?error_msg("ERROR: unknown native code format: ~P.\n",[X]),
      ?EXIT(unknown_format);
    {'EXIT', Error} -> 
      ?when_option(verbose, Opts,?debug_untagged_msg("\n",[])),
      ?error_msg("ERROR: ~p~n",[Error]),
      ?EXIT(Error)
  end.

finalize_fun_fixpoint(CallGraph, Opts) ->
  finalize_fun_fixpoint(CallGraph, Opts, []).

finalize_fun_fixpoint(CallGraph, Opts, Acc) ->
  case hipe_icode_callgraph:is_empty(CallGraph) of
    true ->
      lists:flatten(lists:reverse(Acc));
    false ->
      case hipe_icode_callgraph:take_first(CallGraph) of
	{[SCC], NewCallGraph} ->
	  %% One function
	  finalize_fun_fixpoint(NewCallGraph, Opts, 
				[finalize_fun(SCC, Opts)|Acc]);
	{SCC, NewCallGraph} ->
	  %% Ensure that this is only used with the option type_only
	  Res = dets_fixpoint_loop(SCC, [type_only|Opts]),
	  finalize_fun_fixpoint(NewCallGraph, Opts, [Res|Acc])
      end
  end.

dets_fixpoint_loop(SCC, Opts) ->
  case proplists:get_value(type_warnings, Opts) of
    {pid, _Pid} ->
      %% Ensure warnings are only sent once.
      TmpOpts = proplists:normalize([no_type_warnings|Opts], 
				    [{negations, opt_negations()}]),
      {_Res, Redo} = dets_fixpoint_loop(SCC, false, TmpOpts, []),
      case Redo of
	true ->
	  dets_fixpoint_loop(SCC, Opts);
	false -> 
	  %% Additional pass to emit warnings.
	  {Res1, _} = dets_fixpoint_loop(SCC, false, Opts, []),
	  Res1
      end;
    _ ->
      {Res, Redo} = dets_fixpoint_loop(SCC, false, Opts, []),
      case Redo of
	true ->
	  dets_fixpoint_loop(SCC, Opts);
	false -> 
	  Res
      end
  end.

dets_fixpoint_loop([Fun = {MFA, _}|Left], Redo, Opts, Acc) ->
  Res = finalize_fun(Fun, Opts),
  case Res of
    {MFA, fixpoint} ->
      dets_fixpoint_loop(Left, Redo, Opts, [Res|Acc]);
    {MFA, not_fixpoint} -> 
      dets_fixpoint_loop(Left, true, Opts, [Res|Acc])
  end;
dets_fixpoint_loop([], Redo, _Opts, Acc) ->
  {Acc, Redo}.


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
      %% In this case, the emulated code for the module must be loaded.
      code:ensure_loaded(Mod),
      hipe_unified_loader:load(Mod, Bin);
    _ ->
      case code:is_sticky(Mod) of
	true ->
	  %% Don't purge or register sticky mods; just load native.
	  hipe_unified_loader:load_module(Mod, Bin, WholeModule);
	false ->
	  %% Normal loading of a whole module
	  Architecture = erlang:system_info(hipe_architecture),
	  ChunkName = hipe_unified_loader:chunk_name(Architecture),
	  {ok,_,Chunks0} = beam_lib:all_chunks(WholeModule),
	  Chunks = [{ChunkName,Bin}|lists:keydelete(ChunkName,1,Chunks0)],
	  {ok,BeamPlusNative} = beam_lib:build_module(Chunks),
	  code:load_binary(Mod,code:which(Mod),BeamPlusNative)
      end
  end.

assemble(CompiledCode, Closures, Exports, Options) ->
  case get(hipe_target_arch) of
    ultrasparc ->
      hipe_sparc_assemble:assemble(CompiledCode, Closures, Exports, Options);
    powerpc ->
      hipe_ppc_assemble:assemble(CompiledCode, Closures, Exports, Options);
    x86 ->
      hipe_x86_assemble:assemble(CompiledCode, Closures, Exports, Options);
    amd64 ->
      hipe_amd64_assemble:assemble(CompiledCode, Closures, Exports, Options);
    Arch ->
      ?EXIT({executing_on_an_unsupported_architecture,Arch})
  end.

%% --------------------------------------------------------------------

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
      case proplists:get_value(time, Options, false) of
	true -> [hipe,hipe_main];
	OptTime -> OptTime
      end),
  [?set_hipe_timer_val(Timer,0) || Timer <- hipe_timers()],
  [case Counter of
     {CounterName,InitVal} -> put(CounterName, InitVal);
     CounterName  -> put(CounterName, 0)
   end
   || Counter <- proplists:get_value(counters, Options, [])],
  
  put(hipe_debug,     proplists:get_bool(debug, Options)),  
  put(hipe_inline_fp, proplists:get_bool(inline_fp, Options)),
  ok.

%% Prepare the compiler process by setting up variables which are
%% accessed globally. Options have been fully expanded at ths point.

init(_Options) ->
  % XXX: If more options start to use this then make a list instead
  %      and change hipe_icode_ssa_const_prop.erl accordingly
  put(warn_expression_throws_exception,
	proplists:get_bool(warn_expression_throws_exception, _Options)),

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

%% --------------------------------------------------------------------

post(Res, Icode, Options) ->
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

  Info = TimerVals ++ CounterVals ++ Measures,
  
  case proplists:get_bool(get_called_modules, Options) of
    true ->
      CalledMods = hipe_icode_callgraph:get_called_modules(Icode),
      case Info of
	[] ->
	  {Res, {called_modules, CalledMods}};
	_ ->
	  {Res, {info, Info}, {called_modules, CalledMods}}
      end;
    false ->
      case Info of
	[] ->
	  Res;
	_ ->
	  {Res, {info, Info}}
      end
  end.

%% --------------------------------------------------------------------

%% @spec version() -> {Major, Minor, Increment}
%% Major = integer()
%% Minor = integer()
%% Increment  = integer() 
%% @doc Returns the current HiPE version.
version() ->
  ?VERSION().

%% ____________________________________________________________________
%% 
%% D O C U M E N T A T I O N   -   H E L P 
%%

%% @spec () -> ok
%% @doc Prints on-line documentation to the standard output.
help() ->
  {V1,V2,V3} = ?VERSION(),
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
	    "   pp_amd64 = pp_native,\n" ++
	    "   pp_ppc = pp_native,\n" ++
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
option_text(icode_ssa_check) ->
  "Checks wheter Icode is on SSA form or not\n";
option_text(icode_ssa_const_prop) ->
  "Performs sparse conditional constant propagation on Icode SSA";
option_text(warn_expression_throws_exception) ->
  "Warns when detecting code that will throw an exception at runtime.";
option_text(load) ->
  "Automatically load the produced code into memory.";
option_text(peephole) ->
  "Enables peephole optimizations.";
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
  "Display the generated (back-end specific) native code.";
option_text(regalloc) ->
  "Select register allocation algorithm. Used as {regalloc, METHOD}.\n" ++
  "    Currently available methods:\n" ++
  "        naive - spills everything (for debugging and testing).\n" ++
  "        linear_scan - fast; not so good if few registers available.\n" ++
  "        graph_color - slow, but gives better performance.\n" ++
  "        coalescing - tries hard to use registers.";
option_text(remove_comments) ->
  "Strip comments from intermediate code.";
option_text(rtl_ssa) ->
  "Perform SSA conversion on the RTL level -- default starting at O2";
option_text(rtl_ssa_const_prop) ->
  "Performs sparse conditional constant propagation on RTL SSA";
option_text(rtl_cse) ->
  "Common Subexpression Elimination on RTL.";
option_text(rtl_prop) ->
  "Perform RTL-level constant propagation.";
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
option_text(type_only) ->
  "Breaks the compilation after the icode type pass. No native code will " ++
  "  be produced and no code will be loaded.";
option_text(type_warnings) ->
  "Turns on warnings from the icode type propagator";
option_text(use_indexing) ->
  "Use indexing for multiple-choice branch selection.";
option_text(use_callgraph) ->
  "Compile the functions in a module according to a reversed topological "
    "sorted order to gain more information when using a dets for storing "
    "intra-modular type information.";
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
%%     icode_type:
%%     {ls_order, Order}:
%%     {regalloc, Algorithm}:
%%     remove_comments
%%     sparc_estimate_block_time
%%     sparc_peephole:
%%     sparc_post_schedule
%%     sparc_profile:
%%     sparc_rename:
%%     sparc_schedule
%%     timeregalloc:
%%     timers
%%     safe
%%     use_indexing
%%     rtl_cse

%% Valid option keys. (Don't list aliases or negations - the check is
%% done after the options have been expanded to normal form.)

opt_keys() ->
    ['O',
     check_for_inlining,
     core,
     core_transform,
     counters,
     count_instrs,
     count_spills,
     count_temps,
     debug,
     fill_delayslot,
     finalise_x86,
     frame_x86,
     get_called_modules,
     hot,
     icode_split_arith,
     icode_ssa_check,
     icode_ssa_copy_prop,
     icode_ssa_const_prop,
     icode_type,
     inline_bs,
     inline_fp,
     ls_order,
     load,
     measure_regalloc,
     peephole,
     pp_asm,
     pp_beam,
     pp_icode,
     pp_icode_ssa,
     pp_opt_icode,
     pp_typed_icode,
     pp_icode_liveness,
     pp_native,
     pp_rtl,
     pp_rtl_liveness,
     pp_rtl_ssa,
     regalloc,
     remove_comments,
     rtl_ssa,
     rtl_ssa_const_prop,
     rtl_cse,
     rtl_prop,
     rtl_show_translation,
     safe,
     sparc_estimate_block_time,
     sparc_peephole,
     sparc_post_schedule,
     sparc_profile,
     sparc_prop,
     sparc_schedule,
     sparc_rename,
     target,
     time,
     timeout,
     timeregalloc,
     timers,
     to_rtl,
     type_warnings,
     type_only,
     use_indexing,
     use_inline_atom_search,
     use_callgraph,
     use_clusters,
     use_jumptable,
     verbose,
     warn_expression_throws_exception,
     x87].

%% Definitions: 

o1_opts() ->
  Common = [rtl_prop, inline_fp, inline_bs, peephole],
  case get(hipe_target_arch) of
    ultrasparc ->
      [sparc_peephole, fill_delayslot | Common];
    powerpc ->
      [rtl_prop, inline_bs];	% XXX: inline_fp not yet allowed
    x86 ->
      Common;
    amd64 ->
      Common;
    Arch ->
      ?EXIT({executing_on_an_unsupported_architecture,Arch})
  end.

o2_opts() ->
  Common = [icode_ssa_const_prop, icode_ssa_copy_prop, icode_type,
	    %% warn_expression_throws_exception, 
	    rtl_ssa, rtl_ssa_const_prop,
	    use_indexing, remove_comments | o1_opts()],
  %% KOSTIS: The following do not work currently -- why?
  % Common = [rtl_cse | Common],
  % Common = [{rtl_cse,global} | Common],
  case get(hipe_target_arch) of
    ultrasparc ->
      [sparc_prop | Common];
    powerpc ->
      Common;
    x86 ->
      Common;
    amd64 ->
      Common;
    Arch -> 
      ?EXIT({executing_on_an_unsupported_architecture,Arch})
  end.

o3_opts() ->
  Common = [{regalloc,coalescing} | o2_opts()],
  case get(hipe_target_arch) of
    ultrasparc ->
      Common;
    powerpc ->
      Common;
    x86 ->
      Common;
    amd64 ->
      Common;
    Arch ->
      ?EXIT({executing_on_an_unsupported_architecture,Arch})
  end.

%% Note that in general, the normal form for options should be positive.
%% This is a good programming convention, so that tests in the code say
%% "if 'x' ..." instead of "if not 'no_x' ...".

opt_negations() ->
  [{no_core, core},
   {no_debug, debug},
   {no_fill_delayslot, fill_delayslot},
   {no_finalise_x86, finalise_x86},
   {no_get_called_modules, get_called_modules},
   {no_icode_split_arith, icode_split_arith},
   {no_icode_ssa_check, icode_ssa_check},
   {no_icode_ssa_copy_prop, icode_ssa_copy_prop},
   {no_icode_ssa_const_prop, icode_ssa_const_prop},
   {no_warn_expression_throws_exception, warn_expression_throws_exception},
   {no_icode_type, icode_type},
   {no_inline_bs, inline_bs},
   {no_inline_fp, inline_fp},
   {no_load, load},
   {no_peephole, peephole},
   {no_pp_beam, pp_beam},
   {no_pp_icode, pp_icode},
   {no_pp_icode_ssa, pp_icode_ssa},
   {no_pp_opt_icode, pp_opt_icode},
   {no_pp_typed_icode, pp_typed_icode},
   {no_pp_rtl, pp_rtl},
   {no_pp_native, pp_native},
   {no_remove_comments, remove_comments},
   {no_rtl_ssa, rtl_ssa},
   {no_rtl_ssa_const_prop, rtl_ssa_const_prop},
   {no_rtl_cse, rtl_cse},
   {no_rtl_prop, rtl_prop},
   {no_rtl_show_translation, rtl_show_translation},
   {no_sparc_estimate_block_time, sparc_estimate_block_time},
   {no_sparc_peephole, sparc_peephole},
   {no_sparc_post_schedule, sparc_post_schedule},
   {no_sparc_profile, sparc_profile},
   {no_sparc_prop, sparc_prop},
   {no_sparc_schedule, sparc_schedule},
   {no_time, time},
   {no_type_only, type_only},
   {no_type_warnings, type_warnings},
   {no_use_callgraph, use_callgraph},
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
   {pp_x86, pp_native},
   {pp_amd64, pp_native},
   {pp_ppc, pp_native}].

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
   {safe, [{regalloc,graph_color}, no_use_indexing]},
   {x87, [x87, inline_fp]}].

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
      ?WARNING_MSG("Unknown options: ~p.\n", [L]),
      ok
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
