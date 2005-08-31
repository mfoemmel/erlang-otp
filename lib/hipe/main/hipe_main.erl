%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id$
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% @doc	This is the HiPE compiler's main "loop".
%%
%% <h3>Purpose</h3>
%%
%% <p> This module provides code which compiles a single Erlang
%% function, represented as linear ICode all the way down to a linear
%% native code representation (which depends on the 'hipe_target_arch'
%% global variable). </p>
%%
%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%=====================================================================

-module(hipe_main).
-export([compile_icode/3]).

%%=====================================================================

-ifndef(DEBUG).
-define(DEBUG,1).
-endif.

-define(HIPE_INSTRUMENT_COMPILER, true). %% Turn on instrumentation.

-include("../main/hipe.hrl").

%%=====================================================================

%% @spec compile_icode(MFA::mfa(),
%%                     LinearIcode::term(),
%%                     CompilerOptions::options()) ->
%%          {native,Platform,{unprofiled,NativeCode}} | {rtl,RTLCode}
%%
%%     options() = [option()]
%%     option() = term()
%%
%% @type mfa() = {M::mod(),F::fun(),A::arity()}.
%%       A fully qualified function name.
%%
%% @type fun() = atom(). A function identifier.
%%
%% @type arity() = integer(). A function arity; always nonnegative.
%%
%% @doc Compiles the Icode (in linear form) of a single MFA down to
%% native code for the platform of the target architecture.
%% CompilerOptions influence the steps of this compilation process.
%%
%% <p> In particular, the compiler option '<code>to_rtl</code>' stops
%% compilation after translation to RTL (in which case RTL code is
%% generated). The compiler options must have already been expanded
%% (cf. `<a href="hipe.html">hipe:expand_options</a>'). </p>

compile_icode(MFA, LinearIcode, Options) ->
  compile_icode(MFA, LinearIcode, Options, get(hipe_debug)).

%%--------------------------------------------------------------------
%%
%% The following constraints apply to the passes on Icode:
%% 
%% 1. The no_comment pass must be done on linear form;
%%
%% 2. linear_to_cfg, which turns linear form into a CFG, must be
%%    performed before any of the passes on CFG form;
%%
%% 3. binary_pass must be performed before handle_exceptions;
%%
%% 4. handle_exceptions must be performed before icode_ssa;
%%
%% 5. split_arith should be performed after icode_ssa for
%%    effectiveness reasons (and perhaps to work at all);
%%
%% 6. remove_trivial_bbs should be performed last to tidy up the CFG.
%%
%%---------------------------------------------------------------------

compile_icode(MFA, LinearIcode0, Options, DebugState) ->  
  %% Set up gensym with the right ranges for this function.
  {_LMin,LMax} = hipe_icode:icode_label_range(LinearIcode0),
  hipe_gensym:set_label(icode,LMax+1),
  {_VMin,VMax} = hipe_icode:icode_var_range(LinearIcode0),
  hipe_gensym:set_var(icode,VMax+1),
  %%hipe_icode_pp:pp(LinearIcode0),

  ?opt_start_timer("Icode"),
  LinearIcode1 = icode_no_comment(LinearIcode0, Options),
  IcodeCfg0 = icode_linear_to_cfg(LinearIcode1, Options),
  %%hipe_icode_cfg:pp(IcodeCfg0),
  IcodeCfg1 = icode_binary_pass(IcodeCfg0, Options),
  %%hipe_icode_cfg:pp(IcodeCfg1),
  IcodeCfg2 = icode_handle_exceptions(IcodeCfg1, MFA, Options),
  icode_pp(IcodeCfg2, MFA, proplists:get_value(pp_icode, Options)),
  
  case icode_ssa(IcodeCfg2, MFA, Options) of
    {dialyzer, IcodeSSA} -> {dialyzer, IcodeSSA};
    {Fixpoint, IcodeCfg3} -> 
      case proplists:get_bool(type_only, Options) of
	false ->
	  compile_icode_2(MFA, IcodeCfg3, Options, DebugState);
	true ->
	  {type_only, Fixpoint}
      end
  end.

compile_icode_2(MFA, IcodeCfg3, Options, DebugState) ->  
  IcodeCfg4 = icode_split_arith(IcodeCfg3, MFA, Options),
  %%hipe_icode_cfg:pp(IcodeCfg4),
  IcodeCfg5 = icode_heap_test(IcodeCfg4, Options),
  %%hipe_icode_cfg:pp(IcodeCfg5),
  IcodeCfg6 = icode_remove_trivial_bbs(IcodeCfg5, Options),
  icode_pp(IcodeCfg6, MFA, proplists:get_value(pp_opt_icode, Options)),
  icode_liveness_pp(IcodeCfg6, MFA, Options),
  FinalIcode = hipe_icode_cfg:cfg_to_linear(IcodeCfg6),
  ?opt_stop_timer("Icode"),

  ?option_time(LinearRTL=icode_to_rtl(MFA,FinalIcode,Options), "RTL", Options),
  case proplists:get_bool(to_rtl, Options) of
    false ->
      rtl_to_native(MFA, LinearRTL, Options, DebugState);
    true ->
      put(hipe_debug, DebugState),
      {rtl, LinearRTL}
  end.

%%----------------------------------------------------------------
%%
%% Icode passes
%%
%%----------------------------------------------------------------

icode_no_comment(LinearIcode, Options) ->
  case proplists:get_bool(remove_comments, Options) of
    true ->
      ?option_time(hipe_icode:strip_comments(LinearIcode),
		   "Icode remove comments", Options);
    _ ->
      LinearIcode
  end.

icode_linear_to_cfg(LinearIcode, Options) ->
  ?option_time(hipe_icode_cfg:linear_to_cfg(LinearIcode),
	       "transform linear Icode to CFG", Options).

%% The binary_pass needs to occur before the handle_exceptions pass
%% because it assumes that all binary matches have a common end
%% fail-label. This is no longer true after fixing up the catches.

icode_binary_pass(IcodeCfg, Options) ->  
  case proplists:get_bool(inline_bs, Options) of
    true ->
      ?option_time(hipe_icode_binary_pass:make_pass(IcodeCfg),
		   "Icode binary pass", Options);
    _ ->
      IcodeCfg
  end.

icode_handle_exceptions(IcodeCfg, MFA, Options) ->
  debug("Icode fix catches: ~w~n", [MFA], Options),
  ?option_time(hipe_icode_exceptions:fix_catches(IcodeCfg),
	       "Icode fix catches", Options).

%%---------------------------------------------------------------------

icode_split_arith(IcodeCfg, MFA, Options) ->  
  case proplists:get_bool(icode_split_arith, Options) of
    true ->
      ?option_time(hipe_icode_split_arith:cfg(IcodeCfg, MFA),
		   "Icode split arith", Options);
    _ ->
      IcodeCfg
  end.

icode_heap_test(IcodeCfg, Options) ->
  ?option_time(hipe_icode_heap_test:cfg(IcodeCfg),
	       "Icode heap_test", Options).

icode_remove_trivial_bbs(IcodeCfg, Options) ->
  ?option_time(hipe_icode_cfg:remove_trivial_bbs(IcodeCfg),
	       "Icode trivial BB removal", Options).

icode_pp(IcodeCfg, MFA, PrintOption) ->
  case PrintOption of
    true ->
      hipe_icode_cfg:pp(IcodeCfg);
    {only,Lst} when is_list(Lst) ->
      case lists:member(MFA,Lst) of
	true ->
	  hipe_icode_cfg:pp(IcodeCfg);
	false ->
	  ok
      end;
    {only,MFA} ->
       hipe_icode_cfg:pp(IcodeCfg);
    {file,FileName} ->
      {ok,File} = file:open(FileName,[write,append]),
      hipe_icode_cfg:pp(File, IcodeCfg);
    _ ->
      ok
  end.
icode_liveness_pp(IcodeCfg, MFA, Options) ->
  case proplists:get_value(pp_icode_liveness, Options) of
    true ->
      hipe_icode_liveness:pp(IcodeCfg);
    {only,Lst} when is_list(Lst) ->
      case lists:member(MFA,Lst) of
	true ->
	  hipe_icode_liveness:pp(IcodeCfg);
	false ->
	  ok
      end;
    {only,MFA} ->
      hipe_icode_liveness:pp(IcodeCfg);
    _ ->
      ok
  end.
%%--------------------------------------------------------------------
%%
%% Icode passes on SSA form. The following constraints are applicable:
%% 
%% 1. ssa_convert must be first and ssa_unconvert last
%% 
%% 2. ssa_dead_code must be run after the other passes
%%
%% 3. The present order was chosen to maximize effectiveness as
%%    ssa_const_prop might make ssa_type_info more effective
%% 
%% 4. ssa_check could be put in between all passes to make sure that
%%    they preserve SSA-ness
%%
%%---------------------------------------------------------------------

icode_ssa(IcodeCfg0, MFA, Options) ->
  ?opt_start_timer("Icode SSA-passes"),
  IcodeSSA0 = icode_ssa_convert(IcodeCfg0, Options),
  IcodeSSA1 = icode_ssa_binary_pass(IcodeSSA0, Options),
  IcodeSSA2 = icode_ssa_const_prop(IcodeSSA1, Options),
  IcodeSSA3 = icode_ssa_copy_prop(IcodeSSA2, Options),
  case proplists:get_bool(dialyzer, Options) of
    true ->
      {dialyzer, IcodeSSA3};
    false ->
      {Fixpoint, IcodeSSA4} = icode_ssa_type_info(IcodeSSA3, MFA, Options),
      IcodeSSA5 = icode_ssa_dead_code_elimination(IcodeSSA4, Options),
      icode_ssa_check(IcodeSSA5, Options), %% just for sanity
      icode_pp(IcodeSSA5, MFA, proplists:get_value(pp_icode_ssa,Options)),
      IcodeCfg = icode_ssa_unconvert(IcodeSSA5, Options),
      ?opt_stop_timer("Icode SSA-passes"),
      {Fixpoint, IcodeCfg}
  end.

icode_ssa_convert(IcodeCfg, Options) ->
  ?option_time(hipe_icode_ssa:convert(IcodeCfg),
	       "Icode SSA conversion", Options).

icode_ssa_binary_pass(IcodeSSA, Options) ->
  case proplists:get_bool(inline_bs, Options) of
    true ->
      ?option_time(hipe_icode_binary_pass:remove_save_restore(IcodeSSA),
		   "Icode binary pass", Options);
    _ ->
      IcodeSSA
  end.

icode_ssa_const_prop(IcodeSSA, Options) ->
  case proplists:get_bool(icode_ssa_const_prop,Options) of
    true ->
      ?option_time(Tmp=hipe_icode_ssa_const_prop:sparse_cond_const_propagate(IcodeSSA),
		   "Icode SSA sparse conditional constant propagation", Options),
      ?option_time(hipe_icode_ssa:remove_dead_code(Tmp),
		   "Icode SSA dead code elimination pass 1", Options);
    false ->
      IcodeSSA
  end.

icode_ssa_copy_prop(IcodeSSA, Options) ->
  case proplists:get_value(icode_ssa_copy_prop, Options) of
    true ->
      ?option_time(hipe_icode_ssa_copy_prop:cfg(IcodeSSA),
		   "Icode SSA copy propagation", Options);
    _ -> 
      IcodeSSA
  end.

icode_ssa_type_info(IcodeSSA, MFA, Options) ->
  case proplists:get_value(icode_type, Options) of
    false ->
      {ok, IcodeSSA};
    undefined -> 
      {ok, IcodeSSA};
    true ->
      ?option_time(hipe_icode_type:cfg(IcodeSSA, MFA, Options),
		   "Icode type info", Options);
    {plt, _Plt} ->
      ?option_time(hipe_icode_type:cfg(IcodeSSA, MFA, Options),
		   "Icode type info", Options)
  end.

icode_ssa_dead_code_elimination(IcodeSSA, Options) ->
  ?option_time(hipe_icode_ssa:remove_dead_code(IcodeSSA),
	       "Icode SSA dead code elimination pass 2", Options).

icode_ssa_check(IcodeSSA, Options) ->
  case proplists:get_bool(icode_ssa_check, Options) of
    true ->
      ?option_time(hipe_icode_ssa:check(IcodeSSA),
		   "Icode check for SSA-ness", Options);
    false ->
      ok
  end.

icode_ssa_unconvert(IcodeSSA, Options) ->
  ?option_time(hipe_icode_ssa:unconvert(IcodeSSA),
	       "Icode SSA un-convert", Options).


%%=====================================================================
%%
%% @spec icode_to_rtl(MFA::mfa(), Icode, options()) -> Linear_RTL_code
%% @end
%%=====================================================================

%%---------------------------------------------------------------------
%%
%% The following information is known about the passes on RTL:
%%
%% 1. The translation to RTL, in particular the way exceptions are
%%    currently handled in RTL, introduces some unreachable code.
%%    Therefore, unreachable code is removed early followed by a
%%    pass that removes trivial basic blocks so as to have smaller
%%    code to play with.
%%
%% 2. Code is then converted to SSA so as to perform as many
%%    optimizations as possible in this pass; currently NO such
%%    optimization is performed -- this to be changed soon.
%%
%% 3. rtl_prop is almost always used and performs global constant
%%    propagation.
%%
%% 4. rtl_lcm performs a lazy code motion on RTL.
%%
%%----------------------------------------------------------------------
 
icode_to_rtl(MFA, Icode, Options) ->
  debug("ICODE -> RTL: ~w, ~w~n", [MFA, hash(Icode)], Options),
  LinearRTL = translate_to_rtl(Icode, Options),
  %% hipe_rtl:pp(standard_io, LinearRTL),
  RtlCfg  = initialize_rtl_cfg(LinearRTL, Options),
  %% hipe_rtl_cfg:pp(RtlCfg),
  RtlCfg0 = hipe_rtl_cfg:remove_unreachable_code(RtlCfg),
  RtlCfg1 = hipe_rtl_cfg:remove_trivial_bbs(RtlCfg0),
  %% hipe_rtl_cfg:pp(RtlCfg1),

  %RtlCfg3 = rtl_ssa(RtlCfg1, Options),
  {RtlCfg3,Options2} = rtl_ssa(RtlCfg1, Options),
 
  RtlCfg5 = rtl_symbolic(RtlCfg3, Options2),
  %% hipe_rtl_cfg:pp(RtlCfg5),
 
  RtlCfg6 = rtl_prop(RtlCfg5, Options2),
  rtl_liveness_pp(MFA,RtlCfg6, Options2),

  rtl_pp(MFA, RtlCfg6, Options2),	%% TAKE ME OUT
  RtlCfg7 = rtl_lcm(RtlCfg6, Options2),
%  RtlCfg7 = RtlCfg6,
  rtl_pp(MFA, RtlCfg7, Options2),	%% TAKE ME OUT

  %% rtl_pp(MFA, RtlCfg6, Options),
  debug("linearize: ~w, ~w~n", [MFA, hash(RtlCfg7)], Options2),
  LinearRTL1 = hipe_rtl_cfg:linearize(RtlCfg7),
  LinearRTL2 = hipe_rtl_cleanup_const:cleanup(LinearRTL1),
  %% hipe_rtl:pp(standard_io, LinearRTL2),
  LinearRTL2.

translate_to_rtl(Icode, Options) ->
  %% GC tests should have been added in the conversion to Icode.
  ?option_time(hipe_icode2rtl:translate(Icode, Options),
	       "translate", Options).

initialize_rtl_cfg(LinearRTL, Options) ->
  ?option_time(hipe_rtl_cfg:init(LinearRTL), "to cfg", Options).

rtl_symbolic(RtlCfg, _Options) ->
  RtlCfg1=
    case hipe_rtl_arch:safe_handling_of_registers() of
      true ->
	hipe_rtl_symbolic:find_and_replace(RtlCfg);
      false ->
	RtlCfg
    end,
  hipe_rtl_symbolic:expand(RtlCfg1).

rtl_prop(RtlCfg, Options) ->
  case proplists:get_bool(rtl_prop, Options) of
    true ->
      ?opt_start_timer("RTL prop"),
      RtlCfg2 = hipe_rtl_prop:do(RtlCfg),
      ?opt_stop_timer("RTL prop"),
      ?option_time(hipe_rtl_cfg:remove_trivial_bbs(RtlCfg2),
		   "RTL trivial BB removal", Options);
    false ->
      RtlCfg
  end.

rtl_liveness_pp(MFA, RtlCfg, Options) ->
  case proplists:get_value(pp_rtl_liveness, Options) of
    true ->
      hipe_rtl_liveness:pp(RtlCfg);
    {only,Lst} when is_list(Lst) ->
      case lists:member(MFA,Lst) of
	true ->
	  hipe_rtl_liveness:pp(RtlCfg);
	false ->
	  ok
      end;
    {only,MFA} ->
      hipe_rtl_liveness:pp(RtlCfg);
    _ ->
      ok
  end.

%%----------------------------------------------------------------------
%%
%% RTL passes on SSA form. The following constraints are applicable:
%% 
%% 1. ssa_convert must be first and ssa_unconvert last.
%%
%% 2. dead_code_elimination should be performed after conditional
%%    constant propagation in order to cleanup dead code that might
%%    be created by that pass.
%%
%% 3. rtl_ssapre performs A-SSAPRE and has to be done after all other optimizations.
%%
%% 4. ssa_check could be put in between all passes to make sure that
%%    they preserve SSA-ness.
%%
%%----------------------------------------------------------------------

rtl_ssa(RtlCfg0, Options) ->
  case proplists:get_bool(rtl_ssa, Options) of
    true ->
      ?opt_start_timer("RTL SSA-passes"),
      RtlSSA0 = rtl_ssa_convert(RtlCfg0, Options),
      RtlSSA1 = rtl_ssa_const_prop(RtlSSA0, Options),
      %% RtlSSA2 = rtl_ssa_copy_prop(RtlSSA1, Options),
      RtlSSA3 = rtl_ssa_dead_code_elimination(RtlSSA1, Options),
      RtlSSA4 = rtl_ssapre(RtlSSA3, Options),
      
      %% rtl_ssa_check(RtlSSA3, Options), %% just for sanity
      %% rtl_pp(IcodeSSA5, MFA, proplists:get_value(pp_rtl_ssa,Options)),
      RtlCfg = rtl_ssa_unconvert(RtlSSA4, Options),
      case proplists:get_bool(pp_rtl_ssa, Options) of
	true ->
	  io:format("%%------------- After  SSA un-conversion -----------\n"),
	  hipe_rtl_cfg:pp(RtlCfg);
	false ->
	  ok
      end,
      ?opt_stop_timer("RTL SSA-passes"),
      {RtlCfg,Options};
    false ->
      {RtlCfg0,Options}
  end.

rtl_ssa_convert(RtlCfg, Options) ->
  case proplists:get_bool(pp_rtl_ssa, Options) of
    true ->
      io:format("%%------------- Before SSA conversion --------------\n"),
      hipe_rtl_cfg:pp(RtlCfg),
      io:format("%%------------- After  SSA conversion --------------\n"),
      RtlCfgSSA = hipe_rtl_ssa:convert(RtlCfg),
      hipe_rtl_cfg:pp(RtlCfgSSA),
      io:format("%%------------- SSA check warnings below -----------\n"),
      hipe_rtl_ssa:check(RtlCfgSSA),
      RtlCfgSSA;
    false ->
      ?option_time(hipe_rtl_ssa:convert(RtlCfg),
		   "RTL SSA conversion", Options)
  end.

%% foo bar
rtl_ssa_const_prop(RtlCfgSSA, Options) ->
  case proplists:get_bool(rtl_ssa_const_prop,Options) of
    true ->
      ?option_time(hipe_rtl_ssa_const_prop:sparse_cond_const_propagate(RtlCfgSSA),
		   "RTL SSA sparse conditional constant propagation", Options);
    false ->
      RtlCfgSSA
  end.

rtl_ssa_dead_code_elimination(RtlCfgSSA, Options) ->
  ?option_time(hipe_rtl_ssa:remove_dead_code(RtlCfgSSA),
	       "RTL SSA dead code elimination", Options).

%%---------------------------------------------------------------------

rtl_ssapre(RtlCfg, Options) ->
  case proplists:get_bool(rtl_ssapre, Options) of
    true ->
      ?opt_start_timer("Partial Redundancy Elimination (A-SSAPRE)"),
      NewRtlCfg = hipe_rtl_ssapre:rtl_ssapre(RtlCfg,Options),
      ?opt_stop_timer("Partial Redundancy Elimination (A-SSAPRE)"),
      NewRtlCfg;
    false ->
      RtlCfg
  end.

%%---------------------------------------------------------------------

rtl_ssa_unconvert(RtlCfgSSA, Options) ->
  ?option_time(hipe_rtl_ssa:unconvert(RtlCfgSSA),
	       "RTL SSA un-convert", Options).

%%---------------------------------------------------------------------

rtl_lcm(RtlCfg, Options) ->
  case proplists:get_bool(rtl_lcm, Options) of
    true ->
      ?opt_start_timer("RTL lazy code motion"),
%      ?option_time(hipe_rtl_lcm:rtl_lcm(RtlCfg, Options),
%		   "RTL lazy code motion", Options);
      RtlCfg1 = hipe_rtl_lcm:rtl_lcm(RtlCfg, Options),
      ?opt_stop_timer("RTL lazy code motion"),
      RtlCfg1;
    false ->
      RtlCfg
  end.
  
%%---------------------------------------------------------------------

rtl_pp(MFA, RtlCfg, Options) ->
  case proplists:get_value(pp_rtl, Options) of
    true ->
      hipe_rtl_cfg:pp(RtlCfg);
    {only,Lst} when is_list(Lst) ->
      case lists:member(MFA,Lst) of
	true ->
	  hipe_rtl_cfg:pp(RtlCfg);
	false ->
	  ok
      end;
    {only,MFA} ->
      hipe_rtl_cfg:pp(RtlCfg);
    {file,FileName} ->
      {ok,File} = file:open(FileName, [write,append]),
      hipe_rtl_cfg:pp(File, RtlCfg);
    _ ->
      ok
  end.

%%=====================================================================

rtl_to_native(MFA, LinearRTL, Options, DebugState) ->
  ?opt_start_timer("Native code"),
  LinearNativeCode =
    case get(hipe_target_arch) of
      ultrasparc ->
	hipe_sparc_main:rtl_to_sparc(MFA, LinearRTL, Options);
      powerpc ->
	hipe_ppc_main:rtl_to_ppc(MFA, LinearRTL, Options);
      x86 ->
	hipe_x86_main:rtl_to_x86(MFA, LinearRTL, Options);
      amd64 ->
	hipe_amd64_main:rtl_to_amd64(MFA, LinearRTL, Options)
    end,
  ?opt_stop_timer("Native code"),
  put(hipe_debug, DebugState),
  LinearNativeCode.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Debugging stuff ...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

debug(Text, Args, Options) ->
  case proplists:get_bool(debug,Options) of
    true ->
      ?msg(Text,Args);
    false ->
      ok
  end.

hash(X) ->
  erlang:phash(X, 16#7f3f5f1).
