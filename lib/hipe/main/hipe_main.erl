%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id$
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%			  COMPILER MAIN "LOOP"
%%
%% This module compiles a single function, represented as ICode or
%% RTL, to a linear native code representation.
%% The option 'to_rtl' stops compilation after the RTL stage. Compiler
%% options must have been expanded (cf. `hipe:expand_options').
%%

%%=====================================================================

-module(hipe_main).
-export([compile_icode/3,
	 compile_icode/4,
	 compile_rtl/3,
	 compile_rtl/4,
	 icode_to_rtl/3,
	 rtl_to_sparc/3,
	 sparc_cfg_to_sparc/3]).

%%=====================================================================

-ifndef(DEBUG).
-define(DEBUG,1).
-endif.

-define(HIPE_INSTRUMENT_COMPILER, true). %% Turn on instrumentation.

-include("../main/hipe.hrl").

%%=====================================================================

compile_icode(Fun,LinearIcode,Options) ->
  compile_icode(Fun,LinearIcode,Options,get(hipe_debug)).

compile_icode(Fun,LinearIcode,Options,DebugState) ->
  ?opt_start_timer("Icode opts"),
  Icode1 = hipe_icode_cleanup:code(LinearIcode),
  %% hipe_icode_pp:pp(Icode1),
  ?opt_start_timer("Icode step1"),
  IcodeCfg1  = icode_step1(Icode1, Fun, Options),
  ?opt_stop_timer("Icode step1"),
  icode_pp(IcodeCfg1, Fun, proplists:get_value(pp_icode, Options)),
  ?opt_start_timer("Icode step2"),
  IcodeCfg2  = icode_step2(IcodeCfg1, Options),
  ?opt_stop_timer("Icode step2"),
  ?opt_start_timer("icode step3"),
  IcodeCfg3 = icode_step3(IcodeCfg2, Fun, Options),
  ?opt_stop_timer("icode step3 done"),
  icode_pp(IcodeCfg3, Fun, proplists:get_value(pp_opt_icode, Options)),
  Icode2 = hipe_icode_cfg:linearize(IcodeCfg3),
  ?opt_stop_timer("Icode opts done"),
  %%hipe_icode_pp:pp(Icode2),

  ?opt_start_timer("To RTL-code"),
  LinearRTL = icode_to_rtl(Fun,Icode2,Options),
  ?opt_stop_timer("Got RTL-code"),
  case proplists:get_bool(to_rtl, Options) of
    false ->
      compile_rtl(Fun, LinearRTL, Options, DebugState);
    true ->
      put(hipe_debug, DebugState),
      {rtl, LinearRTL}
  end.

%%=====================================================================

compile_rtl(Fun,LinearRTL,Options) ->
  compile_rtl(Fun,LinearRTL,Options,get(hipe_debug)).

compile_rtl(Fun,LinearRTL,Options,DebugState) ->
  LinearCode =
    case get(hipe_target_arch) of
      ultrasparc ->
	compile_rtl_to_sparc(Fun, LinearRTL, Options);
      x86 ->
	compile_rtl_to_x86(Fun, LinearRTL, Options)
    end,
  put(hipe_debug, DebugState),
  LinearCode.

compile_rtl_to_x86(Fun, LinearRTL, Options) ->
  ?opt_start_timer("To x86 code"),
  LinearCode = rtl_to_x86(Fun, LinearRTL, Options),
  ?opt_stop_timer("Got x86 code"),
  LinearCode.

compile_rtl_to_sparc(Fun, LinearRTL, Options) ->
  ?opt_start_timer("To SPARC-code"),
  LinearSparc = rtl_to_sparc(Fun,LinearRTL,Options),
  ?opt_stop_timer("Got SPARC-code"),
  LinearSparc.

%%=====================================================================

icode_step1(Icode, Fun,Options) ->
  %%  hipe_icode_pp:pp(Icode),
  Icode1 = 
    case proplists:get_bool(remove_comments, Options) of
      true -> 
	?opt_start_timer("remove_comments"),
	Ic1 = hipe_icode:strip_comments(Icode),
	?opt_stop_timer("remove_comments"),
	Ic1;
      false -> Icode
    end,
  ?opt_start_timer("cleanup icode"),
  Icode2 = hipe_icode_cleanup:code(Icode1),
  ?opt_stop_timer("cleanup icode"),
  ?opt_start_timer("preprocess icode"),
  Icode3 = hipe_icode:preprocess_code(Icode2),
  ?opt_stop_timer("preprocess icode"),
  ?opt_start_timer("init icode"),
  IcodeCfg1 = hipe_icode_cfg:init(Icode3),
  ?opt_stop_timer("init icode"),
  %%hipe_icode_cfg:pp(IcodeCfg1),
  ?opt_start_timer("update catches"),
  debug("update catches: ~w~n", [Fun], Options),
  IcodeCfg2 = hipe_update_catches:update_catches(IcodeCfg1, Options),
  ?opt_stop_timer("update catches"),
  %% hipe_icode_cfg:pp(IcodeCfg2),
  IcodeCfg2.

icode_step2(IcodeCfg,Options) ->
  IcodeCfg1 =
    case proplists:get_bool(icode_prop,Options) of
      true ->
	debug("icode_prop: ~w~n",
	      [hash(IcodeCfg)], Options),
	?opt_start_timer("icode prop"),
	CFG = hipe_icode_prop:cfg(IcodeCfg),
	?opt_stop_timer("icode prop done."),
	hipe_icode_cfg:remove_dead_code(CFG);
      false ->
	IcodeCfg
    end,
  
  ?opt_start_timer("Icode binary pass"),
  IcodeCfg2 = 
    case get(hipe_inline_bs) of
      true -> 
 	hipe_icode_binary_pass:make_pass(IcodeCfg1);
      _ ->
 	IcodeCfg1
    end,
  ?opt_stop_timer("Icode binary pass"),
  IcodeCfg2.

icode_step3(IcodeCfg, Fun, Options) ->
  IcodeCfg1 = icode_ssa(IcodeCfg, Fun, Options),
  ?opt_start_timer("Icode heaptests"),
  IcodeCfg2 =
    case proplists:get_bool(rtl_add_gc,Options) of
      false ->
	hipe_icode_heap_test:cfg(IcodeCfg1);
      true ->
	IcodeCfg1
    end,
  ?opt_stop_timer("Icode heaptests"),
  IcodeCfg2.

icode_ssa(IcodeCfg, Fun, Options)->
  ?opt_start_timer("Icode ssa"),
  X0 = hipe_icode_ssa:ssa(IcodeCfg),
  ?opt_stop_timer("Icode ssa done."),
  X0b = hipe_icode_binary_pass:remove_save_restore(X0),
  X1 = case proplists:get_value(icode_ssa_copy_prop, Options) of
	 true ->
	   ?opt_start_timer("Icode copy propagation"),
	   TmpCfg0 = hipe_icode_ssa_copy_prop:cfg(X0b),
	   ?opt_stop_timer("Icode copy propagation"),
	   TmpCfg0;
	 _ -> 
	   X0b
       end,

  ?opt_start_timer("Icode type info"),
  X2 = hipe_icode_type:cfg(X1, Fun, Options),
  ?opt_stop_timer("Icode type info"),

  case proplists:get_bool(icode_ssa_check, Options) of
    true ->
      hipe_icode_ssa_check:check(X2);
    false ->
      ok
  end,
  ?opt_start_timer("Icode ssa propagate"),
  X3 = hipe_icode_ssa_propagate:propagate(X2),
  ?opt_stop_timer("Icode ssa propagate done."),
  ?opt_start_timer("Icode ssa cleanup"),
  X4 = hipe_icode_prop:remove_dead_code(X3),
  X5 = hipe_icode_cfg:remove_dead_code(X4),
  ?opt_stop_timer("Icode ssa cleanup done."),
  X5.

icode_pp(IcodeCfg, Fun, PrintOption)->
  case PrintOption of
    true ->
      hipe_icode_cfg:pp(IcodeCfg);
    {only,Lst} ->
      case lists:member(Fun,Lst) of
	true ->
	  hipe_icode_cfg:pp(IcodeCfg);
	false ->
	  ok
      end;
    {file,FileName} ->
      {ok,File} = file:open(FileName,[write,append]),
      hipe_icode_cfg:pp(File, IcodeCfg);
    _ ->
      ok
  end.

%%=====================================================================

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% icode_to_rtl(Icode_list, Options_list)
%%
%% Returns { Linear_RTL_code, ConstTab }

icode_to_rtl(Fun, Icode,Options) ->

  RtlCfg7 = icode_to_rtl_cfg(Fun,Icode,Options),
  debug("linearize: ~w, ~w~n", [Fun, hash(RtlCfg7)], Options),
  Rtl8 = hipe_rtl_cfg:linearize(RtlCfg7),
  Rtl8.

icode_to_rtl_cfg(Fun, Icode, Options) ->

  debug("ICODE -> RTL: ~w, ~w~n", [Fun, hash(Icode)], Options),
  ?opt_start_timer("translate..."),
  %% GC tests should have been added to the icode.
  %%  if Options contains 'rtl_add_gc' then gc_test will be added.
  %%hipe_icode_pp:pp(Icode),
  Rtl0 = hipe_icode2rtl:translate(Icode, Options),
  %%hipe_rtl:pp(Rtl0),
  ?opt_stop_timer("translate done."),
  ?opt_start_timer("to cfg"),
  RtlCfg1 = hipe_rtl_cfg:init(Rtl0),
  ?opt_stop_timer("to cfg"),
  %% hipe_rtl_cfg:pp(RtlCfg1),
  ?opt_start_timer("RTL step2"),
  RtlCfg2 = rtl_step2(Fun, RtlCfg1, Options),
  ?opt_stop_timer("RTL step2"),
  ?opt_start_timer("RTL step3"),
  %% hipe_rtl_cfg:pp(RtlCfg2),
  RtlCfg3 = rtl_step3(Fun, RtlCfg2, Options),
  ?opt_stop_timer("RTL step3"),
  ?opt_start_timer("RTL step4"),
  %%hipe_rtl_cfg:pp(RtlCfg3),
  RtlCfg4 = rtl_step4(Fun, RtlCfg3, Options),
  ?opt_stop_timer("RTL step4"), 
  %%hipe_rtl_ssa_check:check(RtlCfg4),
  ?opt_start_timer("RTL step5"),
  %% hipe_rtl_cfg:pp(RtlCfg4),
  %% io:format("cfg~n~w~n", [RtlCfg4]),
  RtlCfg5 = rtl_step5(Fun, RtlCfg4, Options),
  ?opt_stop_timer("RTL step5"),
  rtl_pp(Fun, RtlCfg5, Options),
  RtlCfg5.

rtl_step2(Fun,RtlCfg1,Options)->
  %%hipe_rtl_cfg:pp(RtlCfg1),
  case proplists:get_value(rtl_cse,Options) of
    local ->
      debug("cse (local): ~w~n", [Fun], Options),
      ?opt_start_timer("CSE"),
      Cfg2 = hipe_rtl_cse:blocks(RtlCfg1),
      ?opt_stop_timer("CSE"),
      Cfg2;
    ebb ->
      debug("cse (ebb): ~w~n", [Fun], Options),
      ?opt_start_timer("CSE"),
      Cfg2 = hipe_rtl_cse:ebb(RtlCfg1),
      ?opt_stop_timer("CSE"),
      Cfg2;
    global ->
      debug("cse (global): ~w~n", [Fun], Options),
      ?opt_start_timer("CSE"),
      Cfg2 = hipe_rtl_cse:fix(RtlCfg1),
      ?opt_stop_timer("CSE"),
      Cfg2;
    true ->
      debug("cse (local): ~w~n", [Fun], Options),
      ?opt_start_timer("CSE"),
      Cfg2 = hipe_rtl_cse:blocks(RtlCfg1),
      ?opt_stop_timer("CSE"),
      Cfg2;
    _ ->
      RtlCfg1
  end.

rtl_step3(Fun,RtlCfg2,Options)->

  case proplists:get_bool(rtl_prop_1, Options) of
    true ->
      debug("rtl prop: ~w, ~w~n", 
	    [Fun, hash(RtlCfg2)], Options),
      ?opt_start_timer("rtl prop..."),
      CFG = hipe_rtl_prop:cfg(RtlCfg2),
      %% hipe_rtl_cfg:pp(RtlCfg2),
      ?opt_stop_timer("rtl prop done."),
      ?opt_start_timer("Dead code"),
      CFG2 = hipe_rtl_cfg:remove_dead_code(CFG),
      ?opt_stop_timer("Dead code"),
      CFG2;
    false ->
      RtlCfg2
  end.


rtl_step4(Fun, RtlCfg3, Options) ->
  debug("expand gc: ~w~n", [Fun], Options),
  ?option_time(RtlCfg4 = hipe_rtl_gctests:expand(RtlCfg3),
	       "Expand GC-tests", Options),
  RtlCfg4.

rtl_step5(Fun, RtlCfg4, Options) ->
  case get(hipe_target_arch) of
    ultrasparc ->
      rtl_step5_for_sparc(Fun, RtlCfg4, Options);
    x86 ->
      rtl_step5_for_x86(Fun, RtlCfg4, Options)
  end.

rtl_step5_for_x86(_Fun, RtlCfg, Options) ->
    %% may do more later, but RTL's frame stuff is Verboten
    case proplists:get_bool(rtl_prop_2, Options) of
      true ->
	?opt_start_timer("Rtl-prop2"),
	RtlCfg2 =
	  case proplists:get_bool(rtl_prop_3, Options) of
	    true ->
	      hipe_rtl_prop:do(RtlCfg);
	    _ ->
	      hipe_rtl_prop:cfg(RtlCfg)
	  end,
	?opt_stop_timer("Rtl-prop2"),
	?opt_start_timer("Dead Code"),
	RtlCfg3 = hipe_rtl_cfg:remove_dead_code(RtlCfg2),
	?opt_stop_timer("Dead Code"),
	RtlCfg3;
      false ->
	RtlCfg
    end.

rtl_step5_for_sparc(_Fun, RtlCfg, Options) ->
      case proplists:get_bool(rtl_prop_2, Options) of
      true ->
	?opt_start_timer("Rtl-prop2"),
	RtlCfg2 =
	  case proplists:get_bool(rtl_prop_3, Options) of
	    true ->
	      hipe_rtl_prop:do(RtlCfg);
	    _ ->
	      hipe_rtl_prop:cfg(RtlCfg)
	  end,
	?opt_stop_timer("Rtl-prop2"),
	?opt_start_timer("Dead Code"),
	RtlCfg3 = hipe_rtl_cfg:remove_dead_code(RtlCfg2),
	?opt_stop_timer("Dead Code"),
	RtlCfg3;
      false ->
	RtlCfg
    end.

rtl_pp(Fun, RtlCfg, Options) ->
  case proplists:get_value(pp_rtl, Options) of
    true ->
      hipe_rtl_cfg:pp(RtlCfg);
    {only, Lst} ->
      case lists:member(Fun, Lst) of
	true ->
	  hipe_rtl_cfg:pp(RtlCfg);
	false ->
	  ok
      end;
    {file, FileName} ->
      {ok, File} = file:open(FileName, [write,append]),
      hipe_rtl_cfg:pp(File, RtlCfg);
    _ ->
      ok
  end.

%%%
%%% RTL -> x86
%%%

rtl_to_x86(_Fun, RTL, Options) ->
  ?when_option(time, Options, ?start_timer("translate")),
  X86a = hipe_rtl_to_x86:translate(RTL),
  ?when_option(time, Options, ?stop_timer("translate done.")),
  ?when_option(time, Options, ?start_timer("Regalloc")),
  X86b = x86_ra(X86a, Options),
  ?when_option(time, Options, ?stop_timer("Regalloc done")),
  ?when_option(time, Options, ?start_timer("frame")),
  X86c = x86_frame(X86b, Options),
  ?when_option(time, Options, ?stop_timer("frame")),
  ?when_option(time, Options, ?start_timer("finalise")),
  X86d = x86_finalise(X86c, Options),
  ?when_option(time, Options, ?stop_timer("finalise done.")),
  x86_pp(X86d, Options),
  {native, x86, {unprofiled, X86d}}.

%%% use option {regalloc,coalescing} to choose the coalescing one
x86_ra(X86, Options) ->
  %%hipe_x86_pp:pp(X86),

  {X86_RA_FP_Defun, Coloring_fp, SpillIndex} = 
    case proplists:get_bool(inline_fp, Options) of
      true ->
	hipe_x86_ra_fp_ls:ra(X86,Options);
      false ->
	{X86,[],0}
    end,

  %%hipe_x86_pp:pp(X86_RA_FP_Defun),
  ?start_ra_instrumentation(Options, 
			    length(hipe_x86:defun_code(X86)),
			    element(2,hipe_x86:defun_var_range(X86))),

  {X86_RA_Defun, Coloring}
    = case proplists:get_value(regalloc,Options,default) of
	coalescing ->
	  hipe_x86_ra_coalescing:ra(X86_RA_FP_Defun,SpillIndex,Options);
	linear_scan ->
	  hipe_x86_ra_ls:ra(X86_RA_FP_Defun,SpillIndex, Options);
	graph_color ->
	  hipe_x86_ra_graph_color:ra(X86_RA_FP_Defun,SpillIndex,Options);
	naive ->
	  hipe_x86_ra_dummy:ra(X86_RA_FP_Defun,Coloring_fp,Options);
	default ->
	  hipe_x86_ra_coalescing:ra(X86_RA_FP_Defun,SpillIndex,Options)
      end,   

  ?stop_ra_instrumentation(Options, 
			   length(hipe_x86:defun_code(X86_RA_Defun)),
			   element(2,hipe_x86:defun_var_range(X86_RA_Defun))),
  %%hipe_x86_pp:pp(X86_RA_Defun),
  {X86_RA_Defun, Coloring, Coloring_fp}.


%%% use option no_frame_x86 to disable calling hipe_x86_frame
x86_frame(X86, Options) ->
  case proplists:get_value(frame_x86, Options, true) of
    true ->
      hipe_x86_frame:frame(X86, Options);
    false ->
      X86	% illegal code, but allows you to exercise the compiler
  end.

%%% use option no_finalise_x86 to disable calling hipe_x86_finalise
x86_finalise(X86, Options) ->
  case proplists:get_value(finalise_x86, Options, true) of
    true ->
      hipe_x86_finalise:finalise(X86);
    false ->
      X86	% illegal code, but allows you to exercise the compiler
  end.

x86_pp(X86, Options) ->
  case proplists:get_value(pp_native, Options) of
    true ->
      hipe_x86_pp:pp(X86);
    {file, FileName} ->
      {ok, File} = file:open(FileName, [write,append]),
      hipe_x86_pp:pp(File, X86),
      file:close(File);
    _ ->
      []
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% rtl_to_sparc( { RTL, ConstTab }, Options ).
%%
%% Returns:
%%    {native, ultrasparc, { unprofiled, [SparcInstr], ConstTab }} |


rtl_to_sparc(Fun, Rtl, Options) ->
  %% hipe_rtl_cfg:pp(Rtl),
  debug("rtl -> sparc: ~w~n", [Fun], Options),
  ?when_option(time, Options, ?start_timer("translate...")),
  Sparc0    = hipe_rtl2sparc:translate(Rtl, Options),
  ?when_option(time, Options, ?stop_timer("translate done.")),
  SparcCfg1 = hipe_sparc_cfg:init(Sparc0),
  %% hipe_sparc_cfg:pp(SparcCfg1),
  sparc_cfg_to_sparc(Fun, SparcCfg1, Options).

sparc_cfg_to_sparc(Fun,SparcCfg1,Options)->
  %% hipe_sparc_cfg:pp(SparcCfg1),
  %% hipe_sparc_cfg:linearize(SparcCfg1),
  SparcCfg2 = rtl_to_sparc1(Fun, SparcCfg1, Options),
  %% hipe_sparc_cfg:pp(SparcCfg2),
  %% hipe_sparc_cfg:linearize(SparcCfg2),
  SparcCfg3 = rtl_to_sparc2(Fun, SparcCfg2, Options),
  %% hipe_sparc_cfg:pp(SparcCfg3),
  %% hipe_sparc_cfg:linearize(SparcCfg3),
  SparcCfg4 = rtl_to_sparc3(Fun, SparcCfg3, Options),
  %% hipe_sparc_cfg:pp(SparcCfg4),
  %% hipe_sparc_cfg:linearize(SparcCfg4),
  SparcCode = rtl_to_sparc4(Fun, SparcCfg4, Options),
  {native, ultrasparc, {unprofiled, SparcCode}}.


rtl_to_sparc1(Fun, SparcCfg, Options) ->
  %% hipe_sparc_pp:pp(hipe_sparc_cfg:linearize(SparcCfg)),
  debug("split constants: ~w~n", [Fun], Options),
  ?when_option(time, Options, ?start_timer("sparc split constants...")),
  SparcCfg2        = hipe_finalize:split_constants(SparcCfg),
  ?when_option(time, Options, ?stop_timer("sparc split constants done.")),
  SparcCfg2.    

rtl_to_sparc2(_Fun, SparcCfg2, Options) ->

  %% Is this realy needed now?
  %%  It adds ~40% to the number of basic blocks... 
  %% I'll take it out and we'll see what happens.
  %%                                                            /Happi
  %% BEGIN removed code
  %%   debug("straighten ~w~n", [Fun], Options),
  %%   ?opt_start_timer("sparc straighten..."),
  %%   SparcCfg3 = hipe_finalize:straighten(SparcCfg2, Options),
  %%   %% hipe_sparc_cfg:pp(SparcCfg3),
  %%   ?opt_stop_timer("sparc straighten done."),
  %% END removed code
  %% Alternative code:
  SparcCfg3 = SparcCfg2,

  SparcCfg3b =
    case proplists:get_value(sparc_schedule,Options) of
      undefined ->
	case proplists:get_bool(sparc_estimate_block_times,
				Options) of
	  true ->
	    ?opt_start_timer("Schedule (estimate)"),
	    SparcCfg3a = hipe_schedule:est_cfg(SparcCfg3),
	    ?opt_stop_timer("Schedule (estimate)"),
	    SparcCfg3a;
	  false ->
	    SparcCfg3
	end;
      true ->
	%% report("Schedule: assuming machine ultra\n",[],true),
	%% debug('schedule (ultra)~n',Options),
	?opt_start_timer("Schedule (ultra)"),
	SparcCfg3a = hipe_schedule:cfg(SparcCfg3),
	?opt_stop_timer("Schedule (ultra)"),
	SparcCfg3a;
      false ->
	SparcCfg3;
      ultra ->
	debug('schedule (ultra)~n',Options),
	?opt_start_timer("Schedule (ultra)"),
	SparcCfg3a = hipe_schedule:cfg(SparcCfg3),
	?opt_stop_timer("Schedule (ultra)"),
	SparcCfg3a;
      Machine ->
	report('Schedule: machine ~p not available~n',[Machine],true),
	SparcCfg3;
      _ ->
	SparcCfg3
    end,
  SparcCfg3b.

rtl_to_sparc3(Fun, SparcCfg, Options) ->
  %% hipe_sparc_pp:pp(hipe_sparc_cfg:linearize(SparcCfg)),
  debug("regalloc: ~w~n", [Fun], Options),
  ?opt_start_timer("sparc ra"),
  {{SparcCfg1,TempMap, NextPos},FpMap} = 
    hipe_sparc_ra:allocate(Fun, SparcCfg, Options),
  ?opt_stop_timer("sparc ra"),
  ?opt_start_timer("sparc frame"),
  SparcCfg2 =
    hipe_sparc_frame:frame(SparcCfg1, TempMap, FpMap, 
			   NextPos, Options),
  %% hipe_sparc_cfg:pp(SparcCfg2),
  SparcCfg3 = 
    case proplists:get_value(regalloc,Options) of
      naive ->
	%% Excessive spilling can cause large constants
	%% in stack offsets.
	hipe_sparc_ra_memory:split_constants(SparcCfg2);
      _ -> SparcCfg2
    end,
  ?opt_stop_timer("sparc frame"),
  SparcCfg3.


rtl_to_sparc4(Fun, SparcCfg, Options) ->
  ?opt_start_timer("sparc multimoves"),
  SparcCfg2 = hipe_sparc_multimove:remove_multimoves(SparcCfg),
  ?opt_stop_timer("sparc multimoves"),

  SparcCfg3 =
    case proplists:get_bool(sparc_prop,Options) of
      true ->
	?opt_start_timer("sparc prop"),
	SparcCfg2a = hipe_sparc_prop:cfg(SparcCfg2),
	?opt_stop_timer("sparc prop"),
	SparcCfg2a;
      false ->
	SparcCfg2
    end,


  %% Note: Post-scheduling might be ineffective. Still, we provide it.
  SparcCfg4 = 
    case proplists:get_bool(sparc_post_schedule,Options) of
      true ->
	?opt_start_timer("sparc post schedule"),
	SparcCfg3a = hipe_schedule:cfg(SparcCfg3),
	?opt_stop_timer("sparc post schedule"),
	SparcCfg3a;
      false ->
	SparcCfg3
    end,

  debug("finalize~w~n",[Fun],Options),
  ?opt_start_timer("finalize..."),
  {SparcCfg5,[Entry,Rest]} = 
    case proplists:get_value(hotness,Options) of 
      %% ToDo abstract form for all hot: {-1,[]}
      undefined ->
	hipe_hot_cold:finalize(SparcCfg4,{-1,[]}, Options);
      true ->
	hipe_hot_cold:finalize(SparcCfg4,
			       hipe_hot_cold:get_hotness(Fun),Options);
      Hotness ->
	hipe_hot_cold:finalize(SparcCfg4,Hotness, Options)
    end,
  ?opt_stop_timer("finalize done."),

  Data = hipe_sparc_cfg:data(SparcCfg5),
  case proplists:get_value(pp_native,Options) of
    undefined -> ok;
    PpMode ->
      Sparc= hipe_sparc:sparc_code_update(
	       hipe_sparc_cfg:linearize(SparcCfg5),
	       lists:flatten([element(2,Block)
			      || Block <- [Entry|Rest]])),
      case PpMode of
	true ->
	  hipe_sparc_pp:pp(Sparc);
	{file, FileName} ->
	  {ok, File} = file:open(FileName, [write,append]),
	  hipe_sparc_pp:pp(Sparc, File);
	{only,Lst} ->
	  case lists:member(Fun,Lst) of
	    true ->
	      hipe_sparc_pp:pp(Sparc);
	    false ->
	      ok
	  end;
	_ ->
	  ok
      end
  end,

  {Entry, Rest, Data}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

report(F, A, true) ->
  ?msg(F, A);
report(_F, _A, false) ->
  true.

debug(Text,Options) ->
  debug(Text,[],Options).

debug(Text,Args,Options) ->
  case proplists:get_bool(debug,Options) of
    true ->
      ?msg(Text,Args);
    false ->
      ok
  end.

hash(X) ->
  erlang:phash(X, 16#7f3f5f1).
