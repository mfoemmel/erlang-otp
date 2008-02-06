%% -*- erlang-indent-level: 2 -*-
%% $Id$

-module(hipe_sparc_main).
-export([rtl_to_sparc/3]).

-ifndef(DEBUG).
-define(DEBUG,1).
-endif.
-define(HIPE_INSTRUMENT_COMPILER, true). %% Turn on instrumentation.
-include("../main/hipe.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% rtl_to_sparc(MFA, RTL, Options).
%%
%% Returns:
%%    {native, ultrasparc, {unprofiled, [SparcInstr]}

rtl_to_sparc(MFA, Rtl, Options) ->
  %% hipe_rtl_cfg:pp(Rtl),
  debug("rtl -> sparc: ~w~n", [MFA], Options),
  ?when_option(time, Options, ?start_timer("RTL-to-Sparc")),
  Sparc0 = hipe_rtl_to_sparc:translate(Rtl, Options),
  ?when_option(time, Options, ?stop_timer("RTL-to-Sparc")),
  SparcCfg1 = hipe_sparc_cfg:init(Sparc0),
  %% hipe_sparc_pp:pp(hipe_sparc_cfg:linearize(SparcCfg1)),
  %% hipe_sparc_cfg:linearize(SparcCfg1),
  SparcCfg2 = rtl_to_sparc1(MFA, SparcCfg1, Options),
  %% hipe_sparc_pp:pp(hipe_sparc_cfg:linearize(SparcCfg2)),
  %% hipe_sparc_cfg:linearize(SparcCfg2),
  SparcCfg3 = rtl_to_sparc2(MFA, SparcCfg2, Options),
  %% hipe_sparc_pp:pp(hipe_sparc_cfg:linearize(SparcCfg3)),
  %% hipe_sparc_cfg:linearize(SparcCfg3),
  SparcCfg4 = rtl_to_sparc3(MFA, SparcCfg3, Options),
  %% hipe_sparc_pp:pp(hipe_sparc_cfg:linearize(SparcCfg4)),
  %% hipe_sparc_cfg:linearize(SparcCfg4),
  SparcCode = rtl_to_sparc4(MFA, SparcCfg4, Options),
  {native, ultrasparc, {unprofiled, SparcCode}}.

rtl_to_sparc1(MFA, SparcCfg, Options) ->
  %% hipe_sparc_pp:pp(hipe_sparc_cfg:linearize(SparcCfg)),
  debug("split constants: ~w~n", [MFA], Options),
  ?when_option(time, Options, ?start_timer("Sparc split constants")),
  SparcCfg2 = hipe_finalize:split_constants(SparcCfg),
  ?when_option(time, Options, ?stop_timer("Sparc split constants")),
  SparcCfg2.    

rtl_to_sparc2(MFA, SparcCfg2, Options) ->
  SparcCfg3b =
    case proplists:get_value(sparc_schedule,Options) of
      undefined ->
	case proplists:get_bool(sparc_estimate_block_times, Options) of
	  true ->
	    ?opt_start_timer("Schedule (estimate)"),
	    SparcCfg3a = hipe_schedule:est_cfg(SparcCfg2),
	    ?opt_stop_timer("Schedule (estimate)"),
	    SparcCfg3a;
	  false ->
	    SparcCfg2
	end;
      true ->
	%% report("Schedule: assuming machine ultra\n",[],true),
	%% debug('schedule (ultra)~n',Options),
	?opt_start_timer("Schedule (ultra)"),
	SparcCfg3a = hipe_schedule:cfg(SparcCfg2),
	?opt_stop_timer("Schedule (ultra)"),
	SparcCfg3a;
      false ->
	SparcCfg2;
      ultra ->
	debug("schedule (ultra)~n", [MFA], Options),
	?opt_start_timer("Schedule (ultra)"),
	SparcCfg3a = hipe_schedule:cfg(SparcCfg2),
	?opt_stop_timer("Schedule (ultra)"),
	SparcCfg3a;
      Machine ->
	?msg("Schedule: machine ~p not available~n",[Machine]),
	SparcCfg2
    end,
  SparcCfg3b.

rtl_to_sparc3(MFA, SparcCfg, Options) ->
  %% hipe_sparc_pp:pp(hipe_sparc_cfg:linearize(SparcCfg)),
  debug("regalloc: ~w~n", [MFA], Options),
  ?when_option(time, Options, ?start_timer("Sparc register allocation")),
  {{SparcCfg1,TempMap,NextPos},FpMap} = 
    hipe_sparc_ra:allocate(MFA, SparcCfg, Options),
  ?when_option(time, Options, ?stop_timer("Sparc register allocation")),
  ?when_option(time, Options, ?start_timer("Sparc frame")),
  SparcCfg2 =
    hipe_sparc_frame:frame(SparcCfg1, TempMap, FpMap, NextPos, Options),
  %% hipe_sparc_cfg:pp(SparcCfg2),
  SparcCfg3 = 
    case proplists:get_value(regalloc,Options) of
      naive ->
	%% Excessive spilling can cause large constants
	%% in stack offsets.
	hipe_sparc_ra_naive:split_constants(SparcCfg2);
      _ ->
	SparcCfg2
    end,
  ?when_option(time, Options, ?stop_timer("Sparc frame")),
  SparcCfg3.

rtl_to_sparc4(MFA, SparcCfg, Options) ->
  ?when_option(time, Options, ?start_timer("Sparc multimoves")),
  SparcCfg2 = hipe_sparc_multimove:remove_multimoves(SparcCfg),
  ?when_option(time, Options, ?stop_timer("Sparc multimoves")),

  SparcCfg3 =
    case proplists:get_bool(sparc_prop,Options) of
      true ->
	?when_option(time, Options, ?start_timer("Sparc prop")),
	SparcCfg2a = hipe_sparc_prop:cfg(SparcCfg2),
	?when_option(time, Options, ?stop_timer("Sparc prop")),
	SparcCfg2a;
      false ->
	SparcCfg2
    end,

  %% Note: Post-scheduling might be ineffective. Still, we provide it.
  SparcCfg4 = 
    case proplists:get_bool(sparc_post_schedule,Options) of
      true ->
	?when_option(time, Options, ?start_timer("Sparc post schedule")),
	SparcCfg3a = hipe_schedule:cfg(SparcCfg3),
	?when_option(time, Options, ?stop_timer("Sparc post schedule")),
	SparcCfg3a;
      false ->
	SparcCfg3
    end,

  debug("finalize~w~n",[MFA],Options),
  ?when_option(time, Options, ?start_timer("Sparc finalize")),
  {SparcCfg5,Entry} =
    hipe_sparc_finalize:finalize(SparcCfg4, Options),
  ?when_option(time, Options, ?stop_timer("Sparc finalize")),

  Data = hipe_sparc_cfg:data(SparcCfg5),
  case proplists:get_value(pp_native,Options) of
    undefined ->
      ok;
    PpMode ->
      Sparc =
	hipe_sparc:sparc_code_update(hipe_sparc_cfg:linearize(SparcCfg5),
				     Entry),
      case PpMode of
	true ->
	  hipe_sparc_pp:pp(Sparc);
	{file,FileName} ->
	  {ok, File} = file:open(FileName, [write,append]),
	  hipe_sparc_pp:pp(Sparc, File);
	{only,Lst} when is_list(Lst) ->
	  case lists:member(MFA,Lst) of
	    true ->
	      hipe_sparc_pp:pp(Sparc);
	    false ->
	      ok
	  end;
	{only,MFA} ->
	  hipe_sparc_pp:pp(Sparc);
	_ ->
	  ok
      end
  end,
  {Entry, Data}.

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
