%% -*- erlang-indent-level: 4 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% $Id$
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%			  COMPILER MAIN "LOOP"
%%
%% This module compiles a single function, represented as ICode or
%% RTL, to a linear native code (currently only Sparc) representation.
%% The option 'to_rtl' stops compilation after the RTL stage.  Options
%% must have been expanded (cf. `hipe:expand_options').
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
    ?when_option(time, Options, ?start_timer("To RTL-code")),
    Icode1 = hipe_icode_cleanup:code(LinearIcode),

    %% hipe_icode:pp(Icode1),
    ?when_option(time, Options, ?start_timer("Icode step1")),
    IcodeCfg0  = icode_step1(Fun, Options, Icode1),
    ?when_option(time, Options, ?stop_timer("Icode step1")),
    %% hipe_icode:pp(hipe_icode_cfg:linearize(IcodeCfg0)),
    ?when_option(time, Options, ?start_timer("Icode heaptests")),
    IcodeCfg1 =
      case property_lists:get_bool(rtl_add_gc,Options) of
        false ->
	  hipe_icode_heap_test:cfg(IcodeCfg0);
        true ->
	 IcodeCfg0
      end,
    ?when_option(time, Options, ?stop_timer("Icode heaptests")),
    case property_lists:get_value(pp_opt_icode, Options) of
	true ->
	    hipe_icode_cfg:pp(IcodeCfg1);
	{only,Lst} ->
	    case lists:member(Fun,Lst) of
		true ->
		    hipe_icode_cfg:pp(IcodeCfg1);
		false ->
		    ok
	    end;
	{file,FileName} ->
	    {ok,File} = file:open(FileName,[write,append]),
	    hipe_icode_cfg:pp(File, IcodeCfg1);
	_ ->
	    ok
    end,
    Icode2 = hipe_icode_cfg:linearize(IcodeCfg1),
    %% hipe_icode:pp(Icode2),
    LinearRTL = icode_to_rtl(Fun,Icode2,Options),
    ?when_option(time, Options, ?stop_timer("Got RTL-code")),
    case property_lists:get_bool(to_rtl, Options) of
        false ->
	    compile_rtl(Fun, LinearRTL, Options, DebugState);
        true ->
	    put(hipe_debug, DebugState),
	    {rtl, LinearRTL}
    end.

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
    ?when_option(time, Options, ?start_timer("To x86 code")),
    LinearCode = rtl_to_x86(Fun, LinearRTL, Options),
    ?when_option(time, Options, ?stop_timer("Got x86 code")),
    LinearCode.

compile_rtl_to_sparc(Fun, LinearRTL, Options) ->
    ?when_option(time, Options, ?start_timer("To SPARC-code")),
    LinearSparc = rtl_to_sparc(Fun,LinearRTL,Options),
    ?when_option(time, Options, ?stop_timer("Got SPARC-code")),
    LinearSparc.

icode_step1(Fun,Options,Icode) ->
    %% hipe_icode:pp(Icode),
  Icode1 = 
    case property_lists:get_bool(remove_comments, Options) of
      true -> 
	?when_option(time, Options, ?start_timer("remove_comments")),
	Ic1 = hipe_icode:strip_comments(Icode),
	?when_option(time, Options, ?stop_timer("remove_comments")),
	Ic1;
      false -> Icode
    end,

    ?when_option(time, Options, ?start_timer("init icode")),
    IcodeCfg1        = hipe_icode_cfg:init(Icode1),
    ?when_option(time, Options, ?stop_timer("init icode")),
    %% hipe_icode_cfg:pp(IcodeCfg1),

    ?when_option(time, Options, ?start_timer("update catches")),
    debug("update catches: ~w~n", [Fun], Options),
    IcodeCfg2        = hipe_update_catches:update_catches(IcodeCfg1, Options),
    ?when_option(time, Options, ?stop_timer("update catches")),

    %% hipe_icode_cfg:pp(IcodeCfg2),
    icode_step2(Fun,Options,IcodeCfg2).

icode_step2(Fun,Options,IcodeCfg) ->
    ?when_option(time, Options, ?start_timer("icode opt")),
    %% hipe_icode_cfg:pp(IcodeCfg),
    IcodeCfg1 = icode_opt(IcodeCfg, Options),
    ?when_option(time, Options, ?stop_timer("icode opt done")),
    IcodeCfg2 = IcodeCfg1,
    case property_lists:get_value(pp_icode, Options) of
	true ->
	    hipe_icode_cfg:pp(IcodeCfg2);
	{only,Lst} ->
	    case lists:member(Fun,Lst) of
		true ->
		    hipe_icode_cfg:pp(IcodeCfg2);
		false ->
		    ok
	    end;
	{file,FileName} ->
	    {ok,File} = file:open(FileName,[write,append]),
	    hipe_icode_cfg:pp(File, IcodeCfg2);
	_ ->
	    ok
    end,
    IcodeCfg2.

icode_opt(IcodeCfg, Options) ->
    debug("rename.~n", [], Options),
    ?when_option(time, Options, ?start_timer("icode rename")),
    ?optional_start_timer(time_icode_rename, Options),  
    CFG =
	case hipe_update_catches:has_catches(IcodeCfg) of
	    true ->     %% Renaming is not an option...
		CFG1 = hipe_rename:cfg(IcodeCfg),
		?when_option(time, Options, ?stop_timer("icode rename done.")),
		CFG1;  %%;
	    false ->  %% Renaming is an option
		case property_lists:get_bool(icode_rename,Options) of
		    true ->
			CFG1 = hipe_rename:cfg(IcodeCfg),
			?when_option(time, Options, 
				     ?stop_timer("icode rename done.")),
			CFG1;
		    false ->
			?when_option(time, Options, 
				     ?stop_timer("icode rename not done.")),
			IcodeCfg
		end
	end,
    ?optional_stop_timer(time_icode_rename, Options),
    icode_opt2(CFG, Options).

icode_opt2(IcodeCfg, Options) ->
    %%   hipe_icode_cfg:pp(IcodeCfg),
    icode_opt3(case property_lists:get_bool(icode_prop,Options) of
		   true ->
		       debug("icode_prop: ~w~n",
			     [hash(IcodeCfg)], Options),
		       ?when_option(time, Options, ?start_timer("icode prop")),
		       CFG = hipe_icode_prop:cfg(IcodeCfg),
		       ?when_option(time, Options, ?stop_timer("icode prop done.")),
		       CFG;
		   false ->
		       IcodeCfg
	       end,Options).

icode_opt3(IcodeCfg, Options) ->
    IcodeCfg1 = 
	case property_lists:get_bool(icode_bwd_cprop,Options) of
	    true ->
		debug("bwd cprop~n", [], Options),
		hipe_bwd_cprop:cfg(IcodeCfg);
	    false ->
		IcodeCfg
	end,
    IcodeCfg2 = 
	case property_lists:get_bool(icode_type, Options) of
	    true ->
		debug("icode type~n", [], Options),
		EBBs = hipe_icode_ebb:cfg(IcodeCfg1),
		hipe_icode_ebb:pp(EBBs),
		DAGs = hipe_icode_ebb:dag(EBBs, IcodeCfg1),
		io:format("DAGs: ~w~n", [DAGs]),
		IcodeCfg1;
	    false ->
		IcodeCfg1
	end,
    IcodeCfg2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% icode_to_rtl(Icode_list, Options_list)
%%
%% Returns { Linear_RTL_code, ConstTab }

icode_to_rtl(Fun, Icode,Options) ->
    RtlCfg7 = icode_to_rtl_cfg(Fun,Icode,Options),
    debug("linearize: ~w, ~w~n", [Fun, hash(RtlCfg7)], Options),
    Rtl8             = hipe_rtl_cfg:linearize(RtlCfg7),
    Rtl8.

icode_to_rtl_cfg(Fun, Icode, Options) ->
    debug("ICODE -> RTL: ~w, ~w~n", [Fun, hash(Icode)], Options),
    ?when_option(time, Options, ?start_timer("translate...")),
    %% GC tests should have been added to the icode.
    %%  if Options contains 'rtl_add_gc' then gc_test will be added.
    %% hipe_icode:pp(Icode),
    Rtl0 = hipe_icode2rtl:translate(Icode, Options),
    %% hipe_rtl:pp(Rtl0),
    ?when_option(time, Options, ?stop_timer("translate done.")),
    RtlCfg1 = hipe_rtl_cfg:init(Rtl0),
    %% hipe_rtl_cfg:pp(RtlCfg1),
    RtlCfg2 = rtl_step2(Fun, RtlCfg1, Options),
    RtlCfg3 = rtl_step3(Fun, RtlCfg2, Options),
    RtlCfg4 = rtl_step4(Fun, RtlCfg3, Options),
    RtlCfg5 = rtl_step5(Fun, RtlCfg4, Options),
    rtl_pp(Fun, RtlCfg5, Options),
    RtlCfg5.

rtl_step2(Fun,RtlCfg1,Options)->
    %%  hipe_rtl_cfg:pp(RtlCfg1),
    case property_lists:get_value(rtl_cse,Options) of
	local ->
	    debug("cse (local): ~w~n", [Fun], Options),
	    hipe_rtl_cse:blocks(RtlCfg1);
	ebb ->
	    debug("cse (ebb): ~w~n", [Fun], Options),
	    hipe_rtl_cse:ebb(RtlCfg1);
	global ->
	    debug("cse (global): ~w~n", [Fun], Options),
	    hipe_rtl_cse:fix(RtlCfg1);
	true ->
	    debug("cse (local): ~w~n", [Fun], Options),
	    hipe_rtl_cse:blocks(RtlCfg1);
	_ ->
	    RtlCfg1
    end.

rtl_step3(Fun,RtlCfg2,Options)->
    case property_lists:get_bool(rtl_prop_1, Options) of
	true ->
	    debug("rtl prop: ~w, ~w~n", 
		  [Fun, hash(RtlCfg2)], Options),
	    ?when_option(time, Options, ?start_timer("rtl prop...")),
	    CFG = hipe_rtl_prop:cfg(RtlCfg2),
	    ?when_option(time, Options, ?stop_timer("rtl prop done.")),
	    CFG;
	false ->
	    RtlCfg2
    end.

rtl_step4(Fun, RtlCfg3, Options) ->
    debug("expand gc: ~w~n", [Fun], Options),
    ?option_time(RtlCfg4 = hipe_rtl_gctests:expand(RtlCfg3),
		 "Expand GC-tests", Options),
    %% hipe_rtl_cfg:pp(RtlCfg4),
    RtlCfg4.

rtl_step5(Fun, RtlCfg4, Options) ->
    case get(hipe_target_arch) of
	ultrasparc ->
	    rtl_step5_for_sparc(Fun, RtlCfg4, Options);
	x86 ->
	    rtl_step5_for_x86(Fun, RtlCfg4, Options)
    end.

rtl_step5_for_x86(Fun, RtlCfg4, Options) ->
    %% may do more later, but RTL's frame stuff is Verboten
    case property_lists:get_bool(rtl_prop_2, Options) of
	true ->
	    hipe_rtl_prop:cfg(RtlCfg4);
	false ->
	    RtlCfg4
    end.

rtl_step5_for_sparc(Fun, RtlCfg4, Options) ->
    ?when_option(time, Options, ?start_timer("rtl expand...")),
    %% hipe_rtl_cfg:pp(RtlCfg4),

    debug("add save/restore: ~w~n", [Fun], Options),
    ?option_time(RtlCfg5 = case property_lists:get_bool(trim_frame, Options) of
		true ->
		  hipe_rtl_trim_frame:add_save_restore(RtlCfg4);
		false ->
		  hipe_frame:add_save_restore(RtlCfg4)
	      end,
		 "Add save/restore", Options),

    %% hipe_rtl_cfg:pp(RtlCfg5),
    debug("expand frames: ~w~n", [Fun], Options),
    ?option_time(RtlCfg6 = hipe_frame:expand(RtlCfg5),
		 "Expand frames", Options),

    %% hipe_rtl_cfg:pp(RtlCfg6),
    ?option_time(RtlCfg7 = case property_lists:get_bool(rtl_prop_2, Options) of
		  true ->
		      debug("rtl prop: ~w~n", [Fun], Options),
		      hipe_rtl_prop:cfg(RtlCfg6);
		  false ->
		      RtlCfg6
			   end,
		 "rtl_prop_2", Options),

    ?when_option(time, Options, ?stop_timer("rtl expand done.")),
    RtlCfg7.

rtl_pp(Fun, RtlCfg, Options) ->
    case property_lists:get_value(pp_rtl, Options) of
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

rtl_to_x86(Fun, RTL, Options) ->
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
    %% hipe_x86_pp:pp(X86),

    ?start_ra_instrumentation(Options, 
			      length(hipe_x86:defun_code(X86)),
			      element(2,hipe_x86:defun_var_range(X86))),

    {X86_RA_Defun, Coloring}
	= case property_lists:get_value(regalloc,Options,default) of
	      coalescing ->
		  hipe_x86_ra_coalescing:ra(X86,Options);
	      linear_scan ->
		  hipe_x86_ra_ls:ra(X86,Options);
	      graph_color ->
		  hipe_x86_ra_graph_color:ra(X86,Options);
	      naive ->
		  hipe_x86_ra_dummy:ra(X86,Options);
	      default ->  %% linear_scan made default also here (temporarily)
		  hipe_x86_ra_ls:ra(X86,Options)
	  end,   
    ?stop_ra_instrumentation(Options, 
			     length(hipe_x86:defun_code(X86_RA_Defun)),
			     element(2,hipe_x86:defun_var_range(X86_RA_Defun))),

    {X86_RA_Defun, Coloring}.


%%% use option no_frame_x86 to disable calling hipe_x86_frame
x86_frame(X86, Options) ->
    case property_lists:get_value(frame_x86, Options, true) of
	true ->
	    hipe_x86_frame:frame(X86, Options);
	false ->
	    X86	% illegal code, but allows you to exercise the compiler
    end.

%%% use option no_finalise_x86 to disable calling hipe_x86_finalise
x86_finalise(X86, Options) ->
    case property_lists:get_value(finalise_x86, Options, true) of
	true ->
	    hipe_x86_finalise:finalise(X86);
	false ->
	    X86	% illegal code, but allows you to exercise the compiler
    end.

x86_pp(X86, Options) ->
    case property_lists:get_value(pp_native, Options) of
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
%%    {native, ultrasparc, { profiled, [SparcInstr], SparcCfg, ConstTab, Label, #Ctrs, CtrInfo }}
%% where
%%    - elt 1 is the list of instructions to assemble
%%    - SparcCfg is the CFG without profiling instrs
%%    - ConstTab is the constant table
%%    - Label is the label of the array of profiling counters
%%    - #Ctrs is the number of counters used
%%    - CtrInfo tells us how to compute basic block frequencies, etc.

rtl_to_sparc(Fun, Rtl, Options) ->
    debug("rtl -> sparc: ~w~n", [Fun], Options),
    ?when_option(time, Options, ?start_timer("translate...")),
    Sparc0           = hipe_rtl2sparc:translate(Rtl),
    ?when_option(time, Options, ?stop_timer("translate done.")),
    SparcCfg1        = hipe_sparc_cfg:init(Sparc0),
    %% hipe_sparc_cfg:pp(SparcCfg1),
    sparc_cfg_to_sparc(Fun, SparcCfg1, Options).

sparc_cfg_to_sparc(Fun,SparcCfg1,Options)->
    SparcCfg2 = rtl_to_sparc1(Fun, SparcCfg1, Options),
    {native, ultrasparc, {unprofiled, SparcCfg2}}.
%%%    case property_lists:get_value(sparc_profile,Options) of
%%% 	true ->
%%% 	    debug('adding profile code~n',Options),
%%% 	    { SparcCfg1a, Ctab1a, CtrLabel, NumCtrs, CtrsLst } =
%%% 		hipe_profile:blocks(SparcCfg1),
%%% 	    Sparc5 = 
%%% 		rtl_to_sparc1(Fun, SparcCfg1a, Options),
%%% 	    {profiled, Sparc5, SparcCfg1, 
%%% 	     CtrLabel, NumCtrs, CtrsLst};
%%% 	block ->
%%% 	    debug('adding profile code~n',Options),
%%% 	    { SparcCfg1a, Ctab1a, CtrLabel, NumCtrs, CtrsLst } =
%%% 		hipe_profile:blocks(SparcCfg1),
%%% 	    Sparc5 = 
%%% 		rtl_to_sparc1(Fun, SparcCfg1a, Options),
%%% 	    {profiled, Sparc5, SparcCfg1, 
%%% 	     CtrLabel, NumCtrs, CtrsLst};
%%% 	arc ->
%%% 	    debug('adding profile code~n',Options),
%%% 	    { SparcCfg1a, Ctab1a, CtrLabel, NumCtrs, CtrsLst, ArcExps } =
%%% 		hipe_profile:arcs(SparcCfg1),
%%% 	    Sparc5 = 
%%% 		rtl_to_sparc1(Fun, SparcCfg1a, Options),
%%% 	    {profiled, Sparc5, SparcCfg1,
%%% 	     CtrLabel, NumCtrs, CtrsLst};
%%% 	_ ->
%%% 	    Sparc5 = 
%%% 		rtl_to_sparc1(Fun, SparcCfg1, Options),
%%% 	    {unprofiled, Sparc5}
%%%    end.

rtl_to_sparc1(Fun, SparcCfg, Options) ->
    %% hipe_sparc:pp(hipe_sparc_cfg:linearize(SparcCfg)),

    debug("split constants: ~w~n", [Fun], Options),
    ?when_option(time, Options, ?start_timer("sparc split constants...")),
    SparcCfg2        = hipe_finalize:split_constants(SparcCfg),
    ?when_option(time, Options, ?stop_timer("sparc split constants done.")),
    rtl_to_sparc2(Fun, SparcCfg2, Options).

rtl_to_sparc2(Fun, SparcCfg2, Options) ->
    debug("straighten ~w~n", [Fun], Options),
    ?when_option(time, Options, ?start_timer("sparc straighten...")),
    SparcCfg3 = hipe_finalize:straighten(SparcCfg2, Options),
    %% hipe_sparc_cfg:pp(SparcCfg3),
    ?when_option(time, Options, ?stop_timer("sparc straighten done.")),
    SparcCfg3a =
	case property_lists:get_value(sparc_schedule,Options) of
	    undefined ->
		case property_lists:get_bool(sparc_estimate_block_times,
					     Options) of
		    true ->
			hipe_schedule:est_cfg(SparcCfg3);
		    false ->
			SparcCfg3
		end;
	    true ->
		report("Schedule: assuming machine ultra\n",[],true),
		debug('schedule (ultra)~n',Options),
		hipe_schedule:cfg(SparcCfg3);
	    false ->
		SparcCfg3;
	    ultra ->
		debug('schedule (ultra)~n',Options),
		hipe_schedule:cfg(SparcCfg3);
	    Machine ->
		report('Schedule: machine ~p not available~n',[Machine],true),
		SparcCfg3;
	    _ ->
		SparcCfg3
	end,
    rtl_to_sparc3(Fun, SparcCfg3a, Options).

rtl_to_sparc3(Fun, SparcCfg, Options) ->
    %% hipe_sparc:pp(hipe_sparc_cfg:linearize(SparcCfg)),

    debug("regalloc: ~w~n", [Fun], Options),
    SparcCfg4 =
	hipe_sparc_ra:allocate(Fun, SparcCfg, Options),

    rtl_to_sparc4(Fun, SparcCfg4, Options).

rtl_to_sparc4(Fun, SparcCfg, Options) ->
    SparcCfg4 = hipe_sparc_multimove:remove_multimoves(SparcCfg),

    %% Note: Post-scheduling might be ineffective. Still, we provide it.
    SparcCfg4a = case property_lists:get_bool(sparc_post_schedule,Options) of
		     true ->
			 hipe_schedule:cfg(SparcCfg4);
		     false ->
			 SparcCfg4
		 end,
    debug("finalize~w~n",[Fun],Options),
    ?when_option(time, Options, ?start_timer("finalize...")),
    [Entry,Rest] = 
	case property_lists:get_value(hotness,Options) of 
	    %% ToDo abstract form for all hot: {-1,[]}
	    undefined ->
		hipe_hot_cold:finalize(SparcCfg4a,{-1,[]}, Options);
	    true ->
		hipe_hot_cold:finalize(SparcCfg4a,
				       hipe_hot_cold:get_hotness(Fun),Options);
	    Hotness ->
		hipe_hot_cold:finalize(SparcCfg4a,Hotness, Options)
	end,
    ?when_option(time, Options, ?stop_timer("finalize done.")),
    case property_lists:get_value(pp_native,Options) of
	true ->
	    {_,Start} = Entry,
	    hipe_sparc:pp(hipe_sparc:mk_sparc(Fun, Start,
					      hipe_sparc_cfg:data(SparcCfg4a), {},{})),
	    {M,F,A} = Fun,
	    Fname =
		atom_to_list(M)++"_"++atom_to_list(F)++"_"++integer_to_list(A),
	    lists:map(fun ({_,Code}) ->
			      hipe_sparc:pp_instrs(Code,
					      standard_io,Fname) end,Rest),
	    
	    io:format("~n~n", []);
	{file, FileName} ->
	    {ok, File} = file:open(FileName, [write,append]),
	    {_,Start} = Entry,
	    hipe_sparc:pp(hipe_sparc:mk_sparc(Fun,
					      Start,
					      hipe_sparc_cfg:data(SparcCfg4a), {},{}),File),
	    {M,F,A} = Fun,
	    Fname =
		atom_to_list(M)++"_"++atom_to_list(F)++"_"++integer_to_list(A),
	    lists:map(fun ({_,Code}) -> hipe_sparc:pp_instrs(Code,
							File,Fname) end,Rest),
	    
	    io:format("~n~n", []);
	%%	 hipe_sparc:pp(Entry, File),hipe_sparc:pp(Rest, File);
	{only,Lst} ->
	    case lists:member(Fun,Lst) of
		true ->
		{_,Start} = Entry,
		hipe_sparc:pp(hipe_sparc:mk_sparc(Fun, Start,hipe_sparc_cfg:data(SparcCfg4a), {},{})),
		{M,F,A} = Fun,
		Fname =
		  atom_to_list(M)++"_"++atom_to_list(F)++"_"++integer_to_list(A),
		lists:map(fun ({_,Code}) ->
			      hipe_sparc:pp_instrs(Code,
					      standard_io,Fname) end,Rest),
		
		io:format("~n~n", []);
		false ->
		    ok
	    end;
	_ ->
	    ok
    end,
    %%   debug("Size: ~w~n", [hipe_sparc:sparc_size(Entry)+
    %%			hipe_sparc:sparc_size(Rest)], Options),
    {Entry, Rest, hipe_sparc:sparc_data(SparcCfg4a)}.

% %% Construct an unoptimized Sparc CFG

% rtl_to_sparc_cfg(Fun, Rtl8, Options) ->
%   report("sparc~n", [], property_lists:get_bool(verbose,Options)),
%   debug('rtl -> sparc~n',Options),
%   Sparc0    = hipe_rtl2sparc:translate(Rtl8),
%   SparcCfg1 = hipe_sparc_cfg:init(Sparc0),
%   case property_lists:get_bool(sparc_opt_cfg,Options) of
%     true ->
%       rtl_to_sparc(Fun, SparcCfg1, Options);
%     false ->
%       SparcCfg1
%   end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

report(F, A, true) ->
    ?msg(F, A);
report(F, A, false) ->
    true.

debug(Text,Options) ->
    debug(Text,[],Options).

debug(Text,Args,Options) ->
    case property_lists:get_bool(debug,Options) of
	true ->
	    ?msg(Text,Args);
	false ->
	    ok
    end.

hash(X) ->
    erlang:phash(X, 16#7f3f5f1).
