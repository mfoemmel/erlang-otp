%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

-ifdef(HIPE_AMD64).
-define(HIPE_X86_MAIN, hipe_amd64_main).
-define(RTL_TO_X86, rtl_to_amd64). % XXX: kill this crap
-define(HIPE_RTL_TO_X86, hipe_rtl_to_amd64).
-define(HIPE_X86_RA, hipe_amd64_ra).
-define(HIPE_X86_FRAME, hipe_amd64_frame).
-define(HIPE_X86_PP, hipe_amd64_pp).
-define(X86TAG, amd64). % XXX: kill this crap
-define(X86STR, "amd64").
-else.
-define(HIPE_X86_MAIN, hipe_x86_main).
-define(RTL_TO_X86, rtl_to_x86). % XXX: kill this crap
-define(HIPE_RTL_TO_X86, hipe_rtl_to_x86).
-define(HIPE_X86_RA, hipe_x86_ra).
-define(HIPE_X86_FRAME, hipe_x86_frame).
-define(HIPE_X86_PP, hipe_x86_pp).
-define(X86TAG, x86). % XXX: kill this crap
-define(X86STR, "x86").
-endif.

-module(?HIPE_X86_MAIN).
-export([?RTL_TO_X86/3]). % XXX: change to 'from_rtl' to avoid $ARCH substring

-ifndef(DEBUG).
-define(DEBUG,1).
-endif.
-define(HIPE_INSTRUMENT_COMPILER, true). %% Turn on instrumentation.
-include("../main/hipe.hrl").

?RTL_TO_X86(MFA, RTL, Options) ->
  Translated = ?option_time(?HIPE_RTL_TO_X86:translate(RTL),
			    "RTL-to-"?X86STR, Options),
  Allocated  = ?option_time(?HIPE_X86_RA:ra(Translated, Options),
			    ?X86STR" register allocation", Options),
  Framed     = ?option_time(?HIPE_X86_FRAME:frame(Allocated, Options), 
			    ?X86STR" frame", Options),
  Finalised  = ?option_time(hipe_x86_postpass:postpass(Framed, Options),
			    ?X86STR" finalise", Options),
  ?HIPE_X86_PP:optional_pp(Finalised, MFA, Options),
  {native, ?X86TAG, {unprofiled, Finalised}}.
