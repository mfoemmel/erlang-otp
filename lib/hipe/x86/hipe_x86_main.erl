%%% -*- erlang-indent-level: 2 -*-
%%% $Id: hipe_x86_main.erl,v 1.4 2005/02/22 15:12:55 dalu7049 Exp $

-module(hipe_x86_main).
-export([rtl_to_x86/3]).

-ifndef(DEBUG).
-define(DEBUG,1).
-endif.
-define(HIPE_INSTRUMENT_COMPILER, true). %% Turn on instrumentation.
-include("../main/hipe.hrl").

rtl_to_x86(MFA, RTL, Options) ->
  Translated = ?option_time(hipe_rtl_to_x86:translate(RTL),
			    "RTL-to-x86", Options),
  Allocated  = ?option_time(hipe_x86_ra:ra(Translated, Options),
			    "x86 register allocation", Options),
  Framed     = ?option_time(hipe_x86_frame:frame(Allocated, Options), 
			    "x86 frame", Options),
  Finalised  = ?option_time(hipe_x86_postpass:postpass(Framed, Options),
			    "x86 finalise", Options),
  hipe_x86_pp:optional_pp(Finalised, MFA, Options),
  {native, x86, {unprofiled, Finalised}}.
