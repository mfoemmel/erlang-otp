%% -*- erlang-indent-level: 2 -*-
%% $Id$

-module(hipe_amd64_main).
-export([rtl_to_amd64/3]).

-ifndef(DEBUG).
-define(DEBUG,1).
-endif.
-define(HIPE_INSTRUMENT_COMPILER, true). %% Turn on instrumentation.
-include("../main/hipe.hrl").

rtl_to_amd64(MFA, RTL, Options) ->
  Translated = ?option_time(hipe_rtl_to_amd64:translate(RTL),
			    "RTL-to-amd64", Options),
  Allocated  = ?option_time(hipe_amd64_ra:ra(Translated, Options),
			    "amd64 register allocation", Options),
  Framed     = ?option_time(hipe_amd64_frame:frame(Allocated, Options), 
			    "amd64 frame", Options),
  Finalised  = ?option_time(hipe_amd64_postpass:postpass(Framed, Options),
			    "amd64 finalise", Options),
  hipe_amd64_pp:optional_pp(Finalised, MFA, Options),
  {native, amd64, {unprofiled, Finalised}}.
