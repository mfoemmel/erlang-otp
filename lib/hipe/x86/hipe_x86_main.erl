%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

-module(hipe_x86_main).
-export([rtl_to_x86/3]).

-ifndef(DEBUG).
-define(DEBUG,1).
-endif.
-define(HIPE_INSTRUMENT_COMPILER, true). %% Turn on instrumentation.
-include("../main/hipe.hrl").

rtl_to_x86(MFA, RTL, Options) ->
  ?when_option(time, Options, ?start_timer("RTL-to-x86")),
  X86a = hipe_rtl_to_x86:translate(RTL),
  ?when_option(time, Options, ?stop_timer("RTL-to-x86")),
  ?when_option(time, Options, ?start_timer("register allocation")),
  X86b = hipe_x86_ra:ra(X86a, Options),
  ?when_option(time, Options, ?stop_timer("register allocation")),
  ?when_option(time, Options, ?start_timer("x86 frame")),
  X86c = hipe_x86_frame:frame(X86b, Options),
  ?when_option(time, Options, ?stop_timer("x86 frame")),
  ?when_option(time, Options, ?start_timer("x86 finalise")),
  X86d = x86_finalise(X86c, Options),
  ?when_option(time, Options, ?stop_timer("x86 finalise")),
  x86_pp(X86d, MFA, Options),
  {native, x86, {unprofiled, X86d}}.

%%% use option no_finalise_x86 to disable calling hipe_x86_postpass
x86_finalise(X86, Options) ->
  case proplists:get_value(finalise_x86, Options, true) of
    true ->
      hipe_x86_postpass:postpass(X86);
    false ->
      X86	% illegal code, but allows exercising the compiler
  end.

x86_pp(X86, MFA, Options) ->
  case proplists:get_value(pp_native, Options) of
    true ->
      hipe_x86_pp:pp(X86);
    {only,Lst} when is_list(Lst) ->
      case lists:member(MFA,Lst) of
	true ->
	  hipe_x86_pp:pp(X86);
	false ->
	  ok
      end;
    {only,MFA} ->
       hipe_x86_pp:pp(X86);
    {file,FileName} ->
      {ok, File} = file:open(FileName, [write,append]),
      hipe_x86_pp:pp(File, X86),
      file:close(File);
    _ ->
      []
  end.
