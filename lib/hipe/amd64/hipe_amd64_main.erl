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
  ?when_option(time, Options, ?start_timer("RTL-to-amd64")),
  Translation = hipe_rtl_to_amd64:translate(RTL),
  ?when_option(time, Options, ?stop_timer("RTL-to-amd64")),
  ?when_option(time, Options, ?start_timer("amd64 register allocation")),
  Allocated = hipe_amd64_ra:ra(Translation, Options),
  ?when_option(time, Options, ?stop_timer("amd64 register allocation")),
  ?when_option(time, Options, ?start_timer("amd64 frame")),
  Framed = amd64_frame(Allocated, Options),
  ?when_option(time, Options, ?stop_timer("amd64 frame")),
  ?when_option(time, Options, ?start_timer("amd64 finalise")),
  Finalised = amd64_finalise(Framed, Options),
  ?when_option(time, Options, ?stop_timer("amd64 finalise")),
  Amd64Code = Finalised,
  amd64_pp(Amd64Code, MFA, Options),
  {native, amd64, {unprofiled, Amd64Code}}.

amd64_finalise(Amd64Defun, Options) ->
  hipe_amd64_postpass:postpass(Amd64Defun, Options).

amd64_frame(Amd64Defun, Options) ->
  hipe_amd64_frame:frame(Amd64Defun, Options).
  
amd64_pp(Amd64, MFA, Options) ->
  case proplists:get_value(pp_native, Options) of
    true ->
      hipe_amd64_pp:pp(Amd64);
    {only,Lst} when is_list(Lst) ->
      case lists:member(MFA,Lst) of
	true ->
	  hipe_amd64_pp:pp(Amd64);
	false ->
	  ok
      end;
    {only,MFA} ->
       hipe_amd64_pp:pp(Amd64);
    {file,FileName} ->
      {ok,File} = file:open(FileName, [write,append]),
      hipe_amd64_pp:pp(File, Amd64),
      file:close(File);
    _ ->
      []
  end.
