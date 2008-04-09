%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

-module(hipe_sparc_main).
-export([rtl_to_sparc/3]).

rtl_to_sparc(MFA, RTL, Options) ->
  Defun1 = hipe_rtl_to_sparc:translate(RTL),
  %% io:format("~w: after translate\n", [?MODULE]),
  %% hipe_sparc_pp:pp(Defun1),
  Defun2 = hipe_sparc_ra:ra(Defun1, Options),
  %% io:format("~w: after regalloc\n", [?MODULE]),
  %% hipe_sparc_pp:pp(Defun2),
  Defun3 = hipe_sparc_frame:frame(Defun2),
  %% io:format("~w: after frame\n", [?MODULE]),
  %% hipe_sparc_pp:pp(Defun3),
  Defun4 = hipe_sparc_finalise:finalise(Defun3),
  %% io:format("~w: after finalise\n", [?MODULE]),
  pp(Defun4, MFA, Options),
  {native, sparc, {unprofiled, Defun4}}.

pp(Defun, MFA, Options) ->
  case proplists:get_value(pp_native, Options) of
    true ->
      hipe_sparc_pp:pp(Defun);
    {only,Lst} when is_list(Lst) ->
      case lists:member(MFA,Lst) of
	true ->
	  hipe_sparc_pp:pp(Defun);
	false ->
	  ok
      end;
    {only,MFA} ->
       hipe_sparc_pp:pp(Defun);
    {file,FileName} ->
      {ok, File} = file:open(FileName, [write,append]),
      hipe_sparc_pp:pp(File, Defun),
      ok = file:close(File);
    _ ->
      ok
  end.
