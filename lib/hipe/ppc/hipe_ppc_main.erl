%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

-module(hipe_ppc_main).
-export([rtl_to_ppc/3]).

rtl_to_ppc(MFA, RTL, Options) ->
  stupid(0),
  PPC1 = hipe_rtl_to_ppc:translate(RTL),
  PPC2 = hipe_ppc_ra:ra(PPC1, Options),
  PPC3 = hipe_ppc_frame:frame(PPC2),
  PPC4 = hipe_ppc_finalise:finalise(PPC3),
  ppc_pp(PPC4, MFA, Options),
  {native, powerpc, {unprofiled, PPC4}}.

stupid(0) -> [];
stupid(_) ->
  hipe_ppc_liveness:analyze([]),
  hipe_ppc_liveness:livein([],[]).

ppc_pp(PPC, MFA, Options) ->
  case proplists:get_value(pp_native, Options) of
    true ->
      hipe_ppc_pp:pp(PPC);
    {only,Lst} when is_list(Lst) ->
      case lists:member(MFA,Lst) of
	true ->
	  hipe_ppc_pp:pp(PPC);
	false ->
	  ok
      end;
    {only,MFA} ->
       hipe_ppc_pp:pp(PPC);
    {file,FileName} ->
      {ok, File} = file:open(FileName, [write,append]),
      hipe_ppc_pp:pp(File, PPC),
      file:close(File);
    _ ->
      []
  end.
