%%% -*- erlang-indent-level: 2 -*-
%%% $Id: hipe_ppc_finalise.erl,v 1.5 2004/06/23 08:34:59 mikpe Exp $

-module(hipe_ppc_finalise).
-export([finalise/1]).
-include("hipe_ppc.hrl").

finalise(Defun) ->
  #defun{code=Code0} = Defun,
  Code1 = peep(expand(Code0)),
  Defun#defun{code=Code1}.

expand(Insns) ->
  expand_list(Insns, []).

expand_list([I|Insns], Accum) ->
  expand_list(Insns, expand_insn(I, Accum));
expand_list([], Accum) ->
  lists:reverse(Accum).

expand_insn(I, Accum) ->
  case I of
    #pseudo_bc{bcond=BCond,true_label=TrueLab,false_label=FalseLab,pred=Pred} ->
      [hipe_ppc:mk_b_label(FalseLab),
       hipe_ppc:mk_bc(BCond, TrueLab, Pred) |
       Accum];
    #pseudo_call{func=FunC,sdesc=SDesc,contlab=ContLab,linkage=Linkage} ->
      [hipe_ppc:mk_b_label(ContLab),
       case FunC of
	 'ctr' -> hipe_ppc:mk_bctrl(SDesc);
	 Fun -> hipe_ppc:mk_bl(Fun, SDesc, Linkage)
       end |
       Accum];
    #pseudo_tailcall_prepare{} ->
      Accum;
    _ ->
      [I|Accum]
  end.

peep(Insns) ->
  peep_list(Insns, []).

peep_list([#b_label{label=Label} | (Insns = [#label{label=Label}|_])], Accum) ->
  peep_list(Insns, Accum);
peep_list([I|Insns], Accum) ->
  peep_list(Insns, [I|Accum]);
peep_list([], Accum) ->
  lists:reverse(Accum).
