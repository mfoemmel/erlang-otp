%%% -*- erlang-indent-level: 2 -*-
%%% $Id: hipe_x86_liveness.erl,v 1.8 2005/03/29 19:39:47 mikpe Exp $
%%% x86_liveness -- compute register liveness for x86 CFGs

-ifndef(HIPE_X86_LIVENESS).
-define(HIPE_X86_LIVENESS,	hipe_x86_liveness).
-define(HIPE_X86_DEFUSE,	hipe_x86_defuse).
-define(HIPE_X86_REGISTERS,	hipe_x86_registers).
-endif.

-module(?HIPE_X86_LIVENESS).
-export([analyse/1]).
-export([liveout/2]).
-include("../flow/liveness.inc").

analyse(CFG) -> analyze(CFG).
cfg_bb(CFG, L) -> hipe_x86_cfg:bb(CFG, L).
cfg_postorder(CFG) -> hipe_x86_cfg:postorder(CFG).
cfg_succ_map(CFG) -> hipe_x86_cfg:succ_map(CFG).
cfg_succ(CFG, L) -> hipe_x86_cfg:succ(CFG, L).
uses(Insn) -> ?HIPE_X86_DEFUSE:insn_use(Insn).
defines(Insn) -> ?HIPE_X86_DEFUSE:insn_def(Insn).
liveout_no_succ() ->
  ordsets:from_list(lists:map(fun({Reg,Type}) ->
				  hipe_x86:mk_temp(Reg, Type)
			      end,
			      ?HIPE_X86_REGISTERS:live_at_return())).

-ifdef(DEBUG_LIVENESS).
cfg_labels(CFG) -> hipe_x86_cfg:labels(CFG).
cfg_bb_add(CFG,L,NewBB) -> hipe_x86_cfg:bb_add(CFG,L,NewBB).
mk_comment(Text) -> hipe_x86:mk_comment(Text).
-endif.
