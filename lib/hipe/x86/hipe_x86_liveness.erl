%%% -*- erlang-indent-level: 2 -*-
%%% $Id$
%%% x86_liveness -- compute register liveness for x86 CFGs

-module(hipe_x86_liveness).
-export([analyse/1]).
-export([liveout/2]).
-include("../flow/liveness.inc").

analyse(CFG) -> analyze(CFG).
cfg_bb(CFG, L) -> hipe_x86_cfg:bb(CFG, L).
cfg_postorder(CFG) -> hipe_x86_cfg:postorder(CFG).
cfg_succ_map(CFG) -> hipe_x86_cfg:succ_map(CFG).
cfg_succ(CFG, L) -> hipe_x86_cfg:succ(CFG, L).
uses(Insn) -> hipe_x86_defuse:insn_use(Insn).
defines(Insn) -> hipe_x86_defuse:insn_def(Insn).
liveout_no_succ() ->
  ordsets:from_list(lists:map(fun({Reg,Type}) ->
				  hipe_x86:mk_temp(Reg, Type)
			      end,
			      hipe_x86_registers:live_at_return())).
