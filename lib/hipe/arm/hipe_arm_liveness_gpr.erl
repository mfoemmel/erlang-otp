%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

-module(hipe_arm_liveness_gpr).
-export([analyse/1]).
-export([liveout/2]).
-include("../flow/liveness.inc").

analyse(CFG) -> analyze(CFG).
cfg_bb(CFG, L) -> hipe_arm_cfg:bb(CFG, L).
cfg_postorder(CFG) -> hipe_arm_cfg:postorder(CFG).
cfg_succ_map(CFG) -> hipe_arm_cfg:succ_map(CFG).
cfg_succ(CFG, L) -> hipe_arm_cfg:succ(CFG, L).
uses(Insn) -> hipe_arm_defuse:insn_use_gpr(Insn).
defines(Insn) -> hipe_arm_defuse:insn_def_gpr(Insn).
liveout_no_succ() ->
  ordsets:from_list(lists:map(fun({Reg,Type}) ->
				  hipe_arm:mk_temp(Reg, Type)
			      end,
			      hipe_arm_registers:live_at_return())).
