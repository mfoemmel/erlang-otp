%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

-module(hipe_ppc_liveness_fpr).
-export([analyse/1]).
-export([liveout/2]).

-include("hipe_ppc.hrl").
-include("../flow/liveness.inc").

analyse(CFG) -> analyze(CFG).
cfg_bb(CFG, L) -> hipe_ppc_cfg:bb(CFG, L).
cfg_postorder(CFG) -> hipe_ppc_cfg:postorder(CFG).
cfg_succ(CFG, L) -> hipe_ppc_cfg:succ(CFG, L).
uses(Insn) -> hipe_ppc_defuse:insn_use_fpr(Insn).
defines(Insn) -> hipe_ppc_defuse:insn_def_fpr(Insn).
liveout_no_succ() -> [].
