%%% -*- erlang-indent-level: 2 -*-
%%% $Id$
%%% amd64_liveness -- compute register liveness for amd64 CFGs

-module(hipe_amd64_liveness).
-export([analyse/1]).

-define(LIVEOUT_NEEDED,true).   % needed for liveness.inc below.

-include("../flow/liveness.inc").

analyse(CFG) -> analyze(CFG).
cfg_bb(CFG,L) -> hipe_amd64_cfg:bb(CFG,L).
cfg_postorder(CFG) -> hipe_amd64_cfg:postorder(CFG).
cfg_succ_map(CFG) -> hipe_amd64_cfg:succ_map(CFG).
cfg_succ(CFG,L) -> hipe_amd64_cfg:succ(CFG,L).
uses(Insn) -> hipe_amd64_defuse:insn_use(Insn).
defines(Insn) -> hipe_amd64_defuse:insn_def(Insn).
liveout_no_succ() ->
    ordsets:from_list(lists:map(fun({Reg,Type}) ->
				    hipe_x86:mk_temp(Reg,Type)
				end,
				hipe_amd64_registers:live_at_return())).

-ifdef(DEBUG_LIVENESS).
cfg_labels(CFG) -> hipe_amd64_cfg:labels(CFG).
cfg_bb_add(CFG,L,NewBB) -> hipe_amd64_cfg:bb_add(CFG,L,NewBB).
mk_comment(Text) -> hipe_x86:mk_comment(Text).
-endif.
