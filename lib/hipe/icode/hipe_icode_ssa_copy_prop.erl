%%%-------------------------------------------------------------------
%%% File    : hipe_icode_ssa_copy_prop.erl
%%% Author  : Tobias Lindahl <tobiasl@fan.it.uu.se>
%%% Description : Copy propagation on ssa form.
%%%
%%% Created :  4 Apr 2003 by Tobias Lindahl <tobiasl@fan.it.uu.se>
%%%-------------------------------------------------------------------
-module(hipe_icode_ssa_copy_prop).

-define(code, hipe_icode).
-define(cfg, hipe_icode_cfg).
-include("../ssa/hipe_ssa_copy_prop.inc").
