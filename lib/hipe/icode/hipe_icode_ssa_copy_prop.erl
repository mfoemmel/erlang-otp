%%-------------------------------------------------------------------
%% File        : hipe_icode_ssa_copy_prop.erl
%% Author      : Tobias Lindahl <tobiasl@it.uu.se>
%% Description : Performs copy propagation on SSA form.
%%
%% Created     : 4 Apr 2003 by Tobias Lindahl <tobiasl@it.uu.se>
%%-------------------------------------------------------------------

-module(hipe_icode_ssa_copy_prop).

-define(code, hipe_icode).
-define(cfg, hipe_icode_cfg).

-include("../ssa/hipe_ssa_copy_prop.inc").
