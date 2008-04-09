%% -*- erlang-indent-level: 2 -*-
%%-------------------------------------------------------------------
%% File        : hipe_icode_ssa_copy_prop.erl
%% Author      : Tobias Lindahl <tobiasl@it.uu.se>
%% Description : Performs copy propagation on SSA form.
%%
%% Created     : 4 Apr 2003 by Tobias Lindahl <tobiasl@it.uu.se>
%%-------------------------------------------------------------------

-module(hipe_icode_ssa_copy_prop).

%%
%% modules given as parameters
%%
-define(code, hipe_icode).
-define(cfg, hipe_icode_cfg).

%%
%% appropriate include files
%%
-include("hipe_icode.hrl").
-include("../flow/cfg.hrl").
-include("../ssa/hipe_ssa_copy_prop.inc").
