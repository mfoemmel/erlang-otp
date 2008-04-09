%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Icode version of extended basic blocks.
%%
 
-module(hipe_icode_ebb).

-define(CFG, hipe_icode_cfg).

-include("hipe_icode.hrl").
-include("../flow/cfg.hrl").
-include("../flow/ebb.inc").
