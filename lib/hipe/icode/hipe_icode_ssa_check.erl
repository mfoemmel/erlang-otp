%%%----------------------------------------------------------------------
%%% File    : rename.erl
%%% Author  : Christoffer Vikström
%%%           Daniel Deogun
%%%           Jesper Bengtsson
%%% Purpose : This file contains the function to perform the rename pass of
%%%           ssa.
%%% Created : 18 Mar 2002 by 
%%%----------------------------------------------------------------------
-module(hipe_icode_ssa_check).
-define(cfg,hipe_icode_cfg). 
-define(code,hipe_icode).    
-include("../ssa/hipe_ssa_checker.inc").
