%%%----------------------------------------------------------------------
%%% File    : hipe_rtl_ssa_check.erl
%%% Author  : Per Gustafsson
%%%          
%%% Purpose : This file contains the function to check that a CFG is on
%%%           SSA form
%%%           
%%% Created : 18 Mar 2002 by  
%%%----------------------------------------------------------------------
-module(hipe_rtl_ssa_check).
-define(cfg,hipe_rtl_cfg).
-define(code,hipe_rtl).  
-include("hipe_rtl.hrl").
-include("../ssa/hipe_ssa_checker.inc").

