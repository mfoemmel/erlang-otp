%%%----------------------------------------------------------------------
%%% File    : hipe_icode_ssa_rename.erl
%%% Author  : Christoffer Vikström
%%%           Daniel Deogun
%%%           Jesper Bengtsson
%%% Purpose : This file contains the function that propagates the assignments
%%%           in the phi-functions up to their appropriate predecessor node.
%%% Created : 18 Mar 2002 by 
%%%----------------------------------------------------------------------
-module(hipe_icode_ssa_propagate).
-define(cfg,hipe_icode_cfg).
-define(code,hipe_icode).
-define(hash, hipe_hash).
-include("../ssa/hipe_ssa_propagate.inc").
