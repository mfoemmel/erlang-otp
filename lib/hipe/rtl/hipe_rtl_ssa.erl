%% -*- erlang-indent-level: 2 -*-
%%----------------------------------------------------------------------
%% File    : hipe_rtl_ssa.erl
%% Author  : Kostis Sagonas <kostis@it.uu.se>
%% Created : 30 Jan 2004
%% Purpose : Provides interface functions for converting RTL code into
%%	     SSA form and back using the generic SSA converter.
%%----------------------------------------------------------------------

-module(hipe_rtl_ssa).

%% The following defines are needed by the included file below
-define(CODE, hipe_rtl).
-define(CFG,  hipe_rtl_cfg).
-define(LIVENESS, hipe_rtl_liveness).
-export([uses_to_rename/1]).	%% needed by hipe_rtl_ssa_const_prop

-include("../ssa/hipe_ssa.inc").
     
%%----------------------------------------------------------------------
%% Auxiliary operations which seriously differ between Icode and RTL.
%%----------------------------------------------------------------------

defs_to_rename(Statement) ->
  Defs = hipe_rtl:defines(Statement),
  lists:filter(fun(X) -> not hipe_rtl_arch:is_precoloured(X) end, Defs).

uses_to_rename(Statement) ->
  Uses = hipe_rtl:uses(Statement),
  lists:filter(fun(X) -> not hipe_rtl_arch:is_precoloured(X) end, Uses).

liveout_no_succ() ->
  hipe_rtl_arch:live_at_return().

%-----------------------------------------------------------------------

reset_var_indx() ->
  hipe_gensym:set_var(rtl, hipe_rtl_arch:first_virtual_reg()).

%%----------------------------------------------------------------------

is_fp_temp(Temp) ->
  hipe_rtl:is_fpreg(Temp).

mk_new_fp_temp() ->
  hipe_rtl:mk_new_fpreg().

%-----------------------------------------------------------------------
%% Procedure : makePhiMove 
%% Purpose   : Create an RTL-specific version of a move instruction
%%             depending on the type of the arguments.
%% Arguments : Dst, Src - the arguments of a Phi instruction that is
%%                        to be moved up the predecessor block as part
%%                        of the SSA un-convert phase.
%% Returns   : Code
%% Note      : ?CODE here is hipe_rtl
%%----------------------------------------------------------------------

makePhiMove(Dst, Src) ->
  case hipe_rtl:is_fpreg(Dst) of
    false ->
      case hipe_rtl:is_fpreg(Src) of %% this test is just a sanity check
	false ->
	  hipe_rtl:mk_move(Dst, Src)
      end;
    true ->
      case hipe_rtl:is_fpreg(Src) of %% this test is just a sanity check
	true ->
	  hipe_rtl:mk_fmove(Dst, Src)
      end
  end.

%-----------------------------------------------------------------------
