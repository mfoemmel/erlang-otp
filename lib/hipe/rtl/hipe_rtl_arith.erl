%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2002 by Erik Johansson.  
%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%%  Filename : 	hipe_rtl_arith.erl
%%  Module   :	hipe_rtl_arith
%%  Purpose  :  To implement RTL-arithmetic 
%%  Notes    :  This module is somewhat prepared for 64-bit arithmatic
%%              but at the moment only 32-bit arithmetic is implemented.
%%              The wordsize of registers and arithmetic operation is
%%              dependent on the target. At the moment only 32-bit words
%%              are supported.
%%              XXX: Fix this module for 64-bit support.    
%%  History  :	* 2002-10-23 Erik Stenman (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: pergu $
%%              $Date: 2003/04/23 11:58:14 $
%%              $Revision: 1.3 $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-module(hipe_rtl_arith).
-export([eval_alu/3,eval_alub/4, eval_cond/3]).

-define(ARITH_WS,hipe_rtl_arith_32).

eval_alu(Op, Arg1, Arg2) ->
 %% io:format("Evaluated alu: ~w ~w ~w = ",[Arg1, Op, Arg2]),
  Res=?ARITH_WS:eval_alu(Op, Arg1, Arg2),
 %% io:format("~w~n ",[Res]),
  Res.
eval_alub(Op, Cond, Arg1, Arg2) ->
 %% io:format("Evaluated alub: ~w ~w ~w cond ~w = ",[Arg1, Op, Arg2, Cond]),
  Res=?ARITH_WS:eval_alub(Op, Cond, Arg1, Arg2),
 %% io:format("~w~n ",[Res]),
  Res.
eval_cond(Cond, Arg1, Arg2) ->
 %%io:format("Evaluated cond: ~w ~w ~w = ",[Arg1, Cond, Arg2]),
  Res=?ARITH_WS:eval_cond(Cond, Arg1, Arg2),
 %%io:format("~w~n ",[Res]),
  Res.
