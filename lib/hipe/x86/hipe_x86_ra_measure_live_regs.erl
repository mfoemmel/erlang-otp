%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%%  Filename : 	hipe_x86_ra_measure_live_regs.erl
%%  Module   :	hipe_x86_ra_measure_live_regs
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-08-09 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: happi $
%%              $Date: 2001/09/05 15:26:51 $
%%              $Revision: 1.1 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_x86_ra_measure_live_regs).
-export([measure/2]).

measure(X86Defun,Options) ->
  NewDefun = X86Defun, 
  CFG = hipe_x86_cfg:init(NewDefun),
  Liveness = hipe_x86_liveness:analyse(CFG),
  Labels = hipe_x86_cfg:labels(CFG),
  {_,Max} =  hipe_x86_cfg:var_range(CFG),
  V = vector:from_list((lists:duplicate(Max+1,0))),
  R = measure(Labels, V, CFG, Liveness),
  lists:sum(vector:to_list(R)).

measure([L|Ls], V , CFG, Liveness) ->
  Code = hipe_bb:code(hipe_x86_cfg:bb(CFG,L)),
  V1 = traverse(Code, V, hipe_x86_liveness:liveout(Liveness,L)),
  measure(Ls, V1, CFG, Liveness);
measure([],V ,_ ,_) -> V.

traverse([I], V , Live) -> 
  case hipe_x86:insn_type(I) of
    call ->
      set(Live, V);
    pseudo_call ->
      set(Live, V);
    _ -> V
  end;
traverse([I|Is], V , Live) -> 
  traverse(Is, V, Live);
traverse([], V, Live) -> 
  V.

set([], V) ->
  V;
set([R|Rs], V) ->
  N = hipe_x86:temp_reg(R),
  case hipe_x86_registers:is_precoloured(N) of
    true -> set(Rs, V);
    false ->
      case hipe_x86:temp_is_allocatable(R) of
	true ->
	  set(Rs, vector:set(N+1, V, 1));
	false -> set(Rs, V)
      end
  end.

