%% -*- erlang-indent-level: 2 -*-
%% =====================================================================
%%  Filename :  hipe_x86_loader.erl
%%  Authors  :  Erik Johansson and Ulf Magnusson
%%  Purpose  :  Provides x86-specific code patching for the HiPE loader.
%% =====================================================================
%% Exported functions (short description):
%%  patch_instr/3
%%  offsets_to_addresses/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_x86_loader).
-export([patch_instr/3]).

-define(HIPE_LOGGING,true).
-include("../../hipe/main/hipe.hrl").

patch_instr(Address, Val, Type) ->
  ?debug_msg("Patch, addr:~w, value:~w type:~w\n",
	     [Address,Val,Type]),
  case Type of
%%    1 -> 
%%      Rel32 = Val - Address - 4,
%%      hipe_bifs:write_s32(Address,Rel32);
    call ->
      Rel32 = Val - Address - 4,
      hipe_bifs:write_s32(Address,Rel32);
%%    label ->
%%      hipe_bifs:write_s32(Address, Val);
    closure -> %% Absolute 32bit (eterm) value
      hipe_bifs:write_u32(Address, Val);
    constant ->
      hipe_bifs:write_u32(Address, Val);
%%    offset ->
%%      hipe_bifs:write_u32(Address, Val);
    atom ->
      hipe_bifs:write_u32(Address, Val);
    c_const ->
      hipe_bifs:write_u32(Address, Val);
    x86_abs_pcrel ->
      hipe_bifs:write_u32(Address, Address+Val);
    _ ->
      ?debug_msg("Error: Undefined Callback, FIXME\n",[]),
      ?EXIT("undefined callback")
  end.
