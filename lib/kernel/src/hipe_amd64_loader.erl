%% -*- erlang-indent-level: 2 -*-
%% ========================================================================
%%  Filename :  hipe_amd64_loader.erl
%%  Author   :  Daniel Luna
%%  Purpose  :  Provides AMD64-specific code patching for the HiPE loader.
%% ========================================================================

-module(hipe_amd64_loader).
-export([patch_instr/3]).

-define(HIPE_LOGGING,true).
-include("../../hipe/main/hipe.hrl").

patch_instr(Address, Val, Type) ->
  ?debug_msg("Patch, addr:~w, value:~w type:~w\n", [Address,Val,Type]),
  case Type of
%     1 -> %%% don't uncomment without testing
%       Rel32 = Val - Address - 4,
%       hipe_bifs:write_s32(Address,Rel32);
    call ->
      Rel32 = Val - Address - 4, %% -4 since this is a 32-bit offset
      hipe_bifs:write_s32(Address,Rel32);
%     label -> %% don't uncomment without testing
%       hipe_bifs:write_s64(Address, Val);
    closure ->
      hipe_bifs:write_u64(Address, Val);
    constant ->
      hipe_bifs:write_u64(Address, Val);
%     offset -> %% don't uncomment without testing
%       hipe_bifs:write_u32(Address, Val);
    atom ->
      hipe_bifs:write_u32(Address, Val);
    c_const ->
      case (Val < 0) or (Val >= (1 bsl 32)) of
	true ->
	  ?EXIT({too_big_c_const, Val});
	false ->
	  hipe_bifs:write_u32(Address, Val)
      end;
%     amd64_abs_pcrel -> %% don't uncomment without testing
%       hipe_bifs:write_u64(Address, Address+Val);
    _ ->
      ?debug_msg("Error: Undefined Callback, FIXME\n",[]),
      ?EXIT({undefinedcallback, Type})
  end.
