%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%%  Filename : 	hipe_sparc_loader.erl
%%  Module   :	hipe_sparc_loader
%%  Purpose  :  To provide SPARC specific code-patching for the loader
%%  Notes    :  
%%              Very sparc specific.
%%              Also, the native code-stubs for non-existing functions
%%               depend on the use of specific sparc registers.
%%
%% ====================================================================
%% Exported functions (short description):
%%  patch_instr/3
%%  offsets_to_addresses/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_sparc_loader).
-export([patch_instr/3]).
%%-define(DEBUG,true).
-define(HIPE_LOGGING,true).
-include("../../hipe/main/hipe.hrl").

%% ____________________________________________________________________
%% 
patch_instr(Addr, Val, Type) ->
  case Type of
    call -> patch_call_instr(Addr, Val);
    load_mfa -> patch_hi_or(Addr, Val);
    atom -> patch_hi_or(Addr,Val);
    constant -> patch_hi_or(Addr,Val);
    closure -> patch_hi_or(Addr,Val);
    c_const -> patch_hi_or(Addr,Val)
  end.

patch_call_instr(Address,DestAddress) ->
  RelDest = DestAddress - Address,
  NewI = ((1 bsl 30) bor (bits_32(RelDest) bsr 2)),
  hipe_bifs:write_u32(Address,NewI).

patch_hi_or(Address,Value) ->
  Hi = (Value bsr 10),
  patch_sethi(Address,Hi),
  Lo = (Value band 16#3FF),
  patch_alu(Address+4,Lo).

patch_sethi(Addr,Bits) ->
  I = hipe_bifs:read_u32(Addr),
  NewI = ((I band (2#1111111111 bsl 22)) bor bits_22(Bits)),
  hipe_bifs:write_u32(Addr,NewI).


patch_alu(Addr,Bits) ->
  I = hipe_bifs:read_u32(Addr),
  NewI = ((I band (2#1111111111111111111 bsl 13)) bor bits_13(Bits)),
  hipe_bifs:write_u32(Addr,NewI).


bits_13(X) -> X band     16#1fff.
bits_22(X) -> X band   16#3fffff.
bits_32(X) -> X band 16#ffffffff.
