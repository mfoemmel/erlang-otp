%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%%  Filename : 	hipe_sparc_loader.erl
%%  Module   :	hipe_sparc_loader
%%  Purpose  :  To provide SPARC specific code-patching for the loader
%%  Notes    :  
%%              Very sparc specific.
%%              Also, the native code-stubs for non-existing functions
%%               depends on the use of specific sparc registers.
%%
%% ====================================================================
%% Exported functions (short description):
%%  make_native_stub/2
%%  patch_instr/3
%%  offsets_to_addresses/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(hipe_sparc_loader).
-export([make_native_stub/2, 
	 patch_instr/3,
	 offsets_to_addresses/2]).
%%-define(DEBUG,true).
-include("hipe.hrl").
%% ____________________________________________________________________
%% 
offsets_to_addresses([O|Os],Base) ->
  [{O+Base,sethi},{O+Base+4,'or'}|offsets_to_addresses(Os,Base)];
offsets_to_addresses([],_) -> [].

%% ____________________________________________________________________
%% 
patch_instr(Addr, Val, Type) ->
  case Type of
    call -> patch_call_instr(Addr, Val);
    sethiref -> Hi = (Val bsr 10),
	 patch_sethi(Addr, Hi);
    orref -> Lo = (Val band 16#3FF),
	 patch_alu(Addr, Lo);
    atom -> patch_hi_or(Addr,Val);
    constant -> patch_hi_or(Addr,Val);
    closure -> patch_hi_or(Addr,Val)
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

%% ____________________________________________________________________
%% 
make_native_stub(Address, Arity) ->
  CodeSize = 5*4,
  Mem = hipe_bifs:alloc_code(CodeSize),
  RelOffset = (hipe_bifs:primop_address(callemu)-(Mem+3*4)) ,

  Code = 
    [%% sethi $temp0, hi(Address)
     ((50331648 band (2#1111111111 bsl 22)) bor bits_22((Address bsr 10))), 
     %% move $temp2, $CP
     2886729743, %% 2819620879,
     %% or $temp0, lo(Address)
     ((2182111232 band (2#1111111111111111111 bsl 13)) 
      bor bits_13((Address band 16#3FF))),
     %% Call callemu

     ((1 bsl 30) bor (bits_32(RelOffset) bsr 2)),

     %% mov $temp1, Arity
     ((2920292352 band (2#1111111111111111111 bsl 13)) bor
      bits_13(Arity))],
  hipe_unified_loader:write_words(Code, Mem),
  Mem.










 

      

