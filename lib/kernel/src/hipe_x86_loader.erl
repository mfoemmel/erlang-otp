%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%%  Filename :           hipe_x86_loader
%%  Module   :	hipe_x86_loader
%%  Purpose  :  To provide x86 specific code-patching for the loader
%%  Notes    :  
%%              Very x86 specific.
%% Original authors:  Erik Johansson and Ulf Magnusson (ulf.magnusson@ubm-computing.com)
%%
%% ====================================================================
%% Exported functions (short description):
%%  make_native_stub/2
%%  patch_instr/3
%%  offsets_to_addresses/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(hipe_x86_loader).
-export([make_native_stub/2, 
	 patch_instr/3,
	 offsets_to_addresses/2]).

-include("../../hipe/main/hipe.hrl").
-include("../../hipe/rtl/hipe_literals.hrl").

offsets_to_addresses(Os, Base) ->
  [{O+Base,address}|| O <- Os].

patch_instr(Address, Val, Type) ->
  ?debug_msg("Patch, addr:~w, value:~w type:~w\n",
	     [Addr,Val,Type]),
  case Type of
    1 -> 
      Rel32 = Val - Address - 4,
      hipe_bifs:write_s32(Address,Rel32);
    call -> 
      Rel32 = Val - Address - 4,
      hipe_bifs:write_s32(Address,Rel32);
    label ->
      hipe_bifs:write_s32(Address, Val);
    closure -> %% Absolute 32bit (eterm) value
      hipe_bifs:write_u32(Address, Val);
    constant ->
      hipe_bifs:write_u32(Address, Val);
    offset ->
      hipe_bifs:write_u32(Address, Val);
    atom ->
      hipe_bifs:write_u32(Address, Val);
    _ ->
      ?debug_msg("Error: Undefined Callback, FIXME\n",[]),
      ?EXIT(undefinedcallback)
  end.


%% Native code stub must be encoded manually since encode module is
%% not available at bootloader time...
make_native_stub(Address, Arity) ->
  %%
  %% This creates a native code stub with the following contents:
  %%
  %% movl $Address, P_BEAM_IP(%ebp)
  %% movb $Arity, P_ARITY(%ebp)
  %% jmp callemu
  %%
  %% The stub has variable size, depending on whether the P_BEAM_IP
  %% and P_ARITY offsets fit in 8-bit signed displacements or not.
  %% The rel32 offset in the final jmp depends on its actual location,
  %% which also depends on the size of the previous instructions.
  %% Arity is stored with a movb because (a) Björn tells me arities
  %% are <= 255, and (b) a movb is smaller and faster than a movl.
  %%
  %% The strange looking code below assembles the stub backwards.
  %%
  CodeSize =	% 16, 19, or 22 bytes
    16 +	% 16 when both offsets are 8-bit
    if ?P_BEAM_IP >= 128 -> 3 ; true -> 0 end +
    if ?P_ARITY >= 128 -> 3 ; true -> 0 end,
  Mem = hipe_bifs:alloc_code(CodeSize),
  CallEmuOffset = hipe_bifs:primop_address(callemu) - (Mem + CodeSize),
  Code4 = [%% jmp callemu; 5 bytes
	   16#e9,
	   CallEmuOffset band 16#ff,
	   (CallEmuOffset bsr 8) band 16#ff,
	   (CallEmuOffset bsr 16) band 16#ff,
	   (CallEmuOffset bsr 24) band 16#ff],
  Code3 = [%% the 8-bit Arity immediate; 1 byte
	   Arity |
	   Code4],
  Code2 = [%% movb ..., P_ARITY(%ebp); 3 or 6 bytes
	   16#c6 |
	   if ?P_ARITY >= 128 ->
	       [16#85,	% disp32[EBP]
		?P_ARITY band 16#ff,
		(?P_ARITY bsr 8) band 16#ff,
		(?P_ARITY bsr 16) band 16#ff,
		(?P_ARITY bsr 24) band 16#ff |
		Code3];
	      true ->
	       [16#45,	% disp8[EBP]
		?P_ARITY |
		Code3]
	   end],
  Code1 = [%% the 32-bit Address immediate; 4 bytes
	   Address band 16#ff,
	   (Address bsr 8) band 16#ff,
	   (Address bsr 16) band 16#ff,
	   (Address bsr 24) band 16#ff |
	   Code2],
  Code  = [%% movl ..., P_BEAM_IP(%ebp); 3 or 6 bytes
	   16#c7 |
	   if ?P_BEAM_IP >= 128 ->
	       [16#85,	% disp32[EBP]
		?P_BEAM_IP band 16#ff,
		(?P_BEAM_IP bsr 8) band 16#ff,
		(?P_BEAM_IP bsr 16) band 16#ff,
		(?P_BEAM_IP bsr 24) band 16#ff |
		Code1];
	      true ->
	       [16#45,	% disp8[EBP]
		?P_BEAM_IP |
		Code1]
	   end],
  hipe_unified_loader:write_bytes(Code, Mem),
  Mem.

  
