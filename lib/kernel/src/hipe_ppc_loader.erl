%%% -*- erlang-indent-level: 2 -*-
%%% $Id$

-module(hipe_ppc_loader).
-export([patch_instr/3]).
-export([patch_call/3]).

%-define(DEBUG,true).
-include("../../hipe/main/hipe.hrl").

patch_instr(Addr, Val, Type) ->
  ?debug_msg("Patch, addr:~w, value:~w type:~w\n", [Addr,Val,Type]),
  case Type of
    closure -> patch_li(Addr, Val);
    constant -> patch_li(Addr, Val);
    atom -> patch_li(Addr, Val);
    c_const -> patch_li(Addr, Val);
    load_mfa -> patch_li(Addr, Val);
    _ -> exit({?MODULE,patch_instr,Addr,Val,Type})
  end.

patch_call(CallAddress, DestAddress, Trampoline) ->
  if DestAddress =:= (DestAddress band 16#01FFFFFC) ->
      %% The destination is in the [0,32MB[ range.
      %% We can reach it with a ba/bla instruction.
      %% This is the typical case for BIFs and primops.
      %% It's also common for trap-to-BEAM stubs.
      patch_b(CallAddress, DestAddress bsr 2, 2);
     true ->
      DestOffset = (DestAddress - CallAddress) bsr 2,
      if DestOffset >= -16#800000, DestOffset =< 16#7FFFFF ->
	  %% The destination is within a [-32MB,+32MB[ range from us.
	  %% We can reach it with a b/bl instruction.
	  %% This is typical for nearby Erlang code.
	  patch_b(CallAddress, DestOffset, 0);
	 true ->
	  %% The destination is too distant for b/bl/ba/bla.
	  %% Must do a b/bl to the trampoline.
	  TrampOffset = (Trampoline - CallAddress) bsr 2,
	  if TrampOffset >= -16#800000, TrampOffset =< 16#7FFFFF ->
	      %% Update the trampoline's address computation.
	      %% (May be redundant, but we can't tell.)
	      patch_li(Trampoline, DestAddress),
	      %% Update this call site.
	      patch_b(CallAddress, TrampOffset, 0)
	  end
      end
  end.

patch_li(Address, Value) ->
  patch_at_l(Address, Value),
  patch_at_ha(Address+4, Value).

patch_at_l(Address, Value) ->
  patch_simm16(Address, Value).

patch_at_ha(Address, Value) ->
  patch_simm16(Address, (Value + 16#8000) bsr 16).

patch_simm16(Address, Simm16) ->
  OldI = hipe_bifs:read_u32(Address),
  NewI = (OldI band 16#FFFF0000) bor (Simm16 band 16#FFFF),
  hipe_bifs:write_u32(Address, NewI).

patch_b(Address, Offset, AA) ->
  OldI = hipe_bifs:read_u32(Address),
  NewI = (OldI band 16#FC000001) bor ((Offset band 16#00FFFFFF) bsl 2) bor (AA band 2),
  hipe_bifs:write_u32(Address, NewI).
