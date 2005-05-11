%%% -*- erlang-indent-level: 2 -*-
%%% $Id: hipe_ppc64_loader.erl,v 1.1 2005/03/16 12:31:40 mikpe Exp $

-module(hipe_ppc64_loader).
-export([patch_instr/3]).
-export([patch_call/3]).

%-define(DEBUG,true).
-include("../../hipe/main/hipe.hrl").

patch_instr(Addr, Val, Type) ->
  ?debug_msg("Patch, addr:~w, value:~w type:~w\n", [Addr,Val,Type]),
  case Type of
    closure -> patch_li64(Addr, Val);
    constant -> patch_li64(Addr, Val);
    atom -> patch_li31(Addr, Val);
    c_const -> patch_li31(Addr, Val);
    _ -> exit({?MODULE,patch_instr,Addr,Val,Type})
  end.

patch_call(CallAddress, DestAddress, Trampoline) ->
  if DestAddress =:= (DestAddress band 16#01FFFFFC) ->
      %% The destination is in the [0,32MB[ range.
      %% We can reach it with a ba/bla instruction.
      %% This is the typical case for BIFs and primops.
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
	      patch_li31(Trampoline, DestAddress),
	      %% Update this call site.
	      patch_b(CallAddress, TrampOffset, 0)
	  end
      end
  end.

patch_li64(Address, Value) ->
  patch_imm16(Address, Value bsr 48),		% addis r,0,Value@highest
  patch_imm16(Address+4, Value bsr 32),		% ori r,r,Value@higher
						% sldi r,r,32
  patch_imm16(Address+12, Value bsr 16),	% oris r,r,Value@h
  patch_imm16(Address+16, Value).		% ori r,r,Value@l

patch_li31(Address, Value) ->
  patch_at_h15(Address, Value),	% addis r,0,Value@h
  patch_at_l(Address+4, Value).	% ori r,r,Value@l

patch_at_h15(Address, Value) ->
  High = Value bsr 16,
  if High >= 0, High =< 16#7FFF ->
      patch_imm16(Address, High)
  end.

patch_at_l(Address, Value) ->
  patch_imm16(Address, Value).

patch_imm16(Address, Imm16) ->
  OldI = hipe_bifs:read_u32(Address),
  NewI = (OldI band 16#FFFF0000) bor (Imm16 band 16#FFFF),
  hipe_bifs:write_u32(Address, NewI).

patch_b(Address, Offset, AA) ->
  OldI = hipe_bifs:read_u32(Address),
  NewI = (OldI band 16#FC000001) bor ((Offset band 16#00FFFFFF) bsl 2) bor (AA band 2),
  hipe_bifs:write_u32(Address, NewI).
