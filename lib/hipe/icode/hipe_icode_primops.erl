%% -*- erlang-indent-level: 2 -*-
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2001 by Erik Johansson.  All Rights Reserved 
%% Time-stamp: <01/07/19 13:00:47 happi>
%% ====================================================================
%%  Filename : 	hipe_icode_primops.erl
%%  Module   :	hipe_icode_primops
%%  Purpose  :  
%%  Notes    : 
%%  History  :	* 2001-06-13 Erik Johansson (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: happi $
%%              $Date: 2001/07/19 23:44:36 $
%%              $Revision: 1.6 $
%% ====================================================================
%%  Exports  :
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(hipe_icode_primops).
-export([pp/2]).

pp(Op, Dev) ->
  case Op of
    {hipe_bs_primop, BsOp} ->
      case BsOp of 
	{bs_put_binary_all, Flags} -> 
	  io:format(Dev, "bs_put_binary_all<~w>", [Flags]);
	{bs_put_binary, Size} ->
	  io:format(Dev, "bs_put_binary<~w>", [Size]);
	{bs_put_float, Flags, Size} ->
	  io:format(Dev, "bs_put_float<~w, ~w>", [Flags, Size]);
	{bs_put_string, String, SizeInBytes} ->
	  io:format(Dev, "bs_put_string<~w, ~w>", [String, SizeInBytes]);
	{bs_put_integer, Bits, Flags} ->
	  io:format(Dev, "bs_put_integer<~w, ~w>", [Bits, Flags]);
	{bs_skip_bits_all, Flags} ->
	  io:format(Dev, "bs_skip_bits_all<~w>", [Flags]);
	{bs_skip_bits, Unit} ->
	  io:format(Dev, "bs_skip_bits<~w>", [Unit]);
	bs_start_match ->
	  io:format(Dev, "bs_start_match", []);
	{bs_get_integer,Size,Flags} ->
	  io:format(Dev, "bs_get_integer<~w, ~w>", [Size, Flags]);
	{bs_get_float,Size,Flags} ->
	  io:format(Dev, "bs_get_float<~w, ~w>", [Size, Flags]);
	{bs_get_binary,Size,Flags} ->
	  io:format(Dev, "bs_get_binary<~w, ~w>", [Size, Flags]);
	{bs_get_binary_all,Flags} ->
	  io:format(Dev, "bs_get_binary_all<~w>", [Flags]);
	{bs_test_tail,NumBits} ->
	  io:format(Dev, "bs_test_tail<~w>", [NumBits]);
	{bs_restore, Index} ->
	  io:format(Dev, "bs_restore<~w>", [Index]);
	{bs_save, Index} ->
	  io:format(Dev, "bs_save<~w>", [Index]);
	bs_init ->
	  io:format(Dev, "bs_init", []);
	bs_final ->
	  io:format(Dev, "bs_final", [])


      end;
    {mkfun, {Mod, Fun, Arity}, U, I} ->
      io:format(Dev, "mkfun<~w,~w,~w,~w,~w>", [Mod, Fun, Arity, U, I]);
    {Mod, Fun, Arity} ->
      io:format(Dev, "~w:~w", [Mod, Fun]);
    {Fun, Arity} ->
      io:format(Dev, "~w", [Fun]);
    Fun ->
      io:format(Dev, "~w", [Fun])
  end.
