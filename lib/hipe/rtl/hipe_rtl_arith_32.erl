%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2002 by Erik Johansson.  
%% -*- erlang-indent-level: 2 -*-
%% ====================================================================
%%  Filename : 	hipe_rtl_arith_32.erl
%%  Module   :	hipe_rtl_arith_32
%%  Purpose  :  To implement 32-bit RTL-arithmetic 
%%  Notes    :  The arithmetic works on 32-bit signed integers. 
%%              The implementation is taken from the implementation
%%              of arithmetic on SPARC.
%%              XXX: This code is seldom used, and hence also
%%                   seldom tested. 
%%                   Look here for strange bugs appering when
%%                   turning on rtl_prop.
%%
%%  History  :	* 2002-10-23 Erik Stenman (happi@csd.uu.se): 
%%               Created.
%%  CVS      :
%%              $Author: pergu $
%%              $Date: 2003/04/23 11:58:14 $
%%              $Revision: 1.4 $
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-include("../main/hipe.hrl").

-module(hipe_rtl_arith_32).
-export([eval_alu/3,eval_alub/4, eval_cond/3]).

-define(BITS,32).
-define(SIGN_BIT,31).
-define(WORDMASK,16#ffffffff).
-define(MAX_SIGNED_INT, 16#7fffffff).
-define(MIN_SIGNED_INT,-16#80000000).
-define(MAX_UNSIGNED_INT, 16#ffffffff).



%% Returns a tuple
%%  {Res, Sign, Zero, Overflow, Carry}
%%  Res will be a number in the range 
%%   MAX_SIGNED_INT >= Res >= MIN_SIGNED_INT
%% The other four values are flags that are either true or false
%% 
eval_alu(Op, Arg1, Arg2) 
  when Arg1 =< ?MAX_SIGNED_INT, 
       Arg1 >= ?MIN_SIGNED_INT,
       Arg2 =< ?MAX_SIGNED_INT, 
       Arg2 >= ?MIN_SIGNED_INT ->

  Sign1 = sign_bit(Arg1),
  Sign2 = sign_bit(Arg2),

  case Op of
    'sub' ->
      Res = (Arg1 - Arg2) band ?WORDMASK,
      N = sign_bit(Res),
      Z = zero(Res),
      V = (Sign1 and (not Sign2) and (not N)) 
	or
          ((not Sign1) and Sign2 and N),
      C = ((not Sign1) and Sign2) 
	or 
	  (N and ((not Sign1) or Sign2));
  
    'add' ->
      Res = (Arg1 + Arg2) band ?WORDMASK,
      N = sign_bit(Res),
      Z = zero(Res),
      V = (Sign1 and Sign2 and (not N)) 
	or
          ((not Sign1) and (not Sign2) and N),
      C = (Sign1 and Sign2)
	or 
	  ((not N) and (Sign1 or Sign2));
    'sra' ->
      Res = (Arg1 bsr Arg2) band ?WORDMASK,
      N = sign_bit(Res),
      Z = zero(Res),    
      V = 0,
      C = 0;
    'srl' ->
      Res = (Arg1 bsr Arg2) band shiftmask(Arg2),
      N = sign_bit(Res),
      Z = zero(Res),     
      V = 0,
      C = 0;
    'sll' ->
      Res = (Arg1 bsl Arg2) band ?WORDMASK, 
      N = sign_bit(Res),
      Z = zero(Res),     
      V = 0,
      C = 0;
    'or' ->
      Res = (Arg1 bor Arg2) band ?WORDMASK,
      N = sign_bit(Res),
      Z = zero(Res),     
      V = 0,
      C = 0;
    'and' ->
      Res =  (Arg1 band Arg2) band ?WORDMASK,
      N = sign_bit(Res),
      Z = zero(Res),     
      V = 0,
      C = 0;
    'xor' ->
      Res = (Arg1 bxor Arg2) band ?WORDMASK,
      N = sign_bit(Res),
      Z = zero(Res),     
      V = 0,
      C = 0;
    Op ->
      Res = N = Z = V = C = 0,
      ?EXIT({"unknown alu op", Op})
  end,
  {two_comp_to_erl(Res),N,Z,V,C};
eval_alu(Op,Arg1,Arg2) ->
  ?EXIT({argument_overflow,Op,Arg1,Arg2}).
  

eval_alub(Op, Cond, Arg1, Arg2) ->
  {Res,N,Z,V,C} = eval_alu(Op,Arg1,Arg2),
  case Cond of
    'eq' ->
      {Res, Z};
    'ne' -> 
      {Res, not Z};
    'gt'	-> 
      {Res, not (Z or (N xor V))};
    'gtu' -> 
      {Res, not (C or Z)};
    'ge' -> 
      {Res, not (N xor V)};
    'geu'-> 
      {Res, not C};
    'lt'	->
      {Res, N xor V};
    'ltu'-> 
      {Res, C};
    'le'	->
      {Res, Z or (N xor V)};
    'leu'-> 
      {Res, C or Z};
    'overflow' ->
      {Res,V};
    'not_overflow' ->
      {Res, not V};
    _ ->
      ?EXIT({'condition code not handled',Cond})
  end.

eval_cond(Cond, Arg1, Arg2) ->
  {_,Bool} = eval_alub('sub', Cond, Arg1, Arg2),
  Bool.


sign_bit(Val) ->
  ((Val bsr ?SIGN_BIT) band 1) =:= 1.
two_comp_to_erl(V) ->
  if V > ?MAX_SIGNED_INT ->
      - ((?MAX_UNSIGNED_INT + 1) - V);
     true -> V
  end.

shiftmask(Arg) ->
  Setbits=32-Arg,
  round(math:pow(2,Setbits)-1).

zero(Val) ->
  Val =:= 0.

