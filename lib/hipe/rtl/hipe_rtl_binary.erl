%%%-------------------------------------------------------------------
%%% File    : hipe_rtl_binary_2.erl
%%% Author  : Per Gustafsson <pergu@it.uu.se>
%%% Description : 
%%%
%%% Created : 5 Mar 2007 by Per Gustafsson <pergu@it.uu.se>
%%%-------------------------------------------------------------------
-module(hipe_rtl_binary).

-export([gen_rtl/6]).

gen_rtl(BsOP, Dst, Args, TrueLblName, FalseLblName, ConstTab) ->
  case type_of_operation(BsOP) of
    match ->
      {hipe_rtl_binary_match:gen_rtl(
	BsOP, Dst, Args, TrueLblName, FalseLblName),ConstTab};
    construct ->
      hipe_rtl_binary_construct:gen_rtl(
	BsOP, Dst, Args, TrueLblName, FalseLblName, ConstTab)
  end.

type_of_operation({bs_start_match,_}) -> match;
type_of_operation({bs_get_binary,_,_}) -> match;
type_of_operation({bs_get_binary_all,_,_}) -> match;
type_of_operation({bs_get_integer,_,_}) -> match;
type_of_operation({bs_get_float,_,_}) ->  match;
type_of_operation({bs_skip_bits,_}) -> match;
type_of_operation({bs_skip_bits_all,_,_}) ->  match;
type_of_operation({bs_test_tail,_}) -> match;
type_of_operation({bs_restore,_}) -> match;
type_of_operation({bs_save,_}) ->  match;
type_of_operation({bs_add,_}) -> construct;
type_of_operation({bs_add,_,_}) -> construct;
type_of_operation(bs_bits_to_bytes) -> construct;
type_of_operation(bs_bits_to_bytes2) -> construct;
type_of_operation({bs_init,_}) -> construct;
type_of_operation({bs_init,_,_}) -> construct;
type_of_operation({bs_put_binary,_,_}) -> construct;
type_of_operation({bs_put_binary_all,_}) -> construct;  
type_of_operation({bs_put_float,_,_,_}) -> construct;
type_of_operation({bs_put_integer,_,_,_}) -> construct;
type_of_operation({bs_put_string,_,_}) -> construct;  
type_of_operation({unsafe_bs_put_integer,_,_,_}) -> construct;
type_of_operation(bs_final) -> construct.
