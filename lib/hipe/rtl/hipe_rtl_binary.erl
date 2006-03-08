%%%-------------------------------------------------------------------
%%% File    : hipe_rtl_ibo_2.erl
%%% Author  : Per Gustafsson <pergu@dhcp-12-245.it.uu.se>
%%% Description : 
%%%
%%% Created :  7 Sep 2005 by Per Gustafsson <pergu@dhcp-12-245.it.uu.se>
%%%-------------------------------------------------------------------
-module(hipe_rtl_binary).

-export([gen_rtl/5]).

gen_rtl(BsOP, Dst, Args, TrueLblName, FalseLblName) ->
  %% needs an R11 runtime system, so is an error stub in R10
  exit({?MODULE,gen_rtl,BsOP,Dst,Args,TrueLblName,FalseLblName}).
