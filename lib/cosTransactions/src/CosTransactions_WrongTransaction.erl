%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: CosTransactions_WrongTransaction
%% Source: /net/shelob/ldisk/daily_build/otp_prebuild_r13a.2009-03-16_22/otp_src_R13A/lib/cosTransactions/src/CosTransactions.idl
%% IC vsn: 4.2.20
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module('CosTransactions_WrongTransaction').
-ic_compiled("4_2_20").


-include("CosTransactions.hrl").

-export([tc/0,id/0,name/0]).



%% returns type code
tc() -> {tk_except,"IDL:omg.org/CosTransactions/WrongTransaction:1.0",
                   "WrongTransaction",[]}.

%% returns id
id() -> "IDL:omg.org/CosTransactions/WrongTransaction:1.0".

%% returns name
name() -> "CosTransactions_WrongTransaction".



