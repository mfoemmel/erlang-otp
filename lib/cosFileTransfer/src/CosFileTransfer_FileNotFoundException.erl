%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: CosFileTransfer_FileNotFoundException
%% Source: /net/shelob/ldisk/daily_build/otp_prebuild_r13a.2009-03-16_22/otp_src_R13A/lib/cosFileTransfer/src/CosFileTransfer.idl
%% IC vsn: 4.2.20
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module('CosFileTransfer_FileNotFoundException').
-ic_compiled("4_2_20").


-include("CosFileTransfer.hrl").

-export([tc/0,id/0,name/0]).



%% returns type code
tc() -> {tk_except,"IDL:omg.org/CosFileTransfer/FileNotFoundException:1.0",
                   "FileNotFoundException",
                   [{"reason",{tk_string,0}}]}.

%% returns id
id() -> "IDL:omg.org/CosFileTransfer/FileNotFoundException:1.0".

%% returns name
name() -> "CosFileTransfer_FileNotFoundException".



