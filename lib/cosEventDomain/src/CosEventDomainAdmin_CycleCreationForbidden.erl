%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: CosEventDomainAdmin_CycleCreationForbidden
%% Source: /net/isildur/ldisk/daily_build/otp_prebuild_r13b02.2009-09-21_11/otp_src_R13B02/lib/cosEventDomain/src/CosEventDomainAdmin.idl
%% IC vsn: 4.2.22
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module('CosEventDomainAdmin_CycleCreationForbidden').
-ic_compiled("4_2_22").


-include("CosEventDomainAdmin.hrl").

-export([tc/0,id/0,name/0]).



%% returns type code
tc() -> {tk_except,"IDL:omg.org/CosEventDomainAdmin/CycleCreationForbidden:1.0",
                   "CycleCreationForbidden",
                   [{"cyc",{tk_sequence,tk_long,0}}]}.

%% returns id
id() -> "IDL:omg.org/CosEventDomainAdmin/CycleCreationForbidden:1.0".

%% returns name
name() -> "CosEventDomainAdmin_CycleCreationForbidden".



