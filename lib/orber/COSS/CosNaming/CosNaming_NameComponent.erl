%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: CosNaming_NameComponent
%% Source: /ldisk/daily_build/otp_prebuild_r12b.2008-09-02_20/otp_src_R12B-4/lib/orber/COSS/CosNaming/cos_naming.idl
%% IC vsn: 4.2.18
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module('CosNaming_NameComponent').
-ic_compiled("4_2_18").


-include("CosNaming.hrl").

-export([tc/0,id/0,name/0]).



%% returns type code
tc() -> {tk_struct,"IDL:omg.org/CosNaming/NameComponent:1.0","NameComponent",
                   [{"id",{tk_string,0}},{"kind",{tk_string,0}}]}.

%% returns id
id() -> "IDL:omg.org/CosNaming/NameComponent:1.0".

%% returns name
name() -> "CosNaming_NameComponent".



