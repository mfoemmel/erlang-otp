%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: CosNaming_BindingList
%% Source: /ldisk/daily_build/otp_prebuild_r12b.2008-09-02_20/otp_src_R12B-4/lib/orber/COSS/CosNaming/cos_naming.idl
%% IC vsn: 4.2.18
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module('CosNaming_BindingList').
-ic_compiled("4_2_18").


-include("CosNaming.hrl").

-export([tc/0,id/0,name/0]).



%% returns type code
tc() -> {tk_sequence,
            {tk_struct,"IDL:omg.org/CosNaming/Binding:1.0","Binding",
                [{"binding_name",
                  {tk_sequence,
                      {tk_struct,"IDL:omg.org/CosNaming/NameComponent:1.0",
                          "NameComponent",
                          [{"id",{tk_string,0}},{"kind",{tk_string,0}}]},
                      0}},
                 {"binding_type",
                  {tk_enum,"IDL:omg.org/CosNaming/BindingType:1.0",
                      "BindingType",
                      ["nobject","ncontext"]}}]},
            0}.

%% returns id
id() -> "IDL:omg.org/CosNaming/BindingList:1.0".

%% returns name
name() -> "CosNaming_BindingList".



