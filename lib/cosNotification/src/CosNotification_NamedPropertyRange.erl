%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: CosNotification_NamedPropertyRange
%% Source: /ldisk/daily_build/otp_prebuild_r12b.2008-09-02_20/otp_src_R12B-4/lib/cosNotification/src/CosNotification.idl
%% IC vsn: 4.2.18
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module('CosNotification_NamedPropertyRange').
-ic_compiled("4_2_18").


-include("CosNotification.hrl").

-export([tc/0,id/0,name/0]).



%% returns type code
tc() -> {tk_struct,"IDL:omg.org/CosNotification/NamedPropertyRange:1.0",
                   "NamedPropertyRange",
                   [{"name",{tk_string,0}},
                    {"range",
                     {tk_struct,"IDL:omg.org/CosNotification/PropertyRange:1.0",
                                "PropertyRange",
                                [{"low_val",tk_any},{"high_val",tk_any}]}}]}.

%% returns id
id() -> "IDL:omg.org/CosNotification/NamedPropertyRange:1.0".

%% returns name
name() -> "CosNotification_NamedPropertyRange".



