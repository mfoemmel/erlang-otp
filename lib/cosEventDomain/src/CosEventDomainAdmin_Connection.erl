%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: CosEventDomainAdmin_Connection
%% Source: /net/shelob/ldisk/daily_build/otp_prebuild_r13a.2009-03-16_22/otp_src_R13A/lib/cosEventDomain/src/CosEventDomainAdmin.idl
%% IC vsn: 4.2.20
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module('CosEventDomainAdmin_Connection').
-ic_compiled("4_2_20").


-include("CosEventDomainAdmin.hrl").

-export([tc/0,id/0,name/0]).



%% returns type code
tc() -> {tk_struct,"IDL:omg.org/CosEventDomainAdmin/Connection:1.0",
                   "Connection",
                   [{"supplier_id",tk_long},
                    {"consumer_id",tk_long},
                    {"ctype",
                     {tk_enum,"IDL:omg.org/CosNotifyChannelAdmin/ClientType:1.0",
                              "ClientType",
                              ["ANY_EVENT","STRUCTURED_EVENT",
                               "SEQUENCE_EVENT"]}},
                    {"notification_style",
                     {tk_enum,"IDL:omg.org/CosEventDomainAdmin/NotificationStyle:1.0",
                              "NotificationStyle",
                              ["Push","Pull"]}}]}.

%% returns id
id() -> "IDL:omg.org/CosEventDomainAdmin/Connection:1.0".

%% returns name
name() -> "CosEventDomainAdmin_Connection".



