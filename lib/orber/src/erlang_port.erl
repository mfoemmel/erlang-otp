%%------------------------------------------------------------
%%
%% Implementation stub file
%% 
%% Target: erlang_port
%% Source: /net/shelob/ldisk/daily_build/otp_prebuild_r13a.2009-03-16_22/otp_src_R13A/lib/ic/include/erlang.idl
%% IC vsn: 4.2.20
%% 
%% This file is automatically generated. DO NOT EDIT IT.
%%
%%------------------------------------------------------------

-module(erlang_port).
-ic_compiled("4_2_20").


-include("erlang.hrl").

-export([tc/0,id/0,name/0]).



%% returns type code
tc() -> {tk_struct,"IDL:erlang/port:1.0","port",
                   [{"node",{tk_string,256}},
                    {"id",tk_ulong},
                    {"creation",tk_ulong}]}.

%% returns id
id() -> "IDL:erlang/port:1.0".

%% returns name
name() -> "erlang_port".



