%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(test1).
-compile(export_all).

bits1(get) ->
    {value, [b0, b2]}.
bits1(set, _) ->
    noError.

bits2(get) ->
    {value, 2#11000000110}.
bits2(set, _) ->
    noError.

bits3(get) ->
    {value, [b0, b4]}. % error!

bits4(get) ->
    {value, 2#1000}. % error!

opaque_obj(get) ->
    {value, "opaque-data"}.

cnt64(get) ->
    {value, 18446744073709551615}.

multiStr(get) ->
    global:re_register_name(snmp_multi_tester, self()),
    receive
	continue -> ok
    end,
    {value, "ok"}.

multiStr(set, "block") ->
    global:re_register_name(snmp_multi_tester, self()),
    receive
	continue -> ok
    end,
    noError;
multiStr(set, _Value) ->
    noError.
