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
-module(snmp_mgr_tests).
%% c(snmp_mgr_tests).
%%----------------------------------------------------------------------
%% This module examplifies how to write test suites for your SNMP agent.
%% Don't forget that the manager must be started in 'quiet' mode
%% otherwise 'expect' won't receive any responses.
%%----------------------------------------------------------------------

-compile(export_all).
-import(snmp_mgr, [gn/1, g/1, s/1, expect/2, expect/4, expect/6]).

start() ->
    udp:start(),
    snmp_mgr:start([{agent,"dront.nada.kth.se"},{community,"all-rights"},
		    {mibs,["STANDARD-MIB.bin"]},quiet]),
    simple_standard_test().

simple_standard_test() ->
    gn([[1,1]]),
    expect(1, [{[sysDescr,0], "Erlang SNMPv1 agent"}]),
    gn([[1,3]]),
    expect(11, [{[sysDescr,0], "Erlang SNMPv1 agent"}]),
    gn([[1,3,6]]),
    expect(12, [{[sysDescr,0], "Erlang SNMPv1 agent"}]),
    gn([[1,3,6,1]]),
    expect(13, [{[sysDescr,0], "Erlang SNMPv1 agent"}]),
    gn([[1,3,6,1,2]]),
    expect(14, [{[sysDescr,0], "Erlang SNMPv1 agent"}]),
    gn([[1,3,6,1,2,1]]),
    expect(15, [{[sysDescr,0], "Erlang SNMPv1 agent"}]),
    gn([[1,3,6,1,2,1,1]]),
    expect(16, [{[sysDescr,0], "Erlang SNMPv1 agent"}]),
    gn([[1,3,6,1,2,1,1,1]]),
    expect(17, [{[sysDescr,0], "Erlang SNMPv1 agent"}]),
    g([[sysDescr,0]]),
    expect(2, [{[sysDescr,0], "Erlang SNMPv1 agent"}]),
    g([[1,3,6,1,2,1,1,1]]),
    expect(3, noSuchName,1, any),
    gn( [[1,13]]),
    expect(4, noSuchName,1, any),
    s([{[sysLocation, 0], s, "new_value"}]),
    expect(5, [{[sysLocation, 0], "new_value"}]),
    g([[sysLocation, 0]]),
    expect(6, [{[sysLocation, 0], "new_value"}]),
    p("Testing readOnly and badValue..."),
    s([{[sysServices,0], 3}]),  
    expect(61, noSuchName, 1, any),
    s([{[sysLocation, 0], i, 3}]),
    expect(62, badValue, 1, any),
    io:format("Test completed.~n").

p(Str) ->
    io:format("-Test- "), io:format(Str), io:format("~n").
