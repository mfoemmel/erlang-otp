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

-module(snmpm_user_default).

-behaviour(snmpm_user).

-export([handle_agent/4,
	 handle_pdu/5,
	 handle_trap/4,
	 handle_inform/4,
	 handle_report/4]).

handle_agent(Addr, Port, SnmpInfo, UserData) ->
    info("received handle_agent:"
	 "~n   Addr:     ~p"
	 "~n   Port:     ~p"
	 "~n   SnmpInfo: ~p"
	 "~n   UserData: ~p", [Addr, Port, SnmpInfo, UserData]),
    ignore.


handle_pdu(Addr, Port, ReqId, SnmpResponse, UserData) ->
    info("received handle_pdu:"
	 "~n   Addr:         ~p"
	 "~n   Port:         ~p"
	 "~n   ReqId:        ~p"
	 "~n   SnmpResponse: ~p"
	 "~n   UserData:     ~p", 
	 [Addr, Port, ReqId, SnmpResponse, UserData]),
    ignore.


handle_trap(Addr, Port, SnmpTrap, UserData) ->
    info("received handle_trap:"
	 "~n   Addr:     ~p"
	 "~n   Port:     ~p"
	 "~n   SnmpTrap: ~p"
	 "~n   UserData: ~p", 
	 [Addr, Port, SnmpTrap, UserData]),
    ok.


handle_inform(Addr, Port, SnmpInform, UserData) ->
    info("received handle_inform:"
	 "~n   Addr:       ~p"
	 "~n   Port:       ~p"
	 "~n   SnmpInform: ~p"
	 "~n   UserData:   ~p", 
	 [Addr, Port, SnmpInform, UserData]),
    ok.


handle_report(Addr, Port, SnmpReport, UserData) ->
    info("received handle_inform:"
	 "~n   Addr:       ~p"
	 "~n   Port:       ~p"
	 "~n   SnmpReport: ~p"
	 "~n   UserData:   ~p", 
	 [Addr, Port, SnmpReport, UserData]),
    ok.


info(F, A) ->
    error_logger:info_msg("SNMPM default user callback " ++ F ++ "~n", A).
