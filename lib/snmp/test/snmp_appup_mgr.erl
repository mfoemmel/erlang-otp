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
%%----------------------------------------------------------------------
%% Purpose: Simple (snmp) manager used when performing appup tests.
%%----------------------------------------------------------------------
-module(snmp_appup_mgr).

-include_lib("snmp/include/STANDARD-MIB.hrl").
-include_lib("snmp/include/snmp_types.hrl").

-export([start/0, start/2]).

-define(v1_2(V1,V2),
	case get(vsn) of
	    v1 -> V1;
	    _  -> V2
	end).


start() ->
    {ok, AgentHost} = inet:gethostname(),
    AgentPort = 4000,
    start(AgentHost, AgentPort).

start(AgentHost, AgentPort) 
  when list(AgentHost), integer(AgentPort) ->
    StdM = filename:join(code:priv_dir(snmp), "mibs") ++ "/",
    Dir         = "/tmp",
    Community   = "all-rights",
    User        = "all-rights",
    SecLevel    = noAuthNoPriv,
    EngineID    = "agentEngine",
    CtxEngineID = EngineID,
    Args = [
	    {debug,     false},
	    {agent,     AgentHost},
	    {agent_udp, AgentPort},
	    {trap_udp,  5000},
	    quiet,
	    v1,
	    {community, Community},
	    {user,      User},
	    {sec_level, SecLevel},
	    {engine_id, EngineID},
	    {context_engine_id, CtxEngineID},
	    {dir,       Dir},
	    {mibs,      mibs(StdM)}
	   ],
     
    {ok, Pid} = snmp_mgr:start_link(Args),

    Req1 = {"system",         get, ?sysDescr_instance},
    Req2 = {"systemObjectID", get, ?sysObjectID_instance},
    Req3 = {"systemUpTime",   get, ?sysUpTime_instance},
    Reqs = [Req1,Req2,Req3],
    %% Reqs = [Req1],
    loop(Pid, Reqs).

mibs(StdMibDir) ->
    [join(StdMibDir, ?v1_2("STANDARD-MIB.bin", "SNMPv2-MIB.bin")),
     join(StdMibDir, "SNMP-FRAMEWORK-MIB"),
     join(StdMibDir, "SNMP-MPD-MIB")].

join(D,F) ->    
    filename:join(D,F).

loop(Pid, Reqs) ->
    handle_reqs(Pid, Reqs),
    %% sleep(10000),
    loop(Pid, Reqs).

handle_reqs(_, []) ->
    ok;
handle_reqs(P, [Req|Reqs]) ->
    sleep(1000),
    handle_req(P, Req),
    handle_reqs(P, Reqs).

handle_req(P, {Desc, Op, Oid}) ->
    io:format("~w ~s: ", [Op, Desc]),
    ok = snmp_mgr_op(Op, [Oid]),
    Reply = snmp_mgr:receive_response(),
    %% io:format("reply: ~p~n", [Reply]),
    verify_reply(Op,Oid,Reply).

snmp_mgr_op(get, Oid) ->
    snmp_mgr:g(Oid).

verify_reply(get, Oid, #pdu{type         = 'get-response', 
			    error_status = noError,
			    error_index  = 0,
			    varbinds     = Varbinds}) ->
    case verify_varbind(Oid, Varbinds) of
	{ok, Type, Val} ->
	    print_value(Type, Val),
	    ok;
	error ->
	    io:format("unknown varbinds: ~n~p~n", [Varbinds])
    end;
verify_reply(get, _Oid, #pdu{type         = 'get-response', 
			     error_status = Es,
			     error_index  = Ei}) ->
    io:format("~p - ~p~n", [Es,Ei]);
verify_reply(get, _Oid, Reply) ->
    io:format("invalid reply: ~p~n", [Reply]);
verify_reply(Op, Oid, Reply) ->
    throw({error, {invalid_op, Op, Oid, Reply}}).

verify_varbind(Oid, []) ->
    error;
verify_varbind(Oid, [#varbind{oid          = Oid, 
			      variabletype = Type, 
			      value        = Val}|_]) ->
    {ok, Type, Val};
verify_varbind(Oid, [_|Varbinds]) ->
    verify_varbind(Oid, Varbinds).
    
    
print_value('OBJECT IDENTIFIER', Val) ->
    io:format("~p~n", [Val]);
print_value('OCTET STRING', Val) ->
    io:format("~s~n", [Val]);
print_value('TimeTicks', Val) ->
    io:format("~w~n", [Val]);
print_value(Type, Val) ->
    io:format("[~p] ~p~n", [Type, Val]).

sleep(T) ->
    receive 
    after T -> ok
    end.

