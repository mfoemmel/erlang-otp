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
-module(snmp_net_if).

-export([start_link/1, get_log_type/1, set_log_type/2]).
-export([system_continue/3, system_terminate/4, system_code_change/4]).
-export([init/2]).

-include("snmp_types.hrl").

-record(state, {parent, master_agent, usock, mpd_state, log,
		reqs = [], debug = false}).

%% Debug macros
-define(pdebug(Format, X), 
	case get(debug) of 
	    undefined -> ok;
	    _ -> io:format(lists:concat(["** SNMP Net if debug: \n   ",
					 Format, "\n"]), X)
	end.

-define(ipdebug(Format, X), 
	case get(debug) of 
	    undefined -> ok;
	    _ -> io:format(Format, X)
	end.
					
%%%-----------------------------------------------------------------
%%% This module implements the default Network Interface part
%%% of the SNMP agent. It uses UDP, and read the agent.conf to find
%%% the UDP port.
%%%-----------------------------------------------------------------
start_link(MasterAgent) ->
    proc_lib:start_link(?MODULE, init, [MasterAgent, self()]).

get_log_type(Pid) ->
    Pid ! {self(), get_log_type},
    receive
	{Pid, Type} ->
	    Type
    after
	5000 ->
	    undefined
    end.

set_log_type(Pid, Type) ->
    Pid ! {set_log_type, Type}.


init(MasterAgent, Parent) ->
    process_flag(trap_exit, true),
    {value, UDPPort} = snmp_framework_mib:intAgentUDPPort(get),
    {value, IPAddress} = snmp_framework_mib:intAgentIpAddress(get),
    Vsns = snmp_misc:get_vsns(),
    Log = case snmp_log:create_log() of
	      {ok, L} -> L;
	      false -> false;
	      {error, Err} -> exit(Err)
	  end,
    IPOpts1 = case application:get_env(snmp, bind_to_ip_address) of
		  {ok, true} ->
		      [{ip, list_to_tuple(IPAddress)}];
		  _ ->
		      []
	      end,
    IPOpts2 = case application:get_env(snmp, no_reuse_address) of
		  {ok, true} ->
		      [{reuseaddr, true}];
		  _ ->
		      []
	      end,
    IPOpts = IPOpts1 ++ IPOpts2, 
    case gen_udp_open(UDPPort, [binary| IPOpts]) of
	{ok, Id} ->
	    MpdState = snmp_mpd:init_mpd(Vsns),
	    S = #state{parent = Parent,
		       master_agent = MasterAgent, mpd_state = MpdState,
		       usock = Id, log = Log},
	    proc_lib:init_ack({ok, self()}),
	    loop(S);
	{error, Reason} ->
	    proc_lib:init_ack({error, {udp, UDPPort, Reason}})
    end.

gen_udp_open(Port, Opts) ->
    case init:get_argument(snmp_fd) of
	{ok, [[FdStr]]} ->
	    Fd = list_to_integer(FdStr),
	    gen_udp:open(0, [{fd, Fd}|Opts]);
	error ->
	    gen_udp:open(Port, Opts)
    end.
	
loop(S) ->
    receive
	{udp, _UdpId, Ip, Port, Packet} ->
	    ?pdebug("net_if got packet from ~w:~w~n", [Ip, Port]),
	    put(n1,erlang:now()),
	    LogF = case S#state.log of
		       false -> nofunc;
		       Log ->
			   fun(Type, Data) ->
				   snmp_log:log(Log, Type, Data, {Ip, Port})
			   end
		   end,
	    case snmp_mpd:process_packet(Packet, snmpUDPDomain, {Ip, Port}, 
					 S#state.mpd_state, LogF) of
		{ok, Vsn, Pdu, PduMS, ACMData} ->
		    ?pdebug("got pdu from ~w:~w ~p", [Ip, Port, Pdu]),
		    Type = Pdu#pdu.type,
		    case Type of
			'get-response' ->
			    handle_response(Vsn, Pdu, {Ip, Port}, S);
			_ ->
			    S#state.master_agent !
				{snmp_pdu, Vsn, Pdu, PduMS,
				 ACMData, {Ip, Port}, []}
		    end;
		{discarded, Reason} ->
		    ?pdebug("got packet from ~p, discarded for reason: ~p", 
			    [{Ip, Port}, Reason]);
		{discarded, Reason, ReportPacket} ->
		   ?pdebug("got packet from ~p, sending report for reason: ~p", 
			    [{Ip, Port}, Reason]),
		    gen_udp:send(S#state.usock, Ip, Port, ReportPacket)
		end,
	    loop(S);
	{snmp_response, Vsn, RePdu, Type, ACMData, {Ip, Port}, []} ->
	    ?pdebug("reply pdu: ~p", [RePdu]),
	    LogF = case S#state.log of
		       false -> nofunc;
		       Log ->
			   fun(Type2, Data) ->
				   snmp_log:log(Log, Type2, Data, {Ip, Port})
			   end
		   end,
	    case snmp_mpd:generate_response_msg(Vsn,RePdu,Type,ACMData,LogF) of
		{ok, Packet} ->
		    N2 = erlang:now(),
		    NT = subtr(N2, get(n1)),
		    ?pdebug("Time in agent: ~w mysec~n", [NT]),
		    gen_udp:send(S#state.usock, Ip, Port, Packet);
		{discarded, _Reason} ->
		    ok
	    end,
	    loop(S);
	{send_pdu, Vsn, Pdu, MsgData, To} ->
	    NewS = handle_send_pdu(S, Vsn, Pdu, MsgData, To, undefined),
	    loop(NewS);
	{send_pdu_req, Vsn, Pdu, MsgData, To, From} ->
	    NewS = handle_send_pdu(S, Vsn, Pdu, MsgData, To, From),
	    loop(NewS);
	{discarded_pdu, _Vsn, _ReqId, _ACMData, Variable, _Extra} ->
	    snmp_mpd:discarded_pdu(Variable),
	    loop(S);
	{From, get_log_type} ->
	    From ! {self(), S#state.log},
	    loop(S);
	{set_log_type, Type} ->
	    loop(S#state{log = Type});
	{debug, true} ->
	    put(debug, true),
	    loop(S);
	{debug, false} ->
	    erase(debug),
	    loop(S);
	{'EXIT', Parent, Reason} when Parent == S#state.parent ->
	    exit(Reason);
	{'EXIT', Pid, Reason} ->
	    NewS = clear_reqs(Pid, S),
	    loop(NewS);
	{system, From, Msg} ->
	    sys:handle_system_msg(Msg, From, S#state.parent, ?MODULE, [], S);
	_ ->
	    loop(S)
    end.

handle_send_pdu(S, Vsn, Pdu, MsgData, To, From) ->
    case snmp_mpd:generate_msg(Vsn, Pdu, MsgData, To) of
	{ok, Addresses} ->
	    ?pdebug("sending pdu ~p, to the following mgrs:", [Pdu]),
	    lists:foreach(fun({snmpUDPDomain, {Ip, Port}, Packet}) ->
				  ?ipdebug("    ~w:~w~n",[Ip,Port]),
				  log(S#state.log, Pdu#pdu.type, Packet,
				      {Ip, Port}),
				  gen_udp:send(S#state.usock, Ip,
					       Port, Packet);
			     (_X) ->
				  io:format(user, "** bad res: ~p\n", [_X]),
				  ok
			  end, Addresses);
	{discarded, Reason} ->
	    ?pdebug("do not send too big pdu ~p", [Pdu]),
	    ok
    end,
    case From of
	undefined ->
	    S;
	Pid ->
	    link(Pid),
	    NReqs = snmp_misc:keyreplaceadd(Pid, 2, S#state.reqs, 
					    {Pdu#pdu.request_id, From}),
	    S#state{reqs = NReqs}
    end.

handle_response(Vsn, Pdu, From, S) ->
    case lists:keysearch(Pdu#pdu.request_id, 1, S#state.reqs) of
	{value, {_, Pid}} ->
	    Pid ! {snmp_response_received, Vsn, Pdu, From};
	_ ->
	    ?pdebug("No receiver available for response pdu", [])
    end.

clear_reqs(Pid, S) ->
    NReqs = lists:keydelete(Pid, 2, S#state.reqs),
    S#state{reqs = NReqs}.


log(false, _Type, _Packet, _Address) ->
    ok;
log(Log, Type, Packet, Address) ->
    snmp_log:log(Log, Type, Packet, Address).

%%%-----------------------------------------------------------------
%%% System messages
%%%-----------------------------------------------------------------
system_continue(_Parent, _Dbg, S) ->
    loop(S).

system_terminate(Reason, _Parent, _Dbg, S) ->
    exit(Reason).

system_code_change(S, _Module, _OldVsn, _Extra) ->
    {ok, S}.

%%%-----------------------------------------------------------------
%%% DEBUG FUNCTIONS
%%%-----------------------------------------------------------------
subtr({X1,Y1,Z1}, {X1,Y1,Z2}) ->
    Z1 - Z2;
subtr({X1,Y1,Z1}, {X1,Y2,Z2}) ->
    ((Y1 * 1000000) + Z1) - ((Y2 * 1000000) + Z2).
