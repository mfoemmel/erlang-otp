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

-export([start_link/1, start_link/2, 
	 get_log_type/1, set_log_type/2, verbosity/2]).
-export([system_continue/3, system_terminate/4, system_code_change/4]).
-export([init/3]).

-include("snmp_types.hrl").
-include("snmp_verbosity.hrl").

-record(state, {parent, 
		master_agent, 
		usock, 
		usock_opts, 
		mpd_state, 
		log,
		reqs = [], 
		debug = false,
		limit = infinity,
		rcnt = []}).

-ifndef(default_verbosity).
-define(default_verbosity,silence).
-endif.


%%%-----------------------------------------------------------------
%%% This module implements the default Network Interface part
%%% of the SNMP agent. It uses UDP, and read the agent.conf to find
%%% the UDP port.
%%%
%%% Opts = [Opt]
%%% Opt  = {verbosity,silence | log | debug}
%%%-----------------------------------------------------------------
start_link(MasterAgent) ->
    start_link(MasterAgent,[]).

start_link(MasterAgent,Opts) ->
    proc_lib:start_link(?MODULE, init, [MasterAgent, self(), Opts]).

verbosity(Pid, Verbosity) -> Pid ! {verbosity, Verbosity}.

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

get_port() ->
    {value, UDPPort} = snmp_framework_mib:intAgentUDPPort(get),
    UDPPort.

get_address() ->
    {value, IPAddress} = snmp_framework_mib:intAgentIpAddress(get),
    IPAddress.

init(MasterAgent, Parent, Opts) ->
    process_flag(trap_exit, true),
    put(sname,nif),
    put(verbosity,get_verbosity(Opts)),
    ?vlog("starting",[]),
    UDPPort = get_port(),
    ?vdebug("port: ~w",[UDPPort]),
    IPAddress = get_address(),
    ?vdebug("addr: ~w",[IPAddress]),
    Vsns = snmp_misc:get_vsns(),
    ?vdebug("vsns: ~w",[Vsns]),
    Limit = get_req_limit(Opts),
    Log = case snmp_log:create_log() of
	      {ok, L} -> L;
	      false -> false;
	      {error, Err} -> exit(Err)
	  end,
    ?vdebug("Log: ~w",[Log]),
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
    IPOpts3 = case get_recbuf(Opts) of
		  use_default ->
		      [];
		  RecSz ->
		      [{recbuf,RecSz}]
	      end,
    IPOpts = IPOpts1 ++ IPOpts2 ++ IPOpts3, 
    ?vdebug("open socket with options: ~w",[IPOpts]),
    case gen_udp_open(UDPPort, [binary| IPOpts]) of
	{ok, Id} ->
	    active_once(Id),
	    MpdState = snmp_mpd:init_mpd(Vsns),
	    S = #state{parent       = Parent,
		       master_agent = MasterAgent, 
		       mpd_state    = MpdState,
		       usock        = Id, 
		       usock_opts   = [binary| IPOpts],
		       log          = Log,
		       limit        = Limit},
	    proc_lib:init_ack({ok, self()}),
	    ?vdebug("started",[]),
	    loop(S);
	{error, Reason} ->
	    ?vinfo("Failed to open UDP socket: ~p",[Reason]),
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
	    ?vlog("got paket from ~w:~w (~w)",[Ip,Port,size(Packet)]),
	    S1 = handle_recv(S, Ip, Port, Packet),
	    loop(S1);

	{snmp_response, Vsn, RePdu, Type, ACMData, Dest, []} ->
	    ?vlog("reply pdu: "
		  "~n   ~s", 
		  [?vapply(snmp_misc, format, [256, "~w", [RePdu]])]),
	    S1 = handle_reply_pdu(S, Vsn, RePdu, Type, ACMData, Dest),
	    loop(S1);

	{send_pdu, Vsn, Pdu, MsgData, To} ->
	    ?vdebug("send pdu: "
		"~n   Pdu: ~p"
		"~n   To:  ~p", [Pdu,To]),
	    NewS = handle_send_pdu(S, Vsn, Pdu, MsgData, To, undefined),
	    loop(NewS);

	{send_pdu_req, Vsn, Pdu, MsgData, To, From} ->
	    ?vdebug("send pdu request: "
		    "~n   Pdu:  ~p"
		    "~n   To:   ~p"
		    "~n   From: ~p", 
		    [Pdu,To,toname(From)]),
	    NewS = handle_send_pdu(S, Vsn, Pdu, MsgData, To, From),
	    loop(NewS);

	{discarded_pdu, _Vsn, ReqId, _ACMData, Variable, _Extra} ->
	    ?vdebug("discard PDU: ~p", [Variable]),
	    snmp_mpd:discarded_pdu(Variable),
	    S1 = update_req_counter_outgoing(S, ReqId),
	    loop(S1);

	{From, get_log_type} ->
	    From ! {self(), S#state.log},
	    loop(S);

	{set_log_type, Type} ->
	    loop(S#state{log = Type});

	{verbosity,Verbosity} ->
	    ?vlog("verbosity: ~p -> ~p",[get(verbosity),Verbosity]),
	    put(verbosity,snmp_verbosity:validate(Verbosity)),
	    loop(S);

	{'EXIT', Parent, Reason} when Parent == S#state.parent ->
	    ?vlog("~n   parent (~p) exited: ~p", [Parent,Reason]),
	    exit(Reason);

	{'EXIT', Port, Reason} when Port == S#state.usock ->
	    UDPPort = get_port(),
	    S1 = 
		case gen_udp_open(UDPPort, S#state.usock_opts) of
		    {ok, Id} ->
			error_msg("Port ~p exited for reason ~p~n"
				  "Re-opened (~p)", [Port, Reason, Id]),
			S#state{usock = Id};
		    {error, ReopenReason} ->
			error_msg("Port ~p exited for reason ~p~n"
				  "Re-open failed for reason ~p", 
				  [Port, Reason, ReopenReason]),
			ok
		end,
	    loop(S1);

	{'EXIT', Port, Reason} when port(Port) ->
	    error_msg("Exit message from port ~p for reason ~p~n", 
		      [Port, Reason]),
	    loop(S);

	{'EXIT', Pid, Reason} ->
	    ?vlog("~p exited: ~p", [Pid,Reason]),
	    NewS = clear_reqs(Pid, S),
	    loop(NewS);

	{system, From, Msg} ->
	    ?vdebug("system signal ~p from ~p", [Msg,From]),
	    sys:handle_system_msg(Msg, From, S#state.parent, ?MODULE, [], S);
	_ ->
	    loop(S)

    end.


update_req_counter_incomming(#state{limit = infinity, usock = Sock} = S, _) ->
    active_once(Sock), %% No limit so activate directly
    S; 
update_req_counter_incomming(#state{limit = Limit, 
				    rcnt  = RCnt, 
				    usock = Sock} = S, Rid) 
  when length(RCnt) + 1 == Limit ->
    %% Ok, one more and we are at the limit.
    %% Just make sure we are not already processing this one...
    case lists:member(Rid, RCnt) of
	false ->
	    %% We are at the limit, do _not_ activate socket
	    S#state{rcnt = [Rid|RCnt]};
	true ->
	    active_once(Sock),
	    S
    end;
update_req_counter_incomming(#state{rcnt  = RCnt, 
				    usock = Sock} = S, Rid) ->
    active_once(Sock),
    case lists:member(Rid, RCnt) of
	false ->
	    S#state{rcnt = [Rid|RCnt]};
	true ->
	    S
    end.


update_req_counter_outgoing(#state{limit = infinity} = S, _Rid) ->
    %% Already activated (in the incoming function)
    S;
update_req_counter_outgoing(#state{limit = Limit, 
				   rcnt  = RCnt, 
				   usock = Sock} = S, Rid) 
  when length(RCnt) == Limit ->
    ?vtrace("handle_req_counter_outgoing(~w) -> entry with"
	"~n   Rid:          ~w"
	"~n   length(RCnt): ~w", [Limit, Rid, length(RCnt)]),
    case lists:delete(Rid, RCnt) of
	NewRCnt when length(NewRCnt) < Limit ->
	    ?vtrace("update_req_counter_outgoing -> "
		"passed below limit: activate", []),
	    active_once(Sock),
	    S#state{rcnt = NewRCnt};
	_ ->
	    S
    end;
update_req_counter_outgoing(#state{limit = Limit, rcnt = RCnt} = S, 
			    Rid) ->
    ?vtrace("handle_req_counter_outgoing(~w) -> entry with"
	"~n   Rid:          ~w"
	"~n   length(RCnt): ~w", [Limit, Rid, length(RCnt)]),
    NewRCnt = lists:delete(Rid, RCnt),
    S#state{rcnt = NewRCnt}.
    

handle_recv(#state{usock = Sock} = S, Ip, Port, Packet) ->
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
	    ?vlog("got pdu"
		"~n   ~s", 
		[?vapply(snmp_misc, format, [256, "~w", [Pdu]])]),
	    handle_recv_pdu(Ip, Port, Vsn, Pdu, PduMS, ACMData, S);
	{discarded, Reason} ->
	    ?vlog("packet discarded for reason: "
		"~n   ~s",
		[?vapply(snmp_misc, format, [256, "~w", [Reason]])]),
	    active_once(Sock),
	    S;
	{discarded, Reason, ReportPacket} ->
	    ?vlog("sending report for reason: "
		"~n   ~s", 
		[?vapply(snmp_misc, format, [256, "~w", [Reason]])]),
	    (catch udp_send(S#state.usock, Ip, Port, ReportPacket)),
	    active_once(Sock),
	    S
    end.
    
handle_recv_pdu(Ip, Port, Vsn, #pdu{type = 'get-response'} = Pdu, 
		_PduMS, _ACMData, #state{usock = Sock} = S) ->
    active_once(Sock),
    handle_response(Vsn, Pdu, {Ip, Port}, S),
    S;
handle_recv_pdu(Ip, Port, Vsn, #pdu{request_id = Rid, type = Type} = Pdu, 
		PduMS, ACMData, #state{master_agent = Pid} = S) 
  when Type == 'get-request'; 
       Type == 'get-next-request'; 
       Type == 'get-bulk-request' ->
    ?vtrace("handle_recv_pdu -> received get (~w)", [Type]),
    Pid ! {snmp_pdu, Vsn, Pdu, PduMS, ACMData, {Ip, Port}, []},
    update_req_counter_incomming(S, Rid);
handle_recv_pdu(Ip, Port, Vsn, Pdu, PduMS, ACMData, 
		#state{usock = Sock, master_agent = Pid} = S) ->
    ?vtrace("handle_recv_pdu -> received other request", []),
    active_once(Sock),
    Pid ! {snmp_pdu, Vsn, Pdu, PduMS, ACMData, {Ip, Port}, []},
    S.


handle_reply_pdu(S, Vsn, #pdu{request_id = Rid} = Pdu, 
		 Type, ACMData, {Ip, Port}) ->
    S1 = update_req_counter_outgoing(S, Rid),
    LogF = case S#state.log of
	       false -> nofunc;
	       Log ->
		   fun(Type2, Data) ->
			   log(Log, Type2, Data, {Ip, Port})
		   end
	   end,
    case (catch snmp_mpd:generate_response_msg(Vsn, Pdu, Type,
					       ACMData, LogF)) of
	{ok, Packet} ->
	    ?vinfo("time in agent: ~w mysec", 
		[subtr(erlang:now(),get(n1))]),
	    (catch udp_send(S#state.usock, Ip, Port, Packet)),
	    S1;
	{discarded, Reason} ->
	    ?vlog("~n   reply discarded for reason: ~s", 
		[?vapply(snmp_misc, format, [256, "~w", [Reason]])]),
	    S1;
	{'EXIT', Reason} ->
	    user_err("failed generating response message: "
		     "~nPDU: ~w~n~w", [Pdu, Reason]),
	    S1
    end.
    

handle_send_pdu(S, Vsn, Pdu, MsgData, To, From) ->
    case (catch snmp_mpd:generate_msg(Vsn, Pdu, MsgData, To)) of
	{ok, Addresses} ->
	    handle_send_pdu(S,Pdu,Addresses);
	{discarded, Reason} ->
	    ?vlog("~n   PDU ~p not sent due to ~p", [Pdu,Reason]),
	    ok;
	{'EXIT', Reason} ->
	    user_err("failed generating message: "
		     "~nPDU: ~w~n~w", [Pdu, Reason]),
	    ok
    end,
    case From of
	undefined ->
	    S;
	Pid ->
	    ?vtrace("~n   link to ~p and add to request list", [Pid]),
	    link(Pid),
	    NReqs = snmp_misc:keyreplaceadd(Pid, 2, S#state.reqs, 
					    {Pdu#pdu.request_id, From}),
	    S#state{reqs = NReqs}
    end.


handle_send_pdu(S,Pdu,Addresses) ->
    case (catch handle_send_pdu1(S,Pdu#pdu.type,Addresses)) of
	{Reason,Sz} ->
	    error_msg("snmp[error]: cannot send message "
		      "(size: ~p, reason: ~p, pdu: ~p)",
		      [Sz, Reason, Pdu]);
	_ ->
	    ok
    end.

handle_send_pdu1(S, PduType, Addresses) ->
    Fun = fun({snmpUDPDomain, {Ip, Port}, Packet}) ->
		  ?vdebug("sending packet:"
		      "~n   of size: ~p"
		      "~n   to mgr:  ~p:~p",
		  [sz(Packet),Ip,Port]),
		  log(S#state.log, PduType, Packet, {Ip, Port}),
		  udp_send(S#state.usock, Ip, Port, Packet);
	     (_X) ->
		  ?vlog("** bad res: ~p", [_X]),
		  ok
	  end,
    lists:foreach(Fun, Addresses).


handle_response(Vsn, #pdu{request_id = Rid} = Pdu, 
		From, #state{reqs = Reqs}) ->
    case lists:keysearch(Rid, 1, Reqs) of
	{value, {_, Pid}} ->
	    ?vdebug("send response to receiver ~p", [Pid]),
	    Pid ! {snmp_response_received, Vsn, Pdu, From};
	_ ->
	    ?vdebug("~n   No receiver available for response pdu", [])
    end.


udp_send(UdpId, AgentIp, UdpPort, B) ->
    case (catch gen_udp:send(UdpId, AgentIp, UdpPort, B)) of
	{error,emsgsize} ->
	    %% From this message we cannot recover, so exit sending loop
	    throw({emsgsize,sz(B)});
	{error,ErrorReason} ->
	    error_msg("snmp[error]: cannot send message "
		      "(destination: ~p:~p, size: ~p, reason: ~p)",
		      [AgentIp, UdpPort, sz(B), ErrorReason]);
	{'EXIT',ExitReason} ->
	    error_msg("snmp[exit]: cannot send message "
		      "(destination: ~p:~p, size: ~p, reason: ~p)",
		      [AgentIp, UdpPort, sz(B), ExitReason]);
	_ ->
	    ok
    end.

sz(L) when list(L) -> length(L);
sz(B) when binary(B) -> size(B);
sz(_) -> undefined.

log(false, _Type, _Packet, _Address) ->
    ok;
log(Log, Type, Packet, Address) ->
    snmp_log:log(Log, Type, Packet, Address).

user_err(F, A) ->
    snmp_error_report:user_err(F, A).

error_msg(F,A) -> 
    catch error_logger:error_msg(F,A).

clear_reqs(Pid, S) ->
    NReqs = lists:keydelete(Pid, 2, S#state.reqs),
    S#state{reqs = NReqs}.


toname(P) when pid(P) ->
    case process_info(P,registered_name) of
	{registered_name,Name} ->
	    Name;
	_ ->
	    P
    end;
toname(Else) ->
    Else.


active_once(Sock) ->
    ?vtrace("activate once", []),
    inet:setopts(Sock, [{active, once}]).


%%%-----------------------------------------------------------------
%%% System messages
%%%-----------------------------------------------------------------
system_continue(_Parent, _Dbg, S) ->
    loop(S).

system_terminate(Reason, _Parent, _Dbg, _S) ->
    exit(Reason).

system_code_change(S, _Module, _OldVsn, upgrade_from_pre_3_4_3) ->
    ?vtrace("system_code_change(up_from_pre_3_4_3) -> entry with"
	"~n   S:       ~p"
	"~n   _Module: ~p"
	"~n   _OldVsn: ~p", [S, _Module, _OldVsn]),
    {state, Parent, Master, Sock, SockOpts, MpdState, Log, Reqs, Dbg} = S,
    NewS = #state{parent       = Parent, 
		  master_agent = Master, 
		  usock        = Sock, 
		  usock_opts   = SockOpts, 
		  mpd_state    = MpdState, 
		  log          = Log,
		  reqs         = Reqs, 
		  debug        = Dbg,
		  limit        = infinity,
		  rcnt         = []},
    active_once(Sock),
    {ok, NewS};

system_code_change(S, _Module, _OldVsn, downgrade_to_pre_3_4_3) ->
    ?vtrace("system_code_change(down_to_pre_3_4_3) -> entry with"
	"~n   S:       ~p"
	"~n   _Module: ~p"
	"~n   _OldVsn: ~p", [S, _Module, _OldVsn]),
    #state{parent       = Parent, 
	   master_agent = Master, 
	   usock        = Sock, 
	   usock_opts   = SockOpts, 
	   mpd_state    = MpdState, 
	   log          = Log,
	   reqs         = Reqs, 
	   debug        = Dbg} = S,
    NewS = {state, Parent, Master, 
	    Sock, SockOpts, MpdState, Log, Reqs, Dbg},
    inet:setopts(Sock, [{active, true}]), 
    {ok, NewS};

system_code_change(S, _Module, _OldVsn, _Extra) ->
    ?vtrace("system_code_change -> entry with"
	"~n   S:       ~p"
	"~n   _Module: ~p"
	"~n   _OldVsn: ~p"
	"~n   _Extra:  ~p", [S, _Module, _OldVsn, _Extra]),
    {ok, S}.


%%%-----------------------------------------------------------------
%%% DEBUG FUNCTIONS
%%%-----------------------------------------------------------------
subtr({X1,Y1,Z1}, {X1,Y1,Z2}) ->
    Z1 - Z2;
subtr({X1,Y1,Z1}, {X1,Y2,Z2}) ->
    ((Y1-Y2) * 1000000) + (Z1 - Z2);
subtr({X1,Y1,Z1}, {X2,Y2,Z2}) ->
    ((X1 - X2) * 1000000000000) + ((Y1 - Y2) * 1000000) + (Z1 - Z2).


get_verbosity(O) ->
    snmp_misc:get_option(verbosity, O, ?default_verbosity).

get_recbuf(O) -> 
    snmp_misc:get_option(recbuf, O, use_default).

get_req_limit(O) ->
    snmp_misc:get_option(req_limit, O, infinity).
