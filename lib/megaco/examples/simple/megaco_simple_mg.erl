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
%% Purpose: Simple example of an MG
%%
%% Example usage:
%% 
%%   cd megaco/examples/simple
%%   erl -pa ../../../megaco/ebin -s megaco_filter -s megaco
%%   megaco_simple_mg:start().
%%----------------------------------------------------------------------

-module(megaco_simple_mg).

-behaviour(megaco_user).

-export([
	 start_batch/0, init_batch/1,
	 start/0, start/4,
	 start_tcp_text/2, start_tcp_binary/2, start_udp_text/2, start_udp_binary/2,
	 stop/0, stop/1
	 ]).

-export([
         handle_connect/2,
         handle_disconnect/3,
         handle_syntax_error/3,
         handle_message_error/3,
         handle_trans_request/3,
         handle_trans_long_request/3,
         handle_trans_reply/4,
         handle_trans_ack/4,
	 handle_unexpected_trans/3
        ]).

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").

%%----------------------------------------------------------------------
%% Starting the MG
%%----------------------------------------------------------------------

start() ->
    {ok, LocalHost} = inet:gethostname(),
    Starters = [fun start_tcp_text/2,
		fun start_tcp_binary/2,
		fun start_udp_text/2,
		fun start_udp_binary/2],
    [Fun(LocalHost, []) || Fun <- Starters].

start_tcp_text(MgcHost, Default) ->
    Config = [{encoding_mod, megaco_pretty_text_encoder},
	      {encoding_config, []},
	      {send_mod, megaco_tcp} | Default],
    Mid = {deviceName, "gateway_tt"},
    {Mid, start(MgcHost, ?megaco_ip_port_text, Mid, Config)}.

start_tcp_binary(MgcHost, Default) ->
    Config = [{encoding_mod, megaco_binary_encoder},
	      {encoding_config, []},
	      {send_mod, megaco_tcp} | Default],
    Mid = {deviceName, "gateway_tb"},
    {Mid, start(MgcHost, ?megaco_ip_port_binary, Mid, Config)}.

start_udp_text(MgcHost, Default) ->
    Config = [{encoding_mod, megaco_pretty_text_encoder},
	      {encoding_config, []},
	      {send_mod, megaco_udp} | Default],
    Mid = {deviceName, "gateway_ut"},
    {Mid, start(MgcHost, ?megaco_ip_port_text, Mid, Config)}.

start_udp_binary(MgcHost, Default) ->
    Config = [{encoding_mod, megaco_binary_encoder},
	      {encoding_config, []},
	      {send_mod, megaco_udp} | Default],
    Mid = {deviceName, "gateway_ub"},
    {Mid, start(MgcHost, ?megaco_ip_port_binary, Mid, Config)}.

start(MgcHost, MgcPort, Mid, Config) ->
    case megaco:start_user(Mid, [{user_mod, ?MODULE} | Config]) of
	ok ->
	    case start_transport(MgcHost, MgcPort, Mid) of
		{ok, ConnHandle} ->
		    service_change(ConnHandle);
		{error, Reason} ->
		    {error, Reason}
	    end;
	{error, Reason} ->
	    {error, {start_user, Reason}}
    end.

start_transport(MgcHost, MgcPort, Mid) ->
    RecHandle = megaco:user_info(Mid, receive_handle),
    case RecHandle#megaco_receive_handle.send_mod of
	megaco_tcp -> start_tcp(MgcHost, MgcPort, RecHandle);
	megaco_udp -> start_udp(MgcHost, MgcPort, RecHandle);
	SendMod    -> {error, {bad_send_mod, SendMod}}
    end.

start_tcp(MgcHost, MgcPort, RecHandle) ->
    case megaco_tcp:start_transport() of
	{ok, Pid} ->
	    Options = [{host, MgcHost},
		       {port, MgcPort},
		       {receive_handle, RecHandle}],
	    case megaco_tcp:connect(Pid, Options) of
		{ok, SendHandle, ControlPid} ->
		    MgcMid = preliminary_mid,
		    megaco:connect(RecHandle, MgcMid, SendHandle, ControlPid);
		{error, Reason} ->
		    {error, {megaco_tcp_connect, Reason}}
	    end;
	{error, Reason} ->
	    {error, {megaco_tcp_start_transport, Reason}}
    end.

start_udp(MgcHost, MgcPort, RecHandle) ->
    case megaco_udp:start_transport() of
	{ok, SupPid} ->
	    Options = [{port, 0}, {receive_handle, RecHandle}],
	    case megaco_udp:open(SupPid, Options) of
		{ok, Handle, ControlPid} ->
		    Socket = megaco_udp:socket(Handle),
		    MgPort = inet:port(Socket),
		    MgcMid = preliminary_mid,
		    SendHandle = megaco_udp:create_send_handle(Handle, MgcHost, MgcPort),
		    megaco:connect(RecHandle, MgcMid, SendHandle, ControlPid);
		{error, Reason} ->
		    {error, {megaco_udp_open, Reason}}
	    end;
	{error, Reason} ->
	    {error, {megaco_udp_start_transport, Reason}}
    end.

service_change(ConnHandle) ->
    service_change(ConnHandle, restart, ?megaco_cold_boot).

service_change(ConnHandle, Method, Reason) ->
    SCP = #'ServiceChangeParm'{serviceChangeMethod = Method,
			       serviceChangeReason = [Reason]},
    TermId = [?megaco_root_termination_id],
    SCR = #'ServiceChangeRequest'{terminationID = TermId,
				  serviceChangeParms = SCP},
    CR = #'CommandRequest'{command = {serviceChangeReq, SCR}},
    AR = #'ActionRequest'{contextId = ?megaco_null_context_id,
			  commandRequests = [CR]},
    megaco:call(ConnHandle, [AR], []).
    
%%----------------------------------------------------------------------
%% Stopping the MG
%%----------------------------------------------------------------------

stop() ->
    [{Mid, stop(Mid)} || Mid <- megaco:system_info(users)].

stop(Mid) ->
    Reason = stopped_by_user,
    Disco = fun(CH) ->
		    Pid = megaco:conn_info(CH, control_pid),
		    megaco:disconnect(CH, Reason),
		    megaco:cancel(CH, Reason),
		    exit(Pid, Reason)
	    end,
    lists:map(Disco, megaco:user_info(Mid, connections)),
    megaco:stop_user(Mid).

%%----------------------------------------------------------------------
%% Invoked when a new connection is established
%%----------------------------------------------------------------------

handle_connect(ConnHandle, ProtocolVersion) ->
    ok.

%%----------------------------------------------------------------------
%% Invoked when a connection is teared down
%%----------------------------------------------------------------------

handle_disconnect(ConnHandle, ProtocolVersion, Reason) ->
    megaco:cancel(ConnHandle, Reason), % Cancel the outstanding messages
    ok.

%%----------------------------------------------------------------------
%% Invoked when  a received message had syntax errors
%%----------------------------------------------------------------------

handle_syntax_error(ReceiveHandle, ProtocolVersion, ErrorDescriptor) ->
    reply.

%%----------------------------------------------------------------------
%% Invoked when a received message contained no transactions
%%----------------------------------------------------------------------

handle_message_error(ConnHandle, ProtocolVersion, ErrorDescriptor) ->
    no_reply.

%%----------------------------------------------------------------------
%% Invoked for each transaction request
%%----------------------------------------------------------------------

handle_trans_request(ConnHandle, ProtocolVersion, ActionRequests) ->
    ED =  #'ErrorDescriptor'{errorCode = ?megaco_not_implemented,
                             errorText = "Transaction requests not handled"},
    {discard_ack, ED}.

%%----------------------------------------------------------------------
%% Optionally invoked for a time consuming transaction request
%%----------------------------------------------------------------------

handle_trans_long_request(ConnHandle, ProtocolVersion, ReqData) ->
    ED = #'ErrorDescriptor'{errorCode = ?megaco_not_implemented,
                            errorText = "Long transaction requests not handled"},
    {discard_ack,  ED}.

%%----------------------------------------------------------------------
%% Optionally invoked for a transaction reply
%%----------------------------------------------------------------------

handle_trans_reply(ConnHandle, ProtocolVersion, ActualReply, ReplyData) ->
    ok.

%%----------------------------------------------------------------------
%% Optionally invoked for a transaction acknowledgement
%%----------------------------------------------------------------------

handle_trans_ack(ConnHandle, ProtocolVersion, AckStatus, AckData) ->
    ok.

%%----------------------------------------------------------------------
%% Invoked when  an unexpected message has been received
%%----------------------------------------------------------------------

handle_unexpected_trans(ConnHandle, ProtocolVersion, Trans) ->
    ok.
%%----------------------------------------------------------------------
%% To be used at command line: erl -s ?MODULE start_batch
%%----------------------------------------------------------------------

start_batch() ->
    Pid = spawn(?MODULE, init_batch, [self()]),
    receive
	{init_batch, Pid, Res} ->
	    io:format("~p(~p): ~p~n", [?MODULE, ?LINE, Res]),
	    Res
    end.
	    
init_batch(ReplyTo) ->
    register(?MODULE, self()),
    Res = start(),
    ReplyTo ! {init_batch, self(), Res},
    receive
    after infinity -> Res
    end.
