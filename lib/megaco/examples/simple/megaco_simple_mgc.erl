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
%% Purpose: Simple example of an MGC
%% 
%% Example usage:
%%
%%   cd megaco/examples/simple
%%   erl -pa ../../../megaco/ebin -s megaco_filter -s megaco
%%   megaco_simple_mgc:start().
%%----------------------------------------------------------------------

-module(megaco_simple_mgc).

-behaviour(megaco_user).

-export([
	 start_batch/0, init_batch/1,
	 start/0, start/2,
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
%% Starting the MGC
%%----------------------------------------------------------------------

start() ->
    start({deviceName, "controller"}, []).

start(Mid, Config) ->
    d("start -> entry with~n   Mid: ~p", [Mid]),
    case megaco:start_user(Mid, [{user_mod, ?MODULE} | Config]) of
	ok ->
	    case catch do_start(Mid) of
		{'EXIT', Reason} ->
		    {error, Reason};
		Other ->
		    Other
	    end;
	{error, Reason} ->
	    {error, {start_user, Reason}}
    end.

do_start(Mid) ->
    RecHandle = megaco:user_info(Mid, receive_handle),

    TextMod  = megaco_pretty_text_encoder,
    TextTcp  = RecHandle#megaco_receive_handle{encoding_mod = TextMod,
					       encoding_config = [],
					       send_mod = megaco_tcp},
    TextUdp  = TextTcp#megaco_receive_handle{send_mod = megaco_udp},

    BinMod  = megaco_binary_encoder,
    BinTcp  = RecHandle#megaco_receive_handle{encoding_mod = BinMod,
					      encoding_config = [],
					      send_mod = megaco_tcp},
    BinUdp  = BinTcp#megaco_receive_handle{send_mod = megaco_udp},

    ListenTo = [{?megaco_ip_port_text, TextTcp},
		{?megaco_ip_port_text, TextUdp},
		{?megaco_ip_port_binary,  BinTcp},
		{?megaco_ip_port_binary,  BinUdp}
	       ],

    {ok, [{start_transport(Port, RH), Port, RH} || {Port, RH} <- ListenTo]}.

start_transport(MgcPort, RecHandle) ->
    case RecHandle#megaco_receive_handle.send_mod of
	megaco_tcp -> start_tcp(MgcPort, RecHandle);
	megaco_udp -> start_udp(MgcPort, RecHandle);
	SendMod    -> {error, {bad_send_mod, SendMod}}
    end.

start_udp(MgcPort, RecHandle) ->
    d("start_udp -> entry with"
      "~n   MgcPort:   ~p"
      "~n   RecHandle: ~p", [MgcPort, RecHandle]),
    case megaco_udp:start_transport() of
	{ok, SupPid} ->
	    Options = [{port, MgcPort}, {receive_handle, RecHandle}],
	    case megaco_udp:open(SupPid, Options) of
		{ok, SendHandle, ControlPid} ->
		    ok;
		{error, Reason} ->
		    {error, {megaco_udp_open, Reason}}
	    end;
	{error, Reason} ->
	    {error, {megaco_udp_start_transport, Reason}}
    end.

start_tcp(MgcPort, RecHandle) ->
    d("start_tcp -> entry with"
      "~n   MgcPort:   ~p"
      "~n   RecHandle: ~p", [MgcPort, RecHandle]),
    case megaco_tcp:start_transport() of
	{ok, SupPid} ->
	    Options = [{port, MgcPort}, {receive_handle, RecHandle}],
	    case megaco_tcp:listen(SupPid, Options) of
		ok ->
		    ok;
		{error, Reason} ->
		    {error, {megaco_tcp_listen, Reason}}
	    end;
	{error, Reason} ->
	    {error, {megaco_tcp_start_transport, Reason}}
    end.

%%----------------------------------------------------------------------
%% Stopping the MGC
%%----------------------------------------------------------------------

stop() ->
    [{Mid, stop(Mid)} || Mid <- megaco:system_info(users)].

stop(Mid) ->
    d("stop -> entry with~n   Mid: ~p", [Mid]),
    Disco = fun(CH) ->
		    d("stop -> CH: ~p", [CH]),
		    Reason = stopped_by_user, 
		    Pid = megaco:conn_info(CH, control_pid),
		    SendMod = megaco:conn_info(CH, send_mod),
		    SendHandle = megaco:conn_info(CH, send_handle),

		    d("stop -> disconnect", []),
		    megaco:disconnect(CH, Reason),
		    d("stop -> cancel", []),
		    megaco:cancel(CH, Reason),
		    d("stop -> close transport"
		      "~n   SendMod:    ~p"
		      "~n   SendHandle: ~p", [SendMod, SendHandle]),
		    case SendMod of
			megaco_tcp -> megaco_tcp:close(SendHandle);
			megaco_udp -> megaco_udp:close(SendHandle);
			SendMod    -> exit(Pid, Reason)
		    end
	    end,
    Conns = megaco:user_info(Mid, connections),
    d("stop -> Conns: ~p", [Conns]),
    Disconns = lists:map(Disco, Conns),
    d("stop -> Disconns: ~p", [Disconns]),
    megaco:stop_user(Mid),
    case whereis(?MODULE) of
	undefined ->
	    ignore;
	Pid ->
	    d("stop -> Pid: ~p", [Pid]),
	    unlink(Pid),
	    exit(Pid, shutdown)
    end,
    ok.

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
                             errorText = "Only single service change on null context handled"},
    case ActionRequests of
	[AR] ->
	    ContextId = AR#'ActionRequest'.contextId,
	    case AR#'ActionRequest'.commandRequests of
		[CR] when ContextId == ?megaco_null_context_id ->
		    case CR#'CommandRequest'.command of
			{serviceChangeReq, Req} ->
			    Rep = service_change(ConnHandle, ProtocolVersion, Req),
			    {discard_ack,
			     [#'ActionReply'{contextId = ContextId,
					     commandReply = [{serviceChangeReply, Rep}]}]};
			_ ->
			    {discard_ack, ED}
		    end;
		_ ->
		    {discard_ack, ED}
	    end;
	_ ->
	    {discard_ack, ED}
    end.

service_change(ConnHandle, ProtocolVersion, SCR) ->
    SCP = SCR#'ServiceChangeRequest'.serviceChangeParms,
    #'ServiceChangeParm'{serviceChangeMethod  = Method,
			 serviceChangeAddress = Address,
			 serviceChangeProfile = Profile,
			 serviceChangeReason  = [Reason],
			 serviceChangeDelay   = Delay,
			 serviceChangeMgcId   = MgcId} = SCP,
    TermId = SCR#'ServiceChangeRequest'.terminationID,
    if
	TermId == [?megaco_root_termination_id] ->
	    MyMid = ConnHandle#megaco_conn_handle.local_mid,
	    Res = {serviceChangeResParms,
		   #'ServiceChangeResParm'{serviceChangeMgcId   = MyMid,
					   serviceChangeAddress = Address,
					   serviceChangeProfile = Profile}},
	    #'ServiceChangeReply'{terminationID       = TermId,
				  serviceChangeResult = Res};
	true ->
	    Res = {errorDescriptor,
		   #'ErrorDescriptor'{errorCode = ?megaco_not_implemented,
				      errorText = "Only handled for root"}},
	    
	     #'ServiceChangeReply'{terminationID       = TermId,
				   serviceChangeResult = Res}
    end.
	
%%----------------------------------------------------------------------
%% Optionally invoked for a time consuming transaction request
%%----------------------------------------------------------------------

handle_trans_long_request(ConnHandle, ProtocolVersion, ReqData) ->
    ED = #'ErrorDescriptor'{errorCode = ?megaco_not_implemented,
			    errorText = "Long transaction requests not handled"},
    {discard_ack, ED}.

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


%%----------------------------------------------------------------------
%% DEBUGGING
%%----------------------------------------------------------------------

d(F,A) ->
    d(get(dbg),F,A).

d(true,F,A) ->
    io:format("SIMPLE_MGC: " ++ F ++ "~n", A);
d(_, _F, _A) ->
    ok.

