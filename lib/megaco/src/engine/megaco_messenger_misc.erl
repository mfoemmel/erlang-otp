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
%% Purpose: Misc functions used both from the megaco_messenger module
%%          and the megaco_ack_sender module.
%% 
%%----------------------------------------------------------------------

-module(megaco_messenger_misc).

%% Application internal export
-export([encode_body/3,
	 send_body/3,
	 send_message/2
        ]).


-include_lib("megaco/include/megaco.hrl").
-include("megaco_message_internal.hrl").
-include("megaco_internal.hrl").


%%----------------------------------------------------------------------
%% Send the message body
%%----------------------------------------------------------------------

send_body(ConnData, TraceLabel, Body) ->
    case encode_body(ConnData, TraceLabel, Body) of
        {ok, Bin} ->
            send_message(ConnData, Bin);
        {error, Reason} ->
            {error, Reason}
    end.


%%----------------------------------------------------------------------
%% Encode the message body
%%----------------------------------------------------------------------

encode_body(ConnData, TraceLabel, Body) ->
    %% Create the message envelope
    LocalMid = (ConnData#conn_data.conn_handle)#megaco_conn_handle.local_mid,
    Msg = #'Message'{version     = ConnData#conn_data.protocol_version,
                     mId         = LocalMid,
                     messageBody = Body},
    MsgAuth = ConnData#conn_data.auth_data,
    MegaMsg = #'MegacoMessage'{authHeader = MsgAuth, % BUGBUG: Compute?
                               mess       = Msg},

    ?report_debug(ConnData, TraceLabel, [MegaMsg]),

    %% Encode the message
    EncodingMod    = ConnData#conn_data.encoding_mod,
    EncodingConfig = ConnData#conn_data.encoding_config,
    case (catch EncodingMod:encode_message(EncodingConfig, MegaMsg)) of
        {ok, Bin} when binary(Bin) ->
            {ok, Bin};
        Error ->
	    %% d("encode_body -> failed encode message body: ~n   ~p", [Error]),
	    incNumErrors(ConnData#conn_data.conn_handle),	    
            {error,{EncodingMod,encode_message,[EncodingConfig,MegaMsg],Error}}
    end.


%%----------------------------------------------------------------------
%% Send the (encoded) message
%%----------------------------------------------------------------------

send_message(ConnData, Bin) ->
    %% Send the message
    SendMod    = ConnData#conn_data.send_mod,
    SendHandle = ConnData#conn_data.send_handle,
    ?report_trace(ConnData, "send bytes", [{bytes, Bin}]),
    case (catch SendMod:send_message(SendHandle, Bin)) of
        ok ->
            {ok, Bin};
        {error, Reason} ->
	    %% d("send_message -> error sending message: ~n   ~p~", [Reason]),
	    incNumErrors(ConnData#conn_data.conn_handle),
            ?report_important(ConnData, "<ERROR> send_message callback",
                              [{bytes, Bin}, {error, Reason}]),
	    error_msg("error sending message: ~w", [Reason]),
            {error, {send_message_failed, Reason}};
        Reason ->
	    %% d("send_message -> failed sending message: ~n   ~p", [Reason]),
	    incNumErrors(ConnData#conn_data.conn_handle),
            ?report_important(ConnData, "<ERROR> send_message callback",
                              [{bytes, Bin}, {error, Reason}]),
	    error_msg("failed sending message: ~w", [Reason]),
            {error, {send_message_failed, Reason}}
    end.


%%%-----------------------------------------------------------------
%%% Misc internal util functions
%%%-----------------------------------------------------------------


%%-----------------------------------------------------------------
%% Func: error_msg/2
%% Description: Send an error message
%%-----------------------------------------------------------------
error_msg(F, A) ->
    (catch error_logger:error_msg(F ++ "~n", A)).


%%-----------------------------------------------------------------
%% Func: incNumErrors/0, incNumErrors/1, incNumTimerRecovery/1
%% Description: SNMP counter increment functions
%%-----------------------------------------------------------------

incNumErrors(CH) ->
    incNum({CH, medGwyGatewayNumErrors}).

incNum(Cnt) ->
    case (catch ets:update_counter(megaco_stats, Cnt, 1)) of
	{'EXIT', {badarg, R}} ->
	    ets:insert(megaco_stats, {Cnt, 1});
	Old ->
	    Old
    end.
	    
