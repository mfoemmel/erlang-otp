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
	 encode_trans_request/2,
	 encode_trans_reply/2,
	 encode_actions/3,
	 send_body/3,
	 send_message/2
        ]).

%% Test functions
-export([compose_message/3, encode_message/2]).


-include_lib("megaco/include/megaco.hrl").
-include("megaco_message_internal.hrl").
-include("megaco_internal.hrl").

-ifdef(MEGACO_TEST_CODE).
-define(SIM(Other,Where),
	fun(Afun,Bfun) ->
		Kfun = {?MODULE,Bfun},
		case (catch ets:lookup(megaco_test_data, Kfun)) of
		    [{Kfun,Cfun}] ->
			Cfun(Afun);
		    _ ->
			Afun
		end
	end(Other,Where)).
-else.
-define(SIM(Other,Where),Other).
-endif.


%%----------------------------------------------------------------------
%% Encode the transaction request
%%----------------------------------------------------------------------

encode_trans_request(CD, TR) when record(TR, 'TransactionRequest') ->
    ?report_debug(CD, "encode trans request", [TR]),
    Trans = {transactionRequest, TR},
    encode_trans(CD, Trans).

encode_trans_reply(CD, TR) when record(TR, 'TransactionReply') ->
    ?report_debug(CD, "encode trans reply", [TR]),
    Trans = {transactionReply, TR},
    encode_trans(CD, Trans).


encode_trans(#conn_data{protocol_version = V,
			encoding_mod     = EM,
			encoding_config  = EC} = CD, Trans) ->
    case (catch EM:encode_transaction(EC, V, Trans)) of
	{ok, Bin} ->
	    ?SIM({ok, Bin}, encode_trans);
	Error ->
	    incNumErrors(CD#conn_data.conn_handle),	    
            {error,{EM, encode_transaction, [EC, Trans], Error}}
    end.


%%----------------------------------------------------------------------
%% Encode the action request's
%%----------------------------------------------------------------------

encode_actions(#conn_data{protocol_version = V} = CD, TraceLabel, ARs) ->
    ?report_debug(CD, TraceLabel, [ARs]),

    %% Encode the actions
    EM = CD#conn_data.encoding_mod,
    EC = CD#conn_data.encoding_config,
    case (catch EM:encode_action_requests(EC, V, ARs)) of
        {ok, Bin} when binary(Bin) ->
            ?SIM({ok, Bin}, encode_actions);
        Error ->
	    incNumErrors(CD#conn_data.conn_handle),	    
            {error, {EM, encode_action_requests, [EC, ARs], Error}}
    end.


%%----------------------------------------------------------------------
%% Encode the message body
%%----------------------------------------------------------------------

encode_body(#conn_data{protocol_version = V} = ConnData, 
	    TraceLabel, Body) ->
    %% Create the message envelope
    MegaMsg = compose_message(ConnData, V, Body),

    %% p("encode_body -> ~n~p", [MegaMsg]),
    ?report_debug(ConnData, TraceLabel, [MegaMsg]),

    %% Encode the message
    EncodingMod    = ConnData#conn_data.encoding_mod,
    EncodingConfig = ConnData#conn_data.encoding_config,
    case (catch EncodingMod:encode_message(EncodingConfig, V, MegaMsg)) of
        {ok, Bin} when binary(Bin) ->
            ?SIM({ok, Bin}, encode_body);
        Error ->
	    incNumErrors(ConnData#conn_data.conn_handle),	    
            {error,{EncodingMod,encode_message,[EncodingConfig,MegaMsg],Error}}
    end.


%%----------------------------------------------------------------------
%% Compose and encode a message
%%----------------------------------------------------------------------
compose_message(#conn_data{conn_handle = CH,
			   auth_data   = MsgAuth}, V, Body) ->
    LocalMid = CH#megaco_conn_handle.local_mid,
    Msg      = #'Message'{version     = V,
			  mId         = LocalMid,
			  messageBody = Body},
    MegaMsg  = #'MegacoMessage'{authHeader = MsgAuth, % BUGBUG: Compute?
				mess       = Msg},
    MegaMsg.
    

encode_message(#conn_data{protocol_version = Version,
			  encoding_mod     = EncodingMod,
			  encoding_config  = EncodingConfig},  MegaMsg) ->
    (catch EncodingMod:encode_message(EncodingConfig, Version, MegaMsg)).


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
%% Send the (encoded) message
%%----------------------------------------------------------------------

send_message(ConnData, Bin) ->
    %% Send the message
    #conn_data{send_mod    = SendMod,
	       send_handle = SendHandle} = ConnData,
    ?report_trace(ConnData, "send bytes", [{bytes, Bin}]),
    case (catch SendMod:send_message(SendHandle, Bin)) of
        ok ->
            ?SIM({ok, Bin}, send_message);
        {error, Reason} ->
	    incNumErrors(ConnData#conn_data.conn_handle),
            ?report_important(ConnData, "<ERROR> send_message callback",
                              [{bytes, Bin}, {error, Reason}]),
	    error_msg("error sending message on ~w: ~w", [SendHandle, Reason]),
            {error, {send_message_failed, Reason}};
        Reason ->
	    incNumErrors(ConnData#conn_data.conn_handle),
            ?report_important(ConnData, "<ERROR> send_message callback",
                              [{bytes, Bin}, {error, Reason}]),
	    error_msg("failed sending message on ~w: ~w", 
		      [SendHandle, Reason]),
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
	{'EXIT', {badarg, _R}} ->
	    ets:insert(megaco_stats, {Cnt, 1});
	Old ->
	    Old
    end.
	    
% p(F, A) ->
%     print(now(), F, A).

% print(Ts, F, A) ->
%     io:format("*** [~s] ~p ***"
%               "~n   " ++ F ++ "~n", 
%               [format_timestamp(Ts), self() | A]).

% format_timestamp(Now) ->
%     {N1, N2, N3}   = Now,
%     {Date, Time}   = calendar:now_to_datetime(Now),
%     {YYYY,MM,DD}   = Date,
%     {Hour,Min,Sec} = Time,
%     FormatDate = 
%         io_lib:format("~.4w:~.2.0w:~.2.0w ~.2.0w:~.2.0w:~.2.0w 4~w",
%                       [YYYY,MM,DD,Hour,Min,Sec,round(N3/1000)]),  
%     lists:flatten(FormatDate).
