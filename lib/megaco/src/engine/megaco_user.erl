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
%% Purpose: Default implementation of user callbacks
%%----------------------------------------------------------------------

-module(megaco_user).

-behaviour(megaco_user).

-export([
         handle_connect/2,
         handle_disconnect/3,
         handle_syntax_error/3,
         handle_message_error/3,
         handle_trans_request/3,
         handle_trans_long_request/3,
         handle_trans_reply/4,
         handle_trans_ack/4
        ]).

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").

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
    case ProtocolVersion of
	1 ->
	    ED = #'ErrorDescriptor'{errorCode = ?megaco_not_implemented,
				    errorText = "Trans requests not handled"},
	    {discard_ack, ED};
	_ ->
	    ED = #'ErrorDescriptor'{errorCode = ?megaco_version_not_supported,
				    errorText = "Only version 1 is supported"},
	    {discard_ack, ED}
    end.

%%----------------------------------------------------------------------
%% Optionally invoked for a time consuming transaction request
%%----------------------------------------------------------------------

handle_trans_long_request(ConnHandle, ProtocolVersion, ReqData) ->
    ED = #'ErrorDescriptor'{errorCode = ?megaco_not_implemented,
                            errorText = "Long trans requests not handled"},
    {discard_ack,  ED}.

%%----------------------------------------------------------------------
%% Optionally invoked for a transaction reply
%%----------------------------------------------------------------------

handle_trans_reply(ConnHandle, _, {error, {send_message_failed, Reason}}, _) ->
    megaco:disconnect(ConnHandle, {send_message_failed, Reason}),
    ok;
handle_trans_reply(ConnHandle, ProtocolVersion, ActualReply, ReplyData) ->
    ok.

%%----------------------------------------------------------------------
%% Optionally invoked for a transaction acknowledgement
%%----------------------------------------------------------------------

handle_trans_ack(ConnHandle, ProtocolVersion, AckStatus, AckData) ->
    ok.
