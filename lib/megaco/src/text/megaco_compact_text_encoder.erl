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
%% Purpose: Encode COMPACT Megaco/H.248 text messages from internal form
%%----------------------------------------------------------------------

-module(megaco_compact_text_encoder).

-behaviour(megaco_encoder).

-export([encode_message/3, decode_message/3,
	 decode_mini_message/3, 

	 version_of/2, 

	 encode_transaction/3,
	 encode_action_requests/3,
	 encode_action_request/3,
	 encode_command_request/3,
	 encode_action_reply/3]).

%% Backward compatible funcs:
-export([encode_message/2, decode_message/2]).

-include_lib("megaco/src/engine/megaco_message_internal.hrl").

-define(V1_PARSE_MOD,     megaco_text_parser_v1).
-define(V2_PARSE_MOD,     megaco_text_parser_v2).
-define(V3_PARSE_MOD,     megaco_text_parser_v3).
-define(PREV3A_PARSE_MOD, megaco_text_parser_prev3a).


%%----------------------------------------------------------------------
%% Convert a 'MegacoMessage' record into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------

encode_message(EncodingConfig, 
	       #'MegacoMessage'{mess = #'Message'{version = V}} = MegaMsg) ->
    encode_message(EncodingConfig, V, MegaMsg).

encode_message([{version3,_}|EC], 1, MegaMsg) ->
    megaco_compact_text_encoder_v1:encode_message(EC, MegaMsg);
encode_message(EC, 1, MegaMsg) ->
    megaco_compact_text_encoder_v1:encode_message(EC, MegaMsg);
encode_message([{version3,_}|EC], 2, MegaMsg) ->
    megaco_compact_text_encoder_v2:encode_message(EC, MegaMsg);
encode_message(EC, 2, MegaMsg) ->
    megaco_compact_text_encoder_v2:encode_message(EC, MegaMsg);
encode_message([{version3,prev3a}|EC], 3, MegaMsg) ->
    megaco_compact_text_encoder_prev3a:encode_message(EC, MegaMsg);
encode_message([{version3,v3}|EC], 3, MegaMsg) ->
    megaco_compact_text_encoder_v3:encode_message(EC, MegaMsg);
encode_message(EC, 3, MegaMsg) ->
    megaco_compact_text_encoder_v3:encode_message(EC, MegaMsg).


%%----------------------------------------------------------------------
%% Convert a binary into a 'MegacoMessage' record
%% Return {ok, MegacoMessageRecord} | {error, Reason}
%%----------------------------------------------------------------------

version_of(_EC, Bin) ->
    case megaco_text_scanner:scan(Bin) of
	{ok, _Tokens, V, _LastLine} ->
	    {ok, V};
	{error, Reason, Line} ->
	    {error, {decode_failed, Reason, Line}}
    end.


decode_message(EC, Bin) ->
    decode_message(EC, dynamic, Bin).

decode_message([], _, Bin) when binary(Bin) ->
    case megaco_text_scanner:scan(Bin) of
	{ok, Tokens, 1, _LastLine} ->
	    do_decode_message(?V1_PARSE_MOD, Tokens, Bin);

	{ok, Tokens, 2, _LastLine} ->
	    do_decode_message(?V2_PARSE_MOD, Tokens, Bin);

	{ok, Tokens, 3, _LastLine} ->
	    do_decode_message(?V3_PARSE_MOD, Tokens, Bin);

	{error, Reason, Line} ->               %% OTP-4007
	    parse_error(Reason, Line, [], Bin) %% OTP-4007
    end;
decode_message([{version3,prev3a}], _, Bin) when binary(Bin) ->
    case megaco_text_scanner:scan(Bin) of
	{ok, Tokens, 1, _LastLine} ->
	    do_decode_message(?V1_PARSE_MOD, Tokens, Bin);

	{ok, Tokens, 2, _LastLine} ->
	    do_decode_message(?V2_PARSE_MOD, Tokens, Bin);

	{ok, Tokens, 3, _LastLine} ->
	    do_decode_message(?PREV3A_PARSE_MOD, Tokens, Bin);

	{error, Reason, Line} ->               %% OTP-4007
	    parse_error(Reason, Line, [], Bin) %% OTP-4007
    end;
decode_message([{version3,v3}], _, Bin) when binary(Bin) ->
    case megaco_text_scanner:scan(Bin) of
	{ok, Tokens, 1, _LastLine} ->
	    do_decode_message(?V1_PARSE_MOD, Tokens, Bin);

	{ok, Tokens, 2, _LastLine} ->
	    do_decode_message(?V2_PARSE_MOD, Tokens, Bin);

	{ok, Tokens, 3, _LastLine} ->
	    do_decode_message(?V3_PARSE_MOD, Tokens, Bin);

	{error, Reason, Line} ->               %% OTP-4007
	    parse_error(Reason, Line, [], Bin) %% OTP-4007
    end;
decode_message([{flex, Port}], _, Bin) when binary(Bin) ->
    case megaco_flex_scanner:scan(Bin, Port) of
	{ok, Tokens, 1, _LastLine} ->
	    do_decode_message(?V1_PARSE_MOD, Tokens, Bin);

	{ok, Tokens, 2, _LastLine} ->
	    do_decode_message(?V2_PARSE_MOD, Tokens, Bin);

	{ok, Tokens, 3, _LastLine} ->
	    do_decode_message(?V3_PARSE_MOD, Tokens, Bin);

	{error, Reason, Line} ->               %% OTP-4007
	    parse_error(Reason, Line, [], Bin) %% OTP-4007
    end;
decode_message([{version3,prev3a},{flex, Port}], _, Bin) when binary(Bin) ->
    case megaco_flex_scanner:scan(Bin, Port) of
	{ok, Tokens, 1, _LastLine} ->
	    do_decode_message(?V1_PARSE_MOD, Tokens, Bin);

	{ok, Tokens, 2, _LastLine} ->
	    do_decode_message(?V2_PARSE_MOD, Tokens, Bin);

	{ok, Tokens, 3, _LastLine} ->
	    do_decode_message(?PREV3A_PARSE_MOD, Tokens, Bin);

	{error, Reason, Line} ->               %% OTP-4007
	    parse_error(Reason, Line, [], Bin) %% OTP-4007
    end;
decode_message([{version3,v3},{flex, Port}], _, Bin) when binary(Bin) ->
    case megaco_flex_scanner:scan(Bin, Port) of
	{ok, Tokens, 1, _LastLine} ->
	    do_decode_message(?V1_PARSE_MOD, Tokens, Bin);

	{ok, Tokens, 2, _LastLine} ->
	    do_decode_message(?V2_PARSE_MOD, Tokens, Bin);

	{ok, Tokens, 3, _LastLine} ->
	    do_decode_message(?V3_PARSE_MOD, Tokens, Bin);

	{error, Reason, Line} ->               %% OTP-4007
	    parse_error(Reason, Line, [], Bin) %% OTP-4007
    end;
decode_message(EC, _, Bin) when binary(Bin) ->
    {error, {bad_encoding_config, EC}};
decode_message(_EC, _, _BadBin) ->
    {error, bad_binary}.


do_decode_message(ParseMod, Tokens, Bin) ->
    case (catch ParseMod:parse(Tokens)) of
	{ok, MegacoMessage} ->
	    {ok, MegacoMessage};
	{error, Reason} ->
	    parse_error(Reason, Tokens, Bin);

	%% OTP-4007
	{'EXIT', Reason} ->
	    parse_error(Reason, Tokens, Bin)
    end.


decode_mini_message(EC, _, Bin) when binary(Bin) ->
    megaco_text_mini_decoder:decode_message(EC, Bin).


parse_error(Reason, Tokens, Chars) ->
    {error, [{reason, Reason}, {token, Tokens}, {chars, Chars}]}.

parse_error(Reason, Line, Tokens, Chars) ->
    {error, [{reason, Reason, Line}, {token, Tokens}, {chars, Chars}]}.


%%----------------------------------------------------------------------
%% Convert a transaction record into a deep io list
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_transaction([{version3,_}|EC], 1, Trans) ->
    megaco_compact_text_encoder_v1:encode_transaction(EC, Trans);
encode_transaction(EC, 1, Trans) ->
    megaco_compact_text_encoder_v1:encode_transaction(EC, Trans);
encode_transaction([{version3,_}|EC], 2, Trans) ->
    megaco_compact_text_encoder_v2:encode_transaction(EC, Trans);
encode_transaction(EC, 2, Trans) ->
    megaco_compact_text_encoder_v2:encode_transaction(EC, Trans);
encode_transaction([{version3,prev3a}|EC], 3, Trans) ->
    megaco_compact_text_encoder_prev3a:encode_transaction(EC, Trans);
encode_transaction([{version3,v3}|EC], 3, Trans) ->
    megaco_compact_text_encoder_v3:encode_transaction(EC, Trans);
encode_transaction(EC, 3, Trans) ->
    megaco_compact_text_encoder_v3:encode_transaction(EC, Trans).


%%----------------------------------------------------------------------
%% Convert a list of ActionRequest record's into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_requests([{version3,_}|EC], 1, ActReqs) 
  when list(ActReqs) ->
    megaco_compact_text_encoder_v1:encode_action_requests(EC, ActReqs);
encode_action_requests(EC, 1, ActReqs) when list(ActReqs) ->
    megaco_compact_text_encoder_v1:encode_action_requests(EC, ActReqs);
encode_action_requests([{version3,_}|EC], 2, ActReqs) 
  when list(ActReqs) ->
    megaco_compact_text_encoder_v2:encode_action_requests(EC, ActReqs);
encode_action_requests(EC, 2, ActReqs) when list(ActReqs) ->
    megaco_compact_text_encoder_v2:encode_action_requests(EC, ActReqs);
encode_action_requests([{version3,prev3a}|EC], 3, ActReqs) 
  when list(ActReqs) ->
    megaco_compact_text_encoder_prev3a:encode_action_requests(EC, ActReqs);
encode_action_requests([{version3,v3}|EC], 3, ActReqs) 
  when list(ActReqs) ->
    megaco_compact_text_encoder_v3:encode_action_requests(EC, ActReqs);
encode_action_requests(EC, 3, ActReqs) when list(ActReqs) ->
    megaco_compact_text_encoder_v3:encode_action_requests(EC, ActReqs).


%%----------------------------------------------------------------------
%% Convert a ActionRequest record into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_request([{version3,_}|EC], 1, ActReq) ->
    megaco_compact_text_encoder_v1:encode_action_request(EC, ActReq);
encode_action_request(EC, 1, ActReq) ->
    megaco_compact_text_encoder_v1:encode_action_request(EC, ActReq);
encode_action_request([{version3,_}|EC], 2, ActReq) ->
    megaco_compact_text_encoder_v2:encode_action_request(EC, ActReq);
encode_action_request(EC, 2, ActReq) ->
    megaco_compact_text_encoder_v2:encode_action_request(EC, ActReq);
encode_action_request([{version3,prev3a}|EC], 3, ActReq) ->
    megaco_compact_text_encoder_prev3a:encode_action_request(EC, ActReq);
encode_action_request([{version3,v3}|EC], 3, ActReq) ->
    megaco_compact_text_encoder_v3:encode_action_request(EC, ActReq);
encode_action_request(EC, 3, ActReq) ->
    megaco_compact_text_encoder_v3:encode_action_request(EC, ActReq).


%%----------------------------------------------------------------------
%% Convert a CommandRequest record into a deep io list
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_command_request([{version3,_}|EC], 1, CmdReq) ->
    megaco_compact_text_encoder_v1:encode_command_request(EC, CmdReq);
encode_command_request(EC, 1, CmdReq) ->
    megaco_compact_text_encoder_v1:encode_command_request(EC, CmdReq);
encode_command_request([{version3,_}|EC], 2, CmdReq) ->
    megaco_compact_text_encoder_v2:encode_command_request(EC, CmdReq);
encode_command_request(EC, 2, CmdReq) ->
    megaco_compact_text_encoder_v2:encode_command_request(EC, CmdReq);
encode_command_request([{version3,prev3a}|EC], 3, CmdReq) ->
    megaco_compact_text_encoder_prev3a:encode_command_request(EC, CmdReq);
encode_command_request([{version3,v3}|EC], 3, CmdReq) ->
    megaco_compact_text_encoder_v3:encode_command_request(EC, CmdReq);
encode_command_request(EC, 3, CmdReq) ->
    megaco_compact_text_encoder_v3:encode_command_request(EC, CmdReq).


%%----------------------------------------------------------------------
%% Convert a action reply into a deep io list
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_reply([{version3,_}|EC], 1, ActRep) ->
    megaco_compact_text_encoder_v1:encode_action_reply(EC, ActRep);
encode_action_reply(EC, 1, ActRep) ->
    megaco_compact_text_encoder_v1:encode_action_reply(EC, ActRep);
encode_action_reply([{version3,_}|EC], 2, ActRep) ->
    megaco_compact_text_encoder_v2:encode_action_reply(EC, ActRep);
encode_action_reply(EC, 2, ActRep) ->
    megaco_compact_text_encoder_v2:encode_action_reply(EC, ActRep);
encode_action_reply([{version3,prev3a}|EC], 3, ActRep) ->
    megaco_compact_text_encoder_prev3a:encode_action_reply(EC, ActRep);
encode_action_reply([{version3,v3}|EC], 3, ActRep) ->
    megaco_compact_text_encoder_v3:encode_action_reply(EC, ActRep);
encode_action_reply(EC, 3, ActRep) ->
    megaco_compact_text_encoder_v3:encode_action_reply(EC, ActRep).


% d(F) ->
%     d(F, []).

% d(F, A) ->
%     d(get(dbg), F, A).

% d(true, F, A) ->
%     io:format("~p:" ++ F ++ "~n", [?MODULE|A]);
% d(_, _, _) ->
%     ok.
