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
%%% Purpose: Encode PRETTY Megaco/H.248 text messages from internal form
%%----------------------------------------------------------------------

-module(megaco_pretty_text_encoder).

-behaviour(megaco_encoder).

-export([encode_message/3, decode_message/3,

	 version_of/2, 

	 encode_transaction/3,
	 encode_action_requests/3,
	 encode_action_request/3,
	 encode_command_request/3,
	 encode_action_reply/3]).

%% Backward compatible funcs:
-export([encode_message/2, decode_message/2,

	 encode_transaction/1,
	 encode_command_request/1,
	 encode_action_reply/1]).

%% Do we need these here?
-export([trim_quoted_string/1,
	 term_to_compact_string/1,
	 term_to_pretty_string/1]).

-include("megaco_text_tokens.hrl").
-include_lib("megaco/src/engine/megaco_message_internal.hrl").

-define(V1_PARSE_MOD, megaco_text_parser_v1).
-define(V2_PARSE_MOD, megaco_text_parser_v2).


%%----------------------------------------------------------------------
%% Convert a 'MegacoMessage' record into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------

encode_message(EncodingConfig, 
	       #'MegacoMessage'{mess = #'Message'{version = V}} = MegaMsg) ->
    encode_message(EncodingConfig, V, MegaMsg).


encode_message(EncodingConfig, 1, MegaMsg) ->
    megaco_pretty_text_encoder_v1:encode_message(EncodingConfig, MegaMsg);
encode_message(EncodingConfig, 2, MegaMsg) ->
    megaco_pretty_text_encoder_v2:encode_message(EncodingConfig, MegaMsg).
	    

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

	{error, Reason, Line} ->               %% OTP-4007
	    parse_error(Reason, Line, [], Bin) %% OTP-4007
    end;
decode_message([{flex, Port}], _, Bin) when binary(Bin) ->
    case megaco_flex_scanner:scan(Bin, Port) of
	{ok, Tokens, 1, _LastLine} ->
	    do_decode_message(?V1_PARSE_MOD, Tokens, Bin);

	{ok, Tokens, 2, _LastLine} ->
	    do_decode_message(?V2_PARSE_MOD, Tokens, Bin);

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

parse_error(Reason, Tokens, Chars) ->
    {error, [{reason, Reason}, {token, Tokens}, {chars, Chars}]}.

parse_error(Reason, Line, Tokens, Chars) ->
    {error, [{reason, Reason, Line}, {token, Tokens}, {chars, Chars}]}.


%%----------------------------------------------------------------------
%% Convert a transaction record into a binary
%% Return {ok, Bin} | {error, Reason}
%%----------------------------------------------------------------------
encode_transaction(Trans) ->
    encode_transaction([], 1, Trans).

encode_transaction(EC, 1, Trans) ->
    megaco_pretty_text_encoder_v1:encode_transaction(EC, Trans);
encode_transaction(EC, 2, Trans) ->
    megaco_pretty_text_encoder_v2:encode_transaction(EC, Trans).


%%----------------------------------------------------------------------
%% Convert a list of ActionRequest record's into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_requests(EC, 1, ActReqs) when list(ActReqs) ->
    megaco_pretty_text_encoder_v1:encode_action_requests(EC, ActReqs);
encode_action_requests(EC, 2, ActReqs) when list(ActReqs) ->
    megaco_pretty_text_encoder_v2:encode_action_requests(EC, ActReqs).


%%----------------------------------------------------------------------
%% Convert a ActionRequest record into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_request(EC, 1, ActReq) ->
    megaco_compact_text_encoder_v1:encode_action_request(EC, ActReq);
encode_action_request(EC, 2, ActReq) ->
    megaco_compact_text_encoder_v2:encode_action_request(EC, ActReq).


%%----------------------------------------------------------------------
%% Convert a CommandRequest record into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_command_request(CmdReq) ->
    encode_command_request([], 1, CmdReq).

encode_command_request(EC, 1, CmdReq) ->
    megaco_pretty_text_encoder_v1:encode_command_request(EC, CmdReq);
encode_command_request(EC, 2, CmdReq) ->
    megaco_pretty_text_encoder_v2:encode_command_request(EC, CmdReq).


%%----------------------------------------------------------------------
%% Convert a action reply into a deep io list
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_reply(ActRep) ->
    encode_action_reply([], 1, ActRep).
				       
encode_action_reply(EC, 1, ActRep) ->
    megaco_pretty_text_encoder_v1:encode_action_reply(EC, ActRep);
encode_action_reply(EC, 2, ActRep) ->
    megaco_pretty_text_encoder_v2:encode_action_reply(EC, ActRep).
	    

%%----------------------------------------------------------------------
term_to_compact_string(Term) ->
    case catch io_lib:format("~s", [Term]) of
        {'EXIT', _} -> lists:flatten(io_lib:format("~w", [Term]));
        GoodString  -> lists:flatten(GoodString)
    end.

%%----------------------------------------------------------------------
term_to_pretty_string(Term) ->
    case catch io_lib:format("~s", [Term]) of
        {'EXIT', _} -> lists:flatten(io_lib:format("~p", [Term]));
        GoodString  -> lists:flatten(GoodString)
    end.

%%----------------------------------------------------------------------
trim_quoted_string([H | T]) ->
    case ?classify_char(H) of
	safe_char   -> [H  | trim_quoted_string(T)];
	rest_char   -> [H  | trim_quoted_string(T)];
	white_space -> [H  | trim_quoted_string(T)];
	_BadChar     -> [$? | trim_quoted_string(T)]
    end;
trim_quoted_string([]) ->
    [].


%%----------------------------------------------------------------------

% d(F) ->
%     d(F, []).

% d(F, A) ->
%     d(false, F, A).

% d(true, F, A) ->
%     io:format("~p:" ++ F ++ "~n", [?MODULE|A]);
% d(_, _, _) ->
%     ok.
