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
%% Purpose : Handle ASN.1 BER encoding of Megaco/H.248
%%----------------------------------------------------------------------

-module(megaco_binary_encoder).

-behaviour(megaco_encoder).

%% API
-export([encode_message/3, decode_message/3,
	
	 encode_transaction/3,
	 encode_action_requests/3,
	 encode_action_request/3,
	 
	 version_of/2, version_of/5]).

%% Internal API
-export([encode_message/5, decode_message/5, decode_message_dynamic/5,
	 encode_transaction/5,
	 encode_action_requests/5,
	 encode_action_request/5]).

%% Backward compatible functions:
-export([encode_message/2, decode_message/2]).

% -include_lib("megaco/include/megaco_message.hrl").
-include_lib("megaco/src/engine/megaco_message_internal.hrl").


%%----------------------------------------------------------------------
%% Convert a 'MegacoMessage' record into a binary
%% Return {ok, Binary} | {error, Reason}
%%----------------------------------------------------------------------

encode_message(EncodingConfig, 
	       #'MegacoMessage'{mess = #'Message'{version = V}} = MegaMsg) ->
    encode_message(EncodingConfig, V, MegaMsg).

encode_message(EncodingConfig, 1, MegaMsg) ->
    AsnMod   = megaco_ber_media_gateway_control_v1,
    TransMod = megaco_binary_transformer_v1,
    encode_message(EncodingConfig, MegaMsg, AsnMod, TransMod, io_list);
encode_message(EncodingConfig, 2, MegaMsg) ->
    AsnMod   = megaco_ber_media_gateway_control_v2,
    TransMod = megaco_binary_transformer_v2,
    encode_message(EncodingConfig, MegaMsg, AsnMod, TransMod, io_list).


%%----------------------------------------------------------------------
%% Convert a transaction (or transactions in the case of ack) record(s) 
%% into a binary
%% Return {ok, Binary} | {error, Reason}
%%----------------------------------------------------------------------

encode_transaction(EncodingConfig, 1, Trans) ->
    AsnMod   = megaco_ber_media_gateway_control_v1,
    TransMod = megaco_binary_transformer_v1,
    encode_transaction(EncodingConfig, Trans, AsnMod, TransMod, io_list);
encode_transaction(EncodingConfig, 2, Trans) ->
    AsnMod   = megaco_ber_media_gateway_control_v2,
    TransMod = megaco_binary_transformer_v2,
    encode_transaction(EncodingConfig, Trans, AsnMod, TransMod, io_list).


%%----------------------------------------------------------------------
%% Convert a list of ActionRequest record's into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_requests(EC, 1, ActReqs) when list(ActReqs) ->
    AsnMod   = megaco_ber_media_gateway_control_v1,
    TransMod = megaco_binary_transformer_v1,
    encode_action_requests(EC, ActReqs, AsnMod, TransMod, io_list);
encode_action_requests(EC, 2, ActReqs) when list(ActReqs) ->
    AsnMod   = megaco_ber_media_gateway_control_v1,
    TransMod = megaco_binary_transformer_v1,
    encode_action_requests(EC, ActReqs, AsnMod, TransMod, io_list).


%%----------------------------------------------------------------------
%% Convert a ActionRequest record into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_request(EC, 1, ActReq) ->
    AsnMod   = megaco_ber_media_gateway_control_v1,
    TransMod = megaco_binary_transformer_v1,
    encode_action_request(EC, ActReq, AsnMod, TransMod, io_list);
encode_action_request(EC, 2, ActReq) ->
    AsnMod   = megaco_ber_media_gateway_control_v1,
    TransMod = megaco_binary_transformer_v1,
    encode_action_request(EC, ActReq, AsnMod, TransMod, io_list).


%%----------------------------------------------------------------------
%% Detect (check) which version a message is
%% Return {ok, Version} | {error, Reason}
%%----------------------------------------------------------------------

version_of([] = EC, Binary) ->
    AsnModV1 = megaco_ber_bin_media_gateway_control_v1,
    AsnModV2 = megaco_ber_bin_media_gateway_control_v2,
    version_of(EC, Binary, dynamic, AsnModV1, AsnModV2);

version_of([native] = EC, Binary) ->
    AsnModV1 = megaco_ber_bin_media_gateway_control_v1,
    AsnModV2 = megaco_ber_bin_media_gateway_control_v2,
    version_of(EC, Binary, dynamic, AsnModV1, AsnModV2);

version_of([driver|EC], Binary) ->
    AsnModV1 = megaco_ber_bin_drv_media_gateway_control_v1,
    AsnModV2 = megaco_ber_bin_drv_media_gateway_control_v2,
    version_of(EC, Binary, dynamic, AsnModV1, AsnModV2);

%% All the possible (valid) encoding configs have already been
%% taken care of, but pass it on just in case...
version_of(EncodingConfig, Binary) ->
    AsnModV1 = megaco_ber_bin_media_gateway_control_v1,
    AsnModV2 = megaco_ber_bin_media_gateway_control_v2,
    version_of(EncodingConfig, Binary, dynamic, AsnModV1, AsnModV2).

version_of(_EncodingConfig, Binary, dynamic, AsnModV1, _AsnModV2) 
  when binary(Binary), atom(AsnModV1) ->
    case (catch AsnModV1:decode_message_version(Binary)) of
	{ok, PartialMsg} ->
	    V = (PartialMsg#'MegacoMessage'.mess)#'Message'.version,
	    {ok, V};
	Error ->
	    Error
    end;
version_of(_EncodingConfig, Binary, 1, AsnModV1, AsnModV2) 
  when binary(Binary), atom(AsnModV1), atom(AsnModV2) ->
    version_of([AsnModV1, AsnModV2], Binary, []);
version_of(_EncodingConfig, Binary, 2, AsnModV1, AsnModV2) 
  when binary(Binary), atom(AsnModV1), atom(AsnModV2) ->
    version_of([AsnModV2, AsnModV1], Binary, []).

version_of([], _Binary, Err) ->
    {error, {decode_failed, lists:reverse(Err)}};
version_of([AsnMod|AsnMods], Binary, Errs) ->
    case (catch asn1rt:decode(AsnMod, 'MegacoMessage', Binary)) of
	{ok, M} ->
	    V = (M#'MegacoMessage'.mess)#'Message'.version,
	    {ok, V};
	Err ->
	    version_of(AsnMods, Binary, [Err|Errs])
    end.

	    
%%----------------------------------------------------------------------
%% Convert a binary into a 'MegacoMessage' record
%% Return {ok, MegacoMessageRecord} | {error, Reason}
%%----------------------------------------------------------------------

decode_message(EncodingConfig, Binary) ->
    decode_message(EncodingConfig, 1, Binary).

decode_message([] = EC, dynamic, Binary) ->
    AsnModV1 = megaco_ber_bin_media_gateway_control_v1,
    AsnModV2 = megaco_ber_bin_media_gateway_control_v2,
    decode_message_dynamic(EC, Binary, AsnModV1, AsnModV2, binary);

decode_message([native] = EC, dynamic, Binary) ->
    AsnModV1 = megaco_ber_bin_media_gateway_control_v1,
    AsnModV2 = megaco_ber_bin_media_gateway_control_v2,
    decode_message_dynamic(EC, Binary, AsnModV1, AsnModV2, binary);

decode_message([driver|EC], dynamic, Binary) ->
    AsnModV1 = megaco_ber_bin_drv_media_gateway_control_v1,
    AsnModV2 = megaco_ber_bin_drv_media_gateway_control_v2,
    decode_message_dynamic(EC, Binary, AsnModV1, AsnModV2, binary);

%% All the possible (valid) encoding configs have already been
%% taken care of, but pass it on just in case...
decode_message(EncodingConfig, dynamic, Binary) ->
    AsnModV1 = megaco_ber_bin_media_gateway_control_v1,
    AsnModV2 = megaco_ber_bin_media_gateway_control_v2,
    decode_message_dynamic(EncodingConfig, Binary, 
     			   AsnModV1, AsnModV2, binary);


decode_message([] = EC, 1, Binary) ->
    AsnMod   = megaco_ber_bin_media_gateway_control_v1,
    TransMod = megaco_binary_transformer_v1,
    decode_message(EC, Binary, AsnMod, TransMod, binary);

decode_message([native] = EC, 1, Binary) ->
    AsnMod   = megaco_ber_bin_media_gateway_control_v1,
    TransMod = megaco_binary_transformer_v1,
    decode_message(EC, Binary, AsnMod, TransMod, binary);

decode_message([driver|EC], 1, Binary) ->
    AsnMod   = megaco_ber_bin_drv_media_gateway_control_v1,
    TransMod = megaco_binary_transformer_v1,
    decode_message(EC, Binary, AsnMod, TransMod, binary);

%% All the possible (valid) encoding configs have already been
%% taken care of, but pass it on just in case...
decode_message(EncodingConfig, 1, Binary) ->
    AsnMod   = megaco_ber_bin_media_gateway_control_v1,
    TransMod = megaco_binary_transformer_v1,
    decode_message(EncodingConfig, Binary, AsnMod, TransMod, binary);


decode_message([] = EC, 2, Binary) ->
    AsnMod   = megaco_ber_bin_media_gateway_control_v2,
    TransMod = megaco_binary_transformer_v2,
    decode_message(EC, Binary, AsnMod, TransMod, binary);

decode_message([native] = EC, 2, Binary) ->
    AsnMod   = megaco_ber_bin_media_gateway_control_v2,
    TransMod = megaco_binary_transformer_v2,
    decode_message(EC, Binary, AsnMod, TransMod, binary);

decode_message([driver|EC], 2, Binary) ->
    AsnMod   = megaco_ber_bin_drv_media_gateway_control_v2,
    TransMod = megaco_binary_transformer_v2,
    decode_message(EC, Binary, AsnMod, TransMod, binary);

%% All the possible (valid) encoding configs have already been
%% taken care of, but pass it on just in case...
decode_message(EncodingConfig, 2, Binary) ->
    AsnMod   = megaco_ber_bin_media_gateway_control_v2,
    TransMod = megaco_binary_transformer_v2,
    decode_message(EncodingConfig, Binary, AsnMod, TransMod, binary).



%%----------------------------------------------------------------------
%% Convert a 'MegacoMessage' record into a binary
%% Return {ok, Binary} | {error, Reason}
%%----------------------------------------------------------------------

encode_message([native], MegaMsg, AsnMod, _TransMod, binary) 
  when record(MegaMsg, 'MegacoMessage') ->
    asn1rt:encode(AsnMod, 'MegacoMessage', MegaMsg);
encode_message(Config, MegaMsg, AsnMod, TransMod, binary) 
  when list(Config), record(MegaMsg, 'MegacoMessage') ->
    MegaMsg2 = TransMod:tr_message(MegaMsg, encode, Config),
    asn1rt:encode(AsnMod, 'MegacoMessage', MegaMsg2);
encode_message(Config, MegaMsg, AsnMod, TransMod, io_list) ->
    case encode_message(Config, MegaMsg, AsnMod, TransMod, binary) of
	{ok, Bin} when binary(Bin) ->
	    {ok, Bin};
	{ok, DeepIoList} ->
	    Bin = erlang:list_to_binary(DeepIoList),
	    {ok, Bin};
	{error, Reason} ->
	    {error, Reason} 
    end;
encode_message(Config, MegaMsg, _AsnMod, _TransMod, _Type)
  when record(MegaMsg, 'MegacoMessage')  ->
    {error, {bad_encoding_config, Config}};
encode_message(_Config, MegaMsg, _AsnMod, _TransMod, _Type) ->
    {error, {no_megaco_message, MegaMsg}}.


%%----------------------------------------------------------------------
%% Convert a transaction (or transactions in the case of ack) record(s) 
%% into a binary
%% Return {ok, Binary} | {error, Reason}
%%----------------------------------------------------------------------

%% Should handle encoding of all types of transactions:
%% TransactionAck, TransactionPending, TransactionRequest
%% and TransactionReply
encode_transaction(Config, [T|_] = Ts, AsnMod, TransMod, Type) 
  when record(T, 'TransactionAck') ->
    Tag = transactionResponseAck,
    do_encode_transaction(Config, Tag, Ts, AsnMod, TransMod, Type);
encode_transaction(Config, T, AsnMod, TransMod, Type) 
  when record(T, 'TransactionAck') ->
    Tag = transactionResponseAck,
    do_encode_transaction(Config, Tag, [T], AsnMod, TransMod, Type);
encode_transaction(Config, T, AsnMod, TransMod, Type) 
  when record(T, 'TransactionPending') ->
    Tag = transactionPending,
    do_encode_transaction(Config, Tag, T, AsnMod, TransMod, Type);
encode_transaction(Config, T, AsnMod, TransMod, Type) 
  when record(T, 'TransactionRequest') ->
    Tag = transactionRequest,
    do_encode_transaction(Config, Tag, T, AsnMod, TransMod, Type);
encode_transaction(Config, T, AsnMod, TransMod, Type) 
  when record(T, 'TransactionReply') ->
    Tag = transactionReply,
    do_encode_transaction(Config, Tag, T, AsnMod, TransMod, Type);
encode_transaction(_Config, T, _AsnMod, _TransMod, _Type) ->
    {error, {no_megaco_transaction, T}}.

do_encode_transaction([native], _Tag, _T, _AsnMod, _TransMod, binary) ->
    %% asn1rt:encode(AsnMod, element(1, T), T);
    {error, not_implemented};
do_encode_transaction(Config, _Tag, _T, _AsnMod, _TransMod, binary) 
  when list(Config) ->
    %% T2 = TransMod:tr_transaction({Tag,T}, encode, Config),
    %% asn1rt:encode(AsnMod, element(1, T), T2);
    {error, not_implemented};
do_encode_transaction(Config, Tag, T, AsnMod, TransMod, io_list) ->
    case do_encode_transaction(Config, Tag, T, AsnMod, TransMod, binary) of
	{ok, Bin} when binary(Bin) ->
	    {ok, Bin};
	{ok, DeepIoList} ->
	    Bin = erlang:list_to_binary(DeepIoList),
	    {ok, Bin};
	{error, Reason} ->
	    {error, Reason} 
    end;
do_encode_transaction(Config, _Tag, _T, _AsnMod, _TransMod, _Type) ->
    {error, {bad_encoding_config, Config}}.


%%----------------------------------------------------------------------
%% Convert a list of ActionRequest record's into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_requests([native], _ARs, _AsnMod, _TransMod, binary) ->
    %% asn1rt:encode(AsnMod, element(1, T), T);
    {error, not_implemented};
encode_action_requests(_Config, _ARs0, _AsnMod, _TransMod, binary) ->
    {error, not_implemented};
encode_action_requests(Config, ARs, AsnMod, TransMod, io_list) ->
    case encode_action_requests(Config, ARs, AsnMod, TransMod, binary) of
	{ok, Bin} when binary(Bin) ->
	    {ok, Bin};
	{ok, DeepIoList} ->
	    Bin = erlang:list_to_binary(DeepIoList),
	    {ok, Bin};
	{error, Reason} ->
	    {error, Reason} 
    end;
encode_action_requests(Config, _ARs, _AsnMod, _TransMod, _Type) ->
    {error, {bad_encoding_config, Config}}.


%%----------------------------------------------------------------------
%% Convert a ActionRequest record into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_request([native], _ARs, _AsnMod, _TransMod, binary) ->
    %% asn1rt:encode(AsnMod, element(1, T), T);
    {error, not_implemented};
encode_action_request(_Config, _ARs0, _AsnMod, _TransMod, binary) ->
    {error, not_implemented};
encode_action_request(Config, ARs, AsnMod, TransMod, io_list) ->
    case encode_action_request(Config, ARs, AsnMod, TransMod, binary) of
	{ok, Bin} when binary(Bin) ->
	    {ok, Bin};
	{ok, DeepIoList} ->
	    Bin = erlang:list_to_binary(DeepIoList),
	    {ok, Bin};
	{error, Reason} ->
	    {error, Reason} 
    end;
encode_action_request(Config, _ARs, _AsnMod, _TransMod, _Type) ->
    {error, {bad_encoding_config, Config}}.


%%----------------------------------------------------------------------
%% Convert a binary into a 'MegacoMessage' record
%% Return {ok, MegacoMessageRecord} | {error, Reason}
%%----------------------------------------------------------------------

decode_message_dynamic(Config, Bin, AsnModV1, AsnModV2, Form)
  when list(Config), binary(Bin) ->
    case AsnModV1:decode_message_version(Bin) of
	{ok, PartialMsg} ->
	    V = (PartialMsg#'MegacoMessage'.mess)#'Message'.version,
	    case V of
		1 -> 
		    TransMod = megaco_binary_transformer_v1,
		    decode_message(Config, Bin, AsnModV1, TransMod, Form);
		2 ->
		    TransMod = megaco_binary_transformer_v2,
		    decode_message(Config, Bin, AsnModV2, TransMod, Form)
	    end;
	{error, Reason} ->
	    {error, Reason}
    end;
decode_message_dynamic(Config, Bin, _AsnModV1, _AsnModV2, _Type) 
  when binary(Bin) ->
    {error, {bad_encoding_config, Config}};
decode_message_dynamic(_Config, _BadBin, _AsnMod, _AsnModV1, _Type) ->
    {error, no_binary}.


decode_message(Config, Bin, AsnMod, TransMod, binary) ->
    case asn1rt:decode(AsnMod, 'MegacoMessage', Bin) of
	{ok, MegaMsg} ->
	    case Config of
		[native] ->
		    {ok, MegaMsg};
		_ ->		
		    {ok, TransMod:tr_message(MegaMsg, decode, Config)}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end;
decode_message(Config, Bin, AsnMod, TransMod, io_list) ->
    ShallowIoList = erlang:binary_to_list(Bin),
    case asn1rt:decode(AsnMod, 'MegacoMessage', ShallowIoList) of
	{ok, MegaMsg} ->
	    case Config of
		[native] ->
		    {ok, MegaMsg};
		_ ->
		    {ok, TransMod:tr_message(MegaMsg, decode, Config)}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

