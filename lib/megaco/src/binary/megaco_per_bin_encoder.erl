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
%% Purpose : Handle ASN.1 PER encoding of Megaco/H.248
%%----------------------------------------------------------------------

-module(megaco_per_bin_encoder).

-behaviour(megaco_encoder).

-export([encode_message/3, decode_message/3,

	 encode_transaction/3,
	 encode_action_requests/3,
	 encode_action_request/3,

	 version_of/2]).

%% Backward compatible functions:
-export([encode_message/2, decode_message/2]).

-include_lib("megaco/src/engine/megaco_message_internal.hrl").

-define(V1_ASN1_MOD,     megaco_per_bin_media_gateway_control_v1).
-define(V2_ASN1_MOD,     megaco_per_bin_media_gateway_control_v2).
-define(V1_ASN1_MOD_DRV, megaco_per_bin_drv_media_gateway_control_v1).
-define(V2_ASN1_MOD_DRV, megaco_per_bin_drv_media_gateway_control_v2).

-define(V1_TRANS_MOD, megaco_binary_transformer_v1).
-define(V2_TRANS_MOD, megaco_binary_transformer_v2).


%%----------------------------------------------------------------------
%% Convert a 'MegacoMessage' record into a binary
%% Return {ok, Binary} | {error, Reason}
%%----------------------------------------------------------------------

encode_message(EncodingConfig, 
	       #'MegacoMessage'{mess = #'Message'{version = V}} = MegaMsg) ->
    encode_message(EncodingConfig, V, MegaMsg).

encode_message([] = EC, 1, MegaMsg) ->
    AsnMod   = ?V1_ASN1_MOD, 
    TransMod = ?V1_TRANS_MOD,
    megaco_binary_encoder:encode_message(EC, MegaMsg, AsnMod, TransMod, 
					 io_list);
encode_message([native] = EC, 1, MegaMsg) ->
    AsnMod   = ?V1_ASN1_MOD, 
    TransMod = ?V1_TRANS_MOD,
    megaco_binary_encoder:encode_message(EC, MegaMsg, AsnMod, TransMod, 
					 io_list);
encode_message([driver|EC], 1, MegaMsg) ->
    AsnMod   = ?V1_ASN1_MOD_DRV, 
    TransMod = ?V1_TRANS_MOD,
    megaco_binary_encoder:encode_message(EC, MegaMsg, AsnMod, TransMod, 
					 io_list);

%% All the possible (valid) encoding configs have already been
%% taken care of, but pass it on just in case...
encode_message(EncodingConfig, 1, MegaMsg) ->
    AsnMod   = ?V1_ASN1_MOD, 
    TransMod = ?V1_TRANS_MOD,
    megaco_binary_encoder:encode_message(EncodingConfig, MegaMsg, 
					 AsnMod, TransMod, io_list);

encode_message([] = EC, 2, MegaMsg) ->
    AsnMod   = ?V2_ASN1_MOD, 
    TransMod = ?V2_TRANS_MOD,
    megaco_binary_encoder:encode_message(EC, MegaMsg, AsnMod, TransMod, 
					 io_list);
encode_message([native] = EC, 2, MegaMsg) ->
    AsnMod   = ?V2_ASN1_MOD, 
    TransMod = ?V2_TRANS_MOD,
    megaco_binary_encoder:encode_message(EC, MegaMsg, AsnMod, TransMod, 
					 io_list);
encode_message([driver|EC], 2, MegaMsg) ->
    AsnMod   = ?V2_ASN1_MOD_DRV, 
    TransMod = ?V2_TRANS_MOD,
    megaco_binary_encoder:encode_message(EC, MegaMsg, AsnMod, TransMod, 
					 io_list);

%% All the possible (valid) encoding configs have already been
%% taken care of, but pass it on just in case...
encode_message(EncodingConfig, 2, MegaMsg) ->
    AsnMod   = ?V2_ASN1_MOD, 
    TransMod = ?V2_TRANS_MOD,
    megaco_binary_encoder:encode_message(EncodingConfig, MegaMsg, 
					 AsnMod, TransMod, io_list).


%%----------------------------------------------------------------------
%% Convert a transaction (or transactions in the case of ack) record(s) 
%% into a binary
%% Return {ok, Binary} | {error, Reason}
%%----------------------------------------------------------------------

%% encode_transaction([] = EC, 1, Trans) ->
%%     AsnMod   = ?V1_ASN1_MOD, 
%%     TransMod = ?V1_TRANS_MOD,
%%     megaco_binary_encoder:encode_transaction(EC, Trans, AsnMod, TransMod, 
%% 					     io_list);
%% encode_transaction([native] = EC, 1, Trans) ->
%%     AsnMod   = ?V1_ASN1_MOD, 
%%     TransMod = ?V1_TRANS_MOD,
%%     megaco_binary_encoder:encode_transaction(EC, Trans, AsnMod, TransMod, 
%% 					     io_list);
%% encode_transaction([driver|EC], 1, Trans) ->
%%     AsnMod   = ?V1_ASN1_MOD_DRV, 
%%     TransMod = ?V1_TRANS_MOD,
%%     megaco_binary_encoder:encode_transaction(EC, Trans, AsnMod, TransMod, 
%% 					     io_list);
encode_transaction(_EC, 1, _Trans) ->
    %%     AsnMod   = ?V1_ASN1_MOD, 
    %%     TransMod = ?V1_TRANS_MOD,
    %%     megaco_binary_encoder:encode_transaction(EC, Trans, AsnMod, TransMod, 
    %% 					     io_list);
    {error, not_implemented};

%% encode_transaction([] = EC, 2, Trans) ->
%%     AsnMod   = ?V2_ASN1_MOD, 
%%     TransMod = ?V2_TRANS_MOD,
%%     megaco_binary_encoder:encode_transaction(EC, Trans, AsnMod, TransMod,
%% 					     io_list);
%% encode_transaction([native] = EC, 2, Trans) ->
%%     AsnMod   = ?V2_ASN1_MOD, 
%%     TransMod = ?V2_TRANS_MOD,
%%     megaco_binary_encoder:encode_transaction(EC, Trans, AsnMod, TransMod,
%% 					     io_list);
%% encode_transaction([driver|EC], 2, Trans) ->
%%     AsnMod   = ?V2_ASN1_MOD_DRV, 
%%     TransMod = ?V2_TRANS_MOD,
%%     megaco_binary_encoder:encode_transaction(EC, Trans, AsnMod, TransMod,
%% 					     io_list);
encode_transaction(_EC, 2, _Trans) ->
    %%     AsnMod   = ?V2_ASN1_MOD, 
    %%     TransMod = ?V2_TRANS_MOD,
    %%     megaco_binary_encoder:encode_transaction(EC, Trans, AsnMod, TransMod,
    %% 					     io_list).
    {error, not_implemented}.


%%----------------------------------------------------------------------
%% Convert a list of ActionRequest record's into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_requests(_EC, 1, ActReqs) when list(ActReqs) ->
    %%     megaco_binary_encoder:encode_action_requests(EC, ActReqs,
    %% 						 ?V1_ASN1_MOD, 
    %% 						 ?V1_TRANS_MOD,
    %% 						 io_list);
    {error, not_implemented};
encode_action_requests(_EC, 2, ActReqs) when list(ActReqs) ->
    %%     megaco_binary_encoder:encode_action_requests(EC, ActReqs,
    %% 						 ?V1_ASN1_MOD, 
    %% 						 ?V1_TRANS_MOD,
    %% 						 io_list).
    {error, not_implemented}.


%%----------------------------------------------------------------------
%% Convert a ActionRequest record into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_request(_EC, 1, _ActReq) ->
    %%     megaco_binary_encoder:encode_action_request(EC, ActReq,
    %% 						?V1_ASN1_MOD, 
    %% 						?V1_TRANS_MOD,
    %% 						io_list);
    {error, not_implemented};
encode_action_request(_EC, 2, _ActReq) ->
    %%     megaco_binary_encoder:encode_action_request(EC, ActReq,
    %% 						?V1_ASN1_MOD, 
    %% 						?V1_TRANS_MOD,
    %% 						io_list).
    {error, not_implemented}.


%%----------------------------------------------------------------------
%% Detect (check/get) message version
%% Return {ok, Version} | {error, Reason}
%%----------------------------------------------------------------------

version_of([] = EC, Binary) ->
    AsnModV1 = ?V1_ASN1_MOD, 
    AsnModV2 = ?V2_ASN1_MOD, 
    megaco_binary_encoder:version_of(EC, Binary, 1, 
				     AsnModV1, AsnModV2);
version_of([native] = EC, Binary) ->
    AsnModV1 = ?V1_ASN1_MOD, 
    AsnModV2 = ?V2_ASN1_MOD, 
    megaco_binary_encoder:version_of(EC, Binary, 1, 
				     AsnModV1, AsnModV2);
version_of([driver|EC], Binary) ->
    AsnModV1 = ?V1_ASN1_MOD_DRV, 
    AsnModV2 = ?V2_ASN1_MOD_DRV, 
    megaco_binary_encoder:version_of(EC, Binary, 1, 
				     AsnModV1, AsnModV2);

%% All the possible (valid) encoding configs have already been
%% taken care of, but pass it on just in case...
version_of(EncodingConfig, Binary) ->
    AsnModV1 = ?V1_ASN1_MOD, 
    AsnModV2 = ?V2_ASN1_MOD, 
    megaco_binary_encoder:version_of(EncodingConfig, Binary, 1, 
				     AsnModV1, AsnModV2).



%%----------------------------------------------------------------------
%% Convert a binary into a 'MegacoMessage' record
%% Return {ok, MegacoMessageRecord} | {error, Reason}
%%----------------------------------------------------------------------

decode_message(EncodingConfig, Binary) ->
    decode_message(EncodingConfig, 1, Binary).

%% PER does not support partial decode, so this means V1
decode_message([] = EC, 1, Binary) ->
    AsnMod   = ?V1_ASN1_MOD, 
    TransMod = ?V1_TRANS_MOD, 
    megaco_binary_encoder:decode_message(EC, Binary, 
					 AsnMod, TransMod, binary);
decode_message([native] = EC, 1, Binary) ->
    AsnMod   = ?V1_ASN1_MOD, 
    TransMod = ?V1_TRANS_MOD, 
    megaco_binary_encoder:decode_message(EC, Binary, 
					 AsnMod, TransMod, binary);
decode_message([driver|EC], 1, Binary) ->
    AsnMod   = ?V1_ASN1_MOD_DRV, 
    TransMod = ?V1_TRANS_MOD, 
    megaco_binary_encoder:decode_message(EC, Binary, 
					 AsnMod, TransMod, binary);

%% All the possible (valid) encoding configs have already been
%% taken care of, but pass it on just in case...
decode_message(EncodingConfig, 1, Binary) ->
    AsnMod   = ?V1_ASN1_MOD, 
    TransMod = ?V1_TRANS_MOD, 
    megaco_binary_encoder:decode_message(EncodingConfig, Binary, 
					 AsnMod, TransMod, binary);

decode_message([] = EC, 2, Binary) ->
    AsnMod   = ?V2_ASN1_MOD, 
    TransMod = ?V2_TRANS_MOD, 
    megaco_binary_encoder:decode_message(EC, Binary, 
					 AsnMod, TransMod, binary);
decode_message([native] = EC, 2, Binary) ->
    AsnMod   = ?V2_ASN1_MOD, 
    TransMod = ?V2_TRANS_MOD, 
    megaco_binary_encoder:decode_message(EC, Binary, 
					 AsnMod, TransMod, binary);
decode_message([driver|EC], 2, Binary) ->
    AsnMod   = ?V2_ASN1_MOD_DRV, 
    TransMod = ?V2_TRANS_MOD, 
    megaco_binary_encoder:decode_message(EC, Binary, 
					 AsnMod, TransMod, binary);

%% All the possible (valid) encoding configs have already been
%% taken care of, but pass it on just in case...
decode_message(EncodingConfig, 2, Binary) ->
    AsnMod   = ?V2_ASN1_MOD, 
    TransMod = ?V2_TRANS_MOD, 
    megaco_binary_encoder:decode_message(EncodingConfig, Binary, 
					 AsnMod, TransMod, binary).
