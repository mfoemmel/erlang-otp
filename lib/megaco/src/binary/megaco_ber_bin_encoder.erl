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

-module(megaco_ber_bin_encoder).

-behaviour(megaco_encoder).

-export([encode_message/2, decode_message/2,
	 encode_transaction/2]).

-define(ASN1_MOD, megaco_ber_bin_media_gateway_control).


%%----------------------------------------------------------------------
%% Convert a 'MegacoMessage' record into a binary
%% Return {ok, Binary} | {error, Reason}
%%----------------------------------------------------------------------

encode_message(EncodingConfig, MegaMsg) ->
    megaco_binary_encoder:encode_message(EncodingConfig, MegaMsg, 
					 ?ASN1_MOD, io_list).


%%----------------------------------------------------------------------
%% Convert a transaction record into a binary
%% Return {ok, Binary} | {error, Reason}
%%----------------------------------------------------------------------
encode_transaction(EncodingConfig, Trans) ->
    megaco_binary_encoder:encode_transaction(EncodingConfig, Trans, 
					     ?ASN1_MOD, io_list).


%%----------------------------------------------------------------------
%% Convert a binary into a 'MegacoMessage' record
%% Return {ok, MegacoMessageRecord} | {error, Reason}
%%----------------------------------------------------------------------

decode_message(EncodingConfig, Binary) ->
    megaco_binary_encoder:decode_message(EncodingConfig, Binary, 
					 ?ASN1_MOD, binary).
