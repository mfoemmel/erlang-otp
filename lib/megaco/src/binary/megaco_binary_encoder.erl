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

-export([encode_message/2, decode_message/2]).
-export([encode_message/4, decode_message/4]).

-include_lib("megaco/src/engine/megaco_message_internal.hrl").

%%----------------------------------------------------------------------
%% Convert a 'MegacoMessage' record into a binary
%% Return {ok, Binary} | {error, Reason}
%%----------------------------------------------------------------------

encode_message(EncodingConfig, MegaMsg) ->
    AsnMod = megaco_ber_media_gateway_control,
    encode_message(EncodingConfig, MegaMsg, AsnMod, io_list).

%%----------------------------------------------------------------------
%% Convert a binary into a 'MegacoMessage' record
%% Return {ok, MegacoMessageRecord} | {error, Reason}
%%----------------------------------------------------------------------

decode_message(EncodingConfig, Binary) ->
    AsnMod = megaco_ber_bin_media_gateway_control,
    decode_message(EncodingConfig, Binary, AsnMod, binary).

%%----------------------------------------------------------------------
%% Convert a 'MegacoMessage' record into a binary
%% Return {ok, Binary} | {error, Reason}
%%----------------------------------------------------------------------

encode_message(Config, MegaMsg, AsnMod, binary) 
  when list(Config), record(MegaMsg, 'MegacoMessage') ->
    case Config of
	[native] ->
	    asn1rt:encode(AsnMod, 'MegacoMessage', MegaMsg);
	_ ->
	    MegaMsg2 = megaco_binary_transformer:tr_message(MegaMsg, encode, Config),
	    asn1rt:encode(AsnMod, 'MegacoMessage', MegaMsg2)
    end;
encode_message(Config, MegaMsg, AsnMod, io_list) ->
    case encode_message(Config, MegaMsg, AsnMod, binary) of
	{ok, Bin} when binary(Bin) ->
	    {ok, Bin};
	{ok, DeepIoList} ->
	    Bin = erlang:list_to_binary(DeepIoList),
	    {ok, Bin};
	{error, Reason} ->
	    {error, Reason} 
    end;
encode_message(Config, MegaMsg, _AsnMod, _Type)
  when record(MegaMsg, 'MegacoMessage')  ->
    {error, {bad_encoding_config, Config}};
encode_message(_Config, MegaMsg, _AsnMod, _Type) ->
    {error, {no_megaco_message, MegaMsg}}.
	 
%%----------------------------------------------------------------------
%% Convert a binary into a 'MegacoMessage' record
%% Return {ok, MegacoMessageRecord} | {error, Reason}
%%----------------------------------------------------------------------

decode_message(Config, Bin, AsnMod, binary)
  when list(Config), binary(Bin) ->
    case asn1rt:decode(AsnMod, 'MegacoMessage', Bin) of
	{ok, MegaMsg} ->
	    case Config of
		[native] ->
		    {ok, MegaMsg};
		_ ->		
		    {ok, megaco_binary_transformer:tr_message(MegaMsg, decode, Config)}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end;
decode_message(Config, Bin, AsnMod, io_list)
  when list(Config), binary(Bin) ->
    ShallowIoList = erlang:binary_to_list(Bin),
    case asn1rt:decode(AsnMod, 'MegacoMessage', ShallowIoList) of
	{ok, MegaMsg} ->
	    case Config of
		[native] ->
		    {ok, MegaMsg};
		_ ->
		    {ok, megaco_binary_transformer:tr_message(MegaMsg, decode, Config)}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end;
decode_message(Config, Bin, _AsnMod, _Type) when binary(Bin) ->
    {error, {bad_encoding_config, Config}};
decode_message(_Config, _BadBin, _AsnMod, _Type) ->
    {error, no_binary}.
    
