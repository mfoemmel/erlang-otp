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

-module(megaco_binary_encoder_lib).

%% API
-export([version_of/4, 
	 decode_message/5, decode_message_dynamic/4, 
	 decode_mini_message/4, decode_mini_message_dynamic/4, 
	 encode_message/5, 
	 encode_transaction/5, 
	 encode_action_requests/5, 
	 encode_action_request/5]).

-include_lib("megaco/src/engine/megaco_message_internal.hrl").


%%----------------------------------------------------------------------
%% Detect (check) which version a message is
%% Return {ok, Version} | {error, Reason}
%%----------------------------------------------------------------------

version_of(_EC, Binary, dynamic, [AsnModV1|_AsnMods]) 
  when binary(Binary), atom(AsnModV1) ->
    case (catch AsnModV1:decode_message_version(Binary)) of
	{ok, PartialMsg} ->
	    V = (PartialMsg#'MegacoMessage'.mess)#'Message'.version,
	    {ok, V};
	Error ->
	    Error
    end;
version_of(_EC, Binary, 1, AsnMods) 
  when binary(Binary), list(AsnMods) ->
    version_of(AsnMods, Binary, []);
version_of(_EC, Binary, 2, [AsnModV1, AsnModV2, AsnModV3]) 
  when binary(Binary) ->
    version_of([AsnModV2, AsnModV1, AsnModV3], Binary, []);
version_of(_EC, Binary, 3, [AsnModV1, AsnModV2, AsnModV3]) 
  when binary(Binary) ->
    version_of([AsnModV3, AsnModV1, AsnModV2], Binary, []).

version_of([], _Binary, Err) ->
    {error, {decode_failed, lists:reverse(Err)}};
version_of([AsnMod|AsnMods], Binary, Errs) when atom(AsnMod) ->
    case (catch asn1rt:decode(AsnMod, 'MegacoMessage', Binary)) of
	{ok, M} ->
	    V = (M#'MegacoMessage'.mess)#'Message'.version,
	    {ok, V};
	Err ->
	    version_of(AsnMods, Binary, [Err|Errs])
    end.

	    
%%----------------------------------------------------------------------
%% Convert a 'MegacoMessage' record into a binary
%% Return {ok, Binary} | {error, Reason}
%%----------------------------------------------------------------------

encode_message([native], MegaMsg, AsnMod, _TransMod, binary) 
  when record(MegaMsg, 'MegacoMessage') ->
    asn1rt:encode(AsnMod, 'MegacoMessage', MegaMsg);
encode_message(EC, MegaMsg, AsnMod, TransMod, binary) 
  when list(EC), record(MegaMsg, 'MegacoMessage') ->
    case (catch TransMod:tr_message(MegaMsg, encode, EC)) of
	{'EXIT', Reason} ->
	    {error, Reason};
	MegaMsg2 ->
	    asn1rt:encode(AsnMod, 'MegacoMessage', MegaMsg2)
    end;
encode_message(EC, MegaMsg, AsnMod, TransMod, io_list) ->
    case encode_message(EC, MegaMsg, AsnMod, TransMod, binary) of
	{ok, Bin} when binary(Bin) ->
	    {ok, Bin};
	{ok, DeepIoList} ->
	    Bin = erlang:list_to_binary(DeepIoList),
	    {ok, Bin};
	{error, Reason} ->
	    {error, Reason} 
    end;
encode_message(EC, MegaMsg, _AsnMod, _TransMod, _Type)
  when record(MegaMsg, 'MegacoMessage')  ->
    {error, {bad_encoding_config, EC}};
encode_message(_EC, MegaMsg, _AsnMod, _TransMod, _Type) ->
    {error, {no_megaco_message, MegaMsg}}.


%%----------------------------------------------------------------------
%% Convert a transaction (or transactions in the case of ack) record(s) 
%% into a binary
%% Return {ok, Binary} | {error, Reason}
%%----------------------------------------------------------------------

%% Should handle encoding of all types of transactions:
%% TransactionAck, TransactionPending, TransactionRequest
%% and TransactionReply
encode_transaction(EC, [T|_] = Ts, AsnMod, TransMod, Type) 
  when record(T, 'TransactionAck') ->
    Tag = transactionResponseAck,
    do_encode_transaction(EC, Tag, Ts, AsnMod, TransMod, Type);
encode_transaction(EC, T, AsnMod, TransMod, Type) 
  when record(T, 'TransactionAck') ->
    Tag = transactionResponseAck,
    do_encode_transaction(EC, Tag, [T], AsnMod, TransMod, Type);
encode_transaction(EC, T, AsnMod, TransMod, Type) 
  when record(T, 'TransactionPending') ->
    Tag = transactionPending,
    do_encode_transaction(EC, Tag, T, AsnMod, TransMod, Type);
encode_transaction(EC, T, AsnMod, TransMod, Type) 
  when record(T, 'TransactionRequest') ->
    Tag = transactionRequest,
    do_encode_transaction(EC, Tag, T, AsnMod, TransMod, Type);
encode_transaction(EC, T, AsnMod, TransMod, Type) 
  when record(T, 'TransactionReply') ->
    Tag = transactionReply,
    do_encode_transaction(EC, Tag, T, AsnMod, TransMod, Type);
encode_transaction(_EC, T, _AsnMod, _TransMod, _Type) ->
    {error, {no_megaco_transaction, T}}.

do_encode_transaction([native], _Tag, _T, _AsnMod, _TransMod, binary) ->
    %% asn1rt:encode(AsnMod, element(1, T), T);
    {error, not_implemented};
do_encode_transaction(EC, _Tag, _T, _AsnMod, _TransMod, binary) 
  when list(EC) ->
    %% T2 = TransMod:tr_transaction({Tag,T}, encode, EC),
    %% asn1rt:encode(AsnMod, element(1, T), T2);
    {error, not_implemented};
do_encode_transaction(EC, Tag, T, AsnMod, TransMod, io_list) ->
    case do_encode_transaction(EC, Tag, T, AsnMod, TransMod, binary) of
	{ok, Bin} when binary(Bin) ->
	    {ok, Bin};
	{ok, DeepIoList} ->
	    Bin = erlang:list_to_binary(DeepIoList),
	    {ok, Bin};
	{error, Reason} ->
	    {error, Reason} 
    end;
do_encode_transaction(EC, _Tag, _T, _AsnMod, _TransMod, _Type) ->
    {error, {bad_encoding_config, EC}}.


%%----------------------------------------------------------------------
%% Convert a list of ActionRequest record's into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_requests([native], _ARs, _AsnMod, _TransMod, binary) ->
    %% asn1rt:encode(AsnMod, element(1, T), T);
    {error, not_implemented};
encode_action_requests(_EC, _ARs0, _AsnMod, _TransMod, binary) ->
    {error, not_implemented};
encode_action_requests(EC, ARs, AsnMod, TransMod, io_list) ->
    case encode_action_requests(EC, ARs, AsnMod, TransMod, binary) of
	{ok, Bin} when binary(Bin) ->
	    {ok, Bin};
	{ok, DeepIoList} ->
	    Bin = erlang:list_to_binary(DeepIoList),
	    {ok, Bin};
	{error, Reason} ->
	    {error, Reason} 
    end;
encode_action_requests(EC, _ARs, _AsnMod, _TransMod, _Type) ->
    {error, {bad_encoding_config, EC}}.


%%----------------------------------------------------------------------
%% Convert a ActionRequest record into a binary
%% Return {ok, DeepIoList} | {error, Reason}
%%----------------------------------------------------------------------
encode_action_request([native], _ARs, _AsnMod, _TransMod, binary) ->
    %% asn1rt:encode(AsnMod, element(1, T), T);
    {error, not_implemented};
encode_action_request(_EC, _ARs0, _AsnMod, _TransMod, binary) ->
    {error, not_implemented};
encode_action_request(EC, ARs, AsnMod, TransMod, io_list) ->
    case encode_action_request(EC, ARs, AsnMod, TransMod, binary) of
	{ok, Bin} when binary(Bin) ->
	    {ok, Bin};
	{ok, DeepIoList} ->
	    Bin = erlang:list_to_binary(DeepIoList),
	    {ok, Bin};
	{error, Reason} ->
	    {error, Reason} 
    end;
encode_action_request(EC, _ARs, _AsnMod, _TransMod, _Type) ->
    {error, {bad_encoding_config, EC}}.


%%----------------------------------------------------------------------
%% Convert a binary into a 'MegacoMessage' record
%% Return {ok, MegacoMessageRecord} | {error, Reason}
%%----------------------------------------------------------------------

decode_message_dynamic(EC, Bin, 
		       [{AsnModV1, TransModV1}, 
			{AsnModV2, TransModV2}, 
			{AsnModV3, TransModV3}], Form)
  when list(EC), binary(Bin) ->
    case AsnModV1:decode_message_version(Bin) of
	{ok, PartialMsg} ->
	    V = (PartialMsg#'MegacoMessage'.mess)#'Message'.version,
	    case V of
		1 -> 
		    decode_message(EC, Bin, AsnModV1, TransModV1, Form);
		2 ->
		    decode_message(EC, Bin, AsnModV2, TransModV2, Form);
		3 ->
		    decode_message(EC, Bin, AsnModV3, TransModV3, Form)
	    end;
	{error, Reason} ->
	    {error, Reason}
    end;
decode_message_dynamic(EC, Bin, _Mods, _Type) 
  when binary(Bin) ->
    {error, {bad_encoding_config, EC}};
decode_message_dynamic(_EC, _BadBin, _Mods, _Type) ->
    {error, no_binary}.


decode_message(EC, Bin, AsnMod, TransMod, binary) ->
    case asn1rt:decode(AsnMod, 'MegacoMessage', Bin) of
	{ok, MegaMsg} ->
	    case EC of
		[native] ->
		    {ok, MegaMsg};
		_ ->		
		    {ok, TransMod:tr_message(MegaMsg, decode, EC)}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end;
decode_message(EC, Bin, AsnMod, TransMod, io_list) ->
    ShallowIoList = erlang:binary_to_list(Bin),
    case asn1rt:decode(AsnMod, 'MegacoMessage', ShallowIoList) of
	{ok, MegaMsg} ->
	    case EC of
		[native] ->
		    {ok, MegaMsg};
		_ ->
		    {ok, TransMod:tr_message(MegaMsg, decode, EC)}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.


%%----------------------------------------------------------------------
%% Convert a binary into a partial 'MegacoMessage' record
%% I.e. only version and Mid is fully decoded.
%% Return {ok, MegacoMessageRecord} | {error, Reason}
%%----------------------------------------------------------------------

decode_mini_message(_, Bin, Mod, _) ->
    case (catch Mod:decode_message_mId(Bin)) of
	{ok, #'MegacoMessage'{mess = Mess} = MegaMsg} ->
	    Mess2 = Mess#'Message'{messageBody = undefined},
	    {ok, MegaMsg#'MegacoMessage'{mess = Mess2}};
	Error ->
	    Error
    end.


decode_mini_message_dynamic(EC, Bin, [Mod1, Mod2, Mod3], Form) ->
    case Mod1:decode_message_version(Bin) of
	{ok, PartialMsg} ->
	    V = (PartialMsg#'MegacoMessage'.mess)#'Message'.version,
	    case V of
		1 -> 
		    decode_mini_message(EC, Bin, Mod1, Form);
		2 ->
		    decode_mini_message(EC, Bin, Mod2, Form);
		3 ->
		    decode_mini_message(EC, Bin, Mod3, Form)
	    end;
	Error ->
	    Error
    end.

