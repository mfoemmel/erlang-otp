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
%% Purpose: Test library module for Megaco/H.248 encode/decode
%%----------------------------------------------------------------------

-module(megaco_codec_test_lib).

%% ----

-include_lib("megaco/include/megaco.hrl").
-include_lib("megaco/include/megaco_message_v1.hrl").
-include("megaco_test_lib.hrl").

%% ----

-export([
	 display_text_messages/2,
	 test_msgs/6,

	 plain_encode_decode/5,
	 trans_first_encode_decode/5,
	 actions_first_encode_decode/5,
	 action_first_encode_decode/5,
	
	 encode_message/4,
	 decode_message/5
	]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

display_text_messages(_, []) ->
    ok;
display_text_messages(V, [{Name, Msg, _Check, _ED, Conf}|Msgs]) ->
    display_text_message(Name, Msg, V),
    display_text_messages(V, Msgs).


display_text_message(Name, Msg, V) ->
    io:format("~n(Erlang) message ~p:~n~p~n", [Name, Msg]),
    case (catch megaco_pretty_text_encoder:encode_message([],V,Msg)) of
	{'EXIT', Reason} ->
	    io:format("~nPretty encoded: failed~n", []);
	{ok, Pretty} ->
	    io:format("~nPretty encoded:~n~s~n", [binary_to_list(Pretty)])
    end,
    case (catch megaco_compact_text_encoder:encode_message([],V,Msg)) of
	{'EXIT', _Reason} ->
	    io:format("~nCompact encoded: failed~n", []);
	{ok, Compact} ->
	    io:format("~nCompact encoded:~n~s~n", [binary_to_list(Compact)])
    end.


test_msgs(Codec, DynamicDecode, Ver, EC, Check, Msgs) 
  when function(Check), list(Msgs) ->
    io:format("~n", []),
    test_msgs(Codec, DynamicDecode, Ver, EC, Check, Msgs, []).

test_msgs(_Codec, _DD, _Ver, _EC, _Check, [], []) ->
    ok;
test_msgs(_Codec, _DD, _Ver, _EC, _Check, [], Errs) ->
    ?ERROR(lists:reverse(Errs));
test_msgs(Codec, DD, Ver, EC, Check, 
	  [{Name, Msg, ED, Conf}|Msgs], Acc) ->
    Dbg = test_msgs_debug(Conf),
    put(dbg, Dbg),
    io:format("~-16w ", [Name]),
    case (catch encode_decode(ED, Check, Codec, DD, Ver, EC, Msg)) of
	ok ->
	    io:format("ok~n", []),
	    erase(dbg),
	    test_msgs(Codec, DD, Ver, EC, Check, Msgs, Acc);
	Error ->
	    io:format("error~n", []),
	    erase(dbg),
	    test_msgs(Codec, DD, Ver, EC, Check, Msgs, [{Name, Error}|Acc])
    end.

test_msgs_debug(Conf) ->
    case lists:keysearch(dbg, 1, Conf) of
	{value, {dbg, true}} ->
	    true;
	_ ->
	    false
    end.
    
encode_decode(Func, Check, Codec, DynamicDecode, Ver, EC, Msg1) 
  when function(Func) ->
    case (catch Func(Codec, DynamicDecode, Ver, EC, Msg1)) of
	{ok, Msg1} ->
	    ok;
	{ok, Msg2} ->
	    case (catch Check(Msg1, Msg2)) of
		{equal, _} ->
		    ok;
		{error, Reason} ->
		    {error, {Reason, Msg1, Msg2}};
		{wrong_type, What} ->
		    {error, {wrong_type, What, Msg1, Msg2}};
		{not_equal, What} ->
		    {error, {not_equal, What, Msg1, Msg2}};
		Else ->
		    {error, {invalid_check_result, Else}}
	    end;
	Else ->
	    Else
    end.


%% *** plain_encode_decode ***

plain_encode_decode(Codec, DynamicDecode, Ver, EC, M1) ->
    case (catch encode_message(Codec, Ver, EC, M1)) of
	{ok, Bin} ->
	    decode_message(Codec, DynamicDecode, Ver, EC, Bin);
	Error ->
	    Error 
    end.


%% *** trans_first_encode_decode ***

trans_first_encode_decode(Codec, DynamicDecode, Ver, EC, M1) ->
    case (catch trans_first_encode_message(Codec, Ver, EC, M1)) of
	{ok, Bin} ->
	    decode_message(Codec, DynamicDecode, Ver, EC, Bin);
	Error ->
	    Error 
    end.

trans_first_encode_message(Codec, Ver, EC, M1) ->
    Mess1 = M1#'MegacoMessage'.mess,
    {transactions, Trans1} = Mess1#'Message'.messageBody,
    Trans2 = encode_transactions(Codec, Ver, EC, Trans1),
    Mess2  = Mess1#'Message'{messageBody = {transactions, Trans2}},
    M2     = M1#'MegacoMessage'{mess = Mess2},
    encode_message(Codec, Ver, EC, M2).

encode_transactions(Codec, Ver, EC, Trans) when list(Trans) ->
    [encode_transaction(Codec, Ver, EC, T) || T <- Trans].

encode_transaction(Codec, Ver, EC, T) ->
    case (catch Codec:encode_transaction(EC, Ver, T)) of
	{ok, EncodecTransactions} ->
	    EncodecTransactions;
	Error ->
	    throw({error, {transaction_encode_failed, Error, T}})
    end.


%% *** actions_first_encode_decode ***

actions_first_encode_decode(Codec, DynamicDecode, Ver, EC, M1) ->
    case (catch actions_first_encode_message(Codec, Ver, EC, M1)) of
	{ok, Bin} ->
	    decode_message(Codec, DynamicDecode, Ver, EC, Bin);
	Error ->
	    Error 
    end.

actions_first_encode_message(Codec, Ver, EC, M1) ->
    Mess1 = M1#'MegacoMessage'.mess,
    {transactions, Trans1} = Mess1#'Message'.messageBody,
    Trans2 = encode_actions(Codec, Ver, EC, Trans1),
    Mess2  = Mess1#'Message'{messageBody = {transactions, Trans2}},
    M2     = M1#'MegacoMessage'{mess = Mess2},
    encode_message(Codec, Ver, EC, M2).

encode_actions(Codec, Ver, EC, Trans) when list(Trans) ->
    [encode_actions1(Codec, Ver, EC, T) || T <- Trans].

encode_actions1(Codec, Ver, EC, {transactionRequest, TR1}) ->
    #'TransactionRequest'{actions = ARs} = TR1,
    case (catch encode_action_requests(Codec, Ver, EC, ARs)) of
	{ok, EncodedARs} ->
	    TR2 = TR1#'TransactionRequest'{actions = EncodedARs},
	    {transactionRequest, TR2};
	Error ->
	    throw({error, {actions_encode_failed, Error, TR1}})
    end.

encode_action_requests(Codec, Ver, EC, ARs) ->
    Codec:encode_action_requests(EC, Ver, ARs).


%% *** action_first_encode_decode ***

action_first_encode_decode(Codec, DynamicDecode, Ver, EC, M1) ->
    case (catch action_first_encode_message(Codec, Ver, EC, M1)) of
	{ok, Bin} ->
	    decode_message(Codec, DynamicDecode, Ver, EC, Bin);
	Error ->
	    Error 
    end.

action_first_encode_message(Codec, Ver, EC, M1) ->
    Mess1 = M1#'MegacoMessage'.mess,
    {transactions, Trans1} = Mess1#'Message'.messageBody,
    Trans2 = encode_action(Codec, Ver, EC, Trans1),
    Mess2  = Mess1#'Message'{messageBody = {transactions, Trans2}},
    M2     = M1#'MegacoMessage'{mess = Mess2},
    encode_message(Codec, Ver, EC, M2).

encode_action(Codec, Ver, EC, Trans) when list(Trans) ->
    [encode_action1(Codec, Ver, EC, T) || T <- Trans].

encode_action1(Codec, Ver, EC, {transactionRequest, TR1}) ->
    #'TransactionRequest'{actions = ARs1} = TR1,
    ARs2 = [encode_action_request(Codec, Ver, EC, AR) || AR <- ARs1],
    TR2  = TR1#'TransactionRequest'{actions = ARs2},
    {transactionRequest, TR2}.

encode_action_request(Codec, Ver, EC, AR) ->
    case (catch Codec:encode_action_request(EC, Ver, AR)) of
	{ok, Bin} ->
	    Bin;
	Error ->
	    throw({error, {encode_action_request_failed, Error, AR}})
    end.


encode_message(Codec, Ver, EC, M) ->
    case (catch Codec:encode_message(EC, Ver, M)) of
	{ok, Bin} ->
	    {ok, Bin};
	Error ->
	    throw({error, {message_encode_failed, Error, M}})
    end.

decode_message(Codec, true, _Ver, EC, M) ->
    Codec:decode_message(EC, dynamic, M);
decode_message(Codec, _, Ver, EC, M) ->
    Codec:decode_message(EC, Ver, M).


