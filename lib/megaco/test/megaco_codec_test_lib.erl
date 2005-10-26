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
	 display_text_messages/2, display_text_messages/3,
	 generate_text_messages/4,
	 test_msgs/6,

	 plain_decode_encode/5,
	 plain_encode_decode/5,
	 trans_first_encode_decode/5,
	 actions_first_encode_decode/5,
	 action_first_encode_decode/5,
	
	 encode_message/4,
	 decode_message/5
	]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

display_text_messages(V, Msgs) ->
    display_text_messages(V, [], Msgs).

display_text_messages(_, _, []) ->
    ok;
display_text_messages(V, EC, [{Name, Msg, _ED, _Conf}|Msgs]) ->
    (catch display_text_message(Name, EC, Msg, V)),
    display_text_messages(V, EC, Msgs).


display_text_message(Name, EC, Msg, V) when tuple(Msg) ->
    io:format("~n(Erlang) message ~p:~n~p~n", [Name, Msg]),
    case (catch megaco_pretty_text_encoder:encode_message(EC,V,Msg)) of
	{'EXIT', _R} ->
	    io:format("~nPretty encoded: failed (exit)~n", []);
	{error, {{deprecated, PWhat}, _}} ->
 	    io:format("~nPretty encoded: deprecated~n~p~n", [PWhat]),
	    throw(continue);
	{error, PReason} ->
 	    io:format("~nPretty encoded: failed (error)~n~p~n", [PReason]),
	    throw(continue);
	{ok, Pretty} ->
	    io:format("~nPretty encoded:~n~s~n", [binary_to_list(Pretty)])
    end,
    case (catch megaco_compact_text_encoder:encode_message(EC,V,Msg)) of
 	{'EXIT', _} ->
 	    io:format("~nCompact encoded: failed~n", []);
 	{error, {{deprecated, CWhat}, _}} ->
  	    io:format("~nPretty encoded: deprecated~n~p~n", [CWhat]);
 	{ok, Compact} ->
 	    io:format("~nCompact encoded:~n~s~n", [binary_to_list(Compact)])
    end;
display_text_message(_, _, _, _) ->
    skipping.

generate_text_messages(DirName, V, EC, Msgs) when is_atom(DirName) ->
    generate_text_messages(atom_to_list(DirName), V, EC, Msgs);
generate_text_messages(DirName, V, EC, Msgs) when is_list(DirName) ->
    DirPath = filename:join(["/tmp", DirName]),
    case file:make_dir(DirPath) of
	ok ->
	    generate_text_messages2(DirPath, V, EC, Msgs);
	{error, eexist} ->
	    generate_text_messages2(DirPath, V, EC, Msgs);
	{error, Reason} ->
	    io:format("Failed creating directory ~s: ~p~n", [DirPath, Reason]),
	    ok
    end.

generate_text_messages2(_, _, _, []) ->
    ok;
generate_text_messages2(Dir, V, EC, [{Name, Msg, _ED, _Conf}|Msgs]) ->
    (catch generate_text_message(Dir, Name, EC, Msg, V)),
    generate_text_messages2(Dir, V, EC, Msgs).

generate_text_message(Dir, Name, EC, Msg, V) ->
    io:format("~p: ", [Name]),
    case (catch megaco_pretty_text_encoder:encode_message(EC,V,Msg)) of
	{'EXIT', EReason} ->
	    io:format("failed encoding [exit]: ~n~p~n", [EReason]),
	    throw(continue);
	{error, {{deprecated, PWhat}, _}} ->
 	    io:format("failed encoding [deprecated]: ~n~p~n", [PWhat]),
	    throw(continue);
	{error, PReason} ->
 	    io:format("failed encoding [error]: ~n~p~n", [PReason]),
	    throw(continue);
	{ok, Pretty} ->
	    io:format("encoded", []),
	    FName = filename:flatten([Name, ".txt"]),
	    Filename = filename:join([Dir, FName]),
	    case (catch file:open(Filename, [write])) of
		{ok, Fd} ->
		    io:format(Fd, "~s~n", [binary_to_list(Pretty)]),
		    io:format(" - written to disk~n", []),
		    (catch file:close(Fd)),
		    ok;
		{error, OReason} ->
		    io:format(" - failed writing to disk: "
			      "~n~p~n~s~n", 
			      [OReason, binary_to_list(Pretty)]),
		    throw(continue)
	    end
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
	  [{Name, {error, Error}, _ED, _Conf}|Msgs], Acc) ->
    io:format("error~n", []),
    test_msgs(Codec, DD, Ver, EC, Check, Msgs, [{Name, Error}|Acc]);
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
    d("encode_decode -> entry with"
      "~n   Func:          ~p"
      "~n   Check:         ~p"
      "~n   Codec:         ~p"
      "~n   DynamicDecode: ~p"
      "~n   Ver:           ~p"
      "~n   EC:            ~p", 
      [Func, Check, Codec, DynamicDecode, Ver, EC]),
    case (catch Func(Codec, DynamicDecode, Ver, EC, Msg1)) of
	{ok, Msg1} ->
	    d("encode_decode -> expected result"),
	    ok;
	{ok, Msg2} ->
	    d("encode_decode -> unexpected result - check"),
	    case (catch Check(Msg1, Msg2)) of
		ok ->
		    d("encode_decode -> check - ok"),
		    ok;
		{error, Reason} ->
		    d("encode_decode -> check - error: "
		      "~n   Reason: ~p", [Reason]),
		    {error, {Reason, Msg1, Msg2}};
		Else ->
		    d("encode_decode -> check - failed: "
		      "~n   Else: ~p", [Else]),
		    {error, {invalid_check_result, Else}}
	    end;
	Else ->
	    d("encode_decode -> failed: "
	      "~n   Else: ~p", [Else]),
	    Else
    end.


%% *** plain_encode_decode ***

plain_encode_decode(Codec, DynamicDecode, Ver, EC, M1) ->
    d("plain_encode_decode -> entry with"
      "~n   Codec:         ~p"
      "~n   DynamicDecode: ~p"
      "~n   Ver:           ~p"
      "~n   EC:            ~p", [Codec, DynamicDecode, Ver, EC]),
    case (catch encode_message(Codec, Ver, EC, M1)) of
	{ok, Bin} ->
	    d("plain_encode_decode -> encode - ok"),
	    decode_message(Codec, DynamicDecode, Ver, EC, Bin);
	Error ->
	    d("plain_encode_decode -> encode - failed: "
	      "~n   Error: ~p", [Error]),
	    Error 
    end.


%% *** plain_decode_encode ***

plain_decode_encode(Codec, DynamicDecode, Ver, EC, M) when list(M) ->
    Bin = list_to_binary(M),
    plain_decode_encode(Codec, DynamicDecode, Ver, EC, Bin);
plain_decode_encode(Codec, DynamicDecode, Ver, EC, B) when binary(B) ->
    case (catch decode_message(Codec, DynamicDecode, Ver, EC, B)) of
	{ok, M} ->
	    encode_message(Codec, Ver, EC, M);
	Error ->
	    Error 
    end.


%% *** trans_first_encode_decode ***

trans_first_encode_decode(Codec, DynamicDecode, Ver, EC, M1) ->
    d("trans_first_encode_decode -> entry"),
    case (catch trans_first_encode_message(Codec, Ver, EC, M1)) of
	{ok, Bin} ->
	    decode_message(Codec, DynamicDecode, Ver, EC, Bin);
	Error ->
	    Error 
    end.

trans_first_encode_message(Codec, Ver, EC, M1) ->
    d("trans_first_encode_message -> entry"),
    Mess1 = M1#'MegacoMessage'.mess,
    {transactions, Trans1} = Mess1#'Message'.messageBody,
    Trans2 = encode_transactions(Codec, Ver, EC, Trans1),
    Mess2  = Mess1#'Message'{messageBody = {transactions, Trans2}},
    M2     = M1#'MegacoMessage'{mess = Mess2},
    encode_message(Codec, Ver, EC, M2).

encode_transactions(Codec, Ver, EC, Trans) when list(Trans) ->
    d("encode_transactions -> entry"),
    [encode_transaction(Codec, Ver, EC, T) || T <- Trans].

encode_transaction(Codec, Ver, EC, T) ->
    d("encode_transaction -> entry"),
    case (catch Codec:encode_transaction(EC, Ver, T)) of
	{ok, EncodecTransactions} ->
	    EncodecTransactions;
	Error ->
	    throw({error, {transaction_encode_failed, Error, T}})
    end.


%% *** actions_first_encode_decode ***

actions_first_encode_decode(Codec, DynamicDecode, Ver, EC, M1) ->
    d("actions_first_encode_decode -> entry"),
    case (catch actions_first_encode_message(Codec, Ver, EC, M1)) of
	{ok, Bin} ->
	    decode_message(Codec, DynamicDecode, Ver, EC, Bin);
	Error ->
	    Error 
    end.

actions_first_encode_message(Codec, Ver, EC, M1) ->
    d("actions_first_encode_message -> entry"),
    Mess1 = M1#'MegacoMessage'.mess,
    {transactions, Trans1} = Mess1#'Message'.messageBody,
    Trans2 = encode_actions(Codec, Ver, EC, Trans1),
    Mess2  = Mess1#'Message'{messageBody = {transactions, Trans2}},
    M2     = M1#'MegacoMessage'{mess = Mess2},
    encode_message(Codec, Ver, EC, M2).

encode_actions(Codec, Ver, EC, Trans) when list(Trans) ->
    d("encode_actions -> entry"),
    [encode_actions1(Codec, Ver, EC, T) || T <- Trans].

encode_actions1(Codec, Ver, EC, {transactionRequest, TR1}) ->
    d("encode_actions1 -> entry"),
    #'TransactionRequest'{actions = ARs} = TR1,
    case (catch encode_action_requests(Codec, Ver, EC, ARs)) of
	{ok, EncodedARs} ->
	    TR2 = TR1#'TransactionRequest'{actions = EncodedARs},
	    {transactionRequest, TR2};
	Error ->
	    throw({error, {actions_encode_failed, Error, TR1}})
    end.

encode_action_requests(Codec, Ver, EC, ARs) ->
    d("encode_action_requests -> entry"),
    Codec:encode_action_requests(EC, Ver, ARs).


%% *** action_first_encode_decode ***

action_first_encode_decode(Codec, DynamicDecode, Ver, EC, M1) ->
    d("action_first_encode_decode -> entry"),
    case (catch action_first_encode_message(Codec, Ver, EC, M1)) of
	{ok, Bin} ->
	    decode_message(Codec, DynamicDecode, Ver, EC, Bin);
	Error ->
	    Error 
    end.

action_first_encode_message(Codec, Ver, EC, M1) ->
    d("action_first_encode_message -> entry"),
    Mess1 = M1#'MegacoMessage'.mess,
    {transactions, Trans1} = Mess1#'Message'.messageBody,
    Trans2 = encode_action(Codec, Ver, EC, Trans1),
    Mess2  = Mess1#'Message'{messageBody = {transactions, Trans2}},
    M2     = M1#'MegacoMessage'{mess = Mess2},
    encode_message(Codec, Ver, EC, M2).

encode_action(Codec, Ver, EC, Trans) when list(Trans) ->
    d("encode_action -> entry"),
    [encode_action1(Codec, Ver, EC, T) || T <- Trans].

encode_action1(Codec, Ver, EC, {transactionRequest, TR1}) ->
    d("encode_action1 -> entry"),
    #'TransactionRequest'{actions = ARs1} = TR1,
    ARs2 = [encode_action_request(Codec, Ver, EC, AR) || AR <- ARs1],
    TR2  = TR1#'TransactionRequest'{actions = ARs2},
    {transactionRequest, TR2}.

encode_action_request(Codec, Ver, EC, AR) ->
    d("encode_action_request -> entry"),
    case (catch Codec:encode_action_request(EC, Ver, AR)) of
	{ok, Bin} ->
	    Bin;
	Error ->
	    throw({error, {encode_action_request_failed, Error, AR}})
    end.


encode_message(Codec, Ver, EC, M) ->
    d("encode_message -> entry with"
      "~n   Codec: ~p"
      "~n   Ver:   ~p"
      "~n   EC:    ~p"
      "~n   M:     ~p", [Codec, Ver, EC, M]),
    case (catch Codec:encode_message(EC, Ver, M)) of
	{ok, Bin} ->
	    d("encode_message -> encode - ok: "
	      "~n~s", [binary_to_list(Bin)]),
	    {ok, Bin};
	Error ->
	    d("encode_message -> encode - failed"),
	    throw({error, {message_encode_failed, Error, M}})
    end.

decode_message(Codec, true, _Ver, EC, M) ->
    d("decode_message -> entry - when using dynamic"),
    Codec:decode_message(EC, dynamic, M);
decode_message(Codec, _, Ver, EC, M) ->
    d("decode_message -> entry with"
      "~n   Ver: ~p", [Ver]),
    Codec:decode_message(EC, Ver, M).


%% ----------------------------------------------------------------------

d(F) ->
    d(F, []).

d(F, A) ->
    d(get(dbg), F, A).

d(true, F, A) ->
    io:format("DBG:~w:" ++ F ++ "~n", [?MODULE|A]);
d(_, _, _) ->
    ok.

