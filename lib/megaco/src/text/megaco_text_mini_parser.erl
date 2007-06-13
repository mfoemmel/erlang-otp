-module(megaco_text_mini_parser).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("megaco_text_mini_parser.yrl", 159).

%% The following directive is needed for (significantly) faster compilation
%% of the generated .erl file by the HiPE compiler.  Please do not remove.
-compile([{hipe,[{regalloc,linear_scan}]}]).

-include("megaco_text_mini_parser.hrl").



-file("/ldisk/daily_build/otp_prebuild_r11b.2007-06-11_19/otp_src_R11B-5/bootstrap/lib/parsetools/include/yeccpre.hrl", 0).
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
%%     $Id $
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

parse(Tokens) ->
    yeccpars0(Tokens, false).

parse_and_scan({F, A}) -> % Fun or {M, F}
    yeccpars0([], {F, A});
parse_and_scan({M, F, A}) ->
    yeccpars0([], {{M, F}, A}).

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
	true ->
	    Message;
	_ ->
	    io_lib:write(Message)
    end.

% To be used in grammar files to throw an error message to the parser
% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function,{return_error,2}}).
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

yeccpars0(Tokens, MFA) ->
    try yeccpars1(Tokens, MFA, 0, [], [])
    catch 
        throw: {error, {_Line, ?MODULE, _M}} = Error -> 
                   Error % probably from return_error/1
    end.

% Don't change yeccpars1/6 too much, it is called recursively by yeccpars2/8!
yeccpars1([Token | Tokens], Tokenizer, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens,
	      Tokenizer);
yeccpars1([], {F, A}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, _Endline} ->
	    yeccpars1(Tokens, {F, A}, State, States, Vstack);
        {eof, _Endline} ->
            yeccpars1([], false, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], false, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, {'$end', 999999}, [], false).

% For internal use only.
yeccerror(Token) ->
    {error,
     {element(2, Token), ?MODULE,
      ["syntax error before: ", yecctoken2string(Token)]}}.

yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format('~s', [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:format('~w', [A]);
yecctoken2string({_Cat, _, Val}) -> io_lib:format('~w', [Val]);
yecctoken2string({'dot', _}) -> io_lib:format('~w', ['.']);
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:format('~w', [Other]);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("./megaco_text_mini_parser.erl", 106).

yeccpars2(0, 'SEP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_0_(__Stack),
 yeccpars2(1, __Cat, [0 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(1, 'AuthToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_1_(__Stack),
 yeccpars2(4, __Cat, [1 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(2, '$end', _, __Stack, _, _, _) ->
 {ok, hd(__Stack)};
yeccpars2(2, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(3, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_3_(__Stack),
 yeccpars2(yeccgoto(optSep, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(4, 'MgcIdToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'PendingToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'ReplyToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'ResponseAckToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'SafeChars', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, 'TransToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(5, 'EQUAL', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(6, 'MgcIdToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, 'PendingToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, 'ReplyToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, 'ResponseAckToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, 'SafeChars', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, 'TransToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(7, 'COLON', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(8, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_8_(__Stack),
 yeccpars2(yeccgoto(safeToken, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(9, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_9_(__Stack),
 yeccpars2(yeccgoto(safeToken, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(10, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_10_(__Stack),
 yeccpars2(yeccgoto(safeToken, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(11, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_11_(__Stack),
 yeccpars2(yeccgoto(safeToken, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(12, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_12_(__Stack),
 yeccpars2(yeccgoto(safeToken, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(13, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_13_(__Stack),
 yeccpars2(yeccgoto(safeToken, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(14, 'MgcIdToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'PendingToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'ReplyToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'ResponseAckToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'SafeChars', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'TransToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(15, 'COLON', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(16, 'MgcIdToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, 'PendingToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, 'ReplyToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, 'ResponseAckToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, 'SafeChars', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, 'TransToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(17, 'SEP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_17_(__Stack),
 yeccpars2(18, __Cat, [17 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(18, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_18_(__Stack),
 __Nss = lists:nthtail(7, __Ss),
 yeccpars2(yeccgoto(authenticationHeader, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(19, 'LESSER', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [19 | __Ss], [__T | __Stack]);
yeccpars2(19, 'LSBRKT', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [19 | __Ss], [__T | __Stack]);
yeccpars2(19, 'SEP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [19 | __Ss], [__T | __Stack]);
yeccpars2(19, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_19_(__Stack),
 yeccpars2(22, __Cat, [19 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(20, endOfMessage, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(21, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_21_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(megacoMessage, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(22, 'MgcIdToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [22 | __Ss], [__T | __Stack]);
yeccpars2(22, 'MtpAddressToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [22 | __Ss], [__T | __Stack]);
yeccpars2(22, 'PendingToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [22 | __Ss], [__T | __Stack]);
yeccpars2(22, 'ReplyToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [22 | __Ss], [__T | __Stack]);
yeccpars2(22, 'ResponseAckToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [22 | __Ss], [__T | __Stack]);
yeccpars2(22, 'SafeChars', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [22 | __Ss], [__T | __Stack]);
yeccpars2(22, 'TransToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [22 | __Ss], [__T | __Stack]);
yeccpars2(22, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(23, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_23_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(message, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(24, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(mId, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(25, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(mId, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(26, 'MgcIdToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [26 | __Ss], [__T | __Stack]);
yeccpars2(26, 'PendingToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [26 | __Ss], [__T | __Stack]);
yeccpars2(26, 'ReplyToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [26 | __Ss], [__T | __Stack]);
yeccpars2(26, 'ResponseAckToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [26 | __Ss], [__T | __Stack]);
yeccpars2(26, 'SafeChars', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [26 | __Ss], [__T | __Stack]);
yeccpars2(26, 'TransToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [26 | __Ss], [__T | __Stack]);
yeccpars2(26, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(27, 'COLON', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [27 | __Ss], [__T | __Stack]);
yeccpars2(27, 'MgcIdToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [27 | __Ss], [__T | __Stack]);
yeccpars2(27, 'PendingToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [27 | __Ss], [__T | __Stack]);
yeccpars2(27, 'ReplyToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [27 | __Ss], [__T | __Stack]);
yeccpars2(27, 'ResponseAckToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [27 | __Ss], [__T | __Stack]);
yeccpars2(27, 'SafeChars', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [27 | __Ss], [__T | __Stack]);
yeccpars2(27, 'TransToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [27 | __Ss], [__T | __Stack]);
yeccpars2(27, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_27_(__Stack),
 yeccpars2(29, __Cat, [27 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(28, 'COLON', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, 'MgcIdToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, 'PendingToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, 'ReplyToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, 'ResponseAckToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, 'SafeChars', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, 'TransToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_28_(__Stack),
 yeccpars2(37, __Cat, [28 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(29, 'RSBRKT', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(30, 'COLON', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [30 | __Ss], [__T | __Stack]);
yeccpars2(30, 'MgcIdToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [30 | __Ss], [__T | __Stack]);
yeccpars2(30, 'PendingToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [30 | __Ss], [__T | __Stack]);
yeccpars2(30, 'ReplyToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [30 | __Ss], [__T | __Stack]);
yeccpars2(30, 'ResponseAckToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [30 | __Ss], [__T | __Stack]);
yeccpars2(30, 'SafeChars', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [30 | __Ss], [__T | __Stack]);
yeccpars2(30, 'TransToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [30 | __Ss], [__T | __Stack]);
yeccpars2(30, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_30_(__Stack),
 yeccpars2(31, __Cat, [30 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(31, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_31_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(daddr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(32, 'COLON', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_32_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(domainAddress, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(33, 'MgcIdToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'PendingToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'ReplyToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'ResponseAckToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'SafeChars', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, 'TransToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(34, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_34_(__Stack),
 yeccpars2(yeccgoto(portNumber, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(35, 'SEP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_35_(__Stack),
 yeccpars2(36, __Cat, [35 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(36, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_36_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(domainAddress, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(37, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_37_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(daddr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(38, 'GREATER', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [38 | __Ss], [__T | __Stack]);
yeccpars2(38, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(39, 'COLON', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_39_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(domainName, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(40, 'MgcIdToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, 'PendingToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, 'ReplyToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, 'ResponseAckToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, 'SafeChars', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, 'TransToken', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(41, 'SEP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_41_(__Stack),
 yeccpars2(42, __Cat, [41 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(42, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_42_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(domainName, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(43, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_43_(__Stack),
 yeccpars2(yeccgoto(pathName, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(44, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_44_(__Stack),
 yeccpars2(yeccgoto(deviceName, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(45, 'SEP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_45_(__Stack),
 yeccpars2(49, __Cat, [45 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(46, 'SEP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_46_(__Stack),
 yeccpars2(48, __Cat, [46 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(47, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_47_(__Stack),
 yeccpars2(yeccgoto(mtpAddress, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(48, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_48_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(mId, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(49, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_49_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(mId, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(__Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.1",{missing_state_in_action_table, __Other}}).

yeccgoto(authenticationHeader, 1) ->
 4;
yeccgoto(daddr, 27) ->
 29;
yeccgoto(daddr, 28) ->
 37;
yeccgoto(daddr, 30) ->
 31;
yeccgoto(deviceName, 22) ->
 46;
yeccgoto(domainAddress, 19) ->
 25;
yeccgoto(domainName, 19) ->
 24;
yeccgoto(mId, 19) ->
 23;
yeccgoto(megacoMessage, 0) ->
 2;
yeccgoto(message, 4) ->
 20;
yeccgoto(mtpAddress, 22) ->
 45;
yeccgoto(optSep, 0) ->
 1;
yeccgoto(optSep, 17) ->
 18;
yeccgoto(optSep, 19) ->
 22;
yeccgoto(optSep, 35) ->
 36;
yeccgoto(optSep, 41) ->
 42;
yeccgoto(optSep, 45) ->
 49;
yeccgoto(optSep, 46) ->
 48;
yeccgoto(pathName, 22) ->
 44;
yeccgoto(portNumber, 33) ->
 35;
yeccgoto(portNumber, 40) ->
 41;
yeccgoto(safeToken, 4) ->
 19;
yeccgoto(safeToken, 6) ->
 7;
yeccgoto(safeToken, 14) ->
 15;
yeccgoto(safeToken, 16) ->
 17;
yeccgoto(safeToken, 22) ->
 43;
yeccgoto(safeToken, 26) ->
 38;
yeccgoto(safeToken, 27) ->
 28;
yeccgoto(safeToken, 28) ->
 28;
yeccgoto(safeToken, 30) ->
 28;
yeccgoto(safeToken, 33) ->
 34;
yeccgoto(safeToken, 40) ->
 34;
yeccgoto(__Symbol, __State) ->
 erlang:error({yecc_bug,"1.1",{__Symbol, __State, missing_in_goto_table}}).

-compile({inline,{yeccpars2_0_,1}}).
-file("megaco_text_mini_parser.yrl", 113).
yeccpars2_0_(__Stack) ->
 [begin
   no_sep
  end | __Stack].

-compile({inline,{yeccpars2_1_,1}}).
-file("megaco_text_mini_parser.yrl", 118).
yeccpars2_1_(__Stack) ->
 [begin
   asn1_NOVALUE
  end | __Stack].

-compile({inline,{yeccpars2_3_,1}}).
-file("megaco_text_mini_parser.yrl", 112).
yeccpars2_3_([__1 | __Stack]) ->
 [begin
   sep
  end | __Stack].

-compile({inline,{yeccpars2_8_,1}}).
-file("megaco_text_mini_parser.yrl", 154).
yeccpars2_8_([__1 | __Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_9_,1}}).
-file("megaco_text_mini_parser.yrl", 150).
yeccpars2_9_([__1 | __Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_10_,1}}).
-file("megaco_text_mini_parser.yrl", 151).
yeccpars2_10_([__1 | __Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_11_,1}}).
-file("megaco_text_mini_parser.yrl", 152).
yeccpars2_11_([__1 | __Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_12_,1}}).
-file("megaco_text_mini_parser.yrl", 149).
yeccpars2_12_([__1 | __Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_13_,1}}).
-file("megaco_text_mini_parser.yrl", 153).
yeccpars2_13_([__1 | __Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_17_,1}}).
-file("megaco_text_mini_parser.yrl", 113).
yeccpars2_17_(__Stack) ->
 [begin
   no_sep
  end | __Stack].

-compile({inline,{yeccpars2_18_,1}}).
-file("megaco_text_mini_parser.yrl", 117).
yeccpars2_18_([__8,__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   ensure_auth_header ( __3 , __5 , __7 )
  end | __Stack].

-compile({inline,{yeccpars2_19_,1}}).
-file("megaco_text_mini_parser.yrl", 113).
yeccpars2_19_(__Stack) ->
 [begin
   no_sep
  end | __Stack].

-compile({inline,{yeccpars2_21_,1}}).
-file("megaco_text_mini_parser.yrl", 110).
yeccpars2_21_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   # 'MegacoMessage' { authHeader = __2 , mess = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_23_,1}}).
-file("megaco_text_mini_parser.yrl", 120).
yeccpars2_23_([__2,__1 | __Stack]) ->
 [begin
   ensure_message ( __1 , __2 )
  end | __Stack].

-compile({inline,{yeccpars2_27_,1}}).
-file("megaco_text_mini_parser.yrl", 139).
yeccpars2_27_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_28_,1}}).
-file("megaco_text_mini_parser.yrl", 139).
yeccpars2_28_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_30_,1}}).
-file("megaco_text_mini_parser.yrl", 139).
yeccpars2_30_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_31_,1}}).
-file("megaco_text_mini_parser.yrl", 140).
yeccpars2_31_([__2,__1 | __Stack]) ->
 [begin
   [ colon | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_32_,1}}).
-file("megaco_text_mini_parser.yrl", 137).
yeccpars2_32_([__3,__2,__1 | __Stack]) ->
 [begin
   ensure_domainAddress ( __2 , asn1_NOVALUE )
  end | __Stack].

-compile({inline,{yeccpars2_34_,1}}).
-file("megaco_text_mini_parser.yrl", 143).
yeccpars2_34_([__1 | __Stack]) ->
 [begin
   ensure_uint16 ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_35_,1}}).
-file("megaco_text_mini_parser.yrl", 113).
yeccpars2_35_(__Stack) ->
 [begin
   no_sep
  end | __Stack].

-compile({inline,{yeccpars2_36_,1}}).
-file("megaco_text_mini_parser.yrl", 135).
yeccpars2_36_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   ensure_domainAddress ( __2 , __5 )
  end | __Stack].

-compile({inline,{yeccpars2_37_,1}}).
-file("megaco_text_mini_parser.yrl", 141).
yeccpars2_37_([__2,__1 | __Stack]) ->
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_39_,1}}).
-file("megaco_text_mini_parser.yrl", 130).
yeccpars2_39_([__3,__2,__1 | __Stack]) ->
 [begin
   ensure_domainName ( __2 , asn1_NOVALUE )
  end | __Stack].

-compile({inline,{yeccpars2_41_,1}}).
-file("megaco_text_mini_parser.yrl", 113).
yeccpars2_41_(__Stack) ->
 [begin
   no_sep
  end | __Stack].

-compile({inline,{yeccpars2_42_,1}}).
-file("megaco_text_mini_parser.yrl", 128).
yeccpars2_42_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   ensure_domainName ( __2 , __5 )
  end | __Stack].

-compile({inline,{yeccpars2_43_,1}}).
-file("megaco_text_mini_parser.yrl", 147).
yeccpars2_43_([__1 | __Stack]) ->
 [begin
   ensure_pathName ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_44_,1}}).
-file("megaco_text_mini_parser.yrl", 132).
yeccpars2_44_([__1 | __Stack]) ->
 [begin
   { deviceName , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_45_,1}}).
-file("megaco_text_mini_parser.yrl", 113).
yeccpars2_45_(__Stack) ->
 [begin
   no_sep
  end | __Stack].

-compile({inline,{yeccpars2_46_,1}}).
-file("megaco_text_mini_parser.yrl", 113).
yeccpars2_46_(__Stack) ->
 [begin
   no_sep
  end | __Stack].

-compile({inline,{yeccpars2_47_,1}}).
-file("megaco_text_mini_parser.yrl", 145).
yeccpars2_47_([__1 | __Stack]) ->
 [begin
   ensure_mtpAddress ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_48_,1}}).
-file("megaco_text_mini_parser.yrl", 125).
yeccpars2_48_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_49_,1}}).
-file("megaco_text_mini_parser.yrl", 124).
yeccpars2_49_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].


-file("megaco_text_mini_parser.yrl", 168).
