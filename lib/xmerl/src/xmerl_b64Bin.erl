-module(xmerl_b64Bin).
-export([parse/1, parse_and_scan/1, format_error/1]).

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



-file("./xmerl_b64Bin.erl", 97).

yeccpars2(0, b04, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, b16x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, b64x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_0_(__Stack),
 yeccpars2(2, __Cat, [0 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(1, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_1_(__Stack),
 yeccpars2(yeccgoto(base64Binary, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(2, '$end', _, __Stack, _, _, _) ->
 {ok, hd(__Stack)};
yeccpars2(2, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(3, b04, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, b16x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, b64x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(4, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_4_(__Stack),
 yeccpars2(yeccgoto(b64, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(5, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_5_(__Stack),
 yeccpars2(yeccgoto(b64, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(6, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_6_(__Stack),
 yeccpars2(yeccgoto(b64, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(7, b04, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, b16x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, b64x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(8, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [8 | __Ss], [__T | __Stack]);
yeccpars2(8, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_8_(__Stack),
 yeccpars2(yeccgoto(b64, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(9, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(10, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_10_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(base64Binary2, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(11, b04, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, b16x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, b64x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(12, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(13, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_13_='(__Stack),
 yeccpars2(yeccgoto(b16, hd(__Ss)), '=', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(13, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_13_(__Stack),
 yeccpars2(yeccgoto(b64, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(14, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_14_='(__Stack),
 yeccpars2(yeccgoto(b16, hd(__Ss)), '=', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(14, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_14_(__Stack),
 yeccpars2(yeccgoto(b64, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(15, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_15_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(base64Binary2, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(16, b04, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, b16x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, b64x, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_16_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(base64Binary2, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(17, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_17_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(base64Binary2, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(__Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.1",{missing_state_in_action_table, __Other}}).

yeccgoto(b16, 7) ->
 12;
yeccgoto(b64, 0) ->
 3;
yeccgoto(b64, 3) ->
 7;
yeccgoto(b64, 7) ->
 11;
yeccgoto(b64, 11) ->
 16;
yeccgoto(b64, 16) ->
 3;
yeccgoto(base64Binary, 0) ->
 2;
yeccgoto(base64Binary2, 0) ->
 1;
yeccgoto(base64Binary2, 16) ->
 17;
yeccgoto(__Symbol, __State) ->
 erlang:error({yecc_bug,"1.1",{__Symbol, __State, missing_in_goto_table}}).

-compile({inline,{yeccpars2_0_,1}}).
-file("xmerl_b64Bin.yrl", 0).
yeccpars2_0_(__Stack) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_1_,1}}).
-file("xmerl_b64Bin.yrl", 0).
yeccpars2_1_([__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_4_,1}}).
-file("xmerl_b64Bin.yrl", 0).
yeccpars2_4_([__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_5_,1}}).
-file("xmerl_b64Bin.yrl", 0).
yeccpars2_5_([__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_6_,1}}).
-file("xmerl_b64Bin.yrl", 0).
yeccpars2_6_([__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_8_,1}}).
-file("xmerl_b64Bin.yrl", 0).
yeccpars2_8_([__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_10_,1}}).
-file("xmerl_b64Bin.yrl", 0).
yeccpars2_10_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{'yeccpars2_13_=',1}}).
-file("xmerl_b64Bin.yrl", 0).
'yeccpars2_13_='([__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_13_,1}}).
-file("xmerl_b64Bin.yrl", 0).
yeccpars2_13_([__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{'yeccpars2_14_=',1}}).
-file("xmerl_b64Bin.yrl", 0).
'yeccpars2_14_='([__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_14_,1}}).
-file("xmerl_b64Bin.yrl", 0).
yeccpars2_14_([__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_15_,1}}).
-file("xmerl_b64Bin.yrl", 0).
yeccpars2_15_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_16_,1}}).
-file("xmerl_b64Bin.yrl", 0).
yeccpars2_16_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_17_,1}}).
-file("xmerl_b64Bin.yrl", 0).
yeccpars2_17_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].


