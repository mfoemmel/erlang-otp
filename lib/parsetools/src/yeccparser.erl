-module(yeccparser).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("yeccgramm.yrl", 59).

-record(symbol, {line, name}).

symbol(Symbol) ->
    #symbol{line = line_of(Symbol), name = value_of(Symbol)}.

value_of(Token) ->
    element(3, Token).

line_of(Token) ->
    element(2, Token).

-file("./yeccpre.hrl", 0).
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



-file("yeccparser.erl", 110).

yeccpars2(0, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, reserved_word, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(1, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, reserved_word, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(head, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(2, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(grammar, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(3, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(4, '$end', _, __Stack, _, _, _) ->
 {ok, hd(__Stack)};
yeccpars2(4, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(5, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(grammar, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(6, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_6_(__Stack),
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(7, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_7_(__Stack),
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(8, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_8_(__Stack),
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(9, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_9_(__Stack),
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(10, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [10 | __Ss], [__T | __Stack]);
yeccpars2(10, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [10 | __Ss], [__T | __Stack]);
yeccpars2(10, reserved_word, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [10 | __Ss], [__T | __Stack]);
yeccpars2(10, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [10 | __Ss], [__T | __Stack]);
yeccpars2(10, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(11, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_11_(__Stack),
 yeccpars2(14, __Cat, [11 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(12, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, reserved_word, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_12_(__Stack),
 yeccpars2(yeccgoto(symbols, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(13, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_13_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(symbols, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(14, dot, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(15, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, reserved_symbol, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, reserved_word, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(16, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_16_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(attached_code, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(17, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, reserved_symbol, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, reserved_word, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_17_(__Stack),
 yeccpars2(yeccgoto(tokens, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(18, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_18_(__Stack),
 yeccpars2(yeccgoto(token, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(19, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_19_(__Stack),
 yeccpars2(yeccgoto(token, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(20, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(token, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(21, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(token, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(22, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(token, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(23, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(token, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(24, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_24_(__Stack),
 yeccpars2(yeccgoto(token, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(25, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_25_(__Stack),
 yeccpars2(yeccgoto(token, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(26, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(token, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(27, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(token, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(28, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_28_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(tokens, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(29, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_29_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(rule, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(30, dot, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [30 | __Ss], [__T | __Stack]);
yeccpars2(30, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(31, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_31_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(declaration, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(__Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.1",{missing_state_in_action_table, __Other}}).

yeccgoto(attached_code, 11) ->
 14;
yeccgoto(declaration, 0) ->
 5;
yeccgoto(grammar, 0) ->
 4;
yeccgoto(head, 0) ->
 3;
yeccgoto(rule, 0) ->
 2;
yeccgoto(symbol, 0) ->
 1;
yeccgoto(symbol, 1) ->
 12;
yeccgoto(symbol, 10) ->
 12;
yeccgoto(symbol, 12) ->
 12;
yeccgoto(symbols, 1) ->
 30;
yeccgoto(symbols, 10) ->
 11;
yeccgoto(symbols, 12) ->
 13;
yeccgoto(token, 15) ->
 17;
yeccgoto(token, 17) ->
 17;
yeccgoto(tokens, 15) ->
 16;
yeccgoto(tokens, 17) ->
 28;
yeccgoto(__Symbol, __State) ->
 erlang:error({yecc_bug,"1.1",{__Symbol, __State, missing_in_goto_table}}).

-compile({inline,{yeccpars2_6_,1}}).
-file("yeccgramm.yrl", 41).
yeccpars2_6_([__1 | __Stack]) ->
 [begin
   symbol ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_7_,1}}).
-file("yeccgramm.yrl", 42).
yeccpars2_7_([__1 | __Stack]) ->
 [begin
   symbol ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_8_,1}}).
-file("yeccgramm.yrl", 43).
yeccpars2_8_([__1 | __Stack]) ->
 [begin
   symbol ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_9_,1}}).
-file("yeccgramm.yrl", 40).
yeccpars2_9_([__1 | __Stack]) ->
 [begin
   symbol ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_11_,1}}).
-file("yeccgramm.yrl", 37).
yeccpars2_11_(__Stack) ->
 [begin
   { erlang_code , [ { atom , 0 , '$undefined' } ] }
  end | __Stack].

-compile({inline,{yeccpars2_12_,1}}).
-file("yeccgramm.yrl", 34).
yeccpars2_12_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_13_,1}}).
-file("yeccgramm.yrl", 35).
yeccpars2_13_([__2,__1 | __Stack]) ->
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_16_,1}}).
-file("yeccgramm.yrl", 36).
yeccpars2_16_([__2,__1 | __Stack]) ->
 [begin
   { erlang_code , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_17_,1}}).
-file("yeccgramm.yrl", 38).
yeccpars2_17_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_18_,1}}).
-file("yeccgramm.yrl", 52).
yeccpars2_18_([__1 | __Stack]) ->
 [begin
   { '->' , line_of ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_19_,1}}).
-file("yeccgramm.yrl", 53).
yeccpars2_19_([__1 | __Stack]) ->
 [begin
   { ':' , line_of ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_24_,1}}).
-file("yeccgramm.yrl", 50).
yeccpars2_24_([__1 | __Stack]) ->
 [begin
   { value_of ( __1 ) , line_of ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_25_,1}}).
-file("yeccgramm.yrl", 51).
yeccpars2_25_([__1 | __Stack]) ->
 [begin
   { value_of ( __1 ) , line_of ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_28_,1}}).
-file("yeccgramm.yrl", 39).
yeccpars2_28_([__2,__1 | __Stack]) ->
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_29_,1}}).
-file("yeccgramm.yrl", 32).
yeccpars2_29_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { rule , [ __1 | __3 ] , __4 }
  end | __Stack].

-compile({inline,{yeccpars2_31_,1}}).
-file("yeccgramm.yrl", 31).
yeccpars2_31_([__3,__2,__1 | __Stack]) ->
 [begin
   { __1 , __2 }
  end | __Stack].


-file("yeccgramm.yrl", 71).
