-module(xmerl_xpath_parse).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("xmerl_xpath_parse.yrl", 305).

% token({Token, _Line}) ->
% 	Token;
% token({Token, _Line, _Value}) ->
% 	Token.

value({Token, _Line}) ->
	Token;
value({_Token, _Line, Value}) ->
	Value.

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



-file("./xmerl_xpath_parse.erl", 108).

yeccpars2(0, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, axis, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, function_name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, node_type, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, number, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, prefix_test, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, var_reference, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, wildcard, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(1, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 108, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('UnaryExpr', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(2, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('MultiplicativeExpr', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(3, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('RelativeLocationPath', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(4, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_4_(__Stack),
 yeccpars2(yeccgoto('LocationPath', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(5, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 91, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, '<=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 92, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 93, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, '>=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 94, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('EqualityExpr', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(6, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('FilterExpr', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(7, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('UnionExpr', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(8, 'or', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 106, [8 | __Ss], [__T | __Stack]);
yeccpars2(8, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('Expr', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(9, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_9_(__Stack),
 yeccpars2(yeccgoto('Step', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(10, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('NodeTest', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(11, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 79, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, 'div', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 80, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, mod, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 81, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('AdditiveExpr', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(12, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('PathExpr', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(13, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('PrimaryExpr', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(14, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 102, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('PathExpr', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(15, '$end', _, __Stack, _, _, _) ->
 {ok, hd(__Stack)};
yeccpars2(15, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(16, '!=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 88, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 89, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('AndExpr', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(17, 'and', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 86, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('OrExpr', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(18, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('RelationalExpr', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(19, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_19_(__Stack),
 yeccpars2(yeccgoto('LocationPath', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(20, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_20_(__Stack),
 yeccpars2(yeccgoto('Step', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(21, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('RelativeLocationPath', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(22, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('RelativeLocationPath', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(23, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, axis, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, function_name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, node_type, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, number, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, prefix_test, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, var_reference, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, wildcard, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(24, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, axis, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, function_name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, node_type, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, number, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, prefix_test, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, var_reference, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, wildcard, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(25, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('AbbreviatedStep', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(26, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('AbbreviatedStep', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(27, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [27 | __Ss], [__T | __Stack]);
yeccpars2(27, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [27 | __Ss], [__T | __Stack]);
yeccpars2(27, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [27 | __Ss], [__T | __Stack]);
yeccpars2(27, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [27 | __Ss], [__T | __Stack]);
yeccpars2(27, axis, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [27 | __Ss], [__T | __Stack]);
yeccpars2(27, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [27 | __Ss], [__T | __Stack]);
yeccpars2(27, node_type, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [27 | __Ss], [__T | __Stack]);
yeccpars2(27, prefix_test, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [27 | __Ss], [__T | __Stack]);
yeccpars2(27, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [27 | __Ss], [__T | __Stack]);
yeccpars2(27, wildcard, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [27 | __Ss], [__T | __Stack]);
yeccpars2(27, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_27_(__Stack),
 yeccpars2(yeccgoto('AbsoluteLocationPath', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(28, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, axis, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, node_type, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, prefix_test, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, wildcard, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(29, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 64, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(30, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [30 | __Ss], [__T | __Stack]);
yeccpars2(30, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(31, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(32, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_32_(__Stack),
 yeccpars2(yeccgoto('PrimaryExpr', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(33, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_33_(__Stack),
 yeccpars2(yeccgoto('NameTest', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(34, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(35, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_35_(__Stack),
 yeccpars2(yeccgoto('PrimaryExpr', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(36, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_36_(__Stack),
 yeccpars2(yeccgoto('NameTest', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(37, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(38, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_38_(__Stack),
 yeccpars2(yeccgoto('PrimaryExpr', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(39, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_39_(__Stack),
 yeccpars2(yeccgoto('NameTest', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(40, literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(41, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(42, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_42_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('NodeTest', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(43, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [43 | __Ss], [__T | __Stack]);
yeccpars2(43, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(44, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_44_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('NodeTest', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(45, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, axis, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, function_name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, node_type, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, number, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, prefix_test, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, var_reference, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, wildcard, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(46, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('Argument', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(47, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_47_(__Stack),
 yeccpars2(yeccgoto('<ArgumentMember>', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(48, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_48_(__Stack),
 yeccpars2(yeccgoto('<ArgumentList>', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(49, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [49 | __Ss], [__T | __Stack]);
yeccpars2(49, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(50, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_50_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('FunctionCall', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(51, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_51_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('FunctionCall', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(52, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, axis, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, function_name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, node_type, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, number, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, prefix_test, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, var_reference, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, wildcard, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(53, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_53_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<ArgumentMember>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(54, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, node_type, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, prefix_test, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, wildcard, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(55, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_55_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('Step', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(56, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_56_(__Stack),
 yeccpars2(yeccgoto('<PredicateMember>', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(57, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [57 | __Ss], [__T | __Stack]);
yeccpars2(57, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_57_(__Stack),
 yeccpars2(yeccgoto('<PredicateList>', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(58, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_58_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('Step', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(59, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, axis, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, function_name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, node_type, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, number, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, prefix_test, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, var_reference, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, wildcard, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(60, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(61, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('PredicateExpr', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(62, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_62_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('Predicate', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(63, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_63_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<PredicateMember>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(64, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_64_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Step', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(65, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_65_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('Step', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(66, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [66 | __Ss], [__T | __Stack]);
yeccpars2(66, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_66_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('AbbreviatedAbsoluteLocationPath', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(67, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [67 | __Ss], [__T | __Stack]);
yeccpars2(67, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [67 | __Ss], [__T | __Stack]);
yeccpars2(67, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [67 | __Ss], [__T | __Stack]);
yeccpars2(67, axis, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [67 | __Ss], [__T | __Stack]);
yeccpars2(67, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [67 | __Ss], [__T | __Stack]);
yeccpars2(67, node_type, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [67 | __Ss], [__T | __Stack]);
yeccpars2(67, prefix_test, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [67 | __Ss], [__T | __Stack]);
yeccpars2(67, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [67 | __Ss], [__T | __Stack]);
yeccpars2(67, wildcard, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [67 | __Ss], [__T | __Stack]);
yeccpars2(67, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(68, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, axis, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, node_type, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, prefix_test, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, wildcard, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(69, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_69_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('AbbreviatedRelativeLocationPath', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(70, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_70_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('RelativeLocationPath', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(71, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [71 | __Ss], [__T | __Stack]);
yeccpars2(71, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [71 | __Ss], [__T | __Stack]);
yeccpars2(71, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_71_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('AbsoluteLocationPath', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(72, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_72_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('UnaryExpr', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(73, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [73 | __Ss], [__T | __Stack]);
yeccpars2(73, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(74, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_74_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('PrimaryExpr', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(75, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, axis, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, function_name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, node_type, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, number, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, prefix_test, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, var_reference, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, wildcard, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(76, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, axis, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, function_name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, node_type, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, number, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, prefix_test, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, var_reference, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, wildcard, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(77, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 79, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, 'div', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 80, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, mod, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 81, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_77_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('AdditiveExpr', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(78, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, axis, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, function_name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, node_type, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, number, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, prefix_test, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, var_reference, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, wildcard, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(79, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_79_(__Stack),
 yeccpars2(yeccgoto('MultiplyOperator', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(80, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, axis, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, function_name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, node_type, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, number, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, prefix_test, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, var_reference, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, wildcard, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(81, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, axis, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, function_name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, node_type, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, number, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, prefix_test, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, var_reference, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, wildcard, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(82, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_82_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('MultiplicativeExpr', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(83, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_83_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('MultiplicativeExpr', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(84, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_84_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('MultiplicativeExpr', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(85, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 79, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, 'div', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 80, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, mod, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 81, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_85_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('AdditiveExpr', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(86, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, axis, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, function_name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, node_type, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, number, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, prefix_test, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, var_reference, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, wildcard, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(87, '!=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 88, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 89, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_87_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('AndExpr', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(88, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, axis, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, function_name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, node_type, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, number, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, prefix_test, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, var_reference, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, wildcard, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(89, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, axis, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, function_name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, node_type, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, number, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, prefix_test, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, var_reference, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, wildcard, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [89 | __Ss], [__T | __Stack]);
yeccpars2(89, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(90, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 91, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, '<=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 92, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 93, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, '>=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 94, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_90_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('EqualityExpr', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(91, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, axis, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, function_name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, node_type, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, number, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, prefix_test, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, var_reference, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, wildcard, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [91 | __Ss], [__T | __Stack]);
yeccpars2(91, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(92, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, axis, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, function_name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, node_type, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, number, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, prefix_test, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, var_reference, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, wildcard, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(93, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, axis, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, function_name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, node_type, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, number, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, prefix_test, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, var_reference, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, wildcard, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [93 | __Ss], [__T | __Stack]);
yeccpars2(93, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(94, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, axis, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, function_name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, node_type, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, number, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, prefix_test, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, var_reference, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, wildcard, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [94 | __Ss], [__T | __Stack]);
yeccpars2(94, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(95, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [95 | __Ss], [__T | __Stack]);
yeccpars2(95, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [95 | __Ss], [__T | __Stack]);
yeccpars2(95, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_95_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('RelationalExpr', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(96, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_96_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('RelationalExpr', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(97, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [97 | __Ss], [__T | __Stack]);
yeccpars2(97, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_97_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('RelationalExpr', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(98, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [98 | __Ss], [__T | __Stack]);
yeccpars2(98, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_98_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('RelationalExpr', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(99, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 91, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, '<=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 92, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 93, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, '>=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 94, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_99_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('EqualityExpr', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(100, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_100_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('FilterExpr', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(101, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, axis, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, node_type, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, prefix_test, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, wildcard, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(102, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [102 | __Ss], [__T | __Stack]);
yeccpars2(102, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [102 | __Ss], [__T | __Stack]);
yeccpars2(102, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [102 | __Ss], [__T | __Stack]);
yeccpars2(102, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [102 | __Ss], [__T | __Stack]);
yeccpars2(102, axis, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [102 | __Ss], [__T | __Stack]);
yeccpars2(102, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [102 | __Ss], [__T | __Stack]);
yeccpars2(102, node_type, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [102 | __Ss], [__T | __Stack]);
yeccpars2(102, prefix_test, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [102 | __Ss], [__T | __Stack]);
yeccpars2(102, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [102 | __Ss], [__T | __Stack]);
yeccpars2(102, wildcard, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [102 | __Ss], [__T | __Stack]);
yeccpars2(102, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(103, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [103 | __Ss], [__T | __Stack]);
yeccpars2(103, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [103 | __Ss], [__T | __Stack]);
yeccpars2(103, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_103_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('PathExpr', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(104, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_104_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('PathExpr', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(105, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_105_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Step', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(106, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, axis, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, function_name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, node_type, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, number, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, prefix_test, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, var_reference, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, wildcard, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(107, 'and', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 86, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_107_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('OrExpr', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(108, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '..', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '//', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '@', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, axis, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, function_name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, literal, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, name, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, node_type, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, number, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, prefix_test, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'processing-instruction', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, var_reference, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, wildcard, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(109, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_109_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('UnionExpr', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(__Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.1",{missing_state_in_action_table, __Other}}).

yeccgoto('<ArgumentList>', 45) ->
 49;
yeccgoto('<ArgumentMember>', 45) ->
 48;
yeccgoto('<PredicateList>', 9) ->
 105;
yeccgoto('<PredicateList>', 55) ->
 58;
yeccgoto('<PredicateList>', 64) ->
 65;
yeccgoto('<PredicateMember>', 9) ->
 57;
yeccgoto('<PredicateMember>', 55) ->
 57;
yeccgoto('<PredicateMember>', 64) ->
 57;
yeccgoto('AbbreviatedAbsoluteLocationPath', 0) ->
 22;
yeccgoto('AbbreviatedAbsoluteLocationPath', 23) ->
 22;
yeccgoto('AbbreviatedAbsoluteLocationPath', 24) ->
 22;
yeccgoto('AbbreviatedAbsoluteLocationPath', 27) ->
 22;
yeccgoto('AbbreviatedAbsoluteLocationPath', 28) ->
 22;
yeccgoto('AbbreviatedAbsoluteLocationPath', 45) ->
 22;
yeccgoto('AbbreviatedAbsoluteLocationPath', 52) ->
 22;
yeccgoto('AbbreviatedAbsoluteLocationPath', 59) ->
 22;
yeccgoto('AbbreviatedAbsoluteLocationPath', 75) ->
 22;
yeccgoto('AbbreviatedAbsoluteLocationPath', 76) ->
 22;
yeccgoto('AbbreviatedAbsoluteLocationPath', 78) ->
 22;
yeccgoto('AbbreviatedAbsoluteLocationPath', 80) ->
 22;
yeccgoto('AbbreviatedAbsoluteLocationPath', 81) ->
 22;
yeccgoto('AbbreviatedAbsoluteLocationPath', 86) ->
 22;
yeccgoto('AbbreviatedAbsoluteLocationPath', 88) ->
 22;
yeccgoto('AbbreviatedAbsoluteLocationPath', 89) ->
 22;
yeccgoto('AbbreviatedAbsoluteLocationPath', 91) ->
 22;
yeccgoto('AbbreviatedAbsoluteLocationPath', 92) ->
 22;
yeccgoto('AbbreviatedAbsoluteLocationPath', 93) ->
 22;
yeccgoto('AbbreviatedAbsoluteLocationPath', 94) ->
 22;
yeccgoto('AbbreviatedAbsoluteLocationPath', 101) ->
 22;
yeccgoto('AbbreviatedAbsoluteLocationPath', 102) ->
 22;
yeccgoto('AbbreviatedAbsoluteLocationPath', 106) ->
 22;
yeccgoto('AbbreviatedAbsoluteLocationPath', 108) ->
 22;
yeccgoto('AbbreviatedRelativeLocationPath', 0) ->
 21;
yeccgoto('AbbreviatedRelativeLocationPath', 23) ->
 21;
yeccgoto('AbbreviatedRelativeLocationPath', 24) ->
 21;
yeccgoto('AbbreviatedRelativeLocationPath', 27) ->
 21;
yeccgoto('AbbreviatedRelativeLocationPath', 28) ->
 21;
yeccgoto('AbbreviatedRelativeLocationPath', 45) ->
 21;
yeccgoto('AbbreviatedRelativeLocationPath', 52) ->
 21;
yeccgoto('AbbreviatedRelativeLocationPath', 59) ->
 21;
yeccgoto('AbbreviatedRelativeLocationPath', 75) ->
 21;
yeccgoto('AbbreviatedRelativeLocationPath', 76) ->
 21;
yeccgoto('AbbreviatedRelativeLocationPath', 78) ->
 21;
yeccgoto('AbbreviatedRelativeLocationPath', 80) ->
 21;
yeccgoto('AbbreviatedRelativeLocationPath', 81) ->
 21;
yeccgoto('AbbreviatedRelativeLocationPath', 86) ->
 21;
yeccgoto('AbbreviatedRelativeLocationPath', 88) ->
 21;
yeccgoto('AbbreviatedRelativeLocationPath', 89) ->
 21;
yeccgoto('AbbreviatedRelativeLocationPath', 91) ->
 21;
yeccgoto('AbbreviatedRelativeLocationPath', 92) ->
 21;
yeccgoto('AbbreviatedRelativeLocationPath', 93) ->
 21;
yeccgoto('AbbreviatedRelativeLocationPath', 94) ->
 21;
yeccgoto('AbbreviatedRelativeLocationPath', 101) ->
 21;
yeccgoto('AbbreviatedRelativeLocationPath', 102) ->
 21;
yeccgoto('AbbreviatedRelativeLocationPath', 106) ->
 21;
yeccgoto('AbbreviatedRelativeLocationPath', 108) ->
 21;
yeccgoto('AbbreviatedStep', 0) ->
 20;
yeccgoto('AbbreviatedStep', 23) ->
 20;
yeccgoto('AbbreviatedStep', 24) ->
 20;
yeccgoto('AbbreviatedStep', 27) ->
 20;
yeccgoto('AbbreviatedStep', 28) ->
 20;
yeccgoto('AbbreviatedStep', 45) ->
 20;
yeccgoto('AbbreviatedStep', 52) ->
 20;
yeccgoto('AbbreviatedStep', 59) ->
 20;
yeccgoto('AbbreviatedStep', 67) ->
 20;
yeccgoto('AbbreviatedStep', 68) ->
 20;
yeccgoto('AbbreviatedStep', 75) ->
 20;
yeccgoto('AbbreviatedStep', 76) ->
 20;
yeccgoto('AbbreviatedStep', 78) ->
 20;
yeccgoto('AbbreviatedStep', 80) ->
 20;
yeccgoto('AbbreviatedStep', 81) ->
 20;
yeccgoto('AbbreviatedStep', 86) ->
 20;
yeccgoto('AbbreviatedStep', 88) ->
 20;
yeccgoto('AbbreviatedStep', 89) ->
 20;
yeccgoto('AbbreviatedStep', 91) ->
 20;
yeccgoto('AbbreviatedStep', 92) ->
 20;
yeccgoto('AbbreviatedStep', 93) ->
 20;
yeccgoto('AbbreviatedStep', 94) ->
 20;
yeccgoto('AbbreviatedStep', 101) ->
 20;
yeccgoto('AbbreviatedStep', 102) ->
 20;
yeccgoto('AbbreviatedStep', 106) ->
 20;
yeccgoto('AbbreviatedStep', 108) ->
 20;
yeccgoto('AbsoluteLocationPath', 0) ->
 19;
yeccgoto('AbsoluteLocationPath', 23) ->
 19;
yeccgoto('AbsoluteLocationPath', 24) ->
 19;
yeccgoto('AbsoluteLocationPath', 45) ->
 19;
yeccgoto('AbsoluteLocationPath', 52) ->
 19;
yeccgoto('AbsoluteLocationPath', 59) ->
 19;
yeccgoto('AbsoluteLocationPath', 75) ->
 19;
yeccgoto('AbsoluteLocationPath', 76) ->
 19;
yeccgoto('AbsoluteLocationPath', 78) ->
 19;
yeccgoto('AbsoluteLocationPath', 80) ->
 19;
yeccgoto('AbsoluteLocationPath', 81) ->
 19;
yeccgoto('AbsoluteLocationPath', 86) ->
 19;
yeccgoto('AbsoluteLocationPath', 88) ->
 19;
yeccgoto('AbsoluteLocationPath', 89) ->
 19;
yeccgoto('AbsoluteLocationPath', 91) ->
 19;
yeccgoto('AbsoluteLocationPath', 92) ->
 19;
yeccgoto('AbsoluteLocationPath', 93) ->
 19;
yeccgoto('AbsoluteLocationPath', 94) ->
 19;
yeccgoto('AbsoluteLocationPath', 106) ->
 19;
yeccgoto('AbsoluteLocationPath', 108) ->
 19;
yeccgoto('AdditiveExpr', 0) ->
 18;
yeccgoto('AdditiveExpr', 23) ->
 18;
yeccgoto('AdditiveExpr', 45) ->
 18;
yeccgoto('AdditiveExpr', 52) ->
 18;
yeccgoto('AdditiveExpr', 59) ->
 18;
yeccgoto('AdditiveExpr', 86) ->
 18;
yeccgoto('AdditiveExpr', 88) ->
 18;
yeccgoto('AdditiveExpr', 89) ->
 18;
yeccgoto('AdditiveExpr', 91) ->
 98;
yeccgoto('AdditiveExpr', 92) ->
 97;
yeccgoto('AdditiveExpr', 93) ->
 96;
yeccgoto('AdditiveExpr', 94) ->
 95;
yeccgoto('AdditiveExpr', 106) ->
 18;
yeccgoto('AndExpr', 0) ->
 17;
yeccgoto('AndExpr', 23) ->
 17;
yeccgoto('AndExpr', 45) ->
 17;
yeccgoto('AndExpr', 52) ->
 17;
yeccgoto('AndExpr', 59) ->
 17;
yeccgoto('AndExpr', 106) ->
 107;
yeccgoto('Argument', 45) ->
 47;
yeccgoto('Argument', 52) ->
 53;
yeccgoto('EqualityExpr', 0) ->
 16;
yeccgoto('EqualityExpr', 23) ->
 16;
yeccgoto('EqualityExpr', 45) ->
 16;
yeccgoto('EqualityExpr', 52) ->
 16;
yeccgoto('EqualityExpr', 59) ->
 16;
yeccgoto('EqualityExpr', 86) ->
 87;
yeccgoto('EqualityExpr', 106) ->
 16;
yeccgoto('Expr', 0) ->
 15;
yeccgoto('Expr', 23) ->
 73;
yeccgoto('Expr', 45) ->
 46;
yeccgoto('Expr', 52) ->
 46;
yeccgoto('Expr', 59) ->
 61;
yeccgoto('FilterExpr', 0) ->
 14;
yeccgoto('FilterExpr', 23) ->
 14;
yeccgoto('FilterExpr', 24) ->
 14;
yeccgoto('FilterExpr', 45) ->
 14;
yeccgoto('FilterExpr', 52) ->
 14;
yeccgoto('FilterExpr', 59) ->
 14;
yeccgoto('FilterExpr', 75) ->
 14;
yeccgoto('FilterExpr', 76) ->
 14;
yeccgoto('FilterExpr', 78) ->
 14;
yeccgoto('FilterExpr', 80) ->
 14;
yeccgoto('FilterExpr', 81) ->
 14;
yeccgoto('FilterExpr', 86) ->
 14;
yeccgoto('FilterExpr', 88) ->
 14;
yeccgoto('FilterExpr', 89) ->
 14;
yeccgoto('FilterExpr', 91) ->
 14;
yeccgoto('FilterExpr', 92) ->
 14;
yeccgoto('FilterExpr', 93) ->
 14;
yeccgoto('FilterExpr', 94) ->
 14;
yeccgoto('FilterExpr', 106) ->
 14;
yeccgoto('FilterExpr', 108) ->
 14;
yeccgoto('FunctionCall', 0) ->
 13;
yeccgoto('FunctionCall', 23) ->
 13;
yeccgoto('FunctionCall', 24) ->
 13;
yeccgoto('FunctionCall', 45) ->
 13;
yeccgoto('FunctionCall', 52) ->
 13;
yeccgoto('FunctionCall', 59) ->
 13;
yeccgoto('FunctionCall', 75) ->
 13;
yeccgoto('FunctionCall', 76) ->
 13;
yeccgoto('FunctionCall', 78) ->
 13;
yeccgoto('FunctionCall', 80) ->
 13;
yeccgoto('FunctionCall', 81) ->
 13;
yeccgoto('FunctionCall', 86) ->
 13;
yeccgoto('FunctionCall', 88) ->
 13;
yeccgoto('FunctionCall', 89) ->
 13;
yeccgoto('FunctionCall', 91) ->
 13;
yeccgoto('FunctionCall', 92) ->
 13;
yeccgoto('FunctionCall', 93) ->
 13;
yeccgoto('FunctionCall', 94) ->
 13;
yeccgoto('FunctionCall', 106) ->
 13;
yeccgoto('FunctionCall', 108) ->
 13;
yeccgoto('LocationPath', 0) ->
 12;
yeccgoto('LocationPath', 23) ->
 12;
yeccgoto('LocationPath', 24) ->
 12;
yeccgoto('LocationPath', 45) ->
 12;
yeccgoto('LocationPath', 52) ->
 12;
yeccgoto('LocationPath', 59) ->
 12;
yeccgoto('LocationPath', 75) ->
 12;
yeccgoto('LocationPath', 76) ->
 12;
yeccgoto('LocationPath', 78) ->
 12;
yeccgoto('LocationPath', 80) ->
 12;
yeccgoto('LocationPath', 81) ->
 12;
yeccgoto('LocationPath', 86) ->
 12;
yeccgoto('LocationPath', 88) ->
 12;
yeccgoto('LocationPath', 89) ->
 12;
yeccgoto('LocationPath', 91) ->
 12;
yeccgoto('LocationPath', 92) ->
 12;
yeccgoto('LocationPath', 93) ->
 12;
yeccgoto('LocationPath', 94) ->
 12;
yeccgoto('LocationPath', 106) ->
 12;
yeccgoto('LocationPath', 108) ->
 12;
yeccgoto('MultiplicativeExpr', 0) ->
 11;
yeccgoto('MultiplicativeExpr', 23) ->
 11;
yeccgoto('MultiplicativeExpr', 45) ->
 11;
yeccgoto('MultiplicativeExpr', 52) ->
 11;
yeccgoto('MultiplicativeExpr', 59) ->
 11;
yeccgoto('MultiplicativeExpr', 75) ->
 85;
yeccgoto('MultiplicativeExpr', 76) ->
 77;
yeccgoto('MultiplicativeExpr', 86) ->
 11;
yeccgoto('MultiplicativeExpr', 88) ->
 11;
yeccgoto('MultiplicativeExpr', 89) ->
 11;
yeccgoto('MultiplicativeExpr', 91) ->
 11;
yeccgoto('MultiplicativeExpr', 92) ->
 11;
yeccgoto('MultiplicativeExpr', 93) ->
 11;
yeccgoto('MultiplicativeExpr', 94) ->
 11;
yeccgoto('MultiplicativeExpr', 106) ->
 11;
yeccgoto('MultiplyOperator', 11) ->
 78;
yeccgoto('MultiplyOperator', 77) ->
 78;
yeccgoto('MultiplyOperator', 85) ->
 78;
yeccgoto('NameTest', 0) ->
 10;
yeccgoto('NameTest', 23) ->
 10;
yeccgoto('NameTest', 24) ->
 10;
yeccgoto('NameTest', 27) ->
 10;
yeccgoto('NameTest', 28) ->
 10;
yeccgoto('NameTest', 45) ->
 10;
yeccgoto('NameTest', 52) ->
 10;
yeccgoto('NameTest', 54) ->
 10;
yeccgoto('NameTest', 59) ->
 10;
yeccgoto('NameTest', 67) ->
 10;
yeccgoto('NameTest', 68) ->
 10;
yeccgoto('NameTest', 75) ->
 10;
yeccgoto('NameTest', 76) ->
 10;
yeccgoto('NameTest', 78) ->
 10;
yeccgoto('NameTest', 80) ->
 10;
yeccgoto('NameTest', 81) ->
 10;
yeccgoto('NameTest', 86) ->
 10;
yeccgoto('NameTest', 88) ->
 10;
yeccgoto('NameTest', 89) ->
 10;
yeccgoto('NameTest', 91) ->
 10;
yeccgoto('NameTest', 92) ->
 10;
yeccgoto('NameTest', 93) ->
 10;
yeccgoto('NameTest', 94) ->
 10;
yeccgoto('NameTest', 101) ->
 10;
yeccgoto('NameTest', 102) ->
 10;
yeccgoto('NameTest', 106) ->
 10;
yeccgoto('NameTest', 108) ->
 10;
yeccgoto('NodeTest', 0) ->
 9;
yeccgoto('NodeTest', 23) ->
 9;
yeccgoto('NodeTest', 24) ->
 9;
yeccgoto('NodeTest', 27) ->
 9;
yeccgoto('NodeTest', 28) ->
 9;
yeccgoto('NodeTest', 45) ->
 9;
yeccgoto('NodeTest', 52) ->
 9;
yeccgoto('NodeTest', 54) ->
 55;
yeccgoto('NodeTest', 59) ->
 9;
yeccgoto('NodeTest', 67) ->
 9;
yeccgoto('NodeTest', 68) ->
 9;
yeccgoto('NodeTest', 75) ->
 9;
yeccgoto('NodeTest', 76) ->
 9;
yeccgoto('NodeTest', 78) ->
 9;
yeccgoto('NodeTest', 80) ->
 9;
yeccgoto('NodeTest', 81) ->
 9;
yeccgoto('NodeTest', 86) ->
 9;
yeccgoto('NodeTest', 88) ->
 9;
yeccgoto('NodeTest', 89) ->
 9;
yeccgoto('NodeTest', 91) ->
 9;
yeccgoto('NodeTest', 92) ->
 9;
yeccgoto('NodeTest', 93) ->
 9;
yeccgoto('NodeTest', 94) ->
 9;
yeccgoto('NodeTest', 101) ->
 9;
yeccgoto('NodeTest', 102) ->
 9;
yeccgoto('NodeTest', 106) ->
 9;
yeccgoto('NodeTest', 108) ->
 9;
yeccgoto('OrExpr', 0) ->
 8;
yeccgoto('OrExpr', 23) ->
 8;
yeccgoto('OrExpr', 45) ->
 8;
yeccgoto('OrExpr', 52) ->
 8;
yeccgoto('OrExpr', 59) ->
 8;
yeccgoto('PathExpr', 0) ->
 7;
yeccgoto('PathExpr', 23) ->
 7;
yeccgoto('PathExpr', 24) ->
 7;
yeccgoto('PathExpr', 45) ->
 7;
yeccgoto('PathExpr', 52) ->
 7;
yeccgoto('PathExpr', 59) ->
 7;
yeccgoto('PathExpr', 75) ->
 7;
yeccgoto('PathExpr', 76) ->
 7;
yeccgoto('PathExpr', 78) ->
 7;
yeccgoto('PathExpr', 80) ->
 7;
yeccgoto('PathExpr', 81) ->
 7;
yeccgoto('PathExpr', 86) ->
 7;
yeccgoto('PathExpr', 88) ->
 7;
yeccgoto('PathExpr', 89) ->
 7;
yeccgoto('PathExpr', 91) ->
 7;
yeccgoto('PathExpr', 92) ->
 7;
yeccgoto('PathExpr', 93) ->
 7;
yeccgoto('PathExpr', 94) ->
 7;
yeccgoto('PathExpr', 106) ->
 7;
yeccgoto('PathExpr', 108) ->
 109;
yeccgoto('Predicate', 9) ->
 56;
yeccgoto('Predicate', 14) ->
 100;
yeccgoto('Predicate', 55) ->
 56;
yeccgoto('Predicate', 57) ->
 63;
yeccgoto('Predicate', 64) ->
 56;
yeccgoto('PredicateExpr', 59) ->
 60;
yeccgoto('PrimaryExpr', 0) ->
 6;
yeccgoto('PrimaryExpr', 23) ->
 6;
yeccgoto('PrimaryExpr', 24) ->
 6;
yeccgoto('PrimaryExpr', 45) ->
 6;
yeccgoto('PrimaryExpr', 52) ->
 6;
yeccgoto('PrimaryExpr', 59) ->
 6;
yeccgoto('PrimaryExpr', 75) ->
 6;
yeccgoto('PrimaryExpr', 76) ->
 6;
yeccgoto('PrimaryExpr', 78) ->
 6;
yeccgoto('PrimaryExpr', 80) ->
 6;
yeccgoto('PrimaryExpr', 81) ->
 6;
yeccgoto('PrimaryExpr', 86) ->
 6;
yeccgoto('PrimaryExpr', 88) ->
 6;
yeccgoto('PrimaryExpr', 89) ->
 6;
yeccgoto('PrimaryExpr', 91) ->
 6;
yeccgoto('PrimaryExpr', 92) ->
 6;
yeccgoto('PrimaryExpr', 93) ->
 6;
yeccgoto('PrimaryExpr', 94) ->
 6;
yeccgoto('PrimaryExpr', 106) ->
 6;
yeccgoto('PrimaryExpr', 108) ->
 6;
yeccgoto('RelationalExpr', 0) ->
 5;
yeccgoto('RelationalExpr', 23) ->
 5;
yeccgoto('RelationalExpr', 45) ->
 5;
yeccgoto('RelationalExpr', 52) ->
 5;
yeccgoto('RelationalExpr', 59) ->
 5;
yeccgoto('RelationalExpr', 86) ->
 5;
yeccgoto('RelationalExpr', 88) ->
 99;
yeccgoto('RelationalExpr', 89) ->
 90;
yeccgoto('RelationalExpr', 106) ->
 5;
yeccgoto('RelativeLocationPath', 0) ->
 4;
yeccgoto('RelativeLocationPath', 23) ->
 4;
yeccgoto('RelativeLocationPath', 24) ->
 4;
yeccgoto('RelativeLocationPath', 27) ->
 71;
yeccgoto('RelativeLocationPath', 28) ->
 66;
yeccgoto('RelativeLocationPath', 45) ->
 4;
yeccgoto('RelativeLocationPath', 52) ->
 4;
yeccgoto('RelativeLocationPath', 59) ->
 4;
yeccgoto('RelativeLocationPath', 75) ->
 4;
yeccgoto('RelativeLocationPath', 76) ->
 4;
yeccgoto('RelativeLocationPath', 78) ->
 4;
yeccgoto('RelativeLocationPath', 80) ->
 4;
yeccgoto('RelativeLocationPath', 81) ->
 4;
yeccgoto('RelativeLocationPath', 86) ->
 4;
yeccgoto('RelativeLocationPath', 88) ->
 4;
yeccgoto('RelativeLocationPath', 89) ->
 4;
yeccgoto('RelativeLocationPath', 91) ->
 4;
yeccgoto('RelativeLocationPath', 92) ->
 4;
yeccgoto('RelativeLocationPath', 93) ->
 4;
yeccgoto('RelativeLocationPath', 94) ->
 4;
yeccgoto('RelativeLocationPath', 101) ->
 104;
yeccgoto('RelativeLocationPath', 102) ->
 103;
yeccgoto('RelativeLocationPath', 106) ->
 4;
yeccgoto('RelativeLocationPath', 108) ->
 4;
yeccgoto('Step', 0) ->
 3;
yeccgoto('Step', 23) ->
 3;
yeccgoto('Step', 24) ->
 3;
yeccgoto('Step', 27) ->
 3;
yeccgoto('Step', 28) ->
 3;
yeccgoto('Step', 45) ->
 3;
yeccgoto('Step', 52) ->
 3;
yeccgoto('Step', 59) ->
 3;
yeccgoto('Step', 67) ->
 70;
yeccgoto('Step', 68) ->
 69;
yeccgoto('Step', 75) ->
 3;
yeccgoto('Step', 76) ->
 3;
yeccgoto('Step', 78) ->
 3;
yeccgoto('Step', 80) ->
 3;
yeccgoto('Step', 81) ->
 3;
yeccgoto('Step', 86) ->
 3;
yeccgoto('Step', 88) ->
 3;
yeccgoto('Step', 89) ->
 3;
yeccgoto('Step', 91) ->
 3;
yeccgoto('Step', 92) ->
 3;
yeccgoto('Step', 93) ->
 3;
yeccgoto('Step', 94) ->
 3;
yeccgoto('Step', 101) ->
 3;
yeccgoto('Step', 102) ->
 3;
yeccgoto('Step', 106) ->
 3;
yeccgoto('Step', 108) ->
 3;
yeccgoto('UnaryExpr', 0) ->
 2;
yeccgoto('UnaryExpr', 23) ->
 2;
yeccgoto('UnaryExpr', 24) ->
 72;
yeccgoto('UnaryExpr', 45) ->
 2;
yeccgoto('UnaryExpr', 52) ->
 2;
yeccgoto('UnaryExpr', 59) ->
 2;
yeccgoto('UnaryExpr', 75) ->
 2;
yeccgoto('UnaryExpr', 76) ->
 2;
yeccgoto('UnaryExpr', 78) ->
 84;
yeccgoto('UnaryExpr', 80) ->
 83;
yeccgoto('UnaryExpr', 81) ->
 82;
yeccgoto('UnaryExpr', 86) ->
 2;
yeccgoto('UnaryExpr', 88) ->
 2;
yeccgoto('UnaryExpr', 89) ->
 2;
yeccgoto('UnaryExpr', 91) ->
 2;
yeccgoto('UnaryExpr', 92) ->
 2;
yeccgoto('UnaryExpr', 93) ->
 2;
yeccgoto('UnaryExpr', 94) ->
 2;
yeccgoto('UnaryExpr', 106) ->
 2;
yeccgoto('UnionExpr', 0) ->
 1;
yeccgoto('UnionExpr', 23) ->
 1;
yeccgoto('UnionExpr', 24) ->
 1;
yeccgoto('UnionExpr', 45) ->
 1;
yeccgoto('UnionExpr', 52) ->
 1;
yeccgoto('UnionExpr', 59) ->
 1;
yeccgoto('UnionExpr', 75) ->
 1;
yeccgoto('UnionExpr', 76) ->
 1;
yeccgoto('UnionExpr', 78) ->
 1;
yeccgoto('UnionExpr', 80) ->
 1;
yeccgoto('UnionExpr', 81) ->
 1;
yeccgoto('UnionExpr', 86) ->
 1;
yeccgoto('UnionExpr', 88) ->
 1;
yeccgoto('UnionExpr', 89) ->
 1;
yeccgoto('UnionExpr', 91) ->
 1;
yeccgoto('UnionExpr', 92) ->
 1;
yeccgoto('UnionExpr', 93) ->
 1;
yeccgoto('UnionExpr', 94) ->
 1;
yeccgoto('UnionExpr', 106) ->
 1;
yeccgoto(__Symbol, __State) ->
 erlang:error({yecc_bug,"1.1",{__Symbol, __State, missing_in_goto_table}}).

-compile({inline,{yeccpars2_4_,1}}).
-file("xmerl_xpath_parse.yrl", 101).
yeccpars2_4_([__1 | __Stack]) ->
 [begin
   { path , rel , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_9_,1}}).
-file("xmerl_xpath_parse.yrl", 127).
yeccpars2_9_([__1 | __Stack]) ->
 [begin
   { step , { child , __1 , [ ] } }
  end | __Stack].

-compile({inline,{yeccpars2_19_,1}}).
-file("xmerl_xpath_parse.yrl", 102).
yeccpars2_19_([__1 | __Stack]) ->
 [begin
   { path , abs , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_20_,1}}).
-file("xmerl_xpath_parse.yrl", 129).
yeccpars2_20_([__1 | __Stack]) ->
 [begin
   { abbrev_step , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_27_,1}}).
-file("xmerl_xpath_parse.yrl", 106).
yeccpars2_27_([__1 | __Stack]) ->
 [begin
   '/'
  end | __Stack].

-compile({inline,{yeccpars2_32_,1}}).
-file("xmerl_xpath_parse.yrl", 180).
yeccpars2_32_([__1 | __Stack]) ->
 [begin
   { literal , value ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_33_,1}}).
-file("xmerl_xpath_parse.yrl", 298).
yeccpars2_33_([__1 | __Stack]) ->
 [begin
   { name , value ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_35_,1}}).
-file("xmerl_xpath_parse.yrl", 181).
yeccpars2_35_([__1 | __Stack]) ->
 [begin
   { number , value ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_36_,1}}).
-file("xmerl_xpath_parse.yrl", 297).
yeccpars2_36_([__1 | __Stack]) ->
 [begin
   { prefix_test , value ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_38_,1}}).
-file("xmerl_xpath_parse.yrl", 178).
yeccpars2_38_([__1 | __Stack]) ->
 [begin
   { variable_reference , value ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_39_,1}}).
-file("xmerl_xpath_parse.yrl", 296).
yeccpars2_39_([__1 | __Stack]) ->
 [begin
   { wildcard , value ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_42_,1}}).
-file("xmerl_xpath_parse.yrl", 149).
yeccpars2_42_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { processing_instruction , value ( __3 ) }
  end | __Stack].

-compile({inline,{yeccpars2_44_,1}}).
-file("xmerl_xpath_parse.yrl", 147).
yeccpars2_44_([__3,__2,__1 | __Stack]) ->
 [begin
   { node_type , value ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_47_,1}}).
-file("xmerl_xpath_parse.yrl", 194).
yeccpars2_47_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_48_,1}}).
-file("xmerl_xpath_parse.yrl", 190).
yeccpars2_48_([__1 | __Stack]) ->
 [begin
   lists : reverse ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_50_,1}}).
-file("xmerl_xpath_parse.yrl", 186).
yeccpars2_50_([__3,__2,__1 | __Stack]) ->
 [begin
   { function_call , value ( __1 ) , [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_51_,1}}).
-file("xmerl_xpath_parse.yrl", 188).
yeccpars2_51_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { function_call , value ( __1 ) , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_53_,1}}).
-file("xmerl_xpath_parse.yrl", 193).
yeccpars2_53_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __3 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_55_,1}}).
-file("xmerl_xpath_parse.yrl", 119).
yeccpars2_55_([__3,__2,__1 | __Stack]) ->
 [begin
   { step , { value ( __1 ) , __3 , [ ] } }
  end | __Stack].

-compile({inline,{yeccpars2_56_,1}}).
-file("xmerl_xpath_parse.yrl", 137).
yeccpars2_56_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_57_,1}}).
-file("xmerl_xpath_parse.yrl", 132).
yeccpars2_57_([__1 | __Stack]) ->
 [begin
   lists : reverse ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_58_,1}}).
-file("xmerl_xpath_parse.yrl", 117).
yeccpars2_58_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { step , { value ( __1 ) , __3 , __4 } }
  end | __Stack].

-compile({inline,{yeccpars2_62_,1}}).
-file("xmerl_xpath_parse.yrl", 153).
yeccpars2_62_([__3,__2,__1 | __Stack]) ->
 [begin
   { pred , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_63_,1}}).
-file("xmerl_xpath_parse.yrl", 136).
yeccpars2_63_([__2,__1 | __Stack]) ->
 [begin
   [ __2 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_64_,1}}).
-file("xmerl_xpath_parse.yrl", 123).
yeccpars2_64_([__2,__1 | __Stack]) ->
 [begin
   { step , { attribute , __2 , [ ] } }
  end | __Stack].

-compile({inline,{yeccpars2_65_,1}}).
-file("xmerl_xpath_parse.yrl", 121).
yeccpars2_65_([__3,__2,__1 | __Stack]) ->
 [begin
   { step , { value ( __1 ) , __2 , __3 } }
  end | __Stack].

-compile({inline,{yeccpars2_66_,1}}).
-file("xmerl_xpath_parse.yrl", 160).
yeccpars2_66_([__2,__1 | __Stack]) ->
 [begin
   { '//' , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_69_,1}}).
-file("xmerl_xpath_parse.yrl", 164).
yeccpars2_69_([__3,__2,__1 | __Stack]) ->
 [begin
   { __1 , '//' , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_70_,1}}).
-file("xmerl_xpath_parse.yrl", 112).
yeccpars2_70_([__3,__2,__1 | __Stack]) ->
 [begin
   { refine , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_71_,1}}).
-file("xmerl_xpath_parse.yrl", 105).
yeccpars2_71_([__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_72_,1}}).
-file("xmerl_xpath_parse.yrl", 267).
yeccpars2_72_([__2,__1 | __Stack]) ->
 [begin
   { negative , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_74_,1}}).
-file("xmerl_xpath_parse.yrl", 179).
yeccpars2_74_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_77_,1}}).
-file("xmerl_xpath_parse.yrl", 252).
yeccpars2_77_([__3,__2,__1 | __Stack]) ->
 [begin
   { arith , '-' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_79_,1}}).
-file("xmerl_xpath_parse.yrl", 292).
yeccpars2_79_([__1 | __Stack]) ->
 [begin
   '*'
  end | __Stack].

-compile({inline,{yeccpars2_82_,1}}).
-file("xmerl_xpath_parse.yrl", 262).
yeccpars2_82_([__3,__2,__1 | __Stack]) ->
 [begin
   { arith , mod , __1 , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_83_,1}}).
-file("xmerl_xpath_parse.yrl", 260).
yeccpars2_83_([__3,__2,__1 | __Stack]) ->
 [begin
   { arith , 'div' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_84_,1}}).
-file("xmerl_xpath_parse.yrl", 258).
yeccpars2_84_([__3,__2,__1 | __Stack]) ->
 [begin
   { arith , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_85_,1}}).
-file("xmerl_xpath_parse.yrl", 250).
yeccpars2_85_([__3,__2,__1 | __Stack]) ->
 [begin
   { arith , '+' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_87_,1}}).
-file("xmerl_xpath_parse.yrl", 226).
yeccpars2_87_([__3,__2,__1 | __Stack]) ->
 [begin
   { bool , 'and' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_90_,1}}).
-file("xmerl_xpath_parse.yrl", 231).
yeccpars2_90_([__3,__2,__1 | __Stack]) ->
 [begin
   { comp , '=' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_95_,1}}).
-file("xmerl_xpath_parse.yrl", 244).
yeccpars2_95_([__3,__2,__1 | __Stack]) ->
 [begin
   { comp , '>=' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_96_,1}}).
-file("xmerl_xpath_parse.yrl", 240).
yeccpars2_96_([__3,__2,__1 | __Stack]) ->
 [begin
   { comp , '>' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_97_,1}}).
-file("xmerl_xpath_parse.yrl", 242).
yeccpars2_97_([__3,__2,__1 | __Stack]) ->
 [begin
   { comp , '<=' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_98_,1}}).
-file("xmerl_xpath_parse.yrl", 238).
yeccpars2_98_([__3,__2,__1 | __Stack]) ->
 [begin
   { comp , '<' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_99_,1}}).
-file("xmerl_xpath_parse.yrl", 233).
yeccpars2_99_([__3,__2,__1 | __Stack]) ->
 [begin
   { comp , '!=' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_100_,1}}).
-file("xmerl_xpath_parse.yrl", 214).
yeccpars2_100_([__2,__1 | __Stack]) ->
 [begin
   { path , filter , { __1 , __2 } }
  end | __Stack].

-compile({inline,{yeccpars2_103_,1}}).
-file("xmerl_xpath_parse.yrl", 210).
yeccpars2_103_([__3,__2,__1 | __Stack]) ->
 [begin
   { __1 , '//' , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_104_,1}}).
-file("xmerl_xpath_parse.yrl", 209).
yeccpars2_104_([__3,__2,__1 | __Stack]) ->
 [begin
   { refine , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_105_,1}}).
-file("xmerl_xpath_parse.yrl", 125).
yeccpars2_105_([__2,__1 | __Stack]) ->
 [begin
   { step , { child , __1 , __2 } }
  end | __Stack].

-compile({inline,{yeccpars2_107_,1}}).
-file("xmerl_xpath_parse.yrl", 220).
yeccpars2_107_([__3,__2,__1 | __Stack]) ->
 [begin
   { bool , 'or' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_109_,1}}).
-file("xmerl_xpath_parse.yrl", 203).
yeccpars2_109_([__3,__2,__1 | __Stack]) ->
 [begin
   { path , union , { __1 , __3 } }
  end | __Stack].


-file("xmerl_xpath_parse.yrl", 316).
