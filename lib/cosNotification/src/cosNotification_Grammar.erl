-module(cosNotification_Grammar).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("cosNotification_Grammar.yrl", 132).
%%----------------------------------------------------------------------
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
%% File    : cosNotification_Grammar.erl
%% Purpose : THIS FILE HAS BEEN GENERATED. DO NOT EDIT!!!!
%%----------------------------------------------------------------------

-include("CosNotification_Definitions.hrl").

create_unary('+', Val) when number(Val) -> Val;
create_unary('-', Val) when number(Val) -> -Val;
create_unary(_, _) -> return_error(0, "syntax error").

examin_comp({T, []}) ->
	{T, '$empty'};
examin_comp(V) ->
	V.


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



-file("./cosNotification_Grammar.erl", 132).

yeccpars2(0, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'ADDOP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, bslsh, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, default, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, dollar, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, exist, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, ident, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, int, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, num, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_0_(__Stack),
 yeccpars2(1, __Cat, [0 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(1, '$end', _, __Stack, _, _, _) ->
 {ok, hd(__Stack)};
yeccpars2(1, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(2, 'MULOP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 84, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<expr>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(3, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<term>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(4, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<factor_not>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(5, in, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 88, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<expr_in>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(6, 'RELOP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 86, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<bool_compare>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(7, 'ADDOP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 80, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 81, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<expr_twiddle>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(8, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<toplevel>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(9, 'or', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<bool>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(10, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<bool_and>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(11, 'and', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<bool_or>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(12, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<constraint>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(13, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_13_(__Stack),
 yeccpars2(yeccgoto('<factor>', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(14, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'ADDOP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, bslsh, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, default, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, dollar, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, exist, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, ident, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, int, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, num, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(15, int, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, num, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(16, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_16_(__Stack),
 yeccpars2(yeccgoto('<factor>', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(17, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_17_(__Stack),
 yeccpars2(yeccgoto('<factor>', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(18, ident, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(19, dollar, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [19 | __Ss], [__T | __Stack]);
yeccpars2(19, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(20, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, bslsh, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, ident, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_20_(__Stack),
 yeccpars2(68, __Cat, [20 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(21, dollar, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [21 | __Ss], [__T | __Stack]);
yeccpars2(21, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(22, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_22_(__Stack),
 yeccpars2(yeccgoto('<Ident>', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(23, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_23_(__Stack),
 yeccpars2(yeccgoto('<factor>', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(24, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, 'ADDOP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, bslsh, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, default, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, dollar, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, exist, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, ident, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, int, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, num, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(25, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_25_(__Stack),
 yeccpars2(yeccgoto('<factor>', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(26, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_26_(__Stack),
 yeccpars2(yeccgoto('<factor>', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(27, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_27_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<factor_not>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(28, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, bslsh, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, ident, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_28_(__Stack),
 yeccpars2(30, __Cat, [28 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(29, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_29_(__Stack),
 yeccpars2(67, __Cat, [29 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(30, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_30_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<factor>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(31, bslsh, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, ident, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(32, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, '_d', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, '_length', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, '_repos_id', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, '_type_id', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, bslsh, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, ident, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, int, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(33, int, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(34, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(35, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_35_(__Stack),
 yeccpars2(36, __Cat, [35 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(36, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_36_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('<Component>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(37, bslsh, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, ident, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(38, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [38 | __Ss], [__T | __Stack]);
yeccpars2(38, '_d', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [38 | __Ss], [__T | __Stack]);
yeccpars2(38, '_length', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [38 | __Ss], [__T | __Stack]);
yeccpars2(38, '_repos_id', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [38 | __Ss], [__T | __Stack]);
yeccpars2(38, '_type_id', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [38 | __Ss], [__T | __Stack]);
yeccpars2(38, bslsh, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [38 | __Ss], [__T | __Stack]);
yeccpars2(38, ident, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [38 | __Ss], [__T | __Stack]);
yeccpars2(38, int, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [38 | __Ss], [__T | __Stack]);
yeccpars2(38, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(39, int, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(40, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(41, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_41_(__Stack),
 yeccpars2(42, __Cat, [41 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(42, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_42_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('<CompExt>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(43, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [43 | __Ss], [__T | __Stack]);
yeccpars2(43, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [43 | __Ss], [__T | __Stack]);
yeccpars2(43, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [43 | __Ss], [__T | __Stack]);
yeccpars2(43, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_43_(__Stack),
 yeccpars2(59, __Cat, [43 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(44, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_44_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<CompExt>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(45, 'ADDOP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, int, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_45_(__Stack),
 yeccpars2(52, __Cat, [45 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(46, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_46_(__Stack),
 yeccpars2(yeccgoto('<CompDot>', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(47, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_47_(__Stack),
 yeccpars2(yeccgoto('<CompDot>', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(48, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_48_(__Stack),
 yeccpars2(yeccgoto('<CompDot>', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(49, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_49_(__Stack),
 yeccpars2(yeccgoto('<CompDot>', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(50, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_50_(__Stack),
 yeccpars2(51, __Cat, [50 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(51, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_51_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<CompDot>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(52, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(53, int, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(54, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_54_(__Stack),
 yeccpars2(yeccgoto('<UnionVal>', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(55, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_55_(__Stack),
 yeccpars2(yeccgoto('<UnionVal>', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(56, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_56_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<UnionVal>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(57, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [57 | __Ss], [__T | __Stack]);
yeccpars2(57, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [57 | __Ss], [__T | __Stack]);
yeccpars2(57, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [57 | __Ss], [__T | __Stack]);
yeccpars2(57, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_57_(__Stack),
 yeccpars2(58, __Cat, [57 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(58, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_58_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('<CompDot>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(59, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_59_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<CompDot>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(60, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(61, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [61 | __Ss], [__T | __Stack]);
yeccpars2(61, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [61 | __Ss], [__T | __Stack]);
yeccpars2(61, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [61 | __Ss], [__T | __Stack]);
yeccpars2(61, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_61_(__Stack),
 yeccpars2(62, __Cat, [61 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(62, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_62_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('<CompExt>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(63, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_63_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<Component>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(64, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(65, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_65_(__Stack),
 yeccpars2(66, __Cat, [65 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(66, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_66_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('<Component>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(67, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_67_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<Component>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(68, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_68_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<factor>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(69, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [69 | __Ss], [__T | __Stack]);
yeccpars2(69, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [69 | __Ss], [__T | __Stack]);
yeccpars2(69, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [69 | __Ss], [__T | __Stack]);
yeccpars2(69, bslsh, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [69 | __Ss], [__T | __Stack]);
yeccpars2(69, ident, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [69 | __Ss], [__T | __Stack]);
yeccpars2(69, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_69_(__Stack),
 yeccpars2(70, __Cat, [69 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(70, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_70_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<factor>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(71, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_71_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<Ident>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(72, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_72_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<factor>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(73, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_73_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<factor>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(74, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, 'or', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(75, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_75_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<factor>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(76, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, 'ADDOP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, bslsh, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, default, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, dollar, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, exist, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, ident, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, int, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, num, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(77, 'and', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [77 | __Ss], [__T | __Stack]);
yeccpars2(77, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_77_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<bool_or>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(78, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'ADDOP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, bslsh, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, default, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, dollar, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, exist, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, ident, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, int, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, num, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(79, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_79_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<bool_and>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(80, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, 'ADDOP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, bslsh, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, default, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, dollar, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, exist, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, ident, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, int, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, num, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(81, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'ADDOP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, bslsh, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, default, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, dollar, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, exist, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, ident, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, int, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, num, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [81 | __Ss], [__T | __Stack]);
yeccpars2(81, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(82, 'ADDOP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 80, [82 | __Ss], [__T | __Stack]);
yeccpars2(82, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_82_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<expr_twiddle>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(83, 'MULOP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 84, [83 | __Ss], [__T | __Stack]);
yeccpars2(83, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_83_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<expr>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(84, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'ADDOP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, bslsh, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, default, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, dollar, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, exist, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, ident, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, int, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, num, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [84 | __Ss], [__T | __Stack]);
yeccpars2(84, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(85, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_85_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<term>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(86, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, 'ADDOP', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, bslsh, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, default, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, dollar, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, exist, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, ident, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, int, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, 'not', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, num, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(87, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_87_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<bool_compare>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(88, bslsh, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, dollar, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 90, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, ident, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(89, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_89_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<expr_in>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(90, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, bslsh, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, ident, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_90_(__Stack),
 yeccpars2(91, __Cat, [90 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(91, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_91_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('<expr_in>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(__Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.1",{missing_state_in_action_table, __Other}}).

yeccgoto('<CompDot>', 32) ->
 63;
yeccgoto('<CompDot>', 38) ->
 44;
yeccgoto('<CompExt>', 29) ->
 67;
yeccgoto('<CompExt>', 35) ->
 36;
yeccgoto('<CompExt>', 41) ->
 42;
yeccgoto('<CompExt>', 43) ->
 59;
yeccgoto('<CompExt>', 50) ->
 51;
yeccgoto('<CompExt>', 57) ->
 58;
yeccgoto('<CompExt>', 61) ->
 62;
yeccgoto('<CompExt>', 65) ->
 66;
yeccgoto('<Component>', 20) ->
 68;
yeccgoto('<Component>', 28) ->
 30;
yeccgoto('<Component>', 69) ->
 70;
yeccgoto('<Component>', 90) ->
 91;
yeccgoto('<Ident>', 0) ->
 13;
yeccgoto('<Ident>', 14) ->
 13;
yeccgoto('<Ident>', 20) ->
 29;
yeccgoto('<Ident>', 24) ->
 13;
yeccgoto('<Ident>', 28) ->
 29;
yeccgoto('<Ident>', 31) ->
 64;
yeccgoto('<Ident>', 32) ->
 43;
yeccgoto('<Ident>', 37) ->
 60;
yeccgoto('<Ident>', 38) ->
 43;
yeccgoto('<Ident>', 69) ->
 29;
yeccgoto('<Ident>', 76) ->
 13;
yeccgoto('<Ident>', 78) ->
 13;
yeccgoto('<Ident>', 80) ->
 13;
yeccgoto('<Ident>', 81) ->
 13;
yeccgoto('<Ident>', 84) ->
 13;
yeccgoto('<Ident>', 86) ->
 13;
yeccgoto('<Ident>', 88) ->
 89;
yeccgoto('<Ident>', 90) ->
 29;
yeccgoto('<UnionVal>', 45) ->
 52;
yeccgoto('<bool>', 0) ->
 12;
yeccgoto('<bool_and>', 0) ->
 11;
yeccgoto('<bool_and>', 14) ->
 11;
yeccgoto('<bool_and>', 76) ->
 77;
yeccgoto('<bool_compare>', 0) ->
 10;
yeccgoto('<bool_compare>', 14) ->
 10;
yeccgoto('<bool_compare>', 76) ->
 10;
yeccgoto('<bool_compare>', 78) ->
 79;
yeccgoto('<bool_or>', 0) ->
 9;
yeccgoto('<bool_or>', 14) ->
 74;
yeccgoto('<constraint>', 0) ->
 8;
yeccgoto('<expr>', 0) ->
 7;
yeccgoto('<expr>', 14) ->
 7;
yeccgoto('<expr>', 76) ->
 7;
yeccgoto('<expr>', 78) ->
 7;
yeccgoto('<expr>', 81) ->
 82;
yeccgoto('<expr>', 86) ->
 7;
yeccgoto('<expr_in>', 0) ->
 6;
yeccgoto('<expr_in>', 14) ->
 6;
yeccgoto('<expr_in>', 76) ->
 6;
yeccgoto('<expr_in>', 78) ->
 6;
yeccgoto('<expr_in>', 86) ->
 87;
yeccgoto('<expr_twiddle>', 0) ->
 5;
yeccgoto('<expr_twiddle>', 14) ->
 5;
yeccgoto('<expr_twiddle>', 76) ->
 5;
yeccgoto('<expr_twiddle>', 78) ->
 5;
yeccgoto('<expr_twiddle>', 86) ->
 5;
yeccgoto('<factor>', 0) ->
 4;
yeccgoto('<factor>', 14) ->
 4;
yeccgoto('<factor>', 24) ->
 27;
yeccgoto('<factor>', 76) ->
 4;
yeccgoto('<factor>', 78) ->
 4;
yeccgoto('<factor>', 80) ->
 4;
yeccgoto('<factor>', 81) ->
 4;
yeccgoto('<factor>', 84) ->
 4;
yeccgoto('<factor>', 86) ->
 4;
yeccgoto('<factor_not>', 0) ->
 3;
yeccgoto('<factor_not>', 14) ->
 3;
yeccgoto('<factor_not>', 76) ->
 3;
yeccgoto('<factor_not>', 78) ->
 3;
yeccgoto('<factor_not>', 80) ->
 3;
yeccgoto('<factor_not>', 81) ->
 3;
yeccgoto('<factor_not>', 84) ->
 85;
yeccgoto('<factor_not>', 86) ->
 3;
yeccgoto('<term>', 0) ->
 2;
yeccgoto('<term>', 14) ->
 2;
yeccgoto('<term>', 76) ->
 2;
yeccgoto('<term>', 78) ->
 2;
yeccgoto('<term>', 80) ->
 83;
yeccgoto('<term>', 81) ->
 2;
yeccgoto('<term>', 86) ->
 2;
yeccgoto('<toplevel>', 0) ->
 1;
yeccgoto(__Symbol, __State) ->
 erlang:error({yecc_bug,"1.1",{__Symbol, __State, missing_in_goto_table}}).

-compile({inline,{yeccpars2_0_,1}}).
-file("cosNotification_Grammar.yrl", 55).
yeccpars2_0_(__Stack) ->
 [begin
   '$empty'
  end | __Stack].

-compile({inline,{yeccpars2_13_,1}}).
-file("cosNotification_Grammar.yrl", 95).
yeccpars2_13_([__1 | __Stack]) ->
 [begin
   list_to_atom ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_16_,1}}).
-file("cosNotification_Grammar.yrl", 92).
yeccpars2_16_([__1 | __Stack]) ->
 [begin
   false
  end | __Stack].

-compile({inline,{yeccpars2_17_,1}}).
-file("cosNotification_Grammar.yrl", 91).
yeccpars2_17_([__1 | __Stack]) ->
 [begin
   true
  end | __Stack].

-compile({inline,{yeccpars2_20_,1}}).
-file("cosNotification_Grammar.yrl", 106).
yeccpars2_20_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_22_,1}}).
-file("cosNotification_Grammar.yrl", 121).
yeccpars2_22_([__1 | __Stack]) ->
 [begin
   element ( 2 , __1 )
  end | __Stack].

-compile({inline,{yeccpars2_23_,1}}).
-file("cosNotification_Grammar.yrl", 89).
yeccpars2_23_([__1 | __Stack]) ->
 [begin
   element ( 2 , __1 )
  end | __Stack].

-compile({inline,{yeccpars2_25_,1}}).
-file("cosNotification_Grammar.yrl", 88).
yeccpars2_25_([__1 | __Stack]) ->
 [begin
   element ( 2 , __1 )
  end | __Stack].

-compile({inline,{yeccpars2_26_,1}}).
-file("cosNotification_Grammar.yrl", 90).
yeccpars2_26_([__1 | __Stack]) ->
 [begin
   element ( 2 , __1 )
  end | __Stack].

-compile({inline,{yeccpars2_27_,1}}).
-file("cosNotification_Grammar.yrl", 85).
yeccpars2_27_([__2,__1 | __Stack]) ->
 [begin
   { 'not' , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_28_,1}}).
-file("cosNotification_Grammar.yrl", 106).
yeccpars2_28_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_29_,1}}).
-file("cosNotification_Grammar.yrl", 111).
yeccpars2_29_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_30_,1}}).
-file("cosNotification_Grammar.yrl", 98).
yeccpars2_30_([__3,__2,__1 | __Stack]) ->
 [begin
   examin_comp ( { exist_component , __3 } )
  end | __Stack].

-compile({inline,{yeccpars2_35_,1}}).
-file("cosNotification_Grammar.yrl", 111).
yeccpars2_35_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_36_,1}}).
-file("cosNotification_Grammar.yrl", 103).
yeccpars2_36_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   [ { arrindex , element ( 2 , __2 ) } | __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_41_,1}}).
-file("cosNotification_Grammar.yrl", 111).
yeccpars2_41_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_42_,1}}).
-file("cosNotification_Grammar.yrl", 109).
yeccpars2_42_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   [ { arrindex , element ( 2 , __2 ) } | __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_43_,1}}).
-file("cosNotification_Grammar.yrl", 111).
yeccpars2_43_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_44_,1}}).
-file("cosNotification_Grammar.yrl", 108).
yeccpars2_44_([__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_45_,1}}).
-file("cosNotification_Grammar.yrl", 127).
yeccpars2_45_(__Stack) ->
 [begin
   default
  end | __Stack].

-compile({inline,{yeccpars2_46_,1}}).
-file("cosNotification_Grammar.yrl", 117).
yeccpars2_46_([__1 | __Stack]) ->
 [begin
   [ '_d' ]
  end | __Stack].

-compile({inline,{yeccpars2_47_,1}}).
-file("cosNotification_Grammar.yrl", 116).
yeccpars2_47_([__1 | __Stack]) ->
 [begin
   [ '_length' ]
  end | __Stack].

-compile({inline,{yeccpars2_48_,1}}).
-file("cosNotification_Grammar.yrl", 119).
yeccpars2_48_([__1 | __Stack]) ->
 [begin
   [ '_repos_id' ]
  end | __Stack].

-compile({inline,{yeccpars2_49_,1}}).
-file("cosNotification_Grammar.yrl", 118).
yeccpars2_49_([__1 | __Stack]) ->
 [begin
   [ '_type_id' ]
  end | __Stack].

-compile({inline,{yeccpars2_50_,1}}).
-file("cosNotification_Grammar.yrl", 111).
yeccpars2_50_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_51_,1}}).
-file("cosNotification_Grammar.yrl", 114).
yeccpars2_51_([__2,__1 | __Stack]) ->
 [begin
   [ { dotint , element ( 2 , __1 ) } | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_54_,1}}).
-file("cosNotification_Grammar.yrl", 124).
yeccpars2_54_([__1 | __Stack]) ->
 [begin
   { uint , element ( 2 , __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_55_,1}}).
-file("cosNotification_Grammar.yrl", 126).
yeccpars2_55_([__1 | __Stack]) ->
 [begin
   { ustr , element ( 2 , __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_56_,1}}).
-file("cosNotification_Grammar.yrl", 125).
yeccpars2_56_([__2,__1 | __Stack]) ->
 [begin
   { uint , create_unary ( element ( 2 , __1 ) , element ( 2 , __2 ) ) }
  end | __Stack].

-compile({inline,{yeccpars2_57_,1}}).
-file("cosNotification_Grammar.yrl", 111).
yeccpars2_57_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_58_,1}}).
-file("cosNotification_Grammar.yrl", 115).
yeccpars2_58_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   [ __2 | __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_59_,1}}).
-file("cosNotification_Grammar.yrl", 113).
yeccpars2_59_([__2,__1 | __Stack]) ->
 [begin
   [ { dotid , __1 } | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_61_,1}}).
-file("cosNotification_Grammar.yrl", 111).
yeccpars2_61_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_62_,1}}).
-file("cosNotification_Grammar.yrl", 110).
yeccpars2_62_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   [ { associd , __2 } | __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_63_,1}}).
-file("cosNotification_Grammar.yrl", 102).
yeccpars2_63_([__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_65_,1}}).
-file("cosNotification_Grammar.yrl", 111).
yeccpars2_65_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_66_,1}}).
-file("cosNotification_Grammar.yrl", 104).
yeccpars2_66_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   [ { associd , __2 } | __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_67_,1}}).
-file("cosNotification_Grammar.yrl", 105).
yeccpars2_67_([__2,__1 | __Stack]) ->
 [begin
   [ { varid , __1 } | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_68_,1}}).
-file("cosNotification_Grammar.yrl", 96).
yeccpars2_68_([__2,__1 | __Stack]) ->
 [begin
   examin_comp ( { component , __2 } )
  end | __Stack].

-compile({inline,{yeccpars2_69_,1}}).
-file("cosNotification_Grammar.yrl", 106).
yeccpars2_69_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_70_,1}}).
-file("cosNotification_Grammar.yrl", 97).
yeccpars2_70_([__3,__2,__1 | __Stack]) ->
 [begin
   examin_comp ( { default_component , __3 } )
  end | __Stack].

-compile({inline,{yeccpars2_71_,1}}).
-file("cosNotification_Grammar.yrl", 122).
yeccpars2_71_([__2,__1 | __Stack]) ->
 [begin
   element ( 2 , __2 )
  end | __Stack].

-compile({inline,{yeccpars2_72_,1}}).
-file("cosNotification_Grammar.yrl", 94).
yeccpars2_72_([__2,__1 | __Stack]) ->
 [begin
   create_unary ( element ( 2 , __1 ) , element ( 2 , __2 ) )
  end | __Stack].

-compile({inline,{yeccpars2_73_,1}}).
-file("cosNotification_Grammar.yrl", 93).
yeccpars2_73_([__2,__1 | __Stack]) ->
 [begin
   create_unary ( element ( 2 , __1 ) , element ( 2 , __2 ) )
  end | __Stack].

-compile({inline,{yeccpars2_75_,1}}).
-file("cosNotification_Grammar.yrl", 87).
yeccpars2_75_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_77_,1}}).
-file("cosNotification_Grammar.yrl", 62).
yeccpars2_77_([__3,__2,__1 | __Stack]) ->
 [begin
   { 'or' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_79_,1}}).
-file("cosNotification_Grammar.yrl", 65).
yeccpars2_79_([__3,__2,__1 | __Stack]) ->
 [begin
   { 'and' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_82_,1}}).
-file("cosNotification_Grammar.yrl", 76).
yeccpars2_82_([__3,__2,__1 | __Stack]) ->
 [begin
   { '~' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_83_,1}}).
-file("cosNotification_Grammar.yrl", 79).
yeccpars2_83_([__3,__2,__1 | __Stack]) ->
 [begin
   { element ( 2 , __2 ) , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_85_,1}}).
-file("cosNotification_Grammar.yrl", 82).
yeccpars2_85_([__3,__2,__1 | __Stack]) ->
 [begin
   { element ( 2 , __2 ) , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_87_,1}}).
-file("cosNotification_Grammar.yrl", 68).
yeccpars2_87_([__3,__2,__1 | __Stack]) ->
 [begin
   { element ( 2 , __2 ) , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_89_,1}}).
-file("cosNotification_Grammar.yrl", 72).
yeccpars2_89_([__3,__2,__1 | __Stack]) ->
 [begin
   { in , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_90_,1}}).
-file("cosNotification_Grammar.yrl", 106).
yeccpars2_90_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_91_,1}}).
-file("cosNotification_Grammar.yrl", 73).
yeccpars2_91_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { in , __1 , examin_comp ( { component , __4 } ) }
  end | __Stack].


-file("cosNotification_Grammar.yrl", 167).
