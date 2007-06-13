-module(core_parse).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("core_parse.yrl", 372).

-export([abstract/1,abstract/2,normalise/1]).

%% The following directive is needed for (significantly) faster compilation
%% of the generated .erl file by the HiPE compiler.  Please do not remove.
-compile([{hipe,[{regalloc,linear_scan}]}]).

-include("core_parse.hrl").

tok_val(T) -> element(3, T).
tok_line(T) -> element(2, T).

abstract(T, _N) -> abstract(T).

abstract(Term) -> core_lib:make_literal(Term).

normalise(Core) -> core_lib:literal_value(Core).

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



-file("./core_parse.erl", 115).

yeccpars2(0, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 2, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, module, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(1, '$end', _, __Stack, _, _, _) ->
 {ok, hd(__Stack)};
yeccpars2(1, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(2, module, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 315, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(3, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(4, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(5, attributes, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(6, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 10, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(7, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(exported_name, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(8, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [8 | __Ss], [__T | __Stack]);
yeccpars2(8, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(9, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 14, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_9_(__Stack),
 yeccpars2(yeccgoto(exported_names, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(10, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_10_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(module_export, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(11, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(12, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(13, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_13_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(function_name, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(14, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(15, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_15_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(exported_names, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(16, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_16_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(module_export, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(17, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_17_(__Stack),
 yeccpars2(58, __Cat, [17 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(18, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(19, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [19 | __Ss], [__T | __Stack]);
yeccpars2(19, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [19 | __Ss], [__T | __Stack]);
yeccpars2(19, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(20, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(21, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [21 | __Ss], [__T | __Stack]);
yeccpars2(21, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_21_(__Stack),
 yeccpars2(yeccgoto(attribute_list, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(22, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_22_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(module_attribute, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(23, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(24, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(25, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(literal, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(26, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_26_(__Stack),
 yeccpars2(yeccgoto(atomic_literal, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(27, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_27_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(attribute, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(28, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(literal, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(29, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(literal, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(30, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [30 | __Ss], [__T | __Stack]);
yeccpars2(30, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [30 | __Ss], [__T | __Stack]);
yeccpars2(30, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [30 | __Ss], [__T | __Stack]);
yeccpars2(30, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [30 | __Ss], [__T | __Stack]);
yeccpars2(30, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [30 | __Ss], [__T | __Stack]);
yeccpars2(30, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [30 | __Ss], [__T | __Stack]);
yeccpars2(30, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [30 | __Ss], [__T | __Stack]);
yeccpars2(30, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [30 | __Ss], [__T | __Stack]);
yeccpars2(30, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(31, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_31_(__Stack),
 yeccpars2(yeccgoto(atomic_literal, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(32, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_32_(__Stack),
 yeccpars2(yeccgoto(atomic_literal, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(33, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_33_(__Stack),
 yeccpars2(yeccgoto(atomic_literal, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(34, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_34_(__Stack),
 yeccpars2(yeccgoto(atomic_literal, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(35, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_35_(__Stack),
 yeccpars2(yeccgoto(atomic_literal, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(36, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 39, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(37, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(38, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [38 | __Ss], [__T | __Stack]);
yeccpars2(38, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_38_(__Stack),
 yeccpars2(yeccgoto(literals, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(39, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_39_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(tuple_literal, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(40, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(41, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_41_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(literals, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(42, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_42_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tuple_literal, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(43, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [43 | __Ss], [__T | __Stack]);
yeccpars2(43, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [43 | __Ss], [__T | __Stack]);
yeccpars2(43, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [43 | __Ss], [__T | __Stack]);
yeccpars2(43, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(44, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_44_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(nil, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(45, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_45_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(cons_literal, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(46, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(47, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_47_(__Stack),
 yeccpars2(yeccgoto(tail_literal, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(48, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(49, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [49 | __Ss], [__T | __Stack]);
yeccpars2(49, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(50, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_50_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tail_literal, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(51, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(52, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_52_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tail_literal, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(53, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [53 | __Ss], [__T | __Stack]);
yeccpars2(53, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(54, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_54_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(attribute_list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(55, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_55_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(module_attribute, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(56, 'end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 314, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(57, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(anno_function_name, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(58, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(module_defs, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(59, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_59_(__Stack),
 yeccpars2(313, __Cat, [59 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(60, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 96, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(61, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [61 | __Ss], [__T | __Stack]);
yeccpars2(61, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(62, '-|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 63, [62 | __Ss], [__T | __Stack]);
yeccpars2(62, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(63, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [63 | __Ss], [__T | __Stack]);
yeccpars2(63, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(64, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 95, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(65, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 79, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(66, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(constant, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(67, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_67_(__Stack),
 yeccpars2(yeccgoto(atomic_constant, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(68, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 94, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(69, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 92, [69 | __Ss], [__T | __Stack]);
yeccpars2(69, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_69_(__Stack),
 yeccpars2(yeccgoto(constants, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(70, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(constant, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(71, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(constant, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(72, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 79, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(73, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_73_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(annotation, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(74, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_74_(__Stack),
 yeccpars2(yeccgoto(atomic_constant, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(75, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_75_(__Stack),
 yeccpars2(yeccgoto(atomic_constant, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(76, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_76_(__Stack),
 yeccpars2(yeccgoto(atomic_constant, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(77, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_77_(__Stack),
 yeccpars2(yeccgoto(atomic_constant, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(78, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_78_(__Stack),
 yeccpars2(yeccgoto(atomic_constant, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(79, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 79, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 81, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(80, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 82, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(81, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_81_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(tuple_constant, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(82, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_82_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tuple_constant, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(83, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [83 | __Ss], [__T | __Stack]);
yeccpars2(83, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 86, [83 | __Ss], [__T | __Stack]);
yeccpars2(83, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 87, [83 | __Ss], [__T | __Stack]);
yeccpars2(83, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(84, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_84_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(cons_constant, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(85, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 79, [85 | __Ss], [__T | __Stack]);
yeccpars2(85, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(86, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_86_(__Stack),
 yeccpars2(yeccgoto(tail_constant, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(87, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 79, [87 | __Ss], [__T | __Stack]);
yeccpars2(87, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(88, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 89, [88 | __Ss], [__T | __Stack]);
yeccpars2(88, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(89, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_89_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tail_constant, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(90, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 85, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 86, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 87, [90 | __Ss], [__T | __Stack]);
yeccpars2(90, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(91, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_91_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tail_constant, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(92, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 79, [92 | __Ss], [__T | __Stack]);
yeccpars2(92, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(93, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_93_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(constants, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(94, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_94_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(annotation, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(95, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_95_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(anno_function_name, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(96, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [96 | __Ss], [__T | __Stack]);
yeccpars2(96, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(97, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(anno_fun, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(98, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_98_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(function_definition, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(99, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [99 | __Ss], [__T | __Stack]);
yeccpars2(99, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(100, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [100 | __Ss], [__T | __Stack]);
yeccpars2(100, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(101, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 106, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(102, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(anno_variable, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(103, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 306, [103 | __Ss], [__T | __Stack]);
yeccpars2(103, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(104, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 304, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_104_(__Stack),
 yeccpars2(yeccgoto(anno_variables, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(105, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [105 | __Ss], [__T | __Stack]);
yeccpars2(105, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(106, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 108, [106 | __Ss], [__T | __Stack]);
yeccpars2(106, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(107, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_107_(__Stack),
 yeccpars2(yeccgoto(variable, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(108, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(109, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(110, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(111, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(112, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expression, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(113, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(114, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(115, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(116, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(117, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(118, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(119, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(120, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(anno_expression, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(121, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(122, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(123, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(124, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(125, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(126, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(127, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(single_expression, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(128, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_128_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(fun_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(129, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 287, [129 | __Ss], [__T | __Stack]);
yeccpars2(129, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(130, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [130 | __Ss], [__T | __Stack]);
yeccpars2(130, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(131, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [131 | __Ss], [__T | __Stack]);
yeccpars2(131, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [131 | __Ss], [__T | __Stack]);
yeccpars2(131, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [131 | __Ss], [__T | __Stack]);
yeccpars2(131, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 281, [131 | __Ss], [__T | __Stack]);
yeccpars2(131, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [131 | __Ss], [__T | __Stack]);
yeccpars2(131, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [131 | __Ss], [__T | __Stack]);
yeccpars2(131, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [131 | __Ss], [__T | __Stack]);
yeccpars2(131, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [131 | __Ss], [__T | __Stack]);
yeccpars2(131, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [131 | __Ss], [__T | __Stack]);
yeccpars2(131, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [131 | __Ss], [__T | __Stack]);
yeccpars2(131, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [131 | __Ss], [__T | __Stack]);
yeccpars2(131, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [131 | __Ss], [__T | __Stack]);
yeccpars2(131, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [131 | __Ss], [__T | __Stack]);
yeccpars2(131, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [131 | __Ss], [__T | __Stack]);
yeccpars2(131, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [131 | __Ss], [__T | __Stack]);
yeccpars2(131, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [131 | __Ss], [__T | __Stack]);
yeccpars2(131, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [131 | __Ss], [__T | __Stack]);
yeccpars2(131, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [131 | __Ss], [__T | __Stack]);
yeccpars2(131, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [131 | __Ss], [__T | __Stack]);
yeccpars2(131, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [131 | __Ss], [__T | __Stack]);
yeccpars2(131, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [131 | __Ss], [__T | __Stack]);
yeccpars2(131, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [131 | __Ss], [__T | __Stack]);
yeccpars2(131, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [131 | __Ss], [__T | __Stack]);
yeccpars2(131, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(132, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [132 | __Ss], [__T | __Stack]);
yeccpars2(132, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [132 | __Ss], [__T | __Stack]);
yeccpars2(132, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [132 | __Ss], [__T | __Stack]);
yeccpars2(132, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [132 | __Ss], [__T | __Stack]);
yeccpars2(132, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [132 | __Ss], [__T | __Stack]);
yeccpars2(132, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [132 | __Ss], [__T | __Stack]);
yeccpars2(132, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [132 | __Ss], [__T | __Stack]);
yeccpars2(132, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [132 | __Ss], [__T | __Stack]);
yeccpars2(132, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [132 | __Ss], [__T | __Stack]);
yeccpars2(132, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [132 | __Ss], [__T | __Stack]);
yeccpars2(132, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [132 | __Ss], [__T | __Stack]);
yeccpars2(132, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [132 | __Ss], [__T | __Stack]);
yeccpars2(132, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [132 | __Ss], [__T | __Stack]);
yeccpars2(132, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [132 | __Ss], [__T | __Stack]);
yeccpars2(132, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [132 | __Ss], [__T | __Stack]);
yeccpars2(132, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [132 | __Ss], [__T | __Stack]);
yeccpars2(132, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [132 | __Ss], [__T | __Stack]);
yeccpars2(132, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [132 | __Ss], [__T | __Stack]);
yeccpars2(132, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [132 | __Ss], [__T | __Stack]);
yeccpars2(132, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [132 | __Ss], [__T | __Stack]);
yeccpars2(132, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [132 | __Ss], [__T | __Stack]);
yeccpars2(132, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [132 | __Ss], [__T | __Stack]);
yeccpars2(132, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [132 | __Ss], [__T | __Stack]);
yeccpars2(132, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(133, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [133 | __Ss], [__T | __Stack]);
yeccpars2(133, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(134, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [134 | __Ss], [__T | __Stack]);
yeccpars2(134, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_134_(__Stack),
 yeccpars2(yeccgoto(atomic_literal, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(135, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [135 | __Ss], [__T | __Stack]);
yeccpars2(135, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(136, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [136 | __Ss], [__T | __Stack]);
yeccpars2(136, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(137, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [137 | __Ss], [__T | __Stack]);
yeccpars2(137, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [137 | __Ss], [__T | __Stack]);
yeccpars2(137, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [137 | __Ss], [__T | __Stack]);
yeccpars2(137, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [137 | __Ss], [__T | __Stack]);
yeccpars2(137, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [137 | __Ss], [__T | __Stack]);
yeccpars2(137, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [137 | __Ss], [__T | __Stack]);
yeccpars2(137, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [137 | __Ss], [__T | __Stack]);
yeccpars2(137, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [137 | __Ss], [__T | __Stack]);
yeccpars2(137, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [137 | __Ss], [__T | __Stack]);
yeccpars2(137, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [137 | __Ss], [__T | __Stack]);
yeccpars2(137, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [137 | __Ss], [__T | __Stack]);
yeccpars2(137, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [137 | __Ss], [__T | __Stack]);
yeccpars2(137, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [137 | __Ss], [__T | __Stack]);
yeccpars2(137, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [137 | __Ss], [__T | __Stack]);
yeccpars2(137, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [137 | __Ss], [__T | __Stack]);
yeccpars2(137, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [137 | __Ss], [__T | __Stack]);
yeccpars2(137, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [137 | __Ss], [__T | __Stack]);
yeccpars2(137, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [137 | __Ss], [__T | __Stack]);
yeccpars2(137, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [137 | __Ss], [__T | __Stack]);
yeccpars2(137, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [137 | __Ss], [__T | __Stack]);
yeccpars2(137, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [137 | __Ss], [__T | __Stack]);
yeccpars2(137, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [137 | __Ss], [__T | __Stack]);
yeccpars2(137, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(138, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(139, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [139 | __Ss], [__T | __Stack]);
yeccpars2(139, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 155, [139 | __Ss], [__T | __Stack]);
yeccpars2(139, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [139 | __Ss], [__T | __Stack]);
yeccpars2(139, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(140, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [140 | __Ss], [__T | __Stack]);
yeccpars2(140, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [140 | __Ss], [__T | __Stack]);
yeccpars2(140, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_140_(__Stack),
 yeccpars2(250, __Cat, [140 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(141, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 244, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(142, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 178, [142 | __Ss], [__T | __Stack]);
yeccpars2(142, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 179, [142 | __Ss], [__T | __Stack]);
yeccpars2(142, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 180, [142 | __Ss], [__T | __Stack]);
yeccpars2(142, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 181, [142 | __Ss], [__T | __Stack]);
yeccpars2(142, 'after', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 182, [142 | __Ss], [__T | __Stack]);
yeccpars2(142, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [142 | __Ss], [__T | __Stack]);
yeccpars2(142, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [142 | __Ss], [__T | __Stack]);
yeccpars2(142, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [142 | __Ss], [__T | __Stack]);
yeccpars2(142, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [142 | __Ss], [__T | __Stack]);
yeccpars2(142, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [142 | __Ss], [__T | __Stack]);
yeccpars2(142, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [142 | __Ss], [__T | __Stack]);
yeccpars2(142, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 183, [142 | __Ss], [__T | __Stack]);
yeccpars2(142, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(143, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(144, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 147, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(145, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 150, [145 | __Ss], [__T | __Stack]);
yeccpars2(145, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(146, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 148, [146 | __Ss], [__T | __Stack]);
yeccpars2(146, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_146_(__Stack),
 yeccpars2(yeccgoto(anno_expressions, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(147, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_147_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(tuple, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(148, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [148 | __Ss], [__T | __Stack]);
yeccpars2(148, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [148 | __Ss], [__T | __Stack]);
yeccpars2(148, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [148 | __Ss], [__T | __Stack]);
yeccpars2(148, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [148 | __Ss], [__T | __Stack]);
yeccpars2(148, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [148 | __Ss], [__T | __Stack]);
yeccpars2(148, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [148 | __Ss], [__T | __Stack]);
yeccpars2(148, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [148 | __Ss], [__T | __Stack]);
yeccpars2(148, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [148 | __Ss], [__T | __Stack]);
yeccpars2(148, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [148 | __Ss], [__T | __Stack]);
yeccpars2(148, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [148 | __Ss], [__T | __Stack]);
yeccpars2(148, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [148 | __Ss], [__T | __Stack]);
yeccpars2(148, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [148 | __Ss], [__T | __Stack]);
yeccpars2(148, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [148 | __Ss], [__T | __Stack]);
yeccpars2(148, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [148 | __Ss], [__T | __Stack]);
yeccpars2(148, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [148 | __Ss], [__T | __Stack]);
yeccpars2(148, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [148 | __Ss], [__T | __Stack]);
yeccpars2(148, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [148 | __Ss], [__T | __Stack]);
yeccpars2(148, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [148 | __Ss], [__T | __Stack]);
yeccpars2(148, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [148 | __Ss], [__T | __Stack]);
yeccpars2(148, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [148 | __Ss], [__T | __Stack]);
yeccpars2(148, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [148 | __Ss], [__T | __Stack]);
yeccpars2(148, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [148 | __Ss], [__T | __Stack]);
yeccpars2(148, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(149, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_149_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(anno_expressions, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(150, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_150_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tuple, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(151, 'of', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 152, [151 | __Ss], [__T | __Stack]);
yeccpars2(151, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(152, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [152 | __Ss], [__T | __Stack]);
yeccpars2(152, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 155, [152 | __Ss], [__T | __Stack]);
yeccpars2(152, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [152 | __Ss], [__T | __Stack]);
yeccpars2(152, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(153, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 159, [153 | __Ss], [__T | __Stack]);
yeccpars2(153, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(154, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_154_(__Stack),
 yeccpars2(yeccgoto(let_vars, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(155, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [155 | __Ss], [__T | __Stack]);
yeccpars2(155, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 157, [155 | __Ss], [__T | __Stack]);
yeccpars2(155, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [155 | __Ss], [__T | __Stack]);
yeccpars2(155, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(156, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 158, [156 | __Ss], [__T | __Stack]);
yeccpars2(156, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(157, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_157_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(let_vars, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(158, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_158_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(let_vars, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(159, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [159 | __Ss], [__T | __Stack]);
yeccpars2(159, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [159 | __Ss], [__T | __Stack]);
yeccpars2(159, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [159 | __Ss], [__T | __Stack]);
yeccpars2(159, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [159 | __Ss], [__T | __Stack]);
yeccpars2(159, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [159 | __Ss], [__T | __Stack]);
yeccpars2(159, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [159 | __Ss], [__T | __Stack]);
yeccpars2(159, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [159 | __Ss], [__T | __Stack]);
yeccpars2(159, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [159 | __Ss], [__T | __Stack]);
yeccpars2(159, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [159 | __Ss], [__T | __Stack]);
yeccpars2(159, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [159 | __Ss], [__T | __Stack]);
yeccpars2(159, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [159 | __Ss], [__T | __Stack]);
yeccpars2(159, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [159 | __Ss], [__T | __Stack]);
yeccpars2(159, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [159 | __Ss], [__T | __Stack]);
yeccpars2(159, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [159 | __Ss], [__T | __Stack]);
yeccpars2(159, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [159 | __Ss], [__T | __Stack]);
yeccpars2(159, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [159 | __Ss], [__T | __Stack]);
yeccpars2(159, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [159 | __Ss], [__T | __Stack]);
yeccpars2(159, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [159 | __Ss], [__T | __Stack]);
yeccpars2(159, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [159 | __Ss], [__T | __Stack]);
yeccpars2(159, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [159 | __Ss], [__T | __Stack]);
yeccpars2(159, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [159 | __Ss], [__T | __Stack]);
yeccpars2(159, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [159 | __Ss], [__T | __Stack]);
yeccpars2(159, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(160, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 161, [160 | __Ss], [__T | __Stack]);
yeccpars2(160, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(161, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 155, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [161 | __Ss], [__T | __Stack]);
yeccpars2(161, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(162, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 163, [162 | __Ss], [__T | __Stack]);
yeccpars2(162, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(163, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [163 | __Ss], [__T | __Stack]);
yeccpars2(163, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [163 | __Ss], [__T | __Stack]);
yeccpars2(163, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [163 | __Ss], [__T | __Stack]);
yeccpars2(163, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [163 | __Ss], [__T | __Stack]);
yeccpars2(163, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [163 | __Ss], [__T | __Stack]);
yeccpars2(163, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [163 | __Ss], [__T | __Stack]);
yeccpars2(163, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [163 | __Ss], [__T | __Stack]);
yeccpars2(163, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [163 | __Ss], [__T | __Stack]);
yeccpars2(163, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [163 | __Ss], [__T | __Stack]);
yeccpars2(163, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [163 | __Ss], [__T | __Stack]);
yeccpars2(163, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [163 | __Ss], [__T | __Stack]);
yeccpars2(163, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [163 | __Ss], [__T | __Stack]);
yeccpars2(163, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [163 | __Ss], [__T | __Stack]);
yeccpars2(163, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [163 | __Ss], [__T | __Stack]);
yeccpars2(163, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [163 | __Ss], [__T | __Stack]);
yeccpars2(163, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [163 | __Ss], [__T | __Stack]);
yeccpars2(163, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [163 | __Ss], [__T | __Stack]);
yeccpars2(163, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [163 | __Ss], [__T | __Stack]);
yeccpars2(163, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [163 | __Ss], [__T | __Stack]);
yeccpars2(163, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [163 | __Ss], [__T | __Stack]);
yeccpars2(163, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [163 | __Ss], [__T | __Stack]);
yeccpars2(163, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [163 | __Ss], [__T | __Stack]);
yeccpars2(163, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(164, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_164_(__Stack),
 __Nss = lists:nthtail(9, __Ss),
 yeccpars2(yeccgoto(try_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(165, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(other_pattern, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(166, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_166_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(receive_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(167, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(anno_pattern, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(168, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(other_pattern, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(169, 'when', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 240, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(170, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(anno_clause, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(171, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(other_pattern, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(172, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(other_pattern, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(173, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(atomic_pattern, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(174, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 191, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(anno_pattern, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(175, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_175_(__Stack),
 yeccpars2(yeccgoto(clause_pattern, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(176, 'after', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 182, [176 | __Ss], [__T | __Stack]);
yeccpars2(176, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(177, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 178, [177 | __Ss], [__T | __Stack]);
yeccpars2(177, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 179, [177 | __Ss], [__T | __Stack]);
yeccpars2(177, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 180, [177 | __Ss], [__T | __Stack]);
yeccpars2(177, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 181, [177 | __Ss], [__T | __Stack]);
yeccpars2(177, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [177 | __Ss], [__T | __Stack]);
yeccpars2(177, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [177 | __Ss], [__T | __Stack]);
yeccpars2(177, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [177 | __Ss], [__T | __Stack]);
yeccpars2(177, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [177 | __Ss], [__T | __Stack]);
yeccpars2(177, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [177 | __Ss], [__T | __Stack]);
yeccpars2(177, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [177 | __Ss], [__T | __Stack]);
yeccpars2(177, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 183, [177 | __Ss], [__T | __Stack]);
yeccpars2(177, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_177_(__Stack),
 yeccpars2(yeccgoto(anno_clauses, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(178, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 222, [178 | __Ss], [__T | __Stack]);
yeccpars2(178, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(179, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 178, [179 | __Ss], [__T | __Stack]);
yeccpars2(179, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 186, [179 | __Ss], [__T | __Stack]);
yeccpars2(179, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 180, [179 | __Ss], [__T | __Stack]);
yeccpars2(179, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 181, [179 | __Ss], [__T | __Stack]);
yeccpars2(179, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [179 | __Ss], [__T | __Stack]);
yeccpars2(179, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [179 | __Ss], [__T | __Stack]);
yeccpars2(179, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [179 | __Ss], [__T | __Stack]);
yeccpars2(179, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [179 | __Ss], [__T | __Stack]);
yeccpars2(179, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [179 | __Ss], [__T | __Stack]);
yeccpars2(179, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [179 | __Ss], [__T | __Stack]);
yeccpars2(179, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 183, [179 | __Ss], [__T | __Stack]);
yeccpars2(179, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(180, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 178, [180 | __Ss], [__T | __Stack]);
yeccpars2(180, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 186, [180 | __Ss], [__T | __Stack]);
yeccpars2(180, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 215, [180 | __Ss], [__T | __Stack]);
yeccpars2(180, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 181, [180 | __Ss], [__T | __Stack]);
yeccpars2(180, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [180 | __Ss], [__T | __Stack]);
yeccpars2(180, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [180 | __Ss], [__T | __Stack]);
yeccpars2(180, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [180 | __Ss], [__T | __Stack]);
yeccpars2(180, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [180 | __Ss], [__T | __Stack]);
yeccpars2(180, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [180 | __Ss], [__T | __Stack]);
yeccpars2(180, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [180 | __Ss], [__T | __Stack]);
yeccpars2(180, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 183, [180 | __Ss], [__T | __Stack]);
yeccpars2(180, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(181, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 178, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 186, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 181, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 183, [181 | __Ss], [__T | __Stack]);
yeccpars2(181, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(182, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(183, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 178, [183 | __Ss], [__T | __Stack]);
yeccpars2(183, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 186, [183 | __Ss], [__T | __Stack]);
yeccpars2(183, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 181, [183 | __Ss], [__T | __Stack]);
yeccpars2(183, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [183 | __Ss], [__T | __Stack]);
yeccpars2(183, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [183 | __Ss], [__T | __Stack]);
yeccpars2(183, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [183 | __Ss], [__T | __Stack]);
yeccpars2(183, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [183 | __Ss], [__T | __Stack]);
yeccpars2(183, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [183 | __Ss], [__T | __Stack]);
yeccpars2(183, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [183 | __Ss], [__T | __Stack]);
yeccpars2(183, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 183, [183 | __Ss], [__T | __Stack]);
yeccpars2(183, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 187, [183 | __Ss], [__T | __Stack]);
yeccpars2(183, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(184, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 201, [184 | __Ss], [__T | __Stack]);
yeccpars2(184, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(185, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 199, [185 | __Ss], [__T | __Stack]);
yeccpars2(185, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_185_(__Stack),
 yeccpars2(yeccgoto(anno_patterns, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(186, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 178, [186 | __Ss], [__T | __Stack]);
yeccpars2(186, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [186 | __Ss], [__T | __Stack]);
yeccpars2(186, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 181, [186 | __Ss], [__T | __Stack]);
yeccpars2(186, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [186 | __Ss], [__T | __Stack]);
yeccpars2(186, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [186 | __Ss], [__T | __Stack]);
yeccpars2(186, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [186 | __Ss], [__T | __Stack]);
yeccpars2(186, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [186 | __Ss], [__T | __Stack]);
yeccpars2(186, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [186 | __Ss], [__T | __Stack]);
yeccpars2(186, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [186 | __Ss], [__T | __Stack]);
yeccpars2(186, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 183, [186 | __Ss], [__T | __Stack]);
yeccpars2(186, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(187, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_187_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(tuple_pattern, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(188, '-|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 196, [188 | __Ss], [__T | __Stack]);
yeccpars2(188, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(anno_variable, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(189, '-|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 193, [189 | __Ss], [__T | __Stack]);
yeccpars2(189, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(190, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 191, [190 | __Ss], [__T | __Stack]);
yeccpars2(190, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(191, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 178, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 186, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 181, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 183, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(192, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_192_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(other_pattern, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(193, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [193 | __Ss], [__T | __Stack]);
yeccpars2(193, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(194, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 195, [194 | __Ss], [__T | __Stack]);
yeccpars2(194, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(195, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_195_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(anno_pattern, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(196, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [196 | __Ss], [__T | __Stack]);
yeccpars2(196, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(197, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 198, [197 | __Ss], [__T | __Stack]);
yeccpars2(197, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(198, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_198_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(anno_variable, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(199, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 178, [199 | __Ss], [__T | __Stack]);
yeccpars2(199, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 186, [199 | __Ss], [__T | __Stack]);
yeccpars2(199, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 181, [199 | __Ss], [__T | __Stack]);
yeccpars2(199, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [199 | __Ss], [__T | __Stack]);
yeccpars2(199, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [199 | __Ss], [__T | __Stack]);
yeccpars2(199, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [199 | __Ss], [__T | __Stack]);
yeccpars2(199, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [199 | __Ss], [__T | __Stack]);
yeccpars2(199, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [199 | __Ss], [__T | __Stack]);
yeccpars2(199, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [199 | __Ss], [__T | __Stack]);
yeccpars2(199, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 183, [199 | __Ss], [__T | __Stack]);
yeccpars2(199, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(200, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_200_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(anno_patterns, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(201, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_201_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tuple_pattern, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(202, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 203, [202 | __Ss], [__T | __Stack]);
yeccpars2(202, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(203, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(204, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_204_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(timeout, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(205, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 207, [205 | __Ss], [__T | __Stack]);
yeccpars2(205, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 208, [205 | __Ss], [__T | __Stack]);
yeccpars2(205, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 209, [205 | __Ss], [__T | __Stack]);
yeccpars2(205, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(206, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_206_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(cons_pattern, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(207, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 178, [207 | __Ss], [__T | __Stack]);
yeccpars2(207, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 186, [207 | __Ss], [__T | __Stack]);
yeccpars2(207, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 181, [207 | __Ss], [__T | __Stack]);
yeccpars2(207, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [207 | __Ss], [__T | __Stack]);
yeccpars2(207, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [207 | __Ss], [__T | __Stack]);
yeccpars2(207, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [207 | __Ss], [__T | __Stack]);
yeccpars2(207, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [207 | __Ss], [__T | __Stack]);
yeccpars2(207, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [207 | __Ss], [__T | __Stack]);
yeccpars2(207, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [207 | __Ss], [__T | __Stack]);
yeccpars2(207, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 183, [207 | __Ss], [__T | __Stack]);
yeccpars2(207, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(208, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_208_(__Stack),
 yeccpars2(yeccgoto(tail_pattern, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(209, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 178, [209 | __Ss], [__T | __Stack]);
yeccpars2(209, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 186, [209 | __Ss], [__T | __Stack]);
yeccpars2(209, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 181, [209 | __Ss], [__T | __Stack]);
yeccpars2(209, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [209 | __Ss], [__T | __Stack]);
yeccpars2(209, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [209 | __Ss], [__T | __Stack]);
yeccpars2(209, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [209 | __Ss], [__T | __Stack]);
yeccpars2(209, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [209 | __Ss], [__T | __Stack]);
yeccpars2(209, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [209 | __Ss], [__T | __Stack]);
yeccpars2(209, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [209 | __Ss], [__T | __Stack]);
yeccpars2(209, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 183, [209 | __Ss], [__T | __Stack]);
yeccpars2(209, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(210, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 211, [210 | __Ss], [__T | __Stack]);
yeccpars2(210, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(211, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_211_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tail_pattern, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(212, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 207, [212 | __Ss], [__T | __Stack]);
yeccpars2(212, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 208, [212 | __Ss], [__T | __Stack]);
yeccpars2(212, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 209, [212 | __Ss], [__T | __Stack]);
yeccpars2(212, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(213, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_213_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tail_pattern, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(214, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 216, [214 | __Ss], [__T | __Stack]);
yeccpars2(214, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(215, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_215_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(clause_pattern, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(216, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_216_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(clause_pattern, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(217, '-|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 193, [217 | __Ss], [__T | __Stack]);
yeccpars2(217, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(anno_pattern, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(218, '-|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 219, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(219, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [219 | __Ss], [__T | __Stack]);
yeccpars2(219, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(220, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 221, [220 | __Ss], [__T | __Stack]);
yeccpars2(220, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(221, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_221_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(anno_clause, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(222, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 225, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 226, [222 | __Ss], [__T | __Stack]);
yeccpars2(222, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(223, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 236, [223 | __Ss], [__T | __Stack]);
yeccpars2(223, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(224, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 234, [224 | __Ss], [__T | __Stack]);
yeccpars2(224, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_224_(__Stack),
 yeccpars2(yeccgoto(segment_patterns, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(225, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 228, [225 | __Ss], [__T | __Stack]);
yeccpars2(225, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(226, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 227, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(227, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_227_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(binary_pattern, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(228, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 178, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 186, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 181, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 183, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(229, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 230, [229 | __Ss], [__T | __Stack]);
yeccpars2(229, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(230, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 231, [230 | __Ss], [__T | __Stack]);
yeccpars2(230, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(231, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 178, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 186, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 181, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 183, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(232, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 233, [232 | __Ss], [__T | __Stack]);
yeccpars2(232, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(233, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_233_(__Stack),
 __Nss = lists:nthtail(6, __Ss),
 yeccpars2(yeccgoto(segment_pattern, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(234, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 225, [234 | __Ss], [__T | __Stack]);
yeccpars2(234, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(235, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_235_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(segment_patterns, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(236, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 237, [236 | __Ss], [__T | __Stack]);
yeccpars2(236, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(237, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_237_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(binary_pattern, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(238, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_238_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(anno_clauses, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(239, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_239_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(receive_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(240, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(241, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 242, [241 | __Ss], [__T | __Stack]);
yeccpars2(241, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(242, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [242 | __Ss], [__T | __Stack]);
yeccpars2(242, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [242 | __Ss], [__T | __Stack]);
yeccpars2(242, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [242 | __Ss], [__T | __Stack]);
yeccpars2(242, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [242 | __Ss], [__T | __Stack]);
yeccpars2(242, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [242 | __Ss], [__T | __Stack]);
yeccpars2(242, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [242 | __Ss], [__T | __Stack]);
yeccpars2(242, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [242 | __Ss], [__T | __Stack]);
yeccpars2(242, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [242 | __Ss], [__T | __Stack]);
yeccpars2(242, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [242 | __Ss], [__T | __Stack]);
yeccpars2(242, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [242 | __Ss], [__T | __Stack]);
yeccpars2(242, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [242 | __Ss], [__T | __Stack]);
yeccpars2(242, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [242 | __Ss], [__T | __Stack]);
yeccpars2(242, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [242 | __Ss], [__T | __Stack]);
yeccpars2(242, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [242 | __Ss], [__T | __Stack]);
yeccpars2(242, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [242 | __Ss], [__T | __Stack]);
yeccpars2(242, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [242 | __Ss], [__T | __Stack]);
yeccpars2(242, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [242 | __Ss], [__T | __Stack]);
yeccpars2(242, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [242 | __Ss], [__T | __Stack]);
yeccpars2(242, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [242 | __Ss], [__T | __Stack]);
yeccpars2(242, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [242 | __Ss], [__T | __Stack]);
yeccpars2(242, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [242 | __Ss], [__T | __Stack]);
yeccpars2(242, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [242 | __Ss], [__T | __Stack]);
yeccpars2(242, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(243, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_243_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(clause, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(244, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 246, [244 | __Ss], [__T | __Stack]);
yeccpars2(244, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(245, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_245_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(primop_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(246, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [246 | __Ss], [__T | __Stack]);
yeccpars2(246, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [246 | __Ss], [__T | __Stack]);
yeccpars2(246, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 248, [246 | __Ss], [__T | __Stack]);
yeccpars2(246, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [246 | __Ss], [__T | __Stack]);
yeccpars2(246, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [246 | __Ss], [__T | __Stack]);
yeccpars2(246, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [246 | __Ss], [__T | __Stack]);
yeccpars2(246, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [246 | __Ss], [__T | __Stack]);
yeccpars2(246, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [246 | __Ss], [__T | __Stack]);
yeccpars2(246, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [246 | __Ss], [__T | __Stack]);
yeccpars2(246, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [246 | __Ss], [__T | __Stack]);
yeccpars2(246, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [246 | __Ss], [__T | __Stack]);
yeccpars2(246, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [246 | __Ss], [__T | __Stack]);
yeccpars2(246, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [246 | __Ss], [__T | __Stack]);
yeccpars2(246, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [246 | __Ss], [__T | __Stack]);
yeccpars2(246, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [246 | __Ss], [__T | __Stack]);
yeccpars2(246, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [246 | __Ss], [__T | __Stack]);
yeccpars2(246, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [246 | __Ss], [__T | __Stack]);
yeccpars2(246, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [246 | __Ss], [__T | __Stack]);
yeccpars2(246, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [246 | __Ss], [__T | __Stack]);
yeccpars2(246, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [246 | __Ss], [__T | __Stack]);
yeccpars2(246, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [246 | __Ss], [__T | __Stack]);
yeccpars2(246, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [246 | __Ss], [__T | __Stack]);
yeccpars2(246, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [246 | __Ss], [__T | __Stack]);
yeccpars2(246, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(247, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 249, [247 | __Ss], [__T | __Stack]);
yeccpars2(247, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(248, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_248_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(arg_list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(249, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_249_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(arg_list, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(250, in, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 251, [250 | __Ss], [__T | __Stack]);
yeccpars2(250, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(251, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [251 | __Ss], [__T | __Stack]);
yeccpars2(251, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [251 | __Ss], [__T | __Stack]);
yeccpars2(251, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [251 | __Ss], [__T | __Stack]);
yeccpars2(251, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [251 | __Ss], [__T | __Stack]);
yeccpars2(251, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [251 | __Ss], [__T | __Stack]);
yeccpars2(251, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [251 | __Ss], [__T | __Stack]);
yeccpars2(251, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [251 | __Ss], [__T | __Stack]);
yeccpars2(251, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [251 | __Ss], [__T | __Stack]);
yeccpars2(251, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [251 | __Ss], [__T | __Stack]);
yeccpars2(251, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [251 | __Ss], [__T | __Stack]);
yeccpars2(251, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [251 | __Ss], [__T | __Stack]);
yeccpars2(251, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [251 | __Ss], [__T | __Stack]);
yeccpars2(251, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [251 | __Ss], [__T | __Stack]);
yeccpars2(251, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [251 | __Ss], [__T | __Stack]);
yeccpars2(251, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [251 | __Ss], [__T | __Stack]);
yeccpars2(251, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [251 | __Ss], [__T | __Stack]);
yeccpars2(251, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [251 | __Ss], [__T | __Stack]);
yeccpars2(251, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [251 | __Ss], [__T | __Stack]);
yeccpars2(251, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [251 | __Ss], [__T | __Stack]);
yeccpars2(251, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [251 | __Ss], [__T | __Stack]);
yeccpars2(251, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [251 | __Ss], [__T | __Stack]);
yeccpars2(251, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [251 | __Ss], [__T | __Stack]);
yeccpars2(251, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(252, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_252_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(letrec_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(253, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 254, [253 | __Ss], [__T | __Stack]);
yeccpars2(253, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(254, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [254 | __Ss], [__T | __Stack]);
yeccpars2(254, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [254 | __Ss], [__T | __Stack]);
yeccpars2(254, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [254 | __Ss], [__T | __Stack]);
yeccpars2(254, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [254 | __Ss], [__T | __Stack]);
yeccpars2(254, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [254 | __Ss], [__T | __Stack]);
yeccpars2(254, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [254 | __Ss], [__T | __Stack]);
yeccpars2(254, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [254 | __Ss], [__T | __Stack]);
yeccpars2(254, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [254 | __Ss], [__T | __Stack]);
yeccpars2(254, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [254 | __Ss], [__T | __Stack]);
yeccpars2(254, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [254 | __Ss], [__T | __Stack]);
yeccpars2(254, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [254 | __Ss], [__T | __Stack]);
yeccpars2(254, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [254 | __Ss], [__T | __Stack]);
yeccpars2(254, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [254 | __Ss], [__T | __Stack]);
yeccpars2(254, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [254 | __Ss], [__T | __Stack]);
yeccpars2(254, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [254 | __Ss], [__T | __Stack]);
yeccpars2(254, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [254 | __Ss], [__T | __Stack]);
yeccpars2(254, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [254 | __Ss], [__T | __Stack]);
yeccpars2(254, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [254 | __Ss], [__T | __Stack]);
yeccpars2(254, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [254 | __Ss], [__T | __Stack]);
yeccpars2(254, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [254 | __Ss], [__T | __Stack]);
yeccpars2(254, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [254 | __Ss], [__T | __Stack]);
yeccpars2(254, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [254 | __Ss], [__T | __Stack]);
yeccpars2(254, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(255, in, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 256, [255 | __Ss], [__T | __Stack]);
yeccpars2(255, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(256, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [256 | __Ss], [__T | __Stack]);
yeccpars2(256, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [256 | __Ss], [__T | __Stack]);
yeccpars2(256, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [256 | __Ss], [__T | __Stack]);
yeccpars2(256, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [256 | __Ss], [__T | __Stack]);
yeccpars2(256, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [256 | __Ss], [__T | __Stack]);
yeccpars2(256, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [256 | __Ss], [__T | __Stack]);
yeccpars2(256, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [256 | __Ss], [__T | __Stack]);
yeccpars2(256, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [256 | __Ss], [__T | __Stack]);
yeccpars2(256, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [256 | __Ss], [__T | __Stack]);
yeccpars2(256, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [256 | __Ss], [__T | __Stack]);
yeccpars2(256, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [256 | __Ss], [__T | __Stack]);
yeccpars2(256, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [256 | __Ss], [__T | __Stack]);
yeccpars2(256, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [256 | __Ss], [__T | __Stack]);
yeccpars2(256, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [256 | __Ss], [__T | __Stack]);
yeccpars2(256, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [256 | __Ss], [__T | __Stack]);
yeccpars2(256, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [256 | __Ss], [__T | __Stack]);
yeccpars2(256, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [256 | __Ss], [__T | __Stack]);
yeccpars2(256, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [256 | __Ss], [__T | __Stack]);
yeccpars2(256, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [256 | __Ss], [__T | __Stack]);
yeccpars2(256, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [256 | __Ss], [__T | __Stack]);
yeccpars2(256, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [256 | __Ss], [__T | __Stack]);
yeccpars2(256, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [256 | __Ss], [__T | __Stack]);
yeccpars2(256, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(257, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_257_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(let_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(258, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(259, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_259_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(sequence, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(260, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_260_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(catch_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(261, 'of', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 262, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(262, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 178, [262 | __Ss], [__T | __Stack]);
yeccpars2(262, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 179, [262 | __Ss], [__T | __Stack]);
yeccpars2(262, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 180, [262 | __Ss], [__T | __Stack]);
yeccpars2(262, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 181, [262 | __Ss], [__T | __Stack]);
yeccpars2(262, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [262 | __Ss], [__T | __Stack]);
yeccpars2(262, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [262 | __Ss], [__T | __Stack]);
yeccpars2(262, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [262 | __Ss], [__T | __Stack]);
yeccpars2(262, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [262 | __Ss], [__T | __Stack]);
yeccpars2(262, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [262 | __Ss], [__T | __Stack]);
yeccpars2(262, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [262 | __Ss], [__T | __Stack]);
yeccpars2(262, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 183, [262 | __Ss], [__T | __Stack]);
yeccpars2(262, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(263, 'end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 264, [263 | __Ss], [__T | __Stack]);
yeccpars2(263, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(264, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_264_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(case_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(265, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 266, [265 | __Ss], [__T | __Stack]);
yeccpars2(265, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(266, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [266 | __Ss], [__T | __Stack]);
yeccpars2(266, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(267, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 246, [267 | __Ss], [__T | __Stack]);
yeccpars2(267, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(268, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_268_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(call_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(269, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 246, [269 | __Ss], [__T | __Stack]);
yeccpars2(269, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(270, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_270_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(application_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(271, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 273, [271 | __Ss], [__T | __Stack]);
yeccpars2(271, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 274, [271 | __Ss], [__T | __Stack]);
yeccpars2(271, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 275, [271 | __Ss], [__T | __Stack]);
yeccpars2(271, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(272, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_272_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(cons, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(273, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [273 | __Ss], [__T | __Stack]);
yeccpars2(273, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [273 | __Ss], [__T | __Stack]);
yeccpars2(273, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [273 | __Ss], [__T | __Stack]);
yeccpars2(273, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [273 | __Ss], [__T | __Stack]);
yeccpars2(273, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [273 | __Ss], [__T | __Stack]);
yeccpars2(273, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [273 | __Ss], [__T | __Stack]);
yeccpars2(273, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [273 | __Ss], [__T | __Stack]);
yeccpars2(273, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [273 | __Ss], [__T | __Stack]);
yeccpars2(273, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [273 | __Ss], [__T | __Stack]);
yeccpars2(273, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [273 | __Ss], [__T | __Stack]);
yeccpars2(273, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [273 | __Ss], [__T | __Stack]);
yeccpars2(273, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [273 | __Ss], [__T | __Stack]);
yeccpars2(273, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [273 | __Ss], [__T | __Stack]);
yeccpars2(273, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [273 | __Ss], [__T | __Stack]);
yeccpars2(273, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [273 | __Ss], [__T | __Stack]);
yeccpars2(273, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [273 | __Ss], [__T | __Stack]);
yeccpars2(273, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [273 | __Ss], [__T | __Stack]);
yeccpars2(273, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [273 | __Ss], [__T | __Stack]);
yeccpars2(273, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [273 | __Ss], [__T | __Stack]);
yeccpars2(273, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [273 | __Ss], [__T | __Stack]);
yeccpars2(273, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [273 | __Ss], [__T | __Stack]);
yeccpars2(273, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [273 | __Ss], [__T | __Stack]);
yeccpars2(273, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(274, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_274_(__Stack),
 yeccpars2(yeccgoto(tail, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(275, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(276, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 277, [276 | __Ss], [__T | __Stack]);
yeccpars2(276, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(277, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_277_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tail, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(278, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 273, [278 | __Ss], [__T | __Stack]);
yeccpars2(278, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 274, [278 | __Ss], [__T | __Stack]);
yeccpars2(278, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 275, [278 | __Ss], [__T | __Stack]);
yeccpars2(278, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(279, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_279_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(tail, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(280, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 282, [280 | __Ss], [__T | __Stack]);
yeccpars2(280, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(281, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_281_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(expression, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(282, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_282_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expression, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(283, '-|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 284, [283 | __Ss], [__T | __Stack]);
yeccpars2(283, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(284, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [284 | __Ss], [__T | __Stack]);
yeccpars2(284, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(285, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 286, [285 | __Ss], [__T | __Stack]);
yeccpars2(285, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(286, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_286_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(anno_expression, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(287, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 290, [287 | __Ss], [__T | __Stack]);
yeccpars2(287, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 291, [287 | __Ss], [__T | __Stack]);
yeccpars2(287, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(288, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 301, [288 | __Ss], [__T | __Stack]);
yeccpars2(288, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(289, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 299, [289 | __Ss], [__T | __Stack]);
yeccpars2(289, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_289_(__Stack),
 yeccpars2(yeccgoto(segments, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(290, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 293, [290 | __Ss], [__T | __Stack]);
yeccpars2(290, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(291, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 292, [291 | __Ss], [__T | __Stack]);
yeccpars2(291, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(292, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_292_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(binary, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(293, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(294, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 295, [294 | __Ss], [__T | __Stack]);
yeccpars2(294, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(295, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 296, [295 | __Ss], [__T | __Stack]);
yeccpars2(295, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(296, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [296 | __Ss], [__T | __Stack]);
yeccpars2(296, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [296 | __Ss], [__T | __Stack]);
yeccpars2(296, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [296 | __Ss], [__T | __Stack]);
yeccpars2(296, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [296 | __Ss], [__T | __Stack]);
yeccpars2(296, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [296 | __Ss], [__T | __Stack]);
yeccpars2(296, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [296 | __Ss], [__T | __Stack]);
yeccpars2(296, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [296 | __Ss], [__T | __Stack]);
yeccpars2(296, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [296 | __Ss], [__T | __Stack]);
yeccpars2(296, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [296 | __Ss], [__T | __Stack]);
yeccpars2(296, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [296 | __Ss], [__T | __Stack]);
yeccpars2(296, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [296 | __Ss], [__T | __Stack]);
yeccpars2(296, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [296 | __Ss], [__T | __Stack]);
yeccpars2(296, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [296 | __Ss], [__T | __Stack]);
yeccpars2(296, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [296 | __Ss], [__T | __Stack]);
yeccpars2(296, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [296 | __Ss], [__T | __Stack]);
yeccpars2(296, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [296 | __Ss], [__T | __Stack]);
yeccpars2(296, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [296 | __Ss], [__T | __Stack]);
yeccpars2(296, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [296 | __Ss], [__T | __Stack]);
yeccpars2(296, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [296 | __Ss], [__T | __Stack]);
yeccpars2(296, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [296 | __Ss], [__T | __Stack]);
yeccpars2(296, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [296 | __Ss], [__T | __Stack]);
yeccpars2(296, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [296 | __Ss], [__T | __Stack]);
yeccpars2(296, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(297, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 298, [297 | __Ss], [__T | __Stack]);
yeccpars2(297, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(298, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_298_(__Stack),
 __Nss = lists:nthtail(6, __Ss),
 yeccpars2(yeccgoto(segment, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(299, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 290, [299 | __Ss], [__T | __Stack]);
yeccpars2(299, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(300, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_300_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(segments, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(301, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 302, [301 | __Ss], [__T | __Stack]);
yeccpars2(301, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(302, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_302_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(binary, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(303, '-|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 196, [303 | __Ss], [__T | __Stack]);
yeccpars2(303, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(304, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [304 | __Ss], [__T | __Stack]);
yeccpars2(304, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [304 | __Ss], [__T | __Stack]);
yeccpars2(304, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(305, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_305_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(anno_variables, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(306, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 307, [306 | __Ss], [__T | __Stack]);
yeccpars2(306, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(307, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [307 | __Ss], [__T | __Stack]);
yeccpars2(307, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [307 | __Ss], [__T | __Stack]);
yeccpars2(307, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [307 | __Ss], [__T | __Stack]);
yeccpars2(307, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [307 | __Ss], [__T | __Stack]);
yeccpars2(307, apply, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [307 | __Ss], [__T | __Stack]);
yeccpars2(307, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [307 | __Ss], [__T | __Stack]);
yeccpars2(307, call, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [307 | __Ss], [__T | __Stack]);
yeccpars2(307, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [307 | __Ss], [__T | __Stack]);
yeccpars2(307, 'catch', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [307 | __Ss], [__T | __Stack]);
yeccpars2(307, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [307 | __Ss], [__T | __Stack]);
yeccpars2(307, do, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 138, [307 | __Ss], [__T | __Stack]);
yeccpars2(307, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [307 | __Ss], [__T | __Stack]);
yeccpars2(307, 'fun', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [307 | __Ss], [__T | __Stack]);
yeccpars2(307, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [307 | __Ss], [__T | __Stack]);
yeccpars2(307, 'let', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [307 | __Ss], [__T | __Stack]);
yeccpars2(307, letrec, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [307 | __Ss], [__T | __Stack]);
yeccpars2(307, primop, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [307 | __Ss], [__T | __Stack]);
yeccpars2(307, 'receive', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 142, [307 | __Ss], [__T | __Stack]);
yeccpars2(307, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [307 | __Ss], [__T | __Stack]);
yeccpars2(307, 'try', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [307 | __Ss], [__T | __Stack]);
yeccpars2(307, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [307 | __Ss], [__T | __Stack]);
yeccpars2(307, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [307 | __Ss], [__T | __Stack]);
yeccpars2(307, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(308, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_308_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(fun_expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(309, '-|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 310, [309 | __Ss], [__T | __Stack]);
yeccpars2(309, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(310, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [310 | __Ss], [__T | __Stack]);
yeccpars2(310, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(311, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 312, [311 | __Ss], [__T | __Stack]);
yeccpars2(311, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(312, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_312_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(anno_fun, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(313, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_313_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(function_definitions, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(314, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_314_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(module_definition, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(315, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 316, [315 | __Ss], [__T | __Stack]);
yeccpars2(315, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(316, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [316 | __Ss], [__T | __Stack]);
yeccpars2(316, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(317, attributes, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [317 | __Ss], [__T | __Stack]);
yeccpars2(317, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(318, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 61, [318 | __Ss], [__T | __Stack]);
yeccpars2(318, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [318 | __Ss], [__T | __Stack]);
yeccpars2(318, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_318_(__Stack),
 yeccpars2(58, __Cat, [318 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(319, 'end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 320, [319 | __Ss], [__T | __Stack]);
yeccpars2(319, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(320, '-|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 321, [320 | __Ss], [__T | __Stack]);
yeccpars2(320, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(321, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 65, [321 | __Ss], [__T | __Stack]);
yeccpars2(321, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(322, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 323, [322 | __Ss], [__T | __Stack]);
yeccpars2(322, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(323, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_323_(__Stack),
 __Nss = lists:nthtail(9, __Ss),
 yeccpars2(yeccgoto(module_definition, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(__Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.1",{missing_state_in_action_table, __Other}}).

yeccgoto(anno_clause, 142) ->
 177;
yeccgoto(anno_clause, 177) ->
 177;
yeccgoto(anno_clause, 262) ->
 177;
yeccgoto(anno_clauses, 142) ->
 176;
yeccgoto(anno_clauses, 177) ->
 238;
yeccgoto(anno_clauses, 262) ->
 263;
yeccgoto(anno_expression, 108) ->
 128;
yeccgoto(anno_expression, 131) ->
 146;
yeccgoto(anno_expression, 132) ->
 271;
yeccgoto(anno_expression, 133) ->
 269;
yeccgoto(anno_expression, 135) ->
 265;
yeccgoto(anno_expression, 136) ->
 261;
yeccgoto(anno_expression, 137) ->
 260;
yeccgoto(anno_expression, 138) ->
 258;
yeccgoto(anno_expression, 143) ->
 151;
yeccgoto(anno_expression, 144) ->
 146;
yeccgoto(anno_expression, 148) ->
 146;
yeccgoto(anno_expression, 159) ->
 160;
yeccgoto(anno_expression, 163) ->
 164;
yeccgoto(anno_expression, 182) ->
 202;
yeccgoto(anno_expression, 203) ->
 204;
yeccgoto(anno_expression, 240) ->
 241;
yeccgoto(anno_expression, 242) ->
 243;
yeccgoto(anno_expression, 246) ->
 146;
yeccgoto(anno_expression, 251) ->
 252;
yeccgoto(anno_expression, 254) ->
 255;
yeccgoto(anno_expression, 256) ->
 257;
yeccgoto(anno_expression, 258) ->
 259;
yeccgoto(anno_expression, 266) ->
 267;
yeccgoto(anno_expression, 273) ->
 278;
yeccgoto(anno_expression, 275) ->
 276;
yeccgoto(anno_expression, 293) ->
 294;
yeccgoto(anno_expression, 296) ->
 146;
yeccgoto(anno_expression, 307) ->
 308;
yeccgoto(anno_expressions, 131) ->
 280;
yeccgoto(anno_expressions, 144) ->
 145;
yeccgoto(anno_expressions, 148) ->
 149;
yeccgoto(anno_expressions, 246) ->
 247;
yeccgoto(anno_expressions, 296) ->
 297;
yeccgoto(anno_fun, 96) ->
 98;
yeccgoto(anno_function_name, 17) ->
 60;
yeccgoto(anno_function_name, 59) ->
 60;
yeccgoto(anno_function_name, 140) ->
 60;
yeccgoto(anno_function_name, 318) ->
 60;
yeccgoto(anno_pattern, 142) ->
 175;
yeccgoto(anno_pattern, 177) ->
 175;
yeccgoto(anno_pattern, 179) ->
 175;
yeccgoto(anno_pattern, 180) ->
 185;
yeccgoto(anno_pattern, 181) ->
 205;
yeccgoto(anno_pattern, 183) ->
 185;
yeccgoto(anno_pattern, 191) ->
 192;
yeccgoto(anno_pattern, 199) ->
 185;
yeccgoto(anno_pattern, 207) ->
 212;
yeccgoto(anno_pattern, 209) ->
 210;
yeccgoto(anno_pattern, 228) ->
 229;
yeccgoto(anno_pattern, 231) ->
 185;
yeccgoto(anno_pattern, 262) ->
 175;
yeccgoto(anno_patterns, 180) ->
 214;
yeccgoto(anno_patterns, 183) ->
 184;
yeccgoto(anno_patterns, 199) ->
 200;
yeccgoto(anno_patterns, 231) ->
 232;
yeccgoto(anno_variable, 101) ->
 104;
yeccgoto(anno_variable, 139) ->
 154;
yeccgoto(anno_variable, 142) ->
 174;
yeccgoto(anno_variable, 152) ->
 154;
yeccgoto(anno_variable, 155) ->
 104;
yeccgoto(anno_variable, 161) ->
 154;
yeccgoto(anno_variable, 177) ->
 174;
yeccgoto(anno_variable, 179) ->
 174;
yeccgoto(anno_variable, 180) ->
 174;
yeccgoto(anno_variable, 181) ->
 174;
yeccgoto(anno_variable, 183) ->
 174;
yeccgoto(anno_variable, 186) ->
 190;
yeccgoto(anno_variable, 191) ->
 174;
yeccgoto(anno_variable, 199) ->
 174;
yeccgoto(anno_variable, 207) ->
 174;
yeccgoto(anno_variable, 209) ->
 174;
yeccgoto(anno_variable, 228) ->
 174;
yeccgoto(anno_variable, 231) ->
 174;
yeccgoto(anno_variable, 262) ->
 174;
yeccgoto(anno_variable, 304) ->
 104;
yeccgoto(anno_variables, 101) ->
 103;
yeccgoto(anno_variables, 155) ->
 156;
yeccgoto(anno_variables, 304) ->
 305;
yeccgoto(annotation, 63) ->
 64;
yeccgoto(annotation, 193) ->
 194;
yeccgoto(annotation, 196) ->
 197;
yeccgoto(annotation, 219) ->
 220;
yeccgoto(annotation, 284) ->
 285;
yeccgoto(annotation, 310) ->
 311;
yeccgoto(annotation, 321) ->
 322;
yeccgoto(application_expr, 108) ->
 127;
yeccgoto(application_expr, 130) ->
 127;
yeccgoto(application_expr, 131) ->
 127;
yeccgoto(application_expr, 132) ->
 127;
yeccgoto(application_expr, 133) ->
 127;
yeccgoto(application_expr, 135) ->
 127;
yeccgoto(application_expr, 136) ->
 127;
yeccgoto(application_expr, 137) ->
 127;
yeccgoto(application_expr, 138) ->
 127;
yeccgoto(application_expr, 143) ->
 127;
yeccgoto(application_expr, 144) ->
 127;
yeccgoto(application_expr, 148) ->
 127;
yeccgoto(application_expr, 159) ->
 127;
yeccgoto(application_expr, 163) ->
 127;
yeccgoto(application_expr, 182) ->
 127;
yeccgoto(application_expr, 203) ->
 127;
yeccgoto(application_expr, 240) ->
 127;
yeccgoto(application_expr, 242) ->
 127;
yeccgoto(application_expr, 246) ->
 127;
yeccgoto(application_expr, 251) ->
 127;
yeccgoto(application_expr, 254) ->
 127;
yeccgoto(application_expr, 256) ->
 127;
yeccgoto(application_expr, 258) ->
 127;
yeccgoto(application_expr, 266) ->
 127;
yeccgoto(application_expr, 273) ->
 127;
yeccgoto(application_expr, 275) ->
 127;
yeccgoto(application_expr, 293) ->
 127;
yeccgoto(application_expr, 296) ->
 127;
yeccgoto(application_expr, 307) ->
 127;
yeccgoto(arg_list, 244) ->
 245;
yeccgoto(arg_list, 267) ->
 268;
yeccgoto(arg_list, 269) ->
 270;
yeccgoto(atomic_constant, 65) ->
 71;
yeccgoto(atomic_constant, 72) ->
 71;
yeccgoto(atomic_constant, 79) ->
 71;
yeccgoto(atomic_constant, 85) ->
 71;
yeccgoto(atomic_constant, 87) ->
 71;
yeccgoto(atomic_constant, 92) ->
 71;
yeccgoto(atomic_literal, 24) ->
 29;
yeccgoto(atomic_literal, 30) ->
 29;
yeccgoto(atomic_literal, 36) ->
 29;
yeccgoto(atomic_literal, 40) ->
 29;
yeccgoto(atomic_literal, 46) ->
 29;
yeccgoto(atomic_literal, 48) ->
 29;
yeccgoto(atomic_literal, 108) ->
 126;
yeccgoto(atomic_literal, 130) ->
 126;
yeccgoto(atomic_literal, 131) ->
 126;
yeccgoto(atomic_literal, 132) ->
 126;
yeccgoto(atomic_literal, 133) ->
 126;
yeccgoto(atomic_literal, 135) ->
 126;
yeccgoto(atomic_literal, 136) ->
 126;
yeccgoto(atomic_literal, 137) ->
 126;
yeccgoto(atomic_literal, 138) ->
 126;
yeccgoto(atomic_literal, 142) ->
 173;
yeccgoto(atomic_literal, 143) ->
 126;
yeccgoto(atomic_literal, 144) ->
 126;
yeccgoto(atomic_literal, 148) ->
 126;
yeccgoto(atomic_literal, 159) ->
 126;
yeccgoto(atomic_literal, 163) ->
 126;
yeccgoto(atomic_literal, 177) ->
 173;
yeccgoto(atomic_literal, 179) ->
 173;
yeccgoto(atomic_literal, 180) ->
 173;
yeccgoto(atomic_literal, 181) ->
 173;
yeccgoto(atomic_literal, 182) ->
 126;
yeccgoto(atomic_literal, 183) ->
 173;
yeccgoto(atomic_literal, 186) ->
 173;
yeccgoto(atomic_literal, 191) ->
 173;
yeccgoto(atomic_literal, 199) ->
 173;
yeccgoto(atomic_literal, 203) ->
 126;
yeccgoto(atomic_literal, 207) ->
 173;
yeccgoto(atomic_literal, 209) ->
 173;
yeccgoto(atomic_literal, 228) ->
 173;
yeccgoto(atomic_literal, 231) ->
 173;
yeccgoto(atomic_literal, 240) ->
 126;
yeccgoto(atomic_literal, 242) ->
 126;
yeccgoto(atomic_literal, 246) ->
 126;
yeccgoto(atomic_literal, 251) ->
 126;
yeccgoto(atomic_literal, 254) ->
 126;
yeccgoto(atomic_literal, 256) ->
 126;
yeccgoto(atomic_literal, 258) ->
 126;
yeccgoto(atomic_literal, 262) ->
 173;
yeccgoto(atomic_literal, 266) ->
 126;
yeccgoto(atomic_literal, 273) ->
 126;
yeccgoto(atomic_literal, 275) ->
 126;
yeccgoto(atomic_literal, 293) ->
 126;
yeccgoto(atomic_literal, 296) ->
 126;
yeccgoto(atomic_literal, 307) ->
 126;
yeccgoto(atomic_pattern, 142) ->
 172;
yeccgoto(atomic_pattern, 177) ->
 172;
yeccgoto(atomic_pattern, 179) ->
 172;
yeccgoto(atomic_pattern, 180) ->
 172;
yeccgoto(atomic_pattern, 181) ->
 172;
yeccgoto(atomic_pattern, 183) ->
 172;
yeccgoto(atomic_pattern, 186) ->
 172;
yeccgoto(atomic_pattern, 191) ->
 172;
yeccgoto(atomic_pattern, 199) ->
 172;
yeccgoto(atomic_pattern, 207) ->
 172;
yeccgoto(atomic_pattern, 209) ->
 172;
yeccgoto(atomic_pattern, 228) ->
 172;
yeccgoto(atomic_pattern, 231) ->
 172;
yeccgoto(atomic_pattern, 262) ->
 172;
yeccgoto(attribute, 19) ->
 21;
yeccgoto(attribute, 53) ->
 21;
yeccgoto(attribute_list, 19) ->
 20;
yeccgoto(attribute_list, 53) ->
 54;
yeccgoto(binary, 108) ->
 125;
yeccgoto(binary, 130) ->
 125;
yeccgoto(binary, 131) ->
 125;
yeccgoto(binary, 132) ->
 125;
yeccgoto(binary, 133) ->
 125;
yeccgoto(binary, 135) ->
 125;
yeccgoto(binary, 136) ->
 125;
yeccgoto(binary, 137) ->
 125;
yeccgoto(binary, 138) ->
 125;
yeccgoto(binary, 143) ->
 125;
yeccgoto(binary, 144) ->
 125;
yeccgoto(binary, 148) ->
 125;
yeccgoto(binary, 159) ->
 125;
yeccgoto(binary, 163) ->
 125;
yeccgoto(binary, 182) ->
 125;
yeccgoto(binary, 203) ->
 125;
yeccgoto(binary, 240) ->
 125;
yeccgoto(binary, 242) ->
 125;
yeccgoto(binary, 246) ->
 125;
yeccgoto(binary, 251) ->
 125;
yeccgoto(binary, 254) ->
 125;
yeccgoto(binary, 256) ->
 125;
yeccgoto(binary, 258) ->
 125;
yeccgoto(binary, 266) ->
 125;
yeccgoto(binary, 273) ->
 125;
yeccgoto(binary, 275) ->
 125;
yeccgoto(binary, 293) ->
 125;
yeccgoto(binary, 296) ->
 125;
yeccgoto(binary, 307) ->
 125;
yeccgoto(binary_pattern, 142) ->
 171;
yeccgoto(binary_pattern, 177) ->
 171;
yeccgoto(binary_pattern, 179) ->
 171;
yeccgoto(binary_pattern, 180) ->
 171;
yeccgoto(binary_pattern, 181) ->
 171;
yeccgoto(binary_pattern, 183) ->
 171;
yeccgoto(binary_pattern, 186) ->
 171;
yeccgoto(binary_pattern, 191) ->
 171;
yeccgoto(binary_pattern, 199) ->
 171;
yeccgoto(binary_pattern, 207) ->
 171;
yeccgoto(binary_pattern, 209) ->
 171;
yeccgoto(binary_pattern, 228) ->
 171;
yeccgoto(binary_pattern, 231) ->
 171;
yeccgoto(binary_pattern, 262) ->
 171;
yeccgoto(call_expr, 108) ->
 124;
yeccgoto(call_expr, 130) ->
 124;
yeccgoto(call_expr, 131) ->
 124;
yeccgoto(call_expr, 132) ->
 124;
yeccgoto(call_expr, 133) ->
 124;
yeccgoto(call_expr, 135) ->
 124;
yeccgoto(call_expr, 136) ->
 124;
yeccgoto(call_expr, 137) ->
 124;
yeccgoto(call_expr, 138) ->
 124;
yeccgoto(call_expr, 143) ->
 124;
yeccgoto(call_expr, 144) ->
 124;
yeccgoto(call_expr, 148) ->
 124;
yeccgoto(call_expr, 159) ->
 124;
yeccgoto(call_expr, 163) ->
 124;
yeccgoto(call_expr, 182) ->
 124;
yeccgoto(call_expr, 203) ->
 124;
yeccgoto(call_expr, 240) ->
 124;
yeccgoto(call_expr, 242) ->
 124;
yeccgoto(call_expr, 246) ->
 124;
yeccgoto(call_expr, 251) ->
 124;
yeccgoto(call_expr, 254) ->
 124;
yeccgoto(call_expr, 256) ->
 124;
yeccgoto(call_expr, 258) ->
 124;
yeccgoto(call_expr, 266) ->
 124;
yeccgoto(call_expr, 273) ->
 124;
yeccgoto(call_expr, 275) ->
 124;
yeccgoto(call_expr, 293) ->
 124;
yeccgoto(call_expr, 296) ->
 124;
yeccgoto(call_expr, 307) ->
 124;
yeccgoto(case_expr, 108) ->
 123;
yeccgoto(case_expr, 130) ->
 123;
yeccgoto(case_expr, 131) ->
 123;
yeccgoto(case_expr, 132) ->
 123;
yeccgoto(case_expr, 133) ->
 123;
yeccgoto(case_expr, 135) ->
 123;
yeccgoto(case_expr, 136) ->
 123;
yeccgoto(case_expr, 137) ->
 123;
yeccgoto(case_expr, 138) ->
 123;
yeccgoto(case_expr, 143) ->
 123;
yeccgoto(case_expr, 144) ->
 123;
yeccgoto(case_expr, 148) ->
 123;
yeccgoto(case_expr, 159) ->
 123;
yeccgoto(case_expr, 163) ->
 123;
yeccgoto(case_expr, 182) ->
 123;
yeccgoto(case_expr, 203) ->
 123;
yeccgoto(case_expr, 240) ->
 123;
yeccgoto(case_expr, 242) ->
 123;
yeccgoto(case_expr, 246) ->
 123;
yeccgoto(case_expr, 251) ->
 123;
yeccgoto(case_expr, 254) ->
 123;
yeccgoto(case_expr, 256) ->
 123;
yeccgoto(case_expr, 258) ->
 123;
yeccgoto(case_expr, 266) ->
 123;
yeccgoto(case_expr, 273) ->
 123;
yeccgoto(case_expr, 275) ->
 123;
yeccgoto(case_expr, 293) ->
 123;
yeccgoto(case_expr, 296) ->
 123;
yeccgoto(case_expr, 307) ->
 123;
yeccgoto(catch_expr, 108) ->
 122;
yeccgoto(catch_expr, 130) ->
 122;
yeccgoto(catch_expr, 131) ->
 122;
yeccgoto(catch_expr, 132) ->
 122;
yeccgoto(catch_expr, 133) ->
 122;
yeccgoto(catch_expr, 135) ->
 122;
yeccgoto(catch_expr, 136) ->
 122;
yeccgoto(catch_expr, 137) ->
 122;
yeccgoto(catch_expr, 138) ->
 122;
yeccgoto(catch_expr, 143) ->
 122;
yeccgoto(catch_expr, 144) ->
 122;
yeccgoto(catch_expr, 148) ->
 122;
yeccgoto(catch_expr, 159) ->
 122;
yeccgoto(catch_expr, 163) ->
 122;
yeccgoto(catch_expr, 182) ->
 122;
yeccgoto(catch_expr, 203) ->
 122;
yeccgoto(catch_expr, 240) ->
 122;
yeccgoto(catch_expr, 242) ->
 122;
yeccgoto(catch_expr, 246) ->
 122;
yeccgoto(catch_expr, 251) ->
 122;
yeccgoto(catch_expr, 254) ->
 122;
yeccgoto(catch_expr, 256) ->
 122;
yeccgoto(catch_expr, 258) ->
 122;
yeccgoto(catch_expr, 266) ->
 122;
yeccgoto(catch_expr, 273) ->
 122;
yeccgoto(catch_expr, 275) ->
 122;
yeccgoto(catch_expr, 293) ->
 122;
yeccgoto(catch_expr, 296) ->
 122;
yeccgoto(catch_expr, 307) ->
 122;
yeccgoto(clause, 142) ->
 170;
yeccgoto(clause, 177) ->
 170;
yeccgoto(clause, 179) ->
 218;
yeccgoto(clause, 262) ->
 170;
yeccgoto(clause_pattern, 142) ->
 169;
yeccgoto(clause_pattern, 177) ->
 169;
yeccgoto(clause_pattern, 179) ->
 169;
yeccgoto(clause_pattern, 262) ->
 169;
yeccgoto(cons, 108) ->
 121;
yeccgoto(cons, 130) ->
 121;
yeccgoto(cons, 131) ->
 121;
yeccgoto(cons, 132) ->
 121;
yeccgoto(cons, 133) ->
 121;
yeccgoto(cons, 135) ->
 121;
yeccgoto(cons, 136) ->
 121;
yeccgoto(cons, 137) ->
 121;
yeccgoto(cons, 138) ->
 121;
yeccgoto(cons, 143) ->
 121;
yeccgoto(cons, 144) ->
 121;
yeccgoto(cons, 148) ->
 121;
yeccgoto(cons, 159) ->
 121;
yeccgoto(cons, 163) ->
 121;
yeccgoto(cons, 182) ->
 121;
yeccgoto(cons, 203) ->
 121;
yeccgoto(cons, 240) ->
 121;
yeccgoto(cons, 242) ->
 121;
yeccgoto(cons, 246) ->
 121;
yeccgoto(cons, 251) ->
 121;
yeccgoto(cons, 254) ->
 121;
yeccgoto(cons, 256) ->
 121;
yeccgoto(cons, 258) ->
 121;
yeccgoto(cons, 266) ->
 121;
yeccgoto(cons, 273) ->
 121;
yeccgoto(cons, 275) ->
 121;
yeccgoto(cons, 293) ->
 121;
yeccgoto(cons, 296) ->
 121;
yeccgoto(cons, 307) ->
 121;
yeccgoto(cons_constant, 65) ->
 70;
yeccgoto(cons_constant, 72) ->
 70;
yeccgoto(cons_constant, 79) ->
 70;
yeccgoto(cons_constant, 85) ->
 70;
yeccgoto(cons_constant, 87) ->
 70;
yeccgoto(cons_constant, 92) ->
 70;
yeccgoto(cons_literal, 24) ->
 28;
yeccgoto(cons_literal, 30) ->
 28;
yeccgoto(cons_literal, 36) ->
 28;
yeccgoto(cons_literal, 40) ->
 28;
yeccgoto(cons_literal, 46) ->
 28;
yeccgoto(cons_literal, 48) ->
 28;
yeccgoto(cons_pattern, 142) ->
 168;
yeccgoto(cons_pattern, 177) ->
 168;
yeccgoto(cons_pattern, 179) ->
 168;
yeccgoto(cons_pattern, 180) ->
 168;
yeccgoto(cons_pattern, 181) ->
 168;
yeccgoto(cons_pattern, 183) ->
 168;
yeccgoto(cons_pattern, 186) ->
 168;
yeccgoto(cons_pattern, 191) ->
 168;
yeccgoto(cons_pattern, 199) ->
 168;
yeccgoto(cons_pattern, 207) ->
 168;
yeccgoto(cons_pattern, 209) ->
 168;
yeccgoto(cons_pattern, 228) ->
 168;
yeccgoto(cons_pattern, 231) ->
 168;
yeccgoto(cons_pattern, 262) ->
 168;
yeccgoto(constant, 65) ->
 69;
yeccgoto(constant, 72) ->
 83;
yeccgoto(constant, 79) ->
 69;
yeccgoto(constant, 85) ->
 90;
yeccgoto(constant, 87) ->
 88;
yeccgoto(constant, 92) ->
 69;
yeccgoto(constants, 65) ->
 68;
yeccgoto(constants, 79) ->
 80;
yeccgoto(constants, 92) ->
 93;
yeccgoto(exported_name, 6) ->
 9;
yeccgoto(exported_name, 14) ->
 9;
yeccgoto(exported_names, 6) ->
 8;
yeccgoto(exported_names, 14) ->
 15;
yeccgoto(expression, 108) ->
 120;
yeccgoto(expression, 130) ->
 283;
yeccgoto(expression, 131) ->
 120;
yeccgoto(expression, 132) ->
 120;
yeccgoto(expression, 133) ->
 120;
yeccgoto(expression, 135) ->
 120;
yeccgoto(expression, 136) ->
 120;
yeccgoto(expression, 137) ->
 120;
yeccgoto(expression, 138) ->
 120;
yeccgoto(expression, 143) ->
 120;
yeccgoto(expression, 144) ->
 120;
yeccgoto(expression, 148) ->
 120;
yeccgoto(expression, 159) ->
 120;
yeccgoto(expression, 163) ->
 120;
yeccgoto(expression, 182) ->
 120;
yeccgoto(expression, 203) ->
 120;
yeccgoto(expression, 240) ->
 120;
yeccgoto(expression, 242) ->
 120;
yeccgoto(expression, 246) ->
 120;
yeccgoto(expression, 251) ->
 120;
yeccgoto(expression, 254) ->
 120;
yeccgoto(expression, 256) ->
 120;
yeccgoto(expression, 258) ->
 120;
yeccgoto(expression, 266) ->
 120;
yeccgoto(expression, 273) ->
 120;
yeccgoto(expression, 275) ->
 120;
yeccgoto(expression, 293) ->
 120;
yeccgoto(expression, 296) ->
 120;
yeccgoto(expression, 307) ->
 120;
yeccgoto(fun_expr, 96) ->
 97;
yeccgoto(fun_expr, 99) ->
 309;
yeccgoto(fun_expr, 108) ->
 119;
yeccgoto(fun_expr, 130) ->
 119;
yeccgoto(fun_expr, 131) ->
 119;
yeccgoto(fun_expr, 132) ->
 119;
yeccgoto(fun_expr, 133) ->
 119;
yeccgoto(fun_expr, 135) ->
 119;
yeccgoto(fun_expr, 136) ->
 119;
yeccgoto(fun_expr, 137) ->
 119;
yeccgoto(fun_expr, 138) ->
 119;
yeccgoto(fun_expr, 143) ->
 119;
yeccgoto(fun_expr, 144) ->
 119;
yeccgoto(fun_expr, 148) ->
 119;
yeccgoto(fun_expr, 159) ->
 119;
yeccgoto(fun_expr, 163) ->
 119;
yeccgoto(fun_expr, 182) ->
 119;
yeccgoto(fun_expr, 203) ->
 119;
yeccgoto(fun_expr, 240) ->
 119;
yeccgoto(fun_expr, 242) ->
 119;
yeccgoto(fun_expr, 246) ->
 119;
yeccgoto(fun_expr, 251) ->
 119;
yeccgoto(fun_expr, 254) ->
 119;
yeccgoto(fun_expr, 256) ->
 119;
yeccgoto(fun_expr, 258) ->
 119;
yeccgoto(fun_expr, 266) ->
 119;
yeccgoto(fun_expr, 273) ->
 119;
yeccgoto(fun_expr, 275) ->
 119;
yeccgoto(fun_expr, 293) ->
 119;
yeccgoto(fun_expr, 296) ->
 119;
yeccgoto(fun_expr, 307) ->
 119;
yeccgoto(function_definition, 17) ->
 59;
yeccgoto(function_definition, 59) ->
 59;
yeccgoto(function_definition, 140) ->
 59;
yeccgoto(function_definition, 318) ->
 59;
yeccgoto(function_definitions, 17) ->
 58;
yeccgoto(function_definitions, 59) ->
 313;
yeccgoto(function_definitions, 140) ->
 250;
yeccgoto(function_definitions, 318) ->
 58;
yeccgoto(function_name, 6) ->
 7;
yeccgoto(function_name, 14) ->
 7;
yeccgoto(function_name, 17) ->
 57;
yeccgoto(function_name, 59) ->
 57;
yeccgoto(function_name, 61) ->
 62;
yeccgoto(function_name, 108) ->
 118;
yeccgoto(function_name, 130) ->
 118;
yeccgoto(function_name, 131) ->
 118;
yeccgoto(function_name, 132) ->
 118;
yeccgoto(function_name, 133) ->
 118;
yeccgoto(function_name, 135) ->
 118;
yeccgoto(function_name, 136) ->
 118;
yeccgoto(function_name, 137) ->
 118;
yeccgoto(function_name, 138) ->
 118;
yeccgoto(function_name, 140) ->
 57;
yeccgoto(function_name, 143) ->
 118;
yeccgoto(function_name, 144) ->
 118;
yeccgoto(function_name, 148) ->
 118;
yeccgoto(function_name, 159) ->
 118;
yeccgoto(function_name, 163) ->
 118;
yeccgoto(function_name, 182) ->
 118;
yeccgoto(function_name, 203) ->
 118;
yeccgoto(function_name, 240) ->
 118;
yeccgoto(function_name, 242) ->
 118;
yeccgoto(function_name, 246) ->
 118;
yeccgoto(function_name, 251) ->
 118;
yeccgoto(function_name, 254) ->
 118;
yeccgoto(function_name, 256) ->
 118;
yeccgoto(function_name, 258) ->
 118;
yeccgoto(function_name, 266) ->
 118;
yeccgoto(function_name, 273) ->
 118;
yeccgoto(function_name, 275) ->
 118;
yeccgoto(function_name, 293) ->
 118;
yeccgoto(function_name, 296) ->
 118;
yeccgoto(function_name, 307) ->
 118;
yeccgoto(function_name, 318) ->
 57;
yeccgoto(let_expr, 108) ->
 117;
yeccgoto(let_expr, 130) ->
 117;
yeccgoto(let_expr, 131) ->
 117;
yeccgoto(let_expr, 132) ->
 117;
yeccgoto(let_expr, 133) ->
 117;
yeccgoto(let_expr, 135) ->
 117;
yeccgoto(let_expr, 136) ->
 117;
yeccgoto(let_expr, 137) ->
 117;
yeccgoto(let_expr, 138) ->
 117;
yeccgoto(let_expr, 143) ->
 117;
yeccgoto(let_expr, 144) ->
 117;
yeccgoto(let_expr, 148) ->
 117;
yeccgoto(let_expr, 159) ->
 117;
yeccgoto(let_expr, 163) ->
 117;
yeccgoto(let_expr, 182) ->
 117;
yeccgoto(let_expr, 203) ->
 117;
yeccgoto(let_expr, 240) ->
 117;
yeccgoto(let_expr, 242) ->
 117;
yeccgoto(let_expr, 246) ->
 117;
yeccgoto(let_expr, 251) ->
 117;
yeccgoto(let_expr, 254) ->
 117;
yeccgoto(let_expr, 256) ->
 117;
yeccgoto(let_expr, 258) ->
 117;
yeccgoto(let_expr, 266) ->
 117;
yeccgoto(let_expr, 273) ->
 117;
yeccgoto(let_expr, 275) ->
 117;
yeccgoto(let_expr, 293) ->
 117;
yeccgoto(let_expr, 296) ->
 117;
yeccgoto(let_expr, 307) ->
 117;
yeccgoto(let_vars, 139) ->
 253;
yeccgoto(let_vars, 152) ->
 153;
yeccgoto(let_vars, 161) ->
 162;
yeccgoto(letrec_expr, 108) ->
 116;
yeccgoto(letrec_expr, 130) ->
 116;
yeccgoto(letrec_expr, 131) ->
 116;
yeccgoto(letrec_expr, 132) ->
 116;
yeccgoto(letrec_expr, 133) ->
 116;
yeccgoto(letrec_expr, 135) ->
 116;
yeccgoto(letrec_expr, 136) ->
 116;
yeccgoto(letrec_expr, 137) ->
 116;
yeccgoto(letrec_expr, 138) ->
 116;
yeccgoto(letrec_expr, 143) ->
 116;
yeccgoto(letrec_expr, 144) ->
 116;
yeccgoto(letrec_expr, 148) ->
 116;
yeccgoto(letrec_expr, 159) ->
 116;
yeccgoto(letrec_expr, 163) ->
 116;
yeccgoto(letrec_expr, 182) ->
 116;
yeccgoto(letrec_expr, 203) ->
 116;
yeccgoto(letrec_expr, 240) ->
 116;
yeccgoto(letrec_expr, 242) ->
 116;
yeccgoto(letrec_expr, 246) ->
 116;
yeccgoto(letrec_expr, 251) ->
 116;
yeccgoto(letrec_expr, 254) ->
 116;
yeccgoto(letrec_expr, 256) ->
 116;
yeccgoto(letrec_expr, 258) ->
 116;
yeccgoto(letrec_expr, 266) ->
 116;
yeccgoto(letrec_expr, 273) ->
 116;
yeccgoto(letrec_expr, 275) ->
 116;
yeccgoto(letrec_expr, 293) ->
 116;
yeccgoto(letrec_expr, 296) ->
 116;
yeccgoto(letrec_expr, 307) ->
 116;
yeccgoto(literal, 24) ->
 27;
yeccgoto(literal, 30) ->
 43;
yeccgoto(literal, 36) ->
 38;
yeccgoto(literal, 40) ->
 38;
yeccgoto(literal, 46) ->
 51;
yeccgoto(literal, 48) ->
 49;
yeccgoto(literals, 36) ->
 37;
yeccgoto(literals, 40) ->
 41;
yeccgoto(module_attribute, 5) ->
 17;
yeccgoto(module_attribute, 317) ->
 318;
yeccgoto(module_definition, 0) ->
 1;
yeccgoto(module_defs, 17) ->
 56;
yeccgoto(module_defs, 318) ->
 319;
yeccgoto(module_export, 4) ->
 5;
yeccgoto(module_export, 316) ->
 317;
yeccgoto(nil, 24) ->
 26;
yeccgoto(nil, 30) ->
 26;
yeccgoto(nil, 36) ->
 26;
yeccgoto(nil, 40) ->
 26;
yeccgoto(nil, 46) ->
 26;
yeccgoto(nil, 48) ->
 26;
yeccgoto(nil, 65) ->
 67;
yeccgoto(nil, 72) ->
 67;
yeccgoto(nil, 79) ->
 67;
yeccgoto(nil, 85) ->
 67;
yeccgoto(nil, 87) ->
 67;
yeccgoto(nil, 92) ->
 67;
yeccgoto(nil, 108) ->
 26;
yeccgoto(nil, 130) ->
 26;
yeccgoto(nil, 131) ->
 26;
yeccgoto(nil, 132) ->
 26;
yeccgoto(nil, 133) ->
 26;
yeccgoto(nil, 135) ->
 26;
yeccgoto(nil, 136) ->
 26;
yeccgoto(nil, 137) ->
 26;
yeccgoto(nil, 138) ->
 26;
yeccgoto(nil, 142) ->
 26;
yeccgoto(nil, 143) ->
 26;
yeccgoto(nil, 144) ->
 26;
yeccgoto(nil, 148) ->
 26;
yeccgoto(nil, 159) ->
 26;
yeccgoto(nil, 163) ->
 26;
yeccgoto(nil, 177) ->
 26;
yeccgoto(nil, 179) ->
 26;
yeccgoto(nil, 180) ->
 26;
yeccgoto(nil, 181) ->
 26;
yeccgoto(nil, 182) ->
 26;
yeccgoto(nil, 183) ->
 26;
yeccgoto(nil, 186) ->
 26;
yeccgoto(nil, 191) ->
 26;
yeccgoto(nil, 199) ->
 26;
yeccgoto(nil, 203) ->
 26;
yeccgoto(nil, 207) ->
 26;
yeccgoto(nil, 209) ->
 26;
yeccgoto(nil, 228) ->
 26;
yeccgoto(nil, 231) ->
 26;
yeccgoto(nil, 240) ->
 26;
yeccgoto(nil, 242) ->
 26;
yeccgoto(nil, 246) ->
 26;
yeccgoto(nil, 251) ->
 26;
yeccgoto(nil, 254) ->
 26;
yeccgoto(nil, 256) ->
 26;
yeccgoto(nil, 258) ->
 26;
yeccgoto(nil, 262) ->
 26;
yeccgoto(nil, 266) ->
 26;
yeccgoto(nil, 273) ->
 26;
yeccgoto(nil, 275) ->
 26;
yeccgoto(nil, 293) ->
 26;
yeccgoto(nil, 296) ->
 26;
yeccgoto(nil, 307) ->
 26;
yeccgoto(other_pattern, 142) ->
 167;
yeccgoto(other_pattern, 177) ->
 167;
yeccgoto(other_pattern, 179) ->
 217;
yeccgoto(other_pattern, 180) ->
 167;
yeccgoto(other_pattern, 181) ->
 167;
yeccgoto(other_pattern, 183) ->
 167;
yeccgoto(other_pattern, 186) ->
 189;
yeccgoto(other_pattern, 191) ->
 167;
yeccgoto(other_pattern, 199) ->
 167;
yeccgoto(other_pattern, 207) ->
 167;
yeccgoto(other_pattern, 209) ->
 167;
yeccgoto(other_pattern, 228) ->
 167;
yeccgoto(other_pattern, 231) ->
 167;
yeccgoto(other_pattern, 262) ->
 167;
yeccgoto(primop_expr, 108) ->
 115;
yeccgoto(primop_expr, 130) ->
 115;
yeccgoto(primop_expr, 131) ->
 115;
yeccgoto(primop_expr, 132) ->
 115;
yeccgoto(primop_expr, 133) ->
 115;
yeccgoto(primop_expr, 135) ->
 115;
yeccgoto(primop_expr, 136) ->
 115;
yeccgoto(primop_expr, 137) ->
 115;
yeccgoto(primop_expr, 138) ->
 115;
yeccgoto(primop_expr, 143) ->
 115;
yeccgoto(primop_expr, 144) ->
 115;
yeccgoto(primop_expr, 148) ->
 115;
yeccgoto(primop_expr, 159) ->
 115;
yeccgoto(primop_expr, 163) ->
 115;
yeccgoto(primop_expr, 182) ->
 115;
yeccgoto(primop_expr, 203) ->
 115;
yeccgoto(primop_expr, 240) ->
 115;
yeccgoto(primop_expr, 242) ->
 115;
yeccgoto(primop_expr, 246) ->
 115;
yeccgoto(primop_expr, 251) ->
 115;
yeccgoto(primop_expr, 254) ->
 115;
yeccgoto(primop_expr, 256) ->
 115;
yeccgoto(primop_expr, 258) ->
 115;
yeccgoto(primop_expr, 266) ->
 115;
yeccgoto(primop_expr, 273) ->
 115;
yeccgoto(primop_expr, 275) ->
 115;
yeccgoto(primop_expr, 293) ->
 115;
yeccgoto(primop_expr, 296) ->
 115;
yeccgoto(primop_expr, 307) ->
 115;
yeccgoto(receive_expr, 108) ->
 114;
yeccgoto(receive_expr, 130) ->
 114;
yeccgoto(receive_expr, 131) ->
 114;
yeccgoto(receive_expr, 132) ->
 114;
yeccgoto(receive_expr, 133) ->
 114;
yeccgoto(receive_expr, 135) ->
 114;
yeccgoto(receive_expr, 136) ->
 114;
yeccgoto(receive_expr, 137) ->
 114;
yeccgoto(receive_expr, 138) ->
 114;
yeccgoto(receive_expr, 143) ->
 114;
yeccgoto(receive_expr, 144) ->
 114;
yeccgoto(receive_expr, 148) ->
 114;
yeccgoto(receive_expr, 159) ->
 114;
yeccgoto(receive_expr, 163) ->
 114;
yeccgoto(receive_expr, 182) ->
 114;
yeccgoto(receive_expr, 203) ->
 114;
yeccgoto(receive_expr, 240) ->
 114;
yeccgoto(receive_expr, 242) ->
 114;
yeccgoto(receive_expr, 246) ->
 114;
yeccgoto(receive_expr, 251) ->
 114;
yeccgoto(receive_expr, 254) ->
 114;
yeccgoto(receive_expr, 256) ->
 114;
yeccgoto(receive_expr, 258) ->
 114;
yeccgoto(receive_expr, 266) ->
 114;
yeccgoto(receive_expr, 273) ->
 114;
yeccgoto(receive_expr, 275) ->
 114;
yeccgoto(receive_expr, 293) ->
 114;
yeccgoto(receive_expr, 296) ->
 114;
yeccgoto(receive_expr, 307) ->
 114;
yeccgoto(segment, 287) ->
 289;
yeccgoto(segment, 299) ->
 289;
yeccgoto(segment_pattern, 222) ->
 224;
yeccgoto(segment_pattern, 234) ->
 224;
yeccgoto(segment_patterns, 222) ->
 223;
yeccgoto(segment_patterns, 234) ->
 235;
yeccgoto(segments, 287) ->
 288;
yeccgoto(segments, 299) ->
 300;
yeccgoto(sequence, 108) ->
 113;
yeccgoto(sequence, 130) ->
 113;
yeccgoto(sequence, 131) ->
 113;
yeccgoto(sequence, 132) ->
 113;
yeccgoto(sequence, 133) ->
 113;
yeccgoto(sequence, 135) ->
 113;
yeccgoto(sequence, 136) ->
 113;
yeccgoto(sequence, 137) ->
 113;
yeccgoto(sequence, 138) ->
 113;
yeccgoto(sequence, 143) ->
 113;
yeccgoto(sequence, 144) ->
 113;
yeccgoto(sequence, 148) ->
 113;
yeccgoto(sequence, 159) ->
 113;
yeccgoto(sequence, 163) ->
 113;
yeccgoto(sequence, 182) ->
 113;
yeccgoto(sequence, 203) ->
 113;
yeccgoto(sequence, 240) ->
 113;
yeccgoto(sequence, 242) ->
 113;
yeccgoto(sequence, 246) ->
 113;
yeccgoto(sequence, 251) ->
 113;
yeccgoto(sequence, 254) ->
 113;
yeccgoto(sequence, 256) ->
 113;
yeccgoto(sequence, 258) ->
 113;
yeccgoto(sequence, 266) ->
 113;
yeccgoto(sequence, 273) ->
 113;
yeccgoto(sequence, 275) ->
 113;
yeccgoto(sequence, 293) ->
 113;
yeccgoto(sequence, 296) ->
 113;
yeccgoto(sequence, 307) ->
 113;
yeccgoto(single_expression, 108) ->
 112;
yeccgoto(single_expression, 130) ->
 112;
yeccgoto(single_expression, 131) ->
 112;
yeccgoto(single_expression, 132) ->
 112;
yeccgoto(single_expression, 133) ->
 112;
yeccgoto(single_expression, 135) ->
 112;
yeccgoto(single_expression, 136) ->
 112;
yeccgoto(single_expression, 137) ->
 112;
yeccgoto(single_expression, 138) ->
 112;
yeccgoto(single_expression, 143) ->
 112;
yeccgoto(single_expression, 144) ->
 112;
yeccgoto(single_expression, 148) ->
 112;
yeccgoto(single_expression, 159) ->
 112;
yeccgoto(single_expression, 163) ->
 112;
yeccgoto(single_expression, 182) ->
 112;
yeccgoto(single_expression, 203) ->
 112;
yeccgoto(single_expression, 240) ->
 112;
yeccgoto(single_expression, 242) ->
 112;
yeccgoto(single_expression, 246) ->
 112;
yeccgoto(single_expression, 251) ->
 112;
yeccgoto(single_expression, 254) ->
 112;
yeccgoto(single_expression, 256) ->
 112;
yeccgoto(single_expression, 258) ->
 112;
yeccgoto(single_expression, 266) ->
 112;
yeccgoto(single_expression, 273) ->
 112;
yeccgoto(single_expression, 275) ->
 112;
yeccgoto(single_expression, 293) ->
 112;
yeccgoto(single_expression, 296) ->
 112;
yeccgoto(single_expression, 307) ->
 112;
yeccgoto(tail, 271) ->
 272;
yeccgoto(tail, 278) ->
 279;
yeccgoto(tail_constant, 83) ->
 84;
yeccgoto(tail_constant, 90) ->
 91;
yeccgoto(tail_literal, 43) ->
 45;
yeccgoto(tail_literal, 51) ->
 52;
yeccgoto(tail_pattern, 205) ->
 206;
yeccgoto(tail_pattern, 212) ->
 213;
yeccgoto(timeout, 142) ->
 166;
yeccgoto(timeout, 176) ->
 239;
yeccgoto(try_expr, 108) ->
 111;
yeccgoto(try_expr, 130) ->
 111;
yeccgoto(try_expr, 131) ->
 111;
yeccgoto(try_expr, 132) ->
 111;
yeccgoto(try_expr, 133) ->
 111;
yeccgoto(try_expr, 135) ->
 111;
yeccgoto(try_expr, 136) ->
 111;
yeccgoto(try_expr, 137) ->
 111;
yeccgoto(try_expr, 138) ->
 111;
yeccgoto(try_expr, 143) ->
 111;
yeccgoto(try_expr, 144) ->
 111;
yeccgoto(try_expr, 148) ->
 111;
yeccgoto(try_expr, 159) ->
 111;
yeccgoto(try_expr, 163) ->
 111;
yeccgoto(try_expr, 182) ->
 111;
yeccgoto(try_expr, 203) ->
 111;
yeccgoto(try_expr, 240) ->
 111;
yeccgoto(try_expr, 242) ->
 111;
yeccgoto(try_expr, 246) ->
 111;
yeccgoto(try_expr, 251) ->
 111;
yeccgoto(try_expr, 254) ->
 111;
yeccgoto(try_expr, 256) ->
 111;
yeccgoto(try_expr, 258) ->
 111;
yeccgoto(try_expr, 266) ->
 111;
yeccgoto(try_expr, 273) ->
 111;
yeccgoto(try_expr, 275) ->
 111;
yeccgoto(try_expr, 293) ->
 111;
yeccgoto(try_expr, 296) ->
 111;
yeccgoto(try_expr, 307) ->
 111;
yeccgoto(tuple, 108) ->
 110;
yeccgoto(tuple, 130) ->
 110;
yeccgoto(tuple, 131) ->
 110;
yeccgoto(tuple, 132) ->
 110;
yeccgoto(tuple, 133) ->
 110;
yeccgoto(tuple, 135) ->
 110;
yeccgoto(tuple, 136) ->
 110;
yeccgoto(tuple, 137) ->
 110;
yeccgoto(tuple, 138) ->
 110;
yeccgoto(tuple, 143) ->
 110;
yeccgoto(tuple, 144) ->
 110;
yeccgoto(tuple, 148) ->
 110;
yeccgoto(tuple, 159) ->
 110;
yeccgoto(tuple, 163) ->
 110;
yeccgoto(tuple, 182) ->
 110;
yeccgoto(tuple, 203) ->
 110;
yeccgoto(tuple, 240) ->
 110;
yeccgoto(tuple, 242) ->
 110;
yeccgoto(tuple, 246) ->
 110;
yeccgoto(tuple, 251) ->
 110;
yeccgoto(tuple, 254) ->
 110;
yeccgoto(tuple, 256) ->
 110;
yeccgoto(tuple, 258) ->
 110;
yeccgoto(tuple, 266) ->
 110;
yeccgoto(tuple, 273) ->
 110;
yeccgoto(tuple, 275) ->
 110;
yeccgoto(tuple, 293) ->
 110;
yeccgoto(tuple, 296) ->
 110;
yeccgoto(tuple, 307) ->
 110;
yeccgoto(tuple_constant, 65) ->
 66;
yeccgoto(tuple_constant, 72) ->
 66;
yeccgoto(tuple_constant, 79) ->
 66;
yeccgoto(tuple_constant, 85) ->
 66;
yeccgoto(tuple_constant, 87) ->
 66;
yeccgoto(tuple_constant, 92) ->
 66;
yeccgoto(tuple_literal, 24) ->
 25;
yeccgoto(tuple_literal, 30) ->
 25;
yeccgoto(tuple_literal, 36) ->
 25;
yeccgoto(tuple_literal, 40) ->
 25;
yeccgoto(tuple_literal, 46) ->
 25;
yeccgoto(tuple_literal, 48) ->
 25;
yeccgoto(tuple_pattern, 142) ->
 165;
yeccgoto(tuple_pattern, 177) ->
 165;
yeccgoto(tuple_pattern, 179) ->
 165;
yeccgoto(tuple_pattern, 180) ->
 165;
yeccgoto(tuple_pattern, 181) ->
 165;
yeccgoto(tuple_pattern, 183) ->
 165;
yeccgoto(tuple_pattern, 186) ->
 165;
yeccgoto(tuple_pattern, 191) ->
 165;
yeccgoto(tuple_pattern, 199) ->
 165;
yeccgoto(tuple_pattern, 207) ->
 165;
yeccgoto(tuple_pattern, 209) ->
 165;
yeccgoto(tuple_pattern, 228) ->
 165;
yeccgoto(tuple_pattern, 231) ->
 165;
yeccgoto(tuple_pattern, 262) ->
 165;
yeccgoto(variable, 101) ->
 102;
yeccgoto(variable, 105) ->
 303;
yeccgoto(variable, 108) ->
 109;
yeccgoto(variable, 130) ->
 109;
yeccgoto(variable, 131) ->
 109;
yeccgoto(variable, 132) ->
 109;
yeccgoto(variable, 133) ->
 109;
yeccgoto(variable, 135) ->
 109;
yeccgoto(variable, 136) ->
 109;
yeccgoto(variable, 137) ->
 109;
yeccgoto(variable, 138) ->
 109;
yeccgoto(variable, 139) ->
 102;
yeccgoto(variable, 142) ->
 102;
yeccgoto(variable, 143) ->
 109;
yeccgoto(variable, 144) ->
 109;
yeccgoto(variable, 148) ->
 109;
yeccgoto(variable, 152) ->
 102;
yeccgoto(variable, 155) ->
 102;
yeccgoto(variable, 159) ->
 109;
yeccgoto(variable, 161) ->
 102;
yeccgoto(variable, 163) ->
 109;
yeccgoto(variable, 177) ->
 102;
yeccgoto(variable, 179) ->
 188;
yeccgoto(variable, 180) ->
 102;
yeccgoto(variable, 181) ->
 102;
yeccgoto(variable, 182) ->
 109;
yeccgoto(variable, 183) ->
 102;
yeccgoto(variable, 186) ->
 188;
yeccgoto(variable, 191) ->
 102;
yeccgoto(variable, 199) ->
 102;
yeccgoto(variable, 203) ->
 109;
yeccgoto(variable, 207) ->
 102;
yeccgoto(variable, 209) ->
 102;
yeccgoto(variable, 228) ->
 102;
yeccgoto(variable, 231) ->
 102;
yeccgoto(variable, 240) ->
 109;
yeccgoto(variable, 242) ->
 109;
yeccgoto(variable, 246) ->
 109;
yeccgoto(variable, 251) ->
 109;
yeccgoto(variable, 254) ->
 109;
yeccgoto(variable, 256) ->
 109;
yeccgoto(variable, 258) ->
 109;
yeccgoto(variable, 262) ->
 102;
yeccgoto(variable, 266) ->
 109;
yeccgoto(variable, 273) ->
 109;
yeccgoto(variable, 275) ->
 109;
yeccgoto(variable, 293) ->
 109;
yeccgoto(variable, 296) ->
 109;
yeccgoto(variable, 304) ->
 102;
yeccgoto(variable, 307) ->
 109;
yeccgoto(__Symbol, __State) ->
 erlang:error({yecc_bug,"1.1",{__Symbol, __State, missing_in_goto_table}}).

-compile({inline,{yeccpars2_9_,1}}).
-file("core_parse.yrl", 90).
yeccpars2_9_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_10_,1}}).
-file("core_parse.yrl", 86).
yeccpars2_10_([__2,__1 | __Stack]) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_13_,1}}).
-file("core_parse.yrl", 289).
yeccpars2_13_([__3,__2,__1 | __Stack]) ->
 [begin
   # c_fname { id = tok_val ( __1 ) , arity = tok_val ( __3 ) }
  end | __Stack].

-compile({inline,{yeccpars2_15_,1}}).
-file("core_parse.yrl", 89).
yeccpars2_15_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_16_,1}}).
-file("core_parse.yrl", 87).
yeccpars2_16_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_17_,1}}).
-file("core_parse.yrl", 111).
yeccpars2_17_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_21_,1}}).
-file("core_parse.yrl", 98).
yeccpars2_21_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_22_,1}}).
-file("core_parse.yrl", 94).
yeccpars2_22_([__3,__2,__1 | __Stack]) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_26_,1}}).
-file("core_parse.yrl", 253).
yeccpars2_26_([__1 | __Stack]) ->
 [begin
   # c_literal { val = [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_27_,1}}).
-file("core_parse.yrl", 101).
yeccpars2_27_([__3,__2,__1 | __Stack]) ->
 [begin
   { # c_literal { val = tok_val ( __1 ) } , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_31_,1}}).
-file("core_parse.yrl", 251).
yeccpars2_31_([__1 | __Stack]) ->
 [begin
   # c_literal { val = tok_val ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_32_,1}}).
-file("core_parse.yrl", 248).
yeccpars2_32_([__1 | __Stack]) ->
 [begin
   # c_literal { val = tok_val ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_33_,1}}).
-file("core_parse.yrl", 250).
yeccpars2_33_([__1 | __Stack]) ->
 [begin
   # c_literal { val = tok_val ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_34_,1}}).
-file("core_parse.yrl", 249).
yeccpars2_34_([__1 | __Stack]) ->
 [begin
   # c_literal { val = tok_val ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_35_,1}}).
-file("core_parse.yrl", 252).
yeccpars2_35_([__1 | __Stack]) ->
 [begin
   # c_literal { val = tok_val ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_38_,1}}).
-file("core_parse.yrl", 246).
yeccpars2_38_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_39_,1}}).
-file("core_parse.yrl", 255).
yeccpars2_39_([__2,__1 | __Stack]) ->
 [begin
   # c_tuple { es = [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_41_,1}}).
-file("core_parse.yrl", 245).
yeccpars2_41_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_42_,1}}).
-file("core_parse.yrl", 256).
yeccpars2_42_([__3,__2,__1 | __Stack]) ->
 [begin
   # c_tuple { es = __2 }
  end | __Stack].

-compile({inline,{yeccpars2_44_,1}}).
-file("core_parse.yrl", 67).
yeccpars2_44_([__2,__1 | __Stack]) ->
 [begin
   { nil , tok_line ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_45_,1}}).
-file("core_parse.yrl", 258).
yeccpars2_45_([__3,__2,__1 | __Stack]) ->
 [begin
   # c_cons { hd = __2 , tl = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_47_,1}}).
-file("core_parse.yrl", 260).
yeccpars2_47_([__1 | __Stack]) ->
 [begin
   # c_literal { val = [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_50_,1}}).
-file("core_parse.yrl", 261).
yeccpars2_50_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_52_,1}}).
-file("core_parse.yrl", 262).
yeccpars2_52_([__3,__2,__1 | __Stack]) ->
 [begin
   # c_cons { hd = __2 , tl = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_54_,1}}).
-file("core_parse.yrl", 97).
yeccpars2_54_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_55_,1}}).
-file("core_parse.yrl", 95).
yeccpars2_55_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_59_,1}}).
-file("core_parse.yrl", 111).
yeccpars2_59_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_67_,1}}).
-file("core_parse.yrl", 136).
yeccpars2_67_([__1 | __Stack]) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_69_,1}}).
-file("core_parse.yrl", 129).
yeccpars2_69_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_73_,1}}).
-file("core_parse.yrl", 105).
yeccpars2_73_([__2,__1 | __Stack]) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_74_,1}}).
-file("core_parse.yrl", 134).
yeccpars2_74_([__1 | __Stack]) ->
 [begin
   tok_val ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_75_,1}}).
-file("core_parse.yrl", 131).
yeccpars2_75_([__1 | __Stack]) ->
 [begin
   tok_val ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_76_,1}}).
-file("core_parse.yrl", 133).
yeccpars2_76_([__1 | __Stack]) ->
 [begin
   tok_val ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_77_,1}}).
-file("core_parse.yrl", 132).
yeccpars2_77_([__1 | __Stack]) ->
 [begin
   tok_val ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_78_,1}}).
-file("core_parse.yrl", 135).
yeccpars2_78_([__1 | __Stack]) ->
 [begin
   tok_val ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_81_,1}}).
-file("core_parse.yrl", 138).
yeccpars2_81_([__2,__1 | __Stack]) ->
 [begin
   { }
  end | __Stack].

-compile({inline,{yeccpars2_82_,1}}).
-file("core_parse.yrl", 139).
yeccpars2_82_([__3,__2,__1 | __Stack]) ->
 [begin
   list_to_tuple ( __2 )
  end | __Stack].

-compile({inline,{yeccpars2_84_,1}}).
-file("core_parse.yrl", 141).
yeccpars2_84_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_86_,1}}).
-file("core_parse.yrl", 143).
yeccpars2_86_([__1 | __Stack]) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_89_,1}}).
-file("core_parse.yrl", 144).
yeccpars2_89_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_91_,1}}).
-file("core_parse.yrl", 145).
yeccpars2_91_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_93_,1}}).
-file("core_parse.yrl", 128).
yeccpars2_93_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_94_,1}}).
-file("core_parse.yrl", 106).
yeccpars2_94_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_95_,1}}).
-file("core_parse.yrl", 293).
yeccpars2_95_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   core_lib : set_anno ( __2 , __4 )
  end | __Stack].

-compile({inline,{yeccpars2_98_,1}}).
-file("core_parse.yrl", 115).
yeccpars2_98_([__3,__2,__1 | __Stack]) ->
 [begin
   { __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_104_,1}}).
-file("core_parse.yrl", 202).
yeccpars2_104_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_107_,1}}).
-file("core_parse.yrl", 199).
yeccpars2_107_([__1 | __Stack]) ->
 [begin
   # c_var { name = tok_val ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_128_,1}}).
-file("core_parse.yrl", 303).
yeccpars2_128_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # c_fun { vars = [ ] , body = __5 }
  end | __Stack].

-compile({inline,{yeccpars2_134_,1}}).
-file("core_parse.yrl", 251).
yeccpars2_134_([__1 | __Stack]) ->
 [begin
   # c_literal { val = tok_val ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_140_,1}}).
-file("core_parse.yrl", 111).
yeccpars2_140_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_146_,1}}).
-file("core_parse.yrl", 217).
yeccpars2_146_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_147_,1}}).
-file("core_parse.yrl", 264).
yeccpars2_147_([__2,__1 | __Stack]) ->
 [begin
   # c_tuple { es = [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_149_,1}}).
-file("core_parse.yrl", 216).
yeccpars2_149_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_150_,1}}).
-file("core_parse.yrl", 265).
yeccpars2_150_([__3,__2,__1 | __Stack]) ->
 [begin
   # c_tuple { es = __2 }
  end | __Stack].

-compile({inline,{yeccpars2_154_,1}}).
-file("core_parse.yrl", 295).
yeccpars2_154_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_157_,1}}).
-file("core_parse.yrl", 296).
yeccpars2_157_([__2,__1 | __Stack]) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_158_,1}}).
-file("core_parse.yrl", 297).
yeccpars2_158_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_164_,1}}).
-file("core_parse.yrl", 347).
yeccpars2_164_([__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   if length ( __8 ) =:= 2 ->
    # c_try { arg = __2 , vars = __4 , body = __6 , evars = __8 , handler = __10 } ;
    true ->
    return_error ( tok_line ( __7 ) ,
    "expected 2 exception variables in 'try'" )
    end
  end | __Stack].

-compile({inline,{yeccpars2_166_,1}}).
-file("core_parse.yrl", 357).
yeccpars2_166_([__2,__1 | __Stack]) ->
 [begin
   { T , A } = __2 ,
    # c_receive { clauses = [ ] , timeout = T , action = A }
  end | __Stack].

-compile({inline,{yeccpars2_175_,1}}).
-file("core_parse.yrl", 326).
yeccpars2_175_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_177_,1}}).
-file("core_parse.yrl", 317).
yeccpars2_177_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_185_,1}}).
-file("core_parse.yrl", 162).
yeccpars2_185_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_187_,1}}).
-file("core_parse.yrl", 173).
yeccpars2_187_([__2,__1 | __Stack]) ->
 [begin
   # c_tuple { es = [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_192_,1}}).
-file("core_parse.yrl", 169).
yeccpars2_192_([__3,__2,__1 | __Stack]) ->
 [begin
   # c_alias { var = __1 , pat = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_195_,1}}).
-file("core_parse.yrl", 157).
yeccpars2_195_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   core_lib : set_anno ( __2 , __4 )
  end | __Stack].

-compile({inline,{yeccpars2_198_,1}}).
-file("core_parse.yrl", 206).
yeccpars2_198_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   core_lib : set_anno ( __2 , __4 )
  end | __Stack].

-compile({inline,{yeccpars2_200_,1}}).
-file("core_parse.yrl", 161).
yeccpars2_200_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_201_,1}}).
-file("core_parse.yrl", 174).
yeccpars2_201_([__3,__2,__1 | __Stack]) ->
 [begin
   # c_tuple { es = __2 }
  end | __Stack].

-compile({inline,{yeccpars2_204_,1}}).
-file("core_parse.yrl", 364).
yeccpars2_204_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { __2 , __4 }
  end | __Stack].

-compile({inline,{yeccpars2_206_,1}}).
-file("core_parse.yrl", 177).
yeccpars2_206_([__3,__2,__1 | __Stack]) ->
 [begin
   # c_cons { hd = __2 , tl = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_208_,1}}).
-file("core_parse.yrl", 179).
yeccpars2_208_([__1 | __Stack]) ->
 [begin
   # c_literal { val = [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_211_,1}}).
-file("core_parse.yrl", 180).
yeccpars2_211_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_213_,1}}).
-file("core_parse.yrl", 182).
yeccpars2_213_([__3,__2,__1 | __Stack]) ->
 [begin
   # c_cons { hd = __2 , tl = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_215_,1}}).
-file("core_parse.yrl", 327).
yeccpars2_215_([__2,__1 | __Stack]) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_216_,1}}).
-file("core_parse.yrl", 328).
yeccpars2_216_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_221_,1}}).
-file("core_parse.yrl", 321).
yeccpars2_221_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   core_lib : set_anno ( __2 , __4 )
  end | __Stack].

-compile({inline,{yeccpars2_224_,1}}).
-file("core_parse.yrl", 188).
yeccpars2_224_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_227_,1}}).
-file("core_parse.yrl", 184).
yeccpars2_227_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   # c_binary { segments = [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_233_,1}}).
-file("core_parse.yrl", 191).
yeccpars2_233_([__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   case __6 of
    [ S , U , T , Fs ] ->
    # c_bitstr { val = __3 , size = S , unit = U , type = T , flags = Fs } ;
    true ->
    return_error ( tok_line ( __1 ) ,
    "expected 4 arguments in binary segment" )
    end
  end | __Stack].

-compile({inline,{yeccpars2_235_,1}}).
-file("core_parse.yrl", 187).
yeccpars2_235_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_237_,1}}).
-file("core_parse.yrl", 185).
yeccpars2_237_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # c_binary { segments = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_238_,1}}).
-file("core_parse.yrl", 316).
yeccpars2_238_([__2,__1 | __Stack]) ->
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_239_,1}}).
-file("core_parse.yrl", 360).
yeccpars2_239_([__3,__2,__1 | __Stack]) ->
 [begin
   { T , A } = __3 ,
    # c_receive { clauses = __2 , timeout = T , action = A }
  end | __Stack].

-compile({inline,{yeccpars2_243_,1}}).
-file("core_parse.yrl", 324).
yeccpars2_243_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # c_clause { pats = __1 , guard = __3 , body = __5 }
  end | __Stack].

-compile({inline,{yeccpars2_245_,1}}).
-file("core_parse.yrl", 338).
yeccpars2_245_([__3,__2,__1 | __Stack]) ->
 [begin
   Name = # c_literal { val = tok_val ( __2 ) } ,
    # c_primop { name = Name , args = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_248_,1}}).
-file("core_parse.yrl", 341).
yeccpars2_248_([__2,__1 | __Stack]) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_249_,1}}).
-file("core_parse.yrl", 342).
yeccpars2_249_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_252_,1}}).
-file("core_parse.yrl", 311).
yeccpars2_252_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   # c_letrec { defs = __2 , body = __4 }
  end | __Stack].

-compile({inline,{yeccpars2_257_,1}}).
-file("core_parse.yrl", 308).
yeccpars2_257_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # c_let { vars = __2 , arg = __4 , body = __6 }
  end | __Stack].

-compile({inline,{yeccpars2_259_,1}}).
-file("core_parse.yrl", 300).
yeccpars2_259_([__3,__2,__1 | __Stack]) ->
 [begin
   # c_seq { arg = __2 , body = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_260_,1}}).
-file("core_parse.yrl", 354).
yeccpars2_260_([__2,__1 | __Stack]) ->
 [begin
   # c_catch { body = __2 }
  end | __Stack].

-compile({inline,{yeccpars2_264_,1}}).
-file("core_parse.yrl", 314).
yeccpars2_264_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # c_case { arg = __2 , clauses = __4 }
  end | __Stack].

-compile({inline,{yeccpars2_268_,1}}).
-file("core_parse.yrl", 335).
yeccpars2_268_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # c_call { module = __2 , name = __4 , args = __5 }
  end | __Stack].

-compile({inline,{yeccpars2_270_,1}}).
-file("core_parse.yrl", 331).
yeccpars2_270_([__3,__2,__1 | __Stack]) ->
 [begin
   # c_apply { op = __2 , args = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_272_,1}}).
-file("core_parse.yrl", 267).
yeccpars2_272_([__3,__2,__1 | __Stack]) ->
 [begin
   # c_cons { hd = __2 , tl = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_274_,1}}).
-file("core_parse.yrl", 269).
yeccpars2_274_([__1 | __Stack]) ->
 [begin
   # c_literal { val = [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_277_,1}}).
-file("core_parse.yrl", 270).
yeccpars2_277_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_279_,1}}).
-file("core_parse.yrl", 271).
yeccpars2_279_([__3,__2,__1 | __Stack]) ->
 [begin
   # c_cons { hd = __2 , tl = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_281_,1}}).
-file("core_parse.yrl", 219).
yeccpars2_281_([__2,__1 | __Stack]) ->
 [begin
   # c_values { es = [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_282_,1}}).
-file("core_parse.yrl", 220).
yeccpars2_282_([__3,__2,__1 | __Stack]) ->
 [begin
   # c_values { es = __2 }
  end | __Stack].

-compile({inline,{yeccpars2_286_,1}}).
-file("core_parse.yrl", 214).
yeccpars2_286_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   core_lib : set_anno ( __2 , __4 )
  end | __Stack].

-compile({inline,{yeccpars2_289_,1}}).
-file("core_parse.yrl", 277).
yeccpars2_289_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_292_,1}}).
-file("core_parse.yrl", 273).
yeccpars2_292_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   # c_binary { segments = [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_298_,1}}).
-file("core_parse.yrl", 280).
yeccpars2_298_([__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   case __6 of
    [ S , U , T , Fs ] ->
    # c_bitstr { val = __3 , size = S , unit = U , type = T , flags = Fs } ;
    true ->
    return_error ( tok_line ( __1 ) ,
    "expected 4 arguments in binary segment" )
    end
  end | __Stack].

-compile({inline,{yeccpars2_300_,1}}).
-file("core_parse.yrl", 276).
yeccpars2_300_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_302_,1}}).
-file("core_parse.yrl", 274).
yeccpars2_302_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # c_binary { segments = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_305_,1}}).
-file("core_parse.yrl", 201).
yeccpars2_305_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_308_,1}}).
-file("core_parse.yrl", 305).
yeccpars2_308_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # c_fun { vars = __3 , body = __6 }
  end | __Stack].

-compile({inline,{yeccpars2_312_,1}}).
-file("core_parse.yrl", 118).
yeccpars2_312_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   core_lib : set_anno ( __2 , __4 )
  end | __Stack].

-compile({inline,{yeccpars2_313_,1}}).
-file("core_parse.yrl", 109).
yeccpars2_313_([__2,__1 | __Stack]) ->
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_314_,1}}).
-file("core_parse.yrl", 78).
yeccpars2_314_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # c_module { name = # c_literal { val = tok_val ( __2 ) } , exports = __3 ,
    attrs = __4 , defs = __5 }
  end | __Stack].

-compile({inline,{yeccpars2_318_,1}}).
-file("core_parse.yrl", 111).
yeccpars2_318_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_323_,1}}).
-file("core_parse.yrl", 83).
yeccpars2_323_([__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # c_module { anno = __9 , name = tok_val ( __3 ) , exports = __4 ,
    attrs = __5 , defs = __6 }
  end | __Stack].


-file("core_parse.yrl", 390).
