-module(icparse).
-file("/ldisk/daily_build/otp_prebuild_r11b.2007-06-11_19/otp_src_R11B-5/lib/ic/src/icyeccpre.hrl", 0).
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


-export([parse/1, parse_and_scan/1, format_error/1]).

-import(lists, [reverse/1]).

-ifdef(JAM).
-compile([{parse_transform,jam_yecc_pj},pj]).
-endif.


-include("icforms.hrl").



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

parse(Tokens) ->
    case catch yeccpars1(Tokens, false, 0, [], []) of
	error ->
	    Errorline =
		if Tokens == [] -> 0; true -> element(2, hd(Tokens)) end,
	    {error,
	     {Errorline, ?MODULE, "syntax error at or after this line."}};
	Other ->
	    Other
    end.

parse_and_scan({Mod, Fun, Args}) ->
    case apply(Mod, Fun, Args) of
	{eof, _} ->
	    {ok, eof};
	{error, Descriptor, _} ->
	    {error, Descriptor};
	{ok, Tokens, _} ->
	    yeccpars1(Tokens, {Mod, Fun, Args}, 0, [], [])
    end.

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
	true ->
	    Message;
	_ ->
	    io_lib:write(Message)
    end.

% To be used in grammar files to throw an error message to the parser toplevel.
% Doesn't have to be exported!
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).


% Don't change yeccpars1/6 too much, it is called recursively by yeccpars2/8!
yeccpars1([Token | Tokens], Tokenizer, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens,
	      Tokenizer);
yeccpars1([], {M, F, A}, State, States, Vstack) ->
    case catch apply(M, F, A) of
        {eof, Endline} ->
            {error, {Endline, ?MODULE, "end_of_file"}};
        {error, Descriptor, Endline} ->
            {error, Descriptor};
        {'EXIT', Reason} ->
            {error, {0, ?MODULE, Reason}};
        {ok, Tokens, Endline} ->
	    case catch yeccpars1(Tokens, {M, F, A}, State, States, Vstack) of
		error ->
		    Errorline = element(2, hd(Tokens)),
		    {error, {Errorline, ?MODULE,
			     "syntax error at or after this line."}};
		Other ->
		    Other
	    end
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
yecctoken2string({'dot', _}) -> io_lib:format('~w', ['.']);
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when atom(Other) ->
    io_lib:format('~w', [Other]);
yecctoken2string({_, _, Other}) when list(Other), number(hd(Other)) ->
    Other;
yecctoken2string({_, _, Other}) ->
    io_lib:format('~p', [Other]);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-file("icparse.yrl", 858).
%%-----------------------------------------------------------




-file("./icparse.erl", 132).

yeccpars2(0, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, const, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, enum, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, exception, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, interface, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, module, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, struct, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, typedef, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, union, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(1, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, const, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, enum, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, exception, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, interface, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, module, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, struct, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, typedef, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, union, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_1_(__Stack),
 yeccpars2(yeccgoto('<specification>', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(2, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<definition>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(3, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<definition>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(4, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<type_dcl>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(5, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 342, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(6, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<type_dcl>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(7, '$end', _, __Stack, _, _, _) ->
 {ok, hd(__Stack)};
yeccpars2(7, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(8, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 341, [8 | __Ss], [__T | __Stack]);
yeccpars2(8, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(9, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 270, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(10, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<interface>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(11, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 269, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(12, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<interface>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(13, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 268, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(14, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<type_dcl>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(15, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_15_(__Stack),
 yeccpars2(yeccgoto('OorM_<definition>', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(16, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 267, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(17, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 260, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, '<integer_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 261, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(18, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, boolean, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, double, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, fixed, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 256, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, long, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, octet, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 106, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, short, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 108, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, unsigned, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, wchar, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, wstring, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 109, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(19, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 235, [19 | __Ss], [__T | __Stack]);
yeccpars2(19, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(20, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 229, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(21, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 221, [21 | __Ss], [__T | __Stack]);
yeccpars2(21, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(22, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 216, [22 | __Ss], [__T | __Stack]);
yeccpars2(22, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(23, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 203, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(24, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, 'Object', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, any, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, boolean, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, double, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, enum, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, fixed, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, long, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, octet, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 106, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, sequence, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, short, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 108, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, struct, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, union, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, unsigned, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, wchar, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, wstring, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 109, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(25, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(26, switch, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [26 | __Ss], [__T | __Stack]);
yeccpars2(26, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(27, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [27 | __Ss], [__T | __Stack]);
yeccpars2(27, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(28, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, boolean, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, enum, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, long, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, short, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, unsigned, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, wchar, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(29, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<unsigned_int>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(30, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<unsigned_int>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(31, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_31_(__Stack),
 yeccpars2(yeccgoto('<integer_type>', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(32, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [32 | __Ss], [__T | __Stack]);
yeccpars2(32, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(33, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<signed_int>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(34, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<signed_int>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(35, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<integer_type>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(36, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<switch_type_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(37, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<switch_type_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(38, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<switch_type_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(39, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<switch_type_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(40, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<switch_type_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(41, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(42, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_42_(__Stack),
 yeccpars2(yeccgoto('<scoped_name>', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(43, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<boolean_type>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(44, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<char_type>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(45, long, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<signed_long_int>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(46, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<signed_short_int>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(47, long, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [47 | __Ss], [__T | __Stack]);
yeccpars2(47, short, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [47 | __Ss], [__T | __Stack]);
yeccpars2(47, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(48, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<char_type>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(49, long, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [49 | __Ss], [__T | __Stack]);
yeccpars2(49, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_49_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<unsigned_long_int>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(50, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_50_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<unsigned_short_int>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(51, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_51_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<unsigned_long_int>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(52, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_52_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<signed_long_int>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(53, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_53_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<scoped_name>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(54, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 55, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(55, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_55_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<scoped_name>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(56, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 57, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(57, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_57_(__Stack),
 yeccpars2(58, __Cat, [57 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(58, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [58 | __Ss], [__T | __Stack]);
yeccpars2(58, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_58_(__Stack),
 yeccpars2(64, __Cat, [58 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(59, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_59_#'(__Stack),
 yeccpars2(58, '#', [59 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(59, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_59_case(__Stack),
 yeccpars2(58, 'case', [59 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(59, default, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_59_default(__Stack),
 yeccpars2(58, default, [59 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(59, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_59_(__Stack),
 yeccpars2(yeccgoto('<switch_body>', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(60, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(61, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_61_(__Stack),
 yeccpars2(yeccgoto('OorM_<case>', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(62, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_62_(__Stack),
 __Nss = lists:nthtail(8, __Ss),
 yeccpars2(yeccgoto('<union_type>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(63, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_63_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('OorM_<case>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(64, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, default, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 102, [64 | __Ss], [__T | __Stack]);
yeccpars2(64, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(65, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_65_(__Stack),
 yeccpars2(78, __Cat, [65 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(66, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_66_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('Ugly_pragmas', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(67, '<integer_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 68, [67 | __Ss], [__T | __Stack]);
yeccpars2(67, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(68, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [68 | __Ss], [__T | __Stack]);
yeccpars2(68, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(69, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 70, [69 | __Ss], [__T | __Stack]);
yeccpars2(69, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(70, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, '<string_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 72, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(71, '<floating_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [71 | __Ss], [__T | __Stack]);
yeccpars2(71, '<string_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 75, [71 | __Ss], [__T | __Stack]);
yeccpars2(71, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(72, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 73, [72 | __Ss], [__T | __Stack]);
yeccpars2(72, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(73, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_73_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto('OE_pragma', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(74, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(75, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 76, [75 | __Ss], [__T | __Stack]);
yeccpars2(75, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(76, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_76_(__Stack),
 __Nss = lists:nthtail(6, __Ss),
 yeccpars2(yeccgoto('OE_pragma', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(77, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_77_(__Stack),
 __Nss = lists:nthtail(6, __Ss),
 yeccpars2(yeccgoto('OE_pragma', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(78, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'Object', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, any, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, boolean, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, 'case', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 101, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, default, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 102, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, double, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, enum, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, fixed, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, long, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, octet, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 106, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, sequence, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, short, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 108, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, struct, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, union, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, unsigned, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, wchar, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, wstring, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 109, [78 | __Ss], [__T | __Stack]);
yeccpars2(78, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(79, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<constr_type_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(80, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 187, [80 | __Ss], [__T | __Stack]);
yeccpars2(80, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(81, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<simple_type_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(82, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<constr_type_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(83, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<template_type_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(84, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<type_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(85, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<template_type_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(86, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [86 | __Ss], [__T | __Stack]);
yeccpars2(86, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<simple_type_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(87, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<base_type_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(88, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<base_type_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(89, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<base_type_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(90, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<template_type_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(91, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<constr_type_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(92, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_92_(__Stack),
 yeccpars2(180, __Cat, [92 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(93, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<type_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(94, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<base_type_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(95, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_95_(__Stack),
 yeccpars2(179, __Cat, [95 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(96, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<base_type_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(97, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<simple_type_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(98, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<base_type_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(99, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<base_type_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(100, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<any_type>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(101, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 125, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, '<character_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, '<fixed_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, '<floating_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, '<integer_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, '<string_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, '<wcharacter_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, '<wstring_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [101 | __Ss], [__T | __Stack]);
yeccpars2(101, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(102, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 176, [102 | __Ss], [__T | __Stack]);
yeccpars2(102, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(103, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<floating_pt_type>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(104, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 171, [104 | __Ss], [__T | __Stack]);
yeccpars2(104, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(105, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<floating_pt_type>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(106, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<octet_type>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(107, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 165, [107 | __Ss], [__T | __Stack]);
yeccpars2(107, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(108, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 162, [108 | __Ss], [__T | __Stack]);
yeccpars2(108, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_108_(__Stack),
 yeccpars2(yeccgoto('<string_type>', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(109, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 110, [109 | __Ss], [__T | __Stack]);
yeccpars2(109, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_109_(__Stack),
 yeccpars2(yeccgoto('<string_type>', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(110, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 125, [110 | __Ss], [__T | __Stack]);
yeccpars2(110, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [110 | __Ss], [__T | __Stack]);
yeccpars2(110, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [110 | __Ss], [__T | __Stack]);
yeccpars2(110, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [110 | __Ss], [__T | __Stack]);
yeccpars2(110, '<character_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [110 | __Ss], [__T | __Stack]);
yeccpars2(110, '<fixed_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [110 | __Ss], [__T | __Stack]);
yeccpars2(110, '<floating_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [110 | __Ss], [__T | __Stack]);
yeccpars2(110, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [110 | __Ss], [__T | __Stack]);
yeccpars2(110, '<integer_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [110 | __Ss], [__T | __Stack]);
yeccpars2(110, '<string_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [110 | __Ss], [__T | __Stack]);
yeccpars2(110, '<wcharacter_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [110 | __Ss], [__T | __Stack]);
yeccpars2(110, '<wstring_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [110 | __Ss], [__T | __Stack]);
yeccpars2(110, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [110 | __Ss], [__T | __Stack]);
yeccpars2(110, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [110 | __Ss], [__T | __Stack]);
yeccpars2(110, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [110 | __Ss], [__T | __Stack]);
yeccpars2(110, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(111, '^', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 158, [111 | __Ss], [__T | __Stack]);
yeccpars2(111, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<or_expr>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(112, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 125, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '<character_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '<fixed_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '<floating_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '<integer_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '<string_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '<wcharacter_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, '<wstring_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [112 | __Ss], [__T | __Stack]);
yeccpars2(112, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(113, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<mult_expr>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(114, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 152, [114 | __Ss], [__T | __Stack]);
yeccpars2(114, '>>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 153, [114 | __Ss], [__T | __Stack]);
yeccpars2(114, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<and_expr>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(115, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [115 | __Ss], [__T | __Stack]);
yeccpars2(115, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<primary_expr>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(116, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<unary_expr>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(117, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 160, [117 | __Ss], [__T | __Stack]);
yeccpars2(117, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(118, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 156, [118 | __Ss], [__T | __Stack]);
yeccpars2(118, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<const_exp>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(119, '%', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [119 | __Ss], [__T | __Stack]);
yeccpars2(119, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [119 | __Ss], [__T | __Stack]);
yeccpars2(119, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 145, [119 | __Ss], [__T | __Stack]);
yeccpars2(119, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<add_expr>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(120, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<primary_expr>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(121, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<positive_int_const>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(122, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<literal>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(123, '&', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 150, [123 | __Ss], [__T | __Stack]);
yeccpars2(123, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<xor_expr>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(124, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [124 | __Ss], [__T | __Stack]);
yeccpars2(124, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<shift_expr>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(125, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 125, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, '<character_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, '<fixed_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, '<floating_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, '<integer_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, '<string_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, '<wcharacter_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, '<wstring_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [125 | __Ss], [__T | __Stack]);
yeccpars2(125, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(126, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<unary_operator>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(127, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<unary_operator>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(128, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<literal>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(129, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<literal>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(130, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<literal>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(131, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<literal>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(132, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<literal>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(133, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<literal>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(134, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<literal>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(135, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<boolean_literal>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(136, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<boolean_literal>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(137, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<unary_operator>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(138, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 139, [138 | __Ss], [__T | __Stack]);
yeccpars2(138, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(139, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_139_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<primary_expr>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(140, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 125, [140 | __Ss], [__T | __Stack]);
yeccpars2(140, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [140 | __Ss], [__T | __Stack]);
yeccpars2(140, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [140 | __Ss], [__T | __Stack]);
yeccpars2(140, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [140 | __Ss], [__T | __Stack]);
yeccpars2(140, '<character_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [140 | __Ss], [__T | __Stack]);
yeccpars2(140, '<fixed_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [140 | __Ss], [__T | __Stack]);
yeccpars2(140, '<floating_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [140 | __Ss], [__T | __Stack]);
yeccpars2(140, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [140 | __Ss], [__T | __Stack]);
yeccpars2(140, '<integer_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [140 | __Ss], [__T | __Stack]);
yeccpars2(140, '<string_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [140 | __Ss], [__T | __Stack]);
yeccpars2(140, '<wcharacter_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [140 | __Ss], [__T | __Stack]);
yeccpars2(140, '<wstring_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [140 | __Ss], [__T | __Stack]);
yeccpars2(140, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [140 | __Ss], [__T | __Stack]);
yeccpars2(140, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [140 | __Ss], [__T | __Stack]);
yeccpars2(140, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [140 | __Ss], [__T | __Stack]);
yeccpars2(140, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(141, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 125, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '<character_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '<fixed_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '<floating_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '<integer_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '<string_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '<wcharacter_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '<wstring_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [141 | __Ss], [__T | __Stack]);
yeccpars2(141, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(142, '%', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [142 | __Ss], [__T | __Stack]);
yeccpars2(142, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [142 | __Ss], [__T | __Stack]);
yeccpars2(142, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 145, [142 | __Ss], [__T | __Stack]);
yeccpars2(142, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_142_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<add_expr>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(143, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 125, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, '<character_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, '<fixed_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, '<floating_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, '<integer_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, '<string_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, '<wcharacter_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, '<wstring_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [143 | __Ss], [__T | __Stack]);
yeccpars2(143, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(144, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 125, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, '<character_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, '<fixed_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, '<floating_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, '<integer_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, '<string_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, '<wcharacter_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, '<wstring_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [144 | __Ss], [__T | __Stack]);
yeccpars2(144, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(145, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 125, [145 | __Ss], [__T | __Stack]);
yeccpars2(145, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [145 | __Ss], [__T | __Stack]);
yeccpars2(145, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [145 | __Ss], [__T | __Stack]);
yeccpars2(145, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [145 | __Ss], [__T | __Stack]);
yeccpars2(145, '<character_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [145 | __Ss], [__T | __Stack]);
yeccpars2(145, '<fixed_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [145 | __Ss], [__T | __Stack]);
yeccpars2(145, '<floating_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [145 | __Ss], [__T | __Stack]);
yeccpars2(145, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [145 | __Ss], [__T | __Stack]);
yeccpars2(145, '<integer_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [145 | __Ss], [__T | __Stack]);
yeccpars2(145, '<string_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [145 | __Ss], [__T | __Stack]);
yeccpars2(145, '<wcharacter_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [145 | __Ss], [__T | __Stack]);
yeccpars2(145, '<wstring_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [145 | __Ss], [__T | __Stack]);
yeccpars2(145, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [145 | __Ss], [__T | __Stack]);
yeccpars2(145, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [145 | __Ss], [__T | __Stack]);
yeccpars2(145, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [145 | __Ss], [__T | __Stack]);
yeccpars2(145, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(146, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_146_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<mult_expr>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(147, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_147_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<mult_expr>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(148, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_148_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<mult_expr>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(149, '%', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 143, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 144, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 145, [149 | __Ss], [__T | __Stack]);
yeccpars2(149, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_149_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<add_expr>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(150, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 125, [150 | __Ss], [__T | __Stack]);
yeccpars2(150, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [150 | __Ss], [__T | __Stack]);
yeccpars2(150, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [150 | __Ss], [__T | __Stack]);
yeccpars2(150, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [150 | __Ss], [__T | __Stack]);
yeccpars2(150, '<character_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [150 | __Ss], [__T | __Stack]);
yeccpars2(150, '<fixed_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [150 | __Ss], [__T | __Stack]);
yeccpars2(150, '<floating_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [150 | __Ss], [__T | __Stack]);
yeccpars2(150, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [150 | __Ss], [__T | __Stack]);
yeccpars2(150, '<integer_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [150 | __Ss], [__T | __Stack]);
yeccpars2(150, '<string_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [150 | __Ss], [__T | __Stack]);
yeccpars2(150, '<wcharacter_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [150 | __Ss], [__T | __Stack]);
yeccpars2(150, '<wstring_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [150 | __Ss], [__T | __Stack]);
yeccpars2(150, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [150 | __Ss], [__T | __Stack]);
yeccpars2(150, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [150 | __Ss], [__T | __Stack]);
yeccpars2(150, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [150 | __Ss], [__T | __Stack]);
yeccpars2(150, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(151, '<<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 152, [151 | __Ss], [__T | __Stack]);
yeccpars2(151, '>>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 153, [151 | __Ss], [__T | __Stack]);
yeccpars2(151, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_151_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<and_expr>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(152, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 125, [152 | __Ss], [__T | __Stack]);
yeccpars2(152, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [152 | __Ss], [__T | __Stack]);
yeccpars2(152, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [152 | __Ss], [__T | __Stack]);
yeccpars2(152, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [152 | __Ss], [__T | __Stack]);
yeccpars2(152, '<character_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [152 | __Ss], [__T | __Stack]);
yeccpars2(152, '<fixed_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [152 | __Ss], [__T | __Stack]);
yeccpars2(152, '<floating_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [152 | __Ss], [__T | __Stack]);
yeccpars2(152, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [152 | __Ss], [__T | __Stack]);
yeccpars2(152, '<integer_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [152 | __Ss], [__T | __Stack]);
yeccpars2(152, '<string_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [152 | __Ss], [__T | __Stack]);
yeccpars2(152, '<wcharacter_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [152 | __Ss], [__T | __Stack]);
yeccpars2(152, '<wstring_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [152 | __Ss], [__T | __Stack]);
yeccpars2(152, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [152 | __Ss], [__T | __Stack]);
yeccpars2(152, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [152 | __Ss], [__T | __Stack]);
yeccpars2(152, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [152 | __Ss], [__T | __Stack]);
yeccpars2(152, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(153, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 125, [153 | __Ss], [__T | __Stack]);
yeccpars2(153, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [153 | __Ss], [__T | __Stack]);
yeccpars2(153, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [153 | __Ss], [__T | __Stack]);
yeccpars2(153, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [153 | __Ss], [__T | __Stack]);
yeccpars2(153, '<character_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [153 | __Ss], [__T | __Stack]);
yeccpars2(153, '<fixed_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [153 | __Ss], [__T | __Stack]);
yeccpars2(153, '<floating_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [153 | __Ss], [__T | __Stack]);
yeccpars2(153, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [153 | __Ss], [__T | __Stack]);
yeccpars2(153, '<integer_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [153 | __Ss], [__T | __Stack]);
yeccpars2(153, '<string_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [153 | __Ss], [__T | __Stack]);
yeccpars2(153, '<wcharacter_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [153 | __Ss], [__T | __Stack]);
yeccpars2(153, '<wstring_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [153 | __Ss], [__T | __Stack]);
yeccpars2(153, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [153 | __Ss], [__T | __Stack]);
yeccpars2(153, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [153 | __Ss], [__T | __Stack]);
yeccpars2(153, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [153 | __Ss], [__T | __Stack]);
yeccpars2(153, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(154, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [154 | __Ss], [__T | __Stack]);
yeccpars2(154, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [154 | __Ss], [__T | __Stack]);
yeccpars2(154, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_154_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<shift_expr>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(155, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 140, [155 | __Ss], [__T | __Stack]);
yeccpars2(155, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 141, [155 | __Ss], [__T | __Stack]);
yeccpars2(155, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_155_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<shift_expr>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(156, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 125, [156 | __Ss], [__T | __Stack]);
yeccpars2(156, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [156 | __Ss], [__T | __Stack]);
yeccpars2(156, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [156 | __Ss], [__T | __Stack]);
yeccpars2(156, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [156 | __Ss], [__T | __Stack]);
yeccpars2(156, '<character_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [156 | __Ss], [__T | __Stack]);
yeccpars2(156, '<fixed_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [156 | __Ss], [__T | __Stack]);
yeccpars2(156, '<floating_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [156 | __Ss], [__T | __Stack]);
yeccpars2(156, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [156 | __Ss], [__T | __Stack]);
yeccpars2(156, '<integer_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [156 | __Ss], [__T | __Stack]);
yeccpars2(156, '<string_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [156 | __Ss], [__T | __Stack]);
yeccpars2(156, '<wcharacter_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [156 | __Ss], [__T | __Stack]);
yeccpars2(156, '<wstring_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [156 | __Ss], [__T | __Stack]);
yeccpars2(156, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [156 | __Ss], [__T | __Stack]);
yeccpars2(156, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [156 | __Ss], [__T | __Stack]);
yeccpars2(156, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [156 | __Ss], [__T | __Stack]);
yeccpars2(156, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(157, '^', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 158, [157 | __Ss], [__T | __Stack]);
yeccpars2(157, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_157_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<or_expr>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(158, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 125, [158 | __Ss], [__T | __Stack]);
yeccpars2(158, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [158 | __Ss], [__T | __Stack]);
yeccpars2(158, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [158 | __Ss], [__T | __Stack]);
yeccpars2(158, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [158 | __Ss], [__T | __Stack]);
yeccpars2(158, '<character_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [158 | __Ss], [__T | __Stack]);
yeccpars2(158, '<fixed_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [158 | __Ss], [__T | __Stack]);
yeccpars2(158, '<floating_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [158 | __Ss], [__T | __Stack]);
yeccpars2(158, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [158 | __Ss], [__T | __Stack]);
yeccpars2(158, '<integer_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [158 | __Ss], [__T | __Stack]);
yeccpars2(158, '<string_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [158 | __Ss], [__T | __Stack]);
yeccpars2(158, '<wcharacter_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [158 | __Ss], [__T | __Stack]);
yeccpars2(158, '<wstring_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [158 | __Ss], [__T | __Stack]);
yeccpars2(158, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [158 | __Ss], [__T | __Stack]);
yeccpars2(158, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [158 | __Ss], [__T | __Stack]);
yeccpars2(158, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [158 | __Ss], [__T | __Stack]);
yeccpars2(158, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(159, '&', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 150, [159 | __Ss], [__T | __Stack]);
yeccpars2(159, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_159_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<xor_expr>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(160, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_160_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('<string_type>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(161, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_161_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<unary_expr>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(162, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 125, [162 | __Ss], [__T | __Stack]);
yeccpars2(162, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [162 | __Ss], [__T | __Stack]);
yeccpars2(162, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [162 | __Ss], [__T | __Stack]);
yeccpars2(162, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [162 | __Ss], [__T | __Stack]);
yeccpars2(162, '<character_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [162 | __Ss], [__T | __Stack]);
yeccpars2(162, '<fixed_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [162 | __Ss], [__T | __Stack]);
yeccpars2(162, '<floating_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [162 | __Ss], [__T | __Stack]);
yeccpars2(162, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [162 | __Ss], [__T | __Stack]);
yeccpars2(162, '<integer_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [162 | __Ss], [__T | __Stack]);
yeccpars2(162, '<string_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [162 | __Ss], [__T | __Stack]);
yeccpars2(162, '<wcharacter_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [162 | __Ss], [__T | __Stack]);
yeccpars2(162, '<wstring_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [162 | __Ss], [__T | __Stack]);
yeccpars2(162, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [162 | __Ss], [__T | __Stack]);
yeccpars2(162, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [162 | __Ss], [__T | __Stack]);
yeccpars2(162, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [162 | __Ss], [__T | __Stack]);
yeccpars2(162, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(163, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 164, [163 | __Ss], [__T | __Stack]);
yeccpars2(163, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(164, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_164_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('<string_type>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(165, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [165 | __Ss], [__T | __Stack]);
yeccpars2(165, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [165 | __Ss], [__T | __Stack]);
yeccpars2(165, 'Object', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [165 | __Ss], [__T | __Stack]);
yeccpars2(165, any, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [165 | __Ss], [__T | __Stack]);
yeccpars2(165, boolean, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [165 | __Ss], [__T | __Stack]);
yeccpars2(165, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [165 | __Ss], [__T | __Stack]);
yeccpars2(165, double, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [165 | __Ss], [__T | __Stack]);
yeccpars2(165, fixed, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [165 | __Ss], [__T | __Stack]);
yeccpars2(165, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [165 | __Ss], [__T | __Stack]);
yeccpars2(165, long, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [165 | __Ss], [__T | __Stack]);
yeccpars2(165, octet, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 106, [165 | __Ss], [__T | __Stack]);
yeccpars2(165, sequence, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [165 | __Ss], [__T | __Stack]);
yeccpars2(165, short, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [165 | __Ss], [__T | __Stack]);
yeccpars2(165, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 108, [165 | __Ss], [__T | __Stack]);
yeccpars2(165, unsigned, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [165 | __Ss], [__T | __Stack]);
yeccpars2(165, wchar, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [165 | __Ss], [__T | __Stack]);
yeccpars2(165, wstring, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 109, [165 | __Ss], [__T | __Stack]);
yeccpars2(165, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(166, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 167, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 168, [166 | __Ss], [__T | __Stack]);
yeccpars2(166, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(167, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 125, [167 | __Ss], [__T | __Stack]);
yeccpars2(167, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [167 | __Ss], [__T | __Stack]);
yeccpars2(167, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [167 | __Ss], [__T | __Stack]);
yeccpars2(167, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [167 | __Ss], [__T | __Stack]);
yeccpars2(167, '<character_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [167 | __Ss], [__T | __Stack]);
yeccpars2(167, '<fixed_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [167 | __Ss], [__T | __Stack]);
yeccpars2(167, '<floating_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [167 | __Ss], [__T | __Stack]);
yeccpars2(167, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [167 | __Ss], [__T | __Stack]);
yeccpars2(167, '<integer_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [167 | __Ss], [__T | __Stack]);
yeccpars2(167, '<string_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [167 | __Ss], [__T | __Stack]);
yeccpars2(167, '<wcharacter_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [167 | __Ss], [__T | __Stack]);
yeccpars2(167, '<wstring_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [167 | __Ss], [__T | __Stack]);
yeccpars2(167, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [167 | __Ss], [__T | __Stack]);
yeccpars2(167, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [167 | __Ss], [__T | __Stack]);
yeccpars2(167, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [167 | __Ss], [__T | __Stack]);
yeccpars2(167, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(168, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_168_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('<sequence_type>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(169, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 170, [169 | __Ss], [__T | __Stack]);
yeccpars2(169, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(170, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_170_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto('<sequence_type>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(171, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 125, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, '<character_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, '<fixed_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, '<floating_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, '<integer_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, '<string_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, '<wcharacter_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, '<wstring_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [171 | __Ss], [__T | __Stack]);
yeccpars2(171, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(172, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 173, [172 | __Ss], [__T | __Stack]);
yeccpars2(172, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(173, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 125, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, '<character_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, '<fixed_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, '<floating_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, '<integer_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, '<string_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, '<wcharacter_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, '<wstring_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [173 | __Ss], [__T | __Stack]);
yeccpars2(173, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(174, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 175, [174 | __Ss], [__T | __Stack]);
yeccpars2(174, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(175, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_175_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto('<fixed_pt_type>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(176, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_176_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<case_label>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(177, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 178, [177 | __Ss], [__T | __Stack]);
yeccpars2(177, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(178, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_178_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<case_label>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(179, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [179 | __Ss], [__T | __Stack]);
yeccpars2(179, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_179_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('OorM_<case_label>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(180, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [180 | __Ss], [__T | __Stack]);
yeccpars2(180, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 181, [180 | __Ss], [__T | __Stack]);
yeccpars2(180, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(181, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_181_(__Stack),
 yeccpars2(182, __Cat, [181 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(182, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [182 | __Ss], [__T | __Stack]);
yeccpars2(182, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_182_(__Stack),
 __Nss = lists:nthtail(6, __Ss),
 yeccpars2(yeccgoto('<case>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(183, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<declarator>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(184, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_184_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<element_spec>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(185, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<declarator>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(186, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<complex_declarator>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(187, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 190, [187 | __Ss], [__T | __Stack]);
yeccpars2(187, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<simple_declarator>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(188, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 190, [188 | __Ss], [__T | __Stack]);
yeccpars2(188, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_188_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<array_declarator>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(189, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_189_(__Stack),
 yeccpars2(yeccgoto('OorM_<fixed_array_size>', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(190, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 125, [190 | __Ss], [__T | __Stack]);
yeccpars2(190, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [190 | __Ss], [__T | __Stack]);
yeccpars2(190, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [190 | __Ss], [__T | __Stack]);
yeccpars2(190, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [190 | __Ss], [__T | __Stack]);
yeccpars2(190, '<character_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [190 | __Ss], [__T | __Stack]);
yeccpars2(190, '<fixed_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [190 | __Ss], [__T | __Stack]);
yeccpars2(190, '<floating_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [190 | __Ss], [__T | __Stack]);
yeccpars2(190, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [190 | __Ss], [__T | __Stack]);
yeccpars2(190, '<integer_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [190 | __Ss], [__T | __Stack]);
yeccpars2(190, '<string_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [190 | __Ss], [__T | __Stack]);
yeccpars2(190, '<wcharacter_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [190 | __Ss], [__T | __Stack]);
yeccpars2(190, '<wstring_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [190 | __Ss], [__T | __Stack]);
yeccpars2(190, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [190 | __Ss], [__T | __Stack]);
yeccpars2(190, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [190 | __Ss], [__T | __Stack]);
yeccpars2(190, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [190 | __Ss], [__T | __Stack]);
yeccpars2(190, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(191, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 192, [191 | __Ss], [__T | __Stack]);
yeccpars2(191, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(192, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_192_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<fixed_array_size>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(193, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_193_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('OorM_<fixed_array_size>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(194, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_194_(__Stack),
 yeccpars2(195, __Cat, [194 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(195, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [195 | __Ss], [__T | __Stack]);
yeccpars2(195, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_195_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('OorM_<case_label>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(196, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 187, [196 | __Ss], [__T | __Stack]);
yeccpars2(196, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(197, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_197_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<type_dcl>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(198, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_198_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<type_declarator>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(199, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_199_(__Stack),
 yeccpars2(200, __Cat, [199 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(200, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 201, [200 | __Ss], [__T | __Stack]);
yeccpars2(200, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_200_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<declarators>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(201, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 187, [201 | __Ss], [__T | __Stack]);
yeccpars2(201, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(202, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_202_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('ZorM_<declarator>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(203, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 204, [203 | __Ss], [__T | __Stack]);
yeccpars2(203, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(204, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_204_(__Stack),
 yeccpars2(205, __Cat, [204 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(205, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [205 | __Ss], [__T | __Stack]);
yeccpars2(205, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [205 | __Ss], [__T | __Stack]);
yeccpars2(205, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [205 | __Ss], [__T | __Stack]);
yeccpars2(205, 'Object', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [205 | __Ss], [__T | __Stack]);
yeccpars2(205, any, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [205 | __Ss], [__T | __Stack]);
yeccpars2(205, boolean, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [205 | __Ss], [__T | __Stack]);
yeccpars2(205, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [205 | __Ss], [__T | __Stack]);
yeccpars2(205, double, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [205 | __Ss], [__T | __Stack]);
yeccpars2(205, enum, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [205 | __Ss], [__T | __Stack]);
yeccpars2(205, fixed, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 104, [205 | __Ss], [__T | __Stack]);
yeccpars2(205, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [205 | __Ss], [__T | __Stack]);
yeccpars2(205, long, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [205 | __Ss], [__T | __Stack]);
yeccpars2(205, octet, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 106, [205 | __Ss], [__T | __Stack]);
yeccpars2(205, sequence, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 107, [205 | __Ss], [__T | __Stack]);
yeccpars2(205, short, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [205 | __Ss], [__T | __Stack]);
yeccpars2(205, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 108, [205 | __Ss], [__T | __Stack]);
yeccpars2(205, struct, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [205 | __Ss], [__T | __Stack]);
yeccpars2(205, union, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [205 | __Ss], [__T | __Stack]);
yeccpars2(205, unsigned, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [205 | __Ss], [__T | __Stack]);
yeccpars2(205, wchar, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [205 | __Ss], [__T | __Stack]);
yeccpars2(205, wstring, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 109, [205 | __Ss], [__T | __Stack]);
yeccpars2(205, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(206, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_206_#'(__Stack),
 yeccpars2(205, '#', [206 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(206, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_206_::'(__Stack),
 yeccpars2(205, '::', [206 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(206, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_206_<identifier>'(__Stack),
 yeccpars2(205, '<identifier>', [206 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(206, 'Object', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_206_Object(__Stack),
 yeccpars2(205, 'Object', [206 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(206, any, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_206_any(__Stack),
 yeccpars2(205, any, [206 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(206, boolean, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_206_boolean(__Stack),
 yeccpars2(205, boolean, [206 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(206, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_206_char(__Stack),
 yeccpars2(205, char, [206 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(206, double, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_206_double(__Stack),
 yeccpars2(205, double, [206 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(206, enum, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_206_enum(__Stack),
 yeccpars2(205, enum, [206 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(206, fixed, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_206_fixed(__Stack),
 yeccpars2(205, fixed, [206 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(206, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_206_float(__Stack),
 yeccpars2(205, float, [206 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(206, long, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_206_long(__Stack),
 yeccpars2(205, long, [206 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(206, octet, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_206_octet(__Stack),
 yeccpars2(205, octet, [206 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(206, sequence, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_206_sequence(__Stack),
 yeccpars2(205, sequence, [206 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(206, short, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_206_short(__Stack),
 yeccpars2(205, short, [206 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(206, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_206_string(__Stack),
 yeccpars2(205, string, [206 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(206, struct, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_206_struct(__Stack),
 yeccpars2(205, struct, [206 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(206, union, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_206_union(__Stack),
 yeccpars2(205, union, [206 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(206, unsigned, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_206_unsigned(__Stack),
 yeccpars2(205, unsigned, [206 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(206, wchar, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_206_wchar(__Stack),
 yeccpars2(205, wchar, [206 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(206, wstring, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_206_wstring(__Stack),
 yeccpars2(205, wstring, [206 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(206, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_206_(__Stack),
 yeccpars2(yeccgoto('<member_list>', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(207, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 209, [207 | __Ss], [__T | __Stack]);
yeccpars2(207, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(208, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('OorM_<member>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(209, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_209_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto('<struct_type>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(210, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_210_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('OorM_<member>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(211, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 187, [211 | __Ss], [__T | __Stack]);
yeccpars2(211, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(212, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_212_(__Stack),
 yeccpars2(213, __Cat, [212 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(213, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [213 | __Ss], [__T | __Stack]);
yeccpars2(213, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 214, [213 | __Ss], [__T | __Stack]);
yeccpars2(213, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(214, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_214_(__Stack),
 yeccpars2(215, __Cat, [214 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(215, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [215 | __Ss], [__T | __Stack]);
yeccpars2(215, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_215_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto('<member>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(216, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 217, [216 | __Ss], [__T | __Stack]);
yeccpars2(216, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(217, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [217 | __Ss], [__T | __Stack]);
yeccpars2(217, const, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [217 | __Ss], [__T | __Stack]);
yeccpars2(217, enum, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [217 | __Ss], [__T | __Stack]);
yeccpars2(217, exception, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [217 | __Ss], [__T | __Stack]);
yeccpars2(217, interface, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [217 | __Ss], [__T | __Stack]);
yeccpars2(217, module, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [217 | __Ss], [__T | __Stack]);
yeccpars2(217, struct, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [217 | __Ss], [__T | __Stack]);
yeccpars2(217, typedef, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [217 | __Ss], [__T | __Stack]);
yeccpars2(217, union, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [217 | __Ss], [__T | __Stack]);
yeccpars2(217, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(218, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, const, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, enum, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, exception, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, interface, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, module, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, struct, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, typedef, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, union, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 220, [218 | __Ss], [__T | __Stack]);
yeccpars2(218, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(219, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_219_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('OorM_<definition>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(220, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_220_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto('<module>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(221, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 224, [221 | __Ss], [__T | __Stack]);
yeccpars2(221, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_221_;'(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<forward_dcl>', hd(__Nss)), ';', __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(221, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_221_(__Stack),
 yeccpars2(222, __Cat, [221 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(222, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_222_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<interface_header>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(223, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('Opt_<inheritance_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(224, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [224 | __Ss], [__T | __Stack]);
yeccpars2(224, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [224 | __Ss], [__T | __Stack]);
yeccpars2(224, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(225, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [225 | __Ss], [__T | __Stack]);
yeccpars2(225, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_225_(__Stack),
 yeccpars2(226, __Cat, [225 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(226, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 227, [226 | __Ss], [__T | __Stack]);
yeccpars2(226, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_226_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<inheritance_spec>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(227, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [227 | __Ss], [__T | __Stack]);
yeccpars2(227, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [227 | __Ss], [__T | __Stack]);
yeccpars2(227, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(228, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [228 | __Ss], [__T | __Stack]);
yeccpars2(228, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_228_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('ZorM_<scoped_name>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(229, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 230, [229 | __Ss], [__T | __Stack]);
yeccpars2(229, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(230, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_230_(__Stack),
 yeccpars2(232, __Cat, [230 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(231, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 234, [231 | __Ss], [__T | __Stack]);
yeccpars2(231, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_231_(__Stack),
 yeccpars2(205, __Cat, [231 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(232, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [232 | __Ss], [__T | __Stack]);
yeccpars2(232, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('ZorM_<member>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(233, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_233_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('ZorM_<member>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(234, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_234_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto('<except_dcl>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(235, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 236, [235 | __Ss], [__T | __Stack]);
yeccpars2(235, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(236, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_236_(__Stack),
 yeccpars2(237, __Cat, [236 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(237, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [237 | __Ss], [__T | __Stack]);
yeccpars2(237, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 239, [237 | __Ss], [__T | __Stack]);
yeccpars2(237, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(238, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_238_(__Stack),
 yeccpars2(240, __Cat, [238 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(239, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_239_(__Stack),
 yeccpars2(yeccgoto('<enumerator>', hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(240, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [240 | __Ss], [__T | __Stack]);
yeccpars2(240, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_240_(__Stack),
 yeccpars2(241, __Cat, [240 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(241, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_241_(__Stack),
 yeccpars2(242, __Cat, [241 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(242, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [242 | __Ss], [__T | __Stack]);
yeccpars2(242, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 243, [242 | __Ss], [__T | __Stack]);
yeccpars2(242, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 244, [242 | __Ss], [__T | __Stack]);
yeccpars2(242, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(243, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_243_(__Stack),
 yeccpars2(245, __Cat, [243 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(244, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_244_(__Stack),
 __Nss = lists:nthtail(8, __Ss),
 yeccpars2(yeccgoto('<enum_type>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(245, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [245 | __Ss], [__T | __Stack]);
yeccpars2(245, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 239, [245 | __Ss], [__T | __Stack]);
yeccpars2(245, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(246, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_246_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto('ZorM_<enumerator>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(247, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<const_type>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(248, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [248 | __Ss], [__T | __Stack]);
yeccpars2(248, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<const_type>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(249, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<const_type>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(250, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<const_type>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(251, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<const_type>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(252, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<const_type>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(253, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 257, [253 | __Ss], [__T | __Stack]);
yeccpars2(253, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(254, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<const_type>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(255, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<const_type>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(256, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<fixed_pt_const_type>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(257, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 258, [257 | __Ss], [__T | __Stack]);
yeccpars2(257, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(258, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 125, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 126, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 127, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, '<character_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 128, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, '<fixed_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 129, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, '<floating_pt_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 130, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, '<integer_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 131, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, '<string_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 132, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, '<wcharacter_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 133, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, '<wstring_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 134, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, 'FALSE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 135, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, 'TRUE', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 136, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, '~', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 137, [258 | __Ss], [__T | __Stack]);
yeccpars2(258, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(259, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_259_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto('<const_dcl>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(260, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_260_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('OE_preproc', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(261, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 69, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, '<string_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 262, [261 | __Ss], [__T | __Stack]);
yeccpars2(261, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(262, '<integer_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 264, [262 | __Ss], [__T | __Stack]);
yeccpars2(262, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_262_(__Stack),
 yeccpars2(263, __Cat, [262 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(263, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 266, [263 | __Ss], [__T | __Stack]);
yeccpars2(263, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(264, '<integer_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 264, [264 | __Ss], [__T | __Stack]);
yeccpars2(264, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_264_(__Stack),
 yeccpars2(265, __Cat, [264 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(265, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_265_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('ZorM_<integer_literal>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(266, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_266_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto('OE_preproc', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(267, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_267_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<definition>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(268, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_268_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<definition>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(269, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_269_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<definition>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(270, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_270_(__Stack),
 yeccpars2(271, __Cat, [270 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(271, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [271 | __Ss], [__T | __Stack]);
yeccpars2(271, const, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [271 | __Ss], [__T | __Stack]);
yeccpars2(271, enum, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [271 | __Ss], [__T | __Stack]);
yeccpars2(271, exception, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [271 | __Ss], [__T | __Stack]);
yeccpars2(271, oneway, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 285, [271 | __Ss], [__T | __Stack]);
yeccpars2(271, readonly, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 286, [271 | __Ss], [__T | __Stack]);
yeccpars2(271, struct, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [271 | __Ss], [__T | __Stack]);
yeccpars2(271, typedef, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [271 | __Ss], [__T | __Stack]);
yeccpars2(271, union, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [271 | __Ss], [__T | __Stack]);
yeccpars2(271, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_271_::'(__Stack),
 yeccpars2(275, '::', [271 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(271, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_271_<identifier>'(__Stack),
 yeccpars2(275, '<identifier>', [271 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(271, 'Object', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_271_Object(__Stack),
 yeccpars2(275, 'Object', [271 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(271, any, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_271_any(__Stack),
 yeccpars2(275, any, [271 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(271, boolean, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_271_boolean(__Stack),
 yeccpars2(275, boolean, [271 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(271, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_271_char(__Stack),
 yeccpars2(275, char, [271 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(271, double, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_271_double(__Stack),
 yeccpars2(275, double, [271 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(271, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_271_float(__Stack),
 yeccpars2(275, float, [271 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(271, long, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_271_long(__Stack),
 yeccpars2(275, long, [271 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(271, octet, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_271_octet(__Stack),
 yeccpars2(275, octet, [271 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(271, short, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_271_short(__Stack),
 yeccpars2(275, short, [271 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(271, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_271_string(__Stack),
 yeccpars2(275, string, [271 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(271, unsigned, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_271_unsigned(__Stack),
 yeccpars2(275, unsigned, [271 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(271, void, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_271_void(__Stack),
 yeccpars2(275, void, [271 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(271, wchar, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_271_wchar(__Stack),
 yeccpars2(275, wchar, [271 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(271, wstring, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_271_wstring(__Stack),
 yeccpars2(275, wstring, [271 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(271, attribute, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_271_attribute(__Stack),
 yeccpars2(274, attribute, [271 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(271, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<interface_body>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(272, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 273, [272 | __Ss], [__T | __Stack]);
yeccpars2(272, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(273, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_273_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto('<interface_dcl>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(274, attribute, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 335, [274 | __Ss], [__T | __Stack]);
yeccpars2(274, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(275, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, 'Object', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, any, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, boolean, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, double, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, long, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, octet, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 106, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, short, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 108, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, unsigned, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, void, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 297, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, wchar, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, wstring, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 109, [275 | __Ss], [__T | __Stack]);
yeccpars2(275, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(276, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<export>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(277, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<export>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(278, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 291, [278 | __Ss], [__T | __Stack]);
yeccpars2(278, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(279, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 290, [279 | __Ss], [__T | __Stack]);
yeccpars2(279, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(280, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('Opt_<op_attribute>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(281, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_281_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('ZorM_<export>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(282, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 289, [282 | __Ss], [__T | __Stack]);
yeccpars2(282, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(283, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 288, [283 | __Ss], [__T | __Stack]);
yeccpars2(283, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(284, ';', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 287, [284 | __Ss], [__T | __Stack]);
yeccpars2(284, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(285, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<op_attribute>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(286, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('Opt_readonly', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(287, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_287_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<export>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(288, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_288_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<export>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(289, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_289_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<export>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(290, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_290_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<export>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(291, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_291_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<export>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(292, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<param_type_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(293, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [293 | __Ss], [__T | __Stack]);
yeccpars2(293, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<param_type_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(294, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<op_type_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(295, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 298, [295 | __Ss], [__T | __Stack]);
yeccpars2(295, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(296, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<param_type_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(297, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<op_type_spec>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(298, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 300, [298 | __Ss], [__T | __Stack]);
yeccpars2(298, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(299, raises, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 321, [299 | __Ss], [__T | __Stack]);
yeccpars2(299, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_299_(__Stack),
 yeccpars2(319, __Cat, [299 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(300, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_300_(__Stack),
 yeccpars2(301, __Cat, [300 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(301, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [301 | __Ss], [__T | __Stack]);
yeccpars2(301, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 304, [301 | __Ss], [__T | __Stack]);
yeccpars2(301, in, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 305, [301 | __Ss], [__T | __Stack]);
yeccpars2(301, inout, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 306, [301 | __Ss], [__T | __Stack]);
yeccpars2(301, out, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 307, [301 | __Ss], [__T | __Stack]);
yeccpars2(301, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(302, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_302_(__Stack),
 yeccpars2(312, __Cat, [302 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(303, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [303 | __Ss], [__T | __Stack]);
yeccpars2(303, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [303 | __Ss], [__T | __Stack]);
yeccpars2(303, 'Object', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [303 | __Ss], [__T | __Stack]);
yeccpars2(303, any, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [303 | __Ss], [__T | __Stack]);
yeccpars2(303, boolean, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [303 | __Ss], [__T | __Stack]);
yeccpars2(303, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [303 | __Ss], [__T | __Stack]);
yeccpars2(303, double, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [303 | __Ss], [__T | __Stack]);
yeccpars2(303, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [303 | __Ss], [__T | __Stack]);
yeccpars2(303, long, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [303 | __Ss], [__T | __Stack]);
yeccpars2(303, octet, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 106, [303 | __Ss], [__T | __Stack]);
yeccpars2(303, short, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [303 | __Ss], [__T | __Stack]);
yeccpars2(303, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 108, [303 | __Ss], [__T | __Stack]);
yeccpars2(303, unsigned, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [303 | __Ss], [__T | __Stack]);
yeccpars2(303, wchar, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [303 | __Ss], [__T | __Stack]);
yeccpars2(303, wstring, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 109, [303 | __Ss], [__T | __Stack]);
yeccpars2(303, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(304, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_304_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<parameter_dcls>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(305, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<param_attribute>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(306, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<param_attribute>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(307, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<param_attribute>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(308, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 310, [308 | __Ss], [__T | __Stack]);
yeccpars2(308, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(309, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_309_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('<param_dcl>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(310, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('<simple_declarator>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(311, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 314, [311 | __Ss], [__T | __Stack]);
yeccpars2(311, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_311_(__Stack),
 yeccpars2(313, __Cat, [311 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(312, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [312 | __Ss], [__T | __Stack]);
yeccpars2(312, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('ZorM_<param_dcl>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(313, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [313 | __Ss], [__T | __Stack]);
yeccpars2(313, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 315, [313 | __Ss], [__T | __Stack]);
yeccpars2(313, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(314, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_314_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto('<parameter_dcls>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(315, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_315_(__Stack),
 yeccpars2(316, __Cat, [315 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(316, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [316 | __Ss], [__T | __Stack]);
yeccpars2(316, in, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 305, [316 | __Ss], [__T | __Stack]);
yeccpars2(316, inout, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 306, [316 | __Ss], [__T | __Stack]);
yeccpars2(316, out, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 307, [316 | __Ss], [__T | __Stack]);
yeccpars2(316, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(317, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_317_(__Stack),
 yeccpars2(318, __Cat, [317 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(318, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [318 | __Ss], [__T | __Stack]);
yeccpars2(318, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_318_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto('ZorM_<param_dcl>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(319, context, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 328, [319 | __Ss], [__T | __Stack]);
yeccpars2(319, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_319_(__Stack),
 yeccpars2(326, __Cat, [319 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(320, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('Opt_<raises_expr>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(321, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 322, [321 | __Ss], [__T | __Stack]);
yeccpars2(321, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(322, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [322 | __Ss], [__T | __Stack]);
yeccpars2(322, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [322 | __Ss], [__T | __Stack]);
yeccpars2(322, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(323, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [323 | __Ss], [__T | __Stack]);
yeccpars2(323, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_323_(__Stack),
 yeccpars2(324, __Cat, [323 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(324, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 325, [324 | __Ss], [__T | __Stack]);
yeccpars2(324, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 227, [324 | __Ss], [__T | __Stack]);
yeccpars2(324, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(325, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_325_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto('<raises_expr>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(326, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_326_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto('<op_dcl>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(327, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto('Opt_<context_expr>', hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(328, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 329, [328 | __Ss], [__T | __Stack]);
yeccpars2(328, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(329, '<string_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 330, [329 | __Ss], [__T | __Stack]);
yeccpars2(329, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(330, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_330_(__Stack),
 yeccpars2(331, __Cat, [330 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(331, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 332, [331 | __Ss], [__T | __Stack]);
yeccpars2(331, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 333, [331 | __Ss], [__T | __Stack]);
yeccpars2(331, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(332, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_332_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto('<context_expr>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(333, '<string_literal>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 334, [333 | __Ss], [__T | __Stack]);
yeccpars2(333, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(334, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_334_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('ZorM_<string_literal>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(335, '::', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [335 | __Ss], [__T | __Stack]);
yeccpars2(335, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 42, [335 | __Ss], [__T | __Stack]);
yeccpars2(335, 'Object', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 99, [335 | __Ss], [__T | __Stack]);
yeccpars2(335, any, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 100, [335 | __Ss], [__T | __Stack]);
yeccpars2(335, boolean, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [335 | __Ss], [__T | __Stack]);
yeccpars2(335, char, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [335 | __Ss], [__T | __Stack]);
yeccpars2(335, double, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 103, [335 | __Ss], [__T | __Stack]);
yeccpars2(335, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 105, [335 | __Ss], [__T | __Stack]);
yeccpars2(335, long, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [335 | __Ss], [__T | __Stack]);
yeccpars2(335, octet, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 106, [335 | __Ss], [__T | __Stack]);
yeccpars2(335, short, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [335 | __Ss], [__T | __Stack]);
yeccpars2(335, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 108, [335 | __Ss], [__T | __Stack]);
yeccpars2(335, unsigned, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [335 | __Ss], [__T | __Stack]);
yeccpars2(335, wchar, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [335 | __Ss], [__T | __Stack]);
yeccpars2(335, wstring, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 109, [335 | __Ss], [__T | __Stack]);
yeccpars2(335, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(336, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 310, [336 | __Ss], [__T | __Stack]);
yeccpars2(336, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(337, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_337_(__Stack),
 yeccpars2(338, __Cat, [337 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(338, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 339, [338 | __Ss], [__T | __Stack]);
yeccpars2(338, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_338_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto('<attr_dcl>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(339, '<identifier>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 310, [339 | __Ss], [__T | __Stack]);
yeccpars2(339, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(340, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_340_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto('ZorM_<simple_declarator>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(341, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_341_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<definition>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(342, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_342_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto('<definition>', hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(__Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.1",{missing_state_in_action_table, __Other}}).

yeccgoto('<add_expr>', 101) ->
 124;
yeccgoto('<add_expr>', 110) ->
 124;
yeccgoto('<add_expr>', 125) ->
 124;
yeccgoto('<add_expr>', 150) ->
 124;
yeccgoto('<add_expr>', 152) ->
 155;
yeccgoto('<add_expr>', 153) ->
 154;
yeccgoto('<add_expr>', 156) ->
 124;
yeccgoto('<add_expr>', 158) ->
 124;
yeccgoto('<add_expr>', 162) ->
 124;
yeccgoto('<add_expr>', 167) ->
 124;
yeccgoto('<add_expr>', 171) ->
 124;
yeccgoto('<add_expr>', 173) ->
 124;
yeccgoto('<add_expr>', 190) ->
 124;
yeccgoto('<add_expr>', 258) ->
 124;
yeccgoto('<and_expr>', 101) ->
 123;
yeccgoto('<and_expr>', 110) ->
 123;
yeccgoto('<and_expr>', 125) ->
 123;
yeccgoto('<and_expr>', 156) ->
 123;
yeccgoto('<and_expr>', 158) ->
 159;
yeccgoto('<and_expr>', 162) ->
 123;
yeccgoto('<and_expr>', 167) ->
 123;
yeccgoto('<and_expr>', 171) ->
 123;
yeccgoto('<and_expr>', 173) ->
 123;
yeccgoto('<and_expr>', 190) ->
 123;
yeccgoto('<and_expr>', 258) ->
 123;
yeccgoto('<any_type>', 24) ->
 98;
yeccgoto('<any_type>', 78) ->
 98;
yeccgoto('<any_type>', 165) ->
 98;
yeccgoto('<any_type>', 205) ->
 98;
yeccgoto('<any_type>', 275) ->
 98;
yeccgoto('<any_type>', 303) ->
 98;
yeccgoto('<any_type>', 335) ->
 98;
yeccgoto('<array_declarator>', 80) ->
 186;
yeccgoto('<array_declarator>', 196) ->
 186;
yeccgoto('<array_declarator>', 201) ->
 186;
yeccgoto('<array_declarator>', 211) ->
 186;
yeccgoto('<attr_dcl>', 271) ->
 284;
yeccgoto('<base_type_spec>', 24) ->
 97;
yeccgoto('<base_type_spec>', 78) ->
 97;
yeccgoto('<base_type_spec>', 165) ->
 97;
yeccgoto('<base_type_spec>', 205) ->
 97;
yeccgoto('<base_type_spec>', 275) ->
 296;
yeccgoto('<base_type_spec>', 303) ->
 296;
yeccgoto('<base_type_spec>', 335) ->
 296;
yeccgoto('<boolean_literal>', 101) ->
 122;
yeccgoto('<boolean_literal>', 110) ->
 122;
yeccgoto('<boolean_literal>', 112) ->
 122;
yeccgoto('<boolean_literal>', 125) ->
 122;
yeccgoto('<boolean_literal>', 140) ->
 122;
yeccgoto('<boolean_literal>', 141) ->
 122;
yeccgoto('<boolean_literal>', 143) ->
 122;
yeccgoto('<boolean_literal>', 144) ->
 122;
yeccgoto('<boolean_literal>', 145) ->
 122;
yeccgoto('<boolean_literal>', 150) ->
 122;
yeccgoto('<boolean_literal>', 152) ->
 122;
yeccgoto('<boolean_literal>', 153) ->
 122;
yeccgoto('<boolean_literal>', 156) ->
 122;
yeccgoto('<boolean_literal>', 158) ->
 122;
yeccgoto('<boolean_literal>', 162) ->
 122;
yeccgoto('<boolean_literal>', 167) ->
 122;
yeccgoto('<boolean_literal>', 171) ->
 122;
yeccgoto('<boolean_literal>', 173) ->
 122;
yeccgoto('<boolean_literal>', 190) ->
 122;
yeccgoto('<boolean_literal>', 258) ->
 122;
yeccgoto('<boolean_type>', 18) ->
 255;
yeccgoto('<boolean_type>', 24) ->
 96;
yeccgoto('<boolean_type>', 28) ->
 40;
yeccgoto('<boolean_type>', 78) ->
 96;
yeccgoto('<boolean_type>', 165) ->
 96;
yeccgoto('<boolean_type>', 205) ->
 96;
yeccgoto('<boolean_type>', 275) ->
 96;
yeccgoto('<boolean_type>', 303) ->
 96;
yeccgoto('<boolean_type>', 335) ->
 96;
yeccgoto('<case>', 57) ->
 61;
yeccgoto('<case>', 59) ->
 63;
yeccgoto('<case_label>', 64) ->
 194;
yeccgoto('<case_label>', 78) ->
 95;
yeccgoto('<char_type>', 18) ->
 254;
yeccgoto('<char_type>', 24) ->
 94;
yeccgoto('<char_type>', 28) ->
 39;
yeccgoto('<char_type>', 78) ->
 94;
yeccgoto('<char_type>', 165) ->
 94;
yeccgoto('<char_type>', 205) ->
 94;
yeccgoto('<char_type>', 275) ->
 94;
yeccgoto('<char_type>', 303) ->
 94;
yeccgoto('<char_type>', 335) ->
 94;
yeccgoto('<complex_declarator>', 80) ->
 185;
yeccgoto('<complex_declarator>', 196) ->
 185;
yeccgoto('<complex_declarator>', 201) ->
 185;
yeccgoto('<complex_declarator>', 211) ->
 185;
yeccgoto('<const_dcl>', 0) ->
 16;
yeccgoto('<const_dcl>', 1) ->
 16;
yeccgoto('<const_dcl>', 217) ->
 16;
yeccgoto('<const_dcl>', 218) ->
 16;
yeccgoto('<const_dcl>', 271) ->
 283;
yeccgoto('<const_exp>', 101) ->
 177;
yeccgoto('<const_exp>', 110) ->
 121;
yeccgoto('<const_exp>', 125) ->
 138;
yeccgoto('<const_exp>', 162) ->
 121;
yeccgoto('<const_exp>', 167) ->
 121;
yeccgoto('<const_exp>', 171) ->
 121;
yeccgoto('<const_exp>', 173) ->
 121;
yeccgoto('<const_exp>', 190) ->
 121;
yeccgoto('<const_exp>', 258) ->
 259;
yeccgoto('<const_type>', 18) ->
 253;
yeccgoto('<constr_type_spec>', 24) ->
 93;
yeccgoto('<constr_type_spec>', 78) ->
 93;
yeccgoto('<constr_type_spec>', 205) ->
 93;
yeccgoto('<context_expr>', 319) ->
 327;
yeccgoto('<declarator>', 80) ->
 184;
yeccgoto('<declarator>', 196) ->
 199;
yeccgoto('<declarator>', 201) ->
 202;
yeccgoto('<declarator>', 211) ->
 199;
yeccgoto('<declarators>', 196) ->
 198;
yeccgoto('<declarators>', 211) ->
 212;
yeccgoto('<definition>', 0) ->
 15;
yeccgoto('<definition>', 1) ->
 219;
yeccgoto('<definition>', 217) ->
 15;
yeccgoto('<definition>', 218) ->
 219;
yeccgoto('<element_spec>', 78) ->
 92;
yeccgoto('<enum_type>', 0) ->
 14;
yeccgoto('<enum_type>', 1) ->
 14;
yeccgoto('<enum_type>', 24) ->
 91;
yeccgoto('<enum_type>', 28) ->
 38;
yeccgoto('<enum_type>', 78) ->
 91;
yeccgoto('<enum_type>', 205) ->
 91;
yeccgoto('<enum_type>', 217) ->
 14;
yeccgoto('<enum_type>', 218) ->
 14;
yeccgoto('<enum_type>', 271) ->
 14;
yeccgoto('<enumerator>', 237) ->
 238;
yeccgoto('<enumerator>', 245) ->
 246;
yeccgoto('<except_dcl>', 0) ->
 13;
yeccgoto('<except_dcl>', 1) ->
 13;
yeccgoto('<except_dcl>', 217) ->
 13;
yeccgoto('<except_dcl>', 218) ->
 13;
yeccgoto('<except_dcl>', 271) ->
 282;
yeccgoto('<export>', 271) ->
 281;
yeccgoto('<fixed_array_size>', 187) ->
 189;
yeccgoto('<fixed_array_size>', 188) ->
 193;
yeccgoto('<fixed_pt_const_type>', 18) ->
 252;
yeccgoto('<fixed_pt_type>', 24) ->
 90;
yeccgoto('<fixed_pt_type>', 78) ->
 90;
yeccgoto('<fixed_pt_type>', 165) ->
 90;
yeccgoto('<fixed_pt_type>', 205) ->
 90;
yeccgoto('<floating_pt_type>', 18) ->
 251;
yeccgoto('<floating_pt_type>', 24) ->
 89;
yeccgoto('<floating_pt_type>', 78) ->
 89;
yeccgoto('<floating_pt_type>', 165) ->
 89;
yeccgoto('<floating_pt_type>', 205) ->
 89;
yeccgoto('<floating_pt_type>', 275) ->
 89;
yeccgoto('<floating_pt_type>', 303) ->
 89;
yeccgoto('<floating_pt_type>', 335) ->
 89;
yeccgoto('<forward_dcl>', 0) ->
 12;
yeccgoto('<forward_dcl>', 1) ->
 12;
yeccgoto('<forward_dcl>', 217) ->
 12;
yeccgoto('<forward_dcl>', 218) ->
 12;
yeccgoto('<inheritance_spec>', 221) ->
 223;
yeccgoto('<integer_type>', 18) ->
 250;
yeccgoto('<integer_type>', 24) ->
 88;
yeccgoto('<integer_type>', 28) ->
 37;
yeccgoto('<integer_type>', 78) ->
 88;
yeccgoto('<integer_type>', 165) ->
 88;
yeccgoto('<integer_type>', 205) ->
 88;
yeccgoto('<integer_type>', 275) ->
 88;
yeccgoto('<integer_type>', 303) ->
 88;
yeccgoto('<integer_type>', 335) ->
 88;
yeccgoto('<interface>', 0) ->
 11;
yeccgoto('<interface>', 1) ->
 11;
yeccgoto('<interface>', 217) ->
 11;
yeccgoto('<interface>', 218) ->
 11;
yeccgoto('<interface_body>', 270) ->
 272;
yeccgoto('<interface_dcl>', 0) ->
 10;
yeccgoto('<interface_dcl>', 1) ->
 10;
yeccgoto('<interface_dcl>', 217) ->
 10;
yeccgoto('<interface_dcl>', 218) ->
 10;
yeccgoto('<interface_header>', 0) ->
 9;
yeccgoto('<interface_header>', 1) ->
 9;
yeccgoto('<interface_header>', 217) ->
 9;
yeccgoto('<interface_header>', 218) ->
 9;
yeccgoto('<literal>', 101) ->
 120;
yeccgoto('<literal>', 110) ->
 120;
yeccgoto('<literal>', 112) ->
 120;
yeccgoto('<literal>', 125) ->
 120;
yeccgoto('<literal>', 140) ->
 120;
yeccgoto('<literal>', 141) ->
 120;
yeccgoto('<literal>', 143) ->
 120;
yeccgoto('<literal>', 144) ->
 120;
yeccgoto('<literal>', 145) ->
 120;
yeccgoto('<literal>', 150) ->
 120;
yeccgoto('<literal>', 152) ->
 120;
yeccgoto('<literal>', 153) ->
 120;
yeccgoto('<literal>', 156) ->
 120;
yeccgoto('<literal>', 158) ->
 120;
yeccgoto('<literal>', 162) ->
 120;
yeccgoto('<literal>', 167) ->
 120;
yeccgoto('<literal>', 171) ->
 120;
yeccgoto('<literal>', 173) ->
 120;
yeccgoto('<literal>', 190) ->
 120;
yeccgoto('<literal>', 258) ->
 120;
yeccgoto('<member>', 204) ->
 208;
yeccgoto('<member>', 206) ->
 210;
yeccgoto('<member>', 231) ->
 233;
yeccgoto('<member_list>', 204) ->
 207;
yeccgoto('<module>', 0) ->
 8;
yeccgoto('<module>', 1) ->
 8;
yeccgoto('<module>', 217) ->
 8;
yeccgoto('<module>', 218) ->
 8;
yeccgoto('<mult_expr>', 101) ->
 119;
yeccgoto('<mult_expr>', 110) ->
 119;
yeccgoto('<mult_expr>', 125) ->
 119;
yeccgoto('<mult_expr>', 140) ->
 149;
yeccgoto('<mult_expr>', 141) ->
 142;
yeccgoto('<mult_expr>', 150) ->
 119;
yeccgoto('<mult_expr>', 152) ->
 119;
yeccgoto('<mult_expr>', 153) ->
 119;
yeccgoto('<mult_expr>', 156) ->
 119;
yeccgoto('<mult_expr>', 158) ->
 119;
yeccgoto('<mult_expr>', 162) ->
 119;
yeccgoto('<mult_expr>', 167) ->
 119;
yeccgoto('<mult_expr>', 171) ->
 119;
yeccgoto('<mult_expr>', 173) ->
 119;
yeccgoto('<mult_expr>', 190) ->
 119;
yeccgoto('<mult_expr>', 258) ->
 119;
yeccgoto('<octet_type>', 18) ->
 249;
yeccgoto('<octet_type>', 24) ->
 87;
yeccgoto('<octet_type>', 78) ->
 87;
yeccgoto('<octet_type>', 165) ->
 87;
yeccgoto('<octet_type>', 205) ->
 87;
yeccgoto('<octet_type>', 275) ->
 87;
yeccgoto('<octet_type>', 303) ->
 87;
yeccgoto('<octet_type>', 335) ->
 87;
yeccgoto('<op_attribute>', 271) ->
 280;
yeccgoto('<op_dcl>', 271) ->
 279;
yeccgoto('<op_type_spec>', 275) ->
 295;
yeccgoto('<or_expr>', 101) ->
 118;
yeccgoto('<or_expr>', 110) ->
 118;
yeccgoto('<or_expr>', 125) ->
 118;
yeccgoto('<or_expr>', 162) ->
 118;
yeccgoto('<or_expr>', 167) ->
 118;
yeccgoto('<or_expr>', 171) ->
 118;
yeccgoto('<or_expr>', 173) ->
 118;
yeccgoto('<or_expr>', 190) ->
 118;
yeccgoto('<or_expr>', 258) ->
 118;
yeccgoto('<param_attribute>', 301) ->
 303;
yeccgoto('<param_attribute>', 316) ->
 303;
yeccgoto('<param_dcl>', 301) ->
 302;
yeccgoto('<param_dcl>', 316) ->
 317;
yeccgoto('<param_type_spec>', 275) ->
 294;
yeccgoto('<param_type_spec>', 303) ->
 308;
yeccgoto('<param_type_spec>', 335) ->
 336;
yeccgoto('<parameter_dcls>', 298) ->
 299;
yeccgoto('<positive_int_const>', 110) ->
 117;
yeccgoto('<positive_int_const>', 162) ->
 163;
yeccgoto('<positive_int_const>', 167) ->
 169;
yeccgoto('<positive_int_const>', 171) ->
 172;
yeccgoto('<positive_int_const>', 173) ->
 174;
yeccgoto('<positive_int_const>', 190) ->
 191;
yeccgoto('<primary_expr>', 101) ->
 116;
yeccgoto('<primary_expr>', 110) ->
 116;
yeccgoto('<primary_expr>', 112) ->
 161;
yeccgoto('<primary_expr>', 125) ->
 116;
yeccgoto('<primary_expr>', 140) ->
 116;
yeccgoto('<primary_expr>', 141) ->
 116;
yeccgoto('<primary_expr>', 143) ->
 116;
yeccgoto('<primary_expr>', 144) ->
 116;
yeccgoto('<primary_expr>', 145) ->
 116;
yeccgoto('<primary_expr>', 150) ->
 116;
yeccgoto('<primary_expr>', 152) ->
 116;
yeccgoto('<primary_expr>', 153) ->
 116;
yeccgoto('<primary_expr>', 156) ->
 116;
yeccgoto('<primary_expr>', 158) ->
 116;
yeccgoto('<primary_expr>', 162) ->
 116;
yeccgoto('<primary_expr>', 167) ->
 116;
yeccgoto('<primary_expr>', 171) ->
 116;
yeccgoto('<primary_expr>', 173) ->
 116;
yeccgoto('<primary_expr>', 190) ->
 116;
yeccgoto('<primary_expr>', 258) ->
 116;
yeccgoto('<raises_expr>', 299) ->
 320;
yeccgoto('<scoped_name>', 18) ->
 248;
yeccgoto('<scoped_name>', 24) ->
 86;
yeccgoto('<scoped_name>', 28) ->
 36;
yeccgoto('<scoped_name>', 78) ->
 86;
yeccgoto('<scoped_name>', 101) ->
 115;
yeccgoto('<scoped_name>', 110) ->
 115;
yeccgoto('<scoped_name>', 112) ->
 115;
yeccgoto('<scoped_name>', 125) ->
 115;
yeccgoto('<scoped_name>', 140) ->
 115;
yeccgoto('<scoped_name>', 141) ->
 115;
yeccgoto('<scoped_name>', 143) ->
 115;
yeccgoto('<scoped_name>', 144) ->
 115;
yeccgoto('<scoped_name>', 145) ->
 115;
yeccgoto('<scoped_name>', 150) ->
 115;
yeccgoto('<scoped_name>', 152) ->
 115;
yeccgoto('<scoped_name>', 153) ->
 115;
yeccgoto('<scoped_name>', 156) ->
 115;
yeccgoto('<scoped_name>', 158) ->
 115;
yeccgoto('<scoped_name>', 162) ->
 115;
yeccgoto('<scoped_name>', 165) ->
 86;
yeccgoto('<scoped_name>', 167) ->
 115;
yeccgoto('<scoped_name>', 171) ->
 115;
yeccgoto('<scoped_name>', 173) ->
 115;
yeccgoto('<scoped_name>', 190) ->
 115;
yeccgoto('<scoped_name>', 205) ->
 86;
yeccgoto('<scoped_name>', 224) ->
 225;
yeccgoto('<scoped_name>', 227) ->
 228;
yeccgoto('<scoped_name>', 258) ->
 115;
yeccgoto('<scoped_name>', 275) ->
 293;
yeccgoto('<scoped_name>', 303) ->
 293;
yeccgoto('<scoped_name>', 322) ->
 323;
yeccgoto('<scoped_name>', 335) ->
 293;
yeccgoto('<sequence_type>', 24) ->
 85;
yeccgoto('<sequence_type>', 78) ->
 85;
yeccgoto('<sequence_type>', 165) ->
 85;
yeccgoto('<sequence_type>', 205) ->
 85;
yeccgoto('<shift_expr>', 101) ->
 114;
yeccgoto('<shift_expr>', 110) ->
 114;
yeccgoto('<shift_expr>', 125) ->
 114;
yeccgoto('<shift_expr>', 150) ->
 151;
yeccgoto('<shift_expr>', 156) ->
 114;
yeccgoto('<shift_expr>', 158) ->
 114;
yeccgoto('<shift_expr>', 162) ->
 114;
yeccgoto('<shift_expr>', 167) ->
 114;
yeccgoto('<shift_expr>', 171) ->
 114;
yeccgoto('<shift_expr>', 173) ->
 114;
yeccgoto('<shift_expr>', 190) ->
 114;
yeccgoto('<shift_expr>', 258) ->
 114;
yeccgoto('<signed_int>', 18) ->
 35;
yeccgoto('<signed_int>', 24) ->
 35;
yeccgoto('<signed_int>', 28) ->
 35;
yeccgoto('<signed_int>', 78) ->
 35;
yeccgoto('<signed_int>', 165) ->
 35;
yeccgoto('<signed_int>', 205) ->
 35;
yeccgoto('<signed_int>', 275) ->
 35;
yeccgoto('<signed_int>', 303) ->
 35;
yeccgoto('<signed_int>', 335) ->
 35;
yeccgoto('<signed_long_int>', 18) ->
 34;
yeccgoto('<signed_long_int>', 24) ->
 34;
yeccgoto('<signed_long_int>', 28) ->
 34;
yeccgoto('<signed_long_int>', 78) ->
 34;
yeccgoto('<signed_long_int>', 165) ->
 34;
yeccgoto('<signed_long_int>', 205) ->
 34;
yeccgoto('<signed_long_int>', 275) ->
 34;
yeccgoto('<signed_long_int>', 303) ->
 34;
yeccgoto('<signed_long_int>', 335) ->
 34;
yeccgoto('<signed_short_int>', 18) ->
 33;
yeccgoto('<signed_short_int>', 24) ->
 33;
yeccgoto('<signed_short_int>', 28) ->
 33;
yeccgoto('<signed_short_int>', 78) ->
 33;
yeccgoto('<signed_short_int>', 165) ->
 33;
yeccgoto('<signed_short_int>', 205) ->
 33;
yeccgoto('<signed_short_int>', 275) ->
 33;
yeccgoto('<signed_short_int>', 303) ->
 33;
yeccgoto('<signed_short_int>', 335) ->
 33;
yeccgoto('<simple_declarator>', 80) ->
 183;
yeccgoto('<simple_declarator>', 196) ->
 183;
yeccgoto('<simple_declarator>', 201) ->
 183;
yeccgoto('<simple_declarator>', 211) ->
 183;
yeccgoto('<simple_declarator>', 308) ->
 309;
yeccgoto('<simple_declarator>', 336) ->
 337;
yeccgoto('<simple_declarator>', 339) ->
 340;
yeccgoto('<simple_type_spec>', 24) ->
 84;
yeccgoto('<simple_type_spec>', 78) ->
 84;
yeccgoto('<simple_type_spec>', 165) ->
 166;
yeccgoto('<simple_type_spec>', 205) ->
 84;
yeccgoto('<specification>', 0) ->
 7;
yeccgoto('<string_type>', 18) ->
 247;
yeccgoto('<string_type>', 24) ->
 83;
yeccgoto('<string_type>', 78) ->
 83;
yeccgoto('<string_type>', 165) ->
 83;
yeccgoto('<string_type>', 205) ->
 83;
yeccgoto('<string_type>', 275) ->
 292;
yeccgoto('<string_type>', 303) ->
 292;
yeccgoto('<string_type>', 335) ->
 292;
yeccgoto('<struct_type>', 0) ->
 6;
yeccgoto('<struct_type>', 1) ->
 6;
yeccgoto('<struct_type>', 24) ->
 82;
yeccgoto('<struct_type>', 78) ->
 82;
yeccgoto('<struct_type>', 205) ->
 82;
yeccgoto('<struct_type>', 217) ->
 6;
yeccgoto('<struct_type>', 218) ->
 6;
yeccgoto('<struct_type>', 271) ->
 6;
yeccgoto('<switch_body>', 57) ->
 60;
yeccgoto('<switch_type_spec>', 28) ->
 32;
yeccgoto('<template_type_spec>', 24) ->
 81;
yeccgoto('<template_type_spec>', 78) ->
 81;
yeccgoto('<template_type_spec>', 165) ->
 81;
yeccgoto('<template_type_spec>', 205) ->
 81;
yeccgoto('<type_dcl>', 0) ->
 5;
yeccgoto('<type_dcl>', 1) ->
 5;
yeccgoto('<type_dcl>', 217) ->
 5;
yeccgoto('<type_dcl>', 218) ->
 5;
yeccgoto('<type_dcl>', 271) ->
 278;
yeccgoto('<type_declarator>', 24) ->
 197;
yeccgoto('<type_spec>', 24) ->
 196;
yeccgoto('<type_spec>', 78) ->
 80;
yeccgoto('<type_spec>', 205) ->
 211;
yeccgoto('<unary_expr>', 101) ->
 113;
yeccgoto('<unary_expr>', 110) ->
 113;
yeccgoto('<unary_expr>', 125) ->
 113;
yeccgoto('<unary_expr>', 140) ->
 113;
yeccgoto('<unary_expr>', 141) ->
 113;
yeccgoto('<unary_expr>', 143) ->
 148;
yeccgoto('<unary_expr>', 144) ->
 147;
yeccgoto('<unary_expr>', 145) ->
 146;
yeccgoto('<unary_expr>', 150) ->
 113;
yeccgoto('<unary_expr>', 152) ->
 113;
yeccgoto('<unary_expr>', 153) ->
 113;
yeccgoto('<unary_expr>', 156) ->
 113;
yeccgoto('<unary_expr>', 158) ->
 113;
yeccgoto('<unary_expr>', 162) ->
 113;
yeccgoto('<unary_expr>', 167) ->
 113;
yeccgoto('<unary_expr>', 171) ->
 113;
yeccgoto('<unary_expr>', 173) ->
 113;
yeccgoto('<unary_expr>', 190) ->
 113;
yeccgoto('<unary_expr>', 258) ->
 113;
yeccgoto('<unary_operator>', 101) ->
 112;
yeccgoto('<unary_operator>', 110) ->
 112;
yeccgoto('<unary_operator>', 125) ->
 112;
yeccgoto('<unary_operator>', 140) ->
 112;
yeccgoto('<unary_operator>', 141) ->
 112;
yeccgoto('<unary_operator>', 143) ->
 112;
yeccgoto('<unary_operator>', 144) ->
 112;
yeccgoto('<unary_operator>', 145) ->
 112;
yeccgoto('<unary_operator>', 150) ->
 112;
yeccgoto('<unary_operator>', 152) ->
 112;
yeccgoto('<unary_operator>', 153) ->
 112;
yeccgoto('<unary_operator>', 156) ->
 112;
yeccgoto('<unary_operator>', 158) ->
 112;
yeccgoto('<unary_operator>', 162) ->
 112;
yeccgoto('<unary_operator>', 167) ->
 112;
yeccgoto('<unary_operator>', 171) ->
 112;
yeccgoto('<unary_operator>', 173) ->
 112;
yeccgoto('<unary_operator>', 190) ->
 112;
yeccgoto('<unary_operator>', 258) ->
 112;
yeccgoto('<union_type>', 0) ->
 4;
yeccgoto('<union_type>', 1) ->
 4;
yeccgoto('<union_type>', 24) ->
 79;
yeccgoto('<union_type>', 78) ->
 79;
yeccgoto('<union_type>', 205) ->
 79;
yeccgoto('<union_type>', 217) ->
 4;
yeccgoto('<union_type>', 218) ->
 4;
yeccgoto('<union_type>', 271) ->
 4;
yeccgoto('<unsigned_int>', 18) ->
 31;
yeccgoto('<unsigned_int>', 24) ->
 31;
yeccgoto('<unsigned_int>', 28) ->
 31;
yeccgoto('<unsigned_int>', 78) ->
 31;
yeccgoto('<unsigned_int>', 165) ->
 31;
yeccgoto('<unsigned_int>', 205) ->
 31;
yeccgoto('<unsigned_int>', 275) ->
 31;
yeccgoto('<unsigned_int>', 303) ->
 31;
yeccgoto('<unsigned_int>', 335) ->
 31;
yeccgoto('<unsigned_long_int>', 18) ->
 30;
yeccgoto('<unsigned_long_int>', 24) ->
 30;
yeccgoto('<unsigned_long_int>', 28) ->
 30;
yeccgoto('<unsigned_long_int>', 78) ->
 30;
yeccgoto('<unsigned_long_int>', 165) ->
 30;
yeccgoto('<unsigned_long_int>', 205) ->
 30;
yeccgoto('<unsigned_long_int>', 275) ->
 30;
yeccgoto('<unsigned_long_int>', 303) ->
 30;
yeccgoto('<unsigned_long_int>', 335) ->
 30;
yeccgoto('<unsigned_short_int>', 18) ->
 29;
yeccgoto('<unsigned_short_int>', 24) ->
 29;
yeccgoto('<unsigned_short_int>', 28) ->
 29;
yeccgoto('<unsigned_short_int>', 78) ->
 29;
yeccgoto('<unsigned_short_int>', 165) ->
 29;
yeccgoto('<unsigned_short_int>', 205) ->
 29;
yeccgoto('<unsigned_short_int>', 275) ->
 29;
yeccgoto('<unsigned_short_int>', 303) ->
 29;
yeccgoto('<unsigned_short_int>', 335) ->
 29;
yeccgoto('<xor_expr>', 101) ->
 111;
yeccgoto('<xor_expr>', 110) ->
 111;
yeccgoto('<xor_expr>', 125) ->
 111;
yeccgoto('<xor_expr>', 156) ->
 157;
yeccgoto('<xor_expr>', 162) ->
 111;
yeccgoto('<xor_expr>', 167) ->
 111;
yeccgoto('<xor_expr>', 171) ->
 111;
yeccgoto('<xor_expr>', 173) ->
 111;
yeccgoto('<xor_expr>', 190) ->
 111;
yeccgoto('<xor_expr>', 258) ->
 111;
yeccgoto('OE_pragma', 0) ->
 3;
yeccgoto('OE_pragma', 1) ->
 3;
yeccgoto('OE_pragma', 58) ->
 66;
yeccgoto('OE_pragma', 64) ->
 66;
yeccgoto('OE_pragma', 78) ->
 66;
yeccgoto('OE_pragma', 179) ->
 66;
yeccgoto('OE_pragma', 180) ->
 66;
yeccgoto('OE_pragma', 182) ->
 66;
yeccgoto('OE_pragma', 195) ->
 66;
yeccgoto('OE_pragma', 205) ->
 66;
yeccgoto('OE_pragma', 213) ->
 66;
yeccgoto('OE_pragma', 215) ->
 66;
yeccgoto('OE_pragma', 217) ->
 3;
yeccgoto('OE_pragma', 218) ->
 3;
yeccgoto('OE_pragma', 232) ->
 66;
yeccgoto('OE_pragma', 237) ->
 66;
yeccgoto('OE_pragma', 240) ->
 66;
yeccgoto('OE_pragma', 242) ->
 66;
yeccgoto('OE_pragma', 245) ->
 66;
yeccgoto('OE_pragma', 271) ->
 277;
yeccgoto('OE_pragma', 301) ->
 66;
yeccgoto('OE_pragma', 312) ->
 66;
yeccgoto('OE_pragma', 313) ->
 66;
yeccgoto('OE_pragma', 316) ->
 66;
yeccgoto('OE_pragma', 318) ->
 66;
yeccgoto('OE_preproc', 0) ->
 2;
yeccgoto('OE_preproc', 1) ->
 2;
yeccgoto('OE_preproc', 217) ->
 2;
yeccgoto('OE_preproc', 218) ->
 2;
yeccgoto('OE_preproc', 271) ->
 276;
yeccgoto('OorM_<case>', 57) ->
 59;
yeccgoto('OorM_<case_label>', 58) ->
 65;
yeccgoto('OorM_<definition>', 0) ->
 1;
yeccgoto('OorM_<definition>', 217) ->
 218;
yeccgoto('OorM_<fixed_array_size>', 187) ->
 188;
yeccgoto('OorM_<member>', 204) ->
 206;
yeccgoto('Opt_<context_expr>', 319) ->
 326;
yeccgoto('Opt_<inheritance_spec>', 221) ->
 222;
yeccgoto('Opt_<op_attribute>', 271) ->
 275;
yeccgoto('Opt_<raises_expr>', 299) ->
 319;
yeccgoto('Opt_readonly', 271) ->
 274;
yeccgoto('Ugly_pragmas', 57) ->
 58;
yeccgoto('Ugly_pragmas', 58) ->
 64;
yeccgoto('Ugly_pragmas', 59) ->
 58;
yeccgoto('Ugly_pragmas', 65) ->
 78;
yeccgoto('Ugly_pragmas', 92) ->
 180;
yeccgoto('Ugly_pragmas', 95) ->
 179;
yeccgoto('Ugly_pragmas', 181) ->
 182;
yeccgoto('Ugly_pragmas', 194) ->
 195;
yeccgoto('Ugly_pragmas', 204) ->
 205;
yeccgoto('Ugly_pragmas', 206) ->
 205;
yeccgoto('Ugly_pragmas', 212) ->
 213;
yeccgoto('Ugly_pragmas', 214) ->
 215;
yeccgoto('Ugly_pragmas', 230) ->
 232;
yeccgoto('Ugly_pragmas', 231) ->
 205;
yeccgoto('Ugly_pragmas', 236) ->
 237;
yeccgoto('Ugly_pragmas', 238) ->
 240;
yeccgoto('Ugly_pragmas', 241) ->
 242;
yeccgoto('Ugly_pragmas', 243) ->
 245;
yeccgoto('Ugly_pragmas', 300) ->
 301;
yeccgoto('Ugly_pragmas', 302) ->
 312;
yeccgoto('Ugly_pragmas', 311) ->
 313;
yeccgoto('Ugly_pragmas', 315) ->
 316;
yeccgoto('Ugly_pragmas', 317) ->
 318;
yeccgoto('ZorM_<declarator>', 199) ->
 200;
yeccgoto('ZorM_<enumerator>', 240) ->
 241;
yeccgoto('ZorM_<export>', 270) ->
 271;
yeccgoto('ZorM_<integer_literal>', 262) ->
 263;
yeccgoto('ZorM_<integer_literal>', 264) ->
 265;
yeccgoto('ZorM_<member>', 230) ->
 231;
yeccgoto('ZorM_<param_dcl>', 302) ->
 311;
yeccgoto('ZorM_<scoped_name>', 225) ->
 226;
yeccgoto('ZorM_<scoped_name>', 323) ->
 324;
yeccgoto('ZorM_<simple_declarator>', 337) ->
 338;
yeccgoto('ZorM_<string_literal>', 330) ->
 331;
yeccgoto(__Symbol, __State) ->
 erlang:error({yecc_bug,"1.1",{__Symbol, __State, missing_in_goto_table}}).

-compile({inline,{yeccpars2_1_,1}}).
-file("icparse.yrl", 286).
yeccpars2_1_([__1 | __Stack]) ->
 [begin
   reverse ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_15_,1}}).
-file("icparse.yrl", 290).
yeccpars2_15_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_31_,1}}).
-file("icparse.yrl", 541).
yeccpars2_31_([__1 | __Stack]) ->
 [begin
   { unsigned , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_42_,1}}).
-file("icparse.yrl", 369).
yeccpars2_42_([__1 | __Stack]) ->
 [begin
   ic_symtab : scoped_id_new ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_49_,1}}).
-file("icparse.yrl", 564).
yeccpars2_49_([__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_50_,1}}).
-file("icparse.yrl", 569).
yeccpars2_50_([__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_51_,1}}).
-file("icparse.yrl", 565).
yeccpars2_51_([__3,__2,__1 | __Stack]) ->
 [begin
   { 'long long' , element ( 2 , __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_52_,1}}).
-file("icparse.yrl", 551).
yeccpars2_52_([__2,__1 | __Stack]) ->
 [begin
   { 'long long' , element ( 2 , __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_53_,1}}).
-file("icparse.yrl", 370).
yeccpars2_53_([__2,__1 | __Stack]) ->
 [begin
   ic_symtab : scoped_id_new_global ( __2 )
  end | __Stack].

-compile({inline,{yeccpars2_55_,1}}).
-file("icparse.yrl", 372).
yeccpars2_55_([__3,__2,__1 | __Stack]) ->
 [begin
   ic_symtab : scoped_id_add ( __1 , __3 )
  end | __Stack].

-compile({inline,{yeccpars2_57_,1}}).
-file("icparse.yrl", 257).
yeccpars2_57_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_58_,1}}).
-file("icparse.yrl", 257).
yeccpars2_58_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{'yeccpars2_59_#',1}}).
-file("icparse.yrl", 257).
'yeccpars2_59_#'(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_59_case,1}}).
-file("icparse.yrl", 257).
yeccpars2_59_case(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_59_default,1}}).
-file("icparse.yrl", 257).
yeccpars2_59_default(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_59_,1}}).
-file("icparse.yrl", 639).
yeccpars2_59_([__1 | __Stack]) ->
 [begin
   reverse ( lists : flatten ( __1 ) )
  end | __Stack].

-compile({inline,{yeccpars2_61_,1}}).
-file("icparse.yrl", 645).
yeccpars2_61_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_62_,1}}).
-file("icparse.yrl", 627).
yeccpars2_62_([__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # union { id = __2 , type = __5 , body = __8 }
  end | __Stack].

-compile({inline,{yeccpars2_63_,1}}).
-file("icparse.yrl", 646).
yeccpars2_63_([__2,__1 | __Stack]) ->
 [begin
   [ __2 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_65_,1}}).
-file("icparse.yrl", 257).
yeccpars2_65_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_66_,1}}).
-file("icparse.yrl", 258).
yeccpars2_66_([__2,__1 | __Stack]) ->
 [begin
   [ __2 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_73_,1}}).
-file("icparse.yrl", 238).
yeccpars2_73_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # pragma { type = __4 , to = followed , apply = __5 }
  end | __Stack].

-compile({inline,{yeccpars2_76_,1}}).
-file("icparse.yrl", 243).
yeccpars2_76_([__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # pragma { type = __4 , to = __5 , apply = __6 }
  end | __Stack].

-compile({inline,{yeccpars2_77_,1}}).
-file("icparse.yrl", 248).
yeccpars2_77_([__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # pragma { type = __4 , to = __5 , apply = ic_options : float_to_version ( __6 ) }
  end | __Stack].

-compile({inline,{yeccpars2_92_,1}}).
-file("icparse.yrl", 257).
yeccpars2_92_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_95_,1}}).
-file("icparse.yrl", 257).
yeccpars2_95_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_108_,1}}).
-file("icparse.yrl", 714).
yeccpars2_108_([__1 | __Stack]) ->
 [begin
   # string { }
  end | __Stack].

-compile({inline,{yeccpars2_109_,1}}).
-file("icparse.yrl", 718).
yeccpars2_109_([__1 | __Stack]) ->
 [begin
   # wstring { }
  end | __Stack].

-compile({inline,{yeccpars2_139_,1}}).
-file("icparse.yrl", 443).
yeccpars2_139_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_142_,1}}).
-file("icparse.yrl", 419).
yeccpars2_142_([__3,__2,__1 | __Stack]) ->
 [begin
   { '-' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_146_,1}}).
-file("icparse.yrl", 425).
yeccpars2_146_([__3,__2,__1 | __Stack]) ->
 [begin
   { '/' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_147_,1}}).
-file("icparse.yrl", 424).
yeccpars2_147_([__3,__2,__1 | __Stack]) ->
 [begin
   { '*' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_148_,1}}).
-file("icparse.yrl", 426).
yeccpars2_148_([__3,__2,__1 | __Stack]) ->
 [begin
   { '%' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_149_,1}}).
-file("icparse.yrl", 418).
yeccpars2_149_([__3,__2,__1 | __Stack]) ->
 [begin
   { '+' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_151_,1}}).
-file("icparse.yrl", 407).
yeccpars2_151_([__3,__2,__1 | __Stack]) ->
 [begin
   { 'and' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_154_,1}}).
-file("icparse.yrl", 412).
yeccpars2_154_([__3,__2,__1 | __Stack]) ->
 [begin
   { rshift , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_155_,1}}).
-file("icparse.yrl", 413).
yeccpars2_155_([__3,__2,__1 | __Stack]) ->
 [begin
   { lshift , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_157_,1}}).
-file("icparse.yrl", 397).
yeccpars2_157_([__3,__2,__1 | __Stack]) ->
 [begin
   { 'or' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_159_,1}}).
-file("icparse.yrl", 402).
yeccpars2_159_([__3,__2,__1 | __Stack]) ->
 [begin
   { 'xor' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_160_,1}}).
-file("icparse.yrl", 717).
yeccpars2_160_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   # wstring { length = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_161_,1}}).
-file("icparse.yrl", 430).
yeccpars2_161_([__2,__1 | __Stack]) ->
 [begin
   { __1 , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_164_,1}}).
-file("icparse.yrl", 713).
yeccpars2_164_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   # string { length = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_168_,1}}).
-file("icparse.yrl", 708).
yeccpars2_168_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   # sequence { type = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_170_,1}}).
-file("icparse.yrl", 706).
yeccpars2_170_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # sequence { type = __3 , length = __5 }
  end | __Stack].

-compile({inline,{yeccpars2_175_,1}}).
-file("icparse.yrl", 828).
yeccpars2_175_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # fixed { digits = __3 , scale = __5 }
  end | __Stack].

-compile({inline,{yeccpars2_176_,1}}).
-file("icparse.yrl", 672).
yeccpars2_176_([__2,__1 | __Stack]) ->
 [begin
   __1
  end | __Stack].

-compile({inline,{yeccpars2_178_,1}}).
-file("icparse.yrl", 671).
yeccpars2_178_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_179_,1}}).
-file("icparse.yrl", 667).
yeccpars2_179_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   __2 ++ [ __3 | __1 ] ++ __4
  end | __Stack].

-compile({inline,{yeccpars2_181_,1}}).
-file("icparse.yrl", 257).
yeccpars2_181_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_182_,1}}).
-file("icparse.yrl", 657).
yeccpars2_182_([__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   __1 ++ __3 ++ __5 ++ __7 ++ [ __4 # case_dcl { label = reverse ( __2 ) } ]
  end | __Stack].

-compile({inline,{yeccpars2_184_,1}}).
-file("icparse.yrl", 677).
yeccpars2_184_([__2,__1 | __Stack]) ->
 [begin
   # case_dcl { type = __1 , id = __2 }
  end | __Stack].

-compile({inline,{yeccpars2_188_,1}}).
-file("icparse.yrl", 723).
yeccpars2_188_([__2,__1 | __Stack]) ->
 [begin
   # array { id = __1 , size = reverse ( __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_189_,1}}).
-file("icparse.yrl", 727).
yeccpars2_189_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_192_,1}}).
-file("icparse.yrl", 733).
yeccpars2_192_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_193_,1}}).
-file("icparse.yrl", 729).
yeccpars2_193_([__2,__1 | __Stack]) ->
 [begin
   [ __2 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_194_,1}}).
-file("icparse.yrl", 257).
yeccpars2_194_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_195_,1}}).
-file("icparse.yrl", 665).
yeccpars2_195_([__3,__2,__1 | __Stack]) ->
 [begin
   __1 ++ [ __2 ] ++ __3
  end | __Stack].

-compile({inline,{yeccpars2_197_,1}}).
-file("icparse.yrl", 467).
yeccpars2_197_([__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_198_,1}}).
-file("icparse.yrl", 474).
yeccpars2_198_([__2,__1 | __Stack]) ->
 [begin
   # typedef { type = __1 , id = __2 }
  end | __Stack].

-compile({inline,{yeccpars2_199_,1}}).
-file("icparse.yrl", 516).
yeccpars2_199_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_200_,1}}).
-file("icparse.yrl", 513).
yeccpars2_200_([__2,__1 | __Stack]) ->
 [begin
   [ __1 | reverse ( __2 ) ]
  end | __Stack].

-compile({inline,{yeccpars2_202_,1}}).
-file("icparse.yrl", 518).
yeccpars2_202_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __3 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_204_,1}}).
-file("icparse.yrl", 257).
yeccpars2_204_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{'yeccpars2_206_#',1}}).
-file("icparse.yrl", 257).
'yeccpars2_206_#'(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{'yeccpars2_206_::',1}}).
-file("icparse.yrl", 257).
'yeccpars2_206_::'(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{'yeccpars2_206_<identifier>',1}}).
-file("icparse.yrl", 257).
'yeccpars2_206_<identifier>'(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_206_Object,1}}).
-file("icparse.yrl", 257).
yeccpars2_206_Object(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_206_any,1}}).
-file("icparse.yrl", 257).
yeccpars2_206_any(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_206_boolean,1}}).
-file("icparse.yrl", 257).
yeccpars2_206_boolean(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_206_char,1}}).
-file("icparse.yrl", 257).
yeccpars2_206_char(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_206_double,1}}).
-file("icparse.yrl", 257).
yeccpars2_206_double(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_206_enum,1}}).
-file("icparse.yrl", 257).
yeccpars2_206_enum(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_206_fixed,1}}).
-file("icparse.yrl", 257).
yeccpars2_206_fixed(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_206_float,1}}).
-file("icparse.yrl", 257).
yeccpars2_206_float(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_206_long,1}}).
-file("icparse.yrl", 257).
yeccpars2_206_long(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_206_octet,1}}).
-file("icparse.yrl", 257).
yeccpars2_206_octet(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_206_sequence,1}}).
-file("icparse.yrl", 257).
yeccpars2_206_sequence(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_206_short,1}}).
-file("icparse.yrl", 257).
yeccpars2_206_short(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_206_string,1}}).
-file("icparse.yrl", 257).
yeccpars2_206_string(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_206_struct,1}}).
-file("icparse.yrl", 257).
yeccpars2_206_struct(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_206_union,1}}).
-file("icparse.yrl", 257).
yeccpars2_206_union(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_206_unsigned,1}}).
-file("icparse.yrl", 257).
yeccpars2_206_unsigned(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_206_wchar,1}}).
-file("icparse.yrl", 257).
yeccpars2_206_wchar(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_206_wstring,1}}).
-file("icparse.yrl", 257).
yeccpars2_206_wstring(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_206_,1}}).
-file("icparse.yrl", 599).
yeccpars2_206_([__1 | __Stack]) ->
 [begin
   reverse ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_209_,1}}).
-file("icparse.yrl", 595).
yeccpars2_209_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # struct { id = __2 , body = __4 }
  end | __Stack].

-compile({inline,{yeccpars2_210_,1}}).
-file("icparse.yrl", 609).
yeccpars2_210_([__2,__1 | __Stack]) ->
 [begin
   __2 ++ __1
  end | __Stack].

-compile({inline,{yeccpars2_212_,1}}).
-file("icparse.yrl", 257).
yeccpars2_212_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_214_,1}}).
-file("icparse.yrl", 257).
yeccpars2_214_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_215_,1}}).
-file("icparse.yrl", 618).
yeccpars2_215_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   __1 ++ __4 ++ __6 ++ [ # member { type = __2 , id = __3 } ]
  end | __Stack].

-compile({inline,{yeccpars2_219_,1}}).
-file("icparse.yrl", 292).
yeccpars2_219_([__2,__1 | __Stack]) ->
 [begin
   [ __2 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_220_,1}}).
-file("icparse.yrl", 307).
yeccpars2_220_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # module { id = __2 , body = reverse ( __4 ) }
  end | __Stack].

-compile({inline,{'yeccpars2_221_;',1}}).
-file("icparse.yrl", 323).
'yeccpars2_221_;'([__2,__1 | __Stack]) ->
 [begin
   # forward { id = __2 }
  end | __Stack].

-compile({inline,{yeccpars2_221_,1}}).
-file("icparse.yrl", 354).
yeccpars2_221_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_222_,1}}).
-file("icparse.yrl", 328).
yeccpars2_222_([__3,__2,__1 | __Stack]) ->
 [begin
   { __2 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_225_,1}}).
-file("icparse.yrl", 363).
yeccpars2_225_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_226_,1}}).
-file("icparse.yrl", 359).
yeccpars2_226_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __2 | reverse ( __3 ) ]
  end | __Stack].

-compile({inline,{yeccpars2_228_,1}}).
-file("icparse.yrl", 365).
yeccpars2_228_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __3 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_230_,1}}).
-file("icparse.yrl", 257).
yeccpars2_230_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_231_,1}}).
-file("icparse.yrl", 257).
yeccpars2_231_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_233_,1}}).
-file("icparse.yrl", 846).
yeccpars2_233_([__2,__1 | __Stack]) ->
 [begin
   __2 ++ __1
  end | __Stack].

-compile({inline,{yeccpars2_234_,1}}).
-file("icparse.yrl", 749).
yeccpars2_234_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # except { id = __2 , body = reverse ( __4 ) }
  end | __Stack].

-compile({inline,{yeccpars2_236_,1}}).
-file("icparse.yrl", 257).
yeccpars2_236_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_238_,1}}).
-file("icparse.yrl", 257).
yeccpars2_238_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_239_,1}}).
-file("icparse.yrl", 700).
yeccpars2_239_([__1 | __Stack]) ->
 [begin
   # enumerator { id = __1 }
  end | __Stack].

-compile({inline,{yeccpars2_240_,1}}).
-file("icparse.yrl", 695).
yeccpars2_240_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_241_,1}}).
-file("icparse.yrl", 257).
yeccpars2_241_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_243_,1}}).
-file("icparse.yrl", 257).
yeccpars2_243_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_244_,1}}).
-file("icparse.yrl", 687).
yeccpars2_244_([__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # enum { id = __2 , body = __4 ++ __6 ++ __8 ++ [ __5 | reverse ( __7 ) ] }
  end | __Stack].

-compile({inline,{yeccpars2_246_,1}}).
-file("icparse.yrl", 697).
yeccpars2_246_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   __2 ++ __4 ++ [ __5 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_259_,1}}).
-file("icparse.yrl", 377).
yeccpars2_259_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # const { type = __2 , id = __3 , val = __5 }
  end | __Stack].

-compile({inline,{yeccpars2_260_,1}}).
-file("icparse.yrl", 0).
yeccpars2_260_([__2,__1 | __Stack]) ->
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_262_,1}}).
-file("icparse.yrl", 281).
yeccpars2_262_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_264_,1}}).
-file("icparse.yrl", 281).
yeccpars2_264_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_265_,1}}).
-file("icparse.yrl", 283).
yeccpars2_265_([__2,__1 | __Stack]) ->
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_266_,1}}).
-file("icparse.yrl", 268).
yeccpars2_266_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   case __4 of
    [ ] ->
    case __2 of
    { _ , _ , "1" } ->
    # preproc { cat = line_nr , id = __3 , aux = __4 } ;
    _ ->
    [ ]
    end ;
    _ ->
    # preproc { cat = line_nr , id = __3 , aux = __4 }
    end
  end | __Stack].

-compile({inline,{yeccpars2_267_,1}}).
-file("icparse.yrl", 297).
yeccpars2_267_([__2,__1 | __Stack]) ->
 [begin
   __1
  end | __Stack].

-compile({inline,{yeccpars2_268_,1}}).
-file("icparse.yrl", 298).
yeccpars2_268_([__2,__1 | __Stack]) ->
 [begin
   __1
  end | __Stack].

-compile({inline,{yeccpars2_269_,1}}).
-file("icparse.yrl", 299).
yeccpars2_269_([__2,__1 | __Stack]) ->
 [begin
   __1
  end | __Stack].

-compile({inline,{yeccpars2_270_,1}}).
-file("icparse.yrl", 336).
yeccpars2_270_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{'yeccpars2_271_::',1}}).
-file("icparse.yrl", 756).
'yeccpars2_271_::'(__Stack) ->
 [begin
   nil
  end | __Stack].

-compile({inline,{'yeccpars2_271_<identifier>',1}}).
-file("icparse.yrl", 756).
'yeccpars2_271_<identifier>'(__Stack) ->
 [begin
   nil
  end | __Stack].

-compile({inline,{yeccpars2_271_Object,1}}).
-file("icparse.yrl", 756).
yeccpars2_271_Object(__Stack) ->
 [begin
   nil
  end | __Stack].

-compile({inline,{yeccpars2_271_any,1}}).
-file("icparse.yrl", 756).
yeccpars2_271_any(__Stack) ->
 [begin
   nil
  end | __Stack].

-compile({inline,{yeccpars2_271_boolean,1}}).
-file("icparse.yrl", 756).
yeccpars2_271_boolean(__Stack) ->
 [begin
   nil
  end | __Stack].

-compile({inline,{yeccpars2_271_char,1}}).
-file("icparse.yrl", 756).
yeccpars2_271_char(__Stack) ->
 [begin
   nil
  end | __Stack].

-compile({inline,{yeccpars2_271_double,1}}).
-file("icparse.yrl", 756).
yeccpars2_271_double(__Stack) ->
 [begin
   nil
  end | __Stack].

-compile({inline,{yeccpars2_271_float,1}}).
-file("icparse.yrl", 756).
yeccpars2_271_float(__Stack) ->
 [begin
   nil
  end | __Stack].

-compile({inline,{yeccpars2_271_long,1}}).
-file("icparse.yrl", 756).
yeccpars2_271_long(__Stack) ->
 [begin
   nil
  end | __Stack].

-compile({inline,{yeccpars2_271_octet,1}}).
-file("icparse.yrl", 756).
yeccpars2_271_octet(__Stack) ->
 [begin
   nil
  end | __Stack].

-compile({inline,{yeccpars2_271_short,1}}).
-file("icparse.yrl", 756).
yeccpars2_271_short(__Stack) ->
 [begin
   nil
  end | __Stack].

-compile({inline,{yeccpars2_271_string,1}}).
-file("icparse.yrl", 756).
yeccpars2_271_string(__Stack) ->
 [begin
   nil
  end | __Stack].

-compile({inline,{yeccpars2_271_unsigned,1}}).
-file("icparse.yrl", 756).
yeccpars2_271_unsigned(__Stack) ->
 [begin
   nil
  end | __Stack].

-compile({inline,{yeccpars2_271_void,1}}).
-file("icparse.yrl", 756).
yeccpars2_271_void(__Stack) ->
 [begin
   nil
  end | __Stack].

-compile({inline,{yeccpars2_271_wchar,1}}).
-file("icparse.yrl", 756).
yeccpars2_271_wchar(__Stack) ->
 [begin
   nil
  end | __Stack].

-compile({inline,{yeccpars2_271_wstring,1}}).
-file("icparse.yrl", 756).
yeccpars2_271_wstring(__Stack) ->
 [begin
   nil
  end | __Stack].

-compile({inline,{yeccpars2_271_attribute,1}}).
-file("icparse.yrl", 850).
yeccpars2_271_attribute(__Stack) ->
 [begin
   nil
  end | __Stack].

-compile({inline,{yeccpars2_273_,1}}).
-file("icparse.yrl", 317).
yeccpars2_273_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   # interface { id = element ( 1 , __1 ) , inherit = element ( 2 , __1 ) ,
    body = lists : reverse ( __3 ) }
  end | __Stack].

-compile({inline,{yeccpars2_281_,1}}).
-file("icparse.yrl", 339).
yeccpars2_281_([__2,__1 | __Stack]) ->
 [begin
   if list ( __2 ) -> __2 ++ __1 ;
    true -> [ __2 | __1 ]
    end
  end | __Stack].

-compile({inline,{yeccpars2_287_,1}}).
-file("icparse.yrl", 348).
yeccpars2_287_([__2,__1 | __Stack]) ->
 [begin
   __1
  end | __Stack].

-compile({inline,{yeccpars2_288_,1}}).
-file("icparse.yrl", 346).
yeccpars2_288_([__2,__1 | __Stack]) ->
 [begin
   __1
  end | __Stack].

-compile({inline,{yeccpars2_289_,1}}).
-file("icparse.yrl", 347).
yeccpars2_289_([__2,__1 | __Stack]) ->
 [begin
   __1
  end | __Stack].

-compile({inline,{yeccpars2_290_,1}}).
-file("icparse.yrl", 349).
yeccpars2_290_([__2,__1 | __Stack]) ->
 [begin
   __1
  end | __Stack].

-compile({inline,{yeccpars2_291_,1}}).
-file("icparse.yrl", 345).
yeccpars2_291_([__2,__1 | __Stack]) ->
 [begin
   __1
  end | __Stack].

-compile({inline,{yeccpars2_299_,1}}).
-file("icparse.yrl", 802).
yeccpars2_299_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_300_,1}}).
-file("icparse.yrl", 257).
yeccpars2_300_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_302_,1}}).
-file("icparse.yrl", 257).
yeccpars2_302_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_304_,1}}).
-file("icparse.yrl", 775).
yeccpars2_304_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_309_,1}}).
-file("icparse.yrl", 792).
yeccpars2_309_([__3,__2,__1 | __Stack]) ->
 [begin
   # param { inout = __1 , type = __2 , id = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_311_,1}}).
-file("icparse.yrl", 257).
yeccpars2_311_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_314_,1}}).
-file("icparse.yrl", 774).
yeccpars2_314_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   __2 ++ [ __3 | reverse ( __4 ) ]
  end | __Stack].

-compile({inline,{yeccpars2_315_,1}}).
-file("icparse.yrl", 257).
yeccpars2_315_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_317_,1}}).
-file("icparse.yrl", 257).
yeccpars2_317_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_318_,1}}).
-file("icparse.yrl", 785).
yeccpars2_318_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   __2 ++ __4 ++ __6 ++ [ __5 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_319_,1}}).
-file("icparse.yrl", 811).
yeccpars2_319_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_323_,1}}).
-file("icparse.yrl", 363).
yeccpars2_323_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_325_,1}}).
-file("icparse.yrl", 807).
yeccpars2_325_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   [ __3 | reverse ( __4 ) ]
  end | __Stack].

-compile({inline,{yeccpars2_326_,1}}).
-file("icparse.yrl", 753).
yeccpars2_326_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # op { oneway = __1 , type = __2 , id = __3 , params = __4 , raises = __5 , ctx = __6 }
  end | __Stack].

-compile({inline,{yeccpars2_330_,1}}).
-file("icparse.yrl", 832).
yeccpars2_330_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_332_,1}}).
-file("icparse.yrl", 816).
yeccpars2_332_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   [ __3 | reverse ( __4 ) ]
  end | __Stack].

-compile({inline,{yeccpars2_334_,1}}).
-file("icparse.yrl", 834).
yeccpars2_334_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __3 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_337_,1}}).
-file("icparse.yrl", 837).
yeccpars2_337_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_338_,1}}).
-file("icparse.yrl", 739).
yeccpars2_338_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   # attr { readonly = __1 , type = __3 , id = [ __4 | reverse ( __5 ) ] }
  end | __Stack].

-compile({inline,{yeccpars2_340_,1}}).
-file("icparse.yrl", 839).
yeccpars2_340_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __3 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_341_,1}}).
-file("icparse.yrl", 300).
yeccpars2_341_([__2,__1 | __Stack]) ->
 [begin
   __1
  end | __Stack].

-compile({inline,{yeccpars2_342_,1}}).
-file("icparse.yrl", 296).
yeccpars2_342_([__2,__1 | __Stack]) ->
 [begin
   __1
  end | __Stack].


-file("icparse.yrl", 863).
