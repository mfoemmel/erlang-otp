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
-module(yeccparser).
-define(THIS_MODULE, yeccparser).
-export([parse/1, parse_and_scan/1, format_error/1]).

value_of(Token) ->
    element(3, Token).

line_of(Token) ->
    element(2, Token).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

parse(Tokens) ->
    case catch yeccpars1(Tokens, false, 0, [], []) of
	error ->
	    Errorline =
		if Tokens == [] -> 0; true -> element(2, hd(Tokens)) end,
	    {error,
	     {Errorline, ?THIS_MODULE, "syntax error at or after this line."}};
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

% Don't change yeccpars1/6 too much, it is called recursively by yeccpars2/8!
yeccpars1([Token | Tokens], Tokenizer, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens,
	      Tokenizer);
yeccpars1([], {M, F, A}, State, States, Vstack) ->
    case catch apply(M, F, A) of
        {eof, Endline} ->
            {error, {Endline, ?THIS_MODULE, "end_of_file"}};
        {error, Descriptor, _Endline} ->
            {error, Descriptor};
        {'EXIT', Reason} ->
            {error, {0, ?THIS_MODULE, Reason}};
        {ok, Tokens, _Endline} ->
	    case catch yeccpars1(Tokens, {M, F, A}, State, States, Vstack) of
		error ->
		    Errorline = element(2, hd(Tokens)),
		    {error, {Errorline, ?THIS_MODULE,
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
     {element(2, Token), ?THIS_MODULE,
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
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


yeccpars2(0, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(1, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  value_of(__1),
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(2, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(grammar, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(3, '$end', _, __Stack, _, _, _) ->
 {ok, hd(__Stack)};
yeccpars2(3, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(4, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(5, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  value_of(__1),
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(6, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(grammar, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(7, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(head, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(8, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  value_of(__1),
 yeccpars2(yeccgoto(symbol, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(9, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(symbols, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(10, dot, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 11, [10 | __Ss], [__T | __Stack]);
yeccpars2(10, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(11, __Cat, __Ss,  [__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {__1,__2},
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(declaration, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(12, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__2],
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(symbols, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(13, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 1, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(14, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [14 | __Ss], [__T | __Stack]);
yeccpars2(14, __Cat, __Ss,  __Stack, __T, __Ts, __Tzr) ->
 __Val =  {erlang_code,[{atom,0,'$undefined'}]},
 yeccpars2(16, __Cat, [14 | __Ss], [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(15, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, reserved_symbol, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(16, dot, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(17, __Cat, __Ss,  [__5,__4,__3,__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {rule,[__1|__3],__4},
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(rule, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(18, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {'->',line_of(__1)},
 yeccpars2(yeccgoto(token, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(19, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {':',line_of(__1)},
 yeccpars2(yeccgoto(token, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(20, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(token, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(21, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(token, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(22, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(token, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(23, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {value_of(__1),line_of(__1)},
 yeccpars2(yeccgoto(token, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(24, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(token, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(25, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, reserved_symbol, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, float, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1],
 yeccpars2(yeccgoto(tokens, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(26, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  {erlang_code,__2},
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(attached_code, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(27, __Cat, __Ss,  [__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  __1,
 yeccpars2(yeccgoto(token, hd(__Ss)), __Cat, __Ss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(28, __Cat, __Ss,  [__2,__1|__Stack], __T, __Ts, __Tzr) ->
 __Val =  [__1|__2],
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(tokens, hd(__Nss)), __Cat, __Nss, [__Val | __Stack], __T, __Ts, __Tzr);
yeccpars2(__Other, _, _, _, _, _, _) ->
 exit({parser, __Other, missing_state_in_action_table}).

yeccgoto(grammar, 0) ->
 3;
yeccgoto(declaration, 0) ->
 2;
yeccgoto(rule, 0) ->
 6;
yeccgoto(head, 0) ->
 4;
yeccgoto(symbol, 0) ->
 7;
yeccgoto(symbol, 7) ->
 9;
yeccgoto(symbol, 9) ->
 9;
yeccgoto(symbol, 13) ->
 9;
yeccgoto(symbols, 7) ->
 10;
yeccgoto(symbols, 9) ->
 12;
yeccgoto(symbols, 13) ->
 14;
yeccgoto(attached_code, 14) ->
 16;
yeccgoto(token, 15) ->
 25;
yeccgoto(token, 25) ->
 25;
yeccgoto(tokens, 15) ->
 26;
yeccgoto(tokens, 25) ->
 28;
yeccgoto(__Symbol, __State) ->
 exit({__Symbol, __State, missing_in_goto_table}).


