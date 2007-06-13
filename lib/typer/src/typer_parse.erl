-module(typer_parse).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("typer_parse.yrl", 43).

build_type({atom,_,any}, []) -> {type, any, []};
build_type({atom,_,atom}, []) -> {type, atom, []};
build_type({atom,_,binary}, []) -> {type, binary, []};
build_type({atom,_,bool}, []) -> {type, bool, []};
build_type({atom,_,byte}, []) -> {type, byte, []};
build_type({atom,_,char}, []) -> {type, char, []};
build_type({atom,_,float}, []) -> {type, float, []};
build_type({atom,_,function}, []) -> {type, 'fun', []};
build_type({atom,_,identifier}, []) -> {type, identifier, []};
build_type({atom,_,integer}, []) -> {type, integer, []};
build_type({atom,_,list}, []) -> {type, list, []};
build_type({atom,_,mfa}, []) -> {type, mfa, []};
build_type({atom,_,neg_integer}, []) -> {type, neg_integer, []};
build_type({atom,_,non_neg_integer}, []) -> {type, non_neg_integer, []};
build_type({atom,_,none}, []) -> {type, none, []};
build_type({atom,_,nonempty_list}, [C,T]) -> {type, cons, [C,T]};
build_type({atom,_,nonempty_possibly_improper_list}, []) -> 
    {type, cons, []};
build_type({atom,_,nonempty_posssibly_improper_list}, [C,T]) -> 
    {type, cons, [C, T]};
build_type({atom,_,number}, []) -> {type, number, []};
build_type({atom,_,pid}, []) -> {type, pid, []};
build_type({atom,_,port}, []) -> {type, port, []};
build_type({atom,_,pos_integer}, []) -> {type, pos_integer, []};
build_type({atom,_,possibly_improper_list}, [C,T]) -> 
    {type, pos_improper_list, [C,T]};
build_type({atom,_,possibly_improper_list}, []) -> 
    {type, pos_improper_list, []};
build_type({atom,_,ref}, []) -> {type, ref, []};
build_type({atom,_,string}, []) -> {type, string, []};
build_type({atom,_,tuple}, []) -> {type, tuple, []};
build_type({atom,La,_}, _) -> error_bad_decl(La,type).

lift_unions(T1, {type, union, List}) ->
    {type, union, [T1|List]};
lift_unions(T1, T2 = {type, _, _}) ->
    {type, union, [T1, T2]}.

get_atom({atom, _, Atom}) -> Atom.

get_int({integer, _, Integer}) -> Integer.

error_bad_decl(L, S) ->
    return_error(L, io_lib:format("bad ~w declaration", [S])).

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



-file("./typer_parse.erl", 143).

yeccpars2(0, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(1, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 59, [1 | __Ss], [__T | __Stack]);
yeccpars2(1, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(top_type, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(2, dot, _, __Stack, _, _, _) ->
 {ok, hd(__Stack)};
yeccpars2(2, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(3, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [3 | __Ss], [__T | __Stack]);
yeccpars2(3, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(4, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 34, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(5, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [5 | __Ss], [__T | __Stack]);
yeccpars2(5, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(6, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(7, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [7 | __Ss], [__T | __Stack]);
yeccpars2(7, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_7_(__Stack),
 yeccpars2(yeccgoto(type, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(8, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_8_(__Stack),
 yeccpars2(yeccgoto(type, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(9, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 12, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(10, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 15, [10 | __Ss], [__T | __Stack]);
yeccpars2(10, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(11, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 13, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_11_(__Stack),
 yeccpars2(yeccgoto(top_types, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(12, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_12_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(13, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(14, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_14_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(top_types, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(15, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_15_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(16, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [16 | __Ss], [__T | __Stack]);
yeccpars2(16, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(17, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(18, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_18_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(19, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_19_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(20, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [20 | __Ss], [__T | __Stack]);
yeccpars2(20, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(21, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [21 | __Ss], [__T | __Stack]);
yeccpars2(21, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(22, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_22_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(23, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 25, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 26, [23 | __Ss], [__T | __Stack]);
yeccpars2(23, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(24, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_24_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(25, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 27, [25 | __Ss], [__T | __Stack]);
yeccpars2(25, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(26, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_26_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(27, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [27 | __Ss], [__T | __Stack]);
yeccpars2(27, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(28, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 29, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(29, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 30, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(30, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_30_(__Stack),
 __Nss = lists:nthtail(6, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(31, '>', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 32, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(32, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_32_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(33, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [33 | __Ss], [__T | __Stack]);
yeccpars2(33, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(34, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 35, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(35, '.', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(36, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 37, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(37, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 38, [37 | __Ss], [__T | __Stack]);
yeccpars2(37, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(38, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_38_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(39, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 44, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(40, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(41, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [41 | __Ss], [__T | __Stack]);
yeccpars2(41, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(42, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 43, [42 | __Ss], [__T | __Stack]);
yeccpars2(42, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(43, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_43_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(44, '->', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 45, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(45, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(46, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [46 | __Ss], [__T | __Stack]);
yeccpars2(46, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(47, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_47_(__Stack),
 __Nss = lists:nthtail(6, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(48, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [48 | __Ss], [__T | __Stack]);
yeccpars2(48, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(49, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [49 | __Ss], [__T | __Stack]);
yeccpars2(49, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [49 | __Ss], [__T | __Stack]);
yeccpars2(49, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(50, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 58, [50 | __Ss], [__T | __Stack]);
yeccpars2(50, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(51, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 56, [51 | __Ss], [__T | __Stack]);
yeccpars2(51, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_51_(__Stack),
 yeccpars2(yeccgoto(record_fields, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(52, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 54, [52 | __Ss], [__T | __Stack]);
yeccpars2(52, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(53, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_53_(__Stack),
 __Nss = lists:nthtail(3, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(54, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(55, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_55_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(record_field, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(56, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [56 | __Ss], [__T | __Stack]);
yeccpars2(56, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(57, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_57_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(record_fields, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(58, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_58_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(59, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 3, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 4, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, '<', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 5, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 6, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 7, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 8, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 9, [59 | __Ss], [__T | __Stack]);
yeccpars2(59, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(60, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_60_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(top_type, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(__Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.1",{missing_state_in_action_table, __Other}}).

yeccgoto(record_field, 49) ->
 51;
yeccgoto(record_field, 56) ->
 51;
yeccgoto(record_fields, 49) ->
 50;
yeccgoto(record_fields, 56) ->
 57;
yeccgoto(top_type, 0) ->
 2;
yeccgoto(top_type, 5) ->
 11;
yeccgoto(top_type, 6) ->
 23;
yeccgoto(top_type, 9) ->
 11;
yeccgoto(top_type, 13) ->
 11;
yeccgoto(top_type, 16) ->
 17;
yeccgoto(top_type, 20) ->
 21;
yeccgoto(top_type, 33) ->
 11;
yeccgoto(top_type, 41) ->
 42;
yeccgoto(top_type, 45) ->
 46;
yeccgoto(top_type, 54) ->
 55;
yeccgoto(top_type, 59) ->
 60;
yeccgoto(top_types, 5) ->
 31;
yeccgoto(top_types, 9) ->
 10;
yeccgoto(top_types, 13) ->
 14;
yeccgoto(top_types, 33) ->
 39;
yeccgoto(type, 0) ->
 1;
yeccgoto(type, 5) ->
 1;
yeccgoto(type, 6) ->
 1;
yeccgoto(type, 9) ->
 1;
yeccgoto(type, 13) ->
 1;
yeccgoto(type, 16) ->
 1;
yeccgoto(type, 20) ->
 1;
yeccgoto(type, 33) ->
 1;
yeccgoto(type, 41) ->
 1;
yeccgoto(type, 45) ->
 1;
yeccgoto(type, 54) ->
 1;
yeccgoto(type, 59) ->
 1;
yeccgoto(__Symbol, __State) ->
 erlang:error({yecc_bug,"1.1",{__Symbol, __State, missing_in_goto_table}}).

-compile({inline,{yeccpars2_7_,1}}).
-file("typer_parse.yrl", 17).
yeccpars2_7_([__1 | __Stack]) ->
 [begin
   { type , atom , [ get_atom ( __1 ) ] }
  end | __Stack].

-compile({inline,{yeccpars2_8_,1}}).
-file("typer_parse.yrl", 30).
yeccpars2_8_([__1 | __Stack]) ->
 [begin
   { type , integer , [ get_int ( __1 ) ] }
  end | __Stack].

-compile({inline,{yeccpars2_11_,1}}).
-file("typer_parse.yrl", 11).
yeccpars2_11_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_12_,1}}).
-file("typer_parse.yrl", 26).
yeccpars2_12_([__2,__1 | __Stack]) ->
 [begin
   { type , tuple , [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_14_,1}}).
-file("typer_parse.yrl", 12).
yeccpars2_14_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_15_,1}}).
-file("typer_parse.yrl", 27).
yeccpars2_15_([__3,__2,__1 | __Stack]) ->
 [begin
   { type , tuple , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_18_,1}}).
-file("typer_parse.yrl", 18).
yeccpars2_18_([__3,__2,__1 | __Stack]) ->
 [begin
   build_type ( __1 , [ ] )
  end | __Stack].

-compile({inline,{yeccpars2_19_,1}}).
-file("typer_parse.yrl", 19).
yeccpars2_19_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   build_type ( __1 , [ __3 ] )
  end | __Stack].

-compile({inline,{yeccpars2_22_,1}}).
-file("typer_parse.yrl", 20).
yeccpars2_22_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   build_type ( __1 , [ __3 , __5 ] )
  end | __Stack].

-compile({inline,{yeccpars2_24_,1}}).
-file("typer_parse.yrl", 21).
yeccpars2_24_([__2,__1 | __Stack]) ->
 [begin
   { type , nil , [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_26_,1}}).
-file("typer_parse.yrl", 22).
yeccpars2_26_([__3,__2,__1 | __Stack]) ->
 [begin
   { type , list , [ __2 ] }
  end | __Stack].

-compile({inline,{yeccpars2_30_,1}}).
-file("typer_parse.yrl", 23).
yeccpars2_30_([__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { type , nonempty_list , [ __2 ] }
  end | __Stack].

-compile({inline,{yeccpars2_32_,1}}).
-file("typer_parse.yrl", 31).
yeccpars2_32_([__3,__2,__1 | __Stack]) ->
 [begin
   { type , product , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_38_,1}}).
-file("typer_parse.yrl", 33).
yeccpars2_38_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { type , range , [ get_int ( __2 ) , get_int ( __5 ) ] }
  end | __Stack].

-compile({inline,{yeccpars2_43_,1}}).
-file("typer_parse.yrl", 24).
yeccpars2_43_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { type , 'fun' , [ [ ] , __5 ] }
  end | __Stack].

-compile({inline,{yeccpars2_47_,1}}).
-file("typer_parse.yrl", 25).
yeccpars2_47_([__7,__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { type , 'fun' , [ __3 , __6 ] }
  end | __Stack].

-compile({inline,{yeccpars2_51_,1}}).
-file("typer_parse.yrl", 35).
yeccpars2_51_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_53_,1}}).
-file("typer_parse.yrl", 28).
yeccpars2_53_([__4,__3,__2,__1 | __Stack]) ->
 [begin
   { type , record , [ get_atom ( __2 ) ] }
  end | __Stack].

-compile({inline,{yeccpars2_55_,1}}).
-file("typer_parse.yrl", 38).
yeccpars2_55_([__3,__2,__1 | __Stack]) ->
 [begin
   { get_atom ( __1 ) , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_57_,1}}).
-file("typer_parse.yrl", 36).
yeccpars2_57_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_58_,1}}).
-file("typer_parse.yrl", 29).
yeccpars2_58_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   { type , record , [ get_atom ( __2 ) | __4 ] }
  end | __Stack].

-compile({inline,{yeccpars2_60_,1}}).
-file("typer_parse.yrl", 15).
yeccpars2_60_([__3,__2,__1 | __Stack]) ->
 [begin
   lift_unions ( __1 , __3 )
  end | __Stack].


-file("typer_parse.yrl", 89).
