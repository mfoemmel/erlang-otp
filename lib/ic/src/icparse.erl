-module(icparse).
-file("/net/shelob/ldisk/daily_build/otp_prebuild_r13b.2009-04-20_20/otp_src_R13B/lib/ic/src/icyeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1997-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
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
        {error, Descriptor, _Endline} ->
            {error, Descriptor};
        {'EXIT', Reason} ->
            {error, {0, ?MODULE, Reason}};
        {ok, Tokens, _Endline} ->
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
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:format("~w", [A]);
yecctoken2string({'dot', _}) -> "'.'";
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when atom(Other) ->
    io_lib:format("~w", [Other]);
yecctoken2string({_, _, Other}) when list(Other), number(hd(Other)) ->
    Other;
yecctoken2string({_, _, Other}) ->
    io_lib:format("~p", [Other]);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-file("icparse.yrl", 860).
%%-----------------------------------------------------------




-file("./icparse.erl", 134).

yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_117(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_118(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_119(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(120=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_120(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_121(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_123(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_124(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(125=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(126=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(128=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(130=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_138(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_142(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_146(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(147=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(148=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_148(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(149=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(150=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(151=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_151(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(152=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(153=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(154=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_154(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(155=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_155(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(156=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(157=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(158=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(159=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_159(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(160=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(161=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_161(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(162=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(163=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(164=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_164(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(165=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(166=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_166(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(167=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(168=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(169=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(170=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(171=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(172=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_172(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(173=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(174=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(175=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(176=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(177=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_177(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(178=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(179=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_179(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(180=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_180(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(181=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(182=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_182(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(183=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_183(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(184=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_184(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(185=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_185(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(186=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_186(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(187=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_187(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(188=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_188(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(189=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_189(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(190=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(191=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_191(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(192=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(193=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_193(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(194=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_194(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(195=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_195(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(196=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(197=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_197(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(198=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_198(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(199=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_199(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(200=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_200(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(201=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(202=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_202(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(203=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(204=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(205=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_205(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(206=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_206(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(207=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_207(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(208=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_208(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(209=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_209(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(210=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_210(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(211=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(212=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_212(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(213=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_213(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(214=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(215=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_215(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(216=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_216(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(217=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(218=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_218(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(219=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_219(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(220=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_220(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(221=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(222=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_222(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(223=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_223(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(224=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_224(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(225=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_225(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(226=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_226(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(227=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_224(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(228=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_228(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(229=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(230=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(231=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_231(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(232=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_232(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(233=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_233(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(234=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_234(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(235=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_235(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(236=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_236(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(237=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_237(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(238=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_238(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(239=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_239(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(240=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_240(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(241=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_241(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(242=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_242(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(243=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(244=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(245=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_237(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(246=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_246(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(247=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_247(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(248=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_248(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(249=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_249(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(250=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_250(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(251=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_251(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(252=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_252(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(253=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_253(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(254=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_254(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(255=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_255(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(256=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_256(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(257=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_257(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(258=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(259=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_259(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(260=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_260(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(261=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_261(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(262=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_262(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(263=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_263(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(264=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_264(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(265=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_265(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(266=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_266(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(267=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_267(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(268=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_268(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(269=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_269(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(270=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_270(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(271=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_271(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(272=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_272(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(273=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_273(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(274=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_274(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(275=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_275(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(276=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_276(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(277=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_277(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(278=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_278(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(279=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_279(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(280=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_280(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(281=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_281(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(282=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_282(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(283=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_283(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(284=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_284(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(285=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_285(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(286=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_286(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(287=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_287(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(288=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_288(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(289=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_289(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(290=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_290(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(291=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_291(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(292=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_292(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(293=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_293(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(294=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_294(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(295=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_295(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(296=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_296(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(297=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_297(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(298=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_298(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(299=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_299(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(300=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_300(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(301=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_301(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(302=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_302(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(303=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_303(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(304=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_304(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(305=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_305(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(306=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_306(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(307=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_307(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(308=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_308(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(309=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_309(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(310=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_310(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(311=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_311(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(312=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_312(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(313=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_313(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(314=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_314(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(315=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_315(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(316=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_316(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(317=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_317(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(318=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_318(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(319=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_319(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(320=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_320(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(321=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_321(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(322=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_224(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(323=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_323(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(324=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_324(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(325=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_325(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(326=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_326(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(327=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_327(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(328=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_328(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(329=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_329(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(330=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_330(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(331=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_331(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(332=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_332(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(333=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_333(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(334=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_334(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(335=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_303(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(336=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_308(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(337=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_337(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(338=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_338(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(339=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_308(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(340=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_340(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(341=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(342=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.3",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 19, [S | Ss], [T | Stack]);
yeccpars2_0(S, struct, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 23, [S | Ss], [T | Stack]);
yeccpars2_0(S, union, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 25, [S | Ss], [T | Stack]);
yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_0(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_cont_0(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 17, [S | Ss], [T | Stack]);
yeccpars2_cont_0(S, const, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 18, [S | Ss], [T | Stack]);
yeccpars2_cont_0(S, exception, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 20, [S | Ss], [T | Stack]);
yeccpars2_cont_0(S, interface, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 21, [S | Ss], [T | Stack]);
yeccpars2_cont_0(S, module, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 22, [S | Ss], [T | Stack]);
yeccpars2_cont_0(S, typedef, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 24, [S | Ss], [T | Stack]);
yeccpars2_cont_0(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_1(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 17, [S | Ss], [T | Stack]);
yeccpars2_1(S, const, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 18, [S | Ss], [T | Stack]);
yeccpars2_1(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 19, [S | Ss], [T | Stack]);
yeccpars2_1(S, exception, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 20, [S | Ss], [T | Stack]);
yeccpars2_1(S, interface, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 21, [S | Ss], [T | Stack]);
yeccpars2_1(S, module, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 22, [S | Ss], [T | Stack]);
yeccpars2_1(S, struct, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 23, [S | Ss], [T | Stack]);
yeccpars2_1(S, typedef, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 24, [S | Ss], [T | Stack]);
yeccpars2_1(S, union, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 25, [S | Ss], [T | Stack]);
yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_1_(Stack),
 'yeccgoto_\'<specification>\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<definition>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<definition>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<type_dcl>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_5(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 342, [S | Ss], [T | Stack]);
yeccpars2_5(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<type_dcl>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_7(_S, '$end', _Ss, Stack,  _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_7(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_8(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 341, [S | Ss], [T | Stack]);
yeccpars2_8(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_9(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 270, [S | Ss], [T | Stack]);
yeccpars2_9(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<interface>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_11(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 269, [S | Ss], [T | Stack]);
yeccpars2_11(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<interface>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_13(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 268, [S | Ss], [T | Stack]);
yeccpars2_13(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<type_dcl>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_15_(Stack),
 'yeccgoto_\'OorM_<definition>\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_16(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 267, [S | Ss], [T | Stack]);
yeccpars2_16(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_17(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 260, [S | Ss], [T | Stack]);
yeccpars2_17(S, '<integer_literal>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 261, [S | Ss], [T | Stack]);
yeccpars2_17(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_18(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 41, [S | Ss], [T | Stack]);
yeccpars2_18(S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 42, [S | Ss], [T | Stack]);
yeccpars2_18(S, double, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 103, [S | Ss], [T | Stack]);
yeccpars2_18(S, fixed, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 256, [S | Ss], [T | Stack]);
yeccpars2_18(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 105, [S | Ss], [T | Stack]);
yeccpars2_18(S, octet, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 106, [S | Ss], [T | Stack]);
yeccpars2_18(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 108, [S | Ss], [T | Stack]);
yeccpars2_18(S, wstring, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 109, [S | Ss], [T | Stack]);
yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_18(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_cont_18(S, boolean, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 43, [S | Ss], [T | Stack]);
yeccpars2_cont_18(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 44, [S | Ss], [T | Stack]);
yeccpars2_cont_18(S, long, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 45, [S | Ss], [T | Stack]);
yeccpars2_cont_18(S, short, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 46, [S | Ss], [T | Stack]);
yeccpars2_cont_18(S, unsigned, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 47, [S | Ss], [T | Stack]);
yeccpars2_cont_18(S, wchar, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 48, [S | Ss], [T | Stack]);
yeccpars2_cont_18(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_19(S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 235, [S | Ss], [T | Stack]);
yeccpars2_19(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_20(S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 229, [S | Ss], [T | Stack]);
yeccpars2_20(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_21(S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 221, [S | Ss], [T | Stack]);
yeccpars2_21(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_22(S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 216, [S | Ss], [T | Stack]);
yeccpars2_22(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_23(S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 203, [S | Ss], [T | Stack]);
yeccpars2_23(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_24(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 41, [S | Ss], [T | Stack]);
yeccpars2_24(S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 42, [S | Ss], [T | Stack]);
yeccpars2_24(S, 'Object', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 99, [S | Ss], [T | Stack]);
yeccpars2_24(S, any, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 100, [S | Ss], [T | Stack]);
yeccpars2_24(S, double, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 103, [S | Ss], [T | Stack]);
yeccpars2_24(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 19, [S | Ss], [T | Stack]);
yeccpars2_24(S, fixed, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 104, [S | Ss], [T | Stack]);
yeccpars2_24(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 105, [S | Ss], [T | Stack]);
yeccpars2_24(S, octet, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 106, [S | Ss], [T | Stack]);
yeccpars2_24(S, sequence, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 107, [S | Ss], [T | Stack]);
yeccpars2_24(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 108, [S | Ss], [T | Stack]);
yeccpars2_24(S, struct, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 23, [S | Ss], [T | Stack]);
yeccpars2_24(S, union, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 25, [S | Ss], [T | Stack]);
yeccpars2_24(S, wstring, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 109, [S | Ss], [T | Stack]);
yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_18(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_25(S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 26, [S | Ss], [T | Stack]);
yeccpars2_25(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_26(S, switch, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 27, [S | Ss], [T | Stack]);
yeccpars2_26(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_27(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 28, [S | Ss], [T | Stack]);
yeccpars2_27(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_28(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 41, [S | Ss], [T | Stack]);
yeccpars2_28(S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 42, [S | Ss], [T | Stack]);
yeccpars2_28(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 19, [S | Ss], [T | Stack]);
yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_18(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<unsigned_int>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<unsigned_int>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_(Stack),
 'yeccgoto_\'<integer_type>\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_32(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 56, [S | Ss], [T | Stack]);
yeccpars2_32(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<signed_int>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<signed_int>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<integer_type>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_36(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 54, [S | Ss], [T | Stack]);
yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<switch_type_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<switch_type_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<switch_type_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<switch_type_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<switch_type_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_41(S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 53, [S | Ss], [T | Stack]);
yeccpars2_41(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_42_(Stack),
 'yeccgoto_\'<scoped_name>\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<boolean_type>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<char_type>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_45(S, long, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 52, [S | Ss], [T | Stack]);
yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<signed_long_int>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<signed_short_int>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_47(S, long, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 49, [S | Ss], [T | Stack]);
yeccpars2_47(S, short, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 50, [S | Ss], [T | Stack]);
yeccpars2_47(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<char_type>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_49(S, long, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 51, [S | Ss], [T | Stack]);
yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_49_(Stack),
 'yeccgoto_\'<unsigned_long_int>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_50_(Stack),
 'yeccgoto_\'<unsigned_short_int>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_51_(Stack),
 'yeccgoto_\'<unsigned_long_int>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_52_(Stack),
 'yeccgoto_\'<signed_long_int>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_53_(Stack),
 'yeccgoto_\'<scoped_name>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_54(S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 55, [S | Ss], [T | Stack]);
yeccpars2_54(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_55_(Stack),
 'yeccgoto_\'<scoped_name>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_56(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 57, [S | Ss], [T | Stack]);
yeccpars2_56(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_57_(Stack),
 yeccpars2_58(58, Cat, [57 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_58(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 67, [S | Ss], [T | Stack]);
yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_58_(Stack),
 yeccpars2_64(64, Cat, [58 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_59(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_59_#'(Stack),
 yeccpars2_58(58, '#', [59 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_59(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_59_case(Stack),
 yeccpars2_58(58, 'case', [59 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_59(_S, default, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_59_default(Stack),
 yeccpars2_58(58, default, [59 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_59_(Stack),
 'yeccgoto_\'<switch_body>\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_60(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 62, [S | Ss], [T | Stack]);
yeccpars2_60(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_61_(Stack),
 'yeccgoto_\'OorM_<case>\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_62_(Stack),
 'yeccgoto_\'<union_type>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_63_(Stack),
 'yeccgoto_\'OorM_<case>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_64(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 67, [S | Ss], [T | Stack]);
yeccpars2_64(S, 'case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 101, [S | Ss], [T | Stack]);
yeccpars2_64(S, default, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 102, [S | Ss], [T | Stack]);
yeccpars2_64(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_65_(Stack),
 yeccpars2_78(78, Cat, [65 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_66_(Stack),
 'yeccgoto_\'Ugly_pragmas\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_67(S, '<integer_literal>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 68, [S | Ss], [T | Stack]);
yeccpars2_67(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_68(S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 69, [S | Ss], [T | Stack]);
yeccpars2_68(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_69(S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 70, [S | Ss], [T | Stack]);
yeccpars2_69(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_70(S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 71, [S | Ss], [T | Stack]);
yeccpars2_70(S, '<string_literal>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 72, [S | Ss], [T | Stack]);
yeccpars2_70(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_71(S, '<floating_pt_literal>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 74, [S | Ss], [T | Stack]);
yeccpars2_71(S, '<string_literal>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 75, [S | Ss], [T | Stack]);
yeccpars2_71(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_72(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 73, [S | Ss], [T | Stack]);
yeccpars2_72(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_73_(Stack),
 'yeccgoto_\'OE_pragma\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_74(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 77, [S | Ss], [T | Stack]);
yeccpars2_74(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_75(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 76, [S | Ss], [T | Stack]);
yeccpars2_75(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_76_(Stack),
 'yeccgoto_\'OE_pragma\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_77_(Stack),
 'yeccgoto_\'OE_pragma\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_78(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 67, [S | Ss], [T | Stack]);
yeccpars2_78(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 41, [S | Ss], [T | Stack]);
yeccpars2_78(S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 42, [S | Ss], [T | Stack]);
yeccpars2_78(S, 'Object', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 99, [S | Ss], [T | Stack]);
yeccpars2_78(S, any, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 100, [S | Ss], [T | Stack]);
yeccpars2_78(S, 'case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 101, [S | Ss], [T | Stack]);
yeccpars2_78(S, default, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 102, [S | Ss], [T | Stack]);
yeccpars2_78(S, double, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 103, [S | Ss], [T | Stack]);
yeccpars2_78(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 19, [S | Ss], [T | Stack]);
yeccpars2_78(S, fixed, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 104, [S | Ss], [T | Stack]);
yeccpars2_78(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 105, [S | Ss], [T | Stack]);
yeccpars2_78(S, octet, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 106, [S | Ss], [T | Stack]);
yeccpars2_78(S, sequence, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 107, [S | Ss], [T | Stack]);
yeccpars2_78(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 108, [S | Ss], [T | Stack]);
yeccpars2_78(S, struct, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 23, [S | Ss], [T | Stack]);
yeccpars2_78(S, union, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 25, [S | Ss], [T | Stack]);
yeccpars2_78(S, wstring, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 109, [S | Ss], [T | Stack]);
yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_18(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<constr_type_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_80(S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 187, [S | Ss], [T | Stack]);
yeccpars2_80(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<simple_type_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<constr_type_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<template_type_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<type_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<template_type_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_86(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 54, [S | Ss], [T | Stack]);
yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<simple_type_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<base_type_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<base_type_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<base_type_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<template_type_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<constr_type_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_92_(Stack),
 yeccpars2_180(180, Cat, [92 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<type_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<base_type_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_95_(Stack),
 yeccpars2_179(179, Cat, [95 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<base_type_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<simple_type_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<base_type_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<base_type_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<any_type>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_101(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 126, [S | Ss], [T | Stack]);
yeccpars2_101(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 127, [S | Ss], [T | Stack]);
yeccpars2_101(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 41, [S | Ss], [T | Stack]);
yeccpars2_101(S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 42, [S | Ss], [T | Stack]);
yeccpars2_101(S, '~', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 137, [S | Ss], [T | Stack]);
yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_101(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_cont_101(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 125, [S | Ss], [T | Stack]);
yeccpars2_cont_101(S, '<character_literal>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 128, [S | Ss], [T | Stack]);
yeccpars2_cont_101(S, '<fixed_pt_literal>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 129, [S | Ss], [T | Stack]);
yeccpars2_cont_101(S, '<floating_pt_literal>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 130, [S | Ss], [T | Stack]);
yeccpars2_cont_101(S, '<integer_literal>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 131, [S | Ss], [T | Stack]);
yeccpars2_cont_101(S, '<string_literal>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 132, [S | Ss], [T | Stack]);
yeccpars2_cont_101(S, '<wcharacter_literal>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 133, [S | Ss], [T | Stack]);
yeccpars2_cont_101(S, '<wstring_literal>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 134, [S | Ss], [T | Stack]);
yeccpars2_cont_101(S, 'FALSE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 135, [S | Ss], [T | Stack]);
yeccpars2_cont_101(S, 'TRUE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 136, [S | Ss], [T | Stack]);
yeccpars2_cont_101(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_102(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 176, [S | Ss], [T | Stack]);
yeccpars2_102(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<floating_pt_type>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_104(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 171, [S | Ss], [T | Stack]);
yeccpars2_104(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<floating_pt_type>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<octet_type>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_107(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 165, [S | Ss], [T | Stack]);
yeccpars2_107(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_108(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 162, [S | Ss], [T | Stack]);
yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_108_(Stack),
 'yeccgoto_\'<string_type>\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_109(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 110, [S | Ss], [T | Stack]);
yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_109_(Stack),
 'yeccgoto_\'<string_type>\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_110: see yeccpars2_101

yeccpars2_111(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 158, [S | Ss], [T | Stack]);
yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<or_expr>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_112(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 41, [S | Ss], [T | Stack]);
yeccpars2_112(S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 42, [S | Ss], [T | Stack]);
yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_101(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<mult_expr>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_114(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 152, [S | Ss], [T | Stack]);
yeccpars2_114(S, '>>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 153, [S | Ss], [T | Stack]);
yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<and_expr>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_115(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 54, [S | Ss], [T | Stack]);
yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<primary_expr>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<unary_expr>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_117(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 160, [S | Ss], [T | Stack]);
yeccpars2_117(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_118(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 156, [S | Ss], [T | Stack]);
yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<const_exp>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_119(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 143, [S | Ss], [T | Stack]);
yeccpars2_119(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 144, [S | Ss], [T | Stack]);
yeccpars2_119(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 145, [S | Ss], [T | Stack]);
yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<add_expr>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<primary_expr>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<positive_int_const>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<literal>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_123(S, '&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 150, [S | Ss], [T | Stack]);
yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<xor_expr>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_124(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 140, [S | Ss], [T | Stack]);
yeccpars2_124(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 141, [S | Ss], [T | Stack]);
yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<shift_expr>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_125: see yeccpars2_101

yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<unary_operator>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<unary_operator>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_128(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<literal>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<literal>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<literal>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<literal>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<literal>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<literal>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_134(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<literal>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<boolean_literal>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<boolean_literal>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<unary_operator>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_138(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 139, [S | Ss], [T | Stack]);
yeccpars2_138(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_139_(Stack),
 'yeccgoto_\'<primary_expr>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_140: see yeccpars2_101

%% yeccpars2_141: see yeccpars2_101

yeccpars2_142(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 143, [S | Ss], [T | Stack]);
yeccpars2_142(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 144, [S | Ss], [T | Stack]);
yeccpars2_142(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 145, [S | Ss], [T | Stack]);
yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_142_(Stack),
 'yeccgoto_\'<add_expr>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_143: see yeccpars2_101

%% yeccpars2_144: see yeccpars2_101

%% yeccpars2_145: see yeccpars2_101

yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_146_(Stack),
 'yeccgoto_\'<mult_expr>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_147(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_147_(Stack),
 'yeccgoto_\'<mult_expr>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_148(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_148_(Stack),
 'yeccgoto_\'<mult_expr>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_149(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 143, [S | Ss], [T | Stack]);
yeccpars2_149(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 144, [S | Ss], [T | Stack]);
yeccpars2_149(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 145, [S | Ss], [T | Stack]);
yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_149_(Stack),
 'yeccgoto_\'<add_expr>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_150: see yeccpars2_101

yeccpars2_151(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 152, [S | Ss], [T | Stack]);
yeccpars2_151(S, '>>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 153, [S | Ss], [T | Stack]);
yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_151_(Stack),
 'yeccgoto_\'<and_expr>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_152: see yeccpars2_101

%% yeccpars2_153: see yeccpars2_101

yeccpars2_154(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 140, [S | Ss], [T | Stack]);
yeccpars2_154(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 141, [S | Ss], [T | Stack]);
yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_154_(Stack),
 'yeccgoto_\'<shift_expr>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_155(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 140, [S | Ss], [T | Stack]);
yeccpars2_155(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 141, [S | Ss], [T | Stack]);
yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_155_(Stack),
 'yeccgoto_\'<shift_expr>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_156: see yeccpars2_101

yeccpars2_157(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 158, [S | Ss], [T | Stack]);
yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_157_(Stack),
 'yeccgoto_\'<or_expr>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_158: see yeccpars2_101

yeccpars2_159(S, '&', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 150, [S | Ss], [T | Stack]);
yeccpars2_159(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_159_(Stack),
 'yeccgoto_\'<xor_expr>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_160_(Stack),
 'yeccgoto_\'<string_type>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_161(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_161_(Stack),
 'yeccgoto_\'<unary_expr>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_162: see yeccpars2_101

yeccpars2_163(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 164, [S | Ss], [T | Stack]);
yeccpars2_163(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_164(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_164_(Stack),
 'yeccgoto_\'<string_type>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_165(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 41, [S | Ss], [T | Stack]);
yeccpars2_165(S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 42, [S | Ss], [T | Stack]);
yeccpars2_165(S, 'Object', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 99, [S | Ss], [T | Stack]);
yeccpars2_165(S, any, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 100, [S | Ss], [T | Stack]);
yeccpars2_165(S, double, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 103, [S | Ss], [T | Stack]);
yeccpars2_165(S, fixed, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 104, [S | Ss], [T | Stack]);
yeccpars2_165(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 105, [S | Ss], [T | Stack]);
yeccpars2_165(S, octet, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 106, [S | Ss], [T | Stack]);
yeccpars2_165(S, sequence, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 107, [S | Ss], [T | Stack]);
yeccpars2_165(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 108, [S | Ss], [T | Stack]);
yeccpars2_165(S, wstring, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 109, [S | Ss], [T | Stack]);
yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_18(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_166(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 167, [S | Ss], [T | Stack]);
yeccpars2_166(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 168, [S | Ss], [T | Stack]);
yeccpars2_166(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_167: see yeccpars2_101

yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_168_(Stack),
 'yeccgoto_\'<sequence_type>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_169(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 170, [S | Ss], [T | Stack]);
yeccpars2_169(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_170_(Stack),
 'yeccgoto_\'<sequence_type>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_171: see yeccpars2_101

yeccpars2_172(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 173, [S | Ss], [T | Stack]);
yeccpars2_172(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_173: see yeccpars2_101

yeccpars2_174(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 175, [S | Ss], [T | Stack]);
yeccpars2_174(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_175(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_175_(Stack),
 'yeccgoto_\'<fixed_pt_type>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_176_(Stack),
 'yeccgoto_\'<case_label>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_177(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 178, [S | Ss], [T | Stack]);
yeccpars2_177(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_178_(Stack),
 'yeccgoto_\'<case_label>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_179(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 67, [S | Ss], [T | Stack]);
yeccpars2_179(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_179_(Stack),
 'yeccgoto_\'OorM_<case_label>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_180(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 67, [S | Ss], [T | Stack]);
yeccpars2_180(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 181, [S | Ss], [T | Stack]);
yeccpars2_180(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_181(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_181_(Stack),
 yeccpars2_182(182, Cat, [181 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_182(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 67, [S | Ss], [T | Stack]);
yeccpars2_182(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_182_(Stack),
 'yeccgoto_\'<case>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<declarator>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_184_(Stack),
 'yeccgoto_\'<element_spec>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_185(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<declarator>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_186(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<complex_declarator>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_187(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 190, [S | Ss], [T | Stack]);
yeccpars2_187(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<simple_declarator>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_188(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 190, [S | Ss], [T | Stack]);
yeccpars2_188(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_188_(Stack),
 'yeccgoto_\'<array_declarator>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_189(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_189_(Stack),
 'yeccgoto_\'OorM_<fixed_array_size>\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_190: see yeccpars2_101

yeccpars2_191(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 192, [S | Ss], [T | Stack]);
yeccpars2_191(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_192_(Stack),
 'yeccgoto_\'<fixed_array_size>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_193(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_193_(Stack),
 'yeccgoto_\'OorM_<fixed_array_size>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_194(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_194_(Stack),
 yeccpars2_195(195, Cat, [194 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_195(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 67, [S | Ss], [T | Stack]);
yeccpars2_195(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_(Stack),
 'yeccgoto_\'OorM_<case_label>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_196: see yeccpars2_80

yeccpars2_197(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_197_(Stack),
 'yeccgoto_\'<type_dcl>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_198_(Stack),
 'yeccgoto_\'<type_declarator>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_199_(Stack),
 yeccpars2_200(200, Cat, [199 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_200(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 201, [S | Ss], [T | Stack]);
yeccpars2_200(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_200_(Stack),
 'yeccgoto_\'<declarators>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_201: see yeccpars2_80

yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_202_(Stack),
 'yeccgoto_\'ZorM_<declarator>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_203(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 204, [S | Ss], [T | Stack]);
yeccpars2_203(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_204(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_204_(Stack),
 yeccpars2_205(205, Cat, [204 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_205(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 67, [S | Ss], [T | Stack]);
yeccpars2_205(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 41, [S | Ss], [T | Stack]);
yeccpars2_205(S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 42, [S | Ss], [T | Stack]);
yeccpars2_205(S, 'Object', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 99, [S | Ss], [T | Stack]);
yeccpars2_205(S, any, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 100, [S | Ss], [T | Stack]);
yeccpars2_205(S, double, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 103, [S | Ss], [T | Stack]);
yeccpars2_205(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 19, [S | Ss], [T | Stack]);
yeccpars2_205(S, fixed, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 104, [S | Ss], [T | Stack]);
yeccpars2_205(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 105, [S | Ss], [T | Stack]);
yeccpars2_205(S, octet, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 106, [S | Ss], [T | Stack]);
yeccpars2_205(S, sequence, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 107, [S | Ss], [T | Stack]);
yeccpars2_205(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 108, [S | Ss], [T | Stack]);
yeccpars2_205(S, struct, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 23, [S | Ss], [T | Stack]);
yeccpars2_205(S, union, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 25, [S | Ss], [T | Stack]);
yeccpars2_205(S, wstring, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 109, [S | Ss], [T | Stack]);
yeccpars2_205(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_18(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_206(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_206_#'(Stack),
 yeccpars2_205(205, '#', [206 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_206(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_206_::'(Stack),
 yeccpars2_205(205, '::', [206 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_206(_S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_206_<identifier>'(Stack),
 yeccpars2_205(205, '<identifier>', [206 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_206(_S, 'Object', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_Object(Stack),
 yeccpars2_205(205, 'Object', [206 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_206(_S, any, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_any(Stack),
 yeccpars2_205(205, any, [206 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_206(_S, boolean, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_boolean(Stack),
 yeccpars2_205(205, boolean, [206 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_206(_S, char, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_char(Stack),
 yeccpars2_205(205, char, [206 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_206(_S, double, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_double(Stack),
 yeccpars2_205(205, double, [206 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_206(_S, enum, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_enum(Stack),
 yeccpars2_205(205, enum, [206 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_206(_S, fixed, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_fixed(Stack),
 yeccpars2_205(205, fixed, [206 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_206(_S, float, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_float(Stack),
 yeccpars2_205(205, float, [206 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_206(_S, long, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_long(Stack),
 yeccpars2_205(205, long, [206 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_206(_S, octet, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_octet(Stack),
 yeccpars2_205(205, octet, [206 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_206(_S, sequence, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_sequence(Stack),
 yeccpars2_205(205, sequence, [206 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_206(_S, short, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_short(Stack),
 yeccpars2_205(205, short, [206 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_206(_S, string, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_string(Stack),
 yeccpars2_205(205, string, [206 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_206(_S, struct, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_struct(Stack),
 yeccpars2_205(205, struct, [206 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_206(_S, union, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_union(Stack),
 yeccpars2_205(205, union, [206 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_206(_S, unsigned, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_unsigned(Stack),
 yeccpars2_205(205, unsigned, [206 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_206(_S, wchar, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_wchar(Stack),
 yeccpars2_205(205, wchar, [206 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_206(_S, wstring, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_wstring(Stack),
 yeccpars2_205(205, wstring, [206 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_(Stack),
 'yeccgoto_\'<member_list>\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_207(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 209, [S | Ss], [T | Stack]);
yeccpars2_207(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_208(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'OorM_<member>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_209(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_209_(Stack),
 'yeccgoto_\'<struct_type>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_210(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_210_(Stack),
 'yeccgoto_\'OorM_<member>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_211: see yeccpars2_80

yeccpars2_212(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_212_(Stack),
 yeccpars2_213(213, Cat, [212 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_213(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 67, [S | Ss], [T | Stack]);
yeccpars2_213(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 214, [S | Ss], [T | Stack]);
yeccpars2_213(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_214(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_214_(Stack),
 yeccpars2_215(215, Cat, [214 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_215(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 67, [S | Ss], [T | Stack]);
yeccpars2_215(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_215_(Stack),
 'yeccgoto_\'<member>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_216(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 217, [S | Ss], [T | Stack]);
yeccpars2_216(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_217: see yeccpars2_0

yeccpars2_218(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 19, [S | Ss], [T | Stack]);
yeccpars2_218(S, struct, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 23, [S | Ss], [T | Stack]);
yeccpars2_218(S, union, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 25, [S | Ss], [T | Stack]);
yeccpars2_218(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 220, [S | Ss], [T | Stack]);
yeccpars2_218(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_0(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_219_(Stack),
 'yeccgoto_\'OorM_<definition>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_220(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_220_(Stack),
 'yeccgoto_\'<module>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_221(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 224, [S | Ss], [T | Stack]);
yeccpars2_221(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = 'yeccpars2_221_;'(Stack),
 'yeccgoto_\'<forward_dcl>\''(hd(Nss), ';', Nss, NewStack, T, Ts, Tzr);
yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_221_(Stack),
 yeccpars2_222(_S, Cat, [221 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_222_(Stack),
 'yeccgoto_\'<interface_header>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_223(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Opt_<inheritance_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_224(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 41, [S | Ss], [T | Stack]);
yeccpars2_224(S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 42, [S | Ss], [T | Stack]);
yeccpars2_224(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_225(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 54, [S | Ss], [T | Stack]);
yeccpars2_225(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_225_(Stack),
 yeccpars2_226(226, Cat, [225 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_226(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 227, [S | Ss], [T | Stack]);
yeccpars2_226(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_226_(Stack),
 'yeccgoto_\'<inheritance_spec>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_227: see yeccpars2_224

yeccpars2_228(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 54, [S | Ss], [T | Stack]);
yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_228_(Stack),
 'yeccgoto_\'ZorM_<scoped_name>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_229(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 230, [S | Ss], [T | Stack]);
yeccpars2_229(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_230(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_230_(Stack),
 yeccpars2_232(232, Cat, [230 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_231(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 234, [S | Ss], [T | Stack]);
yeccpars2_231(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_231_(Stack),
 yeccpars2_205(205, Cat, [231 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_232(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 67, [S | Ss], [T | Stack]);
yeccpars2_232(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ZorM_<member>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_233(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_233_(Stack),
 'yeccgoto_\'ZorM_<member>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_234(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_234_(Stack),
 'yeccgoto_\'<except_dcl>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_235(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 236, [S | Ss], [T | Stack]);
yeccpars2_235(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_236(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_236_(Stack),
 yeccpars2_237(237, Cat, [236 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_237(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 67, [S | Ss], [T | Stack]);
yeccpars2_237(S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 239, [S | Ss], [T | Stack]);
yeccpars2_237(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_238(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_238_(Stack),
 yeccpars2_240(240, Cat, [238 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_239(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_239_(Stack),
 'yeccgoto_\'<enumerator>\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_240(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 67, [S | Ss], [T | Stack]);
yeccpars2_240(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_240_(Stack),
 yeccpars2_241(_S, Cat, [240 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_241(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_241_(Stack),
 yeccpars2_242(242, Cat, [241 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_242(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 67, [S | Ss], [T | Stack]);
yeccpars2_242(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 243, [S | Ss], [T | Stack]);
yeccpars2_242(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 244, [S | Ss], [T | Stack]);
yeccpars2_242(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_243(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_243_(Stack),
 yeccpars2_237(245, Cat, [243 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_244(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_244_(Stack),
 'yeccgoto_\'<enum_type>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_245: see yeccpars2_237

yeccpars2_246(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_246_(Stack),
 'yeccgoto_\'ZorM_<enumerator>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_247(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<const_type>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_248(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 54, [S | Ss], [T | Stack]);
yeccpars2_248(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<const_type>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_249(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<const_type>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_250(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<const_type>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_251(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<const_type>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_252(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<const_type>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_253(S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 257, [S | Ss], [T | Stack]);
yeccpars2_253(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_254(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<const_type>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_255(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<const_type>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_256(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<fixed_pt_const_type>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_257(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 258, [S | Ss], [T | Stack]);
yeccpars2_257(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_258: see yeccpars2_101

yeccpars2_259(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_259_(Stack),
 'yeccgoto_\'<const_dcl>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_260(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_260_(Stack),
 'yeccgoto_\'OE_preproc\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_261(S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 69, [S | Ss], [T | Stack]);
yeccpars2_261(S, '<string_literal>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 262, [S | Ss], [T | Stack]);
yeccpars2_261(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_262(S, '<integer_literal>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 264, [S | Ss], [T | Stack]);
yeccpars2_262(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_262_(Stack),
 yeccpars2_263(263, Cat, [262 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_263(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 266, [S | Ss], [T | Stack]);
yeccpars2_263(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_264(S, '<integer_literal>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 264, [S | Ss], [T | Stack]);
yeccpars2_264(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_264_(Stack),
 yeccpars2_265(_S, Cat, [264 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_265(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_265_(Stack),
 'yeccgoto_\'ZorM_<integer_literal>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_266(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_266_(Stack),
 'yeccgoto_\'OE_preproc\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_267(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_267_(Stack),
 'yeccgoto_\'<definition>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_268(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_268_(Stack),
 'yeccgoto_\'<definition>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_269(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_269_(Stack),
 'yeccgoto_\'<definition>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_270(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_270_(Stack),
 yeccpars2_271(271, Cat, [270 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_271(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 17, [S | Ss], [T | Stack]);
yeccpars2_271(S, const, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 18, [S | Ss], [T | Stack]);
yeccpars2_271(S, enum, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 19, [S | Ss], [T | Stack]);
yeccpars2_271(S, exception, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 20, [S | Ss], [T | Stack]);
yeccpars2_271(S, oneway, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 285, [S | Ss], [T | Stack]);
yeccpars2_271(S, readonly, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 286, [S | Ss], [T | Stack]);
yeccpars2_271(S, struct, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 23, [S | Ss], [T | Stack]);
yeccpars2_271(S, typedef, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 24, [S | Ss], [T | Stack]);
yeccpars2_271(S, union, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 25, [S | Ss], [T | Stack]);
yeccpars2_271(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_271_::'(Stack),
 yeccpars2_275(275, '::', [271 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_271(_S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_271_<identifier>'(Stack),
 yeccpars2_275(275, '<identifier>', [271 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_271(_S, 'Object', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_271_Object(Stack),
 yeccpars2_275(275, 'Object', [271 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_271(_S, any, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_271_any(Stack),
 yeccpars2_275(275, any, [271 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_271(_S, boolean, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_271_boolean(Stack),
 yeccpars2_275(275, boolean, [271 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_271(_S, char, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_271_char(Stack),
 yeccpars2_275(275, char, [271 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_271(_S, double, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_271_double(Stack),
 yeccpars2_275(275, double, [271 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_271(_S, float, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_271_float(Stack),
 yeccpars2_275(275, float, [271 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_271(_S, long, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_271_long(Stack),
 yeccpars2_275(275, long, [271 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_271(_S, octet, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_271_octet(Stack),
 yeccpars2_275(275, octet, [271 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_271(_S, short, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_271_short(Stack),
 yeccpars2_275(275, short, [271 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_271(_S, string, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_271_string(Stack),
 yeccpars2_275(275, string, [271 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_271(_S, unsigned, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_271_unsigned(Stack),
 yeccpars2_275(275, unsigned, [271 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_271(_S, void, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_271_void(Stack),
 yeccpars2_275(275, void, [271 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_271(_S, wchar, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_271_wchar(Stack),
 yeccpars2_275(275, wchar, [271 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_271(_S, wstring, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_271_wstring(Stack),
 yeccpars2_275(275, wstring, [271 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_271(_S, attribute, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_271_attribute(Stack),
 yeccpars2_274(274, attribute, [271 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_271(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<interface_body>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_272(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 273, [S | Ss], [T | Stack]);
yeccpars2_272(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_273(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_273_(Stack),
 'yeccgoto_\'<interface_dcl>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_274(S, attribute, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 335, [S | Ss], [T | Stack]);
yeccpars2_274(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_275(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 41, [S | Ss], [T | Stack]);
yeccpars2_275(S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 42, [S | Ss], [T | Stack]);
yeccpars2_275(S, 'Object', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 99, [S | Ss], [T | Stack]);
yeccpars2_275(S, any, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 100, [S | Ss], [T | Stack]);
yeccpars2_275(S, double, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 103, [S | Ss], [T | Stack]);
yeccpars2_275(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 105, [S | Ss], [T | Stack]);
yeccpars2_275(S, octet, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 106, [S | Ss], [T | Stack]);
yeccpars2_275(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 108, [S | Ss], [T | Stack]);
yeccpars2_275(S, void, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 297, [S | Ss], [T | Stack]);
yeccpars2_275(S, wstring, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 109, [S | Ss], [T | Stack]);
yeccpars2_275(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_18(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_276(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<export>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_277(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<export>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_278(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 291, [S | Ss], [T | Stack]);
yeccpars2_278(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_279(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 290, [S | Ss], [T | Stack]);
yeccpars2_279(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_280(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Opt_<op_attribute>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_281(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_281_(Stack),
 'yeccgoto_\'ZorM_<export>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_282(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 289, [S | Ss], [T | Stack]);
yeccpars2_282(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_283(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 288, [S | Ss], [T | Stack]);
yeccpars2_283(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_284(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 287, [S | Ss], [T | Stack]);
yeccpars2_284(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_285(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<op_attribute>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_286(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Opt_readonly\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_287(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_287_(Stack),
 'yeccgoto_\'<export>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_288(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_288_(Stack),
 'yeccgoto_\'<export>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_289(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_289_(Stack),
 'yeccgoto_\'<export>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_290(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_290_(Stack),
 'yeccgoto_\'<export>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_291(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_291_(Stack),
 'yeccgoto_\'<export>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_292(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<param_type_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_293(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 54, [S | Ss], [T | Stack]);
yeccpars2_293(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<param_type_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_294(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<op_type_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_295(S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 298, [S | Ss], [T | Stack]);
yeccpars2_295(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_296(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<param_type_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_297(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<op_type_spec>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_298(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 300, [S | Ss], [T | Stack]);
yeccpars2_298(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_299(S, raises, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 321, [S | Ss], [T | Stack]);
yeccpars2_299(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_299_(Stack),
 yeccpars2_319(319, Cat, [299 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_300(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_300_(Stack),
 yeccpars2_301(301, Cat, [300 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_301(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 67, [S | Ss], [T | Stack]);
yeccpars2_301(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 304, [S | Ss], [T | Stack]);
yeccpars2_301(S, in, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 305, [S | Ss], [T | Stack]);
yeccpars2_301(S, inout, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 306, [S | Ss], [T | Stack]);
yeccpars2_301(S, out, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 307, [S | Ss], [T | Stack]);
yeccpars2_301(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_302(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_302_(Stack),
 yeccpars2_312(312, Cat, [302 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_303(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 41, [S | Ss], [T | Stack]);
yeccpars2_303(S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 42, [S | Ss], [T | Stack]);
yeccpars2_303(S, 'Object', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 99, [S | Ss], [T | Stack]);
yeccpars2_303(S, any, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 100, [S | Ss], [T | Stack]);
yeccpars2_303(S, double, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 103, [S | Ss], [T | Stack]);
yeccpars2_303(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 105, [S | Ss], [T | Stack]);
yeccpars2_303(S, octet, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 106, [S | Ss], [T | Stack]);
yeccpars2_303(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 108, [S | Ss], [T | Stack]);
yeccpars2_303(S, wstring, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 109, [S | Ss], [T | Stack]);
yeccpars2_303(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_18(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_304(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_304_(Stack),
 'yeccgoto_\'<parameter_dcls>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_305(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<param_attribute>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_306(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<param_attribute>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_307(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<param_attribute>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_308(S, '<identifier>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 310, [S | Ss], [T | Stack]);
yeccpars2_308(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_309(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_309_(Stack),
 'yeccgoto_\'<param_dcl>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_310(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'<simple_declarator>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_311(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 314, [S | Ss], [T | Stack]);
yeccpars2_311(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_(Stack),
 yeccpars2_313(313, Cat, [311 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_312(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 67, [S | Ss], [T | Stack]);
yeccpars2_312(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ZorM_<param_dcl>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_313(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 67, [S | Ss], [T | Stack]);
yeccpars2_313(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 315, [S | Ss], [T | Stack]);
yeccpars2_313(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_314(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_314_(Stack),
 'yeccgoto_\'<parameter_dcls>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_315(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_315_(Stack),
 yeccpars2_316(316, Cat, [315 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_316(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 67, [S | Ss], [T | Stack]);
yeccpars2_316(S, in, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 305, [S | Ss], [T | Stack]);
yeccpars2_316(S, inout, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 306, [S | Ss], [T | Stack]);
yeccpars2_316(S, out, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 307, [S | Ss], [T | Stack]);
yeccpars2_316(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_317(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_317_(Stack),
 yeccpars2_318(318, Cat, [317 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_318(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 67, [S | Ss], [T | Stack]);
yeccpars2_318(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_318_(Stack),
 'yeccgoto_\'ZorM_<param_dcl>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_319(S, context, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 328, [S | Ss], [T | Stack]);
yeccpars2_319(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_319_(Stack),
 yeccpars2_326(_S, Cat, [319 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_320(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Opt_<raises_expr>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_321(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 322, [S | Ss], [T | Stack]);
yeccpars2_321(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_322: see yeccpars2_224

yeccpars2_323(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 54, [S | Ss], [T | Stack]);
yeccpars2_323(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_323_(Stack),
 yeccpars2_324(324, Cat, [323 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_324(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 325, [S | Ss], [T | Stack]);
yeccpars2_324(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 227, [S | Ss], [T | Stack]);
yeccpars2_324(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_325(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_325_(Stack),
 'yeccgoto_\'<raises_expr>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_326(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_326_(Stack),
 'yeccgoto_\'<op_dcl>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_327(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Opt_<context_expr>\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_328(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 329, [S | Ss], [T | Stack]);
yeccpars2_328(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_329(S, '<string_literal>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 330, [S | Ss], [T | Stack]);
yeccpars2_329(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_330(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_330_(Stack),
 yeccpars2_331(331, Cat, [330 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_331(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 332, [S | Ss], [T | Stack]);
yeccpars2_331(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 333, [S | Ss], [T | Stack]);
yeccpars2_331(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_332(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_332_(Stack),
 'yeccgoto_\'<context_expr>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_333(S, '<string_literal>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 334, [S | Ss], [T | Stack]);
yeccpars2_333(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_334(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_334_(Stack),
 'yeccgoto_\'ZorM_<string_literal>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_335: see yeccpars2_303

%% yeccpars2_336: see yeccpars2_308

yeccpars2_337(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_337_(Stack),
 yeccpars2_338(338, Cat, [337 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_338(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(Ts, Tzr, 339, [S | Ss], [T | Stack]);
yeccpars2_338(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_338_(Stack),
 'yeccgoto_\'<attr_dcl>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_339: see yeccpars2_308

yeccpars2_340(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_340_(Stack),
 'yeccgoto_\'ZorM_<simple_declarator>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_341_(Stack),
 'yeccgoto_\'<definition>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_342(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_342_(Stack),
 'yeccgoto_\'<definition>\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

'yeccgoto_\'<add_expr>\''(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(124, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<add_expr>\''(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(124, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<add_expr>\''(125, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(124, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<add_expr>\''(150, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(124, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<add_expr>\''(152, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(155, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<add_expr>\''(153, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(154, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<add_expr>\''(156, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(124, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<add_expr>\''(158, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(124, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<add_expr>\''(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(124, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<add_expr>\''(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(124, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<add_expr>\''(171, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(124, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<add_expr>\''(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(124, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<add_expr>\''(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(124, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<add_expr>\''(258, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(124, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<add_expr>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<add_expr>', State, missing_in_goto_table}}).

'yeccgoto_\'<and_expr>\''(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(123, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<and_expr>\''(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(123, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<and_expr>\''(125, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(123, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<and_expr>\''(156, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(123, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<and_expr>\''(158, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(159, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<and_expr>\''(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(123, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<and_expr>\''(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(123, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<and_expr>\''(171, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(123, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<and_expr>\''(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(123, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<and_expr>\''(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(123, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<and_expr>\''(258, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(123, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<and_expr>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<and_expr>', State, missing_in_goto_table}}).

'yeccgoto_\'<any_type>\''(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<any_type>\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<any_type>\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<any_type>\''(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<any_type>\''(275=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<any_type>\''(303=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<any_type>\''(335=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<any_type>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<any_type>', State, missing_in_goto_table}}).

'yeccgoto_\'<array_declarator>\''(80=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_186(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<array_declarator>\''(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_186(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<array_declarator>\''(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_186(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<array_declarator>\''(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_186(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<array_declarator>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<array_declarator>', State, missing_in_goto_table}}).

'yeccgoto_\'<attr_dcl>\''(271, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_284(284, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<attr_dcl>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<attr_dcl>', State, missing_in_goto_table}}).

'yeccgoto_\'<base_type_spec>\''(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<base_type_spec>\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<base_type_spec>\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<base_type_spec>\''(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<base_type_spec>\''(275=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_296(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<base_type_spec>\''(303=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_296(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<base_type_spec>\''(335=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_296(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<base_type_spec>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<base_type_spec>', State, missing_in_goto_table}}).

'yeccgoto_\'<boolean_literal>\''(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_literal>\''(110=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_literal>\''(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_literal>\''(125=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_literal>\''(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_literal>\''(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_literal>\''(143=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_literal>\''(144=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_literal>\''(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_literal>\''(150=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_literal>\''(152=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_literal>\''(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_literal>\''(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_literal>\''(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_literal>\''(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_literal>\''(167=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_literal>\''(171=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_literal>\''(173=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_literal>\''(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_literal>\''(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_literal>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<boolean_literal>', State, missing_in_goto_table}}).

'yeccgoto_\'<boolean_type>\''(18=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_255(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_type>\''(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_type>\''(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_type>\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_type>\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_type>\''(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_type>\''(275=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_type>\''(303=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_type>\''(335=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<boolean_type>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<boolean_type>', State, missing_in_goto_table}}).

'yeccgoto_\'<case>\''(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<case>\''(59=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<case>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<case>', State, missing_in_goto_table}}).

'yeccgoto_\'<case_label>\''(64=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_194(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<case_label>\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<case_label>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<case_label>', State, missing_in_goto_table}}).

'yeccgoto_\'<char_type>\''(18=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<char_type>\''(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<char_type>\''(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<char_type>\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<char_type>\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<char_type>\''(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<char_type>\''(275=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<char_type>\''(303=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<char_type>\''(335=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<char_type>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<char_type>', State, missing_in_goto_table}}).

'yeccgoto_\'<complex_declarator>\''(80=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_185(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<complex_declarator>\''(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_185(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<complex_declarator>\''(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_185(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<complex_declarator>\''(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_185(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<complex_declarator>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<complex_declarator>', State, missing_in_goto_table}}).

'yeccgoto_\'<const_dcl>\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(16, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<const_dcl>\''(1, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(16, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<const_dcl>\''(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(16, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<const_dcl>\''(218, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(16, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<const_dcl>\''(271, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_283(283, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<const_dcl>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<const_dcl>', State, missing_in_goto_table}}).

'yeccgoto_\'<const_exp>\''(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(177, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<const_exp>\''(110=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<const_exp>\''(125, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(138, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<const_exp>\''(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<const_exp>\''(167=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<const_exp>\''(171=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<const_exp>\''(173=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<const_exp>\''(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<const_exp>\''(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_259(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<const_exp>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<const_exp>', State, missing_in_goto_table}}).

'yeccgoto_\'<const_type>\''(18, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_253(253, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<const_type>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<const_type>', State, missing_in_goto_table}}).

'yeccgoto_\'<constr_type_spec>\''(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<constr_type_spec>\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<constr_type_spec>\''(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<constr_type_spec>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<constr_type_spec>', State, missing_in_goto_table}}).

'yeccgoto_\'<context_expr>\''(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_327(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<context_expr>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<context_expr>', State, missing_in_goto_table}}).

'yeccgoto_\'<declarator>\''(80=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<declarator>\''(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<declarator>\''(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<declarator>\''(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<declarator>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<declarator>', State, missing_in_goto_table}}).

'yeccgoto_\'<declarators>\''(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<declarators>\''(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<declarators>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<declarators>', State, missing_in_goto_table}}).

'yeccgoto_\'<definition>\''(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<definition>\''(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<definition>\''(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<definition>\''(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<definition>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<definition>', State, missing_in_goto_table}}).

'yeccgoto_\'<element_spec>\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<element_spec>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<element_spec>', State, missing_in_goto_table}}).

'yeccgoto_\'<enum_type>\''(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<enum_type>\''(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<enum_type>\''(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<enum_type>\''(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<enum_type>\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<enum_type>\''(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<enum_type>\''(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<enum_type>\''(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<enum_type>\''(271=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<enum_type>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<enum_type>', State, missing_in_goto_table}}).

'yeccgoto_\'<enumerator>\''(237=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_238(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<enumerator>\''(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_246(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<enumerator>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<enumerator>', State, missing_in_goto_table}}).

'yeccgoto_\'<except_dcl>\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(13, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<except_dcl>\''(1, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(13, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<except_dcl>\''(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(13, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<except_dcl>\''(218, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(13, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<except_dcl>\''(271, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_282(282, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<except_dcl>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<except_dcl>', State, missing_in_goto_table}}).

'yeccgoto_\'<export>\''(271=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_281(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<export>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<export>', State, missing_in_goto_table}}).

'yeccgoto_\'<fixed_array_size>\''(187=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<fixed_array_size>\''(188=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_193(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<fixed_array_size>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<fixed_array_size>', State, missing_in_goto_table}}).

'yeccgoto_\'<fixed_pt_const_type>\''(18=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_252(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<fixed_pt_const_type>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<fixed_pt_const_type>', State, missing_in_goto_table}}).

'yeccgoto_\'<fixed_pt_type>\''(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<fixed_pt_type>\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<fixed_pt_type>\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<fixed_pt_type>\''(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<fixed_pt_type>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<fixed_pt_type>', State, missing_in_goto_table}}).

'yeccgoto_\'<floating_pt_type>\''(18=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_251(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<floating_pt_type>\''(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<floating_pt_type>\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<floating_pt_type>\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<floating_pt_type>\''(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<floating_pt_type>\''(275=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<floating_pt_type>\''(303=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<floating_pt_type>\''(335=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<floating_pt_type>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<floating_pt_type>', State, missing_in_goto_table}}).

'yeccgoto_\'<forward_dcl>\''(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<forward_dcl>\''(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<forward_dcl>\''(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<forward_dcl>\''(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<forward_dcl>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<forward_dcl>', State, missing_in_goto_table}}).

'yeccgoto_\'<inheritance_spec>\''(221=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_223(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<inheritance_spec>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<inheritance_spec>', State, missing_in_goto_table}}).

'yeccgoto_\'<integer_type>\''(18=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_250(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<integer_type>\''(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<integer_type>\''(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<integer_type>\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<integer_type>\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<integer_type>\''(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<integer_type>\''(275=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<integer_type>\''(303=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<integer_type>\''(335=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<integer_type>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<integer_type>', State, missing_in_goto_table}}).

'yeccgoto_\'<interface>\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(11, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<interface>\''(1, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(11, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<interface>\''(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(11, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<interface>\''(218, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(11, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<interface>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<interface>', State, missing_in_goto_table}}).

'yeccgoto_\'<interface_body>\''(270, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_272(272, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<interface_body>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<interface_body>', State, missing_in_goto_table}}).

'yeccgoto_\'<interface_dcl>\''(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<interface_dcl>\''(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<interface_dcl>\''(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<interface_dcl>\''(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<interface_dcl>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<interface_dcl>', State, missing_in_goto_table}}).

'yeccgoto_\'<interface_header>\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<interface_header>\''(1, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<interface_header>\''(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<interface_header>\''(218, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<interface_header>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<interface_header>', State, missing_in_goto_table}}).

'yeccgoto_\'<literal>\''(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<literal>\''(110=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<literal>\''(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<literal>\''(125=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<literal>\''(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<literal>\''(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<literal>\''(143=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<literal>\''(144=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<literal>\''(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<literal>\''(150=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<literal>\''(152=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<literal>\''(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<literal>\''(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<literal>\''(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<literal>\''(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<literal>\''(167=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<literal>\''(171=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<literal>\''(173=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<literal>\''(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<literal>\''(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<literal>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<literal>', State, missing_in_goto_table}}).

'yeccgoto_\'<member>\''(204=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<member>\''(206=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_210(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<member>\''(231=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_233(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<member>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<member>', State, missing_in_goto_table}}).

'yeccgoto_\'<member_list>\''(204, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_207(207, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<member_list>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<member_list>', State, missing_in_goto_table}}).

'yeccgoto_\'<module>\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<module>\''(1, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<module>\''(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<module>\''(218, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<module>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<module>', State, missing_in_goto_table}}).

'yeccgoto_\'<mult_expr>\''(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(119, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<mult_expr>\''(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(119, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<mult_expr>\''(125, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(119, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<mult_expr>\''(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(149, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<mult_expr>\''(141, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(142, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<mult_expr>\''(150, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(119, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<mult_expr>\''(152, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(119, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<mult_expr>\''(153, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(119, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<mult_expr>\''(156, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(119, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<mult_expr>\''(158, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(119, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<mult_expr>\''(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(119, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<mult_expr>\''(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(119, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<mult_expr>\''(171, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(119, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<mult_expr>\''(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(119, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<mult_expr>\''(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(119, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<mult_expr>\''(258, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(119, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<mult_expr>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<mult_expr>', State, missing_in_goto_table}}).

'yeccgoto_\'<octet_type>\''(18=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_249(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<octet_type>\''(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<octet_type>\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<octet_type>\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<octet_type>\''(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<octet_type>\''(275=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<octet_type>\''(303=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<octet_type>\''(335=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<octet_type>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<octet_type>', State, missing_in_goto_table}}).

'yeccgoto_\'<op_attribute>\''(271=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_280(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<op_attribute>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<op_attribute>', State, missing_in_goto_table}}).

'yeccgoto_\'<op_dcl>\''(271, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_279(279, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<op_dcl>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<op_dcl>', State, missing_in_goto_table}}).

'yeccgoto_\'<op_type_spec>\''(275, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_295(295, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<op_type_spec>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<op_type_spec>', State, missing_in_goto_table}}).

'yeccgoto_\'<or_expr>\''(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<or_expr>\''(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<or_expr>\''(125, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<or_expr>\''(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<or_expr>\''(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<or_expr>\''(171, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<or_expr>\''(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<or_expr>\''(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<or_expr>\''(258, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<or_expr>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<or_expr>', State, missing_in_goto_table}}).

'yeccgoto_\'<param_attribute>\''(301, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_303(303, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<param_attribute>\''(316, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_303(303, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<param_attribute>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<param_attribute>', State, missing_in_goto_table}}).

'yeccgoto_\'<param_dcl>\''(301=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_302(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<param_dcl>\''(316=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_317(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<param_dcl>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<param_dcl>', State, missing_in_goto_table}}).

'yeccgoto_\'<param_type_spec>\''(275=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_294(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<param_type_spec>\''(303, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_308(308, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<param_type_spec>\''(335, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_308(336, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<param_type_spec>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<param_type_spec>', State, missing_in_goto_table}}).

'yeccgoto_\'<parameter_dcls>\''(298, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_299(299, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<parameter_dcls>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<parameter_dcls>', State, missing_in_goto_table}}).

'yeccgoto_\'<positive_int_const>\''(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(117, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<positive_int_const>\''(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(163, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<positive_int_const>\''(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(169, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<positive_int_const>\''(171, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(172, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<positive_int_const>\''(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(174, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<positive_int_const>\''(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_191(191, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<positive_int_const>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<positive_int_const>', State, missing_in_goto_table}}).

'yeccgoto_\'<primary_expr>\''(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<primary_expr>\''(110=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<primary_expr>\''(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<primary_expr>\''(125=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<primary_expr>\''(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<primary_expr>\''(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<primary_expr>\''(143=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<primary_expr>\''(144=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<primary_expr>\''(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<primary_expr>\''(150=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<primary_expr>\''(152=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<primary_expr>\''(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<primary_expr>\''(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<primary_expr>\''(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<primary_expr>\''(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<primary_expr>\''(167=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<primary_expr>\''(171=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<primary_expr>\''(173=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<primary_expr>\''(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<primary_expr>\''(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<primary_expr>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<primary_expr>', State, missing_in_goto_table}}).

'yeccgoto_\'<raises_expr>\''(299=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_320(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<raises_expr>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<raises_expr>', State, missing_in_goto_table}}).

'yeccgoto_\'<scoped_name>\''(18, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_248(248, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(24, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(86, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(28, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(36, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(78, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(86, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(112, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(125, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(141, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(143, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(144, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(150, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(152, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(153, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(156, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(158, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(165, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(86, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(171, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(205, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(86, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(224, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_225(225, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(227, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(228, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(258, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(275, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_293(293, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(303, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_293(293, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(322, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_323(323, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(335, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_293(293, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<scoped_name>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<scoped_name>', State, missing_in_goto_table}}).

'yeccgoto_\'<sequence_type>\''(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<sequence_type>\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<sequence_type>\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<sequence_type>\''(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<sequence_type>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<sequence_type>', State, missing_in_goto_table}}).

'yeccgoto_\'<shift_expr>\''(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(114, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<shift_expr>\''(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(114, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<shift_expr>\''(125, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(114, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<shift_expr>\''(150, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(151, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<shift_expr>\''(156, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(114, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<shift_expr>\''(158, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(114, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<shift_expr>\''(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(114, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<shift_expr>\''(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(114, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<shift_expr>\''(171, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(114, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<shift_expr>\''(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(114, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<shift_expr>\''(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(114, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<shift_expr>\''(258, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(114, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<shift_expr>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<shift_expr>', State, missing_in_goto_table}}).

'yeccgoto_\'<signed_int>\''(18=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<signed_int>\''(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<signed_int>\''(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<signed_int>\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<signed_int>\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<signed_int>\''(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<signed_int>\''(275=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<signed_int>\''(303=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<signed_int>\''(335=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<signed_int>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<signed_int>', State, missing_in_goto_table}}).

'yeccgoto_\'<signed_long_int>\''(18=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<signed_long_int>\''(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<signed_long_int>\''(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<signed_long_int>\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<signed_long_int>\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<signed_long_int>\''(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<signed_long_int>\''(275=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<signed_long_int>\''(303=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<signed_long_int>\''(335=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<signed_long_int>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<signed_long_int>', State, missing_in_goto_table}}).

'yeccgoto_\'<signed_short_int>\''(18=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<signed_short_int>\''(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<signed_short_int>\''(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<signed_short_int>\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<signed_short_int>\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<signed_short_int>\''(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<signed_short_int>\''(275=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<signed_short_int>\''(303=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<signed_short_int>\''(335=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<signed_short_int>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<signed_short_int>', State, missing_in_goto_table}}).

'yeccgoto_\'<simple_declarator>\''(80=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<simple_declarator>\''(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<simple_declarator>\''(201=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<simple_declarator>\''(211=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<simple_declarator>\''(308=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_309(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<simple_declarator>\''(336=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_337(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<simple_declarator>\''(339=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_340(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<simple_declarator>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<simple_declarator>', State, missing_in_goto_table}}).

'yeccgoto_\'<simple_type_spec>\''(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<simple_type_spec>\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<simple_type_spec>\''(165, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(166, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<simple_type_spec>\''(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<simple_type_spec>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<simple_type_spec>', State, missing_in_goto_table}}).

'yeccgoto_\'<specification>\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<specification>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<specification>', State, missing_in_goto_table}}).

'yeccgoto_\'<string_type>\''(18=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_247(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<string_type>\''(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<string_type>\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<string_type>\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<string_type>\''(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<string_type>\''(275=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_292(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<string_type>\''(303=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_292(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<string_type>\''(335=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_292(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<string_type>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<string_type>', State, missing_in_goto_table}}).

'yeccgoto_\'<struct_type>\''(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<struct_type>\''(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<struct_type>\''(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<struct_type>\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<struct_type>\''(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<struct_type>\''(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<struct_type>\''(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<struct_type>\''(271=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<struct_type>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<struct_type>', State, missing_in_goto_table}}).

'yeccgoto_\'<switch_body>\''(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(60, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<switch_body>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<switch_body>', State, missing_in_goto_table}}).

'yeccgoto_\'<switch_type_spec>\''(28, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<switch_type_spec>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<switch_type_spec>', State, missing_in_goto_table}}).

'yeccgoto_\'<template_type_spec>\''(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<template_type_spec>\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<template_type_spec>\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<template_type_spec>\''(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<template_type_spec>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<template_type_spec>', State, missing_in_goto_table}}).

'yeccgoto_\'<type_dcl>\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<type_dcl>\''(1, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<type_dcl>\''(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<type_dcl>\''(218, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<type_dcl>\''(271, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_278(278, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<type_dcl>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<type_dcl>', State, missing_in_goto_table}}).

'yeccgoto_\'<type_declarator>\''(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_197(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<type_declarator>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<type_declarator>', State, missing_in_goto_table}}).

'yeccgoto_\'<type_spec>\''(24, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(196, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<type_spec>\''(78, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(80, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<type_spec>\''(205, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(211, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<type_spec>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<type_spec>', State, missing_in_goto_table}}).

'yeccgoto_\'<unary_expr>\''(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_expr>\''(110=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_expr>\''(125=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_expr>\''(140=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_expr>\''(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_expr>\''(143=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_expr>\''(144=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_expr>\''(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_expr>\''(150=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_expr>\''(152=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_expr>\''(153=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_expr>\''(156=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_expr>\''(158=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_expr>\''(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_expr>\''(167=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_expr>\''(171=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_expr>\''(173=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_expr>\''(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_expr>\''(258=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_expr>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<unary_expr>', State, missing_in_goto_table}}).

'yeccgoto_\'<unary_operator>\''(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_operator>\''(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_operator>\''(125, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_operator>\''(140, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_operator>\''(141, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_operator>\''(143, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_operator>\''(144, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_operator>\''(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_operator>\''(150, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_operator>\''(152, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_operator>\''(153, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_operator>\''(156, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_operator>\''(158, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_operator>\''(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_operator>\''(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_operator>\''(171, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_operator>\''(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_operator>\''(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_operator>\''(258, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unary_operator>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<unary_operator>', State, missing_in_goto_table}}).

'yeccgoto_\'<union_type>\''(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<union_type>\''(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<union_type>\''(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<union_type>\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<union_type>\''(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<union_type>\''(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<union_type>\''(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<union_type>\''(271=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<union_type>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<union_type>', State, missing_in_goto_table}}).

'yeccgoto_\'<unsigned_int>\''(18=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unsigned_int>\''(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unsigned_int>\''(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unsigned_int>\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unsigned_int>\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unsigned_int>\''(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unsigned_int>\''(275=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unsigned_int>\''(303=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unsigned_int>\''(335=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unsigned_int>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<unsigned_int>', State, missing_in_goto_table}}).

'yeccgoto_\'<unsigned_long_int>\''(18=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unsigned_long_int>\''(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unsigned_long_int>\''(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unsigned_long_int>\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unsigned_long_int>\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unsigned_long_int>\''(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unsigned_long_int>\''(275=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unsigned_long_int>\''(303=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unsigned_long_int>\''(335=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unsigned_long_int>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<unsigned_long_int>', State, missing_in_goto_table}}).

'yeccgoto_\'<unsigned_short_int>\''(18=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unsigned_short_int>\''(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unsigned_short_int>\''(28=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unsigned_short_int>\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unsigned_short_int>\''(165=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unsigned_short_int>\''(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unsigned_short_int>\''(275=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unsigned_short_int>\''(303=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unsigned_short_int>\''(335=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<unsigned_short_int>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<unsigned_short_int>', State, missing_in_goto_table}}).

'yeccgoto_\'<xor_expr>\''(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<xor_expr>\''(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<xor_expr>\''(125, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<xor_expr>\''(156, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(157, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<xor_expr>\''(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<xor_expr>\''(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<xor_expr>\''(171, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<xor_expr>\''(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<xor_expr>\''(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<xor_expr>\''(258, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(111, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'<xor_expr>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'<xor_expr>', State, missing_in_goto_table}}).

'yeccgoto_\'OE_pragma\''(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_pragma\''(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_pragma\''(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_pragma\''(64=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_pragma\''(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_pragma\''(179=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_pragma\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_pragma\''(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_pragma\''(195=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_pragma\''(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_pragma\''(213=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_pragma\''(215=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_pragma\''(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_pragma\''(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_pragma\''(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_pragma\''(237=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_pragma\''(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_pragma\''(242=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_pragma\''(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_pragma\''(271=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_277(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_pragma\''(301=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_pragma\''(312=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_pragma\''(313=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_pragma\''(316=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_pragma\''(318=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_pragma\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'OE_pragma', State, missing_in_goto_table}}).

'yeccgoto_\'OE_preproc\''(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_preproc\''(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_preproc\''(217=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_preproc\''(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_preproc\''(271=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_276(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OE_preproc\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'OE_preproc', State, missing_in_goto_table}}).

'yeccgoto_\'OorM_<case>\''(57=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OorM_<case>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'OorM_<case>', State, missing_in_goto_table}}).

'yeccgoto_\'OorM_<case_label>\''(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OorM_<case_label>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'OorM_<case_label>', State, missing_in_goto_table}}).

'yeccgoto_\'OorM_<definition>\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OorM_<definition>\''(217, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_218(218, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OorM_<definition>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'OorM_<definition>', State, missing_in_goto_table}}).

'yeccgoto_\'OorM_<fixed_array_size>\''(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(188, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OorM_<fixed_array_size>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'OorM_<fixed_array_size>', State, missing_in_goto_table}}).

'yeccgoto_\'OorM_<member>\''(204=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'OorM_<member>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'OorM_<member>', State, missing_in_goto_table}}).

'yeccgoto_\'Opt_<context_expr>\''(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_326(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Opt_<context_expr>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'Opt_<context_expr>', State, missing_in_goto_table}}).

'yeccgoto_\'Opt_<inheritance_spec>\''(221=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Opt_<inheritance_spec>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'Opt_<inheritance_spec>', State, missing_in_goto_table}}).

'yeccgoto_\'Opt_<op_attribute>\''(271, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_275(275, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Opt_<op_attribute>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'Opt_<op_attribute>', State, missing_in_goto_table}}).

'yeccgoto_\'Opt_<raises_expr>\''(299, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_319(319, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Opt_<raises_expr>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'Opt_<raises_expr>', State, missing_in_goto_table}}).

'yeccgoto_\'Opt_readonly\''(271, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_274(274, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Opt_readonly\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'Opt_readonly', State, missing_in_goto_table}}).

'yeccgoto_\'Ugly_pragmas\''(57, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(58, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Ugly_pragmas\''(58, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(64, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Ugly_pragmas\''(59, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(58, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Ugly_pragmas\''(65, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Ugly_pragmas\''(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(180, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Ugly_pragmas\''(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(179, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Ugly_pragmas\''(181, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_182(182, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Ugly_pragmas\''(194, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_195(195, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Ugly_pragmas\''(204, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(205, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Ugly_pragmas\''(206, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(205, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Ugly_pragmas\''(212, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(213, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Ugly_pragmas\''(214, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_215(215, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Ugly_pragmas\''(230, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_232(232, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Ugly_pragmas\''(231, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(205, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Ugly_pragmas\''(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_237(237, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Ugly_pragmas\''(238, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_240(240, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Ugly_pragmas\''(241, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_242(242, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Ugly_pragmas\''(243, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_237(245, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Ugly_pragmas\''(300, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_301(301, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Ugly_pragmas\''(302, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_312(312, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Ugly_pragmas\''(311, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_313(313, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Ugly_pragmas\''(315, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_316(316, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Ugly_pragmas\''(317, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_318(318, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Ugly_pragmas\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'Ugly_pragmas', State, missing_in_goto_table}}).

'yeccgoto_\'ZorM_<declarator>\''(199, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(200, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ZorM_<declarator>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'ZorM_<declarator>', State, missing_in_goto_table}}).

'yeccgoto_\'ZorM_<enumerator>\''(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_241(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ZorM_<enumerator>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'ZorM_<enumerator>', State, missing_in_goto_table}}).

'yeccgoto_\'ZorM_<export>\''(270, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_271(271, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ZorM_<export>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'ZorM_<export>', State, missing_in_goto_table}}).

'yeccgoto_\'ZorM_<integer_literal>\''(262, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_263(263, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ZorM_<integer_literal>\''(264=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_265(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ZorM_<integer_literal>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'ZorM_<integer_literal>', State, missing_in_goto_table}}).

'yeccgoto_\'ZorM_<member>\''(230, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_231(231, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ZorM_<member>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'ZorM_<member>', State, missing_in_goto_table}}).

'yeccgoto_\'ZorM_<param_dcl>\''(302, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_311(311, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ZorM_<param_dcl>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'ZorM_<param_dcl>', State, missing_in_goto_table}}).

'yeccgoto_\'ZorM_<scoped_name>\''(225, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_226(226, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ZorM_<scoped_name>\''(323, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_324(324, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ZorM_<scoped_name>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'ZorM_<scoped_name>', State, missing_in_goto_table}}).

'yeccgoto_\'ZorM_<simple_declarator>\''(337, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_338(338, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ZorM_<simple_declarator>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'ZorM_<simple_declarator>', State, missing_in_goto_table}}).

'yeccgoto_\'ZorM_<string_literal>\''(330, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_331(331, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ZorM_<string_literal>\''(State, _Cat, _Ss, _Stack, _T, _Ts, _Tzr) ->
 erlang:error({yecc_bug,"1.3",{'ZorM_<string_literal>', State, missing_in_goto_table}}).

-compile({inline,{yeccpars2_1_,1}}).
-file("icparse.yrl", 287).
yeccpars2_1_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   reverse ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_15_,1}}).
-file("icparse.yrl", 291).
yeccpars2_15_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_31_,1}}).
-file("icparse.yrl", 542).
yeccpars2_31_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { unsigned , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_42_,1}}).
-file("icparse.yrl", 370).
yeccpars2_42_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ic_symtab : scoped_id_new ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_49_,1}}).
-file("icparse.yrl", 565).
yeccpars2_49_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_50_,1}}).
-file("icparse.yrl", 570).
yeccpars2_50_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_51_,1}}).
-file("icparse.yrl", 566).
yeccpars2_51_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { 'long long' , element ( 2 , __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_52_,1}}).
-file("icparse.yrl", 552).
yeccpars2_52_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { 'long long' , element ( 2 , __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_53_,1}}).
-file("icparse.yrl", 371).
yeccpars2_53_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   ic_symtab : scoped_id_new_global ( __2 )
  end | __Stack].

-compile({inline,{yeccpars2_55_,1}}).
-file("icparse.yrl", 373).
yeccpars2_55_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ic_symtab : scoped_id_add ( __1 , __3 )
  end | __Stack].

-compile({inline,{yeccpars2_57_,1}}).
-file("icparse.yrl", 258).
yeccpars2_57_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_58_,1}}).
-file("icparse.yrl", 258).
yeccpars2_58_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{'yeccpars2_59_#',1}}).
-file("icparse.yrl", 258).
'yeccpars2_59_#'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_59_case,1}}).
-file("icparse.yrl", 258).
yeccpars2_59_case(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_59_default,1}}).
-file("icparse.yrl", 258).
yeccpars2_59_default(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_59_,1}}).
-file("icparse.yrl", 640).
yeccpars2_59_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   reverse ( lists : flatten ( __1 ) )
  end | __Stack].

-compile({inline,{yeccpars2_61_,1}}).
-file("icparse.yrl", 646).
yeccpars2_61_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_62_,1}}).
-file("icparse.yrl", 628).
yeccpars2_62_(__Stack0) ->
 [__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # union { id = __2 , type = __5 , body = __8 }
  end | __Stack].

-compile({inline,{yeccpars2_63_,1}}).
-file("icparse.yrl", 647).
yeccpars2_63_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_65_,1}}).
-file("icparse.yrl", 258).
yeccpars2_65_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_66_,1}}).
-file("icparse.yrl", 259).
yeccpars2_66_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_73_,1}}).
-file("icparse.yrl", 239).
yeccpars2_73_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # pragma { type = __4 , to = followed , apply = __5 }
  end | __Stack].

-compile({inline,{yeccpars2_76_,1}}).
-file("icparse.yrl", 244).
yeccpars2_76_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # pragma { type = __4 , to = __5 , apply = __6 }
  end | __Stack].

-compile({inline,{yeccpars2_77_,1}}).
-file("icparse.yrl", 249).
yeccpars2_77_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # pragma { type = __4 , to = __5 , apply = ic_options : float_to_version ( __6 ) }
  end | __Stack].

-compile({inline,{yeccpars2_92_,1}}).
-file("icparse.yrl", 258).
yeccpars2_92_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_95_,1}}).
-file("icparse.yrl", 258).
yeccpars2_95_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_108_,1}}).
-file("icparse.yrl", 715).
yeccpars2_108_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # string { }
  end | __Stack].

-compile({inline,{yeccpars2_109_,1}}).
-file("icparse.yrl", 719).
yeccpars2_109_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # wstring { }
  end | __Stack].

-compile({inline,{yeccpars2_139_,1}}).
-file("icparse.yrl", 444).
yeccpars2_139_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_142_,1}}).
-file("icparse.yrl", 420).
yeccpars2_142_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { '-' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_146_,1}}).
-file("icparse.yrl", 426).
yeccpars2_146_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { '/' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_147_,1}}).
-file("icparse.yrl", 425).
yeccpars2_147_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { '*' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_148_,1}}).
-file("icparse.yrl", 427).
yeccpars2_148_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { '%' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_149_,1}}).
-file("icparse.yrl", 419).
yeccpars2_149_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { '+' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_151_,1}}).
-file("icparse.yrl", 408).
yeccpars2_151_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { 'and' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_154_,1}}).
-file("icparse.yrl", 413).
yeccpars2_154_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { rshift , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_155_,1}}).
-file("icparse.yrl", 414).
yeccpars2_155_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { lshift , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_157_,1}}).
-file("icparse.yrl", 398).
yeccpars2_157_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { 'or' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_159_,1}}).
-file("icparse.yrl", 403).
yeccpars2_159_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { 'xor' , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_160_,1}}).
-file("icparse.yrl", 718).
yeccpars2_160_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # wstring { length = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_161_,1}}).
-file("icparse.yrl", 431).
yeccpars2_161_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { __1 , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_164_,1}}).
-file("icparse.yrl", 714).
yeccpars2_164_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # string { length = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_168_,1}}).
-file("icparse.yrl", 709).
yeccpars2_168_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # sequence { type = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_170_,1}}).
-file("icparse.yrl", 707).
yeccpars2_170_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # sequence { type = __3 , length = __5 }
  end | __Stack].

-compile({inline,{yeccpars2_175_,1}}).
-file("icparse.yrl", 829).
yeccpars2_175_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # fixed { digits = __3 , scale = __5 }
  end | __Stack].

-compile({inline,{yeccpars2_176_,1}}).
-file("icparse.yrl", 673).
yeccpars2_176_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,{yeccpars2_178_,1}}).
-file("icparse.yrl", 672).
yeccpars2_178_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_179_,1}}).
-file("icparse.yrl", 668).
yeccpars2_179_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2 ++ [ __3 | __1 ] ++ __4
  end | __Stack].

-compile({inline,{yeccpars2_181_,1}}).
-file("icparse.yrl", 258).
yeccpars2_181_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_182_,1}}).
-file("icparse.yrl", 658).
yeccpars2_182_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ __3 ++ __5 ++ __7 ++ [ __4 # case_dcl { label = reverse ( __2 ) } ]
  end | __Stack].

-compile({inline,{yeccpars2_184_,1}}).
-file("icparse.yrl", 678).
yeccpars2_184_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   # case_dcl { type = __1 , id = __2 }
  end | __Stack].

-compile({inline,{yeccpars2_188_,1}}).
-file("icparse.yrl", 724).
yeccpars2_188_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   # array { id = __1 , size = reverse ( __2 ) }
  end | __Stack].

-compile({inline,{yeccpars2_189_,1}}).
-file("icparse.yrl", 728).
yeccpars2_189_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_192_,1}}).
-file("icparse.yrl", 734).
yeccpars2_192_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_193_,1}}).
-file("icparse.yrl", 730).
yeccpars2_193_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_194_,1}}).
-file("icparse.yrl", 258).
yeccpars2_194_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_195_,1}}).
-file("icparse.yrl", 666).
yeccpars2_195_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ] ++ __3
  end | __Stack].

-compile({inline,{yeccpars2_197_,1}}).
-file("icparse.yrl", 468).
yeccpars2_197_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_198_,1}}).
-file("icparse.yrl", 475).
yeccpars2_198_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   # typedef { type = __1 , id = __2 }
  end | __Stack].

-compile({inline,{yeccpars2_199_,1}}).
-file("icparse.yrl", 517).
yeccpars2_199_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_200_,1}}).
-file("icparse.yrl", 514).
yeccpars2_200_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | reverse ( __2 ) ]
  end | __Stack].

-compile({inline,{yeccpars2_202_,1}}).
-file("icparse.yrl", 519).
yeccpars2_202_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_204_,1}}).
-file("icparse.yrl", 258).
yeccpars2_204_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{'yeccpars2_206_#',1}}).
-file("icparse.yrl", 258).
'yeccpars2_206_#'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{'yeccpars2_206_::',1}}).
-file("icparse.yrl", 258).
'yeccpars2_206_::'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{'yeccpars2_206_<identifier>',1}}).
-file("icparse.yrl", 258).
'yeccpars2_206_<identifier>'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_206_Object,1}}).
-file("icparse.yrl", 258).
yeccpars2_206_Object(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_206_any,1}}).
-file("icparse.yrl", 258).
yeccpars2_206_any(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_206_boolean,1}}).
-file("icparse.yrl", 258).
yeccpars2_206_boolean(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_206_char,1}}).
-file("icparse.yrl", 258).
yeccpars2_206_char(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_206_double,1}}).
-file("icparse.yrl", 258).
yeccpars2_206_double(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_206_enum,1}}).
-file("icparse.yrl", 258).
yeccpars2_206_enum(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_206_fixed,1}}).
-file("icparse.yrl", 258).
yeccpars2_206_fixed(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_206_float,1}}).
-file("icparse.yrl", 258).
yeccpars2_206_float(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_206_long,1}}).
-file("icparse.yrl", 258).
yeccpars2_206_long(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_206_octet,1}}).
-file("icparse.yrl", 258).
yeccpars2_206_octet(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_206_sequence,1}}).
-file("icparse.yrl", 258).
yeccpars2_206_sequence(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_206_short,1}}).
-file("icparse.yrl", 258).
yeccpars2_206_short(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_206_string,1}}).
-file("icparse.yrl", 258).
yeccpars2_206_string(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_206_struct,1}}).
-file("icparse.yrl", 258).
yeccpars2_206_struct(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_206_union,1}}).
-file("icparse.yrl", 258).
yeccpars2_206_union(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_206_unsigned,1}}).
-file("icparse.yrl", 258).
yeccpars2_206_unsigned(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_206_wchar,1}}).
-file("icparse.yrl", 258).
yeccpars2_206_wchar(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_206_wstring,1}}).
-file("icparse.yrl", 258).
yeccpars2_206_wstring(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_206_,1}}).
-file("icparse.yrl", 600).
yeccpars2_206_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   reverse ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_209_,1}}).
-file("icparse.yrl", 596).
yeccpars2_209_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # struct { id = __2 , body = __4 }
  end | __Stack].

-compile({inline,{yeccpars2_210_,1}}).
-file("icparse.yrl", 610).
yeccpars2_210_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2 ++ __1
  end | __Stack].

-compile({inline,{yeccpars2_212_,1}}).
-file("icparse.yrl", 258).
yeccpars2_212_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_214_,1}}).
-file("icparse.yrl", 258).
yeccpars2_214_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_215_,1}}).
-file("icparse.yrl", 619).
yeccpars2_215_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ __4 ++ __6 ++ [ # member { type = __2 , id = __3 } ]
  end | __Stack].

-compile({inline,{yeccpars2_219_,1}}).
-file("icparse.yrl", 293).
yeccpars2_219_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_220_,1}}).
-file("icparse.yrl", 308).
yeccpars2_220_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # module { id = __2 , body = reverse ( __4 ) }
  end | __Stack].

-compile({inline,{'yeccpars2_221_;',1}}).
-file("icparse.yrl", 324).
'yeccpars2_221_;'(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   # forward { id = __2 }
  end | __Stack].

-compile({inline,{yeccpars2_221_,1}}).
-file("icparse.yrl", 355).
yeccpars2_221_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_222_,1}}).
-file("icparse.yrl", 329).
yeccpars2_222_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __2 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_225_,1}}).
-file("icparse.yrl", 364).
yeccpars2_225_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_226_,1}}).
-file("icparse.yrl", 360).
yeccpars2_226_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | reverse ( __3 ) ]
  end | __Stack].

-compile({inline,{yeccpars2_228_,1}}).
-file("icparse.yrl", 366).
yeccpars2_228_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_230_,1}}).
-file("icparse.yrl", 258).
yeccpars2_230_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_231_,1}}).
-file("icparse.yrl", 258).
yeccpars2_231_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_233_,1}}).
-file("icparse.yrl", 847).
yeccpars2_233_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2 ++ __1
  end | __Stack].

-compile({inline,{yeccpars2_234_,1}}).
-file("icparse.yrl", 750).
yeccpars2_234_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # except { id = __2 , body = reverse ( __4 ) }
  end | __Stack].

-compile({inline,{yeccpars2_236_,1}}).
-file("icparse.yrl", 258).
yeccpars2_236_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_238_,1}}).
-file("icparse.yrl", 258).
yeccpars2_238_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_239_,1}}).
-file("icparse.yrl", 701).
yeccpars2_239_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # enumerator { id = __1 }
  end | __Stack].

-compile({inline,{yeccpars2_240_,1}}).
-file("icparse.yrl", 696).
yeccpars2_240_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_241_,1}}).
-file("icparse.yrl", 258).
yeccpars2_241_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_243_,1}}).
-file("icparse.yrl", 258).
yeccpars2_243_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_244_,1}}).
-file("icparse.yrl", 688).
yeccpars2_244_(__Stack0) ->
 [__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # enum { id = __2 , body = __4 ++ __6 ++ __8 ++ [ __5 | reverse ( __7 ) ] }
  end | __Stack].

-compile({inline,{yeccpars2_246_,1}}).
-file("icparse.yrl", 698).
yeccpars2_246_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2 ++ __4 ++ [ __5 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_259_,1}}).
-file("icparse.yrl", 378).
yeccpars2_259_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # const { type = __2 , id = __3 , val = __5 }
  end | __Stack].

-compile({inline,{yeccpars2_260_,1}}).
-file("icparse.yrl", 0).
yeccpars2_260_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_262_,1}}).
-file("icparse.yrl", 282).
yeccpars2_262_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_264_,1}}).
-file("icparse.yrl", 282).
yeccpars2_264_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_265_,1}}).
-file("icparse.yrl", 284).
yeccpars2_265_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_266_,1}}).
-file("icparse.yrl", 269).
yeccpars2_266_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
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
-file("icparse.yrl", 298).
yeccpars2_267_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,{yeccpars2_268_,1}}).
-file("icparse.yrl", 299).
yeccpars2_268_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,{yeccpars2_269_,1}}).
-file("icparse.yrl", 300).
yeccpars2_269_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,{yeccpars2_270_,1}}).
-file("icparse.yrl", 337).
yeccpars2_270_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{'yeccpars2_271_::',1}}).
-file("icparse.yrl", 757).
'yeccpars2_271_::'(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,{'yeccpars2_271_<identifier>',1}}).
-file("icparse.yrl", 757).
'yeccpars2_271_<identifier>'(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,{yeccpars2_271_Object,1}}).
-file("icparse.yrl", 757).
yeccpars2_271_Object(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,{yeccpars2_271_any,1}}).
-file("icparse.yrl", 757).
yeccpars2_271_any(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,{yeccpars2_271_boolean,1}}).
-file("icparse.yrl", 757).
yeccpars2_271_boolean(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,{yeccpars2_271_char,1}}).
-file("icparse.yrl", 757).
yeccpars2_271_char(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,{yeccpars2_271_double,1}}).
-file("icparse.yrl", 757).
yeccpars2_271_double(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,{yeccpars2_271_float,1}}).
-file("icparse.yrl", 757).
yeccpars2_271_float(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,{yeccpars2_271_long,1}}).
-file("icparse.yrl", 757).
yeccpars2_271_long(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,{yeccpars2_271_octet,1}}).
-file("icparse.yrl", 757).
yeccpars2_271_octet(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,{yeccpars2_271_short,1}}).
-file("icparse.yrl", 757).
yeccpars2_271_short(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,{yeccpars2_271_string,1}}).
-file("icparse.yrl", 757).
yeccpars2_271_string(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,{yeccpars2_271_unsigned,1}}).
-file("icparse.yrl", 757).
yeccpars2_271_unsigned(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,{yeccpars2_271_void,1}}).
-file("icparse.yrl", 757).
yeccpars2_271_void(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,{yeccpars2_271_wchar,1}}).
-file("icparse.yrl", 757).
yeccpars2_271_wchar(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,{yeccpars2_271_wstring,1}}).
-file("icparse.yrl", 757).
yeccpars2_271_wstring(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,{yeccpars2_271_attribute,1}}).
-file("icparse.yrl", 851).
yeccpars2_271_attribute(__Stack0) ->
 [begin
   nil
  end | __Stack0].

-compile({inline,{yeccpars2_273_,1}}).
-file("icparse.yrl", 318).
yeccpars2_273_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # interface { id = element ( 1 , __1 ) , inherit = element ( 2 , __1 ) ,
    body = lists : reverse ( __3 ) }
  end | __Stack].

-compile({inline,{yeccpars2_281_,1}}).
-file("icparse.yrl", 340).
yeccpars2_281_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   if list ( __2 ) -> __2 ++ __1 ;
    true -> [ __2 | __1 ]
    end
  end | __Stack].

-compile({inline,{yeccpars2_287_,1}}).
-file("icparse.yrl", 349).
yeccpars2_287_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,{yeccpars2_288_,1}}).
-file("icparse.yrl", 347).
yeccpars2_288_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,{yeccpars2_289_,1}}).
-file("icparse.yrl", 348).
yeccpars2_289_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,{yeccpars2_290_,1}}).
-file("icparse.yrl", 350).
yeccpars2_290_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,{yeccpars2_291_,1}}).
-file("icparse.yrl", 346).
yeccpars2_291_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,{yeccpars2_299_,1}}).
-file("icparse.yrl", 803).
yeccpars2_299_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_300_,1}}).
-file("icparse.yrl", 258).
yeccpars2_300_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_302_,1}}).
-file("icparse.yrl", 258).
yeccpars2_302_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_304_,1}}).
-file("icparse.yrl", 776).
yeccpars2_304_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_309_,1}}).
-file("icparse.yrl", 793).
yeccpars2_309_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # param { inout = __1 , type = __2 , id = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_311_,1}}).
-file("icparse.yrl", 258).
yeccpars2_311_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_314_,1}}).
-file("icparse.yrl", 775).
yeccpars2_314_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2 ++ [ __3 | reverse ( __4 ) ]
  end | __Stack].

-compile({inline,{yeccpars2_315_,1}}).
-file("icparse.yrl", 258).
yeccpars2_315_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_317_,1}}).
-file("icparse.yrl", 258).
yeccpars2_317_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_318_,1}}).
-file("icparse.yrl", 786).
yeccpars2_318_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2 ++ __4 ++ __6 ++ [ __5 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_319_,1}}).
-file("icparse.yrl", 812).
yeccpars2_319_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_323_,1}}).
-file("icparse.yrl", 364).
yeccpars2_323_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_325_,1}}).
-file("icparse.yrl", 808).
yeccpars2_325_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | reverse ( __4 ) ]
  end | __Stack].

-compile({inline,{yeccpars2_326_,1}}).
-file("icparse.yrl", 754).
yeccpars2_326_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # op { oneway = __1 , type = __2 , id = __3 , params = __4 , raises = __5 , ctx = __6 }
  end | __Stack].

-compile({inline,{yeccpars2_330_,1}}).
-file("icparse.yrl", 833).
yeccpars2_330_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_332_,1}}).
-file("icparse.yrl", 817).
yeccpars2_332_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | reverse ( __4 ) ]
  end | __Stack].

-compile({inline,{yeccpars2_334_,1}}).
-file("icparse.yrl", 835).
yeccpars2_334_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_337_,1}}).
-file("icparse.yrl", 838).
yeccpars2_337_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_338_,1}}).
-file("icparse.yrl", 740).
yeccpars2_338_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # attr { readonly = __1 , type = __3 , id = [ __4 | reverse ( __5 ) ] }
  end | __Stack].

-compile({inline,{yeccpars2_340_,1}}).
-file("icparse.yrl", 840).
yeccpars2_340_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_341_,1}}).
-file("icparse.yrl", 301).
yeccpars2_341_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].

-compile({inline,{yeccpars2_342_,1}}).
-file("icparse.yrl", 297).
yeccpars2_342_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1
  end | __Stack].


-file("icparse.yrl", 865).
