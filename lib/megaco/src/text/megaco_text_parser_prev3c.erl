-module(megaco_text_parser_prev3c).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("megaco_text_parser_prev3c.yrl", 1665).

%% The following directive is needed for (significantly) faster compilation
%% of the generated .erl file by the HiPE compiler.  Please do not remove.
-compile([{hipe,[{regalloc,linear_scan}]}]).

-include("megaco_text_parser_prev3c.hrl").



-file("/net/shelob/ldisk/daily_build/otp_prebuild_r13b.2009-04-15_20/otp_src_R13B/bootstrap/lib/parsetools/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type(yecc_ret() :: {'error', _} | {'ok', _}).

-spec(parse/1 :: (_) -> yecc_ret()).
parse(Tokens) ->
    yeccpars0(Tokens, false).

-spec(parse_and_scan/1 ::
      ({function() | {atom(), atom()}, [_]} | {atom(), atom(), [_]}) ->
            yecc_ret()).
parse_and_scan({F, A}) -> % Fun or {M, F}
    yeccpars0([], {F, A});
parse_and_scan({M, F, A}) ->
    yeccpars0([], {{M, F}, A}).

-spec(format_error/1 :: (any()) -> [char() | list()]).
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
-spec(return_error/2 :: (integer(), any()) -> no_return()).
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.3").

yeccpars0(Tokens, MFA) ->
    try yeccpars1(Tokens, MFA, 0, [], [])
    catch 
        error: Error ->
            Stacktrace = erlang:get_stacktrace(),
            try yecc_error_type(Error, Stacktrace) of
                {syntax_error, Token} ->
                    yeccerror(Token);
                {missing_in_goto_table=Tag, Symbol, State} ->
                    Desc = {Symbol, State, Tag},
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        throw: {error, {_Line, ?MODULE, _M}} = Error -> 
            Error % probably from return_error/2
    end.

yecc_error_type(function_clause, [{?MODULE,F,[State,_,_,_,Token,_,_]} | _]) ->
    case atom_to_list(F) of
        "yeccpars2" ++ _ ->
            {syntax_error, Token};
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            {missing_in_goto_table, Symbol, State}
    end.

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

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Stack1, [Token | Tokens], 
          Tokenizer) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Stack1 | Vstack], Token, Tokens, Tokenizer);
yeccpars1(State1, State, States, Vstack, Stack1, [], {F, A}) ->
    case apply(F, A) of
        {ok, Tokens, _Endline} ->
	    yeccpars1(State1, State, States, Vstack, Stack1, Tokens, {F, A});
        {eof, _Endline} ->
            yeccpars1(State1, State, States, Vstack, Stack1, [], false);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1(State1, State, States, Vstack, Stack1, [], false) ->
    yeccpars2(State, '$end', [State1 | States], [Stack1 | Vstack],
              {'$end', 999999}, [], false).

% For internal use only.
yeccerror(Token) ->
    Text = case catch erl_scan:token_info(Token, text) of
               {text, Txt} -> Txt;
               _ -> yecctoken2string(Token)
           end,
    Location = case catch erl_scan:token_info(Token, location) of
                   {location, Loc} -> Loc;
                   _ -> element(2, Token)
               end,
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_unicode_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:format("~w", [A]);
yecctoken2string({_Cat, _, Val}) -> io_lib:format("~w", [Val]);
yecctoken2string({dot, _}) -> "'.'";
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:format("~w", [Other]);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("./megaco_text_parser_prev3c.erl", 165).

yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(120=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(125=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(126=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_127(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(128=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(130=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_130(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_133(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_134(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_135(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_136(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_137(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_138(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_139(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_140(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_142(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_143(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_145(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_146(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(147=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(148=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(149=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(150=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(151=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(152=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(153=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(154=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(155=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(156=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(157=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(158=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(159=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(160=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(161=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(162=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(163=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(164=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_164(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(165=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(166=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_166(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(167=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_167(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(168=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_168(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(169=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(170=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(171=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_171(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(172=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(173=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(174=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(175=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(176=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_176(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(177=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_177(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(178=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_178(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(179=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(180=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_180(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(181=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_181(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(182=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_182(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(183=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(184=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_184(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(185=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_185(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(186=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(187=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_187(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(188=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_188(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(189=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(190=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_190(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(191=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_191(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(192=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(193=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_193(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(194=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_194(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(195=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_195(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(196=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_196(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(197=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_197(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(198=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_198(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(199=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_199(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(200=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_200(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(201=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_201(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(202=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_202(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(203=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_203(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(204=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_204(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(205=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_205(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(206=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_206(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(207=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_207(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(208=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_208(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(209=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_209(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(210=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_210(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(211=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_211(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(212=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(213=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(214=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(215=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_215(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(216=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_216(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(217=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(218=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_218(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(219=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_219(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(220=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_220(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(221=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(222=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_222(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(223=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_223(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(224=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_224(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(225=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_225(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(226=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_226(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(227=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_227(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(228=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_228(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(229=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_229(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(230=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_230(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(231=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_231(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(232=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_232(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(233=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_233(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(234=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(235=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_235(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(236=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_236(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(237=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(238=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_238(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(239=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_239(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(240=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_240(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(241=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_241(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(242=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_242(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(243=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_243(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(244=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_244(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(245=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_245(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(246=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_246(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(247=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_247(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(248=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_248(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(249=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_249(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(250=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_250(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(251=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_251(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(252=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_252(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(253=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(254=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_254(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(255=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_255(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(256=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_256(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(257=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_257(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(258=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(259=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_259(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(260=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(261=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_261(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(262=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_262(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(263=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_263(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(272=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_264(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(273=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_273(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(274=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_274(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(275=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_275(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(276=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(277=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_277(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(278=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(279=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_279(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(280=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_280(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(281=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_281(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(282=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_282(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(283=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_283(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(284=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_284(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(285=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_285(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(286=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_286(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(287=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_287(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(288=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_287(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(289=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_287(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(290=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_290(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(291=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_291(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(292=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_292(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(293=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_293(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(294=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_294(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(295=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_295(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(296=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_296(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(297=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_287(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(298=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_287(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(299=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_299(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(300=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_300(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(301=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_287(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(302=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_287(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(303=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_303(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(304=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_304(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(305=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_305(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(312=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_242(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(313=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_313(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(314=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_314(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(315=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_315(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(316=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(317=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_317(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(318=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_318(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(319=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_319(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(320=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_320(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(321=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_321(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(322=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_322(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(323=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_323(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(324=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_324(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(325=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_325(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(326=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_326(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(327=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_327(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(328=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_328(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(329=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_329(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(330=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_330(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(331=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_331(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(332=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_332(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(333=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_333(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(334=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_334(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(335=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_335(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(336=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_336(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(337=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_337(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(338=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_338(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(339=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_339(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(340=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_340(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(341=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_340(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(342=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_342(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(343=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(344=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(345=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(346=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_346(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(347=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_347(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(348=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_348(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(349=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(350=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_350(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(351=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_351(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(352=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_352(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(353=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_353(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(354=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_354(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(355=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_355(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(356=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_356(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(357=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_357(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(358=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_358(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(359=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_359(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(360=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_360(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(361=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_361(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(362=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_361(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(363=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_363(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(364=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_364(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(365=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_365(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(366=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_366(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(367=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_367(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(368=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_368(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(369=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_369(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(370=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_370(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(371=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_354(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(372=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_372(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(373=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_373(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(374=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_374(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(375=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_375(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(376=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_320(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(377=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_377(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(378=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_378(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(379=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_379(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(380=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(381=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(382=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_382(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(383=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_383(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(384=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_384(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(385=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_385(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(386=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(387=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_387(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(388=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_388(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(389=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(390=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_390(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(391=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_391(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(392=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_392(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(393=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_393(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(394=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_394(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(395=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_395(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(396=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_396(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(397=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_397(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(398=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_398(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(399=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_399(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(400=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_400(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(401=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_401(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(402=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_402(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(403=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_403(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(404=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_404(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(405=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_405(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(406=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_406(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(407=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_407(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(408=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_408(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(409=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_409(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(410=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_410(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(411=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_411(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(412=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_412(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(413=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_413(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(414=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_414(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(415=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(416=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_416(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(417=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_417(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(418=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_418(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(419=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_419(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(420=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_420(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(421=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_421(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(422=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_422(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(423=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_423(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(424=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_424(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(425=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_425(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(426=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_426(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(427=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_427(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(428=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_428(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(429=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_429(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(430=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_430(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(431=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_244(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(432=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_432(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(433=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_433(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(434=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_434(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(435=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_435(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(436=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_436(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(437=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_437(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(438=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_438(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(439=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_439(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(440=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_440(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(441=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_441(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(442=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_442(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(443=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(444=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_444(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(445=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_445(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(446=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_446(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(447=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_447(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(448=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_287(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(449=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_449(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(450=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(451=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_451(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(452=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_452(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(453=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_453(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(454=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(455=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_455(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(456=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(457=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_457(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(458=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_458(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(459=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_459(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(460=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_420(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(461=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_461(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(462=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_462(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(463=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_463(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(464=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_464(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(465=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(466=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_466(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(467=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(468=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_468(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(469=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_469(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(470=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_470(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(471=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_471(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(472=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_472(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(473=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_473(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(474=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(475=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_475(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(476=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_476(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(477=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_477(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(478=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(479=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_479(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(480=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_480(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(481=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_481(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(482=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_482(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(483=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_483(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(484=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_484(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(485=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_485(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(486=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_486(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(487=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(488=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_488(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(489=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_489(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(490=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_244(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(491=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_491(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(492=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_492(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(493=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(494=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_494(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(495=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_495(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(496=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_496(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(497=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_497(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(498=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_498(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(499=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(500=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_500(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(501=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_501(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(502=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_502(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(503=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_503(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(504=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_504(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(505=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_505(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(506=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_506(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(507=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_507(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(508=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_508(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(509=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_509(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(510=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_510(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(511=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_511(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(512=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_512(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(513=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_513(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(514=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_514(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(515=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_515(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(516=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_516(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(517=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_517(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(518=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_518(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(519=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_519(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(520=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_520(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(521=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_521(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(522=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_522(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(523=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_523(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(524=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_524(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(525=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_525(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(526=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_526(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(527=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_527(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(528=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_528(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(529=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_529(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(530=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_530(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(531=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_531(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(532=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_532(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(533=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_533(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(534=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_534(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(535=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_535(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(536=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(537=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_537(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(538=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_538(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(539=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(540=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_540(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(541=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_541(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(542=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_542(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(543=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_543(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(544=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_544(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(545=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_545(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(546=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_546(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(547=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_547(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(548=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_548(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(549=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_549(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(550=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(551=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_551(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(552=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_244(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(553=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_553(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(554=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_554(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(555=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_555(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(556=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_556(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(557=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_557(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(558=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_558(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(559=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(560=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_560(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(561=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_561(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(562=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(563=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_563(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(564=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_564(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(565=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_565(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(566=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_566(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(567=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(568=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_568(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(569=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_569(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(570=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(571=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_571(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(572=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_572(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(573=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_573(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(574=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_574(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(575=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_575(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(576=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_576(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(577=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_577(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(578=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_578(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(579=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_579(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(580=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_580(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(581=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_581(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(582=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_582(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(583=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_583(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(584=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_584(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(585=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_585(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(586=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_586(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(587=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_587(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(588=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_588(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(589=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_589(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(590=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_590(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(591=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_591(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(592=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(593=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_593(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(594=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_594(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(595=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_595(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(596=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_596(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(597=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_287(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(598=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_598(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(599=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_599(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(600=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_600(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(601=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_601(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(602=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(603=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_603(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(604=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_604(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(605=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_605(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(606=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_606(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(607=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_607(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(608=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_608(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(609=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_609(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(610=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_610(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(611=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(612=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_612(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(613=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(614=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_614(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(615=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_615(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(616=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(617=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_617(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(618=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_618(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(619=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_619(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(620=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_620(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(621=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_606(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(622=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_622(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(623=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_623(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(624=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_624(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(625=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(626=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_626(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(627=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_627(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(628=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_628(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(629=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(630=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_630(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(631=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_631(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(632=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_632(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(633=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(634=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(635=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_635(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(636=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_636(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(637=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_637(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(638=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(639=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_639(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(640=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_640(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(641=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_641(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(642=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_642(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(643=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(644=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_644(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(645=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_645(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(646=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_646(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(647=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_647(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(648=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_648(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(649=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_649(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(650=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_650(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(651=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_651(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(652=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_652(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(653=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_653(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(654=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_654(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(655=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_655(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(656=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_656(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(657=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_657(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(658=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_658(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(659=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_659(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(660=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_660(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(661=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_661(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(662=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_662(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(663=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_663(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(664=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_664(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(665=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_665(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(666=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_666(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(667=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_667(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(668=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_340(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(669=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_669(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(670=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_670(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(671=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_671(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(672=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_672(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(673=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_673(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(674=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_674(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(675=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_661(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(676=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_676(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(677=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_677(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(678=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_678(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(679=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(680=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_680(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(681=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_681(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(682=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_682(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(683=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_683(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(684=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_681(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(685=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_685(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(686=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_686(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(687=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_687(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(688=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_688(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(689=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_689(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(690=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_690(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(691=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_691(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(692=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_692(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(693=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_693(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(694=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_503(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(695=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_695(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(696=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_503(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(697=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_697(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(698=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_361(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(699=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_699(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(700=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_700(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(701=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_688(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(702=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_702(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(703=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_703(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(704=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_704(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(705=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_705(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(706=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_649(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(707=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_707(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(708=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_708(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(709=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_709(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(710=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(711=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_711(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(712=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(713=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_713(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(714=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_714(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(715=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_715(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(716=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_716(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(717=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_717(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(718=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_718(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(719=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_719(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(720=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_720(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(721=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_721(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(722=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_722(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(723=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_723(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(724=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_724(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(725=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_725(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(726=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_726(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(727=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_727(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(728=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_728(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(729=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_729(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(730=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_730(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(731=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_731(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(732=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_732(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(733=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_733(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(734=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_734(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(735=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_735(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(736=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_736(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(737=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_737(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(738=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_738(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(739=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(740=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_740(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(741=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(742=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_742(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(743=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_743(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(744=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_744(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(745=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_745(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(746=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_746(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(747=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_747(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(748=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_748(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(749=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_749(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(750=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_750(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(751=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_751(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(752=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_752(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(753=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_753(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(754=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_754(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(755=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_755(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(756=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_756(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(757=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_757(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(758=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_745(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(759=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_759(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(760=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_760(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(761=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_761(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(762=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_762(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(763=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(764=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_764(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(765=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_765(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(766=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_766(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(767=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_767(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(768=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_768(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(769=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_769(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(770=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_770(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(771=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_771(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(772=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_772(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(773=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_773(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(774=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_774(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(775=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_716(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(776=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_776(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(777=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_777(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(778=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_778(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(779=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_779(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(780=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(781=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_781(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(782=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_782(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(783=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_783(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(784=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_784(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(785=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_785(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(786=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_786(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(787=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_787(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(788=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_788(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(789=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_789(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(790=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_790(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(791=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_791(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(792=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_792(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(793=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_573(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(794=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_794(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(795=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_795(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(796=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_796(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(797=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_797(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(798=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(799=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_799(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(800=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_800(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(801=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_801(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(802=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_802(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(803=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_803(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(804=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(805=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_805(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(806=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_806(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(807=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_807(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(808=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(809=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_809(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(810=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_810(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(811=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_811(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(812=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(813=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_813(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(814=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_814(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(815=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_815(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(816=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(817=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_817(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(818=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_818(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(819=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_819(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(820=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(821=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_821(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(822=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_822(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(823=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_823(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(824=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_824(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(825=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_825(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(826=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_826(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(827=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_827(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(828=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_828(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(829=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_829(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(830=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(831=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_831(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(832=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_832(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(833=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_833(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(834=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_834(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(835=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_835(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(836=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_836(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(837=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_837(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(838=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_838(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(839=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_839(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(840=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_840(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(841=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_841(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(842=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_842(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(843=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_843(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(844=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_844(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(845=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_845(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(846=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_846(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(847=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_847(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(848=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_848(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(849=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_849(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(850=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(851=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_851(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(852=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_852(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(853=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_853(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(854=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_854(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(855=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_855(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(856=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_856(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(857=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_857(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(858=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_858(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(859=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_859(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(860=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_860(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(861=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_861(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(862=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_862(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(863=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_863(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(864=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_864(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(865=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_865(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(866=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_866(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(867=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_867(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(868=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_868(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(869=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_857(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(870=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_870(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(871=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_871(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(872=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_872(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(873=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_873(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(874=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_874(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(875=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(876=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_876(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(877=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_877(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(878=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_878(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(879=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_879(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(880=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_880(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(881=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_881(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(882=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_882(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(883=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_883(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(884=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_884(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(885=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_885(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(886=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_886(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(887=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_887(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(888=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_888(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(889=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_889(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(890=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(891=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_891(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(892=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_892(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(893=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_893(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(894=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_894(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(895=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_895(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(896=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_896(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(897=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_897(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(898=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_898(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(899=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_899(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(900=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_900(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(901=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_901(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(902=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_902(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(903=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_903(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(904=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_904(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(905=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_905(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(906=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_906(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(907=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_907(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(908=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_908(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(909=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_909(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(910=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_910(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(911=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_911(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(912=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_912(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(913=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_913(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(914=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_914(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(915=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_915(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(916=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_916(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(917=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_917(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(918=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_918(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(919=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(920=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_920(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(921=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_921(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(922=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(923=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_923(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(924=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_924(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(925=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_925(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(926=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_926(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(927=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_898(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(928=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_928(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(929=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_929(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(930=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_930(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(931=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_931(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(932=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_932(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(933=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_933(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(934=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_934(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(935=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_935(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(936=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(937=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_937(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(938=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_938(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(939=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_898(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(940=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_940(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(941=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_941(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(942=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_942(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(943=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_832(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(944=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_944(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(945=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_945(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(946=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_946(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(947=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_947(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(948=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_948(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(949=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_949(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(950=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_950(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(951=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_951(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(952=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(953=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_953(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(954=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_954(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(955=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_955(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(956=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_956(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(957=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_957(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(958=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_958(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(959=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_959(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(960=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_960(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(961=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_961(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(962=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_962(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(963=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_963(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.3",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_0_(Stack),
 yeccpars2_1(1, Cat, [0 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_1(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 5, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_1_(Stack),
 yeccpars2_4(4, Cat, [1 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_2(_S, '$end', _Ss, Stack,  _T, _Ts, _Tzr) ->
 {ok, hd(Stack)}.

yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_3_(Stack),
 yeccgoto_optSep(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_4(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_cont_4(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'MessageSegmentToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'SegmentationCompleteToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr).

yeccpars2_5(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_6: see yeccpars2_4

yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_7_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_8(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr).

yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_83: see yeccpars2_4

yeccpars2_84(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_85: see yeccpars2_4

yeccpars2_86(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_86_(Stack),
 yeccpars2_87(_S, Cat, [86 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_87_(Stack),
 yeccgoto_authenticationHeader(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_88(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_88_(Stack),
 yeccpars2_91(91, Cat, [88 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_89(S, endOfMessage, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr).

yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_90_(Stack),
 yeccgoto_megacoMessage(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_91(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'MtpAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 961, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_92(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr).

yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_mId(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_mId(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_95: see yeccpars2_4

yeccpars2_96(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'MessageSegmentToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'SegmentationCompleteToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_96_(Stack),
 yeccpars2_98(98, Cat, [96 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_97(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'MessageSegmentToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'SegmentationCompleteToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_97_(Stack),
 yeccpars2_106(_S, Cat, [97 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_98(S, 'RSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr).

yeccpars2_99(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'MessageSegmentToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'SegmentationCompleteToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_(Stack),
 yeccpars2_100(_S, Cat, [99 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_100_(Stack),
 yeccgoto_daddr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_101(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_101_(Stack),
 yeccgoto_domainAddress(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_102: see yeccpars2_4

yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_(Stack),
 yeccgoto_portNumber(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_104(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_104_(Stack),
 yeccpars2_105(_S, Cat, [104 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_105_(Stack),
 yeccgoto_domainAddress(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_106_(Stack),
 yeccgoto_daddr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_107(S, 'GREATER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr).

yeccpars2_108(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 109, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_108_(Stack),
 yeccgoto_domainName(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_109: see yeccpars2_4

yeccpars2_110(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_110_(Stack),
 yeccpars2_111(_S, Cat, [110 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_111_(Stack),
 yeccgoto_domainName(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_112_(Stack),
 yeccgoto_transactionItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_113_(Stack),
 yeccgoto_transactionItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_114_(Stack),
 yeccgoto_transactionItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_115_(Stack),
 yeccgoto_transactionItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_116_(Stack),
 yeccgoto_messageBody(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_117(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_117(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr);
yeccpars2_117(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_117(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_117_(Stack),
 yeccgoto_transactionList(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_118_(Stack),
 yeccgoto_message(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_119_(Stack),
 yeccgoto_messageBody(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_120(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 890, Ss, Stack, T, Ts, Tzr).

yeccpars2_121(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 952, Ss, Stack, T, Ts, Tzr).

yeccpars2_122(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 820, Ss, Stack, T, Ts, Tzr).

yeccpars2_123(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 812, Ss, Stack, T, Ts, Tzr).

yeccpars2_124(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr).

yeccpars2_125(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 804, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_126(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr).

yeccpars2_127(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 798, Ss, Stack, T, Ts, Tzr);
yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_127_(Stack),
 yeccpars2_797(797, Cat, [127 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_128(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 129, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_129: see yeccpars2_4

yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_130_(Stack),
 yeccgoto_contextID(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_131(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr).

yeccpars2_132(S, 'AddToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 149, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 150, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'ContextAttrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 151, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 152, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'EmergencyOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 153, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'EmergencyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 154, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'IEPSToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 155, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'ModifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'MoveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 157, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'PriorityToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 159, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 160, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'SubtractToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'TopologyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr).

yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_133_(Stack),
 yeccgoto_contextProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_134(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_commandRequest(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_commandRequest(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_136_(Stack),
 yeccgoto_contextProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_commandRequest(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_138(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_138_(Stack),
 yeccgoto_contextProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_139_(Stack),
 yeccgoto_actionRequestItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_140(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_140_(Stack),
 yeccgoto_actionRequestItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_contextProperty(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_142_(Stack),
 yeccgoto_actionRequestItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_commandRequest(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_144(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 570, Ss, Stack, T, Ts, Tzr).

yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_commandRequest(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_146(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 567, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_146_(Stack),
 yeccpars2_566(_S, Cat, [146 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_147(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 565, Ss, Stack, T, Ts, Tzr).

yeccpars2_148(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_148_(Stack),
 yeccgoto_ammToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_149(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 562, Ss, Stack, T, Ts, Tzr).

yeccpars2_150(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 559, Ss, Stack, T, Ts, Tzr).

yeccpars2_151(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 558, Ss, Stack, T, Ts, Tzr).

yeccpars2_152(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 507, Ss, Stack, T, Ts, Tzr).

yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_153_(Stack),
 yeccgoto_contextProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_154_(Stack),
 yeccgoto_contextProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_155(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 503, Ss, Stack, T, Ts, Tzr).

yeccpars2_156(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_156_(Stack),
 yeccgoto_ammToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_157_(Stack),
 yeccgoto_ammToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_158(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 467, Ss, Stack, T, Ts, Tzr).

yeccpars2_159(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 465, Ss, Stack, T, Ts, Tzr).

yeccpars2_160(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 415, Ss, Stack, T, Ts, Tzr).

yeccpars2_161(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr).

yeccpars2_162(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr).

yeccpars2_163(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 170, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, 'OnewayBothToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 171, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, 'OnewayExternalToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 173, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_164(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_topologyDescComp(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_165(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_165_(Stack),
 yeccpars2_178(178, Cat, [165 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_166_(Stack),
 yeccgoto_topologyDescComp(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_167_(Stack),
 yeccgoto_terminationID(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_168_(Stack),
 yeccgoto_topologyDescComp(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_169(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_169_(Stack),
 yeccgoto_topologyDirection(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_170_(Stack),
 yeccgoto_topologyDirection(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_171_(Stack),
 yeccgoto_topologyDirection(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_172_(Stack),
 yeccgoto_topologyDirection(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_173_(Stack),
 yeccgoto_topologyDirection(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_174(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 175, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_175: see yeccpars2_4

yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_(Stack),
 yeccgoto_eventStream(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_177(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_177_(Stack),
 yeccgoto_streamID(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_178(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_179: see yeccpars2_163

yeccpars2_180(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_180_(Stack),
 yeccpars2_181(_S, Cat, [180 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_181(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_181_(Stack),
 yeccgoto_topologyDescCompList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_182(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_182_(Stack),
 yeccgoto_topologyDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_183(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_184_(Stack),
 yeccgoto_termIDList(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_185(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_185(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_185_(Stack),
 yeccpars2_193(_S, Cat, [185 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_186: see yeccpars2_4

yeccpars2_187(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_187_(Stack),
 yeccpars2_188(188, Cat, [187 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_188(S, 'RSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_189: see yeccpars2_4

yeccpars2_190(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_190_(Stack),
 yeccpars2_191(_S, Cat, [190 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_191(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_191_(Stack),
 yeccgoto_terminationIDListRepeat(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_192_(Stack),
 yeccgoto_termIDList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_193(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_193_(Stack),
 yeccgoto_subtractRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_194(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr).

yeccpars2_195(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 414, Ss, Stack, T, Ts, Tzr).

yeccpars2_196(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 197, Ss, Stack, T, Ts, Tzr).

yeccpars2_197(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 210, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, 'DigitMapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 211, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 212, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 213, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 214, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, 'ModemToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 215, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, 'MuxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 216, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, 'ObservedEventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 217, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, 'PackagesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 218, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 219, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 220, Ss, Stack, T, Ts, Tzr);
yeccpars2_197(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_197_(Stack),
 yeccpars2_209(209, Cat, [197 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_198_(Stack),
 yeccgoto_auditItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_199_(Stack),
 yeccgoto_indAudauditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_200(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_200_(Stack),
 yeccgoto_indAudauditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_201_(Stack),
 yeccgoto_indAudauditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_202_(Stack),
 yeccgoto_indAudauditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_203(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_203_(Stack),
 yeccgoto_indAudauditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_204(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_204_(Stack),
 yeccgoto_indAudauditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_205(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_205_(Stack),
 yeccgoto_indAudauditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_206(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 406, Ss, Stack, T, Ts, Tzr);
yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_(Stack),
 yeccpars2_405(_S, Cat, [206 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_207(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_auditItem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_208(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 402, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_208_(Stack),
 yeccpars2_401(_S, Cat, [208 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_209(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 400, Ss, Stack, T, Ts, Tzr).

yeccpars2_210(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_210_(Stack),
 yeccgoto_indAuddigitMapDescriptor(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_211(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_211_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_212(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 389, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_212_(Stack),
 yeccgoto_auditItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_213(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 380, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 381, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_213_(Stack),
 yeccgoto_auditItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_214(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 320, Ss, Stack, T, Ts, Tzr);
yeccpars2_214(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_214_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_215(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_215_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_216(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_216_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_217_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_218(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 316, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_218_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_219(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 226, Ss, Stack, T, Ts, Tzr);
yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_219_(Stack),
 yeccgoto_auditItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_220(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 221, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_220_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_221: see yeccpars2_4

yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_222_(Stack),
 yeccgoto_pkgdName(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_223(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 224, Ss, Stack, T, Ts, Tzr).

yeccpars2_224(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_224_(Stack),
 yeccgoto_indAudstatisticsDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_225(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_225_(Stack),
 yeccgoto_indAudsignalsDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_226(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 232, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 233, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_226(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_227_(Stack),
 yeccgoto_indAudsignalParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_228(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 242, Ss, Stack, T, Ts, Tzr);
yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_228_(Stack),
 yeccgoto_signalRequest(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_229(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_signalName(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_230(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 241, Ss, Stack, T, Ts, Tzr).

yeccpars2_231(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_231_(Stack),
 yeccgoto_indAudsignalParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_232(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_232_(Stack),
 yeccgoto_optIndAudsignalParm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_233(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 234, Ss, Stack, T, Ts, Tzr);
yeccpars2_233(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_234: see yeccpars2_4

yeccpars2_235(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 237, Ss, Stack, T, Ts, Tzr);
yeccpars2_235(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_235_(Stack),
 yeccgoto_indAudsignalList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_236(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_236_(Stack),
 yeccgoto_signalListId(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_237: see yeccpars2_4

yeccpars2_238(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_signalListParm(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_239(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 240, Ss, Stack, T, Ts, Tzr).

yeccpars2_240(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_240_(Stack),
 yeccgoto_indAudsignalList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_241(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_241_(Stack),
 yeccgoto_optIndAudsignalParm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_242(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_242(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_242(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 245, Ss, Stack, T, Ts, Tzr);
yeccpars2_242(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 246, Ss, Stack, T, Ts, Tzr);
yeccpars2_242(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_242(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_242(S, 'IntsigDelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 247, Ss, Stack, T, Ts, Tzr);
yeccpars2_242(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 248, Ss, Stack, T, Ts, Tzr);
yeccpars2_242(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_242(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_242(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 249, Ss, Stack, T, Ts, Tzr);
yeccpars2_242(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_242(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_242(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 250, Ss, Stack, T, Ts, Tzr);
yeccpars2_242(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_242(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_242(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 251, Ss, Stack, T, Ts, Tzr);
yeccpars2_242(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 252, Ss, Stack, T, Ts, Tzr);
yeccpars2_242(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_242(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_243(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 312, Ss, Stack, T, Ts, Tzr);
yeccpars2_243(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_243_(Stack),
 yeccpars2_311(311, Cat, [243 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_244(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 286, Ss, Stack, T, Ts, Tzr);
yeccpars2_244(S, 'GREATER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 287, Ss, Stack, T, Ts, Tzr);
yeccpars2_244(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 288, Ss, Stack, T, Ts, Tzr);
yeccpars2_244(S, 'NEQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 289, Ss, Stack, T, Ts, Tzr).

yeccpars2_245(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 280, Ss, Stack, T, Ts, Tzr);
yeccpars2_245(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_246(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 278, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_247(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 276, Ss, Stack, T, Ts, Tzr).

yeccpars2_248(_S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_248_COMMA(Stack),
 yeccgoto_sigParameter(hd(Ss), 'COMMA', Ss, NewStack, T, Ts, Tzr);
yeccpars2_248(_S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_248_RBRKT(Stack),
 yeccgoto_sigParameter(hd(Ss), 'RBRKT', Ss, NewStack, T, Ts, Tzr);
yeccpars2_248(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_249(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 263, Ss, Stack, T, Ts, Tzr);
yeccpars2_249(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_250(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 260, Ss, Stack, T, Ts, Tzr);
yeccpars2_250(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_251(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 255, Ss, Stack, T, Ts, Tzr);
yeccpars2_251(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_252(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 253, Ss, Stack, T, Ts, Tzr);
yeccpars2_252(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_253: see yeccpars2_4

yeccpars2_254(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_254_(Stack),
 yeccgoto_sigParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_255(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 257, Ss, Stack, T, Ts, Tzr);
yeccpars2_255(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 258, Ss, Stack, T, Ts, Tzr);
yeccpars2_255(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 259, Ss, Stack, T, Ts, Tzr).

yeccpars2_256(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_256_(Stack),
 yeccgoto_sigParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_257(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_257_(Stack),
 yeccgoto_signalType(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_258(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_258_(Stack),
 yeccgoto_signalType(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_259(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_259_(Stack),
 yeccgoto_signalType(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_260: see yeccpars2_4

yeccpars2_261(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_261_(Stack),
 yeccgoto_requestID(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_262(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_262_(Stack),
 yeccgoto_sigParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_263(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 264, Ss, Stack, T, Ts, Tzr).

yeccpars2_264(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 266, Ss, Stack, T, Ts, Tzr);
yeccpars2_264(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 267, Ss, Stack, T, Ts, Tzr);
yeccpars2_264(S, 'IterationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 268, Ss, Stack, T, Ts, Tzr);
yeccpars2_264(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 269, Ss, Stack, T, Ts, Tzr);
yeccpars2_264(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 270, Ss, Stack, T, Ts, Tzr).

yeccpars2_265(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 272, Ss, Stack, T, Ts, Tzr);
yeccpars2_265(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_265_(Stack),
 yeccpars2_271(271, Cat, [265 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_266(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_266_(Stack),
 yeccgoto_notificationReason(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_267(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_267_(Stack),
 yeccgoto_notificationReason(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_268(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_268_(Stack),
 yeccgoto_notificationReason(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_269(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_269_(Stack),
 yeccgoto_notificationReason(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_270(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_270_(Stack),
 yeccgoto_notificationReason(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_271(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 275, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_272: see yeccpars2_264

yeccpars2_273(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 272, Ss, Stack, T, Ts, Tzr);
yeccpars2_273(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_273_(Stack),
 yeccpars2_274(_S, Cat, [273 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_274(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_274_(Stack),
 yeccgoto_notificationReasons(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_275(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_275_(Stack),
 yeccgoto_sigParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_276: see yeccpars2_4

yeccpars2_277(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_277_(Stack),
 yeccgoto_sigParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_278: see yeccpars2_4

yeccpars2_279(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_279_(Stack),
 yeccgoto_sigParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_280(S, 'BothToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 282, Ss, Stack, T, Ts, Tzr);
yeccpars2_280(S, 'ExternalToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 283, Ss, Stack, T, Ts, Tzr);
yeccpars2_280(S, 'InternalToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 284, Ss, Stack, T, Ts, Tzr).

yeccpars2_281(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_281_(Stack),
 yeccgoto_sigParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_282(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_282_(Stack),
 yeccgoto_direction(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_283(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_283_(Stack),
 yeccgoto_direction(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_284(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_284_(Stack),
 yeccgoto_direction(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_285(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_285_(Stack),
 yeccgoto_sigParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_286(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 297, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 298, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'QuotedChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 292, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_287(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(S, 'QuotedChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 292, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_288: see yeccpars2_287

%% yeccpars2_289: see yeccpars2_287

yeccpars2_290(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_290_(Stack),
 yeccgoto_parmValue(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_291(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_291_(Stack),
 yeccgoto_value(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_292(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_292_(Stack),
 yeccgoto_value(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_293(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_293_(Stack),
 yeccgoto_parmValue(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_294(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_294_(Stack),
 yeccgoto_parmValue(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_295(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_295_(Stack),
 yeccgoto_alternativeValue(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_296(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_296_(Stack),
 yeccgoto_parmValue(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_297: see yeccpars2_287

%% yeccpars2_298: see yeccpars2_287

yeccpars2_299(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 301, Ss, Stack, T, Ts, Tzr);
yeccpars2_299(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 302, Ss, Stack, T, Ts, Tzr);
yeccpars2_299(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_299_(Stack),
 yeccpars2_300(300, Cat, [299 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_300(S, 'RSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 307, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_301: see yeccpars2_287

%% yeccpars2_302: see yeccpars2_287

yeccpars2_303(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 302, Ss, Stack, T, Ts, Tzr);
yeccpars2_303(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_303_(Stack),
 yeccpars2_304(_S, Cat, [303 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_304(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_304_(Stack),
 yeccgoto_valueList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_305(S, 'RSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 306, Ss, Stack, T, Ts, Tzr).

yeccpars2_306(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_306_(Stack),
 yeccgoto_alternativeValue(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_307(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_307_(Stack),
 yeccgoto_alternativeValue(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_308(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 302, Ss, Stack, T, Ts, Tzr);
yeccpars2_308(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_308_(Stack),
 yeccpars2_309(309, Cat, [308 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_309(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 310, Ss, Stack, T, Ts, Tzr).

yeccpars2_310(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_310_(Stack),
 yeccgoto_alternativeValue(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_311(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 315, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_312: see yeccpars2_242

yeccpars2_313(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 312, Ss, Stack, T, Ts, Tzr);
yeccpars2_313(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_313_(Stack),
 yeccpars2_314(_S, Cat, [313 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_314(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_314_(Stack),
 yeccgoto_sigParameters(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_315(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_315_(Stack),
 yeccgoto_signalRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_316: see yeccpars2_4

yeccpars2_317(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_317_(Stack),
 yeccgoto_packagesItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_318(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 319, Ss, Stack, T, Ts, Tzr).

yeccpars2_319(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_319_(Stack),
 yeccgoto_indAudpackagesDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_320(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 330, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 331, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 332, Ss, Stack, T, Ts, Tzr);
yeccpars2_320(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_320(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_cont_320(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 327, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_320(S, 'LocalDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 328, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_320(S, 'RemoteDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 329, Ss, Stack, T, Ts, Tzr).

yeccpars2_321(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_321_(Stack),
 yeccgoto_indAudmediaParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_322(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_322_(Stack),
 yeccgoto_indAudmediaParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_323(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_323_(Stack),
 yeccgoto_indAudmediaParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_324(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_324_(Stack),
 yeccgoto_indAudstreamParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_325(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 376, Ss, Stack, T, Ts, Tzr);
yeccpars2_325(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_325_(Stack),
 yeccpars2_375(375, Cat, [325 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_326(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_326_(Stack),
 yeccgoto_indAudstreamParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_327(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 354, Ss, Stack, T, Ts, Tzr).

yeccpars2_328(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_328_(Stack),
 yeccgoto_indAudstreamParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_329(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_329_(Stack),
 yeccgoto_indAudstreamParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_330(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 221, Ss, Stack, T, Ts, Tzr).

yeccpars2_331(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 349, Ss, Stack, T, Ts, Tzr).

yeccpars2_332(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 333, Ss, Stack, T, Ts, Tzr).

yeccpars2_333(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 338, Ss, Stack, T, Ts, Tzr);
yeccpars2_333(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_333(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_333(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_333(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_333(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_333(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_333(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_333(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_333(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_333(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_333(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_333(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_333(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_333(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_333(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 339, Ss, Stack, T, Ts, Tzr);
yeccpars2_333(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_333(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_333(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_333(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_333(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_334(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_334_(Stack),
 yeccgoto_indAudterminationStateParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_335(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 286, Ss, Stack, T, Ts, Tzr);
yeccpars2_335(S, 'GREATER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 287, Ss, Stack, T, Ts, Tzr);
yeccpars2_335(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 288, Ss, Stack, T, Ts, Tzr);
yeccpars2_335(S, 'NEQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 289, Ss, Stack, T, Ts, Tzr);
yeccpars2_335(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_335_(Stack),
 yeccgoto_indAudterminationStateParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_336(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 347, Ss, Stack, T, Ts, Tzr).

yeccpars2_337(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_indAudterminationStateParm(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_338(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_338_(Stack),
 yeccgoto_indAudterminationStateParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_339(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 340, Ss, Stack, T, Ts, Tzr);
yeccpars2_339(S, 'INEQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 341, Ss, Stack, T, Ts, Tzr);
yeccpars2_339(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_339_(Stack),
 yeccgoto_iaServiceStates(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_340(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 343, Ss, Stack, T, Ts, Tzr);
yeccpars2_340(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 344, Ss, Stack, T, Ts, Tzr);
yeccpars2_340(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 345, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_341: see yeccpars2_340

yeccpars2_342(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_342_(Stack),
 yeccgoto_iaServiceStates(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_343(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_343_(Stack),
 yeccgoto_serviceStatesValue(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_344(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_344_(Stack),
 yeccgoto_serviceStatesValue(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_345_(Stack),
 yeccgoto_serviceStatesValue(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_346(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_346_(Stack),
 yeccgoto_iaServiceStates(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_347(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_347_(Stack),
 yeccgoto_indAudterminationStateDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_348(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_348_(Stack),
 yeccgoto_propertyParm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_349: see yeccpars2_4

yeccpars2_350(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 351, Ss, Stack, T, Ts, Tzr).

yeccpars2_351(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 330, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_320(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_352(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 353, Ss, Stack, T, Ts, Tzr).

yeccpars2_353(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_353_(Stack),
 yeccgoto_indAudstreamDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_354(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_354(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_354(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_354(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_354(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_354(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_354(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_354(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_354(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_354(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 358, Ss, Stack, T, Ts, Tzr);
yeccpars2_354(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_354(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_354(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_354(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_354(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 359, Ss, Stack, T, Ts, Tzr);
yeccpars2_354(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 360, Ss, Stack, T, Ts, Tzr);
yeccpars2_354(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_354(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_354(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_354(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_354(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_354(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_355(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_355_(Stack),
 yeccgoto_indAudlocalParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_356(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 286, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'GREATER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 287, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 288, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'NEQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 289, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_356_(Stack),
 yeccgoto_indAudlocalParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_357(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 371, Ss, Stack, T, Ts, Tzr);
yeccpars2_357(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_357_(Stack),
 yeccpars2_370(370, Cat, [357 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_358(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 361, Ss, Stack, T, Ts, Tzr);
yeccpars2_358(S, 'INEQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 362, Ss, Stack, T, Ts, Tzr);
yeccpars2_358(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_358_(Stack),
 yeccgoto_indAudlocalParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_359(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_359_(Stack),
 yeccgoto_indAudlocalParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_360(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_360_(Stack),
 yeccgoto_indAudlocalParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_361(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 364, Ss, Stack, T, Ts, Tzr);
yeccpars2_361(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 365, Ss, Stack, T, Ts, Tzr);
yeccpars2_361(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 366, Ss, Stack, T, Ts, Tzr);
yeccpars2_361(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 367, Ss, Stack, T, Ts, Tzr);
yeccpars2_361(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 368, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_362: see yeccpars2_361

yeccpars2_363(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_363_(Stack),
 yeccgoto_indAudlocalParm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_364(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_364_(Stack),
 yeccgoto_streamModes(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_365(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_365_(Stack),
 yeccgoto_streamModes(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_366(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_(Stack),
 yeccgoto_streamModes(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_367(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_367_(Stack),
 yeccgoto_streamModes(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_368(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_368_(Stack),
 yeccgoto_streamModes(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_369(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_369_(Stack),
 yeccgoto_indAudlocalParm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_370(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 374, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_371: see yeccpars2_354

yeccpars2_372(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 371, Ss, Stack, T, Ts, Tzr);
yeccpars2_372(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_372_(Stack),
 yeccpars2_373(_S, Cat, [372 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_373(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_373_(Stack),
 yeccgoto_indAudlocalParmList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_374(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_374_(Stack),
 yeccgoto_indAudlocalControlDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_375(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 379, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_376: see yeccpars2_320

yeccpars2_377(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 376, Ss, Stack, T, Ts, Tzr);
yeccpars2_377(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_377_(Stack),
 yeccpars2_378(_S, Cat, [377 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_378(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_378_(Stack),
 yeccgoto_indAudmediaParms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_379(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_379_(Stack),
 yeccgoto_indAudmediaDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_380: see yeccpars2_4

%% yeccpars2_381: see yeccpars2_4

yeccpars2_382(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_indAudrequestedEvent(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_383(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 384, Ss, Stack, T, Ts, Tzr).

yeccpars2_384(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_384_(Stack),
 yeccgoto_indAudeventsDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_385(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 386, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_386: see yeccpars2_4

yeccpars2_387(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 388, Ss, Stack, T, Ts, Tzr).

yeccpars2_388(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_388_(Stack),
 yeccgoto_indAudeventsDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_389: see yeccpars2_4

yeccpars2_390(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 394, Ss, Stack, T, Ts, Tzr);
yeccpars2_390(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_390_(Stack),
 yeccpars2_393(_S, Cat, [390 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_391(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 392, Ss, Stack, T, Ts, Tzr).

yeccpars2_392(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_392_(Stack),
 yeccgoto_indAudeventBufferDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_393(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_393_(Stack),
 yeccgoto_indAudeventSpec(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_394(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_394(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_394(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_394(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_394(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_394(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_394(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_394(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_394(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_394(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_394(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_394(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_394(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_394(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_394(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_394(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_394(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_394(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_394(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_395(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_395_(Stack),
 yeccgoto_eventParameterName(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_396(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 399, Ss, Stack, T, Ts, Tzr).

yeccpars2_397(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_397_(Stack),
 yeccgoto_indAudeventSpecParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_398(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_398_(Stack),
 yeccgoto_indAudeventSpecParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_399(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_399_(Stack),
 yeccgoto_optIndAudeventSpecParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_400(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_400_(Stack),
 yeccgoto_auditDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_401(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_401_(Stack),
 yeccgoto_auditDescriptorBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_402(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 210, Ss, Stack, T, Ts, Tzr);
yeccpars2_402(S, 'DigitMapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 211, Ss, Stack, T, Ts, Tzr);
yeccpars2_402(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 212, Ss, Stack, T, Ts, Tzr);
yeccpars2_402(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 213, Ss, Stack, T, Ts, Tzr);
yeccpars2_402(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 214, Ss, Stack, T, Ts, Tzr);
yeccpars2_402(S, 'ModemToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 215, Ss, Stack, T, Ts, Tzr);
yeccpars2_402(S, 'MuxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 216, Ss, Stack, T, Ts, Tzr);
yeccpars2_402(S, 'ObservedEventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 217, Ss, Stack, T, Ts, Tzr);
yeccpars2_402(S, 'PackagesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 218, Ss, Stack, T, Ts, Tzr);
yeccpars2_402(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 219, Ss, Stack, T, Ts, Tzr);
yeccpars2_402(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 220, Ss, Stack, T, Ts, Tzr).

yeccpars2_403(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 402, Ss, Stack, T, Ts, Tzr);
yeccpars2_403(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_403_(Stack),
 yeccpars2_404(_S, Cat, [403 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_404(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_404_(Stack),
 yeccgoto_auditItemList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_405(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_405_(Stack),
 yeccgoto_indAudterminationAudit(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_406(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 210, Ss, Stack, T, Ts, Tzr);
yeccpars2_406(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 408, Ss, Stack, T, Ts, Tzr);
yeccpars2_406(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 409, Ss, Stack, T, Ts, Tzr);
yeccpars2_406(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 410, Ss, Stack, T, Ts, Tzr);
yeccpars2_406(S, 'PackagesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 411, Ss, Stack, T, Ts, Tzr);
yeccpars2_406(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 412, Ss, Stack, T, Ts, Tzr);
yeccpars2_406(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 330, Ss, Stack, T, Ts, Tzr).

yeccpars2_407(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 406, Ss, Stack, T, Ts, Tzr);
yeccpars2_407(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_407_(Stack),
 yeccpars2_413(_S, Cat, [407 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_408(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 389, Ss, Stack, T, Ts, Tzr).

yeccpars2_409(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 380, Ss, Stack, T, Ts, Tzr);
yeccpars2_409(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 381, Ss, Stack, T, Ts, Tzr).

yeccpars2_410(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 320, Ss, Stack, T, Ts, Tzr).

yeccpars2_411(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 316, Ss, Stack, T, Ts, Tzr).

yeccpars2_412(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 226, Ss, Stack, T, Ts, Tzr).

yeccpars2_413(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_413_(Stack),
 yeccgoto_indAudterminationAuditList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_414(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_414_(Stack),
 yeccgoto_optAuditDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_415: see yeccpars2_183

yeccpars2_416(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 417, Ss, Stack, T, Ts, Tzr).

yeccpars2_417(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 419, Ss, Stack, T, Ts, Tzr).

yeccpars2_418(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 464, Ss, Stack, T, Ts, Tzr).

yeccpars2_419(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 420, Ss, Stack, T, Ts, Tzr).

yeccpars2_420(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 434, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 210, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'DigitMapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 211, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 212, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 213, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 214, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 435, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 436, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'ModemToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 215, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'MuxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 216, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'ObservedEventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 217, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'PackagesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 218, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 437, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 438, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 439, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'ServiceChangeIncompleteToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 440, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 219, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 220, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 441, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 442, Ss, Stack, T, Ts, Tzr);
yeccpars2_420(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_421(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_421_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_422(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_422_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_423(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_423_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_424(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_424_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_425(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 460, Ss, Stack, T, Ts, Tzr);
yeccpars2_425(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_425_(Stack),
 yeccpars2_459(459, Cat, [425 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_426(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_426_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_427(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_427_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_428(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_428_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_429(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_429_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_430(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_430_(Stack),
 yeccgoto_extensionParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_431: see yeccpars2_244

yeccpars2_432(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_432_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_433(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_433_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_434(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 456, Ss, Stack, T, Ts, Tzr);
yeccpars2_434(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_435(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 454, Ss, Stack, T, Ts, Tzr);
yeccpars2_435(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_436(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 452, Ss, Stack, T, Ts, Tzr);
yeccpars2_436(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_437(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 450, Ss, Stack, T, Ts, Tzr);
yeccpars2_437(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_438(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 448, Ss, Stack, T, Ts, Tzr);
yeccpars2_438(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_439(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 445, Ss, Stack, T, Ts, Tzr);
yeccpars2_439(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_440(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_440_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_441(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_441_(Stack),
 yeccgoto_timeStamp(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_442(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 443, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_443: see yeccpars2_4

yeccpars2_444(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_444_(Stack),
 yeccgoto_serviceChangeVersion(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_445(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'MessageSegmentToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'SegmentationCompleteToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_445_(Stack),
 yeccpars2_91(91, Cat, [445 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_446(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_446_(Stack),
 yeccgoto_serviceChangeAddress(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_447(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_447_(Stack),
 yeccgoto_serviceChangeAddress(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_448: see yeccpars2_287

yeccpars2_449(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_449_(Stack),
 yeccgoto_serviceChangeReason(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_450: see yeccpars2_4

yeccpars2_451(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_451_(Stack),
 yeccgoto_serviceChangeProfile(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_452(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_452(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_452(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_452(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_452_(Stack),
 yeccpars2_91(91, Cat, [452 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_453(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_453_(Stack),
 yeccgoto_serviceChangeMgcId(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_454: see yeccpars2_4

yeccpars2_455(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_455_(Stack),
 yeccgoto_serviceChangeMethod(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_456: see yeccpars2_4

yeccpars2_457(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_457_(Stack),
 yeccgoto_serviceChangeDelay(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_458(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_458_(Stack),
 yeccgoto_extension(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_459(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 463, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_460: see yeccpars2_420

yeccpars2_461(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 460, Ss, Stack, T, Ts, Tzr);
yeccpars2_461(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_461_(Stack),
 yeccpars2_462(_S, Cat, [461 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_462(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_462_(Stack),
 yeccgoto_serviceChangeParms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_463(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_463_(Stack),
 yeccgoto_serviceChangeDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_464(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_464_(Stack),
 yeccgoto_serviceChangeRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_465: see yeccpars2_4

yeccpars2_466(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_466_(Stack),
 yeccgoto_priority(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_467: see yeccpars2_183

yeccpars2_468(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 469, Ss, Stack, T, Ts, Tzr).

yeccpars2_469(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_469(S, 'ObservedEventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 473, Ss, Stack, T, Ts, Tzr).

yeccpars2_470(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_470_(Stack),
 yeccgoto_notifyRequestBody(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_471(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 502, Ss, Stack, T, Ts, Tzr).

yeccpars2_472(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_472_(Stack),
 yeccgoto_notifyRequestBody(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_473(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 474, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_474: see yeccpars2_4

yeccpars2_475(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 476, Ss, Stack, T, Ts, Tzr).

yeccpars2_476(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_476(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 441, Ss, Stack, T, Ts, Tzr);
yeccpars2_476(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_476_(Stack),
 yeccpars2_4(478, Cat, [476 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_477(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_477(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_477_(Stack),
 yeccpars2_497(497, Cat, [477 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_478: see yeccpars2_4

yeccpars2_479(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 481, Ss, Stack, T, Ts, Tzr);
yeccpars2_479(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_479_(Stack),
 yeccpars2_480(480, Cat, [479 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_480(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 484, Ss, Stack, T, Ts, Tzr).

yeccpars2_481(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_481(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 441, Ss, Stack, T, Ts, Tzr);
yeccpars2_481(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_481_(Stack),
 yeccpars2_4(478, Cat, [481 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_482(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 481, Ss, Stack, T, Ts, Tzr);
yeccpars2_482(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_482_(Stack),
 yeccpars2_483(_S, Cat, [482 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_483(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_483_(Stack),
 yeccgoto_observedEvents(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_484(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_484_(Stack),
 yeccgoto_observedEventsDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_485(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 487, Ss, Stack, T, Ts, Tzr);
yeccpars2_485(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_485_(Stack),
 yeccpars2_486(_S, Cat, [485 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_486(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_486_(Stack),
 yeccgoto_observedEvent(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_487: see yeccpars2_4

yeccpars2_488(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 493, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_488_(Stack),
 yeccpars2_492(492, Cat, [488 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_489(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_observedEventParameter(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_490: see yeccpars2_244

yeccpars2_491(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_491_(Stack),
 yeccgoto_eventStreamOrOther(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_492(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 496, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_493: see yeccpars2_4

yeccpars2_494(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 493, Ss, Stack, T, Ts, Tzr);
yeccpars2_494(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_494_(Stack),
 yeccpars2_495(_S, Cat, [494 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_495(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_495_(Stack),
 yeccgoto_observedEventParameters(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_496(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_496_(Stack),
 yeccgoto_observedEventBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_497(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 498, Ss, Stack, T, Ts, Tzr).

yeccpars2_498(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_498(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_498_(Stack),
 yeccpars2_4(499, Cat, [498 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_499: see yeccpars2_4

yeccpars2_500(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 487, Ss, Stack, T, Ts, Tzr);
yeccpars2_500(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_500_(Stack),
 yeccpars2_501(_S, Cat, [500 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_501(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_501_(Stack),
 yeccgoto_observedEvent(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_502(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_502_(Stack),
 yeccgoto_notifyRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_503(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 505, Ss, Stack, T, Ts, Tzr);
yeccpars2_503(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 506, Ss, Stack, T, Ts, Tzr).

yeccpars2_504(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_504_(Stack),
 yeccgoto_iepsValue(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_505(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_505_(Stack),
 yeccgoto_onOrOff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_506(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_506_(Stack),
 yeccgoto_onOrOff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_507(S, 'AndAUDITselectToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 517, Ss, Stack, T, Ts, Tzr);
yeccpars2_507(S, 'ContextAttrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 518, Ss, Stack, T, Ts, Tzr);
yeccpars2_507(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_507(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_507(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_507(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_507(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_507(S, 'EmergencyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 519, Ss, Stack, T, Ts, Tzr);
yeccpars2_507(S, 'EmergencyValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 520, Ss, Stack, T, Ts, Tzr);
yeccpars2_507(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_507(S, 'IEPSToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 521, Ss, Stack, T, Ts, Tzr);
yeccpars2_507(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_507(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_507(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_507(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_507(S, 'OrAUDITselectToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 522, Ss, Stack, T, Ts, Tzr);
yeccpars2_507(S, 'PriorityToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 523, Ss, Stack, T, Ts, Tzr);
yeccpars2_507(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_507(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_507(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_507(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_507(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_507(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_507(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_507(S, 'TopologyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 524, Ss, Stack, T, Ts, Tzr);
yeccpars2_507(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_507(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_508(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_508_(Stack),
 yeccgoto_contextAuditSelector(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_509(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_509_(Stack),
 yeccgoto_contextAuditProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_510(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 557, Ss, Stack, T, Ts, Tzr).

yeccpars2_511(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_511_(Stack),
 yeccgoto_contextAuditSelector(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_512(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_512_(Stack),
 yeccgoto_contextAuditSelector(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_513(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_contextAuditProperty(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_514(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 544, Ss, Stack, T, Ts, Tzr);
yeccpars2_514(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_514_(Stack),
 yeccpars2_555(555, Cat, [514 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_515(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_contextAuditSelector(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_516(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_516_(Stack),
 yeccgoto_contextAuditSelector(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_517(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_517_(Stack),
 yeccgoto_auditSelectLogic(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_518(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 528, Ss, Stack, T, Ts, Tzr).

yeccpars2_519(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_519_(Stack),
 yeccgoto_contextAuditProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_520(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 525, Ss, Stack, T, Ts, Tzr).

yeccpars2_521(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 503, Ss, Stack, T, Ts, Tzr);
yeccpars2_521(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_521_(Stack),
 yeccgoto_contextAuditProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_522(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_522_(Stack),
 yeccgoto_auditSelectLogic(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_523(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 465, Ss, Stack, T, Ts, Tzr);
yeccpars2_523(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_523_(Stack),
 yeccgoto_contextAuditProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_524(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_524_(Stack),
 yeccgoto_contextAuditProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_525(S, 'EmergencyOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 526, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'EmergencyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 527, Ss, Stack, T, Ts, Tzr).

yeccpars2_526(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_526_(Stack),
 yeccgoto_emergencyValue(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_527(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_527_(Stack),
 yeccgoto_emergencyValue(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_528(S, 'AndAUDITselectToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 517, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'ContextAttrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 151, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'ContextListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 534, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'EmergencyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 519, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'EmergencyValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 520, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'IEPSToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 521, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'OrAUDITselectToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 522, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'PriorityToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 523, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'TopologyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 524, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_529(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 554, Ss, Stack, T, Ts, Tzr).

yeccpars2_530(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 550, Ss, Stack, T, Ts, Tzr);
yeccpars2_530(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_530_(Stack),
 yeccpars2_549(_S, Cat, [530 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_531(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 286, Ss, Stack, T, Ts, Tzr);
yeccpars2_531(S, 'GREATER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 287, Ss, Stack, T, Ts, Tzr);
yeccpars2_531(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 288, Ss, Stack, T, Ts, Tzr);
yeccpars2_531(S, 'NEQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 289, Ss, Stack, T, Ts, Tzr);
yeccpars2_531(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_531_(Stack),
 yeccgoto_contextAuditProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_532(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 548, Ss, Stack, T, Ts, Tzr).

yeccpars2_533(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 544, Ss, Stack, T, Ts, Tzr);
yeccpars2_533(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_533_(Stack),
 yeccpars2_543(543, Cat, [533 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_534(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 535, Ss, Stack, T, Ts, Tzr).

yeccpars2_535(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 536, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_536: see yeccpars2_4

yeccpars2_537(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 539, Ss, Stack, T, Ts, Tzr);
yeccpars2_537(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_537_(Stack),
 yeccpars2_538(538, Cat, [537 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_538(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 542, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_539: see yeccpars2_4

yeccpars2_540(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 539, Ss, Stack, T, Ts, Tzr);
yeccpars2_540(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_540_(Stack),
 yeccpars2_541(_S, Cat, [540 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_541(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_541_(Stack),
 yeccgoto_contextIDs(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_542(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_542_(Stack),
 yeccgoto_contextIdList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_543(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 547, Ss, Stack, T, Ts, Tzr).

yeccpars2_544(S, 'AndAUDITselectToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 517, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(S, 'ContextAttrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 151, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(S, 'EmergencyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 519, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(S, 'EmergencyValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 520, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(S, 'IEPSToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 521, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(S, 'OrAUDITselectToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 522, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(S, 'PriorityToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 523, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(S, 'TopologyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 524, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_545(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 544, Ss, Stack, T, Ts, Tzr);
yeccpars2_545(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_545_(Stack),
 yeccpars2_546(_S, Cat, [545 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_546(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_546_(Stack),
 yeccgoto_contextAuditProperties(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_547(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_547_(Stack),
 yeccgoto_indAudcontextAttrDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_548(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_548_(Stack),
 yeccgoto_contextAttrDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_549(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_549_(Stack),
 yeccgoto_propertyParms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_550: see yeccpars2_4

yeccpars2_551(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 550, Ss, Stack, T, Ts, Tzr);
yeccpars2_551(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_551_(Stack),
 yeccpars2_553(_S, Cat, [551 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_552: see yeccpars2_244

yeccpars2_553(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_553_(Stack),
 yeccgoto_propertyParmList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_554(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_554_(Stack),
 yeccgoto_contextAttrDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_555(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 556, Ss, Stack, T, Ts, Tzr).

yeccpars2_556(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_556_(Stack),
 yeccgoto_contextAudit(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_557(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_557_(Stack),
 yeccgoto_contextAudit(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_558(S, 'ContextListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 534, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_559: see yeccpars2_183

yeccpars2_560(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_560(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_560_(Stack),
 yeccpars2_561(_S, Cat, [560 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_561(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_561_(Stack),
 yeccgoto_auditRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_562: see yeccpars2_183

yeccpars2_563(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr);
yeccpars2_563(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_563_(Stack),
 yeccpars2_564(_S, Cat, [563 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_564(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_564_(Stack),
 yeccgoto_auditRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_565(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_565_(Stack),
 yeccgoto_actionRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_566(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_566_(Stack),
 yeccgoto_actionRequestBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_567: see yeccpars2_132

yeccpars2_568(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 567, Ss, Stack, T, Ts, Tzr);
yeccpars2_568(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_568_(Stack),
 yeccpars2_569(_S, Cat, [568 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_569(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_569_(Stack),
 yeccgoto_actionRequestItems(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_570: see yeccpars2_183

yeccpars2_571(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 573, Ss, Stack, T, Ts, Tzr);
yeccpars2_571(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_571_(Stack),
 yeccpars2_572(_S, Cat, [571 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_572(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_572_(Stack),
 yeccgoto_ammRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_573(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_573(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 584, Ss, Stack, T, Ts, Tzr);
yeccpars2_573(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 585, Ss, Stack, T, Ts, Tzr);
yeccpars2_573(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 586, Ss, Stack, T, Ts, Tzr);
yeccpars2_573(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 587, Ss, Stack, T, Ts, Tzr);
yeccpars2_573(S, 'ModemToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 588, Ss, Stack, T, Ts, Tzr);
yeccpars2_573(S, 'MuxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 589, Ss, Stack, T, Ts, Tzr);
yeccpars2_573(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 590, Ss, Stack, T, Ts, Tzr);
yeccpars2_573(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 591, Ss, Stack, T, Ts, Tzr).

yeccpars2_574(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_574_(Stack),
 yeccgoto_ammParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_575(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_575_(Stack),
 yeccgoto_ammParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_576(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_576_(Stack),
 yeccgoto_ammParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_577(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_577_(Stack),
 yeccgoto_ammParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_578(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_578_(Stack),
 yeccgoto_ammParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_579(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_579_(Stack),
 yeccgoto_ammParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_580(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_580_(Stack),
 yeccgoto_ammParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_581(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_581_(Stack),
 yeccgoto_ammParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_582(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_582_(Stack),
 yeccgoto_ammParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_583(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 793, Ss, Stack, T, Ts, Tzr);
yeccpars2_583(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_583_(Stack),
 yeccpars2_792(792, Cat, [583 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_584(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_584_(Stack),
 yeccgoto_digitMapDescriptor(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_585(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 784, Ss, Stack, T, Ts, Tzr);
yeccpars2_585(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_585_(Stack),
 yeccgoto_eventBufferDescriptor(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_586(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 710, Ss, Stack, T, Ts, Tzr);
yeccpars2_586(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_586_(Stack),
 yeccgoto_eventsDescriptor(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_587(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 649, Ss, Stack, T, Ts, Tzr).

yeccpars2_588(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 633, Ss, Stack, T, Ts, Tzr);
yeccpars2_588(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 634, Ss, Stack, T, Ts, Tzr).

yeccpars2_589(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 625, Ss, Stack, T, Ts, Tzr).

yeccpars2_590(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 606, Ss, Stack, T, Ts, Tzr);
yeccpars2_590(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_590_(Stack),
 yeccgoto_signalsDescriptor(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_591(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 592, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_592: see yeccpars2_4

yeccpars2_593(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 602, Ss, Stack, T, Ts, Tzr);
yeccpars2_593(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_593_(Stack),
 yeccpars2_601(601, Cat, [593 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_594(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 595, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_594_(Stack),
 yeccgoto_statisticsParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_595(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_595(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_595(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_595(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_595(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_595(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_595(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_595(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 597, Ss, Stack, T, Ts, Tzr);
yeccpars2_595(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_595(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_595(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_595(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_595(S, 'QuotedChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 292, Ss, Stack, T, Ts, Tzr);
yeccpars2_595(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_595(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_595(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_595(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_595(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_595(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_595(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_595(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_596(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_596_(Stack),
 yeccgoto_statisticsParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_597: see yeccpars2_287

yeccpars2_598(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 302, Ss, Stack, T, Ts, Tzr);
yeccpars2_598(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_598_(Stack),
 yeccpars2_599(599, Cat, [598 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_599(S, 'RSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 600, Ss, Stack, T, Ts, Tzr).

yeccpars2_600(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_600_(Stack),
 yeccgoto_statisticsParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_601(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 605, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_602: see yeccpars2_4

yeccpars2_603(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 602, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_603_(Stack),
 yeccpars2_604(_S, Cat, [603 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_604(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_604_(Stack),
 yeccgoto_statisticsParameters(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_605(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_605_(Stack),
 yeccgoto_statisticsDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_606(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_606(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_606(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_606(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_606(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_606(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_606(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_606(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_606(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_606(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_606(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_606(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_606(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_606(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_606(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 610, Ss, Stack, T, Ts, Tzr);
yeccpars2_606(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_606(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_606(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_606(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_607(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_607_(Stack),
 yeccgoto_signalParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_608(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 621, Ss, Stack, T, Ts, Tzr);
yeccpars2_608(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_608_(Stack),
 yeccpars2_620(620, Cat, [608 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_609(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_609_(Stack),
 yeccgoto_signalParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_610(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 611, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_611: see yeccpars2_4

yeccpars2_612(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 613, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_613: see yeccpars2_4

yeccpars2_614(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 616, Ss, Stack, T, Ts, Tzr);
yeccpars2_614(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_614_(Stack),
 yeccpars2_615(615, Cat, [614 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_615(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 619, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_616: see yeccpars2_4

yeccpars2_617(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 616, Ss, Stack, T, Ts, Tzr);
yeccpars2_617(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_617_(Stack),
 yeccpars2_618(_S, Cat, [617 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_618(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_618_(Stack),
 yeccgoto_signalListParms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_619(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_619_(Stack),
 yeccgoto_signalList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_620(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 624, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_621: see yeccpars2_606

yeccpars2_622(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 621, Ss, Stack, T, Ts, Tzr);
yeccpars2_622(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_622_(Stack),
 yeccpars2_623(_S, Cat, [622 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_623(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_623_(Stack),
 yeccgoto_signalParms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_624(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_624_(Stack),
 yeccgoto_signalsDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_625: see yeccpars2_4

yeccpars2_626(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_626_(Stack),
 yeccgoto_muxType(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_627(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 629, Ss, Stack, T, Ts, Tzr).

yeccpars2_628(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_628_(Stack),
 yeccgoto_muxDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_629: see yeccpars2_4

yeccpars2_630(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_630(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_630_(Stack),
 yeccpars2_631(631, Cat, [630 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_631(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 632, Ss, Stack, T, Ts, Tzr).

yeccpars2_632(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_632_(Stack),
 yeccgoto_terminationIDList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_633: see yeccpars2_4

%% yeccpars2_634: see yeccpars2_4

yeccpars2_635(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_635_(Stack),
 yeccgoto_modemType(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_636(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 638, Ss, Stack, T, Ts, Tzr);
yeccpars2_636(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_636_(Stack),
 yeccpars2_637(637, Cat, [636 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_637(S, 'RSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 641, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_638: see yeccpars2_4

yeccpars2_639(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 638, Ss, Stack, T, Ts, Tzr);
yeccpars2_639(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_639_(Stack),
 yeccpars2_640(_S, Cat, [639 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_640(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_640_(Stack),
 yeccgoto_modemTypeList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_641(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 643, Ss, Stack, T, Ts, Tzr);
yeccpars2_641(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_641_(Stack),
 yeccpars2_642(_S, Cat, [641 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_642(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_642_(Stack),
 yeccgoto_modemDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_643: see yeccpars2_4

yeccpars2_644(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 550, Ss, Stack, T, Ts, Tzr);
yeccpars2_644(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_644_(Stack),
 yeccpars2_645(645, Cat, [644 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_645(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 646, Ss, Stack, T, Ts, Tzr).

yeccpars2_646(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_646_(Stack),
 yeccgoto_optPropertyParms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_647(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 643, Ss, Stack, T, Ts, Tzr);
yeccpars2_647(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_647_(Stack),
 yeccpars2_648(_S, Cat, [647 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_648(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_648_(Stack),
 yeccgoto_modemDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_649(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 591, Ss, Stack, T, Ts, Tzr);
yeccpars2_649(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 659, Ss, Stack, T, Ts, Tzr);
yeccpars2_649(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 660, Ss, Stack, T, Ts, Tzr);
yeccpars2_649(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_649(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_cont_649(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 656, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_649(S, 'LocalDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 657, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_649(S, 'RemoteDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 658, Ss, Stack, T, Ts, Tzr).

yeccpars2_650(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_650_(Stack),
 yeccgoto_mediaParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_651(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_651_(Stack),
 yeccgoto_mediaParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_652(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_652_(Stack),
 yeccgoto_mediaParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_653(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_653_(Stack),
 yeccgoto_streamParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_654(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 706, Ss, Stack, T, Ts, Tzr);
yeccpars2_654(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_654_(Stack),
 yeccpars2_705(705, Cat, [654 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_655(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_655_(Stack),
 yeccgoto_streamParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_656(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 688, Ss, Stack, T, Ts, Tzr).

yeccpars2_657(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_657_(Stack),
 yeccgoto_streamParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_658(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_658_(Stack),
 yeccgoto_streamParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_659(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 679, Ss, Stack, T, Ts, Tzr).

yeccpars2_660(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 661, Ss, Stack, T, Ts, Tzr).

yeccpars2_661(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 666, Ss, Stack, T, Ts, Tzr);
yeccpars2_661(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_661(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_661(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_661(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_661(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_661(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_661(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_661(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_661(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_661(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_661(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_661(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_661(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_661(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_661(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 667, Ss, Stack, T, Ts, Tzr);
yeccpars2_661(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_661(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_661(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_661(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_661(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_662(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 675, Ss, Stack, T, Ts, Tzr);
yeccpars2_662(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_662_(Stack),
 yeccpars2_674(674, Cat, [662 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_663(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_663_(Stack),
 yeccgoto_terminationStateParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_664(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_664_(Stack),
 yeccgoto_terminationStateParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_665(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_665_(Stack),
 yeccgoto_terminationStateParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_666(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 670, Ss, Stack, T, Ts, Tzr).

yeccpars2_667(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 668, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_668: see yeccpars2_340

yeccpars2_669(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_669_(Stack),
 yeccgoto_serviceStates(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_670(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 672, Ss, Stack, T, Ts, Tzr);
yeccpars2_670(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 673, Ss, Stack, T, Ts, Tzr).

yeccpars2_671(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_671_(Stack),
 yeccgoto_eventBufferControl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_672(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_672_(Stack),
 yeccgoto_eventBufferControlValue(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_673(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_673_(Stack),
 yeccgoto_eventBufferControlValue(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_674(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 678, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_675: see yeccpars2_661

yeccpars2_676(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 675, Ss, Stack, T, Ts, Tzr);
yeccpars2_676(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_676_(Stack),
 yeccpars2_677(_S, Cat, [676 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_677(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_677_(Stack),
 yeccgoto_terminationStateParms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_678(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_678_(Stack),
 yeccgoto_terminationStateDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_679: see yeccpars2_4

yeccpars2_680(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 681, Ss, Stack, T, Ts, Tzr).

yeccpars2_681(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 591, Ss, Stack, T, Ts, Tzr);
yeccpars2_681(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_649(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_682(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 684, Ss, Stack, T, Ts, Tzr);
yeccpars2_682(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_682_(Stack),
 yeccpars2_683(683, Cat, [682 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_683(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 687, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_684: see yeccpars2_681

yeccpars2_685(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 684, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_685_(Stack),
 yeccpars2_686(_S, Cat, [685 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_686(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_686_(Stack),
 yeccgoto_streamParmList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_687(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_687_(Stack),
 yeccgoto_streamDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_688(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_688(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_688(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_688(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_688(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_688(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_688(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_688(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_688(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_688(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 691, Ss, Stack, T, Ts, Tzr);
yeccpars2_688(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_688(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_688(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_688(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_688(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 692, Ss, Stack, T, Ts, Tzr);
yeccpars2_688(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 693, Ss, Stack, T, Ts, Tzr);
yeccpars2_688(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_688(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_688(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_688(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_688(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_688(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_689(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_689_(Stack),
 yeccgoto_localParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_690(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 701, Ss, Stack, T, Ts, Tzr);
yeccpars2_690(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_690_(Stack),
 yeccpars2_700(700, Cat, [690 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_691(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 698, Ss, Stack, T, Ts, Tzr).

yeccpars2_692(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 696, Ss, Stack, T, Ts, Tzr).

yeccpars2_693(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 694, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_694: see yeccpars2_503

yeccpars2_695(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_695_(Stack),
 yeccgoto_localParm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_696: see yeccpars2_503

yeccpars2_697(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_697_(Stack),
 yeccgoto_localParm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_698: see yeccpars2_361

yeccpars2_699(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_699_(Stack),
 yeccgoto_localParm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_700(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 704, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_701: see yeccpars2_688

yeccpars2_702(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 701, Ss, Stack, T, Ts, Tzr);
yeccpars2_702(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_702_(Stack),
 yeccpars2_703(_S, Cat, [702 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_703(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_703_(Stack),
 yeccgoto_localParmList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_704(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_704_(Stack),
 yeccgoto_localControlDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_705(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 709, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_706: see yeccpars2_649

yeccpars2_707(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 706, Ss, Stack, T, Ts, Tzr);
yeccpars2_707(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_707_(Stack),
 yeccpars2_708(_S, Cat, [707 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_708(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_708_(Stack),
 yeccgoto_mediaParmList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_709(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_709_(Stack),
 yeccgoto_mediaDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_710: see yeccpars2_4

yeccpars2_711(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 712, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_712: see yeccpars2_4

yeccpars2_713(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 780, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_713_(Stack),
 yeccpars2_779(779, Cat, [713 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_714(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 716, Ss, Stack, T, Ts, Tzr);
yeccpars2_714(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_714_(Stack),
 yeccpars2_715(_S, Cat, [714 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_715(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_715_(Stack),
 yeccgoto_requestedEvent(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_716(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_716(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_716(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 724, Ss, Stack, T, Ts, Tzr);
yeccpars2_716(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_716(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_716(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 725, Ss, Stack, T, Ts, Tzr);
yeccpars2_716(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_716(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 726, Ss, Stack, T, Ts, Tzr);
yeccpars2_716(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_716(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_716(S, 'NeverNotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 727, Ss, Stack, T, Ts, Tzr);
yeccpars2_716(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_716(S, 'NotifyImmediateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 728, Ss, Stack, T, Ts, Tzr);
yeccpars2_716(S, 'NotifyRegulatedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 729, Ss, Stack, T, Ts, Tzr);
yeccpars2_716(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_716(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_716(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_716(S, 'ResetEventsDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 730, Ss, Stack, T, Ts, Tzr);
yeccpars2_716(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_716(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_716(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_716(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_716(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_716(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_717(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_717_(Stack),
 yeccgoto_notifyBehaviour(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_718(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_718_(Stack),
 yeccgoto_eventParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_719(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_eventParameter(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_720(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 775, Ss, Stack, T, Ts, Tzr);
yeccpars2_720(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_720_(Stack),
 yeccpars2_774(774, Cat, [720 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_721(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_eventParameter(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_722(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_eventParameter(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_723(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_eventParameter(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_724(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_724_(Stack),
 yeccgoto_eventDM(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_725(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 735, Ss, Stack, T, Ts, Tzr);
yeccpars2_725(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_726(_S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_726_COMMA(Stack),
 yeccgoto_eventParameter(hd(Ss), 'COMMA', Ss, NewStack, T, Ts, Tzr);
yeccpars2_726(_S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_726_RBRKT(Stack),
 yeccgoto_eventParameter(hd(Ss), 'RBRKT', Ss, NewStack, T, Ts, Tzr);
yeccpars2_726(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_727(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_727_(Stack),
 yeccgoto_notifyBehaviour(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_728(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_728_(Stack),
 yeccgoto_notifyBehaviour(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_729(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 731, Ss, Stack, T, Ts, Tzr);
yeccpars2_729(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_729_(Stack),
 yeccgoto_notifyRegulated(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_730(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_730_(Stack),
 yeccgoto_eventParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_731(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 734, Ss, Stack, T, Ts, Tzr).

yeccpars2_732(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 773, Ss, Stack, T, Ts, Tzr).

yeccpars2_733(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 772, Ss, Stack, T, Ts, Tzr).

yeccpars2_734(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 735, Ss, Stack, T, Ts, Tzr).

yeccpars2_735(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 738, Ss, Stack, T, Ts, Tzr);
yeccpars2_735(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 590, Ss, Stack, T, Ts, Tzr).

yeccpars2_736(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 768, Ss, Stack, T, Ts, Tzr);
yeccpars2_736(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 769, Ss, Stack, T, Ts, Tzr).

yeccpars2_737(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 767, Ss, Stack, T, Ts, Tzr).

yeccpars2_738(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 739, Ss, Stack, T, Ts, Tzr);
yeccpars2_738(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_738_(Stack),
 yeccgoto_embedFirst(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_739: see yeccpars2_4

yeccpars2_740(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 741, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_741: see yeccpars2_4

yeccpars2_742(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 763, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_742_(Stack),
 yeccpars2_762(762, Cat, [742 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_743(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 745, Ss, Stack, T, Ts, Tzr);
yeccpars2_743(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_743_(Stack),
 yeccpars2_744(_S, Cat, [743 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_744(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_744_(Stack),
 yeccgoto_secondRequestedEvent(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_745(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_745(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_745(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 724, Ss, Stack, T, Ts, Tzr);
yeccpars2_745(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_745(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_745(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 751, Ss, Stack, T, Ts, Tzr);
yeccpars2_745(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_745(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 752, Ss, Stack, T, Ts, Tzr);
yeccpars2_745(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_745(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_745(S, 'NeverNotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 727, Ss, Stack, T, Ts, Tzr);
yeccpars2_745(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_745(S, 'NotifyImmediateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 728, Ss, Stack, T, Ts, Tzr);
yeccpars2_745(S, 'NotifyRegulatedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 729, Ss, Stack, T, Ts, Tzr);
yeccpars2_745(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_745(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_745(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_745(S, 'ResetEventsDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 753, Ss, Stack, T, Ts, Tzr);
yeccpars2_745(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_745(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_745(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_745(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_745(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_745(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_746(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 758, Ss, Stack, T, Ts, Tzr);
yeccpars2_746(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_746_(Stack),
 yeccpars2_757(757, Cat, [746 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_747(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_747_(Stack),
 yeccgoto_secondEventParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_748(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondEventParameter(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_749(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondEventParameter(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_750(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondEventParameter(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_751(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 754, Ss, Stack, T, Ts, Tzr);
yeccpars2_751(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_752(_S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_752_COMMA(Stack),
 yeccgoto_secondEventParameter(hd(Ss), 'COMMA', Ss, NewStack, T, Ts, Tzr);
yeccpars2_752(_S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_752_RBRKT(Stack),
 yeccgoto_secondEventParameter(hd(Ss), 'RBRKT', Ss, NewStack, T, Ts, Tzr);
yeccpars2_752(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_753(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_753_(Stack),
 yeccgoto_secondEventParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_754(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 590, Ss, Stack, T, Ts, Tzr).

yeccpars2_755(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 756, Ss, Stack, T, Ts, Tzr).

yeccpars2_756(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_756_(Stack),
 yeccgoto_embedSig(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_757(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 761, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_758: see yeccpars2_745

yeccpars2_759(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 758, Ss, Stack, T, Ts, Tzr);
yeccpars2_759(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_759_(Stack),
 yeccpars2_760(_S, Cat, [759 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_760(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_760_(Stack),
 yeccgoto_secondEventParameters(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_761(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_761_(Stack),
 yeccgoto_secondRequestedEventBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_762(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 766, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_763: see yeccpars2_4

yeccpars2_764(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 763, Ss, Stack, T, Ts, Tzr);
yeccpars2_764(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_764_(Stack),
 yeccpars2_765(_S, Cat, [764 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_765(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_765_(Stack),
 yeccgoto_secondRequestedEvents(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_766(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_766_(Stack),
 yeccgoto_embedFirst(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_767(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_767_(Stack),
 yeccgoto_embedNoSig(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_768(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 738, Ss, Stack, T, Ts, Tzr).

yeccpars2_769(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_769_(Stack),
 yeccgoto_embedWithSig(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_770(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 771, Ss, Stack, T, Ts, Tzr).

yeccpars2_771(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_771_(Stack),
 yeccgoto_embedWithSig(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_772(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_772_(Stack),
 yeccgoto_notifyRegulated(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_773(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_773_(Stack),
 yeccgoto_notifyRegulated(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_774(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 778, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_775: see yeccpars2_716

yeccpars2_776(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 775, Ss, Stack, T, Ts, Tzr);
yeccpars2_776(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_776_(Stack),
 yeccpars2_777(_S, Cat, [776 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_777(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_777_(Stack),
 yeccgoto_eventParameters(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_778(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_778_(Stack),
 yeccgoto_requestedEventBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_779(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 783, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_780: see yeccpars2_4

yeccpars2_781(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 780, Ss, Stack, T, Ts, Tzr);
yeccpars2_781(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_781_(Stack),
 yeccpars2_782(_S, Cat, [781 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_782(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_782_(Stack),
 yeccgoto_requestedEvents(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_783(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_783_(Stack),
 yeccgoto_eventsDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_784(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_784(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 441, Ss, Stack, T, Ts, Tzr);
yeccpars2_784(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_784_(Stack),
 yeccpars2_4(478, Cat, [784 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_785(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_785_(Stack),
 yeccgoto_eventSpec(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_786(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 788, Ss, Stack, T, Ts, Tzr);
yeccpars2_786(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_786_(Stack),
 yeccpars2_787(787, Cat, [786 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_787(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 791, Ss, Stack, T, Ts, Tzr).

yeccpars2_788(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_788(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 441, Ss, Stack, T, Ts, Tzr);
yeccpars2_788(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_788_(Stack),
 yeccpars2_4(478, Cat, [788 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_789(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 788, Ss, Stack, T, Ts, Tzr);
yeccpars2_789(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_789_(Stack),
 yeccpars2_790(_S, Cat, [789 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_790(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_790_(Stack),
 yeccgoto_eventSpecList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_791(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_791_(Stack),
 yeccgoto_eventBufferDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_792(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 796, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_793: see yeccpars2_573

yeccpars2_794(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 793, Ss, Stack, T, Ts, Tzr);
yeccpars2_794(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_794_(Stack),
 yeccpars2_795(_S, Cat, [794 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_795(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_795_(Stack),
 yeccgoto_ammParameters(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_796(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_796_(Stack),
 yeccgoto_ammRequestBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_797(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 801, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_798: see yeccpars2_126

yeccpars2_799(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 798, Ss, Stack, T, Ts, Tzr);
yeccpars2_799(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_799_(Stack),
 yeccpars2_800(_S, Cat, [799 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_800(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_800_(Stack),
 yeccgoto_actionRequestList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_801(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_801_(Stack),
 yeccgoto_transactionRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_802(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 808, Ss, Stack, T, Ts, Tzr).

yeccpars2_803(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_803_(Stack),
 yeccgoto_transactionID(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_804: see yeccpars2_126

yeccpars2_805(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 798, Ss, Stack, T, Ts, Tzr);
yeccpars2_805(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_805_(Stack),
 yeccpars2_806(806, Cat, [805 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_806(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 807, Ss, Stack, T, Ts, Tzr).

yeccpars2_807(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_807_(Stack),
 yeccgoto_transactionRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_808: see yeccpars2_126

yeccpars2_809(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 798, Ss, Stack, T, Ts, Tzr);
yeccpars2_809(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_809_(Stack),
 yeccpars2_810(810, Cat, [809 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_810(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 811, Ss, Stack, T, Ts, Tzr).

yeccpars2_811(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_811_(Stack),
 yeccgoto_transactionRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_812: see yeccpars2_4

yeccpars2_813(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 816, Ss, Stack, T, Ts, Tzr);
yeccpars2_813(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_813_(Stack),
 yeccpars2_815(815, Cat, [813 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_814(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_814_(Stack),
 yeccgoto_transactionAck(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_815(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 819, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_816: see yeccpars2_4

yeccpars2_817(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 816, Ss, Stack, T, Ts, Tzr);
yeccpars2_817(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_817_(Stack),
 yeccpars2_818(_S, Cat, [817 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_818(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_818_(Stack),
 yeccgoto_transactionAckList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_819(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_819_(Stack),
 yeccgoto_transactionResponseAck(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_820: see yeccpars2_4

yeccpars2_821(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 822, Ss, Stack, T, Ts, Tzr).

yeccpars2_822(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 824, Ss, Stack, T, Ts, Tzr);
yeccpars2_822(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_822_(Stack),
 yeccpars2_823(823, Cat, [822 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_823(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 829, Ss, Stack, T, Ts, Tzr);
yeccpars2_823(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr).

yeccpars2_824(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 825, Ss, Stack, T, Ts, Tzr).

yeccpars2_825(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_825_(Stack),
 yeccgoto_optImmAckRequired(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_826(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 951, Ss, Stack, T, Ts, Tzr).

yeccpars2_827(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_827_(Stack),
 yeccgoto_transactionReplyBody(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_828(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 948, Ss, Stack, T, Ts, Tzr);
yeccpars2_828(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_828_(Stack),
 yeccpars2_947(_S, Cat, [828 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_829(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 830, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_830: see yeccpars2_4

yeccpars2_831(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 832, Ss, Stack, T, Ts, Tzr);
yeccpars2_831(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_831_(Stack),
 yeccgoto_actionReply(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_832(S, 'AddToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 842, Ss, Stack, T, Ts, Tzr);
yeccpars2_832(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 843, Ss, Stack, T, Ts, Tzr);
yeccpars2_832(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 844, Ss, Stack, T, Ts, Tzr);
yeccpars2_832(S, 'ContextAttrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 151, Ss, Stack, T, Ts, Tzr);
yeccpars2_832(S, 'EmergencyOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 153, Ss, Stack, T, Ts, Tzr);
yeccpars2_832(S, 'EmergencyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 154, Ss, Stack, T, Ts, Tzr);
yeccpars2_832(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_832(S, 'IEPSToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 155, Ss, Stack, T, Ts, Tzr);
yeccpars2_832(S, 'ModifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 845, Ss, Stack, T, Ts, Tzr);
yeccpars2_832(S, 'MoveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 846, Ss, Stack, T, Ts, Tzr);
yeccpars2_832(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 847, Ss, Stack, T, Ts, Tzr);
yeccpars2_832(S, 'PriorityToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 159, Ss, Stack, T, Ts, Tzr);
yeccpars2_832(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 848, Ss, Stack, T, Ts, Tzr);
yeccpars2_832(S, 'SubtractToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 849, Ss, Stack, T, Ts, Tzr);
yeccpars2_832(S, 'TopologyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr).

yeccpars2_833(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_833_(Stack),
 yeccgoto_commandReplys(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_834(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_834_(Stack),
 yeccgoto_commandReplys(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_835(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_835_(Stack),
 yeccgoto_actionReplyBody(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_836(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_836_(Stack),
 yeccgoto_commandReplys(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_837(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 943, Ss, Stack, T, Ts, Tzr);
yeccpars2_837(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_837_(Stack),
 yeccpars2_942(_S, Cat, [837 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_838(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_838_(Stack),
 yeccgoto_commandReplys(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_839(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 936, Ss, Stack, T, Ts, Tzr).

yeccpars2_840(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_840_(Stack),
 yeccgoto_commandReplys(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_841(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 935, Ss, Stack, T, Ts, Tzr).

yeccpars2_842(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_842_(Stack),
 yeccgoto_ammsToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_843(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 931, Ss, Stack, T, Ts, Tzr).

yeccpars2_844(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 881, Ss, Stack, T, Ts, Tzr).

yeccpars2_845(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_845_(Stack),
 yeccgoto_ammsToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_846(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_846_(Stack),
 yeccgoto_ammsToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_847(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 875, Ss, Stack, T, Ts, Tzr).

yeccpars2_848(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 850, Ss, Stack, T, Ts, Tzr).

yeccpars2_849(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_849_(Stack),
 yeccgoto_ammsToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_850: see yeccpars2_183

yeccpars2_851(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 853, Ss, Stack, T, Ts, Tzr);
yeccpars2_851(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_851_(Stack),
 yeccpars2_852(_S, Cat, [851 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_852(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_852_(Stack),
 yeccgoto_serviceChangeReply(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_853(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_853(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 856, Ss, Stack, T, Ts, Tzr).

yeccpars2_854(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 874, Ss, Stack, T, Ts, Tzr).

yeccpars2_855(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 873, Ss, Stack, T, Ts, Tzr).

yeccpars2_856(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 857, Ss, Stack, T, Ts, Tzr).

yeccpars2_857(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 864, Ss, Stack, T, Ts, Tzr);
yeccpars2_857(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 865, Ss, Stack, T, Ts, Tzr);
yeccpars2_857(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 866, Ss, Stack, T, Ts, Tzr);
yeccpars2_857(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 441, Ss, Stack, T, Ts, Tzr);
yeccpars2_857(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 867, Ss, Stack, T, Ts, Tzr).

yeccpars2_858(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_858_(Stack),
 yeccgoto_servChgReplyParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_859(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_859_(Stack),
 yeccgoto_servChgReplyParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_860(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_860_(Stack),
 yeccgoto_servChgReplyParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_861(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_861_(Stack),
 yeccgoto_servChgReplyParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_862(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_862_(Stack),
 yeccgoto_servChgReplyParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_863(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 869, Ss, Stack, T, Ts, Tzr);
yeccpars2_863(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_863_(Stack),
 yeccpars2_868(868, Cat, [863 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_864(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 452, Ss, Stack, T, Ts, Tzr).

yeccpars2_865(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 450, Ss, Stack, T, Ts, Tzr).

yeccpars2_866(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 445, Ss, Stack, T, Ts, Tzr).

yeccpars2_867(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 443, Ss, Stack, T, Ts, Tzr).

yeccpars2_868(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 872, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_869: see yeccpars2_857

yeccpars2_870(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 869, Ss, Stack, T, Ts, Tzr);
yeccpars2_870(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_870_(Stack),
 yeccpars2_871(_S, Cat, [870 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_871(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_871_(Stack),
 yeccgoto_servChgReplyParms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_872(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_872_(Stack),
 yeccgoto_serviceChangeReplyDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_873(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_873_(Stack),
 yeccgoto_serviceChangeReplyBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_874(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_874_(Stack),
 yeccgoto_serviceChangeReplyBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_875: see yeccpars2_183

yeccpars2_876(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 878, Ss, Stack, T, Ts, Tzr);
yeccpars2_876(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_876_(Stack),
 yeccpars2_877(_S, Cat, [876 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_877(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_877_(Stack),
 yeccgoto_notifyReply(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_878(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr).

yeccpars2_879(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 880, Ss, Stack, T, Ts, Tzr).

yeccpars2_880(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_880_(Stack),
 yeccgoto_notifyReplyBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_881(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 884, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_882(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 898, Ss, Stack, T, Ts, Tzr);
yeccpars2_882(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_882_(Stack),
 yeccgoto_auditOther(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_883(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_883_(Stack),
 yeccgoto_auditReply(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_884(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 887, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_885(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_885_(Stack),
 yeccgoto_contextTerminationAudit(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_886(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_886_(Stack),
 yeccgoto_auditReply(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_887(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 889, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_888(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 897, Ss, Stack, T, Ts, Tzr).

yeccpars2_889(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 890, Ss, Stack, T, Ts, Tzr);
yeccpars2_889(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_890: see yeccpars2_4

yeccpars2_891(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_891_(Stack),
 yeccgoto_errorCode(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_892(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 893, Ss, Stack, T, Ts, Tzr).

yeccpars2_893(S, 'QuotedChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 895, Ss, Stack, T, Ts, Tzr);
yeccpars2_893(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_893_(Stack),
 yeccpars2_894(894, Cat, [893 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_894(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 896, Ss, Stack, T, Ts, Tzr).

yeccpars2_895(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_895_(Stack),
 yeccgoto_errorText(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_896(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_896_(Stack),
 yeccgoto_errorDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_897(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_897_(Stack),
 yeccgoto_contextTerminationAudit(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_898(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 584, Ss, Stack, T, Ts, Tzr);
yeccpars2_898(S, 'DigitMapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 211, Ss, Stack, T, Ts, Tzr);
yeccpars2_898(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_898(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 585, Ss, Stack, T, Ts, Tzr);
yeccpars2_898(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 586, Ss, Stack, T, Ts, Tzr);
yeccpars2_898(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 913, Ss, Stack, T, Ts, Tzr);
yeccpars2_898(S, 'ModemToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 914, Ss, Stack, T, Ts, Tzr);
yeccpars2_898(S, 'MuxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 915, Ss, Stack, T, Ts, Tzr);
yeccpars2_898(S, 'ObservedEventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 916, Ss, Stack, T, Ts, Tzr);
yeccpars2_898(S, 'PackagesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 917, Ss, Stack, T, Ts, Tzr);
yeccpars2_898(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 590, Ss, Stack, T, Ts, Tzr);
yeccpars2_898(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 918, Ss, Stack, T, Ts, Tzr).

yeccpars2_899(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 930, Ss, Stack, T, Ts, Tzr).

yeccpars2_900(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_900_(Stack),
 yeccgoto_auditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_901(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_901_(Stack),
 yeccgoto_auditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_902(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_902_(Stack),
 yeccgoto_auditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_903(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_903_(Stack),
 yeccgoto_auditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_904(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_904_(Stack),
 yeccgoto_auditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_905(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_905_(Stack),
 yeccgoto_auditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_906(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_906_(Stack),
 yeccgoto_auditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_907(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_907_(Stack),
 yeccgoto_auditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_908(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_908_(Stack),
 yeccgoto_auditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_909(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_909_(Stack),
 yeccgoto_auditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_910(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_910_(Stack),
 yeccgoto_auditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_911(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 927, Ss, Stack, T, Ts, Tzr);
yeccpars2_911(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_911_(Stack),
 yeccpars2_926(_S, Cat, [911 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_912(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_912_(Stack),
 yeccgoto_auditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_913(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 649, Ss, Stack, T, Ts, Tzr);
yeccpars2_913(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_913_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_914(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 633, Ss, Stack, T, Ts, Tzr);
yeccpars2_914(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 634, Ss, Stack, T, Ts, Tzr);
yeccpars2_914(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_914_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_915(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 625, Ss, Stack, T, Ts, Tzr);
yeccpars2_915(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_915_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_916(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 474, Ss, Stack, T, Ts, Tzr);
yeccpars2_916(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_916_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_917(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 919, Ss, Stack, T, Ts, Tzr);
yeccpars2_917(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_917_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_918(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 592, Ss, Stack, T, Ts, Tzr);
yeccpars2_918(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_918_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_919: see yeccpars2_4

yeccpars2_920(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 922, Ss, Stack, T, Ts, Tzr);
yeccpars2_920(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_920_(Stack),
 yeccpars2_921(921, Cat, [920 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_921(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 925, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_922: see yeccpars2_4

yeccpars2_923(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 922, Ss, Stack, T, Ts, Tzr);
yeccpars2_923(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_923_(Stack),
 yeccpars2_924(_S, Cat, [923 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_924(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_924_(Stack),
 yeccgoto_packagesItems(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_925(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_925_(Stack),
 yeccgoto_packagesDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_926(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_926_(Stack),
 yeccgoto_terminationAudit(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_927: see yeccpars2_898

yeccpars2_928(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 927, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_928_(Stack),
 yeccpars2_929(_S, Cat, [928 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_929(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_929_(Stack),
 yeccgoto_auditReturnParameterList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_930(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_930_(Stack),
 yeccgoto_auditOther(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_931(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 933, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_932(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_932_(Stack),
 yeccgoto_auditReply(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_933(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 887, Ss, Stack, T, Ts, Tzr);
yeccpars2_933(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_safeToken2(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_934(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_934_(Stack),
 yeccgoto_auditReply(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_935(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_935_(Stack),
 yeccgoto_actionReply(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_936: see yeccpars2_183

yeccpars2_937(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 939, Ss, Stack, T, Ts, Tzr);
yeccpars2_937(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_937_(Stack),
 yeccpars2_938(_S, Cat, [937 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_938(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_938_(Stack),
 yeccgoto_ammsReply(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_939: see yeccpars2_898

yeccpars2_940(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 941, Ss, Stack, T, Ts, Tzr).

yeccpars2_941(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_941_(Stack),
 yeccgoto_ammsReplyBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_942(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_942_(Stack),
 yeccgoto_actionReplyBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_943: see yeccpars2_832

yeccpars2_944(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_944_(Stack),
 yeccgoto_commandReplyList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_945(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 943, Ss, Stack, T, Ts, Tzr);
yeccpars2_945(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_945_(Stack),
 yeccpars2_946(_S, Cat, [945 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_946(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_946_(Stack),
 yeccgoto_commandReplyList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_947(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_947_(Stack),
 yeccgoto_transactionReplyBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_948(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 829, Ss, Stack, T, Ts, Tzr).

yeccpars2_949(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 948, Ss, Stack, T, Ts, Tzr);
yeccpars2_949(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_949_(Stack),
 yeccpars2_950(_S, Cat, [949 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_950(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_950_(Stack),
 yeccgoto_actionReplyList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_951(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_951_(Stack),
 yeccgoto_transactionReply(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_952: see yeccpars2_4

yeccpars2_953(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 954, Ss, Stack, T, Ts, Tzr).

yeccpars2_954(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 955, Ss, Stack, T, Ts, Tzr).

yeccpars2_955(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_955_(Stack),
 yeccgoto_transactionPending(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_956(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_956_(Stack),
 yeccgoto_transactionList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_957(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_957_(Stack),
 yeccgoto_pathName(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_958(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_958_(Stack),
 yeccgoto_deviceName(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_959(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_959(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_959_(Stack),
 yeccpars2_963(_S, Cat, [959 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_960(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_960(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_960_(Stack),
 yeccpars2_962(_S, Cat, [960 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_961(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_961_(Stack),
 yeccgoto_mtpAddress(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_962(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_962_(Stack),
 yeccgoto_mId(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_963(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_963_(Stack),
 yeccgoto_mId(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccgoto_actionReply(823, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_828(828, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actionReply(948, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_949(949, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_actionReplyBody(832, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_841(841, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_actionReplyList(828=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_947(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actionReplyList(949=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_950(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_actionRequest(126, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(127, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actionRequest(798, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_799(799, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actionRequest(804, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_805(805, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actionRequest(808, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_809(809, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_actionRequestBody(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(147, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_actionRequestItem(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(146, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actionRequestItem(567, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_568(568, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_actionRequestItems(146=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_566(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actionRequestItems(568=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_569(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_actionRequestList(127, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_797(797, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actionRequestList(799=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_800(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actionRequestList(805, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_806(806, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actionRequestList(809, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_810(810, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_alternativeValue(286=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_296(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ammParameter(573, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_583(583, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ammParameter(793, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_794(794, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ammParameters(583, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_792(792, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ammParameters(794=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_795(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ammRequest(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ammRequest(567=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ammRequestBody(571=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_572(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ammToken(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(144, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ammToken(567, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(144, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ammsReply(832=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_840(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ammsReply(943=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_840(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ammsReplyBody(937=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_938(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ammsToken(832, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_839(839, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ammsToken(943, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_839(839, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_auditDescriptor(194, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_195(195, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditDescriptor(573=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_582(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditDescriptor(793=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_582(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_auditDescriptorBody(197, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_209(209, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_auditItem(197, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(208, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditItem(402, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_403(403, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditItem(420=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_433(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditItem(460=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_433(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_auditItemList(208=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_401(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditItemList(403=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_404(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_auditOther(881=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_883(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditOther(931=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_932(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_auditReply(832=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_838(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditReply(943=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_838(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_auditRequest(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditRequest(567=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_auditReturnItem(197=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_207(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditReturnItem(402=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_207(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditReturnItem(420=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_207(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditReturnItem(460=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_207(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditReturnItem(898=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_912(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditReturnItem(927=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_912(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditReturnItem(939=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_912(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_auditReturnParameter(898, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_911(911, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditReturnParameter(927, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_928(928, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditReturnParameter(939, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_911(911, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_auditReturnParameterList(911=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_926(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditReturnParameterList(928=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_929(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_auditSelectLogic(507=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_516(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditSelectLogic(528=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_516(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditSelectLogic(544=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_516(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_authenticationHeader(1, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_commandReplyList(837=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_942(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_commandReplyList(945=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_946(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_commandReplys(832, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_837(837, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_commandReplys(943, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_945(945, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_commandRequest(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_commandRequest(567=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextAttrDescriptor(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAttrDescriptor(507=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_515(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAttrDescriptor(528=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_515(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAttrDescriptor(544=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_515(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAttrDescriptor(567=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAttrDescriptor(832=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAttrDescriptor(943=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextAudit(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAudit(567=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextAuditProperties(514, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_555(555, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAuditProperties(533, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_543(543, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAuditProperties(545=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_546(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextAuditProperty(507, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_514(514, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAuditProperty(528, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_533(533, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAuditProperty(544, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_545(545, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextAuditSelector(507=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_513(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAuditSelector(528=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_513(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAuditSelector(544=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_513(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextID(129, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(131, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextID(536, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_537(537, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextID(539, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_540(540, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextID(830, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_831(831, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextIDs(537, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_538(538, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextIDs(540=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_541(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextIdList(528, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_532(532, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextIdList(558, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_532(532, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextProperty(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextProperty(567=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextProperty(832=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_836(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextProperty(943=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_836(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextTerminationAudit(884=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_886(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextTerminationAudit(933=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_934(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_daddr(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(98, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_daddr(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_daddr(99=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_deviceName(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_960(960, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_digitMapDescriptor(573=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_581(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_digitMapDescriptor(793=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_581(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_digitMapDescriptor(898=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_910(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_digitMapDescriptor(927=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_910(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_digitMapDescriptor(939=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_910(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_direction(280=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_281(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_domainAddress(88=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_domainAddress(445=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_domainAddress(452=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_domainName(88=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_domainName(445=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_domainName(452=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_embedFirst(735, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_737(737, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_embedFirst(768, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_770(770, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_embedNoSig(716=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_723(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_embedNoSig(731, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_733(733, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_embedNoSig(775=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_723(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_embedSig(745=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_750(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_embedSig(758=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_750(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_embedWithSig(716=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_722(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_embedWithSig(731, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_732(732, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_embedWithSig(775=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_722(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_emergencyValue(507=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_512(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_emergencyValue(528=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_512(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_emergencyValue(544=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_512(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_errorCode(890, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_892(892, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_errorDescriptor(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_errorDescriptor(469=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_472(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_errorDescriptor(823=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_827(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_errorDescriptor(832=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_835(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_errorDescriptor(853, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_855(855, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_errorDescriptor(878, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_879(879, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_errorDescriptor(887, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_888(888, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_errorDescriptor(898=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_909(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_errorDescriptor(927=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_909(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_errorDescriptor(939=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_909(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_errorDescriptor(943=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_944(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_errorText(893, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_894(894, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventBufferControl(661=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_665(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventBufferControl(675=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_665(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventBufferControlValue(670=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_671(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventBufferDescriptor(573=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_580(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventBufferDescriptor(793=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_580(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventBufferDescriptor(898=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_908(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventBufferDescriptor(927=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_908(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventBufferDescriptor(939=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_908(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventDM(716=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_721(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventDM(745=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_749(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventDM(758=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_749(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventDM(775=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_721(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventParameter(716, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_720(720, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventParameter(775, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_776(776, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventParameterName(394=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_398(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventParameterName(487, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(490, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventParameterName(493, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(490, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventParameterName(716, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(490, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventParameterName(745, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(490, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventParameterName(758, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(490, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventParameterName(775, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(490, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventParameters(720, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_774(774, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventParameters(776=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_777(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventSpec(784, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_786(786, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventSpec(788, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_789(789, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventSpecList(786, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_787(787, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventSpecList(789=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_790(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventStream(163=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventStream(179=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventStream(394=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_397(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventStreamOrOther(487=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_489(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventStreamOrOther(493=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_489(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventStreamOrOther(716=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_719(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventStreamOrOther(745=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_748(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventStreamOrOther(758=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_748(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventStreamOrOther(775=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_719(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventsDescriptor(573=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_579(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventsDescriptor(793=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_579(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventsDescriptor(898=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_907(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventsDescriptor(927=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_907(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventsDescriptor(939=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_907(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_extension(420=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_432(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_extension(460=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_432(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_extensionParameter(420, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(431, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_extensionParameter(460, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(431, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_iaServiceStates(333=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_337(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_iepsValue(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_iepsValue(507=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_511(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_iepsValue(528=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_511(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_iepsValue(544=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_511(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_iepsValue(567=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_iepsValue(832=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_iepsValue(943=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudauditReturnParameter(197, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(206, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudauditReturnParameter(402, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(206, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudauditReturnParameter(406, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_407(407, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudauditReturnParameter(420, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(206, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudauditReturnParameter(460, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(206, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudcontextAttrDescriptor(507, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_510(510, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAuddigitMapDescriptor(197=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAuddigitMapDescriptor(402=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAuddigitMapDescriptor(406=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAuddigitMapDescriptor(420=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAuddigitMapDescriptor(460=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudeventBufferDescriptor(197=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudeventBufferDescriptor(402=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudeventBufferDescriptor(406=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudeventBufferDescriptor(420=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudeventBufferDescriptor(460=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudeventSpec(389, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_391(391, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudeventSpecParameter(394, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_396(396, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudeventsDescriptor(197=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudeventsDescriptor(402=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudeventsDescriptor(406=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudeventsDescriptor(420=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudeventsDescriptor(460=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudlocalControlDescriptor(320=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_326(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudlocalControlDescriptor(351=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_326(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudlocalControlDescriptor(376=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_326(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudlocalParm(354, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_357(357, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudlocalParm(371, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_372(372, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudlocalParmList(357, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_370(370, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudlocalParmList(372=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_373(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudmediaDescriptor(197=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudmediaDescriptor(402=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudmediaDescriptor(406=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudmediaDescriptor(420=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudmediaDescriptor(460=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudmediaParm(320, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_325(325, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudmediaParm(376, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_377(377, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudmediaParms(325, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_375(375, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudmediaParms(377=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_378(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudpackagesDescriptor(197=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudpackagesDescriptor(402=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudpackagesDescriptor(406=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudpackagesDescriptor(420=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudpackagesDescriptor(460=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudrequestedEvent(381, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_383(383, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudrequestedEvent(386, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_387(387, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudsignalList(226=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_231(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudsignalParm(226, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(230, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudsignalsDescriptor(197=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudsignalsDescriptor(402=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudsignalsDescriptor(406=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudsignalsDescriptor(420=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudsignalsDescriptor(460=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudstatisticsDescriptor(197=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudstatisticsDescriptor(320=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_324(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudstatisticsDescriptor(351=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_324(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudstatisticsDescriptor(376=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_324(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudstatisticsDescriptor(402=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudstatisticsDescriptor(406=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudstatisticsDescriptor(420=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudstatisticsDescriptor(460=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudstreamDescriptor(320=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_323(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudstreamDescriptor(376=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_323(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudstreamParm(320=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_322(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudstreamParm(351, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_352(352, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudstreamParm(376=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_322(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudterminationAudit(197=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudterminationAudit(402=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudterminationAudit(420=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudterminationAudit(460=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudterminationAuditList(206=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_405(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudterminationAuditList(407=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_413(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudterminationStateDescriptor(320=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_321(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudterminationStateDescriptor(376=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_321(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudterminationStateParm(333, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_336(336, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_localControlDescriptor(649=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_655(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_localControlDescriptor(681=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_655(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_localControlDescriptor(684=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_655(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_localControlDescriptor(706=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_655(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_localParm(688, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_690(690, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_localParm(701, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_702(702, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_localParmList(690, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_700(700, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_localParmList(702=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_703(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mId(88, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(92, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mId(445=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_447(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mId(452=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_453(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mediaDescriptor(573=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_578(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mediaDescriptor(793=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_578(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mediaDescriptor(898=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_906(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mediaDescriptor(927=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_906(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mediaDescriptor(939=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_906(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mediaParm(649, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_654(654, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mediaParm(706, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_707(707, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mediaParmList(654, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_705(705, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mediaParmList(707=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_708(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_megacoMessage(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_message(4, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(89, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_messageBody(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_modemDescriptor(573=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_577(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_modemDescriptor(793=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_577(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_modemDescriptor(898=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_905(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_modemDescriptor(927=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_905(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_modemDescriptor(939=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_905(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_modemType(633, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_647(647, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_modemType(634, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_636(636, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_modemType(638, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_639(639, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_modemTypeList(636, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_637(637, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_modemTypeList(639=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_640(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mtpAddress(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_959(959, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_muxDescriptor(573=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_576(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_muxDescriptor(793=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_576(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_muxDescriptor(898=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_904(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_muxDescriptor(927=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_904(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_muxDescriptor(939=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_904(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_muxType(625, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_627(627, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_notificationReason(264, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_265(265, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_notificationReason(272, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_273(273, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_notificationReasons(265, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_271(271, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_notificationReasons(273=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_274(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_notifyBehaviour(716=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_718(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_notifyBehaviour(745=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_747(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_notifyBehaviour(758=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_747(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_notifyBehaviour(775=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_718(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_notifyRegulated(716=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_717(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_notifyRegulated(745=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_717(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_notifyRegulated(758=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_717(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_notifyRegulated(775=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_717(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_notifyReply(832=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_834(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_notifyReply(943=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_834(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_notifyReplyBody(876=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_877(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_notifyRequest(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_notifyRequest(567=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_notifyRequestBody(469, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_471(471, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_observedEvent(476, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_479(479, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEvent(481, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_482(482, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEvent(784=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_785(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEvent(788=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_785(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_observedEventBody(485=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_486(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEventBody(500=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_501(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_observedEventParameter(487, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_488(488, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEventParameter(493, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_494(494, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_observedEventParameters(488, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_492(492, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEventParameters(494=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_495(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_observedEvents(479, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_480(480, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEvents(482=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_483(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_observedEventsDescriptor(469=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_470(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEventsDescriptor(898=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_903(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEventsDescriptor(927=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_903(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEventsDescriptor(939=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_903(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_onOrOff(503=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_504(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_onOrOff(694=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_695(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_onOrOff(696=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_697(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_optAuditDescriptor(185=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_193(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optAuditDescriptor(560=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_561(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optAuditDescriptor(563=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_564(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_optImmAckRequired(822, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_823(823, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_optIndAudeventSpecParameter(390=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_393(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_optIndAudsignalParm(219=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_225(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optIndAudsignalParm(412=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_225(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_optPropertyParms(641=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_642(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optPropertyParms(647=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_648(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_optSep(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(86=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(88, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(91, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(104=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(110=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(445, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(91, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(452, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(91, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(476, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(478, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(477, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_497(497, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(481, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(478, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(498, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(499, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(784, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(478, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(788, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(478, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(959=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_963(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(960=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_962(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_packagesDescriptor(898=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_902(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_packagesDescriptor(927=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_902(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_packagesDescriptor(939=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_902(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_packagesItem(316, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_318(318, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_packagesItem(919, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_920(920, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_packagesItem(922, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_923(923, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_packagesItems(920, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_921(921, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_packagesItems(923=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_924(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_parmValue(244=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_285(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parmValue(335=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_348(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parmValue(356=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_348(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parmValue(431=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_458(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parmValue(490=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_491(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parmValue(531=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_348(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parmValue(552=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_348(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_pathName(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_958(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_pkgdName(221, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_223(223, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(226=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(237=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(333, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_335(335, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(354, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_356(356, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(371, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_356(356, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(381=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_382(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(386=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_382(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(389, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_390(390, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(478, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_485(485, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(499, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_500(500, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(507=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_509(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(528, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_531(531, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(544=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_509(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(550, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(552, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(558, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(552, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(592, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_594(594, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(602, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_594(594, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(606=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(613=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(616=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(621=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(643, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(552, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(661, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(552, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(675, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(552, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(688, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(552, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(701, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(552, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(712, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_714(714, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(741, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_743(743, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(763, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_743(743, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(780, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_714(714, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_portNumber(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(104, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_portNumber(109, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(110, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_portNumber(445=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_446(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_priority(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_priority(507=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_508(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_priority(528=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_508(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_priority(544=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_508(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_priority(567=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_priority(832=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_priority(943=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_propertyParm(333=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_334(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParm(354=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_355(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParm(371=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_355(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParm(528, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_530(530, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParm(550, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_551(551, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParm(558, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_530(530, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParm(643, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_644(644, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParm(661=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_664(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParm(675=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_664(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParm(688=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_689(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParm(701=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_689(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_propertyParmList(530=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_549(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParmList(551=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_553(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParmList(644, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_645(645, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_propertyParms(528, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_529(529, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParms(558, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_529(529, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_requestID(260=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_262(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_requestID(380, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_385(385, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_requestID(474, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_475(475, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_requestID(710, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_711(711, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_requestID(739, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_740(740, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_requestedEvent(712, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_713(713, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_requestedEvent(780, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_781(781, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_requestedEventBody(714=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_715(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_requestedEvents(713, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_779(779, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_requestedEvents(781=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_782(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_safeToken(4, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(88, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(6, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(83, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(84, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(85, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(86, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_957(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(95, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(107, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(97, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(97, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(97, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(99, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(97, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(109=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(125=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_803(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(129=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(163=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(175=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(179=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(189=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(221=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(226=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(234=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_236(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(237=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(242, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(244, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(253=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(260=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_261(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(276=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_277(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(278=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_279(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(286=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_291(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(287=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_291(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(288=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_291(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(289=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_291(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(297=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_291(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(298=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_291(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(301=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_291(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(302=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_291(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(312, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(244, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(316=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_317(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(333=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(349=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(354=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(371=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(380=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_261(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(381=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(386=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(389=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(394=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_395(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(415=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(420=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_430(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(443=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_444(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(445=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(448=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_291(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(450=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_451(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(454=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_455(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(456=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_457(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(460=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_430(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(465=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_466(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(467=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(474=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_261(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(478=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(487=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_395(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(493=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_395(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(499=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(507=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(528=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(536=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(539=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(544=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(550=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(558=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(559=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(562=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(570=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(592=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(595=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_291(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(597=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_291(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(602=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(606=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(611=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_236(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(613=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(616=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(621=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(625=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_626(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(629=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(633=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_635(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(634=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_635(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(638=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_635(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(643=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(661=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(675=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(679=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(688=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(701=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(710=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_261(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(712=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(716=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_395(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(739=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_261(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(741=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(745=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_395(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(758=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_395(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(763=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(775=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_395(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(780=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(812=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_814(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(816=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_814(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(820=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_803(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(830=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(850=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(875=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(881=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(887=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(890=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_891(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(919=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_317(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(922=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_317(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(931=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(936=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(952=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_803(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_safeToken2(4=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(6=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(83=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(85=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(99=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(109=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(125=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(129=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(163=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(175=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(179=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(186=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(189=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(221=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(226=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(234=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(237=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(242=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(253=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(260=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(276=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(278=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(286=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(287=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(288=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(289=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(297=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(298=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(301=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(302=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(312=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(316=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(333=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(349=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(354=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(371=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(380=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(381=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(386=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(389=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(394=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(415=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(420=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(443=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(445=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(448=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(450=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(454=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(456=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(460=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(465=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(467=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(474=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(478=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(487=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(493=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(499=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(507=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(528=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(536=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(539=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(544=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(550=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(558=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(559=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(562=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(570=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(592=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(595=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(597=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(602=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(606=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(611=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(613=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(616=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(621=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(625=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(629=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(633=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(634=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(638=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(643=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(661=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(675=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(679=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(688=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(701=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(710=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(712=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(716=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(739=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(741=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(745=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(758=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(763=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(775=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(780=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(812=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(816=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(820=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(830=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(850=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(875=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(881=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(887=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(890=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(919=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(922=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(931=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(936=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken2(952=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_secondEventParameter(745, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_746(746, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_secondEventParameter(758, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_759(759, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_secondEventParameters(746, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_757(757, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_secondEventParameters(759=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_760(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_secondRequestedEvent(741, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_742(742, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_secondRequestedEvent(763, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_764(764, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_secondRequestedEventBody(743=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_744(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_secondRequestedEvents(742, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_762(762, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_secondRequestedEvents(764=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_765(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_servChgReplyParm(857, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_863(863, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_servChgReplyParm(869, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_870(870, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_servChgReplyParms(863, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_868(868, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_servChgReplyParms(870=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_871(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeAddress(420=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_429(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeAddress(460=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_429(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeAddress(857=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_862(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeAddress(869=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_862(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeDelay(420=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_428(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeDelay(460=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_428(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeDescriptor(417, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_418(418, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeMethod(420=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_427(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeMethod(460=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_427(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeMgcId(420=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_426(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeMgcId(460=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_426(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeMgcId(857=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_861(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeMgcId(869=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_861(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeParm(420, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_425(425, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeParm(460, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_461(461, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeParms(425, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_459(459, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeParms(461=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_462(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeProfile(420=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_424(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeProfile(460=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_424(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeProfile(857=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_860(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeProfile(869=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_860(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeReason(420=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_423(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeReason(460=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_423(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeReply(832=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_833(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeReply(943=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_833(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeReplyBody(851=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_852(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeReplyDescriptor(853, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_854(854, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeRequest(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeRequest(567=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeVersion(420=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_422(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeVersion(460=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_422(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeVersion(857=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_859(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeVersion(869=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_859(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceStates(661=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_663(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceStates(675=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_663(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceStatesValue(340=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_346(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceStatesValue(341=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceStatesValue(668=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_669(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_sigParameter(242, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(243, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sigParameter(312, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_313(313, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_sigParameters(243, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_311(311, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sigParameters(313=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_314(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalList(606=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_609(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalList(621=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_609(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalListId(234, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_235(235, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalListId(611, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_612(612, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalListParm(237, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_239(239, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalListParm(613, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_614(614, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalListParm(616, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_617(617, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalListParms(614, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_615(615, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalListParms(617=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_618(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalName(226, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(228, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalName(237, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(228, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalName(606, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(228, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalName(613, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(228, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalName(616, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(228, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalName(621, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(228, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalParm(606, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_608(608, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalParm(621, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_622(622, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalParms(608, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_620(620, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalParms(622=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_623(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalRequest(226=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalRequest(237=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_238(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalRequest(606=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_607(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalRequest(613=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_238(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalRequest(616=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_238(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalRequest(621=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_607(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalType(255=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_256(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalsDescriptor(573=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_575(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalsDescriptor(735, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_736(736, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalsDescriptor(754, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_755(755, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalsDescriptor(793=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_575(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalsDescriptor(898=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_901(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalsDescriptor(927=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_901(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalsDescriptor(939=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_901(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_statisticsDescriptor(573=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_574(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsDescriptor(649=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_653(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsDescriptor(681=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_653(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsDescriptor(684=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_653(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsDescriptor(706=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_653(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsDescriptor(793=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_574(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsDescriptor(898=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_900(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsDescriptor(927=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_900(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsDescriptor(939=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_900(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_statisticsParameter(592, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_593(593, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsParameter(602, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_603(603, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_statisticsParameters(593, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_601(601, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsParameters(603=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_604(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_streamDescriptor(649=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_652(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_streamDescriptor(706=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_652(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_streamID(175=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_streamID(253=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_streamID(349, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(350, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_streamID(679, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_680(680, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_streamModes(361=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_369(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_streamModes(362=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_363(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_streamModes(698=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_699(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_streamParm(649=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_651(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_streamParm(681, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_682(682, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_streamParm(684, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_685(685, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_streamParm(706=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_651(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_streamParmList(682, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_683(683, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_streamParmList(685=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_686(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_subtractRequest(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_subtractRequest(567=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_termIDList(183, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_185(185, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termIDList(415, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_416(416, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termIDList(467, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_468(468, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termIDList(559, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_560(560, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termIDList(562, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_563(563, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termIDList(570, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_571(571, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termIDList(850, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_851(851, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termIDList(875, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_876(876, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termIDList(881, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_882(882, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termIDList(931, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_882(882, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termIDList(936, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_937(937, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_terminationAudit(898, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_899(899, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationAudit(939, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_940(940, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_terminationID(163=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(179=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_187(187, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(189, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_190(190, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(415=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(467=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(559=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(562=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(570=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(629, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_630(630, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(850=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(875=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(881=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(887, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_630(630, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(931=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(936=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_terminationIDList(627=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_628(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationIDList(884=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_885(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationIDList(933=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_885(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_terminationIDListRepeat(187, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(188, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationIDListRepeat(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_191(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationIDListRepeat(630, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_631(631, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_terminationStateDescriptor(649=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_650(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationStateDescriptor(706=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_650(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_terminationStateParm(661, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_662(662, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationStateParm(675, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_676(676, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_terminationStateParms(662, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_674(674, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationStateParms(676=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_677(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_timeStamp(420=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_421(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_timeStamp(460=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_421(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_timeStamp(476, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_477(477, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_timeStamp(481, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_477(477, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_timeStamp(784, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_477(477, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_timeStamp(788, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_477(477, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_timeStamp(857=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_858(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_timeStamp(869=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_858(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_topologyDescComp(163, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(165, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_topologyDescComp(179, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(180, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_topologyDescCompList(165, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(178, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_topologyDescCompList(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_topologyDescriptor(132=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_topologyDescriptor(567=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_topologyDescriptor(832=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_topologyDescriptor(943=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_topologyDirection(163=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_164(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_topologyDirection(179=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_164(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionAck(812, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_813(813, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionAck(816, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_817(817, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionAckList(813, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_815(815, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionAckList(817=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_818(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionID(125, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_802(802, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionID(820, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_821(821, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionID(952, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_953(953, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionItem(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(117, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionItem(117, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(117, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionList(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionList(117=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_956(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionPending(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionPending(117=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionReply(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionReply(117=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionReplyBody(823, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_826(826, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionRequest(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionRequest(117=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionResponseAck(92=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionResponseAck(117=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_value(286=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_295(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(287=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_294(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(288=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_293(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(289=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_290(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(297, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_308(308, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(298, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_299(299, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(301, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_305(305, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(302, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_303(303, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(448=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_449(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(595=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_596(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(597, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_598(598, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_valueList(299, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_300(300, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_valueList(303=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_304(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_valueList(308, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_309(309, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_valueList(598, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_599(599, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,{yeccpars2_0_,1}}).
-file("megaco_text_parser_prev3c.yrl", 482).
yeccpars2_0_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_1_,1}}).
-file("megaco_text_parser_prev3c.yrl", 487).
yeccpars2_1_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_3_,1}}).
-file("megaco_text_parser_prev3c.yrl", 481).
yeccpars2_3_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   sep
  end | __Stack].

-compile({inline,{yeccpars2_7_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1537).
yeccpars2_7_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_86_,1}}).
-file("megaco_text_parser_prev3c.yrl", 482).
yeccpars2_86_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_87_,1}}).
-file("megaco_text_parser_prev3c.yrl", 486).
yeccpars2_87_(__Stack0) ->
 [__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_auth_header ( __3 , __5 , __7 )
  end | __Stack].

-compile({inline,{yeccpars2_88_,1}}).
-file("megaco_text_parser_prev3c.yrl", 482).
yeccpars2_88_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_90_,1}}).
-file("megaco_text_parser_prev3c.yrl", 479).
yeccpars2_90_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'MegacoMessage' { authHeader = __2 , mess = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_96_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1014).
yeccpars2_96_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_97_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1014).
yeccpars2_97_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_99_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1014).
yeccpars2_99_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_100_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1015).
yeccpars2_100_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ colon | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_101_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1012).
yeccpars2_101_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_domainAddress ( __2 , asn1_NOVALUE )
  end | __Stack].

-compile({inline,{yeccpars2_103_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1019).
yeccpars2_103_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_uint16 ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_104_,1}}).
-file("megaco_text_parser_prev3c.yrl", 482).
yeccpars2_104_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_105_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1010).
yeccpars2_105_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_domainAddress ( __2 , __5 )
  end | __Stack].

-compile({inline,{yeccpars2_106_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1016).
yeccpars2_106_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_108_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1002).
yeccpars2_108_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_domainName ( __2 , asn1_NOVALUE )
  end | __Stack].

-compile({inline,{yeccpars2_110_,1}}).
-file("megaco_text_parser_prev3c.yrl", 482).
yeccpars2_110_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_111_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1000).
yeccpars2_111_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_domainName ( __2 , __5 )
  end | __Stack].

-compile({inline,{yeccpars2_112_,1}}).
-file("megaco_text_parser_prev3c.yrl", 501).
yeccpars2_112_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { transactionResponseAck , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_113_,1}}).
-file("megaco_text_parser_prev3c.yrl", 498).
yeccpars2_113_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { transactionRequest , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_114_,1}}).
-file("megaco_text_parser_prev3c.yrl", 499).
yeccpars2_114_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { transactionReply , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_115_,1}}).
-file("megaco_text_parser_prev3c.yrl", 500).
yeccpars2_115_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { transactionPending , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_116_,1}}).
-file("megaco_text_parser_prev3c.yrl", 493).
yeccpars2_116_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { transactions , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_117_,1}}).
-file("megaco_text_parser_prev3c.yrl", 495).
yeccpars2_117_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_118_,1}}).
-file("megaco_text_parser_prev3c.yrl", 490).
yeccpars2_118_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_message ( __1 , __2 , __3 )
  end | __Stack].

-compile({inline,{yeccpars2_119_,1}}).
-file("megaco_text_parser_prev3c.yrl", 492).
yeccpars2_119_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { messageError , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_127_,1}}).
-file("megaco_text_parser_prev3c.yrl", 530).
yeccpars2_127_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_130_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1007).
yeccpars2_130_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_contextID ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_133_,1}}).
-file("megaco_text_parser_prev3c.yrl", 555).
yeccpars2_133_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { topology , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_136_,1}}).
-file("megaco_text_parser_prev3c.yrl", 556).
yeccpars2_136_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { priority , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_138_,1}}).
-file("megaco_text_parser_prev3c.yrl", 559).
yeccpars2_138_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { iepsCallind , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_139_,1}}).
-file("megaco_text_parser_prev3c.yrl", 549).
yeccpars2_139_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { contextProp , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_140_,1}}).
-file("megaco_text_parser_prev3c.yrl", 550).
yeccpars2_140_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { contextAudit , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_142_,1}}).
-file("megaco_text_parser_prev3c.yrl", 551).
yeccpars2_142_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { commandRequest , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_146_,1}}).
-file("megaco_text_parser_prev3c.yrl", 547).
yeccpars2_146_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_148_,1}}).
-file("megaco_text_parser_prev3c.yrl", 666).
yeccpars2_148_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { addReq , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_153_,1}}).
-file("megaco_text_parser_prev3c.yrl", 558).
yeccpars2_153_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { emergency , false }
  end | __Stack].

-compile({inline,{yeccpars2_154_,1}}).
-file("megaco_text_parser_prev3c.yrl", 557).
yeccpars2_154_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { emergency , true }
  end | __Stack].

-compile({inline,{yeccpars2_156_,1}}).
-file("megaco_text_parser_prev3c.yrl", 668).
yeccpars2_156_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { modReq , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_157_,1}}).
-file("megaco_text_parser_prev3c.yrl", 667).
yeccpars2_157_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { moveReq , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_165_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1515).
yeccpars2_165_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_166_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1511).
yeccpars2_166_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { tid , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_167_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1037).
yeccpars2_167_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_terminationID ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_168_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1512).
yeccpars2_168_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { sid , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_169_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1519).
yeccpars2_169_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { direction , bothway }
  end | __Stack].

-compile({inline,{yeccpars2_170_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1520).
yeccpars2_170_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { direction , isolate }
  end | __Stack].

-compile({inline,{yeccpars2_171_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1523).
yeccpars2_171_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { direction_ext , onewayboth }
  end | __Stack].

-compile({inline,{yeccpars2_172_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1522).
yeccpars2_172_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { direction_ext , onewayexternal }
  end | __Stack].

-compile({inline,{yeccpars2_173_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1521).
yeccpars2_173_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { direction , oneway }
  end | __Stack].

-compile({inline,{yeccpars2_176_,1}}).
-file("megaco_text_parser_prev3c.yrl", 939).
yeccpars2_176_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_177_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1176).
yeccpars2_177_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_streamID ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_180_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1515).
yeccpars2_180_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_181_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1517).
yeccpars2_181_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_182_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1509).
yeccpars2_182_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_topologyDescriptor ( [ __3 | __4 ] )
  end | __Stack].

-compile({inline,{yeccpars2_184_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1023).
yeccpars2_184_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_185_,1}}).
-file("megaco_text_parser_prev3c.yrl", 706).
yeccpars2_185_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_187_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1032).
yeccpars2_187_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_190_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1032).
yeccpars2_190_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_191_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1031).
yeccpars2_191_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_192_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1025).
yeccpars2_192_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_193_,1}}).
-file("megaco_text_parser_prev3c.yrl", 700).
yeccpars2_193_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   SR = # 'SubtractRequest' { terminationID = __3 ,
    auditDescriptor = __4 } ,
    make_commandRequest ( { subtractReq , __1 } , SR )
  end | __Stack].

-compile({inline,{yeccpars2_197_,1}}).
-file("megaco_text_parser_prev3c.yrl", 758).
yeccpars2_197_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_198_,1}}).
-file("megaco_text_parser_prev3c.yrl", 779).
yeccpars2_198_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { terminationAudit , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_199_,1}}).
-file("megaco_text_parser_prev3c.yrl", 806).
yeccpars2_199_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { indAudStatisticsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_200_,1}}).
-file("megaco_text_parser_prev3c.yrl", 800).
yeccpars2_200_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { indAudSignalsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_201_,1}}).
-file("megaco_text_parser_prev3c.yrl", 808).
yeccpars2_201_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { indAudPackagesDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_202_,1}}).
-file("megaco_text_parser_prev3c.yrl", 796).
yeccpars2_202_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { indAudMediaDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_203_,1}}).
-file("megaco_text_parser_prev3c.yrl", 798).
yeccpars2_203_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { indAudEventsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_204_,1}}).
-file("megaco_text_parser_prev3c.yrl", 804).
yeccpars2_204_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { indAudEventBufferDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_205_,1}}).
-file("megaco_text_parser_prev3c.yrl", 802).
yeccpars2_205_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { indAudDigitMapDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_206_,1}}).
-file("megaco_text_parser_prev3c.yrl", 793).
yeccpars2_206_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_208_,1}}).
-file("megaco_text_parser_prev3c.yrl", 761).
yeccpars2_208_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_210_,1}}).
-file("megaco_text_parser_prev3c.yrl", 931).
yeccpars2_210_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_IADMD ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_211_,1}}).
-file("megaco_text_parser_prev3c.yrl", 768).
yeccpars2_211_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   digitMapToken
  end | __Stack].

-compile({inline,{yeccpars2_212_,1}}).
-file("megaco_text_parser_prev3c.yrl", 777).
yeccpars2_212_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   eventBufferToken
  end | __Stack].

-compile({inline,{yeccpars2_213_,1}}).
-file("megaco_text_parser_prev3c.yrl", 778).
yeccpars2_213_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   eventsToken
  end | __Stack].

-compile({inline,{yeccpars2_214_,1}}).
-file("megaco_text_parser_prev3c.yrl", 767).
yeccpars2_214_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   mediaToken
  end | __Stack].

-compile({inline,{yeccpars2_215_,1}}).
-file("megaco_text_parser_prev3c.yrl", 766).
yeccpars2_215_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   modemToken
  end | __Stack].

-compile({inline,{yeccpars2_216_,1}}).
-file("megaco_text_parser_prev3c.yrl", 765).
yeccpars2_216_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   muxToken
  end | __Stack].

-compile({inline,{yeccpars2_217_,1}}).
-file("megaco_text_parser_prev3c.yrl", 770).
yeccpars2_217_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   observedEventsToken
  end | __Stack].

-compile({inline,{yeccpars2_218_,1}}).
-file("megaco_text_parser_prev3c.yrl", 771).
yeccpars2_218_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   packagesToken
  end | __Stack].

-compile({inline,{yeccpars2_219_,1}}).
-file("megaco_text_parser_prev3c.yrl", 776).
yeccpars2_219_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   signalsToken
  end | __Stack].

-compile({inline,{yeccpars2_220_,1}}).
-file("megaco_text_parser_prev3c.yrl", 769).
yeccpars2_220_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   statsToken
  end | __Stack].

-compile({inline,{yeccpars2_222_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1178).
yeccpars2_222_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_pkgdName ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_224_,1}}).
-file("megaco_text_parser_prev3c.yrl", 934).
yeccpars2_224_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'IndAudStatisticsDescriptor' { statName = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_225_,1}}).
-file("megaco_text_parser_prev3c.yrl", 911).
yeccpars2_225_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_227_,1}}).
-file("megaco_text_parser_prev3c.yrl", 918).
yeccpars2_227_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { signal , ensure_indAudSignal ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_228_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1287).
yeccpars2_228_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   merge_signalRequest ( __1 , [ ] )
  end | __Stack].

-compile({inline,{yeccpars2_231_,1}}).
-file("megaco_text_parser_prev3c.yrl", 917).
yeccpars2_231_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { seqSigList , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_232_,1}}).
-file("megaco_text_parser_prev3c.yrl", 914).
yeccpars2_232_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   asn1_NOVALUE
  end | __Stack].

-compile({inline,{yeccpars2_235_,1}}).
-file("megaco_text_parser_prev3c.yrl", 921).
yeccpars2_235_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'IndAudSeqSigList' { id = ensure_uint16 ( __3 ) }
  end | __Stack].

-compile({inline,{yeccpars2_236_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1357).
yeccpars2_236_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_uint16 ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_240_,1}}).
-file("megaco_text_parser_prev3c.yrl", 924).
yeccpars2_240_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'IndAudSeqSigList' { id = ensure_uint16 ( __3 ) ,
    signalList =
    ensure_indAudSignalListParm ( __5 ) }
  end | __Stack].

-compile({inline,{yeccpars2_241_,1}}).
-file("megaco_text_parser_prev3c.yrl", 915).
yeccpars2_241_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_243_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1290).
yeccpars2_243_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_248_COMMA,1}}).
-file("megaco_text_parser_prev3c.yrl", 1321).
yeccpars2_248_COMMA(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   keepActive
  end | __Stack].

-compile({inline,{yeccpars2_248_RBRKT,1}}).
-file("megaco_text_parser_prev3c.yrl", 1321).
yeccpars2_248_RBRKT(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   keepActive
  end | __Stack].

-compile({inline,{yeccpars2_254_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1313).
yeccpars2_254_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { stream , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_256_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1315).
yeccpars2_256_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { signal_type , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_257_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1333).
yeccpars2_257_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   brief
  end | __Stack].

-compile({inline,{yeccpars2_258_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1331).
yeccpars2_258_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   onOff
  end | __Stack].

-compile({inline,{yeccpars2_259_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1332).
yeccpars2_259_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   timeOut
  end | __Stack].

-compile({inline,{yeccpars2_261_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1391).
yeccpars2_261_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_requestID ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_262_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1325).
yeccpars2_262_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { requestId , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_265_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1340).
yeccpars2_265_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_266_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1343).
yeccpars2_266_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   onInterruptByEvent
  end | __Stack].

-compile({inline,{yeccpars2_267_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1344).
yeccpars2_267_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   onInterruptByNewSignalDescr
  end | __Stack].

-compile({inline,{yeccpars2_268_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1346).
yeccpars2_268_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   iteration
  end | __Stack].

-compile({inline,{yeccpars2_269_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1345).
yeccpars2_269_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   otherReason
  end | __Stack].

-compile({inline,{yeccpars2_270_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1342).
yeccpars2_270_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   onTimeOut
  end | __Stack].

-compile({inline,{yeccpars2_273_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1340).
yeccpars2_273_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_274_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1339).
yeccpars2_274_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_275_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1320).
yeccpars2_275_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { notify_completion , [ __4 | __5 ] }
  end | __Stack].

-compile({inline,{yeccpars2_277_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1327).
yeccpars2_277_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { intersigDelay , ensure_uint16 ( __3 ) }
  end | __Stack].

-compile({inline,{yeccpars2_279_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1317).
yeccpars2_279_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { duration , ensure_uint16 ( __3 ) }
  end | __Stack].

-compile({inline,{yeccpars2_281_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1323).
yeccpars2_281_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { direction , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_282_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1337).
yeccpars2_282_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   both
  end | __Stack].

-compile({inline,{yeccpars2_283_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1335).
yeccpars2_283_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   external
  end | __Stack].

-compile({inline,{yeccpars2_284_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1336).
yeccpars2_284_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   internal
  end | __Stack].

-compile({inline,{yeccpars2_285_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1329).
yeccpars2_285_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { other , ensure_NAME ( __1 ) , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_290_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1111).
yeccpars2_290_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   # 'PropertyParm' { value = [ __2 ] ,
    extraInfo = { relation , unequalTo } }
  end | __Stack].

-compile({inline,{yeccpars2_291_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1535).
yeccpars2_291_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_value ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_292_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1534).
yeccpars2_292_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_value ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_293_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1114).
yeccpars2_293_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   # 'PropertyParm' { value = [ __2 ] ,
    extraInfo = { relation , smallerThan } }
  end | __Stack].

-compile({inline,{yeccpars2_294_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1117).
yeccpars2_294_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   # 'PropertyParm' { value = [ __2 ] ,
    extraInfo = { relation , greaterThan } }
  end | __Stack].

-compile({inline,{yeccpars2_295_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1138).
yeccpars2_295_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'PropertyParm' { value = [ __1 ] }
  end | __Stack].

-compile({inline,{yeccpars2_296_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1108).
yeccpars2_296_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_299_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1141).
yeccpars2_299_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_303_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1141).
yeccpars2_303_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_304_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1140).
yeccpars2_304_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_306_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1130).
yeccpars2_306_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'PropertyParm' { value = [ __2 , __4 ] ,
    extraInfo = { range , true } }
  end | __Stack].

-compile({inline,{yeccpars2_307_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1134).
yeccpars2_307_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'PropertyParm' { value = [ __2 | __3 ] ,
    extraInfo = { sublist , true } }
  end | __Stack].

-compile({inline,{yeccpars2_308_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1141).
yeccpars2_308_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_310_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1126).
yeccpars2_310_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'PropertyParm' { value = [ __2 | __3 ] ,
    extraInfo = { sublist , false } }
  end | __Stack].

-compile({inline,{yeccpars2_313_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1290).
yeccpars2_313_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_314_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1289).
yeccpars2_314_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_315_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1286).
yeccpars2_315_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_signalRequest ( __1 , [ __3 | __4 ] )
  end | __Stack].

-compile({inline,{yeccpars2_317_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1483).
yeccpars2_317_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_packagesItem ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_319_,1}}).
-file("megaco_text_parser_prev3c.yrl", 937).
yeccpars2_319_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_indAudPackagesDescriptor ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_321_,1}}).
-file("megaco_text_parser_prev3c.yrl", 821).
yeccpars2_321_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { termStateDescr , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_322_,1}}).
-file("megaco_text_parser_prev3c.yrl", 819).
yeccpars2_322_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { streamParm , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_323_,1}}).
-file("megaco_text_parser_prev3c.yrl", 820).
yeccpars2_323_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { streamDescr , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_324_,1}}).
-file("megaco_text_parser_prev3c.yrl", 836).
yeccpars2_324_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'IndAudStreamParms' { statisticsDescriptor = __1 }
  end | __Stack].

-compile({inline,{yeccpars2_325_,1}}).
-file("megaco_text_parser_prev3c.yrl", 824).
yeccpars2_325_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_326_,1}}).
-file("megaco_text_parser_prev3c.yrl", 834).
yeccpars2_326_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'IndAudStreamParms' { localControlDescriptor = __1 }
  end | __Stack].

-compile({inline,{yeccpars2_328_,1}}).
-file("megaco_text_parser_prev3c.yrl", 831).
yeccpars2_328_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   LD = ensure_prop_groups ( __1 ) ,
    # 'IndAudStreamParms' { localDescriptor = LD }
  end | __Stack].

-compile({inline,{yeccpars2_329_,1}}).
-file("megaco_text_parser_prev3c.yrl", 828).
yeccpars2_329_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   RD = ensure_prop_groups ( __1 ) ,
    # 'IndAudStreamParms' { remoteDescriptor = RD }
  end | __Stack].

-compile({inline,{yeccpars2_334_,1}}).
-file("megaco_text_parser_prev3c.yrl", 877).
yeccpars2_334_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { prop , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_335_,1}}).
-file("megaco_text_parser_prev3c.yrl", 878).
yeccpars2_335_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { name , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_338_,1}}).
-file("megaco_text_parser_prev3c.yrl", 876).
yeccpars2_338_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   bufferToken
  end | __Stack].

-compile({inline,{yeccpars2_339_,1}}).
-file("megaco_text_parser_prev3c.yrl", 881).
yeccpars2_339_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   serviceStatesToken
  end | __Stack].

-compile({inline,{yeccpars2_342_,1}}).
-file("megaco_text_parser_prev3c.yrl", 885).
yeccpars2_342_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { serviceStates , { inequal , __3 } }
  end | __Stack].

-compile({inline,{yeccpars2_343_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1163).
yeccpars2_343_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   inSvc
  end | __Stack].

-compile({inline,{yeccpars2_344_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1162).
yeccpars2_344_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   outOfSvc
  end | __Stack].

-compile({inline,{yeccpars2_345_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1161).
yeccpars2_345_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   test
  end | __Stack].

-compile({inline,{yeccpars2_346_,1}}).
-file("megaco_text_parser_prev3c.yrl", 883).
yeccpars2_346_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { serviceStates , { equal , __3 } }
  end | __Stack].

-compile({inline,{yeccpars2_347_,1}}).
-file("megaco_text_parser_prev3c.yrl", 869).
yeccpars2_347_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_indAudTerminationStateDescriptor ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_348_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1105).
yeccpars2_348_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   setelement ( # 'PropertyParm' .name , __2 , __1 )
  end | __Stack].

-compile({inline,{yeccpars2_353_,1}}).
-file("megaco_text_parser_prev3c.yrl", 840).
yeccpars2_353_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'IndAudStreamDescriptor' { streamID = __3 ,
    streamParms = __5 }
  end | __Stack].

-compile({inline,{yeccpars2_355_,1}}).
-file("megaco_text_parser_prev3c.yrl", 863).
yeccpars2_355_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { prop , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_356_,1}}).
-file("megaco_text_parser_prev3c.yrl", 864).
yeccpars2_356_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { name , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_357_,1}}).
-file("megaco_text_parser_prev3c.yrl", 851).
yeccpars2_357_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_358_,1}}).
-file("megaco_text_parser_prev3c.yrl", 860).
yeccpars2_358_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   modeToken
  end | __Stack].

-compile({inline,{yeccpars2_359_,1}}).
-file("megaco_text_parser_prev3c.yrl", 858).
yeccpars2_359_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   reservedGroupToken
  end | __Stack].

-compile({inline,{yeccpars2_360_,1}}).
-file("megaco_text_parser_prev3c.yrl", 859).
yeccpars2_360_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   reservedValueToken
  end | __Stack].

-compile({inline,{yeccpars2_363_,1}}).
-file("megaco_text_parser_prev3c.yrl", 862).
yeccpars2_363_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { mode , { inequal , __3 } }
  end | __Stack].

-compile({inline,{yeccpars2_364_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1101).
yeccpars2_364_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   inactive
  end | __Stack].

-compile({inline,{yeccpars2_365_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1102).
yeccpars2_365_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   loopBack
  end | __Stack].

-compile({inline,{yeccpars2_366_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1099).
yeccpars2_366_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   recvOnly
  end | __Stack].

-compile({inline,{yeccpars2_367_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1098).
yeccpars2_367_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   sendOnly
  end | __Stack].

-compile({inline,{yeccpars2_368_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1100).
yeccpars2_368_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   sendRecv
  end | __Stack].

-compile({inline,{yeccpars2_369_,1}}).
-file("megaco_text_parser_prev3c.yrl", 861).
yeccpars2_369_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { mode , { equal , __3 } }
  end | __Stack].

-compile({inline,{yeccpars2_372_,1}}).
-file("megaco_text_parser_prev3c.yrl", 851).
yeccpars2_372_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_373_,1}}).
-file("megaco_text_parser_prev3c.yrl", 850).
yeccpars2_373_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_374_,1}}).
-file("megaco_text_parser_prev3c.yrl", 847).
yeccpars2_374_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_indAudLocalControlDescriptor ( [ __3 | __4 ] )
  end | __Stack].

-compile({inline,{yeccpars2_377_,1}}).
-file("megaco_text_parser_prev3c.yrl", 824).
yeccpars2_377_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_378_,1}}).
-file("megaco_text_parser_prev3c.yrl", 823).
yeccpars2_378_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_379_,1}}).
-file("megaco_text_parser_prev3c.yrl", 813).
yeccpars2_379_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_indAudMediaDescriptor ( [ __3 | __4 ] )
  end | __Stack].

-compile({inline,{yeccpars2_384_,1}}).
-file("megaco_text_parser_prev3c.yrl", 902).
yeccpars2_384_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'IndAudEventsDescriptor' { pkgdName = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_388_,1}}).
-file("megaco_text_parser_prev3c.yrl", 905).
yeccpars2_388_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'IndAudEventsDescriptor' { requestID = __3 ,
    pkgdName = __5 }
  end | __Stack].

-compile({inline,{yeccpars2_390_,1}}).
-file("megaco_text_parser_prev3c.yrl", 895).
yeccpars2_390_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_392_,1}}).
-file("megaco_text_parser_prev3c.yrl", 888).
yeccpars2_392_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_393_,1}}).
-file("megaco_text_parser_prev3c.yrl", 891).
yeccpars2_393_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   merge_indAudEventBufferDescriptor ( __1 , __2 )
  end | __Stack].

-compile({inline,{yeccpars2_395_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1268).
yeccpars2_395_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_NAME ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_397_,1}}).
-file("megaco_text_parser_prev3c.yrl", 898).
yeccpars2_397_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { streamID , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_398_,1}}).
-file("megaco_text_parser_prev3c.yrl", 899).
yeccpars2_398_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { eventParameterName , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_399_,1}}).
-file("megaco_text_parser_prev3c.yrl", 894).
yeccpars2_399_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_400_,1}}).
-file("megaco_text_parser_prev3c.yrl", 755).
yeccpars2_400_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_auditDescriptor ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_401_,1}}).
-file("megaco_text_parser_prev3c.yrl", 757).
yeccpars2_401_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_403_,1}}).
-file("megaco_text_parser_prev3c.yrl", 761).
yeccpars2_403_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_404_,1}}).
-file("megaco_text_parser_prev3c.yrl", 760).
yeccpars2_404_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_405_,1}}).
-file("megaco_text_parser_prev3c.yrl", 788).
yeccpars2_405_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_407_,1}}).
-file("megaco_text_parser_prev3c.yrl", 793).
yeccpars2_407_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_413_,1}}).
-file("megaco_text_parser_prev3c.yrl", 792).
yeccpars2_413_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_414_,1}}).
-file("megaco_text_parser_prev3c.yrl", 705).
yeccpars2_414_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_421_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1433).
yeccpars2_421_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { time_stamp , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_422_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1435).
yeccpars2_422_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { version , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_423_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1428).
yeccpars2_423_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { reason , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_424_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1431).
yeccpars2_424_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { profile , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_425_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1425).
yeccpars2_425_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_426_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1434).
yeccpars2_426_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { mgc_id , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_427_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1427).
yeccpars2_427_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { method , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_428_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1429).
yeccpars2_428_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { delay , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_429_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1430).
yeccpars2_429_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { address , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_430_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1532).
yeccpars2_430_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_extensionParameter ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_432_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1432).
yeccpars2_432_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { extension , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_433_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1437).
yeccpars2_433_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { audit_item , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_440_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1436).
yeccpars2_440_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   incomplete
  end | __Stack].

-compile({inline,{yeccpars2_441_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1485).
yeccpars2_441_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_timeStamp ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_444_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1454).
yeccpars2_444_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_version ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_445_,1}}).
-file("megaco_text_parser_prev3c.yrl", 482).
yeccpars2_445_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_446_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1448).
yeccpars2_446_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { portNumber , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_447_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1446).
yeccpars2_447_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_449_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1442).
yeccpars2_449_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_451_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1452).
yeccpars2_451_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_profile ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_452_,1}}).
-file("megaco_text_parser_prev3c.yrl", 482).
yeccpars2_452_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_453_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1450).
yeccpars2_453_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_455_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1440).
yeccpars2_455_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_serviceChangeMethod ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_457_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1444).
yeccpars2_457_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_uint32 ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_458_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1457).
yeccpars2_458_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   setelement ( # 'PropertyParm' .name , __2 , __1 )
  end | __Stack].

-compile({inline,{yeccpars2_461_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1425).
yeccpars2_461_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_462_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1424).
yeccpars2_462_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_463_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1421).
yeccpars2_463_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_ServiceChangeParm ( [ __3 | __4 ] )
  end | __Stack].

-compile({inline,{yeccpars2_464_,1}}).
-file("megaco_text_parser_prev3c.yrl", 965).
yeccpars2_464_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   make_commandRequest ( { serviceChangeReq , __1 } ,
    # 'ServiceChangeRequest' { terminationID = __3 ,
    serviceChangeParms = __5 } )
  end | __Stack].

-compile({inline,{yeccpars2_466_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1530).
yeccpars2_466_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_uint16 ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_470_,1}}).
-file("megaco_text_parser_prev3c.yrl", 952).
yeccpars2_470_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'NotifyRequest' { observedEventsDescriptor = __1 }
  end | __Stack].

-compile({inline,{yeccpars2_472_,1}}).
-file("megaco_text_parser_prev3c.yrl", 954).
yeccpars2_472_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'NotifyRequest' { errorDescriptor = __1 }
  end | __Stack].

-compile({inline,{yeccpars2_476_,1}}).
-file("megaco_text_parser_prev3c.yrl", 482).
yeccpars2_476_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_477_,1}}).
-file("megaco_text_parser_prev3c.yrl", 482).
yeccpars2_477_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_479_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1371).
yeccpars2_479_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_481_,1}}).
-file("megaco_text_parser_prev3c.yrl", 482).
yeccpars2_481_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_482_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1371).
yeccpars2_482_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_483_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1370).
yeccpars2_483_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_484_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1367).
yeccpars2_484_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'ObservedEventsDescriptor' { requestId = __3 ,
    observedEventLst = [ __5 | __6 ] }
  end | __Stack].

-compile({inline,{yeccpars2_485_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1383).
yeccpars2_485_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_486_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1378).
yeccpars2_486_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_observed_event ( __3 , __2 , asn1_NOVALUE )
  end | __Stack].

-compile({inline,{yeccpars2_488_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1386).
yeccpars2_488_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_491_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1266).
yeccpars2_491_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   select_stream_or_other ( __1 , __2 )
  end | __Stack].

-compile({inline,{yeccpars2_494_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1386).
yeccpars2_494_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_495_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1385).
yeccpars2_495_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_496_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1382).
yeccpars2_496_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_498_,1}}).
-file("megaco_text_parser_prev3c.yrl", 482).
yeccpars2_498_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_500_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1383).
yeccpars2_500_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_501_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1376).
yeccpars2_501_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_observed_event ( __6 , __5 , __1 )
  end | __Stack].

-compile({inline,{yeccpars2_502_,1}}).
-file("megaco_text_parser_prev3c.yrl", 947).
yeccpars2_502_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   NR = setelement ( # 'NotifyRequest' .terminationID ,
    __5 , __3 ) ,
    make_commandRequest ( { notifyReq , __1 } , NR )
  end | __Stack].

-compile({inline,{yeccpars2_504_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1525).
yeccpars2_504_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_505_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1095).
yeccpars2_505_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   false
  end | __Stack].

-compile({inline,{yeccpars2_506_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1094).
yeccpars2_506_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   true
  end | __Stack].

-compile({inline,{yeccpars2_508_,1}}).
-file("megaco_text_parser_prev3c.yrl", 600).
yeccpars2_508_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { select_prio , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_509_,1}}).
-file("megaco_text_parser_prev3c.yrl", 596).
yeccpars2_509_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { prop , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_511_,1}}).
-file("megaco_text_parser_prev3c.yrl", 602).
yeccpars2_511_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { select_ieps , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_512_,1}}).
-file("megaco_text_parser_prev3c.yrl", 601).
yeccpars2_512_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { select_emergency , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_514_,1}}).
-file("megaco_text_parser_prev3c.yrl", 589).
yeccpars2_514_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_516_,1}}).
-file("megaco_text_parser_prev3c.yrl", 603).
yeccpars2_516_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { select_logic , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_517_,1}}).
-file("megaco_text_parser_prev3c.yrl", 606).
yeccpars2_517_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { andAUDITSelect , 'NULL' }
  end | __Stack].

-compile({inline,{yeccpars2_519_,1}}).
-file("megaco_text_parser_prev3c.yrl", 593).
yeccpars2_519_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   emergencyAudit
  end | __Stack].

-compile({inline,{yeccpars2_521_,1}}).
-file("megaco_text_parser_prev3c.yrl", 595).
yeccpars2_521_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   iepsCallind
  end | __Stack].

-compile({inline,{yeccpars2_522_,1}}).
-file("megaco_text_parser_prev3c.yrl", 607).
yeccpars2_522_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { orAUDITSelect , 'NULL' }
  end | __Stack].

-compile({inline,{yeccpars2_523_,1}}).
-file("megaco_text_parser_prev3c.yrl", 594).
yeccpars2_523_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   priorityAudit
  end | __Stack].

-compile({inline,{yeccpars2_524_,1}}).
-file("megaco_text_parser_prev3c.yrl", 592).
yeccpars2_524_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   topologyAudit
  end | __Stack].

-compile({inline,{yeccpars2_526_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1528).
yeccpars2_526_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   false
  end | __Stack].

-compile({inline,{yeccpars2_527_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1527).
yeccpars2_527_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   true
  end | __Stack].

-compile({inline,{yeccpars2_530_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1407).
yeccpars2_530_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_531_,1}}).
-file("megaco_text_parser_prev3c.yrl", 596).
yeccpars2_531_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { prop , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_533_,1}}).
-file("megaco_text_parser_prev3c.yrl", 589).
yeccpars2_533_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_537_,1}}).
-file("megaco_text_parser_prev3c.yrl", 571).
yeccpars2_537_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_540_,1}}).
-file("megaco_text_parser_prev3c.yrl", 571).
yeccpars2_540_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_541_,1}}).
-file("megaco_text_parser_prev3c.yrl", 570).
yeccpars2_541_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_542_,1}}).
-file("megaco_text_parser_prev3c.yrl", 568).
yeccpars2_542_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __4 | __5 ]
  end | __Stack].

-compile({inline,{yeccpars2_545_,1}}).
-file("megaco_text_parser_prev3c.yrl", 589).
yeccpars2_545_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_546_,1}}).
-file("megaco_text_parser_prev3c.yrl", 588).
yeccpars2_546_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_547_,1}}).
-file("megaco_text_parser_prev3c.yrl", 585).
yeccpars2_547_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_548_,1}}).
-file("megaco_text_parser_prev3c.yrl", 565).
yeccpars2_548_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { contextList , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_549_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1405).
yeccpars2_549_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_551_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1407).
yeccpars2_551_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_553_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1406).
yeccpars2_553_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_554_,1}}).
-file("megaco_text_parser_prev3c.yrl", 563).
yeccpars2_554_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { contextProp , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_556_,1}}).
-file("megaco_text_parser_prev3c.yrl", 579).
yeccpars2_556_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_context_attr_audit_request (
    # 'ContextAttrAuditRequest' { } , [ __3 | __4 ] )
  end | __Stack].

-compile({inline,{yeccpars2_557_,1}}).
-file("megaco_text_parser_prev3c.yrl", 575).
yeccpars2_557_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_context_attr_audit_request (
    # 'ContextAttrAuditRequest' { } , __3 )
  end | __Stack].

-compile({inline,{yeccpars2_560_,1}}).
-file("megaco_text_parser_prev3c.yrl", 706).
yeccpars2_560_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_561_,1}}).
-file("megaco_text_parser_prev3c.yrl", 709).
yeccpars2_561_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   make_commandRequest ( { auditValueRequest , __1 } ,
    make_auditRequest ( __3 , __4 ) )
  end | __Stack].

-compile({inline,{yeccpars2_563_,1}}).
-file("megaco_text_parser_prev3c.yrl", 706).
yeccpars2_563_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_564_,1}}).
-file("megaco_text_parser_prev3c.yrl", 712).
yeccpars2_564_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   make_commandRequest ( { auditCapRequest , __1 } ,
    make_auditRequest ( __3 , __4 ) )
  end | __Stack].

-compile({inline,{yeccpars2_565_,1}}).
-file("megaco_text_parser_prev3c.yrl", 541).
yeccpars2_565_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_action_request ( __3 , __5 )
  end | __Stack].

-compile({inline,{yeccpars2_566_,1}}).
-file("megaco_text_parser_prev3c.yrl", 543).
yeccpars2_566_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_568_,1}}).
-file("megaco_text_parser_prev3c.yrl", 547).
yeccpars2_568_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_569_,1}}).
-file("megaco_text_parser_prev3c.yrl", 546).
yeccpars2_569_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_571_,1}}).
-file("megaco_text_parser_prev3c.yrl", 671).
yeccpars2_571_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_572_,1}}).
-file("megaco_text_parser_prev3c.yrl", 661).
yeccpars2_572_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   Descs = merge_AmmRequest_descriptors ( __4 , [ ] ) ,
    make_commandRequest ( __1 ,
    # 'AmmRequest' { terminationID = __3 ,
    descriptors = Descs } )
  end | __Stack].

-compile({inline,{yeccpars2_574_,1}}).
-file("megaco_text_parser_prev3c.yrl", 685).
yeccpars2_574_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { statisticsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_575_,1}}).
-file("megaco_text_parser_prev3c.yrl", 682).
yeccpars2_575_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { signalsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_576_,1}}).
-file("megaco_text_parser_prev3c.yrl", 679).
yeccpars2_576_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { muxDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_577_,1}}).
-file("megaco_text_parser_prev3c.yrl", 678).
yeccpars2_577_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { modemDescriptor , deprecated }
  end | __Stack].

-compile({inline,{yeccpars2_578_,1}}).
-file("megaco_text_parser_prev3c.yrl", 677).
yeccpars2_578_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { mediaDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_579_,1}}).
-file("megaco_text_parser_prev3c.yrl", 680).
yeccpars2_579_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { eventsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_580_,1}}).
-file("megaco_text_parser_prev3c.yrl", 681).
yeccpars2_580_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { eventBufferDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_581_,1}}).
-file("megaco_text_parser_prev3c.yrl", 683).
yeccpars2_581_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { digitMapDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_582_,1}}).
-file("megaco_text_parser_prev3c.yrl", 684).
yeccpars2_582_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { auditDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_583_,1}}).
-file("megaco_text_parser_prev3c.yrl", 674).
yeccpars2_583_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_584_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1413).
yeccpars2_584_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_DMD ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_585_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1144).
yeccpars2_585_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_586_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1181).
yeccpars2_586_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'EventsDescriptor' { requestID = asn1_NOVALUE ,
    eventList = [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_590_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1277).
yeccpars2_590_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_593_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1493).
yeccpars2_593_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_594_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1497).
yeccpars2_594_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'StatisticsParameter' { statName = __1 ,
    statValue = asn1_NOVALUE }
  end | __Stack].

-compile({inline,{yeccpars2_596_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1500).
yeccpars2_596_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'StatisticsParameter' { statName = __1 ,
    statValue = [ __3 ] }
  end | __Stack].

-compile({inline,{yeccpars2_598_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1141).
yeccpars2_598_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_600_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1503).
yeccpars2_600_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'StatisticsParameter' { statName = __1 ,
    statValue = [ __4 | __5 ] }
  end | __Stack].

-compile({inline,{yeccpars2_603_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1493).
yeccpars2_603_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_604_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1492).
yeccpars2_604_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_605_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1490).
yeccpars2_605_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_607_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1283).
yeccpars2_607_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { signal , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_608_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1280).
yeccpars2_608_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_609_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1282).
yeccpars2_609_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { seqSigList , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_614_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1355).
yeccpars2_614_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_617_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1355).
yeccpars2_617_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_618_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1354).
yeccpars2_618_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_619_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1350).
yeccpars2_619_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'SeqSigList' { id = ensure_uint16 ( __3 ) ,
    signalList = [ __5 | __6 ] }
  end | __Stack].

-compile({inline,{yeccpars2_622_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1280).
yeccpars2_622_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_623_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1279).
yeccpars2_623_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_624_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1276).
yeccpars2_624_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_626_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1174).
yeccpars2_626_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_muxType ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_628_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1171).
yeccpars2_628_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'MuxDescriptor' { muxType = __3 ,
    termList = __4 }
  end | __Stack].

-compile({inline,{yeccpars2_630_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1032).
yeccpars2_630_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_632_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1028).
yeccpars2_632_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_635_,1}}).
-file("megaco_text_parser_prev3c.yrl", 0).
yeccpars2_635_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_636_,1}}).
-file("megaco_text_parser_prev3c.yrl", 0).
yeccpars2_636_(__Stack0) ->
 [begin
   '$undefined'
  end | __Stack0].

-compile({inline,{yeccpars2_639_,1}}).
-file("megaco_text_parser_prev3c.yrl", 0).
yeccpars2_639_(__Stack0) ->
 [begin
   '$undefined'
  end | __Stack0].

-compile({inline,{yeccpars2_640_,1}}).
-file("megaco_text_parser_prev3c.yrl", 0).
yeccpars2_640_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_641_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1403).
yeccpars2_641_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_642_,1}}).
-file("megaco_text_parser_prev3c.yrl", 0).
yeccpars2_642_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_644_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1407).
yeccpars2_644_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_646_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1402).
yeccpars2_646_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_647_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1403).
yeccpars2_647_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_648_,1}}).
-file("megaco_text_parser_prev3c.yrl", 0).
yeccpars2_648_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_650_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1053).
yeccpars2_650_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { termState , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_651_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1049).
yeccpars2_651_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { streamParm , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_652_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1051).
yeccpars2_652_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { streamDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_653_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1064).
yeccpars2_653_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { statistics , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_654_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1043).
yeccpars2_654_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_655_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1063).
yeccpars2_655_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { control , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_657_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1058).
yeccpars2_657_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   PGs = ensure_prop_groups ( __1 ) ,
    { local , # 'LocalRemoteDescriptor' { propGrps = PGs } }
  end | __Stack].

-compile({inline,{yeccpars2_658_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1061).
yeccpars2_658_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   PGs = ensure_prop_groups ( __1 ) ,
    { remote , # 'LocalRemoteDescriptor' { propGrps = PGs } }
  end | __Stack].

-compile({inline,{yeccpars2_662_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1086).
yeccpars2_662_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_663_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1155).
yeccpars2_663_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { serviceState , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_664_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1157).
yeccpars2_664_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { propertyParm , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_665_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1156).
yeccpars2_665_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { eventBufferControl , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_669_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1159).
yeccpars2_669_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_671_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1165).
yeccpars2_671_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_672_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1168).
yeccpars2_672_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   lockStep
  end | __Stack].

-compile({inline,{yeccpars2_673_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1167).
yeccpars2_673_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   off
  end | __Stack].

-compile({inline,{yeccpars2_676_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1086).
yeccpars2_676_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_677_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1085).
yeccpars2_677_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_678_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1083).
yeccpars2_678_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_terminationStateDescriptor ( [ __3 | __4 ] )
  end | __Stack].

-compile({inline,{yeccpars2_682_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1072).
yeccpars2_682_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_685_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1072).
yeccpars2_685_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_686_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1071).
yeccpars2_686_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_687_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1068).
yeccpars2_687_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'StreamDescriptor' { streamID = __3 ,
    streamParms = merge_streamParms ( [ __5 | __6 ] ) }
  end | __Stack].

-compile({inline,{yeccpars2_689_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1092).
yeccpars2_689_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { prop , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_690_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1078).
yeccpars2_690_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_695_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1090).
yeccpars2_695_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { value , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_697_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1089).
yeccpars2_697_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { group , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_699_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1091).
yeccpars2_699_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { mode , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_702_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1078).
yeccpars2_702_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_703_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1077).
yeccpars2_703_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_704_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1075).
yeccpars2_704_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_707_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1043).
yeccpars2_707_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_708_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1042).
yeccpars2_708_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_709_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1040).
yeccpars2_709_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_mediaDescriptor ( [ __3 | __4 ] )
  end | __Stack].

-compile({inline,{yeccpars2_713_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1189).
yeccpars2_713_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_714_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1196).
yeccpars2_714_(__Stack0) ->
 [begin
   # 'RequestedEvent' { evParList = [ ] }
  end | __Stack0].

-compile({inline,{yeccpars2_715_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1192).
yeccpars2_715_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   setelement ( # 'RequestedEvent' .pkgdName , __2 , __1 )
  end | __Stack].

-compile({inline,{yeccpars2_717_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1208).
yeccpars2_717_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { notifyRegulated , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_718_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1220).
yeccpars2_718_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { notifyBehaviour , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_720_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1212).
yeccpars2_720_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_724_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1272).
yeccpars2_724_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_eventDM ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_726_COMMA,1}}).
-file("megaco_text_parser_prev3c.yrl", 1215).
yeccpars2_726_COMMA(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   keepActive
  end | __Stack].

-compile({inline,{yeccpars2_726_RBRKT,1}}).
-file("megaco_text_parser_prev3c.yrl", 1215).
yeccpars2_726_RBRKT(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   keepActive
  end | __Stack].

-compile({inline,{yeccpars2_727_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1207).
yeccpars2_727_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { neverNotify , 'NULL' }
  end | __Stack].

-compile({inline,{yeccpars2_728_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1206).
yeccpars2_728_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { notifyImmediate , 'NULL' }
  end | __Stack].

-compile({inline,{yeccpars2_729_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1200).
yeccpars2_729_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'RegulatedEmbeddedDescriptor' { }
  end | __Stack].

-compile({inline,{yeccpars2_730_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1221).
yeccpars2_730_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   resetEventsDescriptor
  end | __Stack].

-compile({inline,{yeccpars2_738_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1233).
yeccpars2_738_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'SecondEventsDescriptor' { requestID = asn1_NOVALUE ,
    eventList = [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_742_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1241).
yeccpars2_742_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_743_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1249).
yeccpars2_743_(__Stack0) ->
 [begin
   # 'SecondRequestedEvent' { evParList = [ ] }
  end | __Stack0].

-compile({inline,{yeccpars2_744_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1245).
yeccpars2_744_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   setelement ( # 'SecondRequestedEvent' .pkgdName , __2 , __1 )
  end | __Stack].

-compile({inline,{yeccpars2_746_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1252).
yeccpars2_746_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_747_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1259).
yeccpars2_747_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { notifyBehaviour , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_752_COMMA,1}}).
-file("megaco_text_parser_prev3c.yrl", 1255).
yeccpars2_752_COMMA(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   keepActive
  end | __Stack].

-compile({inline,{yeccpars2_752_RBRKT,1}}).
-file("megaco_text_parser_prev3c.yrl", 1255).
yeccpars2_752_RBRKT(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   keepActive
  end | __Stack].

-compile({inline,{yeccpars2_753_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1260).
yeccpars2_753_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   resetEventsDescriptor
  end | __Stack].

-compile({inline,{yeccpars2_756_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1263).
yeccpars2_756_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { second_embed , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_759_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1252).
yeccpars2_759_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_760_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1251).
yeccpars2_760_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_761_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1248).
yeccpars2_761_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_secondEventParameters ( [ __2 | __3 ] )
  end | __Stack].

-compile({inline,{yeccpars2_764_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1241).
yeccpars2_764_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_765_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1240).
yeccpars2_765_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_766_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1237).
yeccpars2_766_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'SecondEventsDescriptor' { requestID = __3 ,
    eventList = [ __5 | __6 ] }
  end | __Stack].

-compile({inline,{yeccpars2_767_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1230).
yeccpars2_767_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { embed , asn1_NOVALUE , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_769_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1227).
yeccpars2_769_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { embed , __3 , asn1_NOVALUE }
  end | __Stack].

-compile({inline,{yeccpars2_771_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1225).
yeccpars2_771_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { embed , __3 , __5 }
  end | __Stack].

-compile({inline,{yeccpars2_772_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1204).
yeccpars2_772_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   make_RegulatedEmbeddedDescriptor ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_773_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1202).
yeccpars2_773_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   make_RegulatedEmbeddedDescriptor ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_776_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1212).
yeccpars2_776_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_777_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1211).
yeccpars2_777_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_778_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1195).
yeccpars2_778_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_eventParameters ( [ __2 | __3 ] )
  end | __Stack].

-compile({inline,{yeccpars2_781_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1189).
yeccpars2_781_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_782_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1188).
yeccpars2_782_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_783_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1185).
yeccpars2_783_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'EventsDescriptor' { requestID = __3 ,
    eventList = [ __5 | __6 ] }
  end | __Stack].

-compile({inline,{yeccpars2_784_,1}}).
-file("megaco_text_parser_prev3c.yrl", 482).
yeccpars2_784_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_785_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1152).
yeccpars2_785_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   merge_eventSpec ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_786_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1150).
yeccpars2_786_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_788_,1}}).
-file("megaco_text_parser_prev3c.yrl", 482).
yeccpars2_788_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_789_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1150).
yeccpars2_789_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_790_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1149).
yeccpars2_790_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_791_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1147).
yeccpars2_791_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_794_,1}}).
-file("megaco_text_parser_prev3c.yrl", 674).
yeccpars2_794_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_795_,1}}).
-file("megaco_text_parser_prev3c.yrl", 673).
yeccpars2_795_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_796_,1}}).
-file("megaco_text_parser_prev3c.yrl", 670).
yeccpars2_796_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_799_,1}}).
-file("megaco_text_parser_prev3c.yrl", 530).
yeccpars2_799_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_800_,1}}).
-file("megaco_text_parser_prev3c.yrl", 529).
yeccpars2_800_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_801_,1}}).
-file("megaco_text_parser_prev3c.yrl", 518).
yeccpars2_801_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'TransactionRequest' { transactionId = asn1_NOVALUE ,
    actions = [ __3 | __4 ] }
  end | __Stack].

-compile({inline,{yeccpars2_803_,1}}).
-file("megaco_text_parser_prev3c.yrl", 992).
yeccpars2_803_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_uint32 ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_805_,1}}).
-file("megaco_text_parser_prev3c.yrl", 530).
yeccpars2_805_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_807_,1}}).
-file("megaco_text_parser_prev3c.yrl", 522).
yeccpars2_807_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'TransactionRequest' { transactionId = asn1_NOVALUE ,
    actions = [ __4 | __5 ] }
  end | __Stack].

-compile({inline,{yeccpars2_809_,1}}).
-file("megaco_text_parser_prev3c.yrl", 530).
yeccpars2_809_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_811_,1}}).
-file("megaco_text_parser_prev3c.yrl", 526).
yeccpars2_811_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'TransactionRequest' { transactionId = ensure_transactionID ( __3 ) ,
    actions = [ __5 | __6 ] }
  end | __Stack].

-compile({inline,{yeccpars2_813_,1}}).
-file("megaco_text_parser_prev3c.yrl", 509).
yeccpars2_813_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_814_,1}}).
-file("megaco_text_parser_prev3c.yrl", 511).
yeccpars2_814_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_transactionAck ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_817_,1}}).
-file("megaco_text_parser_prev3c.yrl", 509).
yeccpars2_817_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_818_,1}}).
-file("megaco_text_parser_prev3c.yrl", 508).
yeccpars2_818_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_819_,1}}).
-file("megaco_text_parser_prev3c.yrl", 505).
yeccpars2_819_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_822_,1}}).
-file("megaco_text_parser_prev3c.yrl", 624).
yeccpars2_822_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_825_,1}}).
-file("megaco_text_parser_prev3c.yrl", 623).
yeccpars2_825_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   'NULL'
  end | __Stack].

-compile({inline,{yeccpars2_827_,1}}).
-file("megaco_text_parser_prev3c.yrl", 626).
yeccpars2_827_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { transactionError , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_828_,1}}).
-file("megaco_text_parser_prev3c.yrl", 630).
yeccpars2_828_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_831_,1}}).
-file("megaco_text_parser_prev3c.yrl", 636).
yeccpars2_831_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'ActionReply' { contextId = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_833_,1}}).
-file("megaco_text_parser_prev3c.yrl", 653).
yeccpars2_833_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { command , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_834_,1}}).
-file("megaco_text_parser_prev3c.yrl", 656).
yeccpars2_834_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { command , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_835_,1}}).
-file("megaco_text_parser_prev3c.yrl", 639).
yeccpars2_835_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'ActionReply' { errorDescriptor = __1 }
  end | __Stack].

-compile({inline,{yeccpars2_836_,1}}).
-file("megaco_text_parser_prev3c.yrl", 657).
yeccpars2_836_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { context , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_837_,1}}).
-file("megaco_text_parser_prev3c.yrl", 651).
yeccpars2_837_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_838_,1}}).
-file("megaco_text_parser_prev3c.yrl", 654).
yeccpars2_838_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { command , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_840_,1}}).
-file("megaco_text_parser_prev3c.yrl", 655).
yeccpars2_840_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { command , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_842_,1}}).
-file("megaco_text_parser_prev3c.yrl", 691).
yeccpars2_842_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   addReply
  end | __Stack].

-compile({inline,{yeccpars2_845_,1}}).
-file("megaco_text_parser_prev3c.yrl", 693).
yeccpars2_845_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   modReply
  end | __Stack].

-compile({inline,{yeccpars2_846_,1}}).
-file("megaco_text_parser_prev3c.yrl", 692).
yeccpars2_846_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   moveReply
  end | __Stack].

-compile({inline,{yeccpars2_849_,1}}).
-file("megaco_text_parser_prev3c.yrl", 694).
yeccpars2_849_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   subtractReply
  end | __Stack].

-compile({inline,{yeccpars2_851_,1}}).
-file("megaco_text_parser_prev3c.yrl", 980).
yeccpars2_851_(__Stack0) ->
 [begin
   { serviceChangeResParms , # 'ServiceChangeResParm' { } }
  end | __Stack0].

-compile({inline,{yeccpars2_852_,1}}).
-file("megaco_text_parser_prev3c.yrl", 971).
yeccpars2_852_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { serviceChangeReply ,
    # 'ServiceChangeReply' { terminationID = __3 ,
    serviceChangeResult = __4 } }
  end | __Stack].

-compile({inline,{yeccpars2_858_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1474).
yeccpars2_858_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { time_stamp , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_859_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1473).
yeccpars2_859_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { version , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_860_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1472).
yeccpars2_860_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { profile , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_861_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1471).
yeccpars2_861_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { mgc_id , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_862_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1470).
yeccpars2_862_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { address , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_863_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1468).
yeccpars2_863_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_870_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1468).
yeccpars2_870_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_871_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1467).
yeccpars2_871_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_872_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1464).
yeccpars2_872_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_ServiceChangeResParm ( [ __3 | __4 ] )
  end | __Stack].

-compile({inline,{yeccpars2_873_,1}}).
-file("megaco_text_parser_prev3c.yrl", 976).
yeccpars2_873_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { errorDescriptor , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_874_,1}}).
-file("megaco_text_parser_prev3c.yrl", 978).
yeccpars2_874_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { serviceChangeResParms , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_876_,1}}).
-file("megaco_text_parser_prev3c.yrl", 961).
yeccpars2_876_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_877_,1}}).
-file("megaco_text_parser_prev3c.yrl", 957).
yeccpars2_877_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { notifyReply , # 'NotifyReply' { terminationID = __3 ,
    errorDescriptor = __4 } }
  end | __Stack].

-compile({inline,{yeccpars2_880_,1}}).
-file("megaco_text_parser_prev3c.yrl", 960).
yeccpars2_880_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_882_,1}}).
-file("megaco_text_parser_prev3c.yrl", 730).
yeccpars2_882_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   merge_auditOther ( __1 , [ ] )
  end | __Stack].

-compile({inline,{yeccpars2_883_,1}}).
-file("megaco_text_parser_prev3c.yrl", 720).
yeccpars2_883_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { auditValueReply , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_885_,1}}).
-file("megaco_text_parser_prev3c.yrl", 725).
yeccpars2_885_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { contextAuditResult , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_886_,1}}).
-file("megaco_text_parser_prev3c.yrl", 716).
yeccpars2_886_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { auditValueReply , __4 }
  end | __Stack].

-compile({inline,{yeccpars2_891_,1}}).
-file("megaco_text_parser_prev3c.yrl", 987).
yeccpars2_891_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_uint ( __1 , 0 , 999 )
  end | __Stack].

-compile({inline,{yeccpars2_893_,1}}).
-file("megaco_text_parser_prev3c.yrl", 990).
yeccpars2_893_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_895_,1}}).
-file("megaco_text_parser_prev3c.yrl", 989).
yeccpars2_895_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   value_of ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_896_,1}}).
-file("megaco_text_parser_prev3c.yrl", 984).
yeccpars2_896_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'ErrorDescriptor' { errorCode = __3 ,
    errorText = __5 }
  end | __Stack].

-compile({inline,{yeccpars2_897_,1}}).
-file("megaco_text_parser_prev3c.yrl", 727).
yeccpars2_897_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { error , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_900_,1}}).
-file("megaco_text_parser_prev3c.yrl", 749).
yeccpars2_900_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { statisticsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_901_,1}}).
-file("megaco_text_parser_prev3c.yrl", 745).
yeccpars2_901_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { signalsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_902_,1}}).
-file("megaco_text_parser_prev3c.yrl", 750).
yeccpars2_902_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { packagesDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_903_,1}}).
-file("megaco_text_parser_prev3c.yrl", 747).
yeccpars2_903_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { observedEventsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_904_,1}}).
-file("megaco_text_parser_prev3c.yrl", 743).
yeccpars2_904_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { muxDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_905_,1}}).
-file("megaco_text_parser_prev3c.yrl", 0).
yeccpars2_905_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_906_,1}}).
-file("megaco_text_parser_prev3c.yrl", 741).
yeccpars2_906_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { mediaDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_907_,1}}).
-file("megaco_text_parser_prev3c.yrl", 744).
yeccpars2_907_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { eventsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_908_,1}}).
-file("megaco_text_parser_prev3c.yrl", 748).
yeccpars2_908_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { eventBufferDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_909_,1}}).
-file("megaco_text_parser_prev3c.yrl", 751).
yeccpars2_909_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { errorDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_910_,1}}).
-file("megaco_text_parser_prev3c.yrl", 746).
yeccpars2_910_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { digitMapDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_911_,1}}).
-file("megaco_text_parser_prev3c.yrl", 739).
yeccpars2_911_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_912_,1}}).
-file("megaco_text_parser_prev3c.yrl", 752).
yeccpars2_912_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { auditReturnItem , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_913_,1}}).
-file("megaco_text_parser_prev3c.yrl", 767).
yeccpars2_913_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   mediaToken
  end | __Stack].

-compile({inline,{yeccpars2_914_,1}}).
-file("megaco_text_parser_prev3c.yrl", 766).
yeccpars2_914_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   modemToken
  end | __Stack].

-compile({inline,{yeccpars2_915_,1}}).
-file("megaco_text_parser_prev3c.yrl", 765).
yeccpars2_915_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   muxToken
  end | __Stack].

-compile({inline,{yeccpars2_916_,1}}).
-file("megaco_text_parser_prev3c.yrl", 770).
yeccpars2_916_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   observedEventsToken
  end | __Stack].

-compile({inline,{yeccpars2_917_,1}}).
-file("megaco_text_parser_prev3c.yrl", 771).
yeccpars2_917_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   packagesToken
  end | __Stack].

-compile({inline,{yeccpars2_918_,1}}).
-file("megaco_text_parser_prev3c.yrl", 769).
yeccpars2_918_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   statsToken
  end | __Stack].

-compile({inline,{yeccpars2_920_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1481).
yeccpars2_920_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_923_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1481).
yeccpars2_923_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_924_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1480).
yeccpars2_924_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_925_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1478).
yeccpars2_925_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_926_,1}}).
-file("megaco_text_parser_prev3c.yrl", 736).
yeccpars2_926_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   merge_terminationAudit ( [ __1 | __2 ] )
  end | __Stack].

-compile({inline,{yeccpars2_928_,1}}).
-file("megaco_text_parser_prev3c.yrl", 739).
yeccpars2_928_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_929_,1}}).
-file("megaco_text_parser_prev3c.yrl", 738).
yeccpars2_929_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_930_,1}}).
-file("megaco_text_parser_prev3c.yrl", 732).
yeccpars2_930_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_auditOther ( __1 , __3 )
  end | __Stack].

-compile({inline,{yeccpars2_932_,1}}).
-file("megaco_text_parser_prev3c.yrl", 722).
yeccpars2_932_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { auditCapReply , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_934_,1}}).
-file("megaco_text_parser_prev3c.yrl", 718).
yeccpars2_934_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { auditCapReply , __4 }
  end | __Stack].

-compile({inline,{yeccpars2_935_,1}}).
-file("megaco_text_parser_prev3c.yrl", 634).
yeccpars2_935_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   setelement ( # 'ActionReply' .contextId , __5 , __3 )
  end | __Stack].

-compile({inline,{yeccpars2_937_,1}}).
-file("megaco_text_parser_prev3c.yrl", 697).
yeccpars2_937_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_938_,1}}).
-file("megaco_text_parser_prev3c.yrl", 688).
yeccpars2_938_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __1 , # 'AmmsReply' { terminationID = __3 ,
    terminationAudit = __4 } }
  end | __Stack].

-compile({inline,{yeccpars2_941_,1}}).
-file("megaco_text_parser_prev3c.yrl", 696).
yeccpars2_941_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_942_,1}}).
-file("megaco_text_parser_prev3c.yrl", 641).
yeccpars2_942_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   merge_action_reply ( [ __1 | __2 ] )
  end | __Stack].

-compile({inline,{yeccpars2_944_,1}}).
-file("megaco_text_parser_prev3c.yrl", 648).
yeccpars2_944_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ { error , __2 } ]
  end | __Stack].

-compile({inline,{yeccpars2_945_,1}}).
-file("megaco_text_parser_prev3c.yrl", 651).
yeccpars2_945_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_946_,1}}).
-file("megaco_text_parser_prev3c.yrl", 650).
yeccpars2_946_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_947_,1}}).
-file("megaco_text_parser_prev3c.yrl", 627).
yeccpars2_947_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { actionReplies , [ __1 | __2 ] }
  end | __Stack].

-compile({inline,{yeccpars2_949_,1}}).
-file("megaco_text_parser_prev3c.yrl", 630).
yeccpars2_949_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_950_,1}}).
-file("megaco_text_parser_prev3c.yrl", 629).
yeccpars2_950_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_951_,1}}).
-file("megaco_text_parser_prev3c.yrl", 619).
yeccpars2_951_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'TransactionReply' { transactionId = __3 ,
    immAckRequired = __5 ,
    transactionResult = __6 }
  end | __Stack].

-compile({inline,{yeccpars2_955_,1}}).
-file("megaco_text_parser_prev3c.yrl", 514).
yeccpars2_955_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'TransactionPending' { transactionId = ensure_transactionID ( __3 ) }
  end | __Stack].

-compile({inline,{yeccpars2_956_,1}}).
-file("megaco_text_parser_prev3c.yrl", 496).
yeccpars2_956_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_957_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1035).
yeccpars2_957_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_pathName ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_958_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1004).
yeccpars2_958_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { deviceName , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_959_,1}}).
-file("megaco_text_parser_prev3c.yrl", 482).
yeccpars2_959_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_960_,1}}).
-file("megaco_text_parser_prev3c.yrl", 482).
yeccpars2_960_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_961_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1021).
yeccpars2_961_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_mtpAddress ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_962_,1}}).
-file("megaco_text_parser_prev3c.yrl", 997).
yeccpars2_962_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_963_,1}}).
-file("megaco_text_parser_prev3c.yrl", 996).
yeccpars2_963_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].


-file("megaco_text_parser_prev3c.yrl", 1674).
