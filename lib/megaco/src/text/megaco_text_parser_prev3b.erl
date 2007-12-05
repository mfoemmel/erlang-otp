-module(megaco_text_parser_prev3b).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("megaco_text_parser_prev3b.yrl", 1559).

%% The following directive is needed for (significantly) faster compilation
%% of the generated .erl file by the HiPE compiler.  Please do not remove.
-compile([{hipe,[{regalloc,linear_scan}]}]).

-include("megaco_text_parser_prev3b.hrl").



-file("/ldisk/daily_build/otp_prebuild_r12b.2007-12-04_15/otp_src_R12B-0/bootstrap/lib/parsetools/include/yeccpre.hrl", 0).
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
-spec(return_error/2 :: (integer(), any()) -> no_return()).
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.2").

yeccpars0(Tokens, MFA) ->
    try yeccpars1(Tokens, MFA, 0, [], [])
    catch 
        error: Error ->
            Stacktrace = erlang:get_stacktrace(),
            try yecc_error_type(Error, Stacktrace) of
                {syntax_error, Token} ->
                    yeccerror(Token);
                {missing_in_goto_table=Tag, State} ->
                    Desc = {State, Tag},
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                Stacktrace);
                {missing_in_goto_table=Tag, Symbol, State} ->
                    Desc = {Symbol, State, Tag},
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        throw: {error, {_Line, ?MODULE, _M}} = Error -> 
            Error % probably from return_error/2
    end.

yecc_error_type(function_clause, [{?MODULE,F,[_,_,_,_,Token,_,_]} | _]) ->
    "yeccpars2" ++ _ = atom_to_list(F),
    {syntax_error, Token};
yecc_error_type({case_clause,{State}}, [{?MODULE,yeccpars2,_}|_]) ->
    %% Inlined goto-function
    {missing_in_goto_table, State};
yecc_error_type(function_clause, [{?MODULE,F,[State]}|_]) ->
    "yeccgoto_" ++ SymbolL = atom_to_list(F),
    {ok,[{atom,_,Symbol}]} = erl_scan:string(SymbolL),
    {missing_in_goto_table, Symbol, State}.

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



-file("./megaco_text_parser_prev3b.erl", 157).

yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(147=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(164=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_164(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(165=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(166=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(167=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(168=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(169=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(170=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(171=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_171(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(172=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(173=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(174=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(175=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(176=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(177=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(178=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(179=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(180=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(181=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(182=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(183=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(184=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(185=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_185(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(186=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(187=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_187(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(188=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(189=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(190=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_190(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(191=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_191(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(192=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(193=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_193(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(194=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_194(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(195=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_195(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(196=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_196(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(197=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_197(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(198=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(199=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(200=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(201=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(202=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(203=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(204=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(205=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(206=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(207=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_207(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(208=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(209=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_209(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(217=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(218=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_218(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(219=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_219(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(220=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_220(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(221=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(222=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(223=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_223(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(224=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_224(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(225=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_225(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(226=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_226(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(227=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(228=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(229=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(230=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(231=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_231(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(232=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(233=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_233(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(234=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_234(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(235=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_235(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(236=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_236(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(237=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_237(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(238=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_238(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(239=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_239(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(240=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_240(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(241=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_241(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(242=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_242(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(243=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(244=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(245=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_245(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(246=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_246(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(247=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_253(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(254=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(255=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(256=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_256(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(257=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_257(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(258=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(259=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_259(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(260=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_260(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(261=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_261(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(262=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_262(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(263=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_263(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(264=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_264(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(265=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_265(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(266=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_259(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(267=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_267(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(268=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_268(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(269=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_269(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(270=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(271=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_271(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(272=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_272(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(273=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_273(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(274=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_274(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(275=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_275(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(276=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_276(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(277=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_277(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(278=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_278(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(279=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_279(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(280=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_279(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(281=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_279(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(282=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_282(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(283=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_283(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(284=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_284(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(285=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_285(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(286=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_286(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(287=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_287(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(288=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_288(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(289=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_279(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(290=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_279(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(291=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_291(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(292=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_292(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(293=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_279(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(294=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_279(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(295=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_295(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(296=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_296(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(297=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_297(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(298=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_298(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(299=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_299(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(300=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_300(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(301=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_301(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(302=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_302(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(303=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_303(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(304=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_237(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(305=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_305(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(306=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_306(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(307=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_307(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(308=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(309=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_309(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(310=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_310(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(311=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_311(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(312=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_312(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(313=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_313(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(314=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_314(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(315=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_315(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(316=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_316(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(317=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_317(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(318=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_318(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(319=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_319(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(320=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_320(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(321=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_321(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(322=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_322(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(323=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(324=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_324(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(325=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_325(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(326=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_326(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(327=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(328=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_328(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(329=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_329(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(330=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_330(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(331=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_331(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(332=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(333=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_333(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(334=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_334(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(335=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_335(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(336=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(337=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_337(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(338=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_338(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(339=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_339(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(340=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_340(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(341=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_312(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(342=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(343=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(344=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(345=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(346=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_346(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(347=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(348=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_348(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(349=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(350=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(351=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(352=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_352(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(353=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_353(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(354=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_354(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(355=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_355(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(356=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_356(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(357=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_357(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(358=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_358(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(359=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_359(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(360=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_360(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(361=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_361(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(362=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(363=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_363(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(369=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_369(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(370=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_370(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(371=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_371(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(372=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_372(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(373=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_373(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(374=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_374(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(375=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_375(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(376=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_376(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(377=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_377(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(378=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_378(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(379=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_379(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(380=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(381=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_381(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(382=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_382(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(383=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_383(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(384=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_384(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(385=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_385(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(386=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_386(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(387=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_387(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(388=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_388(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(389=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_389(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(390=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_390(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(391=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_391(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(392=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_392(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(393=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_393(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(394=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_394(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(395=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_395(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(396=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_239(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(397=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_397(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(398=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_398(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(399=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_399(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(400=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_400(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(401=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_401(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(402=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_402(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(403=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_403(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(404=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_404(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(405=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_405(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(406=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_406(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(407=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_407(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(408=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(409=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_409(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(410=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_410(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(411=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_411(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(412=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_412(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(413=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_279(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(414=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_414(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(415=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(416=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_416(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(417=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_417(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(418=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_418(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(419=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(420=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_420(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(421=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(422=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_422(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(423=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_423(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(424=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_424(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(425=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_385(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(426=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_426(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(427=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_427(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(428=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_428(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(429=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_429(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(430=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(431=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_431(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(432=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(433=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_433(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(440=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_440(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(441=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_441(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(442=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_442(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(443=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(444=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_444(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(445=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_445(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(446=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_446(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(447=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_447(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(448=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_448(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(449=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_449(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(450=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_450(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(451=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_451(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(452=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(453=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_453(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(454=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_454(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(455=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_239(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(456=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_456(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(457=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_457(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(458=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(459=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_459(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(460=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_460(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(461=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_461(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(462=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_462(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(463=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_463(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(464=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(465=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_465(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(466=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_466(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(467=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_467(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(468=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_468(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(469=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_469(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(470=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_470(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(471=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_471(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(472=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_472(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(473=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_473(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(474=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_474(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(475=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_475(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(476=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_476(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(477=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_477(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(478=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_478(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(479=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_479(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(480=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_480(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(481=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_481(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(482=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_482(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(483=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_475(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(484=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_484(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(485=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_485(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(486=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_486(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(487=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_487(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(488=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_488(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(489=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_489(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(490=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_490(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(491=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_239(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(492=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_492(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(493=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_493(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(494=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_494(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(495=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(496=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_496(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(497=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_497(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(498=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(499=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_499(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(500=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_500(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(501=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_501(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(502=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_502(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(503=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_503(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(504=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_504(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(505=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(506=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_506(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(507=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_507(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(508=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_508(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(509=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(510=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_510(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(511=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_511(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(512=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(513=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_513(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(514=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_514(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(515=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_515(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(516=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_516(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(517=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(518=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_518(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(519=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_519(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(520=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(529=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_529(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(530=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_530(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(531=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_531(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(532=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_532(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(533=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_533(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(534=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_534(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(535=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_535(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(536=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_536(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(537=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_537(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(538=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_538(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(539=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_539(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(540=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_540(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(541=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_541(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(542=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(543=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_543(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(544=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_544(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(545=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_279(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(546=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_546(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(547=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_547(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(548=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(549=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_549(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(550=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_550(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(551=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_551(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(552=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_552(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(553=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_553(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(554=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_554(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(555=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_555(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(556=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_556(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(557=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(558=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_558(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(559=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(560=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_560(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(561=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_561(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(562=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(563=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_563(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(564=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_564(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(565=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_565(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(566=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_566(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(567=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_552(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(568=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_568(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(569=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_569(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(570=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_570(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(571=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(572=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_572(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(573=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_573(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(574=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_574(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(575=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(576=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_576(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(577=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_577(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(578=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(579=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_579(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(580=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_580(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(581=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_581(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(582=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(583=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(584=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_584(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(585=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_585(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(586=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_586(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(587=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(593=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_593(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(594=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_594(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(595=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_595(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(596=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_596(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(597=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_597(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(598=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_598(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(599=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_599(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(600=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_600(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(601=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_601(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(602=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_602(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(603=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_603(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(604=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_604(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(605=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_605(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(606=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_606(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(607=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_607(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(608=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_608(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(609=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_609(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(610=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_610(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(611=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_611(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(612=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_612(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(613=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_613(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(614=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_614(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(615=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_615(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(616=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_616(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(617=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_617(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(618=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_618(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(619=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_619(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(620=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_620(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(621=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_621(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(622=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_622(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(623=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_623(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(624=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_624(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(625=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_625(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(626=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_626(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(627=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_610(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(628=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_628(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(629=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_629(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(630=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_630(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(631=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(632=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_632(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(633=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_633(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(634=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_634(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(635=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_635(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(636=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_633(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(637=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_637(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(638=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_638(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(639=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_639(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(640=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_640(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(641=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_641(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(642=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_642(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(643=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_643(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(644=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_644(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(645=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_645(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(646=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_468(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(647=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_647(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(648=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_468(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(649=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_649(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(650=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_650(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(651=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_651(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(652=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_652(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(653=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_653(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(654=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_654(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(655=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_655(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(656=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_656(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(657=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_657(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(658=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_640(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(659=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_659(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(660=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_660(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(661=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_661(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(662=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_662(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(663=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_598(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(664=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_664(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(665=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_665(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(666=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_666(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(667=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(668=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_668(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(669=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(670=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_670(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(671=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_671(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(672=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_672(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(673=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_673(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(674=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_674(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(675=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_675(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(676=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_676(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(677=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_677(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(678=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_678(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(679=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_679(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(680=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_680(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(681=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_681(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(682=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_682(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(683=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_683(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(684=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_684(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(685=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_685(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(686=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(687=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_687(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(688=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(689=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_689(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(690=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_690(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(691=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_691(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(692=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_692(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(693=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_693(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(694=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_694(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(695=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_695(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(696=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_696(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(697=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_697(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(698=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_698(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(699=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_699(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(700=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_700(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(701=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_701(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(702=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_702(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(703=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_692(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(704=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_704(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(705=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_705(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(706=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_706(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(707=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_707(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(708=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(709=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_709(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(710=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_710(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(711=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_711(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(712=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_712(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(713=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_713(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(714=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_714(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(715=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_715(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(716=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_716(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(717=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_717(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(718=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_673(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(719=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_719(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(720=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_720(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(721=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_721(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(722=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_722(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(723=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(732=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_732(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(733=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_733(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(734=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_734(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(735=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_735(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(736=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_523(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(737=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_737(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(738=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_738(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(739=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_739(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(740=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_740(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(741=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(742=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_742(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(743=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_743(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(744=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_744(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(745=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_745(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(746=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_746(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(747=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(748=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_748(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(749=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_749(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(750=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_750(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(751=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(752=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_752(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(753=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_753(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(754=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_754(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(755=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(756=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_756(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(757=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_757(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(758=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_758(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(759=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(760=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_760(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(761=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_761(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(762=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_762(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(763=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(764=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_764(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(765=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_765(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(766=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_766(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(767=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_767(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(768=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_768(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(769=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_769(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(770=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_770(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(771=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_771(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(772=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_772(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(773=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(774=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_774(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(775=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_775(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(776=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_776(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(777=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_777(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(778=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_778(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(779=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_779(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(780=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_780(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(781=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_781(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(782=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_782(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(783=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_783(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(784=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_784(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(785=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_785(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(786=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_786(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(787=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_787(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(788=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_788(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(789=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_789(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(790=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_790(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(791=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_791(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(792=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_792(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(793=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(794=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_794(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(795=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_795(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(796=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_796(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(797=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_797(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(798=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_798(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(799=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_799(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(800=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_800(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(801=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_801(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(802=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_802(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(803=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_803(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(804=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_804(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(805=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_805(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(806=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_806(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(807=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_807(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(808=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_808(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(809=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_809(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(810=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_810(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(811=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_811(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(812=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_800(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(813=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_813(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(814=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_814(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(815=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_815(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(816=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_816(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(817=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_817(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(818=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(819=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_819(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(820=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_820(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(821=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_821(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(822=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_822(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(823=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_823(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(824=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_824(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(825=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_825(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(826=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_826(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(827=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_827(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(828=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_828(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(829=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_829(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(830=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_830(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(831=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_831(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(832=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_832(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(833=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(834=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_834(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(835=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_835(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(836=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_836(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(837=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_837(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(838=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_838(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(839=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_839(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(840=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_840(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(841=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_841(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_850(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(851=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_851(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(852=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_852(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(853=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_853(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(854=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_854(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(855=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_855(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(856=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_856(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(857=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_857(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(858=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_858(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(859=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_859(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(860=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_860(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(861=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_861(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(862=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(863=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_863(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(864=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_864(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(865=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(866=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_866(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(867=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_867(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(868=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_868(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(869=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_869(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(870=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_841(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(871=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_871(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(872=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_872(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(873=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_873(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(874=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_874(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(875=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_875(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(876=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_876(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(877=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_877(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(878=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_878(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(879=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(880=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_880(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(881=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_881(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(882=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_841(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(883=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_883(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(884=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_884(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(885=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_885(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(886=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_775(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(887=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_887(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(888=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_888(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(889=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_889(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(890=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_890(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(891=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_891(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(892=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_892(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(893=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_893(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(894=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_894(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(895=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(896=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_896(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(897=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_897(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(898=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_898(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(899=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_899(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(900=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_900(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(901=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_901(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(902=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_902(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(903=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_903(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(904=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_904(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(905=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_905(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(906=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_906(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.2",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_0_(Stack),
 yeccpars2(1, Cat, [0 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_1(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 5, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_1_(Stack),
 yeccpars2(4, Cat, [1 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_2(_S, '$end', _Ss, Stack,  _T, _Ts, _Tzr) ->
 {ok, hd(Stack)}.

yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_3_(Stack),
 yeccpars2(yeccgoto_optSep(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_4(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr).

yeccpars2_5(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_6: see yeccpars2_4

yeccpars2_7(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr).

yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_8_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_9_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_11_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_12_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_13_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_14_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_15_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_16_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_17_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_18_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_19_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_20_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_21_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_22_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_23_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_24_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_25_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_26_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_27_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_28_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_29_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_30_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_32_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_33_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_34_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_35_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_36_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_37_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_38_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_39_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_40_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_41_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_42_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_43_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_44_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_45_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_46_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_47_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_48_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_49_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_50_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_51_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_52_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_53_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_54_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_55_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_56_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_57_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_58_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_59_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_60_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_61_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_62_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_63_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_64_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_65_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_66_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_67_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_68_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_69_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_70_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_71_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_72_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_73_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_74_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_75_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_76_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_77_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_78_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_79_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_80_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_81_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_82_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_83_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_84_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_85_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_86_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_87_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_88: see yeccpars2_4

yeccpars2_89(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_90: see yeccpars2_4

yeccpars2_91(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_91_(Stack),
 yeccpars2(92, Cat, [91 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_92_(Stack),
 Nss = lists:nthtail(7, Ss),
 yeccpars2(yeccgoto_authenticationHeader(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_93(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_93_(Stack),
 yeccpars2(96, Cat, [93 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_94(S, endOfMessage, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr).

yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_95_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_megacoMessage(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_96(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'MtpAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 904, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr).

yeccpars2_97(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 127, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 129, Ss, Stack, T, Ts, Tzr).

yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_mId(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_mId(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_100: see yeccpars2_4

yeccpars2_101(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_(Stack),
 yeccpars2(103, Cat, [101 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_102(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_(Stack),
 yeccpars2(111, Cat, [102 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_103(S, 'RSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr).

yeccpars2_104(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_104_(Stack),
 yeccpars2(105, Cat, [104 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_105_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_daddr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_106(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_domainAddress(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_107: see yeccpars2_4

yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_108_(Stack),
 yeccpars2(yeccgoto_portNumber(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_109(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_109_(Stack),
 yeccpars2(110, Cat, [109 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_110_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_domainAddress(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_111_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_daddr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_112(S, 'GREATER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr).

yeccpars2_113(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_113_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_domainName(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_114: see yeccpars2_4

yeccpars2_115(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_115_(Stack),
 yeccpars2(116, Cat, [115 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_116_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_domainName(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_117_(Stack),
 yeccpars2(yeccgoto_transactionItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_118_(Stack),
 yeccpars2(yeccgoto_transactionItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_119_(Stack),
 yeccpars2(yeccgoto_transactionItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_120_(Stack),
 yeccpars2(yeccgoto_transactionItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_121_(Stack),
 yeccpars2(yeccgoto_messageBody(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_122(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 127, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 129, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_122_(Stack),
 yeccpars2(yeccgoto_transactionList(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_123_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_message(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_124_(Stack),
 yeccpars2(yeccgoto_messageBody(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_125(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 833, Ss, Stack, T, Ts, Tzr).

yeccpars2_126(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 895, Ss, Stack, T, Ts, Tzr).

yeccpars2_127(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 763, Ss, Stack, T, Ts, Tzr).

yeccpars2_128(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 755, Ss, Stack, T, Ts, Tzr).

yeccpars2_129(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr).

yeccpars2_130(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 747, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr).

yeccpars2_131(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr).

yeccpars2_132(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 741, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_132_(Stack),
 yeccpars2(740, Cat, [132 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_133(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_134: see yeccpars2_4

yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_135_(Stack),
 yeccpars2(yeccgoto_contextID(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_136(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr).

yeccpars2_137(S, 'AddToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 153, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 154, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 155, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(S, 'ContextAttrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 157, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(S, 'EmergencyOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(S, 'EmergencyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 159, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(S, 'IEPSToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 160, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(S, 'ModifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(S, 'MoveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(S, 'PriorityToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 165, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(S, 'SubtractToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 166, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(S, 'TopologyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr).

yeccpars2_138(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_138_(Stack),
 yeccpars2(yeccgoto_contextProperty(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_commandRequest(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_140(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_commandRequest(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_141_(Stack),
 yeccpars2(yeccgoto_contextProperty(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_commandRequest(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_143_(Stack),
 yeccpars2(yeccgoto_contextProperty(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_144_(Stack),
 yeccpars2(yeccgoto_actionRequestItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_145_(Stack),
 yeccpars2(yeccgoto_actionRequestItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_contextProperty(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_147(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_147_(Stack),
 yeccpars2(yeccgoto_actionRequestItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_148(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_commandRequest(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_149(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 520, Ss, Stack, T, Ts, Tzr).

yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_commandRequest(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_151(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 517, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_151_(Stack),
 yeccpars2(516, Cat, [151 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_152(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 515, Ss, Stack, T, Ts, Tzr).

yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_153_(Stack),
 yeccpars2(yeccgoto_ammToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_154(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 512, Ss, Stack, T, Ts, Tzr).

yeccpars2_155(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 509, Ss, Stack, T, Ts, Tzr).

yeccpars2_156(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 488, Ss, Stack, T, Ts, Tzr).

yeccpars2_157(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 472, Ss, Stack, T, Ts, Tzr).

yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_158_(Stack),
 yeccpars2(yeccgoto_contextProperty(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_159(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_159_(Stack),
 yeccpars2(yeccgoto_contextProperty(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_160(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 468, Ss, Stack, T, Ts, Tzr).

yeccpars2_161(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_161_(Stack),
 yeccpars2(yeccgoto_ammToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_162_(Stack),
 yeccpars2(yeccgoto_ammToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_163(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 432, Ss, Stack, T, Ts, Tzr).

yeccpars2_164(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 430, Ss, Stack, T, Ts, Tzr).

yeccpars2_165(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 380, Ss, Stack, T, Ts, Tzr).

yeccpars2_166(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr).

yeccpars2_167(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 168, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_168: see yeccpars2_4

yeccpars2_169(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_169_(Stack),
 yeccpars2(181, Cat, [169 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_terminationA(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_171(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 173, Ss, Stack, T, Ts, Tzr).

yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_172_(Stack),
 yeccpars2(yeccgoto_terminationID(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_173: see yeccpars2_4

yeccpars2_174(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_terminationB(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_175(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr).

yeccpars2_176(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr).

yeccpars2_177(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_177_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_topologyTriple(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_178_(Stack),
 yeccpars2(yeccgoto_topologyDirection(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_179(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_179_(Stack),
 yeccpars2(yeccgoto_topologyDirection(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_180_(Stack),
 yeccpars2(yeccgoto_topologyDirection(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_181(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_182: see yeccpars2_4

yeccpars2_183(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_183_(Stack),
 yeccpars2(184, Cat, [183 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_184_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_topologyTripleList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_185(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_185_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_topologyDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_186: see yeccpars2_4

yeccpars2_187(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_187_(Stack),
 yeccpars2(188, Cat, [187 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_188(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_188_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_subtractRequest(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_189(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr).

yeccpars2_190(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 379, Ss, Stack, T, Ts, Tzr).

yeccpars2_191(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 192, Ss, Stack, T, Ts, Tzr).

yeccpars2_192(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 205, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(S, 'DigitMapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 206, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 207, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 208, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 209, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(S, 'ModemToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 210, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(S, 'MuxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 211, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(S, 'ObservedEventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 212, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(S, 'PackagesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 213, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 214, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 215, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_192_(Stack),
 yeccpars2(204, Cat, [192 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_193(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_193_(Stack),
 yeccpars2(yeccgoto_auditItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_194(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_194_(Stack),
 yeccpars2(yeccgoto_indAudauditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_195(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_195_(Stack),
 yeccpars2(yeccgoto_indAudauditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_196(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_196_(Stack),
 yeccpars2(yeccgoto_indAudauditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_197(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_197_(Stack),
 yeccpars2(yeccgoto_indAudauditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_198_(Stack),
 yeccpars2(yeccgoto_indAudauditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_199_(Stack),
 yeccpars2(yeccgoto_indAudauditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_200(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_200_(Stack),
 yeccpars2(yeccgoto_indAudauditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_201(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 371, Ss, Stack, T, Ts, Tzr);
yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_201_(Stack),
 yeccpars2(370, Cat, [201 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_auditItem(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_203(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 367, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_203_(Stack),
 yeccpars2(366, Cat, [203 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_204(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 365, Ss, Stack, T, Ts, Tzr).

yeccpars2_205(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_205_(Stack),
 yeccpars2(yeccgoto_indAuddigitMapDescriptor(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_(Stack),
 yeccpars2(yeccgoto_auditReturnItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_207(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 351, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_207_(Stack),
 yeccpars2(yeccgoto_auditItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_208(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 345, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_208_(Stack),
 yeccpars2(yeccgoto_auditItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_209(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 312, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_209_(Stack),
 yeccpars2(yeccgoto_auditReturnItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_210(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_210_(Stack),
 yeccpars2(yeccgoto_auditReturnItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_211(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_211_(Stack),
 yeccpars2(yeccgoto_auditReturnItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_212(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_212_(Stack),
 yeccpars2(yeccgoto_auditReturnItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_213(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 308, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_213_(Stack),
 yeccpars2(yeccgoto_auditReturnItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_214(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 221, Ss, Stack, T, Ts, Tzr);
yeccpars2_214(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_214_(Stack),
 yeccpars2(yeccgoto_auditItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_215(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 216, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_215_(Stack),
 yeccpars2(yeccgoto_auditReturnItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_216: see yeccpars2_4

yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_217_(Stack),
 yeccpars2(yeccgoto_pkgdName(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_218(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 219, Ss, Stack, T, Ts, Tzr).

yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_219_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_indAudstatisticsDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_220(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_220_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_indAudsignalsDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_221(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 227, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 228, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr).

yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_222_(Stack),
 yeccpars2(yeccgoto_indAudsignalParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_223(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 237, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_223_(Stack),
 yeccpars2(yeccgoto_signalRequest(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_224(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_signalName(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_225(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 236, Ss, Stack, T, Ts, Tzr).

yeccpars2_226(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_226_(Stack),
 yeccpars2(yeccgoto_indAudsignalParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_227_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_optIndAudsignalParm(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_228(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 229, Ss, Stack, T, Ts, Tzr);
yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_228_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_229: see yeccpars2_4

yeccpars2_230(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 232, Ss, Stack, T, Ts, Tzr).

yeccpars2_231(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_231_(Stack),
 yeccpars2(yeccgoto_signalListId(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_232: see yeccpars2_4

yeccpars2_233(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_signalListParm(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_234(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 235, Ss, Stack, T, Ts, Tzr).

yeccpars2_235(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_235_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_indAudsignalList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_236(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_236_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_optIndAudsignalParm(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_237(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 240, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 241, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 242, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 243, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 244, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 245, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 246, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_237(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr).

yeccpars2_238(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 304, Ss, Stack, T, Ts, Tzr);
yeccpars2_238(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_238_(Stack),
 yeccpars2(303, Cat, [238 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_239(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 278, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'GREATER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 279, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 280, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'NEQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 281, Ss, Stack, T, Ts, Tzr).

yeccpars2_240(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 272, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_240_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_241(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 270, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_241_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_242(_S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_242_COMMA(Stack),
 yeccpars2(yeccgoto_sigParameter(hd(Ss)), 'COMMA', Ss, NewStack, T, Ts, Tzr);
yeccpars2_242(_S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_242_RBRKT(Stack),
 yeccpars2(yeccgoto_sigParameter(hd(Ss)), 'RBRKT', Ss, NewStack, T, Ts, Tzr);
yeccpars2_242(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_242_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_243(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 258, Ss, Stack, T, Ts, Tzr);
yeccpars2_243(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_243_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_244(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 255, Ss, Stack, T, Ts, Tzr);
yeccpars2_244(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_244_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_245(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 250, Ss, Stack, T, Ts, Tzr);
yeccpars2_245(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_245_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_246(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 247, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_246_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_247: see yeccpars2_4

yeccpars2_248(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_248_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_sigParameter(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_249(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_249_(Stack),
 yeccpars2(yeccgoto_streamID(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_250(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 252, Ss, Stack, T, Ts, Tzr);
yeccpars2_250(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 253, Ss, Stack, T, Ts, Tzr);
yeccpars2_250(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 254, Ss, Stack, T, Ts, Tzr).

yeccpars2_251(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_251_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_sigParameter(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_252(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_252_(Stack),
 yeccpars2(yeccgoto_signalType(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_253(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_253_(Stack),
 yeccpars2(yeccgoto_signalType(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_254(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_254_(Stack),
 yeccpars2(yeccgoto_signalType(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_255: see yeccpars2_4

yeccpars2_256(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_256_(Stack),
 yeccpars2(yeccgoto_requestID(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_257(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_257_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_sigParameter(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_258(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 259, Ss, Stack, T, Ts, Tzr).

yeccpars2_259(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 261, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 262, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 263, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 264, Ss, Stack, T, Ts, Tzr).

yeccpars2_260(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 266, Ss, Stack, T, Ts, Tzr);
yeccpars2_260(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_260_(Stack),
 yeccpars2(265, Cat, [260 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_261(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_261_(Stack),
 yeccpars2(yeccgoto_notificationReason(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_262(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_262_(Stack),
 yeccpars2(yeccgoto_notificationReason(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_263(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_263_(Stack),
 yeccpars2(yeccgoto_notificationReason(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_264(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_264_(Stack),
 yeccpars2(yeccgoto_notificationReason(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_265(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 269, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_266: see yeccpars2_259

yeccpars2_267(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 266, Ss, Stack, T, Ts, Tzr);
yeccpars2_267(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_267_(Stack),
 yeccpars2(268, Cat, [267 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_268(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_268_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_notificationReasons(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_269(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_269_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_sigParameter(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_270: see yeccpars2_4

yeccpars2_271(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_271_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_sigParameter(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_272(S, 'BothToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 274, Ss, Stack, T, Ts, Tzr);
yeccpars2_272(S, 'ExternalToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 275, Ss, Stack, T, Ts, Tzr);
yeccpars2_272(S, 'InternalToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 276, Ss, Stack, T, Ts, Tzr).

yeccpars2_273(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_273_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_sigParameter(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_274(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_274_(Stack),
 yeccpars2(yeccgoto_direction(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_275(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_275_(Stack),
 yeccpars2(yeccgoto_direction(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_276(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_276_(Stack),
 yeccpars2(yeccgoto_direction(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_277(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_277_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_sigParameter(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_278(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 289, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 290, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'QuotedChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 284, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_278(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr).

yeccpars2_279(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'QuotedChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 284, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_280: see yeccpars2_279

%% yeccpars2_281: see yeccpars2_279

yeccpars2_282(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_282_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_parmValue(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_283(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_283_(Stack),
 yeccpars2(yeccgoto_value(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_284(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_284_(Stack),
 yeccpars2(yeccgoto_value(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_285(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_285_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_parmValue(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_286(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_286_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_parmValue(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_287(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_287_(Stack),
 yeccpars2(yeccgoto_alternativeValue(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_288(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_288_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_parmValue(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_289: see yeccpars2_279

%% yeccpars2_290: see yeccpars2_279

yeccpars2_291(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 293, Ss, Stack, T, Ts, Tzr);
yeccpars2_291(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 294, Ss, Stack, T, Ts, Tzr);
yeccpars2_291(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_291_(Stack),
 yeccpars2(292, Cat, [291 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_292(S, 'RSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 299, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_293: see yeccpars2_279

%% yeccpars2_294: see yeccpars2_279

yeccpars2_295(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 294, Ss, Stack, T, Ts, Tzr);
yeccpars2_295(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_295_(Stack),
 yeccpars2(296, Cat, [295 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_296(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_296_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_valueList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_297(S, 'RSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 298, Ss, Stack, T, Ts, Tzr).

yeccpars2_298(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_298_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_alternativeValue(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_299(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_299_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_alternativeValue(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_300(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 294, Ss, Stack, T, Ts, Tzr);
yeccpars2_300(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_300_(Stack),
 yeccpars2(301, Cat, [300 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_301(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 302, Ss, Stack, T, Ts, Tzr).

yeccpars2_302(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_302_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_alternativeValue(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_303(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 307, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_304: see yeccpars2_237

yeccpars2_305(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 304, Ss, Stack, T, Ts, Tzr);
yeccpars2_305(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_305_(Stack),
 yeccpars2(306, Cat, [305 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_306(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_306_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_sigParameters(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_307(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_307_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_signalRequest(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_308: see yeccpars2_4

yeccpars2_309(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_309_(Stack),
 yeccpars2(yeccgoto_packagesItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_310(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 311, Ss, Stack, T, Ts, Tzr).

yeccpars2_311(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_indAudpackagesDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_312(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 319, Ss, Stack, T, Ts, Tzr);
yeccpars2_312(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 320, Ss, Stack, T, Ts, Tzr);
yeccpars2_312(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 321, Ss, Stack, T, Ts, Tzr);
yeccpars2_312(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 322, Ss, Stack, T, Ts, Tzr).

yeccpars2_313(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_313_(Stack),
 yeccpars2(yeccgoto_indAudmediaParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_314(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_314_(Stack),
 yeccpars2(yeccgoto_indAudmediaParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_315(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_315_(Stack),
 yeccpars2(yeccgoto_indAudmediaParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_316(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_(Stack),
 yeccpars2(yeccgoto_indAudstreamParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_317(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 341, Ss, Stack, T, Ts, Tzr);
yeccpars2_317(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_317_(Stack),
 yeccpars2(340, Cat, [317 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_318(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_318_(Stack),
 yeccpars2(yeccgoto_indAudstreamParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_319(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 332, Ss, Stack, T, Ts, Tzr).

yeccpars2_320(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 216, Ss, Stack, T, Ts, Tzr).

yeccpars2_321(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 327, Ss, Stack, T, Ts, Tzr).

yeccpars2_322(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 323, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_323: see yeccpars2_4

yeccpars2_324(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_324_(Stack),
 yeccpars2(yeccgoto_indAudterminationStateParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_325(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 326, Ss, Stack, T, Ts, Tzr).

yeccpars2_326(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_326_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_indAudterminationStateDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_327: see yeccpars2_4

yeccpars2_328(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 329, Ss, Stack, T, Ts, Tzr).

yeccpars2_329(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 319, Ss, Stack, T, Ts, Tzr);
yeccpars2_329(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 320, Ss, Stack, T, Ts, Tzr).

yeccpars2_330(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 331, Ss, Stack, T, Ts, Tzr).

yeccpars2_331(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_331_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_indAudstreamDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_332: see yeccpars2_4

yeccpars2_333(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_333_(Stack),
 yeccpars2(yeccgoto_indAudlocalParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_334(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 336, Ss, Stack, T, Ts, Tzr);
yeccpars2_334(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_334_(Stack),
 yeccpars2(335, Cat, [334 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_335(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 339, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_336: see yeccpars2_4

yeccpars2_337(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 336, Ss, Stack, T, Ts, Tzr);
yeccpars2_337(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_337_(Stack),
 yeccpars2(338, Cat, [337 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_338(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_338_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_indAudlocalParmList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_339(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_339_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_indAudlocalControlDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_340(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 344, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_341: see yeccpars2_312

yeccpars2_342(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 341, Ss, Stack, T, Ts, Tzr);
yeccpars2_342(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_342_(Stack),
 yeccpars2(343, Cat, [342 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_343(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_343_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_indAudmediaParms(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_344(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_344_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_indAudmediaDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_345: see yeccpars2_4

yeccpars2_346(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 347, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_347: see yeccpars2_4

yeccpars2_348(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_indAudrequestedEvent(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_349(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 350, Ss, Stack, T, Ts, Tzr).

yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_350_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_indAudeventsDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_351: see yeccpars2_4

yeccpars2_352(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 356, Ss, Stack, T, Ts, Tzr);
yeccpars2_352(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_352_(Stack),
 yeccpars2(355, Cat, [352 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_353(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 354, Ss, Stack, T, Ts, Tzr).

yeccpars2_354(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_354_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_indAudeventBufferDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_355(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_355_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_indAudeventSpec(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_356(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 361, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr).

yeccpars2_357(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_357_(Stack),
 yeccpars2(yeccgoto_eventParameterName(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_358(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 364, Ss, Stack, T, Ts, Tzr).

yeccpars2_359(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_359_(Stack),
 yeccpars2(yeccgoto_indAudeventSpecParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_360(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_360_(Stack),
 yeccpars2(yeccgoto_indAudeventSpecParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_361(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 362, Ss, Stack, T, Ts, Tzr);
yeccpars2_361(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_361_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_362: see yeccpars2_4

yeccpars2_363(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_363_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_eventStream(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_364(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_364_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_optIndAudeventSpecParameter(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_365(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_365_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_auditDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_366(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_auditDescriptorBody(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_367(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 205, Ss, Stack, T, Ts, Tzr);
yeccpars2_367(S, 'DigitMapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 206, Ss, Stack, T, Ts, Tzr);
yeccpars2_367(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 207, Ss, Stack, T, Ts, Tzr);
yeccpars2_367(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 208, Ss, Stack, T, Ts, Tzr);
yeccpars2_367(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 209, Ss, Stack, T, Ts, Tzr);
yeccpars2_367(S, 'ModemToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 210, Ss, Stack, T, Ts, Tzr);
yeccpars2_367(S, 'MuxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 211, Ss, Stack, T, Ts, Tzr);
yeccpars2_367(S, 'ObservedEventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 212, Ss, Stack, T, Ts, Tzr);
yeccpars2_367(S, 'PackagesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 213, Ss, Stack, T, Ts, Tzr);
yeccpars2_367(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 214, Ss, Stack, T, Ts, Tzr);
yeccpars2_367(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 215, Ss, Stack, T, Ts, Tzr).

yeccpars2_368(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 367, Ss, Stack, T, Ts, Tzr);
yeccpars2_368(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_368_(Stack),
 yeccpars2(369, Cat, [368 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_369(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_369_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_auditItemList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_370(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_indAudterminationAudit(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_371(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 205, Ss, Stack, T, Ts, Tzr);
yeccpars2_371(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 373, Ss, Stack, T, Ts, Tzr);
yeccpars2_371(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 374, Ss, Stack, T, Ts, Tzr);
yeccpars2_371(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 375, Ss, Stack, T, Ts, Tzr);
yeccpars2_371(S, 'PackagesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 376, Ss, Stack, T, Ts, Tzr);
yeccpars2_371(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 377, Ss, Stack, T, Ts, Tzr);
yeccpars2_371(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 320, Ss, Stack, T, Ts, Tzr).

yeccpars2_372(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 371, Ss, Stack, T, Ts, Tzr);
yeccpars2_372(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_372_(Stack),
 yeccpars2(378, Cat, [372 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_373(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 351, Ss, Stack, T, Ts, Tzr).

yeccpars2_374(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 345, Ss, Stack, T, Ts, Tzr).

yeccpars2_375(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 312, Ss, Stack, T, Ts, Tzr).

yeccpars2_376(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 308, Ss, Stack, T, Ts, Tzr).

yeccpars2_377(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 221, Ss, Stack, T, Ts, Tzr).

yeccpars2_378(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_378_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_indAudterminationAuditList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_379(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_379_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_optAuditDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_380: see yeccpars2_4

yeccpars2_381(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 382, Ss, Stack, T, Ts, Tzr).

yeccpars2_382(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 384, Ss, Stack, T, Ts, Tzr).

yeccpars2_383(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 429, Ss, Stack, T, Ts, Tzr).

yeccpars2_384(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 385, Ss, Stack, T, Ts, Tzr).

yeccpars2_385(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 399, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 205, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'DigitMapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 206, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 207, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 208, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 209, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 400, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 401, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'ModemToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 210, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'MuxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 211, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'ObservedEventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 212, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'PackagesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 213, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 402, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 403, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 404, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'ServiceChangeIncompleteToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 405, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 214, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 215, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 406, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 407, Ss, Stack, T, Ts, Tzr).

yeccpars2_386(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_386_(Stack),
 yeccpars2(yeccgoto_serviceChangeParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_387(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_387_(Stack),
 yeccpars2(yeccgoto_serviceChangeParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_388(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_388_(Stack),
 yeccpars2(yeccgoto_serviceChangeParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_389(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_389_(Stack),
 yeccpars2(yeccgoto_serviceChangeParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_390(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 425, Ss, Stack, T, Ts, Tzr);
yeccpars2_390(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_390_(Stack),
 yeccpars2(424, Cat, [390 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_391(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_391_(Stack),
 yeccpars2(yeccgoto_serviceChangeParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_392(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_392_(Stack),
 yeccpars2(yeccgoto_serviceChangeParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_393(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_393_(Stack),
 yeccpars2(yeccgoto_serviceChangeParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_394(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_394_(Stack),
 yeccpars2(yeccgoto_serviceChangeParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_395(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_395_(Stack),
 yeccpars2(yeccgoto_extensionParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_396: see yeccpars2_239

yeccpars2_397(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_397_(Stack),
 yeccpars2(yeccgoto_serviceChangeParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_398(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_398_(Stack),
 yeccpars2(yeccgoto_serviceChangeParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_399(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 421, Ss, Stack, T, Ts, Tzr);
yeccpars2_399(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_399_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_400(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 419, Ss, Stack, T, Ts, Tzr);
yeccpars2_400(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_400_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_401(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 417, Ss, Stack, T, Ts, Tzr);
yeccpars2_401(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_401_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_402(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 415, Ss, Stack, T, Ts, Tzr);
yeccpars2_402(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_402_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_403(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 413, Ss, Stack, T, Ts, Tzr);
yeccpars2_403(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_403_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_404(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 410, Ss, Stack, T, Ts, Tzr);
yeccpars2_404(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_404_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_405(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_405_(Stack),
 yeccpars2(yeccgoto_serviceChangeParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_406(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_406_(Stack),
 yeccpars2(yeccgoto_timeStamp(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_407(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 408, Ss, Stack, T, Ts, Tzr);
yeccpars2_407(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_407_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_408: see yeccpars2_4

yeccpars2_409(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_409_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_serviceChangeVersion(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_410(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_410_(Stack),
 yeccpars2(96, Cat, [410 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_411(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_411_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_serviceChangeAddress(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_412(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_412_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_serviceChangeAddress(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_413: see yeccpars2_279

yeccpars2_414(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_414_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_serviceChangeReason(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_415: see yeccpars2_4

yeccpars2_416(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_416_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_serviceChangeProfile(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_417(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_417_(Stack),
 yeccpars2(96, Cat, [417 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_418(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_418_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_serviceChangeMgcId(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_419: see yeccpars2_4

yeccpars2_420(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_420_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_serviceChangeMethod(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_421: see yeccpars2_4

yeccpars2_422(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_422_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_serviceChangeDelay(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_423(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_423_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_extension(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_424(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 428, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_425: see yeccpars2_385

yeccpars2_426(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 425, Ss, Stack, T, Ts, Tzr);
yeccpars2_426(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_426_(Stack),
 yeccpars2(427, Cat, [426 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_427(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_427_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_serviceChangeParms(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_428(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_428_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_serviceChangeDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_429(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_429_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_serviceChangeRequest(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_430: see yeccpars2_4

yeccpars2_431(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_431_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_priority(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_432: see yeccpars2_4

yeccpars2_433(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 434, Ss, Stack, T, Ts, Tzr).

yeccpars2_434(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_434(S, 'ObservedEventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 438, Ss, Stack, T, Ts, Tzr).

yeccpars2_435(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_435_(Stack),
 yeccpars2(yeccgoto_notifyRequestBody(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_436(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 467, Ss, Stack, T, Ts, Tzr).

yeccpars2_437(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_437_(Stack),
 yeccpars2(yeccgoto_notifyRequestBody(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_438(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 439, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_439: see yeccpars2_4

yeccpars2_440(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 441, Ss, Stack, T, Ts, Tzr).

yeccpars2_441(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_441(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 406, Ss, Stack, T, Ts, Tzr);
yeccpars2_441(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_441_(Stack),
 yeccpars2(443, Cat, [441 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_442(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_442_(Stack),
 yeccpars2(462, Cat, [442 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_443: see yeccpars2_4

yeccpars2_444(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 446, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_444_(Stack),
 yeccpars2(445, Cat, [444 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_445(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 449, Ss, Stack, T, Ts, Tzr).

yeccpars2_446(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_446(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 406, Ss, Stack, T, Ts, Tzr);
yeccpars2_446(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_446_(Stack),
 yeccpars2(443, Cat, [446 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_447(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 446, Ss, Stack, T, Ts, Tzr);
yeccpars2_447(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_447_(Stack),
 yeccpars2(448, Cat, [447 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_448(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_448_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_observedEvents(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_449(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_449_(Stack),
 Nss = lists:nthtail(6, Ss),
 yeccpars2(yeccgoto_observedEventsDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_450(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 452, Ss, Stack, T, Ts, Tzr);
yeccpars2_450(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_450_(Stack),
 yeccpars2(451, Cat, [450 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_451(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_451_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_observedEvent(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_452: see yeccpars2_4

yeccpars2_453(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 458, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_453_(Stack),
 yeccpars2(457, Cat, [453 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_454(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_observedEventParameter(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_455: see yeccpars2_239

yeccpars2_456(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_456_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_eventStreamOrOther(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_457(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 461, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_458: see yeccpars2_4

yeccpars2_459(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 458, Ss, Stack, T, Ts, Tzr);
yeccpars2_459(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_459_(Stack),
 yeccpars2(460, Cat, [459 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_460(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_460_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_observedEventParameters(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_461(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_461_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_observedEventBody(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_462(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 463, Ss, Stack, T, Ts, Tzr).

yeccpars2_463(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_463(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_463_(Stack),
 yeccpars2(464, Cat, [463 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_464: see yeccpars2_4

yeccpars2_465(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 452, Ss, Stack, T, Ts, Tzr);
yeccpars2_465(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_465_(Stack),
 yeccpars2(466, Cat, [465 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_466(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_466_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_observedEvent(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_467(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_467_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_notifyRequest(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_468(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 470, Ss, Stack, T, Ts, Tzr);
yeccpars2_468(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 471, Ss, Stack, T, Ts, Tzr).

yeccpars2_469(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_469_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_iepsValue(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_470(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_470_(Stack),
 yeccpars2(yeccgoto_onOrOff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_471(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_471_(Stack),
 yeccpars2(yeccgoto_onOrOff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_472(S, 'ContextAttrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 474, Ss, Stack, T, Ts, Tzr).

yeccpars2_473(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 487, Ss, Stack, T, Ts, Tzr).

yeccpars2_474(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 475, Ss, Stack, T, Ts, Tzr).

yeccpars2_475(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'EmergencyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 478, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'IEPSToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 479, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'PriorityToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 480, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'TopologyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 481, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr).

yeccpars2_476(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_476_(Stack),
 yeccpars2(yeccgoto_contextAuditProperty(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_477(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 483, Ss, Stack, T, Ts, Tzr);
yeccpars2_477(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_477_(Stack),
 yeccpars2(482, Cat, [477 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_478(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_478_(Stack),
 yeccpars2(yeccgoto_contextAuditProperty(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_479(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_479_(Stack),
 yeccpars2(yeccgoto_contextAuditProperty(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_480(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_480_(Stack),
 yeccpars2(yeccgoto_contextAuditProperty(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_481(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_481_(Stack),
 yeccpars2(yeccgoto_contextAuditProperty(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_482(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 486, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_483: see yeccpars2_475

yeccpars2_484(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 483, Ss, Stack, T, Ts, Tzr);
yeccpars2_484(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_484_(Stack),
 yeccpars2(485, Cat, [484 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_485(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_485_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_contextAuditProperties(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_486(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_486_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_indAudcontextAttrDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_487(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_487_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_contextAudit(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_488(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'ContextListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 493, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr).

yeccpars2_489(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 508, Ss, Stack, T, Ts, Tzr).

yeccpars2_490(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 505, Ss, Stack, T, Ts, Tzr);
yeccpars2_490(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_490_(Stack),
 yeccpars2(504, Cat, [490 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_491: see yeccpars2_239

yeccpars2_492(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 502, Ss, Stack, T, Ts, Tzr).

yeccpars2_493(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 494, Ss, Stack, T, Ts, Tzr).

yeccpars2_494(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 495, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_495: see yeccpars2_4

yeccpars2_496(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 498, Ss, Stack, T, Ts, Tzr);
yeccpars2_496(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_496_(Stack),
 yeccpars2(497, Cat, [496 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_497(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 501, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_498: see yeccpars2_4

yeccpars2_499(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 498, Ss, Stack, T, Ts, Tzr);
yeccpars2_499(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_499_(Stack),
 yeccpars2(500, Cat, [499 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_500(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_500_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_contextIDs(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_501(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_501_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_contextIdList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_502(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_502_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_contextAttrDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_503(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_503_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_propertyParm(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_504(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_504_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_propertyParms(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_505: see yeccpars2_4

yeccpars2_506(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 505, Ss, Stack, T, Ts, Tzr);
yeccpars2_506(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_506_(Stack),
 yeccpars2(507, Cat, [506 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_507(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_507_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_propertyParmList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_508(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_508_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_contextAttrDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_509: see yeccpars2_4

yeccpars2_510(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_510(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_510_(Stack),
 yeccpars2(511, Cat, [510 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_511(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_511_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_auditRequest(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_512: see yeccpars2_4

yeccpars2_513(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_513(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_513_(Stack),
 yeccpars2(514, Cat, [513 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_514(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_514_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_auditRequest(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_515(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_515_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_actionRequest(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_516(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_516_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_actionRequestBody(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_517: see yeccpars2_137

yeccpars2_518(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 517, Ss, Stack, T, Ts, Tzr);
yeccpars2_518(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_518_(Stack),
 yeccpars2(519, Cat, [518 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_519(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_519_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_actionRequestItems(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_520: see yeccpars2_4

yeccpars2_521(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 523, Ss, Stack, T, Ts, Tzr);
yeccpars2_521(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_521_(Stack),
 yeccpars2(522, Cat, [521 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_522(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_522_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_ammRequest(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_523(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_523(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 534, Ss, Stack, T, Ts, Tzr);
yeccpars2_523(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 535, Ss, Stack, T, Ts, Tzr);
yeccpars2_523(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 536, Ss, Stack, T, Ts, Tzr);
yeccpars2_523(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 537, Ss, Stack, T, Ts, Tzr);
yeccpars2_523(S, 'ModemToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 538, Ss, Stack, T, Ts, Tzr);
yeccpars2_523(S, 'MuxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 539, Ss, Stack, T, Ts, Tzr);
yeccpars2_523(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 540, Ss, Stack, T, Ts, Tzr);
yeccpars2_523(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 541, Ss, Stack, T, Ts, Tzr).

yeccpars2_524(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_524_(Stack),
 yeccpars2(yeccgoto_ammParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_525(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_525_(Stack),
 yeccpars2(yeccgoto_ammParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_526(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_526_(Stack),
 yeccpars2(yeccgoto_ammParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_527(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_527_(Stack),
 yeccpars2(yeccgoto_ammParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_528(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_528_(Stack),
 yeccpars2(yeccgoto_ammParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_529(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_529_(Stack),
 yeccpars2(yeccgoto_ammParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_530(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_530_(Stack),
 yeccpars2(yeccgoto_ammParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_531(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_531_(Stack),
 yeccpars2(yeccgoto_ammParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_532(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_532_(Stack),
 yeccpars2(yeccgoto_ammParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_533(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 736, Ss, Stack, T, Ts, Tzr);
yeccpars2_533(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_533_(Stack),
 yeccpars2(735, Cat, [533 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_534(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_534_(Stack),
 yeccpars2(yeccgoto_digitMapDescriptor(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_535(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 727, Ss, Stack, T, Ts, Tzr);
yeccpars2_535(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_535_(Stack),
 yeccpars2(yeccgoto_eventBufferDescriptor(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_536(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 667, Ss, Stack, T, Ts, Tzr);
yeccpars2_536(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_536_(Stack),
 yeccpars2(yeccgoto_eventsDescriptor(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_537(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 598, Ss, Stack, T, Ts, Tzr).

yeccpars2_538(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 582, Ss, Stack, T, Ts, Tzr);
yeccpars2_538(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 583, Ss, Stack, T, Ts, Tzr).

yeccpars2_539(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 571, Ss, Stack, T, Ts, Tzr).

yeccpars2_540(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 552, Ss, Stack, T, Ts, Tzr);
yeccpars2_540(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_540_(Stack),
 yeccpars2(yeccgoto_signalsDescriptor(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_541(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 542, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_542: see yeccpars2_4

yeccpars2_543(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 548, Ss, Stack, T, Ts, Tzr);
yeccpars2_543(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_543_(Stack),
 yeccpars2(547, Cat, [543 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_544(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 545, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_544_(Stack),
 yeccpars2(yeccgoto_statisticsParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_545: see yeccpars2_279

yeccpars2_546(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_546_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_statisticsParameter(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_547(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 551, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_548: see yeccpars2_4

yeccpars2_549(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 548, Ss, Stack, T, Ts, Tzr);
yeccpars2_549(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_549_(Stack),
 yeccpars2(550, Cat, [549 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_550(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_550_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_statisticsParameters(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_551(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_551_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_statisticsDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_552(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 556, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_552(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr).

yeccpars2_553(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_553_(Stack),
 yeccpars2(yeccgoto_signalParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_554(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 567, Ss, Stack, T, Ts, Tzr);
yeccpars2_554(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_554_(Stack),
 yeccpars2(566, Cat, [554 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_555(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_555_(Stack),
 yeccpars2(yeccgoto_signalParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_556(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 557, Ss, Stack, T, Ts, Tzr);
yeccpars2_556(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_556_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_557: see yeccpars2_4

yeccpars2_558(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 559, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_559: see yeccpars2_4

yeccpars2_560(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 562, Ss, Stack, T, Ts, Tzr);
yeccpars2_560(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_560_(Stack),
 yeccpars2(561, Cat, [560 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_561(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 565, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_562: see yeccpars2_4

yeccpars2_563(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 562, Ss, Stack, T, Ts, Tzr);
yeccpars2_563(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_563_(Stack),
 yeccpars2(564, Cat, [563 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_564(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_564_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_signalListParms(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_565(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_565_(Stack),
 Nss = lists:nthtail(6, Ss),
 yeccpars2(yeccgoto_signalList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_566(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 570, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_567: see yeccpars2_552

yeccpars2_568(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 567, Ss, Stack, T, Ts, Tzr);
yeccpars2_568(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_568_(Stack),
 yeccpars2(569, Cat, [568 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_569(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_569_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_signalParms(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_570(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_570_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_signalsDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_571: see yeccpars2_4

yeccpars2_572(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_572_(Stack),
 yeccpars2(yeccgoto_muxType(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_573(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 575, Ss, Stack, T, Ts, Tzr).

yeccpars2_574(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_574_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_muxDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_575: see yeccpars2_4

yeccpars2_576(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 578, Ss, Stack, T, Ts, Tzr);
yeccpars2_576(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_576_(Stack),
 yeccpars2(577, Cat, [576 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_577(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 581, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_578: see yeccpars2_4

yeccpars2_579(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 578, Ss, Stack, T, Ts, Tzr);
yeccpars2_579(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_579_(Stack),
 yeccpars2(580, Cat, [579 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_580(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_580_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_terminationIDListRepeat(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_581(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_581_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_terminationIDList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_582: see yeccpars2_4

%% yeccpars2_583: see yeccpars2_4

yeccpars2_584(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_584_(Stack),
 yeccpars2(yeccgoto_modemType(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_585(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 587, Ss, Stack, T, Ts, Tzr);
yeccpars2_585(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_585_(Stack),
 yeccpars2(586, Cat, [585 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_586(S, 'RSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 590, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_587: see yeccpars2_4

yeccpars2_588(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 587, Ss, Stack, T, Ts, Tzr);
yeccpars2_588(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_588_(Stack),
 yeccpars2(589, Cat, [588 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_589(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_589_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_modemTypeList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_590(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 592, Ss, Stack, T, Ts, Tzr);
yeccpars2_590(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_590_(Stack),
 yeccpars2(591, Cat, [590 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_591(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_591_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_modemDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_592: see yeccpars2_4

yeccpars2_593(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 505, Ss, Stack, T, Ts, Tzr);
yeccpars2_593(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_593_(Stack),
 yeccpars2(594, Cat, [593 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_594(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 595, Ss, Stack, T, Ts, Tzr).

yeccpars2_595(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_595_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_optPropertyParms(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_596(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 592, Ss, Stack, T, Ts, Tzr);
yeccpars2_596(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_596_(Stack),
 yeccpars2(597, Cat, [596 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_597(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_597_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_modemDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_598(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 605, Ss, Stack, T, Ts, Tzr);
yeccpars2_598(S, 'LocalDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 606, Ss, Stack, T, Ts, Tzr);
yeccpars2_598(S, 'RemoteDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 607, Ss, Stack, T, Ts, Tzr);
yeccpars2_598(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 541, Ss, Stack, T, Ts, Tzr);
yeccpars2_598(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 608, Ss, Stack, T, Ts, Tzr);
yeccpars2_598(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 609, Ss, Stack, T, Ts, Tzr).

yeccpars2_599(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_599_(Stack),
 yeccpars2(yeccgoto_mediaParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_600(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_600_(Stack),
 yeccpars2(yeccgoto_mediaParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_601(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_601_(Stack),
 yeccpars2(yeccgoto_mediaParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_602(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_602_(Stack),
 yeccpars2(yeccgoto_streamParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_603(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 663, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_603_(Stack),
 yeccpars2(662, Cat, [603 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_604(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_604_(Stack),
 yeccpars2(yeccgoto_streamParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_605(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 640, Ss, Stack, T, Ts, Tzr).

yeccpars2_606(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_606_(Stack),
 yeccpars2(yeccgoto_streamParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_607(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_607_(Stack),
 yeccpars2(yeccgoto_streamParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_608(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 631, Ss, Stack, T, Ts, Tzr).

yeccpars2_609(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 610, Ss, Stack, T, Ts, Tzr).

yeccpars2_610(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 615, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 616, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr).

yeccpars2_611(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 627, Ss, Stack, T, Ts, Tzr);
yeccpars2_611(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_611_(Stack),
 yeccpars2(626, Cat, [611 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_612(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_612_(Stack),
 yeccpars2(yeccgoto_terminationStateParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_613(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_613_(Stack),
 yeccpars2(yeccgoto_terminationStateParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_614(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_614_(Stack),
 yeccpars2(yeccgoto_terminationStateParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_615(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 622, Ss, Stack, T, Ts, Tzr);
yeccpars2_615(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_615_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_616(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 617, Ss, Stack, T, Ts, Tzr);
yeccpars2_616(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_616_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_617(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 619, Ss, Stack, T, Ts, Tzr);
yeccpars2_617(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 620, Ss, Stack, T, Ts, Tzr);
yeccpars2_617(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 621, Ss, Stack, T, Ts, Tzr).

yeccpars2_618(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_618_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_serviceStates(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_619(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_619_(Stack),
 yeccpars2(yeccgoto_serviceState(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_620(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_620_(Stack),
 yeccpars2(yeccgoto_serviceState(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_621(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_621_(Stack),
 yeccpars2(yeccgoto_serviceState(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_622(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 624, Ss, Stack, T, Ts, Tzr);
yeccpars2_622(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 625, Ss, Stack, T, Ts, Tzr).

yeccpars2_623(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_623_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_eventBufferControl(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_624(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_624_(Stack),
 yeccpars2(yeccgoto_eventBufferControlState(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_625(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_625_(Stack),
 yeccpars2(yeccgoto_eventBufferControlState(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_626(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 630, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_627: see yeccpars2_610

yeccpars2_628(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 627, Ss, Stack, T, Ts, Tzr);
yeccpars2_628(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_628_(Stack),
 yeccpars2(629, Cat, [628 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_629(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_629_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_terminationStateParms(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_630(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_630_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_terminationStateDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_631: see yeccpars2_4

yeccpars2_632(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 633, Ss, Stack, T, Ts, Tzr).

yeccpars2_633(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 605, Ss, Stack, T, Ts, Tzr);
yeccpars2_633(S, 'LocalDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 606, Ss, Stack, T, Ts, Tzr);
yeccpars2_633(S, 'RemoteDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 607, Ss, Stack, T, Ts, Tzr);
yeccpars2_633(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 541, Ss, Stack, T, Ts, Tzr).

yeccpars2_634(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 636, Ss, Stack, T, Ts, Tzr);
yeccpars2_634(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_634_(Stack),
 yeccpars2(635, Cat, [634 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_635(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 639, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_636: see yeccpars2_633

yeccpars2_637(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 636, Ss, Stack, T, Ts, Tzr);
yeccpars2_637(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_637_(Stack),
 yeccpars2(638, Cat, [637 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_638(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_638_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_streamParmList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_639(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_639_(Stack),
 Nss = lists:nthtail(6, Ss),
 yeccpars2(yeccgoto_streamDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_640(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 643, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 644, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 645, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr).

yeccpars2_641(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_641_(Stack),
 yeccpars2(yeccgoto_localParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_642(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 658, Ss, Stack, T, Ts, Tzr);
yeccpars2_642(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_642_(Stack),
 yeccpars2(657, Cat, [642 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_643(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 650, Ss, Stack, T, Ts, Tzr);
yeccpars2_643(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_643_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_644(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 648, Ss, Stack, T, Ts, Tzr);
yeccpars2_644(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_644_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_645(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 646, Ss, Stack, T, Ts, Tzr);
yeccpars2_645(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_645_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_646: see yeccpars2_468

yeccpars2_647(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_647_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_localParm(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_648: see yeccpars2_468

yeccpars2_649(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_649_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_localParm(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_650(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 652, Ss, Stack, T, Ts, Tzr);
yeccpars2_650(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 653, Ss, Stack, T, Ts, Tzr);
yeccpars2_650(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 654, Ss, Stack, T, Ts, Tzr);
yeccpars2_650(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 655, Ss, Stack, T, Ts, Tzr);
yeccpars2_650(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 656, Ss, Stack, T, Ts, Tzr).

yeccpars2_651(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_651_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_localParm(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_652(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_652_(Stack),
 yeccpars2(yeccgoto_streamModes(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_653(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_653_(Stack),
 yeccpars2(yeccgoto_streamModes(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_654(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_654_(Stack),
 yeccpars2(yeccgoto_streamModes(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_655(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_655_(Stack),
 yeccpars2(yeccgoto_streamModes(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_656(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_656_(Stack),
 yeccpars2(yeccgoto_streamModes(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_657(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 661, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_658: see yeccpars2_640

yeccpars2_659(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 658, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_659_(Stack),
 yeccpars2(660, Cat, [659 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_660(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_660_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_localParmList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_661(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_661_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_localControlDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_662(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 666, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_663: see yeccpars2_598

yeccpars2_664(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 663, Ss, Stack, T, Ts, Tzr);
yeccpars2_664(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_664_(Stack),
 yeccpars2(665, Cat, [664 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_665(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_665_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_mediaParmList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_666(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_666_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_mediaDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_667: see yeccpars2_4

yeccpars2_668(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 669, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_669: see yeccpars2_4

yeccpars2_670(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 723, Ss, Stack, T, Ts, Tzr);
yeccpars2_670(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_670_(Stack),
 yeccpars2(722, Cat, [670 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_671(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 673, Ss, Stack, T, Ts, Tzr);
yeccpars2_671(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_671_(Stack),
 yeccpars2(672, Cat, [671 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_672(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_672_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_requestedEvent(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_673(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 679, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 680, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 681, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr).

yeccpars2_674(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_eventParameter(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_675(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 718, Ss, Stack, T, Ts, Tzr);
yeccpars2_675(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_675_(Stack),
 yeccpars2(717, Cat, [675 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_676(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_eventParameter(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_677(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_eventParameter(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_678(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_eventParameter(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_679(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_679_(Stack),
 yeccpars2(yeccgoto_eventDM(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_680(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 682, Ss, Stack, T, Ts, Tzr);
yeccpars2_680(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_680_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_681(_S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_681_COMMA(Stack),
 yeccpars2(yeccgoto_eventParameter(hd(Ss)), 'COMMA', Ss, NewStack, T, Ts, Tzr);
yeccpars2_681(_S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_681_RBRKT(Stack),
 yeccpars2(yeccgoto_eventParameter(hd(Ss)), 'RBRKT', Ss, NewStack, T, Ts, Tzr);
yeccpars2_681(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_681_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_682(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 685, Ss, Stack, T, Ts, Tzr);
yeccpars2_682(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 540, Ss, Stack, T, Ts, Tzr).

yeccpars2_683(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 713, Ss, Stack, T, Ts, Tzr);
yeccpars2_683(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 714, Ss, Stack, T, Ts, Tzr).

yeccpars2_684(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 712, Ss, Stack, T, Ts, Tzr).

yeccpars2_685(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 686, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_685_(Stack),
 yeccpars2(yeccgoto_embedFirst(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_686: see yeccpars2_4

yeccpars2_687(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 688, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_688: see yeccpars2_4

yeccpars2_689(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 708, Ss, Stack, T, Ts, Tzr);
yeccpars2_689(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_689_(Stack),
 yeccpars2(707, Cat, [689 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_690(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 692, Ss, Stack, T, Ts, Tzr);
yeccpars2_690(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_690_(Stack),
 yeccpars2(691, Cat, [690 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_691(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_691_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_secondRequestedEvent(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_692(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 679, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 697, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 698, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_692(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr).

yeccpars2_693(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 703, Ss, Stack, T, Ts, Tzr);
yeccpars2_693(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_693_(Stack),
 yeccpars2(702, Cat, [693 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_694(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_secondEventParameter(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_695(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_secondEventParameter(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_696(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_secondEventParameter(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_697(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 699, Ss, Stack, T, Ts, Tzr);
yeccpars2_697(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_697_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_698(_S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_698_COMMA(Stack),
 yeccpars2(yeccgoto_secondEventParameter(hd(Ss)), 'COMMA', Ss, NewStack, T, Ts, Tzr);
yeccpars2_698(_S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_698_RBRKT(Stack),
 yeccpars2(yeccgoto_secondEventParameter(hd(Ss)), 'RBRKT', Ss, NewStack, T, Ts, Tzr);
yeccpars2_698(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_698_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_699(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 540, Ss, Stack, T, Ts, Tzr).

yeccpars2_700(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 701, Ss, Stack, T, Ts, Tzr).

yeccpars2_701(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_701_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_embedSig(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_702(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 706, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_703: see yeccpars2_692

yeccpars2_704(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 703, Ss, Stack, T, Ts, Tzr);
yeccpars2_704(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_704_(Stack),
 yeccpars2(705, Cat, [704 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_705(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_705_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_secondEventParameters(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_706(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_706_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_secondRequestedEventBody(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_707(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 711, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_708: see yeccpars2_4

yeccpars2_709(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 708, Ss, Stack, T, Ts, Tzr);
yeccpars2_709(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_709_(Stack),
 yeccpars2(710, Cat, [709 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_710(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_710_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_secondRequestedEvents(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_711(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_711_(Stack),
 Nss = lists:nthtail(6, Ss),
 yeccpars2(yeccgoto_embedFirst(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_712(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_712_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_embedNoSig(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_713(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 685, Ss, Stack, T, Ts, Tzr).

yeccpars2_714(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_714_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_embedWithSig(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_715(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 716, Ss, Stack, T, Ts, Tzr).

yeccpars2_716(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_716_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_embedWithSig(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_717(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 721, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_718: see yeccpars2_673

yeccpars2_719(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 718, Ss, Stack, T, Ts, Tzr);
yeccpars2_719(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_719_(Stack),
 yeccpars2(720, Cat, [719 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_720(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_720_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_eventParameters(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_721(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_721_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_requestedEventBody(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_722(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 726, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_723: see yeccpars2_4

yeccpars2_724(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 723, Ss, Stack, T, Ts, Tzr);
yeccpars2_724(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_724_(Stack),
 yeccpars2(725, Cat, [724 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_725(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_725_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_requestedEvents(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_726(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_726_(Stack),
 Nss = lists:nthtail(6, Ss),
 yeccpars2(yeccgoto_eventsDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_727(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_727(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 406, Ss, Stack, T, Ts, Tzr);
yeccpars2_727(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_727_(Stack),
 yeccpars2(443, Cat, [727 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_728(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_728_(Stack),
 yeccpars2(yeccgoto_eventSpec(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_729(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 731, Ss, Stack, T, Ts, Tzr);
yeccpars2_729(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_729_(Stack),
 yeccpars2(730, Cat, [729 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_730(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 734, Ss, Stack, T, Ts, Tzr).

yeccpars2_731(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_731(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 406, Ss, Stack, T, Ts, Tzr);
yeccpars2_731(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_731_(Stack),
 yeccpars2(443, Cat, [731 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_732(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 731, Ss, Stack, T, Ts, Tzr);
yeccpars2_732(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_732_(Stack),
 yeccpars2(733, Cat, [732 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_733(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_733_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_eventSpecList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_734(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_734_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_eventBufferDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_735(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 739, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_736: see yeccpars2_523

yeccpars2_737(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 736, Ss, Stack, T, Ts, Tzr);
yeccpars2_737(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_737_(Stack),
 yeccpars2(738, Cat, [737 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_738(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_738_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_ammParameters(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_739(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_739_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_ammRequestBody(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_740(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 744, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_741: see yeccpars2_131

yeccpars2_742(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 741, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_742_(Stack),
 yeccpars2(743, Cat, [742 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_743(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_743_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_actionRequestList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_744(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_744_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_transactionRequest(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_745(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 751, Ss, Stack, T, Ts, Tzr).

yeccpars2_746(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_746_(Stack),
 yeccpars2(yeccgoto_transactionID(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_747: see yeccpars2_131

yeccpars2_748(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 741, Ss, Stack, T, Ts, Tzr);
yeccpars2_748(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_748_(Stack),
 yeccpars2(749, Cat, [748 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_749(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 750, Ss, Stack, T, Ts, Tzr).

yeccpars2_750(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_750_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_transactionRequest(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_751: see yeccpars2_131

yeccpars2_752(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 741, Ss, Stack, T, Ts, Tzr);
yeccpars2_752(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_752_(Stack),
 yeccpars2(753, Cat, [752 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_753(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 754, Ss, Stack, T, Ts, Tzr).

yeccpars2_754(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_754_(Stack),
 Nss = lists:nthtail(6, Ss),
 yeccpars2(yeccgoto_transactionRequest(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_755: see yeccpars2_4

yeccpars2_756(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 759, Ss, Stack, T, Ts, Tzr);
yeccpars2_756(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_756_(Stack),
 yeccpars2(758, Cat, [756 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_757(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_757_(Stack),
 yeccpars2(yeccgoto_transactionAck(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_758(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 762, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_759: see yeccpars2_4

yeccpars2_760(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 759, Ss, Stack, T, Ts, Tzr);
yeccpars2_760(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_760_(Stack),
 yeccpars2(761, Cat, [760 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_761(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_761_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_transactionAckList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_762(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_762_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_transactionResponseAck(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_763: see yeccpars2_4

yeccpars2_764(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 765, Ss, Stack, T, Ts, Tzr).

yeccpars2_765(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 767, Ss, Stack, T, Ts, Tzr);
yeccpars2_765(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_765_(Stack),
 yeccpars2(766, Cat, [765 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_766(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 772, Ss, Stack, T, Ts, Tzr);
yeccpars2_766(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr).

yeccpars2_767(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 768, Ss, Stack, T, Ts, Tzr).

yeccpars2_768(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_768_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_optImmAckRequired(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_769(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 894, Ss, Stack, T, Ts, Tzr).

yeccpars2_770(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_770_(Stack),
 yeccpars2(yeccgoto_transactionReplyBody(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_771(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 891, Ss, Stack, T, Ts, Tzr);
yeccpars2_771(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_771_(Stack),
 yeccpars2(890, Cat, [771 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_772(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 773, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_773: see yeccpars2_4

yeccpars2_774(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 775, Ss, Stack, T, Ts, Tzr).

yeccpars2_775(S, 'AddToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 785, Ss, Stack, T, Ts, Tzr);
yeccpars2_775(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 786, Ss, Stack, T, Ts, Tzr);
yeccpars2_775(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 787, Ss, Stack, T, Ts, Tzr);
yeccpars2_775(S, 'ContextAttrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_775(S, 'EmergencyOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr);
yeccpars2_775(S, 'EmergencyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 159, Ss, Stack, T, Ts, Tzr);
yeccpars2_775(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_775(S, 'IEPSToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 160, Ss, Stack, T, Ts, Tzr);
yeccpars2_775(S, 'ModifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 788, Ss, Stack, T, Ts, Tzr);
yeccpars2_775(S, 'MoveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 789, Ss, Stack, T, Ts, Tzr);
yeccpars2_775(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 790, Ss, Stack, T, Ts, Tzr);
yeccpars2_775(S, 'PriorityToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_775(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 791, Ss, Stack, T, Ts, Tzr);
yeccpars2_775(S, 'SubtractToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 792, Ss, Stack, T, Ts, Tzr);
yeccpars2_775(S, 'TopologyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr).

yeccpars2_776(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_776_(Stack),
 yeccpars2(yeccgoto_commandReplys(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_777(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_777_(Stack),
 yeccpars2(yeccgoto_commandReplys(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_778(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_778_(Stack),
 yeccpars2(yeccgoto_actionReplyBody(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_779(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_779_(Stack),
 yeccpars2(yeccgoto_commandReplys(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_780(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 886, Ss, Stack, T, Ts, Tzr);
yeccpars2_780(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_780_(Stack),
 yeccpars2(885, Cat, [780 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_781(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_781_(Stack),
 yeccpars2(yeccgoto_commandReplys(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_782(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 879, Ss, Stack, T, Ts, Tzr).

yeccpars2_783(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_783_(Stack),
 yeccpars2(yeccgoto_commandReplys(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_784(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 878, Ss, Stack, T, Ts, Tzr).

yeccpars2_785(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_785_(Stack),
 yeccpars2(yeccgoto_ammsToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_786(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 874, Ss, Stack, T, Ts, Tzr).

yeccpars2_787(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 824, Ss, Stack, T, Ts, Tzr).

yeccpars2_788(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_788_(Stack),
 yeccpars2(yeccgoto_ammsToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_789(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_789_(Stack),
 yeccpars2(yeccgoto_ammsToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_790(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 818, Ss, Stack, T, Ts, Tzr).

yeccpars2_791(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 793, Ss, Stack, T, Ts, Tzr).

yeccpars2_792(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_792_(Stack),
 yeccpars2(yeccgoto_ammsToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_793: see yeccpars2_4

yeccpars2_794(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 796, Ss, Stack, T, Ts, Tzr);
yeccpars2_794(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_794_(Stack),
 yeccpars2(795, Cat, [794 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_795(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_795_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_serviceChangeReply(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_796(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_796(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 799, Ss, Stack, T, Ts, Tzr).

yeccpars2_797(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 817, Ss, Stack, T, Ts, Tzr).

yeccpars2_798(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 816, Ss, Stack, T, Ts, Tzr).

yeccpars2_799(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 800, Ss, Stack, T, Ts, Tzr).

yeccpars2_800(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 807, Ss, Stack, T, Ts, Tzr);
yeccpars2_800(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 808, Ss, Stack, T, Ts, Tzr);
yeccpars2_800(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 809, Ss, Stack, T, Ts, Tzr);
yeccpars2_800(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 406, Ss, Stack, T, Ts, Tzr);
yeccpars2_800(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 810, Ss, Stack, T, Ts, Tzr).

yeccpars2_801(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_801_(Stack),
 yeccpars2(yeccgoto_servChgReplyParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_802(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_802_(Stack),
 yeccpars2(yeccgoto_servChgReplyParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_803(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_803_(Stack),
 yeccpars2(yeccgoto_servChgReplyParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_804(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_804_(Stack),
 yeccpars2(yeccgoto_servChgReplyParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_805(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_805_(Stack),
 yeccpars2(yeccgoto_servChgReplyParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_806(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 812, Ss, Stack, T, Ts, Tzr);
yeccpars2_806(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_806_(Stack),
 yeccpars2(811, Cat, [806 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_807(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 417, Ss, Stack, T, Ts, Tzr).

yeccpars2_808(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 415, Ss, Stack, T, Ts, Tzr).

yeccpars2_809(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 410, Ss, Stack, T, Ts, Tzr).

yeccpars2_810(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 408, Ss, Stack, T, Ts, Tzr).

yeccpars2_811(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 815, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_812: see yeccpars2_800

yeccpars2_813(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 812, Ss, Stack, T, Ts, Tzr);
yeccpars2_813(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_813_(Stack),
 yeccpars2(814, Cat, [813 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_814(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_814_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_servChgReplyParms(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_815(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_815_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_serviceChangeReplyDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_816(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_816_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_serviceChangeReplyBody(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_817(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_817_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_serviceChangeReplyBody(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_818: see yeccpars2_4

yeccpars2_819(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 821, Ss, Stack, T, Ts, Tzr);
yeccpars2_819(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_819_(Stack),
 yeccpars2(820, Cat, [819 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_820(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_820_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_notifyReply(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_821(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr).

yeccpars2_822(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 823, Ss, Stack, T, Ts, Tzr).

yeccpars2_823(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_823_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_notifyReplyBody(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_824(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 827, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_824(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr).

yeccpars2_825(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 841, Ss, Stack, T, Ts, Tzr);
yeccpars2_825(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_825_(Stack),
 yeccpars2(yeccgoto_auditOther(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_826(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_826_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_auditReply(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_827(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 830, Ss, Stack, T, Ts, Tzr);
yeccpars2_827(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_827_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_828(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_828_(Stack),
 yeccpars2(yeccgoto_contextTerminationAudit(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_829(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_829_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_auditReply(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_830(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 832, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_830(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr).

yeccpars2_831(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 840, Ss, Stack, T, Ts, Tzr).

yeccpars2_832(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 833, Ss, Stack, T, Ts, Tzr);
yeccpars2_832(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_832_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_833: see yeccpars2_4

yeccpars2_834(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_834_(Stack),
 yeccpars2(yeccgoto_errorCode(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_835(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 836, Ss, Stack, T, Ts, Tzr).

yeccpars2_836(S, 'QuotedChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 838, Ss, Stack, T, Ts, Tzr);
yeccpars2_836(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_836_(Stack),
 yeccpars2(837, Cat, [836 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_837(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 839, Ss, Stack, T, Ts, Tzr).

yeccpars2_838(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_838_(Stack),
 yeccpars2(yeccgoto_errorText(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_839(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_839_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_errorDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_840(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_840_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_contextTerminationAudit(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_841(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 534, Ss, Stack, T, Ts, Tzr);
yeccpars2_841(S, 'DigitMapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 206, Ss, Stack, T, Ts, Tzr);
yeccpars2_841(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_841(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 535, Ss, Stack, T, Ts, Tzr);
yeccpars2_841(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 536, Ss, Stack, T, Ts, Tzr);
yeccpars2_841(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 856, Ss, Stack, T, Ts, Tzr);
yeccpars2_841(S, 'ModemToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 857, Ss, Stack, T, Ts, Tzr);
yeccpars2_841(S, 'MuxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 858, Ss, Stack, T, Ts, Tzr);
yeccpars2_841(S, 'ObservedEventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 859, Ss, Stack, T, Ts, Tzr);
yeccpars2_841(S, 'PackagesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 860, Ss, Stack, T, Ts, Tzr);
yeccpars2_841(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 540, Ss, Stack, T, Ts, Tzr);
yeccpars2_841(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 861, Ss, Stack, T, Ts, Tzr).

yeccpars2_842(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 873, Ss, Stack, T, Ts, Tzr).

yeccpars2_843(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_843_(Stack),
 yeccpars2(yeccgoto_auditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_844(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_844_(Stack),
 yeccpars2(yeccgoto_auditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_845(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_845_(Stack),
 yeccpars2(yeccgoto_auditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_846(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_846_(Stack),
 yeccpars2(yeccgoto_auditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_847(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_847_(Stack),
 yeccpars2(yeccgoto_auditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_848(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_848_(Stack),
 yeccpars2(yeccgoto_auditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_849(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_849_(Stack),
 yeccpars2(yeccgoto_auditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_850(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_850_(Stack),
 yeccpars2(yeccgoto_auditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_851(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_851_(Stack),
 yeccpars2(yeccgoto_auditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_852(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_852_(Stack),
 yeccpars2(yeccgoto_auditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_853(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_853_(Stack),
 yeccpars2(yeccgoto_auditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_854(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 870, Ss, Stack, T, Ts, Tzr);
yeccpars2_854(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_854_(Stack),
 yeccpars2(869, Cat, [854 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_855(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_855_(Stack),
 yeccpars2(yeccgoto_auditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_856(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 598, Ss, Stack, T, Ts, Tzr);
yeccpars2_856(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_856_(Stack),
 yeccpars2(yeccgoto_auditReturnItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_857(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 582, Ss, Stack, T, Ts, Tzr);
yeccpars2_857(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 583, Ss, Stack, T, Ts, Tzr);
yeccpars2_857(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_857_(Stack),
 yeccpars2(yeccgoto_auditReturnItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_858(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 571, Ss, Stack, T, Ts, Tzr);
yeccpars2_858(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_858_(Stack),
 yeccpars2(yeccgoto_auditReturnItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_859(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 439, Ss, Stack, T, Ts, Tzr);
yeccpars2_859(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_859_(Stack),
 yeccpars2(yeccgoto_auditReturnItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_860(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 862, Ss, Stack, T, Ts, Tzr);
yeccpars2_860(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_860_(Stack),
 yeccpars2(yeccgoto_auditReturnItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_861(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 542, Ss, Stack, T, Ts, Tzr);
yeccpars2_861(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_861_(Stack),
 yeccpars2(yeccgoto_auditReturnItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_862: see yeccpars2_4

yeccpars2_863(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 865, Ss, Stack, T, Ts, Tzr);
yeccpars2_863(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_863_(Stack),
 yeccpars2(864, Cat, [863 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_864(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 868, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_865: see yeccpars2_4

yeccpars2_866(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 865, Ss, Stack, T, Ts, Tzr);
yeccpars2_866(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_866_(Stack),
 yeccpars2(867, Cat, [866 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_867(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_867_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_packagesItems(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_868(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_868_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_packagesDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_869(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_869_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_terminationAudit(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_870: see yeccpars2_841

yeccpars2_871(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 870, Ss, Stack, T, Ts, Tzr);
yeccpars2_871(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_871_(Stack),
 yeccpars2(872, Cat, [871 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_872(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_872_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_auditReturnParameterList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_873(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_873_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_auditOther(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_874(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 876, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr).

yeccpars2_875(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_875_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_auditReply(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_876(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 830, Ss, Stack, T, Ts, Tzr);
yeccpars2_876(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_876_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_877(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_877_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_auditReply(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_878(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_878_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_actionReply(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_879: see yeccpars2_4

yeccpars2_880(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 882, Ss, Stack, T, Ts, Tzr);
yeccpars2_880(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_880_(Stack),
 yeccpars2(881, Cat, [880 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_881(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_881_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_ammsReply(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_882: see yeccpars2_841

yeccpars2_883(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 884, Ss, Stack, T, Ts, Tzr).

yeccpars2_884(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_884_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_ammsReplyBody(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_885(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_885_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_actionReplyBody(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_886: see yeccpars2_775

yeccpars2_887(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_887_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_commandReplyList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_888(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 886, Ss, Stack, T, Ts, Tzr);
yeccpars2_888(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_888_(Stack),
 yeccpars2(889, Cat, [888 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_889(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_889_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_commandReplyList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_890(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_890_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_transactionReplyBody(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_891(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 772, Ss, Stack, T, Ts, Tzr).

yeccpars2_892(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 891, Ss, Stack, T, Ts, Tzr);
yeccpars2_892(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_892_(Stack),
 yeccpars2(893, Cat, [892 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_893(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_893_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_actionReplyList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_894(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_894_(Stack),
 Nss = lists:nthtail(6, Ss),
 yeccpars2(yeccgoto_transactionReply(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_895: see yeccpars2_4

yeccpars2_896(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 897, Ss, Stack, T, Ts, Tzr).

yeccpars2_897(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 898, Ss, Stack, T, Ts, Tzr).

yeccpars2_898(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_898_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_transactionPending(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_899(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_899_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_transactionList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_900(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_900_(Stack),
 yeccpars2(yeccgoto_pathName(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_901(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_901_(Stack),
 yeccpars2(yeccgoto_deviceName(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_902(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_902(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_902_(Stack),
 yeccpars2(906, Cat, [902 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_903(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_903(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_903_(Stack),
 yeccpars2(905, Cat, [903 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_904(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_904_(Stack),
 yeccpars2(yeccgoto_mtpAddress(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_905(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_905_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_mId(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_906(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_906_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_mId(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccgoto_actionReply(766) -> 771;
yeccgoto_actionReply(891) -> 892.

yeccgoto_actionReplyBody(775) -> 784.

yeccgoto_actionReplyList(771) -> 890;
yeccgoto_actionReplyList(892) -> 893.

yeccgoto_actionRequest(131) -> 132;
yeccgoto_actionRequest(741) -> 742;
yeccgoto_actionRequest(747) -> 748;
yeccgoto_actionRequest(751) -> 752.

yeccgoto_actionRequestBody(137) -> 152.

yeccgoto_actionRequestItem(137) -> 151;
yeccgoto_actionRequestItem(517) -> 518.

yeccgoto_actionRequestItems(151) -> 516;
yeccgoto_actionRequestItems(518) -> 519.

yeccgoto_actionRequestList(132) -> 740;
yeccgoto_actionRequestList(742) -> 743;
yeccgoto_actionRequestList(748) -> 749;
yeccgoto_actionRequestList(752) -> 753.

yeccgoto_alternativeValue(278) -> 288.

yeccgoto_ammParameter(523) -> 533;
yeccgoto_ammParameter(736) -> 737.

yeccgoto_ammParameters(533) -> 735;
yeccgoto_ammParameters(737) -> 738.

yeccgoto_ammRequest(137) -> 150;
yeccgoto_ammRequest(517) -> 150.

yeccgoto_ammRequestBody(521) -> 522.

yeccgoto_ammToken(137) -> 149;
yeccgoto_ammToken(517) -> 149.

yeccgoto_ammsReply(775) -> 783;
yeccgoto_ammsReply(886) -> 783.

yeccgoto_ammsReplyBody(880) -> 881.

yeccgoto_ammsToken(775) -> 782;
yeccgoto_ammsToken(886) -> 782.

yeccgoto_auditDescriptor(189) -> 190;
yeccgoto_auditDescriptor(523) -> 532;
yeccgoto_auditDescriptor(736) -> 532.

yeccgoto_auditDescriptorBody(192) -> 204.

yeccgoto_auditItem(192) -> 203;
yeccgoto_auditItem(367) -> 368;
yeccgoto_auditItem(385) -> 398;
yeccgoto_auditItem(425) -> 398.

yeccgoto_auditItemList(203) -> 366;
yeccgoto_auditItemList(368) -> 369.

yeccgoto_auditOther(824) -> 826;
yeccgoto_auditOther(874) -> 875.

yeccgoto_auditReply(775) -> 781;
yeccgoto_auditReply(886) -> 781.

yeccgoto_auditRequest(137) -> 148;
yeccgoto_auditRequest(517) -> 148.

yeccgoto_auditReturnItem(192) -> 202;
yeccgoto_auditReturnItem(367) -> 202;
yeccgoto_auditReturnItem(385) -> 202;
yeccgoto_auditReturnItem(425) -> 202;
yeccgoto_auditReturnItem(841) -> 855;
yeccgoto_auditReturnItem(870) -> 855;
yeccgoto_auditReturnItem(882) -> 855.

yeccgoto_auditReturnParameter(841) -> 854;
yeccgoto_auditReturnParameter(870) -> 871;
yeccgoto_auditReturnParameter(882) -> 854.

yeccgoto_auditReturnParameterList(854) -> 869;
yeccgoto_auditReturnParameterList(871) -> 872.

yeccgoto_authenticationHeader(1) -> 4.

yeccgoto_commandReplyList(780) -> 885;
yeccgoto_commandReplyList(888) -> 889.

yeccgoto_commandReplys(775) -> 780;
yeccgoto_commandReplys(886) -> 888.

yeccgoto_commandRequest(137) -> 147;
yeccgoto_commandRequest(517) -> 147.

yeccgoto_contextAttrDescriptor(137) -> 146;
yeccgoto_contextAttrDescriptor(517) -> 146;
yeccgoto_contextAttrDescriptor(775) -> 146;
yeccgoto_contextAttrDescriptor(886) -> 146.

yeccgoto_contextAudit(137) -> 145;
yeccgoto_contextAudit(517) -> 145.

yeccgoto_contextAuditProperties(477) -> 482;
yeccgoto_contextAuditProperties(484) -> 485.

yeccgoto_contextAuditProperty(475) -> 477;
yeccgoto_contextAuditProperty(483) -> 484.

yeccgoto_contextID(134) -> 136;
yeccgoto_contextID(495) -> 496;
yeccgoto_contextID(498) -> 499;
yeccgoto_contextID(773) -> 774.

yeccgoto_contextIDs(496) -> 497;
yeccgoto_contextIDs(499) -> 500.

yeccgoto_contextIdList(488) -> 492.

yeccgoto_contextProperty(137) -> 144;
yeccgoto_contextProperty(517) -> 144;
yeccgoto_contextProperty(775) -> 779;
yeccgoto_contextProperty(886) -> 779.

yeccgoto_contextTerminationAudit(827) -> 829;
yeccgoto_contextTerminationAudit(876) -> 877.

yeccgoto_daddr(101) -> 103;
yeccgoto_daddr(102) -> 111;
yeccgoto_daddr(104) -> 105.

yeccgoto_deviceName(96) -> 903.

yeccgoto_digitMapDescriptor(523) -> 531;
yeccgoto_digitMapDescriptor(736) -> 531;
yeccgoto_digitMapDescriptor(841) -> 853;
yeccgoto_digitMapDescriptor(870) -> 853;
yeccgoto_digitMapDescriptor(882) -> 853.

yeccgoto_direction(272) -> 273.

yeccgoto_domainAddress(93) -> 99;
yeccgoto_domainAddress(410) -> 99;
yeccgoto_domainAddress(417) -> 99.

yeccgoto_domainName(93) -> 98;
yeccgoto_domainName(410) -> 98;
yeccgoto_domainName(417) -> 98.

yeccgoto_embedFirst(682) -> 684;
yeccgoto_embedFirst(713) -> 715.

yeccgoto_embedNoSig(673) -> 678;
yeccgoto_embedNoSig(718) -> 678.

yeccgoto_embedSig(692) -> 696;
yeccgoto_embedSig(703) -> 696.

yeccgoto_embedWithSig(673) -> 677;
yeccgoto_embedWithSig(718) -> 677.

yeccgoto_errorCode(833) -> 835.

yeccgoto_errorDescriptor(97) -> 124;
yeccgoto_errorDescriptor(434) -> 437;
yeccgoto_errorDescriptor(766) -> 770;
yeccgoto_errorDescriptor(775) -> 778;
yeccgoto_errorDescriptor(796) -> 798;
yeccgoto_errorDescriptor(821) -> 822;
yeccgoto_errorDescriptor(830) -> 831;
yeccgoto_errorDescriptor(841) -> 852;
yeccgoto_errorDescriptor(870) -> 852;
yeccgoto_errorDescriptor(882) -> 852;
yeccgoto_errorDescriptor(886) -> 887.

yeccgoto_errorText(836) -> 837.

yeccgoto_eventBufferControl(610) -> 614;
yeccgoto_eventBufferControl(627) -> 614.

yeccgoto_eventBufferControlState(622) -> 623.

yeccgoto_eventBufferDescriptor(523) -> 530;
yeccgoto_eventBufferDescriptor(736) -> 530;
yeccgoto_eventBufferDescriptor(841) -> 851;
yeccgoto_eventBufferDescriptor(870) -> 851;
yeccgoto_eventBufferDescriptor(882) -> 851.

yeccgoto_eventDM(673) -> 676;
yeccgoto_eventDM(692) -> 695;
yeccgoto_eventDM(703) -> 695;
yeccgoto_eventDM(718) -> 676.

yeccgoto_eventParameter(673) -> 675;
yeccgoto_eventParameter(718) -> 719.

yeccgoto_eventParameterName(356) -> 360;
yeccgoto_eventParameterName(452) -> 455;
yeccgoto_eventParameterName(458) -> 455;
yeccgoto_eventParameterName(673) -> 455;
yeccgoto_eventParameterName(692) -> 455;
yeccgoto_eventParameterName(703) -> 455;
yeccgoto_eventParameterName(718) -> 455.

yeccgoto_eventParameters(675) -> 717;
yeccgoto_eventParameters(719) -> 720.

yeccgoto_eventSpec(727) -> 729;
yeccgoto_eventSpec(731) -> 732.

yeccgoto_eventSpecList(729) -> 730;
yeccgoto_eventSpecList(732) -> 733.

yeccgoto_eventStream(356) -> 359.

yeccgoto_eventStreamOrOther(452) -> 454;
yeccgoto_eventStreamOrOther(458) -> 454;
yeccgoto_eventStreamOrOther(673) -> 674;
yeccgoto_eventStreamOrOther(692) -> 694;
yeccgoto_eventStreamOrOther(703) -> 694;
yeccgoto_eventStreamOrOther(718) -> 674.

yeccgoto_eventsDescriptor(523) -> 529;
yeccgoto_eventsDescriptor(736) -> 529;
yeccgoto_eventsDescriptor(841) -> 850;
yeccgoto_eventsDescriptor(870) -> 850;
yeccgoto_eventsDescriptor(882) -> 850.

yeccgoto_extension(385) -> 397;
yeccgoto_extension(425) -> 397.

yeccgoto_extensionParameter(385) -> 396;
yeccgoto_extensionParameter(425) -> 396.

yeccgoto_iepsValue(137) -> 143;
yeccgoto_iepsValue(517) -> 143;
yeccgoto_iepsValue(775) -> 143;
yeccgoto_iepsValue(886) -> 143.

yeccgoto_indAudauditReturnParameter(192) -> 201;
yeccgoto_indAudauditReturnParameter(367) -> 201;
yeccgoto_indAudauditReturnParameter(371) -> 372;
yeccgoto_indAudauditReturnParameter(385) -> 201;
yeccgoto_indAudauditReturnParameter(425) -> 201.

yeccgoto_indAudcontextAttrDescriptor(472) -> 473.

yeccgoto_indAuddigitMapDescriptor(192) -> 200;
yeccgoto_indAuddigitMapDescriptor(367) -> 200;
yeccgoto_indAuddigitMapDescriptor(371) -> 200;
yeccgoto_indAuddigitMapDescriptor(385) -> 200;
yeccgoto_indAuddigitMapDescriptor(425) -> 200.

yeccgoto_indAudeventBufferDescriptor(192) -> 199;
yeccgoto_indAudeventBufferDescriptor(367) -> 199;
yeccgoto_indAudeventBufferDescriptor(371) -> 199;
yeccgoto_indAudeventBufferDescriptor(385) -> 199;
yeccgoto_indAudeventBufferDescriptor(425) -> 199.

yeccgoto_indAudeventSpec(351) -> 353.

yeccgoto_indAudeventSpecParameter(356) -> 358.

yeccgoto_indAudeventsDescriptor(192) -> 198;
yeccgoto_indAudeventsDescriptor(367) -> 198;
yeccgoto_indAudeventsDescriptor(371) -> 198;
yeccgoto_indAudeventsDescriptor(385) -> 198;
yeccgoto_indAudeventsDescriptor(425) -> 198.

yeccgoto_indAudlocalControlDescriptor(312) -> 318;
yeccgoto_indAudlocalControlDescriptor(329) -> 318;
yeccgoto_indAudlocalControlDescriptor(341) -> 318.

yeccgoto_indAudlocalParm(332) -> 334;
yeccgoto_indAudlocalParm(336) -> 337.

yeccgoto_indAudlocalParmList(334) -> 335;
yeccgoto_indAudlocalParmList(337) -> 338.

yeccgoto_indAudmediaDescriptor(192) -> 197;
yeccgoto_indAudmediaDescriptor(367) -> 197;
yeccgoto_indAudmediaDescriptor(371) -> 197;
yeccgoto_indAudmediaDescriptor(385) -> 197;
yeccgoto_indAudmediaDescriptor(425) -> 197.

yeccgoto_indAudmediaParm(312) -> 317;
yeccgoto_indAudmediaParm(341) -> 342.

yeccgoto_indAudmediaParms(317) -> 340;
yeccgoto_indAudmediaParms(342) -> 343.

yeccgoto_indAudpackagesDescriptor(192) -> 196;
yeccgoto_indAudpackagesDescriptor(367) -> 196;
yeccgoto_indAudpackagesDescriptor(371) -> 196;
yeccgoto_indAudpackagesDescriptor(385) -> 196;
yeccgoto_indAudpackagesDescriptor(425) -> 196.

yeccgoto_indAudrequestedEvent(347) -> 349.

yeccgoto_indAudsignalList(221) -> 226.

yeccgoto_indAudsignalParm(221) -> 225.

yeccgoto_indAudsignalsDescriptor(192) -> 195;
yeccgoto_indAudsignalsDescriptor(367) -> 195;
yeccgoto_indAudsignalsDescriptor(371) -> 195;
yeccgoto_indAudsignalsDescriptor(385) -> 195;
yeccgoto_indAudsignalsDescriptor(425) -> 195.

yeccgoto_indAudstatisticsDescriptor(192) -> 194;
yeccgoto_indAudstatisticsDescriptor(312) -> 316;
yeccgoto_indAudstatisticsDescriptor(329) -> 316;
yeccgoto_indAudstatisticsDescriptor(341) -> 316;
yeccgoto_indAudstatisticsDescriptor(367) -> 194;
yeccgoto_indAudstatisticsDescriptor(371) -> 194;
yeccgoto_indAudstatisticsDescriptor(385) -> 194;
yeccgoto_indAudstatisticsDescriptor(425) -> 194.

yeccgoto_indAudstreamDescriptor(312) -> 315;
yeccgoto_indAudstreamDescriptor(341) -> 315.

yeccgoto_indAudstreamParm(312) -> 314;
yeccgoto_indAudstreamParm(329) -> 330;
yeccgoto_indAudstreamParm(341) -> 314.

yeccgoto_indAudterminationAudit(192) -> 193;
yeccgoto_indAudterminationAudit(367) -> 193;
yeccgoto_indAudterminationAudit(385) -> 193;
yeccgoto_indAudterminationAudit(425) -> 193.

yeccgoto_indAudterminationAuditList(201) -> 370;
yeccgoto_indAudterminationAuditList(372) -> 378.

yeccgoto_indAudterminationStateDescriptor(312) -> 313;
yeccgoto_indAudterminationStateDescriptor(341) -> 313.

yeccgoto_indAudterminationStateParm(323) -> 325.

yeccgoto_localControlDescriptor(598) -> 604;
yeccgoto_localControlDescriptor(633) -> 604;
yeccgoto_localControlDescriptor(636) -> 604;
yeccgoto_localControlDescriptor(663) -> 604.

yeccgoto_localParm(640) -> 642;
yeccgoto_localParm(658) -> 659.

yeccgoto_localParmList(642) -> 657;
yeccgoto_localParmList(659) -> 660.

yeccgoto_mId(93) -> 97;
yeccgoto_mId(410) -> 412;
yeccgoto_mId(417) -> 418.

yeccgoto_mediaDescriptor(523) -> 528;
yeccgoto_mediaDescriptor(736) -> 528;
yeccgoto_mediaDescriptor(841) -> 849;
yeccgoto_mediaDescriptor(870) -> 849;
yeccgoto_mediaDescriptor(882) -> 849.

yeccgoto_mediaParm(598) -> 603;
yeccgoto_mediaParm(663) -> 664.

yeccgoto_mediaParmList(603) -> 662;
yeccgoto_mediaParmList(664) -> 665.

yeccgoto_megacoMessage(0) -> 2.

yeccgoto_message(4) -> 94.

yeccgoto_messageBody(97) -> 123.

yeccgoto_modemDescriptor(523) -> 527;
yeccgoto_modemDescriptor(736) -> 527;
yeccgoto_modemDescriptor(841) -> 848;
yeccgoto_modemDescriptor(870) -> 848;
yeccgoto_modemDescriptor(882) -> 848.

yeccgoto_modemType(582) -> 596;
yeccgoto_modemType(583) -> 585;
yeccgoto_modemType(587) -> 588.

yeccgoto_modemTypeList(585) -> 586;
yeccgoto_modemTypeList(588) -> 589.

yeccgoto_mtpAddress(96) -> 902.

yeccgoto_muxDescriptor(523) -> 526;
yeccgoto_muxDescriptor(736) -> 526;
yeccgoto_muxDescriptor(841) -> 847;
yeccgoto_muxDescriptor(870) -> 847;
yeccgoto_muxDescriptor(882) -> 847.

yeccgoto_muxType(571) -> 573.

yeccgoto_notificationReason(259) -> 260;
yeccgoto_notificationReason(266) -> 267.

yeccgoto_notificationReasons(260) -> 265;
yeccgoto_notificationReasons(267) -> 268.

yeccgoto_notifyReply(775) -> 777;
yeccgoto_notifyReply(886) -> 777.

yeccgoto_notifyReplyBody(819) -> 820.

yeccgoto_notifyRequest(137) -> 142;
yeccgoto_notifyRequest(517) -> 142.

yeccgoto_notifyRequestBody(434) -> 436.

yeccgoto_observedEvent(441) -> 444;
yeccgoto_observedEvent(446) -> 447;
yeccgoto_observedEvent(727) -> 728;
yeccgoto_observedEvent(731) -> 728.

yeccgoto_observedEventBody(450) -> 451;
yeccgoto_observedEventBody(465) -> 466.

yeccgoto_observedEventParameter(452) -> 453;
yeccgoto_observedEventParameter(458) -> 459.

yeccgoto_observedEventParameters(453) -> 457;
yeccgoto_observedEventParameters(459) -> 460.

yeccgoto_observedEvents(444) -> 445;
yeccgoto_observedEvents(447) -> 448.

yeccgoto_observedEventsDescriptor(434) -> 435;
yeccgoto_observedEventsDescriptor(841) -> 846;
yeccgoto_observedEventsDescriptor(870) -> 846;
yeccgoto_observedEventsDescriptor(882) -> 846.

yeccgoto_onOrOff(468) -> 469;
yeccgoto_onOrOff(646) -> 647;
yeccgoto_onOrOff(648) -> 649.

yeccgoto_optAuditDescriptor(187) -> 188;
yeccgoto_optAuditDescriptor(510) -> 511;
yeccgoto_optAuditDescriptor(513) -> 514.

yeccgoto_optImmAckRequired(765) -> 766.

yeccgoto_optIndAudeventSpecParameter(352) -> 355.

yeccgoto_optIndAudsignalParm(214) -> 220;
yeccgoto_optIndAudsignalParm(377) -> 220.

yeccgoto_optPropertyParms(590) -> 591;
yeccgoto_optPropertyParms(596) -> 597.

yeccgoto_optSep(0) -> 1;
yeccgoto_optSep(91) -> 92;
yeccgoto_optSep(93) -> 96;
yeccgoto_optSep(109) -> 110;
yeccgoto_optSep(115) -> 116;
yeccgoto_optSep(410) -> 96;
yeccgoto_optSep(417) -> 96;
yeccgoto_optSep(441) -> 443;
yeccgoto_optSep(442) -> 462;
yeccgoto_optSep(446) -> 443;
yeccgoto_optSep(463) -> 464;
yeccgoto_optSep(727) -> 443;
yeccgoto_optSep(731) -> 443;
yeccgoto_optSep(902) -> 906;
yeccgoto_optSep(903) -> 905.

yeccgoto_packagesDescriptor(841) -> 845;
yeccgoto_packagesDescriptor(870) -> 845;
yeccgoto_packagesDescriptor(882) -> 845.

yeccgoto_packagesItem(308) -> 310;
yeccgoto_packagesItem(862) -> 863;
yeccgoto_packagesItem(865) -> 866.

yeccgoto_packagesItems(863) -> 864;
yeccgoto_packagesItems(866) -> 867.

yeccgoto_parmValue(239) -> 277;
yeccgoto_parmValue(396) -> 423;
yeccgoto_parmValue(455) -> 456;
yeccgoto_parmValue(491) -> 503.

yeccgoto_pathName(96) -> 901.

yeccgoto_pkgdName(216) -> 218;
yeccgoto_pkgdName(221) -> 224;
yeccgoto_pkgdName(232) -> 224;
yeccgoto_pkgdName(347) -> 348;
yeccgoto_pkgdName(351) -> 352;
yeccgoto_pkgdName(443) -> 450;
yeccgoto_pkgdName(464) -> 465;
yeccgoto_pkgdName(475) -> 476;
yeccgoto_pkgdName(483) -> 476;
yeccgoto_pkgdName(488) -> 491;
yeccgoto_pkgdName(505) -> 491;
yeccgoto_pkgdName(542) -> 544;
yeccgoto_pkgdName(548) -> 544;
yeccgoto_pkgdName(552) -> 224;
yeccgoto_pkgdName(559) -> 224;
yeccgoto_pkgdName(562) -> 224;
yeccgoto_pkgdName(567) -> 224;
yeccgoto_pkgdName(592) -> 491;
yeccgoto_pkgdName(610) -> 491;
yeccgoto_pkgdName(627) -> 491;
yeccgoto_pkgdName(640) -> 491;
yeccgoto_pkgdName(658) -> 491;
yeccgoto_pkgdName(669) -> 671;
yeccgoto_pkgdName(688) -> 690;
yeccgoto_pkgdName(708) -> 690;
yeccgoto_pkgdName(723) -> 671.

yeccgoto_portNumber(107) -> 109;
yeccgoto_portNumber(114) -> 115;
yeccgoto_portNumber(410) -> 411.

yeccgoto_priority(137) -> 141;
yeccgoto_priority(517) -> 141;
yeccgoto_priority(775) -> 141;
yeccgoto_priority(886) -> 141.

yeccgoto_propertyParm(488) -> 490;
yeccgoto_propertyParm(505) -> 506;
yeccgoto_propertyParm(592) -> 593;
yeccgoto_propertyParm(610) -> 613;
yeccgoto_propertyParm(627) -> 613;
yeccgoto_propertyParm(640) -> 641;
yeccgoto_propertyParm(658) -> 641.

yeccgoto_propertyParmList(490) -> 504;
yeccgoto_propertyParmList(506) -> 507;
yeccgoto_propertyParmList(593) -> 594.

yeccgoto_propertyParms(488) -> 489.

yeccgoto_requestID(255) -> 257;
yeccgoto_requestID(345) -> 346;
yeccgoto_requestID(439) -> 440;
yeccgoto_requestID(667) -> 668;
yeccgoto_requestID(686) -> 687.

yeccgoto_requestedEvent(669) -> 670;
yeccgoto_requestedEvent(723) -> 724.

yeccgoto_requestedEventBody(671) -> 672.

yeccgoto_requestedEvents(670) -> 722;
yeccgoto_requestedEvents(724) -> 725.

yeccgoto_safeToken(4) -> 93;
yeccgoto_safeToken(6) -> 7;
yeccgoto_safeToken(88) -> 89;
yeccgoto_safeToken(90) -> 91;
yeccgoto_safeToken(96) -> 900;
yeccgoto_safeToken(100) -> 112;
yeccgoto_safeToken(101) -> 102;
yeccgoto_safeToken(102) -> 102;
yeccgoto_safeToken(104) -> 102;
yeccgoto_safeToken(107) -> 108;
yeccgoto_safeToken(114) -> 108;
yeccgoto_safeToken(130) -> 746;
yeccgoto_safeToken(134) -> 135;
yeccgoto_safeToken(168) -> 172;
yeccgoto_safeToken(173) -> 172;
yeccgoto_safeToken(182) -> 172;
yeccgoto_safeToken(186) -> 172;
yeccgoto_safeToken(216) -> 217;
yeccgoto_safeToken(221) -> 217;
yeccgoto_safeToken(229) -> 231;
yeccgoto_safeToken(232) -> 217;
yeccgoto_safeToken(237) -> 239;
yeccgoto_safeToken(247) -> 249;
yeccgoto_safeToken(255) -> 256;
yeccgoto_safeToken(270) -> 271;
yeccgoto_safeToken(278) -> 283;
yeccgoto_safeToken(279) -> 283;
yeccgoto_safeToken(280) -> 283;
yeccgoto_safeToken(281) -> 283;
yeccgoto_safeToken(289) -> 283;
yeccgoto_safeToken(290) -> 283;
yeccgoto_safeToken(293) -> 283;
yeccgoto_safeToken(294) -> 283;
yeccgoto_safeToken(304) -> 239;
yeccgoto_safeToken(308) -> 309;
yeccgoto_safeToken(323) -> 324;
yeccgoto_safeToken(327) -> 249;
yeccgoto_safeToken(332) -> 333;
yeccgoto_safeToken(336) -> 333;
yeccgoto_safeToken(345) -> 256;
yeccgoto_safeToken(347) -> 217;
yeccgoto_safeToken(351) -> 217;
yeccgoto_safeToken(356) -> 357;
yeccgoto_safeToken(362) -> 249;
yeccgoto_safeToken(380) -> 172;
yeccgoto_safeToken(385) -> 395;
yeccgoto_safeToken(408) -> 409;
yeccgoto_safeToken(410) -> 108;
yeccgoto_safeToken(413) -> 283;
yeccgoto_safeToken(415) -> 416;
yeccgoto_safeToken(419) -> 420;
yeccgoto_safeToken(421) -> 422;
yeccgoto_safeToken(425) -> 395;
yeccgoto_safeToken(430) -> 431;
yeccgoto_safeToken(432) -> 172;
yeccgoto_safeToken(439) -> 256;
yeccgoto_safeToken(443) -> 217;
yeccgoto_safeToken(452) -> 357;
yeccgoto_safeToken(458) -> 357;
yeccgoto_safeToken(464) -> 217;
yeccgoto_safeToken(475) -> 217;
yeccgoto_safeToken(483) -> 217;
yeccgoto_safeToken(488) -> 217;
yeccgoto_safeToken(495) -> 135;
yeccgoto_safeToken(498) -> 135;
yeccgoto_safeToken(505) -> 217;
yeccgoto_safeToken(509) -> 172;
yeccgoto_safeToken(512) -> 172;
yeccgoto_safeToken(520) -> 172;
yeccgoto_safeToken(542) -> 217;
yeccgoto_safeToken(545) -> 283;
yeccgoto_safeToken(548) -> 217;
yeccgoto_safeToken(552) -> 217;
yeccgoto_safeToken(557) -> 231;
yeccgoto_safeToken(559) -> 217;
yeccgoto_safeToken(562) -> 217;
yeccgoto_safeToken(567) -> 217;
yeccgoto_safeToken(571) -> 572;
yeccgoto_safeToken(575) -> 172;
yeccgoto_safeToken(578) -> 172;
yeccgoto_safeToken(582) -> 584;
yeccgoto_safeToken(583) -> 584;
yeccgoto_safeToken(587) -> 584;
yeccgoto_safeToken(592) -> 217;
yeccgoto_safeToken(610) -> 217;
yeccgoto_safeToken(627) -> 217;
yeccgoto_safeToken(631) -> 249;
yeccgoto_safeToken(640) -> 217;
yeccgoto_safeToken(658) -> 217;
yeccgoto_safeToken(667) -> 256;
yeccgoto_safeToken(669) -> 217;
yeccgoto_safeToken(673) -> 357;
yeccgoto_safeToken(686) -> 256;
yeccgoto_safeToken(688) -> 217;
yeccgoto_safeToken(692) -> 357;
yeccgoto_safeToken(703) -> 357;
yeccgoto_safeToken(708) -> 217;
yeccgoto_safeToken(718) -> 357;
yeccgoto_safeToken(723) -> 217;
yeccgoto_safeToken(755) -> 757;
yeccgoto_safeToken(759) -> 757;
yeccgoto_safeToken(763) -> 746;
yeccgoto_safeToken(773) -> 135;
yeccgoto_safeToken(793) -> 172;
yeccgoto_safeToken(818) -> 172;
yeccgoto_safeToken(824) -> 172;
yeccgoto_safeToken(830) -> 172;
yeccgoto_safeToken(833) -> 834;
yeccgoto_safeToken(862) -> 309;
yeccgoto_safeToken(865) -> 309;
yeccgoto_safeToken(874) -> 172;
yeccgoto_safeToken(879) -> 172;
yeccgoto_safeToken(895) -> 746.

yeccgoto_secondEventParameter(692) -> 693;
yeccgoto_secondEventParameter(703) -> 704.

yeccgoto_secondEventParameters(693) -> 702;
yeccgoto_secondEventParameters(704) -> 705.

yeccgoto_secondRequestedEvent(688) -> 689;
yeccgoto_secondRequestedEvent(708) -> 709.

yeccgoto_secondRequestedEventBody(690) -> 691.

yeccgoto_secondRequestedEvents(689) -> 707;
yeccgoto_secondRequestedEvents(709) -> 710.

yeccgoto_servChgReplyParm(800) -> 806;
yeccgoto_servChgReplyParm(812) -> 813.

yeccgoto_servChgReplyParms(806) -> 811;
yeccgoto_servChgReplyParms(813) -> 814.

yeccgoto_serviceChangeAddress(385) -> 394;
yeccgoto_serviceChangeAddress(425) -> 394;
yeccgoto_serviceChangeAddress(800) -> 805;
yeccgoto_serviceChangeAddress(812) -> 805.

yeccgoto_serviceChangeDelay(385) -> 393;
yeccgoto_serviceChangeDelay(425) -> 393.

yeccgoto_serviceChangeDescriptor(382) -> 383.

yeccgoto_serviceChangeMethod(385) -> 392;
yeccgoto_serviceChangeMethod(425) -> 392.

yeccgoto_serviceChangeMgcId(385) -> 391;
yeccgoto_serviceChangeMgcId(425) -> 391;
yeccgoto_serviceChangeMgcId(800) -> 804;
yeccgoto_serviceChangeMgcId(812) -> 804.

yeccgoto_serviceChangeParm(385) -> 390;
yeccgoto_serviceChangeParm(425) -> 426.

yeccgoto_serviceChangeParms(390) -> 424;
yeccgoto_serviceChangeParms(426) -> 427.

yeccgoto_serviceChangeProfile(385) -> 389;
yeccgoto_serviceChangeProfile(425) -> 389;
yeccgoto_serviceChangeProfile(800) -> 803;
yeccgoto_serviceChangeProfile(812) -> 803.

yeccgoto_serviceChangeReason(385) -> 388;
yeccgoto_serviceChangeReason(425) -> 388.

yeccgoto_serviceChangeReply(775) -> 776;
yeccgoto_serviceChangeReply(886) -> 776.

yeccgoto_serviceChangeReplyBody(794) -> 795.

yeccgoto_serviceChangeReplyDescriptor(796) -> 797.

yeccgoto_serviceChangeRequest(137) -> 140;
yeccgoto_serviceChangeRequest(517) -> 140.

yeccgoto_serviceChangeVersion(385) -> 387;
yeccgoto_serviceChangeVersion(425) -> 387;
yeccgoto_serviceChangeVersion(800) -> 802;
yeccgoto_serviceChangeVersion(812) -> 802.

yeccgoto_serviceState(617) -> 618.

yeccgoto_serviceStates(610) -> 612;
yeccgoto_serviceStates(627) -> 612.

yeccgoto_sigParameter(237) -> 238;
yeccgoto_sigParameter(304) -> 305.

yeccgoto_sigParameters(238) -> 303;
yeccgoto_sigParameters(305) -> 306.

yeccgoto_signalList(552) -> 555;
yeccgoto_signalList(567) -> 555.

yeccgoto_signalListId(229) -> 230;
yeccgoto_signalListId(557) -> 558.

yeccgoto_signalListParm(232) -> 234;
yeccgoto_signalListParm(559) -> 560;
yeccgoto_signalListParm(562) -> 563.

yeccgoto_signalListParms(560) -> 561;
yeccgoto_signalListParms(563) -> 564.

yeccgoto_signalName(221) -> 223;
yeccgoto_signalName(232) -> 223;
yeccgoto_signalName(552) -> 223;
yeccgoto_signalName(559) -> 223;
yeccgoto_signalName(562) -> 223;
yeccgoto_signalName(567) -> 223.

yeccgoto_signalParm(552) -> 554;
yeccgoto_signalParm(567) -> 568.

yeccgoto_signalParms(554) -> 566;
yeccgoto_signalParms(568) -> 569.

yeccgoto_signalRequest(221) -> 222;
yeccgoto_signalRequest(232) -> 233;
yeccgoto_signalRequest(552) -> 553;
yeccgoto_signalRequest(559) -> 233;
yeccgoto_signalRequest(562) -> 233;
yeccgoto_signalRequest(567) -> 553.

yeccgoto_signalType(250) -> 251.

yeccgoto_signalsDescriptor(523) -> 525;
yeccgoto_signalsDescriptor(682) -> 683;
yeccgoto_signalsDescriptor(699) -> 700;
yeccgoto_signalsDescriptor(736) -> 525;
yeccgoto_signalsDescriptor(841) -> 844;
yeccgoto_signalsDescriptor(870) -> 844;
yeccgoto_signalsDescriptor(882) -> 844.

yeccgoto_statisticsDescriptor(523) -> 524;
yeccgoto_statisticsDescriptor(598) -> 602;
yeccgoto_statisticsDescriptor(633) -> 602;
yeccgoto_statisticsDescriptor(636) -> 602;
yeccgoto_statisticsDescriptor(663) -> 602;
yeccgoto_statisticsDescriptor(736) -> 524;
yeccgoto_statisticsDescriptor(841) -> 843;
yeccgoto_statisticsDescriptor(870) -> 843;
yeccgoto_statisticsDescriptor(882) -> 843.

yeccgoto_statisticsParameter(542) -> 543;
yeccgoto_statisticsParameter(548) -> 549.

yeccgoto_statisticsParameters(543) -> 547;
yeccgoto_statisticsParameters(549) -> 550.

yeccgoto_streamDescriptor(598) -> 601;
yeccgoto_streamDescriptor(663) -> 601.

yeccgoto_streamID(247) -> 248;
yeccgoto_streamID(327) -> 328;
yeccgoto_streamID(362) -> 363;
yeccgoto_streamID(631) -> 632.

yeccgoto_streamModes(650) -> 651.

yeccgoto_streamParm(598) -> 600;
yeccgoto_streamParm(633) -> 634;
yeccgoto_streamParm(636) -> 637;
yeccgoto_streamParm(663) -> 600.

yeccgoto_streamParmList(634) -> 635;
yeccgoto_streamParmList(637) -> 638.

yeccgoto_subtractRequest(137) -> 139;
yeccgoto_subtractRequest(517) -> 139.

yeccgoto_terminationA(168) -> 171;
yeccgoto_terminationA(182) -> 171.

yeccgoto_terminationAudit(841) -> 842;
yeccgoto_terminationAudit(882) -> 883.

yeccgoto_terminationB(173) -> 175.

yeccgoto_terminationID(168) -> 170;
yeccgoto_terminationID(173) -> 174;
yeccgoto_terminationID(182) -> 170;
yeccgoto_terminationID(186) -> 187;
yeccgoto_terminationID(380) -> 381;
yeccgoto_terminationID(432) -> 433;
yeccgoto_terminationID(509) -> 510;
yeccgoto_terminationID(512) -> 513;
yeccgoto_terminationID(520) -> 521;
yeccgoto_terminationID(575) -> 576;
yeccgoto_terminationID(578) -> 579;
yeccgoto_terminationID(793) -> 794;
yeccgoto_terminationID(818) -> 819;
yeccgoto_terminationID(824) -> 825;
yeccgoto_terminationID(830) -> 576;
yeccgoto_terminationID(874) -> 825;
yeccgoto_terminationID(879) -> 880.

yeccgoto_terminationIDList(573) -> 574;
yeccgoto_terminationIDList(827) -> 828;
yeccgoto_terminationIDList(876) -> 828.

yeccgoto_terminationIDListRepeat(576) -> 577;
yeccgoto_terminationIDListRepeat(579) -> 580.

yeccgoto_terminationStateDescriptor(598) -> 599;
yeccgoto_terminationStateDescriptor(663) -> 599.

yeccgoto_terminationStateParm(610) -> 611;
yeccgoto_terminationStateParm(627) -> 628.

yeccgoto_terminationStateParms(611) -> 626;
yeccgoto_terminationStateParms(628) -> 629.

yeccgoto_timeStamp(385) -> 386;
yeccgoto_timeStamp(425) -> 386;
yeccgoto_timeStamp(441) -> 442;
yeccgoto_timeStamp(446) -> 442;
yeccgoto_timeStamp(727) -> 442;
yeccgoto_timeStamp(731) -> 442;
yeccgoto_timeStamp(800) -> 801;
yeccgoto_timeStamp(812) -> 801.

yeccgoto_topologyDescriptor(137) -> 138;
yeccgoto_topologyDescriptor(517) -> 138;
yeccgoto_topologyDescriptor(775) -> 138;
yeccgoto_topologyDescriptor(886) -> 138.

yeccgoto_topologyDirection(176) -> 177.

yeccgoto_topologyTriple(168) -> 169;
yeccgoto_topologyTriple(182) -> 183.

yeccgoto_topologyTripleList(169) -> 181;
yeccgoto_topologyTripleList(183) -> 184.

yeccgoto_transactionAck(755) -> 756;
yeccgoto_transactionAck(759) -> 760.

yeccgoto_transactionAckList(756) -> 758;
yeccgoto_transactionAckList(760) -> 761.

yeccgoto_transactionID(130) -> 745;
yeccgoto_transactionID(763) -> 764;
yeccgoto_transactionID(895) -> 896.

yeccgoto_transactionItem(97) -> 122;
yeccgoto_transactionItem(122) -> 122.

yeccgoto_transactionList(97) -> 121;
yeccgoto_transactionList(122) -> 899.

yeccgoto_transactionPending(97) -> 120;
yeccgoto_transactionPending(122) -> 120.

yeccgoto_transactionReply(97) -> 119;
yeccgoto_transactionReply(122) -> 119.

yeccgoto_transactionReplyBody(766) -> 769.

yeccgoto_transactionRequest(97) -> 118;
yeccgoto_transactionRequest(122) -> 118.

yeccgoto_transactionResponseAck(97) -> 117;
yeccgoto_transactionResponseAck(122) -> 117.

yeccgoto_value(278) -> 287;
yeccgoto_value(279) -> 286;
yeccgoto_value(280) -> 285;
yeccgoto_value(281) -> 282;
yeccgoto_value(289) -> 300;
yeccgoto_value(290) -> 291;
yeccgoto_value(293) -> 297;
yeccgoto_value(294) -> 295;
yeccgoto_value(413) -> 414;
yeccgoto_value(545) -> 546.

yeccgoto_valueList(291) -> 292;
yeccgoto_valueList(295) -> 296;
yeccgoto_valueList(300) -> 301.

-compile({inline,{yeccpars2_0_,1}}).
-file("megaco_text_parser_prev3b.yrl", 462).
yeccpars2_0_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_1_,1}}).
-file("megaco_text_parser_prev3b.yrl", 467).
yeccpars2_1_(Stack) ->
 [begin
   asn1_NOVALUE
  end | Stack].

-compile({inline,{yeccpars2_3_,1}}).
-file("megaco_text_parser_prev3b.yrl", 461).
yeccpars2_3_([__1 | Stack]) ->
 [begin
   sep
  end | Stack].

-compile({inline,{yeccpars2_8_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1443).
yeccpars2_8_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_9_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1442).
yeccpars2_9_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_10_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1444).
yeccpars2_10_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_11_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1445).
yeccpars2_11_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_12_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1447).
yeccpars2_12_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_13_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1448).
yeccpars2_13_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_14_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1449).
yeccpars2_14_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_15_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1452).
yeccpars2_15_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_16_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1450).
yeccpars2_16_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_17_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1460).
yeccpars2_17_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_18_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1457).
yeccpars2_18_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_19_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1458).
yeccpars2_19_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_20_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1459).
yeccpars2_20_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_21_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1461).
yeccpars2_21_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_22_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1462).
yeccpars2_22_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_23_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1465).
yeccpars2_23_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_24_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1469).
yeccpars2_24_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_25_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1470).
yeccpars2_25_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_26_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1471).
yeccpars2_26_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_27_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1472).
yeccpars2_27_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_28_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1473).
yeccpars2_28_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_29_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1474).
yeccpars2_29_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_30_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1475).
yeccpars2_30_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_31_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1477).
yeccpars2_31_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_32_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1483).
yeccpars2_32_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_33_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1478).
yeccpars2_33_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_34_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1480).
yeccpars2_34_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_35_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1481).
yeccpars2_35_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_36_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1482).
yeccpars2_36_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_37_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1484).
yeccpars2_37_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_38_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1487).
yeccpars2_38_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_39_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1489).
yeccpars2_39_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_40_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1488).
yeccpars2_40_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_41_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1492).
yeccpars2_41_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_42_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1493).
yeccpars2_42_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_43_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1494).
yeccpars2_43_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_44_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1502).
yeccpars2_44_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_45_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1501).
yeccpars2_45_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_46_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1503).
yeccpars2_46_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_47_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1506).
yeccpars2_47_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_48_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1508).
yeccpars2_48_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_49_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1507).
yeccpars2_49_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_50_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1505).
yeccpars2_50_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_51_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1510).
yeccpars2_51_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_52_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1509).
yeccpars2_52_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_53_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1512).
yeccpars2_53_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_54_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1514).
yeccpars2_54_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_55_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1515).
yeccpars2_55_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_56_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1516).
yeccpars2_56_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_57_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1517).
yeccpars2_57_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_58_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1519).
yeccpars2_58_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_59_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1524).
yeccpars2_59_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_60_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1525).
yeccpars2_60_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_61_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1520).
yeccpars2_61_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_62_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1521).
yeccpars2_62_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_63_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1440).
yeccpars2_63_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_64_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1526).
yeccpars2_64_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_65_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1527).
yeccpars2_65_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_66_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1532).
yeccpars2_66_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_67_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1530).
yeccpars2_67_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_68_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1529).
yeccpars2_68_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_69_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1528).
yeccpars2_69_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_70_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1533).
yeccpars2_70_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_71_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1535).
yeccpars2_71_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_72_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1537).
yeccpars2_72_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_73_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1539).
yeccpars2_73_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_74_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1540).
yeccpars2_74_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_75_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1541).
yeccpars2_75_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_76_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1542).
yeccpars2_76_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_77_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1544).
yeccpars2_77_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_78_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1545).
yeccpars2_78_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_79_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1546).
yeccpars2_79_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_80_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1547).
yeccpars2_80_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_81_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1548).
yeccpars2_81_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_82_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1549).
yeccpars2_82_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_83_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1550).
yeccpars2_83_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_84_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1551).
yeccpars2_84_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_85_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1552).
yeccpars2_85_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_86_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1553).
yeccpars2_86_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_87_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1554).
yeccpars2_87_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_91_,1}}).
-file("megaco_text_parser_prev3b.yrl", 462).
yeccpars2_91_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_92_,1}}).
-file("megaco_text_parser_prev3b.yrl", 466).
yeccpars2_92_([__8,__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   ensure_auth_header ( __3 , __5 , __7 )
  end | Stack].

-compile({inline,{yeccpars2_93_,1}}).
-file("megaco_text_parser_prev3b.yrl", 462).
yeccpars2_93_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_95_,1}}).
-file("megaco_text_parser_prev3b.yrl", 459).
yeccpars2_95_([__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'MegacoMessage' { authHeader = __2 , mess = __3 }
  end | Stack].

-compile({inline,{yeccpars2_101_,1}}).
-file("megaco_text_parser_prev3b.yrl", 947).
yeccpars2_101_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_102_,1}}).
-file("megaco_text_parser_prev3b.yrl", 947).
yeccpars2_102_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_104_,1}}).
-file("megaco_text_parser_prev3b.yrl", 947).
yeccpars2_104_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_105_,1}}).
-file("megaco_text_parser_prev3b.yrl", 948).
yeccpars2_105_([__2,__1 | Stack]) ->
 [begin
   [ colon | __2 ]
  end | Stack].

-compile({inline,{yeccpars2_106_,1}}).
-file("megaco_text_parser_prev3b.yrl", 945).
yeccpars2_106_([__3,__2,__1 | Stack]) ->
 [begin
   ensure_domainAddress ( __2 , asn1_NOVALUE )
  end | Stack].

-compile({inline,{yeccpars2_108_,1}}).
-file("megaco_text_parser_prev3b.yrl", 952).
yeccpars2_108_([__1 | Stack]) ->
 [begin
   ensure_uint16 ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_109_,1}}).
-file("megaco_text_parser_prev3b.yrl", 462).
yeccpars2_109_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_110_,1}}).
-file("megaco_text_parser_prev3b.yrl", 943).
yeccpars2_110_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   ensure_domainAddress ( __2 , __5 )
  end | Stack].

-compile({inline,{yeccpars2_111_,1}}).
-file("megaco_text_parser_prev3b.yrl", 949).
yeccpars2_111_([__2,__1 | Stack]) ->
 [begin
   [ __1 | __2 ]
  end | Stack].

-compile({inline,{yeccpars2_113_,1}}).
-file("megaco_text_parser_prev3b.yrl", 935).
yeccpars2_113_([__3,__2,__1 | Stack]) ->
 [begin
   ensure_domainName ( __2 , asn1_NOVALUE )
  end | Stack].

-compile({inline,{yeccpars2_115_,1}}).
-file("megaco_text_parser_prev3b.yrl", 462).
yeccpars2_115_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_116_,1}}).
-file("megaco_text_parser_prev3b.yrl", 933).
yeccpars2_116_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   ensure_domainName ( __2 , __5 )
  end | Stack].

-compile({inline,{yeccpars2_117_,1}}).
-file("megaco_text_parser_prev3b.yrl", 480).
yeccpars2_117_([__1 | Stack]) ->
 [begin
   { transactionResponseAck , __1 }
  end | Stack].

-compile({inline,{yeccpars2_118_,1}}).
-file("megaco_text_parser_prev3b.yrl", 477).
yeccpars2_118_([__1 | Stack]) ->
 [begin
   { transactionRequest , __1 }
  end | Stack].

-compile({inline,{yeccpars2_119_,1}}).
-file("megaco_text_parser_prev3b.yrl", 478).
yeccpars2_119_([__1 | Stack]) ->
 [begin
   { transactionReply , __1 }
  end | Stack].

-compile({inline,{yeccpars2_120_,1}}).
-file("megaco_text_parser_prev3b.yrl", 479).
yeccpars2_120_([__1 | Stack]) ->
 [begin
   { transactionPending , __1 }
  end | Stack].

-compile({inline,{yeccpars2_121_,1}}).
-file("megaco_text_parser_prev3b.yrl", 472).
yeccpars2_121_([__1 | Stack]) ->
 [begin
   { transactions , __1 }
  end | Stack].

-compile({inline,{yeccpars2_122_,1}}).
-file("megaco_text_parser_prev3b.yrl", 474).
yeccpars2_122_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_123_,1}}).
-file("megaco_text_parser_prev3b.yrl", 469).
yeccpars2_123_([__3,__2,__1 | Stack]) ->
 [begin
   ensure_message ( __1 , __2 , __3 )
  end | Stack].

-compile({inline,{yeccpars2_124_,1}}).
-file("megaco_text_parser_prev3b.yrl", 471).
yeccpars2_124_([__1 | Stack]) ->
 [begin
   { messageError , __1 }
  end | Stack].

-compile({inline,{yeccpars2_132_,1}}).
-file("megaco_text_parser_prev3b.yrl", 507).
yeccpars2_132_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_135_,1}}).
-file("megaco_text_parser_prev3b.yrl", 940).
yeccpars2_135_([__1 | Stack]) ->
 [begin
   ensure_contextID ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_138_,1}}).
-file("megaco_text_parser_prev3b.yrl", 531).
yeccpars2_138_([__1 | Stack]) ->
 [begin
   { topology , __1 }
  end | Stack].

-compile({inline,{yeccpars2_141_,1}}).
-file("megaco_text_parser_prev3b.yrl", 532).
yeccpars2_141_([__1 | Stack]) ->
 [begin
   { priority , __1 }
  end | Stack].

-compile({inline,{yeccpars2_143_,1}}).
-file("megaco_text_parser_prev3b.yrl", 535).
yeccpars2_143_([__1 | Stack]) ->
 [begin
   { iepsCallind , __1 }
  end | Stack].

-compile({inline,{yeccpars2_144_,1}}).
-file("megaco_text_parser_prev3b.yrl", 520).
yeccpars2_144_([__1 | Stack]) ->
 [begin
   { contextProp , __1 }
  end | Stack].

-compile({inline,{yeccpars2_145_,1}}).
-file("megaco_text_parser_prev3b.yrl", 521).
yeccpars2_145_([__1 | Stack]) ->
 [begin
   { contextAudit , __1 }
  end | Stack].

-compile({inline,{yeccpars2_147_,1}}).
-file("megaco_text_parser_prev3b.yrl", 522).
yeccpars2_147_([__1 | Stack]) ->
 [begin
   { commandRequest , __1 }
  end | Stack].

-compile({inline,{yeccpars2_151_,1}}).
-file("megaco_text_parser_prev3b.yrl", 517).
yeccpars2_151_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_153_,1}}).
-file("megaco_text_parser_prev3b.yrl", 624).
yeccpars2_153_([__1 | Stack]) ->
 [begin
   { addReq , __1 }
  end | Stack].

-compile({inline,{yeccpars2_158_,1}}).
-file("megaco_text_parser_prev3b.yrl", 534).
yeccpars2_158_([__1 | Stack]) ->
 [begin
   { emergency , false }
  end | Stack].

-compile({inline,{yeccpars2_159_,1}}).
-file("megaco_text_parser_prev3b.yrl", 533).
yeccpars2_159_([__1 | Stack]) ->
 [begin
   { emergency , true }
  end | Stack].

-compile({inline,{yeccpars2_161_,1}}).
-file("megaco_text_parser_prev3b.yrl", 626).
yeccpars2_161_([__1 | Stack]) ->
 [begin
   { modReq , __1 }
  end | Stack].

-compile({inline,{yeccpars2_162_,1}}).
-file("megaco_text_parser_prev3b.yrl", 625).
yeccpars2_162_([__1 | Stack]) ->
 [begin
   { moveReq , __1 }
  end | Stack].

-compile({inline,{yeccpars2_169_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1423).
yeccpars2_169_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_172_,1}}).
-file("megaco_text_parser_prev3b.yrl", 968).
yeccpars2_172_([__1 | Stack]) ->
 [begin
   ensure_terminationID ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_177_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1419).
yeccpars2_177_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'TopologyRequest' { terminationFrom = __1 ,
    terminationTo = __3 ,
    topologyDirection = __5 }
  end | Stack].

-compile({inline,{yeccpars2_178_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1427).
yeccpars2_178_([__1 | Stack]) ->
 [begin
   bothway
  end | Stack].

-compile({inline,{yeccpars2_179_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1428).
yeccpars2_179_([__1 | Stack]) ->
 [begin
   isolate
  end | Stack].

-compile({inline,{yeccpars2_180_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1429).
yeccpars2_180_([__1 | Stack]) ->
 [begin
   oneway
  end | Stack].

-compile({inline,{yeccpars2_183_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1423).
yeccpars2_183_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_184_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1425).
yeccpars2_184_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_185_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1410).
yeccpars2_185_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __3 | __4 ]
  end | Stack].

-compile({inline,{yeccpars2_187_,1}}).
-file("megaco_text_parser_prev3b.yrl", 665).
yeccpars2_187_(Stack) ->
 [begin
   asn1_NOVALUE
  end | Stack].

-compile({inline,{yeccpars2_188_,1}}).
-file("megaco_text_parser_prev3b.yrl", 659).
yeccpars2_188_([__4,__3,__2,__1 | Stack]) ->
 [begin
   make_commandRequest ( { subtractReq , __1 } ,
    # 'SubtractRequest' { terminationID = [ __3 ] ,
    auditDescriptor = __4 } )
  end | Stack].

-compile({inline,{yeccpars2_192_,1}}).
-file("megaco_text_parser_prev3b.yrl", 725).
yeccpars2_192_(Stack) ->
 [begin
   asn1_NOVALUE
  end | Stack].

-compile({inline,{yeccpars2_193_,1}}).
-file("megaco_text_parser_prev3b.yrl", 746).
yeccpars2_193_([__1 | Stack]) ->
 [begin
   { terminationAudit , __1 }
  end | Stack].

-compile({inline,{yeccpars2_194_,1}}).
-file("megaco_text_parser_prev3b.yrl", 773).
yeccpars2_194_([__1 | Stack]) ->
 [begin
   { indAudStatisticsDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_195_,1}}).
-file("megaco_text_parser_prev3b.yrl", 767).
yeccpars2_195_([__1 | Stack]) ->
 [begin
   { indAudSignalsDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_196_,1}}).
-file("megaco_text_parser_prev3b.yrl", 775).
yeccpars2_196_([__1 | Stack]) ->
 [begin
   { indAudPackagesDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_197_,1}}).
-file("megaco_text_parser_prev3b.yrl", 763).
yeccpars2_197_([__1 | Stack]) ->
 [begin
   { indAudMediaDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_198_,1}}).
-file("megaco_text_parser_prev3b.yrl", 765).
yeccpars2_198_([__1 | Stack]) ->
 [begin
   { indAudEventsDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_199_,1}}).
-file("megaco_text_parser_prev3b.yrl", 771).
yeccpars2_199_([__1 | Stack]) ->
 [begin
   { indAudEventBufferDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_200_,1}}).
-file("megaco_text_parser_prev3b.yrl", 769).
yeccpars2_200_([__1 | Stack]) ->
 [begin
   { indAudDigitMapDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_201_,1}}).
-file("megaco_text_parser_prev3b.yrl", 760).
yeccpars2_201_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_203_,1}}).
-file("megaco_text_parser_prev3b.yrl", 728).
yeccpars2_203_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_205_,1}}).
-file("megaco_text_parser_prev3b.yrl", 867).
yeccpars2_205_([__1 | Stack]) ->
 [begin
   ensure_IADMD ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_206_,1}}).
-file("megaco_text_parser_prev3b.yrl", 735).
yeccpars2_206_([__1 | Stack]) ->
 [begin
   digitMapToken
  end | Stack].

-compile({inline,{yeccpars2_207_,1}}).
-file("megaco_text_parser_prev3b.yrl", 744).
yeccpars2_207_([__1 | Stack]) ->
 [begin
   eventBufferToken
  end | Stack].

-compile({inline,{yeccpars2_208_,1}}).
-file("megaco_text_parser_prev3b.yrl", 745).
yeccpars2_208_([__1 | Stack]) ->
 [begin
   eventsToken
  end | Stack].

-compile({inline,{yeccpars2_209_,1}}).
-file("megaco_text_parser_prev3b.yrl", 734).
yeccpars2_209_([__1 | Stack]) ->
 [begin
   mediaToken
  end | Stack].

-compile({inline,{yeccpars2_210_,1}}).
-file("megaco_text_parser_prev3b.yrl", 733).
yeccpars2_210_([__1 | Stack]) ->
 [begin
   modemToken
  end | Stack].

-compile({inline,{yeccpars2_211_,1}}).
-file("megaco_text_parser_prev3b.yrl", 732).
yeccpars2_211_([__1 | Stack]) ->
 [begin
   muxToken
  end | Stack].

-compile({inline,{yeccpars2_212_,1}}).
-file("megaco_text_parser_prev3b.yrl", 737).
yeccpars2_212_([__1 | Stack]) ->
 [begin
   observedEventsToken
  end | Stack].

-compile({inline,{yeccpars2_213_,1}}).
-file("megaco_text_parser_prev3b.yrl", 738).
yeccpars2_213_([__1 | Stack]) ->
 [begin
   packagesToken
  end | Stack].

-compile({inline,{yeccpars2_214_,1}}).
-file("megaco_text_parser_prev3b.yrl", 743).
yeccpars2_214_([__1 | Stack]) ->
 [begin
   signalsToken
  end | Stack].

-compile({inline,{yeccpars2_215_,1}}).
-file("megaco_text_parser_prev3b.yrl", 736).
yeccpars2_215_([__1 | Stack]) ->
 [begin
   statsToken
  end | Stack].

-compile({inline,{yeccpars2_217_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1106).
yeccpars2_217_([__1 | Stack]) ->
 [begin
   ensure_pkgdName ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_219_,1}}).
-file("megaco_text_parser_prev3b.yrl", 870).
yeccpars2_219_([__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'IndAudStatisticsDescriptor' { statName = __3 }
  end | Stack].

-compile({inline,{yeccpars2_220_,1}}).
-file("megaco_text_parser_prev3b.yrl", 849).
yeccpars2_220_([__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_222_,1}}).
-file("megaco_text_parser_prev3b.yrl", 856).
yeccpars2_222_([__1 | Stack]) ->
 [begin
   { signal , ensure_indAudSignal ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_223_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1199).
yeccpars2_223_([__1 | Stack]) ->
 [begin
   merge_signalRequest ( __1 , [ ] )
  end | Stack].

-compile({inline,{yeccpars2_226_,1}}).
-file("megaco_text_parser_prev3b.yrl", 855).
yeccpars2_226_([__1 | Stack]) ->
 [begin
   { seqSigList , __1 }
  end | Stack].

-compile({inline,{yeccpars2_227_,1}}).
-file("megaco_text_parser_prev3b.yrl", 852).
yeccpars2_227_([__2,__1 | Stack]) ->
 [begin
   asn1_NOVALUE
  end | Stack].

-compile({inline,{yeccpars2_228_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1533).
yeccpars2_228_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_231_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1263).
yeccpars2_231_([__1 | Stack]) ->
 [begin
   ensure_uint16 ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_235_,1}}).
-file("megaco_text_parser_prev3b.yrl", 860).
yeccpars2_235_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'IndAudSeqSigList' { id = ensure_uint16 ( __3 ) ,
    signalList =
    ensure_indAudSignalListParm ( __5 ) }
  end | Stack].

-compile({inline,{yeccpars2_236_,1}}).
-file("megaco_text_parser_prev3b.yrl", 853).
yeccpars2_236_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_238_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1202).
yeccpars2_238_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_240_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1457).
yeccpars2_240_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_241_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1461).
yeccpars2_241_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_242_COMMA,1}}).
-file("megaco_text_parser_prev3b.yrl", 1232).
yeccpars2_242_COMMA([__1 | Stack]) ->
 [begin
   keepActive
  end | Stack].

-compile({inline,{yeccpars2_242_RBRKT,1}}).
-file("megaco_text_parser_prev3b.yrl", 1232).
yeccpars2_242_RBRKT([__1 | Stack]) ->
 [begin
   keepActive
  end | Stack].

-compile({inline,{yeccpars2_242_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1484).
yeccpars2_242_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_243_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1502).
yeccpars2_243_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_244_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1519).
yeccpars2_244_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_245_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1535).
yeccpars2_245_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_246_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1537).
yeccpars2_246_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_248_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1224).
yeccpars2_248_([__3,__2,__1 | Stack]) ->
 [begin
   { stream , __3 }
  end | Stack].

-compile({inline,{yeccpars2_249_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1104).
yeccpars2_249_([__1 | Stack]) ->
 [begin
   ensure_streamID ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_251_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1226).
yeccpars2_251_([__3,__2,__1 | Stack]) ->
 [begin
   { signal_type , __3 }
  end | Stack].

-compile({inline,{yeccpars2_252_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1240).
yeccpars2_252_([__1 | Stack]) ->
 [begin
   brief
  end | Stack].

-compile({inline,{yeccpars2_253_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1238).
yeccpars2_253_([__1 | Stack]) ->
 [begin
   onOff
  end | Stack].

-compile({inline,{yeccpars2_254_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1239).
yeccpars2_254_([__1 | Stack]) ->
 [begin
   timeOut
  end | Stack].

-compile({inline,{yeccpars2_256_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1297).
yeccpars2_256_([__1 | Stack]) ->
 [begin
   ensure_requestID ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_257_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1234).
yeccpars2_257_([__3,__2,__1 | Stack]) ->
 [begin
   { requestId , __3 }
  end | Stack].

-compile({inline,{yeccpars2_260_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1247).
yeccpars2_260_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_261_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1250).
yeccpars2_261_([__1 | Stack]) ->
 [begin
   onInterruptByEvent
  end | Stack].

-compile({inline,{yeccpars2_262_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1251).
yeccpars2_262_([__1 | Stack]) ->
 [begin
   onInterruptByNewSignalDescr
  end | Stack].

-compile({inline,{yeccpars2_263_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1252).
yeccpars2_263_([__1 | Stack]) ->
 [begin
   otherReason
  end | Stack].

-compile({inline,{yeccpars2_264_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1249).
yeccpars2_264_([__1 | Stack]) ->
 [begin
   onTimeOut
  end | Stack].

-compile({inline,{yeccpars2_267_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1247).
yeccpars2_267_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_268_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1246).
yeccpars2_268_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_269_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1231).
yeccpars2_269_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { notify_completion , [ __4 | __5 ] }
  end | Stack].

-compile({inline,{yeccpars2_271_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1228).
yeccpars2_271_([__3,__2,__1 | Stack]) ->
 [begin
   { duration , ensure_uint16 ( __3 ) }
  end | Stack].

-compile({inline,{yeccpars2_273_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1233).
yeccpars2_273_([__3,__2,__1 | Stack]) ->
 [begin
   { direction , __3 }
  end | Stack].

-compile({inline,{yeccpars2_274_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1244).
yeccpars2_274_([__1 | Stack]) ->
 [begin
   both
  end | Stack].

-compile({inline,{yeccpars2_275_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1242).
yeccpars2_275_([__1 | Stack]) ->
 [begin
   external
  end | Stack].

-compile({inline,{yeccpars2_276_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1243).
yeccpars2_276_([__1 | Stack]) ->
 [begin
   internal
  end | Stack].

-compile({inline,{yeccpars2_277_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1236).
yeccpars2_277_([__2,__1 | Stack]) ->
 [begin
   { other , ensure_NAME ( __1 ) , __2 }
  end | Stack].

-compile({inline,{yeccpars2_282_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1040).
yeccpars2_282_([__2,__1 | Stack]) ->
 [begin
   # 'PropertyParm' { value = [ __2 ] ,
    extraInfo = { relation , unequalTo } }
  end | Stack].

-compile({inline,{yeccpars2_283_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1438).
yeccpars2_283_([__1 | Stack]) ->
 [begin
   ensure_value ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_284_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1437).
yeccpars2_284_([__1 | Stack]) ->
 [begin
   ensure_value ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_285_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1043).
yeccpars2_285_([__2,__1 | Stack]) ->
 [begin
   # 'PropertyParm' { value = [ __2 ] ,
    extraInfo = { relation , smallerThan } }
  end | Stack].

-compile({inline,{yeccpars2_286_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1046).
yeccpars2_286_([__2,__1 | Stack]) ->
 [begin
   # 'PropertyParm' { value = [ __2 ] ,
    extraInfo = { relation , greaterThan } }
  end | Stack].

-compile({inline,{yeccpars2_287_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1067).
yeccpars2_287_([__1 | Stack]) ->
 [begin
   # 'PropertyParm' { value = [ __1 ] }
  end | Stack].

-compile({inline,{yeccpars2_288_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1037).
yeccpars2_288_([__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_291_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1070).
yeccpars2_291_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_295_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1070).
yeccpars2_295_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_296_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1069).
yeccpars2_296_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_298_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1059).
yeccpars2_298_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'PropertyParm' { value = [ __2 , __4 ] ,
    extraInfo = { range , true } }
  end | Stack].

-compile({inline,{yeccpars2_299_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1063).
yeccpars2_299_([__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'PropertyParm' { value = [ __2 | __3 ] ,
    extraInfo = { sublist , true } }
  end | Stack].

-compile({inline,{yeccpars2_300_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1070).
yeccpars2_300_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_302_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1055).
yeccpars2_302_([__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'PropertyParm' { value = [ __2 | __3 ] ,
    extraInfo = { sublist , false } }
  end | Stack].

-compile({inline,{yeccpars2_305_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1202).
yeccpars2_305_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_306_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1201).
yeccpars2_306_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_307_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1198).
yeccpars2_307_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_signalRequest ( __1 , [ __3 | __4 ] )
  end | Stack].

-compile({inline,{yeccpars2_309_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1389).
yeccpars2_309_([__1 | Stack]) ->
 [begin
   ensure_packagesItem ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_311_,1}}).
-file("megaco_text_parser_prev3b.yrl", 873).
yeccpars2_311_([__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_indAudPackagesDescriptor ( __3 )
  end | Stack].

-compile({inline,{yeccpars2_313_,1}}).
-file("megaco_text_parser_prev3b.yrl", 788).
yeccpars2_313_([__1 | Stack]) ->
 [begin
   { termStateDescr , __1 }
  end | Stack].

-compile({inline,{yeccpars2_314_,1}}).
-file("megaco_text_parser_prev3b.yrl", 786).
yeccpars2_314_([__1 | Stack]) ->
 [begin
   { streamParm , __1 }
  end | Stack].

-compile({inline,{yeccpars2_315_,1}}).
-file("megaco_text_parser_prev3b.yrl", 787).
yeccpars2_315_([__1 | Stack]) ->
 [begin
   { streamDescr , __1 }
  end | Stack].

-compile({inline,{yeccpars2_316_,1}}).
-file("megaco_text_parser_prev3b.yrl", 797).
yeccpars2_316_([__1 | Stack]) ->
 [begin
   # 'IndAudStreamParms' { statisticsDescriptor = __1 }
  end | Stack].

-compile({inline,{yeccpars2_317_,1}}).
-file("megaco_text_parser_prev3b.yrl", 791).
yeccpars2_317_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_318_,1}}).
-file("megaco_text_parser_prev3b.yrl", 795).
yeccpars2_318_([__1 | Stack]) ->
 [begin
   # 'IndAudStreamParms' { localControlDescriptor = __1 }
  end | Stack].

-compile({inline,{yeccpars2_324_,1}}).
-file("megaco_text_parser_prev3b.yrl", 825).
yeccpars2_324_([__1 | Stack]) ->
 [begin
   ensure_indAudTerminationStateParm ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_326_,1}}).
-file("megaco_text_parser_prev3b.yrl", 819).
yeccpars2_326_([__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_indAudTerminationStateDescriptor ( __3 )
  end | Stack].

-compile({inline,{yeccpars2_331_,1}}).
-file("megaco_text_parser_prev3b.yrl", 801).
yeccpars2_331_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'IndAudStreamDescriptor' { streamID = __3 ,
    streamParms = __5 }
  end | Stack].

-compile({inline,{yeccpars2_333_,1}}).
-file("megaco_text_parser_prev3b.yrl", 814).
yeccpars2_333_([__1 | Stack]) ->
 [begin
   ensure_indAudLocalParm ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_334_,1}}).
-file("megaco_text_parser_prev3b.yrl", 810).
yeccpars2_334_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_337_,1}}).
-file("megaco_text_parser_prev3b.yrl", 810).
yeccpars2_337_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_338_,1}}).
-file("megaco_text_parser_prev3b.yrl", 809).
yeccpars2_338_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_339_,1}}).
-file("megaco_text_parser_prev3b.yrl", 807).
yeccpars2_339_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_indAudLocalControlDescriptor ( [ __3 | __4 ] )
  end | Stack].

-compile({inline,{yeccpars2_342_,1}}).
-file("megaco_text_parser_prev3b.yrl", 791).
yeccpars2_342_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_343_,1}}).
-file("megaco_text_parser_prev3b.yrl", 790).
yeccpars2_343_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_344_,1}}).
-file("megaco_text_parser_prev3b.yrl", 780).
yeccpars2_344_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_indAudMediaDescriptor ( [ __3 | __4 ] )
  end | Stack].

-compile({inline,{yeccpars2_350_,1}}).
-file("megaco_text_parser_prev3b.yrl", 843).
yeccpars2_350_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'IndAudEventsDescriptor' { requestID = __3 ,
    pkgdName = __5 }
  end | Stack].

-compile({inline,{yeccpars2_352_,1}}).
-file("megaco_text_parser_prev3b.yrl", 835).
yeccpars2_352_(Stack) ->
 [begin
   asn1_NOVALUE
  end | Stack].

-compile({inline,{yeccpars2_354_,1}}).
-file("megaco_text_parser_prev3b.yrl", 828).
yeccpars2_354_([__4,__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_355_,1}}).
-file("megaco_text_parser_prev3b.yrl", 831).
yeccpars2_355_([__2,__1 | Stack]) ->
 [begin
   merge_indAudEventBufferDescriptor ( __1 , __2 )
  end | Stack].

-compile({inline,{yeccpars2_357_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1180).
yeccpars2_357_([__1 | Stack]) ->
 [begin
   ensure_NAME ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_359_,1}}).
-file("megaco_text_parser_prev3b.yrl", 838).
yeccpars2_359_([__1 | Stack]) ->
 [begin
   { streamID , __1 }
  end | Stack].

-compile({inline,{yeccpars2_360_,1}}).
-file("megaco_text_parser_prev3b.yrl", 839).
yeccpars2_360_([__1 | Stack]) ->
 [begin
   { eventParameterName , __1 }
  end | Stack].

-compile({inline,{yeccpars2_361_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1537).
yeccpars2_361_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_363_,1}}).
-file("megaco_text_parser_prev3b.yrl", 875).
yeccpars2_363_([__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_364_,1}}).
-file("megaco_text_parser_prev3b.yrl", 834).
yeccpars2_364_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_365_,1}}).
-file("megaco_text_parser_prev3b.yrl", 722).
yeccpars2_365_([__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_auditDescriptor ( __3 )
  end | Stack].

-compile({inline,{yeccpars2_366_,1}}).
-file("megaco_text_parser_prev3b.yrl", 724).
yeccpars2_366_([__2,__1 | Stack]) ->
 [begin
   [ __1 | __2 ]
  end | Stack].

-compile({inline,{yeccpars2_368_,1}}).
-file("megaco_text_parser_prev3b.yrl", 728).
yeccpars2_368_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_369_,1}}).
-file("megaco_text_parser_prev3b.yrl", 727).
yeccpars2_369_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_370_,1}}).
-file("megaco_text_parser_prev3b.yrl", 755).
yeccpars2_370_([__2,__1 | Stack]) ->
 [begin
   [ __1 | __2 ]
  end | Stack].

-compile({inline,{yeccpars2_372_,1}}).
-file("megaco_text_parser_prev3b.yrl", 760).
yeccpars2_372_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_378_,1}}).
-file("megaco_text_parser_prev3b.yrl", 759).
yeccpars2_378_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_379_,1}}).
-file("megaco_text_parser_prev3b.yrl", 664).
yeccpars2_379_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_386_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1339).
yeccpars2_386_([__1 | Stack]) ->
 [begin
   { time_stamp , __1 }
  end | Stack].

-compile({inline,{yeccpars2_387_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1341).
yeccpars2_387_([__1 | Stack]) ->
 [begin
   { version , __1 }
  end | Stack].

-compile({inline,{yeccpars2_388_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1334).
yeccpars2_388_([__1 | Stack]) ->
 [begin
   { reason , __1 }
  end | Stack].

-compile({inline,{yeccpars2_389_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1337).
yeccpars2_389_([__1 | Stack]) ->
 [begin
   { profile , __1 }
  end | Stack].

-compile({inline,{yeccpars2_390_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1331).
yeccpars2_390_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_391_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1340).
yeccpars2_391_([__1 | Stack]) ->
 [begin
   { mgc_id , __1 }
  end | Stack].

-compile({inline,{yeccpars2_392_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1333).
yeccpars2_392_([__1 | Stack]) ->
 [begin
   { method , __1 }
  end | Stack].

-compile({inline,{yeccpars2_393_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1335).
yeccpars2_393_([__1 | Stack]) ->
 [begin
   { delay , __1 }
  end | Stack].

-compile({inline,{yeccpars2_394_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1336).
yeccpars2_394_([__1 | Stack]) ->
 [begin
   { address , __1 }
  end | Stack].

-compile({inline,{yeccpars2_395_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1435).
yeccpars2_395_([__1 | Stack]) ->
 [begin
   ensure_extensionParameter ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_397_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1338).
yeccpars2_397_([__1 | Stack]) ->
 [begin
   { extension , __1 }
  end | Stack].

-compile({inline,{yeccpars2_398_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1343).
yeccpars2_398_([__1 | Stack]) ->
 [begin
   { audit_item , __1 }
  end | Stack].

-compile({inline,{yeccpars2_399_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1460).
yeccpars2_399_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_400_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1492).
yeccpars2_400_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_401_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1493).
yeccpars2_401_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_402_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1514).
yeccpars2_402_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_403_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1515).
yeccpars2_403_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_404_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1532).
yeccpars2_404_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_405_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1342).
yeccpars2_405_([__1 | Stack]) ->
 [begin
   incomplete
  end | Stack].

-compile({inline,{yeccpars2_406_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1391).
yeccpars2_406_([__1 | Stack]) ->
 [begin
   ensure_timeStamp ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_407_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1554).
yeccpars2_407_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_409_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1360).
yeccpars2_409_([__3,__2,__1 | Stack]) ->
 [begin
   ensure_version ( __3 )
  end | Stack].

-compile({inline,{yeccpars2_410_,1}}).
-file("megaco_text_parser_prev3b.yrl", 462).
yeccpars2_410_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_411_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1354).
yeccpars2_411_([__3,__2,__1 | Stack]) ->
 [begin
   { portNumber , __3 }
  end | Stack].

-compile({inline,{yeccpars2_412_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1352).
yeccpars2_412_([__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_414_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1348).
yeccpars2_414_([__3,__2,__1 | Stack]) ->
 [begin
   [ __3 ]
  end | Stack].

-compile({inline,{yeccpars2_416_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1358).
yeccpars2_416_([__3,__2,__1 | Stack]) ->
 [begin
   ensure_profile ( __3 )
  end | Stack].

-compile({inline,{yeccpars2_417_,1}}).
-file("megaco_text_parser_prev3b.yrl", 462).
yeccpars2_417_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_418_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1356).
yeccpars2_418_([__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_420_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1346).
yeccpars2_420_([__3,__2,__1 | Stack]) ->
 [begin
   ensure_serviceChangeMethod ( __3 )
  end | Stack].

-compile({inline,{yeccpars2_422_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1350).
yeccpars2_422_([__3,__2,__1 | Stack]) ->
 [begin
   ensure_uint32 ( __3 )
  end | Stack].

-compile({inline,{yeccpars2_423_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1363).
yeccpars2_423_([__2,__1 | Stack]) ->
 [begin
   setelement ( # 'PropertyParm' .name , __2 , __1 )
  end | Stack].

-compile({inline,{yeccpars2_426_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1331).
yeccpars2_426_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_427_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1330).
yeccpars2_427_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_428_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1327).
yeccpars2_428_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_ServiceChangeParm ( [ __3 | __4 ] )
  end | Stack].

-compile({inline,{yeccpars2_429_,1}}).
-file("megaco_text_parser_prev3b.yrl", 901).
yeccpars2_429_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   make_commandRequest ( { serviceChangeReq , __1 } ,
    # 'ServiceChangeRequest' { terminationID = [ __3 ] ,
    serviceChangeParms = __5 } )
  end | Stack].

-compile({inline,{yeccpars2_431_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1433).
yeccpars2_431_([__3,__2,__1 | Stack]) ->
 [begin
   ensure_uint16 ( __3 )
  end | Stack].

-compile({inline,{yeccpars2_435_,1}}).
-file("megaco_text_parser_prev3b.yrl", 887).
yeccpars2_435_([__1 | Stack]) ->
 [begin
   # 'NotifyRequest' { observedEventsDescriptor = __1 }
  end | Stack].

-compile({inline,{yeccpars2_437_,1}}).
-file("megaco_text_parser_prev3b.yrl", 889).
yeccpars2_437_([__1 | Stack]) ->
 [begin
   # 'NotifyRequest' { errorDescriptor = __1 }
  end | Stack].

-compile({inline,{yeccpars2_441_,1}}).
-file("megaco_text_parser_prev3b.yrl", 462).
yeccpars2_441_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_442_,1}}).
-file("megaco_text_parser_prev3b.yrl", 462).
yeccpars2_442_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_444_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1277).
yeccpars2_444_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_446_,1}}).
-file("megaco_text_parser_prev3b.yrl", 462).
yeccpars2_446_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_447_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1277).
yeccpars2_447_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_448_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1276).
yeccpars2_448_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_449_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1273).
yeccpars2_449_([__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'ObservedEventsDescriptor' { requestId = __3 ,
    observedEventLst = [ __5 | __6 ] }
  end | Stack].

-compile({inline,{yeccpars2_450_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1289).
yeccpars2_450_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_451_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1284).
yeccpars2_451_([__3,__2,__1 | Stack]) ->
 [begin
   merge_observed_event ( __3 , __2 , asn1_NOVALUE )
  end | Stack].

-compile({inline,{yeccpars2_453_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1292).
yeccpars2_453_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_456_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1178).
yeccpars2_456_([__2,__1 | Stack]) ->
 [begin
   select_stream_or_other ( __1 , __2 )
  end | Stack].

-compile({inline,{yeccpars2_459_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1292).
yeccpars2_459_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_460_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1291).
yeccpars2_460_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_461_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1288).
yeccpars2_461_([__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_463_,1}}).
-file("megaco_text_parser_prev3b.yrl", 462).
yeccpars2_463_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_465_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1289).
yeccpars2_465_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_466_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1282).
yeccpars2_466_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_observed_event ( __6 , __5 , __1 )
  end | Stack].

-compile({inline,{yeccpars2_467_,1}}).
-file("megaco_text_parser_prev3b.yrl", 883).
yeccpars2_467_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   make_commandRequest ( { notifyReq , __1 } ,
    setelement ( # 'NotifyRequest' .terminationID , __5 , [ __3 ] ) )
  end | Stack].

-compile({inline,{yeccpars2_469_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1431).
yeccpars2_469_([__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_470_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1024).
yeccpars2_470_([__1 | Stack]) ->
 [begin
   false
  end | Stack].

-compile({inline,{yeccpars2_471_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1023).
yeccpars2_471_([__1 | Stack]) ->
 [begin
   true
  end | Stack].

-compile({inline,{yeccpars2_476_,1}}).
-file("megaco_text_parser_prev3b.yrl", 567).
yeccpars2_476_([__1 | Stack]) ->
 [begin
   { prop , __1 }
  end | Stack].

-compile({inline,{yeccpars2_477_,1}}).
-file("megaco_text_parser_prev3b.yrl", 560).
yeccpars2_477_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_478_,1}}).
-file("megaco_text_parser_prev3b.yrl", 564).
yeccpars2_478_([__1 | Stack]) ->
 [begin
   emergencyAudit
  end | Stack].

-compile({inline,{yeccpars2_479_,1}}).
-file("megaco_text_parser_prev3b.yrl", 566).
yeccpars2_479_([__1 | Stack]) ->
 [begin
   iepsCallind
  end | Stack].

-compile({inline,{yeccpars2_480_,1}}).
-file("megaco_text_parser_prev3b.yrl", 565).
yeccpars2_480_([__1 | Stack]) ->
 [begin
   priorityAudit
  end | Stack].

-compile({inline,{yeccpars2_481_,1}}).
-file("megaco_text_parser_prev3b.yrl", 563).
yeccpars2_481_([__1 | Stack]) ->
 [begin
   topologyAudit
  end | Stack].

-compile({inline,{yeccpars2_484_,1}}).
-file("megaco_text_parser_prev3b.yrl", 560).
yeccpars2_484_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_485_,1}}).
-file("megaco_text_parser_prev3b.yrl", 559).
yeccpars2_485_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_486_,1}}).
-file("megaco_text_parser_prev3b.yrl", 556).
yeccpars2_486_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __3 | __4 ]
  end | Stack].

-compile({inline,{yeccpars2_487_,1}}).
-file("megaco_text_parser_prev3b.yrl", 550).
yeccpars2_487_([__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_context_attr_audit_request (
    # 'ContextAttrAuditRequest' { } , __3 )
  end | Stack].

-compile({inline,{yeccpars2_490_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1313).
yeccpars2_490_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_496_,1}}).
-file("megaco_text_parser_prev3b.yrl", 547).
yeccpars2_496_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_499_,1}}).
-file("megaco_text_parser_prev3b.yrl", 547).
yeccpars2_499_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_500_,1}}).
-file("megaco_text_parser_prev3b.yrl", 546).
yeccpars2_500_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_501_,1}}).
-file("megaco_text_parser_prev3b.yrl", 544).
yeccpars2_501_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __4 | __5 ]
  end | Stack].

-compile({inline,{yeccpars2_502_,1}}).
-file("megaco_text_parser_prev3b.yrl", 541).
yeccpars2_502_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { contextList , __3 }
  end | Stack].

-compile({inline,{yeccpars2_503_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1034).
yeccpars2_503_([__2,__1 | Stack]) ->
 [begin
   setelement ( # 'PropertyParm' .name , __2 , __1 )
  end | Stack].

-compile({inline,{yeccpars2_504_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1311).
yeccpars2_504_([__2,__1 | Stack]) ->
 [begin
   [ __1 | __2 ]
  end | Stack].

-compile({inline,{yeccpars2_506_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1313).
yeccpars2_506_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_507_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1312).
yeccpars2_507_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_508_,1}}).
-file("megaco_text_parser_prev3b.yrl", 539).
yeccpars2_508_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { contextProp , __3 }
  end | Stack].

-compile({inline,{yeccpars2_510_,1}}).
-file("megaco_text_parser_prev3b.yrl", 665).
yeccpars2_510_(Stack) ->
 [begin
   asn1_NOVALUE
  end | Stack].

-compile({inline,{yeccpars2_511_,1}}).
-file("megaco_text_parser_prev3b.yrl", 669).
yeccpars2_511_([__4,__3,__2,__1 | Stack]) ->
 [begin
   make_commandRequest ( { auditValueRequest , __1 } ,
    # 'AuditRequest' { terminationID = __3 ,
    auditDescriptor = __4 } )
  end | Stack].

-compile({inline,{yeccpars2_513_,1}}).
-file("megaco_text_parser_prev3b.yrl", 665).
yeccpars2_513_(Stack) ->
 [begin
   asn1_NOVALUE
  end | Stack].

-compile({inline,{yeccpars2_514_,1}}).
-file("megaco_text_parser_prev3b.yrl", 674).
yeccpars2_514_([__4,__3,__2,__1 | Stack]) ->
 [begin
   make_commandRequest ( { auditCapRequest , __1 } ,
    # 'AuditRequest' { terminationID = __3 ,
    auditDescriptor = __4 } )
  end | Stack].

-compile({inline,{yeccpars2_515_,1}}).
-file("megaco_text_parser_prev3b.yrl", 511).
yeccpars2_515_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_action_request ( __3 , __5 )
  end | Stack].

-compile({inline,{yeccpars2_516_,1}}).
-file("megaco_text_parser_prev3b.yrl", 513).
yeccpars2_516_([__2,__1 | Stack]) ->
 [begin
   [ __1 | __2 ]
  end | Stack].

-compile({inline,{yeccpars2_518_,1}}).
-file("megaco_text_parser_prev3b.yrl", 517).
yeccpars2_518_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_519_,1}}).
-file("megaco_text_parser_prev3b.yrl", 516).
yeccpars2_519_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_521_,1}}).
-file("megaco_text_parser_prev3b.yrl", 629).
yeccpars2_521_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_522_,1}}).
-file("megaco_text_parser_prev3b.yrl", 619).
yeccpars2_522_([__4,__3,__2,__1 | Stack]) ->
 [begin
   Descs = merge_AmmRequest_descriptors ( __4 , [ ] ) ,
    make_commandRequest ( __1 ,
    # 'AmmRequest' { terminationID = [ __3 ] ,
    descriptors = Descs } )
  end | Stack].

-compile({inline,{yeccpars2_524_,1}}).
-file("megaco_text_parser_prev3b.yrl", 643).
yeccpars2_524_([__1 | Stack]) ->
 [begin
   { statisticsDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_525_,1}}).
-file("megaco_text_parser_prev3b.yrl", 640).
yeccpars2_525_([__1 | Stack]) ->
 [begin
   { signalsDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_526_,1}}).
-file("megaco_text_parser_prev3b.yrl", 637).
yeccpars2_526_([__1 | Stack]) ->
 [begin
   { muxDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_527_,1}}).
-file("megaco_text_parser_prev3b.yrl", 636).
yeccpars2_527_([__1 | Stack]) ->
 [begin
   { modemDescriptor , deprecated }
  end | Stack].

-compile({inline,{yeccpars2_528_,1}}).
-file("megaco_text_parser_prev3b.yrl", 635).
yeccpars2_528_([__1 | Stack]) ->
 [begin
   { mediaDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_529_,1}}).
-file("megaco_text_parser_prev3b.yrl", 638).
yeccpars2_529_([__1 | Stack]) ->
 [begin
   { eventsDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_530_,1}}).
-file("megaco_text_parser_prev3b.yrl", 639).
yeccpars2_530_([__1 | Stack]) ->
 [begin
   { eventBufferDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_531_,1}}).
-file("megaco_text_parser_prev3b.yrl", 641).
yeccpars2_531_([__1 | Stack]) ->
 [begin
   { digitMapDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_532_,1}}).
-file("megaco_text_parser_prev3b.yrl", 642).
yeccpars2_532_([__1 | Stack]) ->
 [begin
   { auditDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_533_,1}}).
-file("megaco_text_parser_prev3b.yrl", 632).
yeccpars2_533_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_534_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1319).
yeccpars2_534_([__1 | Stack]) ->
 [begin
   ensure_DMD ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_535_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1073).
yeccpars2_535_([__1 | Stack]) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_536_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1109).
yeccpars2_536_([__1 | Stack]) ->
 [begin
   # 'EventsDescriptor' { requestID = asn1_NOVALUE ,
    eventList = [ ] }
  end | Stack].

-compile({inline,{yeccpars2_540_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1189).
yeccpars2_540_([__1 | Stack]) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_543_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1399).
yeccpars2_543_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_544_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1403).
yeccpars2_544_([__1 | Stack]) ->
 [begin
   # 'StatisticsParameter' { statName = __1 ,
    statValue = asn1_NOVALUE }
  end | Stack].

-compile({inline,{yeccpars2_546_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1406).
yeccpars2_546_([__3,__2,__1 | Stack]) ->
 [begin
   # 'StatisticsParameter' { statName = __1 ,
    statValue = [ __3 ] }
  end | Stack].

-compile({inline,{yeccpars2_549_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1399).
yeccpars2_549_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_550_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1398).
yeccpars2_550_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_551_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1396).
yeccpars2_551_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __3 | __4 ]
  end | Stack].

-compile({inline,{yeccpars2_553_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1195).
yeccpars2_553_([__1 | Stack]) ->
 [begin
   { signal , __1 }
  end | Stack].

-compile({inline,{yeccpars2_554_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1192).
yeccpars2_554_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_555_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1194).
yeccpars2_555_([__1 | Stack]) ->
 [begin
   { seqSigList , __1 }
  end | Stack].

-compile({inline,{yeccpars2_556_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1533).
yeccpars2_556_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_560_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1261).
yeccpars2_560_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_563_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1261).
yeccpars2_563_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_564_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1260).
yeccpars2_564_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_565_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1256).
yeccpars2_565_([__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'SeqSigList' { id = ensure_uint16 ( __3 ) ,
    signalList = [ __5 | __6 ] }
  end | Stack].

-compile({inline,{yeccpars2_568_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1192).
yeccpars2_568_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_569_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1191).
yeccpars2_569_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_570_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1188).
yeccpars2_570_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __3 | __4 ]
  end | Stack].

-compile({inline,{yeccpars2_572_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1102).
yeccpars2_572_([__1 | Stack]) ->
 [begin
   ensure_muxType ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_574_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1099).
yeccpars2_574_([__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'MuxDescriptor' { muxType = __3 ,
    termList = __4 }
  end | Stack].

-compile({inline,{yeccpars2_576_,1}}).
-file("megaco_text_parser_prev3b.yrl", 963).
yeccpars2_576_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_579_,1}}).
-file("megaco_text_parser_prev3b.yrl", 963).
yeccpars2_579_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_580_,1}}).
-file("megaco_text_parser_prev3b.yrl", 962).
yeccpars2_580_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_581_,1}}).
-file("megaco_text_parser_prev3b.yrl", 959).
yeccpars2_581_([__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_584_,1}}).
-file("megaco_text_parser_prev3b.yrl", 0).
yeccpars2_584_([__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_585_,1}}).
-file("megaco_text_parser_prev3b.yrl", 0).
yeccpars2_585_(Stack) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_588_,1}}).
-file("megaco_text_parser_prev3b.yrl", 0).
yeccpars2_588_(Stack) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_589_,1}}).
-file("megaco_text_parser_prev3b.yrl", 0).
yeccpars2_589_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_590_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1309).
yeccpars2_590_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_591_,1}}).
-file("megaco_text_parser_prev3b.yrl", 0).
yeccpars2_591_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_593_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1313).
yeccpars2_593_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_595_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1308).
yeccpars2_595_([__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_596_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1309).
yeccpars2_596_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_597_,1}}).
-file("megaco_text_parser_prev3b.yrl", 0).
yeccpars2_597_([__4,__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_599_,1}}).
-file("megaco_text_parser_prev3b.yrl", 984).
yeccpars2_599_([__1 | Stack]) ->
 [begin
   { termState , __1 }
  end | Stack].

-compile({inline,{yeccpars2_600_,1}}).
-file("megaco_text_parser_prev3b.yrl", 980).
yeccpars2_600_([__1 | Stack]) ->
 [begin
   { streamParm , __1 }
  end | Stack].

-compile({inline,{yeccpars2_601_,1}}).
-file("megaco_text_parser_prev3b.yrl", 982).
yeccpars2_601_([__1 | Stack]) ->
 [begin
   { streamDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_602_,1}}).
-file("megaco_text_parser_prev3b.yrl", 993).
yeccpars2_602_([__1 | Stack]) ->
 [begin
   { statistics , __1 }
  end | Stack].

-compile({inline,{yeccpars2_603_,1}}).
-file("megaco_text_parser_prev3b.yrl", 974).
yeccpars2_603_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_604_,1}}).
-file("megaco_text_parser_prev3b.yrl", 992).
yeccpars2_604_([__1 | Stack]) ->
 [begin
   { control , __1 }
  end | Stack].

-compile({inline,{yeccpars2_606_,1}}).
-file("megaco_text_parser_prev3b.yrl", 989).
yeccpars2_606_([__1 | Stack]) ->
 [begin
   { local , # 'LocalRemoteDescriptor' { propGrps = ensure_prop_groups ( __1 ) } }
  end | Stack].

-compile({inline,{yeccpars2_607_,1}}).
-file("megaco_text_parser_prev3b.yrl", 991).
yeccpars2_607_([__1 | Stack]) ->
 [begin
   { remote , # 'LocalRemoteDescriptor' { propGrps = ensure_prop_groups ( __1 ) } }
  end | Stack].

-compile({inline,{yeccpars2_611_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1015).
yeccpars2_611_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_612_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1083).
yeccpars2_612_([__1 | Stack]) ->
 [begin
   { serviceState , __1 }
  end | Stack].

-compile({inline,{yeccpars2_613_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1085).
yeccpars2_613_([__1 | Stack]) ->
 [begin
   { propertyParm , __1 }
  end | Stack].

-compile({inline,{yeccpars2_614_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1084).
yeccpars2_614_([__1 | Stack]) ->
 [begin
   { eventBufferControl , __1 }
  end | Stack].

-compile({inline,{yeccpars2_615_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1449).
yeccpars2_615_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_616_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1529).
yeccpars2_616_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_618_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1087).
yeccpars2_618_([__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_619_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1091).
yeccpars2_619_([__1 | Stack]) ->
 [begin
   inSvc
  end | Stack].

-compile({inline,{yeccpars2_620_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1090).
yeccpars2_620_([__1 | Stack]) ->
 [begin
   outOfSvc
  end | Stack].

-compile({inline,{yeccpars2_621_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1089).
yeccpars2_621_([__1 | Stack]) ->
 [begin
   test
  end | Stack].

-compile({inline,{yeccpars2_623_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1093).
yeccpars2_623_([__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_624_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1096).
yeccpars2_624_([__1 | Stack]) ->
 [begin
   lockStep
  end | Stack].

-compile({inline,{yeccpars2_625_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1095).
yeccpars2_625_([__1 | Stack]) ->
 [begin
   off
  end | Stack].

-compile({inline,{yeccpars2_628_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1015).
yeccpars2_628_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_629_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1014).
yeccpars2_629_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_630_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1012).
yeccpars2_630_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_terminationStateDescriptor ( [ __3 | __4 ] )
  end | Stack].

-compile({inline,{yeccpars2_634_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1001).
yeccpars2_634_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_637_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1001).
yeccpars2_637_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_638_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1000).
yeccpars2_638_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_639_,1}}).
-file("megaco_text_parser_prev3b.yrl", 997).
yeccpars2_639_([__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'StreamDescriptor' { streamID = __3 ,
    streamParms = merge_streamParms ( [ __5 | __6 ] ) }
  end | Stack].

-compile({inline,{yeccpars2_641_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1021).
yeccpars2_641_([__1 | Stack]) ->
 [begin
   { prop , __1 }
  end | Stack].

-compile({inline,{yeccpars2_642_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1007).
yeccpars2_642_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_643_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1494).
yeccpars2_643_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_644_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1524).
yeccpars2_644_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_645_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1525).
yeccpars2_645_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_647_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1019).
yeccpars2_647_([__3,__2,__1 | Stack]) ->
 [begin
   { value , __3 }
  end | Stack].

-compile({inline,{yeccpars2_649_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1018).
yeccpars2_649_([__3,__2,__1 | Stack]) ->
 [begin
   { group , __3 }
  end | Stack].

-compile({inline,{yeccpars2_651_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1020).
yeccpars2_651_([__3,__2,__1 | Stack]) ->
 [begin
   { mode , __3 }
  end | Stack].

-compile({inline,{yeccpars2_652_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1030).
yeccpars2_652_([__1 | Stack]) ->
 [begin
   inactive
  end | Stack].

-compile({inline,{yeccpars2_653_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1031).
yeccpars2_653_([__1 | Stack]) ->
 [begin
   loopBack
  end | Stack].

-compile({inline,{yeccpars2_654_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1028).
yeccpars2_654_([__1 | Stack]) ->
 [begin
   recvOnly
  end | Stack].

-compile({inline,{yeccpars2_655_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1027).
yeccpars2_655_([__1 | Stack]) ->
 [begin
   sendOnly
  end | Stack].

-compile({inline,{yeccpars2_656_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1029).
yeccpars2_656_([__1 | Stack]) ->
 [begin
   sendRecv
  end | Stack].

-compile({inline,{yeccpars2_659_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1007).
yeccpars2_659_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_660_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1006).
yeccpars2_660_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_661_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1004).
yeccpars2_661_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __3 | __4 ]
  end | Stack].

-compile({inline,{yeccpars2_664_,1}}).
-file("megaco_text_parser_prev3b.yrl", 974).
yeccpars2_664_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_665_,1}}).
-file("megaco_text_parser_prev3b.yrl", 973).
yeccpars2_665_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_666_,1}}).
-file("megaco_text_parser_prev3b.yrl", 971).
yeccpars2_666_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_mediaDescriptor ( [ __3 | __4 ] )
  end | Stack].

-compile({inline,{yeccpars2_670_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1117).
yeccpars2_670_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_671_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1124).
yeccpars2_671_(Stack) ->
 [begin
   # 'RequestedEvent' { evParList = [ ] }
  end | Stack].

-compile({inline,{yeccpars2_672_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1120).
yeccpars2_672_([__2,__1 | Stack]) ->
 [begin
   setelement ( # 'RequestedEvent' .pkgdName , __2 , __1 )
  end | Stack].

-compile({inline,{yeccpars2_675_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1128).
yeccpars2_675_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_679_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1184).
yeccpars2_679_([__1 | Stack]) ->
 [begin
   ensure_eventDM ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_680_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1462).
yeccpars2_680_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_681_COMMA,1}}).
-file("megaco_text_parser_prev3b.yrl", 1131).
yeccpars2_681_COMMA([__1 | Stack]) ->
 [begin
   keepActive
  end | Stack].

-compile({inline,{yeccpars2_681_RBRKT,1}}).
-file("megaco_text_parser_prev3b.yrl", 1131).
yeccpars2_681_RBRKT([__1 | Stack]) ->
 [begin
   keepActive
  end | Stack].

-compile({inline,{yeccpars2_681_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1484).
yeccpars2_681_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_685_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1147).
yeccpars2_685_([__1 | Stack]) ->
 [begin
   # 'SecondEventsDescriptor' { requestID = asn1_NOVALUE ,
    eventList = [ ] }
  end | Stack].

-compile({inline,{yeccpars2_689_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1155).
yeccpars2_689_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_690_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1163).
yeccpars2_690_(Stack) ->
 [begin
   # 'SecondRequestedEvent' { evParList = [ ] }
  end | Stack].

-compile({inline,{yeccpars2_691_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1159).
yeccpars2_691_([__2,__1 | Stack]) ->
 [begin
   setelement ( # 'SecondRequestedEvent' .pkgdName , __2 , __1 )
  end | Stack].

-compile({inline,{yeccpars2_693_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1166).
yeccpars2_693_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_697_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1462).
yeccpars2_697_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_698_COMMA,1}}).
-file("megaco_text_parser_prev3b.yrl", 1169).
yeccpars2_698_COMMA([__1 | Stack]) ->
 [begin
   keepActive
  end | Stack].

-compile({inline,{yeccpars2_698_RBRKT,1}}).
-file("megaco_text_parser_prev3b.yrl", 1169).
yeccpars2_698_RBRKT([__1 | Stack]) ->
 [begin
   keepActive
  end | Stack].

-compile({inline,{yeccpars2_698_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1484).
yeccpars2_698_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_701_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1175).
yeccpars2_701_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { second_embed , __3 }
  end | Stack].

-compile({inline,{yeccpars2_704_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1166).
yeccpars2_704_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_705_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1165).
yeccpars2_705_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_706_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1162).
yeccpars2_706_([__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_secondEventParameters ( [ __2 | __3 ] )
  end | Stack].

-compile({inline,{yeccpars2_709_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1155).
yeccpars2_709_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_710_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1154).
yeccpars2_710_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_711_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1151).
yeccpars2_711_([__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'SecondEventsDescriptor' { requestID = __3 ,
    eventList = [ __5 | __6 ] }
  end | Stack].

-compile({inline,{yeccpars2_712_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1144).
yeccpars2_712_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { embed , asn1_NOVALUE , __3 }
  end | Stack].

-compile({inline,{yeccpars2_714_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1141).
yeccpars2_714_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { embed , __3 , asn1_NOVALUE }
  end | Stack].

-compile({inline,{yeccpars2_716_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1139).
yeccpars2_716_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { embed , __3 , __5 }
  end | Stack].

-compile({inline,{yeccpars2_719_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1128).
yeccpars2_719_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_720_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1127).
yeccpars2_720_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_721_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1123).
yeccpars2_721_([__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_eventParameters ( [ __2 | __3 ] )
  end | Stack].

-compile({inline,{yeccpars2_724_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1117).
yeccpars2_724_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_725_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1116).
yeccpars2_725_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_726_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1113).
yeccpars2_726_([__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'EventsDescriptor' { requestID = __3 ,
    eventList = [ __5 | __6 ] }
  end | Stack].

-compile({inline,{yeccpars2_727_,1}}).
-file("megaco_text_parser_prev3b.yrl", 462).
yeccpars2_727_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_728_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1080).
yeccpars2_728_([__1 | Stack]) ->
 [begin
   merge_eventSpec ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_729_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1078).
yeccpars2_729_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_731_,1}}).
-file("megaco_text_parser_prev3b.yrl", 462).
yeccpars2_731_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_732_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1078).
yeccpars2_732_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_733_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1077).
yeccpars2_733_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_734_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1075).
yeccpars2_734_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __3 | __4 ]
  end | Stack].

-compile({inline,{yeccpars2_737_,1}}).
-file("megaco_text_parser_prev3b.yrl", 632).
yeccpars2_737_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_738_,1}}).
-file("megaco_text_parser_prev3b.yrl", 631).
yeccpars2_738_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_739_,1}}).
-file("megaco_text_parser_prev3b.yrl", 628).
yeccpars2_739_([__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_742_,1}}).
-file("megaco_text_parser_prev3b.yrl", 507).
yeccpars2_742_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_743_,1}}).
-file("megaco_text_parser_prev3b.yrl", 506).
yeccpars2_743_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_744_,1}}).
-file("megaco_text_parser_prev3b.yrl", 495).
yeccpars2_744_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'TransactionRequest' { transactionId = asn1_NOVALUE ,
    actions = [ __3 | __4 ] }
  end | Stack].

-compile({inline,{yeccpars2_746_,1}}).
-file("megaco_text_parser_prev3b.yrl", 925).
yeccpars2_746_([__1 | Stack]) ->
 [begin
   ensure_uint32 ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_748_,1}}).
-file("megaco_text_parser_prev3b.yrl", 507).
yeccpars2_748_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_750_,1}}).
-file("megaco_text_parser_prev3b.yrl", 499).
yeccpars2_750_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'TransactionRequest' { transactionId = asn1_NOVALUE ,
    actions = [ __4 | __5 ] }
  end | Stack].

-compile({inline,{yeccpars2_752_,1}}).
-file("megaco_text_parser_prev3b.yrl", 507).
yeccpars2_752_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_754_,1}}).
-file("megaco_text_parser_prev3b.yrl", 503).
yeccpars2_754_([__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'TransactionRequest' { transactionId = ensure_transactionID ( __3 ) ,
    actions = [ __5 | __6 ] }
  end | Stack].

-compile({inline,{yeccpars2_756_,1}}).
-file("megaco_text_parser_prev3b.yrl", 486).
yeccpars2_756_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_757_,1}}).
-file("megaco_text_parser_prev3b.yrl", 488).
yeccpars2_757_([__1 | Stack]) ->
 [begin
   ensure_transactionAck ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_760_,1}}).
-file("megaco_text_parser_prev3b.yrl", 486).
yeccpars2_760_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_761_,1}}).
-file("megaco_text_parser_prev3b.yrl", 485).
yeccpars2_761_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_762_,1}}).
-file("megaco_text_parser_prev3b.yrl", 483).
yeccpars2_762_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __3 | __4 ]
  end | Stack].

-compile({inline,{yeccpars2_765_,1}}).
-file("megaco_text_parser_prev3b.yrl", 584).
yeccpars2_765_(Stack) ->
 [begin
   asn1_NOVALUE
  end | Stack].

-compile({inline,{yeccpars2_768_,1}}).
-file("megaco_text_parser_prev3b.yrl", 583).
yeccpars2_768_([__2,__1 | Stack]) ->
 [begin
   'NULL'
  end | Stack].

-compile({inline,{yeccpars2_770_,1}}).
-file("megaco_text_parser_prev3b.yrl", 586).
yeccpars2_770_([__1 | Stack]) ->
 [begin
   { transactionError , __1 }
  end | Stack].

-compile({inline,{yeccpars2_771_,1}}).
-file("megaco_text_parser_prev3b.yrl", 590).
yeccpars2_771_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_776_,1}}).
-file("megaco_text_parser_prev3b.yrl", 611).
yeccpars2_776_([__1 | Stack]) ->
 [begin
   { command , __1 }
  end | Stack].

-compile({inline,{yeccpars2_777_,1}}).
-file("megaco_text_parser_prev3b.yrl", 614).
yeccpars2_777_([__1 | Stack]) ->
 [begin
   { command , __1 }
  end | Stack].

-compile({inline,{yeccpars2_778_,1}}).
-file("megaco_text_parser_prev3b.yrl", 597).
yeccpars2_778_([__1 | Stack]) ->
 [begin
   # 'ActionReply' { errorDescriptor = __1 }
  end | Stack].

-compile({inline,{yeccpars2_779_,1}}).
-file("megaco_text_parser_prev3b.yrl", 615).
yeccpars2_779_([__1 | Stack]) ->
 [begin
   { context , __1 }
  end | Stack].

-compile({inline,{yeccpars2_780_,1}}).
-file("megaco_text_parser_prev3b.yrl", 609).
yeccpars2_780_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_781_,1}}).
-file("megaco_text_parser_prev3b.yrl", 612).
yeccpars2_781_([__1 | Stack]) ->
 [begin
   { command , __1 }
  end | Stack].

-compile({inline,{yeccpars2_783_,1}}).
-file("megaco_text_parser_prev3b.yrl", 613).
yeccpars2_783_([__1 | Stack]) ->
 [begin
   { command , __1 }
  end | Stack].

-compile({inline,{yeccpars2_785_,1}}).
-file("megaco_text_parser_prev3b.yrl", 649).
yeccpars2_785_([__1 | Stack]) ->
 [begin
   addReply
  end | Stack].

-compile({inline,{yeccpars2_788_,1}}).
-file("megaco_text_parser_prev3b.yrl", 651).
yeccpars2_788_([__1 | Stack]) ->
 [begin
   modReply
  end | Stack].

-compile({inline,{yeccpars2_789_,1}}).
-file("megaco_text_parser_prev3b.yrl", 650).
yeccpars2_789_([__1 | Stack]) ->
 [begin
   moveReply
  end | Stack].

-compile({inline,{yeccpars2_792_,1}}).
-file("megaco_text_parser_prev3b.yrl", 652).
yeccpars2_792_([__1 | Stack]) ->
 [begin
   subtractReply
  end | Stack].

-compile({inline,{yeccpars2_794_,1}}).
-file("megaco_text_parser_prev3b.yrl", 914).
yeccpars2_794_(Stack) ->
 [begin
   { serviceChangeResParms , # 'ServiceChangeResParm' { } }
  end | Stack].

-compile({inline,{yeccpars2_795_,1}}).
-file("megaco_text_parser_prev3b.yrl", 906).
yeccpars2_795_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { serviceChangeReply ,
    # 'ServiceChangeReply' { terminationID = [ __3 ] ,
    serviceChangeResult = __4 } }
  end | Stack].

-compile({inline,{yeccpars2_801_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1380).
yeccpars2_801_([__1 | Stack]) ->
 [begin
   { time_stamp , __1 }
  end | Stack].

-compile({inline,{yeccpars2_802_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1379).
yeccpars2_802_([__1 | Stack]) ->
 [begin
   { version , __1 }
  end | Stack].

-compile({inline,{yeccpars2_803_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1378).
yeccpars2_803_([__1 | Stack]) ->
 [begin
   { profile , __1 }
  end | Stack].

-compile({inline,{yeccpars2_804_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1377).
yeccpars2_804_([__1 | Stack]) ->
 [begin
   { mgc_id , __1 }
  end | Stack].

-compile({inline,{yeccpars2_805_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1376).
yeccpars2_805_([__1 | Stack]) ->
 [begin
   { address , __1 }
  end | Stack].

-compile({inline,{yeccpars2_806_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1374).
yeccpars2_806_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_813_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1374).
yeccpars2_813_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_814_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1373).
yeccpars2_814_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_815_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1370).
yeccpars2_815_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_ServiceChangeResParm ( [ __3 | __4 ] )
  end | Stack].

-compile({inline,{yeccpars2_816_,1}}).
-file("megaco_text_parser_prev3b.yrl", 911).
yeccpars2_816_([__3,__2,__1 | Stack]) ->
 [begin
   { errorDescriptor , __2 }
  end | Stack].

-compile({inline,{yeccpars2_817_,1}}).
-file("megaco_text_parser_prev3b.yrl", 913).
yeccpars2_817_([__3,__2,__1 | Stack]) ->
 [begin
   { serviceChangeResParms , __2 }
  end | Stack].

-compile({inline,{yeccpars2_819_,1}}).
-file("megaco_text_parser_prev3b.yrl", 897).
yeccpars2_819_(Stack) ->
 [begin
   asn1_NOVALUE
  end | Stack].

-compile({inline,{yeccpars2_820_,1}}).
-file("megaco_text_parser_prev3b.yrl", 892).
yeccpars2_820_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { notifyReply ,
    # 'NotifyReply' { terminationID = [ __3 ] ,
    errorDescriptor = __4 } }
  end | Stack].

-compile({inline,{yeccpars2_823_,1}}).
-file("megaco_text_parser_prev3b.yrl", 896).
yeccpars2_823_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_825_,1}}).
-file("megaco_text_parser_prev3b.yrl", 693).
yeccpars2_825_([__1 | Stack]) ->
 [begin
   { auditResult ,
    # 'AuditResult' { terminationID = __1 ,
    terminationAuditResult = [ ] } }
  end | Stack].

-compile({inline,{yeccpars2_826_,1}}).
-file("megaco_text_parser_prev3b.yrl", 683).
yeccpars2_826_([__3,__2,__1 | Stack]) ->
 [begin
   { auditValueReply , __3 }
  end | Stack].

-compile({inline,{yeccpars2_827_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1450).
yeccpars2_827_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_828_,1}}).
-file("megaco_text_parser_prev3b.yrl", 688).
yeccpars2_828_([__1 | Stack]) ->
 [begin
   { contextAuditResult , __1 }
  end | Stack].

-compile({inline,{yeccpars2_829_,1}}).
-file("megaco_text_parser_prev3b.yrl", 679).
yeccpars2_829_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { auditValueReply , __4 }
  end | Stack].

-compile({inline,{yeccpars2_832_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1465).
yeccpars2_832_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_834_,1}}).
-file("megaco_text_parser_prev3b.yrl", 920).
yeccpars2_834_([__1 | Stack]) ->
 [begin
   ensure_uint ( __1 , 0 , 999 )
  end | Stack].

-compile({inline,{yeccpars2_836_,1}}).
-file("megaco_text_parser_prev3b.yrl", 923).
yeccpars2_836_(Stack) ->
 [begin
   asn1_NOVALUE
  end | Stack].

-compile({inline,{yeccpars2_838_,1}}).
-file("megaco_text_parser_prev3b.yrl", 922).
yeccpars2_838_([__1 | Stack]) ->
 [begin
   value_of ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_839_,1}}).
-file("megaco_text_parser_prev3b.yrl", 917).
yeccpars2_839_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'ErrorDescriptor' { errorCode = __3 ,
    errorText = __5 }
  end | Stack].

-compile({inline,{yeccpars2_840_,1}}).
-file("megaco_text_parser_prev3b.yrl", 690).
yeccpars2_840_([__3,__2,__1 | Stack]) ->
 [begin
   { error , __2 }
  end | Stack].

-compile({inline,{yeccpars2_843_,1}}).
-file("megaco_text_parser_prev3b.yrl", 716).
yeccpars2_843_([__1 | Stack]) ->
 [begin
   { statisticsDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_844_,1}}).
-file("megaco_text_parser_prev3b.yrl", 712).
yeccpars2_844_([__1 | Stack]) ->
 [begin
   { signalsDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_845_,1}}).
-file("megaco_text_parser_prev3b.yrl", 717).
yeccpars2_845_([__1 | Stack]) ->
 [begin
   { packagesDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_846_,1}}).
-file("megaco_text_parser_prev3b.yrl", 714).
yeccpars2_846_([__1 | Stack]) ->
 [begin
   { observedEventsDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_847_,1}}).
-file("megaco_text_parser_prev3b.yrl", 710).
yeccpars2_847_([__1 | Stack]) ->
 [begin
   { muxDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_848_,1}}).
-file("megaco_text_parser_prev3b.yrl", 0).
yeccpars2_848_([__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_849_,1}}).
-file("megaco_text_parser_prev3b.yrl", 708).
yeccpars2_849_([__1 | Stack]) ->
 [begin
   { mediaDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_850_,1}}).
-file("megaco_text_parser_prev3b.yrl", 711).
yeccpars2_850_([__1 | Stack]) ->
 [begin
   { eventsDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_851_,1}}).
-file("megaco_text_parser_prev3b.yrl", 715).
yeccpars2_851_([__1 | Stack]) ->
 [begin
   { eventBufferDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_852_,1}}).
-file("megaco_text_parser_prev3b.yrl", 718).
yeccpars2_852_([__1 | Stack]) ->
 [begin
   { errorDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_853_,1}}).
-file("megaco_text_parser_prev3b.yrl", 713).
yeccpars2_853_([__1 | Stack]) ->
 [begin
   { digitMapDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_854_,1}}).
-file("megaco_text_parser_prev3b.yrl", 706).
yeccpars2_854_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_855_,1}}).
-file("megaco_text_parser_prev3b.yrl", 719).
yeccpars2_855_([__1 | Stack]) ->
 [begin
   { auditReturnItem , __1 }
  end | Stack].

-compile({inline,{yeccpars2_856_,1}}).
-file("megaco_text_parser_prev3b.yrl", 734).
yeccpars2_856_([__1 | Stack]) ->
 [begin
   mediaToken
  end | Stack].

-compile({inline,{yeccpars2_857_,1}}).
-file("megaco_text_parser_prev3b.yrl", 733).
yeccpars2_857_([__1 | Stack]) ->
 [begin
   modemToken
  end | Stack].

-compile({inline,{yeccpars2_858_,1}}).
-file("megaco_text_parser_prev3b.yrl", 732).
yeccpars2_858_([__1 | Stack]) ->
 [begin
   muxToken
  end | Stack].

-compile({inline,{yeccpars2_859_,1}}).
-file("megaco_text_parser_prev3b.yrl", 737).
yeccpars2_859_([__1 | Stack]) ->
 [begin
   observedEventsToken
  end | Stack].

-compile({inline,{yeccpars2_860_,1}}).
-file("megaco_text_parser_prev3b.yrl", 738).
yeccpars2_860_([__1 | Stack]) ->
 [begin
   packagesToken
  end | Stack].

-compile({inline,{yeccpars2_861_,1}}).
-file("megaco_text_parser_prev3b.yrl", 736).
yeccpars2_861_([__1 | Stack]) ->
 [begin
   statsToken
  end | Stack].

-compile({inline,{yeccpars2_863_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1387).
yeccpars2_863_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_866_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1387).
yeccpars2_866_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_867_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1386).
yeccpars2_867_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_868_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1384).
yeccpars2_868_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __3 | __4 ]
  end | Stack].

-compile({inline,{yeccpars2_869_,1}}).
-file("megaco_text_parser_prev3b.yrl", 703).
yeccpars2_869_([__2,__1 | Stack]) ->
 [begin
   merge_terminationAudit ( [ __1 | __2 ] )
  end | Stack].

-compile({inline,{yeccpars2_871_,1}}).
-file("megaco_text_parser_prev3b.yrl", 706).
yeccpars2_871_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_872_,1}}).
-file("megaco_text_parser_prev3b.yrl", 705).
yeccpars2_872_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_873_,1}}).
-file("megaco_text_parser_prev3b.yrl", 697).
yeccpars2_873_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { auditResult ,
    # 'AuditResult' { terminationID = __1 ,
    terminationAuditResult = __3 } }
  end | Stack].

-compile({inline,{yeccpars2_875_,1}}).
-file("megaco_text_parser_prev3b.yrl", 685).
yeccpars2_875_([__3,__2,__1 | Stack]) ->
 [begin
   { auditCapReply , __3 }
  end | Stack].

-compile({inline,{yeccpars2_876_,1}}).
-file("megaco_text_parser_prev3b.yrl", 1450).
yeccpars2_876_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_877_,1}}).
-file("megaco_text_parser_prev3b.yrl", 681).
yeccpars2_877_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { auditCapReply , __4 }
  end | Stack].

-compile({inline,{yeccpars2_878_,1}}).
-file("megaco_text_parser_prev3b.yrl", 594).
yeccpars2_878_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   setelement ( # 'ActionReply' .contextId , __5 , __3 )
  end | Stack].

-compile({inline,{yeccpars2_880_,1}}).
-file("megaco_text_parser_prev3b.yrl", 655).
yeccpars2_880_(Stack) ->
 [begin
   asn1_NOVALUE
  end | Stack].

-compile({inline,{yeccpars2_881_,1}}).
-file("megaco_text_parser_prev3b.yrl", 646).
yeccpars2_881_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { __1 , # 'AmmsReply' { terminationID = [ __3 ] ,
    terminationAudit = __4 } }
  end | Stack].

-compile({inline,{yeccpars2_884_,1}}).
-file("megaco_text_parser_prev3b.yrl", 654).
yeccpars2_884_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_885_,1}}).
-file("megaco_text_parser_prev3b.yrl", 599).
yeccpars2_885_([__2,__1 | Stack]) ->
 [begin
   merge_action_reply ( [ __1 | __2 ] )
  end | Stack].

-compile({inline,{yeccpars2_887_,1}}).
-file("megaco_text_parser_prev3b.yrl", 606).
yeccpars2_887_([__2,__1 | Stack]) ->
 [begin
   [ { error , __2 } ]
  end | Stack].

-compile({inline,{yeccpars2_888_,1}}).
-file("megaco_text_parser_prev3b.yrl", 609).
yeccpars2_888_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_889_,1}}).
-file("megaco_text_parser_prev3b.yrl", 608).
yeccpars2_889_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_890_,1}}).
-file("megaco_text_parser_prev3b.yrl", 587).
yeccpars2_890_([__2,__1 | Stack]) ->
 [begin
   { actionReplies , [ __1 | __2 ] }
  end | Stack].

-compile({inline,{yeccpars2_892_,1}}).
-file("megaco_text_parser_prev3b.yrl", 590).
yeccpars2_892_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_893_,1}}).
-file("megaco_text_parser_prev3b.yrl", 589).
yeccpars2_893_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_894_,1}}).
-file("megaco_text_parser_prev3b.yrl", 579).
yeccpars2_894_([__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'TransactionReply' { transactionId = __3 ,
    immAckRequired = __5 ,
    transactionResult = __6 }
  end | Stack].

-compile({inline,{yeccpars2_898_,1}}).
-file("megaco_text_parser_prev3b.yrl", 491).
yeccpars2_898_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'TransactionPending' { transactionId = ensure_transactionID ( __3 ) }
  end | Stack].

-compile({inline,{yeccpars2_899_,1}}).
-file("megaco_text_parser_prev3b.yrl", 475).
yeccpars2_899_([__2,__1 | Stack]) ->
 [begin
   [ __1 | __2 ]
  end | Stack].

-compile({inline,{yeccpars2_900_,1}}).
-file("megaco_text_parser_prev3b.yrl", 966).
yeccpars2_900_([__1 | Stack]) ->
 [begin
   ensure_pathName ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_901_,1}}).
-file("megaco_text_parser_prev3b.yrl", 937).
yeccpars2_901_([__1 | Stack]) ->
 [begin
   { deviceName , __1 }
  end | Stack].

-compile({inline,{yeccpars2_902_,1}}).
-file("megaco_text_parser_prev3b.yrl", 462).
yeccpars2_902_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_903_,1}}).
-file("megaco_text_parser_prev3b.yrl", 462).
yeccpars2_903_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_904_,1}}).
-file("megaco_text_parser_prev3b.yrl", 954).
yeccpars2_904_([__1 | Stack]) ->
 [begin
   ensure_mtpAddress ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_905_,1}}).
-file("megaco_text_parser_prev3b.yrl", 930).
yeccpars2_905_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_906_,1}}).
-file("megaco_text_parser_prev3b.yrl", 929).
yeccpars2_906_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].


-file("megaco_text_parser_prev3b.yrl", 1568).
