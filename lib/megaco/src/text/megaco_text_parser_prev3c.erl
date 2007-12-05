-module(megaco_text_parser_prev3c).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("megaco_text_parser_prev3c.yrl", 1655).

%% The following directive is needed for (significantly) faster compilation
%% of the generated .erl file by the HiPE compiler.  Please do not remove.
-compile([{hipe,[{regalloc,linear_scan}]}]).

-include("megaco_text_parser_prev3c.hrl").



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



-file("./megaco_text_parser_prev3c.erl", 157).

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
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_168(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(169=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(170=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(171=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_171(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(172=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(173=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(174=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(175=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(176=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_182(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(183=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_216(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(217=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(218=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_229(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(230=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(231=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(232=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_232(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(233=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_233(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(234=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_247(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(248=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_248(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(249=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_249(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(250=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(251=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_251(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(252=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_252(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(253=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_253(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(254=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(255=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_255(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(256=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_256(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(257=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_266(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(267=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_267(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(268=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_268(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(269=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_261(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(270=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_270(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(271=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_271(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(272=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_272(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(273=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(274=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_274(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(275=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(276=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_276(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(277=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_277(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(278=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_278(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(279=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_279(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(280=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_280(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(281=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_281(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(282=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_282(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(283=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_283(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(284=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_284(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(285=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_284(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(286=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_284(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(292=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_292(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(293=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_293(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(294=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_284(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(295=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_284(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(296=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_296(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(297=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_297(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(298=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_284(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(299=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_284(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(300=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_300(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(301=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_301(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(302=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_302(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(303=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_303(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(304=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_304(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(305=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_305(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(306=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_306(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(307=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_307(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(308=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_308(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(309=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_239(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(310=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_310(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(311=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_311(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(312=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_312(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(313=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_323(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(324=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_324(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(325=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_325(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(326=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_326(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(334=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_334(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(335=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_335(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(336=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_336(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(337=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_337(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(338=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_337(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(339=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_339(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(340=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_340(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(341=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(342=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(343=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(344=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(345=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(346=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(347=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_347(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(348=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_348(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(349=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(350=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(351=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_351(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_358(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(360=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_360(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(361=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_361(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(362=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_362(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_351(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(369=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_369(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(370=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_370(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(371=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_371(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(372=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_372(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(373=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_317(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(374=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_374(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(375=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_375(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(376=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_376(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(377=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(378=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(379=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_379(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(380=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_380(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(381=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_381(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(382=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_382(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(383=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(384=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_384(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(385=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_385(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(386=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_396(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_408(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(409=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_409(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(410=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_410(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(411=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_411(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(412=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(413=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_413(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(414=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_414(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(415=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_415(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(416=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_416(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(417=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_417(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(418=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_418(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(419=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_419(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(420=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_420(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(421=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_421(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(422=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_422(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(423=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_423(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(424=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_424(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(425=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_425(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(426=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_426(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(427=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_427(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(428=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_241(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(429=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_429(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(430=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_430(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(431=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_431(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(432=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_432(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_439(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(440=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(441=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_441(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(442=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_442(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(443=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_443(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(444=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_444(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(445=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_284(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(446=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_446(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(447=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(448=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_448(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(449=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_449(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(450=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_450(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(451=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(452=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_452(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(453=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(454=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_454(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(455=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_455(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(456=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_456(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(457=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_417(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(458=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_458(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(459=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_459(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(460=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_460(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(461=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_461(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(462=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(463=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_463(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(464=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(472=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_472(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(473=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_473(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(474=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_474(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(475=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_483(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(484=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(485=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_485(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(486=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_486(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(487=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_241(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(488=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_488(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(489=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_489(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(490=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(491=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_491(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(492=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_492(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(493=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_493(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(494=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_494(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(495=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_495(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(496=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(497=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_497(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(498=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_498(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_505(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(506=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_506(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(507=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_507(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(508=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_508(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(509=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_509(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(510=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_510(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(511=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_511(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(512=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_512(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(513=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_513(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(514=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_514(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(515=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_515(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(516=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_516(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(529=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_529(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(530=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_530(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(531=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_531(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(532=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_532(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(533=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(534=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_534(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(535=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_535(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(536=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_542(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(543=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_543(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(544=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_544(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(545=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_545(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(546=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_546(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(547=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(548=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_548(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(549=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_241(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_180(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(557=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_557(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(558=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_558(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(559=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(560=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_560(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(561=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_561(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(562=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_562(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(563=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_563(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(564=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(565=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_565(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(566=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_566(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(567=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(568=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_568(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(569=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_569(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(570=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_570(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(571=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_571(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(572=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_572(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(573=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_573(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(574=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_574(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(575=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_575(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(576=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_576(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(577=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_577(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(578=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_578(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(579=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_579(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(580=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_580(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(581=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_581(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(582=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_582(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(583=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_583(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(590=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_590(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(591=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_591(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(592=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_592(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(593=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_593(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(594=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_284(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(595=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_595(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(596=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_596(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(597=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_597(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(598=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_598(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(599=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(609=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_609(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(610=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(611=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_611(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(612=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_612(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(613=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(614=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_614(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(615=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_615(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(616=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_616(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(617=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_617(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(618=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_603(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(619=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_619(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(620=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_620(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(621=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_621(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(622=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(623=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_623(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(624=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_624(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(625=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_625(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(626=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(627=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_627(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(628=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_628(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(629=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_629(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(630=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(631=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(632=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_632(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(633=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_633(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(634=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_634(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(635=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(636=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_636(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(637=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_637(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(638=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_638(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(639=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_639(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(640=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_646(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(647=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_647(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(648=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_648(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_658(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(659=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_659(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(660=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_660(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(661=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_661(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(662=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_662(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(663=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_663(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(664=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_664(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(665=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_337(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(666=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_666(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(667=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_667(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(668=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_668(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(669=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_669(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(670=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_670(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(671=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_671(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(672=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_658(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(673=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_673(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(674=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_674(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(675=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_675(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(676=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(677=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_677(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(678=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_678(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(679=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_679(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(680=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_680(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(681=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_678(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(682=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_682(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(683=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_683(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(684=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_684(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(685=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_685(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(686=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_686(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(687=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_687(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(688=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_688(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(689=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_689(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(690=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_690(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(691=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_500(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(692=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_692(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(693=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_500(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(694=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_694(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(695=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_358(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(696=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_696(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(697=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_697(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(698=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_685(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(699=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_699(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(700=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_700(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(701=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_701(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(702=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_702(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(703=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_646(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(704=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_704(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(705=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_705(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(706=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_706(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(707=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(708=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_708(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(709=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_718(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(719=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_719(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(720=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_720(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(721=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_721(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(722=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_722(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(723=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_723(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(737=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_737(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(738=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(739=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_739(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(740=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_740(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(741=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_741(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_747(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(748=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_748(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(749=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_749(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(750=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_750(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(751=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_751(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(752=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_752(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(753=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_753(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(754=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_754(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(755=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_742(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(756=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_756(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(757=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_757(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(758=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_758(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(759=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_759(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(760=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(761=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_761(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(762=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_762(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(763=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_763(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_713(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(773=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_773(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(774=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_774(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(775=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_775(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(776=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_776(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(777=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_570(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(791=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_791(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(792=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_792(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(793=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_793(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(794=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_794(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(795=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_123(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(802=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_802(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(803=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_803(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(804=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_804(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(805=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(806=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_806(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(807=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_807(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(808=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_808(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(809=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(810=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_810(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(811=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_811(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(812=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_812(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(813=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(814=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_814(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(815=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_815(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(816=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_816(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(817=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(818=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_818(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_833(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_180(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_862(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(863=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_863(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(864=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_864(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(865=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_865(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(866=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_854(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(867=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_867(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(868=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_868(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(869=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_869(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(870=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_870(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(871=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_871(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(872=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_879(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(880=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_880(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(881=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_881(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(882=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_882(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(883=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_883(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(884=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_884(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(885=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_885(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(886=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_886(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(887=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_895(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(907=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_907(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(908=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_908(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(909=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_909(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(910=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_910(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(911=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_911(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(912=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_912(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(913=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_913(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(914=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_914(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(915=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_915(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(916=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(917=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_917(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(918=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_918(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(919=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(920=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_920(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(921=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_921(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(922=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_922(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(923=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_923(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(924=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_895(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(925=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_925(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(926=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_926(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(927=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_927(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(928=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_928(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(929=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_929(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(930=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_930(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(931=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_931(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(932=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_932(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(933=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(934=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_934(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(935=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_935(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(936=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_895(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(937=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_937(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(938=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_938(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(939=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_939(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(940=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_829(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(941=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_941(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(942=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_942(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(943=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_943(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(944=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_944(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(945=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_945(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(946=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_946(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(947=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_947(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(948=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_948(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(949=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(950=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_950(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(951=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_951(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(952=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_952(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(953=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_953(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(954=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_954(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(955=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_955(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(956=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_956(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(957=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_957(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(958=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_958(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(959=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_959(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(960=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_960(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2_4(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_5(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_6: see yeccpars2_4

yeccpars2_7(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr).

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

%% yeccpars2_80: see yeccpars2_4

yeccpars2_81(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_82: see yeccpars2_4

yeccpars2_83(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_83_(Stack),
 yeccpars2(84, Cat, [83 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_84_(Stack),
 Nss = lists:nthtail(7, Ss),
 yeccpars2(yeccgoto_authenticationHeader(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_85(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_85_(Stack),
 yeccpars2(88, Cat, [85 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_86(S, endOfMessage, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr).

yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_87_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_megacoMessage(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_88(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'MtpAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 958, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_89(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr).

yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_mId(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_mId(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_92: see yeccpars2_4

yeccpars2_93(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_93_(Stack),
 yeccpars2(95, Cat, [93 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_94(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_(Stack),
 yeccpars2(103, Cat, [94 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_95(S, 'RSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr).

yeccpars2_96(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_96_(Stack),
 yeccpars2(97, Cat, [96 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_97_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_daddr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_98(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_domainAddress(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_99: see yeccpars2_4

yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_100_(Stack),
 yeccpars2(yeccgoto_portNumber(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_101(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_(Stack),
 yeccpars2(102, Cat, [101 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_domainAddress(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_daddr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_104(S, 'GREATER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr).

yeccpars2_105(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_105_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_domainName(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_106: see yeccpars2_4

yeccpars2_107(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_(Stack),
 yeccpars2(108, Cat, [107 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_108_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_domainName(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_109_(Stack),
 yeccpars2(yeccgoto_transactionItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_110_(Stack),
 yeccpars2(yeccgoto_transactionItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_111_(Stack),
 yeccpars2(yeccgoto_transactionItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_112_(Stack),
 yeccpars2(yeccgoto_transactionItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_113_(Stack),
 yeccpars2(yeccgoto_messageBody(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_114(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_114_(Stack),
 yeccpars2(yeccgoto_transactionList(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_115_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_message(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_116_(Stack),
 yeccpars2(yeccgoto_messageBody(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_117(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 887, Ss, Stack, T, Ts, Tzr).

yeccpars2_118(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 949, Ss, Stack, T, Ts, Tzr).

yeccpars2_119(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 817, Ss, Stack, T, Ts, Tzr).

yeccpars2_120(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 809, Ss, Stack, T, Ts, Tzr).

yeccpars2_121(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr).

yeccpars2_122(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 801, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_123(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr).

yeccpars2_124(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 795, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_124_(Stack),
 yeccpars2(794, Cat, [124 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_125(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_126: see yeccpars2_4

yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_127_(Stack),
 yeccpars2(yeccgoto_contextID(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_128(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 129, Ss, Stack, T, Ts, Tzr).

yeccpars2_129(S, 'AddToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 145, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 146, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 147, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, 'ContextAttrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 149, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, 'EmergencyOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 150, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, 'EmergencyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 151, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, 'IEPSToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 152, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, 'ModifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 153, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, 'MoveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 154, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 155, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, 'PriorityToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 157, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, 'SubtractToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, 'TopologyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 159, Ss, Stack, T, Ts, Tzr).

yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_130_(Stack),
 yeccpars2(yeccgoto_contextProperty(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_commandRequest(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_commandRequest(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_133_(Stack),
 yeccpars2(yeccgoto_contextProperty(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_134(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_commandRequest(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_135_(Stack),
 yeccpars2(yeccgoto_contextProperty(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_136_(Stack),
 yeccpars2(yeccgoto_actionRequestItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_137_(Stack),
 yeccpars2(yeccgoto_actionRequestItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_138(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_contextProperty(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_139_(Stack),
 yeccpars2(yeccgoto_actionRequestItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_140(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_commandRequest(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_141(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 567, Ss, Stack, T, Ts, Tzr).

yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_commandRequest(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_143(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 564, Ss, Stack, T, Ts, Tzr);
yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_143_(Stack),
 yeccpars2(563, Cat, [143 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_144(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 562, Ss, Stack, T, Ts, Tzr).

yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_145_(Stack),
 yeccpars2(yeccgoto_ammToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_146(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 559, Ss, Stack, T, Ts, Tzr).

yeccpars2_147(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 556, Ss, Stack, T, Ts, Tzr).

yeccpars2_148(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 555, Ss, Stack, T, Ts, Tzr).

yeccpars2_149(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 504, Ss, Stack, T, Ts, Tzr).

yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_(Stack),
 yeccpars2(yeccgoto_contextProperty(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_151_(Stack),
 yeccpars2(yeccgoto_contextProperty(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_152(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 500, Ss, Stack, T, Ts, Tzr).

yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_153_(Stack),
 yeccpars2(yeccgoto_ammToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_154_(Stack),
 yeccpars2(yeccgoto_ammToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_155(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 464, Ss, Stack, T, Ts, Tzr).

yeccpars2_156(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 462, Ss, Stack, T, Ts, Tzr).

yeccpars2_157(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 412, Ss, Stack, T, Ts, Tzr).

yeccpars2_158(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr).

yeccpars2_159(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 160, Ss, Stack, T, Ts, Tzr).

yeccpars2_160(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 166, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'OnewayBothToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 168, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'OnewayExternalToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 170, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 171, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_161(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_topologyDescComp(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_162(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_162_(Stack),
 yeccpars2(175, Cat, [162 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_163(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_163_(Stack),
 yeccpars2(yeccgoto_topologyDescComp(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_164(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_164_(Stack),
 yeccpars2(yeccgoto_terminationID(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_165(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_165_(Stack),
 yeccpars2(yeccgoto_topologyDescComp(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_166_(Stack),
 yeccpars2(yeccgoto_topologyDirection(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_167_(Stack),
 yeccpars2(yeccgoto_topologyDirection(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_168_(Stack),
 yeccpars2(yeccgoto_topologyDirection(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_169(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_169_(Stack),
 yeccpars2(yeccgoto_topologyDirection(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_170_(Stack),
 yeccpars2(yeccgoto_topologyDirection(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_171(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_171_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_172: see yeccpars2_4

yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_173_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_eventStream(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_174(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_174_(Stack),
 yeccpars2(yeccgoto_streamID(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_175(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_176: see yeccpars2_160

yeccpars2_177(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_177_(Stack),
 yeccpars2(178, Cat, [177 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_178_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_topologyDescCompList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_179(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_179_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_topologyDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_180(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_181(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_181_(Stack),
 yeccpars2(yeccgoto_termIDList(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_182(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_182_(Stack),
 yeccpars2(190, Cat, [182 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_183: see yeccpars2_4

yeccpars2_184(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_184_(Stack),
 yeccpars2(185, Cat, [184 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_185(S, 'RSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_186: see yeccpars2_4

yeccpars2_187(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_187(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_187_(Stack),
 yeccpars2(188, Cat, [187 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_188(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_188_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_terminationIDListRepeat(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_189(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_189_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_termIDList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_190(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_190_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_subtractRequest(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_191(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr).

yeccpars2_192(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 411, Ss, Stack, T, Ts, Tzr).

yeccpars2_193(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr).

yeccpars2_194(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 207, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(S, 'DigitMapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 208, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 209, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 210, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 211, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(S, 'ModemToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 212, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(S, 'MuxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 213, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(S, 'ObservedEventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 214, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(S, 'PackagesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 215, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 216, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 217, Ss, Stack, T, Ts, Tzr);
yeccpars2_194(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_194_(Stack),
 yeccpars2(206, Cat, [194 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_195(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_195_(Stack),
 yeccpars2(yeccgoto_auditItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

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

yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_201_(Stack),
 yeccpars2(yeccgoto_indAudauditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_202_(Stack),
 yeccpars2(yeccgoto_indAudauditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_203(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 403, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_203_(Stack),
 yeccpars2(402, Cat, [203 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_204(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_auditItem(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_205(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 399, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_205_(Stack),
 yeccpars2(398, Cat, [205 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_206(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 397, Ss, Stack, T, Ts, Tzr).

yeccpars2_207(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_207_(Stack),
 yeccpars2(yeccgoto_indAuddigitMapDescriptor(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_208(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_208_(Stack),
 yeccpars2(yeccgoto_auditReturnItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_209(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 386, Ss, Stack, T, Ts, Tzr);
yeccpars2_209(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_209_(Stack),
 yeccpars2(yeccgoto_auditItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_210(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 377, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 378, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_210_(Stack),
 yeccpars2(yeccgoto_auditItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_211(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 317, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_211_(Stack),
 yeccpars2(yeccgoto_auditReturnItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_212(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_212_(Stack),
 yeccpars2(yeccgoto_auditReturnItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_213(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_213_(Stack),
 yeccpars2(yeccgoto_auditReturnItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_214(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_214_(Stack),
 yeccpars2(yeccgoto_auditReturnItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_215(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 313, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_215_(Stack),
 yeccpars2(yeccgoto_auditReturnItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_216(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 223, Ss, Stack, T, Ts, Tzr);
yeccpars2_216(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_216_(Stack),
 yeccpars2(yeccgoto_auditItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_217(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 218, Ss, Stack, T, Ts, Tzr);
yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_217_(Stack),
 yeccpars2(yeccgoto_auditReturnItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_218: see yeccpars2_4

yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_219_(Stack),
 yeccpars2(yeccgoto_pkgdName(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_220(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 221, Ss, Stack, T, Ts, Tzr).

yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_221_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_indAudstatisticsDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_222(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_222_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_indAudsignalsDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_223(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 229, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 230, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_223(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_224(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_224_(Stack),
 yeccpars2(yeccgoto_indAudsignalParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_225(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 239, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_225_(Stack),
 yeccpars2(yeccgoto_signalRequest(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_226(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_signalName(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_227(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 238, Ss, Stack, T, Ts, Tzr).

yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_228_(Stack),
 yeccpars2(yeccgoto_indAudsignalParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_229(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_229_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_optIndAudsignalParm(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_230(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 231, Ss, Stack, T, Ts, Tzr);
yeccpars2_230(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_230_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_231: see yeccpars2_4

yeccpars2_232(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 234, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_232_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_indAudsignalList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_233(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_233_(Stack),
 yeccpars2(yeccgoto_signalListId(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_234: see yeccpars2_4

yeccpars2_235(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_signalListParm(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_236(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 237, Ss, Stack, T, Ts, Tzr).

yeccpars2_237(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_237_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_indAudsignalList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_238(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_238_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_optIndAudsignalParm(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_239(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 242, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 243, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'IntsigDelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 244, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 245, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 246, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 247, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 248, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 249, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_239(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_240(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 309, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_240_(Stack),
 yeccpars2(308, Cat, [240 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_241(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 283, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'GREATER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 284, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 285, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'NEQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 286, Ss, Stack, T, Ts, Tzr).

yeccpars2_242(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 277, Ss, Stack, T, Ts, Tzr);
yeccpars2_242(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_242_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_243(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 275, Ss, Stack, T, Ts, Tzr);
yeccpars2_243(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_243_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_244(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 273, Ss, Stack, T, Ts, Tzr).

yeccpars2_245(_S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_245_COMMA(Stack),
 yeccpars2(yeccgoto_sigParameter(hd(Ss)), 'COMMA', Ss, NewStack, T, Ts, Tzr);
yeccpars2_245(_S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_245_RBRKT(Stack),
 yeccpars2(yeccgoto_sigParameter(hd(Ss)), 'RBRKT', Ss, NewStack, T, Ts, Tzr);
yeccpars2_245(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_245_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_246(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 260, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_246_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_247(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 257, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_247_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_248(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 252, Ss, Stack, T, Ts, Tzr);
yeccpars2_248(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_248_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_249(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 250, Ss, Stack, T, Ts, Tzr);
yeccpars2_249(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_249_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_250: see yeccpars2_4

yeccpars2_251(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_251_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_sigParameter(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_252(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 254, Ss, Stack, T, Ts, Tzr);
yeccpars2_252(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 255, Ss, Stack, T, Ts, Tzr);
yeccpars2_252(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 256, Ss, Stack, T, Ts, Tzr).

yeccpars2_253(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_253_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_sigParameter(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_254(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_254_(Stack),
 yeccpars2(yeccgoto_signalType(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_255(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_255_(Stack),
 yeccpars2(yeccgoto_signalType(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_256(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_256_(Stack),
 yeccpars2(yeccgoto_signalType(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_257: see yeccpars2_4

yeccpars2_258(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_258_(Stack),
 yeccpars2(yeccgoto_requestID(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_259(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_259_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_sigParameter(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_260(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 261, Ss, Stack, T, Ts, Tzr).

yeccpars2_261(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 263, Ss, Stack, T, Ts, Tzr);
yeccpars2_261(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 264, Ss, Stack, T, Ts, Tzr);
yeccpars2_261(S, 'IterationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 265, Ss, Stack, T, Ts, Tzr);
yeccpars2_261(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 266, Ss, Stack, T, Ts, Tzr);
yeccpars2_261(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 267, Ss, Stack, T, Ts, Tzr).

yeccpars2_262(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 269, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_262_(Stack),
 yeccpars2(268, Cat, [262 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_263(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_263_(Stack),
 yeccpars2(yeccgoto_notificationReason(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_264(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_264_(Stack),
 yeccpars2(yeccgoto_notificationReason(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_265(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_265_(Stack),
 yeccpars2(yeccgoto_notificationReason(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_266(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_266_(Stack),
 yeccpars2(yeccgoto_notificationReason(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_267(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_267_(Stack),
 yeccpars2(yeccgoto_notificationReason(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_268(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 272, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_269: see yeccpars2_261

yeccpars2_270(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 269, Ss, Stack, T, Ts, Tzr);
yeccpars2_270(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_270_(Stack),
 yeccpars2(271, Cat, [270 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_271(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_271_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_notificationReasons(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_272(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_272_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_sigParameter(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_273: see yeccpars2_4

yeccpars2_274(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_274_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_sigParameter(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_275: see yeccpars2_4

yeccpars2_276(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_276_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_sigParameter(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_277(S, 'BothToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 279, Ss, Stack, T, Ts, Tzr);
yeccpars2_277(S, 'ExternalToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 280, Ss, Stack, T, Ts, Tzr);
yeccpars2_277(S, 'InternalToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 281, Ss, Stack, T, Ts, Tzr).

yeccpars2_278(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_278_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_sigParameter(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_279(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_279_(Stack),
 yeccpars2(yeccgoto_direction(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_280(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_280_(Stack),
 yeccpars2(yeccgoto_direction(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_281(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_281_(Stack),
 yeccpars2(yeccgoto_direction(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_282(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_282_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_sigParameter(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_283(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 294, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 295, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'QuotedChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 289, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_284(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'QuotedChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 289, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_284(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_285: see yeccpars2_284

%% yeccpars2_286: see yeccpars2_284

yeccpars2_287(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_287_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_parmValue(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_288(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_288_(Stack),
 yeccpars2(yeccgoto_value(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_289(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_289_(Stack),
 yeccpars2(yeccgoto_value(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_290(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_290_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_parmValue(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_291(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_291_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_parmValue(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_292(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_292_(Stack),
 yeccpars2(yeccgoto_alternativeValue(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_293(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_293_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_parmValue(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_294: see yeccpars2_284

%% yeccpars2_295: see yeccpars2_284

yeccpars2_296(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 298, Ss, Stack, T, Ts, Tzr);
yeccpars2_296(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 299, Ss, Stack, T, Ts, Tzr);
yeccpars2_296(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_296_(Stack),
 yeccpars2(297, Cat, [296 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_297(S, 'RSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 304, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_298: see yeccpars2_284

%% yeccpars2_299: see yeccpars2_284

yeccpars2_300(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 299, Ss, Stack, T, Ts, Tzr);
yeccpars2_300(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_300_(Stack),
 yeccpars2(301, Cat, [300 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_301(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_301_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_valueList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_302(S, 'RSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 303, Ss, Stack, T, Ts, Tzr).

yeccpars2_303(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_303_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_alternativeValue(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_304(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_304_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_alternativeValue(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_305(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 299, Ss, Stack, T, Ts, Tzr);
yeccpars2_305(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_305_(Stack),
 yeccpars2(306, Cat, [305 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_306(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 307, Ss, Stack, T, Ts, Tzr).

yeccpars2_307(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_307_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_alternativeValue(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_308(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 312, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_309: see yeccpars2_239

yeccpars2_310(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 309, Ss, Stack, T, Ts, Tzr);
yeccpars2_310(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_310_(Stack),
 yeccpars2(311, Cat, [310 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_311(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_sigParameters(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_312(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_312_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_signalRequest(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_313: see yeccpars2_4

yeccpars2_314(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_314_(Stack),
 yeccpars2(yeccgoto_packagesItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_315(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 316, Ss, Stack, T, Ts, Tzr).

yeccpars2_316(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_indAudpackagesDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_317(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 324, Ss, Stack, T, Ts, Tzr);
yeccpars2_317(S, 'LocalDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 325, Ss, Stack, T, Ts, Tzr);
yeccpars2_317(S, 'RemoteDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 326, Ss, Stack, T, Ts, Tzr);
yeccpars2_317(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 327, Ss, Stack, T, Ts, Tzr);
yeccpars2_317(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 328, Ss, Stack, T, Ts, Tzr);
yeccpars2_317(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 329, Ss, Stack, T, Ts, Tzr).

yeccpars2_318(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_318_(Stack),
 yeccpars2(yeccgoto_indAudmediaParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_319(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_319_(Stack),
 yeccpars2(yeccgoto_indAudmediaParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_320(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_320_(Stack),
 yeccpars2(yeccgoto_indAudmediaParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_321(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_321_(Stack),
 yeccpars2(yeccgoto_indAudstreamParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_322(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 373, Ss, Stack, T, Ts, Tzr);
yeccpars2_322(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_322_(Stack),
 yeccpars2(372, Cat, [322 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_323(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_323_(Stack),
 yeccpars2(yeccgoto_indAudstreamParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_324(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 351, Ss, Stack, T, Ts, Tzr).

yeccpars2_325(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_325_(Stack),
 yeccpars2(yeccgoto_indAudstreamParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_326(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_326_(Stack),
 yeccpars2(yeccgoto_indAudstreamParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_327(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 218, Ss, Stack, T, Ts, Tzr).

yeccpars2_328(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 346, Ss, Stack, T, Ts, Tzr).

yeccpars2_329(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 330, Ss, Stack, T, Ts, Tzr).

yeccpars2_330(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 335, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 336, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_330(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_331(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_331_(Stack),
 yeccpars2(yeccgoto_indAudterminationStateParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_332(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 283, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(S, 'GREATER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 284, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 285, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(S, 'NEQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 286, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_332_(Stack),
 yeccpars2(yeccgoto_indAudterminationStateParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_333(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 344, Ss, Stack, T, Ts, Tzr).

yeccpars2_334(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_indAudterminationStateParm(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_335(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_335_(Stack),
 yeccpars2(yeccgoto_indAudterminationStateParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_336(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 337, Ss, Stack, T, Ts, Tzr);
yeccpars2_336(S, 'INEQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 338, Ss, Stack, T, Ts, Tzr);
yeccpars2_336(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_336_(Stack),
 yeccpars2(yeccgoto_iaServiceStates(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_337(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 340, Ss, Stack, T, Ts, Tzr);
yeccpars2_337(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 341, Ss, Stack, T, Ts, Tzr);
yeccpars2_337(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 342, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_338: see yeccpars2_337

yeccpars2_339(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_339_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_iaServiceStates(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_340(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_340_(Stack),
 yeccpars2(yeccgoto_serviceStatesValue(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_341_(Stack),
 yeccpars2(yeccgoto_serviceStatesValue(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_342(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_342_(Stack),
 yeccpars2(yeccgoto_serviceStatesValue(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_343(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_343_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_iaServiceStates(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_344(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_344_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_indAudterminationStateDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_345_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_propertyParm(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_346: see yeccpars2_4

yeccpars2_347(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 348, Ss, Stack, T, Ts, Tzr).

yeccpars2_348(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 324, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(S, 'LocalDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 325, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(S, 'RemoteDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 326, Ss, Stack, T, Ts, Tzr);
yeccpars2_348(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 327, Ss, Stack, T, Ts, Tzr).

yeccpars2_349(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 350, Ss, Stack, T, Ts, Tzr).

yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_350_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_indAudstreamDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_351(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 355, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 356, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 357, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_351(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_352(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_352_(Stack),
 yeccpars2(yeccgoto_indAudlocalParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_353(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 283, Ss, Stack, T, Ts, Tzr);
yeccpars2_353(S, 'GREATER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 284, Ss, Stack, T, Ts, Tzr);
yeccpars2_353(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 285, Ss, Stack, T, Ts, Tzr);
yeccpars2_353(S, 'NEQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 286, Ss, Stack, T, Ts, Tzr);
yeccpars2_353(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_353_(Stack),
 yeccpars2(yeccgoto_indAudlocalParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_354(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 368, Ss, Stack, T, Ts, Tzr);
yeccpars2_354(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_354_(Stack),
 yeccpars2(367, Cat, [354 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_355(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 358, Ss, Stack, T, Ts, Tzr);
yeccpars2_355(S, 'INEQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 359, Ss, Stack, T, Ts, Tzr);
yeccpars2_355(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_355_(Stack),
 yeccpars2(yeccgoto_indAudlocalParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_356(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_356_(Stack),
 yeccpars2(yeccgoto_indAudlocalParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_357(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_357_(Stack),
 yeccpars2(yeccgoto_indAudlocalParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_358(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 361, Ss, Stack, T, Ts, Tzr);
yeccpars2_358(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 362, Ss, Stack, T, Ts, Tzr);
yeccpars2_358(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 363, Ss, Stack, T, Ts, Tzr);
yeccpars2_358(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 364, Ss, Stack, T, Ts, Tzr);
yeccpars2_358(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 365, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_359: see yeccpars2_358

yeccpars2_360(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_360_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_indAudlocalParm(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_361(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_361_(Stack),
 yeccpars2(yeccgoto_streamModes(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_362(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_362_(Stack),
 yeccpars2(yeccgoto_streamModes(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_363(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_363_(Stack),
 yeccpars2(yeccgoto_streamModes(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_364(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_364_(Stack),
 yeccpars2(yeccgoto_streamModes(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_365(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_365_(Stack),
 yeccpars2(yeccgoto_streamModes(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_366(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_indAudlocalParm(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_367(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 371, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_368: see yeccpars2_351

yeccpars2_369(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 368, Ss, Stack, T, Ts, Tzr);
yeccpars2_369(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_369_(Stack),
 yeccpars2(370, Cat, [369 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_370(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_indAudlocalParmList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_371(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_371_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_indAudlocalControlDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_372(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 376, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_373: see yeccpars2_317

yeccpars2_374(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 373, Ss, Stack, T, Ts, Tzr);
yeccpars2_374(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_374_(Stack),
 yeccpars2(375, Cat, [374 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_375(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_375_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_indAudmediaParms(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_376(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_376_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_indAudmediaDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_377: see yeccpars2_4

%% yeccpars2_378: see yeccpars2_4

yeccpars2_379(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_indAudrequestedEvent(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_380(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 381, Ss, Stack, T, Ts, Tzr).

yeccpars2_381(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_381_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_indAudeventsDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_382(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 383, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_383: see yeccpars2_4

yeccpars2_384(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 385, Ss, Stack, T, Ts, Tzr).

yeccpars2_385(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_indAudeventsDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_386: see yeccpars2_4

yeccpars2_387(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 391, Ss, Stack, T, Ts, Tzr);
yeccpars2_387(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_387_(Stack),
 yeccpars2(390, Cat, [387 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_388(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 389, Ss, Stack, T, Ts, Tzr).

yeccpars2_389(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_389_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_indAudeventBufferDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_390(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_390_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_indAudeventSpec(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_391(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 171, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_391(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_392(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_392_(Stack),
 yeccpars2(yeccgoto_eventParameterName(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_393(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 396, Ss, Stack, T, Ts, Tzr).

yeccpars2_394(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_394_(Stack),
 yeccpars2(yeccgoto_indAudeventSpecParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_395(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_395_(Stack),
 yeccpars2(yeccgoto_indAudeventSpecParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_396(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_396_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_optIndAudeventSpecParameter(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_397(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_397_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_auditDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_398(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_398_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_auditDescriptorBody(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_399(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 207, Ss, Stack, T, Ts, Tzr);
yeccpars2_399(S, 'DigitMapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 208, Ss, Stack, T, Ts, Tzr);
yeccpars2_399(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 209, Ss, Stack, T, Ts, Tzr);
yeccpars2_399(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 210, Ss, Stack, T, Ts, Tzr);
yeccpars2_399(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 211, Ss, Stack, T, Ts, Tzr);
yeccpars2_399(S, 'ModemToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 212, Ss, Stack, T, Ts, Tzr);
yeccpars2_399(S, 'MuxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 213, Ss, Stack, T, Ts, Tzr);
yeccpars2_399(S, 'ObservedEventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 214, Ss, Stack, T, Ts, Tzr);
yeccpars2_399(S, 'PackagesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 215, Ss, Stack, T, Ts, Tzr);
yeccpars2_399(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 216, Ss, Stack, T, Ts, Tzr);
yeccpars2_399(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 217, Ss, Stack, T, Ts, Tzr).

yeccpars2_400(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 399, Ss, Stack, T, Ts, Tzr);
yeccpars2_400(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_400_(Stack),
 yeccpars2(401, Cat, [400 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_401(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_401_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_auditItemList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_402(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_402_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_indAudterminationAudit(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_403(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 207, Ss, Stack, T, Ts, Tzr);
yeccpars2_403(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 405, Ss, Stack, T, Ts, Tzr);
yeccpars2_403(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 406, Ss, Stack, T, Ts, Tzr);
yeccpars2_403(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 407, Ss, Stack, T, Ts, Tzr);
yeccpars2_403(S, 'PackagesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 408, Ss, Stack, T, Ts, Tzr);
yeccpars2_403(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 409, Ss, Stack, T, Ts, Tzr);
yeccpars2_403(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 327, Ss, Stack, T, Ts, Tzr).

yeccpars2_404(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 403, Ss, Stack, T, Ts, Tzr);
yeccpars2_404(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_404_(Stack),
 yeccpars2(410, Cat, [404 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_405(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 386, Ss, Stack, T, Ts, Tzr).

yeccpars2_406(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 377, Ss, Stack, T, Ts, Tzr);
yeccpars2_406(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 378, Ss, Stack, T, Ts, Tzr).

yeccpars2_407(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 317, Ss, Stack, T, Ts, Tzr).

yeccpars2_408(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 313, Ss, Stack, T, Ts, Tzr).

yeccpars2_409(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 223, Ss, Stack, T, Ts, Tzr).

yeccpars2_410(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_410_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_indAudterminationAuditList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_411(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_411_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_optAuditDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_412: see yeccpars2_180

yeccpars2_413(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 414, Ss, Stack, T, Ts, Tzr).

yeccpars2_414(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 416, Ss, Stack, T, Ts, Tzr).

yeccpars2_415(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 461, Ss, Stack, T, Ts, Tzr).

yeccpars2_416(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 417, Ss, Stack, T, Ts, Tzr).

yeccpars2_417(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 431, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 207, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'DigitMapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 208, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 209, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 210, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 211, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 432, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 433, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'ModemToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 212, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'MuxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 213, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'ObservedEventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 214, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'PackagesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 215, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 434, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 435, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 436, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'ServiceChangeIncompleteToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 437, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 216, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 217, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 438, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_417(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 439, Ss, Stack, T, Ts, Tzr).

yeccpars2_418(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_418_(Stack),
 yeccpars2(yeccgoto_serviceChangeParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_419(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_419_(Stack),
 yeccpars2(yeccgoto_serviceChangeParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_420(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_420_(Stack),
 yeccpars2(yeccgoto_serviceChangeParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_421(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_421_(Stack),
 yeccpars2(yeccgoto_serviceChangeParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_422(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 457, Ss, Stack, T, Ts, Tzr);
yeccpars2_422(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_422_(Stack),
 yeccpars2(456, Cat, [422 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_423(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_423_(Stack),
 yeccpars2(yeccgoto_serviceChangeParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_424(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_424_(Stack),
 yeccpars2(yeccgoto_serviceChangeParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_425(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_425_(Stack),
 yeccpars2(yeccgoto_serviceChangeParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_426(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_426_(Stack),
 yeccpars2(yeccgoto_serviceChangeParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_427(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_427_(Stack),
 yeccpars2(yeccgoto_extensionParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_428: see yeccpars2_241

yeccpars2_429(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_429_(Stack),
 yeccpars2(yeccgoto_serviceChangeParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_430(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_430_(Stack),
 yeccpars2(yeccgoto_serviceChangeParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_431(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 453, Ss, Stack, T, Ts, Tzr);
yeccpars2_431(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_431_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_432(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 451, Ss, Stack, T, Ts, Tzr);
yeccpars2_432(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_432_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_433(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 449, Ss, Stack, T, Ts, Tzr);
yeccpars2_433(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_433_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_434(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 447, Ss, Stack, T, Ts, Tzr);
yeccpars2_434(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_434_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_435(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 445, Ss, Stack, T, Ts, Tzr);
yeccpars2_435(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_435_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_436(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 442, Ss, Stack, T, Ts, Tzr);
yeccpars2_436(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_436_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_437(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_437_(Stack),
 yeccpars2(yeccgoto_serviceChangeParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_438(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_438_(Stack),
 yeccpars2(yeccgoto_timeStamp(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_439(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 440, Ss, Stack, T, Ts, Tzr);
yeccpars2_439(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_439_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_440: see yeccpars2_4

yeccpars2_441(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_441_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_serviceChangeVersion(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_442(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_442_(Stack),
 yeccpars2(88, Cat, [442 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_443(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_443_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_serviceChangeAddress(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_444(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_444_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_serviceChangeAddress(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_445: see yeccpars2_284

yeccpars2_446(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_446_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_serviceChangeReason(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_447: see yeccpars2_4

yeccpars2_448(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_448_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_serviceChangeProfile(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_449(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_449(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_449(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_449(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_449_(Stack),
 yeccpars2(88, Cat, [449 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_450(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_450_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_serviceChangeMgcId(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_451: see yeccpars2_4

yeccpars2_452(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_452_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_serviceChangeMethod(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_453: see yeccpars2_4

yeccpars2_454(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_454_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_serviceChangeDelay(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_455(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_455_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_extension(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_456(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 460, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_457: see yeccpars2_417

yeccpars2_458(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 457, Ss, Stack, T, Ts, Tzr);
yeccpars2_458(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_458_(Stack),
 yeccpars2(459, Cat, [458 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_459(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_459_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_serviceChangeParms(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_460(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_460_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_serviceChangeDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_461(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_461_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_serviceChangeRequest(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_462: see yeccpars2_4

yeccpars2_463(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_463_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_priority(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_464: see yeccpars2_180

yeccpars2_465(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 466, Ss, Stack, T, Ts, Tzr).

yeccpars2_466(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_466(S, 'ObservedEventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 470, Ss, Stack, T, Ts, Tzr).

yeccpars2_467(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_467_(Stack),
 yeccpars2(yeccgoto_notifyRequestBody(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_468(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 499, Ss, Stack, T, Ts, Tzr).

yeccpars2_469(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_469_(Stack),
 yeccpars2(yeccgoto_notifyRequestBody(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_470(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 471, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_471: see yeccpars2_4

yeccpars2_472(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 473, Ss, Stack, T, Ts, Tzr).

yeccpars2_473(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_473(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 438, Ss, Stack, T, Ts, Tzr);
yeccpars2_473(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_473_(Stack),
 yeccpars2(475, Cat, [473 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_474(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_474(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_474_(Stack),
 yeccpars2(494, Cat, [474 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_475: see yeccpars2_4

yeccpars2_476(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 478, Ss, Stack, T, Ts, Tzr);
yeccpars2_476(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_476_(Stack),
 yeccpars2(477, Cat, [476 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_477(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 481, Ss, Stack, T, Ts, Tzr).

yeccpars2_478(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_478(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 438, Ss, Stack, T, Ts, Tzr);
yeccpars2_478(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_478_(Stack),
 yeccpars2(475, Cat, [478 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_479(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 478, Ss, Stack, T, Ts, Tzr);
yeccpars2_479(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_479_(Stack),
 yeccpars2(480, Cat, [479 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_480(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_480_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_observedEvents(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_481(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_481_(Stack),
 Nss = lists:nthtail(6, Ss),
 yeccpars2(yeccgoto_observedEventsDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_482(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 484, Ss, Stack, T, Ts, Tzr);
yeccpars2_482(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_482_(Stack),
 yeccpars2(483, Cat, [482 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_483(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_483_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_observedEvent(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_484: see yeccpars2_4

yeccpars2_485(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 490, Ss, Stack, T, Ts, Tzr);
yeccpars2_485(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_485_(Stack),
 yeccpars2(489, Cat, [485 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_486(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_observedEventParameter(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_487: see yeccpars2_241

yeccpars2_488(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_488_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_eventStreamOrOther(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_489(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 493, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_490: see yeccpars2_4

yeccpars2_491(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 490, Ss, Stack, T, Ts, Tzr);
yeccpars2_491(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_491_(Stack),
 yeccpars2(492, Cat, [491 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_492(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_492_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_observedEventParameters(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_493(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_493_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_observedEventBody(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_494(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 495, Ss, Stack, T, Ts, Tzr).

yeccpars2_495(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_495(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_495_(Stack),
 yeccpars2(496, Cat, [495 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_496: see yeccpars2_4

yeccpars2_497(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 484, Ss, Stack, T, Ts, Tzr);
yeccpars2_497(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_497_(Stack),
 yeccpars2(498, Cat, [497 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_498(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_498_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_observedEvent(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_499(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_499_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_notifyRequest(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_500(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 502, Ss, Stack, T, Ts, Tzr);
yeccpars2_500(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 503, Ss, Stack, T, Ts, Tzr).

yeccpars2_501(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_501_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_iepsValue(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_502(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_502_(Stack),
 yeccpars2(yeccgoto_onOrOff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_503(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_503_(Stack),
 yeccpars2(yeccgoto_onOrOff(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_504(S, 'AndAUDITselectToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 514, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'ContextAttrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 515, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'EmergencyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 516, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'EmergencyValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 517, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'IEPSToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 518, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'OrAUDITselectToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 519, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'PriorityToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 520, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'TopologyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 521, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_505(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_505_(Stack),
 yeccpars2(yeccgoto_contextAuditSelector(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_506(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_506_(Stack),
 yeccpars2(yeccgoto_contextAuditProperty(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_507(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 554, Ss, Stack, T, Ts, Tzr).

yeccpars2_508(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_508_(Stack),
 yeccpars2(yeccgoto_contextAuditSelector(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_509(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_509_(Stack),
 yeccpars2(yeccgoto_contextAuditSelector(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_510(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_contextAuditProperty(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_511(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 541, Ss, Stack, T, Ts, Tzr);
yeccpars2_511(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_511_(Stack),
 yeccpars2(552, Cat, [511 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_512(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_contextAuditSelector(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_513(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_513_(Stack),
 yeccpars2(yeccgoto_contextAuditSelector(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_514(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_514_(Stack),
 yeccpars2(yeccgoto_auditSelectLogic(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_515(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 525, Ss, Stack, T, Ts, Tzr).

yeccpars2_516(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_516_(Stack),
 yeccpars2(yeccgoto_contextAuditProperty(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_517(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 522, Ss, Stack, T, Ts, Tzr).

yeccpars2_518(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 500, Ss, Stack, T, Ts, Tzr);
yeccpars2_518(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_518_(Stack),
 yeccpars2(yeccgoto_contextAuditProperty(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_519(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_519_(Stack),
 yeccpars2(yeccgoto_auditSelectLogic(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_520(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 462, Ss, Stack, T, Ts, Tzr);
yeccpars2_520(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_520_(Stack),
 yeccpars2(yeccgoto_contextAuditProperty(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_521(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_521_(Stack),
 yeccpars2(yeccgoto_contextAuditProperty(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_522(S, 'EmergencyOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 523, Ss, Stack, T, Ts, Tzr);
yeccpars2_522(S, 'EmergencyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 524, Ss, Stack, T, Ts, Tzr).

yeccpars2_523(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_523_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_emergencyValue(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_524(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_524_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_emergencyValue(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_525(S, 'AndAUDITselectToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 514, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'ContextAttrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'ContextListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 531, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'EmergencyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 516, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'EmergencyValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 517, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'IEPSToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 518, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'OrAUDITselectToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 519, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'PriorityToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 520, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'TopologyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 521, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_526(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 551, Ss, Stack, T, Ts, Tzr).

yeccpars2_527(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 547, Ss, Stack, T, Ts, Tzr);
yeccpars2_527(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_527_(Stack),
 yeccpars2(546, Cat, [527 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_528(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 283, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'GREATER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 284, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 285, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(S, 'NEQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 286, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_528_(Stack),
 yeccpars2(yeccgoto_contextAuditProperty(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_529(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 545, Ss, Stack, T, Ts, Tzr).

yeccpars2_530(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 541, Ss, Stack, T, Ts, Tzr);
yeccpars2_530(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_530_(Stack),
 yeccpars2(540, Cat, [530 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_531(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 532, Ss, Stack, T, Ts, Tzr).

yeccpars2_532(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 533, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_533: see yeccpars2_4

yeccpars2_534(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 536, Ss, Stack, T, Ts, Tzr);
yeccpars2_534(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_534_(Stack),
 yeccpars2(535, Cat, [534 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_535(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 539, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_536: see yeccpars2_4

yeccpars2_537(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 536, Ss, Stack, T, Ts, Tzr);
yeccpars2_537(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_537_(Stack),
 yeccpars2(538, Cat, [537 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_538(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_538_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_contextIDs(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_539(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_539_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_contextIdList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_540(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 544, Ss, Stack, T, Ts, Tzr).

yeccpars2_541(S, 'AndAUDITselectToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 514, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'ContextAttrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'EmergencyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 516, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'EmergencyValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 517, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'IEPSToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 518, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'OrAUDITselectToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 519, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'PriorityToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 520, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'TopologyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 521, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_542(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 541, Ss, Stack, T, Ts, Tzr);
yeccpars2_542(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_542_(Stack),
 yeccpars2(543, Cat, [542 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_543(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_543_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_contextAuditProperties(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_544(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_544_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_indAudcontextAttrDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_545(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_545_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_contextAttrDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_546(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_546_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_propertyParms(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_547: see yeccpars2_4

yeccpars2_548(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 547, Ss, Stack, T, Ts, Tzr);
yeccpars2_548(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_548_(Stack),
 yeccpars2(550, Cat, [548 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_549: see yeccpars2_241

yeccpars2_550(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_550_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_propertyParmList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_551(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_551_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_contextAttrDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_552(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 553, Ss, Stack, T, Ts, Tzr).

yeccpars2_553(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_553_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_contextAudit(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_554(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_554_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_contextAudit(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_555(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'ContextListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 531, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_556: see yeccpars2_180

yeccpars2_557(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_557_(Stack),
 yeccpars2(558, Cat, [557 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_558(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_558_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_auditRequest(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_559: see yeccpars2_180

yeccpars2_560(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_560(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_560_(Stack),
 yeccpars2(561, Cat, [560 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_561(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_561_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_auditRequest(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_562(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_562_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_actionRequest(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_563(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_563_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_actionRequestBody(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_564: see yeccpars2_129

yeccpars2_565(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 564, Ss, Stack, T, Ts, Tzr);
yeccpars2_565(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_565_(Stack),
 yeccpars2(566, Cat, [565 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_566(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_566_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_actionRequestItems(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_567: see yeccpars2_180

yeccpars2_568(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 570, Ss, Stack, T, Ts, Tzr);
yeccpars2_568(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_568_(Stack),
 yeccpars2(569, Cat, [568 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_569(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_569_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_ammRequest(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_570(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_570(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 581, Ss, Stack, T, Ts, Tzr);
yeccpars2_570(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 582, Ss, Stack, T, Ts, Tzr);
yeccpars2_570(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 583, Ss, Stack, T, Ts, Tzr);
yeccpars2_570(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 584, Ss, Stack, T, Ts, Tzr);
yeccpars2_570(S, 'ModemToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 585, Ss, Stack, T, Ts, Tzr);
yeccpars2_570(S, 'MuxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 586, Ss, Stack, T, Ts, Tzr);
yeccpars2_570(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 587, Ss, Stack, T, Ts, Tzr);
yeccpars2_570(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 588, Ss, Stack, T, Ts, Tzr).

yeccpars2_571(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_571_(Stack),
 yeccpars2(yeccgoto_ammParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_572(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_572_(Stack),
 yeccpars2(yeccgoto_ammParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_573(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_573_(Stack),
 yeccpars2(yeccgoto_ammParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_574(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_574_(Stack),
 yeccpars2(yeccgoto_ammParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_575(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_575_(Stack),
 yeccpars2(yeccgoto_ammParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_576(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_576_(Stack),
 yeccpars2(yeccgoto_ammParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_577(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_577_(Stack),
 yeccpars2(yeccgoto_ammParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_578(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_578_(Stack),
 yeccpars2(yeccgoto_ammParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_579(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_579_(Stack),
 yeccpars2(yeccgoto_ammParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_580(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 790, Ss, Stack, T, Ts, Tzr);
yeccpars2_580(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_580_(Stack),
 yeccpars2(789, Cat, [580 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_581(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_581_(Stack),
 yeccpars2(yeccgoto_digitMapDescriptor(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_582(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 781, Ss, Stack, T, Ts, Tzr);
yeccpars2_582(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_582_(Stack),
 yeccpars2(yeccgoto_eventBufferDescriptor(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_583(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 707, Ss, Stack, T, Ts, Tzr);
yeccpars2_583(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_583_(Stack),
 yeccpars2(yeccgoto_eventsDescriptor(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_584(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 646, Ss, Stack, T, Ts, Tzr).

yeccpars2_585(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 630, Ss, Stack, T, Ts, Tzr);
yeccpars2_585(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 631, Ss, Stack, T, Ts, Tzr).

yeccpars2_586(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 622, Ss, Stack, T, Ts, Tzr).

yeccpars2_587(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 603, Ss, Stack, T, Ts, Tzr);
yeccpars2_587(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_587_(Stack),
 yeccpars2(yeccgoto_signalsDescriptor(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_588(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 589, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_589: see yeccpars2_4

yeccpars2_590(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 599, Ss, Stack, T, Ts, Tzr);
yeccpars2_590(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_590_(Stack),
 yeccpars2(598, Cat, [590 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_591(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 592, Ss, Stack, T, Ts, Tzr);
yeccpars2_591(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_591_(Stack),
 yeccpars2(yeccgoto_statisticsParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_592(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 594, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'QuotedChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 289, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_593(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_593_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_statisticsParameter(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_594: see yeccpars2_284

yeccpars2_595(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 299, Ss, Stack, T, Ts, Tzr);
yeccpars2_595(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_595_(Stack),
 yeccpars2(596, Cat, [595 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_596(S, 'RSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 597, Ss, Stack, T, Ts, Tzr).

yeccpars2_597(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_597_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_statisticsParameter(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_598(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 602, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_599: see yeccpars2_4

yeccpars2_600(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 599, Ss, Stack, T, Ts, Tzr);
yeccpars2_600(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_600_(Stack),
 yeccpars2(601, Cat, [600 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_601(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_601_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_statisticsParameters(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_602(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_602_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_statisticsDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_603(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 607, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_604(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_604_(Stack),
 yeccpars2(yeccgoto_signalParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_605(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 618, Ss, Stack, T, Ts, Tzr);
yeccpars2_605(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_605_(Stack),
 yeccpars2(617, Cat, [605 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_606(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_606_(Stack),
 yeccpars2(yeccgoto_signalParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_607(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 608, Ss, Stack, T, Ts, Tzr);
yeccpars2_607(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_607_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_608: see yeccpars2_4

yeccpars2_609(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 610, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_610: see yeccpars2_4

yeccpars2_611(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 613, Ss, Stack, T, Ts, Tzr);
yeccpars2_611(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_611_(Stack),
 yeccpars2(612, Cat, [611 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_612(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 616, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_613: see yeccpars2_4

yeccpars2_614(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 613, Ss, Stack, T, Ts, Tzr);
yeccpars2_614(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_614_(Stack),
 yeccpars2(615, Cat, [614 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_615(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_615_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_signalListParms(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_616(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_616_(Stack),
 Nss = lists:nthtail(6, Ss),
 yeccpars2(yeccgoto_signalList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_617(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 621, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_618: see yeccpars2_603

yeccpars2_619(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 618, Ss, Stack, T, Ts, Tzr);
yeccpars2_619(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_619_(Stack),
 yeccpars2(620, Cat, [619 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_620(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_620_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_signalParms(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_621(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_621_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_signalsDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_622: see yeccpars2_4

yeccpars2_623(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_623_(Stack),
 yeccpars2(yeccgoto_muxType(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_624(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 626, Ss, Stack, T, Ts, Tzr).

yeccpars2_625(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_625_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_muxDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_626: see yeccpars2_4

yeccpars2_627(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_627(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_627_(Stack),
 yeccpars2(628, Cat, [627 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_628(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 629, Ss, Stack, T, Ts, Tzr).

yeccpars2_629(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_629_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_terminationIDList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_630: see yeccpars2_4

%% yeccpars2_631: see yeccpars2_4

yeccpars2_632(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_632_(Stack),
 yeccpars2(yeccgoto_modemType(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_633(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 635, Ss, Stack, T, Ts, Tzr);
yeccpars2_633(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_633_(Stack),
 yeccpars2(634, Cat, [633 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_634(S, 'RSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 638, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_635: see yeccpars2_4

yeccpars2_636(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 635, Ss, Stack, T, Ts, Tzr);
yeccpars2_636(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_636_(Stack),
 yeccpars2(637, Cat, [636 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_637(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_637_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_modemTypeList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_638(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 640, Ss, Stack, T, Ts, Tzr);
yeccpars2_638(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_638_(Stack),
 yeccpars2(639, Cat, [638 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_639(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_639_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_modemDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_640: see yeccpars2_4

yeccpars2_641(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 547, Ss, Stack, T, Ts, Tzr);
yeccpars2_641(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_641_(Stack),
 yeccpars2(642, Cat, [641 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_642(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 643, Ss, Stack, T, Ts, Tzr).

yeccpars2_643(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_643_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_optPropertyParms(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_644(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 640, Ss, Stack, T, Ts, Tzr);
yeccpars2_644(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_644_(Stack),
 yeccpars2(645, Cat, [644 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_645(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_645_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_modemDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_646(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 653, Ss, Stack, T, Ts, Tzr);
yeccpars2_646(S, 'LocalDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 654, Ss, Stack, T, Ts, Tzr);
yeccpars2_646(S, 'RemoteDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 655, Ss, Stack, T, Ts, Tzr);
yeccpars2_646(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 588, Ss, Stack, T, Ts, Tzr);
yeccpars2_646(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 656, Ss, Stack, T, Ts, Tzr);
yeccpars2_646(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 657, Ss, Stack, T, Ts, Tzr).

yeccpars2_647(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_647_(Stack),
 yeccpars2(yeccgoto_mediaParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_648(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_648_(Stack),
 yeccpars2(yeccgoto_mediaParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_649(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_649_(Stack),
 yeccpars2(yeccgoto_mediaParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_650(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_650_(Stack),
 yeccpars2(yeccgoto_streamParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_651(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 703, Ss, Stack, T, Ts, Tzr);
yeccpars2_651(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_651_(Stack),
 yeccpars2(702, Cat, [651 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_652(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_652_(Stack),
 yeccpars2(yeccgoto_streamParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_653(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 685, Ss, Stack, T, Ts, Tzr).

yeccpars2_654(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_654_(Stack),
 yeccpars2(yeccgoto_streamParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_655(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_655_(Stack),
 yeccpars2(yeccgoto_streamParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_656(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 676, Ss, Stack, T, Ts, Tzr).

yeccpars2_657(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 658, Ss, Stack, T, Ts, Tzr).

yeccpars2_658(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 663, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 664, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_659(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 672, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_659_(Stack),
 yeccpars2(671, Cat, [659 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_660(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_660_(Stack),
 yeccpars2(yeccgoto_terminationStateParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_661(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_661_(Stack),
 yeccpars2(yeccgoto_terminationStateParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_662(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_662_(Stack),
 yeccpars2(yeccgoto_terminationStateParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_663(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 667, Ss, Stack, T, Ts, Tzr).

yeccpars2_664(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 665, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_665: see yeccpars2_337

yeccpars2_666(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_666_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_serviceStates(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_667(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 669, Ss, Stack, T, Ts, Tzr);
yeccpars2_667(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 670, Ss, Stack, T, Ts, Tzr).

yeccpars2_668(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_668_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_eventBufferControl(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_669(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_669_(Stack),
 yeccpars2(yeccgoto_eventBufferControlValue(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_670(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_670_(Stack),
 yeccpars2(yeccgoto_eventBufferControlValue(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_671(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 675, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_672: see yeccpars2_658

yeccpars2_673(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 672, Ss, Stack, T, Ts, Tzr);
yeccpars2_673(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_673_(Stack),
 yeccpars2(674, Cat, [673 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_674(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_674_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_terminationStateParms(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_675(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_675_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_terminationStateDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_676: see yeccpars2_4

yeccpars2_677(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 678, Ss, Stack, T, Ts, Tzr).

yeccpars2_678(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 653, Ss, Stack, T, Ts, Tzr);
yeccpars2_678(S, 'LocalDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 654, Ss, Stack, T, Ts, Tzr);
yeccpars2_678(S, 'RemoteDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 655, Ss, Stack, T, Ts, Tzr);
yeccpars2_678(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 588, Ss, Stack, T, Ts, Tzr).

yeccpars2_679(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 681, Ss, Stack, T, Ts, Tzr);
yeccpars2_679(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_679_(Stack),
 yeccpars2(680, Cat, [679 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_680(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 684, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_681: see yeccpars2_678

yeccpars2_682(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 681, Ss, Stack, T, Ts, Tzr);
yeccpars2_682(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_682_(Stack),
 yeccpars2(683, Cat, [682 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_683(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_683_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_streamParmList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_684(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_684_(Stack),
 Nss = lists:nthtail(6, Ss),
 yeccpars2(yeccgoto_streamDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_685(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 688, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 689, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 690, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_685(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_686(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_686_(Stack),
 yeccpars2(yeccgoto_localParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_687(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 698, Ss, Stack, T, Ts, Tzr);
yeccpars2_687(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_687_(Stack),
 yeccpars2(697, Cat, [687 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_688(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 695, Ss, Stack, T, Ts, Tzr).

yeccpars2_689(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 693, Ss, Stack, T, Ts, Tzr).

yeccpars2_690(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 691, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_691: see yeccpars2_500

yeccpars2_692(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_692_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_localParm(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_693: see yeccpars2_500

yeccpars2_694(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_694_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_localParm(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_695: see yeccpars2_358

yeccpars2_696(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_696_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_localParm(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_697(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 701, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_698: see yeccpars2_685

yeccpars2_699(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 698, Ss, Stack, T, Ts, Tzr);
yeccpars2_699(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_699_(Stack),
 yeccpars2(700, Cat, [699 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_700(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_700_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_localParmList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_701(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_701_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_localControlDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_702(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 706, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_703: see yeccpars2_646

yeccpars2_704(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 703, Ss, Stack, T, Ts, Tzr);
yeccpars2_704(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_704_(Stack),
 yeccpars2(705, Cat, [704 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_705(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_705_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_mediaParmList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_706(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_706_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_mediaDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_707: see yeccpars2_4

yeccpars2_708(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 709, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_709: see yeccpars2_4

yeccpars2_710(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 777, Ss, Stack, T, Ts, Tzr);
yeccpars2_710(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_710_(Stack),
 yeccpars2(776, Cat, [710 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_711(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 713, Ss, Stack, T, Ts, Tzr);
yeccpars2_711(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_711_(Stack),
 yeccpars2(712, Cat, [711 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_712(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_712_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_requestedEvent(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_713(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 721, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 722, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 723, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'NeverNotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 724, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'NotifyImmediateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 725, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'NotifyRegulatedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 726, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'ResetEventsDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 727, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_714(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_714_(Stack),
 yeccpars2(yeccgoto_notifyBehaviour(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_715(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_715_(Stack),
 yeccpars2(yeccgoto_eventParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_716(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_eventParameter(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_717(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 772, Ss, Stack, T, Ts, Tzr);
yeccpars2_717(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_717_(Stack),
 yeccpars2(771, Cat, [717 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_718(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_eventParameter(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_719(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_eventParameter(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_720(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_eventParameter(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_721(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_721_(Stack),
 yeccpars2(yeccgoto_eventDM(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_722(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 732, Ss, Stack, T, Ts, Tzr);
yeccpars2_722(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_722_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_723(_S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_723_COMMA(Stack),
 yeccpars2(yeccgoto_eventParameter(hd(Ss)), 'COMMA', Ss, NewStack, T, Ts, Tzr);
yeccpars2_723(_S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_723_RBRKT(Stack),
 yeccpars2(yeccgoto_eventParameter(hd(Ss)), 'RBRKT', Ss, NewStack, T, Ts, Tzr);
yeccpars2_723(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_723_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_724(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_724_(Stack),
 yeccpars2(yeccgoto_notifyBehaviour(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_725(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_725_(Stack),
 yeccpars2(yeccgoto_notifyBehaviour(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_726(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 728, Ss, Stack, T, Ts, Tzr);
yeccpars2_726(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_726_(Stack),
 yeccpars2(yeccgoto_notifyRegulated(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_727(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_727_(Stack),
 yeccpars2(yeccgoto_eventParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_728(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 731, Ss, Stack, T, Ts, Tzr).

yeccpars2_729(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 770, Ss, Stack, T, Ts, Tzr).

yeccpars2_730(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 769, Ss, Stack, T, Ts, Tzr).

yeccpars2_731(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 732, Ss, Stack, T, Ts, Tzr).

yeccpars2_732(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 735, Ss, Stack, T, Ts, Tzr);
yeccpars2_732(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 587, Ss, Stack, T, Ts, Tzr).

yeccpars2_733(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 765, Ss, Stack, T, Ts, Tzr);
yeccpars2_733(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 766, Ss, Stack, T, Ts, Tzr).

yeccpars2_734(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 764, Ss, Stack, T, Ts, Tzr).

yeccpars2_735(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 736, Ss, Stack, T, Ts, Tzr);
yeccpars2_735(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_735_(Stack),
 yeccpars2(yeccgoto_embedFirst(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_736: see yeccpars2_4

yeccpars2_737(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 738, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_738: see yeccpars2_4

yeccpars2_739(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 760, Ss, Stack, T, Ts, Tzr);
yeccpars2_739(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_739_(Stack),
 yeccpars2(759, Cat, [739 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_740(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 742, Ss, Stack, T, Ts, Tzr);
yeccpars2_740(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_740_(Stack),
 yeccpars2(741, Cat, [740 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_741(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_741_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_secondRequestedEvent(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_742(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 721, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 748, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 749, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'NeverNotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 724, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'NotifyImmediateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 725, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'NotifyRegulatedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 726, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'ResetEventsDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 750, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_743(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 755, Ss, Stack, T, Ts, Tzr);
yeccpars2_743(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_743_(Stack),
 yeccpars2(754, Cat, [743 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_744(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_744_(Stack),
 yeccpars2(yeccgoto_secondEventParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_745(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_secondEventParameter(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_746(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_secondEventParameter(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_747(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_secondEventParameter(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_748(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 751, Ss, Stack, T, Ts, Tzr);
yeccpars2_748(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_748_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_749(_S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_749_COMMA(Stack),
 yeccpars2(yeccgoto_secondEventParameter(hd(Ss)), 'COMMA', Ss, NewStack, T, Ts, Tzr);
yeccpars2_749(_S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_749_RBRKT(Stack),
 yeccpars2(yeccgoto_secondEventParameter(hd(Ss)), 'RBRKT', Ss, NewStack, T, Ts, Tzr);
yeccpars2_749(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_749_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_750(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_750_(Stack),
 yeccpars2(yeccgoto_secondEventParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_751(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 587, Ss, Stack, T, Ts, Tzr).

yeccpars2_752(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 753, Ss, Stack, T, Ts, Tzr).

yeccpars2_753(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_753_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_embedSig(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_754(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 758, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_755: see yeccpars2_742

yeccpars2_756(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 755, Ss, Stack, T, Ts, Tzr);
yeccpars2_756(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_756_(Stack),
 yeccpars2(757, Cat, [756 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_757(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_757_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_secondEventParameters(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_758(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_758_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_secondRequestedEventBody(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_759(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 763, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_760: see yeccpars2_4

yeccpars2_761(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 760, Ss, Stack, T, Ts, Tzr);
yeccpars2_761(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_761_(Stack),
 yeccpars2(762, Cat, [761 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_762(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_762_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_secondRequestedEvents(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_763(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_763_(Stack),
 Nss = lists:nthtail(6, Ss),
 yeccpars2(yeccgoto_embedFirst(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_764(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_764_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_embedNoSig(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_765(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 735, Ss, Stack, T, Ts, Tzr).

yeccpars2_766(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_766_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_embedWithSig(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_767(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 768, Ss, Stack, T, Ts, Tzr).

yeccpars2_768(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_768_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_embedWithSig(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_769(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_769_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_notifyRegulated(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_770(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_770_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_notifyRegulated(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_771(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 775, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_772: see yeccpars2_713

yeccpars2_773(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 772, Ss, Stack, T, Ts, Tzr);
yeccpars2_773(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_773_(Stack),
 yeccpars2(774, Cat, [773 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_774(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_774_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_eventParameters(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_775(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_775_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_requestedEventBody(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_776(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 780, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_777: see yeccpars2_4

yeccpars2_778(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 777, Ss, Stack, T, Ts, Tzr);
yeccpars2_778(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_778_(Stack),
 yeccpars2(779, Cat, [778 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_779(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_779_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_requestedEvents(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_780(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_780_(Stack),
 Nss = lists:nthtail(6, Ss),
 yeccpars2(yeccgoto_eventsDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_781(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_781(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 438, Ss, Stack, T, Ts, Tzr);
yeccpars2_781(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_781_(Stack),
 yeccpars2(475, Cat, [781 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_782(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_782_(Stack),
 yeccpars2(yeccgoto_eventSpec(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_783(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 785, Ss, Stack, T, Ts, Tzr);
yeccpars2_783(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_783_(Stack),
 yeccpars2(784, Cat, [783 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_784(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 788, Ss, Stack, T, Ts, Tzr).

yeccpars2_785(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_785(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 438, Ss, Stack, T, Ts, Tzr);
yeccpars2_785(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_785_(Stack),
 yeccpars2(475, Cat, [785 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_786(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 785, Ss, Stack, T, Ts, Tzr);
yeccpars2_786(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_786_(Stack),
 yeccpars2(787, Cat, [786 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_787(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_787_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_eventSpecList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_788(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_788_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_eventBufferDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_789(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 793, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_790: see yeccpars2_570

yeccpars2_791(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 790, Ss, Stack, T, Ts, Tzr);
yeccpars2_791(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_791_(Stack),
 yeccpars2(792, Cat, [791 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_792(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_792_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_ammParameters(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_793(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_793_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_ammRequestBody(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_794(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 798, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_795: see yeccpars2_123

yeccpars2_796(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 795, Ss, Stack, T, Ts, Tzr);
yeccpars2_796(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_796_(Stack),
 yeccpars2(797, Cat, [796 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_797(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_797_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_actionRequestList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_798(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_798_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_transactionRequest(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_799(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 805, Ss, Stack, T, Ts, Tzr).

yeccpars2_800(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_800_(Stack),
 yeccpars2(yeccgoto_transactionID(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_801: see yeccpars2_123

yeccpars2_802(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 795, Ss, Stack, T, Ts, Tzr);
yeccpars2_802(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_802_(Stack),
 yeccpars2(803, Cat, [802 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_803(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 804, Ss, Stack, T, Ts, Tzr).

yeccpars2_804(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_804_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_transactionRequest(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_805: see yeccpars2_123

yeccpars2_806(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 795, Ss, Stack, T, Ts, Tzr);
yeccpars2_806(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_806_(Stack),
 yeccpars2(807, Cat, [806 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_807(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 808, Ss, Stack, T, Ts, Tzr).

yeccpars2_808(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_808_(Stack),
 Nss = lists:nthtail(6, Ss),
 yeccpars2(yeccgoto_transactionRequest(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_809: see yeccpars2_4

yeccpars2_810(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 813, Ss, Stack, T, Ts, Tzr);
yeccpars2_810(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_810_(Stack),
 yeccpars2(812, Cat, [810 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_811(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_811_(Stack),
 yeccpars2(yeccgoto_transactionAck(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_812(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 816, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_813: see yeccpars2_4

yeccpars2_814(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 813, Ss, Stack, T, Ts, Tzr);
yeccpars2_814(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_814_(Stack),
 yeccpars2(815, Cat, [814 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_815(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_815_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_transactionAckList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_816(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_816_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_transactionResponseAck(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_817: see yeccpars2_4

yeccpars2_818(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 819, Ss, Stack, T, Ts, Tzr).

yeccpars2_819(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 821, Ss, Stack, T, Ts, Tzr);
yeccpars2_819(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_819_(Stack),
 yeccpars2(820, Cat, [819 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_820(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 826, Ss, Stack, T, Ts, Tzr);
yeccpars2_820(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr).

yeccpars2_821(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 822, Ss, Stack, T, Ts, Tzr).

yeccpars2_822(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_822_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_optImmAckRequired(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_823(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 948, Ss, Stack, T, Ts, Tzr).

yeccpars2_824(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_824_(Stack),
 yeccpars2(yeccgoto_transactionReplyBody(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_825(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 945, Ss, Stack, T, Ts, Tzr);
yeccpars2_825(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_825_(Stack),
 yeccpars2(944, Cat, [825 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_826(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 827, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_827: see yeccpars2_4

yeccpars2_828(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 829, Ss, Stack, T, Ts, Tzr);
yeccpars2_828(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_828_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_actionReply(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_829(S, 'AddToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 839, Ss, Stack, T, Ts, Tzr);
yeccpars2_829(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 840, Ss, Stack, T, Ts, Tzr);
yeccpars2_829(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 841, Ss, Stack, T, Ts, Tzr);
yeccpars2_829(S, 'ContextAttrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr);
yeccpars2_829(S, 'EmergencyOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 150, Ss, Stack, T, Ts, Tzr);
yeccpars2_829(S, 'EmergencyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 151, Ss, Stack, T, Ts, Tzr);
yeccpars2_829(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_829(S, 'IEPSToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 152, Ss, Stack, T, Ts, Tzr);
yeccpars2_829(S, 'ModifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 842, Ss, Stack, T, Ts, Tzr);
yeccpars2_829(S, 'MoveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 843, Ss, Stack, T, Ts, Tzr);
yeccpars2_829(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 844, Ss, Stack, T, Ts, Tzr);
yeccpars2_829(S, 'PriorityToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_829(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 845, Ss, Stack, T, Ts, Tzr);
yeccpars2_829(S, 'SubtractToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 846, Ss, Stack, T, Ts, Tzr);
yeccpars2_829(S, 'TopologyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 159, Ss, Stack, T, Ts, Tzr).

yeccpars2_830(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_830_(Stack),
 yeccpars2(yeccgoto_commandReplys(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_831(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_831_(Stack),
 yeccpars2(yeccgoto_commandReplys(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_832(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_832_(Stack),
 yeccpars2(yeccgoto_actionReplyBody(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_833(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_833_(Stack),
 yeccpars2(yeccgoto_commandReplys(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_834(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 940, Ss, Stack, T, Ts, Tzr);
yeccpars2_834(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_834_(Stack),
 yeccpars2(939, Cat, [834 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_835(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_835_(Stack),
 yeccpars2(yeccgoto_commandReplys(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_836(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 933, Ss, Stack, T, Ts, Tzr).

yeccpars2_837(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_837_(Stack),
 yeccpars2(yeccgoto_commandReplys(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_838(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 932, Ss, Stack, T, Ts, Tzr).

yeccpars2_839(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_839_(Stack),
 yeccpars2(yeccgoto_ammsToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_840(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 928, Ss, Stack, T, Ts, Tzr).

yeccpars2_841(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 878, Ss, Stack, T, Ts, Tzr).

yeccpars2_842(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_842_(Stack),
 yeccpars2(yeccgoto_ammsToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_843(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_843_(Stack),
 yeccpars2(yeccgoto_ammsToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_844(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 872, Ss, Stack, T, Ts, Tzr).

yeccpars2_845(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 847, Ss, Stack, T, Ts, Tzr).

yeccpars2_846(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_846_(Stack),
 yeccpars2(yeccgoto_ammsToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_847: see yeccpars2_180

yeccpars2_848(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 850, Ss, Stack, T, Ts, Tzr);
yeccpars2_848(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_848_(Stack),
 yeccpars2(849, Cat, [848 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_849(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_849_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_serviceChangeReply(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_850(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_850(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 853, Ss, Stack, T, Ts, Tzr).

yeccpars2_851(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 871, Ss, Stack, T, Ts, Tzr).

yeccpars2_852(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 870, Ss, Stack, T, Ts, Tzr).

yeccpars2_853(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 854, Ss, Stack, T, Ts, Tzr).

yeccpars2_854(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 861, Ss, Stack, T, Ts, Tzr);
yeccpars2_854(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 862, Ss, Stack, T, Ts, Tzr);
yeccpars2_854(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 863, Ss, Stack, T, Ts, Tzr);
yeccpars2_854(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 438, Ss, Stack, T, Ts, Tzr);
yeccpars2_854(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 864, Ss, Stack, T, Ts, Tzr).

yeccpars2_855(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_855_(Stack),
 yeccpars2(yeccgoto_servChgReplyParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_856(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_856_(Stack),
 yeccpars2(yeccgoto_servChgReplyParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_857(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_857_(Stack),
 yeccpars2(yeccgoto_servChgReplyParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_858(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_858_(Stack),
 yeccpars2(yeccgoto_servChgReplyParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_859(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_859_(Stack),
 yeccpars2(yeccgoto_servChgReplyParm(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_860(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 866, Ss, Stack, T, Ts, Tzr);
yeccpars2_860(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_860_(Stack),
 yeccpars2(865, Cat, [860 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_861(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 449, Ss, Stack, T, Ts, Tzr).

yeccpars2_862(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 447, Ss, Stack, T, Ts, Tzr).

yeccpars2_863(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 442, Ss, Stack, T, Ts, Tzr).

yeccpars2_864(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 440, Ss, Stack, T, Ts, Tzr).

yeccpars2_865(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 869, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_866: see yeccpars2_854

yeccpars2_867(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 866, Ss, Stack, T, Ts, Tzr);
yeccpars2_867(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_867_(Stack),
 yeccpars2(868, Cat, [867 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_868(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_868_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_servChgReplyParms(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_869(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_869_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_serviceChangeReplyDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_870(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_870_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_serviceChangeReplyBody(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_871(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_871_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_serviceChangeReplyBody(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_872: see yeccpars2_180

yeccpars2_873(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 875, Ss, Stack, T, Ts, Tzr);
yeccpars2_873(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_873_(Stack),
 yeccpars2(874, Cat, [873 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_874(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_874_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_notifyReply(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_875(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr).

yeccpars2_876(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 877, Ss, Stack, T, Ts, Tzr).

yeccpars2_877(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_877_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_notifyReplyBody(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_878(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 881, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_879(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 895, Ss, Stack, T, Ts, Tzr);
yeccpars2_879(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_879_(Stack),
 yeccpars2(yeccgoto_auditOther(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_880(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_880_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_auditReply(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_881(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 884, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_881_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_882(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_882_(Stack),
 yeccpars2(yeccgoto_contextTerminationAudit(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_883(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_883_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_auditReply(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_884(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 886, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_884(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_885(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 894, Ss, Stack, T, Ts, Tzr).

yeccpars2_886(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 887, Ss, Stack, T, Ts, Tzr);
yeccpars2_886(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_886_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_887: see yeccpars2_4

yeccpars2_888(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_888_(Stack),
 yeccpars2(yeccgoto_errorCode(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_889(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 890, Ss, Stack, T, Ts, Tzr).

yeccpars2_890(S, 'QuotedChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 892, Ss, Stack, T, Ts, Tzr);
yeccpars2_890(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_890_(Stack),
 yeccpars2(891, Cat, [890 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_891(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 893, Ss, Stack, T, Ts, Tzr).

yeccpars2_892(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_892_(Stack),
 yeccpars2(yeccgoto_errorText(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_893(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_893_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_errorDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_894(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_894_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_contextTerminationAudit(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_895(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 581, Ss, Stack, T, Ts, Tzr);
yeccpars2_895(S, 'DigitMapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 208, Ss, Stack, T, Ts, Tzr);
yeccpars2_895(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 117, Ss, Stack, T, Ts, Tzr);
yeccpars2_895(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 582, Ss, Stack, T, Ts, Tzr);
yeccpars2_895(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 583, Ss, Stack, T, Ts, Tzr);
yeccpars2_895(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 910, Ss, Stack, T, Ts, Tzr);
yeccpars2_895(S, 'ModemToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 911, Ss, Stack, T, Ts, Tzr);
yeccpars2_895(S, 'MuxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 912, Ss, Stack, T, Ts, Tzr);
yeccpars2_895(S, 'ObservedEventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 913, Ss, Stack, T, Ts, Tzr);
yeccpars2_895(S, 'PackagesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 914, Ss, Stack, T, Ts, Tzr);
yeccpars2_895(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 587, Ss, Stack, T, Ts, Tzr);
yeccpars2_895(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 915, Ss, Stack, T, Ts, Tzr).

yeccpars2_896(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 927, Ss, Stack, T, Ts, Tzr).

yeccpars2_897(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_897_(Stack),
 yeccpars2(yeccgoto_auditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_898(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_898_(Stack),
 yeccpars2(yeccgoto_auditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_899(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_899_(Stack),
 yeccpars2(yeccgoto_auditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_900(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_900_(Stack),
 yeccpars2(yeccgoto_auditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_901(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_901_(Stack),
 yeccpars2(yeccgoto_auditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_902(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_902_(Stack),
 yeccpars2(yeccgoto_auditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_903(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_903_(Stack),
 yeccpars2(yeccgoto_auditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_904(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_904_(Stack),
 yeccpars2(yeccgoto_auditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_905(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_905_(Stack),
 yeccpars2(yeccgoto_auditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_906(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_906_(Stack),
 yeccpars2(yeccgoto_auditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_907(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_907_(Stack),
 yeccpars2(yeccgoto_auditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_908(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 924, Ss, Stack, T, Ts, Tzr);
yeccpars2_908(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_908_(Stack),
 yeccpars2(923, Cat, [908 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_909(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_909_(Stack),
 yeccpars2(yeccgoto_auditReturnParameter(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_910(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 646, Ss, Stack, T, Ts, Tzr);
yeccpars2_910(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_910_(Stack),
 yeccpars2(yeccgoto_auditReturnItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_911(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 630, Ss, Stack, T, Ts, Tzr);
yeccpars2_911(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 631, Ss, Stack, T, Ts, Tzr);
yeccpars2_911(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_911_(Stack),
 yeccpars2(yeccgoto_auditReturnItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_912(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 622, Ss, Stack, T, Ts, Tzr);
yeccpars2_912(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_912_(Stack),
 yeccpars2(yeccgoto_auditReturnItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_913(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 471, Ss, Stack, T, Ts, Tzr);
yeccpars2_913(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_913_(Stack),
 yeccpars2(yeccgoto_auditReturnItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_914(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 916, Ss, Stack, T, Ts, Tzr);
yeccpars2_914(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_914_(Stack),
 yeccpars2(yeccgoto_auditReturnItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_915(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 589, Ss, Stack, T, Ts, Tzr);
yeccpars2_915(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_915_(Stack),
 yeccpars2(yeccgoto_auditReturnItem(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_916: see yeccpars2_4

yeccpars2_917(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 919, Ss, Stack, T, Ts, Tzr);
yeccpars2_917(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_917_(Stack),
 yeccpars2(918, Cat, [917 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_918(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 922, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_919: see yeccpars2_4

yeccpars2_920(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 919, Ss, Stack, T, Ts, Tzr);
yeccpars2_920(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_920_(Stack),
 yeccpars2(921, Cat, [920 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_921(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_921_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_packagesItems(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_922(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_922_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_packagesDescriptor(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_923(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_923_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_terminationAudit(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_924: see yeccpars2_895

yeccpars2_925(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 924, Ss, Stack, T, Ts, Tzr);
yeccpars2_925(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_925_(Stack),
 yeccpars2(926, Cat, [925 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_926(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_926_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_auditReturnParameterList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_927(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_927_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_auditOther(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_928(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 930, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_928(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_929(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_929_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_auditReply(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_930(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 884, Ss, Stack, T, Ts, Tzr);
yeccpars2_930(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_930_(Stack),
 yeccpars2(yeccgoto_safeToken(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_931(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_931_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_auditReply(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_932(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_932_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_actionReply(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_933: see yeccpars2_180

yeccpars2_934(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 936, Ss, Stack, T, Ts, Tzr);
yeccpars2_934(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_934_(Stack),
 yeccpars2(935, Cat, [934 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_935(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_935_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_ammsReply(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_936: see yeccpars2_895

yeccpars2_937(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 938, Ss, Stack, T, Ts, Tzr).

yeccpars2_938(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_938_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_ammsReplyBody(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_939(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_939_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_actionReplyBody(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_940: see yeccpars2_829

yeccpars2_941(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_941_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_commandReplyList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_942(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 940, Ss, Stack, T, Ts, Tzr);
yeccpars2_942(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_942_(Stack),
 yeccpars2(943, Cat, [942 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_943(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_943_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_commandReplyList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_944(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_944_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_transactionReplyBody(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_945(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 826, Ss, Stack, T, Ts, Tzr).

yeccpars2_946(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 945, Ss, Stack, T, Ts, Tzr);
yeccpars2_946(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_946_(Stack),
 yeccpars2(947, Cat, [946 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_947(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_947_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_actionReplyList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_948(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_948_(Stack),
 Nss = lists:nthtail(6, Ss),
 yeccpars2(yeccgoto_transactionReply(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_949: see yeccpars2_4

yeccpars2_950(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 951, Ss, Stack, T, Ts, Tzr).

yeccpars2_951(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 952, Ss, Stack, T, Ts, Tzr).

yeccpars2_952(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_952_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_transactionPending(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_953(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_953_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_transactionList(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_954(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_954_(Stack),
 yeccpars2(yeccgoto_pathName(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_955(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_955_(Stack),
 yeccpars2(yeccgoto_deviceName(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_956(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_956(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_956_(Stack),
 yeccpars2(960, Cat, [956 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_957(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_957(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_957_(Stack),
 yeccpars2(959, Cat, [957 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_958(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_958_(Stack),
 yeccpars2(yeccgoto_mtpAddress(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_959(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_959_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_mId(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_960(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_960_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_mId(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccgoto_actionReply(820) -> 825;
yeccgoto_actionReply(945) -> 946.

yeccgoto_actionReplyBody(829) -> 838.

yeccgoto_actionReplyList(825) -> 944;
yeccgoto_actionReplyList(946) -> 947.

yeccgoto_actionRequest(123) -> 124;
yeccgoto_actionRequest(795) -> 796;
yeccgoto_actionRequest(801) -> 802;
yeccgoto_actionRequest(805) -> 806.

yeccgoto_actionRequestBody(129) -> 144.

yeccgoto_actionRequestItem(129) -> 143;
yeccgoto_actionRequestItem(564) -> 565.

yeccgoto_actionRequestItems(143) -> 563;
yeccgoto_actionRequestItems(565) -> 566.

yeccgoto_actionRequestList(124) -> 794;
yeccgoto_actionRequestList(796) -> 797;
yeccgoto_actionRequestList(802) -> 803;
yeccgoto_actionRequestList(806) -> 807.

yeccgoto_alternativeValue(283) -> 293.

yeccgoto_ammParameter(570) -> 580;
yeccgoto_ammParameter(790) -> 791.

yeccgoto_ammParameters(580) -> 789;
yeccgoto_ammParameters(791) -> 792.

yeccgoto_ammRequest(129) -> 142;
yeccgoto_ammRequest(564) -> 142.

yeccgoto_ammRequestBody(568) -> 569.

yeccgoto_ammToken(129) -> 141;
yeccgoto_ammToken(564) -> 141.

yeccgoto_ammsReply(829) -> 837;
yeccgoto_ammsReply(940) -> 837.

yeccgoto_ammsReplyBody(934) -> 935.

yeccgoto_ammsToken(829) -> 836;
yeccgoto_ammsToken(940) -> 836.

yeccgoto_auditDescriptor(191) -> 192;
yeccgoto_auditDescriptor(570) -> 579;
yeccgoto_auditDescriptor(790) -> 579.

yeccgoto_auditDescriptorBody(194) -> 206.

yeccgoto_auditItem(194) -> 205;
yeccgoto_auditItem(399) -> 400;
yeccgoto_auditItem(417) -> 430;
yeccgoto_auditItem(457) -> 430.

yeccgoto_auditItemList(205) -> 398;
yeccgoto_auditItemList(400) -> 401.

yeccgoto_auditOther(878) -> 880;
yeccgoto_auditOther(928) -> 929.

yeccgoto_auditReply(829) -> 835;
yeccgoto_auditReply(940) -> 835.

yeccgoto_auditRequest(129) -> 140;
yeccgoto_auditRequest(564) -> 140.

yeccgoto_auditReturnItem(194) -> 204;
yeccgoto_auditReturnItem(399) -> 204;
yeccgoto_auditReturnItem(417) -> 204;
yeccgoto_auditReturnItem(457) -> 204;
yeccgoto_auditReturnItem(895) -> 909;
yeccgoto_auditReturnItem(924) -> 909;
yeccgoto_auditReturnItem(936) -> 909.

yeccgoto_auditReturnParameter(895) -> 908;
yeccgoto_auditReturnParameter(924) -> 925;
yeccgoto_auditReturnParameter(936) -> 908.

yeccgoto_auditReturnParameterList(908) -> 923;
yeccgoto_auditReturnParameterList(925) -> 926.

yeccgoto_auditSelectLogic(504) -> 513;
yeccgoto_auditSelectLogic(525) -> 513;
yeccgoto_auditSelectLogic(541) -> 513.

yeccgoto_authenticationHeader(1) -> 4.

yeccgoto_commandReplyList(834) -> 939;
yeccgoto_commandReplyList(942) -> 943.

yeccgoto_commandReplys(829) -> 834;
yeccgoto_commandReplys(940) -> 942.

yeccgoto_commandRequest(129) -> 139;
yeccgoto_commandRequest(564) -> 139.

yeccgoto_contextAttrDescriptor(129) -> 138;
yeccgoto_contextAttrDescriptor(504) -> 512;
yeccgoto_contextAttrDescriptor(525) -> 512;
yeccgoto_contextAttrDescriptor(541) -> 512;
yeccgoto_contextAttrDescriptor(564) -> 138;
yeccgoto_contextAttrDescriptor(829) -> 138;
yeccgoto_contextAttrDescriptor(940) -> 138.

yeccgoto_contextAudit(129) -> 137;
yeccgoto_contextAudit(564) -> 137.

yeccgoto_contextAuditProperties(511) -> 552;
yeccgoto_contextAuditProperties(530) -> 540;
yeccgoto_contextAuditProperties(542) -> 543.

yeccgoto_contextAuditProperty(504) -> 511;
yeccgoto_contextAuditProperty(525) -> 530;
yeccgoto_contextAuditProperty(541) -> 542.

yeccgoto_contextAuditSelector(504) -> 510;
yeccgoto_contextAuditSelector(525) -> 510;
yeccgoto_contextAuditSelector(541) -> 510.

yeccgoto_contextID(126) -> 128;
yeccgoto_contextID(533) -> 534;
yeccgoto_contextID(536) -> 537;
yeccgoto_contextID(827) -> 828.

yeccgoto_contextIDs(534) -> 535;
yeccgoto_contextIDs(537) -> 538.

yeccgoto_contextIdList(525) -> 529;
yeccgoto_contextIdList(555) -> 529.

yeccgoto_contextProperty(129) -> 136;
yeccgoto_contextProperty(564) -> 136;
yeccgoto_contextProperty(829) -> 833;
yeccgoto_contextProperty(940) -> 833.

yeccgoto_contextTerminationAudit(881) -> 883;
yeccgoto_contextTerminationAudit(930) -> 931.

yeccgoto_daddr(93) -> 95;
yeccgoto_daddr(94) -> 103;
yeccgoto_daddr(96) -> 97.

yeccgoto_deviceName(88) -> 957.

yeccgoto_digitMapDescriptor(570) -> 578;
yeccgoto_digitMapDescriptor(790) -> 578;
yeccgoto_digitMapDescriptor(895) -> 907;
yeccgoto_digitMapDescriptor(924) -> 907;
yeccgoto_digitMapDescriptor(936) -> 907.

yeccgoto_direction(277) -> 278.

yeccgoto_domainAddress(85) -> 91;
yeccgoto_domainAddress(442) -> 91;
yeccgoto_domainAddress(449) -> 91.

yeccgoto_domainName(85) -> 90;
yeccgoto_domainName(442) -> 90;
yeccgoto_domainName(449) -> 90.

yeccgoto_embedFirst(732) -> 734;
yeccgoto_embedFirst(765) -> 767.

yeccgoto_embedNoSig(713) -> 720;
yeccgoto_embedNoSig(728) -> 730;
yeccgoto_embedNoSig(772) -> 720.

yeccgoto_embedSig(742) -> 747;
yeccgoto_embedSig(755) -> 747.

yeccgoto_embedWithSig(713) -> 719;
yeccgoto_embedWithSig(728) -> 729;
yeccgoto_embedWithSig(772) -> 719.

yeccgoto_emergencyValue(504) -> 509;
yeccgoto_emergencyValue(525) -> 509;
yeccgoto_emergencyValue(541) -> 509.

yeccgoto_errorCode(887) -> 889.

yeccgoto_errorDescriptor(89) -> 116;
yeccgoto_errorDescriptor(466) -> 469;
yeccgoto_errorDescriptor(820) -> 824;
yeccgoto_errorDescriptor(829) -> 832;
yeccgoto_errorDescriptor(850) -> 852;
yeccgoto_errorDescriptor(875) -> 876;
yeccgoto_errorDescriptor(884) -> 885;
yeccgoto_errorDescriptor(895) -> 906;
yeccgoto_errorDescriptor(924) -> 906;
yeccgoto_errorDescriptor(936) -> 906;
yeccgoto_errorDescriptor(940) -> 941.

yeccgoto_errorText(890) -> 891.

yeccgoto_eventBufferControl(658) -> 662;
yeccgoto_eventBufferControl(672) -> 662.

yeccgoto_eventBufferControlValue(667) -> 668.

yeccgoto_eventBufferDescriptor(570) -> 577;
yeccgoto_eventBufferDescriptor(790) -> 577;
yeccgoto_eventBufferDescriptor(895) -> 905;
yeccgoto_eventBufferDescriptor(924) -> 905;
yeccgoto_eventBufferDescriptor(936) -> 905.

yeccgoto_eventDM(713) -> 718;
yeccgoto_eventDM(742) -> 746;
yeccgoto_eventDM(755) -> 746;
yeccgoto_eventDM(772) -> 718.

yeccgoto_eventParameter(713) -> 717;
yeccgoto_eventParameter(772) -> 773.

yeccgoto_eventParameterName(391) -> 395;
yeccgoto_eventParameterName(484) -> 487;
yeccgoto_eventParameterName(490) -> 487;
yeccgoto_eventParameterName(713) -> 487;
yeccgoto_eventParameterName(742) -> 487;
yeccgoto_eventParameterName(755) -> 487;
yeccgoto_eventParameterName(772) -> 487.

yeccgoto_eventParameters(717) -> 771;
yeccgoto_eventParameters(773) -> 774.

yeccgoto_eventSpec(781) -> 783;
yeccgoto_eventSpec(785) -> 786.

yeccgoto_eventSpecList(783) -> 784;
yeccgoto_eventSpecList(786) -> 787.

yeccgoto_eventStream(160) -> 165;
yeccgoto_eventStream(176) -> 165;
yeccgoto_eventStream(391) -> 394.

yeccgoto_eventStreamOrOther(484) -> 486;
yeccgoto_eventStreamOrOther(490) -> 486;
yeccgoto_eventStreamOrOther(713) -> 716;
yeccgoto_eventStreamOrOther(742) -> 745;
yeccgoto_eventStreamOrOther(755) -> 745;
yeccgoto_eventStreamOrOther(772) -> 716.

yeccgoto_eventsDescriptor(570) -> 576;
yeccgoto_eventsDescriptor(790) -> 576;
yeccgoto_eventsDescriptor(895) -> 904;
yeccgoto_eventsDescriptor(924) -> 904;
yeccgoto_eventsDescriptor(936) -> 904.

yeccgoto_extension(417) -> 429;
yeccgoto_extension(457) -> 429.

yeccgoto_extensionParameter(417) -> 428;
yeccgoto_extensionParameter(457) -> 428.

yeccgoto_iaServiceStates(330) -> 334.

yeccgoto_iepsValue(129) -> 135;
yeccgoto_iepsValue(504) -> 508;
yeccgoto_iepsValue(525) -> 508;
yeccgoto_iepsValue(541) -> 508;
yeccgoto_iepsValue(564) -> 135;
yeccgoto_iepsValue(829) -> 135;
yeccgoto_iepsValue(940) -> 135.

yeccgoto_indAudauditReturnParameter(194) -> 203;
yeccgoto_indAudauditReturnParameter(399) -> 203;
yeccgoto_indAudauditReturnParameter(403) -> 404;
yeccgoto_indAudauditReturnParameter(417) -> 203;
yeccgoto_indAudauditReturnParameter(457) -> 203.

yeccgoto_indAudcontextAttrDescriptor(504) -> 507.

yeccgoto_indAuddigitMapDescriptor(194) -> 202;
yeccgoto_indAuddigitMapDescriptor(399) -> 202;
yeccgoto_indAuddigitMapDescriptor(403) -> 202;
yeccgoto_indAuddigitMapDescriptor(417) -> 202;
yeccgoto_indAuddigitMapDescriptor(457) -> 202.

yeccgoto_indAudeventBufferDescriptor(194) -> 201;
yeccgoto_indAudeventBufferDescriptor(399) -> 201;
yeccgoto_indAudeventBufferDescriptor(403) -> 201;
yeccgoto_indAudeventBufferDescriptor(417) -> 201;
yeccgoto_indAudeventBufferDescriptor(457) -> 201.

yeccgoto_indAudeventSpec(386) -> 388.

yeccgoto_indAudeventSpecParameter(391) -> 393.

yeccgoto_indAudeventsDescriptor(194) -> 200;
yeccgoto_indAudeventsDescriptor(399) -> 200;
yeccgoto_indAudeventsDescriptor(403) -> 200;
yeccgoto_indAudeventsDescriptor(417) -> 200;
yeccgoto_indAudeventsDescriptor(457) -> 200.

yeccgoto_indAudlocalControlDescriptor(317) -> 323;
yeccgoto_indAudlocalControlDescriptor(348) -> 323;
yeccgoto_indAudlocalControlDescriptor(373) -> 323.

yeccgoto_indAudlocalParm(351) -> 354;
yeccgoto_indAudlocalParm(368) -> 369.

yeccgoto_indAudlocalParmList(354) -> 367;
yeccgoto_indAudlocalParmList(369) -> 370.

yeccgoto_indAudmediaDescriptor(194) -> 199;
yeccgoto_indAudmediaDescriptor(399) -> 199;
yeccgoto_indAudmediaDescriptor(403) -> 199;
yeccgoto_indAudmediaDescriptor(417) -> 199;
yeccgoto_indAudmediaDescriptor(457) -> 199.

yeccgoto_indAudmediaParm(317) -> 322;
yeccgoto_indAudmediaParm(373) -> 374.

yeccgoto_indAudmediaParms(322) -> 372;
yeccgoto_indAudmediaParms(374) -> 375.

yeccgoto_indAudpackagesDescriptor(194) -> 198;
yeccgoto_indAudpackagesDescriptor(399) -> 198;
yeccgoto_indAudpackagesDescriptor(403) -> 198;
yeccgoto_indAudpackagesDescriptor(417) -> 198;
yeccgoto_indAudpackagesDescriptor(457) -> 198.

yeccgoto_indAudrequestedEvent(378) -> 380;
yeccgoto_indAudrequestedEvent(383) -> 384.

yeccgoto_indAudsignalList(223) -> 228.

yeccgoto_indAudsignalParm(223) -> 227.

yeccgoto_indAudsignalsDescriptor(194) -> 197;
yeccgoto_indAudsignalsDescriptor(399) -> 197;
yeccgoto_indAudsignalsDescriptor(403) -> 197;
yeccgoto_indAudsignalsDescriptor(417) -> 197;
yeccgoto_indAudsignalsDescriptor(457) -> 197.

yeccgoto_indAudstatisticsDescriptor(194) -> 196;
yeccgoto_indAudstatisticsDescriptor(317) -> 321;
yeccgoto_indAudstatisticsDescriptor(348) -> 321;
yeccgoto_indAudstatisticsDescriptor(373) -> 321;
yeccgoto_indAudstatisticsDescriptor(399) -> 196;
yeccgoto_indAudstatisticsDescriptor(403) -> 196;
yeccgoto_indAudstatisticsDescriptor(417) -> 196;
yeccgoto_indAudstatisticsDescriptor(457) -> 196.

yeccgoto_indAudstreamDescriptor(317) -> 320;
yeccgoto_indAudstreamDescriptor(373) -> 320.

yeccgoto_indAudstreamParm(317) -> 319;
yeccgoto_indAudstreamParm(348) -> 349;
yeccgoto_indAudstreamParm(373) -> 319.

yeccgoto_indAudterminationAudit(194) -> 195;
yeccgoto_indAudterminationAudit(399) -> 195;
yeccgoto_indAudterminationAudit(417) -> 195;
yeccgoto_indAudterminationAudit(457) -> 195.

yeccgoto_indAudterminationAuditList(203) -> 402;
yeccgoto_indAudterminationAuditList(404) -> 410.

yeccgoto_indAudterminationStateDescriptor(317) -> 318;
yeccgoto_indAudterminationStateDescriptor(373) -> 318.

yeccgoto_indAudterminationStateParm(330) -> 333.

yeccgoto_localControlDescriptor(646) -> 652;
yeccgoto_localControlDescriptor(678) -> 652;
yeccgoto_localControlDescriptor(681) -> 652;
yeccgoto_localControlDescriptor(703) -> 652.

yeccgoto_localParm(685) -> 687;
yeccgoto_localParm(698) -> 699.

yeccgoto_localParmList(687) -> 697;
yeccgoto_localParmList(699) -> 700.

yeccgoto_mId(85) -> 89;
yeccgoto_mId(442) -> 444;
yeccgoto_mId(449) -> 450.

yeccgoto_mediaDescriptor(570) -> 575;
yeccgoto_mediaDescriptor(790) -> 575;
yeccgoto_mediaDescriptor(895) -> 903;
yeccgoto_mediaDescriptor(924) -> 903;
yeccgoto_mediaDescriptor(936) -> 903.

yeccgoto_mediaParm(646) -> 651;
yeccgoto_mediaParm(703) -> 704.

yeccgoto_mediaParmList(651) -> 702;
yeccgoto_mediaParmList(704) -> 705.

yeccgoto_megacoMessage(0) -> 2.

yeccgoto_message(4) -> 86.

yeccgoto_messageBody(89) -> 115.

yeccgoto_modemDescriptor(570) -> 574;
yeccgoto_modemDescriptor(790) -> 574;
yeccgoto_modemDescriptor(895) -> 902;
yeccgoto_modemDescriptor(924) -> 902;
yeccgoto_modemDescriptor(936) -> 902.

yeccgoto_modemType(630) -> 644;
yeccgoto_modemType(631) -> 633;
yeccgoto_modemType(635) -> 636.

yeccgoto_modemTypeList(633) -> 634;
yeccgoto_modemTypeList(636) -> 637.

yeccgoto_mtpAddress(88) -> 956.

yeccgoto_muxDescriptor(570) -> 573;
yeccgoto_muxDescriptor(790) -> 573;
yeccgoto_muxDescriptor(895) -> 901;
yeccgoto_muxDescriptor(924) -> 901;
yeccgoto_muxDescriptor(936) -> 901.

yeccgoto_muxType(622) -> 624.

yeccgoto_notificationReason(261) -> 262;
yeccgoto_notificationReason(269) -> 270.

yeccgoto_notificationReasons(262) -> 268;
yeccgoto_notificationReasons(270) -> 271.

yeccgoto_notifyBehaviour(713) -> 715;
yeccgoto_notifyBehaviour(742) -> 744;
yeccgoto_notifyBehaviour(755) -> 744;
yeccgoto_notifyBehaviour(772) -> 715.

yeccgoto_notifyRegulated(713) -> 714;
yeccgoto_notifyRegulated(742) -> 714;
yeccgoto_notifyRegulated(755) -> 714;
yeccgoto_notifyRegulated(772) -> 714.

yeccgoto_notifyReply(829) -> 831;
yeccgoto_notifyReply(940) -> 831.

yeccgoto_notifyReplyBody(873) -> 874.

yeccgoto_notifyRequest(129) -> 134;
yeccgoto_notifyRequest(564) -> 134.

yeccgoto_notifyRequestBody(466) -> 468.

yeccgoto_observedEvent(473) -> 476;
yeccgoto_observedEvent(478) -> 479;
yeccgoto_observedEvent(781) -> 782;
yeccgoto_observedEvent(785) -> 782.

yeccgoto_observedEventBody(482) -> 483;
yeccgoto_observedEventBody(497) -> 498.

yeccgoto_observedEventParameter(484) -> 485;
yeccgoto_observedEventParameter(490) -> 491.

yeccgoto_observedEventParameters(485) -> 489;
yeccgoto_observedEventParameters(491) -> 492.

yeccgoto_observedEvents(476) -> 477;
yeccgoto_observedEvents(479) -> 480.

yeccgoto_observedEventsDescriptor(466) -> 467;
yeccgoto_observedEventsDescriptor(895) -> 900;
yeccgoto_observedEventsDescriptor(924) -> 900;
yeccgoto_observedEventsDescriptor(936) -> 900.

yeccgoto_onOrOff(500) -> 501;
yeccgoto_onOrOff(691) -> 692;
yeccgoto_onOrOff(693) -> 694.

yeccgoto_optAuditDescriptor(182) -> 190;
yeccgoto_optAuditDescriptor(557) -> 558;
yeccgoto_optAuditDescriptor(560) -> 561.

yeccgoto_optImmAckRequired(819) -> 820.

yeccgoto_optIndAudeventSpecParameter(387) -> 390.

yeccgoto_optIndAudsignalParm(216) -> 222;
yeccgoto_optIndAudsignalParm(409) -> 222.

yeccgoto_optPropertyParms(638) -> 639;
yeccgoto_optPropertyParms(644) -> 645.

yeccgoto_optSep(0) -> 1;
yeccgoto_optSep(83) -> 84;
yeccgoto_optSep(85) -> 88;
yeccgoto_optSep(101) -> 102;
yeccgoto_optSep(107) -> 108;
yeccgoto_optSep(442) -> 88;
yeccgoto_optSep(449) -> 88;
yeccgoto_optSep(473) -> 475;
yeccgoto_optSep(474) -> 494;
yeccgoto_optSep(478) -> 475;
yeccgoto_optSep(495) -> 496;
yeccgoto_optSep(781) -> 475;
yeccgoto_optSep(785) -> 475;
yeccgoto_optSep(956) -> 960;
yeccgoto_optSep(957) -> 959.

yeccgoto_packagesDescriptor(895) -> 899;
yeccgoto_packagesDescriptor(924) -> 899;
yeccgoto_packagesDescriptor(936) -> 899.

yeccgoto_packagesItem(313) -> 315;
yeccgoto_packagesItem(916) -> 917;
yeccgoto_packagesItem(919) -> 920.

yeccgoto_packagesItems(917) -> 918;
yeccgoto_packagesItems(920) -> 921.

yeccgoto_parmValue(241) -> 282;
yeccgoto_parmValue(332) -> 345;
yeccgoto_parmValue(353) -> 345;
yeccgoto_parmValue(428) -> 455;
yeccgoto_parmValue(487) -> 488;
yeccgoto_parmValue(528) -> 345;
yeccgoto_parmValue(549) -> 345.

yeccgoto_pathName(88) -> 955.

yeccgoto_pkgdName(218) -> 220;
yeccgoto_pkgdName(223) -> 226;
yeccgoto_pkgdName(234) -> 226;
yeccgoto_pkgdName(330) -> 332;
yeccgoto_pkgdName(351) -> 353;
yeccgoto_pkgdName(368) -> 353;
yeccgoto_pkgdName(378) -> 379;
yeccgoto_pkgdName(383) -> 379;
yeccgoto_pkgdName(386) -> 387;
yeccgoto_pkgdName(475) -> 482;
yeccgoto_pkgdName(496) -> 497;
yeccgoto_pkgdName(504) -> 506;
yeccgoto_pkgdName(525) -> 528;
yeccgoto_pkgdName(541) -> 506;
yeccgoto_pkgdName(547) -> 549;
yeccgoto_pkgdName(555) -> 549;
yeccgoto_pkgdName(589) -> 591;
yeccgoto_pkgdName(599) -> 591;
yeccgoto_pkgdName(603) -> 226;
yeccgoto_pkgdName(610) -> 226;
yeccgoto_pkgdName(613) -> 226;
yeccgoto_pkgdName(618) -> 226;
yeccgoto_pkgdName(640) -> 549;
yeccgoto_pkgdName(658) -> 549;
yeccgoto_pkgdName(672) -> 549;
yeccgoto_pkgdName(685) -> 549;
yeccgoto_pkgdName(698) -> 549;
yeccgoto_pkgdName(709) -> 711;
yeccgoto_pkgdName(738) -> 740;
yeccgoto_pkgdName(760) -> 740;
yeccgoto_pkgdName(777) -> 711.

yeccgoto_portNumber(99) -> 101;
yeccgoto_portNumber(106) -> 107;
yeccgoto_portNumber(442) -> 443.

yeccgoto_priority(129) -> 133;
yeccgoto_priority(504) -> 505;
yeccgoto_priority(525) -> 505;
yeccgoto_priority(541) -> 505;
yeccgoto_priority(564) -> 133;
yeccgoto_priority(829) -> 133;
yeccgoto_priority(940) -> 133.

yeccgoto_propertyParm(330) -> 331;
yeccgoto_propertyParm(351) -> 352;
yeccgoto_propertyParm(368) -> 352;
yeccgoto_propertyParm(525) -> 527;
yeccgoto_propertyParm(547) -> 548;
yeccgoto_propertyParm(555) -> 527;
yeccgoto_propertyParm(640) -> 641;
yeccgoto_propertyParm(658) -> 661;
yeccgoto_propertyParm(672) -> 661;
yeccgoto_propertyParm(685) -> 686;
yeccgoto_propertyParm(698) -> 686.

yeccgoto_propertyParmList(527) -> 546;
yeccgoto_propertyParmList(548) -> 550;
yeccgoto_propertyParmList(641) -> 642.

yeccgoto_propertyParms(525) -> 526;
yeccgoto_propertyParms(555) -> 526.

yeccgoto_requestID(257) -> 259;
yeccgoto_requestID(377) -> 382;
yeccgoto_requestID(471) -> 472;
yeccgoto_requestID(707) -> 708;
yeccgoto_requestID(736) -> 737.

yeccgoto_requestedEvent(709) -> 710;
yeccgoto_requestedEvent(777) -> 778.

yeccgoto_requestedEventBody(711) -> 712.

yeccgoto_requestedEvents(710) -> 776;
yeccgoto_requestedEvents(778) -> 779.

yeccgoto_safeToken(4) -> 85;
yeccgoto_safeToken(6) -> 7;
yeccgoto_safeToken(80) -> 81;
yeccgoto_safeToken(82) -> 83;
yeccgoto_safeToken(88) -> 954;
yeccgoto_safeToken(92) -> 104;
yeccgoto_safeToken(93) -> 94;
yeccgoto_safeToken(94) -> 94;
yeccgoto_safeToken(96) -> 94;
yeccgoto_safeToken(99) -> 100;
yeccgoto_safeToken(106) -> 100;
yeccgoto_safeToken(122) -> 800;
yeccgoto_safeToken(126) -> 127;
yeccgoto_safeToken(160) -> 164;
yeccgoto_safeToken(172) -> 174;
yeccgoto_safeToken(176) -> 164;
yeccgoto_safeToken(180) -> 164;
yeccgoto_safeToken(183) -> 164;
yeccgoto_safeToken(186) -> 164;
yeccgoto_safeToken(218) -> 219;
yeccgoto_safeToken(223) -> 219;
yeccgoto_safeToken(231) -> 233;
yeccgoto_safeToken(234) -> 219;
yeccgoto_safeToken(239) -> 241;
yeccgoto_safeToken(250) -> 174;
yeccgoto_safeToken(257) -> 258;
yeccgoto_safeToken(273) -> 274;
yeccgoto_safeToken(275) -> 276;
yeccgoto_safeToken(283) -> 288;
yeccgoto_safeToken(284) -> 288;
yeccgoto_safeToken(285) -> 288;
yeccgoto_safeToken(286) -> 288;
yeccgoto_safeToken(294) -> 288;
yeccgoto_safeToken(295) -> 288;
yeccgoto_safeToken(298) -> 288;
yeccgoto_safeToken(299) -> 288;
yeccgoto_safeToken(309) -> 241;
yeccgoto_safeToken(313) -> 314;
yeccgoto_safeToken(330) -> 219;
yeccgoto_safeToken(346) -> 174;
yeccgoto_safeToken(351) -> 219;
yeccgoto_safeToken(368) -> 219;
yeccgoto_safeToken(377) -> 258;
yeccgoto_safeToken(378) -> 219;
yeccgoto_safeToken(383) -> 219;
yeccgoto_safeToken(386) -> 219;
yeccgoto_safeToken(391) -> 392;
yeccgoto_safeToken(412) -> 164;
yeccgoto_safeToken(417) -> 427;
yeccgoto_safeToken(440) -> 441;
yeccgoto_safeToken(442) -> 100;
yeccgoto_safeToken(445) -> 288;
yeccgoto_safeToken(447) -> 448;
yeccgoto_safeToken(451) -> 452;
yeccgoto_safeToken(453) -> 454;
yeccgoto_safeToken(457) -> 427;
yeccgoto_safeToken(462) -> 463;
yeccgoto_safeToken(464) -> 164;
yeccgoto_safeToken(471) -> 258;
yeccgoto_safeToken(475) -> 219;
yeccgoto_safeToken(484) -> 392;
yeccgoto_safeToken(490) -> 392;
yeccgoto_safeToken(496) -> 219;
yeccgoto_safeToken(504) -> 219;
yeccgoto_safeToken(525) -> 219;
yeccgoto_safeToken(533) -> 127;
yeccgoto_safeToken(536) -> 127;
yeccgoto_safeToken(541) -> 219;
yeccgoto_safeToken(547) -> 219;
yeccgoto_safeToken(555) -> 219;
yeccgoto_safeToken(556) -> 164;
yeccgoto_safeToken(559) -> 164;
yeccgoto_safeToken(567) -> 164;
yeccgoto_safeToken(589) -> 219;
yeccgoto_safeToken(592) -> 288;
yeccgoto_safeToken(594) -> 288;
yeccgoto_safeToken(599) -> 219;
yeccgoto_safeToken(603) -> 219;
yeccgoto_safeToken(608) -> 233;
yeccgoto_safeToken(610) -> 219;
yeccgoto_safeToken(613) -> 219;
yeccgoto_safeToken(618) -> 219;
yeccgoto_safeToken(622) -> 623;
yeccgoto_safeToken(626) -> 164;
yeccgoto_safeToken(630) -> 632;
yeccgoto_safeToken(631) -> 632;
yeccgoto_safeToken(635) -> 632;
yeccgoto_safeToken(640) -> 219;
yeccgoto_safeToken(658) -> 219;
yeccgoto_safeToken(672) -> 219;
yeccgoto_safeToken(676) -> 174;
yeccgoto_safeToken(685) -> 219;
yeccgoto_safeToken(698) -> 219;
yeccgoto_safeToken(707) -> 258;
yeccgoto_safeToken(709) -> 219;
yeccgoto_safeToken(713) -> 392;
yeccgoto_safeToken(736) -> 258;
yeccgoto_safeToken(738) -> 219;
yeccgoto_safeToken(742) -> 392;
yeccgoto_safeToken(755) -> 392;
yeccgoto_safeToken(760) -> 219;
yeccgoto_safeToken(772) -> 392;
yeccgoto_safeToken(777) -> 219;
yeccgoto_safeToken(809) -> 811;
yeccgoto_safeToken(813) -> 811;
yeccgoto_safeToken(817) -> 800;
yeccgoto_safeToken(827) -> 127;
yeccgoto_safeToken(847) -> 164;
yeccgoto_safeToken(872) -> 164;
yeccgoto_safeToken(878) -> 164;
yeccgoto_safeToken(884) -> 164;
yeccgoto_safeToken(887) -> 888;
yeccgoto_safeToken(916) -> 314;
yeccgoto_safeToken(919) -> 314;
yeccgoto_safeToken(928) -> 164;
yeccgoto_safeToken(933) -> 164;
yeccgoto_safeToken(949) -> 800.

yeccgoto_secondEventParameter(742) -> 743;
yeccgoto_secondEventParameter(755) -> 756.

yeccgoto_secondEventParameters(743) -> 754;
yeccgoto_secondEventParameters(756) -> 757.

yeccgoto_secondRequestedEvent(738) -> 739;
yeccgoto_secondRequestedEvent(760) -> 761.

yeccgoto_secondRequestedEventBody(740) -> 741.

yeccgoto_secondRequestedEvents(739) -> 759;
yeccgoto_secondRequestedEvents(761) -> 762.

yeccgoto_servChgReplyParm(854) -> 860;
yeccgoto_servChgReplyParm(866) -> 867.

yeccgoto_servChgReplyParms(860) -> 865;
yeccgoto_servChgReplyParms(867) -> 868.

yeccgoto_serviceChangeAddress(417) -> 426;
yeccgoto_serviceChangeAddress(457) -> 426;
yeccgoto_serviceChangeAddress(854) -> 859;
yeccgoto_serviceChangeAddress(866) -> 859.

yeccgoto_serviceChangeDelay(417) -> 425;
yeccgoto_serviceChangeDelay(457) -> 425.

yeccgoto_serviceChangeDescriptor(414) -> 415.

yeccgoto_serviceChangeMethod(417) -> 424;
yeccgoto_serviceChangeMethod(457) -> 424.

yeccgoto_serviceChangeMgcId(417) -> 423;
yeccgoto_serviceChangeMgcId(457) -> 423;
yeccgoto_serviceChangeMgcId(854) -> 858;
yeccgoto_serviceChangeMgcId(866) -> 858.

yeccgoto_serviceChangeParm(417) -> 422;
yeccgoto_serviceChangeParm(457) -> 458.

yeccgoto_serviceChangeParms(422) -> 456;
yeccgoto_serviceChangeParms(458) -> 459.

yeccgoto_serviceChangeProfile(417) -> 421;
yeccgoto_serviceChangeProfile(457) -> 421;
yeccgoto_serviceChangeProfile(854) -> 857;
yeccgoto_serviceChangeProfile(866) -> 857.

yeccgoto_serviceChangeReason(417) -> 420;
yeccgoto_serviceChangeReason(457) -> 420.

yeccgoto_serviceChangeReply(829) -> 830;
yeccgoto_serviceChangeReply(940) -> 830.

yeccgoto_serviceChangeReplyBody(848) -> 849.

yeccgoto_serviceChangeReplyDescriptor(850) -> 851.

yeccgoto_serviceChangeRequest(129) -> 132;
yeccgoto_serviceChangeRequest(564) -> 132.

yeccgoto_serviceChangeVersion(417) -> 419;
yeccgoto_serviceChangeVersion(457) -> 419;
yeccgoto_serviceChangeVersion(854) -> 856;
yeccgoto_serviceChangeVersion(866) -> 856.

yeccgoto_serviceStates(658) -> 660;
yeccgoto_serviceStates(672) -> 660.

yeccgoto_serviceStatesValue(337) -> 343;
yeccgoto_serviceStatesValue(338) -> 339;
yeccgoto_serviceStatesValue(665) -> 666.

yeccgoto_sigParameter(239) -> 240;
yeccgoto_sigParameter(309) -> 310.

yeccgoto_sigParameters(240) -> 308;
yeccgoto_sigParameters(310) -> 311.

yeccgoto_signalList(603) -> 606;
yeccgoto_signalList(618) -> 606.

yeccgoto_signalListId(231) -> 232;
yeccgoto_signalListId(608) -> 609.

yeccgoto_signalListParm(234) -> 236;
yeccgoto_signalListParm(610) -> 611;
yeccgoto_signalListParm(613) -> 614.

yeccgoto_signalListParms(611) -> 612;
yeccgoto_signalListParms(614) -> 615.

yeccgoto_signalName(223) -> 225;
yeccgoto_signalName(234) -> 225;
yeccgoto_signalName(603) -> 225;
yeccgoto_signalName(610) -> 225;
yeccgoto_signalName(613) -> 225;
yeccgoto_signalName(618) -> 225.

yeccgoto_signalParm(603) -> 605;
yeccgoto_signalParm(618) -> 619.

yeccgoto_signalParms(605) -> 617;
yeccgoto_signalParms(619) -> 620.

yeccgoto_signalRequest(223) -> 224;
yeccgoto_signalRequest(234) -> 235;
yeccgoto_signalRequest(603) -> 604;
yeccgoto_signalRequest(610) -> 235;
yeccgoto_signalRequest(613) -> 235;
yeccgoto_signalRequest(618) -> 604.

yeccgoto_signalType(252) -> 253.

yeccgoto_signalsDescriptor(570) -> 572;
yeccgoto_signalsDescriptor(732) -> 733;
yeccgoto_signalsDescriptor(751) -> 752;
yeccgoto_signalsDescriptor(790) -> 572;
yeccgoto_signalsDescriptor(895) -> 898;
yeccgoto_signalsDescriptor(924) -> 898;
yeccgoto_signalsDescriptor(936) -> 898.

yeccgoto_statisticsDescriptor(570) -> 571;
yeccgoto_statisticsDescriptor(646) -> 650;
yeccgoto_statisticsDescriptor(678) -> 650;
yeccgoto_statisticsDescriptor(681) -> 650;
yeccgoto_statisticsDescriptor(703) -> 650;
yeccgoto_statisticsDescriptor(790) -> 571;
yeccgoto_statisticsDescriptor(895) -> 897;
yeccgoto_statisticsDescriptor(924) -> 897;
yeccgoto_statisticsDescriptor(936) -> 897.

yeccgoto_statisticsParameter(589) -> 590;
yeccgoto_statisticsParameter(599) -> 600.

yeccgoto_statisticsParameters(590) -> 598;
yeccgoto_statisticsParameters(600) -> 601.

yeccgoto_streamDescriptor(646) -> 649;
yeccgoto_streamDescriptor(703) -> 649.

yeccgoto_streamID(172) -> 173;
yeccgoto_streamID(250) -> 251;
yeccgoto_streamID(346) -> 347;
yeccgoto_streamID(676) -> 677.

yeccgoto_streamModes(358) -> 366;
yeccgoto_streamModes(359) -> 360;
yeccgoto_streamModes(695) -> 696.

yeccgoto_streamParm(646) -> 648;
yeccgoto_streamParm(678) -> 679;
yeccgoto_streamParm(681) -> 682;
yeccgoto_streamParm(703) -> 648.

yeccgoto_streamParmList(679) -> 680;
yeccgoto_streamParmList(682) -> 683.

yeccgoto_subtractRequest(129) -> 131;
yeccgoto_subtractRequest(564) -> 131.

yeccgoto_termIDList(180) -> 182;
yeccgoto_termIDList(412) -> 413;
yeccgoto_termIDList(464) -> 465;
yeccgoto_termIDList(556) -> 557;
yeccgoto_termIDList(559) -> 560;
yeccgoto_termIDList(567) -> 568;
yeccgoto_termIDList(847) -> 848;
yeccgoto_termIDList(872) -> 873;
yeccgoto_termIDList(878) -> 879;
yeccgoto_termIDList(928) -> 879;
yeccgoto_termIDList(933) -> 934.

yeccgoto_terminationAudit(895) -> 896;
yeccgoto_terminationAudit(936) -> 937.

yeccgoto_terminationID(160) -> 163;
yeccgoto_terminationID(176) -> 163;
yeccgoto_terminationID(180) -> 181;
yeccgoto_terminationID(183) -> 184;
yeccgoto_terminationID(186) -> 187;
yeccgoto_terminationID(412) -> 181;
yeccgoto_terminationID(464) -> 181;
yeccgoto_terminationID(556) -> 181;
yeccgoto_terminationID(559) -> 181;
yeccgoto_terminationID(567) -> 181;
yeccgoto_terminationID(626) -> 627;
yeccgoto_terminationID(847) -> 181;
yeccgoto_terminationID(872) -> 181;
yeccgoto_terminationID(878) -> 181;
yeccgoto_terminationID(884) -> 627;
yeccgoto_terminationID(928) -> 181;
yeccgoto_terminationID(933) -> 181.

yeccgoto_terminationIDList(624) -> 625;
yeccgoto_terminationIDList(881) -> 882;
yeccgoto_terminationIDList(930) -> 882.

yeccgoto_terminationIDListRepeat(184) -> 185;
yeccgoto_terminationIDListRepeat(187) -> 188;
yeccgoto_terminationIDListRepeat(627) -> 628.

yeccgoto_terminationStateDescriptor(646) -> 647;
yeccgoto_terminationStateDescriptor(703) -> 647.

yeccgoto_terminationStateParm(658) -> 659;
yeccgoto_terminationStateParm(672) -> 673.

yeccgoto_terminationStateParms(659) -> 671;
yeccgoto_terminationStateParms(673) -> 674.

yeccgoto_timeStamp(417) -> 418;
yeccgoto_timeStamp(457) -> 418;
yeccgoto_timeStamp(473) -> 474;
yeccgoto_timeStamp(478) -> 474;
yeccgoto_timeStamp(781) -> 474;
yeccgoto_timeStamp(785) -> 474;
yeccgoto_timeStamp(854) -> 855;
yeccgoto_timeStamp(866) -> 855.

yeccgoto_topologyDescComp(160) -> 162;
yeccgoto_topologyDescComp(176) -> 177.

yeccgoto_topologyDescCompList(162) -> 175;
yeccgoto_topologyDescCompList(177) -> 178.

yeccgoto_topologyDescriptor(129) -> 130;
yeccgoto_topologyDescriptor(564) -> 130;
yeccgoto_topologyDescriptor(829) -> 130;
yeccgoto_topologyDescriptor(940) -> 130.

yeccgoto_topologyDirection(160) -> 161;
yeccgoto_topologyDirection(176) -> 161.

yeccgoto_transactionAck(809) -> 810;
yeccgoto_transactionAck(813) -> 814.

yeccgoto_transactionAckList(810) -> 812;
yeccgoto_transactionAckList(814) -> 815.

yeccgoto_transactionID(122) -> 799;
yeccgoto_transactionID(817) -> 818;
yeccgoto_transactionID(949) -> 950.

yeccgoto_transactionItem(89) -> 114;
yeccgoto_transactionItem(114) -> 114.

yeccgoto_transactionList(89) -> 113;
yeccgoto_transactionList(114) -> 953.

yeccgoto_transactionPending(89) -> 112;
yeccgoto_transactionPending(114) -> 112.

yeccgoto_transactionReply(89) -> 111;
yeccgoto_transactionReply(114) -> 111.

yeccgoto_transactionReplyBody(820) -> 823.

yeccgoto_transactionRequest(89) -> 110;
yeccgoto_transactionRequest(114) -> 110.

yeccgoto_transactionResponseAck(89) -> 109;
yeccgoto_transactionResponseAck(114) -> 109.

yeccgoto_value(283) -> 292;
yeccgoto_value(284) -> 291;
yeccgoto_value(285) -> 290;
yeccgoto_value(286) -> 287;
yeccgoto_value(294) -> 305;
yeccgoto_value(295) -> 296;
yeccgoto_value(298) -> 302;
yeccgoto_value(299) -> 300;
yeccgoto_value(445) -> 446;
yeccgoto_value(592) -> 593;
yeccgoto_value(594) -> 595.

yeccgoto_valueList(296) -> 297;
yeccgoto_valueList(300) -> 301;
yeccgoto_valueList(305) -> 306;
yeccgoto_valueList(595) -> 596.

-compile({inline,{yeccpars2_0_,1}}).
-file("megaco_text_parser_prev3c.yrl", 481).
yeccpars2_0_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_1_,1}}).
-file("megaco_text_parser_prev3c.yrl", 486).
yeccpars2_1_(Stack) ->
 [begin
   asn1_NOVALUE
  end | Stack].

-compile({inline,{yeccpars2_3_,1}}).
-file("megaco_text_parser_prev3c.yrl", 480).
yeccpars2_3_([__1 | Stack]) ->
 [begin
   sep
  end | Stack].

-compile({inline,{yeccpars2_8_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1537).
yeccpars2_8_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_9_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1536).
yeccpars2_9_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_10_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1538).
yeccpars2_10_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_11_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1539).
yeccpars2_11_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_12_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1542).
yeccpars2_12_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_13_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1546).
yeccpars2_13_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_14_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1544).
yeccpars2_14_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_15_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1554).
yeccpars2_15_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_16_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1551).
yeccpars2_16_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_17_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1552).
yeccpars2_17_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_18_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1553).
yeccpars2_18_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_19_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1555).
yeccpars2_19_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_20_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1556).
yeccpars2_20_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_21_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1559).
yeccpars2_21_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_22_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1563).
yeccpars2_22_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_23_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1564).
yeccpars2_23_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_24_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1565).
yeccpars2_24_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_25_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1566).
yeccpars2_25_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_26_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1567).
yeccpars2_26_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_27_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1568).
yeccpars2_27_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_28_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1569).
yeccpars2_28_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_29_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1571).
yeccpars2_29_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_30_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1577).
yeccpars2_30_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_31_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1572).
yeccpars2_31_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_32_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1574).
yeccpars2_32_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_33_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1575).
yeccpars2_33_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_34_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1578).
yeccpars2_34_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_35_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1581).
yeccpars2_35_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_36_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1583).
yeccpars2_36_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_37_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1582).
yeccpars2_37_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_38_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1586).
yeccpars2_38_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_39_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1587).
yeccpars2_39_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_40_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1596).
yeccpars2_40_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_41_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1595).
yeccpars2_41_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_42_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1597).
yeccpars2_42_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_43_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1602).
yeccpars2_43_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_44_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1604).
yeccpars2_44_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_45_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1603).
yeccpars2_45_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_46_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1606).
yeccpars2_46_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_47_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1605).
yeccpars2_47_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_48_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1608).
yeccpars2_48_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_49_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1610).
yeccpars2_49_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_50_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1611).
yeccpars2_50_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_51_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1612).
yeccpars2_51_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_52_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1613).
yeccpars2_52_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_53_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1615).
yeccpars2_53_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_54_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1616).
yeccpars2_54_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_55_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1617).
yeccpars2_55_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_56_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1534).
yeccpars2_56_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_57_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1622).
yeccpars2_57_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_58_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1623).
yeccpars2_58_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_59_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1628).
yeccpars2_59_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_60_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1626).
yeccpars2_60_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_61_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1624).
yeccpars2_61_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_62_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1629).
yeccpars2_62_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_63_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1631).
yeccpars2_63_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_64_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1633).
yeccpars2_64_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_65_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1635).
yeccpars2_65_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_66_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1636).
yeccpars2_66_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_67_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1637).
yeccpars2_67_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_68_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1638).
yeccpars2_68_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_69_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1640).
yeccpars2_69_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_70_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1641).
yeccpars2_70_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_71_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1642).
yeccpars2_71_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_72_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1643).
yeccpars2_72_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_73_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1644).
yeccpars2_73_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_74_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1645).
yeccpars2_74_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_75_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1646).
yeccpars2_75_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_76_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1647).
yeccpars2_76_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_77_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1648).
yeccpars2_77_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_78_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1649).
yeccpars2_78_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_79_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1650).
yeccpars2_79_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_83_,1}}).
-file("megaco_text_parser_prev3c.yrl", 481).
yeccpars2_83_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_84_,1}}).
-file("megaco_text_parser_prev3c.yrl", 485).
yeccpars2_84_([__8,__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   ensure_auth_header ( __3 , __5 , __7 )
  end | Stack].

-compile({inline,{yeccpars2_85_,1}}).
-file("megaco_text_parser_prev3c.yrl", 481).
yeccpars2_85_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_87_,1}}).
-file("megaco_text_parser_prev3c.yrl", 478).
yeccpars2_87_([__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'MegacoMessage' { authHeader = __2 , mess = __3 }
  end | Stack].

-compile({inline,{yeccpars2_93_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1013).
yeccpars2_93_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_94_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1013).
yeccpars2_94_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_96_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1013).
yeccpars2_96_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_97_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1014).
yeccpars2_97_([__2,__1 | Stack]) ->
 [begin
   [ colon | __2 ]
  end | Stack].

-compile({inline,{yeccpars2_98_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1011).
yeccpars2_98_([__3,__2,__1 | Stack]) ->
 [begin
   ensure_domainAddress ( __2 , asn1_NOVALUE )
  end | Stack].

-compile({inline,{yeccpars2_100_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1018).
yeccpars2_100_([__1 | Stack]) ->
 [begin
   ensure_uint16 ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_101_,1}}).
-file("megaco_text_parser_prev3c.yrl", 481).
yeccpars2_101_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_102_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1009).
yeccpars2_102_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   ensure_domainAddress ( __2 , __5 )
  end | Stack].

-compile({inline,{yeccpars2_103_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1015).
yeccpars2_103_([__2,__1 | Stack]) ->
 [begin
   [ __1 | __2 ]
  end | Stack].

-compile({inline,{yeccpars2_105_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1001).
yeccpars2_105_([__3,__2,__1 | Stack]) ->
 [begin
   ensure_domainName ( __2 , asn1_NOVALUE )
  end | Stack].

-compile({inline,{yeccpars2_107_,1}}).
-file("megaco_text_parser_prev3c.yrl", 481).
yeccpars2_107_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_108_,1}}).
-file("megaco_text_parser_prev3c.yrl", 999).
yeccpars2_108_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   ensure_domainName ( __2 , __5 )
  end | Stack].

-compile({inline,{yeccpars2_109_,1}}).
-file("megaco_text_parser_prev3c.yrl", 500).
yeccpars2_109_([__1 | Stack]) ->
 [begin
   { transactionResponseAck , __1 }
  end | Stack].

-compile({inline,{yeccpars2_110_,1}}).
-file("megaco_text_parser_prev3c.yrl", 497).
yeccpars2_110_([__1 | Stack]) ->
 [begin
   { transactionRequest , __1 }
  end | Stack].

-compile({inline,{yeccpars2_111_,1}}).
-file("megaco_text_parser_prev3c.yrl", 498).
yeccpars2_111_([__1 | Stack]) ->
 [begin
   { transactionReply , __1 }
  end | Stack].

-compile({inline,{yeccpars2_112_,1}}).
-file("megaco_text_parser_prev3c.yrl", 499).
yeccpars2_112_([__1 | Stack]) ->
 [begin
   { transactionPending , __1 }
  end | Stack].

-compile({inline,{yeccpars2_113_,1}}).
-file("megaco_text_parser_prev3c.yrl", 492).
yeccpars2_113_([__1 | Stack]) ->
 [begin
   { transactions , __1 }
  end | Stack].

-compile({inline,{yeccpars2_114_,1}}).
-file("megaco_text_parser_prev3c.yrl", 494).
yeccpars2_114_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_115_,1}}).
-file("megaco_text_parser_prev3c.yrl", 489).
yeccpars2_115_([__3,__2,__1 | Stack]) ->
 [begin
   ensure_message ( __1 , __2 , __3 )
  end | Stack].

-compile({inline,{yeccpars2_116_,1}}).
-file("megaco_text_parser_prev3c.yrl", 491).
yeccpars2_116_([__1 | Stack]) ->
 [begin
   { messageError , __1 }
  end | Stack].

-compile({inline,{yeccpars2_124_,1}}).
-file("megaco_text_parser_prev3c.yrl", 529).
yeccpars2_124_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_127_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1006).
yeccpars2_127_([__1 | Stack]) ->
 [begin
   ensure_contextID ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_130_,1}}).
-file("megaco_text_parser_prev3c.yrl", 554).
yeccpars2_130_([__1 | Stack]) ->
 [begin
   { topology , __1 }
  end | Stack].

-compile({inline,{yeccpars2_133_,1}}).
-file("megaco_text_parser_prev3c.yrl", 555).
yeccpars2_133_([__1 | Stack]) ->
 [begin
   { priority , __1 }
  end | Stack].

-compile({inline,{yeccpars2_135_,1}}).
-file("megaco_text_parser_prev3c.yrl", 558).
yeccpars2_135_([__1 | Stack]) ->
 [begin
   { iepsCallind , __1 }
  end | Stack].

-compile({inline,{yeccpars2_136_,1}}).
-file("megaco_text_parser_prev3c.yrl", 548).
yeccpars2_136_([__1 | Stack]) ->
 [begin
   { contextProp , __1 }
  end | Stack].

-compile({inline,{yeccpars2_137_,1}}).
-file("megaco_text_parser_prev3c.yrl", 549).
yeccpars2_137_([__1 | Stack]) ->
 [begin
   { contextAudit , __1 }
  end | Stack].

-compile({inline,{yeccpars2_139_,1}}).
-file("megaco_text_parser_prev3c.yrl", 550).
yeccpars2_139_([__1 | Stack]) ->
 [begin
   { commandRequest , __1 }
  end | Stack].

-compile({inline,{yeccpars2_143_,1}}).
-file("megaco_text_parser_prev3c.yrl", 546).
yeccpars2_143_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_145_,1}}).
-file("megaco_text_parser_prev3c.yrl", 665).
yeccpars2_145_([__1 | Stack]) ->
 [begin
   { addReq , __1 }
  end | Stack].

-compile({inline,{yeccpars2_150_,1}}).
-file("megaco_text_parser_prev3c.yrl", 557).
yeccpars2_150_([__1 | Stack]) ->
 [begin
   { emergency , false }
  end | Stack].

-compile({inline,{yeccpars2_151_,1}}).
-file("megaco_text_parser_prev3c.yrl", 556).
yeccpars2_151_([__1 | Stack]) ->
 [begin
   { emergency , true }
  end | Stack].

-compile({inline,{yeccpars2_153_,1}}).
-file("megaco_text_parser_prev3c.yrl", 667).
yeccpars2_153_([__1 | Stack]) ->
 [begin
   { modReq , __1 }
  end | Stack].

-compile({inline,{yeccpars2_154_,1}}).
-file("megaco_text_parser_prev3c.yrl", 666).
yeccpars2_154_([__1 | Stack]) ->
 [begin
   { moveReq , __1 }
  end | Stack].

-compile({inline,{yeccpars2_162_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1512).
yeccpars2_162_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_163_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1508).
yeccpars2_163_([__1 | Stack]) ->
 [begin
   { tid , __1 }
  end | Stack].

-compile({inline,{yeccpars2_164_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1036).
yeccpars2_164_([__1 | Stack]) ->
 [begin
   ensure_terminationID ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_165_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1509).
yeccpars2_165_([__1 | Stack]) ->
 [begin
   { sid , __1 }
  end | Stack].

-compile({inline,{yeccpars2_166_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1516).
yeccpars2_166_([__1 | Stack]) ->
 [begin
   { direction , bothway }
  end | Stack].

-compile({inline,{yeccpars2_167_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1517).
yeccpars2_167_([__1 | Stack]) ->
 [begin
   { direction , isolate }
  end | Stack].

-compile({inline,{yeccpars2_168_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1520).
yeccpars2_168_([__1 | Stack]) ->
 [begin
   { direction_ext , onewayboth }
  end | Stack].

-compile({inline,{yeccpars2_169_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1519).
yeccpars2_169_([__1 | Stack]) ->
 [begin
   { direction_ext , onewayexternal }
  end | Stack].

-compile({inline,{yeccpars2_170_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1518).
yeccpars2_170_([__1 | Stack]) ->
 [begin
   { direction , oneway }
  end | Stack].

-compile({inline,{yeccpars2_171_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1633).
yeccpars2_171_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_173_,1}}).
-file("megaco_text_parser_prev3c.yrl", 938).
yeccpars2_173_([__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_174_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1173).
yeccpars2_174_([__1 | Stack]) ->
 [begin
   ensure_streamID ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_177_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1512).
yeccpars2_177_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_178_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1514).
yeccpars2_178_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_179_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1506).
yeccpars2_179_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_topologyDescriptor ( [ __3 | __4 ] )
  end | Stack].

-compile({inline,{yeccpars2_181_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1022).
yeccpars2_181_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_182_,1}}).
-file("megaco_text_parser_prev3c.yrl", 705).
yeccpars2_182_(Stack) ->
 [begin
   asn1_NOVALUE
  end | Stack].

-compile({inline,{yeccpars2_184_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1031).
yeccpars2_184_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_187_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1031).
yeccpars2_187_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_188_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1030).
yeccpars2_188_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_189_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1024).
yeccpars2_189_([__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_190_,1}}).
-file("megaco_text_parser_prev3c.yrl", 699).
yeccpars2_190_([__4,__3,__2,__1 | Stack]) ->
 [begin
   SR = # 'SubtractRequest' { terminationID = __3 ,
    auditDescriptor = __4 } ,
    make_commandRequest ( { subtractReq , __1 } , SR )
  end | Stack].

-compile({inline,{yeccpars2_194_,1}}).
-file("megaco_text_parser_prev3c.yrl", 757).
yeccpars2_194_(Stack) ->
 [begin
   asn1_NOVALUE
  end | Stack].

-compile({inline,{yeccpars2_195_,1}}).
-file("megaco_text_parser_prev3c.yrl", 778).
yeccpars2_195_([__1 | Stack]) ->
 [begin
   { terminationAudit , __1 }
  end | Stack].

-compile({inline,{yeccpars2_196_,1}}).
-file("megaco_text_parser_prev3c.yrl", 805).
yeccpars2_196_([__1 | Stack]) ->
 [begin
   { indAudStatisticsDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_197_,1}}).
-file("megaco_text_parser_prev3c.yrl", 799).
yeccpars2_197_([__1 | Stack]) ->
 [begin
   { indAudSignalsDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_198_,1}}).
-file("megaco_text_parser_prev3c.yrl", 807).
yeccpars2_198_([__1 | Stack]) ->
 [begin
   { indAudPackagesDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_199_,1}}).
-file("megaco_text_parser_prev3c.yrl", 795).
yeccpars2_199_([__1 | Stack]) ->
 [begin
   { indAudMediaDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_200_,1}}).
-file("megaco_text_parser_prev3c.yrl", 797).
yeccpars2_200_([__1 | Stack]) ->
 [begin
   { indAudEventsDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_201_,1}}).
-file("megaco_text_parser_prev3c.yrl", 803).
yeccpars2_201_([__1 | Stack]) ->
 [begin
   { indAudEventBufferDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_202_,1}}).
-file("megaco_text_parser_prev3c.yrl", 801).
yeccpars2_202_([__1 | Stack]) ->
 [begin
   { indAudDigitMapDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_203_,1}}).
-file("megaco_text_parser_prev3c.yrl", 792).
yeccpars2_203_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_205_,1}}).
-file("megaco_text_parser_prev3c.yrl", 760).
yeccpars2_205_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_207_,1}}).
-file("megaco_text_parser_prev3c.yrl", 930).
yeccpars2_207_([__1 | Stack]) ->
 [begin
   ensure_IADMD ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_208_,1}}).
-file("megaco_text_parser_prev3c.yrl", 767).
yeccpars2_208_([__1 | Stack]) ->
 [begin
   digitMapToken
  end | Stack].

-compile({inline,{yeccpars2_209_,1}}).
-file("megaco_text_parser_prev3c.yrl", 776).
yeccpars2_209_([__1 | Stack]) ->
 [begin
   eventBufferToken
  end | Stack].

-compile({inline,{yeccpars2_210_,1}}).
-file("megaco_text_parser_prev3c.yrl", 777).
yeccpars2_210_([__1 | Stack]) ->
 [begin
   eventsToken
  end | Stack].

-compile({inline,{yeccpars2_211_,1}}).
-file("megaco_text_parser_prev3c.yrl", 766).
yeccpars2_211_([__1 | Stack]) ->
 [begin
   mediaToken
  end | Stack].

-compile({inline,{yeccpars2_212_,1}}).
-file("megaco_text_parser_prev3c.yrl", 765).
yeccpars2_212_([__1 | Stack]) ->
 [begin
   modemToken
  end | Stack].

-compile({inline,{yeccpars2_213_,1}}).
-file("megaco_text_parser_prev3c.yrl", 764).
yeccpars2_213_([__1 | Stack]) ->
 [begin
   muxToken
  end | Stack].

-compile({inline,{yeccpars2_214_,1}}).
-file("megaco_text_parser_prev3c.yrl", 769).
yeccpars2_214_([__1 | Stack]) ->
 [begin
   observedEventsToken
  end | Stack].

-compile({inline,{yeccpars2_215_,1}}).
-file("megaco_text_parser_prev3c.yrl", 770).
yeccpars2_215_([__1 | Stack]) ->
 [begin
   packagesToken
  end | Stack].

-compile({inline,{yeccpars2_216_,1}}).
-file("megaco_text_parser_prev3c.yrl", 775).
yeccpars2_216_([__1 | Stack]) ->
 [begin
   signalsToken
  end | Stack].

-compile({inline,{yeccpars2_217_,1}}).
-file("megaco_text_parser_prev3c.yrl", 768).
yeccpars2_217_([__1 | Stack]) ->
 [begin
   statsToken
  end | Stack].

-compile({inline,{yeccpars2_219_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1175).
yeccpars2_219_([__1 | Stack]) ->
 [begin
   ensure_pkgdName ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_221_,1}}).
-file("megaco_text_parser_prev3c.yrl", 933).
yeccpars2_221_([__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'IndAudStatisticsDescriptor' { statName = __3 }
  end | Stack].

-compile({inline,{yeccpars2_222_,1}}).
-file("megaco_text_parser_prev3c.yrl", 910).
yeccpars2_222_([__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_224_,1}}).
-file("megaco_text_parser_prev3c.yrl", 917).
yeccpars2_224_([__1 | Stack]) ->
 [begin
   { signal , ensure_indAudSignal ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_225_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1284).
yeccpars2_225_([__1 | Stack]) ->
 [begin
   merge_signalRequest ( __1 , [ ] )
  end | Stack].

-compile({inline,{yeccpars2_228_,1}}).
-file("megaco_text_parser_prev3c.yrl", 916).
yeccpars2_228_([__1 | Stack]) ->
 [begin
   { seqSigList , __1 }
  end | Stack].

-compile({inline,{yeccpars2_229_,1}}).
-file("megaco_text_parser_prev3c.yrl", 913).
yeccpars2_229_([__2,__1 | Stack]) ->
 [begin
   asn1_NOVALUE
  end | Stack].

-compile({inline,{yeccpars2_230_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1629).
yeccpars2_230_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_232_,1}}).
-file("megaco_text_parser_prev3c.yrl", 920).
yeccpars2_232_([__3,__2,__1 | Stack]) ->
 [begin
   # 'IndAudSeqSigList' { id = ensure_uint16 ( __3 ) }
  end | Stack].

-compile({inline,{yeccpars2_233_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1354).
yeccpars2_233_([__1 | Stack]) ->
 [begin
   ensure_uint16 ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_237_,1}}).
-file("megaco_text_parser_prev3c.yrl", 923).
yeccpars2_237_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'IndAudSeqSigList' { id = ensure_uint16 ( __3 ) ,
    signalList =
    ensure_indAudSignalListParm ( __5 ) }
  end | Stack].

-compile({inline,{yeccpars2_238_,1}}).
-file("megaco_text_parser_prev3c.yrl", 914).
yeccpars2_238_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_240_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1287).
yeccpars2_240_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_242_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1551).
yeccpars2_242_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_243_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1555).
yeccpars2_243_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_245_COMMA,1}}).
-file("megaco_text_parser_prev3c.yrl", 1318).
yeccpars2_245_COMMA([__1 | Stack]) ->
 [begin
   keepActive
  end | Stack].

-compile({inline,{yeccpars2_245_RBRKT,1}}).
-file("megaco_text_parser_prev3c.yrl", 1318).
yeccpars2_245_RBRKT([__1 | Stack]) ->
 [begin
   keepActive
  end | Stack].

-compile({inline,{yeccpars2_245_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1578).
yeccpars2_245_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_246_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1596).
yeccpars2_246_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_247_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1615).
yeccpars2_247_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_248_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1631).
yeccpars2_248_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_249_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1633).
yeccpars2_249_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_251_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1310).
yeccpars2_251_([__3,__2,__1 | Stack]) ->
 [begin
   { stream , __3 }
  end | Stack].

-compile({inline,{yeccpars2_253_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1312).
yeccpars2_253_([__3,__2,__1 | Stack]) ->
 [begin
   { signal_type , __3 }
  end | Stack].

-compile({inline,{yeccpars2_254_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1330).
yeccpars2_254_([__1 | Stack]) ->
 [begin
   brief
  end | Stack].

-compile({inline,{yeccpars2_255_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1328).
yeccpars2_255_([__1 | Stack]) ->
 [begin
   onOff
  end | Stack].

-compile({inline,{yeccpars2_256_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1329).
yeccpars2_256_([__1 | Stack]) ->
 [begin
   timeOut
  end | Stack].

-compile({inline,{yeccpars2_258_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1388).
yeccpars2_258_([__1 | Stack]) ->
 [begin
   ensure_requestID ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_259_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1322).
yeccpars2_259_([__3,__2,__1 | Stack]) ->
 [begin
   { requestId , __3 }
  end | Stack].

-compile({inline,{yeccpars2_262_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1337).
yeccpars2_262_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_263_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1340).
yeccpars2_263_([__1 | Stack]) ->
 [begin
   onInterruptByEvent
  end | Stack].

-compile({inline,{yeccpars2_264_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1341).
yeccpars2_264_([__1 | Stack]) ->
 [begin
   onInterruptByNewSignalDescr
  end | Stack].

-compile({inline,{yeccpars2_265_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1343).
yeccpars2_265_([__1 | Stack]) ->
 [begin
   iteration
  end | Stack].

-compile({inline,{yeccpars2_266_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1342).
yeccpars2_266_([__1 | Stack]) ->
 [begin
   otherReason
  end | Stack].

-compile({inline,{yeccpars2_267_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1339).
yeccpars2_267_([__1 | Stack]) ->
 [begin
   onTimeOut
  end | Stack].

-compile({inline,{yeccpars2_270_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1337).
yeccpars2_270_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_271_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1336).
yeccpars2_271_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_272_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1317).
yeccpars2_272_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { notify_completion , [ __4 | __5 ] }
  end | Stack].

-compile({inline,{yeccpars2_274_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1324).
yeccpars2_274_([__3,__2,__1 | Stack]) ->
 [begin
   { intersigDelay , ensure_uint16 ( __3 ) }
  end | Stack].

-compile({inline,{yeccpars2_276_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1314).
yeccpars2_276_([__3,__2,__1 | Stack]) ->
 [begin
   { duration , ensure_uint16 ( __3 ) }
  end | Stack].

-compile({inline,{yeccpars2_278_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1320).
yeccpars2_278_([__3,__2,__1 | Stack]) ->
 [begin
   { direction , __3 }
  end | Stack].

-compile({inline,{yeccpars2_279_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1334).
yeccpars2_279_([__1 | Stack]) ->
 [begin
   both
  end | Stack].

-compile({inline,{yeccpars2_280_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1332).
yeccpars2_280_([__1 | Stack]) ->
 [begin
   external
  end | Stack].

-compile({inline,{yeccpars2_281_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1333).
yeccpars2_281_([__1 | Stack]) ->
 [begin
   internal
  end | Stack].

-compile({inline,{yeccpars2_282_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1326).
yeccpars2_282_([__2,__1 | Stack]) ->
 [begin
   { other , ensure_NAME ( __1 ) , __2 }
  end | Stack].

-compile({inline,{yeccpars2_287_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1108).
yeccpars2_287_([__2,__1 | Stack]) ->
 [begin
   # 'PropertyParm' { value = [ __2 ] ,
    extraInfo = { relation , unequalTo } }
  end | Stack].

-compile({inline,{yeccpars2_288_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1532).
yeccpars2_288_([__1 | Stack]) ->
 [begin
   ensure_value ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_289_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1531).
yeccpars2_289_([__1 | Stack]) ->
 [begin
   ensure_value ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_290_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1111).
yeccpars2_290_([__2,__1 | Stack]) ->
 [begin
   # 'PropertyParm' { value = [ __2 ] ,
    extraInfo = { relation , smallerThan } }
  end | Stack].

-compile({inline,{yeccpars2_291_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1114).
yeccpars2_291_([__2,__1 | Stack]) ->
 [begin
   # 'PropertyParm' { value = [ __2 ] ,
    extraInfo = { relation , greaterThan } }
  end | Stack].

-compile({inline,{yeccpars2_292_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1135).
yeccpars2_292_([__1 | Stack]) ->
 [begin
   # 'PropertyParm' { value = [ __1 ] }
  end | Stack].

-compile({inline,{yeccpars2_293_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1105).
yeccpars2_293_([__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_296_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1138).
yeccpars2_296_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_300_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1138).
yeccpars2_300_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_301_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1137).
yeccpars2_301_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_303_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1127).
yeccpars2_303_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'PropertyParm' { value = [ __2 , __4 ] ,
    extraInfo = { range , true } }
  end | Stack].

-compile({inline,{yeccpars2_304_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1131).
yeccpars2_304_([__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'PropertyParm' { value = [ __2 | __3 ] ,
    extraInfo = { sublist , true } }
  end | Stack].

-compile({inline,{yeccpars2_305_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1138).
yeccpars2_305_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_307_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1123).
yeccpars2_307_([__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'PropertyParm' { value = [ __2 | __3 ] ,
    extraInfo = { sublist , false } }
  end | Stack].

-compile({inline,{yeccpars2_310_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1287).
yeccpars2_310_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_311_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1286).
yeccpars2_311_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_312_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1283).
yeccpars2_312_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_signalRequest ( __1 , [ __3 | __4 ] )
  end | Stack].

-compile({inline,{yeccpars2_314_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1480).
yeccpars2_314_([__1 | Stack]) ->
 [begin
   ensure_packagesItem ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_316_,1}}).
-file("megaco_text_parser_prev3c.yrl", 936).
yeccpars2_316_([__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_indAudPackagesDescriptor ( __3 )
  end | Stack].

-compile({inline,{yeccpars2_318_,1}}).
-file("megaco_text_parser_prev3c.yrl", 820).
yeccpars2_318_([__1 | Stack]) ->
 [begin
   { termStateDescr , __1 }
  end | Stack].

-compile({inline,{yeccpars2_319_,1}}).
-file("megaco_text_parser_prev3c.yrl", 818).
yeccpars2_319_([__1 | Stack]) ->
 [begin
   { streamParm , __1 }
  end | Stack].

-compile({inline,{yeccpars2_320_,1}}).
-file("megaco_text_parser_prev3c.yrl", 819).
yeccpars2_320_([__1 | Stack]) ->
 [begin
   { streamDescr , __1 }
  end | Stack].

-compile({inline,{yeccpars2_321_,1}}).
-file("megaco_text_parser_prev3c.yrl", 835).
yeccpars2_321_([__1 | Stack]) ->
 [begin
   # 'IndAudStreamParms' { statisticsDescriptor = __1 }
  end | Stack].

-compile({inline,{yeccpars2_322_,1}}).
-file("megaco_text_parser_prev3c.yrl", 823).
yeccpars2_322_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_323_,1}}).
-file("megaco_text_parser_prev3c.yrl", 833).
yeccpars2_323_([__1 | Stack]) ->
 [begin
   # 'IndAudStreamParms' { localControlDescriptor = __1 }
  end | Stack].

-compile({inline,{yeccpars2_325_,1}}).
-file("megaco_text_parser_prev3c.yrl", 830).
yeccpars2_325_([__1 | Stack]) ->
 [begin
   LD = ensure_prop_groups ( __1 ) ,
    # 'IndAudStreamParms' { localDescriptor = LD }
  end | Stack].

-compile({inline,{yeccpars2_326_,1}}).
-file("megaco_text_parser_prev3c.yrl", 827).
yeccpars2_326_([__1 | Stack]) ->
 [begin
   RD = ensure_prop_groups ( __1 ) ,
    # 'IndAudStreamParms' { remoteDescriptor = RD }
  end | Stack].

-compile({inline,{yeccpars2_331_,1}}).
-file("megaco_text_parser_prev3c.yrl", 876).
yeccpars2_331_([__1 | Stack]) ->
 [begin
   { prop , __1 }
  end | Stack].

-compile({inline,{yeccpars2_332_,1}}).
-file("megaco_text_parser_prev3c.yrl", 877).
yeccpars2_332_([__1 | Stack]) ->
 [begin
   { name , __1 }
  end | Stack].

-compile({inline,{yeccpars2_335_,1}}).
-file("megaco_text_parser_prev3c.yrl", 875).
yeccpars2_335_([__1 | Stack]) ->
 [begin
   bufferToken
  end | Stack].

-compile({inline,{yeccpars2_336_,1}}).
-file("megaco_text_parser_prev3c.yrl", 880).
yeccpars2_336_([__1 | Stack]) ->
 [begin
   serviceStatesToken
  end | Stack].

-compile({inline,{yeccpars2_339_,1}}).
-file("megaco_text_parser_prev3c.yrl", 884).
yeccpars2_339_([__3,__2,__1 | Stack]) ->
 [begin
   { serviceStates , { inequal , __3 } }
  end | Stack].

-compile({inline,{yeccpars2_340_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1160).
yeccpars2_340_([__1 | Stack]) ->
 [begin
   inSvc
  end | Stack].

-compile({inline,{yeccpars2_341_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1159).
yeccpars2_341_([__1 | Stack]) ->
 [begin
   outOfSvc
  end | Stack].

-compile({inline,{yeccpars2_342_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1158).
yeccpars2_342_([__1 | Stack]) ->
 [begin
   test
  end | Stack].

-compile({inline,{yeccpars2_343_,1}}).
-file("megaco_text_parser_prev3c.yrl", 882).
yeccpars2_343_([__3,__2,__1 | Stack]) ->
 [begin
   { serviceStates , { equal , __3 } }
  end | Stack].

-compile({inline,{yeccpars2_344_,1}}).
-file("megaco_text_parser_prev3c.yrl", 868).
yeccpars2_344_([__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_indAudTerminationStateDescriptor ( __3 )
  end | Stack].

-compile({inline,{yeccpars2_345_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1102).
yeccpars2_345_([__2,__1 | Stack]) ->
 [begin
   setelement ( # 'PropertyParm' .name , __2 , __1 )
  end | Stack].

-compile({inline,{yeccpars2_350_,1}}).
-file("megaco_text_parser_prev3c.yrl", 839).
yeccpars2_350_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'IndAudStreamDescriptor' { streamID = __3 ,
    streamParms = __5 }
  end | Stack].

-compile({inline,{yeccpars2_352_,1}}).
-file("megaco_text_parser_prev3c.yrl", 862).
yeccpars2_352_([__1 | Stack]) ->
 [begin
   { prop , __1 }
  end | Stack].

-compile({inline,{yeccpars2_353_,1}}).
-file("megaco_text_parser_prev3c.yrl", 863).
yeccpars2_353_([__1 | Stack]) ->
 [begin
   { name , __1 }
  end | Stack].

-compile({inline,{yeccpars2_354_,1}}).
-file("megaco_text_parser_prev3c.yrl", 850).
yeccpars2_354_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_355_,1}}).
-file("megaco_text_parser_prev3c.yrl", 859).
yeccpars2_355_([__1 | Stack]) ->
 [begin
   modeToken
  end | Stack].

-compile({inline,{yeccpars2_356_,1}}).
-file("megaco_text_parser_prev3c.yrl", 857).
yeccpars2_356_([__1 | Stack]) ->
 [begin
   reservedGroupToken
  end | Stack].

-compile({inline,{yeccpars2_357_,1}}).
-file("megaco_text_parser_prev3c.yrl", 858).
yeccpars2_357_([__1 | Stack]) ->
 [begin
   reservedValueToken
  end | Stack].

-compile({inline,{yeccpars2_360_,1}}).
-file("megaco_text_parser_prev3c.yrl", 861).
yeccpars2_360_([__3,__2,__1 | Stack]) ->
 [begin
   { mode , { inequal , __3 } }
  end | Stack].

-compile({inline,{yeccpars2_361_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1098).
yeccpars2_361_([__1 | Stack]) ->
 [begin
   inactive
  end | Stack].

-compile({inline,{yeccpars2_362_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1099).
yeccpars2_362_([__1 | Stack]) ->
 [begin
   loopBack
  end | Stack].

-compile({inline,{yeccpars2_363_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1096).
yeccpars2_363_([__1 | Stack]) ->
 [begin
   recvOnly
  end | Stack].

-compile({inline,{yeccpars2_364_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1095).
yeccpars2_364_([__1 | Stack]) ->
 [begin
   sendOnly
  end | Stack].

-compile({inline,{yeccpars2_365_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1097).
yeccpars2_365_([__1 | Stack]) ->
 [begin
   sendRecv
  end | Stack].

-compile({inline,{yeccpars2_366_,1}}).
-file("megaco_text_parser_prev3c.yrl", 860).
yeccpars2_366_([__3,__2,__1 | Stack]) ->
 [begin
   { mode , { equal , __3 } }
  end | Stack].

-compile({inline,{yeccpars2_369_,1}}).
-file("megaco_text_parser_prev3c.yrl", 850).
yeccpars2_369_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_370_,1}}).
-file("megaco_text_parser_prev3c.yrl", 849).
yeccpars2_370_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_371_,1}}).
-file("megaco_text_parser_prev3c.yrl", 846).
yeccpars2_371_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_indAudLocalControlDescriptor ( [ __3 | __4 ] )
  end | Stack].

-compile({inline,{yeccpars2_374_,1}}).
-file("megaco_text_parser_prev3c.yrl", 823).
yeccpars2_374_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_375_,1}}).
-file("megaco_text_parser_prev3c.yrl", 822).
yeccpars2_375_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_376_,1}}).
-file("megaco_text_parser_prev3c.yrl", 812).
yeccpars2_376_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_indAudMediaDescriptor ( [ __3 | __4 ] )
  end | Stack].

-compile({inline,{yeccpars2_381_,1}}).
-file("megaco_text_parser_prev3c.yrl", 901).
yeccpars2_381_([__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'IndAudEventsDescriptor' { pkgdName = __3 }
  end | Stack].

-compile({inline,{yeccpars2_385_,1}}).
-file("megaco_text_parser_prev3c.yrl", 904).
yeccpars2_385_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'IndAudEventsDescriptor' { requestID = __3 ,
    pkgdName = __5 }
  end | Stack].

-compile({inline,{yeccpars2_387_,1}}).
-file("megaco_text_parser_prev3c.yrl", 894).
yeccpars2_387_(Stack) ->
 [begin
   asn1_NOVALUE
  end | Stack].

-compile({inline,{yeccpars2_389_,1}}).
-file("megaco_text_parser_prev3c.yrl", 887).
yeccpars2_389_([__4,__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_390_,1}}).
-file("megaco_text_parser_prev3c.yrl", 890).
yeccpars2_390_([__2,__1 | Stack]) ->
 [begin
   merge_indAudEventBufferDescriptor ( __1 , __2 )
  end | Stack].

-compile({inline,{yeccpars2_392_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1265).
yeccpars2_392_([__1 | Stack]) ->
 [begin
   ensure_NAME ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_394_,1}}).
-file("megaco_text_parser_prev3c.yrl", 897).
yeccpars2_394_([__1 | Stack]) ->
 [begin
   { streamID , __1 }
  end | Stack].

-compile({inline,{yeccpars2_395_,1}}).
-file("megaco_text_parser_prev3c.yrl", 898).
yeccpars2_395_([__1 | Stack]) ->
 [begin
   { eventParameterName , __1 }
  end | Stack].

-compile({inline,{yeccpars2_396_,1}}).
-file("megaco_text_parser_prev3c.yrl", 893).
yeccpars2_396_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_397_,1}}).
-file("megaco_text_parser_prev3c.yrl", 754).
yeccpars2_397_([__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_auditDescriptor ( __3 )
  end | Stack].

-compile({inline,{yeccpars2_398_,1}}).
-file("megaco_text_parser_prev3c.yrl", 756).
yeccpars2_398_([__2,__1 | Stack]) ->
 [begin
   [ __1 | __2 ]
  end | Stack].

-compile({inline,{yeccpars2_400_,1}}).
-file("megaco_text_parser_prev3c.yrl", 760).
yeccpars2_400_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_401_,1}}).
-file("megaco_text_parser_prev3c.yrl", 759).
yeccpars2_401_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_402_,1}}).
-file("megaco_text_parser_prev3c.yrl", 787).
yeccpars2_402_([__2,__1 | Stack]) ->
 [begin
   [ __1 | __2 ]
  end | Stack].

-compile({inline,{yeccpars2_404_,1}}).
-file("megaco_text_parser_prev3c.yrl", 792).
yeccpars2_404_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_410_,1}}).
-file("megaco_text_parser_prev3c.yrl", 791).
yeccpars2_410_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_411_,1}}).
-file("megaco_text_parser_prev3c.yrl", 704).
yeccpars2_411_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_418_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1430).
yeccpars2_418_([__1 | Stack]) ->
 [begin
   { time_stamp , __1 }
  end | Stack].

-compile({inline,{yeccpars2_419_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1432).
yeccpars2_419_([__1 | Stack]) ->
 [begin
   { version , __1 }
  end | Stack].

-compile({inline,{yeccpars2_420_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1425).
yeccpars2_420_([__1 | Stack]) ->
 [begin
   { reason , __1 }
  end | Stack].

-compile({inline,{yeccpars2_421_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1428).
yeccpars2_421_([__1 | Stack]) ->
 [begin
   { profile , __1 }
  end | Stack].

-compile({inline,{yeccpars2_422_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1422).
yeccpars2_422_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_423_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1431).
yeccpars2_423_([__1 | Stack]) ->
 [begin
   { mgc_id , __1 }
  end | Stack].

-compile({inline,{yeccpars2_424_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1424).
yeccpars2_424_([__1 | Stack]) ->
 [begin
   { method , __1 }
  end | Stack].

-compile({inline,{yeccpars2_425_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1426).
yeccpars2_425_([__1 | Stack]) ->
 [begin
   { delay , __1 }
  end | Stack].

-compile({inline,{yeccpars2_426_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1427).
yeccpars2_426_([__1 | Stack]) ->
 [begin
   { address , __1 }
  end | Stack].

-compile({inline,{yeccpars2_427_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1529).
yeccpars2_427_([__1 | Stack]) ->
 [begin
   ensure_extensionParameter ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_429_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1429).
yeccpars2_429_([__1 | Stack]) ->
 [begin
   { extension , __1 }
  end | Stack].

-compile({inline,{yeccpars2_430_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1434).
yeccpars2_430_([__1 | Stack]) ->
 [begin
   { audit_item , __1 }
  end | Stack].

-compile({inline,{yeccpars2_431_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1554).
yeccpars2_431_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_432_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1586).
yeccpars2_432_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_433_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1587).
yeccpars2_433_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_434_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1610).
yeccpars2_434_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_435_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1611).
yeccpars2_435_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_436_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1628).
yeccpars2_436_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_437_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1433).
yeccpars2_437_([__1 | Stack]) ->
 [begin
   incomplete
  end | Stack].

-compile({inline,{yeccpars2_438_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1482).
yeccpars2_438_([__1 | Stack]) ->
 [begin
   ensure_timeStamp ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_439_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1650).
yeccpars2_439_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_441_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1451).
yeccpars2_441_([__3,__2,__1 | Stack]) ->
 [begin
   ensure_version ( __3 )
  end | Stack].

-compile({inline,{yeccpars2_442_,1}}).
-file("megaco_text_parser_prev3c.yrl", 481).
yeccpars2_442_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_443_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1445).
yeccpars2_443_([__3,__2,__1 | Stack]) ->
 [begin
   { portNumber , __3 }
  end | Stack].

-compile({inline,{yeccpars2_444_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1443).
yeccpars2_444_([__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_446_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1439).
yeccpars2_446_([__3,__2,__1 | Stack]) ->
 [begin
   [ __3 ]
  end | Stack].

-compile({inline,{yeccpars2_448_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1449).
yeccpars2_448_([__3,__2,__1 | Stack]) ->
 [begin
   ensure_profile ( __3 )
  end | Stack].

-compile({inline,{yeccpars2_449_,1}}).
-file("megaco_text_parser_prev3c.yrl", 481).
yeccpars2_449_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_450_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1447).
yeccpars2_450_([__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_452_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1437).
yeccpars2_452_([__3,__2,__1 | Stack]) ->
 [begin
   ensure_serviceChangeMethod ( __3 )
  end | Stack].

-compile({inline,{yeccpars2_454_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1441).
yeccpars2_454_([__3,__2,__1 | Stack]) ->
 [begin
   ensure_uint32 ( __3 )
  end | Stack].

-compile({inline,{yeccpars2_455_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1454).
yeccpars2_455_([__2,__1 | Stack]) ->
 [begin
   setelement ( # 'PropertyParm' .name , __2 , __1 )
  end | Stack].

-compile({inline,{yeccpars2_458_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1422).
yeccpars2_458_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_459_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1421).
yeccpars2_459_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_460_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1418).
yeccpars2_460_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_ServiceChangeParm ( [ __3 | __4 ] )
  end | Stack].

-compile({inline,{yeccpars2_461_,1}}).
-file("megaco_text_parser_prev3c.yrl", 964).
yeccpars2_461_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   make_commandRequest ( { serviceChangeReq , __1 } ,
    # 'ServiceChangeRequest' { terminationID = __3 ,
    serviceChangeParms = __5 } )
  end | Stack].

-compile({inline,{yeccpars2_463_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1527).
yeccpars2_463_([__3,__2,__1 | Stack]) ->
 [begin
   ensure_uint16 ( __3 )
  end | Stack].

-compile({inline,{yeccpars2_467_,1}}).
-file("megaco_text_parser_prev3c.yrl", 951).
yeccpars2_467_([__1 | Stack]) ->
 [begin
   # 'NotifyRequest' { observedEventsDescriptor = __1 }
  end | Stack].

-compile({inline,{yeccpars2_469_,1}}).
-file("megaco_text_parser_prev3c.yrl", 953).
yeccpars2_469_([__1 | Stack]) ->
 [begin
   # 'NotifyRequest' { errorDescriptor = __1 }
  end | Stack].

-compile({inline,{yeccpars2_473_,1}}).
-file("megaco_text_parser_prev3c.yrl", 481).
yeccpars2_473_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_474_,1}}).
-file("megaco_text_parser_prev3c.yrl", 481).
yeccpars2_474_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_476_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1368).
yeccpars2_476_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_478_,1}}).
-file("megaco_text_parser_prev3c.yrl", 481).
yeccpars2_478_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_479_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1368).
yeccpars2_479_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_480_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1367).
yeccpars2_480_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_481_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1364).
yeccpars2_481_([__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'ObservedEventsDescriptor' { requestId = __3 ,
    observedEventLst = [ __5 | __6 ] }
  end | Stack].

-compile({inline,{yeccpars2_482_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1380).
yeccpars2_482_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_483_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1375).
yeccpars2_483_([__3,__2,__1 | Stack]) ->
 [begin
   merge_observed_event ( __3 , __2 , asn1_NOVALUE )
  end | Stack].

-compile({inline,{yeccpars2_485_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1383).
yeccpars2_485_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_488_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1263).
yeccpars2_488_([__2,__1 | Stack]) ->
 [begin
   select_stream_or_other ( __1 , __2 )
  end | Stack].

-compile({inline,{yeccpars2_491_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1383).
yeccpars2_491_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_492_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1382).
yeccpars2_492_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_493_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1379).
yeccpars2_493_([__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_495_,1}}).
-file("megaco_text_parser_prev3c.yrl", 481).
yeccpars2_495_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_497_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1380).
yeccpars2_497_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_498_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1373).
yeccpars2_498_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_observed_event ( __6 , __5 , __1 )
  end | Stack].

-compile({inline,{yeccpars2_499_,1}}).
-file("megaco_text_parser_prev3c.yrl", 946).
yeccpars2_499_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   NR = setelement ( # 'NotifyRequest' .terminationID ,
    __5 , __3 ) ,
    make_commandRequest ( { notifyReq , __1 } , NR )
  end | Stack].

-compile({inline,{yeccpars2_501_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1522).
yeccpars2_501_([__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_502_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1092).
yeccpars2_502_([__1 | Stack]) ->
 [begin
   false
  end | Stack].

-compile({inline,{yeccpars2_503_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1091).
yeccpars2_503_([__1 | Stack]) ->
 [begin
   true
  end | Stack].

-compile({inline,{yeccpars2_505_,1}}).
-file("megaco_text_parser_prev3c.yrl", 599).
yeccpars2_505_([__1 | Stack]) ->
 [begin
   { select_prio , __1 }
  end | Stack].

-compile({inline,{yeccpars2_506_,1}}).
-file("megaco_text_parser_prev3c.yrl", 595).
yeccpars2_506_([__1 | Stack]) ->
 [begin
   { prop , __1 }
  end | Stack].

-compile({inline,{yeccpars2_508_,1}}).
-file("megaco_text_parser_prev3c.yrl", 601).
yeccpars2_508_([__1 | Stack]) ->
 [begin
   { select_ieps , __1 }
  end | Stack].

-compile({inline,{yeccpars2_509_,1}}).
-file("megaco_text_parser_prev3c.yrl", 600).
yeccpars2_509_([__1 | Stack]) ->
 [begin
   { select_emergency , __1 }
  end | Stack].

-compile({inline,{yeccpars2_511_,1}}).
-file("megaco_text_parser_prev3c.yrl", 588).
yeccpars2_511_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_513_,1}}).
-file("megaco_text_parser_prev3c.yrl", 602).
yeccpars2_513_([__1 | Stack]) ->
 [begin
   { select_logic , __1 }
  end | Stack].

-compile({inline,{yeccpars2_514_,1}}).
-file("megaco_text_parser_prev3c.yrl", 605).
yeccpars2_514_([__1 | Stack]) ->
 [begin
   { andAUDITSelect , 'NULL' }
  end | Stack].

-compile({inline,{yeccpars2_516_,1}}).
-file("megaco_text_parser_prev3c.yrl", 592).
yeccpars2_516_([__1 | Stack]) ->
 [begin
   emergencyAudit
  end | Stack].

-compile({inline,{yeccpars2_518_,1}}).
-file("megaco_text_parser_prev3c.yrl", 594).
yeccpars2_518_([__1 | Stack]) ->
 [begin
   iepsCallind
  end | Stack].

-compile({inline,{yeccpars2_519_,1}}).
-file("megaco_text_parser_prev3c.yrl", 606).
yeccpars2_519_([__1 | Stack]) ->
 [begin
   { orAUDITSelect , 'NULL' }
  end | Stack].

-compile({inline,{yeccpars2_520_,1}}).
-file("megaco_text_parser_prev3c.yrl", 593).
yeccpars2_520_([__1 | Stack]) ->
 [begin
   priorityAudit
  end | Stack].

-compile({inline,{yeccpars2_521_,1}}).
-file("megaco_text_parser_prev3c.yrl", 591).
yeccpars2_521_([__1 | Stack]) ->
 [begin
   topologyAudit
  end | Stack].

-compile({inline,{yeccpars2_523_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1525).
yeccpars2_523_([__3,__2,__1 | Stack]) ->
 [begin
   false
  end | Stack].

-compile({inline,{yeccpars2_524_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1524).
yeccpars2_524_([__3,__2,__1 | Stack]) ->
 [begin
   true
  end | Stack].

-compile({inline,{yeccpars2_527_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1404).
yeccpars2_527_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_528_,1}}).
-file("megaco_text_parser_prev3c.yrl", 595).
yeccpars2_528_([__1 | Stack]) ->
 [begin
   { prop , __1 }
  end | Stack].

-compile({inline,{yeccpars2_530_,1}}).
-file("megaco_text_parser_prev3c.yrl", 588).
yeccpars2_530_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_534_,1}}).
-file("megaco_text_parser_prev3c.yrl", 570).
yeccpars2_534_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_537_,1}}).
-file("megaco_text_parser_prev3c.yrl", 570).
yeccpars2_537_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_538_,1}}).
-file("megaco_text_parser_prev3c.yrl", 569).
yeccpars2_538_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_539_,1}}).
-file("megaco_text_parser_prev3c.yrl", 567).
yeccpars2_539_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __4 | __5 ]
  end | Stack].

-compile({inline,{yeccpars2_542_,1}}).
-file("megaco_text_parser_prev3c.yrl", 588).
yeccpars2_542_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_543_,1}}).
-file("megaco_text_parser_prev3c.yrl", 587).
yeccpars2_543_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_544_,1}}).
-file("megaco_text_parser_prev3c.yrl", 584).
yeccpars2_544_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __3 | __4 ]
  end | Stack].

-compile({inline,{yeccpars2_545_,1}}).
-file("megaco_text_parser_prev3c.yrl", 564).
yeccpars2_545_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { contextList , __3 }
  end | Stack].

-compile({inline,{yeccpars2_546_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1402).
yeccpars2_546_([__2,__1 | Stack]) ->
 [begin
   [ __1 | __2 ]
  end | Stack].

-compile({inline,{yeccpars2_548_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1404).
yeccpars2_548_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_550_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1403).
yeccpars2_550_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_551_,1}}).
-file("megaco_text_parser_prev3c.yrl", 562).
yeccpars2_551_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { contextProp , __3 }
  end | Stack].

-compile({inline,{yeccpars2_553_,1}}).
-file("megaco_text_parser_prev3c.yrl", 578).
yeccpars2_553_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_context_attr_audit_request (
    # 'ContextAttrAuditRequest' { } , [ __3 | __4 ] )
  end | Stack].

-compile({inline,{yeccpars2_554_,1}}).
-file("megaco_text_parser_prev3c.yrl", 574).
yeccpars2_554_([__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_context_attr_audit_request (
    # 'ContextAttrAuditRequest' { } , __3 )
  end | Stack].

-compile({inline,{yeccpars2_557_,1}}).
-file("megaco_text_parser_prev3c.yrl", 705).
yeccpars2_557_(Stack) ->
 [begin
   asn1_NOVALUE
  end | Stack].

-compile({inline,{yeccpars2_558_,1}}).
-file("megaco_text_parser_prev3c.yrl", 708).
yeccpars2_558_([__4,__3,__2,__1 | Stack]) ->
 [begin
   make_commandRequest ( { auditValueRequest , __1 } ,
    make_auditRequest ( __3 , __4 ) )
  end | Stack].

-compile({inline,{yeccpars2_560_,1}}).
-file("megaco_text_parser_prev3c.yrl", 705).
yeccpars2_560_(Stack) ->
 [begin
   asn1_NOVALUE
  end | Stack].

-compile({inline,{yeccpars2_561_,1}}).
-file("megaco_text_parser_prev3c.yrl", 711).
yeccpars2_561_([__4,__3,__2,__1 | Stack]) ->
 [begin
   make_commandRequest ( { auditCapRequest , __1 } ,
    make_auditRequest ( __3 , __4 ) )
  end | Stack].

-compile({inline,{yeccpars2_562_,1}}).
-file("megaco_text_parser_prev3c.yrl", 540).
yeccpars2_562_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_action_request ( __3 , __5 )
  end | Stack].

-compile({inline,{yeccpars2_563_,1}}).
-file("megaco_text_parser_prev3c.yrl", 542).
yeccpars2_563_([__2,__1 | Stack]) ->
 [begin
   [ __1 | __2 ]
  end | Stack].

-compile({inline,{yeccpars2_565_,1}}).
-file("megaco_text_parser_prev3c.yrl", 546).
yeccpars2_565_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_566_,1}}).
-file("megaco_text_parser_prev3c.yrl", 545).
yeccpars2_566_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_568_,1}}).
-file("megaco_text_parser_prev3c.yrl", 670).
yeccpars2_568_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_569_,1}}).
-file("megaco_text_parser_prev3c.yrl", 660).
yeccpars2_569_([__4,__3,__2,__1 | Stack]) ->
 [begin
   Descs = merge_AmmRequest_descriptors ( __4 , [ ] ) ,
    make_commandRequest ( __1 ,
    # 'AmmRequest' { terminationID = __3 ,
    descriptors = Descs } )
  end | Stack].

-compile({inline,{yeccpars2_571_,1}}).
-file("megaco_text_parser_prev3c.yrl", 684).
yeccpars2_571_([__1 | Stack]) ->
 [begin
   { statisticsDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_572_,1}}).
-file("megaco_text_parser_prev3c.yrl", 681).
yeccpars2_572_([__1 | Stack]) ->
 [begin
   { signalsDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_573_,1}}).
-file("megaco_text_parser_prev3c.yrl", 678).
yeccpars2_573_([__1 | Stack]) ->
 [begin
   { muxDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_574_,1}}).
-file("megaco_text_parser_prev3c.yrl", 677).
yeccpars2_574_([__1 | Stack]) ->
 [begin
   { modemDescriptor , deprecated }
  end | Stack].

-compile({inline,{yeccpars2_575_,1}}).
-file("megaco_text_parser_prev3c.yrl", 676).
yeccpars2_575_([__1 | Stack]) ->
 [begin
   { mediaDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_576_,1}}).
-file("megaco_text_parser_prev3c.yrl", 679).
yeccpars2_576_([__1 | Stack]) ->
 [begin
   { eventsDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_577_,1}}).
-file("megaco_text_parser_prev3c.yrl", 680).
yeccpars2_577_([__1 | Stack]) ->
 [begin
   { eventBufferDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_578_,1}}).
-file("megaco_text_parser_prev3c.yrl", 682).
yeccpars2_578_([__1 | Stack]) ->
 [begin
   { digitMapDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_579_,1}}).
-file("megaco_text_parser_prev3c.yrl", 683).
yeccpars2_579_([__1 | Stack]) ->
 [begin
   { auditDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_580_,1}}).
-file("megaco_text_parser_prev3c.yrl", 673).
yeccpars2_580_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_581_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1410).
yeccpars2_581_([__1 | Stack]) ->
 [begin
   ensure_DMD ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_582_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1141).
yeccpars2_582_([__1 | Stack]) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_583_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1178).
yeccpars2_583_([__1 | Stack]) ->
 [begin
   # 'EventsDescriptor' { requestID = asn1_NOVALUE ,
    eventList = [ ] }
  end | Stack].

-compile({inline,{yeccpars2_587_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1274).
yeccpars2_587_([__1 | Stack]) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_590_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1490).
yeccpars2_590_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_591_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1494).
yeccpars2_591_([__1 | Stack]) ->
 [begin
   # 'StatisticsParameter' { statName = __1 ,
    statValue = asn1_NOVALUE }
  end | Stack].

-compile({inline,{yeccpars2_593_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1497).
yeccpars2_593_([__3,__2,__1 | Stack]) ->
 [begin
   # 'StatisticsParameter' { statName = __1 ,
    statValue = [ __3 ] }
  end | Stack].

-compile({inline,{yeccpars2_595_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1138).
yeccpars2_595_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_597_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1500).
yeccpars2_597_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'StatisticsParameter' { statName = __1 ,
    statValue = [ __4 | __5 ] }
  end | Stack].

-compile({inline,{yeccpars2_600_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1490).
yeccpars2_600_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_601_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1489).
yeccpars2_601_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_602_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1487).
yeccpars2_602_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __3 | __4 ]
  end | Stack].

-compile({inline,{yeccpars2_604_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1280).
yeccpars2_604_([__1 | Stack]) ->
 [begin
   { signal , __1 }
  end | Stack].

-compile({inline,{yeccpars2_605_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1277).
yeccpars2_605_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_606_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1279).
yeccpars2_606_([__1 | Stack]) ->
 [begin
   { seqSigList , __1 }
  end | Stack].

-compile({inline,{yeccpars2_607_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1629).
yeccpars2_607_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_611_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1352).
yeccpars2_611_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_614_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1352).
yeccpars2_614_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_615_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1351).
yeccpars2_615_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_616_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1347).
yeccpars2_616_([__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'SeqSigList' { id = ensure_uint16 ( __3 ) ,
    signalList = [ __5 | __6 ] }
  end | Stack].

-compile({inline,{yeccpars2_619_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1277).
yeccpars2_619_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_620_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1276).
yeccpars2_620_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_621_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1273).
yeccpars2_621_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __3 | __4 ]
  end | Stack].

-compile({inline,{yeccpars2_623_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1171).
yeccpars2_623_([__1 | Stack]) ->
 [begin
   ensure_muxType ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_625_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1168).
yeccpars2_625_([__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'MuxDescriptor' { muxType = __3 ,
    termList = __4 }
  end | Stack].

-compile({inline,{yeccpars2_627_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1031).
yeccpars2_627_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_629_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1027).
yeccpars2_629_([__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_632_,1}}).
-file("megaco_text_parser_prev3c.yrl", 0).
yeccpars2_632_([__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_633_,1}}).
-file("megaco_text_parser_prev3c.yrl", 0).
yeccpars2_633_(Stack) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_636_,1}}).
-file("megaco_text_parser_prev3c.yrl", 0).
yeccpars2_636_(Stack) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_637_,1}}).
-file("megaco_text_parser_prev3c.yrl", 0).
yeccpars2_637_([__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_638_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1400).
yeccpars2_638_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_639_,1}}).
-file("megaco_text_parser_prev3c.yrl", 0).
yeccpars2_639_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_641_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1404).
yeccpars2_641_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_643_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1399).
yeccpars2_643_([__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_644_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1400).
yeccpars2_644_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_645_,1}}).
-file("megaco_text_parser_prev3c.yrl", 0).
yeccpars2_645_([__4,__3,__2,__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_647_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1052).
yeccpars2_647_([__1 | Stack]) ->
 [begin
   { termState , __1 }
  end | Stack].

-compile({inline,{yeccpars2_648_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1048).
yeccpars2_648_([__1 | Stack]) ->
 [begin
   { streamParm , __1 }
  end | Stack].

-compile({inline,{yeccpars2_649_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1050).
yeccpars2_649_([__1 | Stack]) ->
 [begin
   { streamDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_650_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1061).
yeccpars2_650_([__1 | Stack]) ->
 [begin
   { statistics , __1 }
  end | Stack].

-compile({inline,{yeccpars2_651_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1042).
yeccpars2_651_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_652_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1060).
yeccpars2_652_([__1 | Stack]) ->
 [begin
   { control , __1 }
  end | Stack].

-compile({inline,{yeccpars2_654_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1057).
yeccpars2_654_([__1 | Stack]) ->
 [begin
   { local , # 'LocalRemoteDescriptor' { propGrps = ensure_prop_groups ( __1 ) } }
  end | Stack].

-compile({inline,{yeccpars2_655_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1059).
yeccpars2_655_([__1 | Stack]) ->
 [begin
   { remote , # 'LocalRemoteDescriptor' { propGrps = ensure_prop_groups ( __1 ) } }
  end | Stack].

-compile({inline,{yeccpars2_659_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1083).
yeccpars2_659_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_660_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1152).
yeccpars2_660_([__1 | Stack]) ->
 [begin
   { serviceState , __1 }
  end | Stack].

-compile({inline,{yeccpars2_661_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1154).
yeccpars2_661_([__1 | Stack]) ->
 [begin
   { propertyParm , __1 }
  end | Stack].

-compile({inline,{yeccpars2_662_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1153).
yeccpars2_662_([__1 | Stack]) ->
 [begin
   { eventBufferControl , __1 }
  end | Stack].

-compile({inline,{yeccpars2_666_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1156).
yeccpars2_666_([__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_668_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1162).
yeccpars2_668_([__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_669_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1165).
yeccpars2_669_([__1 | Stack]) ->
 [begin
   lockStep
  end | Stack].

-compile({inline,{yeccpars2_670_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1164).
yeccpars2_670_([__1 | Stack]) ->
 [begin
   off
  end | Stack].

-compile({inline,{yeccpars2_673_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1083).
yeccpars2_673_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_674_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1082).
yeccpars2_674_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_675_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1080).
yeccpars2_675_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_terminationStateDescriptor ( [ __3 | __4 ] )
  end | Stack].

-compile({inline,{yeccpars2_679_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1069).
yeccpars2_679_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_682_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1069).
yeccpars2_682_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_683_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1068).
yeccpars2_683_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_684_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1065).
yeccpars2_684_([__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'StreamDescriptor' { streamID = __3 ,
    streamParms = merge_streamParms ( [ __5 | __6 ] ) }
  end | Stack].

-compile({inline,{yeccpars2_686_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1089).
yeccpars2_686_([__1 | Stack]) ->
 [begin
   { prop , __1 }
  end | Stack].

-compile({inline,{yeccpars2_687_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1075).
yeccpars2_687_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_692_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1087).
yeccpars2_692_([__3,__2,__1 | Stack]) ->
 [begin
   { value , __3 }
  end | Stack].

-compile({inline,{yeccpars2_694_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1086).
yeccpars2_694_([__3,__2,__1 | Stack]) ->
 [begin
   { group , __3 }
  end | Stack].

-compile({inline,{yeccpars2_696_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1088).
yeccpars2_696_([__3,__2,__1 | Stack]) ->
 [begin
   { mode , __3 }
  end | Stack].

-compile({inline,{yeccpars2_699_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1075).
yeccpars2_699_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_700_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1074).
yeccpars2_700_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_701_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1072).
yeccpars2_701_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __3 | __4 ]
  end | Stack].

-compile({inline,{yeccpars2_704_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1042).
yeccpars2_704_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_705_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1041).
yeccpars2_705_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_706_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1039).
yeccpars2_706_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_mediaDescriptor ( [ __3 | __4 ] )
  end | Stack].

-compile({inline,{yeccpars2_710_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1186).
yeccpars2_710_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_711_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1193).
yeccpars2_711_(Stack) ->
 [begin
   # 'RequestedEvent' { evParList = [ ] }
  end | Stack].

-compile({inline,{yeccpars2_712_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1189).
yeccpars2_712_([__2,__1 | Stack]) ->
 [begin
   setelement ( # 'RequestedEvent' .pkgdName , __2 , __1 )
  end | Stack].

-compile({inline,{yeccpars2_714_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1205).
yeccpars2_714_([__1 | Stack]) ->
 [begin
   { notifyRegulated , __1 }
  end | Stack].

-compile({inline,{yeccpars2_715_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1217).
yeccpars2_715_([__1 | Stack]) ->
 [begin
   { notifyBehaviour , __1 }
  end | Stack].

-compile({inline,{yeccpars2_717_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1209).
yeccpars2_717_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_721_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1269).
yeccpars2_721_([__1 | Stack]) ->
 [begin
   ensure_eventDM ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_722_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1556).
yeccpars2_722_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_723_COMMA,1}}).
-file("megaco_text_parser_prev3c.yrl", 1212).
yeccpars2_723_COMMA([__1 | Stack]) ->
 [begin
   keepActive
  end | Stack].

-compile({inline,{yeccpars2_723_RBRKT,1}}).
-file("megaco_text_parser_prev3c.yrl", 1212).
yeccpars2_723_RBRKT([__1 | Stack]) ->
 [begin
   keepActive
  end | Stack].

-compile({inline,{yeccpars2_723_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1578).
yeccpars2_723_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_724_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1204).
yeccpars2_724_([__1 | Stack]) ->
 [begin
   { neverNotify , 'NULL' }
  end | Stack].

-compile({inline,{yeccpars2_725_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1203).
yeccpars2_725_([__1 | Stack]) ->
 [begin
   { notifyImmediate , 'NULL' }
  end | Stack].

-compile({inline,{yeccpars2_726_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1197).
yeccpars2_726_([__1 | Stack]) ->
 [begin
   # 'RegulatedEmbeddedDescriptor' { }
  end | Stack].

-compile({inline,{yeccpars2_727_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1218).
yeccpars2_727_([__1 | Stack]) ->
 [begin
   resetEventsDescriptor
  end | Stack].

-compile({inline,{yeccpars2_735_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1230).
yeccpars2_735_([__1 | Stack]) ->
 [begin
   # 'SecondEventsDescriptor' { requestID = asn1_NOVALUE ,
    eventList = [ ] }
  end | Stack].

-compile({inline,{yeccpars2_739_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1238).
yeccpars2_739_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_740_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1246).
yeccpars2_740_(Stack) ->
 [begin
   # 'SecondRequestedEvent' { evParList = [ ] }
  end | Stack].

-compile({inline,{yeccpars2_741_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1242).
yeccpars2_741_([__2,__1 | Stack]) ->
 [begin
   setelement ( # 'SecondRequestedEvent' .pkgdName , __2 , __1 )
  end | Stack].

-compile({inline,{yeccpars2_743_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1249).
yeccpars2_743_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_744_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1256).
yeccpars2_744_([__1 | Stack]) ->
 [begin
   { notifyBehaviour , __1 }
  end | Stack].

-compile({inline,{yeccpars2_748_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1556).
yeccpars2_748_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_749_COMMA,1}}).
-file("megaco_text_parser_prev3c.yrl", 1252).
yeccpars2_749_COMMA([__1 | Stack]) ->
 [begin
   keepActive
  end | Stack].

-compile({inline,{yeccpars2_749_RBRKT,1}}).
-file("megaco_text_parser_prev3c.yrl", 1252).
yeccpars2_749_RBRKT([__1 | Stack]) ->
 [begin
   keepActive
  end | Stack].

-compile({inline,{yeccpars2_749_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1578).
yeccpars2_749_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_750_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1257).
yeccpars2_750_([__1 | Stack]) ->
 [begin
   resetEventsDescriptor
  end | Stack].

-compile({inline,{yeccpars2_753_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1260).
yeccpars2_753_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { second_embed , __3 }
  end | Stack].

-compile({inline,{yeccpars2_756_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1249).
yeccpars2_756_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_757_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1248).
yeccpars2_757_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_758_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1245).
yeccpars2_758_([__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_secondEventParameters ( [ __2 | __3 ] )
  end | Stack].

-compile({inline,{yeccpars2_761_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1238).
yeccpars2_761_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_762_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1237).
yeccpars2_762_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_763_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1234).
yeccpars2_763_([__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'SecondEventsDescriptor' { requestID = __3 ,
    eventList = [ __5 | __6 ] }
  end | Stack].

-compile({inline,{yeccpars2_764_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1227).
yeccpars2_764_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { embed , asn1_NOVALUE , __3 }
  end | Stack].

-compile({inline,{yeccpars2_766_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1224).
yeccpars2_766_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { embed , __3 , asn1_NOVALUE }
  end | Stack].

-compile({inline,{yeccpars2_768_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1222).
yeccpars2_768_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { embed , __3 , __5 }
  end | Stack].

-compile({inline,{yeccpars2_769_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1201).
yeccpars2_769_([__4,__3,__2,__1 | Stack]) ->
 [begin
   make_RegulatedEmbeddedDescriptor ( __3 )
  end | Stack].

-compile({inline,{yeccpars2_770_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1199).
yeccpars2_770_([__4,__3,__2,__1 | Stack]) ->
 [begin
   make_RegulatedEmbeddedDescriptor ( __3 )
  end | Stack].

-compile({inline,{yeccpars2_773_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1209).
yeccpars2_773_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_774_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1208).
yeccpars2_774_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_775_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1192).
yeccpars2_775_([__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_eventParameters ( [ __2 | __3 ] )
  end | Stack].

-compile({inline,{yeccpars2_778_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1186).
yeccpars2_778_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_779_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1185).
yeccpars2_779_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_780_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1182).
yeccpars2_780_([__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'EventsDescriptor' { requestID = __3 ,
    eventList = [ __5 | __6 ] }
  end | Stack].

-compile({inline,{yeccpars2_781_,1}}).
-file("megaco_text_parser_prev3c.yrl", 481).
yeccpars2_781_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_782_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1149).
yeccpars2_782_([__1 | Stack]) ->
 [begin
   merge_eventSpec ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_783_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1147).
yeccpars2_783_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_785_,1}}).
-file("megaco_text_parser_prev3c.yrl", 481).
yeccpars2_785_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_786_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1147).
yeccpars2_786_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_787_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1146).
yeccpars2_787_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_788_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1144).
yeccpars2_788_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __3 | __4 ]
  end | Stack].

-compile({inline,{yeccpars2_791_,1}}).
-file("megaco_text_parser_prev3c.yrl", 673).
yeccpars2_791_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_792_,1}}).
-file("megaco_text_parser_prev3c.yrl", 672).
yeccpars2_792_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_793_,1}}).
-file("megaco_text_parser_prev3c.yrl", 669).
yeccpars2_793_([__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_796_,1}}).
-file("megaco_text_parser_prev3c.yrl", 529).
yeccpars2_796_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_797_,1}}).
-file("megaco_text_parser_prev3c.yrl", 528).
yeccpars2_797_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_798_,1}}).
-file("megaco_text_parser_prev3c.yrl", 517).
yeccpars2_798_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'TransactionRequest' { transactionId = asn1_NOVALUE ,
    actions = [ __3 | __4 ] }
  end | Stack].

-compile({inline,{yeccpars2_800_,1}}).
-file("megaco_text_parser_prev3c.yrl", 991).
yeccpars2_800_([__1 | Stack]) ->
 [begin
   ensure_uint32 ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_802_,1}}).
-file("megaco_text_parser_prev3c.yrl", 529).
yeccpars2_802_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_804_,1}}).
-file("megaco_text_parser_prev3c.yrl", 521).
yeccpars2_804_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'TransactionRequest' { transactionId = asn1_NOVALUE ,
    actions = [ __4 | __5 ] }
  end | Stack].

-compile({inline,{yeccpars2_806_,1}}).
-file("megaco_text_parser_prev3c.yrl", 529).
yeccpars2_806_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_808_,1}}).
-file("megaco_text_parser_prev3c.yrl", 525).
yeccpars2_808_([__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'TransactionRequest' { transactionId = ensure_transactionID ( __3 ) ,
    actions = [ __5 | __6 ] }
  end | Stack].

-compile({inline,{yeccpars2_810_,1}}).
-file("megaco_text_parser_prev3c.yrl", 508).
yeccpars2_810_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_811_,1}}).
-file("megaco_text_parser_prev3c.yrl", 510).
yeccpars2_811_([__1 | Stack]) ->
 [begin
   ensure_transactionAck ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_814_,1}}).
-file("megaco_text_parser_prev3c.yrl", 508).
yeccpars2_814_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_815_,1}}).
-file("megaco_text_parser_prev3c.yrl", 507).
yeccpars2_815_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_816_,1}}).
-file("megaco_text_parser_prev3c.yrl", 504).
yeccpars2_816_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __3 | __4 ]
  end | Stack].

-compile({inline,{yeccpars2_819_,1}}).
-file("megaco_text_parser_prev3c.yrl", 623).
yeccpars2_819_(Stack) ->
 [begin
   asn1_NOVALUE
  end | Stack].

-compile({inline,{yeccpars2_822_,1}}).
-file("megaco_text_parser_prev3c.yrl", 622).
yeccpars2_822_([__2,__1 | Stack]) ->
 [begin
   'NULL'
  end | Stack].

-compile({inline,{yeccpars2_824_,1}}).
-file("megaco_text_parser_prev3c.yrl", 625).
yeccpars2_824_([__1 | Stack]) ->
 [begin
   { transactionError , __1 }
  end | Stack].

-compile({inline,{yeccpars2_825_,1}}).
-file("megaco_text_parser_prev3c.yrl", 629).
yeccpars2_825_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_828_,1}}).
-file("megaco_text_parser_prev3c.yrl", 635).
yeccpars2_828_([__3,__2,__1 | Stack]) ->
 [begin
   # 'ActionReply' { contextId = __3 }
  end | Stack].

-compile({inline,{yeccpars2_830_,1}}).
-file("megaco_text_parser_prev3c.yrl", 652).
yeccpars2_830_([__1 | Stack]) ->
 [begin
   { command , __1 }
  end | Stack].

-compile({inline,{yeccpars2_831_,1}}).
-file("megaco_text_parser_prev3c.yrl", 655).
yeccpars2_831_([__1 | Stack]) ->
 [begin
   { command , __1 }
  end | Stack].

-compile({inline,{yeccpars2_832_,1}}).
-file("megaco_text_parser_prev3c.yrl", 638).
yeccpars2_832_([__1 | Stack]) ->
 [begin
   # 'ActionReply' { errorDescriptor = __1 }
  end | Stack].

-compile({inline,{yeccpars2_833_,1}}).
-file("megaco_text_parser_prev3c.yrl", 656).
yeccpars2_833_([__1 | Stack]) ->
 [begin
   { context , __1 }
  end | Stack].

-compile({inline,{yeccpars2_834_,1}}).
-file("megaco_text_parser_prev3c.yrl", 650).
yeccpars2_834_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_835_,1}}).
-file("megaco_text_parser_prev3c.yrl", 653).
yeccpars2_835_([__1 | Stack]) ->
 [begin
   { command , __1 }
  end | Stack].

-compile({inline,{yeccpars2_837_,1}}).
-file("megaco_text_parser_prev3c.yrl", 654).
yeccpars2_837_([__1 | Stack]) ->
 [begin
   { command , __1 }
  end | Stack].

-compile({inline,{yeccpars2_839_,1}}).
-file("megaco_text_parser_prev3c.yrl", 690).
yeccpars2_839_([__1 | Stack]) ->
 [begin
   addReply
  end | Stack].

-compile({inline,{yeccpars2_842_,1}}).
-file("megaco_text_parser_prev3c.yrl", 692).
yeccpars2_842_([__1 | Stack]) ->
 [begin
   modReply
  end | Stack].

-compile({inline,{yeccpars2_843_,1}}).
-file("megaco_text_parser_prev3c.yrl", 691).
yeccpars2_843_([__1 | Stack]) ->
 [begin
   moveReply
  end | Stack].

-compile({inline,{yeccpars2_846_,1}}).
-file("megaco_text_parser_prev3c.yrl", 693).
yeccpars2_846_([__1 | Stack]) ->
 [begin
   subtractReply
  end | Stack].

-compile({inline,{yeccpars2_848_,1}}).
-file("megaco_text_parser_prev3c.yrl", 979).
yeccpars2_848_(Stack) ->
 [begin
   { serviceChangeResParms , # 'ServiceChangeResParm' { } }
  end | Stack].

-compile({inline,{yeccpars2_849_,1}}).
-file("megaco_text_parser_prev3c.yrl", 970).
yeccpars2_849_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { serviceChangeReply ,
    # 'ServiceChangeReply' { terminationID = __3 ,
    serviceChangeResult = __4 } }
  end | Stack].

-compile({inline,{yeccpars2_855_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1471).
yeccpars2_855_([__1 | Stack]) ->
 [begin
   { time_stamp , __1 }
  end | Stack].

-compile({inline,{yeccpars2_856_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1470).
yeccpars2_856_([__1 | Stack]) ->
 [begin
   { version , __1 }
  end | Stack].

-compile({inline,{yeccpars2_857_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1469).
yeccpars2_857_([__1 | Stack]) ->
 [begin
   { profile , __1 }
  end | Stack].

-compile({inline,{yeccpars2_858_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1468).
yeccpars2_858_([__1 | Stack]) ->
 [begin
   { mgc_id , __1 }
  end | Stack].

-compile({inline,{yeccpars2_859_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1467).
yeccpars2_859_([__1 | Stack]) ->
 [begin
   { address , __1 }
  end | Stack].

-compile({inline,{yeccpars2_860_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1465).
yeccpars2_860_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_867_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1465).
yeccpars2_867_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_868_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1464).
yeccpars2_868_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_869_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1461).
yeccpars2_869_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_ServiceChangeResParm ( [ __3 | __4 ] )
  end | Stack].

-compile({inline,{yeccpars2_870_,1}}).
-file("megaco_text_parser_prev3c.yrl", 975).
yeccpars2_870_([__3,__2,__1 | Stack]) ->
 [begin
   { errorDescriptor , __2 }
  end | Stack].

-compile({inline,{yeccpars2_871_,1}}).
-file("megaco_text_parser_prev3c.yrl", 977).
yeccpars2_871_([__3,__2,__1 | Stack]) ->
 [begin
   { serviceChangeResParms , __2 }
  end | Stack].

-compile({inline,{yeccpars2_873_,1}}).
-file("megaco_text_parser_prev3c.yrl", 960).
yeccpars2_873_(Stack) ->
 [begin
   asn1_NOVALUE
  end | Stack].

-compile({inline,{yeccpars2_874_,1}}).
-file("megaco_text_parser_prev3c.yrl", 956).
yeccpars2_874_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { notifyReply , # 'NotifyReply' { terminationID = __3 ,
    errorDescriptor = __4 } }
  end | Stack].

-compile({inline,{yeccpars2_877_,1}}).
-file("megaco_text_parser_prev3c.yrl", 959).
yeccpars2_877_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_879_,1}}).
-file("megaco_text_parser_prev3c.yrl", 729).
yeccpars2_879_([__1 | Stack]) ->
 [begin
   merge_auditOther ( __1 , [ ] )
  end | Stack].

-compile({inline,{yeccpars2_880_,1}}).
-file("megaco_text_parser_prev3c.yrl", 719).
yeccpars2_880_([__3,__2,__1 | Stack]) ->
 [begin
   { auditValueReply , __3 }
  end | Stack].

-compile({inline,{yeccpars2_881_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1544).
yeccpars2_881_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_882_,1}}).
-file("megaco_text_parser_prev3c.yrl", 724).
yeccpars2_882_([__1 | Stack]) ->
 [begin
   { contextAuditResult , __1 }
  end | Stack].

-compile({inline,{yeccpars2_883_,1}}).
-file("megaco_text_parser_prev3c.yrl", 715).
yeccpars2_883_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { auditValueReply , __4 }
  end | Stack].

-compile({inline,{yeccpars2_886_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1559).
yeccpars2_886_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_888_,1}}).
-file("megaco_text_parser_prev3c.yrl", 986).
yeccpars2_888_([__1 | Stack]) ->
 [begin
   ensure_uint ( __1 , 0 , 999 )
  end | Stack].

-compile({inline,{yeccpars2_890_,1}}).
-file("megaco_text_parser_prev3c.yrl", 989).
yeccpars2_890_(Stack) ->
 [begin
   asn1_NOVALUE
  end | Stack].

-compile({inline,{yeccpars2_892_,1}}).
-file("megaco_text_parser_prev3c.yrl", 988).
yeccpars2_892_([__1 | Stack]) ->
 [begin
   value_of ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_893_,1}}).
-file("megaco_text_parser_prev3c.yrl", 983).
yeccpars2_893_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'ErrorDescriptor' { errorCode = __3 ,
    errorText = __5 }
  end | Stack].

-compile({inline,{yeccpars2_894_,1}}).
-file("megaco_text_parser_prev3c.yrl", 726).
yeccpars2_894_([__3,__2,__1 | Stack]) ->
 [begin
   { error , __2 }
  end | Stack].

-compile({inline,{yeccpars2_897_,1}}).
-file("megaco_text_parser_prev3c.yrl", 748).
yeccpars2_897_([__1 | Stack]) ->
 [begin
   { statisticsDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_898_,1}}).
-file("megaco_text_parser_prev3c.yrl", 744).
yeccpars2_898_([__1 | Stack]) ->
 [begin
   { signalsDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_899_,1}}).
-file("megaco_text_parser_prev3c.yrl", 749).
yeccpars2_899_([__1 | Stack]) ->
 [begin
   { packagesDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_900_,1}}).
-file("megaco_text_parser_prev3c.yrl", 746).
yeccpars2_900_([__1 | Stack]) ->
 [begin
   { observedEventsDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_901_,1}}).
-file("megaco_text_parser_prev3c.yrl", 742).
yeccpars2_901_([__1 | Stack]) ->
 [begin
   { muxDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_902_,1}}).
-file("megaco_text_parser_prev3c.yrl", 0).
yeccpars2_902_([__1 | Stack]) ->
 [begin
   '$undefined'
  end | Stack].

-compile({inline,{yeccpars2_903_,1}}).
-file("megaco_text_parser_prev3c.yrl", 740).
yeccpars2_903_([__1 | Stack]) ->
 [begin
   { mediaDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_904_,1}}).
-file("megaco_text_parser_prev3c.yrl", 743).
yeccpars2_904_([__1 | Stack]) ->
 [begin
   { eventsDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_905_,1}}).
-file("megaco_text_parser_prev3c.yrl", 747).
yeccpars2_905_([__1 | Stack]) ->
 [begin
   { eventBufferDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_906_,1}}).
-file("megaco_text_parser_prev3c.yrl", 750).
yeccpars2_906_([__1 | Stack]) ->
 [begin
   { errorDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_907_,1}}).
-file("megaco_text_parser_prev3c.yrl", 745).
yeccpars2_907_([__1 | Stack]) ->
 [begin
   { digitMapDescriptor , __1 }
  end | Stack].

-compile({inline,{yeccpars2_908_,1}}).
-file("megaco_text_parser_prev3c.yrl", 738).
yeccpars2_908_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_909_,1}}).
-file("megaco_text_parser_prev3c.yrl", 751).
yeccpars2_909_([__1 | Stack]) ->
 [begin
   { auditReturnItem , __1 }
  end | Stack].

-compile({inline,{yeccpars2_910_,1}}).
-file("megaco_text_parser_prev3c.yrl", 766).
yeccpars2_910_([__1 | Stack]) ->
 [begin
   mediaToken
  end | Stack].

-compile({inline,{yeccpars2_911_,1}}).
-file("megaco_text_parser_prev3c.yrl", 765).
yeccpars2_911_([__1 | Stack]) ->
 [begin
   modemToken
  end | Stack].

-compile({inline,{yeccpars2_912_,1}}).
-file("megaco_text_parser_prev3c.yrl", 764).
yeccpars2_912_([__1 | Stack]) ->
 [begin
   muxToken
  end | Stack].

-compile({inline,{yeccpars2_913_,1}}).
-file("megaco_text_parser_prev3c.yrl", 769).
yeccpars2_913_([__1 | Stack]) ->
 [begin
   observedEventsToken
  end | Stack].

-compile({inline,{yeccpars2_914_,1}}).
-file("megaco_text_parser_prev3c.yrl", 770).
yeccpars2_914_([__1 | Stack]) ->
 [begin
   packagesToken
  end | Stack].

-compile({inline,{yeccpars2_915_,1}}).
-file("megaco_text_parser_prev3c.yrl", 768).
yeccpars2_915_([__1 | Stack]) ->
 [begin
   statsToken
  end | Stack].

-compile({inline,{yeccpars2_917_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1478).
yeccpars2_917_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_920_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1478).
yeccpars2_920_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_921_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1477).
yeccpars2_921_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_922_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1475).
yeccpars2_922_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   [ __3 | __4 ]
  end | Stack].

-compile({inline,{yeccpars2_923_,1}}).
-file("megaco_text_parser_prev3c.yrl", 735).
yeccpars2_923_([__2,__1 | Stack]) ->
 [begin
   merge_terminationAudit ( [ __1 | __2 ] )
  end | Stack].

-compile({inline,{yeccpars2_925_,1}}).
-file("megaco_text_parser_prev3c.yrl", 738).
yeccpars2_925_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_926_,1}}).
-file("megaco_text_parser_prev3c.yrl", 737).
yeccpars2_926_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_927_,1}}).
-file("megaco_text_parser_prev3c.yrl", 731).
yeccpars2_927_([__4,__3,__2,__1 | Stack]) ->
 [begin
   merge_auditOther ( __1 , __3 )
  end | Stack].

-compile({inline,{yeccpars2_929_,1}}).
-file("megaco_text_parser_prev3c.yrl", 721).
yeccpars2_929_([__3,__2,__1 | Stack]) ->
 [begin
   { auditCapReply , __3 }
  end | Stack].

-compile({inline,{yeccpars2_930_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1544).
yeccpars2_930_([__1 | Stack]) ->
 [begin
   make_safe_token ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_931_,1}}).
-file("megaco_text_parser_prev3c.yrl", 717).
yeccpars2_931_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { auditCapReply , __4 }
  end | Stack].

-compile({inline,{yeccpars2_932_,1}}).
-file("megaco_text_parser_prev3c.yrl", 633).
yeccpars2_932_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   setelement ( # 'ActionReply' .contextId , __5 , __3 )
  end | Stack].

-compile({inline,{yeccpars2_934_,1}}).
-file("megaco_text_parser_prev3c.yrl", 696).
yeccpars2_934_(Stack) ->
 [begin
   asn1_NOVALUE
  end | Stack].

-compile({inline,{yeccpars2_935_,1}}).
-file("megaco_text_parser_prev3c.yrl", 687).
yeccpars2_935_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { __1 , # 'AmmsReply' { terminationID = __3 ,
    terminationAudit = __4 } }
  end | Stack].

-compile({inline,{yeccpars2_938_,1}}).
-file("megaco_text_parser_prev3c.yrl", 695).
yeccpars2_938_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_939_,1}}).
-file("megaco_text_parser_prev3c.yrl", 640).
yeccpars2_939_([__2,__1 | Stack]) ->
 [begin
   merge_action_reply ( [ __1 | __2 ] )
  end | Stack].

-compile({inline,{yeccpars2_941_,1}}).
-file("megaco_text_parser_prev3c.yrl", 647).
yeccpars2_941_([__2,__1 | Stack]) ->
 [begin
   [ { error , __2 } ]
  end | Stack].

-compile({inline,{yeccpars2_942_,1}}).
-file("megaco_text_parser_prev3c.yrl", 650).
yeccpars2_942_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_943_,1}}).
-file("megaco_text_parser_prev3c.yrl", 649).
yeccpars2_943_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_944_,1}}).
-file("megaco_text_parser_prev3c.yrl", 626).
yeccpars2_944_([__2,__1 | Stack]) ->
 [begin
   { actionReplies , [ __1 | __2 ] }
  end | Stack].

-compile({inline,{yeccpars2_946_,1}}).
-file("megaco_text_parser_prev3c.yrl", 629).
yeccpars2_946_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_947_,1}}).
-file("megaco_text_parser_prev3c.yrl", 628).
yeccpars2_947_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_948_,1}}).
-file("megaco_text_parser_prev3c.yrl", 618).
yeccpars2_948_([__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'TransactionReply' { transactionId = __3 ,
    immAckRequired = __5 ,
    transactionResult = __6 }
  end | Stack].

-compile({inline,{yeccpars2_952_,1}}).
-file("megaco_text_parser_prev3c.yrl", 513).
yeccpars2_952_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # 'TransactionPending' { transactionId = ensure_transactionID ( __3 ) }
  end | Stack].

-compile({inline,{yeccpars2_953_,1}}).
-file("megaco_text_parser_prev3c.yrl", 495).
yeccpars2_953_([__2,__1 | Stack]) ->
 [begin
   [ __1 | __2 ]
  end | Stack].

-compile({inline,{yeccpars2_954_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1034).
yeccpars2_954_([__1 | Stack]) ->
 [begin
   ensure_pathName ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_955_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1003).
yeccpars2_955_([__1 | Stack]) ->
 [begin
   { deviceName , __1 }
  end | Stack].

-compile({inline,{yeccpars2_956_,1}}).
-file("megaco_text_parser_prev3c.yrl", 481).
yeccpars2_956_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_957_,1}}).
-file("megaco_text_parser_prev3c.yrl", 481).
yeccpars2_957_(Stack) ->
 [begin
   no_sep
  end | Stack].

-compile({inline,{yeccpars2_958_,1}}).
-file("megaco_text_parser_prev3c.yrl", 1020).
yeccpars2_958_([__1 | Stack]) ->
 [begin
   ensure_mtpAddress ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_959_,1}}).
-file("megaco_text_parser_prev3c.yrl", 996).
yeccpars2_959_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_960_,1}}).
-file("megaco_text_parser_prev3c.yrl", 995).
yeccpars2_960_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].


-file("megaco_text_parser_prev3c.yrl", 1664).
