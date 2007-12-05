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



-file("./xmerl_xpath_parse.erl", 159).

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
 yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.2",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '..', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, axis, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, function_name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, node_type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, number, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, prefix_test, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'processing-instruction', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, var_reference, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, wildcard, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr).

yeccpars2_1(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'UnaryExpr\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'MultiplicativeExpr\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'RelativeLocationPath\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_4(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_4_(Stack),
 yeccpars2('yeccgoto_\'LocationPath\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_5(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'EqualityExpr\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'FilterExpr\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'UnionExpr\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_8(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'Expr\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_9(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_9_(Stack),
 yeccpars2('yeccgoto_\'Step\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'NodeTest\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_11(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, 'div', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, mod, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'AdditiveExpr\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'PathExpr\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'PrimaryExpr\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_14(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'PathExpr\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_15(_S, '$end', _Ss, Stack,  _T, _Ts, _Tzr) ->
 {ok, hd(Stack)}.

yeccpars2_16(S, '!=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'AndExpr\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_17(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'OrExpr\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_18(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'RelationalExpr\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_19_(Stack),
 yeccpars2('yeccgoto_\'LocationPath\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_20_(Stack),
 yeccpars2('yeccgoto_\'Step\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'RelativeLocationPath\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'RelativeLocationPath\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_23: see yeccpars2_0

%% yeccpars2_24: see yeccpars2_0

yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'AbbreviatedStep\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'AbbreviatedStep\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_27(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(S, '..', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(S, axis, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(S, node_type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(S, prefix_test, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(S, 'processing-instruction', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(S, wildcard, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_27_(Stack),
 yeccpars2('yeccgoto_\'AbsoluteLocationPath\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_28(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, '..', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, axis, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, node_type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, prefix_test, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, 'processing-instruction', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, wildcard, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr).

yeccpars2_29(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr).

yeccpars2_30(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr).

yeccpars2_31(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr).

yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_32_(Stack),
 yeccpars2('yeccgoto_\'PrimaryExpr\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_33_(Stack),
 yeccpars2('yeccgoto_\'NameTest\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_34(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr).

yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_35_(Stack),
 yeccpars2('yeccgoto_\'PrimaryExpr\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_36_(Stack),
 yeccpars2('yeccgoto_\'NameTest\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_37(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr).

yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_38_(Stack),
 yeccpars2('yeccgoto_\'PrimaryExpr\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_39_(Stack),
 yeccpars2('yeccgoto_\'NameTest\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_40(S, literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr).

yeccpars2_41(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr).

yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_42_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'NodeTest\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_43(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr).

yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_44_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'NodeTest\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_45(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '..', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, axis, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, function_name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, node_type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, number, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, prefix_test, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, 'processing-instruction', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, var_reference, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_45(S, wildcard, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr).

yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'Argument\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_47_(Stack),
 yeccpars2('yeccgoto_\'<ArgumentMember>\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_48(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_48_(Stack),
 yeccpars2('yeccgoto_\'<ArgumentList>\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_49(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr).

yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_50_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'FunctionCall\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_51_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'FunctionCall\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_52: see yeccpars2_0

yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_53_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'<ArgumentMember>\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_54(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, node_type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, prefix_test, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, 'processing-instruction', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, wildcard, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr).

yeccpars2_55(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_55_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'Step\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_56_(Stack),
 yeccpars2('yeccgoto_\'<PredicateMember>\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_57(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_57_(Stack),
 yeccpars2('yeccgoto_\'<PredicateList>\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_58_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2('yeccgoto_\'Step\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_59: see yeccpars2_0

yeccpars2_60(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr).

yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2('yeccgoto_\'PredicateExpr\''(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_62_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'Predicate\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_63_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'<PredicateMember>\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_64(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_64_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Step\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_65_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'Step\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_66(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_66_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'AbbreviatedAbsoluteLocationPath\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_67(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, '..', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, axis, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, node_type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, prefix_test, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, 'processing-instruction', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, wildcard, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_68: see yeccpars2_67

yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_69_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'AbbreviatedRelativeLocationPath\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_70_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'RelativeLocationPath\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_71(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_71_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'AbsoluteLocationPath\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_72_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'UnaryExpr\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_73(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr).

yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_74_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'PrimaryExpr\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_75: see yeccpars2_0

%% yeccpars2_76: see yeccpars2_0

yeccpars2_77(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(S, 'div', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(S, mod, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_77_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'AdditiveExpr\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_78: see yeccpars2_0

yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_79_(Stack),
 yeccpars2('yeccgoto_\'MultiplyOperator\''(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_80: see yeccpars2_0

%% yeccpars2_81: see yeccpars2_0

yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_82_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'MultiplicativeExpr\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_83_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'MultiplicativeExpr\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_84_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'MultiplicativeExpr\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_85(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(S, 'div', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(S, mod, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_85_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'AdditiveExpr\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_86: see yeccpars2_0

yeccpars2_87(S, '!=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_87_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'AndExpr\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_88: see yeccpars2_0

%% yeccpars2_89: see yeccpars2_0

yeccpars2_90(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_90_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'EqualityExpr\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_91: see yeccpars2_0

%% yeccpars2_92: see yeccpars2_0

%% yeccpars2_93: see yeccpars2_0

%% yeccpars2_94: see yeccpars2_0

yeccpars2_95(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_95_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'RelationalExpr\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_96(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_96_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'RelationalExpr\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_97(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_97_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'RelationalExpr\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_98(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_98(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'RelationalExpr\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_99(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_99_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'EqualityExpr\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_100_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'FilterExpr\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_101: see yeccpars2_28

%% yeccpars2_102: see yeccpars2_28

yeccpars2_103(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'PathExpr\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_104(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_104_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'PathExpr\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_105_(Stack),
 Nss = tl(Ss),
 yeccpars2('yeccgoto_\'Step\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_106: see yeccpars2_0

yeccpars2_107(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'OrExpr\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_108(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, '..', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, '//', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, '@', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, axis, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, function_name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, literal, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, name, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, node_type, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, number, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, prefix_test, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, 'processing-instruction', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, var_reference, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, wildcard, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr).

yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_109_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2('yeccgoto_\'UnionExpr\''(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

'yeccgoto_\'<ArgumentList>\''(45) -> 49.

'yeccgoto_\'<ArgumentMember>\''(45) -> 48.

'yeccgoto_\'<PredicateList>\''(9) -> 105;
'yeccgoto_\'<PredicateList>\''(55) -> 58;
'yeccgoto_\'<PredicateList>\''(64) -> 65.

'yeccgoto_\'<PredicateMember>\''(9) -> 57;
'yeccgoto_\'<PredicateMember>\''(55) -> 57;
'yeccgoto_\'<PredicateMember>\''(64) -> 57.

'yeccgoto_\'AbbreviatedAbsoluteLocationPath\''(0) -> 22;
'yeccgoto_\'AbbreviatedAbsoluteLocationPath\''(23) -> 22;
'yeccgoto_\'AbbreviatedAbsoluteLocationPath\''(24) -> 22;
'yeccgoto_\'AbbreviatedAbsoluteLocationPath\''(27) -> 22;
'yeccgoto_\'AbbreviatedAbsoluteLocationPath\''(28) -> 22;
'yeccgoto_\'AbbreviatedAbsoluteLocationPath\''(45) -> 22;
'yeccgoto_\'AbbreviatedAbsoluteLocationPath\''(52) -> 22;
'yeccgoto_\'AbbreviatedAbsoluteLocationPath\''(59) -> 22;
'yeccgoto_\'AbbreviatedAbsoluteLocationPath\''(75) -> 22;
'yeccgoto_\'AbbreviatedAbsoluteLocationPath\''(76) -> 22;
'yeccgoto_\'AbbreviatedAbsoluteLocationPath\''(78) -> 22;
'yeccgoto_\'AbbreviatedAbsoluteLocationPath\''(80) -> 22;
'yeccgoto_\'AbbreviatedAbsoluteLocationPath\''(81) -> 22;
'yeccgoto_\'AbbreviatedAbsoluteLocationPath\''(86) -> 22;
'yeccgoto_\'AbbreviatedAbsoluteLocationPath\''(88) -> 22;
'yeccgoto_\'AbbreviatedAbsoluteLocationPath\''(89) -> 22;
'yeccgoto_\'AbbreviatedAbsoluteLocationPath\''(91) -> 22;
'yeccgoto_\'AbbreviatedAbsoluteLocationPath\''(92) -> 22;
'yeccgoto_\'AbbreviatedAbsoluteLocationPath\''(93) -> 22;
'yeccgoto_\'AbbreviatedAbsoluteLocationPath\''(94) -> 22;
'yeccgoto_\'AbbreviatedAbsoluteLocationPath\''(101) -> 22;
'yeccgoto_\'AbbreviatedAbsoluteLocationPath\''(102) -> 22;
'yeccgoto_\'AbbreviatedAbsoluteLocationPath\''(106) -> 22;
'yeccgoto_\'AbbreviatedAbsoluteLocationPath\''(108) -> 22.

'yeccgoto_\'AbbreviatedRelativeLocationPath\''(0) -> 21;
'yeccgoto_\'AbbreviatedRelativeLocationPath\''(23) -> 21;
'yeccgoto_\'AbbreviatedRelativeLocationPath\''(24) -> 21;
'yeccgoto_\'AbbreviatedRelativeLocationPath\''(27) -> 21;
'yeccgoto_\'AbbreviatedRelativeLocationPath\''(28) -> 21;
'yeccgoto_\'AbbreviatedRelativeLocationPath\''(45) -> 21;
'yeccgoto_\'AbbreviatedRelativeLocationPath\''(52) -> 21;
'yeccgoto_\'AbbreviatedRelativeLocationPath\''(59) -> 21;
'yeccgoto_\'AbbreviatedRelativeLocationPath\''(75) -> 21;
'yeccgoto_\'AbbreviatedRelativeLocationPath\''(76) -> 21;
'yeccgoto_\'AbbreviatedRelativeLocationPath\''(78) -> 21;
'yeccgoto_\'AbbreviatedRelativeLocationPath\''(80) -> 21;
'yeccgoto_\'AbbreviatedRelativeLocationPath\''(81) -> 21;
'yeccgoto_\'AbbreviatedRelativeLocationPath\''(86) -> 21;
'yeccgoto_\'AbbreviatedRelativeLocationPath\''(88) -> 21;
'yeccgoto_\'AbbreviatedRelativeLocationPath\''(89) -> 21;
'yeccgoto_\'AbbreviatedRelativeLocationPath\''(91) -> 21;
'yeccgoto_\'AbbreviatedRelativeLocationPath\''(92) -> 21;
'yeccgoto_\'AbbreviatedRelativeLocationPath\''(93) -> 21;
'yeccgoto_\'AbbreviatedRelativeLocationPath\''(94) -> 21;
'yeccgoto_\'AbbreviatedRelativeLocationPath\''(101) -> 21;
'yeccgoto_\'AbbreviatedRelativeLocationPath\''(102) -> 21;
'yeccgoto_\'AbbreviatedRelativeLocationPath\''(106) -> 21;
'yeccgoto_\'AbbreviatedRelativeLocationPath\''(108) -> 21.

'yeccgoto_\'AbbreviatedStep\''(0) -> 20;
'yeccgoto_\'AbbreviatedStep\''(23) -> 20;
'yeccgoto_\'AbbreviatedStep\''(24) -> 20;
'yeccgoto_\'AbbreviatedStep\''(27) -> 20;
'yeccgoto_\'AbbreviatedStep\''(28) -> 20;
'yeccgoto_\'AbbreviatedStep\''(45) -> 20;
'yeccgoto_\'AbbreviatedStep\''(52) -> 20;
'yeccgoto_\'AbbreviatedStep\''(59) -> 20;
'yeccgoto_\'AbbreviatedStep\''(67) -> 20;
'yeccgoto_\'AbbreviatedStep\''(68) -> 20;
'yeccgoto_\'AbbreviatedStep\''(75) -> 20;
'yeccgoto_\'AbbreviatedStep\''(76) -> 20;
'yeccgoto_\'AbbreviatedStep\''(78) -> 20;
'yeccgoto_\'AbbreviatedStep\''(80) -> 20;
'yeccgoto_\'AbbreviatedStep\''(81) -> 20;
'yeccgoto_\'AbbreviatedStep\''(86) -> 20;
'yeccgoto_\'AbbreviatedStep\''(88) -> 20;
'yeccgoto_\'AbbreviatedStep\''(89) -> 20;
'yeccgoto_\'AbbreviatedStep\''(91) -> 20;
'yeccgoto_\'AbbreviatedStep\''(92) -> 20;
'yeccgoto_\'AbbreviatedStep\''(93) -> 20;
'yeccgoto_\'AbbreviatedStep\''(94) -> 20;
'yeccgoto_\'AbbreviatedStep\''(101) -> 20;
'yeccgoto_\'AbbreviatedStep\''(102) -> 20;
'yeccgoto_\'AbbreviatedStep\''(106) -> 20;
'yeccgoto_\'AbbreviatedStep\''(108) -> 20.

'yeccgoto_\'AbsoluteLocationPath\''(0) -> 19;
'yeccgoto_\'AbsoluteLocationPath\''(23) -> 19;
'yeccgoto_\'AbsoluteLocationPath\''(24) -> 19;
'yeccgoto_\'AbsoluteLocationPath\''(45) -> 19;
'yeccgoto_\'AbsoluteLocationPath\''(52) -> 19;
'yeccgoto_\'AbsoluteLocationPath\''(59) -> 19;
'yeccgoto_\'AbsoluteLocationPath\''(75) -> 19;
'yeccgoto_\'AbsoluteLocationPath\''(76) -> 19;
'yeccgoto_\'AbsoluteLocationPath\''(78) -> 19;
'yeccgoto_\'AbsoluteLocationPath\''(80) -> 19;
'yeccgoto_\'AbsoluteLocationPath\''(81) -> 19;
'yeccgoto_\'AbsoluteLocationPath\''(86) -> 19;
'yeccgoto_\'AbsoluteLocationPath\''(88) -> 19;
'yeccgoto_\'AbsoluteLocationPath\''(89) -> 19;
'yeccgoto_\'AbsoluteLocationPath\''(91) -> 19;
'yeccgoto_\'AbsoluteLocationPath\''(92) -> 19;
'yeccgoto_\'AbsoluteLocationPath\''(93) -> 19;
'yeccgoto_\'AbsoluteLocationPath\''(94) -> 19;
'yeccgoto_\'AbsoluteLocationPath\''(106) -> 19;
'yeccgoto_\'AbsoluteLocationPath\''(108) -> 19.

'yeccgoto_\'AdditiveExpr\''(0) -> 18;
'yeccgoto_\'AdditiveExpr\''(23) -> 18;
'yeccgoto_\'AdditiveExpr\''(45) -> 18;
'yeccgoto_\'AdditiveExpr\''(52) -> 18;
'yeccgoto_\'AdditiveExpr\''(59) -> 18;
'yeccgoto_\'AdditiveExpr\''(86) -> 18;
'yeccgoto_\'AdditiveExpr\''(88) -> 18;
'yeccgoto_\'AdditiveExpr\''(89) -> 18;
'yeccgoto_\'AdditiveExpr\''(91) -> 98;
'yeccgoto_\'AdditiveExpr\''(92) -> 97;
'yeccgoto_\'AdditiveExpr\''(93) -> 96;
'yeccgoto_\'AdditiveExpr\''(94) -> 95;
'yeccgoto_\'AdditiveExpr\''(106) -> 18.

'yeccgoto_\'AndExpr\''(0) -> 17;
'yeccgoto_\'AndExpr\''(23) -> 17;
'yeccgoto_\'AndExpr\''(45) -> 17;
'yeccgoto_\'AndExpr\''(52) -> 17;
'yeccgoto_\'AndExpr\''(59) -> 17;
'yeccgoto_\'AndExpr\''(106) -> 107.

'yeccgoto_\'Argument\''(45) -> 47;
'yeccgoto_\'Argument\''(52) -> 53.

'yeccgoto_\'EqualityExpr\''(0) -> 16;
'yeccgoto_\'EqualityExpr\''(23) -> 16;
'yeccgoto_\'EqualityExpr\''(45) -> 16;
'yeccgoto_\'EqualityExpr\''(52) -> 16;
'yeccgoto_\'EqualityExpr\''(59) -> 16;
'yeccgoto_\'EqualityExpr\''(86) -> 87;
'yeccgoto_\'EqualityExpr\''(106) -> 16.

'yeccgoto_\'Expr\''(0) -> 15;
'yeccgoto_\'Expr\''(23) -> 73;
'yeccgoto_\'Expr\''(45) -> 46;
'yeccgoto_\'Expr\''(52) -> 46;
'yeccgoto_\'Expr\''(59) -> 61.

'yeccgoto_\'FilterExpr\''(0) -> 14;
'yeccgoto_\'FilterExpr\''(23) -> 14;
'yeccgoto_\'FilterExpr\''(24) -> 14;
'yeccgoto_\'FilterExpr\''(45) -> 14;
'yeccgoto_\'FilterExpr\''(52) -> 14;
'yeccgoto_\'FilterExpr\''(59) -> 14;
'yeccgoto_\'FilterExpr\''(75) -> 14;
'yeccgoto_\'FilterExpr\''(76) -> 14;
'yeccgoto_\'FilterExpr\''(78) -> 14;
'yeccgoto_\'FilterExpr\''(80) -> 14;
'yeccgoto_\'FilterExpr\''(81) -> 14;
'yeccgoto_\'FilterExpr\''(86) -> 14;
'yeccgoto_\'FilterExpr\''(88) -> 14;
'yeccgoto_\'FilterExpr\''(89) -> 14;
'yeccgoto_\'FilterExpr\''(91) -> 14;
'yeccgoto_\'FilterExpr\''(92) -> 14;
'yeccgoto_\'FilterExpr\''(93) -> 14;
'yeccgoto_\'FilterExpr\''(94) -> 14;
'yeccgoto_\'FilterExpr\''(106) -> 14;
'yeccgoto_\'FilterExpr\''(108) -> 14.

'yeccgoto_\'FunctionCall\''(0) -> 13;
'yeccgoto_\'FunctionCall\''(23) -> 13;
'yeccgoto_\'FunctionCall\''(24) -> 13;
'yeccgoto_\'FunctionCall\''(45) -> 13;
'yeccgoto_\'FunctionCall\''(52) -> 13;
'yeccgoto_\'FunctionCall\''(59) -> 13;
'yeccgoto_\'FunctionCall\''(75) -> 13;
'yeccgoto_\'FunctionCall\''(76) -> 13;
'yeccgoto_\'FunctionCall\''(78) -> 13;
'yeccgoto_\'FunctionCall\''(80) -> 13;
'yeccgoto_\'FunctionCall\''(81) -> 13;
'yeccgoto_\'FunctionCall\''(86) -> 13;
'yeccgoto_\'FunctionCall\''(88) -> 13;
'yeccgoto_\'FunctionCall\''(89) -> 13;
'yeccgoto_\'FunctionCall\''(91) -> 13;
'yeccgoto_\'FunctionCall\''(92) -> 13;
'yeccgoto_\'FunctionCall\''(93) -> 13;
'yeccgoto_\'FunctionCall\''(94) -> 13;
'yeccgoto_\'FunctionCall\''(106) -> 13;
'yeccgoto_\'FunctionCall\''(108) -> 13.

'yeccgoto_\'LocationPath\''(0) -> 12;
'yeccgoto_\'LocationPath\''(23) -> 12;
'yeccgoto_\'LocationPath\''(24) -> 12;
'yeccgoto_\'LocationPath\''(45) -> 12;
'yeccgoto_\'LocationPath\''(52) -> 12;
'yeccgoto_\'LocationPath\''(59) -> 12;
'yeccgoto_\'LocationPath\''(75) -> 12;
'yeccgoto_\'LocationPath\''(76) -> 12;
'yeccgoto_\'LocationPath\''(78) -> 12;
'yeccgoto_\'LocationPath\''(80) -> 12;
'yeccgoto_\'LocationPath\''(81) -> 12;
'yeccgoto_\'LocationPath\''(86) -> 12;
'yeccgoto_\'LocationPath\''(88) -> 12;
'yeccgoto_\'LocationPath\''(89) -> 12;
'yeccgoto_\'LocationPath\''(91) -> 12;
'yeccgoto_\'LocationPath\''(92) -> 12;
'yeccgoto_\'LocationPath\''(93) -> 12;
'yeccgoto_\'LocationPath\''(94) -> 12;
'yeccgoto_\'LocationPath\''(106) -> 12;
'yeccgoto_\'LocationPath\''(108) -> 12.

'yeccgoto_\'MultiplicativeExpr\''(0) -> 11;
'yeccgoto_\'MultiplicativeExpr\''(23) -> 11;
'yeccgoto_\'MultiplicativeExpr\''(45) -> 11;
'yeccgoto_\'MultiplicativeExpr\''(52) -> 11;
'yeccgoto_\'MultiplicativeExpr\''(59) -> 11;
'yeccgoto_\'MultiplicativeExpr\''(75) -> 85;
'yeccgoto_\'MultiplicativeExpr\''(76) -> 77;
'yeccgoto_\'MultiplicativeExpr\''(86) -> 11;
'yeccgoto_\'MultiplicativeExpr\''(88) -> 11;
'yeccgoto_\'MultiplicativeExpr\''(89) -> 11;
'yeccgoto_\'MultiplicativeExpr\''(91) -> 11;
'yeccgoto_\'MultiplicativeExpr\''(92) -> 11;
'yeccgoto_\'MultiplicativeExpr\''(93) -> 11;
'yeccgoto_\'MultiplicativeExpr\''(94) -> 11;
'yeccgoto_\'MultiplicativeExpr\''(106) -> 11.

'yeccgoto_\'MultiplyOperator\''(11) -> 78;
'yeccgoto_\'MultiplyOperator\''(77) -> 78;
'yeccgoto_\'MultiplyOperator\''(85) -> 78.

'yeccgoto_\'NameTest\''(0) -> 10;
'yeccgoto_\'NameTest\''(23) -> 10;
'yeccgoto_\'NameTest\''(24) -> 10;
'yeccgoto_\'NameTest\''(27) -> 10;
'yeccgoto_\'NameTest\''(28) -> 10;
'yeccgoto_\'NameTest\''(45) -> 10;
'yeccgoto_\'NameTest\''(52) -> 10;
'yeccgoto_\'NameTest\''(54) -> 10;
'yeccgoto_\'NameTest\''(59) -> 10;
'yeccgoto_\'NameTest\''(67) -> 10;
'yeccgoto_\'NameTest\''(68) -> 10;
'yeccgoto_\'NameTest\''(75) -> 10;
'yeccgoto_\'NameTest\''(76) -> 10;
'yeccgoto_\'NameTest\''(78) -> 10;
'yeccgoto_\'NameTest\''(80) -> 10;
'yeccgoto_\'NameTest\''(81) -> 10;
'yeccgoto_\'NameTest\''(86) -> 10;
'yeccgoto_\'NameTest\''(88) -> 10;
'yeccgoto_\'NameTest\''(89) -> 10;
'yeccgoto_\'NameTest\''(91) -> 10;
'yeccgoto_\'NameTest\''(92) -> 10;
'yeccgoto_\'NameTest\''(93) -> 10;
'yeccgoto_\'NameTest\''(94) -> 10;
'yeccgoto_\'NameTest\''(101) -> 10;
'yeccgoto_\'NameTest\''(102) -> 10;
'yeccgoto_\'NameTest\''(106) -> 10;
'yeccgoto_\'NameTest\''(108) -> 10.

'yeccgoto_\'NodeTest\''(0) -> 9;
'yeccgoto_\'NodeTest\''(23) -> 9;
'yeccgoto_\'NodeTest\''(24) -> 9;
'yeccgoto_\'NodeTest\''(27) -> 9;
'yeccgoto_\'NodeTest\''(28) -> 9;
'yeccgoto_\'NodeTest\''(45) -> 9;
'yeccgoto_\'NodeTest\''(52) -> 9;
'yeccgoto_\'NodeTest\''(54) -> 55;
'yeccgoto_\'NodeTest\''(59) -> 9;
'yeccgoto_\'NodeTest\''(67) -> 9;
'yeccgoto_\'NodeTest\''(68) -> 9;
'yeccgoto_\'NodeTest\''(75) -> 9;
'yeccgoto_\'NodeTest\''(76) -> 9;
'yeccgoto_\'NodeTest\''(78) -> 9;
'yeccgoto_\'NodeTest\''(80) -> 9;
'yeccgoto_\'NodeTest\''(81) -> 9;
'yeccgoto_\'NodeTest\''(86) -> 9;
'yeccgoto_\'NodeTest\''(88) -> 9;
'yeccgoto_\'NodeTest\''(89) -> 9;
'yeccgoto_\'NodeTest\''(91) -> 9;
'yeccgoto_\'NodeTest\''(92) -> 9;
'yeccgoto_\'NodeTest\''(93) -> 9;
'yeccgoto_\'NodeTest\''(94) -> 9;
'yeccgoto_\'NodeTest\''(101) -> 9;
'yeccgoto_\'NodeTest\''(102) -> 9;
'yeccgoto_\'NodeTest\''(106) -> 9;
'yeccgoto_\'NodeTest\''(108) -> 9.

'yeccgoto_\'OrExpr\''(0) -> 8;
'yeccgoto_\'OrExpr\''(23) -> 8;
'yeccgoto_\'OrExpr\''(45) -> 8;
'yeccgoto_\'OrExpr\''(52) -> 8;
'yeccgoto_\'OrExpr\''(59) -> 8.

'yeccgoto_\'PathExpr\''(0) -> 7;
'yeccgoto_\'PathExpr\''(23) -> 7;
'yeccgoto_\'PathExpr\''(24) -> 7;
'yeccgoto_\'PathExpr\''(45) -> 7;
'yeccgoto_\'PathExpr\''(52) -> 7;
'yeccgoto_\'PathExpr\''(59) -> 7;
'yeccgoto_\'PathExpr\''(75) -> 7;
'yeccgoto_\'PathExpr\''(76) -> 7;
'yeccgoto_\'PathExpr\''(78) -> 7;
'yeccgoto_\'PathExpr\''(80) -> 7;
'yeccgoto_\'PathExpr\''(81) -> 7;
'yeccgoto_\'PathExpr\''(86) -> 7;
'yeccgoto_\'PathExpr\''(88) -> 7;
'yeccgoto_\'PathExpr\''(89) -> 7;
'yeccgoto_\'PathExpr\''(91) -> 7;
'yeccgoto_\'PathExpr\''(92) -> 7;
'yeccgoto_\'PathExpr\''(93) -> 7;
'yeccgoto_\'PathExpr\''(94) -> 7;
'yeccgoto_\'PathExpr\''(106) -> 7;
'yeccgoto_\'PathExpr\''(108) -> 109.

'yeccgoto_\'Predicate\''(9) -> 56;
'yeccgoto_\'Predicate\''(14) -> 100;
'yeccgoto_\'Predicate\''(55) -> 56;
'yeccgoto_\'Predicate\''(57) -> 63;
'yeccgoto_\'Predicate\''(64) -> 56.

'yeccgoto_\'PredicateExpr\''(59) -> 60.

'yeccgoto_\'PrimaryExpr\''(0) -> 6;
'yeccgoto_\'PrimaryExpr\''(23) -> 6;
'yeccgoto_\'PrimaryExpr\''(24) -> 6;
'yeccgoto_\'PrimaryExpr\''(45) -> 6;
'yeccgoto_\'PrimaryExpr\''(52) -> 6;
'yeccgoto_\'PrimaryExpr\''(59) -> 6;
'yeccgoto_\'PrimaryExpr\''(75) -> 6;
'yeccgoto_\'PrimaryExpr\''(76) -> 6;
'yeccgoto_\'PrimaryExpr\''(78) -> 6;
'yeccgoto_\'PrimaryExpr\''(80) -> 6;
'yeccgoto_\'PrimaryExpr\''(81) -> 6;
'yeccgoto_\'PrimaryExpr\''(86) -> 6;
'yeccgoto_\'PrimaryExpr\''(88) -> 6;
'yeccgoto_\'PrimaryExpr\''(89) -> 6;
'yeccgoto_\'PrimaryExpr\''(91) -> 6;
'yeccgoto_\'PrimaryExpr\''(92) -> 6;
'yeccgoto_\'PrimaryExpr\''(93) -> 6;
'yeccgoto_\'PrimaryExpr\''(94) -> 6;
'yeccgoto_\'PrimaryExpr\''(106) -> 6;
'yeccgoto_\'PrimaryExpr\''(108) -> 6.

'yeccgoto_\'RelationalExpr\''(0) -> 5;
'yeccgoto_\'RelationalExpr\''(23) -> 5;
'yeccgoto_\'RelationalExpr\''(45) -> 5;
'yeccgoto_\'RelationalExpr\''(52) -> 5;
'yeccgoto_\'RelationalExpr\''(59) -> 5;
'yeccgoto_\'RelationalExpr\''(86) -> 5;
'yeccgoto_\'RelationalExpr\''(88) -> 99;
'yeccgoto_\'RelationalExpr\''(89) -> 90;
'yeccgoto_\'RelationalExpr\''(106) -> 5.

'yeccgoto_\'RelativeLocationPath\''(0) -> 4;
'yeccgoto_\'RelativeLocationPath\''(23) -> 4;
'yeccgoto_\'RelativeLocationPath\''(24) -> 4;
'yeccgoto_\'RelativeLocationPath\''(27) -> 71;
'yeccgoto_\'RelativeLocationPath\''(28) -> 66;
'yeccgoto_\'RelativeLocationPath\''(45) -> 4;
'yeccgoto_\'RelativeLocationPath\''(52) -> 4;
'yeccgoto_\'RelativeLocationPath\''(59) -> 4;
'yeccgoto_\'RelativeLocationPath\''(75) -> 4;
'yeccgoto_\'RelativeLocationPath\''(76) -> 4;
'yeccgoto_\'RelativeLocationPath\''(78) -> 4;
'yeccgoto_\'RelativeLocationPath\''(80) -> 4;
'yeccgoto_\'RelativeLocationPath\''(81) -> 4;
'yeccgoto_\'RelativeLocationPath\''(86) -> 4;
'yeccgoto_\'RelativeLocationPath\''(88) -> 4;
'yeccgoto_\'RelativeLocationPath\''(89) -> 4;
'yeccgoto_\'RelativeLocationPath\''(91) -> 4;
'yeccgoto_\'RelativeLocationPath\''(92) -> 4;
'yeccgoto_\'RelativeLocationPath\''(93) -> 4;
'yeccgoto_\'RelativeLocationPath\''(94) -> 4;
'yeccgoto_\'RelativeLocationPath\''(101) -> 104;
'yeccgoto_\'RelativeLocationPath\''(102) -> 103;
'yeccgoto_\'RelativeLocationPath\''(106) -> 4;
'yeccgoto_\'RelativeLocationPath\''(108) -> 4.

'yeccgoto_\'Step\''(0) -> 3;
'yeccgoto_\'Step\''(23) -> 3;
'yeccgoto_\'Step\''(24) -> 3;
'yeccgoto_\'Step\''(27) -> 3;
'yeccgoto_\'Step\''(28) -> 3;
'yeccgoto_\'Step\''(45) -> 3;
'yeccgoto_\'Step\''(52) -> 3;
'yeccgoto_\'Step\''(59) -> 3;
'yeccgoto_\'Step\''(67) -> 70;
'yeccgoto_\'Step\''(68) -> 69;
'yeccgoto_\'Step\''(75) -> 3;
'yeccgoto_\'Step\''(76) -> 3;
'yeccgoto_\'Step\''(78) -> 3;
'yeccgoto_\'Step\''(80) -> 3;
'yeccgoto_\'Step\''(81) -> 3;
'yeccgoto_\'Step\''(86) -> 3;
'yeccgoto_\'Step\''(88) -> 3;
'yeccgoto_\'Step\''(89) -> 3;
'yeccgoto_\'Step\''(91) -> 3;
'yeccgoto_\'Step\''(92) -> 3;
'yeccgoto_\'Step\''(93) -> 3;
'yeccgoto_\'Step\''(94) -> 3;
'yeccgoto_\'Step\''(101) -> 3;
'yeccgoto_\'Step\''(102) -> 3;
'yeccgoto_\'Step\''(106) -> 3;
'yeccgoto_\'Step\''(108) -> 3.

'yeccgoto_\'UnaryExpr\''(0) -> 2;
'yeccgoto_\'UnaryExpr\''(23) -> 2;
'yeccgoto_\'UnaryExpr\''(24) -> 72;
'yeccgoto_\'UnaryExpr\''(45) -> 2;
'yeccgoto_\'UnaryExpr\''(52) -> 2;
'yeccgoto_\'UnaryExpr\''(59) -> 2;
'yeccgoto_\'UnaryExpr\''(75) -> 2;
'yeccgoto_\'UnaryExpr\''(76) -> 2;
'yeccgoto_\'UnaryExpr\''(78) -> 84;
'yeccgoto_\'UnaryExpr\''(80) -> 83;
'yeccgoto_\'UnaryExpr\''(81) -> 82;
'yeccgoto_\'UnaryExpr\''(86) -> 2;
'yeccgoto_\'UnaryExpr\''(88) -> 2;
'yeccgoto_\'UnaryExpr\''(89) -> 2;
'yeccgoto_\'UnaryExpr\''(91) -> 2;
'yeccgoto_\'UnaryExpr\''(92) -> 2;
'yeccgoto_\'UnaryExpr\''(93) -> 2;
'yeccgoto_\'UnaryExpr\''(94) -> 2;
'yeccgoto_\'UnaryExpr\''(106) -> 2.

'yeccgoto_\'UnionExpr\''(0) -> 1;
'yeccgoto_\'UnionExpr\''(23) -> 1;
'yeccgoto_\'UnionExpr\''(24) -> 1;
'yeccgoto_\'UnionExpr\''(45) -> 1;
'yeccgoto_\'UnionExpr\''(52) -> 1;
'yeccgoto_\'UnionExpr\''(59) -> 1;
'yeccgoto_\'UnionExpr\''(75) -> 1;
'yeccgoto_\'UnionExpr\''(76) -> 1;
'yeccgoto_\'UnionExpr\''(78) -> 1;
'yeccgoto_\'UnionExpr\''(80) -> 1;
'yeccgoto_\'UnionExpr\''(81) -> 1;
'yeccgoto_\'UnionExpr\''(86) -> 1;
'yeccgoto_\'UnionExpr\''(88) -> 1;
'yeccgoto_\'UnionExpr\''(89) -> 1;
'yeccgoto_\'UnionExpr\''(91) -> 1;
'yeccgoto_\'UnionExpr\''(92) -> 1;
'yeccgoto_\'UnionExpr\''(93) -> 1;
'yeccgoto_\'UnionExpr\''(94) -> 1;
'yeccgoto_\'UnionExpr\''(106) -> 1.

-compile({inline,{yeccpars2_4_,1}}).
-file("xmerl_xpath_parse.yrl", 101).
yeccpars2_4_([__1 | Stack]) ->
 [begin
   { path , rel , __1 }
  end | Stack].

-compile({inline,{yeccpars2_9_,1}}).
-file("xmerl_xpath_parse.yrl", 127).
yeccpars2_9_([__1 | Stack]) ->
 [begin
   { step , { child , __1 , [ ] } }
  end | Stack].

-compile({inline,{yeccpars2_19_,1}}).
-file("xmerl_xpath_parse.yrl", 102).
yeccpars2_19_([__1 | Stack]) ->
 [begin
   { path , abs , __1 }
  end | Stack].

-compile({inline,{yeccpars2_20_,1}}).
-file("xmerl_xpath_parse.yrl", 129).
yeccpars2_20_([__1 | Stack]) ->
 [begin
   { abbrev_step , __1 }
  end | Stack].

-compile({inline,{yeccpars2_27_,1}}).
-file("xmerl_xpath_parse.yrl", 106).
yeccpars2_27_([__1 | Stack]) ->
 [begin
   '/'
  end | Stack].

-compile({inline,{yeccpars2_32_,1}}).
-file("xmerl_xpath_parse.yrl", 180).
yeccpars2_32_([__1 | Stack]) ->
 [begin
   { literal , value ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_33_,1}}).
-file("xmerl_xpath_parse.yrl", 298).
yeccpars2_33_([__1 | Stack]) ->
 [begin
   { name , value ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_35_,1}}).
-file("xmerl_xpath_parse.yrl", 181).
yeccpars2_35_([__1 | Stack]) ->
 [begin
   { number , value ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_36_,1}}).
-file("xmerl_xpath_parse.yrl", 297).
yeccpars2_36_([__1 | Stack]) ->
 [begin
   { prefix_test , value ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_38_,1}}).
-file("xmerl_xpath_parse.yrl", 178).
yeccpars2_38_([__1 | Stack]) ->
 [begin
   { variable_reference , value ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_39_,1}}).
-file("xmerl_xpath_parse.yrl", 296).
yeccpars2_39_([__1 | Stack]) ->
 [begin
   { wildcard , value ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_42_,1}}).
-file("xmerl_xpath_parse.yrl", 149).
yeccpars2_42_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { processing_instruction , value ( __3 ) }
  end | Stack].

-compile({inline,{yeccpars2_44_,1}}).
-file("xmerl_xpath_parse.yrl", 147).
yeccpars2_44_([__3,__2,__1 | Stack]) ->
 [begin
   { node_type , value ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_47_,1}}).
-file("xmerl_xpath_parse.yrl", 194).
yeccpars2_47_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_48_,1}}).
-file("xmerl_xpath_parse.yrl", 190).
yeccpars2_48_([__1 | Stack]) ->
 [begin
   lists : reverse ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_50_,1}}).
-file("xmerl_xpath_parse.yrl", 186).
yeccpars2_50_([__3,__2,__1 | Stack]) ->
 [begin
   { function_call , value ( __1 ) , [ ] }
  end | Stack].

-compile({inline,{yeccpars2_51_,1}}).
-file("xmerl_xpath_parse.yrl", 188).
yeccpars2_51_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { function_call , value ( __1 ) , __3 }
  end | Stack].

-compile({inline,{yeccpars2_53_,1}}).
-file("xmerl_xpath_parse.yrl", 193).
yeccpars2_53_([__3,__2,__1 | Stack]) ->
 [begin
   [ __3 | __1 ]
  end | Stack].

-compile({inline,{yeccpars2_55_,1}}).
-file("xmerl_xpath_parse.yrl", 119).
yeccpars2_55_([__3,__2,__1 | Stack]) ->
 [begin
   { step , { value ( __1 ) , __3 , [ ] } }
  end | Stack].

-compile({inline,{yeccpars2_56_,1}}).
-file("xmerl_xpath_parse.yrl", 137).
yeccpars2_56_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_57_,1}}).
-file("xmerl_xpath_parse.yrl", 132).
yeccpars2_57_([__1 | Stack]) ->
 [begin
   lists : reverse ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_58_,1}}).
-file("xmerl_xpath_parse.yrl", 117).
yeccpars2_58_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { step , { value ( __1 ) , __3 , __4 } }
  end | Stack].

-compile({inline,{yeccpars2_62_,1}}).
-file("xmerl_xpath_parse.yrl", 153).
yeccpars2_62_([__3,__2,__1 | Stack]) ->
 [begin
   { pred , __2 }
  end | Stack].

-compile({inline,{yeccpars2_63_,1}}).
-file("xmerl_xpath_parse.yrl", 136).
yeccpars2_63_([__2,__1 | Stack]) ->
 [begin
   [ __2 | __1 ]
  end | Stack].

-compile({inline,{yeccpars2_64_,1}}).
-file("xmerl_xpath_parse.yrl", 123).
yeccpars2_64_([__2,__1 | Stack]) ->
 [begin
   { step , { attribute , __2 , [ ] } }
  end | Stack].

-compile({inline,{yeccpars2_65_,1}}).
-file("xmerl_xpath_parse.yrl", 121).
yeccpars2_65_([__3,__2,__1 | Stack]) ->
 [begin
   { step , { value ( __1 ) , __2 , __3 } }
  end | Stack].

-compile({inline,{yeccpars2_66_,1}}).
-file("xmerl_xpath_parse.yrl", 160).
yeccpars2_66_([__2,__1 | Stack]) ->
 [begin
   { '//' , __2 }
  end | Stack].

-compile({inline,{yeccpars2_69_,1}}).
-file("xmerl_xpath_parse.yrl", 164).
yeccpars2_69_([__3,__2,__1 | Stack]) ->
 [begin
   { __1 , '//' , __3 }
  end | Stack].

-compile({inline,{yeccpars2_70_,1}}).
-file("xmerl_xpath_parse.yrl", 112).
yeccpars2_70_([__3,__2,__1 | Stack]) ->
 [begin
   { refine , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_71_,1}}).
-file("xmerl_xpath_parse.yrl", 105).
yeccpars2_71_([__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_72_,1}}).
-file("xmerl_xpath_parse.yrl", 267).
yeccpars2_72_([__2,__1 | Stack]) ->
 [begin
   { negative , __2 }
  end | Stack].

-compile({inline,{yeccpars2_74_,1}}).
-file("xmerl_xpath_parse.yrl", 179).
yeccpars2_74_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_77_,1}}).
-file("xmerl_xpath_parse.yrl", 252).
yeccpars2_77_([__3,__2,__1 | Stack]) ->
 [begin
   { arith , '-' , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_79_,1}}).
-file("xmerl_xpath_parse.yrl", 292).
yeccpars2_79_([__1 | Stack]) ->
 [begin
   '*'
  end | Stack].

-compile({inline,{yeccpars2_82_,1}}).
-file("xmerl_xpath_parse.yrl", 262).
yeccpars2_82_([__3,__2,__1 | Stack]) ->
 [begin
   { arith , mod , __1 , __2 }
  end | Stack].

-compile({inline,{yeccpars2_83_,1}}).
-file("xmerl_xpath_parse.yrl", 260).
yeccpars2_83_([__3,__2,__1 | Stack]) ->
 [begin
   { arith , 'div' , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_84_,1}}).
-file("xmerl_xpath_parse.yrl", 258).
yeccpars2_84_([__3,__2,__1 | Stack]) ->
 [begin
   { arith , __2 , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_85_,1}}).
-file("xmerl_xpath_parse.yrl", 250).
yeccpars2_85_([__3,__2,__1 | Stack]) ->
 [begin
   { arith , '+' , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_87_,1}}).
-file("xmerl_xpath_parse.yrl", 226).
yeccpars2_87_([__3,__2,__1 | Stack]) ->
 [begin
   { bool , 'and' , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_90_,1}}).
-file("xmerl_xpath_parse.yrl", 231).
yeccpars2_90_([__3,__2,__1 | Stack]) ->
 [begin
   { comp , '=' , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_95_,1}}).
-file("xmerl_xpath_parse.yrl", 244).
yeccpars2_95_([__3,__2,__1 | Stack]) ->
 [begin
   { comp , '>=' , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_96_,1}}).
-file("xmerl_xpath_parse.yrl", 240).
yeccpars2_96_([__3,__2,__1 | Stack]) ->
 [begin
   { comp , '>' , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_97_,1}}).
-file("xmerl_xpath_parse.yrl", 242).
yeccpars2_97_([__3,__2,__1 | Stack]) ->
 [begin
   { comp , '<=' , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_98_,1}}).
-file("xmerl_xpath_parse.yrl", 238).
yeccpars2_98_([__3,__2,__1 | Stack]) ->
 [begin
   { comp , '<' , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_99_,1}}).
-file("xmerl_xpath_parse.yrl", 233).
yeccpars2_99_([__3,__2,__1 | Stack]) ->
 [begin
   { comp , '!=' , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_100_,1}}).
-file("xmerl_xpath_parse.yrl", 214).
yeccpars2_100_([__2,__1 | Stack]) ->
 [begin
   { path , filter , { __1 , __2 } }
  end | Stack].

-compile({inline,{yeccpars2_103_,1}}).
-file("xmerl_xpath_parse.yrl", 210).
yeccpars2_103_([__3,__2,__1 | Stack]) ->
 [begin
   { __1 , '//' , __3 }
  end | Stack].

-compile({inline,{yeccpars2_104_,1}}).
-file("xmerl_xpath_parse.yrl", 209).
yeccpars2_104_([__3,__2,__1 | Stack]) ->
 [begin
   { refine , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_105_,1}}).
-file("xmerl_xpath_parse.yrl", 125).
yeccpars2_105_([__2,__1 | Stack]) ->
 [begin
   { step , { child , __1 , __2 } }
  end | Stack].

-compile({inline,{yeccpars2_107_,1}}).
-file("xmerl_xpath_parse.yrl", 220).
yeccpars2_107_([__3,__2,__1 | Stack]) ->
 [begin
   { bool , 'or' , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_109_,1}}).
-file("xmerl_xpath_parse.yrl", 203).
yeccpars2_109_([__3,__2,__1 | Stack]) ->
 [begin
   { path , union , { __1 , __3 } }
  end | Stack].


-file("xmerl_xpath_parse.yrl", 316).
