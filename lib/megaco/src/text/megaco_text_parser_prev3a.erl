-module(megaco_text_parser_prev3a).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("megaco_text_parser_prev3a.yrl", 1546).

%% The following directive is needed for (significantly) faster compilation
%% of the generated .erl file by the HiPE compiler.  Please do not remove.
-compile([{hipe,[{regalloc,linear_scan}]}]).

-include("megaco_text_parser_prev3a.hrl").



-file("/ldisk/daily_build/otp_prebuild_r12b.2008-04-07_20/otp_src_R12B-1/bootstrap/lib/parsetools/include/yeccpre.hrl", 0).
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



-file("./megaco_text_parser_prev3a.erl", 164).

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
%% yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_132(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_135(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_136(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(148=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_148(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(149=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(161=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_161(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(162=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(163=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(164=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_164(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(165=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(166=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(167=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(168=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_168(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(169=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(170=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_170(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(171=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_171(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(172=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_172(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(173=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_173(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(174=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(175=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_175(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(176=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_176(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(177=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_177(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(178=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_178(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(179=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_179(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(180=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(181=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(182=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_182(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(183=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(184=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(185=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_185(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(186=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_186(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(187=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_187(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(188=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(189=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(190=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_190(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(191=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(192=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_192(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(193=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_193(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(194=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_194(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(195=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_195(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(196=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_196(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(197=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_197(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(198=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_198(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(199=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_199(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(200=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_200(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(201=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_201(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(202=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(203=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(204=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(205=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_205(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(206=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_206(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(207=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(213=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_213(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(214=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_214(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_221(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(222=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(223=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_223(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(224=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_224(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(225=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_225(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(226=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_226(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(227=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(228=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(229=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(230=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(231=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_231(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(232=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_232(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(233=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_233(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(234=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_234(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(235=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_235(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(236=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_236(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(237=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_237(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(238=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_238(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(239=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_239(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(240=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_240(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(241=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_234(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(242=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_242(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(243=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_243(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(244=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(245=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(246=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_246(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(247=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_247(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(248=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_248(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(249=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_249(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(250=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_250(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(251=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_251(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(252=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_252(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(253=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_253(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(254=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(255=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(256=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(257=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_257(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(258=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(259=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_259(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(260=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_260(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(261=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_261(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(262=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_262(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(263=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_263(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(264=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(265=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(266=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_266(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(267=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_267(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(268=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(269=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(270=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_270(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(271=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_271(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(272=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_272(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(273=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_273(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(274=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_274(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(275=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_275(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(276=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_276(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(277=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_277(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(278=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_278(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(279=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(280=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_280(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(281=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_281(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(282=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_282(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(283=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(284=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_284(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(285=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_285(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(286=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_286(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(287=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_287(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(288=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_288(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(289=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_289(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(290=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_290(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(291=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_291(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(292=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_292(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(293=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_293(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(294=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_294(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(295=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_295(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(296=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_296(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(297=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_297(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(298=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(299=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_299(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(300=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_300(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(301=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_301(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(302=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(303=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_303(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(304=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_304(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(305=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_305(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(306=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_306(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(307=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(308=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_308(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(309=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_309(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(310=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_310(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(311=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(312=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_312(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(313=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_313(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(314=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_314(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(315=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_315(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(316=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(317=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_317(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(318=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(319=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_319(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(320=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_320(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(321=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_321(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(322=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(323=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_323(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(324=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_324(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(325=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_325(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(326=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_326(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(327=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_327(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(328=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_328(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(329=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_329(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(330=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_330(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(331=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_331(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(332=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_332(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(333=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(334=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_334(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(335=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_335(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(336=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_336(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(337=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_337(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(338=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_338(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(339=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_339(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(340=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_340(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(341=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_341(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(342=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(343=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_343(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(344=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(345=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(346=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_346(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(347=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_347(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(348=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_348(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(349=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_349(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(350=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(351=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(352=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_352(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(353=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_353(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(354=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_354(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(355=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_355(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(356=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_356(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(357=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_357(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(358=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_358(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(359=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_359(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(360=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_360(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(361=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_361(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(362=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_362(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(363=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_363(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(364=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_364(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(365=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_365(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(366=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_366(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(367=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_214(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(368=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_368(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(369=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_369(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(380=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_380(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(381=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_381(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(382=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_382(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(383=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_383(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(384=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(385=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_385(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(386=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(387=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_387(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(388=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_388(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(389=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_389(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(390=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(391=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_391(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(392=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(393=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_393(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(394=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_394(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(395=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_395(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(396=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_356(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(397=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_397(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(398=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_398(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(399=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_399(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(400=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_400(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(401=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(402=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_402(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(403=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_403(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(404=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_404(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(405=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_405(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(406=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_406(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(407=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_407(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(408=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(409=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_409(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(410=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_410(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(411=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_411(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(412=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(413=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_413(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(414=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_414(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(415=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_415(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(416=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_416(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(417=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_417(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(418=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_418(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(419=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_419(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(420=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_420(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(421=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(422=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_422(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(423=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_423(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(424=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_214(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(425=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_425(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(426=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_426(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(427=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(428=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_428(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(429=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_429(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(430=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_430(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(431=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_431(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(432=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_432(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(433=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(434=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_434(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(435=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_435(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(436=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_436(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(437=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_437(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(438=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_438(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(439=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_439(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(440=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_440(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(441=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_441(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(442=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_442(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(443=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_443(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(444=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_444(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(445=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_445(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(446=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_446(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(447=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_447(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(448=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_440(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(449=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_449(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(450=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_450(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(451=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_451(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(452=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_452(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(453=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_453(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(454=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_454(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(455=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_455(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(456=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_456(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(457=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_214(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(458=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_458(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(459=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_459(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(460=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_460(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(461=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_461(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(462=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_462(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(463=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_463(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(464=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_464(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(465=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(466=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_466(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(467=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_467(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(468=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_468(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(469=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(470=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_470(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(471=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_471(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(472=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_472(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(473=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_473(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(474=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_474(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(475=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_475(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(476=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_476(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(477=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_477(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(478=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(479=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_479(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(480=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_480(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(481=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_481(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(482=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(483=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_483(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(484=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_484(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(485=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_453(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(486=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_486(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(487=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_487(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(488=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_488(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(489=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_489(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(490=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_137(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(499=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_499(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(500=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_500(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(501=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(502=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_502(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(503=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_503(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(504=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_504(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(505=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_505(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(506=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_506(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(507=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_507(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(524=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_524(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(525=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_525(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(526=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(527=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_527(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(528=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_528(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(529=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(530=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_530(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(531=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_531(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(532=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_532(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(533=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_533(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(534=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_534(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(535=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_535(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(536=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_536(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(537=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_537(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(538=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(539=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_539(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(540=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(541=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_541(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(542=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_542(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(543=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(544=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_544(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(545=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_545(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(546=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_546(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(547=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_547(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(548=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_533(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(549=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_549(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(550=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_550(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(551=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_551(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(552=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(553=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_553(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(554=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_554(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(555=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_555(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(556=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(557=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_557(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(558=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_558(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(559=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(560=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_560(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(561=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_561(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(562=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_562(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(563=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(564=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(565=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_565(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(566=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_566(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(567=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_567(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(568=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(569=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_569(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(570=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_570(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(571=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_571(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(572=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_572(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(573=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(574=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_574(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(575=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_575(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(576=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(577=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_577(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(578=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_578(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(579=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_579(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(580=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_580(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(581=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_581(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(582=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_582(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(583=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_583(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(584=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_584(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(585=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_585(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(586=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_586(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(587=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_587(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(588=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_588(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(589=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_589(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(590=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_590(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(591=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_591(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(592=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_592(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(593=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_593(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(594=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_594(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(595=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_595(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(596=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_596(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(597=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_597(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(598=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_598(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(599=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_599(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(600=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_600(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(601=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_601(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(602=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_602(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(603=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_603(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(604=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_604(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(605=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_605(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(606=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_606(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(607=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_607(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(608=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_608(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(609=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_609(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(610=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_610(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(611=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_594(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(612=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_612(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(613=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_613(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(614=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_614(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(615=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(616=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_616(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(617=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_617(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(618=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_618(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(619=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_619(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(620=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_617(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(621=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_621(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(622=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_622(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(623=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_623(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(624=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_624(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(625=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_625(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(626=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_626(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(627=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_627(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(628=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_628(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(629=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_629(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(630=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_630(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(631=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_631(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(632=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_632(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(633=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_633(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(634=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_630(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(635=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_635(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(636=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_636(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(637=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_637(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(643=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_643(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(644=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_624(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(645=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_645(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(646=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_646(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(647=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_647(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(648=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_648(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(649=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_582(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(650=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_650(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(651=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_651(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(652=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_652(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(653=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(654=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_654(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(655=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(656=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_656(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(657=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_657(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(658=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_658(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(659=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_659(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(660=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_660(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(661=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_661(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(662=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_662(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(663=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_663(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(664=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_664(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(665=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_665(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(666=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_666(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(667=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_667(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(668=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_668(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(669=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_669(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(670=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_670(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(671=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_671(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(672=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(673=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_673(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(674=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(675=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_675(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(676=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_676(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(677=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_677(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(678=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_678(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(679=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_679(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(680=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_680(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(681=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_681(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(682=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_682(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(683=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_683(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(684=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_684(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(685=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_685(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(686=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_686(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(687=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_687(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(688=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_688(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(689=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_678(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(690=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_690(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(691=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_691(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(692=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_692(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(693=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_693(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(694=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(695=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_695(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(696=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_696(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(697=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_697(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(698=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_698(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(699=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_699(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(700=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_700(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(701=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_701(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(702=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_702(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(703=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_703(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(704=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_659(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(705=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_705(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(706=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_706(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(707=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_707(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(708=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_708(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(709=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(710=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_710(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(711=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_711(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(712=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_712(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(713=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_713(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(714=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_714(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(715=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_715(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(716=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_716(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(717=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_717(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(718=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_718(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(719=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_719(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(720=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_720(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(721=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_721(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(722=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_504(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(723=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_723(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(724=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_724(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(725=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_725(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(726=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_726(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(727=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(728=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_728(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(729=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_729(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(730=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_730(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(731=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_731(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(732=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_732(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(733=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(734=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_734(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(735=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_735(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(736=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_736(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(737=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(738=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_738(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(739=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_739(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(740=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_740(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(741=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(742=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_742(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(743=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_743(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(744=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_744(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(745=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(746=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_746(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(747=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_747(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(748=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_748(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(749=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(750=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_750(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(751=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_751(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(752=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_752(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(753=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_753(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(754=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_754(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(755=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_755(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(756=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_756(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(757=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_757(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(758=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_758(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(759=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(760=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_760(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(761=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_761(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(762=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_762(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(763=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_763(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(764=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_764(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(765=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_765(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(766=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_766(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(767=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_767(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(768=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_768(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(769=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_769(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(770=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_770(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(771=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_771(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(772=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_772(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(773=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_773(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(780=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_780(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(781=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_781(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(782=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_782(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(783=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_783(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(784=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_784(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(785=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_785(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(786=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_786(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(787=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_787(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(788=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_788(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(789=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_789(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(790=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_790(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(791=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_791(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(792=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_792(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(793=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_793(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(794=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_794(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(795=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_795(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(796=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_796(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(797=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_797(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(798=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_786(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(799=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_799(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(800=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_800(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(801=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_801(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(802=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_802(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(803=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_803(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(804=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(805=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_805(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(806=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_806(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(807=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_807(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(808=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_808(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(809=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_809(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(810=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_810(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(811=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_811(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(812=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_812(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(813=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_813(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(814=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_814(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(815=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_815(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(816=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_816(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(817=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_817(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(818=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_818(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(819=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(820=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_820(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(826=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_826(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(827=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_827(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(828=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_828(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(829=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_829(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(830=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_830(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(831=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_831(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(832=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_832(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(849=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_849(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(850=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_850(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(851=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(852=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_852(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(853=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_853(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(854=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_854(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(855=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_855(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(856=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_827(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(857=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_857(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(858=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_858(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(859=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_859(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(860=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_860(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(861=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_861(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(862=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_862(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(863=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_863(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(864=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_864(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(865=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(866=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_866(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(867=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_867(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(868=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_827(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(869=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_869(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(870=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_870(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(871=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_871(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(872=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_761(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(873=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_873(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(874=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_874(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(875=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_875(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(876=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_876(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(877=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_877(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(878=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_878(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(879=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_879(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(880=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_880(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(881=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(882=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_882(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(883=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_883(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(884=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_884(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(885=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_885(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(886=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_886(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(887=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_887(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(888=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_888(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(889=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_889(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(890=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_890(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(891=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_891(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(892=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_892(S, Cat, Ss, Stack, T, Ts, Tzr);
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

yeccpars2_4(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_cont_4(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr).

yeccpars2_5(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_6: see yeccpars2_4

yeccpars2_7(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr).

yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_8_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_9_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_11_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_12_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_13_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_14_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_15_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_16_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_17_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_18_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_19_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_20_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_21_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_22_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_23_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_24_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_25_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_26_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_27_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_28_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_29_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_30_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_32_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_33_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_34_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_35_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_36_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_37_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_38_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_39_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_40_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_41_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_42_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_43_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_44_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_45_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_46_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_47_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_48_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_49_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_50_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_51_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_52_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_53_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_54_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_55_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_56_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_57_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_58_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_59_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_60_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_61_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_62_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_63_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_64_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_65_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_66_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_67_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_68_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_69_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_70_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_71_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_72_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_73_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_74_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_75_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_76_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_77_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_78_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_79_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_80_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_81_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_82_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_83_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_84_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_85_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_86_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_87_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_88: see yeccpars2_4

yeccpars2_89(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_90: see yeccpars2_4

yeccpars2_91(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_91_(Stack),
 yeccpars2_92(_S, Cat, [91 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_92_(Stack),
 yeccgoto_authenticationHeader(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_93(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_93_(Stack),
 yeccpars2_96(96, Cat, [93 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_94(S, endOfMessage, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr).

yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_95_(Stack),
 yeccgoto_megacoMessage(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_96(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'MtpAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 890, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

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
 yeccgoto_mId(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_mId(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

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
 yeccpars2_103(103, Cat, [101 | Ss], NewStack, T, Ts, Tzr).

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
 yeccpars2_111(_S, Cat, [102 | Ss], NewStack, T, Ts, Tzr).

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
 yeccpars2_105(_S, Cat, [104 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_105_(Stack),
 yeccgoto_daddr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_106(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_106_(Stack),
 yeccgoto_domainAddress(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_107: see yeccpars2_4

yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_108_(Stack),
 yeccgoto_portNumber(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_109(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_109_(Stack),
 yeccpars2_110(_S, Cat, [109 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_110_(Stack),
 yeccgoto_domainAddress(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_111_(Stack),
 yeccgoto_daddr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_112(S, 'GREATER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 113, Ss, Stack, T, Ts, Tzr).

yeccpars2_113(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_113_(Stack),
 yeccgoto_domainName(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_114: see yeccpars2_4

yeccpars2_115(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_115_(Stack),
 yeccpars2_116(_S, Cat, [115 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_116_(Stack),
 yeccgoto_domainName(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_117_(Stack),
 yeccgoto_transactionItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_118_(Stack),
 yeccgoto_transactionItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_119_(Stack),
 yeccgoto_transactionItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_120_(Stack),
 yeccgoto_transactionItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_121_(Stack),
 yeccgoto_messageBody(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

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
 yeccgoto_transactionList(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_123_(Stack),
 yeccgoto_message(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_124_(Stack),
 yeccgoto_messageBody(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_125(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 819, Ss, Stack, T, Ts, Tzr).

yeccpars2_126(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 881, Ss, Stack, T, Ts, Tzr).

yeccpars2_127(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 749, Ss, Stack, T, Ts, Tzr).

yeccpars2_128(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 741, Ss, Stack, T, Ts, Tzr).

yeccpars2_129(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr).

yeccpars2_130(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 733, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_131(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr).

yeccpars2_132(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 727, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_132_(Stack),
 yeccpars2_726(726, Cat, [132 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_133(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_134: see yeccpars2_4

yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_135_(Stack),
 yeccgoto_contextID(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_136(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr).

yeccpars2_137(S, 'AddToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 150, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 151, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 152, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(S, 'ContextAttrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 153, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 154, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(S, 'ModifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 155, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(S, 'MoveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 157, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(S, 'SubtractToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 159, Ss, Stack, T, Ts, Tzr).

yeccpars2_138(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_commandRequest(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_commandRequest(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_140(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_commandRequest(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_141_(Stack),
 yeccgoto_actionRequestItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_142_(Stack),
 yeccgoto_actionRequestItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_143_(Stack),
 yeccgoto_contextProperties(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_144_(Stack),
 yeccgoto_actionRequestItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_commandRequest(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_146(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 501, Ss, Stack, T, Ts, Tzr).

yeccpars2_147(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_commandRequest(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_148(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 498, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_148_(Stack),
 yeccpars2_497(_S, Cat, [148 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_149(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 496, Ss, Stack, T, Ts, Tzr).

yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_(Stack),
 yeccgoto_ammToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_151(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 493, Ss, Stack, T, Ts, Tzr).

yeccpars2_152(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 490, Ss, Stack, T, Ts, Tzr).

yeccpars2_153(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 453, Ss, Stack, T, Ts, Tzr).

yeccpars2_154(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 437, Ss, Stack, T, Ts, Tzr).

yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_155_(Stack),
 yeccgoto_ammToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_156(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_156_(Stack),
 yeccgoto_ammToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_157(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 401, Ss, Stack, T, Ts, Tzr).

yeccpars2_158(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 351, Ss, Stack, T, Ts, Tzr).

yeccpars2_159(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 160, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_160: see yeccpars2_4

yeccpars2_161(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_161_(Stack),
 yeccpars2_163(_S, Cat, [161 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_162_(Stack),
 yeccgoto_terminationID(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_163(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_163_(Stack),
 yeccgoto_subtractRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_164(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 166, Ss, Stack, T, Ts, Tzr).

yeccpars2_165(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 350, Ss, Stack, T, Ts, Tzr).

yeccpars2_166(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr).

yeccpars2_167(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, 'DigitMapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, 'ModemToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, 'MuxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, 'ObservedEventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, 'PackagesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_167_(Stack),
 yeccpars2_179(179, Cat, [167 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_168_(Stack),
 yeccgoto_auditItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_169(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_169_(Stack),
 yeccgoto_indAudauditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_170_(Stack),
 yeccgoto_indAudauditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_171_(Stack),
 yeccgoto_indAudauditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_172_(Stack),
 yeccgoto_indAudauditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_173_(Stack),
 yeccgoto_indAudauditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_174(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_174_(Stack),
 yeccgoto_indAudauditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_175(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_175_(Stack),
 yeccgoto_indAudauditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_176(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 342, Ss, Stack, T, Ts, Tzr);
yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_176_(Stack),
 yeccpars2_341(_S, Cat, [176 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_177(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_auditItem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_178(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 338, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_178_(Stack),
 yeccpars2_337(_S, Cat, [178 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_179(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 336, Ss, Stack, T, Ts, Tzr).

yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_180_(Stack),
 yeccgoto_indAuddigitMapDescriptor(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_181(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_181_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_182(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 322, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_182_(Stack),
 yeccgoto_auditItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_183(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 316, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_183_(Stack),
 yeccgoto_auditItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_184(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 287, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_184_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_185(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_185_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_186(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_186_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_187(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_187_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_188(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 283, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_188_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_189(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_189_(Stack),
 yeccgoto_auditItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_190(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_190_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_191: see yeccpars2_4

yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_192_(Stack),
 yeccgoto_pkgdName(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_193(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 194, Ss, Stack, T, Ts, Tzr).

yeccpars2_194(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_194_(Stack),
 yeccgoto_indAudstatisticsDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_195(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_195_(Stack),
 yeccgoto_indAudsignalsDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_196(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 202, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_197(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_197_(Stack),
 yeccgoto_indAudsignalParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_198(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 212, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_198_(Stack),
 yeccgoto_signalRequest(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_signalName(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_200(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 211, Ss, Stack, T, Ts, Tzr).

yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_201_(Stack),
 yeccgoto_indAudsignalParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_202_(Stack),
 yeccgoto_optIndAudsignalParm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_203(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 204, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_203_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_204: see yeccpars2_4

yeccpars2_205(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 207, Ss, Stack, T, Ts, Tzr).

yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_(Stack),
 yeccgoto_signalListId(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_207: see yeccpars2_4

yeccpars2_208(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_signalListParm(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_209(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 210, Ss, Stack, T, Ts, Tzr).

yeccpars2_210(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_210_(Stack),
 yeccgoto_indAudsignalList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_211(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_211_(Stack),
 yeccgoto_optIndAudsignalParm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_212(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 215, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 216, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 217, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 218, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 219, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 220, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 221, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_213(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 279, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_213_(Stack),
 yeccpars2_278(278, Cat, [213 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_214(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 253, Ss, Stack, T, Ts, Tzr);
yeccpars2_214(S, 'GREATER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 254, Ss, Stack, T, Ts, Tzr);
yeccpars2_214(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 255, Ss, Stack, T, Ts, Tzr);
yeccpars2_214(S, 'NEQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 256, Ss, Stack, T, Ts, Tzr).

yeccpars2_215(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 247, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_215_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_216(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 245, Ss, Stack, T, Ts, Tzr);
yeccpars2_216(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_216_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_217(_S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_217_COMMA(Stack),
 yeccgoto_sigParameter(hd(Ss), 'COMMA', Ss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_217_RBRKT(Stack),
 yeccgoto_sigParameter(hd(Ss), 'RBRKT', Ss, NewStack, T, Ts, Tzr);
yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_217_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_218(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 233, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_218_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_219(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 230, Ss, Stack, T, Ts, Tzr);
yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_219_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_220(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 225, Ss, Stack, T, Ts, Tzr);
yeccpars2_220(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_220_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_221(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 222, Ss, Stack, T, Ts, Tzr);
yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_221_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_222: see yeccpars2_4

yeccpars2_223(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_223_(Stack),
 yeccgoto_sigParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_224(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_224_(Stack),
 yeccgoto_streamID(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_225(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 227, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 228, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 229, Ss, Stack, T, Ts, Tzr).

yeccpars2_226(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_226_(Stack),
 yeccgoto_sigParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_227_(Stack),
 yeccgoto_signalType(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_228_(Stack),
 yeccgoto_signalType(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_229(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_229_(Stack),
 yeccgoto_signalType(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_230: see yeccpars2_4

yeccpars2_231(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_231_(Stack),
 yeccgoto_requestID(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_232(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_232_(Stack),
 yeccgoto_sigParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_233(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 234, Ss, Stack, T, Ts, Tzr).

yeccpars2_234(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 236, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 237, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 238, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 239, Ss, Stack, T, Ts, Tzr).

yeccpars2_235(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 241, Ss, Stack, T, Ts, Tzr);
yeccpars2_235(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_235_(Stack),
 yeccpars2_240(240, Cat, [235 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_236(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_236_(Stack),
 yeccgoto_notificationReason(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_237(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_237_(Stack),
 yeccgoto_notificationReason(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_238(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_238_(Stack),
 yeccgoto_notificationReason(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_239(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_239_(Stack),
 yeccgoto_notificationReason(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_240(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 244, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_241: see yeccpars2_234

yeccpars2_242(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 241, Ss, Stack, T, Ts, Tzr);
yeccpars2_242(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_242_(Stack),
 yeccpars2_243(_S, Cat, [242 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_243(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_243_(Stack),
 yeccgoto_notificationReasons(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_244(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_244_(Stack),
 yeccgoto_sigParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_245: see yeccpars2_4

yeccpars2_246(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_246_(Stack),
 yeccgoto_sigParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_247(S, 'BothToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 249, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'ExternalToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 250, Ss, Stack, T, Ts, Tzr);
yeccpars2_247(S, 'InternalToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 251, Ss, Stack, T, Ts, Tzr).

yeccpars2_248(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_248_(Stack),
 yeccgoto_sigParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_249(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_249_(Stack),
 yeccgoto_direction(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_250(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_250_(Stack),
 yeccgoto_direction(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_251(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_251_(Stack),
 yeccgoto_direction(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_252(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_252_(Stack),
 yeccgoto_sigParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_253(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 264, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 265, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'QuotedChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 259, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_254(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_254(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_254(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_254(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_254(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_254(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_254(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_254(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_254(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_254(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_254(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_254(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_254(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_254(S, 'QuotedChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 259, Ss, Stack, T, Ts, Tzr);
yeccpars2_254(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_254(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_254(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_254(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_254(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_254(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_254(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_254(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_254(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_254(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_254(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_255: see yeccpars2_254

%% yeccpars2_256: see yeccpars2_254

yeccpars2_257(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_257_(Stack),
 yeccgoto_parmValue(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_258(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_258_(Stack),
 yeccgoto_value(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_259(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_259_(Stack),
 yeccgoto_value(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_260(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_260_(Stack),
 yeccgoto_parmValue(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_261(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_261_(Stack),
 yeccgoto_parmValue(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_262(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_262_(Stack),
 yeccgoto_alternativeValue(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_263(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_263_(Stack),
 yeccgoto_parmValue(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_264: see yeccpars2_254

%% yeccpars2_265: see yeccpars2_254

yeccpars2_266(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 268, Ss, Stack, T, Ts, Tzr);
yeccpars2_266(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 269, Ss, Stack, T, Ts, Tzr);
yeccpars2_266(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_266_(Stack),
 yeccpars2_267(267, Cat, [266 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_267(S, 'RSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 274, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_268: see yeccpars2_254

%% yeccpars2_269: see yeccpars2_254

yeccpars2_270(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 269, Ss, Stack, T, Ts, Tzr);
yeccpars2_270(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_270_(Stack),
 yeccpars2_271(_S, Cat, [270 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_271(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_271_(Stack),
 yeccgoto_valueList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_272(S, 'RSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 273, Ss, Stack, T, Ts, Tzr).

yeccpars2_273(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_273_(Stack),
 yeccgoto_alternativeValue(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_274(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_274_(Stack),
 yeccgoto_alternativeValue(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_275(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 269, Ss, Stack, T, Ts, Tzr);
yeccpars2_275(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_275_(Stack),
 yeccpars2_276(276, Cat, [275 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_276(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 277, Ss, Stack, T, Ts, Tzr).

yeccpars2_277(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_277_(Stack),
 yeccgoto_alternativeValue(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_278(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 282, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_279: see yeccpars2_212

yeccpars2_280(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 279, Ss, Stack, T, Ts, Tzr);
yeccpars2_280(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_280_(Stack),
 yeccpars2_281(_S, Cat, [280 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_281(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_281_(Stack),
 yeccgoto_sigParameters(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_282(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_282_(Stack),
 yeccgoto_signalRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_283: see yeccpars2_4

yeccpars2_284(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_284_(Stack),
 yeccgoto_packagesItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_285(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 286, Ss, Stack, T, Ts, Tzr).

yeccpars2_286(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_286_(Stack),
 yeccgoto_indAudpackagesDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_287(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 294, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 295, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 296, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 297, Ss, Stack, T, Ts, Tzr).

yeccpars2_288(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_288_(Stack),
 yeccgoto_indAudmediaParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_289(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_289_(Stack),
 yeccgoto_indAudmediaParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_290(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_290_(Stack),
 yeccgoto_indAudmediaParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_291(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_291_(Stack),
 yeccgoto_indAudstreamParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_292(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 315, Ss, Stack, T, Ts, Tzr).

yeccpars2_293(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_293_(Stack),
 yeccgoto_indAudstreamParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_294(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 307, Ss, Stack, T, Ts, Tzr).

yeccpars2_295(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr).

yeccpars2_296(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 302, Ss, Stack, T, Ts, Tzr).

yeccpars2_297(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 298, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_298: see yeccpars2_4

yeccpars2_299(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_299_(Stack),
 yeccgoto_indAudterminationStateParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_300(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 301, Ss, Stack, T, Ts, Tzr).

yeccpars2_301(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_301_(Stack),
 yeccgoto_indAudterminationStateDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_302: see yeccpars2_4

yeccpars2_303(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 304, Ss, Stack, T, Ts, Tzr).

yeccpars2_304(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 294, Ss, Stack, T, Ts, Tzr);
yeccpars2_304(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 295, Ss, Stack, T, Ts, Tzr).

yeccpars2_305(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 306, Ss, Stack, T, Ts, Tzr).

yeccpars2_306(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_306_(Stack),
 yeccgoto_indAudstreamDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_307: see yeccpars2_4

yeccpars2_308(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_308_(Stack),
 yeccgoto_indAudlocalParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_309(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 311, Ss, Stack, T, Ts, Tzr);
yeccpars2_309(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_309_(Stack),
 yeccpars2_310(310, Cat, [309 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_310(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 314, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_311: see yeccpars2_4

yeccpars2_312(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 311, Ss, Stack, T, Ts, Tzr);
yeccpars2_312(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_312_(Stack),
 yeccpars2_313(_S, Cat, [312 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_313(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_313_(Stack),
 yeccgoto_indAudlocalParmList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_314(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_314_(Stack),
 yeccgoto_indAudlocalControlDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_315(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_315_(Stack),
 yeccgoto_indAudmediaDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_316: see yeccpars2_4

yeccpars2_317(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 318, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_318: see yeccpars2_4

yeccpars2_319(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_indAudrequestedEvent(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_320(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 321, Ss, Stack, T, Ts, Tzr).

yeccpars2_321(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_321_(Stack),
 yeccgoto_indAudeventsDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_322: see yeccpars2_4

yeccpars2_323(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 327, Ss, Stack, T, Ts, Tzr);
yeccpars2_323(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_323_(Stack),
 yeccpars2_326(_S, Cat, [323 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_324(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 325, Ss, Stack, T, Ts, Tzr).

yeccpars2_325(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_325_(Stack),
 yeccgoto_indAudeventBufferDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_326(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_326_(Stack),
 yeccgoto_indAudeventSpec(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_327(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_327(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_327(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_327(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_327(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_327(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_327(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_327(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_327(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_327(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_327(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_327(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_327(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_327(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_327(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_327(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_327(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_327(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_327(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_327(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_327(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_327(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 332, Ss, Stack, T, Ts, Tzr);
yeccpars2_327(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_327(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_328(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_328_(Stack),
 yeccgoto_eventParameterName(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_329(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 335, Ss, Stack, T, Ts, Tzr).

yeccpars2_330(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_330_(Stack),
 yeccgoto_indAudeventSpecParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_331(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_331_(Stack),
 yeccgoto_indAudeventSpecParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_332(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 333, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_332_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_333: see yeccpars2_4

yeccpars2_334(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_334_(Stack),
 yeccgoto_eventStream(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_335(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_335_(Stack),
 yeccgoto_optIndAudeventSpecParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_336(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_336_(Stack),
 yeccgoto_auditDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_337(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_337_(Stack),
 yeccgoto_auditDescriptorBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_338(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_338(S, 'DigitMapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_338(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_338(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_338(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_338(S, 'ModemToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_338(S, 'MuxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_338(S, 'ObservedEventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_338(S, 'PackagesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_338(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_338(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr).

yeccpars2_339(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 338, Ss, Stack, T, Ts, Tzr);
yeccpars2_339(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_339_(Stack),
 yeccpars2_340(_S, Cat, [339 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_340(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_340_(Stack),
 yeccgoto_auditItemList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_341_(Stack),
 yeccgoto_indAudterminationAudit(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_342(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_342(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 344, Ss, Stack, T, Ts, Tzr);
yeccpars2_342(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 345, Ss, Stack, T, Ts, Tzr);
yeccpars2_342(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 346, Ss, Stack, T, Ts, Tzr);
yeccpars2_342(S, 'PackagesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 347, Ss, Stack, T, Ts, Tzr);
yeccpars2_342(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 348, Ss, Stack, T, Ts, Tzr);
yeccpars2_342(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 295, Ss, Stack, T, Ts, Tzr).

yeccpars2_343(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 342, Ss, Stack, T, Ts, Tzr);
yeccpars2_343(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_343_(Stack),
 yeccpars2_349(_S, Cat, [343 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_344(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 322, Ss, Stack, T, Ts, Tzr).

yeccpars2_345(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 316, Ss, Stack, T, Ts, Tzr).

yeccpars2_346(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 287, Ss, Stack, T, Ts, Tzr).

yeccpars2_347(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 283, Ss, Stack, T, Ts, Tzr).

yeccpars2_348(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr).

yeccpars2_349(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_349_(Stack),
 yeccgoto_indAudterminationAuditList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_350_(Stack),
 yeccgoto_optAuditDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_351: see yeccpars2_4

yeccpars2_352(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 353, Ss, Stack, T, Ts, Tzr).

yeccpars2_353(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 355, Ss, Stack, T, Ts, Tzr).

yeccpars2_354(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 400, Ss, Stack, T, Ts, Tzr).

yeccpars2_355(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 356, Ss, Stack, T, Ts, Tzr).

yeccpars2_356(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 370, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'DigitMapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 184, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 371, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 372, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'ModemToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'MuxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'ObservedEventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'PackagesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 373, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 374, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 375, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'ServiceChangeIncompleteToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 376, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 189, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 190, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 377, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 378, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_357(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_357_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_358(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_358_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_359(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_359_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_360(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_360_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_361(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 396, Ss, Stack, T, Ts, Tzr);
yeccpars2_361(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_361_(Stack),
 yeccpars2_395(395, Cat, [361 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_362(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_362_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_363(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_363_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_364(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_364_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_365(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_365_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_366(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_(Stack),
 yeccgoto_extensionParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_367: see yeccpars2_214

yeccpars2_368(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_368_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_369(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_369_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_370(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 392, Ss, Stack, T, Ts, Tzr);
yeccpars2_370(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_371(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 390, Ss, Stack, T, Ts, Tzr);
yeccpars2_371(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_371_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_372(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 388, Ss, Stack, T, Ts, Tzr);
yeccpars2_372(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_372_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_373(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 386, Ss, Stack, T, Ts, Tzr);
yeccpars2_373(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_373_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_374(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 384, Ss, Stack, T, Ts, Tzr);
yeccpars2_374(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_374_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_375(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 381, Ss, Stack, T, Ts, Tzr);
yeccpars2_375(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_375_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_376(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_376_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_377(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_377_(Stack),
 yeccgoto_timeStamp(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_378(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 379, Ss, Stack, T, Ts, Tzr);
yeccpars2_378(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_378_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_379: see yeccpars2_4

yeccpars2_380(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_380_(Stack),
 yeccgoto_serviceChangeVersion(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_381(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_381(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_381_(Stack),
 yeccpars2_96(96, Cat, [381 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_382(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_382_(Stack),
 yeccgoto_serviceChangeAddress(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_383(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_383_(Stack),
 yeccgoto_serviceChangeAddress(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_384: see yeccpars2_254

yeccpars2_385(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_385_(Stack),
 yeccgoto_serviceChangeReason(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_386: see yeccpars2_4

yeccpars2_387(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_387_(Stack),
 yeccgoto_serviceChangeProfile(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_388(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_388(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_388(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_388(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_388_(Stack),
 yeccpars2_96(96, Cat, [388 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_389(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_389_(Stack),
 yeccgoto_serviceChangeMgcId(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_390: see yeccpars2_4

yeccpars2_391(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_391_(Stack),
 yeccgoto_serviceChangeMethod(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_392: see yeccpars2_4

yeccpars2_393(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_393_(Stack),
 yeccgoto_serviceChangeDelay(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_394(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_394_(Stack),
 yeccgoto_extension(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_395(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 399, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_396: see yeccpars2_356

yeccpars2_397(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 396, Ss, Stack, T, Ts, Tzr);
yeccpars2_397(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_397_(Stack),
 yeccpars2_398(_S, Cat, [397 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_398(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_398_(Stack),
 yeccgoto_serviceChangeParms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_399(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_399_(Stack),
 yeccgoto_serviceChangeDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_400(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_400_(Stack),
 yeccgoto_serviceChangeRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_401: see yeccpars2_4

yeccpars2_402(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 403, Ss, Stack, T, Ts, Tzr).

yeccpars2_403(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_403(S, 'ObservedEventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 407, Ss, Stack, T, Ts, Tzr).

yeccpars2_404(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_404_(Stack),
 yeccgoto_notifyRequestBody(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_405(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 436, Ss, Stack, T, Ts, Tzr).

yeccpars2_406(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_406_(Stack),
 yeccgoto_notifyRequestBody(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_407(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 408, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_408: see yeccpars2_4

yeccpars2_409(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 410, Ss, Stack, T, Ts, Tzr).

yeccpars2_410(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 377, Ss, Stack, T, Ts, Tzr);
yeccpars2_410(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_410_(Stack),
 yeccpars2_4(412, Cat, [410 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_411(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_411(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_411_(Stack),
 yeccpars2_431(431, Cat, [411 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_412: see yeccpars2_4

yeccpars2_413(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 415, Ss, Stack, T, Ts, Tzr);
yeccpars2_413(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_413_(Stack),
 yeccpars2_414(414, Cat, [413 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_414(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 418, Ss, Stack, T, Ts, Tzr).

yeccpars2_415(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_415(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 377, Ss, Stack, T, Ts, Tzr);
yeccpars2_415(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_415_(Stack),
 yeccpars2_4(412, Cat, [415 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_416(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 415, Ss, Stack, T, Ts, Tzr);
yeccpars2_416(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_416_(Stack),
 yeccpars2_417(_S, Cat, [416 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_417(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_417_(Stack),
 yeccgoto_observedEvents(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_418(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_418_(Stack),
 yeccgoto_observedEventsDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_419(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 421, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_419_(Stack),
 yeccpars2_420(_S, Cat, [419 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_420(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_420_(Stack),
 yeccgoto_observedEvent(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_421: see yeccpars2_4

yeccpars2_422(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 427, Ss, Stack, T, Ts, Tzr);
yeccpars2_422(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_422_(Stack),
 yeccpars2_426(426, Cat, [422 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_423(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_observedEventParameter(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_424: see yeccpars2_214

yeccpars2_425(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_425_(Stack),
 yeccgoto_eventStreamOrOther(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_426(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 430, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_427: see yeccpars2_4

yeccpars2_428(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 427, Ss, Stack, T, Ts, Tzr);
yeccpars2_428(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_428_(Stack),
 yeccpars2_429(_S, Cat, [428 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_429(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_429_(Stack),
 yeccgoto_observedEventParameters(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_430(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_430_(Stack),
 yeccgoto_observedEventBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_431(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 432, Ss, Stack, T, Ts, Tzr).

yeccpars2_432(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_432(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_432_(Stack),
 yeccpars2_4(433, Cat, [432 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_433: see yeccpars2_4

yeccpars2_434(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 421, Ss, Stack, T, Ts, Tzr);
yeccpars2_434(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_434_(Stack),
 yeccpars2_435(_S, Cat, [434 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_435(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_435_(Stack),
 yeccgoto_observedEvent(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_436(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_436_(Stack),
 yeccgoto_notifyRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_437(S, 'ContextAttrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 439, Ss, Stack, T, Ts, Tzr).

yeccpars2_438(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 452, Ss, Stack, T, Ts, Tzr).

yeccpars2_439(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 440, Ss, Stack, T, Ts, Tzr).

yeccpars2_440(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'EmergencyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 443, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'IEPSToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 444, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'PriorityToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 445, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'TopologyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 446, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_440(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_441(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_441_(Stack),
 yeccgoto_contextAuditProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_442(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 448, Ss, Stack, T, Ts, Tzr);
yeccpars2_442(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_442_(Stack),
 yeccpars2_447(447, Cat, [442 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_443(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_443_(Stack),
 yeccgoto_contextAuditProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_444(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_444_(Stack),
 yeccgoto_contextAuditProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_445(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_445_(Stack),
 yeccgoto_contextAuditProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_446(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_446_(Stack),
 yeccgoto_contextAuditProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_447(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 451, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_448: see yeccpars2_440

yeccpars2_449(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 448, Ss, Stack, T, Ts, Tzr);
yeccpars2_449(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_449_(Stack),
 yeccpars2_450(_S, Cat, [449 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_450(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_450_(Stack),
 yeccgoto_contextAuditProperties(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_451(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_451_(Stack),
 yeccgoto_indAudcontextAttrDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_452(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_452_(Stack),
 yeccgoto_contextAudit(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_453(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(S, 'EmergencyOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 460, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(S, 'EmergencyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 461, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(S, 'IEPSToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 462, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(S, 'PriorityToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 463, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(S, 'TopologyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 464, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_454(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_454_(Stack),
 yeccgoto_contextProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_455(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_455_(Stack),
 yeccgoto_contextProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_456(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_456_(Stack),
 yeccgoto_contextProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_457: see yeccpars2_214

yeccpars2_458(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_contextAttrDescProp(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_459(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 485, Ss, Stack, T, Ts, Tzr);
yeccpars2_459(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_459_(Stack),
 yeccpars2_484(484, Cat, [459 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_460(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_460_(Stack),
 yeccgoto_contextProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_461(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_461_(Stack),
 yeccgoto_contextProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_462(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_462_(Stack),
 yeccgoto_contextProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_463(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 482, Ss, Stack, T, Ts, Tzr).

yeccpars2_464(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 465, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_465: see yeccpars2_4

yeccpars2_466(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 478, Ss, Stack, T, Ts, Tzr);
yeccpars2_466(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_466_(Stack),
 yeccpars2_477(477, Cat, [466 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_467(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_terminationA(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_468(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 469, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_469: see yeccpars2_4

yeccpars2_470(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_terminationB(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_471(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 472, Ss, Stack, T, Ts, Tzr).

yeccpars2_472(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 474, Ss, Stack, T, Ts, Tzr);
yeccpars2_472(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 475, Ss, Stack, T, Ts, Tzr);
yeccpars2_472(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 476, Ss, Stack, T, Ts, Tzr).

yeccpars2_473(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_473_(Stack),
 yeccgoto_topologyTriple(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_474(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_474_(Stack),
 yeccgoto_topologyDirection(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_475(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_475_(Stack),
 yeccgoto_topologyDirection(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_476(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_476_(Stack),
 yeccgoto_topologyDirection(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_477(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 481, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_478: see yeccpars2_4

yeccpars2_479(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 478, Ss, Stack, T, Ts, Tzr);
yeccpars2_479(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_479_(Stack),
 yeccpars2_480(_S, Cat, [479 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_480(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_480_(Stack),
 yeccgoto_topologyTripleList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_481(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_481_(Stack),
 yeccgoto_topologyDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_482: see yeccpars2_4

yeccpars2_483(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_483_(Stack),
 yeccgoto_priority(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_484(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 488, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_485: see yeccpars2_453

yeccpars2_486(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 485, Ss, Stack, T, Ts, Tzr);
yeccpars2_486(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_486_(Stack),
 yeccpars2_487(_S, Cat, [486 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_487(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_487_(Stack),
 yeccgoto_contextAttrDescProps(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_488(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_488_(Stack),
 yeccgoto_contextAttrDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_489(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_489_(Stack),
 yeccgoto_propertyParm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_490: see yeccpars2_4

yeccpars2_491(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_491(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_491_(Stack),
 yeccpars2_492(_S, Cat, [491 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_492(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_492_(Stack),
 yeccgoto_auditRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_493: see yeccpars2_4

yeccpars2_494(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_494(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_494_(Stack),
 yeccpars2_495(_S, Cat, [494 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_495(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_495_(Stack),
 yeccgoto_auditRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_496(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_496_(Stack),
 yeccgoto_actionRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_497(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_497_(Stack),
 yeccgoto_actionRequestBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_498: see yeccpars2_137

yeccpars2_499(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 498, Ss, Stack, T, Ts, Tzr);
yeccpars2_499(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_499_(Stack),
 yeccpars2_500(_S, Cat, [499 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_500(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_500_(Stack),
 yeccgoto_actionRequestItems(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_501: see yeccpars2_4

yeccpars2_502(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 504, Ss, Stack, T, Ts, Tzr);
yeccpars2_502(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_502_(Stack),
 yeccpars2_503(_S, Cat, [502 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_503(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_503_(Stack),
 yeccgoto_ammRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_504(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 166, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 515, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 516, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 517, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 518, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'ModemToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 519, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'MuxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 520, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 521, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 522, Ss, Stack, T, Ts, Tzr).

yeccpars2_505(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_505_(Stack),
 yeccgoto_ammParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_506(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_506_(Stack),
 yeccgoto_ammParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_507(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_507_(Stack),
 yeccgoto_ammParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_508(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_508_(Stack),
 yeccgoto_ammParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_509(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_509_(Stack),
 yeccgoto_ammParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_510(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_510_(Stack),
 yeccgoto_ammParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_511(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_511_(Stack),
 yeccgoto_ammParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_512(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_512_(Stack),
 yeccgoto_ammParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_513(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_513_(Stack),
 yeccgoto_ammParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_514(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 722, Ss, Stack, T, Ts, Tzr);
yeccpars2_514(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_514_(Stack),
 yeccpars2_721(721, Cat, [514 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_515(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_515_(Stack),
 yeccgoto_digitMapDescriptor(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_516(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 713, Ss, Stack, T, Ts, Tzr);
yeccpars2_516(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_516_(Stack),
 yeccgoto_eventBufferDescriptor(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_517(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 653, Ss, Stack, T, Ts, Tzr);
yeccpars2_517(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_517_(Stack),
 yeccgoto_eventsDescriptor(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_518(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 582, Ss, Stack, T, Ts, Tzr).

yeccpars2_519(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 563, Ss, Stack, T, Ts, Tzr);
yeccpars2_519(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 564, Ss, Stack, T, Ts, Tzr).

yeccpars2_520(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 552, Ss, Stack, T, Ts, Tzr).

yeccpars2_521(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 533, Ss, Stack, T, Ts, Tzr);
yeccpars2_521(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_521_(Stack),
 yeccgoto_signalsDescriptor(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_522(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 523, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_523: see yeccpars2_4

yeccpars2_524(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 529, Ss, Stack, T, Ts, Tzr);
yeccpars2_524(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_524_(Stack),
 yeccpars2_528(528, Cat, [524 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_525(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 526, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_525_(Stack),
 yeccgoto_statisticsParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_526: see yeccpars2_254

yeccpars2_527(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_527_(Stack),
 yeccgoto_statisticsParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_528(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 532, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_529: see yeccpars2_4

yeccpars2_530(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 529, Ss, Stack, T, Ts, Tzr);
yeccpars2_530(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_530_(Stack),
 yeccpars2_531(_S, Cat, [530 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_531(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_531_(Stack),
 yeccgoto_statisticsParameters(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_532(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_532_(Stack),
 yeccgoto_statisticsDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_533(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_533(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_533(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_533(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_533(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_533(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_533(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_533(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_533(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_533(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_533(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_533(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_533(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_533(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_533(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_533(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_533(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_533(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_533(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_533(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 537, Ss, Stack, T, Ts, Tzr);
yeccpars2_533(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_533(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_533(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_533(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_534(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_534_(Stack),
 yeccgoto_signalParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_535(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 548, Ss, Stack, T, Ts, Tzr);
yeccpars2_535(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_535_(Stack),
 yeccpars2_547(547, Cat, [535 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_536(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_536_(Stack),
 yeccgoto_signalParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_537(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 538, Ss, Stack, T, Ts, Tzr);
yeccpars2_537(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_537_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_538: see yeccpars2_4

yeccpars2_539(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 540, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_540: see yeccpars2_4

yeccpars2_541(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 543, Ss, Stack, T, Ts, Tzr);
yeccpars2_541(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_541_(Stack),
 yeccpars2_542(542, Cat, [541 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_542(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 546, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_543: see yeccpars2_4

yeccpars2_544(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 543, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_544_(Stack),
 yeccpars2_545(_S, Cat, [544 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_545(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_545_(Stack),
 yeccgoto_signalListParms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_546(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_546_(Stack),
 yeccgoto_signalList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_547(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 551, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_548: see yeccpars2_533

yeccpars2_549(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 548, Ss, Stack, T, Ts, Tzr);
yeccpars2_549(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_549_(Stack),
 yeccpars2_550(_S, Cat, [549 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_550(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_550_(Stack),
 yeccgoto_signalParms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_551(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_551_(Stack),
 yeccgoto_signalsDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_552: see yeccpars2_4

yeccpars2_553(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_553_(Stack),
 yeccgoto_muxType(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_554(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 556, Ss, Stack, T, Ts, Tzr).

yeccpars2_555(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_555_(Stack),
 yeccgoto_muxDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_556: see yeccpars2_4

yeccpars2_557(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 559, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_557_(Stack),
 yeccpars2_558(558, Cat, [557 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_558(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 562, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_559: see yeccpars2_4

yeccpars2_560(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 559, Ss, Stack, T, Ts, Tzr);
yeccpars2_560(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_560_(Stack),
 yeccpars2_561(_S, Cat, [560 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_561(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_561_(Stack),
 yeccgoto_terminationIDListRepeat(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_562(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_562_(Stack),
 yeccgoto_terminationIDList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_563: see yeccpars2_4

%% yeccpars2_564: see yeccpars2_4

yeccpars2_565(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_565_(Stack),
 yeccgoto_modemType(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_566(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 568, Ss, Stack, T, Ts, Tzr);
yeccpars2_566(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_566_(Stack),
 yeccpars2_567(567, Cat, [566 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_567(S, 'RSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 571, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_568: see yeccpars2_4

yeccpars2_569(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 568, Ss, Stack, T, Ts, Tzr);
yeccpars2_569(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_569_(Stack),
 yeccpars2_570(_S, Cat, [569 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_570(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_570_(Stack),
 yeccgoto_modemTypeList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_571(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 573, Ss, Stack, T, Ts, Tzr);
yeccpars2_571(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_571_(Stack),
 yeccpars2_572(_S, Cat, [571 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_572(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_572_(Stack),
 yeccgoto_modemDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_573: see yeccpars2_4

yeccpars2_574(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 576, Ss, Stack, T, Ts, Tzr);
yeccpars2_574(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_574_(Stack),
 yeccpars2_575(575, Cat, [574 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_575(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 579, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_576: see yeccpars2_4

yeccpars2_577(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 576, Ss, Stack, T, Ts, Tzr);
yeccpars2_577(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_577_(Stack),
 yeccpars2_578(_S, Cat, [577 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_578(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_578_(Stack),
 yeccgoto_propertyParms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_579(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_579_(Stack),
 yeccgoto_optPropertyParms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_580(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 573, Ss, Stack, T, Ts, Tzr);
yeccpars2_580(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_580_(Stack),
 yeccpars2_581(_S, Cat, [580 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_581(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_581_(Stack),
 yeccgoto_modemDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_582(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 522, Ss, Stack, T, Ts, Tzr);
yeccpars2_582(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 592, Ss, Stack, T, Ts, Tzr);
yeccpars2_582(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 593, Ss, Stack, T, Ts, Tzr);
yeccpars2_582(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_582(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_cont_582(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 589, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_582(S, 'LocalDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 590, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_582(S, 'RemoteDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 591, Ss, Stack, T, Ts, Tzr).

yeccpars2_583(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_583_(Stack),
 yeccgoto_mediaParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_584(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_584_(Stack),
 yeccgoto_mediaParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_585(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_585_(Stack),
 yeccgoto_mediaParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_586(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_586_(Stack),
 yeccgoto_streamParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_587(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 649, Ss, Stack, T, Ts, Tzr);
yeccpars2_587(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_587_(Stack),
 yeccpars2_648(648, Cat, [587 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_588(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_588_(Stack),
 yeccgoto_streamParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_589(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 624, Ss, Stack, T, Ts, Tzr).

yeccpars2_590(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_590_(Stack),
 yeccgoto_streamParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_591(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_591_(Stack),
 yeccgoto_streamParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_592(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 615, Ss, Stack, T, Ts, Tzr).

yeccpars2_593(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 594, Ss, Stack, T, Ts, Tzr).

yeccpars2_594(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 599, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 600, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_595(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 611, Ss, Stack, T, Ts, Tzr);
yeccpars2_595(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_595_(Stack),
 yeccpars2_610(610, Cat, [595 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_596(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_596_(Stack),
 yeccgoto_terminationStateParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_597(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_597_(Stack),
 yeccgoto_terminationStateParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_598(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_598_(Stack),
 yeccgoto_terminationStateParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_599(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 606, Ss, Stack, T, Ts, Tzr);
yeccpars2_599(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_599_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_600(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 601, Ss, Stack, T, Ts, Tzr);
yeccpars2_600(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_600_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_601(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 603, Ss, Stack, T, Ts, Tzr);
yeccpars2_601(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 604, Ss, Stack, T, Ts, Tzr);
yeccpars2_601(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 605, Ss, Stack, T, Ts, Tzr).

yeccpars2_602(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_602_(Stack),
 yeccgoto_serviceStates(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_603(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_603_(Stack),
 yeccgoto_serviceState(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_604(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_604_(Stack),
 yeccgoto_serviceState(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_605(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_605_(Stack),
 yeccgoto_serviceState(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_606(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 608, Ss, Stack, T, Ts, Tzr);
yeccpars2_606(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 609, Ss, Stack, T, Ts, Tzr).

yeccpars2_607(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_607_(Stack),
 yeccgoto_eventBufferControl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_608(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_608_(Stack),
 yeccgoto_eventBufferControlState(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_609(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_609_(Stack),
 yeccgoto_eventBufferControlState(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_610(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 614, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_611: see yeccpars2_594

yeccpars2_612(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 611, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_612_(Stack),
 yeccpars2_613(_S, Cat, [612 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_613(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_613_(Stack),
 yeccgoto_terminationStateParms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_614(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_614_(Stack),
 yeccgoto_terminationStateDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_615: see yeccpars2_4

yeccpars2_616(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 617, Ss, Stack, T, Ts, Tzr).

yeccpars2_617(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 522, Ss, Stack, T, Ts, Tzr);
yeccpars2_617(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_582(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_618(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 620, Ss, Stack, T, Ts, Tzr);
yeccpars2_618(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_618_(Stack),
 yeccpars2_619(619, Cat, [618 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_619(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 623, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_620: see yeccpars2_617

yeccpars2_621(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 620, Ss, Stack, T, Ts, Tzr);
yeccpars2_621(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_621_(Stack),
 yeccpars2_622(_S, Cat, [621 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_622(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_622_(Stack),
 yeccgoto_streamParmList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_623(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_623_(Stack),
 yeccgoto_streamDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_624(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_624(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_624(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_624(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_624(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_624(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_624(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_624(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_624(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_624(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_624(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 627, Ss, Stack, T, Ts, Tzr);
yeccpars2_624(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_624(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_624(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_624(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_624(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 628, Ss, Stack, T, Ts, Tzr);
yeccpars2_624(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 629, Ss, Stack, T, Ts, Tzr);
yeccpars2_624(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_624(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_624(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_624(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_624(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_624(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_624(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_625(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_625_(Stack),
 yeccgoto_localParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_626(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 644, Ss, Stack, T, Ts, Tzr);
yeccpars2_626(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_626_(Stack),
 yeccpars2_643(643, Cat, [626 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_627(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 636, Ss, Stack, T, Ts, Tzr);
yeccpars2_627(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_627_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_628(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 634, Ss, Stack, T, Ts, Tzr);
yeccpars2_628(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_628_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_629(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 630, Ss, Stack, T, Ts, Tzr);
yeccpars2_629(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_629_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_630(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 632, Ss, Stack, T, Ts, Tzr);
yeccpars2_630(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 633, Ss, Stack, T, Ts, Tzr).

yeccpars2_631(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_631_(Stack),
 yeccgoto_localParm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_632(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_632_(Stack),
 yeccgoto_onOrOff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_633(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_633_(Stack),
 yeccgoto_onOrOff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_634: see yeccpars2_630

yeccpars2_635(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_635_(Stack),
 yeccgoto_localParm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_636(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 638, Ss, Stack, T, Ts, Tzr);
yeccpars2_636(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 639, Ss, Stack, T, Ts, Tzr);
yeccpars2_636(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 640, Ss, Stack, T, Ts, Tzr);
yeccpars2_636(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 641, Ss, Stack, T, Ts, Tzr);
yeccpars2_636(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 642, Ss, Stack, T, Ts, Tzr).

yeccpars2_637(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_637_(Stack),
 yeccgoto_localParm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_638(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_638_(Stack),
 yeccgoto_streamModes(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_639(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_639_(Stack),
 yeccgoto_streamModes(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_640(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_640_(Stack),
 yeccgoto_streamModes(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_641(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_641_(Stack),
 yeccgoto_streamModes(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_642(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_642_(Stack),
 yeccgoto_streamModes(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_643(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 647, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_644: see yeccpars2_624

yeccpars2_645(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 644, Ss, Stack, T, Ts, Tzr);
yeccpars2_645(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_645_(Stack),
 yeccpars2_646(_S, Cat, [645 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_646(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_646_(Stack),
 yeccgoto_localParmList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_647(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_647_(Stack),
 yeccgoto_localControlDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_648(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 652, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_649: see yeccpars2_582

yeccpars2_650(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 649, Ss, Stack, T, Ts, Tzr);
yeccpars2_650(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_650_(Stack),
 yeccpars2_651(_S, Cat, [650 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_651(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_651_(Stack),
 yeccgoto_mediaParmList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_652(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_652_(Stack),
 yeccgoto_mediaDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_653: see yeccpars2_4

yeccpars2_654(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 655, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_655: see yeccpars2_4

yeccpars2_656(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 709, Ss, Stack, T, Ts, Tzr);
yeccpars2_656(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_656_(Stack),
 yeccpars2_708(708, Cat, [656 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_657(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 659, Ss, Stack, T, Ts, Tzr);
yeccpars2_657(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_657_(Stack),
 yeccpars2_658(_S, Cat, [657 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_658(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_658_(Stack),
 yeccgoto_requestedEvent(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_659(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 665, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 666, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 667, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_660(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_eventParameter(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_661(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 704, Ss, Stack, T, Ts, Tzr);
yeccpars2_661(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_661_(Stack),
 yeccpars2_703(703, Cat, [661 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_662(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_eventParameter(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_663(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_eventParameter(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_664(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_eventParameter(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_665(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_665_(Stack),
 yeccgoto_eventDM(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_666(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 668, Ss, Stack, T, Ts, Tzr);
yeccpars2_666(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_666_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_667(_S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_667_COMMA(Stack),
 yeccgoto_eventParameter(hd(Ss), 'COMMA', Ss, NewStack, T, Ts, Tzr);
yeccpars2_667(_S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_667_RBRKT(Stack),
 yeccgoto_eventParameter(hd(Ss), 'RBRKT', Ss, NewStack, T, Ts, Tzr);
yeccpars2_667(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_667_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_668(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 671, Ss, Stack, T, Ts, Tzr);
yeccpars2_668(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 521, Ss, Stack, T, Ts, Tzr).

yeccpars2_669(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 699, Ss, Stack, T, Ts, Tzr);
yeccpars2_669(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 700, Ss, Stack, T, Ts, Tzr).

yeccpars2_670(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 698, Ss, Stack, T, Ts, Tzr).

yeccpars2_671(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 672, Ss, Stack, T, Ts, Tzr);
yeccpars2_671(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_671_(Stack),
 yeccgoto_embedFirst(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_672: see yeccpars2_4

yeccpars2_673(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 674, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_674: see yeccpars2_4

yeccpars2_675(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 694, Ss, Stack, T, Ts, Tzr);
yeccpars2_675(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_675_(Stack),
 yeccpars2_693(693, Cat, [675 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_676(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 678, Ss, Stack, T, Ts, Tzr);
yeccpars2_676(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_676_(Stack),
 yeccpars2_677(_S, Cat, [676 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_677(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_677_(Stack),
 yeccgoto_secondRequestedEvent(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_678(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_678(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_678(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_678(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 665, Ss, Stack, T, Ts, Tzr);
yeccpars2_678(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_678(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_678(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 683, Ss, Stack, T, Ts, Tzr);
yeccpars2_678(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_678(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 684, Ss, Stack, T, Ts, Tzr);
yeccpars2_678(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_678(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_678(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_678(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_678(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_678(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_678(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_678(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_678(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_678(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_678(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_678(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_678(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_678(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_678(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_678(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_679(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 689, Ss, Stack, T, Ts, Tzr);
yeccpars2_679(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_679_(Stack),
 yeccpars2_688(688, Cat, [679 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_680(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondEventParameter(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_681(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondEventParameter(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_682(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondEventParameter(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_683(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 685, Ss, Stack, T, Ts, Tzr);
yeccpars2_683(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_683_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_684(_S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_684_COMMA(Stack),
 yeccgoto_secondEventParameter(hd(Ss), 'COMMA', Ss, NewStack, T, Ts, Tzr);
yeccpars2_684(_S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_684_RBRKT(Stack),
 yeccgoto_secondEventParameter(hd(Ss), 'RBRKT', Ss, NewStack, T, Ts, Tzr);
yeccpars2_684(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_684_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_685(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 521, Ss, Stack, T, Ts, Tzr).

yeccpars2_686(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 687, Ss, Stack, T, Ts, Tzr).

yeccpars2_687(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_687_(Stack),
 yeccgoto_embedSig(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_688(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 692, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_689: see yeccpars2_678

yeccpars2_690(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 689, Ss, Stack, T, Ts, Tzr);
yeccpars2_690(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_690_(Stack),
 yeccpars2_691(_S, Cat, [690 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_691(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_691_(Stack),
 yeccgoto_secondEventParameters(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_692(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_692_(Stack),
 yeccgoto_secondRequestedEventBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_693(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 697, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_694: see yeccpars2_4

yeccpars2_695(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 694, Ss, Stack, T, Ts, Tzr);
yeccpars2_695(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_695_(Stack),
 yeccpars2_696(_S, Cat, [695 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_696(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_696_(Stack),
 yeccgoto_secondRequestedEvents(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_697(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_697_(Stack),
 yeccgoto_embedFirst(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_698(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_698_(Stack),
 yeccgoto_embedNoSig(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_699(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 671, Ss, Stack, T, Ts, Tzr).

yeccpars2_700(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_700_(Stack),
 yeccgoto_embedWithSig(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_701(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 702, Ss, Stack, T, Ts, Tzr).

yeccpars2_702(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_702_(Stack),
 yeccgoto_embedWithSig(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_703(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 707, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_704: see yeccpars2_659

yeccpars2_705(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 704, Ss, Stack, T, Ts, Tzr);
yeccpars2_705(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_705_(Stack),
 yeccpars2_706(_S, Cat, [705 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_706(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_706_(Stack),
 yeccgoto_eventParameters(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_707(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_707_(Stack),
 yeccgoto_requestedEventBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_708(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 712, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_709: see yeccpars2_4

yeccpars2_710(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 709, Ss, Stack, T, Ts, Tzr);
yeccpars2_710(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_710_(Stack),
 yeccpars2_711(_S, Cat, [710 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_711(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_711_(Stack),
 yeccgoto_requestedEvents(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_712(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_712_(Stack),
 yeccgoto_eventsDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_713(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 377, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_713_(Stack),
 yeccpars2_4(412, Cat, [713 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_714(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_714_(Stack),
 yeccgoto_eventSpec(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_715(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 717, Ss, Stack, T, Ts, Tzr);
yeccpars2_715(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_715_(Stack),
 yeccpars2_716(716, Cat, [715 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_716(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 720, Ss, Stack, T, Ts, Tzr).

yeccpars2_717(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_717(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 377, Ss, Stack, T, Ts, Tzr);
yeccpars2_717(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_717_(Stack),
 yeccpars2_4(412, Cat, [717 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_718(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 717, Ss, Stack, T, Ts, Tzr);
yeccpars2_718(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_718_(Stack),
 yeccpars2_719(_S, Cat, [718 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_719(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_719_(Stack),
 yeccgoto_eventSpecList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_720(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_720_(Stack),
 yeccgoto_eventBufferDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_721(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 725, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_722: see yeccpars2_504

yeccpars2_723(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 722, Ss, Stack, T, Ts, Tzr);
yeccpars2_723(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_723_(Stack),
 yeccpars2_724(_S, Cat, [723 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_724(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_724_(Stack),
 yeccgoto_ammParameters(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_725(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_725_(Stack),
 yeccgoto_ammRequestBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_726(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 730, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_727: see yeccpars2_131

yeccpars2_728(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 727, Ss, Stack, T, Ts, Tzr);
yeccpars2_728(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_728_(Stack),
 yeccpars2_729(_S, Cat, [728 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_729(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_729_(Stack),
 yeccgoto_actionRequestList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_730(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_730_(Stack),
 yeccgoto_transactionRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_731(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 737, Ss, Stack, T, Ts, Tzr).

yeccpars2_732(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_732_(Stack),
 yeccgoto_transactionID(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_733: see yeccpars2_131

yeccpars2_734(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 727, Ss, Stack, T, Ts, Tzr);
yeccpars2_734(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_734_(Stack),
 yeccpars2_735(735, Cat, [734 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_735(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 736, Ss, Stack, T, Ts, Tzr).

yeccpars2_736(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_736_(Stack),
 yeccgoto_transactionRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_737: see yeccpars2_131

yeccpars2_738(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 727, Ss, Stack, T, Ts, Tzr);
yeccpars2_738(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_738_(Stack),
 yeccpars2_739(739, Cat, [738 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_739(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 740, Ss, Stack, T, Ts, Tzr).

yeccpars2_740(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_740_(Stack),
 yeccgoto_transactionRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_741: see yeccpars2_4

yeccpars2_742(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 745, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_742_(Stack),
 yeccpars2_744(744, Cat, [742 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_743(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_743_(Stack),
 yeccgoto_transactionAck(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_744(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 748, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_745: see yeccpars2_4

yeccpars2_746(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 745, Ss, Stack, T, Ts, Tzr);
yeccpars2_746(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_746_(Stack),
 yeccpars2_747(_S, Cat, [746 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_747(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_747_(Stack),
 yeccgoto_transactionAckList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_748(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_748_(Stack),
 yeccgoto_transactionResponseAck(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_749: see yeccpars2_4

yeccpars2_750(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 751, Ss, Stack, T, Ts, Tzr).

yeccpars2_751(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 753, Ss, Stack, T, Ts, Tzr);
yeccpars2_751(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_751_(Stack),
 yeccpars2_752(752, Cat, [751 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_752(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 758, Ss, Stack, T, Ts, Tzr);
yeccpars2_752(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr).

yeccpars2_753(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 754, Ss, Stack, T, Ts, Tzr).

yeccpars2_754(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_754_(Stack),
 yeccgoto_optImmAckRequired(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_755(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 880, Ss, Stack, T, Ts, Tzr).

yeccpars2_756(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_756_(Stack),
 yeccgoto_transactionReplyBody(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_757(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 877, Ss, Stack, T, Ts, Tzr);
yeccpars2_757(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_757_(Stack),
 yeccpars2_876(_S, Cat, [757 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_758(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 759, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_759: see yeccpars2_4

yeccpars2_760(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 761, Ss, Stack, T, Ts, Tzr).

yeccpars2_761(S, 'AddToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 771, Ss, Stack, T, Ts, Tzr);
yeccpars2_761(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 772, Ss, Stack, T, Ts, Tzr);
yeccpars2_761(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 773, Ss, Stack, T, Ts, Tzr);
yeccpars2_761(S, 'ContextAttrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 153, Ss, Stack, T, Ts, Tzr);
yeccpars2_761(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_761(S, 'ModifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 774, Ss, Stack, T, Ts, Tzr);
yeccpars2_761(S, 'MoveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 775, Ss, Stack, T, Ts, Tzr);
yeccpars2_761(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 776, Ss, Stack, T, Ts, Tzr);
yeccpars2_761(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 777, Ss, Stack, T, Ts, Tzr);
yeccpars2_761(S, 'SubtractToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 778, Ss, Stack, T, Ts, Tzr).

yeccpars2_762(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_762_(Stack),
 yeccgoto_commandReplys(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_763(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_763_(Stack),
 yeccgoto_commandReplys(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_764(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_764_(Stack),
 yeccgoto_actionReplyBody(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_765(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_765_(Stack),
 yeccgoto_commandReplys(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_766(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 872, Ss, Stack, T, Ts, Tzr);
yeccpars2_766(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_766_(Stack),
 yeccpars2_871(_S, Cat, [766 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_767(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_767_(Stack),
 yeccgoto_commandReplys(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_768(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 865, Ss, Stack, T, Ts, Tzr).

yeccpars2_769(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_769_(Stack),
 yeccgoto_commandReplys(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_770(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 864, Ss, Stack, T, Ts, Tzr).

yeccpars2_771(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_771_(Stack),
 yeccgoto_ammsToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_772(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 860, Ss, Stack, T, Ts, Tzr).

yeccpars2_773(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 810, Ss, Stack, T, Ts, Tzr).

yeccpars2_774(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_774_(Stack),
 yeccgoto_ammsToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_775(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_775_(Stack),
 yeccgoto_ammsToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_776(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 804, Ss, Stack, T, Ts, Tzr).

yeccpars2_777(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 779, Ss, Stack, T, Ts, Tzr).

yeccpars2_778(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_778_(Stack),
 yeccgoto_ammsToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_779: see yeccpars2_4

yeccpars2_780(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 782, Ss, Stack, T, Ts, Tzr);
yeccpars2_780(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_780_(Stack),
 yeccpars2_781(_S, Cat, [780 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_781(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_781_(Stack),
 yeccgoto_serviceChangeReply(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_782(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_782(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 785, Ss, Stack, T, Ts, Tzr).

yeccpars2_783(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 803, Ss, Stack, T, Ts, Tzr).

yeccpars2_784(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 802, Ss, Stack, T, Ts, Tzr).

yeccpars2_785(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 786, Ss, Stack, T, Ts, Tzr).

yeccpars2_786(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 793, Ss, Stack, T, Ts, Tzr);
yeccpars2_786(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 794, Ss, Stack, T, Ts, Tzr);
yeccpars2_786(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 795, Ss, Stack, T, Ts, Tzr);
yeccpars2_786(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 377, Ss, Stack, T, Ts, Tzr);
yeccpars2_786(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 796, Ss, Stack, T, Ts, Tzr).

yeccpars2_787(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_787_(Stack),
 yeccgoto_servChgReplyParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_788(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_788_(Stack),
 yeccgoto_servChgReplyParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_789(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_789_(Stack),
 yeccgoto_servChgReplyParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_790(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_790_(Stack),
 yeccgoto_servChgReplyParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_791(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_791_(Stack),
 yeccgoto_servChgReplyParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_792(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 798, Ss, Stack, T, Ts, Tzr);
yeccpars2_792(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_792_(Stack),
 yeccpars2_797(797, Cat, [792 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_793(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 388, Ss, Stack, T, Ts, Tzr).

yeccpars2_794(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 386, Ss, Stack, T, Ts, Tzr).

yeccpars2_795(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 381, Ss, Stack, T, Ts, Tzr).

yeccpars2_796(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 379, Ss, Stack, T, Ts, Tzr).

yeccpars2_797(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 801, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_798: see yeccpars2_786

yeccpars2_799(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 798, Ss, Stack, T, Ts, Tzr);
yeccpars2_799(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_799_(Stack),
 yeccpars2_800(_S, Cat, [799 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_800(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_800_(Stack),
 yeccgoto_servChgReplyParms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_801(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_801_(Stack),
 yeccgoto_serviceChangeReplyDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_802(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_802_(Stack),
 yeccgoto_serviceChangeReplyBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_803(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_803_(Stack),
 yeccgoto_serviceChangeReplyBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_804: see yeccpars2_4

yeccpars2_805(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 807, Ss, Stack, T, Ts, Tzr);
yeccpars2_805(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_805_(Stack),
 yeccpars2_806(_S, Cat, [805 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_806(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_806_(Stack),
 yeccgoto_notifyReply(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_807(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr).

yeccpars2_808(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 809, Ss, Stack, T, Ts, Tzr).

yeccpars2_809(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_809_(Stack),
 yeccgoto_notifyReplyBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_810(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_810(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 813, Ss, Stack, T, Ts, Tzr);
yeccpars2_810(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_810(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_810(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_810(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_810(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_810(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_810(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_810(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_810(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_810(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_810(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_810(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_810(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_810(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_810(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_810(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_810(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_810(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_810(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_810(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_810(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_810(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_811(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 827, Ss, Stack, T, Ts, Tzr);
yeccpars2_811(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_811_(Stack),
 yeccgoto_auditOther(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_812(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_812_(Stack),
 yeccgoto_auditReply(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_813(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 816, Ss, Stack, T, Ts, Tzr);
yeccpars2_813(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_813_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_814(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_814_(Stack),
 yeccgoto_contextTerminationAudit(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_815(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_815_(Stack),
 yeccgoto_auditReply(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_816(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_816(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_816(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_816(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_816(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_816(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_816(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 818, Ss, Stack, T, Ts, Tzr);
yeccpars2_816(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_816(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_816(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_816(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_816(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_816(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_816(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_816(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_816(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_816(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_816(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_816(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_816(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_816(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_816(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_816(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_816(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_817(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 826, Ss, Stack, T, Ts, Tzr).

yeccpars2_818(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 819, Ss, Stack, T, Ts, Tzr);
yeccpars2_818(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_818_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_819: see yeccpars2_4

yeccpars2_820(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_820_(Stack),
 yeccgoto_errorCode(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_821(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 822, Ss, Stack, T, Ts, Tzr).

yeccpars2_822(S, 'QuotedChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 824, Ss, Stack, T, Ts, Tzr);
yeccpars2_822(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_822_(Stack),
 yeccpars2_823(823, Cat, [822 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_823(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 825, Ss, Stack, T, Ts, Tzr).

yeccpars2_824(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_824_(Stack),
 yeccgoto_errorText(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_825(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_825_(Stack),
 yeccgoto_errorDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_826(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_826_(Stack),
 yeccgoto_contextTerminationAudit(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_827(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 515, Ss, Stack, T, Ts, Tzr);
yeccpars2_827(S, 'DigitMapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_827(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_827(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 516, Ss, Stack, T, Ts, Tzr);
yeccpars2_827(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 517, Ss, Stack, T, Ts, Tzr);
yeccpars2_827(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 842, Ss, Stack, T, Ts, Tzr);
yeccpars2_827(S, 'ModemToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 843, Ss, Stack, T, Ts, Tzr);
yeccpars2_827(S, 'MuxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 844, Ss, Stack, T, Ts, Tzr);
yeccpars2_827(S, 'ObservedEventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 845, Ss, Stack, T, Ts, Tzr);
yeccpars2_827(S, 'PackagesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 846, Ss, Stack, T, Ts, Tzr);
yeccpars2_827(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 521, Ss, Stack, T, Ts, Tzr);
yeccpars2_827(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 847, Ss, Stack, T, Ts, Tzr).

yeccpars2_828(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 859, Ss, Stack, T, Ts, Tzr).

yeccpars2_829(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_829_(Stack),
 yeccgoto_auditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_830(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_830_(Stack),
 yeccgoto_auditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_831(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_831_(Stack),
 yeccgoto_auditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_832(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_832_(Stack),
 yeccgoto_auditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_833(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_833_(Stack),
 yeccgoto_auditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_834(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_834_(Stack),
 yeccgoto_auditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_835(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_835_(Stack),
 yeccgoto_auditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_836(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_836_(Stack),
 yeccgoto_auditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_837(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_837_(Stack),
 yeccgoto_auditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_838(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_838_(Stack),
 yeccgoto_auditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_839(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_839_(Stack),
 yeccgoto_auditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_840(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 856, Ss, Stack, T, Ts, Tzr);
yeccpars2_840(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_840_(Stack),
 yeccpars2_855(_S, Cat, [840 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_841(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_841_(Stack),
 yeccgoto_auditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_842(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 582, Ss, Stack, T, Ts, Tzr);
yeccpars2_842(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_842_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_843(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 563, Ss, Stack, T, Ts, Tzr);
yeccpars2_843(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 564, Ss, Stack, T, Ts, Tzr);
yeccpars2_843(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_843_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_844(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 552, Ss, Stack, T, Ts, Tzr);
yeccpars2_844(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_844_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_845(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 408, Ss, Stack, T, Ts, Tzr);
yeccpars2_845(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_845_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_846(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 848, Ss, Stack, T, Ts, Tzr);
yeccpars2_846(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_846_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_847(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 523, Ss, Stack, T, Ts, Tzr);
yeccpars2_847(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_847_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_848: see yeccpars2_4

yeccpars2_849(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 851, Ss, Stack, T, Ts, Tzr);
yeccpars2_849(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_849_(Stack),
 yeccpars2_850(850, Cat, [849 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_850(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 854, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_851: see yeccpars2_4

yeccpars2_852(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 851, Ss, Stack, T, Ts, Tzr);
yeccpars2_852(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_852_(Stack),
 yeccpars2_853(_S, Cat, [852 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_853(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_853_(Stack),
 yeccgoto_packagesItems(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_854(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_854_(Stack),
 yeccgoto_packagesDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_855(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_855_(Stack),
 yeccgoto_terminationAudit(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_856: see yeccpars2_827

yeccpars2_857(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 856, Ss, Stack, T, Ts, Tzr);
yeccpars2_857(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_857_(Stack),
 yeccpars2_858(_S, Cat, [857 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_858(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_858_(Stack),
 yeccgoto_auditReturnParameterList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_859(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_859_(Stack),
 yeccgoto_auditOther(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_860(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_860(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 862, Ss, Stack, T, Ts, Tzr);
yeccpars2_860(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_860(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_860(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_860(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_860(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_860(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_860(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_860(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_860(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_860(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_860(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_860(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_860(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_860(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_860(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_860(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_860(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_860(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_860(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_860(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_860(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_860(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_861(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_861_(Stack),
 yeccgoto_auditReply(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_862(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 816, Ss, Stack, T, Ts, Tzr);
yeccpars2_862(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_862_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_863(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_863_(Stack),
 yeccgoto_auditReply(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_864(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_864_(Stack),
 yeccgoto_actionReply(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_865: see yeccpars2_4

yeccpars2_866(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 868, Ss, Stack, T, Ts, Tzr);
yeccpars2_866(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_866_(Stack),
 yeccpars2_867(_S, Cat, [866 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_867(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_867_(Stack),
 yeccgoto_ammsReply(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_868: see yeccpars2_827

yeccpars2_869(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 870, Ss, Stack, T, Ts, Tzr).

yeccpars2_870(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_870_(Stack),
 yeccgoto_ammsReplyBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_871(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_871_(Stack),
 yeccgoto_actionReplyBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_872: see yeccpars2_761

yeccpars2_873(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_873_(Stack),
 yeccgoto_commandReplyList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_874(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 872, Ss, Stack, T, Ts, Tzr);
yeccpars2_874(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_874_(Stack),
 yeccpars2_875(_S, Cat, [874 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_875(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_875_(Stack),
 yeccgoto_commandReplyList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_876(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_876_(Stack),
 yeccgoto_transactionReplyBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_877(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 758, Ss, Stack, T, Ts, Tzr).

yeccpars2_878(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 877, Ss, Stack, T, Ts, Tzr);
yeccpars2_878(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_878_(Stack),
 yeccpars2_879(_S, Cat, [878 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_879(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_879_(Stack),
 yeccgoto_actionReplyList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_880(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_880_(Stack),
 yeccgoto_transactionReply(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_881: see yeccpars2_4

yeccpars2_882(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 883, Ss, Stack, T, Ts, Tzr).

yeccpars2_883(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 884, Ss, Stack, T, Ts, Tzr).

yeccpars2_884(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_884_(Stack),
 yeccgoto_transactionPending(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_885(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_885_(Stack),
 yeccgoto_transactionList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_886(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_886_(Stack),
 yeccgoto_pathName(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_887(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_887_(Stack),
 yeccgoto_deviceName(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_888(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_888(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_888_(Stack),
 yeccpars2_892(_S, Cat, [888 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_889(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_889(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_889_(Stack),
 yeccpars2_891(_S, Cat, [889 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_890(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_890_(Stack),
 yeccgoto_mtpAddress(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_891(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_891_(Stack),
 yeccgoto_mId(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_892(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_892_(Stack),
 yeccgoto_mId(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccgoto_actionReply(752, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_757(757, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actionReply(877, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_878(878, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_actionReplyBody(761, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_770(770, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_actionReplyList(757=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_876(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actionReplyList(878=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_879(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_actionRequest(131, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(132, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actionRequest(727, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_728(728, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actionRequest(733, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_734(734, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actionRequest(737, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_738(738, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_actionRequestBody(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(149, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_actionRequestItem(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(148, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actionRequestItem(498, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_499(499, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_actionRequestItems(148=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_497(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actionRequestItems(499=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_500(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_actionRequestList(132, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_726(726, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actionRequestList(728=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_729(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actionRequestList(734, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_735(735, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actionRequestList(738, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_739(739, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_alternativeValue(253=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_263(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ammParameter(504, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_514(514, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ammParameter(722, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_723(723, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ammParameters(514, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_721(721, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ammParameters(723=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_724(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ammRequest(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ammRequest(498=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ammRequestBody(502=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_503(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ammToken(137, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(146, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ammToken(498, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(146, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ammsReply(761=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_769(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ammsReply(872=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_769(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ammsReplyBody(866=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_867(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ammsToken(761, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_768(768, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ammsToken(872, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_768(768, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_auditDescriptor(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(165, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditDescriptor(504=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_513(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditDescriptor(722=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_513(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_auditDescriptorBody(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(179, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_auditItem(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(178, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditItem(338, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_339(339, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditItem(356=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_369(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditItem(396=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_369(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_auditItemList(178=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_337(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditItemList(339=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_340(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_auditOther(810=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_812(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditOther(860=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_861(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_auditReply(761=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_767(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditReply(872=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_767(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_auditRequest(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditRequest(498=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_auditReturnItem(167=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditReturnItem(338=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditReturnItem(356=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditReturnItem(396=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditReturnItem(827=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_841(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditReturnItem(856=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_841(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditReturnItem(868=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_841(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_auditReturnParameter(827, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_840(840, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditReturnParameter(856, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_857(857, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditReturnParameter(868, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_840(840, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_auditReturnParameterList(840=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_855(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditReturnParameterList(857=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_858(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_authenticationHeader(1, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_commandReplyList(766=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_871(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_commandReplyList(874=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_875(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_commandReplys(761, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_766(766, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_commandReplys(872, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_874(874, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_commandRequest(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_commandRequest(498=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextAttrDescProp(453, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_459(459, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAttrDescProp(485, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_486(486, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextAttrDescProps(459, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_484(484, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAttrDescProps(486=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_487(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextAttrDescriptor(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAttrDescriptor(498=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAttrDescriptor(761=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAttrDescriptor(872=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextAudit(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAudit(498=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextAuditProperties(442, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_447(447, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAuditProperties(449=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_450(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextAuditProperty(440, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_442(442, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAuditProperty(448, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_449(449, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextID(134, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(136, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextID(759, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_760(760, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextProperties(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextProperties(498=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextProperties(761=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_765(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextProperties(872=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_765(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextProperty(453=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_458(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextProperty(485=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_458(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextTerminationAudit(813=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_815(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextTerminationAudit(862=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_863(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_daddr(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(103, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_daddr(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_daddr(104=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_deviceName(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_889(889, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_digitMapDescriptor(504=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_512(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_digitMapDescriptor(722=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_512(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_digitMapDescriptor(827=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_839(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_digitMapDescriptor(856=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_839(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_digitMapDescriptor(868=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_839(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_direction(247=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_248(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_domainAddress(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_domainAddress(381=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_domainAddress(388=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_domainName(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_domainName(381=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_domainName(388=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_embedFirst(668, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_670(670, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_embedFirst(699, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_701(701, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_embedNoSig(659=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_664(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_embedNoSig(704=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_664(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_embedSig(678=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_682(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_embedSig(689=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_682(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_embedWithSig(659=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_663(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_embedWithSig(704=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_663(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_errorCode(819, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_821(821, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_errorDescriptor(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_errorDescriptor(403=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_406(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_errorDescriptor(752=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_756(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_errorDescriptor(761=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_764(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_errorDescriptor(782, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_784(784, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_errorDescriptor(807, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_808(808, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_errorDescriptor(816, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_817(817, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_errorDescriptor(827=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_838(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_errorDescriptor(856=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_838(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_errorDescriptor(868=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_838(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_errorDescriptor(872=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_873(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_errorText(822, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_823(823, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventBufferControl(594=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_598(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventBufferControl(611=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_598(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventBufferControlState(606=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_607(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventBufferDescriptor(504=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_511(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventBufferDescriptor(722=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_511(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventBufferDescriptor(827=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_837(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventBufferDescriptor(856=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_837(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventBufferDescriptor(868=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_837(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventDM(659=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_662(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventDM(678=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_681(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventDM(689=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_681(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventDM(704=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_662(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventParameter(659, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_661(661, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventParameter(704, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_705(705, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventParameterName(327=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_331(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventParameterName(421, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(424, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventParameterName(427, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(424, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventParameterName(659, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(424, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventParameterName(678, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(424, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventParameterName(689, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(424, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventParameterName(704, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(424, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventParameters(661, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_703(703, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventParameters(705=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_706(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventSpec(713, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_715(715, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventSpec(717, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_718(718, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventSpecList(715, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_716(716, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventSpecList(718=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_719(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventStream(327=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_330(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventStreamOrOther(421=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_423(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventStreamOrOther(427=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_423(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventStreamOrOther(659=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_660(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventStreamOrOther(678=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_680(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventStreamOrOther(689=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_680(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventStreamOrOther(704=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_660(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventsDescriptor(504=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_510(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventsDescriptor(722=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_510(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventsDescriptor(827=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_836(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventsDescriptor(856=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_836(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventsDescriptor(868=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_836(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_extension(356=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_368(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_extension(396=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_368(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_extensionParameter(356, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(367, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_extensionParameter(396, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(367, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudauditReturnParameter(167, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(176, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudauditReturnParameter(338, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(176, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudauditReturnParameter(342, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(343, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudauditReturnParameter(356, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(176, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudauditReturnParameter(396, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(176, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudcontextAttrDescriptor(437, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_438(438, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAuddigitMapDescriptor(167=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAuddigitMapDescriptor(338=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAuddigitMapDescriptor(342=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAuddigitMapDescriptor(356=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAuddigitMapDescriptor(396=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudeventBufferDescriptor(167=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudeventBufferDescriptor(338=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudeventBufferDescriptor(342=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudeventBufferDescriptor(356=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudeventBufferDescriptor(396=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudeventSpec(322, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_324(324, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudeventSpecParameter(327, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_329(329, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudeventsDescriptor(167=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudeventsDescriptor(338=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudeventsDescriptor(342=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudeventsDescriptor(356=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudeventsDescriptor(396=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudlocalControlDescriptor(287=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_293(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudlocalControlDescriptor(304=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_293(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudlocalParm(307, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_309(309, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudlocalParm(311, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_312(312, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudlocalParmList(309, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_310(310, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudlocalParmList(312=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_313(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudmediaDescriptor(167=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudmediaDescriptor(338=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudmediaDescriptor(342=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudmediaDescriptor(356=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudmediaDescriptor(396=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudmediaParm(287, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_292(292, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudpackagesDescriptor(167=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudpackagesDescriptor(338=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudpackagesDescriptor(342=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudpackagesDescriptor(356=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudpackagesDescriptor(396=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudrequestedEvent(318, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_320(320, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudsignalList(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudsignalParm(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(200, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudsignalsDescriptor(167=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudsignalsDescriptor(338=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudsignalsDescriptor(342=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudsignalsDescriptor(356=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudsignalsDescriptor(396=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudstatisticsDescriptor(167=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudstatisticsDescriptor(287=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_291(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudstatisticsDescriptor(304=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_291(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudstatisticsDescriptor(338=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudstatisticsDescriptor(342=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudstatisticsDescriptor(356=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudstatisticsDescriptor(396=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudstreamDescriptor(287=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_290(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudstreamParm(287=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_289(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudstreamParm(304, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_305(305, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudterminationAudit(167=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudterminationAudit(338=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudterminationAudit(356=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudterminationAudit(396=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudterminationAuditList(176=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudterminationAuditList(343=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudterminationStateDescriptor(287=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_288(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudterminationStateParm(298, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_300(300, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_localControlDescriptor(582=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_588(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_localControlDescriptor(617=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_588(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_localControlDescriptor(620=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_588(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_localControlDescriptor(649=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_588(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_localParm(624, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_626(626, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_localParm(644, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_645(645, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_localParmList(626, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_643(643, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_localParmList(645=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_646(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mId(93, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(97, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mId(381=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_383(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mId(388=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_389(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mediaDescriptor(504=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_509(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mediaDescriptor(722=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_509(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mediaDescriptor(827=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_835(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mediaDescriptor(856=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_835(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mediaDescriptor(868=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_835(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mediaParm(582, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_587(587, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mediaParm(649, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_650(650, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mediaParmList(587, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_648(648, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mediaParmList(650=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_651(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_megacoMessage(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_message(4, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(94, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_messageBody(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_modemDescriptor(504=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_508(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_modemDescriptor(722=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_508(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_modemDescriptor(827=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_834(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_modemDescriptor(856=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_834(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_modemDescriptor(868=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_834(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_modemType(563, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_580(580, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_modemType(564, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_566(566, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_modemType(568, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_569(569, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_modemTypeList(566, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_567(567, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_modemTypeList(569=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_570(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mtpAddress(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_888(888, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_muxDescriptor(504=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_507(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_muxDescriptor(722=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_507(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_muxDescriptor(827=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_833(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_muxDescriptor(856=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_833(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_muxDescriptor(868=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_833(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_muxType(552, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_554(554, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_notificationReason(234, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_235(235, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_notificationReason(241, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_242(242, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_notificationReasons(235, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_240(240, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_notificationReasons(242=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_notifyReply(761=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_763(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_notifyReply(872=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_763(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_notifyReplyBody(805=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_806(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_notifyRequest(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_notifyRequest(498=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_notifyRequestBody(403, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_405(405, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_observedEvent(410, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_413(413, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEvent(415, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_416(416, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEvent(713=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_714(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEvent(717=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_714(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_observedEventBody(419=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_420(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEventBody(434=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_435(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_observedEventParameter(421, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_422(422, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEventParameter(427, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_428(428, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_observedEventParameters(422, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_426(426, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEventParameters(428=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_429(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_observedEvents(413, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_414(414, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEvents(416=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_417(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_observedEventsDescriptor(403=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_404(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEventsDescriptor(827=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_832(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEventsDescriptor(856=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_832(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEventsDescriptor(868=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_832(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_onOrOff(630=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_631(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_onOrOff(634=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_635(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_optAuditDescriptor(161=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optAuditDescriptor(491=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_492(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optAuditDescriptor(494=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_495(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_optImmAckRequired(751, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_752(752, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_optIndAudeventSpecParameter(323=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_326(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_optIndAudsignalParm(189=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_195(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optIndAudsignalParm(348=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_195(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_optPropertyParms(571=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_572(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optPropertyParms(580=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_581(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_optSep(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(93, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(109=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(115=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(381, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(388, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(410, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(412, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(411, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_431(431, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(415, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(412, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(432, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(433, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(713, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(412, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(717, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(412, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(888=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_892(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(889=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_891(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_packagesDescriptor(827=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_831(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_packagesDescriptor(856=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_831(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_packagesDescriptor(868=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_831(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_packagesItem(283, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_285(285, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_packagesItem(848, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_849(849, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_packagesItem(851, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_852(852, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_packagesItems(849, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_850(850, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_packagesItems(852=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_853(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_parmValue(214=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_252(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parmValue(367=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_394(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parmValue(424=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_425(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parmValue(457=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_489(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_pathName(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_887(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_pkgdName(191, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_193(193, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(207=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(318=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_319(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(322, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_323(323, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(412, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_419(419, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(433, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_434(434, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(440=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_441(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(448=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_441(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(453, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(457, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(485, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(457, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(523, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_525(525, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(529, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_525(525, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(533=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(540=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(543=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(548=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(573, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(457, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(576, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(457, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(594, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(457, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(611, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(457, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(624, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(457, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(644, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(457, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(655, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_657(657, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(674, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_676(676, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(694, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_676(676, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(709, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_657(657, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_portNumber(107, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(109, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_portNumber(114, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(115, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_portNumber(381=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_382(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_priority(453=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_456(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_priority(485=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_456(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_propertyParm(453=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_455(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParm(485=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_455(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParm(573, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_574(574, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParm(576, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_577(577, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParm(594=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_597(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParm(611=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_597(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParm(624=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_625(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParm(644=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_625(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_propertyParms(574, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_575(575, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParms(577=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_578(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_requestID(230=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_232(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_requestID(316, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_317(317, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_requestID(408, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_409(409, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_requestID(653, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_654(654, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_requestID(672, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_673(673, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_requestedEvent(655, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_656(656, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_requestedEvent(709, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_710(710, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_requestedEventBody(657=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_658(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_requestedEvents(656, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_708(708, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_requestedEvents(710=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_711(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_safeToken(4, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(93, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(6, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(88, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(89, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(90, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(91, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_886(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(100, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(101, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(102, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(102, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(104, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(102, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(107=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(114=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(130=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_732(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(134=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(160=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(191=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(204=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(207=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(212, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(214, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(222=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_224(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(230=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_231(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(245=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_246(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(253=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(254=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(255=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(256=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(264=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(265=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(268=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(269=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(279, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(214, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(283=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_284(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(298=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_299(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(302=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_224(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(307=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_308(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(311=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_308(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(316=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_231(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(318=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(322=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(327=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_328(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(333=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_224(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(351=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(356=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_366(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(379=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_380(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(381=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(384=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(386=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_387(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(390=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_391(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(392=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_393(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(396=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_366(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(401=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(408=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_231(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(412=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(421=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_328(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(427=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_328(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(433=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(440=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(448=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(453=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(465=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(469=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(478=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(482=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_483(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(485=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(490=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(493=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(501=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(523=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(526=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(529=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(533=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(538=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(540=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(543=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(548=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(552=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_553(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(556=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(559=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(563=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_565(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(564=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_565(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(568=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_565(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(573=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(576=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(594=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(611=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(615=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_224(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(624=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(644=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(653=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_231(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(655=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(659=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_328(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(672=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_231(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(674=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(678=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_328(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(689=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_328(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(694=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(704=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_328(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(709=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(741=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_743(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(745=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_743(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(749=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_732(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(759=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(779=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(804=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(810=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(816=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(819=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_820(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(848=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_284(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(851=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_284(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(860=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(865=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(881=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_732(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_secondEventParameter(678, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_679(679, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_secondEventParameter(689, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_690(690, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_secondEventParameters(679, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_688(688, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_secondEventParameters(690=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_691(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_secondRequestedEvent(674, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_675(675, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_secondRequestedEvent(694, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_695(695, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_secondRequestedEventBody(676=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_677(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_secondRequestedEvents(675, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_693(693, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_secondRequestedEvents(695=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_696(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_servChgReplyParm(786, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_792(792, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_servChgReplyParm(798, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_799(799, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_servChgReplyParms(792, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_797(797, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_servChgReplyParms(799=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_800(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeAddress(356=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_365(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeAddress(396=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_365(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeAddress(786=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_791(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeAddress(798=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_791(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeDelay(356=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_364(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeDelay(396=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_364(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeDescriptor(353, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_354(354, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeMethod(356=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_363(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeMethod(396=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_363(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeMgcId(356=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_362(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeMgcId(396=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_362(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeMgcId(786=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_790(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeMgcId(798=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_790(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeParm(356, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_361(361, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeParm(396, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_397(397, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeParms(361, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_395(395, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeParms(397=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_398(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeProfile(356=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_360(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeProfile(396=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_360(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeProfile(786=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_789(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeProfile(798=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_789(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeReason(356=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_359(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeReason(396=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_359(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeReply(761=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_762(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeReply(872=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_762(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeReplyBody(780=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_781(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeReplyDescriptor(782, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_783(783, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeRequest(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeRequest(498=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeVersion(356=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_358(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeVersion(396=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_358(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeVersion(786=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_788(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeVersion(798=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_788(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceState(601=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_602(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceStates(594=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_596(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceStates(611=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_596(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_sigParameter(212, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(213, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sigParameter(279, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_280(280, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_sigParameters(213, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_278(278, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sigParameters(280=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_281(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalList(533=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_536(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalList(548=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_536(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalListId(204, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(205, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalListId(538, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_539(539, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalListParm(207, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_209(209, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalListParm(540, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_541(541, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalListParm(543, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_544(544, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalListParms(541, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_542(542, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalListParms(544=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_545(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalName(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(198, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalName(207, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(198, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalName(533, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(198, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalName(540, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(198, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalName(543, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(198, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalName(548, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(198, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalParm(533, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_535(535, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalParm(548, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_549(549, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalParms(535, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_547(547, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalParms(549=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_550(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalRequest(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_197(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalRequest(207=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalRequest(533=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_534(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalRequest(540=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalRequest(543=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalRequest(548=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_534(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalType(225=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_226(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalsDescriptor(504=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_506(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalsDescriptor(668, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_669(669, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalsDescriptor(685, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_686(686, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalsDescriptor(722=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_506(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalsDescriptor(827=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_830(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalsDescriptor(856=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_830(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalsDescriptor(868=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_830(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_statisticsDescriptor(504=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_505(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsDescriptor(582=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_586(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsDescriptor(617=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_586(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsDescriptor(620=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_586(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsDescriptor(649=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_586(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsDescriptor(722=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_505(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsDescriptor(827=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_829(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsDescriptor(856=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_829(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsDescriptor(868=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_829(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_statisticsParameter(523, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_524(524, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsParameter(529, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_530(530, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_statisticsParameters(524, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_528(528, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsParameters(530=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_531(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_streamDescriptor(582=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_585(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_streamDescriptor(649=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_585(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_streamID(222=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_223(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_streamID(302, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_303(303, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_streamID(333=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_334(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_streamID(615, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_616(616, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_streamModes(636=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_637(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_streamParm(582=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_584(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_streamParm(617, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_618(618, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_streamParm(620, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_621(621, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_streamParm(649=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_584(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_streamParmList(618, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_619(619, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_streamParmList(621=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_622(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_subtractRequest(137=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_subtractRequest(498=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_terminationA(465, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_468(468, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationA(478, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_468(468, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_terminationAudit(827, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_828(828, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationAudit(868, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_869(869, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_terminationB(469, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_471(471, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_terminationID(160, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(161, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(351, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_352(352, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(401, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_402(402, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(465=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_467(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(469=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_470(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(478=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_467(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(490, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_491(491, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(493, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_494(494, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(501, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_502(502, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(556, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_557(557, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(559, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_560(560, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(779, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_780(780, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(804, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_805(805, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(810, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_811(811, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(816, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_557(557, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(860, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_811(811, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(865, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_866(866, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_terminationIDList(554=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_555(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationIDList(813=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_814(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationIDList(862=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_814(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_terminationIDListRepeat(557, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_558(558, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationIDListRepeat(560=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_561(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_terminationStateDescriptor(582=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_583(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationStateDescriptor(649=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_583(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_terminationStateParm(594, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_595(595, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationStateParm(611, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_612(612, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_terminationStateParms(595, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_610(610, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationStateParms(612=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_613(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_timeStamp(356=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_357(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_timeStamp(396=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_357(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_timeStamp(410, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_411(411, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_timeStamp(415, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_411(411, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_timeStamp(713, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_411(411, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_timeStamp(717, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_411(411, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_timeStamp(786=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_787(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_timeStamp(798=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_787(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_topologyDescriptor(453=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_454(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_topologyDescriptor(485=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_454(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_topologyDirection(472=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_473(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_topologyTriple(465, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_466(466, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_topologyTriple(478, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_479(479, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_topologyTripleList(466, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_477(477, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_topologyTripleList(479=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_480(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionAck(741, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_742(742, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionAck(745, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_746(746, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionAckList(742, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_744(744, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionAckList(746=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_747(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionID(130, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_731(731, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionID(749, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_750(750, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionID(881, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_882(882, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionItem(97, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(122, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionItem(122, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(122, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionList(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionList(122=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_885(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionPending(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionPending(122=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionReply(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionReply(122=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionReplyBody(752, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_755(755, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionRequest(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionRequest(122=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionResponseAck(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionResponseAck(122=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_value(253=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_262(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(254=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_261(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(255=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_260(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(256=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_257(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(264, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_275(275, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(265, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_266(266, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(268, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_272(272, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(269, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_270(270, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(384=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_385(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(526=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_527(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_valueList(266, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_267(267, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_valueList(270=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_271(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_valueList(275, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_276(276, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,{yeccpars2_0_,1}}).
-file("megaco_text_parser_prev3a.yrl", 455).
yeccpars2_0_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_1_,1}}).
-file("megaco_text_parser_prev3a.yrl", 460).
yeccpars2_1_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_3_,1}}).
-file("megaco_text_parser_prev3a.yrl", 454).
yeccpars2_3_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   sep
  end | __Stack].

-compile({inline,{yeccpars2_8_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1430).
yeccpars2_8_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_9_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1429).
yeccpars2_9_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_10_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1431).
yeccpars2_10_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_11_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1432).
yeccpars2_11_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_12_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1434).
yeccpars2_12_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_13_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1435).
yeccpars2_13_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_14_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1436).
yeccpars2_14_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_15_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1439).
yeccpars2_15_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_16_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1437).
yeccpars2_16_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_17_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1446).
yeccpars2_17_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_18_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1443).
yeccpars2_18_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_19_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1444).
yeccpars2_19_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_20_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1445).
yeccpars2_20_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_21_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1447).
yeccpars2_21_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_22_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1448).
yeccpars2_22_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_23_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1451).
yeccpars2_23_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_24_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1455).
yeccpars2_24_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_25_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1456).
yeccpars2_25_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_26_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1457).
yeccpars2_26_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_27_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1458).
yeccpars2_27_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_28_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1459).
yeccpars2_28_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_29_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1460).
yeccpars2_29_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_30_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1461).
yeccpars2_30_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_31_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1463).
yeccpars2_31_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_32_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1469).
yeccpars2_32_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_33_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1464).
yeccpars2_33_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_34_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1466).
yeccpars2_34_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_35_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1467).
yeccpars2_35_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_36_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1468).
yeccpars2_36_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_37_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1470).
yeccpars2_37_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_38_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1473).
yeccpars2_38_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_39_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1475).
yeccpars2_39_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_40_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1474).
yeccpars2_40_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_41_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1478).
yeccpars2_41_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_42_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1479).
yeccpars2_42_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_43_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1480).
yeccpars2_43_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_44_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1488).
yeccpars2_44_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_45_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1487).
yeccpars2_45_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_46_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1489).
yeccpars2_46_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_47_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1492).
yeccpars2_47_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_48_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1494).
yeccpars2_48_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_49_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1493).
yeccpars2_49_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_50_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1491).
yeccpars2_50_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_51_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1496).
yeccpars2_51_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_52_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1495).
yeccpars2_52_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_53_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1498).
yeccpars2_53_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_54_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1500).
yeccpars2_54_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_55_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1501).
yeccpars2_55_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_56_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1502).
yeccpars2_56_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_57_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1503).
yeccpars2_57_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_58_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1505).
yeccpars2_58_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_59_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1510).
yeccpars2_59_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_60_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1511).
yeccpars2_60_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_61_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1506).
yeccpars2_61_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_62_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1507).
yeccpars2_62_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_63_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1427).
yeccpars2_63_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_64_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1512).
yeccpars2_64_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_65_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1513).
yeccpars2_65_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_66_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1518).
yeccpars2_66_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_67_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1516).
yeccpars2_67_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_68_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1515).
yeccpars2_68_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_69_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1514).
yeccpars2_69_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_70_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1519).
yeccpars2_70_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_71_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1521).
yeccpars2_71_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_72_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1523).
yeccpars2_72_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_73_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1525).
yeccpars2_73_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_74_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1526).
yeccpars2_74_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_75_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1527).
yeccpars2_75_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_76_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1528).
yeccpars2_76_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_77_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1530).
yeccpars2_77_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_78_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1531).
yeccpars2_78_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_79_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1532).
yeccpars2_79_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_80_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1533).
yeccpars2_80_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_81_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1534).
yeccpars2_81_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_82_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1535).
yeccpars2_82_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_83_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1536).
yeccpars2_83_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_84_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1537).
yeccpars2_84_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_85_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1538).
yeccpars2_85_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_86_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1539).
yeccpars2_86_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_87_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1540).
yeccpars2_87_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_91_,1}}).
-file("megaco_text_parser_prev3a.yrl", 455).
yeccpars2_91_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_92_,1}}).
-file("megaco_text_parser_prev3a.yrl", 459).
yeccpars2_92_(__Stack0) ->
 [__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_auth_header ( __3 , __5 , __7 )
  end | __Stack].

-compile({inline,{yeccpars2_93_,1}}).
-file("megaco_text_parser_prev3a.yrl", 455).
yeccpars2_93_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_95_,1}}).
-file("megaco_text_parser_prev3a.yrl", 452).
yeccpars2_95_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'MegacoMessage' { authHeader = __2 , mess = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_101_,1}}).
-file("megaco_text_parser_prev3a.yrl", 937).
yeccpars2_101_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_102_,1}}).
-file("megaco_text_parser_prev3a.yrl", 937).
yeccpars2_102_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_104_,1}}).
-file("megaco_text_parser_prev3a.yrl", 937).
yeccpars2_104_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_105_,1}}).
-file("megaco_text_parser_prev3a.yrl", 938).
yeccpars2_105_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ colon | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_106_,1}}).
-file("megaco_text_parser_prev3a.yrl", 935).
yeccpars2_106_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_domainAddress ( __2 , asn1_NOVALUE )
  end | __Stack].

-compile({inline,{yeccpars2_108_,1}}).
-file("megaco_text_parser_prev3a.yrl", 942).
yeccpars2_108_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_uint16 ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_109_,1}}).
-file("megaco_text_parser_prev3a.yrl", 455).
yeccpars2_109_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_110_,1}}).
-file("megaco_text_parser_prev3a.yrl", 933).
yeccpars2_110_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_domainAddress ( __2 , __5 )
  end | __Stack].

-compile({inline,{yeccpars2_111_,1}}).
-file("megaco_text_parser_prev3a.yrl", 939).
yeccpars2_111_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_113_,1}}).
-file("megaco_text_parser_prev3a.yrl", 925).
yeccpars2_113_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_domainName ( __2 , asn1_NOVALUE )
  end | __Stack].

-compile({inline,{yeccpars2_115_,1}}).
-file("megaco_text_parser_prev3a.yrl", 455).
yeccpars2_115_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_116_,1}}).
-file("megaco_text_parser_prev3a.yrl", 923).
yeccpars2_116_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_domainName ( __2 , __5 )
  end | __Stack].

-compile({inline,{yeccpars2_117_,1}}).
-file("megaco_text_parser_prev3a.yrl", 473).
yeccpars2_117_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { transactionResponseAck , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_118_,1}}).
-file("megaco_text_parser_prev3a.yrl", 470).
yeccpars2_118_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { transactionRequest , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_119_,1}}).
-file("megaco_text_parser_prev3a.yrl", 471).
yeccpars2_119_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { transactionReply , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_120_,1}}).
-file("megaco_text_parser_prev3a.yrl", 472).
yeccpars2_120_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { transactionPending , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_121_,1}}).
-file("megaco_text_parser_prev3a.yrl", 465).
yeccpars2_121_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { transactions , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_122_,1}}).
-file("megaco_text_parser_prev3a.yrl", 467).
yeccpars2_122_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_123_,1}}).
-file("megaco_text_parser_prev3a.yrl", 462).
yeccpars2_123_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_message ( __1 , __2 , __3 )
  end | __Stack].

-compile({inline,{yeccpars2_124_,1}}).
-file("megaco_text_parser_prev3a.yrl", 464).
yeccpars2_124_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { messageError , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_132_,1}}).
-file("megaco_text_parser_prev3a.yrl", 500).
yeccpars2_132_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_135_,1}}).
-file("megaco_text_parser_prev3a.yrl", 930).
yeccpars2_135_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_contextID ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_141_,1}}).
-file("megaco_text_parser_prev3a.yrl", 512).
yeccpars2_141_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { contextProps , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_142_,1}}).
-file("megaco_text_parser_prev3a.yrl", 513).
yeccpars2_142_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { contextAudit , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_143_,1}}).
-file("megaco_text_parser_prev3a.yrl", 517).
yeccpars2_143_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   merge_context_request ( # 'ContextRequest' { } , __1 )
  end | __Stack].

-compile({inline,{yeccpars2_144_,1}}).
-file("megaco_text_parser_prev3a.yrl", 514).
yeccpars2_144_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { commandRequest , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_148_,1}}).
-file("megaco_text_parser_prev3a.yrl", 510).
yeccpars2_148_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_150_,1}}).
-file("megaco_text_parser_prev3a.yrl", 613).
yeccpars2_150_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { addReq , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_155_,1}}).
-file("megaco_text_parser_prev3a.yrl", 615).
yeccpars2_155_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { modReq , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_156_,1}}).
-file("megaco_text_parser_prev3a.yrl", 614).
yeccpars2_156_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { moveReq , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_161_,1}}).
-file("megaco_text_parser_prev3a.yrl", 654).
yeccpars2_161_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_162_,1}}).
-file("megaco_text_parser_prev3a.yrl", 958).
yeccpars2_162_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_terminationID ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_163_,1}}).
-file("megaco_text_parser_prev3a.yrl", 648).
yeccpars2_163_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   make_commandRequest ( { subtractReq , __1 } ,
    # 'SubtractRequest' { terminationID = [ __3 ] ,
    auditDescriptor = __4 } )
  end | __Stack].

-compile({inline,{yeccpars2_167_,1}}).
-file("megaco_text_parser_prev3a.yrl", 712).
yeccpars2_167_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_168_,1}}).
-file("megaco_text_parser_prev3a.yrl", 733).
yeccpars2_168_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { terminationAudit , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_169_,1}}).
-file("megaco_text_parser_prev3a.yrl", 760).
yeccpars2_169_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { indAudStatisticsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_170_,1}}).
-file("megaco_text_parser_prev3a.yrl", 754).
yeccpars2_170_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { indAudSignalsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_171_,1}}).
-file("megaco_text_parser_prev3a.yrl", 762).
yeccpars2_171_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { indAudPackagesDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_172_,1}}).
-file("megaco_text_parser_prev3a.yrl", 750).
yeccpars2_172_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { indAudMediaDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_173_,1}}).
-file("megaco_text_parser_prev3a.yrl", 752).
yeccpars2_173_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { indAudEventsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_174_,1}}).
-file("megaco_text_parser_prev3a.yrl", 758).
yeccpars2_174_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { indAudEventBufferDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_175_,1}}).
-file("megaco_text_parser_prev3a.yrl", 756).
yeccpars2_175_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { indAudDigitMapDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_176_,1}}).
-file("megaco_text_parser_prev3a.yrl", 747).
yeccpars2_176_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_178_,1}}).
-file("megaco_text_parser_prev3a.yrl", 715).
yeccpars2_178_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_180_,1}}).
-file("megaco_text_parser_prev3a.yrl", 857).
yeccpars2_180_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_IADMD ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_181_,1}}).
-file("megaco_text_parser_prev3a.yrl", 722).
yeccpars2_181_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   digitMapToken
  end | __Stack].

-compile({inline,{yeccpars2_182_,1}}).
-file("megaco_text_parser_prev3a.yrl", 731).
yeccpars2_182_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   eventBufferToken
  end | __Stack].

-compile({inline,{yeccpars2_183_,1}}).
-file("megaco_text_parser_prev3a.yrl", 732).
yeccpars2_183_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   eventsToken
  end | __Stack].

-compile({inline,{yeccpars2_184_,1}}).
-file("megaco_text_parser_prev3a.yrl", 721).
yeccpars2_184_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   mediaToken
  end | __Stack].

-compile({inline,{yeccpars2_185_,1}}).
-file("megaco_text_parser_prev3a.yrl", 720).
yeccpars2_185_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   modemToken
  end | __Stack].

-compile({inline,{yeccpars2_186_,1}}).
-file("megaco_text_parser_prev3a.yrl", 719).
yeccpars2_186_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   muxToken
  end | __Stack].

-compile({inline,{yeccpars2_187_,1}}).
-file("megaco_text_parser_prev3a.yrl", 724).
yeccpars2_187_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   observedEventsToken
  end | __Stack].

-compile({inline,{yeccpars2_188_,1}}).
-file("megaco_text_parser_prev3a.yrl", 725).
yeccpars2_188_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   packagesToken
  end | __Stack].

-compile({inline,{yeccpars2_189_,1}}).
-file("megaco_text_parser_prev3a.yrl", 730).
yeccpars2_189_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   signalsToken
  end | __Stack].

-compile({inline,{yeccpars2_190_,1}}).
-file("megaco_text_parser_prev3a.yrl", 723).
yeccpars2_190_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   statsToken
  end | __Stack].

-compile({inline,{yeccpars2_192_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1096).
yeccpars2_192_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_pkgdName ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_194_,1}}).
-file("megaco_text_parser_prev3a.yrl", 860).
yeccpars2_194_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'IndAudStatisticsDescriptor' { statName = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_195_,1}}).
-file("megaco_text_parser_prev3a.yrl", 839).
yeccpars2_195_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_197_,1}}).
-file("megaco_text_parser_prev3a.yrl", 846).
yeccpars2_197_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { signal , ensure_indAudSignal ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_198_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1189).
yeccpars2_198_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   merge_signalRequest ( __1 , [ ] )
  end | __Stack].

-compile({inline,{yeccpars2_201_,1}}).
-file("megaco_text_parser_prev3a.yrl", 845).
yeccpars2_201_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { seqSigList , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_202_,1}}).
-file("megaco_text_parser_prev3a.yrl", 842).
yeccpars2_202_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   asn1_NOVALUE
  end | __Stack].

-compile({inline,{yeccpars2_203_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1519).
yeccpars2_203_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_206_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1253).
yeccpars2_206_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_uint16 ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_210_,1}}).
-file("megaco_text_parser_prev3a.yrl", 850).
yeccpars2_210_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'IndAudSeqSigList' { id = ensure_uint16 ( __3 ) ,
    signalList =
    ensure_indAudSignalListParm ( __5 ) }
  end | __Stack].

-compile({inline,{yeccpars2_211_,1}}).
-file("megaco_text_parser_prev3a.yrl", 843).
yeccpars2_211_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_213_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1192).
yeccpars2_213_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_215_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1443).
yeccpars2_215_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_216_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1447).
yeccpars2_216_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_217_COMMA,1}}).
-file("megaco_text_parser_prev3a.yrl", 1222).
yeccpars2_217_COMMA(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   keepActive
  end | __Stack].

-compile({inline,{yeccpars2_217_RBRKT,1}}).
-file("megaco_text_parser_prev3a.yrl", 1222).
yeccpars2_217_RBRKT(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   keepActive
  end | __Stack].

-compile({inline,{yeccpars2_217_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1470).
yeccpars2_217_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_218_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1488).
yeccpars2_218_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_219_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1505).
yeccpars2_219_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_220_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1521).
yeccpars2_220_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_221_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1523).
yeccpars2_221_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_223_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1214).
yeccpars2_223_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { stream , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_224_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1094).
yeccpars2_224_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_streamID ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_226_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1216).
yeccpars2_226_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { signal_type , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_227_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1230).
yeccpars2_227_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   brief
  end | __Stack].

-compile({inline,{yeccpars2_228_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1228).
yeccpars2_228_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   onOff
  end | __Stack].

-compile({inline,{yeccpars2_229_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1229).
yeccpars2_229_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   timeOut
  end | __Stack].

-compile({inline,{yeccpars2_231_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1287).
yeccpars2_231_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_requestID ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_232_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1224).
yeccpars2_232_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { requestId , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_235_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1237).
yeccpars2_235_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_236_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1240).
yeccpars2_236_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   onInterruptByEvent
  end | __Stack].

-compile({inline,{yeccpars2_237_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1241).
yeccpars2_237_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   onInterruptByNewSignalDescr
  end | __Stack].

-compile({inline,{yeccpars2_238_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1242).
yeccpars2_238_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   otherReason
  end | __Stack].

-compile({inline,{yeccpars2_239_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1239).
yeccpars2_239_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   onTimeOut
  end | __Stack].

-compile({inline,{yeccpars2_242_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1237).
yeccpars2_242_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_243_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1236).
yeccpars2_243_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_244_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1221).
yeccpars2_244_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { notify_completion , [ __4 | __5 ] }
  end | __Stack].

-compile({inline,{yeccpars2_246_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1218).
yeccpars2_246_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { duration , ensure_uint16 ( __3 ) }
  end | __Stack].

-compile({inline,{yeccpars2_248_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1223).
yeccpars2_248_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { direction , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_249_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1234).
yeccpars2_249_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   both
  end | __Stack].

-compile({inline,{yeccpars2_250_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1232).
yeccpars2_250_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   external
  end | __Stack].

-compile({inline,{yeccpars2_251_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1233).
yeccpars2_251_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   internal
  end | __Stack].

-compile({inline,{yeccpars2_252_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1226).
yeccpars2_252_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { other , ensure_NAME ( __1 ) , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_257_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1030).
yeccpars2_257_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   # 'PropertyParm' { value = [ __2 ] ,
    extraInfo = { relation , unequalTo } }
  end | __Stack].

-compile({inline,{yeccpars2_258_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1425).
yeccpars2_258_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_value ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_259_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1424).
yeccpars2_259_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_value ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_260_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1033).
yeccpars2_260_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   # 'PropertyParm' { value = [ __2 ] ,
    extraInfo = { relation , smallerThan } }
  end | __Stack].

-compile({inline,{yeccpars2_261_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1036).
yeccpars2_261_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   # 'PropertyParm' { value = [ __2 ] ,
    extraInfo = { relation , greaterThan } }
  end | __Stack].

-compile({inline,{yeccpars2_262_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1057).
yeccpars2_262_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'PropertyParm' { value = [ __1 ] }
  end | __Stack].

-compile({inline,{yeccpars2_263_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1027).
yeccpars2_263_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_266_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1060).
yeccpars2_266_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_270_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1060).
yeccpars2_270_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_271_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1059).
yeccpars2_271_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_273_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1049).
yeccpars2_273_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'PropertyParm' { value = [ __2 , __4 ] ,
    extraInfo = { range , true } }
  end | __Stack].

-compile({inline,{yeccpars2_274_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1053).
yeccpars2_274_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'PropertyParm' { value = [ __2 | __3 ] ,
    extraInfo = { sublist , true } }
  end | __Stack].

-compile({inline,{yeccpars2_275_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1060).
yeccpars2_275_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_277_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1045).
yeccpars2_277_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'PropertyParm' { value = [ __2 | __3 ] ,
    extraInfo = { sublist , false } }
  end | __Stack].

-compile({inline,{yeccpars2_280_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1192).
yeccpars2_280_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_281_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1191).
yeccpars2_281_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_282_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1188).
yeccpars2_282_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_signalRequest ( __1 , [ __3 | __4 ] )
  end | __Stack].

-compile({inline,{yeccpars2_284_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1378).
yeccpars2_284_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_packagesItem ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_286_,1}}).
-file("megaco_text_parser_prev3a.yrl", 863).
yeccpars2_286_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_indAudPackagesDescriptor ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_288_,1}}).
-file("megaco_text_parser_prev3a.yrl", 781).
yeccpars2_288_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { termStateDescr , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_289_,1}}).
-file("megaco_text_parser_prev3a.yrl", 779).
yeccpars2_289_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { streamParm , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_290_,1}}).
-file("megaco_text_parser_prev3a.yrl", 780).
yeccpars2_290_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { streamDescr , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_291_,1}}).
-file("megaco_text_parser_prev3a.yrl", 787).
yeccpars2_291_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'IndAudStreamParms' { statisticsDescriptor = __1 }
  end | __Stack].

-compile({inline,{yeccpars2_293_,1}}).
-file("megaco_text_parser_prev3a.yrl", 785).
yeccpars2_293_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'IndAudStreamParms' { localControlDescriptor = __1 }
  end | __Stack].

-compile({inline,{yeccpars2_299_,1}}).
-file("megaco_text_parser_prev3a.yrl", 815).
yeccpars2_299_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_indAudTerminationStateParm ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_301_,1}}).
-file("megaco_text_parser_prev3a.yrl", 809).
yeccpars2_301_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_indAudTerminationStateDescriptor ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_306_,1}}).
-file("megaco_text_parser_prev3a.yrl", 791).
yeccpars2_306_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'IndAudStreamDescriptor' { streamID = __3 ,
    streamParms = __5 }
  end | __Stack].

-compile({inline,{yeccpars2_308_,1}}).
-file("megaco_text_parser_prev3a.yrl", 804).
yeccpars2_308_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_indAudLocalParm ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_309_,1}}).
-file("megaco_text_parser_prev3a.yrl", 800).
yeccpars2_309_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_312_,1}}).
-file("megaco_text_parser_prev3a.yrl", 800).
yeccpars2_312_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_313_,1}}).
-file("megaco_text_parser_prev3a.yrl", 799).
yeccpars2_313_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_314_,1}}).
-file("megaco_text_parser_prev3a.yrl", 797).
yeccpars2_314_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_indAudLocalControlDescriptor ( [ __3 | __4 ] )
  end | __Stack].

-compile({inline,{yeccpars2_315_,1}}).
-file("megaco_text_parser_prev3a.yrl", 767).
yeccpars2_315_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_indAudMediaDescriptor ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_321_,1}}).
-file("megaco_text_parser_prev3a.yrl", 833).
yeccpars2_321_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'IndAudEventsDescriptor' { requestID = __3 ,
    pkgdName = __5 }
  end | __Stack].

-compile({inline,{yeccpars2_323_,1}}).
-file("megaco_text_parser_prev3a.yrl", 825).
yeccpars2_323_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_325_,1}}).
-file("megaco_text_parser_prev3a.yrl", 818).
yeccpars2_325_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_326_,1}}).
-file("megaco_text_parser_prev3a.yrl", 821).
yeccpars2_326_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   merge_indAudEventBufferDescriptor ( __1 , __2 )
  end | __Stack].

-compile({inline,{yeccpars2_328_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1170).
yeccpars2_328_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_NAME ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_330_,1}}).
-file("megaco_text_parser_prev3a.yrl", 828).
yeccpars2_330_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { streamID , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_331_,1}}).
-file("megaco_text_parser_prev3a.yrl", 829).
yeccpars2_331_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { eventParameterName , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_332_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1523).
yeccpars2_332_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_334_,1}}).
-file("megaco_text_parser_prev3a.yrl", 865).
yeccpars2_334_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_335_,1}}).
-file("megaco_text_parser_prev3a.yrl", 824).
yeccpars2_335_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_336_,1}}).
-file("megaco_text_parser_prev3a.yrl", 709).
yeccpars2_336_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_auditDescriptor ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_337_,1}}).
-file("megaco_text_parser_prev3a.yrl", 711).
yeccpars2_337_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_339_,1}}).
-file("megaco_text_parser_prev3a.yrl", 715).
yeccpars2_339_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_340_,1}}).
-file("megaco_text_parser_prev3a.yrl", 714).
yeccpars2_340_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_341_,1}}).
-file("megaco_text_parser_prev3a.yrl", 742).
yeccpars2_341_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_343_,1}}).
-file("megaco_text_parser_prev3a.yrl", 747).
yeccpars2_343_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_349_,1}}).
-file("megaco_text_parser_prev3a.yrl", 746).
yeccpars2_349_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_350_,1}}).
-file("megaco_text_parser_prev3a.yrl", 653).
yeccpars2_350_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_357_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1328).
yeccpars2_357_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { time_stamp , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_358_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1330).
yeccpars2_358_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { version , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_359_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1323).
yeccpars2_359_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { reason , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_360_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1326).
yeccpars2_360_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { profile , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_361_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1320).
yeccpars2_361_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_362_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1329).
yeccpars2_362_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { mgc_id , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_363_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1322).
yeccpars2_363_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { method , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_364_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1324).
yeccpars2_364_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { delay , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_365_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1325).
yeccpars2_365_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { address , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_366_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1422).
yeccpars2_366_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_extensionParameter ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_368_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1327).
yeccpars2_368_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { extension , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_369_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1332).
yeccpars2_369_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { audit_item , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_370_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1446).
yeccpars2_370_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_371_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1478).
yeccpars2_371_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_372_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1479).
yeccpars2_372_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_373_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1500).
yeccpars2_373_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_374_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1501).
yeccpars2_374_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_375_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1518).
yeccpars2_375_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_376_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1331).
yeccpars2_376_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   incomplete
  end | __Stack].

-compile({inline,{yeccpars2_377_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1380).
yeccpars2_377_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_timeStamp ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_378_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1540).
yeccpars2_378_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_380_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1349).
yeccpars2_380_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_version ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_381_,1}}).
-file("megaco_text_parser_prev3a.yrl", 455).
yeccpars2_381_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_382_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1343).
yeccpars2_382_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { portNumber , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_383_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1341).
yeccpars2_383_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_385_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1337).
yeccpars2_385_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_387_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1347).
yeccpars2_387_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_profile ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_388_,1}}).
-file("megaco_text_parser_prev3a.yrl", 455).
yeccpars2_388_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_389_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1345).
yeccpars2_389_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_391_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1335).
yeccpars2_391_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_serviceChangeMethod ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_393_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1339).
yeccpars2_393_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_uint32 ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_394_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1352).
yeccpars2_394_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   setelement ( # 'PropertyParm' .name , __2 , __1 )
  end | __Stack].

-compile({inline,{yeccpars2_397_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1320).
yeccpars2_397_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_398_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1319).
yeccpars2_398_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_399_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1316).
yeccpars2_399_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_ServiceChangeParm ( [ __3 | __4 ] )
  end | __Stack].

-compile({inline,{yeccpars2_400_,1}}).
-file("megaco_text_parser_prev3a.yrl", 891).
yeccpars2_400_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   make_commandRequest ( { serviceChangeReq , __1 } ,
    # 'ServiceChangeRequest' { terminationID = [ __3 ] ,
    serviceChangeParms = __5 } )
  end | __Stack].

-compile({inline,{yeccpars2_404_,1}}).
-file("megaco_text_parser_prev3a.yrl", 877).
yeccpars2_404_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'NotifyRequest' { observedEventsDescriptor = __1 }
  end | __Stack].

-compile({inline,{yeccpars2_406_,1}}).
-file("megaco_text_parser_prev3a.yrl", 879).
yeccpars2_406_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'NotifyRequest' { errorDescriptor = __1 }
  end | __Stack].

-compile({inline,{yeccpars2_410_,1}}).
-file("megaco_text_parser_prev3a.yrl", 455).
yeccpars2_410_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_411_,1}}).
-file("megaco_text_parser_prev3a.yrl", 455).
yeccpars2_411_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_413_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1267).
yeccpars2_413_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_415_,1}}).
-file("megaco_text_parser_prev3a.yrl", 455).
yeccpars2_415_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_416_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1267).
yeccpars2_416_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_417_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1266).
yeccpars2_417_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_418_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1263).
yeccpars2_418_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'ObservedEventsDescriptor' { requestId = __3 ,
    observedEventLst = [ __5 | __6 ] }
  end | __Stack].

-compile({inline,{yeccpars2_419_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1279).
yeccpars2_419_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_420_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1274).
yeccpars2_420_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_observed_event ( __3 , __2 , asn1_NOVALUE )
  end | __Stack].

-compile({inline,{yeccpars2_422_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1282).
yeccpars2_422_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_425_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1168).
yeccpars2_425_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   select_stream_or_other ( __1 , __2 )
  end | __Stack].

-compile({inline,{yeccpars2_428_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1282).
yeccpars2_428_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_429_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1281).
yeccpars2_429_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_430_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1278).
yeccpars2_430_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_432_,1}}).
-file("megaco_text_parser_prev3a.yrl", 455).
yeccpars2_432_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_434_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1279).
yeccpars2_434_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_435_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1272).
yeccpars2_435_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_observed_event ( __6 , __5 , __1 )
  end | __Stack].

-compile({inline,{yeccpars2_436_,1}}).
-file("megaco_text_parser_prev3a.yrl", 873).
yeccpars2_436_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   make_commandRequest ( { notifyReq , __1 } ,
    setelement ( # 'NotifyRequest' .terminationID , __5 , [ __3 ] ) )
  end | __Stack].

-compile({inline,{yeccpars2_441_,1}}).
-file("megaco_text_parser_prev3a.yrl", 556).
yeccpars2_441_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { prop , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_442_,1}}).
-file("megaco_text_parser_prev3a.yrl", 549).
yeccpars2_442_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_443_,1}}).
-file("megaco_text_parser_prev3a.yrl", 553).
yeccpars2_443_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   emergencyAudit
  end | __Stack].

-compile({inline,{yeccpars2_444_,1}}).
-file("megaco_text_parser_prev3a.yrl", 555).
yeccpars2_444_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   iepsCallind
  end | __Stack].

-compile({inline,{yeccpars2_445_,1}}).
-file("megaco_text_parser_prev3a.yrl", 554).
yeccpars2_445_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   priorityAudit
  end | __Stack].

-compile({inline,{yeccpars2_446_,1}}).
-file("megaco_text_parser_prev3a.yrl", 552).
yeccpars2_446_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   topologyAudit
  end | __Stack].

-compile({inline,{yeccpars2_449_,1}}).
-file("megaco_text_parser_prev3a.yrl", 549).
yeccpars2_449_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_450_,1}}).
-file("megaco_text_parser_prev3a.yrl", 548).
yeccpars2_450_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_451_,1}}).
-file("megaco_text_parser_prev3a.yrl", 545).
yeccpars2_451_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_452_,1}}).
-file("megaco_text_parser_prev3a.yrl", 539).
yeccpars2_452_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_context_attr_audit_request (
    # 'ContextAttrAuditRequest' { } , __3 )
  end | __Stack].

-compile({inline,{yeccpars2_454_,1}}).
-file("megaco_text_parser_prev3a.yrl", 531).
yeccpars2_454_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { topology , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_455_,1}}).
-file("megaco_text_parser_prev3a.yrl", 536).
yeccpars2_455_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { prop , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_456_,1}}).
-file("megaco_text_parser_prev3a.yrl", 532).
yeccpars2_456_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { priority , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_459_,1}}).
-file("megaco_text_parser_prev3a.yrl", 528).
yeccpars2_459_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_460_,1}}).
-file("megaco_text_parser_prev3a.yrl", 534).
yeccpars2_460_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { emergency , false }
  end | __Stack].

-compile({inline,{yeccpars2_461_,1}}).
-file("megaco_text_parser_prev3a.yrl", 533).
yeccpars2_461_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { emergency , true }
  end | __Stack].

-compile({inline,{yeccpars2_462_,1}}).
-file("megaco_text_parser_prev3a.yrl", 535).
yeccpars2_462_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { iepsCallind , true }
  end | __Stack].

-compile({inline,{yeccpars2_466_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1412).
yeccpars2_466_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_473_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1408).
yeccpars2_473_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'TopologyRequest' { terminationFrom = __1 ,
    terminationTo = __3 ,
    topologyDirection = __5 }
  end | __Stack].

-compile({inline,{yeccpars2_474_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1416).
yeccpars2_474_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   bothway
  end | __Stack].

-compile({inline,{yeccpars2_475_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1417).
yeccpars2_475_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   isolate
  end | __Stack].

-compile({inline,{yeccpars2_476_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1418).
yeccpars2_476_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   oneway
  end | __Stack].

-compile({inline,{yeccpars2_479_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1412).
yeccpars2_479_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_480_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1414).
yeccpars2_480_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_481_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1399).
yeccpars2_481_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_483_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1420).
yeccpars2_483_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_uint16 ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_486_,1}}).
-file("megaco_text_parser_prev3a.yrl", 528).
yeccpars2_486_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_487_,1}}).
-file("megaco_text_parser_prev3a.yrl", 527).
yeccpars2_487_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_488_,1}}).
-file("megaco_text_parser_prev3a.yrl", 522).
yeccpars2_488_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_489_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1024).
yeccpars2_489_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   setelement ( # 'PropertyParm' .name , __2 , __1 )
  end | __Stack].

-compile({inline,{yeccpars2_491_,1}}).
-file("megaco_text_parser_prev3a.yrl", 654).
yeccpars2_491_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_492_,1}}).
-file("megaco_text_parser_prev3a.yrl", 658).
yeccpars2_492_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   make_commandRequest ( { auditValueRequest , __1 } ,
    # 'AuditRequest' { terminationID = __3 ,
    auditDescriptor = __4 } )
  end | __Stack].

-compile({inline,{yeccpars2_494_,1}}).
-file("megaco_text_parser_prev3a.yrl", 654).
yeccpars2_494_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_495_,1}}).
-file("megaco_text_parser_prev3a.yrl", 663).
yeccpars2_495_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   make_commandRequest ( { auditCapRequest , __1 } ,
    # 'AuditRequest' { terminationID = __3 ,
    auditDescriptor = __4 } )
  end | __Stack].

-compile({inline,{yeccpars2_496_,1}}).
-file("megaco_text_parser_prev3a.yrl", 504).
yeccpars2_496_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_action_request ( __3 , __5 )
  end | __Stack].

-compile({inline,{yeccpars2_497_,1}}).
-file("megaco_text_parser_prev3a.yrl", 506).
yeccpars2_497_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_499_,1}}).
-file("megaco_text_parser_prev3a.yrl", 510).
yeccpars2_499_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_500_,1}}).
-file("megaco_text_parser_prev3a.yrl", 509).
yeccpars2_500_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_502_,1}}).
-file("megaco_text_parser_prev3a.yrl", 618).
yeccpars2_502_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_503_,1}}).
-file("megaco_text_parser_prev3a.yrl", 608).
yeccpars2_503_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   Descs = merge_AmmRequest_descriptors ( __4 , [ ] ) ,
    make_commandRequest ( __1 ,
    # 'AmmRequest' { terminationID = [ __3 ] ,
    descriptors = Descs } )
  end | __Stack].

-compile({inline,{yeccpars2_505_,1}}).
-file("megaco_text_parser_prev3a.yrl", 632).
yeccpars2_505_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { statisticsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_506_,1}}).
-file("megaco_text_parser_prev3a.yrl", 629).
yeccpars2_506_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { signalsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_507_,1}}).
-file("megaco_text_parser_prev3a.yrl", 626).
yeccpars2_507_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { muxDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_508_,1}}).
-file("megaco_text_parser_prev3a.yrl", 625).
yeccpars2_508_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { modemDescriptor , deprecated }
  end | __Stack].

-compile({inline,{yeccpars2_509_,1}}).
-file("megaco_text_parser_prev3a.yrl", 624).
yeccpars2_509_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { mediaDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_510_,1}}).
-file("megaco_text_parser_prev3a.yrl", 627).
yeccpars2_510_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { eventsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_511_,1}}).
-file("megaco_text_parser_prev3a.yrl", 628).
yeccpars2_511_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { eventBufferDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_512_,1}}).
-file("megaco_text_parser_prev3a.yrl", 630).
yeccpars2_512_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { digitMapDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_513_,1}}).
-file("megaco_text_parser_prev3a.yrl", 631).
yeccpars2_513_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { auditDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_514_,1}}).
-file("megaco_text_parser_prev3a.yrl", 621).
yeccpars2_514_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_515_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1308).
yeccpars2_515_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_DMD ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_516_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1063).
yeccpars2_516_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_517_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1099).
yeccpars2_517_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'EventsDescriptor' { requestID = asn1_NOVALUE ,
    eventList = [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_521_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1179).
yeccpars2_521_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_524_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1388).
yeccpars2_524_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_525_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1392).
yeccpars2_525_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'StatisticsParameter' { statName = __1 ,
    statValue = asn1_NOVALUE }
  end | __Stack].

-compile({inline,{yeccpars2_527_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1395).
yeccpars2_527_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'StatisticsParameter' { statName = __1 ,
    statValue = [ __3 ] }
  end | __Stack].

-compile({inline,{yeccpars2_530_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1388).
yeccpars2_530_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_531_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1387).
yeccpars2_531_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_532_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1385).
yeccpars2_532_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_534_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1185).
yeccpars2_534_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { signal , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_535_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1182).
yeccpars2_535_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_536_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1184).
yeccpars2_536_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { seqSigList , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_537_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1519).
yeccpars2_537_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_541_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1251).
yeccpars2_541_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_544_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1251).
yeccpars2_544_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_545_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1250).
yeccpars2_545_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_546_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1246).
yeccpars2_546_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'SeqSigList' { id = ensure_uint16 ( __3 ) ,
    signalList = [ __5 | __6 ] }
  end | __Stack].

-compile({inline,{yeccpars2_549_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1182).
yeccpars2_549_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_550_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1181).
yeccpars2_550_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_551_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1178).
yeccpars2_551_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_553_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1092).
yeccpars2_553_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_muxType ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_555_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1089).
yeccpars2_555_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'MuxDescriptor' { muxType = __3 ,
    termList = __4 }
  end | __Stack].

-compile({inline,{yeccpars2_557_,1}}).
-file("megaco_text_parser_prev3a.yrl", 953).
yeccpars2_557_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_560_,1}}).
-file("megaco_text_parser_prev3a.yrl", 953).
yeccpars2_560_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_561_,1}}).
-file("megaco_text_parser_prev3a.yrl", 952).
yeccpars2_561_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_562_,1}}).
-file("megaco_text_parser_prev3a.yrl", 949).
yeccpars2_562_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_565_,1}}).
-file("megaco_text_parser_prev3a.yrl", 0).
yeccpars2_565_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_566_,1}}).
-file("megaco_text_parser_prev3a.yrl", 0).
yeccpars2_566_(__Stack0) ->
 [begin
   '$undefined'
  end | __Stack0].

-compile({inline,{yeccpars2_569_,1}}).
-file("megaco_text_parser_prev3a.yrl", 0).
yeccpars2_569_(__Stack0) ->
 [begin
   '$undefined'
  end | __Stack0].

-compile({inline,{yeccpars2_570_,1}}).
-file("megaco_text_parser_prev3a.yrl", 0).
yeccpars2_570_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_571_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1299).
yeccpars2_571_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_572_,1}}).
-file("megaco_text_parser_prev3a.yrl", 0).
yeccpars2_572_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_574_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1302).
yeccpars2_574_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_577_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1302).
yeccpars2_577_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_578_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1301).
yeccpars2_578_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_579_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1298).
yeccpars2_579_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_580_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1299).
yeccpars2_580_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_581_,1}}).
-file("megaco_text_parser_prev3a.yrl", 0).
yeccpars2_581_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_583_,1}}).
-file("megaco_text_parser_prev3a.yrl", 974).
yeccpars2_583_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { termState , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_584_,1}}).
-file("megaco_text_parser_prev3a.yrl", 970).
yeccpars2_584_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { streamParm , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_585_,1}}).
-file("megaco_text_parser_prev3a.yrl", 972).
yeccpars2_585_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { streamDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_586_,1}}).
-file("megaco_text_parser_prev3a.yrl", 983).
yeccpars2_586_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { statistics , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_587_,1}}).
-file("megaco_text_parser_prev3a.yrl", 964).
yeccpars2_587_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_588_,1}}).
-file("megaco_text_parser_prev3a.yrl", 982).
yeccpars2_588_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { control , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_590_,1}}).
-file("megaco_text_parser_prev3a.yrl", 979).
yeccpars2_590_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { local , # 'LocalRemoteDescriptor' { propGrps = ensure_prop_groups ( __1 ) } }
  end | __Stack].

-compile({inline,{yeccpars2_591_,1}}).
-file("megaco_text_parser_prev3a.yrl", 981).
yeccpars2_591_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { remote , # 'LocalRemoteDescriptor' { propGrps = ensure_prop_groups ( __1 ) } }
  end | __Stack].

-compile({inline,{yeccpars2_595_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1005).
yeccpars2_595_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_596_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1073).
yeccpars2_596_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { serviceState , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_597_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1075).
yeccpars2_597_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { propertyParm , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_598_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1074).
yeccpars2_598_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { eventBufferControl , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_599_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1436).
yeccpars2_599_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_600_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1515).
yeccpars2_600_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_602_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1077).
yeccpars2_602_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_603_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1081).
yeccpars2_603_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   inSvc
  end | __Stack].

-compile({inline,{yeccpars2_604_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1080).
yeccpars2_604_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   outOfSvc
  end | __Stack].

-compile({inline,{yeccpars2_605_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1079).
yeccpars2_605_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   test
  end | __Stack].

-compile({inline,{yeccpars2_607_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1083).
yeccpars2_607_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_608_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1086).
yeccpars2_608_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   lockStep
  end | __Stack].

-compile({inline,{yeccpars2_609_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1085).
yeccpars2_609_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   off
  end | __Stack].

-compile({inline,{yeccpars2_612_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1005).
yeccpars2_612_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_613_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1004).
yeccpars2_613_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_614_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1002).
yeccpars2_614_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_terminationStateDescriptor ( [ __3 | __4 ] )
  end | __Stack].

-compile({inline,{yeccpars2_618_,1}}).
-file("megaco_text_parser_prev3a.yrl", 991).
yeccpars2_618_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_621_,1}}).
-file("megaco_text_parser_prev3a.yrl", 991).
yeccpars2_621_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_622_,1}}).
-file("megaco_text_parser_prev3a.yrl", 990).
yeccpars2_622_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_623_,1}}).
-file("megaco_text_parser_prev3a.yrl", 987).
yeccpars2_623_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'StreamDescriptor' { streamID = __3 ,
    streamParms = merge_streamParms ( [ __5 | __6 ] ) }
  end | __Stack].

-compile({inline,{yeccpars2_625_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1011).
yeccpars2_625_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { prop , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_626_,1}}).
-file("megaco_text_parser_prev3a.yrl", 997).
yeccpars2_626_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_627_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1480).
yeccpars2_627_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_628_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1510).
yeccpars2_628_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_629_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1511).
yeccpars2_629_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_631_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1009).
yeccpars2_631_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { value , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_632_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1014).
yeccpars2_632_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   false
  end | __Stack].

-compile({inline,{yeccpars2_633_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1013).
yeccpars2_633_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   true
  end | __Stack].

-compile({inline,{yeccpars2_635_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1008).
yeccpars2_635_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { group , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_637_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1010).
yeccpars2_637_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { mode , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_638_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1020).
yeccpars2_638_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   inactive
  end | __Stack].

-compile({inline,{yeccpars2_639_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1021).
yeccpars2_639_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   loopBack
  end | __Stack].

-compile({inline,{yeccpars2_640_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1018).
yeccpars2_640_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   recvOnly
  end | __Stack].

-compile({inline,{yeccpars2_641_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1017).
yeccpars2_641_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   sendOnly
  end | __Stack].

-compile({inline,{yeccpars2_642_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1019).
yeccpars2_642_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   sendRecv
  end | __Stack].

-compile({inline,{yeccpars2_645_,1}}).
-file("megaco_text_parser_prev3a.yrl", 997).
yeccpars2_645_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_646_,1}}).
-file("megaco_text_parser_prev3a.yrl", 996).
yeccpars2_646_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_647_,1}}).
-file("megaco_text_parser_prev3a.yrl", 994).
yeccpars2_647_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_650_,1}}).
-file("megaco_text_parser_prev3a.yrl", 964).
yeccpars2_650_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_651_,1}}).
-file("megaco_text_parser_prev3a.yrl", 963).
yeccpars2_651_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_652_,1}}).
-file("megaco_text_parser_prev3a.yrl", 961).
yeccpars2_652_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_mediaDescriptor ( [ __3 | __4 ] )
  end | __Stack].

-compile({inline,{yeccpars2_656_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1107).
yeccpars2_656_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_657_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1114).
yeccpars2_657_(__Stack0) ->
 [begin
   # 'RequestedEvent' { evParList = [ ] }
  end | __Stack0].

-compile({inline,{yeccpars2_658_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1110).
yeccpars2_658_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   setelement ( # 'RequestedEvent' .pkgdName , __2 , __1 )
  end | __Stack].

-compile({inline,{yeccpars2_661_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1118).
yeccpars2_661_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_665_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1174).
yeccpars2_665_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_eventDM ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_666_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1448).
yeccpars2_666_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_667_COMMA,1}}).
-file("megaco_text_parser_prev3a.yrl", 1121).
yeccpars2_667_COMMA(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   keepActive
  end | __Stack].

-compile({inline,{yeccpars2_667_RBRKT,1}}).
-file("megaco_text_parser_prev3a.yrl", 1121).
yeccpars2_667_RBRKT(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   keepActive
  end | __Stack].

-compile({inline,{yeccpars2_667_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1470).
yeccpars2_667_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_671_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1137).
yeccpars2_671_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'SecondEventsDescriptor' { requestID = asn1_NOVALUE ,
    eventList = [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_675_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1145).
yeccpars2_675_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_676_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1153).
yeccpars2_676_(__Stack0) ->
 [begin
   # 'SecondRequestedEvent' { evParList = [ ] }
  end | __Stack0].

-compile({inline,{yeccpars2_677_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1149).
yeccpars2_677_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   setelement ( # 'SecondRequestedEvent' .pkgdName , __2 , __1 )
  end | __Stack].

-compile({inline,{yeccpars2_679_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1156).
yeccpars2_679_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_683_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1448).
yeccpars2_683_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_684_COMMA,1}}).
-file("megaco_text_parser_prev3a.yrl", 1159).
yeccpars2_684_COMMA(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   keepActive
  end | __Stack].

-compile({inline,{yeccpars2_684_RBRKT,1}}).
-file("megaco_text_parser_prev3a.yrl", 1159).
yeccpars2_684_RBRKT(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   keepActive
  end | __Stack].

-compile({inline,{yeccpars2_684_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1470).
yeccpars2_684_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_687_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1165).
yeccpars2_687_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { second_embed , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_690_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1156).
yeccpars2_690_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_691_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1155).
yeccpars2_691_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_692_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1152).
yeccpars2_692_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_secondEventParameters ( [ __2 | __3 ] )
  end | __Stack].

-compile({inline,{yeccpars2_695_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1145).
yeccpars2_695_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_696_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1144).
yeccpars2_696_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_697_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1141).
yeccpars2_697_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'SecondEventsDescriptor' { requestID = __3 ,
    eventList = [ __5 | __6 ] }
  end | __Stack].

-compile({inline,{yeccpars2_698_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1134).
yeccpars2_698_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { embed , asn1_NOVALUE , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_700_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1131).
yeccpars2_700_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { embed , __3 , asn1_NOVALUE }
  end | __Stack].

-compile({inline,{yeccpars2_702_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1129).
yeccpars2_702_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { embed , __3 , __5 }
  end | __Stack].

-compile({inline,{yeccpars2_705_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1118).
yeccpars2_705_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_706_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1117).
yeccpars2_706_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_707_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1113).
yeccpars2_707_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_eventParameters ( [ __2 | __3 ] )
  end | __Stack].

-compile({inline,{yeccpars2_710_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1107).
yeccpars2_710_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_711_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1106).
yeccpars2_711_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_712_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1103).
yeccpars2_712_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'EventsDescriptor' { requestID = __3 ,
    eventList = [ __5 | __6 ] }
  end | __Stack].

-compile({inline,{yeccpars2_713_,1}}).
-file("megaco_text_parser_prev3a.yrl", 455).
yeccpars2_713_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_714_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1070).
yeccpars2_714_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   merge_eventSpec ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_715_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1068).
yeccpars2_715_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_717_,1}}).
-file("megaco_text_parser_prev3a.yrl", 455).
yeccpars2_717_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_718_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1068).
yeccpars2_718_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_719_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1067).
yeccpars2_719_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_720_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1065).
yeccpars2_720_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_723_,1}}).
-file("megaco_text_parser_prev3a.yrl", 621).
yeccpars2_723_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_724_,1}}).
-file("megaco_text_parser_prev3a.yrl", 620).
yeccpars2_724_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_725_,1}}).
-file("megaco_text_parser_prev3a.yrl", 617).
yeccpars2_725_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_728_,1}}).
-file("megaco_text_parser_prev3a.yrl", 500).
yeccpars2_728_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_729_,1}}).
-file("megaco_text_parser_prev3a.yrl", 499).
yeccpars2_729_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_730_,1}}).
-file("megaco_text_parser_prev3a.yrl", 488).
yeccpars2_730_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'TransactionRequest' { transactionId = asn1_NOVALUE ,
    actions = [ __3 | __4 ] }
  end | __Stack].

-compile({inline,{yeccpars2_732_,1}}).
-file("megaco_text_parser_prev3a.yrl", 915).
yeccpars2_732_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_uint32 ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_734_,1}}).
-file("megaco_text_parser_prev3a.yrl", 500).
yeccpars2_734_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_736_,1}}).
-file("megaco_text_parser_prev3a.yrl", 492).
yeccpars2_736_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'TransactionRequest' { transactionId = asn1_NOVALUE ,
    actions = [ __4 | __5 ] }
  end | __Stack].

-compile({inline,{yeccpars2_738_,1}}).
-file("megaco_text_parser_prev3a.yrl", 500).
yeccpars2_738_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_740_,1}}).
-file("megaco_text_parser_prev3a.yrl", 496).
yeccpars2_740_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'TransactionRequest' { transactionId = ensure_transactionID ( __3 ) ,
    actions = [ __5 | __6 ] }
  end | __Stack].

-compile({inline,{yeccpars2_742_,1}}).
-file("megaco_text_parser_prev3a.yrl", 479).
yeccpars2_742_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_743_,1}}).
-file("megaco_text_parser_prev3a.yrl", 481).
yeccpars2_743_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_transactionAck ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_746_,1}}).
-file("megaco_text_parser_prev3a.yrl", 479).
yeccpars2_746_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_747_,1}}).
-file("megaco_text_parser_prev3a.yrl", 478).
yeccpars2_747_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_748_,1}}).
-file("megaco_text_parser_prev3a.yrl", 476).
yeccpars2_748_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_751_,1}}).
-file("megaco_text_parser_prev3a.yrl", 573).
yeccpars2_751_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_754_,1}}).
-file("megaco_text_parser_prev3a.yrl", 572).
yeccpars2_754_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   'NULL'
  end | __Stack].

-compile({inline,{yeccpars2_756_,1}}).
-file("megaco_text_parser_prev3a.yrl", 575).
yeccpars2_756_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { transactionError , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_757_,1}}).
-file("megaco_text_parser_prev3a.yrl", 579).
yeccpars2_757_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_762_,1}}).
-file("megaco_text_parser_prev3a.yrl", 600).
yeccpars2_762_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { command , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_763_,1}}).
-file("megaco_text_parser_prev3a.yrl", 603).
yeccpars2_763_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { command , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_764_,1}}).
-file("megaco_text_parser_prev3a.yrl", 586).
yeccpars2_764_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'ActionReply' { errorDescriptor = __1 }
  end | __Stack].

-compile({inline,{yeccpars2_765_,1}}).
-file("megaco_text_parser_prev3a.yrl", 604).
yeccpars2_765_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { context , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_766_,1}}).
-file("megaco_text_parser_prev3a.yrl", 598).
yeccpars2_766_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_767_,1}}).
-file("megaco_text_parser_prev3a.yrl", 601).
yeccpars2_767_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { command , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_769_,1}}).
-file("megaco_text_parser_prev3a.yrl", 602).
yeccpars2_769_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { command , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_771_,1}}).
-file("megaco_text_parser_prev3a.yrl", 638).
yeccpars2_771_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   addReply
  end | __Stack].

-compile({inline,{yeccpars2_774_,1}}).
-file("megaco_text_parser_prev3a.yrl", 640).
yeccpars2_774_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   modReply
  end | __Stack].

-compile({inline,{yeccpars2_775_,1}}).
-file("megaco_text_parser_prev3a.yrl", 639).
yeccpars2_775_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   moveReply
  end | __Stack].

-compile({inline,{yeccpars2_778_,1}}).
-file("megaco_text_parser_prev3a.yrl", 641).
yeccpars2_778_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   subtractReply
  end | __Stack].

-compile({inline,{yeccpars2_780_,1}}).
-file("megaco_text_parser_prev3a.yrl", 904).
yeccpars2_780_(__Stack0) ->
 [begin
   { serviceChangeResParms , # 'ServiceChangeResParm' { } }
  end | __Stack0].

-compile({inline,{yeccpars2_781_,1}}).
-file("megaco_text_parser_prev3a.yrl", 896).
yeccpars2_781_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { serviceChangeReply ,
    # 'ServiceChangeReply' { terminationID = [ __3 ] ,
    serviceChangeResult = __4 } }
  end | __Stack].

-compile({inline,{yeccpars2_787_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1369).
yeccpars2_787_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { time_stamp , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_788_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1368).
yeccpars2_788_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { version , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_789_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1367).
yeccpars2_789_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { profile , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_790_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1366).
yeccpars2_790_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { mgc_id , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_791_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1365).
yeccpars2_791_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { address , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_792_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1363).
yeccpars2_792_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_799_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1363).
yeccpars2_799_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_800_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1362).
yeccpars2_800_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_801_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1359).
yeccpars2_801_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_ServiceChangeResParm ( [ __3 | __4 ] )
  end | __Stack].

-compile({inline,{yeccpars2_802_,1}}).
-file("megaco_text_parser_prev3a.yrl", 901).
yeccpars2_802_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { errorDescriptor , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_803_,1}}).
-file("megaco_text_parser_prev3a.yrl", 903).
yeccpars2_803_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { serviceChangeResParms , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_805_,1}}).
-file("megaco_text_parser_prev3a.yrl", 887).
yeccpars2_805_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_806_,1}}).
-file("megaco_text_parser_prev3a.yrl", 882).
yeccpars2_806_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { notifyReply ,
    # 'NotifyReply' { terminationID = [ __3 ] ,
    errorDescriptor = __4 } }
  end | __Stack].

-compile({inline,{yeccpars2_809_,1}}).
-file("megaco_text_parser_prev3a.yrl", 886).
yeccpars2_809_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_811_,1}}).
-file("megaco_text_parser_prev3a.yrl", 680).
yeccpars2_811_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { auditResult ,
    # 'AuditResult' { terminationID = __1 ,
    terminationAuditResult = [ ] } }
  end | __Stack].

-compile({inline,{yeccpars2_812_,1}}).
-file("megaco_text_parser_prev3a.yrl", 672).
yeccpars2_812_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { auditValueReply , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_813_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1437).
yeccpars2_813_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_814_,1}}).
-file("megaco_text_parser_prev3a.yrl", 676).
yeccpars2_814_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { contextAuditResult , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_815_,1}}).
-file("megaco_text_parser_prev3a.yrl", 668).
yeccpars2_815_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { auditValueReply , __4 }
  end | __Stack].

-compile({inline,{yeccpars2_818_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1451).
yeccpars2_818_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_820_,1}}).
-file("megaco_text_parser_prev3a.yrl", 910).
yeccpars2_820_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_uint ( __1 , 0 , 999 )
  end | __Stack].

-compile({inline,{yeccpars2_822_,1}}).
-file("megaco_text_parser_prev3a.yrl", 913).
yeccpars2_822_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_824_,1}}).
-file("megaco_text_parser_prev3a.yrl", 912).
yeccpars2_824_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   value_of ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_825_,1}}).
-file("megaco_text_parser_prev3a.yrl", 907).
yeccpars2_825_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'ErrorDescriptor' { errorCode = __3 ,
    errorText = __5 }
  end | __Stack].

-compile({inline,{yeccpars2_826_,1}}).
-file("megaco_text_parser_prev3a.yrl", 677).
yeccpars2_826_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { contextAuditResult , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_829_,1}}).
-file("megaco_text_parser_prev3a.yrl", 703).
yeccpars2_829_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { statisticsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_830_,1}}).
-file("megaco_text_parser_prev3a.yrl", 699).
yeccpars2_830_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { signalsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_831_,1}}).
-file("megaco_text_parser_prev3a.yrl", 704).
yeccpars2_831_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { packagesDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_832_,1}}).
-file("megaco_text_parser_prev3a.yrl", 701).
yeccpars2_832_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { observedEventsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_833_,1}}).
-file("megaco_text_parser_prev3a.yrl", 697).
yeccpars2_833_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { muxDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_834_,1}}).
-file("megaco_text_parser_prev3a.yrl", 0).
yeccpars2_834_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_835_,1}}).
-file("megaco_text_parser_prev3a.yrl", 695).
yeccpars2_835_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { mediaDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_836_,1}}).
-file("megaco_text_parser_prev3a.yrl", 698).
yeccpars2_836_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { eventsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_837_,1}}).
-file("megaco_text_parser_prev3a.yrl", 702).
yeccpars2_837_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { eventBufferDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_838_,1}}).
-file("megaco_text_parser_prev3a.yrl", 705).
yeccpars2_838_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { errorDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_839_,1}}).
-file("megaco_text_parser_prev3a.yrl", 700).
yeccpars2_839_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { digitMapDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_840_,1}}).
-file("megaco_text_parser_prev3a.yrl", 693).
yeccpars2_840_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_841_,1}}).
-file("megaco_text_parser_prev3a.yrl", 706).
yeccpars2_841_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { auditReturnItem , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_842_,1}}).
-file("megaco_text_parser_prev3a.yrl", 721).
yeccpars2_842_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   mediaToken
  end | __Stack].

-compile({inline,{yeccpars2_843_,1}}).
-file("megaco_text_parser_prev3a.yrl", 720).
yeccpars2_843_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   modemToken
  end | __Stack].

-compile({inline,{yeccpars2_844_,1}}).
-file("megaco_text_parser_prev3a.yrl", 719).
yeccpars2_844_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   muxToken
  end | __Stack].

-compile({inline,{yeccpars2_845_,1}}).
-file("megaco_text_parser_prev3a.yrl", 724).
yeccpars2_845_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   observedEventsToken
  end | __Stack].

-compile({inline,{yeccpars2_846_,1}}).
-file("megaco_text_parser_prev3a.yrl", 725).
yeccpars2_846_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   packagesToken
  end | __Stack].

-compile({inline,{yeccpars2_847_,1}}).
-file("megaco_text_parser_prev3a.yrl", 723).
yeccpars2_847_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   statsToken
  end | __Stack].

-compile({inline,{yeccpars2_849_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1376).
yeccpars2_849_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_852_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1376).
yeccpars2_852_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_853_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1375).
yeccpars2_853_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_854_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1373).
yeccpars2_854_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_855_,1}}).
-file("megaco_text_parser_prev3a.yrl", 690).
yeccpars2_855_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   merge_terminationAudit ( [ __1 | __2 ] )
  end | __Stack].

-compile({inline,{yeccpars2_857_,1}}).
-file("megaco_text_parser_prev3a.yrl", 693).
yeccpars2_857_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_858_,1}}).
-file("megaco_text_parser_prev3a.yrl", 692).
yeccpars2_858_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_859_,1}}).
-file("megaco_text_parser_prev3a.yrl", 684).
yeccpars2_859_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { auditResult ,
    # 'AuditResult' { terminationID = __1 ,
    terminationAuditResult = __3 } }
  end | __Stack].

-compile({inline,{yeccpars2_861_,1}}).
-file("megaco_text_parser_prev3a.yrl", 674).
yeccpars2_861_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { auditCapReply , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_862_,1}}).
-file("megaco_text_parser_prev3a.yrl", 1437).
yeccpars2_862_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_863_,1}}).
-file("megaco_text_parser_prev3a.yrl", 670).
yeccpars2_863_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { auditCapReply , __4 }
  end | __Stack].

-compile({inline,{yeccpars2_864_,1}}).
-file("megaco_text_parser_prev3a.yrl", 583).
yeccpars2_864_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   setelement ( # 'ActionReply' .contextId , __5 , __3 )
  end | __Stack].

-compile({inline,{yeccpars2_866_,1}}).
-file("megaco_text_parser_prev3a.yrl", 644).
yeccpars2_866_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_867_,1}}).
-file("megaco_text_parser_prev3a.yrl", 635).
yeccpars2_867_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __1 , # 'AmmsReply' { terminationID = [ __3 ] ,
    terminationAudit = __4 } }
  end | __Stack].

-compile({inline,{yeccpars2_870_,1}}).
-file("megaco_text_parser_prev3a.yrl", 643).
yeccpars2_870_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_871_,1}}).
-file("megaco_text_parser_prev3a.yrl", 588).
yeccpars2_871_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   merge_action_reply ( [ __1 | __2 ] )
  end | __Stack].

-compile({inline,{yeccpars2_873_,1}}).
-file("megaco_text_parser_prev3a.yrl", 595).
yeccpars2_873_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ { error , __2 } ]
  end | __Stack].

-compile({inline,{yeccpars2_874_,1}}).
-file("megaco_text_parser_prev3a.yrl", 598).
yeccpars2_874_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_875_,1}}).
-file("megaco_text_parser_prev3a.yrl", 597).
yeccpars2_875_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_876_,1}}).
-file("megaco_text_parser_prev3a.yrl", 576).
yeccpars2_876_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { actionReplies , [ __1 | __2 ] }
  end | __Stack].

-compile({inline,{yeccpars2_878_,1}}).
-file("megaco_text_parser_prev3a.yrl", 579).
yeccpars2_878_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_879_,1}}).
-file("megaco_text_parser_prev3a.yrl", 578).
yeccpars2_879_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_880_,1}}).
-file("megaco_text_parser_prev3a.yrl", 568).
yeccpars2_880_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'TransactionReply' { transactionId = __3 ,
    immAckRequired = __5 ,
    transactionResult = __6 }
  end | __Stack].

-compile({inline,{yeccpars2_884_,1}}).
-file("megaco_text_parser_prev3a.yrl", 484).
yeccpars2_884_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'TransactionPending' { transactionId = ensure_transactionID ( __3 ) }
  end | __Stack].

-compile({inline,{yeccpars2_885_,1}}).
-file("megaco_text_parser_prev3a.yrl", 468).
yeccpars2_885_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_886_,1}}).
-file("megaco_text_parser_prev3a.yrl", 956).
yeccpars2_886_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_pathName ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_887_,1}}).
-file("megaco_text_parser_prev3a.yrl", 927).
yeccpars2_887_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { deviceName , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_888_,1}}).
-file("megaco_text_parser_prev3a.yrl", 455).
yeccpars2_888_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_889_,1}}).
-file("megaco_text_parser_prev3a.yrl", 455).
yeccpars2_889_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_890_,1}}).
-file("megaco_text_parser_prev3a.yrl", 944).
yeccpars2_890_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_mtpAddress ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_891_,1}}).
-file("megaco_text_parser_prev3a.yrl", 920).
yeccpars2_891_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_892_,1}}).
-file("megaco_text_parser_prev3a.yrl", 919).
yeccpars2_892_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].


-file("megaco_text_parser_prev3a.yrl", 1555).
